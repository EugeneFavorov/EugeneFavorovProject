{globals.i}             /** Глобальные определения */
{intrface.get netw}     /** Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE BUFFER   oe    FOR op-entry.
{getdates.i}

cFl = "./valplat.xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
RUN XLAddStyle('<Style ss:ID="s80"><Alignment ss:Horizontal="Center" ss:Vertical="Top"/></Style>').
PUT UNFORMATTED XLHead("op", "DCCNCCCC", "71,150,150,82,57,250,450,100").

cXL = XLCellHead("Дата",0,0,0)
    + XLCellHead("Дебет",0,0,0)
    + XLCellHead("Кредит",0,0,0)
    + XLCellHead("Сумма",0,0,0)
    + XLCellHead("Валюта",0,0,0)
    + XLCellHead("Плательщик",0,0,0)
    + XLCellHead("Назначение",0,0,0)
    + XLCellHead("Признак 407ф",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH op
    WHERE (op.op-date   >= beg-date)
      AND (op.op-date   <= end-date)
      AND (op.filial-id = shFilial)
/*    AND (op.details   BEGINS "Перевод ин")
*/    AND (op.op-kind   = "TAG_103")
    NO-LOCK,
FIRST oe OF op
    WHERE (CAN-DO("!.....810*,40817*,40820*,423*,426*", oe.acct-db)
      AND  CAN-DO("!.....810*,301*,303*", oe.acct-cr))
      AND CAN-DO("840,978,398", oe.currency)
    NO-LOCK
    BY op.op-date:

    cXL = XLDateCell(op.op-date)
        + XLCell(SUBSTRING(oe.acct-db,1,20))
        + XLCell(SUBSTRING(oe.acct-cr,1,20))
        + XLNumCell(oe.amt-cur)
        + XLCell(ENTRY(LOOKUP(oe.currency, "840,978,398"), "USD,EUR,KZT"))
        + XLCellWrap(GetXAttrValue("op", STRING(op.op), "name-send"))
        + XLCellWrap(op.details)
        + XLCellStyle("s80", IF (GetXAttrValue("op", STRING(op.op), "F407") = "") THEN "" ELSE "*")
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* Перед отправкой отчета проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
END.
/* Отправляем протокол */
RUN sndbispc.p ("file=" + cFl + ";class=bq").
{intrface.del}
