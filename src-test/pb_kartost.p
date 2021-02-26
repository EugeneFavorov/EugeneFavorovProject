/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      
Что делает:     Отчет по остаткам на картотеках
Параметры:      
Место запуска:  
Создан:         19.06.2018 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{tmprecid.def}          /** Используем информацию из броузера */
{intrface.get xclass}
{intrface.get netw}     /** Отправка в bispc */

{sh-defs.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDR   AS CHARACTER NO-UNDO.
DEFINE VARIABLE nPos1 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nPos2 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nPos3 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cN1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cINN  AS CHARACTER NO-UNDO.

cFl = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cFl = "./kartost-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("acct", "CCCDCNDNNNC", "112,300,150,105,61,90,78,90,90,90,300").

cXL = XLCellHead("Подразделение",0,0,0)
    + XLCellHead("Клиент",0,0,0)
    + XLCellHead("Лицевой счет",0,0,0)
    + XLCellHead("Дата открытия",0,0,0)
    + XLCellHead("Банкрот",0,0,0)
    + XLCellHead("Остаток",0,0,0)
    + XLCellHead("Дата последней операции",0,0,0)
    + XLCellHead("47423",0,0,0)
    + XLCellHead("Картотека 2",0,0,0)
    + XLCellHead("КБС",0,0,0)
    + XLCellHead("Ответственный менеджер ДКК",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tmprecid 
    NO-LOCK,
FIRST acct
    WHERE (RECID(acct)   = tmprecid.id)
      AND (acct.currency = "")
    NO-LOCK
    BY acct.branch-id
    BY acct.cust-cat DESC
    BY acct.cust-id:

    nPos1 = 0.0.
    nPos2 = 0.0.
    nPos3 = 0.0.

    cDR   = GetLinks("acctb", acct.acct + "," + acct.currency, "S", "acct47423", ",", ?).
    IF (cDR NE "")
    THEN DO:
        RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "√").
        nPos3 = sh-bal.
    END.

    cDR   = GetXAttrValue("acct", acct.acct + "," + acct.currency, "Карт2ВнСчет").
    IF (cDR NE "")
    THEN DO:
        RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "√").
        nPos2 = sh-bal.
    END.

    cDR   = GetXAttrValue("acct", acct.acct + "," + acct.currency, "КартБВнСчет").
    IF (cDR NE "")
    THEN DO:
        RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "√").
        nPos1 = sh-bal.
    END.

    RUN GetCustName IN h_base (acct.cust-cat, acct.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cINN).
    FIND FIRST blockobject
        WHERE (blockobject.file-name     EQ 'acct')
          AND (blockobject.class-code    EQ 'BlockAcct')
          AND CAN-DO("КонкПр,Банкрот", blockobject.block-type)
          AND (blockobject.beg-datetime  LE DATETIME(TODAY, MTIME))
          AND ((blockobject.end-datetime EQ ?)
            OR (blockobject.end-datetime GE DATETIME(TODAY, MTIME)))
          AND (blockobject.surrogate     EQ acct.acct + "," + acct.currency)
        NO-LOCK NO-ERROR.
    RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "√").

    cDR   = GetXAttrValue(IF (acct.cust-cat = "Ю") THEN "cust-corp" ELSE "person", STRING(acct.cust-id), "Привлеченец").
    FIND FIRST code
        WHERE (code.class   = "Привлеченцы")
          AND (code.parent  = "Привлеченцы")
          AND (code.code    = cDR)
        NO-LOCK NO-ERROR.

    cXL = XLCell(acct.branch-id)
        + XLCellWrap(TRIM(cN1 + " " + cN2))
        + XLCell(acct.number)
        + XLDateCell(acct.open-date)
        + XLCell(IF (AVAIL blockobject) THEN blockobject.block-type ELSE "")
        + XLNumCell(- sh-bal)
        + XLDateCell(lastmove)
        + XLNumCell(nPos3)
        + XLNumCell(nPos2)
        + XLNumCell(nPos1)
        + XLCell(IF (AVAIL code) THEN code.name ELSE "")
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
