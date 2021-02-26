/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      СЗ 
Что делает:     Отчет по блокировкам
Параметры:      Тип клиента = "Ю" или "Ч"
Место запуска:  Отчеты по картотекам
Создан:         21.11.2017 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{intrface.get netw}     /** Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

DEFINE INPUT  PARAMETER iPar    AS CHARACTER    NO-UNDO.

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMask AS CHARACTER NO-UNDO.
DEFINE VARIABLE I     AS INTEGER   NO-UNDO.

cFl = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cFl = "./repblk-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).
cMask = IF (iPar = "Ю") THEN "405*,406*,407*,40802*,40807*" ELSE "40817*,40820*,423*,426*".
I     = 0.

/******************************************* Реализация */
FUNCTION ClientName RETURNS CHARACTER
   (INPUT  iCat     AS CHARACTER,
    INPUT  iId      AS INT64,
    INPUT  iAcct    AS CHARACTER ).

    IF (iCat = "Ю")
    THEN DO:
        FOR FIRST cust-corp
            WHERE (cust-corp.cust-id = iId)
            NO-LOCK:

            RETURN cust-corp.name-short.
        END.
    END.
    ELSE IF (iCat = "Ч")
    THEN DO:
        FOR EACH person
            WHERE (person.person-id = iId)
            NO-LOCK:

            RETURN (IF CAN-DO("40802*", iAcct) THEN "ИП " ELSE "") + person.name-last + " " + person.first-names.
        END.
    END.
    RETURN "".
END FUNCTION.
/******************************************* Реализация */
PUT UNFORMATTED XLHead(IF (iPar = "Ю") THEN "ЮЛ" ELSE "ФЛ", "ICCCCNCCCCCD", "46,56,300,150,106,130,123,53,150,150,52,108").
cXL = XLCellHat('Отчет по блокировкам, установленным на счетах '
    + (IF (iPar = "Ю") THEN "юридических лиц и индивидуальных предпринимателей"
                       ELSE "физических лиц")
    + " на " + STRING(TODAY, "99.99.9999"),11).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("N п/п",0,0,0)
    + XLCellHead("Номер Группы",0,0,0)
    + XLCellHead("Наименование клиента",0,0,0)
    + XLCellHead("Номер счета",0,0,0)
    + XLCellHead("Наименование блокировки",0,0,0)
    + XLCellHead("Сумма блокировки",0,0,0)
    + XLCellHead("Начало действия блокировки",0,0,0)
    + XLCellHead("Орган",0,0,0)
    + XLCellHead("Тип постановления",0,0,0)
    + XLCellHead("Постановление",0,0,0)
    + XLCellHead("Код органа",0,0,0)
    + XLCellHead("Дата по постановления",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH blockobject
    WHERE (blockobject.class-code   = 'BlockAcct')
      AND (blockobject.file-name    = 'acct')
      AND CAN-DO("Банкрот,БлокДб,БлокСумм,БлокТамож,КонкПр", blockobject.block-type)
      AND CAN-DO(cMask, blockobject.surrogate)
      AND (blockobject.end-datetime = ?
        OR blockobject.end-datetime >= DATETIME(TODAY + 1))
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        = ENTRY(1,blockobject.surrogate))
      AND (acct.currency    = ENTRY(2,blockobject.surrogate))
      AND (acct.close-date  = ?)
      AND (acct.filial-id   = shFilial)
    NO-LOCK
    BY acct.acct
    BY blockobject.beg-datetime:

    put screen col 1 row 24 "Обрабатывается " + acct.acct.
    I   = I + 1.
    cXL = XLNumCell(I)
        + XLCell(GetXAttrValue("acct", blockobject.surrogate, "groupOABS"))
        + XLCellWrap(ClientName(acct.cust-cat, acct.cust-id, acct.acct))
        + XLCell(acct.acct)
        + XLCell(blockobject.block-type)
        + XLNumCell(blockobject.val[3])
        + XLCell(STRING(blockobject.beg-datetime, "99.99.9999 HH:MM"))
        + XLCell(blockobject.txt[2])
        + XLCell(blockobject.txt[3])
        + XLCell(ENTRY(1, blockobject.txt[4], ";"))
        + XLCell(IF (NUM-ENTRIES(blockobject.txt[4], ";") >= 2) THEN ENTRY(2, blockobject.txt[4], ";") ELSE "")
        + XLCell(GetXAttrValue("blockobject", STRING(blockobject.BlockObjectID), "ДатаРешПр"))
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
put screen col 1 row 24 color normal STRING(" ","X(80)").

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
