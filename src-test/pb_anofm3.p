{globals.i}             /** Глобальные определения */
{intrface.get xclass}   /** Функции для работы с метасхемой */
{intrface.get netw}     /** Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.

cFl = "./analizofm3.xml".
OUTPUT TO VALUE(cFl).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("acct", "CCCCCD", "40,50,400,150,60,71").

cXL = XLCellHead("Тип",0,0,0)
    + XLCellHead("ID",0,0,0)
    + XLCellHead("Наименование",0,0,0)
    + XLCellHead("Счет",0,0,0)
    + XLCellHead("Филиал",0,0,0)
    + XLCellHead("Закрыт",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH cust-corp
    NO-LOCK
    BY cust-corp.cust-id:

    IF (GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "АнализОФМ") <> "3") THEN NEXT.
    put screen col 1 row 24 "Обрабатывается ЮЛ " + STRING(cust-corp.cust-id) + "     ".
    FOR EACH acct
        WHERE (acct.cust-cat    = "Ю")
          AND (acct.cust-id     = cust-corp.cust-id)
          AND CAN-DO("Расчет,Текущ", acct.contract)
          AND (acct.close-date  = ?
            OR acct.close-date  >= 09/01/2017)
        NO-LOCK:

        cXL = XLCell("ЮЛ")
            + XLCell(STRING(cust-corp.cust-id))
            + XLCell(cust-corp.name-short)
            + XLCell(acct.number)
            + XLCell(acct.filial-id)
            + XLDateCell(acct.close-date)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
END.

FOR EACH person
    NO-LOCK
    BY person.person-id:

    IF (GetXAttrValue("person", STRING(person.person-id), "АнализОФМ") <> "3") THEN NEXT.
    put screen col 1 row 24 "Обрабатывается ФЛ " + STRING(person.person-id) + "     ".
    FOR EACH acct
        WHERE (acct.cust-cat    = "Ч")
          AND (acct.cust-id     = person.person-id)
          AND CAN-DO("Расчет,Текущ", acct.contract)
          AND (acct.close-date  = ?
            OR acct.close-date  >= 09/01/2017)
        NO-LOCK:

        cXL = XLCell(IF (GetXAttrValue("person", STRING(person.person-id), "Предпр") = "Предпр") THEN "ИП" ELSE "ФЛ")
            + XLCell(STRING(person.person-id))
            + XLCell(person.name-last + " " + person.first-names)
            + XLCell(acct.number)
            + XLCell(acct.filial-id)
            + XLDateCell(acct.close-date)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
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
