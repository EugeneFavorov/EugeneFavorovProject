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
{sh-defs.i}

DEFINE INPUT  PARAMETER iPar    AS CHARACTER    NO-UNDO.

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMask AS CHARACTER NO-UNDO.
DEFINE VARIABLE I     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDR   AS CHARACTER NO-UNDO.
DEFINE VARIABLE n01   AS INTEGER   NO-UNDO.
DEFINE VARIABLE n02   AS INTEGER   NO-UNDO.
DEFINE VARIABLE s01   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE s02   AS DECIMAL   NO-UNDO.
DEFINE BUFFER kart  FOR acct.

cFl = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cFl = "./repbankrot-" + cFl + ".xml".
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
PUT UNFORMATTED XLHead(IF (iPar = "Ю") THEN "ЮЛ" ELSE "ФЛ", "ICCCDNNINI", "46,300,213,52,71,97,97,84,97,84").
cXL = XLCellHat("Отчет по банкротам на " + STRING(TODAY, "99.99.9999"),9).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("N п/п",0,0,0)
    + XLCellHead("Наименование клиента",0,0,0)
    + XLCellHead("Номер счета",0,0,0)
    + XLCellHead("Branch счета",0,0,0)
    + XLCellHead("Дата открытия счета",0,0,0)
    + XLCellHead("Остаток на текущую дату",0,0,0)
    + XLCellHead("Остаток на 90901*",0,0,0)
    + XLCellHead("Кол-во документов",0,0,0)
    + XLCellHead("Остаток на 90902*",0,0,0)
    + XLCellHead("Кол-во документов",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH blockobject
    WHERE (blockobject.class-code   = 'BlockAcct')
      AND (blockobject.file-name    = 'acct')
      AND CAN-DO("Банкрот,КонкПр", blockobject.block-type)
      AND CAN-DO(cMask, blockobject.surrogate)
      AND (blockobject.end-datetime = ?
        OR blockobject.end-datetime >= DATETIME(TODAY + 1))
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        = ENTRY(1,blockobject.surrogate))
      AND (acct.currency    = ENTRY(2,blockobject.surrogate))
      AND (acct.close-date  = ?)
    NO-LOCK
    BY acct.filial-id
    BY acct.acct:

    put screen col 1 row 24 "Обрабатывается " + acct.acct.
    I   = I + 1.

    n01 = 0. n02 = 0. s01 = 0. s02 = 0.
    cDR = GetXAttrValue("acct", acct.acct + "," + acct.currency, "КартБВнСчет").
    IF (cDR <> "")
    THEN DO:
        FOR EACH kart
            WHERE (kart.acct     = ENTRY(1, cDR))
              AND (kart.currency = ENTRY(2, cDR))
            NO-LOCK:

            RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "√").
            s01 = sh-bal.
            IF (s01 <> 0)
            THEN DO:
                FOR EACH kau
                    WHERE (kau.acct     = ENTRY(1, cDR))
                      AND (kau.currency = ENTRY(2, cDR))
                      AND (kau.zero-bal = no)
                    NO-LOCK:

                    n01 = n01 + 1.
                END.
            END.
        END.
    END.

    cDR = GetXAttrValue("acct", acct.acct + "," + acct.currency, "Карт2ВнСчет").
    IF (cDR <> "")
    THEN DO:
        FOR EACH kart
            WHERE (kart.acct     = ENTRY(1, cDR))
              AND (kart.currency = ENTRY(2, cDR))
            NO-LOCK:

            RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "√").
            s02 = sh-bal.
            IF (s02 <> 0)
            THEN DO:
                FOR EACH kau
                    WHERE (kau.acct     = ENTRY(1, cDR))
                      AND (kau.currency = ENTRY(2, cDR))
                      AND (kau.zero-bal = no)
                    NO-LOCK:

                    n02 = n02 + 1.
                END.
            END.
        END.
    END.

    RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "√").
    cXL = XLNumCell(I)
        + XLCellWrap(ClientName(acct.cust-cat, acct.cust-id, acct.acct))
        + XLCell(acct.acct)
        + XLCell(acct.branch-id)
        + XLDateCell(acct.open-date)
        + XLNumCell(- sh-bal)
        + XLNumCell(s01)
        + XLNumCell(n01)
        + XLNumCell(s02)
        + XLNumCell(n02)
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
