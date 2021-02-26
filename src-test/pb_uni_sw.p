/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОКМР.1578 - формирование валютных переводов в Бисквите
Что делает:     Печать заявления на перевод SWIFT
Как работает:   Формирует документ WORD и отправляет его на экран через BisPC
Параметры:      RECID документа
Место запуска:  Печать документа процедурой "Документ SWIFT ФЛ/ЮЛ"
Создан:         22.05.2017 Борисов А.В.
*/

{globals.i}
{intrface.get netw}     /* Отправка в bispc */
{prn-doc.def &with_proc=YES}

DEFINE INPUT PARAM rid      AS RECID        NO-UNDO.

DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cSurr       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTxt1       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTxt2       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lFl         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
DEFINE BUFFER   oe      FOR op-entry.
DEFINE BUFFER   ob      FOR op-bank.
DEFINE BUFFER   pass    FOR cust-ident.

/* Функции ******************************************************************** */
FUNCTION CntrNumCode    RETURNS CHARACTER
   (INPUT  iCntr    AS CHARACTER ).

    RELEASE country.
    IF (iCntr <> "") THEN
        FIND FIRST country
            WHERE (country.country-id      = iCntr)
               OR (country.country-alt-id  = INT(iCntr))
            NO-LOCK NO-ERROR.
    RETURN (IF (AVAIL country) THEN STRING(country.country-alt-id, "999") ELSE
           (IF (AVAIL banks) THEN CntrNumCode(banks.country-id) ELSE "")).
END FUNCTION.

FUNCTION Name-Adr   RETURNS CHARACTER
   (INPUT  iStr     AS CHARACTER ).

    DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
    DEFINE VARIABLE I0          AS INTEGER      NO-UNDO.
    DEFINE VARIABLE J           AS INTEGER      NO-UNDO.
    DEFINE VARIABLE J0          AS INTEGER      NO-UNDO.

    I0 = LENGTH(iStr).
    J0 = 0.
    DO I = 35 TO LENGTH(iStr) BY 35:
        J = I - LENGTH(TRIM(SUBSTRING(iStr, 1, I))).    /* Пробелов до конца строки документа SWIFT */
        IF (J > J0 + 1)
        THEN DO:
            J0 = J.
            I0 = I.
        END.
    END.
    IF (J0 <= 1) THEN I0 = 35.   /* Если явного разрыва не обнаружено */

    RETURN  TRIM(SUBSTRING(iStr, 1, I0)) + "~n" + (IF (I0 = 0) THEN "" ELSE SUBSTRING(iStr, I0 + 1)).
END FUNCTION.

/* **************************************************************************** */
FOR FIRST op
    WHERE (RECID(op)        EQ rid)
      AND (op.class-code    BEGINS "opbct")
    NO-LOCK,
FIRST oe OF op
/*  WHERE CAN-DO("30220*", oe.acct-db)
      AND CAN-DO("30110*,30114*", oe.acct-cr)
      AND (oe.currency  NE "")
*/  NO-LOCK:

  {empty ttnames}
    /* Документ *************************************************************** */
    cSurr = STRING(op.op).
    RUN Insert_TTName ("docnum", op.doc-num).
    RUN Insert_TTName ("docdate", STRING(op.op-date, "99.99.9999")).
    cTxt1 = TRIM(REPLACE(GetXAttrValue("op",STRING(op.op),"sw-details"),"~n"," ")).
    RUN Insert_TTName ("docdetails", IF (cTxt1 = "") THEN op.details ELSE cTxt1).
    RUN Insert_TTName ("codevo", IF (INDEX(op.details, "~{VO") EQ 0) THEN "" ELSE SUBSTRING(op.details, INDEX(op.details, "~{VO") + 3, 5)).
    cDR = Name-Adr(GetXAttrValue("op", cSurr, "sw-name-send")).
    RUN Insert_TTName ("custnamesw", ENTRY(1, cDR, "~n")).
    RUN Insert_TTName ("custadress", ENTRY(2, cDR, "~n")).
    cDR = GetXAttrValue("op", cSurr, "swift-det-pay").
    CASE cDR:
        WHEN "" OR
        WHEN "OUR"  THEN RUN Insert_TTName ("doccharges1", "V").
        WHEN "BEN"  THEN RUN Insert_TTName ("doccharges2", "V").
        WHEN "SHA"  THEN RUN Insert_TTName ("doccharges3", "V").
    END CASE.
    cTxt1 = GetXAttrValue("op", cSurr, "swift-det-cod").
    IF (cTxt1 NE "||||||") AND (cTxt1 NE "")
    THEN DO:
        cTxt2 = GetXAttrValue("op", cSurr, "swift-det-inf").
        IF (ENTRY(1, cTxt1, "|") NE "")
        THEN RUN Insert_TTName ("docinfo1", "/" + ENTRY(1, cTxt1, "|") + "/" + ENTRY(1, cTxt2, "|")).
        IF (ENTRY(2, cTxt1, "|") NE "")
        THEN RUN Insert_TTName ("docinfo2", "/" + ENTRY(2, cTxt1, "|") + "/" + ENTRY(2, cTxt2, "|")).
    END.

    /* Валюта и сумма перевода ************************************************ */
    cDR = GetXAttrValue("op", cSurr, "sw-benef-amt").
    IF (cDR EQ "")
    THEN DO:    /* Валюта перевода совпадает с валютой счета клиента */
        FIND FIRST currency
            WHERE (currency.currency    EQ oe.currency)
            NO-LOCK NO-ERROR.
        IF (AVAIL currency)
        THEN RUN Insert_TTName ("doccurr", currency.i-currency).
        RUN Insert_TTName ("docsumm", STRING(oe.amt-cur, ">>>>>>>>>>>9.99")).
        RUN x-amtstr.p (oe.amt-cur, oe.currency, YES, YES, OUTPUT cTxt1, OUTPUT cTxt2).
        RUN Insert_TTName ("docsumtxt", cTxt1 + (IF (ROUND(oe.amt-cur, 0) EQ oe.amt-cur) THEN "" ELSE (" " + cTxt2))).
    END.
    ELSE DO:    /* Мультивалютные перевод */
        cTxt1 = SUBSTRING(cDR, 1, 3).
        RUN Insert_TTName ("doccurr", cTxt1).
        cTxt2 = SUBSTRING(cDR, 4).
        FIND FIRST currency
            WHERE (currency.i-currency  EQ cTxt1)
            NO-LOCK NO-ERROR.
        IF (AVAIL currency) THEN cTxt1 = currency.currency.
        RUN Insert_TTName ("docsumm", cTxt2).
        RUN x-amtstr.p (DEC(cTxt2), cTxt1, YES, YES, OUTPUT cTxt1, OUTPUT cTxt2).
        RUN Insert_TTName ("docsumtxt", cTxt1 + (IF (SUBSTRING(cDR, LENGTH(cDR) - 2) EQ ",00") THEN "" ELSE (" " + cTxt2))).
    END.

    /* Банк-посредник ********************************************************* */
    FOR FIRST ob OF op
        WHERE (ob.op-bank-type              EQ "intermed")
        NO-LOCK,
    FIRST banks-code
        WHERE (banks-code.bank-code-type    EQ ob.bank-code-type)
          AND (banks-code.bank-code         EQ ob.bank-code)
        NO-LOCK,
    FIRST banks OF banks-code
        NO-LOCK:

        RUN Insert_TTName ("intersw",     ob.bank-code).
        RUN Insert_TTName ("intername",   banks.name).
        RUN Insert_TTName ("interadress", banks.country-id + (IF (banks.law-address EQ "") THEN "" ELSE ", ") + banks.law-address).
    END.

    /* Банк бенефициара ******************************************************* */
    FOR FIRST ob OF op
        WHERE (ob.op-bank-type              EQ "")
        NO-LOCK,
    FIRST banks-code
        WHERE (banks-code.bank-code-type    EQ ob.bank-code-type)
          AND (banks-code.bank-code         EQ ob.bank-code)
        NO-LOCK,
    FIRST banks OF banks-code
        NO-LOCK:

        RUN Insert_TTName ("bencoracct",  ob.corr-acct).
        RUN Insert_TTName ("bensw",       ob.bank-code).
        RUN Insert_TTName ("benname",     banks.name).
        RUN Insert_TTName ("benadress",   banks.law-address).
        RUN Insert_TTName ("bencountry",  CntrNumCode(banks.country-id)).
    END.

    /* Бенефициар ************************************************************* */
    RUN Insert_TTName ("beneficacct", op.ben-acct).

    cDR = Name-Adr(op.name-ben).
    RUN Insert_TTName ("beneficname",   ENTRY(1, cDR, "~n")).
    RUN Insert_TTName ("beneficadress", ENTRY(2, cDR, "~n")).

    cDR = GetXAttrValue("op", cSurr, "sw-benef-cntr").
    RUN Insert_TTName ("beneficcountry",  CntrNumCode(cDR)).

    /* Клиент ***************************************************************** */
    cDR = GetXAttrValue("op", cSurr, "acct-send").
    RUN Insert_TTName ("custacct", cDR).
    FIND FIRST acct
        WHERE (acct.acct    BEGINS cDR)
        NO-LOCK NO-ERROR.
    lFl = (acct.cust-cat EQ "Ч") AND NOT (cDR BEGINS "40802").  /* ФЛ с не-ИПшным счетом */

    IF (acct.cust-cat EQ "Ю")
    THEN DO:    /* ЮЛ */
        FIND FIRST cust-corp
            WHERE (cust-corp.cust-id    EQ acct.cust-id)
            NO-LOCK NO-ERROR.
        RUN Insert_TTName ("custname", cust-corp.cust-stat + " " + cust-corp.name-corp).
        cDR = GetXAttrValue("op", cSurr, "sw-send-tel").
        RUN Insert_TTName ("custphone", cDR).
        RUN Insert_TTName ("custinn", cust-corp.inn).
    END.
    ELSE IF (acct.cust-cat EQ "Ч")
    THEN DO:    /* ФЛ и ИП */
        FIND FIRST person
            WHERE (person.person-id EQ acct.cust-id)
            NO-LOCK NO-ERROR.
        RUN Insert_TTName ("custname", (IF lFl THEN "" ELSE "ИП ") + name-last + " " + first-names).
        RUN Insert_TTName ("custphone", IF (ENTRY(2, person.phone[2]) NE "") THEN ENTRY(2, person.phone[2]) ELSE (
                                        IF (ENTRY(1, person.phone[1]) NE "") THEN ENTRY(1, person.phone[1]) ELSE (
                                        IF (ENTRY(2, person.phone[1]) NE "") THEN ENTRY(2, person.phone[1]) ELSE ENTRY(1, person.phone[2])))).
        RUN Insert_TTName ("custinn", person.inn).
    END.
    ELSE DO:
        RUN Insert_TTName ("custname",  GetXAttrValue("op", cSurr, "name-send")).
        RUN Insert_TTName ("custphone", "").
        RUN Insert_TTName ("custinn",   GetXAttrValue("op", cSurr, "inn-send")).
    END.

    IF lFl
    THEN DO:    /* ФЛ */
        cDR = GetXAttrValue("op", cSurr, "sw-benef-res").
        RUN Insert_TTName ("beneficrez",   IF (cDR EQ   "резидент") THEN "V" ELSE "").
        RUN Insert_TTName ("beneficnerez", IF (cDR EQ "нерезидент") THEN "V" ELSE "").
        cDR = GetXAttrValue("op", cSurr, "sw-benef-ul").
        RUN Insert_TTName ("beneficul", IF (cDR EQ "ЮЛ") THEN "V" ELSE "").
        RUN Insert_TTName ("beneficfl", IF (cDR EQ "ФЛ") THEN "V" ELSE "").

        cDR = GetXAttrValue("op", cSurr, "swift-50f-4").
        RUN Insert_TTName ("custbirthday", cDR).
        FOR FIRST code
            WHERE (code.class   EQ "КодДокум")
              AND (code.parent  EQ "КодДокум")
              AND (code.code    EQ person.document-id)
            NO-LOCK:

            RUN Insert_TTName ("custdocum", code.name).
            RUN Insert_TTName ("custdocnum", person.document).

            FOR LAST pass
                WHERE (pass.class-code      EQ 'p-cust-ident')
                  AND (pass.cust-cat        EQ 'Ч')
                  AND (pass.cust-id         EQ person.person-id)
                  AND (pass.close-date      EQ ?)
                  AND (pass.cust-code-type  EQ person.document-id)
                  AND (pass.cust-code       EQ person.document)
                NO-LOCK:

                cDR = GetXAttrValue("cust-ident", pass.cust-code-type + "," + pass.cust-code + "," + STRING(pass.cust-type-num), "Подразд").
                RUN Insert_TTName ("custdocvydan", pass.issue
                                                + (IF (cDR EQ "") THEN "" ELSE (", К/П " + cDR))
                                                + (IF (pass.open-date EQ ?) THEN "" ELSE (", " + STRING(pass.open-date, "99.99.9999")))).
            END.
        END.
    END.
    ELSE DO:    /* ЮЛ */
        cDR = GetXAttrValue("op", cSurr, "sw-charge-acct").
        RUN Insert_TTName ("chargeacct", SUBSTRING(cDR,1,20)).
    END.

    /* Перед отправкой файла проверим, запущен ли bispc */
    DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
    DO WHILE (mRet EQ ""):
        RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
        IF (mRet EQ "")
        THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
    END.
    /* Вывод данных по шаблону template в файл отчета */
    RUN printvd.p (IF lFl THEN "swift_fl" ELSE "swift_ul", INPUT TABLE ttnames).
END.
