/*
{globals.i}           / ** Глобальные определения * /
{intrface.get xclass} / * Функции для работы с метасхемой * /
*/

/* ============================================================================ */
/* ============================================================================ */
/* Преобразование адреса в формате SWIFT вариант K                              */
/* формат КЛАДР: индекс,район,город,нас.пункт,улица,дом,корп.,квартира,строение */
/* при чем поз.2-5 сопровождаются дополнениями г,р-н,ул и т.д.,                 */
/* а поз.6-9 заполняются одними цифрами                                         */
/* ============================================================================ */
FUNCTION KladrSWK RETURNS CHARACTER
   (INPUT  iCustCat AS CHARACTER,
    INPUT  iCustId  AS INTEGER ):

    DEFINE VARIABLE cAdrPart AS CHARACTER  NO-UNDO EXTENT 9 INIT "".
    DEFINE VARIABLE cAdrKl   AS CHARACTER  NO-UNDO INIT "".
    DEFINE VARIABLE iI       AS INTEGER    NO-UNDO.

    FIND LAST cust-ident
        WHERE (cust-ident.cust-cat       EQ iCustCat)
          AND (cust-ident.cust-id        EQ iCustId)
          AND (cust-ident.cust-code-type EQ (IF (iCustCat EQ "Ч") THEN "АдрПроп" ELSE "АдрЮр"))
          AND (cust-ident.class-code     EQ "p-cust-adr")
          AND (cust-ident.close-date     EQ ?
            OR cust-ident.close-date     GT TODAY )
        NO-ERROR.
    IF (AVAIL cust-ident)
    THEN DO:
        DO iI = 1 TO MINIMUM(NUM-ENTRIES(cust-ident.issue), 9):
            cAdrPart[iI] = ENTRY(iI, cust-ident.issue).
        END.

        cAdrKl  = cAdrPart[6]
                + (IF (cAdrPart[7] EQ "") THEN "" ELSE "-") + cAdrPart[7]
                + (IF (cAdrPart[9] EQ "") THEN "" ELSE "-") + cAdrPart[9]
                + (IF (cAdrPart[8] EQ "") THEN "" ELSE "-") + cAdrPart[8]
                .
        IF (cAdrPart[5] NE "")
        THEN DO:
            iI      = NUM-ENTRIES(cAdrPart[5], " ").
            cAdrKl  = cAdrKl + "," + (IF (iI EQ 1) THEN cAdrPart[5] ELSE
                      (ENTRY(iI, cAdrPart[5], " ") + "." + SUBSTRING(cAdrPart[5], 1, R-INDEX(cAdrPart[5], " ") - 1))).
        END.

        IF (cAdrPart[4] NE "")
        THEN DO:
            iI      = NUM-ENTRIES(cAdrPart[4], " ").
            cAdrKl  = cAdrKl + "," + (IF (iI EQ 1) THEN cAdrPart[4] ELSE
                      (ENTRY(iI, cAdrPart[4], " ") + "." + SUBSTRING(cAdrPart[4], 1, R-INDEX(cAdrPart[4], " ") - 1))).
        END.

        IF (cAdrPart[3] NE "")
        THEN DO:
            iI      = NUM-ENTRIES(cAdrPart[3], " ").
            cAdrKl  = cAdrKl + "," + (IF (iI EQ 1) THEN cAdrPart[3] ELSE
                      (ENTRY(iI, cAdrPart[3], " ") + "." + SUBSTRING(cAdrPart[3], 1, R-INDEX(cAdrPart[3], " ") - 1))).
        END.
    END.

    RETURN cAdrKl.
END.

/* ============================================================================ */
/* Преобразование адреса в формате SWIFT вариант F                              */
FUNCTION KladrSWF RETURNS CHARACTER
   (INPUT  iCustCat AS CHARACTER,
    INPUT  iCustId  AS INTEGER ):

    DEFINE VARIABLE cAdrPart AS CHARACTER  NO-UNDO EXTENT 9 INIT "".
    DEFINE VARIABLE cAdrKl   AS CHARACTER  NO-UNDO INIT "".
    DEFINE VARIABLE iI       AS INTEGER    NO-UNDO.

    FIND LAST cust-ident
        WHERE (cust-ident.cust-cat       EQ iCustCat)
          AND (cust-ident.cust-id        EQ iCustId)
          AND (cust-ident.cust-code-type EQ (IF (iCustCat EQ "Ч") THEN "АдрПроп" ELSE "АдрЮр"))
          AND (cust-ident.class-code     EQ "p-cust-adr")
          AND (cust-ident.close-date     EQ ?
            OR cust-ident.close-date     GT TODAY )
        NO-ERROR.
    IF (AVAIL cust-ident)
    THEN DO:
        DO iI = 1 TO MINIMUM(NUM-ENTRIES(cust-ident.issue), 9):
            cAdrPart[iI] = ENTRY(iI, cust-ident.issue).
        END.

        IF (cAdrPart[5] NE "")
        THEN DO:
            iI      = NUM-ENTRIES(cAdrPart[5], " ").
            cAdrKl  = IF (iI EQ 1) THEN cAdrPart[5] ELSE
                      (ENTRY(iI, cAdrPart[5], " ") + "." + SUBSTRING(cAdrPart[5], 1, R-INDEX(cAdrPart[5], " ") - 1)).
        END.

        cAdrKl  = cAdrKl
                + (IF (cAdrPart[6] EQ "") THEN "" ELSE ",д.")    + cAdrPart[6]
                + (IF (cAdrPart[7] EQ "") THEN "" ELSE ",корп.") + cAdrPart[7]
                + (IF (cAdrPart[9] EQ "") THEN "" ELSE ",стр")   + cAdrPart[9]
                + (IF (cAdrPart[8] EQ "") THEN "" ELSE ",кв.")   + cAdrPart[8]
                .
    END.

    RETURN cAdrKl.
END.

/* ============================================================================ */
/* Преобразование адреса в формате SWIFT вариант F (Страна/Город)               */
FUNCTION KladrSWS RETURNS CHARACTER
   (INPUT  iCustCat AS CHARACTER,
    INPUT  iCustId  AS INTEGER ):

    DEFINE VARIABLE cAdrPart AS CHARACTER  NO-UNDO EXTENT 4 INIT "".
    DEFINE VARIABLE cAdrKl   AS CHARACTER  NO-UNDO INIT "".
    DEFINE VARIABLE cTmp     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iI       AS INTEGER    NO-UNDO.

    FIND LAST cust-ident
        WHERE (cust-ident.cust-cat       EQ iCustCat)
          AND (cust-ident.cust-id        EQ iCustId)
          AND (cust-ident.cust-code-type EQ (IF (iCustCat EQ "Ч") THEN "АдрПроп" ELSE "АдрЮр"))
          AND (cust-ident.class-code     EQ "p-cust-adr")
          AND (cust-ident.close-date     EQ ?
            OR cust-ident.close-date     GT TODAY )
        NO-ERROR.
    IF (AVAIL cust-ident)
    THEN DO:
        DO iI = 1 TO MINIMUM(NUM-ENTRIES(cust-ident.issue), 4):
            cAdrPart[iI] = ENTRY(iI, cust-ident.issue).
        END.

        IF (cAdrPart[4] NE "")
        THEN DO:
            iI      = NUM-ENTRIES(cAdrPart[4], " ").
            cAdrKl  = IF (iI EQ 1) THEN cAdrPart[4] ELSE
                      (ENTRY(iI, cAdrPart[4], " ") + "." + SUBSTRING(cAdrPart[4], 1, R-INDEX(cAdrPart[4], " ") - 1)).
        END.

        IF (cAdrPart[3] NE "")
        THEN DO:
            iI      = NUM-ENTRIES(cAdrPart[3], " ").
            cAdrKl  = cAdrKl + (IF (cAdrKl EQ "") THEN "" ELSE ",") + (IF (iI EQ 1) THEN cAdrPart[3] ELSE
                      (ENTRY(iI, cAdrPart[3], " ") + "." + SUBSTRING(cAdrPart[3], 1, R-INDEX(cAdrPart[3], " ") - 1))).
        END.

        IF (cAdrPart[2] NE "")
        THEN DO:
            iI      = NUM-ENTRIES(cAdrPart[2], " ").
            cAdrKl  = cAdrKl + (IF (cAdrKl EQ "") THEN "" ELSE ",") + (IF (iI EQ 1) THEN cAdrPart[2] ELSE
                      (ENTRY(iI, cAdrPart[2], " ") + "." + SUBSTRING(cAdrPart[2], 1, R-INDEX(cAdrPart[2], " ") - 1))).
        END.

        cTmp   = cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num).
        cTmp   = GetXAttrValue("country", GetXAttrValue("cust-ident", cTmp, "country-id"), "ALFA-2").
        cAdrKl = (IF (cTmp EQ "") THEN "RU" ELSE cTmp) + "/" + cAdrKl.
    END.

    RETURN cAdrKl.
END.

