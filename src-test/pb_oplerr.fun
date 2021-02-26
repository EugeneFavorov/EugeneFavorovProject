FUNCTION IsCartAcc  RETURNS LOGICAL
   (INPUT  iSurr    AS CHARACTER,
    INPUT  iDat     AS DATETIME).

    DEFINE VARIABLE dTemp   AS DATETIME     NO-UNDO.
    DEFINE BUFFER   oe      FOR op-entry.
    DEFINE BUFFER   hist1   FOR history.
    DEFINE BUFFER   hist2   FOR history.

    FOR EACH kau
        WHERE (kau.acct         EQ ENTRY(1, iSurr))
          AND (kau.currency     EQ ENTRY(2, iSurr))
        NO-LOCK,
    LAST hist1
        WHERE (hist1.file-name  EQ 'op')
          AND (hist1.field-ref  EQ ENTRY(1,kau.kau))
          AND (hist1.modify     EQ 'C')
        NO-LOCK:

        dTemp = DATETIME(hist1.modif-date, hist1.modif-time * 1000).
        dDt1  = dTemp.
        cCrd  = ENTRY(2, kau.sort).
        dD1   = DATE(SUBSTRING(cCrd, 9, 2) + "." + SUBSTRING(cCrd, 6, 2) + "." + SUBSTRING(cCrd, 1, 4)).
        nSum  = DEC(ENTRY(3, kau.sort)).
        cCrd  = IF (iSurr BEGINS "90902") THEN "К2" ELSE "КБС".

        IF      (dTemp LT iDat)       /* Если постановка после, то пропускаем */
            AND (dD1   LT DATE(iDat))
        THEN DO:
            IF NOT kau.zero-bal  /* Если не списан, то нашли */
            THEN DO:
                dD2  = ?.
                dDt2 = ?.
                RETURN TRUE.
            END.
            ELSE DO:
                FOR EACH oe
                    WHERE (oe.kau-cr        EQ kau.kau)
                      AND (oe.op-date       GT DATE(iDat))
                      AND (oe.op-date       LE TODAY)
                    NO-LOCK,
                LAST hist2
                    WHERE (hist2.file-name  EQ 'op')
                      AND (hist2.field-ref  EQ STRING(oe.op))
                      AND (hist2.modify     EQ 'C')
                    NO-LOCK
                    BY hist2.modif-date DESC
                    BY hist2.modif-time DESC:

                    dTemp = DATETIME(hist2.modif-date, hist2.modif-time * 1000).
                    IF      (dTemp GT iDat)   /* Если последнее списание после, то нашли */
                        AND (DATE(iDat) NE hist2.modif-date)
                    THEN DO:
                        dD2  = oe.op-date.
                        dDt2 = dTemp.
                        RETURN TRUE.
                    END.
                    LEAVE.
                END.
            END.
        END.
    END.
    RETURN FALSE.
END FUNCTION.

FUNCTION IsCart     RETURNS LOGICAL
   (INPUT  iSurr    AS CHARACTER,
    INPUT  iDat     AS DATETIME).

    DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.

    cDR = GetXAttrValue("acct", iSurr, "КартБВнСчет").
    IF (cDR NE "")
    THEN DO:
        IF IsCartAcc(cDR, iDat)
        THEN RETURN TRUE.
    END.

    cDR = GetXAttrValue("acct", iSurr, "Карт2ВнСчет").
    IF (cDR NE "")
    THEN RETURN IsCartAcc(cDR, iDat).
    ELSE RETURN FALSE.
END FUNCTION.