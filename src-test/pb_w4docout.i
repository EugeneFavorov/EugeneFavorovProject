/* Суррогат счета с валютой из номера */
/* ================================== */
FUNCTION AcctSurr   RETURNS CHARACTER
   (INPUT  iAcct    AS CHARACTER ).

    DEFINE VARIABLE cCur        AS CHARACTER    NO-UNDO.

    cCur = SUBSTRING(iAcct,6,3).
    cCur = IF (cCur = "810") THEN "" ELSE cCur.
    RETURN iAcct + "," + cCur.
END FUNCTION.

/* Проверка op/op-entry на выгрузку в WAY4 */
/* ======================================= */
FUNCTION ForExp2W4  RETURNS LOGICAL.
/*
    DEFINE VARIABLE cBAcct      AS CHARACTER    NO-UNDO.
*/
    IF (NOT AVAIL acdb) OR (NOT AVAIL accr) THEN RETURN NO.

    /* Оба счета не того класса */
    IF      (acdb.class-code NE "acctw4" AND acdb.class-code NE "acctow4")
        AND (accr.class-code NE "acctw4" AND accr.class-code NE "acctow4")
    THEN RETURN NO.

    /* Переоценка */
    IF (op.doc-num BEGINS "П") THEN RETURN NO.

    /* Либо импортирован, либо уже экспортирован */
    IF (GetXAttrValue("op", STRING(op.op), "W4_export") NE "") THEN RETURN NO.
    IF (GetXAttrValue("op", STRING(op.op), "W4_import") NE "") THEN RETURN NO.

    /* Один из счетов соответствует маске * /
    IF CAN-DO("40817....0599*,40820....0599*", oe.acct-db) THEN RETURN YES.
    IF CAN-DO("40817....0599*,40820....0599*", oe.acct-cr) THEN RETURN YES.
*/
    /* 60308 -> базовый */
    IF CAN-DO("60308....0577*", oe.acct-cr)
    THEN RETURN NOT (oe.acct-db BEGINS GetXAttrValue("acct", AcctSurr(oe.acct-cr), "W4basicacct")).

    RETURN YES.
END FUNCTION.

/* Замена счета при экспорте в WAY4 */
/* ================================ */
FUNCTION Acct2W4    RETURNS CHARACTER
   (BUFFER iAcct    FOR acct,
    INPUT  iCurr    AS CHARACTER ).

    IF CAN-DO("60308*,91317*", iAcct.acct)
    THEN RETURN SUBSTRING(GetXAttrValue("acct", iAcct.acct + "," + iAcct.currency, "W4basicacct"),1,20).
    ELSE RETURN IF (iAcct.class-code = "acctw4" OR iAcct.class-code = "acctow4")
                THEN iAcct.number
/*              ELSE ENTRY(LOOKUP(iAcct.currency, ",840,978"), "47422810005990000000,47422840005990010001,47422978005990010001").
*/              ELSE GetRefVal("W4subst", TODAY, IF (iCurr = "") THEN "643" ELSE iCurr).
END FUNCTION.

/* Дата в формате WAY4 */
/* =================== */
FUNCTION W4Date     RETURNS CHARACTER
   (INPUT  iDate    AS DATE ).

    RETURN STRING(YEAR(iDate)) + "-" + STRING(MONTH(iDate), "99") + "-" + STRING(DAY(iDate), "99").
END FUNCTION.
