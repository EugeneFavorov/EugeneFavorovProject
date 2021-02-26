{globals.i}             /** Глобальные определения */

{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
beg-date = 09/01/2014.
end-date = 01/01/2016.
{getdates.i &noinit = YES}
/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTmp  AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttDog     NO-UNDO
    FIELD cNum      AS CHARACTER
    FIELD dDat      AS DATE
    FIELD cFIO      AS CHARACTER
    FIELD cCDG      AS CHARACTER
    .

cXL = "./chdg.xml".
REPEAT:
    {getfile.i &filename = cXL &mode = create}
    LEAVE.
END.

/******************************************* Реализация */
PUT UNFORMATTED XLHead("ul", "CDCC", "168,71,285,370").

cXL = XLCellHead("N договора",0,0,0)
    + XLCellHead("Дата выдачи",0,0,0)
    + XLCellHead("ФИО",0,0,0)
    + XLCellHead("Дата ЧДГ",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH loan
    WHERE (loan.contract    EQ "кредит")
      AND CAN-DO("A*,А*", loan.cont-type)
      AND (loan.cust-cat    EQ "Ч")
      AND (loan.filial-id   EQ "0000")
      AND (loan.close-date  EQ ?)
    NO-LOCK,
EACH person
    WHERE (person.person-id EQ loan.cust-id)
    NO-LOCK:

    cTmp = "".
    FOR EACH loan-cond
        WHERE (loan-cond.contract   EQ loan.contract)
          AND (loan-cond.cont-code  EQ loan.cont-code)
          AND (loan-cond.since      GE beg-date)
          AND (loan-cond.since      LE end-date)
        NO-LOCK:

        IF (GetXAttrValue("loan-cond", "кредит," + loan.cont-code + "," + STRING(loan-cond.since), "PayType") EQ "ДосрПогаш")
        THEN cTmp = cTmp + (IF (cTmp EQ "") THEN "" ELSE ", ") + STRING(loan-cond.since, "99.99.9999").
    END.

    IF (cTmp NE "")
    THEN DO:
        CREATE ttDog.
        ASSIGN
            ttDog.cNum = loan.doc-ref
            ttDog.dDat = loan.open-date
            ttDog.cFIO = person.name-last + " " + person.first-names
            ttDog.cCDG = cTmp
            .
    END.
END.

FOR EACH ttDog
    NO-LOCK
    BY ttDog.cFIO
    BY ttDog.dDat:

    cXL = XLCell(ttDog.cNum)
        + XLDateCell(ttDog.dDat)
        + XLCell(ttDog.cFIO)
        + XLCellWrap(ttDog.cCDG)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
{intrface.del}
