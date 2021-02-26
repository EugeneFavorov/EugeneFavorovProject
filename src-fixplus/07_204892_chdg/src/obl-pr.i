DEFINE VARIABLE mIdnt     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStatusTO AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPreschTo AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk       AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE tt-term-obl-per-pr  NO-UNDO LIKE term-obl.
DEFINE TEMP-TABLE tt-term-obl-sum-pr  NO-UNDO LIKE term-obl.
DEFINE TEMP-TABLE tt-term-obl-debt-pr NO-UNDO LIKE term-obl.
DEFINE TEMP-TABLE tt-term-obl-comm-pr NO-UNDO LIKE term-obl.

DEFINE TEMP-TABLE tt-term-obl NO-UNDO 			/*врем.таблица для печати отчета*/
   FIELD end-date-pr     LIKE term-obl.end-date
   FIELD end-date        LIKE term-obl.end-date
   FIELD dsc-beg-date-pr LIKE term-obl.dsc-beg-date
   FIELD dsc-beg-date    LIKE term-obl.dsc-beg-date
   FIELD amt-rub-pr      LIKE term-obl.amt-rub
   FIELD amt-rub         LIKE term-obl.amt-rub
   FIELD idnt            LIKE term-obl.idnt
   .

ASSIGN
   mStatusTO = iStatusTO
   mPreschTo = iPreschTo
   mIdnt = "301,302,303,310"
   .

/*----------------------------------------------------------------------------*/
/* Функция возвращает вид задложенности для отчета                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetIdntType RETURNS CHARACTER (iCls AS INT64):
   CASE iCls:
      WHEN 303 THEN
         RETURN "ссуда".
      WHEN 301  THEN
         RETURN "%%".
      WHEN 302  THEN
         RETURN "план.остаток".
      WHEN 310 THEN
         RETURN "комиссии".
   END CASE.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Процедура удаления записей предварительго графика                          */
/*----------------------------------------------------------------------------*/
PROCEDURE DeleteObl-pr.
   DEFINE PARAMETER BUFFER term-obl FOR term-obl.

   FIND FIRST loan WHERE loan.contract  EQ term-obl.contract
                     AND loan.cont-code EQ term-obl.cont-code
   NO-LOCK NO-ERROR.
   
   IF AVAIL loan THEN
   DO:
      FOR EACH term-obl WHERE term-obl.contract  EQ loan.contract
                          AND term-obl.cont-code EQ loan.cont-code
                          AND CAN-DO(mIdnt,STRING(term-obl.idnt))
      EXCLUSIVE-LOCK:
         DELETE term-obl.
      END.
   END. 
END.
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:07:32.568+04:00' */
/* $LINTFILE='obl-pr.i' */
/*prosignw7ZfOm5Hc87JFjmoqWwrOQ*/