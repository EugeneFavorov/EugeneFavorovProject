/*
cred-pko.p
*/


{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get blkob}

{sh-defs.i}
{tmpobj.def}
{tmprecid.def}

DEFINE VARIABLE vCustID   AS INT64     NO-UNDO.
DEFINE VARIABLE vName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAcct     AS CHARACTER NO-UNDO VIEW-AS COMBO-BOX INNER-LINES 5.
DEFINE VARIABLE vOAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vFilAcct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vLoan     AS CHARACTER NO-UNDO.

DEFINE VARIABLE vAmt      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOAmt     AS CHARACTER NO-UNDO.

DEFINE VARIABLE vInt      AS INT64     NO-UNDO.

DEFINE TEMP-TABLE ttLoan
   FIELD loan-rid  AS RECID
   FIELD filial-id AS CHARACTER
   FIELD cont-code AS CHARACTER
   FIELD acct      AS CHARACTER
   FIELD amt       AS DECIMAL.

DEFINE BUFFER acct FOR acct.

FORM
   vCustID  FORMAT ">>>>>>>9"
            LABEL  "Номер"
            HELP   "F1 - выбор клиента "
   vName    FORMAT "x(55)"
            LABEL  "ФИО"
   vAcct    LABEL  "  Расчетный счет" 
            FORMAT "x(30)"
            HELP   "Выбор расчетного счета "
   vLoan    LABEL  "      Договор(ы)" 
            FORMAT "x(55)"
   vAmt     LABEL  "    Общий платеж" 
            FORMAT "x(20)"
            HELP   "Сумма платежа"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE " Выбор счета клиента ".

ON 'F1':U OF vCustID IN FRAME frAcct
DO:
   RUN person.p (4).
   IF KEYFUNCTION (LASTKEY) NE "end-error" AND
      pick-value            NE ? THEN
   DO:
      vCustID = INT64 (pick-value).
      SELF:SCREEN-VALUE = STRING(vCustID).
      APPLY "LEAVE" TO vCustID.
      APPLY "ENTRY" TO vAcct.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF vCustID
DO:
   FIND FIRST person WHERE
      person.person-id EQ INT64(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAIL(person) THEN
   DO:
      ASSIGN
         vCustID = 0
         vName              = ""
         vName:SCREEN-VALUE = ""
         vName:SENSITIVE IN FRAME frAcct = NO.
      DISPLAY vCustID WITH FRAME frAcct.
      RETURN NO-APPLY.
   END.
   ELSE
   DO:
      ASSIGN 
         vCustID            = INT64(SELF:SCREEN-VALUE)
         vName              = person.name-last + " " + person.first-names
         vName:SCREEN-VALUE = person.name-last + " " + person.first-names
         vName:SENSITIVE IN FRAME frAcct = NO
         vAmt :SENSITIVE IN FRAME frAcct = NO.
      
      RUN SearchCredAcct IN THIS-PROCEDURE 
         (INPUT vCustID,
          OUTPUT vAcct,
          OUTPUT vAmt,
          OUTPUT vLoan).

/*      vAcct = vAcct + "," + "40817810104700000259     @0000".*/
/*      vAmt  = vAmt  + "," + "10000.00".                      */

      ASSIGN 
         vAcct:LIST-ITEMS   = vAcct
         vAcct:SCREEN-VALUE = ENTRY(1,vAcct)
         vAmt :SCREEN-VALUE = ENTRY(1,vAmt)
         vOAcct = ENTRY(1,vAcct)
         vOAmt  = ENTRY(1,vAmt)
/*         vAmt :SCREEN-VALUE = vAmt*/
         vLoan:SCREEN-VALUE = vLoan.
      APPLY "ENTRY" TO vAcct.
      RETURN NO-APPLY.
   END.
END.

ON VALUE-CHANGED OF vAcct
DO:
   vInt = LOOKUP(vAcct:SCREEN-VALUE,vAcct:LIST-ITEMS).
   ASSIGN
      vAcct:SCREEN-VALUE = ENTRY(vInt,vAcct)
      vAmt :SCREEN-VALUE = ENTRY(vInt,vAmt)
      vOAcct = ENTRY(vInt,vAcct)
      vOAmt  = ENTRY(vInt,vAmt).
   APPLY "ENTRY" TO vAcct.
   RETURN NO-APPLY.
END.   

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frAcct.
      RETURN ERROR.
   END.

   UPDATE
      vCustID
      vName 
      vAcct
   WITH FRAME frAcct.

   {find-act.i
      &acct = vAcct 
   }

   IF AVAIL(acct) THEN
   DO:
      ASSIGN
         vFilAcct = acct.filial-id.
         pick-value = TRIM(STRING(vCustID)) + "," + DelFilFromAcct(vOAcct) + "," + vFilAcct + "," + vName + "," + vOAmt.
   END.
   ELSE
      pick-value = "".
END.
HIDE FRAME frAcct.

{intrface.del}   

RETURN pick-value.

PROCEDURE SearchCredAcct:
   DEFINE INPUT  PARAMETER iCustID AS INT64     NO-UNDO.
   DEFINE OUTPUT PARAMETER oAcct   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oAmt    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oLoan   AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE vInt AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAmt      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vAmt1     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vAmt3     AS DECIMAL   NO-UNDO.
   
   DEFINE VARIABLE mParams   AS CHARACTER NO-UNDO.
   
   {empty ttLoan}

   FOR EACH loan WHERE
         (loan.close-date EQ ? OR
          loan.close-date GE TODAY)
      AND loan.contract   EQ "КРЕДИТ"
      AND loan.cust-cat   EQ "Ч"
      AND loan.cust-id    EQ iCustID
	   AND loan.filial-id  NE "0400"
      NO-LOCK,
      LAST loan-acct WHERE
           loan-acct.contract  EQ loan.contract
      AND  loan-acct.cont-code EQ loan.cont-code
      AND  loan-acct.acct-type EQ "КредРасч"
      AND  loan-acct.since     LE loan.since
	/*   AND  loan-acct.acct BEGINS "40817"*/
      NO-LOCK,
      FIRST acct WHERE
          acct.acct     EQ loan-acct.acct
      AND acct.currency EQ loan-acct.currency
      NO-LOCK:
   
      IF CAN-DO("!* ОВ*,СОЮЗ*,РОСКАП*,АЛЕКС*",GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"Priznak","")) 
         THEN NEXT.
   
      ASSIGN
         vAmt1 = 0
         vAmt3 = 0.
   
      FIND FIRST ttLoan WHERE
         ttLoan.loan-rid = RECID(loan) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttLoan THEN
      DO:
         vInt  = vInt + 1.
         CREATE ttLoan.
         
         {empty tmprecid} 
         
         CREATE tmprecid. 
         tmprecid.id = RECID(loan). 
         
         mParams = "7+8+233+9+10+12+18+26+210+16+13+14+15+48+248+229+34+82+519".
         
         RUN sum-plat.p (INPUT  mParams).
         
         ASSIGN
            ttLoan.loan-rid  = RECID(loan)
            ttLoan.filial-id = loan.filial-id
            ttLoan.cont-code = loan.cont-code
            ttLoan.acct      = acct.acct
            ttLoan.amt       = DECIMAL(GetSysConf("SumAllPar")).
      END.
      
   END.
   
   ASSIGN
      oAcct = ""
      oAmt  = ""
      oLoan = ""
      vAmt  = 0.
   FOR EACH ttLoan NO-LOCK BREAK BY ttLoan.acct:
      IF FIRST-OF(ttLoan.acct) THEN vAmt = 0.
      vAmt = vAmt + ttLoan.amt.
      oLoan = IF NOT {assigned oLoan} 
         THEN DelFilFromLoan(ttLoan.cont-code) 
         ELSE oLoan + ";" + DelFilFromLoan(ttLoan.cont-code).
      IF LAST-OF(ttLoan.acct) THEN
      DO:
         oAcct = IF NOT {assigned oAcct} 
            THEN ttLoan.acct 
            ELSE oAcct + "," + ttLoan.acct.
/*         oAmt  = TRIM(STRING(vAmt,">>>>>>>>9.99")).*/
         oAmt  = IF NOT {assigned oAmt}
            THEN              TRIM(STRING(vAmt,">>>>>>>>9.99"))
            ELSE oAmt + "," + TRIM(STRING(vAmt,">>>>>>>>9.99")).
      END.
   END.
   RUN SetSysConf IN h_base ("SumAllPar","").
END PROCEDURE.
