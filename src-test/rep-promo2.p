{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get xobj}
{intrface.get dps}
{intrface.get dpspr}

{sh-defs.i}
{dpsproc.def}

{getdates.i}

{setdest.i &filename = "'rep-promo.txt'"}

DEF VAR mSumm AS DECIMAL NO-UNDO.
DEFINE VARIABLE fCustName    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE vTmp         AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE vAcct         AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE vCustName1    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE vCustName2    AS CHARACTER                 NO-UNDO.

FOR EACH loan WHERE
       loan.contract   EQ 'dps'
   AND loan.open-date  GE beg-date
   AND loan.open-date  LE end-date
   AND loan.close-date EQ ? 
   NO-LOCK:
      
/*   IF GetxattrValueEx("loan",loan.contract + "," + loan.cont-code,"КаналПривл","") NE '02.Интернет' THEN NEXT.*/
    vAcct = ''.
   IF GetxattrValueEx("loan",loan.contract + "," + loan.cont-code,"promo","") NE "" THEN
   DO:
      FIND FIRST acct WHERE
             acct.cust-cat  EQ loan.cust-cat
         AND acct.cust-id   EQ loan.cust-id
         AND acct.open-date LT loan.open-date
         AND CAN-DO('423*',acct.acct)
      NO-LOCK NO-ERROR.
      IF AVAIL(acct) THEN 
      vAcct = 'Да'.
     RUN get-summ-beg-ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT mSumm). 
     RUN GetCustName IN h_base ( 'Ч',
                                   loan.cust-id,
                                                                ?,
                                                                                             OUTPUT vCustName1,OUTPUT vCustName2,INPUT-OUTPUT vTmp).
                                                                                               fCustName = trim(vCustName1) + " " + vCustName2.
                                                                                               
      PUT UNFORMATTED 
         loan.cont-code ";"
         loan.cont-type ";"
         loan.open-date ";"
         mSumm ";"
         fCustName ";" 
         GetxattrValue("loan",loan.contract + "," + loan.cont-code, "КаналПривл") ";" 
         GetxattrValue("loan",loan.contract + "," + loan.cont-code, "promo")";"
         vAcct
      SKIP. 
   END.
END.

{preview.i &filename = "'rep-promo.txt'"}

{intrface.del}

RETURN.
