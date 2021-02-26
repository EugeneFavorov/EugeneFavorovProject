/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ТОО "Банковские информационные системы"
     Filename: pr_chk_saldo.p
      Comment: Красное сальдо
   Parameters:
         Uses:
      Used by:
      Created: 19.08.2010 MUTA 0130259
     Modified: 
*/

DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCnt1      AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt2      AS INT64     NO-UNDO.
DEFINE VARIABLE mAcctMask  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCatMask   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResult    AS LOGICAL NO-UNDO.

DEFINE VARIABLE mBarTotal AS INT64     NO-UNDO.
DEFINE VARIABLE mBarCurnt AS INT64     NO-UNDO.

{filial.def}
{sh-defs.i}
{intrface.get xclass}  

/*mCatMask  = "b".   */
/*mAcctMask = "407*".*/

mCatMask  = ENTRY(1,iParam).
mAcctMask = ENTRY(2,iParam).

{setdest.i &filename = "'chk_red.log'"}

mBarTotal = 0.
FOR EACH acct WHERE TRUE
   AND acct.filial-id EQ shFilial
   AND CAN-DO(mCatMask,acct.acct-cat)
   AND CAN-DO(mAcctMask,acct.acct)    
   AND acct.close-date EQ ?
   NO-LOCK:
   mBarTotal = mBarTotal + 1.  
END.

{bar-beg2.i
   &BarTotal     = mBarTotal
   &BarMessage   = "'Проверка остатков..'"}

mCnt1 = 0.
mCnt2 = 0.
mBarCurnt = 0.
FOR EACH acct WHERE TRUE
   AND acct.filial-id EQ shFilial
   AND CAN-DO(mCatMask,acct.acct-cat)
   AND CAN-DO(mAcctMask,acct.acct)    
   AND acct.close-date EQ ?
   NO-LOCK:

   mBarCurnt = mBarCurnt + 1.
   mCnt1 = mCnt1 + 1.
   
   {bar2.i
      &BarPointer = mBarCurnt}
   
   RUN ChkAcctSaldo(acct.acct,acct.currency,OUTPUT mResult).
   
   IF mResult EQ YES THEN
   DO:
      mCnt2 = mCnt2 + 1.
      PUT UNFORMATTED mCnt2 ";" acct.acct SKIP.
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","mCnt2 = " + TRIM(STRING(mCnt2)) + " " + acct.acct + " " + STRING(mResult)).
   END.
END.

PUT UNFORMATTED "Обработано " TRIM(STRING(mCnt1)) + " счетов." SKIP.

IF mCnt2 EQ 0 THEN
   PUT UNFORMATTED "~nAll Correct." SKIP.

{preview.i &filename = "'chk_red.log'"}

{intrface.del} 

PROCEDURE ChkAcctSaldo:
   DEFINE INPUT PARAMETER  iAcct     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER  iCurrency AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult   AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE vMess      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vLimOver   AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vBlPos     AS CHARACTER  NO-UNDO.

   {find-act.i
      &acct = iAcct
      &curr = iCurrency
   }
   
   oResult = NO.
      
   IF AVAIL(acct) 
      AND acct.acct-cat NE "d" THEN
   DO:
      RUN acct-pos IN h_base 
         (
         acct.acct,
         acct.currency,
         TODAY,
         TODAY,
         CHR(251)
         ).
   
      IF (Acct.side EQ "П" AND 
         ((sh-bal GT vLimOver AND Acct.currency EQ "") OR 
          (sh-val GT vLimOver AND Acct.currency NE ""))) OR
         (Acct.side EQ "А" AND 
         (( - sh-bal GT vLimOver AND Acct.currency EQ "") OR 
          ( - sh-val GT vLimOver AND Acct.currency NE ""))) 
          THEN oResult = YES.
   END.

END PROCEDURE.

