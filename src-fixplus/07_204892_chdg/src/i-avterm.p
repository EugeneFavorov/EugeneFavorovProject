/*
                ­ª®¢áª ï ¨­â¥£à¨à®¢ ­­ ï á¨áâ¥¬  ˆ‘ª¢¨â
    Copyright: (C) 1992-2003 ’ŽŽ " ­ª®¢áª¨¥ ¨­ä®à¬ æ¨®­­ë¥ á¨áâ¥¬ë"
     Filename: i-avterm.p
      Comment: § ¯ãáª ¬¥å ­¨§¬  à §¢®à®âª¨ £à ä¨ª®¢ ¯« â¥¦¥© ¯®á«¥
               ¨¬¯®àâ  ¨¯®â¥ç­®£® ªà¥¤¨â  ‚’
   Parameters:
         Uses:
      Used by:
      Created: ˆ«îå 
     Modified:
*/
{globals.i}
{svarloan.def new}
{form.def}
{intrface.get xclass} /* ‡ £àã§ª  ¨­áâàã¬¥­â à¨ï ¬¥â áå¥¬ë  */

{intrface.get loan}

DEF INPUT PARAM iRecLoan    AS RECID NO-UNDO. /*recid ¤®£®¢®à */
DEF INPUT PARAM iRecCond    AS RECID NO-UNDO. /*recid ãá«®¢¨ï*/
DEF INPUT PARAM iSumm       AS DEC   NO-UNDO. /* ‘ã¬¬  (term-obl.idnt = 2)*/


DEF VAR mCredOffset     AS CHAR NO-UNDO.
DEF VAR mSurrLoanCond   AS CHAR NO-UNDO.
DEF VAR mOffset         AS CHAR NO-UNDO INIT ",->,<-".
DEF VAR mDelayOffset    AS CHAR NO-UNDO.  /* á¤¢¨£ ¤ âë ®ª®­ç ­¨ï ¯« â.¯¥à¨®¤  (®á­.¤®«£) */
DEF VAR mDelayOffsetInt AS CHAR NO-UNDO.  /* á¤¢¨£ ¤ âë ®ª®­ç ­¨ï ¯« â.¯¥à¨®¤  (¯à®æ¥­âë) */
DEF VAR mCondCount      AS INT64  NO-UNDO.

RUN RE_B_LOAN_BY_RID (iRecLoan,BUFFER loan).

IF NOT AVAIL loan THEN RETURN.

{svarloan.ini
  &incontr = loan.contract}

FIND FIRST loan-cond WHERE RECID(loan-cond) = iRecCond NO-WAIT NO-ERROR.

IF LOCKED loan-cond THEN RETURN {&RET-ERROR}.

ASSIGN
   mSurrLoanCond        = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
   mCredOffset          = GetXAttrValueEx("loan-cond",mSurrLoanCond,"cred-offset","")
   mDelayOffset         = GetXAttrValueEx("loan-cond",mSurrLoanCond,"delay-offset","")
   mDelayOffsetInt      = GetXAttrValueEx("loan-cond",mSurrLoanCond,"delay-offset-int","")
   .


IF GetXAttrInit(loan-cond.class-code, "‘å¥¬ « â") NE "„¨ää¥à¥­æ¨à®¢ ­­ ï" THEN 
DO:
   loan-cond.int-date   =  loan-cond.cred-date.
   loan-cond.int-period =  loan-cond.cred-period.

   

END.

RELEASE loan-cond.

RUN SetSysConf IN h_base(
   "ŽŸ‡€’…‹œ‘’‚€ Ž ‚Ž‡‚€’“ ‘„‚ˆƒ",
   STRING(LOOKUP(mCredOffset,mOffset))).

RUN SetSysConf IN h_base(
   "‹€’…†ˆ Ž Ž–…’€Œ ‘„‚ˆƒ",
   STRING(LOOKUP(mCredOffset,mOffset))).

RUN SetSysConf IN h_base(
   "ŽŸ‡. Ž ‚Ž‡‚€’“ ‘„‚ˆƒ ŽŠŽ.‘ŽŠ€",
   STRING(LOOKUP(mDelayOffset,mOffset))).

RUN SetSysConf IN h_base(
   "‹€’. Ž Ž–. ‘„‚ˆƒ ŽŠŽ.‘ŽŠ€",
   STRING(LOOKUP(mDelayOffsetInt,mOffset))).

FOR EACH loan-cond WHERE loan-cond.contract  EQ loan.contract
                     AND loan-cond.cont-code EQ loan.cont-code
NO-LOCK:
   mCondCount = mCondCount + 1.
END.

RUN mm-to.p (iRecLoan,
             iRecCond,
             iSumm,
             {&MOD_ADD},
             YES,
             YES,
             YES,
             YES,
             loan.risk,
             mCondCount).

RUN DeleteOldDataProtocol IN h_base("‹€’…†ˆ Ž Ž–…’€Œ ‘„‚ˆƒ").
RUN DeleteOldDataProtocol IN h_base("ŽŸ‡€’…‹œ‘’‚€ Ž ‚Ž‡‚€’“ ‘„‚ˆƒ").

{intrface.del}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:47:42.104+04:00' */
/* $LINTFILE='i-avterm.p' */
/*prosignHKk82gqATEfsJ/oddXh3kg*/