/*
               Å†≠™Æ¢·™†Ô ®≠‚•£‡®‡Æ¢†≠≠†Ô ·®·‚•¨† Åàë™¢®‚
    Copyright: (C) 1992-2012 áÄé "Å†≠™Æ¢·™®• ®≠‰Æ‡¨†Ê®Æ≠≠Î• ·®·‚•¨Î"
     Filename: memorn_ops.p
      Comment: è•Á†‚Ï ¨≠Æ£ÆØ‡Æ¢Æ§ÆÁ≠Æ£Æ åé (éëè)
      Comment: 
   Parameters: Input: RID -  RecID(op)
         Uses: 
      Used by:
      Created: 13/01/2012 kraw (0159435)
     Modified: 13/04/2012 kraw (0168692) Ç LaTeX'Æ¢Æ© ØÆ§Ø®·® ≠• ·‚†¢®‚Ï ´®Ë≠®• Ø•‡•¢Æ§Î ·‚‡Æ™® ¢ ‚•£†Â
*/ 
{globals.i}                                 /* £´Æ°†´Ï≠Î• Ø•‡•¨•≠≠Î•         */
{intrface.get tmess} 
{intrface.get tmcod}  /* ê†°Æ‚† · ‚•¨ØÆ‡†´Ï≠Î¨® ™´†··®‰®™†‚Æ‡†¨®  */
{intrface.get sgntr}
{intrface.get instrum}
{intrface.get exch}  
{intrface.get cust}  
{intrface.get prnvd}  

/* Ö·´® ®ß¨•≠Ô•‚• Ì‚Æ, ≠• ß†°„§Ï‚• ‚†™¶• ®·Ø‡†¢®‚Ï ¢ userinfo.p */
&SCOPED-DEFINE OP-SURROGATE-SYSCONF "signatures-op-surrogate"

DEFINE VARIABLE mAcctDetails   AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE VARIABLE mAcct          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mRubDb         AS DECIMAL            NO-UNDO.
DEFINE VARIABLE mCurDb         AS DECIMAL            NO-UNDO.
DEFINE VARIABLE mCurrency      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mQty           AS INT64            NO-UNDO.
DEFINE VARIABLE mDocType       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mNumProc       AS INT64            NO-UNDO.
DEFINE VARIABLE mSignatures    AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE mNumLines      AS INT64            NO-UNDO.
DEFINE VARIABLE mTotalPagesPos AS INT64            NO-UNDO.
DEFINE VARIABLE mPrevPageNum   AS INT64 INIT 0     NO-UNDO.
DEFINE VARIABLE mSimpleGroup   AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mCurAcctDB     AS LOGICAL            NO-UNDO.

DEFINE VARIABLE mQuanDocs      AS INT64            NO-UNDO.
DEFINE VARIABLE mQuanPages     AS INT64            NO-UNDO.
DEFINE VARIABLE mStrTMP        AS CHARACTER          NO-UNDO.

DEFINE VARIABLE mPageNumber    AS INT64 INITIAL 1  NO-UNDO.
DEFINE VARIABLE mPageSize      AS INT64            NO-UNDO.

DEFINE VARIABLE mItem AS INT64 NO-UNDO.

DEFINE VARIABLE mCanBeSplitted AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mDoSplit       LIKE ToSplit          NO-UNDO.
DEFINE VARIABLE mStrPar        AS CHARACTER          NO-UNDO.

DEFINE VARIABLE mSuppressEquiv AS LOGICAL            NO-UNDO.

DEFINE VARIABLE mDBCurr        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mPutToScreen   AS LOGICAL            NO-UNDO.

{chkacces.i}

FUNCTION PageBreak RETURNS LOGICAL (INPUT  iTolerance AS INT64):

   IF mPageSize > 0 AND mPageSize - LINE-COUNTER < iTolerance THEN
      RETURN YES.
   ELSE
      RETURN NO.

END FUNCTION.

FUNCTION GetMemornAuthor RETURN CHARACTER PRIVATE (INPUT iSetting    AS CHARACTER,
                                                   INPUT iBankName   AS CHARACTER,
                                                   INPUT iBranchName AS CHARACTER):

   CASE TRIM(iSetting):
      WHEN "Å†≠™" OR WHEN "Å" THEN
         RETURN iBankName.
      WHEN "èÆ§‡†ß§•´•≠®•" OR WHEN "è" THEN
         RETURN iBranchName.
      WHEN "Åè" THEN
         RETURN iBankName + (IF iBranchName = "" THEN "" 
                                                 ELSE (" " + iBranchName)).
      WHEN "èÅ" THEN
         RETURN iBranchName + (IF iBankName = "" THEN "" 
                                                 ELSE (" " + iBankName)).
   END.
   RETURN "".
END FUNCTION.

&GLOBAL-DEFINE op-entry yes
&GLOBAL-DEFINE tt-op-entry yes
&SCOPED-DEFINE MORDER YES

{pp-uni.var &multiline-author = YES}        /* ÆØ‡•§•´•≠®• Ø•‡•¨•≠≠ÎÂ        */
{pp-uni.err}                                /* ·ÆÆ°È•≠®Ô Æ° ÆË®°™†Â          */
{pp-uni.prg}                     /* ÆØ®·†≠®• ·‚†≠§†‡‚≠ÎÂ Ø‡ÆÊ•§„‡ */

{mo-uni.dec}

{def-wf.i NEW}
{mo-pars.i}

mStrPar = FGetSetting("è´†‚ÑÆ™", "å•¨é‡§•‡", "").
RUN ProcProcessFreeField(mStrPar,?,?).

&IF DEFINED(CONSOLIDATED) &THEN
FOR EACH tt-op NO-LOCK:
   rid = tt-op.rcd.
&ENDIF
   {mo-uni.chk}                                /* Ø‡Æ¢•‡™† ¢ÂÆ§≠ÎÂ §†≠≠ÎÂ       */
&IF DEFINED(CONSOLIDATED) &THEN
END.
&ENDIF

RUN DecomposeMO.

&IF DEFINED(CONSOLIDATED) &THEN
FOR EACH mo-entry:
   mo-entry.grp-num = 1.
END.
vGroupNum = 1.

FOR EACH mo-entry NO-LOCK:
   IF mo-entry.acct-db <> ? THEN DO:
      IF mAcctDbCheck = ""
      THEN mAcctDbCheck = mo-entry.acct-db.
      ELSE IF mAcctDbCheck <> mo-entry.acct-db
           THEN mAcctDbBad = YES.
   END.
   IF mo-entry.acct-cr <> ? THEN DO:
      IF mAcctCrCheck = ""
      THEN mAcctCrCheck = mo-entry.acct-cr.
      ELSE IF mAcctCrCheck <> mo-entry.acct-cr
           THEN mAcctCrBad = YES.
   END.
   IF mAcctDbBad AND mAcctCrBad THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Ç §•°•‚• ®´® ™‡•§®‚• Æ‚Æ°‡†≠≠ÎÂ §´Ô Ø•Á†‚® §Æ™„¨•≠‚Æ¢ §Æ´¶•≠ °Î‚Ï Æ§®≠†™Æ¢Î© ·Á•‚.").
      {intrface.del}
      RETURN.
   END.
END.

&ELSE
RUN CheckIfComplex(NO, OUTPUT mCanBeSplitted).

mDoSplit = ToSplit.
IF NOT PackagePrint THEN mDoSplit = ?.
IF mDoSplit = ? THEN DO:
   mDoSplit = NO.

   IF mCanBeSplitted AND vGroupNum = 1 THEN DO:
      mPutToScreen = GetProcSettingByCode("ëë_ÇÎ¢Æ§ç†ù™‡†≠") EQ "Ñ†" NO-ERROR.
      &IF DEFINED(SESSION-REMOTE) &THEN
         pick-value = (IF mDoSplit = YES THEN "yes" ELSE "no").
         RUN Fill-SysMes ("", "", "4", "ê†ß°®¢†‚Ï?").
         mDoSplit = (pick-value = "yes").
      &ELSE
         DEFINE VARIABLE vStreamPos AS INT64 NO-UNDO.
         vStreamPos = SEEK(OUTPUT).
         IF mPutToScreen THEN
         MESSAGE "ê†ß°®¢†‚Ï?" UPDATE mDoSplit.
         SEEK OUTPUT TO vStreamPos.
      &ENDIF
      IF NOT mPutToScreen THEN
         mDoSplit = (FGetSetting("è´†‚ÑÆ™","ê†ß°®¢é‡§","ç•‚") EQ "Ñ†") NO-ERROR.
      ToSplit = mDoSplit.
   END.
END.

IF mDoSplit = YES OR vGroupNum > 1 THEN
    RUN TotallyDecomposeMO.
&ENDIF

/*
{strtout3.i &cols=84}             /* ØÆ§£Æ‚Æ¢™† ™ ¢Î¢Æ§„           */

IF NOT AVAILABLE printer THEN
DO:
   FIND FIRST printer WHERE printer.printer   EQ usr-printer
                        AND printer.page-cols GE 84
      NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE printer THEN
      FIND LAST printer WHERE printer.printer EQ usr-printer
         NO-LOCK NO-ERROR.
     
   IF NOT AVAILABLE printer THEN
      mPageSize = 56.
   ELSE
      mPageSize = printer.page-lines.
END.
ELSE
   mPageSize = printer.page-lines.
*/

FOR EACH tt-op-entry NO-LOCK,
FIRST op WHERE op.op = tt-op-entry.op NO-LOCK:
   {empty Info-Store}
   {pp-uni.run}                                /* ≠•ØÆ·‡•§·‚¢•≠≠Æ ‡†·Á•‚        */
END.

ASSIGN
   &IF DEFINED(CONSOLIDATED) &THEN
   mQuanDocs  = ?
   mQuanPages = ?
   &ELSE
   mQuanDocs  = INT64(GetXAttrValueEx("op", STRING(op.op), "è‡®´ÑÆ™„¨",  "?"))
   mQuanPages = INT64(GetXAttrValueEx("op", STRING(op.op), "è‡®´ã®·‚Æ¢", "?"))
   &ENDIF
.

mNumProc  = INT64(GetSysConf("user-proc-id")) NO-ERROR.
FIND FIRST user-proc WHERE RECID(user-proc) EQ mNumProc
   NO-LOCK NO-ERROR.

IF (AVAIL(user-proc) AND 
    user-proc.procedure NE ENTRY(1, PROGRAM-NAME(1), ".")) OR NOT AVAIL(user-proc)  THEN DO:

   FIND FIRST user-proc WHERE user-proc.PROCEDURE EQ ENTRY(1, PROGRAM-NAME(1), ".")
      NO-LOCK NO-ERROR.
   IF AVAIL(user-proc) THEN
      RUN SetSysConf IN h_base ("user-proc-id",RECID(user-proc)). /* §´Ô signatur.i */
END.

IF AVAIL(user-proc) THEN 
    
   mSignatures = GetXAttrValue("user-proc",
                 STRING(user-proc.public-number),
                 "èÆ§Ø®·®").  

IF {assigned mSignatures} THEN 
   RUN CalcSignLines(INPUT mSignatures, OUTPUT mNumLines).

DEFINE VARIABLE vGrp AS INT64 NO-UNDO.

DO vGrp = 1 TO vGroupNum:
   RUN PrintGroup(vGrp).
END.
{empty mo-entry}
vGroupNum = 0.

IF AVAIL(user-proc) AND RECID(user-proc) NE mNumProc THEN
   RUN SetSysConf IN h_base ("user-proc-id",mNumProc).

PROCEDURE PrintGroup.
   DEFINE INPUT PARAMETER iGrpNum AS INT64 NO-UNDO.
   DEFINE VAR vAcctCat   LIKE op.acct-cat NO-UNDO.
   DEFINE VAR vOpForReqv LIKE op.op NO-UNDO.
   DEFINE BUFFER x-mo-entry FOR mo-entry.

   ASSIGN
      mPrevPageNum = mPageNumber
      mRubDb       = 0.0
      mCurDb       = 0.0
      mCurrency    = ""
      mQty         = 0
      mSuppressEquiv = NO
   .

   {empty mo-entry-grp}
   {empty tt-op-entry-collected}
   FOR EACH mo-entry WHERE mo-entry.grp-num = iGrpNum
                       AND mo-entry.acct-db > "",
   FIRST op WHERE op.op = mo-entry.op NO-LOCK,
   EACH tt-op-entry WHERE tt-op-entry.op       = op.op
                      AND tt-op-entry.op-entry = mo-entry.op-entry
   NO-LOCK:
      FIND FIRST op-entry WHERE op-entry.op       = tt-op-entry.op
                            AND op-entry.op-entry = tt-op-entry.op-entry-half-db
      NO-LOCK NO-ERROR.
      IF AVAIL(op-entry) AND op-entry.amt-rub EQ 0.00 AND op.op-status BEGINS "Ä"  /* †≠≠„´®‡Æ¢†≠≠Î© §Æ™„¨•≠‚ ØÆ·‚†≠Æ¢™® ≠† ™†‡‚-™„ 2 */
      THEN RELEASE op-entry.

      IF tt-op-entry.IsHalfEntryComposited THEN DO:
         IF CAN-FIND(FIRST tt-op-entry-collected WHERE tt-op-entry-collected.op       = tt-op-entry.op
                                                   AND tt-op-entry-collected.op-entry = (IF AVAIL op-entry THEN op-entry.op-entry ELSE tt-op-entry.op-entry-half-db)
                     NO-LOCK)
         THEN NEXT.
         CREATE tt-op-entry-collected.
         ASSIGN
            tt-op-entry-collected.op       = tt-op-entry.op
            tt-op-entry-collected.op-entry = (IF AVAIL op-entry THEN op-entry.op-entry ELSE tt-op-entry.op-entry-half-db)
        .
      END.

      FIND FIRST mo-entry-grp WHERE mo-entry-grp.grp-num = mo-entry.grp-num
                                AND mo-entry-grp.acct-db = mo-entry.acct-db
      EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL mo-entry-grp THEN DO:
         CREATE mo-entry-grp.
         ASSIGN
            mo-entry-grp.grp-num = mo-entry.grp-num
            mo-entry-grp.acct-db = mo-entry.acct-db
         .
         {find-act.i &acct = mo-entry-grp.acct-db}
         IF AVAIL acct THEN mo-entry-grp.currency = acct.currency.
      END.
      ASSIGN
         mo-entry-grp.amt-rub = mo-entry-grp.amt-rub + (IF AVAIL op-entry THEN op-entry.amt-rub ELSE tt-op-entry.amt-rub)
         mo-entry-grp.amt-cur = mo-entry-grp.amt-cur + (IF AVAIL op-entry THEN op-entry.amt-cur ELSE tt-op-entry.amt-cur)
         mo-entry-grp.qty     = mo-entry-grp.qty     + (IF AVAIL op-entry THEN op-entry.qty     ELSE tt-op-entry.qty)
      .
      &IF DEFINED(CONSOLIDATED) &THEN
      CASE mDocSort:
         WHEN "2" THEN mo-entry-grp.SortField = STRING(mo-entry-grp.currency,"x(3)") + STRING((IF mo-entry-grp.currency <> "" THEN mo-entry-grp.amt-cur ELSE mo-entry-grp.amt-rub),"-999999999999999999.99").
         WHEN "3" THEN mo-entry-grp.SortField = SUBSTR(mo-entry-grp.acct-db,1,8) + SUBSTR(mo-entry-grp.acct-db,10,11).
         WHEN "4" THEN mo-entry-grp.SortField = STRING(mo-entry-grp.acct-db).         
         OTHERWISE mo-entry-grp.SortField = STRING(op.op,"999999999999999999").
      END CASE.
      &ELSE
      mo-entry-grp.SortField = STRING(op.op,"999999999999999999").
      &ENDIF
      ASSIGN
         mRubDb    = mRubDb + (IF AVAIL op-entry THEN op-entry.amt-rub ELSE tt-op-entry.amt-rub)
         mCurDb    = mCurDb + (IF AVAIL op-entry THEN op-entry.amt-cur ELSE tt-op-entry.amt-cur)
         mQty      = mQty   + (IF AVAIL op-entry THEN op-entry.qty     ELSE tt-op-entry.qty)
         mCurrency = (IF AVAIL op-entry THEN op-entry.currency ELSE tt-op-entry.currency)
         vAcctCat  = op.acct-cat
      .
   END.

   RELEASE op.
   &IF DEFINED(CONSOLIDATED) &THEN
   IF mDocNumType = "1" THEN DO:
   &ENDIF
      FOR FIRST mo-entry-grp WHERE mo-entry-grp.grp-num = iGrpNum BY mo-entry-grp.SortField:
         FIND FIRST mo-entry WHERE mo-entry.grp-num = mo-entry-grp.grp-num
                               AND mo-entry.acct-db = mo-entry-grp.acct-db
         NO-LOCK.
         &IF DEFINED(CONSOLIDATED) &THEN
         FOR FIRST tt-op BY tt-op.op:
            FIND FIRST op WHERE op.op = tt-op.op NO-LOCK.
         END.
         &ELSE
         FIND FIRST op WHERE op.op = mo-entry.op NO-LOCK NO-ERROR.
         &ENDIF
      END.
   &IF DEFINED(CONSOLIDATED) &THEN
   END.
   IF mDocNumType = "2" THEN DO:
      FIND FIRST tt-op WHERE CAN-FIND(FIRST op WHERE op.op      = tt-op.op
                                                 AND op.doc-num = mDocNum)
      NO-LOCK NO-ERROR.
      IF AVAIL tt-op
      THEN FIND FIRST op WHERE op.op = tt-op.op NO-LOCK.
   END.
   &ENDIF
   mDocType = "".
   IF AVAIL op THEN DO:
      vOpForReqv = op.op.
      &IF DEFINED(CONSOLIDATED) &THEN
      ASSIGN
         mDocNum    = op.doc-num
         mQuanDocs  = INT64(GetXAttrValueEx("op",
                                              STRING(vOpForReqv),
                                              "è‡®´ÑÆ™„¨",
                                              "?"))
         mQuanPages = INT64(GetXAttrValueEx("op",
                                              STRING(vOpForReqv),
                                              "è‡®´ã®·‚Æ¢",
                                              "?"))
      NO-ERROR.
      &ENDIF
      FIND FIRST doc-type WHERE doc-type.doc-type EQ op.doc-type NO-LOCK NO-ERROR.
      IF AVAILABLE doc-type
      THEN mDocType = doc-type.digital.
      ELSE mDocType = op.doc-type.
   END.
   ELSE ASSIGN
      mDocType   = "09"
      vOpForReqv = ?
   .
   IF AVAIL op THEN DO:
      Author[1] = GetUserBranchId(op.user-id).
      FIND FIRST branch WHERE branch.branch-id EQ Author[1] NO-LOCK NO-ERROR.
      Author[1] = IF AVAILABLE branch THEN branch.name ELSE "".
   END.
   ELSE Author[1] = "".
   Author[1] = GetMemornAuthor(FGetSetting("è´†‚ÑÆ™", "ëÆ·‚åÆ", "èÆ§‡†ß§•´•≠®•"),
                               FGetSetting("Å†≠™", ?, ""),
                               Author[1]).
   Author[1] = "ëÆ·‚†¢®‚•´Ï " + Author[1].
   ASSIGN 
      Author[2] = ""
      Author[3] = ""
   .
/*   {wordwrap.i &s=Author &n=3 &l=61}
   RUN ShiftArrayDown3(INPUT-OUTPUT Author).*/
   RUN memorn_pre.

   RUN BeginCircle_TTName IN h_prnvd ("DB").

   FOR EACH mo-entry-grp WHERE mo-entry-grp.grp-num = iGrpNum BY mo-entry-grp.SortField:
      /*
       è‡Æ¢•‡™†, ·Æ·‚Æ®‚ ´® £‡„ØØ† ®ß •§®≠·‚¢•≠≠Æ© ØÆ´≠Æ© Ø‡Æ¢Æ§™® ®´®
       ≠•·™Æ´Ï™®Â ØÆ´≠ÎÂ Ø‡Æ¢Æ§Æ™ · Æ§®≠†™Æ¢Æ© ™Æ‡‡•·ØÆ≠§•≠Ê®•© ·Á•‚Æ¢
      */
      FIND FIRST mo-entry WHERE mo-entry.grp-num = mo-entry-grp.grp-num NO-LOCK.
      mSimpleGroup = NOT CAN-FIND(FIRST x-mo-entry WHERE x-mo-entry.grp-num = mo-entry.grp-num
                                                     AND (   x-mo-entry.acct-db <> mo-entry.acct-db
                                                          OR x-mo-entry.acct-cr <> mo-entry.acct-cr)).
      RUN PrintOneBlock (BUFFER mo-entry-grp,"DB").
      RUN NextCircle_TTName IN h_prnvd ("DB").
   END.
   RUN EndCircle_TTName IN h_prnvd ("DB").
/*
   PUT UNFORMATTED "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥              ≥" SKIP.
   PUT UNFORMATTED " ç†®¨•≠Æ¢†≠®• ·Á•‚†            ≥ä‡•§®‚ ·Á•‚†        ≥              ≥" SKIP.
   PUT UNFORMATTED "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥              ≥" SKIP.
*/
   {empty mo-entry-grp}
   {empty tt-op-entry-collected}
   FOR EACH mo-entry WHERE mo-entry.grp-num = iGrpNum
                       AND mo-entry.acct-cr > "",
   FIRST op WHERE op.op = mo-entry.op NO-LOCK,
   EACH tt-op-entry WHERE tt-op-entry.op       = op.op
                      AND tt-op-entry.op-entry = mo-entry.op-entry
   NO-LOCK:
      FIND FIRST op-entry WHERE op-entry.op       = tt-op-entry.op
                            AND op-entry.op-entry = tt-op-entry.op-entry-half-cr
      NO-LOCK NO-ERROR.
      IF AVAIL(op-entry) AND op-entry.amt-rub EQ 0.00 AND op.op-status BEGINS "Ä"  /* †≠≠„´®‡Æ¢†≠≠Î© §Æ™„¨•≠‚ ØÆ·‚†≠Æ¢™® ≠† ™†‡‚-™„ 2 */
      THEN RELEASE op-entry.

      IF tt-op-entry.IsHalfEntryComposited THEN DO:
         IF CAN-FIND(FIRST tt-op-entry-collected WHERE tt-op-entry-collected.op       = tt-op-entry.op
                                                   AND tt-op-entry-collected.op-entry = (IF AVAIL op-entry THEN op-entry.op-entry ELSE tt-op-entry.op-entry-half-cr)
                     NO-LOCK)
         THEN NEXT.
         CREATE tt-op-entry-collected.
         ASSIGN
            tt-op-entry-collected.op       = tt-op-entry.op
            tt-op-entry-collected.op-entry = (IF AVAIL op-entry THEN op-entry.op-entry ELSE tt-op-entry.op-entry-half-cr)
        .
      END.

      FIND FIRST mo-entry-grp WHERE mo-entry-grp.grp-num = mo-entry.grp-num
                                AND mo-entry-grp.acct-cr = mo-entry.acct-cr
      EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL mo-entry-grp THEN DO:
         CREATE mo-entry-grp.
         ASSIGN
            mo-entry-grp.grp-num = mo-entry.grp-num
            mo-entry-grp.acct-cr = mo-entry.acct-cr
         .
         {find-act.i &acct = mo-entry-grp.acct-cr}
         IF AVAIL acct THEN mo-entry-grp.currency = acct.currency.
      END.
      ASSIGN
         mo-entry-grp.amt-rub = mo-entry-grp.amt-rub + (IF AVAIL op-entry THEN op-entry.amt-rub ELSE tt-op-entry.amt-rub)
         mo-entry-grp.amt-cur = mo-entry-grp.amt-cur + (IF AVAIL op-entry THEN op-entry.amt-cur ELSE tt-op-entry.amt-cur)
         mo-entry-grp.qty     = mo-entry-grp.qty     + (IF AVAIL op-entry THEN op-entry.qty     ELSE tt-op-entry.qty)
      .
      &IF DEFINED(CONSOLIDATED) &THEN
      CASE mDocSort:
         WHEN "2" THEN mo-entry-grp.SortField = STRING(mo-entry-grp.currency,"x(3)") + STRING((IF mo-entry-grp.currency <> "" THEN mo-entry-grp.amt-cur ELSE mo-entry-grp.amt-rub),"-999999999999999999.99").
         WHEN "3" THEN mo-entry-grp.SortField = SUBSTR(mo-entry-grp.acct-cr,1,8) + SUBSTR(mo-entry-grp.acct-cr,10,11).
         WHEN "4" THEN mo-entry-grp.SortField = STRING(mo-entry-grp.acct-cr).          
         OTHERWISE mo-entry-grp.SortField = STRING(op.op,"999999999999999999").
      END CASE.
      &ELSE
      mo-entry-grp.SortField = STRING(op.op,"999999999999999999").
      &ENDIF
   END.

   RUN BeginCircle_TTName IN h_prnvd ("CR").
   FOR EACH mo-entry-grp WHERE mo-entry-grp.grp-num = iGrpNum BY mo-entry-grp.SortField:
      RUN PrintOneBlock (BUFFER mo-entry-grp,"CR").
      RUN NextCircle_TTName IN h_prnvd ("CR").
   END.
   RUN EndCircle_TTName IN h_prnvd ("CR").

   IF vAcctCat NE "d" THEN
      RUN x-amtstr.p(mRubDb,"", TRUE, (IF TRUNCATE(mRubDb,0) <> mRubDb THEN TRUE ELSE ?), OUTPUT amtstr[1], OUTPUT amtstr[2]).
   ELSE
   DO:
      RUN x-amtstr.p(mQty,mCurrency, NO, NO, OUTPUT amtstr[1], OUTPUT amtstr[2]).
      amtstr[2] = "".
   END.
   ASSIGN 
      AmtStr[1] = TRIM(AmtStr[1] + " " + AmtStr[2])
      AmtStr[2] = "".
/*
   IF LENGTH(AmtStr[1]) > 63 THEN DO:
     {wordwrap.i &s=AmtStr &n=3 &l=63}
   END.
*/
   FIND FIRST op WHERE op.op = vOpForReqv NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN Detail = "".
   RUN memorn_post.

END PROCEDURE.

PROCEDURE PrintOneBlock:
   DEFINE PARAMETER BUFFER mo-entry-grp FOR mo-entry-grp.
   DEFINE INPUT PARAMETER iType   AS CHAR    NO-UNDO.

   IF iType = "DB"
   THEN DO:
      FIND FIRST mo-entry WHERE mo-entry.grp-num = mo-entry-grp.grp-num
                            AND mo-entry.acct-db = mo-entry-grp.acct-db
      NO-LOCK.
      mAcct = mo-entry.acct-db.
   END.
   ELSE DO:
      FIND FIRST mo-entry WHERE mo-entry.grp-num = mo-entry-grp.grp-num
                            AND mo-entry.acct-cr = mo-entry-grp.acct-cr
      NO-LOCK.
      mAcct = mo-entry.acct-cr.
   END.

   FIND FIRST op WHERE op.op = mo-entry.op NO-LOCK.
   FIND FIRST tt-op-entry WHERE tt-op-entry.op       = op.op
                            AND tt-op-entry.op-entry = mo-entry.op-entry NO-LOCK.
   IF iType = "DB"
   THEN FIND FIRST op-entry WHERE op-entry.op       = tt-op-entry.op
                              AND op-entry.op-entry = tt-op-entry.op-entry-half-db NO-LOCK NO-ERROR.
   ELSE FIND FIRST op-entry WHERE op-entry.op       = tt-op-entry.op
                              AND op-entry.op-entry = tt-op-entry.op-entry-half-cr NO-LOCK NO-ERROR.

   IF iType = "DB" THEN DO:
      RUN ProcProcessFreeField(mStrPar, tt-op-entry.op, (IF AVAIL op-entry THEN op-entry.op-entry ELSE ?)).
   END.

   RUN PEntry(mo-entry-grp.amt-rub, mo-entry-grp.amt-cur, mo-entry-grp.qty, iType, mo-entry-grp.currency).
END PROCEDURE.
/*
{endout3.i  &nofooter=yes}                   /* ß†¢•‡Ë•≠®• ¢Î¢Æ§†             */ 
*/
RUN prnvd IN h_prnvd ("memorn").
{intrface.del}          /* ÇÎ£‡„ß™† ®≠·‚‡„¨•≠‚†‡®Ô. */ 

PROCEDURE memorn_pre:
   DEFINE VARIABLE vI      AS INT64     NO-UNDO.
   DEFINE VARIABLE vDocNum AS CHARACTER NO-UNDO.

/*
   PUT UNFORMATTED
   "                                                               ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø" SKIP
   "                                                               ≥äÆ§ ‰Æ‡¨Î §Æ™„¨•≠‚†≥" SKIP
   "                                                               ≥      ØÆ éäìÑ      ≥" SKIP
   "                                                               √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥" SKIP
   " " Author[1] FORMAT "x(61)"                                  " ≥      0401108      ≥" SKIP
   " " Author[2] FORMAT "x(61)"                                  " ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ" SKIP
   " " Author[3] FORMAT "x(61)" SKIP(1)
   .
*/
   RUN Insert_TTName IN h_prnvd ("Author", Author[1]).

   IF AVAILABLE op THEN
   DO:
      {isispmem.i}
   END.
   ELSE
      mIsIspravl = NO.

   &IF DEFINED(CONSOLIDATED) <> 0 &THEN
      IF FGetSetting("ä„‡·ê", "Å„™ç¨‡", "NO") EQ "NO" THEN
         DO vI = 1 TO LENGTH(mDocNum):
            IF NOT CAN-DO("1,2,3,4,5,6,7,8,9,0",SUBSTRING(mDocNum,1,1)) THEN
               mDocNum = SUBSTRING(mDocNum,2,LENGTH(mDocNum) - 1).
            ELSE LEAVE.
         END.
      vDocNum = mDocNum. 
   &ELSE
      vDocNum = op.doc-num. 
      IF FGetSetting("ä„‡·ê", "Å„™ç¨‡", "NO") EQ "NO" THEN
         DO vI = 1 TO LENGTH(vDocNum):
            IF NOT CAN-DO("1,2,3,4,5,6,7,8,9,0",SUBSTRING(vDocNum,1,1)) THEN
               vDocNum = SUBSTRING(vDocNum,2,LENGTH(vDocNum) - 1).
            ELSE LEAVE.
         END.                 
   &ENDIF

   IF mIsIspravl THEN /*PUT UNFORMATTED " åÖåéêàÄãúçõâ àëèêÄÇàíÖãúçõâ éêÑÖê N "*/
      RUN Insert_TTName IN h_prnvd ("order-type", "åÖåéêàÄãúçõâ àëèêÄÇàíÖãúçõâ éêÑÖê").
   ELSE /*PUT UNFORMATTED " åÖåéêàÄãúçõâ éêÑÖê N "*/
      RUN Insert_TTName IN h_prnvd ("order-type", "åÖåéêàÄãúçõâ éêÑÖê").

   RUN Insert_TTName IN h_prnvd ("theDate", theDate).
   RUN Insert_TTName IN h_prnvd ("DocNum", vDocNum).

/*
   PUT UNFORMATTED
   "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP
   " " FORMAT "x(46)" "Ñ†‚†" SKIP
   "                                                                    " /*ë¢Æ°Æ§≠Æ• ØÆ´• (§´Ô ¢Î¢Æ§† ™Æ´®Á•·‚¢† ·‚‡†≠®Ê)*/
   .
*/
   mTotalPagesPos = SEEK(OUTPUT).
/*
   PUT UNFORMATTED "                " SKIP(1)
   "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP
   " ç†®¨•≠Æ¢†≠®• ·Á•‚†            ≥Ñ•°•‚ ·Á•‚†         ≥ë„¨¨† Ê®‰‡†¨®" SKIP
   "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP
   .
*/
END PROCEDURE.

PROCEDURE memorn_post.

   RUN GetDetailManual.
/*
   IF PageBreak(INPUT  23 + mNumLines) THEN DO:
      PUT UNFORMATTED "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP.
      RUN PrintSignatures.
      PUT UNFORMATTED "                                                                                  " STRING(mPageNumber - mPrevPageNum + 1,">9") SKIP.
      PAGE.
      mPageNumber = mPageNumber + 1.
      PUT UNFORMATTED
      SKIP(1)
      "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒ" SKIP
      .
   END.
   ELSE
   PUT UNFORMATTED "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒ¡ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒ" SKIP.
*/

   RUN Insert_TTName IN h_prnvd ("AmtStr", AmtStr[1]).
   RUN Insert_TTName IN h_prnvd ("DocType", mDocType).
   RUN Insert_TTName IN h_prnvd ("VarName14", mVarName[14]).
   RUN Insert_TTName IN h_prnvd ("VarVal14", mVarVal[14]).
   RUN Insert_TTName IN h_prnvd ("VarName15", mVarName[15]).
   RUN Insert_TTName IN h_prnvd ("VarVal15", mVarVal[15]).
   RUN Insert_TTName IN h_prnvd ("Detail", Detail[1]).

/*
   PUT UNFORMATTED
   " ë„¨¨† Ø‡ÆØ®·ÏÓ                                                 ≥ ò®‰‡     ≥"  SKIP
   "                                                                ≥ §Æ™„¨•≠‚†≥ " mDocType FORMAT "x(2)" SKIP
   AmtStr[1] FORMAT "x(63)"                                               " √ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ" SKIP
   AmtStr[2] FORMAT "x(63)"                                               " ≥ " mVarName[14] FORMAT "x(9)" "≥ " mVarVal[14] FORMAT "x(5)" SKIP
   AmtStr[3] FORMAT "x(63)"                                               " √ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ" SKIP
   "                                                                ≥ " mVarName[15] FORMAT "x(9)" "≥ " mVarVal[15] FORMAT "x(5)" SKIP
   "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒ" SKIP
   " ëÆ§•‡¶†≠®• ÆØ•‡†Ê®®, ≠†®¨•≠Æ¢†≠®•, ≠Æ¨•‡ ® §†‚† §Æ™„¨•≠‚†," SKIP
   " ≠† Æ·≠Æ¢†≠®® ™Æ‚Æ‡Æ£Æ ·Æ·‚†¢´•≠ ¨•¨Æ‡®†´Ï≠Î© Æ‡§•‡" SKIP
   Detail[1] FORMAT "x(80)" SKIP
   Detail[2] FORMAT "x(80)" SKIP
   Detail[3] FORMAT "x(80)" SKIP
   Detail[4] FORMAT "x(80)" SKIP
   Detail[5] FORMAT "x(80)" SKIP
   "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP
   .
*/
   RUN PrintSignatures.
/*
   PUT UNFORMATTED
   "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP(1)
   "è‡®´Æ¶•≠®•: "
   .
*/

   IF mQuanDocs EQ ? THEN
      RUN Insert_TTName IN h_prnvd ("QuanDocs", "").
   ELSE
      RUN Insert_TTName IN h_prnvd ("QuanDocs", STRING(mQuanDocs)).

   IF mQuanPages EQ ? THEN
      RUN Insert_TTName IN h_prnvd ("QuanPages", "").
   ELSE
      RUN Insert_TTName IN h_prnvd ("QuanPages", STRING(mQuanPages)).

   IF FGetSetting("è´†‚ÑÆ™", "ÇÎ¢Æ§çÆ¨éØ•‡", "ç•‚") EQ "Ñ†" THEN
      RUN Insert_TTName IN h_prnvd ("OpOp", IF AVAILABLE op THEN STRING(op.op)
                                                            ELSE "").
/*
   IF mPageNumber - mPrevPageNum > 0 THEN DO:
      PUT UNFORMATTED "                                                                                  " STRING(mPageNumber - mPrevPageNum + 1,">9") SKIP.
      SEEK OUTPUT TO  mTotalPagesPos.
      PUT UNFORMATTED "Ç·•£Æ ·‚‡†≠®Ê:" + STRING(mPageNumber - mPrevPageNum + 1).
      SEEK OUTPUT TO END.   
   END.
   IF  vGrp NE vGroupNum THEN DO:  /* ≠• ØÆ·´•§≠ÔÔ ·‚‡†≠®Ê† */
      PAGE.
      mPageNumber = mPageNumber + 1.
      PUT UNFORMATTED SKIP(1).
   END.
*/
END PROCEDURE.

PROCEDURE PEntry.
   DEFINE INPUT PARAMETER iAmtRub   AS DECIMAL   NO-UNDO.
   DEFINE INPUT PARAMETER iAmtCur   AS DECIMAL   NO-UNDO.
   DEFINE INPUT PARAMETER iQty      AS DECIMAL   NO-UNDO.
   DEFINE INPUT PARAMETER iType     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iCurrency AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vRub AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCur AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vRub1 AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vAmtCur AS DECIMAL NO-UNDO.

   IF AVAIL op-entry THEN DO:
      {find-act.i &bact=acct  
                  &acct=mAcct
                  &curr=op-entry.currency
      }
   END.
   ELSE DO:
      {find-act.i &bact=acct  
                  &acct=mAcct
      }
   END.

   mAcctDetails = "".

   IF AVAILABLE acct THEN
   DO:
/*      mAcctDetails[1] = acct.Details.*/
      mAcctDetails[1] = GetTempXattrValueEx("acct",acct.acct + "," + acct.currency,"Details",gend-date,"").

      IF NOT {assigned acct.Details} THEN
      DO:
         IF acct.cust-cat EQ "û" THEN
            RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mAcctDetails[1]).
         ELSE DO:
            RUN GetCustName IN h_base (acct.cust-cat,
                                       acct.cust-id,
                                       ?,
                                       OUTPUT mAcctDetails[1],
                                       OUTPUT mAcctDetails[2],
                                       INPUT-OUTPUT mAcctDetails[3]).
            mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
         END.
      END.

/*      {wordwrap.i &s=mAcctDetails
                  &n=6
                  &l=32}*/
   END.

   IF TRUNC(iAmtRub, 0) EQ iAmtRub
   THEN vRub = STRING(STRING(iAmtRub * 100, "-zzzzzzzzzz999"), "x(12)=").
   ELSE vRub = STRING(STRING(iAmtRub * 100, "-zzzzzzzzz999"), "x(11)-x(2)").

   vRub1 = vRub.

   IF acct.currency NE "" THEN
   DO:
      IF acct.acct-cat NE "d" THEN
         vAmtCur = iAmtCur.
      ELSE
         vAmtCur = iQty.

      IF TRUNC(vAmtCur, 0) EQ vAmtCur
      THEN vCur = STRING(STRING(vAmtCur, "-zzzzzzz9"), "x(9)=").
      ELSE vCur = STRING(STRING(vAmtCur * 100, "-zzzzzzz999"), "x(9)-x(2)"). 
   END.
/*
   IF PageBreak(INPUT  7 + mNumLines) THEN DO:
      PUT UNFORMATTED "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP.
      RUN PrintSignatures.
      PUT UNFORMATTED "                                                                                  " STRING(mPageNumber - mPrevPageNum + 1,">9") SKIP.
      PAGE.
      mPageNumber = mPageNumber + 1.
      PUT UNFORMATTED SKIP(1).
      PUT UNFORMATTED "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" SKIP.  
   END.
*/

   IF mSimpleGroup THEN DO:
      IF iType = "DB" THEN DO:
         mCurAcctDB = (acct.currency <> "").
         IF mCurAcctDB THEN
            vRub = "".
      END.
      ELSE DO:
         IF acct.currency = "" THEN DO:
            IF NOT mCurAcctDB THEN
               vRub = "".
         END.
         ELSE DO:
            IF mCurAcctDB THEN
               vCur = "".
            ELSE
               vRub = "".
         END.
      END.
   END.
   IF acct.currency = "" THEN
      vCur = "".

   &IF DEFINED(amtstrval) NE 0 &THEN
   IF vCur NE "" THEN
   DO:
      vCur = vCur + "(" + GetISOCode(iCurrency) + ")".

      IF INDEX(vCur, "=") GT 1 THEN
         vCur = SUBSTRING(vCur, 2).
      ELSE
         vCur = SUBSTRING(vCur, 2).
   END.

   IF mSimpleGroup AND NOT mSuppressEquiv THEN
   DO:

      IF iType EQ "DB" AND vCur NE "" THEN
      DO:
            vRub = vRub1.
      END.
      ELSE IF iType EQ "CR" AND vCur NE "" THEN 
      DO:
            vRub = "".
      END.
      mSuppressEquiv = YES.
   END.

   IF     iType     EQ "CR"
      AND mDBCurr   NE ""
/*      AND mDBCurr   EQ iCurrency*/
      AND iCurrency NE "" THEN
      vRub = "".
   &ENDIF

   RUN Insert_TTName IN h_prnvd ("AcctDetails[" + iType + "]", mAcctDetails[1]).
   RUN Insert_TTName IN h_prnvd ("Acct[" + iType + "]", mAcct).
   RUN Insert_TTName IN h_prnvd ("Rub[" + iType + "]", vRub).
   RUN Insert_TTName IN h_prnvd ("Cur[" + iType + "]", vCur).

/*
   PUT UNFORMATTED mAcctDetails[1]  FORMAT "x(30)"
                   " ≥" mAcct FORMAT "x(20)" 
                    "≥" vRub  FORMAT "x(14)" 
                   "≥" vCur  FORMAT "x(16)" SKIP.
   DO mItem = 2 TO 6:

      IF LENGTH(TRIM(mAcctDetails[mItem])) GT 0 THEN
      DO:
         PUT UNFORMATTED mAcctDetails[mItem] FORMAT "x(30)"
                         " ≥                    ≥              ≥" SKIP.
      END.
   END.
*/

   &IF DEFINED(amtstrval) NE 0 &THEN
   IF iType EQ "DB" THEN
      mDBCurr = iCurrency.
   &ENDIF
   
END PROCEDURE.

PROCEDURE PrintSignatures.
/*

      PUT UNFORMATTED " èÆ§Ø®·®" .
      IF {assigned mSignatures} THEN DO:

         {signatur.i &submit-op-surrogate = YES} 
      END.
      ELSE PUT UNFORMATTED "" SKIP.
*/         
   DEFINE VARIABLE mSignaturIAttrCode   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mSignaturIAttrFormat AS CHARACTER NO-UNDO.

   DEFINE VARIABLE need-sign       AS CHARACTER  NO-UNDO INIT "".
   DEFINE VARIABLE vNumUserProcInt AS INT64    NO-UNDO .

   DEFINE VARIABLE vEntrySignaturI AS INT64    NO-UNDO.
   DEFINE VARIABLE vEntrySignaturJ AS INT64    NO-UNDO.
   DEFINE VARIABLE vSignaturValue  AS MEMPTR      NO-UNDO.

   DEFINE VARIABLE vRet            AS CHARACTER  NO-UNDO INIT "".
   DEFINE VARIABLE vRet1           AS CHARACTER  NO-UNDO INIT "".
   DEFINE VARIABLE vStrTMP         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vStrTMP1        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vStrTMP2        AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vSignatures     AS CHARACTER  NO-UNDO.

   IF NOT AVAIL(op) THEN RETURN.

   vSignatures = GetXAttrValueEx("op",
                                 STRING(op.op),
                                 "ÅéèÆ§Ø®·Ï",
                                 "").

   &GLOBAL-DEFINE DEF-SIGN "Ñ®‡,É´Å„Â,å•·‚Æè•Á†‚®,à·ØÆ´≠,Ñ†‚†"

   &SCOPED-DEFINE SignaturBranchId dept.branch

   IF vSignatures EQ "" THEN
   DO:

      /*** è•Á†‚Ï ØÆ§Ø®·•© ØÆ ß†™Æ≠Æ§†‚•´Ï·‚¢„ **************************************/

      /* ë¨Æ‚‡®¨ ≠†·‚‡Æ©™„ ØÆ§Ø®·•© ¢ Ø‡ÆÊ•§„‡• */
      vNumUserProcInt = INT64(GetSysConf("user-proc-id")).

      FIND FIRST user-proc WHERE RECID(user-proc) EQ vNumUserProcInt
         NO-LOCK NO-ERROR.

      IF AVAILABLE user-proc THEN
         need-sign = GetXAttrValue("user-proc",string(user-proc.public-number),"èÆ§Ø®·®").

      /* Ö·´® ·Ø®·™† ØÆ§Ø®·•© ≠•‚, ‚Æ „·‚†≠†¢´®¢†•¨ ·‚†≠§†‡‚≠Î© ·Ø®·Æ™ */
      IF need-sign EQ "" THEN
         need-sign = {&DEF-SIGN}.

      vRet  = "".
      vRet1 = "".

      IF AVAIL(op) THEN
         RUN SetSysConf IN h_base ({&OP-SURROGATE-SYSCONF}, STRING(op.op)).
     
      DO vEntrySignaturI = 1 TO NUM-ENTRIES (need-sign):
         /* îÆ‡¨®‡Æ¢†≠®• ØÆ§Ø®·® ¢ Æ°´†·‚Ï Ø†¨Ô‚® */
         RUN signatur_rz.p (       ENTRY(vEntrySignaturI,need-sign),
                                   {&SignaturBranchId},
                                   gend-date,
                            OUTPUT vSignaturValue).
         vStrTMP = TRIM(GET-STRING(vSignaturValue, 1)).
         DO vEntrySignaturJ = 1 TO NUM-ENTRIES(vStrTMP, "≥"):
            vStrTMP1 = TRIM(ENTRY(vEntrySignaturJ, vStrTMP, "≥")).

            IF vStrTMP1 NE "" THEN
            DO:

               {additem2.i vRet1 vStrTMP1 "|"}
               vStrTMP1 = vStrTMP1 + CHR(1) + "~\" + CHR(1) + "~\".
               vStrTMP1 = REPLACE(vStrTMP1, "~n", CHR(1) + "~\" + CHR(1) + "~\").
               vStrTMP1 = REPLACE(vStrTMP1, " ", CHR(1) + "~\quad ").
               {additem2.i vRet vStrTMP1 " "}
            END.
         END.
      END.

      RUN DeleteOldDataProtocol IN h_base ({&OP-SURROGATE-SYSCONF}).
   END.
   ELSE
   DO:

      DO vEntrySignaturI = 1 TO NUM-ENTRIES(vSignatures, ";"):

         vStrTMP = ENTRY(vEntrySignaturI, vSignatures, ";").

         vStrTMP1 = TRIM(ENTRY(1, vStrTMP, ",")).

         IF NUM-ENTRIES(vStrTMP, ",") GT 1 THEN
         DO:
            vStrTMP2 =  TRIM(ENTRY(2, vStrTMP, ",")).
         END.
         ELSE
            vStrTMP2 =  "".

         IF vStrTMP1 NE "" AND vStrTMP2 NE "" THEN
         DO:

            vStrTMP1 = vStrTMP1 + ": " + vStrTMP2.
            {additem2.i vRet1 vStrTMP1 "|"}
            vStrTMP1 = vStrTMP1 + CHR(1) + "~\" + CHR(1) + "~\".
            vStrTMP1 = REPLACE(vStrTMP1, "~n", CHR(1) + "~\" + CHR(1) + "~\").
            vStrTMP1 = REPLACE(vStrTMP1, " ", CHR(1) + "~\quad ").
            {additem2.i vRet vStrTMP1 " "}
         END.
      END.
   END.
   RUN Insert_TTName IN h_prnvd ("signtex", vRet).
   RUN Insert_TTName IN h_prnvd ("signtxt", vRet1).

END PROCEDURE. 


/* éØ‡•§•´Ô•‚, ·™Æ´Ï™Æ ·‚‡Æ™ ß†≠®¨†Ó‚ ØÆ§Ø®·® ®ß signatur.i */
PROCEDURE CalcSignLines:
   DEFINE INPUT PARAMETER  iSignsList AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oLines     AS INT64   NO-UNDO.

   DEF VAR vSigns       AS CHAR    NO-UNDO.
   DEF VAR vCnt         AS INT64    NO-UNDO.

   IF {assigned iSignsList} THEN DO:
      oLines  = 2.
      DO vCnt = 1 TO NUM-ENTRIES(iSignsList):
             
         vSigns = getTCodeFld ("Val","èÆ§Ø®·®",ENTRY(vCnt, iSignsList),TODAY).
         oLines = oLines + NUM-ENTRIES(vSigns,"~~") + 1.
    
      END.
   END.

END PROCEDURE.

PROCEDURE GetDetailManual:  
   &IF DEFINED(printErr) <> 0 &THEN
      Detail = "".
      FOR EACH PackObject WHERE (PackObject.file-name EQ     "op-entry" AND 
                                 PackObject.Surrogate BEGINS STRING(op.op) + ",")
                             OR (PackObject.file-name EQ     "op" AND
                                 PackObject.Surrogate EQ     STRING(op.op))
      NO-LOCK:
         FIND FIRST Packet OF PackObject NO-LOCK.
         RUN FillErrorTable IN h_exch (Packet.ClassError,
                                       Packet.PackError,
                                       OUTPUT TABLE ttError).
         FOR EACH ttError WHERE ttError.type EQ "éË®°™†" NO-LOCK:
            Detail[1] = Detail[1] + "; " + ttError.name.
         END.
         {empty ttError}
      END.
      Detail[1] = TRIM(Detail[1],";").      
   &ELSE

      &IF DEFINED(CONSOLIDATED) = 0 &THEN

      DEFINE VARIABLE vStrTMP AS CHARACTER NO-UNDO.
 
      Detail = "".
 
      vStrTMP = GetXAttrValueEx("op",
                                STRING(op.op),
                                "alt-details",
                                "").
      IF vStrTMP NE "" THEN
      DO:
         Detail[1] = vStrTMP.
      END.
      ELSE
      DO:
         RUN DefDetail.
      END.

      &ELSE

      Detail[1] = GetSysConf("cons-ord-override-details").

      &ENDIF
   &ENDIF
/*
   {wordwrap.i &s=Detail &n=5 &l=80}
*/
END PROCEDURE.

PROCEDURE RotateArrayDown3.
    DEFINE INPUT-OUTPUT PARAMETER ioArray AS CHARACTER EXTENT 3.

    DEFINE VARIABLE i AS INT64   NO-UNDO.
    DEFINE VARIABLE s AS CHARACTER NO-UNDO.

    s = ioArray[EXTENT(ioArray)].
    DO i = EXTENT(ioArray) TO 2 BY -1:
        ioArray[i] = ioArray[i - 1].
    END.
    ioArray[1] = s.
END PROCEDURE.

PROCEDURE ShiftArrayDown3.
    DEFINE INPUT-OUTPUT PARAMETER ioArray AS CHARACTER EXTENT 3.

    DEFINE VARIABLE i AS INT64 NO-UNDO.

    DO i = EXTENT(ioArray) TO 1 BY -1:
        IF LENGTH(ioArray[EXTENT(ioArray)]) = 0 THEN
            RUN RotateArrayDown3(INPUT-OUTPUT ioArray).
        ELSE
            LEAVE.
    END.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/10/2014 15:19:02.787+04:00' */
/* $LINTFILE='memorn_ops.p' */
/*prosignZWJfrZEjNOZbog6EUkh+DA*/