assign
  flag-ZO = no
  access  = "".
if ts.dwidth then do:
  {dwidth.i}
end.

IF GetSysConf("AUTOTEST:autotest") EQ "YES" THEN DO:
   IF GetSysConf("AUTOTEST:КонечныйФайл") GE "" THEN DO:
      OS-DELETE VALUE(GetSysConf("AUTOTEST:КонечныйФайл")).
      OS-RENAME VALUE("_spool.tmp") VALUE(GetSysConf("AUTOTEST:КонечныйФайл")).
   END.
END.
ELSE DO:
   {preview.i}
END.

{parsin.def}

DEFINE VARIABLE mUserProcParam AS CHARACTER INITIAL ""  NO-UNDO.
DEFINE VARIABLE mDocTypes      AS CHARACTER INITIAL "*" NO-UNDO.
DEFINE VARIABLE mDbCr          AS CHARACTER INITIAL "*" NO-UNDO.
DEFINE VARIABLE mIsNoDocs      AS LOGICAL               NO-UNDO.

IF ts.pn > 0 THEN 
   FIND FIRST user-proc WHERE user-proc.public-number EQ ts.pn
      NO-LOCK NO-ERROR.
ELSE
   FIND FIRST user-proc WHERE user-proc.PROCEDURE EQ ts.vmode-proc
                          AND user-proc.partition EQ "Выписки"
                          AND user-proc.module    EQ work-module
     NO-LOCK NO-ERROR.

IF AVAILABLE user-proc THEN
   mUserProcParam =  GetParamByNameAsChar(user-proc.params, "PrintDoc", "NO").
ELSE
   mUserProcParam = "".

IF CAN-DO("YES,Да", mUserProcParam) THEN DO:
   IF AVAILABLE user-proc THEN
       ASSIGN
           mDocTypes = GetXAttrValueEx("user-proc",
                                       STRING(user-proc.public-number),
                                       "КодДок",
                                       "*")
           mDbCr     = GetXAttrValueEx("user-proc",
                                       STRING(user-proc.public-number),
                                       "ПризнакДбКр",
                                       "*")
       .
END.
ELSE
   mUserProcParam = "".

IF mUserProcParam NE "" THEN
DO:
   {strtout3.i 
   &norepeat = YES}
   PackagePrint = YES.
   mIsNoDocs = YES.

   FOR EACH opStmt WHERE
           CAN-DO(mDbCr, "Дб") AND opStmt.fDb
           OR
           CAN-DO(mDbCr, "Кр") AND NOT opStmt.fDb
       NO-LOCK,
       EACH op WHERE
           op.op = opStmt.fOp
       NO-LOCK,
       EACH doc-type OF op WHERE
           CAN-DO(mDocTypes, doc-type.doc-type)
       NO-LOCK:
      IF doc-type.printout NE ""
         AND (    SEARCH(doc-type.proc-print[8] + ".p") NE ?
              OR  SEARCH(doc-type.proc-print[8] + ".r") NE ?)
      THEN
      DO:
         RUN CleanTT IN h_print(?).
         OUTPUT TO _txt__spool.tmp PAGED PAGE-SIZE 10000000.
         mIsNoDocs = NO.

         RUN VALUE(doc-type.proc-print[8] + ".p") (RECID(op)).
         OUTPUT CLOSE.
         RUN op_print_print.

      END.
   END.
   OS-DELETE VALUE("_macro.tmp").
   PackagePrint = NO.
   IF mIsNoDocs THEN
   DO:
      PUT UNFORMATTED "В выборку не попало ни одного документа." SKIP.
   END.
   {endout3.i
   &norepeat = YES}
END.
