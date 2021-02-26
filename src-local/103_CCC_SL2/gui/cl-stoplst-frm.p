/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: cl-stoplst-frm.p
      Comment: Форма редактирования
   Parameters: Нет
         Uses:
      Used by:
      Created: 29.08.2013 
     Modified: 29.08.2013 
*/

{globals.i}             
{formpar.i}             
{intrface.get tmess}    
{intrface.get xclass}   
{flt-file.i}
{intrface.get count}
{intrface.get cust}
{wordwrap.def}
{stoplist.fun }
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

DEF VAR mParentValue AS CHAR  NO-UNDO.
DEF VAR mFIOName     AS CHAR  NO-UNDO.
DEF VAR mNote        AS CHAR  NO-UNDO.
DEF VAR mDate        AS DATE  NO-UNDO.
DEF VAR mDataRez     AS DATE  NO-UNDO.
DEF VAR mDataAdd     AS DATE  NO-UNDO.
DEF VAR mDataCls     AS DATE  NO-UNDO.
DEF VAR mBirthDay    AS DATE  NO-UNDO.
DEF VAR mBirthPlace  AS CHAR  NO-UNDO.
DEF VAR in-class     AS CHAR  NO-UNDO.
DEF VAR in-parent    AS CHAR  NO-UNDO.
DEF VAR vclass       AS CHAR  NO-UNDO.
DEF VAR mDataID      AS INT64 NO-UNDO.
DEF VAR mCnt         AS INT64 NO-UNDO.
DEF VAR str-recid    AS CHAR  NO-UNDO.

DEFINE VARIABLE mOk  AS LOGICAL NO-UNDO.

DEF BUFFER bcode FOR CODE. 

{stoplist.pro &DATA-LINE = DataLine}
ASSIGN mDataID = INT64(GetCodeMisc("","StopList",8)) NO-ERROR.

{cl-stoplst.frm}
   
MAIN:
DO:           
   IF iSurrogate >  "" THEN DO:
      FIND FIRST code WHERE
                 code.class =  ENTRY(1,iSurrogate)
             AND code.code  =  ENTRY(2,iSurrogate)
      NO-LOCK NO-ERROR.
      IF NOT AVAIL code THEN
         LEAVE MAIN.
      ASSIGN 
         in-rec-id = RECID(code)
         in-class  = code.class
         in-parent = code.PARENT
      .
      RELEASE code.
      IF NOT tst-rght-CODE(in-class, 'w') THEN 
         RETURN.
   END.
   ELSE
   DO:
         ASSIGN
         in-class  = ENTRY(2,iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  1
         in-parent = ENTRY(3,iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  2
      .
      IF NOT tst-rght-CODE(in-class, 'c') THEN 
         RETURN.

   END.
   FRAME edit:TITLE = "[ " +  (IF iMode = "F1"
                              THEN "ПРОСМОТР"
                              ELSE IF     iMode = "F9"
                                      AND in-rec-id <> 0
                              THEN "РЕДАКТИРОВАНИЕ"
                              ELSE "СОЗДАНИЕ") + " ]".
   ASSIGN
      vclass   =  iClass
      Level    =  Level - 1.

{rec-ed.i
      &file       = code
      &ef         = "cl-stoplst-frm.ef " 
      &create     = "cl-stoplst-frm.cr "
      &update     = "cl-stoplst-frm.upd " 
      &delete     = "pclass.del "
      &eh         = "cl-stoplst-frm.eh "     
      &postfind   = "cl-stoplst-brw.fnd "   
      &lookup     = "cl-stoplst-frm.nau " 
      &NOier      = "/*"
      &NOparent   = "/*"
      &col        = 0
      &ModView    = YES
   }
END.

{intrface.del}          /* Выгрузка инструментария. */ 
/* $LINTFILE='cl-stoplst-frm.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.373+03:00' */
/*prosignFyqv95hEoAK+3UAYwigmYg*/