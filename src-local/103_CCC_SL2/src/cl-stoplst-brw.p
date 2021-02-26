/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: cl-stoplst-brw.p
      Comment: Браузер Стоп-Листов
   Parameters: Нет
         Uses:
      Used by:
      Created: 29.08.2013 
     Modified: 29.08.2013 
*/

{globals.i}
{form.def}
{intrface.get xclass}
{navigate.def}
{flt-file.i}
{intrface.get cust}
{intrface.get count}
{intrface.get tmess}
{wordwrap.def}
{stoplist.fun }
{navigate.def}

DEF VAR in-class     AS CHAR  NO-UNDO. 
DEF VAR in-parent    AS CHAR  NO-UNDO. 
DEF VAR in-title     AS CHAR  NO-UNDO. 
DEF VAR ch           AS CHAR  NO-UNDO.
DEF VAR mDataRez     AS DATE  NO-UNDO.
DEF VAR mDataAdd     AS DATE  NO-UNDO.
DEF VAR mDataCls     AS DATE  NO-UNDO.
DEF VAR mBirthDay    AS DATE  NO-UNDO.
DEF VAR mBirthPlace  AS CHAR  NO-UNDO.
DEF VAR sv-DataRez   AS DATE  NO-UNDO.
DEF VAR sv-DataAdd   AS DATE  NO-UNDO.
DEF VAR sv-DataCls   AS DATE  NO-UNDO.
DEF VAR mDataID      AS INT64 NO-UNDO.
DEF VAR mCnt         AS INT64 NO-UNDO.

DEFINE BUFFER bufcode FOR code.
&GLOBAL-DEFINE tmprecid  YES

{cl-stoplst.frm }    
{qrdef.i
   &buff-list        = "code"
   &need-buff-list   = "code"
   &Fields-Mandatory = "class"
   &Join-list        = "each"
   &SortBy = "' BY CODE.NAME BY int64(CODE.DESCRIPTION[1])  '"
}

ASSIGN
   in-class    =  GetFltVal ("class")
   in-parent   =  GetFltVal ("parent")
. 
{setparam.i in-class in-parent in-title}
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

IF NOT tst-rght-CODE(in-class, 'r') THEN 
   RETURN.


{stoplist.pro &DATA-LINE = DataLine}
ASSIGN mDataID = INT64(GetCodeMisc("","StopList",8)) NO-ERROR.

/*MESSAGE mDataID   */
/*VIEW-AS ALERT-BOX.*/

{navigate.cqr
   &file      = code
   &files     = "code"

   &filt      = yes
   &bf1       = "str-recid code.description[1] code.misc[1] code.misc[2] code.misc[3] code.misc[4] code.misc[5] CODE.misc[6] mBirthDay mBirthPlace CODE.misc[7] code.name code.val mDataRez mDataAdd mDataCls code.misc[8] "
   &CalcFld   = "sv-DataRez sv-DataAdd sv-DataCls "
   &CalcVar   = "cl-stoplst-brw.cv "
             
   &edit      = "bis-tty.ef "
      &before-run-method = "brwinfdog.bfe " 
   &postfind  = "cl-stoplst-brw.fnd "   
   &look      = "cl-stoplst-brw.nav " 
      &class_avail = "'stoplist'"
      &SpecClass   = "'stoplist'"
   &oh6       = "│F6 фильтр"
   &oth6      = "flt-file.f6 "   
   &delete    = "cl-stoplst-frm.upd "  
  
   &print     = "stoplist.prt "
   &return    = "return.cqr "  
      &rfld   = "code"
 }
{intrface.del}
RETURN "".
/* $LINTFILE='cl-stoplst-brw.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.365+03:00' */
/*prosignc7W6+1oSpIm6q7OWXk6uDg*/