/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: cfobrw.P
      Comment: ЦФО - браузер
   Parameters:
         Uses:
      Used by:
      Created: ayv 
     Modified:    
*/

{globals.i}             /* Глобальные переменные сессии. */
{flt-file.i NEW}  
{brow-cod.i}            /* параметры процедуры просмотра + pp-rights */

{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{navigate.def}          /* Переменные для navigate.cqr. */

{intrface.get cust}
{intrface.get count}
{intrface.get tmess}
{intrface.get count}

&GLOBAL-DEFINE Codes      code
&GLOBAL-DEFINE ShortName  name
&GLOBAL-DEFINE Status     val
&GLOBAL-DEFINE ProdType	misc[1]
&GLOBAL-DEFINE StartDate	misc[2]
&GLOBAL-DEFINE EndDate	misc[3]

DEF VAR mBranchID  AS CHAR   NO-UNDO.
DEF VAR mCFO  	   AS CHAR   NO-UNDO.
DEF VAR mStartDate AS DATE   NO-UNDO.
DEF VAR mEndDate   AS DATE   NO-UNDO.

DEF VAR mDummy	  AS CHAR   NO-UNDO.

DEF VAR mFltBranchID   AS CHAR INIT "*" NO-UNDO.
DEF VAR mFltShortName  AS CHAR INIT "*" NO-UNDO.
DEF VAR mFltCFO		   AS CHAR INIT "*" NO-UNDO.
DEF VAR mFltStatus	   AS CHAR INIT "*" NO-UNDO.
DEF VAR mFltProdType   AS CHAR INIT "*" NO-UNDO.
DEF VAR mFltStartDate1 AS DATE NO-UNDO.
DEF VAR mFltStartDate2 AS DATE NO-UNDO.
DEF VAR mFltEndDate1   AS DATE NO-UNDO.
DEF VAR mFltEndDate2   AS DATE NO-UNDO.

DEF VAR ch             AS CHAR   FORMAT "x(1)" NO-UNDO.

DEFINE BUFFER bufcode FOR CODE.

&GLOBAL-DEFINE eh        cfobrw.eh~032
&GLOBAL-DEFINE fieldcode code.code
&GLOBAL-DEFINE tmprecid  YES

{cfobrw.frm}
{cfobrw.qry}


{navigate.cqr 
   &file          = code 
   &avfile        = code 

   &access-class  = "'code'"
   &access-surr   = "code.class + ',' + code.code"
   &access-read   = r

   &maxfrm        = "2" 
   &first-frm     = "1" 
   &bf1           = "str-recid mBranchID code.{&ShortName} mCFO code.{&Status} mStartDate mEndDate code.{&ProdType}" 
   &bf2           = "code.code code.name code.val "
   &ef            = "cfobrw.uf " 

   &edit          = "edit-ef.cqr "	
   &create        = "cfobrw.cr "	
   &postfind      = "cfobrw.fnd "	
   &update        = "cfobrw.upd "	
   
   &befupd        = "cfobrw.bup "   
   &delete        = "pclass.del "	
   
   &look          = "cfobrw.nav " 

   &return        = "return.cqr &rfld = code " 

   &oh3           = "│F3 форма"   
   &oth3          = "frames.cqr " 
   &user-frames_cqr = "RUN RELQ. n-str = 1."
                       
   &oh6           = """ + (IF n-frm EQ 1 THEN ""│F6 фильтр|Ctrl+F6 убрать фильтр"" ELSE """") + """
   &oth6          = "cfobrw.f6 "

   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
}

{intrface.del}          /* Выгрузка инструментария.  */ 