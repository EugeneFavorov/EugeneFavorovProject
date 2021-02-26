/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ehbrw.P
      Comment: Документы для электронного хранилища - браузер
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

&GLOBAL-DEFINE Num       code
&GLOBAL-DEFINE AcctCr    misc[1]
&GLOBAL-DEFINE AcctDb    misc[2]
&GLOBAL-DEFINE DocType   misc[3]
&GLOBAL-DEFINE Currency  misc[4]
&GLOBAL-DEFINE Param     misc[5]
&GLOBAL-DEFINE StartDate misc[6]
&GLOBAL-DEFINE EndDate   misc[7]
&GLOBAL-DEFINE OpKind    misc[8]
&GLOBAL-DEFINE BranchDb  description[1]
&GLOBAL-DEFINE BranchCr  description[2]
&GLOBAL-DEFINE Details   description[3]


DEF VAR mCount     AS INT64  NO-UNDO.

DEF VAR mStartDate AS DATE   NO-UNDO.
DEF VAR mEndDate   AS DATE   NO-UNDO.

DEF VAR mFltBranchDb   AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltBranchCr   AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltAcctCr     AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltAcctDb     AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltDocType    AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltCurrency   AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltOpKind     AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltDetails    AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltParam      AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltActual     AS CHAR INIT 'Да' NO-UNDO.
DEF VAR mFltStartDate1 AS DATE NO-UNDO.
DEF VAR mFltStartDate2 AS DATE NO-UNDO.
DEF VAR mFltEndDate1   AS DATE NO-UNDO.
DEF VAR mFltEndDate2   AS DATE NO-UNDO.

DEF VAR ch             AS CHAR   FORMAT "x(1)" NO-UNDO.

DEFINE BUFFER bufcode FOR code.
DEFINE BUFFER bCode   FOR code.

&GLOBAL-DEFINE eh        ehbrw.eh~032
&GLOBAL-DEFINE fieldcode code.code
&GLOBAL-DEFINE tmprecid  YES

{ehbrw.frm}
{ehbrw.qry}


{navigate.cqr 
   &file          = code 
   &avfile        = code 

   &access-class  = "'code'"
   &access-surr   = "code.class + ',' + code.code"
   &access-read   = r

   &maxfrm        = "2" 
   &first-frm     = "1" 
   &bf1           = "str-recid code.{&BranchDb} code.{&AcctDb} code.{&BranchCr} code.{&AcctCr} code.{&DocType} code.{&Currency} code.{&OpKind} code.{&Details} code.{&Param} mStartDate mEndDate" 
   &bf2           = "code.code code.name code.val "
   &ef            = "ehbrw.uf " 

   &edit          = "edit-ef.cqr "	
   &create        = "ehbrw.cr "	
   &postfind      = "ehbrw.fnd "
   &update        = "ehbrw.upd "   
   
   &befupd        = "ehbrw.bup "   
   &delete        = "pclass.del "	
   
   &look          = "ehbrw.nav " 

   &return        = "return.cqr &rfld = code " 

   &oh3           = "│F3 форма"   
   &oth3          = "frames.cqr " 
   &user-frames_cqr = "RUN RELQ. n-str = 1."
                       
   &oh6           = """ + (IF n-frm EQ 1 THEN ""│F6 фильтр|Ctrl+F6 убрать фильтр"" ELSE """") + """
   &oth6          = "ehbrw.f6 "

   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
}

{intrface.del}          /* Выгрузка инструментария.  */ 