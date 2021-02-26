/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: hdbrw.P
      Comment: Хозяйственные договоры - браузер
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
&GLOBAL-DEFINE Type      name
&GLOBAL-DEFINE DocType   val
&GLOBAL-DEFINE AcctCr    misc[1]
&GLOBAL-DEFINE AcctDb    misc[2]
&GLOBAL-DEFINE Details   misc[3]
&GLOBAL-DEFINE GrpCr     misc[4]
&GLOBAL-DEFINE GrpDb     misc[5]
&GLOBAL-DEFINE StartDate misc[6]
&GLOBAL-DEFINE EndDate   misc[7]

DEF VAR mCount     AS INT64  NO-UNDO.

DEF VAR mStartDate AS DATE   NO-UNDO.
DEF VAR mEndDate   AS DATE   NO-UNDO.

DEF VAR mFltAcctCr     AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltAcctDb     AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltGrpCr      AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltGrpDb      AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltDocType    AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltDetails    AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltType       AS CHAR INIT '*'  NO-UNDO.
DEF VAR mFltActual     AS CHAR INIT 'Да' NO-UNDO.
DEF VAR mFltStartDate1 AS DATE NO-UNDO.
DEF VAR mFltStartDate2 AS DATE NO-UNDO.
DEF VAR mFltEndDate1   AS DATE NO-UNDO.
DEF VAR mFltEndDate2   AS DATE NO-UNDO.

DEF VAR ch             AS CHAR   FORMAT "x(1)" NO-UNDO.

DEFINE BUFFER bufcode FOR code.
DEFINE BUFFER bCode   FOR code.


&GLOBAL-DEFINE fieldcode code.code
&GLOBAL-DEFINE tmprecid  YES

{hdbrw.frm}
{hdbrw.qry}


{navigate.cqr 
   &file          = code 
   &avfile        = code 

   &access-class  = "'code'"
   &access-surr   = "code.class + ',' + code.code"
   &access-read   = r
 
   &bf1           = "str-recid code.{&AcctDb} code.{&GrpDb} code.{&AcctCr} code.{&GrpCr} code.{&DocType} code.{&Details} code.{&Type} mStartDate mEndDate" 
   &ef            = "hdbrw.uf " 

   &edit          = "edit-ef.cqr "	
   &create        = "hdbrw.cr "	
   &postfind      = "hdbrw.fnd "
   &update        = "hdbrw.upd "   
   
   &befupd        = "hdbrw.bup "   
   &delete        = "pclass.del "	
   
   &look          = "hdbrw.nav " 

   &return        = "return.cqr &rfld = code " 
                       
   &oh6           = """ + (IF n-frm EQ 1 THEN ""│F6 фильтр|Ctrl+F6 убрать фильтр"" ELSE """") + """
   &oth6          = "hdbrw.f6 "

   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
}

{intrface.del}          /* Выгрузка инструментария.  */ 