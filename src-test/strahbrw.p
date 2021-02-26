/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: strahbrw.P
      Comment: Получатели страховых премий - браузер
   Parameters:
         Uses:
      Used by:
      Created: kam 
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

&GLOBAL-DEFINE CustCodes   code
&GLOBAL-DEFINE BankName    misc[1]
&GLOBAL-DEFINE WhiteINN    name
&GLOBAL-DEFINE WhiteName   description[1]
&GLOBAL-DEFINE RaschAcct   description[2]
&GLOBAL-DEFINE CorrAcct    misc[2]
&GLOBAL-DEFINE BankBIC     misc[3]
&GLOBAL-DEFINE Telefax     misc[4]
&GLOBAL-DEFINE ShortName   misc[6]
&GLOBAL-DEFINE DocType     misc[5]
&GLOBAL-DEFINE DocCustCode description[3]
&GLOBAL-DEFINE DocOpenDate misc[7]
&GLOBAL-DEFINE DocIssue    misc[8]
&GLOBAL-DEFINE Address     val
&GLOBAL-DEFINE BirthDay    kind

DEFINE VARIABLE mCustCat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCustID  AS INT64     NO-UNDO.

DEFINE VARIABLE mShortName AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mName1     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mName2     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mInn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mRaschAcct AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mTelefax       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mWName   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE ch         AS CHARACTER FORMAT "x(1)"                    NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE work-parent AS INT64   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE list-rec    AS CHARACTER NO-UNDO. /* Список запрещ. подпараметров */

DEFINE BUFFER bufcode FOR CODE.

DEFINE VARIABLE mSubjCls  AS CHARACTER                                   NO-UNDO.
DEFINE VARIABLE mSubjName AS CHARACTER INIT "cust-corp,person"     NO-UNDO.
DEFINE VARIABLE mSubjSurr AS CHARACTER INIT "cust-id,person-id"  NO-UNDO.
DEFINE VARIABLE mSubjCustCat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mSubjCustID  AS INT64     NO-UNDO.

DEFINE VARIABLE mFltCustCat  AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mFltCustID   AS INT64   INIT ?  NO-UNDO.
DEFINE VARIABLE mFltInn      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mFltName     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mFltRaschAcct  AS DECIMAL    INIT ?     NO-UNDO.
DEFINE VARIABLE mFltShortName     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mFltBankName     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mFltCorrAcct     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mFltBankBIC     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mFltTelefax     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE mResult AS LOGICAL NO-UNDO.
DEFINE VARIABLE mTmpDate AS DATE NO-UNDO.
DEFINE VARIABLE mTmpStr AS CHAR NO-UNDO.
DEFINE VARIABLE mTmpDay AS INT NO-UNDO.
DEFINE VARIABLE mTmpMonth AS INT NO-UNDO.
DEFINE VARIABLE mTmpYear AS INT NO-UNDO.
 
DEFINE TEMP-TABLE ttSubj NO-UNDO
   FIELD FileRowid AS ROWID
   FIELD PickValue AS CHARACTER.

DEFINE BUFFER bCode FOR code.

&GLOBAL-DEFINE eh        strahbrw.eh~032
&GLOBAL-DEFINE fieldcode code.code
&GLOBAL-DEFINE tmprecid  YES


{strahbrw.frm}
{strahbrw.qry}

{navigate.cqr
   &file          = code
   &files         = code
   &avfile        = code

   &access-class  = "'code'"
   &access-surr   = "code.class + ',' + code.code"
   &access-read   = r

   &maxfrm        = "2"
   &first-frm     = "1"
   &bf1           = "mCustCat mCustID code.{&WhiteINN} code.{&ShortName} code.{&WhiteName} mRaschAcct code.{&BankBIC} code.{&BankName} code.{&CorrAcct} code.{&Telefax}"
   &bf2           = "code.code code.name code.val "
   &ef            = "strahbrw.uf "

   &edit          = "edit-ef.cqr "
   &create        = "strahbrw.cr "
   &postfind      = "strahbrw.fnd "
   &update        = "strahbrw.upd "
   
   &befupd        = "strahbrw.bup "
   &delete        = "pclass.del "
   
   &look          = "strahbrw.nav "
   &lookup        = "strahbrw.nau " 
                       
   &return        = "return.cqr &rfld = code "

   &oh3           = "│F3 форма"
   &oth3          = "frames.cqr "
                       &user-frames_cqr = "RUN RELQ. n-str = 1."
                       
   &oh6           = """ + (IF n-frm EQ 1 THEN ""│F6 фильтр"" ELSE """") + """
   &oth6          = "strahbrw.f6 "

   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
}

{intrface.del}          /* Выгрузка инструментария.  */ 

{strahbrw.fun}

IF CONNECTED("bank") THEN 
    DISCONNECT bank.
IF CONNECTED("bismfr") THEN
    DISCONNECT bismfr.


PROCEDURE PostSelectQuery:
   h-frm[n-frm]:TITLE =  "[ " + REPLACE (CAPS(in-title), CHR(2), ",") + " ]".
END PROCEDURE.





