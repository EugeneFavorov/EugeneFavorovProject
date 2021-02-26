/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: loan-pn-par.p
      Comment: Параметры договоров с получателями платежей
   Parameters:
         Uses:
      Used by:
      Created: 18.08.2009 ushd 102466
     Modified: 
*/
{globals.i}
{flt-file.i}
{xsignrat.def}
{intrface.get rights}

DEFINE VAR mPNTypePay        AS CHAR    NO-UNDO.
DEFINE VAR mPNTypeCode       AS CHAR    NO-UNDO.
DEFINE VAR mReceiver         AS CHAR    NO-UNDO.
DEFINE VAR mUseVedKvit       AS LOGICAL NO-UNDO.
DEFINE VAR mUseVedKvitFormat AS CHAR    NO-UNDO.
DEFINE VAR mCustName         AS CHAR    NO-UNDO.
DEFINE VAR mCustINN          AS CHAR    NO-UNDO.
DEFINE VAR mCustKPP          AS CHAR    NO-UNDO.
DEFINE VAR mSymbol           AS CHAR    NO-UNDO.
DEFINE VAR mAcct             AS CHAR    NO-UNDO.
DEFINE VAR mLAIdent          AS INT64 NO-UNDO.
DEFINE VAR mLAOrder          AS INT64 NO-UNDO.
DEFINE VAR mLASurr           AS CHAR    NO-UNDO.
DEFINE VAR mLARwd            AS ROWID   NO-UNDO.
DEFINE VAR mReceiverNoUse    AS LOGICAL NO-UNDO.

DEFINE BUFFER xloan      FOR loan.
DEFINE BUFFER xcust-corp FOR cust-corp.
DEFINE BUFFER xloan-acct FOR loan-acct.

mUseVedKvitFormat = GetXAttrEx("loan-pn-par","ВедКвитанция","Data-Format").

{loan-pn-par.frm}
{loan-pn-par.qry}

{navigate.cqr
   &file       = "loan"
   &files      = "loan"
   &avfile     = "loan"

   &tmprecid   = YES
   &filt       = YES
   &autosigns  = YES

   &startup    = "xsignrat.st "
      &runt-set-filtr = YES 

   &CalcFld    = "mPNTypePay mReceiver mUseVedKvit mAcct mCustINN mReceiverNoUse mSymbol mPNTypeCode"
   &CalcVar    = "loan-pn-par.cv "

   &maxfrm     = 2
   &first-frm  = 1
   
   &startup    = "xsignrat.st "

   &oth2       = "xsignrat.f2 "
   &oth3       = "frames.cqr "
   &oth6       = "flt-file.f6 "
   &look       = "loan-pn-par.nav "
    
   &oth5       = "xsigns.rat "
      &rat_file    = loan
      &rat_class   = "loan.class-code"
      &rat_upclass = "'loan-pn-par'"
      &rat_surr    = "loan.contract + ',' + loan.cont-code "
      &rat_num     = 3
      &rat_key     = F2

   &edit        = "bis-tty.ef "
      &BEFORE-RUN-METHOD = "loan-pn-par.bfe "
      &class_avail       = "GetXclassAllChildsEx('loan-pn-par')"

   &bf1        = "mCustINN mAcct mReceiver mSymbol mPNTypeCode"
   &bf2        = "loan.cont-cli mReceiver mAcct mUseVedKvit mPNTypePay"
   &bf3        = "loan.cont-cli mReceiver signs_value "
   
   &postfind   = "xsignrat.fnd "
   &delete     = "loan-pn-par.del "

   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES

   &return        = "ret-surr.cqr "
      &ProgressFile = "'loan'"
}

{intrface.del}          /* Выгрузка инструментария. */ 
