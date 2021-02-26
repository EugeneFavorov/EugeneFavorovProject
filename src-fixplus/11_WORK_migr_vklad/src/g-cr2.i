/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: G-CR2.I
      Comment:
   Parameters: use-unk - если определена, то при выборе клиента используется УНК
         Uses:
      Used by:
      Created: ...
     Modified: 03.06.2002 14:11 SEMA     по заявке 0007837 создание клиента вынесено в внутреннюю процедуру - не
                                         компилировался по размеру action segment
     Modified: 03.06.2002 17:22 SEMA     по заявке 0007837 исправление ошибки передачи параметров
     Modified: 06.08.2002 12:49 SEMA     по заявке 0009237 изменение кассового символа (tt6996) вынесено в файл g-frame.i
     Modified: 20.08.2002 12:56 GORM     по заявке 0007203 при отказе от открытия вклада что бы не выдавалось сообщений об ошибке   
     Modified: 20.08.2002 13:08 GORM     
     Modified: 11.12.2003 15:01 alvel    
     Modified: 27.01.2004 17:17 SAP      
     Modified: 04.02.2004 21:32 SAP      
     Modified: 10.03.2004 12:58 SAP      
     Modified: 12.04.2004 14:38 SAP      
     Modified: 27.04.2004 11:13 SAP      
     Modified: 12.08.2005 15:22 SAP      
     Modified: 15.09.2005 12:10 SAP      
     Modified: 03.08.2007 18:25 fEAk     <comment>
*/

&Scop BYrole yes .
&Scop Dotacct  yes.
&Scop DoOp yes .

DEF INPUT PARAM in-op-date LIKE op.op-date.
DEF INPUT PARAM oprid      AS   RECID.
DEF VAR cur-n LIKE currency.currency NO-UNDO.

DEF VAR beg-templ   AS CHAR INIT '0'.
DEF VAR num-cr-acct AS INT64 NO-UNDO.
DEF VAR return-find-str AS CHAR NO-UNDO.
DEF VAR lst-tmpl-op AS CHAR NO-UNDO.

DEFINE var cr_loanh as handle no-undo.
define new global shared stream err .
DEF BUFFER xloan FOR loan.
def var int-delay as char format 'x(40)' no-undo .
def var fl as INT64 .
def var summa like op-entry.amt-rub no-undo .
DEF VAR c-date AS DATE NO-UNDO.
def var cred as date no-undo.
DEF VAR h_dpsb2p AS HANDLE NO-UNDO.                              /* <- mitr tt 6733 */
DEFINE NEW SHARED BUFFER b-loan FOR loan.
DEF VAR dep_period AS CHAR NO-UNDO.
DEF VAR main-commission LIKE commission.commission INITIAL ? NO-UNDO .

DEFINE VARIABLE vmfo         LIKE op-bank.bank-code NO-UNDO.
DEFINE VARIABLE vcorr-acct   LIKE op-bank.corr-acct NO-UNDO.
DEFINE VARIABLE temp-acct    AS CHAR    NO-UNDO.
DEFINE VARIABLE msg          AS CHAR    FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE acctkey      AS INT64 NO-UNDO.
DEFINE VARIABLE mforeq       AS LOGICAL NO-UNDO.
DEFINE VARIABLE result       AS INT64 NO-UNDO.
DEFINE VARIABLE fl-err       AS INT64 NO-UNDO INIT -1.
DEFINE VARIABLE vCode        AS CHAR    NO-UNDO.
DEFINE VARIABLE vLimitProl   AS CHAR    NO-UNDO.
DEFINE VARIABLE mStatOp      AS CHAR    NO-UNDO.

DEFINE VARIABLE mDBO         AS LOGICAL NO-UNDO. /* открытие вклада через ДБО */
DEFINE VARIABLE mClass-Code  AS CHAR    NO-UNDO.
DEFINE VARIABLE mOpKind-open AS CHAR    NO-UNDO.
DEFINE VARIABLE mOpDate      AS DATE    NO-UNDO. 
DEFINE VARIABLE mCurr        AS CHAR    NO-UNDO.
DEFINE VARIABLE mdep_period  AS CHAR    NO-UNDO.
DEFINE VARIABLE mamt-rub     AS DEC     NO-UNDO.
DEFINE VARIABLE mEnd-Date    AS DATE    NO-UNDO. 
DEFINE VARIABLE mDebAcct     AS CHAR    NO-UNDO.
DEFINE VARIABLE mFilial-id   AS CHAR    NO-UNDO.
DEFINE VARIABLE mBranch-id   AS CHAR    NO-UNDO.
DEFINE VARIABLE mflOk        AS LOGICAL NO-UNDO.
DEFINE VARIABLE mUserCreate  AS CHAR    NO-UNDO.

DEFINE BUFFER chop-date FOR op-date.

{dbgdbo.def}
{globals.i}
{g-error.def}
{svarloan.def NEW}
{g-defs.i}
{crdps.def}
{chkacces.i}

{dpsproc.def}
{def-wf.i new}
{defframe.i new}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get date}
{intrface.get tmess}
{intrface.get tmcod}
{intrface.get db2l}     /* Инструмент для динамической работы с БД. */
{intrface.get lnbh}
{intrface.get cust}
{intrface.get brnch}
{intrface.get dps}
{intrface.get dpspc}
{intrface.get trans}
{intrface.get pbase}
{form.def}

find op-kind where recid(op-kind) eq oprid no-lock no-error.

&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgdboprn.p ("G-CR2.I","op-kind:" + op-kind.op-kind +
                            "; dept.branch:" + dept.branch).
&ENDIF

DEFINE VAR need-valdate AS LOGICAL FORMAT "Дата валютирования/" NO-UNDO.
DEF VAR fmt AS CHAR NO-UNDO.
DEFINE VAR dval LIKE op-entry.value-date NO-UNDO.
DEFINE VAR fler AS LOGICAL NO-UNDO.
define var loan_h as handle no-undo.
define var hProc  as handle no-undo.
def var cod-ost as char no-undo.
DEFINE BUFFER xwop FOR wop.
DEF VAR fl-ok-cr AS LOGICAL NO-UNDO .

Function Set_type returns logical (input l-type as char) in loan_h .
Function Set_ost returns logical (input l-type as char) in loan_h .
run "l-type.p" persistent set loan_h.

{plibinit.i}
RUN peb-unl.p("dps-b2p.p") .

FUNCTION g-checkbank  RETURNS LOGICAL (INPUT vmfo AS CHAR,
                                       INPUT iCodeType AS CHAR,
                                       INPUT vcorr-acct AS CHAR,
                                       INPUT benacct AS CHAR,
                                       OUTPUT result AS INT64,
                                       OUTPUT msg AS CHAR) IN hproc.

/******************************************************************************/
/******************************************************************************/

RUN "g-func.p" PERSISTENT SET hproc.

cur-n = FGetSetting("КодНацВал", ?, "{&in-NC-Code}").

 /* для выходного дня оставим его дату вместо опредня: */
 c-date = if IsWorkDayAll(TODAY, getThisUserXAttrValue('Отделение')) then in-op-date
          else
              IF FGetSetting("ВыхБезОпер", ?, "") = "Да" THEN TODAY
              ELSE in-op-date.

{bfcrloan.i}
{gnumacct.i}
/* Данный стату необходим для схемы начисления процентов */
mStatOp = GetXattrValueEx ("op-kind",
                           op-kind.op-kind,
                           "СтатОтбДок",
                           "").
IF NOT {assigned mStatOp} THEN
   mStatOp = GetXattrInit (op-kind.Class-Code,
                           "СтатОтбДок").
IF {assigned mStatOp} THEN
   RUN SetSysConf IN h_base ("StatOp",
                             mStatOp).

CR_LOAN:
DO TRANSACTION ON ENDKEY UNDO ,LEAVE ON ERROR UNDO,LEAVE
                                      WITH FRAME edit-frame:
   {justasec}
   {insperson.i
      &DefineVars = YES
      &RunSelection = YES}
   
   IF {assigned mOpKind }  THEN 
   DO:
      ASSIGN
         mDBO         = IF GetAttrValue2(mOpKind, mBaseTemplID, "$DBO") = {&RET-ERROR} 
                        THEN NO
                        ELSE YES
         mClass-Code  = GetAttrValue2(mOpKind, mBaseTemplID, "$dps-class-code")
         mCurr        = GetXAttrEx(mClass-Code,"currency","Initial")
         mOpKind-open = GetAttrValue2(mOpKind, mBaseTemplID, "$OpKind") 
         mdep_period  = GetAttrValue2(mOpKind, mBaseTemplID, "$dep_period")
         mOpDate      = IF  GetAttrValue2(mOpKind, mBaseTemplID, "$OpDate") = {&RET-ERROR} 
                        THEN ?
                        ELSE DATE(GetAttrValue2(mOpKind, mBaseTemplID, "$OpDate"))
         mEnd-Date    = IF GetAttrValue2(mOpKind, mBaseTemplID, "$End-Date") = {&RET-ERROR} 
                        THEN ? 
                        ELSE DATE(GetAttrValue2(mOpKind, mBaseTemplID, "$End-Date")) 
         mamt-rub     = IF GetAttrValue2(mOpKind, mBaseTemplID, "$amt-rub") = {&RET-ERROR}
                        THEN 0
                        ELSE DEC(GetAttrValue2(mOpKind, mBaseTemplID, "$amt-rub"))  
         mDebAcct     = GetAttrValue2(mOpKind, mBaseTemplID, "$acct-send") 
         mFilial-id   = GetAttrValue2(mOpKind, mBaseTemplID, "$filial-id") 
         mBranch-id   = GetAttrValue2(mOpKind, mBaseTemplID, "$branch-id") 
         mUserCreate  = IF GetAttrValue2(mOpKind, mBaseTemplID, "$user-id") = {&RET-ERROR}
                        THEN ?
                        ELSE GetAttrValue2(mOpKind, mBaseTemplID, "$user-id") 
      . 
   END.

   IF mDBO THEN
   DO:
      RUN SetSysConf IN h_base ("DBO_Without_Interface", "YES").

      IF NOT {assigned mUserCreate} THEN
      DO:
         &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgdboprn.p ("G-CR2.I","На транзакции не задан реквизит 'Код пользователя'($user-id). Вклад не создан.").
         &ENDIF 
         UNDO CR_LOAN, LEAVE CR_LOAN.
      END.

      IF mOpDate NE ? THEN
         c-date = mOpDate.

      /* проверка на существование опердня */
      DO WHILE NOT mflOk:
         FIND FIRST chop-date WHERE 
               chop-date.op-date EQ c-date 
            NO-LOCK NO-ERROR.

         IF AVAIL chop-date THEN
             mflOk = YES.
         ELSE
         DO:
            IF NOT CAN-FIND(FIRST chop-date WHERE chop-date.op-date GE c-date) THEN
            DO:
               &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgdboprn.p ("G-CR2.I","Не существует подходящего операционного дня после " + STRING(c-date) + ". Вклад не создан.").
               &ENDIF 
               UNDO CR_LOAN, LEAVE CR_LOAN.
            END.

            &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgdboprn.p ("G-CR2.I","Отсутствует операционный день " + STRING(c-date) + ". Вклад будет создан в ближайшем рабочем дне.").
            &ENDIF 
            
            c-date = c-date + 1.
            DO WHILE {holiday.i c-date}:
               c-date = c-date + 1.
            END.
         END.
      END.

      in-op-date = c-date.

      &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgdboprn.p ("G-CR2.I","mOpKind:" + STRING(mOpKind) + 
                           "; mBaseTemplID:" + STRING(mBaseTemplID) +
                           "; mDBO:" + STRING(mDBO) +
                           "; mClass-Code:" + mClass-Code +
                           "; mFilial-id:" + mFilial-id +
                           "; mBranch-id:" + mBranch-id +
                           "; mOpKind-open:" + mOpKind-open +
                           "; mCurr:" + STRING(mCurr) +
                           "; mperson-id:" + STRING(mperson-id) +
                           "; mdep_period :" + STRING(mdep_period) +
                           "; mamt-rub:" + STRING(mamt-rub) +
                           "; mDebAcct:" + mDebAcct +
                           "; c-date:" + STRING(c-date)).
      &ENDIF
   END.  

   {optr.i &DoBefore=YES}
   {g-crloan.i}
   {optr.i &DoAfter=YES}
   {insperson.i
      &RunSetContCode = YES
      &bloan = loan}
END.

IF NOT mDBO THEN
   RUN do_print.

RUN DeleteOldDataProtocol IN h_base ("StatOp").
RUN DeleteOldDataProtocol IN h_base ("ProxyPickVal"). 
RUN DeleteOldDataProtocol IN h_base ("SummaVklada").
RUN DeleteOldDataProtocol IN h_base ("QuestionMinOst").
RUN DeleteOldDataProtocol IN h_base ("SetMinOst").

RUN DeleteOldDataProtocol IN h_base ("PlacementFO_RSHB_cont_code").  
RUN DeleteOldDataProtocol IN h_base ("PlacementFO_RSHB_ShowForms").
RUN DeleteOldDataProtocol IN h_base ("PlacementFO_RSHB_user-id").
RUN SetSysConf IN h_base ("DBO_Without_Interface", "").
hide frame opreq no-pause .

def new shared var rid_loan as recid.
rid-p = RECID (loan). /*для dog.p*/

IF NOT mDBO THEN
   RUN dps-loan-pr.

IF     mDBO 
   AND AVAIL loan THEN
DO:
   RUN AddAttr2TableEx IN h_trans (mOpKind,
                                   mBaseTemplID,
                                   -1,
                                   "", 
                                   0, 
                                   "$dps-contract", 
                                   loan.contract).
   RUN AddAttr2TableEx IN h_trans (mOpKind,
                                   mBaseTemplID,
                                   -1,
                                   "", 
                                   0, 
                                   "$dps-cont-code", 
                                   loan.cont-code).
END.


&IF DEFINED(IS-DEBUG) &THEN
   RUN dbgdboprn.p ("G-CR2.I","avail loan:" + STRING(AVAIL loan) + 
                     "; loan.cont-code:" + (IF AVAIL loan THEN STRING(loan.cont-code) ELSE "") +
                     "; loan.filial-id:" + (IF AVAIL loan THEN STRING(loan.filial-id) ELSE "") +
                     "; loan.cust-id:" + (IF AVAIL loan THEN STRING(loan.cust-id) ELSE "")+
                     "; dept.branch-id:" + STRING(dept.branch) +
                     "; loan.open-date:" + (IF AVAIL loan THEN STRING(loan.open-date) ELSE "") + 
                     "; loan.end-date:" + (IF AVAIL loan THEN STRING(loan.end-date) ELSE "") +
                     "; loan.Class-Code:" + (IF AVAIL loan THEN STRING(loan.Class-Code) ELSE "")) .
&ENDIF

delete procedure(loan_h) .

procedure dps-loan-pr:
{dps-loan.pr}
end procedure.

IF VALID-HANDLE (h_dpsb2p) THEN DO:                        /* <- mitr tt 6733 */
  DELETE PROCEDURE h_dpsb2p.                               /* <- mitr tt 6733 */
END.                                                    /* <- mitr tt 6733 */

{plibdel.i}

{intrface.del}

procedure do_print:
  {g-print1.i}
end.
/* $LINTUSER='BIS' */
/* $LINTENV ='2st' */
/* $LINTVSS ='*' */
/* $LINTDATE='27/11/2014 16:25:38.616+04:00' */
/* $LINTFILE='g-cr2.i' */
/*prosigntql9LiXtgaJo1RoOTP9XsA*/