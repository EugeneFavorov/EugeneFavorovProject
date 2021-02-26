{globals.i}
{intrface.get tmess}

/* +++ g_srocpr.p was humbly modified by (c)blodd converter v.1.11 on 6/7/2017 1:25pm +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: g_srocpr.p
      Comment: Открытие частного вклада
   Parameters: дата опердня, recid(op-kind)
         Uses:
      Used by:
      Created: 27/04/1997 kostik
     Modified: 03/07/2001 eagle заявка 2686,
                                унификация версий,
                                небольшая структуризация кода(define).
     Modified: 06/09/2001 eagle заявка 9193(Пролонгация вкладов с довложением
     modified: 26/10/2002 kostik Для вкладов КАПИТАЛ потребовалось использовать реквизит
                                 ВыпПосле
     Modified: 08/04/2003 Pesv Добавлена обработка парсерных функций в поле "details".
     Modified: 15/11/2007 kraw (0083151) Темпоральные кассовые символы.

*/

def input param in-op-date like op.op-date.
def input param oprid as recid.
DEF VAR old-op-kind AS CHARACTER NO-UNDO.

def var in-op-kind         like op-kind.op-kind no-undo.
def var tcur-db            like op-templ.currency no-undo.
def var tcur-cr            like op-templ.currency no-undo.
def var cur-n              like op-templ.currency no-undo.

def var ac                 like acct.acct no-undo .
def var cr-acct-cr         like acct.acct no-undo .
DEF VAR in-dop-acct        LIKE acct.acct NO-UNDO.
def var vacct-cat          like acct.acct-cat no-undo.

def var cr-cust-id         like person.person-id no-undo .
DEF VAR vclass             LIKE class.class-code NO-UNDO.

def var in-acct-type       like loan-acct.acct-type no-undo.

def var dval               as date no-undo.
def var cred               as date init ?  label 'Плановая дата ' no-undo .
def var in-date            as date no-undo .

def var main-first         as logical no-undo.
def var out-flag           as logical.
def var flag-transfer      AS LOGICAL INITIAL NO NO-UNDO.
def var change-loan-cond   AS LOGICAL INITIAL YES NO-UNDO.
def var modify-flag-cr     as logical no-undo.
def var modify-flag-db     as logical no-undo.
def var fler               as logical no-undo .
def var nopars             as logical no-undo .

def var loan_h             as handle no-undo .
def var str-kau            as char no-undo .
def var tmp-recid          as char no-undo.
def var cod-ost            as char no-undo.
def var cod_               as char no-undo .
def var ret-str            as char no-undo .
DEF VAR templ-acct         AS CHAR                NO-UNDO.
DEF VAR templ-op           AS CHAR                NO-UNDO.
DEF VAR templ-trans        AS CHAR                NO-UNDO.
DEF VAR templ-cond         AS CHAR                NO-UNDO.

def var ii                 as INT64 no-undo .
def var rr                 as INT64 no-undo .
def var fl-o               as INT64 no-undo .

def var kau-rid            as recid extent 2 no-undo .
def var ridd               as recid no-undo .
def var nameproc as char no-undo .
DEF VAR params   AS CHAR NO-UNDO.
DEF VAR tt-cont-code AS CHAR NO-UNDO. /* короткий номер вклада */
def var result as INT64 no-undo .
def var llt                as log no-undo. /* признак довложений */

DEF VAR modify_loan AS RECID NO-UNDO.
DEF VAR op-tmpl-rec AS RECID NO-UNDO.
DEF VAR op-tmpl as CHAR NO-UNDO.
DEF VAR sts AS CHAR NO-UNDO. /*статус для закрытия*/
DEF VAR sts_close AS CHAR NO-UNDO. /*статус, до которого разрешена пролонгация*/
def new global shared stream err .

def buffer xloan           for loan.
def buffer b-loan-cond     for loan-cond.
def buffer xperson         for person.
def buffer cr-acct         for acct.
def buffer sroch-loan-acct for loan-acct.
def buffer xkau-entry      for kau-entry .
def buffer xkau            for kau.
DEF BUFFER prl-op-kind     FOR op-kind.
DEF BUFFER prl-op-template FOR op-template.
DEFINE BUFFER buf_op-template FOR op-template.
DEFINE BUFFER xxkau           FOR kau.
DEFINE VAR hnd-proc     AS HANDLE                NO-UNDO.
DEF VAR mAcct-crPrevVal AS CHAR    NO-UNDO.
DEF VAR mAcct-dbPrevVal AS CHAR    NO-UNDO.

DEFINE VAR vSurr1    AS CHAR NO-UNDO.
DEFINE VAR vSurr2    AS CHAR NO-UNDO.
DEFINE VAR vComm     AS CHAR NO-UNDO.
DEFINE VAR vInter    AS CHAR NO-UNDO.
DEFINE VAR vHeirRole AS CHAR NO-UNDO INIT "".
DEF VAR mReddataoform   AS CHAR NO-UNDO. /* ДР РедДатаОформ */

RUN "l-trans.p" PERSISTENT SET hnd-proc (?,?,?,"ВКЛДВ").

&Scop ByRole  yes

{wdep.def new}
{crdps.def}
{g-defs.i}
{dpsproc.def}
{def-wf.i new}
{dps-a-cl.tmp "NEW SHARED "}
{dps-logs.i
   &DpsLogsQuoter = "'"}
def buffer xwop            for wop.
/* !!!!!!!!!!! здесь */

{defframe.i new}
{ksh-defs.i new}
{setdest2.i &stream="stream err" &filename="_spool1.tmp"}

{f_for_t.i}
{invest.num}
{intrface.get "xclass"}
{intrface.get date}
{intrface.get dps}
{g-currv1.i &OFbase="/*"}
{details.def}
{g-assdps.def} /* "короткий ввод" номера вклада */

Function Set_type returns logical (input l-type as char) in loan_h .
Function Set_ost returns logical (input cod-ost as char) in loan_h .
Function Set_period returns logical (input dat1 as date,input dat2 as date) in loan_h .

{prol.i}

run "l-type.p" persistent set loan_h.
/* Для правильного определения остатка по счету при пролонгации на новый счет */
RUN SetSysConf IN h_base ("RunProl", "YES").

form
perfdep.person-id  colon  14 skip
perfdep.name-last  colon  14 skip
perfdep.first-name colon  14                    perfdep.gender     colon  67
perfdep.birthday   colon  14                    perfdep.country-id colon  67
perfdep.document   colon  14 skip               perfdep.document-id colon 67
perfdep.address[1] colon  14 label "Адрес"  perfdep.phone[1]   colon  59 label "Телефон"
perfdep.address[2] colon  14 label "     "  perfdep.phone[2]   colon  59 label "       " skip
"══════════════════════════════════════════════════════════════════════════════"
in-cont-code     colon  14 label "Номер вклада"FORMAT "x(40)"
cred        &IF DEFINED(recond) = 0 &THEN 
            label 'Дата переоформления'
            &ELSE 
            label 'Дата операции'
            &ENDIF
"═════════════════════════════════════════════════"
"Шаблон:" op-template.op-templ NO-LABEL "═══════════════"
op.doc-num colon 14 op.doc-type doc-type.name NO-LABEL FORMAT "x(30)"
op-entry.amt-cur label "Сумма" colon 14 op-entry.currency op-entry.symbol skip
op-entry.acct-db colon 14
op-entry.acct-cr colon 14
op.details       colon 14 VIEW-AS EDITOR INNER-CHARS 60 INNER-LINES 3
with frame opreq 1 down overlay centered side-labels row 3
      title color bright-white
  "[ ОПЕРАЦИЯ : " + op-kind.name + " ЗА " + string(in-op-date, "99/99/99") + " ]".


release dacct.
release cacct.

{chkacces.i}
{chktempl.i}
{plibinit.i}

ASSIGN
   templ-acct    = list-op-templ(op-kind.op-kind,"acct")
   templ-op      = list-op-templ(op-kind.op-kind,"op")
   templ-cond    = list-op-templ(op-kind.op-kind,"loan-cond")
   templ-trans   = list-op-templ(op-kind.op-kind,"loan-transaction")
   mReddataoform = GetXAttrValueEx ("op-kind",
                                    op-kind.op-kind,
                                    "РедДатаОформ",
                                    GetXattrInit(op-kind.class-code, "РедДатаОформ"))
.

DebugParser = INT64(GetXAttrValueEx("op-kind",
                                  op-kind.op-kind,
                                  "DebugParser",
                                  "0")).

ON any-key OF op.doc-type in frame opreq DO:
  APPLY {&LAST_KEY}.
   DO WITH FRAME opreq :
           FIND doc-type WHERE doc-type.doc-type =  INPUT op.doc-type NO-LOCK NO-ERROR.
           DISPLAY
              doc-type.name WHEN AVAIL doc-type     @ doc-type.name
              ""            WHEN NOT AVAIL doc-type @ doc-type.name
           .
   END.
  {return_no_apply.i}
END.

ON "GO",ctrl-g OF FRAME opreq DO:
   APPLY "GO" TO cred IN FRAME opreq.
   IF {&RETURN_VALUE} <> "" THEN {return_no_apply.i}
   APPLY "ENTRY" TO op.details IN FRAME opreq.
END.

ON  LEAVE , GO OF in-cont-code IN FRAME opreq DO :
  DO WITH FRAME opreq :
     IF LOOKUP(THIS-PROCEDURE:FILE-NAME,PROGRAM-NAME(2)," ") =  0 THEN RETURN.
     /* короткий ввод номера вклада */
     tt-cont-code = INPUT in-cont-code.
     RUN AssumeContCode(INPUT-OUTPUT tt-cont-code).
     IF tt-cont-code =  ? THEN
        {return_no_apply.i}
     IF INPUT in-cont-code <> tt-cont-code THEN
           DISPLAY tt-cont-code @ in-cont-code.
  END.

  in-cont-code = FmtMskAddSuffix(INPUT INPUT in-cont-code,"cont-code").

  FIND last loan WHERE loan.contract  =  "DPS"
          AND loan.cont-code =  in-cont-code
          AND loan.close-date = ? NO-LOCK NO-ERROR.
  if not avail loan then do :
   RUN Fill-AlertSysMes IN h_tmess("","",-1,'Вклад не существует или уже закрыт').

   {return_no_apply.i}
  end.

   RUN chk-blk.p   (loan.cust-cat,loan.cust-id).
   IF {&RETURN_VALUE} =  "0" THEN
   DO:
      RUN Fill-AlertSysMes IN h_tmess ("", "", "-1",
         {&ClientBlocked}).
      {return_no_apply.i}
   END.

  RUN CreateFrmFields (?,"LoanRecid","",STRING(RECID(loan))).

   ASSIGN
      in-contract = loan.contract
      in-date     = loan.end-date
      .
&IF DEFINED(recond) = 0 &THEN
   RUN Init_Contract_Date (OUTPUT cred) .
&ELSE 
   cred = in-op-date.
&ENDIF
   FIND FIRST person WHERE person.person-id =  loan.cust-id NO-LOCK NO-ERROR.
   IF AVAIL person THEN
   DO:
      DISP
         person.person-id   @ perfdep.person-id
         person.name-last   @ perfdep.name-last
         person.first-name  @ perfdep.first-name
         person.gender      @ perfdep.gender
         person.birthday    @ perfdep.birthday
         person.document-id @ perfdep.document-id
         person.document    @ perfdep.document
         person.address[1]  @ perfdep.address[1]
         person.address[2]  @ perfdep.address[2]
         person.phone[1]    @ perfdep.phone[1]
         person.phone[2]    @ perfdep.phone[2]
         cred      @ cred

          with frame opreq.
&IF DEFINED(recond) <> 0 &THEN 
    cred:read-only = TRUE.
&ELSE 
&ENDIF
   END.
   ELSE DO:
      DISP
         ""   @ perfdep.person-id
         ""   @ perfdep.name-last
         ""   @ perfdep.first-name
         ""   @ perfdep.gender
         ""   @ perfdep.birthday
         ""   @ perfdep.document-id
         ""   @ perfdep.document
         ""   @ perfdep.address[1]
         ""   @ perfdep.address[2]
         ""   @ perfdep.phone[1]
         ""   @ perfdep.phone[2]
      WITH FRAME opreq      .
      {return_no_apply.i}
   END.
   set_type(loan.cont-code) .
END.
on  return, go of cred in frame opreq  do :
&IF DEFINED(recond) = 0 &THEN 
  if input cred <  loan.end-date then do :
  RUN Fill-AlertSysMes IN h_tmess("","",-1,"Пролонгацию нельзя делать раньше даты закрытия вклада " + string(loan.end-date)).

  {return_no_apply.i}
 end.
 if in-op-date <  loan.end-date and input cred >  loan.end-date then do :
  RUN Fill-AlertSysMes IN h_tmess("","",-1,'При проведении пролонгации до даты окончания договора плановая дата должна совпадать с датой окончания договора').

  {return_no_apply.i}
 end.
&ELSE
    IF vHeirRole = "" THEN
    DO:
      pick-value = "".
      /* Выбираем, какому наследнику будем сумму выдавать: */
      RUN heiravail.p(loan.class-code, loan.contract, loan.cont-code, 4).
      IF pick-value = "" THEN
         {return_no_apply.i '"Не еыбран наследник"'}
      ELSE
         vHeirRole = pick-value.
    END.
    RUN SetSysConf IN h_Base ("_HeirRol" + loan.cont-code , vHeirRole).
&ENDIF

 IF FGetSetting("ПровДтПрол","","") =  "Да" THEN 
 DO: 
    IF in-op-date >  loan.end-date AND input cred >  in-op-date AND NOT {holiday.i loan.end-date} THEN 
    DO:
  RUN Fill-AlertSysMes IN h_tmess("","",-1,'При проведении пролонгации после даты окончания договора плановая дата должна быть не больше даты опердня').

  {return_no_apply.i}
    END.
 END.

end.



ON "F1" ANYWHERE  DO:
   DEF VAR vamt     AS DECIMAL NO-UNDO.
   DEF VAR field-fr AS CHAR    NO-UNDO.
   field-fr = {&FRAME_FIELD}.
   DO WITH FRAME opreq :
      CASE {&FRAME_FIELD}:
         WHEN 'acct-cr'  THEN RUN acct(op-template.acct-cat,4) .
         WHEN 'acct-db'  THEN RUN acct(op-template.acct-cat,4) .
         WHEN 'currency' THEN RUN currency.p('currency',4).
         WHEN 'amt-cur'  THEN RUN calc.p(4) .
         WHEN 'symbol'   THEN RUN browseld.p("КасСимволы", "class", "КасСимволы", "", 4) .
         WHEN 'in-cont-code' THEN RUN dpsdispc.p('dps',?,'Ч',?,4).
         OTHERWISE RETURN .
      END CASE .
      IF ({&LAST_EVENT_FUNCTION} = "GO" OR {&LAST_EVENT_FUNCTION} = "RETURN") AND pick-value <> ? THEN do :
         CASE field-fr:
            WHEN "symbol"   THEN DISPLAY pick-value @ op-entry.symbol.
            WHEN "currency" THEN DISPLAY pick-value @ op-entry.currency.
            WHEN "acct-db"  THEN DISPLAY ENTRY(1, pick-value) @ op-entry.acct-db.
            WHEN "acct-cr"  THEN DISPLAY ENTRY(1, pick-value) @ op-entry.acct-cr.
            WHEN "amt-cur"  THEN DISPLAY DECIMAL(pick-value) @ op-entry.amt-cur.
            WHEN "in-cont-code" THEN DISPLAY pick-value @ in-cont-code.
         END CASE .
      END.
      {return_no_apply.i}
   END.
END.
ON LEAVE, GO OF op-entry.acct-db IN FRAME opreq DO:
   IF op-entry.acct-db:SCREEN-VALUE <> mAcct-dbPrevVal THEN
   DO:
      RUN transmes.p(STRING(INPUT op-entry.acct-db),
                     STRING(INPUT op-entry.currency),
                     op-kind.op-kind,
                     "ДЕБЕТ",
                     INPUT-OUTPUT mClMessList).
      mAcct-dbPrevVal = op-entry.acct-db:SCREEN-VALUE.
   END.
END.
ON LEAVE, GO OF op-entry.acct-cr IN FRAME opreq DO:
   IF op-entry.acct-cr:SCREEN-VALUE <> mAcct-crPrevVal THEN
   DO:
      RUN transmes.p(STRING(INPUT op-entry.acct-cr),
                     STRING(INPUT op-entry.currency),
                     op-kind.op-kind,
                     "КРЕДИТ",
                     INPUT-OUTPUT mClMessList).
      mAcct-crPrevVal = op-entry.acct-cr:SCREEN-VALUE.
   END.
END.
op-tmpl = string(GET_OP-TEMPL(op-kind.op-kind,"loan","")).
FIND FIRST op-template OF op-kind WHERE op-template.op-template =  INT64(op-tmpl) NO-LOCK NO-ERROR.
op-tmpl-rec = RECID(op-template).
sts_close = Get_Param('loan-status-cl',op-tmpl-rec).

LOAN_CODE:
DO TRANS WITH FRAME opreq ON ENDKEY UNDO loan_code, LEAVE loan_code
                          ON ERROR  UNDO loan_code, LEAVE loan_code:
   COLOR DISPLAY BRIGHT-GREEN
                 doc-type.name
                 op-entry.acct-db
                 op-entry.acct-cr.

   COLOR DISPLAY BRIGHT-WHITE
                 perfdep.name-last
                 perfdep.first-name
                 perfdep.gender
                 perfdep.birthday
                 perfdep.document-id
                 perfdep.document
   .
   {set.i &THIS_FRAME = "opreq" &EXFILE = "g_srocpr.p.st1" {&*}}.

   IF AVAIL person THEN
      RUN pGetMessPerson IN h_dps (op-kind.op-kind,
                                   person.person-id).

   in-cont-code = FmtMskAddSuffix(in-cont-code,"cont-code").
   {chktype.i}
   in-op-kind = loan.op-kind.
   {for_ved.i &filename=_spool1.tmp}
/* проверка на класс вклада - какой тип довложений брать */

/*********************************************************/
/* создаем довложение */
do ii = 1 to num-entries(templ-trans):
 find first op-template of op-kind where op-template.op-templ =  INT64(entry(ii, templ-trans))
        no-lock no-error /*! no-error*/.
 IF Cr_loan_trans(BUFFER loan-transaction,
                  BUFFER loan,
                  RECID(op-template),
                  in-op-date,
                  mess) =  '-1' THEN DO:
  RUN Fill-AlertSysMes IN h_tmess("","",-1,"~n" + CHR(32) + STRING(mess) + CHR(32) + FILL("~n",1)).

                  UNDO Loan_code,LEAVE Loan_code.
 END.
 IF AVAIL loan-transaction THEN DO:
    llt = yes.
    loan-transaction.trans-code = TRANS_NUMBER(loan-transaction.contract,
                                               loan-transaction.cont-code).
     RUN put-loan-trans IN hnd-proc (RECID(loan-transaction)) .
 END.
end.

&IF DEFINED(recond) <> 0 &THEN
  loan_cond:
  DO ON ENDKEY UNDO loan_cond, leave loan_cond
     ON ERROR  UNDO loan_cond, RETRY loan_cond:
   
     DEFINE VAR vLoanTempl  AS INT64 NO-UNDO.
     DEFINE VAR vPenOpKind  AS CHAR    NO-UNDO.
     DEFINE VAR vPenOpTempl AS CHAR    NO-UNDO.
     DEFINE BUFFER b-op-template FOR op-template.
     
     /* если транзакция создания условия - 
        первое, что делаем - создаем условия */
     FIND FIRST op-template OF op-kind 
                            WHERE CAN-DO( templ-cond, STRING(op-template.op-template))
                            NO-LOCK NO-ERROR.
     FIND LAST loan-cond WHERE loan-cond.contract  =  loan.contract
                           AND loan-cond.cont-code =  loan.cont-code
                           AND loan-cond.since     <= in-op-date 
                         NO-LOCK NO-ERROR.
     
     /* проверка необходимости создания условия */
     {bef-heir.i &bef-undo="UNDO loan_cond, LEAVE loan_cond."
                 &undo-no-proc="UNDO loan_cond, LEAVE loan_cond." }
     /* Создаем новое условие */
     CREATE b-loan-cond.
     BUFFER-COPY loan-cond EXCEPT since TO b-loan-cond.
     ASSIGN b-loan-cond.since = in-op-date.
     /* Создаем для условия допреквизиты */
     vSurr1 = loan.contract + "," + loan.cont-code + "," + 
              STRING(YEAR (loan-cond.since),"9999") +
              STRING(MONTH(loan-cond.since),"99") +
              STRING(DAY  (loan-cond.since),"99").
     vSurr2 = loan.contract + "," + loan.cont-code + "," + 
              STRING(YEAR (b-loan-cond.since),"9999") +
              STRING(MONTH(b-loan-cond.since),"99") +
              STRING(DAY  (b-loan-cond.since),"99").
     RUN Get_Last_Pen-Commi IN h_dpspc(RECID(loan), loan-cond.since, in-op-date, OUTPUT vComm).
     RUN Get_Last_Pen_Inter IN h_dpspc(RECID(loan), loan-cond.since, in-op-date, OUTPUT vInter).
     UpdateSigns("loan-cond", vSurr2, "commission", vComm,  ?).
     UpdateSigns("loan-cond", vSurr2, "interest",   vInter, ?).
     RUN SetSysConf IN h_base ("HeirConditionSet", "YES").
     
     /* При создании условия нужно сменить тип договора: */
     FIND CURRENT loan EXCLUSIVE-LOCK NO-ERROR.
     IF NOT AVAILABLE loan THEN UNDO loan_cond, LEAVE loan_cond.
     
     vLoanTempl = Get_Op-templ(loan.op-kind,'loan',"").
     FIND FIRST op-template OF    op-kind 
                            WHERE op-template.op-template = vLoanTempl
                            NO-LOCK NO-ERROR.
     IF NOT AVAILABLE op-template THEN
        UNDO loan_cond, LEAVE loan_cond.
     
     /* Если есть транзакция пролонгации - сменить тип договора, если нет - оставить как есть */
     vPenOpKind = GetXAttrValue("op-template", 
                                loan.op-kind + "," + STRING(op-template.op-template),
                                "prol-kind-pen").
     IF vPenOpKind = "" THEN 
       vPenOpKind = GetXAttrValue("op-template", 
                                  loan.op-kind + "," + STRING(op-template.op-template),
                                  "prol-kind").
     vPenOpTempl = STRING(Get_Op-templ(vPenOpKind,'loan',"")).
     FIND FIRST b-op-template WHERE b-op-template.op-kind     = vPenOpKind
                                AND b-op-template.op-template = INT64(vPenOpTempl)
                              NO-LOCK NO-ERROR.
     IF AVAILABLE b-op-template THEN
     DO:
        loan.cont-type = GetXAttrValue("op-template", 
                                       b-op-template.op-kind + "," + STRING(b-op-template.op-template),
                                       "cont-type").
     END.
  END.
&ENDIF

    set_type(in-cont-code) .
    kau-rid = 0 .

   RUN SetSysConf IN h_base("op-contract-date", STRING(cred)).

   {cr_acc_n.i &Publish_CDate = YES}

   RUN pFillAcctCl (ROWID(loan),
                    cr_acc_p-vsincedate).

   TMPL:
   FOR EACH op-template OF op-kind NO-LOCK BREAK BY op-templ.op-templ
      WITH FRAME opreq ON ENDKEY UNDO loan_code, leave loan_code
                             ON ERROR  UNDO loan_code, RETRY loan_code:
      run deleteolddataprotocol in h_base("КАПИТАЛ%%%") .
      run deleteolddataprotocol in h_base("КАПИТАЛРЕФ") .
      nopars = no.
      release op.
      release op-entry .
      release kau-entry.
      IF CAN-DO(templ-op,STRING(op-template.op-template)) THEN DO:
         IF tmp-recid <> "" THEN DO:
            FIND cr-acct WHERE RECID(cr-acct) =  INT64(ENTRY(1,tmp-recid)) 
               EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            FIND sroch-loan-acct WHERE RECID(sroch-loan-acct) =  INT64(ENTRY(2,tmp-recid)) 
               EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         END.
         &IF DEFINED(recond) = 0 &THEN
         {bef-tran.i &bef-undo="undo tmpl, next tmpl."
                     &undo-no-proc="undo loan_code, leave loan_code ." }
         &ELSE 
         {bef-heir.i &bef-undo="undo tmpl, next tmpl."
                     &undo-no-proc="undo loan_code, leave loan_code ." }
         &ENDIF
         
         CREATE wop .
         assign
            wop.con-date = cr_acc_p-vsincedate
            wop.op-templ = op-templ.op-templ
            wop.op-kind  = op-kind.op-kind /* вставлено Sema 23/05/99 */
            in-status    = op-template.op-status
            cur-op-date  = in-op-date
            tacct-cr     = ?
            tacct-db     = ?
         .

        {g-acctv1.i &vacct  = tacct }
            wop.con-date = cred.
            CREATE op.
            /*validate op no-error.*/
            {op(sess).cr}
            {g-op.ass}
            op.op-status = in-status.
            validate op no-error.

        IF tacct-db <> tacct-cr THEN DO :
           CREATE op-entry.
           {g-en.ass}
        END.

        ELSE DO :

           IF op.class-code <> "opkau" THEN
              UNDO tmpl, NEXT tmpl.

           nopars = YES.
           RUN crkau(tacct-db, (in-contract  + ',' + in-cont-code + ',' + IF NOT AVAIL loan-transaction THEN 'ОстВклС'
                      ELSE (loan-transaction.trans-code + ',ОстВклДв')),
                      RECID(op) ).
        END.

        ASSIGN
           op.contract-date    = cred
           op.details          = op-templ.details
        .
        if avail op-entry then do :
           assign
              op-entry.value-date = in-op-date
              op-entry.op-status  = op.op-status
              op-entry.acct-cat   = op.acct-cat
              op-entry.acct-cr    = tacct-cr
              op-entry.acct-db    = tacct-db
        .
              op-entry.currency   = IF op-templ.currency <> ? then  GetCurr(op-templ.currency)
                                                              ELSE loan.currency .
       end.
&IF DEFINED(recond) = 0 &THEN
       /* здесь подключен метов корректировки плановой даты документов, создаваемых при пролонгации , дата сдвигается на 1 день, если дата переоформления,
          является датой с которой должны начисляться проценты при переоформленни -пролонгации вклада */
          run  GetClassMethod in h_xclass (loan.class-code,'ch_cont_dat',
                                            "","",
                                            OUTPUT nameproc,
                                            OUTPUT params).
           if nameproc <> ? then  do:
              run  value(nameproc + ".p")(recid(loan),recid(op),cred, output fl-o ) .
              if fl-o < 0 then  UNDO loan_code,LEAVE loan_code.
            end.
&ENDIF
       {asswop.i}

       ASSIGN wop.op-recid = RECID(op-entry).

       RUN ProcessDetails (RECID(wop), INPUT-OUTPUT op.details).

        ASSIGN
           wop.acct-db  = tacct-db
           wop.acct-cr  = tacct-cr
           wop.currency = IF op-templ.currency <> ? then  GetCurr(op-templ.currency)
                                                    ELSE loan.currency
           wop.con-date = op.contract-date
           dval         = if avail op-entry then  op-entry.value-date else op.op-date
           wop.op-recid = IF AVAIL op-entry THEN recid(op-entry) ELSE ?
        .
         cod-ost = get-ost-templ(op-kind.op-kind,op-templ.op-templ) .
         set_type(in-cont-code).
         if op-templ.amt-rub matches '*Нач*'  and  cod-ost <> ? then
              set_ost(cod-ost).
        if op-templ.acct-cr matches '*loan-dps-p*' and op-templ.acct-db matches '*loan-dps-t*' then
        do :
           set_type(in-cont-code).
           set_ost('ОстВклВ').
        end.                      
        IF not nopars and
           ((op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "") OR
           (op-templ.prep-amt-natcur <> ? AND op-templ.prep-amt-natcur <> ""))
            THEN DO:            
            RUN parssen.p (RECID(wop), in-op-date, OUTPUT fler).
            IF fler THEN UNDO tmpl,RETRY tmpl.
            IF wop.amt-rub = 0 AND wop.amt-cur = 0 THEN do :
               if avail op-entry then delete op-entry .
               for each kau-entry of op EXCLUSIVE-LOCK:
                  delete kau-entry.
               end.
               delete op .
            end.
        end.
        else nopars = yes.
         /* если ввод суммы осуществляется ручками, то вначале присвоем "0"
           субаналитическим проводкм. */
         if wop.amt-rub = ? then wop.amt-rub = 0.
         if wop.amt-cur = ? then wop.amt-cur = 0.
         /* Ужасно, а что делать */
        if avail op-entry then
        ASSIGN
           op-entry.amt-rub = wop.amt-rub
           op-entry.amt-cur = IF op-entry.currency <> ""  THEN wop.amt-cur
                                                          ELSE op-entry.amt-cur
           op-entry.user-id = userid('bisquit')   .

        FIND FIRST doc-type OF op NO-LOCK NO-ERROR.
        if avail op-entry then do :
          DISP
            op-templ.op-templ
            op.doc-num
            op.doc-type
            doc-type.name WHEN AVAIL doc-type     @ doc-type.name
            ""            WHEN NOT AVAIL doc-type @ doc-type.name
            op-entry.acct-db                      @ op-entry.acct-db
            op-entry.acct-cr                      @ op-entry.acct-cr
            op-entry.amt-rub WHEN op-entry.currency =  "" @ op-entry.amt-cur
            op-entry.amt-cur WHEN op-entry.currency <> "" @ op-entry.amt-cur
            op-entry.currency
            op-entry.symbol
            op.details
         .
         IF AVAIL cr-acct THEN DO:
            IF op-entry.acct-cr =  cr-acct.acct THEN
               modify-flag-cr = YES.
            ELSE
               modify-flag-cr = NO.
            IF op-entry.acct-db =  cr-acct.acct THEN
               modify-flag-db = YES.
            ELSE
               modify-flag-db = NO.
            tmp-recid = "".
         END.
         
         IF  {assigned op-entry.currency}
            AND NOT CAN-FIND(FIRST acct WHERE acct.acct     =       op-entry.acct-cr
                                          AND acct.currency =       ""
                                          AND acct.contract MATCHES "*Касса*")
            AND NOT CAN-FIND(FIRST acct WHERE acct.acct     =       op-entry.acct-db
                                          AND acct.currency =       ""
                                          AND acct.contract MATCHES "*Касса*")
         THEN DO:
            op-entry.symbol = "" .
            DISPLAY op-entry.symbol WITH FRAME opreq.
         END.
         
         {set.i &THIS_FRAME = "opreq" &EXFILE = "g_srocpr.p.st2" &NODISPLAY = "YES" {&*}}.
         end.
         /*здесь может пропадать Exclusive-Lock на cr-acct - делаем заново*/
         FIND CURRENT cr-acct EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAIL op-entry AND AVAIL cr-acct AND AVAIL sroch-loan-acct THEN DO:
            IF modify-flag-cr = YES AND op-entry.acct-cr <> cr-acct.acct THEN
            DO:
               /*данная проверка (а также подобная ей 5 строчками ниже) необходима,
                 чтобы при воборе уже существующего счета в броузере счетов процедура
                 не пыталась создать второй такой же, вызывая ошибку*/
               IF NOT CAN-FIND(FIRST cr-acct WHERE cr-acct.acct =  op-entry.acct-cr) THEN
                  ASSIGN
                     cr-acct.acct         = op-entry.acct-cr
                     sroch-loan-acct.acct = op-entry.acct-cr
                  .
            END.
            IF modify-flag-db = YES AND op-entry.acct-db <> cr-acct.acct THEN
            DO:
               IF NOT CAN-FIND(FIRST cr-acct WHERE cr-acct.acct =  op-entry.acct-db) THEN
                  ASSIGN
                     cr-acct.acct         = op-entry.acct-db
                     sroch-loan-acct.acct = op-entry.acct-db
                  .
            END.
         END.
         if avail op-entry then do :
         if op.op-status begins "А" then
          assign
           op.op-date       = ?
           op-entry.op-date = op.op-date
           .
          if nopars then do:
           if op-entry.currency > "" then do:
               FIND LAST instr-rate WHERE instr-rate.instr-cat = "currency" AND
                         instr-rate.rate-type = "Учетный" AND
                         instr-rate.instr-code = op-entry.currency AND
                         instr-rate.since <= op-entry.op-date  /* ? */
                         /* использование USE-INDEX в соответствии с текущим функционалом */
                         USE-INDEX rate-instr NO-LOCK NO-ERROR.
               assign op-entry.amt-rub =
                       op-entry.amt-cur * instr-rate.rate-instr / instr-rate.per
               .
           end.
           else
               assign op-entry.amt-rub = op-entry.amt-cur
                      op-entry.amt-cur = 0
               .
          end.
          {aft-temp.i &aft-undo=" UNDO tmpl,RETRY tmpl."}
          {op-entry.upd &871=yes &open-undo="UNDO tmpl,RETRY tmpl"}
          /* Создание ДР типа PARSEN_<КодРеквизита>*/
          {g-psigns.i}
          {crs.i}
         end.
      END. /* END IF CAN-DO */ 
   END. /* END op-templates */

&IF DEFINED(recond) = 0 &THEN
   /* Для процедуры создания условий отключаем препроцессором
      модификацию условий и договора после документов */
   ASSIGN
      in-op-kind  = loan.op-kind
      old-op-kind = in-op-kind
   .
   RUN put_kd.p (op-kind.op-kind,
                 RECID(loan),
                 "loan-op-kind",
                  OUTPUT fl-o,
                  output in-op-kind).
   IF fl-o =  -1 THEN DO:
      RUN Fill-AlertSysMes IN h_tmess("","",-1,'С этим договором кто-то работает').

      UNDO loan_code,RETURN.
   END.
   IF fl-o <> 0 THEN
   RUN put_kd.p (loan.op-kind,
                 RECID(loan),
                 "prol-kind",
                 OUTPUT fl-o,
                 output in-op-kind).
   IF fl-o =  -1 THEN DO:
      RUN Fill-AlertSysMes IN h_tmess("","",-1,'С этим договором кто-то работает').

      UNDO loan_code,RETURN.
   END.
   IF in-op-kind =  ? OR in-op-kind =  "" THEN DO:
      ASSIGN
         change-loan-cond = NO
         in-op-kind       = loan.op-kind
      .
   END.
   ELSE IF in-op-kind =  old-op-kind THEN DO:
      ASSIGN
         change-loan-cond = NO
         in-op-kind       = loan.op-kind
      .
   END.
   ELSE DO:
      ASSIGN
         change-loan-cond = YES
      .
   END.
  /* здесь подключается метод для определения новых-старых реквизитов вклада */
   run  GetClassMethod in h_xclass (loan.class-code,'ch_loan',
                                    "","",
                                    OUTPUT nameproc,
                                    OUTPUT params).
   modify_loan = 0.
   if nameproc <> ? then  do:
     RUN Modify-Loan(in-cont-code,YES).
     IF {&RETURN_VALUE} <> "" THEN DO:
        IF {&RETURN_VALUE} = "UNDO_STS" THEN modify_loan = recid(loan).

        UNDO loan_code,LEAVE loan_code.
     END.
     run  value(nameproc + ".p")(recid(loan),in-op-date,cred, output fl-o ) .
     if fl-o < 0 then  UNDO loan_code,LEAVE loan_code.
   end.
   else do :
     RUN Modify-Loan(in-cont-code,no).
     CASE {&RETURN_VALUE}:
        WHEN "UNDO"          THEN UNDO loan_code,LEAVE loan_code.
        WHEN "UNDO_STS"      THEN do:
          /*откат, но необходимо поменять статус у вклада*/
          modify_loan = recid(loan).
          UNDO loan_code,LEAVE loan_code.
        END.
        WHEN "TRANSFER CASH" THEN flag-transfer = YES.
     END CASE.
     IF change-loan-cond =  YES THEN DO:
          RUN Modify-Loan-Cond(In-Cont-Code,in-op-date).
        CASE {&RETURN_VALUE}:
           WHEN "UNDO"       THEN UNDO loan_code,LEAVE loan_code.
        END CASE.
     END.
   end.
   {cdealend.i
       &mt = "UNDO loan_code, LEAVE loan_code"
       &p = "o"
   }

   {cdealend.i
       &mt =  "UNDO loan_code, LEAVE loan_code"
       &p = "n"
   }

   IF fGetSetting ("ЗакрСчетПрол", "", "Нет") =  "Да" AND templ-acct <> "" THEN
   DO:
      RUN acctclopr.p (loan.cont-code, 
                       in-op-date, 
                       OUTPUT fl-o).      
   END.
&ENDIF

  &IF DEFINED(recond) <> 0 &THEN   
  /* Заполнить для выбранной роли реквизит "ВЫДАНО" */
  FIND FIRST cust-role WHERE cust-role.cust-role-id = INT64(vHeirRole) EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE cust-role THEN 
  DO:
     cust-role.close-date = cred.
  END.
  &ENDIF

END. /*LOAN_CODE*/

IF modify_loan <> 0  AND modify_loan <> ? AND sts_close <> ? THEN DO:
 RUN modify-sts(INPUT modify_loan).
END.

RUN SetSysConf IN h_base ("HeirConditionSet", "").
IF AVAIL loan THEN DO:
RUN DeleteOldDataProtocol IN h_base ("_HeirRol" + loan.cont-code).
{g-print1.i}
{preview2.i &stream="stream err" &filename=_spool1.tmp}
END.
output stream err close.
HIDE FRAME opreq no-pause .

{savecond.i }

/* Комманда ОС после выполнения транзакции */
{cmd-exec.i
    &cmd        = "'Postcmd'"
}

return.

FINALLY: 
   RUN DeleteOldDataProtocol IN h_base ("RunProl").    

   delete procedure loan_h .
   delete procedure hnd-proc.

   {plibdel.i}
   {intrface.del}   
END FINALLY.

/* Процедура установки  плановой даты при пролонгации */

PROCEDURE Init_Contract_Date .
  DEF OUTPUT PARAM contract-date AS DATE NO-UNDO .

  DEF VAR dep_period AS CHAR NO-UNDO .
  DEF VAR fl-old AS LOGICAL NO-UNDO .

  New_Dps_Prol(BUFFER loan,OUTPUT dep_period,OUTPUT fl-old) .
  IF fl-old  and in-op-date > loan.open-date
  THEN  contract-date = loan.end-date .
  ELSE IF in-op-date > loan.end-date and {holiday.i loan.end-date} THEN
  DO:
      contract-date = loan.end-date + 1.
      DO WHILE {holiday.i contract-date}:
         contract-date = contract-date + 1.
      END.
      contract-date = contract-date + 1.

      IF chk_date_holiday (contract-date, 
                           contract-date,
                           loan.contract + "," + loan.cont-code,
                           loan.class-code) THEN
         RUN  Get_Contract_date in h_dpspc
            (RECID(loan),in-op-date,output contract-date) .
  END.
  ELSE
  RUN  Get_Contract_date in h_dpspc
     (RECID(loan),in-op-date,output contract-date) .
END.
/* $LINTFILE='g_srocpr.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='07/04/2017 12:15:47.900+03:00' */
/*prosignkipTKGO4G+LukbLdxeX/ZQ*/
/* --- g_srocpr.p was humbly modified by (c)blodd converter v.1.11 on 6/7/2017 1:25pm --- */
