/*
               Банковская интегрированная система БИСквит
    copyright: (c) 1992-2017 ЗАО "Банковские информационные системы"
     filename: g_doloan.p
      comment: Довнесение для вкладов
   parameters: нет
         uses:
      used by:
      created: ??/??/???? kostik
     modified: 04/03/2003 SAP заявка 0012066.Контроль довнесений и изъятий.
  last change:
*/

{g-error.def}
{g-defs.i}
{crdps.def}
{dpsproc.def}
{def-wf.i new}
{defframe.i new}
{invest.num}
{ksh-defs.i new}
{intrface.get "xclass"}
{intrface.get "ltran"}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get dps}
{currency.def}

DEF INPUT PARAMETER in-op-date LIKE op.op-date.
DEF INPUT PARAMETER oprid      AS RECID.

DEFINE VAR CODE-WORD-FOR-LOAN-TRANS
                            AS   CHAR    INITIAL "ВКЛДВ" NO-UNDO.
DEFINE VAR need-valdate     AS   LOGICAL FORMAT "Дата валютирования/" NO-UNDO.
DEFINE VAR fmt              AS   CHAR    NO-UNDO.
DEFINE VAR dval             LIKE op-entry.value-date NO-UNDO.
DEFINE VAR fler             AS   LOGICAL NO-UNDO.
DEFINE VAR result           AS   INT64 NO-UNDO.
DEFINE VAR vmfo             LIKE op-bank.bank-code.
DEFINE VAR vcorr-acct       LIKE op-bank.corr-acct.
DEFINE VAR mforeq           AS   LOGICAL NO-UNDO.
DEFINE VAR msg              AS   CHAR    FORMAT "x(40)" NO-UNDO.
DEFINE VAR acctkey          AS   INT64 NO-UNDO.
DEFINE VAR temp-acct        AS   CHAR    NO-UNDO.
DEFINE VAR fl-err           AS   INT64 INITIAL -1.
DEFINE VAR str_recids       AS   CHAR    NO-UNDO.
DEFINE VAR beg-templ        AS   CHAR    INITIAL '0'.
DEFINE VAR c-date           AS   DATE    NO-UNDO.
DEFINE VAR hProc            AS   HANDLE  NO-UNDO.
DEFINE VAR loan_h           AS   HANDLE  NO-UNDO.
DEFINE VAR hnd-proc         AS   HANDLE  NO-UNDO.
DEFINE VAR method           AS   CHAR    NO-UNDO. /*процедура, выполняемая до иттерации*/
DEFINE VAR vConttypeChar    AS   CHAR    INITIAL "*" NO-UNDO.

{dps-exr.i &DefineVars = YES}

DEFINE BUFFER xxop         FOR op .
DEFINE BUFFER xwop         FOR wop.
DEFINE BUFFER xop-template FOR op-template.

RUN "l-trans.p" PERSISTENT SET hnd-proc (?,?,?,CODE-WORD-FOR-LOAN-TRANS).
Function Set_type returns logical (input l-type as char) in loan_h .
Function Set_ost returns logical (input l-type as char) in loan_h .
function g-checkbank  returns logical (input vmfo as char, INPUT iCodeType AS CHARACTER,input vcorr-acct as char, input benacct as char, output result as INT64, output msg as char) in hproc.
RUN "g-func.p" PERSISTENT SET hProc.
RUN "l-type.p" PERSISTENT SET loan_h.

&Scop BYrole  yes.
&Scop DoLoan  yes.
&Scop Dotacct yes.
&Scop DoOp    yes.
&Scop OFcash  yes.
&scop EdtFlg   'ДА'
&Glob undo_type return "no-apply"
&glob mess_type  'При обработке указанного вклада нельзя использовать выполняемую транзакцию.'

{g-currv1.i &OFbase= "/*"}

{intrface.get date}

{g-frame.i &DoFrame = Yes
           &row     = 1
           &pl-date  = c-date

}

{chkacces.i}
{g-trig.i 
   &OFcash=* 
   &DoLoan = Yes
   &Ret_Name-Ben_Doc-Name = YES
   &recalc-acct=Yes
}


/*Очистка информации о начисленных процентах*/
   RUN DeleteOldDataProtocol IN h_base ("КАПИТАЛ%%%").
   RUN DeleteOldDataProtocol IN h_base ("КАПИТАЛРЕФ").
/******************/

c-date = if IsWorkDayAll(TODAY, getThisUserXAttrValue('Отделение'))
         then in-op-date
         else
             IF FGetSetting("ВыхБезОпер", ?, "") = "Да" THEN TODAY
             ELSE in-op-date.

vContractDate  = c-date.



gen:
DO TRANS WITH FRAME opreq ON ENDKEY UNDO, LEAVE
                          ON ERROR  UNDO, LEAVE:
   FIND FIRST op-kind WHERE RECID(op-kind) = oprid NO-LOCK NO-ERROR.
   {optr.i &DoBefore=YES}
   {befexpr.i &befopkind = op-kind.op-kind}

   vConttypeChar = GetXAttrValueEx("op-kind",op-kind.op-kind,"cont-type","*").
   DebugParser   = INT64(GetXAttrValueEx("op-kind",op-kind.op-kind,"DebugParser","0")).
   ASSIGN
      tcur     = ?
      tacct-db = ?
      tacct-cr = ?
      tamt     = 0
   .
   {plibinit.i}
   FOR EACH op-template OF op-kind WHERE op-template.cr-class-code matches "OP*"
                         NO-LOCK WITH FRAME opreq  ON ENDKEY UNDO gen, LEAVE gen
                                                   ON ERROR  UNDO , RETRY
                                                   BREAK BY op-template.op-template:
      DO:      
      /* Проверка на шаблон без создания объектов */
      IF GetXAttrValueEx("op-template",
                         op-template.op-kind + "," + STRING(op-template.op-template),
                         "PrsnTmpl",
                         "Нет") EQ "Нет" THEN
      DO:
      {g-frame.i &DoBefore = Yes
                 &OP-UNDO  = " tcur = ? .
                               DELETE op-entry .
                               DELETE op .
                               NEXT."
                    &DoBeforeAfterSet = "if  AVAIL loan and can-find(first loan-trans of loan no-lock) then run put-loan IN hnd-proc (RECID(loan)). "
      }
      END.
      ELSE
      DO:
            /* От формы требуется только ввода номера договора и даты */
            {g-frame.i 
               &DoBefore      = Yes 
               &OFcash        = Yes
               &nocreate      = YES
               &browse-entry  = YES
               &NOmfo         = YES
               &NomfoTotal    = YES
            }
      END. /* PrsnTmpl */

      END.      
      
      /* Определение доверенного лица */
      {dps-exr.i 
          &RunSelection    = YES
          &undo-block      = "UNDO GEN, LEAVE GEN. "
          &in-cont-code    = in-cont-code
          &contract-date   = vContractDate
          &op-kind         = op-kind.op-kind
      } 
      RUN SetSysConf IN h_Base ("_CustType_", mAgentCat).
      RUN SetSysConf IN h_Base ("_Proxy-code_", mProxy).
      
      IF FIRST(op-template.op-template) THEN 
      DO:
         IF in-cont-code NE "" AND in-cont-code NE ? THEN 
         DO:
            FIND FIRST loan WHERE loan.contract  EQ in-contract
                        AND loan.cont-code EQ in-cont-code
                                           NO-LOCK NO-ERROR.

            /*8702 - sap происходит контроль за довложениями и изъятиями для вкладов*/
            method =  GetXattrValueEx("op-kind",op-kind.op-kind,"ВыпДоИт",?).
            IF method NE ? THEN 
            DO:
               IF SearchPFile(method)
                  THEN RUN VALUE(method + ".p") (
                     INPUT recid(loan),
                     INPUT c-date,
                     INPUT vContractDate,
                     OUTPUT mess).
                  ELSE RUN VALUE(method) IN h_ltran (
                     INPUT recid(loan),
                     INPUT c-date,
                     INPUT vContractDate,
                     OUTPUT mess).            

               IF mess NE "" THEN 
               DO:
                  RUN GetMesByCodeVal IN h_dps ("dps09",
                                                "%s=" + mess,
                                                OUTPUT fl-err).
                  IF fl-err LT 0 THEN
                     UNDO gen,LEAVE gen.
               END.
            END.

            in-templ =  Get_Op-templ(op-kind.op-kind,"loan-transaction",beg-templ).
            FIND FIRST xop-template OF op-kind WHERE xop-template.op-templ EQ in-templ
                                                                   NO-LOCK NO-ERROR.
            IF AVAIL xop-template THEN 
            DO:
               beg-templ = beg-templ + ',' + string(xop-template.op-template).
               IF Cr_loan_trans(BUFFER loan-transaction,
                                BUFFER loan,
                                RECID(xop-template),
                                vContractDate,
                                mess) EQ '-1' THEN 
               DO:
                  MESSAGE SKIP mess SKIP(1)
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  UNDO gen,LEAVE gen.
               END.
            END.
            IF AVAIL loan-transaction THEN 
            DO:
               loan-transaction.trans-code = TRANS_NUMBER(loan-transaction.contract,
                                                          loan-transaction.cont-code).
               RUN put-loan-trans IN hnd-proc (RECID(loan-transaction)) .
            END.
         END.
      END.

      /* Проверка на шаблон без создания объектов */
      IF GetXAttrValueEx("op-template",
                         op-template.op-kind + "," + STRING(op-template.op-template),
                         "PrsnTmpl",
                         "Нет") EQ "Нет" THEN
      DO:
      {g-frame.i &DoDisp = Yes &ChkBlockAction="undo GEN, leave GEN."}
      mforeq = op-templ.mfo-needed OR
               (GetXattrValueEx('op-template', op-kind.op-kind + ',' + string(op-templ.op-templ), 'МежБанк',?)
                 = 'Да').
      
      sset:
      DO on ERROR  UNDO, RETRY 
         on ENDKEY UNDO gen, RETRY gen:

         {g-frame.i &DoSet  = Yes}
         FIND FIRST loan WHERE loan.contract  EQ in-contract
                     AND loan.cont-code EQ in-cont-code
                                        NO-LOCK NO-ERROR.
         IF loan.end-date EQ ? THEN
            set_ost("ОстВклВ").
         ELSE
            set_ost("ОстВклС").
         IF op.op-status begins "А" THEN
            ASSIGN
               op.op-date       = ?
               op-entry.op-date = op.op-date
               .
         {op-entry.upd &871=yes}
         /* Создание ДР типа PARSEN_<КодРеквизита>*/
         {g-psigns.i}
         IF AVAIL op-entry THEN DO:
         if op-entry.acct-db BEGINS '42306' or
         op-entry.acct-db BEGINS '42305'or
         op-entry.acct-db BEGINS '42307' or
         op-entry.acct-db BEGINS '42605' or
         op-entry.acct-db BEGINS '42606' or
         op-entry.acct-db BEGINS '42607'
         then
         DO:
         MESSAGE "Неверный счет по дебету! Пополнение только со счетов 40817,40820,42301,42601" VIEW-AS ALERT-BOX ERROR.
        undo, RETRY.
         END.
            wop.amt-cur = IF op-entry.currency EQ "" THEN op-entry.amt-rub
                                                     ELSE op-entry.amt-cur.
         END.
         IF AVAIL op-entry  OR CAN-FIND(FIRST xxop WHERE xxop.op-transaction EQ op.op-transaction
                                                     AND RECID(xxop)         NE RECID(op))THEN 
         DO:
             IF mforeq AND NOT g-checkbank(vmfo, mbank-code-type,vcorr-acct, op.ben-acct, OUTPUT result, OUTPUT msg) THEN DO:
               IF op-templ.mfo-needed THEN DO:
                  MESSAGE SKIP msg SKIP(1) VIEW-AS ALERT-BOX ERROR BUTTONS ok.
                  CASE result:
                     WHEN {&egmissingmfo} OR WHEN {&egmissingbank} THEN NEXT-PROMPT vmfo.
                     WHEN {&egmissingcorracct} THEN NEXT-PROMPT vcorr-acct.
                     OTHERWISE NEXT-PROMPT op.ben-acct.
                  END CASE.
                  UNDO, RETRY.
               END.
               ELSE DO:
                  MESSAGE SKIP "Неверно заданы банковские реквизиты. Продолжить?" SKIP(1)
                  VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice AS LOGICAL.
                  IF choice <> TRUE THEN DO:
                     CASE result:
                        WHEN {&egmissingmfo} OR WHEN {&egmissingbank} THEN NEXT-PROMPT vmfo.
                        WHEN {&egmissingcorracct} THEN NEXT-PROMPT vcorr-acct.
                        OTHERWISE NEXT-PROMPT op.ben-acct.
                     END CASE.
                     UNDO, RETRY.
                  END.
               END.
            END.
            RUN "g-bank.p" (op-kind.op-kind, op-templ.op-templ, op.op, 4,OUTPUT fl-err).
            IF fl-err lt 0 THEN UNDO, RETRY.
            IF (vmfo <> "" AND vmfo <> ?) OR (vcorr-acct <> "" AND vcorr-acct <> ?) THEN DO:
               {opbnkcr.i op.op """" ""МФО-9"" vmfo vcorr-acct}
               {op-type.upd &check-format=yes}
            END.
            { op-type.chk }
            RUN post.
         end.

         /* Запомнить какой документы был предъявлен */
         {dps-exr.i 
             &SetAttrs  = YES
             &op_op     = op.op
         }
      END.
      END.
      ELSE
      DO:
            /* От формы требуется только ввода номера договора и даты */
         {wop-cr.i            
            &Err-ParsSumm   = "IF fler THEN                               ~
                                  UNDO gen, LEAVE gen."
            &NoParsDetails  = YES
         }
      END. /* PrsnTmpl */
   END.
   {optr.i &DoAfter=YES}
END.
{g-print1.i}

RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя.document-id").
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя.НаименованиеДокумента").
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя").
RUN DeleteOldDataProtocol IN h_base ("_CustType_").
RUN DeleteOldDataProtocol IN h_base ("_Proxy-code_"). 
RUN DeleteOldDataProtocol IN h_base ("ProxyPickVal").

HIDE FRAME opreq.
DELETE PROCEDURE(hProc).
RUN del-this-proc IN hnd-proc.
{plibdel.i}
delete procedure(loan_h) .
 
{intrface.del}
PROCEDURE post.
     RUN parssign.p (in-op-date,
                     "op-template",
                     op-kind.op-kind + "," + string(op-templ.op-templ),
                     op-templ.class-code,
                     "op",
                     STRING(op.op),
                     op.class-code,
                     RECID(wop)).

         {xttrentr.i &OP-BUF       = op
                     &OP-TEMPL-BUF = op-template}

         IF AVAIL op THEN DO:
            {additem.i str_recids STRING(RECID(op))}
         END.
         IF tcur = ? THEN tcur = op-entry.currency.
         ASSIGN
           wop.op-recid = RECID(op)
           wop.acct-db  = op-entry.acct-db
           wop.acct-cr  = op-entry.acct-cr
           wop.currency = op-entry.currency
           wop.amt-cur  = IF op-entry.currency <> "" THEN op-entry.amt-cur
                                                     ELSE op-entry.amt-rub
           wop.amt-rub  = op-entry.amt-rub
         .

END PROCEDURE.
/* $LINTFILE='g_doloan.p' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='kozv' */
/* $LINTDATE='06/04/2016 14:25:53.051+04:00' */
/*prosignLqExPF2hPxn5A/4VN9h9qw*/