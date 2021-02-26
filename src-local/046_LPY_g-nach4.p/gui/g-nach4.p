{globals.i}
{intrface.get tmess}

/* +++ g-nach4.p was humbly modified by (c)blodd converter v.1.09 on 1/8/2017 7:25pm +++ */

/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  g-nach4.p
      Comment:
         Uses:  -
      Used by:
      Created:  ??/??/?? ???
     Modified:  26/09/00 Om Исправление label,
     Modified:  разрешить ввод документа с плановой датой больше даты опер дня;
     Modified:  выдает предупреждение при повторном начислении процентов.
     Modified:  03/12/01 Kostik Заявка 0004600
     Modified: 13/11/2007 kraw (0083151) Темпоральные кассовые символы.
*/
form "~n@(#) g-nach4.p 1.0 ??? ??/??/?? Om 26/04/00"
with frame sccs-id stream-io width 250.

def input param in-op-date like op.op-date no-undo.
def input param oprid      as   recid      no-undo.

def var b-date      as   date                no-undo.
def var cred        as   date init ?         no-undo.
def var in-dop-acct like acct.acct           no-undo.
def var cod-ost     as   char                no-undo.
def var prn-tit     as   char format 'x(50)' no-undo.

def new global shared stream err.
def stream err1 .

def buffer xop-entry for op-entry .

&Scop Ofsrch 0 .
&Scop DoContract 0 .
&Scop Col-lab 'ПЛАНОВАЯ ДАТА НАЧИСЛЕНИЯ'

{g-defs.i}
{chktempl.i}
{chk-stat.i &NOT_CLOSE = YES}
{dpsproc.def}
{def-wf.i new}
{savewop.i}
{intrface.get dps}
{defframe.i new}
{wdep.def new}
{g-assdps.def} /* "короткий ввод" номера вклада */

def var dval like op-entry.value-date no-undo .
def var fler as logical no-undo .
def var in-contract like loan.contract no-undo .
def var in-cont-code like loan.cont-code no-undo .

def buffer xwop for wop.
def buffer xxop-entry for op-entry.

def var loan_h  as handle no-undo.
def var xtr_h   as handle no-undo.
def var k as INT64 no-undo.

def var rid1 as recid extent 5 no-undo .

def stream doc .

def var str-kau as char no-undo .
def var s-rub like op-entry.amt-rub no-undo .
def var s-cur like op-entry.amt-cur no-undo .
DEF VAR tmp-recid AS CHAR INITIAL "".
DEF VAR tt-cont-code AS CHAR NO-UNDO. /* короткий номер вклада */

{g-currv1.i 
   &OFbase = "/*"
   &ByRole = YES
}

{details.def}

Function Set_type     returns logical (input l-type as char) in loan_h .
Function Set_ost      returns logical (input cod-ost as char) in loan_h .
Function Set_period   returns logical (input dat1 as date,input dat2 as date) in loan_h .

run "l-type.p" persistent set loan_h .
{plibinit.i}
ASSIGN
    DebugParser = INT64(GetXattrValueEx('op-kind', op-kind.op-kind, 'debugparser', '0'))
    cur-op-date = in-op-date
    prn-tit = "Ведомость начисленных процентов  на " + string(in-op-date)
.


cred = in-op-date .

{prn-ved.i
   &DefTempTable       = "Объявляем временную таблицу"
   &EmptyTempTable     = "Очищаем табличку"
   &DefPrintProcedures = "Объявляем процедуры печати"
   &Stream             = " stream err"
   &filename           = _spool1.tmp
   &PutPlanDate        = cred
}


TR:
DO TRANS WITH FRAME opreq ON ENDKEY UNDO tr, LEAVE tr
                          ON ERROR  UNDO tr, LEAVE tr:

form
perfdep.person-id  colon  14 skip
perfdep.name-last  colon  14 skip
perfdep.first-name colon  14                perfdep.gender     colon  59
perfdep.birthday   colon  14                perfdep.country-id  colon  59
perfdep.document   colon  14                perfdep.document-id label "Тип док" colon 59
perfdep.address[1] colon  14 label "Адрес"  perfdep.phone[1]   colon  59 label "Телефон"
perfdep.address[2] colon  14 label "     "  perfdep.phone[2]   colon  59 label "       " skip
"══════════════════════════════════════════════════════════════════════════════"
in-cont-code     colon  14 label "Номер вклада" format "x(30)"
in-dop-acct        colon 23
                   label "Счет до востребования"
                   help "счет до востребования"
cred     label 'Плановая дата'
"══════════════════════════════════════════════════════════════════════════════"
op.doc-num colon 14 op.doc-type doc-type.name NO-LABEL FORMAT "x(30)"
op-entry.amt-cur label "Сумма %%" colon 14 op-entry.currency op-entry.symbol skip
op-entry.acct-db colon 14
op-entry.acct-cr colon 14
op.details       colon 14 VIEW-AS EDITOR INNER-CHARS 60 INNER-LINES 3
with frame opreq 1 down overlay centered side-labels row 3
      title color bright-white
  "[ ОПЕРАЦИЯ : " + op-kind.name + " ЗА " + string(in-op-date, "99/99/99") + " ]".


HIDE in-dop-acct.
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


FOR EACH wop WHERE wop.op-kind EQ op-kind.op-kind:
   DELETE wop.
END.

ON LEAVE OF op-entry.acct-db IN FRAME opreq DO:
   RUN transmes.p(STRING(INPUT op-entry.acct-db),
                  STRING(INPUT op-entry.currency),
                  op-kind.op-kind,
                  "ДЕБЕТ",
                  INPUT-OUTPUT mClMessList).
END.

ON LEAVE OF op-entry.acct-cr IN FRAME opreq DO:
   RUN transmes.p(STRING(INPUT op-entry.acct-db),
                  STRING(INPUT op-entry.currency),
                  op-kind.op-kind,
                  "КРЕДИТ",
                  INPUT-OUTPUT mClMessList).
END.


on  leave , go of in-cont-code in frame opreq do :
   DO WITH FRAME opreq : 
      IF LOOKUP(THIS-PROCEDURE:FILE-NAME,PROGRAM-NAME(2)," ") EQ 0 THEN RETURN.   
      /* короткий ввод номера вклада */                                        
      tt-cont-code = INPUT in-cont-code.
      RUN AssumeContCode(INPUT-OUTPUT tt-cont-code).
      
      IF tt-cont-code EQ ? THEN
         {return_no_apply.i}
      
      IF INPUT in-cont-code <> tt-cont-code THEN
           DISPLAY tt-cont-code @ in-cont-code.                   
      
      in-cont-code = FmtMskAddSuffix(INPUT INPUT in-cont-code,"cont-code").
      

      FIND FIRST loan WHERE loan.contract  EQ "DPS"
                        AND loan.cont-code EQ in-cont-code
                        AND (loan.close-date > in-op-date
                         OR loan.close-date EQ ?)
                                             NO-LOCK NO-ERROR.
      IF AVAIL loan THEN DO:
         IF loan.open-date >in-op-date THEN DO:
            RUN Fill-AlertSysMes IN h_tmess("","",-1,"Договор еще не открыт!").

            {return_no_apply.i}
         END.
         
         RUN CreateFrmFields (?,"LoanRecid","",STRING(RECID(loan))).

         ASSIGN
            in-contract  = loan.contract
            in-cont-code = loan.cont-code
            .
      
         FIND person WHERE person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
         
         IF AVAIL person THEN
            DISP
               person.person-id   @ perfdep.person-id
               person.name-last   @ perfdep.name-last
               person.first-name  @ perfdep.first-name
               person.gender      @ perfdep.gender
               person.birthday    @ perfdep.birthday
               person.country-id  @ perfdep.country-id
               person.document-id @ perfdep.document-id
               person.document    @ perfdep.document
               person.address[1]  @ perfdep.address[1]
               person.address[2]  @ perfdep.address[2]
               person.phone[1]    @ perfdep.phone[1]
               person.phone[2]    @ perfdep.phone[2]
            .
      END.
      ELSE DO:
         RUN Fill-AlertSysMes IN h_tmess("","",-1,'Нет такого вклада').

         {return_no_apply.i}
      END.
   end.
end.

ON any-key OF op.doc-type DO:
  APPLY {&LAST_KEY}.
   DO WITH FRAME opreq :
           FIND doc-type WHERE doc-type.doc-type EQ INPUT op.doc-type NO-LOCK NO-ERROR.
           DISPLAY
              doc-type.name WHEN AVAIL doc-type     @ doc-type.name
              ""            WHEN NOT AVAIL doc-type @ doc-type.name
           .
   END.
  {return_no_apply.i}
END.
ON "F1" of frame opreq ANYWHERE DO:
   DEF VAR field-fr AS CHAR NO-UNDO .
   field-fr = {&FRAME_FIELD} .
   DO WITH FRAME opreq :
      CASE {&FRAME_FIELD}:
         WHEN 'doc-type' THEN RUN browseld.p ("doc-type","","","",4).
         WHEN 'acct-cr'  THEN RUN acct(op-template.acct-cat,4) .
         WHEN 'acct-db'  THEN RUN acct(op-template.acct-cat,4) .
         WHEN 'currency' THEN RUN currency.p('currency',4).
         WHEN 'amt-cur'  THEN RUN calc.p(4) .
         WHEN 'symbol'   THEN RUN browseld.p("КасСимволы", "class", "КасСимволы", "", 4).
         WHEN 'in-cont-code' THEN RUN dpsdispc.p('dps',?,'Ч',?,4).
         OTHERWISE RETURN .
      END CASE .
      IF ({&LAST_EVENT_FUNCTION} = "GO" OR {&LAST_EVENT_FUNCTION} = "RETURN") AND pick-value <> ? THEN do :
         CASE field-fr:
            WHEN "doc-type" THEN DISPLAY pick-value @ op.doc-type.
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

disp in-cont-code cred .
{set.i &THIS_FRAME = "opreq" &EXFILE = "g-nach4.p.st1" {&*}}.
{for_ved.i &filename=_spool1.tmp}

IF AVAIL person THEN
   RUN pGetMessPerson IN h_dps (op-kind.op-kind,
                                person.person-id).

in-cont-code = FmtMskAddSuffix(in-cont-code,"cont-code").

IF NOT chk_loan_stat(in-cont-code,'Ф',YES) THEN UNDO tr,RETRY tr.
Set_type(loan.cont-code) .

CR_OP:
FOR EACH op-templ OF op-kind NO-LOCK ON ERROR  UNDO tr,LEAVE tr
                                     ON ENDKEY UNDO tr,LEAVE tr
                                     WITH FRAME opreq:
 RUN RestoreWop.
 CREATE op.
    {op(sess).cr}
    {g-op.ass}
    op.contract-date = cred .
 CREATE op-entry.

   {g-en.ass &ind="OP-TEMPLATE.OP-TEMPLATE + 1"}

 ASSIGN
   op-entry.value-date = in-op-date
   op-entry.currency   = IF op-templ.currency NE ? THEN GetCurr(op-templ.currency)
                                                   ELSE loan.currency.
   tcur = op-entry.currency.
  
  &IF DEFINED(oracle) &THEN
    VALIDATE op NO-ERROR.
    VALIDATE op-entry NO-ERROR.
  &ENDIF

 CREATE wop .
 ASSIGN
   wop.currency     = op-entry.currency
   dval             = op-entry.value-date.
 ASSIGN            
   op.contract-date = IF cred EQ ? THEN in-op-date
                              ELSE cred
   wop.currency     = op-entry.currency
   dval             = op-entry.value-date
   wop.op-recid     = recid(op-entry)
   wop.con-date     = op.contract-date
   wop.op-kind      = op-kind.op-kind
   wop.op-templ     = op-templ.op-templ
 .


   {g-acctv1.i &OFbase=Yes
               &BYrole=yes
               &vacct=tacct
   }
   
   tacct-cr = FmtMskAddSuffix(tacct-cr,"acct").
   tacct-db = FmtMskAddSuffix(tacct-db,"acct").

 IF (op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "")
 OR (op-templ.prep-amt-natcur <> ? AND op-templ.prep-amt-natcur <> "") THEN DO:
    {asswop.i}
    cod-ost = get-ost-templ(op-kind.op-kind,op-template.op-template).
    IF cod-ost NE ? AND cod-ost NE "" THEN
    Set_ost(cod-ost).
    RUN parssen.p (RECID(wop), in-op-date, OUTPUT fler).
    IF fler THEN UNDO tr, LEAVE tr.
    IF wop.amt-rub EQ 0 OR wop.amt-cur EQ 0 THEN DO:
       RUN SaveWop.
       UNDO cr_op, NEXT cr_op.
    END.
    ASSIGN
      op-entry.amt-rub = wop.amt-rub
      op-entry.amt-cur = IF op-entry.currency <> ""  THEN wop.amt-cur
                                                     ELSE op-entry.amt-cur
      op-entry.user-id = userid('bisquit')
      op-entry.acct-cr = tacct-cr
      op-entry.acct-db = tacct-db
    .
    FIND doc-type OF op NO-LOCK NO-ERROR.
    DISP
        doc-type.name WHEN AVAIL doc-type     @ doc-type.name
        ""            WHEN NOT AVAIL doc-type @ doc-type.name
        op.doc-num
        op.doc-type
        op-entry.acct-db   @ op-entry.acct-db
        op-entry.acct-cr   @ op-entry.acct-cr
        op-entry.amt-rub WHEN op-entry.currency EQ "" @ op-entry.amt-cur
        op-entry.amt-cur WHEN op-entry.currency NE "" @ op-entry.amt-cur
        op-template.currency  @ op-entry.currency
        op-template.symbol    @ op-entry.symbol
        op.details
      .

    {set.i &THIS_FRAME = "opreq" &EXFILE = "g-nach4.p.st2" &NODISPLAY = "YES" {&*}}. 
   IF NOT CAN-FIND(FIRST acct WHERE acct.acct EQ op-entry.acct-db) 
   THEN DO:
      RUN Fill-Sysmes IN h_tmess ("","","0","Счет по дебету не найден."). BELL.
      UNDO ,RETRY.
   END.
   IF NOT CAN-FIND(FIRST acct WHERE acct.acct EQ op-entry.acct-cr) 
   THEN DO:
      RUN Fill-Sysmes IN h_tmess ("","","0","Счет по кредиту не найден."). BELL.
      UNDO ,RETRY.
   END.

   op-entry.amt-cur  = if op-entry.curr > "" then op-entry.amt-cur
                       else 0.

   ASSIGN
      tacct-db = op-entry.acct-db
      tacct-cr = op-entry.acct-cr
      tcur     = op-entry.currency
   .

   {asswop.i}

   ASSIGN
      wop.currency = op-entry.currency
      wop.amt-cur = if op-entry.currency eq "" THEN op-entry.amt-rub
                                               ELSE op-entry.amt-cur
   .
   RUN ProcessDetails (RECID(wop), INPUT-OUTPUT op.details).


   IF op-entry.amt-rub NE 0 OR op-entry.amt-cur NE 0 THEN DO:
      {op-entry.upd &871=yes &Ofnext="/*" &open-date="undo tr,next tr"
          &open-undo="undo tr,leave tr" 
      }
      /* Создание ДР типа PARSEN_<КодРеквизита>*/
      {g-psigns.i}
   END.
   ELSE DO:
      DELETE op-entry .
      NEXT.
   END.
 END.
END. /* FOR */
  /*создание реквизитов на счетах*/
  {cdealend.i 
   &mt = "UNDO tr, LEAVE tr. "
   &p = "n"
  }

  {prn-ved.i
     &RunReportPrint  = "Печатаем ведомость"}

END. /* TR */

{preview2.i &stream="stream err" &filename=_spool1.tmp}
{g-print1.i}

HIDE FRAME opreq NO-PAUSE.
DELETE PROCEDURE loan_h.
{plibdel.i}
 

/* Комманда ОС после выполнения транзакции */
{cmd-exec.i
    &cmd        = "'Postcmd'"
} 
{intrface.del}          /* Выгрузка инструментария. */ 

return.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='06/03/2015 11:18:04.021+04:00' */
/* $LINTUSER='BIS' */
/* $LINTMODE='1' */
/* $LINTFILE='g-nach4.p' */
/*prosignSr1MIZpoaVGBSRDQ7W6OhA*/
/* --- g-nach4.p was humbly modified by (c)blodd converter v.1.09 on 1/8/2017 7:25pm --- */
