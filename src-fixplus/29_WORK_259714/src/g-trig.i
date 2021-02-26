/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: G-TRIG.I
      Comment: обработка ввода для процедуры g-midl?.p g-cash?.p
   Parameters:
         Uses:
      Used by:
      Created: ...
      Modifed: 30.12.98   by Om  Проверка пересчета договора на дату опердня
      Modifed: 23/03/2001 by Om  Вызов картатеки получателей при незаполненном БИКе.
      Modifed: 17/10/2001 by Kraw Откат details при отказе от выбора (либо в исходное, либо в "пусто")
      Modifed: 30/10/2001 by NIK  Для контроля кода банка использован включаемый файл g-bank.lv
                                  Для контроля корсчета банка использован включаемый файл g-corr.lv
                                  Выполнена макрозамена для f-mfo/vmfo и f-corr-acct/vcorr-acct
      Modifed: 08/12/2001 shin Индикация потокового ввода + возможность динамически его изменить
      Modifed: 14/01/2002 by NIK  Добавлен контроль банка при выходе из формы
      Modifed: 01/02/2002 kraw добавлена подготовка начальных значений реквизитов Адрес_пок и Телеф_пок
     Modified: 30.04.2002 16:16 SEMA     по заявке 0006062 Механизм фильтров в допреквизитах шаблонов транзакций
     Modified: 03/06/02 shin - 7783
     modified: 29/06/2002 kostik 0008260 Срабатывала проверка межбанковских
                                         реквизитов после выбора вклада. (Операция донесение процедура g_doloan.p)
     Modified: 05.08.2002 18:34 SEMA     по заявке 0009243 изменен порядок запуска парсера и валидации полей
     Modified: 11.11.2002 15:26 SEMA     по заявке 0011932 код, использовавший таблицу frm-fields вынесен в frmfield.fun
     Modified: 19.11.2002 19:22 SEMA     по заявке 0010950 добавлен триггер на F1 в поле содержания, изменен алгоритм
                                         работы триггеров по выходу из amt-rub amt-cur ben-acct, изменен алгоритм для F6
     Modified: 21.11.2002 18:17 SEMA     по заявке 0010950 исправление ошибки
     Modified: 22.11.2002 20:45 rija     1734
     Modified: 10.12.2002 15:28 kolal    Добавлен контроль ввода даты (doc-date-leave-post) и кассового символа (symbol-leave-post).
                                         Заявка 11646.
     Modified: 26.12.2002 Gunk           Доработка ввода назначения платежа
     Modified: 09.01.2003 15:25 SEMA     по заявке 0010950 вставлена проверка benacct при выходе по ctrl-enter
     Modified: 22.01.2003 13:25 SEMA     по заявке 0010950 исправлена ошибка связанная с проверкой benacct
     Modified: 06.02.2003 Gukn           Обработка ситуации с несколькими получателями
     Modified: 14.02.2003 14:17 kolal    Доработана процедура symbol-leave-post:
                                         контроль ввода КС контролируется
                                         параметром "КонтрольКС". Доработана
                                         процедура doc-date-leave-post: для
                                         контроль даты вызывается проц-ра
                                         проверки из метасхемы. Заявка 11646.
     Modified: 29.04.2003 17:29 SEMA     по заявке 0015363 подключение вызова формы редактирования налоговых реквизитов
     Modified: 05.06.2003 15:13 kolal    15363
     Modified: 10/04/2004  - Добавлен параметр &mod-card
     Modified: 18.05.2004 17:36 sadm     27825 Переделан алгоритм начального заполнения полей Адрес_пок и Телеф_пок
     Modified: 29.06.2004 abko 0024451 изменен выбор счета по F1
     Modified: 20.01.2005 kraw (0026950) проверка кода валюты в ben-acct для рублевых документов
     Modified: 28.09.2006 15:03 OZMI     (0068459).    
     Modified: 31.01.2007 kraw (0056721) В OnGo_Frame.opreq обход всех виджетов с вызовом CreateFrmFields
     Modified: 05.03.2007 kraw (0074304) Контроль кассового символа (испралена ошибка)
     Modified: 01.07.2007 15:44 KSV      (0078824) Адаптирован для Биссмарт
     Modified: 16.10.2007 kraw (0083291) Принудительный пересчет рублевого эквивалента при GO на валютном документе
     Modified: 16.10.2007 kraw (0085036) Вопрос при необходимости пересчета рублевого эквивалента при GO на валютном документе
     Modified: 16.10.2007 kraw (0094099) То же самое, что и 0085036, но с учетом округления
     Modified: 11/03/2009 kraw (0102606) Если на doc-templ установить значение ДР "NoDeleteRS" в "Да", ben-acct не обнуляется.
     Modified: 09/09/2009 kraw (0113623) Проверка наличия блокированной суммы при выходе из поля номера счета
     Modified: 29/09/2009 19:00 SOLM (0126593 ) Исправлена ошибка в QBIS - выход по F4
     Modified: 13/02/2014 sasa (0185756) Добавлен контроль ввода нулевой суммы в QBIS
*/
{g-error.def}

/* реакция на [ESC] */
&GLOB pass-seq "doc-num,acct-db,acct-cr,amt-cur,amt-rub"

/* объявляем уровень основного фрейма (по аналогии с g-frame.i) */
&IF DEFINED(ROW) = 0 &THEN
   &SCOPED-DEFINE ROW 2
&ENDIF

/* DETAILS_DEF-NO_QUESTION_MARK для того, чтобы ProcessDetails
     вместо неопределенного значения или строки, состоящей из единственного символа "?"
     возвращал пустую строку*/
&SCOPED-DEFINE DETAILS_DEF-NO_QUESTION_MARK 1

&IF DEFINED(f) ne 0 &THEN
   &SCOPED-DEFINE SEL-MFO  f-mfo
   &SCOPED-DEFINE SEL-CORR f-corr-acct
&ELSE
   &SCOPED-DEFINE SEL-MFO  vmfo
   &SCOPED-DEFINE SEL-CORR vcorr-acct
&ENDIF

run SetSysConf in h_base("g-trig - Адрес_пок", ?) no-error.
run SetSysConf in h_base("g-trig - Телеф_пок", ?) no-error.

DEFINE VARIABLE vOK AS LOGICAL NO-UNDO.
DEFINE VARIABLE g_trig_i_is_recount AS LOGICAL NO-UNDO.
DEFINE VARIABLE g_trig_i_no_parse   AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE vOrder-PayLst AS CHARACTER NO-UNDO.
{details.def}
{ch_cart.i}
{intrface.get xclass}
{intrface.get op}
{intrface.get cust}
{intrface.get acct}
{intrface.get db2l}
{innchk.i}

&IF DEFINED(likeDiasoft) &THEN
DEF VAR basta AS LOGICAL INIT no NO-UNDO.
DEF VAR screen-value AS CHAR INIT "" NO-UNDO.

ON ENTRY OF FRAME opreq DO:
  {realtrig.i &custom="OnEntry_Frame.opreq"}
END.

ON "END-ERROR" OF FRAME opreq DO:
  &IF DEFINED(SESSION-REMOTE) &THEN
    IF {&LAST_KEY} = KEYCODE("F4") THEN RETURN.
  &ENDIF
  {realtrig.i &custom="OnEnd-Error_Frame.opreq"}
END.
&ENDIF

ON RETURN,GO OF op.details IN FRAME opreq DO:
  {realtrig.i &name="function"}
END.

ON "F3", "F4", "F5" OF op.details IN FRAME opreq DO:
  {realtrig.i &name="label"}
END.

&IF DEFINED(NOmfo) = 0 &THEN
ON "F6" of frame opreq ANYWHERE DO:
  {realtrig.i &custom="OnF6_Frame.opreq"}
END.
&ENDIF

&IF DEFINED(cycle) ne 0 &THEN
  ON "F7" of frame opreq ANYWHERE DO:
    if is_cycle then do:
      message "Прекратить потоковый ввод?"
      view-as alert-box question buttons Yes-No update g_Ok as log.
      if g_Ok then is_cycle = no.
    end.
    else do:
      message "Начать потоковый ввод?"
      view-as alert-box question buttons Yes-No update g_Ok.
      if g_Ok then is_cycle = yes.
      {g-cycle.dsp}
    end.
  END.
&ENDIF

&IF DEFINED(SESSION-REMOTE) &THEN
ON F12 OF op.doc-type IN FRAME opreq DO:
  {realtrig.i &custom="OnSpace_Op.Doc-Type"}
END.
&ELSE
ON " " OF op.doc-type IN FRAME opreq DO:
  {realtrig.i &custom="OnSpace_Op.Doc-Type"}
END.
&ENDIF

&IF DEFINED(browse-entry) = 0 &THEN
ON ENTRY OF op-entry.currency IN FRAME opreq DO:
  {realtrig.i &name="function"}
END.

&IF DEFINED(NOcurr) = 0 &THEN
ON LEAVE OF op-entry.amt-cur IN FRAME opreq DO:
  run OnLeaveAssignBuff.
  {realtrig.i &name="function"}
END.

ON LEAVE OF op-entry.currency IN FRAME opreq DO:
  run OnLeaveAssignBuff.
  {realtrig.i &name="function"}
END.
&ENDIF

&IF DEFINED(NOtamt) &THEN
ON LEAVE OF op-entry.amt-rub IN FRAME opreq DO:
  run OnLeaveAssignBuff.
  {realtrig.i &name="function"}
END.
&ENDIF

ON LEAVE OF 
&IF DEFINED(g_cash1_sym) <> 0 &THEN
   mSymbol
&ELSE
   op-entry.symbol
&ENDIF
IN FRAME opreq
DO:
   RUN OnLeaveAssignBuff.
   {realtrig.i &name = "function"}
END.

&ENDIF

&IF DEFINED(g_cash1_sym) <> 0 &THEN
ON F1 OF mSymbol IN FRAME opreq
DO:
   RUN browseld.p("КасСимволы", "class~001beg-date2", "КасСимволы~001" + STRING(op.op-date), "", 7).
   IF    (   LASTKEY EQ 13
          OR LASTKEY EQ 10 )
      AND pick-value NE ?    THEN
      mSymbol:SCREEN-VALUE = pick-value. 
END.
&ENDIF

&IF DEFINED(NOmfo) = 0 &THEN
ON ANY-PRINTABLE OF vmfo IN FRAME opreq DO:
  {realtrig.i &custom="OnAny-Printable_.Vmfo"}
END.
&ENDIF

&IF DEFINED(browse-entry) = 0 &THEN
ON ENTRY OF op-entry.amt-cur IN FRAME opreq DO:
  {realtrig.i &custom="OnEntry_Op-entry.Amt-Cur"}
END.
&ENDIF

ON ENTRY OF op.details IN FRAME opreq DO:
  {realtrig.i &name="function" &no_undef=Yes}
END.

procedure find-next-widget:
  def input param in-widget-name as char no-undo.
  def output param next-widget as widget-handle no-undo.
  def var wh as widget-handle no-undo.
  def var i as INT64 no-undo.
  def var cur as INT64 no-undo.
  def var v-pass-seq as char no-undo. /* список полей */
  DEFINE  VARIABLE first-widget-list AS CHARACTER NO-UNDO.

  assign
    v-pass-seq = FGetSettingEx("СтандТр","СписокПолей",{&pass-seq},no)
    next-widget = ?
  .
  if v-pass-seq = "" then v-pass-seq = {&pass-seq}.
  cur = lookup(in-widget-name, v-pass-seq).

  ASSIGN
     wh  = FRAME opreq:FIRST-CHILD.
     wh  = IF VALID-HANDLE(wh) THEN wh:FIRST-CHILD ELSE ?
  .
  DO WHILE VALID-HANDLE(wh)
     AND wh:NAME NE ENTRY(1,v-pass-seq):
     IF wh:NAME <> ?
        AND wh:SENSITIVE THEN
     DO:
        {additem.i first-widget-list wh:name}
     END.
     wh = wh:NEXT-SIBLING.
  END.
  IF cur = 0
     AND NOT CAN-DO(first-widget-list,in-widget-name) THEN
     RETURN.

main:
  do i = cur + 1 to num-entries(v-pass-seq):

    assign
      wh  = frame opreq:first-child.
      wh  = if valid-handle(wh) then wh:first-child else ?
    .

    do while valid-handle(wh):
      if wh:name = entry(i, v-pass-seq) then do:
        if wh:sensitive then do:
          next-widget = wh.
          leave main.
        end.
        else
          leave.
      end.
      wh = wh:next-sibling.
    end.
  end.
end procedure.

ON LEAVE of frame opreq ANYWHERE DO:
&IF DEFINED(likeDiasoft) &THEN
   DEF VAR lbl AS CHAR NO-UNDO.
   DEF VAR next-widget AS WIDGET-HANDLE NO-UNDO.
   IF LAST-EVENT:EVENT-TYPE = "KEYPRESS" THEN lbl = LAST-EVENT:LABEL.
&ENDIF
   IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN RETURN "LEAVE".
   {realtrig.i &noevent="leave"}
   IF LOOKUP(self:name + '-leave-post', THIS-PROCEDURE:INTERNAL-ENTRIES) <> 0 THEN DO:
      RUN VALUE(self:name + '-leave-post') IN THIS-PROCEDURE.
      IF RETURN-VALUE = "POST-ERROR" THEN RETURN NO-APPLY.
   END.
run OnLeaveAssignBuff.
&IF DEFINED(likeDiasoft) &THEN
   IF lbl = "ENTER" THEN DO:
      RUN find-next-widget (SELF:NAME, OUTPUT next-widget).
      IF VALID-HANDLE(next-widget) THEN DO:
         APPLY "ENTRY" TO next-widget.
         RETURN NO-APPLY.
      END.
   END.
&ENDIF
END.

ON "F1" of frame opreq ANYWHERE DO:
  {realtrig.i &name="label"}
END.

&IF DEFINED(browse-entry) = 0 &THEN
ON " " OF op-entry.prev-year IN FRAME opreq DO:
  frame-value = (if frame-value = "" then "ЗО" else "").
  RETURN NO-APPLY.
END.
&ENDIF

&IF DEFINED(NOmfo) = 0 &THEN

&ENDIF

&IF DEFINED(browse-entry) = 0 &THEN
   DO:
      {g-assacc.def &currency="op-entry.currency"}
      {g-assdps.def}
   END.
&ENDIF

{ opdetail.i OpReq }
/* real triggers */
/* ^^^^          */
PROCEDURE OnEntry_Op.Details:
  DEF VAR t-details LIKE op.details NO-UNDO.
&IF DEFINED(browse-entry) = 0 &THEN
   &IF DEFINED(g_cash1_sym) <> 0 &THEN
     naimks = mSymbol:SCREEN-VALUE IN FRAME opreq.
   &ELSE
     naimks = op-entry.symbol:SCREEN-VALUE IN FRAME opreq.
   &ENDIF
&ENDIF
  if lookup("ProcessDetails", this-procedure:internal-entries) > 0 then do:
    assign
      t-details = &IF DEFINED(ie) = 0 &THEN SELF:SCREEN-VALUE &ELSE op.det &ENDIF .
    run ProcessDetails (if avail wop then recid(wop) else ?, input-output t-details).
   &IF DEFINED(ie) = 0 &THEN SELF:SCREEN-VALUE &ELSE op.det &ENDIF = t-details.
    IF LAST-EVENT:FUNCTION <> "GO" THEN
       return.
  end.

  IF SEARCH("g-detail.r") <> ? OR SEARCH("g-detail.p") <> ? THEN DO:
    t-details = SELF:SCREEN-VALUE.
    RUN g-detail.p (op-kind.op-kind, op-templ.op-templ, recid(op), FRAME opreq:handle, INPUT-OUTPUT t-details).
    IF RETURN-VALUE = "OK" THEN DO:
      SELF:SCREEN-VALUE = t-details.
    END.
  END.
END PROCEDURE.

PROCEDURE OnReturn_Op.Details:
  DO WITH FRAME opreq:
    IF SELF:CURSOR-CHAR = 0 AND SELF:CURSOR-LINE >= 3 THEN APPLY "GO" TO SELF.
    ELSE SELF:INSERT-STRING("~n").
  END.
END PROCEDURE.

PROCEDURE OnF3_Op.Details:
   IF AVAIL xop
      THEN FIND PREV xop WHERE xop.user-id  = USERID("bisquit")
                           AND xop.op-date <> ?
                           AND xop.op-date <= in-op-date
                           AND xop.op       < op.op
              USE-INDEX op NO-LOCK NO-ERROR.
      ELSE FIND LAST xop WHERE xop.user-id  = USERID("bisquit")
                           AND xop.op-date <> ?
                           AND xop.op-date <= in-op-date
                           AND xop.op       < op.op
              USE-INDEX op NO-LOCK NO-ERROR.
   IF AVAIL xop THEN DO:
      op.details = xop.details.
      DISPLAY op.details WITH FRAME opreq.
   END.
   RETURN "NO-APPLY".
END PROCEDURE.

PROCEDURE OnF4_Op.Details:
   IF AVAIL xop
      THEN FIND NEXT xop WHERE xop.user-id  = USERID("bisquit")
                           AND xop.op-date <> ?
                           AND xop.op-date <= in-op-date
                           AND xop.op       < op.op
              USE-INDEX op NO-LOCK NO-ERROR.
      ELSE BELL.
   IF AVAIL xop THEN DO:
      op.details = xop.details.
      DISPLAY op.details WITH FRAME opreq.
   END.
   RETURN "NO-APPLY".
END PROCEDURE.

PROCEDURE OnF5_Op.Details:
  DEF VAR op-details AS CHAR NO-UNDO.

  DO WITH FRAME opreq:
    op-details = INPUT op.details.
    RUN insrtop.p (INPUT-OUTPUT op-details, RECID(op)).
    op.details = op-details.
    DISPLAY op.details.
    RETURN "NO-APPLY".
  END.
END PROCEDURE.

&IF DEFINED(NOmfo) = 0 &THEN
PROCEDURE OnF6_Frame.opreq:
   IF FRAME-NAME = "opreq" THEN DO:

      IF GetXAttrValueEx ('op-kind',
                          op-kind.op-kind,
                          'SendRec-Switch',
                          "Да") = "Да"
      THEN DO:
         MESSAGE
            'Переключить банк плательщика <-> банк получателя?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE opreg_choice AS LOG.
         IF opreg_choice <> TRUE THEN
            RETURN "NO-APPLY".

         IF op.doc-kind = "rec" THEN
            ASSIGN
               op.doc-kind = "send"
               doc-kind    = {&send-label}
               .
         ELSE
            ASSIGN
               op.doc-kind = "rec"
               doc-kind    = {&rec-label}
               .
         DISPLAY doc-kind WITH FRAME opreq.
         RETURN "NO-APPLY".
      END.
   END.
END PROCEDURE.
&ENDIF

&IF DEFINED(browse-entry) = 0 &THEN
PROCEDURE OnEntry_Op-Entry.Currency:
  IF (LAST-EVENT:CODE = 13 OR LAST-EVENT:CODE = 10) AND op-entry.currency <> ? THEN RETURN "NO-APPLY".
END PROCEDURE.

PROCEDURE OnEntry_Op-Entry.Amt-Cur:
  IF (LAST-EVENT:CODE = 13 OR LAST-EVENT:CODE = 10) AND op-entry.currency = "" THEN RETURN "NO-APPLY".
END PROCEDURE.

&IF DEFINED(NOcurr) = 0 &THEN
PROCEDURE OnLeave_Op-Entry.Amt-Cur:
   DEFINE BUFFER bufAcct FOR Acct.
   IF NOT AVAILABLE op-entry THEN RETURN.
   IF (INPUT FRAME opreq op-entry.amt-cur EQ ? OR
       INPUT FRAME opreq op-entry.amt-cur EQ 0)
   AND CAN-FIND (FIRST bufAcct WHERE
                       bufAcct.filial-id EQ dept.branch
                   AND bufAcct.number    EQ INPUT FRAME opreq op-entry.acct-db
                   AND bufAcct.Curr      NE "" NO-LOCK)
   AND CAN-FIND (FIRST bufAcct WHERE
                       bufAcct.filial-id EQ dept.branch
                   AND bufAcct.Acct      EQ INPUT FRAME opreq op-entry.acct-cr
                   AND bufAcct.Curr      NE "" NO-LOCK) THEN
   DO:
      {message1
         &TEXT    = "|Не указана валютная сумма|валютного документа||Продолжить?"
         &BUTTONS = YES-NO
      }
      IF pick-value EQ "NO" THEN RETURN "NO-APPLY".
   END.

   IF CAN-DO ("0,1,3",
              GetXAttrValueEx ('op-template',
                               op-kind.op-kind + "," + STRING(op-templ.op-templ),
                               'rub-cur',
                               "0"))
   THEN DO:
      IF op-entry.currency <> "" AND
         INPUT FRAME opreq op-entry.amt-cur <> ? AND
         INPUT FRAME opreq op-entry.amt-cur <> 0
      THEN DO WITH FRAME opreq: /* делаем пересчет поля "сумма в рублях" из поля "сумма в валюте" */
         FIND LAST instr-rate WHERE instr-rate.instr-cat = "currency" AND
                                    instr-rate.instr-code = INPUT op-entry.currency AND
                                    instr-rate.rate-type = "УЧЕТНЫЙ" AND
                                    instr-rate.since <= INPUT op-entry.value-date NO-LOCK NO-ERROR.
         IF AVAIL instr-rate THEN DO:
            IF instr-rate.rate-instr ne ? THEN DO:
               vOK = NO.

               IF g_trig_i_is_recount THEN
               DO:

                  IF DECIMAL(STRING(INPUT op-entry.amt-cur * instr-rate.rate-instr / instr-rate.per, op-entry.amt-rub:FORMAT)) NE INPUT op-entry.amt-rub 
                  THEN
                     MESSAGE "Рублевая сумма не соответствует рублевому эквиваленту по курсу ЦБ." SKIP
                     "Пересчитать?"
                     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vOK AS LOGICAL.
               END.

               IF vOK OR NOT g_trig_i_is_recount THEN
                  DISPLAY INPUT op-entry.amt-cur * instr-rate.rate-instr / instr-rate.per @ op-entry.amt-rub WITH FRAME opreq.

               IF tamt = 0 THEN tamt = INPUT op-entry.amt-cur.
            END.
            ELSE DO:
               &IF DEFINED(SESSION-REMOTE) &THEN
               MESSAGE {&EGMBadInstrRate} VIEW-AS ALERT-BOX.
               &ELSE
               MESSAGE {&EGMBadInstrRate}.
               &ENDIF
               RETURN "NO-APPLY".
            END.
         END.
         ELSE DO:
            &IF DEFINED(SESSION-REMOTE) &THEN
            MESSAGE {&EGMNotInstrRate} VIEW-AS ALERT-BOX.
            &ELSE
            MESSAGE {&EGMNotInstrRate}.
            &ENDIF
            RETURN "NO-APPLY".
         END.
         IF GetXAttrValueEx ('op-template',
                             op-kind.op-kind + "," + STRING(op-templ.op-templ),
                             'rub-cur',
                             "0")  EQ "3" THEN
            ASSIGN op-entry.amt-rub:SENSITIVE = FALSE.
      END.
   END.
END PROCEDURE.

PROCEDURE OnLeave_Op-Entry.Currency:
  DO WITH FRAME opreq:
    IF     (AVAIL cacct AND AVAIL dacct)
       AND (   op-entry.currency:SCREEN-VALUE NE dacct.currency
            OR op-entry.currency:SCREEN-VALUE NE cacct.currency)
    THEN DO:
       op-entry.currency:SCREEN-VALUE = MAX(cacct.currency,dacct.currency).
    END.
    ASSIGN op-entry.currency.
    IF op-entry.currency = ? THEN op-entry.currency = "".
  END.
END PROCEDURE.
&ENDIF

PROCEDURE OnLeave_Op-Entry.Amt-Rub:
   /* Контролируем ввод нулевой суммы в QBIS */
   &IF DEFINED(SESSION-REMOTE) <> 0 &THEN
      IF     Op-Entry.Amt-Rub:SENSITIVE EQ TRUE
         AND (INPUT FRAME opreq op-entry.amt-rub EQ ? 
          OR INPUT FRAME opreq op-entry.amt-rub EQ 0)
      THEN
      DO:
         {message1
            &TEXT    = "Сумма не может быть равна нулю"
            &BUTTONS = OK
         }
         RETURN "NO-APPLY".
      END.
   &ENDIF
&IF DEFINED(NOtamt) &THEN
   DO WITH FRAME opreq:
      IF tamt = 0 THEN tamt = INPUT op-entry.amt-rub.
   END.
&ENDIF
&IF DEFINED(NOcurr) = 0 &THEN
   IF CAN-DO ("1,2",
              GetXAttrValueEx ('op-template',
                               op-kind.op-kind + "," + STRING(op-templ.op-templ),
                               'rub-cur',
                               "0"))
   THEN DO:
      IF op-entry.currency <> "" AND
         INPUT FRAME opreq op-entry.amt-rub <> ? AND
         INPUT FRAME opreq op-entry.amt-rub <> 0
      THEN DO WITH FRAME opreq: /* делаем пересчет поля "сумма в валюте" из поля "сумма в рублях" */
         FIND LAST instr-rate WHERE instr-rate.instr-cat = "currency" AND
                                    instr-rate.instr-code = INPUT op-entry.currency AND
                                    instr-rate.rate-type = "УЧЕТНЫЙ" AND
                                    instr-rate.since <= INPUT op-entry.value-date NO-LOCK NO-ERROR.
         IF AVAIL instr-rate THEN DO:
            IF instr-rate.rate-instr ne ? THEN DO:
               DISPLAY INPUT op-entry.amt-rub * instr-rate.per / instr-rate.rate-instr @ op-entry.amt-cur WITH FRAME opreq.
               IF tamt = 0 THEN tamt = INPUT op-entry.amt-cur.
            END.
            ELSE DO:
               &IF DEFINED(SESSION-REMOTE) &THEN
               MESSAGE {&EGMBadInstrRate} VIEW-AS ALERT-BOX.
               &ELSE
               MESSAGE {&EGMBadInstrRate}.
               &ENDIF
               RETURN "NO-APPLY".
            END.
         END.
         ELSE DO:
            &IF DEFINED(SESSION-REMOTE) &THEN
            MESSAGE {&EGMNotInstrRate} VIEW-AS ALERT-BOX.
            &ELSE
            MESSAGE {&EGMNotInstrRate}.
            &ENDIF
            RETURN "NO-APPLY".
         END.
      END. /* do with frame opreq */
   END. /* if can-do */
&ENDIF
END PROCEDURE.

&ENDIF
&IF DEFINED(NOmfo) = 0 &THEN
PROCEDURE OnAny-Printable_.Vmfo:
  APPLY KEYCODE({rus2lat.i keylabel(lastkey)}) TO vmfo IN FRAME opreq.
  RETURN "NO-APPLY".
END PROCEDURE.
&ENDIF

PROCEDURE OnF1_Op.Doc-Type:
  RUN browseld.p ("doc-type","","","",7).
  IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
    DISPLAY pick-value @ op.doc-type WITH FRAME opreq.
  RETURN "NO-APPLY".
END PROCEDURE.

&IF DEFINED(browse-entry) = 0 &THEN
PROCEDURE OnF1_Op-Entry.Op-Cod:
  RUN pclass.p ("КодОп", "КодОп", "Коды операций", 7).
  IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
    DISPLAY pick-value @ op-entry.op-cod WITH FRAME opreq.
END PROCEDURE.

PROCEDURE OnF1_Op-Entry.Symbol:
  RUN browseld.p("КасСимволы", "class~001beg-date2", "КасСимволы~001" + STRING(in-op-Date), "", 7).
  IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
    DISPLAY pick-value @ op-entry.symbol WITH FRAME opreq.
  RETURN "NO-APPLY".
END PROCEDURE.

&IF DEFINED(NOaccts) = 0 &THEN
PROCEDURE OnF1_Op-Entry.Acct-db:
   DO WITH FRAME opreq:
      {find-act.i
         &acct = "INPUT op-entry.acct-db"
      }
      IF AVAIL acct THEN
      DO:
                        /* Проверка права просмотра счета по группам */
         IF NOT GetAcctPermission (acct.acct + "," + acct.currency,"r")
            THEN RETURN "NO-APPLY".
         RUN acct#.p(acct.acct,acct.currency,3).
      END.
      ELSE
         RUN "acct(t).p"(recid(op-templ),
                         'acct-db-flt',
                         in-op-date,
                         3).
   END.
   /*  RUN acct.p (op-template.acct-cat, 3). */
   IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
      DISPLAY ENTRY(1, pick-value) @ op-entry.acct-db WITH FRAME opreq.
   RETURN "NO-APPLY".
END PROCEDURE.

PROCEDURE OnF1_Op-Entry.Acct-cr:
   DO WITH FRAME opreq:
      {find-act.i
         &acct = "INPUT op-entry.acct-cr"
      }
      IF AVAIL acct THEN
      DO:
                        /* Проверка права просмотра счета по группам */
         IF NOT GetAcctPermission (acct.acct + "," + acct.currency,"r")
            THEN RETURN "NO-APPLY".
         RUN acct#.p(acct.acct,acct.currency,3).
      END.
      ELSE
         RUN "acct(t).p"(recid(op-templ),
                         'acct-cr-flt',
                         in-op-date,
                         3).
   END.
   /*  RUN acct.p (op-template.acct-cat, 3). */
   IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
      DISPLAY ENTRY(1, pick-value) @ op-entry.acct-cr WITH FRAME opreq.
   RETURN "NO-APPLY".
END PROCEDURE.

PROCEDURE OnF1_Op-Entry.Amt-Rub:
  RUN calc.p.
  IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
    DISPLAY DECIMAL(pick-value) @ op-entry.amt-rub WITH FRAME opreq.
  RETURN "NO-APPLY".
END PROCEDURE.
&ENDIF
&ENDIF

&IF DEFINED(NOmfo) = 0 &THEN
PROCEDURE OnF1_.{&SEL-MFO}:

  RUN banks.p (3).
  IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
  DO:
     IF    mbank-code-type GT ""
       AND ENTRY(1, pick-value) GT ""
       AND ENTRY(1, pick-value) NE mbank-code-type THEN DO:
           MESSAGE "Выбран неверный тип идентификатора!" SKIP
                   'Правильный тип "' + mbank-code-type + '".'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           RETURN "NO-APPLY".
     END.
     DISPLAY ENTRY(2, pick-value) @ {&SEL-MFO}
            ""                   @ {&SEL-CORR}
            ""                   @ bank1.name
            ""                   @ bank2.name
           WITH FRAME opreq.
  END.
  RETURN "NO-APPLY".
END PROCEDURE.

PROCEDURE OnF1_.{&SEL-CORR}:

  IF AVAIL bank1 AND CAN-FIND(FIRST banks-corr OF bank1) THEN
    RUN banksch3.p (bank1.bank-id, 3).
  IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
    DISPLAY pick-value @ {&SEL-CORR}
            ""         @ bank2.name WITH FRAME opreq.
  RETURN "NO-APPLY".
END PROCEDURE.
&ENDIF

&IF DEFINED(OFcash) = 0 &THEN
PROCEDURE OnF1_Op.Order-Pay:
  IF AVAIL op-template THEN 
     vOrder-PayLst = GetXattrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "order-pay",
                                     "*").
  ELSE vOrder-PayLst = "*".

  FIND code WHERE code.class = "" AND code.code = self:name NO-LOCK NO-ERROR.

  IF AVAIL code THEN DO:
     RUN browseld.p("code",
                    "class" + CHR(1) + "FirstFrame" + CHR(1) + "code",
                    code.code + CHR(1) + "1" + CHR(1) + vOrder-PayLst,
                    "",
                    4). 
  END.
  
  IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN
    DISPLAY pick-value @ op.order-pay WITH FRAME opreq.
  RETURN "NO-APPLY".
END PROCEDURE.
PROCEDURE OnLeave_Op.order-pay:
   IF AVAIL op-template THEN 
     vOrder-PayLst = GetXattrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "order-pay",
                                     "*").
   ELSE vOrder-PayLst = "*".

   IF NOT CAN-FIND(FIRST code WHERE code.class EQ "order-pay" AND code.CODE EQ INPUT FRAME opreq op.order-pay) THEN DO:
      &IF DEFINED(SESSION-REMOTE) &THEN
      MESSAGE "Нет такого кода очередности платежа" VIEW-AS ALERT-BOX.
      &ELSE
      MESSAGE "Нет такого кода очередности платежа".
      &ENDIF      
      RETURN "NO-APPLY".
   END.
   ELSE
      IF NOT CAN-FIND(FIRST code WHERE code.class EQ "order-pay" 
                                   AND code.CODE  EQ INPUT FRAME opreq op.order-pay
                                   AND ((NOT {assigned code.val}) OR 
                                        (INPUT FRAME opreq op.doc-date EQ "") OR 
                                        DATE(code.val) GE INPUT FRAME opreq op.doc-date)) 
      THEN DO:
         &IF DEFINED(SESSION-REMOTE) &THEN
         MESSAGE "Код очередности платежа недействителен на дату" INPUT FRAME opreq op.doc-date VIEW-AS ALERT-BOX.
         &ELSE
         MESSAGE "Код очередности платежа недействителен на дату" INPUT FRAME opreq op.doc-date.
         &ENDIF      
         RETURN "NO-APPLY".
      END.
   IF NOT CAN-DO(vOrder-PayLst, INPUT FRAME opreq op.order-pay) THEN DO:
      &IF DEFINED(SESSION-REMOTE) &THEN
      MESSAGE "Разрешенные очередности платежа " + vOrder-PayLst VIEW-AS ALERT-BOX.
      &ELSE
      MESSAGE "Разрешенные очередности платежа " + vOrder-PayLst.
      &ENDIF      
      RETURN "NO-APPLY".
   
   END.
END.
&ENDIF

&IF DEFINED(DoLoan) &THEN

    /* F1 на номере договора по вкладам/кредитам и депозитам */
    PROCEDURE OnF1_.In-Cont-Code:

        if in-contract eq 'dps'

        /* частные вклады */
      then 
      DO:
         &IF DEFINED (Uni) &THEN
         IF mperson-id NE ? THEN
            RUN dpsdispc.p('dps',?,'ч',mperson-id,4).
         ELSE
            RUN dpsdispc.p('dps',?,'ч',?,4).
         &ELSE
         RUN  dpsdispc.p('dps',?,'ч',?,4).
         &ENDIF
      END.
        /* кредиты и депозиты */
        else if in-contract eq "кредит" or
                in-contract eq "депоз"

            /* Запуск браузера по договорам. */
            then if work-module eq "loan"
               then run browseld.p ((if in-contract eq "кредит"
                                     then "loan_allocat"
                                     else "loan_attract"),
                                     "contract",
                                     in-contract,
                                     ?,
                                     4).

                /* Запуск из из другого модуля требует определения
                ** расшаренных переменных */
                else run "loan_sh.p" (in-contract).

        if (last-event:function = "go" or
            last-event:function = "return") and
            pick-value <> ?
        then display pick-value @ in-cont-code
        with frame opreq.

        return "no-apply".

    END PROCEDURE.

    /* F1 на номере договора по вкладам/кредитам и депозитам */
    PROCEDURE OnF1_.In-Doc-Ref:

        if in-contract eq 'dps'

        /* частные вклады */
        then run dpsdispc.p('dps',?,'ч',?,4).

        /* кредиты и депозиты */
        else if in-contract eq "кредит" or
                in-contract eq "депоз"

            /* Запуск браузера по договорам. */
            then if work-module eq "loan"
               then run browseld.p ((if in-contract eq "кредит"
                                     then "loan_allocat"
                                     else "loan_attract"),
                                     "contract",
                                     in-contract,
                                     ?,
                                     4).

                /* Запуск из из другого модуля требует определения
                ** расшаренных переменных */
                else run "loan_sh.p" (in-contract).

        IF     (LAST-EVENT:FUNCTION = "GO" 
            OR  LAST-EVENT:FUNCTION = "RETURN") and
            pick-value <> ? then 
        DO:
           IF    NOT GetDBMode()            
              OR INDEX(pick-value,"@") GT 0 THEN
              FIND FIRST loan WHERE loan.contract  EQ in-contract
                                AND loan.cont-code EQ pick-value
                 NO-LOCK NO-ERROR.
           ELSE
           DO:
              FIND FIRST loan WHERE loan.contract  EQ in-contract
                                AND loan.cont-code EQ ENTRY(1, pick-value, " ") + 
                                                         "@" + shFilial + 
                                                         IF NUM-ENTRIES(pick-value, " ") > 1 THEN (" " + ENTRY(2, pick-value, " "))
                                                                                             ELSE ""
                 NO-LOCK NO-ERROR.              
           END.                    

           pick-value = IF AVAILABLE loan THEN loan.doc-ref
                                          ELSE "".

           DISPLAY pick-value @ in-doc-ref WITH FRAME opreq.
        END. 

        RETURN "NO-APPLY".

    END PROCEDURE.

    /* F1 на поле выбора валюты для мультивалютного договора */
    PROCEDURE OnF1_.In-Cont-Cur:
        DEFINE BUFFER b-loan FOR loan.
        DEFINE VAR vCurrs  AS CHAR NO-UNDO INIT "".
        DEFINE VAR vLocCur AS CHAR NO-UNDO.
        vLocCur = FGetSetting("КодНацВал", "", "810").
        
        IF in-contract NE 'dps' THEN RETURN "NO-APPLY".

        FOR EACH b-loan WHERE b-loan.parent-contract  = "dps" 
                          AND b-loan.parent-cont-code = INPUT FRAME opreq in-doc-ref
                        NO-LOCK:
           vCurrs = vCurrs + "," + b-loan.currency.
        END.
        vCurrs = SUBSTRING(vCurrs, 2). /*Убираем первую запятую. TRIM делать нельзя, т.к. если есть валюта "", она потеряется*/
        RUN browseld.p ("currency", "instr-cat~001currency", "currency~001" + vCurrs, "", 4).

        IF (LAST-EVENT:FUNCTION = "GO" OR
            LAST-EVENT:FUNCTION = "RETURN") AND
            pick-value <> ?
        THEN DO:
          IF pick-value = "" THEN pick-value = vLocCur.
          DISPLAY pick-value @ in-cont-cur WITH FRAME opreq.
        END.

        RETURN "NO-APPLY".
    END PROCEDURE.

&ENDIF

&IF DEFINED(NOben) = 0 &THEN
ON VALUE-CHANGED OF op.name-ben IN FRAME opreq
DO:
   UpdateSignsEx("op",STRING(op.op),"document-id","").
   UpdateSignsEx("op",STRING(op.op),"Докум","").
END.

PROCEDURE OnF1_Op.Name-Ben:
&IF DEFINED(likeDiasoft) = 0 &THEN
   DEFINE VARIABLE mAddr  AS CHARACTER   NO-UNDO.

   pick-value = ?.
   RELEASE cust-corp.
   RELEASE person.
   FIND client-rec WHERE client-rec.op-kind = op-templ.op-kind AND
                         client-rec.op-temp = op-templ.op-templ NO-LOCK NO-ERROR.
   IF NOT AVAIL client-rec THEN
     IF AMBIG client-rec THEN RUN "cli-r(ot.p" (op-templ.op-kind, op-templ.op-templ, 4).
                         ELSE RUN "cli-r(ot.p" ("", 1, 4).
   IF KEYFUNCTION(LASTKEY) <> "END-ERROR" THEN DO:
     IF pick-value <> ? THEN DO:
        IF pick-value BEGINS "Ю," THEN DO:
           FIND cust-corp WHERE cust-corp.cust-id = INT64(ENTRY(2, pick-value)) NO-LOCK.
        END.
        ELSE IF pick-value BEGINS "Ч," THEN do:
           FIND person WHERE person.person-id = INT64(ENTRY(2, pick-value)) NO-LOCK.
        END.
        ELSE IF pick-value BEGINS "Б," THEN DO:
           FIND banks WHERE banks.bank-id = INT64(ENTRY(2, pick-value)) NO-LOCK.
        END.
     END.
     ELSE DO:
       {cli-rec.fnd}
     END.
   END.
   IF AVAIL cust-corp THEN DO:
     DISPLAY &IF DEFINED(nomfo) = 0 &THEN
               cust-corp.benacct   @ op.ben-acct
               cust-corp.bank-code @ vmfo
               cust-corp.corr-acct @ vcorr-acct
             &ENDIF
             cust-corp.inn @ op.inn
             cust-corp.cust-stat + " " + cust-corp.name-corp @ op.name-ben WITH FRAME opreq.
     RUN GetCustAdr IN h_cust ("Ю",
                               cust-corp.cust-id,
                               gend-date,
                               "АдрЮр",
                               OUTPUT TABLE ttCustAddress).
     FIND LAST ttCustAddress NO-LOCK NO-ERROR.
     IF AVAILABLE ttCustAddress AND {assigned ttCustAddress.AdrStr} THEN
         mAddr = FrmtAddrStr(ttCustAddress.AdrStr,ttCustAddress.fCodReg).
     ELSE  
        mAddr = cust-corp.addr-of-low[1] + " " + cust-corp.addr-of-low[2].            

     /* запоминаем Адрес_пок и Телеф_пок для последующей проверки при сохранении */
     RUN SetSysConf IN h_base("g-trig - Адрес_пок", mAddr) NO-ERROR.
     RUN SetSysConf IN h_base("g-trig - Телеф_пок", cust-corp.fax) NO-ERROR.
   END.
   ELSE IF AVAIL person THEN DO:
     DISPLAY &IF DEFINED(nomfo) = 0 &THEN
               person.benacct   @ op.ben-acct
               person.bank-code @ vmfo
               person.corr-acct @ vcorr-acct
             &ENDIF
             person.name-last + " " + person.first-name @ op.name-ben
             person.inn @ op.inn
     WITH FRAME opreq.
     RUN GetCustAdr IN h_cust ("Ч",
                               person.person-id,
                               gend-date,
                               "АдрФакт",
                               OUTPUT TABLE ttCustAddress).
     FIND LAST ttCustAddress NO-LOCK NO-ERROR.
     IF AVAILABLE ttCustAddress AND {assigned ttCustAddress.AdrStr} THEN
        mAddr = FrmtAddrStr(ttCustAddress.AdrStr,ttCustAddress.fCodReg).
     ELSE 
        mAddr = person.address[1] + " " + person.address[2].                                   

     /* запоминаем Адрес_пок и Телеф_пок для последующей проверки при сохранении */
     RUN SetSysConf IN h_base("g-trig - Адрес_пок", mAddr) NO-ERROR.
     RUN SetSysConf IN h_base("g-trig - Телеф_пок",
                              person.phone[1] + " " + person.phone[2]) NO-ERROR.
  
     /* запоминаем пасп.данные */
     RUN SetSysConf IN h_Base ("ПаспортныеДанныеПостоянногоПолучателя",
                               person.document + " выдан " +
                               TRIM(person.issue + " " +
                                    GetXattrValueEx("person", STRING(person.person-id),
                                                    "Document4Date_Vid",
                                                    ""
                                    )
                               )
  
     ).
     RUN SetSysConf IN h_Base ("ПаспортныеДанныеПостоянногоПолучателя.document-id",
                               person.document-id
     ).
     &IF DEFINED(submit-person-id) NE 0 &THEN
     RUN SetSysConf IN h_Base ("ПостоянныйПолучатель", STRING(person.person-id)).
     &ENDIF
     &IF DEFINED(Ret_Name-Ben_Doc-Name) NE 0 &THEN
     RUN SetSysConf IN h_Base ("ПаспортныеДанныеПостоянногоПолучателя.НаименованиеДокумента",
                               GetCodeName("КодДокум", person.document-id)
     ).
     &ENDIF
     RUN SetSysConf IN h_base("tmp-person-id",STRING(person.person-id)).
     RUN SetSysConf IN h_base("tmp-cust-cat","Ч").
   END.
   ELSE IF AVAIL banks THEN DO:
     RUN GetCustIdent("Б", banks.bank-id, ?, ?, "ИНН", OUTPUT op.inn).
     DISPLAY
             banks.name @ op.name-ben
             op.inn
     WITH FRAME opreq.
     /* запоминаем Адрес_пок и Телеф_пок для последующей проверки при сохранении */
     RUN GetCustAdr IN h_cust ("Б",
                               banks.bank-id,
                               gend-date,
                               "*",
                               OUTPUT TABLE ttCustAddress).
     FIND LAST ttCustAddress NO-LOCK NO-ERROR.
     IF AVAILABLE ttCustAddress AND {assigned ttCustAddress.AdrStr} THEN
        mAddr = FrmtAddrStr(ttCustAddress.AdrStr,ttCustAddress.fCodReg).
     ELSE  
        mAddr = banks.law-address.      
     RUN SetSysConf IN h_base("g-trig - Адрес_пок", mAddr) NO-ERROR.
     RUN SetSysConf IN h_base("g-trig - Телеф_пок", "-") NO-ERROR.
   END.
&ELSE
   DEFINE VARIABLE vRecipCode AS CHARACTER NO-UNDO.
   DO WITH FRAME opreq:
   
      IF INPUT {&SEL-MFO} NE ""
         THEN DO:
            IF INPUT op.ben-acct <> "" THEN
               RUN cli-ds.p ("", 1, INPUT STRING(INT64(INPUT {&SEL-MFO}), "999999999") + "," + INPUT op.ben-acct, 4).
            ELSE
               RUN cli-ds.p ("", 1, INPUT STRING(INT64(INPUT {&SEL-MFO}), "999999999"), 4).
         END.
         ELSE RUN cli-ds.p ("", 1, "", 4).

      IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN DO:
         RUN GetRecipientValue IN h_cust (ENTRY(1,pick-value),
                                          GetEntries(2,pick-value,",",""),
                                          GetEntries(3,pick-value,",",""),
                                          "БИК,РАСЧ_СЧЕТ,ИМЯ,ИНН",
                                          OUTPUT vRecipCode
                                         ).
         IF GetEntries(1,vRecipCode,CHR(2),"") <> "" THEN DISPLAY
            GetEntries(1,vRecipCode,CHR(2),"") @ {&SEL-MFO}
            GetEntries(2,vRecipCode,CHR(2),"") @ op.ben-acct
            GetEntries(3,vRecipCode,CHR(2),"") @ op.name-ben
            GetEntries(4,vRecipCode,CHR(2),"") @ op.inn
         .
      END.
   END.
&ENDIF
   RETURN "NO-APPLY".
END PROCEDURE.
&ENDIF

&IF DEFINED(browse-entry) = 0 &THEN

&SCOPED-DEFINE ProcList-CheckCashAcctCurr "g-cash1,g-kas2"

PROCEDURE OnLeave_Op-Entry.symbol.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vCheckCS  AS LOGICAL   INITIAL NO NO-UNDO.
    DEFINE VARIABLE vErrorMsg AS CHARACTER INITIAL "" NO-UNDO.

    IF op-template.symbol = "-" OR
       FGetSetting("КонтрольКС", ?, "Нет") <> "Да"
       OR NOT AVAIL op-entry
    THEN
        RETURN.
    DO WITH FRAME opreq:
        {find-act.i &acct = "op-entry.acct-db"}
        IF NOT AVAILABLE acct       OR
           acct.contract <> "Касса" OR
           acct.acct-cat = "o"
        THEN DO:
            {find-act.i &acct = "op-entry.acct-cr"}
        END.
        IF NOT AVAILABLE acct       OR
           acct.contract <> "Касса" OR
           acct.acct-cat = "o"
        THEN
            RETURN.
        IF CAN-DO({&ProcList-CheckCashAcctCurr}, op-kind.proc) THEN
            vCheckCS = NOT {assigned acct.currency}.
        ELSE
            vCheckCS = NOT {assigned op-entry.currency}.
        IF vCheckCS THEN DO:
         &IF DEFINED(g_cash1_sym) <> 0 &THEN
            IF INPUT mSymbol = ? OR INPUT mSymbol = "" THEN
         &ELSE
            IF INPUT op-entry.symbol = ? OR INPUT op-entry.symbol = "" THEN
         &ENDIF
                vErrorMsg = "Не выбран кассовый символ!".
            ELSE IF getTCodeFld("val",
                                "КасСимволы",
                                &IF DEFINED(g_cash1_sym) <> 0 &THEN
                                   INPUT INPUT mSymbol,
                                &ELSE
                                   INPUT INPUT op-entry.symbol,
                                &ENDIF
                                INPUT INPUT op.doc-date) = ?
            THEN
                vErrorMsg = "Не найден кассовый символ!".
        END.
        IF vErrorMsg <> "" THEN DO:
            MESSAGE vErrorMsg VIEW-AS ALERT-BOX ERROR TITLE "Ошибка".
            RETURN "NO-APPLY".
        END.
    END.
END PROCEDURE.
&ENDIF

PROCEDURE doc-date-leave-post:
   run RunClassMethod in h_xclass(op.class-code,
                                  "chkupd",
                                  "doc-date","",
                                  ?,
                                  SELF:SCREEN-VALUE + "," + STRING(in-op-date))
      NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RETURN "POST-ERROR".
END.

PROCEDURE OnLeave_Op.Doc-type:
   IF NUM-ENTRIES(op-templ.doc-type) > 1 AND LOOKUP(SELF:SCREEN-VALUE, op-templ.doc-type) = 0 THEN DO:
      &IF DEFINED(SESSION-REMOTE) &THEN
      MESSAGE "Недопустимый код расчетно-денежного документа" VIEW-AS ALERT-BOX.
      &ELSE
      MESSAGE "Недопустимый код расчетно-денежного документа".
      &ENDIF
      RETURN "NO-APPLY".
   END.
   FIND FIRST doc-type WHERE doc-type.doc-type = INPUT FRAME opreq op.doc-type NO-LOCK NO-ERROR.
   IF NOT AVAIL doc-type THEN DO:
      &IF DEFINED(SESSION-REMOTE) &THEN
      MESSAGE {&EGMBadDocType} VIEW-AS ALERT-BOX.
      &ELSE
      MESSAGE {&EGMBadDocType}.
      &ENDIF
      RETURN "NO-APPLY".
   END.
   ELSE DISPLAY doc-type.name WITH FRAME opreq.
END.

&IF DEFINED(NOmfo) = 0 &THEN
PROCEDURE OnLeave_.{&SEL-MFO}:

def var flag-error as INT64 no-undo.

&IF defined(likeDiaSoft) &THEN
  IF LAST-EVENT:LABEL = "SHIFT-TAB" OR LAST-EVENT:LABEL = "CURSOR-UP" THEN RETURN.
&ENDIF
/* Если вызов доп. формы ввода налоговых реквизитов - без проверок */
   IF LAST-EVENT:LABEL = "F2" THEN
      RETURN.

   DO WITH FRAME opreq:
      {g-bank.lv "{&SEL-MFO}" "{&SEL-CORR}" "op.ben-acct" ""NO-APPLY""}
   END.

END PROCEDURE.

PROCEDURE OnLeave_.{&SEL-CORR}:

   DEF VAR vFlag AS INT64 NO-UNDO. /*уровень обработки*/
   DEF VAR vBicAcct AS CHAR NO-UNDO. /*бик,счет*/
&IF defined(likeDiaSoft) &THEN
   IF LAST-EVENT:LABEL = "SHIFT-TAB" OR LAST-EVENT:LABEL = "CURSOR-UP" THEN RETURN.
&ENDIF
   DO WITH FRAME opreq:

      {g-corr.lv {&SEL-CORR} ""NO-APPLY""}
      RUN chbenacc.p (in-op-date, "," + (input {&SEL-CORR}),
                      input input {&SEL-MFO},op.op-kind,1,no,
                      OUTPUT msg, OUTPUT vFlag,OUTPUT vBicAcct).

      IF vFlag EQ 1 THEN DO:
         DISP ENTRY(1,vBicAcct) WHEN {&SEL-MFO}:SENSITIVE
                                @ {&SEL-MFO}
              ENTRY(2,vBicAcct) @ op.ben-acct.
         APPLY "ENTRY" TO {&SEL-MFO}.
         RETURN.
      END.
      ELSE IF vFlag EQ 2 THEN
         RETURN "NO-APPLY".

   END.

END PROCEDURE.

PROCEDURE OnLeave_Op.Ben-Acct:
   def var lvmfo as char no-undo.
   DEF VAR vFlag AS INT64 NO-UNDO. /*уровень обработки*/
   DEF VAR vBicAcct AS CHAR NO-UNDO. /*бик,счет*/

   DEFINE VARIABLE choice AS LOGICAL NO-UNDO.

&IF defined(likeDiaSoft) &THEN
   IF LAST-EVENT:LABEL = "SHIFT-TAB" OR LAST-EVENT:LABEL = "CURSOR-UP" THEN RETURN.
&ENDIF
/* Если вызов доп. формы ввода налоговых реквизитов - без проверок */
   IF LAST-EVENT:LABEL = "F2" THEN
      RETURN.
   DO WITH FRAME opreq:
&IF DEFINED(CheckBenAccCurrency) NE 0 &THEN
      IF mforeq  THEN
      DO:

         /* Проверка кода валюты в ben-acct если рублевая операция */

         IF     op-entry.currency EQ "" 
            AND INPUT op.ben-acct NE "" 
            AND INPUT op.ben-acct NE "00000000000000000000" THEN
         DO:

            IF SUBSTRING(INPUT op.ben-acct,6,3) NE FGetSetting("КодНацВал","","810") THEN
            DO:
               MESSAGE SKIP "Код валюты должен быть равен " FGetSetting("КодНацВал","","810") "! Продолжить?" 
                       SKIP(1) 
               VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO 
               UPDATE choice.

               IF choice NE TRUE THEN
               DO:
                  NEXT-PROMPT op.ben-acct.
                  RETURN "NO-APPLY".
               END.
            END.
         END.
      END.
&ENDIF
      ASSIGN
         lvmfo = input {&SEL-MFO}
         lvmfo = string(dec(lvmfo),"999999999").
      RUN chbenacc.p (in-op-date, INPUT INPUT op.ben-acct,
                      lvmfo,op.op-kind,1,no,
                      OUTPUT msg, OUTPUT vFlag,OUTPUT vBicAcct).

      IF vFlag EQ 1 THEN DO:
         DISP ENTRY(1,vBicAcct) WHEN {&SEL-MFO}:SENSITIVE
                         @ {&SEL-MFO}
              ENTRY(2,vBicAcct)  @ op.ben-acct.
         APPLY "ENTRY" TO {&SEL-MFO}.
         RETURN.
      END.
      ELSE IF vFlag EQ 2 THEN
         RETURN "NO-APPLY".

      ELSE IF vFlag EQ 0 AND
           msg NE "" THEN DO:
         MESSAGE msg VIEW-AS ALERT-BOX ERROR.
         RETURN "NO-APPLY".
      END.

    IF INPUT op.ben-acct = "" THEN msg = {&EGMMissingBenAcct}.
    ELSE IF LENGTH(INPUT op.ben-acct) <> 20 THEN msg = {&EGMBadBenAcctLen}.
    ELSE DO:
      temp-acct = STRING(DEC(INPUT op.ben-acct), "99999999999999999999") no-error.
      IF ERROR-STATUS:ERROR THEN msg = {&EGMBadBenAcctSym}.
      ELSE DO:
         RUN key-tst.p (DEC (INPUT op.ben-acct), INPUT INPUT vmfo, OUTPUT acctkey).
         IF acctkey <> ? AND acctkey <> INT64(SUBSTR(temp-acct, 9, 1)) THEN msg = {&EGMBadBenAcctKey}.
         ELSE /* Ищем получателя только если поля name-ben и inn еще пустые */
         IF op.name-ben:SCREEN-VAL EQ "" AND
            op.inn:SCREEN-VALUE    EQ "" THEN
         DO:
&IF DEFINED(likeDiaSoft) NE 0 AND DEFINED(NOTAUTORECIPIENT) EQ 0 &THEN
            FIND code WHERE
                 code.class = "recipient"
             AND code.code BEGINS STRING (INT64 (INPUT {&SEL-MFO}), "999999999")
                                + "," + INPUT op.ben-acct + "," + INPUT op.inn
               NO-LOCK NO-ERROR.
            IF NOT AVAIL code THEN
               /* Существуют несколько одинаковых получателей */
               IF AMBIGUOUS code THEN
               DO:
                  RUN OnF1_Op.Ben-Acct.
                  RETURN.
               END.
               ELSE
               DO:
                  FIND code WHERE
                       code.class = "recipient"
                   AND code.code BEGINS STRING (INT64 (INPUT {&SEL-MFO}),"999999999")
                                        + "," + INPUT op.ben-acct
                     NO-LOCK NO-ERROR.
                  /* Существуют несколько одинаковых получателей */
                  IF AMBIGUOUS code THEN
                  DO:
                     RUN OnF1_Op.Ben-Acct.
                     RETURN.
                  END.
               END.
            /* Существует единственный получатель */
            IF AVAIL code THEN
               DISPLAY
                  code.name @ op.name-ben
                  code.val  @ op.inn.
            ELSE
               ASSIGN
                  op.name-ben:SCREEN-VALUE = ""
                  op.inn:SCREEN-VALUE      = ""
               .
&ENDIF
            RETURN.
         END. /* IF op.name-ben:SCREEN-VALUE eq ""... */
         ELSE RETURN.
      END.
    END.
    BELL.
    &IF DEFINED(EmptyBenAcctOK) &THEN 
    DO:
      MESSAGE msg "! Продолжить ввод?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO SET choice.
      IF choice <> TRUE THEN RETURN "NO-APPLY".
    END.
    &ELSE 
    DO:
      IF op-templ.mfo-needed THEN DO:
         MESSAGE msg "! Повторите ввод!" VIEW-AS ALERT-BOX ERROR.

      IF NOT GetXAttrValue("op-template", op-kind.op-kind  + "," + STRING(op-templ.op-templ), "NoDeleteRS") = "Да" THEN
         DISP "" @ op.ben-acct.
      RETURN "NO-APPLY".    
      END.                    
      ELSE DO:
         MESSAGE msg "! Продолжить ввод?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO SET choice.
         IF choice <> TRUE THEN RETURN "NO-APPLY".
      END.
    END.
    &ENDIF
  END.
END.
&ENDIF

&IF DEFINED(browse-entry)  = 0 &THEN
&IF DEFINED(NOaccts)       = 0 &THEN
PROCEDURE OnLeave_Op-Entry.Acct-DB:
   DEFINE VAR tt-acct       LIKE op-entry.acct-db NO-UNDO.
   DEFINE VAR vNumber       AS CHAR               NO-UNDO.
   DEFINE VAR vDocTypeDig   AS CHAR               NO-UNDO.
   DEFINE VAR vOrderPay     AS INT64              NO-UNDO.
   DEFINE VAR vCustNameLine AS CHAR               NO-UNDO.
   DEFINE VAR vAmtAftAcct   AS LOGICAL            NO-UNDO.
   vNumber = op-entry.acct-db.
   DO WITH FRAME opreq:
      &IF DEFINED (ie) LT 1
      &THEN 
         tt-acct = INPUT op-entry.acct-db.
         /* Определение счета. */

         APPLY "ENTRY" TO FRAME opreq.
         RUN AssumeAcct (input-output tt-acct).
         APPLY "ENTRY" TO op-entry.acct-db.

         /* Если счет не определен и что-то введено,
         ** то выдаем ошибку. */
         IF       tt-acct                          EQ ?
            AND   LENGTH (INPUT op-entry.acct-db)  GT 0
         THEN DO:
            IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN DO:
               &IF DEFINED(SESSION-REMOTE) &THEN
               MESSAGE "Нет такого счета или у Вас недостаточно прав." VIEW-AS ALERT-BOX.
               &ELSE
               MESSAGE "Нет такого счета или у Вас недостаточно прав.".
               &ENDIF
            END.
            RETURN "NO-APPLY".
         END.
         IF       tt-acct                 NE ?
            AND   INPUT op-entry.acct-db  NE tt-acct
            THEN DISPLAY ENTRY(1,tt-acct) @ op-entry.acct-db.
                        /* Берем номер счета с экрана. */
         vNumber = INPUT op-entry.acct-db.
      &ENDIF
      
      IF CAN-FIND (acct WHERE
               acct.filial-id EQ dept.branch
         AND   acct.number    EQ vNumber)
      THEN DO:
         tacct-db = vNumber.
         {g-c871.chk}
         IF     op-entry.currency NE ?
            AND (AVAIL cacct AND cacct.currency GT "")
         THEN DO:
            IF NOT type-curracct
            THEN DO:
               {find-act.i
                  &fila = LAST
                  &bact = dacct
                  &acct = "INPUT op-entry.acct-db"
                  &curr = op-entry.currency
               }
            END.
            ELSE FIND LAST dacct WHERE
                     dacct.filial-id   EQ dept.branch
               AND   dacct.number      EQ vNumber
               AND op-entry.currency   EQ dacct.currency
            NO-LOCK NO-ERROR.

            IF NOT AVAIL dacct
            THEN DO:
               &IF DEFINED(ie) < 1
               &THEN
                  &IF DEFINED(EGMMismatchAcctCur) &THEN
                  MESSAGE {&EGMMismatchAcctCur}
                  VIEW-AS ALERT-BOX ERROR.
                  &ENDIF
               &ENDIF
               RETURN "NO-APPLY".
            END.
         END.
         ELSE DO:
            FIND FIRST dacct WHERE
                     dacct.filial-id EQ dept.branch
               AND   dacct.number    EQ vNumber
            NO-LOCK.
            FIND FIRST xacct WHERE
                     xacct.filial-id EQ dept.branch
               AND   xacct.number    EQ dacct.number
               AND   xacct.currency  NE dacct.currency
            NO-LOCK NO-ERROR.
            IF AVAIL xacct
            THEN DO:
               RUN "acct(l).p" (dacct.acct, in-op-date, 7).
               IF       LASTKEY     EQ 10
                  AND   pick-value  NE ?
               THEN DO:
                  {find-act.i
                     &bact = dacct 
                     &acct = "INPUT op-entry.acct-db"
                     &curr = pick-value
                  }
               END.
               ELSE RETURN "NO-APPLY".
            END.
            ASSIGN
               &IF DEFINED(ie) LT 1
               &THEN
                  op-entry.acct-db = INPUT op-entry.acct-db
               &ENDIF
               op-entry.currency =  IF dacct.currency GT ""
                                       THEN dacct.currency
                                       ELSE op-entry.currency
               &IF DEFINED (recalc-acct) NE 0
               &THEN
                  wop.currency = op-entry.currency
               &ENDIF
            .
            &IF DEFINED(ie) < 1
            &THEN
               DISP op-entry.currency
               WITH FRAME opreq.
            &ENDIF
         END.
                        /* Проверяем право дебетования счета по группам счета */
         IF     AVAILABLE dacct
            AND NOT GetAcctPermission (dacct.acct + "," + dacct.currency,"db")
         THEN RETURN "NO-APPLY".

         IF AVAILABLE dacct THEN DO:
            RUN GetDocTypeDigital IN h_op (op.doc-type, ?, OUTPUT vDocTypeDig).
            vOrderPay = INT64(op.order-pay) NO-ERROR.
            IF  vDocTypeDig <> "02"
            AND vDocTypeDig <> "06"
            AND (       (vOrderPay = ?)
                 OR NOT (vOrderPay >= 1 AND vOrderPay <= 3)) THEN DO:
               RUN chk-blk.p (dacct.cust-cat,dacct.cust-id).
               IF RETURN-VALUE EQ "0" THEN DO:
                  {getcustline.i &cust-cat = "dacct.cust-cat" &cust-id = "dacct.cust-id" &output-to = "vCustNameLine"}
                  RUN Fill-SysMes IN h_tmess ("", "acct43", "", "%s=" + vCustNameLine + "%s=" + STRING(dacct.number,GetAcctFmt(dacct.acct-cat))).
                  RETURN "NO-APPLY".
               END.
            END.
         END.

         ASSIGN op-entry.acct-db.

         &IF DEFINED (recalc-acct) NE 0
         &THEN
            wop.acct-db = op-entry.acct-db.
         &ENDIF
         {dacct.i
            &pref = d
            &side = db
            {&*}
         }
         RUN CheckCardA (op-entry.acct-db,op-entry.currency, "КБС").
         &IF DEFINED(ie) < 1
         &THEN
            IF RETURN-VALUE NE "" THEN DO:
               MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
            END.
         &ENDIF
         RUN CheckCard2 (op-entry.acct-db,op-entry.currency).
         &IF DEFINED(ie) < 1
         &THEN
            IF RETURN-VALUE NE "" THEN DO:
               MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
            END.
         &ENDIF
         &IF DEFINED (recalc-acct) NE 0
         &THEN
            IF    INPUT op-entry.acct-cr EQ ?
               OR input op-entry.acct-cr EQ ""
            THEN DO:
               {g-acctv1.i
                  &nodef-GetAcct = *
                  &vacct         = "op-entry.acct"
                  &no-dacct      = *
                  &OFbase        = YES
                  {&*}
               }
               IF op-entry.acct-cr NE ?
               THEN DO:
                  IF tcur NE ?
                  THEN DO:
                     {find-act.i
                        &bact = cacct
                        &acct = op-entry.acct-cr
                        &curr = tcur
                     }
                  END.
                  ELSE DO:
                     {find-act.i
                        &bact = cacct
                        &acct = op-entry.acct-cr
                     }
                  END.
                  {dacct.i
                     &pref = c
                     &side = cr
                     &acct = op-entry.acct-cr
                     {&*}
                  }
               END.
            END.
         &ENDIF
         &IF DEFINED(ACCTMESS) NE 0
         &THEN
            RUN transmes.p (dacct.acct, dacct.currency, op-kind.op-kind, "ДЕБЕТ", INPUT-OUTPUT mClMessList).
         &ENDIF
      END.
      vAmtAftAcct = GetXAttrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "AmtAftAcct","НЕТ") EQ "Да".
      IF ({assigned op-template.prep-amt-rub} OR
          {assigned op-template.prep-amt-natcur}) AND
         vAmtAftAcct AND
         NOT g_trig_i_no_parse
      THEN DO:
         RUN parssen.p (RECID(wop), in-op-date, OUTPUT fler) NO-ERROR.
         IF ERROR-STATUS:ERROR OR fler THEN wop.amt-rub = 0.
         IF NOT auto THEN DO:
            DISPLAY
               wop.amt-rub WHEN wop.amt-rub NE ?        @ op-entry.amt-rub
                         0 WHEN wop.amt-rub EQ ?        @ op-entry.amt-rub
               wop.amt-cur WHEN op-entry.currency NE "" @ op-entry.amt-cur
                         0 WHEN op-entry.currency EQ "" OR wop.amt-cur EQ ? @ op-entry.amt-cur
            WITH FRAME opreq.
            op-entry.amt-rub:SENSITIVE = (wop.amt-rub = 0 OR
                                          wop.amt-rub = ? OR
                                          GetXAttrValueEx("op-template",
                                                          Surrogate(BUFFER op-template:HANDLE),
                                                          "EditAmtFlag",
                                                          "Нет") = "Да").
         END.
      END.
   END.
   op-entry.acct-db = AddFilToAcct(op-entry.acct-db,shFilial).
END PROCEDURE.

PROCEDURE OnLeave_Op-Entry.Acct-CR:
   DEFINE VAR tt-acct       LIKE op-entry.acct-db NO-UNDO.
   DEFINE VAR vNumber       AS CHAR               NO-UNDO.
   DEFINE VAR vAmtAftAcct   AS LOGICAL            NO-UNDO.
   vNumber = op-entry.acct-cr.
   DO WITH FRAME opreq:
      &IF DEFINED(ie) LT 1
      &THEN
         tt-acct = input op-entry.acct-cr.
         /* Определение счета. */

         APPLY "ENTRY" TO FRAME opreq.
         RUN AssumeAcct (input-output tt-acct).
         APPLY "ENTRY" TO op-entry.acct-cr.

         /* Если счет не определен и что-то введено,
         ** то выдаем ошибку. */
         IF       tt-acct                          EQ ?
            AND   LENGTH (INPUT op-entry.acct-cr)  GT 0
         THEN DO:
            IF LAST-EVENT:FUNCTION NE "END-ERROR" THEN DO:
               &IF DEFINED(SESSION-REMOTE) &THEN
               MESSAGE "Нет такого счета или у Вас недостаточно прав." VIEW-AS ALERT-BOX.
               &ELSE
               MESSAGE "Нет такого счета или у Вас недостаточно прав.".
               &ENDIF
            END.
            RETURN "NO-APPLY".
         END.
         IF       tt-acct                 NE ?
            AND   INPUT op-entry.acct-cr  NE tt-acct
            THEN DISPLAY ENTRY(1,tt-acct) @ op-entry.acct-cr.
                        /* Берем номер счета с экрана. */
         vNumber = INPUT op-entry.acct-cr.
      &ENDIF
      IF CAN-FIND (acct WHERE
                        acct.filial-id EQ dept.branch
                   AND  acct.number    EQ vNumber)
      THEN DO:
         tacct-cr = vNumber.
         IF       op-entry.currency NE ?
            AND   (AVAIL dacct AND dacct.currency GT "")
         THEN DO:
            IF NOT type-curracct
            THEN DO:
               {find-act.i
                  &fila = LAST
                  &bact = cacct
                  &acct = "INPUT op-entry.acct-cr"
                  &curr = op-entry.currency
               }
            END.
            ELSE FIND LAST cacct WHERE
                        cacct.filial-id   EQ dept.branch
                  AND   cacct.number      EQ vNumber
                  AND   op-entry.currency EQ cacct.currency
               NO-LOCK NO-ERROR.
            IF NOT AVAIL cacct
            THEN DO:
               &IF DEFINED (ie) LT 1
               &THEN
                  &IF DEFINED(EGMMismatchAcctCur) &THEN
                  MESSAGE {&EGMMismatchAcctCur}
                  VIEW-AS ALERT-BOX ERROR.
                  &ENDIF
               &ENDIF
               RETURN "NO-APPLY".
            END.
         END.
         ELSE DO:
            FIND FIRST cacct WHERE
                     cacct.filial-id   EQ dept.branch
               AND   cacct.number      EQ vNumber
            NO-LOCK.
            FIND FIRST xacct WHERE
                     xacct.filial-id   EQ dept.branch
               AND   xacct.number      EQ cacct.acct
               AND   xacct.currency    NE cacct.currency
            NO-LOCK NO-ERROR.
            IF AVAIL xacct
            THEN DO:
               RUN "acct(l).p" (cacct.acct, in-op-date, 7).
               IF       LASTKEY     EQ 10
                  AND   pick-value  NE ?
               THEN DO:
                  {find-act.i
                     &bact = cacct 
                     &acct = "INPUT op-entry.acct-cr"
                     &curr = pick-value
                  }
               END.
               ELSE RETURN "NO-APPLY".
            END.
            ASSIGN
               &IF DEFINED (ie) LT 1
               &THEN
                  op-entry.acct-cr = INPUT op-entry.acct-cr
               &ENDIF
               op-entry.currency = cacct.currency
               &IF DEFINED (recalc-acct) NE 0
               &THEN
                  wop.currency = op-entry.currency
               &ENDIF
            .
            &IF DEFINED (ie) LT 1
            &THEN
               DISP op-entry.currency
               WITH FRAME opreq.
            &ENDIF
         END.

                        /* Проверяем право кредитования счета по группам счета */
         IF     AVAILABLE cacct
            AND NOT GetAcctPermission (cacct.acct + "," + cacct.currency,"CR")
         THEN RETURN "NO-APPLY".

         ASSIGN op-entry.acct-cr.

         &IF DEFINED (recalc-acct) NE 0
         &THEN
            wop.acct-cr = op-entry.acct-cr.
         &ENDIF
         {dacct.i
            &pref = c
            &side = cr
            {&*}
         }
         RUN CheckCardA (op-entry.acct-cr, op-entry.currency, "КБС").
         &IF DEFINED (ie) LT 1
         &THEN
            IF RETURN-VALUE NE "" THEN DO:
               MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
            END.
         &ENDIF
         RUN CheckCard2 (op-entry.acct-cr, op-entry.currency).
         &IF DEFINED (ie) LT 1
         &THEN
            IF RETURN-VALUE NE "" THEN DO:
               MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
            END.
         &ENDIF
         &IF DEFINED (recalc-acct) NE 0
         &THEN
            IF    INPUT op-entry.acct-db EQ ?
               OR INPUT op-entry.acct-db EQ ""
            THEN DO:
               {g-acctv1.i
                  &nodef-GetAcct = *
                  &vacct         = "op-entry.acct"
                  &no-cacct      = *
                  &OFbase        = YES
                  {&*}
               }
               IF op-entry.acct-db NE ?
               THEN DO:
                  IF tcur NE ?
                  THEN DO:
                     {find-act.i
                        &bact = dacct
                        &acct = op-entry.acct-db
                        &curr = tcur
                     }
                  END.
                  ELSE DO:
                     {find-act.i
                        &bact = dacct
                        &acct = op-entry.acct-db
                     }
                  END.
                  {dacct.i
                     &pref = d
                     &side = db
                     &acct = op-entry.acct-db
                     {&*}
                  }
               END.
            END.
         &ENDIF
         &IF DEFINED(ACCTMESS) NE 0
         &THEN
            RUN transmes.p (cacct.acct, cacct.currency, op-kind.op-kind, "КРЕДИТ", INPUT-OUTPUT mClMessList).
         &ENDIF
      END.
      vAmtAftAcct = GetXAttrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "AmtAftAcct","НЕТ") EQ "Да".
      IF ({assigned op-template.prep-amt-rub} OR
          {assigned op-template.prep-amt-natcur}) AND
         vAmtAftAcct AND
         NOT g_trig_i_no_parse
      THEN DO:
         RUN parssen.p (RECID(wop), in-op-date, OUTPUT fler) NO-ERROR.
         IF ERROR-STATUS:ERROR OR fler THEN wop.amt-rub = 0.
         IF NOT auto THEN DO:
            DISPLAY
               wop.amt-rub WHEN wop.amt-rub NE ?        @ op-entry.amt-rub
                         0 WHEN wop.amt-rub EQ ?        @ op-entry.amt-rub
               wop.amt-cur WHEN op-entry.currency NE "" @ op-entry.amt-cur
                         0 WHEN op-entry.currency EQ "" OR wop.amt-cur EQ ? @ op-entry.amt-cur
            WITH FRAME opreq.
            op-entry.amt-rub:SENSITIVE = (wop.amt-rub = 0 OR
                                          wop.amt-rub = ? OR
                                          GetXAttrValueEx("op-template",
                                                          Surrogate(BUFFER op-template:HANDLE),
                                                          "EditAmtFlag",
                                                          "Нет") = "Да").
         END.
      END.
   END.
   op-entry.acct-cr = AddFilToAcct (op-entry.acct-cr,shFilial).
END PROCEDURE.
&ENDIF
&ENDIF  /* &IF DEFINED(browse-entry) &THEN */

&IF DEFINED(DoLoan)
&THEN

    /* Проверки и предустановки по договорам/вкладам */
    PROCEDURE OnLeave_.In-Cont-Code:
        DEF VAR tt-cont-code AS CHAR NO-UNDO.
        do with frame opreq:
            &IF DEFINED( mod-card ) NE 0 &THEN
               IF( loan.contract eq 'card' ) THEN DO:
                  FIND FIRST loan-transaction OF loan NO-LOCK NO-ERROR.
                  RUN put-loan-trans IN loan_h ( RECID(loan-transaction) ).
               END.
            &ELSE
               IF in-contract EQ 'dps' THEN DO:
                  tt-cont-code = INPUT in-cont-code.
                  RUN AssumeContCode(INPUT-OUTPUT tt-cont-code).
                  IF tt-cont-code EQ ? THEN
                     return "no-apply".
                  if INPUT in-cont-code <> tt-cont-code then
                    display tt-cont-code @ in-cont-code.
               END.
            &ENDIF
            /* Если что нибудь на входе? */
            if input frame opreq in-cont-code eq ""
            then do:

                message "Введите номер договора!"
                view-as alert-box error.

                return "no-apply".
            end.

            /* Если есть, то ищем договор/вклад */
            find loan where
                loan.contract  eq in-contract and
                loan.cont-code eq input in-cont-code
            exclusive-lock no-wait no-error.

            if not avail loan
            then do:

                if locked loan
                then do:

                    message "С " + (if in-contract eq 'dps'
                                    then 'вкладом'
                                    else 'договором') +
                            " кто-то-работает.Подождите!"
                    view-as alert-box error.

                    return "no-apply".
            end.
            else do:

                message "Договор не найден!"
                view-as alert-box error.

                return "no-apply".
            end.
        end.

        if loan.close-date ne ?
        then do:

            message 'Договор уже закрыт!'
            view-as alert-box error.

            return "no-apply".

        end.

        if loan.open-date gt in-op-date
        then do:
            message 'Договор еще не открыт'
            view-as alert-box error.

            return "no-apply".

        end.

        if loan.contract eq 'dps'
        then do:

            {chktype.i}

            &if defined(Do_Trans)
            &then

            run put-loan in h_templ (recid(loan)).

            &endif

            Set_type(input input in-cont-code).
        end.

        &IF DEFINED(MOD_DPS)
        &THEN

        else Set_loan(loan.contract,loan.cont-code).

        &ENDIF
        
        IF AVAILABLE op-entry AND op-entry.currency eq ?
        then op-entry.currency = loan.currency.
    end.
END.

/* Проверки по валюте договора для мультивалютных вкладов */
PROCEDURE OnLeave_.in-cont-cur:
DEFINE BUFFER b-loan FOR loan.
DEFINE VAR vLocCur AS CHAR NO-UNDO.
  vLocCur = FGetSetting("КодНацВал", "", "810").
  DO WITH FRAME opreq:
    IF INPUT FRAME opreq in-cont-cur  EQ "" THEN DO:
       MESSAGE "Введите валюту операции!" VIEW-AS ALERT-BOX ERROR.
       RETURN "NO-APPLY".
    END.
    IF INPUT FRAME opreq in-cont-cur <> vLocCur AND 
       NOT CAN-FIND(FIRST currency WHERE currency.currency eq INPUT FRAME opreq in-cont-cur) THEN DO:
       MESSAGE "Такой валюты не существует в справочнике!" VIEW-AS ALERT-BOX ERROR.
       RETURN "NO-APPLY".
    END.
    FIND FIRST b-loan WHERE b-loan.parent-contract  = "dps" 
                        AND b-loan.parent-cont-code = INPUT FRAME opreq in-doc-ref
                        AND b-loan.currency         = IF (INPUT FRAME opreq in-cont-cur = vLocCur) THEN "" ELSE INPUT FRAME opreq in-cont-cur
                      NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-loan THEN DO:
       MESSAGE "Договор" INPUT FRAME opreq in-doc-ref "не поддерживает операции в валюте" 
                         INPUT FRAME opreq in-cont-cur "!" VIEW-AS ALERT-BOX ERROR.
       RETURN "NO-APPLY".
    END.
    in-loan = b-loan.doc-ref.
    DISPLAY in-loan-label in-loan.

    IF b-loan.contract EQ 'dps' THEN DO:
       &if defined(Do_Trans) &then
       run put-loan in h_templ (recid(b-loan)).
       &endif
       Set_type(b-loan.cont-code).
    end.
    &IF DEFINED(MOD_DPS)
    &THEN
    else Set_loan(b-loan.contract,b-loan.cont-code).
    &ENDIF

    op-entry.currency = b-loan.currency.
  END.
END PROCEDURE.

/* Проверки и предустановки по договорам/вкладам */
PROCEDURE OnLeave_.In-Doc-Ref:
DEF VAR tt-cont-code AS CHAR NO-UNDO.
DEFINE BUFFER b-loan FOR loan.
IF LOOKUP(THIS-PROCEDURE:FILE-NAME,PROGRAM-NAME(3)," ") EQ 0 THEN RETURN.
  DO WITH FRAME opreq:
    &IF DEFINED( mod-card ) NE 0 &THEN
       IF( loan.contract eq 'card' ) THEN DO:
          FIND FIRST loan-transaction OF loan NO-LOCK NO-ERROR.
          RUN put-loan-trans IN loan_h ( RECID(loan-transaction) ).
       END.
    &ELSE
       IF in-contract EQ 'dps' THEN DO:
          tt-cont-code = INPUT /*in-cont-code*/ in-doc-ref.
          RUN AssumeContCode(INPUT-OUTPUT tt-cont-code).
          IF tt-cont-code EQ ? THEN
             return "no-apply".
          if INPUT /*in-cont-code*/ in-doc-ref  <> tt-cont-code then
            display tt-cont-code @ /*in-cont-code*/ in-doc-ref.
       END.
    &ENDIF
    
    /* Если что нибудь на входе? */
    IF INPUT FRAME opreq /*in-cont-code*/ in-doc-ref  EQ "" THEN DO:
       message "Введите номер договора!" view-as alert-box error.
       return "no-apply".
    END.

    /* Если есть, то ищем договор/вклад */
    find loan where loan.filial-id EQ shFilial AND
                    loan.contract  eq in-contract and
                  /*loan.cont-code eq input in-cont-code*/
                    loan.doc-ref   EQ INPUT in-doc-ref 
                    exclusive-lock no-wait no-error.
    IF NOT AVAIL loan THEN DO:
        IF locked loan THEN DO:
            message "С " + (if in-contract eq 'dps' then 'вкладом' else 'договором') + " кто-то-работает.Подождите!"
                    view-as alert-box error.
            return "no-apply".
        END. 
        message "Договор не найден!" view-as alert-box error.
        return "no-apply".
    end.

    if loan.close-date ne ? then do:
       message 'Договор уже закрыт!' view-as alert-box error.
       return "no-apply".
    end.

    if loan.open-date gt in-op-date then do:
        message 'Договор еще не открыт' view-as alert-box error.
        return "no-apply".
    end.

    if loan.contract eq 'dps' then do:
       {chktype.i}
       &if defined(Do_Trans) &then
       run put-loan in h_templ (recid(loan)).
       &endif
       Set_type(loan.cont-code).
    end.
    &IF DEFINED(MOD_DPS)
    &THEN
    else Set_loan(loan.contract,loan.cont-code).
    &ENDIF

    IF AVAIL op-entry AND 
    op-entry.currency eq ? then op-entry.currency = loan.currency.
    /* Здесь нужно проверить мультивалютность вклада 
       (если это подкласс мультивалютного и у него есть подчиненные вклады) */
    IF LOOKUP("Мультивалютный", GetXclassAllParents(loan.class-code)) > 0 AND 
       CAN-FIND(FIRST b-loan WHERE b-loan.parent-contract  = "dps" 
                               AND b-loan.parent-cont-code = loan.cont-code) THEN DO:
      in-cont-cur = "".
      ENABLE in-cont-cur.
      DISPLAY in-cont-cur-label in-cont-cur.
    END.
    ELSE DO:
      in-loan = loan.doc-ref .
      in-cont-code = in-loan .
      in-cont-cur = if loan.currency = '' then '810' else loan.currency .
      DISABLE in-cont-cur.
      HIDE in-cont-cur-label in-cont-cur in-loan in-loan-label.
    END. 
  END. /* DO WITH FRAME */
END.



PROCEDURE VCONTRACTDATE-LEAVE-POST:
    do with frame opreq:

        if (loan.contract eq 'Кредит' or
            loan.contract eq 'Депоз') THEN DO:

            IF loan.since     ne  date(vContractDate:screen-value)
            then do:

                {messmenu
                    &text    = "Договор не пересчитан на плановую дату операции"
                    &choices = "Пересчитать договор,Продолжить без пересчета,Отмена"
                    &update  = 1
                }

                if pick-value eq "3" or
                   pick-value eq "4"
                then return "POST-ERROR".
                else if pick-value eq "1"
                then do:

                    /* Перерасчет параметров одного договора. */

                    if work-module eq "loan"
                    then do:

                        {loancalc.i
                            &incontr = "loan.contract"
                            &incontc = "loan.cont-code"
                            &dr      = "date(vContractDate:screen-value)"
                        }
                    end.

                    /* Определяется переменные для модуля */
                    else run loancalc.p (loan.contract,
                                         loan.cont-code,
                                         date(vContractDate:screen-value)).
                end.
                ELSE if loan.since GT date(vContractDate:screen-value)  /* pick-value eq "2" - "Продолжить без пересчета" */
                then do:            /* плановая дата меньше даты пересчета договора */
                    MESSAGE "Плановая дата операции не может быть меньше даты пересчета договора" VIEW-AS ALERT-BOX.
                    return "POST-ERROR".  /* продолжение невозможно */
                END.
             end.
        END.
    end.

END PROCEDURE.

&endif


&IF defined(likeDiaSoft) &THEN
ON VALUE-CHANGED OF Op.Ben-Acct DO:
   &IF DEFINED(EmptyBenAcctOK) = 0 &THEN
   ASSIGN
      op.name-ben:SCREEN-VALUE = ""
      op.inn:SCREEN-VALUE      = ""
   .
   &ENDIF
END.

PROCEDURE OnF1_Op.Ben-Acct:
   DEFINE VARIABLE vRecipCode AS CHARACTER NO-UNDO.
   DO WITH FRAME opreq:
      IF op.ben-acct:SCREEN-VAL NE ""
      THEN RUN cli-ds.p ("", 1, INPUT STRING(INT64(INPUT {&SEL-MFO}), "999999999") + "," + op.ben-acct:SCREEN-VAL, 4).
      ELSE RUN cli-ds.p ("", 1, INPUT STRING(INT64(INPUT {&SEL-MFO}), "999999999"), 4).
      IF (LAST-EVENT:FUNCTION = "GO" OR LAST-EVENT:FUNCTION = "RETURN") AND pick-value <> ? THEN DO:
         RUN GetRecipientValue IN h_cust (ENTRY(1,pick-value),
                                          GetEntries(2,pick-value,",",""),
                                          GetEntries(3,pick-value,",",""),
                                          "БИК,РАСЧ_СЧЕТ,ИМЯ,ИНН,КПП",
                                          OUTPUT vRecipCode
                                         ).
         IF GetEntries(1,vRecipCode,CHR(2),"") <> "" THEN 
         DO:
            DISPLAY
            GetEntries(1,vRecipCode,CHR(2),"") @ {&SEL-MFO}
            GetEntries(2,vRecipCode,CHR(2),"") @ op.ben-acct
            GetEntries(3,vRecipCode,CHR(2),"") @ op.name-ben
            GetEntries(4,vRecipCode,CHR(2),"") @ op.inn
         .
            UpdateSigns("op",string(op.op),"Kpp-rec",GetEntries(5,vRecipCode,CHR(2),""),NO).
         END.
      END.
   END.
   RETURN "NO-APPLY".
END PROCEDURE.

PROCEDURE getScreenValue:
  DEF OUTPUT PARAM scrval AS CHAR NO-UNDO.
  DEF VAR wh AS WIDGET-HANDLE NO-UNDO.
  ASSIGN
    wh     = FRAME opreq:FIRST-CHILD
    wh     = wh:FIRST-CHILD
    scrval = ""
  .
  DO WHILE VALID-HANDLE(wh):
    IF CAN-QUERY(wh, "SCREEN-VALUE") THEN scrval = scrval + wh:SCREEN-VALUE + CHR(255).
    wh = wh:NEXT-SIBLING.
  END.
END.

PROCEDURE OnEntry_Frame.opreq:
  IF screen-value = "" THEN RUN getScreenValue(OUTPUT screen-value).
  basta = no.
END.

PROCEDURE OnEnd-Error_Frame.opreq:
  DEF VAR choice AS LOGICAL NO-UNDO.
  DEF VAR t-scrval AS CHAR NO-UNDO.
  RUN getScreenValue(OUTPUT t-scrval).
  IF t-scrval <> screen-value THEN DO:
    MESSAGE "Данные были изменены. Выйти без сохранения?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.
    IF choice <> yes THEN RETURN "NO-APPLY".
    basta = yes.
  END.
END.
&ENDIF

{frmfield.fun
   &DefTriggerFunction = YES
   &DefProcCheckCustomField = YES
}

PROCEDURE OnGo_Frame.opreq.
  DEF VAR t-details LIKE op.details NO-UNDO.

   def var flag-error as INT64 no-undo.

   DEFINE VARIABLE vHandle AS HANDLE    NO-UNDO.
   DEFINE VARIABLE mAddr   AS CHARACTER NO-UNDO.   

   /* Контролируем ввод нулевой суммы в QBIS */
   &IF DEFINED(SESSION-REMOTE) <> 0 
   AND DEFINED(browse-entry)    = 0
   &THEN 
      RUN OnLeave_Op-Entry.Amt-Rub NO-ERROR.
      IF RETURN-VALUE = "NO-APPLY"
         THEN RETURN RETURN-VALUE.
   &ENDIF
   
   &IF DEFINED(SESSION-REMOTE) <> 0
   AND DEFINED(browse-entry)    = 0
   AND DEFINED(NOaccts)         = 0
   &THEN
   DEFINE VAR vNextWidget AS HANDLE NO-UNDO.
   IF VALID-HANDLE(FOCUS)
   AND (   FOCUS:NAME = "acct-db"
        OR FOCUS:NAME = "acct-cr")
   THEN DO:
      RUN GetNextTabWidget (FOCUS, OUTPUT vNextWidget).
      IF VALID-HANDLE(vNextWidget) THEN DO:
         RUN SilentEntry (vNextWidget).
         RETURN "NO-APPLY".
      END.
   END.
   &ENDIF

   &IF DEFINED(browse-entry) = 0
   AND DEFINED(NOaccts)      = 0
   &THEN
      IF op-entry.acct-db:SENSITIVE IN FRAME opreq
      THEN DO:
         g_trig_i_no_parse = YES.
         RUN OnLeave_Op-Entry.Acct-DB.
         g_trig_i_no_parse = NO.
         IF RETURN-VALUE = "NO-APPLY"
         THEN RETURN RETURN-VALUE.
      END.
      IF op-entry.acct-cr:SENSITIVE IN FRAME opreq
      THEN DO:
         g_trig_i_no_parse = YES.
         RUN OnLeave_Op-Entry.Acct-CR.
         g_trig_i_no_parse = NO.
         IF RETURN-VALUE = "NO-APPLY"
         THEN RETURN RETURN-VALUE.
      END.
   &ENDIF

   &IF DEFINED(NOmfo) eq 0 &THEN
      IF {&SEL-MFO}:SENSITIVE  IN FRAME opreq AND
         op.ben-acct:SENSITIVE IN FRAME opreq THEN
      DO:
         RUN OnLeave_Op.Ben-Acct.
         IF RETURN-VALUE EQ "no-apply" THEN RETURN "NO-APPLY".
      END.

      IF AVAIL wop THEN DO:
         DO WITH FRAME opreq:
            &GLOBAL-DEFINE iTrigOnGo  YES
            {g-bank.lv "{&SEL-MFO}" "{&SEL-CORR}" "op.ben-acct" ""NO-APPLY""}
            &UNDEFINE iTrigOnGo
         END.

         DO WITH FRAME opreq:
            {g-corr.lv {&SEL-CORR} ""NO-APPLY""}
         END.
      END.
   &ENDIF
   &IF DEFINED(browse-entry) = 0 &THEN
      RUN OnLeave_Op-entry.symbol.
      IF RETURN-VALUE EQ "NO-APPLY" THEN
      DO:
         APPLY "ENTRY" TO op-entry.symbol IN FRAME opreq.
         RETURN "NO-APPLY".
      END.
   naimks = op-entry.symbol:SCREEN-VALUE IN  FRAME opreq.
   &ENDIF

   if lookup("ProcessDetails", this-procedure:internal-entries) > 0 
      AND AVAIL op  
    then do:
      assign
         t-details = op.details:SCREEN-VALUE in frame opreq.
      run ProcessDetails (if avail wop then recid(wop) else ?, input-output t-details).
      ASSIGN
         op.details:SCREEN-VALUE IN FRAME opreq = t-details
   &IF DEFINED(DoLoan) EQ 0 &THEN
         op.details = t-details
   &ENDIF
      .
   end.

    /* проверяем категорию клиента по счету дебета для формирования доп.рекв
       адреса и телефона получателя в счете-фактуре */
   IF AVAILABLE dacct THEN
   CASE dacct.cust-cat:
      WHEN "В":U THEN DO: /* внутрибанк (уже установлен в OnF1_Op.Name-Ben) */
      END.
      WHEN "Ю":U THEN DO: /* юрик */
         FIND cust-corp WHERE cust-corp.cust-id = dacct.cust-id NO-LOCK NO-ERROR.
         IF AVAILABLE cust-corp THEN DO:
            RUN GetCustAdr IN h_cust ("Ю",
                                      cust-corp.cust-id,
                                      gend-date,
                                       "АдрЮр",
                                       OUTPUT TABLE ttCustAddress).
            FIND LAST ttCustAddress NO-LOCK NO-ERROR.
            IF AVAILABLE ttCustAddress AND {assigned ttCustAddress.AdrStr} THEN
               mAddr = FrmtAddrStr(ttCustAddress.AdrStr,ttCustAddress.fCodReg).
            ELSE  
               mAddr = cust-corp.addr-of-low[1] + " " + cust-corp.addr-of-low[2].            

            RUN SetSysConf IN h_base("g-trig - Адрес_пок", mAddr) NO-ERROR.             
            RUN SetSysConf IN h_base("g-trig - Телеф_пок", cust-corp.fax) NO-ERROR.
         END.
      END.
      WHEN "Б":U THEN DO: /* банк */
         FIND banks WHERE banks.bank-id = dacct.cust-id NO-LOCK NO-ERROR.
         IF AVAILABLE banks THEN DO:
            RUN GetCustAdr IN h_cust ("Б",
                                      banks.bank-id,
                                      gend-date,
                                       "*",
                                      OUTPUT TABLE ttCustAddress).
            FIND LAST ttCustAddress NO-LOCK NO-ERROR.
            IF AVAILABLE ttCustAddress AND {assigned ttCustAddress.AdrStr} THEN
               mAddr = FrmtAddrStr(ttCustAddress.AdrStr,ttCustAddress.fCodReg).
            ELSE  
               mAddr = banks.country + ", " + banks.town-type + " " + banks.town + ", " + banks.law-address.         
            RUN SetSysConf IN h_base("g-trig - Адрес_пок", mAddr) NO-ERROR.             
            RUN SetSysConf IN h_base("g-trig - Телеф_пок", "-") NO-ERROR.
         END.
      END.
      WHEN "Ч":U THEN DO: /* физик */
         FIND person WHERE person.person-id = dacct.cust-id NO-LOCK NO-ERROR.
         IF AVAILABLE person THEN DO:
            RUN GetCustAdr IN h_cust ("Ч",
                                      person.person-id,
                                      gend-date,
                                       "АдрФакт",
                                      OUTPUT TABLE ttCustAddress).
            FIND LAST ttCustAddress NO-LOCK NO-ERROR.
            IF AVAILABLE ttCustAddress AND {assigned ttCustAddress.AdrStr} THEN
               mAddr = FrmtAddrStr(ttCustAddress.AdrStr,ttCustAddress.fCodReg).
            ELSE  
               mAddr = person.address[1] + " " + person.address[2].               
            RUN SetSysConf IN h_base("g-trig - Адрес_пок", mAddr) NO-ERROR.   
            RUN SetSysConf IN h_base("g-trig - Телеф_пок",
                                     person.phone[1] + " " + person.phone[2]) NO-ERROR.
         END.
      END.
   END CASE.
   
   &IF DEFINED(NOben) = 0 &THEN
   IF AVAIL op THEN
   DO:   
      RUN OnLeave_Op.Inn.
      IF RETURN-VALUE EQ "NO-APPLY":U THEN DO:
         APPLY "ENTRY":U TO op.inn IN FRAME opreq.
         RETURN "NO-APPLY":U.
      END.
   END.
   &ENDIF

   IF AVAIL op THEN
   DO:   
   DEF VAR wh AS WIDGET-HANDLE NO-UNDO.
   RUN CheckFields in h_xclass (FRAME opreq:HANDLE, "",op.class-code,OUTPUT wh).
   IF RETURN-VALUE <> "" THEN DO:
      APPLY "entry" TO wh.
      RETURN "NO-APPLY".
   END.

   vHandle =  FRAME opreq:FIRST-CHILD.
   vHandle =  vHandle:FIRST-CHILD.

   DO WHILE VALID-HANDLE(vHandle):
      IF CAN-QUERY(vHandle,"SCREEN-VALUE") THEN
         RUN CreateFrmFields (IF AVAIL op-templ THEN op-templ.op-templ ELSE ?,
                              "opreq",
                              vHandle:NAME,
                              vHandle:SCREEN-VALUE).
      vHandle =  vHandle:NEXT-SIBLING.
   END.

   IF NOT CAN-FIND(FIRST code WHERE code.class EQ "order-pay" 
                                AND code.CODE  EQ &IF DEFINED(OFcash) = 0 &THEN op.order-pay:SCREEN-VALUE &ELSE op.order-pay &ENDIF
                                AND ((NOT {assigned code.val}) OR 
                                     (NOT {assigned op.doc-date:SCREEN-VALUE}) OR 
                                     DATE(code.val) GE DATE(op.doc-date:SCREEN-VALUE))) 
   THEN DO:
      &IF DEFINED(OFcash) = 0 &THEN
         MESSAGE "Код очередности платежа недействителен на дату" op.doc-date:SCREEN-VALUE 
                 VIEW-AS ALERT-BOX ERROR BUTTON OK.
         APPLY 'ENTRY' TO op.order-pay.
         RETURN "NO-APPLY".
      &ELSE
         FIND LAST code WHERE code.class EQ "order-pay"
                          AND ((NOT {assigned code.val}) OR 
                               (NOT {assigned op.doc-date:SCREEN-VALUE}) OR 
                               DATE(code.val) GE DATE(op.doc-date:SCREEN-VALUE))  
         NO-LOCK NO-ERROR. 
         IF AVAIL code THEN 
            op.order-pay = code.code.
         ELSE DO:
            MESSAGE "Нет ни одного кода очередности платежа, действительного на дату" op.doc-date:SCREEN-VALUE 
                    VIEW-AS ALERT-BOX ERROR BUTTON OK.
            RETURN "NO-APPLY".
         END.
      &ENDIF
   END.

   END.   /*есть op*/
            
&IF DEFINED(browse-entry) = 0 &THEN
&IF DEFINED(NOcurr) = 0 &THEN
   g_trig_i_is_recount = YES.
   RUN OnLeave_Op-Entry.Amt-Cur.
   g_trig_i_is_recount = NO.
&ENDIF
&ENDIF
END PROCEDURE.

&IF DEFINED(NOben) = 0 &THEN
PROCEDURE OnLeave_Op.Inn:
   DO WITH FRAME opreq:
      /* если заполнен ИНН, проверим его */
      IF INPUT op.inn GT "" THEN DO:
         /* если проверка отключена, то не пристаем */
         IF GetXAttrValueEx("op-kind", op.op-kind, "UseINN", "Нет") EQ "Нет" THEN
            RETURN.
         DEFINE VARIABLE choose-button AS LOGICAL INIT YES NO-UNDO.
         DEFINE VARIABLE mchCorrectInnSign AS CHAR NO-UNDO.
         IF NOT fValidInnSignature(
                   INPUT  INPUT op.inn,
                   OUTPUT mchCorrectInnSign)
         THEN DO:
            IF mchCorrectInnSign = "" THEN 
               MESSAGE "Неверная длина ИНН получателя."
                        SKIP "Будете исправлять?"
               VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choose-button.         
            ELSE
               MESSAGE "Последние цифры ИНН должны быть: ~"" + mchCorrectInnSign + "~""
                       SKIP "Будете исправлять?"
               VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choose-button.
            IF choose-button OR choose-button EQ ? THEN DO:
               RETURN "NO-APPLY":U.
            END.
         END.
      END.
   END.
END.
&ENDIF /* DEFINED(NOben) = 0 */

PROCEDURE OnSpace_Op.Doc-Type:
  IF NUM-ENTRIES(op-templ.doc-type) > 1 THEN DO WITH FRAME opreq:
    DISPLAY ENTRY(IF LOOKUP(op.doc-type:SCREEN-VALUE, op-templ.doc-type) > 0 THEN LOOKUP(op.doc-type:SCREEN-VALUE, op-templ.doc-type) + 1 ELSE 1, op-templ.doc-type + "," + ENTRY(1, op-templ.doc-type)) @ op.doc-type.
    RETURN "NO-APPLY".
  END.
END.

procedure OnLeaveAssignBuff.
   def var frm as handle.
   hide message no-pause.
   IF NOT CAN-QUERY(self, "frame") THEN RETURN.
   IF NOT CAN-QUERY(self, "name") THEN RETURN.
   IF NOT CAN-QUERY(self, "SCREEN-VALUE") THEN RETURN.
   assign frm = self:frame.
/*  message ":" self:frame ":" self:name ":" self:SCREEN-VALUE ":" view-as alert-box. */
   RUN CreateFrmFields (IF AVAIL op-templ THEN op-templ.op-templ ELSE ?,
                        if valid-handle(frm) then frm:name else ?,
                        SELF:NAME,
                        SELF:SCREEN-VALUE).
   IF AVAIL loan AND NOT CheckCustomField (IF AVAIL op-templ THEN op-templ.op-templ ELSE ?,
                                           "LoanRecid", "") THEN DO:
      RUN CreateFrmFields (IF AVAIL op-templ THEN op-templ.op-templ ELSE ?,
                           "LoanRecid", "",
                           STRING(RECID(loan))).
   END.
end procedure.

ON "GO",ctrl-g OF FRAME opreq DO:
   {realtrig.i &custom="OnGo_Frame.opreq"}
&IF DEFINED(submit-person-id) NE 0 &THEN
   DEFINE VARIABLE vSysConfVal AS CHARACTER NO-UNDO.
   vSysConfVal = GetSysConf("ПостоянныйПолучатель").
   IF NOT {assigned vSysConfVal} THEN
   DO:
      vSysConfVal = (IF AVAILABLE person
                     THEN STRING(person.person-id)
                     ELSE ?).
      RUN SetSysConf IN h_Base ("ПостоянныйПолучатель", vSysConfVal).
   END.
&ENDIF
END.

ON 'F2':U OF FRAME opreq DO:
   IF GetXAttrValue ("op-kind", op-kind.op-kind, "РучВводНР") EQ "ДА" THEN DO:

      &IF DEFINED(ORACLE) &THEN
         IF AVAIL(op-entry) THEN
         VALIDATE op-entry NO-ERROR.
      &ENDIF

      RUN nalpl_ed.p (RECID(op), 3, {&ROW} + 1).
   END.
   RETURN NO-APPLY.
END.
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='22/07/2015 11:07:19.476+04:00' */
/* $LINTUSER='kuds' */
/* $LINTMODE='1' */
/* $LINTFILE='g-trig.i' */
/*prosignG11Ar0OWaoF0Vq2kNqcNWA*/