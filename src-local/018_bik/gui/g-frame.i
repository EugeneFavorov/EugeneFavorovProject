
/* +++ g-frame.i was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:23am +++ */

/*
               Банковская интегрированная система БИСквит
    copyright: (c) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: G-FRAME.I
      Comment: форма ввода документа
   Parameters: нет
         Uses:
      Used by:
      Created: ...
     Modified: 06/12/1998 Olenka - Заключительные обороты. Конечная дата их
                                   ввода берется из настроечного параметра
                                   ДатаЗО.
     Modified: 03.12.2001 12:04 SEMA     по заявке 0003724
     Modified: 08/12/2001 shin Индикация потокового ввода + возможность динамически его изменить
     modified: 27/02/2002 kostik 0001323 Поля op-entry.amt-rub и op-entry.amt-cur инициализировались сразу
                                         после вызова parssen, что влияло на отображение остатка по счетам
                                         Дебета и Кредита. Сейчас редактирование идет через табличку WOP.
     modified: 29/06/2002 kostik 0003150 (0008150)
                                 Для разрешения редактирования суммы в класс
                                 "doc-templ" добавлен доп. реквизит.
                                 "EditAmtFlag". Раньше если сумма определена в
                                 шаблоне или парсером, то редактирование
                                 блокировалось, теперь можно разрешить,
                                 установив реквизит в значение "Да"
     modified: 23/09/2002 kraw (0008428) - Обработка "EditAmtFlag" и в случае, если OFSum не установлен,
                                           но по умолчанию "Да"
     Modified: 11.11.2002 15:26 SEMA     по заявке 0011932 код, использовавший таблицу frm-field вынесен в frmfield.fun
     Modified: 22.11.2002 20:43 rija     1734
     Modified: 17.01.2003 16:15 kavi     0013352  увеличен формат поля in-cont-code до 60 и добавлен скроллинг
     Modified: 05.02.2003 11:10 KAVI
     Modified: 9/02/2004 fEAk з. 13798 - Реализована функциональность, для работы опер. касс в 
                                          выходные дни в части операций по обслуживанию физ. лиц.               
     Modified: 09.02.2004 18:32 FEAK
     Modified: 04.03.2004 10:24 rija
     Modified: 04.03.2004 10:45 rija     24138: Реализовать кассовую операцию "Прием наличных
                                         денежных средств на деп. счета".
     Modified: 07.07.2004       abko     19280: Плохая нумерация документов, если поле op.op 
                                         больше 1000000.
     Modified: 22/09/2004  Om    Доработка, обработка флага окончания транзакции.
     Modified: 07.04.2005 12:26 SAP
     Modified: 23.06.2006 15:54 fEAk     <comment>
     Modified: 22/05/2009 kraw (0102904) аккуратная автонумерация
     Modified: 06.10.2009 19:03 ksv      (0118168) QBIS незнач. исправление 
     Modified: 12.08.2010 19:57 ksv      (0127883) QBIS доработка пред. исправления
*/

&IF DEFINED (VokTitle) = 0 &THEN
&GLOBAL-DEFINE VokTitle
{voktitle.i}
&ENDIF
RUN id_vok_just (OUTPUT vChIdVokJust).

&IF DEFINED(DoFrame) &THEN
   DEF VAR fl_SearchOP     AS LOGICAL NO-UNDO.
   DEF VAR fl_editOP       AS LOGICAL NO-UNDO.
   DEF VAR atempl          AS CHARACTER NO-UNDO.   
   DEF VAR dt-zo           AS DATE   NO-UNDO.
   DEF VAR fl_edit_rub     AS LOG    NO-UNDO.
   DEF VAR fl_edit_cur     AS LOG    NO-UNDO.
   DEF VAR flag-edit-summ  AS LOG    NO-UNDO.
   DEF VAR flag-edit-acct  AS LOG    NO-UNDO.
   DEF VAR mDataZoStr      AS CHAR   NO-UNDO.
   DEF VAR mDsplSet        AS LOG    NO-UNDO
                           INIT YES. 
   DEF VAR mSetCntxt4Loan  AS LOG    NO-UNDO. /* Есть предустановка контекста по LOAN. */

   DEFINE BUFFER bOp-templ FOR op-template.

   {objopkind.def}      /* Контекст объекта. */
                        /* Получение контекста объекта.
                        ** Для случая, если транзакция запущена от объекта. */
   PUBLISH "GetObjectContent" (OUTPUT DATASET dsObjOpKind).

   &IF DEFINED (DoLoan) &THEN
      FOR FIRST ttObjLoan:
         ASSIGN 
            in-contract    = ttObjLoan.contract    WHEN  LENGTH (ttObjLoan.contract)   GT 0
            in-cont-code   = ttObjLoan.cont-code   WHEN  LENGTH (ttObjLoan.cont-code)  GT 0
            in-doc-ref     = ttObjLoan.doc-ref     WHEN  LENGTH (ttObjLoan.doc-ref)    GT 0
            in-cont-cur    = ttObjLoan.currency    WHEN  LENGTH (ttObjLoan.currency)   GT 0
            mSetCntxt4Loan = YES
         .
      END.
   &ENDIF
   FOR FIRST ttObjOp:
      IF ttObjOp.contract-date NE ? THEN
         vContractDate = ttObjOp.contract-date.
   END.

   mDataZoStr = FGetSetting("ДатаЗо",?,"").
   dt-zo = if mDataZoStr NE "" 
           then date(INT64(substr(mDataZoStr,4,2)),
                     INT64(substr(mDataZoStr,1,2)),
                     year(in-op-date))
           else date(3,31,year(in-op-date)).
   &IF DEFINED(browse-entry) &THEN
      DEFINE QUERY q1 FOR op-entry.
      DEFINE BROWSE b1 QUERY q1
      DISPLAY
         op-entry.acct-db
         op-entry.acct-cr
         op-entry.currency
         op-entry.amt-cur
         op-entry.amt-rub
         /* enable all */
         WITH 3 DOWN WIDTH 76 TITLE "[ ПРОВОДКИ ]" SEPARATORS
            NO-ROW-MARKERS.
   &ENDIF

   FORM
      &IF DEFINED(DoLoan) &THEN
      "НОМЕР ДОГОВОРА" /*in-cont-code*/ in-doc-ref FORMAT "x(60)" VIEW-AS FILL-IN SIZE 25 BY 1
      "ПЛАНОВАЯ ДАТА" vContractDate
      in-cont-cur-label FORMAT "x(3)" VIEW-AS FILL-IN SIZE 3 BY 1 
      in-cont-cur FORMAT "x(3)" VIEW-AS FILL-IN SIZE 3 BY 1
      SKIP
      &ENDIF
      "ДОКУМЕНТ" op.doc-type ":" doc-type.name "N" op.doc-num "ОТ" at 64 op.doc-date at 69 SKIP
      need-valdate op.op-value-date "Поступил" op.ins-date    
      &IF DEFINED(OFcash) = 0 &THEN
      "Очер.плат."  AT 51  op.order-pay FORMAT "x(2)"  HELP "Очередность платежа" at 61
      "Срок"  op.due-date HELP "Срок платежа" SKIP
      &ENDIF
      &IF DEFINED(DoLoan) &THEN
      &IF DEFINED(NO_LOAN_LABEL) = 0 &THEN
      in-loan-label FORMAT "x(7)" in-loan FORMAT "x(20)" VIEW-AS FILL-IN SIZE 20 BY 1 SKIP
      &ENDIF
      &ENDIF
      &IF DEFINED(browse-entry) = 0 &THEN
      "═════════════════════════════════════╦════════════════════════════════════════"
      "ДЕБЕТ:" op-entry.acct-db  "║КРЕДИТ:" AT 38 op-entry.acct-cr SKIP
      " " name-db[1] "║ " AT 38 name-cr[1] SKIP
      " " name-db[2] "║ " AT 38 name-cr[2] SKIP
      "ОСТАТОК:" bal-db LIKE acct-pos.balance "║ОСТАТОК:" AT 38 bal-cr LIKE acct-pos.balance SKIP
      "═════════════════════════════════════╩════════════════════════════════════════"
      "КУРС НА   " "ВАЛ" "{&in-ua-amtfc}     {&in-uf-amtncn}" &IF DEFINED(ONqty) <> 0 &THEN "  КОЛ-ВО" &ENDIF "КС" "ЗО" "КО" SKIP
      op-entry.value-date HELP "Дата курса, на которую рассчитан эквивалент в нац. валюте"
      &IF DEFINED(casho) <> 0 &THEN
         op-entry.currency AT 12 op-entry.amt-cur format ">>>,>>>,>>>,>>9.999" AT 17 op-entry.amt-rub format ">>>,>>>,>>>,>>9.999" AT 38
      &ELSE
         op-entry.currency AT 12 op-entry.amt-cur AT 17 op-entry.amt-rub
      &ENDIF
      &IF DEFINED(ONqty) <> 0 &THEN
      op-entry.qty format ">>>>9.99"
      &ENDIF
      &IF DEFINED(g_cash1_sym) <> 0 &THEN
      mSymbol
      &ELSE 
      op-entry.symbol
      &ENDIF
      op-entry.prev-year op-entry.op-cod format "x(6)" SKIP
      &ENDIF
      &IF DEFINED(likeDiaSoft) = 0 &THEN
      &IF DEFINED(NOben) = 0 &THEN
      "Клиент:" op.name-ben VIEW-AS FILL-IN SIZE 51 BY 1 "ИНН:" op.inn
      &ENDIF
      &ENDIF
      &IF DEFINED(NOmfo) = 0 &THEN
      "═[" doc-kind format "x(18)" "]═══════════════════════════════════════════════════════"
      "БИК:" vmfo help "Идентификационный код банка" bank1.name FORMAT "x(47)" AT 32 SKIP
      "К/с:" vcorr-acct bank2.name FORMAT "x(47)" AT 32 SKIP
      "Р/с:" op.ben-acct
      &ENDIF
      &IF DEFINED(likeDiaSoft) &THEN
      &IF DEFINED(NOben) = 0 &THEN
      SKIP "Клиент:" op.name-ben VIEW-AS FILL-IN SIZE 51 BY 1 "ИНН:" op.inn
      &ENDIF
      &ENDIF
      "═[ СОДЕРЖАНИЕ ОПЕРАЦИИ ]═════════════════[ F1, F3-пред., F4-след., CTRL-F10 ]═"
      op.details VIEW-AS EDITOR INNER-CHARS 78 
      INNER-LINES 3
      &IF DEFINED(browse-entry) &THEN
      "══════════════════════════════════════════════════════════════════════════════"
      b1
      &ENDIF
   WITH FRAME opreq 1 DOWN OVERLAY CENTERED NO-LABEL ROW {&row} TITLE COLOR bright-white
   "[ ОПЕРАЦИЯ : " + op-kind.name + "; ЗА " + STRING(in-op-date, "99/99/99") + vChIdVokJust + " ]".

   &IF DEFINED(cycle) ne 0 &THEN
      wh_opr = frame opreq:handle. /* Присваение указателя на фрейм документа */
   &ENDIF

&ELSEIF DEFINED(DoDisp) &THEN
   &IF DEFINED(SESSION-REMOTE) > 0
   &THEN
      &GLOB NODISPLAY YES
   &ENDIF

   &IF DEFINED(ChkBlockAction) = 0 &THEN
      {chkblock.i
         &surr   = STRING(in-op-date)
         &msg    = "Вы не имеете права работать в заблокированном операционном дне!"
         &action = "PAUSE. HIDE FRAME opreq. UNDO cycle, LEAVE cycle."
      }
   &ELSE
      {chkblock.i
         &surr   = STRING(in-op-date)
         &msg    = "Вы не имеете права работать в заблокированном операционном дне!"
         &action = "{&ChkBlockAction}"
      }
   &ENDIF
   
   IF op-template.doc-type BEGINS "(" THEN DO:               
      
      atempl = TRIM(op-template.doc-type,"(").
      atempl = TRIM(atempl,")").

      FIND FIRST bOp-templ WHERE bOp-templ.op-kind EQ op-templ.op-kind AND 
                                 bOp-templ.op-templ EQ INT(atempl) NO-ERROR.
      op.doc-type = ENTRY(1,bOp-templ.doc-type).
          
   END.

   DISPLAY
      &IF DEFINED(DoLoan) &THEN
         /*in-cont-code*/ in-doc-ref
         vContractDate
         in-cont-cur
      &ENDIF
      ENTRY(1, op-templ.doc-type) WHEN NUM-ENTRIES(op-templ.doc-type) > 1 AND NOT (AVAIL(bop-templ) AND op-template.doc-type BEGINS "(") @ op.doc-type
      op.doc-type WHEN op.doc-type <> "" and (num-entries(op-templ.doc-type) = 1 OR (AVAIL(bop-templ) AND op-template.doc-type BEGINS "("))
      doc-type.name WHEN AVAIL doc-type
      op.doc-num  WHEN op.doc-num <> "" AND op.doc-num:SCREEN-VALUE IN FRAME opreq EQ ""
      op.doc-num:SCREEN-VALUE IN FRAME opreq WHEN op.doc-num  EQ "" AND op.doc-num:SCREEN-VALUE IN FRAME opreq NE ""
      op.doc-date WHEN op.doc-date <> ?
      op.ins-date
      need-valdate
      op.op-value-date  WHEN need-valdate
      &IF DEFINED(OFcash) = 0 &THEN
         op.order-pay
         op.due-date
      &ENDIF
      &IF DEFINED(browse-entry) = 0 &THEN
          &IF DEFINED(ONqty) &THEN
              "" @ op-entry.qty
             op-entry.qty when need-qty
          &ENDIF
      &ENDIF
      &IF DEFINED(NOben) = 0 &THEN
         op.name-ben WHEN op.name-ben <> ?
         &IF DEFINED(SESSION-REMOTE) &THEN
            op.name-ben:INPUT-VALUE WHEN op.name-ben = ? @ op.name-ben
         &ENDIF
         op.inn WHEN op.inn <> ?
         &IF DEFINED(SESSION-REMOTE) &THEN
            op.inn:INPUT-VALUE WHEN op.inn = ? @ op.inn
         &ENDIF
      &ENDIF
      &IF DEFINED(browse-entry) = 0 &THEN
         op-entry.acct-db WHEN op-entry.acct-db <> ?
         op-entry.acct-cr WHEN op-entry.acct-cr <> ?
         wop.amt-cur      WHEN AVAIL wop AND wop.currency NE "" AND wop.amt-cur NE ? @ op-entry.amt-cur
         0                WHEN AVAIL wop AND wop.currency NE "" AND wop.amt-cur EQ ? @ op-entry.amt-cur
         wop.amt-rub      WHEN AVAIL wop AND wop.amt-rub NE ?    @ op-entry.amt-rub
         0                WHEN AVAIL wop AND wop.amt-rub EQ ?    @ op-entry.amt-rub
         op-entry.amt-cur WHEN NOT AVAIL wop AND op-entry.currency NE ""
         op-entry.amt-rub WHEN NOT AVAIL wop
         op-entry.value-date
         op-entry.currency WHEN op-entry.currency <> ?
         &IF DEFINED(g_cash1_sym) <> 0 &THEN
         mSymbol WHEN op-templ.symbol <> "-"
         &ELSE
         op-entry.symbol WHEN op-templ.symbol <> "-"
         &ENDIF
         op-entry.op-cod
      &ENDIF
      op.details
      &IF DEFINED(Nomfo) = 0 &THEN
         vcorr-acct vmfo
         op.ben-acct
         doc-kind
      &ENDIF
   .
   &IF DEFINED(browse-entry) &THEN
      OPEN QUERY q1 FOR EACH op-entry OF op.
      ENABLE b1 WITH FRAME opreq.
   &ENDIF

   /* ПОТОК */
   &IF DEFINED(cycle) ne 0 &THEN
      {g-cycle.dsp}
   &ENDIF

&ELSEIF DEFINED(DoSet) &THEN
   &IF DEFINED(DoLoan) &THEN
      IF in-doc-ref GT '' THEN
      DO WITH FRAME opreq:
         vContractDate:SENSITIVE = YES.
         vContractDate:READ-ONLY = YES.
         vContractDate:PFCOLOR   = 3.
      END.
   &ENDIF
   RUN InitFormatFrameFields IN h_xclass(FRAME opreq:HANDLE, "", "op-template", op-kind.op-kind + "," + STRING(op-templ.op-templ)).
   &IF DEFINED(browse-entry) = 0 &THEN
      IF  {assigned op-entry.currency}
      AND NOT CAN-FIND(FIRST acct WHERE acct.acct     EQ      op-entry.acct-cr
                                    AND acct.currency EQ      ""
                                    AND acct.contract MATCHES "*Касса*")
      AND NOT CAN-FIND(FIRST acct WHERE acct.acct     EQ      op-entry.acct-db
                                    AND acct.currency EQ      ""
                                    AND acct.contract MATCHES "*Касса*")
      THEN DO:
         &IF DEFINED(g_cash1_sym) <> 0 &THEN
         mSymbol = "" .
         DISPLAY mSymbol WITH FRAME opreq.
         &ELSE 
         op-entry.symbol = "" .
         DISPLAY op-entry.symbol WITH FRAME opreq.
         &ENDIF
      END.
   &ENDIF
   flag-edit-acct = GetXAttrValueEx("op-template",
                                    op-template.op-kind + "," + STRING(op-template.op-template),
                                    "EditAcctFlag", &IF DEFINED(EdtFlg) EQ 0 &THEN "НЕТ" &ELSE {&EdtFlg} &ENDIF) EQ "Да".
   flag-edit-summ = GetXAttrValueEx("op-template",
                                    op-template.op-kind + "," + STRING(op-template.op-template),
                                    "EditAmtFlag",
                                    &IF DEFINED(Ofsum) EQ 0 &THEN 
                                    &IF DEFINED(EdtFlg) EQ 0 &THEN "НЕТ" &ELSE {&EdtFlg} &ENDIF
                                    &ELSE "ДА" &ENDIF
                                   ) EQ "ДА".
   &IF DEFINED(Ofsum) EQ 0 &THEN
      ASSIGN
         fl_edit_rub = (AVAIL wop AND (wop.amt-rub EQ ? OR wop.amt-rub EQ 0)) OR
                       (NOT AVAIL wop AND (op-entry.amt-rub EQ ? OR op-entry.amt-rub EQ 0)) OR
                       flag-edit-summ.
         fl_edit_cur = (AVAIL wop AND (wop.amt-cur EQ ? OR wop.amt-cur EQ 0)) OR
                       (NOT AVAIL wop AND (op-entry.amt-cur EQ ? OR op-entry.amt-cur EQ 0)) OR
                       flag-edit-summ
      .
   &ELSE
      ASSIGN
         fl_edit_rub = NO
         fl_edit_cur = NO
      .
   &ENDIF
   
   fl_editOP = &IF DEFINED(Regim-OneDoc) EQ 0 
               &THEN YES 
               &ELSE fl_SearchOP EQ NO 
               &ENDIF.

   /* Принудительно делаем вывод номера документа и очередности, чтобы SET
   ** сохранил эти изменения в QBIS  */
   &IF DEFINED( MANUAL-REMOTE ) &THEN  
   IF NOT RETRY THEN DISPLAY op.doc-num op.order-pay WITH FRAME opreq.
   &ENDIF
                        /* По требованию пользователя ввод данных может не производиться. */
   IF mDsplSet THEN
   DO:
      IF AVAIL(op) THEN
      DO:
         FIND FIRST code WHERE 
                  code.class EQ "acct-cat"
            AND   code.code  EQ op.acct-cat
         NO-LOCK NO-ERROR.
         IF AVAIL(code) 
            AND   code.description[3] NE "Нет" 
         THEN mNeedZO = YES. 
      END.
      {set.i &THIS_FRAME = "opreq" &EXFILE = "g-frame.i.st1" {&*}}.
   END.
   &IF DEFINED(Ofsum) EQ 0 AND DEFINED(browse-entry) = 0 &THEN
      ASSIGN
         op-entry.amt-rub = wop.amt-rub WHEN AVAIL wop AND NOT fl_edit_rub
         op-entry.amt-cur = wop.amt-cur WHEN AVAIL wop AND NOT fl_edit_cur
         wop.amt-rub      = op-entry.amt-rub WHEN AVAIL wop AND fl_edit_rub
         wop.amt-cur      = op-entry.amt-cur WHEN AVAIL wop AND fl_edit_cur
      .
   &ENDIF

   &IF DEFINED(DoLoan) &THEN
      IF in-doc-ref GT '' THEN
      DO WITH FRAME opreq:
         vContractDate:SENSITIVE = NO.
      END.
   &ENDIF

&ELSEIF DEFINED(DoBefore) &THEN

   CLEAR FRAME opreq NO-PAUSE.

   COLOR DISPLAY BRIGHT-GREEN
      name-db name-cr doc-type.name
      &IF DEFINED(NOmfo) = 0 &THEN
         bank1.name bank2.name
      &ENDIF
   WITH FRAME opreq.

   &IF DEFINED(NOmfo) = 0 &THEN
      COLOR DISPLAY input
         doc-kind
      WITH FRAME opreq.
   &ENDIF

   &IF DEFINED(DoLoan) &THEN
      IF vContractDate EQ ?
         THEN vContractDate = in-op-date.
   &ENDIF

   COLOR DISPLAY bright-white
      &IF DEFINED(DoLoan) &THEN
         /*in-cont-code*/ in-doc-ref
         in-cont-cur
         vContractDate
      &ENDIF
      op.doc-type op.doc-num op.doc-date op.ins-date
      &IF DEFINED(browse-entry) = 0 &THEN
         op-entry.acct-db op-entry.acct-cr op-entry.amt-cur
         op-entry.amt-rub 
         &IF DEFINED(g_cash1_sym) <> 0 &THEN
         mSymbol
         &ELSE
         op-entry.symbol
         &ENDIF
         op-entry.op-cod
      &ENDIF
      &IF DEFINED(NOmfo) = 0 &THEN
         vmfo vcorr-acct op.ben-acct
      &ENDIF
      &IF DEFINED(NOben) = 0 &THEN
         op.name-ben
      &ENDIF
      op.details
   WITH FRAME opreq.

   {get-fmt.i &nodeffmt="/*" &obj='" + op-templ.acct-cat + ""-Acct-Fmt"" + "'}
   &IF DEFINED(browse-entry) = 0 &THEN
      ASSIGN
         op-entry.acct-db:FORMAT IN FRAME opreq = fmt
         op-entry.acct-cr:FORMAT IN FRAME opreq = fmt
         cur-op-date = in-op-date
         &IF DEFINED(NOmfo) = 0 &THEN
            vmfo        = ""
            vcorr-acct  = ""
         &ENDIF
         rate-date   = in-op-date
      .
      &IF DEFINED(ONqty) <> 0 &THEN
         op-entry.qty:format = ">>>>9.99".
         IF need-qty THEN DO:
            FIND FIRST signs WHERE signs.FILE-NAME = "op-template" AND
                             signs.surrogate = op-kind.op-kind + "," + STRING(op-templ.op-templ) AND
                             signs.CODE = "QtyFormat" NO-LOCK NO-ERROR.
            IF AVAIL signs THEN DO:
               op-entry.qty:format = signs.xattr-val no-error.
               IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                  MESSAGE "Недопустимый формат для количества. Используется стандартный...".
                  PAUSE 3 NO-MESSAGE.
               END.
            END.
         END.
         op-entry.qty:col    = 66 - length(op-entry.qty:format).
      &ENDIF
   &ENDIF

   &IF DEFINED(nocreate) = 0 &THEN

      &IF DEFINED(Regim-OneDoc) NE 0 &THEN
         fl_SearchOP = NO.
         IF op-template.doc-type BEGINS "(" THEN DO:  

            atempl = TRIM(op-template.doc-type,"(").
            atempl = TRIM(atempl,")").

            FIND FIRST wop WHERE wop.op-templ EQ INT(atempl) NO-ERROR.
            FIND op WHERE RECID(op) EQ wop.op-recid NO-LOCK NO-ERROR.
            IF AVAIL(op) AND CAN-FIND(FIRST signs WHERE signs.file-name  EQ "op"
                                                    AND signs.code       EQ "op-bal"
                                                    AND (signs.code-value EQ STRING(op.op) OR signs.xattr-value EQ STRING(op.op)))  THEN DO : 
               /*Была постановка на картотеку */
               RELEASE op.           
            END.    
            IF AVAIL op THEN
            fl_SearchOP = YES.
         END.
      &ENDIF
      IF &IF DEFINED(Regim-OneDoc) EQ 0 
         &THEN YES 
         &ELSE fl_SearchOP = NO &ENDIF 
      THEN DO:
         fl_editOP = YES.
      CREATE op.
      {op(sess).cr}
      &IF DEFINED(op-doc-num-format) EQ 0 &THEN
         &SCOPED-DEFINE op-doc-num-format op.doc-num:FORMAT
      &ENDIF

      &IF DEFINED(pl-date) &THEN
         {g-op.ass &pl-date=&pl-date}
      &ELSE
         {g-op.ass}
      &ENDIF
      END.
      
      
   &ENDIF
   IF AVAIL op THEN
   FIND doc-type OF op NO-LOCK NO-ERROR.

   {frmfield.fun &DefDeleteTempTable = YES
      &WHERE = "WHERE frm-fields.op-templ = op-templ.op-templ"}

   &IF DEFINED(nocreate) = 0 &THEN
      CREATE op-entry.

      &IF DEFINED(ORACLE) &THEN
         VALIDATE op NO-ERROR.
      &ENDIF
      {g-en.ass}
      ASSIGN
         op-entry.value-date = in-op-date.
         op-entry.currency = IF op-templ.currency <> ? THEN GetCurr(op-templ.currency) ELSE op-entry.currency.
      IF op-entry.currency <> ? THEN 
         tcur = op-entry.currency.
   &ENDIF

   &IF DEFINED(NOmfoTotal) = 0 &THEN
   &IF DEFINED(NOmfo) = 0 &THEN
      {g-bankv1.i}
      DEF VAR bank-cnt AS INT64 NO-UNDO.
      DEF VAR is-rec   AS LOGICAL INIT NO NO-UNDO.
      DEF VAR is-send  AS LOGICAL INIT NO NO-UNDO.
      ASSIGN
         is-rec  = NO
         is-send = NO
      .
      DO bank-cnt = 1 TO NUM-ENTRIES(op-templ.bank-op-templ) BY 4:
         IF      ENTRY(bank-cnt, op-templ.bank-op-templ) = "rec"  THEN is-rec  = YES.
         ELSE IF ENTRY(bank-cnt, op-templ.bank-op-templ) = "send" THEN is-send = YES.
      END.
      IF is-rec OR NOT is-send THEN
      ASSIGN
         op.doc-kind = "rec"
         doc-kind    = {&rec-label}
      .
      ELSE
      ASSIGN
         op.doc-kind = "send"
         doc-kind    = {&send-label}
      .
   &ELSE
      {g-bankv1.i &OFmfo="/*"}
   &ENDIF
   &ENDIF

   &IF DEFINED(nocreate) = 0 &THEN
      FOR EACH wop WHERE wop.op-templ >= op-templ.op-templ:
         DELETE wop.
      END.
   &ENDIF
   &IF DEFINED(DoLoan) &THEN
      DISPLAY
         /*in-cont-code*/ in-doc-ref
         vContractDate
         in-cont-cur
      .
      {set.i &THIS_FRAME = "opreq" &EXFILE = "g-frame.i.st2" {&*}}.
        IF AVAIL op THEN op.contract-date = vContractDate.
   &ENDIF

   /* Действия после присвоения индивидуальные под каждую процедуру */
   {&DoBeforeAfterSet}
   &IF DEFINED(nocreate) = 0 &THEN
      CREATE wop.
      ASSIGN
         wop.acct-db  = op-template.acct-db  /* Инициализация из шаблона. */
         wop.acct-cr  = op-template.acct-cr  /* Инициализация из шаблона. */
         wop.currency = op-entry.currency
         dval         = op-entry.value-date
         wop.op-templ = op-templ.op-templ
         wop.op-kind  = op-kind.op-kind.
      &IF DEFINED(DoOp) &THEN
      ASSIGN wop.op-recid = RECID(op-entry)
         wop.con-date = vContractDate
         wop.op-kind  = op-kind.op-kind.
      &ENDIF
   &ENDIF
  
   &IF DEFINED(DoLoan) &THEN
      IF /*in-cont-code*/ /*in-doc-ref*/ in-loan NE "" AND /*in-cont-code*/ /*in-doc-ref*/ in-loan
      <> ? THEN
      DO:
         FIND loan WHERE loan.filial-id EQ shFilial
                     AND loan.contract  EQ in-contract
                     AND /*loan.cont-code*/ loan.doc-ref EQ /*in-doc-ref*/ in-loan
         NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
         DO:
            in-cont-code = loan.cont-code.
            in-cont-cur = if loan.currency = '' then '810' else loan.currency .  
         END.
      END.
   &ENDIF

   &IF DEFINED(browse-entry) = 0 &THEN
      {g-acctv1.i &OFbase=Yes &vacct=op-entry.acct {&*}}
  
      RUN transmes.p (op-entry.acct-db,
                      op-entry.currency,
                      op-kind.op-kind,
                      "ДЕБЕТ",
                      INPUT-OUTPUT mClMessList).
      RUN transmes.p (op-entry.acct-cr,
                      op-entry.currency,
                      op-kind.op-kind,
                      "КРЕДИТ",
                      INPUT-OUTPUT mClMessList).
   
      /* Если выбрано прекращение транзакции. */
      IF sStop
      THEN UNDO gen, LEAVE gen.

      RUN CreateFrmFields (op-templ.op-templ,"opreq","acct-db", op-entry.acct-db).
      RUN CreateFrmFields (op-templ.op-templ,"opreq","acct-cr", op-entry.acct-cr).
   &ENDIF
   &IF DEFINED(nocreate) = 0 &THEN
      &IF DEFINED(DoTacct) &THEN
         ASSIGN
            tacct-cr = op-entry.acct-cr
            tacct-db = op-entry.acct-db
         .
      &ENDIF
      

      {asswop.i}
      op-entry.currency = IF op-templ.currency <> ? 
                          THEN GetCurr(op-templ.currency) 
                          ELSE op-entry.currency. 
      wop.currency = op-entry.currency.
      IF op-entry.currency NE ? THEN 
         tcur = op-entry.currency.

      DEF VAR vAmtPrep    LIKE op-entry.amt-rub NO-UNDO.
      DEF VAR vAmtAftAcct AS   LOGICAL          NO-UNDO.

      IF (op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "") OR
         (op-templ.prep-amt-natcur <> ? AND op-templ.prep-amt-natcur <> "")
      THEN DO:
         IF op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "" 
         THEN
            vAmtPrep = DECIMAL(op-templ.prep-amt-rub) NO-ERROR.
         ELSE 
            vAmtPrep = DECIMAL(op-templ.prep-amt-natcur) NO-ERROR.

         vAmtAftAcct = GetXAttrValueEx("op-template",
                                       op-template.op-kind + "," + STRING(op-template.op-template),
                                       "AmtAftAcct","НЕТ") EQ "Да".
         /* Для шаблонов с ДР AmtAftAcct = Да вычисляем сумму сразу только если задана постоянная сумма или заданы оба счета. 
            Для остальных шаблонов вычисляем сумму сразу */
         IF NOT vAmtAftAcct OR
            vAmtPrep > 0 OR 
            ({assigned op-entry.acct-db} AND {assigned op-entry.acct-cr}) 
         THEN DO:
            RUN parssen.p (RECID(wop), in-op-date, OUTPUT fler).

            /* Если выбрано прекращение транзакции. */
            IF sStop
               THEN UNDO gen, LEAVE gen.

            IF fler THEN UNDO gen, LEAVE gen.

            IF     wop.amt-rub = 0 
               OR (wop.amt-sign BEGINS ">" AND wop.amt-rub <= 0) 
               OR (wop.amt-sign BEGINS "<" AND wop.amt-rub >= 0) THEN 
            DO:
               HIDE FRAME opreq NO-PAUSE.
               &IF DEFINED(OP-UNDO) &THEN {&OP-UNDO}
               &ELSE
                 UNDO doc ,  NEXT doc    .
               &ENDIF
            END.
         END.
      END.
      DISPLAY
         wop.amt-rub WHEN wop.amt-rub NE ?        @ op-entry.amt-rub
                   0 WHEN wop.amt-rub EQ ?        @ op-entry.amt-rub
         wop.amt-cur WHEN op-entry.currency NE "" @ op-entry.amt-cur
                   0 WHEN op-entry.currency EQ "" OR wop.amt-cur EQ ? @ op-entry.amt-cur
      WITH FRAME opreq.
   &ENDIF

&ELSEIF DEFINED(const-recip) &THEN

   mRecip-data = GetXAttrValueEx("op-template",
                                 op-kind.op-kind + "," + STRING(op-templ.op-templ),
                                 "const-recip",
                                 "").
   IF mRecip-data NE "" THEN DO:
      &IF DEFINED(noben) = 0 &THEN
         ASSIGN
            op.name-ben:SCREEN-VALUE = ENTRY(5,mRecip-data,"^")
            op.name-ben              = op.name-ben:SCREEN-VALUE
            op.inn:SCREEN-VALUE      = ENTRY(3,mRecip-data,"^")
            op.inn                   = op.inn:SCREEN-VALUE
         NO-ERROR.
      &ENDIF
      &IF DEFINED(nomfo) = 0 &THEN
         ASSIGN
            vmfo              = ENTRY(1,mRecip-data,"^")
            vmfo:SCREEN-VALUE = vmfo
            op.ben-acct       = ENTRY(2,mRecip-data,"^")
            op.ben-acct:SCREEN-VALUE = op.ben-acct
         .
      &ENDIF
   END.

&ENDIF
/* $LINTFILE='g-frame.i' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='kozv' */
/* $LINTDATE='06/04/2016 14:30:21.298+04:00' */
/*prosignVa5QCYNdhcizUQuReTRvwA*/
/* --- g-frame.i was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:23am --- */
