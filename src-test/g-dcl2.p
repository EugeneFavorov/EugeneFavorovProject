/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  g-cash2.p
      Comment:  Ввод не сложных, однострочных документов

         Uses:  -
      Used by:  opkindnav.p
      Created:  04/01/1998 Peter from g-cash.p
     Modified:  03/12/2001 NIK Вызов kautrig.p заменен на  Kau-Trigger in h_op.
     modified: 20/01/2002 kostik Задача 4488 поднята из БИН
     Modified: 06/05/2003 Илюха - Форматирование + вставлен инклюдник
                                  g-psigns.i с обработкой реквизитов
                                  создаваемых парсером
     Modified: 28/11/2003 FeaK  добавлен параметр OUTPUT fl-err в вызове 
                                процедуры g_sroch2.p.                             
     Modified: 24.11.2006 16:26 OZMI     (0070663)
     
     Modified: 17.03.2015 pja Сделано специально и только для закрытия срочых вкладов в срок.
                          Изменения вызваны необходимостью печатать заявление на закрытие вклада
*/

{g-defs.i}
{crdps.def}
{g-error.def}
{wordwrap.def}
{globals.def}
{dpsproc.def}
{def-wf.i new}
{defframe.i new}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get lnbh}
{intrface.get cust}
{intrface.get brnch}
{intrface.get tmcod}


DEFINE INPUT PARAMETER in-op-date LIKE op.op-date.
DEFINE INPUT PARAMETER oprid      AS   RECID.

{chktempl.i}

DEFINE VARIABLE fmt    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE dval   LIKE op-entry.value-date NO-UNDO.
DEFINE VARIABLE fler   AS   LOGICAL             NO-UNDO.
DEFINE VARIABLE RESULT AS   INT64             NO-UNDO.
DEFINE VARIABLE msg       AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE hProc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE acctkey   AS INT64   NO-UNDO.
DEFINE VARIABLE temp-acct AS CHARACTER NO-UNDO.
DEFINE VARIABLE loan_h    AS HANDLE    NO-UNDO .

DEFINE VARIABLE need-valdate AS LOGICAL FORMAT "Дата валютирования/" NO-UNDO.
DEFINE VARIABLE f-close-acct AS LOGICAL INITIAL YES                  NO-UNDO.

DEFINE VARIABLE remain-amt   LIKE op-entry.amt-rub NO-UNDO.

DEFINE BUFFER xxop      FOR op.
DEFINE BUFFER xwop      FOR wop.
DEFINE BUFFER xop-kind  FOR op-kind.
DEFINE BUFFER xop-entry FOR op-entry.

DEFINE BUFFER xperson     FOR person.
DEFINE BUFFER xcust-ident FOR cust-ident.

DEFINE NEW GLOBAL SHARED STREAM err .

DEFINE VARIABLE cod-ost      AS   CHARACTER  INITIAL ? NO-UNDO .
DEFINE VARIABLE mforeq       AS   LOGICAL              NO-UNDO.
DEFINE VARIABLE vmfo         LIKE op-bank.bank-code.
DEFINE VARIABLE vcorr-acct   LIKE op-bank.corr-acct.
DEFINE VARIABLE fl-err     AS   INT64            INITIAL -1.
DEFINE VARIABLE str_recids AS   CHARACTER          NO-UNDO.
DEFINE VARIABLE i          AS   INT64            NO-UNDO.
DEFINE VARIABLE fl_u       AS   INT64            NO-UNDO .
DEFINE VAR      h_dpsb2p      AS   HANDLE             NO-UNDO.
DEFINE VAR mSysConfDocument AS   CHAR    NO-UNDO. 
DEFINE VAR mSysConfDocType  AS   CHAR    NO-UNDO. 
DEFINE VAR mSysConfIssue    AS   CHAR    NO-UNDO. 
DEFINE VAR mSysConfDate     AS   DATE    NO-UNDO. 
DEFINE VAR mInt             AS   INT64 NO-UNDO.
/*Признак печати шапки отчета*/
DEFINE VARIABLE vHeaderlog AS LOGICAL INITIAL YES NO-UNDO.

/* В случае, когда вклад закрывается не вкладчиком */
DEFINE VAR mCustType        AS CHAR NO-UNDO.
DEFINE VAR mProxy           AS CHAR NO-UNDO.
DEFINE VAR mAgentID         AS CHAR NO-UNDO.
DEFINE VAR mAgentLastName   AS CHAR NO-UNDO.
DEFINE VAR mAgentFirstNames AS CHAR NO-UNDO.
DEFINE VAR mAgentDocType    AS CHAR NO-UNDO.
DEFINE VAR mAgentDocNum     AS CHAR NO-UNDO.
DEFINE VAR mAgentDocDate    AS DATE NO-UNDO.
DEFINE VAR mSingleDoc       AS CHAR NO-UNDO.
DEFINE VAR mAgentIssue      AS CHAR NO-UNDO.
DEFINE VAR mThirdPerson     AS CHAR NO-UNDO.

DEFINE VARIABLE mPodrA AS CHARACTER NO-UNDO.
DEFINE VARIABLE summa  AS   DECIMAL NO-UNDO.

/*Очистка информации о начисленных процентах*/
RUN DeleteOldDataProtocol IN h_base ("КАПИТАЛ%%%").
RUN DeleteOldDataProtocol IN h_base ("КАПИТАЛНАЛОГ").
RUN DELETEOLDDATAPROTOCOL IN h_base ("КапиталПер%%%") .
/******************/


&SCOPED-DEFINE BYrole    YES .
&SCOPED-DEFINE DoLoan    YES .
&SCOPED-DEFINE Dotacct   YES.
&SCOPED-DEFINE DoOp      YES .
&GLOBAL-DEFINE mess_type 'При обработке указанного вклада нельзя использовать выполняемую транзакцию .'
&GLOBAL-DEFINE undo_type RETURN "no-apply"

FUNCTION g-checkbank  RETURNS LOGICAL (INPUT  vmfo       AS CHAR,
                                       INPUT  iCodeType  AS CHAR,
                                       INPUT  vcorr-acct AS CHAR,
                                       INPUT  benacct    AS CHAR,
                                       OUTPUT result     AS INT64,
                                       OUTPUT msg        AS CHAR) IN hproc.
FUNCTION Set_type     RETURNS LOGICAL (INPUT l-type  AS CHARACTER) IN loan_h .
FUNCTION Set_ost      RETURNS LOGICAL (INPUT l-type  AS CHARACTER) IN loan_h .

RUN "g-func.p" PERSISTENT SET hProc.
RUN "l-type.p" PERSISTENT SET loan_h.

{setdest2.i
   &stream   = "stream err"
   &filename = _spool1.tmp
   &cols     = 120}

{g-currv1.i
   &OFbase  = "/*"}
{g-frame.i
   &DoFrame = YES
   &OFcash  = YES
   &row     = 1}

RELEASE dacct.
RELEASE cacct.

{chkacces.i}
{g-trig.i 
    &DoLoan = Yes
    &OFcash=*
    &Ret_Name-Ben_Doc-Name = YES
 }
/*mMultiCurr = YES - мультивалютный вклад
  mMultiCurr = NO -  обучный вклад */
{tt-multicurr.i}

GEN:
DO TRANSACTION WITH FRAME opreq ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE:
   {optr.i &DoBefore=YES}

   /* Закрытие охватывающего мультивалютного договора */
   IF mMultiCurr EQ YES THEN 
   DO:   
      FIND FIRST Loan WHERE loan.cont-code EQ in-cont-code
                        AND loan.contract  EQ in-contract
                            NO-ERROR.
      IF NOT AVAILABLE Loan THEN  UNDO gen, LEAVE gen.   
      ASSIGN
         loan.close-date  = in-op-date
         loan.loan-status = '√'
         in-doc-ref = loan.doc-ref
      .
   END.

   FOR EACH tt-multicur WITH FRAME opreq
       ON ENDKEY UNDO gen, LEAVE gen 
       ON ERROR  UNDO gen, LEAVE gen: 

      FIND FIRST Loan WHERE loan.cont-code EQ tt-multicur.multicur-cont-code
                        AND loan.contract  EQ in-contract
                        NO-ERROR.
      /*удаление ранее загруженой библиотеки*/
      RUN peb-unl.p("dps-b2p.p").

      DebugParser = INT64(GetXAttrValueEx("op-kind",
                                        op-kind.op-kind,
                                        "DebugParser",
                                        "0")).      
      {plibinit.i}
      ASSIGN
         tcur     = ?
         tacct-db = ?
         tacct-cr = ?
         tamt     = 0
         in-cont-code = tt-multicur.multicur-cont-code
         in-loan  = if avail loan then loan.doc-ref else in-loan
         in-cont-cur = if avail loan then (if loan.currency = '' then '810'
                                             else  loan.currency)
                                     else in-cont-cur          
      .

      /* получить тип договора по номеру */
      Set_type(in-cont-code) . 
      
      DOC:
      FOR EACH op-templ OF op-kind WHERE
               op-template.cr-class-code MATCHES "*OP*"
      NO-LOCK WITH FRAME opreq ON ENDKEY UNDO gen, LEAVE gen
                               ON ERROR  UNDO gen, LEAVE gen
      BREAK BY op-templ.op-templ:
         {bef-tran.i}
         IF op-template.op-status < "√" THEN
            f-close-acct = NO.
                           /* Если у шалона изначально стоит статус "А",
                           ** то используем его как шалон для прочих шалонов.
                           ** Запускаем по нему парсер, но не создаем документы. */
         IF       op-template.op-status   EQ "А"
            AND   GetXattrValueEx (
                     "op-template",
                     op-template.op-kind + "," + STRING (op-template.op-template),
                     "PrsnTmpl",
                     "Нет")   EQ "Да"
            THEN mDsplSet = NO.
            ELSE mDsplSet = YES.
     
         need-valdate = CAN-DO("ДА,YES,TRUE",
                               Get_Param("ДатаВал",RECID(op-templ))).
      
         IF need-valdate EQ ? THEN
            need-valdate = NO.
      
         {g-frame.i
            &DoBefore= YES
            &OFcash  = YES
            &DoTacct = YES
            &run_b2p          = YES
            &OP-UNDO = " DELETE op-entry.
                         DELETE op .
                         NEXT doc .
                       "
            &DoBeforeAfterSet = "RUN PrintHeader."
         }

         /* Проверка наличия мультивалютного вклада */
         IF mMultiCurr EQ NO THEN 
         DO:       
            FIND FIRST loan WHERE loan.contract = "dps" AND 
                                  loan.cont-code = in-cont-code
                                  NO-ERROR.
            IF AVAILABLE loan AND loan.parent-cont-code NE '' THEN 
            DO:
               MESSAGE "Вклад мультивалютный." SKIP
                       "Для закрытия этого вклада выбирете другую" SKIP
                       "транзакцию и мультивалютный охватываюший договор." SKIP
                       VIEW-AS ALERT-BOX ERROR.
               UNDO gen, LEAVE gen.
            END.
         END.
         /*IF loan.end-date <= vContractDate THEN
         DO:
            MESSAGE "Вклад нельзя закрыть досрочно!"
               VIEW-AS ALERT-BOX ERROR.
            UNDO gen, LEAVE gen.
         END.*/
         RUN CreateFrmFields (?,"LoanRecid","",STRING(RECID(loan))).

         IF FIRST(op-template.op-template) THEN 
         DO:
            /* открытие счетов */
            Set_type(in-cont-code).
            RUN SetSysConf IN h_base ("PlacementFO_RSHB_ShowForms", "Нет").
            RUN g-ac4.p(in-op-date, RECID(op-kind), RECID(loan), OUTPUT fl-err, INPUT-OUTPUT summa) .
            RUN DeleteOldDataProtocol IN h_base ("PlacementFO_RSHB_ShowForms").
            IF fl-err LT 0 THEN
               UNDO gen,LEAVE gen.
         END.
         
         {g-frame.i
             &DoDisp         = YES
             &ChkBlockAction = "UNDO GEN, LEAVE GEN."
         }
      
         mforeq = op-templ.mfo-needed OR
                  (GetXAttrValue('op-template',
                                op-kind.op-kind + ',' + STRING(op-templ.op-templ),
                                'МежБанк')  = 'Да').
         IF mforeq THEN
         DO:
            {g-frame.i &const-recip = Yes}
         END.   

         SSET:
         DO ON ERROR UNDO, RETRY ON ENDKEY UNDO gen, LEAVE gen:
      
            /* if op-templ.amt-rub matches '*Нач*'  and cod-ost ne ? then
               Set_ost(cod-ost). */
      
            IF op-templ.cr-class-code MATCHES '*kau*' THEN
            DO :
               RUN Kau-Trigger IN h_op (RECID(op-entry),
                                        OUTPUT flager,
                                        YES).
               IF flager NE 0 THEN
                  UNDO,RETRY.
      
               IF AVAILABLE op-entry THEN
                  DELETE op-entry .
      
               ASSIGN
                  op.doc-num = 'кау' + STRING(op-template.op-template).
               PAUSE 0 .
            END.
            IF AVAILABLE op-entry THEN
            DO :
               IF LAST(op-template.op-template) THEN
               DO:
                  remain-amt = IF wop.currency NE ? AND wop.currency NE "" THEN
                                  wop.amt-cur
                               ELSE
                                  wop.amt-rub.
                  set_type(loan.cont-code).
                  {g-frame.i
                     &DoSet  = YES
                     &OFcash = YES
                     &OFSum  = YES
                     &NODISPLAY = YES
                  }
                  
                  remain-amt = remain-amt - (IF op-entry.currency NE ?  AND
                                                op-entry.currency NE "" THEN
                                                op-entry.amt-cur
                                             ELSE
                                                op-entry.amt-rub).
               END.
               ELSE
               DO:
                  cod-ost = GetXattrValueEx ("op-template",
                                             op-template.op-kind + "," + STRING (op-template.op-template),
                                             "Код_Ост",
                                             "").
                  IF NOT {assigned cod-ost} THEN
                  DO:
                     cod-ost = Get-Ost-templ(op-kind.op-kind,op-templ.op-templ).
                  END.
                  IF op-templ.amt-rub MATCHES '*Нач*' AND cod-ost <> ?  THEN
                     Set_ost(cod-ost).
                  ELSE
                     set_type(in-cont-code).
                  {g-frame.i
                     &DoSet  = YES
                     &OFcash = YES
                  }
               END.
               IF op.op-status BEGINS "А" THEN
                  ASSIGN
                     op.op-date       = ?
                     op-entry.op-date = op.op-date
                     .
               {op-entry.upd &871=yes
                  &open-undo = "UNDO gen, LEAVE gen" 
               }
                /* Создание ДР типа PARSEN_<КодРеквизита>*/
               {g-psigns.i}
      
               /*если вклад закрывается по доверенности*/
               IF  cacct.contract   EQ "Касса" THEN DO:
                   /* Определяем, участвует ли в проведении операции кто-либо, кроме владельца вклада */
                   DO ON ERROR UNDO gen, LEAVE gen:
                     /* запрос */
                     RUN dps-pers.p (loan.cust-id,
                                     in-op-date,
                                     loan.cont-code,
                                     op-kind.op-kind,
                                     OUTPUT fler, OUTPUT mCustType,
                                     OUTPUT mProxy,
                                     OUTPUT mAgentID,      OUTPUT mAgentLastName, OUTPUT mAgentFirstNames,
                                     OUTPUT mAgentDocType, OUTPUT mAgentDocNum,   OUTPUT mAgentDocDate,
                                     0 ).
                     IF NOT fler THEN DO:
                       UNDO gen, LEAVE gen.
                     END.
                     fler = FALSE.
                   END.
                   /****************************************************************************/
                   /* Определяем предъявляемый документ */
                   DO ON ERROR UNDO gen, LEAVE gen:
                     /* Выбор предъявляемого документа */
                     IF mAgentID <> "" AND mSingleDoc = "" /* agent-id определен, но документ еще не определен */ THEN DO:
      
                        FIND FIRST xcust-ident WHERE xcust-ident.cust-cat = "Ч"
                                                 AND xcust-ident.cust-id  = INT64(mAgentID)
                                                NO-LOCK NO-ERROR.
                        IF AVAILABLE xcust-ident THEN DO: /* Документ не единственный - нужно спросить пользователя,
                                                             какой из зарегистрированных документов клиент изволил предъявить? */
                           pick-value = "unknown". /* признак того, что документ не выбрали */
                           
                           RUN browseld.p ("p-cust-ident",
                                           "cust-cat~001cust-id~001close-date1~001close-date2",
                                           "Ч" + CHR(1) + STRING(mAgentID) + CHR(1) + "?" + CHR(1) + "?",
                                           "cust-cat~001cust-id~001close-date1~001close-date2",
                                           3).
                                           
                           IF pick-value = "unknown" THEN DO: /* Отказ выбрать документ прерывает операцию */
                             UNDO gen, LEAVE gen.
                           END.
                           mSingleDoc = pick-value.
                        END.
                     END.
                   END.
      
                   IF mSingleDoc <> ? AND mSingleDoc <> "" THEN DO:
                     FIND FIRST xcust-ident WHERE xcust-ident.cust-code-type =         ENTRY(1, ENTRY(1, mSingleDoc, "~001"), ",")
                                              AND xcust-ident.cust-code      =         ENTRY(2, ENTRY(1, mSingleDoc, "~001"), ",")
                                              AND xcust-ident.cust-type-num  = INT64(ENTRY(3, ENTRY(1, mSingleDoc, "~001"), ","))
                                            NO-LOCK NO-ERROR.
                     FIND FIRST xperson WHERE xperson.person-id = INT64(ENTRY(3, mSingleDoc, "~001"))
                                        NO-LOCK NO-ERROR.
                     mAgentID = ENTRY(3, mSingleDoc, "~001").
      
                     IF AVAILABLE xperson THEN DO:
                       mAgentLastName   = xperson.name-last.
                       mAgentFirstNames = xperson.first-names.
                     END.
                     IF AVAILABLE xcust-ident THEN DO:
                       mAgentDocType = xcust-ident.cust-code-type.
                       mAgentDocNum  = xcust-ident.cust-code.
                       mAgentDocDate = xcust-ident.open-date.
                       mAgentIssue   = xcust-ident.issue.
                       mPodrA = GetXattrValueEx("cust-ident",
                                                                  xcust-ident.cust-code-type + "," + xcust-ident.cust-code + "," + STRING(xcust-ident.cust-type-num),
                                                                 "подразд",
                                                                 "").
                       mAgentIssue = mAgentIssue + IF {assigned mPodrA} THEN (" к/п " + mPodrA)
                                                                                                         ELSE ("").   

                     END.
                     ELSE IF AVAILABLE xperson THEN 
                     DO:
                       mAgentDocType = xperson.document-id.
                       mAgentDocNum  = xperson.document.
                       mAgentDocDate = DATE(GetXAttrValue("person", STRING(xperson.person-id), "Document4Date_vid")).
                     END.
                   END.
                   ELSE DO: /* Документ по какой-то причине не найден:
                               ищем действующую персону и по ней определяем документы */
                      mAgentID = IF mCustType <> "Agent"
                                 THEN STRING(loan.cust-id)
                                 ELSE STRING(mAgentID).
                      FIND FIRST xperson WHERE xperson.person-id = INT64(mAgentID)
                                         NO-LOCK NO-ERROR.
                     IF AVAILABLE xperson THEN 
                     DO:
                        ASSIGN
                           mAgentLastName   = xperson.name-last
                           mAgentFirstNames = xperson.first-names
                           mAgentDocType = xperson.document-id
                           mAgentDocNum  = xperson.document
                           mAgentDocDate = DATE(GetXAttrValue("person", STRING(xperson.person-id), "Document4Date_vid"))
                           .
                        FIND FIRST cust-ident WHERE cust-ident.cust-cat       EQ "Ч"
                                                AND cust-ident.cust-id        EQ xperson.person-id
                                                AND cust-ident.Class-code     EQ "p-cust-ident"
                                                AND (   cust-ident.close-date GE in-op-date
                                                     OR cust-ident.close-date EQ ?)
                                                AND cust-ident.cust-code-type EQ xperson.document-id
                           NO-LOCK NO-ERROR.
                        IF AVAIL cust-ident THEN
                           ASSIGN
                              mAgentDocDate = cust-ident.open-date 
                              mAgentIssue   = cust-ident.issue
                              .
                              mPodrA = GetXattrValueEx("cust-ident",
                                                                        cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num),
                                                                        "подразд",
                                                                        "").
                               mAgentIssue = mAgentIssue + IF {assigned mPodrA} THEN (" к/п " + mPodrA)
                                                                                                                 ELSE ("").  
                     END.
                   END.
      
                   /*------------------------------------------------------------------*/
                   /* Запомнить, по какой доверенности открывается вклад               */
                   /*------------------------------------------------------------------*/
                   IF mCustType = "Agent" 
                      AND mProxy NE "owner"
                      AND {assigned mProxy} THEN
                   DO:                
                       UpdateSigns(op.class-code, STRING(op.op),
                                   "proxy-code", mProxy, ?).
                   END.
                   /*------------------------------------------------------------------*/
                   /* Запомнить, какой документ был предъявлен                         */
                   /*------------------------------------------------------------------*/
                  mSysConfDocument = GetSysConf("ПаспортныеДанныеПостоянногоПолучателя").
                  mSysConfDocType  = GetSysConf("ПаспортныеДанныеПостоянногоПолучателя.document-id").
      
                  IF  {assigned mSysConfDocument}
                     AND {assigned mSysConfDocType} THEN
      
                  DO:
                     IF INDEX(mSysConfDocument,"выдан") > 0 THEN
                     DO:
                        /* Формат "... выдан ..." */
                        mSysConfIssue = TRIM(SUBSTR(mSysConfDocument,
                                             INDEX(mSysConfDocument,"выдан") + 6)).
                        mSysConfDocument = TRIM(SUBSTR(mSysConfDocument, 1,
                                                INDEX(mSysConfDocument,"выдан") - 1)).
                        /* Определяем дату */
                        mSysConfDate = ?.
                        mInt = NUM-ENTRIES(mSysConfIssue," ").
                        mSysConfDate = DATE(ENTRY(mInt, mSysConfIssue, " ")) NO-ERROR.
                     
                        IF NOT ERROR-STATUS:ERROR THEN 
                           ASSIGN
                              ENTRY(mInt, mSysConfIssue, " ") = ""
                              mSysConfIssue = TRIM(mSysConfIssue)
                           .
                        ELSE 
                        DO:
                           mSysConfDate = DATE(ENTRY(1, mSysConfIssue, " ")) NO-ERROR.
                           IF NOT ERROR-STATUS:ERROR THEN 
                              ASSIGN
                                 ENTRY(1, mSysConfIssue, " ") = ""
                                 mSysConfIssue = TRIM(mSysConfIssue)
                              .
                           ELSE 
                              mSysConfDate = ?.
                        END.
                     
                        ASSIGN
                           mAgentDocNum  = mSysConfDocument
                           mAgentIssue   = mSysConfIssue
                           mAgentDocDate = mSysConfDate
                           mAgentDocType = mSysConfDocType
                        .
                     END.
                     ELSE 
                        ASSIGN
                           mAgentDocNum  = mSysConfDocument
                           mAgentIssue   = ""
                           mAgentDocDate = ?
                           mAgentDocType = mSysConfDocType
                        .
                  END.
                /*Если операция совершается владельцем вклада (второй output параметр 
                  dps-pers.p = owner), то никаких реквизитов сохранять не надо (как было до 47 изменения в VSS dvp)*/
                  IF mProxy NE "owner"
                     AND {assigned mProxy} THEN
                  DO:
                     /*СохрДокумент3л. Если реквизит установлен в ДА, то name-send и name-ben сохранять с реквизитами паспорта
                       Иначе - только ФИО*/
                     mThirdPerson = GetXAttrValueEx("op-kind", op-kind.op-kind, "СохрДокумент3л", "Нет").
                     IF mThirdPerson EQ "Да" THEN 
                     DO:
                        UpdateSigns(op.class-code, 
                                    STRING(op.op),
                                     "name-send",
                                     mAgentLastName + " " + mAgentFirstNames + ", " +
                                     mAgentDocType + " " + mAgentDocNum +
                                    (IF    {assigned mAgentIssue}
                                        OR mAgentDocDate <> ? THEN 
                                       (" Выдан " + (IF {assigned mAgentIssue}
                                                     THEN mAgentIssue + " "
                                                     ELSE "")
                                                  + (IF mAgentDocDate <> ?
                                                     THEN STRING(mAgentDocDate, "99/99/9999")
                                                     ELSE "") + ".")
                                     ELSE "."),
                                     ?).
                        op.name-ben = mAgentLastName + " " + mAgentFirstNames + ", " +
                                      mAgentDocType + " " + mAgentDocNum +
                                      (IF    {assigned mAgentIssue}
                                          OR mAgentDocDate <> ? THEN 
                                       (" Выдан " + (IF {assigned mAgentIssue}
                                                     THEN mAgentIssue + " "
                                                     ELSE "")
                                                  + (IF mAgentDocDate <> ?
                                                     THEN STRING(mAgentDocDate, "99/99/9999")
                                                    ELSE "") + ".")
                                       ELSE ".").
                  
                     END.
                     ELSE
                     DO:
                        UpdateSigns(op.class-code, 
                                    STRING(op.op),
                                    "name-send",
                                    mAgentLastName + " " + mAgentFirstNames,
                                    ?).
                        op.name-ben = mAgentLastName + " " + mAgentFirstNames.
                     END.
                     UpdateSigns(op.class-code, 
                                 STRING(op.op), 
                                 "document-id", 
                                 mAgentDocType, 
                                 ?).
                     UpdateSigns(op.class-code, 
                                 STRING(op.op),
                                 "Докум",
                                 mAgentDocNum +
                                 (IF    {assigned mAgentIssue}
                                     OR mAgentDocDate <> ? THEN 
                                    (" Выдан " + (IF {assigned mAgentIssue}
                                                  THEN mAgentIssue + " "
                                                  ELSE "")
                                               + (IF mAgentDocDate <> ?
                                                  THEN STRING(mAgentDocDate, "99/99/9999")
                                                  ELSE "") + ".")
                                  ELSE "."),
                                 ?).

                     UpdateSigns(op.class-code,STRING(op.op),"ФИО",mAgentLastName + " " + mAgentFirstNames,?).
                     UpdateSigns(op.class-code,STRING(op.op),"Довер","да",?).
                     IF GetXattrValueEx("loan","proxy," + mProxy,"single-mark",?) = "Да" THEN
                     DO:
                        DEFINE BUFFER proxy-loan FOR loan.
                        FIND FIRST proxy-loan WHERE 
                                   proxy-loan.contract = "proxy" AND
                                   proxy-loan.cont-code = mProxy
                           EXCLUSIVE-LOCK.
                        ASSIGN
                           proxy-loan.end-date = op.op-date
                           proxy-loan.close-date = op.op-date
                           proxy-loan.loan-status = CHR(251)
                        .
                        RELEASE proxy-loan.
                     END.
                  END.
               END.
               {crs.i}
            END.
            IF AVAILABLE op-entry OR
               CAN-FIND(FIRST xxop WHERE
                              xxop.op-transaction EQ op.op-transaction
                          AND RECID(xxop)         NE RECID(op))
            THEN
            DO:
               IF mforeq AND
                  NOT g-checkbank(vmfo,
                                  mbank-code-type,
                                  vcorr-acct,
                                  op.ben-acct,
                                  OUTPUT result,
                                  OUTPUT msg)
               THEN
               DO:
                  IF op-templ.mfo-needed THEN
                  DO:
                     MESSAGE SKIP msg SKIP(1) VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     CASE result:
                        WHEN {&egmissingmfo} OR WHEN {&egmissingbank} THEN
                           NEXT-PROMPT vmfo.
                        WHEN {&egmissingcorracct} THEN
                           NEXT-PROMPT vcorr-acct.
                        OTHERWISE NEXT-PROMPT op.ben-acct.
                     END CASE.
                     UNDO, RETRY.
                  END.
                  ELSE
                  DO:
                     MESSAGE SKIP
                        "Неверно заданы банковские реквизиты. Продолжить?" SKIP(1)
                        VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
                        UPDATE choice AS LOGICAL.
                     IF choice <> TRUE THEN
                     DO:
                        CASE RESULT:
                           WHEN {&egmissingmfo} OR WHEN {&egmissingbank} THEN
                              NEXT-PROMPT vmfo.
                           WHEN {&egmissingcorracct} THEN
                              NEXT-PROMPT vcorr-acct.
                           OTHERWISE NEXT-PROMPT op.ben-acct.
                        END CASE.
                        UNDO, RETRY.
                     END.
                  END.
               END.
               RUN "g-bank.p" (op-kind.op-kind,
                               op-templ.op-templ,
                               op.op,
                               4,
                               OUTPUT fl-err).
               IF fl-err LT 0 THEN
                  UNDO, RETRY.
               IF ({assigned vmfo}) OR ({assigned vcorr-acct}) THEN
               DO:
                  {opbnkcr.i op.op """" ""МФО-9"" vmfo vcorr-acct}
                  {op-type.upd &check-format=Yes}
               END.
               {op-type.chk}
               RUN post.
            END.
      
            IF op-templ.acct-cr MATCHES '*loan-dps-*' THEN
               Set_type(in-cont-code) .
      
            IF LAST(op-templ.op-templ) THEN
            DO:
               IF remain-amt > 0 THEN
               DO:
                  FIND xop-kind WHERE
                       xop-kind.op-kind EQ loan.op-kind NO-LOCK NO-ERROR.
                  IF AVAILABLE xop-kind THEN
                  DO:
                     RUN g_sroch2.p(in-op-date,
                                    loan.cust-id,
                                    remain-amt,
                                    RECID(xop-kind),
                                    OUTPUT fl-err).
                     IF fl-err LT 0 THEN
                         UNDO gen, LEAVE gen.
                  END.
                  ELSE
                  DO:
                     MESSAGE "Не могу создать новый вклад, т.к. " SKIP
                        "нет транзакции создания "  VIEW-AS ALERT-BOX ERROR.
                     UNDO gen,LEAVE gen.
                  END.
               END.
            END.
         END.
         
         {aft-temp.i 
            &aft-undo = " UNDO gen, LEAVE gen."}
      END.

      IF AVAILABLE loan THEN
      DO:
         RUN dpsclo.p (loan.cont-code,in-op-date,op-kind.op-kind).
         IF RETURN-VALUE = "YES" THEN
            RUN acctclo.p (loan.cont-code,in-op-date).
         ELSE 
            IF mMultiCurr EQ YES THEN UNDO gen,LEAVE gen.
      END.
   END.  /* tt-multicur END */

   {optr.i &DoAfter=YES}
END.

{g-print1.i}
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя.document-id").
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя.НаименованиеДокумента").
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя").
{preview2.i
   &stream   = "stream err"
   &filename = _spool1.tmp}

HIDE FRAME opreq NO-PAUSE.

 
DELETE PROCEDURE(hProc).
DELETE PROCEDURE(loan_h) .
IF VALID-HANDLE (h_dpsb2p) THEN
   DELETE PROCEDURE(h_dpsb2p).
{plibdel.i}

/* Комманда ОС после выполнения транзакции */
{cmd-exec.i
   &cmd        = "'Postcmd'"
}
{intrface.del}          /* Выгрузка инструментария. */ 
   
RETURN.

PROCEDURE PrintHeader:

   IF vHeaderLog THEN
   DO:
      {for_ved.i
         &NO_DEF_STREAM = YES
         &ContractDate  = vContractDate}
      vHeaderLog = NO.
   END.
   IF op-template.amt-rub MATCHES '*НачШтрНач*' THEN
      PUT STREAM err 'Пересчет ранее начисленных процентов' SKIP(1).
   ELSE
   IF op-template.amt-rub MATCHES '*НачШтраф*' THEN
      PUT STREAM err 'Доначисление процентов по пониженной ставке' SKIP(1).

END PROCEDURE.

PROCEDURE post.

   {xttrentr.i
      &OP-BUF       = op
      &OP-TEMPL-BUF = op-template}
   IF AVAILABLE op THEN
   DO:
      {additem.i str_recids STRING(RECID(op))}
   END.
   IF tcur = ? THEN
      tcur = op-entry.currency.
   ASSIGN
      wop.op-recid = RECID(op-entry)
      wop.acct-db  = op-entry.acct-db
      wop.acct-cr  = op-entry.acct-cr
      wop.currency = op-entry.currency
      wop.amt-cur  = IF op-entry.currency <> "" THEN
                        op-entry.amt-cur
                     ELSE
                        op-entry.amt-rub
      wop.amt-rub  = op-entry.amt-rub
      .
END PROCEDURE.
