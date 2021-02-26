/*
                Банковская интегрированная система БИСквит
    Copyright: (c) 1992-2017 ЗАО "Банковские информационные системы"
     Filename:  g-dcl.p
      Comment:  Закрытие вклада
   Parameters:  in-op-date oprid
         Uses:  -
      Used by:  opkindnav.p
      Created:  04/01/1998 Peter from g-cash.p
     Modified:  03/12/2001 NIK Вызов kautrig.p заменен на  Kau-Trigger in h_op.
     modified: 20/01/2002 kostik 0004488 Поднятие из Бина.
     Modified: 06/05/2003 Илюха - Форматирование + вставлен инклюдник
                                  g-psigns.i с обработкой реквизитов
                                  создаваемых парсером
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
{intrface.get pbase}
{intrface.get trans}
{intrface.get lnbh}
{intrface.get cust}
{intrface.get brnch}
{intrface.get tmcod}
{intrface.get dps}
{currency.def}

DEFINE INPUT PARAMETER in-op-date LIKE op.op-date.
DEFINE INPUT PARAMETER oprid      AS   RECID.

{chktempl.i}

DEFINE NEW GLOBAL SHARED STREAM err .

DEFINE VARIABLE fmt          AS CHARACTER             NO-UNDO.
DEFINE VARIABLE dval         LIKE op-entry.value-date NO-UNDO.
DEFINE VARIABLE fler         AS LOGICAL               NO-UNDO.
DEFINE VARIABLE result       AS INT64               NO-UNDO.
DEFINE VARIABLE msg          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE acctkey      AS INT64   NO-UNDO.
DEFINE VARIABLE temp-acct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE need-valdate AS LOGICAL   FORMAT "Дата валютирования/" NO-UNDO.
DEFINE VARIABLE f-close-acct AS LOGICAL INITIAL YES NO-UNDO.

DEFINE VARIABLE cod-ost      AS   CHARACTER          NO-UNDO.
DEFINE VARIABLE mforeq       AS   LOGICAL            NO-UNDO.
DEFINE VARIABLE vmfo         LIKE op-bank.bank-code.
DEFINE VARIABLE vcorr-acct   LIKE op-bank.corr-acct.
DEFINE VARIABLE fl-err       AS   INT64            INITIAL -1.
DEFINE VARIABLE str_recids   AS   CHARACTER        NO-UNDO.
DEFINE VARIABLE i            AS   INT64            NO-UNDO.
DEFINE VAR mSysConfDocument AS   CHAR    NO-UNDO. /* Для получения данных о доверенном */
DEFINE VAR mSysConfDocType  AS   CHAR    NO-UNDO. /* Тип док-та дов. лица */
DEFINE VAR mSysConfIssue    AS   CHAR    NO-UNDO. /* лице из поля op.name-ben          */
DEFINE VAR mSysConfDate     AS   DATE    NO-UNDO. /* "Выдан" для документа дов. лица*/
DEFINE VAR mInt             AS   INT64 NO-UNDO.
DEFINE VAR mCurr            AS CHARACTER NO-UNDO.
DEFINE VAR mOpKind          AS CHARACTER NO-UNDO.
DEFINE VAR mBaseTemplID     AS INT64     NO-UNDO.

/*Признак печати шапки отчета*/
DEFINE VARIABLE vHeaderlog   AS   LOGICAL            INITIAL YES NO-UNDO.
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

/*Переменные из УТ закрытия*/
DEFINE VAR mDBO             AS LOGICAL NO-UNDO.

/* Проверка ДР Откат в случае ошибки */
DEFINE VAR mUndoErr         AS LOGICAL NO-UNDO.

DEFINE BUFFER xxop      FOR op .
DEFINE BUFFER xwop      FOR wop.
DEFINE BUFFER xop-entry FOR op-entry.

DEFINE BUFFER xperson     FOR person.
DEFINE BUFFER xcust-ident FOR cust-ident.
DEFINE BUFFER xloan     FOR loan.


&SCOPED-DEFINE BYrole  YES .
&SCOPED-DEFINE DoLoan  YES .
&SCOPED-DEFINE Dotacct YES.
&SCOPED-DEFINE DoOp    YES .

&GLOB mess_type 'При обработке указанного вклада нельзя использовать выполняемую транзакцию .'
&GLOBAL-DEFINE undo_type RETURN "no-apply"

DEFINE  VARIABLE hProc  AS HANDLE NO-UNDO.
DEFINE  VARIABLE loan_h AS HANDLE NO-UNDO.

FUNCTION g-checkbank RETURNS LOGICAL (INPUT vmfo       AS CHARACTER,
                                      INPUT iCodeType  AS CHARACTER,
                                      INPUT vcorr-acct AS CHARACTER,
                                      INPUT benacct    AS CHARACTER,
                                      OUTPUT RESULT    AS INT64,
                                      OUTPUT msg       AS CHARACTER) IN hproc.
FUNCTION Set_type    RETURNS LOGICAL (INPUT l-type     AS CHARACTER) IN loan_h.
FUNCTION Set_ost     RETURNS LOGICAL (INPUT l-type     AS CHARACTER) IN loan_h.

RUN "g-func.p" PERSISTENT SET hProc.
RUN "l-type.p" PERSISTENT SET loan_h.

ASSIGN
      mOpKind        = GetBaseOpKind ()
      mBaseTemplID   = GetBaseTemplate ().
   
IF {assigned mOpKind }  THEN 
DO:
   ASSIGN
      mDBO          = IF GetAttrValue2(mOpKind, mBaseTemplID, "$DBO") EQ "YES" 
                      THEN YES
                      ELSE NO
      in-cont-code  = GetAttrValue2(mOpKind, mBaseTemplID, "$cont-code")
      . 
END.

FIND FIRST xloan WHERE xloan.contract  EQ "dps" AND 
                       xloan.cont-code EQ in-cont-code NO-LOCK NO-ERROR.
IF AVAIL xloan THEN                       
   in-doc-ref    = xloan.doc-ref.

IF mDBO THEN
   RUN SetSysConf IN h_base ("DBO_Without_Interface", "YES").

{setdest2.i
    &stream   = "stream err"
    &filename = _spool1.tmp
    &cols     = 120
}

{g-currv1.i
    &OFbase   = "/*"
}

mUndoErr = GetXAttrValueEx("op-kind",
                            op-kind.op-kind,
                            "ОткатПриОш",
                            "Нет") EQ "Да".

{g-frame.i
    &DoFrame = YES
    &OFcash  = YES
    &row     = 1
}

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

{befexpr.i &befopkind = op-kind.op-kind}

gen:
DO TRANSACTION WITH FRAME opreq ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE:
   /* Закрытие охватывающего мультивалютного договора */
   IF mMultiCurr EQ YES THEN 
   DO:   
     FIND FIRST Loan WHERE loan.cont-code EQ in-cont-code
                       AND loan.contract  EQ in-contract
                           EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
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

     FIND FIRST Loan WHERE loan.cont-code EQ 
                             (IF {assigned tt-multicur.multicur-cont-code} THEN 
                                 tt-multicur.multicur-cont-code
                              ELSE 
                                 in-cont-code)
                               AND loan.contract  EQ in-contract
                               NO-LOCK NO-ERROR.

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
                     WHEN {assigned tt-multicur.multicur-cont-code}
      in-loan  = if avail loan then loan.doc-ref else in-loan
      in-cont-cur = if avail loan then (if loan.currency = '' then '810'
                                    else  loan.currency) else in-cont-cur.
      .
   /* получить тип договора по номеру */
   Set_type(in-cont-code) . 
   doc:
   FOR EACH op-template OF op-kind WHERE
            op-template.cr-class-code MATCHES "OP*"
   NO-LOCK WITH FRAME opreq
   ON ENDKEY UNDO gen, LEAVE gen
   ON ERROR  UNDO gen, LEAVE gen
   BREAK BY
           op-template.op-template:

      IF op-template.op-status < "√" THEN
         f-close-acct = NO.

      need-valdate = (GetXAttrValueEx("op-template",
                                      op-kind.op-kind + ","
                                      + STRING(op-templ.op-templ),
                                      "ДатаВал",
                                      ?) = "Да").


         /* Проверка на шаблон без создания объектов */
         IF GetXAttrValueEx("op-template",
                            op-template.op-kind + "," + STRING(op-template.op-template),
                            "PrsnTmpl",
                            "Нет") EQ "Нет" THEN
         DO:
         IF NOT mDBO THEN
         DO:
            {g-frame.i
               &DoBefore         = YES
               &OFcash           = YES
               &OP-UNDO          =" tcur = ? .
                                  DELETE op-entry .
                                  DELETE op .
                                  NEXT doc ."
               &DoBeforeAfterSet = "RUN PrintHeader."
            }
         END.
      /* Проверка наличия мультивалютного вклада */
      IF mMultiCurr EQ NO THEN 
      DO:       
        FIND FIRST loan WHERE loan.contract = "dps" AND 
                              loan.cont-code = in-cont-code
                              NO-LOCK NO-ERROR.
        IF AVAILABLE loan AND loan.parent-cont-code NE '' THEN 
        DO:
          MESSAGE "Вклад мультивалютный." SKIP
                  "Для закрытия этого вклада выбирете другую" SKIP
                  "транзакцию и мультивалютный охватываюший договор." SKIP
                  VIEW-AS ALERT-BOX ERROR.
          UNDO gen, LEAVE gen.
        END.
      END.
      IF loan.end-date GT vContractDate THEN
      DO:
         MESSAGE
            "Договор закрывается раньше срока." SKIP
            "Воспользуйтесь транзакцией досрочного закрытия вклада."
            VIEW-AS ALERT-BOX ERROR .
         UNDO gen, LEAVE gen .
      END.
      ELSE
      IF loan.end-date LE vContractDate AND
         vContractDate GT in-op-date    THEN
      DO:
         MESSAGE
            "Плановая дата закрытия не может быть " SKIP
            "больше реальной даты закрытия вклада."
            VIEW-AS ALERT-BOX ERROR .
         UNDO gen, LEAVE gen .
      END.

      RUN CreateFrmFields IN THIS-PROCEDURE (?,"LoanRecid","",STRING(RECID(loan))).

      IF NOT mDBO THEN
      DO:
         {g-frame.i
            &DoDisp         = YES
            &OFcash         = YES
            &ChkBlockAction = "UNDO gen, LEAVE gen."
         }
      END.
      ELSE
      DO:
         IF vContractDate EQ ? THEN 
            vContractDate = in-op-date.
         IF NOT {assigned in-doc-ref}  THEN
            in-doc-ref = loan.doc-ref.
         IF NOT {assigned in-loan}  THEN
            in-loan = loan.doc-ref.
         IF NOT {assigned in-cont-cur}  THEN
            in-cont-cur = mCurr.

         /* создание документа и проводки */
         {cropdoc.i 
            &no-depadd=YES
            &OFcash=Yes 
            &OP-UNDO=" delete op-entry . delete op . next doc ."
            &wrapname=Yes           
         }
         dbo:dbg("G-CRLOAN.I","wop.op-kind:" + (IF AVAIL  wop THEN STRING(wop.op-kind) ELSE "") + 
                              "; wop.op-templ:" + (IF AVAIL wop THEN STRING(wop.op-templ) ELSE "") +
                              "; wop.mfo-needed:" + (IF AVAIL wop THEN STRING(wop.mfo-needed) ELSE "") +
                              "; wop.doc-type:" + (IF AVAIL wop THEN STRING(wop.doc-type) ELSE "")+
                              "; wop.main-db:" + (IF AVAIL wop THEN STRING(wop.main-db) ELSE "")+
                              "; wop.acct-db:" + (IF AVAIL wop THEN STRING(wop.acct-db) ELSE "") + 
                              "; wop.main-cr:" + (IF AVAIL wop THEN STRING(wop.main-cr) ELSE "") +
                              "; wop.acct-cr:" + (IF AVAIL wop THEN STRING(wop.acct-cr) ELSE "") +
                              "; wop.details:" + (IF AVAIL wop THEN STRING(wop.details) ELSE "") +
                           "; wop.con-date :" + (IF AVAIL wop THEN STRING(wop.con-date) ELSE "")).

         naimks = op-entry.symbol.
         IF  AVAILABLE loan 
            AND NOT CheckCustomField (IF AVAIL op-template THEN 
                                         op-template.op-templ 
                                      ELSE ?,
                                      "LoanRecid", 
                                      "") THEN 
         DO:
            RUN CreateFrmFields IN THIS-PROCEDURE (IF AVAIL op-template THEN 
                                                      op-template.op-templ 
                                                   ELSE ?,
                                                   "LoanRecid", 
                                                   "",
                                  STRING(RECID(loan))).
         END.
         /* заполнение содержания документа парсером ProcessDetails */
         IF LOOKUP("ProcessDetails", THIS-PROCEDURE:INTERNAL-ENTRIES) > 0 THEN 
            RUN ProcessDetails IN THIS-PROCEDURE (IF AVAIL wop THEN RECID(wop) 
                                                               ELSE ?, 
                                                               INPUT-OUTPUT op.details).

         {chkblock.i
            &surr   = STRING(in-op-date)
            &msg    = "Вы не имеете права работать в заблокированном операционном дне!"
            &action = "undo gen, leave gen."
         }
         IF op-template.doc-type BEGINS "(" THEN 
         DO:               
            atempl = TRIM(op-template.doc-type,"(").
            atempl = TRIM(atempl,")").
        
            FIND FIRST bOp-templ WHERE bOp-templ.op-kind EQ op-templ.op-kind AND 
                                       bOp-templ.op-templ EQ INT64(atempl) 
                                       NO-LOCK NO-ERROR.
            op.doc-type = ENTRY(1,bOp-templ.doc-type).
         END.

         dbo:dbg("G-CRLOAN.I","op.acct-cat:" + (IF AVAIL op THEN STRING(op.acct-cat) ELSE "") + 
                              "; op.ben-acct:" + (IF AVAIL op THEN STRING(op.ben-acct) ELSE "") +
                              "; op.branch-id :" + (IF AVAIL op THEN STRING(op.branch-id ) ELSE "") +
                              "; op.Class-Code:" + (IF AVAIL op THEN STRING(op.Class-Code) ELSE "")+
                              "; op.contract-date:" + (IF AVAIL op THEN STRING(op.contract-date) ELSE "")+
                              "; op.details:" + (IF AVAIL op THEN STRING(op.details) ELSE "") + 
                              "; op.doc-date:" + (IF AVAIL op THEN STRING(op.doc-date) ELSE "") +
                              "; op.doc-kind:" + (IF AVAIL op THEN STRING(op.doc-kind) ELSE "") +
                              "; op.doc-num:" + (IF AVAIL op THEN STRING(op.doc-num) ELSE "") +
                              "; op.doc-type :" + (IF AVAIL op THEN STRING(op.doc-type) ELSE "") +
                              "; op.filial-id :" + (IF AVAIL op THEN STRING(op.filial-id) ELSE "") +
                              "; op.op :" + (IF AVAIL op THEN STRING(op.op) ELSE "") +
                              "; op.op-date :" + (IF AVAIL op THEN STRING(op.op-date) ELSE "") +
                              "; op.op-kind :" + (IF AVAIL op THEN STRING(op.op-kind) ELSE "") +
                              "; op.op-status :" + (IF AVAIL op THEN STRING(op.op-status) ELSE "") +
                              "; op.op-template :" + (IF AVAIL op THEN STRING(op.op-template) ELSE "") +
                              "; op.user-id :" + (IF AVAIL op THEN STRING(op.user-id) ELSE "")).
         END.
      ASSIGN
         mforeq = op-templ.mfo-needed OR (GetXAttrValueEx('op-template',
                                          op-kind.op-kind + ','
                                          + STRING(op-templ.op-templ),
                                          'МежБанк',
                                          ?) = 'Да')
         cod-ost = Get-Ost-templ(op-kind.op-kind,op-templ.op-templ)
         .

      sset:
      DO ON ERROR UNDO, RETRY ON ENDKEY UNDO gen, LEAVE gen:

         IF op-templ.amt-rub MATCHES '*Нач*' AND cod-ost NE ? THEN
            Set_ost(cod-ost).

         IF op-templ.cr-class-code MATCHES '*kau*' THEN
         DO :
            RUN Kau-Trigger IN h_op (RECID(op-entry),
                                     OUTPUT flager,
                                     YES).
            IF flager NE 0 THEN
               UNDO,RETRY .

            IF AVAILABLE op-entry THEN
               DELETE op-entry .

            ASSIGN
               op.doc-num = 'кау' + STRING(op-template.op-template).
            PAUSE 0 .
         END.
         IF AVAILABLE op-entry THEN
         DO:
            IF NOT mDBO THEN
            DO:
               /* Определяем, участвует ли в проведении 
                  операции кто-либо, кроме владельца вклада */
               IF     AVAIL cacct   
                  AND cacct.contract   EQ "Касса" THEN 
               DO:
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
                  RUN SetSysConf IN h_Base ("_CustType_", mCustType).
                  fler = FALSE.
               END. 
               
               {g-frame.i
                  &DoSet  = YES
                  &OFcash = Yes
               }
            END.
            IF op.op-status BEGINS "А" THEN
               ASSIGN
                  op.op-date       = ?
                  op-entry.op-date = op.op-date
                  .

            IF NOT mDBO THEN 
            DO:
               {op-entry.upd &871=yes
                  &open-undo = "UNDO gen, LEAVE gen"  
               }
            END.
            ELSE 
            DO:
               /* если из ДБО, то при ошибке откатываем документы */
               {op-entry.upd &871=yes 
                  &OffKNF="/*" 
                  &open-undo = "UNDO gen, LEAVE gen"
                  &undo = "undo gen, leave gen"
               } 
            END.
            /* Создание ДР типа PARSEN_<КодРеквизита>*/
            {g-psigns.i}

            /*если вклад закрывается по доверенности*/
            IF NOT mDBO THEN
            DO:
            IF  cacct.contract   EQ "Касса" THEN DO:
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
                  END.
                  ELSE IF AVAILABLE xperson THEN DO:
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
                         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                      ASSIGN
                         proxy-loan.end-date = op.op-date
                         proxy-loan.close-date = op.op-date
                         proxy-loan.loan-status = CHR(251)
                      .
                      RELEASE proxy-loan.
                   END.
                END.
            END.
            END.

            {crs.i}
            IF AVAILABLE op-entry  OR
               CAN-FIND(FIRST xxop WHERE
                              xxop.op-transaction EQ op.op-transaction
                          AND RECID(xxop)         NE RECID(op))
            THEN
            DO:
               IF mforeq AND NOT g-checkbank(vmfo,
                                             mbank-code-type,
                                             vcorr-acct,
                                             op.ben-acct,
                                             OUTPUT result,
                                             OUTPUT msg)
               THEN
               DO:
                  IF op-templ.mfo-needed THEN
                  DO:

                     MESSAGE SKIP msg SKIP(1)
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.

                     CASE RESULT:
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
                           WHEN {&egmissingmfo}  OR
                           WHEN {&egmissingbank} THEN
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
                  {op-type.upd}
               END.

               { op-type.chk }
               {xttrentr.i
                  &OP-BUF       = op
                  &OP-TEMPL-BUF = op-template
               }

               IF AVAILABLE op THEN
               DO:
                  {additem.i str_recids STRING(RECID(op))}
               END.
               IF tcur = ? THEN
                  tcur = op-entry.currency.
               ASSIGN
                  wop.op-recid = RECID(op)
                  wop.acct-db  = op-entry.acct-db
                  wop.acct-cr  = op-entry.acct-cr
                  wop.currency = op-entry.currency
                  wop.amt-cur  = IF op-entry.currency <> "" THEN
                                    op-entry.amt-cur
                                 ELSE
                                    op-entry.amt-rub
                  wop.amt-rub  = op-entry.amt-rub
                  .
            END.
         END.

         Set_type(in-cont-code) .
      END.
      
      {aft-temp.i 
         &aft-undo = " UNDO gen, LEAVE gen."}
         END. /* PrsnTmpl = no */
         ELSE
         DO:
            IF NOT mDBO THEN
            DO:
            /* От формы требуется только ввода номера договора и даты */
               {g-frame.i 
                  &DoBefore         = Yes 
                  &OFcash           = Yes
                  &nocreate         = YES
                  &browse-entry     = YES
                  &NOmfo            = YES
                  &NomfoTotal       = YES
                  &OP-UNDO          = "tcur = ?.           ~                                  
                                       NEXT doc. "
                  &DoBeforeAfterSet = "RUN PrintHeader."
               }
            END.
            {wop-cr.i            
               &Err-ParsSumm   = "IF fler THEN                                                                    UNDO gen, LEAVE gen."
               &NoParsDetails  = YES
            }
         END. /* PrsnTmpl */
   END. /*doc*/

   /* Закрытие договора */
   /* {chk-stat.i}
    Close_Deposit(loan.cont-code,op-kind.op-kind,in-op-date).*/
   IF AVAILABLE loan THEN
   DO:
      RUN dpsclo.p (loan.cont-code,in-op-date,op-kind.op-kind).
      IF RETURN-VALUE = "YES" THEN
         RUN acctclo.p (loan.cont-code,in-op-date).
      ELSE 
         IF (mMultiCurr 
          OR mUndoErr) THEN 
          UNDO gen,LEAVE gen.
   END.
   END.  /* tt-multicur END */
   IF NOT mDBO THEN
   DO:
      {preview2.i
          &stream   = "stream err"
          &filename = _spool1.tmp
      }
   END.
   /* output stream err close . */
END.
IF NOT mDBO THEN
DO:
   {g-print1.i}
END.
RUN DeleteOldDataProtocol IN h_base ("ProxyPickVal"). 
RUN DeleteOldDataProtocol IN h_base ("_CustType_").
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя.document-id").
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя.НаименованиеДокумента").
RUN DeleteOldDataProtocol IN h_base ("ПаспортныеДанныеПостоянногоПолучателя").
RUN SetSysConf IN h_base ("DBO_Without_Interface", "").


OUTPUT STREAM err CLOSE .
HIDE FRAME opreq NO-PAUSE.

{plibdel.i}
DELETE PROCEDURE(hProc).
DELETE PROCEDURE(loan_h) .

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

END PROCEDURE.
/* $LINTFILE='g-dcl.p' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='kuds' */
/* $LINTDATE='31/05/2016 11:07:34.202+03:00' */
/*prosignTRII/Wt6lHcoUqly70Posw*/