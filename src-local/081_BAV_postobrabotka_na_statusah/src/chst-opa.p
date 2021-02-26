/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: CHST-OPA.P
      Comment: Смена статуса документа
   Parameters: дата   (? - дата будет запрошена)
               статус (YES - статус будет запрошен,
                       NO  - статус останется без изменения)
         Uses:
      Used BY:
      Created: 06/03/1996 eagle
     Modified: 28/11/1997 Dima - smarter history IN Chst(Op.I
     Modified: 29/11/1997 nata -  статус проверяется на возможность
                                  проведения обработок
     Modified: 24/07/2002 Gunk - автонумерация в момент изменения статуса.
     Modified: 14/10/2002 Gunk - Интерфейс kau
     Modified: 02/12/2002 Olenka - Смена статуса разрешена только для группы
                                   документов с одинаковыми датами, статусами
                                   и категориями
     Modified: 05/03/2003 Olenka - (0014583) изменила проверки по документам.
     Modified: 09.06.2003 NIK    Контроль документа в целом
     Modified: 25.11.2005 kraw (0047185) возможность автонумерации при помощи Counters
     Modified: 17/09/2009 kraw (0116286) не будем проверять блокировки
*/

DEF INPUT PARAM in-op-date   AS DATE NO-UNDO. /* дата */
DEF INPUT PARAM in-op-status AS LOG  NO-UNDO. /* признак запроса статуса и
                                                 проверок - везде YES */

{defoptr.i new}

{globals.i}             /* Глобальные переменные сессии. */
{def-wf.i new} 
{history.def}
{intrface.get op}       /* Библиотека для работы с документами. */
{intrface.get kau}      /* Библиотека для работы с субсчетами. */
{intrface.get instrum}  /* Библиотека для работы с фин. инструментами. */
{intrface.get separate} /* Библиотека для работы с категориями. */
{intrface.get acct}     /* Библиотека для работы со счетами. */
{intrface.get count}    /* Библиотека для работы со счетчиками. */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get sessions} /* Инструменты обработки смен ВОК. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get db2l}     /* Инструмент для динамической работы с БД. */
{dps-open-lnk.pro}
{op-115fl.def}
{history.def}

{status.tt new}

/* Вставка Плюс банк */
{pqvrn.fun}
/* Конец вставки Плюс банк */
DEF VAR vstatus     LIKE op.op-status NO-UNDO.
DEF VAR cur-op-date AS DATE NO-UNDO.
DEF VAR close-date  AS DATE NO-UNDO. /* Дата закрытия категории документа */
DEF VAR summa       AS DEC NO-UNDO.
DEF VAR summa1      LIKE op-entry.amt-rub NO-UNDO.

DEF VAR rid    AS RECID EXTENT 50 NO-UNDO.
DEF VAR j      AS INT64 NO-UNDO.
DEF VAR flager AS INT64 NO-UNDO.
DEF VAR allsum AS LOG INIT YES NO-UNDO.

DEFINE VARIABLE vTmpStatus    LIKE op.op-status NO-UNDO.
DEFINE VARIABLE vDiffStatuses AS   LOGICAL      NO-UNDO.
DEFINE VARIABLE mDprIdList    AS   CHARACTER    NO-UNDO.
DEFINE VARIABLE mKauDprId     AS   CHARACTER    NO-UNDO.
DEFINE VARIABLE mI            AS   INT64        NO-UNDO.
DEFINE VARIABLE mCorrChk      AS   CHARACTER    INIT "off-corracct"
                                                NO-UNDO.

{tmprecid.def}
{tmprecid.def &PREF = "op-"}
{tmprecid.def &pref = "cl-"}

DEFINE BUFFER cl-tmprecid-1 FOR cl-tmprecid.

DEF BUFFER bufcode FOR code.
DEF BUFFER xop     FOR op.
DEF BUFFER cop     FOR op.
DEF BUFFER cop-en  FOR op-entry.
DEF BUFFER bkau-en FOR kau-entry.

DEF STREAM fout.

DEF VAR lastmess AS CHAR NO-UNDO.
DEF VAR iall     AS INT64  NO-UNDO.
DEF VAR iok      AS INT64  NO-UNDO.
DEF VAR ierr     AS INT64  NO-UNDO.
DEF VAR ierr_prc_cntrl AS INT64  NO-UNDO.

DEFINE VARIABLE vLinkOpStat   LIKE op.op-status NO-UNDO. /* Статус для связанных документов */

DEF VAR ch       AS CHAR NO-UNDO.
DEF VAR rightass AS CHAR NO-UNDO.
DEF VAR rightann AS CHAR NO-UNDO.
DEF VAR rightchg AS CHAR NO-UNDO.
DEF VAR vOpNum   AS INT64  NO-UNDO.
DEF VAR vOpTran  AS INT64  NO-UNDO.
DEF VAR vOpKind  AS CHAR  NO-UNDO.
DEF VAR vStr     AS CHAR NO-UNDO.
DEF VAR vCateg   AS CHAR NO-UNDO.
DEF VAR mAvtStrSmnStatus AS CHARACTER NO-UNDO.

DEFINE VARIABLE mOrigStatus LIKE op.op-status NO-UNDO.

DEFINE BUFFER xop-tmprecid FOR op-tmprecid.
DEFINE BUFFER xlock-op  FOR op.
DEFINE BUFFER bOp       FOR op.

DEFINE QUERY qOp FOR op-tmprecid, bOp.

/*
   Включает в chst(op.i перепроверку статуса документа непосредственно
   перед изменением, чтобы избежать потери сторонних изменений
*/
&GLOBAL-DEFINE recheck-op-status YES

/* Проверки и запрос нового статуса */
ASSIGN 
   rightass = getThisUserXAttrValue('СтатусПрисв')
   rightann = getThisUserXAttrValue('СтатусАннул')
   rightchg = getThisUserXAttrValue('СтатусИзм')
.

IF FGetSetting("КонтрКореспСтат","","") = "Да" THEN mCorrChk = "".

tt1:
DO ON ERROR UNDO,  RETURN
   ON ENDKEY UNDO, RETURN :
  {chstopa.bup}
END.


{setdest.i &cols=168 &stream="stream fout"}

/* Собственно смена статуса у документов */

/* Инициализация процесса протоколирования. */
RUN Init-SysMes IN h_tmess ("СмнСтДок", "", "").

mAvtStrSmnStatus = GetXAttrValueEX( "_user",USERID( "bisquit" ),"СмнСтатАвт","<НЕТ>" ).

IF mAvtStrSmnStatus <> "<НЕТ>" THEN 
RUN PresetAnswers IN h_tmess (mAvtStrSmnStatus).

FIND FIRST code WHERE
           code.class =  "Статус" AND
           code.code =  vstatus NO-LOCK NO-ERROR.
{status.set &1=tt-status &2=code}
FIND FIRST tt-status WHERE
           tt-status.class =  "Статус" AND
           tt-status.code =  vstatus NO-LOCK NO-ERROR.

RUN rid-rest.p (OUTPUT TABLE op-tmprecid).
{empty tmprecid}
{empty cl-tmprecid}

RUN CreateHardLinkClosure(INPUT TABLE op-tmprecid, OUTPUT TABLE cl-tmprecid).
vDiffStatuses = NO.
FOR FIRST cl-tmprecid NO-LOCK,
    FIRST op WHERE RECID(op) = cl-tmprecid.id NO-LOCK:
    ASSIGN
        vTmpStatus    = op.op-status
        vDiffStatuses = YES.
    .
END.
IF vDiffStatuses THEN DO:
    vDiffStatuses = NO.
    FOR EACH cl-tmprecid NO-LOCK,
        FIRST op WHERE
            RECID(op)    =  cl-tmprecid.id AND
            op.op-status <> vTmpStatus
        NO-LOCK:
        RUN Fill-SysMes IN h_tmess ("",
                                    "",
                                    "0",
                                    "Документы, созданные транзакцией №" +
                                    STRING(op.op-transaction) +
                                    ", находятся в разных статусах. " +
                                    "Синхронная смена статуса невозможна").
        vDiffStatuses = YES.
        LEAVE.
    END.
END.

IF NOT vDiffStatuses THEN DO:
    {empty op-tmprecid}
    FOR EACH cl-tmprecid NO-LOCK:
        CREATE op-tmprecid.
        op-tmprecid.id = cl-tmprecid.id.
    END.
END.

_chst_tran:
DO TRANSACTION ON ERROR  UNDO _chst_tran, LEAVE _chst_tran
               ON ENDKEY UNDO _chst_tran, LEAVE _chst_tran:

   FOR EACH xop-tmprecid:
      ASSIGN
         lastmess = ""
         iall     = iall + 1
      .
      FIND FIRST xlock-op WHERE RECID(xlock-op) = xop-tmprecid.id NO-LOCK NO-ERROR.
      IF NOT AVAIL xlock-op THEN DO:
         vStr = "Запись удалена другим пользователем".
         RUN putlog(vStr).
      END.
   END.

   OPEN QUERY qOp FOR EACH op-tmprecid,
        FIRST bOp WHERE RECID(bOp) =  op-tmprecid.id
           EXCLUSIVE-LOCK.
   GET FIRST qOp NO-WAIT.

   DO WHILE AVAIL(op-tmprecid):
      IF LOCKED(bOp) THEN DO: 
         vStr = "Запись редактируется другим пользователем".
         WhoLocks2(RECID(bop), (BUFFER bOp:TABLE) , INPUT-OUTPUT vStr).
         RUN putlog(vStr).
         DELETE op-tmprecid.
      END.
      GET NEXT qOp NO-WAIT.
   END.  

   GET FIRST qOp.
   tt:
      DO WHILE AVAILABLE op-tmprecid
      ON ERROR  UNDO tt, LEAVE tt
      ON ENDKEY UNDO tt, LEAVE tt:

      IF RETRY THEN DO:
         GET NEXT qOp.
         NEXT tt.
      END.

      lastmess = "".

      RUN CheckOpRight IN h_base(op-tmprecid.id,?,"ChgSts") NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         FIND FIRST op WHERE
              RECID(op) =  op-tmprecid.id NO-LOCK NO-ERROR.
         RUN putlog(RETURN-VALUE).      
         UNDO tt, RETRY tt.
      END.

      FIND FIRST op WHERE RECID(op) = op-tmprecid.id NO-LOCK NO-ERROR.
      
      IF NOT IsOsnLinkDoc(op.op) THEN DO:
         FIND FIRST op-kind WHERE op-kind.op-kind = op.op-kind NO-LOCK NO-ERROR.
         IF AVAIL op-kind AND GetXAttrValueEx("op-kind", op-kind.op-kind, "СвязьДок", "Нет") = "Осн" THEN 
         DO:
            vStr = "С документом " + 
                   (IF {assigned op.doc-num} THEN op.doc-num
                                             ELSE STRING(op.op)) +
                   " связан основной документ. Проводка осуществляется только через основной документ!".
            RUN Fill-SysMes IN h_tmess ("",
                                        "",
                                        "0",
                                        vStr).
            RUN PutLog(vStr).
            UNDO tt, RETRY tt.
         END.
      END.
      
      IF GetStatusSessions(INT64(GetXAttrValueEx("op",STRING(op.op),"Dpr-Id",?))) =  "ЗАКРЫТА" THEN
      DO:
         RUN PutLog("В закрытой смене данная операция не возможна!").
         UNDO tt, RETRY tt.
      END.

      mDprIdList = "".
      IF (FGetSetting("СтатНиз", "", "ФПС") >  vStatus) THEN
      DO:
         FOR EACH bkau-en OF op
            WHERE CAN-DO("КодСмены*,ПартияЦМ,ФутлярЦМ",bkau-en.kau-id)
         NO-LOCK:
            mI = IF bkau-en.kau-id BEGINS "КодСмены" THEN 1 ELSE 2.
            mKauDprId = STRING(INT64(ENTRY(mI,bkau-en.kau))) NO-ERROR.
            IF (mKauDprId >  '0') AND 
               (NOT CAN-DO(mDprIdList, mKauDprId))
            THEN
               {additem.i mDprIdList mKauDprId}
         END.
         DO mI = 1 TO NUM-ENTRIES(mDprIdList):
            IF GetStatusSessions(INT64(ENTRY(mI,mDprIdList))) <> "ОТКРЫТА" THEN
            DO:
               RUN PutLog("Данная операция возможна только при открытой~nсмене кассира, передавшего остатки!").
               UNDO tt, RETRY tt.
            END.
         END.
      END.

      /*------------------------------------------------------------------------------*/
      IF vStatus = "А" AND NOT CAN-DO(rightann,op.op-status) THEN
      DO:
         RUN PutLog("У вас нет прав работать с документами такого статуса.").
         UNDO tt, RETRY tt.
      END.

      IF vStatus           = "А"                 AND
         op.user-inspector = USERID( "bisquit" ) AND
         GetXAttrValue( "_user",USERID( "bisquit" ),"АннулДок" ) = "Нет" THEN
      DO:
         RUN PutLog("Вы не можете аннулировать документы, " +
                    "которые вы контролируете.").
         UNDO tt, RETRY tt.
      END.
      /*------------------------------------------------------------------------------*/

      FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
      
/* Вставка Плюс банк */
      DEFINE VARIABLE mMaxSum      AS DECIMAL    NO-UNDO.
      DEFINE VARIABLE mMaskAcct    AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE mResident    AS LOGICAL    NO-UNDO.
      ASSIGN
         mMaxSum   = DECIMAL(GetCode("GT100","MaxSum"))
         mMaskAcct = GetCode("GT100","MaskAcct").
      IF AVAIL(op) 
         AND AVAIL(op-entry) 
         AND vstatus GT "ФБК" 
         AND vstatus GT op.op-status THEN
      DO:
         {find-act.i
		      &acct = op-entry.acct-db
		      &curr = op-entry.currency
		   }   
         
         mResident = IsResident(acct.cust-cat,acct.cust-id).
         
         
         IF AVAIL(acct) 
            AND acct.contract EQ "Расчет"
            AND acct.cust-cat EQ "Ю"
            AND mResident     EQ YES
            AND op-entry.amt-rub GT mMaxSum
            AND CAN-DO(mMaskAcct,op-entry.acct-cr) THEN
         DO: 
            IF USERID("bisquit") EQ "O0400MGK" 
            OR USERID("bisquit") EQ "TMN_VAA"
            OR USERID("bisquit") EQ "OKO_AMR"
            OR USERID("bisquit") EQ "K0400MVS"
            OR USERID("bisquit") EQ "OKO_REV"
            OR USERID("bisquit") EQ "I0400STS" THEN
            DO:
               pick-value = "1". 
               RUN messmenu.p(10 ,
                  "[ СООБЩЕНИЕ ]",
                  "Сумма превышает " + TRIM(STRING(mMaxSum,">>>,>>>,>>>,>>9.99")) + ".~n" +
                  "Неоходимо сообшить в службу ФМ!",
                  "Отменить,Продолжить").
               IF pick-value EQ "1" THEN
               DO:
                  UNDO tt, RETRY tt.
               END.
            END.
            ELSE            
            DO:
               RUN PutLog("Обработка отменена, сумма превышает " + TRIM(STRING(mMaxSum,">>>,>>>,>>>,>>9.99")) + "!").
               UNDO tt, RETRY tt.
            END.
         END.         
      END.
/* Конец вставки Плюс банк */
      IF (NOT CAN-DO(tt-status.doc-type,op.doc-type) AND
                      tt-status.doc-type  <> ""     )
      OR (NOT CAN-DO(tt-status.op-kind,op.op-kind  ) AND
                      tt-status.op-kind   <> ""     )
      OR ((IF AVAIL op-entry THEN NOT CAN-DO(tt-status.type,op-entry.type)
                              ELSE TRUE)  AND tt-status.type <> "") THEN DO:
         RUN putlog("Маски кода, транзакции или техн. платежа статуса не соответствуют коду, транзакции или техн. платежа док.!").
         UNDO tt, RETRY tt.
      END.
      IF vstatus =  "А" AND
          OpIsSvod(RECID(op)) THEN
      DO:
          RUN putlog("Документ входит в группу сводных. Его нельзя аннулировать!").
          UNDO tt, RETRY tt.
      END.

      IF NOT allsum               AND
          in-op-status             AND
          close-date < cur-op-date THEN DO:

         FIND op-entry OF op NO-LOCK NO-ERROR.
         IF ambig op-entry THEN DO:
            RUN putlog("Многострочный документ нельзя переводить частично!").
            UNDO tt, RETRY tt.
         END.
         IF AVAIL op-entry THEN
smf:
         DO ON ERROR UNDO, LEAVE smf ON ENDKEY UNDO , LEAVE smf WITH FRAME edt:
            summa  = IF op-entry.curr =  "" THEN op-entry.amt-rub ELSE op-entry.amt-cur.
            summa1 = summa.
            DISPLAY "Проводить всю сумму?"
                     summa1
               WITH FRAME edt CENTERED ROW 10 OVERLAY no-label.
            UPDATE  summa1
                     VALIDATE (summa1 <> 0 AND
                               (IF summa >  0 THEN summa1 <= summa AND summa1 > 0
                                              ELSE summa1 >= summa AND summa1 < 0),
                     "Сумма должна быть меньше или равна сумме проводки и не равна 0")
                WITH FRAME edt.
            HIDE FRAME edt.
         END.
      END.

      IF summa = summa1 THEN DO:
         FIND FIRST op WHERE recid(op) = op-tmprecid.id EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         {chst(op.i &proc_cntrl_undo = "DO: RUN putlog_1('Ошибка проводок').     UNDO tt, RETRY tt. END."
                    &open-undo = "DO: IF RETURN-VALUE NE 'no_mess' THEN RUN putlog('Ошибка проводок: ' + (IF RETURN-VALUE > '' THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1)) ). UNDO tt, RETRY tt. END. "
                    &kau-undo  = "DO: RUN putlog('Ошибка субаналитики'). UNDO tt, RETRY tt. END. "
                    &xattr-undo="run putlog('У документа не заполнены обязательные доп. реквизиты.'). undo tt, RETRY tt "
                    &undo      = "DO: RUN putlog('Ошибка документа').    UNDO tt, RETRY tt. END. "
                    &del-undo  = "UNDO tt, RETRY tt"
                    &visa      = yes
         }                                                                      
         RUN AnumStat.
      END.
      ELSE DO: /* расщепляем */
         FIND cop WHERE recid(cop) =  recid(op) EXCLUSIVE-LOCK no-wait NO-ERROR.
         IF NOT AVAIL cop THEN DO:
            RUN putlog("Запись редактируется другим или удалена.").
            UNDO tt, RETRY tt.
         END.
         FIND FIRST cop-en OF cop EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF cop-en.curr =  "" THEN
         ASSIGN
            cop-en.amt-rub = summa - summa1
            summa = 0
         .
         ELSE DO:
            cop-en.amt-cur = summa - summa1.
            cop-en.amt-rub = CurToBase("УЧЕТНЫЙ",
                                       op-entry.currency,
                                       (IF op-entry.value-date <> ? THEN op-entry.value-date
                                                                    ELSE op-entry.op-date),
                                        cop-en.amt-cur).
            summa  = summa1.
            summa1 = CurToBase("УЧЕТНЫЙ",
                               op-entry.currency,
                               (IF op-entry.value-date <> ? THEN op-entry.value-date
                                                            ELSE op-entry.op-date),
                               summa ).
         END.
         RUN kauopdel IN h_kau (RECID (cop-en)).
         flager = 1.

         RUN SetSysConf IN h_base ("nokbs", "nokbs").
         RUN Check-Op-Entry IN h_op (BUFFER cop-en,cop-en.op-date,NO,"",mCorrChk,OUTPUT flager).
         RUN SetSysConf IN h_base ("nokbs", "").
         IF flager > 0 THEN DO:
            RUN putlog("Ошибка в op-enupd.p").
            UNDO tt, RETRY tt.
         END.
         IF flager >= 0 THEN DO:
            RUN SetSysConf IN h_base ("nokbs", "nokbs").
            RUN Kau-Trigger IN h_op (recid(cop-en),OUTPUT flager,YES).
            RUN SetSysConf IN h_base ("nokbs", "").
            IF flager <> 0 THEN DO:
               RUN putlog("Ошибка в Kau-Trigger, код ошибки = " + string(flager)).
               UNDO tt, RETRY tt.
            END.
         END.

         CREATE op.
         {op(sess).cr &op-status=vstatus}
         ASSIGN
            op.op-status     = vstatus
            op.ben-acct      = cop.ben-acct
            op.branch-id     = cop.branch-id
            op.details       = cop.details
            op.doc-date      = cop.doc-date
            op.doc-kind      = cop.doc-kind
            op.doc-num       = cop.doc-num
            op.doc-type      = cop.doc-type
            op.misc[1]       = cop.misc[1]
            op.misc[2]       = cop.misc[2]
            op.inn           = cop.inn
            op.name-ben      = cop.name-ben
            op.op-kind       = cop.op-kind
            op.due-date      = cop.due-date
            op.op-value-date = cop.op-value-date
            op.due-date      = cop.due-date
            op.acct-cat      = cop.acct-cat
         .
         /* op-bank op-exp-imp */
         CREATE op-entry.
         buffer-copy cop-en TO op-entry ASSIGN
            op-en.op        = op.op
            op-en.op-date   = op.op-date
            op-en.user-id   = op.user-id
            op-en.op-status = op.op-status
            op-en.acct-cat  = op.acct-cat
            op-en.amt-cur   = summa
            op-en.amt-rub   = summa1
         .

         FIND FIRST code WHERE
              code.class =  "Статус" AND
              code.code =  vstatus NO-LOCK NO-ERROR.
         IF code.misc[1]       =  ""   OR
            TRIM(code.misc[1]) =  "да" THEN DO:
            {op-entry.upd
               &open-undo = "DO: RUN putlog('Ошибка проводок').     UNDO tt, RETRY tt. END. "
               &kau-undo  = "DO: RUN putlog('Ошибка субаналитики'). UNDO tt, RETRY tt. END. "
               &undo      = "DO: RUN putlog('Ошибка документа').    UNDO tt, RETRY tt. END. "
               &Ofnext    = "/*"
            }
         END.
      END.
      ASSIGN 
         vOpNum  = op.op
         vOpKind = op.op-kind
         vOpTran = op.op-transaction 
      .

      RELEASE op NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN putlog('Ошибка документа: ' + RETURN-VALUE). 
         UNDO tt, RETRY tt.
      END.

      FIND FIRST op-kind WHERE op-kind.op-kind = vOpKind NO-LOCK NO-ERROR.
      IF AVAIL op-kind AND GetXAttrValueEx("op-kind", op-kind.op-kind, "СвязьДок", "Нет") = "Осн" THEN 
      DO:
         /* Определяем, в каком статусе должны быть связанные документы */
         vLinkOpStat = ''.
         vTmpStatus = FGetSetting("СтатусПров",?,""). 
         IF vStatus BEGINS 'А' THEN vLinkOpStat = vStatus.
         ELSE IF vStatus >= vTmpStatus THEN vLinkOpStat = '√√'.

         vTmpStatus = vStatus. /* Запоминаем статус основного док-та */
         vStatus = vLinkOpStat.
         FOR EACH cop WHERE cop.op-transaction = vOpTran NO-LOCK:
            IF IsOsnLinkDoc(cop.op) THEN NEXT.
            /* Если нужно перевести в первоначальный статус этого док-та */
            IF vLinkOpStat = '' THEN DO:  
               FIND FIRST history WHERE history.file-name = 'op' 
                                    AND history.field-ref = string(cop.op) 
                                    AND history.field-val MATCHES "*op-status*" 
                                    AND history.modify = '{&hi-w}' 
               NO-LOCK NO-ERROR.
               IF AVAIL history
                  THEN vStatus = ENTRY(LOOKUP("op-status",history.field-val) + 1,history.field-value).
                  ELSE vStatus = cop.op-status.  /* Статус документа не менялся */
            END.
            IF cop.op-status <> vStatus THEN DO:
               FIND FIRST op WHERE op.op = cop.op EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
               {chst(op.i &proc_cntrl_undo = "DO: RUN putlog_1('Ошибка проводок связанного документа').     UNDO tt, RETRY tt. END."
                          &open-undo = "DO: RUN putlog('Ошибка проводок связанного документа').     UNDO tt, RETRY tt. END. "
                          &kau-undo  = "DO: RUN putlog('Ошибка субаналитики связанного документа'). UNDO tt, RETRY tt. END. "
                          &xattr-undo="run putlog('У документа не заполнены обязательные доп. реквизиты.'). undo tt, RETRY tt "
                          &undo      = "DO: RUN putlog('Ошибка смены статуса связанного документа').    UNDO tt, RETRY tt. END. "
                          &del-undo  = "UNDO tt, RETRY tt"
               }
               ELSE DO:
                  IF LOCKED(op) THEN DO:
                     vStr = "Запись редактируется другим пользователем".           
                     WhoLocks2(RECID(cop), (BUFFER cop:TABLE) , INPUT-OUTPUT vStr).
                     RUN putlog(vStr).                                             
                  END.
                  ELSE RUN putlog("Запись удалена другим пользователем.").                                             
                  NEXT.
               END.
            END.
         END.  /* FOR EACH cop */
         vStatus = vTmpStatus.
      END.

      /* в МФ режиме по настройке сменим статус у связанных документов */
      IF     shMode 
      AND fGetSetting("СинхрСтатМФДок",?,"Нет") = 'Да' THEN
      /* предполагается что связка межфилиальных документов находится в obj-transcation */
      FOR EACH obj-transaction WHERE
               obj-transaction.op-transaction =  vOpTran 
           AND obj-transaction.FILE-NAME      =  "op" 
           AND obj-transaction.surrogate      <> STRING(vOpNum) USE-INDEX op-transaction
      NO-LOCK:
         FIND FIRST op WHERE 
                    op.op = INT64(obj-transaction.surrogate) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF NOT AVAIL op THEN DO:
            IF LOCKED(op) THEN DO:
               FIND FIRST op WHERE 
                          op.op = INT64(obj-transaction.surrogate) NO-LOCK NO-ERROR.
               IF AVAIL op  THEN DO:
                  RUN putlog('Ошибка смены статуса связанного документа').    
                  UNDO tt, RETRY tt.
               END.
            END.
         END. 
         ELSE DO:
            IF NOT(op.op-status BEGINS "А") AND 
               op.acct-cat = vCateg THEN DO:
               {chst(op.i &proc_cntrl_undo = "DO: RUN putlog_1('Ошибка проводок связанного документа').     UNDO tt, RETRY tt. END."
                       &open-undo = "DO: RUN putlog('Ошибка проводок связанного документа').     UNDO tt, RETRY tt. END. "
                       &kau-undo  = "DO: RUN putlog('Ошибка субаналитики связанного документа'). UNDO tt, RETRY tt. END. "
                       &xattr-undo="run putlog('У документа не заполнены обязательные доп. реквизиты.'). undo tt, RETRY tt "
                       &undo      = "DO: RUN putlog('Ошибка смены статуса связанного документа').    UNDO tt, RETRY tt. END. "
                       &del-undo  = "UNDO tt, RETRY tt"
               } 
               RUN AnumStat.
            END.
         END.
      END.
/* Вставка Плюс банк. Запуск процедур пост-обработки при успешной смене статуса */
      IF (bOp.op-status EQ vstatus)
      THEN RUN pb_poststatus.p (BUFFER bOp).
/* Конец вставки Плюс банк */
      iok = iok + 1.
      IF AVAIL op-tmprecid THEN
         DELETE op-tmprecid.
      RUN DeleteOldDataProtocol in h_base ("PrevSelectedDprId").
      GET NEXT qOp.
   END. 
END.
CLOSE QUERY qOp.
HIDE FRAME dateframe NO-PAUSE.
HIDE FRAME edt NO-PAUSE.



/* Вывод протокола */
IF iall = 1 THEN DO:
   OUTPUT STREAM fout CLOSE.
   IF lastmess <> "" THEN
      MESSAGE lastmess VIEW-AS ALERT-BOX ERROR.
END.
ELSE DO:
   ierr = iall - iok.
   PUT STREAM fout UNFORMATTED
      SKIP(1) "        ИТОГО:" SKIP
      "документов для перевода   :" iall SKIP
      "новый статус              :" vstatus " (" tt-status.name ")" SKIP
      "документов переведено     :" iok SKIP
      "ошибок перевода документов:" ierr SKIP
      "ошибок процедуры контроля :" ierr_prc_cntrl SKIP.
   {preview.i &stream="stream fout"}
END.

/* Завершение процесса протоколирования. */
RUN End-SysMes IN h_tmess.
RUN DeleteOldDataProtocol in h_base ("PrevSelectedDprId").

{intrface.del}          /* Выгрузка инструментария. */ 

/* Внутренние процедуры */

DEFINE FRAME ferr header "ОШИБКИ ПЕРЕВОДА ДОКУМЕНТОВ" WITH DOWN WIDTH 168.

PROCEDURE putlog.
  DEF INPUT PARAM in-mess AS CHAR NO-UNDO.
  lastmess = in-mess.
  DISPLAY STREAM fout
     iall LABEL "N" FORMAT ">>>9"
     op.doc-num WHEN AVAIL op FORMAT "x(12)"
     "id " + string(op-tmprecid.id) WHEN NOT AVAIL op @ op.doc-num
     op.op-status WHEN AVAIL op
     op.op-date WHEN AVAIL op
     op.user-id WHEN AVAIL op
     lastmess LABEL "ОШИБКА" FORMAT "x(120)"
     WITH FRAME ferr.
  DOWN STREAM fout 1 WITH FRAME ferr.
  ierr = ierr + 1.
END PROCEDURE.

PROCEDURE putlog_1.
  DEF INPUT PARAM in-mess AS CHAR NO-UNDO.
  lastmess = in-mess.
  DISPLAY STREAM fout
     iall LABEL "N" FORMAT ">>>9"
     op.doc-num WHEN AVAIL op FORMAT "x(12)"
     "id " + string(op-tmprecid.id) WHEN NOT AVAIL op @ op.doc-num
     op.op-status WHEN AVAIL op
     op.op-date WHEN AVAIL op
     op.user-id WHEN AVAIL op
     lastmess LABEL "ОШИБКА" FORMAT "x(120)"
     WITH FRAME ferr.
  DOWN STREAM fout 1 WITH FRAME ferr.
  ierr_prc_cntrl = ierr_prc_cntrl + 1.
END PROCEDURE.


PROCEDURE AnumStat:
   {anumstat.i}
END PROCEDURE.

/*
  Вычисление замыкания op-tmprecid относительно жёсткой связи
  между документами с учётом доп. реквизита транзакций "СвязьДок"
*/
PROCEDURE CreateHardLinkClosure.
    DEFINE INPUT  PARAMETER TABLE FOR op-tmprecid.
    DEFINE OUTPUT PARAMETER TABLE FOR cl-tmprecid.

    DEFINE VARIABLE vOpList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vI      AS INT64     NO-UNDO.

    DEFINE BUFFER op  FOR op.
    DEFINE BUFFER cop FOR op.

    FOR EACH op-tmprecid NO-LOCK,
        FIRST op WHERE RECID(op) = op-tmprecid.id NO-LOCK:
        RUN GetLinkedOps IN h_op (op.op, YES, OUTPUT vOpList).
        vOpList = STRING(op.op) +
                  (IF {assigned vOpList} THEN ("," + vOpList) ELSE "").
        DO vI = 1 TO NUM-ENTRIES(vOpList):
            FIND FIRST cop WHERE
                cop.op = INT64(ENTRY(vI, vOpList))
            NO-LOCK NO-ERROR.
            IF AVAILABLE cop AND
               NOT CAN-FIND(FIRST cl-tmprecid WHERE cl-tmprecid.id = RECID(cop))
            THEN DO:
                CREATE cl-tmprecid.
                cl-tmprecid.id = RECID(cop).
            END.
        END.
    END.
END PROCEDURE.

/* Заполнение RETURN-VALUE для вывода ошибки в протокол */
PROCEDURE SetReturnValue PRIVATE:
   DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.

   RETURN iStr.
END PROCEDURE.
/* $LINTFILE='chst-opa.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.316+03:00' */
/*prosignWsS760WksS8WhsJhOqW28Q*/