/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2000 ТОО "Банковские информационные системы"
     Filename: i-mg-aсс.p
      Comment: Импорт счетов
         Uses:
      Used BY:
      Created: 28.06.2000 Mike
     Modified:
*/
{i-mg-tag.def}
{i-mg-tag.pro}
{intrface.get refer}
{pb_logit.i}

DEF VAR mAcct      AS CHAR NO-UNDO. /*Лицевой счет         */
DEF VAR mAcctF     AS CHAR NO-UNDO. /*Лицевой счет         */
DEF VAR mAcctCat   AS CHAR NO-UNDO. /*Категория лиц. счета */
DEF VAR mBAcct     AS INT64  NO-UNDO. /*Счет второго порядка */
DEF VAR mBankINN   AS CHAR NO-UNDO. /*Д.Р. Инн             */
DEF VAR mCloseDate AS DATE NO-UNDO. /*Дата закрытия        */
DEF VAR mContrAcct AS CHAR NO-UNDO. /*Парный счет          */
DEF VAR mCount     AS INT64  NO-UNDO. /*Счетчик              */
DEF VAR mCurr      AS CHAR NO-UNDO. /*Валюта - цифра       */
DEF VAR mContract  AS CHAR NO-UNDO. /*Назначение счета     */
DEF VAR mDate      AS DATE NO-UNDO. /*Дата открытия        */
DEF VAR mDiasoftId AS CHAR NO-UNDO. /*Код клиента - диасофт*/
DEF VAR mDoAfter   AS CHAR NO-UNDO. /*действия после запуска процедуры*/
DEF VAR mError     AS CHAR NO-UNDO. /*Ошибки создания      */
DEF VAR iError     AS INT64  NO-UNDO INIT 0. /*Счетчик ошибок */
DEF VAR mKey       AS INT64  NO-UNDO. /*Ключ                 */
DEF VAR mName      AS CHAR NO-UNDO EXTENT 4. /* Наименование */
DEF VAR mSide      AS CHAR NO-UNDO. /*Актив                */
DEF VAR mSignsCode AS CHAR NO-UNDO. /*Код Д.Р.             */
DEF VAR mStatus    AS CHAR NO-UNDO. /*Статус               */
DEF VAR mSurr      AS CHAR NO-UNDO. /*Код клиента из Д.Р.  */
DEF VAR mSymRep    AS CHAR NO-UNDO. /*Д.Р. Символ отчета   */
DEF VAR mUserId    AS CHAR NO-UNDO. /*Код пользователя     */
DEF VAR mLog       AS CHAR NO-UNDO. /*Протокол ошибок      */
mLog = "/home2/bis/quit41d/log/way4/acct-"
     + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")
     + "-" + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".log".
DEF BUFFER xacct FOR acct.

/*для протокола закрытия счета********/
DEF VAR gLogProc    AS handle NO-UNDO.
DEF VAR gLogMessage AS log    NO-UNDO.

RUN setsysconf IN h_base("gLogProc",STRING(THIS-PROCEDURE:HANDLE)) NO-ERROR.
RUN setsysconf IN h_base("gLogMessage","YES") NO-ERROR.

{getlogp.i}
DEF TEMP-TABLE tt-log NO-UNDO
   FIELD str AS CHAR.
/* *********************************** */

/* Дата закрытия счета */
FUNCTION GetCloseDate returns DATE:
   RETURN mCloseDate.
END FUNCTION.

/*Дата открытия счета*/
FUNCTION GetOpenDate returns DATE:
    RETURN  IF mDate EQ ?
            THEN gend-date
            ELSE mDate.
END FUNCTION.

/* ============================================================================ */
RUN AccessMailKindTempl (iMailNum, iOpTempl).

mFirst = YES.
FOR EACH tt
    WHERE tt.Grp EQ in-group:

    IF mFirst
    THEN DO:
        {empty ttsigns}
        ASSIGN
         mBranch    = ""
         mCurr      = GetXattrValueEx("op-template",
                                      op-temp.op-kind + "," + string(iOpTempl),
                                      "l-currency",
                                      "")
         mBAcct     = INT64(GetXattrValueEx("op-template",
                                          op-temp.op-kind + "," +
                                             string(iOpTempl),
                                          "bal-acct",
                                          "0")
                         )
         mAcct      = ""
         mName[1]   = ""
         mName[2]   = ""
         mName[3]   = ""
         mName[4]   = ""
         mDate      = ?
         mAcctCat   = op-templ.acct-cat
         mSide      = ""
         mDiasoftId = ""
         mUserId    = ""
         mStatus    = ""
         mCloseDate = ?
         mContrAcct = ""
         mBankINN   = ""
         mSymRep    = ""
         mError     = ""
         mCount     = 0
         mSurr      = ""
         mFirst     = NO
      .
    END.

    CASE tt.tag:
      WHEN "ФИЛИАЛ"     THEN mBranch    = tt.val.
      WHEN "ВАЛЮТА"     THEN mCurr      = GetCurrency(tt.val).
      WHEN "СИН/СЧЕТ"   THEN mBAcct     = INT64(tt.val).
      WHEN "СЧЕТ"       THEN mAcct      = tt.val.
      WHEN "ИМЯ1"       THEN mName[1]   = tt.val.
      WHEN "ИМЯ2"       THEN mName[2]   = tt.val.
      WHEN "ИМЯ3"       THEN mName[3]   = tt.val.
      WHEN "ИМЯ4"       THEN mName[4]   = tt.val.
      WHEN "ОТКРЫТ"     THEN mDate      = DATE(tt.val).
      WHEN "ОБЛАСТЬ"    THEN mAcctCat   = GetEntryCat(tt.val).
      WHEN "ТИП"        THEN mSide      = IF tt.val BEGINS "А" THEN "А"
                                          ELSE IF tt.val BEGINS "П" THEN "П"
                                          ELSE tt.val.
      WHEN "КЛИЕНТ1"    THEN mDiasoftId = tt.val.
      WHEN "ГРПОТВИСП"  THEN mUserId    = tt.val.
      WHEN "БЛОКИРОВКА" THEN mStatus    = tt.val.
      WHEN "ЗАКРЫТ"     THEN mCloseDate = DATE(tt.val).
      WHEN "ПАРНЫЙСЧТ"  THEN mContrAcct = tt.val.
      WHEN "ВНУТРИНН"   THEN mBankINN   = tt.val.
      WHEN "Симв.ОТЧ."  THEN mSymRep    = tt.val.
      WHEN "#END"     OR
      WHEN "%END"     THEN
      DO:
         mFirst = YES.
         RUN CreateAcct.

         /* Протокол для отправки по почте */
         IF (mError NE "")
         THEN DO:
            IF (iError EQ 0)
            THEN RUN LogIt(CODEPAGE-CONVERT("Ошибки импорта счетов WAY4 :", "1251"), mLog).
            iError = iError + 1.
            RUN LogIt(CODEPAGE-CONVERT(mError, "1251"), mLog).
         END.
         ELSE DO:
            {find-act.i &bact = xacct
                        &acct = mAcct
                        &curr = mCurr
            }
            IF (NOT AVAIL xacct)
            THEN DO:
               RUN LogIt(CODEPAGE-CONVERT("Не создан счет " + mAcct, "1251"), mLog).
/*             RUN pb_mail.p ("a.borisov,v.ignatchenko,t.stenina", "WAY4 - acct error", "", mLog)). */
               RUN pb_mail.p ("a.borisov", "WAY4 - acct error", "", mLog)).
            END.
         END.
      END.
      OTHERWISE IF (NOT (tt.tag BEGINS "#"  OR
                         tt.tag BEGINS "%"))
                   AND {assigned tt.tag}
                   AND {assigned tt.val}  THEN                 /* ДопРеквизит */
         RUN CreatettSigns("acct" + mAcctCat,tt.tag,tt.val).
    END CASE.
    DELETE tt.
END.

IF (iError NE 0)
THEN DO:
   RUN mail-add.p ("i-mg-acc-w4").
   RUN pb_mail.p (RETURN-VALUE, "WAY4 - BIS: Account export error!", "", mLog)).
END.

{intrface.del}
RETURN.

/*----------------------------------------------------------------------------*/
/* Создание счета                                                             */
/*----------------------------------------------------------------------------*/
PROCEDURE CreateAcct:
   DEF VAR cWho       AS CHAR NO-UNDO. /* Сообщение о блокировке */
   DEF VAR lLocked    AS LOG  NO-UNDO.
   DEF VAR cTab       AS CHAR NO-UNDO. /* person/cust-corp    */
   DEF VAR rRecID     AS RECID NO-UNDO.
   DEF VAR I          AS INT  NO-UNDO.

    CRT-ACCT:
    DO /* TRANSACTION
        ON ERROR UNDO  CRT-ACCT,
                 RETRY CRT-ACCT */ :

        FIND FIRST bal-acct
            WHERE bal-acct.bal-acct EQ mBAcct
            NO-LOCK NO-ERROR.
        {find-act.i &bact = xacct
                    &acct = mAcct
                    &curr = mCurr
        }
        IF (mDiasoftId EQ "")
        THEN mCount = 0.
        ELSE IF (mDiasoftId BEGINS "U")
        THEN DO:
            mCount = 2.
            mDiasoftId = SUBSTRING(mDiasoftId, 2).
        END.
        ELSE mCount = 1.

        mSurr  = IF (mDiasoftId EQ "") THEN ? ELSE mDiasoftId.

        /*проверка л.с.*/
        {i-mg-acc-w4.upd}

        IF (mError = "")
        THEN DO:
            mDoAfter = "000".
            IF      AVAIL xacct
                AND GetCloseDate() NE ?
            THEN DO:            /*закрытие счета*/
                RUN AcctClose(OUTPUT mDoAfter).
                IF mDoAfter EQ "DoAfter1"
                THEN /* UNDO CRT-ACCT, RETRY CRT-ACCT. */
                DO:
                    ASSIGN
                        count-err   = count-err + 1
                        mError      = IF (mError EQ "") THEN
                                      "1 Невозможно закрыть лицевой счет " + mAcct
                                    + "~n Причина ошибки: " + ERROR-STATUS:get-message(1) + "|" + STRING(ERROR-STATUS:ERROR)
                                      ELSE mError
                        .
                    RUN AddMess(mError).
                    LEAVE CRT-ACCT.
                END.
            END.
            ELSE DO:            /* создание счета */
                /* Проверяем блокировку клиента */
                CASE mCount:
                    WHEN 1 THEN DO:
                        FIND FIRST person
                            WHERE (person.person-id = INT64(mSurr))
                            NO-LOCK NO-ERROR.
                        cTab   = "person".
                        rRecID = RECID(person).
                    END.
                    WHEN 2 THEN DO:
                        FIND FIRST cust-corp
                            WHERE (cust-corp.cust-id = INT64(mSurr))
                            NO-LOCK NO-ERROR.
                        cTab   = "cust-corp".
                        rRecID = RECID(cust-corp).
                    END.
                END CASE.

                lLocked = YES.
                DO I = 1 TO 60 WHILE lLocked:
                    cWho = "".
                    WhoLocks2(rRecID, cTab, INPUT-OUTPUT cWho).
                    IF (LENGTH(cWho) = 56)
                    THEN lLocked = NO.
                    ELSE DO:
                        RUN AddMess("Клиента блокирует " + ENTRY(1, SUBSTRING(cWho, INDEX(cWho, "Пользователь:") + 14), "~n")).
                        PAUSE 10.
                    END.
                END.
                RUN AddMess("Клиент разблокирован").

                RUN AcctOpen(OUTPUT mDoAfter) NO-ERROR.
                IF mDoAfter EQ "DoAfter1"
                THEN /* UNDO CRT-ACCT, RETRY CRT-ACCT. */
                DO:
                    ASSIGN
                        count-err   = count-err + 1
                        mError      = IF (mError EQ "") THEN
                                      "2 Невозможно создать лицевой счет " + mAcct
                                    + "~n Причина ошибки: " + ERROR-STATUS:get-message(1) + "|" + STRING(ERROR-STATUS:ERROR)
                                      ELSE mError
                        .
                    RUN AddMess(mError).
                    LEAVE CRT-ACCT.
                END.
            END.
        END.
        ELSE RUN AddMess(mError).
    END.
END PROCEDURE.

/* процедура закрытия счета*/
PROCEDURE AcctClose:
   DEF OUTPUT PARAM oDoAfter AS CHAR NO-UNDO.

   oDoAfter = "-".
   RUN acct-cls.p (recid(xacct)) NO-ERROR.

   FOR EACH tt-log:
      RUN AddMess(tt-log.str).
      DELETE tt-log.
   END.
   FIND FIRST acct
      WHERE recid(acct) EQ recid(xacct)
      NO-LOCK NO-ERROR.
   IF     AVAIL acct
      AND acct.close-date = ?
   THEN DO:
      ASSIGN
         oDoAfter = "DoAfter1"
         mError   = "Лицевой счет " + mAcct + " не закрыт.".
      RETURN.
   END.
   count-cls = count-cls + 1.
   RUN AddMess("#Л/СЧЕТ    " + "~t" + mAcct + " Закрыт." + oDoAfter).
END PROCEDURE.

/* процедура открытие счета */
PROCEDURE AcctOpen:
   DEF OUTPUT PARAM oDoAfter AS CHAR NO-UNDO.
   DEF BUFFER acct  FOR acct.

   oDoAfter = "-".
   IF NOT AVAIL xacct THEN
      CREATE acct.

   ASSIGN
      acct.acct        = AddFilToAcct(mAcct, mBranch)
      acct.currency    = mCurr
      acct.rate-type   = IF mCurr EQ ""
                            THEN ""
                            ELSE "Учетный"
      acct.acct-cat    = IF mAcctCat EQ ""
                            THEN bal-acct.acct-cat
                            ELSE mAcctCat
      acct.Class-Code  = IF op-template.cr-class-code EQ ""
                            THEN "acct" + acct.acct-cat
                            ELSE op-template.cr-class-code
      acct.branch-id   = mBranch
      acct.filial-id   = mBranch
      acct.bal-acct    = mBAcct
      acct.side        = IF mSide EQ ""
                            THEN bal-acct.side
                            ELSE mSide
      acct.cust-id     = INT64(mSurr)
      acct.cust-cat    = ENTRY(mCount + 1, "В,Ч,Ю,Б")
      acct.details     = TRIM(mName[1] + " " + mName[2] +
                              mName[3] + " " + mName[4])
      acct.acct-status = mStatus
      acct.last-date   = today
      acct.contr-acct  = mContrAcct
      acct.open-date   = GetOpenDate()
      acct.close-date  = GetCloseDate()
      acct.user-id     = GetUserImport(mail-user.mail-user-num,
                                       op-kind.op-kind,
                                       mAcct,
                                       mUserId,
                                       "")
      mContract        = GetRefVal("W4contract", TODAY, STRING(mBAcct) + "," + (IF (mCurr = "") THEN "RUB" ELSE "VAL"))
      acct.contract    = IF (mContract NE ?) THEN mContract ELSE bal-acct.contract
/*    acct.contract    = GetXattrValueEx("op-template",
                                         op-temp.op-kind + "," +
                                            string(iOpTempl),
                                         "contract",
                                         bal-acct.contract) */
      NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      oDoAfter = "DoAfter1".
      mError   = ERROR-STATUS:GET-MESSAGE(1).
      RETURN.
   END.
   RELEASE acct NO-ERROR.

   FIND FIRST acct
      WHERE (acct.acct        = AddFilToAcct(mAcct, mBranch))
        AND (acct.currency    = mCurr)
      NO-LOCK NO-ERROR.
   RUN AddMess(STRING(TIME, "HH:MM:SS") + " #Л/СЧЕТ    ~t" + mAcct + (IF (AVAIL acct) THEN "" ELSE " не") + " создан.").

   IF (AVAIL acct)
   THEN DO:
      FOR EACH ttsigns WHERE ttsigns.class BEGINS "acct"
                         AND ttsigns.code  NE "":
         IF NOT  UpdateSigns(ttsigns.class,
                             acct.acct + "," + acct.currency,
                             ttsigns.code,
                             ttsigns.val,
                             ?)
         THEN DO:
            mError   = "Ошибка создания дополнительного реквизита " + ttsigns.code.
            RUN AddMess(mError).
         END.
         ELSE DO:
            IF (ttsigns.code = "groupOABS")
            THEN RUN CreateLinks("acct", "acct-group", acct.acct + "," + acct.currency, ttsigns.val, acct.open-date, ?, "") NO-ERROR.
         END.
      END.

      IF acct.cust-cat EQ "Ч" THEN
      DO:
         CREATE deputy.
         ASSIGN
            deputy.acct       = acct.acct
            deputy.currency   = acct.currency
            deputy.person-id  = acct.cust-id
            deputy.right-priv = NO.
      END.
      count-acct = count-acct + 1.
   END.
END PROCEDURE.

/* протокол закрытия счета*/
PROCEDURE LogMessage:
   DEF INPUT  PARAM iMess AS CHAR NO-UNDO. /* сообщение об ошибке */
   DEF INPUT  PARAM iEvnt AS CHAR NO-UNDO. /* событие вызвавшее ошибку */
   DEF INPUT  PARAM iCont AS CHAR NO-UNDO. /* возможность продолжения обработки
                                          (критичность ошибки */
   DEF OUTPUT PARAM opOk  AS INT64  NO-UNDO. /* флаг ошибки в данной процедуре
                                              не меняется */
   CREATE tt-log.
   tt-log.str = imess.
END PROCEDURE.
/******************************************************************************/
