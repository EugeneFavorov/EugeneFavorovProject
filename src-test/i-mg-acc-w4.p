/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: i-mg-a��.p
      Comment: ������ ��⮢
         Uses:
      Used BY:
      Created: 28.06.2000 Mike
     Modified:
*/
{i-mg-tag.def}
{i-mg-tag.pro}
{intrface.get refer}
{pb_logit.i}

DEF VAR mAcct      AS CHAR NO-UNDO. /*��楢�� ���         */
DEF VAR mAcctF     AS CHAR NO-UNDO. /*��楢�� ���         */
DEF VAR mAcctCat   AS CHAR NO-UNDO. /*��⥣��� ���. ��� */
DEF VAR mBAcct     AS INT64  NO-UNDO. /*��� ��ண� ���浪� */
DEF VAR mBankINN   AS CHAR NO-UNDO. /*�.�. ���             */
DEF VAR mCloseDate AS DATE NO-UNDO. /*��� �������        */
DEF VAR mContrAcct AS CHAR NO-UNDO. /*���� ���          */
DEF VAR mCount     AS INT64  NO-UNDO. /*���稪              */
DEF VAR mCurr      AS CHAR NO-UNDO. /*����� - ���       */
DEF VAR mContract  AS CHAR NO-UNDO. /*�����祭�� ���     */
DEF VAR mDate      AS DATE NO-UNDO. /*��� ������        */
DEF VAR mDiasoftId AS CHAR NO-UNDO. /*��� ������ - ������*/
DEF VAR mDoAfter   AS CHAR NO-UNDO. /*����⢨� ��᫥ ����᪠ ��楤���*/
DEF VAR mError     AS CHAR NO-UNDO. /*�訡�� ᮧ�����      */
DEF VAR iError     AS INT64  NO-UNDO INIT 0. /*���稪 �訡�� */
DEF VAR mKey       AS INT64  NO-UNDO. /*����                 */
DEF VAR mName      AS CHAR NO-UNDO EXTENT 4. /* ������������ */
DEF VAR mSide      AS CHAR NO-UNDO. /*��⨢                */
DEF VAR mSignsCode AS CHAR NO-UNDO. /*��� �.�.             */
DEF VAR mStatus    AS CHAR NO-UNDO. /*�����               */
DEF VAR mSurr      AS CHAR NO-UNDO. /*��� ������ �� �.�.  */
DEF VAR mSymRep    AS CHAR NO-UNDO. /*�.�. ������ ����   */
DEF VAR mUserId    AS CHAR NO-UNDO. /*��� ���짮��⥫�     */
DEF VAR mLog       AS CHAR NO-UNDO. /*��⮪�� �訡��      */
mLog = "/home2/bis/quit41d/log/way4/acct-"
     + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")
     + "-" + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".log".
DEF BUFFER xacct FOR acct.

/*��� ��⮪��� ������� ���********/
DEF VAR gLogProc    AS handle NO-UNDO.
DEF VAR gLogMessage AS log    NO-UNDO.

RUN setsysconf IN h_base("gLogProc",STRING(THIS-PROCEDURE:HANDLE)) NO-ERROR.
RUN setsysconf IN h_base("gLogMessage","YES") NO-ERROR.

{getlogp.i}
DEF TEMP-TABLE tt-log NO-UNDO
   FIELD str AS CHAR.
/* *********************************** */

/* ��� ������� ��� */
FUNCTION GetCloseDate returns DATE:
   RETURN mCloseDate.
END FUNCTION.

/*��� ������ ���*/
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
      WHEN "������"     THEN mBranch    = tt.val.
      WHEN "������"     THEN mCurr      = GetCurrency(tt.val).
      WHEN "���/����"   THEN mBAcct     = INT64(tt.val).
      WHEN "����"       THEN mAcct      = tt.val.
      WHEN "���1"       THEN mName[1]   = tt.val.
      WHEN "���2"       THEN mName[2]   = tt.val.
      WHEN "���3"       THEN mName[3]   = tt.val.
      WHEN "���4"       THEN mName[4]   = tt.val.
      WHEN "������"     THEN mDate      = DATE(tt.val).
      WHEN "�������"    THEN mAcctCat   = GetEntryCat(tt.val).
      WHEN "���"        THEN mSide      = IF tt.val BEGINS "�" THEN "�"
                                          ELSE IF tt.val BEGINS "�" THEN "�"
                                          ELSE tt.val.
      WHEN "������1"    THEN mDiasoftId = tt.val.
      WHEN "���������"  THEN mUserId    = tt.val.
      WHEN "����������" THEN mStatus    = tt.val.
      WHEN "������"     THEN mCloseDate = DATE(tt.val).
      WHEN "���������"  THEN mContrAcct = tt.val.
      WHEN "��������"   THEN mBankINN   = tt.val.
      WHEN "����.���."  THEN mSymRep    = tt.val.
      WHEN "#END"     OR
      WHEN "%END"     THEN
      DO:
         mFirst = YES.
         RUN CreateAcct.

         /* ��⮪�� ��� ��ࠢ�� �� ���� */
         IF (mError NE "")
         THEN DO:
            IF (iError EQ 0)
            THEN RUN LogIt(CODEPAGE-CONVERT("�訡�� ������ ��⮢ WAY4 :", "1251"), mLog).
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
               RUN LogIt(CODEPAGE-CONVERT("�� ᮧ��� ��� " + mAcct, "1251"), mLog).
/*             RUN pb_mail.p ("a.borisov,v.ignatchenko,t.stenina", "WAY4 - acct error", "", mLog)). */
               RUN pb_mail.p ("a.borisov", "WAY4 - acct error", "", mLog)).
            END.
         END.
      END.
      OTHERWISE IF (NOT (tt.tag BEGINS "#"  OR
                         tt.tag BEGINS "%"))
                   AND {assigned tt.tag}
                   AND {assigned tt.val}  THEN                 /* ����������� */
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
/* �������� ���                                                             */
/*----------------------------------------------------------------------------*/
PROCEDURE CreateAcct:
   DEF VAR cWho       AS CHAR NO-UNDO. /* ����饭�� � �����஢�� */
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

        /*�஢�ઠ �.�.*/
        {i-mg-acc-w4.upd}

        IF (mError = "")
        THEN DO:
            mDoAfter = "000".
            IF      AVAIL xacct
                AND GetCloseDate() NE ?
            THEN DO:            /*�����⨥ ���*/
                RUN AcctClose(OUTPUT mDoAfter).
                IF mDoAfter EQ "DoAfter1"
                THEN /* UNDO CRT-ACCT, RETRY CRT-ACCT. */
                DO:
                    ASSIGN
                        count-err   = count-err + 1
                        mError      = IF (mError EQ "") THEN
                                      "1 ���������� ������� ��楢�� ��� " + mAcct
                                    + "~n ��稭� �訡��: " + ERROR-STATUS:get-message(1) + "|" + STRING(ERROR-STATUS:ERROR)
                                      ELSE mError
                        .
                    RUN AddMess(mError).
                    LEAVE CRT-ACCT.
                END.
            END.
            ELSE DO:            /* ᮧ����� ��� */
                /* �஢��塞 �����஢�� ������ */
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
                        RUN AddMess("������ �������� " + ENTRY(1, SUBSTRING(cWho, INDEX(cWho, "���짮��⥫�:") + 14), "~n")).
                        PAUSE 10.
                    END.
                END.
                RUN AddMess("������ ࠧ�����஢��").

                RUN AcctOpen(OUTPUT mDoAfter) NO-ERROR.
                IF mDoAfter EQ "DoAfter1"
                THEN /* UNDO CRT-ACCT, RETRY CRT-ACCT. */
                DO:
                    ASSIGN
                        count-err   = count-err + 1
                        mError      = IF (mError EQ "") THEN
                                      "2 ���������� ᮧ���� ��楢�� ��� " + mAcct
                                    + "~n ��稭� �訡��: " + ERROR-STATUS:get-message(1) + "|" + STRING(ERROR-STATUS:ERROR)
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

/* ��楤�� ������� ���*/
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
         mError   = "��楢�� ��� " + mAcct + " �� ������.".
      RETURN.
   END.
   count-cls = count-cls + 1.
   RUN AddMess("#�/����    " + "~t" + mAcct + " ������." + oDoAfter).
END PROCEDURE.

/* ��楤�� ����⨥ ��� */
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
                            ELSE "����"
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
      acct.cust-cat    = ENTRY(mCount + 1, "�,�,�,�")
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
   RUN AddMess(STRING(TIME, "HH:MM:SS") + " #�/����    ~t" + mAcct + (IF (AVAIL acct) THEN "" ELSE " ��") + " ᮧ���.").

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
            mError   = "�訡�� ᮧ����� �������⥫쭮�� ४����� " + ttsigns.code.
            RUN AddMess(mError).
         END.
         ELSE DO:
            IF (ttsigns.code = "groupOABS")
            THEN RUN CreateLinks("acct", "acct-group", acct.acct + "," + acct.currency, ttsigns.val, acct.open-date, ?, "") NO-ERROR.
         END.
      END.

      IF acct.cust-cat EQ "�" THEN
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

/* ��⮪�� ������� ���*/
PROCEDURE LogMessage:
   DEF INPUT  PARAM iMess AS CHAR NO-UNDO. /* ᮮ�饭�� �� �訡�� */
   DEF INPUT  PARAM iEvnt AS CHAR NO-UNDO. /* ᮡ�⨥ �맢��襥 �訡�� */
   DEF INPUT  PARAM iCont AS CHAR NO-UNDO. /* ����������� �த������� ��ࠡ�⪨
                                          (���筮��� �訡�� */
   DEF OUTPUT PARAM opOk  AS INT64  NO-UNDO. /* 䫠� �訡�� � ������ ��楤��
                                              �� ������� */
   CREATE tt-log.
   tt-log.str = imess.
END PROCEDURE.
/******************************************************************************/
