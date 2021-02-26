/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2002 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ved-acct.p
      Comment: �������� ������ ��⮢ (�������, �� ࠡ����� ����� ����,
               ��⮢, �� ���. �����⢫. ⮫쪮 ������. ����樨
   Parameters: iParam - �᫨ "������", � ��������� ������ ��⮢
                      - �᫨ "��������", � ���, �� ࠡ���騥 ����� ����
                      - �᫨ "���⠁�����", � ������ ��� ⮫쪮 � ��������.
                        �����ﬨ
         Uses:
      Used by:
      Created: 17/04/1003 kolal (�� pos-nal2.p)
     Modified: 17/04/2003 kolal ��� 13134.
     Modified: 16/07/2003 kolal ��ࠢ���� ���⠭���� ���� "⥫�䮭". ��� 13134.
     Modified: 24/05/2004 ABKO 28659 - �������� ���஢�� ��� "��������","���⠁�����"
     Modified: 12/01/2006 kraw (0056474) "��������" ⥯��� �������� �������� �ࠢ��쭮
     Modified: 27.04.2006 TSL  ���४�� ����� � ����� 䨫��� ��⮢
*/
&SCOP DEFAULT_MASK "401*,402*,403*,404*,405*,406*,407*"
DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO. /* ��ப� �室��� ��ࠬ��஢ */

{globals.i}             /* �������� ��६���� ��ᨨ. */
{flt-val.i}    
{tmprecid.def}          /* ������ �⬥⮪. */

DEFINE VARIABLE mCustName AS CHARACTER /* ������������ ������ */
   FORMAT "x(40)"
   EXTENT 10
   NO-UNDO.
DEFINE VARIABLE mIndex        AS INT64 NO-UNDO. /* ���稪 ��� �뢮�� �������
                                                 ����� ������ */
DEFINE VARIABLE mBegDate      AS DATE    NO-UNDO. /* ��砫쭠� ��� ��� ���⪮� */
DEFINE VARIABLE mLastMove     AS DATE    NO-UNDO. /* ��� ��᫥����� �������� */
DEFINE VARIABLE mKassAcct     AS CHARACTER INIT "202*,40906*" NO-UNDO.
                                              /* ��᪠ ��� ��� �஢�ન
                                                 ������� ����権 */
DEFINE VARIABLE mBalAcctMask  AS CHARACTER NO-UNDO. /**/
DEFINE VARIABLE mTelephone    AS CHARACTER NO-UNDO. /* ⥫�䮭 ������ */
DEFINE VARIABLE vINN          AS CHARACTER NO-UNDO. /* ��� */
DEFINE VARIABLE mStrLastMove  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCurrClId         AS INT64 NO-UNDO. /* ����饥 ���祭�� ��� fClId */
DEFINE VARIABLE mCurrBalAcct      AS INT64 NO-UNDO. /* ����饥 ���祭�� ��� fbal-acct */
DEFINE VARIABLE mCountBalAcct AS INT64 NO-UNDO. /* ���稪 ��⮢ � ��㯯� */
DEFINE VARIABLE mCountAll     AS INT64 NO-UNDO. /* ���稪 ��⮢ */

DEFINE TEMP-TABLE tmprwd
   FIELD fBal-acct AS INT64 /* ��� 1-�� ���浪� */
   FIELD fClId     AS INT64 
   FIELD fRwd      AS ROWID   /* ��� �롮� �� 䨫���� */
   FIELD fName     AS CHAR    /* ��� ���஢�� */
  INDEX idxB fBal-acct.

{getdate.i}
{wordwrap.def}
{sh-defs.i}

/* �������㥬 ���� */
IF iParam = "���⠁�����" THEN
DO:
   PAUSE 0.
   DO
      ON ERROR UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE
      WITH FRAME KassAcctFrame:

      UPDATE
         mKassAcct FORMAT "x(100)"
            LABEL "��� ������� ����権"
            HELP  "������ ���� ��� ��⮢ ������� ����権"
            VIEW-AS FILL-IN SIZE 20 BY 1
      WITH CENTERED ROW 10 OVERLAY SIDE-LABELS
         COLOR messages TITLE "[ ������� ����� ������ ]"
      EDITING:
         READKEY.
         APPLY LASTKEY.
      END.
   END.

   HIDE FRAME KassAcctFrame NO-PAUSE.

   IF KEYFUNC(LASTKEY) = "end-error" THEN
      RETURN.
END.

/* �롮ઠ � ᯨ᪥ ��⮢ ����� ���� �����
   �� � ⮩ ���஢���, ��� �� ��.
   ���⮬� ��४���� �� � ��                   */
mBalAcctMask = IF GetFltVal("bal-acct") NE "*"
               THEN GetFltVal("bal-acct")
               ELSE {&DEFAULT_MASK}.

                        /* ��ନ�㥬 ��� ��� ����. */
mIndex = 0.
FOR EACH TmpRecId,
FIRST acct WHERE
         RECID (acct) EQ TmpRecId.id
   AND   CAN-DO (mBalAcctMask, STRING(acct.bal-acct))
NO-LOCK:
   mIndex = mIndex + 1.
   CREATE tmprwd.
   ASSIGN
      tmprwd.fRwd       = ROWID (acct)
      tmprwd.fClId      = acct.cust-id
      tmprwd.fBal-acct  = INT64 (SUBSTRING (STRING (acct.bal-acct), 1, 3))
   .
END.

mIndex = 0.

DEFINE NEW GLOBAL SHARED VARIABLE usr-printer LIKE PRINTER.PRINTER NO-UNDO.
/* �� setdest.i ��� ��������� ����୮�� ��।������ */
IF iParam = "������" THEN
DO:
   {setdest.i &nodef="/*" &cols=120}
END.
ELSE
DO:
   {setdest.i &nodef="/*" &cols=82}
END.

PUT UNFORMATTED name-bank SKIP(1).
CASE iParam:
   WHEN "������" THEN
      PUT UNFORMATTED
         "��������� ������ ��⮢ �� " + STRING(end-date,"99/99/9999") SKIP.
   WHEN "��������" THEN
      PUT UNFORMATTED
         "��������� ��⮢, �� ࠡ����� ����� ���� �� " + STRING(end-date,"99/99/9999") SKIP.
   WHEN "������" THEN
      PUT UNFORMATTED
         "��������� ��⮢, ࠡ����� � �祭�� ���� �� " + STRING(end-date,"99/99/9999") SKIP.         
   WHEN "���⠁�����" THEN
      PUT UNFORMATTED
         "��������� ��⮢ �।���⨩, �� ��⠬ ������ �����⢫����� ����樨" SKIP
         "� �������筮� ���浪� � �� �����⢫����� ����樨 � �����묨 ���죠��" SKIP
         "�� " + STRING(end-date,"99/99/9999") SKIP.
END CASE.

IF iParam = "������" THEN
   PUT UNFORMATTED "����������������������������������������������������������������������������������������������������������������������͸" SKIP
                   "�     ������ ���      �        ������������ ������            � ����� �    ����䮭    �        �        �        �" SKIP
                   "����������������������������������������������������������������������������������������������������������������������͵" SKIP.
ELSE
   PUT UNFORMATTED "�������������������������������������������������������������������������������͸" SKIP
                   "�     ������ ���      �        ������������ ������            �    ���     �" SKIP
                   "�������������������������������������������������������������������������������͵" SKIP.

IF iParam = "������" THEN
DO:
FOR EACH tmprwd,
   FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                AND (acct.close-date GT end-date
                OR   acct.close-date EQ ?)
      NO-LOCK
   BREAK BY tmprwd.fClId:

   /* ���. �᫮��� �⡮� */

   IF mCurrClId <> tmprwd.fClId THEN
   DO:
      IF mCountBalAcct NE 0 THEN
      DO:
         PUT UNFORMATTED
"����������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�" +
                         FILL(" ", 25) +
                         "�    �⮣� �� ��⠬ " +
                         STRING(mCurrBalAcct, "999") +
                         ": " +
                         STRING(mCountBalAcct, ">>>>9") +
                         " ��⮢   " +
                         "�" +
                         FILL(" ", 8) +
                         "�" +
                         FILL(" ", 15) +
                         "�        �        �        �"
                         SKIP.
         PUT UNFORMATTED
"����������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.
      END.
      ASSIGN
         mCurrclid = tmprwd.fClId
         mCurrBalAcct = tmprwd.fBal-acct
         mCountBalAcct = 0
      .
   END.

   ASSIGN
      mCountBalAcct = mCountBalAcct + 1
      mCountAll = mCountAll + 1
   .

   {getcust.i &name="mCustName" &OFFinn = "/*"}

   mCustName[1] = mCustName[1] + " " + mCustName[2].
   {wordwrap.i &s=mCustName &n=10 &l=40}

   IF acct.cust-cat = "�" THEN
   DO:
      mTelephone = GetXAttrValue("banks", STRING(acct.cust-id), "fax").
      IF mTelephone = "" THEN
         mTelephone = GetXAttrValue("banks", STRING(acct.cust-id), "tel").
   END.
   ELSE
      IF acct.cust-cat = "�" THEN
      DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id = acct.cust-id
            NO-LOCK
            NO-ERROR.
         IF AVAIL cust-corp THEN
            mTelephone = cust-corp.fax.
         IF    mTelephone = ?
            OR mTelephone = "" THEN
            mTelephone = GetXAttrValue("cust-corp", STRING(acct.cust-id), "tel").
      END.
      ELSE
      DO:
         FIND person WHERE person.person-id = acct.cust-id
            NO-LOCK
            NO-ERROR.
         IF AVAIL person THEN
         DO:
            mTelephone = person.phone[1].
            IF    mTelephone = ?
               OR mTelephone = "" THEN
               mTelephone = person.phone[2].
            IF    mTelephone = ?
               OR mTelephone = "" THEN
               mTelephone = GetXattrValue("person",STRING(person.person-id),"cell-phone").
            IF    mTelephone = ?
               OR mTelephone = "" THEN
               mTelephone = person.fax.
         END.
         IF    mTelephone = ?
            OR mTelephone = "" THEN
            mTelephone = "".
      END.

   PUT UNFORMATTED "�" +
                   STRING(acct.acct, "x(25)") +
                   "�" +
                   STRING(mCustName[1], "x(40)") +
                   "�" +
                   STRING(acct.open-date, "99/99/99") +
                   "�" +
                   STRING(mTelephone, "x(15)") +
                   "�        �        �        �"
                   SKIP.

   mIndex = 2.
   DO WHILE mCustName[mIndex] NE ""
      AND mIndex LE 10 :
      PUT UNFORMATTED "�" +
                      FILL(" ", 25) +
                      "�" +
                      STRING(mCustName[mIndex], "x(40)") +
                      "�" +
                      FILL(" ", 8) +
                      "�" +
                      FILL(" ", 15) +
                      "�        �        �        �"
                      SKIP.
      mIndex = mIndex + 1.
   END.

END.
END.
ELSE DO:
   FOR EACH tmprwd,
      FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                   AND (acct.close-date GT end-date
                   OR   acct.close-date EQ ?):
      {getcust.i &name="mCustName" &OFFinn = "/*"}
      tmprwd.fName = mCustName[1] + " " + mCustName[2].
   END.

   FOR EACH tmprwd,
      FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                   AND (acct.close-date GT end-date
                   OR   acct.close-date EQ ?)
         NO-LOCK
      BREAK BY tmprwd.fClId
      BY tmprwd.fBal-acct:
          
      IF FIRST-OF(tmprwd.fClId) THEN
      DO:
         IF mCountBalAcct GT 1 THEN
         DO:
            PUT UNFORMATTED
"�������������������������������������������������������������������������������Ĵ" SKIP.
            PUT UNFORMATTED "�" +
                            FILL(" ", 25) +
                            "�    �⮣� ��⮢ : " +
                            STRING(mCountBalAcct, ">>>>9") +
                            FILL(" ", 16) +
                            "�" SKIP.
            PUT UNFORMATTED
"�������������������������������������������������������������������������������Ĵ" SKIP.
         END.
         ELSE IF mCountBalAcct EQ 1 THEN
            PUT UNFORMATTED
"�������������������������������������������������������������������������������Ĵ" SKIP.
         ASSIGN
            mCountBalAcct = 0
            mCustName[1] = tmprwd.fName
         .
         {wordwrap.i &s=mCustName &n=10 &l=40}
      END.

      /* ���. �᫮��� �⡮� */
      IF iParam = "��������" THEN DO:
         mBegDate = DATE(MONTH(end-date),
                         DAY(end-date),
                        YEAR(end-date) - 1) NO-ERROR.
         /* � ���� ����� 29.02.2004 ? */
         IF ERROR-STATUS:ERROR THEN
            mBegDate = DATE(MONTH(end-date),
                            DAY(end-date) - 1,
                            YEAR(end-date) - 1).

         RUN acct-pos IN h_base (acct.acct,
                                 acct.curr,
                                 mBegDate,
                                 end-date,
                                 ?).
         mLastMove = IF acct.curr = "" THEN
                        LastMove
                     ELSE
                        LastCurr.
                        
         /* �ய�᪠�� �, � ������ �뫮 �������� �� ��᫥���� ��� ��� ������ ����� ���� */
         IF mLastMove NE ? AND mLastMove GE mBegDate OR acct.open-date GE mBegDate THEN
            NEXT.
      END.
      /* ���. �᫮��� �⡮� */
      IF iParam = "������" THEN DO:
         mBegDate = DATE(MONTH(end-date),
                         DAY(end-date),
                        YEAR(end-date) - 1) NO-ERROR.
         /* � ���� ����� 29.02.2004 ? */
         IF ERROR-STATUS:ERROR THEN
            mBegDate = DATE(MONTH(end-date),
                            DAY(end-date) - 1,
                            YEAR(end-date) - 1).

         RUN acct-pos IN h_base (acct.acct,
                                 acct.curr,
                                 mBegDate,
                                 end-date,
                                 ?).
         mLastMove = IF acct.curr = "" THEN
                        LastMove
                     ELSE
                        LastCurr.
         /* �ய�᪠�� �, � ������ �� �뫮 �������� �� ��᫥���� ���*/
                        
         mStrLastMove = IF mLastMove NE ? THEN STRING(mLastMove,"99/99/9999") ELSE "".         
         IF mLastMove LE mBegDate OR mLastMove EQ ? THEN
            NEXT.
      END.
      IF iParam = "���⠁�����" THEN
      DO:
         FIND FIRST op-entry WHERE (op-entry.acct-db = acct.acct
                                AND CAN-DO(mKassAcct, op-entry.acct-cr))
                                AND op-entry.op-date NE ?
            NO-LOCK NO-ERROR.
         IF AVAIL op-entry THEN
            NEXT.

         FIND FIRST op-entry WHERE (op-entry.acct-cr = acct.acct
                                AND CAN-DO(mKassAcct, op-entry.acct-db))
                                AND op-entry.op-date NE ?
            NO-LOCK NO-ERROR.
         IF AVAIL op-entry THEN
            NEXT.
      END.



      ASSIGN
         mCountBalAcct = mCountBalAcct + 1
         mCountAll = mCountAll + 1
      .


      PUT UNFORMATTED "�" +
                      STRING(acct.acct, "x(25)") +
                      "�" +
                      STRING(mCustName[1], "x(40)") +
                      "� " +
                      mStrLastMove +  
                      " �" SKIP.

      mIndex = 2.
      DO WHILE mCustName[mIndex] NE ""
         AND mIndex LE 10:
         PUT UNFORMATTED "�" +
                         FILL(" ", 25) +
                         "�" +
                         STRING(mCustName[mIndex], "x(40)") +
                         "�            �" SKIP.
         mIndex = mIndex + 1.
      END.
   END.
END.

IF mCountBalAcct GT (IF iParam EQ "������" THEN 0 ELSE 1) THEN
DO:
   PUT UNFORMATTED (IF iParam EQ "������" THEN
"����������������������������������������������������������������������������������������������������������������������Ĵ"
                    ELSE
"�������������������������������������������������������������������������������Ĵ") SKIP.
   PUT UNFORMATTED (IF iParam = "������" THEN
                       "�" +
                       FILL(" ", 25) +
                       "�    �⮣� �� ��⠬ " +
                       STRING(mCurrBalAcct, "999") +
                       ": " +
                       STRING(mCountBalAcct, ">>>>9") +
                       " ��⮢   " +
                       "�" +
                       FILL(" ", 8) +
                       "�" +
                       FILL(" ", 15) +
                       "�        �        �        �"
                    ELSE "�" +
                         FILL(" ", 25) +
                         "�    �⮣� ��⮢ : " +
                         STRING(mCountBalAcct, ">>>>9") +
                         FILL(" ", 16) +
                         "�"
                   ) SKIP.
   PUT UNFORMATTED (IF iParam = "������" THEN
"����������������������������������������������������������������������������������������������������������������������Ĵ"
                       ELSE
"�������������������������������������������������������������������������������Ĵ") SKIP.
END.
ELSE IF    mCountBalAcct EQ 1
       AND iParam NE "������" THEN
            PUT UNFORMATTED
"�������������������������������������������������������������������������������Ĵ" SKIP.

IF mCountAll NE 0 THEN
DO:
   PUT UNFORMATTED (IF iParam = "������" THEN
                       "�" +
                       FILL(" ", 25) +
                       "�    �⮣� " +
                       STRING(mCountAll, ">>>>9") +
                       " ��⮢" +
                       FILL(" ", 18) +
                       "�" +
                       FILL(" ", 8) +
                       "�" +
                       FILL(" ", 15) +
                       "�        �        �        �"
                    ELSE
                       "�" +
                       FILL(" ", 25) +
                       "�    �⮣� " +
                       STRING(mCountAll, ">>>>9") +
                       " ��⮢" +
                       FILL(" ", 18) +
                       "�            �") SKIP.
   PUT UNFORMATTED (IF iParam = "������" THEN
"������������������������������������������������������������������������������������������������������������������������"
                    ELSE
"���������������������������������������������������������������������������������") SKIP.
END.

{signatur.i}
{preview.i}


OUTPUT TO VALUE("ved-acct2.csv") CONVERT TARGET "1251".

FOR EACH tmprwd,
   FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                AND (acct.close-date GT end-date
                OR   acct.close-date EQ ?):
   {getcust.i &name="mCustName" &OFFinn = "/*"}
   tmprwd.fName = mCustName[1] + " " + mCustName[2].
END.

FOR EACH tmprwd,
   FIRST acct WHERE ROWID(acct) = tmprwd.fRwd
                AND (acct.close-date GT end-date
                OR   acct.close-date EQ ?)
      NO-LOCK:

   mBegDate = DATE(MONTH(end-date),
                   DAY(end-date),
                   YEAR(end-date) - 1) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      mBegDate = DATE(MONTH(end-date),
                      DAY(end-date) - 1,
                      YEAR(end-date) - 1).

   RUN acct-pos IN h_base (acct.acct,
                           acct.curr,
                           mBegDate,
                           end-date,
                           ?).
   mLastMove = IF acct.curr = "" THEN
                  LastMove
               ELSE
                  LastCurr.
   /* �ய�᪠�� �, � ������ �� �뫮 �������� �� ��᫥���� ���*/
   mStrLastMove = IF mLastMove NE ? THEN STRING(mLastMove,"99/99/9999") ELSE "".         
   IF mLastMove LE mBegDate OR mLastMove EQ ? THEN
      NEXT.

   PUT UNFORMATTED
      "'" DelFilFromAcct(acct.acct) ";"
      tmprwd.fName ";"
      acct.open-date ";"
      mStrLastMove
   SKIP.
END.

OUTPUT CLOSE.

MESSAGE 
"���ଠ�� ���㦥�� � 䠩� ved-acct2.csv." 
VIEW-AS ALERT-BOX.

RETURN.
