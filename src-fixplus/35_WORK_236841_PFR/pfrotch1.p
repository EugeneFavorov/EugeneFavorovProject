/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pfrotch1.p
      Comment: 0236841 ������. ����㧪� ॥��� ���᫥��� ���ᨩ �� �� + ���� �� ���ᨮ��ࠬ
   Parameters: ��� ���
         Uses:
      Used by:
      Created: 05/11/2015 KMBIS 0236841 ������. ����㧪� ॥��� ���᫥��� ���ᨩ �� ��
                                        ���� �� ���ᨮ��ࠬ
     Modified: 
                                    
*/
DEF INPUT PARAM iParams   AS CHAR  NO-UNDO.
{globals.i}

{intrface.get date}
{intrface.get prnvd}
{intrface.get strng}
{intrface.get xclass}

DEF TEMP-TABLE ttPensAcct NO-UNDO
   FIELD acct-pfr AS CHAR  /* ����� ���                   */
   FIELD proxy    AS LOG   /* �뫨 ᯨᠭ�� �� ����७���� */
   FIELD print    AS LOG   /* ������� ��� � ����         */
   FIELD pers-id  AS INT64 /* person.person-id              */
   FIELD op-id    AS INT64 /* op.op ���㬥�� �� ���        */
.

DEF VAR mPfrCode  AS CHAR  NO-UNDO. /* ��� �����䨪��� ��� */
DEF VAR mPFRAcct  AS CHAR  NO-UNDO. /* ��� ���              */
DEF VAR mI        AS INT64 NO-UNDO.
DEF VAR mAcct     AS CHAR  NO-UNDO.
DEF VAR mFIO      AS CHAR  NO-UNDO.
DEF VAR mBirth    AS CHAR  NO-UNDO.
DEF VAR mProxy    AS CHAR  NO-UNDO.
DEF VAR mPrBeg    AS CHAR  NO-UNDO.
DEF VAR mPrEnd    AS CHAR  NO-UNDO.
DEF VAR mPrFIO    AS CHAR  NO-UNDO.
DEF VAR mOpDate   AS CHAR  NO-UNDO.
DEF VAR mPFR      AS CHAR  NO-UNDO.
DEF VAR mDateNxt  AS DATE  NO-UNDO.
DEF VAR mPermFnd  AS LOG   NO-UNDO.
DEF VAR mProxyRID AS ROWID NO-UNDO.


DEF BUFFER bAcct     FOR acct.
DEF BUFFER bPensAcct FOR ttPensAcct.

{pfrotch1.fun}
{pfrotch1.pro}

ASSIGN
   mPFRAcct = ENTRY(1, iParams, ";") 
   mPfrCode = ENTRY(2, iParams, ";")  WHEN NUM-ENTRIES(iParams, ";") GE 2
.

{getdates.i &AddPostUpd = "IF beg-date EQ ? OR end-date EQ ? THEN 
                           DO:
                              MESSAGE '������ ��ਮ�!' VIEW-AS ALERT-BOX.
                              UNDO, RETRY.
                           END."}

/*=== ��ॡ�� ��⮢ � ������ �ந�������� ᯨᠭ�� ���ᨩ ===*/
DO mI = 1 TO NUM-ENTRIES(mPFRAcct):

   mAcct = ENTRY(mI, mPFRAcct).
   {find-act.i &acct = mAcct}

   /* ��ந� ⠡���� ��⮢ �� ����� �뫨 ���᫥��� � ��� �� */
   IF AVAIL(acct) THEN
      RUN InitPensTT(acct.acct, beg-date, end-date).

END. /* DO mI = 1 TO NUM-ENTRIES(mPFRAcct): */


FOR EACH ttPensAcct NO-LOCK:
   /*=== ��䨫���㥬 ��� �������騥 �뢮�� � ���� ===*/
   RUN FiltPensTT(ROWID(ttPensAcct), beg-date, end-date).

END. /* FOR EACH ttPensAcct NO-LOCK: */

/*=== ��ନ�㥬 ���� ===*/
RUN BeginCircle_TTName IN h_prnvd("line").
FOR EACH ttPensAcct WHERE ttPensAcct.print EQ YES
                    NO-LOCK,
   FIRST person WHERE person.person-id EQ ttPensAcct.pers-id
                NO-LOCK
                BREAK BY person.name-last
                      BY person.first-names
                      BY person.person-id
                      BY ttPensAcct.proxy:  /* ���묨 ���� ��� ��� ᯨᠭ�� �� ����७���� */

   IF FIRST-OF(person.person-id) THEN
   DO:
      ASSIGN
         /*=== ��� ===*/ 
         mFIO   = SUBST("&1 &2", TRIM(person.name-last), TRIM(person.first-names))
         mFIO   = RemoveDoubleChars(mFIO, " ")
         /*=== ��� ஦����� ===*/ 
         mBirth = STRING(person.birthday, "99.99.9999")
      .
   END. /*IF FIRST-OF(person.person-id) THEN */

   ASSIGN
      /*=== ��� ��᫥����� ���饭�� ��� ᮢ��襭�� ����権 �� ��⠬ ===*/ 
      mOpDate= LastMove(ttPensAcct.acct-pfr)
      /*=== �⤥����� �� ===*/ 
      mPFR   = GetPfrName(ttPensAcct.op-id, mPfrCode)
   .

   IF FIRST-OF(ttPensAcct.proxy) THEN
      mProxy = STRING(ttPensAcct.proxy, "��/���"). /* ����稥 ����७���� */ 

   IF ttPensAcct.proxy EQ YES THEN
   DO:
      FOR EACH loan WHERE loan.cust-cat  EQ "�"
                      AND loan.cust-id   EQ person.person-id
                      AND loan.contract  EQ "proxy"
                      AND loan.open-date LE end-date
                      AND loan.end-date  GE beg-date
                    NO-LOCK:

         /*=== �஢��塞 ����७����� �� ᮮ⢥�ᢨ� �᫮��� �⡮� ===*/
         IF ChkProxy(ROWID(loan), ttPensAcct.acct-pfr) THEN
         DO:
            ASSIGN
               /*=== ��� �뤠� ����७���� ===*/ 
               mPrBeg = STRING(loan.open-date, "99.99.9999")
               /*=== ��� ����砭�� �ப� ����७���� ===*/ 
               mPrEnd = STRING(loan.end-date, "99.99.9999")
               /*=== ����७��� ��� ===*/ 
               mPrFIO = AgentName(loan.cont-code)
            .
            RUN Insert_TTName IN h_prnvd("fio[line]"     , mFIO).
            RUN Insert_TTName IN h_prnvd("birth[line]"   , mBirth).
            RUN Insert_TTName IN h_prnvd("proxy[line]"   , mProxy).
            RUN Insert_TTName IN h_prnvd("proxybeg[line]", mPrBeg).
            RUN Insert_TTName IN h_prnvd("proxyend[line]", mPrEnd).
            RUN Insert_TTName IN h_prnvd("proxyfio[line]", mPrFIO).
            RUN Insert_TTName IN h_prnvd("lastop[line]"  , mOpDate).
            RUN Insert_TTName IN h_prnvd("pfr[line]"     , mPFR).
            RUN NextCircle_TTName IN h_prnvd("line").

            ASSIGN
               mFIO   = ""
               mBirth = ""
               mProxy = ""
               mOpDate= ""
               mPFR   = ""
            .
         END. /* IF mPermFnd THEN */
      END. /* FOR EACH loan WHERE loan.cust-cat EQ "�" */
   END. /* IF ttPensAcct.proxy EQ YES THEN */
   ELSE 
   DO:
      RUN Insert_TTName IN h_prnvd("fio[line]"     , mFIO).
      RUN Insert_TTName IN h_prnvd("birth[line]"   , mBirth).
      RUN Insert_TTName IN h_prnvd("proxy[line]"   , mProxy).
      RUN Insert_TTName IN h_prnvd("proxybeg[line]", "").
      RUN Insert_TTName IN h_prnvd("proxyend[line]", "").
      RUN Insert_TTName IN h_prnvd("proxyfio[line]", "").
      RUN Insert_TTName IN h_prnvd("lastop[line]"  , mOpDate).
      RUN Insert_TTName IN h_prnvd("pfr[line]"     , mPFR).
      RUN NextCircle_TTName IN h_prnvd("line").
      /* �᫨ �� ������� ���� �� ���, � �������騥�� ����� ��������� */
      ASSIGN
         mFIO   = ""
         mBirth = ""
         mProxy = ""
      .
   END. /* IF ttPensAcct.proxy EQ YES THEN ... ELSE */


END. /* FOR EACH ttPensAcct WHERE ttPensAcct.print EQ YES */

RUN EndCircle_TTName IN h_prnvd("line").
RUN prnvd IN h_prnvd("pfrotch1").
