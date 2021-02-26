/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: a-params.p
      Comment: �����頥� ���� ����, ��� ���짮��⥫�, ��� ���ࠧ������� 
               �� 㬮�砭�� ��।���� �� �� �����䨪���� ᬥ�� ���
   Parameters: ID ᬥ��, ��������� ���
         Uses: ���
      Used by: ���
      Created: 19.03.2004 13:35 ligp    
     Modified: 05.04.2004 12:02 ligp     25339
     Modified: 15.04.2004 18:24 ligp     25345
     Modified: 07.05.2004 13:02 ligp     25343
     Modified: 15.07.2004 15:02 ligp     32087
     Modified: 24.09.2004 15:02 ligp     32092: �������� � 1433-� (�ਫ������ 3)֑������ �ࠢ�� �
                                         ���ᮢ�� �������
     Modified: 29.09.2004 15:09 ligp     32095: �������� � 1433-� (�ਫ������ 4)֊���� ��� ���
                                         ����筮�� � ��, 業���⥩�
     Modified: 02.11.2004 13:45 fedm     32607: �맮�� �ࠢ�筨��� � �஢�ન
     Modified: 09.12.2004 11:30 rija      
     Modified: 25.10.2006 15:28 ELUS     0066832: ��� ���� "��ࠢ�� � �ਭ�⮩ � �뤠����
                                         ����筮��" - spr-cash
     Modified: 24.04.2007 14:01 elus     ��ନ஢���� ॥��஢ ����権 ��᫥ ॠ����樨 58567
     Modified: 24.01.2008 15:11 elus     81126
     Modified: 18.02.2008 18:35 elus      0101185: ����࠭�祭�� ������ ������ ��� ������� �
                                          ���୥� �����
*/

{globals.i}
{intrface.get vok}
{intrface.get sessions}
{intrface.get tmess}
{intrface.get date}

RUN Init-SysMes("","","").
/*------------------------------------------------------------------------------
  �����祭��:  �����頥� ����, ��� ���짮��⥫�, ��� ���ࠧ������� 
  ��ࠬ����:   iDprID   - �����䨪��� ᬥ�� 
               iTitle   - ��������� ��� �롮�
               oDateRep - ��� �� ᬥ��
               oUserRep - ��� �
               oRecvRep - ��� ����� - �����⥫�
               oBrchRep - ��� ���ࠧ�������
               oPrmsOK  - ��ଠ�쭮 �� �� ��ࠫ�
               oBookEnd - ������ �� �����⥫��� ������� (�㦭� ��� ���� ���)
               oBookTit - ������ �� ����� ���� (�㦭� ��� ���� ���)               
               oBookTyp - ������ �� ����� ��� � ����⠬� ��� ���               
  �ਬ�砭��:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER iDprID   AS CHARACTER NO-UNDO. /* �����䨪��� ᬥ�� */
DEFINE INPUT  PARAMETER iTitle   AS CHARACTER NO-UNDO. /* ��������� ��� */  
DEFINE OUTPUT PARAMETER oDateRep AS DATE NO-UNDO.      /* ��� ���� */
DEFINE OUTPUT PARAMETER oBrchRep LIKE branch.branch-id NO-UNDO. /* ��� ���ࠧ������� */   
DEFINE OUTPUT PARAMETER oBrchKey LIKE branch.branch-id NO-UNDO. /* ���� �� ���ࠧ������� */   
DEFINE OUTPUT PARAMETER oUserRep LIKE _User._Userid NO-UNDO.       /* ��� � */
DEFINE OUTPUT PARAMETER oRecvRep LIKE _User._Userid NO-UNDO.       /* ��� �����⥫� */
DEFINE OUTPUT PARAMETER oRecvDpr AS CHARACTER             NO-UNDO.
DEFINE OUTPUT PARAMETER oPrmsOK  AS LOGICAL INITIAL FALSE NO-UNDO. /* �� �� �� */
DEFINE OUTPUT PARAMETER oBookEnd AS LOGICAL INITIAL FALSE NO-UNDO. /* ������ �� �����⥫��� ������� (�㦭� ��� ���� ���)*/
DEFINE OUTPUT PARAMETER oBookTit AS LOGICAL INITIAL FALSE NO-UNDO. /* ������ �� ����� ���� (�㦭� ��� ���� ���)*/
DEFINE OUTPUT PARAMETER oBookTyp AS LOGICAL INITIAL FALSE NO-UNDO. /* ������ �� �����⥫��� ������� (�㦭� ��� ���� ���)*/
DEFINE OUTPUT PARAMETER oChBlank AS LOGICAL INITIAL FALSE NO-UNDO. /* ���뢠�� �� ������ (�㦭� ��� spr-cash)*/
DEFINE OUTPUT PARAMETER oDprID   AS CHARACTER NO-UNDO.      /* Id ����� */
DEFINE OUTPUT PARAMETER oBalAcct AS INT64   NO-UNDO.      /* ��� 2-��� ���浪� */
DEFINE OUTPUT PARAMETER oItogEkv AS LOGICAL INITIAL FALSE NO-UNDO. /* �⮣ � �������� */
DEFINE OUTPUT PARAMETER oNameCen AS LOGICAL INITIAL FALSE NO-UNDO. /* ������������ 業���⥩ */
DEFINE OUTPUT PARAMETER oMoreTit AS LOGICAL INITIAL FALSE NO-UNDO. /* ������ ��  �⤥��� �訢� � v-tit-pg318p */
DEFINE OUTPUT PARAMETER oPrnAcct AS LOGICAL INITIAL FALSE NO-UNDO. /* ������ �� ��樢� ��� (book-reg318p) */
DEFINE OUTPUT PARAMETER oCashOrd AS LOGICAL INITIAL FALSE NO-UNDO. /* ������� ���-�� �� ᢮��� �थࠬ */
DEFINE OUTPUT PARAMETER oTotalCS AS LOGICAL INITIAL FALSE NO-UNDO. /* �⮣� �� �� */
DEFINE OUTPUT PARAMETER oByRate  AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oRstPril AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oByInsp  AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oRazbIsp AS LOGICAL INITIAL FALSE NO-UNDO. /* �������� �� �ᯮ���⥫� */
DEFINE OUTPUT PARAMETER oDateOtc AS CHARACTER             NO-UNDO.
DEFINE OUTPUT PARAMETER oFullItg AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oShowZero AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER mSessFlt AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oNCurrency AS LOGICAL  NO-UNDO.
DEFINE OUTPUT PARAMETER oICurrency AS LOGICAL  NO-UNDO.
DEFINE OUTPUT PARAMETER oRCurrency AS LOGICAL  NO-UNDO.
DEFINE OUTPUT PARAMETER oInUse   AS LOGICAL INITIAL TRUE  NO-UNDO.
DEFINE OUTPUT PARAMETER oPK      AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oByPrAcct  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER oTitPage AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oNextDay AS LOGICAL  INIT YES     NO-UNDO.

DEFINE VARIABLE vBookEnd  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vBookTit  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vBookTyp  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vChBlank  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vItogEkv  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vNameCen  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vMoreTit  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vPrnAcct  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vCashOrd  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vTotalCS  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vByRate   AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRstPril  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vByInsp   AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRazbIsp  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRazbDat  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vFullItg  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vShowZero AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vNCurrency AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vICurrency AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRCurrency AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE mInUse    AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE mPK       AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vTitPage  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vNextDay  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.

DEFINE VARIABLE mByPrAcct AS CHARACTER FORMAT "X(8)" 
  VIEW-AS COMBO-BOX INNER-LINES 4
  LIST-ITEMS "","������", "���୨�" 
  DROP-DOWN-LIST
  NO-UNDO.

DEFINE VARIABLE vSessFlt AS CHARACTER FORMAT "X(8)" 
  VIEW-AS COMBO-BOX INNER-LINES 4
  LIST-ITEMS "���","��","������", "���୨�" 
  DROP-DOWN-LIST
  NO-UNDO.

DEFINE VARIABLE mProgramName AS CHARACTER           NO-UNDO.
DEFINE VARIABLE mDprList     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE mDprListType AS CHARACTER INIT 1    NO-UNDO. /* "list" - ᬥ� ��᪮�쪮,
                                                                "*"    - ��, 
                                                                "1"    - ���� */
DEFINE VARIABLE vDateOtc     AS DATE                NO-UNDO.
DEFINE VARIABLE mDateBeg     AS DATE                NO-UNDO.
DEFINE VARIABLE mDateEnd     AS DATE                NO-UNDO.

ASSIGN
   vItogEkv  = oItogEkv
   vChBlank  = oChBlank
   vBookTyp  = oBookTyp
   vBookTit  = oBookTit
   vBookEnd  = oBookEnd
   vNameCen  = oNameCen
   vMoreTit  = oMoreTit
   vPrnAcct  = oPrnAcct
   vCashOrd  = oCashOrd
   vTotalCS  = oTotalCS
   vRazbIsp  = oRazbIsp   
   vShowZero = oShowZero   
   vNCurrency = oNCurrency
   vICurrency = oICurrency   
   vRCurrency = oRCurrency   
   vTitPage  = oTitPage
   vNextDay  = oNextDay
   mProgramName = ENTRY(1, PROGRAM-NAME(2), ".")
   .
IF iTitle = "" THEN
   iTitle = "����� ���������� ������".

FORM /* === ��ଠ ���४�஢�� ��ࠬ��஢ ���� === */
   oDateRep LABEL "��� �� ᬥ��"
            FORMAT "99/99/9999" HELP "��� ���भ� ᬥ��"
            AT ROW 1 COL 2

   oDprID   LABEL "��� ᬥ��" 
            FORMAT "x(7)"       HELP "��� ᬥ��"
            AT ROW 1 COL 30

   vDateOtc LABEL "��� ����"
            FORMAT "99/99/9999" HELP "��� ����"
            AT ROW 2 COL 4

   mDateBeg LABEL "��� ��砫�"
            FORMAT "99/99/9999" HELP "��� ��砫�"
            AT ROW 4 COL 30

   mDateEnd LABEL "��� �����."
            FORMAT "99/99/9999" HELP "��� ����砭��"
            AT ROW 6 COL 30

   vRazbDat LABEL "�������� �� ��⠬"
                                HELP "�������� �� ��⠬"
            AT ROW 2 COL 30
   
   vSessFlt LABEL "������ 䨫����� ᬥ�"
            HELP "�������� �� ⨯�� ᬥ�"
            AT ROW 3 COL 16

   oBrchRep LABEL "��� �����  "
            FORMAT "x(10)"      HELP "��� ���ࠧ�������"
            AT ROW 4 COL 2

   vBookTit LABEL "������ ����" 
            FORMAT "x(10)"      HELP "������ �� ����� ����"
            AT ROW 4 COL 30

   vTitPage LABEL "��. ����"
            FORMAT "x(10)"      HELP "����� ���쭮�� ���� �� �⤥�쭮� ����"
            AT ROW 5 COL 30

   oBalAcct LABEL "���. ����"
            FORMAT ">>>>>"      HELP "��������ᮢ� ���� 2-�� ���浪�"
            AT ROW 4 COL 30

   oUserRep LABEL "��� �����"
            FORMAT "x(10)"      HELP "��� �����"
            AT ROW 6 COL 2
   
   oRecvDpr LABEL "�ਭ�����"                   
            FORMAT "x(5)"       HELP "��� ᬥ��"
            AT ROW 6 COL 30

   oRecvRep LABEL "��� �����"
            FORMAT "x(10)"      HELP "��� �����, �ਭ����饣� 業����"
            AT ROW 6 COL 47

   vRazbIsp LABEL "�������� �� �ᯮ���⥫�"
                                HELP "�������� �� �ᯮ���⥫�"
            AT ROW 7 COL 2
   
   vNCurrency LABEL "�㡫����"
                                HELP "������� �஢���� � �㡫��"
            AT ROW 7 COL 2

   vICurrency LABEL "����⭠�"
                                HELP "������� �஢���� � �����"
            AT ROW 7 COL 15
   vRCurrency LABEL "����⭠� � ��. ��������"
                                HELP "������� �஢���� � ����� c �����⮬"
            AT ROW 7 COL 28

   vItogEkv LABEL "�⮣ � �������."
                                HELP "������ �� ��騩 �⮣"
            AT ROW 6 COL 30

   vPrnAcct LABEL "��楢� ���"
                                HELP "������ �� ��楢� ���"
            AT ROW 6 COL 30

   mByPrAcct LABEL "�ਧ��� ���"
             HELP "�������� �� �ਧ���� ��� �������/���୨�"
             AT ROW 7 COL 30

   vNextDay LABEL "���⮪ �� ᫥�. �� "
                                HELP " �⮡ࠦ��� ���� ᫥���饣� �� ����. ����"
            AT ROW 7 COL 2

   vByInsp  LABEL "��㯯. �� ���. ࠡ�⭨���"
                                HELP "��㯯�஢�� �� ��壠���᪨� ࠡ�⭨���"

            AT ROW 6 COL 30
   vBookEnd LABEL " �����⥫쭠� �������" 
                                HELP "������ �� �����⥫��� �������"
            AT ROW 8 COL 2

   oBrchKey LABEL "���� ��   "
                                HELP "���ࠧ������� �� ���ண� � �࠭���� �࠭���� �㡫����� ���祩"
            AT ROW 8 COL 2

   vChBlank LABEL " � 祪���"
                                HELP "������ �� ����� �� 祪��"
            AT ROW 8 COL 2

   vMoreTit LABEL " �⤥��� �訢� " 
                                HELP "������ �� �⤥��� �訢� "
            AT ROW 8 COL 2

   vBookTyp LABEL " � ����⠬�" 
                                HELP "������ �� ����� � ����⠬� �� ��⠬"
            AT ROW 8 COL 30

   vCashOrd LABEL " �� ᢮�. �थࠬ"
                                HELP "������ ᢮���� �थ஢"
            AT ROW 8 COL 30
   vTotalCS LABEL " �⮣� �� ��"
                                HELP "�⮣� �� ���ᮢ� ᨬ�����"
            AT ROW 4 COL 30
   vNameCen LABEL "����. 業���⥩" 
            FORMAT "x(10)"      HELP "������ �� ������������ 業���⥩"
            AT ROW 8 COL 30
   vByRate  LABEL " �⮣� �� ���ᠬ"
                                HELP "�⮣� �� �����"
            AT ROW 4 COL 30
   vFullItg LABEL " ��騥 �⮣�"
                                HELP "��騥 �⮣� "
            AT ROW 4 COL 50            
   vRstPril LABEL " �뢮� �ਫ������ � ॥����"
                                HELP "�뢮� �ਫ������ � ॥����"
            AT ROW 6 COL 30
   vShowZero LABEL " ��ࠦ��� �㫥�� ���⪨"
                                HELP "��ࠦ��� �㫥�� ���⪨ "
            AT ROW 8 COL 30
   mInUse   LABEL " �ਧ��� ����� � ���饭��"
                                HELP "������ � ���饭��"
            AT ROW 4 COL 30
   mPK      LABEL " ����� ��"
                                HELP "����� ����⨪���� ���⮩"
            AT ROW 6 COL 30
WITH FRAME FrameInpParams CENTERED ROW 10 OVERLAY SIDE-LABELS
   TITLE COLOR bright-white "[ " + iTitle + " ]" WIDTH 72.

ASSIGN
   vTotalCS:CHECKED = fGetSetting("��������","��������","���")  =  "��"
   vRazbIsp:CHECKED = fGetSetting("��������","��������","���") =  "��"
   mInUse:CHECKED   = YES
   vNextDay:CHECKED = YES
   vTitPage:CHECKED = YES
   .

/* =============== �������� ᬥ� ================================ */
ON F1,ANY-PRINTABLE OF vSessFlt IN FRAME FrameInpParams
DO:
   RETURN NO-APPLY.
END.

/* =============== �롮� ���भ� ================================== */
ON F1 OF oDateRep IN FRAME FrameInpParams
DO:
   RUN op-date.p (6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.
END.

ON F1 OF vDateOtc IN FRAME FrameInpParams
DO:
   RUN calend.p.

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF vDateOtc IN FRAME FrameInpParams
DO:
   ASSIGN vDateOtc.
   IF vDateOtc <> ? THEN
   DO:
      ASSIGN
         vRazbDat                                   = NO
         vRazbDat:SCREEN-VALUE                      = "no"
         vRazbDat:SENSITIVE IN FRAME FrameInpParams = NO
         mDateBeg                                   = ?
         mDateEnd                                   = ?
         mDateBeg:SCREEN-VALUE                      = ""
         mDateEnd:SCREEN-VALUE                      = ""
         mDateBeg:SENSITIVE IN FRAME FrameInpParams = NO
         mDateEnd:SENSITIVE IN FRAME FrameInpParams = NO
      .
   END.
   ELSE
   DO:
      ASSIGN
         vRazbDat:SENSITIVE IN FRAME FrameInpParams = YES
         mDateBeg:SENSITIVE IN FRAME FrameInpParams = YES
         mDateEnd:SENSITIVE IN FRAME FrameInpParams = YES.
      /* ENABLE vRazbDat WITH FRAME FrameInpParams. */
      /* APPLY "TAB" TO vRazbDat. */
   END.
END.

ON F1 OF mDateBeg IN FRAME FrameInpParams
DO:
   RUN calend.p.

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mDateBeg IN FRAME FrameInpParams
DO:
   ASSIGN mDateBeg.
END.

ON F1 OF mDateEnd IN FRAME FrameInpParams
DO:
   RUN calend.p.

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mDateEnd IN FRAME FrameInpParams
DO:
   ASSIGN mDateEnd.
END.

ON LEAVE OF vRazbDat IN FRAME FrameInpParams
DO:
   ASSIGN vRazbDat.
   IF vRazbDat THEN
   DO:
      ASSIGN
         vDateOtc                                   = ?
         vDateOtc:SCREEN-VALUE                      = ""
         vDateOtc:SENSITIVE IN FRAME FrameInpParams = NO
      .
   END.
   ELSE
   DO:
      ASSIGN
         vDateOtc                                   = ?
         vDateOtc:SCREEN-VALUE                      = ""
         vDateOtc:SENSITIVE IN FRAME FrameInpParams = YES
      .
      ENABLE vDateOtc WITH FRAME FrameInpParams.
   END.
END.

/* =============== �롮� ᬥ�� ============================ */
ON '*' OF oDprId IN FRAME FrameInpParams
DO:
   ASSIGN
      SELF:SCREEN-VALUE     = "*"
      mDprListType          = "*".
   IF vDateOtc =  ? AND mDateBeg =  ? THEN 
   ASSIGN
      mDateBeg              = TODAY - 1
      mDateBeg:SCREEN-VALUE = String(TODAY - 1,"99/99/9999").
   IF vDateOtc =  ? AND mDateEnd =  ? THEN 
   ASSIGN
      mDateEnd              = TODAY - 1
      mDateEnd:SCREEN-VALUE = String(TODAY,"99/99/9999").
   RETURN.
END.

ON F1,ANY-PRINTABLE OF oDprId IN FRAME FrameInpParams
DO:
   DEFINE VARIABLE mParDate1  AS DATE       NO-UNDO.
   DEFINE VARIABLE mParDate2  AS DATE       NO-UNDO.

   ASSIGN
      mParDate1 = oDateRep
      mParDate2 = mParDate1
      .
   DO WITH FRAME FrameInpParams:
     IF (oRecvDpr:HIDDEN =  NO) AND (oRecvRep:HIDDEN =  NO) AND iTitle BEGINS "���" THEN
     DO: /* ��� ��।�� ����筮�� � ������� */
        ASSIGN 
           mParDate1 = PrevWorkDay(mParDate1)
           mParDate2 = NextWorkDay(mParDate2)
           .
     END.
   END.
   
   pick-value = "".
   RUN browseld.p ("sessions",
                   "op-date1"          + CHR(1) + "op-date2"         + CHR(1) + "ActionLock"          + CHR(1) + "Return",
                   STRING(mParDate1)   + CHR(1) + STRING(mParDate2)  + CHR(1) + "ENTER,F1,INS,DEL,F8" + CHR(1) + "seslist",
                   "ActionLock"        + CHR(1) + "Return",
                   5).

   IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN
   DO:
      ASSIGN
         SELF:SCREEN-VALUE = IF NUM-ENTRIES(pick-value) > 1 THEN "list" ELSE pick-value
         mDprListType      = IF NUM-ENTRIES(pick-value) > 1 THEN "list" ELSE "1"
         mDprList          = pick-value
      .
      APPLY "LEAVE" TO SELF.
   END.
   RETURN NO-APPLY.
END.

ON F1,ANY-PRINTABLE OF oRecvDpr IN FRAME FrameInpParams
DO:
   DEFINE VARIABLE mParDate1  AS DATE       NO-UNDO.
   DEFINE VARIABLE mParDate2  AS DATE       NO-UNDO.

   ASSIGN
      mParDate1 = oDateRep
      mParDate2 = mParDate1
      .
   DO WITH FRAME FrameInpParams:
     IF (oRecvDpr:HIDDEN =  NO) AND (oRecvRep:HIDDEN =  NO) AND iTitle BEGINS "���" THEN
     DO: /* ��� ��।�� ����筮�� � ������� */
        ASSIGN 
           mParDate1 = PrevWorkDay(mParDate1)
           mParDate2 = NextWorkDay(mParDate2)
           .
     END.
   END.
   
   pick-value = "".
   RUN browseld.p ("sessions",
                   "op-date1"          + CHR(1) + "op-date2"         + CHR(1) + "ActionLock"          + CHR(1) + "Return",
                   STRING(mParDate1)   + CHR(1) + STRING(mParDate2)  + CHR(1) + "ENTER,F1,INS,DEL,F8" + CHR(1) + "seslist",
                   "",
                   5).

   IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN
   DO:
      FIND FIRST sessions WHERE 
         sessions.dpr-id =  INT64(pick-value)
         NO-LOCK NO-ERROR.
      IF AVAILABLE sessions THEN
      DO:
         ASSIGN
            SELF:SCREEN-VALUE = pick-value
            oRecvDpr          = pick-value
            oRecvRep:SCREEN-VALUE IN FRAME FrameInpParams = sessions.user-id
            oRecvRep          = sessions.user-id
            .
      END.
   END.
   RETURN NO-APPLY.
END.

/* =============== ��८�।������ ��ࠬ��஢ ============================ */
ON LEAVE OF oDprId IN FRAME FrameInpParams
DO:
   DEFINE VARIABLE vIntDpr AS INT64     NO-UNDO.
   DEFINE VARIABLE vOpDate AS DATE        NO-UNDO.
   DEFINE VARIABLE i       AS INT64     NO-UNDO.

   CASE vSessFlt:SCREEN-VALUE IN FRAME FrameInpParams:
      WHEN "���" THEN
      DO:
         mDprListType = mDprListType.
      END.
      WHEN "������" OR WHEN "���୨�" OR WHEN "��" THEN
      DO:
         mDprListType = "*".
      END.
   END CASE.
   CASE mDprListType:
      WHEN "1" THEN
      DO:
         ASSIGN
            oDprId   = SELF:SCREEN-VALUE
            vIntDpr  = INT64(oDprId) NO-ERROR
            .
         FIND FIRST sessions WHERE 
                    sessions.dpr-id =  vIntDpr 
            NO-LOCK NO-ERROR.
         IF AVAIL sessions THEN
            ASSIGN
               oBrchRep:SCREEN-VALUE = sessions.Branch-Id
               oUserRep:SCREEN-VALUE = sessions.user-id
            .
      END.
      WHEN "*" THEN
      DO:
         IF iTitle =  "������ ����������,���������� ����� ���������" THEN
         DO:
            RUN Fill-SysMes("",
                            "",
                            0,
                            "��� ���� ~"��ୠ� ����筮��, ��।����� ����� ����ࠬ�~"
                             ����室��� 㪠�뢠�� �������� ᬥ��.").
            RETURN NO-APPLY SELF:NAME.
         END.

         CASE vSessFlt:SCREEN-VALUE IN FRAME FrameInpParams:
            WHEN "��" THEN
            DO:
               ASSIGN
                  oDprId   = "*"
                  mDprList = "*".
            END.
            WHEN "������" THEN
            DO:
               ASSIGN
                  oDprId   = "*"
                  mDprList = GetListOpenSessionsDV(work-module,oDateRep,"�",oBrchRep:SCREEN-VALUE)
                  .
            END.
            WHEN "���୨�" THEN
            DO:
               ASSIGN
                  oDprId   = "*"
                  mDprList = GetListOpenSessionsDV(work-module,oDateRep,"�",oBrchRep:SCREEN-VALUE)
                  .
            END.
            OTHERWISE
            DO:
               oDprId = "*".
            END.
         END CASE.
      END.
      WHEN "list" THEN
      DO:
         vIntDpr = INT64(ENTRY(1,mDprList)) NO-ERROR.
         FIND FIRST sessions WHERE
                    sessions.dpr-id =  vIntDpr
            NO-LOCK NO-ERROR.
         IF AVAIL sessions THEN
            ASSIGN
               oBrchRep:SCREEN-VALUE = sessions.Branch-Id
               vOpDate               = sessions.op-date
            .
         DO i = 2 TO NUM-ENTRIES(oDprId):
            vIntDpr = INT64(ENTRY(i,oDprId)) NO-ERROR.
            FIND FIRST sessions WHERE
                       sessions.dpr-id =  vIntDpr
               NO-LOCK NO-ERROR.
            IF     AVAIL sessions 
               AND (   sessions.op-date   <> vOpDate
                    OR sessions.Branch-Id <> oBrchRep:SCREEN-VALUE) THEN
            DO:
               RUN Fill-SysMes("",
                               "",
                               0,
                               "����� ������ �⭮���� � ������ ���ࠧ������� � ���� ������ � ����� ����. ���").
               ASSIGN
                  oDprId            = ""
                  mDprList          = ""
                  SELF:SCREEN-VALUE = ""
                  .
               RETURN NO-APPLY.
            END.
         END.
      END.
   END CASE.
END.

/* =============== �롮� ���ࠧ������� ============================ */
ON F1 OF oBrchRep,oBrchKey IN FRAME FrameInpParams
DO:
   RUN vok-bran.p (6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* =============== �஢�ઠ ���������� ���� ���ࠧ������� ========= */
ON LEAVE OF oBrchRep IN FRAME FrameInpParams
DO:
   DEF VAR vBranch AS CHAR  NO-UNDO.

   vBranch = SELF:SCREEN-VALUE.

   IF vBranch <> ""   AND
      vBranch <> "*"  AND
      NOT BranchIsVOK(vBranch) THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      0,
                      "���ࠧ������� " + vBranch + " �� ���� ���ᮩ.").
      RETURN NO-APPLY SELF:NAME.
   END.
   RETURN.
END.

/* =============== �஢�ઠ ���������� ���� ���ࠧ������� ========= */
ON LEAVE OF oBrchKey IN FRAME FrameInpParams
DO:
   DEF VAR vBranch AS CHAR  NO-UNDO.

   vBranch = SELF:SCREEN-VALUE.

   IF NOT BranchIsVOK(vBranch) THEN
   DO:    
      RUN Fill-SysMes("",
                      "",
                      0,
                      "���ࠧ������� " + vBranch + " �� ���� ���ᮩ.").
      RETURN NO-APPLY SELF:NAME.
   END.
   RETURN.
END.

/* =============== �஢�ઠ ���������� �����ᮢ��� ��� ========= */
ON LEAVE OF oBalAcct IN FRAME FrameInpParams
DO:
   DEF VAR vBalAcct AS INT64  NO-UNDO.

   vBalAcct = INT64(SELF:SCREEN-VALUE).

   FIND FIRST bal-acct WHERE 
              bal-acct.bal-acct =  vBalAcct 
          AND bal-acct.acct-cat =  "o"
      NO-LOCK NO-ERROR.
   IF NOT AVAIL bal-acct THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      0,
                      "������� �� ��୮� ���祭�� ��� ��ண� ���浪�").
      RETURN NO-APPLY SELF:NAME.
   END.
   RETURN.
END.

/* =============== �롮� �����ᮢ��� ��� ======================== */
ON F1 OF oBalAcct IN FRAME FrameInpParams
DO:
   RUN browseld("bal-acct",
                "acct-cat",
                "o",
                "acct-cat",
                6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.

END.

/* =============== �롮� ����� ��� ============================== */
ON F1 OF oUserRep,oRecvRep IN FRAME FrameInpParams
DO:
   RUN vok-user.p (6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* =============== �஢�ઠ ���������� ���� ����� =============== */
ON LEAVE OF oUserRep,oRecvRep IN FRAME FrameInpParams
DO:
   DEF VAR vUser AS CHAR  NO-UNDO.
   
   DEFINE BUFFER sessions FOR sessions. 
   
   FIND FIRST sessions WHERE sessions.dpr-id =  INT64(iDprID)
      NO-LOCK NO-ERROR. 

   vUser = SELF:SCREEN-VALUE.
   
   IF AVAIL sessions AND sessions.user-id <> vUser THEN
      IF vUser <> ""   AND
         vUser <> "*"  AND
         NOT UserIsCashier (vUser) THEN
      DO:
         RUN Fill-SysMes("",
                         "",
                         0,
                         "����㤭�� " +  vUser + " �� ���� ����஬.").
         RETURN NO-APPLY SELF:NAME.
      END. 
   RETURN.
END.

/* ============ � � � � �����६���� �� �ࠡ�⠥� =============== */
ON LEAVE OF vChBlank IN FRAME FrameInpParams
DO:
   IF vChBlank:CHECKED THEN
      vCashOrd:CHECKED = NO.
END.

ON LEAVE OF vCashOrd IN FRAME FrameInpParams
DO:
   IF vCashOrd:CHECKED THEN
      ASSIGN
         vChBlank:CHECKED = NO
         vMoreTit:CHECKED = NO
         vTotalCS:CHECKED = NO
         .
END.

ON LEAVE OF vTotalCS IN FRAME FrameInpParams
DO:
   IF vTotalCS:CHECKED THEN
      vCashOrd:CHECKED = NO.
END.

ON LEAVE OF vMoreTit IN FRAME FrameInpParams
DO:
   IF vMoreTit:CHECKED THEN
      vCashOrd:CHECKED = NO.
END.

/* =============== ����஫� � ��࠭���� ������� ������ ========= */
ON GO OF FRAME FrameInpParams ANYWHERE
DO:
   APPLY "LEAVE" TO oDprId.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO oBrchRep.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO oUserRep.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO mDateBeg.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO mDateEnd.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   IF oBrchKey:VISIBLE THEN
   DO:
      APPLY "LEAVE" TO oBrchKey.
      IF RETURN-VALUE <> "" THEN
         RETURN NO-APPLY.
   END.
   IF oBalAcct:VISIBLE THEN
   DO:
      APPLY "LEAVE" TO oBalAcct.
      IF RETURN-VALUE <> "" THEN
         RETURN NO-APPLY.
   END.

   IF oRecvRep:VISIBLE THEN
   DO:
      APPLY "LEAVE" TO oRecvRep.
      IF RETURN-VALUE <> "" THEN
         RETURN NO-APPLY.
   END.

   ASSIGN
      mByPrAcct
      oBrchRep
      oBrchKey
      oUserRep
      oRecvRep
      oBalAcct.
END.

/* ============== ���⠢�塞 ��ࠬ���� �⡮� �� 㬮�砭�� ======== */
oDateRep = gend-date.  /* �롨ࠥ� ���� ���� - ����. ���� */
IF oDateRep = ? THEN 
   oDateRep = TODAY.

IF iDprID = ? THEN 
DO: /* �᫨ �맮� ���� �� �� ᬥ�� ���, 㧭��� ��ࠬ���� ����᪠ ���� �� ������ */
   oUserRep = USERID("bisquit").
   oBrchRep = GetUserXAttrValue(oUserRep, "�⤥�����").
   oDprID   = "*".
END.
ELSE 
DO: /* �᫨ �맮� ���� �� ᬥ�� ���, 㧭��� ��ࠬ���� ����᪠ ���� �� ᬥ�� ��� */
   oUserRep = GetUserIDFromSessions(INT64(iDprID)).
   oBrchRep = GetBranchIDFromSessions(INT64(iDprID)).
   oDprID   = iDprID.
END.
IF iTitle BEGINS "����" THEN
DO: /* �᫨ ���� "������", � �� 㬮�砭�� ��⠥��� �� �ᥬ ����ࠬ �믮����� */
   oUserRep = "*".
   oDprID   = "*".
END.

oRecvRep = oUserRep. /* ���� ⠪. �� ��ன ����� �� ���⮩ �� */

/* =========================== Main block ============================*/
PAUSE 0.

FrameInpParams: /* ������塞 ��ࠬ���� �⡮� */
DO TRANSACTION ON ERROR  UNDO FrameInpParams, LEAVE FrameInpParams 
               ON ENDKEY UNDO FrameInpParams, LEAVE FrameInpParams:

   DISPLAY 
      oDprId
      vSessFlt  
      oDateRep
      vDateOtc
      vRazbDat
      oBrchRep
      oBrchKey
      oUserRep
      oRecvDpr
      oRecvRep      
      oBalAcct
   WITH FRAME FrameInpParams.

   IF oDprID = ? THEN /* �맮� �� ���� ��� �. �������਩ � ��� 0025339 */
   DO:
      ENABLE
         oDprId
         vSessFlt 
         oDateRep
         vDateOtc
         vRazbDat
         oBrchRep
         oBrchKey
         oUserRep
         oRecvDpr
         oRecvRep         
      WITH FRAME FrameInpParams.
   END.
   ELSE               /* �맮� �� ᬥ�� ��� �. �������਩ � ��� 0025339 */
   DO:
      ENABLE 
         oDprId
         vDateOtc
         vRazbDat
         vSessFlt 
         oUserRep
         oRecvDpr
         oRecvRep
      WITH FRAME FrameInpParams.
   END.

   IF work-module =  "hran" THEN
      HIDE vSessFlt
        IN FRAME FrameInpParams.
   /*
   IF mProgramName EQ "jou-ann-pl"THEN
      HIDE 
      vDateOtc
      vSessFlt
      vRazbDat
        IN FRAME FrameInpParams.
  */

  /* � ����設�⢥ ���⮢ �� ��ࠬ���� �� �ᯮ������� */
   HIDE 
      vBookEnd 
      vMoreTit   
      vBookTit 
      vNameCen 
      vBookTyp 
      oRecvDpr
      oRecvRep 
      vChBlank 
      vByInsp
      vItogEkv 
      vPrnAcct 
      mByPrAcct
      vCashOrd
      vTotalCS
      vByRate
      vFullItg      
      vRstPril
      oBalAcct 
      oBrchKey
      vRazbIsp
      vShowZero
      vNCurrency
      vICurrency
      vRCurrency
      mInUse
      mPK
      mDateBeg
      mDateEnd
      vTitPage
      vNextDay
   IN FRAME FrameInpParams.

   IF CAN-DO("rst-cm,r-rinvok,book-reg,joukasin,joukasout,book-val,rst-136i,rst-opn,cas-svod,spr-cash,v-tit-pg",mProgramName) THEN
      HIDE 
         vDateOtc
         vRazbDat
         IN FRAME FrameInpParams.      

   CASE iTitle:
      WHEN "����� ���������� ������" THEN
         ENABLE
            oBrchKey
         WITH FRAME FrameInpParams.

      WHEN "��� �� ����������� ���������" THEN
         ENABLE
            oBalAcct
         WITH FRAME FrameInpParams.

      WHEN "������� � ����������" THEN
         ENABLE
            vChBlank
         WITH FRAME FrameInpParams.
      WHEN "�������� �������" THEN
      DO:
         vByInsp:CHECKED = YES.
         ENABLE
            vChBlank
            vByInsp
         WITH FRAME FrameInpParams.
      END.

      WHEN "��������� ���� �������� ���������� ���(318-�)" THEN
         ENABLE 
            vMoreTit
         WITH FRAME FrameInpParams.

      WHEN "����������� ����" THEN
         ENABLE
            vNameCen
         WITH FRAME FrameInpParams.

      WHEN "����� ����� �������� � �������� �����" THEN
         ENABLE
            vBookEnd
            vNameCen
         WITH FRAME FrameInpParams.

      WHEN "����� ����� �������� ����������" THEN
         ENABLE
            vBookEnd
            vBookTit
            vItogEkv
            vBookTyp
         WITH FRAME FrameInpParams.

      WHEN "����� ��������� ���������" THEN
         ENABLE
            vNextDay
            vBookEnd
            vBookTit
            vTitPage
            vPrnAcct
            mByPrAcct
            vShowZero
         WITH FRAME FrameInpParams.

      WHEN "������ ���������� ��������" THEN DO:
         ENABLE
            oBrchRep
            mDateBeg
            mDateEnd
         WITH FRAME FrameInpParams.
         HIDE 
            vRazbDat
            vSessFlt
         IN FRAME FrameInpParams.      

      END.
      WHEN "����� ����� ��" THEN
         ENABLE
            vBookTit
         WITH FRAME FrameInpParams.

      OTHERWISE
      DO:
         IF iTitle BEGINS "���" THEN
         DO:
            oRecvRep = "".
            DISPLAY 
               oRecvRep
               WITH FRAME FrameInpParams.

            ENABLE
               oRecvDpr
               oRecvRep
            WITH FRAME FrameInpParams.
            DO WITH FRAME FrameInpParams:
               vSessFlt:HIDDEN    = YES.
               oUserRep:SENSITIVE = NO.
               oRecvRep:SENSITIVE = NO.
            END.
         END.

         IF iTitle BEGINS "�����" OR 
            iTitle =  "��������� ���� �������� ���������� ���" OR
            iTitle =  "������ ����� �������� � �������� ���������" THEN
         DO:
            ENABLE 
               vBookEnd
            WITH FRAME FrameInpParams.
         END.
      END.
   END CASE.

   CASE mProgramName:
      WHEN "cas-svod318p"   OR 
      WHEN "cas-svod318p-o" THEN
         ENABLE 
            vNCurrency
            vICurrency
            vRCurrency
            vCashOrd
         WITH FRAME FrameInpParams.
      WHEN "v-tit-pg318p"   OR 
      WHEN "spr-cash318p"   OR
      WHEN "v-tit-pg318p-o" OR 
      WHEN "spr-cash318p-o" THEN
         ENABLE 
            vCashOrd
         WITH FRAME FrameInpParams.
      WHEN "rvok-pos" THEN
      DO:
         ENABLE
            vBookEnd
            vNameCen
            WITH FRAME FrameInpParams.
         DO WITH FRAME FrameInpParams:
            vBookEnd:LABEL = "��������ᮢ� ���".
         END.
      END.
      WHEN "cashjour" THEN
         ENABLE
            vRazbIsp 
            vTotalCS
            vCashOrd
         WITH FRAME FrameInpParams.
      WHEN "rst-136i" THEN
      DO:
         vByRate:CHECKED = YES.
         ENABLE 
            vByRate
            vFullItg            
            vRstPril
         WITH FRAME FrameInpParams.
         HIDE
            vDateOtc
            vRazbDat
            IN FRAME FrameInpParams.
         
      END.
      WHEN "rst-cm" THEN
      DO:
         ENABLE
            mInUse
            mPK
         WITH FRAME FrameInpParams.
         HIDE
            vSessFlt
            IN FRAME FrameInpParams.
      END.
   END CASE.
   if PROGRAM-NAME(2) = "akt-reg318p.p" then   LEAVE FrameInpParams.
   WAIT-FOR GO OF CURRENT-WINDOW OR WINDOW-CLOSE OF CURRENT-WINDOW.
END.

HIDE FRAME FrameInpParams NO-PAUSE.
IF LASTKEY <> 10 AND LASTKEY <> 13 THEN 
DO:
   {intrface.del}
   RETURN.
END.
ASSIGN
   oDateRep
   oBrchRep
   oBrchKey
   oUserRep
   oDprId
   vSessFlt
   oRecvRep
   oBalAcct
   oByPrAcct = mByPrAcct
   oPrmsOK  = TRUE
   oBookEnd = vBookEnd:CHECKED
   oMoreTit = vMoreTit:CHECKED
   oBookTit = vBookTit:CHECKED
   oBookTyp = vBookTyp:CHECKED
   oChBlank = vChBlank:CHECKED
   oByInsp  = vByInsp:CHECKED
   oItogEkv = vItogEkv:CHECKED
   oPrnAcct = vPrnAcct:CHECKED
   oNameCen = vNameCen:CHECKED
   oCashOrd = vCashOrd:CHECKED
   oTotalCS = vTotalCS:CHECKED
   oByRate  = vByRate:CHECKED
   oFullItg = vFullItg:CHECKED   
   oRstPril = vRstPril:CHECKED
   oRazbIsp = vRazbIsp:CHECKED
   oShowZero = vShowZero:CHECKED 
   oNCurrency = vNCurrency:CHECKED
   oICurrency = vICurrency:CHECKED
   oRCurrency = vRCurrency:CHECKED
   oInUse   = mInUse:CHECKED
   oPK      = mPK:CHECKED
   oTitPage = vTitPage:CHECKED
   oNextDay = vNextDay:CHECKED
   oDateOtc = IF vDateOtc <> ? THEN STRING(vDateOtc, "99/99/9999")
                               ELSE (IF vRazbDat THEN "*"
                                                 ELSE (
                               IF mDateBeg <> ?  AND mDateEnd <> ? THEN 
                                                                   STRING(mDateBeg, "99/99/9999")
                                                                 + ","
                                                                 + STRING(mDateEnd, "99/99/9999")
                                                 ELSE (
                               IF mDateBeg <> ?  THEN STRING(mDateBeg, "99/99/9999")
                                                 ELSE (
                               IF mDateEnd <> ?  THEN STRING(mDateEnd, "99/99/9999")                     
                                                 ELSE ""
                                                    ))))
.

/* ====================== ���塞 ��ࠬ���� �⡮� ========================= */
IF oUserRep = "" OR oUserRep = "*" THEN 
   oUserRep = ?.   /* �᫨ user �� ��࠭, � ���� �� �ᥬ */

IF oBrchRep = "" OR oBrchRep = "*" THEN
   oBrchRep = ?.   /* �᫨ ���� �� ��࠭�, � ���� �� �ᥬ */

/* �᫨ ��祣� �� ��࠭�, � ���� �� ���ࠧ� ��।��塞��� �� ������ � */
IF oUserRep = ? AND oBrchRep = ? THEN 
   oBrchRep = GetUserXAttrValue(USERID("bisquit"), "�⤥�����").

/* GetUserXAttrValue �����頥� "*" �� ��㤠�, � ��� ���� "?" */
IF oBrchRep = "*" THEN
   oBrchRep = ?.

IF oDprId =  "list" THEN
   oDprId = mDprList.

IF vSessFlt <> "���" THEN
   oDprId = mDprList.

mSessFlt = vSessFlt:SCREEN-VALUE IN FRAME FrameInpParams.

/* �᫨ �⡮� �� �� �ᥬ � � �� ᮮ⢥����� ���ࠧ������� */
IF oDprId  =  "*" AND
   oUserRep <> ? AND 
   oBrchRep <> ? AND
   GetUserXAttrValue(oUserRep, "�⤥�����") <> oBrchRep THEN
DO:
   MESSAGE "��� ���ࠧ�������, �� ᮮ⢥����� ���� �����." SKIP
           "�ਮ��� ����� ��� �����."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   oBrchRep = GetUserXAttrValue(oUserRep, "�⤥�����").
END.
/* =========================== Main block ============================*/
{intrface.del}


/* ���ꥬ �� � ��� � 56 ���� (0122104) */
/* $LINTFILE='a-params.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:35.471+03:00' */
/*prosign2iYBXHQa/vrpVrugTp2jpQ*/