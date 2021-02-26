/*nbki-qry.p*/

{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}

{sh-defs.i}
{tmpobj.def}
{tmprecid.def}
{clnt.fun}

DEFINE VARIABLE mTypeQ    AS CHARACTER NO-UNDO VIEW-AS COMBO-BOX INNER-LINES 5.
DEFINE VARIABLE mFName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBDay     AS DATE      NO-UNDO.
DEFINE VARIABLE mBPlace   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSNum     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVydan    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAddr1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAddr2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAddr3    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTelNum   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNalogNum   AS CHARACTER NO-UNDO.

DEFINE VARIABLE mStahNum   AS CHARACTER NO-UNDO.

DEFINE VARIABLE mAcctCr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmt      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mSymbol   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDetail   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustID   AS INT64     NO-UNDO.

DEFINE VARIABLE mInt      AS INT64     NO-UNDO.

FORM
   mTypeQ  LABEL  "��� �����" 
           FORMAT "x(55)"
           HELP   "��� �����"  
   mFName  LABEL  "  �������" 
           FORMAT "x(55)"
           HELP   "������� (F1 - �롮� ������ �� �ࠢ�筨��)"
   mName   LABEL  "���" 
           FORMAT "x(56)"
           HELP   "���"            
   mSName  LABEL  "����⢮" 
           FORMAT "x(56)"
           HELP   "����⢮"
   mBDay   LABEL  "��� ஦�����" 
           FORMAT "99/99/9999"
           HELP   "��� ஦�����"
   mBPlace LABEL  "���� ஦�����" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "���� ஦�����"          
   mSNum   LABEL  "���� �����" 
           FORMAT "x(12)"
           HELP   "���� � ����� ���㬥��"
   mKodP   LABEL  "���.����." 
           FORMAT "x(7)"
           HELP   "��� ���ࠧ�������"                      
   mVydan  LABEL  "�뤠�" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "��� � ����� �뤠� ���㬥��"
   mAddr1  LABEL  "���� �ய�᪨" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "���� �ய�᪨"
   mAddr2  LABEL  "���� ��� ����ᯮ����樨" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "���� ��� ���ࠢ����� ����ᯮ����樨"
   mAddr3  LABEL  "���� �஦������" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "���� �஦������"
   mTelNum LABEL  "����䮭" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "����� ⥫�䮭�"                      
   mStahNum LABEL  "���� �����" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "���客�� �����"

   mNalogNum LABEL  "����� ���������⥫�騪�" 
           FORMAT "x(560)"
           VIEW-AS FILL-IN SIZE 56 BY 1
           HELP   "�����䨪�樮��� ����� ���������⥫�騪�"

WITH FRAME frClient OVERLAY CENTERED ROW 10 SIDE-LABELS 1 COL
TITLE " ����� ����� ".

ASSIGN 
   mTypeQ:LIST-ITEMS = "��㣠 '����� ᢮� �।���� �����',"
                     + "��㣠 '����� ��� �࠭���� �।�⭠� �����',"
                     + "��㣠 '��� � ������'," 
                     + "���/����/�ନ� ���.���� ��ꥪ� �।.���ਨ'"
                     .

ON 'ENTER':U OF mTypeQ
DO:
   APPLY "ENTRY" TO mFName.
   RETURN NO-APPLY.
END.
         
ON LEAVE OF mTypeQ
DO:
   mInt = LOOKUP(mTypeQ:SCREEN-VALUE,mTypeQ:LIST-ITEMS).
   CASE mInt:
   WHEN 1 THEN
   ASSIGN
      mAcctCr = (IF shFilial EQ "0000" THEN "60322810300022790405" ELSE 
                (IF shFilial EQ "0300" THEN "60322810903002790405" ELSE 
                (IF shFilial EQ "0500" THEN "60322810905922790405" ELSE ""))) 
      mAmt    = 900
      mSymbol = "32"
      mDetail = "������� �� �।��⠢����� �।�⭮�� ���� �� ����. � ⮬ �᫥ ��� 18%."
      mBDay:SENSITIVE   IN FRAME frClient = YES
      mBPlace:SENSITIVE IN FRAME frClient = YES
      mSNum:SENSITIVE   IN FRAME frClient = YES
      mKodP:SENSITIVE   IN FRAME frClient = YES
      mVydan:SENSITIVE  IN FRAME frClient = YES
      mAddr1:SENSITIVE  IN FRAME frClient = YES
      mAddr2:SENSITIVE  IN FRAME frClient = YES
      mAddr3:SENSITIVE  IN FRAME frClient = YES
      mTelNum:SENSITIVE IN FRAME frClient = YES
      mStahNum:SENSITIVE IN FRAME frClient = YES
      mNalogNum:SENSITIVE IN FRAME frClient = YES.
   WHEN 2 THEN
   ASSIGN
      mAcctCr = (IF shFilial EQ "0000" THEN "60322810300022790405" ELSE
                (IF shFilial EQ "0300" THEN "60322810903002790405" ELSE
                (IF shFilial EQ "0500" THEN "60322810905922790405" ELSE "")))
      mAmt    = 690
      mSymbol = "32"
      mDetail = "������� �� �।��⠢����� ���� �� ����ࠫ쭮�� ��⠫��� �।���� ���਩. � ⮬ �᫥ ��� 18%."
      mBDay:SENSITIVE   IN FRAME frClient = YES
      mBPlace:SENSITIVE IN FRAME frClient = YES
      mSNum:SENSITIVE   IN FRAME frClient = YES
      mKodP:SENSITIVE   IN FRAME frClient = YES
      mVydan:SENSITIVE  IN FRAME frClient = YES
      mAddr1:SENSITIVE  IN FRAME frClient = YES
      mAddr2:SENSITIVE  IN FRAME frClient = NO
      mAddr3:SENSITIVE  IN FRAME frClient = YES
      mTelNum:SENSITIVE IN FRAME frClient = YES
      mAddr2:SCREEN-VALUE = ""
      mStahNum:SENSITIVE IN FRAME frClient = YES
      mNalogNum:SENSITIVE IN FRAME frClient = YES.
   WHEN 3 THEN
   ASSIGN
      mAcctCr = (IF shFilial EQ "0000" THEN "60322810300022790405" ELSE
                (IF shFilial EQ "0300" THEN "60322810903002790405" ELSE
                (IF shFilial EQ "0500" THEN "60322810905922790405" ELSE "")))
      mAmt    = 900
      mSymbol = "32"
      mDetail = "������� �� �।��⠢����� ���� �� ���� ������ ���� �� ��६������� ����࠭ᯮ���� �।��. � ⮬ �᫥ ��� 18%."
      mBDay:SENSITIVE   IN FRAME frClient = NO
      mBPlace:SENSITIVE IN FRAME frClient = NO
      mSNum:SENSITIVE   IN FRAME frClient = NO
      mKodP:SENSITIVE   IN FRAME frClient = NO
      mVydan:SENSITIVE  IN FRAME frClient = NO
      mAddr1:SENSITIVE  IN FRAME frClient = NO
      mAddr2:SENSITIVE  IN FRAME frClient = NO
      mAddr3:SENSITIVE  IN FRAME frClient = NO
      mTelNum:SENSITIVE IN FRAME frClient = NO
      mBDay:SCREEN-VALUE = ""
      mBPlace:SCREEN-VALUE = ""
      mSNum:SCREEN-VALUE = ""
      mVydan:SCREEN-VALUE = ""
      mAddr1:SCREEN-VALUE = ""
      mAddr2:SCREEN-VALUE = ""
      mAddr3:SCREEN-VALUE = ""
      mTelNum:SCREEN-VALUE = "".
      /*mStahNum:SENSITIVE IN FRAME frClient = YES
      mNalogNum:SENSITIVE IN FRAME frClient = YES. */
   WHEN 4 THEN
   ASSIGN
      mAcctCr = (IF shFilial EQ "0000" THEN "60322810300022790405" ELSE 
                (IF shFilial EQ "0300" THEN "60322810903002790405" ELSE 
                (IF shFilial EQ "0500" THEN "60322810905922790405" ELSE ""))) 
      mAmt    = 500
      mSymbol = "32"
      mDetail = "���������, ���㫨஢����, �ନ஢���� �������⥫쭮�� ���� ��ꥪ� �।�⭮� ���ਨ. � �.�. ���."
      mBDay:SENSITIVE   IN FRAME frClient = YES
      mBPlace:SENSITIVE IN FRAME frClient = YES
      mSNum:SENSITIVE   IN FRAME frClient = YES
      mKodP:SENSITIVE   IN FRAME frClient = YES
      mVydan:SENSITIVE  IN FRAME frClient = YES
      mAddr1:SENSITIVE  IN FRAME frClient = YES
      mAddr2:SENSITIVE  IN FRAME frClient = YES
      mAddr3:SENSITIVE  IN FRAME frClient = YES
      mTelNum:SENSITIVE IN FRAME frClient = YES
      mStahNum:SENSITIVE IN FRAME frClient = YES
      mNalogNum:SENSITIVE IN FRAME frClient = YES.
   END CASE.
   APPLY "ENTRY" TO mFName.
   RETURN NO-APPLY.
END.

ON 'F1':U OF mFName IN FRAME frClient
DO:
   RUN person.p (4).
   IF KEYFUNCTION (LASTKEY) NE "end-error" AND
      pick-value            NE ? THEN
   DO:
      mCustID = INT64(pick-value).
      FIND FIRST person WHERE
         person.person-id EQ mCustID 
      NO-LOCK NO-ERROR.
      IF AVAIL(person) THEN
      DO:
         ASSIGN 
            mFName:SCREEN-VALUE = person.name-last
            mName :SCREEN-VALUE = ENTRY(1,person.first-names," ")
            mSName:SCREEN-VALUE = SUBSTRING(person.first-names,LENGTH(mName :SCREEN-VALUE) + 2).
         IF ( mInt EQ 1 OR mInt EQ 4) THEN
            mAddr2:SCREEN-VALUE  = GetChckAttrByClnt("�",person.person-id,"���ᐥ�").
         IF (mInt EQ 1 OR mInt EQ 2 OR mInt EQ 4) THEN
         ASSIGN
            mBDay:SCREEN-VALUE   = STRING(person.birthday,"99/99/9999")
            mBPlace:SCREEN-VALUE = GetChckAttrByClnt("�",person.person-id,"���⮐���")
            mSNum:SCREEN-VALUE   = person.document
            mSNum:SCREEN-VALUE   = person.document
            mKodP:SCREEN-VALUE   = GetChckAttrByClnt("�",person.person-id,"�������")
            mVydan:SCREEN-VALUE  = TRIM(GetChckAttrByClnt("�",person.person-id,"����뤠����") + " " +
                                   GetChckAttrByClnt("�",person.person-id,"��⠂뤠�"))
            mAddr1:SCREEN-VALUE  = GetChckAttrByClnt("�",person.person-id,"���ᐥ�")
            
            mAddr3:SCREEN-VALUE  = GetChckAttrByClnt("�",person.person-id,"���ᔠ��")
            mTelNum:SCREEN-VALUE = TRIM(person.phone[1],",") + " " + TRIM(person.phone[2],",") + " " + 
                                   GetChckAttrByClnt("�",person.person-id,"������").
         APPLY "ENTRY" TO mFName.
         RETURN NO-APPLY.
      END.
   END.
END.

/*ON LEAVE OF mFName                            */
/*DO:                                           */
/*   IF mFName:SCREEN-VALUE EQ ""               */
/*   AND LAST-KEY NE 301                        */
/*   AND LAST-KEY NE 501                        */
/*   AND LAST-KEY NE 509                        */
/*   THEN                                       */
/*   DO:                                        */
/*      MESSAGE "������� ������ ���� ���������."*/
/*      VIEW-AS ALERT-BOX.                      */
/*      RETURN NO-APPLY.                        */
/*   END.                                       */
/*END.                                          */

ON LEAVE OF mName
DO:          
   IF mName:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "��� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mSName
DO:
   
   IF mSName:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "����⢮ ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mBDay
DO:
   IF DATE(mBDay:SCREEN-VALUE) EQ ? 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "��� ஦����� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mBPlace
DO:
   IF mBPlace:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "���� ஦����� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mSNum
DO:
   IF mSNum:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "���� � ����� ��ᯮ�� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mKodP
DO:
   IF mKodP:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "��� ���ࠧ������� ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mVydan
DO:
   IF mVydan:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "���� �뤠� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mAddr1
DO:
   IF mAddr1:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "���� �ய�᪨ ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mAddr2
DO:
   IF mAddr2:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "���� ��� ����ᯮ����樨 ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mAddr3
DO:
   IF mAddr3:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "���� �஦������ ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mTelNum
DO:
   IF mTelNum:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "����� ⥫�䮭� ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON "GO" OF FRAME frClient
DO:
/*   IF mFName:SCREEN-VALUE EQ ""               */
/*   AND LAST-KEY NE 301                        */
/*   THEN                                       */
/*   DO:                                        */
/*      MESSAGE "������� ������ ���� ���������."*/
/*      VIEW-AS ALERT-BOX.                      */
/*      APPLY "ENTRY" TO mFName.                */
/*      RETURN NO-APPLY.                        */
/*   END.                                       */
   IF mName:SCREEN-VALUE EQ "" 
   THEN
   DO:
      MESSAGE "��� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mName.
      RETURN NO-APPLY.
   END.
   IF mSName:SCREEN-VALUE EQ "" THEN
   DO:
      MESSAGE "����⢮ ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mSName.
      RETURN NO-APPLY.
   END.
   IF DATE(mBDay:SCREEN-VALUE) EQ ?
   AND (mInt EQ 1 OR mInt EQ 2) 
   THEN
   DO:
      MESSAGE "��� ஦����� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mBDay.
      RETURN NO-APPLY.
   END.
   IF mBPlace:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2)
   THEN
   DO:
      MESSAGE "���� ஦����� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mBPlace.
      RETURN NO-APPLY.
   END.
   IF mSNum:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2)
   THEN
   DO:
      MESSAGE "���� � ����� ��ᯮ�� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mSNum.
      RETURN NO-APPLY.
   END.
   IF mKodP:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2)
   THEN
   DO:
      MESSAGE "��� ���ࠧ������� ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mKodP.
      RETURN NO-APPLY.
   END.   
   IF mVydan:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2)
   THEN
   DO:
      MESSAGE "���� �뤠� ������ ���� ���������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mVydan.
      RETURN NO-APPLY.
   END.
   IF mAddr1:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2)  
   THEN
   DO:
      MESSAGE "���� �ய�᪨ ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mAddr1.
      RETURN NO-APPLY.
   END.
   IF mAddr2:SCREEN-VALUE EQ "" 
   AND mInt EQ 1 
   THEN
   DO:
      MESSAGE "���� ��� ����ᯮ����樨 ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mAddr2.
      RETURN NO-APPLY.
   END.
   IF mAddr3:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2) 
   THEN
   DO:
      MESSAGE "���� �஦������ ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mAddr3.
      RETURN NO-APPLY.
   END.
   IF mTelNum:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2)
   THEN
   DO:
      MESSAGE "����� ⥫�䮭� ������ ���� ��������."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mTelNum.
      RETURN NO-APPLY.
   END.
END.   

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frClient.
      RETURN ERROR.
   END.
   
   UPDATE
      mTypeQ
      mFName
      mName
      mSName
      mBDay
      mBPlace
      mSNum
      mKodP
      mVydan
      mAddr1
      mAddr2
      mAddr3
      mTelNum
      mStahNum 
      mNalogNum
   WITH FRAME frClient.

   ASSIGN
      pick-value = STRING(mInt) + CHR(1) +
                   mAcctCr      + CHR(1) +
                   STRING(mAmt) + CHR(1) +
                   mSymbol + CHR(1) +
                   mDetail + CHR(1) + 
                   mFName  + CHR(1) +
                   mName   + CHR(1) +
                   mSName  + CHR(1) +
                   (IF mBDay EQ ? THEN "" ELSE STRING(mBDay,"99/99/9999")) + CHR(1) +
                   mBPlace + CHR(1) +
                   mSNum   + CHR(1) +
                   mVydan  + CHR(1) +
                   mAddr1  + CHR(1) +
                   mAddr2  + CHR(1) +
                   mAddr3  + CHR(1) +
                   mTelNum + CHR(1) +
                   mKodP + CHR(1) +
                   mStahNum + CHR(1) + 
                   mNalogNum.
                      
END.
HIDE FRAME frClient.

{intrface.del}   

RETURN pick-value.

