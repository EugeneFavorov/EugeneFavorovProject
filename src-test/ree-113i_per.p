/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ree-113i_per.p
      Comment: '113-� ����� ॥��� �� �஢������ ������'
   Parameters:
         Uses: 
      Used by:
      Created: 04.12.2013 HERZ (0213758) �������� ���� ��� �ॡ������ 
                                         ���(��� �஢�ન ��)
*/

{globals.i}
{wordwrap.def}
{tmprecid.def}
{intrface.get xclass}
{intrface.get count}
{intrface.get vok}

DEFINE INPUT PARAM iParam AS CHAR NO-UNDO.

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
   "~nBegin").

{ree-113i_per.def}

DEFINE VARIABLE mNumDate AS DATE NO-UNDO.
DEFINE VARIABLE mAcctPer     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBreakReport AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mBreakFlag   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mReeNum   AS INT64 INIT 1 NO-UNDO.
DEFINE VARIABLE mOpReeNum AS CHARACTER      NO-UNDO.
DEFINE VARIABLE mFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBirth    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPayee    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mBegDate  AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate  AS DATE      NO-UNDO.
DEFINE VARIABLE mCnt      AS INT64     NO-UNDO.

/*mBranchMask = "*".*/
mBranchMask = "0111".  /*���᪠�*/
/*mBranchMask = "0501".  /*��ૠ ����*/*/
mUserMask = "*".
mReeNum = 1.

{getdates.i}


mBegDate = beg-date.
mEndDate = end-date.
mFileName = shFilial + "_" + STRING(mBegDate,"99999999") + "_" + STRING(mEndDate,"99999999") + ".csv".

{empty tmprecid}

FOR EACH op WHERE
   op.filial-id EQ shFilial
   AND op.op-date GE mBegDate
   AND op.op-date LE mEndDate 
   NO-LOCK:
   CREATE tmprecid.
   tmprecid.id = RECID(op).
END.

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
   "~nFOR EACH op").

FIND FIRST tmprecid NO-LOCK NO-ERROR.
IF NOT AVAIL tmprecid THEN
DO:
   MESSAGE "�� ��࠭� ���㬥���" VIEW-AS ALERT-BOX.
   RETURN.
END.
   
{tmprecid.def &PREF = "tt-"
              &NGSH = YES}

before:
DO TRANSACTION:
   /*********/
ASSIGN
   mNaznSchKas = FGetSetting("�����犠�","","")
   mScetKomis  = FGetSetting("�113","��⊮���","")
   mCodNVal    = FGetSetting("�����悠�","","")
   mAcctPer    = FGetSetting ("�113","�珥ॢ����","")
   mOpGr15     = FGetSetting ("�113","����15","")
.
mSummKon113 = DEC(FGetSetting("�113","�㬬����113","")) NO-ERROR.
IF    ERROR-STATUS:ERROR 
   OR mSummKon113 EQ 0 THEN
   mSummKon113 = 600000.
mSumm1626 = DECIMAL(FGetSetting("�113","�㬬�1626","")) NO-ERROR.
IF ERROR-STATUS:ERROR 
OR mSumm1626 = 0
THEN mSumm1626 = 600000.

IF NUM-ENTRIES(iParam,";") >= 1 THEN
   ASSIGN
      mParam[1]   = ENTRY(1,iParam,";")
      mBranchMask = mParam[1]
   .
IF NUM-ENTRIES(iParam,";") >= 2 THEN
   ASSIGN
      mParam[2] = ENTRY(2,iParam,";")
      mUserMask = mParam[2]
   .
IF NUM-ENTRIES(iParam,";")   GT 3
   AND TRIM(ENTRY(4,iParam,";")) EQ "��" THEN
   mPrintOpNum = TRUE. 

IF NUM-ENTRIES (iParam, ";") GE 5 AND TRIM (ENTRY (5, iParam, ";")) EQ "��" THEN
    mBreakReport = TRUE.

/* ��࠭�� ��� ���㬥��� */
RUN rid-rest.p (OUTPUT TABLE tt-tmprecid).
{empty tmprecid}

FIND FIRST tt-tmprecid.
FIND FIRST op WHERE RECID(op) EQ tt-tmprecid.id NO-LOCK NO-ERROR.

mNumDate = TODAY.

IF AVAILABLE op THEN
   mNumDate = op.op-date.

/*
/* ����� ���ࠧ������� � ���짮��⥫�� */
PAUSE 0.
   UPDATE 
      mBranchMask
      mUserMask
      mReeNum LABEL "����� ॥���     "
   WITH FRAME enter-cond
      WIDTH 60
      SIDE-LABELS
      CENTERED
      ROW 10
      TITLE "[ �롥�� ���ࠧ������� � ���짮��⥫� ]"
      OVERLAY
   EDITING:
         READKEY.
         IF    FRAME-FIELD EQ "mBranchMask"
           AND LASTKEY = 301 THEN 
         DO:
            RUN browseld.p ("branch",
                            "parent-id",
                            "*",
                            ?,
                            4).
            if keyfunc(lastkey) NE "end-error" then
            DO:
               ASSIGN
                  mBranchMask = pick-value
                  mBranchMask:SCREEN-VALUE = pick-value.
               RELEASE branch.
            END.
         END.
         ELSE IF    FRAME-FIELD EQ "mUserMask"
                AND LASTKEY = 301 THEN 
         DO:
            RUN browseld.p ("_user", /* ����� ��ꥪ�. */
                            "_userid",     /* ���� ��� �।��⠭����. */
                            "*" ,      /* ���᮪ ���祭�� �����. */
                            ?,      /* ���� ��� �����஢��. */
                            6). /* ��ப� �⮡ࠦ���� �३��. */
            if keyfunc(lastkey) NE "end-error" then
            DO:
               ASSIGN
                  mUserMask = pick-value
                  mUserMask:SCREEN-VALUE = pick-value.
               RELEASE _user.
            END.
         END.
         ELSE IF LASTKEY EQ 27 THEN
         DO:
            /* ����⠭���� recid's �� ��� ���㬥��� */
            {empty tmprecid}
            
            FOR EACH tt-tmprecid:
               CREATE tmprecid.
               tmprecid.id = tt-tmprecid.id.
            END.
            HIDE FRAME enter-cond.
            RETURN.
         END.
         ELSE
            APPLY LASTKEY.
   END.
   HIDE FRAME enter-cond.
*/

/* ����⠭���� recid's �� ��� ���㬥��� */

{empty tmprecid}

FOR EACH tt-tmprecid:
   CREATE tmprecid.
   tmprecid.id = tt-tmprecid.id.
END.
 
ASSIGN
   mNameBank  = FGetSetting("����","","")
   mREGN      = FGetSetting("REGN","","")
   mAdres-pch = FGetSetting("����_��","","")
.
IF mBranchMask EQ "*" THEN
   mAdres-kass = mAdres-pch.
ELSE
   mAdres-kass = GetXAttrValueEx("branch",
                                 STRING(mBranchMask),
                                 "����_��",
                                 mAdres-pch).
END. /*before*/

/* ----------------------------------------------------------------------------------------------------------------------------------- */
{setdest.i &filename = mFileName
             &custom = "IF YES THEN 0 ELSE "
             &option = "CONVERT TARGET '1251'" }
             
{justamin}

mBreakFlag = FALSE.

RUN PrintHeader. /* �뢮� ��������� */
ASSIGN
   mDocNum    = 1.

vOpLnk    = getXLinkID ("opbv-svod","opvSvod").

FOR EACH tmprecid NO-LOCK:
   CREATE tt-id.
   tt-id.id = tmprecid.id.
END.

FOR EACH tmprecid NO-LOCK,
   FIRST op WHERE RECID(op) EQ tmprecid.id
              AND CAN-DO(mUserMask,op.user-id)
         NO-LOCK:
   FOR EACH links WHERE
            links.link-id   EQ vOpLnk
        AND links.source-id EQ STRING(op.op)
      NO-LOCK:
      vOp = INT64(links.target-id).
      FIND FIRST dop WHERE
                 dop.op = vOp
         NO-LOCK NO-ERROR.
      IF AVAIL dop THEN
      DO:
         FIND FIRST tt-id WHERE tt-id.id EQ tmprecid.id EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL tt-id
            THEN DELETE tt-id.

         CREATE tt-id.
         tt-id.id = RECID(dop).

         RUN GetDateTimeOpTr(dop.op-transaction,dop.op,OUTPUT mOpTime,OUTPUT mOpDate).
         ASSIGN
            tt-id.dt = mOpDate
            tt-id.tm = mOpTime.
      END.
   END.
END.

FOR EACH tt-id WHERE
   tt-id.tm EQ 0
   NO-LOCK,
   FIRST op WHERE
   RECID(op) EQ tt-id.id
   NO-LOCK:

   mOpTime = INT64(GetXattrValueEx("op",STRING(op.op),"rst-time","?")) NO-ERROR.
   mOpDate = DATE(GetXattrValue("op",STRING(op.op),"rst-date")) NO-ERROR.
   IF mOpTime EQ ? OR mOpDate EQ ? THEN
   RUN GetDateTimeOpTr(op.op-transaction,op.op,OUTPUT mOpTime,OUTPUT mOpDate).
   ASSIGN
      tt-id.dt = mOpDate
      tt-id.tm = mOpTime.
END.

/*
FOR EACH tmprecid NO-LOCK:
message "---" tmprecid.id view-as alert-box.
END.
*/

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
   "~nFOR EACH tt-id = ").

FOR EACH tt-id NO-LOCK,
   FIRST op WHERE 
   RECID(op) EQ tt-id.id
   AND CAN-DO(mUserMask,op.user-id)
	/*AND op.op EQ 132166357*/
	/*   (   op.op EQ 132246504     */
	/*        OR op.op EQ 132234738 */
	/*        OR op.op EQ 132250707)*/
   NO-LOCK,
   EACH op-entry OF op NO-LOCK 
   BREAK BY tt-id.dt BY tt-id.tm: /*BY op.op:*/

/*    RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",*/
/*      "~nop.op    = " + STRING(op.op)).                       */

   IF op.doc-date LT DATE("01/07/2016") THEN 
   ASSIGN
      mSummKon113 = 15000
      mSumm1626   = 15000.
   IF op.doc-date GE DATE("01/07/2016") THEN 
   ASSIGN
      mSummKon113 = 40000
      mSumm1626   = 40000.
      
/*----------------------------------------------- �롮ઠ ----------------------------------------------------------------------*/
   mOpReeNum = GetXattrValueEx ("op",
                                STRING (op.op),
                                "�����������",
                                "").

 /*  IF mOpReeNum NE "" AND */        /* ���㬥�� 㦥 ����祭 � ��㣮� ॥��� */
   /*   mOpReeNum NE STRING (mReeNum) THEN
      NEXT. */

   {ree-113i_per.sel}

   IF NOT (   CAN-DO(mBranchMask,acct-cr.branch-id)
           OR CAN-DO(mBranchMask,acct-db.branch-id)
           ) THEN NEXT.

   IF CAN-DO(mScetKomis,op-entry.acct-cr) OR 
      CAN-DO(mScetKomis,op-entry.acct-db) THEN
      NEXT.

   IF CAN-DO("70*",op-entry.acct-cr) OR 
      CAN-DO("70*",op-entry.acct-db) THEN
      NEXT.
      
/* ����祭�� ������
������ ��ப� ���� ᮤ�ন� ���ଠ�� �� �⤥�쭮� ���㬥�� � (����)�஢�����, 
   �易���� � ���. �᫨ � ���㬥�� ���� ��� ����஢����, � ������������� ���. 
   �᫨ �� ����� ����, � ������������� ���� �஢����, � ������ ��������� ���� 
   ����� � �।��.
*/

   ASSIGN
      m5 = ""
      m6 = 0
      m7 = ""
      m8 = 0
      m10 = 0
      m11 = ""
      m12 = 0
      m13 = ""
      m15 = ""
      mFIO = ""
      m17 = ""
      mADR = ""
      mDocId = ""
   .
   IF(    CAN-DO(mNaznSchKas, acct-db.contract)
      AND NOT CAN-DO("20203*",acct-db.acct)) THEN
   DO:
      IF(acct-db.currency EQ "") THEN 
         ASSIGN m5 = mCodNVal
                m6 = op-entry1.amt-rub
         .
      ELSE
         ASSIGN m5 = acct-db.currency
                m6 = op-entry1.amt-cur
         .
   END.
   IF(    CAN-DO(mNaznSchKas, acct-cr.contract)
      AND NOT CAN-DO("20203*",acct-cr.acct)) THEN 
   DO:
      IF(acct-cr.currency EQ "") THEN 
         ASSIGN m7 = mCodNVal
                m8 = op-entry2.amt-rub
         .
      ELSE
         ASSIGN m7 = acct-cr.currency
                m8 = op-entry2.amt-cur
         .
   END.
   IF (acct-db.acct BEGINS "20203") THEN
   DO:
      m10 = op-entry1.qty.
      IF(acct-db.currency EQ "") THEN 
         ASSIGN m11 = mCodNVal
                m12 = op-entry1.amt-rub
         .
      ELSE
         ASSIGN m11 = acct-db.currency
                m12 = op-entry1.amt-cur
         .
   END.
   IF (acct-cr.acct BEGINS "20203") THEN
   DO:
      m10 = op-entry2.qty.
      IF(acct-cr.currency EQ "") THEN 
         ASSIGN m11 = mCodNVal
                m12 = op-entry2.amt-rub
         .
      ELSE
         ASSIGN m11 = acct-cr.currency
                m12 = op-entry2.amt-cur
         .
   END.

   IF     NOT CAN-DO(mNaznSchKas, acct-cr.contract)
      AND acct-cr.cust-cat EQ "�" THEN
      m13  = acct-cr.acct.
   ELSE IF     NOT CAN-DO(mNaznSchKas, acct-db.contract) 
           AND acct-db.cust-cat EQ "�" THEN
      m13 = acct-db.acct.

   IF CAN-DO (mAcctPer, m13) THEN
      m13 = "".

   IF acct-db.cust-cat EQ "�" THEN
      FIND FIRST person WHERE person.person-id EQ acct-db.cust-id NO-LOCK NO-ERROR.
   ELSE IF acct-cr.cust-cat EQ "�" THEN
      FIND FIRST person WHERE person.person-id EQ acct-cr.cust-id NO-LOCK NO-ERROR.

   IF NOT AVAIL(person) THEN 
   DO:
      mPayee = GetXAttrValueEx("op",STRING(op-entry.op),"���⥫�騪","").
      IF NUM-ENTRIES(mPayee) GT 1 THEN
         mPayee = ENTRY(2,mPayee).
      IF mPayee NE "" THEN
      DO:
         FIND FIRST person WHERE
            person.person-id = INT64(mPayee)
         NO-LOCK NO-ERROR.
      END.
   END.

   m15 = "".
   IF op-entry.amt-rub >= mSumm1626
   OR (    op-entry.amt-rub < mSumm1626
       AND
       NOT CAN-DO(mOpGr15,mVidOpNalV))
   THEN DO:
      IF AVAILABLE person THEN
         m15 = person.country-id.
      ELSE
      DO:
         m15 = GetXAttrValueEx("op",
                               STRING(op-entry.op),
                               "country-rec",
                               "").
         IF m15 EQ "" THEN
            m15 = GetXAttrValueEx("op",
                                  STRING(op-entry.op),
                                  "country-send",
                                  "").
      
         IF m15 EQ "" THEN
            m15 = GetXAttrValueEx("op",
                                  STRING(op-entry.op),
                                  "country-pers",
                                  "").
      END.

      IF m15 EQ "999" OR m15 EQ "nnn" THEN m15 = "".
   END.

   ASSIGN
      mFIO = ""
      m17  = ""
      mDocId  = ""
      mADR = ""
   .
   
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
      "~nop.op             = " + STRING(op.op) +
      "~nAVAILABLE(person) = " + STRING(AVAILABLE(person)) + 
      "~nm15               = " + m15).
   
   IF /*TRUE*/ 
   m15 NE "RUS"  /*��� ��१��*/
   OR CAN-DO("55,57,60,63",mVidOpNalV)   
   OR (op-entry.amt-rub >= mSummKon113
   OR (op-entry.amt-rub  < mSummKon113 AND op-entry.amt-rub >= mSumm1626))
     /*  AND GetXattrValueEx("op",STRING(op.op),"���113","���") = "��") */
   THEN
   DO:        
      IF AVAILABLE(person) THEN
      DO:
         ASSIGN
            mFIO    = person.name-last + " " + person.first-names
            m17     = person.document + ", " + 
                      person.issue    + ", " +
                      GetXAttrValueEx("person",STRING (person.person-id),"Document4Date_vid", "")
            mDocId  = person.document-id
            mADR    = person.address[1]  + " " + person.address[2]
            mBirth  = TRIM(STRING(person.birthday) + " " + GetXAttrValueEx("person",STRING(person.person-id),"BirthPlace","")) 
         .
      END.
      ELSE
      DO:
         mFIO = GetXAttrValueEx("op",
                                  STRING(op-entry.op),
                                  "���",
                                  mFIO).
         mDocId = GetXAttrValueEx("op",
                                  STRING(op.op),
                                  "document-id",
                                   mDocId).
         mTmpStr = GetXAttrValueEx("op",
                                  STRING(op-entry.op),
                                  "����",
                                  "").
         IF mTmpStr NE "" THEN
         DO:
            m17 = mTmpStr                                                             + ", " +
                  GetXAttrValueEx("op", STRING(op-entry.op), "cust-doc-who",      "") + ", " +
                  GetXAttrValueEx("op", STRING(op-entry.op), "Document4Date_vid", "").
         END.
         mADR = GetXAttrValueEx("op",
                                  STRING(op-entry.op),
                                  "����",
                                  mADR).
        mBirth  = GetXAttrValue("op",STRING(op-entry.op),"birthday") + "  " + GetXAttrValueEx("op",STRING(op-entry.op),"birthPlace","").
     END.
   END.

   IF NOT AVAIL person
   THEN DO:   
      mFIO         = GetXAttrValueEx("op", STRING(op.op), "���","").
      mADR         = GetXAttrValueEx("op", STRING(op.op), "����","").
      m17          = GetXAttrValueEx("op", STRING(op.op), "����","") + ", " +
                     GetXAttrValueEx("op", STRING(op.op), "cust-doc-who","") + ", " +
                     GetXAttrValueEx("op", STRING(op.op), "Document4Date_vid","").
   END.
   
   IF AVAILABLE person THEN
      RELEASE person.

   IF  m15 NE "" THEN
   DO:
      FIND FIRST country WHERE country.country-id EQ m15 NO-LOCK NO-ERROR.

      IF AVAIL country THEN
         m15 = STRING(country.country-alt-id).
   END.

   msprate    = DECIMAL(GetXAttrValueEx("op",
                                STRING(op-entry.op),
                                "sprate",
                                "0")) NO-ERROR.
   mDover     = GetXAttrValueEx("op",
                                STRING(op-entry.op),
                                "�����",
                                "���").
   mDover = IF mDover NE "���"
            THEN "X"
            ELSE " ".
	    
   m9 = IF mVidOpNalV EQ "16" OR mVidOpNalV EQ "17"
           THEN "X"
           ELSE " ".	    

   ASSIGN
      mOpTime = tt-id.tm
      mOpDate = tt-id.dt.
   
   IF m17 BEGINS("��ᯮ��") 
   THEN m17 = "".
   ELSE m17 = mDocId + " " + m17. 

   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nmFIO = " + mFIO + 
         "~nm17 = " + m17).

   IF mBreakReport THEN
   DO:
      /*  �஢����, �㦭� �� ࠧ������  */
      FIND FIRST ttBreakInfo WHERE (m5 NE "" AND m7 NE "" AND ttBreakInfo.m5 EQ m5  AND ttBreakInfo.m7 EQ m7) 
                               OR    (m5 NE "" AND m7 EQ "" AND ttBreakInfo.m5 EQ m5  AND ttBreakInfo.m7 EQ m11)
                               OR    (m5 EQ "" AND m7 NE "" AND ttBreakInfo.m5 EQ m11 AND ttBreakInfo.m7 EQ m7)
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ttBreakInfo THEN
        DO:
            CREATE ttBreakInfo.
            ASSIGN
                 ttBreakInfo.m5     = IF m5 NE "" 
                                        THEN m5
                                        ELSE m11
                 ttBreakInfo.m7     = IF m7 NE ""
                                        THEN m7
                                        ELSE m11
                 ttBreakInfo.sprate = msprate
            .
        END.
        ELSE
            mBreakFlag = msprate NE ttBreakInfo.sprate.
   END.

   IF mBreakFlag THEN       /* ����� ���� ॥��� */
   DO:         
       RUN PrintTotals&Footer. /* �뢮� �⮣�� */
                               /* ���᫥��� ������ ����� ॥���*/
       ASSIGN
         mReeNum = mReeNum + 1
         mDocNum = 1.

       RUN PrintHeader.     /* �뢮� ��������� */

       {empty ttBreakInfo}
       CREATE ttBreakInfo.
       ASSIGN
            ttBreakInfo.m5     = IF m5 NE "" 
                                   THEN m5
                                   ELSE m11
            ttBreakInfo.m7     = IF m7 NE ""
                                   THEN m7
                                   ELSE m11
            ttBreakInfo.sprate = msprate
       .
       {empty tt-itog}
       mBreakFlag = FALSE.
   END.

   RUN put-itog(mVidOpNalV).
   RUN put-itog("**").

   /*Birthday BirthPlace document-id ���� cust-doc-who ��� ���� Passport 037 250001*/

   PUT UNFORM 
      STRING(mOpDate, "99/99/9999")  ";" 
      /* REPLACE(STRING(mOpTime,"HH:MM"),":",".")  ";"      */
      STRING(mOpTime,"HH:MM") ";"      
      '="' mVidOpNalV                   '";' 
      IF msprate GT 0 THEN STRING(msprate)
                      ELSE "" 
                                         ";"
      '="' m5                           '";'
      STRING(m6)                         ";"
      '="' m7                           '";'
      STRING(m8)                         ";"
      '="' m9                           '";'
      STRING(m10)                        ";"
      '="' m11                          '";'
      STRING(m12)                        ";"
      '="' ENTRY(1, m13, " ")           '";'
      '="' mDover                       '";'
      '="' m15                          '";'
      TRIM(REPLACE(REPLACE(REPLACE(mFIO,";",","),CHR(10),""),CHR(13),"")) ";"
      TRIM(REPLACE(REPLACE(REPLACE(m17,";",","), CHR(10),""),CHR(13),"")) ";"
      TRIM(REPLACE(REPLACE(REPLACE(mADR,";",","),CHR(10),""),CHR(13),"")) ";"
      TRIM(REPLACE(REPLACE(REPLACE(mBirth,";",","),CHR(10),""),CHR(13),"")) ";"
      
      SKIP.

/*
1    - ����� ���㬥�� op.doc-num
2	- �६� ����� ���㬥�� � ��⥬�.
3	- �������⥫�� ४����� ���㬥�� "���������".
4	- ���祭�� �������⥫쭮�� ४����� "sprate".
5	- ��� ����砥��� ������. ��� ������ �� �஢���� ��� ����஢����, � ���ன ���
       � �����祭��� �� ����஥筮�� ��ࠬ���"�����犠�" �⮨� �� ������,
       �� �᪫�祭��� ��⮢ 20203*.
6	- �㬬� ����砥��� ������. �㬬� � ����� ���� 5 �� �஢���� ��� ����஢����, 
       � ���ன ��� � �����祭��� �� ����஥筮�� ��ࠬ���"�����犠�" �⮨� �� ������,
       �� �᪫�祭��� ��⮢ 20203*.
7	- ��� �뤠������ ������. ��� ������ �� �஢���� ��� ����஢����, � ���ன ���
       � �����祭��� �� ����஥筮�� ��ࠬ���"�����犠�" �⮨� �� �।���,
       �� �᪫�祭��� ��⮢ 20203*.
8	- �㬬� �뤠������ ������. �㬬� � ����� ���� 7 �� �஢���� ��� ����஢����,
       � ���ன ��� � �����祭��� �� ����஥筮�� ��ࠬ���"�����犠�" �⮨� �� �।���,
       �� �᪫�祭��� ��⮢ 20203*.
9	- ���� �� ����������.
10	- ������⢮ ���� op-entry.qty �� �஢���� � ���ன ��� 20203* �⮨� �� ������
       ��� �� �।���.
11	- ��� ������ �� �஢���� � ���ன ��� 20203* �⮨� �� ������ ��� �� �।���.
12	- �㬬� � ����� �� ���� 11 �஢���� � ���ன ��� 20203* �⮨� �� ������ ��� �� �।���.
13	- �᫨ ��� ����� ����ᯮ������ � ���㬥�� � ��⮬, ����� �� ���� ��⮬ �����,
       � �뢮����� ��� ����ᯮ������騩 ���. �᫨ ��� ��� ����� ��⠬� �����,
       � � ���� ��祣� �� �뢮�����.
14	- ���祭�� �������⥫쭮�� ४����� "�����".
15	- �᫨ ��� ����� ����ᯮ������ � ���㬥�� � ��⮬, �����⮬ ���ண� ���� 
       䨧��᪮� ���, � �� ���� person.country-id ������, �易����� � ��⮬.
       ���� - ���祭�� ������������ �������⥫쭮�� ४����� "country-rec" ��� "country-send".
16	- �᫨ ��� ����� ����ᯮ������ � ���㬥�� � ��⮬, �����⮬ ���ண� ���� 
       䨧��᪮� ���, � �� ��� ������� ������. ���� ���祭�� �������⥫쭮�� ४����� "���".
17	- �᫨ ��� ����� ����ᯮ������ � ���㬥�� � ��⮬, �����⮬ ���ண� ����
       䨧��᪮� ���, � �� ⨯ � ����� ���㬥�� ������� ������.
       ���� ���祭�� �������⥫쭮�� ४����� "����".
18	- �᫨ ��� ����� ����ᯮ������ � ���㬥�� � ��⮬, �����⮬ ���ண� ����
       䨧��᪮� ���, � �� ���� ������� ������.
       ���� ���祭�� �������⥫쭮�� ४����� "����".
*/

   mDocNum = mDocNum + 1.
END.

/* ��᫥���� �⮣� */
RUN PrintTotals&Footer.
MESSAGE "����� ����������� � ������� ��������." VIEW-AS ALERT-BOX.

/* {preview.i &filename = "'ree-113i_per.csv'" } */

/* ---------------------------------------------------------------------------------------------------------------------------------- */
PROCEDURE PrintHeader:

   PUT UNFORM
      "������������ 㯮�����祭���� ����� " TRIM(mNameBank) " (䨫���� 㯮�����祭���� �����)" SKIP
      "�������樮��� ����� 㯮�����祭���� ����� " TRIM(mREGN)                               SKIP
      "/���浪��� ����� 䨫���� 㯮�����祭���� �����"                                        SKIP
      "���� 㯮�����祭���� ����� " mAdres-pch                                                SKIP
      "(䨫���� 㯮�����祭���� �����)"                                                        SKIP
      "���� ����樮���� ����� " mAdres-kass                                                  SKIP
      "��� ���������� ������;" STRING(TODAY,"99/99/9999")                                    SKIP
      ";���� ����� ��� "                                                                       SKIP
      "���浪��� ����� ������ � �祭�� ࠡ�祣� ��� ����樮���� ����� " STRING(mReeNum)    SKIP(1)
      "������ �������� � �������� ������� � ������"                                            SKIP(1)
      "���;�६�;���;����;������ ������� �।�⢠;;;;���-;�ਭ�� (�뤠��) ���ᮢ� ࠡ�⭨-;;;����� ���;����-;���;�.�.�. 䨧��᪮��;���㬥��, 㤮�⮢����騩 ��筮���;���� ���� ��⥫��⢠;��� � ����" SKIP
      ";ᮢ��-;����;(����-����);�ਭ��;;�뤠��;;⥦-;��� 祪�� (� ⮬ �᫥ ��஦���;;;;७-;��࠭�;���;;;஦����� 䨧�-" SKIP
      ";襭��;���-;�����-;���ᮢ� ࠡ�⭨���;;���ᮢ� ࠡ�⭨���;;���;祪��), �������쭠��⮨����� ��-;;;;�����;�ࠦ-;;;;�᪮�� ���" SKIP
      ";����-;�樨;࠭���;���;�㬬�;���;�㬬�;���-;���� 㪠���� � �����࠭��� �����;;;;;����⢠;;;;" SKIP
      ";樨;;������;����-;;����-;;�;����-;���;�㬬�;;;䨧�-;;;;" SKIP
      ";;;;��;;��;;;��⢮;����-;;;;�᪮��;;;" SKIP
      ";;;;;;;;;祪��;��;;;;���;;;;" SKIP
      "1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19" SKIP
   .

END PROCEDURE.


PROCEDURE PrintTotals&Footer:
    
   FOR EACH tt-itog WHERE tt-itog.vidop NE "**":

      PUT UNFORM
         "�����;"
         tt-itog.vidop              ";"
                                    ";"
         tt-itog.val5               ";"
         TRIM(STRING(tt-itog.sum6)) ";"
         tt-itog.val7               ";"
         TRIM(STRING(tt-itog.sum8)) ";"
                                    ";"
                                    ";"
         tt-itog.val11              ";"
         STRING(tt-itog.sum12)      ";"
                                    ";"
                                    ";"
                                    ";"
                                    ";"
                                    ";"
                                    ";"
      SKIP.

   END. /* FOR EACH tt-itog WHERE tt-itog.vidop NE "**": */

   PUT UNFORM SKIP(1).

   IF     NUM-ENTRIES(iParam,";")   GT 2 
      AND TRIM(ENTRY(3,iParam,";")) EQ "��"
   THEN 
   DO:

      PUT UNFORM
         "������� �����"                                 SKIP
         "1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18" SKIP
      .

      FOR EACH tt-itog WHERE tt-itog.vidop EQ "**":
         PUT UNFORM
            "�����"                    ";"
            "��"                      ";"
                                       ";"
            tt-itog.val5               ";"
            TRIM(STRING(tt-itog.sum6)) ";"
            tt-itog.val7               ";"
            TRIM(STRING(tt-itog.sum8)) ";"
                                       ";"
                                       ";"
            tt-itog.val11              ";"
            STRING(tt-itog.sum12)      ";"
                                       ";"
                                       ";"
                                       ";"
                                       ";"
                                       ";"
                                       ";"
         SKIP.

      END. /* FOR EACH tt-itog WHERE tt-itog.vidop EQ "**": */

      PUT UNFORM SKIP(1).

   END. /* IF */

   {signatur.i &user-only=yes}

END PROCEDURE.

PROCEDURE put-itog:

   DEFINE INPUT PARAMETER iVidOpNalV AS CHAR.
   
   FIND FIRST tt-itog WHERE tt-itog.vidop EQ iVidOpNalV
                        AND tt-itog.val5  EQ m5
                        AND tt-itog.val7  EQ m7
                        AND tt-itog.val11 EQ m11 NO-ERROR.
   IF NOT AVAIL tt-itog THEN
   DO:
      CREATE tt-itog.
      ASSIGN
         tt-itog.vidop = iVidOpNalV
         tt-itog.val5  = m5
         tt-itog.val7  = m7
         tt-itog.val11 = m11
      .
   END.
   ASSIGN 
      tt-itog.sum6 = tt-itog.sum6 + m6
      tt-itog.sum8 = tt-itog.sum8 + m8
      tt-itog.sum12 = tt-itog.sum12 + m12
   . 
   RELEASE tt-itog.
END PROCEDURE.
  
