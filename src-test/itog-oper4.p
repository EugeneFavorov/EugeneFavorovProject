{globals.i}
{sh-defs.i}
{dpsproc.def}
{intrface.get dps}
{intrface.get dpspr}
{intrface.get rights}
{intrface.get tmess}  /* ��㦡� ��⥬��� ᮮ�饭�� */
/* ��� ������⢥����� �롮� �� �ࠢ�筨��� �� F1*/
{ttretval.def}

/* ������砥� ttnames */
{prn-doc.def &with_proc=YES}


message "work-module �� " work-module    view-as alert-box.


work-module = "base". 


message "work-module ��᫥" work-module    view-as alert-box.

DEFINE VARIABLE mFilial       AS CHARACTER   NO-UNDO. /* ����� 䨫���� */
DEFINE VARIABLE mDate         AS DATE        NO-UNDO. /* ���⭠� ��� */
DEFINE VARIABLE mCur          AS CHARACTER   NO-UNDO. /* ��� ������ */
DEFINE VARIABLE mSvodAcct     AS CHARACTER   NO-UNDO. /* ������ ��� */
DEFINE VARIABLE mIsOpen       AS INTEGER     NO-UNDO. /* ����� �� ���*/
DEFINE VARIABLE mOstVal       AS DECIMAL     NO-UNDO. /* ���⮪ � ����� */
DEFINE VARIABLE mOstRub       AS DECIMAL     NO-UNDO. /* ���⪮� � �� */
DEFINE VARIABLE mLegFil       AS CHARACTER   NO-UNDO. /* ���᮪ ����㯭�� 䨫����� */
                                            
DEFINE VARIABLE mTmpPart      AS DECIMAL     NO-UNDO. /* ��⭮� ��饩 �㬬� � ⥪�饣� �. ��. */
DEFINE VARIABLE mTotalSummRub AS DECIMAL     NO-UNDO. /* ���� �㬬� */
DEFINE VARIABLE mTotalSummVal AS DECIMAL     NO-UNDO. /* ���� �㬬� */
DEFINE VARIABLE mI            AS int64       NO-UNDO. /* bal-acct */
DEFINE VARIABLE mCount        AS INT64       NO-UNDO. /* ���稪 ��� ttOut */
DEFINE VARIABLE mCloseCount   AS INT64       NO-UNDO.
DEFINE VARIABLE mOpenCount    AS INT64       NO-UNDO.
DEFINE VARIABLE mGroup        AS CHARACTER   NO-UNDO. /* ��㯯� �� �ࠢ�筨�� */
DEFINE VARIABLE mGroupName    AS CHARACTER   NO-UNDO. /* ������������ ��㯯� �� �ࠢ�筨�� */
DEFINE VARIABLE mEntry        AS CHARACTER   NO-UNDO.

DEFINE STREAM mStream.

DEFINE BUFFER acct      FOR acct.      /* ���������� ���� */ 
DEFINE BUFFER loan      FOR loan.      /* ���������� ���� */
DEFINE BUFFER loan-acct FOR loan-acct. /* ���������� ���� */

/* ����� ��ࠬ��஢ ���� */
ASSIGN 
   mDate   = gend-date
   mFilial = shfilial
   .
PAUSE 0.
DO 
ON ERROR  UNDO, RETURN 
ON ENDKEY UNDO, RETURN
WITH FRAME dateframe2:
   UPDATE
      mDate  LABEL "���� ��"
             HELP  "F1 - ���������"
      mFilial LABEL "��� 䨫����"
             HELP  "F1 - �ࠢ�筨� 䨫�����"
      mCur   LABEL "��� ������"
             HELP  "F1 - �ࠢ�筨� �����"
      mGroup LABEL "��㯯� ����⮢"
             HELP "F1 - �ࠢ�筨� ᢮���� ��⮢"
   WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
      COLOR messages TITLE "[ ��ࠬ���� ���� ]"
   EDITING:
      READKEY.
      IF LASTKEY EQ 301 AND FRAME-FIELD EQ "mDate"
      THEN DO:
         RUN calend.p.
         IF (LASTKEY EQ 13 OR
             LASTKEY EQ 10) AND
             pick-value NE ? THEN
         DO:
            mDate = DATE(pick-value).
            DISPLAY mDate.      
         END.
      END.
      ELSE
      IF LASTKEY EQ 301 AND FRAME-FIELD EQ "mFilial" THEN
      DO:
         DO TRANSACTION:
         RUN browseld.p('branch',
                        'isbank' + '~001' + 'branch-type' + '~001' + 'RetFld',
                        'YES'    + '~001' + '10,11'       + '~001' + 'Branch-Id',
                         '',
                         4).
         END.
         IF pick-value NE '' AND pick-value NE '?' THEN 
         DO:
            mFilial = pick-value.   
            DISPLAY mFilial.      
         END.
      END.
      ELSE 
      IF LASTKEY EQ 301 AND FRAME-FIELD EQ "mCur" THEN
      DO:
         DO TRANSACTION:
            RUN browseld.p ("currency", 
                            "currency" ,     
                             "*",      
                             "",     
                              1).
            IF pick-value NE '?' THEN 
            DO:
               mCur = pick-value.   
               DISPLAY mCur.      
            END.
         END.
      END.
      ELSE
      IF LASTKEY EQ 301 AND FRAME-FIELD EQ "mGroup" THEN
      DO:
         DO TRANSACTION:
            RUN browseld.p('code',
                           'class'         + '~001' + 'parent'        + '~001' + 'RetFld',
                           '����⠃�㯯�' + '~001' + '����⠃�㯯�' + '~001' + 'val',
                            '',
                            4).
         END.
         IF pick-value NE '' AND pick-value NE '?' THEN 
         DO:
            mGroup = pick-value.   
            DISPLAY mGroup.      
         END.
      END.
      ELSE APPLY LASTKEY .
   END.
END.

DEFINE TEMP-TABLE ttList
   FIELD fList AS CHARACTER
   FIELD fCust AS CHARACTER
.
DEFINE TEMP-TABLE ttAcct
   FIELD fAcct AS CHARACTER
   FIELD fCust AS CHARACTER
.
DEFINE TEMP-TABLE ttNoAcct
   FIELD fAcct AS CHARACTER
.
DEFINE TEMP-TABLE ttResp
   FIELD acct1    AS CHARACTER              /* ����� ��� */
   FIELD acct     AS CHARACTER              /* ������ ��� */
   FIELD OstVal   AS DECIMAL                /* ���줮 � ����� */
   FIELD OstRub   AS DECIMAL                /* ���줮 � �㡫�� */
   FIELD IsOpen   AS INTEGER                /* ������⢮ ������� */
   FIELD AcctName AS CHARACTER              /* �������� */
   FIELD ComRate  AS DECIMAL FORMAT ">9.99" /* �।������襭��� % �⠢�� */
   FIELD percent  AS DECIMAL FORMAT ">9.99" /* ���� */
   /*************** ���� ��� �����饭�� ***************/
   FIELD SvDog    AS CHARACTER              /* ���祭�� �� ����� */
   FIELD tmpRate AS DECIMAL                 /* �࠭��� �ந�������� �㬬� �� �⠢�� */
   .

DEFINE TEMP-TABLE ttOut
   FIELD acct     AS CHARACTER              /* ������ ��� */
   FIELD OstVal   AS DECIMAL                /* ���줮 � ����� */
   FIELD OstRub   AS DECIMAL                /* ���줮 � �㡫�� */
   FIELD OpenNum  AS INTEGER                /* ������⢮ ������� */
   FIELD CloseNum AS INTEGER                /* ������⢮ �������� */
   FIELD AcctName AS CHARACTER              /* �������� */
   FIELD ComRate  AS DECIMAL FORMAT ">9.99" /* �।������襭��� % �⠢�� */
   FIELD percent  AS DECIMAL FORMAT ">9.99" /* ���� */
.

/* ������ ��⮢ � �訡���� */
DEFINE TEMP-TABLE ttError
   FIELD acct AS CHARACTER 
.

FIND _user WHERE _user._userid EQ USERID("bisquit")
   NO-LOCK NO-ERROR.
mLegFil = GetAllUsrFilial (_user._Userid).
IF NOT CAN-DO(mLegFil, mFilial) AND shModeMulty THEN
DO:
   RUN fill-sysmess IN h_tmess ("","","-1", "���짮��⥫� " + _user._Userid + " �� ����� ����㯠 � 䨫���� " + mFilial).
   RETURN.
END.

IF (mGroup EQ "" OR mGroup EQ ?) THEN
DO:
   RUN Fill-SysMess IN h_tmess ("","","-1", "�� ��࠭�� ��㯯� �/�").
   RETURN.
END.

/* ���㫨� ���稪� */
ASSIGN
   mIsOpen  = 0
.

FIND FIRST code WHERE
           code.class EQ "����⠃�㯯�"
       AND code.code  EQ mGroup
   NO-LOCK NO-ERROR.

IF AVAIL code THEN
   mGroupName = code.name.

FOR EACH bal-acct WHERE  
   CAN-DO(code.description[1], STRING(bal-acct.bal-acct))
   NO-LOCK:

   CREATE ttList.
   ASSIGN
      ttList.fList = string(bal-acct.bal-acct)
      ttList.fCust = code.description[3].
END.

IF code.description[2] NE "" THEN
DO mI = 1 TO NUM-ENTRIES(code.description[2]):

   IF ENTRY(mI, code.description[2]) BEGINS "!" THEN
   DO:
      mEntry = SUBSTRING(ENTRY(mI, code.description[2]), 2).
      IF LENGTH(mEntry) NE 20 THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","�訡�� ����� �/� � �����䨪���!").
         RETURN.
      END.
      CREATE ttNoAcct.
      ASSIGN
         ttNoAcct.fAcct = mEntry.
   END.
   ELSE DO:
      mEntry = ENTRY(mI, code.description[2]).
      IF LENGTH(mEntry) NE 20 THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","�訡�� ����� �/� � �����䨪���: " + mEntry + "!").
         RETURN.
      END.
      CREATE ttAcct.
      ASSIGN
         ttAcct.fAcct = mEntry
         ttAcct.fCust = code.description[3].
   END.
END.

FOR EACH ttList 
   NO-LOCK:
   
   FOR EACH acct WHERE
            acct.filial-id   EQ mFilial
        AND acct.bal-acct    EQ int64(ttList.fList)
        AND (IF mCur NE "*" THEN acct.currency EQ mCur ELSE TRUE)
        AND acct.open-date   LE mDate
        AND (IF ttAcct.fCust NE "" THEN CAN-DO(ttAcct.fCust,acct.cust-cat)  ELSE TRUE)
      NO-LOCK:

      IF CAN-FIND(FIRST ttNoAcct WHERE 
                        ttNoAcct.fAcct EQ DelFilFromAcct(acct.acct)) THEN
         NEXT.

      RUN acct-pos IN h_base (acct.acct,
                              acct.currency,
                              mDate,
                              mDate,
                              CHR(251)).
      IF acct.close-date EQ ?
         OR acct.close-date LT mDate 
         THEN mIsOpen = 1.
      ELSE mIsOpen = 0.
     
      mSvodAcct = GetXattrValue ("acct", acct.acct + "," + acct.currency, "�����").
   
      /* ������ �������� ᢮�. ��� �� �����䨪��� */
      FIND FIRST code WHERE
                 code.class  EQ "�����"
             AND code.code   EQ STRING(mSvodAcct)
         NO-LOCK NO-ERROR.
      
      /* ������塞 ���ଠ�� �� ��⠬ */
      CREATE ttResp.
      ASSIGN
         ttResp.acct1    = acct.acct
         ttResp.acct     = mSvodAcct
         ttResp.OstVal   = IF acct.currency EQ "" THEN abs(sh-bal)
                                                  ELSE abs(sh-val)
         ttResp.OstRub   = abs(sh-bal)
         ttResp.IsOpen   = mIsOpen
         ttResp.AcctName = IF AVAIL code THEN code.name ELSE "��� ��� � �����䨪���"
         ttResp.SvDog    = GetXattrValueEx ("acct", acct.acct + "," + acct.currency, "�����", "")
      .
      RELEASE loan.
      RELEASE loan-acct.
   END.
END.

FOR EACH ttAcct 
   NO-LOCK:
   FIND FIRST acct WHERE  
                 acct.acct     EQ AddFilToAcct(ttAcct.fAcct, mFilial)
             AND (IF mCur NE "*" THEN acct.currency EQ mCur ELSE TRUE)
         NO-LOCK NO-ERROR.
   mSvodAcct = GetXattrValue("acct",acct.acct + "," + acct.currency, "�����").
   IF NOT AVAIL acct THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","1","�� ������ ���" + AddFilToAcct(ttAcct.fAcct, mFilial)).
      NEXT.
   END.

   FIND FIRST code WHERE
                 code.class  EQ "�����"
             AND code.code   EQ STRING(mSvodAcct)
         NO-LOCK NO-ERROR.
   IF acct.close-date EQ ?
      OR acct.close-date LT mDate 
      THEN mIsOpen = 1.
   ELSE mIsOpen = 0.

   CREATE ttResp.
   ASSIGN
      ttResp.acct1    = acct.acct
      ttResp.acct     = mSvodAcct
      ttResp.OstVal   =  IF acct.currency EQ "" THEN abs(sh-bal)
                                                ELSE abs(sh-val)
      ttResp.OstRub   = abs(sh-bal)
      ttResp.IsOpen   = mIsOpen
      ttResp.AcctName = IF AVAIL code THEN code.name ELSE "��� ��� � �����䨪���"
      ttResp.SvDog    = GetXattrValueEx ("acct", acct.acct + "," + acct.currency, "�����", "")
   .
   RELEASE loan.
   RELEASE loan-acct.
END.

/* �ந������ ���� ����. 
  �����⠥� �-�� ���/���� */
FOR EACH ttResp  
   NO-LOCK BREAK BY ttResp.acct :
   
   /*�஢�ਬ �� �����*/
   IF ttResp.acct EQ "" AND ttResp.SvDog NE "�� �ନ஢���" THEN 
   DO:
      CREATE ttError.
      ttError.acct = ttResp.acct1.
      NEXT.
   END.
   
   IF STRING(ttResp.IsOpen) EQ "1" THEN
      mOpenCount = mOpenCount + 1.
   ELSE IF STRING(ttResp.IsOpen) EQ "0" THEN
      mCloseCount = mCloseCount + 1.

   /* �訡��� ��� ���﫨, ����� ������ ����� */
   mOstVal = mOstVal + ttResp.OstVal.
   mOstRub = mOstRub + ttResp.OstRub.
   /*--------*/
   mTotalSummRub = mTotalSummRub + ttResp.OstRub.
   mTotalSummVal = mTotalSummVal + mOstVal.

   ACCUMULATE mOstRub(TOTAL) mIsOpen (COUNT) mOstVal (TOTAL).
   IF LAST-OF(ttResp.acct) THEN
   DO:
      CREATE ttOut.
      ASSIGN
         ttOut.acct     = ttResp.Acct
         ttOut.OstVal   = mOstVal
         ttOut.OstRub   = mOstRub
         ttOut.OpenNum  = mOpenCount 
         ttOut.CloseNum = mCloseCount
         ttOut.AcctName = ttResp.AcctName
      .
      ASSIGN
         mOpenCount = 0
         mCloseCount = 0
         mOstVal = 0
         mOstRub = 0.
   END.
END.

/* ᥪ�� ��७��ࠢ����� �뢮�� */
{setdest.i &filename="'ErrorLog.txt'" &stream="stream mStream"}
PUT STREAM mStream "���� �� �訡��� ��⠬." SKIP.
FOR EACH ttError 
   NO-LOCK:
   PUT STREAM mStream UNFORMATTED
      ttError.acct SKIP.
END.


RUN BeginCircle_TTName("mytable").
FOR EACH ttOut
   NO-LOCK:
   mCount = mCount + 1.
   mTmpPart = (ABS(ttOut.OstRub) / ABS(mTotalSummRub)) * 100.

   ACCUMULATE ttOut.OstVal (TOTAL) ttOut.OstRub (TOTAL) ttOut.CloseNum (TOTAL) 
              ttOut.OpenNum (TOTAL) ttOut.ComRate (TOTAL) mTmpPart (TOTAL).

   RUN Insert_TTName ("Acct[mytable]",     ttOut.acct).
   RUN Insert_TTName ("SaldoVal[mytable]", STRING(ttOut.OstVal)).
   RUN Insert_TTName ("SaldoRub[mytable]", STRING(ttOut.OstRub)).
   RUN Insert_TTName ("OpenNum[mytable]",  STRING(ttOut.OpenNum)).
   RUN Insert_TTName ("CloseNum[mytable]", STRING(ttOut.CloseNum)).
   RUN Insert_TTName ("AcctName[mytable]", ttOut.AcctName).
   RUN Insert_TTName ("Part[mytable]",     STRING( mTmpPart)).
   RUN NextCircle_TTName("mytable").
END.
RUN EndCircle_TTName("mytable").
/*--- �뢮� 蠯�� � �⮣�� ---*/

RUN Insert_TTName ("Head", "����饥 ���ﭨ� �� ��⠬ " 
                            + "~"" + mGroupName + "~""
                            + " �� ����� ��� " 
                            + STRING(mDate, "99.99.9999")
                            + " �� ��� �஢����                  �६� ���� "
                            + STRING(TIME,"HH:MM")).

RUN Insert_TTName ("HeadCur", "�����: " + IF mCur EQ "" THEN FGetSetting("�����悠�","","")
                                                         ELSE mCur).
RUN Insert_TTName ("TotalOstVal", STRING(ACCUM TOTAL (ttOut.OstVal))).
RUN Insert_TTName ("TotalOstRub", STRING(ACCUM TOTAL (ttOut.OstRub))).
RUN Insert_TTName ("TotalOpen",   STRING(ACCUM TOTAL (ttOut.OpenNum))).
RUN Insert_TTName ("TotalClose",  STRING(ACCUM TOTAL (ttOut.CloseNum))).
RUN Insert_TTName ("TotalRate",   STRING((ACCUM TOTAL (ttOut.ComRate)) / mCount)). 
RUN Insert_TTName ("TotalPart",   STRING(ACCUM TOTAL (mTmpPart))).

RUN printvd.p("svodacct", INPUT TABLE ttnames). 
{preview.i &filename="'ErrorLog.txt'" &stream="stream mStream"}
{intrface.del} /*���㧪� �����㬥���� */

/*prosignDsG/I8f1xWVdLRXIe6/etg*/