/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: cas-svod318p.p
      Comment: ������� ������� � �������� ��������
   Parameters: ��᪠, ��� ���.������
         Uses:
      Used by:
      Created: 02.08.2008 16:02 elus
     Modified: 02.08.2008 16:02 elus
*/

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.

{globals.i}
{wordwrap.def}
{intrface.get tparam}
{intrface.get sessions}
{intrface.get instrum}
{intrface.get vok} /* �����㬥�� ࠡ��� � ��쥪⠬� ��� */

{ksh-defs.i} /* ����� ����⮢ �� kau */

{svodord.def}

DEFINE TEMP-TABLE ttSprCash NO-UNDO /* ����� ��� �ࠢ�� � ������ �� ��⠬ */
   FIELD Acct       LIKE Acct.acct              /* ��楢�� ��� */
   FIELD Bal-acct   LIKE Acct.bal-acct          /* C�� 2-�� ���浪� */
   FIELD Currency   LIKE Currency.currency      /* ��� ������ */
   FIELD CurrName   LIKE Currency.name-currenc  /* ������������ ������ */
   FIELD ICurrency  LIKE Currency.i-currency    /* ISO-��� ������ */
   FIELD SummDb     AS DECIMAL                  /* ��室 */
   FIELD SummRateDb AS DECIMAL                  /* ��室 � ������*/
   FIELD QtyDb      AS INT64                    /* ���-�� ���㬥�⮢ � ��室� */
   FIELD SummCr     AS DECIMAL                  /* ���室 */
   FIELD SummRateCr AS DECIMAL                  /* ���室 � ������*/
   FIELD QtyCr      AS INT64                    /* ���-�� ���㬥�⮢ � ��室� */
   FIELD Balance    AS DECIMAL                  /* ���⮪ �� ������ ��� */
   FIELD In-Bal     AS DECIMAL                  /* ���⮪ �� ������ �� */
   FIELD Op         AS INTEGER                  /* ����� ����樨 */
   FIELD Debit      AS LOGICAL                  /* ��/�� */
   FIELD DocDate    AS CHARACTER
   FIELD OpDate     AS DATE
   FIELD RecvOnP    AS INT64

   INDEX idxCurrency Currency
   INDEX idxAcct     Acct
   INDEX idxOpDebit  Op debit
   INDEX idxRecvOnP RecvOnP
   INDEX idxdate DocDate
.

DEFINE TEMP-TABLE ttSvodSpr NO-UNDO /* ����� ��� ᢮���� �ࠢ�� � ���ᮢ�� ������ */
   FIELD Currency   LIKE Currency.currency      /* ��� ������ */
   FIELD ICurrency  LIKE Currency.i-currency    /* ISO-��� ������ */
   FIELD CurrName   LIKE Currency.name-currenc  /* ������������ ������ */
   FIELD SummDb     AS DECIMAL                  /* ��室 */
   FIELD SummRateDb AS DECIMAL                  /* ��室 � ������*/
   FIELD QtyDb      AS INT64                    /* ���-�� ���㬥�⮢ � ��室� */
   FIELD SummCr     AS DECIMAL                  /* ���室 */
   FIELD SummRateCr AS DECIMAL                  /* ���室 � ������ */
   FIELD QtyCr      AS INT64                    /* ���-�� ���㬥�⮢ � ��室� */
   FIELD DocDate    AS CHARACTER
   FIELD OpDate     AS DATE
   FIELD RecvOnP    AS INT64

   INDEX idxCurrency RecvOnP Currency
   INDEX idxdate RecvOnP DocDate
.

/*   INDEX idxRecvOnP RecvOnP
*/
DEFINE BUFFER bttSvodSpr FOR ttSvodSpr.
DEFINE BUFFER bTTSvodOrd FOR TTSvodOrd.
DEFINE BUFFER bCurrency  FOR Currency.

{agr-beg.def 
   &NameTitle = "������� ������� � �������� ��������"
   &TypeDoc   = '"spr"'
   &NameRep   = '"��ࠢ����"'} 
   
{agr-beg.i} 

DEFINE VARIABLE mChMask         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChSetCodNacVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChCodNacVal    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLines          AS CHARACTER NO-UNDO EXTENT 16. /* �����窠 */
DEFINE VARIABLE mCols           AS INT64     NO-UNDO. /* ��ਭ� ���� */
DEFINE VARIABLE mCurDprID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOpTime         AS INT64     NO-UNDO.
DEFINE VARIABLE vOpDate         AS DATE      NO-UNDO.
DEFINE VARIABLE vDate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecvType       AS CHARACTER NO-UNDO EXTENT 1. /*2*/
DEFINE VARIABLE mNacISO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPodschDocOtch  AS LOGICAL   NO-UNDO.

ASSIGN
   mLines[ 1] = "��" + FILL("�",{&format-cur-name})                   + 
   "����������������������������������������������������������������������" + 
   "����������Ŀ"
   mLines[ 2] = "� " + PADC("������������ ������",{&format-cur-name}) + " �                ��室                 �                 ���室                �"
   mLines[ 3] = "� " + FILL(" ",{&format-cur-name})                   + 
   " ���������������������������������������������������������������������" + 
   "����������Ĵ"
   mLines[ 4] = "� " + FILL(" ",{&format-cur-name})                   + " � ������⢮ �      �㬬� ��ࠬ�       � ������⢮ �      �㬬� ��ࠬ�       �"
   mLines[ 5] = "� " + FILL(" ",{&format-cur-name})                   + 
   " �  ���ᮢ��  � � 㪠������ ������������ �  ���ᮢ��  � � 㪠������ ��" + 
   "���������� �"
   mLines[ 6] = "� " + FILL(" ",{&format-cur-name})                   + 
   " � ���㬥�⮢ �         ������           � ���㬥�⮢ �         ������" + 
   "           �"
   mLines[ 7] = "��" + FILL("�",{&format-cur-name})                   + 
   "����������������������������������������������������������������������" + 
   "����������Ĵ"
   mLines[10] = ""
   mLines[11] = "� " + PADC("1",{&format-cur-name})                   + " �     2      �            3             �     4      �            5             �"
   mLines[12] = "��" + FILL("�",{&format-cur-name})                   + 
   "����������������������������������������������������������������������" + 
   "����������Ĵ"
   mLines[13] = ""
   mLines[14] = "��" + FILL("�",{&format-cur-name})                   + 
   "����������������������������������������������������������������������" + 
   "������������"
   mLines[15] = "��" + FILL("�",{&format-cur-name})                   + 
   "����������������������������������������������������������������������" + 
   "����������Ĵ"
   mLines[16] = "��" + FILL("�",{&format-cur-name})                   + 
   "����������������������������������������������������������������������" + 
   "����������Ĵ"

   mRecvType[1] = "�� ���㬥�⠬, ��⠢����� �� �㬠���� ���⥫�:"

   .
  /* mRecvType[2] = "�� ���㬥�⠬ � ���஭��� ����:"*/
mCols = LENGTH(mLines[1]).
&GLOBAL-DEFINE cols mCols

ASSIGN 
   mPodschDocOtch  = fGetSetting("����焮����","","���") EQ "��"
   mChMask         = IF NUM-ENTRIES(iParams,";") > 1 THEN ENTRY(1,iParams,";") ELSE iParams
   mChSetCodNacVal = IF NUM-ENTRIES(iParams,";") > 1 THEN ENTRY(NUM-ENTRIES(iParams,";"),iParams,";") ELSE ?
   mChCodNacVal    = fGetSetting(mChSetCodNacVal,"","810")
   mChCodNacVal    = IF CAN-DO("643,810",mChCodNacVal) THEN "" ELSE mChCodNacVal
   mCodOurCur      = mChCodNacVal
   .

FIND FIRST bCurrency WHERE 
           bCurrency.currency EQ ""
NO-LOCK NO-ERROR.
mNacIso = IF AVAILABLE bCurrency THEN 
             (IF mChSetCodNacVal EQ "�����悠�0406007" 
                 AND bCurrency.i-currency EQ "RUR"
              THEN "RUB"
              ELSE bCurrency.i-currency)
          ELSE "???".

DO i = 1 TO NUM-ENTRIES(mUsDprIDLst):
   mCurDprID = ENTRY(i,mUsDprIDLst).
   FOR EACH kau-entry WHERE kau-entry.op-date   EQ mCuDate
                        AND kau-entry.kau-id    BEGINS "��������"
                        AND kau-entry.op-status GE CHR(251)
                        AND kau-entry.kau       EQ mCurDprID
      NO-LOCK,
      FIRST op WHERE op.op EQ kau-entry.op
              AND CAN-DO(mChMask,op.doc-type) 
      NO-LOCK
      BY kau-entry.op:

      RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
                                  
      CASE mDateOtc:
         WHEN "" THEN vDate = "*".
         WHEN "*" THEN
         DO:
            vDate = STRING(vOpDate, "99/99/9999").
            IF NOT CAN-DO(mDateLst, vDate) THEN NEXT.            
         END.
         OTHERWISE
         DO:
            vDate = STRING(vOpDate, "99/99/9999").
            IF mDateOtc NE vDate THEN NEXT.
         END.
      END CASE.

      FIND FIRST RecIdOpDate WHERE RecIdOpDate.op EQ op.op
         NO-LOCK NO-ERROR.
      IF NOT AVAIL RecIdOpDate THEN
      DO:
         CREATE
            RecIdOpDate.
         ASSIGN
            RecIdOpDate.op      = op.op
            RecIdOpDate.DocDate = vDate
            .
      END.
      IF NOT Pereschet(op.op,kau-entry.op-entry,kau-entry.debit) AND 
         NOT (((kau-entry.Currency EQ  "" OR kau-entry.currency EQ ?) AND
                 ((NOT mNCurrency) AND (mICurrency OR mRCurrency)))  
               OR
              ((kau-entry.Currency GT  "" AND kau-entry.currency LE "999") AND 
                 ((NOT (mICurrency OR mRCurrency)) AND mNCurrency)))
      THEN
         RUN AddRecInTemp-Table IN THIS-PROCEDURE(vDate,op.op).
   END.
END.

FOR EACH ttSprCash NO-LOCK:
   FIND FIRST ttSvodSpr WHERE 
              ttSvodSpr.Currency EQ ttSprCash.Currency 
          AND ttSvodSpr.DocDate  EQ ttSprCash.DocDate
          AND ttSvodSpr.RecvOnP  EQ ttSprCash.RecvOnP

      NO-LOCK NO-ERROR.
/* �� �६. ⠡���� ��㯯��㥬 �� �� ����� */
   IF AVAILABLE ttSvodSpr THEN
   DO:
      ASSIGN 
         ttSvodSpr.SummDb     = ttSvodSpr.SummDb     + ttSprCash.SummDb  /* ��室 */
         ttSvodSpr.SummRateDb = ttSvodSpr.SummRateDb + ttSprCash.SummRateDb  /* ��室 */
         ttSvodSpr.QtyDb      = ttSvodSpr.QtyDb      + ttSprCash.QtyDb   /* ���-�� ���㬥�⮢ � ��室� */
         ttSvodSpr.SummCr     = ttSvodSpr.SummCr     + ttSprCash.SummCr  /* ���室 */
         ttSvodSpr.SummRateCr = ttSvodSpr.SummRateCr + ttSprCash.SummRateCr  /* ���室 */
         ttSvodSpr.QtyCr      = ttSvodSpr.QtyCr      + ttSprCash.QtyCr   /* ���-�� ���㬥�⮢ � ��室� */
      .                
   END.
   ELSE
   DO:
      CREATE ttSvodSpr.
      ASSIGN 
         ttSvodSpr.Currency   = ttSprCash.Currency
         ttSvodSpr.iCurrency  = IF mRCurrency THEN mNacISO 
                                ELSE ttSprCash.iCurrency
         ttSvodSpr.CurrName   = ttSprCash.CurrName
         ttSvodSpr.RecvOnP    = ttSprCash.RecvOnP
         ttSvodSpr.SummDb     = ttSprCash.SummDb    /* ��室 */
         ttSvodSpr.SummRateDb = ttSprCash.SummRateDb /* ��室 */
         ttSvodSpr.QtyDb      = ttSprCash.QtyDb     /* ���-�� ���㬥�⮢ � ��室� */
         ttSvodSpr.SummCr     = ttSprCash.SummCr    /* ���室 */
         ttSvodSpr.SummRateCr = ttSprCash.SummRateCr /* ���室 */
         ttSvodSpr.QtyCr      = ttSprCash.QtyCr     /* ���-�� ���㬥�⮢ � ��室� */
         ttSvodSpr.DocDate    = ttSprCash.DocDate
         ttSvodSpr.OpDate     = DATE(ttSprCash.DocDate)
      NO-ERROR.                
   END.
END. /* for each ttSprCash */

IF mCashOrd THEN
DO:
   FOR EACH bttSvodSpr
      BREAK BY bttSvodSpr.DocDate:
      IF FIRST-OF(bttSvodSpr.DocDate) THEN
      DO:
         {empty RecIdOp}
         FOR EACH RecIdOpDate WHERE 
                  RecIdOpDate.DocDate EQ bttSvodSpr.DocDate
            NO-LOCK:
            CREATE
               RecIdOp
               .
            ASSIGN
               RecIdOp.op = RecIdOpDate.op 
               .       
         END.                 
         RUN getcashtt.p(TABLE RecIdOp,OUTPUT TABLE TTSvodOrd,mUsDprIDLst).
         TTSvod:
         FOR EACH ttSvodSpr WHERE
                  ttSvodSpr.DocDate EQ bttSvodSpr.DocDate 
            EXCLUSIVE-LOCK,
             EACH TTSvodOrd WHERE 
                  TTSvodOrd.DocCur EQ ttSvodSpr.currency
            NO-LOCK:
      
            IF TTSvodOrd.OrdType EQ "��室��" THEN
               ttSvodSpr.QtyDb = ttSvodSpr.QtyDb - TTSvodOrd.qty.
            ELSE
            DO:
               IF     mPodschDocOtch 
                  AND IsKasOrd(TTSvodOrd.AcctDb,TTSvodOrd.AcctCr) THEN
                  NEXT TTSvod.
               ttSvodSpr.QtyCr = ttSvodSpr.QtyCr - TTSvodOrd.qty.
            END.
         END.
      END. 
   END.     
END.

{agr-end.i 
   &OnePageRep  = "YES"
   &OnePageName = "PrintOnePageSvodSpr"}

{intrface.del}

/* ========================== Procedure block ============================ */
PROCEDURE PrintOnePageSvodSpr.

   DEFINE VARIABLE vDateStr AS CHARACTER     NO-UNDO.

   DEFINE BUFFER bttSvodSpr FOR ttSvodSpr.

   FOR EACH bttSvodSpr
   BREAK BY bttSvodSpr.OpDate:
   
      IF FIRST-OF(bttSvodSpr.OpDate) THEN
      DO:
         vDateStr = IF {assigned mDateOtc} THEN STRING(DAY(DATE(bttSvodSpr.DocDate)), "99 ") + ENTRY(MONTH(DATE(bttSvodSpr.DocDate)),'ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������') + STRING(YEAR(DATE(bttSvodSpr.DocDate)), " 9999 ����")
                                           ELSE STRING(DAY(mCuDate),        "99 ") + ENTRY(MONTH(mCuDate),       'ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������') + STRING(YEAR(mCuDate),        " 9999 ����").

/* ========================== ��������� ����� =========================== */
         {head-318p.i
            &CodForm = "'0402114'"
         }
         {orgname318p.i
            &CurBranchName = mBranchName
         }
      
         PUT UNFORMATTED
            PADC("������� � �������� ��������",{&Cols}) SKIP
            PADC(vDateStr,{&Cols})                      SKIP(1).

/* ========================== ����� ⠡��窨 ============================= */
         DO i = 1 TO 11:
            IF i <> 10 AND mLines[i] <> "" THEN
               PUT UNFORMATTED mLines[i] SKIP.
         END.

/* ==================== �������� ⠡��窨 � �㬬��� ====================== */
      DO i = 1 TO EXTENT(mRecvType):
         PUT UNFORMATTED
            mLines[15] SKIP
            "� " PADR(mRecvType[i],LENGTH(mLines[1]) - 3)
            "�" SKIP
            mLines[16] SKIP.
         FOR EACH ttSvodSpr WHERE 
                  ttSvodSpr.DocDate EQ bttSvodSpr.DocDate 
              AND ttSvodSpr.RecvOnP EQ i
         NO-LOCK
         BREAK BY ttSvodSpr.RecvOnP:
            IF NOT FIRST-OF(ttSvodSpr.RecvOnP) THEN 
               PUT UNFORMATTED mLines[12] SKIP.              
            PUT UNFORMATTED "� "
               ttSvodSpr.CurrName FORMAT "x({&format-cur-name})"  " � "
               ttSvodSpr.QtyDb    FORMAT ">>,>>>,>>9" " �  " 
               (IF mRCurrency THEN ttSvodSpr.SummRateDb
               ELSE ttSvodSpr.SummDb) FORMAT "->>>,>>>,>>>,>>9.99" " " 
               ttSvodSpr.iCurrency " � " 
               ttSvodSpr.QtyCr    FORMAT ">>,>>>,>>9" " �  "
               (IF mRCurrency THEN ttSvodSpr.SummRateCr
               ELSE ttSvodSpr.SummCr) FORMAT "->>>,>>>,>>>,>>9.99" " " 
               ttSvodSpr.iCurrency " � " SKIP.
         END. /* FOR EACH ttSvodSpr */
      END.
         PUT UNFORMATTED mLines[14] SKIP(1).
/* ==================== ���⠥� ������ ��� �ࠢ��� ==================== */
         RUN GetRepFioByRef(ENTRY(1,PROGRAM-NAME(2), "."),mCuBrchID,?,mCuDprID).
         PUT UNFORMATTED SKIP(1).
         RUN PrintFioAndPost(mFioInRep[1],mPostInRep[1],0).
         PUT UNFORMATTED SKIP(1)
            "� ����묨 ��壠���᪮�� ��� ᢥ७�:" SKIP(1).
         RUN PrintFioAndPost(mFioInRep[2],mPostInRep[2],0).
      END.
   END.

END PROCEDURE. /* PrintOnePageBookVal */

/*------------------------------------------------------------------------------
   �����祭��: �������� �� �६����� ⠡���� ����� � ������ 
------------------------------------------------------------------------------*/
PROCEDURE AddRecInTemp-Table:

   DEFINE INPUT  PARAMETER iOpDate AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iOp AS INT64 NO-UNDO.

   DEFINE VARIABLE vNumEntry AS INT64.             /* ���-�� ᬥ� � */
   DEFINE VARIABLE vDprId    AS CHARACTER  NO-UNDO.  /* ����� */
   DEFINE VARIABLE vCurrName AS CHARACTER  NO-UNDO. /* ������������ ������ */
   DEFINE VARIABLE vOp       AS INT64      NO-UNDO. /* ���㬥�� */
   DEFINE VARIABLE vRate     AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vCurr     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRecvOnPaper  AS INT64     NO-UNDO.

   DEFINE BUFFER bAcct      FOR Acct.
   DEFINE BUFFER bCurrency  FOR Currency.
   DEFINE BUFFER bttSprCash FOR ttSprCash.
   DEFINE BUFFER bkau-entry FOR kau-entry.
   DEFINE BUFFER bOp        FOR op.
   {find-act.i &bact=bAcct &acct=kau-entry.acct}
                          /* ��� �������� ⮫쪮 ����� �� �����ᮢ�� ��� 20207 */
   IF AVAILABLE bAcct THEN 
   DO:               
      mAgreeYes = TRUE. /* ����� ��� ����� ���� */
      FIND FIRST bop WHERE bop.op EQ iOp NO-LOCK NO-ERROR.
      vRecvOnPaper = INT64(GetXAttrValue("op",STRING(bop.op),
                           "���ᮡ�����") NE "") + 1.
      FIND FIRST ttSprCash WHERE
                 ttSprCash.Currency EQ kau-entry.Currency
             AND ttSprCash.DocDate  EQ iOpDate
             AND ttSprCash.RecvOnP  EQ vRecvOnPaper
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttSprCash THEN
      DO:
         vCurr = IF kau-entry.Currency EQ "" THEN mChCodNacVal ELSE kau-entry.Currency.
         /* ������ ������������ ������ */
         FIND FIRST bCurrency WHERE 
                    bCurrency.currency EQ vCurr
            NO-LOCK NO-ERROR.

         vCurrName = IF AVAILABLE bCurrency THEN bCurrency.name-currenc ELSE "?".
         CREATE ttSprCash.
         ASSIGN
            ttSprCash.Currency = kau-entry.Currency
            ttSprCash.CurrName = vCurrName
            ttSprCash.RecvOnP  = vRecvOnPaper
            ttSprCash.iCurrency = IF AVAILABLE bCurrency THEN
                                     (IF mChSetCodNacVal EQ "�����悠�0406007"
                                         AND bcurrency.i-currency EQ "RUR"
                                      THEN "RUB"
                                      ELSE bcurrency.i-currency)
                                  ELSE ?
            ttSprCash.DocDate  = iOpDate
            ttSprCash.OpDate   = DATE(iOpDate)
         NO-ERROR.
      END.

      IF     kau-entry.Currency GT "" 
         AND kau-entry.Currency LE "999" THEN
      DO:
         vRate = DECIMAL(GetXAttrValue("op",STRING(kau-entry.op),"sprate")) NO-ERROR.
         IF vRate EQ 0 THEN
            vRate = FindRateSimple("����",
                                   kau-entry.currency,
                                   kau-entry.op-date).
      END.

      IF kau-entry.debit THEN
         ASSIGN
            ttSprCash.SummDb = ttSprCash.SummDb + (IF kau-entry.Currency EQ "" THEN
                                  kau-entry.amt-rub
                               ELSE
                                  kau-entry.amt-cur)
            ttSprCash.SummRateDb = ttSprCash.SummRateDb + vRate * kau-entry.amt-cur
         .
      ELSE
         ASSIGN
            ttSprCash.SummCr = ttSprCash.SummCr + (IF kau-entry.Currency EQ "" THEN
                                  kau-entry.amt-rub
                               ELSE
                                  kau-entry.amt-cur)
            ttSprCash.SummRateCr = ttSprCash.SummRateCr + vRate * kau-entry.amt-cur
         .

      FIND FIRST bttSprCash WHERE bttSprCash.Op       EQ kau-entry.op
                              AND bttSprCash.debit    EQ kau-entry.debit
                              AND bttSprCash.currency EQ kau-entry.currency
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bttSprCash OR NOT mPodschKolDoc THEN
         IF kau-entry.debit THEN
            ttSprCash.QtyDb = ttSprCash.QtyDb + 1.
         ELSE
         DO:
            IF    NOT mPodschDocOtch 
               OR NOT IsOpKasOrd(iOp) THEN
               ttSprCash.QtyCr = ttSprCash.QtyCr + 1.
         END.

      ASSIGN
         ttSprCash.op    = kau-entry.op
         ttSprCash.debit = kau-entry.debit
      .
   END.

END PROCEDURE. /* AddRecInTemp-Table */
/* $LINTUSER='BIS' */
/* $LINTENV ='2st' */
/* $LINTVSS ='$/ws2-tst/bq/4.1d/' */
/* $LINTDATE='27/11/2014 12:32:20.212+04:00' */
/* $LINTFILE='cas-svod318p.p' */
/*prosign5yPGsZWju9c91WgTMTOowg*/