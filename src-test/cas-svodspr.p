/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: cas-svodspr.p
      Comment: ������� ������� � �������� ��������
   Parameters: ��᪠, ��� ���.������
         Uses:
      Used by:
      Created: 02.08.2008 16:02 elus
     Modified: 02.08.2008 16:02 elus
*/

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.

{globals.i}
{sh-defs.i}
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
   FIELD DocNum     LIKE op.doc-num				/* ����� ���㬥�� */
   FIELD CheckM     AS LOGICAL                  /* �஢���� �७������� ������ 祪�*/

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

DEFINE BUFFER bttSvodSpr FOR ttSvodSpr.
DEFINE BUFFER bTTSvodOrd FOR TTSvodOrd.
DEFINE BUFFER bCurrency  FOR Currency.

{agr-beg.def 
   &NameTitle = "������� ������� � �������� ��������"
   &TypeDoc   = '"spr"'
   &NameRep   = '"��ࠢ����"'} 
   
DEFINE VARIABLE mChMask         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChSetCodNacVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChCodNacVal    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLines          AS CHARACTER NO-UNDO EXTENT 16. /* �����窠 */
DEFINE VARIABLE mCols           AS INT64     NO-UNDO. /* ��ਭ� ���� */
DEFINE VARIABLE mCurDprID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOpTime         AS INT64     NO-UNDO.
DEFINE VARIABLE vOpDate         AS DATE      NO-UNDO.
DEFINE VARIABLE vDate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecvType       AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE mNacISO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctOm			AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPodschDocOtch  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOtstup         AS CHARACTER NO-UNDO.

mOtstup = FILL(" ",14).

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
   mRecvType[2] = "�� ���㬥�⠬ � ���஭��� ����:"
   .

mCols = LENGTH(mLines[1]).
&GLOBAL-DEFINE cols mCols

ASSIGN 
   mPodschDocOtch  = fGetSetting("����焮����","","���") EQ "��"
   mChMask         = IF NUM-ENTRIES(iParams,";") > 1 THEN ENTRY(1,iParams,";") ELSE iParams
   mChSetCodNacVal = IF NUM-ENTRIES(iParams,";") > 1 THEN ENTRY(NUM-ENTRIES(iParams,";"),iParams,";") ELSE ?
   mChCodNacVal    = fGetSetting(mChSetCodNacVal,"","810")
   mChCodNacVal    = IF CAN-DO("643,810",mChCodNacVal) THEN "" ELSE mChCodNacVal
   mCodOurCur      = mChCodNacVal
/*   mAcctOm         = "20202398205001000000*,20202810505001000000*,20202840805001000000*,20202978405001000000*"*/
   mAcctOm         = "20202*"
   .

{getdate.i}
 
FIND FIRST bCurrency WHERE 
           bCurrency.currency EQ ""
NO-LOCK NO-ERROR.
mNacIso = IF AVAILABLE bCurrency THEN 
             (IF mChSetCodNacVal EQ "�����悠�0406007" 
                 AND bCurrency.i-currency EQ "RUR"
              THEN "RUB"
              ELSE bCurrency.i-currency)
          ELSE "???".

/**/

FOR EACH op-entry
 WHERE op-entry.op-date   EQ end-date
	 AND op-entry.filial-id EQ shFilial
	 AND (   CAN-DO(mAcctOm,op-entry.acct-db)
           OR CAN-DO(mAcctOm,op-entry.acct-cr))
  NO-LOCK,
  FIRST op WHERE op.op EQ op-entry.op
		  AND CAN-DO(mChMask,op.doc-type) 
  NO-LOCK
  BY op-entry.op:
	
  RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
							  
  vDate = "*".

   RUN AddRecInTemp-Table IN THIS-PROCEDURE(vDate,op.op).
END.

/**/
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
   
&IF DEFINED(Cols) &THEN
   {setdest.i &Cols=" + {&Cols}"}
&ELSE
   {setdest.i &Cols=" + {&LengthPrinter}"}
&ENDIF
   RUN PrintOnePageSvodSpr.
   
{preview.i}   

{intrface.del}

/* ========================== Procedure block ============================ */
PROCEDURE PrintOnePageSvodSpr.

   DEFINE VARIABLE vDateStr AS CHARACTER     NO-UNDO.

   DEFINE BUFFER bttSvodSpr FOR ttSvodSpr.

   FOR EACH bttSvodSpr
   BREAK BY bttSvodSpr.OpDate:
   
      IF FIRST-OF(bttSvodSpr.OpDate) THEN
      DO:
         vDateStr = STRING(DAY(end-date), "99 ") + ENTRY(MONTH(end-date),'ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������') + STRING(YEAR(end-date), " 9999 ����").

/* ========================== ��������� ����� =========================== */
         {head-318p.i
            &CodForm = "'0402114'"
         }
         PUT UNFORMATTED
	        mOtstup mOtstup mOtstup '��� "���� ����"' SKIP
			mOtstup FILL('�', 82) SKIP.
     	 PUT UNFORMATTED
			mOtstup " ������ �ଥ���� (᮪�饭��� �ଥ����) ������������ �।�⭮� �࣠����樨 ���  " SKIP
			mOtstup "  ������ (᮪�饭���) ������������ 䨫����, ��� ������������ � (���) ����� ���   " SKIP
			mOtstup "(�� ����稨) ���� ��� ����������騥 �ਧ���� ��� (�� ������⢨� ������������" SKIP
			mOtstup "   � �����) � 㪠������ �� ��� �ਭ���������� �।�⭮� �࣠����樨 (䨫����)    " SKIP(1).
         PUT UNFORMATTED
            PADC("������� � �������� ��������",{&Cols}) SKIP
            PADC(vDateStr,{&Cols})                      SKIP(1).

/* ========================== ����� ⠡��窨 ============================= */
         DO i = 1 TO 11:
            IF i <> 10 AND mLines[i] <> "" THEN
               PUT UNFORMATTED mLines[i] SKIP.
         END.

/* ==================== �������� ⠡��窨 � �㬬��� ====================== */
         FOR EACH ttSvodSpr WHERE 
                  ttSvodSpr.DocDate EQ bttSvodSpr.DocDate NO-LOCK
         BREAK BY ttSvodSpr.RecvOnP:

            IF FIRST-OF(ttSvodSpr.RecvOnP) THEN 
               PUT UNFORMATTED
                  mLines[15] SKIP
                  "� " PADR(mRecvType[ttSvodSpr.RecvOnP],LENGTH(mLines[1]) - 3)
                  "�" SKIP
                  mLines[16] SKIP.
            ELSE  
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
   
         PUT UNFORMATTED mLines[14] SKIP(1).
/* ==================== ���⠥� ������ ��� �ࠢ��� ==================== */
         RUN GetRepFioByRef('cas-svod318p','0500',?,?).
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

   DEFINE VARIABLE vDebit       AS LOGICAL.             
   DEFINE VARIABLE vDprId       AS CHARACTER NO-UNDO.  /* ����� */
   DEFINE VARIABLE vCurrName    AS CHARACTER NO-UNDO. /* ������������ ������ */
   DEFINE VARIABLE vOp          AS INT64     NO-UNDO. /* ���㬥�� */
   DEFINE VARIABLE vRate        AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vCurr        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRecvOnPaper AS INT64     NO-UNDO.
   DEFINE VARIABLE vChN         AS INT64     NO-UNDO INIT 1. /* ��� ������ �஢���� ��� 祪�*/

   DEFINE BUFFER bCurrency  FOR Currency.
   DEFINE BUFFER bttSprCash FOR ttSprCash.
   DEFINE BUFFER bop-entry  FOR op-entry.
   DEFINE BUFFER bop        FOR op.
   DEFINE BUFFER bbop       FOR op.
   
      mAgreeYes = TRUE. /* ����� ��� ����� ���� */
      FIND FIRST bop WHERE bop.op EQ iOp NO-LOCK NO-ERROR.
      vRecvOnPaper = INT64(GetXAttrValue("op",STRING(bop.op),
                           "���ᮡ�����") NE "") + 1.
      FIND FIRST ttSprCash WHERE
			           ttSprCash.Op       EQ op-entry.op
             AND ttSprCash.Currency EQ op-entry.currency
             AND ttSprCash.DocDate  EQ iOpDate
             AND ttSprCash.RecvOnP  EQ vRecvOnPaper
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttSprCash THEN
      DO:
         vCurr = IF op-entry.currency EQ "" THEN mChCodNacVal ELSE op-entry.currency.
         /* ������ ������������ ������ */
         FIND FIRST bCurrency WHERE 
                    bCurrency.currency EQ vCurr
            NO-LOCK NO-ERROR.

         vCurrName = IF AVAILABLE bCurrency THEN bCurrency.name-currenc ELSE "?".
         CREATE ttSprCash.
         ASSIGN
			      ttSprCash.op        = op-entry.op
            ttSprCash.Currency  = op-entry.currency
            ttSprCash.Acct      = op-entry.acct-db
            ttSprCash.CurrName  = vCurrName
            ttSprCash.RecvOnP   = vRecvOnPaper
			      ttSprCash.DocNum    = bop.doc-num
            ttSprCash.iCurrency = IF AVAILABLE bCurrency THEN
                                     (IF mChSetCodNacVal EQ "�����悠�0406007"
                                         AND bcurrency.i-currency EQ "RUR"
                                      THEN "RUB"
                                      ELSE bcurrency.i-currency)
                                  ELSE ?
            ttSprCash.DocDate   = iOpDate
            ttSprCash.OpDate    = DATE(iOpDate)
			      ttSprCash.CheckM    = FALSE
         NO-ERROR.
      END.

      IF CAN-DO(mAcctOm,op-entry.acct-db) THEN
         ASSIGN
            ttSprCash.SummDb = ttSprCash.SummDb + (IF op-entry.currency EQ "" THEN
                                  op-entry.amt-rub
                               ELSE
                                  op-entry.amt-cur)
         .
      ELSE
         ASSIGN
            ttSprCash.SummCr = ttSprCash.SummCr + (IF op-entry.Currency EQ "" THEN
                                  op-entry.amt-rub
                               ELSE
                                  op-entry.amt-cur)
         .
      vDebit = IF CAN-DO(mAcctOm,op-entry.acct-db) THEN TRUE ELSE FALSE.
	  
      FIND FIRST bttSprCash WHERE bttSprCash.Op       EQ op-entry.op
                              AND bttSprCash.debit    EQ vDebit
                              AND bttSprCash.currency EQ op-entry.currency
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bttSprCash OR NOT mPodschKolDoc THEN
         IF CAN-DO(mAcctOm,op-entry.acct-db) THEN
            ttSprCash.QtyDb = ttSprCash.QtyDb + 1.
         ELSE
         DO:
            IF     NOT mPodschDocOtch 
               OR  NOT CAN-FIND(FIRST bop-entry WHERE 
                                      bop-entry.Op     EQ op-entry.op
                                  AND CAN-DO(mAcctOm,bop-entry.acct-db)) THEN
               ttSprCash.QtyCr = ttSprCash.QtyCr + 1.
         END.
	  
	  /* �஢�ઠ �� 祪 �� ��᪮� ��� */
	  FIND FIRST bttSprCash WHERE bttSprCash.DocNum EQ bop.doc-num
							  AND bttSprCash.op     NE op-entry.op
                AND bttSprCash.Acct   EQ op-entry.acct-db
		NO-LOCK NO-ERROR.
	  
	   IF AVAIL bttSprCash AND bttSprCash.CheckM AND op.doc-type EQ '04' THEN DO:
			ttSprCash.QtyCr = ttSprCash.QtyCr - 1.
		END.
	  
	  IF op.doc-type EQ '04' THEN DO:
		FOR EACH bop-entry WHERE bop-entry.op-date   EQ op-entry.op-date
							 AND bop-entry.filial-id EQ '0500'
							 AND bop-entry.acct-db   EQ op-entry.acct-db
							 AND bop-entry.acct-cr   EQ op-entry.acct-cr,
		   FIRST bbop of bop-entry WHERE bbop.doc-type EQ '04'
						             AND bbop.doc-num  EQ op.doc-num
		   BY bop-entry.op-date:
				vChN = vChN + 1.
		END.
    IF vChN > 1 THEN DO:
			ttSprCash.CheckM = TRUE.
      vChN = 0.
		END.
	  END.
	  /**/	  
		  
      ASSIGN
         ttSprCash.debit = IF CAN-DO(mAcctOm,op-entry.acct-db) THEN TRUE ELSE FALSE
      .

END PROCEDURE. /* AddRecInTemp-Table */