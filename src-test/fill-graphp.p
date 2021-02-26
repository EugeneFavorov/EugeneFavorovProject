/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: fill-graphp.p
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created: 10/05/2013
*/

{globals.i}

{svarloan.def new}

{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get instrum}  /* ������⥪� ��� ࠡ��� � 䨭. �����㬥�⠬�. */
{intrface.get loan}     /* �����㬥��� ��� ࠡ��� � loan. */
{intrface.get loanc}
{intrface.get corr}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get date}
{intrface.get pogcr}    /* ������⥪� �����㬥�⮢ ��� ࠡ��� � ��䨪��� ����襭�� � ���. */

{fill-graphp.def}           /* ������� ��.⠡���� ttReportTable */
{pp-corr.p}

DEF INPUT PARAM iContract  AS CHAR NO-UNDO.    /* �����祭�� ������� */
DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.    /* ����� ������� */
DEF INPUT PARAM iDate      AS DATE NO-UNDO.    /* ��� */
DEF INPUT PARAM fullKasko as decimal no-undo.
DEF INPUT PARAM typeTable as char no-undo.     /* 1- ���, 2- ���+��᪮, 3 - ��+ */
DEF OUTPUT PARAM TABLE FOR ttReportTable.      /* �६����� ⠡��� */

/* ������� ��६����� */
DEF VAR mSettList      AS CHAR  NO-UNDO.            /* ���᮪ �� */
DEF VAR mSettings      AS CHAR  NO-UNDO EXTENT 5.   /* ����஥�� ��ࠬ���� */
DEF VAR mID            AS INT64 NO-UNDO INIT 1.     /* ���浪��� ����� ��ப� (��� ���� ���) */
DEF VAR mIsReal        AS LOG   NO-UNDO.            /* �ਧ��� ����஢���� ��易⥫��� "��� ����" */
DEF VAR mIsRealTr      AS LOG   NO-UNDO.            /* �ਧ��� ����஢���� ��易⥫��� "��� ����" ��� �࠭�*/
DEF VAR mIsOneTerm     AS LOG   NO-UNDO.            /* �ਧ��� �����⢥����� ��易⥫��⢠ �� �᫮��� */
DEF VAR mIsVirtual     AS LOG   NO-UNDO.            /* �ਧ��� ����室����� ᮧ����� ����㠫쭮�� ��䨪� */
DEF VAR mI             AS INT64 NO-UNDO.
DEF VAR mJ             AS INT64 NO-UNDO.
DEF VAR mTrshAmnt      AS DEC   NO-UNDO EXTENT 3.   /* �㬬� �� �������(1), �࠭襩 (2), �࠭� (3) */
DEF VAR mDateTr        AS DATE  NO-UNDO.            /*��� ����砭�� ��᫥����� �࠭� �� ��������*/

DEF VAR mCopyOD        AS LOG   NO-UNDO.        /* ����஢��� ��� ��ந�� ������ ��䨪 �� �� ����. �������� */
DEF VAR mSumOper       AS DEC   NO-UNDO.        /* �㬬� ��易⥫��� �� ������� (�뤠�) �� ��������\�࠭��, ��� ���� �� �뤠����� ���⪠ */
DEF VAR mSumVyd        AS DEC   NO-UNDO.        /* �㬬� ��� ����権 (� ��⭮� ��砥 �뤠�) �� ��������, 㪠������ � �� ���4��� */
DEF VAR mIsVyd         AS LOG   NO-UNDO.        /* �ਧ��� ������ �뤠� �� �������� */
DEF VAR mStrProp       AS CHAR  NO-UNDO.        /* �� �������ய */
DEF VAR mSumTov        AS CHAR  NO-UNDO.        /* �� �㬒�����। */
DEF VAR mCoefTov       AS DEC   NO-UNDO.        /* ����. ⮢�� */
DEF VAR mCoefSrok      AS DEC   NO-UNDO.        /* ����. �ப� ���客���� */
DEF VAR mNumDCred      AS INT64 NO-UNDO.
DEF VAR mNumDIns       AS INT64 NO-UNDO.
def var procSt as dec no-undo. /* ��業⭠� �⠢�� */
def var datePlat as date no-undo. /* ��� ���⥦� */
def var myIntDate as dec no-undo. /* ���� ���⥦� */

DEF VAR rko11_price as decimal no-undo.

   /* ���������� ���஢ */
DEF BUFFER loan        FOR loan.
DEF BUFFER b-loan      FOR loan.
DEF BUFFER ins-loan    FOR loan.
DEF BUFFER loan-cond   FOR loan-cond.
DEF BUFFER b-loan-cond FOR loan-cond.
DEF BUFFER term-obl    FOR term-obl.
DEF BUFFER b-term-obl  FOR term-obl.
DEF BUFFER x-term-obl  FOR term-obl.
DEF BUFFER loan-acct   FOR loan-acct.
DEF BUFFER b-loan-acct FOR loan-acct.
DEF BUFFER loan-int    FOR loan-int.
DEF BUFFER chowhe      FOR chowhe.
DEF BUFFER comm-rate   FOR comm-rate.

/* ���塞 �������⥫�� �६���� ⠡���� */
DEF TEMP-TABLE tt-term-obl  LIKE term-obl.  /* ��� idnt = 3 �᭮���� ���� � idnt = 1 %% */

   /* �⫠��筠� ���ଠ�� */
IF SESSION:DEBUG-ALERT THEN DO:
   DEF STREAM vStream.
   OUTPUT STREAM vStream TO VALUE("epscalc_debug.tmp") APPEND.
END.

{fill-graphp.i}             /* �����㬥��� ��� ����� ��� */

mFormGrKom = FGetSetting("��䊮��","��ଃ����",?).

   /* ��頥� ��.⠡���� */
{empty tt-term-obl}

   /* �⫠��筠� ���ଠ�� */
IF SESSION:DEBUG-ALERT THEN DO:
   PUT STREAM vStream UNFORMATTED
      "������� �: " iContCode SKIP
      "(1 - %%, 2 - ���⪨, 3 - ��, 4 - ���.��., �����ᨨ, 5 - ���客� ������)"
   SKIP.
END.

rko11_price = 0.
FIND FIRST loan WHERE
           loan.contract  EQ iContract
   AND     loan.cont-code EQ iContCode
NO-LOCK NO-ERROR.

IF AVAIL loan THEN
DO:
	mSumVyd = 0.
	if loan.cont-type = "��祭��" then do: /* �।�⭠� ����� (�� ���⥦� �ਤ�뢠��) */
/*		/* ���� �������饣� �᫮��� �� ���� ���� */
		FIND LAST loan-cond WHERE
            loan-cond.contract    EQ loan.contract
			AND loan-cond.cont-code   EQ loan.cont-code
			AND loan-cond.since       LE iDate
			NO-LOCK NO-ERROR.
		/* ��� �᫮��� - �ய�᪠�� */
		IF AVAIL loan-cond THEN
		DO:
			find first term-obl where term-obl.cont-code = loan.cont-code and term-obl.idnt = 2 and term-obl.nn  = 0 no-lock no-error.
			if avail term-obl then do:
				mSumVyd = term-obl.amt-rub.
				procSt = 0.
				find first comm-rate where commission = '%�।' and kau begins '�।��,' + loan.cont-code no-lock no-error.
				if avail comm-rate then do:
						procSt = rate-comm.
						datePlat = term-obl.end-date.
						/* ���࠭塞 �㬬� ����� �� �६����� ⠡��� */
						RUN CrtRepTbl(2,   /* ���⪨ */
						loan.open-date,
                        - mSumVyd,
						0).
						myIntDate = loan-cond.int-date.
						if myIntDate = 0 then myIntDate = day(loan.open-date).
						
						
						FUNCTION FindNextWorkDate RETURNS date
   (iDay AS Decimal,
    iDate AS DATE):
						
						
						
						
						
						
						
				end.
			end.
		END.
		/* �⫠��筠� ���ଠ�� */
		IF SESSION:DEBUG-ALERT THEN DO:
			OUTPUT STREAM vStream CLOSE.
		END.

		RUN DeleteOldDataProtocol IN h_base ("������� �� ��������� �����").
		RUN DeleteOldDataProtocol IN h_base ("������������� �� �������� �����").

		{intrface.del}
		return.
*/		
	end.

   /* ��� ���客�� ���⥦�� (��易⥫���)
   ** ���� ������� ���客���� */
      FOR EACH ins-loan WHERE
               ins-loan.contract         EQ "����"
         AND   ins-loan.parent-cont-code EQ iContCode
         AND   ins-loan.parent-contract  EQ iContract
         AND   ins-loan.open-date        LE iDate
         AND  (ins-loan.close-date       GE iDate
            OR ins-loan.close-date       EQ ?)
      NO-LOCK,
            EACH term-obl OF ins-loan WHERE term-obl.idnt EQ 1
            NO-LOCK 
            BREAK BY term-obl.end-date:
         /* ���࠭塞 �㬬� �� �६����� ⠡��� */
         RUN CrtRepTbl(5,
                       term-obl.end-date,
                       term-obl.amt-rub,
                       mID).
      END. /* ��� ���客�� ���⥦�� */

      /* ���� �������饣� �᫮��� �� ���� ���� */
   FIND LAST loan-cond WHERE
             loan-cond.contract    EQ loan.contract
      AND    loan-cond.cont-code   EQ loan.cont-code
      AND    loan-cond.since       LE iDate
   NO-LOCK NO-ERROR.
      /* ��� �᫮��� - �ய�᪠�� */
   IF AVAIL loan-cond THEN
   DO:
      /* �᫨ ������� �� "��祭��"*/
      IF loan.cont-type NE "��祭��" THEN
         mIsReal = TRUE. /* �����㥬 ��易⥫��⢠ � ������� "��� ����" */
     
         /* �����㥬 ��易⥫��⢠ "��� ����" */
      IF mIsReal THEN
      DO:
            /* �⫠��筠� ���ଠ�� */
         IF SESSION:DEBUG-ALERT THEN DO:
             PUT STREAM vStream UNFORMATTED
                '�����㥬 ��易⥫��⢠ "��� ����":'
             SKIP.
         END.
         DO mI = 1 TO 3:
            RUN CopyTTData(loan.contract,
                           loan.cont-code,
                           mI,
                           loan.end-date).
         END.
      END.

      /* �⫠��筠� ���ଠ�� */
      IF SESSION:DEBUG-ALERT THEN DO:
          PUT STREAM vStream UNFORMATTED
             "��७�� ������ � ⠡���� ���� ttReportTable: "
          SKIP.
      END.

      /* ������塞 ����� � ⠡���� ���� ttReportTable �� tt-term-obl  */
      FOR EACH tt-term-obl WHERE
               tt-term-obl.idnt GE 1
         AND   tt-term-obl.idnt LE 3:
         
         RUN CrtRepTbl(tt-term-obl.idnt,
                       tt-term-obl.end-date,
                       tt-term-obl.amt-rub,
                       mID).
         mID = mID + 1.
      END.
      
      /* �⫠��筠� ���ଠ�� */
      IF SESSION:DEBUG-ALERT THEN DO:
          PUT STREAM vStream UNFORMATTED
             '�㬬� �� ����� ����権:'
          SKIP.
      END.
     
            /* �᫨ ��䨪 �����ᨩ ������� �� �������, � ��६ ����� �� ��䨪�*/
      IF mFormGrKom EQ "��" THEN
      DO:
         FOR EACH x-term-obl WHERE x-term-obl.contract  EQ loan.contract
                               AND x-term-obl.cont-code EQ loan.cont-code
                               AND x-term-obl.idnt      EQ 10
         NO-LOCK:
               /* ��७�ᨬ ����祭�� १����� � ⠡���� ���� */
               RUN CrtRepTbl (4,
                              x-term-obl.end-date,
                              x-term-obl.amt-rub,
                              mID).
         END.
      END.

      FIND FIRST signs WHERE signs.file-name EQ "loan"
                         AND signs.surrogate EQ loan.contract + "," + loan.cont-code
                         AND signs.code      EQ "rko_comiss" NO-LOCK NO-ERROR.
      IF AVAIL signs THEN DO:
       
         /* ��७�ᨬ ����祭�� १����� � ⠡���� ���� */
         RUN CrtRepTbl (400,
                        loan.open-date,
                        dec(signs.code-value),
                        mID).
		 /*rko11_price = dec(signs.code-value).*/
      END.
      ELSE DO:
         
         FIND LAST comm-rate WHERE comm-rate.commission EQ "%���"
                               AND CAN-DO(comm-rate.kau, loan.cont-code)
                               AND CAN-DO(comm-rate.kau, loan.contract)
                               NO-LOCK NO-ERROR.
         IF AVAIL comm-rate THEN
             /* ��७�ᨬ ����祭�� १����� � ⠡���� ���� */
            RUN CrtRepTbl (400,
                           loan.open-date,
                           comm-rate.rate-comm,
                           mID).
			/*rko11_price = comm-rate.rate-comm.*/
      END.   
   END.  /* AVAIL loan-cond */
END.     /* AVAIL loan      */

   /* �� ��।������ ������ �⮨���� �।�� �� ᡮ�� (�����ᨨ),
   ** �।�����騥 ��� ����᫥��� �������� �।�� ����騪� (���ਬ��,
   ** �� ��ᬮ�७�� ��� �� �।���), ��������� � ��⠢ ���⥦��,
   ** �����⢫塞�� ����騪�� �� ���� ��砫쭮�� ��������� ��⮪� (���⥦�)) */

def var myMonth as int64 no-undo.
def var myLastDate as date no-undo.
def var pred_tf_rest-debts as decimal no-undo.
def var tmpRko11 as char no-undo.

tmpRko11 = getxattrvalue("loan","�।��," + loan.cont-code,"rko11_price").

if tmpRko11 <> ? and tmpRko11 <> "" then do:
	rko11_price = decimal(replace(tmpRko11,",","")).
end.
else rko11_price = 0.

/* if loan.cont-type = '��⮏���' then do: */
 /*if can-do('���+�*,���+�*', loan.cont-type) then do:   */
 if typeTable = "2" then do:
	FOR EACH ttReportTable BY ttReportTable.tf_payment-date DESCENDING:
		myLastDate = ttReportTable.tf_payment-date.
		leave.
	END.
	
	create ttReportTable.
	assign
		ttReportTable.tf_id = -1
		ttReportTable.tf_payment-date = myLastDate.
    release ttReportTable.
	
def var countPlat as int no-undo.
	
   FOR EACH ttReportTable BY ttReportTable.tf_payment-date:
      IF ttReportTable.tf_id EQ 0 THEN
      DO:
		 pred_tf_rest-debts = ttReportTable.tf_rest-debts.
		 /* fullKasko = ttReportTable.tf_actual-payment.  */
         ACCUMULATE ttReportTable.tf_additional-charge1 (TOTAL). /* 4,400. �㬬� �������⥫��� �ॡ������, �����ᨨ */
         ACCUMULATE ttReportTable.tf_additional-charge2 (TOTAL).
         ACCUMULATE ttReportTable.tf_actual-payment (TOTAL).     /* 5. �㬬� ���客�� ����ᮢ */
		 myMonth = month(ttReportTable.tf_payment-date).
		 countPlat = 0.
         ASSIGN
             ttReportTable.tf_additional-charge1 = 0
             ttReportTable.tf_additional-charge2 = 0
         .
         DELETE ttReportTable.
      END.
      ELSE
      DO:
	  countPlat = countPlat + 1.
         ASSIGN
            ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ACCUM TOTAL ttReportTable.tf_additional-charge1
            ttReportTable.tf_additional-charge2 = ttReportTable.tf_additional-charge2 + ACCUM TOTAL ttReportTable.tf_additional-charge2
            ttReportTable.tf_actual-payment     = ttReportTable.tf_actual-payment     + ACCUM TOTAL ttReportTable.tf_actual-payment
         .
      
         IF ttReportTable.tf_id EQ 1 THEN
         DO:
			/* fullKasko = ttReportTable.tf_actual-payment / 2.*/
             ttReportTable.tf_sum-payment = ttReportTable.tf_sum-payment + 
                                            ttReportTable.tf_additional-charge1 + 
                                            ttReportTable.tf_additional-charge2.
             ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ttReportTable.tf_sum-percent.
			 /* if rko11_price <> 0 and myMonth = month(ttReportTable.tf_payment-date) then */
			 if rko11_price <> 0 and countPlat = 12 then do:
				ttReportTable.tf_actual-payment = fullKasko / rko11_price * pred_tf_rest-debts.
				countPlat = 0.
			end.
			 else 
				ttReportTable.tf_actual-payment = 0.
             ttReportTable.tf_sum-percent = 0.
			 pred_tf_rest-debts = ttReportTable.tf_rest-debts.
         END.
         else 
         DO:
			 /* if rko11_price <> 0 and myMonth = month(ttReportTable.tf_payment-date) and myLastDate <> ttReportTable.tf_payment-date then */
			 if rko11_price <> 0 and countPlat = 12 and myLastDate <> ttReportTable.tf_payment-date then do:
			 	ttReportTable.tf_actual-payment = fullKasko / rko11_price * pred_tf_rest-debts.
				countPlat = 0.
			end.
			 else 
				ttReportTable.tf_actual-payment = 0.
			 ttReportTable.tf_additional-charge2 = 0.
			 ttReportTable.tf_additional-charge1 = 0.
			 pred_tf_rest-debts = ttReportTable.tf_rest-debts.
         END.

      END.
   END.
end. /* if loan.cont-type = '���+�' or loan.cont-type = '���+�' */

else 
do:
/* IF CAN-DO("Avto*,���*",loan.cont-type) and loan.cont-type <> '���+�' and loan.cont-type <> '���+�' THEN  */
if typeTable = '1' then
DO:
   SUMM:
   FOR EACH ttReportTable BY ttReportTable.tf_payment-date:
   
      IF ttReportTable.tf_id EQ 0 THEN
      DO:
         ACCUMULATE ttReportTable.tf_additional-charge1 (TOTAL). /* 4,400. �㬬� �������⥫��� �ॡ������, �����ᨨ */
         ACCUMULATE ttReportTable.tf_additional-charge2 (TOTAL).
         ACCUMULATE ttReportTable.tf_actual-payment (TOTAL).     /* 5. �㬬� ���客�� ����ᮢ */
      
         ASSIGN
             ttReportTable.tf_additional-charge1 = 0
             ttReportTable.tf_additional-charge2 = 0
         .
         DELETE ttReportTable.
      END.
      ELSE
      DO:
         ASSIGN
            ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ACCUM TOTAL ttReportTable.tf_additional-charge1
            ttReportTable.tf_additional-charge2 = ttReportTable.tf_additional-charge2 + ACCUM TOTAL ttReportTable.tf_additional-charge2
            ttReportTable.tf_actual-payment     = ttReportTable.tf_actual-payment     + ACCUM TOTAL ttReportTable.tf_actual-payment
            ttReportTable.tf_actual-payment     = 0. 
         .
      
         IF ttReportTable.tf_id EQ 1 THEN
         DO:
/* message 'ttReportTable.tf_sum-payment ' string(ttReportTable.tf_sum-payment) +
		 ' ttReportTable.tf_additional-charge1 ' string(ttReportTable.tf_additional-charge1) view-as alert-box. */
             ttReportTable.tf_sum-payment = ttReportTable.tf_sum-payment + 
                                            ttReportTable.tf_additional-charge1 + 
                                            ttReportTable.tf_additional-charge2.
             ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ttReportTable.tf_sum-percent.
             ttReportTable.tf_sum-percent = 0.
         END.
         
         LEAVE SUMM.
      END.
   END.
END.
 /* run instview.p(TEMP-TABLE ttReportTable:HANDLE). */
/* IF CAN-DO("M�+(���)",loan.cont-type) THEN  */
if typeTable = '3' then
DO:
   SUMM:
   FOR EACH ttReportTable BY ttReportTable.tf_payment-date:  
      IF ttReportTable.tf_id EQ 0 THEN
      DO:
		pred_tf_rest-debts = ttReportTable.tf_additional-charge1.
         DELETE ttReportTable.
      END.
      ELSE
      DO:
         IF ttReportTable.tf_id EQ 1 THEN
         DO:
             ttReportTable.tf_additional-charge1 = pred_tf_rest-debts.
         END.
	  END.
   END.
END.
end. /* if loan.cont-type = '���+�' or loan.cont-type = '���+�'  ... else do: */

   /* �⫠��筠� ���ଠ�� */
IF SESSION:DEBUG-ALERT THEN DO:
   OUTPUT STREAM vStream CLOSE.
END.

RUN DeleteOldDataProtocol IN h_base ("������� �� ��������� �����").
RUN DeleteOldDataProtocol IN h_base ("������������� �� �������� �����").

{intrface.del}

/*
FUNCTION FindNextWorkDate RETURNS date
   (iDay AS Decimal,
    iDate AS DATE):
	def var nextDate as date no-undo.
	nextDate = date_correct(Month(iDate),1,iDay,Year(iDate)).
	DO WHILE holiday(nextDate):
      nextDate = nextDate - 1.
	END.
	return nextDate.
END FUNCTION.
*/









