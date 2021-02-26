/* ��ࠡ�⪠ �����ᨩ "����" (�������਩ �� �ਬ��� "������") */
   
/* ����� ���⪠ �� ��ࠬ���� �� �� Interest */
   /* ��।���� �� ����� interest ��࠭���� ���� (���� ���� �� ����ன��) */


mVar205  = INT64(ENTRY(mI, mIdk)).
mPenyPar = INT64(GetCodeMisc("�奬��犮�", 
                           ENTRY(mI, mCommSpec) + ":" + mSchemCls, 
                           2)) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
   mPenyPar = 0.

ASSIGN
   mSummDob = 0
   mDateEnd = mDateEnd - 1
.
   /* �᫨ ������ ����� */
IF iCalcBack THEN
   ASSIGN
      mDateBeg = loan.open-date  /* ��⠥� � ��砫� ����⢨� ������� */
   .
   /* �᫨ ������ ���। */
ELSE
   IF mPenyPar GT 10 THEN     /* �ﭥ� ����������� �㬬� � ��ࠬ��� */
      mSummDob = LoadPar(mPenyPar, loan.contract + "," + loan.cont-code).

   /* ����塞 ���� �� ��ਮ� */
RUN lncommsh.p (iContract,
                iContCode,
                ENTRY(mI, mCommSpec),
                mDateBeg,
                mDateEnd,
                NO).

   /* �㬬��㥬 �� ���᫥��� �� ��ਮ� ���� */
FOR EACH otch1:
   mSummDob = mSummDob + otch1.summ_pr.
END.

   /* �饬 ����樨 ������ ���� (827), �������騥 � ��ਮ� */
FOR EACH loan-int WHERE
         loan-int.contract  EQ iContract
   AND   loan-int.cont-code EQ iContCode
   AND   loan-int.id-d      EQ 5                     /* 5 */
   AND   loan-int.id-k      EQ mVar205   /* 205 */
   AND   loan-int.mdate     GE mDateBeg - 1  
   AND   loan-int.mdate     LE mDateEnd - 1
NO-LOCK:
   mSummDob = mSummDob - loan-int.amt-rub.
END.

   /* ���࠭塞 �� ��⠫��� � �� interest[mPenyPar] (interest[16] ��� 204) */
IF mPenyPar > 10 THEN
DO:
   mSummDob = (IF iRound THEN ROUND(mSummDob, 0) ELSE mSummDob).
   SavePar(mPenyPar, loan.contract + "," + loan.cont-code, STRING(mSummDob)).
END.
/* eof ����� ���⪠ �� ��ࠬ���� �� �� Interest */


/* ��ࠡ�⪠ ��⮬���᪨� ����権 */
   /* �᫨ ��⠥� �����... */
IF iCalcBack THEN
DO:
      /* �᫨ ��⠥� �����, � 㤠�塞 ��⮬���᪨� ����樨 826 (204 - 5) */
   RUN DelAutoOpers (iContract,
                     iContCode,
                     iBegDate,
                     INT64(ENTRY(mI, mIddk)),  /* 204 */
                     5).
      /* �᫨ ��⠥� �����, � 㤠�塞 ��⮬���᪨� ����樨 828 (205 - 204) */
   RUN DelAutoOpers (iContract,
                     iContCode,
                     iBegDate,
                     mVar205,    /* 205 */
                     INT64(ENTRY(mI, mIddk))).  /* 204 */
END.
ELSE
DO:
      /* �饬 ����樨 ������ ���� (827), �������騥 � ��ਮ� */
   FOR EACH loan-int WHERE
            loan-int.contract  EQ iContract
      AND   loan-int.cont-code EQ iContCode
      AND   loan-int.id-d      EQ 5                     /* 5 */
      AND   loan-int.id-k      EQ mVar205  /* 205 */
      AND   loan-int.mdate     GE iBegDate - 1
      AND   loan-int.mdate     LE iEndDate - 1
   NO-LOCK:
         /* ������� ���.������ �� ���᫥��� ���� (826) */
      RUN CreateAutoOpers (iContract,
                           iContCode,
                           loan-int.mdate,
                           loan-int.amt-rub,
                           INT64(ENTRY(mI, mIddk)),   /* 204 */
                           5).
         /* ������� ���.������ �� ���᫥��� ���� (828) */
      RUN CreateAutoOpers (iContract,
                           iContCode,
                           loan-int.mdate,
                           loan-int.amt-rub,
                           mVar205,                     /* 205 */
                           INT64(ENTRY(mI, mIddk))).    /* 204 */
   END.
END.
/* eof ��ࠡ�⪠ ��⮬���᪨� ����権 */
