/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: MYCOUNT2.P
      Comment: ������ ���ﭨ� ������� ���।
   Parameters: ⨯ �������, ��� �������, ��� ������
         Uses:
      Used by:
      Created: ??/??/???? ???
     Modified: ���� 25.07.2002
     Modified: ���� 08.10.2002
     Modified: ���� 31.10.2002 ⥯��� 13 ��ࠬ��஢
     Modified: ���� 03.12.2002 ��७�� �� VTB
     Modified: ���� 17.12.2003 23075
     Modified: Fepa  16.03.2004 26698
     Modified: ���� 09.10.2004 - ������ ��।���� (�ࠢ������ � �।��騬�
                                ����ﬨ ��ᯮ�����, � rat-dov.i ⥬ �����)
                                - �모�� �� ��譥�
     Modified: jadv  07.11.2007 (0061310) - ���ꥬ �� ᯥ梥�ᨨ
*/

/*&GLOB PRINT_LOG YES*/

{globals.i}
{done}
{intrface.get rights} /* ����㧪� �����㬥���� �� �ࠢ��         */
{intrface.get xclass} /* �����㬥��� ��� ࠡ��� � ����奬��       */
{intrface.get loan}   /* �����㬥��� �� �।�⠬                   */
{intrface.get lv}     /* �����㬥��� ��� loan-ind & loan-var       */
{intrface.get date}   /* �����㬥��� ��� ࠡ��� � ��⠬�           */
{intrface.get db2l}   /* �����㬥��� ���  �������᪮� ࠡ��� � �� */
{intrface.get pogcr}  /* �����㬥��� ��� ࠡ��� � ���. ���. � ��� */
{intrface.get tmess}  /* �����㬥�� ��ࠡ�⪨ ᮮ�饭��. */

{svarloan.def}
{t-otch.i NEW}
{loan-ost-p.i}        /* ����७��� ��楤��� � ᮧ������ ����権 */

DEF INPUT PARAM iContract AS CHAR NO-UNDO.
DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
DEF INPUT PARAM iDob      AS DATE NO-UNDO.

DEF VAR mCurDate     AS DATE  NO-UNDO.
DEF VAR mErrors      AS INT64   NO-UNDO.
DEF VAR mCurErr      AS INT64   NO-UNDO.
DEF VAR mS           AS DEC   NO-UNDO.
DEF VAR mSumma24     AS DEC   NO-UNDO.
DEF VAR mSumma29     AS DEC   NO-UNDO.
DEF VAR mSumma48     AS DEC   NO-UNDO.
DEF VAR mSumma23     AS DEC   NO-UNDO.
DEF VAR mSumPr1      AS DEC   NO-UNDO.
DEF VAR mCondAvail   AS LOG   NO-UNDO.
DEF VAR mCondRec     AS RECID NO-UNDO.
DEF VAR mDelay       AS INT64   NO-UNDO.
DEF VAR mDelay1      AS INT64   NO-UNDO.
DEF VAR mDischType   AS INT64   NO-UNDO.
DEF VAR mDatPer      AS DATE  NO-UNDO.
DEF VAR mProcName    AS CHAR  NO-UNDO.
DEF VAR mRound       AS LOG   NO-UNDO.
DEF VAR mClassCode   AS CHAR  NO-UNDO.
DEF VAR mSince       AS DATE  NO-UNDO.
DEF VAR mAvtProc     AS CHAR  NO-UNDO. /*���祭�� ��ࠬ��� "���%%"*/
DEF VAR mAvtProcPen  AS CHAR  NO-UNDO. /*���祭�� ��ࠬ��� "���%%��"*/
DEF VAR mExt         AS CHAR  NO-UNDO. /*���祭�� ��ࠬ��� "�����"*/
DEF VAR mSumStr      AS CHAR  NO-UNDO.
DEF VAR mSumStr1     AS CHAR  NO-UNDO.
DEF VAR mTypeCond    AS LOG   NO-UNDO.
DEF VAR mSurr        AS CHAR  NO-UNDO.
DEF VAR mNextPlan    AS LOG   NO-UNDO.
DEF VAR vSummaPogPr  AS DEC   NO-UNDO.
DEF VAR mIsOk        AS LOG   NO-UNDO. /* �ਧ��� �ᯥ譮�� �����. ��䨪� �����ᨩ */
DEF VAR vAmt1        AS DEC   NO-UNDO.
DEF VAR mKompPogr    AS CHAR  NO-UNDO.
DEF VAR mMoveRound   AS DATE  NO-UNDO.
DEF VAR vdateRasch   AS DATE  NO-UNDO.
DEF VAR mAmtId       AS INT64 NO-UNDO.
DEF VAR mContType    AS CHAR  NO-UNDO.
DEF VAR mDateN       AS DATE  NO-UNDO.
DEF VAR mDateM       AS DATE  NO-UNDO.
DEF VAR mFlRecalc    AS CHAR  NO-UNDO.

DEF BUFFER yloan-cond FOR loan-cond.
DEF BUFFER bloan FOR loan.

DEF TEMP-TABLE tt-move NO-UNDO
  FIELD mdate   AS DATE
  FIELD is-proc AS LOG
  FIELD is-pd   AS LOG
INDEX mdate mdate
.

DEF SHARED STREAM err .

{mycount.i}

FIND FIRST loan WHERE
           loan.contract  EQ iContract
       AND loan.cont-code EQ iContCode
NO-LOCK NO-ERROR.

&IF DEFINED(PRINT_LOG) &THEN

  DEF VAR mTimeVariableBeg  AS INT64 NO-UNDO.
  DEF VAR mTimeVariableDay  AS INT64 NO-UNDO.
  DEF VAR mTimeVariableEnd  AS INT64 NO-UNDO.
  DEF VAR mTimeVariable1    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable2    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable3    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable4    AS INT64 NO-UNDO.
  DEF VAR mTimeVariable    AS INT64 NO-UNDO.

  mTimeVariable = TIME.

&ENDIF

{ch_dat_proc.i}

ASSIGN
   mCurDate   = loan.since
   mClassCode = loan.class-code
   mSince     = loan.since
   mDatPer    = GetDatPer(iContract,iContCode)
   mProcName  = GetNachProc(iContract,iContCode)
   mRound     = GetRoundFlag(iContract,iContCode)
   mFormGrKom = FGetSetting("��䊮��","��ଃ����",?)
  .

/* ��।����� ���� ��४��祭�� �� ���㣫���� � ०�� ��� ����譮�� */
RUN MovRoundLoan IN h_loan (loan.contract,
                            loan.cont-code,
                            loan.class-code,
                            OUTPUT mMoveRound,
                            OUTPUT mKompPogr).

PUT STREAM err UNFORMATTED
   " "                                   SKIP
   "��������  �������� " +
    GetContCode(iContract,iContCode) + " �� " +
    STRING(iDob)                   AT 13 SKIP
  "����饭�� �ணࠬ�� ������:" AT 13 SKIP.

IF AVAIL loan
   AND loan.close-date NE ?
   AND loan.close-date NE (idob - 1)
   AND loan.since      NE idob
THEN DO:
   PUT STREAM err UNFORMATTED
      "������� " + GetContCode(loan.contract,loan.cont-code) + " ������" AT 13 SKIP.
   RETURN.
END.

IF NOT CAN-DO (GetRightClasses (mClassCode, "W"), mClassCode)
THEN DO:
   PUT STREAM err UNFORMATTED
      "�� �� ����� �ࠢ� �������� ��ꥪ�� ������� ����� '" +
      mClassCode + "'" AT 13 SKIP.
   {intrface.del}
   RETURN.
END.

/*���뢠�� ����஥�� ��ࠬ����*/
ASSIGN
   mErrors       = INT64(FGetSetting("�����",?,"0"))
   mAvtProc      = FGetSetting("���%%",?,"��")
   mAvtProcPen   = FGetSetting("���%%��",?,"��")
   mExt          = FGetSetting("�����",?,"")
   mDateN        = DATE(FGetSettingEx("��⠍��।", ?, "", NO))
   mDateN        = IF mDateN = ? THEN {&BQ-MIN-DATE} ELSE mDateN 
   .

RUN DeleteOldDataProtocol IN h_base ("�����멃�䨪���⪮�").
RUN DeleteOldDataProtocol IN h_base ("�����멃�䨪�����ᨩ").
RUN DeleteOldDataProtocol IN h_base ("��⠎��⪠").
RUN DeleteOldDataProtocol IN h_base ("�㬬����⪠").
RUN DeleteOldDataProtocol IN h_base ("�����⏏������").
RUN DeleteOldDataProtocol IN h_base ("��ࢠ��������").
RUN SetSysConf            IN h_Base ("�����℮�����","��").
RUN SetSysConf            IN h_base ("��⠏����⠄������",STRING(iDob)).


RUN RE_L_COND IN h_Loan(iContract,
                        iContCode,
                        mCurDate,
                        BUFFER loan-cond).
IF AVAIL loan-cond THEN
   ASSIGN
      mDischType = loan-cond.disch-type
      .

/*
  ��।��塞 �� �������� ���� �������� �� �������� � 㪠������ ���ࢠ��
*/

IF mCurDate <> iDob THEN
   RUN PrepareLoanMove(iContract,
                       iContCode,
                       mCurDate,
                       iDob,
                       mClassCode).

CH:
DO WHILE mCurDate < iDob ON ERROR  UNDO, RETURN
                         ON ENDKEY UNDO, RETURN :
   RELEASE tt-move.

   FIND FIRST tt-move  WHERE tt-move.mdate = mCurDate NO-ERROR.
   IF NOT AVAIL tt-move
   THEN DO:
      mCurDate = mCurDate + 1.
      NEXT CH.
   END.

   RELEASE loan-cond.

   RUN RE_L_COND IN h_Loan(iContract,
                           iContCode,
                           mCurDate,
                           BUFFER loan-cond).
   IF AVAIL loan-cond THEN
      ASSIGN
         mCondAvail = YES
         mDelay     = loan-cond.delay
         mDelay1    = loan-cond.delay1
         mDischType = loan-cond.disch-type
         mCondRec   = RECID(loan-cond)
         .
   ELSE
      ASSIGN
         mCondAvail = NO
         mDelay     = 0
         mDelay1    = 0
         mDischType = 1
         .

   IF AVAILABLE loan-cond THEN
   DO:
      /*- �������� �� ��� �����������  ����� !!!(���� ��������������) -*/
      ASSIGN
         mSurr     = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE))
         mTypeCond = GetXAttrValue("loan-cond",mSurr,"�奬�����") = "�����⭠�"
         .

      mNextPlan = ProbegNextPlan(loan-cond.class-code, mSurr).
  /* ����� ����� �� �㤥� */
      IF mTypeCond THEN
      DO:

         FIND LAST term-obl WHERE
                   term-obl.contract  = iContract
               AND term-obl.cont-code = iContCode
               AND term-obl.idnt      = 1
               AND term-obl.end-date  = mCurDate NO-LOCK NO-ERROR.
         IF AVAIL term-obl THEN
         DO:
            RELEASE term-obl.
     /* �᫨ �� ��� ��� �������� �� ���� ��������� ���⥦� � � ���
      �����⭠� �奬� - ��業�� ��⠥� �� ࠧ� - �� ���� - �⮡� ���४⭮
      ��।�����  �� ������� �㬬�, ��祬 ��業�� �������
      ���ࠢ��쭮 � �������⥫�묨 �᪠�� ࠧ������ �� ������ ���᫥���
      ��業⮢. �� �ਢ���� � �訡��� ���㣫���� ���� ��⠢�塞 - ������㥬
      � Nachislenieandoplata*/
             /* ����砥� ���� ��砫� ������� ������஢ � ��⥬� */
             mDateM = DATE(FGetSettingMF("����⠑���",?,?,loan.filial-id, no)).
             /* �᫨ ������� �����஢����, �஢��塞 �� */
             IF mDateM GE loan.open-date THEN
             DO:
                mFlRecalc = FGetSettingMF("����猨����",?,?,loan.filial-id, no).
                /* �᫨ �� ������뢠��, � �஢��塞, ���� �� �᫮��� ��᫥ ���� ����� */
                IF mFlRecalc EQ "���" THEN
                DO:
                   FIND FIRST yloan-cond WHERE yloan-cond.contract  EQ loan.contract
                                           AND yloan-cond.cont-code EQ loan.cont-code
                                           AND yloan-cond.since     GT mDateM
                   NO-LOCK NO-ERROR.
                   /* ������뢠�� � ���� �᫮��� */
                   IF AVAIL yloan-cond THEN
                   DO:
                      IF yloan-cond.since GT loan-cond.since THEN
                         RUN ps_rec.p(MAX(yloan-cond.since,mCurDate),
                                      1, 
                                      BUFFER loan, 
                                      BUFFER yloan-cond).
                      ELSE
                         RUN ps_rec.p(MAX(yloan-cond.since,mCurDate),
                                      1, 
                                      BUFFER loan, 
                                      BUFFER loan-cond).
                   END.
                END.
                ELSE
                   RUN ps_rec.p(mCurDate,1, BUFFER loan, BUFFER loan-cond).
             END.
             ELSE
                RUN ps_rec.p(mCurDate,1, BUFFER loan, BUFFER loan-cond).
         END.

      END.
   END.

   /*===== �᭮���� ���� ������ ������� ================*/
   /* ������ �஢���� */
   FOR EACH loan-int WHERE
            loan-int.contract  = iContract
        AND loan-int.cont-code = iContCode
        AND loan-int.mdate     = mCurDate
   NO-LOCK:

      IF (loan-int.id-d = 30 AND loan-int.id-k = 29) OR
         (loan-int.id-d = 30 AND loan-int.id-k = 48) OR
         (loan-int.id-d = 29 AND loan-int.id-k = 31) OR
         (loan-int.id-d = 10 AND loan-int.id-k = 24) OR
         (loan-int.id-d = 33 AND loan-int.id-k = 24) OR
         (loan-int.id-d = 704 AND loan-int.id-k = 5) OR
         (loan-int.id-d = 200 AND loan-int.id-k = 5)
      THEN NEXT.

      RUN Cr_LoanVar IN h_lv (iContract,iContCode,mCurDate,ROWID(loan-int)).
   END.

      /* ���� ��易⥫��⢠ */
   RUN loan-ost-p IN this-procedure
        (iContract,
                  iContCode,
                  mCurDate,
                  mCondRec).
   /*
   �����뢠�� ⥪�騥 ��業�� 1 ࠧ,� ��⠫��� ��楤��� ����
   ��楯�塞 �㦭�� �㬬� � ���뢠�� �� � ᯨ᪥ mSumStr
   */

   mSumStr = FILL(CHR(1),EXTENT(pick-var) - 1).
  /* ��� �����⭮� �奬� - �� ���� �窠 ���� ��業⮢ */
   IF tt-move.is-proc THEN
   DO:
      /* ��� ���४⭮�� ���� ���ﭨ� ������஢ � ᯥ�. �奬�� ���᫥��� (��_1,��_17) */
      RUN SetSysConf IN h_base ("calc-loan-state","yes").

      vdateRasch = GetDateClc (tt-move.is-pd, mDischType,  mCurDate  ).
      RUN nach-pr_.p
         (iContract,        /* �����䨪���                         */
          iContCode,        /* �������                              */
          vdateRasch,       /* ��� ���� ��業⮢                */
          ?,                /* �� ��ࠬ����                         */
          "",               /* ..���� �� �� �뭮ᨬ �� �ॡ������   */
          mDatPer,          /* ��� ���室� �� 39�                  */
          mProcName,        /* ��楤�� ���᫥���                  */
          NO,               /* �� ��࠭��� loan.interest            */
          OUTPUT mS,        /* �㬬� �� ��᫥����� ��ࠬ����         */
          OUTPUT mSumStr).  /* ��ப� ⥪��� ��業⮢ �१ chr(1) */

      /* � ᢧ� � ⥬, �� �� ���� pint.p ��� 11/36 ��� ���� ��� ᮢ��襭�� ��࠭�� ࠧ������
      ** �� ��ਮ�� ������⢠, ��������� ����ୠ� �㬬� ��� ���४�஢��. �㤥� ࠧ�������
      ** � �⨬ �⤥�쭮. ���� ��� ⠪� �⪫�稬 ��������� */
      IF AVAIL loan-cond
         AND NOT (loan-cond.disch-type EQ 11 OR loan-cond.disch-type EQ 36)
         AND (mMoveRound LE vdateRasch)
         AND (mKompPogr EQ "��"
                      OR CAN-FIND(FIRST term-obl WHERE term-obl.contract  EQ iContract
                                       AND term-obl.cont-code EQ iContCode
                                       AND term-obl.end-date  EQ mCurDate
                                       AND term-obl.idnt      EQ 1)
              ) THEN
      DO:
         RUN mycount2-corr.p (
            iContract,
            iContCode,
            vdateRasch,
            OUTPUT vAmt1).
         ENTRY(1, mSumStr, CHR(1)) = STRING(vAmt1).
      END.

      /*}}}*/

   END.
   /* ���������� %% */
   mSumStr1 = mSumStr.

   /*
   C��砫� ��ࠡ��뢠�� ���᫥��� �� ��������� 29 > 24 - ����砥�, ��
   �஬� ᯨᠭ���ࠦ����� %% ᯨ�뢠���� ���᫥���, �� ����ࠦ����
   */
   RUN bal-o-p (iContract,             /* �����䨪���       */
               iContCode,             /* �������            */
               mCurDate,              /* ���                */
               mExt,                  /* �� �ॡ������       */
               mRound,                /* ���㣫���           */
               INPUT-OUTPUT mSumStr). /* ��ப� � ⥪�騬� % */
   /*
   ������ ᮧ���� ���⪨ �� ���᫥��� % �� ���������
   */
   {crvint.i &cur-date = mCurDate &id-d = 29 &id-k = 31}
   /*
   ...� ���᫥��� �஢����� �� ���������� ��ࠢ������ ���⪨ 29+48 � 24
   */
   RUN bal-o1-p (iContract,  /* �����䨪��� */
                iContCode,  /* �������      */
                mCurDate).  /* ���          */
   /*
   �����᪠ �।�� �� ���᫥��� ��業⮢ �� ���������
   */

   RUN nach-pro-p (iContract,              /* �����䨪���       */
                  iContCode,              /* �������            */
                  mCurDate,               /* ���                */
                  mExt,                   /* �� �ॡ������       */
                  mRound,                 /* ���㣫���           */
                  INPUT-OUTPUT mSumStr).  /* ��ப� � ⥪�騬� % */
   /*
   ���᫥�� ��業⮢ �� �������.
   �᫨ ���� ������ ����塞 � �㬬� ����樨 ࠧ��ᨬ
   */
   RUN nach-prb-p (iContract,              /* �����䨪���       */
                  iContCode,              /* �������            */
                  mCurDate,               /* ���                */
                  mExt,                   /* �� �ॡ������       */
                  mRound,                 /* ���㣫���           */
                  INPUT-OUTPUT mSumStr).  /* ��ப� � ⥪�騬� % */

   /* ���뢠�� ᯨᠭ�� �।�� � ��������� */
   {crvint.i &cur-date = mCurDate &id-d = 30 &id-k = 29}
   /*����� �뭥ᥭ��� �� ������ ��業⮢*/
   {crvint.i &cur-date = mCurDate &id-d = 30 &id-k = 48}

   {crvint.i &cur-date = mCurDate &id-d = 10 &id-k = 24}
   {crvint.i &cur-date = mCurDate &id-d = 33 &id-k = 24}

   /* ������ ࠧ���� 4007 � 4020 ����樨 */
   RUN opl_buypr-p (iContract,iContCode,mCurDate,mRound,mAvtProc) .

   /* ������� ����祭�� ��業�� �� ���᭥��� ,
      �᫨ ���⮪ �� ��ࠬ���� 24 > 29  + 48 */
   ASSIGN
      mSumma24 = LastVarSumm(iContract,iContCode,24,mCurDate)
      mSumma29 = LastVarSumm(iContract,iContCode,29,mCurDate)
      mSumma48 = LastVarSumm(iContract,iContCode,48,mCurDate)
      mSumma23 = LastVarSumm(iContract,iContCode,23,mCurDate)
      .
   IF mSumma24 <> ? THEN
   DO:
      mSumma24 = mSumma24 - (IF mSumma29 <> ? THEN mSumma29 ELSE 0)
                          - (IF mSumma48 <> ? THEN mSumma48 ELSE 0)
                          - (IF mSumma23 <> ? THEN mSumma23 ELSE 0).
      IF mSumma24 > 0 THEN
         RUN Cr_LoanInt IN h_lv
           (iContract,      /* �����䨪���   */
            iContCode,      /* �������        */
            mCurDate,       /* ��� ����樨   */
            mSumma24,       /* ��㬬� ����樨 */
            16,             /* �. ���᫥��    */
            24,             /* �. ���ᠭ�      */
            YES,            /* ���. ������   */
            ?).             /*                 */
   END.

   IF NOT AVAIL(loan-cond) THEN
   DO:
      mCurDate = mCurDate + 1.
      NEXT CH.
   END.

   /* �᫨ ���� ���⮪ �� ����� ���᫥���� ��業⮢ */
      RUN OplataTrebovaniy(iContract,iContCode,mCurDate).

     /*
   �᫨ ����祭� > 0 ᮧ���� loan-int ����祭� �� ⥬ %% ���. > 0 ???
   */
   RUN opl-pr-p(iContract,
                iContCode,
                mCurDate,
                mRound,
                mAvtProc,
                mAvtProcPen,
                mSumStr1).
   /*
   �᫨ ����祭� > 0 � ���� ⥪�騥 ��業�� (� �� �� �� ���᫥��),
   � ᮧ���� ����樨 ���᫥��
   */
   /* � ��� � ����� �᪠ ���� ��業⮢ ��� �����⭮� �奬� - �����
      �����⠥� ������� ���⥦ ��業⮢ */

   RUN NachislenieAndOplata(iContract,
                            iContCode,
                            mCurDate,
                            mSumStr,
                            mDischType,
                            mRound,
                            AVAIL loan-cond AND loan-cond.since = mCurDate).

      RUN DeleteOldDataProtocol IN h_base("calc-loan-state").
   /*
   �⭥ᥭ�� ��業⮢ �� �����᭥���
   */
   {rat-dov.i}
   FIND FIRST loan-var WHERE
            loan-var.contract  = iContract
        AND loan-var.cont-code = iContCode        
   NO-LOCK NO-ERROR .
   DO WHILE AVAIL loan-var :
      mAmtId = loan-var.amt-id .
      FOR EACH loan-var WHERE 
            loan-var.contract  = iContract
        AND loan-var.cont-code = iContCode
        AND loan-var.amt-id    = mAmtId
        AND loan-var.since     = mCurDate NO-LOCK :
         RUN "error(lp.p"(RECID(loan-var),mCurDate,INPUT-OUTPUT mCurErr).
         IF mCurErr > mErrors THEN
         DO:
            PUT STREAM err UNFORMATTED
                "������⢮ �訡�� �ॢ�ᨫ� �����⨬�� - " +
                STRING(mErrors)  +  " . ������ �����祭" AT 4 SKIP.
            UNDO ch , LEAVE ch .
         END.
      END.   
      FIND FIRST loan-var WHERE
            loan-var.contract  = iContract
        AND loan-var.cont-code = iContCode
        AND loan-var.amt-id    > mAmtId        
      NO-LOCK NO-ERROR .

   END.

   RUN res-o-p (iContract,
               iContCode,
               mCurDate).

   mCurDate = mCurDate + 1.
END.  /* DO WHILE */

/* ��� ���४⭮�� ���� ���ﭨ� ������஢ � ᯥ�. �奬�� ���᫥��� (��_1,��_17) */
RUN SetSysConf IN h_base ("calc-loan-state","yes").
/*�饬 �������� �� ��業⠬ �� ���� ����*/
FIND FIRST tt-move  WHERE tt-move.mdate = mCurDate AND tt-move.is-proc = YES NO-ERROR.

IF NOT AVAIL loan-cond THEN

   RUN RE_L_COND IN h_Loan(iContract,
                           iContCode,
                           mCurDate,
                           BUFFER loan-cond).
   IF AVAIL loan-cond THEN
      ASSIGN
         mDischType = loan-cond.disch-type
         .
   ELSE
      ASSIGN
         mDischType = 1
         .

IF AVAIL tt-move THEN
    vdateRasch = GetDateClc (tt-move.is-pd, mDischType, mCurDate).
ELSE
   vdateRasch = mCurDate.

RUN nach-pr_.p
      (iContract,        /* �����䨪���                         */
       iContCode,        /* �������                              */
       vdateRasch,       /* ��� ���� ��業⮢                */
       ?,                /* �� ��ࠬ����                         */
       "",               /* ..���� �� �� �뭮ᨬ �� �ॡ������   */
       mDatPer,          /* ��� ���室� �� 39�                  */
       mProcName,        /* ��楤�� ���᫥���                  */
       YES,              /* ��࠭��� loan.interest               */
       OUTPUT mS,        /* �㬬� �� ��᫥����� ��ࠬ����         */
       OUTPUT mSumStr).  /* ��ப� ⥪��� ��業⮢ �१ chr(1) */

IF NOT AVAIL loan-cond  THEN
   RUN RE_L_COND IN h_Loan(iContract,
                           iContCode,
                           mCurDate,
                           BUFFER loan-cond).

mDischType = IF AVAIL loan-cond THEN loan-cond.disch-type ELSE mDischType.

/* � ᢧ� � ⥬, �� �� ���� pint.p ��� 11/36 ��� ���� ��� ᮢ��襭�� ��࠭�� ࠧ������
** �� ��ਮ�� ������⢠, ��������� ����ୠ� �㬬� ��� ���४�஢��. �㤥� ࠧ�������
** � �⨬ �⤥�쭮. ���� ��� ⠪� �⪫�稬 ��������� */
IF AVAIL loan-cond
   AND NOT (   loan-cond.disch-type EQ 11
            OR loan-cond.disch-type EQ 17
            OR loan-cond.disch-type EQ 36)
   AND (mMoveRound LE vdateRasch)
   AND (  mKompPogr EQ "��"
               OR CAN-FIND(FIRST term-obl WHERE term-obl.contract  EQ iContract
                                            AND term-obl.cont-code EQ iContCode
                                            AND term-obl.end-date  EQ mCurDate
                                            AND term-obl.idnt      EQ 1)
      ) THEN
   RUN mycount2-corr.p (
      iContract,
      iContCode,
      vdateRasch,
      OUTPUT loan.interest[1]).

/*���४�஢�� 9 � 12 ��ࠬ��஢ 0168696*/
RUN corr-par.p ( iContract,
                  iContCode,
                  vDateRasch,
                  mSumStr,
                  9,
                  OUTPUT loan.interest[3]).

RUN corr-par.p ( iContract,
                  iContCode,
                  vDateRasch,
                  mSumStr,
                  12,
                  OUTPUT loan.interest[5]).

/* ���줨஢���� %% � ���� ����� */
RUN AddAmtProc IN THIS-PROCEDURE(iContract,iContCode,mCurDate,loan.Class-Code).

RUN DeleteOldDataProtocol IN h_base("calc-loan-state").
IF GetXattrInit(loan.Class-Code,"�奬��犮�") NE "" THEN
DO:
   /* ����� �������㠫��� �����ᨩ */
   RUN nach-ind.p (iContract,              /* �����䨪���  */
                   iContCode,              /* �������       */
                   loan.since,             /* ��� ��砫�    */
                   iDob,                   /* ��� ����砭�� */
                   mRound,                 /* ���㣫���      */
                   FALSE).                 /* �� ������ ����� */

   /* ���� �����ᨩ �뭥ᥭ��� �� ������  */
   RUN nachpros.p(iContract,
                  iContCode,
                  iDob).

END.

/* ����� ���. ��ࠬ��஢ ᤥ��� ��� ���᫥���, ⠪ � �뭮ᮢ �� ������ */
IF loan.contract EQ "�।��" OR CAN-DO (Ls-Class("loan_ces"),loan.class-code) THEN
   RUN calcaijk.p (loan.contract,
                   loan.cont-code,
                   iDob).

/* �����⠥� ��䨪 �����ᨩ  */
IF mFormGrKom EQ "��" THEN
   RUN CALC_PLAN_COMM (loan.contract,loan.cont-code,loan.open-date,loan.end-date,OUTPUT mIsOk).

RUN DeleteOldDataProtocol IN h_base("�����멃�䨪���⪮�").
RUN DeleteOldDataProtocol IN h_base("�����멃�䨪�����ᨩ").
RUN DeleteOldDataProtocol IN h_base("��⠎��⪠").
RUN DeleteOldDataProtocol IN h_base("�㬬����⪠").
RUN DeleteOldDataProtocol IN h_base("�����⏏������").
RUN DeleteOldDataProtocol IN h_base("��ࢠ��������").
RUN DeleteOldDataProtocol IN h_Base("�����℮�����").
RUN DeleteOldDataProtocol IN h_Base("��⠏����⠄������").

&IF DEFINED(PRINT_LOG) &THEN
  IF  (TIME - mTimeVariable) >= 2 THEN
  DO:
     OUTPUT TO "mycount_.log" APPEND.
     PUT UNFORMATTED
        "������ ������� - " iContCode " c " mSince " �� " iDob
     " - " STRING(TIME - mTimeVariable,"hh:mm:ss")  " " TODAY SKIP.
     OUTPUT CLOSE.
  END.
&ENDIF

FIND FIRST bloan WHERE ROWID(bloan) EQ ROWID(loan) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF NOT AVAIL bloan THEN
DO:
   RUN wholocks2.p ( RECID(loan),"loan" ,
                     program-name(1) +
                     '~n �訡�� �� �������쭮� ����㯥 � �������� ' +
                     ENTRY(1,loan.cont-code," ")).
   UNDo, RETURN.
END.
ASSIGN bloan.l-int-date = bloan.since. /* ����室��� ��࠭��� �।����� ���� ������ */

IF mCurErr <> 0
THEN DO:
   PUT STREAM err UNFORMATTED
       "�����㦥�� - " + STRING(mCurErr) + " �訡��"  AT 13 SKIP
       "������� �����⠭ �� " + STRING(mCurDate)    AT 13 SKIP.
   bloan.since = mCurDate.
END.
ELSE DO:
   PUT STREAM err UNFORMATTED
      "�訡�� �� �����㦥��"                  AT 13 SKIP
      "������� �����⠭ �� " + STRING(iDob) AT 13 SKIP.
   bloan.since = iDob.
END.
/* ����饭�� �� ������ ��䨪� �����ᨩ */
IF NOT mIsOk THEN
   PUT STREAM err UNFORMATTED "��䨪 �����ᨩ �� �����⠭." AT 13 SKIP.
{intrface.del}
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='14/05/2015 15:58:51.658+04:00' */
/* $LINTUSER='priv' */
/* $LINTMODE='1' */
/* $LINTFILE='mycount2.p' */
/*prosignxtILBNfjv5RvM63Y7wQGVw*/