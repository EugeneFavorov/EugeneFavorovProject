
/*
               ������᪠� ��⥣�஢����� ��⥬� QBIS
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-pqres.p
     Comment: ����� �������� ᯨᠭ�� १�ࢠ ��� ���⭮��
              !!! ������� ������ ���� ������� �� ���� �������
                  ���� ࠢ��� ��� ����
  Parameters: no paramaters
         Uses:
      Used by:
     Created: ����
    Modified: ����
    Modified: ����� ����
    Modified: 27-apr-2005 mitr (0045006)
    Modified: jadv 09.11.2007 (0034598)
*/
 
/*


F/P Name                Comment
��� ������������������  ����������������������������������������������������

  P CLC_LOAN_PLAN_RES - ����� ��������� ������� ������� ���������
                        �������� ������� �� ��������

  P PREPARE_SPIS_DOLG �  PREPARE_SPIS_DOLG2
                      - ���������� �������� ��������
                        ��������� ����� �� ��������

  P CALC_RESERV       - ���������� �������� �������� ���������������
                        ������� �� ��������
  P SUMM_SPIS_RES     - ��������� ����� �������� ������� �� ������ ���
                        ������������� ������� �� ������ � ����������
  P DELETE_RES_SPIS   - �������� �������� �������� ������� �� ��������
  P SUMM_SPIS_DOLG    - ����� �������� ��������� ����� �� ������
  P GET_LOAN_PROCENT  - �㬬� ���᫥���� �� ��ਮ� %%
  F LastNachDate      - ��� ��᫥����� ���᫥��� % �� ������
  P GET_SUMMA_POG     - �㬬� ����襭�� �।�� �� ��ਮ�.
  P GET_SUMMA_DOLG_RUB - C㬬� ������������ � ������ � �㡫��
*/

/* pp-pqres.p */
{pfuncdef
   &DefLib="pqres" 
   &Description="����� �������� ᯨᠭ�� १�ࢠ ��� ����⭮��"
}
 
{pqres.def}

{globals.i}
{intrface.get xclass}
{intrface.get loan}
{intrface.get instrum}
{intrface.get i254}    /* ������⥪� ���᫥��� ������⥫��, �易���� � ���⮬ १�ࢠ �� ��㤠� (254-�). */
{intrface.get rsrv}    /* ������⥪� ��� ࠡ��� � ��ࠬ��ࠬ� १�ࢨ஢����. */
{intrface.get cdrep}
{intrface.get strng}
{intrface.get loanx}
{intrface.get comm}
{intrface.get acct}

/*{loan.pro} �� 㦥 ���� � {intrface.get loan} */
{lshpr.pro}

{svarloan.def NEW GLOBAL}
{t-otch.i NEW}
{sh-defs.i new}
{tt_getinfdogob.def}

&GLOB RES_PARAM 21
&GLOB SOGL_TYPE "��祭��"
&GLOB CONTRACT  "�।��"
&GLOB RES_IDNT  4
&GLOB iskldbparam "95"
/*&GLOB PqresLog YES*/

/* ����४�஢���� ���� �������� �믫�� */
DEF TEMP-TABLE ttPlan NO-UNDO
   FIELD CONT-CODE AS CHAR
   FIELD end-date AS DATE
   FIELD summa    AS DEC
   FIELD old-date AS DATE
INDEX end-date end-date
INDEX CONT-CODE END-DATE.


DEF TEMP-TABLE ttRes NO-UNDO
   FIELD end-date AS DATE
   FIELD summa    AS DEC
INDEX end-date end-date.

/* ��易⥫��⢠, ����� �㦭� ���뢠�� */
DEF TEMP-TABLE ttTerm NO-UNDO
   FIELD end-date AS DATE
INDEX end-date end-date.

DEF TEMP-TABLE ttLoanInt NO-UNDO
   FIELD cont-code AS CHAR
   FIELD mdate     AS DATE
INDEX indx1 mdate.

/* ��� �� ���஬� ����� ���⮪, ����� �㦭� ���뢠�� */
DEF TEMP-TABLE ttAcct NO-UNDO
   FIELD acct AS CHARACTER
INDEX acct acct.
/* */
DEF TEMP-TABLE tt-SumDolg NO-UNDO
   FIELD contract    AS CHARACTER
   FIELD cont-code   AS CHARACTER
   FIELD AcctType    AS CHARACTER
   FIELD since       AS DATE
   FIELD AllBasaOst  AS DECIMAL
   FIELD TranshCount AS INT64
INDEX idx1 contract cont-code AcctType since.

/* ⠡��� ��� ���᪠ �� �����䨪���� ����ࢄ�� */
DEF TEMP-TABLE tt-ReservDog NO-UNDO
   FIELD res_par   AS CHAR /* ��ࠬ��� १�ࢠ */
   FIELD res_sch   AS CHAR /* ��� १�ࢠ */
   FIELD plus_par  AS CHAR /* ��ࠬ���� �ॡ������ */
   FIELD minus_par AS CHAR /* ��ࠬ���� �ॡ������, ���������� */
INDEX idx1 res_par.

/* ⠡��� ��� ���஢���� ��।������ १�ࢠ � ����� �࠭蠬� */
DEF TEMP-TABLE tt-ResLoan NO-UNDO
   FIELD res_par   AS CHAR /* ��ࠬ��� १�ࢠ */
   FIELD cont-code AS CHAR /* ����� ������� */
   FIELD sum_tr    AS DEC  /* ��� १�ࢠ */
   FIELD sum_res   AS DEC  /* ��ࠬ���� �ॡ������ */
INDEX prim res_par cont-code.

DEF TEMP-TABLE tt-LoanTmp NO-UNDO
   FIELD id AS RECID                   /* �����䨪��� �����      */
   FIELD contract  AS CHARACTER
   FIELD cont-code AS CHARACTER
INDEX id id
INDEX cont contract cont-code.

/* ⠡��� ��� ���஢���� ��।������ ����⭮�� १�ࢠ � ����� �࠭蠬� */
DEF TEMP-TABLE tt-ResRasch NO-UNDO
   FIELD cont-code AS CHAR
   FIELD end-date  AS DATE
   FIELD summa     AS DEC
INDEX prim cont-code end-date.

/* ⠡��� ��� ��।������ ���ᯥ祭�� �� �࠭蠬 */
DEF TEMP-TABLE tt-GarRasch NO-UNDO
   FIELD cont-code AS CHAR
   FIELD end-date  AS DATE
   FIELD summa     AS DEC
   FIELD dolg      AS DEC
   FIELD vidob     AS CHAR
   FIELD KK        AS CHAR
   FIELD Qsumma    AS DEC
INDEX prim cont-code end-date KK.

DEFINE STREAM sOut.

DEFINE VARIABLE mFOiAKredRole AS CHARACTER NO-UNDO.  /* ���祭�� �� �����।���� */
DEFINE VARIABLE mParz1        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParz2        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParz3        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParz4        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp1        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp2        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp3        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mParp4        AS CHARACTER NO-UNDO .
DEFINE VARIABLE mAlgFIFO      AS LOG       NO-UNDO .
DEFINE VARIABLE mProcPar      AS CHARACTER NO-UNDO. /* ��ࠬ����, �� ����� ��।������ ����稥 ����窨 */
DEFINE VARIABLE mProcPar139   AS CHARACTER NO-UNDO. /* ��ࠬ����, �� ����� ��।������ ����稥 ����窨 */
DEFINE VARIABLE mDelay        AS LOGICAL   NO-UNDO. /* ��ࠬ���, ����� ��।���� ���뢠�� ��� ��� �஡�� ������ ���� */
DEFINE VARIABLE mCurrDayStr   AS CHARACTER NO-UNDO . /* ���뢠�� ������ � ⥪�饬 ��� */
DEFINE VARIABLE mCurrDay      AS LOGICAL   NO-UNDO . /* ���뢠�� ������ � ⥪�饬 ��� */
DEFINE VARIABLE mDateNR       AS DATE      NO-UNDO.  /* ��� �� �� �� ��⠍��। */
DEFINE VARIABLE mPutProt      AS LOG       NO-UNDO. /* �뢮���� �� ��⮪�� ���� � 䠩� calcrsrv.log */
DEFINE VARIABLE mNPRazdYchet      AS CHARACTER NO-UNDO. /* �� ������� */
DEFINE VARIABLE mNPOverClassTranz AS CHARACTER NO-UNDO. /* �� ���������࠭� */
DEFINE VARIABLE mNPNachStraph     AS CHARACTER NO-UNDO. /* �� ����� */
DEFINE VARIABLE mI139_Int         AS CHARACTER NO-UNDO. /* ���᫥��� ��業�� ��� 139� - �� �139_8861����� */
DEFINE VARIABLE mI139_PastDueInt  AS CHARACTER NO-UNDO. /* ����祭�� ��業�� ��� 139� - �� �139_8861����� */
DEFINE VARIABLE mRoleLst          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mModLoan          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mChildsListLoan   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSprReservCCode   AS CHARACTER NO-UNDO.

{pp-acctrisk.pro}
{pqres-buff.pro} /*��楤��� � ����� ����� iContract iContCode ��।��� buffer loan*/

PROCEDURE StartInterface.
   ASSIGN
      mFOiAKredRole  = FGetSetting("�����।����", "",               "")
      mParz1         = FGetSetting("��ଠ115�",    "�㬬������㤠",  "")
      mParz2         = FGetSetting("��ଠ115�",    "�㬬����������", "")
      mParz3         = FGetSetting("��ଠ115�",    "�㬬���������",  "")
      mParz4         = FGetSetting("��ଠ115�",    "�㬬�����ॡ",   "")
      mParp1         = FGetSetting("��ଠ115�",    "�㬬�������㤠","")
      mParp2         = FGetSetting("��ଠ115�",    "�㬬�����������","")
      mParp3         = FGetSetting("��ଠ115�",    "�㬬����������","")
      mParp4         = FGetSetting("��ଠ115�",    "�㬬������ॡ", "")
      mProcPar       = FGetSetting("��ଠ115�",    "�������窠",  "")
      mDelay         = FGetSetting("�஡��������", "�஡���������", "���")   =  "��"      
      mAlgFIFO       = FGetSetting("��ଠ115�",    "�珮�115",       "��")    =  "��"
      mDateNR        = DATE(FGetSetting("��⠍��।",  "",          ""))
      mCurrDayStr    = fGetSetting("��ଠ115�", 
                                   "�璥��"  , 
                                   "")
      mCurrDay       = (if {assigned mCurrDayStr} then
                          fGetSetting("��ଠ115�", 
                                      "�璥��", 
                                      "���")
                       else
                          fGetSetting("���","���������",  "���")) =  "��"
      mPutProt          = FGetSetting("rsvrStream","", "") =  "��"
      mNPRazdYchet      = FGetSetting("�������",?,"���")
      mNPOverClassTranz = FGetSetting("���������࠭�","�᪫����ኮ���","")
      mNPNachStraph     = FGetSetting("�����",?,"")
      mI139_Int         = fGetSetting("110�","�139_8861�����", "")
      mI139_PastDueInt  = fGetSetting("110�","�139_8861�����", "")
      mProcPar139       = FGetSetting("110�","�������","")
      mRoleLst          = FGetSetting("���犐",?,"")
      mModLoan          = fGetSetting ("���㫨","Mod_Loan","���") =  "��"
      mSprReservCCode   = FGetSetting("�������ࢄ��",?,"����ࢄ��") 
   .
   IF mProcPar139 =  "" THEN mProcPar139 = FGetSetting("���", "�����������", "").
      /* ������ �� �㦥� (����� ������ �� �����������) */
   mProcPar139 = IF NUM-ENTRIES(mProcPar139,":") =  2 THEN ENTRY(2,mProcPar139,":")           
                                                      ELSE mProcPar139.
   RETURN.
END PROCEDURE.

PROCEDURE DestroyInterface:
   {intrface.del}
END PROCEDURE.
/*** ���������� ��� ������� **************************************************/

/*===========================================================================*/
/*= ����� ��������� ������� ������� ��������� �������� ������� �� ��������  =*/
/*===========================================================================*/
PROCEDURE CLC_LOAN_PLAN_RES:

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate     AS DATE NO-UNDO.

   DEF VAR vLoanClass   AS CHAR NO-UNDO.
   DEF VAR vContType    AS CHAR NO-UNDO.
   DEF VAR vParamReserv AS DEC NO-UNDO.
   DEF VAR vDb          AS DEC NO-UNDO.
   DEF VAR vCr          AS DEC NO-UNDO.
   DEF VAR vOstatok     AS DEC NO-UNDO.
   DEF VAR vRemLoan     AS DEC NO-UNDO.
   DEF VAR vEndDate     AS DATE NO-UNDO.

   DEF BUFFER bloan         FOR loan.
   DEF BUFFER loan          FOR loan.

   RUN RE_B_LOAN IN h_loan (iContract, iContCode, BUFFER loan).

   IF NOT AVAIL loan THEN
      RETURN.
      
   IF IsLoanUnComLines (iContract, iContCode) THEN
      vEndDate = DATE(GetXAttrValue("loan",
                                    iContract + "," + iContCode,
                                    "FinRevDate")).
   ELSE 
      vEndDate = loan.end-date.
   
   IF    vEndDate =  ? 
      OR vEndDate <  iDate THEN 
      RETURN.

   ASSIGN
      vContType  = loan.cont-type
      vLoanClass = loan.class-code
      .

   IF NOT CAN-DO( GetXAttrInit(vLoanClass,"rel_type"), "�।���")
   THEN
      RETURN.

   RUN RE_PARAM IN h_loan ({&RES_PARAM},          /* ��� ��ࠬ���   */
                           iDate,                 /* ��� ����    */
                           iContract,             /* �����祭��      */
                           iContCode,             /* ����� �������  */
                           OUTPUT vParamReserv,   /* �㬬� ��ࠬ��� */
                           OUTPUT vDb,            /* ����� �� */
                           OUTPUT vCr).           /* ����� �� */

   IF vParamReserv >= 0 THEN RETURN.

      RUN PREPARE_SPIS_DOLG (iContract,iContCode,iDate,
                             FGetSetting("�஡��������",
                                         "�஡���������",
                                         "���") = "��",YES,YES,OUTPUT vOstatok).

  /* �஢�ઠ, �� �।�⭠� ����� */

      FOR EACH bloan WHERE
               bloan.contract = iContract
           AND bloan.cont-code BEGINS iContCode + " "
      NO-LOCK:
       IF RECID(bloan) <> RECID(loan)
       THEN LEAVE.
      END.
      IF AVAIL bloan THEN DO : /* �।�⭠� ����� */
      vOstatok = 0 .

         FOR EACH bloan WHERE
               bloan.contract = iContract
           AND bloan.cont-code BEGINS iContCode + " "
          NO-LOCK:
             IF RECID(bloan) = RECID(loan)
             THEN NEXT.
             RUN PREPARE_SPIS_DOLG (iContract,
                                bLoan.Cont-Code,
                                iDate,
                                FGetSetting("�஡��������",
                                           "�஡���������",
                                           "���") = "��",
                                YES,
                                YES,
                                OUTPUT vRemLoan).


         vOstatok = vOstatok + vRemLoan.
         END.
      END.
   RUN CALC_RESERV(iContract,iContCode,iDate,ABS(vParamReserv),vOstatok).

END PROCEDURE.
/* ========================================================================= */
/* ==== ���������� �������� �������� ��������� ����� �� �������� =========== */
/* ========================================================================= */
PROCEDURE PREPARE_SPIS_DOLG_COMMON:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* ������䨪���        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* ��� ����          */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* ���뢠�� �� �஡��   */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* ���뢠�� �஫������ */
   DEF INPUT  PARAM iRet      AS LOG  NO-UNDO. /* ----------------------*/
   DEFINE INPUT PARAMETER iDirectly AS LOGICAL NO-UNDO. /*���ࠢ�����
                                                       ��ॡ�� ttPLan*/
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* ���⮪ �� ��������   */

   DEF VAR vOstParam AS DEC NO-UNDO.
   DEF VAR vDovParam AS DEC NO-UNDO.
   DEF VAR vDb       AS DEC NO-UNDO.
   DEF VAR vCr       AS DEC NO-UNDO.
   DEF VAR vOstatok  AS DEC NO-UNDO.
   DEF VAR vSumma    AS DEC NO-UNDO.

   DEF BUFFER loan FOR loan.

   RUN RE_B_LOAN IN h_loan (iContract, iContCode, BUFFER loan).

   IF (loan.close-date <> ? AND loan.close-date < iDate) OR
       loan.open-date  > iDate
   THEN
      RETURN.

   IF loan.since >= iDate THEN
   DO:

      RUN RE_PARAM_EX IN h_loan (0,                     /* ��� ��ࠬ���   */
                                 iDate,                 /* ��� ����    */
                                 loan.since,
                                 iContract,             /* �����祭��      */
                                 iContCode,             /* ����� �������  */
                                 OUTPUT vOstParam,      /* �㬬� ��ࠬ��� */
                                 OUTPUT vDb,            /* ����� �� */
                                 OUTPUT vCr).           /* ����� �� */

      RUN RE_PARAM_EX IN h_loan (13,                    /* ��� ��ࠬ���   */
                                 iDate,                 /* ��� ����    */
                                 loan.since,
                                 iContract,             /* �����祭��      */
                                 iContCode,             /* ����� �������  */
                                 OUTPUT vDovParam,      /* �㬬� ��ࠬ��� */
                                 OUTPUT vDb,            /* ����� �� */
                                 OUTPUT vCr).           /* ����� �� */

      /* �㣠���� */
      IF vOstParam = 0 AND
         vDovParam = 0 AND
         iRet
      THEN DO:
         RETURN /*"ERROR"*/ .
      END.

   END.
   ELSE
      RUN CLC_LOAN_OST_ON_DATE(iContract,
                               iContCode,
                               iDate,
                               loan.since,
                               loan.since,
                               OUTPUT vOstParam,
                               OUTPUT vDovParam).
   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
      "����� ������� " iContCode
      " ��� ���� " iDate FORMAT "99/99/9999"               SKIP
      "��ࠬ���  0: " STRING(vOstParam,"->>>,>>>,>>>,>>9.99") SKIP
      "��ࠬ��� 13: " STRING(vDovParam,"->>>,>>>,>>>,>>9.99") SKIP.
   OUTPUT CLOSE.
   &ENDIF
   /* !!!! ��������� ������������ !!!! */
   ASSIGN
      oRem     = vOstParam + vDovParam
      vOstatok = vOstParam
      .

   IF IsLoanUnComLines (iContract,iContCode) THEN
      RUN PREPARE_REAL_OPV(iContract,iContCode,iDelay,iProl,iDate,DATE(GetXAttrValue("loan",
                                                                                     iContract + "," + iContCode,
                                                                                     "FinRevDate"))).
   ELSE
      RUN PREPARE_REAL_OPV(iContract,iContCode,iDelay,iProl,iDate,loan.end-date).

   IF iDirectly =  FALSE THEN
   DO:
      FOR EACH ttPlan WHERE ttPlan.cont-code = icontcode NO-LOCK
         BY ttPlan.end-date DESC:

         IF ttPlan.end-date < iDate THEN LEAVE.
         ASSIGN
            vSumma   = MIN(vOstatok,ttPlan.Summa)
            vOstatok = vOstatok - vSumma
            .

         CREATE ttSpis.

         ASSIGN
            ttSpis.end-date = ttPlan.end-date
            ttSpis.summa    = vSumma
            .

         IF vOstatok <= 0 THEN LEAVE.
      END.
   END.
   ELSE
   DO:
      /*mitr: ����� ������� ���冷� ��室� ttPLan*/
      FOR EACH ttPlan WHERE ttplan.cont-code = icontcode
         NO-LOCK BY ttPlan.end-date :

         IF ttPlan.end-date < iDate THEN LEAVE.

         ASSIGN
            vSumma   = MIN(vOstatok,ttPlan.Summa)
            vOstatok = vOstatok - vSumma
            .

         CREATE ttSpis.

         ASSIGN
            ttSpis.end-date = ttPlan.end-date
            ttSpis.summa    = vSumma
            .
         IF vOstatok <= 0 THEN LEAVE.
      END.
   END.

   /* !!! �� ���� �㤠 �⭮��� ���� �� 1 ���� ���� � ������襩 �� */
   IF vOstatok > 0 THEN
   DO:
      FOR EACH ttPlan WHERE ttPlan.cont-code = icontcode
         NO-LOCK BY ttPlan.end-date:
         CREATE ttSpis.

         ASSIGN
            ttSpis.end-date = ttPlan.end-date
            ttSpis.summa    = vOstatok
            .
         LEAVE.
      END.
   END.

   IF vDovParam > 0 THEN
   DO:

      CREATE ttSpis.

      ASSIGN
         ttSpis.end-date = iDate
         ttSpis.summa    = vDovParam
         .
   END.

   RELEASE ttSpis.


END PROCEDURE.
/* ========================================================================= */
/* === ���������� �������� �������� ��� ��������� ��������� ����� ������� == */
/* === ���� �� ����������� � � ������ ������� ============================== */
/* ========================================================================= */
PROCEDURE PREPARE_REAL_OPV:
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* ������䨪���        */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* �������              */
   DEF INPUT  PARAM iDelay       AS LOG  NO-UNDO. /* ���뢠�� �� �஡��   */
   DEF INPUT  PARAM iProl        AS LOG  NO-UNDO. /* ���뢠�� �஫������ */
   DEF INPUT  PARAM iDate        AS DATE NO-UNDO. /* ��� ����          */
   DEF INPUT  PARAM iLoanEndDate AS DATE NO-UNDO. /* loan.end-date */

   DEF VAR vDate AS DATE NO-UNDO.

   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER loan-cond FOR loan-cond.
   DEF BUFFER pro-obl   FOR pro-obl.

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = 3
      NO-LOCK BY term-obl.end-date:

      vDate = iLoanEndDate. /* ��� ����砭�� ������� */
      /* ��� �������� */
      IF vDate = ? THEN 
         vDate = term-obl.end-date.
      
      IF iProl THEN DO:
        FOR EACH pro-obl WHERE
                  pro-obl.contract   = iContract
              AND pro-obl.cont-code  = iContCode
              AND pro-obl.idnt       = term-obl.idnt
              AND pro-obl.new-nn     = term-obl.nn
              AND pro-obl.n-end-date = term-obl.end-date
              AND pro-obl.pr-date   > iDate
         NO-LOCK BY pro-obl.pr-date /*DESC*/ BY pro-obl.nn /*DESC*/ :
            vDate = pro-obl.end-date. /* ��� ����砭�� �������  � ��⮬ �஫����樨 */
            LEAVE.
         END.
      END.

            /* �᫨ ���뢠�� �஡�� (�� �� �஡��������� = ��), �: */
      IF iDelay THEN
      DO:
         /* ��� term-obl.dsc-beg-date ������ ���� ����� ���� ࠢ�� ��� ����砭�� (�஫����樨) �������.*/
         IF    term-obl.dsc-beg-date <= vDate 
            OR term-obl.dsc-beg-date =  ? THEN
            vDate = IF term-obl.dsc-beg-date =  ? THEN term-obl.end-date ELSE term-obl.dsc-beg-date.
      END.
      ELSE  /* ���� (�஡���������=���)                              */
      DO:
         /* �஢������ ��ண� ���� term-obl.end-date. */
         IF term-obl.end-date <= vDate THEN
            vDate = term-obl.end-date.
      END.

      /* �஢�ઠ �� �宦����� � ��ਮ� �ப� */
      IF vDate >= iDate THEN
      DO:
         CREATE ttPlan.
         ASSIGN
            ttPlan.end-date = vDate
            ttPlan.summa    = term-obl.amt-rub
            ttPlan.old-date = term-obl.end-date
            ttPlan.cont-code  = icontcode
            .
      END.
   END.
   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
     " ������� " iContCode " ������� ᯨᠭ�� �᭮����� �����" SKIP.

   FOR EACH ttPlan:
       PUT UNFORMATTED
          ttPlan.end-date FORMAT "99/99/9999"
          ttPlan.summa   FORMAT  "->>>,>>>,>>>,>>9.99" SKIP.
   END.

   OUTPUT CLOSE.
   &ENDIF

END PROCEDURE.
/* ========================================================================= */
/* === ���������� �������� �������� ��������������� ������� �� �������� ==== */
/* ========================================================================= */
PROCEDURE CALC_RESERV:

   DEF INPUT PARAM iContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO. /* ������� */
   DEF INPUT PARAM iDate     AS DATE NO-UNDO. /* ��� ���� */
   DEF INPUT PARAM iReserv   AS DEC  NO-UNDO. /* ���⮪ 21 ��ࠬ��� */
   DEF INPUT PARAM iFullOst  AS DEC  NO-UNDO. /* 0 + 13 ��ࠬ��� */


   DEF VAR vPlanRes  AS DEC NO-UNDO. /* ��ନ஢���� १�� (21 ��ࠬ���)*/
   DEF VAR vSpisRes  AS DEC NO-UNDO. /* */
   DEF VAR vPlanSpis AS DEC NO-UNDO. /* �������� ᯨᠭ�� १�ࢠ*/

   ASSIGN
      vSpisRes = 0       /* �� �� 㦥 ᯨᠭ� */
      vPlanRes = iReserv /* ��ନ஢���� १��*/
      .

   FOR EACH ttSpis BREAK BY ttSpis.end-date:
      ACCUM ttSpis.summa (TOTAL BY ttSpis.end-date).

      IF LAST (ttSpis.end-date) THEN
      DO:

         vPlanSpis = vPlanRes - vSpisRes.
         CREATE ttRes.
         ASSIGN
            ttRes.end-date = ttSpis.end-date
            ttRes.summa    = vPlanSpis
            .
      END.
      ELSE
      IF LAST-OF(ttSpis.end-date) THEN
      DO:
        (ACCUM TOTAL BY ttSpis.end-date ttSpis.summa) .

          ASSIGN
            vPlanSpis = vPlanRes * (ACCUM TOTAL BY ttSpis.end-date ttSpis.summa) / iFullOst
            vSpisRes  = vSpisRes + vPlanSpis
            .
         CREATE ttRes.
         ASSIGN
            ttRes.end-date = ttSpis.end-date
            ttRes.summa    = vPlanSpis
            .
      END.

   END.

END PROCEDURE.
/* ========================================================================= */
/* === ��������� ����� �������� ������� �� ������ ��� ������������� ======== */
/* === ������� �� ������ � ����������  ===================================== */
/* ========================================================================= */
PROCEDURE SUMM_SPIS_RES:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������*/
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* ��� ���� */
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* ��砫� ��ਮ�� */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* ����砭�� ��ਮ�� */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* �㬬� ᯨᠭ�� १�ࢠ */

   DEF BUFFER loan     FOR loan.
   DEF BUFFER term-obl FOR term-obl.

   RUN EMPTY_TABLES.

   oSumma = 0.

   RUN RE_B_LOAN IN h_loan (iContract, iContCode, BUFFER loan).

   IF     IsLoanUnComLines (iContract, iContCode) 
      AND DATE(GetXAttrValue("loan",
                             iContract + "," + iContCode,
                             "FinRevDate")) =  ? THEN RETURN.
   
   ELSE IF loan.end-date = ? THEN RETURN.

   IF LOOKUP("�।���",GetXAttrEx(loan.class-code,"rel_type","initial")) = 0
   THEN
      RETURN.
   RUN CLC_LOAN_PLAN_RES (iContract,iContCode,iDate).

   FOR EACH ttRes WHERE
            ttRes.end-date >= iBegDate
        AND ttRes.end-date <= iEndDate
   NO-LOCK:
      oSumma = oSumma + ttRes.summa.
   END.
   oSumma = ROUND(oSumma,2).

END PROCEDURE.
/* ========================================================================= */
/*��室  ttSpis====== */
/* ==!!! C ============================================================ */
PROCEDURE GET_SPIS:

   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* ��砫� ��ਮ�� */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* ����砭�� ��ਮ�� */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* �㬬� ᯨᠭ�� १�ࢠ */

   FOR EACH ttSpis WHERE
            ttSpis.end-date >= iBegDate
        AND ttSpis.end-date <= iEndDate
   NO-LOCK:
      oSumma = oSumma + ttSpis.summa.
   END.
END PROCEDURE.
/* ========================================================================= */
/* === ��������� ����� �������� ��������� ����� �� ������ ========= ======== */
/* === ��� ������������� ������� ������������� �� ������ � ���������� ====== */
/* ==!!! C ============================================================ */
PROCEDURE SUMM_SPIS_DOLG:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������*/
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* ��� ���� */
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* ��砫� ��ਮ�� */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* ����砭�� ��ਮ�� */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* ��� �஡��� */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* ��� �஫����樨 */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* �㬬� ᯨᠭ�� १�ࢠ */

   DEF VAR vOstatok AS DEC NO-UNDO.

   RUN EMPTY_TABLES.

   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
      "����� ������� " iContCode " ��� ���� " iDate FORMAT "99/99/9999"  SKIP
      "�ப � " iBegDate FORMAT "99/99/9999"
      " �� "
       iEndDate  FORMAT "99/99/9999" SKIP.
   OUTPUT CLOSE.
   &ENDIF

   RUN PREPARE_SPIS_DOLG (iContract,iContCode,iDate,iDelay,iProl,YES,OUTPUT vOstatok).
   RUN  GET_SPIS(iBegDate, iEndDate, OUTPUT oSumma).

END PROCEDURE.


/* ========================================================================= */
/* === �������� �������� �������� ������� �� �������� ====================== */
/* ========================================================================= */
PROCEDURE DELETE_RES_SPIS:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������*/

   DEF BUFFER term-obl FOR term-obl.

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = {&RES_IDNT}
      EXCLUSIVE-LOCK:
      DELETE term-obl.
   END.

END PROCEDURE.
/* ========================================================================= */
/* === ����� ��業⮢ �� ������ ��������. ================================ */
/* ========================================================================= */
PROCEDURE GET_LOAN_PROCENT:

   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* ������� */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* !!! ��砫� ��ਮ�� - 1 */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* ����砭�� ��ਮ�� */
   DEF INPUT  PARAM iParam   AS INT64  NO-UNDO. /* ��� ��ࠬ��� %% */
   DEF INPUT  PARAM iTypeOst   AS INT64  NO-UNDO. /* ��� ��ࠬ��� %% */
   DEF OUTPUT PARAM oSummaProc AS DEC  NO-UNDO. /* �㬬� %% */

   RUN pint.p (iContract, iContCode, iBegDate, iEndDate, STRING(iParam)).
   FOR EACH otch1:
      ACCUM otch1.summ_pr (TOTAL) .
   END.

   oSummaProc = ACCUM TOTAL  otch1.summ_pr.

END PROCEDURE.
/* ========================================================================= */
/* == ��� ��᫥����� ���᫥��� %% �� ������ ============================= */
/* ========================================================================= */
FUNCTION LastNachDate RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,
    iLastDate AS DATE,
    iDate     AS DATE):

   DEFINE BUFFER loan-int FOR loan-int.
    /* ��।��塞 ���� ��᫥����� ���᫥��� �� 4� ��ࠬ���
       �� ���� enddate �����⥫쭮 */
    FOR EACH loan-int WHERE
             loan-int.contract  = iContract
         AND loan-int.cont-code = iContCode
         AND loan-int.id-d     = 4
         AND loan-int.mdate   <= iDate
    NO-LOCK BY loan-int.mdate DESC:
      IF iLastDate < loan-int.mdate THEN  
         iLastDate = loan-int.mdate.
      LEAVE.
    END.

    /* �����頥� ���� ��᫥����� ���᫥��� %% �� ������ */
    RETURN iLastDate.

END FUNCTION.

/* ========================================================================= */
/* == �㬬� ���᫥���� ��業⮢ + ����祭��� ( 16 ��ࠬ��� ) =========== */
/* ========================================================================= */
PROCEDURE GetProcentUnAccount :

   DEF INPUT  PARAM vContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM vContCode AS CHAR NO-UNDO. /* �������      */
   DEF INPUT  PARAM vEndDate  AS DATE NO-UNDO. /* ��� ��᫥����� ���᫥��� */
   DEF OUTPUT PARAM oAmtProc  AS DEC  NO-UNDO. /* ���᫥��� ��業�� �� �।�⭮� �������� + ����祭�� ��業�� */

   DEF VAR vBegDate   AS DATE NO-UNDO. /* ��� ��᫥����� ���᫥��� ��業⮢ */
   DEF VAR vPint      AS DEC  NO-UNDO. /* �㬬� ���᫥���� ��業⮢ �� �������� */
   DEF VAR PastDueInt AS DEC  NO-UNDO. /* �㬬� ����祭��� ��業⮢, ����� ����祭�� �
                                          ����� �।�⢠ �� ���᫥��� %% ( 31 � 32 ��ࠬ )*/
   DEF VAR vListParam AS CHAR NO-UNDO. /* ���᮪ ����塞�� ��ࠬ��஢ */
   DEF VAR vLDopParam AS CHAR NO-UNDO. /* �������⥫�� ᯨ᮪ ����塞�� ��ࠬ��஢ */
   DEF VAR vI         AS INT64  NO-UNDO. /* ���稪 */
   DEF VAR vPastDI    AS DEC  NO-UNDO. /* ���祭�� ��ࠬ��஢ �� mListParam */
   DEF VAR vCrPastDI  AS DEC  NO-UNDO.
   DEF VAR vDbPastDI  AS DEC  NO-UNDO.
   DEF VAR vParam     AS DEC  NO-UNDO.
   DEF VAR vParamDate AS DATE NO-UNDO.

   DEF BUFFER loan FOR loan.
   FIND FIRST loan where
         loan.contract  = vContract AND
         loan.cont-code = vContCode NO-LOCK NO-ERROR .

   IF loan.class-code =  "own-bill-liability"  OR
      loan.class-code  =  "dsc-bill-asset"
     THEN DO:
       RUN bprocper.p (vContract,
                       vContCode,
                       loan.open-date,
                       vEndDate,
                       "",
                       OUTPUT oAmtProc).
     END.
     ELSE DO:
      /* vListParam = "16,29,6,31,32". */
      vListParam = "16,6,31,32".
      vLDopParam = GetSysConf("AddParams").
      IF {assigned vLDopParam} THEN
         vListParam = vListParam + "," + vLDopParam.

   /* ����塞 ���� ��᫥����� ���᫥��� ��業⮢ */
   vBegDate = lastNachDate (vContract,
                            vContCode,
                            loan.open-date,
                            vEndDate ).

   /* �饬 �㬬� ���᫥���� �� ��ਮ� %% */
      IF vBegDate <> vEndDate  THEN
   RUN get_loan_procent (vContract,
                         vContCode,
                         IF vBegDate <  vEndDate THEN vBegDate + 1 ELSE vBegDate,
                         vEndDate,
                         4,
                         1,
                         OUTPUT vPint ).

   /* ����塞 ���祭�� - �㬬� ��ࠬ��஢ 16,29,6,32 � 31 ��ࠬ��஢.
      �� ��� �� �㬬� ����祭��� ��業⮢, ����� ����祭�� �
      ����� �।�⢠ �� ���᫥��� %% ( 31 � 32 ��ࠬ )*/
   DO vI = 1 TO NUM-ENTRIES(vListParam):
      RUN STNDRT_PARAM IN h_loan(vContract,
                                 vContCode,
                                 INT64(ENTRY(vI,vListParam)),
                                 vEndDate,
                                 OUTPUT vPastDI,
                                 OUTPUT vCrPastDI,
                                 OUTPUT vDbPastDI).

      PastDueInt = PastDueInt + vPastDI .
   END.

   RUN GET_PARAM IN h_loan (vContract,
                            vContCode,
                            4,
                            vEndDate,
                            OUTPUT vParam,
                            OUTPUT vParamDate
                            ).

   /* �����뢠�� ���᫥��� ��業�� �� �������� � ����祭�� ��業�� �
      ���⠥� ����祭�� + �।�⢠ �� ���᫥��� %% */
      IF vPint       = ? then vPint = 0.
      IF PastDueInt  = ? then PastDueInt = 0.
      IF vParam      = ? then vParam     = 0.

   oAmtProc = vPint + PastDueInt + vParam .
   END.

   IF oAmtProc < 0 THEN oAmtProc = 0.
END PROCEDURE.

/* ========================================================================= */
/* === ���⪠ �६����� ⠡��祪 =========================================== */
/* ========================================================================= */
PROCEDURE EMPTY_TABLES:

   EMPTY TEMP-TABLE ttSpis.
   EMPTY TEMP-TABLE ttPlan.
   EMPTY TEMP-TABLE ttRes.
   EMPTY TEMP-TABLE ttTerm.

END PROCEDURE.
/* ========================================================================= */
/*  ������ ������� �������� ���������� 0 & 13 �� ���� ������� ���� ��������� */
/*  �������� � ������ �������� ������� - ������������ ��� �����������        */
/*  ������� ��� �������� �� ������, ���� ������� ������� ������ ������ ����  */
/*  ���������� �� ���� ������� ��� ������ ���� ������                        */
/* ========================================================================= */
PROCEDURE CLC_LOAN_OST_ON_DATE:

   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* �������*/
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO. /* ��� ���� */
   DEF INPUT  PARAM iSince     AS DATE NO-UNDO. /* ��� ������ �������*/
   DEF INPUT  PARAM iLoanSince AS DATE NO-UNDO. /* loan.since */
   DEF OUTPUT PARAM oSumma0    AS DEC  NO-UNDO. /* �㬬� ����� */
   DEF OUTPUT PARAM oSumma13   AS DEC  NO-UNDO. /* �㬬� �� ���᭥��� */

   DEF VAR vBegDate AS DATE NO-UNDO.
   DEF VAR vEndDate AS DATE NO-UNDO.
   DEF VAR vPar0    AS DEC  NO-UNDO.
   DEF VAR vPar13   AS DEC  NO-UNDO.
   DEF VAR vDb      AS DEC  NO-UNDO.
   DEF VAR vCr      AS DEC  NO-UNDO.
   DEF VAR vFirst   AS LOG  NO-UNDO.
   DEF VAR vFind    AS LOG  NO-UNDO.
   DEF VAR vDelta   AS DEC  NO-UNDO.

   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER bterm-obl FOR term-obl.

   IF iSince = ? THEN
      iSince = iLoanSince.

   IF iDate <= iSince THEN RETURN.

   ASSIGN
      vFirst   = YES
      vFind    = NO
      vBegDate = iSince + 1
      .

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = 3
        AND term-obl.end-date >= iSince
        AND term-obl.end-date <  iDate
   NO-LOCK:

      vEndDate = term-obl.end-date.

      IF vFirst THEN
      DO: /* �� ��ࢮ� 蠣� ����뢠�� ��ࠬ���� � ࠧ��᪮�*/
         RUN RE_PARAM_EX IN h_loan (0,               /* ��� ��ࠬ���   */
                                    vEndDate,        /* ��� ����    */
                                    iLoanSince,
                                    iContract,       /* �����祭��      */
                                    iContCode,       /* ����� �������  */
                                    OUTPUT oSumma0,  /* �㬬� ��ࠬ��� */
                                    OUTPUT vDb,      /* ����� �� */
                                    OUTPUT vCr).     /* ����� �� */

         RUN RE_PARAM_EX IN h_loan (13,              /* ��� ��ࠬ���   */
                                    vEndDate,        /* ��� ����    */
                                    iLoanSince,
                                    iContract,       /* �����祭��      */
                                    iContCode,       /* ����� �������  */
                                    OUTPUT oSumma13, /* �㬬� ��ࠬ��� */
                                    OUTPUT vDb,      /* ����� �� */
                                    OUTPUT vCr).     /* ����� �� */
      END.
      ELSE
      DO:
         RUN CORR_DOLG_LOAN_INT (iContract,
                                 iContCode,
                                 vBegDate,
                                 vEndDate,
                                 INPUT-OUTPUT oSumma0,
                                 INPUT-OUTPUT oSumma13).
      END.
      /* ������ ���४��㥬 ���祭�� � ��⮬ �� */
      FIND FIRST bterm-obl WHERE
                 bterm-obl.contract  = iContract
             AND bterm-obl.cont-code = iContCode
             AND bterm-obl.idnt      = 2
             AND bterm-obl.end-date = term-obl.end-date
      NO-LOCK NO-ERROR.
      IF AVAIL bterm-obl THEN
      DO:
         vDelta = oSumma0 - bterm-obl.amt-rub.
         IF vDelta > 0
         THEN
            ASSIGN
               oSumma0  = oSumma0  - vDelta
               oSumma13 = oSumma13 + vDelta
               .
      END.

      ASSIGN
         vBegDate = term-obl.end-date + 1
         vFind    = YES
         vFirst   = NO
         .

   END.
   /* �᫨ �뫨 �����-� ��� ...*/
   IF vFirst = NO THEN
      RUN CORR_DOLG_LOAN_INT (iContract,
                              iContCode,
                              vBegDate,
                              iDate,
                              INPUT-OUTPUT oSumma0,
                              INPUT-OUTPUT oSumma13).
   ELSE DO: /* ���� ���� ��訬 ��ࠬ���� */
      RUN RE_PARAM_EX IN h_loan (0,               /* ��� ��ࠬ���   */
                                 iDate,           /* ��� ����    */
                                 iLoanSince,      /* loan.since */
                                 iContract,       /* �����祭��      */
                                 iContCode,       /* ����� �������  */
                                 OUTPUT oSumma0,  /* �㬬� ��ࠬ��� */
                                 OUTPUT vDb,      /* ����� �� */
                                 OUTPUT vCr).     /* ����� �� */

      RUN RE_PARAM_EX IN h_loan (13,              /* ��� ��ࠬ���   */
                                 iDate,           /* ��� ����    */
                                 iLoanSince,      /* loan.since */
                                 iContract,       /* �����祭��      */
                                 iContCode,       /* ����� �������  */
                                 OUTPUT oSumma13, /* �㬬� ��ࠬ��� */
                                 OUTPUT vDb,      /* ����� �� */
                                 OUTPUT vCr).     /* ����� �� */
   END.

END PROCEDURE.
/* ========================================================================= */
/* = ������������� ���� ���������� 0 � 13 � ������ �������� ����� ��� ====== */
/* ========================================================================= */
PROCEDURE CORR_DOLG_LOAN_INT:

   DEF INPUT        PARAM iContract AS CHAR NO-UNDO. /*�����䨪���*/
   DEF INPUT        PARAM iContCode AS CHAR NO-UNDO. /*�������*/
   DEF INPUT        PARAM iBegDate  AS DATE NO-UNDO. /*��� �। ��� + 1*/
   DEF INPUT        PARAM iEndDate  AS DATE NO-UNDO. /*��� ᫥� ���*/
   DEF INPUT-OUTPUT PARAM pSumma0   AS DEC  NO-UNDO. /* �㬬� 0*/
   DEF INPUT-OUTPUT PARAM pSumma13  AS DEC  NO-UNDO. /* �㬬� 13*/

   DEF VAR vBegDate AS DATE NO-UNDO.
   DEF VAR vEndDate AS DATE NO-UNDO.
   DEF VAR vFind    AS LOG  NO-UNDO.
   DEF VAR vDelta   AS DEC  NO-UNDO.

   DEF BUFFER loan-int  FOR loan-int.
   DEF BUFFER bloan-int FOR loan-int.

   vBegDate = iBegDate.
   /* ������ ࠧ����� �� ����樨 ����襭�� ����  */
   FOR EACH loan-int WHERE
            loan-int.contract  = iContract
        AND loan-int.cont-code = iContCode
        AND loan-int.id-d      = 1
        AND loan-int.id-k      = 2
        AND loan-int.mdate    >= iBegDate
        AND loan-int.mdate    <= iEndDate
   NO-LOCK:

      vEndDate = loan-int.mdate.
      /* �� ����樨 ����� 㢥��稢���/㬥����� ��ࠬ���� */
      FOR EACH bloan-int WHERE
               bloan-int.contract  = iContract
           AND bloan-int.cont-code = iContCode
           AND ( bloan-int.id-d      = 0 OR
                 bloan-int.id-k      = 0)
           AND bloan-int.mdate    >= vBegDate
           AND bloan-int.mdate    <= vEndDate
      NO-LOCK:
         pSumma0 = pSumma0
                   + (IF bloan-int.id-d = 0
                      THEN bloan-int.amt-rub
                      ELSE 0)
                   - (IF bloan-int.id-k = 0
                      THEN bloan-int.amt-rub
                      ELSE 0).
      END.

      FOR EACH bloan-int WHERE
               bloan-int.contract  = iContract
           AND bloan-int.cont-code = iContCode
           AND ( bloan-int.id-d      = 13 OR
                 bloan-int.id-k      = 13)
           AND bloan-int.mdate    >= vBegDate
           AND bloan-int.mdate    <= vEndDate
      NO-LOCK:
         pSumma13 = pSumma13
                   + (IF bloan-int.id-d = 13
                      THEN bloan-int.amt-rub
                      ELSE 0)
                   - (IF bloan-int.id-k = 13
                      THEN bloan-int.amt-rub
                      ELSE 0).
      END.
      /* � ⥯��� ࠧ��᪠ �㬬� ����襭�� */
      ASSIGN
         vDelta   = loan-int.amt-rub - pSumma13
         vBegDate = loan-int.mdate + 1
         vFind    = YES
         .
      IF vDelta  > 0
      THEN
         ASSIGN
            pSumma0  = pSumma0 - vDelta
            pSumma13 = 0
            .
      ELSE
         ASSIGN
            pSumma13 = pSumma13 - loan-int.amt-rub.
   END.
   /* �� ����樨 ����� 㢥��稢���/㬥����� ��ࠬ����
      �� �� ��⠫��� ��᫥ ��᫥����� ����襭�� */
   vEndDate = iEndDate.

   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND ( bloan-int.id-d      = 0 OR
              bloan-int.id-k      = 0)
        AND bloan-int.mdate    >= vBegDate
        AND bloan-int.mdate    <= vEndDate
   NO-LOCK:
      pSumma0 = pSumma0
                + (IF bloan-int.id-d = 0
                   THEN bloan-int.amt-rub
                   ELSE 0)
                - (IF bloan-int.id-k = 0
                   THEN bloan-int.amt-rub
                   ELSE 0).
   END.

   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND ( bloan-int.id-d      = 13 OR
              bloan-int.id-k      = 13)
        AND bloan-int.mdate    >= vBegDate
        AND bloan-int.mdate    <= vEndDate
   NO-LOCK:
      pSumma13 = pSumma13
                + (IF bloan-int.id-d = 13
                   THEN bloan-int.amt-rub
                   ELSE 0)
                - (IF bloan-int.id-k = 13
                   THEN bloan-int.amt-rub
                   ELSE 0).
   END.

END PROCEDURE.
/* ========================================================================= */
/* = ���������� ������������������ ������� ��������� � ������ ����������� == */
/* = � ������� ============================================================= */
PROCEDURE PREPARE_PLAN_LINE:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.
   DEF INPUT  PARAM iDelay    AS DATE NO-UNDO.
   DEF INPUT  PARAM iProl     AS DATE NO-UNDO.
   DEF INPUT  PARAM iType     AS INT64  NO-UNDO.

   DEF VAR vSumma AS DEC NO-UNDO.

   DEF BUFFER loan FOR loan.

   RUN EMPTY_TABLES.

   CASE iType:
      WHEN 1 THEN
         RUN PREPARE_SPIS_DOLG (iContract,
                                iContCode,
                                iDate,
                                iDelay,
                                iProl,
                                NO,
                                OUTPUT vSumma).
      WHEN 2 THEN
      DO:
         RUN PREPARE_SPIS_DOLG (iContract,
                                iContCode,
                                iDate,
                                iDelay,
                                iProl,
                                NO,
                                OUTPUT vSumma).

         FOR EACH loan WHERE
                  loan.contract = iContract
              AND loan.cont-code BEGINS iContCode + " "
              AND loan.cont-code <> iContCode
         NO-LOCK:
            RUN PREPARE_SPIS_DOLG (iContract,
                                   loan.Cont-Code,
                                   iDate,
                                   iDelay,
                                   iProl,
                                   NO,
                                   OUTPUT vSumma).
         END.
      END.
   END CASE.

END PROCEDURE.


PROCEDURE GET_PLAN_DATE:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.
   DEF INPUT  PARAM iLScope   AS DATE NO-UNDO.
   DEF INPUT  PARAM iRScope   AS DATE NO-UNDO.
   DEF OUTPUT PARAM oExist    AS LOG  NO-UNDO.
   DEF OUTPUT PARAM oDate     AS DATE NO-UNDO.
   DEF OUTPUT PARAM oPDate    AS DATE NO-UNDO.

   ASSIGN
      oExist = NO
      oDate  = ?
      oPDate = ?
      .

   FOR EACH ttSpis WHERE
            ttSpis.end-date >= iLScope
        AND ttSpis.end-date <= iRScope
   NO-LOCK BY ttSpis.end-date DESC:
      ASSIGN
         oExist = YES
         oDate  = ttSpis.end-date
         oPDate = ttSpis.old-date
         .
      RETURN.
   END.

END PROCEDURE.

PROCEDURE GET_SUMMA_POG:
   DEF INPUT  PARAM iLScope   AS DATE NO-UNDO.
   DEF INPUT  PARAM iRScope   AS DATE NO-UNDO.
   DEF OUTPUT PARAM oSumma    AS DECIMAL NO-UNDO.

   FOR EACH ttSpis WHERE
      (iLScope =  ? AND ttSpis.end-date <= iRScope) OR
      (iRScope =  ? AND ttSpis.end-date >= iLScope) OR
      (iRScope <> ? AND iLScope <> ? AND
       ttSpis.end-date >= iLScope AND ttSpis.end-date <= iRScope)
   NO-LOCK:
      oSumma = oSumma + ttSpis.summa.
   END.
END PROCEDURE.

{pfuncdef 
   &DefProc="SUMM_PROC"
   &Description="��楤�� �����頥� ������襭�� ���⮪ �� �ᥬ �।��騬 
               ��易⥫��⢠� ������ ��।�����"}
PROCEDURE SUMM_PROC:
    
    DEF PARAM  BUFFER loan FOR loan.
    DEF INPUT  PARAM  iDatePlat AS DATE NO-UNDO. /* ��� ��� ������ */
    DEF INPUT  PARAM  iBegDate  AS DATE NO-UNDO. /* ��� ��砫� ������ ������襭��� ��易⥫���, �᫨ ?, � ������뢠���� �� ��易⥫��⢠ */
    DEF INPUT  PARAM  iEndDate  AS DATE NO-UNDO. /* ��� ����砭�� ������ ������襭��� ��易⥫��� */
    DEF OUTPUT PARAM  oSumm     AS DEC NO-UNDO. /* �㬬� ������襭��� ��易⥫��� */
    
    DEF BUFFER term-obl FOR term-obl.
    DEF BUFFER loan-int FOR loan-int.
    DEF BUFFER chowhe FOR chowhe.
    DEF BUFFER term-obl-hist FOR term-obl-hist.
    DEF BUFFER tobl-hist-amt FOR tobl-hist-amt.
    
   {summ-t1.p &SUMM_PROC=YES &PERC=YES}
    
END PROCEDURE.

{pfuncdef 
   &DefProc="SUMM_PROC2"
   &Description="� �⫨稥 �� summ-proc.p � �⮬ ��砥 �㤥� ������ �� ���ਨ ��䨪��"}
PROCEDURE SUMM_PROC2:
    
    DEF PARAM  BUFFER loan FOR loan.
    DEF INPUT  PARAM  iDatePlat AS DATE NO-UNDO. /* ��� ��� ������ */
    DEF INPUT  PARAM  iBegDate  AS DATE NO-UNDO. /* ��� ��砫� ������ ������襭��� ��易⥫���, �᫨ ?, � ������뢠���� �� ��易⥫��⢠ */
    DEF INPUT  PARAM  iEndDate  AS DATE NO-UNDO. /* ��� ����砭�� ������ ������襭��� ��易⥫��� */
    DEF OUTPUT PARAM  oSumm     AS DEC NO-UNDO. /* �㬬� ������襭��� ��易⥫��� */
    
    DEF BUFFER term-obl FOR term-obl.
    DEF BUFFER loan-int FOR loan-int.
    DEF BUFFER chowhe FOR chowhe.
    DEF BUFFER term-obl-hist FOR term-obl-hist.
    DEF BUFFER tobl-hist-amt FOR tobl-hist-amt.
    
   {summ-t1.p &SUMM_PROC=YES &PERC=YES &SUMM_PROC_HIST=YES}
    
END PROCEDURE.

PROCEDURE PREPARE_SPIS_PROC:
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* ������䨪���        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* ��� ����          */
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* ���⮪ �� ��������   */

   DEF VAR summ-t           AS DEC   NO-UNDO.
   DEF VAR fl1              AS LOG INIT NO.
   DEF VAR e1 AS DEC   NO-UNDO INIT 0.
   DEF VAR e2 AS DEC   NO-UNDO INIT 0.
   DEF VAR e3 AS DEC   NO-UNDO INIT 0.

   DEF VAR i         AS INT64  NO-UNDO.
   DEF VAR vSumm     AS DEC  NO-UNDO.
   DEF VAR vDbSumDec AS DEC  NO-UNDO.
   DEF VAR vCrSumDec AS DEC  NO-UNDO.
   DEF VAR vDateN    AS DATE NO-UNDO.
   DEF VAR mPayDate  AS DATE NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER xerm-obl       FOR term-obl.
   DEF BUFFER loan FOR loan.

   FIND FIRST loan WHERE
              loan.contract  =  iContract
      AND     loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* ��।��塞 ���� ������ � ��砫쭮�� �襭�� �� ������ࠬ */
      ASSIGN
         mPayDate = loan.since
         vDateN   = DATE(FGetSettingEx("��⠍��।", ?, "", NO))
         vDateN   = IF vDateN =  ? THEN DATE(1,1,1900) ELSE vDateN
      .

      FOR EACH term-obl USE-INDEX end-date WHERE
               term-obl.contract  = iContract
           AND term-obl.cont-code = iContCode
           AND term-obl.idnt      = 1
           AND term-obl.end-date >= iDate
      NO-LOCK:

         {summ-t1.i}

         CREATE ttSpis.
         ASSIGN
            ttSpis.end-date = term-obl.end-date
            ttSpis.summa    = summ-t
            .
      END.
   END.
END PROCEDURE.


/*mitr:
����筮�� ����襭�� ���� ���뢠���� � ���� �ப�

0045006: �।���. ����室�� �����㬥��
�������਩, ᮧ����� �� ���室� '�� ࠧࠡ���'  �������� �.�. (22/3/2005 11:52:16)
����� ������ i1p1_2 -�.135(110-�).�����஢�� 2
����室��� ᤥ���� �⤥���� �㭪�� ��� ���� ������ ����஢��:
- �� ������ ���� ���� ���⮪ �� ��� � �� ���� (�� ����� ࠭���� � ����� ��������)��ᬠ�ਢ��� ���쭥�訥 ������� ����襭��

���ਬ��:
�����������                                 �� 21/04     7000
������� �믫���                                        1000
䠪��᪨  �뫮 �믫�祭�   �� 21.05   4500
�� ������ ����  䠪�.���⮪ �� 31.05 2500
⮣�� ������ ��ᬠ�ਢ���
�� 21.06 1000
�� 21.07  1000
�� 21.08    500

*/
/*����� �� PREPARE_SPIS_DOLG*/
/* ========================================================================= */
/* ==== ���������� �������� �������� ��������� ����� �� �������� =========== */
/* ========================================================================= */
PROCEDURE PREPARE_SPIS_DOLG:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* ������䨪���        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* ��� ����          */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* ���뢠�� �� �஡��   */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* ���뢠�� �஫������ */
   DEF INPUT  PARAM iRet      AS LOG  NO-UNDO. /* ----------------------*/
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* ���⮪ �� ��������   */

   RUN PREPARE_SPIS_DOLG_common IN THIS-PROCEDURE (
                           iContract,
                           iContCode,
                           iDate,
                           iDelay,
                           iProl,
                           iRet,
                           FALSE,   /* ���⭮� ���ࠢ� ��ॡ�� ���祭�� � ttPlan*/
                           OUTPUT oRem
                           ).


END PROCEDURE.

/*���� ����� ����஥��� ᯨᠭ��  */
PROCEDURE PREPARE_SPIS_DOLG2:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* ������䨪���        */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������              */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* ��� ����          */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* ���뢠�� �� �஡��   */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* ���뢠�� �஫������ */
   DEF INPUT  PARAM iRet      AS LOG  NO-UNDO. /* ----------------------*/
   DEF OUTPUT PARAM oRem      AS DEC  NO-UNDO. /* ���⮪ �� ��������   */

   RUN PREPARE_SPIS_DOLG_common IN THIS-PROCEDURE (
                           iContract,
                           iContCode,
                           iDate,
                           iDelay,
                           iProl,
                           iRet,
                           TRUE,    /* ��אַ� ���ࠢ� ��ॡ�� ���祭�� � ttPlan*/
                           OUTPUT oRem
                           ).


END PROCEDURE.

PROCEDURE SUMM_SPIS_DOLG2:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* �������*/
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO. /* ��� ���� */
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* ��砫� ��ਮ�� */
   DEF INPUT  PARAM iEndDate  AS DATE NO-UNDO. /* ����砭�� ��ਮ�� */
   DEF INPUT  PARAM iDelay    AS LOG  NO-UNDO. /* ��� �஡��� */
   DEF INPUT  PARAM iProl     AS LOG  NO-UNDO. /* ��� �஫����樨 */
   DEF OUTPUT PARAM oSumma    AS DEC  NO-UNDO. /* �㬬� ᯨᠭ�� १�ࢠ */

   DEF VAR vOstatok AS DEC NO-UNDO.

   RUN EMPTY_TABLES.

   &IF DEFINED(PqresLog) <> 0 &THEN
   OUTPUT TO "pqres.log" APPEND.
   PUT UNFORMATTED
      "����� ������� " iContCode " ��� ���� " iDate FORMAT "99/99/9999"  SKIP
      "�ப � " iBegDate FORMAT "99/99/9999"
      " �� "
       iEndDate  FORMAT "99/99/9999" SKIP.
   OUTPUT CLOSE.
   &ENDIF

   RUN PREPARE_SPIS_DOLG2 (iContract,iContCode,iDate,iDelay,iProl,YES,OUTPUT vOstatok).

   FOR EACH ttSpis WHERE
            ttSpis.end-date >= iBegDate
        AND ttSpis.end-date <= iEndDate
   NO-LOCK:
      oSumma = oSumma + ttSpis.summa.
   END.

END PROCEDURE.

PROCEDURE GET_SUMMA_DOLG :
   DEF INPUT PARAM iContract AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO. /*  ����� ������� */
   DEF INPUT PARAM iBegDate  AS DATE NO-UNDO. /*���� ������ */
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO. /* ��� ���� */
   DEF INPUT PARAM iDelayFl  AS LOG  NO-UNDO. /* ���뢠�� �� �஡�� ����襭��
           ** �᭮����� �����. �.�. �᫨ ����砭�� ���⥦���� ��ਮ�� ��������
           ** � ��ਮ� ����, � ���⥦ ���뢠���� � ����. ��ࠬ���
           ** ����� �ਮ��� ��� �� �஡��������.�஡���������. �᫨ ���祭��
           ** �室���� ��ࠬ��� ࠢ�� ? (�����.), � �ᯮ������ ��. */
   DEF OUTPUT PARAM oSummaDolg  AS  DECIMAL NO-UNDO.
   /* �������� ����襭�� �� ��ਮ� */
   DEF OUTPUT PARAM oSummaPr   AS  DECIMAL NO-UNDO.
   /* ������襭�� ���⮪ �� ��ਮ� */

   DEF VAR vDelay    AS LOGICAL NO-UNDO .
   DEF VAR vProl     AS LOGICAL NO-UNDO .
   DEF VAR vDelayl   AS INT64   NO-UNDO .
   DEF VAR vBegDate  AS DATE    NO-UNDO. /*���� ������ */
   DEF VAR vEndDate  AS DATE    NO-UNDO . /* ��� ���� */
   DEF VAR oSummaPrL AS DECIMAL NO-UNDO.

   DEF BUFFER term-obl FOR term-obl .
   DEF BUFFER pro-obl  FOR pro-obl .

   RUN EMPTY_TABLES.

   /* ���뢠�� �஡�� ����襭�� */
   ASSIGN
      vDelay = IF iDelayFl <> ? THEN iDelayFl ELSE (FGetSetting("�஡��������","�஡���������","���") = "��")
      vProl  = FGetSetting("���஫",?,"���") = "��"
      .

   FIND LAST loan  WHERE loan.contract = iContract
      AND  loan.cont-code = iContCode
      NO-LOCK NO-ERROR .
   IF loan.cont-type = {&Sogl_Type}
      THEN RETURN .

   IF vDelay THEN 
   DO :
      FIND LAST loan-cond  WHERE loan-cond.contract = iContract
         AND  loan-cond.cont-code = iContCode
         AND  loan-cond.since <= iBegDate NO-LOCK NO-ERROR.

      IF AVAIL loan-cond THEN 
      DO :
         /* ᤢ����� �ࠢ�� �࠭���, ���� �����  � �롮થ ��易⥫��⢠,
            �������騥
           � ������� �������� � ��⮬ �஡���, �� ᤢ�� �� ������ �ॢ����
           ���� �᫮���  �� �祭� �ࠢ��쭮 - �� ��� �᫮��� ����� ����
           ��㣨� �᫮��� � ����訬 �஡���� */
         vBegDate = IF iBegDate - loan-cond.delay1 < loan-cond.since
            THEN loan-cond.since
            ELSE iBegDate - loan-cond.delay1 .
      END.
      ELSE vBegDate = iBegDate .
      /* ���������� ��� � ���� ���祭��� �஡��� */
      CREATE ttSpis.
      ASSIGN 
         ttSpis.end-date = vBegDate
         ttSpis.summa    = IF AVAIL loan-cond
                                         THEN loan-cond.delay1
                                         ELSE 0 .
      vDelayl = ttSpis.summa .
      FOR EACH loan-cond WHERE loan-cond.contract = iContract
         AND   loan-cond.cont-code = iContCode
         AND   loan-cond.since >  iBegDate
         AND   loan-cond.since <= iEndDate  NO-LOCK :
         IF loan-cond.delay1  <> vDelayl
            THEN 
         DO:
            /* ���������� ��� � ᫥���騬 ���祭��� �஡��� */
            CREATE ttSpis.
            ASSIGN 
               ttSpis.end-date = loan-cond.since
               ttSpis.summa    = loan-cond.delay1
               vDelayl = ttSpis.summa
               .
         END.
      END.
   END.
   ELSE 
   DO :
      CREATE ttSpis.
      ASSIGN 
         ttSpis.end-date = iBegDate
         ttSpis.summa    = 0 .

   END.
   FIND FIRST  ttSpis NO-ERROR.
   DO WHILE AVAIL ttSpis :

      vDelayl  = ttSpis.summa.
      vBegDate = ttSpis.end-date.

      FIND NEXT ttSpis NO-ERROR .
      IF AVAIL ttSpis
         THEN ASSIGN vEndDate = ttSpis.end-date - 1. /* ���� �஡��
                         ������� �� ���� ������ -1 ����*/
      ELSE vEndDate = iEndDate.
      cicl:
      FOR EACH term-obl WHERE
         term-obl.contract      = iContract
         AND term-obl.cont-code = iContCode
         AND term-obl.idnt      = 3
         AND term-obl.end-date  >= vBegDate
         AND term-obl.end-date  <= vEndDate
         NO-LOCK :

         IF term-obl.end-date + vDelayl < iBegDate
            OR
            (IF iEndDate <  loan.open-date
            THEN term-obl.end-date + vDelayl > iEndDate
         ELSE FALSE)
            THEN NEXT cicl .
         /* �᪫�砥� �� ����室����� ��易⥫��⢠
           �஫����஢���� �� ����� ��ਮ� ��
           ����� ࠭��� ��ਮ��� */
         IF NOT vProl
            THEN 
         DO:
            FIND FIRST pro-obl WHERE
               pro-obl.contract   = iContract
               AND pro-obl.cont-code  = iContCode
               AND pro-obl.idnt       = term-obl.idnt
               AND pro-obl.new-nn     = term-obl.nn
               AND pro-obl.n-end-date = term-obl.end-date
               AND pro-obl.pr-date   <  term-obl.end-date
               NO-LOCK NO-ERROR .
            IF AVAIL pro-obl AND pro-obl.end-date < iBegDate
               THEN  NEXT cicl.
         END.
         RUN summ-t.p(OUTPUT oSummaPrL,iContract,iContCode,RECID(term-obl),iEndDate).
         ASSIGN
            oSummaDolg = oSummaDolg + term-obl.amt-rub
            oSummaPr   = oSummaPr + oSummaPrL
            .
      END.
      IF vEndDate  = iEndDate
         THEN LEAVE .
   END.
END PROCEDURE .

{pfuncdef 
&DefProc="GET_SUMMA_DOLG_RUB"
&Description="�㬬� ������������ � ������ � �㡫��"}
 
 /* �㬬� ������������ � ������ � �㡫�� */
PROCEDURE GET_SUMMA_DOLG_RUB :
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iBegDate     AS DATE NO-UNDO. /* ��� ��砫� */
   DEF INPUT  PARAM iEndDate     AS DATE NO-UNDO. /* ��� ����砭�� */
   DEF INPUT  PARAM iDelayFl     AS LOG  NO-UNDO. /* ���뢠�� �� �஡�� ����襭��
          ** �᭮����� �����. �.�. �᫨ ����砭�� ���⥦���� ��ਮ�� ��������
          ** � ��ਮ� ����, � ���⥦ ���뢠���� � ����. ��ࠬ���
          ** ����� �ਮ��� ��� �� �஡��������.�஡���������. �᫨ ���祭��
          ** �室���� ��ࠬ��� ࠢ�� ? (�����.), � �ᯮ������ ��. */
   DEF OUTPUT PARAM oSummaDolg   AS DEC  NO-UNDO. /* �������� ����襭�� �� ��ਮ� */
                                                  /* �� ��䨪� (�� �� ���� ���⥦� �� ��䨪�) */
   DEF OUTPUT PARAM oSummaPr     AS DEC  NO-UNDO. /* ������襭�� ���⮪ �� ��ਮ� */
                                                  /* (� ����� �������) */
   DEF OUTPUT PARAM oSummaOplRub AS DEC  NO-UNDO. /* �㬬� ����襭�� � ���.���. */
                                                  /* (�� �� ���� ���⥦�) */

   DEF VAR vDelay    AS LOG  NO-UNDO.  /* ���뢠�� �஡�� ����襭�� �� */
   DEF VAR vProl     AS LOG  NO-UNDO.  /* ���뢠�� �஫������ */
   DEF VAR vDelayl   AS INT64  NO-UNDO.  /* �஡�� */
   DEF VAR vBegDate  AS DATE NO-UNDO.  /* ��� ��砫� */
   DEF VAR vEndDate  AS DATE NO-UNDO.  /* ��� ����砭�� */
   DEF VAR vSummaDolgN AS DEC  NO-UNDO.   /* �����.���. ���⥦� �� ��砫� ��ਮ��*/
   DEF VAR vSummaPrL AS DEC  NO-UNDO.  /* �����.���. ���⥦� �� ����� ��ਮ�� */
   DEF VAR vSmDolgR  AS DEC  NO-UNDO.  /* �㬬� ��������� �����. � ��. */
   DEF VAR vSummaOpl AS DEC NO-UNDO. /* �㬬� ������ � ����� ������� */
   DEF VAR vIdDList  AS CHAR NO-UNDO INIT "1,5".
   DEF VAR vIdKList  AS CHAR NO-UNDO INIT "2,7".
   DEF VAR vCount    AS INT64  NO-UNDO.  /* ���稪 */

   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER pro-obl   FOR pro-obl.
   DEF BUFFER loan      FOR loan.
   DEF BUFFER loan-cond FOR loan-cond.
   DEF BUFFER loan-int  FOR loan-int.

   RUN EMPTY_TABLES IN THIS-PROCEDURE.

   FIND LAST loan WHERE
             loan.contract  =  iContract
      AND    loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
      /* �᫨ ⨯ ������� �� "��祭��" */
   IF     AVAIL loan
      AND loan.cont-type <> {&SOGL_TYPE} THEN
   DO:
         /* ���뢠�� �஡�� ����襭��, �஫������ */
      ASSIGN
         vDelay   = IF iDelayFl <> ? THEN iDelayFl ELSE (FGetSetting("�஡��������", 
                                                                     "�஡���������", 
                                                                     "���") =  "��")
         vProl    = FGetSetting("���஫", ?, "���") =  "��"
         vBegDate = iBegDate
      .
         /* �᫨ ���뢠�� �஡�� ����襭�� �� */
      IF vDelay THEN
      DO:
            /* ᤢ����� �ࠢ�� �࠭���, �⮡� ����� � �롮થ ��易⥫��⢠, �������騥 �
            ** ������� �������� � ��⮬ �஡���, �� ᤢ�� �� ������ �ॢ���� ���� �᫮���.
            ** �� �祭� �ࠢ��쭮 - �� ��� �᫮��� ����� ���� ��㣨� �᫮��� � ����訬 �஡���� */
         FIND LAST loan-cond  WHERE
                   loan-cond.contract  =  iContract
            AND    loan-cond.cont-code =  iContCode
            AND    loan-cond.since     <= iBegDate
         NO-LOCK NO-ERROR.
         IF AVAIL loan-cond THEN
            vBegDate = IF (iBegDate - loan-cond.delay1) <  loan-cond.since
                          THEN loan-cond.since
                          ELSE iBegDate - loan-cond.delay1.
            /* ���������� ��� � ���� ���祭��� �஡��� */
         CREATE ttSpis.
         ASSIGN
            ttSpis.end-date = vBegDate
            ttSpis.summa    = IF AVAIL loan-cond THEN loan-cond.delay1 ELSE 0
            vDelayl         = IF AVAIL loan-cond THEN loan-cond.delay1 ELSE 0
         .
            /* ���������� �窨 � ᫥���騬� �⫨�묨 ���祭���� �஡��� */
         FOR EACH loan-cond WHERE
                  loan-cond.contract  =  iContract
            AND   loan-cond.cont-code =  iContCode
            AND   loan-cond.since     >  iBegDate
            AND   loan-cond.since     <= iEndDate
         NO-LOCK:
            IF loan-cond.delay1 <> vDelayl THEN
            DO:
               CREATE ttSpis.
               ASSIGN
                  ttSpis.end-date = loan-cond.since
                  ttSpis.summa    = loan-cond.delay1
                  vDelayl         = loan-cond.delay1
               .
            END.
         END.
      END.
         /* ��� ��� �஡��� ����襭�� �� */
      ELSE
      DO:
         CREATE ttSpis.
         ASSIGN
            ttSpis.end-date = iBegDate
            ttSpis.summa    = 0
         .
      END.

         /* ���� �� �窠�... */
      FIND FIRST ttSpis NO-ERROR.
      BLK:
      DO WHILE AVAIL ttSpis:
            /* ��।���� ��ਮ� � �஡�� */
         ASSIGN
            vDelayl  = ttSpis.summa
            vBegDate = ttSpis.end-date
            vEndDate = iEndDate
         .
            /* ���� �஡�� ������� �� ���� ������ -1 ����*/
         FIND NEXT ttSpis NO-ERROR.
         IF AVAIL ttSpis THEN
            vEndDate = ttSpis.end-date - 1.

            /* ������ ���� �� ��易⥫��⢠�, �������騬 � ��ਮ� */
         CICL:
         FOR EACH term-obl WHERE
                  term-obl.contract  =  iContract
            AND   term-obl.cont-code =  iContCode
            AND   term-obl.idnt      =  3
            AND   term-obl.end-date  >= vBegDate
            AND   term-obl.end-date  <= vEndDate
         NO-LOCK:
               /* �᪫�砥� ��易⥫��⢠ ����訥 ���� ��砫� */
            IF (term-obl.end-date + vDelayl) <  iBegDate THEN
               NEXT CICL.
               /* �� ��� ��������?  */
            IF      iEndDate <  loan.open-date
               OR (term-obl.end-date + vDelayl) >  iEndDate THEN
               NEXT CICL.

               /* �᪫�砥� �� ����室����� ��易⥫��⢠ �஫����஢����
               ** �� ����� ��ਮ� �� ����� ࠭��� ��ਮ��� */
            IF NOT vProl THEN
            DO:
               FIND FIRST pro-obl WHERE
                          pro-obl.contract   =  iContract
                  AND     pro-obl.cont-code  =  iContCode
                  AND     pro-obl.idnt       =  term-obl.idnt
                  AND     pro-obl.new-nn     =  term-obl.nn
                  AND     pro-obl.n-end-date =  term-obl.end-date
                  AND     pro-obl.pr-date    <  term-obl.end-date
               NO-LOCK NO-ERROR.
               IF     AVAIL pro-obl
                  AND pro-obl.end-date <  iBegDate THEN
                  NEXT CICL.
            END.
            /* �஢��塞 �� �� ����襭 ���⥦ ��ண� �� ���⭮�� ��ਮ�� */
            RUN SetSysConf IN h_base ("������������⥦�","��").
            RUN summ-t.p (OUTPUT vSummaDolgN,
                          iContract,
                          iContCode,
                          RECID(term-obl),
                          MAX(iBegDate - 1,loan.open-date)).
            RUN DeleteOldDataProtocol IN h_base ("������������⥦�").
            /* �᫨ ���⥦ ����筮 ����襭 �� ���⭮�� ��ਮ��, � �ய�᪠�� ��� */
            IF vSummaDolgN =  0 THEN NEXT CICL.
            /* ��⠥� ᪮�쪮 ��⠫��� ������襭�� �� ����� ��ਮ�� */
            RUN SetSysConf IN h_base ("������������⥦�","��").
            RUN summ-t.p (OUTPUT vSummaPrL,
                          iContract,
                          iContCode,
                          RECID(term-obl),
                          iEndDate).
            RUN DeleteOldDataProtocol IN h_base ("������������⥦�").
            ASSIGN
               oSummaDolg = oSummaDolg + term-obl.amt-rub
               oSummaPr   = oSummaPr   + vSummaPrL    /* ������襭�� ���⮪ �� ��ਮ� */
               vSmDolgR   = vSmDolgR   + CurToBase("����",
                                                   term-obl.currency,
                                                   term-obl.end-date,
                                                   vSummaDolgN)
            .
               /* ��祬 ? �� ���-� �ᯮ������ ? */
            CREATE ttTerm.
            ttTerm.end-date = term-obl.end-date.
         END.
            /* ����� ��室��� */
         IF vEndDate =  iEndDate THEN
            LEAVE BLK.
      END.
         /* ��।���� �㬬� ����襭�� � �㡫�� � ���⭮� ��ਮ��*/
      DO vCount = 1 TO NUM-ENTRIES(vIdDList):
          /* ��ॡ�� ��� ����権 (5 ��� 50), � ����� ��ਮ�. */
         FOR EACH loan-int OF loan WHERE
                  loan-int.id-d  =  INT64(ENTRY(vCount, vIdDList))
            AND   loan-int.id-k  =  INT64(ENTRY(vCount, vIdKList))
            AND   loan-int.mdate >= iBegDate
            AND   loan-int.mdate <= iEndDate
         NO-LOCK:
            ASSIGN
               vSummaOpl    = vSummaOpl + loan-int.amt-rub
               oSummaOplRub = oSummaOplRub + CurToBase("����",
                                                       loan.currency,
                                                       loan-int.mdate,
                                                       loan-int.amt-rub).
         END.
      END.
         /* �᫨ ॠ�쭮� ����襭�� ����� �㬬� �������� ��襭��,
         ** ��⠥�, �� ��� �⭮����� � ��㣮�� ��ਮ�� (0103005) */
      oSummaOplRub = IF oSummaOplRub >  vSmDolgR
                        THEN vSmDolgR
                        ELSE oSummaOplRub.
      /* �᫨ ��� �㬬� �� ����襭�, � �� �� ���� ����砭�� ��ਮ�� */
      IF loan.currency <> "" THEN   
         IF oSummaPr =  oSummaDolg THEN
            oSummaDolg = CurToBase("����",
                                   loan.currency,
                                   iEndDate,
                                   oSummaDolg). 
         ELSE
         DO:
            oSummaDolg = oSummaOplRub + CurToBase("����",
                                                  loan.currency,
                                                  iEndDate,
                                                  oSummaDolg - vSummaOpl).
         END.
   END.
END PROCEDURE .


{pfuncdef 
&DefProc="GET_SUMMA_DOLG_RUB_NEW"
&Description="�㬬� ������������ � ������ � �㡫��"}

   /* �㬬� ������������ � ������ � �㡫�� */
PROCEDURE GET_SUMMA_DOLG_RUB_NEW:
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iBegDate     AS DATE NO-UNDO. /* ��� ��砫� */
   DEF INPUT  PARAM iEndDate     AS DATE NO-UNDO. /* ��� ����砭�� */
   DEF OUTPUT PARAM oSummaDolg   AS DEC  NO-UNDO. /* �������� ����襭�� �� ��ਮ� �� ��䨪� */ 
                                                  /* (�� �� ���� ���⥦� �� ��䨪�) */
   DEF OUTPUT PARAM oSummaPr     AS DEC  NO-UNDO. /* ������襭�� ���⮪ �� ��ਮ� */
                                                  /* (� ����� �������) */

   DEF VAR vIdDList     AS CHAR  NO-UNDO INIT "1,5". /* ���᮪ ��ࠬ��஢ �� ������ */
   DEF VAR vIdKList     AS CHAR  NO-UNDO INIT "2,7". /* ���᮪ ��ࠬ��஢ �� �।��� */
   DEF VAR vCount       AS INT64 NO-UNDO.  /* ���稪 */
   DEF VAR vListParam   AS CHAR  NO-UNDO INIT "0,7,13,47". /* ���᮪ ��ࠬ��஢ ���⪠ */
   DEF VAR vOst         AS DEC   NO-UNDO.  /* ���⪨ �� ��ࠬ��ࠬ 0,7,13,47 */
   DEF VAR vRazn        AS DEC   NO-UNDO.  /* �����ᥭ��� �㬬�*/
   DEF VAR vNeUch       AS DEC   NO-UNDO.  /* �㬬� ����⥭��� ���⥦�� */
   DEF VAR vNeUchR      AS DEC   NO-UNDO.  /* �㬬� ����⥭��� ���⥦�� � ��.*/
   DEF VAR vSumm        AS DEC   NO-UNDO.  /* ��९���/������������� (��९��� +, ������. -) */
   DEF VAR vDate        AS DATE  NO-UNDO.
   DEF VAR vDb          AS DEC   NO-UNDO.
   DEF VAR vCr          AS DEC   NO-UNDO.
   DEF VAR vBegDate     AS DATE  NO-UNDO. /*����४�஢����� ��� ��ਮ��*/
   DEF VAR vRealDolg    AS DEC   NO-UNDO. /*�����᪠� �㬬� ������������*/

   DEF BUFFER loan           FOR loan.
   DEF BUFFER term-obl       FOR term-obl.
   DEF BUFFER loan-int       FOR loan-int.

   MAIN:
   DO:
      FIND LAST loan WHERE
                loan.contract  =  iContract
         AND    loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.

         /* �᫨ ⨯ ������� �� "��祭��" */
      IF AVAIL loan AND 
               loan.cont-type <> {&SOGL_TYPE} THEN
      DO:
         /* ��।������ ��९����/������������ �� ��砫� ��ਮ�� */
         FIND LAST term-obl  WHERE
                   term-obl.contract  =  loan.Contract
               AND term-obl.cont-code =  loan.Cont-Code
               AND term-obl.idnt      =  2
               AND term-obl.end-date  <= iBegDate - 1
         NO-LOCK NO-ERROR.

         IF AVAIL term-obl THEN
         DO:         
            vSumm = term-obl.amt-rub.
         END. 

         DO vCount = 1 TO NUM-ENTRIES(vListParam):
            RUN RE_PARAM_EX IN h_Loan (INT64(ENTRY(vCount,vListParam)),
                                       iBegDate - 1,
                                       loan.since,
                                       loan.contract,
                                       loan.cont-code,
                                       OUTPUT vOst,
                                       OUTPUT vDb,
                                       OUTPUT vCr ).
            ASSIGN
               vSumm     = vSumm - vOst
               vRealDolg = vRealDolg + vOst
            .
            
         END.

         /*��᫥���� ��易⥫��⢮ �� ��砫� ��ਮ��*/
         FIND LAST term-obl WHERE
                   term-obl.contract  =  iContract
             AND   term-obl.cont-code =  iContCode
             AND   term-obl.idnt      =  3
             AND   term-obl.end-date  <  iBegDate
         NO-LOCK NO-ERROR.

         IF AVAIL term-obl THEN
            vBegDate = term-obl.end-date.
         ELSE 
            vBegDate = iBegDate.

        /* ������ ���� �� ��易⥫��⢠�, �������騬 � ��ਮ� */
         CICL:
         FOR EACH term-obl WHERE
                  term-obl.contract  =  iContract
            AND   term-obl.cont-code =  iContCode
            AND   term-obl.idnt      =  3
            AND   term-obl.end-date  >= vBegDate
         NO-LOCK:

            IF term-obl.dsc-beg-date >  iEndDate THEN
               LEAVE CICL.

            IF term-obl.dsc-beg-date <  iBegDate THEN
               NEXT CICL.

            IF term-obl.sop-date <> ? THEN
               IF (term-obl.sop-date <= iBegDate) THEN 
                  NEXT CICL.
            
            IF vSumm >= term-obl.amt-rub THEN
            DO:
               vSumm = vSumm - term-obl.amt-rub.
               NEXT CICL.   
            END.  

               vNeUch  = vNeUch  + term-obl.amt-rub.
         END.
 
         vNeUch = MIN(vNeUch, vRealDolg).
      
          /* �᫨ ��易⥫��� ��� - ��室�� */
         IF vNeUch <= 0 THEN
            LEAVE MAIN. 

         /* ��� ��९����/������������ */
         ASSIGN
            vDate      = iBegDate
            vRazn      = min(max(vSumm, 0), vNeUch)
            vNeUch     = vNeUch - vRazn
            vSumm      = vSumm - vRazn
         .

         IF FGetSetting("��ଠ115�","��।","") =  "����" THEN
            oSummaDolg = oSummaDolg + CurToBase("����",
                                                loan.currency,
                                                iBegDate, 
                                                vRazn).
         IF vNeUch <= 0 THEN
            LEAVE MAIN. 

         DO WHILE vDate <= iEndDate:
             /* ��।���� �㬬� ����襭�� � �㡫�� � ���⭮� ��ਮ�� */
            DO vCount = 1 TO NUM-ENTRIES(vIdDList):
                /* ��ॡ�� ��� ����権 (5 ��� 50), � ����� ��ਮ�. */
               Opl:
               FOR EACH loan-int OF loan WHERE
                        loan-int.id-d  =  INT64(ENTRY(vCount, vIdDList))
                    AND loan-int.id-k  =  INT64(ENTRY(vCount, vIdKList))
                    AND loan-int.mdate =  vDate
               NO-LOCK:
                  ASSIGN
                     vSumm      = vSumm + loan-int.amt-rub
                     vRazn      = min(max(vSumm, 0), vNeUch)
                     vNeUch     = vNeUch - vRazn
                     vSumm      = vSumm - vRazn
                     oSummaDolg = oSummaDolg + CurToBase("����",
                                                         loan.currency,
                                                         loan-int.mdate, 
                                                         vRazn)
                  .

                  IF vNeUch <= 0 THEN
                     LEAVE MAIN. 

               END.
            END.
            vDate = vDate + 1.
         END.

         ASSIGN
            vNeUchR = CurToBase("����",
                               loan.currency,
                               iEndDate, 
                               vNeUch) 
            oSummaDolg = oSummaDolg + vNeUchR
            oSummaPr   = vNeUchR
         .

      END.
   END. /* MAIN */
END PROCEDURE.

/* �⡨ࠥ� �� ���� ��������, � ����� �ਢ易� ��� �� ����,
** � ᪫��뢠�� �� �६����� ⠡���� contract,cont-code ������஢
** � ஫�, � ���ன �ਢ易�� ��� */
PROCEDURE LoanFromAcct.
   DEF INPUT  PARAM iAcct  AS CHAR   NO-UNDO.   /* ����� ��� */
   DEF INPUT  PARAM iCurr  AS CHAR   NO-UNDO.   /* ����� ��� */
   DEF INPUT  PARAM iSince AS DATE   NO-UNDO.   /* ��� */
   DEF OUTPUT PARAM oAvail AS LOG    NO-UNDO.   /* �ਧ��� ������ ��� ����� ����� */
   DEFINE OUTPUT PARAMETER TABLE FOR tt-oLoanAcct.

   DEF BUFFER b1-lacct FOR loan-acct. /* ���������� ����. */

   {empty tt-oLoanAcct}

   FOR EACH loan-acct WHERE 
            loan-acct.acct  =  iAcct
        AND loan-acct.curr  =  iCurr
        AND loan-acct.since <= iSince 
        AND NOT CAN-FIND (FIRST b1-lacct WHERE b1-lacct.acct       =  loan-acct.acct
                                           AND b1-lacct.curr       =  loan-acct.curr
                                           AND b1-lacct.contract   =  loan-acct.contract
                                           AND b1-lacct.cont-code  =  loan-acct.cont-code
                                           AND b1-lacct.acct-type  =  loan-acct.acct-type
                                           AND b1-lacct.since      <= iSince
                                           AND b1-lacct.since      >  loan-acct.since)
      NO-LOCK:
      CREATE tt-oLoanAcct.
      ASSIGN
         tt-oLoanAcct.contract  = loan-acct.contract
         tt-oLoanAcct.cont-code = loan-acct.cont-code
         tt-oLoanAcct.acct-type = loan-acct.acct-type
         tt-oLoanAcct.since     = loan-acct.since
      .
   END.
   oAvail = AVAILABLE tt-oLoanAcct.

   RETURN.
END PROCEDURE.

/* ��楤��, ��।������ ������������� �� ��ࠬ��ࠬ � ����ᨬ��� �� ஫� ��� */
PROCEDURE GetSummDolg.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.
   DEF INPUT  PARAM iBegDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iEndDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO.
   DEF OUTPUT PARAM oSummDolg AS DEC    NO-UNDO. /* �㬬� ������������ � ࠧ��᪮� �� �ப�� */
   DEF OUTPUT PARAM oAllDolg  AS DEC    NO-UNDO. /* ���� �㬬� ������������ ��� ࠧ��᪨ �� �ப�� */
   DEF OUTPUT PARAM oByTerm   AS LOG    NO-UNDO. /* ����� � ��⮬ ࠧ��᪨ �� �ப�� ��� ��� */

   DEF VAR vTmpSumm   AS DEC    NO-UNDO.
   DEF VAR vTmpCurr   AS CHAR   NO-UNDO.
   DEF VAR vPrmList   AS CHAR   NO-UNDO INIT "�।��,�।��,�।�,�।���,�।�,�।���,�।��%,�।��%1,�।�㤊��,�।�㤏���,�।��᪄�,�।��᪏�,�।���,�।���1,�।�����,�।����,�।�����,�।������,�।�������,�।����,�।������,�।���,�।����". 
   DEF VAR vPrmListP  AS CHAR   NO-UNDO.
   DEF VAR vPrmListM  AS CHAR   NO-UNDO.
   DEF VAR vPrm       AS CHAR   NO-UNDO.
   DEF VAR vi         AS INT64  NO-UNDO.
   DEF VAR vSumm      AS DEC    NO-UNDO.
   DEF VAR vDb        AS DEC    NO-UNDO.
   DEF VAR vCr        AS DEC    NO-UNDO.
   DEF VAR vCodOstpar AS INT64  NO-UNDO.
   DEF VAR vCodeName  AS CHAR   NO-UNDO.

   DEF BUFFER code     FOR code.
   DEF BUFFER loan     FOR loan.
   DEF BUFFER term-obl FOR term-obl.

   MAIN_BLOCK:
   DO:
      IF NOT CAN-DO (vPrmList, iAcctType) THEN
         LEAVE MAIN_BLOCK.
      vCodeName  = FGetSetting("�������ࢄ��",?,"����ࢄ��") .
      /* ��।������ ᯨ᪠ ��ࠬ��஢ �� ஫� ��� */
      IF GetCodeBuff(vCodeName, iAcctType, BUFFER code) THEN
         ASSIGN
            vPrmListP = code.misc[1]
            vPrmListM = code.misc[2]
            vPrmList  = vPrmListP + "," + vPrmListM
         .
      ELSE
      DO:
         FIND FIRST code WHERE code.class =  vCodeName
                           AND code.name  =  iAcctType
         NO-LOCK NO-ERROR.
         IF AVAILABLE (code) THEN
            vPrmList = code.misc[3].
         ELSE
         DO:
            &IF DEFINED(PqresLog) <> 0 &THEN
            OUTPUT TO "pqres.log" APPEND.
            PUT UNFORMATTED
               "PROCEDURE GetSummDolg: " SKIP
               "���� " iAcctType " �� ������� � �����䨪��� " vCodeName SKIP.
            OUTPUT CLOSE.
            &ENDIF
            LEAVE MAIN_BLOCK.
         END.
      END.
      RUN RE_B_LOAN(iContract,iContCode,BUFFER loan).
      vCodOstpar = GetParCode(loan.class-code,'����᭄���').
           /* ����塞 ����� �㬬� ������������ */
      DO vi = 1 TO NUM-ENTRIES (vPrmList):
         vPrm = ENTRY(vi,vPrmList).
            /* �᪫�稬 �����誨 */
         IF vPrm =  "" THEN
            NEXT.
         RUN Get_Param_Cur IN h_loan(INT64(vPrm),
                                     vCodOstpar,
                                     iContract,
                                     iContCode,
                                     iSince,
                                     "",
                                     OUTPUT vTmpCurr,
                                     OUTPUT vTmpSumm,        /* �㬬� � ����� */
                                     OUTPUT vSumm).          /* �㬬� � �㡫�� */
            /* ��ࠬ����, ����� �㦭� ������ ���������� 㦥 ����⥫�묨
            ** ���⮬� ��� ᪫��뢠�� */
         oAllDolg = oAllDolg + vSumm.      /* �㡫� */
      END.
      /* ��ॢ�� �� �㡫�� � 㪠������ ������ (�᫨ ����� - �� �㡫�) */
      IF iCurrency <> "" THEN
         oAllDolg = CurToCurWork ("�������","",iCurrency,iSince,oAllDolg).

               /* �㦭� �� ࠧ��᪠ �� �ப�� */
      IF     iBegDate <> ?
         AND iEndDate <> ?
      THEN DO:
         IF NOT AVAIL loan THEN LEAVE MAIN_BLOCK.
                  /* ��筠� ������������� (����� �������) */
         IF iAcctType =  "�।��"
         THEN DO:
            RUN SUMM_SPIS_DOLG(iContract,
                               iContCode,
                               iSince,
                               iBegDate,
                               iEndDate,
                               mDelay,
                               YES,
                               OUTPUT oSummDolg).
            /* ��ॢ�� �� ������ ������� � 㪠������ ������ (�᫨ ��� �� ᮢ������) */
            IF iCurrency <> loan.currency THEN
               oSummDolg = CurToCurWork ("�������",loan.currency,iCurrency,iSince,oSummDolg).
            oByTerm = YES.
         END.
                  /* ������������� �� ��業⠬ (����� �������) */
         ELSE IF iAcctType =  "�।�" OR iAcctType =  "�।���"
         THEN DO:
            RUN sumsrok-pr-loan (RECID(loan),
                                 iSince,
                                 iBegDate,
                                 iEndDate,
                                 YES,
                                 1,
                                 OUTPUT vTmpSumm,
                                 OUTPUT oSummDolg).
            /* ��ॢ�� �� ������ ������� � 㪠������ ������ (�᫨ ��� �� ᮢ������) */
            IF iCurrency <> loan.currency THEN
               oSummDolg = CurToCurWork ("�������",loan.currency,iCurrency,iSince,oSummDolg).
            oByTerm = YES.
         END.
                  /* ��� ����祭��� �����������⥩ (�� ��㤥, ��業⠬ � �.�.) */
         ELSE IF CAN-DO ("�।��,�।��%,�।�㤊��,�।�㤏���,�।��᪏�,�।��%1",iAcctType) THEN
         DO:
            DO vi = 1 TO NUM-ENTRIES (vPrmList):
               RUN SUMM_BY_PRS_CUR (ENTRY(vi,vPrmList),
                                    iSince,
                                    iContract,
                                    iContCode,
                                    iBegDate,
                                    iEndDate,
                                    NO,
                                    OUTPUT vTmpCurr,
                                    OUTPUT vTmpSumm,  /* �㬬� � ����� ��ࠬ��� */
                                    OUTPUT vSumm).    /* �㬬� � �㡫�� */
               oSummDolg = oSummDolg + vSumm.      /* �㡫� */
            END.
            /* ��ॢ�� �� �㡫�� � 㪠������ ������ (�᫨ ����� - �� �㡫�) */
            IF iCurrency <> "" THEN
               oSummDolg = CurToCurWork ("�������","",iCurrency,iSince,oSummDolg).
            oByTerm = YES.
         END.
         /* ��� �뤠���� ��࠭⨩ */
         ELSE IF iAcctType =  "�।����"
         THEN DO:
            IF loan.cont-type = {&Sogl_Type}  THEN
               oSummDolg = 0.           
            ELSE
            DO:
               FOR EACH term-obl WHERE term-obl.contract  =  iContract
                                   AND term-obl.cont-code =  iContCode 
                                   AND term-obl.end-date  >= iBegDate
                                   AND term-obl.end-date  <= iEndDate
                                   AND term-obl.idnt      =  3 
               NO-LOCK:
                  oSummDolg = oSummDolg + term-obl.amt-rub.
               END.
               /* ��ॢ�� �� ������ ������� � 㪠������ ������ (�᫨ ��� �� ᮢ������) */
               IF iCurrency <> loan.currency THEN
                  oSummDolg = CurToCurWork ("�������",loan.currency,iCurrency,iSince,oSummDolg).

               oByTerm = YES.
            END.
         END.
         ELSE oSummDolg = oAllDolg. 
      END.
      ELSE oSummDolg = oAllDolg.
        
      IF     iAcctType       =  "�।����"
         AND loan.class-code =  "loan-guarantee" 
         AND NOT oByTerm 
         THEN oByTerm = NO.      
      ELSE
      IF iBegDate <> ?
         AND iEndDate <> ? THEN
         oByTerm = YES.
   END.

   RETURN.
END PROCEDURE.


/* ��ନ஢���� १�� �� ஫� ��� � ��⮬ �ப�� */
PROCEDURE SUMM_SPIS_RES_TYPE.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.
   DEF INPUT  PARAM iBegDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iEndDate  AS DATE   NO-UNDO.
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iFromTr   AS LOG    NO-UNDO.   /*�� ������������. ����� �뫮: ��� �࠭襩: ���� १�� � �࠭� (YES) ��� �墠�. ���. (NO) */
   DEF OUTPUT PARAM oSummRsrv AS DEC    NO-UNDO.

   DEF VAR vPrmList       AS CHAR   NO-UNDO.
   DEF VAR vTrRoleCorr    AS CHAR   NO-UNDO INIT "�।���;�।��;�।���1;�।��;
                                                  �।�����;�।�,�।���,�।����;
                                                  �।����;�।�;�।���;�।�����;
                                                  �।��%;�।��%1;�।������;
                                                  �।�㤊��;�।�������;�।�㤏���;
                                                  �।����;�।��᪄�;�।������;
                                                  �।��᪏�;�।�����;�।������".
   DEF VAR vi             AS INT64  NO-UNDO.
   DEF VAR vSumm          AS DEC    NO-UNDO.
   DEF VAR vTmpSumm       AS DEC    NO-UNDO.
   DEF VAR vCurr          AS CHAR   NO-UNDO.
   DEF VAR vOsnRoleList   AS CHAR   NO-UNDO.   /* ஫� ��⮢ ��� ���� �᭮����� ����� */
   DEF VAR vBasaOst       AS DEC    NO-UNDO.   /* ���� ��� ���� १�ࢠ �� ������� �࠭�� */
   DEF VAR vOstParamRes   AS DEC    NO-UNDO.   /* ���᫥��� १�� �� �墠�. ������� */
   DEF VAR vAllBasaOst    AS DEC    NO-UNDO.   /* ���� ��� ���� १�ࢠ �� �ᥬ �࠭蠬 */
   DEF VAR vSummDolg      AS DEC    NO-UNDO.   /* �㬬� ������������ */
   DEF VAR vAllSummDolg   AS DEC    NO-UNDO.   /* �㬬� ������������ ��� ࠧ��ᥭ�� �� �ப�� */
   DEF VAR vTranshCount   AS INT64  NO-UNDO.
   DEF VAR vRate          AS DEC    NO-UNDO.
   DEF VAR vByTerm        AS LOG    NO-UNDO.
   DEF VAR vCalcThisLoan  AS LOG    NO-UNDO. /*����砥� १�� �� ��।������ ��������*/
   DEF VAR vCalcMainLoan  AS LOG    NO-UNDO. /*������� १�� �� �墠�뢠�饬� ��������*/
   DEF VAR vTypeAcctRes   AS CHAR   NO-UNDO.    /* ஫� ��� १�ࢠ */
   DEF VAR vSummRsrvOst   AS DEC    NO-UNDO.
   DEF VAR vj             AS INT64  NO-UNDO.
   DEF VAR vTranshCls     AS CHAR   NO-UNDO.  /* ������ �࠭襩 */
   DEF VAR vOpList        AS CHAR   NO-UNDO.  /* ����樨 �� ���Ꭿ�� */
   DEF VAR vOk            AS LOG    NO-UNDO INIT FALSE.
   DEF VAR vSummRsrvRub   AS DEC    NO-UNDO.
   DEF VAR vCodeName      AS CHAR   NO-UNDO.

   DEF BUFFER loan  FOR loan. /* ���������� ����. */
   DEF BUFFER bloan FOR loan. /* ���������� ����. */
   DEF BUFFER code  FOR code. /* ���������� ����. */
   DEF BUFFER loan-acct FOR loan-acct .

   MAIN_BLOCK:
   DO:
      vCodeName  = FGetSetting("�������ࢄ��",?,"����ࢄ��") .
            /* ��।������ ᯨ᪠ ��ࠬ��஢ �� ஫� ��� */
      IF GetCodeBuff("����ࢄ��", iAcctType, BUFFER code) THEN
         ASSIGN
            vPrmList = code.misc[3]
            vTypeAcctRes = code.name.
      ELSE
      DO:
         FIND FIRST code WHERE code.class =  vCodeName
                           AND code.name  =  iAcctType
         NO-LOCK NO-ERROR.
         IF AVAILABLE (code) THEN
            ASSIGN
               vPrmList = code.misc[3]
               vTypeAcctRes = code.name.
         ELSE
            LEAVE MAIN_BLOCK.
      END.

      IF mPutProt THEN
      DO:
         OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
         PUT STREAM sOut UNFORMATTED
            '����������: �ᯮ��㥬� ��ࠬ���� �� �����䨪���' vCodeName
            ' ��� ���� १�ࢠ' SKIP 
            '   �������: '            TRIM(iContCode) SKIP
            '   ���᮪ ��ࠬ��஢: '  TRIM(vPrmList) SKIP
            '   ���� ��� १�ࢠ: ' TRIM(vTypeAcctRes)
         SKIP.
         OUTPUT STREAM sOut CLOSE.
      END.

      RUN RE_B_LOAN(iContract,iContCode,BUFFER loan).

      /* �뤥�塞 �������� �� ����� �����४⥭ ����� ��㡮��� ������  */
      /* ���Ꭿ��, ����焮�, rel_type                                    */
      /* � ��砥: alt-contract BEGINS "mm" - �������� ���. �뭪�,       */
      /* ����樨 (१��) ������� �� ���.�㬬��                        */
      /* ���� ᮮ⢥������ ��⪠ � ���� vCalcThisLoan,vCalcMainLoan */

      RUN CHECK_CHOWHE_TRANSH (iContract,
                               iContCode,
                               iAcctType,
                               OUTPUT vOk).

      IF    (NUM-ENTRIES(iContCode," ") >  1 AND NOT vOk)
         OR (loan.cont-type = {&Sogl_Type} AND vOk) THEN 
      DO: 
         oSummRsrv = 0.
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               '������: ' + (IF (loan.cont-type = {&Sogl_Type} AND vOk) THEN
                                '�墠�뢠�騩 �������. ����� ������� �� �࠭��.'
                             ELSE
                                '�࠭襢� �������. ����� ������� �� �墠�뢠�饬 �������.')  SKIP
               '   �������: '       TRIM(iContCode) SKIP
               '   ����: '          TRIM(iAcctType)
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
         RETURN.
      END.

       /* �᫨ ������� ᮣ��襭�� � १�� ������� �� �࠭�� - �����頥� ����.
          �᫨ ������� �࠭� � १�� ������� �� ᮣ��襭��, � �㬬� ���. १�ࢠ ��६ � ᮣ��襭��
       */

      /* �᫨ ������� ���� �࠭襬 */
      IF NUM-ENTRIES(iContCode," ") =  2
          AND CAN-FIND (FIRST bloan WHERE bloan.contract  =  iContract
                                AND bloan.cont-code =  ENTRY(1,iContCode," ")
                       NO-LOCK) THEN
         IF LnRsrvCheckType (iContract,iContCode,vTypeAcctRes) THEN /* � १�� ������� �� �⮬ �࠭� */
         DO: 
            vCalcThisLoan = YES. /*�㤥� ����� ���. १�� �� �࠭�� (�.�. �� ��।������ ��������)*/
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: ������� ���� �࠭襬 � १�� ������� �� �࠭�. ����� १�ࢠ �� �࠭��' SKIP 
                  '   �������: '  TRIM(iContCode)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
         ELSE
         DO:
            vCalcMainLoan = YES. /*���� �㤥� ����� १�� �� ᮣ��襭�� (�墠�뢠�饬� ��������)*/
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: ������� ���� �࠭襬, � १�� ������� �� �墠�뢠�饬 �������. ����� १�ࢠ �� �墠�뢠�饬� ��������' SKIP  
                  '   �������: '  TRIM(iContCode)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
      ELSE /*��।���� ������� ���� ����� ��� ᮣ��襭���*/
         IF loan.cont-type = {&Sogl_Type} THEN 
         DO:
            IF LnRsrvCheckType (iContract,iContCode,vTypeAcctRes) THEN  /*१�� ������� �� ���, � �� �� ��� �࠭��*/
            DO:
               vCalcThisLoan = YES. /*�㤥� ����� १�� �� ��।������ ��������*/
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     '����������: ������� ���� �墠�뢠�騬 � १�� ������� �� �墠�뢠�饬 �������. ����� १�ࢠ �� �墠�뢠�饬� ��������' SKIP  
                     '   �������: '  TRIM(iContCode)
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
            END.
            ELSE
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     '����������: ������� ���� �墠�뢠�騬, � १�� ������� �� �࠭��. ����� १�ࢠ �� �࠭蠬' SKIP  
                     '   �������: '  TRIM(iContCode)
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
         END.
         ELSE
         DO:
            vCalcThisLoan = YES.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: ����� १�ࢠ �� ��।������ ��������' SKIP  
                  '   �������: '  TRIM(iContCode)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
      
      /*����� �� �墠�뢠饬� ��������*/
      IF vCalcMainLoan
      THEN DO:
         vi = LOOKUP(iAcctType,vTrRoleCorr,";").
         IF     iAcctType  BEGINS "�।���"
            AND vi         >  0
         THEN vOsnRoleList = ENTRY(vi + 1,vTrRoleCorr,";").
         ELSE vOsnRoleList = iAcctType.
               /* ����塞 ���� ��� ���� १�ࢠ �� ������� �࠭�� (�㬬� ��ࠬ��஢ �� ������ �������-�࠭�) */
         DO vi = 1 TO NUM-ENTRIES(vOsnRoleList):
            RUN GetSummDolg(iContract,
                            iContCode,
                            ENTRY(vi,vOsnRoleList),
                            iSince,
                            iBegDate,
                            iEndDate,
                            iCurrency,
                            OUTPUT vSummDolg,
                            OUTPUT vAllSummDolg,
                            OUTPUT vByTerm).
            vBasaOst = vBasaOst + vSummDolg.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: �㬬� �� ஫� ��� ���� १�ࢠ' SKIP  
                  '   �������: '   TRIM(iContCode) SKIP
                  '   ����: '      TRIM(ENTRY(vi,vOsnRoleList)) SKIP
                  '   �㬬�: '     TRIM(STRING(vSummDolg, "->>>>>>>>>>>>>>9.99"))
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               '����������: ���� ��� ���� १�ࢠ' SKIP  
               '   �������: '       TRIM(iContCode) SKIP
               '   �㬬�: '         TRIM(STRING(vBasaOst, "->>>>>>>>>>>>>>9.99"))
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
         IF vBasaOst =  0 THEN oSummRsrv = 0.
         ELSE DO:
            /* ��।��塞 �㬬� ����. १�ࢠ �� �墠�. ������� (���⮪ �� ᮮ�. ��ࠬ����)
            ** �.�. १�� ������� �� �墠�뢠�饬 �������, � ��� ���᫥��� १�� -
            ** १�� �� �ᥬ �࠭蠬. � ����. १�� �� ������� �࠭�� ������ ����
            ** ����⠭ ��� ���� ��饣� १�ࢠ, ᮮ⢥������� ���� ��易⥫��� �� �������� */
            DO vi = 1 TO NUM-ENTRIES(vPrmList):
               /*���뢠�� ࠧ����� �� �ப��*/
               IF iBegDate <> ? AND iEndDate <> ?
                   AND ENTRY(vi,vPrmList) =  "21"
               THEN
                  RUN SUMM_SPIS_RES(iContract,
                                 ENTRY(1,iContCode," "),
                                 iSince,
                                 iBegDate,
                                 iEndDate,
                                 OUTPUT vSumm).    /* ����� ������� � �㡫��*/
               ELSE /*��� ࠧ����� �� �ப��*/
                  RUN SUMM_BY_PARAM_CUR(ENTRY(vi,vPrmList),
                                     iSince,
                                     iContract,
                                     ENTRY(1,iContCode," "),
                                     OUTPUT vCurr,
                                     OUTPUT vTmpSumm,        /* �㬬� � ����� */
                                     OUTPUT vSumm).          /* �㬬� � �㡫�� */

               /* ��ॢ�� �� �㡫�� � 㪠������ ������ (�᫨ ����� - �� �㡫�) */
               IF iCurrency <> "" THEN
                  vSumm = CurToCurWork ("�������","",iCurrency,iSince,vSumm).
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     '����������: ����� ���᫥����� १�ࢠ �� ��ࠬ��ࠬ' SKIP  
                     '   �������: '  TRIM(iContCode) SKIP
                     '   ��ࠬ���: ' TRIM(ENTRY(vi,vPrmList)) SKIP
                     '   �㬬�: '    TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
                     '   �����: '   TRIM(iCurrency)
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
               /* ���⠥�, �⮡� �㬬� ���᫥����� १�ࢠ �뫠 ������⥫쭮�
               ��� ���४⭮�� ������� */              
               vOstParamRes = vOstParamRes - vSumm.
            END.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: ���᫥��� १�� ' SKIP  
                  '   �������: '  TRIM(iContCode) SKIP
                  '   �㬬�: '    TRIM(STRING(vOstParamRes, "->>>>>>>>>>>>>>9.99"))
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
            vTranshCount = 0.
            FIND FIRST tt-SumDolg WHERE tt-SumDolg.contract  =  iContract
                                    AND tt-SumDolg.cont-code =  ENTRY(1,iContCode," ")
                                    AND tt-SumDolg.AcctType  =  vOsnRoleList
                                    AND tt-SumDolg.since     =  iSince
               NO-LOCK NO-ERROR.
            IF AVAILABLE tt-SumDolg THEN
            DO: 
               ASSIGN
                  vAllBasaOst  = tt-SumDolg.AllBasaOst
                  vTranshCount = tt-SumDolg.TranshCount 
               .
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     '����������: ���� ��� ���� १�ࢠ �� �ᥬ �࠭蠬 ' SKIP  
                     '   �������: '  TRIM(iContCode) SKIP
                     '   �㬬�: '   TRIM(STRING(vAllBasaOst, "->>>>>>>>>>>>>>9.99")) SKIP
                     '   ���-�� �࠭襩: '   TRIM(STRING(vTranshCount))
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
            END.
            ELSE DO:
               /* ��।��塞 �㬬� �� ��ࠬ��ࠬ ��� ��� �࠭襩 �墠�. ������� ������� �࠭� */
               FOR EACH bloan WHERE bloan.contract  =  iContract
                                AND bloan.cont-code BEGINS ENTRY(1,iContCode," ") + " "
               NO-LOCK:
                  DO vi = 1 TO NUM-ENTRIES(vOsnRoleList):
                     RUN GetSummDolg(bloan.contract,
                                     bloan.cont-code,
                                     ENTRY(vi,vOsnRoleList),
                                     iSince,
                                     iBegDate,
                                     iEndDate,
                                     iCurrency,
                                     OUTPUT vSummDolg,
                                     OUTPUT vAllSummDolg,
                                     OUTPUT vByTerm).
                     IF mPutProt THEN
                     DO:
                        OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                        PUT STREAM sOut UNFORMATTED
                           '����������: ���� ��� ���� १�ࢠ �� �����⭮�� �࠭�� ' SKIP  
                           '   �������: ' TRIM(bloan.cont-code) SKIP
                           '   ����: '    TRIM(ENTRY(vi,vOsnRoleList)) SKIP
                           '   �㬬�: '   TRIM(STRING(vSummDolg, "->>>>>>>>>>>>>>9.99"))
                        SKIP.
                        OUTPUT STREAM sOut CLOSE.
                     END.
                     vAllBasaOst = vAllBasaOst + vSummDolg.
                  END.
                  vTranshCount = vTranshCount + 1.
               END.
               IF mPutProt THEN
               DO:
                  OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
                  PUT STREAM sOut UNFORMATTED
                     '����������: ���� ��� ���� १�ࢠ �� �ᥬ �࠭蠬 ' SKIP  
                     '   �������: '  TRIM(iContCode) SKIP
                     '   �㬬�: '   TRIM(STRING(vAllBasaOst, "->>>>>>>>>>>>>>9.99")) SKIP
                     '   ���-�� �࠭襩: '   TRIM(STRING(vTranshCount))
                  SKIP.
                  OUTPUT STREAM sOut CLOSE.
               END.
               CREATE tt-SumDolg.
               ASSIGN
                  tt-SumDolg.contract    = iContract
                  tt-SumDolg.cont-code   = ENTRY(1,iContCode," ")
                  tt-SumDolg.AcctType    = vOsnRoleList
                  tt-SumDolg.since       = iSince
                  tt-SumDolg.AllBasaOst  = vAllBasaOst
                  tt-SumDolg.TranshCount = vTranshCount
                .
            END.
            /* �����뢠�� �㬬� १�ࢠ */
            ASSIGN
               vRate = IF vAllBasaOst =  0 THEN 0 ELSE (vBasaOst / vAllBasaOst)
               oSummRsrv = ROUND(vRate * vOstParamRes,2)
            .
                       
            /* ᪮�४��㥬 ���㣫���� �� ��᫥���� �࠭� */
            IF    oSummRsrv <> 0 
              AND vRate     <> 1 THEN 
            DO:
               ASSIGN
                  vSummRsrvOst = 0 
                  vI           = 0 
               .

               FOR EACH tt-DataLoan WHERE
                            tt-DataLoan.contract  =  iContract  
                        AND tt-DataLoan.cont-code BEGINS ENTRY(1,iContCode," ") + " " 
                        AND tt-DataLoan.since     =  iSince 
               NO-LOCK :
                  ASSIGN
                     vSummRsrvOst = vSummRsrvOst + tt-DataLoan.rsrv-summ 
                     vI = vI + 1 
                  .                 
               END.           
               IF vI =  vTranshCount - 1 THEN 
                  oSummRsrv = vOstParamRes - vSummRsrvOst .
                      
            END.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: ����⠭�� १�� ' SKIP  
                  '   �������: '  TRIM(iContCode) SKIP
                  '   �㬬�: '    TRIM(STRING(oSummRsrv, "->>>>>>>>>>>>>>9.99")) SKIP
                  '   %���: '     TRIM(STRING(vRate, "->>>>>>>>>>>>>>9.99999"))
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END. 
         END.
      END.     /* ���� �� �墠�뢠�饬� �������� */

      ELSE
      /*����� �� ��।������ ��������*/
      IF vCalcThisLoan THEN
      DO:
         DO vi = 1 TO NUM-ENTRIES(vPrmList):
                     /* ����� ࠧ����� �� �ப�� ⮫쪮 १�� �� ��㤭�� ������������ */
            IF     iBegDate <> ?
               AND iEndDate <> ?
               AND ENTRY(vi,vPrmList) =  "21"
            THEN DO:
               RUN SUMM_SPIS_RES(iContract,
                                 iContCode,
                                 iSince,
                                 iBegDate,
                                 iEndDate,
                                 OUTPUT vSumm).    /* �������⭠� ����� */
               /* ��।��塞 ������ ��ࠬ��� */
               RUN GetParP IN h_loan (RECID(loan),
                                      INT64(ENTRY(vi,vPrmList)),
                                      vSumm,
                                      OUTPUT vTmpSumm,  /* �㬬� � �㡫�� */
                                      OUTPUT vCurr).
               /* ��ॢ�� �� ������ ��ࠬ��� � 㪠������ ������ */
               IF iCurrency <> vCurr THEN
                  vSumm = CurToCurWork ("�������",vCurr,iCurrency,iSince,vSumm).
            END.
                     /* ��� ࠧ��᪨ �� �ப�� */
            ELSE DO:

               RUN SUMM_BY_PARAM_CUR(ENTRY(vi,vPrmList),
                                     iSince,
                                     iContract,
                                     iContCode,
                                     OUTPUT vCurr,
                                     OUTPUT vTmpSumm,        /* �㬬� � ����� */
                                     OUTPUT vSumm).          /* �㬬� � �㡫�� */
               /* ��ॢ�� �� �㡫�� � 㪠������ ������ (�᫨ ����� - �� �㡫�) */
               IF iCurrency <> "" THEN
                  vSumm = CurToCurWork ("�������","",iCurrency,iSince,vSumm).
            END.
            
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: ����� ���᫥����� १�ࢠ ' SKIP  
                  '   �������: '  TRIM(iContCode) SKIP
                  '   ��ࠬ���: ' TRIM(ENTRY(vi,vPrmList)) SKIP
                  '   �㬬�: '    TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
                  '   �����: '   TRIM(iCurrency)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END. 
            /* ���⠥�, �⮡� �㬬� ���᫥����� १�ࢠ �뫠 ������⥫쭮�
               ��� ���४⭮�� ������� */
            oSummRsrv = oSummRsrv - vSumm.
         END. /* ���� �� ��।������ ��������*/
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               '����������: ���᫥��� १�� ' SKIP  
               '   �������: '  TRIM(iContCode) SKIP
               '   �㬬�: '    TRIM(STRING(oSummRsrv, "->>>>>>>>>>>>>>9.99"))
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
      END.
   END. /*MAIN_BLOCK*/
   

   /* �������� �ॡ���� ࠧ������ १�ࢠ */
 
     /* �饬  ��� */
      FIND LAST loan-acct WHERE loan-acct.contract  =  iContract
                            AND loan-acct.cont-code =  iContCode
                            AND loan-acct.acct-type =  iAcctType
                            AND loan-acct.since     <= iSince
           NO-LOCK NO-ERROR.


      vPrmList = GetCodeMisc(vCodeName, iAcctType, 3) .
      FIND FIRST code WHERE code.class   =  vCodeName
                        AND code.parent  =  vCodeName
                        AND code.code    <>  iAcctType
                        AND code.misc[3] =  vPrmList
            NO-LOCK NO-ERROR .

      IF     oSummRsrv <> 0
         AND AVAIL loan-acct         
         AND AVAIl code      THEN
      DO:

         RUN GetPartReservRole  IN h_rsrv ( loan-acct.acct,
                                 iCurrency,
                                 iSince,
                                 iAcctType,
                    INPUT-OUTPUT oSummRsrv,
                    INPUT-OUTPUT vSummRsrvRub ) .
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               '����������: �᫨ �ॡ���� ࠧ������ १�ࢠ ' SKIP  
               '   ���: '        TRIM(loan-acct.acct) SKIP
               '   ����: '        TRIM(iAcctType) SKIP
               '   �����: '      TRIM(iCurrency) SKIP
               '   �㬬�: '       TRIM(STRING(oSummRsrv, "->>>>>>>>>>>>>>9.99")) SKIP
               '   �㬬� � ���: ' TRIM(STRING(vSummRsrvRub, "->>>>>>>>>>>>>>9.99"))
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
      END.

   RETURN.
END PROCEDURE.


/* �����頥� ����� १�� � ����� �����樥�� �᪠ �� ஫� ��� */
PROCEDURE LnRsrvByType.
   DEF INPUT  PARAM iAcct     AS CHAR   NO-UNDO. /* ����� ��� */
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO. /* ஫� ��� */
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO. /* �����, � ���ன ���� ������� ����� */
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iDate     AS DATE   NO-UNDO. /* ���, �� ������ �஢����� ���� */
   DEF OUTPUT PARAM oRsrvProc AS DEC    NO-UNDO.
   DEF OUTPUT PARAM oRsrvRate AS DEC    NO-UNDO.
   
   /* ��������㥬 ⨯ ஫� ��� */
            /* ���ᥫ� */
   IF      iAcct BEGINS "51"
      AND (   iAcctType =  "�।�"
           OR iAcctType =  "�।��᪄�"
           OR iAcctType =  "�।��%"
           OR iAcctType =  "�।��᪏�")
   THEN
       RUN LnFormRsrvProc (iContract,
                          iContCode,
                          iDate,
                          iAcctType,
                          iCurrency,
                          NO,
                          OUTPUT oRsrvProc,
                          OUTPUT oRsrvRate).
            /* �ॡ������ �� ��業⠬ */
   ELSE IF iAcctType =  "�।�" OR iAcctType =  "�।���" THEN
   DO:
      ASSIGN
         oRsrvProc = LnFormRsrvRoleSpis (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType) 
         oRsrvRate = LnFormRsrvCalcRole (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType)
      . 
   END.
            /* ����祭�� ��業�� */
   ELSE IF iAcctType =  "�।��%" OR iAcctType =  "�।��%1" THEN
   DO:
      ASSIGN
         oRsrvProc = LnFormRsrvRoleSpis (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType)
         oRsrvRate = LnFormRsrvCalcRole (iContract,
                                         iContCode,
                                         iDate,
                                         iCurrency,
                                         iAcctType)
      . 
   END.
            /* �����ᨨ, �����, ���� */
   ELSE IF CAN-DO("�।�㤊��,�।�㤏���",iAcctType) THEN
   DO:
      oRsrvProc = LnCalcRsrvProc (iContract,
                                  iContCode,
                                  iDate,
                                  iCurrency).
      oRsrvRate = LnFormRsrvCom (iContract,
                                 iContCode,
                                 iDate,
                                 iCurrency,
                                 iAcctType).
   END.
   ELSE IF iAcctType =  "�।��" THEN
   DO:
       ASSIGN
         oRsrvProc =  LnRsrvRate(iContract,
                                 iContCode,
                                 iDate)
         oRsrvRate = LnFormRsrvBadDebtTransh (iContract,
                                              iContCode,
                                              iDate,
                                              iCurrency)
      .
   END.
   /* �᫨ �� ��㣠� ஫� ���, � ��⠥� ��� ��� "�।��", �.�.
   ** ����. १�ࢨ஢���� + ��.�᪠ ������ � ������� (oRsrvProc - ����. १) */
   ELSE ASSIGN
        oRsrvProc =  LnRsrvRate(iContract,
                                iContCode,
                                iDate)
        oRsrvRate = LnFormRsrvGoodDebtTransh(iContract,
                                                  iContCode,
                                                  iDate,
                                                  iCurrency).

   RETURN.
END PROCEDURE.

/* ��楤�� ���᪠ ��� १�ࢠ ������� */
PROCEDURE GetLoanAcctRsrv.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.   /* ������祭�� */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.   /* ����� ������� */
   DEF INPUT  PARAM iAcctType AS CHAR   NO-UNDO.   /* ஫� ���, �� ���ன ��।������ ⨯ १�ࢠ */
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.   /* ���, �� ������ ����� ��� १�ࢠ */
   DEF OUTPUT PARAM oAcct     AS CHAR   NO-UNDO.   /* ����� ��� १�ࢠ */
   DEF OUTPUT PARAM oCurrency AS CHAR   NO-UNDO.   /* ����� ��� १�ࢠ */
   DEF OUTPUT PARAM oRsrvRole AS CHAR   NO-UNDO.   /* ஫� ��� १�ࢠ */
   
   DEF BUFFER loan      FOR loan.
   DEF BUFFER loan-acct FOR loan-acct.

   DEF VAR vCodeName AS CHAR NO-UNDO.

   MAIN_BLOCK:
   DO:
      ASSIGN
         vCodeName  = FGetSetting("�������ࢄ��",?,"����ࢄ��")
         oRsrvRole  = GetCodeName(vCodeName,iAcctType).
      IF NOT {assigned oRsrvRole}
         THEN LEAVE MAIN_BLOCK.

      RUN RE_L_ACCT IN h_loan (iContract,
                               iContCode,
                               oRsrvRole,
                               iSince,
                               BUFFER loan-acct).
      IF AVAIL loan-acct THEN
         ASSIGN oAcct      = loan-acct.acct
                oCurrency  = loan-acct.currency
         .

      IF NOT {assigned oAcct}
         AND NUM-ENTRIES(iContCode, " ") = 2 THEN
      DO:
         FIND FIRST loan WHERE
                    loan.contract  = iContract
                AND loan.cont-code = ENTRY(1, iContCode, " ")
         NO-LOCK NO-ERROR.
         
         IF     AVAIL loan 
            AND CAN-DO(GetXAttrInit(loan.class-code,"Rel_Type"), oRsrvRole) THEN
         DO:
            RUN RE_L_ACCT IN h_loan (loan.contract,
                                     loan.cont-code,
                                     oRsrvRole,
                                     iSince,
                                     BUFFER loan-acct).

            IF AVAIL loan-acct THEN
               ASSIGN 
                  oAcct      = loan-acct.acct
                  oCurrency  = loan-acct.currency
               .
         END.
      END.
   END.
   RETURN.
END PROCEDURE.

/* �����頥� ��ନ஢���� १�� �� ᯨ�� ��ࠬ��஢ � ࠧ������ �� �࠭蠬
   ��宦� �� PROCEDURE SUMM_SPIS_RES_TYPE, ⮫쪮 ��� ���ਭ�, ��� � ����॥ */
{pfuncdef 
 &DefFunc = "LnRsrvByPar"
 &Description = "�����頥� ��ନ஢���� १�� ~
 �� ᯨ�� ��ࠬ��஢ � ࠧ������ �� �࠭蠬"
 &Parameters = "�����祭�� �������,����� �������,~
 ���᮪ ��ࠬ��஢,��� ����,�㬬� १�ࢠ"
 &Result= "���祭�� �� �६����� ⠡��� tt-DataLoanInstr139"
 &Sample="RUN LnRsrvByPar IN h_pqres ( ~
 '�।��', ~
   'br_705@002', ~
   '21,46', ~
   TODAY, ~
   OUTPUT vAmt)."}
PROCEDURE LnRsrvByPar:
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iSpisPar  AS CHAR   NO-UNDO. /* ᯨ᮪ ��ࠬ��஢ */
   DEF INPUT  PARAM iDate     AS DATE   NO-UNDO. /* ���, �� ������ �஢����� ���� */
   DEF OUTPUT PARAM oRsrvSum  AS DEC    NO-UNDO. /* �㬬� १�ࢠ */

   DEF VAR vIsThis   AS LOG   NO-UNDO.
   DEF VAR vPar      AS CHAR  NO-UNDO.
   DEF VAR vParInt   AS INT64 NO-UNDO.
   DEF VAR vPar2     AS CHAR  NO-UNDO.
   DEF VAR vPar2Int  AS INT64 NO-UNDO.
   DEF VAR vCounter  AS INT64 NO-UNDO.
   DEF VAR vCounter2 AS INT64 NO-UNDO.
   DEF VAR vAmtLn    AS DEC   NO-UNDO.
   DEF VAR vAmtTr    AS DEC   NO-UNDO.
   DEF VAR vDif      AS DEC   NO-UNDO.
   DEF VAR vMaxSum   AS DEC   NO-UNDO.
   DEF VAR vMaxRow   AS ROWID NO-UNDO.
   DEF VAR vTmpDec   AS DEC   NO-UNDO EXTENT 3.
   DEF VAR vCodeName AS CHAR  NO-UNDO.

   DEF BUFFER loan  FOR loan.
   DEF BUFFER bloan FOR loan.
   DEF BUFFER bcode FOR code.

   MAIN:
   DO:
      vCodeName  = FGetSetting("�������ࢄ��",?,"����ࢄ��").
      FIND FIRST loan WHERE 
                 loan.contract =  iContract 
             AND loan.cont-code =  iContCode 
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE MAIN.
      LOOP_PRM:
      DO vCounter = 1 TO NUM-ENTRIES (iSpisPar):
         ASSIGN
            vPar = ENTRY(vCounter, iSpisPar)
            /* �᫨ �ਢ� ��ॢ������, � ᫥���騩 ��ࠬ��� */
            vParInt = INT64(vPar) 
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            LEAVE LOOP_PRM.
         vIsThis = YES.
            /* ��।��塞 ��� ������� १�� � ��砥 �࠭襢��� ������� */
         IF NUM-ENTRIES (loan.cont-code, " ") >  1 
            OR loan.cont-type =  "��祭��" THEN
         DO:
               /* ��室�� ஫� ��� १�ࢠ �� ��ࠬ���� */
            FIND FIRST tt-ReservDog WHERE
                       tt-ReservDog.res_par =  vPar
            NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-ReservDog THEN
            DO:
                  /* �᫨ ���� �����, � 㦥 ������﫨 ࠭�� � ᪮॥ �ᥣ� �ਢ�� ��ࠬ��� */
               IF CAN-FIND (FIRST tt-ReservDog NO-LOCK) THEN
                  LEAVE MAIN.
                  /* �᫨ �� ��諨, � ������塞 �६����� ⠡���� �� �����䨪��� */
               FOR EACH bcode WHERE
                        bcode.class  =  vCodeName
                    AND bcode.parent =  vCodeName
               NO-LOCK:
                  FIND FIRST tt-ReservDog WHERE
                             tt-ReservDog.res_par =  bcode.misc[3]
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL tt-ReservDog THEN
                  DO:
                     CREATE tt-ReservDog.
                     ASSIGN
                        tt-ReservDog.res_par = bcode.misc[3]
                        tt-ReservDog.res_sch = bcode.name
                     .
                  END.
                     IF {assigned bcode.misc[1]} THEN
                        {additem.i tt-ReservDog.plus_par bcode.misc[1]}
                     IF {assigned bcode.misc[2]} THEN
                        {additem.i tt-ReservDog.minus_par bcode.misc[2]}
               END.
                  /* ��२᪨���� */
               FIND FIRST tt-ReservDog WHERE
                          tt-ReservDog.res_par =  vPar
               NO-LOCK NO-ERROR.
                  /* �᫨ �� ��諨, � ��ࠬ��� ��������� � �����䨪��� */
               IF NOT AVAIL tt-ReservDog THEN
               DO:
                  LEAVE MAIN.
               END.
            END.
            vIsThis = LnRsrvCheckType (loan.contract,
                                       loan.cont-code,
                                       tt-ReservDog.res_sch).
         END. /* IF NUM-ENTRIES (loan.cont-code, " ") GT 1 
                              OR loan.cont-type EQ "��祭��" THEN */
            /* ��� ������ ������஢ � �墠�뢠���, 
               � ⠪ �� ��� �࠭襩 � �������� १�ࢠ �� ��� */
         IF vIsThis THEN
         DO:
               /* ��� �墠�뢠�饣� ������� �஢��塞 ����稥 ��ࠬ��஢ �ॡ������,
                  �᫨ �� ���, � ����� १�� �㤥� ��।���� ����� �࠭蠬� */
            IF loan.cont-type =  "��祭��" THEN
            CHECK_KL:
            DO:
               LOOP_PRM2:
               DO vCounter2 = 1 TO NUM-ENTRIES (tt-ReservDog.plus_par):
                  ASSIGN
                     vPar2 = ENTRY(vCounter2, tt-ReservDog.plus_par)
                        /* �᫨ �ਢ� ��ॢ������, � ᫥���騩 ��ࠬ��� */
                     vPar2Int = INT64(vPar2) 
                  NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN
                     LEAVE LOOP_PRM2.
                  RUN RE_PARAM_EX IN h_Loan (vPar2Int,
                                             iDate,
                                             loan.since,
                                             loan.contract,
                                             loan.cont-code,
                                             OUTPUT vTmpDec[1], /* ���祭�� */
                                             OUTPUT vTmpDec[2],
                                             OUTPUT vTmpDec[3]).
                     /* �᫨ ��ࠬ��� ����, � ��⠥� १�� ��� � ���筮�� ������� */
                  IF vTmpDec[1] >  0 THEN
                     LEAVE CHECK_KL.
               END.
                  /* �᫨ ��ࠬ��� �ॡ������ ���, 
                     � ���室�� � ᫥���饬� ��ࠬ���� १�ࢠ */
               NEXT LOOP_PRM.
            END.
            RUN RE_PARAM_EX IN h_Loan (vParInt,
                                       iDate,
                                       loan.since,
                                       loan.contract,
                                       loan.cont-code,
                                       OUTPUT vTmpDec[1], /* ���祭�� */
                                       OUTPUT vTmpDec[2],
                                       OUTPUT vTmpDec[3]).
            oRsrvSum = oRsrvSum - vTmpDec[1]. /* ��ࠬ��� १�ࢠ ����⥫�� */
         END.
            /* ��� �࠭襩 � ��砥 ������� १�ࢠ �� �墠�뢠�饬 �������, 
               � �ॡ������ �� �⤥���� �࠭�� ��।��塞 ���� �⭮������� � �࠭�� */
         ELSE
         DO:
               /* ���饬 ������� �� �६����� ⠡��窥 */
            FIND FIRST tt-ResLoan WHERE
                       tt-ResLoan.res_par   =  vPar
                   AND tt-ResLoan.cont-code =  loan.cont-code
            NO-LOCK NO-ERROR.
               /* �᫨ �� ��諨 � ������塞 �� �ᥬ� �࠭襢��� �������� */
            IF NOT AVAIL tt-ResLoan THEN
            DO:
               vAmtLn = 0.
               FOR EACH bloan WHERE 
                        bloan.contract  =  loan.contract 
                    AND bloan.cont-code BEGINS ENTRY(1, loan.cont-code, " ") + " "
                    AND NUM-ENTRIES (bloan.cont-code, " ") >  1 NO-LOCK:
                     /* �㬬��㥬 ��ࠬ���� �ॡ������ */
                  vAmtTr = 0.
                     /* ����㥬 */
                  LOOP_PRM2:
                  DO vCounter2 = 1 TO NUM-ENTRIES (tt-ReservDog.plus_par):
                     ASSIGN
                        vPar2 = ENTRY(vCounter2, tt-ReservDog.plus_par)
                           /* �᫨ �ਢ� ��ॢ������, � ᫥���騩 ��ࠬ��� */
                        vPar2Int = INT64(vPar2) 
                     NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN
                        LEAVE LOOP_PRM2.
                     RUN RE_PARAM_EX IN h_Loan (vPar2Int,
                                                iDate,
                                                bloan.since,
                                                bloan.contract,
                                                bloan.cont-code,
                                                OUTPUT vTmpDec[1], /* ���祭�� */
                                                OUTPUT vTmpDec[2],
                                                OUTPUT vTmpDec[3]).
                     vAmtTr = vAmtTr + vTmpDec[1].
                  END.
                     /* �����㥬 */
                  LOOP_PRM2:
                  DO vCounter2 = 1 TO NUM-ENTRIES (tt-ReservDog.minus_par):
                     ASSIGN
                        vPar2 = ENTRY(vCounter2, tt-ReservDog.minus_par)
                           /* �᫨ �ਢ� ��ॢ������, � ᫥���騩 ��ࠬ��� */
                        vPar2Int = INT64(vPar2) 
                     NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN
                        LEAVE LOOP_PRM2.
                     RUN RE_PARAM_EX IN h_Loan (vPar2Int,
                                                iDate,
                                                bloan.since,
                                                bloan.contract,
                                                bloan.cont-code,
                                                OUTPUT vTmpDec[1], /* ���祭�� */
                                                OUTPUT vTmpDec[2],
                                                OUTPUT vTmpDec[3]).
                     vAmtTr = vAmtTr - vTmpDec[1].
                  END.
                  CREATE tt-ResLoan.
                  ASSIGN 
                     tt-ResLoan.res_par   = vPar
                     tt-ResLoan.cont-code = bloan.cont-code
                     tt-ResLoan.sum_tr    = vAmtTr
                  .
                  vAmtLn = vAmtLn + vAmtTr.
               END. /* FOR EACH bloan WHERE */
                  /* ����塞 �㬬� १�ࢠ, �� �� �墠�뢠�饬 ������� */
               FIND FIRST bloan WHERE
                          bloan.contract  =  loan.contract
                      AND bloan.cont-code =  ENTRY(1, loan.cont-code, " ")
               NO-LOCK NO-ERROR.
               IF NOT AVAIL bloan THEN
                  NEXT LOOP_PRM.
               RUN RE_PARAM_EX IN h_Loan (vParInt,
                                          iDate,
                                          bloan.since,
                                          bloan.contract,
                                          bloan.cont-code,
                                          OUTPUT vTmpDec[1], /* ���祭�� */
                                          OUTPUT vTmpDec[2],
                                          OUTPUT vTmpDec[3]).
                   /* �᫨ ���� �� �� ��।����� */
               IF vAmtLn <> 0 THEN
               DO:
                  /* ��।��塞 १�� �ய��樮���쭮 */
                  ASSIGN
                     vDif    = 0 - vTmpDec[1]
                     vMaxSum = 0
                     vMaxRow = ?
                  .
                  FOR EACH tt-ResLoan WHERE
                           tt-ResLoan.res_par   =  vPar
                       AND tt-ResLoan.cont-code BEGINS bloan.cont-code + " "
                       AND NUM-ENTRIES(tt-ResLoan.cont-code, " ") >  1
                  EXCLUSIVE-LOCK:
                     ASSIGN
                        tt-ResLoan.sum_res = 0 - ROUND (vTmpDec[1] / vAmtLn * tt-ResLoan.sum_tr, 2)
                        vDif = vDif - tt-ResLoan.sum_res
                     .
                        /* ���������� ��ப� � ���ᨬ��쭮� �㬬�� */
                     IF vMaxSum <  tt-ResLoan.sum_res THEN
                        ASSIGN
                           vMaxSum = tt-ResLoan.sum_res
                           vMaxRow = ROWID (tt-ResLoan)
                        .
                  END.
                     /* ��������㥬 ����譮��� */
                  IF vDif <> 0 THEN
                  DO:
                     FIND FIRST tt-ResLoan WHERE 
                                ROWID (tt-ResLoan) =  vMaxRow 
                     NO-ERROR.
                     IF AVAIL tt-ResLoan THEN
                        tt-ResLoan.sum_res = tt-ResLoan.sum_res + vDif.
                  END.
               END.
                  /* ��� ���������, ⥯��� ��२饬 */
               FIND FIRST tt-ResLoan WHERE
                          tt-ResLoan.res_par   =  vPar
                      AND tt-ResLoan.cont-code =  loan.cont-code
               NO-LOCK NO-ERROR.
                  /* �᫨ ���� �� ������, � �� ������筠� �����... */
               IF NOT AVAIL tt-ResLoan THEN
                  NEXT LOOP_PRM.
            END. /* IF NOT AVAIL tt-ResLoan THEN */
            oRsrvSum = oRsrvSum + tt-ResLoan.sum_res.
         END. /* IF vIsThis THEN */
      END. /* DO vCounter = 1 TO NUM-ENTRIES (iSpisPar): */
   END. /* MAIN: */
END PROCEDURE.

/* ����祭�� ������ �� ������� ��� �� ����: ���, �㬬� ������������
** � ᮮ⢥��⢨� � ஫�� ��� �� ��������, ���祭�� % १�ࢨ஢����,
** ��㯯� �᪠, ���祭�� ��ନ஢������ १�ࢠ, ���祭�� ���⭮��
** १�ࢠ � ���⭮�� ����. �᪠.
** ����� ᪫��뢠���� �� �६����� ⠡����. ���� ⠡���� �. pqres.def
** ��ଠ� ��᫥����� ��ࠬ��� (⠡���) �� �맮��:
**     ��ਠ�� 1: OUTPUT TABLE tt-LoanAcct  - � �⮬ ��砥 �� �����,
**         ����⢮���訥 �� �맮��, 㤠������
**     ��ਠ�� 2: OUTPUT TABLE tt-DataLoan APPEND - � �⮬ ��砥 �����,
**         ����祭�� �� ��楤���, ����������� � ������ 㦥 ����⢮���訬
** ��ࠬ��� iCurrency �ᯮ������ ��� ���᪠ ��� � loan-acct.
** �� �㬬� �ਢ������ � ����� ������� 
**
** ��ࠡ�⠭� �� 0195050: ��ࠡ��뢠���� ⠪�� �������� �७�� ᥩ䮢�� �祥�
** (contract = ������)
*/
PROCEDURE GetAllFromLoan.
   DEF INPUT  PARAM iAcct     AS CHAR   NO-UNDO.   /* ����� ��� */
   DEF INPUT  PARAM iCurrency AS CHAR   NO-UNDO.   /* ����� ��� */
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO.   /* ���, �� ������ ᮡ������ ����� */
   DEF INPUT  PARAM iBegDate  AS DATE   NO-UNDO.   /* ��ਮ� ��� ���� */
   DEF INPUT  PARAM iEndDate  AS DATE   NO-UNDO.   /* ������ �� ��ਮ��� */
   DEF INPUT  PARAM iInclClsd AS LOG    NO-UNDO.   /* ������� �� � ���� ������� �������� */
   DEFINE OUTPUT PARAMETER TABLE FOR tt-DataLoan.  /* ⠡��� � ������ ᪫��뢠���� ����� */

   DEF VAR vBag      AS CHAR   NO-UNDO.      /* ��� (�᫨ ������� �室�� � ���) */
   DEF VAR vSummDolg AS DEC    NO-UNDO.      /* �㬬� ������������ � ᮮ⢥��⢨� � ஫�� ��� �� ��������  (� ��⮬ �ப��) */
   DEF VAR vRsrvRate AS DEC    NO-UNDO.      /* �����樥�� १�ࢨ஢���� */
   DEF VAR vGRsrv    AS INT64    NO-UNDO.      /* ��㯯� �᪠ (��⥣��� ����⢠) */
   DEF VAR vRsrvSumm AS DEC    NO-UNDO.      /* ���祭�� ��ନ஢������ १�ࢠ (� ��⮬ �ப��) */
   DEF VAR vRSummClc AS DEC    NO-UNDO.      /* ���祭�� ���⭮�� १�ࢠ */
   DEF VAR vRRsrvClc AS DEC    NO-UNDO.      /* ���祭�� ���⭮�� ����. �᪠ */
   DEF VAR vRsrvAcct AS CHAR   NO-UNDO.      /* ����� ��� १�ࢠ */
   DEF VAR vRsrvCurr AS CHAR   NO-UNDO.      /* ����� ��� १�ࢠ */
   DEF VAR vRsrvRole AS CHAR   NO-UNDO.      /* ஫� ��� १�ࢠ */
   DEF VAR vBasaOst        AS DEC   NO-UNDO.
   DEF VAR vAllBasaOst     AS DEC   NO-UNDO.
   DEF VAR vAllSummDolg    AS DEC   NO-UNDO.
   DEF VAR vGar            AS DEC   NO-UNDO.
   DEF VAR vLoanRiskRate   AS DEC   NO-UNDO.
   DEF VAR vRiskAmount     AS DEC   NO-UNDO.
   DEF VAR vBasaDone       AS LOG   NO-UNDO.
   DEF VAR vByTerm         AS LOG   NO-UNDO.
   DEF VAR vTempByTerm     AS LOG   NO-UNDO.
   DEF VAR vSummDolgRub    AS DEC   NO-UNDO.   /* �㬬� ������������ � �㡫�� */
   DEF VAR vEndDate        AS DATE  NO-UNDO.

   DEF VAR vTrRoleCorr    AS CHAR   NO-UNDO INIT "�।�,�।���,�।��%,
                                                  �।��%1,�।�㤊��,
                                                  �।�㤏���,�।��᪄�,
                                                  �।��᪏�".
   DEF BUFFER bloan         FOR loan.      /* ���������� ����. */
   DEF BUFFER bloan-acct    FOR loan-acct. /* ���������� ����. */
   DEF BUFFER b1-lacct      FOR loan-acct.   /* ���������� ����. */
   DEF BUFFER bloan-cond    FOR loan-cond.

   /* ��-㬮�砭�� ����砥� �� �������� (� ������, � ������� �� ���� iSince) */
   IF iInclClsd =  ? THEN iInclClsd = YES.

   MAIN_BLOCK:
   DO:
      {empty tt-DataLoan}
      {empty tt-SumDolg}
      {empty tt-LoanAcct}

      FOR EACH bloan-acct WHERE 
               bloan-acct.acct     =  iAcct
           AND bloan-acct.curr     =  iCurrency
           AND bloan-acct.since    <= iSince 
           AND bloan-acct.contract =  "�।��" 
                      /* � �� �����।���� �������� ஫� ��⮢, ����� ������
                      ** ��ࠡ��뢠���� �⨬ �����㬥�⮬. */
           AND CAN-DO(mFOiAKredRole,bloan-acct.acct-type)
           AND NOT CAN-FIND (FIRST b1-lacct WHERE b1-lacct.contract   =  bloan-acct.contract
                                              AND b1-lacct.cont-code  =  bloan-acct.cont-code
                                              AND b1-lacct.acct-type  =  bloan-acct.acct-type
                                              AND b1-lacct.since      <= iSince
                                              AND b1-lacct.since      >  bloan-acct.since)
         NO-LOCK:
         
         CREATE tt-LoanAcct.
         ASSIGN
            tt-LoanAcct.contract  = bloan-acct.contract
            tt-LoanAcct.cont-code = bloan-acct.cont-code
            tt-LoanAcct.acct-type = bloan-acct.acct-type
            tt-LoanAcct.since     = bloan-acct.since
         .
      END.

      FOR EACH bloan-acct WHERE 
               bloan-acct.acct     =  iAcct
           AND bloan-acct.curr     =  iCurrency
           AND bloan-acct.since    <= iSince 
           AND bloan-acct.contract =  "������"
           AND NOT CAN-FIND (FIRST b1-lacct WHERE b1-lacct.contract   =  bloan-acct.contract
                                              AND b1-lacct.cont-code  =  bloan-acct.cont-code
                                              AND b1-lacct.acct-type  =  bloan-acct.acct-type
                                              AND b1-lacct.since      <= iSince
                                              AND b1-lacct.since      >  bloan-acct.since)
         NO-LOCK:
         
         CREATE tt-LoanAcct.
         ASSIGN
            tt-LoanAcct.contract  = bloan-acct.contract
            tt-LoanAcct.cont-code = bloan-acct.cont-code
            tt-LoanAcct.acct-type = bloan-acct.acct-type
            tt-LoanAcct.since     = bloan-acct.since
         .
      END.

      IF NOT AVAILABLE tt-LoanAcct THEN LEAVE MAIN_BLOCK.

      /* ��ॡ�� ��� ��������� ������஢ */
      CYCLE:
      FOR EACH tt-LoanAcct,
         FIRST bloan WHERE bloan.contract   =  tt-LoanAcct.contract
                       AND bloan.cont-code  =  tt-LoanAcct.cont-code
                       AND (   bloan.close-date =  ?
                            OR bloan.close-date >  iSince
                            OR iInclClsd)
      NO-LOCK:
         
         IF bloan.contract =  "�।��" THEN
         DO:
            IF mPutProt THEN
            DO:
                  /* ���� ��� १�ࢠ */
               RUN GetLoanAcctRsrv IN THIS-PROCEDURE (
                                   tt-LoanAcct.contract,
                                   tt-LoanAcct.cont-code,
                                   tt-LoanAcct.acct-type,
                                   iSince,
                                   OUTPUT vRsrvAcct,
                                   OUTPUT vRsrvCurr,
                                   OUTPUT vRsrvRole).
             
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '����������: ����� १�� ��� ��� ' TRIM(iAcct) SKIP
                  '   �������: ' TRIM(tt-LoanAcct.cont-code) SKIP
                  '   ��� १�ࢠ: ' TRIM(vRsrvAcct)
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END. 
            
            /* ��� (�᫨ ������� �室�� � ���) */
             vBag = LnInBagOnDate (tt-LoanAcct.contract, tt-LoanAcct.cont-code, iSince).
            /* �㬬� ������������ � ᮮ⢥��⢨� � ஫�� ��� �� ��������  (� ��⮬ �ப��) */
            RUN GetSummDolgBuff IN THIS-PROCEDURE (BUFFER bloan,
                                                   tt-LoanAcct.acct-type,
                                                   iSince,
                                                   iBegDate,
                                                   iEndDate,
                                                   bloan.currency,
                                                   OUTPUT vSummDolg,
                                                   OUTPUT vAllSummDolg,
                                                   OUTPUT vByTerm).
            
                     /* ���祭�� % १�ࢨ஢���� (�����樥�� १�ࢨ஢����) */
            vRsrvRate = LnRsrvRate (tt-LoanAcct.contract, tt-LoanAcct.cont-code, iSince).
                     /* ��㯯� �᪠ (��⥣��� ����⢠) */
            vGRsrv = re_history_risk(tt-LoanAcct.contract, tt-LoanAcct.cont-code,iSince,?).

           /* ���祭�� ��ନ஢������ १�ࢠ (� ��⮬ �ப��) */
            RUN SUMM_SPIS_RES_TYPE_BUFF IN THIS-PROCEDURE (BUFFER bloan,
                                                           tt-LoanAcct.acct-type,
                                                           iSince,
                                                           iBegDate,
                                                           iEndDate,
                                                           "",
                                                           ?,
                                                           OUTPUT vRsrvSumm).
                     /* ���祭�� ���⭮�� १�ࢠ � ����. �᪠ */
            RUN LnRsrvByType (iAcct,
                              tt-LoanAcct.acct-type,
                              "",
                              tt-LoanAcct.contract,
                              tt-LoanAcct.cont-code,
                              iSince,
                              OUTPUT vRRsrvClc,
                              OUTPUT vRSummClc).

            /* �᫨ �㦭� ࠧ��᪠ �� �ப�� */
            IF iBegDate <> ? AND
              iEndDate <> ? AND
              vSummDolg > 0 AND
              vAllSummDolg > 0 AND
              vSummDolg <> vAllSummDolg
              THEN ASSIGN
                     vRSummClc = (vRSummClc * vSummDolg) / vAllSummDolg NO-ERROR.

             /* ���� ��� १�ࢠ */
            RUN GetLoanAcctRsrv(tt-LoanAcct.contract,
                                tt-LoanAcct.cont-code,
                                tt-LoanAcct.acct-type,
                                iSince,
                                OUTPUT vRsrvAcct,
                                OUTPUT vRsrvCurr,
                                OUTPUT vRsrvRole).
             
                      /* ������ ������ �� �������� � ⠡���� */
            DO:
               vSummDolgRub = CurToCurWork ("�������",iCurrency,"",iSince,vSummDolg).
               CREATE tt-DataLoan.
               ASSIGN tt-DataLoan.rid                 = RECID(bloan)
                      tt-DataLoan.acct                = iAcct
                      tt-DataLoan.currency            = iCurrency
                      tt-DataLoan.acct-type           = tt-LoanAcct.acct-type
                      tt-DataLoan.class-code          = bloan.class-code 
                      tt-DataLoan.contract            = tt-LoanAcct.contract
                      tt-DataLoan.cont-code           = tt-LoanAcct.cont-code
                      tt-DataLoan.since               = iSince
                      tt-DataLoan.open-date           = bloan.open-date
                      tt-DataLoan.end-date            = bloan.end-date
                      tt-DataLoan.close-date          = bloan.close-date
                      tt-DataLoan.bag                 = vBag
                      tt-DataLoan.dolg-summ           = IF vSummDolg = ? THEN 0
                                                                         ELSE vSummDolg
                      tt-DataLoan.grsrv               = IF vGRsrv    = ? THEN 1
                                                                         ELSE vGRsrv
                      tt-DataLoan.rsrv-rate           = IF vRsrvRate = ? THEN 0
                                                                         ELSE vRsrvRate
                      tt-DataLoan.rsrv-summ           = IF vRsrvSumm = ? THEN 0
                                                                         ELSE vRsrvSumm
                      tt-DataLoan.rsrv-rate-clc       = IF vRRsrvClc = ? THEN 0
                                                                         ELSE vRRsrvClc
                      tt-DataLoan.rsrv-summ-clc       = IF vRSummClc = ? THEN 0
                                                                         ELSE vRSummClc
                      tt-DataLoan.rsrv-summ-clc-gar   = 
                         IF CAN-DO(vTrRoleCorr, tt-LoanAcct.acct-type) THEN
                            (vSummDolgRub * vRsrvRate / 100)
                         ELSE
                            (vSummDolgRub * tt-DataLoan.rsrv-rate-clc / 100)
                      tt-DataLoan.acct-rsrv           = vRsrvAcct
                      tt-DataLoan.curr-rsrv           = vRsrvCurr
                      tt-DataLoan.role-rsrv           = vRsrvRole
                      tt-DataLoan.ByTerm              = vByTerm
                      tt-DataLoan.rsrv-summ           = ROUND(tt-DataLoan.rsrv-summ,2)
                      tt-DataLoan.rsrv-summ-clc       = ROUND(tt-DataLoan.rsrv-summ-clc,2)
                      tt-DataLoan.rsrv-summ-clc-gar   = ROUND(tt-DataLoan.rsrv-summ-clc-gar,2)
               .
            END.
         END. /* if bloan.contract eq "�।��" */
         ELSE
         IF bloan.contract =  "������" THEN
         DO:
            /* ��ࠡ�⪠ ������� �७�� ᥩ䮢�� �祩�� */
            RUN Acct-Pos-Pure IN h_base (iAcct, iCurrency, iSince, iSince, gOp-status).
            IF iCurrency =  "" THEN
               vSummDolg = ABS(sh-bal).
            ELSE
               vSummDolg = ABS(sh-val).

            IF     iBegDate <> ?
               AND iEndDate <> ? THEN
            DO:
               vByTerm = YES.
               /* �饬 ��ࢮ� �᫮��� � ��⮩ > ���⭮� � �� Prolong = ��
                  �᫨ ��諮�� - ������� �஫����஢�� � �� �ࠢ����� � �ப���
                  �㤥� ������ ��� �஫����樨 (= ��� �᫮���) */
               FOR EACH bloan-cond WHERE
                      bloan-cond.contract  =  bloan.contract
                  AND bloan-cond.cont-code =  bloan.cont-code
                  AND bloan-cond.since     >  iSince
               NO-LOCK
               BY bloan-cond.since:
                  IF GetXAttrValueEx("loan-cond",
                                     bloan-cond.contract + ","
                                     + bloan-cond.cont-code + ","
                                     + STRING(bloan-cond.since),
                                     "Prolong", "���") =  "��" THEN
                     LEAVE.
               END.
               IF AVAILABLE bloan-cond THEN
                  vEndDate = bloan-cond.since.
               ELSE
                  vEndDate = bloan.end-date.

               /* �᫨ ��� ����砭�� �� ������ � �㦭� ��ਮ�, �����頥� 0 */
               IF vEndDate <> ?
                  AND (   vEndDate <  iBegDate
                       OR vEndDate >  iEndDate) THEN
                  vSummDolg = 0.
            END.
            ELSE
            DO:
               /* �ப� �� ������, ⠪ �� �� �஢��塞 �� */
               vByTerm = NO.
            END.

            CREATE tt-DataLoan.
            ASSIGN tt-DataLoan.rid                 = RECID(bloan)
                   tt-DataLoan.acct                = iAcct
                   tt-DataLoan.currency            = iCurrency
                   tt-DataLoan.acct-type           = tt-LoanAcct.acct-type
                   tt-DataLoan.class-code          = bloan.Class-Code
                   tt-DataLoan.contract            = tt-LoanAcct.contract
                   tt-DataLoan.cont-code           = tt-LoanAcct.cont-code
                   tt-DataLoan.since               = iSince
                   tt-DataLoan.open-date           = bloan.open-date
                   tt-DataLoan.end-date            = bloan.end-date
                   tt-DataLoan.close-date          = bloan.close-date
                   tt-DataLoan.bag                 = ""
                   tt-DataLoan.dolg-summ           = vSummDolg
                   tt-DataLoan.grsrv               = 1
                   tt-DataLoan.rsrv-rate           = 0
                   tt-DataLoan.rsrv-summ           = 0
                   tt-DataLoan.rsrv-rate-clc       = 0
                   tt-DataLoan.rsrv-summ-clc       = 0
                   tt-DataLoan.rsrv-summ-clc-gar   = 0
                   tt-DataLoan.acct-rsrv           = ""
                   tt-DataLoan.curr-rsrv           = ""
                   tt-DataLoan.role-rsrv           = ""
                   tt-DataLoan.ByTerm              = vByTerm
            .
         END. /* if bloan.contract eq "������" */
      END. /* of FOR EACH tt-LoanAcct */
   END. /* of MAIN_BLOCK */

   RETURN.
END PROCEDURE.

{pfuncdef 
 &DefFunc="GetValueForInstr139"
 &Description="�����頥� �६����� ⠡���� tt-DataLoanInstr139 � १�ࢮ� � ��業⠬�"
 &Parameters="�����祭�� �������,����� �������,��� ����,�६����� ⠡���"
 &Result="���祭�� �� �६����� ⠡��� tt-DataLoanInstr139"
 &Sample="RUN GetValueForInstr139 IN h_pqres (~
 '�।��', ~
   'br_705@002', ~
   TODAY, ~
   TABLE tt-DataLoanInstr139 BY-REFERENCE)."}
PROCEDURE GetValueForInstr139:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO. /* �����祭�� */
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM iDate     AS DATE NO-UNDO. /* ��� ���� */
   DEF INPUT PARAM TABLE FOR tt-DataLoanInstr139. /* �६����� ⠡��� */
   
   DEF VAR vRsrvSumm        AS DEC   NO-UNDO.
   DEF VAR vRsrvSummProsr   AS DEC   NO-UNDO.
   DEF VAR vInterest        AS DEC   NO-UNDO. /* ���᫥��� ��業�� */
   DEF VAR vPastDueInterest AS DEC   NO-UNDO. /* ����祭�� ��業�� */
   DEF VAR vTmpDec          AS DEC   NO-UNDO EXTENT 3.
   DEF VAR vI               AS INT64 NO-UNDO.
   
   DEF BUFFER loan FOR loan.
   
   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF NOT AVAIL loan THEN
      RETURN.

   {empty tt-DataLoan}
   {empty tt-DataLoanInstr139}
   {empty tt-SumDolg}

   ASSIGN
      vRsrvSumm        = 0
      vRsrvSummProsr   = 0
      vInterest        = 0
      vPastDueInterest = 0
   .
      /* ���祭�� ��ନ஢������ १�ࢠ */
   RUN SUMM_SPIS_RES_TYPE IN THIS-PROCEDURE (
                           loan.contract,
                           loan.cont-code,
                           "�।��",
                           iDate,
                           ?,
                           ?,
                           "",
                           ?,
                           OUTPUT vRsrvSumm).
   
      /* ���祭�� ��ନ஢������ १�ࢠ �� ����祭���� �।��� */
   RUN SUMM_SPIS_RES_TYPE IN THIS-PROCEDURE (
                           loan.contract,
                           loan.cont-code,
                           "�।��",
                           iDate,
                           ?,
                           ?,
                           "",
                           ?,
                           OUTPUT vRsrvSummProsr).
   
      /* ��⠥� ���᫥��� ��業�� */                   
   DO vI = 1 TO NUM-ENTRIES(mI139_Int):
      RUN RE_PARAM IN h_Loan (INT64(ENTRY(vI, mI139_Int)),
                              iDate,
                              loan.contract,
                              loan.cont-code,
                              OUTPUT vTmpDec[1],
                              OUTPUT vTmpDec[2],
                              OUTPUT vTmpDec[3]).
                                    
      vInterest = vInterest + vTmpDec[1].
   END.
            
      /* ��⠥� ����祭�� ��業�� */
   DO vI = 1 TO NUM-ENTRIES(mI139_PastDueInt):
      RUN RE_PARAM IN h_Loan (INT64(ENTRY(vI, mI139_PastDueInt)),
                              iDate,
                              loan.contract,
                              loan.cont-code,
                              OUTPUT vTmpDec[1],
                              OUTPUT vTmpDec[2],
                              OUTPUT vTmpDec[3]).
                                    
      vPastDueInterest = vPastDueInterest + vTmpDec[1].
   END.
   
   CREATE tt-DataLoanInstr139.
   ASSIGN
      tt-DataLoanInstr139.contract        = loan.contract
      tt-DataLoanInstr139.cont-code       = loan.cont-code
      tt-DataLoanInstr139.rsrv-summ       = vRsrvSumm
      tt-DataLoanInstr139.rsrv-summ-prosr = vRsrvSummProsr
      tt-DataLoanInstr139.Interest        = vInterest
      tt-DataLoanInstr139.PastDueInterest = vPastDueInterest
      tt-DataLoanInstr139.Loan-Currency   = loan.currency
   .   
   
END PROCEDURE.

   /* �����頥� ���� ����襭�� �� ��ࢮ��砫쭮�� �������� � ���� ����襭�� ��
   ** ��᫥����� ���������� � ��������, �� ������� iEndDate
   ** �室�騩 ��ࠬ��� iBegDate �� �ᯮ������ */
PROCEDURE GetLoanEndDate.
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iBegDate      AS DATE NO-UNDO. /* ��砫� ��ਮ�� �⡮� */
   DEF INPUT  PARAM iEndDate      AS DATE NO-UNDO. /* ����� ��ਮ�� �⡮� */
   DEF OUTPUT PARAM oEndDateFirst AS DATE NO-UNDO. /* ���� ����襭�� �� ��ࢮ��砫쭮�� �������� */
   DEF OUTPUT PARAM oEndDateLast  AS DATE NO-UNDO. /* ���� ����襭�� �� ��᫥����� ���������� � �������� */

   DEF VAR vSurr       AS CHAR NO-UNDO. /* ���ண�� �᫮��� ������� */
   DEF VAR vDateString AS CHAR NO-UNDO. /* ��� � �� CondEndDate �᫮��� */

   DEF BUFFER loan         FOR loan.     /* ���������� �����. */
   DEF BUFFER term-obl     FOR term-obl. /* ���������� �����. */
   DEF BUFFER t-pro-obl    FOR pro-obl.  /* ���������� �����. */
   DEF BUFFER b-pro-obl    FOR pro-obl.  /* ���������� �����. */
   DEF BUFFER bterm-obl    FOR term-obl. /* ���������� ����. */

   &SCOPED-DEFINE NO-BASE-PROC YES

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* �饬 ������� */
      FIND FIRST loan WHERE loan.contract  =  iContract
                        AND loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE mb.

      /* ��ࢮ��砫쭠� ��� ����襭�� */
      IF IsLoanUnComLines (iContract, iContCode) THEN
         oEndDateFirst = DATE(GetXAttrValue("loan",
                                            iContract + "," + iContCode,
                                            "FinRevDate")).
      ELSE
         oEndDateFirst = loan.end-date.

      FOR EACH term-obl OF loan WHERE
               term-obl.idnt =  3
      NO-LOCK BY term-obl.end-date DESC:
         /* ���樠�����㥬 ��ࢮ��砫��� ����. */
         oEndDateFirst = term-obl.end-date.

         pb:
         FOR EACH t-pro-obl OF loan WHERE t-pro-obl.idnt       =  3
                                      AND t-pro-obl.n-end-date =  term-obl.end-date
         NO-LOCK BY t-pro-obl.end-date:
            /* ��ࢮ���. ��� ����襭�� */
            oEndDateFirst = t-pro-obl.end-date.
            LEAVE pb.
         END.

         FIND LAST pro-obl WHERE
                   pro-obl.contract   =  iContract
            AND    pro-obl.cont-code  =  iContCode
            AND    pro-obl.pr-date    >= loan.open-date
            AND    pro-obl.pr-date    <= iEndDate
         NO-LOCK NO-ERROR.

         IF     AVAIL pro-obl
            AND CAN-FIND (FIRST bterm-obl WHERE bterm-obl.contract  =  pro-obl.contract
                                            AND bterm-obl.cont-code =  pro-obl.cont-code
                                            AND bterm-obl.idnt      =  pro-obl.idnt
                                            AND bterm-obl.end-date  =  pro-obl.n-end-date
                                            AND bterm-obl.nn        =  pro-obl.new-nn)
         THEN
         DO:

            oEndDateLast = IF pro-obl.n-end-date < iEndDate THEN pro-obl.n-end-date
                                                            ELSE term-obl.end-date.

            FIND FIRST b-pro-obl WHERE
                       b-pro-obl.contract   =  iContract
               AND     b-pro-obl.cont-code  =  iContCode
               AND     b-pro-obl.pr-date    >  iEndDate
            NO-LOCK NO-ERROR.
            IF AVAIL b-pro-obl THEN
               oEndDateLast = b-pro-obl.end-date.
         END.
         /* �᫨ �� ��諨 ����ᥩ � �஫����樨, � ���饬 �� �� �᫮��� */
         IF oEndDateLast =  ? THEN
         DO:
            BLK:
            FOR EACH loan-cond WHERE
                     loan-cond.contract  =  iContract
               AND   loan-cond.cont-code =  iContCode
               AND   loan-cond.since     >= loan.open-date
               AND   loan-cond.since     <= iEndDate
            NO-LOCK BY loan-cond.since DESCENDING:
               vSurr = loan-cond.contract  + "," + loan-cond.cont-code + "," + STRING(loan-cond.since).
               IF GetXAttrValue("loan-cond", vSurr, "����������") =  "�஫�������" THEN
               DO:
                  vDateString = GetXAttrValueEx("loan-cond", vSurr, "CondEndDate", "").
                  IF vDateString <> "" THEN
                     oEndDateLast = DATE(vDateString).
                  LEAVE BLK.
               END.
            END. /* BLK: */
         END.
         /* ��� �������� ⮫쪮 ���� �����⭠� ������, ���⮬� ��室�� */
         LEAVE mb.
      END.
   END.

   &UNDEFINE NO-BASE-PROC
   RETURN.
END PROCEDURE.


/* �����뢠�� �᫮ ���� ����窨 �� �᭮����� ����� �⭮�⥫쭮
** ��।������� ���� */
PROCEDURE GetLoanPrQDay.
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iBegDate     AS DATE NO-UNDO. /* ��� ����� */
   DEF INPUT  PARAM iEndDate     AS DATE NO-UNDO. /* ��� ����� */
   DEF OUTPUT PARAM oQDay     AS INT64  NO-UNDO. /* ��᫮ ���� ����窨 �� �᭮����� ����� */

   DEF VAR vResult      AS DEC   NO-UNDO.
   DEF VAR vDbOper      AS DEC   NO-UNDO.
   DEF VAR vCrOper      AS DEC   NO-UNDO.

   DEF VAR vResult1     AS DEC   NO-UNDO.   /*������襭� ���⮪ ����窨*/
   DEF VAR vResult2     AS DEC   NO-UNDO.   /**/

   DEF VAR dd           AS DATE  NO-UNDO.
   DEF VAR dd2          AS DATE  NO-UNDO.
   DEF VAR vNumDays     AS INT64 NO-UNDO.   /*������⢮ ���� ����窨*/
   DEF VAR vDate        AS DATE  NO-UNDO.
   DEF VAR vDateBegPr   AS DATE  NO-UNDO.  /* ��� ��砫� ��ਮ�� ����窨 */

   DEF BUFFER loan-int FOR loan-int.
   DEF BUFFER loan     FOR loan.

   MAIN:
   DO ON ERROR UNDO, RETRY:

      FIND FIRST loan WHERE 
                 loan.contract  =  iContract
             AND loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.

      /* �᫨ ��� ������ ������� ����� ���� ��, ⮣�� � ����⢥ ��砫� ����窨 �� ��
         *** ������ ��� �� �� over-date_loan */
      IF AVAIL loan AND loan.open-date <  mDateNR THEN
         dd = DATE(GetXAttrValue("loan", iContract + "," + iContCode, "over-date_loan")).

      RUN STNDRT_PARAM (iContract,
                       iContCode,
                       7,
                       iEndDate,
                       OUTPUT vResult,
                       OUTPUT vDbOper,
                       OUTPUT vCrOper
                       ).

      IF vResult >  0
      THEN DO:
      /* �᫨ ��� ��砫� ����窨 �� �� ��।�����, ⮣�� ��।��塞 �� */
         IF dd =  ? THEN
         DO:
            vResult1 = 0.
            /*��⠬ �㬬� ����窨 �� ��砫� ��ਮ�� ����*/
            RUN STNDRT_PARAM (iContract,
                              iContCode,
                              7,
                              iBegDate,
                              OUTPUT vResult1,
                              OUTPUT vDbOper,
                              OUTPUT vCrOper
                              ).

            /*�᫨ ����窠 㦥 �뫠*/
            IF vResult1 > 0 THEN
            DO:
               /* ��室�� ���� ������������� ����窨,
               ** ���� ���� ������ �뭮� �� 7-�� �� �����筮, �.�. ����� ���뭮���*/
               LOOP:
               FOR EACH loan-int WHERE
                        loan-int.contract  =  iContract
                    AND loan-int.cont-code =  iContCode
                    AND loan-int.id-d      =  7
                    AND loan-int.mdate     <= iBegDate
               NO-LOCK BY loan-int.mdate DESC:
                  /*��⠬ �㬬� ����窨 �� �।��騩 ����*/
                  RUN STNDRT_PARAM (iContract,
                                    iContCode,
                                    7,
                                    loan-int.mdate - 1,
                                    OUTPUT vResult2,
                                    OUTPUT vDbOper,
                                    OUTPUT vCrOper
                                    ).
                  IF vResult2 = 0 THEN /*����� ��諨*/
                  DO:
                     dd = loan-int.mdate.
                     LEAVE LOOP.
                  END.
               END. /*LOOP*/
            END. /*�᫨ ����窠 㦥 �뫠 vResult1 > 0 */
            ELSE /*��� ����窨 �� ��砫� ��ਮ��, ����� �饬 ����� - ��� ���� ⠪ ��� �� ����� ��ਮ�� ���� ����⮪ �� 7-��*/
            DO:
               /*�饬 ����� ������ - �뭮� �� ������ � �������� ��ਮ��*/
               FIND FIRST loan-int WHERE
                      loan-int.contract  =  iContract
                  AND loan-int.cont-code =  iContCode
                  AND loan-int.id-d      =  7
                  AND loan-int.mdate     >= iBegDate
                  AND loan-int.mdate     <= iEndDate
               NO-LOCK NO-ERROR.

               /*�᫨ ����窠 �� �������� ��� �� ��砫� ���� ����窠 㦥 �뫠*/
               dd = IF NOT AVAIL loan-int THEN iBegDate ELSE loan-int.mdate.

            END. /*�� �뫮 ����窨 �� ��砫� ��ਮ��*/
         END.
         vDate = dd. /*��ᢠ����� ��砫쭮� ���祭�� ��� ������������� ����窨*/
         vDateBegPr = dd.   /* ��������� ��砫� ��ਮ�� ����窨 */

         /*���������� � ���ࢠ�, �饬 ���ᨬ��쭮� ������⢮ ���� ����窨*/
         DO WHILE dd < iEndDate :
            /*�饬 ᫥����� ������ ���᫥��� ����窨 � ⥪�饬 ��ਮ��*/
            FIND FIRST loan-int WHERE
                       loan-int.contract  =  iContract
                   AND loan-int.cont-code =  iContCode
                   AND loan-int.id-d      =  7
                   AND loan-int.mdate     > dd
                   AND loan-int.mdate     <= iEndDate
            NO-LOCK NO-ERROR.

            /*���� ᫥����� ����窠*/
            IF AVAIL loan-int THEN
               dd2 = loan-int.mdate.
            ELSE
               /*��� ᫥���饣� �뭮�, ����� ��⠥� �� ���� ��ਮ�� ����*/
               dd2 = iEndDate.

            /*�����⠥� ���⮪ �� 7-�� �� ��砫� �஢��塞��� ��ਮ��*/

            vNumDays = 0. /*���ᨬ ⥪�饥 ������⢮ ������⢮ ���� ����窨*/

            RUN STNDRT_PARAM (iContract,
                              iContCode,
                              7,
                              dd,
                              OUTPUT vResult1, /*�㬬�, ������ �㦭� �������*/
                              OUTPUT vDbOper,
                              OUTPUT vCrOper
                              ).
            /*�᫨ ���� �������������, ���� ����� ���� ����樨 �뭮� �� 7-�� ��ࠬ���, �� ����祭��� ���।... */
            IF vResult1 > 0  THEN
            DO:
               /*���� �� ������ ��襭�� ����窨*/
               LOOP:
               FOR EACH loan-int WHERE
                        loan-int.contract  =  iContract
                    AND loan-int.cont-code =  iContCode
                    AND loan-int.id-k      =  7
                    AND loan-int.mdate     > dd
                    AND loan-int.mdate     <= dd2
               NO-LOCK:
                  /*�����蠥� �㬬� ����窨 �� �㬬� ����樨 ��襭��*/
                  vResult1 = vResult1 - loan-int.amt-rub.

                  /*�᫨ 㤠���� ������� �������������, ����� �㦭� ������� ᪮�쪮 ������� ����窠*/
                  IF vResult1 <= 0 THEN
                  DO:
                     /*����砥� ������⢮ ���� ����襭��� ����窨 */
                     vNumDays =  loan-int.mdate - vDate .
                     LEAVE loop.
                  END. /*IF*/
               END. /*LOOP*/
            END. /*vResult1 > 0 */

            /*�᫨ ����⠫� ������⢮ ���� ����窨 �ࠢ������ � �।��騬� ���祭�ﬨ ��� ����祭�� ���ᨬ㬠*/
            IF vNumDays > 0 THEN
            DO:
               oQDay = IF oQDay < vNumDays THEN vNumDays ELSE oQDay.
               vDate = dd2. /*����뢠�� ���� ������������� ����窨 �� ���� ᫥���饩 ����樨 �뭮�*/
            END.
            dd = dd2.
         END. /*WHILE*/

         /*�᫨ ����窠 �뫠, � �� ⠪ � �� �����㦨�� �� ������ ��襭��, ����� ���� ��ਮ� ������ ����窠*/
         IF oQDay = 0 THEN
            oQDay = iEndDate - vDateBegPr + 1.

      END. /*�㬬� ����窨 �� ����� ��ਮ�� ����� ���*/
   END. /* MAIN: DO: */
   RETURN.
END PROCEDURE.

PROCEDURE GetQRestrLoan PRIVATE.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iTypeRestr AS CHAR NO-UNDO. /* ��� �������ਧ�樨 */
   DEF INPUT  PARAM iRestrQuan AS CHAR NO-UNDO. /* ���-�� �������ਧ�樨 */
   DEF INPUT  PARAM iIndex     AS INT64 NO-UNDO. /* */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* ��砫� ��ਮ�� �⡮� */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* ����� ��ਮ�� �⡮� */
   DEF INPUT-OUTPUT PARAM oQuant     AS INT64  NO-UNDO. /* ��᫮ ���� ����窨 �� �᭮����� ����� */

   DEF BUFFER pro-obl FOR pro-obl.

   &SCOPED-DEFINE NO-BASE-PROC YES

      /* ��� �஫������� ᭠砫� �饬 �� �������� ����ᥩ pro-obl */
   IF iTypeRestr =  "�஫�������" THEN
   DO:
      FOR EACH pro-obl WHERE
               pro-obl.contract   =  iContract
         AND   pro-obl.cont-code  =  iContCode
         AND   pro-obl.pr-date    >= iBegDate
         AND   pro-obl.pr-date    <= iEndDate
      NO-LOCK:
         oQuant = oQuant + 1.
      END.
   END.
      /* ������ ��६ ���-�� � �� "����������" */
   IF     oQuant =  0
      AND iIndex <= NUM-ENTRIES(iRestrQuan) THEN
         /* ������ ������⢠ � �� "����������" ������ ᮮ⢥��⢮����
         ** ������� ⨯� ����. � �� "����������" */
      ASSIGN
         oQuant = INT64(ENTRY(iIndex ,iRestrQuan))
      NO-ERROR.

   &UNDEFINE NO-BASE-PROC

   RETURN.
END PROCEDURE.

PROCEDURE GetQRestrLoanCond PRIVATE.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iTypeRestr AS CHAR NO-UNDO. /* ��� �������ਧ�樨 */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* ��砫� ��ਮ�� �⡮� */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* ����� ��ਮ�� �⡮� */
   DEF INPUT-OUTPUT PARAM oQuant     AS INT64  NO-UNDO. /* ��᫮ ���� ����窨 �� �᭮����� ����� */

   DEF VAR vRestrCur AS CHAR NO-UNDO. /* ��� ������ਧ�樨 �� �� �᫮��� */

   DEF BUFFER loan-cond FOR loan-cond.

   &SCOPED-DEFINE NO-BASE-PROC YES

   MAIN:
   FOR EACH loan-cond WHERE
            loan-cond.contract  =  iContract
        AND loan-cond.cont-code =  iContCode
        AND loan-cond.since     >= iBegDate
        AND loan-cond.since     <= iEndDate
   NO-LOCK ON ERROR UNDO, RETRY:
      {do-retry.i MAIN}
      vRestrCur = GetXAttrValue("loan-cond",
                                iContract + "," + 
                                iContCode + "," + 
                                STRING (loan-cond.since),
                                "����������").
      IF {assigned vRestrCur} THEN
         IF SortDelimList(vRestrCur, ",", NO) = SortDelimList(iTypeRestr, ",", NO) THEN
            oQuant = oQuant + 1.
   END.

   &UNDEFINE NO-BASE-PROC

   RETURN.
END PROCEDURE.

/* ��।������ ������⢠ �������ਧ�権 �� ������� �।�⭮�� ��������
**  �� ����� ��ਮ�. */
PROCEDURE GetQRestr.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iTypeRestr AS CHAR NO-UNDO. /* ��� �������ਧ�樨 */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* ��砫� ��ਮ�� �⡮� */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* ����� ��ਮ�� �⡮� */
   DEF OUTPUT PARAM oQuant     AS INT64  NO-UNDO. /* ��᫮ ���� ����窨 �� �᭮����� ����� */

   DEF VAR vRestrCur    AS CHAR NO-UNDO. /* ��� ������ਧ�樨 �� �� */
   DEF VAR vRestrQuan   AS CHAR NO-UNDO. /* ���᮪ ���-� �������ਧ�樨 �� �� "����������" */
   DEF VAR vIndex       AS INT64  NO-UNDO. /* ������ �������ਧ�樨 � ᯨ᪥ �� "����������" �� ���.*/
   DEF VAR vRestrDoc     AS CHAR NO-UNDO. /* �� �������� - ���뢠�� ������⢮ �������ਧ�権 �� ������� � �᫮��� */

   DEF BUFFER loan      FOR loan.

   &SCOPED-DEFINE NO-BASE-PROC YES

   oQuant = 0.

   FIND FIRST loan WHERE
              loan.contract  =  iContract
      AND     loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* �᫨ �� �������� = ��, � ��� ��� ������ਧ�樨 � ������⢠  
            ᭠砫� ������������ �� ���������� � �� ���������� �� �������, �᫨ ��� �����, � ��  �᫮���,
            �᫨ �� �������� = ��� ��� ����, � ᬮ�ਬ ��-��஬� */
      VRestrDoc = fGetSetting ("��ଠ117", "��������", "").

         ASSIGN
            vRestrCur  = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "����������",
                                         "")
            vRestrQuan = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "����������",
                                         "")
            vIndex     = LOOKUP(iTypeRestr, vRestrCur)
         vRestrQuan = IF NOT {assigned vRestrQuan} THEN "0" ELSE vRestrQuan
         .
         /* �᫨ �� �������� = ��, � �����뢠�� �᫮ ���� ����窨
            ᭠砫� � �������, � ��⮬ � �᫮��� */
      IF vRestrDoc =  "��" THEN
      DO:
            /* �᫨ �� ������� �� ���������� � ���������� �� �����,
               � ������뢠��, ���� ���� �� ��室�� � ��楤��� */
         IF     vRestrCur  <> ""
            AND vRestrQuan <> "" THEN
            RUN GetQRestrLoan (iContract,
                               iContCode,
                               iTypeRestr,
                               vRestrQuan,
                               vIndex,
                               loan.open-date,
                               iEndDate,
                               INPUT-OUTPUT oQuant).
            IF    iTypeRestr <> "�஫�������"
               OR oQuant     =  0 THEN
               RUN GetQRestrLoanCond (iContract,
                                      iContCode,
                                      iTypeRestr,
                                      loan.open-date,
                                      iEndDate,
                                      INPUT-OUTPUT oQuant).
      END.
         /* �᫨ �� �������� = ��� ��� �����, 
            � �����뢠�� �᫮ ���� ����窨 �� ��஬� ������� */
      ELSE
      DO:
         RUN GetQRestrLoanCond (iContract,
                                iContCode,
                                iTypeRestr,
                                loan.open-date,
                                iEndDate,
                                INPUT-OUTPUT oQuant).
         IF oQuant =  0 THEN
            RUN GetQRestrLoan (iContract,
                               iContCode,
                               iTypeRestr,
                               vRestrQuan,
                               vIndex,
                               loan.open-date,
                               iEndDate,
                               INPUT-OUTPUT oQuant).
      END.
   END.

   &UNDEFINE NO-BASE-PROC

   RETURN.
END PROCEDURE.

/* �믮���� ���� �㬬� ��業⮢, �।�����祭�� � �믫�� � 㪠����� ��ਮ��
** ���祭�� ����ࠧ����� � ����� ������� */
PROCEDURE GetSumPlanProc.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iBegDate   AS DATE NO-UNDO. /* ��� ��砫� ��ਮ�� */
   DEF INPUT  PARAM iEndDate   AS DATE NO-UNDO. /* ��� ����砭�� ��ਮ�� */
   DEF INPUT  PARAM iBegF125   AS DATE NO-UNDO. /* ��� ��砫� ��ਮ�� ���� �.125 */
   DEF INPUT  PARAM iEndF125   AS DATE NO-UNDO. /* ��� ����砭�� ��ਮ�� ���� �.125*/
   DEF OUTPUT PARAM oSumProc   AS DEC  NO-UNDO. /* �㬬� �������� ���⥦�� */
   
   DEF VARIABLE vUseHist AS LOGICAL INIT NO NO-UNDO.

   DEF BUFFER term-obl FOR term-obl. /* ���������� ����. */
   DEF BUFFER loan     FOR loan.     /* ���������� ����. */
   DEF BUFFER term-obl-hist FOR term-obl-hist.     /* ���������� ����. */

   FIND FIRST loan WHERE 
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   
   IF AVAIL loan THEN 
   DO:
      IF NOT (loan.class-code  =  "own-bill-liability"
           OR loan.class-code  =  "dsc-bill-asset") THEN 
      DO:
         IF FGetSetting("��ଠ125", "��������", "��")   =  "��" THEN
            vUseHist = CAN-FIND(
               FIRST term-obl-hist WHERE
                     term-obl-hist.contract  =  loan.contract
                 AND term-obl-hist.cont-code =  loan.cont-code
                 AND term-obl-hist.idnt      =  1
                 AND term-obl-hist.since     >= iEndF125).
         
         IF vUseHist THEN
            RUN SUMM_PROC2 IN THIS-PROCEDURE
                              (BUFFER loan,
                              iEndF125,
                              iBegDate,
                              iEndDate,
                              OUTPUT oSumProc).
         ELSE
            RUN SUMM_PROC IN THIS-PROCEDURE 
                            (BUFFER loan,
                             iEndF125,
                             iBegDate,
                             iEndDate,
                             OUTPUT oSumProc).
      END.
      ELSE /* ���ᥫ� */
         RUN bprocper.p (iContract,
                         iContCode,
                         iBegF125,
                         iEndF125,
                         "Ext",
                         OUTPUT oSumProc).
   END.
   
   IF oSumProc <  0 OR oSumProc =  ? THEN oSumProc = 0.
   
END PROCEDURE.

/* ����� �㭪�� ��� ���� �⠢�� �� ��������. �᫨ ��� �������⥫��� �믫��
** �����頥� ���祭�� �⠢�� %�।, ���� ���祭�� ��� �� ��������.
** ��� ��᫥����� ���᫥��� %% �� ������ */
FUNCTION fCalcEpsLoan RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vList-Comm AS CHAR NO-UNDO.
   DEF VAR vCounter   AS INT64  NO-UNDO.
   DEF VAR vComm      AS CHAR NO-UNDO.
   DEF VAR vCardOver  AS CHAR NO-UNDO.
   DEF VAR vCalcEPS   AS LOG  NO-UNDO.
   DEF VAR vEPS       AS DEC  NO-UNDO.
   DEF VAR vAddPay    AS LOG  NO-UNDO.
   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER loan       FOR loan.
   DEF BUFFER loan-acct  FOR loan-acct.
   DEF BUFFER bloan-acct FOR loan-acct.
   DEFINE BUFFER term-obl FOR term-obl.

   MAIN:
   DO:
      vCalcEPS = FALSE.
      CHK:
      DO:
         vList-Comm =  FGetSetting("���", "������" ,"").  /* ����砥� ���祭�� �� */
         REPEAT vCounter = 1 TO NUM-ENTRIES(vList-Comm):
            vComm = ENTRY(1, ENTRY(vCounter, vList-Comm), "=").
            FIND FIRST comm-rate WHERE
                       comm-rate.acct       =  "0"
                   AND comm-rate.commission =  vComm
                   AND comm-rate.kau        =  iContract + "," + iContCode
                   AND comm-rate.since      <= iDate
            NO-LOCK NO-ERROR.
            IF AVAILABLE comm-rate THEN
            DO:
               vCalcEPS = TRUE.
               LEAVE CHK.
            END.
         END.
         FOR EACH loan WHERE
                  loan.contract         =  "����"
            AND   loan.parent-cont-code =  iContCode
            AND   loan.parent-contract  =  iContract
            AND   loan.open-date        <= iDate
            AND  (loan.close-date       >= iDate
               OR loan.close-date       =  ?)
         NO-LOCK,
             EACH term-obl OF loan WHERE
                  term-obl.idnt =  1
         NO-LOCK:
            vCalcEPS = TRUE.
            LEAVE CHK.
         END.
         vCardOver = GetXattrValueEx("loan",
                                     iContract + "," + iContCode,
                                     "���⎢��",
                                     "").
         IF vCardOver =  "�����" THEN
         DO:
            FIND LAST loan-acct WHERE
                      loan-acct.acct-type =  "�।����"
                  AND loan-acct.cont-code =  iContCode
                  AND loan-acct.contract  =  iContract
                  AND loan-acct.since     <= iDate
            NO-LOCK NO-ERROR.
            IF AVAILABLE loan-acct THEN
            DO:
               vCalcEPS = CAN-FIND (LAST bloan-acct WHERE
                                         bloan-acct.acct      =  loan-acct.acct
                                     AND bloan-acct.acct-type BEGINS "SCS@"
                                     AND bloan-acct.since     <= iDate
                                    NO-LOCK).
            END.
         END. /* IF vCard-Over EQ "�����" THEN */
      END. /* CHK: DO: */
      IF vCalcEPS THEN
      DO:
         RUN pGetEpsLoan IN h_loan (iContract,
                                    iContCode,
                                    iDate,
                                    OUTPUT vEPS,
                                    OUTPUT vAddPay).
         IF vAddPay THEN
         DO:
            vEPS = vEPS * 100.
            LEAVE MAIN.
         END.
      END.
      vEPS = GET_COMM_LOAN (iContract,iContCode,"%�।",iDate).
   END. /* MAIN: DO: */
   RETURN vEPS.
END FUNCTION.

   /* ��� �����뢠���� �� ᫥���饩 ��㫥 %�। + %������ + 12*����� */
FUNCTION fCalcEpsSpec RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):
   DEF VAR vCredRate AS DEC  NO-UNDO. /* �⠢�� "%�।" */
   DEF VAR vOpenLim  AS DEC  NO-UNDO. /* �⠢�� "%������" */
   DEF VAR vComAcct  AS DEC  NO-UNDO. /* �⠢�� "�����" */
   DEF VAR vEPS      AS DEC  NO-UNDO.

   DEF BUFFER comm-rate  FOR comm-rate.

   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%�।"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCredRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%������"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vOpenLim = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "�����"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vComAcct = comm-rate.rate-comm.
   vEPS = vCredRate + vOpenLim + 12 * vComAcct.
   RETURN vEPS.
END FUNCTION.

{pfuncdef 
   &DefFunc = "RecalcFixedToProc"
   &Description = "������뢠�� 䨪�஢����� ������� � % �� �㬬� �������"
   &Parameters = "�����祭�� �������,����� �������,�㬬� ����ᨨ"
   &Result= "���祭�� � %"
   &Sample="RecalcFixedToProc(�।��,123,10000)"}
   
FUNCTION RecalcFixedToProc RETURNS DECIMAL PRIVATE 
   (INPUT iContract AS CHARACTER,
    INPUT iContCode AS CHARACTER,
    INPUT iSummComm AS DECIMAL):

   DEFINE BUFFER term-obl FOR term-obl.
   
   FIND FIRST term-obl WHERE term-obl.contract  =  iContract
                         AND term-obl.cont-code =  iContCode
                         AND term-obl.idnt      =  2
   NO-LOCK NO-ERROR.

   RETURN IF AVAILABLE term-obl 
          THEN iSummComm / term-obl.amt-rub * 100
          ELSE 0.       
END FUNCTION.

{pfuncdef 
   &DefFunc = "fCalcEpsSpec1"
   &Description = "�117��杏�  ��� �����뢠���� �� ᫥���饩 ��㫥 %�। + %��  �117��杏�"
   &Parameters = "�����祭�� �������,����� �������,���"
   &Result= "���祭�� ���"
   &Sample="fCalcEpsSpec1(�।��,123,01/01/16)"}
   
FUNCTION fCalcEpsSpec1 RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vCredRate AS DEC  NO-UNDO. /* �⠢�� "%�।" */
   DEF VAR vOpenLim  AS DEC  NO-UNDO. /* �⠢�� "%��" */
   DEF VAR oSumEPS   AS DEC  NO-UNDO.

   DEF BUFFER comm-rate  FOR comm-rate.

   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%�।"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCredRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%��"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vOpenLim = IF comm-rate.rate-fixed 
                 THEN RecalcFixedToProc(iContract,iContCode,comm-rate.rate-comm)
                 ELSE comm-rate.rate-comm.
   oSumEPS = vCredRate + vOpenLim .
   RETURN oSumEPS.
END FUNCTION.

{pfuncdef 
   &DefFunc = "fCalcEpsSpec2"
   &Description = "�117��杏� ��� �����뢠���� �� ᫥���饩 ��㫥 %�। + %��*365/��������������"
   &Parameters = "�����祭�� �������,����� �������,���"
   &Result= "���祭�� ���"
   &Sample="fCalcEpsSpec2(�।��,123,01/01/16)"}

FUNCTION fCalcEpsSpec2 RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vCredRate AS DEC  NO-UNDO. /* �⠢�� "%�।" */
   DEF VAR vOpenLim  AS DEC  NO-UNDO. /* �⠢�� "%��" */
   DEF VAR vLoanDays AS INT64 NO-UNDO .
   DEF VAR oSumEPS      AS DEC  NO-UNDO.

   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER loan       FOR loan.

   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%�।"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCredRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
             comm-rate.kau        =  iContract + "," + iContCode
         AND comm-rate.commission =  "%��"
         AND comm-rate.since      <= iDate
         AND comm-rate.acct       =  "0"
   NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vOpenLim = IF comm-rate.rate-fixed 
                 THEN RecalcFixedToProc(iContract,iContCode,comm-rate.rate-comm)
                 ELSE comm-rate.rate-comm.
   FIND FIRST loan WHERE
              loan.contract  =   iContract
          AND loan.cont-code =   iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
      vLoanDays = loan.end-date - loan.open-date.
   
   IF vLoanDays = 0
   THEN
      oSumEPS = 0 .
   ELSE
      oSumEPS = vCredRate + vOpenLim * 365 / vLoanDays .
   RETURN oSumEPS.
END FUNCTION.

/* ��業⭠� �⠢�� �� ����祭��� ��� ���� �����뢠���� �� ᫥���饩 ��㫥 
  %���� + %������ + 12*����� */
FUNCTION fCalcPrpsSpec RETURNS DEC
   (iContract AS CHAR,
   iContCode AS CHAR,
   iDate     AS DATE):
   DEF VAR vCrPrRate AS DEC NO-UNDO. /* �⠢�� "%����" */
   DEF VAR vComLim   AS DEC NO-UNDO. /* �⠢�� "%������" */
   DEF VAR vComAcct  AS DEC NO-UNDO. /* �⠢�� "�����" */
   DEF VAR vPenyK    AS DEC NO-UNDO. /* �⠢�� "����-�" */
   DEF VAR vPenyAcct AS DEC NO-UNDO. /* �⠢�� "������" */
   DEF VAR vPrPS     AS DEC NO-UNDO.

   DEF BUFFER comm-rate FOR comm-rate.

   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "%����"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vCrPrRate = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "%������"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vComLim = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "�����"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vComAcct = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "����-�"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vPenyK = comm-rate.rate-comm.
   FIND LAST comm-rate WHERE
      comm-rate.kau        =  iContract + "," + iContCode
      AND comm-rate.commission =  "������"
      AND comm-rate.since      <= iDate
      AND comm-rate.acct       =  "0"
      NO-LOCK NO-ERROR.
   IF AVAIL comm-rate THEN
      vPenyAcct = comm-rate.rate-comm.

   vPrPS = IF vCrPrRate <> ? THEN
               vCrPrRate + vComLim + 12 * vComAcct
           ELSE
               (vPenyK + vPenyAcct) * (DATE(1, 1, YEAR(iDate) + 1) - DATE(1, 1, YEAR(iDate))).

   RETURN vPrPS.
END FUNCTION.

/* ��楤�� �� ������ ������� ��।���� ����� �㭪�� ��� ���� ���
** ����室��� ��������, ����᪠�� ��, �����頥� ����⠭��� �⠢�� ��� */
PROCEDURE pCalcEps117.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iClassCode AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO. /* ��� ����� */
   DEF OUTPUT PARAM oSum       AS DEC  NO-UNDO. /* �㬬� */

   DEF VAR vProcName AS CHAR NO-UNDO. /* ��� ��뢠���� ��楤��� */

   DEF BUFFER term-obl FOR term-obl. /* ���������� ����. */

   vProcName = GetXAttrInit(iClassCode, "�117��杏�").
   IF NOT {assigned vProcName} THEN
      vProcName = fGetSetting("�117��杏�","","fCalcEpsLoan").
   IF NOT {assigned vProcName} THEN
      vProcName = "fCalcEpsLoan".
/* ��⠢�� ���� ���� */
   FIND FIRST loan
      WHERE  loan.contract  = iContract
        AND  loan.cont-code = iContCode
        AND (loan.cont-type = '���ॡ'
          OR loan.cont-type = '��'
          OR loan.cont-type = ?)
      NO-LOCK NO-ERROR.
   IF AVAIL loan THEN vProcName = "PscMortgage".
/* ����� ��⠢�� ���� ���� */
   oSum = DYNAMIC-FUNCTION(vProcName, iContract, iContCode, iDate) NO-ERROR.
END PROCEDURE.

/*  */

{pfuncdef 
    &DefProc="fCalcMarga"
    &Description="����祭�� ��ਠ樮���� ��ন �� ���� ᪮॥ ����� - ����祭�� ��ਠ樮���� ��ন �� �������"}
FUNCTION fCalcMarga RETURNS DEC
   (iAcct AS CHAR,
    iCurrency AS CHAR,
    iDateBeg  AS DATE,
    iDateEnd AS DATE):

   DEF VAR vRetVal AS DEC NO-UNDO.
   DEF VAR vVM     AS DEC NO-UNDO.

   DEF BUFFER acct FOR acct. /* ���������� ����. */
   DEF BUFFER loan FOR loan. /* ���������� ����. */

   MAIN:
   DO:
      {find-act.i
         &bact = acct
         &acct = iAcct
         &curr = iCurrency
      }
      IF NOT AVAIL acct THEN
         LEAVE main.
      FOR EACH loan WHERE
               loan.cust-cat =  acct.cust-cat
           AND loan.cust-id  =  acct.cust-id
           AND loan.contract =  "������"
      NO-LOCK:
         RUN pCalcMarga IN h_loanx (loan.contract,
                                    loan.cont-code,
                                    iDateBeg,
                                    iDateEnd,
                                    NO,          /* �� ������뢠�� �� */
                                    OUTPUT vVM).
         vRetVal = vRetVal + vVM.
      END.
   END.
   RETURN vRetVal.
END FUNCTION.

/* ��।����:
   - �ਧ��� ������ ����窨 �� �������� (�� ���� �� ����������)+ ���ᨬ���� �ப ����窨
   - �᫨ ���� �㬬� ����窨 �� ��㤥
   - �㬬� ��㤭�� ����������
old version <= D66
*/
PROCEDURE CalcDebt115.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.   /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.   /* ����� ������� */
   DEF INPUT  PARAM iDate     AS DATE   NO-UNDO.   /* ��� ���� */
   DEF OUTPUT PARAM oProcFl   AS LOG    NO-UNDO.   /* �ਧ��� ������ ����窨 �� �������� (�� ���� �� ����������) */
   DEF OUTPUT PARAM oQDaysPr  AS INT64    NO-UNDO.   /* ���ᨬ���� �ப ����窨 (������⢮ ���� ����窨) */
   DEF OUTPUT PARAM oCredSum  AS DEC    NO-UNDO.   /* �㬬� ��㤭�� ������������ */
   DEF OUTPUT PARAM oCrPrSum  AS DEC    NO-UNDO.   /* �㬬� ����窨 �� ��㤥 */

   DEF VAR vDate    AS DATE   NO-UNDO. /* �஬����筠� ��� */
   DEF VAR vProcPar AS CHAR   NO-UNDO INIT "7,10,48,210,248". /* ��ࠬ����, �� ����� ��।������ ����稥 ����窨 */
   DEF VAR vCredPar AS CHAR   NO-UNDO INIT "0,13".            /* ��ࠬ����, �� ����� ��।������ �㬬� ��㤭�� ������������ */

   DEF VAR vParSum  AS DEC   NO-UNDO. /* �㬬� ��ࠬ��� */
   DEF VAR vDb      AS DEC   NO-UNDO.
   DEF VAR vCr      AS DEC   NO-UNDO.
   DEF VAR vi       AS INT64 NO-UNDO.
   DEF VAR vPar     AS INT64 NO-UNDO.

   DEF BUFFER loan-int FOR loan-int. /* ���������� ����. */

   MAIN_BLOCK:
   DO:
            /* �㬬� �㤭�� ������������ */
      DO vi = 1 TO NUM-ENTRIES(vCredPar):
         vPar = INT64(ENTRY(vi,vCredPar)).
         RUN RE_PARAM IN h_Loan (
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
         oCredSum = oCredSum + vParSum.
      END.

               /* �ਧ��� ������ ����窨 �� �������� (�� ���� �� ����������) */
      oProcFl = LN_GetParamsInteres (iContract,
                                     iContCode,
                                     vProcPar,
                                     iDate) >  0.
      IF NOT oProcFl THEN LEAVE MAIN_BLOCK.

               /* ��।��塞 ���� ��砫� ����窨 */
      BLCK:
      FOR EACH loan-int WHERE
               loan-int.contract    =  iContract
         AND   loan-int.cont-code   =  iContCode
         AND   loan-int.mdate       <= iDate
         AND  (CAN-DO(vProcPar, STRING (loan-int.id-d))
            OR CAN-DO(vProcPar, STRING (loan-int.id-k)))
      NO-LOCK
      BY loan-int.mdate DESCENDING:
        vDate = loan-int.mdate.
        IF LN_GetParamsInteres (iContract,
                                iContCode,
                                vProcPar,
                                loan-int.mdate - 1) <= 0
        THEN LEAVE BLCK.
      END.
            /* ������⢮ ���� ����窨 */
      oQDaysPr = iDate - vDate.
   END.

   RETURN.
END PROCEDURE.


/* ��।����:
   - �ਧ��� ������ ����窨 �� �������� (�� ���� �� ����������)+ ���ᨬ���� �ப ����窨
   - �᫨ ���� �㬬� ����窨 �� ��㤥
   - �㬬� ��㤭�� ����������
New version > D66
re_proc_name+NO  ��⠥� �㬬� �� ��ࠬ���� ��� ࠧ��᪨.

*/
PROCEDURE CalcDebt115a.
   DEF INPUT  PARAMETER iContract AS CHAR   NO-UNDO.   /* �����祭�� ������� */
   DEF INPUT  PARAMETER iContCode AS CHAR   NO-UNDO.   /* ����� ������� */
   DEF INPUT  PARAMETER iDate     AS DATE   NO-UNDO.   /* ��� ���� */
   DEF INPUT-OUTPUT PARAMETER ioProcFl   AS LOG    NO-UNDO. /* �ਧ��� ������ ����窨 �� �������� (�� ���� �� ����������) */
   DEF INPUT-OUTPUT PARAMETER ioQDaysPr  AS INT64  NO-UNDO. /* ���ᨬ���� �ப ����窨 (������⢮ ���� ����窨)          */
   DEF OUTPUT PARAMETER oSumZ1 AS DECIMAL NO-UNDO .    /* �㬬� ��㤭�� ����������    */
   DEF OUTPUT PARAMETER oSumZ2 AS DECIMAL NO-UNDO .    /* �㬬� ���������� �����ᨨ   */
   DEF OUTPUT PARAMETER oSumZ3 AS DECIMAL NO-UNDO .    /* �㬬� ���������� �����      */
   DEF OUTPUT PARAMETER oSumZ4 AS DECIMAL NO-UNDO .    /* �㬬� ���������� �ॡ������ */
   DEF OUTPUT PARAMETER oSumP1 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ��㤭�� ����������    */
   DEF OUTPUT PARAMETER oSumP2 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ���������� �����ᨨ   */
   DEF OUTPUT PARAMETER oSumP3 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ���������� �����      */
   DEF OUTPUT PARAMETER oSumP4 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ���������� �ॡ������ */

   DEF VAR vDate          AS DATE  NO-UNDO. /* �஬����筠� ��� */
   DEF VAR vParSum        AS DEC   NO-UNDO. /* �㬬� ��ࠬ��� */
   DEF VAR vDb            AS DEC   NO-UNDO.
   DEF VAR vCr            AS DEC   NO-UNDO.
   DEF VAR vi             AS INT64 NO-UNDO.
   DEF VAR vPar           AS INT64 NO-UNDO.
   DEF VAR vCrcDate       AS DATE  NO-UNDO.
   DEF VAR vPrsProcDate   AS DATE  NO-UNDO.

   DEF BUFFER loan-int FOR loan-int. /* ���������� ����. */
   DEF BUFFER loan     FOR loan.
   DEF BUFFER code     FOR code.

   DEFINE VARIABLE oSumm     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE oSummRub  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE oCurr     AS CHARACTER NO-UNDO.

   ASSIGN
      oSumZ1   = 0
      oSumZ2   = 0
      oSumZ3   = 0
      oSumZ4   = 0
      oSumP1   = 0
      oSumP2   = 0
      oSumP3   = 0
      oSumP4   = 0
      vCrcDate = LastWorkDay(iDate)
   .

   MAIN_BLOCK:
   DO:
   FIND FIRST loan NO-LOCK where
              loan.contract = iContract AND
              loan.cont-code = iContCode  NO-ERROR .


    /* �㬬�  ������������ */
      DO vi = 1 TO NUM-ENTRIES(mParz1):
         vPar = INT64(ENTRY(vi,mParz1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */

              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ1 = oSumZ1 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParz2):
         vPar = INT64(ENTRY(vi,mParz2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ2 = oSumZ2 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParz3):
         vPar = INT64(ENTRY(vi,mParz3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ3 = oSumZ3 + vParSum.
      END.
      DO vi = 1 TO NUM-ENTRIES(mParz4):
         vPar = INT64(ENTRY(vi,mParz4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ4 = oSumZ4 + vParSum.
      END.

      IF ioProcFl <> YES THEN 
      DO:   
            /* ��।������ ����稥 ����窨 - ���� ࠧ ��� �।�⭮� ����� */
         RUN LN_GetPrsDate(iContract,
                           ENTRY (1, iContCode, " "),
                           iDate,
                           mProcPar,
                           mAlgFIFO,
                           mCurrDay,
                           OUTPUT vPrsProcDate,
                           OUTPUT ioProcFl).

         ioQDaysPr = iDate - vPrsProcDate.
      END.

         /* ���� �᫨ ����窨 ��� �� ��直� ��砩 �᭮ ����⠥� ��ࠬ����  */
         /* �㬬� ����祭���  ������������ */
      DO vi = 1 TO NUM-ENTRIES(mParp1):
         vPar = INT64(ENTRY(vi,mParp1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSump1 = oSump1 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParp2):
         vPar = INT64(ENTRY(vi,mParp2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSump2 = oSump2 + vParSum.
      END.

      DO vi = 1 TO NUM-ENTRIES(mParp3):
         vPar = INT64(ENTRY(vi,mParp3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSump3 = oSump3 + vParSum.
      END.
      DO vi = 1 TO NUM-ENTRIES(mParp4):
         vPar = INT64(ENTRY(vi,mParp4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).


         oSump4 = oSump4 + vParSum.
      END.
   END.

   RETURN.
END PROCEDURE.

/* ������ CalcDebt115� � ������������ ��८�।������ ��ࠬ��஢ */
PROCEDURE CalcDebt115b.
   DEF INPUT  PARAMETER iContract AS CHAR   NO-UNDO.   /* �����祭�� ������� */
   DEF INPUT  PARAMETER iContCode AS CHAR   NO-UNDO.   /* ����� ������� */
   DEF INPUT  PARAMETER iDate     AS DATE   NO-UNDO.   /* ��� ���� */
   DEF INPUT-OUTPUT PARAMETER ioProcFl   AS LOG    NO-UNDO. /* �ਧ��� ������ ����窨 �� �������� (�� ���� �� ����������) */
   DEF INPUT-OUTPUT PARAMETER ioQDaysPr  AS INT64  NO-UNDO. /* ���ᨬ���� �ப ����窨 (������⢮ ���� ����窨)          */
   DEF OUTPUT PARAMETER oSumZ1 AS DECIMAL NO-UNDO .    /* �㬬� ��㤭�� ����������    */
   DEF OUTPUT PARAMETER oSumZ2 AS DECIMAL NO-UNDO .    /* �㬬� ���������� �����ᨨ   */
   DEF OUTPUT PARAMETER oSumZ3 AS DECIMAL NO-UNDO .    /* �㬬� ���������� �����      */
   DEF OUTPUT PARAMETER oSumZ4 AS DECIMAL NO-UNDO .    /* �㬬� ���������� �ॡ������ */
   DEF OUTPUT PARAMETER oSumP1 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ��㤭�� ����������    */
   DEF OUTPUT PARAMETER oSumP2 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ���������� �����ᨨ   */
   DEF OUTPUT PARAMETER oSumP3 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ���������� �����      */
   DEF OUTPUT PARAMETER oSumP4 AS DECIMAL NO-UNDO .    /* �㬬� ����祭��� ���������� �ॡ������ */
   DEF INPUT  PARAMETER iParz1 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ��㤭�� ����������    */
   DEF INPUT  PARAMETER iParz2 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ���������� �����ᨨ   */
   DEF INPUT  PARAMETER iParz3 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ���������� �����      */
   DEF INPUT  PARAMETER iParz4 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ���������� �ॡ������ */
   DEF INPUT  PARAMETER iParp1 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ����祭��� ��㤭�� ����������    */
   DEF INPUT  PARAMETER iParp2 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ����祭��� ���������� �����ᨨ   */
   DEF INPUT  PARAMETER iParp3 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ����祭��� ���������� �����      */
   DEF INPUT  PARAMETER iParp4 AS CHAR NO-UNDO .    /* ᯨ᮪ ��ࠬ��஢ : �㬬� ����祭��� ���������� �ॡ������ */
   DEF INPUT  PARAMETER iProcPar  AS CHAR NO-UNDO .  /* ��ࠬ����, �� ����� ��।������ ����稥 ����窨 */
   DEF INPUT  PARAMETER iLineLoan AS LOG NO-UNDO .   /* ���ᨬ��쭠� ����窠 �� : ������� - NO, �।�⭮� ����� - YES */
   DEF OUTPUT PARAMETER oMsgChar  AS CHARACTER NO-UNDO . /* ��⮪�� */

   DEFINE VARIABLE mTmpParz1        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParz2        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParz3        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParz4        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp1        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp2        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp3        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpParp4        AS CHARACTER NO-UNDO .
   DEFINE VARIABLE mTmpProcPar      AS CHARACTER NO-UNDO .
   DEFINE VARIABLE vTmpProcPar1     AS CHARACTER NO-UNDO .

   DEF VAR vDate     AS DATE   NO-UNDO. /* �஬����筠� ��� */
   DEF VAR vParSum   AS DEC    NO-UNDO. /* �㬬� ��ࠬ��� */
   DEF VAR vDb       AS DEC    NO-UNDO.
   DEF VAR vCr       AS DEC    NO-UNDO.
   DEF VAR vi        AS INT64  NO-UNDO.
   DEF VAR vPar      AS INT64  NO-UNDO.
   DEF VAR vCrcDate  AS DATE   NO-UNDO.
   DEF VAR vTmpDec   AS DEC    NO-UNDO.

   DEF BUFFER loan-int FOR loan-int. /* ���������� ����. */
   DEF BUFFER loan     FOR loan.

   DEFINE VARIABLE oSumm     AS DECIMAL NO-UNDO .
   DEFINE VARIABLE oSummRub  AS DECIMAL NO-UNDO .
   DEFINE VARIABLE oCurr     AS CHARACTER NO-UNDO .


   DEF BUFFER bbloan FOR loan.
   DEF BUFFER bbbloan FOR loan.

   ASSIGN
      mTmpParz1 = (IF iParz1 >  "" THEN iParz1 ELSE mParz1)
      mTmpParz2 = (IF iParz2 >  "" THEN iParz2 ELSE mParz2)
      mTmpParz3 = (IF iParz3 >  "" THEN iParz3 ELSE mParz3)
      mTmpParz4 = (IF iParz4 >  "" THEN iParz4 ELSE mParz4)
      mTmpParp1 = (IF iParp1 >  "" THEN iParp1 ELSE mParp1)
      mTmpParp2 = (IF iParp2 >  "" THEN iParp2 ELSE mParp2)
      mTmpParp3 = (IF iParp3 >  "" THEN iParp3 ELSE mParp3)
      mTmpParp4 = (IF iParp4 >  "" THEN iParp4 ELSE mParp4)
      mTmpProcPar = (IF iProcPar >  "" THEN iProcPar ELSE mProcPar)
      vTmpProcPar1 = REPLACE(mTmpProcPar, ";", ",")
      oSumZ1   = 0
      oSumZ2   = 0
      oSumZ3   = 0
      oSumZ4   = 0
      oSumP1   = 0
      oSumP2   = 0
      oSumP3   = 0
      oSumP4   = 0
      vCrcDate = LastWorkDay(iDate)
   .

   MAIN_BLOCK:
   DO:
   FIND FIRST loan NO-LOCK where
              loan.contract = iContract AND
              loan.cont-code = iContCode  NO-ERROR .


    /* �㬬�  ������������ */
      IF NOT (mTmpParz1 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz1):
         vPar = INT64(ENTRY(vi,mTmpParz1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */

              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ1 = oSumZ1 + vParSum.
      END.

      IF NOT (mTmpParz2 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz2):
         vPar = INT64(ENTRY(vi,mTmpParz2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ2 = oSumZ2 + vParSum.
      END.

      IF NOT (mTmpParz3 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz3):
         vPar = INT64(ENTRY(vi,mTmpParz3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ3 = oSumZ3 + vParSum.
      END.

      IF NOT (mTmpParz4 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParz4):
         vPar = INT64(ENTRY(vi,mTmpParz4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSumZ4 = oSumZ4 + vParSum.
      END.


      IF ioProcFl =  ? THEN /* ��।������ ����稥 ����窨 - ���� ࠧ ��� �।�⭮� ����� */
      DO:         
          
          
         ioProcFl = FALSE .
         IF iLineLoan THEN /* ������ ����窨 �� �।�⭮� ����� */
         DO:
            /* �ਧ��� ������ ����窨 �� �������� (�� ���� �� ����������) */
            FOR FIRST bbloan where bbloan.contract  =  iContract
                               and bbloan.cont-code =  entry(1,iContCode ," " ) NO-LOCK :
               IF  LN_GetParamsInteres (iContract,
                                        bbloan.Cont-Code,
                                        vTmpProcPar1,
                                        iDate) >  0
               THEN
               DO:  /* ���� ����窠 �� �墠�뢠�饬� */
                 ioProcFl = true .
                 LEAVE.
               END.
               ELSE
               DO: /* ����� ���� ����窠 �� �࠭蠬 */
                  FOR EACH bbbloan where bbbloan.contract = iContract
                                     and bbbloan.cont-code BEGINS  entry(1,iContCode ," " ) + " "
                     NO-LOCK:
                     IF  LN_GetParamsInteres (iContract,
                                              bbbloan.Cont-Code,
                                              vTmpProcPar1,
                                              iDate) >  0
                     THEN DO:
                        ioProcFl = true .
                        LEAVE.
                     END.
                  END.
               END.
            END.
         END.
         ELSE /* ������ ����窨 �� ������� */
         DO:
            FOR FIRST bbloan where bbloan.contract  =  iContract
                               and bbloan.cont-code =  iContCode NO-LOCK :
               IF  LN_GetParamsInteres (iContract,
                                        bbloan.Cont-Code,
                                        vTmpProcPar1,
                                        iDate) >  0
               THEN
               DO:  /* ���� ����窠 �� �������� */
                 ioProcFl = true .
               END.
            END.
         END.

         /* ��।��塞 ���� ��砫� ����窨 */
         ioQDaysPr = 0.
         IF ioProcFl THEN
         DO:
                      /* ��।������ ����稥 ����窨 - ���� ࠧ ��� �।�⭮� ����� */
            
            RUN LN_GetPrsDate(iContract,
                              ENTRY (1, iContCode, " "),
                              iDate,
                              mTmpProcPar,
                              mAlgFIFO,
                              mCurrDay,
                              OUTPUT vDate,
                              OUTPUT ioProcFl).
                                        
            ioQDaysPr = iDate - vDate. 
             
            {empty ttLoanInt}
            IF iLineLoan THEN
            DO:
               FOR EACH loan-int WHERE loan-int.contract  =  iContract
                                   AND loan-int.cont-code =  entry(1,iContCode ," ")
                                   AND loan-int.mdate     <= iDate
                                 NO-LOCK:
                  IF    CAN-DO(mTmpProcPar, STRING (loan-int.id-d))
                     OR CAN-DO(mTmpProcPar, STRING (loan-int.id-k)) THEN
                  DO:
                     CREATE ttLoanInt.
                     ASSIGN
                        ttLoanInt.cont-code = loan-int.cont-code
                        ttLoanInt.mdate     = loan-int.mdate
                     .
                  END.
               END.

               FOR EACH loan-int WHERE loan-int.contract  =  iContract
                                   AND loan-int.cont-code BEGINS entry(1,iContCode ," " ) + " "
                                   AND loan-int.mdate     <= iDate
                                 NO-LOCK:
                  IF    CAN-DO(mTmpProcPar, STRING (loan-int.id-d))
                     OR CAN-DO(mTmpProcPar, STRING (loan-int.id-k)) THEN
                  DO:
                     CREATE ttLoanInt.
                     ASSIGN
                        ttLoanInt.cont-code = loan-int.cont-code
                        ttLoanInt.mdate     = loan-int.mdate
                     .
                  END.
               END.
            END.
            ELSE
            DO:
               FOR EACH loan-int WHERE loan-int.contract  =  iContract
                                   AND loan-int.cont-code =  iContCode
                                   AND loan-int.mdate     <= iDate
                                 NO-LOCK:
                  IF    CAN-DO(mTmpProcPar, STRING (loan-int.id-d))
                     OR CAN-DO(mTmpProcPar, STRING (loan-int.id-k)) THEN
                  DO:
                     CREATE ttLoanInt.
                     ASSIGN
                        ttLoanInt.cont-code = loan-int.cont-code
                        ttLoanInt.mdate     = loan-int.mdate
                     .
                  END.
               END.
            END.

            FOR EACH ttLoanInt BY ttloanInt.mdate DESCENDING:
               vDate   = ttLoanInt.mdate.
               vTmpDec = LN_GetParamsInteres (iContract,
                                       ttLoanInt.cont-code,
                                       vTmpProcPar1,
                                       ttLoanInt.mdate - 1).

               oMsgChar = oMsgChar + ttLoanInt.cont-code + " " + STRING(vDate,"99/99/9999") + " (" + STRING(vDate - 1,"99/99/9999") + " : " + STRING(vTmpDec) + ")~n".
               IF vTmpDec <= 0
               THEN
               DO:
                  /* ������⢮ ���� ����窨 */                  
                  oMsgChar  = oMsgChar + ttLoanInt.cont-code + " " + STRING(iDate,"99/99/9999") + " - " + STRING(vDate,"99/99/9999") + " = " + STRING(ioQDaysPr) + "~n".
                  LEAVE.
               END.
            END.
         END.
      END. /* IF ioProcFl EQ ? THEN */

     /* ���� �᫨ ����窨 ��� �� ��直� ��砩 �᭮ ����⠥� ��ࠬ����  */

    /* �㬬� ����祭���  ������������ */
      IF NOT (mTmpParp1 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp1):
         vPar = INT64(ENTRY(vi,mTmpParp1)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSump1 = oSump1 + vParSum.
      END.

      IF NOT (mTmpParp2 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp2):
         vPar = INT64(ENTRY(vi,mTmpParp2)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */
            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSump2 = oSump2 + vParSum.
      END.

      IF NOT (mTmpParp3 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp3):
         vPar = INT64(ENTRY(vi,mTmpParp3)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
            /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).

         oSump3 = oSump3 + vParSum.
      END.

      IF NOT (mTmpParp4 BEGINS "NO") THEN
      DO vi = 1 TO NUM-ENTRIES(mTmpParp4):
         vPar = INT64(ENTRY(vi,mTmpParp4)).
         RUN VALUE (re_proc_name (vpar, no)) (
              iContract,            /* ��� ������� */
              iContCode,            /* ����� ������� */
              vPar,                 /* ��� ��ࠬ��� */
              iDate,                /* ��� ���� */
              OUTPUT vParSum,       /* �㬬� ��ࠬ��� */
              OUTPUT vDb,           /* ����� ������ */
              OUTPUT vCr).          /* �।�⮢� ������ */

            RUN GetParP  IN h_Loan (RECID(loan),vPar,vParSum,OUTPUT oSummRub, OUTPUT oCurr).
             /* �᫨ ��� ���� ������ ����� �⫨筠� �� ��樮���쭮�,
                ��ॢ���� �㬬� � �������� ������ */
            IF oCurr <> "" THEN
               vParSum = CurToBase("����",oCurr,vCrcDate,vParSum).


         oSump4 = oSump4 + vParSum.
      END.
   END.

   RETURN.
END PROCEDURE.

/* �����頥� �㬬� �� ����� ����権 � �� ����稨 ���祭�� �� �� �������.
   �㬬� �����頥��� ⮫쪮 � ⮬ ��砥, �᫨ ���� �����-���� ���祭�� �� ��,
   � ��⨢��� ��砥 � ����⢥ १���� �㤥� �����饭 ? */
FUNCTION GetPercntLoanDR RETURNS DECIMAL
   (iContract  AS CHARACTER,  /* �����祭�� �������. */
   iContCode  AS CHARACTER,  /* ����� �������. */
   iListIdOp  AS CHARACTER,  /* ���� ����権. */
   iDR        AS CHARACTER,  /* ��� �������⥫쭮�� ४�����. */
   iDate      AS DATE): /* ��� ����砭�� ����, �� ������ ����室��� ������ �㬬� �� ��।���� ������. */

   DEFINE VARIABLE vSumm     AS DECIMAL   INIT 0 NO-UNDO. /* �㬬� �� ������ */
   DEFINE VARIABLE vI        AS INT64     NO-UNDO. 
   DEFINE VARIABLE vDR       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTemporal AS LOG       NO-UNDO. /* �ਧ��� ⥬���஢������ �� */

   DEFINE BUFFER loan FOR loan.
   DEFINE BUFFER bclass FOR class.
   DEFINE BUFFER xattr FOR xattr.

   /* �饬 ������� */
   FIND FIRST loan WHERE
      loan.contract  =  iContract
      AND loan.cont-code =  iContCode
      NO-LOCK NO-ERROR.
   /* �᫨ ������� �� ������, �����頥� ? */
   IF NOT AVAILABLE loan THEN
      RETURN ?.

   /* �஢��塞 �ਧ��� ⥬���஢������ ��।������ �� */
   BLK:
   FOR EACH bclass WHERE 
      bclass.PROGRESS-CODE =  loan.class-code 
      NO-LOCK,
      FIRST xattr WHERE 
         xattr.Class-Code =  bclass.CLASS-CODE
         AND xattr.Xattr-Code =  iDR
         NO-LOCK:
      vTemporal = xattr.temporal. 
      LEAVE BLK.
   END.
   /* �᫨ �� ⥬���஢����, � ��६ ���祭�� �� �� 㪠������ ����,
      �᫨ ���, � ���� ��६ ��� ���祭�� */
   IF vTemporal THEN
      vDR = GetTempXAttrValueEx("loan",
         loan.contract + "," + loan.cont-code,
         iDR,
         iDate,
         ?).
   ELSE
      vDR = GetXAttrValueEx("loan",
         loan.contract + "," + loan.cont-code,
         iDR,
         ?).
   /* �᫨ �� �� ��������, �����頥� � ����⢥ १���� ? */
   IF vDR =  ? THEN
      RETURN ?.

   /* ��⠥� �㬬� �� ��।���� ����� ����権 */
   DO vI = 1 TO NUM-ENTRIES(iListIdOp):      
      RUN pint.p (iContract, iContCode, loan.open-date, iDate, ENTRY(vI, iListIdOp)).
      FOR EACH otch1:
         vSumm = vSumm + otch1.summ_pr .
      END.
   END.

   RETURN vSumm.

END FUNCTION.

/* ����� �����������⥩ �� �࠭蠬, �᫨ %% ������� �� �墠�뢠�饬 ������� � ��� �࠭襩 ࠧ�� %% �⠢�� */
PROCEDURE GET_PROP_PROC_TR:
    DEF INPUT PARAM iContract   AS CHAR NO-UNDO.
    DEF INPUT PARAM iContCode   AS CHAR NO-UNDO.
    DEF INPUT PARAM iEndDate    AS DATE NO-UNDO.
    DEF OUTPUT PARAM table FOR tres.
    
    DEF VAR vOstProc AS DEC NO-UNDO.
    
    DEF BUFFER loan FOR loan.
    
    {empty tres}
    /* ���⮪ ������������ �� %% �� �墠�뢠�饬 ������� */
    RUN SetSysConf IN h_base ("AddParams","10,29,34,48").
    RUN GetProcentUnAccount (iContract,
                             iContcode,
                             iEndDate,
                             OUTPUT vOstProc).
    RUN DeleteOldDataProtocol IN h_base("AddParams").
    /* ����砥� ������ �㬬� ���᫥���� %% �� ������� �࠭�� */
    RUN SetSysConf IN h_Base("�����멃�䨪��業⮢","��").
    FOR EACH loan WHERE loan.contract  =  iContract 
                    AND loan.cont-code BEGINS iContcode + " "
                    AND NUM-ENTRIES(loan.cont-code," ") >  1
        NO-LOCK BY loan.open-date DESC:
        CREATE tres.
        ASSIGN
           tres.cont-code = loan.cont-code
           tres.od        = ROUND(LnPrincipal (loan.contract, loan.cont-code,iEndDate,""),2)
           tres.open-date = loan.open-date. 
        RUN pint.p (loan.contract, loan.cont-code, loan.open-date + 1, iEndDate, "4").
        FOR EACH otch1:
            ACCUM otch1.summ_pr (TOTAL) .
        END.
        ASSIGN
           tres.proc = MIN(ACCUM TOTAL  otch1.summ_pr, vOstProc)
           vOstProc  = vOstProc - tres.proc.    
    END.
    RUN DeleteOldDataProtocol IN h_base("�����멃�䨪��業⮢").
    
END PROCEDURE. 
    
/*
�饬 ���� �� ���� ���� ��ண� ���浪�, �᫨ ��室�� - � �����頥� �㬬� ���⪮� �������� ��⮢ �� 㪠������ ���� � �㡫��
*/
PROCEDURE GET_SUMM_BS2:
   DEF INPUT  PARAM iContract         AS CHAR     NO-UNDO. 
   DEF INPUT  PARAM iContCode         AS CHAR     NO-UNDO.
   DEF INPUT  PARAM iDate             AS DATE     NO-UNDO. /*��� �� ���ன ���� ���⮪*/
   DEF INPUT  PARAM iAcct             AS INT64    NO-UNDO. /*���� ��ண� ���浪�*/
   DEF INPUT  PARAM iFindTransh       AS LOGICAL  NO-UNDO. /*�᪠�� ⮫쪮 �� �墠�뢠�饬 �������(���) ��� ��� � �� �࠭��(��)*/
   DEF OUTPUT PARAM oIsAvailability   AS LOGICAL  NO-UNDO. /*����稥(��) \ ������⢨�(���) ����*/
   DEF OUTPUT PARAM oBalance          AS DEC      NO-UNDO. /*���⪮ �� ���� iDate �� �������� ��� � �㡫��*/
   
   DEF BUFFER bloan-acct FOR loan-acct.
   
   /*���⨬ ⥬����� ⠡���� � ��⠬�*/
   {empty ttAcct}
   
   oIsAvailability = NO.
   oBalance = 0.
   /*�饬 �������,*/
   FOR EACH loan WHERE
            loan.contract  =  iContract
        AND loan.cont-code =  iContCode 
   NO-LOCK,
       /*��室�� �������騩 ��� ������� � 㪠���� ��2*/    
       EACH loan-acct WHERE
            loan-acct.contract  =  loan.contract
        AND loan-acct.cont-code =  loan.cont-code
        AND loan-acct.currency  =  loan.currency
        AND loan-acct.since     <= iDate
        /*�஢��塞 ��� �� ���� � ⠮�� �� ஫�� �� ����� ������� ����*/
        AND NOT CAN-FIND(LAST bloan-acct WHERE
                              bloan-acct.contract  =  loan.contract
                          AND bloan-acct.cont-code =  loan.cont-code
                          AND bloan-acct.acct-type =  loan-acct.acct-type
                          AND bloan-acct.since     >  loan-acct.since
                          AND bloan-acct.since     <= iDate 
                         NO-LOCK)
   NO-LOCK,
       FIRST acct WHERE
             acct.acct     =  loan-acct.acct
         AND acct.currency =  loan.currency
         AND acct.bal-acct =  iAcct
         /*�஢��塞 �� ��ࠡ��뢠���� �� ��� ���� ࠭��*/
         AND NOT CAN-FIND(FIRST ttAcct WHERE
                                ttAcct.acct =  acct.acct 
                          NO-LOCK)
   NO-LOCK BY loan-acct.since:
      /*���������� ����*/
      CREATE ttAcct.
      ttAcct.acct = acct.acct.
      /*���� ������ !*/
      oIsAvailability = YES.
      /*����塞 ���⮪ �� ����*/      
      RUN Acct-Pos-Pure IN h_base (acct.acct, 
                              loan.currency, 
                              iDate, 
                              iDate, 
                              CHR(251)).
      /*��⮢ ����� ���� ��᪮�쪮, ���⮬� �㬬��㥬 १����(� �㡫��)*/
      oBalance  = oBalance + IF loan.currency =  "" THEN sh-bal
                                                    ELSE CurToBase("����",loan.currency,iDate,sh-val).
   END.
   /*�᫨ �᪠�� ⠪�� � �� �࠭��*/
   IF iFindTransh THEN
      /*� �饬 �� �࠭��*/
      FOR EACH loan WHERE
               loan.contract  =                 iContract
           AND loan.cont-code BEGINS            iContCode + " "
           AND NUM-ENTRIES(loan.cont-code, " ") =  2
      NO-LOCK,
      /*��室�� �������騩 ��� ������� � 㪠���� ��2*/    
          EACH loan-acct WHERE
               loan-acct.contract  =  loan.contract
           AND loan-acct.cont-code =  loan.cont-code
           AND loan-acct.currency  =  loan.currency
           AND loan-acct.since     <= iDate
           /*�஢��塞 ��� �� ���� � ⠮�� �� ஫�� �� ����� ������� ����*/
           AND NOT CAN-FIND(LAST bloan-acct WHERE
                                 bloan-acct.contract  =  loan.contract
                             AND bloan-acct.cont-code =  loan.cont-code
                             AND bloan-acct.acct-type =  loan-acct.acct-type
                             AND bloan-acct.since     >  loan-acct.since 
                             AND bloan-acct.since     <= iDate
                            NO-LOCK)
      NO-LOCK,
          FIRST acct WHERE
                acct.acct     =  loan-acct.acct
            AND acct.currency =  loan.currency
            AND acct.bal-acct =  iAcct
            /*�஢��塞 �� ��ࠡ��뢠���� �� ��� ���� ࠭��*/
            AND NOT CAN-FIND(FIRST ttAcct WHERE
                                   ttAcct.acct =  acct.acct 
                             NO-LOCK)
      NO-LOCK BY loan-acct.since:
         /*���������� ����*/
         CREATE ttAcct.
         ttAcct.acct = acct.acct.
         /*���� ������ !*/
         oIsAvailability = YES.
         /*����塞 ���⮪ �� ����*/      
         RUN Acct-Pos-Pure IN h_base (acct.acct, 
                                 loan.currency, 
                                 iDate, 
                                 iDate, 
                                 CHR(251)).
         /*��⮢ ����� ���� ��᪮�쪮, ���⮬� �㬬��㥬 १����(� �㡫��)*/
         oBalance  = oBalance + IF loan.currency =  "" THEN sh-bal
                                                       ELSE CurToBase("����",loan.currency,iDate,sh-val).
      END.  
END PROCEDURE.

/* ���ଠ�� ��� ��� ��������� �� �������� ���� �������� ���ᯥ祭��.
   ����� ��������� � tt_getinfdogob.def */
PROCEDURE GetInfDogOb:

   DEFINE INPUT PARAMETER iContract AS  CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iContCode AS  CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDate     AS  DATE NO-UNDO.
   DEFINE INPUT PARAMETER TABLE     FOR tt-term-obl-gar BIND.

   DEFINE VARIABLE vSurr AS CHARACTER NO-UNDO.

   FOR EACH term-obl WHERE term-obl.contract  =  iContract
      AND term-obl.cont-code =  iContCode
      AND term-obl.idnt      =  5
      AND term-obl.end-date  >= iDate
      AND term-obl.fop-date  <= iDate
      NO-LOCK:

      vSurr = term-obl.contract + "," + term-obl.cont-code + "," + STRING(term-obl.idnt) + "," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn).

      CREATE tt-term-obl-gar.
      ASSIGN
         tt-term-obl-gar.mContract       = term-obl.contract
         tt-term-obl-gar.mCont-code      = term-obl.cont-code
         tt-term-obl-gar.mNomdogob       = GetXAttrValue("term-obl",
                                                      vSurr,
                                                      "��������")
         tt-term-obl-gar.mViddogob       = GetXAttrValue("term-obl",
                                                      vSurr,
                                                      "��������")
         tt-term-obl-gar.mVidob          = GetXAttrValue("term-obl",
                                                      vSurr,
                                                      "�����")
         tt-term-obl-gar.mKawcestvoobesp = Get_QualityGar("term-obl",
                                                       vSurr,
                                                       iDate)
         .
   END.
                                 
END PROCEDURE.

{pfuncdef 
&DefProc="GetTTSpis"}

PROCEDURE GetTTSpis:
   DEFINE OUTPUT PARAMETER TABLE FOR ttSpis.
END PROCEDURE.

{pfuncdef 
&DefProc="Soot"
&Description="���⭮襭�� �� � ���"}

PROCEDURE Soot:

   DEFINE INPUT  PARAM iContract   AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iContCode   AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAM oSummOd     AS DEC  NO-UNDO.
   DEFINE OUTPUT PARAM oSSZ        AS DEC  NO-UNDO.
   DEFINE OUTPUT PARAM oSoot       AS DEC  NO-UNDO.

   DEFINE VARIABLE vParam0db       AS DEC  NO-UNDO.
   DEFINE VARIABLE vParam0cr       AS DEC  NO-UNDO.
   DEFINE VARIABLE vAcct           AS CHAR NO-UNDO.
   DEFINE VARIABLE vSummObStr      AS DEC  NO-UNDO.
   DEFINE VARIABLE vSumStr         AS CHAR NO-UNDO.
   DEFINE VARIABLE vSurrTObl       AS CHAR NO-UNDO.
   
   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER insurance FOR loan.
   DEFINE BUFFER loan-int  FOR loan-int.
   DEFINE BUFFER term-obl  FOR term-obl.
   DEFINE BUFFER signs     FOR signs.

   ASSIGN
      oSummOd    = 0
      oSSZ       = 0
      vSummObStr = 0
   .

   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF AVAIL loan THEN
   DO:
      
      FIND FIRST loan-int WHERE 
                 loan-int.contract  =  loan.contract
             AND loan-int.cont-code =  loan.cont-code 
             AND loan-int.id-d      =  0
             AND loan-int.id-k      =  3 
      NO-LOCK NO-ERROR.

      IF AVAIL loan-int THEN
      DO:
         
         vAcct   = ENTRY(1, LN_GetAcctSurr (loan.Class-Code,
                                            loan.contract,
                                            loan.cont-code,
                                            loan.open-date)).

         oSSz    = LnGetProvAcctSpr(vAcct,
                                    loan-int.mdate,
                                    YES). 

         RUN STNDRT_PARAM IN h_loan (loan.contract,
                                     loan.cont-code,
                                     0,
                                     loan-int.mdate,
                                     OUTPUT oSummOd,
                                     OUTPUT vParam0db,
                                     OUTPUT vParam0cr).

         T-OBL:
         FOR EACH term-obl WHERE 
                  (    term-obl.contract   =  loan.contract
                              AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                              AND term-obl.idnt       =  5
                              AND term-obl.end-date   >  loan-int.mdate 
                              AND term-obl.fop-date   <= loan-int.mdate )
                          OR
                             (    term-obl.contract   =  loan.contract
                              AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                              AND term-obl.idnt       =  5
                              AND term-obl.end-date   =  ? 
                              AND term-obl.fop-date   <= loan-int.mdate )
         NO-LOCK:
            
            vSurrTObl = GetSurrogateBuffer("term-obl",(BUFFER term-obl:HANDLE)).
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "��������",
                               "") <> "�।��" THEN NEXT T-OBL.
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "��-�㬑��",
                               "���") =  "��" THEN
            DO:
               vSumStr = GetXattrValueEx("term-obl", 
                                         vSurrTObl,
                                         "SumStr",
                                         ""). 
                
               IF {assigned vSumStr} THEN
                  vSummObStr = vSummObStr + DEC(vSumStr).
               ELSE
               DO:
                  vSumStr = "0".
                  
                  FOR EACH insurance WHERE
                           insurance.contract         =  "�����"
                       AND insurance.parent-contract  =  term-obl.contract
                       AND insurance.parent-cont-code =  term-obl.cont-code
                  NO-LOCK,
                     FIRST signs WHERE
                           signs.file-name   =  "loan"
                       AND signs.surrogate   =  insurance.contract + "," + 
                                                insurance.cont-code
                       AND signs.code        =  "���审��"
                       AND signs.xattr-value =  vSurrTObl
                  NO-LOCK:
                     UpdateSignsEx(term-obl.class-code,
                                   vSurrTObl,
                                   "SumStr",
                                   STRING(term-obl.amt-rub)) NO-ERROR.
                     vSumStr = STRING(term-obl.amt-rub).
                  END.
                  
                  vSummObStr = vSummObStr + DEC(vSumStr). 
               END.
            END. 
         END.
      END.
   END.

   oSoot = (oSummOd - vSummObStr) / oSSZ.
   
   IF oSoot <  0 THEN oSoot = 0.

END PROCEDURE.

{pfuncdef 
&DefProc="SummStrah"
&Description="�㬬� ���客����"}

PROCEDURE SummStrah:

   DEFINE INPUT  PARAM iContract   AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iContCode   AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAM oSummStrah  AS DEC  NO-UNDO.

   DEFINE VARIABLE vVidOb          AS CHAR NO-UNDO.
   DEFINE VARIABLE vSurrTObl       AS CHAR NO-UNDO.
   DEFINE VARIABLE vSumStr         AS CHAR NO-UNDO.
   
   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER insurance FOR loan.
   DEFINE BUFFER loan-int  FOR loan-int.
   DEFINE BUFFER term-obl  FOR term-obl.
   DEFINE BUFFER signs     FOR signs.

   oSummStrah = 0.

   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF AVAIL loan THEN
   DO:

      FIND FIRST loan-int WHERE 
                 loan-int.contract  =  loan.contract
             AND loan-int.cont-code =  loan.cont-code 
             AND loan-int.id-d      =  0
             AND loan-int.id-k      =  3 
      NO-LOCK NO-ERROR.

      IF AVAIL loan-int THEN
      DO:

         T-OBL:
         FOR EACH term-obl WHERE 
                  (    term-obl.contract   =  loan.contract
                   AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                   AND term-obl.idnt       =  5
                   AND term-obl.end-date   >  loan-int.mdate 
                   AND term-obl.fop-date   <= loan-int.mdate )
               OR (    term-obl.contract   =  loan.contract
                   AND term-obl.cont-code  =  ENTRY(1,loan.cont-code," ")
                   AND term-obl.idnt       =  5
                   AND term-obl.end-date   =  ? 
                   AND term-obl.fop-date   <= loan-int.mdate )
         NO-LOCK:
            
            vSurrTObl = GetSurrogateBuffer("term-obl",(BUFFER term-obl:HANDLE)).
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "��������",
                               "") <> "�।��" THEN NEXT T-OBL.
            
            IF GetXAttrValueEx("term-obl",
                               vSurrTObl, 
                               "��-�㬑��",
                               "���") =  "��" THEN
            DO:
               vSumStr = GetXattrValueEx("term-obl", 
                                         vSurrTObl,
                                         "SumStr",
                                         ""). 
                
               IF {assigned vSumStr} THEN
                  oSummStrah = oSummStrah + DEC(vSumStr).
               ELSE
               DO:
                  vSumStr = "0".
                  
                  FOR EACH insurance WHERE
                           insurance.contract         =  "�����"
                       AND insurance.parent-contract  =  term-obl.contract
                       AND insurance.parent-cont-code =  term-obl.cont-code
                  NO-LOCK,
                     FIRST signs WHERE
                           signs.file-name   =  "loan"
                       AND signs.surrogate   =  insurance.contract + "," + 
                                                insurance.cont-code
                       AND signs.code        =  "���审��"
                       AND signs.xattr-value =  vSurrTObl
                  NO-LOCK:
                     UpdateSignsEx(term-obl.class-code,
                                   vSurrTObl,
                                   "SumStr",
                                   STRING(term-obl.amt-rub)) NO-ERROR.
                     vSumStr = STRING(term-obl.amt-rub).
                  END.
                  
                  oSummStrah = oSummStrah + DEC(vSumStr).
               END.
            END.
         END.
      END.
   END.
END PROCEDURE.

/* ============================================================================== */
/* == �����頥� ��ண�� ������� � ��諮� �����, �᫨ ᬥ�� ���뫮 � ���� == */
/* ============================================================================== */
{pfuncdef 
   &DefProc="ChangeCurrency"
   &Description="��ண�� �������, �� ᬥ�� ������"}
FUNCTION ChangeCurrency RETURNS CHAR
   (iLoanSurr AS CHAR):   
   
   DEF VAR vResult AS CHAR NO-UNDO.
   
   vResult = GetXattrValueEx("loan", 
                             iLoanSurr,
                             "���������",
                             "").
                             
   IF {assigned vResult} THEN
      vResult = "�।��," + vResult. 
   
   RETURN vResult.

END FUNCTION.
{pfuncdef 
&DefProc="GetSummGarInRes"
&Description="����祭�� ����稭� �� ������� �ਭ�⮣� ���ᯥ祭��, ~
   �� ����� 㬥��蠥��� ����稭� ���⭮�� १�ࢠ ~
   �� �������� �� १�ࢨ஢����"}
PROCEDURE GetSummGarInRes:
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iSince    AS DATE   NO-UNDO. /* ���, �� ������ ᮡ������ ����� */
   DEFINE OUTPUT PARAMETER TABLE FOR ttGarRes.
   
   DEF VAR vRsrvRate  AS DEC   NO-UNDO.
   DEF VAR vGrRiska   AS INT64 NO-UNDO.
   DEF VAR vTermSur   AS CHAR  NO-UNDO.
   DEF VAR vKatKach   AS CHAR  NO-UNDO.
   DEF VAR vIndKatKac AS INT64 NO-UNDO.
   DEF VAR vOstSotim  AS DEC   NO-UNDO.
   DEF VAR vListOp    AS CHAR  NO-UNDO INIT "33,32,137,136,471,470,474,473,320,321,426,427,339,340".
   DEF VAR vPrevDate  AS DATE  NO-UNDO.
   DEF VAR vI         AS INT64 NO-UNDO.
   
   DEF BUFFER loan     FOR loan.
   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER chowhe   FOR chowhe.
   DEF BUFFER loan-int FOR loan-int.
   
   {empty ttGarRes}
   
   FIND FIRST loan WHERE
              loan.contract  =  iContract
          AND loan.cont-code =  iContCode
   NO-LOCK NO-ERROR.
   
   IF NOT AVAIL loan THEN
      RETURN.
   
      /* �᫨ ��� ���� �� ��।���, � ��� �� ��������� ���� */
   IF iSince =  ? THEN
      iSince = gend-date.
   
   vPrevDate = iSince.
    
      /* ��室�� ᠬ�� ��᫥���� ������, �� ᯨ᪠ vListOp �� ������� */
   DO vI = 1 TO NUM-ENTRIES(vListOp):
      FOR FIRST chowhe WHERE
                chowhe.id-op =  INT64(ENTRY(vI,vListOp))
      NO-LOCK:
         FOR LAST loan-int WHERE
                  loan-int.contract  =  loan.contract 
              AND loan-int.cont-code =  loan.cont-code
              AND loan-int.id-k      =  chowhe.id-k
              AND loan-int.id-d      =  chowhe.id-d
              AND loan-int.op-date   <= vPrevDate
         NO-LOCK by loan-int.id-k:
               /* �᫨ ���� ��� �।��饣� �ॣ㫨஢���� १�ࢠ, � ���襬 �� ��� */ 
            IF    vPrevDate >  loan-int.op-date
               OR vPrevDate =  ? THEN
               vPrevDate = loan-int.op-date.
         END.
             
         FOR LAST loan-int WHERE
                  loan-int.contract  =  loan.contract 
              AND loan-int.cont-code =  loan.cont-code
              AND loan-int.id-d      =  chowhe.id-d
              AND loan-int.id-k      =  chowhe.id-k
              AND loan-int.op-date   <= vPrevDate
         NO-LOCK by loan-int.id-d :
               /* �᫨ ���� ��� �।��饣� �ॣ㫨஢���� १�ࢠ, � ���襬 �� ��� */ 
            IF    vPrevDate >  loan-int.op-date
               OR vPrevDate =  ? THEN
               vPrevDate = loan-int.op-date.
         END.
      END.
   END.
       
      /* �᫨ ࠭�� ���� �� ॣ㫨஢���, � ���� ���� �।��饣� ����� */
   IF vPrevDate =  ? THEN
   DO: 
      vPrevDate = LastMonDate(GoMonth(iSince,-1)).   
           
         /* �᫨ �।���� ��� �����, ����� ���� ������ ������� */
      IF vPrevDate <= loan.open-date THEN
         vPrevDate = loan.open-date. /* � ���� ���� ������ ������� */
   END.
   
      /* ����. १�ࢨ஢���� */
   RUN GetRateGrRisk IN h_i254 (loan.contract,
                                loan.cont-code,
                                vPrevDate,
                                OUTPUT vRsrvRate,
                                OUTPUT vGrRiska ).
      /* ����� �� ���ᯥ祭�� */
   FOR EACH term-obl WHERE 
            term-obl.contract  =  loan.contract
        AND term-obl.cont-code =  loan.cont-code
        AND term-obl.idnt      =  5
        AND term-obl.fop-date  <= vPrevDate
        AND (   term-obl.sop-date  =  ?
             OR term-obl.sop-date  >  vPrevDate )
   NO-LOCK:

      vTermSur = STRING(term-obl.contract + "," + 
                        term-obl.cont-code + ",5," + 
                        STRING(term-obl.end-date) + "," + 
                        STRING(term-obl.nn)).
                        
         /* ��室�� ��⥣��� ����⢠ */               
      vKatKach = Get_QualityGar ("comm-rate",vTermSur, vPrevDate).

      IF    vKatKach =  "?"
         OR vKatKach =  ?
      THEN
         vKatKach = "".

      IF vKatKach =  "" THEN
         vIndKatKac = 0 .
      ELSE
         vIndKatKac = INT64(GetCode("����⢮����",vKatKach)).

          /* ��室�� ��業� ����筮� �⮨���� */
      vOstSotim = GET_COMM("����",
                           ?,
                           term-obl.currency,
                           vTermSur,
                           0.00,
                           0,
                           vPrevDate).
      IF vOstSotim = ? THEN vOstSotim = 100.

      CREATE ttGarRes.
      ASSIGN
         ttGarRes.fSurr = vTermSur
         ttGarRes.fNumOb  = GetXAttrValueEx("term-obl",
                                            vTermSur,
                                            "��������",
                                            "")
         ttGarRes.fVidOb   = GetXAttrValueEx("term-obl",
                                             vTermSur,
                                             "��������",
                                             "")
         ttGarRes.fSumm   = LnPledge(term-obl.contract,
                                     term-obl.cont-code,
                                     term-obl.idnt,
                                     term-obl.end-date,
                                     term-obl.nn,
                                     vPrevDate,
                                     ?,
                                     term-obl.currency ) * vIndKatKac / 100 *
                            vOstSotim / 100 * vRsrvRate / 100
         ttGarRes.fCurrency = term-obl.currency
         ttGarRes.fLastDate = vPrevDate
      .
      
      IF {assigned ttGarRes.fCurrency} THEN
         ttGarRes.fSummRub = CurToBase("�������", ttGarRes.fCurrency, vPrevDate, ttGarRes.fSumm).
      ELSE
         ttGarRes.fSummRub = ttGarRes.fSumm.
   END.                             
      
END PROCEDURE.

{pfuncdef 
&DefProc="GetLongestDelayForClient"
&Description="����砥� ���������� ������ �� �������, ��ࠬ��� ��� ������, �����䨪��� ������, ��� �����"}
FUNCTION GetLongestDelayForClient RETURNS INT64
  (iCust-cat  AS CHAR,  /* ��� ������ */
   iCust-id   AS INT64,  /* �����䨪��� ������ */
   iDate      AS DATE)  /* ��� ���� ���� */
:
   DEF VAR vLonger     AS INT64 NO-UNDO.
   DEF VAR vResDate    AS DATE  NO-UNDO.
   DEF VAR vProsrFl    AS LOG   NO-UNDO.

   DEF BUFFER loan FOR loan.
   
   vLonger = 0.
   
   FOR EACH loan WHERE
           (loan.cust-cat   =  iCust-cat
        AND loan.cust-id    =  iCust-id
        AND loan.contract   =  "�।��"
        AND loan.close-date >  iDate)
        OR 
           (loan.cust-cat   =  iCust-cat
        AND loan.cust-id    =  iCust-id
        AND loan.contract   =  "�।��"
        AND loan.close-date =  ?)
   NO-LOCK:
       
      RUN LN_GetPrsDate IN h_cdrep (loan.contract,
                                    ENTRY (1, loan.cont-code, " "),
                                    iDate,
                                    mProcPar139,
                                    NO /* ������ ����� - �ᥣ�� �� ���� */, 
                                    YES /* ����騩 ���� �ᥣ�� ���뢠�� */,
                                    OUTPUT vResDate,
                                    OUTPUT vProsrFl).
      vLonger = MAX (vLonger , (iDate - vResDate)).
   END.
   
   RETURN vLonger.

END FUNCTION.

{pfuncdef 
   &DefProc="IsLoanPlavStav"
   &Description="��।����, ���� �� ��������� �� ���� �������� �⠢�� �� �������"}
PROCEDURE IsLoanPlavStav: 
   
   DEF INPUT  PARAM iSurr         AS CHAR NO-UNDO.
   DEF INPUT  PARAM iBegDate      AS DATE NO-UNDO.
   DEF INPUT  PARAM iEndDate      AS DATE NO-UNDO.
   DEF OUTPUT PARAM oDatePlavStav AS DATE NO-UNDO.
   DEF OUTPUT PARAM oMessError    AS CHAR NO-UNDO INIT "".
   
   DEF BUFFER loan      FOR loan.
   DEF BUFFER comm-cond FOR comm-cond.
   
   DEF VAR vContract AS CHAR NO-UNDO.
   DEF VAR vContCode AS CHAR NO-UNDO.
   DEF VAR vBegDate  AS DATE NO-UNDO.
   DEF VAR vEndDate  AS DATE NO-UNDO.

   MAIN-BLOCK:
      DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
         
      IF NUM-ENTRIES(iSurr) <  2 THEN
      DO:
         oMessError = SUBSTITUTE("&1. �訡�� � ���ண�� �������", iSurr). 
         LEAVE MAIN-BLOCK. 
      END.
      
      /* �᫨ ������騥 �⠢�� ������� �� �墠�, � �饬 �墠�뢠�騩 ������� */
      ASSIGN
         vContract = ENTRY(1,iSurr)
         vContCode = ENTRY(2,iSurr)
         vContCode = IF FGetSetting("�����⠢�墠�",?,"���") =  "��" 
                     THEN ENTRY(1,vContCode," ")
                     ELSE vContCode 
      . 
         
      /* ������ ������� */
      FIND FIRST loan WHERE 
                 loan.contract  =  vContract
             AND loan.cont-code =  vContCode  
      NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN 
      DO:
         oMessError = SUBSTITUTE("&1. �� ������ �������", iSurr).
         LEAVE MAIN-BLOCK.          
      END.
      
      ASSIGN 
         vBegDate = IF iBegDate =  ? THEN loan.open-date ELSE iBegDate
         vEndDate = IF iEndDate =  ? THEN loan.end-date  ELSE iEndDate
      . 
      
      RUN GetLastDateFloatRateInPer IN THIS-PROCEDURE (
         BUFFER loan, 
         vBegDate, 
         vEndDate,  
         OUTPUT oDatePlavStav).
         
   END. /* MAIN-BLOCK */
   
END PROCEDURE.


/* ����砥� ��᫥���� �������� ���� ��������� ������饩 �⠢�� � 㪠������ ��ਮ�� */
PROCEDURE GetLastDateFloatRateInPer PRIVATE:
   
   DEF PARAM BUFFER loan      FOR loan.
   
   DEF INPUT PARAM iBegDate AS DATE NO-UNDO.
   DEF INPUT PARAM iEndDate AS DATE NO-UNDO.
   
   DEF OUTPUT PARAM oLastDate AS DATE NO-UNDO INIT ?.
   
   DEF VAR vDate AS DATE NO-UNDO.
   
   DEF BUFFER comm-cond FOR comm-cond.
   
   MAIN-BLOCK:
   DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      FIND LAST comm-cond WHERE
                comm-cond.contract   =  loan.contract
            AND comm-cond.cont-code  =  loan.cont-code
            AND comm-cond.commission =  "%�।"
            AND comm-cond.since      <= iEndDate
      NO-LOCK NO-ERROR.
      
      /* �᫨ ��᫥���� �⠢�� �� ��������, � ��室�� */
      IF NOT AVAIL comm-cond OR 
        (AVAIL comm-cond 
         AND NOT comm-cond.FloatType) THEN 
         LEAVE MAIN-BLOCK.
      
      /* ����砥� ����� �������� ���� ��������� �⠢�� � ��砫� ��ਮ�� iBegDate */
      RUN GetFirstDateFloatRate IN THIS-PROCEDURE (
         BUFFER loan,
         BUFFER comm-cond, 
         iBegDate, 
         OUTPUT vDate).
         
      /* �᫨ ����祭��� ��� ����� ���� ��ਮ��, � � ��ਮ�� �⠢�� 
         �� ���﫠�� - ��室�� */
      IF vDate = ? 
      OR (vDate <> ? 
      AND vDate > iEndDate) THEN 
         LEAVE MAIN-BLOCK.
      
      CL:
      DO WHILE (vDate <> ?) AND (vDate < iEndDate):
         
         oLastDate = vDate.
         
         /* �᫨ ���⨣�� ����砭�� �������, � ��室�� */   
         IF vDate > loan.end-date THEN LEAVE CL.

         /* ����砥� ᫥������ �������� ���� ��������� �⠢�� */
         RUN GetFirstDateFloatRate IN THIS-PROCEDURE (
            BUFFER loan,
            BUFFER comm-cond, 
            vDate + 1, /* ���騢��� ���� */
            OUTPUT vDate).
      END.  
         
   END. /* MAIN-BLOCK */
   
END PROCEDURE.

   /* ������ ����� १�� �� �࠭襢�� �������� � �ந������ ���४�� ���㣫���� */
{pfuncdef 
   &DefProc="GetResRasch"
   &Description="������ ����� १�� �� �࠭襢�� �������� � �ந������ ���४�� ���㣫����"}
PROCEDURE GetResRasch:
   
   DEF PARAM BUFFER loan FOR loan.
   DEF INPUT PARAM  iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oSum  AS DEC  NO-UNDO.
   
   DEF VAR vRes      AS DEC   NO-UNDO.
   DEF VAR vDif      AS DEC   NO-UNDO.
   DEF VAR vSum      AS DEC   NO-UNDO.
   DEF VAR vMax      AS DEC   NO-UNDO.
   DEF VAR vContCode AS CHAR  NO-UNDO.
   DEF VAR vRec      AS RECID NO-UNDO.
   DEF VAR vTmpDec   AS DEC   NO-UNDO EXTENT 3.
   
   DEF BUFFER bloan FOR loan.
   
   MB:
   DO ON ERROR UNDO MB, LEAVE MB:
         /* �饬 㦥 ��ନ஢���� १���� */
      FIND FIRST tt-ResRasch WHERE 
                 tt-ResRasch.cont-code =  loan.cont-code
             AND tt-ResRasch.end-date  =  iDate
      NO-LOCK NO-ERROR.
         /* �᫨ �� �������, � ������塞 ⠡���� */
      IF NOT AVAIL(tt-ResRasch) THEN
      DO:
         {empty tt-ResRasch}
         vContCode = ENTRY(1, loan.cont-code, " ").
         vDif = LnFormRsrv (loan.contract,        
                            vContCode,
                            iDate,
                            "").
         CREATE tt-ResRasch.
         ASSIGN
            tt-ResRasch.cont-code = vContCode
            tt-ResRasch.end-date  = iDate
            tt-ResRasch.summa     = vDif
         .
         FOR EACH bloan WHERE bloan.contract  =  loan.contract 
                          AND bloan.cont-code BEGINS vContCode + " "
                          AND NUM-ENTRIES(bloan.cont-code, " ") >  1
         NO-LOCK:
            vRes = LnFormRsrvTransh (bloan.contract,        
                                     bloan.cont-code,
                                     iDate,
                                     "").
            vRes = ROUND(vRes, 2).
            CREATE tt-ResRasch.
            ASSIGN
               tt-ResRasch.cont-code = bloan.cont-code
               tt-ResRasch.end-date  = iDate
               tt-ResRasch.summa     = vRes
            .
            IF vDif <> 0 THEN
               vDif = vDif - vRes.
               /* ���������� �࠭� � ᠬ�� ����让 �㬬�� */
            RUN RE_PARAM_EX IN h_Loan ("0",
                                       iDate,
                                       bloan.since,
                                       bloan.contract,
                                       bloan.cont-code,
                                       OUTPUT vTmpDec[1], /* ���祭�� */
                                       OUTPUT vTmpDec[2],
                                       OUTPUT vTmpDec[3]).
            vSum = vTmpDec[1].
            IF vSum >  vMax THEN
               ASSIGN
                  vRec = RECID(tt-ResRasch)
                  vMax = vSum
               .
         END.
            /* ���४�� �訡�� ���㣫���� */
         IF vDif <> 0 THEN
         DO:
            FIND FIRST tt-ResRasch WHERE RECID(tt-ResRasch) =  vRec NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-ResRasch THEN /* �����-� 䨣�� ��稫��� */
               LEAVE MB.
            tt-ResRasch.summa = tt-ResRasch.summa + vDif.
         END.
      END. /* IF NOT AVAIL(tt-ResRasch) THEN */
         /* �饬 㦥 ��ନ஢���� १���� */
      FIND FIRST tt-ResRasch WHERE 
                 tt-ResRasch.cont-code =  loan.cont-code
             AND tt-ResRasch.end-date  =  iDate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(tt-ResRasch) THEN
         LEAVE MB.
      oSum = tt-ResRasch.summa.
   END. /* MB */
END PROCEDURE.


 /* ������ ���⭮� ���ᯥ祭�� �� �࠭襢�� �������� � �ந������ ���४�� ���㣫���� */
{pfuncdef 
   &DefProc="GetGarRasch"
   &Description="������ ���⭮� ����.�� �࠭襢�� �������� � �ந������ ���४�.���㣫."}
PROCEDURE GetGarRasch:
   
   DEF PARAM BUFFER loan FOR loan.
   DEF INPUT PARAM  iDate   AS DATE NO-UNDO.
   DEF INPUT PARAM  iKK     AS CHAR NO-UNDO.
   DEF INPUT PARAM  iKndAmt AS LOG NO-UNDO.
   DEF OUTPUT PARAM oSum    AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oSumQ   AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oVid    AS CHAR NO-UNDO.
   
   DEF VAR vDif      AS DEC   NO-UNDO.
   DEF VAR vSum      AS DEC   NO-UNDO.
   DEF VAR vMax      AS DEC   NO-UNDO.
   DEF VAR vContCode AS CHAR  NO-UNDO.
   DEF VAR vRec      AS RECID NO-UNDO.
   DEF VAR vTmpDec   AS DEC   NO-UNDO EXTENT 3.
   DEF VAR vAmtAll   AS DEC   NO-UNDO.
   DEF VAR vGarAll   AS DEC   NO-UNDO.
   DEF VAR vKK       AS CHAR  NO-UNDO.
   DEF VAR vVidOb    AS CHAR  NO-UNDO.
   DEF VAR vVidAll   AS CHAR  NO-UNDO.
   DEF VAR vAmt      AS DEC   NO-UNDO.
   DEF VAR vSurr     AS CHAR  NO-UNDO.
   DEF VAR vQSum     AS DEC   NO-UNDO.
   DEF VAR vGQIndex  AS DEC   NO-UNDO.
   DEF VAR vDecrRate AS DEC   NO-UNDO.
   DEF VAR vGarAllQ  AS DEC   NO-UNDO.
   
   DEF BUFFER bloan    FOR loan.
   DEF BUFFER term-obl FOR term-obl.
  
   MB:
   DO ON ERROR UNDO MB, LEAVE MB:
         /* �饬 㦥 ��ନ஢���� १���� */
      FIND FIRST tt-GarRasch WHERE 
                 tt-GarRasch.cont-code =  loan.cont-code
             AND tt-GarRasch.end-date  =  iDate
             AND tt-GarRasch.KK        =  iKK
      NO-LOCK NO-ERROR.
         /* �᫨ �� �������, � ������塞 ⠡���� */
      IF NOT AVAIL(tt-GarRasch) THEN
      DO:
         {empty tt-GarRasch}
         ASSIGN 
            vContCode = ENTRY(1, loan.cont-code, " ").
            vGQIndex  = DEC(GetCode("����⢮����", iKK ))
         .
         IF vGQIndex  = ? THEN vGQIndex  = 0.

         /* ���� �㬬� ���ᯥ祭�� */
         lGar:
         FOR EACH term-obl WHERE
                  term-obl.contract  = loan.contract
              AND term-obl.cont-code BEGINS vContCode + " "
              AND NUM-ENTRIES(term-obl.cont-code, " ") >  1
              AND term-obl.idnt      = 5
              AND (term-obl.sop-date >  iDate
                OR term-obl.sop-date =  ?) 
         NO-LOCK:
            vSurr = GetSurrogate("term-obl",ROWID(term-obl)).
            vKK = Get_QualityGar ("term-obl",
                                  vSurr,
                                  iDate).
            IF vKK <> iKK THEN 
               NEXT lGar.
            vDecrRate = GET_COMM("����",
                              ?,
                              term-obl.currency,
                              vSurr,
                              0.00,
                              0,
                              iDate).
            IF vDecrRate = ? THEN vDecrRate = 100.


            vVidOb = Get_VidObespech (vSurr,
                                      iDate).
            {additem.i vVidAll vVidOb}
            
            IF iKndAmt THEN
            DO:
               vAmt = DEC(GetTempXAttrValueEx("term-obl",
                                              vSurr,
                                              "�뭑⮨�",
                                              iDate,
                                              "0")) NO-ERROR.
                                           
               IF term-obl.currency <> "" THEN
                  vAmt = CurToBase("�������",
                                   term-obl.currency,
                                   iDate,
                                   vAmt).
            END.
            ELSE
               vAmt  = LnPledge (term-obl.contract,        
                                 term-obl.cont-code,
                                 term-obl.idnt,
                                 term-obl.end-date,
                                 term-obl.nn,
                                 iDate,
                                 iDate,
                                 "").
            ASSIGN 
               vDif  = vDif + vAmt
               vQSum = vQSum + ROUND(vAmt * vGQIndex / 100 * vDecrRate / 100,2)
            .
         END.
         lGar2:
         FOR EACH term-obl WHERE                  
                  term-obl.contract  = loan.contract
              AND term-obl.cont-code = vContCode
              AND term-obl.idnt      = 5
             AND (term-obl.sop-date >  iDate
               OR term-obl.sop-date =  ?) 
         NO-LOCK:
            vSurr = GetSurrogate("term-obl",ROWID(term-obl)).
            vKK = Get_QualityGar ("term-obl",
                                  vSurr,
                                  iDate).
            IF vKK <> iKK THEN 
               NEXT lGar2.

            vVidOb = Get_VidObespech (vSurr,
                                      iDate).
            vDecrRate = GET_COMM("����",
                              ?,
                              term-obl.currency,
                              vSurr,
                              0.00,
                              0,
                              iDate).
            IF vDecrRate = ? THEN vDecrRate = 100.

            {additem.i vVidAll vVidOb}
            
            IF iKndAmt THEN
            DO:
               vAmt = DEC(GetTempXAttrValueEx("term-obl",
                                              vSurr,
                                              "�뭑⮨�",
                                              iDate,
                                              "0")) NO-ERROR.
                                           
               IF term-obl.currency <> "" THEN
                  vAmt = CurToBase("�������",
                                   term-obl.currency,
                                   iDate,
                                   vAmt).
            END.
            ELSE
               vAmt =  LnPledge (term-obl.contract,        
                                 term-obl.cont-code,
                                 term-obl.idnt,
                                 term-obl.end-date,
                                 term-obl.nn,
                                 iDate,
                                 iDate,
                                 "").
            ASSIGN 
               vDif  = vDif + vAmt
               vQSum = vQSum + ROUND(vAmt * vGQIndex / 100 * vDecrRate / 100,2)
            .
         END.

         ASSIGN
           vAmtAll = LnPrincipal(loan.contract,        
                                 vContCode,
                                 iDate,
                                 loan.currency)
           vGarAll = vDif
           vGarAllQ = vQSum    
        .

         CREATE tt-GarRasch.
         ASSIGN
            tt-GarRasch.cont-code = vContCode
            tt-GarRasch.end-date  = iDate
            tt-GarRasch.summa     = vDif
            tt-GarRasch.dolg      = vAmtAll
            tt-GarRasch.vidob     = vVidAll
            tt-GarRasch.KK        = iKK
            tt-GarRasch.Qsumma    = vQSum
            vRec                  = RECID(tt-GarRasch)
         .

         FOR EACH bloan WHERE bloan.contract  =  loan.contract 
                          AND bloan.cont-code BEGINS vContCode + " "
                          AND NUM-ENTRIES(bloan.cont-code, " ") >  1
         NO-LOCK:
               /* ���������� �࠭� � ᠬ�� ����让 �㬬�� */
            RUN RE_PARAM_EX IN h_Loan ("0",
                                       iDate,
                                       bloan.since,
                                       bloan.contract,
                                       bloan.cont-code,
                                       OUTPUT vTmpDec[1], /* ���祭�� */
                                       OUTPUT vTmpDec[2],
                                       OUTPUT vTmpDec[3]).
            vSum = vTmpDec[1].
            IF vSum >  vMax THEN
               ASSIGN
                  vRec = RECID(tt-GarRasch)
                  vMax = vSum
               .
            CREATE tt-GarRasch.
            ASSIGN
               tt-GarRasch.cont-code = bloan.cont-code
               tt-GarRasch.end-date  = iDate
               tt-GarRasch.summa     = ROUND(vTmpDec[1] * vGarAll / vAmtAll,2)
               tt-GarRasch.dolg      = vTmpDec[1]
               tt-GarRasch.vidob     = vVidAll
               tt-GarRasch.KK        = iKK
               tt-GarRasch.Qsumma    = ROUND(vTmpDec[1] * vGarAllQ / vAmtAll,2)
            .
            IF vDif <> 0 THEN
               vDif = vDif - tt-GarRasch.summa.
         END.
         IF NOT CAN-FIND (FIRST bloan WHERE
                                bloan.contract  =  loan.contract 
                            AND bloan.cont-code BEGINS vContCode + " "
                            AND NUM-ENTRIES(bloan.cont-code, " ") >  1) THEN
            vDif = 0.

            /* ���४�� �訡�� ���㣫���� */
         IF vDif <> 0 THEN
         DO:
            FIND FIRST tt-GarRasch WHERE RECID(tt-GarRasch) =  vRec NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-GarRasch THEN /* �����-� 䨣�� ��稫��� */
               LEAVE MB.
            tt-GarRasch.summa = tt-GarRasch.summa + vDif.
         END.

      END. /* IF NOT AVAIL(tt-GarRasch) THEN */
         /* �饬 㦥 ��ନ஢���� १���� */
      FIND FIRST tt-GarRasch WHERE 
                 tt-GarRasch.cont-code =  loan.cont-code
             AND tt-GarRasch.end-date  =  iDate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(tt-GarRasch) THEN
         LEAVE MB.
      ASSIGN
         oSum = tt-GarRasch.summa
         oVid  = tt-GarRasch.vidob
         oSumQ = tt-GarRasch.Qsumma  
      .
   END. /* MB */
END PROCEDURE.

{pfuncdef 
   &DefProc=      "GetCustExInfo"
   &Description=  "�����頥� ��࠭�,����,��� ������"}
PROCEDURE GetCustExInfo:
   DEFINE INPUT PARAMETER iCustCat  AS CHARACTER   NO-UNDO.
   DEFINE INPUT PARAMETER iCustID   AS INT64       NO-UNDO.
   DEFINE OUTPUT PARAMETER oCountry AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oOGRN    AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oINN     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vHQry   AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vHBuff  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vFile   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vField  AS CHARACTER   NO-UNDO.
   
   CASE iCustCat:
      WHEN "�" THEN 
         ASSIGN 
            vFile    = "person"
            vField   = "person-id"
         .
      WHEN "�" THEN 
         ASSIGN 
            vFile    = "cust-corp"
            vField   = "cust-id"
         .
      WHEN "�" THEN 
         ASSIGN 
            vFile    = "banks"
            vField   = "bank-id"
         .
   END CASE.
   
   CREATE QUERY vHQry.
   CREATE BUFFER vHBuff FOR TABLE vFile.
   vHQry:SET-BUFFERS(vHBuff).
   vHQry:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE &2 = &3 NO-LOCK",vFile,vField,iCustID)).
   vHQry:QUERY-OPEN().
   vHQry:GET-FIRST().

   IF NOT vHQry:QUERY-OFF-END THEN 
      ASSIGN 
         oCountry = vHBuff:BUFFER-FIELD("country-id"):BUFFER-VALUE
         oOGRN    = GetXAttrValue(vFile,STRING(iCustID),"����")
         oINN     = vHBuff:BUFFER-FIELD("inn"):BUFFER-VALUE
      .
   
   vHQry:QUERY-CLOSE().
   DELETE OBJECT vHQry.
   
END PROCEDURE. 

{pfuncdef 
   &DefProc=      "GetObremTableByLoan"
   &Description=  "�� ������ �।�⭮�� ������� ��室�� �� �易��� ��६������, 
                   �� ������� ��६������ ����塞 ��室�� ��ࠬ���� (������������ ����ࠣ���,
                   �������樮��� �����,��� ��易⥫��⢠,�����ᮢ�� �⮨�����,�ப ����襭��). 
                   ����� �����頥� � ���� ⠡���� �� �ᥬ ��६������ 
                   �।�⭮�� �������"}
PROCEDURE GetObremTableByLoan:
   DEFINE INPUT PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR ttObrem BIND.
   
   DEFINE BUFFER bloan FOR loan.
   DEFINE BUFFER oloan FOR loan.
   
   DEFINE VARIABLE vClName    AS CHARACTER NO-UNDO EXTENT 3.
   DEFINE VARIABLE vCountry   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOGRN      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vINN       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vVidOb     AS CHARACTER NO-UNDO.
   
   FIND FIRST bloan WHERE bloan.contract    = iContract
                      AND bloan.cont-code   = iContCode
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bloan THEN RETURN.

   
   FOR EACH oloan WHERE oloan.parent-contract   = iContract
                    AND oloan.parent-cont-code  = iContCode
                    AND oloan.contract          = "��६"
                    AND oloan.cont-code         BEGINS iContCode
   NO-LOCK:

      IF oloan.cust-cat <> "�" THEN DO: 
         RUN GetCustName IN h_base (oloan.cust-cat, 
                                    INT64(oloan.cust-id), 
                                    "", 
                             OUTPUT vClName[1], 
                             OUTPUT vClName[2], 
                       INPUT-OUTPUT vClName[3]).

         RUN GetCustExInfo IN THIS-PROCEDURE 
                          (oloan.cust-cat,
                           INT64(oloan.cust-id),
                    OUTPUT vCountry,
                    OUTPUT vOGRN, 
                    OUTPUT vINN).
      END.
      ELSE
         ASSIGN
            vClName[1] = ""
            vClName[2] = ""
            vCountry   = ""
            vOGRN      = ""
            vINN       = ""
         .  
      vVidOb = GetXAttrValueEx("loan",GetSurrogate("loan",ROWID(oloan)),"���������",""). 
      
      CREATE ttObrem.
      ASSIGN 
         ttObrem.CustCat  = oloan.cust-cat
         ttObrem.CustId   = oloan.cust-id
         ttObrem.cname    = IF oloan.cust-cat <> "�"           /* ������������ ����ࠣ��� */
                            THEN vClName[1] + " " + vClName[2]
                            ELSE "1"
         ttObrem.cname    = IF bloan.cont-type = "��祭��" OR NUM-ENTRIES(bloan.cont-code," ") = 2
                            THEN (ttObrem.cname + " (�।�⭠� �����)")
                            ELSE ttObrem.cname      
         ttObrem.rnum     = IF CAN-DO("�,�",oloan.cust-cat)    /* �������樮��� ����� */
                            THEN IF vCountry = "rus" 
                                 THEN vOGRN
                                 ELSE "��"
                            ELSE vINN 
         ttObrem.kind     = IF vVidOb BEGINS "4"               /* ��� ��易⥫��⢠ */
                            THEN (vVidOb + " (" + GetCodeName("���������",vVidOb) + ")")
                            ELSE vVidOb 
         ttObrem.amt      = oloan.interest[1]                  /* �����ᮢ�� �⮨����� */
         ttObrem.open-date = oloan.open-date                   /* ��� ��砫� ��६������ */
         ttObrem.end-date = oloan.end-date                     /* �ப ����襭�� */
         ttObrem.currency = oloan.currency                     /* ����� */
      .
   END.                    
   IF NOT CAN-FIND(FIRST ttObrem) THEN DO:
      CREATE ttObrem.
      ttObrem.cname = "0".
   END.
END PROCEDURE.

{pfuncdef 
   &DefProc=      "GetDateIssuance"
   &Description=  "�����頥� ���� �뤠� �।��"}

FUNCTION GetDateIssuance RETURNS DATE (
   BUFFER vLoan FOR loan
):

   DEFINE VARIABLE vDate      AS DATE NO-UNDO.
   DEFINE VARIABLE vVydDate   AS DATE NO-UNDO.
   
   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER loan-int  FOR loan-int. 

   vDate = {&BQ-MAX-DATE}.
   IF vLoan.cont-type =  "��祭��" 
   THEN /* �ਧ��� �।�⭮� ����� */
      LOOPLOAN:
      FOR EACH loan WHERE 
               loan.contract  =  vLoan.contract
           AND loan.cont-code BEGINS vLoan.cont-code + " "
           AND NUM-ENTRIES(loan.cont-code, " ") >  1
      NO-LOCK:
         vVydDate = DATE(GetXAttrValue("loan",
                                       loan.contract + "," + loan.cont-code,
                                       "��⠂뤑���")
                        ).
         IF vVydDate <> ? THEN
            vDate = MIN (vVydDate, vDate).
         ELSE
            LOOPLI:
            FOR EACH loan-int WHERE 
                     loan-int.contract  =  loan.contract
                 AND loan-int.cont-code =  loan.cont-code
                 AND loan-int.id-d      =  0
                 AND loan-int.id-k      =  3
               NO-LOCK BREAK BY loan-int.mdate:
               IF FIRST(loan-int.mdate) THEN
               DO:
                  vDate = MIN (loan-int.mdate, vDate).
                  LEAVE LOOPLI.
               END.
            END. /* LOOPLI */
      END. /* LOOPLOAN */
   /* END IF */
   
   vVydDate = DATE(GetXAttrValue("loan",
                                 vLoan.contract + "," + vLoan.cont-code,
                                 "��⠂뤑���")).
   IF vVydDate <> ? THEN
      vDate = MIN (vVydDate, vDate).
   ELSE
      LOOPLI:
      FOR EACH loan-int WHERE 
               loan-int.contract  =  vLoan.contract
           AND loan-int.cont-code =  vLoan.cont-code
           AND loan-int.id-d      =  0
           AND loan-int.id-k      =  3
         NO-LOCK BREAK BY loan-int.mdate:
         IF FIRST(loan-int.mdate) THEN
         DO:
            vDate = MIN (loan-int.mdate, vDate).
            LEAVE LOOPLI.
         END.
      END. /* LOOPLI */

   IF vDate =  {&BQ-MAX-DATE} THEN vDate = ?.

   RETURN vDate.
END FUNCTION.
{pfuncdef 
   &DefProc=      "GetObremCBRFAttr"
   &Description=  "�� ������ �।�⭮�� ������� ��室�� �� �易��� ��६������, 
                   ��室��� ��ࠬ��� 1 '�ਧ��� ��६������ ��⨢�� ��易⥫��⢠�� ��। 
                   ������ ���ᨨ' �ਭ����� ���祭�� �� ��६��療� (��/��� (����)). 
                   �����筮 ���� �ਧ��� �� �����  ��६������ (��ࢮ� �����襬 � �ਧ�����).
                   ��室��� ��ࠬ��� 2  '�ਧ��� ����६������� ��⨢��, �ਣ����� ��� 
                   �।��⠢����� � ����⢥ ���ᯥ祭�� ����� ���ᨨ' �ਭ����� ���祭�� 
                   �� ���ᯁ� �� �।�⭮� �������, �������饥 �� ������ ����.
"}
PROCEDURE GetObremCBRFAttr:
   DEFINE INPUT PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oObremCBRF AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oLoanCBRF  AS LOGICAL NO-UNDO.
   
   DEFINE BUFFER oloan FOR loan.
   
   oLoanCBRF = LOGICAL(GetXattrValueEx("loan",iContract + "," + iContCode,"���ᯁ�","���"),
                       "��/���").
   OBR:
   FOR EACH oloan WHERE oloan.parent-contract   = iContract
                    AND oloan.parent-cont-code  = iContCode
                    AND oloan.contract          = "��६"
                    AND oloan.cont-code         BEGINS iContCode
   NO-LOCK:
      oObremCBRF = LOGICAL(GetXattrValueEx("loan",
                                           GetSurrogate("loan",ROWID(oloan)),
                                           "��६��療�","���"),
                           "��/���").
     IF oObremCBRF THEN LEAVE OBR.
   END.
END PROCEDURE.

/* ��⠢�� ���� ���� */
FUNCTION PscMortgage RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):
/* message "tst" view-as alert-box.     */
    def var vPSC as decimal no-undo.
    def var typeTable as char no-undo.
    find first loan where loan.contract = iContract
        and loan.cont-code = iContCode no-lock no-error.
    if not avail loan then return 0.
    typeTable = '3'.
    if loan.cont-type = '���ॡ' or loan.cont-type = '��' or loan.cont-type = ? or loan.cont-type begins '���' or loan.cont-type begins 'Avto' then typeTable = '1'.

typeTable = '1'.

   RUN FillTables (loan.contract,
                   loan.cont-code,
                   iDate,
                   loan.cont-type,
                   typeTable,  /* 1- ���, 2- ���+��᪮, 3 - ��+ */
                   OUTPUT vPSC
                   ).

    return vPSC * 100.
END FUNCTION.

FUNCTION PscMortgage1 RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    def var vPSC as decimal no-undo.
    def var typeTable as char no-undo.
    find first loan where loan.contract = iContract
        and loan.cont-code = iContCode no-lock no-error.
    if not avail loan then return 0.
    typeTable = '2'. /* 2- ���+��᪮ */

   RUN FillTables (loan.contract,
                   loan.cont-code,
                   iDate,
                   loan.cont-type,
                   typeTable,  /* 1- ���, 2- ���+��᪮, 3 - ��+ */
                   OUTPUT vPSC
                   ).
    return vPSC * 100.
END FUNCTION.

FUNCTION PscTransh RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    def var vPSC as decimal no-undo.
    def var typeTable as char no-undo.
    find first loan where loan.contract = iContract
        and loan.cont-code = iContCode no-lock no-error.
    if not avail loan then return 0.
    typeTable = '2'. /* 2- ���+��᪮ */

   RUN FillTables (loan.contract,
                   loan.cont-code,
                   iDate,
                   loan.cont-type,
                   typeTable,  /* 1- ���, 2- ���+��᪮, 3 - ��+ */
                   OUTPUT vPSC
                   ).
    return vPSC * 100.
END FUNCTION.


DEF TEMP-TABLE ttReportTablePsc NO-UNDO
   FIELD tf_id                   AS INT64 /* ���浪��� ����� ��ப� */
   FIELD tf_payment-date         AS DATE  /* ��� ����樨 */
   FIELD tf_sum-payment          AS DEC   /* C㬬� ���⥦�) */
   FIELD tf_sum-percent          AS DEC   /* 1. �㬬� ��業⮢ */
   FIELD tf_rest-debts           AS DEC   /* 2. �㬬� ���⪮�  */
   FIELD tf_basic-sum-loan       AS DEC   /* 3. �㬬� �᭮����� ����� */
   FIELD tf_additional-charge1   AS DEC   /* 4. �㬬� �������⥫��� �ॡ������, �����ᨨ */
   FIELD tf_additional-charge2   AS DEC   /* 400. �㬬� �������⥫��� �ॡ������, �����ᨨ */
   FIELD tf_actual-payment       AS DEC   /* 5. �㬬� ���客�� ����ᮢ */
   FIELD tf_charges              AS CHAR  /* �ॡ������ */
   INDEX tf_id tf_payment-date
.

/* ���������� ⠡���� ����묨 ��� */
PROCEDURE FillTables:
    DEF INPUT PARAM iContract AS CHAR NO-UNDO.
    DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
    DEF INPUT PARAM iDate     AS DATE NO-UNDO.
    DEF INPUT PARAM iContType AS CHAR NO-UNDO.
    DEF INPUT PARAM typeTable AS CHAR NO-UNDO.
    DEF OUTPUT PARAM oEps AS DECIMAL NO-UNDO.

    DEF VAR vAccur   AS DEC   NO-UNDO.
    DEF VAR vTryNum  AS INT64   NO-UNDO.
    DEF VAR vStep    AS DEC   NO-UNDO. /* ���, ��� ���騢���� ��� */
    DEF VAR flag     AS LOG   NO-UNDO. /* yes - 㢥��稢���, no - 㬥��蠥� ��� */
    DEF VAR vI       AS INT64   NO-UNDO.
    DEF VAR vSummPay AS DEC   NO-UNDO. /* ���� �㬬� ���⥦�� */
    DEF VAR vBegDate AS DATE  NO-UNDO. /* ��� �뤠� �।��  */
    DEF VAR vSumma   AS DEC   NO-UNDO. /* �㬬� ��� ������ ��� */
    DEF VAR vInsurFl AS LOG   NO-UNDO. /* ������� � ���� ��� ���客� ���⥦�? */
    DEF VAR vEpsForm AS INT NO-UNDO.
    DEF VAR vPer     AS INT64 NO-UNDO.

    def var fullKasko as decimal no-undo.
    DEF BUFFER bloan FOR loan.
    def var c1 as char no-undo.
    def var c2 as char no-undo.
    DEF var vidstr AS CHAR NO-UNDO.
    def var vTmpDec as decimal no-undo.
    def var lastDate as date no-undo.
    def var summAuto as char no-undo.
    def var summAuto_8 as decimal no-undo.
    def var summLoan_9 as decimal no-undo.
    def var kaskoK_10 as decimal no-undo.
    def var kaskoN_11 as decimal no-undo.
    def var premStrah_12 as decimal no-undo.

    IF iDate >= DATE( 9, 1, 2014)
     THEN vEpsForm = 2.
     ELSE vEpsForm = 1.

    summAuto = getxattrvalue("loan","�।��," + loan.cont-code,"rko11_price").

    if summAuto <> ? and summAuto <> "" then do:
        summAuto_8 = decimal(replace(summAuto,",","")).
    end.
    else summAuto_8 = 1.

    ASSIGN
    vAccur  = 0.000001 /* dec(FGetSetting("���", "������", ?)) */
    vTryNum = 800  /* INT64(FGetSetting("���", "�������", ?)) */
    vInsurFl = yes /* FGetSetting("���", "���������叫" , "") EQ "��" */
    .

       vidstr = "�����*".
    vTmpDec = 0.
    FOR EACH bloan
        WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ '�����'
        NO-LOCK:
    IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                      bloan.contract + "," + bloan.cont-code,
                      "vidstr",
                      ""))
        THEN NEXT.
        IF GetXAttrValueEx("loan",
          bLoan.contract + "," + bLoan.cont-code,
          "��������",
          "��"
          ) EQ "���"
      THEN NEXT.

            FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

            IF AVAILABLE term-obl THEN
            vTmpDec = term-obl.amt-rub.
            ELSE
            vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
    END.
    premStrah_12 = vTmpDec.

    vidstr = "�����_�".
    vTmpDec = 0.
    FOR EACH bloan
        WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ '�����'
        NO-LOCK:
            IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN NEXT.
            FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

            IF AVAILABLE term-obl THEN
            vTmpDec = term-obl.amt-rub.
            ELSE
            vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
    END.
    fullKasko = vTmpDec.
    kaskoK_10 = vTmpDec.
    kaskoK_10 = 0.

    vidstr = "�����_�".
    vTmpDec = 0.

    FOR EACH bloan
        WHERE
        bloan.PARENT-CONT-CODE EQ iContCode
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ '�����'
        NO-LOCK:
            IF NOT can-do( vidstr, GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN NEXT.

            FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

            IF AVAILABLE term-obl THEN
            vTmpDec = term-obl.amt-rub.
            ELSE
            vTmpDec = DECIMAL(GetXAttrValueEx("loan", iContract + "," + iContCode, "kasko3", "")).
    end.

    fullKasko = fullKasko + vTmpDec.
    kaskoN_11 = vTmpDec.
    kaskoN_11 = 0.

    summLoan_9 = 0.

/*    find first term-obl where term-obl.cont-code = loan.cont-code and term-obl.idnt = 2 and term-obl.nn  = 0 no-lock no-error.
    if avail term-obl then summLoan_9 = term-obl.amt-rub.
*/
    for each term-obl
     where term-obl.contract = loan.contract
       AND term-obl.cont-code = loan.cont-code
       and term-obl.idnt = 2 no-lock by term-obl.end-date:
        summLoan_9 = term-obl.amt-rub.
        leave.
    end.
/* message string(summLoan_9) view-as alert-box. */
   RUN fill-graphp1.p(loan.contract,
                    loan.cont-code,
                    loan.since,
                    fullKasko,
                    typeTable,
                    OUTPUT TABLE ttReportTablePsc
                    ).


    lastDate = loan.end-date.
    find first ttReportTablePsc where ttReportTablePsc.tf_id = -1 no-error.
    if avail ttReportTablePsc then do:
        lastDate = ttReportTablePsc.tf_payment-date.
        delete ttReportTablePsc.
    end.

DEF VAR pskbezstrah AS CHAR NO-UNDO.
DEF VAR mytmpdec AS CHAR NO-UNDO.
pskbezstrah = GetSysConf("����������") NO-ERROR.
/*
message string(pskbezstrah) + ' ' + string(summLoan_9) view-as alert-box.
    */
    if summLoan_9 <> 0 then do:
        create ttReportTablePsc.
        if typeTable = '2' then do:  /* 1- ���, 2- ���+��᪮, 3 - ��+ */
            ttReportTablePsc.tf_id = -1.
            ttReportTablePsc.tf_payment-date = loan.open-date.
            if pskbezstrah = ? then
                 ttReportTablePsc.tf_basic-sum-loan = - summLoan_9 + (kaskoK_10 + kaskoN_11) *
                 ((summLoan_9 - kaskoK_10 - premStrah_12) / summAuto_8) + premStrah_12.
            else ttReportTablePsc.tf_basic-sum-loan = - summLoan_9.
            release ttReportTablePsc.
        end.
        else  do:
            assign
                ttReportTablePsc.tf_id = -1
                ttReportTablePsc.tf_payment-date = loan.open-date
                ttReportTablePsc.tf_basic-sum-loan = - summLoan_9
                ttReportTablePsc.tf_sum-percent = premStrah_12.
            release ttReportTablePsc.
        end.
    end.
/*
          run instview.p(TEMP-TABLE ttReportTablePsc:HANDLE).

*/


    CASE vEpsForm:
        WHEN 1 THEN
            vPer = 365.
        WHEN 2 THEN DO:
            DEF VAR numd AS INT NO-UNDO EXTENT 366 INIT 0.
            DEF VAR cda AS INT NO-UNDO.
            DEF VAR d2 AS DATE NO-UNDO.
            FOR EACH ttReportTablePsc BY ttReportTablePsc.tf_payment-date:
            IF ttReportTablePsc.tf_payment-date > loan.open-date THEN DO:
                cda = ttReportTablePsc.tf_payment-date - d2.
                IF cda < 366 THEN
                numd [ cda ] = numd[ cda ] + 1.
            END.
            d2 = ttReportTablePsc.tf_payment-date.
            END.
            DEF VAR imax AS INT NO-UNDO.
            DEF VAR imm AS INT NO-UNDO.
            vPer = 0. imax = 0.
            DO imm = 1 TO 365:
            IF imax < numd [ imm ] THEN DO:
                imax = numd [ imm ].
                vPer = imm.
            END.
            END.
            IF imax = 1 THEN DO:
            DEF VAR summ AS INT64 NO-UNDO.
            summ = 0.
            imax = 0.
            DO imm = 1 TO 365:
                IF numd [ imm ] = 1 THEN DO:
                summ = summ + imm.
                imax = imax + 1.
                END.
            END.
            IF imax > 0 THEN DO:
                vPer = ROUND( summ / imax , 0).
            END. ELSE DO:
                vPer = 0.
            END.
            END.

            /* vPer = 30. */
            END.
    END CASE.
/* message "vEpsForm=" string(vEpsForm) " " string(vPer) view-as alert-box. */


    BLCK:
    DO
    ON ERROR    UNDO BLCK, LEAVE BLCK:
        ASSIGN
         vBegDate = loan.open-date
         oEps = 0.1
         vStep = 0.1
         flag = YES
      .

      DO vi=1 TO vTryNum:
         vSumma = 0.
        FOR EACH ttReportTablePsc:
            if typeTable = '2' then do:  /* 1- ���, 2- ���+��᪮, 3 - ��+ */
                if ttReportTablePsc.tf_id = -1 then do:
                    vSummPay = ttReportTablePsc.tf_basic-sum-loan.

                end.
                else do:
                    /*IF iDate >= DATE( 7, 1, 2014)
                      AND ttReportTablePsc.tf_additional-charge1 > 0
                     THEN vSummPay = ttReportTablePsc.tf_additional-charge1.
                    ELSE*/ DO:
                    vTmpDec = lastDate - ttReportTablePsc.tf_payment-date.
                    if vTmpDec > 365 then vTmpDec = 1. else vTmpDec = vTmpDec / 365.
                    if pskbezstrah = ? then
                       vSummPay = decimal(ttReportTablePsc.tf_sum-payment) + decimal(ttReportTablePsc.tf_actual-payment) * ((summLoan_9 - kaskoK_10 - premStrah_12) / summAuto_8) * vTmpDec.
                    else  vSummPay = decimal(ttReportTablePsc.tf_sum-payment).
                    END.
                end.
            end.

            else  do:
                ASSIGN
                    vSummPay = ttReportTablePsc.tf_sum-percent +
                        ttReportTablePsc.tf_basic-sum-loan +
                        ttReportTablePsc.tf_additional-charge1 +
                        ttReportTablePsc.tf_additional-charge2
                    vSummPay = vSummPay + ttReportTablePsc.tf_actual-payment WHEN vInsurFl
                    .
            end.
        CASE vEpsForm:
        WHEN 1 THEN
            vSumma   = vSumma + vSummPay  /
                    EXP((1 + oEps), (ttReportTablePsc.tf_payment-date
                    - vBegDate) / 365).
                WHEN 2 THEN
            vSumma   = vSumma + vSummPay  / (
            (1 + oEps * ( (ttReportTablePsc.tf_payment-date - vBegDate) MODULO vPer ) / vPer ) *
                    EXP((1 + oEps), TRUNCATE( (ttReportTablePsc.tf_payment-date
                    - vBegDate) / vPer, 0) )
                    ).
                END CASE.

         END.
         IF vSumma LT 0 AND flag EQ YES THEN
         DO:
            flag  = NO.
            vStep = vStep / 10.
         END.
         IF vSumma GT 0 AND flag EQ NO THEN
         DO:
            flag = YES.
            vStep = vStep / 10.
         END.
         IF vStep GE vAccur THEN  DO: /* !! */
            IF flag EQ YES THEN
               oEps = oEps + vStep.
            ELSE
               oEps = oEps - vStep.
         END.
         ELSE LEAVE blck.
      END.
    END. /* blck */
    IF vEpsForm = 2 THEN oEps = oEps * /* ROUND */ ( 365 / vPer).
    oEps = truncate(oEps + 0.000004 ,5).

/*message "oEps=" string(oEps) view-as alert-box.
RUN instview.p(TEMP-TABLE ttReportTablePsc:HANDLE).*/
END PROCEDURE.
/* ����� ��⠢�� ���� ���� */
/* $LINTFILE='pp-pqres.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='chumv' */
/* $LINTDATE='11/07/2017 12:56:05.400+03:00' */
/*prosign2WAk8lEEoAV4Qftiqhd7fQ*/