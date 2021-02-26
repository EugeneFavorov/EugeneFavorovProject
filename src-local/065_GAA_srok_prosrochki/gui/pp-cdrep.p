{globals.i}
{intrface.get tmess}

/* +++ pp-cdrep.p was humbly modified by (c)blodd converter v.1.11 on 3/13/2017 2:16pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� QBIS
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-cdrep.p
      Comment: �����㬥��� ��� ���⮢ �� ������ �।�� - �� �����㬥��� ������ᨬ�
   Parameters: ���
         Uses:
      Used by:
      Created: 10.09.2014 guiv
     Modified: 10.09.2014 guiv
*/

/*===========================================================================*/
/* �����㬥��� ��� ���⮢ �� ������ �।�� - �� �����㬥��� ������ᨬ� */
/* ����� �ᯮ�짮���� � ��㣨� �����                                        */
/*===========================================================================*/
{pfuncdef
 &DefLib="cdrep" 
 &Description="�����㬥��� ��� ���⮢ �� ������ �।��"
}
{globals.i}
{intrface.get loan}
{intrface.get lv}
{intrface.get lnbh}
{intrface.get db2l}
{intrface.get schem}
{intrface.get date}
{intrface.get i254}
{intrface.get instrum}
{intrface.get lngar}   /* ������⥪� ��� ࠡ��� � ��࠭�ﬨ */
{par_mass.i}        /* ����室��� ��� �।�⭮�� ����� */
{loan.pro}
{client.i}
{debug.equ}
{ttdosrpog.i}

DEF VAR mNew316 AS LOG NO-UNDO.

/* �६.⠡��� � �㬬��� ����襭�� ��ࠬ��஢ */
DEF TEMP-TABLE ttPrmPog NO-UNDO
   FIELD grpar   AS CHAR
   FIELD grext   AS CHAR
   FIELD vunos   AS DEC
   FIELD summPog AS DEC
.

/* �६.⠡��� � ⥪�騬 ��䨪�� ��� ��䨪�� �� ���ਨ */
DEF TEMP-TABLE tt-hist-amt NO-UNDO LIKE tobl-hist-amt.

PROCEDURE StartInterface.
   ASSIGN
      mNew316  = FGetSetting("��ଠ316", "�������", "") EQ "���(���)".
   .
   RETURN.
END PROCEDURE.
/* &GLOB IS-DEBUG true */
&GLOB FIND_LOAN  RUN RE_B_LOAN IN h_Loan (iContract,iContCode,BUFFER loan). ~
                               IF NOT AVAIL loan THEN RETURN



/*===========================================================================*/
/*== ���ࠧ������� � �⤥����� ������� (��� & �⤫) ========================*/
/*===========================================================================*/
PROCEDURE LN_Branch:

   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBranchId   AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBranchName AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOtdelId    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOtdelName  AS CHAR NO-UNDO.

   DEF VAR vBranch AS CHAR NO-UNDO.
   DEF VAR vBrList AS CHAR NO-UNDO.

   DEF BUFFER Branch  FOR branch.
   DEF BUFFER xBranch FOR branch.

   RELEASE branch.
   RELEASE xbranch.

   /*
   vBranch = GetXattrValueEx("loan",
                             iContract + "," + iContCode,
                             "branch-id",
                             "").
   */
   vBranch = GetBufferValue ("loan",
                            "WHERE loan.contract EQ '" + iContract + "'
                               AND loan.cont-code EQ '" + iContCode + "'",
                            "branch-id").

   IF NOT ({assigned vBranch}) THEN RETURN.

   RUN LN_GetBranchVTB(vBranch,
                       OUTPUT oBranchId,
                       OUTPUT oBranchName,
                       OUTPUT oOtdelId,
                       OUTPUT oOtdelName).

END PROCEDURE.

/*===========================================================================*/
/*== ���ࠧ������� � �⤥����� ᮮ⢥��⢪�騥 ��।����� ���� ���������� =*/
/*== ����� ���᫥��� �� �ࠢ���� ��� ======================================*/
/*===========================================================================*/
PROCEDURE LN_GetBranchVTB:
   DEF INPUT  PARAM iBranchId   AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBranchId   AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBranchName AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOtdelId    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOtdelName  AS CHAR NO-UNDO.

   DEF VAR vBranch AS CHAR NO-UNDO.
   DEF VAR vBrList AS CHAR NO-UNDO.

   DEF BUFFER Branch  FOR branch.
   DEF BUFFER xBranch FOR branch.

   FIND FIRST branch WHERE branch.branch-id = iBranchId NO-LOCK NO-ERROR.

   IF NOT AVAIL branch OR branch.branch-type < "10" THEN
   DO:
      ASSIGN
         oBranchId   = ""
         oBranchName = ""
         oOtdelId    = ""
         oOtdelName  = ""
         .
      RETURN.
   END.

   IF branch.branch-type >= "10" AND  branch.branch-type < "20" THEN
   DO:
      ASSIGN
         oBranchId   = branch.branch-id
         oBranchName = branch.name
         oOtdelId    = ""
         oOtdelName  = ""
         .
      RETURN.
   END.

   RUN GetBranchParent_Type IN h_lnbh (vBranch,
                                      "10,11,12,13,14,15,16,17,18,19",
                                      INPUT-OUTPUT vBrList).
   IF vBrList <> "" THEN
      FIND FIRST xbranch WHERE
                 xbranch.branch-id = ENTRY(1,vBrList) NO-LOCK NO-ERROR.
   ASSIGN
      oBranchId   = xbranch.branch-id WHEN AVAIL xbranch
      oBranchName = xbranch.name  WHEN AVAIL xbranch
      oOtdelId    = branch.branch-id
      oOtdelName  = branch.name
      .

END PROCEDURE.
/*===========================================================================*/
/*====== �ப ������� (�᫮ ��ਮ��� ����襭��) (�ப)=====================*/
/*===========================================================================*/
FUNCTION LN_GetSrok RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR):

   DEF VAR vCounter AS INT64 NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND CAN-DO("1,3",STRING(term-obl.idnt))
   NO-LOCK BREAK BY term-obl.end-date:
      ACCUM term-obl.end-date (COUNT BY term-obl.end-date).
   END.

   vCounter = ACCUM COUNT term-obl.end-date.

   RETURN (MAX(vCounter - 1,0)).

END FUNCTION.
/*===========================================================================*/
/*==== �ਧ��� ��ᠩ��� ������ (���) ======================================*/
/*===========================================================================*/
FUNCTION LN_GetInsaider RETURNS LOG
   (iCustCat AS CHAR,
    iCustId  AS INT64):

   DEF VAR vInsaiderStr AS CHAR NO-UNDO.
   DEF VAR vInsaiderCli AS CHAR NO-UNDO.

   vInsaiderStr = FGetSetting("�ਧ������",?,"").

   IF NOT ({assigned vInsaiderStr}) THEN RETURN NO.

   vInsaiderCli = GetCustSignEx(iCustCat,iCustId,"������").

   IF NOT ({assigned vInsaiderCli}) THEN RETURN NO.

   RETURN (LOOKUP(vInsaiderCli,vInsaiderStr) > 0).

END FUNCTION.
/*===========================================================================*/
/*=== ������⢮ ��८�ଫ���� ������� ���묨 �᫮��ﬨ (�����८�) =======*/
/*===========================================================================*/
FUNCTION LN_GetNumProl RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iSince    AS DATE):

   DEF BUFFER loan-cond FOR loan-cond. /* ���������� ����. */

   DEF QUERY qLCondTotal FOR loan-cond SCROLLING. /* �롮ઠ �� �᫮���. */

   /* ���� �롮�� ��� ����祭�� ������⢠ ����ᥩ. */
   OPEN QUERY qLCondTotal PRESELECT EACH loan-cond WHERE
      loan-cond.contract  = iContract AND
      loan-cond.cont-code = iContCode AND
      loan-cond.since    <= iSince
   NO-LOCK.

   /* �஢�ઠ ������⢠ �������⥫��� ᮣ��襭�� (�᫮���). */
   RETURN (MAX(NUM-RESULTS ("qLCondTotal") - 1,0)).

END FUNCTION.
/*===========================================================================*/
/*===== ��ࠬ���� �� �������� ===============================================*/
/*===========================================================================*/
FUNCTION LN_GetParams RETURNS DEC
  (iContract AS CHAR,
   iContCode AS CHAR,
   iParams   AS CHAR,
   iDate     AS DATE):

   DEF VAR vParSumm AS DEC NO-UNDO.
   DEF VAR vSumma   AS DEC NO-UNDO.
   DEF VAR vCode    AS INT64 NO-UNDO.
   DEF VAR vDb      AS DEC NO-UNDO.
   DEF VAR vCr      AS DEC NO-UNDO.
   DEF VAR vCounter AS INT64 NO-UNDO.

   DO vCounter = 1 TO NUM-ENTRIES(iParams):

      vCode = INT64(ENTRY(vCounter,iParams)).

      RUN RE_PARAM IN h_Loan (vCode,
                              iDate,
                              iContract,
                              iContCode,
                              OUTPUT vParSumm,
                              OUTPUT vDb,
                              OUTPUT vCr).
      vSumma = vSumma + vParSumm.
   END.

   RETURN vSumma.

END FUNCTION.
/*===========================================================================*/
/*===== ��ࠬ���� �� ��������, � ��⮬ ⥪��� ���᫥���===================*/
/*===========================================================================*/
FUNCTION LN_GetParamsInteres RETURNS DEC
  (iContract AS CHAR,
   iContCode AS CHAR,
   iParams   AS CHAR,
   iDate     AS DATE):

   DEF BUFFER bloan FOR loan.

   DEF VAR vParSumm AS DEC NO-UNDO.
   DEF VAR vSumma   AS DEC NO-UNDO.
   DEF VAR vCode    AS INT64 NO-UNDO.
   DEF VAR vDb      AS DEC NO-UNDO.
   DEF VAR vCr      AS DEC NO-UNDO.
   DEF VAR vCounter AS INT64 NO-UNDO.
   DEF VAR vCodOstpar AS INT64  NO-UNDO.
   DEF VAR vAmtDiff   AS DEC  NO-UNDO. /* �㬬� ���४�஢�� ��ࠬ��� */

   FIND FIRST bloan WHERE bloan.contract  EQ iContract
                      AND bloan.cont-code EQ iContCode
        NO-LOCK NO-ERROR.

   IF AVAILABLE(bloan) THEN
   DO:
      vCodOstpar = GetParCode(bloan.class-code, "����᭄���").

      DO vCounter = 1 TO NUM-ENTRIES(iParams):

         vCode = INT64(ENTRY(vCounter,iParams)).


         RUN RE_PARAM IN h_Loan (vCode,
                                 iDate,
                                 iContract,
                                 iContCode,
                                 OUTPUT vParSumm,
                                 OUTPUT vDb,
                                 OUTPUT vCr).
         RUN inter_current  (BUFFER bloan, vCode - vCodOstpar, OUTPUT vAmtDiff).

         vSumma = vSumma + vParSumm + vAmtDiff.
      END.
   END.

   RETURN vSumma.

END FUNCTION.
/*===========================================================================*/
/*===== ���� �����窨 �᭮����� ����� (�����������) =====================*/
/*===========================================================================*/
FUNCTION LN_GetPrsDolgDays RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    DEF VAR vDb     AS DEC  NO-UNDO.
    DEF VAR vCr     AS DEC  NO-UNDO.
    DEF VAR vParam  AS DEC  NO-UNDO.
    DEF VAR vParam1 AS DEC  NO-UNDO.

    DEF BUFFER loan-int FOR loan-int.

    RELEASE loan-int.

    FOR EACH loan-int WHERE
             loan-int.contract  = iContract
         AND loan-int.cont-code = iContCode
         AND loan-int.mdate    <= iDate
         AND (CAN-DO("7,13",STRING(loan-int.id-d)) OR
              CAN-DO("7,13",STRING(loan-int.id-k)))
    NO-LOCK BY loan-int.mdate DESCENDING:

       IF LN_GetParams (iContract,
                        iContCode,
                        "7,13",
                        loan-int.mdate - 1) = 0
       THEN RETURN (iDate - loan-int.mdate).
    END.

    RETURN 0.

END FUNCTION.
/*===========================================================================*/
/*===== ���� ����窨 %% (����������) ====================================*/
/*===========================================================================*/
FUNCTION LN_GetPrsProcDays RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

    DEF VAR vDb     AS DEC  NO-UNDO.
    DEF VAR vCr     AS DEC  NO-UNDO.
    DEF VAR vParam  AS DEC  NO-UNDO.
    DEF VAR vParam1 AS DEC  NO-UNDO.

    DEF BUFFER loan-int FOR loan-int.

    RELEASE loan-int.

    FOR EACH loan-int WHERE
             loan-int.contract  = iContract
         AND loan-int.cont-code = iContCode
         AND loan-int.mdate    <= iDate
         AND (CAN-DO("10,16",STRING(loan-int.id-d)) OR
              CAN-DO("10,16",STRING(loan-int.id-k)))
    NO-LOCK BY loan-int.mdate DESCENDING:

       IF LN_GetParams (iContract,
                        iContCode,
                        "10,16",
                        loan-int.mdate - 1) = 0
       THEN RETURN (iDate - loan-int.mdate).
    END.

    RETURN 0.

END FUNCTION.
/*===========================================================================*/
/*===== ����� ��ࠬ��� � ���⠭�묨 ���ﬨ ==============================*/
/*===========================================================================*/
FUNCTION LN_GetParamPen RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iParam    AS INT64,
    iDate     AS DATE  ):

   DEF VAR vDb     AS DEC NO-UNDO.
   DEF VAR vCr     AS DEC NO-UNDO.
   DEF VAR vParam  AS DEC NO-UNDO.
   DEF VAR vLoanInterest AS CHAR INIT "4,8,9,11,12,14,15,17,18,20,81,82,96"
                          NO-UNDO.

   DEF BUFFER loan FOR loan.
   
   FIND FIRST loan WHERE
              loan.contract EQ iContract
          AND loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.

   /* ����� ��ࠬ��� ��� ��� ��業⮢ */
   RUN RE_PARAM IN h_loan (iParam,
                           ?,
                           iContract,
                           iContCode,
                           OUTPUT vParam,
                           OUTPUT vDb,
                           OUTPUT vCr).
   RETURN ( vParam +
            IF LOOKUP(STRING(iParam),vLoanInterest) <> 0
            THEN loan.interest [LOOKUP(STRING(iParam),vLoanInterest)]
            ELSE 0 ).

END FUNCTION.
/*===========================================================================*/
/*==== ����筮� ����襭�� ���� �� ��ਮ� (����)============================*/
/*===========================================================================*/
FUNCTION LN_GetBefPog RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iSince    AS DATE,
    iBegDate  AS DATE,
    iEndDate  AS DATE):

   DEF VAR vIsFirst AS LOG   NO-UNDO.
   DEF VAR vRecCond AS RECID NO-UNDO.
   DEF VAR vSurr    AS CHAR  NO-UNDO.
   DEF VAR vOst     AS DEC   NO-UNDO.
   DEF VAR vPredPog AS DEC   NO-UNDO.
   DEF VAR vSumma   AS DEC   NO-UNDO.
   DEF VAR vRem     AS DEC   NO-UNDO.

   DEF BUFFER loan-cond FOR loan-cond.
   DEF BUFFER term-obl  FOR term-obl.

   FOR EACH loan-cond WHERE
            loan-cond.contract  = iContract
        AND loan-cond.cont-code = iContCode
   NO-LOCK BY loan-cond.since:
      vRecCond = RECID(loan-cond).
      LEAVE.
   END.

   FOR EACH loan-cond WHERE
            loan-cond.contract  = iContract
        AND loan-cond.cont-code = iContCode
        AND loan-cond.since    >= iBegDate
        AND loan-cond.since    <= iEndDate
        AND RECID(loan-cond)   <> vRecCond
   NO-LOCK:
      ASSIGN
         vSurr    = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE)).
         vPredPog = DEC(GetXAttrValueEx("loan-cond",
                                        vSurr,
                                        "�।����������",
                                        "0"))
         .
      FIND FIRST term-obl WHERE
                 term-obl.contract  = iContract
             AND term-obl.cont-code = iContCode
             AND term-obl.idnt      = 3
             AND term-obl.end-date  = loan-cond.since
      NO-LOCK NO-ERROR.

      IF NOT AVAIL term-obl THEN NEXT.

      RUN SetSysConf IN h_base ("������������⥦�","��").

      RUN summ-t.p(OUTPUT vSumma,
                   iContract,
                   iContCode,
                   RECID(term-obl),
                   iSince).

      RUN DeleteOldDataProtocol IN h_base ("������������⥦�").

      vOst   = term-obl.amt-rub.

      vRem = vRem + IF vOst > vPredPog
                    THEN IF vSumma > (vOst - vPredPog)
                         THEN vOst - vPredPog
                         ELSE vSumma
                    ELSE 0.
   END.

   RETURN vRem.

END FUNCTION.
/*===========================================================================*/
/*=== ��� ������� (���) ==================================================*/
/*===========================================================================*/
FUNCTION LN_GetAcct RETURNS CHAR
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF BUFFER loan-acct FOR loan-acct.

   RUN RE_L_ACCT IN h_Loan(iContract,
                           iContCode,
                           GetMainAcctRole(iContract,iContCode),
                           iDate,
                           BUFFER loan-acct).
   RETURN ( IF AVAIL loan-acct THEN loan-acct.acct ELSE "").

END FUNCTION.
/*===========================================================================*/
/*=== ��� ������� (���,�����) ===========================================*/
/*===========================================================================*/
FUNCTION LN_GetAcctSurr RETURNS CHAR
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF BUFFER loan-acct FOR loan-acct.

   RUN RE_L_ACCT IN h_Loan(iContract,
                           iContCode,
                           GetMainAcctRole(iContract,iContCode),
                           iDate,
                           BUFFER loan-acct).
   RETURN ( IF AVAIL loan-acct THEN loan-acct.acct + "," + loan-acct.currency ELSE "").

END FUNCTION.
/*===========================================================================*/
/*=== ��� ������� (������) =================================================*/
/*===========================================================================*/
FUNCTION LN_GetContType RETURNS CHAR
   (iContract AS CHAR,
    iContCode AS CHAR):

   DEF BUFFER loan  FOR loan.
   DEF BUFFER class FOR class.

   RUN RE_B_LOAN IN h_Loan (iContract,iContCode,BUFFER loan).

   IF NOT AVAIL loan THEN RETURN "".

   FIND FIRST class WHERE
              CLASS.CLASS-CODE = loan.class-code
   NO-LOCK NO-ERROR.

   RETURN ( IF AVAIL class THEN CLASS.NAME ELSE "").

END FUNCTION.
/*===========================================================================*/
/*=== �㬬� �� �������� (�㬬���) ===========================================*/
/*===========================================================================*/
FUNCTION LN_GetLoanSumma RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR):

   DEF BUFFER term-obl FOR term-obl.

   FOR FIRST term-obl WHERE
             term-obl.contract  = iContract
         AND term-obl.cont-code = iContCode
         AND term-obl.idnt      = 2
   NO-LOCK BY term-obl.end-date:
      RETURN term-obl.amt-rub.
   END.

   RETURN 0.00.

END FUNCTION.
/*===========================================================================*/
/*== ����� ��� ���஢��, �᫨ ����宦��� �����஢��� ᭠砫� �㡫��� ==*/
/*== ��⮬ �����஢�, ��஢�, � ����� �� ��⠫�� ������� ==============*/
/*===========================================================================*/
FUNCTION LN_GetAltCurr RETURNS CHAR
   (iCurrency AS CHAR):

  RETURN(IF iCurrency = ""
         THEN  "!!!"
         ELSE IF iCurrency = "840"
              THEN  "!!1"
              ELSE IF iCurrency = "978"
                   THEN  "!!2"
                   ELSE iCurrency).
END FUNCTION.
/*===========================================================================*/
/*=== ��� �����祭�� ᤥ��� (��⠇���) =====================================*/
/*===========================================================================*/
FUNCTION LN_GetDataSdelki RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,
    iOpenDate AS DATE):

   DEF VAR vDate AS DATE NO-UNDO.

   DEF BUFFER loan FOR loan.

   IF iOpenDate = ? THEN
   DO:
      RUN RE_B_LOAN IN h_Loan (iContract,iContCode,BUFFER loan).
      IF NOT AVAIL loan THEN RETURN ?.
      iOpenDate = loan.open-date.
   END.

   vDate = DATE(GetXAttrValueEx("loan",
                                iContract + "," + iContCode,
                                "��⠑���",
                                ?)) NO-ERROR.

   IF ERROR-STATUS:ERROR OR vDate = ? THEN vDate = iOpenDate.

   RETURN vDate.

END FUNCTION.
/*===========================================================================*/
/*=== ����襭�� ���� (�������) ============================================*/
/*===========================================================================*/
FUNCTION LN_GetDolgPog RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iSince    AS DATE,
    iBegDate  AS DATE,
    iEndDate  AS DATE):

   DEF VAR vSumma AS DEC NO-UNDO.
   DEF VAR vTotal AS DEC NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = 3
        AND term-obl.end-date >= iBegDate
        AND term-obl.end-date <= iEndDate
   NO-LOCK:
      RUN summ-t.p (OUTPUT vSumma,
                    iContract,
                    iContCode,
                    RECID(term-obl),
                    iSince).
      vTotal = vTotal + vSumma.
   END.

   RETURN (vTotal -
           LN_GetBefPog(iContract,iContCode,iSince,iBegDate,iEndDate)).

END FUNCTION.
/*===========================================================================*/
/*=== ����襭�� %% (������) ================================================*/
/*===========================================================================*/
FUNCTION LN_GetProcPog RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iSince    AS DATE,
    iBegDate  AS DATE,
    iEndDate  AS DATE):

   DEF VAR vSumma AS DEC NO-UNDO.
   DEF VAR vTotal AS DEC NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER loan     FOR loan.

   RUN RE_B_LOAN IN h_Loan (iContract,iContCode,BUFFER loan).

   IF NOT AVAIL loan THEN RETURN ?.

   FOR EACH term-obl WHERE
            term-obl.contract  = iContract
        AND term-obl.cont-code = iContCode
        AND term-obl.idnt      = 1
        AND term-obl.end-date >= iBegDate
        AND term-obl.end-date <= iEndDate
   NO-LOCK:
      RUN summ-t1.p (OUTPUT vSumma,RECID(term-obl),RECID(loan)).
      vTotal = vTotal + vSumma.
   END.

   RETURN vTotal.

END FUNCTION.
/*===========================================================================*/
/*=== ���� �� ����筮� ����襭�� (�ℏ��) =================================*/
/*===========================================================================*/
FUNCTION LN_GetSht RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iCodPar   AS INT64,
    iNum      AS INT64,
    iSince    AS DATE):

   DEF VAR vParamStr AS CHAR NO-UNDO.
   DEF VAR vNumStr   AS CHAR NO-UNDO.
   DEF VAR vSumm     AS DEC  NO-UNDO.
   DEF VAR vDb       AS DEC  NO-UNDO.
   DEF VAR vCr       AS DEC  NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.

   IF iCodPar = ? AND iNum = ? THEN RETURN ?.

   IF iCodPar = ? OR iNum = ? THEN
   DO:
      ASSIGN
         vParamStr = FGetSetting("��������","�����ࠬ",?)
         vNumStr   = FGetSetting("��������","������",?)
         .
      IF iNum = ? THEN
         iNum      = INT64(ENTRY(LOOKUP(STRING(iCodPar),vParamStr),vNumStr)).
      IF iCodPar = ? THEN
         iCodPar   = INT64(ENTRY(LOOKUP(STRING(iNum),   vNumStr)  ,vParamStr)).

   END.

   RUN STNDRT_PARAM IN h_Loan (iContract,
                               iContCode,
                               iCodPar,
                               iSince,
                               OUTPUT vSumm,
                               OUTPUT vDb,
                               OUTPUT vCr).
   FIND FIRST term-obl WHERE
              term-obl.contract  = iContract
          AND term-obl.cont-code = iContCode
          AND term-obl.idnt      = 6
          AND term-obl.end-date  = iSince
          AND term-obl.nn        = iNum
   NO-LOCK NO-ERROR.

   IF AVAIL term-obl THEN
       vSumm = vSumm + term-obl.amt-rub.

   RETURN vSumm.

END FUNCTION.
/*===========================================================================*/
/*==== ������������ ������ =================================================*/
/*===========================================================================*/
FUNCTION LN_GetClient RETURNS CHAR
   (iCustCat AS CHAR,
    iCustId  AS INT64):

    DEF VAR vName AS CHAR NO-UNDO.

    RUN RE_CLIENT(iCustCat,iCustId,INPUT-OUTPUT vName).

    RETURN vName.

END FUNCTION.
/*===========================================================================*/
/*=== �ᯮ����⥫쭠� �-�� ��� ���� ������� ��ࠬ��� ===================*/
/*=== ��।���� ������ �� ���� � ���஬� �ਢ易�� ������ ==============*/
/*===========================================================================*/
FUNCTION LN_GetRoleCurr RETURNS CHAR
   (iContract AS CHAR,   /* �����䨪��� */
    iContCode AS CHAR,   /* �������      */
    iOperId   AS INT64,    /* ��� ����樨  */
    iSince    AS DATE):  /* ��� ����  */

   DEF VAR vPref  AS CHAR NO-UNDO.

   DEF BUFFER loan-acct FOR loan-acct.
   DEF BUFFER code      FOR code.

   vPref = SUBSTR(iContract,1,LENGTH(iContract) - 2).

   RELEASE loan-acct .

   /* �饬 ��� �� ������ ����樨 */
   FIND FIRST code WHERE
              code.class  = "����焮�"
          AND code.code   = '��' + STRING(iOperId)
          AND code.parent BEGINS vPref
      NO-LOCK NO-ERROR.

   IF AVAIL code THEN
      RUN RE_L_ACCT IN h_Loan (iContract,
                               iContCode,
                               code.parent,
                               iSince,
                               BUFFER loan-acct).
   RELEASE code.

   /* �饬 ��� �� �।��� ����樨 */
   IF NOT AVAIL loan-acct THEN
      FIND FIRST code WHERE
                 code.class  = "����焮�"
             AND code.code   = '��' + STRING(iOperId)
             AND code.parent BEGINS vPref
         NO-LOCK NO-ERROR.

   IF AVAIL code THEN
      RUN RE_L_ACCT IN h_Loan (iContract,
                               iContCode,
                               code.parent,
                               iSince,
                               BUFFER loan-acct).
   RETURN ( IF AVAIL loan-acct
           THEN loan-acct.currency
           ELSE ?).

END FUNCTION.
/*===========================================================================*/
/*=== ����祭�� ������ ��ࠬ��� ============================================*/
/*===========================================================================*/
FUNCTION LN_GetParamCurr RETURNS CHAR
   (iContract AS CHAR,  /* �����䨪���   */
    iContCode AS CHAR,  /* �������        */
    iKodPar   AS INT64,   /* ��� ��ࠬ���   */
    iLoanCurr AS CHAR,  /* ����� ������� */
    iSince    AS DATE): /* ��� ����    */

   DEF VAR vListParam AS CHAR NO-UNDO.
   DEF VAR vCurr      AS CHAR NO-UNDO.
   DEF VAR vPayParam  AS INT64  NO-UNDO.
   DEF VAR vDefault   AS CHAR NO-UNDO.

   DEF BUFFER chowhe   FOR chowhe.
   DEF BUFFER loan-par FOR loan-par.

   ASSIGN
      vListParam = FGetSetting("OnlyRuR",?,"")
      vCurr      = ?
      vDefault   = "21"
      .

   /* �᫨ ��ࠬ��� ���뢠���� ⮫쪮 � �㡫�� */
   {additem.i vListParam vDefault}

   IF vListParam <> '' AND CAN-DO(vListParam,STRING(iKodPar))
   THEN RETURN "".

   vListParam = FGetSetting("CurrAny",?,"").

   IF vListParam <> "" AND CAN-DO(vListParam,STRING(iKodPar)) THEN
   DO:
      /* �饬 ������ ����� 㢥��稢��� ��ࠬ��� � ��।��塞 ������ */
      FIND FIRST chowhe WHERE chowhe.id-d = iKodPar NO-LOCK NO-ERROR.

      IF AVAIL chowhe THEN
         vCurr = LN_GetRoleCurr(iContract,iContCode,chowhe.id-op,iSince).
      IF vCurr = ? THEN
      DO:
         /* �஡���� WHOLE-INDEX chowhe �㤥� �襭� �� ��� � 0243185 */
         FIND FIRST chowhe WHERE chowhe.id-k = iKodPar NO-LOCK NO-ERROR.

         IF AVAIL chowhe THEN
            vCurr = LN_GetRoleCurr(iContract,iContCode,chowhe.id-op,iSince).
      END.

      IF vCurr = ? THEN
      DO:
         FIND FIRST loan-par WHERE loan-par.amt-id = iKodPar NO-LOCK NO-ERROR.

         IF AVAIL loan-par THEN
         DO:
            vPayParam = DeliverParam(loan-par.amt-id,iSince) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN vPayParam = 0.
         END.

         RELEASE chowhe.

         IF vPayParam <> 0 AND
            vPayParam <> ?
         THEN
            /* �஡���� WHOLE-INDEX chowhe �㤥� �襭� �� ��� � 0243185 */ 
            FIND FIRST chowhe WHERE chowhe.id-k = vPayParam NO-LOCK NO-ERROR.

         IF AVAIL chowhe THEN
            vCurr = LN_GetRoleCurr(iContract,iContCode,chowhe.id-op,iSince).
      END.

      IF vCurr = ? THEN vCurr = iLoanCurr.
   END.
   ELSE
      vCurr = iLoanCurr.

   RETURN vCurr.

END FUNCTION.
/*===========================================================================*/
/*===========================================================================*/
/*===========================================================================*/
FUNCTION LN_GetGarantBeg RETURNS DATE
   (iTermRec AS RECID,
    iBegDate AS DATE):

   DEF VAR vDate AS DATE NO-UNDO.

   DEF BUFFER loan     FOR loan.
   DEF BUFFER term-obl FOR term-obl.

   FIND FIRST term-obl WHERE RECID(term-obl) = iTermRec NO-LOCK NO-ERROR.

   IF iBegDate = ? THEN
   DO:
      RUN RE_B_LOAN IN h_Loan (term-obl.contract,term-obl.cont-code,BUFFER loan).
      IF NOT AVAIL loan THEN RETURN ?.
      iBegDate = loan.open-date.
   END.

   vDate = DATE(GetXAttrValueEx ("term-obl",
                               term-obl.contract + "," + term-obl.cont-code + ",5,"
                               + STRING(term-obl.end-date) + ","
                               + STRING(term-obl.nn),
                               "��⠏���",
                               ?)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      vDate = ?.

   IF vDate = ? THEN vDate = term-obl.fop-date.
   IF vDate = ? THEN vDate = iBegDate.

   RETURN vDate.

END FUNCTION.

/*===========================================================================*/
/*=== ��� ��砫� ��ਮ�� ���㫥���� ���⪠ ��ࠬ��஢.=====================*/
/*===========================================================================*/
PROCEDURE LN_GetPrsDate: 
   DEF INPUT  PARAM iContract    AS CHAR.    /* �����祭�� �������. */
   DEF INPUT  PARAM iContCode    AS CHAR.    /* ����� �������. */
   DEF INPUT  PARAM iDate        AS DATE.    /* ��� �������. */
   DEF INPUT  PARAM iPosCntrPrmO AS CHAR.    /*ᯨ᮪ ��ࠬ��஢ � ࠧ�������� �� ��㯯� */
   DEF INPUT  PARAM iIsFIFO      AS LOGICAL. /*�ਧ��� ����*/
   DEF INPUT  PARAM iCurrDay     AS LOGICAL. /*���뢠�� ⥪�騩 ����*/
   DEF OUTPUT PARAM oResDate     AS DATE.    /*��� ��砫� ����窨*/
   DEF OUTPUT PARAM oProcFl      AS LOGICAL.    /*�᫨ ��, � ���� ����窠*/
   
   RUN LN_GetPrsDateEx IN THIS-PROCEDURE (iContract,
                                          ENTRY (1, iContCode, " "),
                                          iDate,
                                          iPosCntrPrmO,
                                          IF iIsFIFO THEN "fifo" ELSE "",
                                          iCurrDay,
                                          OUTPUT oResDate,
                                          OUTPUT oProcFl).
                    
END PROCEDURE.

{pfuncdef 
 &DefProc="LN_GetPrsDateEx"
 &Description="��� ��砫� ��ਮ�� ���㫥���� ���⪠ ��ࠬ��஢"
 &Parameters="�����祭�� �������,����� �������,��� �������, ~
              ᯨ᮪ ��ࠬ��஢ � ࠧ�������� �� ��㯯�,�ਧ��� ������, ~
              ���� ��� �����������,���뢠�� ⥪�騩 ����"
 &Result="��� ��砫� ����窨,�᫨ ��, � ���� ����窠"
 &Sample="�ਬ��"}
PROCEDURE LN_GetPrsDateEx: 
   DEF INPUT  PARAM iContract    AS CHAR    NO-UNDO. /* �����祭�� �������. */
   DEF INPUT  PARAM iContCode    AS CHAR    NO-UNDO. /* ����� �������. */
   DEF INPUT  PARAM iDate        AS DATE    NO-UNDO. /* ��� �������. */
   DEF INPUT  PARAM iPosCntrPrmO AS CHAR    NO-UNDO. /* ᯨ᮪ ��ࠬ��஢ � 
                                                        ࠧ�������� �� ��㯯� */
   DEF INPUT  PARAM iPosCntrPrmT AS CHAR    NO-UNDO. /* �ਧ��� ������, 
                                                       ���� ��� ����������� */
   DEF INPUT  PARAM iCurrDay     AS LOGICAL NO-UNDO. /* ���뢠�� ⥪�騩 ����*/
   DEF OUTPUT PARAM oResDate     AS DATE    NO-UNDO. /* ��� ��砫� ����窨*/
   DEF OUTPUT PARAM oProcFl      AS LOGICAL NO-UNDO. /* �᫨ ��, 
                                                        � ���� ����窠*/
   
   DEF VAR vDate        AS DATE   NO-UNDO. /* �஬����筠� ��� */
   DEF VAR vContType    AS CHAR   NO-UNDO. /* ��� ������� */
   DEF VAR vCnt         AS INT64  NO-UNDO. /* ���稪 */
   DEF VAR vPosCntrPrm  AS CHAR   NO-UNDO.
   DEF VAR vPosCntrPrmO  AS CHAR  NO-UNDO.
   DEF VAR vBracketL    AS INT64  NO-UNDO. /* ����� ᪮��� */
   DEF VAR vBracketR    AS INT64  NO-UNDO. /* �ࠢ�� ᪮��� */
   DEF VAR vDatePrP     AS DATE   NO-UNDO. /* ��� ����.%% �� ����樨 */
   DEF VAR vDatePr      AS DATE   NO-UNDO. /* ��� ����.�� �� ����樨 */
   DEF VAR vDateM       AS DATE   NO-UNDO. /* ��� ����樨 */
   DEF VAR vParam       AS CHAR   NO-UNDO.
   
   
   DEF VAR vFlag        AS LOG    INIT NO NO-UNDO.
   DEFINE VARIABLE vSumParam AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vQry      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vHQry     AS HANDLE      NO-UNDO.

   DEF BUFFER loan-int FOR loan-int. /* ���������� ����. */
   DEF BUFFER loan     FOR loan.     /* ���������� ����. */
   DEFINE BUFFER bloan FOR loan.
   
   MAIN_BLOCK:
   DO:
      ASSIGN       
         oProcFl = NO   
         vPosCntrPrmO = IF NUM-ENTRIES(iPosCntrPrmO,":") EQ 2 THEN ENTRY(2,iPosCntrPrmO,":")
                                                              ELSE iPosCntrPrmO /* ᪮॥ �ᥣ� �� ��譥� �.�. �ਧ��� fifo ��।����� � iIsFIFO */
      .
      {empty ttPrmPog}
         /* � ��㯯�஢��� ��ࠬ��஢ �१ ";", � �⮬ ��砥
         ** ����塞 ������ �⤥�쭮 ��� ������ ��㯯�  */
      REPEAT vCnt = 1 TO NUM-ENTRIES(vPosCntrPrmO, ";"):
         CREATE ttPrmPog.
         ASSIGN
            ttPrmPog.grpar = ENTRY(vCnt, vPosCntrPrmO, ";")
               /* �뤥�塞 �࠭���� ��ࠬ���� */
            vBracketL      = INDEX(ttPrmPog.grpar, "(")
            vBracketR      = INDEX(ttPrmPog.grpar, ")")
         .
         IF vBracketL GT 0 AND vBracketR GT 0 THEN
            ASSIGN
               ttPrmPog.grext = SUBSTRING (ttPrmPog.grpar, vBracketL + 1, vBracketR - vBracketL - 1)
               ttPrmPog.grpar = SUBSTRING (ttPrmPog.grpar, 1, vBracketL - 1)
            .
         {additem.i vPosCntrPrm ttPrmPog.grpar} /* ᯨ᮪ ��ࠬ��஢ ��� ࠧ������� �� ��㯯� */
      END.
       
      /* ���� ������� */
      FIND FIRST loan WHERE loan.contract  EQ iContract
                        AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN LEAVE MAIN_BLOCK.
      ASSIGN
         vContType = loan.cont-type   /* ⨯ ������� */
         vDate     = iDate
         oResDate  = iDate
      NO-ERROR.
      
      IF SESSION:DEBUG-ALERT THEN 
      DO:
         {putfile.i &nodir="/*" &file = "LN_GetPrsDateEx.log" 
                     &text = "'���� LN_GetPrsDateEx, �������:' + 
                                loan.contract + ',' + loan.cont-code"}
         {putfile.i  &of="/*" 
                     &text = "'���᮪ ��ࠬ��஢ ����窨:' + 
                                   iPosCntrPrmO"}
         
         {putfile.i   &of="/*" 
                     &text = "'��� �������:' + iPosCntrPrmT"}                                
               
      END.
               /* � ��㯯�஢��� ��ࠬ��஢ �१ ";", � �⮬ ��砥
               ** ����塞 ������ �⤥�쭮 ��� ������ ��㯯�  */
      FOR EACH ttPrmPog:
         ASSIGN
            ttPrmPog.summPog = LN_GetParamsInteres (loan.contract,
                                                    loan.cont-code,
                                                    ttPrmPog.grpar,
                                                    iDate)
            vSumParam = vSumParam + ttPrmPog.summPog
         .
      END.

      IF vContType EQ "��祭��" THEN
         FOR EACH bloan WHERE bloan.contract  EQ iContract
                          AND bloan.cont-code BEGINS iContCode + " "
                 AND NUM-ENTRIES(bloan.cont-code," ") EQ 2
         NO-LOCK,
         EACH ttPrmPog:
            ASSIGN
               ttPrmPog.summPog = ttPrmPog.summPog + 
                                  LN_GetParamsInteres (bloan.contract,
                                                       bloan.cont-code,
                                                       ttPrmPog.grpar,
                                                       iDate)
               vSumParam = vSumParam + ttPrmPog.summPog
            .
         END.

      IF vSumParam LE 0 THEN
      DO:
         oResDate = iDate.
         LEAVE MAIN_BLOCK.
      END.
      ELSE 
         oProcFl = YES.
      
      IF SESSION:DEBUG-ALERT THEN 
      DO:
         {putfile.i   &of="/*" 
                     &text = "'����ﭨ� �� ��砫� �������: '"}
         FOR EACH ttPrmPog:
            {putfile.i  &of="/*" 
                        &text = "' ��ࠬ���� ����窨:' + ttPrmPog.grpar + 
                                 ' �㬬�:' + STRING(ttPrmPog.summPog)"}                               
         END.
      
      END.

      ASSIGN
         vQry = "~
         FOR EACH loan-int WHERE (
                  loan-int.contract    EQ '" + iContract + "'  ~
            AND   loan-int.cont-code   EQ '" + iContCode + "' ~
            AND   loan-int.mdate       LE " + STRING(iDate) + "  ~
            AND  (CAN-DO('" + vPosCntrPrm + "', STRING (loan-int.id-d)) ~
               OR CAN-DO('" + vPosCntrPrm + "', STRING (loan-int.id-k))))"
         vQry = vQry + IF vContType NE "��祭��" 
         THEN " "
         ELSE (" OR (~
                  loan-int.contract    EQ '" + iContract + "'  ~
            AND   loan-int.cont-code   BEGINS '" + iContCode + " ' ~
            AND   loan-int.mdate       LE " + STRING(iDate) + "  ~
            AND  (CAN-DO('" + vPosCntrPrm + "', STRING (loan-int.id-d)) ~
               OR CAN-DO('" + vPosCntrPrm + "', STRING (loan-int.id-k))) ~
                  ) ")
         vQry = vQry + " NO-LOCK BREAK BY loan-int.mdate DESCENDING"
      .
         
      CREATE QUERY vHQry.
      vHQry:SET-BUFFERS(BUFFER loan-int:HANDLE).
      vHQry:QUERY-PREPARE(vQry).
      vHQry:QUERY-OPEN().
/* vHQry:GET-FIRST(). �� ���㬥��樨 ������ ����� �.�. ���� ᮤ�ন� BREAK BY
� �⠭������ FORWARD-ONLY */

      BLCK:
      REPEAT:
         vHQry:GET-NEXT().
         IF vHQry:QUERY-OFF-END THEN LEAVE.
      
         FOR EACH ttPrmPog:
               
            CASE iPosCntrPrmT:
                  /* ��� ���� */
               WHEN "fifo" THEN
               DO:
                     /* �� ���뢠�� ��ॡ��� ����� 
                        ��ࠬ��ࠬ� ����� ��㯯� */
                  IF         CAN-DO(ttPrmPog.grpar, 
                                    STRING (loan-int.id-d)) 
                     AND NOT CAN-DO(ttPrmPog.grpar, 
                                    STRING (loan-int.id-k))
                     /* ��� �� �࠭���� ��ࠬ��� 
                        �ᯮ����騩�� ��� ����⠭������� �� 
                        ������ ����窨 �� ���襭�� �� */
                     AND NOT CAN-DO(ttPrmPog.grext, 
                                    STRING (loan-int.id-k))
                  THEN
                     ttPrmPog.summPog = ttPrmPog.summPog - 
                                        loan-int.amt-rub.
               END.
                  /* ��� ������������ */
               WHEN "cont" THEN
               DO:
                  IF CAN-DO(ttPrmPog.grpar, STRING (loan-int.id-d)) THEN
                     ttPrmPog.summPog = ttPrmPog.summPog - 
                                        loan-int.amt-rub.
                  IF CAN-DO(ttPrmPog.grpar, STRING (loan-int.id-k)) THEN
                     ttPrmPog.vunos = ttPrmPog.vunos +  loan-int.amt-rub.
               END.
                  /* ��� �� ���� */
               OTHERWISE
               DO: 
                  IF CAN-DO(ttPrmPog.grpar, STRING (loan-int.id-d)) THEN
                     ttPrmPog.summPog = ttPrmPog.summPog - 
                                        loan-int.amt-rub.
                  IF CAN-DO(ttPrmPog.grpar, STRING (loan-int.id-k)) THEN
                     ttPrmPog.summPog = ttPrmPog.summPog + 
                                        loan-int.amt-rub.
               END.
            END CASE.
         END.
         
         IF SESSION:DEBUG-ALERT THEN 
         DO:
            {putfile.i  &of="/*" 
               &text = "'�������������������[ ������ �஢���� ]������������������Ŀ'"}
            {putfile.i  &of="/*" 
               &text = "'���:' + STRING(loan-int.id-d) + 
                        ' ��:' + STRING(loan-int.id-k) + 
                        ' ���:' + STRING(loan-int.mdate) + 
                        ' �㬬a:' + STRING(loan-int.amt-rub)"}               
            FOR EACH ttPrmPog:                   
               {putfile.i  &of="/*" 
                  &text = "' ��ࠬ���� ����窨:' + ttPrmPog.grpar + 
                          ' �㬬�:' + STRING(ttPrmPog.summPog)"}
            END.
         
            {putfile.i &of="/*"  
               &text = "'����������������������������������������������������������'"}
         END.
         
            /* � ���� ������� ��� �஢��塞 ���⪨ */
         IF vHQry:LAST-OF(1) THEN
         DO:
            vDate = loan-int.mdate.
            IF SESSION:DEBUG-ALERT THEN
            DO: 
               {putfile.i  &of="/*" 
                  &text = "' ����� ���: ' + STRING(vDate)"}
            END.
                     
            FOR EACH ttPrmPog:
                
               IF ttPrmPog.summPog LE 0 THEN
               DO:
                  IF SESSION:DEBUG-ALERT THEN
                  DO: 
                     {putfile.i  &of="/*" 
                        &text = "' ������� ��㯯�:' 
                        + ttPrmPog.grpar + ' c㬬�:' + STRING(ttPrmPog.summPog)"}
                  END.                        
                  DELETE ttPrmPog.
               END.
               ELSE 
                  IF iPosCntrPrmT EQ "cont" THEN
                  DO:
                     ttPrmPog.summPog = ttPrmPog.summPog + ttPrmPog.vunos.
                     
                     IF ttPrmPog.vunos NE 0 THEN
                        IF SESSION:DEBUG-ALERT THEN
                        DO: 
                           {putfile.i  &of="/*" 
                              &text = "' ���४�஢�� ��㯯� :' 
                              + ttPrmPog.grpar + ' �㬬�:' + STRING(ttPrmPog.summPog) + 
                              '(+' + STRING(ttPrmPog.vunos) + ')'"}
                        END.
                     
                     ttPrmPog.vunos = 0.
                  END.
            END.
            
            IF NOT CAN-FIND (FIRST ttPrmPog) THEN
            DO:
                  IF iCurrDay THEN vFlag = YES.
                  LEAVE BLCK.
            END.
         END.
      END. /* REPEAT BLCK */            
      vHQry:QUERY-CLOSE().
      DELETE OBJECT vHQry.

      oResDate = MIN(oResDate,vDate).
      {empty ttPrmPog} 
         /* ���뢠� ����窨 ������訥 �� ����樨 */
      vDateM    = DATE(FGetSettingMF("��⠍��।",?,?,loan.filial-id, no)).
      IF     vDateM   NE ?
         AND oResDate EQ vDateM THEN
      DO:
         ASSIGN
            vParam    = GetXAttrValueEx("loan",
                                        loan.contract + "," + loan.cont-code,
                                        "over-date_pers",
                                        ?)
            vDatePrP  = ( IF {assigned vParam} THEN date(vParam) ELSE vDateM)
            vParam    = GetXAttrValueEx("loan",
                                        loan.contract + "," + loan.cont-code,
                                        "over-date_loan",
                                        ?)
            vDatePr   = ( IF {assigned vParam} THEN date(vParam) ELSE vDateM)
         .
            /* �᫨ �㦭� ⮫쪮 %% */
         IF     GetSysConf("����窠%%") EQ "��"
            AND vDatePrP                  NE ? THEN
            oResDate = vDatePrP.
            /* �᫨ �㦭� ⮫쪮 �� */
         ELSE IF GetSysConf("����窠��") EQ "��"
            AND vDatePr                    NE ? THEN
            oResDate = vDatePr.
            /* �᫨ ���� ����窠 */
         ELSE
         DO:
            IF vDatePr NE ? THEN
               oResDate = vDatePr.
            IF vDatePrP NE ? THEN
               oResDate = MIN(oResDate,vDatePrP).
         END.
      END.
         /* �᫨ �� "���������" = ��, � �����頥� ���譨� ����. */
      IF vFlag /* iCurrDay AND oResDate EQ iDate */ THEN 
         oResDate = oResDate - 1.
   END. /* MAIN_BLOCK */
   
   /* ����뢠�� ��⮬ �⫠��� */
   IF SESSION:DEBUG-ALERT THEN
      OUTPUT CLOSE.
        

END PROCEDURE.

/*===========================================================================*/
/*=== ��ਮ� �� ����� ���᫥�� ��業�� ==================================*/
/*===========================================================================*/
FUNCTION LN_GetProcPeriod RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vEnd  AS DATE NO-UNDO.
   DEF VAR vBeg  AS DATE NO-UNDO.
   DEF VAR vS    AS CHAR NO-UNDO.
   DEF VAR vI    AS INT64  NO-UNDO.
   DEF VAR vDate AS DATE NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER loan     FOR loan.
   DEF BUFFER loan-int FOR loan-int.
   DEF BUFFER chowhe   FOR chowhe.
   DEF BUFFER b-term-obl FOR term-obl.

   RUN RE_FIRST_TERM_OBL IN h_Loan (iContract,iContCode,1,iDate,BUFFER term-obl).

   RUN RE_B_LOAN IN h_Loan (iContract,iContCode,BUFFER loan).

   vS = "65,16,35,83,84,96,97,98,99,304,308,315,10".

   IF AVAIL term-obl THEN
      vEnd = term-obl.end-date.
   ELSE
   DO:
      FIND FIRST b-term-obl WHERE
                 b-term-obl.contract  = loan.contract
             AND b-term-obl.cont-code = loan.cont-code
             AND b-term-obl.idnt      = 1
      NO-LOCK NO-ERROR.
      vEnd = IF AVAIL b-term-obl THEN iDate
                                 ELSE LastMonDate(iDate).
   END.

   vDate = ?.

   DO vI = 1 TO NUM-ENTRIES (vS):
      /* �饬 ��᫥���� ������ */
      FIND FIRST chowhe WHERE chowhe.id-op EQ INT64(ENTRY(vI, vS)) NO-LOCK NO-ERROR.
      IF AVAIL chowhe THEN
      DO:
         FIND LAST loan-int OF loan WHERE
                loan-int.cont-code = loan.cont-code
            AND loan-int.contract  = loan.contract
            AND loan-int.mdate     < iDate
            AND loan-int.id-d      = chowhe.id-d
            AND loan-int.id-k      = chowhe.id-k
            NO-LOCK NO-ERROR.
         IF AVAIL loan-int THEN
            IF vDate EQ ? OR loan-int.mdate > vDate THEN vDate = loan-int.mdate.
      END.
   END.

   /* �饬 �������� ���� ���⥦� �� ��業⠬ */
   RUN RE_TERM_OBL (loan.contract,  /*�����祭�� �������*/
                    loan.cont-code, /*��� �������*/
                    1,              /*⨯ ���⪠ �ᥣ�� 1  �� �ᯮ������*/
                    iDate - 1,      /*�������� ��� ���㬥��*/
                    BUFFER term-obl).

   IF AVAIL term-obl THEN
      IF vDate EQ ? OR term-obl.end-date > vDate THEN vDate = term-obl.end-date.

   IF vDate EQ ? THEN
      vDate = loan.open-date.

   IF NOT AVAIL term-obl AND NOT AVAIL b-term-obl THEN
      vDate = MAXIMUM( vDate, LastMonDate(GoMonth(iDate, -1)) ).

   RETURN (vEnd - vDate).
END.
/*===========================================================================*/
/*=== ��� ��᫥���� �஫����樨 ============================================*/
/*===========================================================================*/
FUNCTION LN_LastProlDate RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF BUFFER pro-obl FOR pro-obl.

   FOR EACH pro-obl WHERE
            pro-obl.contract  = iContract
        AND pro-obl.cont-code = iContCode
        AND pro-obl.idnt      = 3
        AND pro-obl.pr-date  <= iDate
   NO-LOCK BY pro-obl.pr-date DESC:

      RETURN pro-obl.pr-date.

   END.

   RETURN ?.

END FUNCTION.
/*===========================================================================*/
/*=== �����襭� �� ����� �� �������� (0+7+13) = 0 =========================*/
/*===========================================================================*/
FUNCTION LN_IsNullDolg RETURNS LOG
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   {&FIND_LOAN} ?.

   IF loan.since < iDate THEN RETURN NO.

   RETURN (LN_GetParams(iContract,iContCode,"0,7,13",iDate) = 0).

END FUNCTION.
/*===========================================================================*/
/*=== �६����� ���� ��� ��業��� ���᫥��� ==============================*/
/*===========================================================================*/
FUNCTION LN_ProcentBase RETURNS INT64
   (iContract AS CHAR,
    iContCode AS CHAR,
    iDate     AS DATE):

   DEF VAR vClass    AS CHAR  NO-UNDO.
   DEF VAR vCurr     AS CHAR  NO-UNDO.
   DEF VAR vProc     AS CHAR  NO-UNDO.
   DEF VAR vPrms     AS CHAR  NO-UNDO.
   DEF VAR vSchRecid AS RECID NO-UNDO.

   DEF BUFFER loan-cond         FOR loan-cond.
   DEF BUFFER interest-sch-line FOR interest-sch-line.

   vClass = GetBufferValue ("loan",
                            "WHERE loan.contract EQ '" + iContract + "'
                               AND loan.cont-code EQ '" + iContCode + "'",
                            "class-code,currency").

   IF vClass = ? THEN RETURN ?.

   ASSIGN
      vCurr  = ENTRY(2,vClass,CHR(2))
      vClass = ENTRY(1,vClass,CHR(2))
      .

   RUN RE_L_COND IN h_Loan (iContract,iContCode,iDate,BUFFER loan-cond).

   IF NOT AVAIL loan-cond THEN RETURN ?.

   RUN GetClassMethod IN h_Xclass (vClass,"nach-proc","","",
                                   OUTPUT vProc,OUTPUT vPrms).

   IF (NOT {assigned vProc}) OR NOT SearchPFile(vProc) THEN vProc = "nach-pp".

   IF vProc = "nach-pp" OR vProc = "nach-pos" THEN
   DO:
      IF loan-cond.disch-type = 0  OR
         loan-cond.disch-type = 10 OR
         loan-cond.disch-type = 12 OR
         loan-cond.disch-type = 14 OR
         loan-cond.disch-type = 15 OR
         loan-cond.disch-type = 16 OR
         loan-cond.disch-type = 2  OR
        (loan-cond.disch-type >= 19  AND
         loan-cond.disch-type <  30)
      THEN
         vPrms = "360".
      ELSE IF loan-cond.disch-type = 40
         THEN vPrms  = "365".
         ELSE
            vPrms = "ACT".
   END.
   ELSE
   DO:
      vPrms = IF vProc = "lnscheme"
              THEN "��_" + STRING(loan-cond.disch-type)
              ELSE "��1_" + STRING(loan-cond.disch-type).

      RUN get_currenct_scheme IN h_schem
             (vPrms,
              "0",
              vCurr,
              iContract + "," + iContCode,
              iDate,
              OUTPUT vSchRecid).

      RUN GET_SCH_LINE_BY_RID IN h_schem
            (vSchRecid,
             BUFFER interest-sch-line).

      vProc = STRING(interest-sch-line.basis-time).
   END.

   RETURN ( IF vPrms = "ACT"
           THEN ( IF MONTH(DATE(02,28,YEAR(iDate)) + 1) <> 2
                 THEN 365
                 ELSE 366)
           ELSE INT64(vPrms)).

END FUNCTION.
/*===========================================================================*/
/*===========================================================================*/
/*===========================================================================*/
/*
FUNCTION LN_GetLoanComm RETURNS DEC
   (iContract AS CHAR,
    iContCode AS CHAR,
    iCurr     AS CHAR,
    iDate     AS DATE,
    iCommi    AS CHAR):

END FUNCTION.
*/
/*===========================================================================*/
/*=== �뢮� ���祭�� � ��⮬ �ଠ� � �᫨ ���祭��  ���⮥ ��� ������, ===*/
/*=== � ���� �뢮��� ������ ��ப� =======================================*/
/*===========================================================================*/
FUNCTION prDEC RETURNS CHAR
   (iDec    AS DEC,  /* ���祭�� */
    iFormat AS CHAR, /* ��ଠ�   */
    iEmpty  AS LOG):

   RETURN(IF (iEmpty AND iDec = 0.00) OR iDec = ?
          THEN FILL(" ",LENGTH(iFormat))
          ELSE STRING(iDec,iFormat)).

END FUNCTION.

FUNCTION prINT RETURNS CHAR
   (iInt    AS INT64,  /* ���祭�� */
    iFormat AS CHAR, /* ��ଠ�   */
    iEmpty  AS LOG):

   RETURN(IF (iEmpty AND iInt = 0) OR iInt = ?
          THEN FILL(" ",LENGTH(iFormat))
          ELSE STRING(iInt,iFormat)).

END FUNCTION.

FUNCTION prDATE RETURNS CHAR
   (iDate   AS DATE,  /* ���祭�� */
    iFormat AS CHAR,  /* ��ଠ�   */
    iEmpty  AS LOG):

   RETURN(IF iEmpty AND iDate = ?
          THEN FILL(" ",LENGTH(iFormat))
          ELSE STRING(iDate,iFormat)).

END FUNCTION.
/*===========================================================================*/
/*=== ��� ���짮��⥫� ======================================================*/
/*===========================================================================*/
FUNCTION LN_GetUserName RETURNS CHAR
   (iUserId AS CHAR):

   FIND FIRST _User WHERE _User._Userid = iUserId NO-LOCK NO-ERROR.

   RETURN ( IF AVAIL _User
           THEN  _User._User-Name
           ELSE  "��� ⠪��� ���짮��⥫�").
END FUNCTION.

/*====================================================================================*/
/* �����頥� �㬬� ��९���� �� �������� �� ��ਮ�, ������ �� ������ ࠭�� �ப�. */
/*====================================================================================*/
PROCEDURE Ln_BefPogLoan.
   DEF INPUT  PARAM  iBegDate AS DATE   NO-UNDO.
   DEF INPUT  PARAM  iEndDate AS DATE   NO-UNDO.
   DEF PARAM  BUFFER loan     FOR loan.
   DEF INPUT  PARAM  iCrOpEn  AS LOG    NO-UNDO.
   DEF OUTPUT PARAM  oSumPog  AS DEC    NO-UNDO.

   DEF VAR vSumBeg     AS DEC   NO-UNDO. /* ������襭�� ���⮪ ��������� ���⥦� */
   DEF VAR vSumPlanOst AS DEC   NO-UNDO. /* �㬬� ������襭���� ���⪠ */
   DEF VAR vSumPlanCur AS DEC   NO-UNDO. /* ���� �㬬� ������襭���� ���⪠ �� ��砫� ���� */
   DEF VAR vSumPogCur  AS DEC   NO-UNDO.
   DEF VAR vSumPogRub  AS DEC   NO-UNDO.
   DEF VAR vDate       AS DATE  NO-UNDO. /* ��� ��� �த������� ����� */
   DEF VAR vNewYear    AS DATE  NO-UNDO. /* ��� ��砫� ���⭮�� ��ਮ�� 1 ﭢ��� ... */
   DEF VAR vLastDate   AS DATE  NO-UNDO. /*  ��� ��᫥����� 䠪��᪮�� ���⥦� �� ����� ��ਮ� */
   DEF VAR vDate1      AS DATE  NO-UNDO.
   DEF VAR vDate2      AS DATE  NO-UNDO.
   DEF VAR vHistSince  AS DATE  NO-UNDO.
   DEF VAR vSumOb      AS DEC   NO-UNDO.
   DEF VAR vDolg       AS DEC   NO-UNDO.
   DEF VAR vNoper      AS INT64 NO-UNDO.
   DEF VAR vOpPogOD50  AS CHAR  NO-UNDO.
   DEF VAR vListDb     AS CHAR  NO-UNDO.
   DEF VAR vListCr     AS CHAR  NO-UNDO.
   DEF VAR vNumDb      AS INT64 NO-UNDO.
   DEF VAR vNumCr      AS INT64 NO-UNDO.
   DEF VAR vCounter    AS INT64 NO-UNDO.
   DEF VAR vTypeCond   AS CHAR  NO-UNDO. /* ⨯ �᫮��� */
   DEF VAR vSurrCond   AS CHAR  NO-UNDO. /* ���ண�� �᫮��� */
   DEF VAR vSumPerepl  AS DEC   NO-UNDO. /* �㬬� ��९���� */

   DEF BUFFER loan-int       FOR loan-int.
   DEF BUFFER bloan-int      FOR loan-int.
   DEF BUFFER chowhe         FOR chowhe.
   DEF BUFFER xterm-obl      FOR term-obl.
   DEF BUFFER yterm-obl      FOR term-obl.
   DEF BUFFER bterm-obl-hist FOR term-obl-hist.
   DEF BUFFER loan-cond      FOR loan-cond.

   IF NOT AVAIL loan THEN LEAVE.

   vNewYear = iBegDate.

   /* ������ �࠭�� ��ਮ�� ������ ���� �� ࠭�� ���� ������ ������� */
   vDate = IF loan.open-date GT iBegDate
                        THEN loan.open-date
                        ELSE iBegDate.
&IF DEFINED(IS-DEBUG) <> 0 &THEN
   OUTPUT TO "cdrep.log" APPEND.
   PUT UNFORMATTED
      "PROCEDURE Ln_AllBefPogLoan: " SKIP
      "������� " loan.cont-code " ��ਮ� ���� � " STRING(vDate) " �� " STRING(iEndDate) SKIP
   .
   OUTPUT CLOSE.
&ENDIF

   vOpPogOD50 = FGetSetting("��ଠ316", "���������50", "50").
   IF {assigned vOpPogOD50} THEN
   LOOP_50:
   DO vCounter = 1 TO NUM-ENTRIES(vOpPogOD50):
      FIND FIRST chowhe WHERE
                 chowhe.id-op = INT64(ENTRY(vCounter, vOpPogOD50))
      NO-LOCK NO-ERROR.
      IF NOT AVAIL chowhe THEN
         NEXT LOOP_50.
      {additem.i vListDb STRING(chowhe.id-d)}
      {additem.i vListCr STRING(chowhe.id-k)}
   END.
&IF DEFINED(IS-DEBUG) NE 0 &THEN
   OUTPUT TO "cdrep.log" APPEND.
   PUT UNFORMATTED
      "PROCEDURE Ln_AllBefPogLoan: " SKIP
      "������� " loan.cont-code " ��ਮ� ���� � " STRING(vDate) " �� " STRING(iEndDate) SKIP.
   OUTPUT CLOSE.
&ENDIF

      /*----------------------------------------------------------------------------------------------------------------------*/ 
      /*������ ��䨪 � ���ਨ �� ��砫� ��ਮ�� */
   FIND LAST bterm-obl-hist WHERE
             bterm-obl-hist.contract  EQ loan.contract
         AND bterm-obl-hist.cont-code EQ loan.cont-code
         AND bterm-obl-hist.idnt      EQ 3
         AND bterm-obl-hist.olap      EQ "1"
         AND bterm-obl-hist.since     LE vDate
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bterm-obl-hist THEN 
   DO:
      /*�᫨ ���室�饣� ��䨪� ���, � �ᯮ��㥬 ⥪�騩 ��䨪 */
      T-OBL:
      FOR EACH term-obl  WHERE
               term-obl.contract  EQ loan.Contract
           AND term-obl.cont-code EQ loan.Cont-Code
           AND term-obl.idnt      EQ 3
      NO-LOCK 
      BY term-obl.end-date:
         CREATE tt-hist-amt.
         BUFFER-COPY term-obl TO tt-hist-amt.
            /* ����室��� ᮡ���� ��䨪� �� ���� ���� 
               ���� �� ����, �⮡� ����� ����襭�� � 
               ��᫥���� ���� ��������� ����襭�� */
         IF term-obl.end-date GT iEndDate THEN
            LEAVE T-OBL.
      END.
      &IF DEFINED(IS-DEBUG) <> 0 &THEN
         OUTPUT TO "cdrep.log" APPEND.
         PUT UNFORMATTED
            "������� " loan.cont-code " �ᯮ������ ⥪�騩 ��䨪 "
         SKIP.
         OUTPUT CLOSE.
      &ENDIF
   END.
   ELSE  /* ��䨪 ������ */ 
   DO:
      T-OBL_HIST:
      FOR EACH tobl-hist-amt WHERE
               tobl-hist-amt.tobl-id  EQ bterm-obl-hist.tobl-id
      NO-LOCK
      BY tobl-hist-amt.end-date:
         CREATE tt-hist-amt.
         BUFFER-COPY tobl-hist-amt TO tt-hist-amt.
         vHistSince = bterm-obl-hist.since.
            /* ����室��� ᮡ���� ��䨪� �� ���� ���� 
               ���� �� ����, �⮡� ����� ����襭�� � 
               ��᫥���� ���� ��������� ����襭�� */
         IF tobl-hist-amt.end-date GT iEndDate THEN
            LEAVE T-OBL_HIST.
      END.
      &IF DEFINED(IS-DEBUG) <> 0 &THEN
         OUTPUT TO "cdrep.log" APPEND.
         PUT UNFORMATTED
            "������� " loan.cont-code " ������ ��䨪 � "
            string(bterm-obl-hist.tobl-id)
            " �� " string(bterm-obl-hist.since) " "
            bterm-obl-hist.description
         SKIP.
         OUTPUT CLOSE.
      &ENDIF
   END.

/*----------------------------------------------------------------------------------------------------------------------*/
   vDate1 = loan.open-date.
   vNoper = 0.
      /* ��室�� �������������/��९���� �� ��砫� ��ਮ�� */
   IF YEAR(loan.open-date) LT YEAR(iBegDate) THEN
   DO:
      vDate2 = iBegDate - 1.
      LOOP_TO:
      FOR EACH tt-hist-amt WHERE
               tt-hist-amt.end-date LE vDate2
      NO-LOCK:
         vDolg = vDolg + tt-hist-amt.amt-rub.
      END.
      FOR EACH loan-int WHERE
               loan-int.contract  EQ loan.contract
           AND loan-int.cont-code EQ loan.cont-code
           AND loan-int.mdate     LE vDate2
      NO-LOCK:
         IF {assigned vOpPogOD50} THEN
         BLK_50:
         DO:
               /* �஢��塞 ������ �� ᯨ�� �� �� ���������50 */
            vNumDb = LOOKUP (STRING(loan-int.id-d), vListDb).
            IF vNumDb EQ 0 THEN
               LEAVE BLK_50.
            vNumCr = LOOKUP (STRING(loan-int.id-k), vListCr).
            IF vNumCr EQ 0 THEN
               LEAVE BLK_50.
            IF vNumDb NE vNumCr THEN
               LEAVE BLK_50.
            vDolg = vDolg - loan-int.amt-rub.
         END.
            /* 5 ������ */
         IF loan-int.id-d  EQ 1 AND loan-int.id-k EQ 2 THEN
         DO:
            vDolg = vDolg - loan-int.amt-rub.
         END.
      END.
      vDate1 = vDate2 + 1.
   END.
      /* �����뢠�� ��९���� */
   IF NOT mNew316 THEN
   DO:
      LOOP_TO:
      FOR EACH tt-hist-amt WHERE
               tt-hist-amt.end-date GE vDate1 /* �᫨ ����-� ��頥�, �� ��६����� �ᯮ������ ����� �� �������� �� ����� �� �� ����� */
      NO-LOCK:
         vNoper = vNoper + 1.
         IF tt-hist-amt.end-date GT iEndDate THEN
            vDate2 = iEndDate.
         ELSE
            vDate2 = tt-hist-amt.end-date - 1.
         FOR EACH loan-int WHERE
                  loan-int.contract  EQ loan.contract
              AND loan-int.cont-code EQ loan.cont-code
              AND loan-int.mdate     GE vDate1
              AND loan-int.mdate     LE vDate2
         NO-LOCK:
            IF {assigned vOpPogOD50} THEN
            BLK_50:
            DO:
                  /* �஢��塞 ������ �� ᯨ�� �� �� ���������50 */
               vNumDb = LOOKUP (STRING(loan-int.id-d), vListDb).
               IF vNumDb EQ 0 THEN
                  LEAVE BLK_50.
               vNumCr = LOOKUP (STRING(loan-int.id-k), vListCr).
               IF vNumCr EQ 0 THEN
                  LEAVE BLK_50.
               IF vNumDb NE vNumCr THEN
                  LEAVE BLK_50.
               vDolg = vDolg  - loan-int.amt-rub.
            END.
               /* 5 ������ */
            IF loan-int.id-d  EQ 1 AND loan-int.id-k EQ 2 THEN
            DO:
               IF NOT CAN-FIND (FIRST bloan-int WHERE
                                      bloan-int.op        EQ loan-int.op
                                  AND bloan-int.op-entry  EQ loan-int.op-entry
                                  AND bloan-int.contract  EQ loan.contract
                                  AND bloan-int.cont-code EQ loan.cont-code
                                  AND bloan-int.id-d      EQ 0
                                  AND bloan-int.id-k      EQ 3
                                NO-LOCK) THEN
               DO:
                  vDolg = vDolg  - loan-int.amt-rub.
                  IF vDolg LT 0 THEN
                  DO:
                     /* �஢�ઠ, �� ������ �� ���� ��७�ᮬ ������������ � �裡 � ᬥ��� ��� */
                     ASSIGN
                        vSumPogCur = MIN(loan-int.amt-rub, - vDolg)
                        vDolg      = 0
                        vSumPogRub = CurToBase("����", 
                                               loan.currency, 
                                               loan-int.mdate, 
                                               vSumPogCur)  WHEN loan.currency NE ""
                        vSumPogRub = vSumPogCur             WHEN loan.currency EQ ""
                     .
                     IF iCrOpEn THEN
                     DO:
                        CREATE tt-OpEntry.
                        ASSIGN
                           tt-OpEntry.op        = loan-int.op
                           tt-OpEntry.op-entry  = loan-int.op-entry
                           tt-OpEntry.op-date   = loan-int.op-date
                           tt-OpEntry.amt-cur   = vSumPogCur
                           tt-OpEntry.amt-rub   = vSumPogRub
                           tt-OpEntry.currency  = loan.currency
                           tt-OpEntry.contract  = loan.contract
                           tt-OpEntry.cont-code = loan.cont-code
                           oSumPog              = oSumPog + tt-OpEntry.amt-rub
                        .
                     END.
                     oSumPog = oSumPog + vSumPogRub.
                  END.
               END.
            END.
         END.
         IF tt-hist-amt.end-date GT iEndDate THEN
            LEAVE LOOP_TO.
         vDate1 = vDate2 + 1.
         vDolg = vDolg + tt-hist-amt.amt-rub.
         
         IF vHistSince NE ? THEN 
              /* 㬥��蠥� �㬬� ��易⥫��⢠ �� �㬬� ��������� ����筮�� ����襭�� �� �᫮��� */
              /* �饬, ��� �� �᫮��� ����筮�� ����襭�� �� ���� �� */
            FIND FIRST loan-cond WHERE loan-cond.contract  EQ loan.contract
                                   AND loan-cond.cont-code EQ loan.cont-code
                                   AND loan-cond.since     EQ tt-hist-amt.end-date
                                   AND loan-cond.since     LE vHistSince

            NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST loan-cond WHERE loan-cond.contract  EQ loan.contract
                                   AND loan-cond.cont-code EQ loan.cont-code
                                   AND loan-cond.since     EQ tt-hist-amt.end-date
            NO-LOCK NO-ERROR.

         IF AVAIL loan-cond THEN
         DO:
            ASSIGN
               vSurrCond = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
               vTypeCond = GetXattrValueEx("loan-cond",
                                           vSurrCond,
                                           "PayType",
                                           "")
            .
               /* �᫮��� � ������ ����襭��� - ��।��塞 �� ����� �� ���� */
            IF vTypeCond = "���������" THEN 
            BLK_POG:
            DO:
                 /* �㬬� ����筮�� ����襭�� */
               ASSIGN
                  vSumPerepl = DEC(GetXattrValueEx("loan-cond",
                                                   vSurrCond,
                                                   "PaySum",
                                                   ""))
               NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                  LEAVE BLK_POG.
               IF vSumPerepl GT 0 THEN
                  vDolg = vDolg - vSumPerepl.
            END.
         END.
      END.
         /* �᫨ �� �뫮 �� ������ ��������� ��ਮ�� (�ந����쭮� ����襭��) - ⮣�� �� ����襭�� �� ��ਮ� ����筮�  */
      IF vNoper = 0 THEN 
         FOR EACH loan-int WHERE
                  loan-int.contract  EQ loan.contract
              AND loan-int.cont-code EQ loan.cont-code
              AND loan-int.id-d      EQ 1
              AND loan-int.id-k      EQ 2
              AND loan-int.mdate     GE vDate1
              AND loan-int.mdate     LE iEndDate
         NO-LOCK:
            ASSIGN
               vSumPogCur = loan-int.amt-rub
               vSumPogRub = CurToBase("����", 
                                      loan.currency, 
                                      loan-int.mdate, 
                                      vSumPogCur)  WHEN loan.currency NE ""
               vSumPogRub = vSumPogCur             WHEN loan.currency EQ ""
            .
            IF iCrOpEn THEN
            DO:
               CREATE tt-OpEntry.
               ASSIGN
                  tt-OpEntry.op        = loan-int.op
                  tt-OpEntry.op-entry  = loan-int.op-entry
                  tt-OpEntry.op-date   = loan-int.op-date
                  tt-OpEntry.amt-cur   = vSumPogCur
                  tt-OpEntry.amt-rub   = vSumPogRub
                  tt-OpEntry.currency  = loan.currency
                  tt-OpEntry.contract  = loan.contract
                  tt-OpEntry.cont-code = loan.cont-code
                  oSumPog              = oSumPog + tt-OpEntry.amt-rub
               .
            END.
            oSumPog = oSumPog + vSumPogRub.
         END.
   END.
   ELSE
   DO:
      LOOP_TO:
      FOR EACH tt-hist-amt WHERE
               tt-hist-amt.end-date GE vDate1
           AND tt-hist-amt.end-date LE iEndDate
      NO-LOCK:
         vDolg = vDolg + tt-hist-amt.amt-rub.

         IF vHistSince NE ? THEN 
            /* 㬥��蠥� �㬬� ��易⥫��⢠ �� �㬬� ��������� ����筮�� ����襭�� �� �᫮��� */
            /* �饬, ��� �� �᫮��� ����筮�� ����襭�� �� ���� �� */
            FIND FIRST loan-cond WHERE loan-cond.contract  EQ loan.contract
                                   AND loan-cond.cont-code EQ loan.cont-code
                                   AND loan-cond.since     EQ tt-hist-amt.end-date
                                   AND loan-cond.since     LE vHistSince
            NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST loan-cond WHERE loan-cond.contract  EQ loan.contract
                                   AND loan-cond.cont-code EQ loan.cont-code
                                   AND loan-cond.since     EQ tt-hist-amt.end-date
            NO-LOCK NO-ERROR.
         IF AVAIL loan-cond THEN
         DO:
            ASSIGN
               vSurrCond = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
               vTypeCond = GetXattrValueEx("loan-cond",
                                           vSurrCond,
                                           "PayType",
                                           "")
            .
               /* �᫮��� � ������ ����襭��� - ��।��塞 �� ����� �� ���� */
            IF vTypeCond = "���������" THEN 
            BLK_POG:
            DO:
      
                 /* �㬬� ����筮�� ����襭�� */
               ASSIGN
                  vSumPerepl = DEC(GetXattrValueEx("loan-cond",
                                                   vSurrCond,
                                                   "PaySum",
                                                   ""))
               NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                  LEAVE BLK_POG.
               IF vSumPerepl GT 0 THEN
                  vDolg = vDolg - vSumPerepl.
            END.
         END.
      END.
      FOR EACH loan-int WHERE
               loan-int.contract  EQ loan.contract
           AND loan-int.cont-code EQ loan.cont-code
           AND loan-int.mdate     GE vDate1
           AND loan-int.mdate     LE iEndDate
      NO-LOCK:
         IF {assigned vOpPogOD50} THEN
         BLK_50:
         DO:
               /* �஢��塞 ������ �� ᯨ�� �� �� ���������50 */
            vNumDb = LOOKUP (STRING(loan-int.id-d), vListDb).
            IF vNumDb EQ 0 THEN
               LEAVE BLK_50.
            vNumCr = LOOKUP (STRING(loan-int.id-k), vListCr).
            IF vNumCr EQ 0 THEN
               LEAVE BLK_50.
            IF vNumDb NE vNumCr THEN
               LEAVE BLK_50.
            vDolg = vDolg  - loan-int.amt-rub.
         END.
            /* 5 ������ */
         IF loan-int.id-d  EQ 1 AND loan-int.id-k EQ 2 THEN
         DO:
            IF NOT CAN-FIND (FIRST bloan-int WHERE
                                   bloan-int.op        EQ loan-int.op
                               AND bloan-int.op-entry  EQ loan-int.op-entry
                               AND bloan-int.contract  EQ loan.contract
                               AND bloan-int.cont-code EQ loan.cont-code
                               AND bloan-int.id-d      EQ 0
                               AND bloan-int.id-k      EQ 3
                             NO-LOCK) THEN
            DO:
               vDolg = vDolg  - loan-int.amt-rub.
               IF vDolg LT 0 THEN
               DO:
                  /* �஢�ઠ, �� ������ �� ���� ��७�ᮬ ������������ � �裡 � ᬥ��� ��� */
                  ASSIGN
                     vSumPogCur = MIN(loan-int.amt-rub, - vDolg)
                     vDolg      = 0
                     vSumPogRub = CurToBase("����", 
                                            loan.currency, 
                                            loan-int.mdate, 
                                            vSumPogCur)  WHEN loan.currency NE ""
                     vSumPogRub = vSumPogCur             WHEN loan.currency EQ ""
                  .
                  IF iCrOpEn THEN
                  DO:
                     CREATE tt-OpEntry.
                     ASSIGN
                        tt-OpEntry.op        = loan-int.op
                        tt-OpEntry.op-entry  = loan-int.op-entry
                        tt-OpEntry.op-date   = loan-int.op-date
                        tt-OpEntry.amt-cur   = vSumPogCur
                        tt-OpEntry.amt-rub   = vSumPogRub
                        tt-OpEntry.currency  = loan.currency
                        tt-OpEntry.contract  = loan.contract
                        tt-OpEntry.cont-code = loan.cont-code
                        oSumPog              = oSumPog + tt-OpEntry.amt-rub
                     .
                  END.
                  oSumPog = oSumPog + vSumPogRub.
               END.
            END.
         END.
      END.
   END.
   {empty tt-hist-amt}
END PROCEDURE.

   /* ������ �㬬� ����筮�� ����襭�� �� ��ਮ� �� ���� �।��. ��� 1761-�. */
PROCEDURE Ln_BefPogByAcct.
   DEF INPUT  PARAM iAcct    AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iCurr    AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iType    AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iDateBeg AS DATE   NO-UNDO.
   DEF INPUT  PARAM iDateEnd AS DATE   NO-UNDO.
   DEF OUTPUT PARAM oSurr    AS CHAR   NO-UNDO.
   DEF OUTPUT PARAM oSumm    AS CHAR   NO-UNDO.
   DEF OUTPUT PARAM oCur     AS CHAR   NO-UNDO.
   DEF OUTPUT PARAM TABLE FOR tt-OpEntry.

   DEF BUFFER loan       FOR loan.
   DEF BUFFER loan-acct  FOR loan-acct.
   DEF BUFFER bloan-acct FOR loan-acct.
   DEF BUFFER xloan-acct FOR loan-acct.

   DEFINE VARIABLE vSum AS DECIMAL NO-UNDO .

   /* ���� ஫� ������� �� ���� � ��ਮ�� */
   /* �ࠧ� � �஢�મ� �易����� ������� �� ⨯�,
      �⮡� �� ������� �� ������� ���㦭��� ⨯�,
      ����� ��� �ਢ易� � ��᪮�쪨� ������ࠬ */
   {empty tt-OpEntry}
   oSumm = "".
   FOR EACH loan-acct WHERE loan-acct.acct     EQ iAcct
                        AND loan-acct.currency EQ iCurr
                        AND loan-acct.since    LE iDateEnd
   NO-LOCK,
   FIRST loan WHERE
         loan.contract  EQ loan-acct.contract
     AND loan.cont-code EQ loan-acct.cont-code
     AND CAN-DO(iType,loan.cont-type)
   NO-LOCK:
      /* ����� �࠭�� ���� ����⢨� */
      FIND LAST bloan-acct WHERE
                bloan-acct.contract  EQ loan-acct.contract
            AND bloan-acct.cont-code EQ loan-acct.cont-code
            AND bloan-acct.acct-type EQ loan-acct.acct-type
            AND bloan-acct.since     LT loan-acct.since
      NO-LOCK NO-ERROR.
      /* �ࠢ�� �࠭�� ���� ����⢨� */
      FIND FIRST xloan-acct WHERE
                 xloan-acct.contract  EQ loan-acct.contract
             AND xloan-acct.cont-code EQ loan-acct.cont-code
             AND xloan-acct.acct-type EQ loan-acct.acct-type
             AND xloan-acct.since     GT loan-acct.since
      NO-LOCK NO-ERROR.
      /* ��頥�, �⮡� ��������� ������ � Ln_AllBefPog */
      RUN Ln_BefPogLoan(MAX(IF AVAIL bloan-acct THEN  loan-acct.since     ELSE loan.open-date,iDateBeg),
                           MIN(IF AVAIL xloan-acct THEN xloan-acct.since - 1 ELSE loan.end-date, iDateEnd),
                           BUFFER loan,
                           YES,
                           OUTPUT vSum).
      ASSIGN
         oSumm = string( decimal(oSumm) + vSum)
         oSurr = loan.contract + "," + loan.cont-code
         oCur  = loan.currency
      .
   END.
   RETURN.
END PROCEDURE.

   /* =======================================================-===--==-= */
   /* �� ���� �ਭ�⮣� ���ᯥ祭��, �����뢠�� ���⮪ �� ��ࠬ���,
   ** ᮮ⢥�����饬 ஫� ��� ���ᯥ祭��, �� ���� ����           */
PROCEDURE GetCollatSinglByParam.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iAcct      AS CHAR NO-UNDO.  /* ��� ���ᯥ祭�� */
   DEF INPUT  PARAM iCurrency  AS CHAR NO-UNDO.  /* ����� */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.  /* ��� */
   DEF OUTPUT PARAM oSumm      AS DEC  NO-UNDO.  /* ���⮪ �� ��ࠬ��� */
   DEF OUTPUT PARAM oSpisParam AS CHAR NO-UNDO.  /* ���᮪ ��ࠬ��஢ �砢������ � ���� */

   DEF VAR vListOperDb AS CHAR NO-UNDO.   /* ���᮪ ����権 �� ������ */
   DEF VAR vListOperCr AS CHAR NO-UNDO.   /* ���᮪ ����権 �� �।��� */
   DEF VAR vListOper   AS CHAR NO-UNDO.   /* ��騩 ᯨ᮪ ����権 */
   DEF VAR vPrmDec     AS DEC  NO-UNDO.   /* �㬬� ���⪠ �� ��ࠬ��� */
   DEF VAR vDbSumDec   AS DEC  NO-UNDO.
   DEF VAR vCrSumDec   AS DEC  NO-UNDO.
   DEF VAR vParamCur   AS CHAR NO-UNDO.   /* ����� ���⪠ �� ��ࠬ��� */
   DEF VAR vI          AS INT64  NO-UNDO.   /* ���稪 */
      /* �������㥬 ���� */
   DEF BUFFER loan-acct FOR loan-acct.
      /* ��।��塞 � ����� ஫�� �ਢ易� � �������� ����� ��� */
   FIND LAST loan-acct WHERE
             loan-acct.contract  EQ iContract
      AND    loan-acct.cont-code EQ iContCode
      AND    loan-acct.acct      EQ iAcct
      AND    loan-acct.currency  EQ iCurrency
      AND    loan-acct.since     LE iDate
   NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
         /* ��।���� ᯨ᪨ ����権 ��� ������ ஫� ��� */
      RUN LN_GetRoleOper (loan-acct.acct-type,
                          OUTPUT vListOperDb,
                          OUTPUT vListOperCr).
      /* ��ꥤ���� ᯨ᪨ ����権 �� �� � �� � ���� */
   vListOper = vListOperDb + ( IF vListOperCr NE "" AND vListOperDb NE "" THEN "," ELSE "") + vListOperCr.
      /* ��।��塞 ᯨ᮪ ��ࠬ��஢ �� ᯨ᪮� ����権, �������� "?".
      ** ��� ��࠭⨩ � ��� ������஭��� ����樨.
      ** �᫨ ����⠭�� ���� ������஭����, � ��� ������ ����⠭�� ࠡ���� !!!*/
   DO vI = 1 TO NUM-ENTRIES(vListOper):
      FIND FIRST chowhe WHERE
                 chowhe.id-op EQ INT64(ENTRY(vI, vListOper))
      NO-LOCK NO-ERROR.
      IF AVAIL chowhe THEN
      DO:
         IF chowhe.id-d NE ?
            AND NOT CAN-DO(oSpisParam, STRING(chowhe.id-d)) THEN
            oSpisParam = oSpisParam + "," + STRING(chowhe.id-d).
         IF chowhe.id-k NE ?
            AND NOT CAN-DO(oSpisParam, STRING(chowhe.id-k)) THEN
            oSpisParam = oSpisParam + "," + STRING(chowhe.id-k).
      END.
   END.
      /* ����塞 ����� ������� */
   oSpisParam = SUBSTRING(oSpisParam, 2).
      /* �㬬��㥬 ���⪨ �� ��ࠬ���� */
   DO vI = 1 TO NUM-ENTRIES(oSpisParam):
         /* ����塞 ���⮪ �� ��ࠬ��� */
      RUN STNDRT_PARAM IN h_loan (iContract,
                                  iContCode,
                                  INT64(ENTRY(vI, oSpisParam)),
                                  iDate,
                                  OUTPUT vPrmDec,
                                  OUTPUT vDbSumDec,
                                  OUTPUT vCrSumDec).
         /* ��।���� � ����� ����� ��ࠬ��� */
      vParamCur = LN_GetParamCurr (iContract,
                                   iContCode,
                                   INT64(ENTRY(vI, oSpisParam)),
                                   loan-acct.currency,
                                   iDate).
         /* ��������㥬 ���⮪ ��ࠬ��� � ������ ��� ���ᯥ祭�� (�᫨ ����) */
      IF vParamCur NE iCurrency THEN
         vPrmDec = CurToCur ("�������",
                             vParamCur,
                             iCurrency,
                             iDate,
                             vPrmDec).
         /* "����ࠥ�" ���⪨ ��� ��ࠬ��஢ */
      oSumm = oSumm + vPrmDec.
   END.
END PROCEDURE.

   /* =======================================================-===--==-= */
   /* ��।���� �� �����䨪���� "����焮�", � ������ �����ﬨ
   ** �易�� ��।����� ஫� ���                                     */
PROCEDURE LN_GetRoleOper.
   DEF INPUT  PARAM iAcctType AS CHAR NO-UNDO.  /* ���� ��� */
   DEF OUTPUT PARAM oListDbOp AS CHAR NO-UNDO.  /* ���᮪ ����権 �� ������ */
   DEF OUTPUT PARAM oListCrOp AS CHAR NO-UNDO.  /* ���᮪ ����権 �� �।��� */
      /* �������㥬 ���� */
   DEF BUFFER code FOR code.
      /* ������� ᯨ᮪ ����権 �� ������ */
   FOR EACH code WHERE
            code.class  EQ     "����焮�"
      AND   code.code   BEGINS "��"
      AND   code.parent EQ     iAcctType
   NO-LOCK:
      oListDbOp = oListDbOp + "," + SUBSTRING(code.code, 3).
   END.
      /* ������� ᯨ᮪ ����権 �� �।��� */
   FOR EACH code WHERE
            code.class  EQ     "����焮�"
      AND   code.code   BEGINS "��"
      AND   code.parent EQ     iAcctType
   NO-LOCK:
      oListCrOp = oListCrOp + "," + SUBSTRING(code.code, 3).
   END.
      /* ����塞 ��譨� ������ */
   ASSIGN
      oListDbOp = SUBSTRING(oListDbOp, 2)
      oListCrOp = SUBSTRING(oListCrOp, 2)
   .
END PROCEDURE.

   /* =======================================================-===--==-= */
   /* ��।���� ᯨ᮪ ��⮢ ���ᯥ祭�� �������                     */
PROCEDURE GetCollatAcct.
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iSince    AS DATE NO-UNDO.
   DEF OUTPUT PARAM oListAcct AS CHAR NO-UNDO.  /* ���᮪ ��⮢ ���ᯥ祭�� ������� */

   DEF VAR vSurr   AS CHAR NO-UNDO. /* ���ண�� ��易⥫��⢠ */
   DEF VAR vVidDog AS CHAR NO-UNDO. /* ��� ������� ���ᯥ祭�� */
   DEF VAR vNumPP  AS CHAR NO-UNDO. /* � ������� ���ᯥ祭�� */
      /* ���������� ���஢ */
   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER acct     FOR acct.
   DEF BUFFER loan     FOR loan.
      /* ��室�� ᠬ ������� */
   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* ��室�� �� ������� ���ᯥ祭�� ������� ������� */
      FOR EACH term-obl WHERE
               term-obl.contract  EQ iContract
         AND   term-obl.cont-code EQ iContCode
         AND   term-obl.idnt      EQ 5
         AND (    term-obl.sop-date GE iSince
               OR term-obl.sop-date EQ ?)
      NO-LOCK:
         ASSIGN
            vSurr   = term-obl.contract + "," +
                      term-obl.cont-code + "," +
                      STRING(term-obl.idnt) + "," +
                      STRING(term-obl.end-date) + "," +
                      STRING(term-obl.nn)
            vVidDog = GetXattrValueEx ("term-obl", vSurr, "��������", "")
            vNumPP  = GetXattrValueEx ("term-obl", vSurr, "�������",  "")
         .
            /* ��室�� ��� ��� ������� ���ᯥ祭�� */
         RUN GetObespAcct IN h_lngar (vVidDog,
                                      vNumPP,
                                      iContract,
                                      iContCode,
                                      MAX(loan.since,term-obl.fop-date),
                                      term-obl.symbol,
                                      term-obl.fop,
                                      BUFFER acct).
            /* � ������塞 ��� � ᯨ᮪ */
         IF AVAIL acct THEN
            oListAcct = oListAcct + ";" + acct.acct + "," + acct.currency.
      END.
         /* ����塞 ";" � ��砫� ᯨ᪠ */
      oListAcct = SUBSTRING(oListAcct, 2).
   END.
END PROCEDURE.

   /* =======================================================-===--==-= */
   /* �� ������ ��� ���ᯥ祭�� � ��������, �����頥� ���祭��
   ** ��⥣�ਨ ����⢠ ���ᯥ祭�� */
PROCEDURE GetCollatCateg.
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iAcct       AS CHAR NO-UNDO.  /* ��� ���ᯥ祭�� */
   DEF INPUT  PARAM iCurrency   AS CHAR NO-UNDO.  /* ����� ��� ���ᯥ祭�� */
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.  /* ��� */
   DEF OUTPUT PARAM oQualityGar AS CHAR NO-UNDO.  /* ��⥣��� ����⢠ ���ᯥ祭�� */

   DEF VAR vSurr AS CHAR NO-UNDO.
      /* �饬 ������� ���ᯥ祭�� �� �������� � ���� */
   RUN GetGarByAcct IN h_lngar (iContract,
                                iContCode,
                                iAcct,
                                iCurrency,
                                iDate,
                                OUTPUT vSurr).
      /* �᫨ ��諨 ������� - ��।��塞 ��⥣��� ����⢠ */
   IF vSurr NE ""
   THEN DO:
      oQualityGar = Get_QualityGar ("comm-rate",
                                    vSurr,
                                    iDate).
      IF    oQualityGar EQ "?"
         OR oQualityGar EQ ?
      THEN
         oQualityGar = "".
   END.
END PROCEDURE.

   /* =======================================================-===--==-= */
   /* �� ������ ��� ���ᯥ祭�� � ��������, �����頥� ��� ���ᯥ祭�� */
PROCEDURE GetCollatVidOb.
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iAcct       AS CHAR NO-UNDO.  /* ��� ���ᯥ祭�� */
   DEF INPUT  PARAM iCurrency   AS CHAR NO-UNDO.  /* ����� ��� ���ᯥ祭�� */
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.  /* ��� */
   DEF OUTPUT PARAM oVidDog     AS CHAR NO-UNDO.  /* ��� ���ᯥ祭�� */

   DEF VAR vSurr AS CHAR NO-UNDO.
      /* ���������� ���� */
   DEF BUFFER term-obl FOR term-obl.
      /* �饬 ������� ���ᯥ祭�� �� �������� � ���� */
   RUN GetGarByAcct IN h_lngar (iContract,
                                iContCode,
                                iAcct,
                                iCurrency,
                                iDate,
                                OUTPUT vSurr).
      /* �᫨ ��諨 ������� - ��।��塞 ��⥣��� ����⢠ */
   IF vSurr NE "" THEN
      oVidDog = GetXattrValueEx ("term-obl", vSurr, "�����", "").
END PROCEDURE.

   /* =======================================================-===--==-= */
   /* �� ������ ��� ���ᯥ祭�� � ��������, �����頥� ��� ���ᯥ祭��.
      �᫨ ���� ��� ���ᯥ祭�� �� ��⥣�ਨ ����⢠, � �����頥� ���,
      �᫨ ���� - ��� ���ᯥ祭�� �� �।�⭮�� ��������.*/
PROCEDURE GetVidOb.
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iAcct         AS CHAR NO-UNDO.  /* ��� ���ᯥ祭�� */
   DEF INPUT  PARAM iCurrency     AS CHAR NO-UNDO.  /* ����� ��� ���ᯥ祭�� */
   DEF INPUT  PARAM iDate         AS DATE NO-UNDO.  /* ��� */
   DEF OUTPUT PARAM oVidObespech  AS CHAR NO-UNDO.  /* ��� ���ᯥ祭�� */
   DEF OUTPUT PARAM oNameVidObesp AS CHAR NO-UNDO.  /* ������������ ���ᯥ祭�� */

   DEF VARIABLE vSurr AS CHAR NO-UNDO. /* ������� ���ᯥ祭�� */

   DEF BUFFER term-obl FOR term-obl.

   /* �饬 ������� ���ᯥ祭�� �� �������� � ���� */
   RUN GetGarByAcct IN h_lngar (iContract,
                                iContCode,
                                iAcct,
                                iCurrency,
                                iDate,
                                OUTPUT vSurr).
   /* �᫨ ��諨 ������� - ��।��塞 ��⥣��� ����⢠ */
   IF vSurr NE "" THEN
   DO:
      oVidObespech = Get_VidObespech(vSurr,
                                     gend-date).
   END.
   IF oVidObespech = "" OR oVidObespech = "?" THEN
   DO:
      oVidObespech = GetXAttrValueEx("loan",
                                     iContract + "," + iContCode,
                                     "�117_�����",
                                     "").
      IF oVidObespech = "" THEN
      DO:
         oVidObespech = GetCode("�����",
                                GetXAttrValueEx("term-obl",
                                                vSurr,
                                                "�����",
                                                "")
                                ).
         IF NUM-ENTRIES(oVidObespech) NE 1 THEN
            oVidObespech = "".
      END.
   END.
      /* �᫨ ��� ��।���� */
   IF     oVidObespech NE ""
      AND GetCode("�117_�����",
                  oVidObespech) EQ "1" THEN
   DO:
      RUN GetNameVidObesp IN h_lngar (GetXAttrValueEx("term-obl",
                                                      vSurr,
                                                      "�����",
                                                      ""),
                                      OUTPUT oNameVidObesp
                                      ).
   END.

END PROCEDURE.
/* $LINTFILE='pp-cdrep.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='chumv' */
/* $LINTDATE='19/09/2016 05:35:46.979+03:00' */
/*prosignTw1co9NRS3QDbX/GkMFmJg*/
/* --- pp-cdrep.p was humbly modified by (c)blodd converter v.1.11 on 3/13/2017 2:16pm --- */
