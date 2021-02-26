/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2012 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: card_open.i
      Comment:
   Parameters:
         Uses:
      Used by: pp-card.p
      Created:
     Modified:
*/
/*****************************************************************************/
/*                          �������� ��������� � ����                        */
/*****************************************************************************/
/* �����頥� ���� ����砭�� ����⢨� �� */
/* �室�� ��ࠬ����: <��� 業. �㬠�� (loan.sec-code)>, <��� ��砫� (loan.open-date)>
** ��室�� ��ࠬ����: <��� ����砭��>, <�訡�� (? - �᫨ ��� �訡��; ⥪�� �訡�� - �᫨ �뫠)> */
PROCEDURE GetCardEndDate:
   DEF INPUT PARAM  iSecCode   AS CHAR NO-UNDO.
   DEF INPUT PARAM  iOpenDate  AS DATE NO-UNDO.
   DEF OUTPUT PARAM oEndDate   AS DATE NO-UNDO.
   DEF OUTPUT PARAM oErr       AS CHARACTER NO-UNDO.

   DEF VAR lTerm AS INT64 NO-UNDO.

   ASSIGN oErr = ?.
   FIND FIRST code WHERE code.class EQ "����끠���"
                     AND code.code  EQ iSecCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE code THEN DO:
     ASSIGN oErr = "��� ����� [" + iSecCode + "] �� ������ � �����䨪��� ����끠���".
     RETURN.
   END.

   IF code.misc[3] <> "" AND INT64(code.misc[3]) = ? THEN DO:
     ASSIGN oErr = "�����४⭮ 㪠��� �ப ⨯� ����� [" + iSecCode + "] � �����䨪��� ����끠���".
     RETURN.
   END.

   IF code.misc[3] <> "" THEN DO:
     lTerm = INT64(code.misc[3]) NO-ERROR.

     ASSIGN oEndDate = date_correct(MONTH(iOpenDate), lTerm, 31, YEAR(iOpenDate)).
   END.
END PROCEDURE.












/*------------------------------------------------------------------------------
  Purpose: �����頥� ��᪨஢���� ����� ����� ��� ����� ��
  Parameters:  ����᪨஢���� �����
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetNumCardOnUser RETURNS CHARACTER (INPUT iCardNumber AS CHARACTER ):

   DEFINE VARIABLE vCardNumberMask AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCardNumber     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCnt            AS INT64       NO-UNDO.

   IF work-module EQ "card"  THEN
      vCardNumberMask = "*".
   ELSE
   DO:
      FIND FIRST _user WHERE
         _user._userid EQ USERID("bisquit")
         NO-LOCK NO-ERROR.
      IF AVAIL _user THEN
         vCardNumberMask = GetXattrValue("_user",_user._Userid,"��᪠���⍮�").
   END.

   CASE vCardNumberMask:
      WHEN "*" THEN
         vCardNumber = iCardNumber.
      WHEN ""  THEN
         vCardNumber = "".
      OTHERWISE
      DO:
         IF iCardNumber NE "" THEN
         DO vCnt = 1 TO LENGTH(vCardNumberMask):
            IF SUBSTRING(vCardNumberMask, vCnt, 1) = "." THEN
              vCardNumber = vCardNumber + SUBSTRING(iCardNumber, vCnt, 1).
            ELSE
            DO:
              IF SUBSTRING(iCardNumber, vCnt, 1) NE "" THEN
                vCardNumber = vCardNumber + SUBSTRING(vCardNumberMask, vCnt, 1).
            END.
         END.
      END.
   END CASE.

   RETURN vCardNumber.

END FUNCTION.






/*****************************************************************************/
/* �������� ����� ��� ������� �� */
/* �室�� ��ࠬ����: <���� op-template � �������� 蠡����� �����>,
                      <RECID loan � ���஬� �ਢ�뢠���� ����>
                      <��� ᮧ�����>
** ��室�� ��ࠬ����: <�訡�� (? - �᫨ ��� �訡��; ⥪�� �訡�� - �᫨ �뫠)> */
PROCEDURE CR_Card:
   DEFINE PARAM BUFFER bOpTempl FOR op-template.
   DEFINE INPUT  PARAM iLoanRec  AS RECID             NO-UNDO.
   DEFINE INPUT  PARAM iOpenDate AS DATE              NO-UNDO.
   DEFINE OUTPUT PARAM oCardRec  AS RECID             NO-UNDO.
   DEFINE OUTPUT PARAM oErr      AS CHARACTER INIT ?  NO-UNDO.

   DEF VAR vErr       AS CHARACTER INIT ? NO-UNDO.
   DEF VAR vEndDate   AS DATE             NO-UNDO.
   DEF VAR vSetStatus AS CHARACTER        NO-UNDO.
   DEF VAR vRetVal    AS CHARACTER        NO-UNDO.
   DEF VAR vAcctSurr  AS CHARACTER        NO-UNDO.

   DEFINE BUFFER xloan       FOR loan.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      FIND FIRST loan WHERE RECID(loan) EQ iLoanRec NO-LOCK NO-ERROR.


      RUN GetCardEndDate (loan.trade-sys,
                          iOpenDate,
                          OUTPUT vEndDate,
                          OUTPUT vErr).
      IF vErr NE ? THEN
      DO:
         ASSIGN
            vErr    = {&NoEndDate} + " " + vErr
            vRetVal = "-2"
            .
         UNDO MAIN, LEAVE MAIN.
      END.

      /* ����� ����� */
      vSetStatus = GetXAttrValueEx("op-template", bOpTempl.op-kind + "," +
                                      STRING(bOpTempl.op-template), 'set-status', "").
      IF NOT CAN-FIND (FIRST xstatus WHERE xstatus.class-code  EQ "card"
                                       AND xstatus.status-code EQ vSetStatus NO-LOCK) THEN
      DO:
         ASSIGN
            vErr    = {&WrongNewCardStatus}
            vRetVal = "-2"
            .
         UNDO MAIN, LEAVE MAIN.
      END.

      CREATE xloan.
      xloan.cont-code = STRING(GetCounterNextValue("�����", iOpenDate)).
      IF NOT {assigned xloan.cont-code} THEN
      DO:
         ASSIGN
            vErr    = "�訡�� ᮧ����� �����. �஢���� ����ன�� ���稪� <�����>.".
            vRetVal = "-2"
            .
         UNDO MAIN, LEAVE MAIN.
      END.

      ASSIGN
         xloan.Class-Code       = "card"
         xloan.parent-contract  = loan.contract
         xloan.parent-cont-code = loan.cont-code
         xloan.doc-num          = ""
         xloan.sec-code         = loan.trade-sys
         xloan.contract         = "card"
         xloan.currency         = loan.currency
         xloan.cust-cat         = loan.cust-cat
         xloan.cust-id          = loan.cust-id
         xloan.conf-date        = iOpenDate
         xloan.open-date        = iOpenDate
         xloan.end-date         = vEndDate
         xloan.deal-type        = loan.deal-type
         xloan.flag-acsept      = loan.loan-work
         xloan.user-o[1]        = loan.sec-code
         xloan.branch-id        = loan.branch-id
         xloan.loan-status      = vSetStatus
         .
      xloan.loan-work = LOGICAL(GetXAttrValueEx("op-template", bOpTempl.op-kind + "," +
                                   STRING(bOpTempl.op-template), 'main-card', ?), "��/���").
      IF xloan.loan-work EQ ? THEN
         xloan.loan-work = NO.
      xloan.user-o[2] = GetXAttrValueEx("loan",
                                        loan.contract + "," + loan.cont-code,
                                        'lat-line1',
                                        "").
      xloan.user-o[3] = GetXAttrValueEx("loan",
                                        loan.contract + "," + loan.cont-code,
                                        'lat-line2',
                                        "").

      RUN CreateCardSPCLink IN THIS-PROCEDURE (xloan.contract,
                                               xloan.cont-code,
                                                ?,
                                                xloan.loan-work,
                                                Loan.contract,
                                                Loan.cont-code,
                                                iOpenDate,
                                                ?,
                                                OUTPUT vAcctSurr,
                                                OUTPUT vErr).
           IF {assigned vErr} THEN
           DO:
              vRetVal = "-2".
         UNDO MAIN, LEAVE MAIN.
      END.
      IF vErr EQ "" THEN
         vErr = ?.

      oCardRec  = RECID(xloan).
      RELEASE xloan.
   END.

   oErr = vErr.
   IF {assigned vRetVal} THEN
      RETURN vRetVal.

END PROCEDURE. /* CR_Card */

/*****************************************************************************/
/* ��ࠡ�⪠ 蠡���� ������� �� (����� �����) */
/* �室�� ��ࠬ����: <���� op-template � AVAIL 蠡����� �����>,
                      <���� loan � ���஬� �ਢ�뢠���� ���� (� AVAIL loan)>
** ��室�� ��ࠬ����: <�訡�� (? - �᫨ ��� �訡��; ⥪�� �訡�� - �᫨ �뫠)> */
PROCEDURE ProcessLoanSettings:
   DEF INPUT PARAM iOpTemplRec AS RECID NO-UNDO.
   DEF PARAM BUFFER bloan FOR loan.
   DEF OUTPUT PARAM oErr AS CHARACTER NO-UNDO.

   DEF VAR vNewStatus AS CHARACTER NO-UNDO.

   FIND FIRST op-template WHERE RECID(op-template) EQ iOpTemplRec NO-LOCK NO-ERROR.
   IF NOT AVAIL op-template THEN
       RETURN.

   ASSIGN
       oErr = ?
       vNewStatus = GetXAttrValueEx("op-template", op-template.op-kind + "," +
                                STRING(op-template.op-template), "set-status", ?)
       .

   IF NOT CAN-FIND (FIRST xstatus WHERE xstatus.class-code EQ bloan.Class-Code
                                    AND xstatus.status-code EQ vNewStatus NO-LOCK) THEN
   DO:
      ASSIGN oErr = {&WrongNewLoanStatus}.
      RETURN.
   END.

   ASSIGN bloan.loan-status = vNewStatus.
END PROCEDURE. /* ProcessLoanSettings */

/*****************************************************************************/
/* �������� �᫮��� ��� ������� �� (�᫨ ����� ���饣� �᫮��� ��।�� � iCrClass -
   ���� ���, ���� ��⠥��� ���� �� ᠬ�� �������) */
/* �室�� ��ࠬ����: <���� loan � ���஬� �ਢ�뢠���� ���� (� AVAIL loan)>,
                      <���� loan-cond (�����頥� ᮧ���� loan-cond)>,
                      <��� ᮧ�����>,
                      <����� �᫮��� (�᫨ ��������� 蠡��� �⠭������ �᫮���,
                        � ����� ���� ��।��� ����� �᫮���, ����� �� ᮧ������)>
** ��室�� ��ࠬ����: <�訡�� (? - �᫨ ��� �訡��; ⥪�� �訡�� - �᫨ �뫠)> */
PROCEDURE CreateLoanCond4CardLoan.
    DEFINE PARAM BUFFER bloan FOR loan.
    DEFINE PARAM BUFFER bloan-cond FOR loan-cond.
    DEFINE INPUT PARAM iDate AS DATE NO-UNDO.
    DEFINE INPUT PARAM iCrClass AS CHAR NO-UNDO.

    DEFINE OUTPUT PARAM oErr AS CHARACTER NO-UNDO.

    DEF VAR vCond AS CHARACTER NO-UNDO.

    ASSIGN oErr = ?.

    ASSIGN vCond = iCrClass.
    IF vCond EQ ? THEN
        ASSIGN vCond = GetXAttrValueEx("loan", loan.contract + "," +
                                     loan.cont-code, "default-cond", ?).

    FIND FIRST Class WHERE class.class-code EQ vCond NO-LOCK NO-ERROR.
    IF NOT AVAIL Class THEN DO:
        RETURN.
    END.

    CREATE bloan-cond.
    ASSIGN
        bloan-cond.class-code = vCond
        bloan-cond.contract = bloan.contract
        bloan-cond.cont-code = bloan.cont-code
        bloan-cond.since = iDate
        .
END PROCEDURE. /* CreateLoanCond4CardLoan */
/*****************************************************************************/

/* �㭪�� �����樨 ����� ��� ����ᨭ�� */
FUNCTION GenNewSPCNumber RETURNS CHAR (BUFFER loan FOR loan,        /* ���� ����� */
                                       INPUT  iOpDate AS DATE ,     /* ����. ��� */
                                       INPUT  iPCCode AS CHARACTER, /* ��� ����ᨭ�� */
                                       OUTPUT oErr    AS INT64,   /* �ਧ��� �訡�� */
                                       OUTPUT oErrMes AS CHARACTER  /* ����饭�� �� �訡�� */

                                       ):

   DEF VAR vSPCMeth  AS CHARACTER   NO-UNDO. /* ��⮤ �����樨 */
   DEF VAR vSPCProc  AS CHARACTER   NO-UNDO. /* ��楤�� �����樨 */
   DEF VAR vRetVal   AS CHARACTER   NO-UNDO. /* �����頥��� ���祭�� */
   DEF VAR vAcct     AS CHARACTER   NO-UNDO. /* ��� ��� */
   DEF VAR vACurr    AS CHARACTER   NO-UNDO. /* ����� ��� */
   DEF VAR vErrLocal AS INT64     NO-UNDO. /* �����쭮� ���祭�� �訡�� */
   DEF VAR vCardType AS CHARACTER   NO-UNDO. /* ��� ����� */

   oErr = -1.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* ���� ⨯� ����� � �����䨪��� "����끠���" */
      IF AVAIL loan THEN
      DO:
         vCardType = GetCodeMisc ("����끠���",
                                  loan.sec-code,
                                  2).
         IF NOT {assigned vCardType} THEN
         DO:
            oErrMes = "��������� ⨯ ����� <" + loan.sec-code + "> � �����䨪��� <����끠���>.".
            UNDO MAIN, LEAVE MAIN.
         END.
      END.
      ELSE
      DO:
         vCardType = iPCCode.
         IF NOT {assigned vCardType} THEN
         DO:
            oErrMes = "�� 㪠��� �� ����ᨭ�, �� ����.".
            UNDO MAIN, LEAVE MAIN.
         END.
      END.

      /* ���� ����ᨭ�� � �����䨪��� "����ᨭ��" */
      FIND FIRST Code WHERE Code.CLASS  EQ "����ᨭ��"
                        AND Code.CODE   EQ vCardType
         NO-LOCK NO-ERROR.
      IF NOT AVAIL Code THEN
      DO:
         oErrMes = "�� ������ �����䨪��� � ����� <" + vCardType + "> 㪠���� � ⨯� ����� <" + loan.sec-code + ">.".
         UNDO MAIN, LEAVE MAIN.
      END.

      vSPCMeth = GetXAttrValueEx ("CODE",
                                  CODE.CLASS + "," + CODE.CODE,
                                  "�����⮤",
                                  GetXAttrInit("Processing", "�����⮤")).

      CASE vSPCMeth:
         WHEN "��" THEN
            vRetVal = "".
         WHEN "�����" THEN
         DO:
            IF AVAIL loan THEN
               vRetVal = loan.doc-num.
            ELSE
            DO:
               oErrMes = "���� �� ��।�����.".
               UNDO MAIN, LEAVE MAIN.
            END.
         END.
         WHEN "���" THEN
         DO:
            IF AVAIL loan THEN
            DO:
               RUN GetRoleAcct IN THIS-PROCEDURE (loan.parent-contract + "," + loan.parent-cont-code,
                                                  iOpDate,
                                                  "SCS",
                                                  loan.currency,
                                                  OUTPUT vAcct,
                                                  OUTPUT vACurr).
               IF {assigned vAcct} THEN
                  vRetVal = vAcct.
               ELSE
               DO:
                  oErrMes = "�� ������ ��� � ஫�� ���.".
                  UNDO MAIN, LEAVE MAIN.
               END.
            END.
            ELSE
            DO:
               oErrMes = "���� �� ��।�����.".
               UNDO MAIN, LEAVE MAIN.
            END.
         END.
         WHEN "����" THEN
         DO:
            vSPCProc = GetXAttrValueEx ("code",
                                        CODE.CLASS + "," + CODE.CODE,
                                        "�����楤��",
                                        GetXAttrInit("Processing", "�����楤��")).
            IF NOT SearchPFile(vSPCProc) THEN
            DO:
               oErrMes = "�� ������� ��楤�� �����樨 ����� ���.".
               UNDO MAIN, LEAVE MAIN.
            END.
            ELSE
            DO:
               RUN VALUE(vSPCProc + ".p") (BUFFER loan,
                                           iOpDate,
                                           OUTPUT vRetVal,
                                           OUTPUT vErrLocal,
                                           OUTPUT oErrMes).
               IF vErrLocal LT 0 THEN
                  UNDO MAIN, LEAVE MAIN.
            END.
         END.
         OTHERWISE
         DO:
            oErrMes = IF {assigned vSPCMeth} THEN "������ �� ���� ��⮤ �����樨 ����� <" + vSPCMeth + ">" ELSE "�� 㪠��� ��⮤ �����樨 �����".
            UNDO MAIN, LEAVE MAIN.
         END.
      END CASE.
      oErr = 0.
   END.

   RETURN vRetVal.
END FUNCTION.
/*****************************************************************************/

/* ��ନ��� ����� ������� �� 蠡���� */
FUNCTION GenNewLoanNumber RETURNS CHAR (INPUT iClassCode AS CHARACTER, /* ����� ������� */
                                        INPUT iCardType  AS CHARACTER, /* ��� ����� */
                                        INPUT iCurrency  AS CHARACTER, /* ����� ������� */
                                        INPUT iZplLoan   AS CHARACTER, /* ����� ��௫�⭮�� ������� */
                                        INPUT iDate      AS DATE,      /* ��� ��� �ନ஢���� */
                                        INPUT iCustCode  AS CHARACTER  /* ��� ������ */
                                        ):

   DEF VAR vTemplate  AS CHARACTER   NO-UNDO.
   DEF VAR vCounter   AS CHARACTER   NO-UNDO.
   DEF VAR vCounter2  AS CHARACTER   NO-UNDO.
   DEF VAR vPrefix    AS CHARACTER   NO-UNDO.
   DEF VAR vCnt       AS CHARACTER   NO-UNDO.
   DEF VAR vCurr      AS CHARACTER   NO-UNDO.
   DEF VAR vPaySys    AS CHARACTER   NO-UNDO. /* ���⥦��� ��⥬� */
   DEF VAR vBranch    AS CHARACTER   NO-UNDO.
   DEF VAR vNum       AS INT64       NO-UNDO. /* ����饥 ���祭�� ����稪� */
   DEF VAR vTypeResid AS CHARACTER   NO-UNDO.
   DEF VAR vCriter    AS CHARACTER   NO-UNDO.

   RUN GetClassTemplatePars IN h_jloan (iClassCode,
                                        OUTPUT vTemplate,
                                        OUTPUT vCounter).

   FIND FIRST person WHERE person.person-id = INT64 (iCustCode)  NO-LOCK NO-ERROR .
   IF AVAILABLE person THEN
      IF person.country-id EQ FGetSetting("������","","")
         THEN vTypeResid = "�" .
         ELSE vTypeResid = "��" .
   ASSIGN
   vCriter     = SUBSTR(vTypeResid,1,1)
   vPrefix     = GetCodeMisc("����끠���", iCardType, 7)
   vPrefix     = IF vPrefix EQ ?
                    THEN ""
                    ELSE vPrefix
   vCounter2   = ?
   .
   XXX: 
      FOR EACH code WHERE code.class EQ "������" 
                   AND code.parent EQ "������" 
                   AND CAN-DO(ENTRY(1,code.code),vPrefix)
                   AND (NUM-ENTRIES(code.code) LE 1 OR CAN-DO(ENTRY(2,code.code),iCurrency))
                   AND (NUM-ENTRIES(code.code) LE 2 OR CAN-DO(ENTRY(3,code.code),vCriter))
                   NO-LOCK:
                   vCounter2   = code.val.
                   LEAVE XXX. 
      END.
   ASSIGN
      vBranch     = GetXattrValue("_user", USERID("bisquit"), "�⤥�����")
      vCnt        = IF {assigned vCounter2}
                    THEN vCounter2
                    ELSE GetCodeMisc("����끠���", iCardType, 8)
      vPaySys     = REPLACE(GetCodeMisc("����끠���", iCardType, 4),",","/")
      vPaySys     = IF vPaySys EQ ?
                    THEN ""
                    ELSE vPaySys
      vCounter    = IF {assigned vCnt}
                    THEN vCnt
                    ELSE vCounter

      vTemplate   = ReplaceBasicTags (vTemplate, vCounter, iDate)
      vNum        = GetCounterCurrentValue (vCounter, iDate)
      vTemplate   = ReplaceTag (vTemplate, "�", STRING(vNum), NO)
      vTemplate   = ReplaceTag (vTemplate, "�", vBranch  , YES)
      vTemplate   = ReplaceTag (vTemplate, "�", iZplloan , YES)
      vCurr       = GetISOCode (iCurrency)
      vTemplate   = ReplaceTag (vTemplate, "�", vCurr    , YES)
      vTemplate   = ReplaceTag (vTemplate, "�", vPrefix  , NO)
      vTemplate   = ReplaceTag (vTemplate, "�", iCustCode, YES)
      vTemplate   = ReplaceTag (vTemplate, "�", vPaySys  , NO)
      vTemplate   = ReplaceTag (vTemplate, "�", vTypeResid , NO). /* ���᪮� � */
      vTemplate   = IF shmode THEN AddFilToLoan(vTemplate,shfilial) ELSE vTemplate.
      .
   RETURN vTemplate.
END FUNCTION. /* GenNewLoanNumber */
/*****************************************************************************/
/*------------------------------------------------------------------------------
  Purpose:     �ਧ��� �����  ��� ����஢����     
------------------------------------------------------------------------------*/
FUNCTION fGetCardNoEmb RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):

   DEFINE VARIABLE oRetVal AS LOGICAL    NO-UNDO INITIAL NO.   
   DEFINE BUFFER code FOR code.

/* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      FIND FIRST code WHERE
             code.Class EQ "����끠���"
         AND code.Code  EQ iTypeCard
             NO-LOCK NO-ERROR.
      IF NOT AVAILABLE code THEN
      DO:
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
      oRetVal = GetXAttrValueEx("code",
                                code.class + "," + code.code,
                                "���������",
                                "���") EQ "��".      
   END.
   /* *************************  End of Main Block  ********************** */
   RETURN oRetVal.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �ਧ��� �।����祭��� �����      
------------------------------------------------------------------------------*/
FUNCTION fGetPredoplCard RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):

   DEFINE VARIABLE oRetVal AS LOGICAL    NO-UNDO INITIAL NO.
   
   DEFINE BUFFER code FOR code.

/* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      FIND FIRST code WHERE
             code.Class EQ "����끠���"
         AND code.Code  EQ iTypeCard
             NO-LOCK NO-ERROR.
      IF NOT AVAILABLE code THEN
      DO:
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
      oRetVal = GetXAttrValueEx("code",
                                code.class + "," + code.code,
                                "�।����祭��",
                                "���") EQ "��".      
   END.
   /* *************************  End of Main Block  ********************** */
   RETURN oRetVal.
END FUNCTION.

FUNCTION fNotPersCard RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):
   DEFINE VARIABLE oRetVal AS LOGICAL    NO-UNDO INITIAL NO.   

/* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      
      oRetVal = fGetCardNoEmb(iTypeCard) OR fGetPredoplCard(iTypeCard). 
   END.
   /* *************************  End of Main Block  ********************** */
   RETURN oRetVal.
END FUNCTION.

/* ����室������ ᮧ������ ��� �� ᮧ����� ��� */
FUNCTION IsNeedSPCnoSCS RETURNS LOGICAL (INPUT iTypeCard AS CHARACTER):

   DEFINE VARIABLE vRetVal   AS LOGICAL NO-UNDO INIT NO.
   
   DEFINE VARIABLE vPC       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNotPers  AS LOGICAL     NO-UNDO INIT NO.
   
   DEF BUFFER bCode FOR code.
   DEF BUFFER card  FOR loan.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      vPC = GetCodeMisc("����끠���", iTypeCard,2).

      FIND FIRST bCode WHERE bCode.class EQ "����ᨭ��"
                         AND bCode.code  EQ vPC
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bCode THEN
         UNDO MAIN, LEAVE MAIN.

      vNotPers = fNotPersCard(iTypeCard).
      
      vRetVal = GetXAttrValueEx("code",
                                bcode.class + "," + bcode.code,
                                "��������",
                                "") EQ "��" AND vNotPers.
                               
   END.

   RETURN vRetVal.
END FUNCTION.

PROCEDURE CreateCardSPCLink:
DEFINE INPUT  PARAMETER iCardContract AS CHARACTER   NO-UNDO. /* ���ண�� �����: �����祭�� */
DEFINE INPUT  PARAMETER iCardContCode AS CHARACTER   NO-UNDO. /* ���ண�� �����: ����७��� �����䨪��� */
DEFINE INPUT  PARAMETER iOldCard      AS CHARACTER   NO-UNDO. /* ����� ��ன ����� (�᫨ ���� ᮧ������ � ������� ��ॢ��᪠) */
DEFINE INPUT  PARAMETER iMain         AS LOGICAL     NO-UNDO. /* �ਧ��� "������� ����" */
DEFINE INPUT  PARAMETER iLoanContract AS CHARACTER   NO-UNDO. /* ���ண�� �������: �����祭��  */
DEFINE INPUT  PARAMETER iLoanContCode AS CHARACTER   NO-UNDO. /* ���ண�� �������: �����  */
DEFINE INPUT  PARAMETER iOpDate       AS DATE        NO-UNDO. /* ��� ����樨 */
DEFINE INPUT  PARAMETER iSPC          AS CHARACTER   NO-UNDO. /* ���, � ���஬� �㦭� �ਢ易�� ����� */

DEFINE OUTPUT PARAMETER oAcctSurr     AS CHARACTER   NO-UNDO. /* ���ண�� ���, � ���஬� �ਢ易�� ���� */
DEFINE OUTPUT PARAMETER oErrMessage   AS CHARACTER   NO-UNDO. /* ����饭�� �� �訡��, �᫨ �뫠 �訡�� */

   DEFINE VAR vSPCList AS CHARACTER NO-UNDO.
   DEFINE VAR vSPCSurr AS CHARACTER NO-UNDO.
   DEFINE VAR vSPC     AS CHARACTER NO-UNDO.
   DEFINE VAR vSPCCur  AS CHARACTER NO-UNDO.
   DEFINE VAR vi       AS INT64   NO-UNDO.
   DEFINE VAR vOldCardNum  AS CHARACTER NO-UNDO.
   DEFINE VAR vOldCardSurr AS CHARACTER NO-UNDO.

   DEF VAR vAcctSurr AS CHARACTER NO-UNDO.
   DEF VAR vAcct     AS CHARACTER NO-UNDO.
   DEF VAR vACurr    AS CHARACTER NO-UNDO.
   DEF VAR vSPCNum   AS CHARACTER NO-UNDO.
   DEF VAR vErr      AS INT64   NO-UNDO.
   DEF VAR vSPCMeth  AS CHARACTER NO-UNDO.
   DEF VAR vErrMes   AS CHARACTER NO-UNDO.
   DEF VAR vOffSpc   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE mCrStatus AS CHARACTER  NO-UNDO INITIAL "���".

   DEF BUFFER card   FOR loan.
   DEF BUFFER card2  FOR loan.
   DEF BUFFER loan   FOR loan.
   DEF BUFFER acct   FOR acct.
   DEF BUFFER spc    FOR acct.

   MAIN:
   DO ON ERROR  UNDO MAIN, RETRY MAIN
      ON ENDKEY UNDO MAIN, RETRY MAIN:

      IF RETRY THEN DO:
        oErrMessage = "�������� ��� � �����.~n�ந��諠 ��⥬��� �訡��: " + ERROR-STATUS:GET-MESSAGE(1).
        LEAVE MAIN.
      END.

      vOffSpc = FGetSetting("���", "�⪫���", ?) NO-ERROR.
      IF vOffSpc EQ "��" THEN UNDO MAIN, LEAVE MAIN.

      FIND FIRST loan WHERE loan.contract  EQ iLoanContract
                        AND loan.cont-code EQ iLoanContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loan THEN DO:
         oErrMessage = SUBST("�������� ��� � �����.~�� ������ ������� �� [&1,&2]", iLoanContract, iLoanContCode).
         UNDO MAIN, LEAVE MAIN.
      END.

      FIND FIRST card WHERE card.contract  EQ iCardContract
                        AND card.cont-code EQ iCardContCode
                      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE card THEN DO:
         oErrMessage = SUBST("�������� ��� � �����.~�� ������a ���� [&1,&2], � ���ன �ਢ�뢠���� ���", iCardContract, iCardContCode).
         UNDO MAIN, LEAVE MAIN.
      END.

      vOldCardNum  = ENTRY(1, iOldCard) NO-ERROR.
      vOldCardSurr = ENTRY(2, iOldCard) NO-ERROR.
      /* 1. ���� ���� ��ॢ��饭��� - ���� �ਢ�뢠�� � ��� ��� ��ॢ��᪠���� ����� */
      IF {assigned vOldCardNum} OR {assigned vOldCardSurr} THEN DO:
         IF {assigned vOldCardNum} THEN DO:
            FIND FIRST card2 WHERE card2.contract         EQ "card"
                               AND card2.doc-num          EQ vOldCardNum
                               /* �������⥫�� ����஫�: ���� ������ �ਭ�������� ⮬� �� �������� */
                               AND card2.parent-contract  EQ loan.contract
                               AND card2.parent-cont-code EQ loan.cont-code
               NO-LOCK NO-ERROR.
         END.
         ELSE DO: /* ��� ���� ��ॢ��᪠ ���� ��� �����... �� ���� ���� ��ࠡ��뢠��, �� ���� ⠪ */
            FIND FIRST card2 WHERE card2.contract         EQ "card"
                               AND card2.cont-code        EQ vOldCardSurr
                               /* �������⥫�� ����஫�: ���� ������ �ਭ�������� ⮬� �� �������� */
                               AND card2.parent-contract  EQ loan.contract
                               AND card2.parent-cont-code EQ loan.cont-code
               NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE card2 THEN DO:
            oErrMessage = SUBST("�������� ��� � �����.~n���������� ���� ��ॢ��᪠���� ����� [&1] � ������� [&2,&3]",
                                iOldCard, loan.contract, loan.cont-code).
            UNDO MAIN, LEAVE MAIN.
         END.

         vSPCList = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "���⠏��", ";", iOpDate).
         IF NOT {assigned vSPCList} THEN DO:
            oErrMessage = SUBST("�������� ��� � �����.~n� ��ॢ��᪠���� ���� [&1] �� �ਢ易�� �� ������ ���", iOldCard).
            UNDO MAIN, LEAVE MAIN.
         END.

         /* �ਢ�뢠�� �� �������� ��� ��ॢ��᪠���� ����� � ����� ���� */
         DO vi = 1 TO NUM-ENTRIES(vSPCList, ";"):
            vSPCSurr = ENTRY(vi, vSPCList, ";").
            ASSIGN
               vSPC    = ENTRY(1, vSPCSurr)
               vSPCCur = ENTRY(2, vSPCSurr)
            NO-ERROR.

            {find-act.i
               &bact = spc
               &acct = vSPC
               &curr = vSPCCur
            }
            IF NOT AVAILABLE spc THEN DO:
                oErrMessage = SUBST("�������� ��� � �����.~n��� [&1] ��ॢ��᪠���� ����� [&2] �� ������ � ��", vSPCSurr, iOldCard).
                UNDO MAIN, LEAVE MAIN.
            END.

            RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                  spc.class-code,
                                                  card.contract + "," + card.cont-code,
                                                  loan.contract + "," + loan.cont-code,
                                                  iOpDate,
                                                  ?).
         END.
         LEAVE MAIN. /* �� */
      END.

      /* 2. ���� �� ���� ��ॢ��饭��� - ������ �㤥� ��᫮����...
         2.1. �᫨ �� ������� ���������� ��᪮�쪮 ���
               - ��� ��ࢮ� ����� ������� ᮧ���� ��� � ����� �������
               - �� ��⠫�� ����� �ਢ�뢠�� � �⮬� ��� */
      IF NOT loan.loan-work THEN DO:
            /* �஢��塞, ������� �� � ��襬 �������
               �� ��� �� ���� ����, �஬� ᮧ�������� */
            FIND FIRST card2 WHERE card2.parent-contract  EQ loan.contract
                               AND card2.parent-cont-code EQ loan.cont-code
                               AND card2.contract         EQ "card"
                               AND card2.cont-code        NE card.cont-code
               NO-LOCK NO-ERROR.
            /* 2.1.1 �� ������� ᮧ�������� ���� �� ��ࢠ�.
               ��६ ��� � ��������� ��ன ����� � �ਢ�뢠�� ��� � ����� */
            IF AVAILABLE card2 THEN DO:
               vSPCSurr = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "���⠏��", ";", iOpDate).
               IF NOT {assigned vSPCSurr} THEN DO:
                  oErrMessage = SUBST("�������� ��� � �����.~n����⪠ ����஢��� ��� � ����� [&1,&2] �����襭� � �訡���. ��� � �⮩ ���� �� �ਢ易�.",
                                      card2.contract, card2.cont-code).
                  UNDO MAIN, LEAVE MAIN.
               END.

               ASSIGN
                  vSPC    = ENTRY(1, ENTRY(1, vSPCSurr, ";"))
                  vSPCCur = ENTRY(2, ENTRY(1, vSPCSurr, ";"))
               NO-ERROR.
               {find-act.i
                   &bact = spc
                   &acct = vSPC
                   &curr = vSPCCur
               }
               IF NOT AVAILABLE spc THEN DO:
                  oErrMessage = SUBST("�������� ��� � �����.~n��� [&1] �� ������ � ��", vSPCSurr).
                  UNDO MAIN, LEAVE MAIN.
               END.

               RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                     spc.class-code,
                                                     card.contract + "," + card.cont-code,
                                                     loan.contract + "," + loan.cont-code,
                                                     iOpDate,
                                                     ?).
               LEAVE MAIN. /* �� */
            END.

            /* 2.1.2. �� ������� �� ������� ��㣨� ����, �஬� ᮧ��������.
               �� �����, ��� �㦭� ᮧ������ ����
                (!!!!!!! �� ᠬ�� ���� ��। ᮧ������ �㦭� ���஡����� ���� �������騩 ���, �� ���� �� ����⨬) */
            /* ���⠫� ��� �஢����� ����稥 ��� */
            vSPCSurr = GetLinks ("card", card.contract + "," + card.cont-code, ?, "���⠏��", ";", iOpDate).
            IF {assigned vSPCSurr} THEN
            DO:
               ASSIGN
                  vSPC    = ENTRY(1, ENTRY(1, vSPCSurr, ";"))
                  vSPCCur = ENTRY(2, ENTRY(1, vSPCSurr, ";"))
               NO-ERROR.
               {find-act.i
                   &bact = spc
                   &acct = vSPC
                   &curr = vSPCCur
               }               
            END.

            IF NOT AVAILABLE spc THEN
            DO:            
               RUN cm_acct_cr IN h_acct ("acctp", FGetSetting("���", "���2���", ?),
                                         loan.currency, card.cust-cat, card.cust-id, iOpDate,
                                         OUTPUT vAcct, BUFFER spc,
                                         ?, ?, ?,
                                         "���",
                                         USERID("bisquit"),
                                         GetUserBranchId(USERID("bisquit")),
                                         YES) NO-ERROR.
               IF NOT AVAILABLE spc OR ERROR-STATUS:ERROR THEN DO:
                     oErrMessage = SUBST("�������� ��� � �����.~n�訡�� �� ᮧ����� ������ ���", ERROR-STATUS:GET-MESSAGE(1)).
                     UNDO MAIN, LEAVE MAIN.
               END.
   
               RUN GetInitStatusSPCEx(BUFFER card,INPUT-OUTPUT mCrStatus).
               spc.acct-status = mCrStatus.
               vSPCNum         = GenNewSPCNumber (BUFFER card, iOpDate, "", OUTPUT vErr, OUTPUT oErrMessage).
               UpdateTempSignsEx (spc.class-code, spc.acct + "," + spc.currency, "��������", iOpDate, vSPCNum, ?).
            END.

            RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                  spc.class-code,
                                                  card.contract + "," + card.cont-code,
                                                  loan.contract + "," + loan.cont-code,
                                                  iOpDate,
                                                  ?).
            LEAVE MAIN. /* �� */
      END.

      /*  2.2. ���� �� �������� �������� ��������� ��� - ⮦� ��᪮�쪮 ��ਠ�⮢:

          2.2.1 �� ���� 㪠��� ������� ����� ���.
                � �⮬ ��砥 �㦭� ������ ��� ������� ��� � �ਢ易��.
                ��祬, �� ����� ���祭��, ������� ⠪�� ��� ��� ���
                (�᫨ �� ������� - ���� ᮧ����).
             !!!!! � ������ �᪠�� ���� �� �� �� ���, � �� ��� ४������ ��������! � �� ��� ����� ��।����� ��� ��.
             !!!!! � �� ᮧ����� ��� 㪠�뢠�� ��� �������� */
      IF {assigned iSPC} THEN DO:
         /* �஢��塞, ���� �� ᮧ������ ��� */
         FIND FIRST spc WHERE spc.acct = iSPC NO-LOCK NO-ERROR.
         IF NOT AVAILABLE spc THEN DO:
            /* ... ᮧ���� */
            RUN cm_acct_cr IN h_acct ("acctp", FGetSetting("���", "���2���", ?),
                                      loan.currency, card.cust-cat, card.cust-id, iOpDate,
                                      OUTPUT vAcct, BUFFER spc,
                                      ?, ?, ?,
                                      "���",
                                      USERID("bisquit"),
                                      GetUserBranchId(USERID("bisquit")),
                                      YES) NO-ERROR.
            IF NOT AVAILABLE spc OR ERROR-STATUS:ERROR THEN DO:
                  oErrMessage = SUBST("�������� ��� � �����.~n�訡�� �� ᮧ����� ������ ���", ERROR-STATUS:GET-MESSAGE(1)).
                  UNDO MAIN, LEAVE MAIN.
            END.

            RUN GetInitStatusSPCEx(BUFFER card,INPUT-OUTPUT mCrStatus).
            spc.acct-status = mCrStatus.
            vSPCNum         = GenNewSPCNumber (BUFFER card, iOpDate, "", OUTPUT vErr, OUTPUT oErrMessage).
            UpdateTempSignsEx (spc.class-code, spc.acct + "," + spc.currency, "��������", iOpDate, vSPCNum, ?).
         END.
         /* �ਢ�뢠�� */
         RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                               spc.class-code,
                                               card.contract + "," + card.cont-code,
                                               loan.contract + "," + loan.cont-code,
                                               iOpDate,
                                               ?).

         LEAVE MAIN. /* �� */
      END.

      /* 2.2.2 �����⭮�� ����� ��� �� 㪠���� - ᠬ�� ᫮���� �����

         2.2.2.� ���� �������⥫쭠�
                 - �饬 ��� �� ������� ����������� ���� �������, ��६ � ��� ���, �ਢ�뢠��
                 - �᫨ ������� ��������� ������� ���� �������, �� �� ��� ��� ��� -
                   �� �ਢ�뢠�� ���
                 - �᫨ ��������� ������� ���� �� ������� - �饬 ������� �����,
                   ����⢠���� ࠭��
                 - �᫨ ������� ���� �� ������� ��� �� ��� ��� ��� - �� �ਢ�뢠�� ���
       */
      IF NOT card.loan-work THEN DO:
        /* �饬 ��� �� ������� ���� - ᭠砫� �� �������饩 ������� ����... */
        FIND FIRST card2 WHERE card2.parent-contract  = card.parent-contract
                           AND card2.parent-cont-code = card.parent-cont-code
                           AND card2.contract         EQ "card"
                           AND card2.cont-code        NE card.cont-code
                           AND card2.loan-work
                           AND card2.open-date        LE iOpDate
                           AND (card2.end-date   GE iOpDate OR card2.end-date   EQ ?)
                           AND (card2.close-date GE iOpDate OR card2.close-date EQ ?)
                         NO-LOCK NO-ERROR.
        IF AVAILABLE card2 THEN DO:
           vSPCList = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "���⠏��", ";", iOpDate).
           IF NOT {assigned vSPCList} THEN DO:
              oErrMessage = SUBST("�������� ��� � �����.~n��� ᮧ�������� �������⥫쭮� ����� � ����஬ [&3] [&1,&2]" +
                                  "������� ��������� ������� �� ���� &4, �� �� ���� �� ����� �ਢ離� � ���.",
                                  card.contract, card.cont-code, card.doc-num, iOpDate).
              UNDO MAIN, LEAVE MAIN.
           END.
        END.
        /* ... ��⮬ �� �� �� �������, ࠭�� ����⢮����� */
        IF NOT AVAILABLE card2 THEN DO:
           FIND LAST card2 WHERE card2.parent-contract  = card.parent-contract
                             AND card2.parent-cont-code = card.parent-cont-code
                             AND card2.contract         EQ "card"
                             AND card2.cont-code        NE card.cont-code
                             AND card2.loan-work
                             AND card2.open-date        LE iOpDate
                           NO-LOCK NO-ERROR.
           IF NOT AVAILABLE card2 THEN DO:
              oErrMessage = SUBST("�������� ��� � �����.~n��� ᮧ�������� �������⥫쭮� ����� � ����஬ [&3] [&1,&2]" +
                                  "���������� ���� ������� �� ���� [&4]",
                                  card.contract, card.cont-code, card.doc-num, iOpDate).
              UNDO MAIN, LEAVE MAIN.
           END.
           vSPCList = GetLinks ("card", card2.contract + "," + card2.cont-code, ?, "���⠏��", ";", iOpDate).
        END.

        IF NOT {assigned vSPCList} THEN DO:
            oErrMessage = SUBST("�������� ��� � �����.~n��� ᮧ�������� �������⥫쭮� ����� � ����஬ [&3] [&1,&2]" +
                                "������� ������� ���� [&7] [&5,&6] �� ���� [&4], �� � ��� �� �ਢ易� ���.",
                                card.contract,  card.cont-code,  card.doc-num, iOpDate,
                                card2.contract, card2.cont-code, card2.doc-num).
            UNDO MAIN, LEAVE MAIN.
        END.

         /* �ਢ�뢠�� �� �������� ��� ������� ����� � ����� ���� */
         DO vi = 1 TO NUM-ENTRIES(vSPCList, ";"):
            vSPCSurr = ENTRY(vi, vSPCList, ";").
            ASSIGN
               vSPC    = ENTRY(1, vSPCSurr)
               vSPCCur = ENTRY(2, vSPCSurr)
            NO-ERROR.

            {find-act.i
               &bact = spc
               &acct = vSPC
               &curr = vSPCCur
            }
            IF NOT AVAILABLE spc THEN DO:
                oErrMessage = SUBST("�������� ��� � �����.~n��� [&1] ������� ����� � ����஬ [&2] [&3,&4] �� ������ � ��",
                                    vSPCSurr, card2.doc-num, card2.contract, card2.cont-code).
                UNDO MAIN, LEAVE MAIN.
            END.

            RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                                  spc.class-code,
                                                  card.contract + "," + card.cont-code,
                                                  loan.contract + "," + loan.cont-code,
                                                  iOpDate,
                                                  ?).
         END.
         LEAVE MAIN. /* �� */
      END.

      /* 2.2.2 �����⭮�� ����� ��� �� 㪠���� - ᠬ�� ᫮���� �����

         2.2.2.b ���� �᭮����
                 - �᫨ ��㣨� �᭮���� ���� ��� - ᮧ���� ��� � ����� �������
                 - ���� ��㣨� �᭮��� ����� - �롮� �।��⠢�塞 ���짮��⥫�
       */
      FIND FIRST card2 WHERE card2.parent-contract  = card.parent-contract
                         AND card2.parent-cont-code = card.parent-cont-code
                         AND card2.contract         EQ "card"
                         AND card2.cont-code        NE card.cont-code
                         AND card2.loan-work
                         AND card2.open-date        LE iOpDate
                         AND (card2.end-date   GE iOpDate OR card2.end-date   EQ ?)
                         AND (card2.close-date GE iOpDate OR card2.close-date EQ ?)
                       NO-LOCK NO-ERROR.
      IF AVAILABLE card2 THEN DO:
         oErrMessage = SUBST("�������� ��� � �����.~n�᫮��� ������� ����᪠�� ��᪮�쪮 ���, �� ��� ����� � ����஬ [&3] [&1,&2]" +
                             "�� 㪠����, ����� ��� �ਢ易��",
                             card.contract, card.cont-code, card.doc-num).
         UNDO MAIN, LEAVE MAIN.
      END.
      RUN cm_acct_cr IN h_acct ("acctp", FGetSetting("���", "���2���", ?),
                                loan.currency, card.cust-cat, card.cust-id, iOpDate,
                                OUTPUT vAcct, BUFFER spc,
                                ?, ?, ?,
                                "���",
                                USERID("bisquit"),
                                GetUserBranchId(USERID("bisquit")),
                                YES) NO-ERROR.
      IF NOT AVAILABLE spc OR ERROR-STATUS:ERROR THEN DO:
            oErrMessage = SUBST("�������� ��� � �����.~n�訡�� �� ᮧ����� ������ ���", ERROR-STATUS:GET-MESSAGE(1)).
            UNDO MAIN, LEAVE MAIN.
      END.

      RUN GetInitStatusSPCEx(BUFFER card,INPUT-OUTPUT mCrStatus).
      spc.acct-status = mCrStatus.
      vSPCNum         = GenNewSPCNumber (BUFFER card, iOpDate, "", OUTPUT vErr, OUTPUT oErrMessage).
      UpdateTempSignsEx (spc.class-code, spc.acct + "," + spc.currency, "��������", iOpDate, vSPCNum, ?).

      RUN CreateSPCLinks IN THIS-PROCEDURE (spc.acct + "," + spc.currency,
                                            spc.class-code,
                                            card.contract + "," + card.cont-code,
                                            loan.contract + "," + loan.cont-code,
                                            iOpDate,
                                            ?).
      LEAVE MAIN. /* �� */
   END.

   IF AVAIL spc THEN
      oAcctSurr = spc.acct + "," + spc.currency.

   RETURN.
END PROCEDURE. /* CreateCardSPCLink */
/*****************************************************************************/

PROCEDURE CreateSPCLinks.
   DEF INPUT  PARAM iAcctSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iAcctClass AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iCardSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iLoanSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iBegDate   AS DATE        NO-UNDO.
   DEF INPUT  PARAM iEndDate   AS DATE        NO-UNDO.

   DEF VAR vAcctSCS AS CHARACTER NO-UNDO.
   DEF VAR vCurrSCS AS CHARACTER NO-UNDO.
   DEF VAR vNotPers AS LOGICAL   NO-UNDO.


   DEF BUFFER loan FOR loan.
   DEF BUFFER card FOR loan.

   FIND FIRST loan WHERE loan.contract  EQ ENTRY(1, iLoanSurr)
                     AND loan.cont-code EQ ENTRY(2, iLoanSurr)
      NO-LOCK NO-ERROR.
   FIND FIRST card WHERE card.contract  EQ ENTRY(1, iCardSurr)
                     AND card.cont-code EQ ENTRY(2, iCardSurr)
      NO-LOCK NO-ERROR.
   IF AVAIL card AND AVAIL loan THEN
   DO:
      /* ���� � ���⮩ */
      RUN CreateLinks IN h_xclass (iAcctClass,
                                   "���⠏��",
                                   iAcctSurr,
                                   iCardSurr,
                                   iBegDate,
                                   iEndDate,
                                   ?).

      

      /* ���� � ��⮬ ���, �᫨ �����ᮭ. ���� � ������ �����, 
      � ��� ��� ��� */
      vNotPers = fNotPersCard(card.sec-code).
      IF (vNotPers AND card.loan-status NE fGetSetting("���","��⊠�⁥����","���")) OR NOT vNotPers THEN
      DO:
         RUN GetRoleAcct IN THIS-PROCEDURE (iLoanSurr,
                                         iBegDate,
                                         "SCS",
                                         loan.currency,
                                         OUTPUT vAcctSCS,
                                         OUTPUT vCurrSCS).
         RUN CreateLinks IN h_xclass (iAcctClass,
                                      "�����",
                                      iAcctSurr,
                                      vAcctSCS + "," + vCurrSCS,
                                      iBegDate,
                                      iEndDate,
                                      ?).
      END.
      
      /* ���� � ������ ������஬ */
      UpdateSigns (iAcctClass,
                   iAcctSurr,
                   "loan-surrogate",
                   iLoanSurr,
                   ?).
   END.

END PROCEDURE.
/*****************************************************************************/

PROCEDURE Chk_CardType.
   DEF INPUT  PARAM iLoanSurr  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iLoanType  AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iDate      AS DATE        NO-UNDO.
   DEF OUTPUT PARAM oErrMes    AS CHARACTER   NO-UNDO.

   DEF VAR vBankProd    AS CHARACTER   NO-UNDO.
   DEF VAR vValidTypes  AS CHARACTER   NO-UNDO.

   vBankProd = GetXAttrValue ("loan",
                              iLoanSurr,
                              "�����த��").
   IF {assigned vBankProd} THEN
   DO:
      vValidTypes = getTCodeFld ("Val",
                                 "�����த���",
                                 vBankProd,
                                 iDate).
      IF {assigned vValidTypes} THEN
      DO:
         IF NOT CAN-DO(vValidTypes, iLoanType) THEN
            oErrMes = "��� ����� 㪠���� �� ������� <" + iLoanSurr
                         + "> �� ᮮ⢥����� �����⨬� <" + vValidTypes
                         + "> � �����䨪��� ���� ������᪮�� �த�� <"
                         + vBankProd + ">.".
      END.
   END.

END PROCEDURE.
/*****************************************************************************/

PROCEDURE GenNewCardNumber:
DEF INPUT  PARAM iClassCode AS CHARACTER   NO-UNDO.
DEF INPUT  PARAM iBranchID  AS CHARACTER   NO-UNDO.
DEF INPUT  PARAM iDate      AS DATE        NO-UNDO.
DEF OUTPUT PARAM oNumber    AS CHARACTER   NO-UNDO.
DEF OUTPUT PARAM oMessage   AS CHARACTER   NO-UNDO.

   DEF VAR       vTemplate  AS CHARACTER   NO-UNDO.
   DEF VAR       vCounter   AS CHARACTER   NO-UNDO.
   DEF VAR       vCounter2  AS CHARACTER   NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:
      /* ����砥� 蠡��� ����� � �����, ���稪 ��� �� ������� */
      RUN GetClassTemplatePars IN h_jloan (       iClassCode,
                                           OUTPUT vTemplate,
                                           OUTPUT vCounter).

      IF NOT {assigned vTemplate} THEN
      DO:
         oMessage = "���������� ��।����� 蠡��� ����� ����� �� ����� <" + iClassCode + ">".
         UNDO MAIN, LEAVE MAIN.
      END.

      /* ��⠥��� �� �����䨪��� ������� ������� ���稪 �� ���� ���ࠧ�������.
      ** �᫨ ⠪�� ���稪 �� ������ - ��६ vCounter.
      ** �᫨ �� ���� �� ���稪�� �� ������ - �����頥� ? */

      vCounter2 = GetCode("�������", iBranchID).

      IF     NOT {assigned vCounter}
         AND NOT {assigned vCounter2} THEN
      DO:
         oMessage = "���������� ��।����� �� ���稪 ��� ���ࠧ������� <" + iBranchID + ">. �� ���稪 �� ����� <" + iClassCode + ">".
         UNDO MAIN, LEAVE MAIN.
      END.

      ASSIGN
         vTemplate = ReplaceBasicTags (vTemplate, (IF {assigned vCounter2} THEN vCounter2 ELSE vCounter), iDate)
         vTemplate = ReplaceTag (vTemplate, "�", iBranchID , YES)
         oNumber   = vTemplate
      .
      IF NOT {assigned oNumber} THEN
      DO:
         oMessage = "���������� ��ନ஢��� ����� ��� �����, �஢����, ࠡ�⠥� �� �㦡� ���稪��".
         UNDO MAIN, LEAVE MAIN.
      END.
   END.

   RETURN.
END PROCEDURE.
/*****************************************************************************/

PROCEDURE CalcCardEndDate:
DEF INPUT  PARAM iCardType  AS CHAR NO-UNDO.
DEF INPUT  PARAM iDateBegin AS DATE NO-UNDO.
DEF OUTPUT PARAM oDateEnd   AS DATE NO-UNDO.
DEF OUTPUT PARAM oMessage   AS CHAR NO-UNDO.

   DEF VAR       mResult    AS CHAR NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO, LEAVE:

      IF iCardType EQ ""
      THEN DO:
         oMessage = "�� 㪠��� ⨯ �����".
         LEAVE MAIN.
      END.

      mResult = GetCodeMisc("����끠���", iCardType, 3).

      IF mResult EQ ? THEN          /* �᫨ ��� �����䨪��� �� ������ */
      DO:
         oMessage = "��� ����� [" + mResult + "] �� ������ � �����䨪��� ����끠���".
         LEAVE MAIN.
      END.
      IF mResult          NE "" AND   /* �᫨ ���祭�� ���� �����䨪��� ����୮ */
         INT64(mResult) EQ ?
      THEN DO:
         oMessage = "�����४⭮ 㪠��� �ப ⨯� ����� [" + mResult + "] � �����䨪��� ����끠���".
         LEAVE MAIN.
      END.
      IF iDateBegin NE ? THEN
         oDateEnd = date_correct(MONTH(iDateBegin), INT64(mResult), 31, YEAR(iDateBegin)).
      ELSE
         oMessage = "�� ��⠭������ ��� ������ �����.".
   END.
END PROCEDURE.
/*****************************************************************************/
/*==========================================================================*/
PROCEDURE GetSPCMethod:
   DEF INPUT  PARAM iCardContract AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iCardContCode AS CHARACTER   NO-UNDO.
   DEF OUTPUT PARAM oSPCMeth      AS CHARACTER   NO-UNDO.
   DEF OUTPUT PARAM oErrMes       AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vCardType    AS CHARACTER    NO-UNDO.

   DEFINE BUFFER bCode     FOR code.
   DEFINE BUFFER bCard     FOR Loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bcard WHERE bcard.contract  EQ iCardContract
                         AND bcard.cont-code EQ iCardContCode
      NO-LOCK NO-ERROR.

      /* ���� ⨯� ����� � �����䨪��� "����끠���" */
      vCardType = GetCodeMisc ("����끠���",
                              bcard.sec-code,
                              2).
      IF NOT {assigned vCardType} THEN
      DO:
         oErrMes = "��������� ⨯ ����� <" + bcard.sec-code + "> � �����䨪��� <����끠���>.".
         UNDO MAIN, LEAVE MAIN.
      END.

        /* ���� ����ᨭ�� � �����䨪��� "����ᨭ��" */
      FIND FIRST bCode WHERE bCode.CLASS  EQ "����ᨭ��"
                        AND bCode.CODE   EQ vCardType
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bCode THEN
      DO:
         oErrMes = "�� ������ �����䨪��� � ����� <" + vCardType + "> 㪠���� � ⨯� ����� <" + bcard.sec-code + ">.".
         UNDO MAIN, LEAVE MAIN.
      END.

      oSPCMeth = GetXAttrValueEx ("CODE",
                                  bCode.CLASS + "," + bCode.CODE,
                                  "�����⮤",
                                  GetXAttrInit("Processing", "�����⮤")).


   END.
END PROCEDURE. /* GetSPCMethod */

/* �஢�ઠ �����⨬��� ४������ ��樨 ��� ����� */
FUNCTION IsAdvAllowed RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                       INPUT iContCode AS CHARACTER,
                                       INPUT iAdv      AS CHARACTER):

   DEF VAR vRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER bCode FOR code.
   DEF BUFFER card  FOR loan.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST card WHERE card.contract  EQ iContract
                        AND card.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL card THEN
         UNDO MAIN, LEAVE MAIN.

      FIND FIRST bCode WHERE bCode.class EQ "������樨��"
                         AND bCode.code  EQ iAdv
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bCode THEN
         UNDO MAIN, LEAVE MAIN.

      /* �஢�ઠ 䨫���� �� ���� */
      IF     {assigned bCode.misc[1]}
         AND NOT CAN-DO(bCode.misc[1], card.filial-id) THEN
         UNDO MAIN, LEAVE MAIN.

      FIND FIRST bLoan WHERE bLoan.contract  EQ card.parent-contract
                         AND bLoan.cont-code EQ card.parent-cont-code
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.

      /* �஢��� ������᪮�� �த�� ������� ����� */
      IF     {assigned bCode.misc[2]}
         AND NOT CAN-DO(bCode.misc[2], GetXAttrValueEx("loan",
                                                       bLoan.contract + "," + bLoan.cont-code,
                                                       "�����த��",
                                                       "?")) THEN
         UNDO MAIN, LEAVE MAIN.

      /* �஢�ઠ ⨯� ����� */
      IF     {assigned bCode.misc[3]}
         AND NOT CAN-DO(bCode.misc[3], card.sec-code) THEN
         UNDO MAIN, LEAVE MAIN.

      vRetVal = YES.
   END.

   RETURN vRetVal.
END FUNCTION.

/* �஢�ઠ, ���� �� ���� ��௫�⭮� */
FUNCTION IsGrCard RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                   INPUT iContCode AS CHARACTER):

   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   DEF VAR vRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER card  FOR loan.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST card WHERE card.contract  EQ iContract
                        AND card.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL card THEN
         UNDO MAIN, LEAVE MAIN.

      FIND FIRST bLoan WHERE bLoan.contract  EQ card.parent-contract
                         AND bLoan.cont-code EQ card.parent-cont-code
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.

      vLinks = GetLinks (bLoan.Class-Code,
                         bLoan.contract + "," + bLoan.cont-code,
                         ?,
                         "��������⥫�",
                         ";",
                         card.open-date).
      IF {assigned vLinks} THEN
         vRetVal = YES.
   END.

   RETURN vRetVal.
END FUNCTION.

/* �஢�ઠ, ���� �� ������� ��௫��� */
FUNCTION IsGrLoan_stnd RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                        INPUT iContCode AS CHARACTER,
                                        INPUT iDate     AS DATE):
   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   DEF VAR vRetVal AS LOGICAL     NO-UNDO.
   DEF VAR vOk     AS LOGICAL     NO-UNDO.
   DEF VAR vProc   AS CHARACTER   NO-UNDO.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
      vLinks = GetLinks (bLoan.Class-Code,
                         bLoan.contract + "," + bLoan.cont-code,
                         "t",
                         "��������⥫�",
                         ";",
                         iDate).
      IF {assigned vLinks} THEN
         vRetVal = YES.
   END.
   RETURN vRetVal.
END FUNCTION.

FUNCTION IsGrLoan RETURNS LOGICAL (INPUT iContract AS CHARACTER,
                                   INPUT iContCode AS CHARACTER,
                                   INPUT iDate     AS DATE):

   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   DEF VAR vRetVal AS LOGICAL     NO-UNDO.
   DEF VAR vOk     AS LOGICAL     NO-UNDO.
   DEF VAR vProc   AS CHARACTER   NO-UNDO.
   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
      vProc = Get-Class-Method(bLoan.Class-Code,"IsGrLoan").
      IF {assigned vProc} AND vProc NE "?" THEN
      DO:
         OUTPUT TO "errmsg.txt" KEEP-MESSAGES.
         vOk = NO.
         DO ON STOP UNDO,LEAVE
            ON ERROR UNDO, LEAVE:
            RUN VALUE(vProc)(iContract,iContCode,iDate,OUTPUT vRetVal) NO-ERROR.
            vOk = YES.
         END.
         OUTPUT CLOSE.
         IF vOk <> YES OR ERROR-STATUS:ERROR THEN
         DO:
           MESSAGE
              PROGRAM-NAME(1) SKIP
              "�訡�� �맮�� ��⮤� IsGrLoan ��� ����� " + bLoan.Class-Code SKIP
              ERROR-STATUS:GET-MESSAGE(1)
              VIEW-AS ALERT-BOX.
           vRetVal = ?.
         END.
      END.
      ELSE
         vRetVal = IsGrLoan_stnd(iContract,iContCode,iDate).
   END.

   RETURN vRetVal.
END FUNCTION.

PROCEDURE cpers_isgrrshb:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER bLoan FOR loan.
   DEF BUFFER tmp-code FOR tmp-code.
   DEF VAR vLinkId  AS INT64   NO-UNDO.
   DEF VAR vCode AS CHAR NO-UNDO.
   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
      vCode = GetXattrValue("loan",iContract + "," + iContCode,"�����த��").
      FIND LAST tmp-code WHERE tmp-code.class    EQ "�����த���"
                           AND tmp-code.code     EQ vCode
                           AND tmp-code.beg-date <= iDate
      NO-LOCK NO-ERROR.
      RUN GetXLink IN h_xclass ("card-loan-gr",
                                "��������⥫�",
                                OUTPUT vLinkId,
                                BUFFER xlink).
      oRetVal =
         (AVAIL tmp-code AND
          GetXattrValue("tmp-code",GetSurrogate("tmp-code",ROWID(tmp-code)),"��⥣���") = "��௫���" AND
         CAN-FIND(FIRST links  WHERE
                        links.link-id   EQ vLinkId
                    AND links.target-id EQ bLoan.contract + "," + bLoan.cont-code
                    AND links.beg-date < iDate
                    AND links.end-date < iDate
                    AND links.end-date <> ?)
         )
         OR
         IsGrLoan_stnd(iContract,iContCode,iDate).
   END.
END PROCEDURE.

PROCEDURE cpers_isgr:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRetVal AS LOGICAL     NO-UNDO.

   oRetVal = IsGrLoan_stnd(iContract,iContCode,iDate).
END PROCEDURE.


PROCEDURE ccorp_isgr:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRetVal AS LOGICAL     NO-UNDO.

   DEF BUFFER bLoan FOR loan.
   DEF VAR vLinks  AS CHARACTER   NO-UNDO.
   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      FIND FIRST bLoan WHERE bLoan.contract  EQ iContract
                         AND bLoan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         UNDO MAIN, LEAVE MAIN.
   END.
END PROCEDURE.

PROCEDURE GetInitStatusSPCEx:
   DEFINE PARAMETER BUFFER card           FOR loan.
   DEFINE INPUT-OUTPUT PARAMETER oStatus  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE mInitStatus   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mTypeCard     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mPC           AS CHARACTER  NO-UNDO.

   mInitStatus = oStatus.

   /* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

      IF NOT AVAILABLE card THEN
      DO:
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.

      mTypeCard = card.sec-code.
      mPC       = GetCodeMisc("����끠���",mTypeCard,2).
      oStatus   = GetXAttrValueEx("code","����ᨭ��," + mPC ,"InitStatusSPC",mInitStatus).
   END.
   /* *************************  End of Main Block  ********************** */
END PROCEDURE.
