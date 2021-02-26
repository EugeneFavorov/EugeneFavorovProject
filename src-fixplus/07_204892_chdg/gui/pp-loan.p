{globals.i}

/* +++ pp-loan.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-LOAN.P
      Comment: �����㬥��� ��� ࠡ��� � ⠡��窮� loan �
               ������� ��� �� �।��� ������ࠬ.
   Parameters: ���
         Uses:
      Used by:
      Created: 01/11/01 Om
     Modified: 04.05.2004 16:48 KSV      (0029735) ��������� ��楤��
                                         MakePureLoanInt.
     Modified: 05.05.2004 19:46 KSV      (0029735) ��ࠢ���� �訡�� �맮��
                                         Fill-SysMes.
     Modified: 14.07.2004 17:28 KSV      (0032906) � ��楤��� NextContCode
                                         ��������� ��࠭���� ⥪�饣� ���祭��
                                         ���稪� deal-id ��� ��� ����������
                                         ����⠭�������.
     Modified: 12.10.2007 16:36 koch     <comment>
*/

FORM "~n@(#) pp-loan.p 1.0 Om 01/11/01"
WITH frame sccs-id stream-io width 250.

DEF NEW SHARED VAR mask AS CHAR NO-UNDO INIT ?.

&GLOB  CodPar '����᭄���,����᭏�'
{globals.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get lv}
{intrface.get count}
{intrface.get refer}
{intrface.get i254}
{brwdpdsr.def} /* �����㬥��� ��� ࠡ��� � �� ������� */
{pick-var.i}
{ln-dep.i}
{t-otch.i NEW} /* ������� ⠡��窨 otch1 */
{par_mass.i} /* ���ᨢ ��� ���� ��ࠬ��஢ ������� */
{savepars.i}
{tt-transh.def NO-UNDO} /* ⠡��� ��� ࠧ��᪨ �� �࠭蠬 */

DEFINE TEMP-TABLE ttIndicate1 LIKE ttIndicate.
DEFINE TEMP-TABLE ttIndicate2 LIKE ttIndicate.

{pfuncdef 
   &NAME="Loan"
   &LIBDEF=TRUE
   &LibName="������⥪� �-権 ࠡ��� � ������ࠬ�"
}


/*------------------------------------------------------------------------------
  Purpose:     �����頥� 㭨����� �����䨪��� ᤥ��� ��� ����� ���.
               ᮣ��襭�� + ���浪��� �����.
  Parameters:  iContCode   - ����� ���. ᮣ��襭�� (����� ��䨪� �
                             ����� �ନ����� �����)
               iLenght     - ����� �����
  Notes:
------------------------------------------------------------------------------*/
FUNCTION NextContCode RETURNS CHARACTER (iContCode AS CHAR,
                                         iLength   AS INT64 ):

   DEF VAR vCode AS INT64 NO-UNDO.

   DEF BUFFER signs FOR signs.

   vCode = NEXT-VALUE(deal-id).

   /* Commented by KSV: ���࠭塞 ��᫥���� ���祭�� ���稪� � ᯥ樠�쭮�
   ** ���. ४�����. �� ����室��� �� ��砩, �᫨ ⥪�饥 ���祭�� ���稪�
   ** �㤥� ����ﭮ � ��� ����室��� �㤥� ����⠭�����. ��㣨� ��堭�����
   ** ��� ����⠭�������, � ᮦ������, ���. ��堭��� ����᪠�� ��������
   ** ����譮���, �� �� ����� �७����� */
   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON QUIT UNDO TR,LEAVE TR:
      FIND FIRST signs WHERE
         signs.file-name   = "sequence" AND
         signs.surrogate   = "deal-id"  AND
         signs.code        = "deal-id"  EXCLUSIVE NO-WAIT NO-ERROR.

      IF LOCKED signs THEN UNDO TR,LEAVE TR.

      IF NOT AVAILABLE signs THEN
      DO:
         CREATE signs.
         ASSIGN
            signs.file-name   = "sequence"
            signs.surrogate   = "deal-id"
            signs.code        = "deal-id" .
      END.
      signs.code-value  = STRING(vCode).
   END.  /* End of TR BLOCK */

   RETURN iContCode +
          IF iLength = 0 THEN
             STRING(vCode)
          ELSE
             FILL("0",iLength - LENGTH(STRING(vCode))) + STRING(vCode).

END FUNCTION.

{form.def}
{lshpr.pro}
{loan.pro}
{loanint.pro}
{loan-srk.pro}
{jloan_counters.i}
{clcprmdog.i} /* ��।������ �㭪樨 CalcPrmValue ��� ALL_PARAM */
{dtterm.i}

FUNCTION GetContCode RETURNS CHARACTER (iContract AS CHAR,iContCode AS CHAR):
   DEFINE VARIABLE vRet AS CHARACTER NO-UNDO INIT ?.
   {&GET-LOAN}
   vRet = IF {assigned loan.doc-num} THEN loan.doc-num ELSE loan.doc-ref.
   RETURN vRet.
END FUNCTION.
/* �����頥� ��� ��ࠬ��� �᭮����� �����, �㭪�� ����室��� �ᯮ��-
������ � ��⮤�� ���� %% �� ��ࠬ��ࠬ, ⠪ ��� ���
�� ���祭� ��� �।���*/


FUNCTION GetParCode RETURNS INT64 (iClass AS CHAR,iCod AS CHAR):
  DEF VAR vCod AS INT64 NO-UNDO .
  vCod =  INT64(GetXattrInit(iClass,iCod)) NO-ERROR.
  IF ERROR-STATUS:ERROR OR vCod = ?
  THEN IF iCod = ENTRY(1,{&CodPar})
       THEN vCod = 0.
       ELSE vCod = 4.
  RETURN vCod .
END FUNCTION.

FUNCTION GetFstWrkDay RETURNS DATE (
   INPUT iClass-Code AS CHAR,     /* ����� ��ꥪ� */
   INPUT iUser-id    AS CHAR,     /* ����㤭�� */
   INPUT in-date     AS DATE,     /* ��� ��砫� ���᪠ */
   INPUT iDelay      AS INT64,  /* ������⢮ ���� ���ࢠ�� ���� */
   INPUT iDirect     AS INT64   /* ���ࠢ�����: < 0 - ���� ����� �� �������� ����
                                   **             > 0 - ���� �.�. ���। */
):

   DEF VAR vBranch   AS CHAR        NO-UNDO.   /* ���ࠧ������� �� (�� �ਮ����) �������/��.���/���짮��⥫� */
   DEF VAR vLoanGr   AS CHAR        NO-UNDO.   /* ��䨪 ࠡ��� � ����� ����ᮬ ������஢ */
   DEF VAR vi        AS INT64         NO-UNDO.   /* ���稪 横�� */
   DEF VAR vResDate  AS DATE        NO-UNDO.   /* �����頥��� ���祭�� */

   /* ������ ���ࠧ������� ���짮��⥫� ������襣� ������� */
   vBranch = GetXAttrValue("_user", iUser-id, "�⤥�����").
   /* ��।���� ��䨪 ࠡ��� � ����� ����ᮬ ������஢ */
   vLoanGr = GetXAttrInit(iClass-Code, "WorkGraf").

   CYCLE:
   DO vi = 0 TO iDelay:
      vResDate = in-date + vi * iDirect.
      IF        {holiday.i vResDate}                /* ��室�. �� �������� */
         OR NOT IsWorkDayBranch(vResDate, vBranch)  /* ��� NO(��室�.) �� ���. ���ࠧ�.*/
         OR NOT IsWorkDayGraf(vResDate, vLoanGr)    /* ��� NO(��室�.) �� ��䨪� �������*/
      THEN DO:
         vResDate = ?.
         NEXT CYCLE.     /* ���室�� � ᫥���饩 ��� */
      END.
      ELSE LEAVE CYCLE.
   END.  /* of CYCLE block */

   /* �᫨ ����⪠ ���᪠ �� �ਭ�᫠ १���� � �����頥� ���. ���� */
   vResDate = IF vResDate EQ ? THEN in-date ELSE vResDate.
   RETURN vResDate.
END FUNCTION.

/*!!! �� ���ꥬ� � �⠭���� ��७��� � pp-loan !!! (��७�᫨!) */
FUNCTION GetPercentRate RETURNS DECIMAL
   (iDscBegDate AS DATE,     /* ��砫� ��ਮ�� ��᪮��஢���� */
    iDscEndDate AS DATE,     /* ����砭�� ��ਮ�� ��᪮��஢���� */
    iDscRate    AS DECIMAL): /* �⠢�� ��᪮��஢���� */

   DEFINE VARIABLE vDiscBeg AS DATE    NO-UNDO.
   DEFINE VARIABLE vDiscEnd AS DATE    NO-UNDO.
   DEFINE VARIABLE vDiscTmp AS DATE    NO-UNDO.
   DEFINE VARIABLE vDays    AS INT64 NO-UNDO.
   DEFINE VARIABLE vYear    AS INT64 NO-UNDO.
   DEFINE VARIABLE vPerCent AS DECIMAL NO-UNDO.

   ASSIGN
      vDiscBeg = iDscBegDate + 1
      vDiscEnd = iDscEndDate
      .
   DO WHILE vDiscBeg LE vDiscEnd:
      ASSIGN
         vYear    = YEAR(vDiscBeg)
         vDiscTmp = DATE(12,31,vYear)
         vDays    = vDiscTmp - DATE(1,1,vYear) + 1
         .
      IF vDiscTmp GT vDiscEnd THEN vDiscTmp = vDiscEnd.
      vPerCent = vPerCent + iDscRate * (vDiscTmp - vDiscBeg + 1) / vDays.
      IF vDiscBeg EQ vDiscEnd THEN LEAVE.
      vDiscBeg = vDiscTmp + 1.
   END.

   RETURN vPerCent.

END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ������� ����७��� ����樨 �� ������� ��� �ਢ離� � ���.
               �஢����
  Parameters:  iContract   - �����祭�� �������
               iContCode   - �����䨪��� ������
               iCodOp      - ��� ����樨
               iDate       - ��� ����樨
               iAmount     - �㬬� ����樨
               iAvt        - �ਧ��� ���. ����樨
               oOk         - 䫠� �����襭��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE MakePureLoanInt:
   DEFINE INPUT  PARAMETER iContract AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCodOp    AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iAmount   AS DECIMAL    NO-UNDO.
   DEFINE INPUT  PARAMETER iAvt      AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk       AS LOGICAL    .

   DEFINE BUFFER loan-int FOR loan-int.

   DEFINE VARIABLE incontr    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cur-date   AS DATE       NO-UNDO.
   DEFINE VARIABLE vOperMask  AS CHARACTER  NO-UNDO.

   ASSIGN
      incontr  = iContract
      cur-date = iDate.

   {&GET-LOAN}

   IF loan.open-date > iDate THEN
   DO:
      RUN Fill-SysMes("","16L","","%s=" + iContract      +
                                  "%s=" + iContCode).
      RETURN.
   END.

   vOperMask = GetXattrInit(loan.class-code,"���Ꭿ��").

   IF NOT {assigned vOperMask} THEN vOperMask = "*".
   IF NOT CAN-DO(vOperMask,STRING(iCodOp)) THEN
   DO:
      RUN Fill-SysMes("","13L","","%s=" + STRING(iCodOp) +
                                  "%s=" + iContract      +
                                  "%s=" + iContCode).
      RETURN.
   END.


   FIND FIRST chowhe WHERE
      chowhe.id-op = iCodOp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE chowhe THEN
   DO:
      RUN Fill-SysMes("","14L","","%s=" + STRING(iCodOp)).
      RETURN.
   END.

   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON QUIT UNDO TR,LEAVE TR:
      {cr-lint.i
         &df    = "/*"
         &id-d  = "chowhe.id-d"
         &id-k  = "chowhe.id-k"
         &s     = "iAmount"
         &avt   = "iAvt"
         &ruch  = "/*"
         &off   = "/*"
       }
       ASSIGN
          loan-int.user-id = USERID("bisquit").
      oOk = YES.
   END.  /* End of TR BLOCK */

   IF oOk <> YES THEN
      RUN Fill-SysMes("","15L","","%s=" + STRING(iCodOp) +
                                  "%s=" + iContract      +
                                  "%s=" + iContCode).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:    �� �����ᮢ��� ���� ��� �� �������,
              � ������ ��� �᭮����� ����� �ਭ������� �⮬� �����ᮢ���,
              ��᫥ 祣� ��� ������� ������� ����� ��� � 㪠������
              ஫��, �� ���祭��� ��������� ��⮢ ��⠢����� ᯨ᮪
              �����ᮢ�� ��⮢, ����� �����頥���.

              ���ᬠ�ਢ����� ������� � ����ᨬ��� �� �����祭��.

  Parameters:  iOrigBalAcct - �����ᮢ� ��� �᭮����� �����
               iLoanPurpose - �����祭�� ࠧ����ਢ����� ������஢
               iAcctRole    - �᪮��� ஫�
               oBalAcctList - ����祭�� ᯨ᮪ �����ᮢ�� ��⮢

  Notes:
------------------------------------------------------------------------------*/
PROCEDURE GetBalAcctsOfRole:
DEFINE INPUT  PARAMETER iOrigBalAcct  AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iLoanContract AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iAcctRole     AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iOpDate       AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER oBalAcctList  AS CHAR NO-UNDO INIT "".

DEFINE BUFFER b-acct       FOR acct.
DEFINE BUFFER bb-acct      FOR acct.
DEFINE BUFFER b-loan-acct  FOR loan-acct.
DEFINE BUFFER bb-loan-acct FOR loan-acct.
DEFINE BUFFER b-loan       FOR loan.

DEFINE VAR vRoleList AS CHAR NO-UNDO INIT "".

   IF iLoanContract = "dps" THEN
     vRoleList = "loan-dps-p,loan-dps-t,loan-dps-ts,loan-dps-tsk".

   FOR EACH b-acct      WHERE b-acct.bal-acct      =  INT64(iOrigBalAcct) NO-LOCK,
       EACH b-loan-acct WHERE b-loan-acct.acct     =  b-acct.acct
                          AND b-loan-acct.currency =  b-acct.currency
                          AND b-loan-acct.contract =  iLoanContract         NO-LOCK:
       /* �ࠧ� ��ᥪ��� ���㦭� ��� (� �����室�饩 ஫��) */
       IF iLoanContract = "dps" THEN DO:
          IF NOT CAN-DO(vRoleList,b-loan-acct.acct-type) THEN NEXT.
       END.
       ELSE DO:
          IF NOT CAN-DO(GetMainAcctRole(b-loan-acct.contract, b-loan-acct.cont-code),
                        b-loan-acct.acct-type) THEN NEXT.
       END.

       FIND FIRST b-loan OF b-loan-acct
                         /* ��������� ������� */
                         WHERE b-loan.open-date  <= iOpDate
                           AND (b-loan.close-date = ? OR b-loan.close-date >= iOpDate)
                         NO-LOCK NO-ERROR.
       IF NOT AVAILABLE b-loan THEN NEXT.

       /* ������ ������� ��� �筮 ���室�� -
          �饬 ��� � ஫�� iAcctRole � ��� �����ᮢ� ���  */
       FIND LAST bb-loan-acct OF    b-loan
                              WHERE bb-loan-acct.acct-type =  iAcctRole
                                AND bb-loan-acct.since     <= iOpDate
                              NO-LOCK NO-ERROR.
       IF NOT AVAILABLE bb-loan-acct THEN NEXT.
       FIND FIRST bb-acct OF bb-loan-acct NO-LOCK NO-ERROR.
       IF NOT AVAILABLE bb-acct THEN NEXT.

       IF LOOKUP(STRING(bb-acct.bal-acct,'99999'),oBalAcctList) <> 0 THEN NEXT.

       {additem.i oBalAcctList STRING(bb-acct.bal-acct,'99999')}
       oBalAcctList = oBalAcctList + "," + b-loan.cont-code.

   END. /* for each b-acct, each b-loan-acct */

END PROCEDURE. /* END OF PROCEDURE GetBalAcctOfRole */

/*------------------------------------------------------------------------------
  Purpose:     ���� ����室����� ���㣫���� ���⥦��
  Parameters:  iContract - �����䨪���
               iContCode - �������
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetRoundFlag RETURNS LOG
   (iContract AS CHAR,
    iContCode AS CHAR):

   DEF VAR vStr AS CHAR NO-UNDO.

   {&GET-LOAN}

   vStr = GetXAttrValueEx("loan",iContract + "," + iContCode,"round",?).
   IF vStr <> ? THEN
      RETURN (vStr = "��").
   ELSE
   DO:
      vStr = FGetSetting("����",?,?).
      IF vStr <> ?
      THEN
         RETURN ( IF loan.currency EQ ""
                 THEN INDEX(vStr,"��悠�")      <> 0
                 ELSE INDEX(vStr,loan.currency) <> 0).
      ELSE
         RETURN(NO).
   END.

END FUNCTION.

/* ���� �������饩 �奬� �� �������� �� ���� */
PROCEDURE GET_CURRENT_LOAN_SCHEME.

   DEF INPUT  PARAM iScheme AS CHAR          NO-UNDO. /* ��� �奬� ���᫥��� */
   DEF INPUT  PARAM iDate   AS DATE          NO-UNDO. /* ��� */
   DEF OUTPUT PARAM oSchRid AS RECID INIT ?  NO-UNDO. /* RecId �奬� */

   DEF BUFFER shc_line FOR interest-sch-line.

   FOR EACH shc_line WHERE
            shc_line.interest-sch EQ iScheme
        AND shc_line.since        LE iDate
   NO-LOCK BY shc_line.since  DESC :  /* ���������� ����� , find last �ࠫ �� ����� � ⠬ �� ����� ���  ���뢠��� !!! */
      oSchRid = RECID(shc_line).
      LEAVE.
   END.
END PROCEDURE.

/* ���� ���� ����砭�� ⥪�饩 �奬� (���� ᫥���饩 �奬�) */
PROCEDURE GET_NEXT_DATE_LOAN_SCHEME.
   DEF INPUT  PARAM iScheme AS CHAR        NO-UNDO. /* ��� �奬� ���᫥��� */
   DEF INPUT  PARAM iDate   AS DATE        NO-UNDO. /* ��� */
   DEF OUTPUT PARAM oDate   AS DATE INIT ? NO-UNDO. /* RecId �奬� */

   DEF BUFFER shc_line FOR interest-sch-line.
   /* ���� �� ���᭨� �� ����॥
   FOR EACH  shc_line where
             shc_line.interest-sch = iScheme
         AND shc_line.since        > iDate
   NO-LOCK BY shc_line.since:
      oDate = shc_line.since.
      LEAVE.
   END.
   */
   FOR FIRST shc_line WHERE
             shc_line.interest-sch = iScheme
         AND shc_line.since        > iDate
   NO-LOCK:
      oDate = shc_line.since.
   END.
END PROCEDURE.

PROCEDURE GET_COMM_LOAN_BUF:

   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iComm      AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.
   DEF PARAM BUFFER comm-rate FOR comm-rate.

   RELEASE comm-rate.

   FOR LAST comm-rate WHERE
            comm-rate.commission = iComm
        AND comm-rate.acct       = "0"
        AND comm-rate.kau        = iContract + "," + iContCode
        AND comm-rate.since     <= iDate
      USE-INDEX kau NO-LOCK:
      RETURN.
   END.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ���� �������饩 �����ᨨ �� �।�⭮�� ��������
  Parameters:  iContract - �����䨪���
               iContCode - �������
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GET_COMM_LOAN RETURNS DEC
  (INPUT iContract  AS CHAR,   /* �����䨪��� */
   INPUT iContCode  AS CHAR,   /* �������      */
   INPUT iComm      AS CHAR,   /* ��� �����ᨨ. */
   INPUT iDate      AS DATE):  /* ��� ���᪠.  */

   DEF BUFFER comm-rate FOR comm-rate.

   RUN GET_COMM_LOAN_BUF(iContract,iContCode,iComm,iDate,BUFFER comm-rate).

   RETURN ( IF AVAIL  comm-rate
           THEN  comm-rate.rate-comm
           ELSE ?).

END FUNCTION.
/*------------------------------------------------------------------------------
  Purpose:     ���� �������饩 �����ᨨ �� �।�⭮�� ��������
  Parameters:  iContract - �����䨪���
               iContCode - �������
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GET_NEXT_COMM_DATE_LOAN RETURNS DATE
  (INPUT iContract  AS CHAR,   /* �����䨪��� */
   INPUT iContCode  AS CHAR,   /* �������      */
   INPUT iComm      AS CHAR,   /* ��� �����ᨨ. */
   INPUT iBegDate   AS DATE,   /*  ���ࢠ�    */
   INPUT iEndDate   AS DATE):  /*  ���᪠.     */

   DEF BUFFER comm-rate FOR comm-rate.

   FOR FIRST comm-rate WHERE
             comm-rate.commission = iComm
         AND comm-rate.acct       = "0"
         AND comm-rate.kau        = iContract + "," + iContCode
         AND comm-rate.since      > iBegDate
         AND comm-rate.since     <= iEndDate
   USE-INDEX kau NO-LOCK:
      RETURN comm-rate.since.
   END.
   RETURN ?.

END FUNCTION.

FUNCTION GET_MAIN_COMM_RATE RETURNS DEC
  (INPUT iContract  AS CHAR,   /* �����䨪��� */
   INPUT iContCode  AS CHAR,   /* �������      */
   INPUT iComm      AS CHAR,   /* ��� �����ᨨ. */
   INPUT iDate      AS DATE):  /* ��� ���᪠.  */

   DEF VAR vRec AS RECID NO-UNDO .
   DEF BUFFER comm-rate FOR comm-rate.
   DEF BUFFER loan FOR loan.

   vRec = GetLoanRecid(iContract,iContCode).
   RUN GetMainLnId(INPUT vRec, OUTPUT vRec) .
   FIND loan WHERE RECID(loan) = vRec NO-LOCK NO-ERROR.
   IF AVAIL loan
   THEN
   RUN GET_COMM_LOAN_BUF(loan.contract,loan.cont-code,iComm,iDate,BUFFER comm-rate).

   RETURN ( IF AVAIL  comm-rate
           THEN  comm-rate.rate-comm
           ELSE ?).

END FUNCTION.

FUNCTION getNextCommDateTransh RETURNS DATE
     (INPUT iContract  AS CHAR,
      INPUT iContCode  AS CHAR,
      INPUT iComm      AS CHAR,
      INPUT iBegDate   AS DATE,
      INPUT iEndDate   AS DATE):

   DEFINE VARIABLE vRetVal AS DATE       NO-UNDO.

   vRetVal = GET_NEXT_COMM_DATE_LOAN (iContract,
                                      iContCode,
                                      iComm,
                                      iBegDate,
                                      iEndDate).

   IF vRetVal EQ ? AND NUM-ENTRIES (iContCode," ") GT 1 THEN
      vRetVal = GET_NEXT_COMM_DATE_LOAN (iContract,
                                         ENTRY (1, iContCode, " "),
                                         iComm,
                                         iBegDate,
                                         iEndDate).
   RETURN vRetVal.
END FUNCTION.

FUNCTION getCommRateTransh RETURNS DECIMAL
   (INPUT iContract AS CHAR,
    INPUT iContCode AS CHAR,
    INPUT iComm     AS CHAR,
    INPUT iSince    AS DATE):

   DEFINE VARIABLE vRetVal AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vRecid  AS RECID      NO-UNDO.
   DEFINE VARIABLE vSurr   AS CHARACTER  NO-UNDO.
   /* ��।���� ������� �� ⥪�饬 ������� */
   vRetVal = get_Comm_Loan (iContract,
                            iContCode,
                            iComm,
                            iSince).
   /* ������� �� �墠�뢠�饬 ������� */
   IF vRetVal EQ ?
   THEN vRetVal = get_Main_Comm_Rate (iContract,
                                      iContCode,
                                      iComm,
                                      iSince).
   RETURN vRetVal.
END FUNCTION.

/* �㭪�� ��।���� ���室 �� ���� ��業⮢  � �������樥�
���㣫���� */
FUNCTION MovRound RETURN DATE:
 DEF VAR vDate AS DATE NO-UNDO.
 vDate = DATE(FGetSetting("MoveRound",?,"01/01/1970")) NO-ERROR.
 IF vDate = ?
 THEN RETURN 01/01/1970.
 ELSE RETURN vDate .
END FUNCTION .

PROCEDURE MovRoundLoan.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iClassCode AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oMoveRound AS DATE NO-UNDO.
   DEF OUTPUT PARAM oKompPogr  AS CHAR NO-UNDO.

   DEF VAR vMoveRound AS CHAR NO-UNDO.

   vMoveRound = GetXattrValueEx("loan",
                                iContract + "," + iContCode,
                                "MoveRound",
                                "").
   IF vMoveRound EQ "" THEN
      vMoveRound = GetXattrInit(iClassCode,
                                "MoveRound").
   IF vMoveRound EQ "" THEN
   DO:
      oMoveRound = MovRound().
   END.
   ELSE
   DO:
      oMoveRound = DATE(vMoveRound) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         oMoveRound = 01/01/1970.
   END.
   oKompPogr = GetXattrValueEx("loan",
                                iContract + "," + iContCode,
                                "��������",
                                "").
   IF oKompPogr EQ "" THEN
   DO:
      oKompPogr = GetXattrInit(iClassCode,
                               "��������").
      IF oKompPogr EQ "" THEN
         oKompPogr = FGetSetting("��������",?,"").
   END.
END PROCEDURE.

/* ��⮤ ᮧ���� �ਢ離� �஢���� � ⥪�饬� ��������, �᫨ �� �������� */
PROCEDURE LinkOpEntryMeth.
   DEF INPUT PARAM iOpEntry         AS RECID       NO-UNDO.
   DEF INPUT PARAM iContract        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iContCode        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCodeInt         AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iSide            AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCurrentLinkType AS CHARACTER   NO-UNDO.

   DEF BUFFER op        FOR op.
   DEF BUFFER loan      FOR loan.
   DEF BUFFER op-entry  FOR op-entry.

   DEF VAR vContractDate  AS DATE NO-UNDO.   /* �������� ��� ���㬥��. */

   FIND FIRST op-entry WHERE RECID(op-entry) EQ INT64(iOpEntry) NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN
      FIND FIRST loan WHERE
                           loan.contract  EQ iContract
                     AND   loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
      FIND LAST op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
   
   IF AVAIL op THEN
   IF     IsBindEarlier(op.contract-date, loan.since)
      AND IsDateBanSince(loan.contract, loan.cont-code , op.contract-date)
   THEN
   DO TRANSACTION
      ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* ����祭�� �������� ���� ����権. */
      RUN UpdContract (iContract,               /* �����祭�� �������. */
                       iContCode,               /* ����� �������. */
                       op.contract-date,        /* �������� ��� ���㬥��. */
                       iCodeInt,                /* ������ */
                       OUTPUT vContractDate).   /* ��ୠ� ��� ���㬥��. */

      /* ��ࠡ�⪠ �訡��. */
      IF {&RETURN_VALUE} NE ""
      THEN UNDO, LEAVE.

      /* ���⢥ত���� ����� ���㬥��. */
      RUN ConfirmAction (
         RECID (op),                   /* �����䨪��� �������. */
         INPUT-OUTPUT vContractDate).  /* �������� ��� ���㬥��. */

      IF {&RETURN_VALUE} NE ""
      THEN UNDO, LEAVE.

      /* ���४�஢�� �������� ���� ����樨. */
      RUN SetLIntDate (vContractDate).

      /* ������ ������� �� ���⢥ত����� �������� ����. */
      IF vContractDate NE loan.since
      THEN RUN LoanCalc (iContract,       /* �����祭�� �������. */
                         iContCode,       /* ����� �������. */
                         vContractDate).  /* �������� ��� ����㬥��. */

      IF vContractDate <> DATE(GetBufferValue("loan","WHERE loan.contract = '" + iContract + "'
                                                        AND loan.cont-code = '" + iContCode + "'",
                                              "since")) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", "��� �஢���� �� ࠢ�� " +
            "��� ������ ���ﭨ� �������. �� �ਢ離� �������� �訡��. " +
            "�����⠩� �������.").
         UNDO, RETRY.
      END.

      /* �맮� ��⮤� �஢�ન ����樨 �� ����奬�.
      ** ������ �����।�⢥��� ��। ᮧ������ ����樨,
      ** ��-�� �஢�ન �뫨 ��᫥ ���⢥ত���� �㬬� � �������� ���� */
      RUN RunChkMethod (iContract,        /* �����祭�� �������. */
                        iContCode,        /* ����� �������. */
                        RECID(op-entry),  /* �����䨪��� �஢����. */
                        iCodeInt).        /* ��� ����樨 �����⥬���� ���. */

      /* ��ࠡ�⪠ �訡��. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO, RETRY.
      END.

      /* ��ନ஢���� ����� ����� ��� ᮧ�����.
      ** ��।������ �㬬� � ������ ����権. */
      RUN GetOper (RECID (op-entry),   /* �����䨪��� �஢����. */
                  iContract,           /* �����祭�� �������. */
                  iContCode,           /* ����� �������. */
                  vContractDate,       /* �������� ��� ���-�. */
                  iCodeInt,            /* ��� ���� ����樨. */
                  YES).                /* �饬 ����� ������ */

      /* ��ࠡ�⪠ �訡��. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO, RETRY.
      END.

      /*�᫨ ����樨 �����࠭�, �� �� �㫥��, � ᮮ�饭�� ���짮��⥫� � �����⨪� �� ᮧ����*/
      FOR EACH lInt BY lInt.Amt DESC:
         LEAVE.
      END.
      IF (AVAIL lInt) AND (lInt.Amt LE 0) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0",
             "�ਢ離� �� ����� ���� �믮�����,�.� ��ࠬ��� �㫥���. �஢���� �।��騥 ����樨.").
         UNDO, LEAVE.
      END.

      /* �������� ����樨. */
      RUN CreLInt (iContract,       /* �����祭�� �������. */
                   iContCode).      /* ����� �������. */

      /* ��ࠡ�⪠ �訡��. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO,  RETRY.
      END.

      /* �������� �㡠����⨪� �� �஢����. */
      RUN CreEntryKau(RECID(op-entry), /* �����䨪��� �஢����. */
                     iContract,        /* �����祭�� �������. */
                     iContCode,        /* ����� �������. */
                     iCodeInt,         /* ��� ���� ����樨. */
                     LOGICAL(iSide),   /* ��஭� ��� Yes - �।��. */
                     vContractDate).   /* ��ୠ� �������� ��� ���㬥��. */

      /* ��ࠡ�⪠ �訡��. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         IF {&RETURN_VALUE} BEGINS "lock"
         THEN RUN wholock(IF {&RETURN_VALUE} EQ "lock_op"
                          THEN RECID (op)
                          ELSE RECID(op-entry), "").

         ELSE
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
            UNDO, RETRY.
         END.
      END.
   END.

END PROCEDURE.

/* ��⮤ 㤠��� �ਢ離� �஢���� � ⥪�饬� �������� */
PROCEDURE UnLinkOpEntryMeth.
   DEF INPUT PARAM iOpEntry         AS RECID       NO-UNDO.
   DEF INPUT PARAM iContract        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iContCode        AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCodeInt         AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iSide            AS CHARACTER   NO-UNDO.
   DEF INPUT PARAM iCurrentLinkType AS CHARACTER   NO-UNDO.

   DEF BUFFER op        FOR op.
   DEF BUFFER loan      FOR loan.
   DEF BUFFER op-entry  FOR op-entry.

   FIND FIRST op-entry WHERE RECID(op-entry) EQ INT64(iOpEntry) NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN
      FIND FIRST loan WHERE
                           loan.contract  EQ iContract
                     AND   loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      
   IF AVAIL loan THEN
      FIND LAST op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
      
   /* �஢���� �ਢ易��, ������ ��易��. */
   IF AVAIL op THEN    
   IF     IsBindEarlier(op.contract-date, loan.since)
        AND IsDateBanSince(loan.contract, loan.cont-code , op.contract-date)
   THEN
   DO TRANSACTION
      ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /* ������ ������� �� �������� ���� ���㬥��. */
      IF op.contract-date LT loan.since
      THEN RUN LoanCalc (iContract,          /* �����祭�� �������. */
                         iContCode,          /* ����� �������. */
                         op.contract-date).  /* �������� ��� ����㬥��. */

      /* �������� ����樨. */
      RUN DelLInt (op-entry.op,
                  op-entry.op-entry,
                  iContract,
                  iContCode,
                  iCodeInt).

      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
         UNDO, RETRY.
      END.

      /* �������� �㡠����⨪� �� �஢����. */
      RUN DelEntryKau(RECID(op-entry), /* �����䨪��� �஢����. */
                     iContract,        /* �����祭�� �������. */
                     iContCode,        /* ����� �������. */
                     iCodeint,         /* ��� ���� ����樨. */
                     LOGICAL(iSide)).  /* ��஭� ��� Yes - �।��. */

      /* ��ࠡ�⪠ �訡��. */
      IF {&RETURN_VALUE} NE ""
      THEN
      DO:
         IF {&RETURN_VALUE} BEGINS "lock"
         THEN RUN wholock(IF {&RETURN_VALUE} EQ "lock_op"
                          THEN RECID (op)
                          ELSE RECID(op-entry), "").

         ELSE
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0", {&RETURN_VALUE}).
            UNDO, LEAVE.
         END.
      END.
   END.

END PROCEDURE.


   /* ��⮤ ��⮬���᪮� �����樨 ����� �।�⭮�� ������� */
PROCEDURE GetNumLoan.
   DEF INPUT  PARAM iClassCode    AS CHAR NO-UNDO. /* ����� �������    */
   DEF INPUT  PARAM iDate         AS DATE NO-UNDO. /* ������� ���      */
   DEF INPUT  PARAM iBranch-id    AS CHAR NO-UNDO. /* ��� 䨫���� � �� ����室��� ⥣�  */
   DEF INPUT  PARAM iGenNum       AS LOG  NO-UNDO. /* ��易⥫쭮 �����஢��� ����� ? */
   DEF OUTPUT PARAM oNum          AS CHAR NO-UNDO. /* ����� �������    */
   DEF OUTPUT PARAM oCounterValue AS INT64  NO-UNDO. /* ���祭�� ���稪� */

   DEF VAR vAutoGenNum AS CHAR NO-UNDO. /* ����-� ����.४�. "AutoGenNum" */
   DEF VAR vTemplate   AS CHAR NO-UNDO. /* ��᪠ ����� */
   DEF VAR vCounter    AS CHAR NO-UNDO. /* ��� ���稪� */
   DEF VAR oNumCD      AS CHAR NO-UNDO . /* �ᯮ��㥬�� ���� ����� �।�⭮�� ������� */
   DEF VAR vNumRegion  AS CHAR NO-UNDO . /* ��� ॣ���� �� ����� - 2 ����� ᨬ���� */
   DEF VAR vNumOffis   AS CHAR NO-UNDO . /* ����� ��� ����� - 2 ��᫥���� ᨬ���� */
   DEF VAR vTmpStr     AS CHAR NO-UNDO.
   DEF VAR vTmpStr2    AS CHAR NO-UNDO.
   DEF VAR vTmpStr3    AS CHAR NO-UNDO.
   DEF VAR vTmpStrT    AS CHAR NO-UNDO.
   DEF VAR vNumDogOb   AS CHAR NO-UNDO.
   DEF VAR vSurr       AS CHAR NO-UNDO.
   DEF VAR vList       AS CHAR NO-UNDO.
   DEF VAR vTmpInt     AS INT64 NO-UNDO.
   DEF VAR vTmpInt2    AS INT64 NO-UNDO.
   DEF VAR vI          AS INT64 NO-UNDO.

   /*��६���� ���������� �� iBranch-id, �� ���� ᠬ��� Branch-id                 */
   DEF VAR vCountract  AS CHAR NO-UNDO INIT "" . /* 2 -  �।��� �������          */
   DEF VAR vCounCode   AS CHAR NO-UNDO INIT "" . /* 3 -  �।��� �������          */
   DEF VAR vObType     AS CHAR NO-UNDO INIT "" . /* 4 -  ��� ������� ���ᯥ祭��   */
   DEF VAR vLoanType   AS CHAR NO-UNDO INIT "" . /* 5 -  ��� ������� loan          */
   /*-------------------------------------------------------------------------------*/
   DEF VAR proc-name   AS CHAR NO-UNDO .   
   DEF VAR params      AS CHAR NO-UNDO .   
   DEF BUFFER bloan FOR loan.

      /* ����砥� ���祭�� ����᪮�� ४����� "AutoGenNum" */
   vAutoGenNum = GetXAttrInit (iClassCode, "AutoGenNum").
   IF    vAutoGenNum EQ "��"
      OR iGenNum THEN
   DO:

      /* �����⮢�� ��६�����  � �ᯮ�짮����� */

      IF NUM-ENTRIES(iBranch-id,"|") = 5
      THEN
         ASSIGN
         vCountract = ENTRY(2,iBranch-id,"|" )
         vCounCode  = ENTRY(3,iBranch-id,"|" )
         vObType    = ENTRY(4,iBranch-id,"|" )
         vLoanType  = ENTRY(5,iBranch-id,"|" )
         .
      iBranch-id = ENTRY(1,iBranch-id,"|" ) .

      vObType = GetCodeEx ('���䄮���',vObType,"") .
      vLoanType = GetCodeEx ('���䄮�',vLoanType,"") .
      RUN GetClassMethod IN h_xclass (iClassCode, 
                                      "GetLoanNumSb", 
                                      "", 
                                      "",
                                      OUTPUT proc-name,
                                      OUTPUT params).                                                                           
      IF {assigned proc-name} THEN 
         RUN RunClassMethod IN h_xclass (iClassCode, 
                                         "GetLoanNumSb", 
                                         "", 
                                         "", 
                                         ?,
                                         vCountract + "," + vCounCode + CHR(1)).
      IF {&RETURN_VALUE} NE "" THEN                                                                     
         oNumCD = {&RETURN_VALUE}.                                                                       
      vNumRegion = SUBSTRING( GetXAttrValue("branch", iBranch-id, "���������") ,1 ,2 ).
      vNumOffis  = SUBSTRING( iBranch-id , length(iBranch-id) - 1 , 2) .

         /* ��।��塞 ���� ����� � ��� ���稪� */
      RUN GetClassTemplatePars (iClassCode,
                                OUTPUT vTemplate,
                                OUTPUT vCounter).
                                                                 
      vList = CHR(92) + "," + CHR(47) + "," + CHR(45) + "," + CHR(95).
      CYCLE:                                                          
      DO vI = 1 TO NUM-ENTRIES(vList):
         oNumCD = Replace(oNumCD,ENTRY(vI,vList),"").
      END.
      ASSIGN
         iBranch-id    = IF iBranch-id EQ ? THEN "" ELSE iBranch-id         /* �஢�ਬ �� "���⮥" ���祭�� ���� 䨫���� */
         oNum          = ReplaceBasicTags (vTemplate, vCounter, iDate)      /* ����⠢�塞 � 蠡��� �� ����� � ���� */
         oCounterValue = GetCounterCurrentValue (vCounter, iDate)      /* ����砥� ⥪�饥 ���祭�� ���稪�,
                                                                       ** �� ������ ���⠬� ��ப� �.�. ReplaceBasicTags
                                                                       ** � �।��饩 ��ப� ������� �� �� ���稪 �� 1 .*/
         oNum          = ReplaceTag (oNum, "�", STRING(oCounterValue), NO)    /* ����⠢�塞 � 蠡��� ����� �������  ���᪠� �㪢�*/
         oNum          = ReplaceTag (oNum, "�", iBranch-id, YES)              /* ����⠢�塞 � 蠡��� ��� ���ࠧ������� ���᪠� �㪢�*/
         oNum          = ReplaceTag (oNum, "l", oNumCD, NO)                   /* ����⠢�塞 � 蠡��� ��� �� �।�⭮�� �������         ��⨭��*/
         oNum          = ReplaceTag (oNum, "�", vNumRegion,NO)                /* ����⠢�塞 � 蠡��� ��� ॣ����  ���᪠� �㪢� */
         oNum          = ReplaceTag (oNum, "�", vNumOffis,YES)                 /* ����⠢�塞 � 蠡��� ��� ���    ���᪠� �㪢�*/
      .
      
      IF vObType NE "" THEN DO:    
         oNum = ReplaceTag (oNum, "v", vObType, NO).                   /* ����⠢�塞 � 蠡��� ��� �� ⨯� ������� ���ᯥ祭�� ��⨭��*/
         IF INDEX(oNum,"�") NE 0 THEN 
         DO:                              /* �� "�" ���浪��� ����� */
            vTmpInt = oCounterValue.
            vTmpStrT = SUBSTRING (oNum, INDEX(oNum, "[�") + 1, INDEX(oNum, "�]") - INDEX(oNum, "[�")).
            IF LENGTH(STRING(vTmpInt + 1)) GE LENGTH(vTmpStrT) THEN
            oNum = ReplaceTag (oNum, "�", STRING(vTmpInt + 1), NO). 
            ELSE 
            DO:
               vTmpStr = SUBSTRING (oNum, 1, INDEX(oNum, "[�") - 1).
               vTmpStr2 = SUBSTRING (oNum, INDEX(oNum, "�]") + 2).
               oNum = vTmpStr + FILL("0", (LENGTH(vTmpStrT) - LENGTH(STRING(vTmpInt + 1)))) + STRING(vTmpInt + 1) + vTmpStr2.
            END.
         END.
      END.
      ELSE
         SUBSTRING (oNum, INDEX(oNum, "-[v"), INDEX(oNum, "v]") - INDEX(oNum, "-[v") + 2) = "" NO-ERROR.         
      IF vLoanType NE "" THEN     
         oNum = ReplaceTag (oNum, "t", vLoanType, NO).                   /* ����⠢�塞 � 蠡��� ��� �� ⨯� ������� ��⨭��*/
      ELSE
         SUBSTRING (oNum, INDEX(oNum, "-[t"), INDEX(oNum, "t]") - INDEX(oNum, "-[t") + 2) = "" NO-ERROR.       
   END.
END PROCEDURE.

/* ��।������ �⠢�� ���� � ����� ������ (१�����/��१�����) �� ���� */
PROCEDURE GetStaffByAcct.
   DEF INPUT  PARAM iAcct AS CHAR   NO-UNDO. /* ��� */
   DEF INPUT  PARAM iCurr AS CHAR   NO-UNDO. /* ����� */
   DEF INPUT  PARAM iDate AS DATE   NO-UNDO. /* ��� ��� ��।������ �⠢�� */
   DEF OUTPUT PARAM oRate AS DEC    NO-UNDO. /* �⠢�� ���� */
   DEF OUTPUT PARAM oStat AS CHAR   NO-UNDO. /* ����� ������ */

   DEF VAR vTax AS CHAR   NO-UNDO.
   DEF BUFFER acct FOR acct.

   MAIN_BLOCK:
   DO:
      oStat = ?.
      /* ���� ��� */
      FIND FIRST acct WHERE acct.acct EQ iAcct
                        AND acct.curr EQ iCurr
      NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         /* ��।������ ����� �� ������ ��� 1-�� ���浪� */
         oStat = GetCode("����Ꮾ������",SUBSTRING(STRING(acct.bal-acct),1,3) + "*").
      /* �᫨ �� ��諨 ᮮ⢥�ᢨ� � �����䨪���, ��⠥� �� �� १����� */
      IF oStat EQ ? THEN oStat = "१�����".
      /* ��।������ ���� �⠢�� �� ������ */
      vTax = GetCode("������������",oStat).
      /* ��।������ ���祭�� �⠢�� �� ���� */
      /*oRate = GET_COMM(vTax,
                       RECID(acct),
                       iCurr,
                       "",0,0,
                       iDate).*/
      FIND LAST comm-rate WHERE comm-rate.since       LE iDate
                            AND comm-rate.filial-id = shfilial
                            AND comm-rate.branch-id = ""
                            AND comm-rate.acct        EQ '0'
                            AND comm-rate.commission  EQ vTax
                            AND comm-rate.currency    EQ ''
                            AND comm-rate.min-value   EQ 0
                            AND comm-rate.period      EQ 0
      NO-LOCK NO-ERROR.
      IF AVAIL comm-rate THEN oRate = comm-rate.rate-comm.
   END.

   RETURN.
END PROCEDURE.

/* ��楤�� ���� ��ࠬ��� �� ���� ������ ������� ��⮤��,
** �ਬ��塞� ��� ��㧥� brwl-par.p  */
PROCEDURE ALL_PARAM.
   DEF INPUT  PARAM iContract AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iAmtID    AS INT64    NO-UNDO. /* ��� ��ࠬ��� */
   DEF OUTPUT PARAM oAmt      AS DEC    NO-UNDO. /* �㬬� ��ࠬ��� */
   DEF OUTPUT PARAM oCurr     AS CHAR   NO-UNDO. /* ����� ��ࠬ��� */
   DEF OUTPUT PARAM oAmtRub   AS DEC    NO-UNDO. /* �㬬� ��ࠬ��� � �㡫�� */

   DEF VAR vDbSumDec  AS DEC  NO-UNDO.
   DEF VAR vCrSumDec  AS DEC  NO-UNDO.
   DEF VAR vCodOstpar AS INT64  NO-UNDO.
   DEF VAR vParamStr  AS CHAR NO-UNDO.
   DEF VAR vCodeStr   AS CHAR NO-UNDO.
   DEF VAR vCode      AS INT64  NO-UNDO.
   DEF VAR vAmtDiff   AS DEC  NO-UNDO. /* �㬬� ���४�஢�� ��ࠬ��� */

   DEF BUFFER bloan     FOR loan.
   DEF BUFFER bterm-obl FOR term-obl.

   MAIN:
   DO:
      FIND FIRST bloan WHERE
                 bloan.contract  EQ iContract
             AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.

      IF AVAILABLE(bloan) THEN
      DO:
         ASSIGN
            vCodOstpar = GetParCode(bloan.class-code, "����᭄���").
            vParamStr  = FGetSetting("��������","�����ࠬ",?).
            vCodeStr   = FGetSetting("��������","������",?)
            .

          /* ����� ��ࠬ��� ��� ��� ��業⮢ */
         RUN STNDRT_PARAM (iContract,
                           iContCode,
                           iAmtID,
                           ?,                    /* ����� �� ���� loan.since */
                           OUTPUT oAmt,
                           OUTPUT vDbSumDec,
                           OUTPUT vCrSumDec).

         RUN inter_current  (BUFFER bloan, (iAmtID - vCodOstPar), OUTPUT vAmtDiff) .
         oAmt = oAmt + vAmtDiff.
         IF LOOKUP(STRING(iAmtID), vParamStr) NE 0 THEN
         DO:
            vCode = INT64(ENTRY(LOOKUP(STRING(iAmtID), vParamStr), vCodeStr)) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR
               AND vCode NE 0
               AND vCode NE ? THEN
            DO:
               FIND FIRST bterm-obl WHERE
                          bterm-obl.contract  EQ iContract
                      AND bterm-obl.cont-code EQ iContCode
                      AND bterm-obl.idnt      EQ 6
                      AND bterm-obl.end-date  EQ bloan.since
                      AND bterm-obl.nn        EQ vCode
               NO-LOCK NO-ERROR.
               IF AVAIL bterm-obl THEN
                  oAmt = oAmt + term-obl.amt-rub.
            END.
         END.

         IF CAN-DO("9,15,12,18,82",STRING(iAmtID)) THEN
         DO:
            vAmtDiff = 0.
            RUN CalcPrmValue (iContract,
                              iContCode,
                              iAmtID,
                              oAmt,
                              oCurr, /* � ��㧥� brwl-par.p ����� �ᥣ�� �����誠 */
                              OUTPUT vAmtDiff).

            IF vAmtDiff NE ? THEN
            ASSIGN
               oAmt = vAmtDiff.
         END.

         RUN GetParP (RECID(bloan), iAmtID, oAmt, OUTPUT oAmtRub, OUTPUT oCurr).

      END.
   END.

   RETURN.
END PROCEDURE.

/* �����㬥�� ��� ��।������ ᬥ饭�� ���� ��砫� � ���� ����砭�� ����
** ��業⮢ �� �������� � ��� ���室� �� 39� */
PROCEDURE GetOffset.
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDatePer     AS DATE NO-UNDO.
   DEF OUTPUT PARAM oBeg-Offset  AS INT64  NO-UNDO.

   DEF VAR vStr AS CHAR NO-UNDO.

   DEF BUFFER loan FOR loan.

   MAIN_BLOCK:
   DO:
      FIND FIRST loan WHERE
                 loan.contract  EQ iContract
             AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF AVAIL loan THEN
      DO:
         vStr = GetXAttrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "beg-offset",
                                ?).
         IF vStr EQ ? THEN
            vStr = GetXattrInit(loan.class-code,
                                "beg-offset").
         oBeg-offset = INT64(vStr) NO-ERROR.
         IF    ERROR-STATUS:ERROR
            OR oBeg-offset EQ ? THEN
               oBeg-offset = 0.
         IF loan.open-date < iDatePer THEN
         /* ���᫥��� �ந�室�� �� ��室�騩 ���⮪, � �㦭� ���뢠�� ���� �뤠� */
            IF oBeg-Offset GE 1 THEN
               oBeg-Offset = oBeg-Offset - 1.
      END.
   END.
   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �஫������� �������� �������
  Parameters:  iContract   - �����祭�� �������
               iContCode   - �����䨪��� ������
               iDate       - ��� ����樨
               iShiftType  - ��� ᤢ��� 1 - ���, 2 - ���।, 3 - �����
               oResult     - ������� ��ࠡ�⪨
               oOk         - ���� �����襭��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE DepDogProl:
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.
   DEF INPUT  PARAM iShiftType AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oResult    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOk        AS LOG  NO-UNDO.

   DEF VAR mProlCount         AS INT64  NO-UNDO INIT 1.  /* ���-�� �஫����権 ������� � ��⮬ ⥪�饩 */
   DEF VAR mFirstProlDate     AS DATE NO-UNDO.         /* ����筠� ��� ��ࢮ��砫쭮�� �ப� ������� */
   DEF VAR mSurr              AS CHAR NO-UNDO.         /* ���ண�� */
   DEF VAR mSurC              AS CHAR NO-UNDO.         /* ���ண�� �����㥬��� �᫮��� */
   DEF VAR mDays              AS INT64  NO-UNDO.         /* ���� �ப ������� � ���� */
   DEF VAR mAmount            AS DEC  NO-UNDO.         /* C㬬� �� (�㬬� ��ࠬ��஢ 0+2). */
   DEF VAR mAmtParam          AS DEC  NO-UNDO.         /* �㬬� �� ��ࠬ��� */
   DEF VAR mDbSumDec          AS DEC  NO-UNDO.
   DEF VAR mCrSumDec          AS DEC  NO-UNDO.
   DEF VAR mMove              AS INT64  NO-UNDO.         /* ���ࠢ���� ᤢ��� */
   DEF VAR mOldEndDate        AS DATE NO-UNDO.         /* ��ࢮ��砫쭠� ����筠� ��� ������� */
   DEF VAR mNDays             AS INT64  NO-UNDO.
   DEF VAR mNMonthes          AS INT64  NO-UNDO.
   DEF VAR mNYears            AS INT64  NO-UNDO.
   DEF VAR mProcs             AS CHAR NO-UNDO.         /* ���᮪ %% �⠢�� ��� ����஢���� */
   DEF VAR mCntr              AS INT64  NO-UNDO.         /* ���稪 */
   DEF VAR mDopRecList        AS CHAR NO-UNDO.         /* ���᮪ �� ��� ����஢���� */
   DEF VAR mRateD             AS DEC  NO-UNDO.
   DEF VAR mRates             AS CHAR NO-UNDO.
   DEF VAR mTarif             AS CHAR NO-UNDO.         /* ���� �� ��஬� �᫮��� */
   DEF VAR mTarifNew          AS CHAR NO-UNDO.         /* ���� �� ⥪���� ���� */
   DEF VAR mTarifErr          AS CHAR NO-UNDO.
   DEF VAR vCondCount         AS INT64  NO-UNDO.

   DEF BUFFER bloan      FOR loan.
   DEF BUFFER term-obl   FOR term-obl.
   DEF BUFFER bterm-obl  FOR term-obl.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER xloan-cond FOR loan-cond.
   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER bcomm-rate FOR comm-rate.
   DEF BUFFER tloan-cond FOR loan-cond. /* ���������� ����. */

 MAIN:
   DO:
      FIND FIRST bloan WHERE
                 bloan.contract  EQ iContract
             AND bloan.cont-code EQ iContCode
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      IF NOT AVAILABLE bloan THEN
      DO: 
         IF LOCKED bloan THEN 
              RUN Fill-SysMes IN h_tmess("","","-1","������� �������஢�� ��㣨� ���짮��⥫��").
         ELSE RUN Fill-SysMes IN h_tmess("","","-1","������� �� ������").
         RETURN.
      END.

      IF AVAIL bloan THEN
      DO:
            /* ������� ᯨ᮪ ��������� �⠢�� */
         FOR EACH comm-rate WHERE
                  comm-rate.kau        EQ bloan.contract + "," + bloan.cont-code
             AND  comm-rate.commission NE "%���"
             AND  comm-rate.commission NE "%����"
         USE-INDEX kau NO-LOCK:
            IF LOOKUP (comm-rate.commission, mProcs) EQ 0 THEN
               mProcs = mProcs + ( IF mProcs NE "" THEN "," ELSE "") + comm-rate.commission.
         END.
            /* �����⠥� �᫮ �஫����権 */
         FOR EACH bloan-cond WHERE
                  bloan-cond.contract  EQ bloan.contract
            AND   bloan-cond.cont-code EQ bloan.cont-code
         NO-LOCK:
            mSurr = bloan-cond.contract + "," + bloan-cond.cont-code + "," + STRING(bloan-cond.since).
            IF GetXAttrValueEx("loan-cond", mSurr, "PayType", "") EQ "�஫�����" THEN
               ASSIGN
                  mProlCount     = mProlCount + 1
                  mFirstProlDate = IF mFirstProlDate EQ ?
                                      THEN bloan-cond.since
                                      ELSE mFirstProlDate
               .
         END.
            /* �᫨ �஫����権 �� �� �뫮, � ��६ ���� ����砭�� ������� */
         IF mFirstProlDate EQ ? THEN
            mFirstProlDate = bloan.end-date.

         ASSIGN
               /* ���� �ப ������� = (n + 1) * ��ࢮ��砫�� �ப �������, ��� n � ����� �஫����樨 � ��⮬ �஢������. */
            mDays             = (mProlCount + 1) * (mFirstProlDate - bloan.open-date)
            mOldEndDate       = bloan.end-date
               /* ����� ��� ����砭�� ������� = ��� ������ ������� + ���� �ப �������. */
            bloan.end-date    = bloan.open-date + mDays
               /* �������� ��⠭�������� ����� ֏���� */
            bloan.loan-status = "����"
               /* ��८�।��塞 ⨯ ᤢ��� ��� �ᯮ�짮����� �㭪樥� GetFstWrkDay */
            mMove             = IF iShiftType = "3" THEN -1 ELSE IF iShiftType = "2" THEN 1 ELSE 0
         .
            /* ������� ����, �᫨ ����室��� */
         IF mMove NE 0 THEN
            bloan.end-date = GetFstWrkDay (bloan.Class-Code,
                                           bloan.user-id,
                                           bloan.end-date,
                                           9,
                                           mMove).
            /* ������� ��ࠬ��஢ ������� �� ���� �஫����樨. */
         IF bloan.since NE iDate THEN
            RUN l-calc2.p (iContract,
                           iContCode,
                           iDate,
                           YES,
                           YES).
            /* ������ �㬬� ��ࠬ��஢ 0 + 2 */
         RUN STNDRT_PARAM (iContract,
                           iContCode,
                           0,
                           iDate,
                           OUTPUT mAmtParam,
                           OUTPUT mDbSumDec,
                           OUTPUT mCrSumDec).
         mAmount = mAmtParam.
         RUN STNDRT_PARAM (iContract,
                           iContCode,
                           2,
                           iDate,
                           OUTPUT mAmtParam,
                           OUTPUT mDbSumDec,
                           OUTPUT mCrSumDec).
         mAmount = mAmount + mAmtParam.
            /* ������ �᫮��� ��� ����஢���� */
         FIND LAST bloan-cond WHERE
                   bloan-cond.contract  EQ bloan.contract
            AND    bloan-cond.cont-code EQ bloan.cont-code
            AND    bloan-cond.since     LE bloan.end-date /* ����� ��� ������� */
         NO-LOCK NO-ERROR.
         IF AVAIL bloan-cond THEN
         DO:
               /* ������ ������ �� ���� ���� ����� */
            RUN l-calc2.p (iContract,
                           iContCode,
                           iDate - 1,
                           YES,
                           YES).
               /* ������� ����� �᫮��� �� ������� � ⨯�� "�஫������"
               ** ���饭�� ���� ����砭��, ����� ��室���� �� ��室��� ����,
               ** �����⢫塞 � ᮮ⢥��⢨� � ����ன���� �࠭���樨 (��ࠬ��� iShiftType) */
            CREATE xloan-cond.
            BUFFER-COPY bloan-cond EXCEPT since TO xloan-cond
               ASSIGN
                  xloan-cond.since = iDate + 1 /* �������� ��� + 1 */
            .

            ASSIGN
                  /* ���ண�� �����㥬��� �᫮��� */
               mSurC       = bloan-cond.contract + "," + bloan-cond.cont-code + "," + STRING(bloan-cond.since)
                  /* ���ண�� ������ �᫮��� */
               mSurr       = xloan-cond.contract + "," + xloan-cond.cont-code + "," + STRING(xloan-cond.since)
                  /* ���᮪ �����㥬�� �� */
               mDopRecList = "cred-work-calend,int-work-calend,cred-mode,int-mode,�奬�����,�����⏥�,DateDelay," +
                             "DateDelayInt,�����⏥����,PayType,cred-curr-next,NDays,NYears,NMonthes,�த���"
            .
               /* �����㥬 �� � ����������� �᫮��� */
            DO mCntr = 1 TO NUM-ENTRIES(mDopRecList):
               UpdateSignsEx (xloan-cond.class-code,
                              mSurr,
                              ENTRY(mCntr, mDopRecList),
                              GetXAttrValue("loan-cond", mSurC, ENTRY(mCntr, mDopRecList))).
            END.
               /* �㬬� ���⪠ �� �������� */
            UpdateSignsEx (xloan-cond.class-code,
                           mSurr,
                           "PaySum",
                           TRIM(STRING(mAmount, ">>>,>>>,>>>,>>>,>>9.99"))).
               /* ��� ���⥦� - �஫������ */
            UpdateSignsEx (xloan-cond.class-code,
                           mSurr,
                           "PayType",
                           "�஫�����").
               /* ��� ����砭�� ������� */
            UpdateSignsEx (xloan-cond.class-code,
                           mSurr,
                           "CondEndDate",
                           STRING(bloan.end-date)).
               /* �����㥬 �⠢�� �� ����� �᫮��� */
            DO mCntr = 1 TO NUM-ENTRIES(mProcs):
               FIND LAST comm-rate WHERE
                         comm-rate.kau        EQ iContract + "," + iContCode
                  AND    comm-rate.since      LE xloan-cond.since /* ��� �஫����樨 */
                  AND    comm-rate.commission EQ ENTRY(mCntr, mProcs)
               USE-INDEX kau NO-LOCK NO-ERROR.
               IF AVAIL comm-rate THEN
               DO:
                  CREATE bcomm-rate.
                  BUFFER-COPY comm-rate EXCEPT since comm-rate-id TO bcomm-rate
                     ASSIGN
                        bcomm-rate.since = xloan-cond.since
                  .
                     /* ���࠭�� ���祭�� �⠢�� "%���" ��� ���� */
                  IF comm-rate.commission EQ "%���" THEN
                     mRates = TRIM(STRING(comm-rate.rate-comm, ">>>9.99999")).
               END.
            END.
               /* �᫨ �� ��� ���� 㦥 ������� ������ � ��䨪�
               ** �������� ���⪮� � �����塞 �㬬� */
            FIND LAST term-obl WHERE
                      term-obl.contract  EQ iContract
               AND    term-obl.cont-code EQ iContCode
               AND    term-obl.idnt      EQ 2
               AND    term-obl.end-date  EQ iDate
               AND    term-obl.amt-rub   EQ 0
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

            IF NOT AVAILABLE term-obl THEN
            DO:
               IF LOCKED term-obl THEN
                    RUN Fill-SysMes IN h_tmess("","","-1","������ � ������� ���⪮� �������஢��� ��㣨� ���짮��⥫��").
               ELSE RUN Fill-SysMes IN h_tmess("","","-1","������ � ������� ���⪮� �� �������").
               RETURN.
            END.

            IF AVAIL term-obl THEN
               ASSIGN
                  term-obl.amt-rub  = mAmount
                  term-obl.end-date = iDate + 1
               .
               /* ��� ᮧ���� ��易⥫��⢮ �� ��� ���� */
            ELSE
            DO:
               FIND LAST term-obl WHERE
                         term-obl.contract  EQ iContract
                  AND    term-obl.cont-code EQ iContCode
                  AND    term-obl.idnt      EQ 2
               NO-LOCK NO-ERROR.
               IF AVAIL term-obl THEN
               DO:
                  CREATE bterm-obl.
                  ASSIGN
                     bterm-obl.contract     = term-obl.contract
                     bterm-obl.cont-code    = term-obl.cont-code
                     bterm-obl.end-date     = iDate
                     bterm-obl.idnt         = 2
                     bterm-obl.nn           = term-obl.nn + 1
                     bterm-obl.amt          = mAmount
                     bterm-obl.dsc-beg-date = term-obl.dsc-beg-date
                  .
                  RELEASE bterm-obl NO-ERROR.
               END.
            END.
               /* ������� ��᫥���� �㫥��� ��易⥫��⢮ �� ����� ���� ����砭�� ������� */
            FIND LAST term-obl WHERE
                      term-obl.contract  EQ iContract
               AND    term-obl.cont-code EQ iContCode
               AND    term-obl.idnt      EQ 2
            NO-LOCK NO-ERROR.
            CREATE bterm-obl.
            ASSIGN
               bterm-obl.contract     = iContract
               bterm-obl.cont-code    = iContCode
               bterm-obl.end-date     = bloan.end-date
               bterm-obl.idnt         = 2
               bterm-obl.nn           = ( IF AVAIL term-obl THEN term-obl.nn ELSE 0) + 1
               bterm-obl.amt          = 0
            .
            RELEASE bterm-obl NO-ERROR.
               /* ��� ��୮�� �ନ஢���� ��䨪� �� 㤠��� ���� ��䨪 */
            FOR EACH term-obl WHERE
                     term-obl.contract  EQ iContract
               AND   term-obl.cont-code EQ iContCode
               AND   term-obl.idnt      EQ 3
            EXCLUSIVE-LOCK:
               DELETE term-obl.
            END.
            RELEASE term-obl NO-ERROR.
               /* �஢��塞 ᮮ⢥��⢨� ��䮢 */
            ASSIGN
                  /* ���ண�� ������ �᫮��� */
               mSurr = xloan-cond.contract + "," + xloan-cond.cont-code + "," + STRING(xloan-cond.since)
               mTarif = GetXAttrValueEx("loan-cond", mSurr, "�த���", "")
            .
            RUN GetLoanTarif (iContract,
                              iContCode,
                              iDate + 1, /* ��।��塞 �� ���� ������ �᫮���, ��� ��� ����⠫���樨 */
                              FALSE,
                              OUTPUT mTarifNew).
            IF mTarifNew EQ "" THEN
               mTarifErr = "��������������. �� ������ ���室�騩 ���, ������� �஫����஢�� � �⠢���� �� �।��饬� �᫮���!!!".
            ELSE IF mTarifNew NE mTarif THEN
            DO:
                  /* �᫨ ����� �᫮��� �������� ��� ��㣮� ���, � ������� ��� */
               UpdateSignsEx (xloan-cond.class-code, mSurr, "�த���", mTarifNew).
               mTarif = mTarifNew.
            END.
            ELSE
               mTarifNew = "".   /* ��� ���� */
               /* �����㥬 �⠢�� �� ����� �᫮��� */
            DO mCntr = 1 TO NUM-ENTRIES(mProcs):
                  /* ����稬 �⠢�� �� ��� �� ���� �����ᨨ */
               RUN GetTarifRate (mTarif,                /* ��� ��� */
                                 ENTRY(mCntr, mProcs),  /* ��� �����ᨨ */
                                 iDate,                 /* �� ���� */
                                 OUTPUT mRateD).        /* �⠢�� */
                  /* �᫨ �⠢�� �������, � ������� �� �� �᫮��� */
               IF mRateD GT 0 THEN
               DO:
                  FIND LAST comm-rate WHERE
                            comm-rate.kau        EQ iContract + "," + iContCode
                     AND    comm-rate.since      LE xloan-cond.since /* ��� �஫����樨 */
                     AND    comm-rate.commission EQ ENTRY(mCntr, mProcs)
                  USE-INDEX kau EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

                  IF NOT AVAILABLE comm-rate THEN
                  DO:
                     IF LOCKED comm-rate THEN 
                          RUN Fill-SysMes IN h_tmess("","","-1","������� �������஢��� ��㣨� ���짮��⥫��").
                     ELSE RUN Fill-SysMes IN h_tmess("","","-1","������� �� �������").
                     RETURN.
                  END.

                  IF AVAIL comm-rate THEN
                  DO:
                     comm-rate.rate-comm = mRateD.
                        /* ���࠭�� ����� ���祭�� �⠢�� "%���" ��� ���� */
                     IF comm-rate.commission EQ "%���" THEN
                        mRates = TRIM(STRING(comm-rate.rate-comm, ">>>9.99999")).
                  END.
               END.
            END.
               /* ������뢠�� ��䨪 ���⥦�� �� ��業⠬. ����� ���⥦��, ���� ������ ��室���� �� ��室�� ���,
               ** �஢������ � ᮮ⢥��⢨� � ����ன��� �࠭���樨. (��ࠬ��� iShiftType) */
            RUN SetSysConf IN h_base ("������������� �� �������� �����", iShiftType).
            RUN SetSysConf IN h_base ("������� �� ��������� �����",  iShiftType).
            RUN SetSysConf IN h_base ("����. �� �������� ����� ����.�����", "1").
            RUN SetSysConf IN h_base ("����. �� ����. ����� ����.�����", "1").
               /* ����� �뢮� �� �࠭ ��䨪�� ��� ���४�஢��  */
            RUN SetSysConf IN h_base ("�� �������� ������� �� �����", "��").

            FOR EACH xloan-cond WHERE xloan-cond.contract  EQ loan.contract
                               AND xloan-cond.cont-code EQ loan.cont-code
            NO-LOCK:
               vCondCount = vCondCount + 1.
            END.
            RUN mm-to.p (RECID(bloan),
                         RECID(xloan-cond),
                         mAmount,
                         {&MOD_ADD},
                         YES,
                         YES,
                         YES,
                         YES,
                         bloan.risk,
                         vCondCount) NO-ERROR.
            oResult = {&RETURN_VALUE}.
               /* ���⨬ ��... */
            RUN DeleteOldDataProtocol IN h_base ("������� �� ��������� �����").
            RUN DeleteOldDataProtocol IN h_base ("������������� �� �������� �����").
            RUN DeleteOldDataProtocol IN h_base ("����. �� �������� ����� ����.�����").
            RUN DeleteOldDataProtocol IN h_base ("����. �� ����. ����� ����.�����").
            RUN DeleteOldDataProtocol IN h_base ("�� �������� ������� �� �����").

            IF oResult NE {&RET-ERROR} THEN
               ASSIGN
                  oOk     = TRUE
                  oResult = ( IF mTarifErr NE "" THEN mTarifErr + " " ELSE "") +
                            ( IF mTarifNew NE "" THEN "���� ���: " + mTarifNew + " " ELSE "") +
                            "��ࢮ���.�ப = " + STRING(xloan-cond.since) +
                            " ��� �஫����樨 = " + STRING(xloan-cond.since) +
                            " ����� ��� ����砭�� = " + STRING(bloan.end-date) +
                            " �⠢�� = " + mRates +
                            " �஫����஢��� �㬬� = " +
                            TRIM(STRING(mAmount, ">>>,>>>,>>>,>>>,>>9.99"))
               .
            ELSE
               oResult = "�訡�� �஫����樨 �������".
         END.
      END.
   END.
END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose:     ��楤�� �����頥� ���祭�� �த�� ������� � 䫠� ���������� �஫����樨
  Parameters:  iContract   - �����祭�� �������
               iContCode   - �����䨪��� ������
               iDate       - ��� �஫����樨
               oResult     - ������� ��ࠡ�⪨
               oOk         - ����뢠�� �� ����������� �஫����樨 (�� ���樠����樨 - �����)
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE DepPogProlChance:
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.
   DEF OUTPUT PARAM oResult    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oOk        AS LOG  NO-UNDO.

   DEF VAR vTarif        AS CHAR NO-UNDO.
   DEF VAR vProduct      AS CHAR NO-UNDO.

   DEF BUFFER loan-cond  FOR loan-cond.
   DEF BUFFER comm-rate  FOR comm-rate.
   DEF BUFFER bcomm-rate FOR comm-rate.

      /* �� "������⠢" - ����஫� ��業⭮� �⠢�� �� �஫����樨 = ���,
      ** ����� �஫����஢��� �������஫쭮 */
   IF FGetSetting("������⠢", "", "��") EQ "���" THEN
      oOk = TRUE.
   ELSE
   DO:
         /* ��।��塞 ��� �� �������� */
      FIND LAST loan-cond WHERE
                loan-cond.contract  EQ iContract
         AND    loan-cond.cont-code EQ iContCode
         AND    loan-cond.since     LE iDate
      NO-LOCK NO-ERROR.
      IF AVAIL loan-cond THEN
      DO:
         ASSIGN
            vProduct = GetXAttrValueEx ("loan",
                                        iContract + "," + iContCode,
                                        "�த���",
                                        "?")
            vTarif   = GetXAttrValueEx ("loan-cond",
                                        loan-cond.contract + "," +
                                        loan-cond.cont-code + "," +
                                        STRING(loan-cond.since),
                                        "�த���",
                                        "?")
         .
            /* �஢�ઠ ����⢮����� ��� */
         FIND FIRST code WHERE
                    code.class EQ "�த�����"
            AND     code.code  EQ vTarif
         NO-LOCK NO-ERROR.
            /* �஢�ઠ �� �ப ����⢨� �த�� */
         FIND LAST tmp-code WHERE
                   tmp-code.class    EQ "�த���"
            AND    tmp-code.code     EQ vProduct
            AND    tmp-code.beg-date LE iDate
         NO-LOCK NO-ERROR.
         IF vProduct EQ "?" THEN
            oResult = "�� ������� �� ��⠭����� �த��.".
         ELSE IF NOT AVAIL tmp-code THEN
            oResult = "��⠭������� �� ������� �த�� <" + vProduct + "> �� ������.".
         ELSE IF     tmp-code.end-date NE ?
                 AND tmp-code.end-date LT iDate THEN
            oResult = "�ப ����⢨� �த�� <" + vProduct + "> ����祭.".
         ELSE  IF vTarif EQ "?" THEN
            oResult = "�� �᫮��� ������� �� ��⠭����� ���.".
         ELSE IF NOT AVAIL code THEN
            oResult = "��⠭������� �� �������饬 �᫮��� ��� <" + vTarif + "> �� �������.".
         ELSE
         DO:
               /* ��।���� %% �⠢�� %��� �� ���� � �ࠢ��� �� � �⠢��� ������� */
            FIND LAST comm-rate WHERE
                      comm-rate.kau        EQ iContract + "," + iContCode
               AND    comm-rate.commission EQ "%���"
               AND    comm-rate.since      LE iDate
            USE-INDEX kau NO-LOCK NO-ERROR.
            FIND LAST bcomm-rate WHERE
                      bcomm-rate.kau        EQ "�த���," + vTarif
               AND    bcomm-rate.commission EQ "%���"
               AND    bcomm-rate.since      LE iDate
            USE-INDEX kau NO-LOCK NO-ERROR.
               /* �᫨ �⠢�� �� ���﫠��, � �஫�����㥬 */
            IF     AVAIL comm-rate
               AND AVAIL bcomm-rate
               AND comm-rate.rate-comm EQ bcomm-rate.rate-comm THEN
               oOk = TRUE.
               /* ��� ��⮪��� */
            oResult = oResult + "�������騩 ���: " + vTarif + ".".
            IF NOT AVAIL bcomm-rate THEN
               oResult = oResult + " �� ������� ��������� %% �⠢�� ���.".
            IF NOT AVAIL comm-rate THEN
               oResult = oResult + " �� ������� ��������� %% �⠢�� �������.".
            IF     AVAIL comm-rate
               AND AVAIL bcomm-rate
               AND comm-rate.rate-comm NE bcomm-rate.rate-comm THEN
               oResult = oResult + " �������騥 %% �⠢�� ������� � ��� �� ࠢ��.".
          END.
      END.
   END.

END PROCEDURE.


   /* ===============================-=--=-= */
   /* �����㬥�� ��� ����祭�� �⠢�� ��� */
PROCEDURE GetTarifRate.
   DEF INPUT  PARAM iTarif      AS CHAR NO-UNDO.   /* ��� ��� */
   DEF INPUT  PARAM iCommission AS CHAR NO-UNDO.   /* ��� �⠢�� */
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.   /* �� ���� */
   DEF OUTPUT PARAM oRate       AS DEC  NO-UNDO.   /* �⠢�� */
      /* �������㥬 ���� */
   DEF BUFFER comm-rate  FOR comm-rate.
      /* �᫨ ��� ����� */
   IF {assigned iTarif} THEN
   DO:
      FIND LAST comm-rate WHERE
                comm-rate.kau        EQ "�த���," + iTarif
         AND    comm-rate.commission EQ iCommission
         AND    comm-rate.since      LE iDate
      USE-INDEX kau NO-LOCK NO-ERROR.
      IF AVAIL comm-rate THEN
         oRate = comm-rate.rate-comm.
   END.
END PROCEDURE.

   /* ������� ��ࠬ���� ��� */
PROCEDURE GetParamTarif.
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. /* �����䨪���   */
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. /* �������        */
   DEF INPUT  PARAM iDate         AS DATE NO-UNDO. /* ���            */
   DEF OUTPUT PARAM oStrParam     AS CHAR NO-UNDO. /* ���� �ᯥ譮�� */

      /* ��६���� � ������ �࠭���� ��ࠬ���� �।�⮢���� */
   DEF VAR vCurrency   AS CHAR NO-UNDO.
   DEF VAR vContType   AS CHAR NO-UNDO.
   DEF VAR vProduct    AS CHAR NO-UNDO.
   DEF VAR vRate       AS CHAR NO-UNDO.
   DEF VAR vKolDok     AS CHAR NO-UNDO.
   DEF VAR vTarif      AS CHAR NO-UNDO.
   DEF VAR vPoruchit   AS CHAR NO-UNDO.
   DEF VAR vVznos      AS CHAR NO-UNDO.
   DEF VAR vVznosProc  AS CHAR NO-UNDO.
   DEF VAR vStrah      AS CHAR NO-UNDO.
   DEF VAR vIndeks     AS CHAR NO-UNDO.
   DEF VAR vTypeClient AS CHAR NO-UNDO.
   DEF VAR vSumma      AS CHAR NO-UNDO.
   DEF VAR vSrok       AS CHAR NO-UNDO.
   DEF VAR vSurr       AS CHAR NO-UNDO.   /* ���ண��� */

   DEF BUFFER loan FOR loan. /* ���������� ���� */

   mb:
   DO ON ERROR UNDO, LEAVE:

         /* �饬 ������� */
      FIND FIRST loan WHERE
                 loan.contract  EQ iContract
         AND     loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE mb.

         /* ����砥� ������, ⨯ �।�� � ������� � �� */
      ASSIGN
         vSurr       = loan.contract + "," + loan.cont-code
         vCurrency   = loan.currency
         vContType   = loan.cont-type
         vProduct    = GetXAttrValueEx ("loan", vSurr, "�த���"    , "")
         vKolDok     = GetXAttrValueEx ("loan", vSurr, "�த������" , "")
         vTarif      = GetXAttrValueEx ("loan", vSurr, "�த���"    , "")
         vPoruchit   = GetXAttrValueEx ("loan", vSurr, "�த�����"  , "")
         vVznos      = GetXAttrValueEx ("loan", vSurr, "�த�����"  , "")
         vStrah      = GetXAttrValueEx ("loan", vSurr, "�த����"  , "")
         vIndeks     = GetXAttrValueEx ("loan", vSurr, "�த������" , "")
         vTypeClient = GetXAttrValueEx ("loan", vSurr, "�த�����"  , "")
         vRate       = GetXAttrValueEx ("loan", vSurr, "�த���⨭�", "")
         vRate       = IF NUM-ENTRIES(vRate, "R") GE 2
                          THEN ENTRY(2, vRate, "R")
                          ELSE vRate
      .
         /* ��।���� �㬬� */
      FIND LAST term-obl WHERE
                term-obl.contract  EQ iContract
         AND    term-obl.cont-code EQ iContCode
         AND    term-obl.idnt      EQ 2
         AND    term-obl.end-date  LE iDate
      NO-LOCK NO-ERROR.
         /* �᫨ ��諨, � ��࠭塞 */
      IF AVAIL term-obl THEN
         vSumma = TRIM( STRING(term-obl.amt-rub, ">>>>>>>9.99") ).
         /* �饬 �᫮��� ������� ��� ����祭�� �ப� � ���� ��� */
      FIND FIRST loan-cond WHERE
                 loan-cond.contract  EQ iContract
         AND     loan-cond.cont-code EQ iContCode
         AND     loan-cond.since     EQ loan.open-date
      NO-LOCK NO-ERROR.
         /* �᫨ ��諨, � ����砥� �� */
      IF AVAIL loan-cond THEN
         ASSIGN
            vSurr  = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
            vSrok  = GetXAttrValue("loan-cond", vSurr, "NMonthes")
            vTarif = GetXAttrValue("loan-cond", vSurr, "�த���")
         .
         /* ��ନ�㥬 �����頥��� ��ப� */
      oStrParam = vProduct       + ";" +
                  STRING(vSumma) + ";" +
                  vCurrency      + ";" +
                  vSrok          + ";" +
                  vRate          + ";" +
                  vKolDok        + ";" +
                  vTarif         + ";" +
                  vContType      + ";" +
                  vPoruchit      + ";" +
                  vVznos         + ";" +
                  vVznosProc     + ";" +
                  vStrah         + ";" +
                  vIndeks        + ";" +
                  vTypeClient.
   END. /* mb: */
END PROCEDURE.


   /* ==========================================-=--=-= */
   /* �����㬥�� ��� ���᫥��� ���� ��� �� �������� */
PROCEDURE GetLoanTarif.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.        /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.        /* � ������� */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.        /* ��� */
   DEF INPUT  PARAM iCalcTrf   AS LOG  NO-UNDO.        /* �᪠�� �� ��� �� �� "�த���" */
   DEF OUTPUT PARAM oTarif     AS CHAR NO-UNDO INIT ?. /* ���� */

   DEF VAR vStrParam   AS CHAR NO-UNDO. /* ��ࠬ���� �த�� � ������� */
   DEF VAR vProdTmpC   AS CHAR NO-UNDO. /* ��� �த�� */
   DEF VAR vIndCount   AS INT64  NO-UNDO. /* ������⢮ �த�⮢ � ��������� */
   DEF VAR vSummMin    AS CHAR NO-UNDO. /* �������쭠� �㬬� */
   DEF VAR vSummMax    AS CHAR NO-UNDO. /* ���ᨬ��쭠� �㬬� */
   DEF VAR vSrokMin    AS CHAR NO-UNDO. /* ��������� �ப */
   DEF VAR vSrokMax    AS CHAR NO-UNDO. /* ���ᨬ���� �ப */
   DEF VAR vVznosMin   AS CHAR NO-UNDO. /* ��������� ����� */
   DEF VAR vVznosMax   AS CHAR NO-UNDO. /* ���ᨬ���� ����� */
   DEF VAR vSurr       AS CHAR NO-UNDO. /* ������ - ������ �� ⨯� �த�� */
   DEF VAR vSurrTmp    AS CHAR NO-UNDO.
   DEF VAR vTmp        AS CHAR NO-UNDO.
   DEF VAR vI          AS INT64  NO-UNDO.
      /* ��ࠬ���� �த�� */
   DEF VAR vProduct    AS CHAR NO-UNDO. /* ��� */
   DEF VAR vLoanSumm   AS CHAR NO-UNDO. /* �㬬� ������� */
   DEF VAR vCurrency   AS CHAR NO-UNDO. /* ����� */
   DEF VAR vSrok       AS CHAR NO-UNDO. /* �ப */
   DEF VAR vRate       AS CHAR NO-UNDO. /* ���⨭� */
   DEF VAR vKolDok     AS CHAR NO-UNDO. /* ���-�� ���㬥�⮢ */
   DEF VAR vTarif      AS CHAR NO-UNDO. /* ���� */
   DEF VAR vContType   AS CHAR NO-UNDO. /* ��� ������� */
   DEF VAR vPoruchit   AS CHAR NO-UNDO. /* �����⥫� */
   DEF VAR vVznos      AS CHAR NO-UNDO. /* ����� */
   DEF VAR vVznosProc  AS CHAR NO-UNDO. /* %% ����� ? */
   DEF VAR vStrah      AS CHAR NO-UNDO. /* ���客�� */
   DEF VAR vIndeks     AS CHAR NO-UNDO. /* ������ */
   DEF VAR vTypeClient AS CHAR NO-UNDO. /* ��� ������ */

      /* ����稬 ��ࠬ���� ������� */
   RUN GetParamTarif (iContract,
                      iContCode,
                      iDate,
                      OUTPUT vStrParam).
      /* �᫨ ���� ᯨ᮪, � ��������� ����� */
   IF NUM-ENTRIES(vStrParam, ";") NE 0 THEN
   DO:
      ASSIGN
         vProduct    = ENTRY( 1, vStrParam, ";")
         vLoanSumm   = ENTRY( 2, vStrParam, ";")
         vCurrency   = ENTRY( 3, vStrParam, ";")
         vSrok       = ENTRY( 4, vStrParam, ";")
         vRate       = ENTRY( 5, vStrParam, ";")
         vKolDok     = ENTRY( 6, vStrParam, ";")
         vTarif      = IF iCalcTrf THEN ENTRY( 7, vStrParam, ";") ELSE ""
         vContType   = ENTRY( 8, vStrParam, ";")
         vPoruchit   = ENTRY( 9, vStrParam, ";")
         vVznos      = ENTRY(10, vStrParam, ";")
         vVznosProc  = ENTRY(11, vStrParam, ";")
         vStrah      = ENTRY(12, vStrParam, ";")
         vIndeks     = ENTRY(13, vStrParam, ";")
         vTypeClient = ENTRY(14, vStrParam, ";")
         vPoruchit   = IF vPoruchit EQ "" THEN "���" ELSE vPoruchit
         vStrah      = IF vStrah    EQ "" THEN "���" ELSE vStrah
         vSummMin    = "0.00"
         vSummMax    = "0.00"
         vSrokMin    = "0"
         vSrokMax    = "0"
         vVznosMin   = "0.00"
         vVznosMax   = "0.00"
      .
         /* �᫨ ��� ��।���� �� �� �������, � �����頥� ��� */
      IF vTarif NE "" THEN
         oTarif = vTarif.
         /* ���� �饬 �� indicate */
      ELSE
      DO:
            /* �饬 �� "�த���"-� ⨯ �த�� */
         FIND LAST tmp-code WHERE
                   tmp-code.class    EQ "�த���"
            AND    tmp-code.code     EQ vProduct
            AND    tmp-code.beg-date LE iDate
            AND   (tmp-code.end-date GE iDate
                OR tmp-code.end-date EQ ?)
         NO-LOCK NO-ERROR.
         IF AVAIL tmp-code THEN
            vProdTmpC = tmp-code.val.
            /* ��।���� �������� �㬬 */
         ASSIGN
            vTmp  = ""
            vSurr = IF CAN-DO("��⮏த,���ॡ�த,������", vProdTmpC)
                       THEN vProduct + "," + vCurrency
                       ELSE vProduct
         .
            /* ������� ���ᨢ ������ �࠭�� �㬬 (vTmp) */
         vIndCount = GetRefCrVal (vProdTmpC,
                                   "�㬬� �",
                                   iDate,
                                   vSurr,
                                  (TEMP-TABLE ttIndicate:HANDLE)).
         FOR EACH ttIndicate WHERE
                  ttIndicate.fdec LE DEC(vLoanSumm):
            vTmp = vTmp + TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99")) + ",".
         END.
            /* � 横�� �� �������� ������ �࠭�栬 �㬬 ��।��塞 ������ �࠭��� */
         BLK1:
         DO vI = 1 TO NUM-ENTRIES(vTmp) - 1:
            vSurrTmp = vSurr + "," + ENTRY(vI, vTmp).
            vIndCount = GetRefCrVal (vProdTmpC,
                                      "�㬬� ��",
                                      iDate,
                                      vSurrTmp,
                                     (TEMP-TABLE ttIndicate:HANDLE)).
            FOR EACH ttIndicate WHERE
                     ttIndicate.fdec GE DEC(vLoanSumm):
               ASSIGN
                  vSummMin = ENTRY(vI, vTmp)
                  vSummMax = TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99"))
               .
               LEAVE BLK1.
            END.
         END. /* BLK1 */
            /* ��।���� �������� �ப�� */
         ASSIGN
            vTmp  = ""
            vSurr = IF CAN-DO("��⮏த,���ॡ�த,������", vProdTmpC)
                       THEN vSurr + "," + vSummMin + "," + vSummMax
                       ELSE vSurr + "," + vSummMin + "," + vSummMax + "," + vCurrency
         .
         vIndCount = GetRefCrVal (vProdTmpC,
                                  "�ப �",
                                  iDate,
                                  vSurr,
                                 (TEMP-TABLE ttIndicate:HANDLE)).
         FOR FIRST ttIndicate WHERE
                   ttIndicate.fint LE INT64(vSrok):
            vTmp = vTmp + TRIM(STRING(INT64(ttIndicate.fint), ">>>>>>>9")) + ",".
         END.
         BLK2:
         DO vI = 1 TO NUM-ENTRIES(vTmp) - 1:
            vSurrTmp = vSurr + "," + ENTRY(vI, vTmp).
            vIndCount = GetRefCrVal (vProdTmpC,
                                     "�ப ��",
                                     iDate,
                                     vSurrTmp,
                                    (TEMP-TABLE ttIndicate:HANDLE)).
            FOR EACH ttIndicate WHERE
                     ttIndicate.fint GE INT64(vSrok):
               ASSIGN
                  vSrokMin = ENTRY(vI, vTmp)
                  vSrokMax = TRIM(STRING(INT64(ttIndicate.fint), ">>>>>>>9"))
               .
               LEAVE BLK2.
            END.
         END. /* BLK2 */
            /* ��।���� �������� ����ᮢ */
         ASSIGN
            vTmp  = ""
            vSurr = IF CAN-DO("��⮏த,���ॡ�த", vProdTmpC)
                       THEN vSurr + "," + vSrokMin + "," + vSrokMax
                       ELSE vSurr + "," + vSrok
            vSurr = IF CAN-DO("��⮏த,�।�த", vProdTmpC)
                       THEN vSurr + "," + vPoruchit
                       ELSE vSurr
         .
         vIndCount = GetRefCrVal (vProdTmpC,
                                  "����� �",
                                  iDate,
                                  vSurr,
                                 (TEMP-TABLE ttIndicate:HANDLE)).
         FOR FIRST ttIndicate WHERE
                   ttIndicate.fdec LE DEC(vVznos):
            vTmp = vTmp + TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99")) + ",".
         END.
         BLK3:
         DO vI = 1 TO NUM-ENTRIES(vTmp) - 1:
            vSurrTmp = vSurr + "," + ENTRY(vI, vTmp).
            vIndCount = GetRefCrVal (vProdTmpC,
                                     "����� ��",
                                     iDate,
                                     vSurrTmp,
                                    (TEMP-TABLE ttIndicate:HANDLE)).
            FOR EACH ttIndicate WHERE
                     ttIndicate.fdec GE DEC(vVznos):
               ASSIGN
                  vVznosMin = ENTRY(vI, vTmp)
                  vVznosMax = TRIM(STRING(DEC(ttIndicate.fdec), ">>>>>>>9.99"))
               .
               LEAVE BLK3.
            END.
         END. /* BLK3 */

            /* ��।������ ��� ������ �� ⨯� �த�� */
         CASE vProdTmpC:
            WHEN "�।�த" THEN
                  /* �த��; �㬬� �; �㬬� ��; ���; �ப; ���; ���㬥���; ���⨭�; ���� */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vCurrency + "," +
                                   vSrok     + "," +
                                   vPoruchit + "," +
                                   vKolDok   + "," +
                                   vRate).
            WHEN "��⮏த" THEN
                  /* �த��; ���; �㬬� �; �㬬� ��; ��.�; ��.�; ���; ��.�; ��.��; ���; ���; ����; ���� */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vCurrency + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vSrokMin  + "," +
                                   vSrokMax  + "," +
                                   vPoruchit + "," +
                                   vVznosMin + "," +
                                   vVznosMax + "," +
                                   vKolDok   + "," +
                                   vStrah    + "," +
                                   vRate).
            WHEN "���ॡ�த" THEN
                  /* �த��; ���; �㬬� �; �㬬� ��; ��.�; ��.�; ��.�; ��.��; ���; ����; ��; ���� */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vCurrency + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vSrokMin  + "," +
                                   vSrokMax  + "," +
                                   vVznosMin + "," +
                                   vVznosMax + "," +
                                   vKolDok   + "," +
                                   vRate     + "," +
                                   vIndeks).
            WHEN "���த" THEN
                  /* �த��; �㬬� �; �㬬� ��; ���; �ப; �� �; �� ��; ���� */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vCurrency + "," +
                                   vSrok     + "," +
                                   vVznosMin + "," +
                                   vVznosMax).
            WHEN "������" THEN
                  /* �த��; ���; �㬬� �; �㬬� ��; ��.�; ��.�; ���� */
               oTarif = GetRefVal (vProdTmpC,
                                   iDate,
                                   vProduct  + "," +
                                   vCurrency + "," +
                                   vSummMin  + "," +
                                   vSummMax  + "," +
                                   vSrokMin  + "," +
                                   vSrokMax).
         END CASE.
      END.
   END.

      /* �஢�ਬ, � ������� �� ⠪�� ��� � �����䨪���? */
   IF oTarif NE "" THEN
   DO:
      FIND FIRST code WHERE
                 code.class  EQ '�த�����'
         AND     code.parent EQ vProdTmpC
         AND     code.code   EQ oTarif
      NO-LOCK NO-ERROR.
      IF NOT AVAIL code THEN
         oTarif = "".
   END.
END PROCEDURE.

   /* ====================================================-=--=-= */
   /* ��楤�� ��� ᮧ����� �����ᨨ %��� (������� �ਬ������
   ** ��� ��।������ ���� १�ࢨ஢���� � ����ᨬ��� �� �᪠) */
PROCEDURE CrResRate.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.        /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.        /* � ������� */
   DEF INPUT  PARAM iRate      AS DEC  NO-UNDO.        /* �⠢�� */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.        /* ���   */
      /* ���������� ���� */
   DEF BUFFER loan      FOR loan.
   DEF BUFFER comm-rate FOR comm-rate.
      /* ���� ������� */
   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* �饬 ������� */
      FIND FIRST comm-rate WHERE
                 comm-rate.commission EQ "%���"
         AND     comm-rate.kau        EQ loan.contract + "," + loan.cont-code
         AND     comm-rate.currency   EQ loan.currency
         AND     comm-rate.acct       EQ "0"
         AND     comm-rate.min-value  EQ 0.00
         AND     comm-rate.period     EQ 0
         AND     comm-rate.since      LE iDate
      USE-INDEX kau NO-LOCK NO-ERROR.
      IF NOT AVAIL comm-rate THEN
      DO:
         CREATE comm-rate.
         ASSIGN
            comm-rate.commission = "%���"
            comm-rate.kau        = loan.contract + "," + loan.cont-code
            comm-rate.currency   = loan.currency
            comm-rate.rate-comm  = iRate
            comm-rate.acct       = "0"
            comm-rate.min-value  = 0.00
            comm-rate.period     = 0
            comm-rate.since      = iDate
         .
      END.
   END.
END PROCEDURE.

   /* ====================================================-=--=-= */
   /* ��楤�� ��� ��࠭���� �����ᨨ %��� (������� �ਬ������
   ** ��� ��।������ ���� १�ࢨ஢���� � ����ᨬ��� �� �᪠)
      � �⫨稥 �� CrResRate ������� ���祭��, �᫨ ������� �� ���� 㦥 ����,
      � �� ⮫쪮 ᮧ���� ����� */
PROCEDURE SaveResRate.
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.        /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.        /* � ������� */
   DEF INPUT  PARAM iRate      AS DEC  NO-UNDO.        /* �⠢�� */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.        /* ���   */
      /* ���������� ���� */
   DEF BUFFER loan      FOR loan.
   DEF BUFFER comm-rate FOR comm-rate.
      /* ���� ������� */
   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* �饬 ������� */
      DO WHILE TRUE:
         FIND FIRST comm-rate WHERE
                    comm-rate.commission EQ "%���"
            AND     comm-rate.kau        EQ loan.contract + "," + loan.cont-code
            AND     comm-rate.currency   EQ loan.currency
            AND     comm-rate.acct       EQ "0"
            AND     comm-rate.min-value  EQ 0.00
            AND     comm-rate.period     EQ 0
            AND     comm-rate.since      LE iDate
         USE-INDEX kau EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF LOCKED comm-rate THEN
         DO:
            pick-value = "yes".
            RUN Fill-SysMes IN h_tmess ("", "", "4",
                "�� 㤠���� ��࠭��� �����樥�� १�ࢨ஢����.~n" +
                "������ �������஢��� ��㣨� ���짮��⥫��.~n" +
                "������� ������ (��) / �⬥���� (���)?").
            IF pick-value EQ "no" THEN
               RETURN ERROR "�� 㤠���� ��࠭��� �����樥�� १�ࢨ஢����".
         END.
         ELSE
            LEAVE.
      END.

      IF NOT AVAIL comm-rate THEN
      DO:
         CREATE comm-rate.
         ASSIGN
            comm-rate.commission = "%���"
            comm-rate.kau        = loan.contract + "," + loan.cont-code
            comm-rate.currency   = loan.currency
            comm-rate.acct       = "0"
            comm-rate.min-value  = 0.00
            comm-rate.period     = 0
            comm-rate.since      = iDate
         .
      END.
      comm-rate.rate-comm = iRate.
      RELEASE comm-rate.
   END.
END PROCEDURE.

/* ���᫥��� ������६����� �����ᨩ */
PROCEDURE CalcCommStart.
   DEF INPUT  PARAM iComm         AS CHAR NO-UNDO. /* ��� �����ᨨ          */
   DEF INPUT  PARAM iCalcBase     AS DEC  NO-UNDO. /* ���� ����. �����ᨨ */
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. /* �����䨪���         */
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. /* �������              */
   DEF INPUT  PARAM iCurrency     AS CHAR NO-UNDO. /* �����                */
   DEF INPUT  PARAM iAvailComm    AS CHAR NO-UNDO. /* ����� ���.�������   */
   DEF OUTPUT PARAM oCommVal      AS DEC  NO-UNDO. /* �㬬� �����ᨨ        */
   DEF OUTPUT PARAM oOk           AS INT64  NO-UNDO. /* ���� �ᯥ譮��       */

   DEF VAR vProdCode   AS CHAR NO-UNDO.
   DEF VAR vCommToNach AS CHAR NO-UNDO.

   DEF BUFFER loan FOR loan. /* ���������� ����� */

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* ������ �������, � ��� �த�� */
      FIND FIRST loan WHERE loan.contract  EQ iContract
                     AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      vProdCode = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"�த���","").
      IF vProdCode EQ "" THEN
         LEAVE mb.

      vCommToNach = GetCode ("������",vProdCode).

      /* �᫨ iAvailComm=��, � ����塞, � �� �࠭���� � ������,
      ** �᫨ iAvailComm=���, � ����塞 ����������(��) �� ᯨ᪠
      ** %���,%�� */
      IF    iAvailComm EQ "���"
        AND CAN-DO(vCommToNach,iComm) THEN
         LEAVE mb.

      IF    iAvailComm EQ "��"
        AND NOT CAN-DO(vCommToNach,iComm) THEN
         LEAVE mb.

      /* �஢��塞 ���� �� ⠪�� ������� �����
      ** �ᤨ ���, � �㣠���� � �뤠�� �訡�� */
      FIND FIRST commission WHERE commission.commission EQ iComm
                              AND commission.currency   EQ iCurrency
                              AND commission.min-value  EQ 0
                              AND commission.period     EQ 0
      NO-LOCK NO-ERROR.
      IF NOT AVAIL commission THEN
      DO:
         IF iAvailComm NE "?" THEN
            RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ������� " + iComm + " � ����� '" + iCurrency + "' � �ࠢ�筨�� '������ � ����'").
         LEAVE mb.
      END.

      FIND FIRST comm-rate WHERE comm-rate.commission EQ iComm
                             AND comm-rate.acct       EQ "0"
                             AND comm-rate.currency   EQ iCurrency
                             AND comm-rate.kau        EQ iContract + "," + iContCode
                             AND comm-rate.min-value  EQ 0
                             AND comm-rate.period     EQ 0
      USE-INDEX kau NO-LOCK NO-ERROR.
      IF NOT AVAIL comm-rate THEN
      DO:
         IF iAvailComm NE "?" THEN
            RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ������� " + iComm + " � ����� '" + iCurrency + "' �� �������� " + iContCode ).
         LEAVE mb.
      END.

      /* �᫨ ⨯ �����ᨨ "=", � ��� �㬬� � ��६,
      ** ���� ��⠥� */
      IF comm-rate.rate-fixed THEN
         oCommVal = comm-rate.rate-comm.
      ELSE
         oCommVal = iCalcBase * comm-rate.rate-comm / 100.

      oOk = 1.
   END. /* mb: */
END PROCEDURE.


   /* =======================================================-===--==-= */
   /* ��।������ ���� ����樨 �� ����                                */
PROCEDURE GetOpCodeFromAcct.
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iAcct       AS CHAR NO-UNDO.  /* ��� */
   DEF INPUT  PARAM iCurrency   AS CHAR NO-UNDO.  /* ����� */
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.  /* ��� */
   DEF INPUT  PARAM iSide       AS CHAR NO-UNDO.  /* "��" ��� "��" */
   DEF OUTPUT PARAM oOpCode     AS CHAR NO-UNDO.  /* ��� ����樨 */
      /* �������㥬 ����� */
   DEF BUFFER loan-acct FOR loan-acct.
   DEF BUFFER code      FOR code.
      /* �᫨ ��� ����樨 �� ���।�� */
   oOpCode = ?.
      /* ��।���� ஫�, � ���ன ����� ��� �易� � ������஬ */
   FIND FIRST loan-acct WHERE
              loan-acct.contract  EQ iContract
      AND     loan-acct.cont-code EQ iContCode
      AND     loan-acct.acct      EQ iAcct
      AND     loan-acct.currency  EQ iCurrency
      AND     loan-acct.since     LE iDate
   NO-LOCK NO-ERROR.
   IF AVAIL loan-acct THEN
   DO:
         /* �饬 � "����焮�" ������ ��� ஫� loan-acct.acct-type
         ** �� ������/�।��� (� ����ᨬ��� �� �室���� ��ࠬ���) */
      FIND FIRST code WHERE
                 code.class  EQ     "����焮�"
         AND     code.parent EQ     loan-acct.acct-type
         AND     code.code   BEGINS iSide
      NO-LOCK NO-ERROR.
      IF AVAIL code THEN
         oOpCode = REPLACE(code.code, iSide, "").
   END.

END PROCEDURE.

   /* =======================---=-=-=-=-=-=-=-=- */
   /* ����/�롮� ��� �� ஫�, ������ PlAcct  */
PROCEDURE ReqAcctByRole.
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iAcctRole AS CHAR NO-UNDO.  /* ���� ��� */
   DEF INPUT  PARAM iAcctCat  AS CHAR NO-UNDO.  /* ��� ��� */
   DEF INPUT  PARAM iPLAcct   AS CHAR NO-UNDO.  /* �����䨪��� PLAcct */
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.  /* ��� */
   DEF INPUT  PARAM iMess     AS LOG  NO-UNDO.  /* �뢮���� ᮮ�饭�� � ����室����� �롮� ��� */
   DEF OUTPUT PARAM oAcct     AS CHAR NO-UNDO.  /* ��� */

   DEF VAR vTemp        AS CHAR NO-UNDO.
   DEF VAR DTType       AS CHAR NO-UNDO.
   DEF VAR DTKind       AS CHAR NO-UNDO.
   DEF VAR DTCust       AS CHAR NO-UNDO.
   DEF VAR vBal2        AS CHAR NO-UNDO.
   DEF VAR vFlt         AS CHAR NO-UNDO EXTENT 2.

   DEF BUFFER loan      FOR loan.
   DEF BUFFER loan-acct FOR loan-acct.

      /* ������ ������� */
   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
         /* �饬 ��� �� ஫� � ��� � ����⥪� ��⮢ */
      FIND LAST loan-acct WHERE
                loan-acct.contract  EQ iContract
         AND    loan-acct.cont-code EQ iContCode
         AND    loan-acct.acct-type EQ iAcctRole
         AND    loan-acct.since     LE iDate
      NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN
      DO:
         oAcct = loan-acct.acct.
      END.
         /* �᫨ �� ������, � �롨ࠥ� �� ��㧥� ��⮢ */
      ELSE
      DO:
         RUN DTCust (loan.cust-cat,
                     loan.cust-id,
                     ?,
                     OUTPUT DTCust).

         ASSIGN
            vTemp      = FGetSetting("�302�", "PLAcct", "")
            iPLAcct    = IF vTemp NE ""
                            THEN vTemp
                            ELSE iPLAcct
            DTType     = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "DTType",
                                         GetXAttrInit(loan.class-code, "DTType"))
            DTType     = IF DTType = ? OR DTType = ""
                            THEN "*"
                            ELSE DTType
            DTKind     = GetXAttrValueEx("loan",
                                         iContract + "," + iContCode,
                                         "DTKind",
                                         GetXAttrInit(loan.class-code, "DTKind"))
            DTKind     = IF DTKind = ? OR DTKind = ""
                            THEN "*"
                            ELSE DTKind
            mask       = iAcctRole + CHR(1) + DTType + CHR(1) + DTCust + CHR(1) +
                            DTKind + CHR(1) + ( IF loan.currency = "" THEN "��" ELSE "���")
            pick-value = ?
         .
            /* ����稬 ���� ��� �� �����䨪��� (PLAcct) */
         RUN placct.p (iPLAcct,
                       iPLAcct,
                       "PlAcct",
                       0).

         IF     pick-value NE ?
            AND pick-value NE "" THEN
         DO:
            ASSIGN
               vBal2      = ENTRY(1, ENTRY(1, pick-value, "."), "*")
               vBal2      = IF LENGTH(vBal2) GE 5 THEN SUBSTR(vBal2, 1, 5) ELSE ""
               vFlt[1]    = "MustOffLdFlt|acct-cat"  + CHR(1) + "bal-acct"           + CHR(1) +
                            "acct"                   + CHR(1) + "currency"           + CHR(1) + "close-date1"
               vFlt[2]    = "YES|" + ( IF iAcctCat      EQ ? THEN "*" ELSE iAcctCat)      + CHR(1) +
                                     ( IF vBal2         EQ ? THEN "*" ELSE vBal2)         + CHR(1) +
                                     ( IF pick-value    EQ ? THEN "*" ELSE pick-value)    + CHR(1) +
                                                                 "*"                     + CHR(1) + "?"
               pick-value = ?
            .
            IF iMess THEN
               RUN Fill-SysMes IN h_tmess ("", "", "",
                                           "�롥�� ��� � ஫�� <" + iAcctRole + ">").

               /* �롨ࠥ� ��� */
            DO TRANS:
               RUN browseld.p ("acct",
                               vFlt[1],
                               vFlt[2],
                               "",
                               4).
            END.
            IF pick-value NE ? THEN
               oAcct = ENTRY(1, pick-value).
         END.
      END.
   END.
END.

   /* ================---=-=-=-=-=-=-=-=- */
 /* ��᫮��� ��� �맮�� CRCONDEX */
PROCEDURE CRCOND:
   DEF INPUT PARAM iContract    AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode    AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate        AS DATE NO-UNDO.
   DEF INPUT PARAM iLIstOutStav AS CHAR NO-UNDO. /* �ய�᪠�� �⠢�� , ����� �� �㦭� ����஢��� � ��ண� �᫮���*/

   RUN CRCONDEX(iContract,
                iContCode,
                iDate,
                iLIstOutStav,
                ?,
                ?).

END PROCEDURE.

/* =============================================================== */
   /* ��楤�� ᮧ����� �᫮��� �� ���� */
PROCEDURE CRCONDEX:
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.
   DEF INPUT PARAM iLIstOutStav AS CHAR NO-UNDO. /* �ய�᪠�� �⠢�� , ����� �� �㦭� ����஢��� � ��ண� �᫮���*/
   DEF INPUT PARAM iPaySum      AS DEC  NO-UNDO.
   DEF INPUT PARAM iPayType     AS CHAR NO-UNDO.

   DEF VAR vSurr      AS CHAR NO-UNDO.
   DEF VAR vNDays     AS INT64  NO-UNDO.
   DEF VAR vNMonthes  AS INT64  NO-UNDO.
   DEF VAR vNYears    AS INT64  NO-UNDO.
   DEF VAR vPov       AS DEC  NO-UNDO.
   DEF VAR vProcs     AS CHAR NO-UNDO.
   DEF VAR vCntr      AS INT64  NO-UNDO.
   DEF VAR vlgot      AS INT64   NO-UNDO.
   DEF VAR vlgotpr    AS INT64   NO-UNDO.
   DEF VAR vErrMsg    AS CHAR   NO-UNDO.

    DEF VAR vSumOD     AS DECIMAL NO-UNDO .

   DEF BUFFER b-loan       FOR loan.      /* ������ */
   DEF BUFFER loan-cond    FOR loan-cond.
   DEF BUFFER b-loan-cond  FOR loan-cond. /* �᫮��� ᤥ��� */
   DEF BUFFER comm-rate    FOR comm-rate.
   DEF BUFFER b-comm-rate  FOR comm-rate.
   DEF BUFFER c-comm-rate  FOR comm-rate.
   DEF BUFFER term-obl     FOR term-obl.
   DEF BUFFER bto-copy     FOR term-obl.
    DEF BUFFER fterm-obl    FOR term-obl.

   IF iPayType EQ ? THEN iPayType = "���⮪".
   IF iLIstOutStav EQ ? THEN iLIstOutStav = "".

   MAIN-BLOCK:
   DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

         /* �᫨ ���� �᫮��� �� 㪠������ ���� ... */
      FIND FIRST b-loan-cond WHERE
                 b-loan-cond.contract  EQ iContract
         AND     b-loan-cond.cont-code EQ iContCode
         AND     b-loan-cond.since     EQ iDate
      NO-LOCK NO-ERROR.
      IF AVAIL (b-loan-cond) THEN
         LEAVE MAIN-BLOCK. /* ... � ��室�� */

      FIND FIRST b-loan WHERE
                 b-loan.Contract  EQ iContract
         AND     b-loan.Cont-Code EQ iContCode
      NO-LOCK NO-ERROR.
         /* �饬 �।��騥 �᫮���  */
      FIND LAST loan-cond WHERE
                loan-cond.contract  EQ iContract
            AND loan-cond.cont-code EQ iContCode
            AND loan-cond.since     LT iDate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-cond THEN
         LEAVE MAIN-BLOCK.

         /* ������� ᯨ᮪ ��������� �⠢�� */
      FOR EACH comm-rate WHERE
               comm-rate.kau        EQ b-loan.contract + "," + b-loan.cont-code
          AND  NOT CAN-DO(iLIstOutStav,comm-rate.commission) NO-LOCK:
         IF LOOKUP (comm-rate.commission, vProcs) EQ 0 THEN
            vProcs = vProcs + comm-rate.commission + ",".
      END.
         /* ������� �᫮��� */
      CREATE b-loan-cond.

      IF GetXAttrValueEx("loan-cond", loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since), "�奬�����","") EQ "����७�஢�����" THEN
         BUFFER-COPY loan-cond EXCEPT since int-period int-date TO b-loan-cond.
      ELSE
      BUFFER-COPY loan-cond EXCEPT since TO b-loan-cond.
      ASSIGN
         b-loan-cond.since = iDate
         vSurr             = b-loan-cond.Contract + "," +
                             b-loan-cond.Cont-Code + "," +
                             STRING(b-loan-cond.since)
      .

      /* �᫨ ��� ����砭�� ������� ����� ��� ࠢ�� ���,
      ** �� ������ ᮧ������ �᫮���, ᮧ���� �������� �㬬� (idnt=2)
      ** �� ���� �᫮��� �� �㬬� ���� */
      IF b-loan.end-date LE iDate THEN
      DO:
         CREATE term-obl.
         ASSIGN
            term-obl.contract    = b-loan.contract
            term-obl.cont-code   = b-loan.cont-code
            term-obl.nn          = 1
            term-obl.end-date    = iDate
            term-obl.idnt        = 2
            term-obl.fop-date    = iDate
            term-obl.currency    = b-loan.currency
            term-obl.acct        = "0"
            term-obl.cont-type   = b-loan.cont-type
            term-obl.amt-rub     = 0
         .
      END.

      IF iPaySum = ? THEN DO:
      /* ������� ���⮪ */
      FIND LAST  fterm-obl  WHERE
                 fterm-obl.contract  EQ b-loan.contract
            AND  fterm-obl.cont-code EQ b-loan.cont-code
            AND  fterm-obl.idnt      EQ 2
            AND  fterm-obl.end-date  LE iDate
            NO-LOCK NO-ERROR .
         IF AVAILABLE fterm-obl THEN  vSumOD = fterm-obl.amt-rub.
      END.
      ELSE
         vSumOD = iPaySum.
            
         /* �����㥬 � ᮧ���� �� */
      RUN CopySigns IN h_xclass (loan-cond.Class-Code,      /* ���筨� */
                                 loan-cond.Contract + "," + loan-cond.Cont-Code + "," + STRING(loan-cond.since),
                                 b-loan-cond.Class-Code,    /* �ਥ���� */
                                 vSurr)
                                 NO-ERROR.

      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "PayType",
                     iPayType).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "PaySum",
                     STRING(vSumOD)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "����������",
                     "").
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "CondEndDate",
                     "").
      RUN DMY_In_Per (iDate,
                      b-loan.end-date,
                      OUTPUT vNDays,
                      OUTPUT vNMonthes,
                      OUTPUT vNYears).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "NDays",
                     STRING(vNDays)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr, "NMonthes",
                     STRING(vNMonthes)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "NYears",
                     STRING(vNYears)).

/* ���४��㥬 ������⢮ �죮��� ��ਮ��� */
      vlgot  = INT64(GetXAttrValueEx("loan-cond",
                                     vSurr,
                                     "�����⏥�","0")).
      vlgotpr = INT64(GetXAttrValueEx("loan-cond",
                                      vSurr,
                                      "�����⏥����","0")).
      IF vlgot GT 0 THEN
      FOR EACH term-obl WHERE term-obl.contract  EQ iContract
                          AND term-obl.cont-code EQ iContCode
                          AND term-obl.idnt      EQ 3
                          AND term-obl.end-date  GT loan-cond.since
                          AND term-obl.end-date  LE b-loan-cond.since NO-LOCK:
         vlgot = vlgot - 1.
         IF vlgot LE 0 THEN LEAVE.
      END.
      IF vlgotpr GT 0 THEN
      FOR EACH term-obl WHERE term-obl.contract  EQ iContract
                          AND term-obl.cont-code EQ iContCode
                          AND term-obl.idnt      EQ 1
                          AND term-obl.end-date  GT loan-cond.since
                          AND term-obl.end-date  LE b-loan-cond.since NO-LOCK:
         vlgotpr = vlgotpr - 1.
         IF vlgotpr LE 0 THEN LEAVE.
      END.
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "�����⏥�",
                     STRING(vlgot)).
      UpdateSignsEx (b-loan-cond.Class-Code,
                     vSurr,
                     "�����⏥����",
                     STRING(vlgotpr)).

         /* K����㥬 �⠢�� �� �᫮��� */
      DO vCntr = 1 TO NUM-ENTRIES(vProcs):
         IF ENTRY(vCntr, vProcs) NE "" THEN
         DO:
            FIND LAST comm-rate WHERE
                      comm-rate.kau        EQ iContract + "," + iContCode
               AND    comm-rate.commission EQ ENTRY(vCntr, vProcs)
               AND    comm-rate.since      LT iDate
               AND    comm-rate.acct       EQ "0"
               NO-LOCK NO-ERROR.
            IF     AVAIL comm-rate
               AND comm-rate.commission NE "%���" THEN
            DO:
               IF CAN-FIND (FIRST b-comm-rate WHERE
                                  b-comm-rate.kau        EQ comm-rate.kau
                              AND b-comm-rate.since      EQ iDate
                              AND b-comm-rate.commission EQ comm-rate.commission
                              AND b-comm-rate.acct       EQ comm-rate.acct) THEN
               DO:
                  vErrMsg = "��� ������� ~"" + iContract + "," + iContCode + 
                            "~" 㦥 ���� �⠢�� ~"" + comm-rate.commission +
                            "~" �� " + STRING(iDate) + ".".
                  UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
               END.
               CREATE b-comm-rate.
               BUFFER-COPY comm-rate EXCEPT since comm-rate-id TO b-comm-rate NO-ERROR.
               b-comm-rate.since = iDate.
            END.
         END.
      END.
      RELEASE b-comm-rate.
   END.
   IF {assigned vErrMsg} THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1",
                                  vErrMsg).
      RETURN ERROR vErrMsg.
   END.
END PROCEDURE.

   /* ================================== */
   /* ��楤�� ��������� �������饩 �⠢�� ������� �� ���� */
PROCEDURE UpdComm:
   DEF INPUT PARAM iContract    AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode    AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate        AS DATE NO-UNDO.
   DEF INPUT PARAM iCommision   AS CHAR NO-UNDO. /* �⠢�a */ 
   DEF INPUT PARAM iStav        AS DEC  NO-UNDO. /* ���祭�� */

   DEF BUFFER comm-rate    FOR comm-rate.
   MAIN-BLOCK:
   DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      IF iStav <> 0 THEN DO:
               /* ������塞 �⠢�� iCommision �㦭� ���祭��� */
         FIND FIRST comm-rate WHERE
                    comm-rate.kau        EQ iContract + "," + iContCode
                AND comm-rate.commission EQ iCommision
                AND comm-rate.since      EQ iDate
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE comm-rate THEN
         DO:
            IF LOCKED comm-rate THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","������� �������஢��� ��㣨� ���짮��⥫��").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","������� �� �������").
            RETURN.
         END.

         IF AVAIL comm-rate THEN
            ASSIGN
               comm-rate.rate-comm = iStav NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
         IF AVAIL comm-rate THEN
            RELEASE comm-rate NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
   END.

END PROCEDURE.

   /* ================================== */
/* �����頥� �㬬� ��業⮢, � ���᫥��� �� ��ਮ� �� �⠢�� */
PROCEDURE NachProcRate.
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iDateBeg    AS DATE NO-UNDO.  /* ��� ��砫� ���� */
   DEF INPUT  PARAM iDateEnd    AS DATE NO-UNDO.  /* ��� ����砭�� ���� */
   DEF INPUT  PARAM iCommission AS CHAR NO-UNDO.  /* ��� �⠢�� */
   DEF INPUT  PARAM iBasePar    AS CHAR NO-UNDO.  /* ��ࠬ���� ��� ����� ���� */
   DEF OUTPUT PARAM oResult     AS DEC  NO-UNDO.

   DEF VAR vAmnt       AS DEC  NO-UNDO.   /* �㬬� */
   DEF VAR vCommission AS CHAR NO-UNDO.   /* ������� �⠢�� */
   DEF VAR vBaseComm   AS DEC  NO-UNDO.   /* ������� �⠢�� %% */
   DEF VAR vPenyComm   AS DEC  NO-UNDO.   /* ���䭠� �⠢�� %% */

   DEF VAR vSurr     AS CHAR NO-UNDO.
   DEF VAR vBaseNach AS CHAR NO-UNDO.
   DEF VAR vBaseStav AS CHAR NO-UNDO.
   DEF VAR vN        AS INT64  NO-UNDO.

   DEF VAR dat-per   AS DATE NO-UNDO.

      /* �᫨ ��ࠬ���� ��� ����� ���� �� ��।��� - ��६ �⠭���⭮ 0+7 */
   IF iBasePar EQ ? THEN
      iBasePar = "0+7".

   FIND FIRST loan WHERE
              loan.contract  EQ iContract
      AND     loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      oResult = ?.
   ELSE
   DO:
         /* ����祭�� ���� ���室� �� �������� 39�. */
      {ch_dat_p.i}
         /* ����砥� ���祭�� �� "�������" � "�����⠢" */
      ASSIGN
         vSurr     = loan.contract + "," + loan.cont-code
         vBaseNach = GetXattrValueEx("loan", vSurr, "�������",  "")
         vBaseStav = GetXattrValueEx("loan", vSurr, "�����⠢", "")
         vN        = NUM-ENTRIES(vBaseStav)
      .
         /* ������� � �� ������� � ��ࠬ���� ��� ���� ���� */
      UpdateSignsEx(loan.Class-Code, vSurr, "�������",  vBaseNach + ( IF vN EQ 0 THEN "" ELSE ",") + iBasePar).
      UpdateSignsEx(loan.Class-Code, vSurr, "�����⠢", vBaseStav + ( IF vN EQ 0 THEN "" ELSE ",") + iCommission).

         /* ����塞 ⠡���� */
     {empty otch1}
         /* ����᪠�� ���᫥��� ⮫쪮 �� ��।����� �����ᨨ */
      RUN lnscheme.p (iContract,    /* �����祭�� ������� */
                      iContCode,    /* ����� ������� */
                      iDateBeg,     /* ��� ��砫� ���� */
                      iDateEnd,     /* ��� ����砭�� ���� */
                      dat-per,      /* ��� ���室� �� 39� */
                      vN + 1,       /* ������ �⠢�� � �� "�������" */
                      1).           /* �ᥣ�� ��।����� 1 �� ��. */
         /* "����ࠥ�" �㬬� � ���᫥��� */
      FOR EACH otch1:
         vAmnt = vAmnt + otch1.summ_pr.
      END.
         /* �����頥� ����� ���祭�� �� */
      UpdateSignsEx(loan.Class-Code, vSurr, "�������",  vBaseNach).
      UpdateSignsEx(loan.Class-Code, vSurr, "�����⠢", vBaseStav).

      oResult = vAmnt.
   END.

   RETURN.
END PROCEDURE.

/* �������� ������ �᫮��� �� �������� */
PROCEDURE CrNewTermObl.
   DEFINE INPUT  PARAMETER iContract AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iSince    AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iSumma    AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrMes   AS CHARACTER   NO-UNDO.

   DEF BUFFER bLoanCond  FOR loan-cond.
   DEF BUFFER bLoanCond2 FOR loan-cond.
   DEF BUFFER bSigns     FOR signs.
   DEF BUFFER bSigns2    FOR signs.
   DEF BUFFER bTermObl   FOR term-obl.
   DEF BUFFER bTermObl2  FOR term-obl.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:

      /* ���� �᫮��� */
      RUN RE_L_COND (iContract,
                     iContCode,
                     iSince,
                     BUFFER bLoanCond).

      CREATE bLoanCond2.
      BUFFER-COPY bLoanCond EXCEPT since TO bLoanCond2.
      bLoanCond2.since = iSince.

      FOR EACH bSigns WHERE bSigns.file-name EQ "loan-cond"
                        AND bSigns.surrogate EQ bLoanCond2.contract + "," + bLoanCond2.cont-code
         NO-LOCK:

         CREATE bSigns2.
         BUFFER-COPY bSigns EXCEPT surrogate TO bSigns2.
         bSigns2.surrogate = bLoanCond2.contract + "," + bLoanCond2.cont-code.
      END.

      /* ���� ��������� ���⪠ */
      RUN RE_TERM_OBL (iContract,
                       iContCode,
                       2,
                       bLoanCond.since,
                       BUFFER bTermObl).

      CREATE bTermObl2.
      BUFFER-COPY bTermObl EXCEPT end-date amt-rub TO bTermObl2.
      ASSIGN
         bTermObl2.amt-rub   = iSumma
         bTermObl2.end-date  = iSince
         .
   END.
END PROCEDURE.

   /* ���� �������㠫쭮� �⠢�� �� ����筮�� ������� ������� */
PROCEDURE pGetBefDepRate.
   DEFINE INPUT  PARAMETER iContract AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER oRate     AS DEC  NO-UNDO.

   DEF VAR vNumDays  AS INT64  NO-UNDO.
   DEF VAR vProdCode AS CHAR NO-UNDO.

   DEF BUFFER loan     FOR loan.
   DEF BUFFER pro-obl  FOR pro-obl.
   DEF BUFFER ttRate   FOR ttRate.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:
      FIND FIRST loan WHERE
                 loan.contract  EQ iContract
             AND loan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         LEAVE MAIN.
      RUN GetDepDosr (BUFFER loan) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         LEAVE MAIN.
         /* �᫨ ४����� ���, � ������塞 � indicate */
      IF NOT CAN-FIND(FIRST ttRate) THEN
         RUN FillIntoIndicate (BUFFER loan).
      FOR EACH pro-obl WHERE
               pro-obl.contract  EQ loan.contract
           AND pro-obl.cont-code EQ loan.cont-code
           AND pro-obl.idnt      EQ 3
      NO-LOCK BY pro-obl.pr-date DESC:
         LEAVE.
      END.
      IF AVAIL pro-obl THEN
         vNumDays = iDate - pro-obl.pr-date.
      ELSE
         vNumDays = iDate - loan.open-date.
      FIND FIRST ttRate WHERE
              ttRate.firstday LE vNumDays
          AND ttRate.lastday  GE vNumDays
      NO-LOCK NO-ERROR.
      IF AVAIL ttRate THEN
         oRate = ttRate.rate.
   END.
END PROCEDURE.

/* ������ ��ࠬ��஢ ������� �� ������ ��� ������� �������,   */
/* ilist-ost - ��ࠬ���� ����� �� �㦭� �������஢��� */
/* oIsValidClose - 䫠� ���������� ������� */
/* oSumm - �㬬� �� �㫥���� ��ࠬ��� */
/* oParam - ����� �� �㫥���� ��ࠬ��� */
PROCEDURE ValidCloseLoan.
   DEFINE INPUT   PARAMETER iContract     AS CHAR NO-UNDO.
   DEFINE INPUT   PARAMETER iContCode     AS CHAR NO-UNDO.
   DEFINE INPUT   PARAMETER ilist-ost     AS CHAR NO-UNDO.   
   DEFINE OUTPUT  PARAMETER oIsValidClose AS LOGICAL NO-UNDO.
   DEFINE OUTPUT  PARAMETER oSumm         AS DECIMAL NO-UNDO.
   DEFINE OUTPUT  PARAMETER oParam        AS INT64   NO-UNDO.
   
   
   DEFINE VAR list-ost  AS CHAR NO-UNDO.
   DEFINE VAR vPrmCurr  AS CHAR NO-UNDO.
   DEFINE VAR vNachSumm AS DEC  NO-UNDO.
   
   DEFINE VAR summ   LIKE loan-var.balance.
   
   FIND FIRST loan WHERE 
              loan.contract EQ iContract
          AND loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.
   
   oIsValidClose = YES.
   
   FOR EACH loan-par WHERE loan-par.amt-id < 1000 NO-LOCK:

      IF loan-par.amt-id = 1 OR
         loan-par.amt-id = 3 OR
         loan-par.amt-id = 5
      THEN
         NEXT.

       summ = 0.

      /* ����ࠡ��뢠��� ��ࠬ���� */
      IF CAN-DO(ilist-ost, STRING(loan-par.amt-id)) THEN 
      DO:
         NEXT.
      END.

      IF mass[loan-par.amt-id + 1] NE 0 THEN
         summ = loan.interest[mass[loan-par.amt-id + 1]].

      

      FIND LAST loan-var
         {wh-t &f=loan-var &i=loan-par.amt-id}
      NO-LOCK NO-ERROR.

      IF     AVAIL loan-var
         AND loan-var.balance NE 0
      THEN
         summ = summ + loan-var.balance.
   
      
      IF AVAIL loan-var THEN
         vPrmCurr = loan-var.currency.
      ELSE
         vPrmCurr = "".

      vNachSumm = 0.
      
      RUN CalcPrmValue (loan.contract,
                        loan.cont-code,
                        loan-par.amt-id,
                        summ,
                        vPrmCurr,
                        OUTPUT vNachSumm).

      IF vNachSumm NE ? THEN
          ASSIGN
             summ = vNachSumm 
          .
      
      

      
      IF summ NE 0 THEN
      DO:
         oIsValidClose = NO.
         oSumm = summ.
         oParam = loan-par.amt-id.
         
         RETURN.
      END.      
   END.
END.



PROCEDURE CloseLoan.
   DEFINE INPUT  PARAMETER iContract AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iContCode AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER iWhat     AS INT64  NO-UNDO.

   DEFINE VAR mess      AS CHAR    FORMAT "x(64)".
   DEFINE VAR mess1     AS CHAR    FORMAT "x(20)".
   DEFINE VAR fl-o      AS INT64.
   DEFINE VAR fl-er     AS LOG  INIT YES.
   DEFINE VAR i         AS INT64.
   DEFINE VAR mPayDate  AS DATE NO-UNDO.
   DEFINE VAR vDateN    AS DATE NO-UNDO.
   DEFINE VAR kk        AS INT64  INIT 0.
   DEFINE VAR list-ost  AS CHAR NO-UNDO.
   DEFINE VAR vPrmCurr  AS CHAR NO-UNDO.
   DEFINE VAR vNachSumm AS DEC  NO-UNDO.
   DEFINE VAR j         AS INT64     INIT 2.
   DEFINE VAR i1        AS INT64.
   DEFINE VAR j1        AS INT64.
   DEFINE VAR fl-undo   AS LOG  INIT NO.    /* yes - �����㦥�� �訡��, �⪠�  */
   DEFINE VAR fl1       AS LOG.
   DEFINE VAR fl-er1    AS LOG  INIT YES.
   DEFINE VAR vDbSumDec AS DEC  NO-UNDO.
   DEFINE VAR vCrSumDec AS DEC  NO-UNDO.
   DEFINE VAR vPar      AS CHAR NO-UNDO INIT '33,29,10'.
   DEFINE VAR vSumm     AS DEC  NO-UNDO.
   DEFINE VAR list_type    AS CHAR NO-UNDO. /* ᯨ᮪ ஫�� ��⮢ ��� ��⮬���.
                                         ** ������� �� �����⨨ ������� */
   DEFINE VAR was_noerr    AS LOG  NO-UNDO. /*yes - �ᯥ譮� �����⨨ ��⮢ */
   DEFINE VAR isValidCloseLoan AS LOG  NO-UNDO. /* ����� �� ��ࠬ���� */
   DEFINE VAR vamt-id AS INT64 NO-UNDO.

   DEFINE VAR summ   LIKE loan-var.balance.
   DEFINE VAR e1     LIKE loan-var.balance.
   DEFINE VAR e2     LIKE loan-var.balance.
   DEFINE VAR e3     LIKE loan-var.balance.
   DEFINE VAR summ-t LIKE term-obl.amt     COLUMN-LABEL "����������� �������".

   DEFINE VARIABLE vPutStrem  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vDispMess  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vDispMess2 AS LOGICAL     NO-UNDO.

   DEFINE BUFFER xerm-obl FOR term-obl.
   DEFINE BUFFER loan     FOR loan. /* ���������� ����. */

   &GLOB iskldbparam "95"

   FIND FIRST loan WHERE loan.contract  EQ iContract
                     AND loan.cont-code EQ iContCode
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF NOT AVAILABLE loan THEN
   DO:
      IF LOCKED loan THEN
           RUN Fill-SysMes IN h_tmess("","","-1","������� �������஢�� ��㣨� ���짮��⥫��").
      ELSE RUN Fill-SysMes IN h_tmess("","","-1","������� �� ������").
      RETURN.
   END.

   IF AVAIL loan
   THEN DO:

      list-ost  = FGetSetting("�᪫�������",?,"") + ",19,36".
      list_type = GetXAttrInit(loan.class-code,"list_type").
      ASSIGN
         vPutStrem  = LOOKUP("PutStrem" ,SOURCE-PROCEDURE:INTERNAL-ENTRIES) > 0
         vDispMess  = LOOKUP("DispMess" ,SOURCE-PROCEDURE:INTERNAL-ENTRIES) > 0
         vDispMess2 = LOOKUP("DispMess2",SOURCE-PROCEDURE:INTERNAL-ENTRIES) > 0
      .

      IF vPutStrem
      THEN
         RUN PutStrem IN SOURCE-PROCEDURE
            (FILL (" ",29) + "~n~n�������� ��������  " + STRING(LOAN.CONT-CODE) + "~n~n" +
             FILL (" ",29) + "����饭�� �� �訡���:" + "~n") NO-ERROR.

      LOANCLOSE:
      DO
      ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

         IF loan.end-date GT iDate THEN
         DO:
            {f-error.i &CLASS   = �����
                       &par     = 1l
                       &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
            }
         END.
         IF     iWhat NE 1
            AND DATE(iDate + 1) LT loan.since THEN /* ������� ����� ���� �����⠭ �� 1 ���� ���। */
         DO:
            {f-error.i &class   = �����
                       &par     = 2l
                       &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
            }
         END.


         IF loan.since LE iDate THEN
         DO:

            {loancalc.i
               &incontr = "loan.contract"
               &incontc = "loan.cont-code"
               &dr      = "iDate + 1"
            }

            IF loan.since <> iDate + 1 THEN
            DO:
               IF vPutStrem
               THEN
                  RUN PutStrem IN SOURCE-PROCEDURE
                     ("�訡�� �� ����⪥ �������� �������.").
               RETURN.
            END.
         END.


                  /* ���� ��᫥���� ����樨 �� �������� */
         FIND LAST loan-int USE-INDEX prim
              {wh-t &f=loan-int &c="/*"}
         NO-LOCK NO-ERROR.

         IF     AVAIL loan-int
            AND loan-int.mdate GT loan.since
         THEN DO:
            {f-error.i
               &class   = �����
               &par     = 3l
               &params  = "(loan.contract,loan.cont-code,loan-int.mdate + 1)"
               &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
            }
         END.

         i = 0.
                     /* ��।��塞 ���� ������ � ��砫쭮�� �襭�� �� ������ࠬ */
         ASSIGN
            mPayDate = loan.since
            vDateN   = DATE(FGetSettingEx("��⠍��।", ?, "", NO))
            vDateN   = IF vDateN EQ ? THEN DATE(1,1,1900) ELSE vDateN
         .

         RUN ValidCloseLoan(loan.contract,
                            loan.cont-code,
                            FGetSetting("�᪫�������",?,"") + ",19,36",
                            OUTPUT isValidCloseLoan,
                            OUTPUT summ,
                            OUTPUT vamt-id  ).

         IF NOT isValidCloseLoan THEN
         DO:
               IF fl-er THEN
               DO:
                  mess = "������: ���㫥�� ���祭�� ��ࠬ��஢ ������஢".
                  IF vDispMess
                  THEN
                     RUN DispMess IN SOURCE-PROCEDURE (mess).

                  mess = "��������� ��������.".

                  IF vPutStrem
                  THEN
                     RUN PutStrem IN SOURCE-PROCEDURE
                        ( FILL(" ",29) + mess +  "~n"
                        + FILL(" ",3)  + "���  ������������ ��ࠬ���"
                        + FILL(" ",19) + "���⮪" + "~n").
                  fl-er = NO.
               END.

               {f-error.i &class   = �����
                          &par     = 4l
                          &params  = "(vamt-id,summ)"
                          &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
               }
         END.
         

         fl-er = YES.

         FOR EACH term-obl WHERE term-obl.contract  EQ loan.contract
                             AND term-obl.cont-code EQ loan.cont-code
                             AND term-obl.idnt      EQ 3
                             AND term-obl.sop-date  EQ ?
         EXCLUSIVE-LOCK:

            {summ-t.i}

            IF fl-er THEN
            DO:
               mess = "�।�०�����: ���� ��������� ���� ��易⥫��⢠".
               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess) .
               fl-er = NO.
            END.

            IF summ-t EQ 0 THEN
            DO:
               ASSIGN
                  term-obl.sop-date = iDate
                  mess = "������ ��筮� ��易⥫��⢮ � �������� ��⮩ : " + string(term-obl.end-date).
               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess).
            END.
            ELSE
            DO:
               fl-Undo = YES. /* ���� ��� �⪠� ����室��� �������� */
               IF fl-er1 THEN
               DO:
                  mess = "�訡��: ���� ���� ��易⥫��⢠ � ���㫥�� ���⪮�".
                  IF vDispMess
                  THEN
                     RUN DispMess IN SOURCE-PROCEDURE (mess).

                  mess = "������� ������������� � ��������� �������� " + ":".
                  IF vPutStrem
                  THEN
                     RUN PutStrem IN SOURCE-PROCEDURE
                        ( FILL(" ",19) + mess + "~n"
                        +                " �������� ���"
                        + FILL(" ",11) + "�㬬�"
                        + FILL(" ",9)  + "�����᪨� ���⮪").
                  fl-er1 = NO.
               END.

               {f-error.i &class   = �����
                          &par     = 5l
                          &params  = "(recid(term-obl),summ-t)"
                          &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
               }
            END.
         END.

         ASSIGN
            fl-er  = YES
            fl-er1 = YES
         .

         FOR EACH term-obl WHERE term-obl.contract  EQ loan.contract
                             AND term-obl.cont-code EQ loan.cont-code
                             AND term-obl.idnt      EQ 1
                             AND term-obl.sop-date  EQ ?
         EXCLUSIVE-LOCK:

            {summ-t1.i}

            IF fl-er THEN
            DO:
               mess = "��������������: ���� ��������� ��易⥫��⢠ �� ����襭�� ��業⮢".
               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess) .
               fl-er = NO.
            END.
            IF summ-t EQ 0 THEN
            DO:
               ASSIGN
                  term-obl.sop-date  = iDate
                  mess = "������ ��易⥫��⢮ � �������� ��⮩ : " + string(term-obl.end-date).

               IF vDispMess
               THEN
                  RUN DispMess IN SOURCE-PROCEDURE (mess).
            END.
            ELSE
            DO:
               fl-undo = YES.  /* ���� ��� �⪠� ����室��� �������� */
               IF fl-er1 THEN
               DO:
                  ASSIGN
                     mess = "�訡��: ���� ��易⥫��⢠ �� ����襭�� ��業⮢ "
                     mess1 = "� ���㫥�� ���⪮�".

                  IF vDispMess2
                  THEN
                     RUN DispMess2 IN SOURCE-PROCEDURE (mess,mess1) .

                  mess = "������������� �� ��������� ��������� � ��������� ��������  " + ":".
                  IF vPutStrem
                  THEN
                     RUN PutStrem IN SOURCE-PROCEDURE
                        ( FILL(" ",14) + mess + "~n"
                        +                " �������� ���"
                        + FILL(" ",11) + "�㬬�"
                        + FILL(" ",9)  + "�����᪨� ���⮪") .

                  fl-er1 = NO.
               END.

               {f-error.i &class   = �����
                          &par     = 5l
                          &params  = "(recid(term-obl),summ-t)"
                          &RunProc = "IF vDispMess THEN RUN DispMess IN SOURCE-PROCEDURE (mess). "
               }

            END.
         END.

         /* �����⨥ ��⮢ (�� �᫮���, �� �ᯥ譮 ��諮 ��⠫쭮�) */
         IF fl-o = 0 THEN
         DO:
            RUN close_acct.p(RECID(loan),
                             iDate,
                             OUTPUT was_noerr,
                             OUTPUT mess).
            IF vPutStrem
            THEN
               RUN PutStrem IN SOURCE-PROCEDURE (mess) .
            IF NOT was_noerr THEN
            DO:

               fl-o = fl-o + 1.
               UNDO LOANCLOSE, LEAVE LOANCLOSE.
            END.
         END.

         IF     NOT  fl-undo
            AND     fl-o = 0 THEN
            ASSIGN
               mess             = "~n������� ������ "
               loan.close-date  = iDate
               loan.loan-status = "����".
         ELSE
         DO:
            ASSIGN
               mess            = "~n������� �� ������"
               mess1           = "�⪠� ��� ���������"
            .
            IF vPutStrem
            THEN
               RUN PutStrem IN SOURCE-PROCEDURE (mess + "~n" + mess1).
            UNDO LOANCLOSE, LEAVE LOANCLOSE.
         END.
      END.
      IF vPutStrem
      THEN
         RUN PutStrem IN SOURCE-PROCEDURE (mess).


   END.

END PROCEDURE.

/* �㭪�� �����頥� ��⨭�, �᫨ ��।����� ��� �室�� � �죮�� ��ਮ� ������� (��� �஡���) */
FUNCTION fLgPeriod RETURN LOGICAL (
   iContract AS CHARACTER,
   iContCode AS CHARACTER,
   iDate     AS DATE
):
   DEF VAR oResult   AS LOG    NO-UNDO.
   DEF VAR vLgCnt    AS INT64    NO-UNDO.   /* ���-�� �죮��� ��ਮ��� */
   DEF VAR vi        AS INT64    NO-UNDO.

   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bterm-obl  FOR term-obl.

   MAIN_BLOCK:
   DO:
      RUN RE_L_COND (iContract,iContCode,iDate,BUFFER bloan-cond).
      IF NOT AVAIL bloan-cond THEN LEAVE MAIN_BLOCK.

            /* ������� ������⢮ �죮��� ��ਮ��� �� %%, �������� �� ⥪�饬 �᫮��� */
      vLgCnt = INT64(GetXAttrValueEx("loan-cond",
                                   bloan-cond.contract    + "," +
                                   bloan-cond.cont-code   + "," +
                                   STRING(bloan-cond.since),
                                   "�����⏥����",
                                   "0")).

            /*���� ��ࢮ�� �� �죮⭮�� ���⥦�*/
      FOR EACH bterm-obl WHERE bterm-obl.contract  EQ bloan-cond.contract
                           AND bterm-obl.cont-code EQ bloan-cond.cont-code
                           AND bterm-obl.idnt      EQ 1
                           AND bterm-obl.nn        EQ 1
      NO-LOCK
      vi = 1 TO vLgCnt:
      END.
      IF NOT AVAIL bterm-obl THEN LEAVE MAIN_BLOCK.

            /* �ࠢ������ ���� ���⥦� (term-obl.end-date) � ��।����� ��⮩ */
      IF iDate LT bterm-obl.end-date THEN oResult = YES. /* ��।����� ��� ��室���� ����� �죮⭮�� ��ਮ�� �� �������� */
   END.
   RETURN oResult.
END FUNCTION.
/* ��楤�� ���� ���ᮢ�� ࠧ���              */
FUNCTION fNVPIKurs RETURN DECIMAL (
   iContract AS CHARACTER,     /* ⨯ ������� */
   iContCode AS CHARACTER,     /* ��� ������� */
   iDate     AS DATE,          /* ��� ����樨 */
   iCodPar   AS CHARACTER,     /* ��ࠬ��� ������� */
   iSumm     AS DECIMAL,       /* �㬬� ����襭��   */
   iSign     AS CHARACTER      /* ��� ��� ��� ����� ���ᮢ�� ࠧ���� ��⠥�*/
   ) :

DEF VAR oResult   AS DECIMAL    NO-UNDO.
DEF VAR vSumProv  AS DECIMAL NO-UNDO .

DEF BUFFER b-loan   FOR loan.

      IF iSumm EQ 0 THEN DO:
         oResult = 0 .
         RETURN oResult.
      END.
      FIND FIRST b-loan WHERE b-loan.contract   =  iContract
                          AND b-loan.cont-code  =  iContCode
                          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE b-loan
      THEN DO:
         oResult = 0 .
         RETURN oResult.
      END.
      IF b-loan.currency = ""   /* � �㡫��묨 ������ﬨ ���ᮢ�� ࠧ��� �� ��������� */
      THEN DO:
         oResult = 0 .
          RETURN oResult.
      END.

   /* ����� ���ᮢ�� ࠧ���� */
      RUN PROC_NVPIKurs IN THIS-PROCEDURE (
         INPUT  iContract ,
         INPUT  iContCode ,
         INPUT  iDate     ,
         INPUT  iCodPar   ,
         INPUT  iSumm     ,
         INPUT  b-loan.currency,
         INPUT  iSign     ,
         OUTPUT vSumProv  ).

       oResult = 0.
       IF iSign EQ "���"  AND
          vSumProv > 0
       THEN
         ASSIGN
            oResult = vSumProv
         .

       IF iSign NE "���"  AND
          vSumProv < 0
       THEN
         ASSIGN
            oResult = ABS(vSumProv)
         .

   RETURN oResult.
END FUNCTION.

/* �ᯮ����⥫�� ⠡���� ��� ��楤��� PROC_NVPIKurs*/
DEFINE TEMP-TABLE tt-loan-int NO-UNDO LIKE loan-int
FIELD Kurs AS DECIMAL
FIELD Id AS INTEGER
.
DEFINE TEMP-TABLE tt-nach NO-UNDO LIKE loan-int
FIELD Kurs AS DECIMAL
FIELD Id AS INTEGER
.

/* ��楤�� ���� ���ᮢ�� ࠧ���   */
PROCEDURE PROC_NVPIKurs :
DEFINE INPUT PARAMETER    iContract AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iContCode AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iDate     AS DATE NO-UNDO      .
DEFINE INPUT PARAMETER    iCodPar   AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iSumm     AS DECIMAL  NO-UNDO  .
DEFINE INPUT PARAMETER    iCurrency AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER    iSign     AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER   oSumProv  AS DECIMAL NO-UNDO   .

DEF VAR loan_ost   AS DECIMAL NO-UNDO .
DEF VAR loan_cr    AS DECIMAL NO-UNDO .
DEF VAR loan_db    AS DECIMAL NO-UNDO .
DEF VAR vFirst     AS LOGICAL NO-UNDO .
DEF VAR vFlagDel   AS LOGICAL NO-UNDO .
DEF VAR vSummDelta AS DECIMAL NO-UNDO .
DEF VAR vNN        AS INT64 NO-UNDO .

DEF BUFFER loan-int FOR loan-int.

EMPTY TEMP-TABLE tt-loan-int.
EMPTY TEMP-TABLE tt-nach.

   /* ������ ᯨ᮪ ����権 � ��ࢮ� ������襭��� */
   vFirst = FALSE .
   cycle_loan_int:
   FOR EACH loan-int WHERE
            loan-int.contract    = iContract
         AND loan-int.cont-code  = iContCode
         AND loan-int.mdate     <= iDate
         AND ( loan-int.id-d     = INT64(iCodPar)
         OR    loan-int.id-k     = INT64(iCodPar))
         NO-LOCK
            BY loan-int.mdate DESC
            BY loan-int.nn    DESC:
      /* ��।���� ���⮪ �� �ࠬ��� ��室�騩 �� ����樨 */
      RUN STNDRT_PARAM (icontract,
                        icontcode,
                        INT64(iCodPar),
                        loan-int.mdate,
                        OUTPUT loan_ost,
                        OUTPUT loan_cr ,
                        OUTPUT loan_db
                        ).
      IF loan-int.id-d = INT64(iCodPar) THEN DO:
         IF loan_ost  - loan-int.amt-rub  > 0  /* �室�騩 ���⮪ �� ��ࠬ��� �� ����樨 */
         THEN DO:
            CREATE tt-loan-int.
            BUFFER-COPY loan-int TO tt-loan-int
            ASSIGN
               tt-loan-int.kurs = FindRateWork ("�������",iCurrency,loan-int.mdate)
            .
         END.
         ELSE DO:
            IF vFirst = FALSE THEN DO:
               CREATE tt-loan-int.
               BUFFER-COPY loan-int TO tt-loan-int
               ASSIGN
                  tt-loan-int.kurs = FindRateWork ("�������",iCurrency,loan-int.mdate)
               .
               vFirst = TRUE .
            END.
            LEAVE cycle_loan_int.
         END.
      END.
      ELSE DO:
         /* 㯫��� */
         CREATE tt-loan-int.
         BUFFER-COPY loan-int TO tt-loan-int
         ASSIGN
            tt-loan-int.kurs = FindRateWork ("�������",iCurrency,loan-int.mdate)
         .
      END.
   END.

   /* ��७㬥�㥬 �� ���浪� � ���祬 �, �� �� ��������� */
   vNN = 0 .
   vFlagDel = FALSE.



M1:
   FOR EACH tt-loan-int :
     IF vFlagDel = TRUE
     THEN DO:
       DELETE tt-loan-int.
     END.
     ELSE DO:
         vNN = vNN + 1.
         tt-loan-int.id = vNN.
         /* ����襭�� �� ���थ�� ������ ���� �� ��।�����  �㬬� iSumm*/
         IF tt-loan-int.id-k   =  INT64(iCodPar) AND
            tt-loan-int.mdate  =  iDate
         THEN DO:
            iSumm = iSumm - tt-loan-int.amt-rub.

            IF iSumm = 0
            THEN DO:
               ASSIGN
                  vFlagDel = TRUE
               .
               NEXT M1.
            END.
            IF iSumm < 0
            THEN DO:
               ASSIGN
                  vFlagDel = TRUE
                  iSumm  = 0
                  tt-loan-int.amt-rub = ABS(iSumm)
                  tt-loan-int.mdate   = iDate
                  .
               NEXT M1.
            END.
         END.
     END.
   END. /* FOR EACH m1 */

   IF iSumm > 0
   THEN DO:
      vNN = vNN + 1.
      CREATE tt-loan-int.
      ASSIGN
         tt-loan-int.Contract  = iContract
         tt-loan-int.Cont-code = iContCode
         tt-loan-int.id-d    = ?
         tt-loan-int.id-k    = INT64(iCodPar)
         tt-loan-int.amt-rub = iSumm
         tt-loan-int.mdate   = iDate
         tt-loan-int.id      = vNN
         tt-loan-int.nn      = vNN
          .
         tt-loan-int.Kurs    = FindRateWork ("�������",iCurrency,iDate)
      .
   END.


   oSumProv = 0.
   FOR EACH tt-loan-int
         BY tt-loan-int.id
          :
       IF tt-loan-int.id-d   = INT64(iCodPar) /* ���᫥��� */
       THEN DO:
         CREATE tt-nach.
         BUFFER-COPY tt-loan-int TO tt-nach.
       END.
       ELSE DO: /* ����� */
         FOR EACH  tt-nach WHERE
                   tt-nach.amt-rub > 0 AND
                   tt-nach.id < tt-loan-int.id
                   :
            IF TT-nach.amt-rub <= TT-loan-int.amt-rub
            THEN
               ASSIGN
                   vSummDelta = TT-nach.amt-rub
               .
            ELSE
               ASSIGN
                  vSummDelta = TT-loan-int.amt-rub
               .

            /* �᫨ ��⠥� ������⥫��� ��: */
            IF iSign EQ "���"  AND
             ( TT-loan-int.kurs - tt-nach.kurs ) > 0
            THEN
               oSumProv = oSumProv + ( vSummDelta * (TT-loan-int.kurs - tt-nach.kurs)) .

            /* �᫨ ��⠥� ����⥫��� ��: */
            IF iSign EQ "���"  AND
             ( TT-loan-int.kurs - tt-nach.kurs ) < 0
            THEN
               oSumProv = oSumProv + ( vSummDelta * (TT-loan-int.kurs - tt-nach.kurs)) .


            IF TT-nach.amt-rub <= TT-loan-int.amt-rub
            THEN
               ASSIGN
                  TT-loan-int.amt-rub  = TT-loan-int.amt-rub - vSummDelta
                  TT-nach.amt-rub = 0
               .
            ELSE
               ASSIGN
                  TT-nach.amt-rub     = TT-nach.amt-rub     - vSummDelta
                  TT-loan-int.amt-rub = TT-loan-int.amt-rub - vSummDelta
               .
         END. /* FOR EACH  �� ���᫥��� */
       END.
   END.  /* FOR EACH tt-loan-int �� ������ */
END PROCEDURE.

PROCEDURE pTranshFill:
   DEF INPUT  PARAM iOp          AS INT64 NO-UNDO. /* ��� ���㬥�� */
   DEF INPUT  PARAM iOpEntry     AS INT64 NO-UNDO. /* ��� �஢���� */
   DEF INPUT  PARAM iSide        AS LOG   NO-UNDO. /* ��/�� */
   DEF INPUT  PARAM iTranshOnly  AS LOG   NO-UNDO. /* ���쪮 ��� �࠭襩 */
   DEF INPUT  PARAM iRole        AS CHAR  NO-UNDO. /* ���� ��� */
   DEF INPUT  PARAM iContract    AS CHAR  NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode    AS CHAR  NO-UNDO. /* ��� ������� */

   DEF VAR vAcct  AS CHAR  NO-UNDO.
   DEF VAR vCurr  AS CHAR  NO-UNDO.
   DEF VAR vDate  AS DATE  NO-UNDO.

   DEF BUFFER op-entry     FOR op-entry.
   DEF BUFFER loan-acct    FOR loan-acct.
   DEF BUFFER bloan-acct   FOR loan-acct.
   DEF BUFFER tt-Transh    FOR tt-Transh.

   MAIN:
   DO:
      FIND FIRST op-entry WHERE
                 op-entry.op       EQ iOp
             AND op-entry.op-entry EQ iOpEntry
      NO-LOCK NO-ERROR.
      IF NOT AVAIL op-entry THEN
         LEAVE MAIN.
      RUN pTranshClear (iOp, iOpEntry).
      ASSIGN
         vAcct       = IF iSide THEN op-entry.acct-db ELSE op-entry.acct-cr 
         vCurr       = op-entry.currency
         vDate       = op-entry.op-date
         iTranshOnly = IF iTranshOnly EQ ? THEN YES ELSE iTranshOnly                
         iRole       = IF NOT {assigned iRole} THEN "*" ELSE iRole                
         iContract   = IF iContract EQ ? THEN "" ELSE iContract
         iContCode   = IF iContCode EQ ? THEN "" ELSE iContCode
      .
      FOR EACH loan-acct WHERE
         (
           (   iContract           EQ ""
           AND iContCode           EQ ""
           AND loan-acct.acct      EQ vAcct
           AND loan-acct.currency  EQ vCurr
           )
         OR
           (   iContract           NE ""
           AND iContCode           EQ ""
           AND loan-acct.acct      EQ vAcct
           AND loan-acct.currency  EQ vCurr
           AND loan-acct.contract  EQ iContract
           )
         OR
           (   iContract           NE ""
           AND iContCode           NE ""
           AND loan-acct.acct      EQ vAcct
           AND loan-acct.currency  EQ vCurr
           AND loan-acct.contract  EQ iContract
           AND (   loan-acct.cont-code BEGINS iContCode + " "
               OR (NOT iTranshOnly
                   AND loan-acct.cont-code EQ iContCode
                   )
               )
           )
         )
         AND CAN-DO (iRole, loan-acct.acct-type)
         AND loan-acct.since LE vDate
         AND ( NOT iTranshOnly
            OR NUM-ENTRIES (loan-acct.cont-code, " ") GT 1
             )
         AND NOT CAN-FIND(FIRST bloan-acct WHERE
                                bloan-acct.contract  EQ loan-acct.contract
                            AND bloan-acct.cont-code EQ loan-acct.cont-code
                            AND bloan-acct.acct-type EQ loan-acct.acct-type
                            AND bloan-acct.since     GT loan-acct.since
                            AND bloan-acct.since     LE vDate
                          )
      NO-LOCK,
         FIRST loan WHERE
               loan.contract   EQ loan-acct.contract
           AND loan.cont-code  EQ loan-acct.cont-code
           AND loan.close-date EQ ?
           AND loan.open-date  LE vDate
      NO-LOCK:
          CREATE tt-Transh.
         ASSIGN
            tt-Transh.op        = op-entry.op
            tt-Transh.op-entry  = op-entry.op-entry
            tt-Transh.contract  = loan-acct.contract
            tt-Transh.cont-code = loan-acct.cont-code
            tt-Transh.acct-type = loan-acct.acct-type
            tt-Transh.currency  = loan-acct.currency
            tt-Transh.since     = loan-acct.since
            tt-Transh.open-date = loan.open-date
            tt-Transh.end-date  = loan.end-date
            tt-Transh.Side      = iSide
         .
      END.
   END.
END PROCEDURE.

PROCEDURE pTranshCheck:
   DEF INPUT  PARAM iOp          AS INT64 NO-UNDO. /* ��� ���㬥�� */
   DEF INPUT  PARAM iOpEntry     AS INT64 NO-UNDO. /* ��� �஢���� */
   DEF INPUT  PARAM iCheckLoan   AS LOG   NO-UNDO. /* �஢�ઠ �� 㭨���쭮��� ������� � �롮થ */
   DEF INPUT  PARAM iCheckRole   AS LOG   NO-UNDO. /* �஢�ઠ �� 㭨���쭮��� ૨ ��� � �롮થ */
   DEF OUTPUT PARAM oResult      AS LOG   NO-UNDO.

   DEF BUFFER tt-Transh    FOR tt-Transh.
   DEF BUFFER btt-Transh   FOR tt-Transh.
   DEF BUFFER loan         FOR loan.
   DEF BUFFER bloan        FOR loan.

   oResult = YES.
   IF iCheckLoan AND oResult THEN
         /* �஢�ઠ ������ � �롮થ �࠭襩 ࠧ��� ������஢ */
      FOR FIRST tt-Transh WHERE
                tt-Transh.op       EQ iOp
            AND tt-Transh.op-entry EQ iOpEntry,
          FIRST loan WHERE
                loan.contract  EQ tt-Transh.contract
            AND loan.cont-code EQ ENTRY (1, tt-Transh.cont-code, " ")
          NO-LOCK,
          FIRST btt-Transh WHERE
                btt-Transh.op       EQ iOp
            AND btt-Transh.op-entry EQ iOpEntry
            AND CAN-FIND (FIRST bloan WHERE
                                bloan.contract  EQ btt-Transh.contract
                            AND bloan.cont-code EQ ENTRY (1, btt-Transh.cont-code, " ")
                            AND bloan.cont-code NE loan.cont-code
                          NO-LOCK
                         )
      :
         oResult = NO.
      END.
   IF iCheckRole AND oResult THEN
         /* �஢�ઠ ������ � �롮થ �ਢ燐� � ࠧ�묨 ஫ﬨ */
      FOR FIRST tt-Transh WHERE
                tt-Transh.op       EQ iOp
            AND tt-Transh.op-entry EQ iOpEntry
            AND CAN-FIND (FIRST btt-Transh WHERE
                                btt-Transh.op        EQ iOp
                            AND btt-Transh.op-entry  EQ iOpEntry
                            AND btt-Transh.acct-type NE tt-Transh.acct-type
                         )
      :
         oResult = NO.
      END.
END PROCEDURE.

PROCEDURE pTranshPosting:
   DEF INPUT  PARAM iOp          AS INT64 NO-UNDO. /* ��� ���㬥�� */
   DEF INPUT  PARAM iOpEnrty     AS INT64 NO-UNDO. /* ��� �஢���� */

   DEF VAR vDate      AS DATE NO-UNDO.
   DEF VAR vSummOst   AS DEC  NO-UNDO. /* �㬬� �஢���� ����� �㬬� ࠧ��ᥭ��� ����権 */
   DEF VAR vSummTr    AS DEC  NO-UNDO. /* �㬬� �� �࠭�� */
   DEF VAR vSummOb    AS DEC  NO-UNDO. /* �㬬� �� ��易⥫���� */
   DEF VAR vSummTek   AS DEC  NO-UNDO. /* �㬬� ���⪠ */
   DEF VAR vDb        AS DEC  NO-UNDO. /* ��� ����᪠ RE_PARAM */
   DEF VAR vCr        AS DEC  NO-UNDO. /* ��� ����᪠ RE_PARAM */
   DEF VAR vSumm      AS DEC  NO-UNDO. /* �㬬� �஢���� */

   DEF BUFFER op           FOR op.
   DEF BUFFER op-entry     FOR op-entry.
   DEF BUFFER tt-Transh    FOR tt-Transh.
   DEF BUFFER loan-cond    FOR loan-cond.
   DEF BUFFER term-obl     FOR term-obl.
   DEF BUFFER loan-int     FOR loan-int. 
   MAIN:
   DO:
      FIND FIRST op-entry WHERE
              op-entry.op       EQ iOp
          AND op-entry.op-entry EQ iOpEnrty
      NO-LOCK NO-ERROR.
      IF NOT AVAIL op-entry THEN
         LEAVE MAIN.
      FIND FIRST op WHERE op.op = iOp NO-LOCK NO-ERROR. 
      IF NOT AVAIL op THEN LEAVE MAIN. 

      ASSIGN
         vDate    = ( IF op.contract-date  EQ ?  THEN op-entry.op-date ELSE op.contract-date)
         vSumm    = ( IF op-entry.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur)  
         vSummTek = vSumm
      .
         /* ���⠥� 㦥 ࠧ��ᥭ��� �㬬� */
      FOR EACH loan-int WHERE
               loan-int.op       EQ op-entry.op
           AND loan-int.op-entry EQ op-entry.op-entry
      NO-LOCK:
         vSummTek = vSummTek - loan-int.amt-rub.
      END.
      /* ��� ��� ����ᥩ �� �஢���� */
      TRANSH:
      FOR EACH tt-Transh WHERE
               tt-Transh.op       EQ op-entry.op
           AND tt-Transh.op-entry EQ op-entry.op-entry
      BY tt-Transh.open-date:
         /* ��室�� ⥪�饥 �᫮��� */
         FIND LAST loan-cond WHERE
                   loan-cond.contract  EQ tt-Transh.contract
               AND loan-cond.cont-code EQ tt-Transh.cont-code
               AND loan-cond.since     LE vDate
         NO-LOCK NO-ERROR.
         IF NOT AVAIL loan-cond THEN
            NEXT TRANSH.
         IF loan-cond.cred-period EQ "�" THEN
            vDate = tt-Transh.end-date.
         /* ��室�� ���������� ������ */
         FIND FIRST loan-int WHERE
                    loan-int.op        EQ tt-Transh.op
                AND loan-int.op-entry  EQ tt-Transh.op-entry
                AND loan-int.contract  EQ tt-Transh.contract
                AND loan-int.cont-code EQ tt-Transh.cont-code
         NO-LOCK NO-ERROR.
         IF AVAIL loan-int THEN
            ASSIGN
               tt-Transh.amt      = loan-int.amt-rub
               tt-Transh.loan-int = YES
         .
         CASE tt-Transh.acct-type:
            WHEN "�।��" THEN
            DO:
                  /* ����塞 �㬬� � ����襭�� ��� ��� ����権 
                  ** �� ⥪���� ���� */
               FIND LAST term-obl WHERE
                        term-obl.contract  EQ tt-Transh.contract
                    AND term-obl.cont-code EQ tt-Transh.cont-code
                    AND term-obl.idnt      EQ 3
                    AND term-obl.end-date  LE vDate
               NO-LOCK NO-ERROR.
               IF AVAIL term-obl THEN DO:
                  RUN summ-t.p (OUTPUT tt-Transh.amt-max,
                                tt-Transh.contract,
                                tt-Transh.cont-code,
                                RECID(term-obl),
                                vDate - 1
                                ).
               END.
               FOR EACH term-obl WHERE
                        term-obl.contract  EQ tt-Transh.contract
                    AND term-obl.cont-code EQ tt-Transh.cont-code
                    AND term-obl.idnt      EQ 3
                    AND term-obl.end-date  GT vDate
               NO-LOCK:

                  RUN summ-t.p (OUTPUT vSummOb,
                                tt-Transh.contract,
                                tt-Transh.cont-code,
                                RECID(term-obl),
                                vDate ).
                  ASSIGN
                     tt-Transh.amt-dos = tt-Transh.amt-dos + ( IF vSummTek GE vSummOb THEN vSummOb ELSE
                                                             ( IF vSummTek GT 0 THEN vSummTek ELSE 0))
                  .
                  vSummTek = vSummTek - vSummOb.
               END.
            END.
            WHEN "�।��" THEN
            DO:
               /* �㬬� � ����襭�� ����窨 */ 
               RUN RE_PARAM (7,
                             vDate,
                             tt-Transh.contract,
                             tt-Transh.cont-code,
                             OUTPUT tt-Transh.amt-max,
                             OUTPUT vDb,
                             OUTPUT vCr).
               IF tt-Transh.amt EQ 0 THEN
                  ASSIGN
                     tt-Transh.amt = MIN(vSummTek, tt-Transh.amt-max)
                  .
            END.
         END CASE.
      END. 
         /* �஢�ઠ, ����� ���� �㬬� 㦥 ࠧ��ᥭ�  */ 
      FIND FIRST tt-Transh WHERE tt-Transh.loan-int NO-LOCK NO-ERROR. 
      IF NOT AVAIL tt-Transh THEN
         /* ��᫥ ���������� tt-Transh - ࠧ������ �㬬� �� �࠭蠬  */ 
      FOR EACH tt-Transh NO-LOCK
      BY tt-Transh.end-date
      BY tt-Transh.open-date: 
         IF vSumm LE 0 THEN LEAVE. 
         ASSIGN
            tt-Transh.amt = ( IF vSumm > tt-Transh.amt-max THEN tt-Transh.amt-max ELSE vSumm)
            vSumm         = vSumm - tt-Transh.amt
         .
      END.
   END.
END PROCEDURE.

PROCEDURE pTranshCreateInt:
   DEF INPUT  PARAM iRole   AS CHAR NO-UNDO. /* ���᮪ ஫�� */
   DEF INPUT  PARAM iOper   AS CHAR NO-UNDO. /* ���᮪ ����権 */
   DEF OUTPUT PARAM oResult AS CHAR NO-UNDO. 

   DEF VAR vCounter  AS INT64     NO-UNDO.
   DEF VAR vKau      AS CHARACTER NO-UNDO. 
   DEF VAR vRow      AS ROWID     NO-UNDO.

   DEF BUFFER tt-Transh FOR tt-Transh.
   DEF BUFFER chowhe    FOR chowhe.
   DEF BUFFER op-entry  FOR op-entry.
   DEF BUFFER loan-int  FOR loan-int. 

   TRANSH:
   DO TRANS ON ERROR UNDO, RETRY:
      IF RETRY THEN
      DO:
         /* ������ �訡�� */
         ASSIGN 
            oResult = "ERROR:" + ERROR-STATUS:GET-MESSAGE(1) + " " + {&RETURN_VALUE}
            .  
         UNDO TRANSH, LEAVE TRANSH.
      END.
      FOR EACH tt-Transh WHERE tt-Transh.loan-int ON ERROR UNDO TRANSH, RETRY TRANSH:
         FIND FIRST loan-int WHERE
                    loan-int.op        EQ tt-Transh.op
                AND loan-int.op-entry  EQ tt-Transh.op-entry
                AND loan-int.contract  EQ tt-Transh.contract
                AND loan-int.cont-code EQ tt-Transh.cont-code
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE loan-int THEN
         DO:
            IF LOCKED loan-int THEN
                 RUN Fill-SysMes IN h_tmess("","","-1","������ �������஢��� ��㣨� ���짮��⥫��").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","������ �� �������").
            RETURN.
         END.

         IF AVAIL loan-int THEN
            DELETE loan-int.
         ELSE
            UNDO TRANSH, RETRY TRANSH.
      END.
      FOR EACH tt-Transh WHERE (tt-Transh.amt     GT 0
                             OR tt-Transh.amt-dos GT 0 )
      ON ERROR UNDO TRANSH, RETRY TRANSH:
         FIND FIRST op-entry WHERE
                    op-entry.op       EQ tt-Transh.op
                AND op-entry.op-entry EQ tt-Transh.op-entry
         NO-LOCK NO-ERROR.
         IF NOT AVAIL op-entry THEN
            UNDO TRANSH, RETRY TRANSH.
         vCounter = LOOKUP(tt-Transh.acct-type, iRole).
         IF vCounter EQ 0 OR vCounter GT NUM-ENTRIES(iOper) THEN
            UNDO TRANSH, RETRY TRANSH.
         FIND FIRST chowhe WHERE chowhe.id-op EQ INT64(ENTRY(vCounter, iOper)) NO-LOCK NO-ERROR.
         IF NOT AVAIL chowhe THEN
            UNDO TRANSH, RETRY TRANSH.
         IF tt-Transh.amt GT 0 THEN
            RUN Cr_LoanIntSimple IN h_lv (tt-Transh.contract,
                                    tt-Transh.cont-code,
                                    op-entry.op-date,
                                    tt-Transh.amt,
                                    chowhe.id-d,
                                    chowhe.id-k,
                                    NO,
                                    (BUFFER op-entry:HANDLE),
                                    OUTPUT vRow).
         /* ����筮� ����襭�� */
         IF tt-Transh.amt-dos GT 0 THEN
             RUN Cr_LoanIntSimple IN h_lv (tt-Transh.contract,
                                     tt-Transh.cont-code,
                                     op-entry.op-date,
                                     tt-Transh.amt-dos,
                                     chowhe.id-d,
                                     chowhe.id-k,
                                     NO,
                                     (BUFFER op-entry:HANDLE),
                                     OUTPUT vRow).
         FIND FIRST loan-int WHERE
                    loan-int.op        EQ tt-Transh.op
                AND loan-int.op-entry  EQ tt-Transh.op-entry
                AND loan-int.contract  EQ tt-Transh.contract
                AND loan-int.cont-code EQ tt-Transh.cont-code
         NO-LOCK NO-ERROR.
         IF NOT AVAIL loan-int THEN
            UNDO TRANSH, RETRY TRANSH.
         /* �������� �㡠����⨪� �� �஢���� */ 
         ASSIGN 
            vKau = tt-Transh.Contract + "," + 
                   tt-Transh.cont-code + "," + 
                   STRING(chowhe.id-op)       
            .
         IF    (tt-Transh.Side AND NOT {assigned Op-entry.Kau-db}) 
            OR (NOT tt-Transh.Side AND NOT {assigned Op-entry.Kau-cr}) 
            THEN DO:
            FIND CURRENT op-entry EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

            IF NOT AVAILABLE op-entry THEN
            DO:
               IF LOCKED op-entry THEN 
                    RUN Fill-SysMes IN h_tmess("","","-1","�஢���� �������஢��� ��㣨� ���짮��⥫��").
               ELSE RUN Fill-SysMes IN h_tmess("","","-1","�஢���� �� �������").
               RETURN.
            END.

            IF NOT AVAIL op-entry THEN
               UNDO TRANSH, RETRY TRANSH.
            IF tt-Transh.Side THEN 
               ASSIGN 
                  op-entry.kau-db = vKau NO-ERROR. 
            ELSE 
               ASSIGN 
                  op-entry.kau-cr = vKau NO-ERROR. 
            IF ERROR-STATUS:ERROR THEN
               UNDO TRANSH, RETRY TRANSH.
            RELEASE op-entry NO-ERROR. 
            IF ERROR-STATUS:ERROR THEN
               UNDO TRANSH, RETRY TRANSH.
         END.
         /* Loan-int ᮧ��� �ᯥ譮  */
         ASSIGN 
            tt-Transh.IsCrLoanInt = TRUE. 
      
      END.
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
    ����� ࠧ��᪨ �࠭襩 
------------------------------------------------------------------------------*/
PROCEDURE pTT-TranshToFile:
   DEF INPUT  PARAM iFileName     AS CHAR    NO-UNDO. 
   DEF INPUT  PARAM iIsCrLoanInt  AS LOGICAL NO-UNDO. 
   
   DEF VAR vI    AS INT64 NO-UNDO. 
   DEF VAR vSumm AS DEC   NO-UNDO.

   OUTPUT TO VALUE(iFileName).
   PUT UNFORMATTED 
      SPACE(15) 
      "�������� ���������� �������� �� �������" SKIP(1)
      .
   FORM 
      vI                   COLUMN-LABEL "N �/�"
                           FORMAT ">>>>9"
      tt-Transh.Cont-code  COLUMN-LABEL "����� �������"
                           FORMAT "x(20)"
      tt-Transh.end-date   COLUMN-LABEL "��� ����襭��"
                           FORMAT "99/99/9999"
      tt-Transh.Amt-max    COLUMN-LABEL "�㬬� � ����襭��"
                           FORMAT "->>>,>>>,>>>,>>9.99"
      tt-Transh.Amt        COLUMN-LABEL "�㬬� ����樨"
                           FORMAT "->>>,>>>,>>>,>>9.99"
      tt-Transh.amt-dos    COLUMN-LABEL "�㬬� ����.��襭��"
                           FORMAT "->>>,>>>,>>>,>>9.99"
      WITH FRAME list WIDTH 104 DOWN NO-BOX.
   FOR EACH tt-Transh WHERE
      ( IF iIsCrLoanInt = ? THEN TRUE ELSE tt-Transh.IsCrLoanInt = iIsCrLoanInt)
      NO-LOCK:
      ASSIGN 
         vI    = vI + 1
         vSumm = vSumm + tt-Transh.Amt + tt-Transh.amt-dos
         .
      DISPLAY 
         vI
         tt-Transh.Cont-code
         tt-Transh.end-date
         tt-Transh.Amt-max
         tt-Transh.Amt
         tt-Transh.amt-dos
         WITH FRAME list.
      DOWN WITH FRAME List. 
   END.
   DISPLAY 
      "�⮣�" @ tt-Transh.Cont-code 
      vSumm   @ tt-Transh.Amt
      WITH FRAME List. 
   DOWN WITH FRAME list.
   OUTPUT CLOSE.
END PROCEDURE. 

PROCEDURE pTranshGet:
   DEF OUTPUT PARAM TABLE FOR tt-Transh.
END PROCEDURE.

PROCEDURE pTranshPut:
   DEF INPUT PARAM TABLE FOR tt-Transh.
END PROCEDURE.

PROCEDURE pTranshClear:
   DEF INPUT  PARAM iOp       AS INT64 NO-UNDO. /* ��� ���㬥�� */
   DEF INPUT  PARAM iOpEntry  AS INT64 NO-UNDO. /* ��� �஢���� */

   FOR EACH tt-Transh WHERE
         (  iOp                  EQ ?
        AND iOpEntry             EQ ?
         )
      OR (  iOp                  NE ?
        AND iOpEntry             EQ ?
        AND tt-Transh.op         EQ iOp
         )
      OR (  iOp                  NE ?
        AND iOpEntry             NE ?
        AND tt-Transh.op         EQ iOp
        AND tt-Transh.op-entry   EQ iOpEntry
         )
   :
      DELETE tt-Transh.
   END.
END PROCEDURE.

/* ��楤�� ��� �ନ஢���� ����� ������� ���ᯥ祭�� */
/* �� �᭮�� ����� �।�⭮�� �������                   */

PROCEDURE GETLoanNumO :
DEFINE INPUT PARAMETER iCountract AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER iCounCode  AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER oNumCD    AS CHARACTER NO-UNDO .

DEF BUFFER bloan FOR loan.
DEFINE VARIABLE vSimbol AS CHARACTER NO-UNDO .
DEFINE VARIABLE vForm  AS CHARACTER NO-UNDO .
vSimbol = "-" .
vForm   = "&1-&2-&3" .

   FIND FIRST bloan WHERE
            bloan.contract  = iCountract AND
            bloan.cont-code = iCouncode
            NO-LOCK NO-ERROR .
   IF AVAILABLE bloan THEN DO:
      IF NUM-ENTRIES(bloan.doc-ref ,vSimbol) > 3
      THEN
         oNumCD   = substitute( vForm ,
                  ENTRY(1,bloan.doc-ref,vSimbol )
                  , ENTRY(2,bloan.doc-ref,vSimbol )
                  , ENTRY(3,bloan.doc-ref,vSimbol )
                  ) .
      ELSE
         oNumCD   = ENTRY(1,bloan.doc-ref,"@") .
   END.
END PROCEDURE. /* GETLoanNumO */


/* ��� �ନ஢���� ����� ���㬥�� �� 蠡���� . �஢���� ����稥 ⥣� � 蠡���� */
PROCEDURE AutoCodeNeed :
DEFINE INPUT  PARAMETER iClass AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER iTeg AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER oRes AS LOGICAL NO-UNDO .

DEFINE VARIABLE vTemplate AS CHARACTER NO-UNDO .
DEFINE VARIABLE vCounter AS CHARACTER NO-UNDO .

   oRes = FALSE .

   IF    GetXAttrInit (iClass, "AutoGenNum") EQ "��" THEN
   DO:
      RUN GetClassTemplatePars (iClass,
                                 OUTPUT vTemplate,
                                 OUTPUT vCounter).
      vTemplate = FGetSetting("����������",vTemplate,"")  .
      IF INDEX(vTemplate,iTeg) > 0
      THEN
         oRes = TRUE .
   END.

END PROCEDURE. /* AutoCodeNeed */

/* �� �।�⭮� ����� ��।���� �ப ����砭�� ����� */
PROCEDURE EndDateKreditLine :
DEFINE INPUT  PARAMETER  iContract AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iContCode AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iAcct     AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iCurr     AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER  iDateEnd  AS DATE      NO-UNDO .
DEFINE OUTPUT PARAMETER  oDateEnd  AS DATE      NO-UNDO .

DEF BUFFER tloan     FOR loan.
DEF BUFFER loan      FOR loan.
DEF BUFFER loan-acct FOR loan-acct.
DEF BUFFER limits    FOR limits.
DEF BUFFER acct      FOR acct.

DEFINE VARIABLE vAccType  AS CHARACTER NO-UNDO .
DEFINE VARIABLE vContcode AS CHARACTER NO-UNDO .
DEFINE VARIABLE vTYpeLim  AS CHARACTER NO-UNDO .

   ASSIGN
      vAccType  = ""
      vContcode = ""
      vTypeLim  = ""
      .

   /* �墠�뢠�騩 ������� */
   FIND FIRST  tloan WHERE
               tloan.contract  EQ iContract
         AND   tloan.cont-code EQ ENTRY(1,iContCode," ")
         NO-LOCK NO-ERROR .
   IF NOT AVAILABLE tloan THEN RETURN ERROR "�� ������ ������� " + iContract + " " + ENTRY(1,iContCode," ")  .

   IF iDateEnd EQ ? THEN iDateEnd = tloan.end-date.

   FOR EACH    loan WHERE
         &IF DEFINED(oracle) &THEN
                 loan.contract  EQ iContract
            AND  loan.cont-code MATCHES(ENTRY(1,  tloan.cont-code, " ") + "*")
            AND  NUM-ENTRIES( loan.cont-code, " ") LE 2
            USE-INDEX PRIMARY       
         &ELSE
              (loan.contract  EQ iContract
         AND   loan.cont-code EQ tloan.cont-code)
         OR
              (loan.contract  EQ iContract
         AND   loan.cont-code BEGINS tloan.cont-code + " " )
         &ENDIF
         NO-LOCK ,
         FIRST   loan-acct WHERE
                 loan-acct.acct      EQ iAcct
            AND  loan-acct.currency  EQ iCurr
            AND  loan-acct.contract  EQ loan.contract
            AND  loan-acct.cont-code EQ loan.cont-code
            NO-LOCK :
            vAccType  = loan-acct.acct-type.
            vContCode = loan-acct.cont-code.
            LEAVE.
   END.

   CASE vAccType:
      WHEN "�।���"
         THEN DO:
            vTypeLim  = "limit-l-distr".
         END.
      WHEN "�।�"
         THEN DO:
            vTypeLim  = "limit-l-debts".
         END.
   END CASE.

   /* ������ ������� �� �墠�뢠�饬� �������� */
   FOR EACH limits WHERE
            limits.file-name  EQ "loan"
        AND limits.surrogate  EQ iContract + "," + ENTRY(1,iContCode, " ")
        AND limits.Class-Code EQ vTypeLim
        AND limits.open-date  LT iDateEnd
        AND limits.quantity   EQ 0
   NO-LOCK BY limits.open-date DESCENDING:
      oDateEnd = limits.open-date.
      LEAVE.
   END.

   IF oDateEnd EQ ? THEN
      oDateEnd = DATE(GetTempXAttrValue("acct",iAcct + "," + iCurr,"dealenddate")) NO-ERROR.
   IF oDateEnd EQ ? THEN
      oDateEnd = iDateEnd.

END PROCEDURE. /* EndDateKreditLine */

PROCEDURE pGetDatePay:
DEFINE INPUT PARAMETER  iContract AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iContCode AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iTypeDate AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iDate     AS DATE      NO-UNDO .
DEFINE INPUT PARAMETER  iSign     AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER  iTypeGraf AS INT64     NO-UNDO .
DEFINE OUTPUT PARAMETER oDate     AS DATE      NO-UNDO .

DEFINE VARIABLE         mFlPer    AS LOG       NO-UNDO .

   IF iTypeDate EQ "������" THEN
      mFlPer = TRUE.
   ELSE
      mFlPer = FALSE.
   CASE iSign:
      WHEN "=" OR
      WHEN "EQ" THEN
         FIND LAST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date EQ iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date EQ iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "LE" THEN
         FIND LAST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date LE iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date LE iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "LT" THEN
         FIND LAST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date LT iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date LT iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "GT" THEN
         FIND FIRST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date GT iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date GT iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
      WHEN "GE" THEN
         FIND FIRST term-obl WHERE
                   term-obl.contract  EQ iContract
               AND term-obl.cont-code EQ iContCode
               AND term-obl.idnt      EQ iTypeGraf
               AND ( IF NOT mFlPer THEN term-obl.end-date GE iDate ELSE TRUE)
               AND ( IF mFlPer THEN term-obl.dsc-beg-date GE iDate ELSE TRUE)
         NO-LOCK NO-ERROR.
   END CASE.

   IF AVAIL term-obl THEN
   DO:
      IF iTypeDate EQ "����" THEN
         oDate = term-obl.end-date.
      ELSE IF iTypeDate EQ "������" THEN
         oDate = term-obl.dsc-beg-date.
      ELSE
         oDate = ?.
   END.
   ELSE
      oDate = ?.

END PROCEDURE. /* pGetDatePay *//* ��楤�� ��࠭���� ��䨪� */

/* ��楤�� ��࠭���� ��䨪�� */
PROCEDURE SetTermOblHist :
DEFINE INPUT  PARAMETER iContract AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iContCode AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iSince    AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER iIdnt     AS INT64      NO-UNDO.
DEFINE INPUT  PARAMETER ichType   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iDescription   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iOlap          AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER iFlagMess      AS LOGICAL NO-UNDO .
DEFINE INPUT  PARAMETER iFlagGT        AS LOGICAL NO-UNDO . /* �� - ������� ��� ����� � �⮩ ����  */
DEFINE OUTPUT PARAMETER oErrorStat     AS LOGICAL NO-UNDO . /* �� - ���� �訡�� � ��䨪 �� ��࠭�� */

   RUN SetTermOblHistEx(iContract
                      , iContCode
                      , iSince
                      , iIdnt
                      , 0
                      , ichType
                      , iDescription
                      , NO
                      , ?
                      , iOlap
                      , OUTPUT oErrorStat).

END PROCEDURE. /* SetTermOblHist */

/* ��楤�� ��࠭���� ��䨪�� */
PROCEDURE SetTermOblHistEx:
DEFINE INPUT  PARAMETER iContract    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iContCode    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iSince       AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER iIdnt        AS INT64     NO-UNDO.
DEFINE INPUT  PARAMETER iNGr         AS INT64     NO-UNDO.
DEFINE INPUT  PARAMETER ichType      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iDescription AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iSigned      AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iSignDate    AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER iOlap        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oErrorStat   AS LOGICAL   NO-UNDO. /* �� - ���� �訡�� � ��䨪 �� ��࠭�� */

DEFINE VARIABLE vUserId    AS CHARACTER NO-UNDO .

DEF BUFFER term-obl-hist FOR term-obl-hist.
DEF BUFFER tobl-hist-amt FOR tobl-hist-amt.

IF FGetSetting ("�����", "������䈧�",?) NE "��"  THEN RETURN.
IF LOOKUP ( STRING(iIdnt), FGetSetting ("�����", "��䒨�",?)) = 0 THEN RETURN. 

   vUserId = USERID("bisquit").
   oErrorStat = TRUE  .

TR:
DO TRANSACTION 
   ON ERROR UNDO TR,LEAVE TR
   ON QUIT UNDO TR,LEAVE TR:

   /* ᮧ����� 蠯�� ���ਨ  */
   CREATE term-obl-hist .
      ASSIGN
         term-obl-hist.contract  = iContract
         term-obl-hist.cont-code = iContCode
         term-obl-hist.idnt      = iIdnt
         term-obl-hist.olap      = iOlap
         term-obl-hist.since     = iSince
         term-obl-hist.chtype    = ichtype
         term-obl-hist.description = idescription
         term-obl-hist.User-Id     = vUserId
   .

   VALIDATE term-obl-hist.
   
   /* ����⪠� */
     FOR EACH tobl-hist-amt WHERE
            tobl-hist-amt.tobl-id EQ term-obl-hist.tobl-id
   EXCLUSIVE-LOCK:
      DELETE tobl-hist-amt.
     END.
   /* ����஢���� ��䨪�� */
     FOR EACH term-obl  WHERE
            term-obl.contract  EQ iContract
        AND term-obl.cont-code EQ iContCode
        AND term-obl.idnt      EQ iIdnt
          NO-LOCK :
             CREATE tobl-hist-amt.
      BUFFER-COPY term-obl TO tobl-hist-amt.
             ASSIGN
               tobl-hist-amt.tobl-id = term-obl-hist.tobl-id
               .
      VALIDATE tobl-hist-amt.
     END.
   /* ������ ���祭�� � �� */
   UpdateSigns("term-obl-hist",STRING(term-obl-hist.tobl-id),"DateSave",STRING(NOW),YES).
   UpdateSigns("term-obl-hist",STRING(term-obl-hist.tobl-id),"NumGr"   ,STRING(iNGr),YES).

     oErrorStat = FALSE .
END.  /* TR */

END PROCEDURE. /* SetTermOblHistEx */


/* ��楤�� 㤠����� ��䨪�� */
PROCEDURE DelTermOblHist :
DEFINE INPUT  PARAMETER iContract AS CHARACTER  NO-UNDO.   /* ������� �����祭�� */
DEFINE INPUT  PARAMETER iContCode AS CHARACTER  NO-UNDO.   /* ������� ����� */
DEFINE INPUT  PARAMETER iSince    AS DATE       NO-UNDO.   /* ��� ��࠭���� ��䨪�� */
DEFINE INPUT  PARAMETER iIdnt     AS INT64      NO-UNDO.   /* ⨯ ��䨪� 1,3 � �� ��... */
DEFINE INPUT  PARAMETER iFlagGT   AS LOGICAL    NO-UNDO.   /* �� - 㤠���� ��� ����� � �⮩ ���� */
/* 㤠����� tobl-hist-amt � �ਣ��� �� 㤠����� term-obl-hist */

DEF BUFFER bterm-obl-hist FOR term-obl-hist.
DEF BUFFER bsigns FOR signs.

   IF iFlagGT THEN DO:
      FOR EACH  bterm-obl-hist WHERE
                bterm-obl-hist.contract  EQ iContract
            AND bterm-obl-hist.cont-code EQ iContcode
            AND bterm-obl-hist.idnt      EQ iIdnt
            AND bterm-obl-hist.since     GE iSince
            EXCLUSIVE-LOCK :
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(term-obl-hist.tobl-id)
            AND bsigns.code      EQ "Signed" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
            IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","�������� �������஢�� ��㣨� ���짮��⥫��").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","�������� �� ������").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(bterm-obl-hist.tobl-id)
            AND bsigns.code      EQ "SignDate" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
           IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","�������� �������஢�� ��㣨� ���짮��⥫��").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","�������� �� ������").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         DELETE bterm-obl-hist.
      END.
   END.
   ELSE DO:
      FIND FIRST bterm-obl-hist WHERE
                bterm-obl-hist.contract  EQ iContract
            AND bterm-obl-hist.cont-code EQ iContcode
            AND bterm-obl-hist.idnt      EQ iIdnt
            AND bterm-obl-hist.since     EQ iSince
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR .

      IF NOT AVAILABLE bterm-obl-hist THEN
      DO:
        IF LOCKED bterm-obl-hist THEN 
              RUN Fill-SysMes IN h_tmess("","","-1","����� �������஢��� ��㣨� ���짮��⥫��").
         ELSE RUN Fill-SysMes IN h_tmess("","","-1","����� �� �������").
         RETURN.
      END.

      IF AVAILABLE bterm-obl-hist THEN
      DO:
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(bterm-obl-hist.tobl-id)
            AND bsigns.code      EQ "Signed" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
            IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","�������� �������஢�� ��㣨� ���짮��⥫��").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","�������� �� ������").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         FIND FIRST bsigns WHERE
                bsigns.file-name EQ "term-obl-hist"
            AND bsigns.surrogate EQ STRING(bterm-obl-hist.tobl-id)
            AND bsigns.code      EQ "SignDate" 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF NOT AVAILABLE bsigns THEN
         DO:
            IF LOCKED bsigns THEN 
                 RUN Fill-SysMes IN h_tmess("","","-1","�������� �������஢�� ��㣨� ���짮��⥫��").
            ELSE RUN Fill-SysMes IN h_tmess("","","-1","�������� �� ������").
            RETURN.
         END.

         IF AVAIL bsigns THEN DELETE bsigns.
         DELETE bterm-obl-hist.
      END.
   END.
END PROCEDURE. /* DelTermOblHist */

/* ��楤�� ����祭�� �⠢�� � ����ᨬ��� �� �࠭�� �������� ��ࠬ���  */
PROCEDURE GetFloatRate:
   DEF INPUT  PARAM iCommission AS CHAR   NO-UNDO. /* ��� �����ᨨ */
   DEF INPUT  PARAM iDate       AS DATE   NO-UNDO. /* ��� */
   DEF INPUT  PARAM iBaseAmt    AS DEC    NO-UNDO. /* ����稭� ���� ��� ���� �� �⠢�� */
   DEF OUTPUT PARAM oRate       AS DEC    NO-UNDO INIT ?. /* ����稭� �⠢�� */
   MAIN:
   DO:
      /* ���砫� �� ���� �⠢�� */
      GetRefCrVal("init-float",
                  "commission",
                  iDate,
                  ?,
                  (TEMP-TABLE ttIndicate:HANDLE)).
      FOR EACH ttIndicate WHERE ttIndicate.fChar EQ iCommission:
          /* ����� �� �࠭�� "��" */
          GetRefCrVal("init-float",
                      "amt-min",
                      iDate,
                      ttIndicate.fChar,
                      (TEMP-TABLE ttIndicate1:HANDLE)).
          FOR EACH ttIndicate1 WHERE ttIndicate1.fDec LE iBaseAmt:
              /* ����� �� �࠭�� "��" */
              GetRefCrVal("init-float",
                          "amt-max",
                          iDate,
                          ttIndicate.fChar + CHR(44) + STRING(ttIndicate1.fDec),

                          (TEMP-TABLE ttIndicate2:HANDLE)).
              FOR FIRST ttIndicate2 WHERE ttIndicate2.fDec GT iBaseAmt
                                       OR ttIndicate2.fDec EQ 0: /* ��� ��� ���孥� �࠭��� */
                  oRate = DECIMAL(GetRefVal("init-float",
                                            iDate,
                                            ttIndicate.fChar + "," + STRING(ttIndicate1.fDec) + "," + STRING(ttIndicate2.fDec))).
                  LEAVE MAIN.
              END.
          END.
      END.
   END.

END PROCEDURE.

/* �������� ��ண� �᫮��� ��� ������� �����⭮� �㬬� */
PROCEDURE Cr_Cond_DblAnn.
  DEFINE INPUT PARAMETER iContract    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER iContCode    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER iSince       AS DATE      NO-UNDO.
  DEFINE INPUT PARAMETER iRecalcGraf  AS LOGICAL   NO-UNDO.
  DEFINE INPUT PARAMETER iRecalcAnn   AS LOGICAL   NO-UNDO.
  DEFINE INPUT PARAMETER iNewSince    AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER opRcd       AS RECID     NO-UNDO.
  DEFINE OUTPUT PARAMETER opSumm      AS DECIMAL   NO-UNDO.

  DEF VAR vFirstPeriod  AS INTEGER   NO-UNDO.
  DEF VAR vPartAmount   AS DECIMAL   NO-UNDO.
  DEF VAR vKolLgtPer    AS INTEGER   NO-UNDO.  
  DEF VAR vCredOffSet   AS CHARACTER NO-UNDO.    
  DEF VAR vAnnuitKorr   AS INTEGER   NO-UNDO. 
  DEF VAR vSumDepos     AS DECIMAL   NO-UNDO.
  DEF VAR vSince        AS DATE      NO-UNDO. /* ���, �� ������ ᮧ���� �᫮��� */
  DEF VAR vNumPers      AS INTEGER   NO-UNDO.
  DEF VAR vAmount       AS DECIMAL   NO-UNDO.
  DEF VAR vCommRate     AS DECIMAL   NO-UNDO.
  DEF VAR vAnnuit2      AS DECIMAL   NO-UNDO.
  DEF VAR vCondCount    AS INTEGER   NO-UNDO.
  DEF VAR vWorkGraf       AS CHAR    NO-UNDO.   /* ��䨪 ࠡ��� */
  DEF VAR vBranch         AS CHAR    NO-UNDO.   /* ���ࠧ������� */
  DEF VAR vTmpDate        AS DATE    NO-UNDO.
  DEF VAR vPrimPartAmount AS DECIMAL NO-UNDO.
  DEF VAR vDateCond       AS DATE    NO-UNDO.
  
  DEF BUFFER loan        FOR loan.
  DEF BUFFER loan-cond   FOR loan-cond.
  DEF BUFFER bloan-cond  FOR loan-cond.
  DEF BUFFER bbloan-cond FOR loan-cond.
  DEF BUFFER term-obl    FOR term-obl.

  opRcd = ?.

  FIND FIRST loan WHERE loan.contract  EQ iContract
                    AND loan.cont-code EQ iContCode
          NO-LOCK NO-ERROR.
  IF NOT AVAIL loan THEN RETURN "�訡�� ���᪠ �������".

  FIND LAST bloan-cond WHERE bloan-cond.contract  EQ iContract
                         AND bloan-cond.cont-code EQ iContCode
                         AND bloan-cond.since     LE iSince
      NO-LOCK NO-ERROR.
  IF NOT AVAIL bloan-cond THEN RETURN ERROR "�訡�� ���᪠ �᫮���" .

  ASSIGN
    vKolLgtPer  = INT(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "�����⏥�",
                                      "0"))
    vCredOffSet = GetXattrValueEx("loan-cond",
                                   bloan-cond.contract + "," 
                                   + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                   "cred-offset",
                                   "--")
    vAnnuitKorr = INT(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "����⊮��",
                                      "0"))
    vSumDepos   = DEC(GetXattrValueEx("loan",
                                      loan.contract + "," + loan.cont-code,
                                      "sum-depos",
                                      "0"))
    vAnnuit2   = DEC(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "AnnuitPlat2",
                                      "0"))
           
      .

  RUN GetFirstPerPartAmt(bloan-cond.contract, 
                         bloan-cond.cont-code,
                         bloan-cond.since,
                         OUTPUT vFirstPeriod,
                         OUTPUT vPartAmount).
  
  IF vPartAmount = 0 OR vFirstPeriod = 0 THEN RETURN ERROR "�訡�� ������� ४����⮢".

  /* �饬 ��᫥���� �᫮���, �� ���஬ ������ ���� �����⭠� �㬬� */
  IF iRecalcAnn <> YES THEN DO:
      RUN GetAnnuitPlat2(iContract,iContCode,bloan-cond.since,OUTPUT vAnnuit2).
      /* �᫨ �� 㤠���� ��।����� �㬬�, � �ਤ���� �� �������� */
      IF vAnnuit2 = 0 THEN iRecalcAnn = YES.
  END.
  
  IF iNewSince = ? THEN DO:  
      ASSIGN
         vBranch   = GetBranchForLoan((BUFFER loan:HANDLE))
         vWorkGraf = GetWorkGraf(iContract + "," + iContCode, 
                                 loan.Class-Code)
         vTmpDate  = loan.open-date
         vNumPers  = - 1
         .
    
      dt:
      DO WHILE vTmpDate < loan.end-date:
          
          vNumPers = vNumPers + 1.
    
          IF vNumPers = vFirstPeriod THEN DO:
             vSince = vTmpDate.
              LEAVE dt.
          END.
    
          /* ��।������ ���� ���⥦� � ��⮬ ��ਮ��筮��. */
          vTmpDate = RE_MOVE_DATE (vTmpDate,
                                   loan.end-date + 366,
                                   INT(bloan-cond.cred-date),
                                   STRING(bloan-cond.cred-period) + ":" + STRING(bloan-cond.cred-month),
                                   LOOKUP(vCredOffset,"--,->,<-"),
                                   loan.open-date,
                                   vBranch, 
                                   vWorkGraf).
          
      END. /* DO WHILE */
  END.
  ELSE vSince = iNewSince.

  IF vSince = ? THEN RETURN "�� ��।����� ��� ��砫� ��ਮ�� 㢥��祭��".

  /* �᫨ ���� ⥪�騩 ������� ���⮪, � �� � ���� 
  ** �㬬� �ᥣ� �।�� ����� �믫�祭��� � ��ࢮ� ��ਮ�� */
  FIND FIRST term-obl WHERE term-obl.contract  EQ iContract        
                        AND term-obl.cont-code EQ iContCode        
                        AND term-obl.end-date  EQ vSince 
                        AND term-obl.idnt      EQ 2
      NO-LOCK NO-ERROR.
  IF AVAIL term-obl THEN
      vAmount = term-obl.amt-rub.
  /* �᫨ �������� ���⪮� ��祬�-� ���, � ��।��塞 ᪮�쪮 �뤠�� 
  * �� ��ਮ� 㬥��襭�� � ᪮�쪮 �� �⮩ �㬬� ������ �뫨 ������� �� ��ࢮ��砫쭮�� �᫮��� */
  ELSE DO:
     RUN GetCurAmt(iContract,
                   iContCode,
                   vSince,
                   OUTPUT vAmount).
     FIND FIRST bbloan-cond WHERE  bbloan-cond.contract  EQ iContract
                               AND bbloan-cond.cont-code EQ iContCode
                               AND bbloan-cond.since     EQ loan.open-date
      NO-LOCK NO-ERROR.
     IF NOT AVAIL bbloan-cond THEN  RETURN ERROR  "�� ������� ��ࢮ� �᫮��� �������".

     vPrimPartAmount = DEC(GetXattrValueEx("loan-cond",
                                           bbloan-cond.contract + "," 
                                           + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                           "PartAmount",
                                           "0")).

     FIND FIRST term-obl WHERE term-obl.contract  EQ iContract        
                           AND term-obl.cont-code EQ iContCode        
                           AND term-obl.end-date  EQ loan.open-date 
                           AND term-obl.idnt      EQ 2
        NO-LOCK NO-ERROR.
     IF NOT AVAIL term-obl THEN  RETURN ERROR  "�� ��।����� ��ࢮ��砫쭠� �㬬� �������".
     
     vAmount = vAmount - ((term-obl.amt-rub * vPrimPartAmount) / 100).
  END.

  opRcd = ?.

  TR:
  DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
    ON QUIT UNDO TR,LEAVE TR:

          RUN CrCond(iContract, 
                     iContCode, 
                     vSince,
                     "%���,%����")
          NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
               UNDO TR,LEAVE TR.
          END.

          FIND FIRST loan-cond WHERE
                     loan-cond.contract  EQ iContract
                 AND loan-cond.cont-code EQ iContCode
                 AND loan-cond.since     EQ vSince
             NO-LOCK NO-ERROR.
          IF NOT AVAIL loan-cond THEN UNDO TR,LEAVE TR.

          UpdateSignsEx(loan-cond.class-code,
                        loan-cond.Contract + "," + loan-cond.Cont-code + "," + STRING(loan-cond.since),
                        "AutoCond",
                        "��").
          UpdateSignsEx(loan-cond.class-code,
                        loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                        "�����⏥�",
                        "0").
          IF iRecalcAnn THEN DO:
          
              vCommRate = GET_COMM_LOAN(loan.contract,
                                        loan.cont-code,
                                        "%�।",
                                        vSince).
    
              RUN CalcAnnuitet2(loan.contract,
                                loan.cont-code,
                                vSince,
                                loan.end-date,
                                vAmount,
                                vCommRate,
                                loan-cond.cred-date,
                                loan-cond.cred-period,
                                loan-cond.cred-month,
                                vKolLgtPer,
                                LOOKUP(vCredOffset,"--,->,<-"),
                                vAnnuitKorr,
                                vSumDepos,
                                vFirstPeriod,
                                vPartAmount,
                                2,
                                OUTPUT vAnnuit2).              
           END.
           
           UpdateSigns(loan-cond.class-code,
                       loan-cond.Contract + "," + loan-cond.Cont-code + "," + STRING(loan-cond.since),
                       "����⏫��",
                       STRING(vAnnuit2),
                       ?).       
           /* ���࠭塞 �� ⥪�饬 �᫮��� �㬬� ������ ��ਮ�� 㢥��祭�� */
           UpdateSigns(bloan-cond.class-code,
                       bloan-cond.Contract + "," + bloan-cond.Cont-code + "," + STRING(bloan-cond.since),
                       "AnnuitPlat2",
                       STRING(vAnnuit2),
                       ?).
            
           opRcd = RECID(loan-cond).
  END.  /* End of TR BLOCK */

  IF opRcd = ? THEN
     RETURN ERROR "�訡�� ᮧ����� ��⮬���᪮�� �᫮��� ������� � " + Loan.cont-code.

  FIND FIRST term-obl WHERE term-obl.contract  = iContract
                        AND term-obl.cont-code = iContCode
                        AND term-obl.idnt = 2
                        AND term-obl.end-date = vSince
      NO-LOCK NO-ERROR.
  IF AVAIL term-obl THEN opSumm = term-obl.amt-rub.
  ELSE RETURN ERROR "�訡�� ��।������ �㬬� �� ᮧ����� ��⮬���᪮�� �᫮��� ������� � " + Loan.cont-code.    

  IF iRecalcGraf THEN DO:
      FOR EACH loan-cond WHERE
               loan-cond.contract  = loan.contract
           AND loan-cond.cont-code = loan.cont-code
      NO-LOCK:
         vCondCount = vCondCount + 1.
      END.
      
      /* ������ ��䨪�� */
      RUN SetSysConf IN h_Base("�� �������� ������� �� �����","��").              
      RUN mm-to.p(RECID(loan),
                  opRcd,
                   opSumm,
                   1,
                   YES,
                   YES,
                   YES,
                   YES,
                   ?,
                   vCondCount) NO-ERROR.
      RUN DeleteOldDataProtocol IN h_Base("�� �������� ������� �� �����").
      IF ERROR-STATUS:ERROR THEN RETURN ERROR {&RETURN_VALUE}.
   END.
END PROCEDURE.

/* ��।������ ���� ��ண� �᫮��� ��� ������� �����⭮� �㬬� */
PROCEDURE GetDateDblAnn.
  DEFINE INPUT  PARAMETER iContract    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iContCode    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iSince       AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER oSince       AS DATE      NO-UNDO.

  DEF VAR vFirstPeriod  AS INTEGER   NO-UNDO.
  DEF VAR vPartAmount   AS DECIMAL   NO-UNDO.
  DEF VAR vKolLgtPer    AS INTEGER   NO-UNDO.  
  DEF VAR vCredOffSet   AS CHARACTER NO-UNDO.    
  DEF VAR vSince        AS DATE      NO-UNDO. /* ���, �� ������ ᮧ���� �᫮��� */
  DEF VAR vNumPers      AS INTEGER   NO-UNDO.
  DEF VAR vWorkGraf       AS CHAR    NO-UNDO.   /* ��䨪 ࠡ��� */
  DEF VAR vBranch         AS CHAR    NO-UNDO.   /* ���ࠧ������� */
  DEF VAR vTmpDate        AS DATE    NO-UNDO.

  DEF BUFFER loan        FOR loan.
  DEF BUFFER loan-cond   FOR loan-cond.
  DEF BUFFER bloan-cond  FOR loan-cond.
  DEF BUFFER bbloan-cond FOR loan-cond.

  FIND FIRST loan WHERE loan.contract  EQ iContract
                    AND loan.cont-code EQ iContCode
          NO-LOCK NO-ERROR.
  IF NOT AVAIL loan THEN RETURN "�訡�� ���᪠ �������".

  FIND LAST bloan-cond WHERE bloan-cond.contract  EQ iContract
                         AND bloan-cond.cont-code EQ iContCode
                         AND bloan-cond.since     LE iSince
      NO-LOCK NO-ERROR.
  IF NOT AVAIL bloan-cond THEN RETURN ERROR "�訡�� ���᪠ �᫮���" .

  ASSIGN
    vKolLgtPer  = INT(GetXattrValueEx("loan-cond",
                                      bloan-cond.contract + "," 
                                      + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                      "�����⏥�",
                                      "0"))
    vCredOffSet = GetXattrValueEx("loan-cond",
                                   bloan-cond.contract + "," 
                                   + bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                   "cred-offset",
                                   "--")        
      .
 
  RUN GetFirstPerPartAmt(bloan-cond.contract, 
                         bloan-cond.cont-code,
                         bloan-cond.since,
                         OUTPUT vFirstPeriod,
                         OUTPUT vPartAmount).

  IF vFirstPeriod = 0 THEN RETURN ERROR "�訡�� ������� ४����� FirstPeriod".

  ASSIGN
     vBranch   = GetBranchForLoan((BUFFER loan:HANDLE))
     vWorkGraf = GetWorkGraf(iContract + "," + iContCode, 
                             loan.Class-Code)
     vTmpDate  = loan.open-date
     vNumPers  = - 1
     .

  dt:
  DO WHILE vTmpDate < loan.end-date:
      
      vNumPers = vNumPers + 1.

      IF vNumPers = vFirstPeriod THEN DO:
         vSince = vTmpDate.
          LEAVE dt.
      END.

      /* ��।������ ���� ���⥦� � ��⮬ ��ਮ��筮��. */
      vTmpDate = RE_MOVE_DATE (vTmpDate,
                               loan.end-date + 366,
                               INT(bloan-cond.cred-date),
                               STRING(bloan-cond.cred-period) + ":" + STRING(bloan-cond.cred-month),
                               LOOKUP(vCredOffset,"--,->,<-"),
                               loan.open-date,
                               vBranch, 
                               vWorkGraf).
      IF vTmpDate = ? THEN LEAVE dt.
      
  END. /* DO WHILE */

  oSince = vSince.

END PROCEDURE.

/* ��।������ ���� � �த����⥫쭮�� ��ਮ�� 㬥��襭�� */
PROCEDURE GetFirstPerPartAmt.
    DEFINE INPUT PARAMETER iContract    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iContCode    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iSince       AS DATE      NO-UNDO.

    DEFINE OUTPUT PARAMETER  oFirstPeriod     AS INT   NO-UNDO.
    DEFINE OUTPUT PARAMETER  oPartAmount      AS DEC   NO-UNDO.

    DEF BUFFER bbloan-cond FOR loan-cond.

    DEF VAR vDateCond       AS DATE NO-UNDO.

    ASSIGN
        oPartAmount  = DEC(GetXattrValueEx("loan-cond",
                                           iContract + "," + iContCode + "," + STRING(iSince),
                                           "PartAmount",
                                           "0"))
        oFirstPeriod = INT(GetXattrValueEx("loan-cond",
                                           iContract + "," + iContCode + "," + STRING(iSince),
                                           "FirstPeriod",
                                           "0"))
          .
    /* �饬 ��᫥���� �᫮���, �� ���஬ ������ ��ࠬ���� 
    ** ��� ���� ��ன �����⭮� �㬬� */
    vDateCond = iSince.
    fcnd:
    DO WHILE  oPartAmount = 0 
         AND oFirstPeriod = 0:
       FIND LAST bbloan-cond WHERE bbloan-cond.contract  EQ iContract
                               AND bbloan-cond.cont-code EQ iContCode
                               AND bbloan-cond.since     LT vDateCond
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bbloan-cond THEN LEAVE fcnd.
      ASSIGN
          vDateCond    = bbloan-cond.since
          oPartAmount  = DEC(GetXattrValueEx("loan-cond",
                                            bbloan-cond.contract + "," 
                                            + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                            "PartAmount",
                                            "0"))
          oFirstPeriod = INT(GetXattrValueEx("loan-cond",
                                             bbloan-cond.contract + "," 
                                             + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                             "FirstPeriod",
                                             "0"))
          .
    END.
      
END PROCEDURE.

/* ��।������ ��࠭����� �㬬� �����⭮�� ���⥦� ��ਮ�� 㢥��祭�� */
PROCEDURE GetAnnuitPlat2:
    DEFINE INPUT PARAMETER iContract    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iContCode    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iSince       AS DATE      NO-UNDO.
   
    DEFINE OUTPUT PARAMETER  oAnnuit2      AS DEC   NO-UNDO.
   
    DEF BUFFER bbloan-cond FOR loan-cond.
   
    DEF VAR vDateCond AS DATE NO-UNDO.
   
    ASSIGN
       vDateCond = iSince
       oAnnuit2 = 0.

    fcnd1:
    DO WHILE oAnnuit2 = 0:
        FIND LAST bbloan-cond WHERE bbloan-cond.contract  EQ iContract
                                AND bbloan-cond.cont-code EQ iContCode
                                AND bbloan-cond.since     LT vDateCond
       NO-LOCK NO-ERROR.
       IF NOT AVAIL bbloan-cond THEN LEAVE fcnd1.
       ASSIGN
           vDateCond  = bbloan-cond.since
           oAnnuit2   = DEC(GetXattrValueEx("loan-cond",
                                             bbloan-cond.contract + "," 
                                             + bbloan-cond.cont-code + "," + STRING(bbloan-cond.since),
                                             "AnnuitPlat2",
                                             "0")).
    END.

END PROCEDURE.

/* ��������� ���� ������ �।�⭮�� ������� */
PROCEDURE ChangeLoanDate.
   DEF INPUT  PARAM iContract    AS CHAR NO-UNDO. /* �����䨪���     */
   DEF INPUT  PARAM iContCode    AS CHAR NO-UNDO. /* �������          */
   DEF INPUT  PARAM iNewDate     AS DATE NO-UNDO. /* ����� ���        */
   DEF INPUT  PARAM iChange      AS CHAR NO-UNDO. /* �� - ������ ���� ��砫� � ��ந�� ��䨪�
                                                      ��� - ���쪮 ����஥��� ��䨪�� */
   DEF INPUT  PARAM iChEndDate   AS CHAR NO-UNDO. /* �� ᤢ����� ���� ����砭�� */
   DEF INPUT  PARAM iChkSrok     AS CHAR NO-UNDO. /* �஢����� ��筮筮��� ��㤭��� ��� */
   DEF INPUT  PARAM iChLacct     AS CHAR NO-UNDO. /* �������� ���� �ਢ離� ��⮢ */
   DEF OUTPUT PARAM oOk          AS LOG  NO-UNDO. /* ���� �ᯥ譮��   */

   DEF BUFFER loan       FOR loan.      /* ���������� ����� */
   DEF BUFFER loan-cond  FOR loan-cond. /* ���������� ����� */
   DEF BUFFER signs      FOR signs.     /* ���������� ����� */
   DEF BUFFER bsigns     FOR signs.     /* ���������� ����� */
   DEF BUFFER bloan-cond FOR loan-cond. /* ���������� ����� */ 
   DEF BUFFER ins-loan   FOR loan.      /* ���������� ����� */
   DEF BUFFER vcomm-rate FOR comm-rate. /* ���������� ����� */ 
   DEF BUFFER loan-int   FOR loan-int.  /* ���������� ����� */
   DEF BUFFER bloan      FOR loan.      /* ���������� ����� */
   DEF BUFFER loan-acct  FOR loan-acct. /* ���������� ����� */
   DEF BUFFER acct       FOR acct.      /* ���������� ����� */
   DEF BUFFER bcomm-rate FOR comm-rate. /* ���������� ����� */
   DEF BUFFER bterm-obl  FOR term-obl . /* ���������� ����� */

   DEF VAR vNDays       AS INT   NO-UNDO. /* ������⢮ ���� ����⢨� ���-� */
   DEF VAR vNMonthes    AS INT   NO-UNDO. /* ������⢮ ����楢 ����⢨� ���-� */
   DEF VAR vNYears      AS INT   NO-UNDO. /* ������⢮ ��� ����⢨� ���-� */
   DEF VAR vNEnd-date   AS DATE  NO-UNDO. /* ���, ᤢ����� � ᮮ�. � ࠡ/��ࠡ ��ﬨ */
   DEF VAR vMove        AS INT   NO-UNDO. /* ���ࠢ����� ᤢ��� ���� */
   DEF VAR vCounter     AS INT   NO-UNDO. /* ���稪 ��� 㤠����� ��� ��䨪�� */
   DEF VAR vLrecid      AS RECID NO-UNDO. /* RECID loan */
   DEF VAR vLCrecid     AS RECID NO-UNDO. /* RECID loan-cond */
   DEF VAR vLCSurr      AS CHAR  NO-UNDO. /* ���ண�� loan-cond */
   DEF VAR vTOSurr      AS CHAR  NO-UNDO. /* ���ண�� term-obl */
   DEF VAR vTOSurrNew   AS CHAR  NO-UNDO. /* ���ண�� ������ term-obl */
   DEF VAR vCredSumm    AS DEC   NO-UNDO. /* �㬬� �।�⭮�� ������� */
   DEF VAR vCredRisk    AS DEC   NO-UNDO. /* ����.�᪠ �।�⭮�� ������� */
   DEF VAR vCredOffs    AS CHAR  NO-UNDO. /* ���祭�� ���.४� cred-offset */
   DEF VAR vIntOffs     AS CHAR  NO-UNDO. /* ���祭�� ���.४�int-offset */
   DEF VAR vOldDate     AS DATE  NO-UNDO. /* ���� ��� ������ ������� � �᫮��� */
   DEF VAR vOCRSurr     AS CHAR  NO-UNDO. /* ���ண�� ��ண� comm-rate'a */
   DEF VAR vNCRSurr     AS CHAR  NO-UNDO. /* ���ண�� ������ comm-rate'a */
   DEF VAR vQualGar     AS LOG   NO-UNDO. /* �ᯥ譮��� ��⠭���� ��⥣�ਨ ����⢠ */
   DEF VAR vSurrIns     AS CHAR  NO-UNDO. /* ���ண�� ������� ���客����*/
   DEF VAR vAnnSumm     AS DEC   NO-UNDO. /* ����� �㬬� ������ */
   DEF VAR vRateCred    AS DEC   NO-UNDO. /* �⠢�� %�। */
   DEF VAR vCredDate    AS INT   NO-UNDO. /* cred-date � �᫮��� */
   DEF VAR vCredPer     AS CHAR  NO-UNDO. /* cred-period � �᫮��� */
   DEF VAR vCredMon     AS CHAR  NO-UNDO. /* cred-month � �᫮��� */
   DEF VAR vKolLgtPer   AS INT   NO-UNDO. /* �����⏥� � 㫮��� */
   DEF VAR vCondClass   AS CHAR  NO-UNDO. /* ����� � 㫮��� */
   DEF VAR vBal2Acct    AS CHAR  NO-UNDO. /* �����ᮢ� ��� 2-�� ���浪� */
   DEF VAR vListType    AS CHAR  NO-UNDO. /* �� list_type */
   DEF VAR vAnnuitCorr  AS INT   NO-UNDO. /* �� ����⊮�� */
   DEF VAR vSummDepos   AS DEC   NO-UNDO. /* �� sum-depos */
   DEF VAR vFirstPeriod AS INT   NO-UNDO. /* �� FirstPeriod */
   DEF VAR vPartAmount  AS DEC   NO-UNDO. /* �� PartAmount */
   DEF VAR vComm        AS CHAR  NO-UNDO.

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* ������㥬 ������� */
      {profind.i &fnoer=TRUE &fnowh=TRUE
          &fway=FIRST &fbuf=loan &flock=EXCLUSIVE-LOCK &ffor=TRUE
          &fcond="WHERE loan.contract  EQ iContract ~
                    AND loan.cont-code EQ iContCode"}
      IF NOT AVAILABLE loan THEN DO:
         IF LOCKED loan THEN 
            RUN Fill-SysMes IN h_tmess("","","-1",
                                       SUBSTITUTE("������� &1 �������஢�� ��㣨� ���짮��⥫��",
                                                  iContCode)
                                      ).
         ELSE                             
            RUN Fill-SysMes IN h_tmess("","","-1",
                                       SUBSTITUTE("������� �� ������ &1", iContCode)
                                      ).
         LEAVE mb .
      END.   

      /* ������㥬 �᫮��� ������� */
      {profind.i &fnoer=TRUE &fnowh=TRUE
          &fway=FIRST &fbuf=loan-cond &flock=EXCLUSIVE-LOCK &ffor=TRUE
          &fcond="WHERE loan-cond.contract  EQ iContract ~
                    AND loan-cond.cont-code EQ iContCode"}
      IF NOT AVAILABLE loan-cond THEN DO:
         IF LOCKED loan-cond THEN 
            RUN Fill-SysMes IN h_tmess("","","-1",
                                       SUBSTITUTE("�᫮��� ������� &1 �������஢�� ��㣨� ���짮��⥫��",
                                                  iContCode)
                                      ).
         ELSE
            RUN Fill-SysMes IN h_tmess("","","-1",
                                    SUBSTITUTE("�᫮��� ������� &1 �� �������", iContCode)
                                   ).
         LEAVE mb .
      END.   

      ASSIGN
         /* ���� ������� ���客���� �� �।�⭮�� ��������  */
         vSurrIns = ENTRY(1,
                          GetLinks("Link-Insur",               /* ID �����                            */
                                   iContract + "," + iContCode,/* ID(c��ண��) ��ꥪ�                 */
                                   ?,                          /* ���ࠢ����� �裡: s | t | ?         */
                                   "*",                        /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                                   CHR(1),                     /* �������⥫� १������饣� ᯨ᪠   */
                                   ?                           /* ���, �� ������ �����⢫���� ���� �痢� */
                           ),
                           CHR(1)
                           )
      .
      FIND FIRST ins-loan WHERE ins-loan.contract  EQ ENTRY(1,vSurrIns)
                            AND ins-loan.cont-code EQ ENTRY(2,vSurrIns)
      NO-LOCK NO-ERROR.
      
      /* ���樠�����㥬 ����室��� ��६���� */
      ASSIGN
         vLrecid          = RECID(loan)
         vLCrecid         = RECID(loan-cond)
         vCredRisk        = loan.risk
         vCredOffs        = GetXAttrValueEx("loan-cond",
                                            loan-cond.contract + "," 
                                          + loan-cond.cont-code + "," 
                                          + STRING(loan-cond.since),
                                            "cred-offset",
                                            "--")
         vIntOffs         = GetXAttrValueEx("loan-cond",
                                            loan-cond.contract + "," 
                                          + loan-cond.cont-code + "," 
                                          + STRING(loan-cond.since),
                                            "int-offset",
                                            "--")
      .

      /* ������ ������� ���⮪ � ��࠭�� ��� �㬬�, ��� ���� ��䨪�� � ��� */
      FIND FIRST term-obl WHERE term-obl.contract  EQ iContract
                            AND term-obl.cont-code EQ iContCode
                            AND term-obl.idnt      EQ 2
      NO-LOCK NO-ERROR.
      vCredSumm = term-obl.amt-rub.

      /* �᫨ ����室��� ᬥ�� ���� ������ ������� � �᫮��� */
      IF iChange EQ "��" THEN
      CHANGE:
      DO:
         IF    CAN-FIND(FIRST loan-int WHERE 
                              loan-int.contract  EQ loan.contract
                          AND loan-int.cont-code EQ loan.cont-code
                        NO-LOCK)
            OR CAN-FIND(FIRST bloan WHERE 
                              bloan.contract  EQ     loan.contract
                          AND bloan.cont-code BEGINS loan.cont-code + " "
                          AND NUM-ENTRIES (bloan.cont-code, " ") GT 1
                        NO-LOCK) THEN
         DO:
            RUN Fill-SysMes("",
                            "",
                            0,
                            "��� ������� �������� ����樨, ��� �祭��, "
                          + "~n��������� ���� ����������."
                            ).
            LEAVE CHANGE.
         END.
         IF iChkSrok EQ "��" THEN
         DO:
            FIND LAST loan-acct WHERE 
                      loan-acct.contract  EQ loan.contract
                  AND loan-acct.cont-code EQ loan.cont-code
                  AND loan-acct.acct-type EQ "�।��"
            NO-LOCK NO-ERROR.
            IF AVAIL loan-acct THEN
            DO:
               FIND FIRST acct WHERE 
                          acct.acct     EQ loan-acct.acct
                      AND acct.currency EQ loan-acct.currency
               NO-LOCK NO-ERROR.
               IF AVAIL acct THEN
               DO:
                  RUN GetBalAcctFromLoan (loan.contract,
                                          loan.cont-code,
                                          "�।��"
                                          ).
                  vBal2Acct = GetSysConf("���������� ����").
                  IF vBal2Acct NE STRING(acct.bal-acct) THEN
                  DO:
                     pick-value = ?.
                     
                     RUN Fill-SysMes("",
                                     "",
                                     4,
                                     "�������騩 ��� "
                                   + STRING(acct.bal-acct)
                                   + " �� ���室�� �� �ப� � ������� �।���.~n������ ���� ��� "
                                   + vBal2Acct
                                   + ".~n�த������ ᬥ�� ���� ?~n(ᬥ�� ��� ������ ���� �믮����� ������)"
                                     ).
                                     
                     IF pick-value EQ "NO" THEN
                        LEAVE CHANGE.
                  END. /* IF vBal2Acct NE acct.acct-bal THEN */
               END. /* IF AVAIL acct THEN */
            END. /* IF AVAIL loan-acct THEN */
         END. /* IF iChkSrok THEN */


         /* �᫨ �맢��� � ��� �� ����, � ������ � �뫠 �������� ���
         ** �� �।��, � �� ᮧ���� ����� �᫮���.
         ** !!!!! ����� ��� �஢��� �뭥�� ���, �.�. �� �� �� �⮣� ����� ⮣�� ����� ��� !!!! */
         IF iNewDate NE loan-cond.since THEN
         DO:
            CREATE bloan-cond.
            BUFFER-COPY loan-cond EXCEPT since TO bloan-cond.
            ASSIGN
               bloan-cond.since = iNewDate
               vLCrecid         = RECID(bloan-cond)
            .
            vLCSurr = GetSurrogateBuffer("loan-cond",(BUFFER bloan-cond:HANDLE)).
         END.
         ELSE
            vLCSurr = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE)).

         /* �����塞 ���� ������ ������� � ���� ������ �������,
         ** � ⠪�� ���� ��砫� �᫮��� */
         ASSIGN
            vOldDate            = loan.open-date
            loan.open-date      = iNewDate
            loan.since          = iNewDate
            ins-loan.open-date  = iNewDate WHEN AVAIL ins-loan
            ins-loan.since      = iNewDate WHEN AVAIL ins-loan
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.

         /* ������塞 ���.४����� ��⠑��� �� ������� */
         UpdateSigns(loan.class-code,
                     iContract + "," + iContCode,
                     "��⠑���",
                     STRING(iNewDate),
                     ?).
   

         /* ������ �����塞 ���� ����砭�� �������, �஢�ਢ �� ���. ���� */
         ASSIGN
            vCondClass   = IF iNewDate NE loan-cond.since THEN bloan-cond.class-code
                                                          ELSE loan-cond.class-code
            vCredDate    = IF iNewDate NE loan-cond.since THEN bloan-cond.cred-date
                                                          ELSE loan-cond.cred-date
            vCredPer     = IF iNewDate NE loan-cond.since THEN bloan-cond.cred-period
                                                          ELSE loan-cond.cred-period
            vCredMon     = IF iNewDate NE loan-cond.since THEN STRING(bloan-cond.cred-month)
                                                          ELSE STRING(loan-cond.cred-month)
            vKolLgtPer   = INT(GetXAttrValue("loan-cond",vLCSurr,"�����⏥�"))
            vAnnuitCorr  = INT(GetXAttrValueEx("loan-cond",vLCSurr,"����⊮��",?))
            vSummDepos   = DEC(GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"Sum-depos","0"))
            vFirstPeriod = INT(GetXattrValueEx("loan-cond",vLCSurr,"FirstPeriod","0"))
            vPartAmount  = DEC(GetXattrValueEx("loan-cond",vLCSurr,"PartAmount","0"))
         .
         
         IF iChEndDate NE "��" THEN
         DO:
            ASSIGN
               vNDays     = INT(GetXAttrValueEx ("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"NDays","0"))
               vNMonthes  = INT(GetXAttrValueEx ("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"NMonthes","0"))
               vNYears    = INT(GetXAttrValueEx ("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),"NYears","0"))
               vNEnd-date = GoMonth(iNewDate,vNYears * 12 + vNMonthes) + vNDays
            .
            /* �ࠢ������ ����稢����� ���� � ��⮩ ��ࢮ�� ࠡ�祣� ���
            ** �᫨ ��� �⫨����� - ����� ����稫��� ��� ��室���� � ���� ������� */
            IF vNEnd-date NE GetFirstDayForLoan((BUFFER loan:HANDLE),
                                                vNEnd-date,
                                               -1) THEN
            DO:
               RUN Fill-SysMes in h_tmess("","",3,"��� ����砭�� ������� ������ �� ��室��� ����" + "~n" + "�������� �� ������訩 ࠡ�稩 ?|�������� ���।,�������� �����,�� ��������").
               /* �.�. ��� ࠡ��� �㭪樨 GetFirstDayForLoan ���� ��।����� 
               ** ������⢮ ���� � ����� ��������� �᪠�� ࠡ�稩 ����
               ** �� �� �� ��訢����� �� ������� ���� - ���� ��ࢮ�� ࠡ�祣� 
               ** ��� ��࠭�稬 ���⥫쭮���� �।�� */
               CASE pick-value:
                  WHEN "2" THEN
                     vMove = loan.open-date - loan.end-date.
                  WHEN "1" THEN
                     vMove = - (loan.open-date - loan.end-date).
                  OTHERWISE
                     vMove = 0.
               END CASE.
               vNEnd-date = GetFirstDayForLoan((BUFFER loan:HANDLE),vNEnd-date,vMove).
            END.

            ASSIGN
               loan.end-date     = vNEnd-date
               ins-loan.end-date = vNEnd-date WHEN AVAIL ins-loan
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
         END.



         /* ������, ��᫥ ᤢ���, ᭮�� ��ॢ��᫨� �ப ����/����楢/���
         ** �.�. �ப ��� ����������, � ⠪�� �᫨ ��� ��砫� ��⠥��� ⮩ ��
         ** � ⮦� ���� ��ॢ�稫᫨�� �ப, �.�. �� ����� ���� ��������� 
         ** �᫮���, ��� �᫨ �� �����﫨 ���� ����砭�� ������� */
         IF   vMove      NE 0 
           OR iNewDate   EQ vOldDate
           OR iChEndDate EQ "��" THEN
         DO:
            RUN DMY_In_Per(loan.open-date, 
                           loan.end-date,
                           OUTPUT vNDays,
                           OUTPUT vNMonthes,
                           OUTPUT vNYears).
                     
         END.
         IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"���ጥ�",GetXAttrInit(loan.class-code,"���ጥ�")) EQ "��" THEN
         DO:
            UpdateSigns(loan-cond.class-code,vLCSurr,"NDays",STRING(vNDays),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NMonthes",STRING(vNMonthes + vNYears * 12),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NYears","0",?).
         END.
         ELSE
         DO:
            UpdateSigns(loan-cond.class-code,vLCSurr,"NDays",STRING(vNDays),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NMonthes",STRING(vNMonthes),?).
            UpdateSigns(loan-cond.class-code,vLCSurr,"NYears",STRING(vNYears),?).
         END.
         UpdateSigns(loan-cond.class-code,vLCSurr,"CondEndDate",STRING(vNEnd-date),?).
         
         /* ������塞 ���ண��� ���.४�� �.�. � ���ண�� �室�� loan-cond.since */
         IF iNewDate NE vOldDate THEN
         DO:
            FOR EACH bsigns WHERE bsigns.file-name EQ "loan-cond"
                              AND bsigns.surrogate EQ loan.contract + "," + loan.cont-code + "," + STRING(vOldDate)
            NO-LOCK:
               {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=FIRST &fbuf=signs &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE RECID(signs) EQ RECID(bsigns) "}
               IF NOT AVAILABLE signs THEN DO:
                  UNDO mb, LEAVE mb.
               END.   
            
               IF CAN-DO("NDays,NMonthes,NYears,CondEndDate",signs.code) THEN
               DO:
                  DELETE signs.
                  NEXT.
               END.
               ASSIGN
                   signs.surrogate = loan.contract + "," + loan.cont-code + "," + STRING(iNewDate)
               NO-ERROR.

               IF ERROR-STATUS:ERROR THEN
                  UNDO mb, LEAVE mb.
            END.
         END.
         
         /* ���塞 ���� � �⠢�� */
         FIND FIRST bcomm-rate WHERE
                    bcomm-rate.kau   EQ loan-cond.contract + "," + loan-cond.cont-code
         NO-LOCK NO-ERROR.
         DO WHILE AVAIL bcomm-rate:
            vComm = bcomm-rate.commission.
            {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=FIRST &fbuf=comm-rate &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE RECID(comm-rate) EQ RECID(bcomm-rate) "}
            IF NOT AVAILABLE comm-rate THEN DO:
               IF LOCKED comm-rate THEN 
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    SUBSTITUTE("�⠢�� &1 �������஢��� ��㣨� ���짮��⥫��",
                                               comm-rate.kau )
                                   ).
               ELSE
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    SUBSTITUTE("�⠢�� &1 �� �������", comm-rate.kau )
                                   ).
               UNDO mb, LEAVE mb.
            END. 
            comm-rate.since = iNewDate .
            
            RELEASE comm-rate .   
            RELEASE bcomm-rate .   
            FIND FIRST bcomm-rate WHERE
                       bcomm-rate.kau        EQ loan-cond.contract + "," + loan-cond.cont-code
                   AND bcomm-rate.commission GT vComm
                   AND bcomm-rate.since      EQ vOldDate
            NO-LOCK NO-ERROR.
         END.  
                                       
         /* ������ �����塞 ���� �� ᮧ����� ���ᯥ祭��, �᫨ ��� �뫨 */
         FOR EACH bterm-obl WHERE bterm-obl.contract  EQ loan.contract
                              AND bterm-obl.cont-code EQ loan.cont-code
                              AND bterm-obl.idnt      EQ 5
         NO-LOCK:

            /* ������㥬 ���ᯥ祭�� */
            {profind.i &fnoer=TRUE &fnowh=TRUE
               &fway=FIRST &fbuf=term-obl &flock=EXCLUSIVE-LOCK &ffor=TRUE
               &fcond="WHERE RECID(term-obl) EQ RECID(bterm-obl) "}
            IF NOT AVAILABLE term-obl THEN DO:
               IF LOCKED term-obl THEN 
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    "���ᯥ祭�� �������஢��� ��㣨� ���짮��⥫��"                                               
                                   ).
               ELSE
                  RUN Fill-SysMes IN h_tmess("","","-1", "���ᯥ祭�� �� �������" ).
               UNDO mb, LEAVE mb.
            END.   
          
            ASSIGN
               term-obl.end-date = vNEnd-date
               term-obl.fop-date = iNewDate
            .

            
            /* �� � ��⮣��� ����⢠ term-obl ���������� �� �ਣ��� term-obl */
         END.
         
         /* ��᫥ ᬥ�� ����, 㤠�塞 ��஥ �᫮��� */
         IF iNewDate NE loan-cond.since THEN
         DO:
            RELEASE bloan-cond NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
            DELETE loan-cond NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
         END.

         /* ������ ���� �������� �㬬� �����⭮�� ���⥦�, �᫨ �奬� �����⭠� */
         IF GetXAttrValue("loan-cond",vLCSurr,"�奬�����") EQ "�����⭠�" THEN
         DO:
            FIND FIRST vcomm-rate WHERE vcomm-rate.kau        EQ loan.contract + "," + loan.cont-code
                                    AND vcomm-rate.commission EQ "%�।"
                                    AND vcomm-rate.since      EQ iNewDate
            NO-LOCK NO-ERROR.
            
            IF AVAIL vcomm-rate THEN
               vRateCred = vcomm-rate.rate-comm.

            RUN CalcAnnuitet (loan.contract,
                              loan.cont-code,
                              loan.open-date,
                              loan.end-date,
                              vCredSumm,
                              vRateCred,
                              vCredDate, 
                              vCredPer,  
                              vCredMon,
                              vKolLgtPer,
                              STRING(LOOKUP(vCredOffs,"--,->,<-")),
                              vAnnuitCorr, 
                              vSummDepos,  
                              vFirstPeriod,
                              vPartAmount, 
                              OUTPUT vAnnSumm).
            UpdateSigns(vCondClass,vLCSurr,"����⏫��",STRING(vAnnSumm),?).
         END.

         IF iChLAcct EQ "��" THEN
         DO:
            vListType = GetXAttrInit(loan.class-code, "list_type").
            REPEAT vCounter = 1 TO NUM-ENTRIES(vListType):

               {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=LAST &fbuf=loan-acct &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE loan-acct.contract  EQ loan.contract ~
                            AND loan-acct.cont-code EQ loan.cont-code ~
                            AND loan-acct.acct-type EQ ENTRY(vCounter, vListType) "}
               IF LOCKED loan-acct THEN DO:
                  RUN Fill-SysMes IN h_tmess("","","-1",
                                    "��� �������஢�� ��㣨� ���짮��⥫��" 
                                   ).
                  UNDO mb, LEAVE mb.
               END.   

               IF AVAIL loan-acct THEN
               DO:
                  loan-acct.since = iNewDate.

                  {profind.i &fnoer=TRUE &fnowh=TRUE
                     &fway=FIRST &fbuf=acct &flock=EXCLUSIVE-LOCK &ffor=TRUE
                     &fcond="WHERE acct.acct     EQ loan-acct.acct ~
                               AND acct.currency EQ loan-acct.currency "}
                  IF NOT AVAILABLE acct THEN DO:
                     IF LOCKED acct THEN 
                        RUN Fill-SysMes IN h_tmess("","","-1",
                                    "��� �������஢�� ��㣨� ���짮��⥫��" 
                                   ).
                     ELSE
                        RUN Fill-SysMes IN h_tmess("","","-1", "��� �� ������" ).
                     
                     UNDO mb, LEAVE mb.
                  END.   

                  IF AVAIL acct THEN
                     acct.open-date = iNewDate.
               END.
            END.
         END.

         /* ���᪠�� (ࠧ��稢���) loan � loan-cond */
         RELEASE loan NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
         RELEASE loan-acct NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.            
         RELEASE acct NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
         RELEASE loan-cond NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
         RELEASE comm-rate NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO mb, LEAVE mb.
      END.
      
      /* ������ ������뢠�� ��䨪� 
      ** ���砫� ��⨬ �� �� ( � idnt = 1,2,3 ) */
      DO vCounter = 1 TO 3:
         FOR EACH bterm-obl WHERE bterm-obl.contract  EQ iContract
                              AND bterm-obl.cont-code EQ iContCode
                              AND bterm-obl.idnt      EQ vCounter
         NO-LOCK:
            {profind.i &fnoer=TRUE &fnowh=TRUE
                  &fway=FIRST &fbuf=term-obl &flock=EXCLUSIVE-LOCK &ffor=TRUE
                  &fcond="WHERE RECID(term-obl) EQ RECID(bterm-obl) "}

            DELETE term-obl NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               UNDO mb, LEAVE mb.
         END.
      END.

      RUN SetSysConf IN h_base ("������������� �� �������� �����",STRING(LOOKUP(vCredOffs,"--,->,<-"))).
      RUN SetSysConf IN h_base ("������� �� ��������� �����",STRING(LOOKUP(vIntOffs,"--,->,<-"))).

      /* ����᪠�� ᮧ����� ��䨪�� */
      RUN mm-to.p(vLrecid,
                  vLCrecid,
                  vCredSumm,
                  1,         /* ���� ����� ����� */
                  YES,
                  YES,
                  YES,
                  YES,
                  vCredRisk,
                  0) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         UNDO mb, LEAVE mb.
      
      RUN DeleteOldDataProtocol IN h_base("������� �� ��������� �����").
      RUN DeleteOldDataProtocol IN h_base("������������� �� �������� �����").
      
      oOk = YES.
   END. /* mb: */
END PROCEDURE.

/*-------------------------------------------------------------------------
  ��楤�� ��� ��।������ �����ᮢ��� ��� 2-�� ���浪� ��� ��㤭��� ���.
  ����� �����ᮢ��� ��� ��।������ �� �᭮����� ������ �������.
  -------------------------------------------------------------------------*/
PROCEDURE GetBalAcctFromLoan:
   DEF INPUT  PARAM iContract     AS CHAR NO-UNDO. 
   DEF INPUT  PARAM iContCode     AS CHAR NO-UNDO. 
   DEF INPUT  PARAM iType         AS CHAR NO-UNDO. /* ���� ��� */

   DEF VAR vTerm         AS CHAR    INITIAL "" NO-UNDO.
    DEF VAR DTType        AS CHAR    INITIAL "" NO-UNDO.
    DEF VAR DTKind        AS CHAR    INITIAL "" NO-UNDO.
    DEF VAR DTTerm        AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR DTCust        AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR mask-internal AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR s             AS CHAR    INITIAL "" NO-UNDO.
   DEF VAR yy            AS INT     NO-UNDO.
   DEF VAR dd            AS INT     NO-UNDO.
   DEF VAR vBal2Acct     AS CHAR    NO-UNDO. /* �����ᮢ� ��� 2-�� ���浪� */

   DEF BUFFER loan FOR loan.

   RUN RE_B_LOAN (iContract, iContCode, BUFFER loan). 

    pick-value = ?.
   vBal2Acct  = ?.
   RUN GetDBITerm(loan.open-date,
                  loan.end-date,
                    loan.contract,
                    loan.cust-cat,
                        OUTPUT vTerm).

    RUN DTCust(loan.cust-cat, loan.cust-id, ?, OUTPUT DtCust).

    ASSIGN 
       DTType = GetXAttrInit(loan.class, "DTType")
       DTKind = GetXAttrInit(loan.class, "DTKind")
       DTTerm = Entry(3, vTerm,"/").

    IF DTType = ? OR DTType = "" THEN DTType = "*".
    IF DTKind = ? OR DTKind = "" THEN DTKind = "*".
    IF DTTerm = ? OR DTTerm = "" THEN DTTerm = "*".

   FIND FIRST code WHERE code.class = "����焮�"
                     AND code.code  = iType
      NO-LOCK NO-ERROR.
   IF NOT AVAIL code THEN
      RUN Fill-SysMes("",
                      "",
                      0,
                      "�� ������ ��� ��� " + code.code
                     ).

    ASSIGN
        mask-internal = code.code + CHR(1) +   
                        DTType + CHR(1) +
                        DTCust + CHR(1) +
                        DTKind + CHR(1) +
                        DTTerm
        s = "".

    FOR EACH code WHERE code.class = "DTTerm" AND code.parent = "DTTerm"
      NO-LOCK:
       IF IS-Term(loan.open-date,
                  ( IF loan.end-date = ? THEN
                      12/31/9999
                   ELSE
                      loan.end-date),
                   code.code,
                   NO,
                   0,
                   OUTPUT yy,
                   OUTPUT dd)
       THEN
          {additem.i s code.code}

    END. /*FOR*/

    ASSIGN
       ENTRY(5,mask-internal,CHR(1)) = s
       mask = mask-internal
      .

   RUN cbracct.p("DecisionTable", "DecisionTable", "DecisionTable", -1).

   IF pick-value <> ? AND pick-value <> "" AND
      CAN-FIND( bal-acct WHERE bal-acct.bal-acct = INT(TRIM(pick-value)) )
   THEN
   DO:
      vBal2Acct = TRIM(pick-value).
      RUN SetSysConf in h_base ("���������� ����", vBal2Acct).
   END.

END PROCEDURE.

/*-------------------------------------------------------------------------
  ��楤�� �����頥� �㬬� ���ᯥ祭�� �� ��������
  -------------------------------------------------------------------------*/
PROCEDURE SummObesp:
   DEFINE INPUT   PARAM iContract  AS CHARACTER    NO-UNDO. /* ������� */
   DEFINE INPUT   PARAM iContCode  AS CHARACTER    NO-UNDO. /* ������� */
   DEFINE INPUT   PARAM iTypeObesp AS CHARACTER    NO-UNDO. /* ⨯ ���ᯥ祭�� */
   DEFINE INPUT   PARAM iDate      AS DATE         NO-UNDO. /* ��� ����� */
   DEFINE OUTPUT  PARAM oSumm      AS INT64 INIT 0 NO-UNDO. /* �㬬� ���ᯥ祭�� */
   
   DEFINE BUFFER gar-loan FOR loan.
   DEFINE BUFFER term-obl FOR term-obl.

   DEFINE VARIABLE vTypeShDog        AS CHARACTER   NO-UNDO. /* ��ப� ��ࠬ��஢ */
   DEFINE VARIABLE vParam        AS CHARACTER   NO-UNDO. /* ��ப� ��ࠬ��஢ */
   DEFINE VARIABLE vParamCode    AS CHARACTER   NO-UNDO. /* ��ப� ��ࠬ��஢ */
   DEFINE VARIABLE vSummTmp      AS DEC         NO-UNDO.
   DEFINE VARIABLE vnn           AS CHAR        NO-UNDO.
   DEFINE VARIABLE vTmp          AS INT64       NO-UNDO.
   DEFINE VARIABLE vTmp1         AS DEC         NO-UNDO.
   DEFINE VARIABLE vTmp2         AS DEC         NO-UNDO.
   DEFINE VARIABLE vDogObesp     AS LOG INIT NO NO-UNDO.
   DEFINE VARIABLE vterm-obl-sur AS CHARACTER   NO-UNDO. /* ��ப� ��ࠬ��஢ */

   vParam = GetCode("��ࠬ����",iTypeObesp).
   
/* ��室�� ���ᯥ祭�� */
   FOR EACH term-obl WHERE 
            term-obl.contract   EQ iContract
        AND term-obl.cont-code  EQ iContCode
        AND term-obl.idnt       EQ 5  
        AND ((term-obl.sop-date NE ? 
          AND term-obl.sop-date GE iDate) 
          OR (term-obl.sop-date EQ ? 
          AND term-obl.end-date GE iDate))
        AND term-obl.cont-type  EQ iTypeObesp
   NO-LOCK:
        
      IF iDate GT DATE(GetXattrValueEx("term-obl", 
                                       iContract + "," + iContCode + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),
                                       "��⠏���",
                                       "" )) THEN
      DO:
         /* �饬 �������騩 ������� ���ᯥ祭�� */
         FIND FIRST gar-loan WHERE 
                    gar-loan.contract  EQ "�।��" 
                AND gar-loan.cont-code EQ term-obl.lnk-cont-code
         NO-LOCK NO-ERROR.
         IF NOT AVAIL gar-loan THEN 
             FIND FIRST gar-loan WHERE 
                        gar-loan.contract  EQ "�����" 
                    AND gar-loan.cont-code EQ term-obl.lnk-cont-code
             NO-LOCK NO-ERROR.

         IF AVAILABLE gar-loan THEN
         DO:
            vDogObesp = YES.
            RUN RE_PARAM (INT64(ENTRY(1,vParam)), /* ��� ��ࠬ��� ������� */
                          iDate,                  /* ��� ���� ������� */
                          gar-loan.contract,      /* �����祭�� ������� */
                          gar-loan.cont-code,     /* ����� ������� */
                          OUTPUT vSummTmp,
                          OUTPUT vTmp1,
                          OUTPUT vTmp2).
            oSumm = oSumm + CurToBase ("�������",term-obl.currency,iDate,vSummTmp).  
         END.
      END.
   END.
   /* �᫨ ������� ���ᯥ祭�� �� ������ */
   IF NOT vDogObesp THEN
      FOR EACH term-obl WHERE 
               term-obl.contract   EQ iContract
           AND term-obl.cont-code  EQ iContCode
           AND term-obl.idnt       EQ 5  
           AND ((term-obl.sop-date NE ? 
           AND term-obl.sop-date GE iDate) 
           OR (term-obl.sop-date EQ ? 
           AND term-obl.end-date GE iDate))
      NO-LOCK:
         vterm-obl-sur = iContract + "," + iContCode + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn).
         vnn = GetXAttrValueEx ("term-obl", vterm-obl-sur, "�������", "").
         vTypeShDog = GetXAttrValueEx ("term-obl", vterm-obl-sur, "��������", "") + IF  INT(vnn) EQ 0 THEN "" ELSE vnn.
         IF vTypeShDog BEGINS iTypeObesp 
         THEN DO:
            FOR FIRST code WHERE code.parent EQ vTypeShDog
                             AND code.class  EQ "����焮�"
            NO-LOCK:
                         
               vParamCode = code.code.            
                 /*���� ��� ᨬ���� �� ��, ��� �� - �� �㦭�*/
               SUBSTRING(vParamCode,1,2) = "".
               FOR FIRST chowhe WHERE chowhe.id-op EQ INT(vParamCode)
               NO-LOCK:
                   vTmp = IF chowhe.id-k EQ ? THEN chowhe.id-d ELSE chowhe.id-k.
                   IF LOOKUP(STRING(vTmp),vParam) NE 0
                   THEN DO:
                      
                      RUN RE_PARAM (vTmp, /* ��� ��ࠬ��� ������� */
                                    iDate,                                                        /* ��� ���� ������� */
                                    iContract,                                                    /* �����祭�� ������� */
                                    iContCode,                                                    /* ����� ������� */
                                    OUTPUT vSummTmp,
                                    OUTPUT vTmp1,
                                    OUTPUT vTmp2).
                      oSumm = oSumm + CurToBase ("�������",term-obl.currency,iDate,vSummTmp).  
                   END.
                END.
            END.
         END.
      END.
END PROCEDURE /* SummObesp */.

/* ����祭�� ����஥� �������㠫��� �����ᨩ */
procedure GetIndComm:
    def input  param iContract as char no-undo.
    def input  param iContCode as char no-undo.
    DEF output param oIdk      AS CHAR NO-UNDO.     /* ��ࠬ���� ᯨᠭ�� ��業⮢ */
    DEF output param oIddk     AS CHAR NO-UNDO.     /* ��ࠬ���� ���᫥��� ��業⮢ */
    DEF output param oIdDop    AS CHAR NO-UNDO.     /* �������⥫�� (3-�) ��ࠬ��� */
    DEF output param oCommSpec AS CHAR NO-UNDO.     /* ��� �����ᨨ */
    DEF output param oCalcBack AS CHAR NO-UNDO.     /* ��ࠬ��� �ய�᪠ �� ������ ����� */
    DEF output param oSchemCls AS CHAR NO-UNDO.     /* ����� ��� "�奬��犮�" */
        
    DEF VAR vTmp      AS CHAR NO-UNDO.
    
    def buffer loan  for loan.
    def buffer code  for code.
    def buffer bcode for code.
    
    FIND FIRST loan WHERE 
               loan.contract  EQ iContract 
       AND     loan.cont-code EQ iContCode 
    NO-LOCK NO-ERROR.
    IF NOT AVAIL loan THEN
       RETURN.
    
       /* ��।���� �� ������ ������ ��c�ன�� � "�奬��犮�" */
   {shemnach.i &mSchemCls = oSchemCls &loan = loan}
    
       /* ���������� ��६����� �� �����䨪��� ��犮� */
    FOR EACH bcode WHERE bcode.class  EQ "�奬��犮�" 
                     AND bcode.parent EQ oSchemCls + ":�奬��犮�" 
    NO-LOCK,
       EACH code WHERE 
             code.class EQ "��犮�"
         AND code.code  EQ ENTRY(1,bcode.code,":")
    NO-LOCK:
       {additem.i oCommSpec code.code}  /* ��� �����ᨨ */
       vTmp = ENTRY(1, code.val).       
       {additem.i oIddk vTmp}           /* 1-� ��ࠬ��� */ 
       vTmp = ENTRY(2, code.val).       
       {additem.i oIdk vTmp}            /* 2-� ��ࠬ��� */
       vTmp = IF NUM-ENTRIES(code.val) GE 3 THEN ENTRY(3, code.val) ELSE "-1".
       {additem.i oIdDop vTmp}          /* 3-� ��ࠬ��� */
       vTmp = IF GetCodeMiscEx("�奬��犮�", code.code + ":" + oSchemCls, 3, "���") EQ "��" 
                 THEN "��" 
                 ELSE "���".
       {additem.i oCalcBack vTmp}       /* ���᮪ 䫠���� ���� ����� */
    END.
       /* ��-㬮�砭��: */
    IF oCommSpec EQ "" THEN
       ASSIGN
          oCommSpec = "�����,%��,%������,%���"
          oIddk     = "171,173,175,177"
          oIdk      = "172,174,176,178"
          oIdDop    = "-1,-1,-1,-1"
          oCalcBack = "���,���,���,���".
   /* eof ���������� ��६����� �� �����䨪��� ��犮� */
end procedure.

PROCEDURE ExtendAcctFromLoan :
DEFINE INPUT PARAMETER  iContract as char no-undo.
DEFINE INPUT PARAMETER  iContCode as char no-undo.
DEFINE INPUT PARAMETER  idate     AS DATE NO-UNDO .
DEFINE INPUT PARAMETER  iOrder    AS INT64 NO-UNDO .
DEFINE OUTPUT PARAMETER oAcct AS CHARACTER NO-UNDO .

   DEF VAR vStr AS CHAR NO-UNDO.
   DEF VAR vNameDR AS CHAR NO-UNDO.

   {&GET-LOAN}

   vStr = GetXattrInit(loan.class-code,"���ᄮ����").
   IF Num-entries(vstr) LT iOrder THEN DO:
     oAcct = "" .
     RETURN.
   END.
   vNameDR = ENTRY(iOrder,vstr).
   oAcct = GetTempXAttrValueEx("loan",iContract + "," + iContCode,vNameDR,idate,"").  /* ��६ ���祭�� ��� �� ⥬���஢������ �� */
   IF oAcct EQ "" THEN
      oAcct = GetXAttrValueEx("loan",iContract + "," + iContCode,vNameDR,"").         /* �������� �� ����� ���⮩ �� , �஢�ਬ � ��� */

END PROCEDURE. /* ExtendAcctFromLoan */

/* ��楤�� ����砥� ���ᨬ���� ���浪��� ����� ��䨪� � ���ਨ 
   ��䨪�� (�ᯮ���� ���४ NumGr �� term-obl-hist) */

PROCEDURE GetMaxNumGraphHist:
   DEF INPUT  PARAM  iContract   AS CHAR NO-UNDO. 
   DEF INPUT  PARAM  iCont-code  AS CHAR NO-UNDO. 
   DEF OUTPUT PARAM  oMaxNGr     AS INT64 NO-UNDO.

   DEF VAR vMaxNGr AS INT64 NO-UNDO.
   DEF VAR vNGr    AS INT64 NO-UNDO.
                     
   vMaxNGr = 0.
   FOR EACH term-obl-hist WHERE
            term-obl-hist.contract  EQ iContract
        AND term-obl-hist.cont-code EQ iCont-code
   NO-LOCK:
      vNGr = INT64(GetXattrValueEx("term-obl-hist",
                                    STRING(term-obl-hist.tobl-id),
                                    "NumGr",
                                    "")).
      IF vNGr GT vMaxNGr THEN vMaxNGr = vNGr.
   END.

   oMaxNGr = vMaxNGr.

END PROCEDURE.

   /* ��।������ �㬬� ��ࢮ��砫쭮�� ����� */
PROCEDURE GetFirstInvest.
   DEF INPUT  PARAM iAcctCred  AS CHAR NO-UNDO.    /* ��� �������  */
   DEF INPUT  PARAM iCurrency  AS CHAR NO-UNDO.    /* ����� ����樨 */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* ��� */
   DEF INPUT  PARAM iOpStatus  AS CHAR NO-UNDO.    /* ����� ����樨 */
   DEF OUTPUT PARAM oAmnt      AS DEC  NO-UNDO.    /* �㬬� ����� */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO.    /* ��� ��ࢮ��砫쭮�� ����� */

   DEF BUFFER op-entry FOR op-entry.
   
   FOR EACH op-entry WHERE 
            op-entry.acct-cr   EQ iAcctCred
      AND   op-entry.op-date   LE iDate
      AND   op-entry.op-status GE iOpStatus
      AND   op-entry.currency  EQ iCurrency  
      AND   op-entry.op        NE ?  
   NO-LOCK
   BY op-entry.op-date:
      IF oDate EQ ? THEN
         oDate = op-entry.op-date.
      IF oDate EQ op-entry.op-date THEN
         oAmnt = oAmnt + ( IF iCurrency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
      ELSE
         LEAVE.
   END.
END PROCEDURE.

   /* ��।������ ���� ����㯫���� �ᥩ �㬬�. */
PROCEDURE GetDateAllFirstInvest.
   DEF INPUT  PARAM iAcctCred  AS CHAR NO-UNDO.    /* ��� �������  */
   DEF INPUT  PARAM iCurrency  AS CHAR NO-UNDO.    /* ����� ����樨 */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* ��� */
   DEF INPUT  PARAM iOpStatus  AS CHAR NO-UNDO.    /* ����� ����樨 */
   DEF INPUT  PARAM iSumm      AS DEC  NO-UNDO.    /* ��� */
   DEF OUTPUT PARAM oAmnt      AS DEC  NO-UNDO.    /* �㬬� ����� */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO.    /* ��� ��ࢮ��砫쭮�� ����� */

   DEF BUFFER op-entry FOR op-entry.

   FOR EACH op-entry WHERE
            op-entry.acct-cr   EQ iAcctCred
      AND   op-entry.op-date   LE iDate
      AND   op-entry.op-status GE iOpStatus
      AND   op-entry.currency  EQ iCurrency
      AND   op-entry.op        NE ?
   NO-LOCK
   BY op-entry.op-date:
      oAmnt = oAmnt + ( IF iCurrency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
      IF iSumm <= oAmnt 
      THEN DO:
         oDate = op-entry.op-date.
         LEAVE.
      END.
   END.
END PROCEDURE.

/* ��� ��砫쭮�� �襭�� �� �।�⠬ */
PROCEDURE GetDateNRKred.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO. /* ��� �� 㬮�砭��, �᫨ �� �������� �� ��⠍��। */
   DEF OUTPUT PARAM oDate AS DATE NO-UNDO. /* ��� ��砫쭮�� �襭�� �� �।�⠬ */

   oDate = DATE(FGetSettingEx("��⠍��।",?,?,NO)).
   IF oDate = ? THEN 
      oDate = iDate.
END PROCEDURE.

/* ��楤�� ��࠭�� ⥪�騩 ��䨪 ����襭�� �� �������� � term-obl-hist */
PROCEDURE SaveGraphToHistory:
   DEF INPUT  PARAM iContract    AS CHAR    NO-UNDO. 
   DEF INPUT  PARAM iCont-code   AS CHAR    NO-UNDO.
   DEF INPUT  PARAM iSince       AS DATE    NO-UNDO. 
   DEF INPUT  PARAM iChType      AS CHAR    NO-UNDO.
   DEF INPUT  PARAM iDescription AS CHAR    NO-UNDO.
   DEF INPUT  PARAM iSigned      AS LOGICAL NO-UNDO.
   DEF INPUT  PARAM iSignDate    AS DATE    NO-UNDO.
   DEF INPUT  PARAM iOlap        AS LOGICAL NO-UNDO.

   DEF OUTPUT PARAM oError       AS LOGICAL NO-UNDO.

   DEF VAR vMaxNGr   AS INT64 NO-UNDO.
   DEF VAR vI        AS INT64 NO-UNDO.
   DEF VAR vIdnt     AS INT64 NO-UNDO.
   DEF VAR vTypeIdnt AS CHAR  NO-UNDO.

   DEF BUFFER loan FOR loan.
   
   /* �᫨ � 㪠���� �� �� �㦭� ��࠭��� ��䨪, ��� � ����஥筨�� 㪠���� �� ��� ������� ���� ���㦭� ��࠭��� ��䨪 */
   IF  GetSysConf("�����࠭����䨪����襭�����") EQ "��" 
    OR FGetSetting ("�଑����", iChType,"���") NE "��" THEN 
      /* � ��室��, ��祣� �� ����� */
      RETURN.
   
   oError = FALSE.

   FIND FIRST loan WHERE
              loan.contract  EQ iContract
          AND loan.cont-code EQ iCont-code
   NO-LOCK NO-ERROR.

   IF AVAIL loan THEN
   DO:
      vTypeIdnt = FGetSetting ("�����", "��䒨�","1,2,3").

      /* ��।���� ���ᨬ���� ���浪��� ����� ��䨪� �।� 㦥 ��࠭����� 
         ��䨪�� � ��६����� vMaxNGr. ����騩 ��䨪 㦥 �㤥� ��࠭��� � 
         ���浪��� ����஬ (vMaxNGr + 1) */
   
      RUN GetMaxNumGraphHist (loan.contract,
                              loan.cont-code,
                              OUTPUT vMaxNGr).
   
      DO vI = 1 TO NUM-ENTRIES(vTypeIdnt):
         vIdnt = INT64(ENTRY(vI,vTypeIdnt)) .
         RUN SetTermOblHistEx (
                             iContract
                            ,iCont-Code
                            ,iSince
                            ,vIdnt
                            ,vMaxNGr + 1
                            ,iChType 
                            ,iDescription
                            ,iSigned
                            ,iSignDate
                            , ( IF iOlap THEN "1" ELSE "" )
                            , OUTPUT oError ).

      END.
   END.
   ELSE
      oError = TRUE.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:09:34.512+04:00' */
/* $LINTFILE='pp-loan.p' */
/*prosigne+Gc6KAp1Es5RNxsZRerOQ*/
/* --- pp-loan.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am --- */
