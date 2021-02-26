/* ��ନ஢���� ���� ���᫥���.
   ��६ �� ���. ४����� ������� � �᫨ ���, � �� ��砫쭮�� ���祭��
   ४�����.  ���祭��(���� ��ࠬ��஢) �࠭���� �१ �������,����᪠����
   㪠�����  ��᪮�쪨� ��ࠬ��஢ �१ "+"
   ����� 㪠�뢠�� ��᪮�쪮 ��㯯 ��ࠬ��஢ ࠧ�������� "&" �� �⮬ १-��
   ���᫥��� �㤥� �㬬� ���᫥��� �� ������ ��㯯� ��ࠬ��஢ �� �⠢�� ��
   ४����� �����⠢, ��� 㪠�뢠���� �⠢�� ��� ������ ��㯯� �१ "&.
   ���� ���ᨬ��쭮 10 ��㯯.

   ������� - 0+7,....,0&7+13
   �����⠢ = %�।,..,%�����&%����
*/

DEF BUFFER loan      FOR loan.
DEF BUFFER curloan   FOR loan.

DEF TEMP-TABLE ttsh NO-UNDO
   FIELD since    AS DATE
   FIELD Summa    AS DEC
INDEX since since.

PROCEDURE GetCalcCharNew.

   DEF INPUT  PARAM iContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iCodPar       AS INT64  NO-UNDO. /* ��� ��ࠬ��� */
   DEF OUTPUT PARAM oCurrCalcChar AS CHAR NO-UNDO. /* ���� ���᫥��� */
   DEF OUTPUT PARAM oNumGrups     AS INT64  NO-UNDO.

   DEF VAR vCalcString   AS CHAR NO-UNDO. /* ��ப� ��� ��।������ ����
                                          ���᫥��� �࠭���� � ���. ४�����
                                          ������� ��� � ��砫쭮� ���祭��
                                          ��� ����� � ����奬� */

   vCalcString = GetXattrValueEx("loan",
                                 iContractChar + "," + iContCodeChar,
                                 "�������","").
   IF vCalcString = "" THEN
      vCalcString = GetXattrInit(loan.class-code,"�������").

   vCalcString = ENTRY(iCodPar,vCalcString) NO-ERROR.

   IF NOT ERROR-STATUS:ERROR THEN
      vCalcString = REPLACE(vCalcString,"+",",") NO-ERROR.

   IF NOT ERROR-STATUS:ERROR AND
      vCalcString <> ""      AND
      vCalcString <> ?       THEN
      oCurrCalcChar = vCalcString.
   ELSE
      oCurrCalcChar = IF iCodPar EQ 1 THEN
                         "0,7,13"
                      ELSE
                         STRING(most[iCodPar]).

   oNumGrups = NUM-ENTRIES(oCurrCalcChar,"&").
END PROCEDURE. /*GetCalcChar*/

/* �����頥� ��� ��業⭮� �⠢��.
** ������ ���, ������ ��訢����� � ����ன��� ��.
** ��� ࠡ��� ������ ��楤��� ����室�� ������ svarloan.def */
PROCEDURE GetCodeRateNew.

   DEF INPUT  PARAM iPrmInt   AS INT64  NO-UNDO. /* ���浪��� ����� %%, ����
                                                loan.interest[cod-par] */
   DEF INPUT  PARAM iNumGrups AS INT64  NO-UNDO. /* ��᫮ ��㯯 � ���� ���� */
   DEF OUTPUT PARAM oRateChar AS CHAR NO-UNDO. /* ��� ���/�����ᨨ */

   DEF PARAM  BUFFER loan FOR loan.

   DEF VAR vCommChar    AS CHAR
                          INIT "1,2,3,4,5,2,3,4,5,6,4,5,7,8"
                          NO-UNDO. /* ���ᨢ �裡 ��業⮢ � �����ᨩ */
   DEF VAR vRateString  AS CHAR NO-UNDO.
   DEF VAR vCounter     AS INT64  NO-UNDO.

   /*��६ ��ப� ��業��� �⠢�� �� ���. ४����� �������
    � �᫨ ���, � �� ��砫쭮�� ���祭�� ४�����.*/

   ASSIGN
      vRateString = GetXattrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "�����⠢","")
      oRateChar = lrate [lr-st + INT64(ENTRY(iPrmInt, vCommChar)) - 1]
      .

   IF vRateString = "" THEN
      vRateString = GetXattrInit(loan.class-code,"�����⠢").

   /*�᫨ ��ப� ���� � �������� ����� �� �⮩ ��ப� �����
    � ����஬ cod-par, � �� � �㤥� ��業⭮� �⠢���...*/
   IF vRateString <> ?  AND
      vRateString <> "" THEN
   DO:
      oRateChar = ENTRY(iPrmInt,vRateString) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         oRateChar = lrate [lr-st + INT64(ENTRY(iPrmInt, vCommChar)) - 1].
   END.

   IF NUM-ENTRIES(oRateChar,"&") < iNumGrups THEN
   DO vCounter = NUM-ENTRIES(oRateChar,"&") + 1 TO iNumGrups:
      oRateChar = oRateChar + "&"
                  + lrate [lr-st + INT64(ENTRY(iPrmInt, vCommChar)) - 1].
   END.

END PROCEDURE.
/* �⥫��� �� �ᯮ�짮���� ���� ���������� ��� ����祭�� ���� �������
   �� ������ � %5 �⠢�� , �� �� ��졠 - � 㪠������ ��ࠬ���*/
PROCEDURE GetCodeTax.

   DEF INPUT  PARAM iPrmInt   AS INT64  NO-UNDO. /* ���浪��� ����� %%, ����
                                                loan.interest[cod-par] */
   DEF INPUT  PARAM iNumGrups AS INT64  NO-UNDO. /* ��᫮ ��㯯 � ���� ���� */
   DEF OUTPUT PARAM oRateChar AS CHAR NO-UNDO. /* ��� ���/�����ᨨ */

   DEF PARAM  BUFFER loan FOR loan.

   DEF VAR vRateString  AS CHAR NO-UNDO.
   DEF VAR vCounter     AS INT64  NO-UNDO.

   /*��६ ��ப� ��業��� �⠢�� �� ���. ४����� �������
    � �᫨ ���, � �� ��砫쭮�� ���祭�� ४�����.*/

   ASSIGN
      vRateString = GetXattrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "���������","")
       .

    IF vRateString = "" THEN
       vRateString = GetXattrInit(loan.class-code,"���������").

   /*�᫨ ��ப� ���� � �������� ����� �� �⮩ ��ப� �����
    � ����஬ cod-par, � �� � �㤥� ��業⭮� �⠢���...*/
   IF vRateString <> ?  AND
      vRateString <> "" THEN
   DO:
      oRateChar = ENTRY(iPrmInt,vRateString) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         oRateChar = ?.
   END.
   ELSE oRateChar = ? . /* ��� ������� ������� */
END PROCEDURE .

/* ����祭�� �������� ���⪠ �� ��ࠬ��ࠬ �������.
** ����� ��砩 - ����祭�� ���祭�� �� ������ ��ࠬ����. */
PROCEDURE GET_REM_BY_PRM_NEW.

   DEF INPUT PARAM iContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM iContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM iPrmChar      AS CHAR NO-UNDO. /* ��ࠬ���� ��� ����
                                                                ���⪠. */
   DEF INPUT PARAM iBegDate      AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM iEndDate      AS DATE NO-UNDO. /* "��" ����砭�� ���ࢠ��*/
   DEF INPUT PARAM iCodPar       AS INT64  NO-UNDO. /* ��� ��ࠬ���*/
   DEF INPUT PARAM iCurrency     AS CHAR NO-UNDO. /* �����*/
   DEF INPUT PARAM iSince        AS DATE NO-UNDO. /* ��� ���ﭨ� ������� */
   DEF INPUT PARAM iRecalcLoan   AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP     AS LOG  NO-UNDO.
   DEF INPUT PARAM iForTransh    AS LOG  NO-UNDO.

   DEF VAR vChgDate     AS DATE NO-UNDO. /* ��� ����������� ��ଥ�� */
   DEF VAR vChgDate1    AS DATE NO-UNDO. 
   DEF VAR vChgDate2    AS DATE NO-UNDO. 
   DEF VAR vCurrPrmInt  AS INT64  NO-UNDO. /* i-� ��ࠬ��� */
   DEF VAR vTotalPrmDec AS DEC  NO-UNDO. /* �㬬� ��ଥ�஢ �� ���� */
   DEF VAR vPrmValDec   AS DEC  NO-UNDO. /* �㬬� i-��� ��ࠬ��� */
   DEF VAR vDbDec       AS DEC  NO-UNDO. /* �㬬� ����⮢�� ����⮢ */
   DEF VAR vCrDec       AS DEC  NO-UNDO. /* �㬬� �।�⮢�� ����⮢ */
   DEF VAR vPrmCharBase AS CHAR NO-UNDO. /* ��ப� ��ࠬ��஢ "," ����� "&" */
   DEF VAR vPrmChar     AS CHAR NO-UNDO. /* ��ࠬ���� � ⥪�饩 ��㯯� */
   DEF VAR vGrupCounter AS INT64  NO-UNDO. /* ���稪 ��㯯 */
   DEF VAR vOsnDolg     AS LOG  NO-UNDO. /* ��ࠡ�⪠ ᨬ���� "�" */
   DEF VAR vCurrentEndDate AS DATE NO-UNDO.
   
   DEF BUFFER xloan FOR loan. /* ���������� ����� */
   
   vPrmCharBase = REPLACE(iPrmChar,"&",",").
   
   FIND FIRST term-obl WHERE 
          term-obl.cont-code EQ iContCodeChar 
      AND term-obl.contract  EQ iContractChar 
      AND term-obl.idnt      EQ 2 
   NO-LOCK NO-ERROR.
  
  /* ��ࠡ�⪠ �� ஫� �����ন���� ⮫쪮 ���� ஫� � ᯨ᪥
     ���� �� ��ࠡ��뢠�� + � & */  
   IF iPrmChar MATCHES "*����*" THEN
   DO:
      RUN GET-OST-ACCT (iPrmChar,
                        RECID(curloan),
                        iBegDate,
                        iEndDate,
                        1).
   END.
   ELSE DO :
   /* ���������� � 㪠����� ���ࢠ� ��� */
   DO WHILE iBegDate LE iEndDate:
/* �뢥�� �� �⤥�쭮� ����ன��, 
   ���ਬ�� "��" - ������ "�", ⮫쪮 �� ⥪��.
   FIND LAST term-obl WHERE 
          term-obl.cont-code EQ iContCodeChar 
      AND term-obl.contract  EQ iContractChar 
      AND term-obl.idnt      EQ 2 
      AND term-obl.end-date  LE iBegDate
   NO-LOCK NO-ERROR.
*/
      /* ����祭�� ᫥���饩 ���� ��������� ��ࠬ��� */
      RUN GetChgDateParam (iContractChar,
                        iContCodeChar,
                        vPrmCharBase,      /* ���᮪ ��������� ��ࠬ��஢ */
                        iBegDate - IF vIshOst THEN 0 ELSE 1, /* �� ����� ���� ᬮ���� */
                        NOT iRecalcPP,
                        OUTPUT vChgDate1).
      IF iContractChar EQ "�����" 
      THEN
         vChgDate2 = GET_NEXT_COMM_DATE_LOAN (iContractChar ,
                                              iContCodeChar ,
                                              "���ᄥ�",
                                              iBegDate,
                                              iEndDate).
      vChgDate = IF vChgDate2 EQ ? 
                 THEN vChgDate1 
                 ELSE IF vChgDate1 EQ ?
                 THEN vChgDate2 
                 ELSE MIN(vChgDate1,vChgDate2).
 
      IF iCodPar = 1                   
         AND LOOKUP(STRING(CodOstPar),vPrmCharBase) <> 0 
         AND NOT iRecalcLoan               
         AND NOT iRecalcPP THEN
            RUN CORR_NEXT_DATE (iBegDate - IF vIshOst THEN 0 ELSE 1 ,
                   INPUT-OUTPUT vChgDate).
      IF vChgDate EQ ? THEN vChgDate = iEndDate + 1.
      /* ���४�஢�� ���� ⥪�饣� ���ࢠ�� */
      vCurrentEndDate = IF vChgDate GT iEndDate THEN
                           iEndDate + 1
                        ELSE
                           vChgDate + IF vIshOst THEN 0 ELSE 1. /* ���/�� ���⮪ */
      DO vGrupCounter = 1 TO NUM-ENTRIES (iPrmChar,"&"):
         ASSIGN
            vPrmChar     = ENTRY(vGrupCounter,iPrmChar,"&")
            vTotalPrmDec = 0
         .
         
         DO vCurrPrmInt = 1 TO NUM-ENTRIES (vPrmChar):
            IF ENTRY(vCurrPrmInt, vPrmChar) MATCHES "*�*"  THEN
               ASSIGN
                  vPrmChar = REPLACE(vPrmChar,"�","")
                  vOsnDolg = YES
               .
  
            IF INT64 (ENTRY(vCurrPrmInt, vPrmChar)) = CodOstPar 
                   AND iCodPar = 1                                    
                   AND NOT iRecalcLoan                                
                   AND NOT iRecalcPP THEN
            DO:
               IF INT64(ENTRY(vCurrPrmInt, vPrmChar)) = 0  THEN
               DO:
                  RUN PARAM_0_NEW IN h_Loan
                         (iContractChar,
                         iContCodeChar,    /* ����� ������� */
                         INT64 (ENTRY(vCurrPrmInt, vPrmChar)), /* ��� ��ࠬ��� */
                         iBegDate - IF vIshOst THEN 0 ELSE 1,  /* ��� ���� ��ࠬ��� */
                         OUTPUT vPrmValDec).     /* ���祭�� ��ࠬ��� �� ���� ������ ������� */
                               
               END.
               ELSE
               DO:
                  RUN RE_PARAM IN h_Loan
                                 (INT64 (ENTRY(vCurrPrmInt, vPrmChar)),
                                 iBegDate - IF vIshOst THEN 0 ELSE 1, /* ��/��� ���⮪ */
                                 iContractChar,
                                 iContCodeChar,
                                 OUTPUT vPrmValDec,
                                 OUTPUT vDbDec,
                                 OUTPUT vCrDec).
             
               END.

               FIND LAST ttCorr WHERE 
                           ttCorr.end-date <= iBegDate - 1 
               NO-LOCK NO-ERROR.
               vPrmValDec = IF AVAIL ttCorr THEN 
                               vPrmValDec - ttCorr.Corr 
                            ELSE 
                               vPrmValDec.
            END.
            ELSE
            DO: 
               IF INT64(ENTRY(vCurrPrmInt, vPrmChar)) = CodOstPar 
                       AND iCodPar = 1                           
                       AND iRecalcPP THEN
                  RUN GetFilOstSumm(iBegDate - IF vIshOst THEN 0 ELSE 1,
                                    CodOstPar,
                             OUTPUT vPrmValDec).
               ELSE
               DO:
                  IF NOT vOsnDolg THEN
                     RUN STNDRT_PARAM IN h_Loan
                                     (iContractChar,
                                      iContCodeChar,
                                      INT64 (ENTRY(vCurrPrmInt, vPrmChar)),
                                      iBegDate - IF vIshOst THEN 0 ELSE 1, /* ��/��� ���⮪ */
                                      OUTPUT vPrmValDec,
                                      OUTPUT vDbDec,
                                      OUTPUT vCrDec).
                  ELSE
                  DO:
                     vTotalPrmDec = IF AVAIL term-obl THEN 
                                       term-obl.amt-rub 
                                     ELSE 0.
                  END.
               END.
            END.
            vTotalPrmDec = vTotalPrmDec + vPrmValDec.
            vOsnDolg = NO.
         END.

         IF iContractChar EQ "�����" THEN
         DO:
            RUN RE_B_LOAN IN h_Loan (iContractChar,
                                     iContCodeChar,
                                     BUFFER xloan).

            RUN base-sum-correct (iContractChar,
                                  iContCodeChar,
                                  xloan.currency,
                                  iBegDate,
                                  ENTRY(vGrupCounter,GetXAttrInit(xloan.class-code,"�����⠢"),"&"),
                                  INPUT-OUTPUT vTotalPrmDec).
         END.
         /* ���४�஢�� ���⪠ */ 
         IF iForTransh THEN
            RUN SetTranshSum (iContCodeChar,iBegDate,vTotalPrmDec,vGrupCounter).
         ELSE
            RUN SetLnShPrm(iBegDate,?,?,iCurrency,?,vTotalPrmDec,?,?,?,?,vGrupCounter,?,?).
      END. 
      iBegDate = vCurrentEndDate.
   END. /* ����      */
   END. /* ��ࠬ���� */
   RETURN.
END PROCEDURE.

/* �㬬��㥬 䠪��᪨� ���⪨ �� �࠭蠬 */
PROCEDURE GET_REM_BY_PRM_TR.
   DEF INPUT PARAM iContractChar AS CHAR NO-UNDO. /* �����祭�� ��. ������� */
   DEF INPUT PARAM iContCodeChar AS CHAR NO-UNDO. /* ����� ��. ������� */
   DEF INPUT PARAM iPrmChar      AS CHAR NO-UNDO. /* ��ࠬ���� ��� ����
                                                                ���⪠. */
   DEF INPUT PARAM iBegDate      AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM iEndDate      AS DATE NO-UNDO. /* "��" ����砭�� ���ࢠ��*/
   DEF INPUT PARAM iCodPar       AS INT64  NO-UNDO. /* ��� ��ࠬ���*/
   DEF INPUT PARAM iCurrency     AS CHAR NO-UNDO. /* �����*/
   DEF INPUT PARAM iSince        AS DATE NO-UNDO. /* ��� ���ﭨ� ������� */
   DEF INPUT PARAM iRecalcLoan   AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP     AS LOG  NO-UNDO.
   
   FOR EACH curloan WHERE curloan.contract  EQ     iContractChar
                      AND curloan.cont-code BEGINS iContCodeChar + " " 
                      AND NUM-ENTRIES(curloan.cont-code, " ") EQ 2 
                      AND curloan.open-date LE iEndDate
                      AND (   curloan.close-date EQ ?
                           OR curloan.close-date GE iBegDate)
       NO-LOCK:

      RUN GET_REM_BY_PRM_NEW (curloan.contract,
                              curloan.cont-code,
                              iPrmChar,  /* ���� ���᫥��� */
                              iBegDate,
                              iEndDate,
                              iCodPar,
                              iCurrency,
                              iSince,
                              iRecalcLoan,
                              iRecalcPP,
                              YES).
   END.
   RETURN.
END PROCEDURE.

/* �㬬��㥬 ������� ���⪨ �� �࠭蠬 */
PROCEDURE GET_REM_BY_TERM_TR.
   DEF INPUT PARAM iContractChar AS CHAR  NO-UNDO. /* �����祭�� ��. ������� */
   DEF INPUT PARAM iContCodeChar AS CHAR  NO-UNDO. /* ����� ��. ������� */
   DEF INPUT PARAM iCodGraf      AS INT64 NO-UNDO. /* ���. ��䨪� */
   DEF INPUT PARAM iBegDate      AS DATE  NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM iEndDate      AS DATE  NO-UNDO. /* "��" ����砭�� ���ࢠ��*/
   DEF INPUT PARAM iCurrency     AS CHAR  NO-UNDO. /* �����*/
   
   FOR EACH curloan WHERE curloan.contract  EQ     iContractChar
                      AND curloan.cont-code BEGINS iContCodeChar + " " 
                      AND NUM-ENTRIES(curloan.cont-code, " ") EQ 2 
                      AND curloan.open-date LE iEndDate
                      AND (   curloan.close-date EQ ?
                           OR curloan.close-date GE iBegDate)
       NO-LOCK:
         RUN GET_REM_BY_TERM (curloan.contract,
                              curloan.cont-code,
                              iCodGraf,    /* ��� ��䨪� "�������� �㬬" */
                              iBegDate,
                              iEndDate,
                              iCurrency,
                              YES).

   END.
   RETURN.
END PROCEDURE.

/* ��ନ஢���� �����ᨨ �� ����� */
PROCEDURE GET_COMM_BY_REM_NEW.

   DEF INPUT PARAM iContractChar AS CHAR  NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM iContCodeChar AS CHAR  NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM iCommChar AS CHAR NO-UNDO. /* ��� �����ᨨ */
   DEF INPUT PARAM iKauChar  AS CHAR NO-UNDO. /* ��� ��� */
   DEF INPUT PARAM iBegDate  AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO. /* "��" ����砭�� ���ࢠ�� */
   DEF INPUT PARAM iCodPar   AS INT64  NO-UNDO. /* ��� ��ࠬ���*/

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* �����䨪��� ��� */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* �����. ���� %% �⠢��/��� */
   DEF VAR vCommTypeChar AS CHAR  NO-UNDO. /* �⠢��/��� */
   DEF VAR vGrupCounter  AS INT64   NO-UNDO. /* ���稪 ��㯯*/
   DEF VAR vCommChar     AS CHAR  NO-UNDO. /* ������� � ࠬ��� ��㯯�*/
   DEF VAR vBegDate      AS DATE  NO-UNDO. /* ��६���� ��� ��।������ ���*/
   DEF VAR vBegDateT     AS DATE  NO-UNDO. /* ���� ��������� �����ᨨ �� �ᥩ
                                              ��㯯� */
   DEF VAR vMinRate      AS DEC   NO-UNDO. /* �������쭮� ���祭�� */
   DEF VAR vMaxRate      AS DEC   NO-UNDO. /* ���ᨬ��쭮� ���祭�� */

   DEF VAR vCommCur      AS CHAR  NO-UNDO. /* ����� */

   DEFINE BUFFER bLnShPrm FOR LnShPrm.
   DEFINE BUFFER b-comm-rate FOR comm-rate.
   DEFINE BUFFER bc-rate     FOR comm-rate.

   /* ��ନ஢���� �����ᨨ � ���ࢠ�� ��� */
   DO WHILE iBegDate <= iEndDate:

      /* ���� ��ࠬ��஢ �� ���� */
      FIND LAST LnShPrm WHERE
                LnShPrm.since <= iBegDate
      NO-ERROR.

      /* �᫨ ��ࠬ���� ��।�����,
      ** � ����⪠ ���᪠ ���祭�� �����ᨨ/��� */
      IF AVAIL LnShPrm THEN
      DO:

         ASSIGN
            vAcctRecid = ?
            vBegDateT  = iEndDate  + 1
            vCommCur   = LnShPrm.Currency
            .

         DO vGrupCounter = 1 TO NUM-ENTRIES(iCommChar,"&"):

            ASSIGN
               vCommChar = ENTRY(vGrupCounter,iCommChar,"&").
            FIND LAST bc-rate WHERE bc-rate.kau        EQ iKauChar
                                AND bc-rate.commission EQ vCommChar
                                AND bc-rate.since      LE iBegDate
               NO-LOCK NO-ERROR.
               /* ����祭�� �����ᨨ/��� �� ���� */
               vRateDec  = IF AVAIL bc-rate THEN bc-rate.rate-comm
                                     ELSE 0.
               /* ����祭�� ⨯�: "%,=,����" */
               vCommTypeChar = IF CAN-DO("*" + {&GCodePeny} + "*", iCommChar)
                               THEN
                                  {&GCodePeny}
                               ELSE
                               IF GET_COMM_TYPE (
                                   vCommChar,  /*��� �����ᨨ */
                                   vAcctRecid, /*�����䨪��� ��� */
                                   vCommCur,   /*��� �ਢ������� ������ */
                                   iKauChar,   /*��� ��� ("" - �� 㬮�砭��) */
                                   LnShPrm.balance[vGrupCounter], /*���������
                                                   ���⮪ (0 - ��㬮�砭��) */
                                   0,          /*��ਮ�/�ப (0-��㬮�砭��)*/
                                   iBegDate) THEN
                                  "="
                               ELSE
                                  "%"
               .


            /* ���� ��� ���������� ��� � ���� ����. */
            RUN GET_COMM_LOAN_BUF IN h_Loan (
                                   iContractChar,       /* �����祭�� ������� */
                                   iContCodeChar,       /* ����� ������� */
                                   iCommChar,           /* ��� �����ᨨ */
                                   iBegDate,            /* �� ���� */
                                   BUFFER b-comm-rate). /* ���� */


            /* �������쭮� ���祭�� */
            vMinRate = DEC (GetXAttrValueEx("comm-rate",
                                  GetSurrogateBuffer("comm-rate",
                                                     BUFFER b-comm-rate:HANDLE
                                                     ),
                                  "�������",
                                  "0"
                                  )
                               ) NO-ERROR.
            /* ���ᨬ��쭮� ���祭�� */
            vMaxRate = DEC (GetXAttrValueEx("comm-rate",
                                  GetSurrogateBuffer("comm-rate",
                                                     BUFFER b-comm-rate:HANDLE
                                                     ),
                                  "���ᇭ��",
                                  "0"
                                  )
                               ) NO-ERROR.
          /* ��筨�, �� � ������ �� ���� 䨪�஢���� */
          IF vCommTypeChar = {&GCodePeny} THEN DO:
            IF GET_COMM_TYPE (
               vCommChar,  /*��� �����ᨨ */
               vAcctRecid, /*�����䨪��� ��� */
               vCommCur,   /*��� �ਢ������� ������ */
               iKauChar,   /*��� ��� ("" - �� 㬮�砭��) */
               LnShPrm.balance[vGrupCounter], /*���������
                              ���⮪ (0 - ��㬮�砭��) */
               0,             /*��ਮ�/�ப (0-��㬮�砭��)*/
               iBegDate)
            THEN
              vCommTypeChar = {&GCodePenyFix}.
          END.
            /* ���४�஢��/ᮧ����� %% �⠢�� */
            RUN SetLnShPrm (iBegDate,?,?,?,?,?,?,vRateDec,
                            vCommTypeChar,?,vGrupCounter, vMinRate , vMaxRate ).

            /* ������ ���� ��������� ������ / ������ */
            vBegDate = GET_NEXT_COMM_DATE (
                         vCommChar,       /* ��� �����ᨨ */
                         vAcctRecid,      /* �����䨪��� ��� */
                         vCommCur,        /* ��� �ਢ������� ������ */
                         iKauChar,        /* ��� ��� ("" - �� 㬮�砭��) */
                         LnShPrm.balance[vGrupCounter], /* ��������� ���⮪
                                                          (0 - ��㬮�砭��) */
                         0,               /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                         iBegDate,
                         iEndDate).

            IF vBegDate < vBegDateT AND vBegDate <> ? THEN vBegDateT = vBegDate.

         END.
         iBegDate = vBegDateT.
      END.

      /* �᫨ ��ࠬ��஢ ��� ���᪠ �����ᨩ �� �������,
      ** � �饬 �� ���।� */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since > iBegDate
         NO-ERROR.

         /* � �᫨ �� ���,
         ** � � �� ���� �᪠�� �������/��� */
         iBegDate = IF AVAIL LnShPrm THEN
                       LnShPrm.since
                    ELSE
                       iEndDate + 1.
      END.
   END.
   RETURN.

END PROCEDURE.

/* ��ନ஢���� ������ �� ����� */
PROCEDURE GET_COMM_BY_TAX.

   DEF INPUT PARAM iCommChar AS CHAR NO-UNDO. /* ��� �����ᨨ */
   DEF INPUT PARAM iKauChar  AS CHAR NO-UNDO. /* ��� ��� */
   DEF INPUT PARAM iBegDate  AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO. /* "��" ����砭�� ���ࢠ�� */
   DEF INPUT PARAM iCodPar   AS INT64  NO-UNDO. /* ��� ��ࠬ���*/

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* �����䨪��� ��� */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* �����. ���� %% �⠢��/��� */
   DEF VAR vGrupCounter  AS INT64   NO-UNDO. /* ���稪 ��㯯*/
   DEF VAR vCommChar     AS CHAR  NO-UNDO. /* ������� � ࠬ��� ��㯯�*/
   DEF VAR vBegDate      AS DATE  NO-UNDO. /* ��६���� ��� ��।������ ���*/
   DEF VAR vBegDateT     AS DATE  NO-UNDO. /* ���� ��������� �����ᨨ �� �ᥩ
                                              ��㯯� */
   DEF VAR vCommCur      AS CHAR  NO-UNDO. /* ����� */

   DEFINE BUFFER bLnShPrm FOR LnShPrm.
   DEFINE BUFFER comm-rate FOR comm-rate .

   /* ��ନ஢���� �����ᨨ � ���ࢠ�� ��� */
   DO WHILE iBegDate <= iEndDate:

      /* ���� ��ࠬ��஢ �� ���� */
      FIND LAST LnShPrm WHERE
                LnShPrm.since <= iBegDate
      NO-ERROR.

      /* �᫨ ��ࠬ���� ��।�����,
      ** � ����⪠ ���᪠ ���祭�� �����ᨨ/��� */
      IF AVAIL LnShPrm THEN
      DO:

         ASSIGN
            vAcctRecid = ?
            vBegDateT  = iEndDate  + 1
            vCommCur   = LnShPrm.Currency
            .

         DO vGrupCounter = 1 TO NUM-ENTRIES(iCommChar,"&"):

            ASSIGN
               vCommChar = ENTRY(vGrupCounter,iCommChar,"&") .
               /* ����祭�� ������ �� ���� . �饬 �������㠫��� �⠢��
                  ������ ��� ������� - �᫨ ��� - �����. 
               */
               RUN  GET_COMM_BUF (
                            vCommChar,      /* ��� �����ᨨ */
                            vAcctRecid,     /* �����䨪��� ��� */
                            vCommCur,       /* ��� �ਢ������� ������ */
                            iKauChar,       /* ��� ��� ("" - �� 㬮�砭��) */
                            LnShPrm.balance[vGrupCounter], /*�����. ���⮪
                                                           (0 - ��㬮�砭��) */
                            0,              /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                            iBegDate,
                            BUFFER comm-rate
                       )
               .
              /* �᫨ ������� �������㠫쭠� �⠢�� - � ������� �ᥣ��
         ������ ���� � �������㠫쭮� - ��� ��㤠 �� ����室����� �����
         ⠪�� ��. ��� �⠭���⭠� */
            IF NOT AVAIL comm-rate
             THEN  DO :
               RUN  GET_COMM_BUF (
                            vCommChar,      /* ��� �����ᨨ */
                            vAcctRecid,     /* �����䨪��� ��� */
                            vCommCur,       /* ��� �ਢ������� ������ */
                            "",       /* ��� ��� ("" - �� 㬮�砭��) */
                            LnShPrm.balance[vGrupCounter], /*�����. ���⮪
                                                           (0 - ��㬮�砭��) */
                            0,              /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                            iBegDate,
                            BUFFER comm-rate
                       ).
                   IF AVAIL comm-rate
                   THEN  /*  ������� ���㦨������ �� �⠭���⭮�� ������ */
                   ASSIGN
                      vRateDec = comm-rate.rate-comm
                      iKauChar = "".
                   ELSE  vRateDec = 0.0 .
            END.           
            ELSE
             vRateDec = comm-rate.rate-comm .
            /* ���४�஢��/ᮧ����� %% �⠢�� */
            RUN SetLnTax (iBegDate,vRateDec,vGrupCounter).

            /* ������ ���� ��������� ������ / ������ */
            vBegDate = GET_NEXT_COMM_DATE (
                         vCommChar,       /* ��� �����ᨨ */
                         vAcctRecid,      /* �����䨪��� ��� */
                         vCommCur,        /* ��� �ਢ������� ������ */
                         iKauChar,        /* ��� ��� ("" - �� 㬮�砭��) */
                         LnShPrm.balance[vGrupCounter], /* ��������� ���⮪
                                                          (0 - ��㬮�砭��) */
                         0,               /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                         iBegDate,
                         iEndDate).

            IF vBegDate < vBegDateT AND vBegDate <> ? THEN vBegDateT = vBegDate.

         END.
         iBegDate = vBegDateT.
      END.

      /* �᫨ ��ࠬ��஢ ��� ���᪠ �����ᨩ �� �������,
      ** � �饬 �� ���।� */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since > iBegDate
         NO-ERROR.

         /* � �᫨ �� ���,
         ** � � �� ���� �᪠�� �������/��� */
         iBegDate = IF AVAIL LnShPrm THEN
                       LnShPrm.since
                    ELSE
                       iEndDate + 1.
      END.
   END.

   RETURN.

END PROCEDURE.

/* ���᫥��� ��業⮢ � �ନ஠��� ���� */
PROCEDURE GET_NAC_REP_NEW.

   DEF INPUT PARAM iSchmRecid AS RECID NO-UNDO. /* ��� �奬� ���᫥��� */
   DEF INPUT PARAM iContCode  AS CHAR  NO-UNDO.
   DEF INPUT PARAM iContract  AS CHAR  NO-UNDO.
   DEF INPUT PARAM iBegDate   AS DATE  NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM iEndDate   AS DATE  NO-UNDO. /* "��" ����砭�� ���ࢠ�� */

   DEF VAR vEndDate   AS DATE NO-UNDO. /* ����砭�� ��ਮ�� ���᫥��� ��業⮢*/
   DEF VAR vDaysInt   AS INT64  NO-UNDO. /* ������⢮ ���� � ��ਮ�� */
   DEF VAR vCounter   AS INT64  NO-UNDO. /* ���稪 ��㯯*/
   DEF VAR vTermCheck AS LOG  NO-UNDO.
   DEF VAR vNextDate1 AS DATE NO-UNDO.
   DEF VAR vNextDate2 AS DATE NO-UNDO.
   DEF VAR vIsEqRat   AS LOG  NO-UNDO.
   DEF VAR vtax       AS DECIMAL INIT 1 NO-UNDO .
   DEF VAR mFormRasch AS CHAR NO-UNDO. /* ���㫠 ���� ��業⮢ � �� �롔�ଐ����� */

   DEF BUFFER buf-lnFost        FOR LnShPrm.
   DEF BUFFER interest-sch-line FOR interest-sch-line.

   /* ���� �����ᨨ */
   RUN get_sch_line_by_rid IN h_schem
       (iSchmRecid,
        BUFFER interest-sch-line).

   /* ���� ��ࠬ��஢ ���᫥��� �� ���� */
   FIND LAST LnShPrm WHERE
             LnShPrm.since <= iBegDate
   NO-ERROR.

   /* ���� ��ࠬ��஢ ���᫥��� ���। */
   FIND FIRST buf-lnFost WHERE
              buf-lnFost.since > iBegDate
   NO-ERROR.

   /* ��।������ ���� ��砫� ���᫥���,
   ** ��室� �� ������ ��ࠬ��஢ ���᫥��� */
   iBegDate = IF AVAIL LnShPrm THEN
                 iBegDate
              ELSE
              IF AVAIL buf-lnFost THEN
                 buf-lnFost.since
              ELSE
                 iEndDate.

   FIND FIRST loan WHERE 
              loan.cont-code EQ iContCode 
          AND loan.contract  EQ iContract 
   NO-LOCK NO-ERROR.
   FIND FIRST loan-cond WHERE 
              loan-cond.cont-code EQ iContCode 
          AND loan-cond.contract  EQ iContract 
   NO-LOCK NO-ERROR.

   /* ����砥� ���� ���� ��業⮢ � ������� ��� � ����� */
   mFormRasch = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"�롔�ଐ�����",GetXAttrInit(loan.class-code,"�롔�ଐ�����")).
   IF mFormRasch EQ ? 
      THEN mFormRasch = fGetSetting("�롔�ଐ�����","","1").
   /* ���������� � ��⠭������� (᪮�४�஢����)
   ** ���ࢠ� ��� */
   DO WHILE iBegDate <= iEndDate:
      /* ��室�� ��ࠬ���� ���� */
      FIND LAST LnShPrm WHERE
                LnShPrm.since <= iBegDate
      NO-ERROR.

      /* ���� ᫥����� ��ࠬ��஢ */
      FIND FIRST buf-lnFost WHERE
                 buf-lnFost.since > iBegDate
      NO-ERROR.

      /* ����祭�� ���� ����砭�� ⥪�饣� ��ਮ�� */
      vEndDate = IF AVAIL buf-lnFost THEN
                    buf-lnFost.since - 1
                 ELSE
                    iEndDate.

      /* ��।������ ���-�� ���� � ���ࢠ�� */
      vDaysInt = cDay(interest-sch-line.interest-month,
                      iBegDate,
                      vEndDate + 1).

     

      FIND FIRST term-obl WHERE term-obl.contract  EQ iContract
                            AND term-obl.cont-code EQ iContCode
                            AND term-obl.idnt EQ 1
                            AND term-obl.end-date GE iBegDate
                            AND term-obl.end-date LE vEndDate
      NO-LOCK NO-ERROR.

      vNextDate1 = FRST_DATE(iBegDate,
                    vEndDate,
                    loan-cond.int-date,
                    loan-cond.int-period,
                    loan-cond.since).
      vNextDate2 = FRST_DATE(iBegDate,
                    vEndDate + 1,
                    loan-cond.int-date,
                    loan-cond.int-period,
                    loan-cond.since).

      IF AVAIL term-obl THEN 
      DO:
         IF term-obl.end-date EQ loan.end-date THEN 
         DO:
            IF vNextDate1 EQ vNextDate2 AND vEndDate <= loan.end-date THEN 
            ASSIGN
               vTermCheck = YES.
         END.
         ELSE
            ASSIGN
               vTermCheck = YES.
      END.

      /* otch1 - ���७�. � ��� � ᮧ���� ���� */
      IF AVAIL LnShPrm THEN
      DO vCounter = 1 TO EXTENT(LnShPrm.balance):
         
         IF LnShPrm.balance[vCounter] = ? OR LnShPrm.rate[vCounter] = ? THEN 
            NEXT.
         vIsEqRat = IF LnShPrm.CommType[vCounter] EQ "=" THEN 
                       YES 
                    ELSE NO.
        /* ��।��塞 ���ࠢ��� �����樥��, �᫨ ���� ���뢠�� ��� */
        IF  LnShPrm.tax[vCounter] <> ?
        THEN ASSIGN  vTax = 100 / (100 +  LnShPrm.tax[vCounter]).
      	ELSE VTAX = 1.  
        CREATE otch1.
        IF  LnShPrm.tax[vCounter] <> ?
        THEN ASSIGN  vTax = 100 / (100 +  LnShPrm.tax[vCounter])
                     otch1.summ_opl =  LnShPrm.tax[vCounter].
    
         ASSIGN
            otch1.bal-summ = IF vIsEqRat EQ NO THEN LnShPrm.balance[vCounter] ELSE ?
            otch1.beg-date = iBegDate
            otch1.end-date = vEndDate
            otch1.ndays    = IF vIsEqRat EQ NO THEN vDaysInt ELSE ?
            otch1.rat1     = IF vIsEqRat EQ NO THEN LnShPrm.rate[vCounter] ELSE ?
         /* ����� ��業⭮� �⠢�� � ����ᨬ��� �� ⨯� */

           otch1.summ_pr = 
            /* ����� ���祭�� �� � ⨯�� "�⠢��". � ����ᨬ��� �� ���� */
            IF    LnShPrm.CommType[vCounter] EQ "%" 
              AND mFormRasch EQ "1" THEN
                              /*�� � ���*//*    �㬬�             */  /*      �⠢��      */    /*       ���� � ����        */
                ROUND ((vTax * vDaysInt * (LnShPrm.balance[vCounter] * LnShPrm.rate[vCounter] / (interest-sch-line.basis-time * 100))), 2)
            ELSE
            IF    LnShPrm.CommType[vCounter] EQ "%" 
              AND mFormRasch EQ "2" THEN
                      /*          �㬬�              */   /*      �⠢��      */ /*100*/ /*�� � ���*/ /*       ���� � ����       */ 
               ROUND(((vTax * LnShPrm.balance[vCounter] * LnShPrm.rate[vCounter] / 100 ) * vDaysInt / interest-sch-line.basis-time ),2)

            /* ����� ���祭�� � ⨯�� "���". */
            ELSE
            IF LnShPrm.CommType[vCounter] EQ "=" AND vTermCheck THEN
               ROUND(vTax * LnShPrm.rate[vCounter], 2)

            /* ����� ���祭�� � ⨯�� "����". */
            ELSE
            IF LnShPrm.CommType[vCounter] EQ {&GCodePeny} THEN
               ROUND ((vTax * vDaysInt * (LnShPrm.balance[vCounter] *
                                   LnShPrm.rate[vCounter] / 100)), 2)
             /* ����� ���祭�� � ⨯�� "���� = 䨪�஢�����". */
            ELSE
            IF LnShPrm.CommType[vCounter] EQ {&GCodePenyFix} THEN
                IF LnShPrm.balance[vCounter] > 0
                  THEN
                        ROUND ((vTax * vDaysInt * LnShPrm.rate[vCounter]), 2)
                  ELSE 0

            ELSE
               ?

            otch1.summ_pr  = IF otch1.summ_pr EQ ? THEN
                                0
                             ELSE
                                otch1.summ_pr
         .
         /* �஢�ઠ �� �������쭮� ���祭�� */
         IF     LnShPrm.balance[vCounter] GT 0 
            AND LnShPrm.MinRate[vCounter] NE 0 
            AND LnShPrm.MinRate[vCounter] GT otch1.summ_pr  THEN 
            otch1.summ_pr = LnShPrm.MinRate[vCounter].
         /* �஢�ઠ �� ���ᨬ��쭮� ���祭�� */
         IF     LnShPrm.balance[vCounter] GT 0 
            AND LnShPrm.MaxRate[vCounter] NE 0 
            AND LnShPrm.MaxRate[vCounter] LT otch1.summ_pr THEN 
            otch1.summ_pr = LnShPrm.MaxRate[vCounter].
      END.
      iBegDate = vEndDate + 1.
   END.
   RETURN.
END PROCEDURE.

PROCEDURE GET-OST-ACCT.
   DEF INPUT PARAM iStr         AS CHAR  NO-UNDO.
   DEF INPUT PARAM iRid         AS RECID NO-UNDO .
   DEF INPUT PARAM iBegDate     AS DATE  NO-UNDO .
   DEF INPUT PARAM iEndDate     AS DATE  NO-UNDO .
   DEF INPUT PARAM iGrupCounter AS INT64   NO-UNDO .
  
   DEF VAR vAcctType AS CHAR   NO-UNDO.
   DEF VAR vFunc     AS CHAR   NO-UNDO.
   DEF VAR vList     AS CHAR   NO-UNDO.
   DEF VAR i         AS INT64    NO-UNDO.
   DEF VAR vI        AS INT64    NO-UNDO. /* ���稪 */
   DEF VAR vCurrency AS CHAR   NO-UNDO. /* � ��� � ����� ����� */
   DEF BUFFER acct FOR acct .

   {empty ttsh}
   DO vI = 1 TO NUM-ENTRIES(iStr):
      ASSIGN
         iStr      = REPLACE(iStr, " ", "")
         vFunc     = ENTRY(vi,iStr)
         vAcctType = SUBSTR(SUBSTR(vFunc,INDEX(vFunc, "(") + 1), 1,
                            LENGTH(SUBSTR(vFunc,INDEX(vFunc, "(") + 1)) - 1)
      .
      RUN listacct.p(vAcctType,iRid,iBegDate,iEndDate,OUTPUT vList) .
      DO i = 1 TO 	NUM-ENTRIES(vList) :
         FIND FIRST acct  WHERE acct.acct = ENTRY(i,vList) NO-LOCK NO-ERROR .
         IF NOT AVAIL acct
         THEN NEXT .
         RUN apos-sh.p(acct.acct,acct.currency,iBegDate,iEndDate,gop-status).
         FOR EACH sh NO-LOCK:
            /* ���४�஢�� ���⪠ */
            FIND FIRST ttsh WHERE ttsh.since EQ sh.since NO-LOCK NO-ERROR.
            IF NOT AVAIL ttsh 
            THEN DO:
               CREATE ttsh.
               ASSIGN 
                  ttsh.Summa = 0
                  ttsh.since = sh.since
                  vCurrency  = acct.currency
               .
            END.
            ttsh.Summa = ttsh.Summa + IF acct.currency = '' THEN sh.bal ELSE sh.val.
            
         END.
      END.
   END.
   FOR EACH ttsh NO-LOCK:
      RUN SetLnShPrm (ttsh.since + IF vIshOst THEN 0 ELSE 1,
                      ?,
                      ?,
                      vCurrency,
                      ?,
                      ttsh.Summa,
                      ?,
                      ?,
                      ?,
                      ?,
                      iGrupCounter,
                      ?,
                      ?).
   END.
END PROCEDURE .

FUNCTION is-need-correct RETURNS LOGICAL ( iContract AS CHAR,
                                           iContCode AS CHAR,
                                           iDate     AS DATE ):

   RETURN IF is-depmax (iContract,iContCode,iDate) EQ "1" THEN YES
                                                          ELSE NO.

END FUNCTION.

PROCEDURE base-sum-correct.

   DEF INPUT        PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT        PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT        PARAM iDate     AS DATE NO-UNDO.
   DEF INPUT        PARAM iRateCode AS CHAR NO-UNDO.
   DEF INPUT-OUTPUT PARAM ioBaseSum AS DEC  NO-UNDO.
   
   DEF VAR vDepMax AS DEC NO-UNDO.
   DEF BUFFER bc-rate  FOR comm-rate.

   mb:
   DO ON ERROR UNDO, LEAVE:

      IF NOT CAN-DO("%���,%�������",iRateCode) THEN
         LEAVE mb.
      
      FIND LAST bc-rate WHERE bc-rate.kau        EQ iContract + "," + iContCode
                          AND bc-rate.commission EQ "���ᄥ�"
                          AND bc-rate.since      LE iDate
         NO-LOCK NO-ERROR.
      /* ����祭�� �����ᨨ/��� �� ���� */
      vDepMax = IF AVAIL bc-rate THEN bc-rate.rate-comm
                                 ELSE ?.
      
      IF    iRateCode EQ "%���" 
        AND ioBaseSum > vDepMax THEN
        ioBaseSum = vDepMax.

      IF iRateCode EQ "%�������" THEN
      DO:
        IF ioBaseSum > vDepMax THEN
           ioBaseSum = ioBaseSum - vDepMax.
        ELSE
        IF ioBaseSum <= vDepMax  THEN
           ioBaseSum = 0.
      END.

   END. /* mb: */

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:20:59.602+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='ln-proc.i' */
/*prosignO7aoTRNnCFzWlMV+jTIX5Q*/