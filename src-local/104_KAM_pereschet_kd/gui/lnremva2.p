/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: lnremva2.p
      Comment: �奬� ���᫥��� ��業⮢ �� �� ������ࠬ


               iBegDate - ��� ��砫� ����, ��।����� �� 1-� ����
               ����� ॠ�쭮� (�����᪨� 䠪�). �� �뫮 ᤥ���� ���
               ������� workost1.i, 楫� - �ନ஢���� ��������� ���⪠.

   Parameters:
         Uses:
      Used by:
      Created: ���� 18.10.2002
     Modified: ���� 21.10.2002
*/

{globals.i}     /* �������� ���७�� ��६���� ��ᨨ */
{svarloan.def}  /* �������� ���७�� ��६���� ����� */
{t-otch.i}      /* �६����� ⠡��� ��� ���� � ���� */

{intrface.get xclass}  /* �����㬥��� ��� ࠡ��� � ����奬��. */
{intrface.get comm}    /* �����㬥��� ��� ࠡ��� � ������ﬨ. */
{intrface.get schem}   /* �����㬥��� ��� ࠡ��� � �奬��� ���᫥���. */
{intrface.get date}    /* �����㬥��� ��� ࠡ��� � ��⠬� */
{intrface.get db2l}    /* �����㬥��� ��� ࠡ��� � ����묨 */
{intrface.get instrum} /* �����㬥��� ��� ࠡ��� � 䨭.�����㬥�⠬� */
{intrface.get loan}    /* �����㬥��� ��� ࠡ��� � ������ࠬ� */
{sh-defs.i new}
{sh-temp.i new}
{ln-nach.i}     /* �����㬥��਩ ��� �奬 ���᫥��� �� ������ࠬ �ॡ��
                   svarloan.def*/
{ln-proc.i}
/*{empty LnShPrm}*/
EMPTY TEMP-TABLE LnShPrm.

DEFINE INPUT PARAM iContractChar AS CHAR  NO-UNDO. /* �����祭�� ������� */
DEFINE INPUT PARAM iContCodeChar AS CHAR  NO-UNDO. /* ����� ������� */
DEFINE INPUT PARAM iSchRecid     AS RECID NO-UNDO. /* �����䨪���� �奬� */
DEFINE INPUT PARAM iBegDate      AS DATE  NO-UNDO. /* ��� ��砫� ���� */
DEFINE INPUT PARAM iEndDate      AS DATE  NO-UNDO. /* ��� ����砭�� ���� */
DEFINE INPUT PARAM iDatPer       AS DATE  NO-UNDO. /* ��� ���室� �� 39� */
DEFINE INPUT PARAM iCodPar       AS INT64   NO-UNDO. /* ��� ��ࠬ��� */
DEFINE INPUT PARAM iTypeOst      AS INT64   NO-UNDO. /* �ᥣ�� ��।����� 1 �� ��. */

DEF VAR vCurrCalcChar AS CHAR NO-UNDO. /* ���� ��� ���� */
DEF VAR vRateChar     AS CHAR NO-UNDO. /* ���饭�� �� �ॡ㥬� %% �⠢�� */
DEF VAR vRateTax      AS CHAR NO-UNDO. /* ��� ������ */
DEF VAR vNumGrups     AS INT64  NO-UNDO. /* ��᫮ ��㯯 ��� ������ ��ࠬ���*/
DEF VAR vGrupCounter  AS INT64  NO-UNDO. /* ��㯯� ��� ������ ��ࠬ���     */
DEF VAR vCommChar     AS CHAR NO-UNDO.
DEF VAR vRateDec      AS DEC  NO-UNDO.

DEF VAR mRecalcPP   AS LOG  NO-UNDO.
DEF VAR mRecalcLoan AS LOG  NO-UNDO.
DEF VAR mSurr       AS CHAR NO-UNDO.
DEF VAR mTypeCond   AS LOG  NO-UNDO.
DEF VAR vDateD      AS DATE NO-UNDO.
DEF VAR vDateLastOp AS DATE NO-UNDO.
DEF VAR vNeedGraf   AS LOG  NO-UNDO.
DEF VAR vPrmCharBase AS CHAR NO-UNDO. /* ��ப� ��ࠬ��஢ "," ����� "&+" */
DEF VAR vJAmTrahsh  AS LOG  NO-UNDO.
DEF VAR vFromTrahsh AS LOG  NO-UNDO.

DEF BUFFER loan-cond FOR loan-cond.
DEF BUFFER bl-int    FOR loan-int.
DEF BUFFER bc-rate   FOR comm-rate.

/* ���� ��ࠬ��஢ �࠭��� ��業�� */
DEF VAR vAllRateChar AS CHAR
                       INIT "4,8,9,11,12,14,15,17,18,20,81,82,96,704"
                       NO-UNDO.

vJAmTrahsh = NUM-ENTRIES(iContCodeChar," ") EQ 2.

/* ���� ������� */
FIND FIRST loan WHERE loan.contract  EQ iContractChar  
                  AND loan.cont-code EQ iContCodeChar 
     NO-LOCK NO-ERROR.
FIND FIRST curloan WHERE RECID(curloan) EQ RECID(loan) 
     NO-LOCK NO-ERROR.

SetCodPar(loan.class-code) .

/* ���� ��ࢮ�� �᫮���- �⮡� �஢���� �����⭮��� 
- �������� �� ������ � ⮬� �� �������� ���� �᫮��� � �����⭮� � ��
������묨 �奬��� ��-�� ������ਧ�樨 */

FIND LAST  loan-cond WHERE loan-cond.contract  EQ iContractChar
                       AND loan-cond.cont-code EQ iContCodeChar
                       AND loan-cond.since     LE iBegDate
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE loan-cond THEN
   FIND FIRST loan-cond WHERE loan-cond.contract  EQ iContractChar 
       AND loan-cond.cont-code EQ iContCodeChar 
       AND loan-cond.since     GE loan.open-date
NO-LOCK NO-ERROR.

mTypeCond  = (GetXAttrValue("loan-cond",
                            iContractChar + "," +  iContCodeChar + "," + string(loan-cond.since),
                            "�奬�����") = "�����⭠�").
ASSIGN
   mRecalcLoan = (GetSysConf("�����℮�����") = "��")
   mRecalcPP   = (GetSysConf("�����⏏") = "��") AND NOT mTypeCond 
   /*
   ��� �����⭮� �奬� ��易⥫쭮 ����᪠�� ������ �� 䠪�� + ���� � ��砥
   ������ ���⥦�� �� ������  ���ﭨ� ������� */
   .

Cr-Otch:
DO TRANSACTION:

/* ��ନ஢���� ���� ���᫥���. */
RUN GetCalcCharNew (iContractChar,
                    iContCodeChar,
                    iCodPar,
                    OUTPUT vCurrCalcChar,
                    OUTPUT vNumGrups).
IF vCurrCalcChar MATCHES "*�*"  THEN
   ASSIGN
      vCurrCalcChar = REPLACE(vCurrCalcChar,"�","")
      vNeedGraf = YES
   .
IF vCurrCalcChar MATCHES "*��*"  THEN
   ASSIGN
      vCurrCalcChar = REPLACE(vCurrCalcChar,"��","")
      vFromTrahsh = YES
   .

vPrmCharBase = REPLACE(REPLACE(vCurrCalcChar,"&",","),"+",",").
IF CAN-DO(vCurrCalcChar, STRING(CodOstPar)) THEN
   vNeedGraf = YES.

/* ����祭�� ���� �����ᨨ */
RUN GetCodeRateNew (
    iCodPar,            /* ���浪��� ����� %%, ���� loan.interest[iCodPar] */
    vNumGrups,          /* ��᫮ ��㯯*/
    OUTPUT vRateChar,   /* ��� ���/�����ᨨ */
    BUFFER loan).

/* ���� �᪠�� �� ��ࢮ�� ���祭�� */
vRateDec = 0.
DO vGrupCounter = 1 TO NUM-ENTRIES(vRateChar,"&"):
   ASSIGN
      vCommChar = ENTRY(vGrupCounter,vRateChar,"&").
      FIND LAST bc-rate WHERE bc-rate.kau     EQ iContractChar + "," + iContCodeChar
                       AND bc-rate.commission EQ vCommChar
                       AND bc-rate.since      LE iEndDate
      NO-LOCK NO-ERROR. 

            /* ����祭�� �����ᨨ/��� �� ���� */
      vRateDec = vRateDec + 
                 IF AVAIL bc-rate THEN bc-rate.rate-comm
                               ELSE 0.
      IF vRateDec GT 0 THEN LEAVE.
/* ��� �࠭� �஢���� �� ��. */
      IF vJAmTrahsh THEN 
      DO:
         FIND LAST bc-rate WHERE bc-rate.kau        EQ iContractChar + "," + ENTRY(1,iContCodeChar," ")
                          AND bc-rate.commission EQ vCommChar
                          AND bc-rate.since      LE iEndDate
         NO-LOCK NO-ERROR.
         vRateDec = vRateDec + 
                    IF AVAIL bc-rate THEN bc-rate.rate-comm
                                        ELSE 0.
         IF vRateDec GT 0 THEN LEAVE.
      END.
/*   A ��� ��. �஢���� � �� �࠭�? 
     ���� �� �㤥� */

END.
IF    vRateDec EQ ? THEN /* �᫨ �⠢�� ������� �� �뫮 */
   LEAVE Cr-Otch.


/*IF loan.since < iEndDate AND iCodPar = 1 AND iTypeOst = 1 AND
   GetSysConf("�����℮�����") <> "��"
THEN
*/
/* �஡���� �  �ᯮ�짮������ iTypeOst - �� ����� ������ �᫮��� ��
��।����� � ���祭��� 0, �.� �� �㤥� �ନ஢����� ⮫쪮
�� ������� ���⪠�. �᫨ �� ��஥ �᫮��� � �����⭠� �奬� -
� nach-pp ���-� � �⨬ ������ , � ������ �奬� ��� - ����室���
��⠭����� �᫮���, �� ���஥ ������﫮 ��ॡ���� iTypeOst �
1 , �᫨ �������� ����� ��஥ �᫮��� � ���� 㦥 �������� �� ��������.
*/

/* �஢�ਬ, �� �� ���o���� �뫮 �������� 
   ��� �� ��室���� ������. �⮡� ������� �᫮��� ����
   �㦭� ��९஥��஢���� */

IF iTypeOst = 0 
   AND vNeedGraf THEN 
DO:
/* ��ॡ�� �� �࠭蠬 ��宦� �� �㦥�, ����� ���� 
   ���� �஢���� ������⢮ �᫮���? */
     /* ���� ��ࢮ� ����⮢�� ����樨 �� �������� �᭮����� ��ࠬ��� */
   IF CAN-FIND( FIRST loan-int WHERE loan-int.contract  EQ iContractChar 
                                 AND loan-int.cont-code EQ iContCodeChar 
                                 AND loan-int.mdate     GE loan.open-date 
                                 AND loan-int.id-d      EQ CodOstPar)
      OR
     /* ���� ��ࢮ� �।�⮢�� ����樨 �� �������� �᭮����� ��ࠬ��� */
      CAN-FIND( FIRST bl-int WHERE bl-int.contract  EQ iContractChar 
                               AND bl-int.cont-code EQ iContCodeChar 
                               AND bl-int.mdate     GE loan.open-date
                               AND loan-int.id-k    EQ CodOstPar)
   THEN 
      iTypeOst = 1.
END.

IF loan.since < iEndDate 
   AND iCodPar  = 1          
   AND iTypeOst = 1          
   AND NOT mRecalcLoan       
   AND NOT mRecalcPP THEN
      RUN GET_DOLG_CORR (iContractChar,
                      iContCodeChar,
                      loan.since,
                      iEndDate,
                      YES).

/* � lnscheme �࣠�������� 2 横�� �᫨ ��� ���� � ��ਮ�� */
vIshOst = iEndDate LE iDatPer.
IF vIshOst THEN
   iEndDate = iEndDate - 1.

/* ��ନ஢���� ���⪠ �� ��ࠬ���� �������. */
IF iTypeOst EQ 1 THEN
DO:
/* ࠧ����� ��� vFromTrahsh  
   ��� �࠭襩 ��६ �� ���� ������ �������, 
   �⮡� �� ����� �� ������� �࠭�� */

   IF vFromTrahsh THEN
   DO:
      RUN GET_REM_BY_PRM_TR (iContractChar,
                             iContCodeChar,
                             vCurrCalcChar,  /* ���� ���᫥��� */
                             iBegDate,
                             iEndDate,
                             iCodPar,
                             loan.currency,
                             loan.since,
                             mRecalcLoan,
                             mRecalcPP).

     RUN SetLnShPrmAdd (loan.currency).

   END.
   ELSE DO:
      RUN GET_REM_BY_PRM_NEW (iContractChar,
                              iContCodeChar,
                              vCurrCalcChar,  /* ���� ���᫥��� */
                              iBegDate,
                              iEndDate,
                              iCodPar,
                              loan.currency,
                              loan.since,
                              mRecalcLoan,
                              mRecalcPP,
                              NO).
   END.
/*
   IF vFromTrahsh THEN
   DO:
      vDateLastOp = loan.since.
      RUN GET_REM_BY_PRM_TR (iContractChar,
                             iContCodeChar,
                             vCurrCalcChar,  /* ���� ���᫥��� */
                             iBegDate,
                             MIN(iEndDate,vDateLastOp),
                             iCodPar,
                             loan.currency,
                             loan.since,
                             mRecalcLoan,
                             mRecalcPP).

      IF     vNeedGraf 
         AND vDateLastOp LT iEndDate THEN
         RUN GET_REM_BY_TERM_TR (iContractChar,
                                 iContCodeChar,
                                 2,              /* ��� "�������� �㬬" */
                                 vDateLastOp + 1,
                                 iEndDate,
                                 loan.currency).
     RUN SetLnShPrmAdd (loan.currency).

   END.
   ELSE DO:
      RUN GetLastDateParam (iContractChar,
                            iContCodeChar,
                            vCurrCalcChar,
                            iBegDate,
                     OUTPUT vDateLastOp). /* ��᫥���� ��� �������� */
      IF vDateLastOp EQ ? THEN
         vDateLastOp = loan.since.
      IF MAX(vDateLastOp,loan.since) GE iBegDate THEN
      DO:
         RUN GET_REM_BY_PRM_NEW (iContractChar,
                                 iContCodeChar,
                                 vCurrCalcChar,  /* ���� ���᫥��� */
                                 iBegDate,
                                 MIN(iEndDate,MAX(vDateLastOp,loan.since)),
                                 iCodPar,
                                 loan.currency,
                                 loan.since,
                                 mRecalcLoan,
                                 mRecalcPP,
                                 NO).
      END.
      IF     vNeedGraf 
         AND vDateLastOp LT iEndDate THEN
         RUN GET_REM_BY_TERM (iContractChar,
                              iContCodeChar,
                              2,              /* ��� "�������� �㬬" */
                              MAX(iBegDate,MAX(vDateLastOp,loan.since) + IF vIshOst THEN 1 ELSE 0),
                              iEndDate,
                              loan.currency,
                              NO).
   END.
  */
END.
/* ��ନ஢���� ���⪠ �� ������� ��魮��� �������. */
ELSE
DO:
   IF vFromTrahsh THEN
   DO:
      RUN GET_REM_BY_TERM_TR (iContractChar,
                              iContCodeChar,
                              2,              /* ��� "�������� �㬬" */
                              iBegDate,
                              iEndDate,
                              loan.currency).
      RUN SetLnShPrmAdd (loan.currency).
   END.
   ELSE
      RUN GET_REM_BY_TERM (iContractChar,
                           iContCodeChar,
                           2,              /* ��� "�������� �㬬" */
                           iBegDate,
                           iEndDate,
                           loan.currency,
                           NO).
END.

/* ����祭�� ���� ��� ��� */
RUN GetCodeTax (
    iCodPar,            
    vNumGrups,          /* ��᫮ ��㯯*/
    OUTPUT vRateTax,   /* ��� ��� */
    BUFFER loan).


/* ��ନ஢���� ��業⭮� �⠢�� �� �������� ���⪠. */
/* �।��������, �� �⠢�� �� �墠�뢠�饬. ��� �������㠫��� �⠢��
   �࠭襩 �ਤ���� ��ࠡ��뢠��  */
RUN GET_COMM_BY_REM_NEW( iContractChar ,
                         iContCodeChar , 
                         vRateChar,       /* ��� �����ᨨ/��� */      
                         iContractChar + "," + iContCodeChar, /* KAU */
                         iBegDate,
                         iEndDate,
                         iCodPar).
IF vRateTax = ?
THEN 
/* ��ନ஢���� ��業⭮� �⠢�� �� ��� */
   RUN GET_COMM_BY_TAX   (vRateTax,             /* ��� �����ᨨ/��� */
                         iContractChar + "," + iContCodeChar, /* KAU */
                         iBegDate,
                         iEndDate,
                         iCodPar).
/* ���᫥��� � �ନ஢���� १����. */
RUN GET_NAC_REP_NEW (iSchRecid,
                     iContCodeChar,
                     iContractChar,
                     iBegDate,
                     iEndDate).

/* �᫨ �㬬� ���᫥��� �� ��ਮ� �㫥���,
** � 㤠�塞 ����
** ��� ����஥� ������ �� ������ࠬ */

FIND FIRST otch1 WHERE
           otch1.comment EQ ""
       AND otch1.summ_pr NE 0.00
NO-ERROR.

IF NOT AVAIL otch1 THEN
DO:
   FOR EACH otch1 WHERE
      otch1.comment EQ "":
      DELETE otch1.
   END.
END.
/* �饬 ��ࠬ��� - ��� ��業� �� ���஬� ����塞 */
FIND FIRST loan-par WHERE
           loan-par.amt-id = INT64(ENTRY(iCodPar, vAllRateChar)) + CodostPar 
NO-LOCK NO-ERROR.


FOR EACH otch1:
   otch1.amt-id = IF AVAIL loan-par THEN loan-par.amt-id
                                    ELSE ?.
   IF otch1.comment EQ "" THEN
      otch1.comment =  IF AVAIL loan-par THEN loan-par.name 
                                         ELSE otch1.comment.
END.

END. /* do trans: */

{intrface.del}

RETURN.
