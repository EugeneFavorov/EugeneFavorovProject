/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: lnremvar.p
      Comment: �奬� ���᫥��� ��業⮢ �� �� ������ࠬ.

               ipBegDate - ��� ��砫� ����, ��।����� �� 1-� ����
               ����� ॠ�쭮� (�����᪨� 䠪�). �� �뫮 ᤥ���� ���
               ������� workost1.i, 楫� - �ନ஢���� ��������� ���⪠.

   Parameters: ���
         Uses:
      Used by:
      Created: 18/03/2002
     Modified: ���� 09.10.2002
     Modified: ���� 11.11.2002 ���� ���᫥��� � ��業�� �⠢�� ����� ������
               �� ���. ४����⮢ ������� � ��砫��� ���祭��
     Modified:
*/

form "~n@(#) lnremvar.p 1.0 18/03/2002"
with frame sccs-id stream-io width 250.

{globals.i}     /* �������� ���७�� ��६���� ��ᨨ */
{svarloan.def}  /* �������� ���७�� ��६���� ����� */
{t-otch.i}      /* �६����� ⠡��� ��� ���� � ���� */

{intrface.get xclass} /* �����㬥��� ��� ࠡ��� � ����奬��. */
{intrface.get comm}   /* �����㬥��� ��� ࠡ��� � ������ﬨ. */
{intrface.get schem}  /* �����㬥��� ��� ࠡ��� � �奬��� ���᫥���. */
{intrface.get date}   /* �����㬥��� ��� ࠡ��� � ��⠬� */
{intrface.get loan}   /* �����㬥��� ��� ࠡ��� � ������ࠬ� */

{ln-nach.i}     /* �����㬥��਩ ��� �奬 ���᫥��� �� ������ࠬ �ॡ�� svarloan.def*/
/*{empty LnShPrm}*/
EMPTY TEMP-TABLE LnShPrm.

DEFINE INPUT PARAM ipContractChar AS CHAR  NO-UNDO. /* �����祭�� ������� */
DEFINE INPUT PARAM ipContCodeChar AS CHAR  NO-UNDO. /* ����� ������� */
DEFINE INPUT PARAM ipSchRecid     AS RECID NO-UNDO. /* �����䨪���� �奬� */
DEFINE INPUT PARAM ipBegDate      AS DATE  NO-UNDO. /* ��� ��砫� ���� */
DEFINE INPUT PARAM ipEndDate      AS DATE  NO-UNDO. /* ��� ����砭�� ���� */
DEFINE INPUT PARAM dat-per        AS DATE  NO-UNDO. /* ��� ���室� �� 39� */
DEFINE INPUT PARAM cod-par        AS INT64 NO-UNDO. /* ��� ��ࠬ��� */
DEFINE INPUT PARAM fl-type-ost    AS INT64 NO-UNDO. /* �ᥣ�� ��।����� 1 �� ��. */

DEF VAR vCurrCalcChar AS CHAR NO-UNDO. /* ���� ��� ���� */
DEF VAR vRateChar     AS CHAR NO-UNDO. /* ���饭�� �� �ॡ㥬� %% �⠢�� */

DEF VAR mRecalcPP   AS LOG NO-UNDO.
DEF VAR mRecalcLoan AS LOG NO-UNDO.
DEF VAR mFormRasch  AS CHAR NO-UNDO. /* ���㫠 ���� ��業⮢ � �� �롔�ଐ����� */
DEF VAR vNeedGraf   AS LOG  NO-UNDO.

/* ���� ��ࠬ��஢ �࠭��� ��業�� */
DEFINE VAR vAllRateChar AS CHAR
                        INIT "4,8,9,11,12,14,15,17,18,20,81,82,96,704"
                        NO-UNDO.

/* ���� ������� */
FIND FIRST loan WHERE loan.contract  EQ ipContractChar  
                  AND loan.cont-code EQ ipContCodeChar 
     NO-LOCK NO-ERROR.

SetCodPar(loan.class-code) .

ASSIGN
   mRecalcLoan = (GetSysConf("�����℮�����") = "��")
   mRecalcPP   = (GetSysConf("�����⏏") = "��")
   mFormRasch   = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"�롔�ଐ�����",GetXAttrInit(loan.class-code,"�롔�ଐ�����"))
.
IF mFormRasch EQ ? 
   THEN mFormRasch = fGetSetting("�롔�ଐ�����","","1").

IF loan.since < ipEndDate AND
   cod-par = 1            AND
   fl-Type-Ost = 1        AND
   NOT mRecalcLoan        AND
   NOT mRecalcPP
THEN
   RUN GET_DOLG_CORR (ipContractChar,
                      ipContCodeChar,
                      loan.since,
                      ipEndDate,
                      YES).

/* � lnscheme �࣠�������� 2 横�� �᫨ ��� ���� � ��ਮ�� */
vIshOst = ipEndDate LE dat-per.
IF vIshOst THEN
   ipEndDate = ipEndDate - 1.

/* ��ନ஢���� ���� ���᫥���. */
RUN GetCalcChar (ipContractChar,
                 ipContCodeChar,
                 cod-par,
                 OUTPUT vCurrCalcChar).

IF CAN-DO(vCurrCalcChar, STRING(CodOstPar)) THEN
   vNeedGraf = YES.
IF fl-type-ost EQ 1

/* ��ନ஢���� ���⪠ �� ��ࠬ���� �������. */
THEN DO:
   RUN GET_REM_BY_PRM (ipContractChar,
                      ipContCodeChar,
                      vCurrCalcChar,  /* ���� ���᫥��� */
                      ipBegDate,
                      ipEndDate,
                      cod-par,
                      loan.currency,
                      MIN(ipEndDate,loan.since),
                      mRecalcLoan,
                      mRecalcPP).
   IF     vNeedGraf 
      AND loan.since LT ipEndDate THEN
   DO:
      RUN GET_REM_BY_TERM (ipContractChar,
                           ipContCodeChar,
                           "2",              /* ��� "�������� �㬬" */
                           MAX(ipBegDate,loan.since + IF vIshOst THEN 1 ELSE 0),
                           ipEndDate,
                           loan.currency,
                           NO).
   END.
END.
/* ��ନ஢���� ���⪠ �� ������� ��魮��� �������. */
ELSE RUN GET_REM_BY_TERM (ipContractChar,
                          ipContCodeChar,
                          "2",              /* ��� "�������� �㬬" */
                          ipBegDate,
                          ipEndDate,
                          loan.currency,no).

/* ����祭�� ���� �����ᨨ */
RUN GetCodeRate (
    cod-par,            /* ���浪��� ����� %%, ���� loan.interest[cod-par] */
    OUTPUT vRateChar,   /* ��� ���/�����ᨨ */
    BUFFER loan).

/* ��ନ஢���� ��業⭮� �⠢�� �� �������� ���⪠. */
RUN GET_COMM_BY_REM (vRateChar,             /* ��� �����ᨨ/��� */
                     ipContractChar + "," + ipContCodeChar, /* KAU */
                     ipBegDate,
                     ipEndDate).

/* ���᫥��� � �ନ஢���� १����. */
RUN GET_NAC_REP (ipSchRecid,
                 ipBegDate,
                 ipEndDate,
                 mFormRasch).

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
           loan-par.amt-id EQ INT64(ENTRY(cod-par, vAllRateChar)) + CodostPar 
NO-LOCK NO-ERROR.

/* ���४��㥬 ���� */
FOR EACH otch1:
   otch1.amt-id = IF AVAIL loan-par THEN loan-par.amt-id
                                    ELSE ?.
   IF otch1.comment EQ "" THEN
      otch1.comment =  IF AVAIL loan-par THEN loan-par.name 
                                         ELSE otch1.comment.
END.

{intrface.del}

RETURN.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:46:33.175+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='lnremvar.p' */
/*prosignwAkthN5qdOCybaqsXSiQiA*/