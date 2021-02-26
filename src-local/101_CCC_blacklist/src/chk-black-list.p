/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: chk-black-list.p
      Comment: ��楤�� ����஫� � ��� ᯨ᪮� ���㬥�⮢ (black-list)
               �����⮢ ���⥦���� ���㬥��
   Parameters: ���
         Uses:
      Used by:
      Created: 25.05.2015 paus
     Modified: 
*/
{globals.i}
{intrface.get tmess}
/* ��⠢�� ���� ���� */
{intrface.get cust}
{intrface.get trans}
{intrface.get pbase}
{wordwrap.def}
{stoplist.fun}
/* ����� ��⠢�� ���� ���� */

DEFINE INPUT PARAMETER iOp     AS INT64 NO-UNDO. /* ����७��� ����� ���㬥�� */
DEFINE INPUT PARAMETER iTypMes AS CHARACTER NO-UNDO. /* ���ᮡ ᮮ�饭�� */
DEFINE OUTPUT PARAMETER oRes   AS LOGICAL NO-UNDO. /* ������� �஢�ન */
DEFINE STREAM fil.
/* ��⠢�� ���� ���� */
DEFINE VARIABLE mOpKind   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmplID   AS INT64     NO-UNDO.
DEFINE VARIABLE mTranAuto AS CHARACTER NO-UNDO.

DEFINE VARIABLE mDbgPrint AS LOGICAL NO-UNDO.
mDbgPrint = NO.
{chk-black-list.pro}

ASSIGN
   mTranAuto = FGetSetting("black-list","TranAuto","")
   mOpKind   = GetBaseOpKind()
   mTmplID   = GetBaseTemplate().

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
   "~nmTranAuto = " + mTranAuto + 
   "~nmOpKind = " + mOpKind + 
   "~nmTmplID = " + STRING(mTmplID)).

IF  GetSysConf("DPS-PERS-PROXY") NE ?
AND GetSysConf("DPS-PERS-PROXY") NE "" THEN
DO:
   oRes = NO.
   RETURN.
END.

IF CAN-DO(mTranAuto,mOpKind) THEN
DO:
   oRes = NO.
   RETURN.
END.
 
/* ����� ��⠢�� ���� ���� */

/* ����᪠�� ����� ��楤��� �஢�ન ���㬥�� */
RUN chk-black-all IN THIS-PROCEDURE(iOp, OUTPUT TABLE tt-black-list).

IF CAN-FIND(FIRST tt-black-list)  THEN DO:
   {setdest.i &stream="stream fil" &filename="'chkblacklist.txt'" &cols=125}
   PUT STREAM fil UNFORMATTED
      "������������������������������������������������������������������������������Ŀ" SKIP
      "�         ���                  � ��� ���㬥�� ����� � ����೏�稭� ����ᥭ��" SKIP
      '�                              �               �             �� "��� ᯨ᮪"�' SKIP.
    
   FOR EACH tt-black-list NO-LOCK:
      PUT STREAM fil UNFORMATTED
      "������������������������������������������������������������������������������Ĵ" skip
                                              "�"
         tt-black-list.fio     FORMAT 'x(30)' "�"
         tt-black-list.typedoc FORMAT 'x(15)' "�"
         tt-black-list.numdoc  FORMAT 'x(13)' "�"
         tt-black-list.cause   FORMAT 'x(17)' "�"
      SKIP.
   END.
   PUT STREAM fil UNFORMATTED
      "��������������������������������������������������������������������������������" SKIP.
   {preview.i &stream="stream fil" &filename="'chkblacklist.txt'"}
   oRes = YES. /* ��⠭�������� 䫠� ᮢ������� ���⭨�� ����樨 � black-list */

   IF iTypMes =  "�����" THEN DO:
      /* ����� ���짮��⥫� */
      RUN Fill-SysMes IN h_tmess("","core61","","").
      IF pick-value = "yes" THEN
         oRes = NO.
   END.
   ELSE IF iTypMes =  "�����" THEN
      /* �뢮� ᮮ�饭�� (� ��⮪��) */
      RUN Fill-SysMes IN h_tmess("","core611","","").
END.

{intrface.del}
/* $LINTFILE='chk-black-list.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.200+03:00' */
/*prosigny9bNY8ksGWaf94zg3+W4Yg*/