/*
               ������᪠� ��⥣�஢����� ��⥬� QBIS
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: innsgnch.p
      Comment: �஢�ઠ ����஫쭮�� ࠧ�鸞 ��� ���⥫�騪� ��� �����⥫�
               ��� ��⮤� ChkUpd �⠭���⭮� �࠭���樨
   Parameters: iChWhoseINN - �.�. "inn-send" ��� "inn-rec",
               iChScreenValue - ᮡ�⢥��� �࠭��� ���祭�� ���
      Created: 08.06.2004 10:51 sadm
     Modified: 08.06.2004 10:57 sadm
     Modified: <date> <who>
*/

{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get tmess}

DEF INPUT PARAMETER iChWhoseINN    AS CHAR NO-UNDO.
DEF INPUT PARAMETER iSurrObj       AS CHAR NO-UNDO. /* �����䨪��� ��ꥪ�. */
DEF INPUT PARAMETER iClass         AS CHAR NO-UNDO. /* ����� ��ꥪ�. */
DEF INPUT PARAMETER iChScreenValue AS CHAR NO-UNDO.

DEFINE VARIABLE mChCorrectInnSign AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMessage          AS CHARACTER NO-UNDO.

IF LENGTH(iChScreenValue) NE 5 THEN
DO:
   IF NOT (iChScreenValue = "0" OR
           fValidInnSignature(INPUT iChScreenValue, OUTPUT mChCorrectInnSign))
   THEN DO:
      iChWhoseINN = IF iChWhoseINN = "inn-send" THEN
                       "���⥫�騪�"
                    ELSE
                       "�����⥫�"
      .
      mMessage = IF {assigned mChCorrectInnSign}
                 THEN SUBSTITUTE("��᫥���� ���� ��� &1 ������ ����: &2",
                                 iChWhoseINN,
                                 QUOTER(mChCorrectInnSign))
                 ELSE SUBSTITUTE("����ୠ� ����� ��� &1",
                                 iChWhoseINN).
	/*   RUN Fill-SysMes IN h_tmess ("", "", "0", mMessage).*/
      RETURN ERROR mMessage.
   END.
END.
RUN chkstoplistdr.p (iChWhoseINN,iSurrObj,iClass,iChScreenValue).
{intrface.del}          /* ���㧪� �����㬥����. */ 

RETURN "".
/* $LINTFILE='innsgnch.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:41.926+03:00' */
/*prosign2hyRJoF0Iqp6CHTX5G52qQ*/