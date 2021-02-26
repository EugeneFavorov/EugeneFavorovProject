/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
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
{chk-black-list.pro}
DEFINE INPUT PARAMETER iOp     AS INT64 NO-UNDO. /* ����७��� ����� ���㬥�� */
DEFINE INPUT PARAMETER iTypMes AS CHARACTER NO-UNDO. /* ���ᮡ ᮮ�饭�� */
DEFINE OUTPUT PARAMETER oRes   AS LOGICAL NO-UNDO. /* ������� �஢�ન */
DEFINE STREAM fil.

 /* ����᪠�� ����� ��楤��� �஢�ન ���㬥�� */
 RUN chk-black-all IN THIS-PROCEDURE(iOp, OUTPUT TABLE tt-black-list).

 IF CAN-FIND(FIRST tt-black-list)  THEN DO:
    PUT  UNFORMATTED
      "������������������������������������������������������������������������������Ŀ" SKIP
      "�         ���                  � ��� ���㬥�� ����� � ����೏�稭� ����ᥭ��" SKIP
      '�                              �               �             �� "��� ᯨ᮪"�' SKIP.
    
    FOR EACH tt-black-list NO-LOCK:
      PUT UNFORMATTED
      "������������������������������������������������������������������������������Ĵ" skip
                                              "�"
         tt-black-list.fio     FORMAT 'x(30)' "�"
         tt-black-list.typedoc FORMAT 'x(15)' "�"
         tt-black-list.numdoc  FORMAT 'x(13)' "�"
         tt-black-list.cause   FORMAT 'x(17)' "�"
      SKIP.
    END.
    PUT  UNFORMATTED
      "��������������������������������������������������������������������������������" SKIP.

    oRes = YES. /* ��⠭�������� 䫠� ᮢ������� ���⭨�� ����樨 � black-list */
    oRes = NO.
    IF iTypMes EQ "�����" THEN DO:
     /* ����� ���짮��⥫� */
      RUN Fill-SysMes IN h_tmess("","core61","","").
      IF pick-value = "yes" THEN
         oRes = NO.
    END.
    ELSE IF iTypMes EQ "�����" THEN
      /* �뢮� ᮮ�饭�� (� ��⮪��) */
      RUN Fill-SysMes IN h_tmess("","core611","","").
 END.

{intrface.del}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='01/07/2015 14:29:31.716+04:00' */
/* $LINTUSER='paus' */
/* $LINTMODE='1' */
/* $LINTFILE='chk-black-list.p' */
/*prosign+AFp2i+5bo8jUwBn8oDEdg*/