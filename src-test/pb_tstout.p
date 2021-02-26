/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �� 
�� ������:     ����஫� ���譨� ���㬥�⮢: ��� ���.� ��� ������
���� ����᪠:  ��楤�� ����஫� �� �室� ���
������:         16.08.2017 ���ᮢ �.�.
*/

DEFINE PARAMETER BUFFER Op FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INIT YES.  /* �� 㬮�砭�� - ᬥ�� ����� */

{globals.i}
{intrface.get tmess}

FOR FIRST op-entry OF op
    WHERE (op-entry.acct-cr BEGINS "30102")
    NO-LOCK:

    IF (op.doc-date > op.op-date)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "��� ���㬥�� ����� ���� ���भ�!").
        oResult = ?.
    END.
    ELSE IF (LENGTH(op.name-ben) > 160)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "���� ������ > 160 ᨬ�����!").
        oResult = ?.
    END.
    ELSE IF (op.ben-acct = "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "3", "��� ������ ���⮩.|�த������,�⬥����").
        IF pick-value <> "1" THEN oResult = ?.
    END.
END.
{intrface.del}
RETURN IF (oResult = ?) THEN "�⪠�" ELSE "".
