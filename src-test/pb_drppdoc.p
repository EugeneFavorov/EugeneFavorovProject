/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      ���쬮 ��ᨭ���� �.�. � ������������ �.�. �� 25.06.2018
�� ������:     ���⠢��� �� ppdoc = "2" �� ����稨 ����� ��
���� ����᪠:  ��楤��늮��� - ���⎡ࠡ�
������:         25.06.2018 ���ᮢ �.�.
*/

{intrface.get xclass}

DEFINE INPUT  PARAMETER iop     AS INT64    NO-UNDO.    /* ���㬥�� */
DEFINE INPUT  PARAMETER iParam  AS CHAR     NO-UNDO.
iParam = "~{VO" + REPLACE(iParam, ",", "}*,~{VO") + "}*".

FOR EACH op
    WHERE (op.op = iop)
      AND CAN-DO(iParam, op.details)
    NO-LOCK:

    UpdateSigns("op", STRING(iop), "ppdoc", "2", YES).
END.

{intrface.del}
