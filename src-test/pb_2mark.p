/**
�� ������:     ��ॢ���� �뤥����� �஢���� � ����� "���"
���� ����᪠:  ���᮪ ���㬥�⮢ - Ctrl-G
������:         14.02.2018 ���ᮢ �.�.
*/

{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */

FOR EACH tmprecid 
    NO-LOCK,
FIRST op
    WHERE (RECID(op) EQ tmprecid.id)
    EXCLUSIVE-LOCK:

    MESSAGE "���㬥�� - " op.doc-num op.op-status
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
    op.op-status = "�".
END.
