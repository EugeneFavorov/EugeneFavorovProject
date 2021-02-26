/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�� ������:     �⪫��뢠�� ��ॢ�� SWIFT �� "�����", �������� ��� �� �� 30220
��� ࠡ�⠥�:   �� �뤥����� ���㬥�⠬
���� ����᪠:  ���� Ctrl-G
������:         16.02.2018 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */
{intrface.get tmess}

DEFINE VARIABLE iSw         AS INTEGER      NO-UNDO INIT 0.
DEFINE BUFFER   opsw    FOR op.
DEFINE BUFFER   oesw    FOR op-entry.

FOR EACH tmprecid 
    NO-LOCK,
FIRST opsw
    WHERE (RECID(opsw) EQ tmprecid.id)
      AND (opsw.op-status   = "����")
    EXCLUSIVE-LOCK,
FIRST oesw OF opsw
    WHERE (oesw.acct-cr     BEGINS "3011")
    EXCLUSIVE-LOCK:

    FIND FIRST acct
        WHERE (acct.bal-acct    = 30220)
          AND (acct.currency    = oesw.currency)
          AND (acct.filial-id   = opsw.filial-id)
        NO-LOCK NO-ERROR.
    oesw.acct-cr    = acct.acct.
    opsw.op-status  = "�".
    iSw = iSw + 1.
END.

RUN Fill-SysMes IN h_tmess ("","","1","��ࠡ�⠭� " + STRING(iSw) + " ���㬥�⮢.").
{intrface.del}
