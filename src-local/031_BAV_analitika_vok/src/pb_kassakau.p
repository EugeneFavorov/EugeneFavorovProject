/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�� ������:     ��ࠢ��� 㤠����� ���㬥��� ���, �� �������騥 � ॥���
��� ࠡ�⠥�:   ��⠭�������� �� "�� ᬥ��", kau �� �஢���� � ᮧ���� �㡠������᪨� �஢����
��ࠬ����:      ��ࠡ��뢠�� �⮡࠭��/����祭�� ���㬥���, ����訢��� N ᬥ��
���� ����᪠:  ��� - ���㬥��� - Ctrl-G - ��ࠢ����� 㤠������ ���㬥�⮢
������:         11.10.2016 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{intrface.get xclass}
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */

DEFINE VARIABLE cSmena      AS CHARACTER    NO-UNDO.

RUN g-prompt.p ("integer", "�����", ">>>>>>>9", ",",
                "������ ����� ᬥ��", 30, ",","",?,?,OUTPUT cSmena).
IF (cSmena EQ ?) THEN RETURN.

FOR EACH tmprecid 
    NO-LOCK,
FIRST op
    WHERE (RECID(op)        EQ tmprecid.id)
      AND (op.op-status     EQ "��")
    NO-LOCK,
FIRST op-entry OF op
    WHERE (op-entry.acct-db BEGINS "20202")
      AND (op-entry.acct-cr BEGINS "20202")
    EXCLUSIVE-LOCK:

    cSmena = ENTRY(1, cSmena).
    op-entry.kau-db = cSmena.
    op-entry.kau-cr = cSmena.
    UpdateSigns ("op", STRING(op.op), "dpr-id", cSmena, YES).

    CREATE kau-entry.
    ASSIGN
        kau-entry.op        = op.op
        kau-entry.op-entry  = op-entry.op-entry
        kau-entry.kau-entry = 1
        kau-entry.kau       = cSmena
        kau-entry.acct      = op-entry.acct-db
        kau-entry.currency  = ""
        kau-entry.qty       = 0
        kau-entry.amt-cur   = 0
        kau-entry.amt-rub   = op-entry.amt-rub
        kau-entry.op-code   = "000000"
        kau-entry.contract-date = op.contract-date
        kau-entry.op-status = op.op-status
        kau-entry.kau-id    = "�������낎�".
        kau-entry.value-date = op-entry.value-date.
        kau-entry.user-id   = op.user-id.
        .
    CREATE kau-entry.
    ASSIGN
        kau-entry.op        = op.op
        kau-entry.op-entry  = op-entry.op-entry
        kau-entry.kau-entry = 2
        kau-entry.kau       = cSmena
        kau-entry.acct      = op-entry.acct-cr
        kau-entry.currency  = op-entry.currency
        kau-entry.qty       = 0
        kau-entry.amt-cur   = op-entry.amt-cur
        kau-entry.amt-rub   = op-entry.amt-rub
        kau-entry.op-code   = "000000"
        kau-entry.contract-date = op.contract-date
        kau-entry.op-status = op.op-status
        kau-entry.kau-id    = "�������낎�".
        kau-entry.value-date = op-entry.value-date.
        kau-entry.user-id   = op.user-id.
        .
END.
{intrface.del}          /* ���㧪� �����㬥����. */
