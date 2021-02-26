/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�� ������:     ���⠢��� �� �������� �� ����_�������� � ����_��⠇��
��� ࠡ�⠥�:   �� �뤥����� ������ࠬ ��
���� ����᪠:  ���᮪ ������஢ - Ctrl-G
������:         30.01.2018 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get tmess}

DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iK          AS INTEGER      NO-UNDO INIT 0.

FOR EACH tmprecid 
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan)      = tmprecid.id)
      AND (loan.cust-cat    = "�")
    NO-LOCK:

    cDR = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "PLDealID").
    IF (cDR <> "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������� " + loan.doc-ref + " ���⠢��� �� '��������: ����� ���' = " + cDR).
        NEXT.
    END.

    cDR = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "PLDealDate").
    IF (cDR <> "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������� " + loan.doc-ref + " ���⠢��� �� '��������: ��� ���' = " + cDR).
        NEXT.
    END.

    cDR = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "����_��������").
    IF (cDR <> "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������� " + loan.doc-ref + " ���⠢��� �� '����: ����� ���' = " + cDR).
        NEXT.
    END.

    UpdateSigns("loan", loan.contract + "," + loan.cont-code, "����_��������", "�" + loan.doc-ref, NO).
    UpdateSigns("loan", loan.contract + "," + loan.cont-code, "����_��⠇��", STRING(loan.open-date), YES).
    iK = iK + 1.
END.

RUN Fill-SysMes IN h_tmess ("", "", "0", "���.४������ ��⠭������ �� " + STRING(iK) + " ��������").
{intrface.del}
