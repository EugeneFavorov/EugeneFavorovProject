/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
���������:     comgroup.p
�᭮�����:      �� �����.511
�� ������:     ������ ������� � ����筮� ����� �� ��
                � ��।������ 1,2,3 � �� ��⠬ � �����஢���� ������ � ������
��� ࠡ�⠥�:   
��ࠬ����:      
���� ����᪠:  ����-��ࠡ�⪠ �� ᬥ�� �����
������:         07.04.2017 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get blkob}
{intrface.get tmess}
{topkind.def}

DEFINE INPUT  PARAMETER iop     AS INT64        NO-UNDO.    /* ���㬥�� */
DEFINE INPUT  PARAMETER iParam  AS CHARACTER    NO-UNDO.    /* <�����ᨮ���� �࠭�����>;<��᪠ ��>;<��᪠ ��> */

DEFINE VARIABLE cBlock      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTranz      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lOk         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER    NO-UNDO.
DEFINE BUFFER   oe      FOR op-entry.

FOR FIRST op
    WHERE (op.op    EQ iop)
    NO-LOCK,
FIRST oe OF op
    WHERE CAN-DO(ENTRY(2, iParam, ";"), oe.acct-db)
      AND CAN-DO(ENTRY(3, iParam, ";"), oe.acct-cr)
    NO-LOCK,
FIRST history
    WHERE (history.file-name    EQ "op")
      AND (history.field-ref    EQ STRING(iop))
      AND (history.modify       EQ 'C')
    NO-LOCK:

    /* �᫨ 㦥 ���� ������� - �ய�᪠�� */
    IF (GetLinks(op.class-code, STRING(iop), ?, "�����", ",", op.op-date) NE "") THEN RETURN.

    cBlock = BlockAcct(oe.acct-db + "," + oe.currency, DATETIME(op.op-date, IF (op.op-date EQ TODAY) THEN TIME ELSE 0)).

    IF      CAN-DO("1,2,3", op.order-pay)
        OR  CAN-DO(cBlock, "������")
        OR  CAN-DO(cBlock, "������")
    THEN DO:
        cErrMsg = "���㬥�� ��ॢ���� � ����� " + op.op-status + "~n����塞 �������.".
        RUN Fill-SysMes IN h_tmess("", "", "1", cErrMsg).

        /* ����� �࠭���樨  �� */
        {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
        ASSIGN
            lOk =   TDAddParam("_OpMain", STRING(iop))
                AND TDAddParam("_OpDate", STRING(history.modif-date, "99.99.99"))
                AND TDAddParam("_OpTime", STRING(history.modif-time, "HH:MM:SS"))
            NO-ERROR.
        RUN ex-trans.p (ENTRY(1, iParam, ";"), op.op-date, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).

        IF NOT lOk
        THEN DO:
            cErrMsg = "�訡��: ������� �� ���᫥�� - " + cErrMsg.
            RUN Fill-SysMes IN h_tmess("", "", "-1", cErrMsg).
        END.
    END.
END.
{intrface.del}
