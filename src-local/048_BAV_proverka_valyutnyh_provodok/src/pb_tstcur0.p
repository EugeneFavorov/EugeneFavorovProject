/* ����஢���� ������� �஢���� �� ��⠬ �� */

{globals.i}             /** �������� ��।������ */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{intrface.get instrum}  /** �㭪樨 ��� ࠡ��� � ���ᠬ� */
{intrface.get netw}     /* ��ࠢ�� � bispc */

DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cDb         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCr         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
DEFINE BUFFER   oe      FOR op-entry.

{getdates.i}

cFl = "./tstcur0.xml".
OUTPUT TO VALUE(cFl).

PUT UNFORMATTED XLHead("acct", "DCCIICCNNC", "71,59,57,60,60,145,145,100,100,400").
cXL = XLCellHead("���",0,0,0)
    + XLCellHead("������",0,0,0)
    + XLCellHead("�����",0,0,0)
    + XLCellHead("op",0,0,0)
    + XLCellHead("op-entry",0,0,0)
    + XLCellHead("��� ��",0,0,0)
    + XLCellHead("��� ��",0,0,0)
    + XLCellHead("�㬬� ��",0,0,0)
    + XLCellHead("�㬬� ���",0,0,0)
    + XLCellHead("�����祭��",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

cDb = "*".
cCr = "*".
FOR EACH code
    WHERE (code.class   EQ 'TstCur0')
      AND (code.parent  EQ 'TstCur0')
    NO-LOCK:

    IF (code.name EQ "��")
    THEN cDb = "!" + code.val + " *," + cDb.
    ELSE cCr = "!" + code.val + " *," + cCr.
END.

I = 0.
FOR EACH op
    WHERE (op.op-date   GE beg-date)
      AND (op.op-date   LE end-date)
      AND CAN-FIND (FIRST op-entry OF op
                        WHERE op-entry.currency NE ""
                          AND CAN-DO(cDb, op-entry.acct-db)
                          AND CAN-DO(cCr, op-entry.acct-cr)
                          AND op-entry.amt-cur  EQ 0
                          AND op-entry.amt-rub  NE 0)
      AND CAN-DO("!*��८業��*,!*���ᮢ�� ࠧ���*,*", op.details)
    NO-LOCK
    BY op.filial-id
    BY op.op-date:

    FOR EACH oe OF op
        NO-LOCK
        BY oe.op-entry:

        cXL = XLDateCell(oe.op-date)
            + XLCell(op.filial-id)
            + XLCell(oe.currency)
            + XLNumCell(oe.op)
            + XLNumCell(oe.op-entry)
            + XLCell(IF (oe.acct-db EQ ?) THEN "" ELSE oe.acct-db)
            + XLCell(IF (oe.acct-cr EQ ?) THEN "" ELSE oe.acct-cr)
            + XLNumCell(oe.amt-rub)
            + XLNumCell(oe.amt-cur)
            + XLCellWrap(op.details)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
    I = I + 1.
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* **************************************************************************** */
IF (I EQ 0)
THEN MESSAGE "��� �㫥��� ������� �஢����"
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
ELSE DO:
    /* ��। ��ࠢ��� ��⮪��� �஢�ਬ, ����饭 �� bispc */
    DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
    DO WHILE (mRet EQ ""):
        RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
        IF (mRet EQ "")
        THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
    END.
    /* ��ࠢ�塞 ��⮪�� */
    RUN sndbispc.p ("file=" + cFl + ";class=bq").
END.
{intrface.del}
