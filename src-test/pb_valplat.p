{globals.i}             /** �������� ��।������ */
{intrface.get netw}     /** ��ࠢ�� � bispc */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE BUFFER   oe    FOR op-entry.
{getdates.i}

cFl = "./valplat.xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
RUN XLAddStyle('<Style ss:ID="s80"><Alignment ss:Horizontal="Center" ss:Vertical="Top"/></Style>').
PUT UNFORMATTED XLHead("op", "DCCNCCCC", "71,150,150,82,57,250,450,100").

cXL = XLCellHead("���",0,0,0)
    + XLCellHead("�����",0,0,0)
    + XLCellHead("�।��",0,0,0)
    + XLCellHead("�㬬�",0,0,0)
    + XLCellHead("�����",0,0,0)
    + XLCellHead("���⥫�騪",0,0,0)
    + XLCellHead("�����祭��",0,0,0)
    + XLCellHead("�ਧ��� 407�",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH op
    WHERE (op.op-date   >= beg-date)
      AND (op.op-date   <= end-date)
      AND (op.filial-id = shFilial)
/*    AND (op.details   BEGINS "��ॢ�� ��")
*/    AND (op.op-kind   = "TAG_103")
    NO-LOCK,
FIRST oe OF op
    WHERE (CAN-DO("!.....810*,40817*,40820*,423*,426*", oe.acct-db)
      AND  CAN-DO("!.....810*,301*,303*", oe.acct-cr))
      AND CAN-DO("840,978,398", oe.currency)
    NO-LOCK
    BY op.op-date:

    cXL = XLDateCell(op.op-date)
        + XLCell(SUBSTRING(oe.acct-db,1,20))
        + XLCell(SUBSTRING(oe.acct-cr,1,20))
        + XLNumCell(oe.amt-cur)
        + XLCell(ENTRY(LOOKUP(oe.currency, "840,978,398"), "USD,EUR,KZT"))
        + XLCellWrap(GetXAttrValue("op", STRING(op.op), "name-send"))
        + XLCellWrap(op.details)
        + XLCellStyle("s80", IF (GetXAttrValue("op", STRING(op.op), "F407") = "") THEN "" ELSE "*")
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* ��। ��ࠢ��� ���� �஢�ਬ, ����饭 �� bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
END.
/* ��ࠢ�塞 ��⮪�� */
RUN sndbispc.p ("file=" + cFl + ";class=bq").
{intrface.del}
