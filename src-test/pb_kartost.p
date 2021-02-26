/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      
�� ������:     ���� �� ���⪠� �� ����⥪��
��ࠬ����:      
���� ����᪠:  
������:         19.06.2018 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */
{intrface.get xclass}
{intrface.get netw}     /** ��ࠢ�� � bispc */

{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDR   AS CHARACTER NO-UNDO.
DEFINE VARIABLE nPos1 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nPos2 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nPos3 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cN1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cINN  AS CHARACTER NO-UNDO.

cFl = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cFl = "./kartost-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
PUT UNFORMATTED XLHead("acct", "CCCDCNDNNNC", "112,300,150,105,61,90,78,90,90,90,300").

cXL = XLCellHead("���ࠧ�������",0,0,0)
    + XLCellHead("������",0,0,0)
    + XLCellHead("��楢�� ���",0,0,0)
    + XLCellHead("��� ������",0,0,0)
    + XLCellHead("������",0,0,0)
    + XLCellHead("���⮪",0,0,0)
    + XLCellHead("��� ��᫥���� ����樨",0,0,0)
    + XLCellHead("47423",0,0,0)
    + XLCellHead("����⥪� 2",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("�⢥��⢥��� �������� ���",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tmprecid 
    NO-LOCK,
FIRST acct
    WHERE (RECID(acct)   = tmprecid.id)
      AND (acct.currency = "")
    NO-LOCK
    BY acct.branch-id
    BY acct.cust-cat DESC
    BY acct.cust-id:

    nPos1 = 0.0.
    nPos2 = 0.0.
    nPos3 = 0.0.

    cDR   = GetLinks("acctb", acct.acct + "," + acct.currency, "S", "acct47423", ",", ?).
    IF (cDR NE "")
    THEN DO:
        RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "�").
        nPos3 = sh-bal.
    END.

    cDR   = GetXAttrValue("acct", acct.acct + "," + acct.currency, "����2�����").
    IF (cDR NE "")
    THEN DO:
        RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "�").
        nPos2 = sh-bal.
    END.

    cDR   = GetXAttrValue("acct", acct.acct + "," + acct.currency, "���⁂����").
    IF (cDR NE "")
    THEN DO:
        RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "�").
        nPos1 = sh-bal.
    END.

    RUN GetCustName IN h_base (acct.cust-cat, acct.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cINN).
    FIND FIRST blockobject
        WHERE (blockobject.file-name     EQ 'acct')
          AND (blockobject.class-code    EQ 'BlockAcct')
          AND CAN-DO("������,������", blockobject.block-type)
          AND (blockobject.beg-datetime  LE DATETIME(TODAY, MTIME))
          AND ((blockobject.end-datetime EQ ?)
            OR (blockobject.end-datetime GE DATETIME(TODAY, MTIME)))
          AND (blockobject.surrogate     EQ acct.acct + "," + acct.currency)
        NO-LOCK NO-ERROR.
    RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "�").

    cDR   = GetXAttrValue(IF (acct.cust-cat = "�") THEN "cust-corp" ELSE "person", STRING(acct.cust-id), "�ਢ��祭��").
    FIND FIRST code
        WHERE (code.class   = "�ਢ��祭��")
          AND (code.parent  = "�ਢ��祭��")
          AND (code.code    = cDR)
        NO-LOCK NO-ERROR.

    cXL = XLCell(acct.branch-id)
        + XLCellWrap(TRIM(cN1 + " " + cN2))
        + XLCell(acct.number)
        + XLDateCell(acct.open-date)
        + XLCell(IF (AVAIL blockobject) THEN blockobject.block-type ELSE "")
        + XLNumCell(- sh-bal)
        + XLDateCell(lastmove)
        + XLNumCell(nPos3)
        + XLNumCell(nPos2)
        + XLNumCell(nPos1)
        + XLCell(IF (AVAIL code) THEN code.name ELSE "")
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
