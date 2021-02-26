/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �� 
�� ������:     ���� �� �����஢���
��ࠬ����:      ��� ������ = "�" ��� "�"
���� ����᪠:  ����� �� ����⥪��
������:         21.11.2017 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{intrface.get netw}     /** ��ࠢ�� � bispc */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

DEFINE INPUT  PARAMETER iPar    AS CHARACTER    NO-UNDO.

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMask AS CHARACTER NO-UNDO.
DEFINE VARIABLE I     AS INTEGER   NO-UNDO.

cFl = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cFl = "./repblk-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).
cMask = IF (iPar = "�") THEN "405*,406*,407*,40802*,40807*" ELSE "40817*,40820*,423*,426*".
I     = 0.

/******************************************* ��������� */
FUNCTION ClientName RETURNS CHARACTER
   (INPUT  iCat     AS CHARACTER,
    INPUT  iId      AS INT64,
    INPUT  iAcct    AS CHARACTER ).

    IF (iCat = "�")
    THEN DO:
        FOR FIRST cust-corp
            WHERE (cust-corp.cust-id = iId)
            NO-LOCK:

            RETURN cust-corp.name-short.
        END.
    END.
    ELSE IF (iCat = "�")
    THEN DO:
        FOR EACH person
            WHERE (person.person-id = iId)
            NO-LOCK:

            RETURN (IF CAN-DO("40802*", iAcct) THEN "�� " ELSE "") + person.name-last + " " + person.first-names.
        END.
    END.
    RETURN "".
END FUNCTION.
/******************************************* ��������� */
PUT UNFORMATTED XLHead(IF (iPar = "�") THEN "��" ELSE "��", "ICCCCNCCCCCD", "46,56,300,150,106,130,123,53,150,150,52,108").
cXL = XLCellHat('���� �� �����஢���, ��⠭������� �� ���� '
    + (IF (iPar = "�") THEN "�ਤ��᪨� ��� � �������㠫��� �।�ਭ���⥫��"
                       ELSE "䨧��᪨� ���")
    + " �� " + STRING(TODAY, "99.99.9999"),11).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("N �/�",0,0,0)
    + XLCellHead("����� ��㯯�",0,0,0)
    + XLCellHead("������������ ������",0,0,0)
    + XLCellHead("����� ���",0,0,0)
    + XLCellHead("������������ �����஢��",0,0,0)
    + XLCellHead("�㬬� �����஢��",0,0,0)
    + XLCellHead("��砫� ����⢨� �����஢��",0,0,0)
    + XLCellHead("�࣠�",0,0,0)
    + XLCellHead("��� ���⠭�������",0,0,0)
    + XLCellHead("���⠭�������",0,0,0)
    + XLCellHead("��� �࣠��",0,0,0)
    + XLCellHead("��� �� ���⠭�������",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH blockobject
    WHERE (blockobject.class-code   = 'BlockAcct')
      AND (blockobject.file-name    = 'acct')
      AND CAN-DO("������,������,�����㬬,���������,������", blockobject.block-type)
      AND CAN-DO(cMask, blockobject.surrogate)
      AND (blockobject.end-datetime = ?
        OR blockobject.end-datetime >= DATETIME(TODAY + 1))
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        = ENTRY(1,blockobject.surrogate))
      AND (acct.currency    = ENTRY(2,blockobject.surrogate))
      AND (acct.close-date  = ?)
      AND (acct.filial-id   = shFilial)
    NO-LOCK
    BY acct.acct
    BY blockobject.beg-datetime:

    put screen col 1 row 24 "��ࠡ��뢠���� " + acct.acct.
    I   = I + 1.
    cXL = XLNumCell(I)
        + XLCell(GetXAttrValue("acct", blockobject.surrogate, "groupOABS"))
        + XLCellWrap(ClientName(acct.cust-cat, acct.cust-id, acct.acct))
        + XLCell(acct.acct)
        + XLCell(blockobject.block-type)
        + XLNumCell(blockobject.val[3])
        + XLCell(STRING(blockobject.beg-datetime, "99.99.9999 HH:MM"))
        + XLCell(blockobject.txt[2])
        + XLCell(blockobject.txt[3])
        + XLCell(ENTRY(1, blockobject.txt[4], ";"))
        + XLCell(IF (NUM-ENTRIES(blockobject.txt[4], ";") >= 2) THEN ENTRY(2, blockobject.txt[4], ";") ELSE "")
        + XLCell(GetXAttrValue("blockobject", STRING(blockobject.BlockObjectID), "��⠐���"))
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
put screen col 1 row 24 color normal STRING(" ","X(80)").

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
