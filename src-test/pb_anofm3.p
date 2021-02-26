{globals.i}             /** �������� ��।������ */
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get netw}     /** ��ࠢ�� � bispc */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.

cFl = "./analizofm3.xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
PUT UNFORMATTED XLHead("acct", "CCCCCD", "40,50,400,150,60,71").

cXL = XLCellHead("���",0,0,0)
    + XLCellHead("ID",0,0,0)
    + XLCellHead("������������",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("������",0,0,0)
    + XLCellHead("������",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH cust-corp
    NO-LOCK
    BY cust-corp.cust-id:

    IF (GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "���������") <> "3") THEN NEXT.
    put screen col 1 row 24 "��ࠡ��뢠���� �� " + STRING(cust-corp.cust-id) + "     ".
    FOR EACH acct
        WHERE (acct.cust-cat    = "�")
          AND (acct.cust-id     = cust-corp.cust-id)
          AND CAN-DO("�����,�����", acct.contract)
          AND (acct.close-date  = ?
            OR acct.close-date  >= 09/01/2017)
        NO-LOCK:

        cXL = XLCell("��")
            + XLCell(STRING(cust-corp.cust-id))
            + XLCell(cust-corp.name-short)
            + XLCell(acct.number)
            + XLCell(acct.filial-id)
            + XLDateCell(acct.close-date)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
END.

FOR EACH person
    NO-LOCK
    BY person.person-id:

    IF (GetXAttrValue("person", STRING(person.person-id), "���������") <> "3") THEN NEXT.
    put screen col 1 row 24 "��ࠡ��뢠���� �� " + STRING(person.person-id) + "     ".
    FOR EACH acct
        WHERE (acct.cust-cat    = "�")
          AND (acct.cust-id     = person.person-id)
          AND CAN-DO("�����,�����", acct.contract)
          AND (acct.close-date  = ?
            OR acct.close-date  >= 09/01/2017)
        NO-LOCK:

        cXL = XLCell(IF (GetXAttrValue("person", STRING(person.person-id), "�।��") = "�।��") THEN "��" ELSE "��")
            + XLCell(STRING(person.person-id))
            + XLCell(person.name-last + " " + person.first-names)
            + XLCell(acct.number)
            + XLCell(acct.filial-id)
            + XLDateCell(acct.close-date)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
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
