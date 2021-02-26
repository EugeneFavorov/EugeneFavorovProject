/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
���������:     
�᭮�����:      �����.838 ���� �� ������� �ਤ��᪨� ���
�� ������:     
��� ࠡ�⠥�:   
��ࠬ����:      ��� ���짮��⥫� - �⢥��⢥����� �� ���� ������
���� ����᪠:  
������:         01.01.2012 ���ᮢ �.�.
*/

DEFINE INPUT  PARAMETER iUser   AS CHARACTER    NO-UNDO.

{globals.i}             /** �������� ��।������ */
{intrface.get date}
{intrface.get xclass}   /** �㭪樨 ��� ࠡ��� � ����奬�� */
{intrface.get strng}    /** �㭪樨 ��� ࠡ��� � ��ப��� */
{intrface.get netw}     /* ��ࠢ�� � bispc */

{sh-defs.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE nDeb6   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nDeb1   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE nBud6   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dBegTek AS DATE      NO-UNDO.
DEFINE BUFFER   oe      FOR op-entry.
{getdate.i}

beg-date = kvart_beg(end-date).
dBegTek  = FirstMonDate(end-date).

cFl = "./nalogul.xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
PUT UNFORMATTED XLHead("acct", "CCCDNNNN", "325,80,150,81,153,153,153,73").

cXL = XLCellHead("������������ ������",0,0,0)
    + XLCellHead("���",0,0,0)
    + XLCellHead("����� ���",0,0,0)
    + XLCellHead("��� ������ ���⭮�� ���",0,0,0)
    + XLCellHead("����⮢� ����� �� ��᫥���� ����⠫",0,0,0)
    + XLCellHead("����⮢� ����� �� ��᫥���� �����",0,0,0)
    + XLCellHead("���⥦� � ��� �� ��᫥���� ����⠫",0,0,0)
    + XLCellHead("��� %",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE CAN-DO("�����,�����", acct.contract)
      AND (acct.currency        EQ "")
      AND (acct.filial-id       EQ shFilial)
      AND (acct.user-id         EQ iUser)
      AND (acct.cust-cat        EQ "�")
      AND (acct.open-date       LE end-date)
      AND (acct.close-date      EQ ?
        OR acct.close-date      GE beg-date)
    NO-LOCK,
FIRST cust-corp
    WHERE (cust-corp.cust-id    EQ acct.cust-id)
    NO-LOCK
    BY acct.acct:

    put screen col 1 row 24 "��ࠡ��뢠���� " + acct.number.
    nDeb6 = 0.0.
    nDeb1 = 0.0.
    nBud6 = 0.0.
    FOR EACH oe
        WHERE (oe.op-date   GE beg-date)
          AND (oe.op-date   LE end-date)
          AND (oe.acct-db   EQ acct.acct)
          AND NOT CAN-DO("41*,!427*,!428*,!429*,42*,!440*,44*,45*", oe.acct-cr)
        NO-LOCK,
    FIRST op OF oe
        NO-LOCK:

        nDeb6 = nDeb6 + oe.amt-rub.
        IF CAN-DO("401*,402*,404*", op.ben-acct)
        THEN nBud6 = nBud6 + oe.amt-rub.
    END.

    FOR EACH oe
        WHERE (oe.op-date   GE dBegTek)
          AND (oe.op-date   LE end-date)
          AND (oe.acct-db   EQ acct.acct)
        NO-LOCK,
    FIRST op OF oe
        NO-LOCK:

        nDeb1 = nDeb1 + oe.amt-rub.
    END.

    cXL = XLCell(cust-corp.cust-stat + " " + cust-corp.name-corp)
        + XLCell(cust-corp.inn)
        + XLCell(acct.number)
        + XLDateCell(acct.open-date)
        + XLNumCell(nDeb6)
        + XLNumCell(nDeb1)
        + XLNumCell(nBud6)
        + XLNumCell(IF (nDeb6 EQ 0.0) THEN 0.0 ELSE (nBud6 / nDeb6 * 100.0))
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
put screen col 1 row 24 color normal STRING(" ","X(80)").

/* ��। ��ࠢ��� ��⮪��� �஢�ਬ, ����饭 �� bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "������� �ணࠬ�� bispc � ������ ��" VIEW-AS ALERT-BOX.
END.
/* ��ࠢ�塞 ��⮪�� */
RUN sndbispc.p ("file=" + cFl + ";class=bq").
{intrface.del}
