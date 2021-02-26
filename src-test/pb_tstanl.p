/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�� ������:     �஢�ઠ ᮮ⢥��⢨� �����⨪� � ���⪠ �� ����⥪�
���� ����᪠:  
������:         24.06.2016 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{sh-defs.i}             /* ���⮪ �� ��� */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
{intrface.get netw}     /* ��ࠢ�� � bispc */

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE nSumKau     AS DECIMAL      NO-UNDO.
DEFINE VARIABLE nTmp        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE cXL         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFl         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cPr         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iNum        AS INTEGER      NO-UNDO INIT 0.

cFl = "./tstanl.xml".
OUTPUT TO VALUE(cFl).

/******************************************* ��������� */
PUT UNFORMATTED XLHead("����⥪�", "CCNNC", "59,150,83,83,450").

cXL = XLCellHead("������",0,0,0)
    + XLCellHead("��� ����⥪�",0,0,0)
    + XLCellHead("���⮪",0,0,0)
    + XLCellHead("�����⨪�",0,0,0)
    + XLCellHead("�㬬� � ���.���㬥���",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH acct
    WHERE (acct.acct    BEGINS "9090")
      AND CAN-DO("���⁫,����2", acct.contract)
      AND (acct.close-date  EQ ?)
      AND (acct.filial-id   NE "0400")
    NO-LOCK
    BY acct.filial-id
    BY acct.acct:

    put screen col 1 row 24 "��ࠡ��뢠����: " + acct.filial-id + " - " + acct.number.
    nSumKau = 0.
    cPr     = "".
    FOR EACH kau
        WHERE (kau.acct     EQ acct.acct)
          AND (kau.currency EQ acct.currency)
          AND (kau.zero-bal EQ no)
        NO-LOCK:

        nSumKau = nSumKau + kau.balance.
        cDR = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").
        FIND FIRST op-entry
            WHERE (op-entry.op  = INT64(cDR))
            NO-LOCK NO-ERROR.
        cDR  = GetXAttrValue("op", cDR, "amt-rub").
        nTmp = DEC(cDR) NO-ERROR.
        IF (AVAIL op-entry) AND (op-entry.amt-rub <> DEC(TRIM(ENTRY(3, kau.sort))))
        THEN cPr = cPr + (IF (cPr = "") THEN "" ELSE "~n")
                 + "� �����⨪� " + ENTRY(2, kau.sort) + "," + TRIM(ENTRY(3, kau.sort)) + " �㬬� ���.�஢���� = " + STRING(op-entry.amt-rub).
        IF (nTmp <> DEC(TRIM(ENTRY(3, kau.sort))))
        THEN cPr = cPr + (IF (cPr = "") THEN "" ELSE "~n")
                 + "� �����⨪� " + ENTRY(2, kau.sort) + "," + TRIM(ENTRY(3, kau.sort)) + " ���.४. amt-rub = " + cDR.
    END.
    RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "�").

    IF (sh-bal NE nSumKau) OR (cPr NE "")
    THEN DO:
        cXL = XLCell(acct.filial-id)
            + XLCell(acct.number)
            + XLNumCell(sh-bal)
            + XLNumCell(nSumKau)
            + XLCellWrap(cPr)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        iNum = iNum + 1.
    END.
/*  ELSE DO:
        cXL = XLCell(acct.filial-id)
            + XLCell(acct.number)
            + XLNumCell(sh-bal)
            + XLNumCell(nSumKau)
            + XLCell("OK")
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END. */
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
put screen col 1 row 24 color normal STRING(" ","X(80)").

IF (iNum EQ 0)
THEN MESSAGE "�訡�� � ����⥪�� �� �����㦥��."
    VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
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
