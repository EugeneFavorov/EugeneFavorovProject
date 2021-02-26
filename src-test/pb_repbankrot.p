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
{sh-defs.i}

DEFINE INPUT  PARAMETER iPar    AS CHARACTER    NO-UNDO.

/******************************************* ��।������ ��६����� � ��. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMask AS CHARACTER NO-UNDO.
DEFINE VARIABLE I     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDR   AS CHARACTER NO-UNDO.
DEFINE VARIABLE n01   AS INTEGER   NO-UNDO.
DEFINE VARIABLE n02   AS INTEGER   NO-UNDO.
DEFINE VARIABLE s01   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE s02   AS DECIMAL   NO-UNDO.
DEFINE BUFFER kart  FOR acct.

cFl = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cFl = "./repbankrot-" + cFl + ".xml".
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
PUT UNFORMATTED XLHead(IF (iPar = "�") THEN "��" ELSE "��", "ICCCDNNINI", "46,300,213,52,71,97,97,84,97,84").
cXL = XLCellHat("���� �� �����⠬ �� " + STRING(TODAY, "99.99.9999"),9).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("N �/�",0,0,0)
    + XLCellHead("������������ ������",0,0,0)
    + XLCellHead("����� ���",0,0,0)
    + XLCellHead("Branch ���",0,0,0)
    + XLCellHead("��� ������ ���",0,0,0)
    + XLCellHead("���⮪ �� ⥪���� ����",0,0,0)
    + XLCellHead("���⮪ �� 90901*",0,0,0)
    + XLCellHead("���-�� ���㬥�⮢",0,0,0)
    + XLCellHead("���⮪ �� 90902*",0,0,0)
    + XLCellHead("���-�� ���㬥�⮢",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH blockobject
    WHERE (blockobject.class-code   = 'BlockAcct')
      AND (blockobject.file-name    = 'acct')
      AND CAN-DO("������,������", blockobject.block-type)
      AND CAN-DO(cMask, blockobject.surrogate)
      AND (blockobject.end-datetime = ?
        OR blockobject.end-datetime >= DATETIME(TODAY + 1))
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        = ENTRY(1,blockobject.surrogate))
      AND (acct.currency    = ENTRY(2,blockobject.surrogate))
      AND (acct.close-date  = ?)
    NO-LOCK
    BY acct.filial-id
    BY acct.acct:

    put screen col 1 row 24 "��ࠡ��뢠���� " + acct.acct.
    I   = I + 1.

    n01 = 0. n02 = 0. s01 = 0. s02 = 0.
    cDR = GetXAttrValue("acct", acct.acct + "," + acct.currency, "���⁂����").
    IF (cDR <> "")
    THEN DO:
        FOR EACH kart
            WHERE (kart.acct     = ENTRY(1, cDR))
              AND (kart.currency = ENTRY(2, cDR))
            NO-LOCK:

            RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "�").
            s01 = sh-bal.
            IF (s01 <> 0)
            THEN DO:
                FOR EACH kau
                    WHERE (kau.acct     = ENTRY(1, cDR))
                      AND (kau.currency = ENTRY(2, cDR))
                      AND (kau.zero-bal = no)
                    NO-LOCK:

                    n01 = n01 + 1.
                END.
            END.
        END.
    END.

    cDR = GetXAttrValue("acct", acct.acct + "," + acct.currency, "����2�����").
    IF (cDR <> "")
    THEN DO:
        FOR EACH kart
            WHERE (kart.acct     = ENTRY(1, cDR))
              AND (kart.currency = ENTRY(2, cDR))
            NO-LOCK:

            RUN acct-pos IN h_base(ENTRY(1, cDR), ENTRY(2, cDR), TODAY, TODAY, "�").
            s02 = sh-bal.
            IF (s02 <> 0)
            THEN DO:
                FOR EACH kau
                    WHERE (kau.acct     = ENTRY(1, cDR))
                      AND (kau.currency = ENTRY(2, cDR))
                      AND (kau.zero-bal = no)
                    NO-LOCK:

                    n02 = n02 + 1.
                END.
            END.
        END.
    END.

    RUN acct-pos IN h_base(acct.acct, acct.currency, TODAY, TODAY, "�").
    cXL = XLNumCell(I)
        + XLCellWrap(ClientName(acct.cust-cat, acct.cust-id, acct.acct))
        + XLCell(acct.acct)
        + XLCell(acct.branch-id)
        + XLDateCell(acct.open-date)
        + XLNumCell(- sh-bal)
        + XLNumCell(s01)
        + XLNumCell(n01)
        + XLNumCell(s02)
        + XLNumCell(n02)
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
