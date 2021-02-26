/**
����᪨� �ࠢ� �ਭ�������: ��吝�� ���� (���)
�᭮�����:      �� �����.389
�� ������:     ���⨥ ��� ���㬥�⮢ � �1 �� ��稭� ����砭�� �ப� �������� ��楯�
��� ࠡ�⠥�:
��ࠬ����:      mode=hand/auto,mail=<ᯨ᮪ �१ '|'>
���� ����᪠:  �����஢騪 (auto) ��� �࠭����� (hand)
������:         07.11.2016 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */
/* {intrface.get xclass} */
{intrface.get refer}
{intrface.get op}
{sh-defs.i}
{parsin.def}
{pb_logit.i}

/******************************************* ��।������ ��६����� � ��. */
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO.   /* mode=hand/auto,mail=<ᯨ᮪ �१ '|'> */

DEFINE VARIABLE iMail AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMode AS LOGICAL   NO-UNDO.     /* YES - �����஢騪, NO - �࠭����� */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE clog  AS CHARACTER NO-UNDO.
DEFINE VARIABLE I     AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE dK1   AS DATE      NO-UNDO.
DEFINE BUFFER   opo   FOR op.
DEFINE BUFFER   opb   FOR op.

iParam = REPLACE(REPLACE(iParam, ",",";"), "|",",").
iMail  = GetParamByNameAsChar(iParam, "mail", "").          /* ���᮪ �/� ��� ���뫪� ��⮪��� */
iMode  = GetParamByNameAsChar(iParam, "mode", "") EQ "auto".
IF iMode
THEN DO:
    end-date = TODAY.
    RUN pb_tstwork.p.
    IF NOT (RETURN-VALUE BEGINS "bis-work") THEN RETURN.
END.
ELSE end-date = gend-date.

/* ���⮢� ��������� ********************************************************* */
cfile = STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99") + "-" + shFilial.
cfile = "/tmp/K1end-" + cfile + ".xml".
/* cfile = "/home/aborisov/K1end-" + cfile + ".xml". */
OUTPUT TO VALUE(cfile).

PUT UNFORMATTED XLHead(STRING(end-date, "99.99.9999"), "IDCCCCN", "36,79,150,83,83,83,92").
PUT UNFORMATTED XLRowH(0, 34) XLCellHat("��������� �����饭��� �ᯮ�殮��� �� ���祭�� �ப� ��楯�", 6) XLRowEnd().
cXL = XLCellHead("� ��",0,0,0)
    + XLCellHead("�ப ���⥦�",0,0,0)
    + XLCellHead("��� ����⥪�",0,0,0)
    + XLCellHead("N ���. ���⠭����",0,0,0)
    + XLCellHead("N ���. ᯨᠭ��",0,0,0)
    + XLCellHead("N ���. ���.",0,0,0)
    + XLCellHead("�㬬�",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
/* **************************************************************************** */

FOR EACH acct
    WHERE (acct.bal-acct        EQ 90901)
      AND (acct.contract        EQ "����1")
      AND (acct.close-date      EQ ?)
      AND (acct.filial-id       EQ shFilial)
    EXCLUSIVE-LOCK,
FIRST signs
    WHERE (signs.file-name      EQ "acct")
      AND (signs.surrogate      EQ acct.acct + "," + acct.currency)
      AND (signs.code           EQ "�ப���⥦�")
      AND (IF iMode THEN
          (DATE(signs.code-val) LE end-date) ELSE
          (DATE(signs.code-val) EQ end-date))
    NO-LOCK,
EACH kau
    WHERE (kau.acct             EQ acct.acct)
      AND (kau.currency         EQ acct.currency)
      AND NOT kau.zero-bal
    EXCLUSIVE-LOCK,
FIRST opo
    WHERE (opo.op               EQ INT64(ENTRY(1,kau.kau)))
    NO-LOCK
    BREAK BY DATE(signs.code-val)
          BY acct.acct
          BY kau.kau:

    dK1 = DATE(signs.code-val).

    CREATE op.
    {auto-num.i &DefCount  = "op.op"
                &DefNum    = "''"
                &DateCount = "dK1"
                &AssignVar = "op.doc-num"
    }
    ASSIGN
        op.class-code       = "opo"
        op.doc-type         = "���"
        op.order-pay        = "5"
        op.acct-cat         = "o"
        op.op-date          = dK1
        op.doc-date         = dK1
        op.ins-date         = dK1
        op.due-date         = dK1
        op.contract-date    = dK1
        op.op-value-date    = dK1
        op.op-status        = "�"
        op.details          = "���ᠭ�� � ����⥪� 1. ������: �� ����祭� ᮣ��ᨥ �� ��楯�"
        op.filial-id        = acct.filial-id
        op.op-transaction   = NEXT-VALUE(op-transaction-id)
        op.op-kind          = "070105m"
        op.user-id          = USERID('bisquit')
        op.user-inspector   = USERID('bisquit')
    NO-ERROR.

    CREATE op-entry.
    ASSIGN
        op-entry.op         = op.op
        op-entry.amt-rub    = kau.balance
        op-entry.amt-cur    = 0.0
        op-entry.acct-cat   = "o"
        op-entry.op-cod     = "000000"
        op-entry.currency   = ""
        op-entry.acct-db    = GetRefVal("99999", dK1, "99999" + "," + acct.filial-id)
        op-entry.acct-cr    = acct.acct
        op-entry.kau-cr     = kau.kau
        op-entry.op-status  = op.op-status
        op-entry.op-date    = op.op-date
        op-entry.filial-id  = op.filial-id
        NO-ERROR.
    I   = I + 1.
/*  IF (I EQ 1)
    THEN DO:
        cfile = STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99") + "-" + shFilial.
        cfile = "/tmp/K1end-" + cfile + ".xml".
        OUTPUT TO VALUE(cfile).

        PUT UNFORMATTED XLHead(STRING(end-date, "99.99.9999"), "IDCCCCN", "36,100,150,125,125,125,125").

        cXL = XLCellHead("� ��",0,0,0)
            + XLCellHead("�ப ���⥦�",0,0,0)
            + XLCellHead("��� ����⥪�",0,0,0)
            + XLCellHead("N ���.���⠭����",0,0,0)
            + XLCellHead("N ���.ᯨᠭ��",0,0,0)
            + XLCellHead("N ���.���.",0,0,0)
            + XLCellHead("�㬬�",0,0,0)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
*/
    cXL = GetXAttrValue("op", ENTRY(1,kau.kau), "op-bal").
    FIND FIRST opb
        WHERE (opb.op   EQ INT64(cXL))
        NO-LOCK NO-ERROR.

    cXL = XLNumCell(I)
        + XLDateCell(DATE(signs.code-val))
        + XLCell(acct.number)
        + XLCellWrapR(opo.doc-num)
        + XLCellWrapR(op.doc-num)
        + XLCellWrapR(IF (AVAIL opb) THEN opb.doc-num ELSE "")
        + XLNumCell(kau.balance)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    ASSIGN
        kau.balance         = 0
        kau.zero-bal        = YES
        kau.numop           = kau.numop + 1
        .

    IF LAST-OF(acct.acct)
    THEN DO:
        RELEASE op.
        RELEASE op-entry.
        RUN acct-pos IN h_base(acct.acct, acct.currency, dK1, dK1, "�").

        IF (sh-bal EQ 0)
        THEN DO:
            acct.close-date = dK1.
            cXL = XLEmptyCell()
                + XLCell("������ ���")
                + XLCell(acct.number)
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        END.
        ELSE DO:
            cXL = XLEmptyCell()
                + XLCell("��� �� ������")
                + XLCell(acct.number)
                + XLEmptyCells(2)
                + XLCell("���⮪ :")
                + XLNumCell(sh-bal)
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        END.
    END.
END.

/* ���⮢�� ����砭�� ********************************************************* */
PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.
/* **************************************************************************** */

IF (I EQ 0)
THEN DO:
    IF iMode
    THEN RUN pb_mail.p (iMail, "Segodnja net spisanij s K1", "", "").
    ELSE MESSAGE "������� ��� ᯨᠭ�� � �1"
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.
ELSE DO:
/*  PUT UNFORMATTED XLEnd().
    OUTPUT CLOSE.
*/  RUN pb_mail.p (iMail, "Spisanie s K1 po okoncanii sroka", "", cfile).
/*  OS-DELETE VALUE(cfile). */
    IF (NOT iMode)
    THEN MESSAGE "�ந������� " I " ᯨᠭ�� � �1"
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.

OS-DELETE VALUE(cfile).
{intrface.del}
