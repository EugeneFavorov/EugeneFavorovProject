/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
�� ������:     ��楤��� ��� �ॣ㫨஢���� �2 - ���
������:         30.03.2016 ���ᮢ �.�.
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get xclass}
{intrface.get strng}    /** �㭪樨 ��� ࠡ��� � ��ப��� */
{intrface.get op}       /* ���. IsBudgetPayment */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

/* **************************************************************************** */
/* ������ ���ᨢ �����஢�� �� �������� ��� ��� ������ ��।���� (0 - 5) */
/* �������:   <��-�� ��� ���-� 0>,...,<��-�� ��� ���-� 5>;<�� ��-��>     */
/*              ��� <�����஢�� ��� ��।���� n> :                            */
/*            - <����> - �����஢�� ��� ��।���� ���,                       */
/*            - ������  - ������᭮� �ந�����⢮,                              */
/*            - ��      - ��।����� �����஢��� (���� ��� ������),            */
/*            - <�㬬�> - ��।����� �����஢��� �� 㪠������ �㬬�            */
/*                        (�����㬬 ��� ���������)                              */
/* **************************************************************************** */
FUNCTION BlockArr   RETURNS CHARACTER
   (INPUT  iSurr    AS CHARACTER ).    /* ���ண�� ��� <acct,curr> */

    DEFINE VARIABLE cBlOrd      AS CHARACTER NO-UNDO EXTENT 6 INIT "". /* ������� */
    DEFINE VARIABLE I           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBlTyp      AS CHARACTER NO-UNDO.           /* ⨯ �����஢�� �� �����䨪��� acct-status, misc[1] */
    DEFINE VARIABLE nBlSum      AS DECIMAL   NO-UNDO.           /* �㬬�, �����஢����� blockobject */
    DEFINE VARIABLE dNow        AS DATETIME  NO-UNDO.           /* TODAY */
    DEFINE VARIABLE cBlockType  AS CHARACTER NO-UNDO.           /* ���� ����஢�� ��� ��७�� �� ��� */
    DEFINE VARIABLE cBlOrdL     AS CHARACTER NO-UNDO.           /* ���᮪ ��।���⥩ �����஢�� */
    DEFINE VARIABLE lKonkPr     AS LOGICAL   NO-UNDO INIT NO.   /* �ਧ��� ������᭮�� �ந�����⢠ */
    DEFINE VARIABLE lBankrot    AS LOGICAL   NO-UNDO INIT NO.   /* �ਧ��� �������⢠ */

    DEFINE VARIABLE cBlockPrt   AS CHARACTER NO-UNDO EXTENT 6 INIT "". /* �����஢�� ��� ��⮪���: */
                                                                /* ����; ������; �����㬬 ����; �����㬬 1,2; �����㬬 1,2,3; ��⠫�� */
    DEFINE VARIABLE nSumA       AS DECIMAL   NO-UNDO EXTENT 3 INIT 0.    /* �㬬� �� �ᥬ �����㬬 ����/ 1,2/ 1,2,3 */
    DEFINE VARIABLE cAllBl      AS CHARACTER NO-UNDO INIT "".   /* ���᮪ ��� �����஢�� ��� ��⮪��� */

    cBlockType = FGetSetting("���⁫��", "���끫���", "") + ",������,������".
    dNow = DATETIME(TODAY, MTIME).

    /* ��࣠�� ORACLE �⮡� �������� "Memory violation" */
    FIND FIRST blockobject
        NO-LOCK NO-ERROR.
    
    FOR EACH blockobject
        WHERE (blockobject.file-name     EQ 'acct')
          AND (blockobject.class-code    EQ 'BlockAcct')
          AND (blockobject.beg-datetime  LE dNow)
          AND ((blockobject.end-datetime EQ ?)
            OR (blockobject.end-datetime GE dNow))
          AND (blockobject.surrogate     EQ iSurr)
/*        AND (blockobject.txt[1]        NE "*") */
        NO-LOCK
        BY blockobject.beg-datetime:

        /* ⨯ �����஢�� � �����஢����� �㬬� */
        cBlTyp  = GetCodeMisc("acct-status", blockobject.block-type, 1).
        nBlSum  = IF ((ENTRY(2, iSurr) EQ "") AND (blockobject.val[3] EQ 0)) THEN blockobject.val[4] ELSE blockobject.val[3].

        /* ������塞 �����஢�� � ᯨ᮪ ��� ��⮪��� */
        IF      (blockobject.txt[1] EQ "")              /* ����� ��।����� � "����" � ���� ���⠭������� */
            AND (INDEX(blockobject.txt[3], "����") NE 0)
        THEN cBlOrdL = "����".
        ELSE cBlOrdL = blockobject.txt[1].

        CASE blockobject.block-type:
            WHEN "������"   THEN .
            WHEN "������"  THEN .
            WHEN "����"     THEN cBlockPrt[1] = "����".
            WHEN "������"   THEN cBlockPrt[2] = "������".
            OTHERWISE DO:
                IF (cBlTyp EQ "�����㬬")
                THEN DO:
                    IF (cBlOrdL EQ "����")
                    THEN ASSIGN
                            cBlockPrt[3] = "�����㬬 ����"
                            nSumA[1]     = nSumA[1] + nBlSum.
                    IF (cBlOrdL EQ "1,2")
                    THEN ASSIGN
                            cBlockPrt[4] = "�����㬬 1,2"
                            nSumA[2]     = nSumA[2] + nBlSum.
                    IF (cBlOrdL EQ "1,2,3")
                    THEN ASSIGN
                            cBlockPrt[5] = "�����㬬 1,2,3"
                            nSumA[3]     = nSumA[3] + nBlSum.
                END.
                ELSE DO:
                    IF   NOT CAN-DO(cBlockPrt[6], blockobject.block-type)
                    THEN {additem.i cBlockPrt[6]  blockobject.block-type}
                END.
            END.
        END CASE.

        nBlSum  = ABSOLUTE(nBlSum).

        IF NOT CAN-DO(cBlockType, blockobject.block-type)   /* �� �����஢�� �� ��� �� �⠢�� */
        THEN NEXT.

        /* ������᭮� �ந�����⢮ � �������⢮ ��४�뢠�� �� */
        IF (blockobject.block-type EQ "������")
        THEN lKonkPr = YES.
        IF (blockobject.block-type EQ "������")
        THEN lBankrot = YES.
        IF (lKonkPr OR lBankrot) THEN NEXT.

        IF (cBlTyp NE ?)   /* ��������� �����஢�� �ய�᪠��, ��� ��� �� ��� ������� � �� ���⁫��/���끫��� */
        THEN DO:
            DO I = 1 TO 5:
                IF (cBlTyp EQ "�����㬬")
                THEN DO:
                    IF NOT CAN-DO(blockobject.txt[1], STRING(I))    /* ��।����� ��������� ��� ��� �����஢�� */
                    THEN
                        CASE cBlOrd [I + 1]:
                            WHEN ""    THEN cBlOrd[I + 1] = STRING(nBlSum).                             /* �� �뫮 �����஢�� => �㬬�        */
                            WHEN "��"  THEN .                                                           /* 㦥 �⮨� ����� ᨫ쭠� �����஢�� */
                            OTHERWISE       cBlOrd[I + 1] = STRING(DECIMAL(cBlOrd[I + 1]) + nBlSum).    /* �� ���� �㬬� => �㬬��㥬        */
                        END CASE.
                END.
                ELSE DO:
                    IF NOT CAN-DO(blockobject.txt[1], STRING(I))    /* ��।����� ��������� ��� ��� �����஢�� */
                    THEN cBlOrd [I + 1] = "��".                     /* ��४�뢠�� ��, �� �뫮                */
                END.
            END.
        END.
        cBlOrd[1] = cBlOrd[2]. /* �����஢�� ��� ��।���� 0 ⠪�� ��, ��� � ��� ��।���� 1 */
    END.

    /* ��ꥤ��塞 �����஢�� ��� ��⮪��� */
    DO I = 1 TO 6:
        IF (cBlockPrt[I] NE "")
        THEN DO:
            CASE I:
                WHEN 1 OR WHEN 2 OR WHEN 6 THEN
                    {additem2.i cAllBl cBlockPrt[I] "|"}
                WHEN 3 OR WHEN 4 OR WHEN 5 THEN DO:
                    cBlockPrt[I] = cBlockPrt[I] + " (" + STRING(nSumA[I - 2]) + ")".
                    {additem2.i cAllBl cBlockPrt[I] "|"}
                END.
            END CASE.
        END.
    END.

    IF lKonkPr
    THEN RETURN "������,������,������,������,������,������;������|" + cAllBl.
    ELSE IF lBankrot
    THEN RETURN "������,������,������,������,������,������;������|" + cAllBl.
    ELSE RETURN cBlOrd[1] + "," + cBlOrd[2] + "," + cBlOrd[3] + "," + cBlOrd[4] + "," + cBlOrd[5] + "," + cBlOrd[6] + "," + cBlOrd[6] + ";" + cAllBl.
        /* �� ���� cBlOrd[6] � ���� - �� ��直� ��砩, �᫨ ���-� �����﫨�� ���㬥��� 6-� ��।���� */
END FUNCTION.


/* **************************************************************************** */
/* ��ନ�㥬 ����� ������ � ⠡��� ttKauCrt                                    */
/* **************************************************************************** */
PROCEDURE New_ttKauCrt:
    DEFINE INPUT  PARAMETER iCrt    AS CHARACTER    NO-UNDO.    /* ����⥪�: K2 ��� KBS */

    DEFINE VARIABLE cOrd        AS CHARACTER    NO-UNDO.    /* ��।����� ��室���     */
    DEFINE VARIABLE iOrdP       AS INTEGER      NO-UNDO.    /* ��।����� ��� ��७�� */
    DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.    /* �����祭�� ���⥦�  */
    DEFINE VARIABLE cAcc        AS CHARACTER    NO-UNDO.    /* �/� ���.���㬥��   */
    DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cCrtCur     AS CHARACTER    NO-UNDO.    /* ����� ����⥪�    */
    DEFINE VARIABLE lNal        AS LOGICAL      NO-UNDO.    /* ���� ���⥦    */
    DEFINE BUFFER op-bal FOR op.
    DEFINE BUFFER oe-bal FOR op-entry.

    /* �� ��ࢨ筮�� ���㬥��� ����塞 : ��।����� (cOrd), �����祭�� ���⥦� (cDet) � �/� �����⨪� (cAcc) */
    cCrtCur = IF (iCrt EQ "K2") THEN cCurrK2 ELSE cCurrKBS.
    cDet = "".
    cAcc = "".

    /* �᫨ �� ������ ���.���㬥��, � ��।����� - �� ���㬥��� ���⠭���� �� �2 */
    FOR FIRST op-bal
        WHERE (op-bal.op EQ INT(ENTRY(1, kau.kau)))
        NO-LOCK:

        cOrd  = op-bal.order-pay.
        iOrdP = INT(cOrd).
    END.

    cDR  = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").    /* ��室�� ���㬥��, ���⠢����� �� ����⥪� */
    IF (cDR NE "")
    THEN DO:
        FIND FIRST op-bal
            WHERE (op-bal.op    EQ INT64(cDR))
            NO-LOCK NO-ERROR.

        IF (AVAIL op-bal)
        THEN DO:
            cOrd  = op-bal.order-pay.
            iOrdP = INT(cOrd).

            RUN IsBudgetPayment IN h_op(op-bal.op, OUTPUT lNal).   /* ���� ���⥦� �� 3 ��।���� */    
            IF lNal AND (iOrdP GE 4)
            THEN iOrdP = 3.

            cDet = op-bal.details.
            cAcc = GetXAttrValue("op", cDR, "acctbal").

            /* �᫨ �� acctbal ���⮩, � ��६ ��� � acct-db �஢���� */
            FIND FIRST oe-bal OF op-bal
                NO-LOCK NO-ERROR.
            IF (cAcc EQ "") AND (AVAIL oe-bal)
            THEN cAcc = oe-bal.acct-db.
        END.
    END.

    /* �᫨ ��� �� ���.���-� �� ᮢ������ � �/�, � �����⨪� �ய�᪠�� */
    IF    ((cAcc NE "") AND (cAcc EQ iAcct))
        OR (cAcc EQ "")
    THEN DO:
        CREATE ttKauCrt.
        ASSIGN
            ttKauCrt.crt        = iCrt
            ttKauCrt.crtn       = iCrt
            ttKauCrt.kau        = kau.kau
            ttKauCrt.sort       = ENTRY(1, kau.sort) + "," + ENTRY(2, kau.sort)
            ttKauCrt.opdate     = DATE(INT64(SUBSTRING(ENTRY(2,kau.sort),6,2)),
                                       INT64(SUBSTRING(ENTRY(2,kau.sort),9,2)),
                                       INT64(SUBSTRING(ENTRY(2,kau.sort),1,4)))
            ttKauCrt.numop      = kau.numop
            ttKauCrt.balance    = (IF (cCrtCur EQ "") THEN kau.balance      ELSE kau.curr-bal)
            ttKauCrt.firstsum   = DEC(ENTRY(3,kau.sort))
            ttKauCrt.summa      = 0
            ttKauCrt.order-pay  = iOrdP
            ttKauCrt.order-sym  = IF (CAN-DO("4,5", cOrd) AND lNal) THEN (cOrd + "(�����)") ELSE cOrd
            ttKauCrt.order-new  = IF  CAN-DO("4,5", cOrd)           THEN (iOrdP + 1)        ELSE iOrdP
            ttKauCrt.details    = cDet
            ttKauCrt.action     = ""
            ttKauCrt.eerror     = ""
            ttKauCrt.block      = ENTRY(iOrdP + 1, cBlOrdL)
            ttKauCrt.blocksum   = (IF CAN-DO("������,������,��,", ttKauCrt.block) THEN 0 ELSE DEC(ttKauCrt.block))
            ttKauCrt.priost     = (GetXAttrValue('op', ENTRY(1, kau.kau), "�ਮ�⑯��") EQ "��")
            .
    END.
END PROCEDURE.

/* **************************************************************************** */
/* �����뢠�� ���ଠ�� �� ���� � ��⮪�� XL                                 */
/* **************************************************************************** */
PROCEDURE XLProtokol:
    DEFINE INPUT  PARAMETER cPrt    AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER lFirst  AS LOGICAL      NO-UNDO.

    OUTPUT TO VALUE(cPrt) APPEND.

    DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.

    IF lFirst
    THEN DO:
        IF (shFilial EQ "0500")
        THEN PUT UNFORMATTED XLHead("ul", "CCNCCDNCCC", "213,53,103,200,96,71,103,89,122,171").
        ELSE PUT UNFORMATTED XLHead("ul", "CNCCDNCCC",  "213,103,200,96,71,103,89,122,171").

        cXL = XLCellHead("���",0,0,0)
            + (IF (shFilial EQ "0500") THEN XLCellHead("��㯯� ���",0,0,0) ELSE "")
            + XLCellHead("���⮪ �� �/�",0,0,0)
            + XLCellHead("�����஢��",0,0,0)
            + XLCellHead("��।����� �� ���㬥���",0,0,0)
            + XLCellHead("��� ���",0,0,0)
            + XLCellHead("���⮪",0,0,0)
            + XLCellHead("��室���� � ����⥪�",0,0,0)
            + XLCellHead("�믮����� ������",0,0,0)
            + XLCellHead("�������ਨ (�訡��)",0,0,0)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
    ELSE DO:
        PUT UNFORMATTED XLRow(0) XLEmptyCell() XLRowEnd().  /* �/� ࠧ���塞 � ��⮪��� ���⮩ ��ப�� */
    END.

    FOR EACH ttKauCrt
            WHERE (IF (iPrtAll EQ "��") THEN YES ELSE (ttKauCrt.action NE ""))
        NO-LOCK
        BY ttKauCrt.order-new
        BY ttKauCrt.sort:

        cXL = XLCell(iAcct)
            + (IF (shFilial EQ "0500") THEN XLCell(GetLinks("acct", iAcct + "," + iCurr, "s", "acct-group", ",", TODAY)) ELSE "")
            + XLNumCell(nAcPos)
            + XLCellWrap(REPLACE(cAllOrd, "|", "&#10;"))
            + XLCell(ttKauCrt.order-sym)
            + XLDateCell(ttKauCrt.opdate)
            + XLNumCell(ttKauCrt.balance)
            + XLCell(ttKauCrt.crt)
            + XLCellWrap(REPLACE(ttKauCrt.action, "|", "&#10;"))
            + XLCellWrap(ttKauCrt.eerror)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.

    OUTPUT CLOSE.
END PROCEDURE.
