/* =================================-==-===
** �����㬥��� ��� ����� ��䨪�� ��� */

/* =========================================-==-===
** �����頥� ४����� �� "᫮�����" ᯨ᪠
** �������1=��� ��.1,�������2=��� ��.2  � �.�. */
FUNCTION GetParsSett RETURNS CHAR
  (INPUT iPar AS INT64,         /* 1 �������, 2 ��� ����. */
   INPUT iQty  AS INT64,        /* ���浪��� ����� � ��ப� iStr */
   INPUT iStr AS CHAR).         /* ��ப� � ᯨ᪮� */
   RETURN ENTRY(iPar, ENTRY(iQty, iStr), "=").
END FUNCTION.

/* =============================-===
** True  - ���� ������ � Term-obl */
PROCEDURE IsOneRecOnTerm-Obl.
    DEF INPUT  PARAM iContract AS CHAR NO-UNDO.    /* �����祭�� ������� */
    DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.    /* ����� ������� */
    DEF INPUT  PARAM iDate     AS DATE NO-UNDO.    /* �� ���� */
    DEF OUTPUT PARAM oIsRealTr AS LOG  NO-UNDO.    /* �ਧ��� �����⢥����� ��易⥫��⢠ �� */
    DEF OUTPUT PARAM oAmntTr   AS DEC  NO-UNDO.    /* ���-�� �� ��  */

    DEF VAR vCount   AS INT64 NO-UNDO.     /* ���-�� ����ᥩ �᫮��� */

    /* ����塞 ������⢮ ����ᥩ � ��䨪� �� ࠢ�� 1 ��� ��� */
    FOR EACH b-term-obl
        WHERE b-term-obl.contract  EQ iContract
          AND b-term-obl.cont-code EQ iContCode
          AND b-term-obl.idnt      EQ 3
          AND b-term-obl.end-date  LE iDate
        NO-LOCK:

        ASSIGN
            vCount  = vCount + 1
            oAmntTr = oAmntTr + b-term-obl.amt-rub
            .
    END.

    /* �᫨ ������ ⮫쪮 ����, � ����室��� ��ந�� ����㠫�� ��䨪.
    ** � ��⨢��� ��砥 ����஢��� ����-�-����, �.�. ��� ���� */
    IF vCount EQ 1
    THEN oIsRealTr = TRUE.
END PROCEDURE.

/* ============================================================-==-===
** ����஢���� ��䨪� ���⥦�� �� �� �� �६����� ⠡���� tt-term-obl
** ��� ���४�஢�� */
PROCEDURE CopyTTData.
    DEF INPUT PARAM iContract AS CHAR   NO-UNDO. /* �����祭�� ������� */
    DEF INPUT PARAM iContCode AS CHAR   NO-UNDO. /* ����� ������� */
    DEF INPUT PARAM iIdnt     AS INT64  NO-UNDO. /* ��� ��易⥫��⢠ */
    DEF INPUT PARAM iDatR     AS DATE   NO-UNDO. /* ��� ��ॢ��� ��䨪� �� ���� */
    DEF INPUT PARAM iDate     AS DATE   NO-UNDO. /* ��� ����砭�� ������� */

    /* �஡���� �� ��易⥫��⢠� ��᫥ ���� ���� */
    FOR EACH b-term-obl
        WHERE b-term-obl.contract  EQ iContract
          AND b-term-obl.cont-code EQ iContCode
          AND b-term-obl.idnt      EQ iIdnt
          AND b-term-obl.end-date  GE iDatR
          AND b-term-obl.end-date  LE iDate
          AND b-term-obl.amt-rub   NE 0
        NO-LOCK:

        FIND FIRST tt-term-obl OF b-term-obl
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL tt-term-obl
        THEN tt-term-obl.amt-rub = tt-term-obl.amt-rub + b-term-obl.amt-rub.
        ELSE DO:
            CREATE tt-term-obl.
            BUFFER-COPY b-term-obl TO tt-term-obl.
        END.
    END.
END PROCEDURE.

/* ==================================-==-===
** ���࠭���� ������ �� �६����� ⠡��� */
PROCEDURE CrtRepTbl.
    DEF INPUT PARAM iNum   AS INT64 NO-UNDO.    /* ��� (�����) ����樨 */
    DEF INPUT PARAM iDate  AS DATE  NO-UNDO.    /* ��� ����樨 */
    DEF INPUT PARAM iSumma AS DEC   NO-UNDO.    /* �㬬� ����樨 */

    IF (iSumma EQ 0) THEN RETURN.
/*
IF (iDate EQ 07/14/2016)
THEN DO:
    MESSAGE "OP2 - " iNum iDate iSumma
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
    
END.
*/
    /* �饬 ������ �� �६����� ⠡��� ��䨪� �� ��� */
    FIND FIRST ttReport
        WHERE ttReport.tf_payment-date EQ iDate
        NO-LOCK NO-ERROR.
    /* �᫨ �� ��諨, ⮣�� ���� ᮧ���� ������ �� ���� */
    IF NOT AVAIL ttReport
    THEN DO:
        CREATE ttReport.
        ttReport.tf_id = 0.    /* ��⮬ ��७㬥�㥬 */
        tf_payment-date = iDate.
    END.

    /* ��業�� ���������� � ������ ��ப�, ��⠫�� ����� ������⢮���� * /
    IF iNum EQ 1
    THEN ttReport.tf_id = iID. */

    /* �����ࠥ��� � ����� ����樨 �� �� ���� � ����������� �㬬� */
    CASE iNum:
        WHEN 1 THEN     /* ��業�� */
            ttReport.tf_sum-percent        = ttReport.tf_sum-percent        + iSumma.
        WHEN 2 THEN     /* ���⪨ */
            ttReport.tf_rest-debts         = ttReport.tf_rest-debts         + iSumma.
        WHEN 3 THEN     /* ��易⥫��⢠ �� ����襭�� �᭮����� ����� */
            ttReport.tf_basic-sum-loan     = ttReport.tf_basic-sum-loan     + iSumma.
    END CASE.

    myLastDate  = MAXIMUM(myLastDate, iDate).
END PROCEDURE.