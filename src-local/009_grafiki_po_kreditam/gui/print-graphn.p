{globals.i}
{intrface.get tmess}

/* +++ print-graphn.p was humbly modified by (c)blodd converter v.1.11 on 3/23/2017 1:56pm +++ */

/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
���������:     print-graphp.p
�᭮�����:      ����.1211
�� ������:     ����� ��䨪� ���⥦�� �� �।��� ���+ � ��⮬ ���
��ࠬ����:      templ=<蠡��� XL>;dogtype=<⨯ �������>;payments=ALL/�����  (templ=graphm0914;dogtype=4;payments=ALL) 
���� ����᪠:  �।��� - Ctrl-G - ������ ��������� � ������� EXCEL
������:         06.07.2016 ���ᮢ �.�.
*/

&GLOB nodate YES

{globals.i}
{tmprecid.def}
{parsin.def}
{prn-doc.def &with_proc=YES}
{norm.i NEW}
{intrface.get pqres}

{fill-graphn.def}
{svarloan.def NEW}
{intrface.get xclass}

/* ��ப� ��ࠬ��஢ */
DEF INPUT PARAM     iStr    AS CHAR NO-UNDO.

DEF NEW SHARED VAR rid_loan AS RECID.

def var vEps as decimal no-undo.
def var tmpSign as char no-undo.

DEF VAR in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEF VAR in-dataClass-id LIKE DataClass.DataClass-id NO-UNDO.
def var template  as char no-undo.
def var typeTable as char no-undo. /* 1- ���, 2- ���+��᪮, 3 - ��+ */
def var payments  as char no-undo.

template  = GetParamByNameAsChar(iStr,"templ",   "graphm0914").
typeTable = GetParamByNameAsChar(iStr,"dogtype", "4").
payments  = GetParamByNameAsChar(iStr,"payments","ALL").

/* �� �⬥祭�� ������ࠬ */
FOR EACH tmprecid NO-LOCK,
EACH loan
    WHERE (RECID(loan)  EQ tmprecid.id)
    NO-LOCK:

    {empty ttnames}

/* ayv �࠭� ������ ��� � ���.४������*/
/*
    RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.since, OUTPUT vEps).

    tmpSign = GetXattrValueEx("loan",
                    loan.contract + "," + loan.cont-code,
                    "���",
                    "").

    IF   tmpSign = ""
    THEN DO:
        UpdateSigns('loan', loan.contract + "," + loan.cont-code, '���', string(vEps), no).
        UpdateSigns('loan', loan.contract + "," + loan.cont-code, '���', string(vEps), yes).

        RUN SetSysConf IN h_base ("����������","1").

        RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.since, OUTPUT vEps).
        UpdateSigns('loan', loan.contract + "," + loan.cont-code, '����������', string(vEps), yes).
        run deleteolddataprotocol in h_base("����������") .
    END.
*/

    ASSIGN
        rid-p    = RECID(loan)
        rid_loan = RECID(loan)
        .
    {norm-beg.i }

    FIND LAST loan-cond
        WHERE loan-cond.contract  EQ loan.contract
          AND loan-cond.cont-code EQ loan.cont-code
          AND loan-cond.since     LE gend-date
        NO-LOCK NO-ERROR.
    IF AVAIL loan-cond
    THEN rid-t = RECID(loan-cond).

    /* ��ࠡ�⪠ ��楤�ࠬ� bankinfo,userinfo,dog,lgarterm */
    RUN loanagval.p (template, INPUT-OUTPUT TABLE ttnames).

    /* ���������� ⠡���� ����묨 ��� */
    RUN FillTables (loan.contract, loan.cont-code, loan.cont-type).

    OUTPUT STREAM fil CLOSE.

    {norm-end.i &nofil=YES &nopreview=YES}

    /* �뢮� ������ �� 蠡���� template � 䠩� ���� */
    RUN printvd.p (template, INPUT TABLE ttnames).
END.

{intrface.del}

/* ���������� ⠡���� ����묨 ��� */
PROCEDURE FillTables:
    DEF INPUT PARAM iContract AS CHAR NO-UNDO.
    DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
    DEF INPUT PARAM iContType AS CHAR NO-UNDO.

    DEF BUFFER bloan    FOR loan.
    def var c1          as char no-undo.
    def var c2          as char no-undo.
    def var vTmpDec     as decimal no-undo.

    RUN fill-graphn.p(loan.contract, loan.cont-code, loan.since, typeTable, payments,
                      OUTPUT TABLE ttReportTable).

    /* �饬 ���� ���客� �६�� �� �뤠� �।�� */
    FOR EACH bloan
        WHERE bloan.PARENT-CONT-CODE EQ iContCode
          AND bloan.class-code       EQ "insurance"
          AND bloan.contract         EQ '�����'
        NO-LOCK
        BY bloan.open-date:

        IF (GetXAttrValue("loan", bloan.contract + "," + bloan.cont-code, "vidstr") NE "�����_�")
        THEN NEXT.

        FIND FIRST term-obl OF bloan
            WHERE term-obl.idnt     EQ 1
              AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE term-obl
        THEN vTmpDec = term-obl.amt-rub.
        ELSE vTmpDec = DECIMAL(GetXAttrValue("loan", iContract + "," + iContCode, "kasko3")).
        RUN Insert_TTName ("insurkaskkpremia", STRING(vTmpDec)).
        LEAVE.
    END.

    FOR EACH bloan
        WHERE bloan.PARENT-CONT-CODE EQ iContCode
          AND bloan.class-code EQ "insurance"
          AND bloan.contract   EQ '�����'
        NO-LOCK
        BY bloan.open-date:

        IF (GetXAttrValue("loan", bloan.contract + "," + bloan.cont-code, "vidstr") NE "�����_�")
        THEN NEXT.

        FIND FIRST term-obl OF bloan
            WHERE term-obl.idnt     EQ 1
              AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE term-obl
        THEN vTmpDec = term-obl.amt-rub.
        ELSE vTmpDec = DECIMAL(GetXAttrValue("loan", iContract + "," + iContCode, "kasko3")).
        RUN Insert_TTName ("insurkasknpremia", STRING(vTmpDec)).
        LEAVE.
    end.

    RUN Insert_TTName ("DateVyd",   GetSysConf("����_������")).
    RUN Insert_TTName ("lastDate",  GetSysConf("���������_����")).
    RUN Insert_TTName ("GrafProc",  GetSysConf("������_���������")).
    RUN Insert_TTName ("GraphDate", GetSysConf("����_�������")).
    vTmpDec = DEC(GetSysConf("�����_��������")).  /* ��砫쭮� ���祭�� ��� ����஫쭮�� �⮫�� */

    RUN Insert_TTName ("graph", "").

    FIND FIRST ttNames
        WHERE ttnames.tname EQ 'graph'
        NO-LOCK NO-ERROR.

    /* IF CAN-DO("Avto*,���*",iContType) and loan.cont-type <> '���+�' and loan.cont-type <> '���+�' THEN */
    if typeTable = '1'
    then DO:
        FOR EACH ttReportTable
            BREAK BY ttReportTable.tf_payment-date:

            ttnames.tvalue  = ttnames.tvalue
                            + STRING(ttReportTable.tf_id)                         + '\n'
                            + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                            + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                            + STRING(ttReportTable.tf_sum-percent)                + '\n'
                            + STRING(ttReportTable.tf_additional-charge1)         + '\n'
                            + STRING(ttReportTable.tf_additional-charge2)         + '\n'
                            + STRING(ttReportTable.tf_sum-payment)                + '\n'
                            + STRING(ttReportTable.tf_rest-debts)                 + '\n'
                            + STRING(ttReportTable.tf_actual-payment)             + '\n'
                            .
        END.
    END.

    /* if loan.cont-type = '��⮏���' then do:      */
    /* if can-do( '���+�*,���+�*', loan.cont-type) then */
    if typeTable = '2'
    then DO:
        FOR EACH ttReportTable
            BREAK BY ttReportTable.tf_payment-date:

            ttnames.tvalue  = ttnames.tvalue
                            + STRING(ttReportTable.tf_id)                         + '\n'
                            + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                            + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                            + STRING(ttReportTable.tf_sum-percent)                + '\n'
                            + STRING(ttReportTable.tf_additional-charge1)         + '\n'
                            + '\n'
                            + STRING(ttReportTable.tf_additional-charge2)         + '\n'
                            + STRING(ttReportTable.tf_sum-payment)                + '\n'
                            + STRING(ttReportTable.tf_rest-debts)                 + '\n'
                            + STRING(ttReportTable.tf_actual-payment)             + '\n'
                            .
        END.
    END.

    /*IF CAN-DO("M�+(���)",iContType) THEN  */
    if typeTable = '3'
    then DO:
        FOR EACH ttReportTable
            BREAK BY ttReportTable.tf_payment-date:

            ttnames.tvalue  = ttnames.tvalue
                            + STRING(ttReportTable.tf_id)                         + '\n'
                            + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                            + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                            + STRING(ttReportTable.tf_sum-percent)                + '\n'
                            + STRING(ttReportTable.tf_sum-payment)                + '\n'
                            + STRING(ttReportTable.tf_rest-debts)                 + '\n'
                            .
        END.
    END.

    /* ayv ��� ������ ��䨪� 15.09.14 */
    if typeTable = '4'
    then DO:
        FOR EACH ttReportTable
            BREAK BY ttReportTable.tf_payment-date:

            ttnames.tvalue  = ttnames.tvalue
                            + STRING(ttReportTable.tf_id)                         + '\n'
                            + STRING(ttReportTable.tf_payment-date, "99.99.9999") + '\n'
                            + STRING(ttReportTable.tf_basic-sum-loan)             + '\n'
                            + STRING(ttReportTable.tf_sum-percent)                + '\n'
                            + STRING(ttReportTable.tf_additional-charge2)         + '\n'
                            + STRING(ttReportTable.tf_sum-payment)                + '\n'
                            + STRING(ttReportTable.tf_rest-debts)                 + '\n'
/*                          + STRING(ttReportTable.tf_actual-payment)             + '\n'    */
                            /* ����஫�� �⮫���. �.�.= 0 */
/*                          + STRING(vTmpDec - ttReportTable.tf_basic-sum-loan - ttReportTable.tf_rest-debts) + '\n' */
                            .
            vTmpDec         = ttReportTable.tf_rest-debts.
        END.
    END.
END PROCEDURE.

/* --- print-graphn.p was humbly modified by (c)blodd converter v.1.11 on 3/23/2017 1:56pm --- */
