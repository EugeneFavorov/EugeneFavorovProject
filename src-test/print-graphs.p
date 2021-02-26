/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
���������:     print-graphp.p
�᭮�����:      ���쬮 �⥭���� �.�.
�� ������:     ����� �ࠢ�� � ������������
��ࠬ����:      templ=<蠡��� XL>;dogtype=<⨯ �������>;payments=ALL/�����  (templ=graphm0914;dogtype=4;payments=ALL) 
���� ����᪠:  �।��� - Ctrl-G - ������ ��������� � ������� EXCEL
������:         22.11.2017 ���ᮢ �.�.
*/

&GLOB nodate YES

{globals.i}
{tmprecid.def}
{parsin.def}            /* GetParamByNameAsChar */
{prn-doc.def &with_proc=YES}    /* ttnames */
{svarloan.def NEW}
{norm.i NEW}
{intrface.get loan}
{intrface.get tmess}
{param-dog.p}

/* ��ப� ��ࠬ��஢ */
DEFINE INPUT PARAM iStr     AS CHARACTER    NO-UNDO.

DEFINE NEW SHARED VARIABLE rid_loan     AS RECID.
DEFINE VARIABLE vEps        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE tmpSign     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEFINE VARIABLE in-dataClass-id LIKE DataClass.DataClass-id NO-UNDO.
DEFINE VARIABLE template    AS CHARACTER no-undo.

DEFINE VARIABLE cSumPRM     AS CHARACTER    NO-UNDO EXTENT 9    /* ���⪨ �� ������� �� �㬬� ��ࠬ��஢ */
    INIT [ "0+2+13", "4+32+33+35+29+16+34", "9+12+82+26+509+516+531", "7", "10+48+377", "8+210+233+248+229", "519+526", "209+301", "530"].
DEFINE VARIABLE cDelOPR     AS CHARACTER    NO-UNDO EXTENT 9    /* ������ ����樨 �� ���� �ࠢ�� */
    INIT [ "", "83", "", "", "", "283", "", "", ""].
DEFINE VARIABLE dSumOst     AS DECIMAL      NO-UNDO EXTENT 9.   /* ���⪨ �� ������� */
DEFINE VARIABLE iPar        AS INTEGER      NO-UNDO.
DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
DEFINE VARIABLE J           AS INTEGER      NO-UNDO.
DEFINE VARIABLE dPrm        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE dT1         AS DECIMAL      NO-UNDO.
DEFINE VARIABLE dT2         AS DECIMAL      NO-UNDO.

{fill-graphs.def}       /* ������� ��.⠡���� ttReport */
template = GetParamByNameAsChar(iStr, "templ", "graphzadol").

/* �� �⬥祭�� ������ࠬ */
FOR EACH tmprecid
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan)  EQ tmprecid.id)
    NO-LOCK:
/*
    IF (loan.close-date <> ?)
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "������� " + loan.doc-ref + " ������.~n��ନ஢���� ᢥ����� �������� ⮫쪮 �� ������ ������ࠬ").
        NEXT.
    END.
*/
    IF (loan.filial-id = "0400")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "��࠭ ������� " + loan.doc-ref + " � �����⮬ ��᪮�᪮� 䨫���� (0400).~n"
                                  + "������ ������� � ⥬ �� ����஬ � �������� ��� (0000).").
        NEXT.
    END.

    IF (loan.close-date <> ?)
    THEN end-date = loan.close-date + 1.
    ELSE DO:
        {getdate.i}     /* end-date - ��� �ࠢ�� � ������������ */
    END.

    /* ������ ������� �� ���� �ࠢ�� */
    IF (loan.close-date = ?)
    THEN RUN l-calc2.p (loan.contract, loan.cont-code, end-date, FALSE, TRUE).

    {empty ttnames}
    ASSIGN
        rid-p    = RECID(loan)
        rid_loan = RECID(loan)
        .
    {norm-beg.i }
    OUTPUT STREAM fil TO NUL.

    FIND LAST loan-cond
        WHERE loan-cond.contract  EQ loan.contract
          AND loan-cond.cont-code EQ loan.cont-code
          AND loan-cond.since     LE gend-date
        NO-LOCK NO-ERROR.
    IF AVAIL loan-cond
    THEN rid-t = RECID(loan-cond).

    /* ��ࠡ�⪠ ��楤�ࠬ� bankinfo,userinfo,dog,lgarterm */
    RUN loanagval.p (template, INPUT-OUTPUT TABLE ttnames).
    RUN Insert_TTName ("BankBic_", FGetSettingMF ("�������", "", "", loan.filial-id, ?)).

    tmpSign = FGetSettingMF ("�����",   "", "", loan.filial-id, ?).
    FOR EACH acct
        WHERE (acct.acct BEGINS tmpSign)
        NO-LOCK,
    FIRST c-nostro OF acct
        NO-LOCK:

        tmpSign = c-nostro.corr-acct.
    END.
    RUN Insert_TTName ("BankKrC_", tmpSign).
    RUN Insert_TTName ("BankName", FGetSettingMF ("����",    "", "", loan.filial-id, ?)).

    /* ���������� ⠡���� ����묨 ��� */
    IF (loan.filial-id EQ "0300") OR (loan.filial-id EQ "0500")
    THEN beg-date = MAXIMUM(loan.open-date, 01/01/2017).
    ELSE beg-date = loan.open-date.
    RUN fill-graphs.p(loan.contract, loan.cont-code, beg-date, end-date, OUTPUT TABLE ttReport).

    RUN Insert_TTName ("DateZadol",  STRING(IF (loan.close-date = ?) THEN end-date ELSE TODAY, "99.99.9999")).
    RUN Insert_TTName ("DateVyd",    GetSysConf("����_������")).
    RUN Insert_TTName ("SumLoanVyd", GetSysConf("�����_��������")).

    dSumOst = 0.0.
    IF (loan.close-date = ?) THEN
    DO I = 1 TO 9:
        DO J = 1 TO NUM-ENTRIES(cSumPRM[I], "+"):
            iPar = INT64(ENTRY(J, cSumPRM[I], "+")).

            IF (J = 3) OR (J = 2)  /* ⮫쪮 ��� ����� � %% */
            THEN DO:
                RUN PRM(loan.Contract,          /* �����祭�� ������� */
                        loan.Cont-Code,         /* ����� ������� */
                        iPar,                   /* ��� ��ࠬ���  */
                        end-date,               /* ���祭�� ��ࠬ��� �� ���� ������ ������� */
                        TRUE,                   /* ����� % */
                        OUTPUT dPrm).           /* ���祭�� ��ࠬ��� ��� loan.interest[i] */
            END.
            ELSE DO:
                RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, iPar,
                                        end-date, OUTPUT dPrm, OUTPUT dT1, OUTPUT dT2).
            END.

            /* ���४�஢�� �� ᥣ����譨� ������ */
            FOR EACH loan-int OF loan
               WHERE (loan-int.mdate   = end-date)
                 AND (loan-int.id-d    = iPar)
               NO-LOCK:

               dPrm = dPrm - loan-int.amt-rub.
            END.

            FOR EACH loan-int OF loan
               WHERE (loan-int.mdate   = end-date)
                 AND (loan-int.id-k    = iPar)
               NO-LOCK:

               dPrm = dPrm + loan-int.amt-rub.
            END.

            dSumOst[I] = dSumOst[I] + dPrm.
        END.
/*
        DO J = 1 TO NUM-ENTRIES(cDelOPR[I], "+"):
            RUN GetSumLoanInt IN h_loan(loan.contract, loan.cont-code, INT64(ENTRY(J, cDelOPR[I], "+")),
                                        end-date, end-date, OUTPUT dPrm).
            dSumOst[I] = dSumOst[I] - dPrm.
        END.
*/
    END.
    RUN Insert_TTName ("OstOsnDolg",      STRING(dSumOst[1])).
    RUN Insert_TTName ("SumProcCred",     STRING(dSumOst[2])).
    RUN Insert_TTName ("SumPeni",         STRING(dSumOst[3])).
    RUN Insert_TTName ("SumProsrOsn",     STRING(dSumOst[4])).
    RUN Insert_TTName ("SumProsrProcCred",STRING(dSumOst[5])).
    RUN Insert_TTName ("SumProcProsrOsn", STRING(dSumOst[6])).
    RUN Insert_TTName ("SumPeniPTS",      STRING(dSumOst[7])).
    RUN Insert_TTName ("SumComm",         STRING(dSumOst[8])).
    RUN Insert_TTName ("GosZadol",        STRING(dSumOst[9])).
    RUN Insert_TTName ("OstZadolCred", STRING(dSumOst[1] + dSumOst[2] + dSumOst[3] + dSumOst[4] + dSumOst[5] + dSumOst[6] + dSumOst[7] + dSumOst[8])).
    FOR EACH _user
        WHERE (_user._userid = USERID("bisquit"))
        NO-LOCK:

        RUN Insert_TTName ("Sotrudnik", _user._user-Name + ", " + GetXAttrValue("_user", _user._userid, "���������")).
    END.

    FIND FIRST ttReport
        WHERE (ttReport.tf_payment-date = end-date)
        NO-LOCK NO-ERROR.
    IF (AVAIL ttReport)
    THEN DO:
        RUN Insert_TTName ("PlanPlat", STRING(ttReport.tf_basic-sum-loan)).
        RUN Insert_TTName ("PlanProc", STRING(ttReport.tf_sum-percent)).
    END.
    ELSE DO:
        RUN Insert_TTName ("PlanPlat", "0").
        RUN Insert_TTName ("PlanProc", "0").
    END.


    IF CAN-FIND(FIRST ttReport WHERE (ttReport.tf_payment-date < end-date))
    THEN DO:
        RUN Insert_TTName ("graph1", "").
        FIND FIRST ttNames
            WHERE ttnames.tname EQ 'graph1'
            NO-LOCK NO-ERROR.
        FOR EACH ttReport
            WHERE (ttReport.tf_payment-date < end-date)
            BREAK BY ttReport.tf_payment-date:

            ttnames.tvalue  = ttnames.tvalue
                       + STRING(ttReport.tf_payment-date, "99.99.9999")
                + '\n' + STRING(ttReport.tf_basic-sum-loan)
                + '\n' + STRING(ttReport.tf_prosr-osn)
                + '\n' + STRING(ttReport.tf_sum-percent)
                + '\n' + STRING(ttReport.tf_prosr-proc)
                + '\n' + STRING(ttReport.tf_proc-pr-osn)
                + '\n' + STRING(ttReport.tf_peni)
                + '\n' + STRING(ttReport.tf_peni-pts)
                + '\n' + STRING(ttReport.tf_comm)
                + '\n' + STRING(ttReport.tf_sum-payment)
                + '\n' + STRING(ttReport.tf_rest-debts)
                + '\n\n'.
        END.
    END.

    IF (loan.close-date = ?)
    THEN DO:
        RUN Insert_TTName ("graph3", "").
        FIND FIRST ttNames
            WHERE ttnames.tname EQ 'graph3'
            NO-LOCK NO-ERROR.
        ttnames.tvalue  = '\n\n\n\n\n\n\n\n\n\n\n\n'.
    END.

    IF      CAN-FIND(FIRST ttReport WHERE (ttReport.tf_payment-date > end-date))
        AND (loan.close-date = ?)
    THEN DO:
        RUN Insert_TTName ("graph2", "").
        FIND FIRST ttNames
            WHERE ttnames.tname EQ 'graph2'
            NO-LOCK NO-ERROR.
        FOR EACH ttReport
            WHERE (ttReport.tf_payment-date > end-date)
            BREAK BY ttReport.tf_payment-date:

            ttnames.tvalue  = ttnames.tvalue
                       + STRING(ttReport.tf_payment-date, "99.99.9999")
                + '\n' + STRING(ttReport.tf_basic-sum-loan)
                + '\n' /* + STRING(ttReport.tf_prosr-osn) */
                + '\n' + STRING(ttReport.tf_sum-percent)
                + '\n' /* + STRING(ttReport.tf_prosr-proc) */
                + '\n' /* + STRING(ttReport.tf_proc-pr-osn) */
                + '\n' /* + STRING(ttReport.tf_peni) */
                + '\n' /* + STRING(ttReport.tf_peni-pts) */
                + '\n' /* + STRING(ttReport.tf_comm) */
                + '\n' + STRING(ttReport.tf_sum-payment)
                + '\n' + STRING(ttReport.tf_rest-debts)
                + '\n\n'.
        END.
    END.

    OUTPUT STREAM fil CLOSE.
    {norm-end.i &nofil=YES &nopreview=YES}

    /* �뢮� ������ �� 蠡���� template � 䠩� ���� */
    RUN printvd.p (template, INPUT TABLE ttnames).
END.

{intrface.del}
