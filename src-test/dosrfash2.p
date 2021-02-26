/**
�� ������:     �㬬� ������� ����筮�� ��襭��
��ࠬ����:      ������ ���� �������� = �㬬� ��ࠬ��஢:
                0+7+8+233+9+10+12+18+26+82+210+16+13+14+15+48+248+29+229+519+509+530+373+777+4
���� ����᪠:  ������ ����� - ������� - �।��� ������� ������ - Ctrl-G
                �।��� � �������� - ���᮪ ������஢ - Ctrl-G
*/

define input param ipCountTypeChar as char no-undo. /* ������ ���� �������� */

{client.i}
{tmprecid.def}  /* ��室�� �������� */
{param-dog.p}

DEFINE VARIABLE vCountInt   as INT64    INIT 0 no-undo. /* ���稪 */
DEFINE VARIABLE vCustName   AS CHAR     NO-UNDO.   /* ������������ ������ */
DEFINE VARIABLE out_Result  AS DECIMAL  NO-UNDO.
DEFINE VARIABLE vDbOpDec    AS DECIMAL  NO-UNDO.
DEFINE VARIABLE vCrOpDec    AS DECIMAL  NO-UNDO.
DEFINE VARIABLE mSum-prosr  AS DECIMAL  label "" init 0  NO-UNDO.
DEFINE VARIABLE mSum-all    AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mSum-annu   AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mdate       AS DATE     NO-UNDO.
DEFINE VARIABLE mRs-acct    AS CHAR     NO-UNDO.   /* ������ ��� */
DEFINE VARIABLE mVkl-acct   AS CHAR     NO-UNDO.   /* ������ ��� */
DEFINE VARIABLE e-date      AS DATE     LABEL "" NO-UNDO.
DEFINE VARIABLE mRs-ost     AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mVkl-ost    AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE i           AS INTEGER  NO-UNDO.
DEFINE VARIABLE iPar        AS CHAR     NO-UNDO.
DEFINE VARIABLE mSumFullComm AS DECIMAL INIT 0  NO-UNDO.
DEFINE VARIABLE mSumComm    AS DECIMAL  INIT 0  NO-UNDO.
DEFINE VARIABLE firstDate   AS DATE     NO-UNDO.
DEFINE VARIABLE not0        AS LOGICAL  NO-UNDO.
DEFINE VARIABLE pr_problem  AS CHAR.
DEFINE BUFFER   term-obl    FOR term-obl.
DEFINE BUFFER   bterm-obl   FOR term-obl.
DEFINE BUFFER   bLA         FOR loan-acct.

not0 = TRUE.

/* �६����� ⠡��� ��� ���� */
DEFINE TEMP-TABLE ttReport NO-UNDO
   FIELD since       AS DATE LABEL "�ப ������" /* ��� ������ ������� */
   FIELD param_id    AS CHAR /* �����䨪��� ��ࠬ��� */
   FIELD param_value AS DEC  FORMAT "->>>>>>>>9.99" LABEL "�㬬�" /* ���祭�� ��ࠬ��� */
   FIELD name-par    AS CHAR LABEL "������������ ��ࠬ���" /* ������������ ��ࠬ��� (��ࠬ��஢). �᫨ ��ࠬ��஢ �����, � �뢮��� ������������ ��ࠬ��஢ �१ "," */
.

FOR EACH tmprecid NO-LOCK:
    vCountInt = vCountInt + 1.
END.

IF vCountInt NE 1
THEN DO:
    MESSAGE "������ ���� ����祭 1 �������"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

FOR FIRST tmprecid,
FIRST loan WHERE
    RECID(loan) EQ tmprecid.id
    NO-LOCK:      /* 3 */

    /* ������ ��ப� - �������� ࠡ��� ����� */
/*      {move-bar.i vLnCountInt vLnTotalInt}
*/
pr_problem = GetXAttrValue("loan",
                           loan.contract + "," + loan.cont-code,
                           "pr_problem").

IF pr_problem EQ '��_�����' THEN MESSAGE '��������! �����᭮ �।�⠢������ � ���� ���㬥�⮢- ����㯨�� ᬥ��� ����騪�.' VIEW-AS ALERT-BOX.


    RUN RE_CLIENT (loan.cust-cat, loan.cust-id, INPUT-OUTPUT vCustName).
    mdate = loan.since.

    DO vCountInt = 1 TO NUM-ENTRIES (ipCountTypeChar,"+"):

        /* ����祭�� ���祭�� ��ࠬ��� */
        iPar = ENTRY(vCountInt, ipCountTypeChar, "+").
        RUN PRM(loan.Contract,          /* �����祭�� ������� */
                loan.Cont-Code,         /* ����� ������� */
                INTEGER(iPar),          /* ��� ��ࠬ���  */
                loan.since,             /* ���祭�� ��ࠬ��� �� ���� ������ ������� */
                TRUE,                   /* ����� % */
                OUTPUT out_result).     /* ���祭�� ��ࠬ��� ��� loan.interest[i] */

/*          MESSAGE iPar SKIP out_result
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
        CREATE ttReport.
        ASSIGN
            ttReport.since       = loan.since
            ttReport.param_id    = iPar
            ttReport.param_value = out_result
            .

        /* ����稬 ������������ ��ࠬ��� �� �ࠢ�筨�� */
        FIND FIRST loan-par
            WHERE loan-par.amt-id EQ INTEGER(iPar)
            NO-LOCK NO-ERROR.
        IF AVAIL loan-par
        THEN ttReport.name-par = ttReport.param_id + " - " + loan-par.NAME.

        /* ���४�஢�� 4 ��ࠬ��� */
        IF (iPar EQ "4")
        THEN DO:
            DO i = 32 TO 35:
                RUN PRM(loan.Contract, loan.Cont-Code, i, loan.since, TRUE, OUTPUT out_result).
                ttReport.param_value = ttReport.param_value + out_result.
            END.
        END.

        /* ���४�஢�� 29 ��ࠬ��� */
        IF (iPar EQ "29")
        THEN DO:
            FOR EACH loan-int OF loan
                WHERE (loan-int.mdate   EQ loan.since)
                NO-LOCK,
            FIRST chowhe
                WHERE (chowhe.id-d      EQ loan-int.id-d)
                  AND (chowhe.id-k      EQ loan-int.id-k)
                  AND (chowhe.id-op     EQ 83)
                NO-LOCK:

                ttReport.param_value = ttReport.param_value - loan-int.amt-rub.
            END.
        END.
        
        /* ���४�஢�� 229 ��ࠬ��� */
        IF (iPar EQ "229")
        THEN DO:
            FOR EACH loan-int OF loan
                WHERE (loan-int.mdate   EQ loan.since)
                NO-LOCK,
            FIRST chowhe
                WHERE (chowhe.id-d      EQ loan-int.id-d)
                  AND (chowhe.id-k      EQ loan-int.id-k)
                  AND (chowhe.id-op     EQ 283)
                NO-LOCK:

                ttReport.param_value = ttReport.param_value - loan-int.amt-rub.
            END.
        END.
    END. /* DO vCountInt = 1 TO ... */

/* 
    CREATE ttReport.
    ASSIGN
        ttReport.since      = loan.since
        ttReport.param_id   = "555"
        ttReport.param_value  = 0.00
        ttReport.name-par   = " �⮣� ����祭��� ������������� - "
        .

    / * ���� ��ࢮ� �������⮩ �������� ��魮�� �� �������� * /
    RUN RE_FIRST_TERM_OBL IN h_loan(loan.contract, loan.cont-code, 3, loan.since, BUFFER term-obl).
    IF AVAIL term-obl
    THEN e-date   = term-obl.end-date.
*/

/*
    FOR EACH acct NO-LOCK
        WHERE acct.cust-cat EQ "�"
          AND acct.cust-id EQ loan.cust-id
          AND acct.bal-acct EQ 47423:
       
        RUN acct-pos IN h_base (acct.acct, acct.currency, mdate, mdate, ?).
        CREATE ttReport.
        ASSIGN
            ttReport.since       = loan.since
            ttReport.param_id    = "777"
            ttReport.param_value = ABSOLUTE(sh-bal)
        / * ttReport.name-par  = " ������� � ��� " + ENTRY(1, acct.acct, "@") + " - " * /
            ttReport.name-par    = " ���⥦ ��業⮢ �� ���� ��業�� ��ਮ�"
            .
    / *     mSum-com2 = ABSOLUTE(sh-bal). * /
    END. / * 10 * /
*/

    mSumComm = 0.
    FIND FIRST term-obl
        WHERE term-obl.contract  EQ loan.contract
          AND term-obl.cont-code EQ loan.cont-code
          AND term-obl.idnt      EQ 10
          NO-LOCK NO-ERROR.
    IF AVAIL term-obl
    THEN DO:
        mSumFullComm = term-obl.amt-rub.

        RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract, loan.cont-code, 3, loan.open-date, BUFFER term-obl).
        RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract, loan.cont-code, 1, loan.open-date, BUFFER bterm-obl).

        IF AVAIL term-obl AND
            term-obl.dsc-beg-date <=
            (IF AVAIL bterm-obl THEN bterm-obl.dsc-beg-date ELSE term-obl.dsc-beg-date)
        THEN firstDate   = term-obl.dsc-beg-date.

        IF AVAIL bterm-obl AND
            bterm-obl.dsc-beg-date <=
                (IF AVAIL term-obl THEN term-obl.dsc-beg-date ELSE bterm-obl.dsc-beg-date)
        THEN firstDate   = bterm-obl.dsc-beg-date.

        IF firstDate > loan.since
        THEN DO:
            mSumComm = (mSumFullComm / (firstDate - loan.open-date)) * (loan.since - loan.open-date).
            FOR EACH loan-int OF loan
                WHERE loan-int.id-k = 377
                  AND loan-int.mdate <= loan.since
                  NO-LOCK:
                
                mSumComm = mSumComm - loan-int.amt-rub.
            END.
            not0 = FALSE.
        END.
    END.


    /*
    FOR EACH acct NO-LOCK
        WHERE acct.cust-cat EQ "�"
        AND acct.cust-id EQ loan.cust-id
        AND acct.bal-acct EQ 47423:
    */
    FIND FIRST loan-acct OF loan
        WHERE loan-acct.acct-type = '�।�㤊��'
        NO-LOCK NO-ERROR.
    IF AVAIL loan-acct
    THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, mdate, mdate, ?).
        IF not0 OR mSumComm > ABSOLUTE(sh-bal)
        THEN mSumComm = ABSOLUTE(sh-bal).

        CREATE ttReport.
        ASSIGN
            ttReport.since        = loan.since
            ttReport.param_id     = "777"
            ttReport.param_value  = mSumComm
          /*ttReport.name-par   = " ������� � ��� " + ENTRY(1, acct.acct, "@") + " - "*/
                                  /* 1234567890123456789012345678901234567890 */
            ttReport.name-par     = " ���⥦ ��業⮢ �� ���� ��業�� ��ਮ�"
            .
   END.

/*
   FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
                         AND loan-cond.cont-code EQ loan.cont-code
                         AND loan-cond.since     LE loan.Since NO-LOCK NO-ERROR.
   IF AVAIL loan-cond THEN
   DO:  / * 11 * /
   mSum-annu = DEC(GetXAttrValueEx("loan-cond",
                                    loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                    "����⏫��",
                                    "0")).
       CREATE ttReport.
       ASSIGN
          ttReport.since       = e-date
          ttReport.param_id    = "888"
          ttReport.param_value = mSum-annu
          ttReport.name-par    = " ��।��� ���⥦ �� �।��� (������)  "
          .

    END.  /* 11 */
*/
/*
    mSum-all = mSum-all + mSum-annu.
*/
    RUN RE_L_ACCT(loan.Contract,loan.Cont-Code,"�।����",loan.since,BUFFER bLA).
    IF AVAILABLE bLA
    THEN DO:
        mRs-acct = "   ����騩 ��� " + ENTRY(1, bLA.acct, "@").
        RUN acct-pos IN h_base (bLA.acct, loan.currency, mdate, mdate, ?).
        mRs-ost = ABSOLUTE(sh-bal).
    END.
    ELSE mRs-acct = "   ������ ��� �� �������� � ��������".

    {setdest.i}
    PUT UNFORMATTED
        "���⥦ �� �������� "
        ENTRY(1,loan.cont-code,"@") format "x(20)"
        " "
        vCustName
        SKIP
        mRs-acct
        "���⮪ " mRs-ost
        SKIP
        "                   �� "
        loan.since
        SKIP.

    RUN RE_L_ACCT(loan.Contract,loan.Cont-Code,"�।����1",loan.since,BUFFER bLA).
    IF AVAILABLE bLA
    THEN DO:
        mVkl-acct = "��易⥫��⢠ ����騪� ����� " + ENTRY(1, bLA.acct, "@").
        RUN acct-pos IN h_base (bLA.acct, loan.currency, mdate, mdate, ?).
        mVkl-ost = ABSOLUTE(sh-bal).
    END.
    ELSE mVkl-acct = "   ����� �� �������� � ��������".

    /* {setdest.i} */
    PUT UNFORMATTED
/*               "���⥦ �� �������� "
               ENTRY(1,loan.cont-code,"@") format "x(20)"
               " "
               vCustName
               SKIP */
        mVkl-acct
        "���⮪ " mVkl-ost
        SKIP
        "                   �� "
        loan.since
        SKIP(2).

    FOR EACH ttReport
        SHARE-LOCK
        BY INTEGER(ttReport.param_id): /* 5 */

        IF INTEGER(ttReport.param_id) LT 555
        THEN mSum-prosr = mSum-prosr + ttReport.param_value.

        IF INTEGER(ttReport.param_id) EQ 555
        THEN DO:
            ttReport.param_value = mSum-prosr.
            PUT UNFORMATTED FILL("-",71).
        END.
        ELSE mSum-all = mSum-all + ttReport.param_value.

        IF ABSOLUTE(ttReport.param_value) GT 0
        THEN DISPLAY
                ttReport.name-par format "x(45)"
                ttReport.param_value
                ttReport.since
/*              (IF ttReport.param_id EQ "888" THEN "�� ��䨪� " + STRING(e-date) ELSE "") FORMAT "x(20)" */
                .
        IF INTEGER(ttReport.param_id) EQ 555
        THEN PUT UNFORMATTED FILL("-",71) SKIP.
    END.  /* 5 */

    DISPLAY "�⮣� ���⥦ �� �।���  " format "x(40)" mSum-all FORMAT "->>>>>>>>9.99".
/*  DISPLAY "�ப ���⥦� �� �।���  " format "x(40)" e-date. */
    mRs-ost = mRs-ost + mVkl-ost.
    DISPLAY "������� �� ���  "        format "x(40)" (if mSum-all GT mRs-ost THEN mSum-all - mRs-ost ELSE 0 ) FORMAT "->>>>>>>>9.99".

    {preview.i}
END. /* 3 */
