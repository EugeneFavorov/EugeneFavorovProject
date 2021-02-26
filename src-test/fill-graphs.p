/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
���������:     fill-graphp.p
�᭮�����:      
�� ������:     ��⮢�� �� ����� ��䨪 ���⥦�� �� �।��� ���+ � ��⮬ ���
���� ����᪠:  print-graphs.p 
������:         28.11.2017 ���ᮢ �.�.
*/

{globals.i}
{svarloan.def new}

{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get instrum}  /* ������⥪� ��� ࠡ��� � 䨭. �����㬥�⠬�. */
{intrface.get loan}     /* �����㬥��� ��� ࠡ��� � loan. */
{intrface.get loanc}
{intrface.get corr}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get date}
{intrface.get pogcr}    /* ������⥪� �����㬥�⮢ ��� ࠡ��� � ��䨪��� ����襭�� � ���. */

{fill-graphs.def}       /* ������� ��.⠡���� ttReport */
{pp-corr.p}

DEF INPUT PARAM iContract   AS CHAR     NO-UNDO.    /* �����祭�� ������� */
DEF INPUT PARAM iContCode   AS CHAR     NO-UNDO.    /* ����� ������� */
DEF INPUT PARAM iStart      AS DATE     NO-UNDO.    /* ��� ��砫� ��䨪� */
DEF INPUT PARAM iDate       AS DATE     NO-UNDO.    /* ��� ���� ������� */
DEF OUTPUT PARAM TABLE FOR ttReport.           /* �६����� ⠡��� */

/* ������� ��६����� */
DEFINE VARIABLE cLoanSurr   AS CHAR     NO-UNDO.            /* ���ண�� ������� */
DEFINE VARIABLE cPayType    AS CHAR     NO-UNDO.            /* ��� �᫮��� */
DEF VAR mID                 AS INT64    NO-UNDO.            /* ���浪��� ����� ��ப� (��� ���� ���) */
DEFINE VARIABLE iKod        AS INT64    NO-UNDO.            /* ��� (�����) ����樨 */
DEFINE VARIABLE dDateVyd    AS DATE     NO-UNDO.            /* ��� �뤠� �।�� */
DEFINE VARIABLE dFirstPlan  AS DATE     NO-UNDO.            /* ��� ��ࢮ� ��ப� �� ��������� ��䨪� */
DEFINE VARIABLE nLoanSumm   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE nLoanRest   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE cOpLNPr     AS CHAR     NO-UNDO INIT "10,46,362,378".           /* ���᮪ ����権 ������ ������祭��� ��業⮢, ��� ������ ���४�஢�� */

DEFINE VARIABLE myLastDate  AS DATE     NO-UNDO.            /* ��᫥���� ��� ��䨪� */
DEFINE VARIABLE nProc       AS DECIMAL  NO-UNDO INIT 0.     /* �㬬� 䠪��᪨ 㯫�祭��� %% */
DEFINE VARIABLE nFirstProc  AS DECIMAL  NO-UNDO INIT 0.     /* ���⥦ %% �� ���� ��業�� ��ਮ� */
DEFINE VARIABLE nRKOcomm    AS DECIMAL  NO-UNDO INIT 0.     /* ���⥦ �� ��� */
DEFINE VARIABLE dFirstPay   AS DATE     NO-UNDO.            /* ��� ��ࢮ�� ��������� ���⥦� (��ࢠ� �믫�� ��業⮢) */
DEFINE VARIABLE cTmp        AS CHAR     NO-UNDO.
DEFINE VARIABLE dT1         AS DECIMAL      NO-UNDO.
DEFINE VARIABLE dT2         AS DECIMAL      NO-UNDO.

DEFINE VARIABLE I           AS INTEGER      NO-UNDO.
DEFINE VARIABLE cSumOPR     AS CHARACTER    NO-UNDO EXTENT 8    /* ���⥦� �� �������� - �㬬� ����権 */
    INIT [ "5", "50,578", "46,100,720,409", "362,580,600,582", "478,361,378", "57,516", "526", "974,976,978"].
DEFINE VARIABLE dSumPlt     AS DECIMAL      NO-UNDO EXTENT 9.   /* ���⥦� �� �������� */
DEFINE VARIABLE cOpLAll     AS CHARACTER    NO-UNDO.            /* �� ����樨 */
cOpLAll = cSumOPR[1] + "," + cSumOPR[2] + "," + cSumOPR[3] + "," + cSumOPR[4] + ","
        + cSumOPR[5] + "," + cSumOPR[6] + "," + cSumOPR[7] + "," + cSumOPR[8].

   /* ���������� ���஢ */
DEFINE BUFFER b-loan        FOR loan.
DEFINE BUFFER ins-loan      FOR loan.        /* ������� ���客���� */
DEFINE BUFFER b-loan-cond   FOR loan-cond.
DEFINE BUFFER b-term-obl    FOR term-obl.
DEFINE BUFFER x-term-obl    FOR term-obl.
DEFINE BUFFER b-loan-acct   FOR loan-acct.
DEFINE BUFFER b-RepTable    FOR ttReport.
DEFINE BUFFER li100         FOR loan-int.
DEFINE BUFFER ch100         FOR chowhe.

/* ���塞 �������⥫�� �६���� ⠡���� */
DEFINE TEMP-TABLE tt-term-obl  LIKE term-obl.  /* ��� idnt = 3 �᭮���� ���� � idnt = 1 %% */
{fill-graphs.i}         /* �����㬥��� ��� ����� ��� */

mFormGrKom = FGetSetting("��䊮��","��ଃ����",?).

/* ��頥� ��.⠡���� */
{empty tt-term-obl}
{empty ttReport}
mID = 1.

/* ****************************************************************************************************************************** */
FOR loan 
    WHERE (loan.contract    EQ iContract)
      AND (loan.cont-code   EQ iContCode)
    NO-LOCK:

    cLoanSurr   = iContract + "," + iContCode.
    myLastDate  = loan.open-date.

    /* ��� �뤠� � �㬬� �।�� ********************************************************************************************** */
    IF (loan.open-date = iStart)
    THEN DO:
        FOR EACH loan-int OF loan           /* �� 䠪��᪨� ���⥦�� */
            NO-LOCK,
        FIRST chowhe
            WHERE (chowhe.id-d  EQ loan-int.id-d)
              AND (chowhe.id-k  EQ loan-int.id-k)
              AND (chowhe.id-op EQ 4)
            NO-LOCK
            BY loan-int.mdate:

            nLoanRest   = loan-int.amt-rub.
            nLoanSumm   = nLoanRest.
            dDateVyd    = loan-int.mdate.
            LEAVE.
        END.
    END.
    ELSE DO:
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 0,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanRest.
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 2,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanSumm + nLoanRest.
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 13,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanSumm + nLoanRest.
        RUN ALL_PARAM IN h_loan(loan.contract, loan.cont-code, 7,
                                iStart, OUTPUT nLoanRest, OUTPUT dT1, OUTPUT dT2).
        nLoanSumm   = nLoanSumm + nLoanRest.
        nLoanRest   = nLoanSumm.
        dDateVyd    = iStart.
    END.

    RUN SetSysConf IN h_base ("�����_��������", STRING(nLoanSumm)).
    RUN SetSysConf IN h_base ("����_������",    STRING(dDateVyd, "99.99.9999")).

    /* ����ᨬ � ⠡���� �� ॠ��� ���⥦� ******************************************************************************* */
    FOR EACH loan-int OF loan
        WHERE (loan-int.mdate   LT iDate) /* �� ���� ���� */
          AND (loan-int.mdate   GE iStart)
        NO-LOCK,
    FIRST chowhe
        WHERE (chowhe.id-d      EQ loan-int.id-d)
          AND (chowhe.id-k      EQ loan-int.id-k)
          AND CAN-DO(cOpLAll, STRING(chowhe.id-op))
        NO-LOCK
        BREAK BY loan-int.mdate:

        IF FIRST-OF(loan-int.mdate) THEN dSumPlt = 0.0.

        DO I = 1 TO 8:
            IF CAN-DO(cSumOPR[I], STRING(chowhe.id-op))
            THEN DO:
                IF (chowhe.id-op = 100)
                THEN DO:    /* ���� ������ 10 �� �� �� �㬬� */
                    FOR EACH li100 OF loan
                        WHERE (li100.mdate      EQ loan-int.mdate)
                          AND (li100.amt-rub    EQ loan-int.amt-rub)
                        NO-LOCK,
                    FIRST ch100
                        WHERE (ch100.id-d       EQ li100.id-d)
                          AND (ch100.id-k       EQ li100.id-k)
                          AND (ch100.id-op      EQ 10)
                        NO-LOCK:
                    
                        dSumPlt[I] = dSumPlt[I] + loan-int.amt-rub.
                        LEAVE.
                    END.
                END.
                ELSE dSumPlt[I] = dSumPlt[I] + loan-int.amt-rub.
            END.
        END.

        IF CAN-DO(cOpLNPr, STRING(chowhe.id-op))
        THEN nProc = nProc + loan-int.amt-rub.

        /* ������塞 ��䨪 */
        IF LAST-OF(loan-int.mdate)
        THEN DO:
            CREATE ttReport.
            ASSIGN
                ttReport.tf_id             = mID
                mID                        = mID + 1
                ttReport.tf_payment-date   = loan-int.mdate
                ttReport.tf_basic-sum-loan = dSumPlt[1]
                ttReport.tf_prosr-osn      = dSumPlt[2]
                ttReport.tf_sum-percent    = dSumPlt[3]
                ttReport.tf_prosr-proc     = dSumPlt[4]
                ttReport.tf_proc-pr-osn    = dSumPlt[5]
                ttReport.tf_peni           = dSumPlt[6]
                ttReport.tf_peni-pts       = dSumPlt[7]
                ttReport.tf_comm           = dSumPlt[8]
                ttReport.tf_sum-payment    = dSumPlt[1] + dSumPlt[2] + dSumPlt[3] + dSumPlt[4] + dSumPlt[5] + dSumPlt[6] + dSumPlt[7] + dSumPlt[8]
                nLoanRest                  = nLoanRest  - dSumPlt[1] - dSumPlt[2].
                ttReport.tf_rest-debts     = nLoanRest
                .
        END.
    END.

    /* ����ᨬ � ⠡���� ��䨪 �������� ���⥦�� ��稭�� � ᥣ����譥� ���� ************************************************ */
    /* �����㥬 ��易⥫��⢠ "��� ����" */
    DO I = 1 TO 3:
        /* ������� ��䨪 ��稭��� � ���� �ࠢ�� */
        RUN CopyTTData(loan.contract, loan.cont-code, I, iDate, loan.end-date).
    END.

    /* ������塞 ����� � ⠡���� ���� ttReport �� tt-term-obl  */
    dFirstPlan = ?.
    FOR EACH tt-term-obl
        WHERE tt-term-obl.idnt GE 1
          AND tt-term-obl.idnt LE 3
        BY tt-term-obl.end-date DESC:

        RUN CrtRepTbl(tt-term-obl.idnt,
                      tt-term-obl.end-date,
                      tt-term-obl.amt-rub).
        IF (tt-term-obl.idnt NE 2)      /* ���⮪ �.�.�� ���� �᫮��� */
        THEN dFirstPlan = tt-term-obl.end-date.
    END.

    /* �㬥�㥬 ��ப� � ��⠥� �㬬� ���������� ���⥦�� ****************************************************************** */
    FOR EACH ttReport
        WHERE (ttReport.tf_id = 0)
        NO-LOCK
        BY ttReport.tf_payment-date:

        ttReport.tf_sum-payment = ttReport.tf_sum-percent + ttReport.tf_basic-sum-loan.

        IF (ttReport.tf_sum-payment EQ 0)
        THEN DELETE ttReport.                  /* ���ࠥ� �� ��䨪� ����� ��ப� */
        ELSE ASSIGN
                ttReport.tf_id = mID
                mID                 = mID + 1.
    END.

    /* ���४�஢�� %% - ��९�᪠ � ���⠥��� **************************************************************************** */
    IF (dFirstPlan NE ?)    /* �᫨ ���� ��-� �� ��������� ��䨪� */
    THEN DO:
        FOR EACH term-obl OF loan
            WHERE CAN-DO("1,3", STRING(term-obl.idnt))
              AND (term-obl.end-date    LT dFirstPlan)
            NO-LOCK
            BY term-obl.end-date DESC:  /* �।���� ��� ��������� ��䨪� */

            FIND FIRST b-term-obl OF loan
                WHERE (b-term-obl.idnt      EQ 1)
                  AND (b-term-obl.end-date  EQ term-obl.end-date)
                NO-LOCK NO-ERROR.

            /* �᫨ �� �।����� ���� �� �����஢����� ��業��, � �� �� �����猪 ���᫨�� �� ���� �ப� */
            IF (NOT AVAIL b-term-obl) OR (term-obl.amt-rub EQ 0)
            THEN DO:
                /* ��⠥� �� ��業�� �� ��������� ��䨪� �� �।��饩 ���� */
                
                FOR EACH x-term-obl OF loan
                    WHERE (x-term-obl.idnt      EQ 1)
                      AND (x-term-obl.end-date  LT dFirstPlan)
                    NO-LOCK:

                    nProc = nProc - x-term-obl.amt-rub. /* �㬬� ���४�஢�� */
                END.

                FOR FIRST ttReport
                    WHERE (ttReport.tf_payment-date    EQ dFirstPlan)
                    NO-LOCK:

                    /* ���४�஢�� */
                    ttReport.tf_sum-percent = ttReport.tf_sum-percent - nProc.
                    ttReport.tf_sum-payment = ttReport.tf_sum-payment - nProc.
                END.
            END.

            LEAVE.
        END.
    END.
END. /* FOR loan */

{intrface.del}
