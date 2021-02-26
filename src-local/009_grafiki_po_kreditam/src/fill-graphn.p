/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
���������:     fill-graphp.p
�᭮�����:      ����.1211
�� ������:     ��⮢�� �� ����� ��䨪 ���⥦�� �� �।��� ���+ � ��⮬ ���
���� ����᪠:  print-graphn.p 
������:         06.07.2016 ���ᮢ �.�.
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

{fill-graphn.def}       /* ������� ��.⠡���� ttReportTable */
{pp-corr.p}

DEF INPUT PARAM iContract   AS CHAR     NO-UNDO.    /* �����祭�� ������� */
DEF INPUT PARAM iContCode   AS CHAR     NO-UNDO.    /* ����� ������� */
DEF INPUT PARAM iDate       AS DATE     NO-UNDO.    /* ��� ���� ������� */
/*
DEF INPUT PARAM fullKasko   AS DECIMAL  NO-UNDO.
*/
DEF INPUT PARAM typeTable   AS CHAR     NO-UNDO.    /* 1- ���, 2- ���+��᪮, 3 - ��+, 4 - ���+��᪮(09.14) */
DEF INPUT PARAM ipayments   AS CHAR     NO-UNDO.    /* ALL - ��, � �� � ���騥,  ����� - �� ���� ��� ��� ᥣ����譥� */
DEF OUTPUT PARAM TABLE FOR ttReportTable.           /* �६����� ⠡��� */

/* ������� ��६����� */
DEFINE VARIABLE cLoanSurr   AS CHAR     NO-UNDO.            /* ���ண�� ������� */
DEFINE VARIABLE cPayType    AS CHAR     NO-UNDO.            /* ��� �᫮��� */
DEF VAR mID                 AS INT64    NO-UNDO INIT 1.     /* ���浪��� ����� ��ப� (��� ���� ���) */
DEF VAR mI                  AS INT64    NO-UNDO.
/*
DEF VAR rko11_price         AS INT64    NO-UNDO.
*/
DEFINE VARIABLE cOp         AS CHAR     NO-UNDO.            /* ��� ����樨 �� �������� �� chowhe */
DEFINE VARIABLE iKod        AS INT64    NO-UNDO.            /* ��� (�����) ����樨 */
DEFINE VARIABLE dDateVyd    AS DATE     NO-UNDO.            /* ��� �뤠� �।�� */
DEFINE VARIABLE dReal2Plan  AS DATE     NO-UNDO.            /* ��� ���室� ��䨪� � ॠ���� ���⥦�� �� ������� */
DEFINE VARIABLE dFirstPlan  AS DATE     NO-UNDO.            /* ��� ��ࢮ� ��ப� �� ��������� ��䨪� */
DEFINE VARIABLE nLoanSumm   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE nLoanRest   AS DECIMAL  NO-UNDO.
DEFINE VARIABLE cOpLOsn     AS CHAR     NO-UNDO INIT "5,50".                    /* ���᮪ ����権 ������ ��.����� */
DEFINE VARIABLE cOpLPrc     AS CHAR     NO-UNDO INIT "10,46,361,362,378,478".   /* ���᮪ ����権 ������ ��業⮢ */
DEFINE VARIABLE cOpLNPr     AS CHAR     NO-UNDO INIT "10,46,362,378".           /* ���᮪ ����権 ������ ������祭��� ��業⮢, ��� ������ ���४�஢�� */
DEFINE VARIABLE cOpLPr1     AS CHAR     NO-UNDO INIT "721". /* ���᮪ ����権 ������ ��業⮢ �� ���� ��業�� ��ਮ� */
DEFINE VARIABLE cOpLDop     AS CHAR     NO-UNDO INIT "381,409,713,976,978".     /* �������⥫�� ���⥦� */
DEFINE VARIABLE cOpLAll     AS CHAR     NO-UNDO.            /* �� ����樨 */
DEFINE VARIABLE cAllPrc     AS CHAR     NO-UNDO.            /* �� ��業�� */
cOpLAll = cOpLOsn + "," + cOpLPrc + "," + cOpLPr1 + "," + cOpLDop.
cAllPrc = cOpLPrc + "," + cOpLPr1.

DEFINE VARIABLE myLastDate  AS DATE     NO-UNDO.            /* ��᫥���� ��� ��䨪� */
DEFINE VARIABLE nProc       AS DECIMAL  NO-UNDO INIT 0.     /* �㬬� 䠪��᪨ 㯫�祭��� %% */
DEFINE VARIABLE nFirstProc  AS DECIMAL  NO-UNDO INIT 0.     /* ���⥦ %% �� ���� ��業�� ��ਮ� */
DEFINE VARIABLE nRKOcomm    AS DECIMAL  NO-UNDO INIT 0.     /* ���⥦ �� ��� */
DEFINE VARIABLE dFirstPay   AS DATE     NO-UNDO.            /* ��� ��ࢮ�� ��������� ���⥦� (��ࢠ� �믫�� ��業⮢) */
DEFINE VARIABLE cTmp        AS CHAR     NO-UNDO.
DEFINE VARIABLE nTmp        AS DECIMAL  NO-UNDO.

   /* ���������� ���஢ */
DEF BUFFER b-loan       FOR loan.
DEF BUFFER ins-loan     FOR loan.        /* ������� ���客���� */
DEF BUFFER b-loan-cond  FOR loan-cond.
DEF BUFFER b-term-obl   FOR term-obl.
DEF BUFFER x-term-obl   FOR term-obl.
DEF BUFFER b-loan-acct  FOR loan-acct.
DEF BUFFER b-RepTable   FOR ttReportTable.

/* ���塞 �������⥫�� �६���� ⠡���� */
DEF TEMP-TABLE tt-term-obl  LIKE term-obl.  /* ��� idnt = 3 �᭮���� ���� � idnt = 1 %% */

{fill-graphn.i}             /* �����㬥��� ��� ����� ��� */

mFormGrKom = FGetSetting("��䊮��","��ଃ����",?).

/* ��頥� ��.⠡���� */
{empty tt-term-obl}
{empty ttReportTable}

/* ****************************************************************************************************************************** */
FOR loan 
    WHERE (loan.contract    EQ iContract)
      AND (loan.cont-code   EQ iContCode)
    NO-LOCK:

    cLoanSurr   = iContract + "," + iContCode.
    myLastDate  = loan.open-date.

    /* ��� �뤠� � �㬬� �।�� ********************************************************************************************** */
    dDateVyd    = loan.open-date.
    FOR EACH term-obl OF loan           /* �� ��������� ��䨪� */
        WHERE (term-obl.idnt  EQ 2)
        NO-LOCK
        BY term-obl.end-date:

        nLoanRest   = term-obl.amt-rub.
        nLoanSumm   = nLoanRest.
        dDateVyd    = term-obl.end-date.
        LEAVE.
    END.

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

    RUN SetSysConf IN h_base ("�����_��������", STRING(nLoanSumm)).
    RUN SetSysConf IN h_base ("����_������",    STRING(dDateVyd, "99.99.9999")).

    /* ��ࢠ� �������� ��� - ��ࢠ� 㯫�� ��業⮢ *************************************************************************** */
    FOR EACH term-obl OF loan
        WHERE (term-obl.idnt    EQ 1)
          AND (term-obl.amt-rub NE 0)
        NO-LOCK
        BY term-obl.end-date:

        dFirstPay = term-obl.end-date.
        LEAVE.
    END.

    /* ���� �������饣� �᫮��� (��᫥�����, �.�.� ���饬) ******************************************************************* */
    FOR EACH loan-cond
        WHERE loan-cond.contract    EQ loan.contract
          AND loan-cond.cont-code   EQ loan.cont-code
        NO-LOCK
        BY loan-cond.since DESCENDING:

        cPayType = GetXAttrValue("loan-cond", cLoanSurr + "," + STRING(loan-cond.since), "PayType").

        /* ����ᨬ � ⠡���� �� ॠ��� ���⥦� ******************************************************************************* */
        dReal2Plan = loan.open-date.    /* ��� ���室� �� ������� ���⥦� - ��᫥���� 䠪��᪨� ���⥦ */
        FOR EACH loan-int OF loan
            WHERE IF (ipayments EQ "ALL") THEN TRUE ELSE
                  (loan-int.mdate   LE MINIMUM(loan-cond.since, TODAY)) /* �� ��᫥����� �᫮��� ��� ⥪�饩 ���� */
            NO-LOCK,
        FIRST chowhe
            WHERE (chowhe.id-d      EQ loan-int.id-d)
              AND (chowhe.id-k      EQ loan-int.id-k)
              AND CAN-DO(cOpLAll, STRING(chowhe.id-op))
            NO-LOCK
            BREAK BY loan-int.mdate:

            cOp  = STRING(chowhe.id-op).
            iKod = IF CAN-DO(cOpLOsn, cOp) THEN   3 ELSE   (    /* �᭮���� ���� */
                   IF CAN-DO(cOpLPrc, cOp) THEN   1 ELSE  (     /* ��業�� */
                   IF CAN-DO(cOpLPr1, cOp) THEN   4 ELSE (      /* ��業�� �� ���� ��業�� ��ਮ� */
                   IF CAN-DO(cOpLDop, cOp) THEN 400 ELSE 0))).  /* �������⥫�� ���⥦� */
            IF (iKod NE 0)
            THEN DO:
                RUN CrtRepTbl(iKod,
                              loan-int.mdate,
                              loan-int.amt-rub).
                CASE iKod:
                    WHEN 4   THEN nFirstProc  = nFirstProc + loan-int.amt-rub.
                    WHEN 400 THEN nRKOcomm    = nRKOcomm   + loan-int.amt-rub.
                    WHEN 3   THEN nLoanRest   = nLoanRest  - loan-int.amt-rub.
                END CASE.
                IF CAN-DO(cOpLNPr, cOp)
                THEN nProc = nProc      + loan-int.amt-rub.
            END.

            /* ������뢠�� ���⮪ �� */
            IF LAST-OF(loan-int.mdate)
            THEN DO:
                RUN CrtRepTbl(2,
                              loan-int.mdate,
                              nLoanRest).
                dReal2Plan   = loan-int.mdate.
            END.
        END.

        /* ����ᨬ � ⠡���� ��䨪 �������� ���⥦�� ��稭�� � ᥣ����譥� ���� ************************************************ */
        /* �����㥬 ��易⥫��⢠ "��� ����" */
        DO mI = 1 TO 3:
            /* �᫨ ᥣ���� �뫨 ���⥦�, � ������� ��䨪 ��稭��� � �����譥�� ��� */
            RUN CopyTTData(loan.contract, loan.cont-code, mI, dReal2Plan + 1, loan.end-date).
        END.

        /* ������塞 ����� � ⠡���� ���� ttReportTable �� tt-term-obl  */
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

        /* ����ᨬ � ⠡���� �������祭�� �����ᨨ ***************************************************************************** */
        IF mFormGrKom EQ "��"       /* �᫨ ��䨪 �����ᨩ ������� �� ������� */
        THEN DO:
            nTmp = 0.
            FOR EACH x-term-obl
                WHERE (x-term-obl.contract  EQ loan.contract)
                  AND (x-term-obl.cont-code EQ loan.cont-code)
                  AND (x-term-obl.idnt      EQ 10)
                  AND (x-term-obl.end-date  GE loan-cond.since)
                NO-LOCK:

                nTmp = nTmp + x-term-obl.amt-rub.
            END.

            /* �᫨ �������祭�, ������塞 � ⠡���� ���� */
            IF (nFirstProc LT nTmp)
            THEN DO:
                RUN CrtRepTbl (4,
                               dFirstPay,
                               nTmp - nFirstProc).
                nFirstProc = nTmp.
            END.
        END.

        /* �㬬� �����ᨨ �� ��� ********************************************** */
        cTmp = GetXAttrValue("loan", cLoanSurr, "rko_comiss").

        IF (cTmp NE "")
        THEN nTmp = DEC(cTmp).
        ELSE DO:
            FOR EACH comm-rate
                WHERE comm-rate.commission EQ "%���"
                  AND CAN-DO(comm-rate.kau, loan.cont-code)
                  AND CAN-DO(comm-rate.kau, loan.contract)
                NO-LOCK
                BY comm-rate.since DESC:

                nTmp = comm-rate.rate-comm.
                LEAVE.  /* ������� �� ��� ���� � ������ �᫮���. ��६ ��᫥���� */
            END.
        END.

        /* �᫨ �������祭�, ������塞 � ⠡���� ���� */
        IF (nRKOcomm LT nTmp)
        THEN DO:
            RUN CrtRepTbl (400,
                           dFirstPay,
                           nTmp - nRKOcomm).
            nRKOcomm = nTmp.
        END.

        /* �᫨ ���� dFirstPay �� �뫮 � ��䨪�, �㦭� ᪮�४�஢��� ���⮪ ����� */
        FIND FIRST ttReportTable
            WHERE (ttReportTable.tf_payment-date    EQ dFirstPay)
            NO-LOCK NO-ERROR.
        IF (NOT AVAIL ttReportTable)
        THEN DO:
            /* ��६ ᫥������ ���� ��䨪� - ��� ���客�� */
            FOR EACH b-RepTable
                WHERE (b-RepTable.tf_payment-date   GE dFirstPay)
                NO-LOCK
                BY b-RepTable.tf_payment-date:

                dFirstPay = b-RepTable.tf_payment-date.
                LEAVE.
            END.
        END.
        ELSE IF (ttReportTable.tf_rest-debts EQ 0)
        THEN DO:
            nTmp = nLoanSumm.
            FOR EACH b-RepTable
                WHERE (b-RepTable.tf_payment-date   LT dFirstPay)
                NO-LOCK
                BY b-RepTable.tf_payment-date DESC:

                nTmp = b-RepTable.tf_rest-debts.
                LEAVE.
            END.

            /* ��६ �।����� �㬬� �� ��䨪� */
            RUN CrtRepTbl(2,
                          dFirstPay,
                          nTmp).
        END.

        /* ��� ���客�� ���⥦�� (��易⥫���) *********************************************************************************** */
        IF CAN-DO("1,2", typeTable)
        THEN DO:
            /* �� �ᥬ ������ࠬ ���客���� */
            FOR EACH ins-loan
                WHERE  ins-loan.contract         EQ "����"
                  AND  ins-loan.parent-cont-code EQ iContCode
                  AND  ins-loan.parent-contract  EQ iContract
                  AND  ins-loan.open-date        LE iDate
                  AND (ins-loan.close-date       GE iDate
                    OR ins-loan.close-date       EQ ?)
                NO-LOCK,
            EACH term-obl OF ins-loan
                WHERE term-obl.idnt EQ 1
                NO-LOCK
                BREAK BY term-obl.end-date:

                /* ���࠭塞 �㬬� �� �६����� ⠡��� */
                RUN CrtRepTbl(5,
                              MAXIMUM(term-obl.end-date, dFirstPay),
                              term-obl.amt-rub).
            END. /* ��� ���客�� ���⥦�� */
        END.

        /* �㬥�㥬 ��ப� � ��⠥� �㬬� ���������� ���⥦�� ****************************************************************** */
        mID = 1.
        FOR EACH ttReportTable
            NO-LOCK
            BY ttReportTable.tf_payment-date:

            ttReportTable.tf_sum-payment =
                ttReportTable.tf_sum-percent
              + ttReportTable.tf_basic-sum-loan
              + (IF (typeTable EQ "3") THEN 0 ELSE (    /* � 3-��� ��� ���.���⥦�� */
                ttReportTable.tf_additional-charge1
              + ttReportTable.tf_additional-charge2)).

            IF (ttReportTable.tf_sum-payment EQ 0)
            THEN DELETE ttReportTable.                  /* ���ࠥ� �� ��䨪� ����� ��ப� */
            ELSE ASSIGN
                    ttReportTable.tf_id = mID
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

                    FOR FIRST ttReportTable
                        WHERE (ttReportTable.tf_payment-date    EQ dFirstPlan)
                        NO-LOCK:

                        /* ���४�஢�� */
                        ttReportTable.tf_sum-percent = ttReportTable.tf_sum-percent - nProc.
                        ttReportTable.tf_sum-payment = ttReportTable.tf_sum-payment - nProc.
                    END.
                END.

                LEAVE.
            END.

            IF (dFirstPlan GE TODAY)
            THEN dReal2Plan = TODAY.    /* �᫨ ���� ������� ��䨪 � ���饬, � ��� ᮧ����� ��䨪� - ᥣ���� */
        END.
        ELSE dReal2Plan = TODAY.        /* �᫨ ���� ��䨪 䠪��᪨�, � �� �����祭, � ��� ᮧ����� ��䨪� - ᥣ���� */

        /* �����襭�� *********************************************************************************************************** */
        CASE typeTable:
            WHEN "1" THEN DO:
                FOR FIRST ttReportTable
                    WHERE (ttReportTable.tf_id EQ 1)
                    NO-LOCK:

                    ASSIGN
                        ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1  + ttReportTable.tf_sum-percent
                        ttReportTable.tf_sum-percent        = 0.
                        .
                END.
            END.
            WHEN "2" OR WHEN "4" THEN DO:
                /* ���४��㥬 ��ப� ��䨪� */
                FOR EACH ttReportTable
                    BY ttReportTable.tf_id:

                    /* "���� %-�� ��ਮ�" ��७�ᨬ � ������� "��業��" */
                    IF      (typeTable EQ "4")
                        AND (ttReportTable.tf_additional-charge1 NE 0)
                    THEN DO:
                        ASSIGN
                            ttReportTable.tf_sum-percent        = ttReportTable.tf_sum-percent + ttReportTable.tf_additional-charge1
                            ttReportTable.tf_additional-charge1 = 0
                            .
                    END.
                END.

                /* ��ନ�㥬 ��䨪 ��業⮢ */
                cTmp  = "".
                nTmp = 0.
                FOR EACH comm-rate
                    WHERE (comm-rate.kau        EQ cLoanSurr)
                      AND (comm-rate.commission EQ "%�।")
                    NO-LOCK
                    BY comm-rate.since:

                    IF (comm-rate.rate-comm NE nTmp)
                    THEN DO:
                        cTmp = cTmp + (IF (cTmp EQ "") THEN "� " ELSE "|� ")
                                    + STRING(comm-rate.since, "99.99.9999") + "�. - "
                                    + STRING(comm-rate.rate-comm, ">>9.99") + "%".
                        nTmp = comm-rate.rate-comm.
                    END.
                END.

                RUN SetSysConf IN h_base ("������_���������", cTmp).
                RUN SetSysConf IN h_base ("���������_����",   STRING(myLastDate, "99.99.9999")).
                RUN SetSysConf IN h_base ("����_�������",     STRING(dReal2Plan, "99.99.9999")).
            END.
        END CASE. /* CASE typeTable: */

        LEAVE.  /* �ᯮ��㥬 ⮫쪮 ��᫥���� �᫮��� */
    END.  /* FOR EACH loan-cond */
END. /* FOR loan */

RUN DeleteOldDataProtocol IN h_base ("������� �� ��������� �����").
RUN DeleteOldDataProtocol IN h_base ("������������� �� �������� �����").

{intrface.del}
