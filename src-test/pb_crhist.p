/**
���������:     cr-t-hist.p
�� ������:     ����஢���� ⥪��� ��䨪�� � �����.
��� ࠡ�⠥�:   ���࠭�� ��䨪� ��� ��� ��������� ������஢ �� ���� ��᫥����� �᫮���,
                �᫨ ⠪��� ��䨪� ��� � ���ਨ. ����� ��䨪� (�� NumGr) ��������
                �� ���浪����� ������ �᫮��� �������
���� ����᪠:  �����஢騪
������:         27.03.2017 ���ᮢ �.�.
*/

DEFINE INPUT  PARAMETER iAuto   AS CHARACTER    NO-UNDO.    /* �����஢騪 (AUTO) ��� ������ */

{globals.i}
{pb_logit.i}
{intrface.get xclass}

DEFINE VARIABLE cLog        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dLcDate     AS DATE      NO-UNDO.
DEFINE VARIABLE iLcNum      AS INT64     NO-UNDO.
DEFINE VARIABLE mTypeIdnt   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vI          AS INT64     NO-UNDO.
DEFINE VARIABLE vIdnt       AS INT64     NO-UNDO.

IF FGetSetting ("�����", "������䈧�",?) NE "��"
THEN RETURN .

end-date  = TODAY - (IF (iAuto EQ "AUTO") THEN 1 ELSE 0).   /* �����஢騪 ����᪠�� ��஬ ᫥���饣� ��� */
mTypeIdnt = FGetSetting ("�����", "��䒨�","1,2,3").
cLog      = "/home2/bis/quit41d/log/to-hist/"
          + STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99") + ".log".
RUN LogIt("���", cLog).

FOR EACH loan
    WHERE (loan.contract    EQ "�।��")
      AND (loan.open-date   LE end-date)
      AND (loan.close-date  EQ ?)
      AND (loan.loan-status NE "����")
      AND (loan.filial-id   NE "0400")
    NO-LOCK
    BY loan.filial-id
    BY loan.cont-code
    :

    iLcNum = 0.
    FOR EACH loan-cond
        WHERE (loan-cond.contract   EQ loan.contract)
          AND (loan-cond.cont-code  EQ loan.cont-code)
        NO-LOCK
        BY loan-cond.since:

        iLcNum  = iLcNum + 1.
        dLcDate = loan-cond.since.
    END.

    DO vi = 1 TO num-entries (mTypeIdnt):
        vIdnt = INT64(ENTRY (vi,mTypeIdnt)).
        RUN CreateHistHeader(vIdnt, dLcDate, iLcNum) NO-ERROR.
        IF ERROR-STATUS:ERROR
        THEN RUN LogIt("  ??? �訡��: " + ERROR-STATUS:GET-MESSAGE(1), cLog).
    END.
END.

IF (iAuto NE "AUTO") THEN
put screen col 1 row 24 color normal STRING(" ","X(80)").
RUN LogIt("�⮯", cLog).
{intrface.del}
RETURN.

/* ���࠭���� ������ ��䨪� ************************************************** */
PROCEDURE CreateHistHeader:
    DEFINE INPUT PARAMETER  iIdnt   AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER  iSince  AS DATE  NO-UNDO.
    DEFINE INPUT PARAMETER  iNumGr  AS INT64 NO-UNDO.

    DEFINE VARIABLE vnn     AS INT64 NO-UNDO INITIAL 0.

    FIND FIRST term-obl-hist
        WHERE term-obl-hist.contract    EQ loan.contract
          AND term-obl-hist.cont-code   EQ loan.cont-code
          AND term-obl-hist.idnt        EQ iIdnt
          AND term-obl-hist.since       EQ iSince
        NO-LOCK NO-ERROR .
    IF NOT AVAILABLE term-obl-hist
    THEN DO:
        RUN LogIt(STRING(loan.cont-code, "x(30)") + STRING(iSince, "99.99.9999") + " ��䨪 N " + STRING(iNumGr)
                + " idnt=" + STRING(iIdnt), cLog).
        IF (iAuto NE "AUTO") THEN
        put screen col 1 row 24 "��ࠡ��뢠���� " + STRING(loan.cont-code, "x(30)") + STRING(iSince, "99.99.9999") + STRING(" ","X(10)").

        CREATE term-obl-hist.
        ASSIGN
            term-obl-hist.contract      = loan.contract
            term-obl-hist.cont-code     = loan.cont-code
            term-obl-hist.idnt          = iIdnt
            term-obl-hist.olap          = "1"
            term-obl-hist.since         = iSince
            term-obl-hist.chtype        = '1'
            term-obl-hist.description   = "�����࠭���� ������ �᫮��� " + STRING(iSince)
            term-obl-hist.user-id       = USERID("bisquit")
            .
        UpdateSigns ("term-obl-hist", STRING(term-obl-hist.tobl-id), "DateSave", STRING(NOW), YES).
        UpdateSigns ("term-obl-hist", STRING(term-obl-hist.tobl-id), "NumGr", STRING(iNumGr), YES).

        /* ����⪠ �� ��直� ��砩 */
        FOR EACH tobl-hist-amt
            WHERE (tobl-hist-amt.tobl-id    EQ term-obl-hist.tobl-id)
            EXCLUSIVE-LOCK:

            DELETE tobl-hist-amt.
        END.

        /* ����஢���� ��䨪� */
        FOR EACH term-obl
            WHERE term-obl.contract     EQ loan.contract
              AND term-obl.cont-code    EQ loan.cont-code
              AND term-obl.idnt         EQ iIdnt
            NO-LOCK:

            CREATE tobl-hist-amt.
            ASSIGN
                tobl-hist-amt.tobl-id      = term-obl-hist.tobl-id
                tobl-hist-amt.class-code   = "term-obl-hist"
                tobl-hist-amt.end-date     = term-obl.end-date
                tobl-hist-amt.dsc-beg-date = term-obl.dsc-beg-date
                tobl-hist-amt.amt-rub      = term-obl.amt-rub
                tobl-hist-amt.currency     = loan.currency
                vnn                        = vnn + 1
                tobl-hist-amt.nn           = vnn
                .
        END.

        RUN LogIt(STRING(loan.cont-code, "x(30)") + STRING(iSince, "99.99.9999") + " ��䨪 N " + STRING(iNumGr)
                + " - ��࠭��� idnt=" + STRING(iIdnt) + " : " + STRING(vnn, ">>>9") + " ��ப", cLog).
    END.
END PROCEDURE.  /* CreateHistHeader */
