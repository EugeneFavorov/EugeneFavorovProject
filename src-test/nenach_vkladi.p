/* DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no  */
/*      LABEL "Toggle 1"                           */
/*      VIEW-AS TOGGLE-BOX                         */
/*      SIZE 13.4 BY .81 NO-UNDO.                  */

{tmprecid.def}

/*
def new global shared temp-table tmprecid no-undo
   field id as recid
index id id.
  */
{globals.i}
{g-error.def}
{g-defs.i}
{crdps.def}
{dpsproc.def}
{def-wf.i new}
{defframe.i new}
{invest.num}
{ksh-defs.i new}
{intrface.get "xclass"}
{intrface.get "ltran"}
{intrface.get cust}
{intrface.get tmcod}
{sh-defs.i}
{ksh-defs.i}

define var ostatok as DECIMAL no-undo.
define var MIN_ost as CHAR no-undo.
def var acctdate AS DATE FORMAT 99/99/9999 no-undo.
DEF VAR galka AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEF VAR galka2 AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.

DEFINE TEMP-TABLE tt-1lacct5
    FIELD cont-code AS CHARACTER
    FIELD cont-type AS CHARACTER
    FIELD open-date AS date
    FIELD ostatok AS DECIMAL
    /*INDEX cont-code cont-code*/
    .

/* ��室 �᫨ ��� �� ������ �⮡࠭���� ������� */
if not can-find(first tmprecid) then return.



acctdate = today.

pause 0.

DEF FRAME ftune /* ����䥩� */
    acctdate label "��� ���᫥��� " help " ��� ���᫥��� "
    galka LABEL  "�஢����� �� ����ॡ������" HELP "�஢����� �� ����ॡ������"
    galka2 LABEL  "�� �஢����� � ��� < 120 ��" HELP "�� �஢����� ������ �� ����ॡ������ � ���⪮� ����� 120 �㡫��"
    with centered row 10 overlay side-labels 1 col
    color messages title "[ ��ࠬ���� ]".
    
Do on error undo, leave on endkey undo, leave:
  Update
    acctdate view-as fill-in
    galka view-as TOGGLE-BOX
    galka2 view-as TOGGLE-BOX
    with frame ftune.
End.

Hide frame ftune no-pause.
{init-bar.i "���㭤���..."}
{setdest.i &custom = "IF TRUE THEN 0 ELSE"}
for each tmprecid,
    first loan where loan.contract eq "dps" and
    recid(loan) eq tmprecid.id
    NO-LOCK:
        IF galka THEN
        DO:
        find first loan-acct of loan where
        (loan-acct.acct-type = "loan-dps-p") OR (loan-acct.acct-type = "loan-dps-t") no-lock no-error. /*���� �� ⨯� �������*/
        if avail loan-acct THEN
        DO:
            RUN acct-pos IN h_base (LOAN-ACCT.ACCT,LOAN-ACCT.CURRENCY,acctdate, acctdate, ?).
                IF length(loan-acct.currency) > 0 THEN /*�஢�ઠ �� �ਧ��� �����*/
                    ostatok = ABS(sh-in-val).
                ELSE ostatok = ABS(sh-bal).
                IF (ostatok > 0 and not galka2) or (ostatok >= 120 and galka2 and loan-acct.acct-type = "loan-dps-p") THEN
                DO:
                    find first loan-acct of loan where
                        (loan-acct.acct-type = "loan-dps-int") no-lock no-error. /*���� �� ⨯� �������*/
                        if avail loan-acct THEN 
                        DO:
                            FIND FIRST op-entry                                    /* ���� �஢���� */
                                WHERE op-entry.acct-cr = loan-acct.acct AND 
                                    substring(op-entry.acct-db,1,3) = "706" AND 
                                        op-entry.op-date = acctdate NO-LOCK NO-ERROR.
                            IF NOT AVAIL op-entry THEN
                            DO:
                            CREATE tt-1lacct5.
                            
                            ASSIGN
                            tt-1lacct5.cont-code = loan.cont-code
                            tt-1lacct5.cont-type = loan.cont-type
                            tt-1lacct5.open-date = loan.open-date
                            tt-1lacct5.ostatok = ostatok
                            .
                                /*PUT UNFORMATTED string(loan.cont-code) format "x(30)" ' ' string(loan.cont-type) format "x(10)" ' ' loan.open-date ' ' ostatok ' '  SKIP.*/
                                end.
                        END.
               END.
        END.
        END.
        ELSE DO:
                find first loan-acct of loan where
                (loan-acct.acct-type = "loan-dps-t") no-lock no-error. /*���� �� ⨯� �������*/
                if avail loan-acct THEN
                DO:
                    RUN acct-pos IN h_base (LOAN-ACCT.ACCT,LOAN-ACCT.CURRENCY,acctdate, acctdate, ?).
                        IF length(loan-acct.currency) > 0 THEN /*�஢�ઠ �� �ਧ��� �����*/
                            ostatok = ABS(sh-in-val).
                        ELSE ostatok = ABS(sh-bal).
                        IF ostatok > 0 THEN
                        DO:
                            find first loan-acct of loan where
                                (loan-acct.acct-type = "loan-dps-int") no-lock no-error. /*���� �� ⨯� �������*/
                                if avail loan-acct THEN 
                                DO:
                                    FIND FIRST op-entry                                    /* ���� �஢���� */
                                        WHERE op-entry.acct-cr = loan-acct.acct AND 
                                            substring(op-entry.acct-db,1,3) = "706" AND 
                                                op-entry.op-date = acctdate NO-LOCK NO-ERROR.
                                    IF NOT AVAIL op-entry THEN
                                    DO:
                                    CREATE tt-1lacct5.
                                    
                                    ASSIGN
                                    tt-1lacct5.cont-code = loan.cont-code
                                    tt-1lacct5.cont-type = loan.cont-type
                                    tt-1lacct5.open-date = loan.open-date
                                    tt-1lacct5.ostatok = ostatok
                                    .
                                    
                                            end.
                                        /*PUT UNFORMATTED string(loan.doc-ref) format "x(30)" ' ' string(loan.cont-type) format "x(10)" ' ' loan.open-date ' ' ostatok ' '  SKIP.*/
                                END.
                       END.
                END.
        END.
END.
for each tt-1lacct5
NO-LOCK BY tt-1lacct5.ostatok DESCENDING:
PUT UNFORMATTED tt-1lacct5.cont-code format "x(30)" ' ' tt-1lacct5.cont-type format "x(10)" ' ' tt-1lacct5.open-date ' ' tt-1lacct5.ostatok ' '  SKIP.
end.
{preview.i}
