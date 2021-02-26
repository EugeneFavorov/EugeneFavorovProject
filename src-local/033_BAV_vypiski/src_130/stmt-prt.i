IF ts.cmode EQ 2 AND acct.cust-cat EQ "�" THEN {&NEXT}.
/*
IF NOT ts.emptystmt                                              AND
   NOT CAN-FIND(FIRST op-ENTRY WHERE op-entry.acct-db EQ acct.acct
                           AND op-entry.op-DATE GE (IF vZoBeg EQ ? THEN {&begdate} ELSE MIN ({&begdate}, vZoBeg))
                           AND op-entry.op-DATE LE (IF vZoEnd EQ ? THEN {&enddate} ELSE MAX ({&enddate}, vZoEnd))
                           AND op-entry.op-STATUS GE gop-status) AND
   NOT CAN-FIND(FIRST op-ENTRY WHERE op-entry.acct-cr EQ acct.acct
                           AND op-entry.op-DATE GE (IF vZoBeg EQ ? THEN {&begdate} ELSE MIN ({&begdate}, vZoBeg))
                           AND op-entry.op-DATE LE (IF vZoEnd EQ ? THEN {&enddate} ELSE MAX ({&enddate}, vZoEnd))
                           AND op-entry.op-STATUS GE gop-status)

THEN {&NEXT}.
*/

NotEmpty:
DO:
    IF (NOT ts.emptystmt)   /* ����� �� ���⠥� */
    THEN DO:
        Oe-db:  /* �஢��塞 ����⮢� �஢���� */
        FOR EACH op-entry
            WHERE (op-entry.acct-db EQ acct.acct)
              AND (op-entry.op-date GE (IF vZoBeg EQ ? THEN {&begdate} ELSE MIN ({&begdate}, vZoBeg)))
              AND (op-entry.op-date LE (IF vZoEnd EQ ? THEN {&enddate} ELSE MAX ({&enddate}, vZoEnd)))
            NO-LOCK,
        FIRST op OF op-entry
            NO-LOCK:

            {stmt-tst.i &label=Oe-db &corr=cr}
            LEAVE NotEmpty. /* ������� �஢���� ��� �믨᪨ */
        END.

        Oe-cr:  /* �஢��塞 �।�⮢� �஢���� */
        FOR EACH op-entry
            WHERE (op-entry.acct-cr EQ acct.acct)
              AND (op-entry.op-date GE (IF vZoBeg EQ ? THEN {&begdate} ELSE MIN ({&begdate}, vZoBeg)))
              AND (op-entry.op-date LE (IF vZoEnd EQ ? THEN {&enddate} ELSE MAX ({&enddate}, vZoEnd)))
            NO-LOCK,
        FIRST op OF op-entry
            NO-LOCK:

            {stmt-tst.i &label=Oe-cr &corr=db}
            LEAVE NotEmpty. /* ������� �஢���� ��� �믨᪨ */
        END.

        {&NEXT}.    /* �� ����� �஢���� ��� �믨᪨ �� ������� */
    END.
END.

run stmt-prt.p ({&begdate},{&enddate},{&bufacct}.currency,RECID({&bufacct})).
{empty stmt}
{empty stmtotal}
RUN DeleteOldDataProtocol IN h_base ("stmtPrt").
