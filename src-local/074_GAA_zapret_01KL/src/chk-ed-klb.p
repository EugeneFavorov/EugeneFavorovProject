{globals.i}

DEFINE INPUT  PARAMETER iOp  AS INT64   NO-UNDO. /* операция */
def output param oCanEdit as log init Yes.

for each op where op.op eq iOp AND op.doc-type eq "01КЛ" NO-LOCK:
/*IF op.filial-id EQ "0300" THEN oCanEdit = No.*/
IF NOT CAN-DO("I0400STS,I0400FEV",USERID("bisquit")) THEN oCanEdit = No.
end.
