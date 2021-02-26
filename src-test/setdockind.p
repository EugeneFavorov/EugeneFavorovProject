/*
    Copyright: 
     Filename: 
      Comment: Процедура "ВыпПосле".
   Parameters:
      Created: kam
     Modified:
*/


{globals.i}
DEF INPUT  PARAMETER in-rid  AS RECID   NO-UNDO.
    find first op where recid(op) = in-rid exclusive-lock no-error.
    if avail op then do:
        op.doc-kind = 'send'.
	pick-value = "YES".
    end.
