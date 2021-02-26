/*              
Банковская интегрированная система БИСквит
*/

{globals.i}
{intrface.get tmess}

DEFINE INPUT PARAMETER iCID AS CHARACTER NO-UNDO.

FIND FIRST code WHERE TRUE
   AND code.class   EQ "Over-MIR"
   AND code.parent  EQ "Over-MIR"
   AND code.code    EQ iCID
EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL(code) THEN ASSIGN code.misc[1] = "YES".

RETURN "YES".
