/*              
Банковская интегрированная система БИСквит
*/

{globals.i}
{intrface.get tmess}

DEFINE INPUT PARAMETER iAcct AS CHARACTER NO-UNDO.

FIND FIRST acct WHERE TRUE
   AND acct.acct     EQ iAcct
   AND acct.currency EQ ""
EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL(acct) THEN ASSIGN acct.close-date = TODAY.

RETURN "YES".
