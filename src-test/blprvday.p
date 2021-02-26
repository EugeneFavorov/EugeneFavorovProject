{globals.i}
{sh-defs.i}

DEFINE INPUT PARAMETER iFilial AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCurrFilial AS CHARACTER NO-UNDO.

mCurrFilial = shFilial.

shFilial = iFilial.

FIND FIRST op-date WHERE
   op-date.op-date EQ (TODAY - 1)
NO-LOCK NO-ERROR.
IF AVAIL(op-date) THEN
DO:   
   {blockday.i
      &op-date      = "op-date.op-date"
      &BLOCK        = YES
      &error-action = "."
   }
   MESSAGE "День заблокирован.".
END.
ELSE MESSAGE "День для блокировки не найден.".

shFilial = mCurrFilial.

{intrface.del}

RETURN.
