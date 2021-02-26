/*
    Copyright:  (C) 1990-2016 ПАО "Плюс Банк"
    Filename:  saldo-plus.p
    Comment:  Сальдирование остатков. Списание текущей суммы
*/
{globals.i}
{sh-defs.i}

DEFINE VARIABLE mKindRID AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFilial  AS CHARACTER NO-UNDO.

mFilial = shFilial.

FIND FIRST op-kind WHERE
   op-kind.op-kind EQ "СальдоАвт"
NO-LOCK NO-ERROR.

shFilial = "0000".
IF AVAIL(op-kind) THEN
   RUN g-saldo-plus.p (TODAY - 1,RECID(op-kind),NO) NO-ERROR.

shFilial = "0300".
IF AVAIL(op-kind) THEN
   RUN g-saldo-plus.p (TODAY - 1,RECID(op-kind),NO) NO-ERROR.

shFilial = "0500".
IF AVAIL(op-kind) THEN
   RUN g-saldo-plus.p (TODAY - 1,RECID(op-kind),NO) NO-ERROR.

shFilial = mFilial.

IF ERROR-STATUS:ERROR
   THEN RETURN ERROR "error".
   ELSE RETURN.
/******************************************************************************/
