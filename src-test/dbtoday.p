/*
               ОАО "Плюс Банк"
    filename: dbtoday.p
      comment: Запрет запуска любой транзакции в будущем операционном дне.
      comment: Ставится вместо процедуры запуска транзакции. 
                Сама процедура запуска перемещается в Выполнить До.
      Created: 27/05/2014
     Modified: ...
*/
{globals.i}
DEF INPUT  PARAM iDate    AS DATE        NO-UNDO.
DEF INPUT  PARAM iOprid   AS RECID       NO-UNDO.

DEF VAR iopkind as char no-undo.

FIND FIRST op-kind WHERE RECID(op-kind) EQ iOprid NO-LOCK NO-ERROR.

IF iDate <= TODAY THEN 
DO:
	iopkind = getxattrvalue ("op-kind", op-kind.op-kind, "РеалПроцТр").
    RUN VALUE(iopkind + '.p') (iDate,iOpRid).
END.
ELSE MESSAGE ('Нельзя запускать данную транзакцию в будущем дне') VIEW-AS ALERT-BOX.

RETURN "NO".