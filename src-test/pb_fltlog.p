/*   Логирование запуска фильтров */

DEFINE PARAMETER BUFFER bOp FOR op.   
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER oResult AS LOGICAL NO-UNDO INIT YES.

{pb_logit.i}
DEFINE VARIABLE iLog        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.

iLog = "/home2/bis/quit41d/log/code_vo/" + "fbn_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + ".log".
cDet = SEARCH(iLog).
RUN LogIt(iParam + "  " + STRING(bop.op) + " " + bOp.op-status + " - Валютный контроль", iLog).
IF (cDet EQ ?) THEN OS-COMMAND SILENT VALUE("chmod 666 " + iLog).   /* Если лог только что создан, разрешаем запись всем */
