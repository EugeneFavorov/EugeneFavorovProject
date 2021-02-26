/*для удаления проводок при расчете резервов*/
{globals.i}
{rsrv.def}
{sh-defs.i}

DEF INPUT  PARAM pDate    AS DATE NO-UNDO.
DEF INPUT  PARAM pBefDate AS DATE NO-UNDO.

DEF INPUT-OUTPUT PARAM TABLE FOR tProv.

    
DEFINE BUFFER xop FOR op.

FIND FIRST op-kind WHERE op-kind.op-kind EQ 'rsrv_pr7' NO-LOCK NO-ERROR.

{op(ok.del
   &date     = pDate
   &op-kind  = 'rsrv_pr7'
   &set-code = "'КурсУдал'"
   &mes1     = "В этом дне уже проводился расчет"
   &mes2     = "резервов! Удалить?"
   &mes3     = "Удаление документов..."
}