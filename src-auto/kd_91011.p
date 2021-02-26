/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: rsrv-chk.p
      Comment: Исключение из временной таблицы тех договоров, по которым
               нет оплаты в текущий день.

   Parameters:
      Used by:
      Created: 
     Modified: xaro 01.12.2005 заявка 0054846
*/

{globals.i}
{flt-file.i}        /* Объявление фильтра по договорам */
{all_note.def}      /* Таблица с recid, выбранных по фильтру записей Shared
                       и датой текущего опер.дня - in_op_date */
{flt_var.def}
{sh-defs.i}

{intrface.get olap}
{intrface.get loan}
{intrface.get pint}
{checkov.i}

DEFINE INPUT PARAMETER iParam  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER iOpRid  AS RECID  NO-UNDO.

DEF VAR mFlDel AS LOG NO-UNDO.
def var vkol-bal-tot as int no-undo.
def var vkol-bal as int no-undo.
DEF VAR vR AS LOG NO-UNDO.

DEF BUFFER bloan FOR loan.
DEF BUFFER xterm-obl FOR term-obl.


/* проверка количества отобранных записей */
FIND LAST all_recids NO-ERROR.
IF NOT AVAIL all_recids THEN
DO:
   {intrface.del }
   RETURN.
END.

ASSIGN
   gend-date  = svPlanDate
   end-date   = svPlanDate
   in_op_date = svPlanDate.

FOR EACH all_recids NO-LOCK, 
   FIRST loan WHERE RECID(loan) = all_recids.rid
   NO-LOCK:

   FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ "Кредит"
   NO-LOCK NO-ERROR.

   mFlDel = TRUE.

   IF     CAN-DO("!loan-tran-lin,!loan-guarantee,!loan-transh-sim,!loan_trans_ov,*",TRIM(loan.class-code)) 
      AND CAN-DO("!созд,*",loan.loan-status)
      AND AVAIL loan-acct
      AND CAN-DO ("4*,60315*", TRIM (loan-acct.acct))

   THEN mFlDel = False.

   IF mFlDel THEN
   DO:
      DELETE all_recids.
      NEXT.
   END.

if loan.user-id = 'SERVSOUZ' OR loan.loan-status EQ "РЕФ" then DELETE all_recids.

/*   IF checkov(BUFFER loan) THEN DELETE all_recids. */ 
END. /*FOR EACH*/

   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES     FILL (" ",79).
   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 FILL (" ",79).
   PAUSE 0.  

{intrface.del}
