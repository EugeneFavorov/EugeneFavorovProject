/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: E-TELEX.I
      Comment: (0078824) Адаптирован для Биссмарт
   Parameters: нет
         Uses:
      Used by:
     Modified: 01.07.2007 12:01 KSV      (0078824) Адаптирован для Биссмарт
     Modified: <date> <who> <comment>
*/
find op-kind where recid(op-kind) = in-rec-kind no-lock no-error.
find first op-template of op-kind no-lock  no-error.

find first signs where signs.file = "op-kind" and signs.code = "debug" and
           signs.surr eq  op-kind.op-kind no-lock no-error.

if avail signs then  debug = INT64(signs.code-value).

{justasec}

{status.i}
test-ser = in-series-num.
if in-series-num = "1" then test-ser = "".
if not auto then do:
MESSAGE COLOR NORMAL SKIP "Тестовый режим ?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.
  CASE choice:
      WHEN NO THEN test = NO.
      WHEN YES THEN test = YES.
  END CASE.
end.
else test = no.
if choice eq ? then return.

cur-op-date = in-op-date.

if test eq no then do:
  {swchkser.i}
  assign
   nn = asc(in-series-num) - 48
   test-ser = "".
end. /* test eq no */


{chkacces.i}
{justasec}

assign
 test-ser = ""
 flag-err = no.
form
      op.doc-num
      op-entry.op-entry
      code.code
      code.name
with frame error.

procedure db:
/* Замена Плюс банк
   {e-swdbcr.i &dbcr=db}
*/ {pbe-swdbcr.i &dbcr=db}
/* Конец замены Плюс банк */
end procedure.

procedure cr:
/* Замена Плюс банк
   {e-swdbcr.i}
*/ {pbe-swdbcr.i}
/* Конец замены Плюс банк */
end procedure.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='08/09/2015 08:36:41.804+04:00' */
/* $LINTUSER='fiyu' */
/* $LINTMODE='1' */
/* $LINTFILE='e-telex.i' */
/*prosignC4G6F1eZ/mB4cxPdCTQ2aA*/