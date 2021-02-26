/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: summ-t1.p
      Comment: Summ-t1
   Parameters: нет
         Uses:
      Used by: 
      Created: ??/??/?? ??
     Modified: 21/09/00 Om Ошибка прав.
     Modified:             (Не разрешалось копировать договор созданным собою.)
     Modified: 16/03/2003 Om Доработка: изменение вызова браузера договоров.
*/

&IF DEFINED (PERC) =  0 &THEN 
{globals.i}
{lshpr.pro}
&ENDIF
&GLOB iskldbparam "95"

&IF DEFINED (SUMM_PROC) =  0 &THEN

DEF OUTPUT PARAM summ-t LIKE term-obl.amt INIT 0.
DEF INPUT  PARAM l1     AS RECID.
DEF INPUT  PARAM l2     AS RECID.
&ELSE

DEF VAR summ-t AS DEC NO-UNDO.
&ENDIF

DEF VAR e1        LIKE loan-var.balance INIT 0.
DEF VAR e2        LIKE loan-var.balance INIT 0.
DEF VAR e3        LIKE loan-var.balance INIT 0.
DEF VAR i         AS INT64  NO-UNDO.
DEF VAR fl1       AS LOG  NO-UNDO INIT NO.
DEF VAR vSumm     AS DEC  NO-UNDO.
DEF VAR vDbSumDec AS DEC  NO-UNDO.
DEF VAR vCrSumDec AS DEC  NO-UNDO.
DEF VAR vDateN    AS DATE NO-UNDO.
DEF VAR mPayDate  AS DATE NO-UNDO.

DEF BUFFER xerm-obl FOR term-obl.

&IF DEFINED (SUMM_PROC) =  0 &THEN
FIND term-obl WHERE 
   RECID(term-obl) =  l1 
NO-LOCK NO-ERROR.
FIND loan WHERE 
   RECID(loan) =  l2 
NO-LOCK NO-ERROR.

mPayDate = DATE(GetSysConf("ДатаОплаты%")) NO-ERROR.
&ENDIF
ASSIGN
   ERROR-STATUS:ERROR = NO
&IF DEFINED (SUMM_PROC) =  0 &THEN
      /* Если ?, то берем дату пересчета договора. */
   mPayDate           = IF mPayDate =  ? THEN loan.since 
                                         ELSE mPayDate
&ELSE
   mPayDate = iDatePlat
&ENDIF
      /* Определяем дату начального решения по Договорам */
   vDateN             = DATE(FGetSettingEx("ДатаНачКред", ?, "", NO))
   vDateN             = IF vDateN   =  ? THEN DATE(1,1,1900)
                                         ELSE vDateN
.  

{summ-t1.i {&*}}
/* $LINTFILE='summ-t1.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='pase' */
/* $LINTDATE='13/03/2017 11:45:49.896+03:00' */
/*prosignBjc1OBaX7uPQ72ZqVvgdtA*/