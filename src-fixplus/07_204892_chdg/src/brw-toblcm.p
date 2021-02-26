/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: BRW-TOBLCM.P
      Comment: Браузер графика комиссий по кредитным договорам.
   Parameters: none
         Uses:
      Used by:
      Created: 08.11.2008 16:22 Fepa (95876)
*/

{globals.i}
{flt-file.i} /* Определение структуры динамического фильтра */

DEF VAR mParName   AS CHAR NO-UNDO. /* Наименование параметра */
DEF VAR mSummToPay AS DEC  FORMAT ">>>,>>>,>>>,>>9.99" 
         COLUMN-LABEL "НЕПОГАШ. ОСТАТОК" NO-UNDO. /* Непогашенный остаток   */
DEF VAR mIdnt      AS CHAR NO-UNDO.
DEF VAR mFirstFrm  AS INT64 NO-UNDO.                      /* Стартовый фрейм */
DEF VAR mSumm-t     AS DEC  FORMAT ">>>,>>>,>>>,>>9.99" 
         COLUMN-LABEL "НЕПОГАШ. ОСТАТОК" NO-UNDO.
DEF VAR mProl       AS LOG             NO-UNDO.            /* Признак пролонгации */ 


mIdnt = GetFltVal("idnt").
mIdnt = IF mIdnt EQ ? OR mIdnt EQ "0" THEN "10" ELSE GetFltVal("idnt").

/* Найдем договор, интересует дата открытия в brw-toblcm.fnd --> t-otchcm.p */
FIND FIRST loan WHERE loan.contract  EQ GetFltVal("contract")
                  AND loan.cont-code EQ GetFltVal("cont-code")
NO-LOCK NO-ERROR.


CASE mIdnt:
   WHEN "301" THEN
      mFirstFrm = 4.
   WHEN "302" THEN
      mFirstFrm = 2.
   WHEN "303" THEN
      mFirstFrm = 3.
   OTHERWISE
      mFirstFrm = 1.
END CASE.

{brw-toblcm.frm}
{brw-toblcm.qry}

{navigate.cqr
   &file       = "term-obl"
   &files      = "term-obl"
   
   &nodel      = "/*"
   &help-label = "'+'"
   &first-frm  = mFirstFrm
   &bf1        = " term-obl.end-date term-obl.dsc-beg-date term-obl.nn mParName term-obl.amt-rub  ~
mSumm-t "
   &bf2        = "term-obl.end-date term-obl.amt "
   &bf3        = "term-obl.fop-date term-obl.end-date term-obl.dsc-beg-date term-obl.amt-rub ~
mSumm-t term-obl.sop-date mProl "
   &bf4        = "term-obl.end-date term-obl.dsc-beg-date term-obl.amt mSumm-t term-obl.sop-date "
   &postfind   = "brw-toblcm.fnd "
   &look       = "brw-toblcm.nav "
   &total      = "total.cqr "
      &cfldx     = "term-obl.amt-rub mSumm-t"
}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='10/12/2014 15:11:06.357+04:00' */
/* $LINTFILE='brw-toblcm.p' */
/*prosignwHsokvyqhesXJzk9MQ7+/Q*/