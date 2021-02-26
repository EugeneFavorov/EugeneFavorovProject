
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2005 ЗАО "Банковские информационные системы"
     Filename: mycount2-corr.p
      Comment: рассчитать проценты для параметра номер 4 и вычесть
               все дебитовые обороты
   Parameters:
         Uses:
      Used by:
      Created: 10.05.2007 16:45 BIS
     Modified: 02.08.2011 16:25 Daru   <comment>
*/

{globals.i}
{t-otch.i NEW}
{pick-var.i}
{intrface.get xclass} /* Инструменты для работы с матасхемой       */
{intrface.get loan}
{intrface.get date}

DEFINE INPUT PARAMETER iContract AS CHAR.
DEFINE INPUT PARAMETER iContCode AS CHAR.
DEFINE INPUT PARAMETER i-end-date AS DATE.
DEFINE OUTPUT PARAMETER o-summa AS DECIMAL INITIAL 0 NO-UNDO.

DEFINE VARIABLE mSumm_pr AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumm_db AS DECIMAL NO-UNDO.
DEFINE VARIABLE CodOstPar AS INT64 NO-UNDO .
DEFINE VARIABLE vDateNR AS DATE NO-UNDO .

DEFINE VARIABLE mBeg-date AS DATE NO-UNDO.
DEFINE VARIABLE dat-per   AS DATE NO-UNDO .
DEFINE BUFFER   fterm-obl FOR term-obl.
DEFINE BUFFER   pterm-obl FOR term-obl.
DEFINE BUFFER   bloan-cond FOR loan-cond.
DEFINE BUFFER   loan-int FOR loan-int.

/*
   Предусловия:
   i-end-date - данный входной параметр совпадает с плановой датой погашения
   процентов

   Необходимо вычислить i-beg-date - предыдущая плановая дата .

*/

FIND FIRST loan WHERE
   loan.contract EQ iContract
   AND loan.cont-code EQ iContCode
   NO-LOCK NO-ERROR.

FIND LAST bloan-cond WHERE 
          bloan-cond.contract  EQ iContract
      AND bloan-cond.cont-code EQ iContCode
      AND bloan-cond.since     LE i-end-date
NO-LOCK NO-ERROR.

CodOstPar = GetParCode(loan.class-code,'КодОснДолг') .

FIND LAST term-obl OF loan WHERE
   term-obl.idnt EQ 1
   AND term-obl.end-date  < i-end-date
   NO-LOCK NO-ERROR.

{ch_dat_p.i}

IF AVAIL term-obl THEN
   mBeg-date = term-obl.end-date + 1 .
ELSE
   mBeg-date = loan.open-date + 1 .

RUN pint.p (iContract, iContCode, mBeg-date, i-end-date, STRING(mint[1])).

FOR EACH otch1:
   ACCUMULATE otch1.summ_pr (TOTAL).
END.
mSumm_pr = ACCUM TOTAL otch1.summ_pr. /*Сколько должно быть на дату mCurDate*/

IF bloan-cond.disch-type eq 17 then
   mBeg-date = max(FirstMonDate(i-end-date), (loan.open-date + 1)).

RUN GetDateNRKred(?,OUTPUT vDateNR).
IF vDateNR NE ?
   AND mBeg-date LE vDateNR THEN
   mBeg-date = vDateNR + 1.

/*вычесть все дебетовые обороты по параметру 4*/
FOR EACH loan-int WHERE
       loan-int.contract  EQ iContract
   AND loan-int.cont-code EQ iContCode
   AND loan-int.mdate     GE mBeg-date
   AND loan-int.mdate     LE i-end-date
   AND loan-int.id-d      EQ mint[1] + CodOstPar
   NO-LOCK
:
   ACCUMULATE loan-int.amt-rub (TOTAL).
END.
mSumm_db = ACCUM TOTAL loan-int.amt-rub.

/*
 message
   mint[1] skip
   vDateNR mBeg-date i-end-date SKIP
   program-name(1) skip
   "mSumm_pr = " mSumm_pr skip
   "mSumm_db = " mSumm_db skip(2)
   "Что должны начислить : mSumm_pr - mSumm_db = " mSumm_pr - mSumm_db
   loan.interest[1]
view-as alert-box.
*/

o-summa = mSumm_pr - mSumm_db.


IF ABS(o-summa) EQ 0.01 THEN
DO:
   FIND FIRST pterm-obl WHERE
              pterm-obl.contract  EQ loan.contract
         AND  pterm-obl.cont-code EQ loan.cont-code
         AND  pterm-obl.idnt      EQ 1
         AND  pterm-obl.end-date  LT i-end-date
         NO-LOCK NO-ERROR.
   IF AVAIL pterm-obl  THEN
      FOR EACH loan-int OF loan WHERE loan-int.mdate GT pterm-obl.end-date NO-LOCK,
          EACH op OF loan-int WHERE op.op-kind BEGINS "prod_201" NO-LOCK:
          o-summa = 0.
          LEAVE.  
      END.
END.



/* i-end-date */
/* Первая плановая после Даты нач решения */
/* Если выходит отриц значение , то  восстановим Сколько должно быть на дату mCurDate*/

IF o-summa LT 0 AND vDateNR NE ? AND i-end-date GE vDateNR 
THEN DO:
   FIND FIRST fterm-obl WHERE
              fterm-obl.contract  EQ loan.contract
         AND  fterm-obl.cont-code EQ loan.cont-code
         AND  fterm-obl.idnt      EQ 1
         AND  fterm-obl.end-date  EQ i-end-date
         NO-LOCK NO-ERROR .

   IF     AVAILABLE fterm-obl
      AND o-summa LT 0 THEN
          o-summa =  mSumm_pr.

END.

/*
message
    o-summa
view-as alert-box.
*/
{intrface.del loan}
RETURN "".
