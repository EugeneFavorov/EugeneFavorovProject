   /* === График платежей по процентам === idnt=301 */ 
   /* Стандартная */
FORM
   term-obl.end-date 
      COLUMN-LABEL "ПЛАН"
      HELP         "Плановая дата оплаты процентов"
   term-obl.dsc-beg-date
      COLUMN-LABEL "ПЕРИОД"
      HELP         "Дата окончания платежного периода"
      FORMAT       "99.99.9999"
   term-obl.amt
      COLUMN-LABEL "СУММА"
   mSumm-t 
      COLUMN-LABEL "НЕПОГАШЕННЫЙ ОСТАТОК"
      HELP         "Непогашенный остаток по процентам"
      FORMAT ">>>,>>>,>>9.99"
   term-obl.sop-date 
      COLUMN-LABEL "ДАТА ОПЛАТЫ"
      HELP         "Дата фактической оплаты процентов"
WITH FRAME browse4 WIDTH 80 CENTERED
   TITLE COLOR bright-white "[ ГРАФИК ПЛАТЕЖЕЙ ПО ПРОЦЕНТАМ ]".

   /* === Плановый остаток === idnt=302 */
   /* Стандартная */
FORM 
   term-obl.end-date
      COLUMN-LABEL "ПЛАН. ДАТА"
      HELP         "Плановая дата выдачи кредита"
   term-obl.amt
      COLUMN-LABEL "СУММА"
WITH FRAME browse2 WIDTH 40 CENTERED
   TITLE COLOR bright-white "[ ПЛАНОВЫЙ ОСТАТОК ]".

   /* === Плановое погашение ссуды === idnt=303 */
   /* Стандартная */
FORM
   term-obl.fop-date
      FORMAT "99.99.99"
   term-obl.end-date
      COLUMN-LABEL "СРОК С"
      FORMAT "99.99.99"
   term-obl.dsc-beg-date
      COLUMN-LABEL "ПО"
      FORMAT "99.99.99"
      HELP "Дата окончания платежного периода"
   term-obl.amt-rub
      COLUMN-LABEL "ПЛАНОВАЯ СУММА"
   mSumm-t
      FORMAT       ">>,>>>,>>>,>>9.99"
      COLUMN-LABEL "НЕПОГАШ. ОСТАТОК"
      HELP         "Непогашенный остаток по срочному обязательству"
   term-obl.sop-date
      FORMAT "99.99.99"
   mProl
      FORMAT       "√/"
      COLUMN-LABEL "ПРОЛ"
      HELP         "Признак пролонгации срочного обязательства"
WITH FRAME browse3 
   /* WIDTH 80 CENTERED */
   TITLE COLOR bright-white "[ ПЛАНОВОЕ ПОГАШЕНИЕ ССУДЫ ]".

FORM
   term-obl.end-date
      COLUMN-LABEL "ПЛАН"
      HELP "Плановая дата оплаты комиссии"
      FORMAT "99.99.9999"
   term-obl.dsc-beg-date
      COLUMN-LABEL "ПЕРИОД"
      HELP "Дата окончания платежного периода"
      FORMAT "99.99.9999"
   term-obl.nn
      COLUMN-LABEL "КОД"
      FORMAT ">>>9"
      HELP "Код параметра комиссии"
   mParName
      COLUMN-LABEL "НАЗВАНИЕ ПАРАМЕТРА"
      FORMAT "x(19)"
      HELP "Наименование кода параметра комиссии"
   term-obl.amt-rub
      COLUMN-LABEL "СУММА"
      FORMAT ">>>,>>>,>>9.99"
      HELP "Сумма обязательств по оплате комиссии"
   mSumm-t
      COLUMN-LABEL "НЕПОГАШ. ОСТАТОК"
      FORMAT ">>>,>>>,>>9.99"
      HELP "Непогашенный остаток по комиссии"
WITH FRAME browse1 WIDTH 80
TITLE COLOR bright-white "[ ГРАФИК ПЛАТЕЖЕЙ ПО КОМИССИЯМ ]".
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='10/12/2014 15:11:25.113+04:00' */
/* $LINTFILE='brw-toblcm.frm' */
/*prosignDiI3rhcYtXv11A7hyhrqUQ*/