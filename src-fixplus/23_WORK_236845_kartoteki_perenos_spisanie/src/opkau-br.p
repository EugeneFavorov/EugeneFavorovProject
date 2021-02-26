/*
     Filename: OP-KAU.P
      Comment: Браузер балансовых документов по картотеке. (вызывается из браузера документов катротеке)
   Parameters:
         Uses:
      Used by:
      Created: 31.01.2009 Sami 
*/

{globals.i}
{flt-file.i}
{form.def}
{sh-defs.i}

DEFINE BUFFER lop FOR op.
DEFINE BUFFER lop-entry FOR op-entry.
DEFINE BUFFER nkau FOR kau.

DEFINE VARIABLE mBic      AS CHARACTER NO-UNDO.    /*Бик банка*/
DEFINE VARIABLE mAcct     AS CHARACTER NO-UNDO.    /*Счет получателя*/
DEFINE VARIABLE mAcctStat AS CHARACTER NO-UNDO.    /*Счет счета*/
DEFINE VARIABLE mDocDate  AS DATE      NO-UNDO.    /*Дата планирования*/
DEFINE VARIABLE mSumma    AS DECIMAL   NO-UNDO.    /*Сумма*/
DEFINE VARIABLE mSummaOst AS DECIMAL   NO-UNDO.    /*Сумма остаток*/
DEFINE VARIABLE mSummaRub AS DECIMAL   NO-UNDO.    /*Сумма нац. вал.*/
DEFINE VARIABLE mPerenos  AS CHARACTER NO-UNDO.    /*Картотека переноса*/
DEFINE VARIABLE mPos      AS DECIMAL   NO-UNDO.    /*Остаток на балансовом счете*/
DEFINE VARIABLE mNumKau   AS CHARACTER NO-UNDO.    /*Для хранения КАУ по дебету проводки*/
DEFINE VARIABLE mTypeKart AS CHARACTER NO-UNDO.    /*Вид картотеки*/ 

DEFINE SHARED VARIABLE vKart AS CHARACTER NO-UNDO.    /*Перечень картотек, получаемых при запуске браузера*/

mTypeKart = vKart.

FORM
   op.order-pay          FORMAT "x(2)"               COLUMN-LABEL "Группа!очередности"             HELP "Группа очередности исполнения документа"
   op.ins-date           FORMAT "99/99/9999"         COLUMN-LABEL "Дата!поступления"               HELP "Дата поступления в банк"
   mDocDate              FORMAT "99/99/9999"         COLUMN-LABEL "Плановая!дата"                  HELP "Плановая дата акцепта"
   mSumma                FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "Сумма документа"                HELP "Сумма докумета"
   op.doc-date           FORMAT "99/99/9999"         COLUMN-LABEL "Документ от"                    HELP "Документ от"
   op.doc-num            FORMAT "x(12)"              COLUMN-LABEL "N Балансового!документа"        HELP "Номер балансового документа"
   mSummaRub             FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "В национальной!валюте"          HELP "Сумма в нац. валюте по учетному курсу ЦБ"
   mSummaOst             FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "Остаток списания!на картотеке"  HELP "Остаток на картотеке"
   op.doc-type           FORMAT "x(2)"               COLUMN-LABEL "Род!операции"                   HELP "Род операции"
   mBic                  FORMAT "x(9)"               COLUMN-LABEL "Бик банка"                      HELP "БИК банка"
   mAcct                 FORMAT "x(20)"              COLUMN-LABEL "Счет плательщика"               HELP "Счет плательщика"
   mPos                  FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "Остаток на!балансовом счете"    HELP "Остаток на счете, доступный для списания"
   mAcctStat             FORMAT "x(12)"              COLUMN-LABEL "Статус счета"                   HELP "Статус счета"
   mPerenos              FORMAT "x(13)"              COLUMN-LABEL "Перенос между!картотеками"      HELP "Откуда/куда был перенесен документ"
   WITH FRAME browse1
   TITLE COLOR bright-white "[ " + "БАЛАНСОВЫЕ ДОКУМЕНТЫ НА КАРТОТЕКЕ" + " ]" WIDTH 220.

{qrdef.i
   &buff-list        = "op"
   &need-buff-list   = "op"
   &Join-list        = "EACH"
   &SortBy           = "'BY op.order-pay BY op.doc-date BY op.doc-num'"
   &condition        = "YES"
   }

{navigate.cqr
   &autosigns        = "YES"
   &filt             = "YES"
   &file             = "op"
   &avfile           = "op"
   &files            = "op"
   &bf1              = "op.order-pay op.ins-date mDocDate mSumma op.doc-date op.doc-num mSummaRub mSummaOst op.doc-type mBic mAcct mPos mAcctStat mPerenos "
   &tmprecid         = "YES"
   &first-frm        = 1
   &maxfrm           = 1
   &CalcFld          = "mDocDate mSumma mSummaRub mSummaOst mBic mAcct mPos mAcctStat mPerenos "
   &CalcVar          = "opkau.clc "
   &postfind         = "opkau.clc "
   &oth1             = "opkau.f1 "
   &oth2             = "opkau.f2 "
   &oth6             = "flt-file.f6 "
   &help-label       = "'F1│F2 списание│F6 фильтр '"
   }

{intrface.del}
