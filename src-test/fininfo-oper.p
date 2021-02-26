/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: FININFO-OPER.P
      Comment: Финансовая информация по юр. лицам. Планируемые операции по счету (224331)
   Parameters: iCustCat - тип клиента
               iCustId  - код клиента
               ifrmName - заголовок формы
               iClassCode - код класса
         Uses:
      Used by:
      Created: 25.04.2014 14:16
*/

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get dynqr}    /* Библиотека для работы с динамическими запросами. */
{navigate.def}
{flt-file.i}

DEFINE INPUT  PARAMETER iCustCat    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iCustId     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ifrmname    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iClassCode  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE num-line    AS INT64     NO-UNDO. /* Необходимо для navigate.cqr */
DEFINE VARIABLE mVidOper    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPeriodOper AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmountOper AS INT64     NO-UNDO.
DEFINE VARIABLE mSummaOper  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCurrency   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymbol     AS CHARACTER  NO-UNDO.

/* Хэндл временной таблицы TmpObj при фильтрации по ней */
DEFINE VARIABLE mHTmpObj    AS HANDLE    NO-UNDO.

ASSIGN
   mSymbol = CHR(9).

FORM
   mVidOper
      VIEW-AS FILL-IN SIZE 17 BY 1
      FORMAT       "x(20)"
      COLUMN-LABEL "Код вида!операции"
      HELP         "Код Вида Операции"
   mPeriodOper
      VIEW-AS FILL-IN SIZE 16 BY 1
      FORMAT       "x(20)"
      COLUMN-LABEL "Частота!операций"
      HELP         "Частота Операций"
   mAmountOper
      VIEW-AS FILL-IN SIZE 7 BY 1
      FORMAT       ">>>>>>9"
      COLUMN-LABEL "Кол-во!операций"
      HELP         "Количество Операций"
   mSummaOper
      VIEW-AS FILL-IN SIZE 11 BY 1
      FORMAT       ">>>>>>>9.99"
      COLUMN-LABEL "Сумма!операций,!в миллионах"
      HELP         "Сумма Операций"
   mCurrency
      VIEW-AS FILL-IN SIZE 3 BY 1
      FORMAT       "x(3)"
      COLUMN-LABEL "Код!Вал"
      HELP         "Код валюты"
   cust-ident.open-date
      COLUMN-LABEL "Дата!начала!действия"
      HELP         "Дата начала действия"
      FORMAT "99/99/99" 
   cust-ident.close-date
      COLUMN-LABEL "Дата!окончания!действия"
      HELP         "Дата окончания действия"
      FORMAT "99/99/99" 
WITH FRAME browse1  TITLE  COLOR BRIGHT-WHITE ifrmname .

mHTmpObj   = WIDGET-HANDLE (GetFltValEx("UseTmpObjInQuery","*")) NO-ERROR.
RUN SetFltField IN THIS-PROCEDURE ("UseTmpObjInQuery","no").

MAIN-BLOCK:
DO 
   ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

{qrdef.i
   &buff-list        = "cust-ident"
   &Join-list        = "EACH"
   &fixed-where      = "'WHERE cust-ident.cust-id EQ ' + STRING(iCustId) + 
                        ' AND cust-ident.cust-cat EQ ""' + iCustCat + 
                      '"" AND cust-ident.class-code EQ ""' + iClassCode + '""' " 
}

{navigate.cqr
   &file       = "cust-ident"
   &files      = "cust-ident"
   &avfile     = "cust-ident"
   &maxfrm     = 1
   &bf1        = "mVidOper mPeriodOper mAmountOper mSummaOper mCurrency cust-ident.open-date cust-ident.close-date"
   &cf1        = "mVidOper mPeriodOper mAmountOper mSummaOper mCurrency cust-ident.open-date cust-ident.close-date"
   
   &edit       = "bis-tty.ef "
                     &class             = iClassCode
                     &before-run-method = "fininfo-cbc.bfe "
   &postfind   = "fininfo-oper.fnd "
                    
   &look      = "bis-tty.nav "
   
   &rat_upclass = iClassCode
}

END. /* MAIN-BLOCK: */

RUN SetFltField IN THIS-PROCEDURE ("UseTmpObjInQuery",mHTmpObj).

{intrface.del}          /* Выгрузка инструментария. */ 
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='15/01/2015 07:49:28.355+04:00' */
/* $LINTFILE='fininfo-oper.p' */
/*prosignfAHwsmzeEP5dgr8IGF/UMQ*/