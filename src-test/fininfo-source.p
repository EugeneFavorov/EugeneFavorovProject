/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: FININFO-SOURCE.P
      Comment: Финансовая информация. Источники происхождения денежных средств (0265220)
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

DEFINE INPUT  PARAMETER iCustCat    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iCustId     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iFrmname    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iClass-Code AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iClassCode  AS CHARACTER  NO-UNDO.

/*DEFINE VARIABLE num-line    AS INT64     NO-UNDO. /* Необходимо для navigate.cqr */*/
DEFINE VARIABLE mSource     AS CHARACTER NO-UNDO. /* Код источника */
DEFINE VARIABLE mSourceDesc AS CHARACTER NO-UNDO. /* Дополнительное описание источника */
DEFINE VARIABLE mSymbol     AS CHARACTER NO-UNDO.

/* Хэндл временной таблицы TmpObj при фильтрации по ней */
DEFINE VARIABLE mHTmpObj    AS HANDLE    NO-UNDO.
ASSIGN
   mSymbol = CHR(9).

FORM
   mSource
      VIEW-AS FILL-IN SIZE 20 BY 1
      FORMAT       "x(1000)"
      COLUMN-LABEL "Код источника"
      HELP         "Источник происхождения денежных средств"
   mSourceDesc
      VIEW-AS FILL-IN SIZE 35 BY 1
      FORMAT       "x(5000)"
      COLUMN-LABEL "Дополнительное описание источника"
      HELP         "Дополнительное описание источника происхождения денежных средств"   
   cust-ident.open-date
      FORMAT       "99/99/9999"
      COLUMN-LABEL "Дата начала!действия"
      HELP         "Дата начала действия"
   cust-ident.close-date
      FORMAT "99/99/99" 
      COLUMN-LABEL "Дата!окончания!действия"
      HELP         "Дата окончания действия"
WITH FRAME browse1  TITLE  COLOR BRIGHT-WHITE iFrmname .

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
   &bf1        = "mSource mSourceDesc cust-ident.open-date cust-ident.close-date"
   &cf1        = "mSource mSourceDesc cust-ident.open-date cust-ident.close-date"
   
   &edit       = "bis-tty.ef "
                     &class             = iClassCode
                     &before-run-method = "fininfo-source.bfe "
   &postfind   = "fininfo-source.fnd "
                    
   &look      = "bis-tty.nav "
   
   &rat_upclass = iClassCode
}

END. /* MAIN-BLOCK: */

RUN SetFltField IN THIS-PROCEDURE ("UseTmpObjInQuery",mHTmpObj).

{intrface.del}          /* Выгрузка инструментария. */ 
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='29/12/2015 19:34:33.997+04:00' */
/* $LINTUSER='kozv' */
/* $LINTMODE='1' */
/* $LINTFILE='fininfo-source.p' */
/*prosignb1kfO9Sq1fUZ2KFgLZgQPQ*/