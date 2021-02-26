/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: FININFO.P
      Comment: Финансовая информация по юр. лицам (202227)
   Parameters: iCustCat - тип клиента
               iCustId  - код клиента
               mfrmName - заголовок формы
               mClass-Code - ФинПолож\ДелРепут
               mClassCode  - код класса
         Uses:
      Used by:
      Created: 05.09.2013 14:16 KUSV    
     Modified: 05.09.2013 14:16 KUSV     <comment>
*/

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get dynqr}    /* Библиотека для работы с динамическими запросами. */
{navigate.def}
{flt-file.i}

DEFINE INPUT  PARAMETER iCustCat AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iCustId  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER mfrmname    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER mClass-Code AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER mClassCode  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE num-line   AS INT64      NO-UNDO. /* Необходимо для navigate.cqr */
DEFINE VARIABLE mdoc-num   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mdoc-date  AS DATE       NO-UNDO.
DEFINE VARIABLE minfodesc  aS CHARACTER  NO-UNDO.
DEFINE VARIABLE msource    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mIsDelete  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mSymbol    AS CHARACTER  NO-UNDO.

/* Хэндл временной таблицы TmpObj при фильтрации по ней */
DEFINE VARIABLE mHTmpObj    AS HANDLE    NO-UNDO.

ASSIGN 
   mSymbol = CHR(9)
   .
FORM
   msource
      VIEW-AS FILL-IN SIZE 10 BY 1
      FORMAT       "x(1000)"
      COLUMN-LABEL "Источник"
      HELP         "Источник"
   mdoc-num
      VIEW-AS FILL-IN SIZE 10 BY 1
      FORMAT       "x(100)"
      COLUMN-LABEL "Номер!документа"
      HELP         "Номер документа"
   mdoc-date
      COLUMN-LABEL "Дата!документа"
      HELP         "Дата документа"
      FORMAT "99/99/99"
  /* cust-ident.cust-code-type
      VIEW-AS FILL-IN SIZE 10 BY 1
      FORMAT       "x(1000)"
      COLUMN-LABEL "Вид!информации"
      HELP         "Вид информации"*/
   minfodesc
      VIEW-AS FILL-IN SIZE 15 BY 1
      FORMAT       "x(5000)"
      COLUMN-LABEL "Описание!информации"
      HELP         "Описание информации"
   cust-ident.open-date
      COLUMN-LABEL "Дата!получения"
      HELP         "Дата получения информации"
      FORMAT "99/99/99" 
   cust-ident.close-date
      COLUMN-LABEL "Дата!окончания"
      HELP         "Дата окончания действия"
      FORMAT "99/99/99" 
WITH FRAME browse1  TITLE  COLOR BRIGHT-WHITE mfrmname .

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
                      '"" AND cust-ident.class-code EQ ""' + mclasscode + '""' " 
}
     
{navigate.cqr
   &file       = "cust-ident"
   &files      = "cust-ident"
   &avfile     = "cust-ident"
   &maxfrm     = 1
   &bf1        = "msource mdoc-num mdoc-date cust-ident.cust-code-type minfodesc cust-ident.open-date cust-ident.close-date"
   &cf1        = "msource mdoc-num mdoc-date cust-ident.cust-code-type minfodesc cust-ident.open-date cust-ident.close-date"
   /* &help-label = " 'F1│F7 Поиск│Ins Добавить│F9│DEL Удалить' " */
   
   &edit       = "bis-tty.ef "
                     &class             = mClassCode
                     &before-run-method = "fininfo.bfe "
   &postfind   = "fininfo.fnd "                  
                    
   &look      = "bis-tty.nav "
   
   &rat_upclass = mClassCode
}

END. /* MAIN-BLOCK: */

RUN SetFltField IN THIS-PROCEDURE ("UseTmpObjInQuery",mHTmpObj).

{intrface.del}          /* Выгрузка инструментария. */ 
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='15/01/2015 07:52:43.374+04:00' */
/* $LINTFILE='fininfo.p' */
/*prosignLLRAdKTZ/6jt35FHFGg52w*/