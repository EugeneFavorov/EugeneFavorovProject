/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: t-obl-joi.p
      Comment: Join меню для классов term-obl 
   Parameters: нет
         Uses:
      Used by:
      Created: 19.05.2009 11:11 Jadv    
     Modified: 
*/
{joinpar.i}
{globals.i}

{flt-file.i} /* Определение структуры динамического фильтра */

DEF VAR mTitle AS CHAR NO-UNDO INIT "[ ВЫБЕРИТЕ ]".   /* Заголовок меню */

FIND FIRST term-obl WHERE 
     ROWID(term-obl) EQ TO-ROWID(iRowId) 
NO-LOCK NO-ERROR.

FIND FIRST loan-cond 
     WHERE loan-cond.contract EQ term-obl.contract
       AND loan-cond.cont-code EQ term-obl.cont-code
NO-LOCK NO-ERROR.

IF NOT AVAIL term-obl THEN 
   RETURN "-1".

IF iClass EQ "term-obl-debt" THEN
   RUN CreateJoin ("Пролонгация",
                   "p-obl(l)`" + 
                   STRING(RECID(term-obl)) + "," + STRING(level + 1),
                   YES).

IF iClass EQ "term-obl-agrm" THEN
DO:
   mTitle = "[ ОФИЦИАЛЬНАЯ ИНФОРМАЦИИЯ ]".
   RUN CreateJoinLd ("Соообщения экспорта/импорта", 
                     "browseld",
                     "PackObject",
                     "file-name" + CHR(1) + "surrogate",
                     "term-obl" + CHR(1) + term-obl.contract         + CHR(2) +
                                           term-obl.cont-code        + CHR(2) +
                                           STRING(term-obl.idnt)     + CHR(2) +
                                           STRING(term-obl.end-date) + CHR(2) +
                                           STRING(term-obl.nn),
                     "file-name" + CHR(1) + "surrogate",
                     level + 1,
                     YES).
END.

IF iClass EQ "term-obl-dspt" THEN
DO:
   mTitle = "[ СПОРЫ ]".
   RUN CreateJoinLd ("Соообщения экспорта/импорта", 
                     "browseld",
                     "PackObject",
                     "file-name" + CHR(1) + "surrogate",
                     "term-obl" + CHR(1) + term-obl.contract         + CHR(2) +
                                           term-obl.cont-code        + CHR(2) +
                                           STRING(term-obl.idnt)     + CHR(2) +
                                           STRING(term-obl.end-date) + CHR(2) +
                                           STRING(term-obl.nn),
                     "file-name" + CHR(1) + "surrogate",
                     level + 1,
                     YES).
END.

IF iClass EQ "presched" THEN
DO:
   RUN CreateJoin ("Дополнительные реквизиты", 
                   "t-obl-sgn`" + 
                   STRING(RECID(term-obl)) + "," + STRING(level + 1),
                   YES).
   RUN CreateJoinLd ("Предварительные плановые остатки", 
                     "browseld",
                     "term-obl-sum-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "302",
                     "",
                     STRING(level + 1),
                     YES).
   RUN CreateJoinLd ("Предварительные плановые погашения ссуды", 
                     "browseld",
                     "term-obl-debt-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "303",
                     "",
                     STRING(level + 1),
                     YES).
   RUN CreateJoinLd ("Предварительные плановые платежи %%", 
                     "browseld",
                     "term-obl-per-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "301",
                     "",
                     STRING(level + 1),
                     YES).
   RUN CreateJoinLd ("Предварительный график комиссий", 
                     "browseld",
                     "term-obl-comm-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "310",
                     "",
                     STRING(level + 1),
                     YES).

END.

   /* Join-меню раскрывать только для указанных классов */
IF CAN-DO("term-obl-debt,term-obl-agrm,term-obl-dspt,presched", iClass) THEN
DO:
      /* Инклюд формирования Join меню */
   {procjoin.i
       &Prefix     = "term-obl"
       &frametitle = mTitle
       &parms      = "(recid(term-obl),level + 1)"
       &showsingle = "YES"
   }
END.
RETURN "0".
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:56:56.035+04:00' */
/* $LINTFILE='t-obl-joi.p' */
/*prosignoEYKHL8hlv2rktEuzJYetQ*/