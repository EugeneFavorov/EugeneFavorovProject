/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: l-gar-joi.p
      Comment: Join меню для класса term-obl-gar
   Parameters:
         Uses:
      Used by:
      Created: 21.06.2009 15:50 Jadv    
     Modified:
*/
{joinpar.i}
{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get db2l}     /* Инструмент для динамической работы с БД. */

DEF VAR mSurr     AS CHAR NO-UNDO.

FIND FIRST term-obl WHERE 
     ROWID(term-obl) EQ TO-ROWID(iRowId) 
NO-LOCK NO-ERROR.
IF NOT AVAIL term-obl THEN 
   RETURN "-1".
FIND FIRST loan WHERE 
           loan.contract  EQ term-obl.contract
   AND     loan.cont-code EQ term-obl.cont-code 
NO-LOCK NO-ERROR.
IF NOT AVAIL loan 
   THEN RETURN "-1".

mSurr = term-obl.contract         + "," +
        term-obl.cont-code        + "," +
        STRING(term-obl.idnt)     + "," +
        STRING(term-obl.end-date) + "," + 
        STRING(term-obl.nn).

RUN CreateJoin("Коэффициент остаточной стоимости", 
               "ob-rate`" + 
               STRING(RECID(term-obl)) + "," + STRING(level + 1),
               YES).


IF term-obl.fop-offbal EQ 2 THEN
   RUN CreateJoin("Котировки финансового инструмента", 
                  "ob-curs`" + 
                  STRING(RECID(term-obl)) + "," + STRING(level + 1), 
                  YES). 
ELSE
   RUN CreateJoin("Индивидуальная стоимость", 
                  "ob-price`" + 
                  STRING(RECID(term-obl)) + "," + STRING(level + 1),
                  YES).

RUN CreateJoinLd("Категория качества",
                 "browseld",
                 "КачОбеспеч",
                 "contract"  + CHR(1) + 
                 "cont-code" + CHR(1) + 
                 "idnt"      + CHR(1) + 
                 "end-date"  + CHR(1) + 
                 "nn",
                 term-obl.contract         + CHR(1) + 
                 term-obl.cont-code        + CHR(1) + 
                 STRING(term-obl.idnt)     + CHR(1) + 
                 STRING(term-obl.end-date) + CHR(1) + 
                 STRING(term-obl.nn),
                 "",
                 STRING(level + 1), 
                 YES).

RUN CreateJoin("Дополнительные реквизиты", 
               "toblsign`" +
               STRING(RECID(term-obl)) + "," + STRING(level + 1),
               YES).

RUN CreateJoin("Учет ПТС", 
               "ptsregbrw`" +
               STRING(RECID(term-obl))+ "," + STRING(level + 1),
               YES).               

RUN CreateJoin("Сообщения экспорта-импорта",
               "ob-pck`" + 
               STRING(RECID(term-obl)) + "," + STRING(level + 1),
               YES).

   /* Инклюд формирования Join меню */
{procjoin.i
    &Prefix     = "term-obl"
    &frametitle = "'[ ОБЕСПЕЧЕНИЕ ДОГОВОРА ]'" 
    &parms      = "(RECID(term-obl),level + 1)"
}

IF RETURN-VALUE NE "" THEN 
   RETURN RETURN-VALUE.
ELSE
   RETURN "0".
