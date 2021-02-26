/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ТОО "Банковские информационные системы"
     Filename: JLOAN_DPS.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 04.03.2007 15:35 OZMI    
     Modified: 04.03.2007 15:35 OZMI     (0070680)
*/

{pp-corr.p}
/******************************************************************************/
FUNCTION Get_Op-templ RETURNS INT64 (INPUT op_kind   AS CHARACTER,
                                       INPUT ccode     AS CHARACTER,
                                       INPUT beg-templ AS CHARACTER):
   DEFINE VARIABLE in-cod LIKE class.class-code NO-UNDO.

   DEFINE BUFFER buf-tmpl  FOR op-template.
   DEFINE BUFFER buf-kind  FOR op-kind.
   DEFINE BUFFER buf-class FOR class.

   FIND FIRST buf-kind WHERE buf-kind.op-kind EQ op_kind NO-LOCK NO-ERROR.
   IF AVAIL buf-kind THEN
      FOR EACH buf-tmpl OF buf-kind WHERE NOT CAN-DO(beg-templ,STRING(buf-tmpl.op-templ)) NO-LOCK:
         IF LOOKUP(ccode,GetXclassAllParentsEx(buf-tmpl.cr-class-code)) NE 0 THEN
            RETURN buf-tmpl.op-templ.
      END.
   RETURN ?.
END FUNCTION.

/******************************************************************************/
FUNCTION list-op-templ RETURNS CHARACTER (INPUT in-op-kind AS CHARACTER,
                                          INPUT in-code    AS CHARACTER):
   DEFINE VARIABLE lst-tmpl-op AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE in-templ    AS INT64     NO-UNDO.
   DEFINE VARIABLE tmp-templ   AS CHARACTER   NO-UNDO.
   
   DEF BUFFER buf-op-kind FOR op-kind.

   FIND FIRST buf-op-kind WHERE buf-op-kind.op-kind EQ in-op-kind NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-op-kind THEN RETURN ?.

   ASSIGN
      lst-tmpl-op = ""
      in-templ    = Get_Op-templ(buf-op-kind.op-kind,in-code,tmp-templ)
   .
   DO WHILE in-templ NE ?:
      {additem.i lst-tmpl-op STRING(in-templ)}
      {additem.i tmp-templ   STRING(in-templ)}
      in-templ =  Get_Op-templ(buf-op-kind.op-kind,in-code,tmp-templ).
   END.
   RETURN lst-tmpl-op.
END FUNCTION.

/******************************************************************************/
FUNCTION end_of_month RETURNS DATE (INPUT iBegDate AS DATE):
   RETURN date_correct(MONTH(iBegDate),0,31,YEAR(iBegDate)).
END FUNCTION.

/******************************************************************************/
FUNCTION end_of_quarter RETURNS DATE (INPUT iBegDate AS DATE):
   DEFINE VARIABLE vstr AS INT64     NO-UNDO.
   vstr = TRUNCATE((MONTH(iBegDate) - 1) / 3 + 1, 0).
   RETURN date_correct(03 * vstr,0,31, YEAR(iBegDate)).
END FUNCTION.

/******************************************************************************/
FUNCTION end_of_year RETURNS DATE (INPUT iBegDate AS DATE):
   RETURN DATE(12,31,YEAR(iBegDate)).
END FUNCTION.

/******************************************************************************/
FUNCTION end_of_halfyear RETURNS DATE (INPUT iBegDate AS DATE):
   DEFINE VARIABLE vstr AS INT64     NO-UNDO.
   vstr = TRUNCATE((MONTH(iBegDate) - 1) / 6 + 1, 0).
   RETURN date_correct(06 * vstr,0,31, YEAR(iBegDate)).
END FUNCTION.
/******************************************************************************/


FUNCTION half_year RETURNS DATE (INPUT iBegDate AS DATE,
                                 INPUT iDay     AS INT64,
                                 INPUT iMonth   AS INT64):
   CASE MONTH(iBegDate):
      WHEN 1 OR WHEN 2 OR WHEN 3 OR WHEN 4 OR WHEN 5 OR WHEN 6 THEN
         RETURN date_correct(06,iMonth,iDay,YEAR(iBegDate)).
      WHEN 7 OR WHEN 8 OR WHEN 9 OR WHEN 10 OR WHEN 11 OR WHEN 12 THEN
         RETURN date_correct(12,iMonth,iDay,YEAR(iBegDate)).
   END CASE.
END FUNCTION.

/******************************************************************************/
/*  NAME:    PROCEDURE MonthDate
    PURPOSE: Ищет дату следующего (относительно iOpDate) начисления 
             для варианта "Начисление раз в N месяцев М-го числа" 
             
    PARS:    iStartDate  - дата, от которой отсчитываем период
                           (например, дата открытия договора, или дата 
                           формирования условия)
             iMDate      - День месяца, в который по условию должно быть 
                           начисление
             iMonths     - Количество месяцев, через которые по условию 
                           должно быть начисление
    RETURNS: iOpDate     - Дата расчета.
             oChargeDate - дата следующего (после iOpDate) начисления
                           процентов (если возможно начисление в iOpDate -
                           вернется iOpDate)

    NOTES:   Предполагается, что iStartDate < iOpDate. Если не так - 
             не знаю, как это все будет работать.

             Возможно сработает, если они равны.

             Уточнить, как это должно работать, когда между 
             iStartDate и iOpDate не прошел ни один целый период.

             Сейчас вернется сам iStartDate.

*/

/******************************************************************************/
PROCEDURE MonthDate.
   DEFINE INPUT  PARAMETER iStartDate  AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iMDay       AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iMonths     AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iOpDate     AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oChargeDate AS DATE INIT ? NO-UNDO.
   
   DEFINE VARIABLE vMonthPeriod AS INT64 NO-UNDO.
   DEFINE VARIABLE vFullPeriod  AS INT64 NO-UNDO.
   DEFINE VARIABLE vFullYears   AS INT64 NO-UNDO.
   DEFINE VARIABLE vFullMonths  AS INT64 NO-UNDO.
   
   IF    iMonths LE 0 
      OR iMonths EQ ? THEN RETURN.

   /* Считаем разницу в месяцах между начальной и конечной датами */
   vMonthPeriod = (YEAR(iOpDate) - YEAR(iStartDate)) * 12 + (MONTH(iOpDate) - MONTH(iStartDate)).
   /* Если период получился <=нулевым (то есть, спрашиваем очередное начисление раньше начала вклада),
     значит мы считаем первое начисление -> и 1 период все-таки нужно прибавить */
   IF vMonthPeriod LE 0 THEN vMonthPeriod = iMonths.
   
   /* Теперь смотрим, сколько целых периодов укладывается в эту разницу и
      сразу пересчитываем, сколько месяцев укладывается в эти целые периоды... */
   vFullPeriod  = TRUNCATE(vMonthPeriod / iMonths, 0) * iMonths.
   /* ... и сколько осталось месяцев от последнего целого периода. */
   vMonthPeriod = vMonthPeriod - vFullPeriod. /* Эта переменная нам на хрен не нужна дальше */
   
   /* Прибавляем к начальной дате целое количество периодов 
      и сразу пытаемся выставить правильный день месяца */
   oChargeDate = date_correct(MONTH(iStartDate) + vFullPeriod,
                              0,
                              iMDay,
                              YEAR(iStartDate)).
   IF     oChargeDate GT iOpDate
      AND vFullPeriod / iMonths GT 1 THEN /* на 1 период меньше */
      oChargeDate = date_correct(MONTH(iStartDate) + vFullPeriod - iMonths,
                                 0,
                                 iMDay,
                                 YEAR(iStartDate)).

   /* Теперь в oChargeDate лежит последний???? ПРАВИЛЬНЫЙ день, меньше или равный iOpDate?????????
      Надо проверить, как это работает... */
   RETURN.
END PROCEDURE.

/******************************************************************************/
FUNCTION quarter RETURNS DATE (INPUT iBegDate AS DATE,
                               INPUT iDay     AS INT64,
                               INPUT iMonth   AS INT64):
   CASE MONTH(iBegDate):
      WHEN 1  OR WHEN 2  OR WHEN 3  THEN RETURN date_correct(03,iMonth,iDay,YEAR(iBegDate)).
      WHEN 4  OR WHEN 5  OR WHEN 6  THEN RETURN date_correct(06,iMonth,iDay,YEAR(iBegDate)).
      WHEN 7  OR WHEN 8  OR WHEN 9  THEN RETURN date_correct(09,iMonth,iDay,YEAR(iBegDate)).
      WHEN 10 OR WHEN 11 OR WHEN 12 THEN RETURN date_correct(12,iMonth,iDay,YEAR(iBegDate)).
   END CASE.
END FUNCTION.

/******************************************************************************/
/* обработка формулы  КМ[123] - начисление через заданное количество месяцев */
FUNCTION NumMonth RETURNS INT64 (INPUT iStr AS CHARACTER):
   DEFINE VARIABLE vlInd  AS INT64 NO-UNDO.
   DEFINE VARIABLE vrInd  AS INT64 NO-UNDO.
   DEFINE VARIABLE vOpNum AS INT64 NO-UNDO.

   ASSIGN
      vlInd = INDEX(iStr,"КМ[") + 3
      vrInd = INDEX(iStr,"]"  ) - 1
   .
   IF    vlInd EQ 0
      OR vrInd EQ 0
      OR vrInd LT vlInd THEN RETURN 0.

   vOpNum = INT64(SUBSTRING(iStr,4,vrInd - vlInd + 1)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN 0.
   RETURN vOpNum.
END FUNCTION.

/******************************************************************************/
/*  NAME:    FUNCTION get-end-date
    PURPOSE: По формуле iFormatStr определяет дату окончания периода, 
             начинающегося iDate.
             
             (Прибавляет время из iFormatStr к дате iDate) 
             
    PARS:    iDate      - дата, от которой отсчитывать время
                          (обычно дата открытия вклада)
             iFormatStr - строка, по которой определяется, какой период 
                          прибавить к дате iDate 
                          (обычно, строка продолжительности вклада)
             
    RETURNS: Дату окончания периода

    NOTES:   
*/
FUNCTION get-end-date RETURNS DATE (INPUT iDate      AS DATE,
                                    INPUT iFormatStr AS CHARACTER):
   DEFINE VARIABLE vi      AS INT64    NO-UNDO.
   DEFINE VARIABLE vStr    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vYears  AS INT64    NO-UNDO.
   DEFINE VARIABLE vMonths AS INT64    NO-UNDO.
   DEFINE VARIABLE vDays   AS INT64    NO-UNDO.
   DEFINE VARIABLE vDate   AS DATE       NO-UNDO.

   /* Бежим по строке */
   DO vi = 1 TO NUM-ENTRIES(iFormatStr,','):
      /* Берем кусочек, отделенный запятой */
      vStr = ENTRY(vi,iFormatStr,',').
      /* Если в нем нет двух параметров, то режем следующий */
      IF NUM-ENTRIES(vStr,'=') NE 2 THEN NEXT.

      CASE TRIM(ENTRY(1,vStr,'=')):
         WHEN 'Г' OR WHEN 'Y' THEN         /* Получаем год */
            vYears = vYears   + INT64(TRIM(ENTRY(2,vStr,'='))).
         WHEN 'М' OR WHEN 'M' THEN         /* Получаем месяц */
            vMonths = vMonths + INT64(TRIM(ENTRY(2,vStr,'='))).
         WHEN 'Д' OR WHEN 'D' THEN         /* Получаем день */
            vDays = vDays     + INT64(TRIM(ENTRY(2,vStr,'='))).
      END CASE.
   END.

   vDate = date_correct(MONTH(iDate),  vMonths,
                        DAY  (iDate), 
                        YEAR (iDate) + vYears).
   vDate = vDate + vDays.
   RETURN vDate.
END FUNCTION.
