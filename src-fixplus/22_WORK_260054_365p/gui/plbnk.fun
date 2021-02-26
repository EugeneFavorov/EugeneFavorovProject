
/* +++ plbnk.fun was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: plbnk.fun
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Функции бибилиотеки pp-plbnk.p
     Modified: 
*/
/*===============================================================================================*/
/*=== Выполнять запрос из внешней системы на дату ===============================================*/
FUNCTION RunExtQuery RETURNS LOG (INPUT iDate AS DATE):

DEF VAR vRes  AS  LOG   NO-UNDO INIT NO.

   IF     iDate       NE ?            /* Входящая дата не пуста                                  */
      AND mDateNR365p NE ?            /* Дата Начального решения определена                      */
      AND iDate       LE mDateNR365p  /* Выходящая дата меньше даты Начального решения           */
      AND mZaprosNO   EQ YES          /* Взведен признак "Запрос Налоговых Орагнов"              */
   THEN
      vRes = YES.

   RETURN vRes.

END FUNCTION. /* IsLessDateNR */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Проверка на существование счета во внешней системе ========================================*/
FUNCTION IsAcctEists RETURNS LOG (INPUT  iAcct AS  CHAR,
                                  OUTPUT oCur  AS  CHAR):

DEF BUFFER bExtAcct FOR ttExtAcct.

   /* Запускаем поиск счета во внешней таблице */
   RUN FindAcct IN THIS-PROCEDURE(iAcct, 
                                  OUTPUT TABLE bExtAcct) 
                                 NO-ERROR.

   FIND FIRST bExtAcct NO-LOCK NO-ERROR.
   oCur = IF AVAIL(bExtAcct) THEN fStrNvl(bExtAcct.currency, "")
                             ELSE "".

   RETURN AVAIL(bExtAcct).

END FUNCTION. /* IsAcctEists */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Преобразование даты в строку ==============================================================*/
FUNCTION date2str RETURNS CHAR PRIVATE (INPUT iDate AS DATE):

   /* Копия date2str из core365p.pro */
   RETURN ( IF iDate EQ ? THEN "" 
                         ELSE STRING(iDate, {&DATE-FORMAT-365P})).

END FUNCTION. /* date2str */

/*===============================================================================================*/

/* --- plbnk.fun was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am --- */
