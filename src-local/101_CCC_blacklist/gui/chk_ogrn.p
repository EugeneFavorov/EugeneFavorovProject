/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-2017 АО "Банковские информационные системы"
     Filename:  chk_ogrn.p
      Comment:  Метод update на реквизите ОГРН на физ. лицах 
   Parameters:  Суррогат клиента,Значение редактируемого ДР
         Uses:  
      Used BY:
      Created:  26.10.17 KSBIS
     Modified:
*/

DEF INPUT  PARAM iN-pers-id AS CHAR   NO-UNDO.   /* Суррогат клиента */
DEF INPUT  PARAM iN-param   AS CHAR   NO-UNDO.   /* Значение редактируемого ДР */

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get tmess}
{intrface.get strng}      /* Инструменты для работы со строками  */


DEFINE VARIABLE mPredpr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mProvOGRN AS CHARACTER NO-UNDO.

mProvOGRN = FGetSetting("Проверки","ЖестПровОГРН","").

IF mProvOGRN <> "Да" THEN
   RETURN.

FIND FIRST person WHERE person.person-id = INT64(iN-pers-id) NO-LOCK NO-ERROR.
mPredpr = GetXAttrValueEx("person", STRING(person.person-id), "Предпр", "").

IF mPredpr = "Предпр" AND LENGTH(in-param) <> 15 THEN 
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "0", "Внимание! Формат ОГРН для предпринимателя 15 символов").
   RETURN ERROR.
END.

