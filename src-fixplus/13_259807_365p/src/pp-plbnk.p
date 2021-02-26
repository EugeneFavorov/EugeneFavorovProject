/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pp-plbnk.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Библиотека для обмена с внешней системой
     Modified: 
*/
{globals.i}

{intrface.get itax}
{intrface.get strng}
{intrface.get xclass}

/* Объявяем общие переменные для всего обмена */
{extexch.def}
{365p.def}

DEF VAR mDateNR365p  AS  DATE  NO-UNDO.         /* Дата начального решения                       */
DEF VAR mZaprosNO    AS  LOG   NO-UNDO INIT NO. /* Запрос требует данных из внешней системы      */
DEF VAR mAllFil      AS  LOG   NO-UNDO INIT NO. /* Поиск счетов идет во всех филиалах            */

DEF VAR mAcctFnd     AS  CHAR  NO-UNDO.         /* Поиск счета по номеру                         */
DEF VAR mAcctFndEx   AS  CHAR  NO-UNDO.         /* Поиск счета с учетом рекв.клиента             */
DEF VAR mAcctPos     AS  CHAR  NO-UNDO.         /* Остатки/обороты по счету                      */
DEF VAR mFillOp      AS  CHAR  NO-UNDO.         /* Операции по счету                             */

/* Функции и процедуру библиотеки */
{plbnk.fun}
{plbnk.pro}

ASSIGN
   mAcctFnd    = fGetSetting("Настройка_365П", "ПроцСчПоиск", "")
   mAcctFndEx  = fGetSetting("Настройка_365П", "ПроцСчПоискКли", "")
   mAcctPos    = fGetSetting("Настройка_365П", "ПроцСчет", "")
   mFillOp     = fGetSetting("Настройка_365П", "ПроцОпер", "")
   mDateNR365p = DATE(fGetSetting("Дата_НР", "", "")) 
NO-ERROR.