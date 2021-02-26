/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-acctpos.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Обороты/остатки по счету.
     Modified: 
*/
{globals.i}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

DEF INPUT  PARAM iBegDate  AS  DATE  NO-UNDO.   /* Дата начала выгрузки                          */
DEF INPUT  PARAM iEndDate  AS  DATE  NO-UNDO.   /* Дата окончания выгрузки                       */
DEF INPUT  PARAM iAllFil   AS  LOG   NO-UNDO.   /* По всем филиалам или только текущий           */
DEF INPUT  PARAM TABLE     FOR ttExtAcct.       /* Таблица по найденным счетам                   */
DEF OUTPUT PARAM oAmtIn    AS  DEC   NO-UNDO.   /* Входящий остаток                              */
DEF OUTPUT PARAM oAmbDb    AS  DEC   NO-UNDO.   /* Обороты по дебету                             */
DEF OUTPUT PARAM oAmtCr    AS  DEC   NO-UNDO.   /* Обороты по кредиту                            */
DEF OUTPUT PARAM oAmt      AS  DEC   NO-UNDO.   /* Исходящий остаток                             */

/* Возвращаем фиктивные остатки по любому счету */
FOR FIRST ttExtAcct NO-LOCK:

   ASSIGN
      oAmtIn = 123.44
      oAmbDb = 1.11
      oAmtCr = 2.22
      oAmt   = 567.88
   .

END. /* FOR FIRST ttExtAcct NO-LOCK: */