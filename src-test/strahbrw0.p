/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: strahbrw0.P
      Comment: Получатели страховых премий - запуск браузера
   Parameters:
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/
{globals.i}             /* Глобальные переменные сессии. */
{intrface.get tmess}    /* Инструменты обработки сообщений. */

DEFINE INPUT PARAMETER iLevel AS INT64 NO-UNDO.

RUN strahbrw.p("strahpol","strahpol","Получатели страховой премии",iLevel).
