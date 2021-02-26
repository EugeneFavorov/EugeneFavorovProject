/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ТОО "Банковские информационные системы"
     Filename: PP-TMESS.P
      Comment: Интерфейс системных сообщений
   Parameters:
         Uses: 
      Used by:
      Created: 21.08.2003 TSL
     Modified: 12.05.2005 14:41 KSV      (0044952) Добавлены переменные
                                         vAnswers и vAutoAnswer.
     Modified: 10.03.2006 Om Ошибка.
                        Неверное удаление описания процесса.
     Modified: 07.11.2006 19:39 Om       (0078824) Адаптирован под работу с
                                         Биссмарт
     Modified: 26.10.07 ler - 83715 Устранить ошибку run-time 4088 для АнализXL (d34)     
     Modified: 27.11.2007 18:50 KSV      (0085164) Для Биссмарт используется то
                                         же имя логфайла что и для Бисквит
     Modified: 24.04.2008 09:50 Om       <comment>

<tmess.pro>     

   Init-SysMes       Инициализация 
   Fill-SysMes       Создание и вывод сообщения
   Fill-ProgressErr     Сохраняет и выводит пргрессовые ошибки
   End-SysMes        Вывод протокола
     
*/     

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

{pptmess.def}           /* Переменные, используемые для службы.*/
{pptmess.fun}           /* Функции для работы службы. */
{tmess.pro}             /* Процедуры для работы службы. */
{pp-tmess-api-bs.i}     /* Процедуры для работы службы. */

/* Запускается при загрузки библиотеки. */
PROCEDURE StartInterface.
   DEF VAR vSCI AS CHAR   NO-UNDO. /* Значение SERVER-CONNECTION-ID. */
   /* Для APPL-SRV формируем имя файла.
   ** Commented by KSV: При запуске процедуры на WebSpeedServer атрибут
   ** SESSION:REMOTE будет NO, т.е. для Биссмарт этот код не выполняется. */
   IF SESSION:REMOTE 
   THEN DO:
      /* Примерный формат SESSION:SERVER-CONNECTION-ID
      ** 192.168.1.7::bq41d_bss::3097::352b2f8e7566887c:b1c5fa:10f4d15e185:-7fe1 */
      IF SESSION:REMOTE THEN vSCI  =  SESSION:SERVER-CONNECTION-ID.

      /* vSCI еще не известен в STARTUP процедуре. */
      vSCI  =  IF LENGTH (vSCI) GT 1
                  THEN ("(" + ENTRY (1, vSCI, ":") + ENTRY (NUM-ENTRIES (vSCI, ":"), vSCI, ":") + ")")
                  ELSE GetGUID ().
      ASSIGN
         mPrefLog =  USERID ("bisquit")   
         + vSCI
         vLogFile =  mPrefLog             + vLogFile
      .
   END.
   
   RETURN.
END PROCEDURE.
