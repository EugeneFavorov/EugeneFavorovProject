{globals.i}
{intrface.get tmess}

/* +++ ext-findact.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-fillop.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Поиск счета во внешней системе.
     Modified: 
*/
{globals.i}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

DEF INPUT  PARAM iAcct       AS  CHAR   NO-UNDO.    /* Номер счета                               */
DEF INPUT  PARAM iAllFilials AS  LOG    NO-UNDO.    /* По всем филиалам или только текущий       */ 
DEF OUTPUT PARAM TABLE       FOR ttExtAcct.         /* Таблица по найденным счетам               */

/*
   Если счет найден в старой системе, ищем его владельца в БИС
   Определение cust-cat, cust-id:
   1. cust-cat = "Ч", если клиент находится в таблице person
      cust-cat = "Ю", если клиент находится в таблице cust-corp
      cust-cat = "Б", если клиент находится в таблице banks
      cust-cat = "" (пусто), если клиент из старой системы не найден в БИС

   2. cust-id = person.person-id/cust-corp.cust-id/banks.bank-id - соответвенно
      cust-id = -1, если клиент не найден

   3. Если найденно несколько счетов, то для каждого создать свою запись в таблице ttExtAcct
*/

/* Находим запись в таблице acct, что бы не заполнять все поля вручную */
FIND FIRST acct WHERE acct.acct BEGINS "40702810050010146608"
                NO-LOCK NO-ERROR.

IF AVAIL(acct) THEN 
DO:
   /* Создаем временную таблицу */
   CREATE ttExtAcct.

   /* Копируем данные из таблицы */
   BUFFER-COPY acct TO ttExtAcct.

   /* Ряд полей переопределим */
   ASSIGN
     ttExtAcct.cust-cat = "Ю" 
     ttExtAcct.cust-id  = 212  /* Номер тестового клиента в базе */
     ttExtAcct.acct     = iAcct
     ttExtAcct.number   = iAcct

     ttExtAcct.open-date  = DATE("01.01.2000")
     ttExtAcct.close-date = DATE("01.11.2015")

   .
   /* Сохраняем изменения в таблицу */
   VALIDATE ttExtAcct NO-ERROR.

END. /* IF AVAIL(acct) THEN  */

RETURN.
/* --- ext-findact.p was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:11am --- */
