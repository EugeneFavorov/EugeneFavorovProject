/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банко вские информационные системы"
     Filename: ext-findactex.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Поиск счета во внешней системе с учетом реквизитов клиента.
     Modified: 
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

DEF INPUT  PARAM iInn        AS  CHAR   NO-UNDO.   /* ИНН клиента                                */
DEF INPUT  PARAM iKpp        AS  CHAR   NO-UNDO.   /* КПП клиента                                */
DEF INPUT  PARAM iName       AS  CHAR   NO-UNDO.   /* Наименование клиента                       */
DEF INPUT  PARAM iAcct       AS  CHAR   NO-UNDO.   /* Номер счета                                */
DEF INPUT  PARAM iAllFilials AS  LOG    NO-UNDO.   /* По всем филиалам или только текущий        */ 
DEF OUTPUT PARAM TABLE       FOR ttExtAcct.        /* Таблица по найденным счетам                */

/*
   Если счет найден в старой системе, ищем его владельца в БИС, с учетом переданных ИНН, КПП
   Определение cust-cat, cust-id:
   1. cust-cat = "Ч", если клиент находится в таблице person
      cust-cat = "Ю", если клиент находится в таблице cust-corp
      cust-cat = "Б", если клиент находится в таблице banks
      cust-cat = "" (пусто), если клиент из старой системы не найден в БИС

   2. cust-id = person.person-id/cust-corp.cust-id/banks.bank-id - соответвенно
      cust-id = -1, если клиент не найден

   3. Одному номеру счета соответсвует одна запись в таблице ttExtAcct. Даже если в системе их 
      несколько, то клиенту принадлежит только один(или не принадлежит). Если счет принадлежит
      другому клиента, то без разницы какому именно.

   4. Процедура предусматривает два режима работы:
      1. Передается номер счета для проверки принадлежности его клинту
      2. Передается маска "*" для формирования всех счетов клиента, подлежащих отправки в НО.

   Оличие полей acct.number от acct.acct: 
      - acct.number не содержит признака филиальности, 
      - acct.acct содержит признак филиальности, после @
*/

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

IF NOT CONNECTED("bank")
  THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

RUN ext-findactex_.p( iInn, iKpp, iName, iAcct, iAllFilials,
    OUTPUT TABLE ttExtAcct).

END.
