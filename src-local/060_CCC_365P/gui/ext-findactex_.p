/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-findactex_.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Поиск счета во внешней системе с учетом реквизитов клиента.
     Modified: 
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

DEF INPUT  PARAM iInn        AS  CHAR   NO-UNDO.   /* ИНН клиента                                */
DEF INPUT  PARAM iKpp        AS  CHAR   NO-UNDO.   /* КПП клиента                                */
DEF INPUT  PARAM iName       AS  CHAR   NO-UNDO.   /* Наименование клиента                       */
DEF INPUT  PARAM iAcct       AS  CHAR   NO-UNDO.   /* Номер счета                                */
DEF INPUT  PARAM iAllFilials AS  LOG    NO-UNDO.   /* По всем филиалам или только текущий        */ 
DEF OUTPUT PARAM TABLE       FOR ttExtAcct.        /* Таблица по найденным счетам                */

DEFINE VARIABLE tthndl AS handle NO-UNDO.
DEFINE VARIABLE res    AS INTEGER NO-UNDO.
DEFINE VARIABLE bh AS HANDLE NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE mCID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCIDIP AS CHARACTER NO-UNDO.

/*
   Если счет найден в старой системе, ищем его владельца в БИС, с учетом переданных ИНН, КПП
   Определение cust-cat, cust-id:
   1. cust-cat = "Ч", если клиент находится в таблице person
      cust-cat = "Ю", если клиент находится в таблице cust-corp
      cust-cat = "Б", если клиент находится в таблице banks
      cust-cat = "" (пусто), если клиент из старой системы не найден в БИС

   2. cust-id = person.person-id/cust-corp.cust-id/banks.bank-id - соответственно
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

IF shFilial EQ '0000' OR shFilial EQ '0300' OR shFilial EQ '0500' THEN
DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

/*MESSAGE "ext-findactex_.p: shFilial = " shFilial*/
/*VIEW-AS ALERT-BOX.                              */

   IF (NOT CONNECTED("bank")) OR (NOT CONNECTED("bismfr"))
      THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

   IF iAcct EQ "*" THEN
   DO:
      IF     {assigned iInn} 
         AND LENGTH(iInn) EQ 10 THEN
      DO:
         FIND FIRST cust-corp WHERE
            cust-corp.inn EQ iInn
         NO-LOCK NO-ERROR.
         IF AVAIL(cust-corp) THEN
         ASSIGN
            mCID = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"CID","").
      END.
      ELSE IF {assigned iInn} 
         AND  LENGTH(iInn) EQ 12 THEN
      DO:
         FIND FIRST person WHERE
            person.inn EQ iInn
         NO-LOCK NO-ERROR.
         IF AVAIL(person) THEN
         ASSIGN
            mCID   = GetXAttrValueEx("person",STRING(person.person-id),"CID","")
            mCIDIP = GetXAttrValueEx("person",STRING(person.person-id),"CIDIP","").
         IF {assigned mCID}
            AND {assigned mCIDIP} 
         THEN mCID = mCID + "," + mCIDIP.
      END.
/*MESSAGE "ext-findactex_.p: mCID = " mCID*/
/*VIEW-AS ALERT-BOX.                      */
      IF {assigned mCID} THEN
      DO:
      	{empty ttExtAcct}      
         
         CREATE TEMP-TABLE tthndl.
         RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
         res = PROC-STATUS
            (
            "select * from BANKER.TOBIS365PCLOSED
			   where CID in (" + mCID + ")"
            ).

         bh = tthndl:DEFAULT-BUFFER-HANDLE.
         CREATE QUERY qh.
         qh:SET-BUFFERS(bh).
         qh:QUERY-PREPARE("for each " + bh:name).
         qh:QUERY-OPEN.
         REPEAT:
            qh:GET-NEXT().
            IF qh:QUERY-OFF-END THEN LEAVE.
/*MESSAGE "ext-findactex_.p: 1 buffer-field(account) = " bh:buffer-field("account"):buffer-value*/
/*VIEW-AS ALERT-BOX.                                                                            */
            RUN ext-findact.p(bh:buffer-field("account"):buffer-value,
                              iAllFilials,
                              INPUT-OUTPUT TABLE ttExtAcct).
         END.
      END.
      ELSE
      DO:
      	{empty ttExtAcct}
         
         CREATE TEMP-TABLE tthndl.
         RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
         res = PROC-STATUS 
            (
            "select * from BANKER.TOBIS365PCLOSED t 
		       where t.inn = '" + iInn + "'"
            ).
      
         bh = tthndl:DEFAULT-BUFFER-HANDLE.
         CREATE QUERY qh.
         qh:SET-BUFFERS(bh).
         qh:QUERY-PREPARE("for each " + bh:name).
         qh:QUERY-OPEN.
         REPEAT:
            qh:GET-NEXT().
/*MESSAGE "ext-findactex_.p: 2 buffer-field(account) = " bh:buffer-field("account"):buffer-value*/
/*VIEW-AS ALERT-BOX.                                                                            */
            IF qh:QUERY-OFF-END THEN LEAVE.
            RUN ext-findact.p(bh:buffer-field("account"):buffer-value,
                              iAllFilials,
                              INPUT-OUTPUT TABLE ttExtAcct).
         END.
      END.
   END.
   ELSE
   DO:
      /* Поиск счета клиента */
      RUN ext-findact.p(iAcct,iAllFilials,INPUT-OUTPUT TABLE ttExtAcct).
   END.
END.

/*MESSAGE "ext-findactex_.p: before For Each ttExtAcct"*/
/*VIEW-AS ALERT-BOX.                                   */

/*For Each ttExtAcct No-Lock:                                                        */
/*   MESSAGE "ext-findactex_.p: For Each ttExtAcct: ttExtAcct.acct = " ttExtAcct.acct*/
/*   VIEW-AS ALERT-BOX.                                                              */
/*End.                                                                               */

CATCH eAnyError AS Progress.Lang.Error:
   message RETURN-VALUE + " " + eAnyError:GetMessage(1) view-as alert-box.

   RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
END CATCH.


