/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ЗАО "Банковские информационные системы"
     Filename: cust-chk.p
      Comment: Метод валидации ЮЛ.
   Parameters:
         Uses:
      Used by:
      Created: 
     Modified: 15/06/2006 ilvi (0062863) Валидация обязательных реквизитов в зависимости от роли.
*/

DEF INPUT PARAM in-rec AS RECID NO-UNDO.

DEF VAR unkg      AS CHAR NO-UNDO.
DEF VAR regn      AS CHAR NO-UNDO.
DEF VAR mMsgErr   AS CHAR NO-UNDO. /* Сообщение об ошибке. */

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get cust}     /* Библиотека для работы с клиентами. */

MAIN_BLOCK:
DO
ON ERROR  UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK
ON ENDKEY UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK:

   FIND FIRST cust-corp WHERE
       RECID (cust-corp) EQ in-rec NO-LOCK NO-ERROR.
   IF NOT AVAIL cust-corp
   THEN DO:
      mMsgErr = "Error".
      LEAVE MAIN_BLOCK.
   END.
   
   regn = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"RegNum",?).

   RUN chk-inn.p(string(cust-corp.cust-id), regn) NO-ERROR. /* Ищем, нет ли  где записи с такими же реквизитами */
   
   IF ERROR-STATUS:ERROR THEN DO: /* Найдена совпадающая по реквизитам запись */
      RUN Fill-SysMes IN h_tmess ("", "", "-1",RETURN-VALUE).
      mMsgErr = "Error".
      LEAVE MAIN_BLOCK.
   END.

   {lg7001cl.i
      &in-class  = 'cust-corp'
      &surrogate = STRING(cust-corp.cust-id)
      &cl_name1  = "cust-corp.name-corp"
      &cl_name2  = "cust-corp.name-short"
   }  


   /* Проверка заполнение обязательных реквизитов
   ** в зависимости от роли субъекта. */
   RUN ChkFieldManByRole IN h_cust ("cust-corp",(BUFFER cust-corp:HANDLE)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      mMsgErr = "Error".
      LEAVE MAIN_BLOCK.
   END.
   /* Если клиент создаем УНКг, иначе удаляем */
   IF IsSubjClient("Ю",cust-corp.cust-id) THEN
      unkg = UNKg("cust-corp",STRING(cust-corp.cust-id)).
   ELSE
      UpdateSigns("cust-corp",STRING(cust-corp.cust-id),'УНКг',?,?).

END.
RUN RunClassMethod IN h_xclass ('cust-corp',
                                "ExtraChkUpd",
                                "",
                                "",
                                ?,
                                STRING(cust-corp.cust-id)).
{intrface.del}          /* Выгрузка инструментария. */ 
IF mMsgErr NE ""
   THEN RETURN ERROR mMsgErr.
RETURN.

