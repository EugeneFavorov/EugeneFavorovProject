/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-acctpos_.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Обороты/остатки по счету.
     Modified: 
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

/*{sh-defs.i}*/
/*{intrface.get count}*/
/*{intrface.get db2l}   */
/*{intrface.get instrum}*/
{intrface.get op}
/*{intrface.get strng}   */
/*{intrface.get separate}*/

DEF INPUT  PARAM iBegDate  AS  DATE  NO-UNDO.   /* Дата начала выгрузки                          */
DEF INPUT  PARAM iEndDate  AS  DATE  NO-UNDO.   /* Дата окончания выгрузки                       */
DEF INPUT  PARAM iAllFil   AS  LOG   NO-UNDO.   /* По всем филиалам или только текущий           */
DEF INPUT  PARAM TABLE     FOR ttExtAcct.       /* Таблица по найденным счетам                   */
DEF OUTPUT PARAM oAmtIn    AS  DEC   NO-UNDO.   /* Входящий остаток                              */
DEF OUTPUT PARAM oAmbDb    AS  DEC   NO-UNDO.   /* Обороты по дебету                             */
DEF OUTPUT PARAM oAmtCr    AS  DEC   NO-UNDO.   /* Обороты по кредиту                            */
DEF OUTPUT PARAM oAmt      AS  DEC   NO-UNDO.   /* Исходящий остаток                             */

DEFINE VARIABLE mTmpAcct   AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

   FOR FIRST ttExtAcct NO-LOCK:
      DEFINE VARIABLE tthndl AS handle NO-UNDO.
      DEFINE VARIABLE res    AS INTEGER NO-UNDO.
      DEFINE VARIABLE bh AS HANDLE NO-UNDO.
      DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   
      CREATE TEMP-TABLE tthndl.
   
      mTmpAcct = DelFilFromAcct(ttExtAcct.number).
      
      IF SUBSTRING(mTmpAcct,6,3) EQ "840" THEN mTmpAcct = "USD" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "978" THEN mTmpAcct = "EUR" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "398" THEN mTmpAcct = "KZT" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "156" THEN mTmpAcct = "CNY" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "826" THEN mTmpAcct = "GBP" + mTmpAcct.

      RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
      res = PROC-STATUS 
      (
           "select * 
	        from BANKER.TOBIS365P2 t
	        where t.account  = '" + mTmpAcct + "'
	          and t.daystart = to_date('" + STRING(iBegDate,'99/99/9999') + "','dd/mm/yyyy')
	          AND t.dayend   = to_date('" + STRING(iEndDate,'99/99/9999') + "','dd/mm/yyyy')"  
      ).
      
      bh = tthndl:DEFAULT-BUFFER-HANDLE. 
      CREATE QUERY qh.
      qh:SET-BUFFERS(bh).
      qh:QUERY-PREPARE("for each " + bh:name).
      qh:QUERY-OPEN.
      REPEAT:
         qh:GET-NEXT().
         if qh:QUERY-OFF-END THEN LEAVE.
         ASSIGN
           oAmtIn = bh:buffer-field("rest_in"):buffer-value
           oAmbDb = bh:buffer-field("debit"):buffer-value
           oAmtCr = bh:buffer-field("credit"):buffer-value
           oAmt   = bh:buffer-field("rest_out"):buffer-value
           .
      END.
   END. /* FOR FIRST ttExtAcct NO-LOCK: */

   RUN dbgprint.p ("ext-acctpos_","~n" +
                   "mTmpAcct: " + GetNullStr(mTmpAcct) + "~n" +
                   "iBegDate: " + GetNullStr(STRING(iBegDate,'99/99/9999')) + "~n" +
                   "iEndDate: " + GetNullStr(STRING(iEndDate,'99/99/9999')) + "~n" +
                   "oAmtIn  : " + GetNullStr(STRING(oAmtIn)) + "~n" +
                   "oAmbDb  : " + GetNullStr(STRING(oAmbDb)) + "~n" +
                   "oAmtCr  : " + GetNullStr(STRING(oAmtCr)) + "~n" +
                   "oAmt    : " + GetNullStr(STRING(oAmt))
                  ).

   CATCH eAnyError AS Progress.Lang.Error:
      message RETURN-VALUE + " " + eAnyError:GetMessage(1) view-as alert-box.
      RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
   END CATCH.
END.

RETURN.
