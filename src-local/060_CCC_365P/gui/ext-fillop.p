/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-fillop.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Документы по счету.
     Modified: 
*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

FUNCTION REPL_VK CHAR(
  INPUT iStr AS CHAR):
  RETURN REPLACE(REPLACE( iStr,CHR(13),' '),CHR(10),' ').
END.

DEF INPUT  PARAM iHRepOp   AS  HANDLE NO-UNDO.   /* Указатель на таблицу для данных по операциям */
DEF INPUT  PARAM iBegDate  AS  DATE   NO-UNDO.   /* Дата начала выгрузки                         */ 
DEF INPUT  PARAM iEndDate  AS  DATE   NO-UNDO.   /* Дата окончания выгрузки                      */
DEF INPUT  PARAM iUpID     AS  INT64  NO-UNDO.   /* ID выгружаемого счета                        */
DEF INPUT  PARAM iAllFil   AS  LOG    NO-UNDO.   /* По всем филиалам или только текущий          */ 
DEF INPUT  PARAM TABLE     FOR ttExtAcct.        /* Таблица с данными по счету                   */
DEF OUTPUT PARAM oNumDocs  AS  INT64  NO-UNDO.   /* Количество документов                        */

/* Не смотря на то что идет передача таблицы, в ней всегда будет 1 счет */

{intrface.get strng}

DEFINE VARIABLE mTmpAcct   AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:
  
  FOR FIRST ttExtAcct NO-LOCK:
  
     DEFINE VARIABLE tthndl AS handle NO-UNDO.
     DEFINE VARIABLE res    AS INTEGER NO-UNDO.
     DEFINE VARIABLE bh AS HANDLE NO-UNDO.
     DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   
     CREATE TEMP-TABLE tthndl.
   
   	/*
		   POSTIME,
		   ACCOUNT,
		   DATE_PAY,
		   DOC_TYPE,
		   DEBIT,
		   CREDIT,
		   DOC_NUM,
		   DOC_DATE,
		   DETAILS,
		   ACC_RECIPIENT,
		   RECIPIENT,
		   BIC_BANK,
		   NAME_BANK,
		   INN_RECIPIENT,
		   CORR_ACC,
		   KPP_RECIPIENT
		*/
   
      mTmpAcct = DelFilFromAcct(ttExtAcct.number).
      
      IF SUBSTRING(mTmpAcct,6,3) EQ "840" THEN mTmpAcct = "USD" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "978" THEN mTmpAcct = "EUR" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "398" THEN mTmpAcct = "KZT" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "156" THEN mTmpAcct = "CNY" + mTmpAcct.
      IF SUBSTRING(mTmpAcct,6,3) EQ "826" THEN mTmpAcct = "GBP" + mTmpAcct.
   
      iEndDate = iEndDate + 1.
   
      RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
   
      res = PROC-STATUS
      ("SELECT * FROM BANKER.TOBIS365P3
	     WHERE TOBIS365P3.account = '" + mTmpAcct + "'
	     AND   TOBIS365P3.postime >= to_date('" + STRING(iBegDate,'99/99/9999') + "','dd/mm/yyyy')
	     AND   TOBIS365P3.postime < to_date('" + STRING(iEndDate,'99/99/9999') + "','dd/mm/yyyy')").
   
      bh = tthndl:DEFAULT-BUFFER-HANDLE.
      
      CREATE QUERY qh.
      qh:SET-BUFFERS(bh).
      qh:QUERY-PREPARE("for each " + bh:name ).
      qh:QUERY-OPEN.
      REPEAT:
         qh:GET-NEXT().
         if qh:QUERY-OFF-END THEN LEAVE.
   
         iHRepOp:BUFFER-CREATE().
         
         ASSIGN
             /* oNumDocs увеличиваем с каждым созданным документом на 1 */
             oNumDocs = oNumDocs + 1
   
             /* Обязательные системные поля */
             iHRepOp:BUFFER-FIELD(GetMangledName("ID")):BUFFER-VALUE       = STRING(oNumDocs)
          
             iHRepOp:BUFFER-FIELD(GetMangledName("UpId")):BUFFER-VALUE     = STRING(iUpID)
             /* Поля для выгрузки, имена соответсвуют тэгам файле */
             iHRepOp:BUFFER-FIELD(GetMangledName("ДатаОпер")):BUFFER-VALUE = REPLACE(STRING(bh:buffer-field("DATE_PAY"):buffer-value, '99.99.9999'), '/', '.')
             iHRepOp:BUFFER-FIELD(GetMangledName("ВидДок")):BUFFER-VALUE   = bh:buffer-field("DOC_TYPE"):buffer-value
             iHRepOp:BUFFER-FIELD(GetMangledName("НомДок")):BUFFER-VALUE   = bh:buffer-field("DOC_NUM"):buffer-value
             iHRepOp:BUFFER-FIELD(GetMangledName("ДатаДок")):BUFFER-VALUE  = bh:buffer-field("DOC_DATE"):buffer-value
             iHRepOp:BUFFER-FIELD(GetMangledName("НомКорСч")):BUFFER-VALUE = bh:buffer-field("CORR_ACC"):buffer-value /* номер кор.счета банка плательщика */
             iHRepOp:BUFFER-FIELD(GetMangledName("НаимБП")):BUFFER-VALUE   = REPL_VK( bh:buffer-field("NAME_BANK"):buffer-value)   /* наименование банка плательщика */
             iHRepOp:BUFFER-FIELD(GetMangledName("БИКБП")):BUFFER-VALUE    = bh:buffer-field("BIC_BANK"):buffer-value    /* БИК банка плательщика */
             iHRepOp:BUFFER-FIELD(GetMangledName("ИННПП")):BUFFER-VALUE    = bh:buffer-field("INN_RECIPIENT"):buffer-value    /* ИНН/КИО плательщика/получателя */
             iHRepOp:BUFFER-FIELD(GetMangledName("КПППП")):BUFFER-VALUE    = bh:buffer-field("KPP_RECIPIENT"):buffer-value    /* КПП плательщика/получателя */
             iHRepOp:BUFFER-FIELD(GetMangledName("НаимПП")):BUFFER-VALUE   = REPL_VK( bh:buffer-field("RECIPIENT"):buffer-value)   /* наименование плательщика/получателя */
             iHRepOp:BUFFER-FIELD(GetMangledName("НомСчПП")):BUFFER-VALUE  = REPL_VK( bh:buffer-field("ACC_RECIPIENT"):buffer-value)  /* счет плательщика/получателя */
             iHRepOp:BUFFER-FIELD(GetMangledName("Дебет")):BUFFER-VALUE    = DEC(bh:buffer-field("DEBIT"):buffer-value)
             iHRepOp:BUFFER-FIELD(GetMangledName("Кредит")):BUFFER-VALUE   = DEC(bh:buffer-field("CREDIT"):buffer-value)
             iHRepOp:BUFFER-FIELD(GetMangledName("НазнПл")):BUFFER-VALUE   = REPL_VK( bh:buffer-field("DETAILS"):buffer-value).
             
             RUN dbgprint.p ("ext-fillop","~n" +
                   "DATE_PAY: " + GetNullStr(STRING(bh:buffer-field("DATE_PAY"):buffer-value, '99.99.9999')) + "~n" +
                   "DOC_NUM : " + GetNullStr(bh:buffer-field("DOC_NUM"):buffer-value) + "~n" +
                   "DEBIT   : " + GetNullStr(bh:buffer-field("DEBIT"):buffer-value) + "~n" +
                   "CREDIT  : " + GetNullStr(bh:buffer-field("CREDIT"):buffer-value)
                  ).
             
         /* Записываем запись и освобождаем ее */
         iHRepOp:BUFFER-RELEASE().
      END.
   END. /* FOR FIRST ttExtAcct NO-LOCK */
   
   CATCH eAnyError AS Progress.Lang.Error:
      message RETURN-VALUE + " " + eAnyError:GetMessage(1) view-as alert-box.
      RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
   END CATCH.
END.
