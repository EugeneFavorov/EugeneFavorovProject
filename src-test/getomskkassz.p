/*
               Банковская интегрированная система БИСквит
    Copyright: 
     Filename: getomskkassz.p
      Comment: Загружаем заготовки кассовых заявок
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/
DEFINE VARIABLE mResult AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOK     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mErrMsg AS CHARACTER NO-UNDO.

{topkind.def}
{empty tOpKindParams} /* очистить таблицу параметров */

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
   IF NOT CONNECTED("bank")
   THEN   CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
   IF NOT CONNECTED("bank") THEN 
   DO:
      MESSAGE "Нет соединения c bank".
   END.
   ELSE
   DO:
      RUN ex-trans.p ("load-kassz",
                TODAY,
                TABLE tOpKindParams,
                OUTPUT mOK,
                OUTPUT mErrMsg).
      /*RUN getomskkassz2.p.*/
   END.

   FINALLY:
      IF CONNECTED("bank")   THEN DISCONNECT bank.
      IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
   END FINALLY.
END.

RETURN.
