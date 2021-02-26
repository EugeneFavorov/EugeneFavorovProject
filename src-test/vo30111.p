DEFINE INPUT PARAMETER in-op-date   LIKE op.op-date NO-UNDO.
DEFINE INPUT PARAMETER in-rec-kind  AS   RECID      NO-UNDO.

{globals.i}
{pick-val.i}
{wordwrap.def}
{intrface.get xclass}
{intrface.get cust}
{tt-ps.def}
{getdates.i}
def stream out_.
{setdest2.i &stream="stream out_" &filename="spool.tmp" &cols=160}


DEF VAR vReturnCode  AS INT64   NO-UNDO. /* Код возврата       */
DEF VAR bStr      AS CHAR  NO-UNDO. /* Строка для проверки с помощью РВ. */
DEF VAR VStr      AS CHAR  NO-UNDO. 
def var vOk  as logic.
put stream out_ unformatted " Протокол изменений  дополнительных реквизитов. Список изменённых документов."  skip.
put stream out_ unformatted " Документ номер:  Содержание: "  skip.
for each op-entry where op-entry.acct-cr begins "30111810"
                    and op-entry.op-date ge beg-date
                    and op-entry.op-date le end-date
                    and op-entry.filial-id eq shfilial
                    .
   find first op where op.op = op-entry.op no-lock no-error.
   RUN ChkDetails (op.details,OUTPUT vReturnCode).
   if vReturnCode <> 0 THEN do:
      bStr = "00034" + ",," + string(op-entry.amt-rub) + ",0,," + string(op-entry.amt-rub) + ",1,1,Д,".
      vOk =  UpdateSigns("op",STRING(op.op),"КодОпВал117",bStr,no).
      vStr = fill(" ",14 - length(trim(op.doc-num))).  
      PUT STREAM  out_  UNFORMATTED vStr .
      PUT STREAM  out_  trim(op.doc-num) " "  op.details  skip.

   end.

end.
PUT STREAM out_ unformatted "Дата  : " TODAY SKIP.
PUT STREAM out_ unformatted "Время : " STRING(TIME, "HH:MM:SS AM") SKIP(1).
{preview2.i &stream="stream out_" &filename=spool.tmp }




PROCEDURE ChkDetails:
   DEFINE INPUT  PARAMETER iStr        AS CHAR    NO-UNDO. /* Содержание документа. */
   DEFINE OUTPUT PARAMETER oRetCode    AS INT64 NO-UNDO.
   DEF VAR iDelim1      AS CHAR  NO-UNDO. 
   DEF VAR iDelim2      AS CHAR  NO-UNDO. 

   ASSIGN
      iDelim1  = "~{"
      iDelim2 = "~}"
   .

   DEF VAR vStr      AS CHAR  NO-UNDO CASE-SENSITIVE. 
   DEF VAR vResult   AS CHAR  NO-UNDO. 
   DEF VAR vErrMsg   AS CHAR  NO-UNDO. 
   DEF VAR vPSCode   AS CHAR  NO-UNDO. 
   DEF VAR vPS       AS CHAR  NO-UNDO. 
   DEF VAR i         AS int  NO-UNDO. 
   DEFINE VARIABLE vDate318p AS DATE    NO-UNDO. /* Дата поступления документа */
   oRetCode = 0.
   IF AVAILABLE(op) THEN
      IF op.ins-date NE ? THEN
         vDate318p = op.ins-date.
      ELSE
         vDate318p = op.op-date.
   ELSE
      vDate318p = TODAY.

   /* Номер должен быть перед текстовой частью документа.
   ** Номер должен быть указан в фигурных скобках. */
   IF    INDEX (iStr, iDelim1) EQ 0
      OR INDEX (iStr, iDelim2) EQ 0
   THEN DO:
      oRetCode = 1.
      RETURN.
   END.

   /* Получили строку для анализа. */
   ASSIGN
      vStr = SUBSTRING (iStr, INDEX (iStr, iDelim1) + 1)
      vStr = SUBSTRING (vStr, 1, INDEX (vStr, iDelim2) - 1)
   .
   IF INDEX(vStr,"PS") > 0 THEN ASSIGN
      vPSCode = SUBSTR(vStr,INDEX(vStr,"PS") + 2)
      vPS     = SUBSTR(vStr,INDEX(vStr,"PS"))
      vStr    = SUBSTR(vStr,1,INDEX(vStr,"PS") - 1)
   .

   /* Пробелов не должно быть. */
   IF INDEX (vStr, " ") NE 0
   THEN DO:
      oRetCode = 2.
      RETURN.
   END.

   /* Проверка корректности номера. */
   IF vPS NE "" THEN DO:
         oRetCode = 3.
         RETURN.
   END.

   IF substr(vStr,1,2) <> "VO"
   THEN DO:
      oRetCode = 31.
      RETURN.
   END.
   do i = 1 to 5.
      IF INDEX ("1234567890",substr(vStr, i + 2 ,1)) = 0
      THEN DO:
         oRetCode = 32.
         RETURN.
      END.
   end.
   vStr = SUBSTRING(vStr, 3, 5).
   IF GetCode("КодОпВал117", vStr) = ? THEN
   DO: 
      oRetCode = 4.
      RETURN.
   END.

   IF CAPS(GetCodeMisc("КодОпВал117", vStr,1)) EQ "ДА" THEN
   DO: 
      oRetCode = 41.
      RETURN.
   END.
   RETURN.
END PROCEDURE.
