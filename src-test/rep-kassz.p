/*
*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}

/*DEFINE INPUT PARAMETER iNum AS INT64     NO-UNDO.*/

DEFINE VARIABLE mFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPredpr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSubject    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBegDate    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSumm       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mPost       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mPostStr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mName       AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mINN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCliName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mItogo      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mDate       AS CHARACTER NO-UNDO.

DEFINE STREAM out-stream.

DEFINE TEMP-TABLE tt-otch
   FIELD fio          AS CHAR
   FIELD datez        AS CHAR
   FIELD acct         AS CHAR 
   FIELD amt          AS DECIMAL 
   FIELD symbol       AS CHAR
   FIELD user-id      AS CHAR.

{empty tt-otch}

{getdate.i}

mFileName = "./" + 
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-kassz.xml".
OUTPUT STREAM out-stream TO VALUE(mFileName) 
       UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

/*
Заявка на выдачу наличности в рублях на 08.11.2016г.	 	 	 	 	 
№	Дата	Наименование клиента	Счет	Сумма, руб.	Кассовый символ	Исполнитель
*/


MESSAGE "Формирование отчета..".

/*Выбор заведенных вручную*/
FOR EACH code WHERE
       code.class  = "KassZ"
   AND code.parent = "KassZ"
   AND code.code   BEGINS STRING(end-date,"99-99-9999")
   NO-LOCK:

   FOR each history WHERE
          history.modify NE "RUN"
      AND history.modify NE "PRINT"
      AND NOT history.FIELD-ref BEGINS "_system_"
      AND history.modify NE "SAVE"
      AND history.field-ref EQ code.class + "," + code.code
      AND history.file-name EQ "code"
      NO-LOCK:
      LEAVE.   
   END.
   
   IF AVAIL(history)
   THEN mDate = STRING(history.modif-date,"99/99/9999") + " " + STRING(history.modif-time,"hh:mm:ss").
   ELSE mDate = SUBSTRING(code.code,1,10).
   
   CREATE tt-otch.
   ASSIGN
      tt-otch.datez   = mDate
      tt-otch.fio     = code.description[1]
      tt-otch.acct    = code.name
      tt-otch.symbol  = ENTRY(2,code.val,"|")
      tt-otch.amt     = DECIMAL(ENTRY(1,code.val,"|"))
      tt-otch.user-id = IF AVAIL(history) THEN history.user-id ELSE "ФЛ". 
END.
/*Выбор заведенных банк-клиентом*/
FOR EACH op WHERE
       op.op-date EQ end-date
   AND op.filial-id EQ "0500"
   AND op.op-status EQ "КЗ"
   NO-LOCK,
   EACH op-entry OF op
   NO-LOCK:
      
   {find-act.i
      &acct = op-entry.acct-db
   }
   
   IF AVAIL(acct) THEN
   DO:
      RUN GetCustName IN h_Base  
         (acct.cust-cat, 
          acct.cust-id,
          "",
          OUTPUT mName[1],
          OUTPUT mName[2],
          INPUT-OUTPUT mINN).
      mCliName = TRIM(mName[1] + " " + mName[2]).
   END.
   
   mDate = GetXattrValueEx("op",STRING(op.op),"Примечание","").
   
   IF NOT {assigned mDate} THEN
   DO: 
      FOR each history WHERE 
             history.modify NE "RUN"
         AND history.modify NE "PRINT"
         AND history.modify NE "SAVE"
         AND NOT history.FIELD-ref BEGINS "_system_"
         AND history.field-ref EQ STRING(op.op)
         AND history.file-name EQ "op"
         NO-LOCK:
         LEAVE.
      END.
      IF AVAIL(history)
      THEN mDate = STRING(history.modif-date,"99/99/9999") + " " + STRING(history.modif-time,"hh:mm:ss").
      ELSE mDate = STRING(op.op-date,"99/99/9999").
   END.
   
   CREATE tt-otch.
   ASSIGN
      tt-otch.datez   = mDate
      tt-otch.fio     = mCliName   /*op.name-ben*/
      tt-otch.acct    = op-entry.acct-db
      tt-otch.symbol  = op-entry.symbol
      tt-otch.amt     = op-entry.amt-rub
      tt-otch.user-id = op.user-id.
END.

{rep-kassz-h.i}

DEFINE VARIABLE mItogoRUR AS DECIMAL NO-UNDO.
DEFINE VARIABLE mItogoUSD AS DECIMAL NO-UNDO.
DEFINE VARIABLE mItogoEUR AS DECIMAL NO-UNDO.
DEFINE VARIABLE mItogoKZT AS DECIMAL NO-UNDO.

ASSIGN
   mCnt   = 0
   mItogoRUR = 0
   mItogoUSD = 0
   mItogoEUR = 0
   mItogoKZT = 0.

FOR EACH tt-otch NO-LOCK:
   mCnt   = mCnt + 1.
   CASE SUBSTRING(tt-otch.acct,6,3):
      WHEN "810" THEN mItogoRUR = mItogoRUR + tt-otch.amt.
      WHEN "840" THEN mItogoUSD = mItogoUSD + tt-otch.amt.
      WHEN "978" THEN mItogoEUR = mItogoEUR + tt-otch.amt.
      WHEN "398" THEN mItogoKZT = mItogoKZT + tt-otch.amt.
   END CASE.
    
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String">' + STRING(mCnt) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String">' + tt-otch.datez + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + tt-otch.fio + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + DelFilFromAcct(tt-otch.acct) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s67"><Data ss:Type="Number">' + TRIM(STRING(tt-otch.amt,">>>>>>>>>>>>>>>>>>>9.99")) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String">' + tt-otch.symbol + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String">' + tt-otch.user-id + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
END.

IF mItogoRUR GT 0 THEN
DO:
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">Итого, RUR</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s67"><Data ss:Type="Number">' + TRIM(STRING(mItogoRUR,">>>,>>>,>>>,>>>,>>>,>>>,>>9.99")) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
END.

IF mItogoUSD GT 0 THEN
DO:
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">Итого, USD</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s67"><Data ss:Type="Number">' + TRIM(STRING(mItogoUSD,">>>,>>>,>>>,>>>,>>>,>>>,>>9.99")) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
END.

IF mItogoEUR GT 0 THEN
DO:
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">Итого, EUR</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s67"><Data ss:Type="Number">' + TRIM(STRING(mItogoEUR,">>>,>>>,>>>,>>>,>>>,>>>,>>9.99")) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
END.
IF mItogoKZT GT 0 THEN
DO:
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">Итого, KZT</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s67"><Data ss:Type="Number">' + TRIM(STRING(mItogoKZT,">>>,>>>,>>>,>>>,>>>,>>>,>>9.99")) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String"></Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
END.

{rep-kassz-f.i}

OUTPUT STREAM out-stream CLOSE.

MESSAGE "Отчет готов.".

RUN sndbispc ("file=" + mFileName + ";class=bq").

RETURN.
