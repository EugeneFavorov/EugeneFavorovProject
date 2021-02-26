/*
*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}

DEFINE INPUT PARAMETER iNum AS INT64     NO-UNDO.

DEFINE VARIABLE mFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPredpr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSubject    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBegDate    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCloseDate  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSumm       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mPost       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mPostStr    AS CHARACTER NO-UNDO.

DEFINE STREAM out-stream.

mFileName = "./" + 
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-bl-fl.xml".
OUTPUT STREAM out-stream TO VALUE(mFileName) 
       UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

{rep-bl-fl-h.i}

/*
Код клиента	
ФИО клиента	
Номер счета	
Остаток на счете
Сумма поступления
Наименование блокировки
Орган указанный в лимите лимита
Тип постановления
Дата начала блокировки
Сумма блокировки
*/

MESSAGE "Формирование отчета..".

FOR EACH acct WHERE
   CAN-DO("40817*,40820*,423*,426*",acct.acct)
   AND acct.filial-id  EQ shFilial
/*   AND acct.acct EQ "40817810501600017452     @0500"*/
/*   AND acct.close-date EQ ?*/
   NO-LOCK,
   EACH BlockObject WHERE
       BlockObject.class-code   EQ "BlockAcct"
   AND BlockObject.file-name    EQ "acct"
   AND BlockObject.surrogate    EQ acct.acct + "," + acct.currency
   AND BlockObject.end-datetime EQ ?
   AND BlockObject.beg-datetime LE NOW
   NO-LOCK,
   FIRST person WHERE
      person.person-id EQ INT64(acct.cust-id)
      AND person.date-out EQ ?
   NO-LOCK:

/*   ASSIGN                                                                        */
/*      mPredpr  = GetXattrValueEx("person",STRING(person.person-id),"Предпр", "") */
/*      mSubject = GetXattrValueEx("person",STRING(person.person-id),"Субъект","").*/
/*                                                                                 */
/*   IF mPredpr  BEGINS "Пред" THEN NEXT.                                          */
/*   IF mSubject EQ     "ФЛП"  THEN NEXT.                                          */

   IF NOT (BlockObject.txt[2] MATCHES "УФССП"
      OR   BlockObject.txt[2] MATCHES "Суд") THEN NEXT.   

   IF NOT (BlockObject.txt[3] MATCHES "Арест*"
      OR   BlockObject.txt[3] MATCHES "Взыскание*") THEN NEXT.
   
   mBegDate   = STRING(DATE(BlockObject.beg-datetime),"99/99/9999").
   mBegDate   = IF {assigned mBegDate} THEN mBegDate ELSE "".
   mCloseDate = IF acct.close-date NE ? THEN STRING(acct.close-date,"99/99/9999") ELSE "".
   mSumm      = TRIM(STRING(BlockObject.val[4],"->>>,>>>,>>>,>>9.99")).
   
   RUN acct-pos IN h_base(acct.acct,acct.currency,TODAY,TODAY,CHR(251)).
   
   mOst = IF acct.currency NE "" 
      THEN TRIM(STRING(ABS(sh-val),"->>>,>>>,>>>,>>9.99")) 
      ELSE TRIM(STRING(ABS(sh-bal),"->>>,>>>,>>>,>>9.99")).

   mPost = 0.
   FOR EACH op-entry WHERE
          op-entry.op-date GE TODAY - iNum
      AND op-entry.op-date LE TODAY
      AND op-entry.acct-cr EQ acct.acct
      NO-LOCK:
      mPost = mPost + op-entry.amt-rub.
   END.
   mPostStr = TRIM(STRING(mPost,"->>>,>>>,>>>,>>9.99")).
   
   PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + STRING(person.person-id) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + person.name-last + ' ' + person.first-names + '</Data></Cell>' SKIP.
   IF mCloseDate EQ "" 
   THEN
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + DelFilFromAcct(acct.acct) + '</Data></Cell>' SKIP.
   ELSE
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + DelFilFromAcct(acct.acct) + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s68"><Data ss:Type="String">' + mCloseDate + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s68"><Data ss:Type="Number">' + mOst     + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s68"><Data ss:Type="Number">' + mPostStr + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + BlockObject.block-type + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + BlockObject.txt[2] + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s66"><Data ss:Type="String">' + BlockObject.txt[3] + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s69"><Data ss:Type="String">' + mBegDate + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s68"><Data ss:Type="Number">' + mSumm + '</Data></Cell>' SKIP.
   PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
/*   LEAVE.*/
END.

{rep-bl-fl-f.i}

OUTPUT STREAM out-stream CLOSE.

MESSAGE "Отчет готов.".

RUN sndbispc ("file=" + mFileName + ";class=bq").

RETURN.
