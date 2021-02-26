/*
    Ведомость поступлений на счета клиентов-ФЛ
    Надежда Мазепа
-------------------------------
ZSS добавление колонки дата КД
*/

FORMAT "~n@(#) npa.p nd 18/04/06"
   WITH FRAME nnn STREAM-IO WIDTH 150.

{globals.i}
{wordwrap.def}          /* Переменные для разбиения строк. */
{chkacces.i}
{client.i}
{sh-defs.i new}

DEFINE VARIABLE sel-n1 AS INTEGER INIT 1.
DEFINE VARIABLE sel-n2 AS INTEGER INIT 3.
DEFINE VARIABLE sel-n1-str AS CHARACTER INIT "*озвр*,*ашени*,*редит*,*плата*".
DEFINE VARIABLE sel-n2-summ AS DECIMAL INIT 100000.
DEFINE NEW SHARED STREAM vvs.
DEFINE VARIABLE fname AS CHARACTER INIT "./vozv-list.csv"  NO-UNDO.
DEFINE VARIABLE delim AS CHARACTER INIT ";" FORMAT "x(1)" NO-UNDO.
DEFINE VARIABLE eol AS CHARACTER FORMAT "x(2)" NO-UNDO.
eol = CHR(13) + CHR(10). 

DEFINE VARIABLE dateKreditDodovor AS CHARACTER.


DEFINE VARIABLE nacctdb AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE nacctcr AS CHARACTER NO-UNDO INIT ''.
DEFINE VARIABLE mMaskDb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskCr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKD     AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ntt 
   FIELD date AS DATE
   FIELD type AS CHARACTER 
   FIELD acct-db AS CHARACTER FORMAT "x(20)"
   FIELD acct-cr AS CHARACTER FORMAT "x(20)"
   FIELD dateKdDog AS CHARACTER
   FIELD name-db AS CHARACTER
   FIELD name-cr AS CHARACTER
   FIELD summ AS DECIMAL
   FIELD kd AS CHARACTER
   FIELD details AS CHARACTER
   .

/*=================*/
FORMAT
   sel-n1 VIEW-AS RADIO-SET VERTICAL
   RADIO-BUTTONS "все назначения",1,
   "назначения с фильтром",2 
   NO-LABELS SKIP
   "_____________" SKIP
   sel-n2 VIEW-AS RADIO-SET VERTICAL
   RADIO-BUTTONS "все суммы",3,
   " > 100 000",4 
   NO-LABELS " " SKIP	 

   WITH FRAME nn COL 0 SIDE-LABELS OVERLAY CENTERED ROW 9 
   TITLE "[ Входные данные ]".

DO ON ERROR UNDO, RETRY ON ENDKEY UNDO, LEAVE:
   PAUSE 0.
   UPDATE sel-n1 sel-n2 WITH FRAME nn.
END.

IF LASTKEY = 27 THEN 
DO: 
   HIDE FRAME nn NO-PAUSE. 
   RETURN. 
END.
HIDE FRAME nn NO-PAUSE.
/*=================*/

{getdates.i}

{spinner.i "Дождитесь окончания работы процедуры... "}


{empty ntt}

mMaskDb = "30102*,30109*,30110*,407*,30223810401100000003*".
mMaskCr = "40817*,40820*".

FOR EACH op-entry
   WHERE op-entry.op-date >= beg-date
   AND op-entry.op-date <= end-date
   AND CAN-DO(mMaskDb, op-entry.acct-db) 
   AND CAN-DO(mMaskCr, op-entry.acct-cr)
   NO-LOCK,
   EACH op WHERE op.op = op-entry.op
   NO-LOCK:

   FIND FIRST acct WHERE op-entry.acct-db EQ acct.acct NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN nacctdb = acct.details. ELSE NEXT.

   FIND FIRST acct WHERE op-entry.acct-cr EQ acct.acct NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN RUN RE_CLIENT(acct.cust-cat, acct.cust-id, INPUT-OUTPUT nacctcr). ELSE next.

   mKD = "".

   FOR EACH loan-acct WHERE loan-acct.acct EQ acct.acct
      AND loan-acct.currency EQ acct.currency
      AND loan-acct.contract EQ "Кредит"
      NO-LOCK:
      mKD = mkd + delfilfromloan(loan-acct.cont-code) + ",".
  
  find FIRST loan WHERE  loan-acct.cont-code EQ loan.cont-code 
      and loan-acct.contract EQ loan.contract   NO-LOCK NO-ERROR.  
 
   if AVAILABLE loan THEN dateKreditDodovor = string(loan.open-date).
    ELSE dateKreditDodovor = ''.
 END.

   

   mKD = TRIM(mKD,",").

   IF (((sel-n1 = 2) AND (CAN-DO(sel-n1-str,op.details))) OR (sel-n1 = 1)) 
      AND ((sel-n2 = 3) OR ((sel-n2 = 4) AND (op-entry.amt-rub >= sel-n2-summ))) THEN
   DO:
      CREATE ntt.
      ASSIGN 
         ntt.date = op-entry.op-date
         ntt.type = op.doc-type
         ntt.acct-db = op-entry.acct-db
         
         ntt.dateKdDog = dateKreditDodovor
         
         ntt.acct-cr = op-entry.acct-cr  /* "'" + delfilfromacct(op-entry.acct-cr) */
         ntt.name-db = IF op.name-ben <> '' THEN op.name-ben ELSE nacctdb
         ntt.name-cr = nacctcr
         ntt.summ = op-entry.amt-rub
         ntt.details = op.details
         ntt.kd = mkd
         .
   END.

END.


OUTPUT STREAM vvs TO VALUE (fname)
   UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".

PUT STREAM vvs UNFORMATTED
   "Дата" delim
   "ТипДок" delim
   "СчетДт" delim
   "СчетКр" delim
   "№ Кредитного договора" delim
   "Дата КД" delim
   "НаименованиеДт" delim
   "НаименованиеКр" delim	
   "Сумма" delim
   "Назначение" eol.

FOR EACH ntt NO-LOCK:
   PUT STREAM vvs UNFORMATTED
      ntt.date delim
      ntt.type delim
      ntt.acct-db delim
      ntt.acct-cr delim
      ntt.kd delim
      ntt.dateKdDog delim   
      ntt.name-db delim
      ntt.name-cr delim	
      ntt.summ delim
      ntt.details eol.
END.

OUTPUT STREAM vvs CLOSE.

MESSAGE "Данные выгружены в файл " + fname + "." VIEW-AS ALERT-BOX.

RUN sndbispc ("file=" + fname + ";class=bq").


{intrface.del}


