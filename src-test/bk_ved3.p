/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: Запросы на выписки для клиент-банка 
   Parameters:
         Uses:
      Used by:
      Created: kam
2017 Глухов. Полностью переработан механизм работы с хистори.
*/

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */

DEF VAR i AS INT64 NO-UNDO.
DEF VAR bFirst AS LOGICAL NO-UNDO.
DEF VAR mLastRID AS INT64 NO-UNDO.
DEF VAR lastRidOp AS INT64 NO-UNDO.
DEF VAR lastRidOpEntry AS INT64 NO-UNDO.
DEF VAR myacctdb AS CHAR NO-UNDO.
DEF VAR myacctcr AS CHAR NO-UNDO.
DEF VAR myacctindex AS INT64 NO-UNDO.
DEF VAR begdate AS DATE.
DEF VAR enddate AS DATE.

DEFINE VARIABLE mOpDate  AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctCr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctDb  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk      AS CHARACTER NO-UNDO.
DEFINE VARIABLE m31F AS CHARACTER  NO-UNDO.
DEFINE VARIABLE m31F2 AS CHARACTER  NO-UNDO.
DEF VAR mAcctMaxRID AS INT64 NO-UNDO.

DEFINE BUFFER bhistory FOR history.

FUNCTION get_hf RETURN CHAR (INPUT iField AS CHAR, INPUT iVal AS CHAR):
   DEF VAR vPos AS INT64 NO-UNDO.
   
   vPos = LOOKUP(iField,iVal).
   IF vPos NE 0 THEN
      RETURN(ENTRY(vPos + 1,iVal)).
   ELSE
      RETURN(?).
END FUNCTION.

DEFINE VARIABLE mFileName AS CHARACTER  NO-UNDO.

DEF TEMP-TABLE tt-acct NO-UNDO
	FIELD acct LIKE acct.acct
	FIELD currency LIKE acct.currency
	FIELD filial-id     AS CHARACTER
   FIELD mail-user-num AS INT64
	INDEX idx acct
	.

DEF TEMP-TABLE tt-telex NO-UNDO
	FIELD acct LIKE acct.acct
	FIELD currency LIKE acct.currency
	FIELD op AS INT64
	FIELD op-date LIKE op-entry.op-date
	FIELD op-status AS CHARACTER
	FIELD modify LIKE history.modify
	FIELD modif-date AS DATE
	FIELD modif-time AS INT64
	FIELD filial-id     AS CHARACTER
   FIELD mail-user-num AS INT64
	INDEX idx acct  op-date
	.

OUTPUT TO VALUE("/home2/bis/quit41d/log/ibank/tt-telex-" + STRING(TODAY,"99-99-9999") + ".txt") APPEND UNBUFFERED.
/* OUTPUT TO VALUE("./tt-telex-" + STRING(TODAY,"99-99-9999") + ".txt") APPEND UNBUFFERED. */


PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " Start" SKIP.

{empty tt-acct}
/*Заполняем счета из mail-user*/
FOR EACH mail-user WHERE 
   mail-user.mail-format EQ "TELEX"
/*   AND  mail-user.mail-user-num EQ 19092 */
   AND mail-user.module EQ "mail-cli" NO-LOCK:
   DO i = 1 TO NUM-ENTRIES(mail-user.acct):
      FIND FIRST tt-acct WHERE tt-acct.acct = ENTRY (i,mail-user.acct) NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-acct THEN 
      DO:
         CREATE tt-acct.
         ASSIGN 
            tt-acct.acct          = ENTRY (i,mail-user.acct)
            tt-acct.mail-user-num = mail-user.mail-user-num
            tt-acct.filial-id     = mail-user.filial-id.
         RELEASE tt-acct.
      END.
   END.
END.

/*
/*Ищем mLastRID в DataBlock*/
mLastRID = 0.
FOR EACH DataBlock WHERE DataBlock.DataClass-Id EQ 'bk_plusbank'
   AND DataBlock.Beg-Date <= TODAY
   NO-LOCK BY DataBlock.Beg-Date DESCENDING:
   mLastRID = INT64(DataBlock.description).
   LEAVE.
END.

/*Если не нашли ищем в history три дня назад*/
IF mLastRID EQ 0 THEN 
DO:
   FIND FIRST history WHERE 
          history.file-name  EQ "op-entry" 
      AND history.modif-date EQ TODAY - 3
   NO-LOCK NO-ERROR.
   IF NOT AVAIL history THEN RETURN.
   mLastRID = RECID(history).
END.
*/
/*mLastRID = 475444288.*/

PUT UNFORMATTED "1 mLastRID  = " STRING(mLastRID) "           " NOW SKIP.

{empty tt-telex}

lastRidOp = 0.
FOR EACH history WHERE 
         history.file-name  EQ 'op-entry'
     AND history.modif-date EQ TODAY
     AND history.modify     EQ "D"
   NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):

   mOpDate = DATE(get_hf('op-date',history.field-value)) NO-ERROR. 

   IF mOpDate EQ ? THEN 
      ASSIGN
         mOpDate = DATE(get_hf('value-date',history.field-value)) NO-ERROR. 

   IF mOpDate EQ ? THEN mOpDate = TODAY. 
   ASSIGN
      mAcctDb =  get_hf('acct-db',history.field-value)
      mAcctCr =  get_hf('acct-cr',history.field-value).

   FOR EACH tt-acct WHERE (tt-acct.acct = macctdb OR tt-acct.acct = macctcr) NO-LOCK:

      FOR EACH bhistory WHERE 
               bhistory.file-name = 'op'
           AND bhistory.field-ref EQ ENTRY(1,history.field-ref)
           AND bhistory.modify EQ "D"
       NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
   
           mOpDate = MIN(mOpDate,
                         DATE(get_hf('doc-date',bhistory.field-value)),
                         DATE(get_hf('ins-date',bhistory.field-value))) NO-ERROR.
          LEAVE. 
      END.
      IF TODAY - mOpDate  GT 15 THEN mOpDate = TODAY - 15.
      RUN create_telex(tt-acct.acct,
                       tt-acct.currency,
                       INT64(ENTRY(1,history.field-ref)),
                       mOpDate,
                       get_hf('op-status',bhistory.field-value),
                       bhistory.modify,
                       bhistory.modif-date,
                       bhistory.modif-time,
                       RECID(history),
                       tt-acct.filial-id).
      lastRidOp = RECID(history).
   END.
END.
FOR EACH history WHERE 
         history.file-name  EQ 'op'
     AND history.modif-date EQ TODAY
     AND CAN-DO("C,W",history.modify)
   NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
   FOR EACH op WHERE op.op EQ INT64(ENTRY(1,history.field-ref)) NO-LOCK,
      EACH op-entry OF op
      NO-LOCK:
      IF op.op-date EQ ? THEN 
         mOpDate = op-entry.value-date.
      ELSE 
         mOpDate = op-entry.op-date.
      mOpDate = MIN(mOpDate,op.doc-date,op.ins-date).
      IF TODAY - mOpDate  GT 15 THEN mOpDate = TODAY - 15.
      FOR EACH tt-acct WHERE
         (tt-acct.acct = op-entry.acct-db OR tt-acct.acct = op-entry.acct-cr) NO-LOCK:
         RUN create_telex(tt-acct.acct,
                          tt-acct.currency,
                          op.op,
                          mOpDate,
                          op.op-status,
                          history.modify,
                          history.modif-date,
                          history.modif-time,
                          RECID(history),
                          tt-acct.filial-id).
         lastRidOp = RECID(history).
      END.
   END.
END.

PUT UNFORMATTED "2 lastRidOp = " STRING(lastRidOp)  "               " NOW SKIP.

IF lastRidOp EQ 0 THEN 
DO:
   OUTPUT CLOSE.
   RETURN.
END.

FOR EACH tt-telex EXCLUSIVE-LOCK
   BREAK BY tt-telex.acct BY tt-telex.op-date :

   FIND FIRST acct WHERE acct.acct EQ tt-telex.acct NO-LOCK NO-ERROR.

   IF NOT AVAIL acct THEN NEXT.

   mOk = "".
   
      mAcctMaxRID = 0.
      FOR EACH signs WHERE signs.file-name EQ "acct"
                       AND signs.surr      EQ tt-telex.acct + ',' + acct.currency
                       AND signs.code      EQ "КЛБ_ИСТОР_ВЫП"
      NO-LOCK:
         mAcctMaxRID = INT64(signs.code-value).
   END.

      IF mAcctMaxRID LT tt-telex.mail-user-num THEN
          mOk = STRING(UpdateSigns(acct.class-code,tt-telex.acct + ',' + acct.currency,"КЛБ_ИСТОР_ВЫП",STRING(tt-telex.mail-user-num),?),"Да/Нет").
      ELSE
      DO:
         DELETE tt-telex.
         NEXT.
      END.

   PUT UNFORMATTED
      tt-telex.mail-user-num ";" 
      mOk ";"
      tt-telex.acct ";"
      tt-telex.op ";" 
      tt-telex.op-date ";" 
      tt-telex.op-status ";"
      tt-telex.modify ";" 
      tt-telex.modif-date ";"  
      STRING(tt-telex.modif-time,"hh:mm:ss") ";" 
      tt-telex.filial-id 
   SKIP.
END.

OUTPUT CLOSE.

RUN make_file IN THIS-PROCEDURE ("0000").
RUN make_file IN THIS-PROCEDURE ("0300").
RUN make_file IN THIS-PROCEDURE ("0500").

/*0500*/


RETURN.
PROCEDURE make_file:

DEFINE INPUT PARAMETER vfil AS CHAR NO-UNDO.

bFirst = false.
mFileName = "100hab" + vfil + TRIM(STRING(TIME,"hh:mm:ss")) + ".tel".
mFileName = REPLACE(mFileName,":","").

OUTPUT TO VALUE("/home2/bis/quit41d/imp-exp/" + vfil + "/bss/in/tmp/" + mFileName).

PUT UNFORMATTED "YZYZ" SKIP(2).

FOR EACH tt-telex WHERE
   tt-telex.filial-id EQ vfil NO-LOCK 
   BREAK BY tt-telex.acct BY tt-telex.op-date:
   IF FIRST-OF(tt-telex.acct) AND FIRST-OF(tt-telex.op-date) THEN
      begdate = tt-telex.op-date.
   IF LAST-OF(tt-telex.acct) THEN 
   DO:
      /*enddate = tt-telex.op-date.*/
      enddate = TODAY.

      m31F  = SUBSTRING(TRIM(STRING(YEAR(begdate))),3,2) + STRING(MONTH(begdate),"99") + STRING(DAY(begdate),"99").
      m31F2 = SUBSTRING(TRIM(STRING(YEAR(enddate))),3,2) + STRING(MONTH(enddate),"99") + STRING(DAY(enddate),"99").
      
      IF bFirst THEN 
         PUT UNFORMATTED "QQ" SKIP(2).
      bFirst = TRUE.
      PUT UNFORMATTED
         "FROM:iBank2" SKIP
         "TO:Bisquit"  SKIP
         "DATE:" + SUBSTRING(STRING(YEAR(TODAY), "9999"), 3, 2) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") SKIP
         "TIME:" + TRIM(STRING(TIME, ">>>>>>>9")) SKIP " " SKIP
         "::920 ЗАПРОС О СОСТОЯНИИ СЧЕТА" SKIP " " SKIP
         ":20:1344493890720" SKIP
         ":12:940" SKIP
         ":25:" + SUBSTR(tt-telex.acct, 1, 20) SKIP
         ":31F:" + m31F + "/" + m31F2 SKIP " " SKIP.
   END. /* if last-of(tt-telex.acct) */
END.

PUT UNFORMATTED   "NNNN" SKIP.
OUTPUT CLOSE.

OS-COMMAND SILENT VALUE("chmod 666 /home2/bis/quit41d/imp-exp/" + vfil + "/bss/in/tmp/*.tel").

IF bFirst THEN
   OS-COMMAND SILENT VALUE("mv /home2/bis/quit41d/imp-exp/" + vfil + "/bss/in/tmp/*.tel /home2/bis/quit41d/imp-exp/" + vfil + "/bss/in").
ELSE
   OS-COMMAND SILENT VALUE("rm /home2/bis/quit41d/imp-exp/" + vfil + "/bss/in/tmp/*.tel").  

PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " - stop " vfil " запрос "  mFileName SKIP.

END PROCEDURE.


PROCEDURE create_telex:
   DEFINE INPUT PARAMETER my_acct AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER my_currency AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER my_op AS INT64 NO-UNDO.
   DEFINE INPUT PARAMETER my_date AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER my_status AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER my_modify AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER my_modif_date AS DATE  NO-UNDO.
   DEFINE INPUT PARAMETER my_modif_time AS INT64  NO-UNDO.
   DEFINE INPUT PARAMETER iNum    AS INT64     NO-UNDO.
   DEFINE INPUT PARAMETER iFilial AS CHARACTER NO-UNDO.
   
   FIND FIRST tt-telex WHERE tt-telex.acct  EQ  my_acct AND
              tt-telex.op-date EQ my_date EXCLUSIVE-LOCK NO-ERROR. 
       
   IF NOT AVAIL tt-telex THEN 
   CREATE tt-telex.
   
   IF iNum GT tt-telex.mail-user-num THEN 
      ASSIGN 
         tt-telex.acct          = my_acct
         tt-telex.op            = my_op
         tt-telex.op-date       = my_date
         tt-telex.op-status     = MAX(my_status,tt-telex.op-status)
         tt-telex.modify        = my_modify
         tt-telex.modif-date    = my_modif_date
         tt-telex.modif-time    = my_modif_time
         tt-telex.mail-user-num = iNum
         tt-telex.filial-id     = iFilial
      .
   RELEASE tt-telex.
END PROCEDURE.







