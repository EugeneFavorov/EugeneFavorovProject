/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: Запросы на выписки для клиент-банка 
   Parameters:
         Uses:
      Used by:
      Created: kam
*/

{globals.i}

DEF VAR i AS INT64 NO-UNDO.
DEF VAR bFirst AS LOGICAL NO-UNDO.
DEF VAR rid AS INT64 NO-UNDO.
DEF VAR lastRidOp AS INT64 NO-UNDO.
DEF VAR lastRidOpEntry AS INT64 NO-UNDO.
DEF VAR myacctdb AS CHAR NO-UNDO.
DEF VAR myacctcr AS CHAR NO-UNDO.
DEF VAR myacctindex AS INT64 NO-UNDO.
DEF VAR begdate AS DATE.
DEF VAR enddate AS DATE.

DEFINE VARIABLE mOpDate  AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate AS CHARACTER NO-UNDO.
/* def new shared stream stream_telex. */
DEFINE VARIABLE m31F AS CHARACTER  NO-UNDO.
DEFINE VARIABLE m31F2 AS CHARACTER  NO-UNDO.

DEF TEMP-TABLE tt-acct NO-UNDO
	FIELD acct LIKE acct.acct
	FIELD filial-id     AS CHARACTER
   FIELD mail-user-num AS INT64
	INDEX idx acct
	.

DEF TEMP-TABLE tt-telex NO-UNDO
	FIELD acct LIKE acct.acct
	FIELD op AS INT64
	FIELD op-date LIKE op-entry.op-date
	FIELD op-status AS CHARACTER
	FIELD modify LIKE history.modify
	FIELD modif-date AS DATE
	FIELD modif-time AS INT64
	FIELD filial-id     AS CHARACTER
   FIELD mail-user-num AS INT64
	INDEX idx acct
	.
	
OUTPUT TO VALUE("/home2/bis/quit41d/imp-exp/0000/bss/in/tmp/tt-telex.txt") APPEND UNBUFFERED.
PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " - start" SKIP.

{empty tt-acct}

FOR EACH mail-user WHERE 
   mail-user.mail-format EQ "TELEX"
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

rid = 0.
FOR EACH DataBlock WHERE DataBlock.DataClass-Id EQ 'bk_plusbank' 
   AND DataBlock.Beg-Date <= TODAY
   NO-LOCK BY DataBlock.Beg-Date DESCENDING:
   rid = int64 (DataBlock.description).
   LEAVE.
END.
IF rid = 0 THEN 
DO:
   FIND FIRST history WHERE history.file-name = 'op-entry' 
      AND history.modif-date = TODAY - 3
      NO-LOCK NO-ERROR.
   IF NOT AVAIL history THEN RETURN.
   rid = RECID(history).
END.
lastRidOp = rid.	
lastRidOpEntry = rid.

PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " rid " STRING(rid) SKIP.

{empty tt-telex}

FOR EACH history WHERE RECID(history) > rid
   AND history.file-name = 'op'
   AND CAN-DO("C,W",history.modify)
   AND history.modif-date GE TODAY 
   NO-LOCK BY RECID(history) QUERY-TUNING ( NO-INDEX-HINT ):
	/* последние 10мин не берем в расчет, могут быть не завершенные транзакции */
/*   IF history.modif-date EQ TODAY AND history.modif-time > TIME - 600 THEN LEAVE.*/
   FOR EACH op-entry 
      WHERE op-entry.op = INT(ENTRY(1, history.field-ref) )
      AND (op-entry.op-status   >= "√"
      OR op-entry.op-status = "√√" 
      OR op-entry.op-status = "А" 
      OR op-entry.op-status = "КЛБ"
      OR op-entry.op-status = "ФБН"
      OR op-entry.op-status = "ФБО"
      OR op-entry.op-status = "ФБП"
      OR op-entry.op-status = "ФДД"
      ) NO-LOCK:
      FOR EACH tt-acct WHERE (tt-acct.acct = op-entry.acct-db OR tt-acct.acct = op-entry.acct-cr) NO-LOCK:
         IF op-entry.op-date EQ ? 
         THEN mOpDate = op-entry.value-date.
         ELSE mOpDate = op-entry.op-date.
         RUN create_telex(tt-acct.acct,
                          op-entry.op,
                          mOpDate,
                          op-entry.op-status,
                          history.modify,
                          history.modif-date,
                          history.modif-time,
                          tt-acct.mail-user-num,
                          tt-acct.filial-id).
      END.
     
   END.
   lastRidOp = RECID(history).
END.

PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " === " SKIP.

FOR EACH history WHERE RECID(history) > rid
   AND history.file-name = 'op-entry'
   AND CAN-DO("D",history.modify)
   AND history.modif-date GE TODAY 
   NO-LOCK BY RECID(history) QUERY-TUNING ( NO-INDEX-HINT ):
	/* последние 10мин не берем в расчет, могут быть не завершенные транзакции */
/*   IF history.modif-date EQ TODAY AND history.modif-time > TIME - 600 THEN LEAVE.*/
   myacctdb = ''.
   myacctcr = ''.
   myacctindex = INDEX(history.field-value, "acct-db").
   IF myacctindex > 0 THEN myacctdb = ENTRY(2,substring(history.field-value,myacctindex)).
   myacctindex = INDEX(history.field-value, "acct-cr").
   IF myacctindex > 0 THEN myacctcr = ENTRY(2,substring(history.field-value,myacctindex)).
   
   myacctindex = INDEX(history.field-value, "op-date").
   mEndDate = ENTRY(2,substring(history.field-value,myacctindex)).
   IF myacctindex > 0 
      AND {assigned mEndDate} 
   THEN enddate = DATE(mEndDate).
   ELSE enddate = history.modif-date.
   
   FOR EACH tt-acct WHERE (tt-acct.acct = myacctdb OR tt-acct.acct = myacctcr) NO-LOCK:
      IF enddate EQ ? 
      THEN mOpDate = today.
      ELSE mOpDate = enddate.
      PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " " enddate ";" mOpDate SKIP.
      RUN create_telex(tt-acct.acct,
                       op-entry.op,
                       mOpDate,
                       op-entry.op-status,
                       history.modify,
                       history.modif-date,
                       history.modif-time,
                       tt-acct.mail-user-num,
                       tt-acct.filial-id).
/*      RUN create_telex(tt-acct.acct,enddate,history.modify,tt-acct.mail-user-num,tt-acct.filial-id).*/
   END.
   lastRidOpEntry = RECID(history).
END.
	
/* if (lastRidOpEntry <>  rid and lastRidOp > lastRidOpEntry)
      or  */
/*if lastRidOp = rid
   then lastRidOp = lastRidOpEntry.*/

PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " lastRidOp " STRING(lastRidOp) SKIP.

FOR EACH DataBlock WHERE DataBlock.DataClass-Id EQ 'bk_plusbank' 
   AND DataBlock.Beg-Date <= TODAY
   BY DataBlock.Beg-Date DESCENDING:
   DataBlock.description = STRING(lastRidOp).
   rid = -1.
   LEAVE.
END.

IF rid <> -1 THEN 
DO:
   CREATE DataBlock.
   ASSIGN 
      DataBlock.dataclass-id = 'bk_plusbank'
      DataBlock.beg-date = TODAY
      DataBlock.end-date = TODAY
      DataBlock.Description = string(lastRidOp)
      .
END.

FOR EACH tt-telex NO-LOCK: 
   PUT UNFORMATTED 
      tt-telex.acct ";"
      tt-telex.op ";" 
      tt-telex.op-date ";" 
      tt-telex.op-status ";"
      tt-telex.modify ";" 
      tt-telex.modif-date ";" 
      tt-telex.modif-time ";" 
      tt-telex.mail-user-num ";" 
      tt-telex.filial-id 
   SKIP.
END.
OUTPUT CLOSE.

/*0000*/
bFirst = false.
OUTPUT TO VALUE("/home2/bis/quit41d/imp-exp/0000/bss/in/tmp/100hab" + TRIM(STRING(TIME, ">>>>>>>9")) + ".tel"). 

PUT UNFORMATTED "YZYZ" SKIP(2).

FOR EACH tt-telex WHERE
   tt-telex.filial-id EQ "0000" NO-LOCK
   BREAK BY tt-telex.acct BY tt-telex.op-date:
   IF FIRST-OF(tt-telex.acct) AND FIRST-OF(tt-telex.op-date) THEN
      begdate = tt-telex.op-date.
   IF LAST-OF(tt-telex.acct) THEN 
   DO:
      /* enddate = tt-telex.op-date. */
      enddate = TODAY.

      m31F = SUBSTRING(TRIM(STRING(YEAR(begdate))), 3, 2) + STRING(MONTH(begdate), "99") + STRING(DAY(begdate), "99").
      m31F2 = SUBSTRING(TRIM(STRING(YEAR(enddate))), 3, 2) + STRING(MONTH(enddate), "99") + STRING(DAY(enddate), "99").
      IF bFirst THEN 
         PUT UNFORMATTED "QQ" SKIP(2).
      bFirst = TRUE.
      PUT UNFORMATTED
         "FROM:iBank2" SKIP
         "TO:Bisquit" SKIP
         "DATE:" + SUBSTRING(STRING(YEAR(TODAY), "9999"), 3, 2) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") SKIP
         "TIME:" + TRIM(STRING(TIME, ">>>>>>>9")) SKIP " " SKIP
         "::920 ЗАПРОС О СОСТОЯНИИ СЧЕТА" SKIP " " SKIP
         ":20:1344493890720" SKIP
         ":12:940" SKIP
         ":25:" + SUBSTR(tt-telex.acct, 1, 20)  SKIP
         ":31F:" + m31F + "/" + m31F2 SKIP " " SKIP.

   END. /* if last-of(tt-telex.acct) then do: */
END.

PUT UNFORMATTED	"NNNN" SKIP.
OUTPUT CLOSE.

OS-COMMAND SILENT VALUE("chmod 666 /home2/bis/quit41d/imp-exp/0000/bss/in/tmp/*.tel").

if bFirst then
	OS-COMMAND SILENT VALUE("mv /home2/bis/quit41d/imp-exp/0000/bss/in/tmp/*.tel /home2/bis/quit41d/imp-exp/0000/bss/in").
else
	OS-COMMAND SILENT VALUE("rm /home2/bis/quit41d/imp-exp/0000/bss/in/tmp/*.tel").

/*0300*/
bFirst = false.
OUTPUT TO VALUE("/home2/bis/quit41d/imp-exp/0300/bss/in/tmp/100hab" + TRIM(STRING(TIME, ">>>>>>>9")) + ".tel"). 

PUT UNFORMATTED "YZYZ" SKIP(2).

FOR EACH tt-telex WHERE
   tt-telex.filial-id EQ "0300" NO-LOCK 
   BREAK BY tt-telex.acct BY tt-telex.op-date:
   IF FIRST-OF(tt-telex.acct) AND FIRST-OF(tt-telex.op-date) THEN
      begdate = tt-telex.op-date.
   IF LAST-OF(tt-telex.acct) THEN 
   DO:
      /* enddate = tt-telex.op-date. */
      enddate = TODAY.

      m31F = SUBSTRING(TRIM(STRING(YEAR(begdate))), 3, 2) + STRING(MONTH(begdate), "99") + STRING(DAY(begdate), "99").
      m31F2 = SUBSTRING(TRIM(STRING(YEAR(enddate))), 3, 2) + STRING(MONTH(enddate), "99") + STRING(DAY(enddate), "99").
      IF bFirst THEN 
         PUT UNFORMATTED "QQ" SKIP(2).
      bFirst = TRUE.
      PUT UNFORMATTED
         "FROM:iBank2" SKIP
         "TO:Bisquit" SKIP
         "DATE:" + SUBSTRING(STRING(YEAR(TODAY), "9999"), 3, 2) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") SKIP
         "TIME:" + TRIM(STRING(TIME, ">>>>>>>9")) SKIP " " SKIP
         "::920 ЗАПРОС О СОСТОЯНИИ СЧЕТА" SKIP " " SKIP
         ":20:1344493890720" SKIP
         ":12:940" SKIP
         ":25:" + SUBSTR(tt-telex.acct, 1, 20)  SKIP
         ":31F:" + m31F + "/" + m31F2 SKIP " " SKIP.

   END. /* if last-of(tt-telex.acct) then do: */
END.

PUT UNFORMATTED	"NNNN" SKIP.
OUTPUT CLOSE.

OS-COMMAND SILENT VALUE("chmod 666 /home2/bis/quit41d/imp-exp/0300/bss/in/tmp/*.tel").

IF bFirst THEN
	OS-COMMAND SILENT VALUE("mv /home2/bis/quit41d/imp-exp/0300/bss/in/tmp/*.tel /home2/bis/quit41d/imp-exp/0300/bss/in").
ELSE
	OS-COMMAND SILENT VALUE("rm /home2/bis/quit41d/imp-exp/0300/bss/in/tmp/*.tel").	

PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " - stop" SKIP.

RETURN.

PROCEDURE create_telex:
	DEFINE INPUT PARAMETER my_acct AS CHAR.
	DEFINE INPUT PARAMETER my_op AS INT64.
	DEFINE INPUT PARAMETER my_date AS DATE.
	DEFINE INPUT PARAMETER my_status AS CHARACTER.
	DEFINE INPUT PARAMETER my_modify AS CHAR.
	DEFINE INPUT PARAMETER my_modif_date AS DATE.
	DEFINE INPUT PARAMETER my_modif_time AS INT64.
	DEFINE INPUT PARAMETER iNum    AS INT64     NO-UNDO.
	DEFINE INPUT PARAMETER iFilial AS CHARACTER NO-UNDO.

	CREATE tt-telex.
	ASSIGN 
   	tt-telex.acct          = my_acct
   	tt-telex.op            = my_op
   	tt-telex.op-date       = my_date
   	tt-telex.op-status     = my_status
   	tt-telex.modify        = my_modify
   	tt-telex.modif-date    = my_modif_date
   	tt-telex.modif-time    = my_modif_time
   	tt-telex.mail-user-num = iNum
      tt-telex.filial-id     = iFilial
	.
	RELEASE tt-telex.
END PROCEDURE.
