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
DEFINE VARIABLE m31F AS CHARACTER  NO-UNDO.
DEFINE VARIABLE m31F2 AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mFileName AS CHARACTER  NO-UNDO.

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
	
OUTPUT TO VALUE("/home2/bis/quit41d/log/ibank/tt-telex-" + STRING(TODAY,"99-99-9999") + ".txt") APPEND UNBUFFERED.

PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " Start" SKIP.

{empty tt-acct}

/*Заполняем счета из mail-user*/
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

/*mLastRID = 475444288.*/

PUT UNFORMATTED "1 mLastRID  = " STRING(mLastRID) SKIP.

{empty tt-telex}

lastRidOp = 0.
FOR EACH history WHERE RECID(history) GT mLastRID
   AND history.file-name = 'op'
   AND CAN-DO("C,W,D",history.modify)
   AND history.modif-date GE TODAY 
   NO-LOCK BY RECID(history) QUERY-TUNING ( NO-INDEX-HINT ):
   FOR EACH op WHERE 
          op.op        EQ INT64(ENTRY(1,history.field-ref))
      AND op.op-status GT "КЛБ"
      NO-LOCK,
      EACH op-entry OF op
      NO-LOCK:
      FOR EACH tt-acct WHERE
         (tt-acct.acct = op-entry.acct-db OR tt-acct.acct = op-entry.acct-cr) NO-LOCK:
         IF op.op-date EQ ? 
         THEN mOpDate = op-entry.value-date.
         ELSE mOpDate = op-entry.op-date.
         RUN create_telex(tt-acct.acct,
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

PUT UNFORMATTED "2 lastRidOp = " STRING(lastRidOp) SKIP.

IF lastRidOp EQ 0 THEN 
DO:
   OUTPUT CLOSE.
   RETURN.
END.

/* Сохраняем lastRidOp */

FOR EACH DataBlock WHERE DataBlock.DataClass-Id EQ 'bk_plusbank'
   AND DataBlock.Beg-Date <= TODAY
   BY DataBlock.Beg-Date DESCENDING:

   DataBlock.description = STRING(lastRidOp).
   mLastRID = -1.
   LEAVE.
END.
IF mLastRID <> -1 THEN
DO:
   CREATE DataBlock.
   ASSIGN
      DataBlock.dataclass-id = 'bk_plusbank'
      DataBlock.beg-date = TODAY
      DataBlock.end-date = TODAY
      DataBlock.Description = string(lastRidOp).
END.

FOR EACH tt-telex NO-LOCK
   BREAK BY tt-telex.acct BY tt-telex.op-date:
   
   IF FIRST-OF(tt-telex.acct) THEN 
   DO:
      PUT UNFORMATTED 
         tt-telex.acct 
      SKIP.
   END.
   PUT UNFORMATTED
      tt-telex.mail-user-num ";" 
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

/*0000*/
bFirst = false.
mFileName = "100hab" + TRIM(STRING(TIME,"hh:mm:ss")) + ".tel".
mFileName = REPLACE(mFileName,":","").

OUTPUT TO VALUE("/home2/bis/quit41d/imp-exp/0000/bss/in/tmp/" + mFileName).

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
mFileName = "100hab" + TRIM(STRING(TIME,"hh:mm:ss")) + ".tel".
mFileName = REPLACE(mFileName,":","").

OUTPUT TO VALUE("/home2/bis/quit41d/imp-exp/0300/bss/in/tmp/" + mFileName).

PUT UNFORMATTED "YZYZ" SKIP(2).

FOR EACH tt-telex WHERE
   tt-telex.filial-id EQ "0300" NO-LOCK 
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
