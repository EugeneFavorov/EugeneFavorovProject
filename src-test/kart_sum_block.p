/* Глухов */
{globals.i}


{intrface.get tmess}
{intrface.get xclass}

DEF        PARAM BUFFER op      FOR op.
DEF INPUT  PARAM        iParam  AS  CHAR NO-UNDO.
DEF OUTPUT PARAM        oResult AS  LOG  NO-UNDO INIT ?.

DEFINE VARIABLE mHandle  AS INT64     NO-UNDO.
DEFINE VARIABLE iStat    AS INT64     NO-UNDO.
DEFINE VARIABLE vInt     AS INT64     NO-UNDO.

DEF VAR mOk    AS LOG  NO-UNDO.
DEF VAR mDr    AS CHAR NO-UNDO.
DEF VAR mAmt   AS DEC  NO-UNDO.
DEF VAR mEntry AS INT  NO-UNDO.


/* message 123 view-as alert-box.*/


IF     iParam EQ "SET"
   AND op.op-status EQ "ФБН" 
   AND op.filial-id EQ "0500" 
   AND CAN-FIND(FIRST op-bank OF op WHERE op-bank.bank-name MATCHES '*сбербанк*' USE-INDEX op-bank NO-LOCK) 
   AND STRING(TIME + (360 - TIMEZONE) * 60,"HH:MM:SS") LT REPLACE(SUBSTR(TRIM(FGetSetting('СБРФЭкспортДо',?,"00:00")),1,5),"-",":")
THEN 
DO:
   FOR EACH op-entry OF op WHERE op-entry.acct-cr EQ "30102810152090000884     @0500" 
                             AND CAN-DO("!30110*,*",op-entry.acct-db)
                             AND op-entry.amt-rub LE 5000000
   EXCLUSIVE-LOCK:
      op-entry.acct-cr = "30110810101100000047     @0500".
   END.
END.


IF op.user-id EQ "SYNC" THEN
DO:
/*   message "SYNC" view-as alert-box.*/
   oResult = Yes.
   RETURN.
ENd.



FOR EACH op-entry OF op WHERE CAN-DO("40702*,40703*,40802*,40807*,40817*,40820*",op-entry.acct-db) NO-LOCK, 
   EACH acct WHERE acct.acct     EQ op-entry.acct-db 
                AND acct.currency EQ op-entry.currency
   NO-LOCK,
   EACH links WHERE                                           
                     links.link-id   EQ 190
                 AND links.source-id EQ acct.acct + "," + acct.currency
                 AND links.target-id EQ "599"
                 AND links.beg-date  LE op-entry.op-date
                 AND (links.end-date GE op-entry.op-date OR links.end-date EQ ?)
   NO-LOCK:  
   LEAVE.
END.


IF NOT AVAIL op-entry OR GetXattrValueEx("op",STRING(op.op),"НеБлокСумКарт","") EQ "ДА" THEN
DO:
   oResult = Yes.
   RETURN.
ENd.

IF op-entry.currency NE "" AND op-entry.amt-cur EQ 0 THEN
DO:
   oResult = Yes.
   RETURN.
END.


mOk = Yes.
mDr = GetXattrValueEx("op",STRING(op.op),"БлокСуммКарт","").

IF iParam EQ "SET" AND mDr EQ ""
   OR 
   iParam EQ "SET" AND mDr NE "" AND (if op-entry.currency eq "" then op-entry.amt-rub else op-entry.amt-cur) NE DECIMAL(ENTRY(2,mDr,"|"))
   OR
   iParam EQ "DEL" AND op.op-status LT "ФБН" AND mDr NE ""
THEN
DO:
         mOk = Yes.
         IF    iParam EQ "SET" AND mDr NE "" AND (if op-entry.currency eq "" then op-entry.amt-rub else op-entry.amt-cur) NE DECIMAL(ENTRY(2,mDr,"|")) THEN
         DO:
            ASSIGN 
                mEntry = INT(ENTRY(1,mDr,"|"))
                mAmt   = DECIMAL(ENTRY(2,mDr,"|")).
            RUN _ksumblock IN THIS-PROCEDURE("DEL").
         END.
         IF mOk NE ? THEN
         DO:
            ASSIGN 
                mEntry = op-entry.op-entry
                mAmt   = (if op-entry.currency eq "" then op-entry.amt-rub else op-entry.amt-cur).
            RUN _ksumblock IN THIS-PROCEDURE(iParam).
         END.
END.

oResult = mOk.

{intrface.del}


PROCEDURE _ksumblock.
DEF INPUT PARAMETER iPararam AS CHAR NO-UNDO.
    ASSIGN
       mOk = ? NO-ERROR.
/*   message iPararam mAmt view-as alert-box. */
   RUN STORED-PROCEDURE CHANGE_ACC_HOLD mHandle = PROC-HANDLE NO-ERROR
      (INPUT PARAM p_Oper = op.op,
       INPUT PARAM p_Oper_Entry = op-entry.op-entry,
       INPUT PARAM p_Acc = op-entry.acct-db,
       INPUT PARAM p_Amount = mAmt,
       INPUT PARAM p_Mode = iPararam).

   IF ERROR-STATUS:ERROR THEN 
   DO:  
         RUN Fill-SysMes IN h_tmess ("","","",
            SUBSTITUTE("Не удалось запустить &1 блокировки суммы на карте &3. &2",
                      IF iPararam = "SET" THEN "установку" ELSE "снятие",
                      ERROR-STATUS:GET-MESSAGE(1),op-entry.acct-db)).      
   END.
   ELSE 
   DO:
      CLOSE STORED-PROC Change_Acc_Hold  iStat = PROC-STATUS WHERE PROC-HANDLE = mHandle.
      /*iStat = 1.*/
      IF iStat EQ 0 THEN
      DO:
         mOk = Yes.

         IF NOT UpdateSigns(op.class-code,
                            STRING(op.op),
                            "БлокСуммКарт",
                            IF iPararam = "SET" THEN STRING(op-entry.op-entry) + "|" + STRING((if op-entry.currency eq "" then op-entry.amt-rub else op-entry.amt-cur)) ELSE "",
                            ?)
         THEN
            RUN Fill-SysMes IN h_tmess ("","","",
               SUBSTITUTE("Не удалось &3 ДР БлокСуммКарт на карте &1. Документ ид=&2",
                          op-entry.acct-db,op.op,
                          IF iPararam = "SET" THEN "установить" ELSE "удалить")).      
      END.
      ELSE 
      DO:
      IF INDEX (ERROR-STATUS:GET-MESSAGE(ERROR-STATUS:NUM-MESSAGES),"не найден в авторизаторе") > 0 THEN
         mOk = Yes.
      ELSE
         RUN Fill-SysMes IN h_tmess ("","","",
            SUBSTITUTE("Не удалось &1блокировать сумму &4 на карте &2 ~n &3",
                       IF iPararam = "SET" THEN "за" ELSE "раз",op-entry.acct-db,
                       ERROR-STATUS:GET-MESSAGE(ERROR-STATUS:NUM-MESSAGES),
                       (if op-entry.currency eq "" then op-entry.amt-rub else op-entry.amt-cur)
                       )).      
      END.
   END.
END PROCEDURE.        	