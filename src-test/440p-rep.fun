FUNCTION GetFileVar RETURNS char
   (INPUT aParam AS CHARACTER, 
   INPUT aName AS CHARACTER,
   INPUT aDef AS CHARACTER).
  
   DEFINE VARIABLE iii AS INT64 NO-UNDO.
    
   DO iii = 1 TO num-entries(aParam, chr(10)):
      IF ENTRY(1, ENTRY(iii, aParam, CHR(10)), ":") EQ aName
      THEN RETURN ENTRY(2, ENTRY(iii, aParam, CHR(10)), ":").
   END.
   RETURN aDef.
END FUNCTION.

FUNCTION GetFileVarEntry RETURNS CHAR
   (INPUT aParam AS CHAR, INPUT aName AS CHAR, INPUT aIdx AS INT, INPUT aDef AS CHAR).
  
   DEF VAR iii AS INT64 NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
   i=1.
   DO iii = 1 TO NUM-ENTRIES(aParam, CHR(10)):
      IF ENTRY(1, ENTRY(iii, aParam, CHR(10)), ":") = aName THEN 
      DO:
         IF i EQ aIdx THEN RETURN ENTRY(2, ENTRY(iii, aParam, CHR(10)), ":").
         i = i + 1.
      END.
   END.
   RETURN aDef.
END FUNCTION.

FUNCTION GetFileStr RETURNS CHAR
   (INPUT aParam AS CHAR, INPUT aNum AS INT, INPUT aDef AS CHAR).
  
   IF NUM-ENTRIES(aParam, CHR(10)) < aNum THEN RETURN aDef.
   RETURN ENTRY( 1, ENTRY(aNum, aParam, CHR(10)), "@@@").
END FUNCTION.

FUNCTION PosonTime RETURNS DECIMAL
   (INPUT iAcct AS CHAR,
    INPUT iCurr AS CHAR,
    INPUT iTime AS DATETIME).
    
   DEF VAR cldt AS DATETIME NO-UNDO.
   DEF VAR opdt AS DATETIME NO-UNDO.
   DEF VAR cld AS DATE NO-UNDO.
   DEF VAR opsum AS DECIMAL NO-UNDO.
   DEF VAR opst AS CHAR NO-UNDO.
   DEF VAR idx AS INTEGER NO-UNDO.
   
   RUN acct-pos IN h_base(iAcct,iCurr,DATE(iTime),DATE(iTime),'”').
   /*   RUN acct-pos IN h_base(iAcct,iCurr,DATE("13/10/2015"),DATE("13/10/2015"),CHR(251)).*/
   
   /*cld ¯®á«¥¤­¨© § ªàëâë© ¤¥­ì*/
   FOR EACH op-date WHERE 
      op-date.op-date <= DATE(iTime) 
      NO-LOCK BY op-date.op-date DESC:
      
      cldt = ?.
      FOR EACH history 
         WHERE history.modify NE "RUN"        
         AND history.modify NE "PRINT"
         AND NOT history.FIELD-ref BEGINS "_system_"
         AND history.modify NE "SAVE" 
         AND history.field-ref EQ STRING( op-date.op-date )
         AND history.file-name EQ 'op-date' NO-LOCK
         BY history.file-name DESC
         BY history.modif-date DESC
         BY history.modif-time DESC
         BY (IF history.modify = "C"  THEN 1 ELSE
         (IF history.modify = "W"  THEN 2 ELSE
         (IF history.modify = "D"  THEN 3 ELSE
         (IF history.modify = "L"  THEN 4 ELSE 0
         ) ) ) ) QUERY-TUNING(NO-INDEX-HINT):
            
         IF CAN-DO('*‡ ªàëâ¨¥ ®¯¥à¤­ï*', history.field-value) THEN 
         DO:
            /* MESSAGE STRING( DATE(iTime) ) + ' ' + STRING( history.field-value) view-as alert-box. */
            cldt = DATETIME( history.modif-date, history.modif-time).
            LEAVE.
         END.
      END. 
      IF iTime > cldt THEN 
      DO:
         cld = op-date.op-date.
         LEAVE.
      END.
   END.
   
   
/*   MESSAGE STRING( iTime) view-as alert-box.      */
/*   MESSAGE '2: ' + STRING( cld) view-as alert-box.*/
   
   FOR EACH op-entry 
      WHERE op-entry.op-date >= cld AND op-entry.op-date <= DATE( iTime)
      AND (op-entry.acct-db EQ iAcct OR op-entry.acct-cr EQ iAcct) NO-LOCK,
      EACH op OF op-entry NO-LOCK:
      opdt = ?. 
      
      opst = op.op-status.
      FOR EACH history 
         WHERE history.modify NE "RUN"        
         AND history.modify NE "PRINT"
         AND NOT history.FIELD-ref BEGINS "_system_"
         AND history.modify NE "SAVE" 
         AND history.field-ref EQ STRING(op.op)
         AND history.file-name EQ 'op' NO-LOCK
                  BREAK BY history.file-name DESC
                  BY history.modif-date DESC
                  BY history.modif-time DESC
                  BY (IF history.modify = "C"  THEN 1 ELSE
                  (IF history.modify = "W"  THEN 2 ELSE
                  (IF history.modify = "D"  THEN 3 ELSE
                  (IF history.modify = "L"  THEN 4 ELSE 0
                  ) ) ) ) QUERY-TUNING(NO-INDEX-HINT):
         /* message iAcct + ' ' + STRING(DATETIME( history.modif-date, history.modif-time * 1000)) + ' ' + history.field-value view-as alert-box. */
         IF DATETIME( history.modif-date, history.modif-time * 1000) < iTime THEN LEAVE.
/*         MESSAGE 1 ";" op-entry.amt-rub ";" STRING(iTime,"99/99/99 HH:MM:SS") ";"                  */
/*                 STRING(DATETIME(history.modif-date,history.modif-time * 1000),"99/99/99 HH:MM:SS")*/
/*         VIEW-AS ALERT-BOX.                                                                        */
         
         idx = INDEX(history.field-value, 'op-status').
         IF idx > 0 THEN 
         DO:
            opst = ENTRY(2, SUBSTR( history.field-value, idx)).
         END.
         IF history.modify = "C"  THEN opst = ?.
      END. 
   
      
      opsum = (IF iCurr <> '' THEN op-entry.amt-cur ELSE op-entry.amt-rub).

/*      message                                                                */
/*         3 ";"                                                               */
/*         iAcct ";"                                                           */
/*         STRING(DATETIME( history.modif-date, history.modif-time * 1000)) ";"*/
/*         STRING(op-entry.amt-rub) ";"                                        */
/*         string(opsum)                                                       */
/*      view-as alert-box.                                                     */
      
      FOR EACH history 
         WHERE history.modify NE "RUN"        
         AND history.modify NE "PRINT"
         AND NOT history.FIELD-ref BEGINS "_system_"
         AND history.modify NE "SAVE" 
         AND history.field-ref EQ STRING(op-entry.op) + ',' + STRING(op-entry.op-entry)
         AND history.file-name EQ 'op-entry' NO-LOCK

                  BREAK BY history.file-name DESC
                  BY history.modif-date DESC
                  BY history.modif-time DESC
                  BY (IF history.modify = "C"  THEN 1 ELSE
                  (IF history.modify = "W"  THEN 2 ELSE
                  (IF history.modify = "D"  THEN 3 ELSE
                  (IF history.modify = "L"  THEN 4 ELSE 0 )))) QUERY-TUNING(NO-INDEX-HINT):
         
         IF DATETIME( history.modif-date, history.modif-time * 1000) < iTime THEN LEAVE.
/*         MESSAGE 2 ";" op-entry.amt-rub ";" STRING(iTime,"99/99/99 HH:MM:SS") ";"                  */
/*                 STRING(DATETIME(history.modif-date,history.modif-time * 1000),"99/99/99 HH:MM:SS")*/
/*         VIEW-AS ALERT-BOX.                                                                        */
         idx = INDEX(history.field-value, (IF iCurr <> '' THEN 'amt-cur' ELSE 'amt-rub')).
         IF idx > 0 THEN 
         DO:
            opsum = DEC(ENTRY(2, SUBSTR( history.field-value, idx))).
         /*
         message iAcct + ' ' + STRING(DATETIME( history.modif-date, history.modif-time * 1000)) + ' ' + 
         STRING(op-entry.amt-rub) + ' ' + string(opsum)
         view-as alert-box. 
         */
         END.
         IF history.modify = "C"  THEN opsum = 0.
      END. 
      
/*      message                                                                */
/*         4 ";"                                                               */
/*         iAcct ";"                                                           */
/*         STRING(DATETIME( history.modif-date, history.modif-time * 1000)) ";"*/
/*         STRING(op-entry.amt-rub) + ' ' + string(opsum) ";"                  */
/*         (if opst eq ? then '?' else opst) ";"                               */
/*         op.op-status                                                        */
/*      view-as alert-box.                                                     */
   
      IF opst EQ ? OR (opst < gop-status AND opst NE '”„„' AND opst NE '”') THEN
      DO:
         /*
         message iAcct + ' ' + STRING(op-entry.amt-rub) + ' ' + (if opst <> ? then opst else '?')
          + ' ' + string( sh-bal) view-as alert-box.
         */
         IF NOT (op.op-status < gop-status AND op.op-status NE '”„„' AND op.op-status NE '”') THEN 
         DO:
            IF iCurr <> '' 
               THEN sh-val = sh-val - (IF op-entry.acct-db EQ iAcct THEN op-entry.amt-cur ELSE - op-entry.amt-cur).
            ELSE sh-bal = sh-bal - (IF op-entry.acct-db EQ iAcct THEN op-entry.amt-rub ELSE - op-entry.amt-rub).
         END.
      END.
      ELSE 
      DO:
         IF NOT (op.op-status < gop-status AND op.op-status NE '”„„' AND op.op-status NE '”') THEN 
         DO:
            IF iCurr <> '' 
               THEN sh-val = sh-val - (IF op-entry.acct-db EQ iAcct THEN op-entry.amt-cur ELSE - op-entry.amt-cur).
            ELSE sh-bal = sh-bal - (IF op-entry.acct-db EQ iAcct THEN op-entry.amt-rub ELSE - op-entry.amt-rub).
         END.
         IF iCurr <> '' 
            THEN sh-val = sh-val - (IF op-entry.acct-db EQ iAcct THEN  - opsum ELSE opsum).
         ELSE sh-bal = sh-bal - (IF op-entry.acct-db EQ iAcct THEN  - opsum ELSE opsum).
      END.
   END.
   RETURN (IF iCurr <> '' THEN sh-val ELSE sh-bal).
END FUNCTION.