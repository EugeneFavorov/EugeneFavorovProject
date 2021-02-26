/*
   Банковская интегрированная система БИСквит
   Comment:     загрузка заготовок овер
*/

{globals.i}
{intrface.get xclass}

{ksh-defs.i}

DEFINE VARIABLE shFilial-defore AS CHARACTER NO-UNDO.

shFilial-defore = shFilial.
shFilial = "0500".

DEF INPUT PARAMETER iForm AS CHAR.
DEF OUTPUT PARAMETER outHandle AS CHAR.
DEF VAR bb AS LOGICAL NO-UNDO.

DEF TEMP-TABLE tt-cyberplat LIKE bank.cyberplat.
 
 FUNCTION ClDay RETURN LOGICAL (INPUT in-op-date  AS DATE, INPUT in-acct-cat AS CHAR):
  DEF BUFFER acct-pos FOR acct-pos.
  FOR LAST acct-pos WHERE 
         acct-pos.filial-id = '0500'
     AND acct-pos.acct-cat  = in-acct-cat  
     AND acct-pos.since    >= in-op-date  
     NO-LOCK:
     RETURN YES.
  END.
  RETURN NO.
END.
    
{empty tt-cyberplat}    

IF iForm = 'YES' THEN 
DO:    
   FOR EACH op-date break by op-date.op-date DESCENDING:
      if ClDay(op-date.op-date, "b") = false 
      then end-date = op-date.op-date.
      else 
      do:
         leave.
      end.
   END.

   FOR EACH bank.cyberplat  WHERE TRUE 
	   /*AND bank.cyberplat.idate GE end-date*/
	   /*AND bank.cyberplat.did   EQ 158060048*/
      AND bank.cyberplat.idate GE DATE("01/11/2017")
      AND bank.cyberplat.PROG  EQ "PK_OVERDRAFT_PAY[CLOSECONTRACT]"
      NO-LOCK QUERY-TUNING (NO-INDEX-HINT):
          
      bb = TRUE.
      FOR EACH signs WHERE signs.file-name = 'op'
         AND signs.code = 'did_over' 
         AND signs.dec-value = bank.cyberplat.did NO-LOCK:
         
         FIND FIRST op WHERE op.op = INT64(signs.surrogate) NO-LOCK NO-ERROR.
         IF AVAIL op THEN DO:
            bb = FALSE.
         END.
      END.
        
      IF bb THEN 
      DO:
         CREATE tt-cyberplat.
         BUFFER-COPY bank.cyberplat TO tt-cyberplat.
         MESSAGE "Обработана заготовка PK_OVERDRAFT_PAY[CLOSECONTRACT] " + TRIM(STRING(bank.cyberplat.did)).
/*         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",    */
/*            "bank.cyberplat.did = " + TRIM(STRING(bank.cyberplat.did))).*/
      END.          
   END.
   
	/*объявляем handle для передачи temp-table в УТ*/
   DEF NEW SHARED VAR hO-tt AS HANDLE NO-UNDO.
   DEF VAR hO-b    AS HANDLE NO-UNDO.
   DEF VAR hO-btt  AS HANDLE NO-UNDO.
   
	/*заполняем таблицу для УТ*/
   hO-b = BUFFER tt-cyberplat:HANDLE.
   
   CREATE TEMP-TABLE hO-tt.
   hO-tt:CREATE-LIKE("tt-cyberplat").
   hO-tt:TEMP-TABLE-PREPARE("xtt-cyberplat").
   
   hO-btt = hO-tt:DEFAULT-BUFFER-HANDLE.
   
   FOR EACH tt-cyberplat:
     hO-btt:BUFFER-CREATE.
     hO-btt:BUFFER-COPY(hO-b).
   END.

   outHandle = STRING(hO-tt).

   shFilial = shFilial-defore.
   
END.

IF iForm = 'NO' THEN DO:
/*   DELETE OBJECT hO-tt.*/
END.    

RETURN.
     