/*
              Банковская интегрированная система БИСквит
 Copyright:
 Filename:    get350-2.p
 Comment:     загрузка заготовок
 Parameters:
 Uses:
 Used by:
 Created:
 Modified:
*/


{globals.i}
{intrface.get xclass}

DEF INPUT  PARAMETER iDay      AS CHAR.
DEF OUTPUT PARAMETER outHandle AS CHAR.

DEFINE VARIABLE mExist AS LOGICAL NO-UNDO.

DEF TEMP-TABLE tt-plat350 LIKE bank.plat_go_350.
 
{empty tt-plat350}    

/*{setdest.i &filename = "'350.log'"}*/

IF iDay NE "" THEN 
DO:
   /*101*/    
   FOR EACH bank.plat_go_350 WHERE 
      bank.plat_go_350.day_file EQ iDay /*"20"*/
/*      AND bank.plat_go_350.tdoc EQ 101*/
/*      AND  (bank.plat_go_350.did EQ 146680101 */
/*         OR bank.plat_go_350.did EQ 146680113 */
/*         OR bank.plat_go_350.did EQ 146680123 */
/*         OR bank.plat_go_350.did EQ 146680127 */
/*         OR bank.plat_go_350.did EQ 146680129 */
/*         OR bank.plat_go_350.did EQ 146681030 */
/*         OR bank.plat_go_350.did EQ 146681087)*/
      NO-LOCK QUERY-TUNING ( NO-INDEX-HINT):
          
/*      PUT UNFORMATTED               */
/*         bank.plat_go_350.did    ";"*/
/*         bank.plat_go_350.idate  ";"*/
/*         bank.plat_go_350.debit  ";"*/
/*         bank.plat_go_350.credit ";"*/
/*      SKIP.                         */
   
      mExist = NO.
      FOR EACH signs WHERE
             signs.file-name EQ 'op'
         AND signs.code      EQ 'did-smartfl'
         AND signs.dec-value EQ bank.plat_go_350.did
         NO-LOCK:
               
         FIND FIRST op WHERE 
            op.op = INT64(signs.surrogate) 
         NO-LOCK NO-ERROR.
         IF AVAIL(op) THEN mExist = YES.
      END.
      IF mExist EQ NO THEN
      DO:
         CREATE tt-plat350.
         BUFFER-COPY bank.plat_go_350 TO tt-plat350.
      END.
   END.
/*   
   /*303*/
   FOR EACH bank.plat_go_350 WHERE 
      bank.plat_go_350.day_file EQ iDay /*"20"*/
      AND bank.plat_go_350.tdoc EQ 303
/*      AND  (bank.plat_go_350.did EQ 146680101 */
/*         OR bank.plat_go_350.did EQ 146680113 */
/*         OR bank.plat_go_350.did EQ 146680123 */
/*         OR bank.plat_go_350.did EQ 146680127 */
/*         OR bank.plat_go_350.did EQ 146680129 */
/*         OR bank.plat_go_350.did EQ 146681030 */
/*         OR bank.plat_go_350.did EQ 146681087)*/
      NO-LOCK QUERY-TUNING ( NO-INDEX-HINT):
          
/*      PUT UNFORMATTED               */
/*         bank.plat_go_350.did    ";"*/
/*         bank.plat_go_350.idate  ";"*/
/*         bank.plat_go_350.debit  ";"*/
/*         bank.plat_go_350.credit ";"*/
/*      SKIP.                         */
   
      mExist = NO.
      FOR EACH signs WHERE
             signs.file-name EQ 'op'
         AND signs.code      EQ 'did-smartfl'
         AND signs.dec-value EQ bank.plat_go_350.did
         NO-LOCK:
               
         FIND FIRST op WHERE 
            op.op = INT64(signs.surrogate) 
         NO-LOCK NO-ERROR.
         IF AVAIL(op) THEN mExist = YES.
      END.
      IF mExist EQ NO THEN
      DO:
         CREATE tt-plat350.
         BUFFER-COPY bank.plat_go_350 TO tt-plat350.
      END.
   END.
*/
 
	/*объявляем handle для передачи temp-table в УТ*/
   DEF NEW SHARED VAR hO-tt AS HANDLE NO-UNDO.
   DEF VAR hO-b    AS HANDLE NO-UNDO.
   DEF VAR hO-btt  AS HANDLE NO-UNDO.
      
	/*заполняем таблицу для УТ*/
   hO-b = BUFFER tt-plat350:HANDLE.
      
   CREATE TEMP-TABLE hO-tt.
   hO-tt:CREATE-LIKE("tt-plat350").
   hO-tt:TEMP-TABLE-PREPARE("xtt-plat350t").
      
   hO-btt = hO-tt:DEFAULT-BUFFER-HANDLE.
      
   FOR EACH tt-plat350:
     hO-btt:BUFFER-CREATE.
     hO-btt:BUFFER-COPY(hO-b).
   END.
   
   outHandle = STRING(hO-tt).
   
END.
     
/*{preview.i &filename = "'350.log'"}*/
