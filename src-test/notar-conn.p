/*
               Банковская интегрированная система БИСквит
    Copyright: 
     Filename: personomsk.p
      Comment: Ищем клиента в базе BANK и создаем в БИСе
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}
{intrface.get osyst}

DEFINE INPUT PARAMETER iTypeNotif AS CHAR.

DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE shFilial-defore AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFilList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInt1           AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2           AS INT64     NO-UNDO.

{justasec}

   mFilList  = "0000,0300,0500".
   shFilial-defore = shFilial.

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
  IF NOT CONNECTED("bank") THEN DO:
    CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
   &IF DEFINED( SESSION-REMOTE ) = 0 &THEN
     put screen col 1 row screen-lines + message-lines + 1
   				color bright-blink-normal "Подключение к МФР...".
   &ELSE
    mblodd_char_Tmp03 = ?.
    RUN bloddProgressBar( INPUT-OUTPUT mblodd_char_Tmp03, ?, ?, ?, "Подключение к МФР...", ? ).
   &ENDIF
  END.


DO mInt1 = 1 TO NUM-ENTRIES(mFilList):
   shFilial = ENTRY(mInt1,mFilList).

  

/*    RUN notar-csupd.p(DATE(1, 26, 2016)). */
    RUN notar-chek.p.
    IF iTypeNotif EQ "OP-ENTRY" THEN
    DO: 
	DO mInt2 = 1 TO 10:
        	RUN notar-csupd.p(TODAY - mInt2).
        END.
    /*
        RUN notar-csupd.p(TODAY - 7).
        RUN notar-csupd.p(TODAY - 6).
        RUN notar-csupd.p(TODAY - 5).
        RUN notar-csupd.p(TODAY - 4).
        RUN notar-csupd.p(TODAY - 3). 
        RUN notar-csupd.p(TODAY - 2). 
        RUN notar-csupd.p(TODAY - 1).
   */
        IF (TIME / 3600) GT 11.00 THEN
        DO:
            RUN notar-csupd.p(TODAY).
        END.
    END.
    ELSE
    DO:
        RUN notar-csupd1.p(iTypeNotif).
    END.    
    RUN notar-otpr.p.  
END.
  CATCH eAnyError AS Progress.Lang.Error:
    PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
  END CATCH.
  FINALLY:
    shFilial = shFilial-defore.
    IF CONNECTED("bank") THEN DISCONNECT bank.
    IF CONNECTED("bismfr") THEN DISCONNECT bismfr.
  END FINALLY.
END. /* DO */
hide message no-pause.


