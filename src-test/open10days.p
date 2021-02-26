DEFINE NEW GLOBAL SHARED VARIABLE h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.

{bislogin.i}

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}
{intrface.get osyst}
{intrface.get date}

RUN LoadStartupInterfaces.

/*if OS-GETENV("HOSTNAME") <> "bis263" then return.*/

def var fname as char  init "" /*"/home2/bis/quit41d/imp-exp/0400/cre/"*/  no-undo.
def new shared stream vvs.
def var isOk as log no-undo.

fname = fname + "open10days"  /* string(year(today)) + string(month(today),"99") + string(day(today),"99")*/ + ".log".
output to value (fname) APPEND UNBUFFERED.

DEFINE VARIABLE mHoliDay AS LOGICAL NO-UNDO.

/*   поиск первого открытого опердня   */
/*ncfil = "0400". */   /* параметры какого филиала брать */
{filial.pro}
DEF VAR vFilialID AS CHAR NO-UNDO.
vFilialID =  getThisUserXAttrValue("filial-id").
IF vFilialID NE ? AND vFilialID NE "" THEN 
DO:
    RUN SetConnectLink (vFilialID).
    RUN SetEnvironment (vFilialID). /*ShFilial = "0400". gend-date = today. * для присвоения темпорированных др */
END. 
ELSE
DO:
    PUT UNFORMATTED 'open10days: у текущего пользователя не установлен др филиал ' SKIP.
END.

DEF VAR dt AS DATE NO-UNDO.

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE:
   dt = TODAY.
   PUT UNFORMATTED "open10days: сегодня " + string( dt) SKIP.
   DO WHILE dt <= TODAY + 10 ON ERROR UNDO, THROW:
      FIND FIRST op-date WHERE op-date.op-date EQ dt NO-LOCK NO-ERROR.
      mHoliDay = {holiday.i dt}.
      PUT UNFORMATTED "AVAIL(op-date) = "  AVAIL(op-date) "; " "mHoliDay = " mHoliDay SKIP.
   	IF NOT AVAIL op-date AND dt LE today + 5 AND {holiday.i dt} THEN 
   	DO:
         PUT UNFORMATTED "open10days: меняем " + string( dt) + " на рабочий." SKIP.
         RUN ChangeHoliday in h_date( dt, "", "", OUTPUT isOk).
      END.
   	IF NOT AVAIL op-date AND {holiday.i dt} EQ FALSE THEN 
   	DO ON ERROR UNDO, THROW:
         PUT UNFORMATTED "open10days: открываем " + string( dt) SKIP.
         CREATE op-date.
         ASSIGN op-date.op-date = dt.
         VALIDATE op-date.
      END.
      dt = dt + 1.
   END.
   CATCH eAnyError AS Progress.Lang.Error:
      PUT UNFORMATTED "open10days CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
   END CATCH.
   FINALLY:
      OUTPUT CLOSE.
   END FINALLY.
END. /* DO */

PROCEDURE LoadStartupInterfaces:
   DEFINE VAR vLibs AS CHARACTER NO-UNDO.
   DEFINE VAR vInt  AS INT64   NO-UNDO.
   DEFINE VAR vName AS CHARACTER NO-UNDO.
   DEFINE VAR vHdl  AS HANDLE    NO-UNDO.
   vLibs = {&STARTUP_INTERFACES}.
   IF {assigned vLibs} THEN DO:
      DO vInt = 1 TO NUM-ENTRIES(vLibs):
         vName = TRIM(ENTRY(vInt,vLibs)).
         vHdl = ?.
         RUN GetHIntrface (vName,OUTPUT vHdl).
         IF VALID-HANDLE(vHdl)
            THEN NEXT.
         RUN StartProc(vName,THIS-PROCEDURE,OUTPUT vHdl).
      END.
   END.
END PROCEDURE.
