DEFINE NEW GLOBAL SHARED VARIABLE h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.

{bislogin.i}

{globals.i}

{intrface.get crd}
{intrface.get pbase} 
{intrface.get date}
{intrface.get trans}
{intrface.get acct}
{intrface.get tmess}
{intrface.get instrum}
{intrface.get trans}
{intrface.get tparam}
{intrface.get rights}
{intrface.get data}
{intrface.get db2l}
{intrface.get count}
{intrface.get strng}
{intrface.get brnch}
{intrface.get refer}
{intrface.get cust}
{intrface.get osyst}
{intrface.get print}
{intrface.get prnvd}
{intrface.get kau}
{intrface.get parsr}
{intrface.get prsfn}
{intrface.get oldpr}
{intrface.get widg}
{intrface.get xclass}
{intrface.get rights}
{intrface.get osyst}
{intrface.get loan}
{intrface.get cdrep}
{intrface.get lv}
{intrface.get chwch}
{tmprecid.def}

{initstrp.i}

{form.def}
{tmprecid.def}
{g-trans.equ}
{ksh-defs.i}

{svarloan.def new global}   /* Переменные модуля кредиты и депозиты. */
{topkind.def}
  
{filial.pro}

DEFINE VARIABLE mFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDelim  AS INT64 NO-UNDO.
DEFINE VARIABLE ipPotok AS INT64 NO-UNDO.

ipPotok = INT64(OS-GETENV("POTOK")).
mFilial = OS-GETENV("FILIAL").
mDelim  = INT64(OS-GETENV("DELIM")).


put unformatted "potok - " ipPotok skip
"filial - " mFilial skip
"Delim - "  mDelim skip
"datetime - " string(today, "99.99.9999")  ' ' string(time,"HH:MM:SS") skip.


RUN LoadStartupInterfaces.

def var fname as char  init "/home2/bis/quit41d/log/autocred/"  no-undo.

fname = fname + "recalcl" + 
	string(year(today)) + 
	string(month(today),"99") + 
	string(day(today),"99") + "_" + 
	mFilial + "_" + 
	TRIM(STRING(ipPotok)) + ".log".

output to value (fname) APPEND UNBUFFERED.

DEF VAR in-fil AS CHAR NO-UNDO.

def var timestart as int no-undo.
def var timeend as int no-undo.
def var datebeg as date no-undo.
def var dateend as date no-undo.
def var timetime as int no-undo.
def var count as int no-undo init 0.

usr-printer = "+laser".

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :

    put unformatted "Запуск на " string(today) ' ,' STRING(NOW,"99/99/9999 HH:MM:SS") SKIP.

    timestart = time. 

    RUN DelConnectLink.
    RUN SetConnectLink (mFilial).
    RUN SetEnvironment (mFilial). /* контекст выбранного филиала */
/*
if ipPotok = 0 or ipPotok = 7 or ipPotok = 12 or ipPotok = 13 then do:
*/
    RUN recalcl.p((today - 1 ), ipPotok, mFilial, mDelim).  
/*    RUN recalcl.p(date("01/06/2018"), ipPotok, mFilial, mDelim). */

/*
end.
*/
    put unformatted 'Закончено ' + STRING(NOW,"99/99/9999 HH:MM:SS") skip.

    CATCH eAnyError AS Progress.Lang.Error:
        PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1).
    END CATCH.
    FINALLY:
        OUTPUT CLOSE.
    END FINALLY.
end.

{intrface.del}

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
