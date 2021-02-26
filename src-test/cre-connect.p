DEFINE NEW GLOBAL SHARED VARIABLE h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.

{bislogin.i}

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}
{intrface.get osyst}

RUN LoadStartupInterfaces.

/* if OS-GETENV("HOSTNAME") <> "bis263" then return.*/

def var fname as char  init "/home2/bis/quit41d/imp-exp/0000/cre/"  no-undo.
def new shared stream vvs.

fname = fname + string(year(today)) + string(month(today),"99") + string(day(today),"99") + ".log".
output to value (fname) APPEND UNBUFFERED.

/*   поиск первого открытого опердня   */
{filial.pro}
DEF VAR vFilialID AS CHAR NO-UNDO.
vFilialID =  getThisUserXAttrValue("filial-id").
vFilialID = '0000'.
IF vFilialID NE ? AND vFilialID NE "" THEN DO:
    RUN SetConnectLink (vFilialID).
    RUN SetEnvironment (vFilialID). /*ShFilial = "0400". gend-date = today. * для присвоения темпорированных др */
END. ELSE DO:
    PUT UNFORMATTED 'у текущего пользователя не установлен др филиал ' SKIP.
END.

gend-date = today. /* для присвоения темпорированных др */

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
/*  RUN cre-export.p(DATE('01/01/2001'),DATE('01/04/2013')).
  RUN cre-export.p(DATE('02/04/2013'),DATE('01/10/2013')).
  RUN cre-export.p(DATE('02/10/2013'),DATE('01/01/2014')).
  RUN cre-export.p(DATE('02/01/2014'),DATE('15/03/2014')).
  RUN cre-export.p(DATE('16/03/2014'),DATE('01/01/2020')). */
  RUN cre-export.p(DATE('01/01/2001'),DATE('01/01/2020'),?).
  CATCH eAnyError AS Progress.Lang.Error:
    PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
  END CATCH.
  FINALLY:
/*  OUTPUT CLOSE. */
  END FINALLY.
END. /* DO */
/* kam */
vFilialID = '0300'.
IF vFilialID NE ? AND vFilialID NE "" THEN DO:
    RUN SetConnectLink (vFilialID).
    RUN SetEnvironment (vFilialID). /*ShFilial = "0400". gend-date = today. * для присвоения темпорированных др */
END. ELSE DO:
    PUT UNFORMATTED 'у текущего пользователя не установлен др филиал ' SKIP.
END.

gend-date = today. /* для присвоения темпорированных др */

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
  RUN cre-export.p(DATE('01/01/2001'),DATE('01/01/2020'),?).
  CATCH eAnyError AS Progress.Lang.Error:
    PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
  END CATCH.
  FINALLY:
/*  OUTPUT CLOSE. */
  END FINALLY.
END. /* DO */

/* kam */
vFilialID = '0500'.
IF vFilialID NE ? AND vFilialID NE "" THEN DO:
    RUN SetConnectLink (vFilialID).
    RUN SetEnvironment (vFilialID). /*ShFilial = "0400". gend-date = today. * для присвоения темпорированных др */
END. ELSE DO:
    PUT UNFORMATTED 'у текущего пользователя не установлен др филиал ' SKIP.
END.

gend-date = today. /* для присвоения темпорированных др */

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
  RUN cre-export.p(DATE('01/01/2001'),DATE('01/01/2020'),?).
  CATCH eAnyError AS Progress.Lang.Error:
    PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
  END CATCH.
  FINALLY:
/*  OUTPUT CLOSE. */
  END FINALLY.
END. /* DO */

OUTPUT CLOSE.


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
