DEFINE NEW GLOBAL SHARED VARIABLE h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.

/*
{bislogin.i}
*/

{globals.i}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get rights}
{intrface.get osyst}
{tmprecid.def}

/*
RUN LoadStartupInterfaces.
*/
/* if OS-GETENV("HOSTNAME") <> "bis263" then return.*/

def var fname as char  init ""  no-undo.
def new shared stream vvs.

def buffer bbbloan for loan.
fname = fname + string(year(today)) + string(month(today),"99") + string(day(today),"99") + ".log".
output to value (fname) APPEND UNBUFFERED.

/*   ���� ��ࢮ�� ����⮣� ���भ�   * /
/*ncfil = "0400". */   /* ��ࠬ���� ������ 䨫���� ���� */
{filial.pro}
DEF VAR vFilialID AS CHAR NO-UNDO.
vFilialID =  getThisUserXAttrValue("filial-id").
vFilialID = /* STRING(shFilial). */ '0400'.
IF vFilialID NE ? AND vFilialID NE "" THEN DO:
    RUN SetConnectLink (vFilialID).
    RUN SetEnvironment (vFilialID). /*ShFilial = "0400". gend-date = today. * ��� ��᢮���� ⥬���஢����� �� */
END. ELSE DO:
    PUT UNFORMATTED '� ⥪�饣� ���짮��⥫� �� ��⠭����� �� 䨫��� ' SKIP.
END.
*/
/* ����᪠�� �ନ஢���� xml ��� �뤥������ ������஢ */
DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE :
 for each tmprecid no-lock:
  RUN cre-export.p(DATE('01/01/2001'),DATE('01/01/2020'), tmprecid.id).
 end.
  CATCH eAnyError AS Progress.Lang.Error:
    PUT UNFORMATTED "CATCH: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
  END CATCH.
  FINALLY:
    OUTPUT CLOSE.
  END FINALLY.
END. /* DO */

/*�� �ᯮ������*/
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
