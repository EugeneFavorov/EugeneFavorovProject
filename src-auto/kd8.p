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

{svarloan.def new global}   /* ��६���� ����� �।��� � ��������. */
{topkind.def}
  
{filial.pro}

DEFINE VARIABLE mFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUser   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMenuId AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDelim  AS INT64 NO-UNDO.
DEFINE VARIABLE ipPotok AS INT64 NO-UNDO.


/*
#  1 - ��砫�� ��⮪
#  2 - ������  ��⮪
#  3 -           ��⮪
#  4 - ������⢮ ��⮪��
#  5 - 䨫���
#  6 - ᯨ᮪ �࠭���権 �१ ;
#  7 - �� ��直� ��砩
*/


IF NUM-ENTRIES(session:parameter) GE 7 THEN
ASSIGN
   ipPotok = INT64(ENTRY(3,session:parameter))
   mFilial = ENTRY(5,session:parameter)
   mDelim  = INT64(ENTRY(4,session:parameter))
   mMenuId = TRIM(ENTRY(1,ENTRY(6,session:parameter)," "))
   mUser   = "_" + ENTRY(7,session:parameter) + "_".
ELSE 
ASSIGN
   ipPotok = INT64(OS-GETENV("POTOK"))
   mFilial = OS-GETENV("FILIAL")
   mDelim  = INT64(OS-GETENV("DELIM"))
   mMenuId = "kd_".

RUN LoadStartupInterfaces.

def var fname as char  init "/home2/bis/quit41d/log/autocred/"  no-undo.

RUN Init-SysMes IN h_tmess ("","","").

/*message SUBSTITUTE("����� kd8. ��ࠬ����: ��⮪ &1 䨫��� &2 ����⥫� &3", ipPotok,mFilial,mDelim).*/


fname = fname + mMenuId + mUser +  
        string(year(today)) + 
        string(month(today),"99") + 
        string(day(today),"99") + "_" + 
        mFilial + "_" + 
        TRIM(STRING(ipPotok)) + ".log".

OS-DELETE VALUE(fname).

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

    /*put unformatted "����� �� " string(today)  SKIP.*/

    timestart = time. 

    RUN DelConnectLink.
    RUN SetConnectLink (mFilial).
    RUN SetEnvironment (mFilial). /* ���⥪�� ��࠭���� 䨫���� */

    RUN kd.p.

    put unformatted '<#�����祭�#>=' + STRING(NOW,"99/99/9999 HH:MM:SS") skip.

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
