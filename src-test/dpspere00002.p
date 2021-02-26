/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: dpsnach30.p 
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created:
     Modified:
*/
{globals.i}

{intrface.get crd}
{intrface.get pbase} 
{intrface.get date}
{intrface.get trans}
{intrface.get acct}
{intrface.get tmess}
{intrface.get instrum}
{intrface.get trans}
{intrface.get xclass}
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
{intrface.get widg}
{initstrp.i}

{form.def}
{tmprecid.def}
{g-trans.equ}
{ksh-defs.i}

/*DEFINE INPUT PARAMETER iFilial AS CHARACTER NO-UNDO.*/


DEFINE SHARED VARIABLE auto     AS LOGICAL NO-UNDO.
DEFINE VARIABLE shFilial-defore AS CHARACTER NO-UNDO.
DEFINE VARIABLE auto-defore     AS LOGICAL NO-UNDO.
DEFINE VARIABLE mOpDate         AS DATE       NO-UNDO.
DEFINE VARIABLE mTmplID         AS INT64    NO-UNDO.
DEFINE VARIABLE mProcHdl        AS HANDLE     NO-UNDO.
DEFINE VARIABLE mQTemplate      AS HANDLE     NO-UNDO.
DEFINE VARIABLE mDebugLevel     AS INT64    NO-UNDO.
DEFINE VARIABLE mOpkind         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBreak          AS INT64    NO-UNDO.

DEFINE VARIABLE mCurrentDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrentFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTMPFile        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLogDirDay      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLogDirTMP      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOSCommamd      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFilList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTranList       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInt1           AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2           AS INT64     NO-UNDO.


ASSIGN
   mFilList  = "0000"
   mTranList = "���ᣮ".

ASSIGN
   auto-defore     = auto
   auto            = yes
   shFilial-defore = shFilial.

/*IF TODAY NE LastMonDate(TODAY) THEN            */
/*DO:                                            */
/*/*   RUN Fill-SysMes IN h_tmess ("", "", "0",*/*/
/*/*      "�� ��᫥���� ���� �����").         */*/
/*   RETURN.                                     */
/*END.                                           */

/*ᤥ���� �஢��� �� ��ன �����*/

/*FilList ENTRY(mInt1,mFilList)*/
DO mInt1 = 1 TO NUM-ENTRIES(mFilList):
   shFilial = ENTRY(mInt1,mFilList).
   mCurrentFile = ENTRY(mInt1,mFilList) + ".txt". 
   OUTPUT TO VALUE(mCurrentFile). 
   PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") SKIP.
   OUTPUT CLOSE. 
   FILE-INFO:FILE-NAME = "./" + mCurrentFile.
   mCurrentDir = RIGHT-TRIM(FILE-INFO:FULL-PATHNAME,"/" + mCurrentFile).
   
   mLogDirDay = "/home2/bis/quit41d/log/dps/" + ENTRY(mInt1,mFilList) + "/".
   mLogDirDay = mLogDirDay + STRING(YEAR(TODAY),"9999") + 
                             STRING(MONTH(TODAY),"99") + 
                             STRING(DAY(TODAY),"99").
   FILE-INFO:FILE-NAME = mLogDirDay.
   IF FILE-INFO:FULL-PATHNAME = ? THEN
   DO:
      mOSCommamd = "mkdir " + mLogDirDay.
      OS-COMMAND SILENT VALUE(mOSCommamd).
      FILE-INFO:FILE-NAME = mLogDirDay.
      IF FILE-INFO:FULL-PATHNAME = ? THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0",
            "�訡�� ᮧ����� ��⠫���" + mLogDirDay).
      END.
   END.
   
	/*TranList ENTRY(mInt2,mTranList) */
   DO mInt2 = 1 TO NUM-ENTRIES(mTranList):
      mTMPFile = ENTRY(mInt1,mFilList) + ".tmp". 
      OUTPUT TO VALUE(mTMPFile). 
      PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      mLogDirTMP = mLogDirDAY + "/" + ENTRY(mInt2,mTranList).
      FILE-INFO:FILE-NAME = mLogDirTMP.
      IF FILE-INFO:FULL-PATHNAME = ? THEN
      DO:
         mOSCommamd = "mkdir " + mLogDirTMP.
         OS-COMMAND SILENT VALUE(mOSCommamd).
         FILE-INFO:FILE-NAME = mLogDirTMP.
         IF FILE-INFO:FULL-PATHNAME = ? THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "0",
               "�訡�� ᮧ����� ��⠫���" + mLogDirTMP).
         END.
      END.
      
      mOSCommamd = "cd " + mCurrentDir.
      OS-COMMAND SILENT VALUE(mOSCommamd).

      RUN RunTransaction2 IN THIS-PROCEDURE(ENTRY(mInt2,mTranList)) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN
      DO:
         MESSAGE "�訡�� �믮������ �࠭���樨 " + ENTRY(mInt2,mTranList)
            VIEW-AS ALERT-BOX ERROR.
            pick-value = "�訡�� �믮������ �࠭���樨 " + ENTRY(mInt2,mTranList).
      END.
      
      mOSCommamd = "cp *.tmp " + mLogDirTMP.
      OS-COMMAND SILENT VALUE(mOSCommamd).
      mOSCommamd = "cp ./sysmess.log " + mLogDirTMP + "/sysmess.log".
      OS-COMMAND SILENT VALUE(mOSCommamd).
      mOSCommamd = "rm ./*.tmp".
      OS-COMMAND SILENT VALUE(mOSCommamd).
   END.
   
   mOSCommamd = "cp " + "./" + mCurrentFile + " /home2/bis/quit41d/log/dps/" + mCurrentFile.
   OS-COMMAND SILENT VALUE(mOSCommamd).
END.


/**/
ASSIGN
   auto     = auto-defore
   shFilial = shFilial-defore.


RETURN.

/*------------------------------------------------------------------------------
  Purpose:     ����᪠�� �࠭����� � 㪠����� �����
  Parameters:  iOpkind  - ��� �࠭���樨
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE RunTransaction2:
   DEFINE INPUT  PARAMETER iOpkind AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vRetVal       AS CHARACTER  NO-UNDO.
   
   DEFINE VARIABLE vOpDay       AS date  NO-UNDO.

   FIND FIRST op-kind WHERE op-kind.op-kind = iOpkind NO-LOCK NO-ERROR.

   IF NOT AVAILABLE op-kind THEN
   DO:
      RUN Fill-SysMes("","","-1","�࠭����� � ����� [" + iOpkind +
                      "] �� �������.").
      RETURN ERROR.
   END.

   IF NOT SearchPFile(op-kind.proc)  THEN
   DO:
      RUN Fill-SysMes("","","-1","�� ������� ��楤�� ����᪠ �࠭���樨 [" +
                      iOpkind + "]").
      RETURN ERROR.
   END.

   IF NOT GetSurrPermission("op-kind",op-kind.op-kind,"run") THEN
   DO:
      RUN Fill-SysMes("","","-1","���짮��⥫�" + CAPS(USERID("bisquit")) +
                      " �� ����� �ࠢ ��� ����᪠ �࠭���樨 [" + iOpkind + "]").
      RETURN ERROR.
   END.

                        /* ����� ��⮤� �஢�ન ���������� ����᪠ �࠭���樨 */
   pick-value = "".
   RUN RunClassMethod (op-kind.Class-Code,
                       "BeforeRun",
                       "","",?,
                       CHR(1) +      /* ����� CHR(1) ���� ����⠢���� 
                                        ���ଥ��� �맮�� �� ����奬� */
                       "op-kind," + op-kind.op-kind).
                        /* �᫨ ��⮤ �����頥� �����⮥ ���祭�� � ��६����� pick-value,
                        ** � ����� ����頥��� */
   IF {assigned pick-value} THEN
   DO:
                        /* �뢮��� ᮮ�饭�� �� �訡��. */
      RUN Fill-SysMes IN h_tmess ("", "", "-1", pick-value).
      RETURN ERROR.
   END.

   mBreak = 0.
   vOpDay = TODAY.

   TR:
   DO ON ERROR UNDO TR,RETURN ERROR
      ON STOP  UNDO TR,RETURN ERROR:
      /* Commented BY KSV: ���������� ���⥪�� ⥪�饩 �࠭���樨 */
      {&PUSH_TSTACK}

      RUN VALUE(op-kind.proc + ".p") (vOpday,RECID(op-kind)) NO-ERROR.

      vRetVal = RETURN-VALUE.
      
      /*MESSAGE vOpDay "," vRetVal
      VIEW-AS ALERT-BOX.*/
      /* Commented BY KSV: ����⠭�������� ���⥪�� ⥪�饩 �࠭���樨 */
      {&POP_TSTACK}

      CASE mBreak:
         WHEN {&BREAK-ALL-UNDO} OR WHEN {&BREAK-ALL-NOUNDO} THEN
            IF VALID-HANDLE(mProcHdl) THEN
               RUN SetBreak IN mProcHdl (mBreak) NO-ERROR.
         WHEN {&BREAK-CUR-UNDO} THEN UNDO,LEAVE.
      END CASE.

      IF vRetVal = {&RET-ERROR} THEN UNDO TR,RETURN ERROR.
   END.  /* END OF TR BLOCK */

   mBreak = 0.

END PROCEDURE.
