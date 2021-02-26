&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loan NO-UNDO LIKE loan
       FIELD op-kind-allowed AS CHARACTER /* op-kind-allowed */
       FIELD agent-cat AS CHARACTER /* agent-cat */
       FIELD agent-id AS INT64 /* agent-id */
       FIELD RevRef2 AS CHARACTER /* RevRef2 */
       FIELD round AS LOGICAL /* round */
       FIELD TermKey AS CHARACTER /* TermKey */
       FIELD TicketNumber AS CHARACTER /* TicketNumber */
       FIELD UniformBag AS CHARACTER /* UniformBag */
       FIELD int-offset AS CHARACTER /* int-offset */
       FIELD IntAcct AS CHARACTER /* IntAcct */
       FIELD list_type AS CHARACTER /* list_type */
       FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
       FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
       FIELD OblAcct AS CHARACTER /* OblAcct */
       FIELD op-date AS DATE /* op-date */
       FIELD Op-kind_Acct AS CHARACTER /* Op-kind_Acct */
       FIELD Partition AS CHARACTER /* Partition */
       FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
       FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
       FIELD rate-list AS CHARACTER /* rate-list */
       FIELD rel_type AS CHARACTER /* rel_type */
       FIELD ReplDate AS DATE /* ReplDate */
       FIELD RevRef1 AS CHARACTER /* RevRef1 */
       FIELD rewzim$ AS CHARACTER /* ����� */
       FIELD sindkred$ AS LOGICAL /* �����। */
       FIELD BankCust AS CHARACTER /* BankCust */
       FIELD Bfnc AS CHARACTER /* Bfnc */
       FIELD CallAcct AS CHARACTER /* CallAcct */
       FIELD cred-offset AS CHARACTER /* cred-offset */
       FIELD dateend AS DATE /* dateend */
       FIELD DTKind AS CHARACTER /* DTKind */
       FIELD DTType AS CHARACTER /* DTType */
       FIELD Exec_D AS LOGICAL /* Exec_D */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD akt_vzv$ AS CHARACTER /* ���_��� */
       FIELD grup_dog$ AS CHARACTER /* ���_��� */
       FIELD datasogl$ AS DATE /* ��⠑��� */
       FIELD data_uar$ AS CHARACTER /* ���_��� */
       FIELD dop_proc$ AS CHARACTER /* ���_��� */
       FIELD dosroka$ AS CHARACTER /* ������� */
       FIELD igndtokwc$ AS LOGICAL /* ����⎪� */
       FIELD nesno$ AS CHARACTER /* ��᭎ */
       FIELD ovrpr$ AS INT64 /* ����� */
       FIELD ovrstop$ AS INT64 /* ����⮯ */
       FIELD okrugsum$ AS LOGICAL /* ���㣑� */
       FIELD general-mark AS LOGICAL /* general-mark */
       FIELD loan-allowed AS CHARACTER /* loan-allowed */
       FIELD details AS CHARACTER /* details */
       FIELD can-redraw-mark AS LOGICAL /* can-redraw-mark */
       FIELD single-mark AS LOGICAL /* single-mark */
       FIELD create-date AS DATE /* create-date */
       FIELD drower-cat AS CHARACTER /* drower-cat */
       FIELD drower-id AS INT64 /* drower-id */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-loan" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: f-trust.p
      Comment: ��ଠ ����� ����७����
   Parameters:
         Uses:
      Used by:
      Created: 21.12.2004 ����
     Modified: 01.04.2009 12:09 BIS      <comment>
*/
/*          This file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Commented by KSV: ����� 蠡��� �।�����祭 ��� ᮧ����� �࠭��� ���
** �����⢫��饩 ����������, ��������� � ��ᬮ�� ���ଠ樨 �� ��ꥪ�
** ����奬� �������� ��� �����।�⢥����� ���饭�� � ���� ������.
**
** ���� �� ᮧ����� �࠭��� ���:
**    0. ����ன� PROPATH AppBuilder �� SRC ��⠫�� ��������. �����, �⮡�
**       �㦥��� ��⠫��� PROGRESS ��室����� ��᫥ ��⠫��� SRC/TOOLS.
**       ���������� � ���� ������ ��������. 
**    1. �롥�� �㭪� ���� AppBuilder Tools - Procedure Settings. ����� 
**       ������ ������ Temp-Table Definition, � ���襬�� ������� ������
**       ������ BISQUIT � �롥�� ����� ����奬�, ��ꥪ� ���ண� �㤥�
**       ��ࠡ��뢠���� �ମ�. �� �᭮�� ��࠭���� ����� � ��� ���������
**       ������� �६����� ⠡��� ��� ��� ��࠭���� �����, ⠪ � ���
**       ��� ���ॣ�஢����� �� ��� ����ᮢ.
**    2. �������� ���� �६����� ⠡��� �� �३��. ��� �裡 ������ � 
**       ����� �� �६����� ⠡���� � �ଥ ᢮��� ���� 饫���� �� ������ 
**       Database Field �ࠢ�� ������ ��� � � ���襬�� ���� �롥�� 
**       �㭪� Bisquit.
**       ��  ����� ᮧ���� ᯥ樠��� ���� ࠧ����⥫�, ��� �⮣� ����室��� 
**       ᮧ���� FILL-IN c �����䨪��஬ SEPARATOR# (��� # - �� �᫮ ��
**       2, ���� FILL-IN ����� �����䪠�� ��� �����) � ���ਡ�⮬ 
**       VIES-AS TEXT. � ������� ࠧ����⥫�� �� ����� ���㠫쭮 �뤥����
**       ��㯯� �����.
**    3. ��ꥤ���� ���� � ᯨ᪨ � ����ᨬ��� �� ⮣� � ����� �� ०����
**       ���� ������ ���� ����㯭� ��� ।���஢����. ��� ���������� ����
**       � ᯨ᮪ � ������� ��� ��ਡ�⮢ ������ ������ Advanced � ���⠢�� 
**       ����� � ����� LIST-1, LIST-2 ��� LIST-3. �����祭�� ᯨ᪮�:
**       -  LIST-1 - ���� ����㯭� ��� ।���஢���� � ०��� ���������� 
**                   ����� 
**       -  LIST-2 - ���� ����㯭� ��� ।���஢���� � ०��� ।���஢���� 
**                   ����� 
**       -  LIST-3 - ���� ����㯭� ��� ।���஢���� � ०��� ��ᬮ��. 
**                   (���筮 �� ����, �⮡ࠦ���� � ������ EDITOR ��� 
**                   ����饭�� �� ��������� ��ᯮ������ ��ਡ�⮬ READ-ONLY)
**       -  LIST-4 - ���� ��� ������ ��ਡ�� �ଠ� ��।������ � �ଥ.
**                   ��� ��㣨� �� ���������� �� ����奬�. 
**    4. ����஫� �� ���祭��� ����� ������ ���� ��।���� �� �ਣ��� LEAVE 
**       ����, ����� � ��砥 ��ᮮ⢥��⢨� ���祭�� �ॡ㥬��� ������ 
**       �������� ���祭�� {&RET-ERROR}.
**       �ࠢ��쭠� ��������� �ਣ���:

   .......

   IF <������> THEN
   DO:
      MESSAGE '......'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   APPLY "LEAVE" TO FRAME {&MAIN-FRAME}. /* �⠭���⭠� �஢�ઠ */
   IF RETURN-VALUE EQ {&RET-ERROR}
      THEN RETURN NO-APPLY.

**    5. ��� �롮� ���祭�� ���� �� ᯨ᪠ ������ ���� ��।��� �ਣ��� F1 ����
**       (�� ����� � �ਣ��஬ �� ᮡ�⨥ HELP � TTY - �� ࠧ�� ᮡ���)
**    6. �᫨ � �ଥ ��������� ������� �� �⭮��騥�� � ���� �६�����
**       ⠡����, ���ਬ�� ������, �� ����� �.�. ����㯭� � ०���� 
**       ।���஢���� � ���������� ������� �� � ᯨ᮪ LIST-4.
**    7. ����� ⮭��� ����ன�� ��������� ��� �� ����� 㪠���� � ��楤��
**       LocalEnableDisable, ����� �㤥� ��뢠����, � c��砥 �᫨ ���
**       ��।�����, � ���� EnableDisable.
**    8. �ᯮ���� ��楤��� LocalSetObject, ����� �㤥� ��뢠����,
**       � c��砥 �᫨ ��� ��।�����, ��। ������� ������ � ��.
**    9. ��� ��।�� ᯥ���᪨� ��ࠬ��஢ ��楤�� �࠭��� ���
**       ��ᯮ����� �㭪�ﬨ ������⥪� PP-TPARA.P
**   10. ���ᠭ�� ��६����� ��� �ࠢ����� �࠭��� �ମ� ��室���� � ᥪ樨
**       Definitions ������⥪� bis-tty.pro 
**   11. ���ᠭ�� TEMP-TABL'��
*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE MAIN-FRAME fMain

{globals.i}
{intrface.get xclass} 
{intrface.get tparam}
{intrface.get loan}
{intrface.get cdrep}
{intrface.get trust}
{intrface.get count} /* �⮡� ࠡ�⠫ GetCounterNextValue */

/* ���᪮�����஢��� � ��砥 �맮�� �� NAVIGATE.CQR
{navigate.cqr
   ...
   &UseBisTTY=YES
   &edit=bis-tty.ef
   ...
}
   �᫨ ��।����� &UseBisTTY - � ��뫪� �� ���������� ⠡���� ���孥�� �����
�㤥� �࠭����� � ��६����� IInstance.
   �᫨ ��।����� &InstanceFile - � �㤥� ��।����� � ��������� ����᪠�
TEMP-TABLE tt-instance LIKE {&InstanceFile}

&GLOBAL-DEFINE UseBisTTY 
&GLOBAL-DEFINE InstanceFile ���_�������_��������_���_��������_������
*/

/* ��� ��ᬮ�� ����祭��� mInstance � GetObject */
/* &GLOBAL-DEFINE DEBUG-INSTANCE-GET */

/* ��� ��ᬮ�� mInstance ��। ������� � ���� � SetObject */
/* &GLOBAL-DEFINE DEBUG-INSTANCE-SET */

/* ����᫮���� ����祭��\�⪫�祭�� �맮�� xattr-ed 
(���� �� ��뢠���� �� ����稥 ������������� ��易⥫��� ४����⮢ */
/*
&GLOBAL-DEFINE XATTR-ED-OFF
&GLOBAL-DEFINE XATTR-ED-ON 
*/

DEF VAR mAgentId  AS INT64  NO-UNDO INIT ?.
DEF VAR mDrowerId AS INT64  NO-UNDO INIT ?.
DEF VAR mCustId   AS INT64  NO-UNDO INIT ?.
DEF VAR mParent   AS CHAR NO-UNDO.
DEF VAR mDate     AS DATE NO-UNDO.

DEFINE VAR mMinDate AS DATE NO-UNDO INIT ?.
DEFINE VAR mMaxDate AS DATE NO-UNDO INIT ?.

DEFINE VAR mFlagCloseDate AS LOGICAL INIT NO NO-UNDO.

DEF TEMP-TABLE tFields NO-UNDO
   FIELD tHandle    AS HANDLE
   FIELD tSensitive AS LOG 
   FIELD tVisible   AS LOG
   FIELD tName      AS CHAR
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-loan

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-loan.create-date ~
tt-loan.loan-status tt-loan.doc-num tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id ~
tt-loan.agent-id tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-loan.create-date ~
tt-loan.loan-status tt-loan.doc-num tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id ~
tt-loan.agent-id tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-loan
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-loan
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loan SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-loan SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loan
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loan


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define ENABLED-TABLES tt-loan
&Scoped-define FIRST-ENABLED-TABLE tt-loan
&Scoped-Define ENABLED-OBJECTS separator2 fRedraw separator1 fProxy ~
fCliName1 
&Scoped-Define DISPLAYED-FIELDS tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.general-mark tt-loan.loan-allowed ~
tt-loan.close-date tt-loan.comment 
&Scoped-define DISPLAYED-TABLES tt-loan
&Scoped-define FIRST-DISPLAYED-TABLE tt-loan
&Scoped-Define DISPLAYED-OBJECTS separator2 fRedraw separator1 fProxy ~
fCliName1 fCliName2 fCliName3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.single-mark ~
tt-loan.can-redraw-mark tt-loan.loan-allowed tt-loan.close-date ~
tt-loan.comment 
&Scoped-define List-2 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.end-date tt-loan.single-mark tt-loan.can-redraw-mark ~
tt-loan.loan-allowed tt-loan.close-date tt-loan.comment 
&Scoped-define List-3 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.loan-allowed tt-loan.close-date ~
tt-loan.comment 
&Scoped-define List-4 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num ~
tt-loan.cont-type tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id ~
tt-loan.open-date tt-loan.end-date tt-loan.close-date tt-loan.comment 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fCliName1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 46 BY 1
     &ELSE SIZE 46 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fCliName2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 46 BY 1
     &ELSE SIZE 46 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fCliName3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 46 BY 1
     &ELSE SIZE 46 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fProxy AS CHARACTER FORMAT "x(22)" 
     LABEL "�� �᭮�����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U INITIAL "---------------------------------------------------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U INITIAL "-------------------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fRedraw AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
     &ELSE SIZE 4 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loan SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 1
          &ELSE AT ROW 10 COL 1 &ENDIF NO-LABEL
     fRedraw
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 25
          &ELSE AT ROW 2 COL 25 &ENDIF
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF NO-LABEL
     tt-loan.create-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 18 COLON-ALIGNED
          &ELSE AT ROW 1 COL 18 COLON-ALIGNED &ENDIF HELP
          "��� ��ଫ���� ����७����"
          LABEL "��� ��ଫ����" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.doc-num
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 38 COLON-ALIGNED
          &ELSE AT ROW 1 COL 38 COLON-ALIGNED &ENDIF HELP
          "�����"
          LABEL "�����" FORMAT "X(25)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-loan.loan-status
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 60 COLON-ALIGNED
          &ELSE AT ROW 1 COL 60 COLON-ALIGNED &ENDIF HELP
          "����� ����७����"
          LABEL "������" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     fProxy
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 43 COLON-ALIGNED
          &ELSE AT ROW 2 COL 43 COLON-ALIGNED &ENDIF HELP
          "����७�����, �� �᭮����� ���ன �뤥��� ������ ����७�����"
     tt-loan.cont-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18 COLON-ALIGNED
          &ELSE AT ROW 3 COL 18 COLON-ALIGNED &ENDIF HELP
          "��� �������1."
          LABEL "��� ����७����" FORMAT "x(42)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 42 BY 1
          &ELSE SIZE 42 BY 1 &ENDIF
     tt-loan.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 18 COLON-ALIGNED
          &ELSE AT ROW 4 COL 18 COLON-ALIGNED &ENDIF HELP
          "���浪��� ����� ������ (������ F1 ��� �롮�)"
          LABEL "������" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCliName1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 29 COLON-ALIGNED
          &ELSE AT ROW 4 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     tt-loan.drower-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 18 COLON-ALIGNED
          &ELSE AT ROW 5 COL 18 COLON-ALIGNED &ENDIF HELP
          "��� ������, �믨ᠢ襣� ����७�����"
          LABEL "�����⥫�" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCliName2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 29 COLON-ALIGNED
          &ELSE AT ROW 5 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     tt-loan.agent-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 18 COLON-ALIGNED
          &ELSE AT ROW 6 COL 18 COLON-ALIGNED &ENDIF HELP
          "��� �।�⠢�⥫� ����७����"
          LABEL "�।�⠢�⥫�" FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     fCliName3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 29 COLON-ALIGNED
          &ELSE AT ROW 6 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     tt-loan.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 15 COLON-ALIGNED
          &ELSE AT ROW 7.99 COL 15 COLON-ALIGNED &ENDIF HELP
          "��� ��砫� ����⢨� ����७����"
          LABEL "��砫�" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 15 COLON-ALIGNED
          &ELSE AT ROW 9 COL 15 COLON-ALIGNED &ENDIF HELP
          "��� ����砭�� ����⢨� ����७����"
          LABEL "����砭��" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.single-mark
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 72
          &ELSE AT ROW 8 COL 72 &ENDIF HELP
          "�⬥⪠ � ࠧ���� ����७����"
          LABEL ""
          VIEW-AS TOGGLE-BOX
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-loan.can-redraw-mark
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 72
          &ELSE AT ROW 9 COL 72 &ENDIF HELP
          "�⬥⪠ � �ࠢ� ��।�����"
          LABEL ""
          VIEW-AS TOGGLE-BOX
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-loan.general-mark
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 72
          &ELSE AT ROW 11 COL 72 &ENDIF HELP
          "�⬥⪠ � ����ࠫ쭮� ����७����"
          LABEL ""
          VIEW-AS TOGGLE-BOX
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-loan.loan-allowed
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 14
          &ELSE AT ROW 12 COL 14 &ENDIF NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 64 BY 4
          &ELSE SIZE 64 BY 4 &ENDIF
     tt-loan.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 15 COLON-ALIGNED
          &ELSE AT ROW 11 COL 15 COLON-ALIGNED &ENDIF HELP
          "��� ������� �������"
          LABEL "��� �������" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan.comment
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 14
          &ELSE AT ROW 15.99 COL 14.01 &ENDIF HELP
          "������ �������਩." NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 64 BY 4
          &ELSE SIZE 64 BY 4 &ENDIF
     "�⬥⪠ � ࠧ���� ����७����" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 30 BY 1
          &ELSE SIZE 30 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 41
          &ELSE AT ROW 8 COL 41 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     "�⬥⪠ � �ࠢ� ��।�����" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 44
          &ELSE AT ROW 9 COL 44 &ENDIF
     "����ࠫ쭠� ����७�����" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
          &ELSE SIZE 24 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 47
          &ELSE AT ROW 11 COL 47 &ENDIF
     "� ���浪� ��।�����" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 21 BY 1
          &ELSE SIZE 21 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 3
          &ELSE AT ROW 2 COL 3 &ENDIF
     "������:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 7 BY 1
          &ELSE SIZE 7 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 2
          &ELSE AT ROW 12 COL 2 &ENDIF
     "�४�饭��" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 2
          &ELSE AT ROW 17 COL 2 &ENDIF
     "�᭮�����" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 2
          &ELSE AT ROW 16 COL 2 &ENDIF
     "����⢨�:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 2
          &ELSE AT ROW 18 COL 2 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loan T "?" NO-UNDO bisquit loan
      ADDITIONAL-FIELDS:
          FIELD op-kind-allowed AS CHARACTER /* op-kind-allowed */
          FIELD agent-cat AS CHARACTER /* agent-cat */
          FIELD agent-id AS INT64 /* agent-id */
          FIELD RevRef2 AS CHARACTER /* RevRef2 */
          FIELD round AS LOGICAL /* round */
          FIELD TermKey AS CHARACTER /* TermKey */
          FIELD TicketNumber AS CHARACTER /* TicketNumber */
          FIELD UniformBag AS CHARACTER /* UniformBag */
          FIELD int-offset AS CHARACTER /* int-offset */
          FIELD IntAcct AS CHARACTER /* IntAcct */
          FIELD list_type AS CHARACTER /* list_type */
          FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
          FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
          FIELD OblAcct AS CHARACTER /* OblAcct */
          FIELD op-date AS DATE /* op-date */
          FIELD Op-kind_Acct AS CHARACTER /* Op-kind_Acct */
          FIELD Partition AS CHARACTER /* Partition */
          FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
          FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
          FIELD rate-list AS CHARACTER /* rate-list */
          FIELD rel_type AS CHARACTER /* rel_type */
          FIELD ReplDate AS DATE /* ReplDate */
          FIELD RevRef1 AS CHARACTER /* RevRef1 */
          FIELD rewzim$ AS CHARACTER /* ����� */
          FIELD sindkred$ AS LOGICAL /* �����। */
          FIELD BankCust AS CHARACTER /* BankCust */
          FIELD Bfnc AS CHARACTER /* Bfnc */
          FIELD CallAcct AS CHARACTER /* CallAcct */
          FIELD cred-offset AS CHARACTER /* cred-offset */
          FIELD dateend AS DATE /* dateend */
          FIELD DTKind AS CHARACTER /* DTKind */
          FIELD DTType AS CHARACTER /* DTType */
          FIELD Exec_D AS LOGICAL /* Exec_D */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD akt_vzv$ AS CHARACTER /* ���_��� */
          FIELD grup_dog$ AS CHARACTER /* ���_��� */
          FIELD datasogl$ AS DATE /* ��⠑��� */
          FIELD data_uar$ AS CHARACTER /* ���_��� */
          FIELD dop_proc$ AS CHARACTER /* ���_��� */
          FIELD dosroka$ AS CHARACTER /* ������� */
          FIELD igndtokwc$ AS LOGICAL /* ����⎪� */
          FIELD nesno$ AS CHARACTER /* ��᭎ */
          FIELD ovrpr$ AS INT64 /* ����� */
          FIELD ovrstop$ AS INT64 /* ����⮯ */
          FIELD okrugsum$ AS LOGICAL /* ���㣑� */
          FIELD general-mark AS LOGICAL /* general-mark */
          FIELD loan-allowed AS CHARACTER /* loan-allowed */
          FIELD details AS CHARACTER /* details */
          FIELD can-redraw-mark AS LOGICAL /* can-redraw-mark */
          FIELD single-mark AS LOGICAL /* single-mark */
          FIELD create-date AS DATE /* create-date */
          FIELD drower-cat AS CHARACTER /* drower-cat */
          FIELD drower-id AS INT64 /* drower-id */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-loan" "" }
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW TERMINAL-SIMULATION ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 21.14
         WIDTH              = 80.43
         MAX-HEIGHT         = 21.14
         MAX-WIDTH          = 80.43
         VIRTUAL-HEIGHT     = 21.14
         VIRTUAL-WIDTH      = 80.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB TERMINAL-SIMULATION 
/* ************************* Included-Libraries *********************** */

{bis-tty.pro}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME fMain
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN tt-loan.agent-id IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR TOGGLE-BOX tt-loan.can-redraw-mark IN FRAME fMain
   1 2 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-loan.close-date IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR EDITOR tt-loan.comment IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR COMBO-BOX tt-loan.cont-type IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.create-date IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.cust-id IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.drower-id IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.end-date IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN fCliName2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fCliName3 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fCliName3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR TOGGLE-BOX tt-loan.general-mark IN FRAME fMain
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR EDITOR tt-loan.loan-allowed IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-loan.loan-status IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan.open-date IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX tt-loan.single-mark IN FRAME fMain
   1 2 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-loan"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-loan.close-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.close-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan.close-date IN FRAME fMain /* ��� ������� */
DO:
   {&BEG_BT_LEAVE}
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.cust-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.cust-id TERMINAL-SIMULATION
ON LEAVE OF tt-loan.cust-id IN FRAME fMain /* ������ */
,agent-id, drower-id
DO:
   /* � ���쭥�襬 ���������� �뭥�� ᮤ������� �ਣ��� �
      ��楤��� ��⮤� chkupd ᮮ⢥�����饣� ४����� 
   */
   {&BEG_BT_LEAVE}
   
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.end-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan.end-date IN FRAME fMain /* ����砭�� */
DO:
   {&BEG_BT_LEAVE}
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fProxy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fProxy TERMINAL-SIMULATION
ON F1 OF fProxy IN FRAME fMain /* �� �᭮����� */
DO:
  /* �������� ��� ��� ��ᬮ�� ����७����-�᭮����� */
  DEFINE VAR lContext  AS CHAR NO-UNDO.
  DEFINE VAR lCustId   AS CHAR NO-UNDO.
  DEFINE VAR lDrowerId AS CHAR NO-UNDO.
  DEFINE VAR lAgentId  AS CHAR NO-UNDO.
  DEFINE VAR lParent   AS CHAR NO-UNDO.
  DEFINE VAR lTitle    AS CHAR NO-UNDO.
  
  DEFINE BUFFER b-loan FOR loan.
  
  lContext  = tGetParam("���⥪��",        "dps","").
  lCustId   = tGetParam("Cust-Id",         "dps","").
  lDrowerId = tGetParam("Drower-Id",       "dps","").
  lAgentId  = tGetParam("Agent-Id",        "dps","").
  lParent   = tGetParam("Parent-Cont-Code","dps","").

  FIND FIRST b-loan WHERE b-loan.contract  = "proxy"
                      AND b-loan.cont-code = TRIM(lParent)
                     NO-LOCK NO-ERROR.
  IF NOT AVAILABLE b-loan THEN RETURN NO-APPLY.
  
  lTitle = tGetParam("TITLE","dps","").
  RUN tSetParam IN h_tparam ("TITLE", "������������-���������", "dps","").
  
  RUN RunClassMethod(b-loan.class-code,
                     "Form",
                     "","",
                     "",
                     CHR(1)                                   + "," 
                                                              + "," +
                     b-loan.class-code                        + "," +
                     b-loan.contract + ";" + b-loan.cont-code + "," +
                     STRING({&MOD_VIEW})                      + "," +
                     STRING(iLevel + 1)) NO-ERROR.

  RUN tSetParam IN h_tparam ("���⥪��",        lContext, "dps","").
  RUN tSetParam IN h_tparam ("Cust-Id",         lCustId,  "dps","").
  RUN tSetParam IN h_tparam ("Drower-Id",       lDrowerId,"dps","").
  RUN tSetParam IN h_tparam ("Agent-Id",        lAgentId, "dps","").
  RUN tSetParam IN h_tparam ("Parent-Cont-Code",lParent,  "dps","").
  RUN tSetParam IN h_tparam ("TITLE",           lTitle,   "dps","").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fRedraw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fRedraw TERMINAL-SIMULATION
ON LEAVE OF fRedraw IN FRAME fMain
DO:
   
   {&BEG_BT_LEAVE}
   
   RUN LocalFormActions(SELF).
   
   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   
   RUN LocalFormBehavior(SELF).
   
   {&END_BT_LEAVE}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.loan-allowed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.loan-allowed TERMINAL-SIMULATION
ON F1 OF tt-loan.loan-allowed IN FRAME fMain
DO:
   DEFINE BUFFER b-loan FOR loan.
   DEFINE VAR vLoans AS CHAR NO-UNDO.
   IF mParent <> ? AND mParent <> "" THEN DO:
      vLoans = GetXAttrValueEx("loan","proxy," + mParent, "loan-allowed","").
   END.

   DO TRANSACTION:
     pick-value = "".
     RUN browseld.p ("dep_person", 
                     "icust-cat" + CHR(1) + 
                     "icust-id"  + 
                     IF vLoans = "" THEN "" ELSE CHR(1) + "cont-code"
                     ,
                     "�" + CHR(1) +
                     STRING(tt-loan.cust-id)  + 
                     IF vLoans = "" THEN "" ELSE CHR(1) + vLoans
                     ,
                     ?,
                     2).

     FOR EACH tmprecid NO-LOCK:
         FIND FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
         DO:
            IF NOT {assigned SELF:SCREEN-VALUE} THEN
               SELF:SCREEN-VALUE = loan.cont-code.
            ELSE
               SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + "," + loan.cont-code.
         END.
      END.

      ASSIGN tt-loan.loan-allowed.
      EMPTY TEMP-TABLE tmprecid.     /* ���㫥��� �⬥⮪. */
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.doc-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.doc-num TERMINAL-SIMULATION
ON F1 OF tt-loan.doc-num IN FRAME fMain /* ����� */
DO:
DEFINE VAR vCtr AS INT64 NO-UNDO.
   IF iMode NE {&MOD_VIEW} THEN
   DO TRANS:
      vCtr = GetCounterNextValue("����७����", TODAY).
      IF vCtr = ? THEN DO:
         MESSAGE "���������� ��ନ஢��� ����� ����७����,"
                 "�.�. � ��⥬� �� ����஥�� �㦡� ���稪��."
                 "��� �襭�� �஡���� ������� � ������������ ��⥬�."
                 VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ASSIGN 
        tt-loan.doc-num:SCREEN-VALUE = STRING(vCtr, "9999999999")
        tt-loan.doc-num
      .
   END.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.loan-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.loan-status TERMINAL-SIMULATION
ON F1 OF tt-loan.loan-status IN FRAME fMain /* ������ */
DO:
   IF iMode NE {&MOD_VIEW} THEN
   DO TRANS:

      RUN "xstat(br.p" ("proxy", iLevel).
      IF LASTKEY EQ 10
      OR
      LASTKEY EQ 13 THEN
      IF {assigned pick-value} THEN
         SELF:SCREEN-VALUE = pick-value.
   END.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-loan.loan-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.loan-status TERMINAL-SIMULATION
ON LEAVE OF tt-loan.loan-status IN FRAME fMain
DO:
   IF tt-loan.loan-status:MODIFIED THEN
   DO:
       ASSIGN
          tt-loan.loan-status.
       IF    tt-loan.loan-status EQ CHR(251) 
          OR tt-loan.loan-status EQ "���" THEN
          mFlagCloseDate = yes.            
   END.    
END.    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan.open-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan.open-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan.open-date IN FRAME fMain /* ��砫� */
DO:
   {&BEG_BT_LEAVE}
   RUN LocalFormActions(SELF).

   IF RETURN-VALUE <> "" THEN
   DO:
      MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF FRAME fMain ANYWHERE DO:
   mRetVal = IF mOnlyForm THEN
      {&RET-ERROR}
      ELSE 
         "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

RUN StartBisTTY.

RUN tSetParam IN h_tparam ("���⥪��","f-proxy","dps","").

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: ���樠������ ��⥬��� ᮮ�饭�� */
   RUN Init-SysMes("","","").

   /* Commented by KSV: ���४��㥬 ���⨪����� ������ �३�� */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.

   /* Commented by KSV: ��⠥� ����� */
   RUN GetObject.

   /* ������塞 COMBO-BOX'� ����묨 �� ����奬� */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* ���ᢥ⪠ ����� �� LIST-5 (����ந�� ��� ᥡ� )*/
   RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                    REPLACE("{&LIST-5}"," ",","),
                    "bright-green").

   RUN LocalInitValues.
   
   /* Commented by KSV: �����뢠�� �࠭��� ��� */
   STATUS DEFAULT "".
   RUN enable_UI.
   RUN BeforeEnableDisable.
   /* Commented by KSV: ���뢠�� � ����, ����� ࠧ�襭� ��������
   ** � ����ᨬ��� �� ०��� ������ */
   RUN EnableDisable.

   /* Commented by KSV: ���㥬 ࠧ����⥫�. �������⥫� �������� ��� FILL-IN
   ** � �����䨪��஬ SEPARATOR# � ��ਡ�⮬ VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
END.

/* Commented by KSV: ����뢠�� �㦡� ��⥬��� ᮮ�饭�� */
RUN End-SysMes.

RUN disable_ui.

RUN EndBisTTY.

RUN tDelParam IN h_tparam ("���⥪��","dps","").

/* Commented by KSV: ���㦠�� ������⥪� */
{intrface.del}

/* Commented by KSV: �����頥� ���祭�� ��뢠�饩 ��楤�� */
RETURN mRetVal.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeEnableDisable TERMINAL-SIMULATION 
PROCEDURE BeforeEnableDisable :
/*------------------------------------------------------------------------------
  Purpose:  ����⨥/����� ����� � ����ᨬ��� �� ���⥪�� ����᪠ ���   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF iMode = {&MOD_ADD} OR iMode = {&MOD_EDIT} THEN
   DO WITH FRAME {&MAIN-FRAME}:
      
      IF tt-loan.general-mark THEN
         tt-loan.loan-allowed:SENSITIVE = NO.
      
      /* ��㧥� ��䨫��஢�� �� �����稪� - �⪫���� ���� "������" */
      IF mCustId <> ? THEN DO:
        tt-loan.cust-id:SENSITIVE   = NO.
      END.
      
      /* ���� �����⥫� �⪫���� �ᥣ�� (��� ���� ࠢ�� �����稪�, ���� ������ �� ����७����-�᭮�����) */
      tt-loan.drower-id:SENSITIVE = NO.
      
      /* ��㧥� ��䨫��஢�� �� ����७����-�᭮����� */
      IF mParent <> "*" AND mParent <> "" THEN 
         ASSIGN 
            fRedraw:SENSITIVE = NO
            fProxy:SENSITIVE  = TRUE
            fProxy:VISIBLE    = TRUE
            fProxy:READ-ONLY  = TRUE
         .
      
      
      ASSIGN 
         tt-loan.agent-id:SENSITIVE  = NO WHEN mAgentId  <> ?
         .
     
      IF mParent = "" AND tt-loan.parent-cont-code = "" THEN
          ASSIGN 
             fRedraw:SENSITIVE                  = NO
             fProxy:VISIBLE   = NO
             .
      
   END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI TERMINAL-SIMULATION  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI TERMINAL-SIMULATION  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY separator2 fRedraw separator1 fProxy fCliName1 fCliName2 fCliName3 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-loan THEN 
    DISPLAY tt-loan.create-date tt-loan.loan-status tt-loan.doc-num tt-loan.cont-type 
          tt-loan.cust-id tt-loan.drower-id tt-loan.agent-id tt-loan.open-date 
          tt-loan.end-date tt-loan.single-mark tt-loan.can-redraw-mark 
          tt-loan.general-mark tt-loan.loan-allowed tt-loan.close-date 
          tt-loan.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE separator2 fRedraw separator1 tt-loan.create-date tt-loan.loan-status tt-loan.doc-num 
         fProxy tt-loan.cont-type tt-loan.cust-id fCliName1 tt-loan.drower-id 
         tt-loan.agent-id tt-loan.open-date tt-loan.end-date 
         tt-loan.single-mark tt-loan.can-redraw-mark tt-loan.general-mark 
         tt-loan.loan-allowed tt-loan.close-date tt-loan.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalEnableDisable TERMINAL-SIMULATION 
PROCEDURE LocalEnableDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR vH AS HANDLE NO-UNDO.
   
   vH = FRAME {&MAIN-FRAME}:HANDLE.
   vH = vH:FIRST-CHILD. 
   vH = vH:FIRST-CHILD.
   
   DO WHILE VALID-HANDLE(vH):
      CREATE tFields.
      ASSIGN 
         tFields.tHandle    = vH
         tFields.tVisible   = vH:VISIBLE
         tFields.tSensitive = vH:SENSITIVE
         tFields.tName      = vH:NAME
         .
      vH = vH:NEXT-SIBLING.
   END.
   
   RUN BeforeEnableDisable.
   
   vH = FRAME {&MAIN-FRAME}:HANDLE.
   vH = vH:FIRST-CHILD. 
   vH = vH:FIRST-CHILD.
   DO WHILE VALID-HANDLE(vH):
      RUN LocalFormBehavior(vH).
      vH = vH:NEXT-SIBLING.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalFormActions TERMINAL-SIMULATION 
PROCEDURE LocalFormActions :
/*------------------------------------------------------------------------------
  Purpose:     �஢�ન ���������� �����
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM iSelf AS HANDLE NO-UNDO.

   DEF VAR vCliExist AS LOG    NO-UNDO.
   DEF VAR vCliName  AS CHAR   NO-UNDO.
   DEF VAR vH        AS HANDLE NO-UNDO.
   DEF VAR vId       AS INT64    NO-UNDO.
   DEF VAR vName     AS CHAR   NO-UNDO.
   DEF VAR vSurr     AS CHAR   NO-UNDO.

   DEF BUFFER loan FOR loan.

   vH = iSelf.

   CASE iSelf:NAME:
      WHEN "cust-id"   OR
      WHEN "agent-id"  OR
      WHEN "drower-id" THEN
      DO WITH FRAME {&MAIN-FRAME}:
         
         IF NOT({assignex iSelf:SCREEN-VALUE}) AND iSelf:NAME <> "agent-id" THEN
            RETURN "�������� ����".
         
         vCliExist = CliExist("�",iSELF:SCREEN-VALUE).

         IF NOT vCliExist THEN DO:
            IF iSELF:SCREEN-VALUE = "" OR iSELF:SCREEN-VALUE = "0" OR
               iSELF:SCREEN-VALUE = ?  OR iSELF:SCREEN-VALUE = "?" THEN
              RETURN "������ �।�⠢�⥫� �� ����७����".
            RETURN "��� 䨧��᪮�� ��� � ⠪�� �����".
         END.
         
         vCliName = LN_GetClient("�",INT64(iSELF:SCREEN-VALUE)).
         IF iSELF:NEXT-TAB-ITEM:NAME MATCHES "*CliName*" THEN
            iSELF:NEXT-TAB-ITEM:SCREEN-VALUE = vCliName.
         /* �᫨ �� �⬥祭� ���� � ���浪� ��।�����
            � ������ ᮢ������ �����⥫�� */
         IF iSELF:NAME = "drower-id" AND
            fRedraw:SCREEN-VALUE = "NO"
         THEN
            ASSIGN
               tt-loan.cust-id              = INT64(iSELF:SCREEN-VALUE)
               tt-loan.cust-id:SCREEN-VALUE = iSELF:SCREEN-VALUE
               fCliName1:SCREEN-VALUE       = fCliName2:SCREEN-VALUE
               .
         IF iSELF:NAME = "drower-id"                 AND
            fRedraw:SCREEN-VALUE = "NO"              AND 
            tt-loan.cust-id:SENSITIVE = NO           AND 
            ({assignex tt-loan.cust-id:INPUT-VALUE}) AND 
            ({assignex iSelf:INPUT-VALUE})           AND 
            tt-loan.cust-id:INPUT-VALUE <> iSelf:INPUT-VALUE
         THEN
            RETURN "�����⥫� � ������ ������ ���� ����� ��殬".
      END.
      WHEN "fRedraw" THEN
      DO WITH FRAME {&MAIN-FRAME}:
         IF iSelf:SCREEN-VALUE = "NO" THEN
         DO:
            ASSIGN
               tt-loan.parent-cont-code = ""
               tt-loan.parent-contract  = ""
               fRedraw:SCREEN-VALUE = "" 
               .
            IF  tt-loan.cust-id:SENSITIVE = NO             AND 
                tt-loan.drower-id:SENSITIVE = NO           AND 
                ({assignex tt-loan.drower-id:INPUT-VALUE}) AND 
                ({assignex tt-loan.cust-id:INPUT-VALUE})   AND 
                tt-loan.cust-id:INPUT-VALUE <> tt-loan.drower-id:INPUT-VALUE
            THEN
               RETURN "������ � �।�⠢�⥫� ࠧ���� � ~n" + 
                      "��⠭���� �ਧ��� � ���浪� ��।����� ~n" + 
                      "��� �⬥��� ����".
            IF tt-loan.cust-id:SENSITIVE = YES THEN
               ASSIGN 
                  tt-loan.cust-id:SCREEN-VALUE = tt-loan.drower-id:SCREEN-VALUE
                  fCliName1:SCREEN-VALUE       = LN_GetClient("�",INT64(tt-loan.cust-id:SCREEN-VALUE)) 
                  fCliName2:SCREEN-VALUE       = LN_GetClient("�",INT64(tt-loan.cust-id:SCREEN-VALUE))
                  . 

         END.
      END.
      WHEN "fProxy" THEN
      DO:
         /*
         IF NOT {assignex fProxy:SCREEN-VALUE} THEN
            RETURN "������ ����७����� �᭮�����".

         /* � ��� �뫮 ����� �������᪨� ����� */
         RUN RE_B_LOAN IN h_Loan("proxy",fProxy:INPUT-VALUE,BUFFER loan).

         IF NOT AVAIL loan THEN
            RETURN "��� ����७���� � ⠪�� �����".

         IF tGetParam("���⥪��","dps","") = "f-proxy"  AND
            BT_Modify(iSELF)                             AND
            (iMode = {&MOD_ADD} OR iMode = {&MOD_EDIT})
         THEN
         DO:
            vSurr = "proxy," + fProxy:INPUT-VALUE.

            vId = INT64(GetXattrValue("loan",vSurr,"agent-id")) NO-ERROR.

            IF NOT ERROR-STATUS:ERROR AND
               vId <> 0               AND
               vId <> ?
            THEN DO:
               vName = LN_GetClient("�",INT64(vId)).
               DO WHILE VALID-HANDLE(vH):
                  IF vH:NAME = "drower-id" AND vH:SENSITIVE THEN
                  DO:
                     vH:SCREEN-VALUE = STRING(vId).
                     IF vH:NEXT-TAB-ITEM:NAME MATCHES "*CliName*" THEN
                        vH:NEXT-TAB-ITEM:SCREEN-VALUE = vName.
                     LEAVE.
                  END.
                  vH = vH:NEXT-SIBLING.
               END.
            END.
         END.
         */
      END.
      WHEN "open-date" THEN
      DO:
         IF DATE(iSelf:SCREEN-VALUE)               <> ? AND
            DATE(tt-loan.create-date:SCREEN-VALUE) <> ? AND
            DATE(iSelf:SCREEN-VALUE)  < DATE(tt-loan.create-date:SCREEN-VALUE)   
         THEN
            RETURN "��� ��砫� �� ����� ���� ����� ���� ��ଫ����".
         IF DATE(iSelf:SCREEN-VALUE)               <> ? AND
            mMinDate                               <> ? AND
            DATE(iSelf:SCREEN-VALUE)  < mMinDate  
         THEN DO:
             iSelf:SCREEN-VALUE = STRING(mMinDate).
             RETURN "����७����� �뤠���� �� �ࠢ� ��।�����. ����⢨� ����७���� �� ����� ������� ࠭�� ��砫� ����⢨� �᭮���� ����७����. ��ࠢ���.".
         END.
      END.
      WHEN "end-date" THEN
      DO:
        IF DATE(iSelf:SCREEN-VALUE) = ? THEN RETURN "������ ����砭�� ��ਮ�� ����⢨� ����७����".
        
        IF DATE(iSelf:SCREEN-VALUE)              <> ? AND
           DATE(tt-loan.open-date:SCREEN-VALUE)  <> ? AND 
           DATE(iSelf:SCREEN-VALUE)  < DATE(tt-loan.open-date:SCREEN-VALUE)   
        THEN
           RETURN "��� ����砭�� �� ����� ���� ����� ���� ��砫�".
        IF DATE(iSelf:SCREEN-VALUE)              <> ? AND
           mMaxDate                              <> ? AND
           DATE(iSelf:SCREEN-VALUE)  > mMaxDate   
        THEN DO:
            iSelf:SCREEN-VALUE = STRING(mMaxDate).
            RETURN "����७����� �뤠���� �� �ࠢ� ��।�����. ����⢨� ����७���� �� ����� ���������� �����, 祬 �����稢����� �᭮���� ����७�����. ��ࠢ���.".
        END.
      END.
      WHEN "close-date" THEN
      DO:
        IF DATE(iSelf:SCREEN-VALUE)              <> ? AND
           DATE(tt-loan.open-date:SCREEN-VALUE)  <> ? AND 
           DATE(iSelf:SCREEN-VALUE)  < DATE(tt-loan.open-date:SCREEN-VALUE)   
        THEN
           RETURN "��� ������� �� ����� ���� ����� ���� ��砫�".
      END.

   END CASE.

   RETURN.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalFormBehavior TERMINAL-SIMULATION 
PROCEDURE LocalFormBehavior :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM iH AS HANDLE NO-UNDO.
   
   FIND FIRST tFields WHERE tFields.tHandle = iH NO-ERROR.
   IF NOT AVAIL tFields THEN RETURN.
   
   DO WITH FRAME {&MAIN-FRAME}:
   
   CASE tFields.tName:
      WHEN "can-redraw-mark" THEN DO:
        IF mParent <> "" AND mParent <> ? AND mParent <> "*" THEN DO:
          tt-loan.can-redraw:SENSITIVE = NO.
        END.
      END.
      WHEN "fRedraw" THEN
      DO:
         IF iH:SCREEN-VALUE = "NO" THEN
         DO:
            ASSIGN 
               fProxy:VISIBLE = NO WHEN tt-loan.parent-cont-code = "" 
               tt-loan.cust-id:SENSITIVE        = NO WHEN iMode <> {&MOD_VIEW}
               .
            
         END.
         ELSE 
         DO:
         
            ASSIGN 
               fProxy:VISIBLE = YES WHEN tFields.tVisible
               tt-loan.cust-id:SENSITIVE        = YES WHEN tFields.tVisible   AND 
                                                           tFields.tSensitive AND
                                                           iMode <> {&MOD_VIEW}
               .
         END.
      END.
   END CASE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalInitValues TERMINAL-SIMULATION 
PROCEDURE LocalInitValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE BUFFER b-loan FOR loan.

   mAgentId  = INT64(TRIM(tGetParam("Agent-Id","dps","")))  NO-ERROR.
   mDrowerId = INT64(TRIM(tGetParam("Drower-Id","dps",""))) NO-ERROR. 
   mCustId   = INT64(TRIM(tGetParam("Cust-Id","dps","")))   NO-ERROR.
   mDate     = DATE(tGetParam("p-date","dps",""))         NO-ERROR.
   ASSIGN 
      mParent   = tGetParam("Parent-Cont-Code","dps","")
      mDate     = TODAY WHEN mDate = ?
      mAgentId  = ? WHEN mAgentId  = 0
      mDrowerId = ? WHEN mDrowerId = 0
      mCustId   = ? WHEN mCustId   = 0
   .
   
   IF mParent <> ? AND mParent <> "*" THEN DO:
      FIND FIRST b-loan WHERE b-loan.contract  = "proxy" 
                          AND b-loan.cont-code = mParent
                        NO-LOCK NO-ERROR.
      IF AVAILABLE b-loan THEN DO:
         mCustID   = b-loan.cust-id.
         mDrowerID = INT64(GetXAttrValue("loan", b-loan.contract + "," + b-loan.cont-code, "drower-id")).
         mMinDate  = b-loan.open-date.
         mMaxDate  = b-loan.end-date.
      END.
   END.
   
   ASSIGN    
      tt-loan.parent-cont-code = mParent WHEN mParent <> "*"  AND 
                                              mParent <> ""   AND  
                                              iMode   = {&MOD_ADD}  
      fProxy = "����७����"
      tt-loan.general-mark    = NO WHEN tt-loan.general-mark    = ? 
      tt-loan.single-mark     = NO WHEN tt-loan.single-mark     = ? 
      tt-loan.can-redraw-mark = NO WHEN tt-loan.can-redraw-mark = ? 
   .
   IF iMode = {&MOD_ADD} THEN
   ASSIGN 
         tt-loan.create-date     = tt-loan.open-date
         tt-loan.general-mark    = NO WHEN tt-loan.general-mark    = ? 
         tt-loan.single-mark     = NO WHEN tt-loan.single-mark     = ? 
         tt-loan.can-redraw-mark = NO WHEN tt-loan.can-redraw-mark = ? 
         tt-loan.agent-id        = mAgentId  WHEN mAgentId  <> ? 
         tt-loan.drower-id       = mDrowerId WHEN mDrowerId <> ?
         tt-loan.cust-id         = IF mCustId <> ? THEN mCustId ELSE mDrowerId.
   .

   IF iMode = {&MOD_ADD} THEN
   DO:
      IF tt-loan.parent-cont-code <> "" THEN 
      DO:
         tt-loan.drower-id =INT64(GetXattrValue(
                  "loan",
                  "proxy," + tt-loan.parent-cont-code,
                  "agent-id")) NO-ERROR.

      END.

      IF tt-loan.general-mark THEN 
         ASSIGN 
             tt-loan.loan-allowed = "*"
             .
      IF mMinDate <> ? THEN tt-loan.open-date = mMinDate.
      IF mMaxDate <> ? THEN tt-loan.end-date  = mMaxDate.
   END.
   IF fRedraw = NO AND iMode = {&MOD_ADD} THEN
      ASSIGN 
         tt-loan.drower-id = tt-loan.cust-id WHEN mCustId <> ? AND 
                                                  mDrowerId = ?
         .

   ASSIGN 
      fCliName1 = LN_GetClient("�",tt-loan.cust-id)
      fCliName2 = LN_GetClient("�",tt-loan.drower-id)
      fCliName3 = LN_GetClient("�",tt-loan.agent-id)
      fRedraw   = YES WHEN tt-loan.parent-cont-code <> ""
      .

   ASSIGN
     fCliName1 = IF fCliName1 = ? OR fCliName1 = "?" THEN "" ELSE fCliName1
     fCliName2 = IF fCliName2 = ? OR fCliName2 = "?" THEN "" ELSE fCliName2
     fCliName3 = IF fCliName3 = ? OR fCliName3 = "?" THEN "" ELSE fCliName3
   .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalSetObject TERMINAL-SIMULATION 
PROCEDURE LocalSetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF iMode = {&MOD_ADD} AND NOT mBT_IsTrans THEN DO:
       tt-loan.cont-code = NextContCode("PROXY",16).
       /*tt-loan.close-date = tt-loan.end-date.*/
   END.
   
   IF     mFlagCloseDate
      AND iMode EQ {&MOD_EDIT} THEN
      tt-loan.close-date = tt-loan.end-date.

   IF {assignex tt-loan.parent-cont-code} THEN
      tt-loan.parent-contract = "proxy".
   ELSE 
      tt-loan.parent-contract = "".

END PROCEDURE.

PROCEDURE LocalPutTitle:
DEFINE VAR vTitle AS CHAR NO-UNDO.
   IF tGetParam("TITLE", "dps","") = ""  OR tGetParam("TITLE", "dps","") = ? OR 
      tGetParam("TITLE", "dps","") = "?" THEN RETURN.
   FRAME fMain:TITLE = "[" + tGetParam("TITLE", "dps","") + "]".
   RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='11/08/2015 12:02:03.355+04:00' */
/* $LINTUSER='fiyu' */
/* $LINTMODE='1' */
/* $LINTFILE='f-trust.p' */
/*prosigndmc5Hp+sKJDEBJjBJFlFMg*/