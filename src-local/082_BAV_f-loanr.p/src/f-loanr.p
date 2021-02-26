&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loan-acct-main NO-UNDO LIKE loan-acct
       FIELD class-code AS CHARACTER /* class-code */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-loan-acct-main" "loan-acct-main" }
       .
DEFINE TEMP-TABLE tt-loanr NO-UNDO LIKE loan
       FIELD akt_vzv$ AS CHARACTER /* ���_��� */
       FIELD grup_dog$ AS CHARACTER /* ���_��� */
       FIELD datasogl$ AS DATE /* ��⠑��� */
       FIELD data_uar$ AS CHARACTER /* ���_��� */
       FIELD dosroka$ AS CHARACTER /* ������� */
       FIELD igndtokwc$ AS LOGICAL /* ����⎪� */
       FIELD ovrpr$ AS INT64 /* ����� */
       FIELD ovrstop$ AS INT64 /* ����⮯ */
       FIELD okrugsum$ AS LOGICAL /* ���㣑� */
       FIELD rewzim$ AS CHARACTER /* ����� */
       FIELD sindkred$ AS LOGICAL /* �����। */
       FIELD BankCust AS CHARACTER /* BankCust */
       FIELD Bfnc AS CHARACTER /* Bfnc */
       FIELD CallAcct AS CHARACTER /* CallAcct */
       FIELD cred-offset AS CHARACTER /* cred-offset */
       FIELD dateend AS DATE /* dateend */
       FIELD delay AS INT64 /* delay */
       FIELD delay1 AS INT64 /* delay1 */
       FIELD DTKind AS CHARACTER /* DTKind */
       FIELD DTType AS CHARACTER /* DTType */
       FIELD Exec_D AS LOGICAL /* Exec_D */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD int-offset AS CHARACTER /* int-offset */
       FIELD IntAcct AS CHARACTER /* IntAcct */
       FIELD list_type AS CHARACTER /* list_type */
       FIELD loan-acct AS CHARACTER /* loan-acct */
       FIELD loan-par-group AS CHARACTER /* loan-par-group */
       FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
       FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
       FIELD OblAcct AS CHARACTER /* OblAcct */
       FIELD op-date AS CHARACTER /* op-date */
       FIELD Partition AS CHARACTER /* Partition */
       FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
       FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
       FIELD qqq AS CHARACTER /* qqq */
       FIELD rate-list AS CHARACTER /* rate-list */
       FIELD rel_type AS CHARACTER /* rel_type */
       FIELD ReplDate AS DATE /* ReplDate */
       FIELD RevRef1 AS CHARACTER /* RevRef1 */
       FIELD RevRef2 AS CHARACTER /* RevRef2 */
       FIELD round AS LOGICAL /* round */
       FIELD TermKey AS CHARACTER /* TermKey */
       FIELD TicketNumber AS CHARACTER /* TicketNumber */
       FIELD UniformBag AS CHARACTER /* UniformBag */
       FIELD WorkDelay AS LOGICAL /* WorkDelay */
       FIELD convert AS LOGICAL /* convert */
       FIELD f634_val AS CHARACTER /* f634_val */
       FIELD dogviduwceta$ AS CHARACTER /* ��������� */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-loanr" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: F-LOANR.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 16.11.2006 17:05 Vasov   
     Modified: 31.05.2007 MUTA 0077173  �� ����� ������ ������� �� ���
                               ��⥣��� ��⮢ �।��⠭������ � "b" 
     Modified:

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
{globals.i}
{intrface.get tmess}    
{intrface.get terr}
{intrface.get xclass}
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */

DEFINE TEMP-TABLE t-obj NO-UNDO
         FIELD rec AS recid.
DEFINE VARIABLE cDR     AS CHARACTER    NO-UNDO.

&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
CREATE WIDGET-POOL.
&ENDIF
/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE MAIN-FRAME fMain
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

DEFINE VARIABLE mContract    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAcctChanged AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mErrMsg      AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttRes NO-UNDO
  FIELD FileRowId AS ROWID
  FIELD PickValue AS CHARACTER
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
&Scoped-define INTERNAL-TABLES tt-loanr tt-loan-acct-main

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-loan-acct-main.acct ~
tt-loanr.doc-ref tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency ~
tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-loan-acct-main.acct ~
tt-loanr.doc-ref tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency ~
tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-loan-acct-main tt-loanr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-loan-acct-main
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-fMain tt-loanr
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loanr SHARE-LOCK, ~
      EACH tt-loan-acct-main WHERE TRUE /* Join to tt-loanr incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-loanr SHARE-LOCK, ~
      EACH tt-loan-acct-main WHERE TRUE /* Join to tt-loanr incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loanr tt-loan-acct-main
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loanr
&Scoped-define SECOND-TABLE-IN-QUERY-fMain tt-loan-acct-main


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define ENABLED-TABLES tt-loan-acct-main tt-loanr
&Scoped-define FIRST-ENABLED-TABLE tt-loan-acct-main
&Scoped-define SECOND-ENABLED-TABLE tt-loanr
&Scoped-Define DISPLAYED-FIELDS tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define DISPLAYED-TABLES tt-loan-acct-main tt-loanr
&Scoped-define FIRST-DISPLAYED-TABLE tt-loan-acct-main
&Scoped-define SECOND-DISPLAYED-TABLE tt-loanr


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define List-2 tt-loanr.end-date tt-loanr.close-date ~
tt-loanr.comment 
&Scoped-define List-3 tt-loan-acct-main.acct tt-loanr.doc-ref ~
tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date ~
tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
&Scoped-define List-4 tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency ~
tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loanr, 
      tt-loan-acct-main SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-loan-acct-main.acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 16 COLON-ALIGNED
          &ELSE AT ROW 2 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
     tt-loanr.doc-ref
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 16 COLON-ALIGNED
          &ELSE AT ROW 3 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
          &ELSE SIZE 24 BY 1 &ENDIF
     tt-loanr.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 16 COLON-ALIGNED
          &ELSE AT ROW 4 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS COMBO-BOX 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     tt-loanr.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 16 COLON-ALIGNED
          &ELSE AT ROW 5 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loanr.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 16 COLON-ALIGNED
          &ELSE AT ROW 6 COL 16 COLON-ALIGNED &ENDIF FORMAT "x(3)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     tt-loanr.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 16 COLON-ALIGNED
          &ELSE AT ROW 7 COL 16 COLON-ALIGNED &ENDIF FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-loanr.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 16 COLON-ALIGNED
          &ELSE AT ROW 8 COL 16 COLON-ALIGNED &ENDIF FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-loanr.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 16 COLON-ALIGNED
          &ELSE AT ROW 9 COL 16 COLON-ALIGNED &ENDIF FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-loanr.comment
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 16 COLON-ALIGNED
          &ELSE AT ROW 10 COL 16 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 32 BY 1
          &ELSE SIZE 32 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 15 ROW 5
         SIZE 52 BY 14
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loan-acct-main T "?" NO-UNDO bisquit loan-acct
      ADDITIONAL-FIELDS:
          FIELD class-code AS CHARACTER /* class-code */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-loan-acct-main" "loan-acct-main" }
          
      END-FIELDS.
      TABLE: tt-loanr T "?" NO-UNDO bisquit loan
      ADDITIONAL-FIELDS:
          FIELD akt_vzv$ AS CHARACTER /* ���_��� */
          FIELD grup_dog$ AS CHARACTER /* ���_��� */
          FIELD datasogl$ AS DATE /* ��⠑��� */
          FIELD data_uar$ AS CHARACTER /* ���_��� */
          FIELD dosroka$ AS CHARACTER /* ������� */
          FIELD igndtokwc$ AS LOGICAL /* ����⎪� */
          FIELD ovrpr$ AS INT64 /* ����� */
          FIELD ovrstop$ AS INT64 /* ����⮯ */
          FIELD okrugsum$ AS LOGICAL /* ���㣑� */
          FIELD rewzim$ AS CHARACTER /* ����� */
          FIELD sindkred$ AS LOGICAL /* �����। */
          FIELD BankCust AS CHARACTER /* BankCust */
          FIELD Bfnc AS CHARACTER /* Bfnc */
          FIELD CallAcct AS CHARACTER /* CallAcct */
          FIELD cred-offset AS CHARACTER /* cred-offset */
          FIELD dateend AS DATE /* dateend */
          FIELD delay AS INT64 /* delay */
          FIELD delay1 AS INT64 /* delay1 */
          FIELD DTKind AS CHARACTER /* DTKind */
          FIELD DTType AS CHARACTER /* DTType */
          FIELD Exec_D AS LOGICAL /* Exec_D */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD int-offset AS CHARACTER /* int-offset */
          FIELD IntAcct AS CHARACTER /* IntAcct */
          FIELD list_type AS CHARACTER /* list_type */
          FIELD loan-acct AS CHARACTER /* loan-acct */
          FIELD loan-par-group AS CHARACTER /* loan-par-group */
          FIELD main-loan-acct AS CHARACTER /* main-loan-acct */
          FIELD main-loan-cust AS CHARACTER /* main-loan-cust */
          FIELD OblAcct AS CHARACTER /* OblAcct */
          FIELD op-date AS CHARACTER /* op-date */
          FIELD Partition AS CHARACTER /* Partition */
          FIELD PrevLoanID AS CHARACTER /* PrevLoanID */
          FIELD ProfitCenter AS CHARACTER /* ProfitCenter */
          FIELD qqq AS CHARACTER /* qqq */
          FIELD rate-list AS CHARACTER /* rate-list */
          FIELD rel_type AS CHARACTER /* rel_type */
          FIELD ReplDate AS DATE /* ReplDate */
          FIELD RevRef1 AS CHARACTER /* RevRef1 */
          FIELD RevRef2 AS CHARACTER /* RevRef2 */
          FIELD round AS LOGICAL /* round */
          FIELD TermKey AS CHARACTER /* TermKey */
          FIELD TicketNumber AS CHARACTER /* TicketNumber */
          FIELD UniformBag AS CHARACTER /* UniformBag */
          FIELD WorkDelay AS LOGICAL /* WorkDelay */
          FIELD convert AS LOGICAL /* convert */
          FIELD f634_val AS CHARACTER /* f634_val */
          FIELD dogviduwceta$ AS CHARACTER /* ��������� */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-loanr" "" }
          
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
         HEIGHT             = 20.99
         WIDTH              = 80.38
         MAX-HEIGHT         = 20.99
         MAX-WIDTH          = 80.38
         VIRTUAL-HEIGHT     = 20.99
         VIRTUAL-WIDTH      = 80.38
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN tt-loan-acct-main.acct IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-loanr.close-date IN FRAME fMain
   1 2 3 4 EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN tt-loanr.comment IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-loanr.currency IN FRAME fMain
   1 3 4 EXP-FORMAT                                                     */
/* SETTINGS FOR COMBO-BOX tt-loanr.cust-cat IN FRAME fMain
   1 3 4                                                                */
/* SETTINGS FOR FILL-IN tt-loanr.cust-id IN FRAME fMain
   1 3 4                                                                */
/* SETTINGS FOR FILL-IN tt-loanr.doc-ref IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-loanr.end-date IN FRAME fMain
   1 2 3 4 EXP-FORMAT                                                   */
/* SETTINGS FOR FILL-IN tt-loanr.open-date IN FRAME fMain
   1 3 4 EXP-FORMAT                                                     */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-loanr,Temp-Tables.tt-loan-acct-main WHERE Temp-Tables.tt-loanr ..."
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-loan-acct-main.acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-acct-main.acct TERMINAL-SIMULATION
ON F1 OF tt-loan-acct-main.acct IN FRAME fMain /* ��楢�� ��� */
DO:
   IF iMode EQ {&MOD_ADD} THEN
   DO:
      {empty ttRes}
      RUN browseld.p ("acct",
                      "RetRcp"                         + CHR(1) + "RetFld" + CHR(1) + "acct-cat",
                      STRING (TEMP-TABLE ttRes:HANDLE) + CHR(1) + "acct,currency,cust-cat,cust-id" + CHR(1) + "b",
                      "",
                      4).
      IF KEYFUNCTION (LASTKEY) NE "END-ERROR" THEN
         FOR FIRST ttRes:
            IF ENTRY(3,ttRes.PickValue) = "�" THEN DO:
               RUN Fill-SysMes IN h_tmess ("","","-1","����� ������ ������� ��� ��� � ⨯�� ������ - �").
               RETURN.
            END.
            ASSIGN
               tt-loan-acct-main.acct = ENTRY (1, ttRes.PickValue)
               tt-loanr.currency      = ENTRY (2, ttRes.PickValue)
               tt-loanr.cust-cat      = ENTRY (3, ttRes.PickValue)
               tt-loanr.cust-id       = INT64 (ENTRY (4, ttRes.PickValue))
               cDR                    = GetXAttrValue("acct", tt-loan-acct-main.acct + "," + tt-loanr.currency, "��������")
               tt-loanr.doc-ref       = IF (NUM-ENTRIES(cDR) GE 2) THEN TRIM(ENTRY(2, cDR)) ELSE tt-loanr.doc-ref
               tt-loanr.open-date     = IF (cDR EQ "") THEN tt-loanr.open-date ELSE DATE(TRIM(ENTRY(1, cDR)))
            NO-ERROR.
            DISPLAY tt-loan-acct-main.acct
                    tt-loanr.currency
                    tt-loanr.cust-cat
                    tt-loanr.cust-id
                    tt-loanr.doc-ref
                    tt-loanr.open-date
            WITH FRAME fMain.
            mAcctChanged = FALSE.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-acct-main.acct TERMINAL-SIMULATION
ON LEAVE OF tt-loan-acct-main.acct IN FRAME fMain /* ��楢�� ��� */
DO:
   {&BEG_BT_LEAVE}
   IF mAcctChanged THEN
   DO:
      {find-act.i
         &acct = SELF:SCREEN-VALUE
      }
      IF AVAIL acct THEN
      DO:
         IF acct.cust-cat = "�" THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","����� ������ ������� ��� ��� � ⨯�� ������ - �").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
         ASSIGN
             tt-loanr.currency  = acct.currency
             tt-loanr.cust-cat  = acct.cust-cat
             tt-loanr.cust-id   = acct.cust-id
             cDR                = GetXAttrValue("acct", acct.acct + "," + acct.currency, "��������")
             tt-loanr.doc-ref   = IF (NUM-ENTRIES(cDR) GE 2) THEN TRIM(ENTRY(2, cDR)) ELSE tt-loanr.doc-ref
             tt-loanr.open-date = IF (cDR EQ "") THEN tt-loanr.open-date ELSE DATE(TRIM(ENTRY(1, cDR)))
         .
         DISPLAY tt-loanr.currency
                 tt-loanr.cust-cat
                 tt-loanr.cust-id
                 tt-loanr.doc-ref
                 tt-loanr.open-date
         WITH FRAME fMain.
         mAcctChanged = FALSE.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-acct-main.acct TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-loan-acct-main.acct IN FRAME fMain /* ��楢�� ��� */
DO:
   mAcctChanged = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loanr.open-date TERMINAL-SIMULATION
ON LEAVE OF tt-loanr.open-date IN FRAME fMain /*  */
DO:
   {&BEG_BT_LEAVE}
   DEFINE VARIABLE mDate AS CHARACTER        NO-UNDO.
   DEFINE VARIABLE iDate AS INTEGER          NO-UNDO.
   DEFINE VARIABLE lDate AS LOGICAL INIT YES NO-UNDO.
   ASSIGN mDate = SELF:SCREEN-VALUE      NO-ERROR.
   iDate = INTEGER(SUBSTRING(mDate,1,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR OR iDate < 1 OR iDate > 31 THEN lDate = NO.
   iDate = INTEGER(SUBSTRING(mDate,4,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR OR iDate < 1 OR iDate > 12 THEN lDate = NO.
   iDate = INTEGER(SUBSTRING(mDate,7,4)) NO-ERROR.
   IF ERROR-STATUS:ERROR OR iDate < 1900            THEN lDate = NO.
   IF lDate = NO THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1","����ୠ� ���!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&SCOPED-DEFINE LstSymNotAllowed "," + CHR(1) + ";" + CHR(1) + "~~~{" + CHR(1) + "~~~}" + CHR(1) + '~"' + CHR(1) + "~'"

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loanr.doc-ref TERMINAL-SIMULATION
ON LEAVE OF tt-loanr.doc-ref IN FRAME fMain /*  */
DO:
   DEFINE VARIABLE vCnt    AS INT64     NO-UNDO.
   DEFINE VARIABLE vNum    AS INT64     NO-UNDO.
   DEFINE VARIABLE vDocRef AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErr    AS CHARACTER NO-UNDO.

   IF iMode EQ {&MOD_ADD} THEN
   DO:
      {&BEG_BT_LEAVE}
      ASSIGN
         vDocRef = SELF:SCREEN-VALUE
         vNum = NUM-ENTRIES({&LstSymNotAllowed},CHR(1))
      NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND {assigned vDocRef} THEN
      DO:                                                  
         DO vCnt = 1 TO vNum:
            IF INDEX(vDocRef,ENTRY(vCnt,{&LstSymNotAllowed},CHR(1))) > 0 
               THEN vErr = vErr + " " + ENTRY(vCnt,{&LstSymNotAllowed},CHR(1)).
         END.
         IF vErr <> "" THEN 
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1","� ����� ������� �������⨬� ᨬ����: ~n" + TRIM(vErr)).
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
      {&END_BT_LEAVE}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */
&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
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
&ENDIF
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

RUN StartBisTTY.

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

   /* Commented by KSV: �����뢠�� �࠭��� ��� */
   STATUS DEFAULT "".
&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
   RUN enable_UI.
&ENDIF
   /* Commented by KSV: ���뢠�� � ����, ����� ࠧ�襭� ��������
   ** � ����ᨬ��� �� ०��� ������ */
   RUN EnableDisable.
&IF DEFINED(SESSION-REMOTE) &THEN
   LEAVE MAIN-BLOCK.
&ENDIF

   /* Commented by KSV: ���㥬 ࠧ����⥫�. �������⥫� �������� ��� FILL-IN
   ** � �����䨪��஬ SEPARATOR# � ��ਡ�⮬ VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
END.

/* Commented by KSV: ����뢠�� �㦡� ��⥬��� ᮮ�饭�� */
RUN End-SysMes.

&IF DEFINED(SESSION-REMOTE) = 0 &THEN
RUN disable_ui.
&ENDIF

RUN EndBisTTY.

/* Commented by KSV: ���㦠�� ������⥪� */
{intrface.del}

/* Commented by KSV: �����頥� ���祭�� ��뢠�饩 ��楤�� */
RETURN mRetVal.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  IF AVAILABLE tt-loan-acct-main THEN 
    DISPLAY tt-loan-acct-main.acct 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-loanr THEN 
    DISPLAY tt-loanr.doc-ref tt-loanr.cust-cat tt-loanr.cust-id tt-loanr.currency 
          tt-loanr.open-date tt-loanr.end-date tt-loanr.close-date 
          tt-loanr.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-loan-acct-main.acct tt-loanr.doc-ref tt-loanr.cust-cat 
         tt-loanr.cust-id tt-loanr.currency tt-loanr.open-date 
         tt-loanr.end-date tt-loanr.close-date tt-loanr.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalSetObject TERMINAL-SIMULATION 
PROCEDURE LocalSetObject :
DEFINE BUFFER bLoanAcct FOR loan-acct.
DEFINE BUFFER bLoan     FOR loan.

DEFINE VARIABLE vHoliday  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vIsClosed AS LOGICAL     NO-UNDO.

IF iMode EQ {&MOD_ADD} THEN
DO:

   tt-loan-acct-main.acct = AddFilToAcct(tt-loan-acct-main.acct, tt-loanr.filial-id).
   tt-loanr.cont-code = FmtMskAddSuffix(tt-loanr.doc-ref,"cont-code").

   IF NOT {assigned tt-loan-acct-main.acct} THEN
   DO:
      mErrMsg = "������ ����� ���⭮�� ���!".
      RETURN ERROR mErrMsg.
   END.
   {find-act.i
      &acct = tt-loan-acct-main.acct
      &curr = tt-loanr.currency
   }
   IF NOT AVAIL acct THEN
   DO:
      mErrMsg = "��� ⠪��� ���! - " + tt-loan-acct-main.acct +
                " � ����⮩ '" + (IF tt-loanr.currency EQ ""
                                   THEN "{&in-LF-NCN}"
                                    ELSE tt-loanr.currency) + "'".
      RETURN ERROR mErrMsg.
   END.

   IF acct.cust-id  NE tt-loanr.cust-id OR
      acct.cust-cat NE tt-loanr.cust-cat
   THEN
   DO:
      mErrMsg = "� ���⭮�� ��� " + tt-loan-acct-main.acct + " ��㣮� 宧鶴!".
      RETURN ERROR mErrMsg.
   END.

   IF acct.currency NE tt-loanr.currency THEN
   DO:
      mErrMsg = "��� " + tt-loan-acct-main.acct + " � ������� ������ � ࠧ��� ������!".
      RETURN ERROR mErrMsg.
   END.

   IF CAN-FIND (FIRST bLoan WHERE bLoan.contract  EQ mContract
                         AND   bLoan.doc-ref   EQ tt-loanr.doc-ref
                         AND   bLoan.filial-id EQ tt-loanr.filial-id
                         AND   ROWID (bLoan)   NE tt-loanr.local__rowid
               NO-LOCK)
   THEN
   DO:
      mErrMsg = "��� ���� ������� � �����祭��� " + mContract + " � ����஬ " + 
                tt-loanr.doc-ref + ".~n������ ��㣮� ����� �������".
      RETURN ERROR mErrMsg.
   END.


   IF tt-loanr.cust-cat EQ "�" THEN
   DO:
      mErrMsg = "����� ������ ������� ��� ��� � ⨯�� ������ - �".
      RETURN ERROR mErrMsg.
   END.

   /* �஢�ઠ �� �।��� �������� ������ */
   RUN custools.p (tt-loanr.cust-cat, tt-loanr.cust-id, OUTPUT vIsClosed).
   IF vIsClosed THEN
   DO:
      mErrMsg = "������ ������".
      RETURN ERROR mErrMsg.
   END.

   FIND FIRST bLoanAcct WHERE bLoanAcct.contract  EQ mContract
                        AND   bLoanAcct.acct-type EQ mContract
                        AND   bLoanAcct.currency  EQ tt-loanr.currency
                        AND   bLoanAcct.acct      EQ tt-loan-acct-main.acct
                        AND   CAN-FIND (FIRST bLoan WHERE bLoan.contract   EQ mContract
                                                    AND   bLoan.cont-code  EQ bLoanAcct.cont-code
                                                    AND   bLoan.cont-code  NE tt-loanr.cont-code
                                                    AND   bLoan.close-date EQ ?)
      NO-LOCK NO-ERROR.
   IF AVAIL bLoanAcct THEN
   DO:
      mErrMsg = "�� ����� ���� ��᪮�쪮 ������஢ ��� ������ � ⮣� �� ���".
      tt-loan-acct-main.acct = ?.
      RETURN ERROR mErrMsg.
   END.

END.     /*MOD_ADD*/

IF iMode EQ {&MOD_ADD} OR iMode EQ {&MOD_EDIT} THEN
DO:
   vHoliday = Holiday (tt-loanr.end-date).
   IF vHoliday THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","4","��� ����砭�� ������ �� �ࠧ����� ��� ��室��� ����! ��⠢���?").
      IF NOT LOGICAL (pick-value) THEN
         RETURN ERROR.
   END.

   IF tt-loanr.end-date LT tt-loanr.open-date THEN
   DO:
      mErrMsg = "��� ����砭�� ������� ����� ���� ��砫� �������".
      RETURN  ERROR mErrMsg.
   END.

   IF tt-loanr.close-date LT tt-loanr.open-date THEN
   DO:
      mErrMsg = "��� ������� ������� ����� ���� ��砫� �������".
      RETURN  ERROR mErrMsg.
   END.

   /* �஢�ન �ᯥ譮 �����襭� */
   ASSIGN
      tt-loan-acct-main.contract  = tt-loanr.contract
      tt-loan-acct-main.cont-code = tt-loanr.cont-code
      tt-loan-acct-main.acct-type = tt-loanr.contract
      tt-loan-acct-main.currency  = tt-loanr.currency
      tt-loan-acct-main.since     = tt-loanr.open-date
      NO-ERROR
   .
   
   FOR EACH term-obl OF tt-loanr WHERE term-obl.sop-date EQ ?
      EXCLUSIVE-LOCK:
      term-obl.sop-date = tt-loanr.close-date.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
DEFINE BUFFER bLoan FOR loan.
DEFINE VARIABLE vCountCode AS DECIMAL     NO-UNDO.

IF iMode EQ {&MOD_EDIT} OR iMode EQ {&MOD_VIEW} THEN
DO:
   ASSIGN
      mContract  = ENTRY (2, iInstanceList, CHR(3)) WHEN NUM-ENTRIES (iInstanceList, CHR (3)) GE 2.
END.

IF iMode EQ {&MOD_ADD} THEN
DO:
   ASSIGN
      mContract  = ENTRY(2, iInstanceList, CHR(3)) WHEN NUM-ENTRIES (iInstanceList, CHR (3)) GE 2
      vCountCode = DECIMAL(YEAR(TODAY) MODULO 100)
   .
   FIND LAST bLoan WHERE bLoan.contract        EQ mContract
                     AND YEAR(bLoan.open-date) EQ YEAR (TODAY)
      NO-LOCK NO-ERROR.
   _try_get_last_num:
   DO WHILE AVAIL bLoan :
      DECIMAL(bLoan.doc-ref) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         FIND PREV bLoan WHERE bLoan.contract        EQ mContract
                           AND YEAR(bLoan.open-date) EQ YEAR (TODAY)
         NO-LOCK NO-ERROR.
         NEXT _try_get_last_num.
      END.
      vCountCode = DECIMAL(bLoan.doc-ref).
      LEAVE _try_get_last_num.
   END.
   RELEASE bLoan.
   ASSIGN
      vCountCode           = vCountCode + 0.00001
      tt-loanr.contract    = mContract
      tt-loanr.doc-ref     = TRIM (STRING (vCountCode,'>>9.99999'))
      tt-loanr.filial-id   = dept.branch
      tt-loanr.cust-cat    = "�"
      tt-loanr.branch-id   = ""
      tt-loanr.loan-status = ""
      tt-loanr.currency    = ""
   .
   tt-loanr.cont-code   = FmtMskAddSuffix(tt-loanr.doc-ref,"cont-code").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION 
PROCEDURE PostSetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEF VAR vName AS CHAR extent 3 NO-UNDO.
   DEF VAR vDogOtkrLS AS CHAR NO-UNDO.
   
   FIND FIRST acct WHERE acct.acct EQ tt-loan-acct-main.acct.
   IF AVAIL(acct) THEN DO:
      vDogOtkrLS = GetXAttrValueEx("acct",SURROGATE(BUFFER acct:HANDLE),"��������","").
      IF NOT {assigned vDogOtkrLS} THEN DO:
         vDogOtkrLS = STRING(tt-loanr.open-date,"99/99/9999") + "," + STRING(tt-loanr.doc-ref).
         UpdateSigns(acct.class-code,SURROGATE(BUFFER acct:HANDLE),"��������",vDogOtkrLS,?).
      END.
   END.


   RUN GetCustName IN h_base (tt-loanr.cust-cat,
                              tt-loanr.cust-id,
                              ?,
                              OUTPUT vName[1],
                              OUTPUT vName[2],
                              INPUT-OUTPUT vName[3]
                             ).
   vName[1] = TRIM(vName[1] + " " + vNAme[2]).                       
   {lg7001cl.i
      &in-class  = 'loan'
      &surrogate = "tt-loanr.contract + ',' + tt-loanr.cont-code"
      &cl_name1  = "vName[1]"
      &nodefpesr = YES
   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
