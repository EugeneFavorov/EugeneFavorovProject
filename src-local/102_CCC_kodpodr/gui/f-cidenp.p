&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-cust-ident NO-UNDO LIKE cust-ident
       FIELD ExpBKI AS LOGICAL /* ExpBKI */
       FIELD podrazd$ AS CHARACTER /* ���ࠧ� */
       FIELD end-date AS DATE /* end-date */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-cust-ident" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: f-cidenp.p
      Comment: (0019947) ��࠭��� �ଠ ��� �����
               �����䨪��� ��ꥪ�.
   Parameters: ���
         Uses:
      Used by:
      Created: 18.03.2004 19:55 KSV     
     Modified: 22.03.2004 15:11 KSV      (0019947) ��࠭��� �ଠ ��� �����
                                         �����䨪��� ��ꥪ�.
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
{intrface.get cust}
{intrface.get date}
{wordwrap.def}
{fms-chkdoc.i &nofmsprint=yes}
{stoplist.fun}
{chknumberpfr.pro}
/* ��⠢�� ���� ���� */
DEFINE VARIABLE mDbgPrint AS LOGICAL NO-UNDO.
mDbgPrint = NO.
{chk-black-list.pro}
/* ����� ��⠢�� ���� ���� */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE mCodeType AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustCat  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustId   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCustCodeTypeDomain AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountry  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBirthday  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vCountryId AS CHARACTER NO-UNDO.
DEFINE VARIABLE mEgrInfo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPredpr    AS LOGICAL   NO-UNDO.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cust-ident

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date ~
tt-cust-ident.issue 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date ~
tt-cust-ident.issue 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-cust-ident
&Scoped-define QUERY-STRING-fMain FOR EACH tt-cust-ident SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-cust-ident SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-cust-ident


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue 
&Scoped-define ENABLED-TABLES tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE tt-cust-ident
&Scoped-Define ENABLED-OBJECTS fCustname fCodeName separator separator-2 
&Scoped-Define DISPLAYED-FIELDS tt-cust-ident.cust-cat ~
tt-cust-ident.cust-id tt-cust-ident.cust-code-type tt-cust-ident.cust-code ~
tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date ~
tt-cust-ident.issue 
&Scoped-define DISPLAYED-TABLES tt-cust-ident
&Scoped-define FIRST-DISPLAYED-TABLE tt-cust-ident
&Scoped-Define DISPLAYED-OBJECTS fCustname fCodeName separator separator-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue
&Scoped-define List-2 tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue
&Scoped-define List-3 tt-cust-ident.cust-cat tt-cust-ident.cust-id ~
tt-cust-ident.cust-code-type tt-cust-ident.cust-code tt-cust-ident.podrazd$ ~
tt-cust-ident.open-date tt-cust-ident.end-date tt-cust-ident.issue
&Scoped-define List-4 tt-cust-ident.podrazd$ 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fCodeName AS CHARACTER FORMAT "X(256)":U 
     LABEL "������������" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 56 BY 1
     &ELSE SIZE 56 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE fCustname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 28 BY 1
     &ELSE SIZE 28 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator AS CHARACTER FORMAT "X(256)":U INITIAL "----------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator-2 AS CHARACTER FORMAT "X(256)":U INITIAL "----------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-cust-ident SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-cust-ident.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 20 COLON-ALIGNED
          &ELSE AT ROW 1 COL 20 COLON-ALIGNED &ENDIF HELP
          "���⭮� (�), �ਤ��᪮� (�) ���"
          LABEL "��� ������" FORMAT "!"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-cust-ident.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 35 COLON-ALIGNED
          &ELSE AT ROW 1 COL 35 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "������" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCustname
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 48 COLON-ALIGNED
          &ELSE AT ROW 1 COL 48 COLON-ALIGNED &ENDIF NO-LABEL
     tt-cust-ident.cust-code-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 20 COLON-ALIGNED
          &ELSE AT ROW 2 COL 20 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "��� �����䨪�樨" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-cust-ident.cust-code
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 48 COLON-ALIGNED
          &ELSE AT ROW 2 COL 48 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "�����䨪���" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 28 BY 1
          &ELSE SIZE 28 BY 1 &ENDIF
     fCodeName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 20 COLON-ALIGNED
          &ELSE AT ROW 3 COL 20 COLON-ALIGNED &ENDIF
     tt-cust-ident.podrazd$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 20 COLON-ALIGNED
          &ELSE AT ROW 5 COL 20 COLON-ALIGNED &ENDIF HELP
          "��� ���ࠧ�������"
          LABEL "��� ���ࠧ�������" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 52 COLON-ALIGNED
          &ELSE AT ROW 5 COL 52 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "��� �뤠�" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 52 COLON-ALIGNED
          &ELSE AT ROW 6 COL 52 COLON-ALIGNED &ENDIF HELP
          "��� ����砭��"
          LABEL "��� ����砭��" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.issue
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 8
          &ELSE AT ROW 8 COL 8 &ENDIF NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 70 BY 4
          &ELSE SIZE 70 BY 4 &ENDIF
     separator
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 1
          &ELSE AT ROW 4 COL 1 &ENDIF NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     separator-2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF NO-LABEL
     "�뤠�:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
          &ELSE SIZE 6 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 2
          &ELSE AT ROW 9 COL 2 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-cust-ident T "?" NO-UNDO bisquit cust-ident
      ADDITIONAL-FIELDS:
          FIELD ExpBKI AS LOGICAL /* ExpBKI */
          FIELD podrazd$ AS CHARACTER /* ���ࠧ� */
          FIELD end-date AS DATE /* end-date */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-cust-ident" "" }
          
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
         HEIGHT             = 21
         WIDTH              = 80.43
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 80.43
         VIRTUAL-HEIGHT     = 21
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
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX tt-cust-ident.cust-cat IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code-type IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-id IN FRAME fMain
   1 3 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-cust-ident.end-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
ASSIGN 
       fCodeName:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fCustname:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR EDITOR tt-cust-ident.issue IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-ident.open-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.podrazd$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN separator IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator-2 IN FRAME fMain
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-cust-ident"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-cust-ident.cust-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code TERMINAL-SIMULATION
ON LEAVE OF tt-cust-ident.cust-code IN FRAME fMain /* �����䨪��� */
DO:
   {&BEG_BT_LEAVE}
   DEFINE VARIABLE vRegExpr           AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErrMes            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackListNameChar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackListCodeChar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackWhyCodeChar  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlackWhyNameChar  AS CHARACTER NO-UNDO.
/* ��⠢�� ���� ���� */
   DEFINE VARIABLE vAnswer            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vMess              AS CHARACTER NO-UNDO.
/* ����� ��⠢�� ���� ���� */
   DEFINE VARIABLE choose-button      AS LOGICAL   NO-UNDO.

   /* �஢�ઠ ����� ���㬥�� �� ॣ.��ࠦ����, �������� � �ࠢ�筨�� ������� */
   vRegExpr = GetCodeMisc("�������", tt-cust-ident.cust-code-type:SCREEN-VALUE, 3).
   IF NOT DYNAMIC-FUNCTION("ereg":U, vRegExpr, SELF:SCREEN-VALUE, OUTPUT vResult, INPUT-OUTPUT vErrMes) THEN DO:
      RUN Fill-SysMes("base", "", -1, "����� ���㬥�� �� ᮮ⢥����� �ࠢ��� ������樨, 㪠������ � �����䨪��� �������"). 
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   
/* ��⠢�� ���� ���� */
   IF tt-cust-ident.cust-code-type:SCREEN-VALUE NE "��ᯮ��" THEN
   DO:
/* ����� ��⠢�� ���� ���� */
   ASSIGN
      vBlackListCodeChar = SELF:SCREEN-VALUE
      vBlackListNameChar = GetCodeName("black-list",vBlackListCodeChar)
   .
   IF     (vBlackListNameChar <> ?         
      AND  FGetSetting("black-list", "�����������", "") =  "*"
          )
       OR 
          (vBlackListNameChar =  "��ᯮ��" 
      AND  FGetSetting("black-list", "�����������", "") =  "��ᯮ��"
          ) THEN
   DO:
      ASSIGN
         vBlackWhyCodeChar  = GetCode("black-list",vBlackListCodeChar)
         vBlackWhyCodeChar  = IF vBlackWhyCodeChar = ? THEN "?" ELSE vBlackWhyCodeChar
         vBlackWhyNameChar  = GetCodeName("black-why",vBlackWhyCodeChar)
         vBlackWhyNameChar  = IF vBlackWhyNameChar = ? THEN "?" ELSE vBlackWhyNameChar
      .
      MESSAGE SUBSTR("    ��������: " + SUBSTR(vBlackListNameChar,1,12)
                                      + FILL(" ",45),1,45) skip
              SUBSTR("�����, �����: " + SUBSTR(vBlackListCodeChar,1,25)
                                      + FILL(" ",45),1,45) skip (1)
              "*** ���������� � ������ ������ ���������� ***" skip (1)
             SUBSTR("    ������� : "  + vBlackWhyCodeChar
                                      + FILL(" ",45),1,45) skip
              SUBSTR("            - " + SUBSTR(vBlackWhyNameChar,1,45)
                                      + FILL(" ",45),1,45) skip (1)
             "�த������?"
      VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choose-button.
      IF choose-button =  NO OR choose-button =  ? THEN
      RETURN NO-APPLY {&RET-ERROR}.
   END.
/* ��⠢�� ���� ���� */
   END.
   ELSE
   DO: 
      RUN chk-pipe IN THIS-PROCEDURE
         (INPUT  SELF:SCREEN-VALUE,
          OUTPUT vAnswer,
          OUTPUT vMess).
      IF vAnswer EQ "1" THEN 
      DO:
         MESSAGE SUBSTR("    ��������: " + SUBSTR(tt-cust-ident.cust-code-type:SCREEN-VALUE,1,12)
                                      + FILL(" ",45),1,45) skip
              SUBSTR("�����, �����: " + SUBSTR(SELF:SCREEN-VALUE,1,25)
                                      + FILL(" ",45),1,45) skip (1)
              "*** ���������� � ������ ������ ���������� ***" skip (1)
             SUBSTR("    ������� : ��ᯮ�� ������⢨⥫��."
                                      + FILL(" ",45),1,45)
         VIEW-AS ALERT-BOX WARNING.
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      ELSE IF vAnswer EQ "-1" THEN 
      DO:
         MESSAGE vMess SKIP (1)
             "�த������?"
         VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choose-button.
         IF choose-button EQ NO OR choose-button EQ ? THEN
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.   
/* ����� ��⠢�� ���� ���� */
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-ident.cust-code-type IN FRAME fMain /* ��� �����䨪�樨 */
DO:  
DO TRANSACTION:
   RUN codelay ("�������","�������", "���� ���㬥�⮢" ,3). 
   IF    (LASTKEY =  13 OR LASTKEY =  10)
     AND pick-value <> ?
   THEN 
      SELF:SCREEN-VALUE = pick-value.
END.
RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-ident.podrazd$ IN FRAME fMain /* ��� ���ࠧ������� */
DO:  
DO TRANSACTION:
   RUN codelay ("�������","�������","��� ���ࠧ�������",3). 
   IF    (LASTKEY =  13 OR LASTKEY =  10)
     AND pick-value <> ?
   THEN
   DO:
      FIND FIRST code WHERE TRUE
         AND code.class  EQ "�������"
         AND code.parent EQ "�������"
         AND code.code EQ pick-value
      NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN
      DO:   
         SELF:SCREEN-VALUE = code.name.
         tt-cust-ident.issue:SCREEN-VALUE = code.val.  
      END.
   END.
END.
RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-ident.cust-code-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON LEAVE OF tt-cust-ident.cust-code-type IN FRAME fMain /* ��� �����䨪�樨 */
DO:
   {&BEG_BT_LEAVE}
   RUN DispCodeName.
   IF tt-cust-ident.cust-code-type:SCREEN-VALUE =  "�����" THEN
      tt-cust-ident.cust-code:FORMAT = "xxx-xxx-xxx xx".
   ELSE
      tt-cust-ident.cust-code:FORMAT = "x(20)".
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON U1 OF tt-cust-ident.cust-code-type IN FRAME fMain /* ��� �����䨪�樨 */
DO:
   RUN DispCodeName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-cust-ident.cust-code-type IN FRAME fMain /* ��� �����䨪�樨 */
DO:
   RUN DispCodeName.
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
   
   ASSIGN
      mCustCat = ENTRY(2, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  1
      mCustId  = ENTRY(3, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  2
   .
   IF mCustCat <> "*" THEN 
      tt-cust-ident.cust-cat = mCustCat.
   
   IF       mCustId        <> "*"
      AND   TRIM (mCustId) >  "" THEN
      tt-cust-ident.cust-id = INT64(mCustId) NO-ERROR.

   /* ���������� ४����⮢ */
   mEgrInfo = GetSysConf("EgrDocPerson").
   IF iMode =  {&MOD_ADD} OR iMode =  {&MOD_EDIT} THEN
   DO:
      IF     NUM-ENTRIES(mEgrInfo,";") >= 1
         AND ENTRY(1,mEgrInfo,";") =  "YES" THEN
         ASSIGN
            tt-cust-ident.cust-code-type = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 2 THEN ENTRY(2,mEgrInfo,";") ELSE ""
            tt-cust-ident.cust-code      = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 3 THEN ENTRY(3,mEgrInfo,";") ELSE ""
            tt-cust-ident.podrazd$       = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 4 THEN ENTRY(4,mEgrInfo,";") ELSE ""
            tt-cust-ident.open-date      = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 5 THEN DATE(ENTRY(5,mEgrInfo,";")) ELSE ?
            tt-cust-ident.issue          = 
               IF NUM-ENTRIES(mEgrInfo,";") >= 6 THEN ENTRY(6,mEgrInfo,";") ELSE ""
         NO-ERROR.
      RUN SetSysConf IN h_base ("EgrDocPerson","").
   END.

   mCountry = GetSysConf("FPersonIsRezident").    /*�� ����窬 ������*/
   mBirthday = GetSysConf("PBirthday").	/* ��� ஦����� ������ */

   IF NOT {assigned mCountry} THEN                    /*�� ��� ।���஢���� ���㬥��*/
   DO:
      IF tt-cust-ident.cust-cat =  "�" THEN DO:
         FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
         IF AVAIL person THEN
            vCountryId = GetXattrValueEx("person",STRING(person.person-id),"country-id2",person.country-id).
      END.
      IF tt-cust-ident.cust-cat =  "�" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN
            vCountryId = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"country-id2",cust-corp.country-id).
      END.
      IF NOT {assigned vCountryId} THEN
            mCountry  = "Stateless".
         ELSE
            mCountry = IF vCountryId =  "rus" THEN "CitizenRF" ELSE "Foreigner".
   END.
   RUN SetSysConf IN h_base ("IsRezident",mCountry ).

   /* ������塞 COMBO-BOX'� ����묨 �� ����奬� */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* Commented by KSV: �����뢠�� �࠭��� ��� */
   STATUS DEFAULT "".
   RUN enable_UI.

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

/* Commented by KSV: ����塞 ������� ��ꥪ� */
IF VALID-HANDLE(mInstance) AND NOT mOnlyForm THEN 
   RUN DelEmptyInstance(mInstance).

RUN DeleteOldDataProtocol IN h_base  ("IsRezident").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispCodeName TERMINAL-SIMULATION 
PROCEDURE DispCodeName :
/*------------------------------------------------------------------------------
  Purpose:     �⮡ࠦ��� ������������ �����䨪���
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER code FOR code.
DO WITH FRAME fMain :
   fCodeName:SCREEN-VALUE = GetCodeName(mCustCodeTypeDomain,
                                        tt-cust-ident.cust-code-type:SCREEN-VALUE
                            ). 

   IF fCodeName:SCREEN-VALUE =  "?" THEN
      fCodeName:SCREEN-VALUE = "".
END.

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
  DISPLAY fCustname fCodeName separator separator-2 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-cust-ident THEN 
    DISPLAY tt-cust-ident.cust-cat tt-cust-ident.cust-id 
          tt-cust-ident.cust-code-type tt-cust-ident.cust-code 
          tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date 
          tt-cust-ident.issue 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-cust-ident.cust-cat tt-cust-ident.cust-id fCustname 
         tt-cust-ident.cust-code-type tt-cust-ident.cust-code fCodeName 
         tt-cust-ident.podrazd$ tt-cust-ident.open-date tt-cust-ident.end-date 
         tt-cust-ident.issue separator separator-2 
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

   mCustCodeTypeDomain = GetXAttrEx(iClass,
                                    "cust-code-type",
                                    "Domain-Code"
                         ).

   DISABLE 
      tt-cust-ident.cust-cat WHEN mCustCat <> "*"
      tt-cust-ident.cust-id  WHEN mCustId  <> "*"
   WITH FRAME {&FRAME-NAME}.

   mCodeType = GetSysConf("CUST-IDENT-CODE-TYPE").
   IF {assigned mCodeType} THEN
   DO:
      tt-cust-ident.cust-code-type:SCREEN-VALUE IN FRAME fMain = mCodeType.
      DISABLE tt-cust-ident.cust-code-type WITH FRAME fMain.
   END.

   IF tt-cust-ident.cust-code-type:SCREEN-VALUE =  "�����" THEN
      tt-cust-ident.cust-code:FORMAT = "xxx-xxx-xxx xx".
   ELSE
      tt-cust-ident.cust-code:FORMAT = "x(20)".

   RUN DispCodeName.
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
   DEFINE VARIABLE vFio     AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vYesNo   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vAvtoSrok AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEndDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE vOldEndDate  AS DATE   NO-UNDO.
   DEFINE VARIABLE vFIOCl       AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vSnils       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErrSnils    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTemp        AS DATE      NO-UNDO.
   DEFINE VARIABLE vTmpDate    AS CHARACTER NO-UNDO.
   DEFINE BUFFER person     FOR person. /* ���������� ����. */
   DEFINE BUFFER cust-corp  FOR cust-corp.
   DEFINE BUFFER cust-ident FOR cust-ident. 
   DEFINE BUFFER bCustIdent FOR cust-ident.

   IF tt-cust-ident.cust-code-type =  "�����" THEN
      tt-cust-ident.cust-code = tt-cust-ident.cust-code:SCREEN-VALUE IN FRAME fMain.

   IF  iMode                 =  {&MOD_ADD}
   AND tt-cust-ident.cust-id <> ?
   THEN DO:
      /* �஢�ઠ �� ����稥 � 祫����� ⠪��� �� ⨯� ���㬥�� */
      FOR EACH cust-ident WHERE cust-ident.cust-code-type =  tt-cust-ident.cust-code-type
                            AND cust-ident.cust-cat       =  tt-cust-ident.cust-cat
                            AND cust-ident.cust-id        =  tt-cust-ident.cust-id
                            AND cust-ident.close-date     =  ? 
                            AND cust-ident.class-code     =  tt-cust-ident.class-code
      EXCLUSIVE-LOCK:
          
         vYesNo = NO.
         
         MESSAGE 
            "���� �� ������� ���㬥�� � ����� ~"" cust-ident.cust-code-type "~"" SKIP
            "����� " cust-ident.cust-code " �� " cust-ident.open-date SKIP
            "������� ���?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ �������� ]"
         UPDATE vYesNo.
         
         IF vYesNo <> YES THEN
            RETURN ERROR "���㬥�� �� ������".
         
         cust-ident.close-date = tt-cust-ident.open-date.
         RELEASE cust-ident.
      END.
   END.
   vAvtoSrok = GetXattrValueEx("code","�������" + "," + tt-cust-ident.cust-code-type,"��⮑ப","").
   IF tt-cust-ident.cust-cat = "�" THEN
   DO:
      FIND FIRST cust-corp WHERE cust-corp.cust-id = tt-cust-ident.cust-id NO-LOCK NO-ERROR.
      IF     AVAIL cust-corp
         AND (GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id), "�।��", "") <> ""
          OR GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id), "��ꥪ�", "") = "���"
          OR CAN-DO(FGetSetting("�⠭���","����ᔋ��",""),cust-corp.cust-stat)) THEN
         ASSIGN
            vTemp   = DATE(GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"BirthDay",""))   
            mPredpr = YES.
   END.
   ELSE
      FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
   IF {assigned vAvtoSrok} THEN DO:
      IF (AVAIL person AND person.birthday <> ?) OR (mPredpr AND vTemp <> ?) THEN DO:
         vTemp = IF mPredpr THEN vTemp ELSE person.birthday.
         vEndDate = CalcSrokForDocument(vAvtoSrok,tt-cust-ident.open-date,vTemp).         
         IF vEndDate <> ? AND vEndDate <> tt-cust-ident.end-date 
         AND vEndDate <> tt-cust-ident.open-date THEN DO:
            MESSAGE 
               "��� ���㬥�� ~"" tt-cust-ident.cust-code-type "~"" SKIP
               "᪮�४�஢��� ���� ����砭�� �� " vEndDate "?"    SKIP
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ �������� ]"
            UPDATE vYesNo.
            IF vYesNo =  YES THEN
            tt-cust-ident.end-date = vEndDate.    
            END.

         ELSE IF vEndDate = tt-cust-ident.open-date THEN DO:
          tt-cust-ident.open-date = tt-cust-ident.open-date + 1.
          vOldEndDate = CalcSrokForDocument(vAvtoSrok,tt-cust-ident.open-date,vTemp).
           MESSAGE 
               "��� ���㬥�� ~"" tt-cust-ident.cust-code-type "~"" SKIP
               "᪮�४�஢��� ���� ����砭�� �� " vOldEndDate "?"    SKIP
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ �������� ]"
            UPDATE vYesNo.
            tt-cust-ident.open-date = tt-cust-ident.open-date - 1.
            IF vYesNo =  YES THEN
            tt-cust-ident.end-date = vOldEndDate.  
         END.
  
         IF tt-cust-ident.end-date <  TODAY THEN DO:
            CASE FGetSetting("�⠭���","�ப����","���"):
               WHEN "��" THEN DO:
                  MESSAGE 
                     "��� ���㬥�� ~"" tt-cust-ident.cust-code-type "~"" SKIP
                     "����祭" STRING(tt-cust-ident.end-date,"99/99/9999")        SKIP
                  VIEW-AS ALERT-BOX TITLE "[ �������� ]".                              
                  RETURN ERROR "���㬥�� �� ������".
               END.
               WHEN "��" THEN DO:
                  MESSAGE 
                     "��� ���㬥�� ~"" tt-cust-ident.cust-code-type "~"" SKIP
                     "����祭" STRING(tt-cust-ident.end-date,"99/99/9999") "?"    SKIP
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "[ �������� ]"
                  UPDATE vYesNo FORMAT "�த������/�⬥����".               
                  if pick-value  <> "yes" THEN
                     RETURN ERROR "���㬥�� �� ������".
               END.
            END CASE.
         END.
         
      END.
   END.

   IF mPredpr THEN
      FIND FIRST cust-corp WHERE cust-corp.cust-id = tt-cust-ident.cust-id NO-LOCK NO-ERROR.
   ELSE IF tt-cust-ident.cust-cat  = "�" THEN
      FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
   IF AVAIL person OR (AVAIL cust-corp AND mPredpr) THEN 
   DO: 
      /*����� ������� ���㬥��� � ��������� ����஬ � ࠧ��� ��⮩ �뤠�*/
      FOR EACH bCustIdent WHERE bCustIdent.cust-code-type =   tt-cust-ident.cust-code-type
                            AND bCustIdent.cust-code      =   tt-cust-ident.cust-code
                            AND bCustIdent.class-code     =   tt-cust-ident.class-code
                            AND RECID(bCustIdent)         <>  RECID(tt-cust-ident)
                            AND bCustIdent.cust-cat       =   tt-cust-ident.cust-cat
                            AND bCustIdent.cust-id        <>  tt-cust-ident.cust-id 
         NO-LOCK:
      
         RUN GetCustName IN h_base (bCustIdent.cust-cat,
                                    bCustIdent.cust-id,
                                    ?,
                                    OUTPUT         vFIOCl[1],
                                    OUTPUT         vFIOCl[2],
                                    INPUT-OUTPUT   vFIOCl[3]).
         vFIOCl [1] =  vFIOCl [1] +  " "   +  vFIOCl [2].
         IF bCustIdent.open-date <> tt-cust-ident.open-date THEN
         DO:
            pick-value = "NO".
            RUN Fill-SysMes IN h_tmess (  "", "", "4",
               SUBSTITUTE(
                  "���㬥�� &1 � ����஬ &2 㦥 ������ ��� ������ ~n &3 (�����: &4). �������� ����� ���㬥��?",
                  bCustIdent.cust-code-type, bCustIdent.cust-code, 
                  vFIOCl [1], bCustIdent.cust-id)).                    
            IF  pick-value =  "YES" THEN
            DO:   
               APPLY "ENTRY" TO tt-cust-ident.cust-code IN FRAME fMain. 
               RETURN ERROR "���㬥�� �� ������".
            END.
         END.
         ELSE
            RETURN ERROR
               SUBSTITUTE(
                  "���㬥�� &1 � ����஬ &2 � ��⮩ �뤠� &3 㦥 ������ ��� ������ ~n &4 (�����: &5).",
                  bCustIdent.cust-code-type, bCustIdent.cust-code,
                  STRING(bCustIdent.open-date,"99/99/9999"), vFIOCl [1],
                  bCustIdent.cust-id).
      END.
   END.
   /* �஢�ઠ ����஫쭮�� ࠧ�鸞 ����� */
   IF tt-cust-ident.cust-code-type =  "�����" THEN DO:
      RUN ChkNumberPFR IN THIS-PROCEDURE (tt-cust-ident.cust-code,
                                          OUTPUT vSnils,
                                          OUTPUT vErrSnils).
      IF {assigned vErrSnils} THEN DO:
         APPLY "ENTRY" TO tt-cust-ident.cust-code IN FRAME fMain. 
         RETURN ERROR vErrSnils.
      END.
   END.

   IF {assigned tt-cust-ident.cust-code-type } THEN
   DO:
      FIND FIRST code WHERE 
                 code.class =  mCustCodeTypeDomain 
             AND code.code =  tt-cust-ident.cust-code-type 
      NO-LOCK NO-ERROR.
      IF AVAIL code THEN
      DO:
         IF NOT( (    code.misc[7]         <> "YES" 
                  AND code.misc[8]         <> "YES" 
                  AND code.description[2]  <> "YES" )
              OR (    code.misc[7]         =  "YES" 
                  AND mCountry             =  "CitizenRF")
              OR (    code.misc[8]         =  "YES" 
                  AND mCountry             =  "Foreigner")
              OR (    code.description[2]  =  "YES" 
                  AND mCountry             =  "Stateless"))
         THEN DO: 
            RUN Fill-SysMes("base", "", 0,  "�����४�� ��� ���㬥��!").
            RETURN ERROR.
         END.
      END.
   END.

   /* �஢�ઠ ���� �뤠� */
   vTmpDate = IF AVAIL person THEN STRING(person.birthday) ELSE mbirthday.
   IF {assigned vTmpDate} THEN 
      IF tt-cust-ident.cust-code-type = "��ᯮ��" THEN
      DO:
         vTmpDate = SUBSTR(vTmpDate,1,6) + STRING(INT64(SUBSTR(vTmpDate,7)) + 14).          /* + 14 ��� � ��� */
         IF (DATE(tt-cust-ident.open-date) <= DATE(vTmpDate)     OR 
             DATE(tt-cust-ident.open-date) <= DATE("01.01.1997") OR
             DATE(tt-cust-ident.open-date) > TODAY
            ) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1",
               "��� �뤠� ��ᯮ�� �� ��室�� �� �������� ��������").
            APPLY "ENTRY" TO tt-cust-ident.open-date IN FRAME fMain. 
            RETURN ERROR.
         END.
      END.
      ELSE
      DO:
         IF DATE(tt-cust-ident.open-date) < DATE(vTmpDate) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1",
                "��� �뤠� ���㬥�� ��室�� �� �������� ��������").
            APPLY "ENTRY" TO tt-cust-ident.open-date IN FRAME fMain. 
            RETURN ERROR.
         END.
      END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_Go TERMINAL-SIMULATION 
PROCEDURE Local_Go :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE mErrorStatus AS LOGICAL      NO-UNDO INIT NO.
   DEFINE VARIABLE mErrorMess   AS CHARACTER    NO-UNDO.
   DEFINE VARIABLE mErrorHfld   AS HANDLE       NO-UNDO.
   DEFINE VARIABLE vAdmCard     AS CHARACTER    NO-UNDO.

   /* ***************************  Main Block  *************************** */
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
       
      ASSIGN 
         mErrorStatus = NO
         mErrorMess   = ""
         mErrorHfld   = ?
         .

      mErrorHfld = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE,"tt-cust-ident.end-date").
      IF  NOT {assigned mErrorHfld:SCREEN-VALUE} AND tt-cust-ident.cust-code-type =  "����抠��" AND tt-cust-ident.cust-code-type =  "��������ࠦ�" THEN
      DO:
         ASSIGN
            mErrorStatus = YES
            mErrorMess   = "���祭�� ��� ���� <" + mErrorHfld:LABEL + "> ��易⥫쭮 ��� ����������"
            .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.         
      END.
   END.
   /* *************************  End of Main Block  ********************** */
   
   IF NOT mErrorStatus THEN
      RETURN "".
   ELSE
   DO:
      IF mErrorMess <> "" THEN
         RUN Fill-SysMes("","","-1",mErrorMess).
      APPLY "ENTRY" TO mErrorHfld.
      RETURN ERROR.
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
   DEFINE VAR vPersIss     AS CHAR NO-UNDO.
   DEFINE VAR vStr         AS CHAR NO-UNDO.
   DEFINE VAR vRezult      AS LOG  NO-UNDO.
   DEFINE VAR vStatIP      AS CHAR NO-UNDO.
   DEFINE VAR vFIO         AS CHAR NO-UNDO.
   DEFINE VAR vTable       AS CHAR NO-UNDO.
   DEFINE VAR vSurr        AS CHAR NO-UNDO.
   DEFINE BUFFER person FOR person. /* ���������� ����. */
   DEFINE BUFFER cust-corp FOR cust-corp.

   /* ᨭ�஭����� � person, cust-corp*/
   IF  CAN-DO("�,�",tt-cust-ident.cust-cat)
   AND NOT mOnlyForm
   THEN DO:

    IF    (tt-cust-ident.end-date = ?
       OR tt-cust-ident.end-date > TODAY)
      AND (tt-cust-ident.close-date =  ?
       OR tt-cust-ident.close-date  >= gend-date)
    THEN DO:  

      IF mPredpr THEN
      DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id = tt-cust-ident.cust-id 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         ASSIGN
            vTable = "cust-corp"
            vSurr  = IF AVAIL cust-corp THEN STRING(cust-corp.cust-id) ELSE "" NO-ERROR.
      END.
      ELSE IF tt-cust-ident.cust-cat = "�" THEN
      DO:
         FIND FIRST person WHERE person.person-id   = tt-cust-ident.cust-id
                             AND person.document-id = tt-cust-ident.cust-code-type 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         ASSIGN
            vTable = "person"
            vSurr  = IF AVAIL person THEN STRING(person.person-id) ELSE "" NO-ERROR.
      END.
      IF AVAIL person OR (AVAIL cust-corp AND mPredpr) THEN DO:
         IF mPredpr THEN
            RUN Fill-SysMes IN h_tmess ("", "", "4",
                  SUBSTITUTE(
                     "������� �᭮��� ���㬥�� &1 � ����஬ &2 ��� ������ (�����: &3)?",
                     tt-cust-ident.cust-code-type, tt-cust-ident.cust-code, tt-cust-ident.cust-id)).                    
         IF AVAIL person OR pick-value = "YES" THEN
         DO:
            IF NOT UpdateSigns(vTable, 
                               vSurr,
                               "Document4Date_vid", 
                               STRING(tt-cust-ident.open-date, "99/99/9999"),
                               ?) THEN DO:
               RUN Fill-SysMes IN h_tmess 
                  ("", "", "-1", 
                   "�訡�� ���������� ���.४����� Document4Date_vid � ����窥 " 
                   + IF mPredpr THEN "�૨�!" ELSE "䨧���!").
               RETURN ERROR.
            END.
            IF mPredpr THEN
            DO:
               UpdateSigns(vTable, vSurr, "document", tt-cust-ident.cust-code, ?) NO-ERROR.
               UpdateSigns(vTable, vSurr, "document-id", tt-cust-ident.cust-code-type, ?) NO-ERROR.
            END.
            ELSE                  
               ASSIGN
                  person.document    = tt-cust-ident.cust-code
                  person.document-id = tt-cust-ident.cust-code-type
               NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN DO:
               RUN MakeIssue IN h_cust (tt-cust-ident.issue, tt-cust-ident.podrazd$, OUTPUT vPersIss).
               IF mPredpr THEN
                  UpdateSigns(vTable, vSurr, "issue", vPersIss, ?) NO-ERROR.
               ELSE IF tt-cust-ident.cust-cat = "�" THEN
                  person.issue = vPersIss NO-ERROR.
            END.
            IF NOT ERROR-STATUS:ERROR THEN
               IF mPredpr THEN VALIDATE cust-corp NO-ERROR.
                          ELSE VALIDATE person    NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               RUN Fill-SysMes IN h_tmess 
                  ("", "", "-1", "�訡�� ᨭ�஭���樨 � ����窮� " 
                   + (IF mPredpr THEN "�૨�: " ELSE "䨧���: ")
                   + (IF RETURN-VALUE > "" THEN GetNullStr(RETURN-VALUE) 
                      ELSE ERROR-STATUS:GET-MESSAGE(1)) ).
               RETURN ERROR.
            END.
         END.
      END.
      ELSE IF LOCKED person THEN DO:
         FIND FIRST person WHERE person.person-id   = tt-cust-ident.cust-id
                             AND person.document-id = tt-cust-ident.cust-code-type
         NO-LOCK NO-ERROR.
         IF AVAIL person THEN DO:
            vStr = "������ 䨧��� ।������ ��㣮� ���짮��⥫�,~nᨭ�஭����� ���㬥�� ����������".
            WhoLocks2(RECID(person),"person",INPUT-OUTPUT vStr).
            RUN Fill-SysMes IN h_tmess ("", "", "-1", vStr).
            RETURN ERROR.
         END.
      END.
      ELSE IF LOCKED cust-corp THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id  = tt-cust-ident.cust-id
                                AND GetXattrValueEx
                                       ("cust-corp", 
                                        STRING(cust-corp.cust-id), 
                                        "document-id", "") = tt-cust-ident.cust-code-type
         NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN DO:
            vStr = "������ �૨� ।������ ��㣮� ���짮��⥫�,~nᨭ�஭����� ���㬥�� ����������".
            WhoLocks2(RECID(cust-corp),"cust-corp",INPUT-OUTPUT vStr).
            RUN Fill-SysMes IN h_tmess ("", "", "-1", vStr).
            RETURN ERROR.
         END.
      END.
    END. 

   END.


   /*�஢�ઠ �� �⮯-���⠬*/
   IF iMode =  {&MOD_ADD}   OR iMode =  {&MOD_EDIT} 
   THEN DO:
      RUN ChkByStopList (tt-cust-ident.cust-cat,tt-cust-ident.cust-id,"������������_��", "CLNT",OUTPUT vRezult).
      IF vRezult THEN 
      DO:
         {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
         RUN PrintClientSLRep.
         {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"}
      END.
   END. 

   /* �஢�ઠ ����⢨⥫쭮�� ��ᯮ�� �� ������ �� ���� ��� */
   IF iMode =  {&MOD_ADD} OR iMode =  {&MOD_EDIT} 
   THEN DO:
      IF tt-cust-ident.cust-cat =  "�" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN
            ASSIGN
               vStr    = GetXattrValueEX("cust-corp", STRING(tt-cust-ident.cust-id), "�।��", "")
               vStatIP = FGetSetting("�⠭���","����ᔋ��","")
               vRezult = FGetSetting("��������","������_���","") =  "��" AND
                         (vStr <> "" OR CAN-DO(vStatIP,cust-corp.cust-stat))
               vStr    = "cust-corp"
               vFIO    = cust-corp.name-corp 
            .
         ELSE
            vRezult = no.    
      END.   
      ELSE IF tt-cust-ident.cust-cat =  "�" THEN DO:
              FIND FIRST person WHERE person.person-id =  tt-cust-ident.cust-id NO-LOCK NO-ERROR.
              IF AVAIL person THEN
                 ASSIGN
                    vRezult = FGetSetting("��������","������_��","") =  "��"
                    vStr    = "person"
                    vFIO    = person.name-last + " " + person.first-names
                 .
              ELSE
                 vRezult = no.
           END.
           ELSE 
              vRezult = no.
      
      IF vRezult THEN DO:
         {cl-fmschk.i
            "vStr"
            "tt-cust-ident.cust-id"
            "tt-cust-ident.cust-code-type"
            "tt-cust-ident.cust-code"
            "STRING(tt-cust-ident.open-date,'99/99/9999')"
            "vFIO"
         }
         IF    (tt-cust-ident.cust-cat = "�"   
            AND FGetSetting("��������","�������_��","") = "��")
            OR (tt-cust-ident.cust-cat = "�"
            AND FGetSetting("��������","�������_���","") = "��") THEN
         DO:
            IF AVAILABLE tt-res THEN DO:
               IF CAN-DO({&KodIsNotValidDoc},tt-res.fChkKodSt) THEN
               DO:   
                  APPLY "ENTRY" TO tt-cust-ident.cust-code IN FRAME fMain.
                  RETURN ERROR.
               END.    
            END.
         END.              
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTFILE='f-cidenp.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='31/07/2017 13:30:41.808+03:00' */
/*prosignoYn5zXbMhgPEccopcWkaQQ*/