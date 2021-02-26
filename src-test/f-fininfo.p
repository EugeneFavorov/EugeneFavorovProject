&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-cust-ident NO-UNDO LIKE cust-ident
       FIELD kodreggni$ AS CHARACTER /* ��������� */
       FIELD kodyadresa$ AS CHARACTER /* ���뀤�� */
       FIELD country-id AS CHARACTER /* country-id */
       FIELD kodreg$ AS CHARACTER /* ������ */
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
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: F-FININFO.P
      Comment: (0202227) ��࠭��� �ଠ ��� �����
               䨭��ᮢ�� ���ଠ樨 �� ��. ����.
   Parameters:
         Uses:
      Used by:
      Created: 
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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE mCustCat            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustId             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mclass-code         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymbol             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mStr                AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mStrTMP             AS CHARACTER NO-UNDO. 

&GLOBAL-DEFINE MAIN-FRAME fMain

&GLOBAL-DEFINE table      tt-cust-ident

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

{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */

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
&Scoped-define FIELDS-IN-QUERY-fMain ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-cust-ident
&Scoped-define QUERY-STRING-fMain FOR EACH tt-cust-ident SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-cust-ident SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-cust-ident
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-cust-ident


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define ENABLED-TABLES tt-cust-ident
&Scoped-define FIRST-ENABLED-TABLE tt-cust-ident
&Scoped-Define ENABLED-OBJECTS vsource vdoc-num vdoc-date vinfodesc ~
fCustName separator 
&Scoped-Define DISPLAYED-FIELDS ~
tt-cust-ident.open-date tt-cust-ident.close-date tt-cust-ident.cust-id ~
tt-cust-ident.cust-cat 
&Scoped-define DISPLAYED-TABLES tt-cust-ident
&Scoped-define FIRST-DISPLAYED-TABLE tt-cust-ident
&Scoped-Define DISPLAYED-OBJECTS vsource vdoc-num vdoc-date vinfodesc ~
fCustName separator 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 vsource vdoc-num vdoc-date ~
vinfodesc tt-cust-ident.open-date ~
tt-cust-ident.close-date 
&Scoped-define List-2 vsource vdoc-num vdoc-date ~
vinfodesc tt-cust-ident.open-date ~
tt-cust-ident.close-date 
&Scoped-define List-3 vsource vdoc-num vdoc-date ~
vinfodesc tt-cust-ident.open-date ~
tt-cust-ident.close-date 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE vinfodesc AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 5000 SCROLLBAR-VERTICAL
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 57 BY 4
     &ELSE SIZE 57 BY 4 &ENDIF NO-UNDO.

DEFINE VARIABLE fCustName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 39 BY 1
     &ELSE SIZE 39 BY 1 &ENDIF.

DEFINE VARIABLE separator AS CHARACTER FORMAT "X(256)":U INITIAL "----------------------------------------------------------------------------------------------------" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vdoc-date AS DATE FORMAT "99/99/9999":U 
     LABEL "��� ���㬥��" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vdoc-num AS CHARACTER FORMAT "X(100)":U 
     LABEL "����� ���㬥��" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 1
     &ELSE SIZE 61 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vsource AS CHARACTER FORMAT "x(1000)" 
     LABEL "���筨�" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 1
     &ELSE SIZE 61 BY 1 &ENDIF.
     
DEFINE VARIABLE vSourceDetails AS CHARACTER FORMAT "X(500)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 1
     &ELSE SIZE 70 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-cust-ident SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     vsource
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 8
          &ELSE AT ROW 2.99 COL 8 &ENDIF HELP
          "���筨�"          
     vSourceDetails
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 16 COLON-ALIGNED
          &ELSE AT ROW 6 COL 16 COLON-ALIGNED &ENDIF NO-LABEL
     vdoc-num
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 16 COLON-ALIGNED
          &ELSE AT ROW 4 COL 16 COLON-ALIGNED &ENDIF HELP
          "����� ���㬥��"
     vdoc-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 16 COLON-ALIGNED
          &ELSE AT ROW 5 COL 16 COLON-ALIGNED &ENDIF HELP
          "��� ���㬥��"
     vinfodesc
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 21
          &ELSE AT ROW 7 COL 21 &ENDIF HELP
          "���ᠭ�� ���ଠ樨" NO-LABEL
     tt-cust-ident.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 16 COLON-ALIGNED
          &ELSE AT ROW 11 COL 16 COLON-ALIGNED &ENDIF HELP
          "��� ����祭�� ���ଠ樨"
          LABEL "��� ����祭��" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 45 COLON-ALIGNED
          &ELSE AT ROW 11 COL 45 COLON-ALIGNED &ENDIF HELP
          "��� ����砭�� ����⢨�"
          LABEL "��� ����砭��" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     fCustName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 38 COLON-ALIGNED
          &ELSE AT ROW 1 COL 38 COLON-ALIGNED &ENDIF NO-LABEL
     tt-cust-ident.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 21
          &ELSE AT ROW 1 COL 21 &ENDIF HELP
          "���浪��� ����� ������"
          LABEL "������" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-ident.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 14 COLON-ALIGNED
          &ELSE AT ROW 1 COL 14 COLON-ALIGNED &ENDIF HELP
          "" FORMAT "!"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     separator
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 1
          &ELSE AT ROW 2 COL 1 &ENDIF NO-LABEL
     "���ᠭ�� ���ଠ樨:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 20 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 14
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-cust-ident T "?" NO-UNDO bisquit cust-ident
      ADDITIONAL-FIELDS:
          FIELD kodreggni$ AS CHARACTER /* ��������� */
          FIELD kodyadresa$ AS CHARACTER /* ���뀤�� */
          FIELD country-id AS CHARACTER /* country-id */
          FIELD kodreg$ AS CHARACTER /* ������ */
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
         HEIGHT             = 26.64
         WIDTH              = 84.71
         MAX-HEIGHT         = 26.64
         MAX-WIDTH          = 84.71
         VIRTUAL-HEIGHT     = 26.64
         VIRTUAL-WIDTH      = 84.71
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
/* SETTINGS FOR FILL-IN tt-cust-ident.close-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-cat IN FRAME fMain
   EXP-FORMAT EXP-HELP                                                  */
ASSIGN 
       tt-cust-ident.cust-cat:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-cust-ident.cust-code-type IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-ident.cust-id IN FRAME fMain
   ALIGN-L EXP-LABEL EXP-FORMAT EXP-HELP                                */
ASSIGN 
       tt-cust-ident.cust-id:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fCustName:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-cust-ident.open-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN separator IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN vdoc-date IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN vdoc-num IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR EDITOR vinfodesc IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN vsource IN FRAME fMain
   ALIGN-L 1 2 3                                                        */
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

&Scoped-define SELF-NAME fMain
/*&Scoped-define SELF-NAME tt-cust-ident.cust-code-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-ident.cust-code-type IN FRAME fMain /* ��� ���ଠ樨 */
DO:
  IF mclass-code EQ "��������" THEN
  DO TRANS:
         RUN browseld.p ("code",  /* ����� ��ꥪ�. */
                "class" + CHR(1) + "parent" + CHR(1) + "title",

                /* ���� ��� �।��⠭����. */
                "��������" + CHR(1) + "��������" + CHR(1) + "�����ᮢ�� ��������� ������",

                /* ���᮪ ���祭�� �����. */
                 ?,          /* ���� ��� �����஢��. */
                iLevel).      /* ��ப� �⮡ࠦ���� �३��. */
  END.
  ELSE
  DO TRANS:
       RUN browseld.p ("code",  /* ����� ��ꥪ�. */
              "class" + CHR(1) + "parent" + CHR(1) + "title",

              /* ���� ��� �।��⠭����. */
              "���������" + CHR(1) + "���������" + CHR(1) + "������� ९���� ������",

              /* ���᮪ ���祭�� �����. */
               ?,          /* ���� ��� �����஢��. */
              iLevel).      /* ��ப� �⮡ࠦ���� �३��. */
   END.

   IF    LASTKEY = 10 
         AND {assigned pick-value} THEN
    SELF:SCREEN-VALUE = pick-value.
      
    APPLY "VALUE-CHANGED" TO SELF.
END.
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-ident.cust-code-type TERMINAL-SIMULATION
ON LEAVE OF tt-cust-ident.cust-code-type IN FRAME fMain /* ��� ���ଠ樨 */
DO:
  {&BEG_BT_LEAVE}
  IF mclass-code EQ "��������" THEN 
     FIND FIRST CODE WHERE
         CODE.class  = "��������" AND
         CODE.parent = "��������" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST CODE WHERE
         CODE.class  = "���������" AND
         CODE.parent = "���������" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL CODE THEN DO:
     RUN Fill-SysMes IN h_tmess ("", "", "-1", "���祭�� ४����� " + SELF:SCREEN-VALUE + " ��� � �����䨪���").
     RETURN NO-APPLY {&RET-ERROR}.
  END.
  {&END_BT_LEAVE}
END.
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME vsource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsource TERMINAL-SIMULATION
ON F1 OF vsource IN FRAME fMain /* ���筨� */
DO:   
  CASE mclass-code:  
  WHEN "��������" THEN
  DO TRANS:
       RUN browseld.p ("code",  /* ����� ��ꥪ�. */
              "class" + CHR(1) + "parent" + CHR(1) + "title",

              /* ���� ��� �।��⠭����. */
              "��℥����" + CHR(1) + "��℥����" + CHR(1) + "���筨�� ������� ९��樨 ������",

              /* ���᮪ ���祭�� �����. */
               ?,          /* ���� ��� �����஢��. */
              iLevel).      /* ��ப� �⮡ࠦ���� �३��. */

   IF    LASTKEY = 10 
         AND {assigned pick-value} THEN
    SELF:SCREEN-VALUE = pick-value.
      
    APPLY "VALUE-CHANGED" TO SELF.
    
    
      IF vsource:SCREEN-VALUE EQ "����" THEN
            DO:
                MESSAGE "��� ������ ������ ������ F2"  VIEW-AS ALERT-BOX .    
                mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR.
                vSourceDetails:SCREEN-VALUE = "(F2) " + GetCodeDesc("��℥����",SELF:SCREEN-VALUE,1,"") + ": " + mStrTMP.
            END. 
            ELSE
                vSourceDetails:SCREEN-VALUE =  GetCodeDesc("��℥����",SELF:SCREEN-VALUE,1,"") .
    
   END.
    WHEN "��������" THEN
  DO TRANS:
      
     RUN browseld.p ("code",  /* ����� ��ꥪ�. */
              "class" + CHR(1) + "parent" + CHR(1) + "title",

              /* ���� ��� �।��⠭����. */
              "�����������" + CHR(1) + "�����������" + CHR(1) + "�������� (���㬥���) � 䨭��ᮢ�� ���������",

              /* ���᮪ ���祭�� �����. */
               ?,          /* ���� ��� �����஢��. */
              iLevel).      /* ��ப� �⮡ࠦ���� �३��. */
   
   
    IF    LASTKEY = 10 
         AND {assigned pick-value} THEN
    SELF:SCREEN-VALUE = pick-value.
      
    APPLY "VALUE-CHANGED" TO SELF.
   END.
   END CASE.
   
END.


ON F2 OF vsource IN FRAME fMain /* ��� 楫� */
    DO:    
        If vsource:SCREEN-VALUE EQ "����" THEN Do: RUN  OpenWin. 
        mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR. 
        vSourceDetails:SCREEN-VALUE = "(F2) " + GetCodeDesc("��℥����",SELF:SCREEN-VALUE,1,"") + ": " + mStrTMP.   
END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsource TERMINAL-SIMULATION
ON LEAVE OF vsource IN FRAME fMain /* ���筨� */
DO:
      
  {&BEG_BT_LEAVE}
  CASE mclass-code: 
      WHEN "��������" THEN
  DO TRANS:
      FIND FIRST CODE WHERE
         CODE.class  = "��℥����" AND
         CODE.parent = "��℥����" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL CODE THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "���祭�� ४����� " + SELF:SCREEN-VALUE + " ��� � �����䨪���").
         RETURN NO-APPLY {&RET-ERROR}.
     END.
     else
     do:
   IF vsource EQ "����" THEN
   DO:
       mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR. 
       vSourceDetails:SCREEN-VALUE = "(F2) " + GetCodeDesc("��℥����",SELF:SCREEN-VALUE,1,"") + ": " + mStrTMP.
   END. 
    /*ELSE
        vVidContDetails:SCREEN-VALUE =  GetCodeDesc("����������",SELF:SCREEN-VALUE,1,"") "123". */
     
      end.
  END.
  
    WHEN "��������" THEN
  DO TRANS:
        /*   FIND FIRST CODE WHERE
         CODE.class  = "�����������" AND
         CODE.parent = "�����������" AND
         CODE.code   = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL CODE THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "���祭�� ४����� " + SELF:SCREEN-VALUE + " ��� � �����䨪���").
         RETURN NO-APPLY {&RET-ERROR}.
     END. */     
      end.
  END CASE.
   ASSIGN
     vsource     = vsource:SCREEN-VALUE
     .  
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
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: ���樠������ ��⥬��� ᮮ�饭�� */
   RUN Init-SysMes("","","").

   /* Commented by KSV: ���४��㥬 ���⨪����� ������ �३�� */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.
   
   ASSIGN
         mCustCat  = ENTRY(2, iInstanceList,CHR(3))
         mCustId   = ENTRY(3, iInstanceList,CHR(3))
         mclass-code = ENTRY(4, iInstanceList,CHR(3))
         mSymbol   = ENTRY(5, iInstanceList,CHR(3))
   .
   /* Commented by KSV: ��⠥� ����� */
   RUN GetObject.
   
   IF     mCustCat NE "*"
      AND TRIM(mCustCat) GT "" 
   THEN 
      tt-cust-ident.cust-cat = mCustCat.
  
   IF     mCustId NE "*" 
      AND TRIM(mCustId) GT "" 
   THEN
      tt-cust-ident.cust-id = INT64(mCustId) NO-ERROR.

   RUN DispCustName.
   
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
  DO:
   WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
  END.
END.

/* Commented by KSV: ����뢠�� �㦡� ��⥬��� ᮮ�饭�� */
RUN End-SysMes.

RUN disable_ui.

/* Commented by KSV: ����塞 ������� ��ꥪ� */
IF VALID-HANDLE(mInstance) AND NOT mOnlyForm THEN 
   RUN DelEmptyInstance(mInstance).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispCustName TERMINAL-SIMULATION 
PROCEDURE DispCustName :
/*------------------------------------------------------------------------------
  Purpose:     �⮡ࠦ��� ������������ ������
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fMain :
  DEFINE VARIABLE vTmp        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vCustName   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vCustName2  AS CHARACTER  NO-UNDO.
  /* ASSIGN FRAME fMain tt-cust-ident.cust-cat tt-cust-ident.cust-id. */
  fCustName:SCREEN-VALUE = "".
  RUN GetCustName IN h_base ( tt-cust-ident.cust-cat,
                              tt-cust-ident.cust-id,
                             ?,
                             OUTPUT vCustName,OUTPUT vCustName2,INPUT-OUTPUT vTmp).
  fCustName = trim(vCustName) + " " + vCustName2.
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
mStrTMP = entry(4,GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",""),";") NO-ERROR.
If vsource EQ "����" THEN   vSourceDetails = "(F2) " + vsource + ": " + mStrTMP. 
else vSourceDetails = "".
IF iMode EQ {&MOD_ADD} THEN
    tt-cust-ident.open-date = today.

  DISPLAY vsource vSourceDetails vdoc-num vdoc-date vinfodesc fCustName separator 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-cust-ident THEN 
    DISPLAY tt-cust-ident.open-date 
          tt-cust-ident.close-date tt-cust-ident.cust-id tt-cust-ident.cust-cat 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE vsource vdoc-num vdoc-date vinfodesc 
         tt-cust-ident.open-date tt-cust-ident.close-date fCustName 
         tt-cust-ident.cust-id tt-cust-ident.cust-cat separator 
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
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local_GO TERMINAL-SIMULATION 
PROCEDURE local_GO :
/*------------------------------------------------------------------------------
  Purpose:      
  Parameters:  <none> 
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME fMain:
      ASSIGN
         vdoc-num    = vdoc-num:SCREEN-VALUE
         vdoc-date   = date(vdoc-date:SCREEN-VALUE) 
         vinfodesc   = vinfodesc:SCREEN-VALUE
         vsource     = vsource:SCREEN-VALUE
      NO-ERROR.
      ASSIGN
         tt-cust-ident.cust-code = vdoc-num + mSymbol + (IF vdoc-date EQ ? THEN "" ELSE string(vdoc-date)) + mSymbol + STRING(TODAY) + STRING(TIME)
         tt-cust-ident.issue     = vsource + mSymbol + vinfodesc                    
         .
         CASE mclass-code:  
              WHEN "��������" THEN tt-cust-ident.cust-code-type = "FinInfoRep". 
              WHEN "��������" THEN tt-cust-ident.cust-code-type = "FinInfoPos". 
            END CASE. 
       END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none> 
  Notes:       
------------------------------------------------------------------------------*/
IF iMode EQ {&MOD_ADD} THEN
   tt-cust-ident.open-date = TODAY .  
ELSE DO:
   ASSIGN 
      vdoc-num = ENTRY(1,tt-cust-ident.cust-code,mSymbol)
      vdoc-date = date(ENTRY(2,tt-cust-ident.cust-code,mSymbol))
      vinfodesc = ENTRY(2,tt-cust-ident.issue,mSymbol)
      vsource   = ENTRY(1,tt-cust-ident.issue,mSymbol)
      .
END.
END PROCEDURE.

PROCEDURE OpenWin :   
DEFINE VARIABLE mTmpStr                 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mTmpStr2                AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mTmpStr3                AS CHARACTER NO-UNDO.                
PAUSE 0.

FORM                                            
     mStr VIEW-AS FILL-IN SIZE 40 BY 1 AT ROW 1 COL 1 LABEL  "����" FORMAT "x(255)"

    WITH FRAME fM CENTERED ROW 10 OVERLAY SIDE-LABELS
    TITLE "[ ������� �������� ]" .

UPDATE    
    mStr
    WITH FRAME fM. 
    mTmpStr3 = GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other","").  
    IF mTmpStr3 NE "" THEN
    DO:
        mTmpStr = entry(4,mTmpStr3,";") NO-ERROR.      
        if  mTmpStr = "" THEN 
            mTmpStr2 = entry(1,mTmpStr3,";") + ";" + entry(2,mTmpStr3,";") + ";" + entry(3,mTmpStr3,";")  + mStr + ";" + ";" + entry(5,mTmpStr3,";") + ";" + entry(6,mTmpStr3,";") + ";".
        else                                                           
            mTmpStr2 =  REPLACE(mTmpStr3,mTmpStr,mStr).     
    END.
    ELSE     
        mTmpStr2 = ";;;" + mStr + ";;;".       
    UpdateSignsEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",mTmpStr2).
    HIDE FRAME fM.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='10/10/2014 12:36:29.770+04:00' */
/* $LINTFILE='f-fininfo.p' */
/*prosign13ixAudWITtOWFZuZdYfzg*/