&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loanr-cond NO-UNDO LIKE loan-cond
       FIELD EndDateBeforeProl AS DATE /* EndDateBeforeProl */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD month-cred AS CHARACTER /* month-cred */
       FIELD month-int AS CHARACTER /* month-int */
       FIELD Partition AS CHARACTER /* Partition */
       FIELD prev-type AS CHARACTER /* prev-type */
       FIELD Prolong AS LOGICAL /* Prolong */
       FIELD annuitplat$ AS DECIMAL /* ����⏫�� */
       FIELD kollw#gtper$ AS INT64 /* �����⏥� */
       FIELD konvertaciwa$ AS LOGICAL /* ��������� */
       FIELD shemaplat$ AS LOGICAL /* �奬����� */
       FIELD nomersogl$ AS CHARACTER /* ��������� */
       FIELD tarifplan$ AS CHARACTER /* ���䏫�� */       
       FIELD cond-cr-date AS DATE /* cond-cr-date */
       FIELD end-date AS DATE /* end-date */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-loanr-cond" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: CCOND-ED.P
      Comment: <comment>
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
{globals.i}
{flt-file.i}
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-loanr-cond

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-loanr-cond.since ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-loanr-cond.since ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-loanr-cond
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-loanr-cond
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loanr-cond SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-loanr-cond SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loanr-cond
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loanr-cond


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loanr-cond.since  ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define ENABLED-TABLES tt-loanr-cond
&Scoped-define FIRST-ENABLED-TABLE tt-loanr-cond
&Scoped-Define ENABLED-OBJECTS  
&Scoped-Define DISPLAYED-FIELDS tt-loanr-cond.since ~
tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define DISPLAYED-TABLES tt-loanr-cond
&Scoped-define FIRST-DISPLAYED-TABLE tt-loanr-cond
&Scoped-Define DISPLAYED-OBJECTS  

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define List-2 tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date
&Scoped-define List-3 tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date ~

/*&Scoped-define List-4 tt-loanr-cond.class-code  */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loanr-cond SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-loanr-cond.since
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 18 COLON-ALIGNED
          &ELSE AT ROW 1 COL 18 COLON-ALIGNED &ENDIF HELP
          "��砫� ����⢨� �᫮��� �������"
          LABEL "��稭�����" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF 
     tt-loanr-cond.nomersogl$ 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 18 COLON-ALIGNED
          &ELSE AT ROW 2 COL 18 COLON-ALIGNED &ENDIF HELP
          "���. ᮣ��襭��"
          LABEL "���. ᮣ��襭��"  FORMAT "X(30)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loanr-cond.tarifplan$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18 COLON-ALIGNED
          &ELSE AT ROW 3 COL 18 COLON-ALIGNED &ENDIF HELP
          "��� ��䭮�� �����"
          LABEL "��� ���. �����" FORMAT "X(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 30 BY 1
          &ELSE SIZE 30 BY 1 &ENDIF             
     tt-loanr-cond.int-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 18 COLON-ALIGNED
          &ELSE AT ROW 4 COL 18 COLON-ALIGNED &ENDIF HELP
          "�ப"
          LABEL "�ப" FORMAT "9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF             
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 12 ROW 3
         SIZE 62 BY 6
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loanr-cond T "?" NO-UNDO bisquit loan-cond
      ADDITIONAL-FIELDS:
          FIELD EndDateBeforeProl AS DATE /* EndDateBeforeProl */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD month-cred AS CHARACTER /* month-cred */
          FIELD month-int AS CHARACTER /* month-int */
          FIELD Partition AS CHARACTER /* Partition */
          FIELD prev-type AS CHARACTER /* prev-type */
          FIELD Prolong AS LOGICAL /* Prolong */
          FIELD commission AS CHARACTER /* commission */
          FIELD commi_cur AS CHARACTER /* commi_cur */
          FIELD cond-sch AS CHARACTER /* cond-sch */
          FIELD interest AS CHARACTER /* interest */
          FIELD annuitplat$ AS DECIMAL /* ����⏫�� */
          FIELD kollw#gtper$ AS INT64 /* �����⏥� */
          FIELD konvertaciwa$ AS LOGICAL /* ��������� */
          FIELD shemaplat$ AS LOGICAL /* �奬����� */
          FIELD cond-cr-date AS DATE /* cond-cr-date */
          FIELD end-date AS DATE /* end-date */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-loanr-cond" "" }
          
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
/* SETTINGS FOR FILL-IN tt-loanr-cond.class-code IN FRAME fMain
   3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-loanr-cond.cond-cr-date IN FRAME fMain
   1 2 3 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-loanr-cond.since IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-loanr-cond"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


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
  IF AVAILABLE tt-loanr-cond THEN 
    DISPLAY tt-loanr-cond.since  tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-loanr-cond.since tt-loanr-cond.nomersogl$ tt-loanr-cond.tarifplan$ tt-loanr-cond.int-date  
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
DEFINE VAR lLoanCode AS CHAR NO-UNDO.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-cond FOR loan-cond.
   DEFINE VARIABLE vContract AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vContCode AS CHARACTER   NO-UNDO.

   IF iMode EQ {&MOD_ADD} THEN
   DO:
       ASSIGN
          vContract = GetFltVal("contract")
          vContCode = GetFltVal("cont-code")
          tt-loanr-cond.contract  = vContract
          tt-loanr-cond.cont-code = vContCode 
       . 
   
   END.
   
   FIND FIRST b-loan WHERE b-loan.contract  = tt-loanr-cond.contract
                       AND b-loan.cont-code = tt-loanr-cond.cont-code
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN RETURN. 
   
   IF iMode = {&MOD_ADD} THEN DO:
     FIND FIRST b-loan-cond WHERE b-loan-cond.contract  = b-loan.contract
                              AND b-loan-cond.cont-code = b-loan.cont-code
                            NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b-loan-cond THEN tt-loanr-cond.since = b-loan.open-date.
     ELSE                              tt-loanr-cond.since = gend-date. 
     DISPLAY tt-loanr-cond.since WITH FRAME fMain.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


