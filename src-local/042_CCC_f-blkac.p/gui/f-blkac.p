{globals.i}
{intrface.get tmess}

/* +++ f-blkac.p was humbly modified by (c)blodd converter v.1.09 on 12/2/2016 11:50am +++ */

&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-BlockAcct NO-UNDO LIKE BlockObject
       FIELD datarewspr$     AS DATE      /* ��⠐��� */
       FIELD datarewsot$     AS DATE      /* ��⠐��� */       
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-BlockAcct" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: F-BLKAC.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 27.11.2007 19:10 ilvi    
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
{intrface.get instrum}
CREATE WIDGET-POOL.
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

DEFINE VARIABLE mFile      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSurrogate AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNum       AS INT64   NO-UNDO.
DEFINE VARIABLE mTmpStr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodNO     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodNO_OT  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodNoOK   AS LOGICAL   NO-UNDO INIT YES.

/* "�ਥ����" �����頥��� ���祭��. */
{ttretval.def}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-BlockAcct

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.datarewsot$~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-BlockAcct
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-BlockAcct
&Scoped-define QUERY-STRING-fMain FOR EACH tt-BlockAcct SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-BlockAcct SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-BlockAcct
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-BlockAcct


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define ENABLED-TABLES tt-BlockAcct
&Scoped-define FIRST-ENABLED-TABLE tt-BlockAcct
&Scoped-Define DISPLAYED-FIELDS tt-BlockAcct.block-type ~
tt-BlockAcct.beg-datetime tt-BlockAcct.end-datetime tt-BlockAcct.val[1] ~
tt-BlockAcct.val[2] tt-BlockAcct.val[3] tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] tt-BlockAcct.datarewspr$ tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define DISPLAYED-TABLES tt-BlockAcct
&Scoped-define FIRST-DISPLAYED-TABLE tt-BlockAcct


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-BlockAcct.block-type tt-BlockAcct.beg-datetime ~
tt-BlockAcct.val[1] tt-BlockAcct.val[2] tt-BlockAcct.val[3] ~
tt-BlockAcct.txt[1] tt-BlockAcct.user-id tt-BlockAcct.txt[2] ~
tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT tt-BlockAcct.datarewspr$ ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] ~
tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define List-2 tt-BlockAcct.end-datetime tt-BlockAcct.txt[1] ~
tt-BlockAcct.user-id tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] ~
tt-BlockAcct.txt[4] mKodNo mKodNo_OT tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] tt-BlockAcct.txt[8]
&Scoped-define List-3 tt-BlockAcct.block-type tt-BlockAcct.beg-datetime ~
tt-BlockAcct.end-datetime tt-BlockAcct.val[1] tt-BlockAcct.val[2] ~
tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.user-id ~
tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT ~
tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
&Scoped-define List-4 tt-BlockAcct.val[1] tt-BlockAcct.val[2] ~
tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.txt[2] ~
tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT tt-BlockAcct.datarewspr$ ~
tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] tt-BlockAcct.txt[7] ~
tt-BlockAcct.txt[8] tt-BlockAcct.val[4]

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-BlockAcct SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-BlockAcct.block-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 21 COLON-ALIGNED
          &ELSE AT ROW 1 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.beg-datetime
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 21 COLON-ALIGNED
          &ELSE AT ROW 2 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.end-datetime
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 21 COLON-ALIGNED
          &ELSE AT ROW 3 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[1]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 21 COLON-ALIGNED
          &ELSE AT ROW 4 COL 21 COLON-ALIGNED &ENDIF HELP
          "�㬬� �� ������"
          LABEL "�㬬� �� ������"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[2]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 21 COLON-ALIGNED
          &ELSE AT ROW 5 COL 21 COLON-ALIGNED &ENDIF HELP
          "�㬬� �� �।���"
          LABEL "�㬬� �� �।���"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[3]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 21 COLON-ALIGNED
          &ELSE AT ROW 6 COL 21 COLON-ALIGNED &ENDIF HELP
          "����� ���⪠"
          LABEL "�����  ���⪠"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-BlockAcct.val[4]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 57 COLON-ALIGNED
          &ELSE AT ROW 6 COL 57 COLON-ALIGNED &ENDIF HELP
          "�㡫��� ����������"
          LABEL "��.���."
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 19 BY 1
          &ELSE SIZE 19 BY 1 &ENDIF
     tt-BlockAcct.txt[1]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 10
          &ELSE AT ROW 7 COL 10 &ENDIF HELP
          "��।�����"
          LABEL "��।�����"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
     tt-BlockAcct.user-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 21 COLON-ALIGNED
          &ELSE AT ROW 8 COL 21 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
     tt-BlockAcct.txt[2]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 12
          &ELSE AT ROW 10 COL 12 &ENDIF HELP
          "     �࣠�"
          LABEL "     �࣠�"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[3]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 5
          &ELSE AT ROW 11 COL 5 &ENDIF HELP
          "��� ���⠭�������"
          LABEL "��� ���⠭�������"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[4]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 9
          &ELSE AT ROW 12 COL 9 &ENDIF HELP
          "���⠭�������" FORMAT "x(600)"
          LABEL "���⠭�������"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
      mKodNO
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 47
          &ELSE AT ROW 12 COL 47 &ENDIF HELP
          "��� �࣠��" FORMAT "x(4)"
          LABEL "��� �࣠��"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
      
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-BlockAcct.datarewspr$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 66 COLON-ALIGNED
          &ELSE AT ROW 12 COL 66 COLON-ALIGNED &ENDIF
          LABEL "��" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-BlockAcct.txt[5]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 12
          &ELSE AT ROW 14 COL 12 &ENDIF HELP
          "     �࣠�"
          LABEL "     �࣠�"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[6]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 5
          &ELSE AT ROW 15 COL 5 &ENDIF HELP
          "��� ���⠭�������"
          LABEL "��� ���⠭�������"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-BlockAcct.txt[7]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 9
          &ELSE AT ROW 16 COL 9 &ENDIF HELP
          "���⠭�������" FORMAT "x(600)"
          LABEL "���⠭�������"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
      mKodNO_OT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 47
          &ELSE AT ROW 16 COL 47 &ENDIF HELP
          "��� �࣠��" FORMAT "x(4)"
          LABEL "��� �࣠��"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF

          tt-BlockAcct.datarewsot$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 66 COLON-ALIGNED
          &ELSE AT ROW 16 COL 66 COLON-ALIGNED &ENDIF
          LABEL "��" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
          
     tt-BlockAcct.txt[8]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 8
          &ELSE AT ROW 17 COL 8 &ENDIF
          LABEL "���.���ଠ��" FORMAT "x(120)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 54 BY 1
          &ELSE SIZE 54 BY 1 &ENDIF
     "���ଠ�� � ��⨨ ��࠭�祭��" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 39 BY 1
          &ELSE SIZE 39 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 2
          &ELSE AT ROW 13 COL 2 &ENDIF
     "���ଠ�� �� ��⠭������� ��࠭�祭��" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 76 BY 1
          &ELSE SIZE 76 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 2
          &ELSE AT ROW 9 COL 2 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-BlockAcct T "?" NO-UNDO bisquit BlockObject
      ADDITIONAL-FIELDS:
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-BlockAcct" "" }
          
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
/* SETTINGS FOR FILL-IN tt-BlockAcct.beg-datetime IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-BlockAcct.block-type IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-BlockAcct.datarewspr$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN tt-BlockAcct.end-datetime IN FRAME fMain
   2 3                                                                  */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[1] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[2] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[3] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[4] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[5] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[6] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[7] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL EXP-HELP                                   */
/* SETTINGS FOR FILL-IN tt-BlockAcct.txt[8] IN FRAME fMain
   ALIGN-L 1 2 3 4 EXP-LABEL                                            */
/* SETTINGS FOR FILL-IN tt-BlockAcct.user-id IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[1] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[2] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[3] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
/* SETTINGS FOR FILL-IN tt-BlockAcct.val[4] IN FRAME fMain
   1 3 4 EXP-LABEL EXP-HELP                                             */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-BlockAcct"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME mKodNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo TERMINAL-SIMULATION
ON F1 OF mKodNo IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "������࣠�" 
      AND   code.parent EQ "������࣠�" 
      AND   code.code   EQ tt-BlockAcct.txt[2]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      IF iMode NE {&MOD_VIEW} 
      THEN DO:
         {empty ttRetVal}
         RUN browseld.p ("code",
                         "class~001parent~001RetFld~001RetType~001RetRcp",
                         TRIM(code.val) + "~001" + TRIM(code.val) + 
                         "~001code~001Singl~001" + 
                         STRING(TEMP-TABLE ttRetVal:HANDLE),
                         "class",
                         iLevel + 1).
         FOR EACH ttRetVal:
            SELF:SCREEN-VALUE = ttRetVal.PickValue.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME mKodNo_OT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo_OT TERMINAL-SIMULATION
ON F1 OF mKodNo_OT IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "������࣠�" 
      AND   code.parent EQ "������࣠�" 
      AND   code.code   EQ tt-BlockAcct.txt[5]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      IF iMode NE {&MOD_VIEW} 
      THEN DO:
         {empty ttRetVal}
         RUN browseld.p ("code",
                         "class~001parent~001RetFld~001RetType~001RetRcp",
                         TRIM(code.val) + "~001" + TRIM(code.val) + 
                         "~001code~001Singl~001" + 
                         STRING(TEMP-TABLE ttRetVal:HANDLE),
                         "class",
                         iLevel + 1).
         FOR EACH ttRetVal:
            SELF:SCREEN-VALUE = ttRetVal.PickValue.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.txt[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[1] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[1] IN FRAME fMain /* ��।����� */
DO:
   IF iMode NE {&MOD_VIEW} 
   THEN DO :
      {empty ttRetVal}
      RUN browseld.p ("code",
                      "class~001parent~001RetFld~001RetType~001RetRcp",
                      "order-pay~001order-pay~001code~001Multi~001" + STRING(TEMP-TABLE ttRetVal:HANDLE),
                      "class",
                      iLevel + 1).
      FOR EACH ttRetVal:
         {additem.i SELF:SCREEN-VALUE ttRetVal.PickValue}
      END.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.block-type TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-BlockAcct.block-type IN FRAME fMain /* ��।����� */
DO:
   mTmpStr = REPLACE(GetCode("acct-status",SELF:SCREEN-VALUE),"�珫��(","").
   mTmpStr = REPLACE(mTmpStr,")","").
   tt-BlockAcct.txt[1]:SCREEN-VALUE = mTmpStr.

   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.block-type TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.block-type IN FRAME fMain /* ��।����� */
DO:
   {&BEG_BT_LEAVE}

   ASSIGN tt-BlockAcct.block-type.

   FIND FIRST code WHERE
       code.class EQ "acct-status"
   AND code.code  EQ tt-BlockAcct.block-type
   NO-LOCK NO-ERROR.
   IF AVAIL(code) THEN
   DO:
      IF NOT GetPermission(
         "code",
         code.class + "," + tt-BlockAcct.block-type,
         "w")
      THEN
      DO:
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.

   IF INDEX(SELF:SCREEN-VALUE,"*")  > 0 THEN DO:

      RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                  "��� �����஢�� ᮤ�ন� �������⨬� ᨬ��� '*'!"). 
      {return_no_apply.i '{&RET-ERROR}'}

   END.
   {&END_BT_LEAVE}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.block-type TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.end-datetime IN FRAME fMain /* ��।����� */
DO:
   {&BEG_BT_LEAVE}

   ASSIGN tt-BlockAcct.end-datetime.

   FIND FIRST code WHERE
       code.class EQ "acct-status"
   AND code.code  EQ tt-BlockAcct.block-type:SCREEN-VALUE
   NO-LOCK NO-ERROR.
   IF AVAIL(code) 
   AND {assigned tt-BlockAcct.end-datetime:SCREEN-VALUE}THEN
   DO:
      IF NOT GetPermission(
         "code",
         code.class + "," + tt-BlockAcct.block-type:SCREEN-VALUE,
         "w")
      THEN
      DO:
         {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.

   {&END_BT_LEAVE}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.beg-datetime TERMINAL-SIMULATION
ON ENTRY OF tt-BlockAcct.beg-datetime IN FRAME fMain
DO:
   IF NOT {assigned SELF:SCREEN-VALUE} THEN
      SELF:SCREEN-VALUE = STRING(TODAY, "99/99/9999") + " " + STRING(TIME, "HH:MM").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[1] TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.txt[1] IN FRAME fMain /* ��।����� */
DO:
   {&BEG_BT_LEAVE}
   IF {assigned SELF:SCREEN-VALUE} THEN DO:
      DO mNum = 1 TO NUM-ENTRIES(SELF:SCREEN-VALUE):
         IF GetCodeName("order-pay",
                        ENTRY(mNum,SELF:SCREEN-VALUE))EQ ? THEN 
         DO:
             RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                         "��।����� ���⥦� � ����� " + ENTRY(mNum,SELF:SCREEN-VALUE) + " ���������!"). 
             {return_no_apply.i '{&RET-ERROR}'}
         END.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo TERMINAL-SIMULATION
ON LEAVE OF mKodNo IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "������࣠�" 
      AND   code.parent EQ "������࣠�" 
      AND   code.code   EQ tt-BlockAcct.txt[2]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      {&BEG_BT_LEAVE}
      IF {assigned SELF:SCREEN-VALUE} THEN DO:
            IF GetCodeName(TRIM(code.val),
                           SELF:SCREEN-VALUE)EQ ? THEN 
            DO:
                RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                            "�࣠� " + 
                                            tt-BlockAcct.txt[2]:SCREEN-VALUE + 
                                            " � ����� " + SELF:SCREEN-VALUE + 
                                            " ���������!").
                mKodNoOK = NO. 
                {return_no_apply.i '{&RET-ERROR}'}
            END.
            ELSE mKodNoOK = YES.
      END.
      {&END_BT_LEAVE}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKodNo_OT TERMINAL-SIMULATION
ON LEAVE OF mKodNo_OT IN FRAME fMain
DO:
   FIND FIRST code WHERE 
            code.class  EQ "������࣠�" 
      AND   code.parent EQ "������࣠�" 
      AND   code.code   EQ tt-BlockAcct.txt[5]:SCREEN-VALUE 
   NO-LOCK NO-ERROR.
   IF       AVAIL(code) 
      AND   TRIM(code.val) NE "" THEN
   DO:
      {&BEG_BT_LEAVE}
      IF {assigned SELF:SCREEN-VALUE} THEN DO:
            IF GetCodeName(TRIM(code.val),
                           SELF:SCREEN-VALUE)EQ ? THEN 
            DO:
                RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                            "�࣠� " + 
                                            tt-BlockAcct.txt[5]:SCREEN-VALUE + 
                                            " � ����� " + SELF:SCREEN-VALUE + 
                                            " ���������!").
                mKodNoOK = NO. 
                {return_no_apply.i '{&RET-ERROR}'}
            END.
            ELSE mKodNoOK = YES.
      END.
      {&END_BT_LEAVE}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.txt[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[2] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[2] IN FRAME fMain /* ��� �࣠�� */
DO:
  IF {assigned SELF:SCREEN-VALUE} THEN DO:
     RUN shifr.p("������࣠�",SELF:SCREEN-VALUE,iLevel + 1).
  END.
  ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
      RUN codelay.p("������࣠�",?,?,iLevel + 1).
      IF     {&LAST_KEY} = 10 
         AND pick-value NE ? THEN
        SELF:SCREEN-VALUE = pick-value. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.txt[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[3] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[3] IN FRAME fMain /* ��� ���⠭������� */
DO:
    IF {assigned SELF:SCREEN-VALUE} THEN DO:
       RUN shifr.p("�।��ᠭ��",SELF:SCREEN-VALUE,iLevel + 1).
    END.
    ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
        RUN codelay.p("�।��ᠭ��",?,?,iLevel + 1).
        IF     {&LAST_KEY} = 10 
           AND pick-value NE ? THEN
          SELF:SCREEN-VALUE = pick-value. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[3] TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.txt[3] IN FRAME fMain /* ��� ���⠭������� */
DO:
   {&BEG_BT_LEAVE}
   IF {assigned SELF:SCREEN-VALUE} THEN DO:
      IF GetCodeName("�।��ᠭ��",
                     SELF:SCREEN-VALUE)EQ ? THEN 
      DO:
          RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                      "��� �।��ᠭ�� � ����� " + SELF:SCREEN-VALUE + " ���������!"). 
          {return_no_apply.i '{&RET-ERROR}'}
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-BlockAcct.txt[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[5] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[5] IN FRAME fMain /* ��� �࣠�� */
DO:
   IF {assigned SELF:SCREEN-VALUE} THEN DO:
      RUN shifr.p("������࣠�",SELF:SCREEN-VALUE,iLevel + 1).
   END.
   ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
      RUN codelay.p("������࣠�",?,?,iLevel + 1).
      IF     {&LAST_KEY} = 10 
         AND pick-value NE ? THEN
         SELF:SCREEN-VALUE = pick-value. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-BlockAcct.txt[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[6] TERMINAL-SIMULATION
ON F1 OF tt-BlockAcct.txt[6] IN FRAME fMain /* ��� ���⠭������� */
DO:
    IF {assigned SELF:SCREEN-VALUE} THEN DO:
       RUN shifr.p("�।��ᠭ��",SELF:SCREEN-VALUE,iLevel + 1).
    END.
    ELSE IF iMode NE {&MOD_VIEW} THEN DO TRANS:
        RUN codelay.p("�।��ᠭ��",?,?,iLevel + 1).
        IF     {&LAST_KEY} = 10 
           AND pick-value NE ? THEN
          SELF:SCREEN-VALUE = pick-value. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-BlockAcct.txt[6] TERMINAL-SIMULATION
ON LEAVE OF tt-BlockAcct.txt[6] IN FRAME fMain /* ��� ���⠭������� */
DO:
    {&BEG_BT_LEAVE}
    IF {assigned SELF:SCREEN-VALUE} THEN DO:
       IF GetCodeName("�।��ᠭ��",
                      SELF:SCREEN-VALUE)EQ ? THEN 
       DO:
           RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                       "��� �।��ᠭ�� � ����� " + SELF:SCREEN-VALUE + " ���������!"). 
           {return_no_apply.i '{&RET-ERROR}'}
       END.
    END.
    {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.val[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL  tt-BlockAcct.val TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-BlockAcct.val[3] IN FRAME fMain
DO:
    {&BEG_BT_LEAVE}
    IF {assigned SELF:SCREEN-VALUE} AND iMode EQ {&MOD_ADD} THEN DO:
         IF ENTRY(2,tt-blockacct.surrogate) EQ "" 
         THEN tt-BlockAcct.val[4] = tt-BlockAcct.val[3].
         ELSE tt-BlockAcct.val[4] = ROUND(100 * DEC(tt-BlockAcct.val[3]:SCREEN-VALUE)
                                              / CurFromBase("�������", 
                                                              ENTRY(2,tt-blockacct.surrogate), 
                                                              gend-date, 
                                                              100),2).
         tt-BlockAcct.val[4]:SCREEN-VALUE = STRING(tt-BlockAcct.val[4]).
    END.
    {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-BlockAcct.val[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL  tt-BlockAcct.val[4] TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-BlockAcct.val[4] IN FRAME fMain
DO:
    {&BEG_BT_LEAVE}
    IF {assigned SELF:SCREEN-VALUE} AND iMode EQ {&MOD_ADD} THEN DO:
         IF ENTRY(2,tt-blockacct.surrogate) EQ "" 
         THEN tt-BlockAcct.val[3] = DEC(tt-BlockAcct.val[4]:SCREEN-VALUE).
         ELSE tt-BlockAcct.val[3] = ROUND(CurFromBase("�������",
                                                          ENTRY(2,tt-blockacct.surrogate), 
                                                          gend-date, 
                                                          DEC(tt-BlockAcct.val[4]:SCREEN-VALUE)),2).
         tt-BlockAcct.val[3]:SCREEN-VALUE = STRING(tt-BlockAcct.val[3]).
         tt-BlockAcct.val[4] = DEC(tt-BlockAcct.val[4]:SCREEN-VALUE).
    END.
    {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */
&IF DEFINED(IF_DEFINED_THEN_THIS_BLOCK_CAN_BE_DELETED) EQ 0 &THEN
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
   {return_no_apply.i}
END.
ON ENDKEY, END-ERROR OF FRAME fMain ANYWHERE DO:
   mRetVal = IF mOnlyForm THEN
      {&RET-ERROR}
      ELSE 
         "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   {return_no_apply.i}
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

   ASSIGN
      mFile      = ENTRY(2, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) GT 1
      mSurrogate = ENTRY(3, iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) GT 2
   .
   IF iMode = {&MOD_ADD} THEN
      ASSIGN
         tt-blockacct.FILE      = mFile
         tt-blockacct.surrogate = mSurrogate
      .

   /* ������塞 COMBO-BOX'� ����묨 �� ����奬� */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* ���ᢥ⪠ ����� �� LIST-5 (����ந�� ��� ᥡ� )*/
   RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                    REPLACE("{&LIST-5}"," ",","),
                    "bright-green").

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
   {wait_for.i 
      &THIS_FRAME  = "fMain"
      &ENTRY_FOCUS = "mFirstTabItem"
      &EXTEXT      = "CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem"
      {&*}}.
END.

/* Commented by KSV: ����뢠�� �㦡� ��⥬��� ᮮ�饭�� */
RUN End-SysMes.

RUN disable_ui.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION 
PROCEDURE Local_go :
   /* izm SAM */
   DEFINE BUFFER code FOR code.

   FOR EACH code WHERE code.class  EQ "acct-status"
                   AND code.parent EQ "acct-status"
                   AND code.code   EQ tt-BlockAcct.block-type:SCREEN-VALUE IN FRAME fMain NO-LOCK:
      IF code.misc[1] EQ "�����㬬" AND
         DECIMAL(tt-BlockAcct.val[3]:SCREEN-VALUE IN FRAME fMain) GE 0 THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                     "��� ������� ⨯� �����஢�� � ���� ����� ���⪠" + CHR(10) +
                                     "�㬬� ������ ���� ����⥫쭮�!").
         RETURN ERROR.
      END.
   END.
     
   /* end izm SAM */

    IF tt-BlockAcct.block-type:SCREEN-VALUE IN FRAME fMain = "�����㬬" AND
       DECIMAL(tt-BlockAcct.val[3]:SCREEN-VALUE IN FRAME fMain) = 0 THEN DO:
          RUN Fill-SysMes IN h_tmess ("", "", "-1",
                                    "��� �����஢�� ⨯� \"�����㬬\" " +
                                    "�㬬� �����஢�� �� ����� ���� �㫥���").
        RETURN ERROR.
    END.

   DO WITH FRAME fMain:
      IF mKodNoOK THEN
      ASSIGN
         tt-BlockAcct.txt[4]:SCREEN-VALUE = tt-BlockAcct.txt[4]:SCREEN-VALUE + ";" + mKodNo:SCREEN-VALUE
         tt-BlockAcct.txt[7]:SCREEN-VALUE = tt-BlockAcct.txt[7]:SCREEN-VALUE + ";" + mKodNo_OT:SCREEN-VALUE
      .   
   END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject:
   DO WITH FRAME fMain:      
      IF NUM-ENTRIES(tt-BlockAcct.txt[4],";") GE 2 THEN DO:
         mKodNo    = ENTRY(2,tt-BlockAcct.txt[4],";").
         DISPLAY
            mKodNo
         .
      END.
      IF NUM-ENTRIES(tt-BlockAcct.txt[7],";") GE 2 THEN DO:
         mKodNo_OT = ENTRY(2,tt-BlockAcct.txt[7],";").
         DISPLAY
            mKodNo
         .
      END.
      DISPLAY
         ENTRY(1,tt-BlockAcct.txt[4],";") @ tt-BlockAcct.txt[4]
         ENTRY(1,tt-BlockAcct.txt[7],";") @ tt-BlockAcct.txt[7]
      .
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
  IF AVAILABLE tt-BlockAcct THEN 
    DISPLAY tt-BlockAcct.block-type tt-BlockAcct.beg-datetime 
          tt-BlockAcct.end-datetime tt-BlockAcct.val[1] tt-BlockAcct.val[2] 
          tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.user-id 
          tt-BlockAcct.txt[2] tt-BlockAcct.txt[3]  mKodNo mKodNo_OT 
          tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] 
          tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-BlockAcct.block-type tt-BlockAcct.beg-datetime 
         tt-BlockAcct.end-datetime tt-BlockAcct.val[1] tt-BlockAcct.val[2] 
         tt-BlockAcct.val[3] tt-BlockAcct.txt[1] tt-BlockAcct.user-id 
         tt-BlockAcct.txt[2] tt-BlockAcct.txt[3] tt-BlockAcct.txt[4] mKodNo mKodNo_OT
         tt-BlockAcct.datarewspr$ tt-BlockAcct.txt[5] tt-BlockAcct.txt[6] 
         tt-BlockAcct.datarewsot$ tt-BlockAcct.txt[7] tt-BlockAcct.txt[8] tt-BlockAcct.val[4]
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTFILE='f-blkac.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='anba' */
/* $LINTDATE='06/04/2016 10:53:18.490+04:00' */
/*prosignjoEu5bYgzoEXtmv8Fg2Gcg*/
/* --- f-blkac.p was humbly modified by (c)blodd converter v.1.09 on 12/2/2016 11:50am --- */
