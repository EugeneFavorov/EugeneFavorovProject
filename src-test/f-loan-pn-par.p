&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loan-pn-par NO-UNDO LIKE loan
       FIELD naznplatvvod$               AS CHARACTER /* �������₢�� */
       FIELD pnswcet$                    AS INT64   /* ����� */
       FIELD pnvidplatewza$              AS CHARACTER /* ��������⥦� */
       FIELD pnotdsummami$               AS LOGICAL   /* ���⤑㬬��� */
       FIELD pnformablanka$              AS CHARACTER /* ����ଠ������ */
       FIELD pntipsroka$                 AS CHARACTER /* ������ப� */
       FIELD pnsdvigsroka$               AS INT64   /* ��������ப� */
       FIELD pnsrok$                     AS INT64   /* ���ப */
       FIELD pnispwstrihkod$             AS LOGICAL   /* ���ᯘ��劮� */
       FIELD pnwstrihkod$                AS CHARACTER /* �����劮� */
       FIELD pnnaznawcenieplatewza$      AS CHARACTER /* �������祭�����⥦� */
       FIELD pnnaznplatkomissiwa$        AS CHARACTER /* ���������⊮����� */
       FIELD pnplatneprovodwatswa$       AS LOGICAL   /* �����⍥�஢������ */
       FIELD pnkassplan$                 AS CHARACTER /* �����Ꮻ�� */
       FIELD pnispvedkvit$               AS LOGICAL   /* ���ᯂ������ */
       FIELD pnvedkvit$                  AS CHARACTER /* ��������� */
       FIELD klassmetashemy$             AS CHARACTER /* ����ጥ��奬� */
       FIELD is_mandatory AS LOGICAL /* is_mandatory */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       FIELD licence         AS CHARACTER /* ��業��� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-loan-pn-par" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: f-loan-pn-par.p
      Comment: ��ଠ ����� loan-pn-par
   Parameters:
         Uses:
      Used by:
      Created: 18.08.2009 ushd 102466
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
{intrface.get cust}
{intrface.get db2l}

DEFINE VAR mParentContract LIKE loan.parent-contract  NO-UNDO.
DEFINE VAR mParentContCode LIKE loan.parent-cont-code NO-UNDO.
DEFINE BUFFER xsignstest  FOR signs.
DEFINE BUFFER xsignstest2 FOR signs.
DEFINE BUFFER xsessions   FOR sessions.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-loan-pn-par

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-loan-pn-par.pnvidplatewza$ ~
tt-loan-pn-par.cont-cli tt-loan-pn-par.pnotdsummami$ ~
tt-loan-pn-par.pnformablanka$ tt-loan-pn-par.pntipsroka$ ~
tt-loan-pn-par.pnsrok$ tt-loan-pn-par.pnispwstrihkod$ ~
tt-loan-pn-par.pnwstrihkod$ tt-loan-pn-par.pnnaznawcenieplatewza$ ~
tt-loan-pn-par.pnnaznplatkomissiwa$ tt-loan-pn-par.pnplatneprovodwatswa$ ~
tt-loan-pn-par.pnkassplan$ tt-loan-pn-par.pnispvedkvit$ ~
tt-loan-pn-par.pnvedkvit$ tt-loan-pn-par.klassmetashemy$ ~
tt-loan-pn-par.naznplatvvod$ 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-loan-pn-par.pnvidplatewza$ ~
tt-loan-pn-par.cont-cli tt-loan-pn-par.pnotdsummami$ ~
tt-loan-pn-par.pnformablanka$ tt-loan-pn-par.pntipsroka$ ~
tt-loan-pn-par.pnsrok$ tt-loan-pn-par.pnispwstrihkod$ ~
tt-loan-pn-par.pnwstrihkod$ tt-loan-pn-par.pnnaznawcenieplatewza$ ~
tt-loan-pn-par.pnnaznplatkomissiwa$ tt-loan-pn-par.pnplatneprovodwatswa$ ~
tt-loan-pn-par.pnkassplan$ tt-loan-pn-par.pnispvedkvit$ ~
tt-loan-pn-par.pnvedkvit$ tt-loan-pn-par.klassmetashemy$ ~
tt-loan-pn-par.naznplatvvod$ 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-loan-pn-par
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-loan-pn-par
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loan-pn-par SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-loan-pn-par SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loan-pn-par
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loan-pn-par


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loan-pn-par.pnvidplatewza$ ~
tt-loan-pn-par.cont-cli tt-loan-pn-par.pnotdsummami$ ~
tt-loan-pn-par.pnformablanka$ tt-loan-pn-par.pntipsroka$ ~
tt-loan-pn-par.pnsrok$ tt-loan-pn-par.pnispwstrihkod$ ~
tt-loan-pn-par.pnwstrihkod$ tt-loan-pn-par.pnnaznawcenieplatewza$ ~
tt-loan-pn-par.pnnaznplatkomissiwa$ tt-loan-pn-par.pnplatneprovodwatswa$ ~
tt-loan-pn-par.pnkassplan$ tt-loan-pn-par.pnispvedkvit$ ~
tt-loan-pn-par.pnvedkvit$ tt-loan-pn-par.klassmetashemy$ ~
tt-loan-pn-par.naznplatvvod$ 
&Scoped-define ENABLED-TABLES tt-loan-pn-par
&Scoped-define FIRST-ENABLED-TABLE tt-loan-pn-par
&Scoped-Define ENABLED-OBJECTS mAcct mReceiver mSdvigChar mOnline 
&Scoped-Define DISPLAYED-FIELDS tt-loan-pn-par.pnvidplatewza$ ~
tt-loan-pn-par.cont-cli tt-loan-pn-par.pnotdsummami$ ~
tt-loan-pn-par.pnformablanka$ tt-loan-pn-par.pntipsroka$ ~
tt-loan-pn-par.pnsrok$ tt-loan-pn-par.pnispwstrihkod$ ~
tt-loan-pn-par.pnwstrihkod$ tt-loan-pn-par.pnnaznawcenieplatewza$ ~
tt-loan-pn-par.pnnaznplatkomissiwa$ tt-loan-pn-par.pnplatneprovodwatswa$ ~
tt-loan-pn-par.pnkassplan$ tt-loan-pn-par.pnispvedkvit$ ~
tt-loan-pn-par.pnvedkvit$ tt-loan-pn-par.klassmetashemy$ ~
tt-loan-pn-par.naznplatvvod$ 
&Scoped-define DISPLAYED-TABLES tt-loan-pn-par
&Scoped-define FIRST-DISPLAYED-TABLE tt-loan-pn-par
&Scoped-Define DISPLAYED-OBJECTS mAcct mReceiver mSdvigChar mOnline 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loan-pn-par.pnvidplatewza$ tt-loan-pn-par.cont-cli ~
mAcct mReceiver tt-loan-pn-par.pnotdsummami$ tt-loan-pn-par.pnformablanka$ ~
tt-loan-pn-par.pntipsroka$ mSdvigChar tt-loan-pn-par.pnsrok$ ~
tt-loan-pn-par.pnispwstrihkod$ tt-loan-pn-par.pnwstrihkod$ ~
tt-loan-pn-par.pnnaznawcenieplatewza$ tt-loan-pn-par.pnnaznplatkomissiwa$ ~
tt-loan-pn-par.pnplatneprovodwatswa$ tt-loan-pn-par.pnkassplan$ ~
tt-loan-pn-par.pnispvedkvit$ tt-loan-pn-par.pnvedkvit$ ~
tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.naznplatvvod$ mOnline 
&Scoped-define List-2 tt-loan-pn-par.pnvidplatewza$ tt-loan-pn-par.cont-cli ~
mAcct mReceiver tt-loan-pn-par.pnotdsummami$ tt-loan-pn-par.pnformablanka$ ~
tt-loan-pn-par.pntipsroka$ mSdvigChar tt-loan-pn-par.pnsrok$ ~
tt-loan-pn-par.pnispwstrihkod$ tt-loan-pn-par.pnwstrihkod$ ~
tt-loan-pn-par.pnnaznawcenieplatewza$ tt-loan-pn-par.pnnaznplatkomissiwa$ ~
tt-loan-pn-par.pnplatneprovodwatswa$ tt-loan-pn-par.pnkassplan$ ~
tt-loan-pn-par.pnispvedkvit$ tt-loan-pn-par.pnvedkvit$ ~
tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.naznplatvvod$ mOnline 
&Scoped-define List-3 tt-loan-pn-par.pnvidplatewza$ tt-loan-pn-par.cont-cli ~
mAcct mReceiver tt-loan-pn-par.pnotdsummami$ tt-loan-pn-par.pnformablanka$ ~
tt-loan-pn-par.pntipsroka$ mSdvigChar tt-loan-pn-par.pnsrok$ ~
tt-loan-pn-par.pnispwstrihkod$ tt-loan-pn-par.pnwstrihkod$ ~
tt-loan-pn-par.pnnaznawcenieplatewza$ tt-loan-pn-par.pnnaznplatkomissiwa$ ~
tt-loan-pn-par.pnplatneprovodwatswa$ tt-loan-pn-par.pnkassplan$ ~
tt-loan-pn-par.pnispvedkvit$ tt-loan-pn-par.pnvedkvit$ ~
tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.naznplatvvod$ 
&Scoped-define List-4 tt-loan-pn-par.pnvidplatewza$ tt-loan-pn-par.cont-cli ~
mAcct mReceiver tt-loan-pn-par.pnotdsummami$ tt-loan-pn-par.pnformablanka$ ~
tt-loan-pn-par.pntipsroka$ tt-loan-pn-par.pnsrok$ ~
tt-loan-pn-par.pnispwstrihkod$ tt-loan-pn-par.pnwstrihkod$ ~
tt-loan-pn-par.pnnaznawcenieplatewza$ tt-loan-pn-par.pnnaznplatkomissiwa$ ~
tt-loan-pn-par.pnplatneprovodwatswa$ tt-loan-pn-par.pnkassplan$ ~
tt-loan-pn-par.pnispvedkvit$ tt-loan-pn-par.pnvedkvit$ ~
tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.naznplatvvod$ mOnline 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mSdvigChar AS CHARACTER FORMAT "X(2)":U 
     LABEL "�����" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "<-","--","->" 
     DROP-DOWN-LIST
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mAcct AS CHARACTER FORMAT "x(20)":U INITIAL "0" 
     LABEL "���" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mOnline AS CHARACTER FORMAT "X(4)":U 
     LABEL "On-line" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
     &ELSE SIZE 5 BY 1 &ENDIF.

DEFINE VARIABLE mReceiver AS CHARACTER FORMAT "X(1000)":U 
     LABEL "�����⥫�" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
     &ELSE SIZE 55 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loan-pn-par SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-loan-pn-par.pnvidplatewza$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 20 COLON-ALIGNED
          &ELSE AT ROW 2 COL 20 COLON-ALIGNED &ENDIF HELP
          "��� ���� ���⥦�"
          LABEL "��� ���⥦�" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     tt-loan-pn-par.cont-cli
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 20 COLON-ALIGNED
          &ELSE AT ROW 3 COL 20 COLON-ALIGNED &ENDIF HELP
          "������������ ��ࠬ��� ���⥦�"
          LABEL "����. ��ࠬ" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     mAcct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 20 COLON-ALIGNED
          &ELSE AT ROW 4 COL 20 COLON-ALIGNED &ENDIF HELP
          "��� �����⥫� ���⥦�"
     mReceiver
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 20 COLON-ALIGNED
          &ELSE AT ROW 5 COL 20 COLON-ALIGNED &ENDIF HELP
          "�����⥫�"
     tt-loan-pn-par.pnotdsummami$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 20 COLON-ALIGNED
          &ELSE AT ROW 6 COL 20 COLON-ALIGNED &ENDIF HELP
          "�������� �⤥��묨 �㬬���"
          LABEL "��. �㬬���" FORMAT "��/���"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-loan-pn-par.pnformablanka$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 48 COLON-ALIGNED
          &ELSE AT ROW 6 COL 48 COLON-ALIGNED &ENDIF HELP
          "��ଠ ������"
          LABEL "�����" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-loan-pn-par.pntipsroka$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 20 COLON-ALIGNED
          &ELSE AT ROW 7 COL 20 COLON-ALIGNED &ENDIF HELP
          "��� �ப� ����᫥��� �।��"
          LABEL "��� �ப�" FORMAT "x(1)"
          VIEW-AS COMBO-BOX INNER-LINES 3
          LIST-ITEMS "�","�","�" 
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     mSdvigChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 34 COLON-ALIGNED
          &ELSE AT ROW 7 COL 34 COLON-ALIGNED &ENDIF HELP
          "����� �ப� ����᫥��� �।��"
     tt-loan-pn-par.pnsrok$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 48 COLON-ALIGNED
          &ELSE AT ROW 7 COL 48 COLON-ALIGNED &ENDIF HELP
          "�ப ����᫥��� �।��"
          LABEL "�ப" FORMAT ">9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-loan-pn-par.pnispwstrihkod$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 20 COLON-ALIGNED
          &ELSE AT ROW 8 COL 20 COLON-ALIGNED &ENDIF HELP
          "�ਧ��� �ᯮ�짮����� ����-����"
          LABEL "��.����-���" FORMAT "��/���"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 7 BY 1
          &ELSE SIZE 7 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 15.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-loan-pn-par.pnwstrihkod$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 48 COLON-ALIGNED
          &ELSE AT ROW 8 COL 48 COLON-ALIGNED &ENDIF HELP
          "��뫪� �� �����䨪��� ����-�����"
          LABEL "����-���" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-loan-pn-par.pnnaznawcenieplatewza$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 20 COLON-ALIGNED
          &ELSE AT ROW 9 COL 20 COLON-ALIGNED &ENDIF HELP
          "�����祭�� ���⥦�"
          LABEL "����.���⥦�" FORMAT "x(250)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-loan-pn-par.pnnaznplatkomissiwa$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 20 COLON-ALIGNED
          &ELSE AT ROW 10 COL 20 COLON-ALIGNED &ENDIF HELP
          "�����祭�� ���⥦� ��� �����ᨨ"
          LABEL "����.���⥦� �����." FORMAT "x(250)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     tt-loan-pn-par.pnplatneprovodwatswa$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 20 COLON-ALIGNED
          &ELSE AT ROW 11 COL 20 COLON-ALIGNED &ENDIF HELP
          "���⥦� �� �஢������"
          LABEL "����.�� �஢������" FORMAT "��/���"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-loan-pn-par.pnkassplan$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 48 COLON-ALIGNED
          &ELSE AT ROW 11 COL 48 COLON-ALIGNED &ENDIF HELP
          "������ ���ᮢ��� �����"
          LABEL "����.����" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 2 BY 1
          &ELSE SIZE 2 BY 1 &ENDIF
     tt-loan-pn-par.pnispvedkvit$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 20 COLON-ALIGNED
          &ELSE AT ROW 12 COL 20 COLON-ALIGNED &ENDIF HELP
          "�ᯮ������ ������⢥���� ���⠭��"
          LABEL "��.���.����" FORMAT "��/���"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-loan-pn-par.pnvedkvit$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 48 COLON-ALIGNED
          &ELSE AT ROW 12 COL 48 COLON-ALIGNED &ENDIF HELP
          "������⢥���� ���⠭��"
          LABEL "���.����" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-loan-pn-par.klassmetashemy$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 20 COLON-ALIGNED
          &ELSE AT ROW 13 COL 20 COLON-ALIGNED &ENDIF HELP
          "����� ����奬�"
          LABEL "�����" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 15.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-loan-pn-par.naznplatvvod$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 20 COLON-ALIGNED
          &ELSE AT ROW 13 COL 20 COLON-ALIGNED &ENDIF HELP
          "�����祭�� ���⥦� ��� �����"
          LABEL "����.���⥦� ����" FORMAT "x(400)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
     mOnline
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 48 COLON-ALIGNED
          &ELSE AT ROW 13 COL 48 COLON-ALIGNED &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 15
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loan-pn-par T "?" NO-UNDO bisquit loan
      ADDITIONAL-FIELDS:
          FIELD naznplatvvod$               AS CHARACTER /* �������₢�� */
          FIELD pnswcet$                    AS INT64   /* ����� */
          FIELD pnvidplatewza$              AS CHARACTER /* ��������⥦� */
          FIELD pnotdsummami$               AS LOGICAL   /* ���⤑㬬��� */
          FIELD pnformablanka$              AS CHARACTER /* ����ଠ������ */
          FIELD pntipsroka$                 AS CHARACTER /* ������ப� */
          FIELD pnsdvigsroka$               AS INT64   /* ��������ப� */
          FIELD pnsrok$                     AS INT64   /* ���ப */
          FIELD pnispwstrihkod$             AS LOGICAL   /* ���ᯘ��劮� */
          FIELD pnwstrihkod$                AS CHARACTER /* �����劮� */
          FIELD pnnaznawcenieplatewza$      AS CHARACTER /* �������祭�����⥦� */
          FIELD pnnaznplatkomissiwa$        AS CHARACTER /* ���������⊮����� */
          FIELD pnplatneprovodwatswa$       AS LOGICAL   /* �����⍥�஢������ */
          FIELD pnkassplan$                 AS CHARACTER /* �����Ꮻ�� */
          FIELD pnispvedkvit$               AS LOGICAL   /* ���ᯂ������ */
          FIELD pnvedkvit$                  AS CHARACTER /* ��������� */
          FIELD klassmetashemy$             AS CHARACTER /* ����ጥ��奬� */
          FIELD is_mandatory AS LOGICAL /* is_mandatory */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          FIELD licence         AS CHARACTER /* ��業��� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-loan-pn-par" "" }
          
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
         HEIGHT             = 24.07
         WIDTH              = 97.14
         MAX-HEIGHT         = 24.07
         MAX-WIDTH          = 97.14
         VIRTUAL-HEIGHT     = 24.07
         VIRTUAL-WIDTH      = 97.14
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
/* SETTINGS FOR FILL-IN tt-loan-pn-par.cont-cli IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.klassmetashemy$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN mAcct IN FRAME fMain
   1 2 3 4                                                              */
/* SETTINGS FOR FILL-IN mOnline IN FRAME fMain
   1 2 4                                                                */
/* SETTINGS FOR FILL-IN mReceiver IN FRAME fMain
   3                                                                    */
/* SETTINGS FOR COMBO-BOX mSdvigChar IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.naznplatvvod$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnformablanka$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnispvedkvit$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnispwstrihkod$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnkassplan$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnnaznawcenieplatewza$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnnaznplatkomissiwa$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnotdsummami$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnplatneprovodwatswa$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnsrok$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR COMBO-BOX tt-loan-pn-par.pntipsroka$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnvedkvit$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnvidplatewza$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-loan-pn-par.pnwstrihkod$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-loan-pn-par"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-loan-pn-par.klassmetashemy$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.klassmetashemy$ TERMINAL-SIMULATION
ON F1 OF tt-loan-pn-par.klassmetashemy$ IN FRAME fMain /* ����� */
DO:
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
      pick-value = ?.
      RUN getclass.p ("�������� ����� ����������",
                      "opb-pay",
                      YES,
                      "C",
                      4).
      IF  LASTKEY     = 10
      AND {assigned pick-value} THEN DO:
         tt-loan-pn-par.klassmetashemy$ = pick-value.
         DISPLAY tt-loan-pn-par.klassmetashemy$.
      END.
   END.
   ELSE DO:
      {&BT_F1}
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.klassmetashemy$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-pn-par.klassmetashemy$ IN FRAME fMain /* ����� */
DO:
   {&BEG_BT_LEAVE}
   ASSIGN tt-loan-pn-par.klassmetashemy$.
   IF NOT {assigned tt-loan-pn-par.klassmetashemy$} THEN DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","������ ����� ���㬥�⮢.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   ELSE IF NOT CAN-DO(GetXclassAllChildsEx("opb-pay"),tt-loan-pn-par.klassmetashemy$) THEN DO:
      RUN Fill-SysMes IN h_tmess ("",
                                  "",
                                  "-1",
                                  "����� ���㬥�⮢ ������ ���� opb-pay ��� ��� �������ᮬ.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mAcct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mAcct TERMINAL-SIMULATION
ON ANY-PRINTABLE OF mAcct IN FRAME fMain /* ��� */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mAcct TERMINAL-SIMULATION
ON F1 OF mAcct IN FRAME fMain /* ��� */
DO:
   DEFINE VAR vOrder AS INT64 NO-UNDO.
   DEFINE VAR vSurr  AS CHAR    NO-UNDO.
   DEFINE VAR vRwd   AS ROWID   NO-UNDO.
   DEFINE BUFFER xloan-acct FOR loan-acct.
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
      pick-value = ?.
      RUN browseldvar.p ("����������",
                         "loan-acct-pn",
                         "contract"                     + CHR(1) + "cont-code",
                         tt-loan-pn-par.parent-contract + CHR(1) + tt-loan-pn-par.parent-cont-code,
                         "contract"                     + CHR(1) + "cont-code",
                         4).
      IF {assigned pick-value} THEN DO:
         mAcct = "".
         tt-loan-pn-par.pnswcet$ = INT64(pick-value) NO-ERROR.
         RUN FindSignsByVal IN h_xclass ("loan-acct", "����������", STRING(tt-loan-pn-par.pnswcet$), OUTPUT vOrder, OUTPUT vSurr).
         IF {assigned vSurr} THEN DO:
            vRwd = GetRowidBySurrogate("loan-acct",vSurr).
            FIND FIRST xloan-acct WHERE ROWID(xloan-acct) = vRwd NO-LOCK NO-ERROR.
            IF AVAIL xloan-acct THEN mAcct = xloan-acct.acct.
         END.
         DISPLAY mAcct.
      END.
   END.
   ELSE DO:
      RUN FindSignsByVal IN h_xclass ("loan-acct", "����������", STRING(tt-loan-pn-par.pnswcet$), OUTPUT vOrder, OUTPUT vSurr).
      IF {assigned vSurr} THEN DO:
         RUN formld.p ("loan-acct-pn",
                       vSurr,
                       "",
                       {&MOD_VIEW},
                       iLevel + 1).
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mAcct TERMINAL-SIMULATION
ON LEAVE OF mAcct IN FRAME fMain /* ��� */
DO:
   DEFINE VAR vOrder AS INT64 NO-UNDO.
   DEFINE VAR vSurr  AS CHAR    NO-UNDO.
   DEFINE VAR vRwd   AS ROWID   NO-UNDO.
   DEFINE BUFFER xloan-acct FOR loan-acct.
   {&BEG_BT_LEAVE}
   IF tt-loan-pn-par.pnswcet$ > 0 THEN DO:
      RUN FindSignsByVal IN h_xclass ("loan-acct", "����������", STRING(tt-loan-pn-par.pnswcet$), OUTPUT vOrder, OUTPUT vSurr).
      IF {assigned vSurr} THEN DO:
         vRwd = GetRowidBySurrogate("loan-acct",vSurr).
         FIND FIRST xloan-acct WHERE ROWID(xloan-acct) = vRwd NO-LOCK NO-ERROR.
      END.
   END.
   IF NOT AVAIL xloan-acct THEN DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","������ ���.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mOnline
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mOnline TERMINAL-SIMULATION
ON LEAVE OF mOnline IN FRAME fMain /* On-line */
DO:
   ASSIGN mOnline.
   IF NOT {assigned mOnline} OR NOT CAN-DO("��,���",mOnline) THEN DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","������ ���祭�� �� ��� ��� � ���� on-line.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mReceiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mReceiver TERMINAL-SIMULATION
ON F1 OF mReceiver IN FRAME fMain /* �����⥫� */
DO:
   DEFINE VAR vCustNm  AS CHAR    NO-UNDO.
   DEFINE VAR vCustINN AS CHAR    NO-UNDO.
   DEFINE VARIABLE mCustKpp         AS CHARACTER   NO-UNDO.
   DEFINE BUFFER xloan      FOR loan.
   DEFINE BUFFER xcust-corp FOR cust-corp.

   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
      pick-value = ?.
      RUN browseld.p ("loan-pn", 
                      "sv-21" + CHR(1) + "Title", 
                      "���" + CHR(1) + "�������� ���������� ��������",
                      "sv-21",
                      4).
      IF LASTKEY = 10 AND {assigned pick-value} AND NUM-ENTRIES(pick-value) GE 2 
      THEN DO:
         FIND FIRST xloan WHERE xloan.contract  = ENTRY(1,pick-value)
                            AND xloan.cont-code = ENTRY(2,pick-value)
         NO-LOCK NO-ERROR.
         IF AVAIL xloan THEN DO:
            IF xloan.contract  NE tt-loan-pn-par.parent-contract OR
               xloan.cont-code NE tt-loan-pn-par.parent-cont-code
            THEN DO:
               ASSIGN
                  mAcct = ""
                  tt-loan-pn-par.pnswcet$ = ?
               .
               DISPLAY mAcct.
            END.
            {loan-pn-cust.i &file=xloan &PNCustName=vCustNm &PNCustINN=vCustINN}
            ASSIGN
               mReceiver = vCustNm
               tt-loan-pn-par.parent-contract  = xloan.contract 
               tt-loan-pn-par.parent-cont-code = xloan.cont-code
            .
         END.
      END.
      DISPLAY mReceiver.
   END.
   ELSE DO:
      FIND FIRST xloan WHERE xloan.contract  = tt-loan-pn-par.parent-contract
                         AND xloan.cont-code = tt-loan-pn-par.parent-cont-code
      NO-LOCK NO-ERROR.
      IF AVAIL xloan THEN DO:
         RUN formld.p (xloan.class-code,
                       GetSurrogateBuffer("loan",(BUFFER xloan:HANDLE)),
                       "",
                       {&MOD_VIEW},
                       iLevel + 1).
      END.
   END.
   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.naznplatvvod$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.naznplatvvod$ TERMINAL-SIMULATION
ON F1 OF tt-loan-pn-par.naznplatvvod$ IN FRAME fMain /* ����.���⥦� ���� */
DO:
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION:
      pick-value = ?.
      IF SearchPfile ("funclist")
         THEN RUN funclist.p ("BaseFunc", "BaseFunc", "������� �������", iLevel + 1).
         ELSE RUN pclass.p   ("BaseFunc", "BaseFunc", "������� �������", iLevel + 1).
      IF     LAST-EVENT:FUNCTION NE "END-ERROR"
         AND pick-value NE ?
      THEN DO:
         IF SELF:SCREEN-VALUE EQ "?"
            THEN SELF:SCREEN-VALUE = "".
         pick-value ="<" + pick-value + ">".
         RUN UsSetFldVal IN THIS-PROCEDURE (SELF, pick-value, 3).
      END.
      RETURN NO-APPLY.   
   END.
   {&BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.pnispvedkvit$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnispvedkvit$ TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-loan-pn-par.pnispvedkvit$ IN FRAME fMain /* ��.���.���� */
DO:
   RUN BT_HiddenFields(SELF,STRING(tt-loan-pn-par.pnispvedkvit$,tt-loan-pn-par.pnispvedkvit$:FORMAT) ,INPUT-OUTPUT mHNext).
   ASSIGN tt-loan-pn-par.pnispvedkvit$.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.pnispwstrihkod$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnispwstrihkod$ TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-loan-pn-par.pnispwstrihkod$ IN FRAME fMain /* ��.����-��� */
DO:
   RUN BT_HiddenFields(SELF,STRING(tt-loan-pn-par.pnispwstrihkod$,tt-loan-pn-par.pnispwstrihkod$:FORMAT) ,INPUT-OUTPUT mHNext).
   ASSIGN tt-loan-pn-par.pnispwstrihkod$.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.pnotdsummami$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnotdsummami$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-pn-par.pnotdsummami$ IN FRAME fMain /* ��. �㬬��� */
DO:
   {&BEG_BT_LEAVE}
   IF tt-loan-pn-par.pnotdsummami$:SCREEN-VALUE NE STRING(tt-loan-pn-par.pnotdsummami$,"��/���")
   THEN RUN Fill-SysMes IN h_tmess ("","","0","������� ⨯ ����᫥���," + "~n" +
                                              "᫥��� �������� ���祭�� �����.").
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.pnsrok$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnsrok$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-pn-par.pnsrok$ IN FRAME fMain /* �ப */
DO:
   {&BEG_BT_LEAVE}
   ASSIGN tt-loan-pn-par.pntipsroka$ tt-loan-pn-par.pnsrok$.
   CASE tt-loan-pn-par.pntipsroka$:
      WHEN "�" THEN DO:
         IF tt-loan-pn-par.pnsrok$ = ?
         OR tt-loan-pn-par.pnsrok$ < 0
         OR tt-loan-pn-par.pnsrok$ > 99 THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","�ப ������ ���� � ���ࢠ�� 0 - 99.").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
      WHEN "�" THEN DO:
         IF tt-loan-pn-par.pnsrok$ = ?
         OR tt-loan-pn-par.pnsrok$ < 0
         OR tt-loan-pn-par.pnsrok$ > 7 THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","�ப ������ ���� � ���ࢠ�� 0 - 7.").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
      WHEN "�" THEN DO:
         IF tt-loan-pn-par.pnsrok$ = ?
         OR tt-loan-pn-par.pnsrok$ < 0
         OR tt-loan-pn-par.pnsrok$ > 31 THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","�ப ������ ���� � ���ࢠ�� 0 - 31.").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
   END CASE.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.pnvedkvit$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnvedkvit$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-pn-par.pnvedkvit$ IN FRAME fMain /* ���.���� */
DO:
   {&BEG_BT_LEAVE}
   ASSIGN tt-loan-pn-par.pnispvedkvit$ tt-loan-pn-par.pnvedkvit$.
   IF tt-loan-pn-par.pnispvedkvit$ THEN DO:
      IF NOT {assigned tt-loan-pn-par.pnvedkvit$} THEN DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","������ ��� ������⢥���� ���⠭樨.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.pnvidplatewza$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnvidplatewza$ TERMINAL-SIMULATION
ON F1 OF tt-loan-pn-par.pnvidplatewza$ IN FRAME fMain /* ��� ���⥦� */
DO:
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT} THEN DO:
      DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
         RUN browseld.p (
            "��������⥦�",
            "class"        + CHR(1) + "parent"       + CHR(1) + "title"                 + CHR(1) + "misc[3]",
            "��������⥦�" + CHR(1) + "��������⥦�" + CHR(1) + "�롥�� ��� ���⥦�"  + CHR(1) + "���",
            "class"        + CHR(1) + "parent",
            iLevel + 1
         ).
         IF LASTKEY = 10 THEN DO:
            tt-loan-pn-par.pnvidplatewza$ = pick-value.
            DISPLAY tt-loan-pn-par.pnvidplatewza$.
            tt-loan-pn-par.klassmetashemy$ = GetCodeMisc("��������⥦�",tt-loan-pn-par.pnvidplatewza$,5).
            tt-loan-pn-par.pnkassplan$ = GetCodeMisc("��������⥦�",tt-loan-pn-par.pnvidplatewza$,2).
            tt-loan-pn-par.pnotdsummami$ = (GetCodeMisc("��������⥦�",tt-loan-pn-par.pnvidplatewza$,1) = "��").
            tt-loan-pn-par.pnformablanka$ = GetCodeMisc("��������⥦�",tt-loan-pn-par.pnvidplatewza$,7).
            DISPLAY tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.pnkassplan$ tt-loan-pn-par.pnotdsummami$ tt-loan-pn-par.pnformablanka$.

            FIND FIRST code WHERE
                       code.class  EQ "��������⥦�"
                   AND code.parent EQ "��������⥦�"
                   AND code.code   EQ tt-loan-pn-par.pnvidplatewza$
               NO-LOCK NO-ERROR.

            IF AVAIL code THEN
            DO:
               ASSIGN
                  mOnline = code.kind.         
               .
               DISPLAY mOnline.
            END.
         END.
      END.
      {&END_BT_F1}
   END.
   ELSE DO:
      {&BT_F1}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnvidplatewza$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-pn-par.pnvidplatewza$ IN FRAME fMain /* ��� ���⥦� */
DO:
   DEFINE BUFFER xloan FOR loan.
   {&BEG_BT_LEAVE}
   ASSIGN tt-loan-pn-par.pnvidplatewza$.
   IF NOT {assigned tt-loan-pn-par.pnvidplatewza$} THEN DO:
      RUN Fill-SysMes IN h_tmess ("","","-1","������ ��� ���⥦�.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-pn-par.pnwstrihkod$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-pn-par.pnwstrihkod$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-pn-par.pnwstrihkod$ IN FRAME fMain /* ����-��� */
DO:
   {&BEG_BT_LEAVE}
   ASSIGN tt-loan-pn-par.pnispwstrihkod$ tt-loan-pn-par.pnwstrihkod$.
   IF tt-loan-pn-par.pnispwstrihkod$ THEN DO:
      IF NOT {assigned tt-loan-pn-par.pnwstrihkod$} THEN DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","������ ��� �������� ���媮��.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
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

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   {get-form-params.i
      mParentContract "CHARACTER"
      mParentContCode "CHARACTER"
   }

   /* Commented by KSV: ���樠������ ��⥬��� ᮮ�饭�� */
   RUN Init-SysMes("","","").

   /* Commented by KSV: ���४��㥬 ���⨪����� ������ �३�� */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.

   IF iMode = {&MOD_EDIT} THEN DO:
      _test_par:
      FOR EACH xsignstest WHERE xsignstest.file-name  = "op"
                            AND xsignstest.code       = "�������ࠬ��ࠏ��⥦�"
                            AND xsignstest.code-value = iSurrogate
      NO-LOCK,
      FIRST xsignstest2 WHERE xsignstest2.file-name  = "op"
                          AND xsignstest2.surrogate  = xsignstest.surrogate
                          AND xsignstest2.code       = "dpr-id"
      NO-LOCK,
      FIRST xsessions WHERE xsessions.dpr-id     = INT64(xsignstest2.dec-value)
                        AND xsessions.dpr-status = "�������"
      NO-LOCK:
          iMode = {&MOD_VIEW}.
          LEAVE _test_par.
      END.
   END.

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
  DISPLAY mAcct mReceiver mSdvigChar mOnline 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-loan-pn-par THEN 
    DISPLAY tt-loan-pn-par.pnvidplatewza$ tt-loan-pn-par.cont-cli 
          tt-loan-pn-par.pnotdsummami$ tt-loan-pn-par.pnformablanka$ 
          tt-loan-pn-par.pntipsroka$ tt-loan-pn-par.pnsrok$ 
          tt-loan-pn-par.pnispwstrihkod$ tt-loan-pn-par.pnwstrihkod$ 
          tt-loan-pn-par.pnnaznawcenieplatewza$ 
          tt-loan-pn-par.pnnaznplatkomissiwa$ 
          tt-loan-pn-par.pnplatneprovodwatswa$ tt-loan-pn-par.pnkassplan$ 
          tt-loan-pn-par.pnispvedkvit$ tt-loan-pn-par.pnvedkvit$ 
          tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.naznplatvvod$ 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-loan-pn-par.pnvidplatewza$ tt-loan-pn-par.cont-cli mAcct mReceiver 
         tt-loan-pn-par.pnotdsummami$ tt-loan-pn-par.pnformablanka$ 
         tt-loan-pn-par.pntipsroka$ mSdvigChar tt-loan-pn-par.pnsrok$ 
         tt-loan-pn-par.pnispwstrihkod$ tt-loan-pn-par.pnwstrihkod$ 
         tt-loan-pn-par.pnnaznawcenieplatewza$ 
         tt-loan-pn-par.pnnaznplatkomissiwa$ 
         tt-loan-pn-par.pnplatneprovodwatswa$ tt-loan-pn-par.pnkassplan$ 
         tt-loan-pn-par.pnispvedkvit$ tt-loan-pn-par.pnvedkvit$ 
         tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.naznplatvvod$ mOnline 
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
   DO WITH FRAME {&MAIN-FRAME}:
      IF tt-loan-pn-par.pnvidplatewza$         = ? THEN tt-loan-pn-par.pnvidplatewza$         = "".
      IF tt-loan-pn-par.pnformablanka$         = ? THEN tt-loan-pn-par.pnformablanka$         = "".
      IF tt-loan-pn-par.pntipsroka$            = ? THEN tt-loan-pn-par.pntipsroka$            = "".
      IF tt-loan-pn-par.pnwstrihkod$           = ? THEN tt-loan-pn-par.pnwstrihkod$           = "".
      IF tt-loan-pn-par.pnnaznawcenieplatewza$ = ? THEN tt-loan-pn-par.pnnaznawcenieplatewza$ = "".
      IF tt-loan-pn-par.pnnaznplatkomissiwa$   = ? THEN tt-loan-pn-par.pnnaznplatkomissiwa$   = "".
      IF tt-loan-pn-par.pnkassplan$            = ? THEN tt-loan-pn-par.pnkassplan$            = "".
      IF tt-loan-pn-par.pnvedkvit$             = ? THEN tt-loan-pn-par.pnvedkvit$             = "".
      IF tt-loan-pn-par.klassmetashemy$        = ? THEN tt-loan-pn-par.klassmetashemy$        = "".
      IF tt-loan-pn-par.naznplatvvod$          = ? THEN tt-loan-pn-par.naznplatvvod$          = "".
      DISPLAY tt-loan-pn-par.pnvidplatewza$ tt-loan-pn-par.pnformablanka$ tt-loan-pn-par.pntipsroka$ tt-loan-pn-par.pnnaznawcenieplatewza$ tt-loan-pn-par.pnnaznplatkomissiwa$ tt-loan-pn-par.pnkassplan$ tt-loan-pn-par.klassmetashemy$ tt-loan-pn-par.naznplatvvod$.
   END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_GO TERMINAL-SIMULATION 
PROCEDURE Local_GO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VAR vOrder      AS INT64 NO-UNDO.
   DEFINE VAR vSurr       AS CHAR    NO-UNDO.
   DEFINE BUFFER xloan-par FOR loan.

   DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN tt-loan-pn-par.pnvidplatewza$.
      RUN FindSignsByVal IN h_xclass ("loan-acct", 
                                      "����������", 
                                      STRING(tt-loan-pn-par.pnswcet$), 
                                      OUTPUT vOrder, 
                                      OUTPUT vSurr).
      IF NOT {assigned vSurr} THEN DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","����୮ ������ ���.").
         RETURN ERROR {&RET-ERROR}.
      END.
      FOR EACH xloan-par WHERE xloan-par.class-code       = "loan-pn-par"
                           AND xloan-par.parent-contract  = tt-loan-pn-par.parent-contract
                           AND xloan-par.parent-cont-code = tt-loan-pn-par.parent-cont-code
                           AND ROWID(xloan-par)          <> tt-loan-pn-par.local__rowid
      NO-LOCK:
         IF NOT INT64(GetXAttrValueEx("loan",
                                        GetSurrogateBuffer("loan",(BUFFER xloan-par:HANDLE)),
                                        "�����",
                                        ?)) = tt-loan-pn-par.pnswcet$
         THEN NEXT.
         IF NOT GetXAttrValueEx("loan",
                                GetSurrogateBuffer("loan",(BUFFER xloan-par:HANDLE)),
                                "��������⥦�",
                                ?) = tt-loan-pn-par.pnvidplatewza$
         THEN NEXT.
         IF NOT GetXAttrValueEx("loan",
                                GetSurrogateBuffer("loan",(BUFFER xloan-par:HANDLE)),
                                "�����⍥�஢������",
                                ?) = "���"
         THEN NEXT.
         RUN Fill-SysMes IN h_tmess ("",
                                     "",
                                     "-1",
                                     "��� ������� ��ࠬ��� � ����� ���⥦� '" + 
                                        GetNullStr(tt-loan-pn-par.pnvidplatewza$) + 
                                        "' � ��⮬ '" + 
                                        mAcct + "'.").
/*         RETURN ERROR {&RET-ERROR}.
*/
      END.
   END.

   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
DEFINE VAR mCustName  AS CHAR    NO-UNDO.
   DEFINE VAR mCustINN   AS CHAR    NO-UNDO.
   DEFINE VAR vHLoanAcct AS HANDLE  NO-UNDO.
   DEFINE VAR vOk        AS LOGICAL NO-UNDO.
   DEFINE VAR vOrder AS INT64 NO-UNDO.
   DEFINE VAR vSurr  AS CHAR    NO-UNDO.
   DEFINE VAR vRwd   AS ROWID   NO-UNDO.
   DEFINE BUFFER xloan-acct FOR loan-acct.
   DEFINE BUFFER xloan      FOR loan.
   DEFINE BUFFER xcust-corp FOR cust-corp.
   DEFINE VARIABLE mCustKpp         AS CHARACTER   NO-UNDO.


   DO WITH FRAME {&MAIN-FRAME}:
      CASE tt-loan-pn-par.pnsdvigsroka$:
         WHEN -1 THEN mSdvigChar = ENTRY(1,mSdvigChar:LIST-ITEMS).
         WHEN 1  THEN mSdvigChar = ENTRY(3,mSdvigChar:LIST-ITEMS).
         OTHERWISE    mSdvigChar = ENTRY(2,mSdvigChar:LIST-ITEMS).
      END CASE.

      IF iMode EQ {&MOD_ADD} THEN DO:
         /* Parent �१ ��ࠬ���� */
         RELEASE xloan.
         IF  {assigned mParentContract}
         AND mParentContract <> "*"
         AND {assigned mParentContCode}
         AND mParentContCode <> "*" THEN DO:
            FIND FIRST xloan WHERE xloan.contract  = mParentContract
                               AND xloan.cont-code = mParentContCode NO-LOCK NO-ERROR.
         END.
         /* Parent �१ 䨫��� */
         IF NOT AVAIL xloan THEN DO:
            ASSIGN
               mParentContract = GetFltVal("parent-contract")
               mParentContCode = GetFltVal("parent-cont-code")
            .
            IF  {assigned mParentContract}
            AND mParentContract <> "*"
            AND {assigned mParentContCode}
            AND mParentContCode <> "*" THEN DO:
               FIND FIRST xloan WHERE xloan.contract  = mParentContract
                                  AND xloan.cont-code = mParentContCode NO-LOCK NO-ERROR.
            END.
         END.
         IF AVAIL xloan THEN DO:
            ASSIGN
               tt-loan-pn-par.parent-contract  = xloan.contract
               tt-loan-pn-par.parent-cont-code = xloan.cont-code
            .
            tt-loan-pn-par.pntipsroka$ = GetXAttrValueEx("loan",GetSurrogateBuffer("loan",(BUFFER xloan:HANDLE)),"������ப�",tt-loan-pn-par.pntipsroka$).
            tt-loan-pn-par.pnsdvigsroka$ = INT64(GetXAttrValueEx("loan",GetSurrogateBuffer("loan",(BUFFER xloan:HANDLE)),"��������ப�",STRING(tt-loan-pn-par.pnsdvigsroka$))) NO-ERROR.
            CASE tt-loan-pn-par.pnsdvigsroka$:
               WHEN -1 THEN mSdvigChar = ENTRY(1,mSdvigChar:LIST-ITEMS).
               WHEN 1  THEN mSdvigChar = ENTRY(3,mSdvigChar:LIST-ITEMS).
               OTHERWISE    mSdvigChar = ENTRY(2,mSdvigChar:LIST-ITEMS).
            END CASE.
            tt-loan-pn-par.pnsrok$ = INT64(GetXAttrValueEx("loan",GetSurrogateBuffer("loan",(BUFFER xloan:HANDLE)),"���ப",STRING(tt-loan-pn-par.pnsrok$))) NO-ERROR.
            tt-loan-pn-par.pnispvedkvit$ = LOGICAL(GetXAttrValueEx("loan",GetSurrogateBuffer("loan",(BUFFER xloan:HANDLE)),"���ᯂ������",STRING(tt-loan-pn-par.pnispvedkvit$,GetXAttrEx(tt-loan-pn-par.class-code,"���ᯂ������","Data-Format"))),GetXAttrEx(tt-loan-pn-par.class-code,"���ᯂ������","Data-Format")) NO-ERROR.
            tt-loan-pn-par.pnvedkvit$ = GetXAttrValueEx("loan",GetSurrogateBuffer("loan",(BUFFER xloan:HANDLE)),"���������",tt-loan-pn-par.pnvedkvit$).
         END.

         {getfreenumber.i
            &file      = "loan"
            &field     = "cont-code"
            &condition = "AND class-code = tt-loan-pn-par.class-code"
            &OutVal    = "tt-loan-pn-par.cont-code"
         }
      END.

      /* ������������ �����⥫� */
      FIND FIRST xloan WHERE xloan.contract  = tt-loan-pn-par.parent-contract
                         AND xloan.cont-code = tt-loan-pn-par.parent-cont-code
      NO-LOCK NO-ERROR.
      IF AVAIL xloan THEN DO:
         {loan-pn-cust.i &file=xloan}
         mReceiver = mCustName.
      END.

      IF iMode = {&MOD_ADD} THEN 
         tt-loan-pn-par.pnvidplatewza$ = GetFltVal('sv-20').
      /* ��� */
      mAcct = "".
      IF iMode <> {&MOD_ADD} THEN DO:
         tt-loan-pn-par.pnswcet$ = INT64(GetXAttrValueEx("loan",iSurrogate,"�����",?)).
         RUN FindSignsByVal IN h_xclass ("loan-acct", 
                                         "����������", 
                                         STRING(tt-loan-pn-par.pnswcet$), 
                                         OUTPUT vOrder, 
                                         OUTPUT vSurr).
         IF {assigned vSurr} THEN DO:
            vRwd = GetRowidBySurrogate("loan-acct",vSurr).
            FIND FIRST xloan-acct WHERE ROWID(xloan-acct) = vRwd NO-LOCK NO-ERROR.
            IF AVAIL xloan-acct THEN mAcct = xloan-acct.acct.
         END.
      END.
      DISPLAY mAcct.

      /* �� on-line */
      mOnline = "".
      IF iMode NE {&MOD_ADD} THEN DO:
         mOnline  = GetXAttrValueEx("loan", 
                                    tt-loan-pn-par.contract + 
                                       "," + 
                                       tt-loan-pn-par.cont-code, 
                                    "on-line", 
                                    "").
      END.
      DISPLAY mOnline.
   END.

   RETURN.
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
   DEFINE VAR vCommis AS CHAR NO-UNDO.
   DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN mSdvigChar.
      IF   mSdvigChar = ENTRY(1,mSdvigChar:LIST-ITEMS)
      THEN tt-loan-pn-par.pnsdvigsroka$ = -1.
      ELSE IF mSdvigChar = ENTRY(3,mSdvigChar:LIST-ITEMS)
           THEN tt-loan-pn-par.pnsdvigsroka$ = 1.
           ELSE tt-loan-pn-par.pnsdvigsroka$ = 0.
      UpdateSigns("loan", 
                  tt-loan-pn-par.contract + ","  + tt-loan-pn-par.cont-code, 
                  "��������ப�",
                  STRING(tt-loan-pn-par.pnsdvigsroka),
                  IsXAttrIndexed(tt-loan-pn-par.class-code, "��������ப�")
                 ).
      UpdateSigns("loan",
                  tt-loan-pn-par.contract + ","  + tt-loan-pn-par.cont-code, 
                  "�����",
                  STRING(tt-loan-pn-par.pnswcet$),
                  IsXAttrIndexed(tt-loan-pn-par.class-code, "�����")
                 ).
      UpdateSigns("loan", 
                  tt-loan-pn-par.contract + "," + tt-loan-pn-par.cont-code, 
                  "on-line", 
                  mOnline, 
                  IsXAttrIndexed(tt-loan-pn-par.class-code, "on-line")).
      IF iMode = {&MOD_ADD} THEN DO:
         vCommis = GetXAttrValueEx("code","��������⥦�," + tt-loan-pn-par.pnvidplatewza$,"���������",?).
         IF {assigned vCommis} THEN DO:
            UpdateSigns("loan",
                        tt-loan-pn-par.contract + ","  + tt-loan-pn-par.cont-code,
                        "���������",
                        vCommis,
                        IsXAttrIndexed(tt-loan-pn-par.class-code, "���������")
                       ).
         END.
         vCommis = GetXAttrValueEx("code","��������⥦�," + tt-loan-pn-par.pnvidplatewza$,"����������",?).
         IF {assigned vCommis} THEN DO:
            UpdateSigns("loan",
                        tt-loan-pn-par.contract + ","  + tt-loan-pn-par.cont-code,
                        "����������",
                        vCommis,
                        IsXAttrIndexed(tt-loan-pn-par.class-code, "����������")
                       ).
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='24/04/2015 19:01:27.543+04:00' */
/* $LINTUSER='shoi' */
/* $LINTMODE='1' */
/* $LINTFILE='f-loan-pn-par.p' */
/*prosignE6JCHcNJitNVhwQ1q7yn8w*/