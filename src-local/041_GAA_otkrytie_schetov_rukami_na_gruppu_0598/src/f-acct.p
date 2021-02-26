&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-acct NO-UNDO LIKE acct
       FIELD akt_vzv$ AS CHARACTER /* ���_��� */
       FIELD alw#tnomer$ AS CHARACTER /* ���⍮��� */
       FIELD datasoobwtls$ AS CHARACTER /* ��⠑���鋑 */
       FIELD doverotkrswc$ AS CHARACTER /* ���������� */
       FIELD dogotkrls$ AS CHARACTER /* �������� */
       FIELD dohkomissii$ AS CHARACTER /* ��劮���ᨨ */
       FIELD internet$ AS LOGICAL /* ���୥� */
       FIELD istpostupl$ AS CHARACTER /* ��⏮��㯫 */
       FIELD korpkl$ AS CHARACTER /* ��ொ� */
       FIELD mbr$ AS LOGICAL /* ��� */
       FIELD mval$ AS DECIMAL /* ���� */
       FIELD minswcetwcik$ AS INT64 /* ������稪 */
       FIELD nezak$ AS LOGICAL /* ����� */
       FIELD nerek$ AS CHARACTER /* ����� */
       FIELD novaciwa$ AS CHARACTER /* ������ */
       FIELD nomdog$ AS CHARACTER /* ������ */
       FIELD odnins$ AS LOGICAL /* ������ */
       FIELD operposwc$ AS CHARACTER /* �������� */
       FIELD pereocotr$ AS CHARACTER /* ��८��� */
       FIELD pereocpol$ AS CHARACTER /* ��८揮� */
       FIELD porvydvypis$ AS CHARACTER /* ����뤂믨� */
       FIELD primls$ AS CHARACTER /* �ਬ�� */
       FIELD priwcinablok$ AS CHARACTER /* ��稭����� */
       FIELD priwcinazakrswc$ AS CHARACTER /* ��稭������� */
       FIELD procprivl$ AS CHARACTER /* ���ਢ� */
       FIELD rezerv$ AS CHARACTER /* ����� */
       FIELD rolw#swcpov$ AS CHARACTER /* ����珎� */
       FIELD svodnswc$ AS CHARACTER /* ������� */
       FIELD svwaz_swc$ AS CHARACTER /* ���_�� */
       FIELD sv_swc_881$ AS CHARACTER /* ��_��_881 */
       FIELD skonsalw#do$ AS LOGICAL /* �������줮 */
       FIELD sotrotkrswc$ AS CHARACTER /* ��������� */
       FIELD sotrutvswc$ AS CHARACTER /* �����⢑� */
       FIELD srok$ AS CHARACTER /* �ப */
       FIELD swcetnu$ AS CHARACTER /* ��⍓ */
       FIELD swcetpk$ AS LOGICAL /* ��⏊ */
       FIELD swc_proc$ AS CHARACTER /* ��_��� */
       FIELD haroper$ AS CHARACTER /* ������� */
       FIELD celw#kred$ AS CHARACTER /* ����। */
       FIELD Acct-DBI AS CHARACTER /* Acct-DBI */
       FIELD acct-def AS LOGICAL /* acct-def */
       FIELD acct-exept AS LOGICAL /* acct-exept */
       FIELD bank-inn AS LOGICAL /* bank-inn */
       FIELD cls-op-templ AS CHARACTER /* cls-op-templ */
       FIELD confiden AS CHARACTER /* confiden */
       FIELD DocRefL AS CHARACTER /* DocRefL */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD form-type-code AS CHARACTER /* form-type-code */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD IDCust AS CHARACTER /* IDCust */
       FIELD Kodtip_sch AS CHARACTER /* Kodtip_sch */
       FIELD kraw AS CHARACTER /* kraw */
       FIELD name-acct AS CHARACTER /* name-acct */
       FIELD RNK AS CHARACTER /* RNK */
       FIELD SourceOfReceipt AS CHARACTER /* SourceOfReceipt */
       FIELD sv_swcet_1571$ AS CHARACTER /* ��_���_1571 */
       FIELD unkg$ AS INT64 /* ���� */
       FIELD msfo-acct AS CHARACTER /* msfo-acct */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD msfo-rez AS CHARACTER /* msfo-rez */
       FIELD swcet_1571$ AS CHARACTER /* ���_1571 */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       FIELD pribubyt446$ AS CHARACTER /* �ਡ����446 */

       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-acct" "" }
       .
DEFINE TEMP-TABLE tt-deputy NO-UNDO LIKE deputy
       FIELD datanazn$ AS DATE /* ��⠍��� */
       FIELD dataotm$ AS DATE /* ��⠎� */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */

       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-deputy" "deputy" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: F-ACCT.P
      Comment: (0078824) �����௮��� ��� ࠡ��� �
               ���ᬠ��
   Parameters:
         Uses:
      Used by:
      Created: 15.10.2005 20:10 Om
     Modified: 15.10.2005 20:10 Om       
     Modified: 05.07.2007 15:25 KSV      (0078824) �����௮��� ��� ࠡ��� �
                                         ���ᬠ��
     Modified: 27.07.2007 14:53 ariz     (0078434) �訡��: �� ����⠢���� ���
                                         �� ᮧ����� ��� �� ��� ��ꥪ�.
     Modified: 20.08.2007 21:00 KSV      (0081029) ������� �ਣ��� �� GO ���
                                         �鸞 ����� �⪫�祭 ��� ���ᬠ��
     Modified: 30.10.2007 12:44 MUTA     (0082120) ��������� ������⢥����� ���
                                         業��� �㬠� � ������� ���㫥
     Modified: 08.04.2008 MUTA 0090931   ��ࠡ�⪠ �����㬥�� acct-qty.
     Modified: 29.05.2008 15:31 KSV      (0085464) ���ᬠ��. ��ࠢ����
                                         ������ join-���� �� ���������
                                         ����� CURRENCY � ACCT
     Modified: 06.10.2009 14:31 ksv      (0118166) QBIS. ������. ��ࠢ�����
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

{intrface.get separate}
{intrface.get acct}     /* ������⥪� ��� ࠡ��� � ��⠬�. */
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */
{intrface.get kau}      /* ������⥪� ��� ࠡ��� � ����⠬�. */
{intrface.get terr}
{intrface.get brnch}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get blkob}    /* ������⥪� ��� ࠡ��� � �����஢���� ��ꥪ⮢. */
{intrface.get op}
{sh-defs.i}             /* ��६���� ��� ����� ���⪠ �� ����. */
{accttok.def}           /* ������� ����⠭� �ᯮ��㥬�� � ��᪥. */

{wordwrap.def}
{fms-chkdoc.i &nofmsprint=yes}
{stoplist.fun}


DEF VAR mFldh     AS HANDLE NO-UNDO.
DEF VAR mAcctMask AS CHAR   NO-UNDO. /* ��᪠ ���. */
DEF VAR mTokAcct  AS CHAR   NO-UNDO. /* ����� ���. */
DEF VAR mTokIdx   AS CHAR   NO-UNDO.
DEF VAR mTokLen   AS CHAR   NO-UNDO.
DEF VAR mKauId    AS CHAR   NO-UNDO. /* ��� 蠡���� ���. */
DEF VAR mFlagUnk  AS LOG    NO-UNDO. /* �⮡ࠦ��� �� ��� ��� ��� ������ (cust-id) */
DEF VAR mDateAcctQty AS DATE NO-UNDO.
DEF VAR mAccessMask  AS CHAR NO-UNDO. /*��᪠ ��⮢, ��� ������ �뢮����� ����㯭� ���⮪ */
DEF VAR mAccessContAcct AS CHAR NO-UNDO.
DEF VAR vEditSet  AS CHAR   NO-UNDO. 
DEF VAR vCustId-ScV   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate446 AS DATE   NO-UNDO.   
mDate446 = DATE(FGetSetting("��446�","���446�","01/01/2016")).

DEFINE TEMP-TABLE t-obj NO-UNDO
   FIELD rec AS RECID.

DEFINE TEMP-TABLE ttBTTUsed NO-UNDO LIKE bis-temp-table.

        /* �ᥣ�� �⮡ࠦ��� ��㧥� ��. */
&GLOBAL-DEFINE XATT-ED-ON
/* ᯨ᮪ ����� ��� �஢�ન �।��⠭��������� ���祭�� � ०��� ᮧ�����
** (��� �������஢����� ����� ���) */
&GLOBAL-DEFINE INITIAL-VALID-LIST tt-acct.bal-acct
/* �஢����� �ࠢ� �� ��㯯� ४����⮢ */
&GLOBAL-DEFINE CHECK-XATTR-GROUP-PERMISSION

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-acct.branch-id tt-acct.cust-cat ~
tt-acct.cust-id tt-acct.rate-type
&Scoped-define ENABLED-TABLES tt-acct
&Scoped-define FIRST-ENABLED-TABLE tt-acct
&Scoped-Define ENABLED-OBJECTS unk$ mNrAcctCur vAcctCur mNrAcctPos vAcctPos ~
vAcctQty sh-val-close mNrShValClose sh-bal-close mNrShBalClose mNrShAvCur ~
vAvCur mNrShAvPos vAvPos mNrShValNa sh-val-na mNrShBalNa sh-bal-na mChoice ~
sh-qty-close sh-qty-na
&Scoped-Define DISPLAYED-FIELDS tt-acct.branch-id tt-acct.cust-cat ~
tt-acct.cust-id tt-acct.bal-acct tt-acct.currency tt-acct.acct ~
tt-acct.rate-type tt-acct.open-date tt-acct.close-date tt-acct.last-date ~
tt-acct.Details tt-acct.alw#tnomer$ tt-acct.contract tt-acct.contr-acct ~
tt-acct.user-id tt-acct.check-op
&Scoped-define DISPLAYED-TABLES tt-acct
&Scoped-define FIRST-DISPLAYED-TABLE tt-acct
&Scoped-Define DISPLAYED-OBJECTS vNameBranch vNameClient unk$ vSide ~
vNameUser vNameBlock vNameControl mKau-id vNameKau separator1 separator2 ~
separator3 separator4 vDateAcctPos mNrAcctCur vAcctCur mNrAcctPos vAcctPos ~
vDateDocAk vAcctQty sh-val-close mNrShValClose sh-bal-close mNrShBalClose ~
mNrShAvCur vAvCur mNrShAvPos vAvPos vDateDocNAk mNrShValNa sh-val-na ~
mNrShBalNa sh-bal-na vDateDocAv mChoice sh-qty-close sh-qty-na

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id ~
unk$ tt-acct.bal-acct tt-acct.currency tt-acct.acct tt-acct.rate-type ~
tt-acct.open-date tt-acct.Details tt-acct.contract tt-acct.contr-acct ~
tt-acct.user-id tt-acct.check-op mKau-id tt-acct.pribubyt446$ 
&Scoped-define List-2 tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id ~
unk$ tt-acct.rate-type tt-acct.open-date tt-acct.close-date tt-acct.Details ~
tt-acct.contract tt-acct.contr-acct tt-acct.user-id tt-acct.check-op mKau-id ~
tt-acct.pribubyt446$ 
&Scoped-define List-3 fMain tt-acct.branch-id tt-acct.cust-cat ~
tt-acct.cust-id unk$ tt-acct.bal-acct tt-acct.currency tt-acct.acct ~
tt-acct.open-date tt-acct.last-date tt-acct.Details tt-acct.contract ~
tt-acct.contr-acct tt-acct.user-id tt-acct.check-op mKau-id tt-acct.pribubyt446$ 
&Scoped-define List-4 tt-acct.acct

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE vSide AS CHARACTER FORMAT "X":U
     LABEL "��⨢/���ᨢ"
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "�","�","-"
     DROP-DOWN-LIST
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
     &ELSE SIZE 5 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameClient AS CHARACTER
     VIEW-AS EDITOR
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 3
     &ELSE SIZE 61 BY 3 &ENDIF NO-UNDO.

DEFINE VARIABLE mChoice AS CHARACTER FORMAT "x(10)" INITIAL "� �������"
      VIEW-AS TEXT
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF.

DEFINE VARIABLE mKau-id AS CHARACTER FORMAT "x(12)" INITIAL ?
     LABEL "��� 蠡�.���"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF.

DEFINE VARIABLE mNrAcctCur AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrAcctPos AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShAvCur AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShAvPos AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShBalClose AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShBalNa AS CHARACTER FORMAT "x(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShValClose AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNrShValNa AS CHARACTER FORMAT "X(23)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
     &ELSE SIZE 3 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U
     LABEL "�������"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
     &ELSE SIZE 13 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator3 AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator4 AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-bal-close AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-bal-na AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-qty-close AS DECIMAL FORMAT "->>,>>>,>>>,>>9.9999999":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-qty-na AS DECIMAL FORMAT "->>,>>>,>>>,>>9.9999999":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-val-close AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE sh-val-na AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE unk$ AS CHARACTER FORMAT "X(256)":U
     LABEL "���"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAcctCur AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAcctPos AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAcctQty AS DECIMAL FORMAT "->>,>>>,>>>,>>9.9999999":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAvCur AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAvPos AS DECIMAL FORMAT "��zz,zzz,zzz,zzz,zz9.99":U INITIAL 0
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateAcctPos AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateDocAk AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateDocAv AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDateDocNAk AS DATE FORMAT "99/99/9999":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameBlock AS LOGICAL FORMAT "�����஢��/":U INITIAL NO
     LABEL "�����"
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameBranch AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 49 BY 1
     &ELSE SIZE 49 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameControl AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 31 BY 1
     &ELSE SIZE 31 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameKau AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 48 BY 1
     &ELSE SIZE 48 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vNameUser AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 48 BY 1
     &ELSE SIZE 48 BY 1 &ENDIF NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-acct.branch-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 16 COLON-ALIGNED
          &ELSE AT ROW 1 COL 16 COLON-ALIGNED &ENDIF HELP
          "��� ���ࠧ�������"
          LABEL "���ࠧ�������" FORMAT "x(10)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     vNameBranch
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 27 COLON-ALIGNED
          &ELSE AT ROW 1 COL 27 COLON-ALIGNED &ENDIF NO-LABEL
     tt-acct.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 16 COLON-ALIGNED
          &ELSE AT ROW 2 COL 16 COLON-ALIGNED &ENDIF FORMAT "X"
          VIEW-AS COMBO-BOX INNER-LINES 5
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-acct.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 40 COLON-ALIGNED
          &ELSE AT ROW 2 COL 40 COLON-ALIGNED &ENDIF HELP
          "���浪��� ����� ������ (������ F1 ��� �롮�)"
          LABEL "������ N" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     vNameClient
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 18
          &ELSE AT ROW 3 COL 18 &ENDIF NO-LABEL
     unk$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 40 COLON-ALIGNED
          &ELSE AT ROW 2 COL 40 COLON-ALIGNED &ENDIF
     tt-acct.bal-acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 16 COLON-ALIGNED
          &ELSE AT ROW 6 COL 16 COLON-ALIGNED &ENDIF HELP
          "����� �����ᮢ��� ���"
          LABEL "��� 2 ���浪�" FORMAT "99999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     vSide
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 40 COLON-ALIGNED
          &ELSE AT ROW 6 COL 40 COLON-ALIGNED &ENDIF HELP
          "��⨢�� (�), ���ᨢ�� (�) ��� ��⨢��-���ᨢ�� (-) ���"
     tt-acct.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 65 COLON-ALIGNED
          &ELSE AT ROW 6 COL 65 COLON-ALIGNED &ENDIF HELP
          "��� ������"
          LABEL "�����" FORMAT "xxx"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-acct.acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 16 COLON-ALIGNED
          &ELSE AT ROW 7 COL 16 COLON-ALIGNED &ENDIF HELP
          "����� ��楢��� ���"
          LABEL "���" FORMAT "x(25)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
          &ELSE SIZE 25 BY 1 &ENDIF
     tt-acct.rate-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 65 COLON-ALIGNED
          &ELSE AT ROW 7 COL 65 COLON-ALIGNED &ENDIF HELP
          "��� ⨯� ���� ��८業�� ���"
          LABEL "    ��� ����" FORMAT "x(12)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-acct.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 16 COLON-ALIGNED
          &ELSE AT ROW 8 COL 16 COLON-ALIGNED &ENDIF HELP
          "��� ������ ���"
          LABEL "�����" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-acct.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 40 COLON-ALIGNED
          &ELSE AT ROW 8 COL 40 COLON-ALIGNED &ENDIF HELP
          "��� ������� ���"
          LABEL "������" FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-acct.last-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 65 COLON-ALIGNED
          &ELSE AT ROW 8 COL 65 COLON-ALIGNED &ENDIF HELP
          "��� ��᫥����� ���������"
          LABEL "��� ���." FORMAT "99/99/9999"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-acct.Details
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 4
          &ELSE AT ROW 9 COL 4 &ENDIF HELP
          "������������ ���"
          LABEL "������������"
          VIEW-AS EDITOR
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 61 BY 2
          &ELSE SIZE 61 BY 2 &ENDIF
     tt-acct.alw#tnomer$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 16 COLON-ALIGNED
          &ELSE AT ROW 11 COL 16 COLON-ALIGNED &ENDIF HELP
          "����ୠ⨢�� �����"
          LABEL "����. �����" FORMAT "x(20)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 19 BY 1 &ENDIF
     tt-acct.contract
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 65 COLON-ALIGNED
          &ELSE AT ROW 11 COL 65 COLON-ALIGNED &ENDIF HELP
          "��� �����祭�� ���"
          LABEL "�����祭��" FORMAT "x(6)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
          &ELSE SIZE 6 BY 1 &ENDIF
     tt-acct.pribubyt446$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 65 COLON-ALIGNED
          &ELSE AT ROW 12 COL 65 COLON-ALIGNED &ENDIF HELP
          "������ ��室�/��室�"
          LABEL "������" FORMAT "x(12)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-acct.contr-acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 16 COLON-ALIGNED
          &ELSE AT ROW 12 COL 16 COLON-ALIGNED &ENDIF HELP
          "����� ��୮�� ���"
          LABEL "���� ���" FORMAT "x(25)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
          &ELSE SIZE 24 BY 1 &ENDIF
     tt-acct.user-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 16 COLON-ALIGNED
          &ELSE AT ROW 13 COL 16 COLON-ALIGNED &ENDIF HELP
          "��� �⢥��⢥����� �ᯮ���⥫�"
          LABEL "�⢥��⢥���" FORMAT "xxxx"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     vNameUser
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 29 COLON-ALIGNED
          &ELSE AT ROW 13 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     vNameBlock
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 16 COLON-ALIGNED
          &ELSE AT ROW 14 COL 16 COLON-ALIGNED &ENDIF
     tt-acct.check-op
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 37 COLON-ALIGNED
          &ELSE AT ROW 14 COL 37 COLON-ALIGNED &ENDIF HELP
          "��� ����஫� ����権 �� ������� ��楢��� ����"
          LABEL "����஫�" FORMAT "x(8)"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     vNameControl
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 46 COLON-ALIGNED
          &ELSE AT ROW 14 COL 46 COLON-ALIGNED &ENDIF NO-LABEL
     mKau-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 16 COLON-ALIGNED
          &ELSE AT ROW 15 COL 16 COLON-ALIGNED &ENDIF HELP
          "����� 蠡���� ���"
     vNameKau
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 29 COLON-ALIGNED
          &ELSE AT ROW 15 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 1
          &ELSE AT ROW 16 COL 1 &ENDIF NO-LABEL
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 12 COLON-ALIGNED
          &ELSE AT ROW 16 COL 12 COLON-ALIGNED &ENDIF
     separator3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 37 COLON-ALIGNED
          &ELSE AT ROW 16 COL 37 COLON-ALIGNED &ENDIF NO-LABEL
     separator4
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 67 COLON-ALIGNED
          &ELSE AT ROW 16 COL 67 COLON-ALIGNED &ENDIF NO-LABEL
     vDateAcctPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 17 COLON-ALIGNED
          &ELSE AT ROW 17 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     mNrAcctCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 29 COLON-ALIGNED
          &ELSE AT ROW 17 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     vAcctCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 29 COLON-ALIGNED
          &ELSE AT ROW 17 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrAcctPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 54 COLON-ALIGNED
          &ELSE AT ROW 17 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     vAcctPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 17 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     vDateDocAk
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 17 COLON-ALIGNED
          &ELSE AT ROW 18 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     vAcctQty
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 29 COLON-ALIGNED
          &ELSE AT ROW 17 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-val-close
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 29 COLON-ALIGNED
          &ELSE AT ROW 18 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShValClose
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 29 COLON-ALIGNED
          &ELSE AT ROW 18 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-bal-close
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 18 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     mNrShBalClose
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 54 COLON-ALIGNED
          &ELSE AT ROW 18 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShAvCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 29 COLON-ALIGNED
          &ELSE AT ROW 20 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     vAvCur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 29 COLON-ALIGNED
          &ELSE AT ROW 20 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShAvPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 54 COLON-ALIGNED
          &ELSE AT ROW 20 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     vAvPos
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 20 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     vDateDocNAk
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 17 COLON-ALIGNED
          &ELSE AT ROW 19 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShValNa
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 29 COLON-ALIGNED
          &ELSE AT ROW 19 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-val-na
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 29 COLON-ALIGNED
          &ELSE AT ROW 19 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     mNrShBalNa
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 54 COLON-ALIGNED
          &ELSE AT ROW 19 COL 54 COLON-ALIGNED &ENDIF NO-LABEL
     sh-bal-na
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 78 RIGHT-ALIGNED
          &ELSE AT ROW 19 COL 78 RIGHT-ALIGNED &ENDIF NO-LABEL
     vDateDocAv
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 17 COLON-ALIGNED
          &ELSE AT ROW 20 COL 17 COLON-ALIGNED &ENDIF NO-LABEL
     mChoice
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 26 COLON-ALIGNED
          &ELSE AT ROW 16 COL 26 COLON-ALIGNED &ENDIF NO-LABEL
     sh-qty-close
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 29 COLON-ALIGNED
          &ELSE AT ROW 18 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     sh-qty-na
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 29 COLON-ALIGNED
          &ELSE AT ROW 19 COL 29 COLON-ALIGNED &ENDIF NO-LABEL
     "������:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 10
          &ELSE AT ROW 3 COL 10 &ENDIF
     " �� ����. ����:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 1
          &ELSE AT ROW 17 COL 1 &ENDIF
     "� ���. �����" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 55
          &ELSE AT ROW 16 COL 55 &ENDIF
     "+ �����.  ���.: �" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 1
          &ELSE AT ROW 19 COL 1 &ENDIF
     "+ ��楯�. ���.: �" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
          &ELSE SIZE 17 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 1
          &ELSE AT ROW 18 COL 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     "     ����㯭�:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 1
          &ELSE AT ROW 20 COL 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 80 BY 22
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-acct T "?" NO-UNDO bisquit acct
      ADDITIONAL-FIELDS:
          FIELD akt_vzv$ AS CHARACTER /* ���_��� */
          FIELD alw#tnomer$ AS CHARACTER /* ���⍮��� */
          FIELD datasoobwtls$ AS CHARACTER /* ��⠑���鋑 */
          FIELD doverotkrswc$ AS CHARACTER /* ���������� */
          FIELD dogotkrls$ AS CHARACTER /* �������� */
          FIELD dohkomissii$ AS CHARACTER /* ��劮���ᨨ */
          FIELD internet$ AS LOGICAL /* ���୥� */
          FIELD istpostupl$ AS CHARACTER /* ��⏮��㯫 */
          FIELD korpkl$ AS CHARACTER /* ��ொ� */
          FIELD mbr$ AS LOGICAL /* ��� */
          FIELD mval$ AS DECIMAL /* ���� */
          FIELD minswcetwcik$ AS INT64 /* ������稪 */
          FIELD nezak$ AS LOGICAL /* ����� */
          FIELD nerek$ AS CHARACTER /* ����� */
          FIELD novaciwa$ AS CHARACTER /* ������ */
          FIELD nomdog$ AS CHARACTER /* ������ */
          FIELD odnins$ AS LOGICAL /* ������ */
          FIELD operposwc$ AS CHARACTER /* �������� */
          FIELD pereocotr$ AS CHARACTER /* ��८��� */
          FIELD pereocpol$ AS CHARACTER /* ��८揮� */
          FIELD porvydvypis$ AS CHARACTER /* ����뤂믨� */
          FIELD primls$ AS CHARACTER /* �ਬ�� */
          FIELD priwcinablok$ AS CHARACTER /* ��稭����� */
          FIELD priwcinazakrswc$ AS CHARACTER /* ��稭������� */
          FIELD procprivl$ AS CHARACTER /* ���ਢ� */
          FIELD rezerv$ AS CHARACTER /* ����� */
          FIELD rolw#swcpov$ AS CHARACTER /* ����珎� */
          FIELD svodnswc$ AS CHARACTER /* ������� */
          FIELD svwaz_swc$ AS CHARACTER /* ���_�� */
          FIELD sv_swc_881$ AS CHARACTER /* ��_��_881 */
          FIELD skonsalw#do$ AS LOGICAL /* �������줮 */
          FIELD sotrotkrswc$ AS CHARACTER /* ��������� */
          FIELD sotrutvswc$ AS CHARACTER /* �����⢑� */
          FIELD srok$ AS CHARACTER /* �ப */
          FIELD swcetnu$ AS CHARACTER /* ��⍓ */
          FIELD swcetpk$ AS LOGICAL /* ��⏊ */
          FIELD swc_proc$ AS CHARACTER /* ��_��� */
          FIELD haroper$ AS CHARACTER /* ������� */
          FIELD celw#kred$ AS CHARACTER /* ����। */
          FIELD Acct-DBI AS CHARACTER /* Acct-DBI */
          FIELD acct-def AS LOGICAL /* acct-def */
          FIELD acct-exept AS LOGICAL /* acct-exept */
          FIELD bank-inn AS LOGICAL /* bank-inn */
          FIELD cls-op-templ AS CHARACTER /* cls-op-templ */
          FIELD confiden AS CHARACTER /* confiden */
          FIELD DocRefL AS CHARACTER /* DocRefL */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD form-type-code AS CHARACTER /* form-type-code */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD IDCust AS CHARACTER /* IDCust */
          FIELD Kodtip_sch AS CHARACTER /* Kodtip_sch */
          FIELD kraw AS CHARACTER /* kraw */
          FIELD name-acct AS CHARACTER /* name-acct */
          FIELD RNK AS CHARACTER /* RNK */
          FIELD SourceOfReceipt AS CHARACTER /* SourceOfReceipt */
          FIELD sv_swcet_1571$ AS CHARACTER /* ��_���_1571 */
          FIELD unkg$ AS INT64 /* ���� */
          FIELD msfo-acct AS CHARACTER /* msfo-acct */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD msfo-rez AS CHARACTER /* msfo-rez */
          FIELD swcet_1571$ AS CHARACTER /* ���_1571 */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          FIELD pribubyt446$ AS CHARACTER /* �ਡ����446 */

          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-acct" "" }

      END-FIELDS.
      TABLE: tt-deputy T "?" NO-UNDO bisquit deputy
      ADDITIONAL-FIELDS:
          FIELD datanazn$ AS DATE /* ��⠍��� */
          FIELD dataotm$ AS DATE /* ��⠎� */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */

          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-deputy" "deputy" }

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
         HEIGHT             = 23.07
         WIDTH              = 79.86
         MAX-HEIGHT         = 23.07
         MAX-WIDTH          = 79.86
         VIRTUAL-HEIGHT     = 23.07
         VIRTUAL-WIDTH      = 79.86
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
   FRAME-NAME 3 Custom                                                  */
/* SETTINGS FOR FILL-IN tt-acct.acct IN FRAME fMain
   NO-ENABLE 1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.alw#tnomer$ IN FRAME fMain
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN tt-acct.bal-acct IN FRAME fMain
   NO-ENABLE 1 3 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FILL-IN tt-acct.branch-id IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-acct.check-op IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.close-date IN FRAME fMain
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN tt-acct.contr-acct IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.contract IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
ASSIGN
       tt-acct.contract:SELECTABLE IN FRAME fMain       = TRUE.

/* SETTINGS FOR FILL-IN tt-acct.pribubyt446$ IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.currency IN FRAME fMain
   NO-ENABLE 1 3 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR COMBO-BOX tt-acct.cust-cat IN FRAME fMain
   1 2 3 EXP-FORMAT                                                     */
/* SETTINGS FOR FILL-IN tt-acct.cust-id IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR EDITOR tt-acct.Details IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.last-date IN FRAME fMain
   NO-ENABLE 3 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN mKau-id IN FRAME fMain
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN tt-acct.open-date IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN tt-acct.rate-type IN FRAME fMain
   1 2 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN separator3 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN separator4 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sh-bal-close IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN sh-bal-na IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN unk$ IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-acct.user-id IN FRAME fMain
   NO-ENABLE 1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                        */
/* SETTINGS FOR FILL-IN vAcctPos IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN vAvPos IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN vDateAcctPos IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vDateDocAk IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vDateDocAv IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vDateDocNAk IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameBlock IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameBranch IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR vNameClient IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN
       vNameClient:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN vNameControl IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameKau IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vNameUser IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX vSide IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain TERMINAL-SIMULATION
ON ENTRY OF FRAME fMain
DO:
   DEFINE VAR vCustNameLine AS CHARACTER NO-UNDO.
   IF iMode EQ {&MOD_EDIT} THEN
   DO:
      RUN chk-blk.p (tt-acct.cust-cat:SCREEN-VALUE,tt-acct.cust-id:SCREEN-VALUE).
      IF RETURN-VALUE EQ "0" THEN DO:
         {getcustline.i &cust-cat = "tt-acct.cust-cat:SCREEN-VALUE" &cust-id = "tt-acct.cust-id:SCREEN-VALUE" &output-to = "vCustNameLine"}
         RUN Fill-SysMes IN h_tmess ("", "acct43", "", "%s=" + vCustNameLine + "%s=" + STRING(tt-acct.number,GetAcctFmt(tt-acct.acct-cat))).
         APPLY "ESC".
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain TERMINAL-SIMULATION
ON F3 OF FRAME fMain ANYWHERE
DO:
IF iMode NE {&Mod_ADD} AND AvailXattr("acct",tt-acct.acct + "," + tt-acct.currency,"sec-code") THEN  DO:
   IF mChoice:SCREEN-VALUE EQ "���⮪ ��"
   THEN DO:
      ASSIGN
         vAcctQty:VISIBLE     = NO
         sh-qty-close:VISIBLE = NO
         sh-qty-na:VISIBLE    = NO
         vAcctCur:VISIBLE     = tt-acct.currency  NE ""
         sh-val-close:VISIBLE = tt-acct.currency  NE ""
         sh-val-na:VISIBLE    = tt-acct.currency  NE ""
         vAvCur:VISIBLE       =  tt-acct.currency  NE "" AND {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
                                                         AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
         vAcctCur:SCREEN-VALUE     = string(vAcctCur)
         sh-val-close:SCREEN-VALUE = string(sh-val-close)
         sh-val-na:SCREEN-VALUE    = string(sh-val-na)
         vAvCur:SCREEN-VALUE       = STRING(vAvCur)
         mChoice:SCREEN-VALUE      = "� �������"
      NO-ERROR.
                       /* �᫨ ����⮪ �� ������� �� ࠢ�� ��� �
                        ** �������� �� �뫮, � ��ᨬ ����. */
      IF       vAcctPos       EQ 0
         AND   vDateAcctPos   EQ ?
      THEN ASSIGN
         vAcctCur:VISIBLE  = NO
      .
                     /* �᫨ ��楯⮢���� ���⮪ ࠢ�� ��� �
                     ** � ���ࢠ�� �� ��᫥����� �����⮣� �� ��
                     ** ⥪�饩 ���� ��� �������� � ����ᮬ ���
                     ** ��� ࠢ���� "���",
                     ** � ��ᨬ ���� ��楯⮢����� ���⪮�. */
      IF       sh-bal-close   EQ 0
         AND   NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, YES)
      THEN DO:
         ASSIGN
            sh-val-close:VISIBLE =  NO
         .
                     /* �᫨ ���楯⮢���� ���⮪ ࠢ�� ��� �
                     ** � ���ࢠ�� �� ��᫥����� �����⮣� �� ��
                     ** ⥪�饩 ���� ��� �������� � ����ᮬ ���� "���",
                     ** � ��ᨬ ���� ���楯⮢����� ���⪮�. */
         IF  NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, NO) THEN DO:

            IF sh-bal-na   EQ 0
            THEN ASSIGN
               sh-val-na:VISIBLE =  NO
            .
            IF vAvPos EQ 0
            THEN ASSIGN
               vAvCur:VISIBLE =  NO
            .

         END.
      END.
   END.
   ELSE DO:
      ASSIGN
           vAcctCur:VISIBLE     = NO
           sh-val-close:VISIBLE = NO
           sh-val-na:VISIBLE    = NO
           vAvCur:VISIBLE       = NO
           vAcctQty:VISIBLE     = YES
           sh-qty-close:VISIBLE = YES
           sh-qty-na:VISIBLE    = YES
           vAcctQty:SCREEN-VALUE = string(vAcctQty)
           sh-qty-close:SCREEN-VALUE = string(sh-qty-close)
           sh-qty-na:SCREEN-VALUE = string(sh-qty-na)
           mChoice:SCREEN-VALUE   = "���⮪ ��"
        NO-ERROR.

      IF       vAcctQty      EQ 0
         AND   mDateAcctQty   EQ ?
      THEN ASSIGN
         vAcctQty:VISIBLE  = NO
      .

      IF       sh-qty-close   EQ 0
         AND   NOT GetFirtsMoveDate (tt-acct.acct, mDateAcctQty, gEnd-date, YES)
      THEN DO:
         ASSIGN
            sh-qty-close:VISIBLE =  NO
         .

         IF       sh-qty-na   EQ 0
            AND   NOT GetFirtsMoveDate (tt-acct.acct, mDateAcctQty, gEnd-date, NO)
         THEN ASSIGN
            sh-qty-na:VISIBLE =  NO
         .
      END.

   END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   DEF VAR vTok      AS CHAR  NO-UNDO. /* ������� ��᪨. */
   DEF VAR vCnt      AS INT64       NO-UNDO. /* ���稪. */
   DEF VAR vOFfset   AS INT64       NO-UNDO. /* ����饥 ��������� �����. */

   IF LAST-EVENT:FUNCTION EQ "?"
      THEN RETURN.

   vOFfset  = tt-acct.acct:CURSOR-OFFSET.
   DO vCnt = 1 TO tt-acct.acct:CURSOR-OFFSET:
      IF       SUBSTR (tt-acct.acct:FORMAT, vCnt, 1) NE "9"
         AND   SUBSTR (tt-acct.acct:FORMAT, vCnt, 1) NE "x"
      THEN vOFfset = vOFfset - 1.
   END.
   vTok = SUBSTR (mTokAcct, vOFfset, 1).
   IF LOOKUP (vTok, "�,z,�") EQ 0
      THEN RETURN NO-APPLY.
   tt-acct.contr-acct:SCREEN-VALUE ="".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON backspace OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON CLEAR OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON delete OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON F1 OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   RETURN NO-APPLY.
END.

&Scoped-define SELF-NAME tt-acct.acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON F5 OF tt-acct.acct IN FRAME fMain /* ���� */
DO:
   RETURN NO-APPLY.
END.

&Scoped-define SELF-NAME tt-acct.contr-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.contr-acct TERMINAL-SIMULATION
ON F1 OF tt-acct.contr-acct IN FRAME fMain /* ���� ��� */
DO:
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN DO TRANSACTION:
      RUN acct-ctr.p (
         tt-acct.bal-acct,
         tt-acct.currency,
         iLevel + 1
      ).
      IF       LAST-EVENT:FUNCTION  NE "END-ERROR"
         AND   pick-value           NE ?
         THEN SELF:SCREEN-VALUE = ENTRY (1, pick-value).
   END.
   ELSE IF iMode EQ {&MOD_VIEW}
   THEN RUN formld.p (
      GetClassObject ("acct", tt-acct.contr-acct + "," + tt-acct.currency),
      tt-acct.contr-acct + "," + tt-acct.currency,
      "",
      {&MOD_VIEW},
      iLevel
   ).
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON LEAVE OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   {&BEG_BT_LEAVE}
   APPLY "RETURN" TO SELF.
   IF RETURN-VALUE EQ {&RET-ERROR}
      THEN RETURN NO-APPLY {&RET-ERROR}.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.acct TERMINAL-SIMULATION
ON RETURN OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   DEF VAR vOldNumber   AS CHAR   NO-UNDO. /* ����� ��� �� ���������. */
   DEF VAR vNumber      AS CHAR   NO-UNDO. /* ����� ��� ��᫥ ���������. */
   DEF VAR vOk          AS LOG    NO-UNDO. /* ������� ࠡ��� � १�ࢮ� ��⮢. */
   DEF VAR vTok         AS CHAR   NO-UNDO.
   DEF VAR vCnt         AS INT64  NO-UNDO. 
   DEF VAR i            AS INT64  NO-UNDO. 

   IF iMode NE {&MOD_ADD}
   THEN DO:
      APPLY "RETURN" TO FRAME {&MAIN-FRAME}.
      RETURN NO-APPLY.
   END.

   CHCK_BLCK:
   DO
   ON ERROR    UNDO CHCK_BLCK, LEAVE CHCK_BLCK
   ON ENDKEY   UNDO CHCK_BLCK, LEAVE CHCK_BLCK:
      ASSIGN
                        /* ����뢠�� 䫠� �訡��. */
         vOk         =  NO
                        /* ���࠭塞 ���� ����� ���. */
         vOldNumber  =  tt-acct.acct
      .
      /* 0185464 ��� QBIS �஢��塞 �� ᮮ⢥��⢨� ��᪥, �.�. ANY-PRINTABLE �� �ࠡ��뢠�� */
      &IF DEFINED(MANUAL-REMOTE) &THEN
         ASSIGN
            vNumber = tt-acct.acct:SCREEN-VALUE
            vCnt = 1
         .
         DO i = 1 TO LENGTH(vNumber):
            IF SUBSTRING(tt-acct.acct:FORMAT,i,1) NE "9" AND SUBSTRING(tt-acct.acct:FORMAT,i,1) NE "x"
               THEN NEXT.
            vTok = SUBSTR(mTokAcct,vCnt,1).
            IF LOOKUP (vTok, "�,z,�") EQ 0
               THEN OVERLAY(vNumber,i,1) = SUBSTRING(vOldNumber,vCnt,1). 
            vCnt = vCnt + 1.
            IF vCnt > LENGTH(mTokAcct) OR vCnt > LENGTH(vOldNumber) 
               THEN LEAVE.
         END.
         tt-acct.acct:SCREEN-VALUE = vNumber.
      &ENDIF

      ASSIGN tt-acct.acct.
                        /* ������ ����. */
      RUN RecalcKey IN h_acct (
         tt-acct.class-code,
         tt-acct.bal-acct,
         tt-acct.acct,
         OUTPUT vNumber
      ) NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN LEAVE CHCK_BLCK.
                        /* �᫨ ��� ���������, �... */
      IF AddFilToAcct(vNumber,shFilial) NE AddFilToAcct(vOldNumber, shFilial)
      THEN DO:
                        /* �஢�ઠ ������ ��� � ��. */
         IF GetValueByQuery ("acct", "acct", "acct.acct  EQ '" + AddFilToAcct(vNumber,shFilial) + "' NO-LOCK") NE ?
         THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "acct03", "", "%s=" + vNumber).
            UNDO CHCK_BLCK, LEAVE CHCK_BLCK.
         END.
                        /* �஢��塞 ����稥 ������ ��� � �����䨪���.
                        ** �᫨ ⠪�� ����, � �⬥�塞 ����⢨�. */
         IF GetCode("��⠐���ࢠ", AddFilToAcct(vNumber,shFilial)) NE ?
         THEN DO:
            RUN Fill-SysMes IN h_tmess (
               "", "", "0",
               "��� ~"" + vNumber + "~" ��१�ࢨ஢�� � �����䨪��� ~"��⠐���ࢠ~"."
            ).
            UNDO CHCK_BLCK, LEAVE CHCK_BLCK.
         END.
         ELSE DO:
                        /* ����� ���� �஡���� �����६����� �����஢��,
                        ** �� ����⭮��� ����,
                        ** ���⮬� �訡�� �� ��ࠡ��뢠��. */
            RUN AcctKeep IN h_acct (AddFilToAcct(vNumber, shFilial),    OUTPUT vOk).
            RUN AcctFree IN h_acct (AddFilToAcct(vOldNumber, shFilial), OUTPUT vOk).
            vOldNumber = AddFilToAcct(vNumber, shFilial).
         END.
      END.
                        /* �஢�ન ��諨 �ᯥ譮. */
      vOk   =  YES.
   END.
                        /* �஢�ઠ 䫠�� �訡��. */
   IF vOk
   THEN ASSIGN
      tt-acct.acct               = vNumber
      tt-acct.acct:SCREEN-VALUE  = tt-acct.acct
      tt-acct.number             = tt-acct.acct
   .
                        /* � ��砥 �訡�� ������ ���祭��. */
   ELSE DO:
      ASSIGN
         tt-acct.number             = vOldNumber
         tt-acct.acct               = vOldNumber
         tt-acct.acct:SCREEN-VALUE  = STRING (REPLACE (vOldNumber," ","0"), tt-acct.acct:FORMAT)
      .
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.open-date TERMINAL-SIMULATION
ON LEAVE OF tt-acct.open-date IN FRAME fMain 
DO:
   {&BEG_BT_LEAVE}
   IF YEAR(DATE(tt-acct.open-date:screen-value)) < 0  OR
      MONTH(DATE(tt-acct.open-date:screen-value)) < 0  OR
      DAY (DATE(tt-acct.open-date:screen-value)) < 0  
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1","����୮� ���祭�� ४����� '�����'!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-acct.bal-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.bal-acct TERMINAL-SIMULATION
ON LEAVE OF tt-acct.bal-acct IN FRAME fMain /* ��� 2 ���浪� */
DO:
   DEF VAR mValid  AS INT64    NO-UNDO. /* ���� �訡��. */
   DEF VAR mIntTmp AS INT64  NO-UNDO.
   
   /*ccc*/
   IF  iMode EQ {&MOD_ADD}
   AND LAST-EVENT:FUNCTION  NE "END-ERROR" 
   AND INT64(tt-acct.bal-acct:SCREEN-VALUE) EQ 91203
   AND (USERID("bisquit") EQ "O0400MGK" 
     OR USERID("bisquit") EQ "K0400MAA"
     OR USERID("bisquit") EQ "K0400SMV"
     OR USERID("bisquit") EQ "I0400STS"
     /*OR USERID("bisquit") EQ "RKO_MVY"*/)
   THEN
   DO:
      RUN Fill-SysMes ("", "", "-1","� ��� ��� �ࠢ ��� ������ ��⮢ ~n�⮣� ��� ��ண� ���浪�.").
      RETURN NO-APPLY.
   END.
   /*ccc*/
   
   {&BEG_BT_LEAVE}
   
   IF NOT imode EQ {&MOD_ADD}
      THEN RETURN.

   mIntTmp = INT64(tt-acct.bal-acct:SCREEN-VALUE) NO-ERROR.

   RUN Check-Acct-Bal-Acct IN h_acct (
      mIntTmp,
      tt-acct.acct-cat,
      tt-acct.cust-cat:SCREEN-VALUE,
      tt-acct.acct:SCREEN-VALUE,
      OUTPUT mValid
   ).
   IF mValid NE 0
      THEN RETURN NO-APPLY.
                        /* ��⠭���� ᢮��� �� �易��� ����. */
   RUN ChkFldPropContr-Bal-Acct.
                        /* ��⠭���� ᢮��� ���� "���� ���". */
   RUN ChkFldPropContr-Acct.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.branch-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.branch-id TERMINAL-SIMULATION
ON LEAVE OF tt-acct.branch-id IN FRAME fMain /* ���ࠧ������� */
DO:
   {&BEG_BT_LEAVE}
   IF    imode EQ {&MOD_EDIT}
      OR imode EQ {&MOD_ADD}
   THEN DO:
                  /* ����� �� ���짮��⥫� ᮧ������ ��� � ������ ���ࠧ�������. */
      RUN CheckBranch IN h_brnch (SELF:SCREEN-VALUE).
      IF RETURN-VALUE NE ""
         THEN RETURN NO-APPLY.

      ASSIGN
         vNameBranch = GetValueAttr ("branch", SELF:SCREEN-VALUE, "name")
         vNameBranch:SCREEN-VALUE = vNameBranch
      .
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.check-op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.check-op TERMINAL-SIMULATION
ON LEAVE OF tt-acct.check-op IN FRAME fMain /* ����஫� */
DO:
   DEF VAR mValid AS INT64    NO-UNDO. /* ���� �訡��. */

   {&BEG_BT_LEAVE}
   IF    imode EQ {&MOD_EDIT}
      OR imode EQ {&MOD_ADD}
   THEN DO:
      RUN Acct-Check-Side IN h_acct (
         tt-acct.check-op:SCREEN-VALUE,
         vSide:SCREEN-VALUE,
         OUTPUT mValid
      ).
      IF mValid NE 0
         THEN RETURN NO-APPLY.
      ASSIGN
         vNameControl = GetValueAttr ("code", "check-op," + SELF:SCREEN-VALUE, "name")
         vNameControl:SCREEN-VALUE = vNameControl
      .
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.close-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.close-date TERMINAL-SIMULATION
ON ENTRY OF tt-acct.close-date IN FRAME fMain /* ������ */
DO:
   IF iMode = {&MOD_EDIT} THEN.
 ELSE
 DO:
    {&BT_ENTRY}
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.close-date TERMINAL-SIMULATION
ON LEAVE OF tt-acct.close-date IN FRAME fMain /* ������ */
DO:
  IF iMode = {&MOD_EDIT} THEN.
  ELSE
  DO:
     {&BT_LEAVE}
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.contract
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.contract TERMINAL-SIMULATION
ON F1 OF tt-acct.contract IN FRAME fMain /* �����祭�� */
DO:
   IF iMode EQ {&MOD_VIEW}
   THEN DO:
      RUN RunClassMethod (
         "contract",
         "look",
         "",
         "",
         ?,
         tt-acct.acct      + "," +
         tt-acct.currency  + "," +
         SELF:SCREEN-VALUE + "," +
         STRING (iLevel + 1)
      ).
      RETURN NO-APPLY.
   END.
   mCall_F1_IN_Frame = YES.
   APPLY "F1" TO FRAME fMain.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.currency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.currency TERMINAL-SIMULATION
ON LEAVE OF tt-acct.currency IN FRAME fMain /* ����� */
DO:
   DEF VAR mForeign  AS CHAR   NO-UNDO. /* ⨯ ������ ��� 2-�� ���浪� */

   /* ����祭�� ४����� foreign-curr (������) ��� 2-�� ���浪� */
   mForeign = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "foreign-curr").
   {&BEG_BT_LEAVE}
   /* �஢�ઠ ������ ��楢��� ��� �� ���� 2-�� ���浪� */
   IF     (mForeign                       EQ "NO"
      AND  tt-acct.currency:SCREEN-VALUE  NE "")
      OR  (mForeign                       EQ "YES"
      AND  tt-acct.currency:SCREEN-VALUE  EQ "")
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "acct37", "","").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.currency TERMINAL-SIMULATION
ON RETURN OF tt-acct.currency IN FRAME fMain /* ����� */
DO:
   DEF VAR vValid       AS INT64    NO-UNDO. /* ���� �訡��. */
   DEF VAR vProcName    AS CHAR   NO-UNDO. /* ��� ��楤��� ��⮤�. */
   DEF VAR vOk          AS LOG    NO-UNDO. /* ���� ��࠭���� ����� ��� � ����-�. */
   DEF VAR vKodDoxRash  AS CHAR   NO-UNDO. /* ��� ��室�� � ��室��. */
   DEF VAR vMakeAcct    AS LOG    NO-UNDO. /* ���� �ᯥ譮�� �믮������ ����樨. */

   DEFINE BUFFER acct      FOR acct.
   DEFINE BUFFER xbal-acct FOR bal-acct.

   IF iMode NE {&MOD_ADD}
   THEN DO:
      APPLY "RETURN" TO FRAME {&MAIN-FRAME}.
      RETURN NO-APPLY.
   END.
                        /* �஢�ઠ �ࠢ��쭮�� ��������� ������.
                        ** �஢�ઠ ������ ��� �� ���� 2-�� ���浪�. */
   APPLY "LEAVE" TO tt-acct.currency IN FRAME {&MAIN-FRAME}.
   IF RETURN-VALUE EQ {&RET-ERROR} THEN RETURN NO-APPLY.

   RUN Check-Acct-Currency IN h_acct (
      tt-acct.currency:SCREEN-VALUE,
      tt-acct.acct-cat,
      OUTPUT vValid
   ).
   IF vValid NE 0
      THEN RETURN NO-APPLY.

   vProcName = GET-CLASS-METHOD (tt-acct.class-code, "Create").
   IF vProcName NE ?
   THEN DO:
      IF SearchPfile (vProcName)
         THEN RUN VALUE (vProcName + ".p") (
            tt-acct.bal-acct:SCREEN-VALUE,
            tt-acct.currency:SCREEN-VALUE
         ).
         ELSE DO:
            RUN Fill-SysMes (
               "", "", "",
               "�� ������� ��楤�� ᮧ����� " + vProcName + ".p."
            ).
            RETURN NO-APPLY.
         END.
   END.
   ELSE
   TR:
   DO
   TRANSACTION
   ON ERROR UNDO TR, LEAVE TR
   ON STOP  UNDO TR, LEAVE TR:
                        /* �᫨ ����୮ ᮧ���� ���, � ����室��� 㤠����
                        ** ���� ��� �� �����䨪��� "��⠐���ࢠ". */
      RUN AcctFree IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
      ASSIGN
         tt-acct.bal-acct
         tt-acct.currency
         tt-acct.cust-cat
         tt-acct.cust-id
         tt-acct.open-date
         tt-acct.details
         tt-acct.contract
         tt-acct.branch-id
      .
                        /* ����祭�� ��᪨ ���. */
      RUN FindAcctMask IN h_acct (
         tt-acct.class-code,
         tt-acct.bal-acct,
         INPUT-OUTPUT mAcctMask,
         INPUT-OUTPUT vKodDoxRash
      ) NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN DO:
         
         RUN Fill-SysMes IN h_tmess ("","","-1",RETURN-VALUE).
         RETURN NO-APPLY.
      END.
                        /* �᫨ ��᪠ ��� ��������� ��⮨� �� ᨬ����� "�",
                        ** � �� ����᪠�� ��楤��� �����樨 ����� ���. */
      IF mAcctMask EQ FILL ({&TOK_ANY}, LENGTH (mAcctMask))
                        /* ������塞 �஡����� ��� ���쭥�襣� �⮡ࠦ����. */
         THEN tt-acct.acct = FILL (" ", LENGTH (mAcctMask)).
      ELSE DO:
                        /* �⪫�砥� ᮧ����� ���ਨ �� ⠡��� "acct". */
         IF LOOKUP ("acct", FilesHist) GT 0
            THEN ENTRY (LOOKUP ("acct", FilesHist), FilesHist) =  "acct_closed".
                        /* ������� ���. */

            /* ����⠭�������� ࠭�� 㤠����� bis-temp-table */
            FOR EACH ttBTTUsed EXCLUSIVE-LOCK:
               IF NOT CAN-FIND(FIRST bis-temp-table WHERE bis-temp-table.surrogate-id = ttBTTUsed.surrogate-id) THEN DO:
                  CREATE bis-temp-table.
                  BUFFER-COPY ttBTTUsed TO bis-temp-table.
                  RELEASE bis-temp-table.
                  DELETE ttBTTUsed.
               END.
            END.
            
            /* �।㣠�뢠�� ����� bis-temp-table �㤥� 㤠��� */
         FIND FIRST bis-temp-table WHERE bis-temp-table.surrogate-id BEGINS STRING(tt-acct.bal-acct) + "," + STRING(tt-acct.currency) + "," NO-LOCK NO-ERROR.
            IF AVAIL bis-temp-table THEN DO:
               IF NOT CAN-FIND(FIRST ttBTTUsed WHERE ttBTTUsed.surrogate-id = bis-temp-table.surrogate-id) THEN DO:
                  CREATE ttBTTUsed.
                  BUFFER-COPY bis-temp-table TO ttBTTUsed.
               END.
            END.

         IF     shFilial EQ "0500"
            AND SUBSTR(GetXAttrValueEx("_user",USERID("bisquit"),"office",""),1,4) = "0598" 
            AND CAN-DO("408*",STRING(tt-acct.bal-acct))  THEN 
            mAcctMask = replace(mAcctMask,"����","0598").

         RUN MakeAcct (
            tt-acct.class-code,  /* iClass                  */
            tt-acct.bal-acct,    /* iBal                    */
            tt-acct.currency,    /* iCurr                   */
            tt-acct.cust-cat,    /* iCustCat                */
            tt-acct.cust-id,     /* iCustID                 */
            tt-acct.open-date,   /* iOpenDate               */
            OUTPUT tt-acct.acct, /* oAcct                   */
            BUFFER acct,         /* BUFFER iacct FOR acct . */
            mAcctMask,           /* iAcctMask               */
            vKodDoxRash,         /* iKodDoxRash             */
            tt-acct.details,     /* iDetails                */
            ?,                   /* iKauId                  */
            tt-acct.contract,    /* iContract               */
            USERID ('bisquit'),  /* iUserId                 */
            tt-acct.branch-id,   /* iBranchId               */
            NO                   /* iCopyBalXattr           */
         ) NO-ERROR.

         IF ERROR-STATUS:ERROR THEN DO:
            DEFINE VAR vErrCnt AS INT64 NO-UNDO.
            vErrStr = "".
            IF {assigned RETURN-VALUE} THEN vErrStr = SUBSTR(RETURN-VALUE,(IF INDEX(RETURN-VALUE,"pp-acct.p<-") > 0 THEN INDEX(RETURN-VALUE,"pp-acct.p<-") + 11 ELSE 1)) + CHR(10).
            DO vErrCnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
                IF INDEX(vErrStr, ERROR-STATUS:GET-MESSAGE(vErrCnt)) = 0 THEN
                vErrStr = vErrStr + CHR(10) + ERROR-STATUS:GET-MESSAGE(vErrCnt).
                IF vErrCnt > 8 THEN LEAVE.
            END.
            IF NOT SESSION:REMOTE THEN
            RUN Fill-SysMes("","","-1",vErrStr).
            UNDO TR, LEAVE TR.
         END.
         
         IF LENGTH(vKodDoxRash) GT 0 AND
            gend-date   GE mDate446
         THEN DO:
            FIND LAST tmp-code WHERE 
                      tmp-code.class    EQ "�ਡ����"   
               AND    tmp-code.code     EQ vKodDoxRash + "_"
               AND    tmp-code.beg-date LE gend-date
            NO-LOCK NO-ERROR.         
            IF AVAIL tmp-code THEN            
            ASSIGN
               tt-acct.pribubyt446$:SCREEN-VALUE = tmp-code.code
               tt-acct.pribubyt446$ = tmp-code.code
            .
         END.

            /* ����塞 ������� �᫨ �� 㣠���� */
            FOR EACH ttBTTUsed EXCLUSIVE-LOCK:
               IF CAN-FIND(FIRST bis-temp-table WHERE bis-temp-table.surrogate-id = ttBTTUsed.surrogate-id) THEN DO:
                  DELETE ttBTTUsed.
               END.
            END.

                                 /* �ᯥ譮��� ��ࠡ�⪨ MakeAcct. */
         vMakeAcct   =  YES.
         IF tt-acct.acct EQ ?
            THEN UNDO TR, LEAVE TR.

         FOR FIRST acct WHERE
                  acct.acct      EQ tt-acct.acct
            AND   acct.currency  EQ tt-acct.currency
         EXCLUSIVE-LOCK:
            RUN DelSigns (
               "acct",
               tt-acct.acct + "," + tt-acct.currency
            ).
            DELETE acct.
                        /* ��� ��⮢, � ������ � ��᪥ ��� ��� ᨬ���� "�".
                        ** ���࠭���� ��� � �����䨪��� १�ࢠ. */
            IF INDEX (mAcctMask, {&TOK_ANY}) EQ 0
            THEN DO:
               RUN AcctKeep IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
               IF vOk NE YES
                  THEN UNDO TR, LEAVE TR.
            END.
         END.
      END.
   END.
                        /* ����⠭�������� ������� ���ਨ �� ⠡��� "acct". */
   IF LOOKUP ("acct_closed", FilesHist) GT 0
      THEN ENTRY (LOOKUP ("acct_closed", FilesHist), FilesHist)   =  "acct".
                        /* � ��砥 �訡�� �⪠� �ਣ���. */
   IF NOT vMakeAcct THEN
      RETURN NO-APPLY.
                        /* ��⠭������� ⨯� ����. */
   IF tt-acct.acct-cat NE "d"
   THEN DO:
      FIND FIRST xbal-acct WHERE xbal-acct.bal-acct = tt-acct.bal-acct NO-LOCK NO-ERROR.
      ASSIGN
         tt-acct.rate-type:VISIBLE        =  tt-acct.currency NE ""
         tt-acct.rate-type                =  IF tt-acct.currency EQ ""
                                             THEN ""
                                             ELSE IF  AVAIL xbal-acct
                                                  AND {assigned xbal-acct.rate-type}
                                                  THEN xbal-acct.rate-type
                                                  ELSE "����"
         tt-acct.rate-type:SCREEN-VALUE   =  tt-acct.rate-type
      .
   END.
   ELSE tt-acct.rate-type:VISIBLE = NO.
                        /* ����祭�� ⮪���� �� ��᪥. */
   RUN GetAcctMask IN h_acct (
      mAcctMask,
      OUTPUT mTokAcct,
      OUTPUT mTokIdx,
      OUTPUT mTokLen
   ).
                        /* �����塞 �஡��� �� ᨬ��� 0.
                        ** � �ଠ��㥬 ���祭�� ᮣ��᭮ ��⥣�ਨ. */
   ASSIGN
      tt-acct.contr-acct:SCREEN-VALUE  = ""
      tt-acct.acct      :SCREEN-VALUE  =  STRING (
                                             REPLACE (tt-acct.acct," ","0"),
                                             tt-acct.acct:FORMAT
                                          )
   .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.cust-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.cust-cat TERMINAL-SIMULATION
ON LEAVE OF tt-acct.cust-cat IN FRAME fMain /* ��� ������ */
DO:
   DEF VAR vValid  AS INT64 NO-UNDO. /* ���� �訡��. */
   DEF VAR vError  AS LOG NO-UNDO. /* ���� �訡��. */

   {&BEG_BT_LEAVE}
                        /* �஢�ઠ ᮮ⢥��⢨� ��⥣�ਨ � ⨯� ������. */
   RUN Check-Acct-Cust-Cat IN h_acct (
      tt-acct.bal-acct:SCREEN-VALUE,
      tt-acct.cust-cat:SCREEN-VALUE,
      tt-acct.acct:SCREEN-VALUE,
      OUTPUT vValid
   ).
   IF vValid NE 0
      THEN RETURN NO-APPLY {&RET-ERROR}.
                        /* �஢�ઠ ��� �।�⮢ � ������⮢. */
   IF       iMode             EQ {&MOD_edit}
      AND   SELF:SCREEN-VALUE NE tt-acct.cust-cat
      AND   tt-acct.cust-cat  NE "�" 
   THEN DO:

      vError = TRUE.
      RUN LoanChk (tt-acct.contract,
                   tt-acct.acct,
                   tt-acct.currency,
                   SELF:SCREEN-VALUE,
                   INT64 (tt-acct.cust-id:SCREEN-VALUE),
                   INPUT-OUTPUT vError).

      IF vError THEN
         RETURN NO-APPLY {&RET-ERROR}.

   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.cust-cat TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-acct.cust-cat IN FRAME fMain /* ��� ������ */
DO:
   DEFINE VAR vClassCode AS CHAR NO-UNDO.
   vClassCode = GetCustClass(SELF:SCREEN-VALUE).
   /* ��।��塞 �����뢠�� �� ��� ��� ������� ⨯� �����⮢ ��� cust-id */
   {getflagunk.i &class-code="vClassCode" &flag-unk="mFlagUnk"}

   ASSIGN
      tt-acct.cust-id:VISIBLE = NO
      unk$           :VISIBLE = NO.

   IF SELF:SCREEN-VALUE EQ "�"
   THEN ASSIGN
      tt-acct.cust-id               = ?
      tt-acct.cust-id:SCREEN-VALUE  = ?
      vNameClient    :SCREEN-VALUE  = ""
      unk$                          = ?
      unk$           :SCREEN-VALUE  = ?
   .
   ELSE
      IF mFlagUnk THEN
         ASSIGN tt-acct.cust-id:VISIBLE = NO
                unk$           :VISIBLE = YES.
      ELSE
         ASSIGN unk$           :VISIBLE = NO
                tt-acct.cust-id:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.cust-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.cust-id TERMINAL-SIMULATION
ON LEAVE OF tt-acct.cust-id IN FRAME fMain /* ������ N */
DO:
   DEF VAR vCustId AS INT64 NO-UNDO.
   DEF VAR vError  AS LOG NO-UNDO. /* ���� �訡��. */

   {&BEG_BT_LEAVE}
                        /* ������ ��ࠡ�⪠ ᤥ���� �� ��稭� ������ � ��
                        ** ������ � �㫥�� ����஬. */
   IF SELF:SCREEN-VALUE EQ "?"
   THEN DO:
      RUN Fill-SysMes (
         "", "", "0",
         "�������� ~"������ N~" ��易⥫��."
      ).
      RETURN NO-APPLY {&RET-ERROR}.
   END.
                        /* �஢�ઠ ��� �।�⮢ � ������⮢. */
   vCustId = INT64 (SELF:SCREEN-VALUE).
   IF       iMode                         EQ {&MOD_edit}
      AND   vCustId                       NE tt-acct.cust-id
   THEN DO:
      vError = TRUE.
      RUN LoanChk (tt-acct.contract,
                   tt-acct.acct,
                   tt-acct.currency,
                   tt-acct.cust-cat:SCREEN-VALUE,
                   vCustId,
                   INPUT-OUTPUT vError).

      IF vError THEN
         RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.Details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.Details TERMINAL-SIMULATION
ON RETURN OF tt-acct.Details IN FRAME fMain
DO:
   DEF VAR vH AS HANDLE NO-UNDO. /* �����⥫� �� �।��饥 ����. */
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN APPLY "TAB" TO SELF.
   ELSE DO:
      vh = SELF:PREV-TAB-ITEM.
      APPLY "RETURN" TO vh.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mKau-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKau-id TERMINAL-SIMULATION
ON F1 OF mKau-id IN FRAME fMain /* ��� 蠡�.��� */
DO:
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN
   DO TRANSACTION:

      RUN browseld.p (
         "�������",
                        /* ���� ��� �।��⠭����. */
         "class"   + CHR(1) + "parent"  + CHR(1) + "code",
         "�������" + CHR(1) + "�������" + CHR(1) + GetXattrValueEx("bal-acct",string(tt-acct.bal-acct),"lst-kau-id","*") ,
         "",
         4
      ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
         THEN SELF:SCREEN-VALUE = pick-value.
   END.
   ELSE APPLY "F1" TO FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mKau-id TERMINAL-SIMULATION
ON LEAVE OF mKau-id IN FRAME fMain /* ��� 蠡�.��� */
DO:
   DEFINE VARIABLE mKauLst AS CHARACTER   NO-UNDO.

   IF {assigned mKau-id:SCREEN-VALUE}
     AND (imode EQ {&MOD_EDIT}
       OR imode EQ {&MOD_ADD}) THEN DO:

     mKauLst = GetXattrValueEx("bal-acct",string(tt-acct.bal-acct),"lst-kau-id","*").

     IF NOT CAN-DO(mKauLst,mKau-id:SCREEN-VALUE) THEN DO:

         RUN Fill-SysMes ("", "", "",
                          "�����⨬� 蠡���� ��� ��� ��� 2-�� ���浪� " + STRING(tt-acct.bal-acct) + " - " + mKauLst).
         RETURN NO-APPLY {&RET-ERROR}.

      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL unk$ TERMINAL-SIMULATION
ON LEAVE OF unk$ IN FRAME fMain /* ��� */
DO:
   DEF VAR d1 AS CHAR   NO-UNDO.
   DEFINE VARIABLE vCustId    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustClass AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vUnkValue  AS CHARACTER NO-UNDO.
   {&BEG_BT_LEAVE}
   IF iMode NE {&MOD_VIEW} THEN
   DO:
      IF    NOT {assigned unk$:SCREEN-VALUE}
         OR unk$:SCREEN-VALUE EQ "?"
      THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", "�������� ~"���~" ��易⥫��!").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      vCustId = LEFT-TRIM(tt-acct.cust-id:SCREEN-VALUE,"0").
      IF NOT {assigned vCustID} OR
         (LEFT-TRIM(SELF:SCREEN-VALUE,"0") EQ "" AND
         vCustID EQ LEFT-TRIM(string(vCustId-ScV),"0"))  THEN
         vCustId = "?".
      ASSIGN
        vCustClass = GetCustClass(tt-acct.cust-cat:SCREEN-VALUE)
        vUnkValue  = LEFT-TRIM(SELF:SCREEN-VALUE,"0").
      /* ���� ������ �� ��� � ���� ������ */
      FIND FIRST signs WHERE signs.file-name    EQ vCustClass
                         AND signs.code         EQ "���"
                         AND (LEFT-TRIM(signs.code-value,"0")  EQ vUnkValue
                           OR LEFT-TRIM(signs.xattr-value,"0") EQ vUnkValue)
                         AND signs.surrogate    EQ vCustId
      NO-LOCK NO-ERROR.
      /* �᫨ ������ �� ������ - �饬 ⮫쪮 �� ��� */
      IF NOT AVAILABLE signs THEN
      FIND FIRST signs WHERE signs.file-name    EQ vCustClass
                         AND signs.code         EQ "���"
                         AND (LEFT-TRIM(signs.code-value,"0")  EQ vUnkValue
                           OR LEFT-TRIM(signs.xattr-value,"0") EQ vUnkValue)
      NO-LOCK NO-ERROR.
      /* �᫨ ������ �� ������ - �訡�� */
      IF NOT AVAILABLE signs THEN
      DO:
         ASSIGN
            tt-acct.cust-id:SCREEN-VALUE = "?"
            vNameClient    :SCREEN-VALUE = "".
         RUN Fill-SysMes IN h_tmess ("", "", "0", "������ �� ������.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      /* �����뢠�� ���祭�� cust-id */
      tt-acct.cust-id:SCREEN-VALUE = signs.surrogate.
      vCustId-ScV = tt-acct.cust-id:SCREEN-VALUE.
      /* ��।��塞 ������������ ������ */
      vNameClient:SCREEN-VALUE = GetCliName(tt-acct.cust-cat:SCREEN-VALUE,
                                            tt-acct.cust-id:SCREEN-VALUE,
                                            OUTPUT d1,OUTPUT d1,OUTPUT d1,
                                            INPUT-OUTPUT d1,OUTPUT d1,OUTPUT d1).
      /* �஢�ઠ ������ */
      APPLY "LEAVE" TO tt-acct.cust-id.
      IF RETURN-VALUE EQ {&RET-ERROR} THEN RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-acct.user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.user-id TERMINAL-SIMULATION
ON F1 OF tt-acct.user-id IN FRAME fMain /* �⢥��⢥��� */
DO:
   IF    iMode EQ {&MOD_ADD}
      OR iMode EQ {&MOD_EDIT}
   THEN
   DO TRANSACTION:
      RUN browseld.p (
         "_user",
                        /* ���� ��� �।��⠭����. */
         "_userid"      + CHR(1) +
         "SetFirstFrm"  + CHR(1) +
         "oth3"         + CHR(1) +
         "sv-3",
                        /* ���᮪ ���祭�� �����. */
         (IF getThisUserXAttrValue("��ᬮ������") EQ "��"
            THEN (USERID ("bisquit") + "," + GetSlaves ())
            ELSE "*")                              + CHR(1) +
         "4"                                       + CHR(1) +
         "NO"                                      + CHR(1) +
         "NO",
         "",
         4
      ).
      IF LAST-EVENT:FUNCTION NE "END-ERROR"
         THEN SELF:SCREEN-VALUE = pick-value.
   END.
   ELSE APPLY "F1" TO FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-acct.user-id TERMINAL-SIMULATION
ON LEAVE OF tt-acct.user-id IN FRAME fMain /* �⢥��⢥��� */
DO:
   {&BEG_BT_LEAVE}
   IF    imode EQ {&MOD_EDIT}
      OR imode EQ {&MOD_ADD}
   THEN ASSIGN
      vNameUser = GetValueAttr ("_user", SELF:SCREEN-VALUE, "_user-name")
      vNameUser:SCREEN-VALUE  = vNameUser
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
   DEF VAR vOk AS LOG    NO-UNDO. /* ���� 㤠����� ����� ��� � ����-�. */
   mRetVal = IF mOnlyForm
               THEN {&RET-ERROR}
               ELSE  "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
                        /* ����塞 ��� �� �����䨪��� "��⠐���ࢠ". */
   IF iMode EQ {&MOD_ADD}
      THEN RUN AcctFree IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
   RETURN NO-APPLY.
END.

&IF DEFINED(MANUAL-REMOTE) &THEN
ON LEAVE OF tt-acct.currency IN FRAME fMain /* ����� */
DO:
   IF NOT mRunGoOfFrame AND iMode <> {&MOD_VIEW} THEN APPLY "RETURN" TO SELF.
END.

ON LEAVE OF tt-acct.acct IN FRAME fMain /* ��� */
DO:
   IF iMode <> {&MOD_VIEW} THEN APPLY "RETURN" TO SELF.
END.
&ELSE
ON GO OF tt-acct.cust-cat IN FRAME fMain /* ��� ������ */
, tt-acct.bal-acct, tt-acct.currency, tt-acct.cust-id
DO:
   RETURN NO-APPLY.
END.
&ENDIF

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

RUN StartBisTTY.

SUBSCRIBE "ACCT-FORM-SET-FOCUS-TO-ACCT" ANYWHERE RUN-PROCEDURE "SubscribeToAcctFocus".

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: ���樠������ ��⥬��� ᮮ�饭�� */
   RUN Init-SysMes("","","").
   
   /* ����祭�� ���祭�� �� ����஫쐥����� */
   vEditSet = fGetSetting("����஫쐥�����", ?, "���").     

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
   RUN enable_UI.

   /* Commented by KSV: ���㥬 ࠧ����⥫�. �������⥫� �������� ��� FILL-IN
   ** � �����䨪��஬ SEPARATOR# � ��ਡ�⮬ VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").

   /* Commented by KSV: ���뢠�� � ����, ����� ࠧ�襭� ��������
   ** � ����ᨬ��� �� ०��� ������ */
   RUN EnableDisable.
   IF vNameBlock THEN DO:
      /* ��⠭���� 梥� ��� ���� acct-status. */
      mFldH          =  GetWidgetHandle (FRAME fMain:HANDLE, "vNameBlock").
      mFldH:DCOLOR   =  GetDCOLOR ("bright-red").
   END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.

END.

/* ����⠭�������� ࠭�� 㤠����� bis-temp-table */
   IF AVAIL tt-acct THEN DO:
      {find-act.i
         &acct   = tt-acct.acct
         &curr   = tt-acct.currency
         &lockac = NO-LOCK
      }
   IF AVAIL acct THEN DO:
      FOR EACH ttBTTUsed WHERE ttBTTUsed.surrogate-id BEGINS STRING(acct.bal-acct) + "," + STRING(acct.currency) + "," EXCLUSIVE-LOCK:
         DELETE ttBTTUsed.
   END.
   END.
END.
FOR EACH ttBTTUsed EXCLUSIVE-LOCK TRANSACTION:
      IF NOT CAN-FIND(FIRST bis-temp-table WHERE bis-temp-table.surrogate-id = ttBTTUsed.surrogate-id) THEN DO:
         CREATE bis-temp-table.
         BUFFER-COPY ttBTTUsed TO bis-temp-table.
         RELEASE bis-temp-table.
         DELETE ttBTTUsed.
      END.
   END.

UNSUBSCRIBE "ACCT-FORM-SET-FOCUS-TO-ACCT".

/* Commented by KSV: ����뢠�� �㦡� ��⥬��� ᮮ�饭�� */
RUN End-SysMes.

RUN disable_ui.

RUN EndBisTTY.

/* Commented by KSV: ���㦠�� ������⥪� */
{intrface.del}

/* Commented by KSV: �����頥� ���祭�� ��뢠�饩 ��楤�� */
RETURN mRetVal.


PROCEDURE BeforePrepareInstance:
   DEFINE INPUT-OUTPUT PARAMETER pAggrInstances AS CHARACTER   NO-UNDO.
   
   DEFINE VARIABLE vExcAggCl  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vParents   AS CHARACTER NO-UNDO.
   vParents = iClass + "," + GetXclassAllParents(iClass).
   
   vExcAggCl  = FGetSetting("ExcAggCl",?,"").
   IF CAN-DO (vExcAggCl, vParents) THEN
   DO:
      ENTRY(LOOKUP("deputy",pAggrInstances),pAggrInstances) = "".
      pAggrInstances = REPLACE(pAggrInstances,",,",",").
      pAggrInstances = TRIM(pAggrInstances,",").      
   END.
   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChkFldPropContr-Acct TERMINAL-SIMULATION
PROCEDURE ChkFldPropContr-Acct :
/* ��⠭�������� ᢮��⢠ ���� � ����ᨬ��� �� ���ﭨ� ��ꥪ�. */
   DO WITH FRAME fMain:
                        /* ����� �� � ��� ���� ���� ���? */
  IF GetValueByQuery (
      "code",
      "code",
      "     code.class  EQ 'Dual-bal-acct' " +
      "AND (code.code   EQ '" + tt-acct.bal-acct:SCREEN-VALUE + "'  " +
      "  OR code.val    EQ '" + tt-acct.bal-acct:SCREEN-VALUE + "') " +
      "NO-LOCK"
   ) NE ?
   THEN tt-acct.contr-acct:SENSITIVE   = YES.
   ELSE ASSIGN
      tt-acct.contr-acct:SCREEN-VALUE  = ""  WHEN  iMode EQ {&MOD_ADD}
                                                OR iMode EQ {&MOD_EDIT}
      tt-acct.contr-acct:SENSITIVE     = NO
   .
   END.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChkFldPropContr-Bal-Acct TERMINAL-SIMULATION
PROCEDURE ChkFldPropContr-Bal-Acct :
DEF VAR mSide     AS CHAR   NO-UNDO. /* ��ࠪ���⨪� ���. */
   DEF VAR vCustCat  AS CHAR   NO-UNDO. /* ��� ������. */

   DO WITH FRAME fMain:
      mSide = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "side").
                        /* �᫨ �� ��� 2-�� ���浪� �⪮ ������ �ࠪ���⨪� ���,
                        ** � � ०��� ᮧ����� ��᫥�㥬 ��� �ࠪ���⨪� ��� �/�.
                        ** ��� ०��� ।���஢���� �஢��塞 ᮮ⢥�ᢨ�
                        ** �ࠪ���⨪ �/� � ��� 2-�� ���浪�.
                        ** �᫨ �� ᮢ������, � ���뢠�� ���� �� ।���஢����. */
      IF       (LENGTH (mSide) LE 1
         AND  (vSide   EQ mSide
            OR iMode   EQ {&MOD_ADD}))
            OR mSide   EQ "��"
      THEN ASSIGN
         vSide               = IF mSide NE "��" THEN mSide ELSE "-"
         vSide:SCREEN-VALUE  = vSide
         vSide:SENSITIVE     = NO
      .

                        /* ���� ���� ᫥��� ��������. */
      ELSE vSide:SENSITIVE   = YES.
                        /* ����⢨� ��� ०��� ᮧ�����. */
      IF iMode EQ {&MOD_ADD}
      THEN DO:
                        /* � ����� ����� ����� ���� ����� ���. */
         IF GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "foreign-curr")   EQ "NO"
            THEN tt-acct.currency:SCREEN-VALUE = "".
                        /* ���樠������ ���� "�����祭��". */
         IF tt-acct.contract:SCREEN-VALUE EQ ""
            THEN tt-acct.contract:SCREEN-VALUE  = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "contract").
                        /* ���樠������ ���� KAU-ID. */
         IF mKau-id:SCREEN-VALUE   EQ ""
         THEN ASSIGN
            mKau-id           :SCREEN-VALUE  = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "kau-id")
            vNameKau          :SCREEN-VALUE  = IF {assigned mKau-id:SCREEN-VALUE}
                                                   THEN IF mKau-id:SCREEN-VALUE EQ "__Multi"
                                                      THEN "������⢥���� �㡠����⨪�"
                                                      ELSE GetCodeName  ("�������",  mKau-id:SCREEN-VALUE)
                                                   ELSE ""
         .
                        /* ���樠������ ���� Cust-Cat */
         vCustCat = GetValueAttr ("bal-acct", tt-acct.bal-acct:SCREEN-VALUE, "cust-cat").
         IF     INDEX ("����", vCustCat)   NE 0
                /* �� ���뢠�� ���� cust-cat, �᫨ ��� ���祭�� ��।��� � ��� ����� */
            AND NOT (NUM-ENTRIES (iInstanceList, CHR (3)) GE 3
                     AND   ENTRY (3, iInstanceList, CHR (3)) NE "?")
         THEN DO:
            ASSIGN
               tt-acct.cust-cat  :SCREEN-VALUE  = vCustCat
               tt-acct.cust-cat  :SENSITIVE     = tt-acct.bal-acct:SENSITIVE
            .
            APPLY "VALUE-CHANGED" TO tt-acct.cust-cat.
         END.
      END.
   END.
   RETURN.
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
  DISPLAY vNameBranch vNameClient unk$ vSide vNameUser vNameBlock vNameControl
          mKau-id vNameKau separator1 separator2 separator3 separator4
          vDateAcctPos mNrAcctCur vAcctCur mNrAcctPos vAcctPos vDateDocAk
          vAcctQty sh-val-close mNrShValClose sh-bal-close mNrShBalClose
          mNrShAvCur vAvCur mNrShAvPos vAvPos vDateDocNAk mNrShValNa sh-val-na
          mNrShBalNa sh-bal-na vDateDocAv mChoice sh-qty-close sh-qty-na
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-acct THEN
    DISPLAY tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id tt-acct.bal-acct
          tt-acct.currency tt-acct.acct tt-acct.rate-type tt-acct.open-date
          tt-acct.close-date tt-acct.last-date tt-acct.Details
          tt-acct.alw#tnomer$ tt-acct.contract tt-acct.pribubyt446$ 
          tt-acct.contr-acct tt-acct.user-id tt-acct.check-op
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-acct.branch-id tt-acct.cust-cat tt-acct.cust-id unk$
         tt-acct.rate-type mNrAcctCur vAcctCur mNrAcctPos vAcctPos vAcctQty
         sh-val-close mNrShValClose sh-bal-close mNrShBalClose mNrShAvCur
         vAvCur mNrShAvPos vAvPos mNrShValNa sh-val-na mNrShBalNa sh-bal-na
         mChoice sh-qty-close sh-qty-na
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalEnableDisable TERMINAL-SIMULATION
PROCEDURE LocalEnableDisable :
DEF VAR vCustCat  AS CHAR   NO-UNDO. /* ��� ������ � ��� 2-�� ���浪�. */
   DEF VAR vXattrFmt AS CHAR   NO-UNDO. /* ��ଠ� ४����� */ 
DEF VAR vFSens AS LOG NO-UNDO.     

                        /* ��⠭���� ᢮��� ���� "���� ���". */
   RUN ChkFldPropContr-Acct.
                        /* ���� ����� ��� 㪠����� ���. */
   DO WITH FRAME fMain:
      IF vEditSet EQ "��" THEN
      DO:  
         FIND FIRST op-date WHERE op-date.op-date EQ tt-acct.open-date NO-LOCK NO-ERROR.
         IF NOT AVAIL(op-date) THEN DO:           
            FIND FIRST op-date WHERE op-date.op-date GE tt-acct.open-date NO-LOCK NO-ERROR.
            IF AVAIL(op-date) THEN
               IF Chk_Date_Cat(op-date.op-date, "b") THEN
                  ASSIGN
                     tt-acct.open-date  :SENSITIVE  = NO.
         END.
         IF Chk_Date_Cat(op-date.op-date, "b") THEN
            ASSIGN
               tt-acct.open-date  :SENSITIVE  = NO.
         IF tt-acct.close-date NE ?  THEN
         DO:
            FIND FIRST op-date WHERE op-date.op-date EQ tt-acct.close-date NO-LOCK NO-ERROR.
            IF NOT AVAIL(op-date) THEN DO:           
               FIND FIRST op-date WHERE op-date.op-date GE tt-acct.close-date NO-LOCK NO-ERROR.
               IF AVAIL(op-date) THEN
                  IF  Chk_Date_Cat(op-date.op-date, "b") THEN
                     ASSIGN
                        tt-acct.close-date :SENSITIVE  = NO
                        vFSens = YES
                     .
            END.
            IF  Chk_Date_Cat(op-date.op-date, "b") THEN
               ASSIGN
                  tt-acct.close-date :SENSITIVE  = NO
                  vFSens = YES
               . 
         END.
      END.       
      IF    iMode EQ {&Mod_Edit}
         OR iMode EQ {&Mod_ADD}
      THEN DO:
         IF       NUM-ENTRIES (iInstanceList, CHR (3))   GE 2
            AND   ENTRY (2, iInstanceList, CHR (3))      NE "?"
            AND   ENTRY (2, iInstanceList, CHR (3))      NE "0"
            THEN tt-acct.bal-acct:SENSITIVE = NO.
                                 /* ��⠭���� ᢮��� �� �易��� ����. */
         RUN ChkFldPropContr-Bal-Acct.

         IF       NUM-ENTRIES (iInstanceList, CHR (3))   GE 3
            AND   ENTRY (3, iInstanceList, CHR (3))      NE "?"
         THEN ASSIGN
            tt-acct.cust-cat  :SENSITIVE  = NO
            tt-acct.cust-id   :SENSITIVE  = NO
            unk$              :SENSITIVE  = NO
            vNameClient       :SENSITIVE  = NO
         .
         vXattrFmt = GetXAttrEx("_user","�������⤥�","Data-Format").
         /* ������� ��� ࠧ���� ���짮��⥫� �������� ��� ���ࠧ������� �� ��� */
         tt-acct.branch-id:SENSITIVE = LOGICAL(GetXAttrValueEx("_user",USERID("bisquit"),"�������⤥�",STRING(NO,vXattrFmt)),vXattrFmt).
      END.
      IF iMode EQ {&Mod_Edit}
      THEN DO:
         vCustCat = GetValueAttr ("bal-acct", STRING (tt-acct.bal-acct), "cust-cat").
         ASSIGN
                        /* ���� ⨯ ������ ���뢠��, �᫨:
                        ** - �� ��� 2-�� ���浪� �� 㪠��� ⨯ ������,
                        ** - ⨯ ������ ��� �� ᮢ������ � ⨯��,
                        ** 㪠����� �� ��� 2-�� ���浪�. */
            tt-acct.cust-cat  :SENSITIVE  =     vCustCat EQ ""
                                             OR vCustCat NE tt-acct.cust-cat
            tt-acct.rate-type :SENSITIVE  = tt-acct.currency   NE ""
            mKau-id:SENSITIVE             = NOT CAN-FIND(FIRST kau WHERE kau.acct EQ tt-acct.acct AND kau.currency EQ tt-acct.currency)
         .
         IF NOT vFSens THEN 
            tt-acct.close-date:SENSITIVE  = tt-acct.close-date NE ?.
      END.

   /* �⮡ࠦ���� ���� KAU-ID */
      mKau-id:SCREEN-VALUE = mKauId.
                        /* � ०��� ᮧ����� ��ᨬ ���� �㬬 ����᫮���. */
      IF iMode EQ {&MOD_ADD}
      THEN DO:
         ASSIGN
            vAcctCur     :VISIBLE   = NO
            vAcctPos     :VISIBLE   = NO
            sh-val-close :VISIBLE   = NO
            sh-bal-close :VISIBLE   = NO
            sh-val-na    :VISIBLE   = NO
            sh-bal-na    :VISIBLE   = NO
            vAvPos       :VISIBLE   = NO
            vAvCur       :VISIBLE   = NO
         .
         APPLY "VALUE-CHANGED" TO tt-acct.cust-cat.
      END.
      ELSE DO:

         ASSIGN
                        /* ������ ���⪨ ⮫쪮 ��� ������� ��⮢. */
            vAcctCur     :VISIBLE   =  tt-acct.currency  NE ""
            sh-val-close :VISIBLE   =  tt-acct.currency  NE ""
            sh-val-na    :VISIBLE   =  tt-acct.currency  NE ""
            vAvCur       :VISIBLE   =  tt-acct.currency  NE "" AND {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
                                                               AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
            vAvPos       :VISIBLE   =  {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
                                                              AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
            tt-acct.cust-id :VISIBLE =  tt-acct.cust-cat  NE "�"
         .
                        /* �᫨ ����⮪ �� ������� �� ࠢ�� ��� �
                        ** �������� �� �뫮, � ��ᨬ ����. */
         IF       vAcctPos       EQ 0
            AND   vDateAcctPos   EQ ?
         THEN ASSIGN
            vAcctPos:VISIBLE  = NO
            vAcctCur:VISIBLE  = NO
         .
                        /* �᫨ ��楯⮢���� ���⮪ ࠢ�� ��� �
                        ** � ���ࢠ�� �� ��᫥����� �����⮣� �� ��
                        ** ⥪�饩 ���� ��� �������� � ����ᮬ ���
                        ** ��� ࠢ���� "���",
                        ** � ��ᨬ ���� ��楯⮢����� ���⪮�. */
         IF       sh-bal-close   EQ 0
            AND   NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, YES)
         THEN DO:
            ASSIGN
               sh-val-close:VISIBLE =  NO
               sh-bal-close:VISIBLE =  NO
            .
                        /* �᫨ ���楯⮢���� ���⮪ ࠢ�� ��� �
                        ** � ���ࢠ�� �� ��᫥����� �����⮣� �� ��
                        ** ⥪�饩 ���� ��� �������� � ����ᮬ ���� "���",
                        ** � ��ᨬ ���� ���楯⮢����� ���⪮�. */
            IF NOT GetFirtsMoveDate (tt-acct.acct, vDateAcctPos, gEnd-date, NO) THEN DO:
               IF       sh-bal-na   EQ 0
               THEN ASSIGN
                  sh-val-na:VISIBLE =  NO
                  sh-bal-na:VISIBLE =  NO
               .

               IF vAvPos:VISIBLE AND vAvPos EQ 0
               THEN ASSIGN
                  vAvPos:VISIBLE =  NO
                  vAvCur:VISIBLE =  NO
               .
            END.
         END.
      END.
      ASSIGN
         vAcctQty     :VISIBLE   = NO
         sh-qty-close :VISIBLE   = NO
         sh-qty-na    :VISIBLE   = NO.

      /* ��뢠�� ��� ��� cust-id � ����ᨬ��� �� ������ ४����� ��� �� ����� */
      IF mFlagUnk
         THEN tt-acct.cust-id:HIDDEN IN FRAME fMain = YES.
         ELSE unk$           :HIDDEN IN FRAME fMain = YES.

      /* ���� ��� �ਡ����446 ����㯭� ⮫쪮 ��� ��⮢ ����� acctb-pl */
      ASSIGN
         tt-acct.pribubyt446$:HIDDEN IN FRAME fMain = NOT tt-acct.class-code matches "acctb-pl*"
         tt-acct.pribubyt446$:SENSITIVE = NO
      .
   END.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_CTRL-G TERMINAL-SIMULATION
PROCEDURE Local_CTRL-G :
IF iMode EQ {&MOD_VIEW}
   THEN RUN printfrm.p (FRAME fMain:HANDLE).
   RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_F9 TERMINAL-SIMULATION
PROCEDURE Local_F9 :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE vBlock AS CHARACTER NO-UNDO.

vBlock = "".

IF iMode EQ {&MOD_VIEW} THEN
DO:

    IF tt-acct.cust-cat EQ "�" THEN
        vBlock = GetXAttrValueEx("cust-corp", STRING(tt-acct.cust-id),"����", "").

    IF tt-acct.cust-cat EQ "�" THEN
        vBlock = GetXAttrValueEx("banks", STRING(tt-acct.cust-id),"����", "").

    IF tt-acct.cust-cat EQ "�" THEN
        vBlock = GetXAttrValueEx("person", STRING(tt-acct.cust-id),"����", "").

    IF vBlock EQ "��" OR vBlock EQ "Yes" THEN
    DO:
        RUN Fill-SysMes IN h_tmess ("", "", 1, "������ �������஢��! ��������� ४����⮢ ����������!").
        RETURN ERROR.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_GO TERMINAL-SIMULATION
PROCEDURE Local_GO :
DEFINE VAR vCustNameLine AS CHARACTER NO-UNDO.
   IF iMode eq {&MOD_ADD} THEN 
   DO:
      IF tt-acct.cust-cat EQ "�" AND 
         AVAIL tt-deputy
      THEN 
      ASSIGN
         tt-deputy.acct       = tt-acct.acct
         tt-deputy.currency   = tt-acct.currency
         tt-deputy.person-id  = tt-acct.cust-id
         tt-deputy.right-priv = NO
      .
                        /* ������塞 ����� ���.
                        ** ������ ��� ����室��� ��� ���� �����஢��.
                        ** ��稭� �����஢�� - ���������� ����ୠ⨢���� ����
                        ** ��������묨 ���祭�ﬨ �� �ࠡ��뢠��� �ਣ��.*/
      ASSIGN
         tt-acct.number = TRIM (ENTRY (1, tt-acct.acct, "@"))
         tt-acct.side   = IF vSide EQ "-" THEN  "��" ELSE vSide
         tt-acct.kau-id = mKau-id:SCREEN-VALUE IN FRAME fMain
      .

      IF NOT CAN-DO(FGetSetting("�����������","",""),tt-acct.acct) THEN DO:
         RUN chk-blk.p (tt-acct.cust-cat,tt-acct.cust-id).
         IF RETURN-VALUE EQ "0" THEN DO:
            {getcustline.i &cust-cat = "tt-acct.cust-cat" &cust-id = "tt-acct.cust-id" &output-to = "vCustNameLine"}
            RUN Fill-SysMes IN h_tmess ("", "acct43", "", "%s=" + vCustNameLine + "%s=" + STRING(tt-acct.number,GetAcctFmt(tt-acct.acct-cat))).
            RETURN ERROR {&RET-ERROR}.
         END.
      END.
   END.
   IF iMode EQ {&MOD_ADD} THEN DO:
      IF mKau-id:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST code WHERE code.class EQ "�������" AND code.code EQ (mKau-id:SCREEN-VALUE IN FRAME fMain))
      THEN DO:
          RUN Fill-SysMes IN h_tmess ("","","-1","�� ������ ��� ��� " + mKau-id:SCREEN-VALUE IN FRAME fMain).
          APPLY "ENTRY" TO mKau-id IN FRAME fmain.
          RETURN ERROR {&RET-ERROR}.
      END.
   END.   
   IF iMode eq {&MOD_EDIT} THEN DO:

      IF mKau-id:SCREEN-VALUE IN FRAME fMain NE mKauID THEN
         tt-acct.kau-id = mKau-id:SCREEN-VALUE IN FRAME fMain.

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local_PostJoin TERMINAL-SIMULATION
PROCEDURE Local_PostJoin :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vFldH  AS HANDLE NO-UNDO.
    /* �����஢�� ���� �����஢�� ���. */
    vNameBlock  =  BlockAcct (tt-acct.acct + ',' + tt-acct.currency,
                              DATETIME(gend-date,(IF gend-date = TODAY THEN MTIME ELSE 86399000))
                              ) GT "".
    vFldH          =  GetWidgetHandle (FRAME fmain:HANDLE, "vNameBlock").
    IF vNameBlock THEN
    vFldH:DCOLOR   =  GetDCOLOR ("bright-red").
    DISP vNameBlock
        @ vNameBlock
        WITH FRAME fmain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION
PROCEDURE PostGetObject :
DEF VAR vFrmh        AS HANDLE NO-UNDO.          /* �����⥫� �� �३�. */
   DEF VAR vFldH        AS HANDLE NO-UNDO.          /* �����⥫� �� ���� ���. */
   DEF VAR vName        AS CHAR   NO-UNDO EXTENT 2. /* ������������ ���. */
   DEF VAR vCnt         AS INT64    NO-UNDO.          /* ���稪. */
   DEF VAR vNrLst       AS CHAR   NO-UNDO           /* ���᮪ ����� ��� ��⠭����. */
                        INIT "mNrAcctCur,mNrShValClose,mNrShValNa,mNrShAvCur,mNrAcctPos,mNrShBalClose,mNrShBalNa,mNrShAvPos".
   DEF VAR vPosLst      AS CHAR   NO-UNDO           /* ���᮪ ����� �࠭��� ���⪨. */
                        INIT "".
   DEF VAR vSResult     AS CHAR   NO-UNDO.          /* ������� ���᪠. */
   DEF VAR vDateAcctCur AS DEC    NO-UNDO.          /* ��� ����⭮�� ���⪠. */
   DEF VAR vClassCode   AS CHAR   NO-UNDO.

         /* �஢�ઠ ������ �ࠢ� ��ᬮ�� ���ଠ樨 � ������ � ⥪�饣� ���짮��⥫� */
   IF    (iMode EQ {&MOD_EDIT}
      OR  iMode EQ {&MOD_VIEW})
      AND tt-acct.cust-cat EQ "�"
      AND NOT GetPersonPermission(tt-acct.cust-id)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "ap16", "", "%s=" + STRING(tt-acct.cust-id)).
      RETURN ERROR.
   END.

                        /* �।��⠭���� ���祭�� (�室��� ��ࠬ��஢). */
   IF iMode EQ {&MOD_ADD}
   THEN DO:
      ASSIGN
         tt-acct.bal-acct  =  INT64 (ENTRY (2, iInstanceList, CHR (3)))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 2
                                    AND   ENTRY (2, iInstanceList, CHR (3)) NE "?"
                                    AND   ENTRY (2, iInstanceList, CHR (3)) NE "0"
                        /* �᫨ 㪠��� ������. */
         tt-acct.cust-cat  =  ENTRY (3, iInstanceList, CHR (3))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 3
                                    AND   ENTRY (3, iInstanceList, CHR (3)) NE "?"
         tt-acct.cust-id   =  INT64 (ENTRY (4, iInstanceList, CHR (3)))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 4
                                    AND   ENTRY (4, iInstanceList, CHR (3)) NE "?"
         tt-acct.branch-id =  ENTRY (5, iInstanceList, CHR (3))
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 5
                                    AND   ENTRY (5, iInstanceList, CHR (3)) NE "?"
      .
      ASSIGN
         tt-acct.contract  = GetValueAttr ("bal-acct", STRING (tt-acct.bal-acct), "contract")
                                 WHEN     NUM-ENTRIES (iInstanceList, CHR (3)) GE 2
                                    AND   ENTRY (2, iInstanceList, CHR (3)) NE "?".
   END.

   ASSIGN
      vFrmh          =  FRAME fMain:HANDLE
                        /* ��ନ஢���� �������� ���ࠧ�������. */
      vNameBranch    =  GetValueAttr ("branch", tt-acct.branch-id, "name")
                        /* ��ନ஢���� ���� "�⢥��⢥���". */
      vNameUser      =  GetValueAttr ("_user", tt-acct.user-id, "_user-name")
                        /* �����஢�� ���� �����஢�� ���. */
      vNameBlock     =  BlockAcct (tt-acct.acct + ',' + tt-acct.currency,
                                   DATETIME(gend-date,(IF gend-date = TODAY THEN MTIME ELSE 86399000))
                                  ) GT ""
                        /* �����஢�� ���� �����஢�� ���. */
      vNameControl   =  GetCodeName ("check-op", tt-acct.check-op)
      vDateDocAk     =  gEnd-Date
      vDateDocNAk    =  gEnd-Date
      vDateDocAv     =  gEnd-Date
                        /* ��⠭�������� �ଠ� �⮡ࠦ���� ���. */
      tt-acct.acct:FORMAT  =  GetAcctFmt (tt-acct.acct-cat)
      /* acct - ? �⠢�� � ⮬ ��砥, ����� ०�� - ᮧ����� � �� mOnlyForm, �.�.
                                ���⮩ ����� ��� ��� ��࠭���� ��ꥪ� */
      tt-acct.acct         =  ?
                              WHEN iMode EQ {&Mod_ADD} AND NOT mOnlyForm
   .
                        /* ����祭�� ���� 蠡���� ���. */
   ASSIGN
      mKauId   =  IF {assigned tt-acct.kau-id}
                     THEN tt-acct.kau-id
                     ELSE IF tt-acct.bal-acct GT 0
                        THEN GetValueAttr ("bal-acct", STRING (tt-acct.bal-acct), "kau-id")
                        ELSE ""
      vNameKau =  IF mKauId NE ""
                     THEN IF mKauId EQ "__Multi"
                        THEN "������⢥���� �㡠����⨪�"
                        ELSE GetCodeName ("�������", mKauId)
                     ELSE ""
      vSide    = IF tt-acct.side EQ "��" THEN "-" ELSE tt-acct.side
   .

   /* ��।��塞 �� �⮡ࠦ���. (��� ��� cust-id) */
   vClassCode = GetCustClass(tt-acct.cust-cat).
   {getflagunk.i &class-code="vClassCode" &flag-unk="mFlagUnk"}

   /* �᫨ �� ����� ���� �� "���", �.�. �㦭� �⮡ࠦ��� ���� "���" */
   IF mFlagUnk
      /* � ��।����� ⨯ � �����䨪��� ������ */
      AND {assigned tt-acct.cust-cat}
      AND {assigned STRING(tt-acct.cust-id)}
        /* �⠥� ���४����� ��� � ������ ��� �⮡ࠦ���� ��� � ���� ��� */
   THEN unk$              = GetXattrValueEx(GetCustClass(tt-acct.cust-cat), LEFT-TRIM(STRING(tt-acct.cust-id),"0"), "���", ?).
   ELSE unk$:SCREEN-VALUE = ?.

                        /* ������ ���ଠ�� ����� ������� ⮫쪮 � ��砥
                        ** ᮧ����� ��� ।���஢���� ���. */
   IF NOT (    iMode EQ {&Mod_View}
            OR iMode EQ {&Mod_Edit})
      THEN RETURN.
                        /* ����祭�� ������������ ������. */
   RUN GetCustName IN h_base (
      tt-acct.cust-cat,
      tt-acct.cust-id,
      tt-acct.acct,
      OUTPUT vName [1],
      OUTPUT vName [2],
      INPUT-OUTPUT vInn
   ).
   vNameClient = TRIM (vName [1] + " " + vName [2]).
   vCustId-ScV = STRING(tt-acct.cust-id).
                        /* ��।������ �ࠢ ����㯠 � ��ᬮ��� ���⪠. */
   IF AcctLookBuffer ((BUFFER tt-acct:HANDLE))
   THEN DO:
                        /* ���� ���⪠ � ��樮���쭮� �����. */
      RUN GetAcctPos IN h_acct (
         (BUFFER tt-acct:HANDLE),
         gEnd-Date,
         OUTPUT vAcctPos,
         OUTPUT vDateAcctPos
      ).
      IF vAcctPos EQ ?
         THEN vAcctPos = 0.
                        /* ���� ���⪠ � ������� �����. */
      IF tt-acct.acct NE ""
      THEN DO:
         RUN GetAcctCur IN h_acct (
            (BUFFER tt-acct:HANDLE),
            gEnd-Date,
            OUTPUT vAcctCur,
            OUTPUT vDateAcctCur
         ).
         IF vAcctCur EQ ?
            THEN vAcctCur = 0.
      END.
                         /*���� ����㯭��� ���⪠ */
      mAccessMask      = FGetSetting("�⠭���", "AccessAcct", "").
      mAccessContAcct  = FGetSetting("�⠭���", "AccessContAcct", "").

      IF {assigned mAccessMask} AND CAN-DO(mAccessMask,tt-acct.acct)
         AND {assigned mAccessContAcct} AND CAN-DO(mAccessContAcct,tt-acct.contract)
      THEN
         RUN CalcAvailPos(tt-Acct.acct, tt-Acct.currency, gend-date, gend-date,"�","�","cli-pos", YES, "*", YES, OUTPUT vAvPos, OUTPUT vAvCur).
      IF iMode NE {&Mod_ADD} AND AvailXattr("acct",tt-acct.acct + "," + tt-acct.currency,"sec-code")
         THEN DO:

         RUN GetAcctQty IN h_acct (
            (BUFFER tt-acct:HANDLE),
            gEnd-Date,
            OUTPUT vAcctQty,
            OUTPUT mDateAcctQty
         ).
         IF vAcctQty EQ ?
            THEN vAcctQty = 0.

         RUN acct-qty IN h_base (
            tt-acct.acct,
            tt-acct.currency,
            gend-date,
            gend-date, "�"
         ).
         ASSIGN
            sh-qty-close = sh-qty
         .
         RUN acct-qty IN h_base (
            tt-acct.acct,
            tt-acct.currency,
            gend-date,
            gend-date,
            "�"
         ).
         ASSIGN
            sh-qty-na = sh-qty.
      END.
                        /* ��।������ ���⪠ �� ���. */
      RUN acct-pos IN h_base (
         tt-acct.acct,
         tt-acct.currency,
         gend-date,
         gend-date, "�"
      ).
      ASSIGN
         sh-val-close = sh-val
         sh-bal-close = sh-bal
      .
      RUN acct-pos IN h_base (
         tt-acct.acct,
         tt-acct.currency,
         gend-date,
         gend-date,
         "�"
      ).
   END.
   ELSE DO:
      ASSIGN
                        /* ���㫥��� ���⪮�. */
         sh-bal         =  0
         sh-val         =  0
      .
      DO vCnt = 1 TO NUM-ENTRIES (vNrLst):
         ASSIGN
            vFldH                =  GetWidgetHandle (vFrmh, ENTRY (vCnt, vNrLst))
            vFldH:DCOLOR         =  GetDCOLOR ("bright-yellow")
         .
      END.
      ASSIGN
         mNrAcctCur     = "            ��� �������"
         mNrShValClose  = "            ��� �������"
         mNrShValNa     = "            ��� �������"
         mNrAcctPos     = "            ��� �������"
         mNrShBalClose  = "            ��� �������"
         mNrShBalNa     = "            ��� �������"
         mNrShAvPos     = "            ��� �������"
         mNrShAvCur     = "            ��� �������"
      .
   END.
   ASSIGN
      vFldH          =  GetWidgetHandle (vFrmh, "vDateAcctPos")
      vFldH:DCOLOR   =  GetDCOLOR ("green")
      vFldH          =  GetWidgetHandle (vFrmh, "close-date")
      vFldH:DCOLOR   =  GetDCOLOR ("bright-yellow")

                        /* ��ନ஢���� 梥�, �ଠ� ��� �⮡ࠦ���� ���⪠. */
      vFldH          =  GetWidgetHandle (vFrmh, "vAcctCur")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAcctCur))
      vFldH:FORMAT   =  IF vAcctCur GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      vAcctCur       =  ABSOLUTE (vAcctCur)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-val-close")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-val-close))
      vFldH:FORMAT   =  IF sh-val-close GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      sh-val-close   =  ABSOLUTE (sh-val-close)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-val-na")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-val))
      vFldH:FORMAT   =  IF sh-val GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      sh-val-na      =  ABSOLUTE (sh-val)

      vFldH          =  GetWidgetHandle (vFrmh, "vAcctPos")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAcctPos))
      vFldH:FORMAT   =  IF vAcctPos GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      vAcctPos       =  ABSOLUTE (vAcctPos)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-bal-close")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-bal-close))
      vFldH:FORMAT   =  IF sh-bal-close GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      sh-bal-close   =  ABSOLUTE (sh-bal-close)

      vFldH          =  GetWidgetHandle (vFrmh, "sh-bal-na")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), sh-bal))
      vFldH:FORMAT   =  IF sh-bal GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      sh-bal-na      =  ABSOLUTE (sh-bal)

      vFldH          =  GetWidgetHandle (vFrmh, "vAvCur")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAvCur))
      vFldH:FORMAT   =  IF vAvCur GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      vAvCur         =  ABSOLUTE (vAvCur)

      vFldH          =  GetWidgetHandle (vFrmh, "vAvPos")
      vFldH:DCOLOR   =  GetDCOLOR (GetBalColorBuffer ((BUFFER tt-acct:HANDLE), vAvPos))
      vFldH:FORMAT   =  IF vAvPos GE 0
                           THEN "��zz,zzz,zzz,zzz,zz9.99"
                           ELSE "��zz,zzz,zzz,zzz,zz9.99"
      vAvPos         =  ABSOLUTE (vAvPos)
      .

   IF iMode NE {&Mod_ADD} AND AvailXattr("acct",tt-acct.acct + "," + tt-acct.currency,"sec-code") THEN
      SetHelpStrAdd("F3 - � �������/���⮪ ��").

   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION
PROCEDURE PostSetObject :
DEF VAR vOk          AS LOG    NO-UNDO. /* ���� �訡��. */
   DEF VAR vError       AS LOG    NO-UNDO. /* ���� �訡��. */
   DEF VAR vField       AS CHAR   NO-UNDO. /* ��� ���� �訡��. */
   DEF VAR vProcName    AS CHAR   NO-UNDO. /* ��楤�� ��⮤�. */
   DEF VAR vUpdValid    AS INT64    NO-UNDO. /* ��� �訡��. */

   DEFINE VARIABLE vClient      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClDate      AS DATE        NO-UNDO.
   DEFINE VARIABLE vDate-In     AS DATE        NO-UNDO.
   DEFINE VARIABLE vIsStopList  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vStr         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStatIP      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDocID       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDoc         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMaskAcct    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFlChk       AS LOGICAL     NO-UNDO.
   DEFINE BUFFER   xop-date1 FOR op-date.
   DEFINE BUFFER   xop-date2 FOR op-date.   
   DEFINE BUFFER   person    FOR person.
   DEFINE BUFFER   cust-corp FOR cust-corp. 
   
                        /* ���� ��࠭������ ���. */
                        
                        
   {find-act.i
      &acct    = tt-acct.acct
      &curr    = tt-acct.currency
   }
   IF NOT AVAIL acct
   THEN DO:
      RUN Fill-SysMes IN h_tmess (
         "", "18l", "",
         "%S=" + tt-acct.acct + "/" + tt-acct.currency
      ).
      RETURN ERROR.
   END.

   IF (iMode EQ {&MOD_ADD} OR (iMode EQ {&MOD_EDIT}  AND tt-acct.open-date:SENSITIVE IN FRAME fMain)) AND vEditSet EQ "��"
   THEN DO:
 
       vClDate = Get_CloseDate_Cat("b").
       FIND FIRST xop-date1 WHERE xop-date1.op-date EQ acct.open-date NO-LOCK NO-ERROR.
       IF NOT AVAIL(xop-date1) AND acct.open-date  NE ? THEN DO:
          RUN Fill-SysMes (
               "", "", "-1","�� ���� ������ ��������� ����樮��� ����").
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.  
       END.
       FIND FIRST xop-date2 WHERE xop-date2.op-date EQ acct.close-date NO-LOCK NO-ERROR.
       IF NOT AVAIL(xop-date2) AND acct.close-date NE ? THEN DO:
          RUN Fill-SysMes (
               "", "", "-1","�� ���� ������� ��������� ����樮��� ����").
          APPLY "ENTRY" TO tt-acct.close-date IN FRAME fmain.
          RETURN ERROR.  
       END.       
      IF acct.open-date LE vClDate OR
         acct.close-date LE vClDate THEN
      DO:
          RUN Fill-SysMes (
               "", "", "-1",
               (IF acct.open-date LE vClDate THEN
                   "����樮��� ���� �� ���� ������ ������"
                ELSE
                   "����樮��� ���� �� ���� ������� ������")               
          ).       
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.        
      END.
      IF acct.open-date LE vClDate OR
         acct.close-date LE vClDate THEN
      DO:
          RUN Fill-SysMes (
               "", "", "-1",
               (IF acct.open-date LE vClDate THEN
                   "����樮��� ���� �� ���� ������ ������"
                ELSE
                   "����樮��� ���� �� ���� ������� ������")               
          ).       
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.        
      END.
   END.

   IF (iMode EQ {&MOD_ADD}   OR iMode EQ {&MOD_EDIT} ) AND
      LOOKUP(acct.cust-cat,"�,�,�") > 0
   THEN DO:
      /* �஢�ઠ ���� ��������� ������ */
      vDate-In = DATE(getValueAttr(getCustClass(acct.cust-cat),
                      STRING(acct.cust-id),
                     "date-in")) NO-ERROR.
       IF vDate-In > acct.open-date THEN DO:
          RUN Fill-SysMes (
               "", "", "-1","��������! ��� ������ ��� �� ����� ���� ����� ���� ॣ����樨 ������!").
          APPLY "ENTRY" TO tt-acct.open-date IN FRAME fmain.
          RETURN ERROR.  
       END.
   END.
                        /* ���樠������ ���.४����⮢ � ��� 2-�� ���浪�
                        ** � �� �����䨪��� "��᪨��᫥�". */
   RUN BalToAcct_Xattr ( RECID (acct), "*", YES, YES).

   RUN Check-Acct IN h_acct (
      BUFFER acct,
      OUTPUT vField,
      OUTPUT vUpdValid
   ).
   IF vUpdValid NE 0
      THEN RETURN ERROR.

   IF iMode EQ {&MOD_ADD}
   THEN DO:
                        /* ����塞 ��� �� �����䨪��� "��⠐���ࢠ". */
      RUN AcctFree IN h_acct (AddFilToAcct(tt-acct.acct, shFilial), OUTPUT vOk).
      ASSIGN
         vProcName = GET-CLASS-METHOD (tt-acct.class-code, "U4").
      IF vProcName NE ?
      THEN DO:
         IF SearchPFile (vProcName)
            THEN RUN VALUE (vProcName + ".p") ( RECID (acct)).
         ELSE DO:
            RUN Fill-SysMes (
               "", "", "0",
               "�� ������� ��楤�� 㤠����� ���稪� " + vProcName + ".p."
            ).
            RETURN ERROR.
         END.
      END.
   END.

   vClient = vNameClient:SCREEN-VALUE IN FRAME {&MAIN-FRAME}.
   {lg7001cl.i
      &in-class  = 'acct'
      &surrogate = "tt-acct.acct + ',' + tt-acct.currency"
      &cl_name1  = "vClient"
      &nodefpesr = YES
   }

   vError = FALSE.
   RUN LoanChk (tt-acct.contract,
                tt-acct.acct,
                tt-acct.currency,
                tt-acct.cust-cat,
                tt-acct.cust-id,
                INPUT-OUTPUT vError).

   /*�஢�ઠ �� �⮯-���⠬*/
   IF iMode EQ {&MOD_ADD}   OR iMode EQ {&MOD_EDIT} 
   THEN DO:
      RUN ChkByStopList (tt-acct.cust-cat,tt-acct.cust-id,"������������_��","CLNT",OUTPUT vIsStopList).
      IF vIsStopList THEN
      DO:
         pick-value = "NO".   
         RUN Fill-SysMes IN h_tmess (  "", "", "4", "������ ���� 䨣�࠭⮬ �ࠢ�筨�� �⮯-�����! �த������ ���� ���?").
      END.
      IF pick-value = "NO" THEN
      DO:
         {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
         RUN PrintClientSLRep.
         {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"}
         RETURN ERROR.
      END.
   END.  

   /* �஢�ઠ ����⢨⥫쭮�� ��ᯮ�� �� ������ �� ���� ��� */
   vStr = FGetSetting("��������","������_���","").
   IF (iMode EQ {&MOD_ADD} OR iMode EQ {&MOD_EDIT}) AND
      (FGetSetting("��������","������_��","") EQ "��") AND
      CAN-DO(vStr,STRING(tt-acct.bal-acct))
   THEN DO:
      vFlChk = no.
      IF tt-acct.cust-cat EQ "�" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ tt-acct.cust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN DO:
            ASSIGN
               vStr    = GetXattrValueEX("cust-corp", STRING(tt-acct.cust-id), "�।��", "")
               vStatIP = FGetSetting("�⠭���","����ᔋ��","")
            .
            IF FGetSetting("��������","������_���","") EQ "��" AND
               (vStr NE "" OR CAN-DO(vStatIP,cust-corp.cust-stat))
            THEN 
               ASSIGN
                  vStr    = "cust-corp"
                  vDocID  = GetXattrValueEX("cust-corp",STRING(cust-corp.cust-id),"document-id","")
                  vDoc    = GetXattrValueEX("cust-corp",STRING(cust-corp.cust-id),"document","")
                  vClient = cust-corp.name-corp
                  vFlChk  = yes
               .
         END.
      END.   
      ELSE
         IF tt-acct.cust-cat EQ "�" AND 
            FGetSetting("��������","������_��","") EQ "��"
         THEN DO:
            FIND FIRST person WHERE person.person-id EQ tt-acct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL person THEN 
               ASSIGN
                  vStr    = "person"
                  vDocID  = person.document-id
                  vDoc    = person.document
                  vClient = person.name-last + " " + person.first-names
                  vFlChk  = yes
               .
         END.
      IF vFlChk THEN DO:
         {cl-fmschk.i
            "'acct'"
            "tt-acct.acct + ',' + tt-acct.currency"
            "vDocID"
            "vDoc"
            "GetXattrValueEX(vStr, STRING(tt-acct.cust-id), 'Document4Date_vid', '')"
            "vClient"
         }
      END.
   END. 

   IF (iMode EQ {&MOD_ADD} OR iMode EQ {&MOD_EDIT} ) AND
      LOOKUP(acct.cust-cat,"�,�,�") > 0
   THEN DO:
      vMaskAcct = FGetSetting("�⠭���","�������℮�","").
      IF CAN-DO(vMaskAcct,acct.acct) THEN
      DO:
         RUN SetSysConf IN h_base ("PROCESS_OP-EDIT","��").
         RUN chkpersdoc.p ("",acct.cust-cat,acct.cust-id,gend-date,"3").
         RUN SetSysConf IN h_base ("PROCESS_OP-EDIT",?).
         IF RETURN-VALUE EQ "YES" THEN
            RETURN ERROR.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoanChk TERMINAL-SIMULATION
/* ��७�ᥭ� �� acct-kl(.p */
PROCEDURE LoanChk :
   DEF INPUT        PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT        PARAM iAcct     AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCustCat  AS CHAR NO-UNDO.
   DEF INPUT        PARAM iCustId   AS INT64  NO-UNDO.
   DEF INPUT-OUTPUT PARAM ioError   AS LOG  NO-UNDO.

   DEF VAR vError AS LOG NO-UNDO INITIAL FALSE.

   DEF BUFFER cacct      FOR acct.
   DEF BUFFER loan-acct  FOR loan-acct.
   DEF BUFFER xloan-acct FOR loan-acct.
   DEF BUFFER bloan      FOR loan.

   IF     iContract EQ "�।��"
      OR  iContract EQ "�����"
      AND iCustCat  NE "�" THEN
   DO:
      FOR EACH loan-acct WHERE
               loan-acct.contract  EQ iContract
           AND loan-acct.acct      EQ iAcct
           AND loan-acct.currency  EQ iCurrency
           AND loan-acct.acct-type EQ loan-acct.contract NO-LOCK,
          EACH loan WHERE
               loan.contract       EQ loan-acct.contract
           AND loan.cont-code      EQ loan-acct.cont-code
         NO-LOCK
         BREAK BY loan.cont-code :
         IF LAST-OF(loan.cont-code) THEN
         DO:
            vError = FALSE.
            LOOP:
            FOR EACH xloan-acct OF loan WHERE
                     xloan-acct.acct <> iAcct
               NO-LOCK:

               IF GetCodeMisc("����焮�",xloan-acct.acct-type,1) EQ "���" THEN
                  NEXT.

               FIND FIRST cacct WHERE
                          cacct.acct     EQ xloan-acct.acct
                      AND cacct.currency EQ xloan-acct.currency NO-LOCK NO-ERROR.

               IF cacct.cust-cat NE "�"
                  AND (   cacct.cust-cat NE iCustCat
                       OR cacct.cust-id  NE iCustId) THEN
               DO:
                  IF ioError THEN
                  DO:
                     RUN Fill-SysMes("","","-1",
                            "��� �� �������� " + loan.cont-code  + "~n" +
                            "� ������� ���� ��� � ��㣨�� �����⠬�").
                     RETURN.
                  END.
                  vError = TRUE.
                  LEAVE LOOP.
               END.
            END.
            IF NOT ioError AND NOT vError THEN DO:
              fixloan:
              DO TRANSACTION
                 ON ERROR  UNDO fixloan, LEAVE fixloan
                 ON ENDKEY UNDO fixloan, LEAVE fixloan:
                 FIND FIRST bloan WHERE
                            ROWID(bloan) = ROWID(loan) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                  IF AVAILABLE bloan THEN
                  ASSIGN
                    bloan.cust-cat = iCustCat
                    bloan.cust-id  = iCustId
                    .
                 RELEASE bloan.
              END.
            END.

         END.
      END.
   END.
   ioError = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='29/02/2016 14:42:53.296+04:00' */
/* $LINTUSER='miam' */
/* $LINTMODE='1' */
/* $LINTFILE='f-acct.p' */
/*prosignOz9Hvcq92wLb0UiL/NnjPQ*/