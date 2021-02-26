&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-person NO-UNDO LIKE person
       FIELD okato_302$ AS CHARACTER /* �����_302 */
       FIELD strahnomer$ AS CHARACTER /* ���卮��� */
       FIELD koliwzd$ AS INT64 /* ������ */
       FIELD sempol$ AS CHARACTER /* ������ */
       FIELD kodsubckki$ AS CHARACTER /* ����㡖��� */
       FIELD rabota_viddewat$ AS CHARACTER /* �����_������� */
       FIELD rabotanawim$ AS DATE /* ����⠍��� */
       FIELD rabotanawimkon$ AS DATE /* ����⠍������ */
       FIELD rabota_polowzenie$ AS CHARACTER /* �����_��������� */
       FIELD rabota_zanwatostw#$ AS CHARACTER /* �����_�������� */
       FIELD umer$ AS LOGICAL /* ���� */
       FIELD izmfipredfam$ AS CHARACTER /* ������।��� */
       FIELD izmfipredimwa$ AS CHARACTER /* ������।��� */
       FIELD izmfidata$ AS DATE /* �������� */
       FIELD datapropiski$ AS DATE /* ��⠏ய�᪨ */
       FIELD nbki_streg$ AS CHARACTER /* ����_�␥� */
       FIELD nbki_stprowz$ AS CHARACTER /* ����_��஦ */
       FIELD izmdokprednomer$ AS CHARACTER /* �������।����� */
       FIELD izmdokdata$ AS DATE /* ��������� */
       FIELD rabota_sferadewat$ AS CHARACTER /* �����_��ࠄ��� */
       FIELD abwawtik$ AS CHARACTER /* ���騪 */
       FIELD arhiv$ AS LOGICAL /* ��娢 */
       FIELD blok$ AS LOGICAL /* ���� */
       FIELD viddewat$ AS CHARACTER /* ������� */
       FIELD vidkli$ AS CHARACTER /* ������ */
       FIELD vidsotr$ AS CHARACTER /* ������� */
       FIELD gvk$ AS CHARACTER /* ��� */
       FIELD grawzd$ AS CHARACTER /* �ࠦ� */
       FIELD gruppakl$ AS CHARACTER /* ��㯯��� */
       FIELD datavpred$ AS CHARACTER /* ��⠂�। */
       FIELD dataokpred$ AS CHARACTER /* ��⠎��। */
       FIELD dko$ AS DECIMAL /* ��� */
       FIELD dkowe$ AS DECIMAL /* ���� */
       FIELD dolwz$ AS CHARACTER /* ���� */
       FIELD dom$ AS CHARACTER /* ��� */
       FIELD iin$ AS CHARACTER /* ��� */
       FIELD indgr$ AS CHARACTER /* ����� */
       FIELD iobss$ AS CHARACTER /* ����� */
       FIELD iogbh$ AS CHARACTER /* ����� */
       FIELD kategfiz$ AS CHARACTER /* ��⥣��� */
       FIELD klient$ AS CHARACTER /* ������ */
       FIELD klientuf$ AS LOGICAL /* �����ⓔ */
       FIELD klreestr$ AS CHARACTER /* �������� */
       FIELD koddokum$ AS CHARACTER /* ������� */
       FIELD kodklienta$ AS CHARACTER /* ��������� */
       FIELD kodreg$ AS CHARACTER /* ������ */
       FIELD kodreggni$ AS CHARACTER /* ��������� */
       FIELD kodyadresa$ AS CHARACTER /* ���뀤�� */
       FIELD kop$ AS INT64 /* ��� */
       FIELD kopf$ AS INT64 /* ���� */
       FIELD korpkl$ AS CHARACTER /* ��ொ� */
       FIELD kpp$ AS CHARACTER /* ��� */
       FIELD licenzorg$ AS CHARACTER /* ��業��� */
       FIELD mestsvedpred$ AS CHARACTER /* ���③���। */
       FIELD migrkart$ AS CHARACTER /* �������� */
       FIELD migrpravprebpo$ AS DATE /* �����ࠢ�ॡ�� */
       FIELD migrpravprebs$ AS DATE /* �����ࠢ�ॡ� */
       FIELD migrprebyvpo$ AS DATE /* �����ॡ뢏� */
       FIELD migrprebyvs$ AS DATE /* �����ॡ뢑 */
       FIELD migrcelw#vizita$ AS CHARACTER /* �������삨��� */
       FIELD nalrez$ AS LOGICAL /* ������ */
       FIELD obrbs$ AS CHARACTER /* ����� */
       FIELD obrgb$ AS CHARACTER /* ����� */
       FIELD ogrn$ AS CHARACTER /* ���� */
       FIELD okato-nalog$ AS CHARACTER /* �����-����� */
       FIELD okvwed$ AS CHARACTER /* ����� */
       FIELD orgsvedpred$ AS CHARACTER /* �࣑����। */
       FIELD osnvidydewat$ AS CHARACTER /* �ᭂ��넥�� */
       FIELD ofwsor$ AS CHARACTER /* ���� */
       FIELD ocenkariska$ AS CHARACTER /* �業����᪠ */
       FIELD predpr$ AS LOGICAL /* �।�� */
       FIELD prim$ AS CHARACTER /* �ਬ */
       FIELD prim1$ AS CHARACTER /* �ਬ1 */
       FIELD prim2$ AS CHARACTER /* �ਬ2 */
       FIELD prim3$ AS CHARACTER /* �ਬ3 */
       FIELD prim4$ AS CHARACTER /* �ਬ4 */
       FIELD prim5$ AS CHARACTER /* �ਬ5 */
       FIELD prim6$ AS CHARACTER /* �ਬ6 */
       FIELD rabota$ AS CHARACTER /* ����� */
       FIELD rabotaadr$ AS CHARACTER /* ����⠀�� */
       FIELD riskotmyv$ AS CHARACTER /* ��᪎�� */
       FIELD sotoper$ AS CHARACTER /* ��⎯�� */
       FIELD svedvygdrlica$ AS CHARACTER /* �����룄���� */
       FIELD svedregpred$ AS CHARACTER /* ��������। */
       FIELD subw%ekt$ AS CHARACTER /* ��ꥪ� */
       FIELD telefon3$ AS CHARACTER /* ����䮭3 */
       FIELD ulica$ AS CHARACTER /* ���� */
       FIELD unk$ AS DECIMAL /* ��� */
       FIELD unkg$ AS INT64 /* ���� */
       FIELD uwcdok$ AS CHARACTER /* �焮� */
       FIELD uwcdokgr$ AS CHARACTER /* �焮��� */
       FIELD uwcdokdata$ AS DATE /* �焮���� */
       FIELD fiobs$ AS CHARACTER /* ����� */
       FIELD fiobuhg$ AS CHARACTER /* ������ */
       FIELD fiogb$ AS CHARACTER /* ����� */
       FIELD fioruk$ AS CHARACTER /* ����� */
       FIELD formsobs$ AS CHARACTER /* ��଑��� */
       FIELD formsobs_118$ AS CHARACTER /* ��଑���_118 */
       FIELD wa$ AS CHARACTER /* � */
       FIELD Address1Indeks AS INT64 /* Address1Indeks */
       FIELD Address2Gorod AS CHARACTER /* Address2Gorod */
       FIELD Address3Street AS CHARACTER /* Address3Street */
       FIELD Address4Dom AS CHARACTER /* Address4Dom */
       FIELD Address5Korpus AS CHARACTER /* Address5Korpus */
       FIELD Address6Kvart AS CHARACTER /* Address6Kvart */
       FIELD Address6Rayon AS CHARACTER /* Address6Rayon */
       FIELD BirthPlace AS CHARACTER /* BirthPlace */
       FIELD branch-id AS CHARACTER /* branch-id */
       FIELD branch-list AS CHARACTER /* branch-list */
       FIELD contr_group AS CHARACTER /* contr_group */
       FIELD contr_type AS CHARACTER /* contr_type */
       FIELD country-id2 AS CHARACTER /* country-id2 */
       FIELD country-id3 AS CHARACTER /* country-id3 */
       FIELD CountryCode AS CHARACTER /* CountryCode */
       FIELD date-export AS CHARACTER /* date-export */
       FIELD diasoft-id AS CHARACTER /* diasoft-id */
       FIELD Document1Ser_Doc AS CHARACTER /* Document1Ser_Doc */
       FIELD Document2Num_Doc AS CHARACTER /* Document2Num_Doc */
       FIELD Document3Kem_Vid AS CHARACTER /* Document3Kem_Vid */
       FIELD Document4Date_vid AS DATE /* Document4Date_vid */
       FIELD e-mail AS CHARACTER /* e-mail */
       FIELD engl-name AS CHARACTER /* engl-name */
       FIELD Exam AS CHARACTER /* Exam */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD holding-id AS CHARACTER /* holding-id */
       FIELD incass AS LOGICAL /* incass */
       FIELD IndCode AS CHARACTER /* IndCode */
       FIELD Isn AS CHARACTER /* Isn */
       FIELD lat_dop AS CHARACTER /* lat_dop */
       FIELD lat_fam AS CHARACTER /* lat_fam */
       FIELD lat_f_n AS CHARACTER /* lat_f_n */
       FIELD lat_name AS CHARACTER /* lat_name */
       FIELD lat_otch AS CHARACTER /* lat_otch */
       FIELD lat_titul AS CHARACTER /* lat_titul */
       FIELD LegTerr AS CHARACTER /* LegTerr */
       FIELD lic-sec AS CHARACTER /* lic-sec */
       FIELD Lic_num AS CHARACTER /* Lic_num */
       FIELD LocCustType AS CHARACTER /* LocCustType */
       FIELD mess AS CHARACTER /* mess */
       FIELD NACE AS CHARACTER /* NACE */
       FIELD Netting AS LOGICAL /* Netting */
       FIELD New$$ AS CHARACTER /* New */
       FIELD New1 AS CHARACTER /* New1 */
       FIELD NoExport AS LOGICAL /* NoExport */
       FIELD num_contr AS INT64 /* num_contr */
       FIELD old-person-id AS CHARACTER /* old-person-id */
       FIELD passw_card AS CHARACTER /* passw_card */
       FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
       FIELD Prim-ID AS CHARACTER /* Prim-ID */
       FIELD RegNum AS CHARACTER /* RegNum */
       FIELD RNK AS CHARACTER /* RNK */
       FIELD Soato AS CHARACTER /* Soato */
       FIELD Svid_num AS CHARACTER /* Svid_num */
       FIELD unstruc_regaddr AS CHARACTER /* unstruc_regaddr */
       FIELD Visa AS CHARACTER /* Visa */
       FIELD VisaNum AS CHARACTER /* VisaNum */
       FIELD VisaType AS CHARACTER /* VisaType */
       FIELD XSysPersonID AS CHARACTER /* XSysPersonID */
       FIELD xview-photo AS CHARACTER /* xview-photo */
       FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-person" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: F-PERS-CONT.P
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
{intrface.get exch}       /* �����㬥��� ��� ����祭�� ������᪨� ४����⮢. */
{intrface.get cust}       /* ������⥪� ��� ࠡ��� � �����⠬�.  */
{intrface.get strng}      /* �����㬥��� ��� ࠡ��� � ��ப���  */

&IF DEFINED(SESSION-REMOTE) =  0 &THEN
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
*/
&GLOBAL-DEFINE XATT-ED-ON

DEFINE VAR mIsChecking AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-person

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-person.sotoper$ tt-person.fax ~
tt-person.e-mail 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-person.sotoper$ ~
tt-person.fax tt-person.e-mail 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-person
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-person
&Scoped-define QUERY-STRING-fMain FOR EACH tt-person SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-person SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-person
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-person


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-person.sotoper$ tt-person.fax ~
tt-person.e-mail 
&Scoped-define ENABLED-TABLES tt-person
&Scoped-define FIRST-ENABLED-TABLE tt-person
&Scoped-Define ENABLED-OBJECTS mPhoneHome1 mPhoneHome2 mPhoneWork mPhoneMob 
&Scoped-Define DISPLAYED-FIELDS tt-person.sotoper$ tt-person.fax ~
tt-person.e-mail 
&Scoped-define DISPLAYED-TABLES tt-person
&Scoped-define FIRST-DISPLAYED-TABLE tt-person
&Scoped-Define DISPLAYED-OBJECTS mPhoneHome1 mPhoneHome2 mPhoneWork ~
mPhoneMob 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ ~
mPhoneMob tt-person.fax tt-person.e-mail 
&Scoped-define List-2 mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ ~
mPhoneMob tt-person.fax tt-person.e-mail 
&Scoped-define List-3 mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ ~
mPhoneMob tt-person.fax tt-person.e-mail 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mPhoneHome1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "����譨� ⥫�䮭" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPhoneHome2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "����譨� ⥫�䮭 䠪�" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPhoneMob AS CHARACTER FORMAT "X(256)":U 
     LABEL "������� ⥫�䮭" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPhoneWork AS CHARACTER FORMAT "X(256)":U 
     LABEL "����稩 ⥫�䮭" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
     &ELSE SIZE 22 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-person SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     mPhoneHome1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 23 COLON-ALIGNED
          &ELSE AT ROW 1 COL 23 COLON-ALIGNED &ENDIF
     mPhoneHome2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 23 COLON-ALIGNED
          &ELSE AT ROW 2 COL 23 COLON-ALIGNED &ENDIF
     mPhoneWork
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 23 COLON-ALIGNED
          &ELSE AT ROW 3 COL 23 COLON-ALIGNED &ENDIF
     tt-person.sotoper$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 23 COLON-ALIGNED
          &ELSE AT ROW 4 COL 23 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
     mPhoneMob
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 23 COLON-ALIGNED
          &ELSE AT ROW 5 COL 23 COLON-ALIGNED &ENDIF
     tt-person.fax
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 23 COLON-ALIGNED
          &ELSE AT ROW 6 COL 23 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
     tt-person.e-mail
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 23 COLON-ALIGNED
          &ELSE AT ROW 7 COL 23 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 1
         SIZE 49 BY 9
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-person T "?" NO-UNDO bisquit person
      ADDITIONAL-FIELDS:
          FIELD okato_302$ AS CHARACTER /* �����_302 */
          FIELD strahnomer$ AS CHARACTER /* ���卮��� */
          FIELD koliwzd$ AS INT64 /* ������ */
          FIELD sempol$ AS CHARACTER /* ������ */
          FIELD kodsubckki$ AS CHARACTER /* ����㡖��� */
          FIELD rabota_viddewat$ AS CHARACTER /* �����_������� */
          FIELD rabotanawim$ AS DATE /* ����⠍��� */
          FIELD rabotanawimkon$ AS DATE /* ����⠍������ */
          FIELD rabota_polowzenie$ AS CHARACTER /* �����_��������� */
          FIELD rabota_zanwatostw#$ AS CHARACTER /* �����_�������� */
          FIELD umer$ AS LOGICAL /* ���� */
          FIELD izmfipredfam$ AS CHARACTER /* ������।��� */
          FIELD izmfipredimwa$ AS CHARACTER /* ������।��� */
          FIELD izmfidata$ AS DATE /* �������� */
          FIELD datapropiski$ AS DATE /* ��⠏ய�᪨ */
          FIELD nbki_streg$ AS CHARACTER /* ����_�␥� */
          FIELD nbki_stprowz$ AS CHARACTER /* ����_��஦ */
          FIELD izmdokprednomer$ AS CHARACTER /* �������।����� */
          FIELD izmdokdata$ AS DATE /* ��������� */
          FIELD rabota_sferadewat$ AS CHARACTER /* �����_��ࠄ��� */
          FIELD abwawtik$ AS CHARACTER /* ���騪 */
          FIELD arhiv$ AS LOGICAL /* ��娢 */
          FIELD blok$ AS LOGICAL /* ���� */
          FIELD viddewat$ AS CHARACTER /* ������� */
          FIELD vidkli$ AS CHARACTER /* ������ */
          FIELD vidsotr$ AS CHARACTER /* ������� */
          FIELD gvk$ AS CHARACTER /* ��� */
          FIELD grawzd$ AS CHARACTER /* �ࠦ� */
          FIELD gruppakl$ AS CHARACTER /* ��㯯��� */
          FIELD datavpred$ AS CHARACTER /* ��⠂�। */
          FIELD dataokpred$ AS CHARACTER /* ��⠎��। */
          FIELD dko$ AS DECIMAL /* ��� */
          FIELD dkowe$ AS DECIMAL /* ���� */
          FIELD dolwz$ AS CHARACTER /* ���� */
          FIELD dom$ AS CHARACTER /* ��� */
          FIELD iin$ AS CHARACTER /* ��� */
          FIELD indgr$ AS CHARACTER /* ����� */
          FIELD iobss$ AS CHARACTER /* ����� */
          FIELD iogbh$ AS CHARACTER /* ����� */
          FIELD kategfiz$ AS CHARACTER /* ��⥣��� */
          FIELD klient$ AS CHARACTER /* ������ */
          FIELD klientuf$ AS LOGICAL /* �����ⓔ */
          FIELD klreestr$ AS CHARACTER /* �������� */
          FIELD koddokum$ AS CHARACTER /* ������� */
          FIELD kodklienta$ AS CHARACTER /* ��������� */
          FIELD kodreg$ AS CHARACTER /* ������ */
          FIELD kodreggni$ AS CHARACTER /* ��������� */
          FIELD kodyadresa$ AS CHARACTER /* ���뀤�� */
          FIELD kop$ AS INT64 /* ��� */
          FIELD kopf$ AS INT64 /* ���� */
          FIELD korpkl$ AS CHARACTER /* ��ொ� */
          FIELD kpp$ AS CHARACTER /* ��� */
          FIELD licenzorg$ AS CHARACTER /* ��業��� */
          FIELD mestsvedpred$ AS CHARACTER /* ���③���। */
          FIELD migrkart$ AS CHARACTER /* �������� */
          FIELD migrpravprebpo$ AS DATE /* �����ࠢ�ॡ�� */
          FIELD migrpravprebs$ AS DATE /* �����ࠢ�ॡ� */
          FIELD migrprebyvpo$ AS DATE /* �����ॡ뢏� */
          FIELD migrprebyvs$ AS DATE /* �����ॡ뢑 */
          FIELD migrcelw#vizita$ AS CHARACTER /* �������삨��� */
          FIELD nalrez$ AS LOGICAL /* ������ */
          FIELD obrbs$ AS CHARACTER /* ����� */
          FIELD obrgb$ AS CHARACTER /* ����� */
          FIELD ogrn$ AS CHARACTER /* ���� */
          FIELD okato-nalog$ AS CHARACTER /* �����-����� */
          FIELD okvwed$ AS CHARACTER /* ����� */
          FIELD orgsvedpred$ AS CHARACTER /* �࣑����। */
          FIELD osnvidydewat$ AS CHARACTER /* �ᭂ��넥�� */
          FIELD ofwsor$ AS CHARACTER /* ���� */
          FIELD ocenkariska$ AS CHARACTER /* �業����᪠ */
          FIELD predpr$ AS LOGICAL /* �।�� */
          FIELD prim$ AS CHARACTER /* �ਬ */
          FIELD prim1$ AS CHARACTER /* �ਬ1 */
          FIELD prim2$ AS CHARACTER /* �ਬ2 */
          FIELD prim3$ AS CHARACTER /* �ਬ3 */
          FIELD prim4$ AS CHARACTER /* �ਬ4 */
          FIELD prim5$ AS CHARACTER /* �ਬ5 */
          FIELD prim6$ AS CHARACTER /* �ਬ6 */
          FIELD rabota$ AS CHARACTER /* ����� */
          FIELD rabotaadr$ AS CHARACTER /* ����⠀�� */
          FIELD riskotmyv$ AS CHARACTER /* ��᪎�� */
          FIELD sotoper$ AS CHARACTER /* ��⎯�� */
          FIELD svedvygdrlica$ AS CHARACTER /* �����룄���� */
          FIELD svedregpred$ AS CHARACTER /* ��������। */
          FIELD subw%ekt$ AS CHARACTER /* ��ꥪ� */
          FIELD telefon3$ AS CHARACTER /* ����䮭3 */
          FIELD ulica$ AS CHARACTER /* ���� */
          FIELD unk$ AS DECIMAL /* ��� */
          FIELD unkg$ AS INT64 /* ���� */
          FIELD uwcdok$ AS CHARACTER /* �焮� */
          FIELD uwcdokgr$ AS CHARACTER /* �焮��� */
          FIELD uwcdokdata$ AS DATE /* �焮���� */
          FIELD fiobs$ AS CHARACTER /* ����� */
          FIELD fiobuhg$ AS CHARACTER /* ������ */
          FIELD fiogb$ AS CHARACTER /* ����� */
          FIELD fioruk$ AS CHARACTER /* ����� */
          FIELD formsobs$ AS CHARACTER /* ��଑��� */
          FIELD formsobs_118$ AS CHARACTER /* ��଑���_118 */
          FIELD wa$ AS CHARACTER /* � */
          FIELD Address1Indeks AS INT64 /* Address1Indeks */
          FIELD Address2Gorod AS CHARACTER /* Address2Gorod */
          FIELD Address3Street AS CHARACTER /* Address3Street */
          FIELD Address4Dom AS CHARACTER /* Address4Dom */
          FIELD Address5Korpus AS CHARACTER /* Address5Korpus */
          FIELD Address6Kvart AS CHARACTER /* Address6Kvart */
          FIELD Address6Rayon AS CHARACTER /* Address6Rayon */
          FIELD BirthPlace AS CHARACTER /* BirthPlace */
          FIELD branch-id AS CHARACTER /* branch-id */
          FIELD branch-list AS CHARACTER /* branch-list */
          FIELD contr_group AS CHARACTER /* contr_group */
          FIELD contr_type AS CHARACTER /* contr_type */
          FIELD country-id2 AS CHARACTER /* country-id2 */
          FIELD country-id3 AS CHARACTER /* country-id3 */
          FIELD CountryCode AS CHARACTER /* CountryCode */
          FIELD date-export AS CHARACTER /* date-export */
          FIELD diasoft-id AS CHARACTER /* diasoft-id */
          FIELD Document1Ser_Doc AS CHARACTER /* Document1Ser_Doc */
          FIELD Document2Num_Doc AS CHARACTER /* Document2Num_Doc */
          FIELD Document3Kem_Vid AS CHARACTER /* Document3Kem_Vid */
          FIELD Document4Date_vid AS DATE /* Document4Date_vid */
          FIELD e-mail AS CHARACTER /* e-mail */
          FIELD engl-name AS CHARACTER /* engl-name */
          FIELD Exam AS CHARACTER /* Exam */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD holding-id AS CHARACTER /* holding-id */
          FIELD incass AS LOGICAL /* incass */
          FIELD IndCode AS CHARACTER /* IndCode */
          FIELD Isn AS CHARACTER /* Isn */
          FIELD lat_dop AS CHARACTER /* lat_dop */
          FIELD lat_fam AS CHARACTER /* lat_fam */
          FIELD lat_f_n AS CHARACTER /* lat_f_n */
          FIELD lat_name AS CHARACTER /* lat_name */
          FIELD lat_otch AS CHARACTER /* lat_otch */
          FIELD lat_titul AS CHARACTER /* lat_titul */
          FIELD LegTerr AS CHARACTER /* LegTerr */
          FIELD lic-sec AS CHARACTER /* lic-sec */
          FIELD Lic_num AS CHARACTER /* Lic_num */
          FIELD LocCustType AS CHARACTER /* LocCustType */
          FIELD mess AS CHARACTER /* mess */
          FIELD NACE AS CHARACTER /* NACE */
          FIELD Netting AS LOGICAL /* Netting */
          FIELD New$$ AS CHARACTER /* New */
          FIELD New1 AS CHARACTER /* New1 */
          FIELD NoExport AS LOGICAL /* NoExport */
          FIELD num_contr AS INT64 /* num_contr */
          FIELD old-person-id AS CHARACTER /* old-person-id */
          FIELD passw_card AS CHARACTER /* passw_card */
          FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
          FIELD Prim-ID AS CHARACTER /* Prim-ID */
          FIELD RegNum AS CHARACTER /* RegNum */
          FIELD RNK AS CHARACTER /* RNK */
          FIELD Soato AS CHARACTER /* Soato */
          FIELD Svid_num AS CHARACTER /* Svid_num */
          FIELD unstruc_regaddr AS CHARACTER /* unstruc_regaddr */
          FIELD Visa AS CHARACTER /* Visa */
          FIELD VisaNum AS CHARACTER /* VisaNum */
          FIELD VisaType AS CHARACTER /* VisaType */
          FIELD XSysPersonID AS CHARACTER /* XSysPersonID */
          FIELD xview-photo AS CHARACTER /* xview-photo */
          FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-person" "" }
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW TERMINAL-SIMULATION ASSIGN
         HIDDEN             = YES
         TITLE              = " <insert window title>"
         HEIGHT             = 25.29
         WIDTH              = 83
         MAX-HEIGHT         = 25.29
         MAX-WIDTH          = 83
         VIRTUAL-HEIGHT     = 25.29
         VIRTUAL-WIDTH      = 83
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
/* SETTINGS FOR FILL-IN tt-person.e-mail IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.fax IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneHome1 IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneHome2 IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneMob IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mPhoneWork IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-person.sotoper$ IN FRAME fMain
   1 2 3                                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-person"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */
&IF DEFINED(SESSION-REMOTE) =  0 &THEN
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

   /* ��. ��ࠬ���� */
   IF NUM-ENTRIES(iInstanceList,{&PARAM_DELIM}) > 1
      THEN mIsChecking = LOGICAL(ENTRY(2, iInstanceList,{&PARAM_DELIM})).

   /* Commented by KSV: ���樠������ ��⥬��� ᮮ�饭�� */
   RUN Init-SysMes("","","").

   RUN MainAction NO-ERROR.
   IF ERROR-STATUS:ERROR
      THEN LEAVE MAIN-BLOCK.

   IF mIsChecking THEN APPLY "GO" TO FRAME {&MAIN-FRAME}.
   IF ERROR-STATUS:ERROR
      THEN mIsChecking = NO.
   IF NOT THIS-PROCEDURE:PERSISTENT AND NOT mIsChecking THEN
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
  DISPLAY mPhoneHome1 mPhoneHome2 mPhoneWork mPhoneMob 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-person THEN 
    DISPLAY tt-person.sotoper$ tt-person.fax tt-person.e-mail 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE mPhoneHome1 mPhoneHome2 mPhoneWork tt-person.sotoper$ mPhoneMob 
         tt-person.fax tt-person.e-mail 
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
   mSwitchF9 = NO.
   RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalPutTitle TERMINAL-SIMULATION 
PROCEDURE LocalPutTitle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
      mBT_Title         = "[ �������� �� - " + LC(ENTRY(iMode,{&MOD_LIST})) + " ]"
      FRAME fMain:TITLE = mBT_Title
   .
   RETURN ERROR.
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
   DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN
         tt-person.phone[1] = "" WHEN tt-person.phone[1] = ?
         tt-person.phone[2] = "" WHEN tt-person.phone[2] = ?
         tt-person.phone[1] = SetEntries(1,tt-person.phone[1],",",TRIM(REPLACE(mPhoneHome1:SCREEN-VALUE,","," ")))
         tt-person.phone[1] = SetEntries(2,tt-person.phone[1],",",TRIM(REPLACE(mPhoneHome2:SCREEN-VALUE,","," ")))
         tt-person.phone[2] = SetEntries(1,tt-person.phone[2],",",TRIM(REPLACE(mPhoneWork:SCREEN-VALUE,","," ")))
         tt-person.phone[2] = SetEntries(2,tt-person.phone[2],",",TRIM(REPLACE(mPhoneMob:SCREEN-VALUE,","," ")))
      .
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
   DEFINE VAR vTestPhone AS CHAR          NO-UNDO.
   DEFINE VAR vPrefixes  AS CHARACTER     NO-UNDO.
   DO WITH FRAME {&MAIN-FRAME}:
      IF TRIM(mPhoneHome1:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "����� ⥫�䮭� ����� ᮤ�ঠ�� ⮫쪮 ���� � ᨬ����: - ( )").
         APPLY "ENTRY" TO mPhoneHome1.
         RETURN ERROR.
      END.
      IF TRIM(mPhoneHome2:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "����� ⥫�䮭� ����� ᮤ�ঠ�� ⮫쪮 ���� � ᨬ����: - ( )").
         APPLY "ENTRY" TO mPhoneHome2.
         RETURN ERROR.
      END.
      IF TRIM(mPhoneWork:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "����� ⥫�䮭� ����� ᮤ�ঠ�� ⮫쪮 ���� � ᨬ����: - ( )").
         APPLY "ENTRY" TO mPhoneWork.
         RETURN ERROR.
      END.
      IF TRIM(mPhoneMob:SCREEN-VALUE,"1234567890-()") <> "" THEN DO:
         RUN Fill-SysMes ("", "", "0", "����� ⥫�䮭� ����� ᮤ�ঠ�� ⮫쪮 ���� � ᨬ����: - ( )").
         APPLY "ENTRY" TO mPhoneMob.
         RETURN ERROR.
      END.
      vTestPhone = mPhoneHome1:SCREEN-VALUE + mPhoneHome2:SCREEN-VALUE + mPhoneWork:SCREEN-VALUE + mPhoneMob:SCREEN-VALUE.
      RUN CheckFullFieldValue (iClass, "phone", iSurrogate, TRIM(vTestPhone)) NO-ERROR.
      IF {assigned RETURN-VALUE} THEN DO:
         IF INDEX(RETURN-VALUE,"��易�") > 0
            THEN RUN Fill-SysMes ("", "", "0","����室��� ��������� ���� �� ⥫�䮭�� (����譨�, ࠡ�稩 ��� �������)").
         ELSE RUN Fill-SysMes ("", "", "0", RETURN-VALUE).
         RETURN ERROR.
      END.

      IF FGetSetting("�஢�ન","�஢����䮭","") = "��" THEN
      IF vTestPhone = "" THEN
         RUN Fill-SysMes ("", "", "0","����䮭 ������ �� 㪠���").


      IF     {assigned "mPhoneMob:SCREEN-VALUE"} 
         AND {assigned "tt-person.sotoper$:SCREEN-VALUE"}
         AND tt-person.sotoper$:SCREEN-VALUE <> "?"
         AND SUBSTRING(mPhoneMob:SCREEN-VALUE, 1, 1) <> "7" THEN
      DO:      
         RUN Fill-SysMes IN h_tmess ("",
                                     "",
                                     "0",
                                     '����� �����쭮�� ⥫�䮭� ������ ��稭����� � ���� "7".').
         RETURN ERROR.
      END.
      IF {assigned "tt-person.sotoper$:SCREEN-VALUE"} AND
         tt-person.sotoper$:SCREEN-VALUE <> "?" THEN
      DO:
         FOR EACH code WHERE CODE.CLASS   =  "��⎯��"
                         AND CODE.PARENT  =  tt-person.sotoper$:SCREEN-VALUE
            NO-LOCK:
            {additem.i vPrefixes CODE.CODE}
         END.
         
         IF NOT CAN-DO(vPrefixes, SUBSTRING(mPhoneMob:SCREEN-VALUE, 2, 3)) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("",
                                        "",
                                        "0",
                                        '��� �����쭮�� ������ <' 
                                           + SUBSTRING(mPhoneMob:SCREEN-VALUE, 2, 3) 
                                           + '> ��������� � ᯨ᪥ ࠧ�襭��� ��� ������ <' 
                                           + tt-person.sotoper$:SCREEN-VALUE + '>.').
            RETURN ERROR.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainAction TERMINAL-SIMULATION 
PROCEDURE MainAction PRIVATE :
/* Commented by KSV: ���४��㥬 ���⨪����� ������ �३�� */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.

   /* Commented by KSV: ��⠥� ����� */
   RUN GetObject NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN RETURN ERROR.
   IF tt-person.sotoper$ = ? THEN 
      tt-person.sotoper$ = "".
   IF tt-person.fax = ? THEN 
      tt-person.fax = "".
   IF tt-person.e-mail = ? THEN 
      tt-person.e-mail = "".

   /* ������塞 COMBO-BOX'� ����묨 �� ����奬� */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* ���ᢥ⪠ ����� �� LIST-5 (����ந�� ��� ᥡ� )*/
   RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                    REPLACE("{&LIST-5}"," ",","),
                    "bright-green").

   /* Commented by KSV: �����뢠�� �࠭��� ��� */
   STATUS DEFAULT "".
&IF DEFINED(SESSION-REMOTE) =  0 &THEN
   RUN enable_UI.
&ENDIF
   /* Commented by KSV: ���뢠�� � ����, ����� ࠧ�襭� ��������
   ** � ����ᨬ��� �� ०��� ������ */
   RUN EnableDisable.
&IF DEFINED(SESSION-REMOTE) &THEN
   RETURN ERROR.
&ENDIF

   /* Commented by KSV: ���㥬 ࠧ����⥫�. �������⥫� �������� ��� FILL-IN
   ** � �����䨪��஬ SEPARATOR# � ��ਡ�⮬ VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").

   RETURN.
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
   IF AVAIL tt-person THEN DO:
      mPhoneHome1 = GetEntries(1, tt-person.Phone[1], ",", "").
      mPhoneHome2 = GetEntries(2, tt-person.Phone[1], ",", "").
      mPhoneWork  = GetEntries(1, tt-person.Phone[2], ",", "").
      mPhoneMob   = GetEntries(2, tt-person.Phone[2], ",", "").
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

