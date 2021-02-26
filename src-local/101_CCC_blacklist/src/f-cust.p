&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-cust-corp NO-UNDO LIKE cust-corp
       FIELD regorgan$ AS CHARACTER /* ����࣠� */
       FIELD reorginwaz$ AS CHARACTER /* ���ࣈ��� */
       FIELD reorgogrn$ AS CHARACTER /* ���࣎��� */
       FIELD reorginn$ AS CHARACTER /* ���ࣈ�� */
       FIELD BQMail AS CHARACTER /* BQMail */
       FIELD BQSms AS CHARACTER /* BQSms */
       FIELD abwawtik$ AS CHARACTER /* ���騪 */
       FIELD adresp$ AS CHARACTER /* ���� */
       FIELD bankwemitent$ AS CHARACTER /* �������⥭� */
       FIELD bki_naimrf$ AS CHARACTER /* ���_������ */
       FIELD bki_naimwazyk$ AS CHARACTER /* ���_������� */
       FIELD blok$ AS LOGICAL /* ���� */
       FIELD viddewat$ AS CHARACTER /* ������� */
       FIELD vidkli$ AS CHARACTER /* ������ */
       FIELD gvk$ AS CHARACTER /* ��� */
       FIELD grawzd$ AS CHARACTER /* �ࠦ� */
       FIELD gruppakl$ AS CHARACTER /* ��㯯��� */
       FIELD dataogrn$ AS DATE /* ��⠎��� */
       FIELD datareg$ AS DATE /* ��⠐�� */
       FIELD okato_302$ AS CHARACTER /* �����_302 */
       FIELD dko$ AS DECIMAL /* ��� */
       FIELD dkowe$ AS DECIMAL /* ���� */
       FIELD dolruk$ AS CHARACTER /* ����� */
       FIELD iin$ AS CHARACTER /* ��� */
       FIELD istoriwakl$ AS CHARACTER /* ���� */
       FIELD kategklient$ AS CHARACTER /* ��⥣������ */
       FIELD klient$ AS CHARACTER /* ������ */
       FIELD klientuf$ AS LOGICAL /* �����ⓔ */
       FIELD koddokum$ AS CHARACTER /* ������� */
       FIELD kodklienta$ AS CHARACTER /* ��������� */
       FIELD kodpriwcposuwcet$ AS CHARACTER /* �����珮��� */
       FIELD kodreg$ AS CHARACTER /* ������ */
       FIELD kodreggni$ AS CHARACTER /* ��������� */
       FIELD kodsubki$ AS CHARACTER /* ����㡊� */
       FIELD kodyadresa$ AS CHARACTER /* ���뀤�� */
       FIELD koldir$ AS INT64 /* ������ */
       FIELD kolrab$ AS INT64 /* ������ */
       FIELD kop$ AS INT64 /* ��� */
       FIELD kopf$ AS INT64 /* ���� */
       FIELD korpkl$ AS CHARACTER /* ��ொ� */
       FIELD kpp$ AS CHARACTER /* ��� */
       FIELD licvydnaim$ AS CHARACTER /* ���뤍��� */
       FIELD licdatan$ AS CHARACTER /* ��愠⠍ */
       FIELD licdatao$ AS CHARACTER /* ��愠⠎ */
       FIELD licenzorg$ AS CHARACTER /* ��業��� */
       FIELD lictip$ AS CHARACTER /* ��撨� */
       FIELD materinkomp$ AS CHARACTER /* ����������� */
       FIELD migrkart$ AS CHARACTER /* �������� */
       FIELD migrpravprebpo$ AS DATE /* �����ࠢ�ॡ�� */
       FIELD migrpravprebs$ AS DATE /* �����ࠢ�ॡ� */
       FIELD migrprebyvpo$ AS DATE /* �����ॡ뢏� */
       FIELD migrprebyvs$ AS DATE /* �����ॡ뢑 */
       FIELD migrcelw#vizita$ AS CHARACTER /* �������삨��� */
       FIELD nbki_godob$ AS CHARACTER /* ����_����� */
       FIELD nbki_stpowct$ AS CHARACTER /* ����_�⏮�� */
       FIELD nbki_streg$ AS CHARACTER /* ����_�␥� */
       FIELD nomdop$ AS CHARACTER /* ������ */
       FIELD nomerpf$ AS CHARACTER /* ������� */
       FIELD obosobpodr$ AS CHARACTER /* ���ᮡ���� */
       FIELD ogrn$ AS CHARACTER /* ���� */
       FIELD okato-nalog$ AS CHARACTER /* �����-����� */
       FIELD okvwed$ AS CHARACTER /* ����� */
       FIELD okogu$ AS INT64 /* ����� */
       FIELD okopf$ AS CHARACTER /* ����� */
       FIELD orguprav$ AS CHARACTER /* �࣓�ࠢ */
       FIELD osnvidydewat$ AS CHARACTER /* �ᭂ��넥�� */
       FIELD osnova$ AS CHARACTER /* �᭮�� */
       FIELD ofwsor$ AS CHARACTER /* ���� */
       FIELD ocenkariska$ AS CHARACTER /* �業����᪠ */
       FIELD pokrytie$ AS LOGICAL /* �����⨥ */
       FIELD postkontrag$ AS CHARACTER /* ���⊮��ࠣ */
       FIELD predpr$ AS LOGICAL /* �।�� */
       FIELD prim$ AS CHARACTER /* �ਬ */
       FIELD prim1$ AS CHARACTER /* �ਬ1 */
       FIELD prim2$ AS CHARACTER /* �ਬ2 */
       FIELD prim3$ AS CHARACTER /* �ਬ3 */
       FIELD prim4$ AS CHARACTER /* �ਬ4 */
       FIELD prim5$ AS CHARACTER /* �ਬ5 */
       FIELD prim6$ AS CHARACTER /* �ਬ6 */
       FIELD prisutorguprav$ AS CHARACTER /* �����࣓�ࠢ */
       FIELD priwcvnes$ AS CHARACTER /* ��炭�� */
       FIELD reorgdata$ AS DATE /* ���ࣄ�� */
       FIELD reorgegrn$ AS CHARACTER /* ���ࣅ��� */
       FIELD reorgkpp$ AS CHARACTER /* ���࣊�� */
       FIELD reorgokato$ AS CHARACTER /* ���࣎���� */
       FIELD reorgokpo$ AS CHARACTER /* ���࣎��� */
       FIELD reorgpolnoe$ AS CHARACTER /* ���࣏����� */
       FIELD reorgsokr$ AS CHARACTER /* ���࣑��� */
       FIELD reorgfirm$ AS CHARACTER /* ���ࣔ�� */
       FIELD reorgwazykrf$ AS CHARACTER /* ���ࣟ�몐� */
       FIELD riskotmyv$ AS CHARACTER /* ��᪎�� */
       FIELD svedvygdrlica$ AS CHARACTER /* �����룄���� */
       FIELD statusdata$ AS DATE /* ����ᄠ� */
       FIELD statuspredpr$ AS CHARACTER /* �����।�� */
       FIELD struktorg$ AS CHARACTER /* ������ */
       FIELD subw%ekt$ AS CHARACTER /* ��ꥪ� */
       FIELD tipkl$ AS CHARACTER /* ����� */
       FIELD unikkodadresa$ AS CHARACTER /* ����������� */
       FIELD unk$ AS DECIMAL /* ��� */
       FIELD unkg$ AS INT64 /* ���� */
       FIELD ustavkap$ AS CHARACTER /* ��⠢��� */
       FIELD uwcdok$ AS CHARACTER /* �焮� */
       FIELD uwcdokgr$ AS CHARACTER /* �焮��� */
       FIELD uwcdokdata$ AS DATE /* �焮���� */
       FIELD uwcredorg$ AS CHARACTER /* ��।�� */
       FIELD fiobuhg$ AS CHARACTER /* ������ */
       FIELD fioruk$ AS CHARACTER /* ����� */
       FIELD formsobs$ AS CHARACTER /* ��଑��� */
       FIELD formsobs_118$ AS CHARACTER /* ��଑���_118 */
       FIELD wekonsekt$ AS CHARACTER /* �������� */
       FIELD Address1Indeks AS INT64 /* Address1Indeks */
       FIELD BirthDay AS DATE /* BirthDay */
       FIELD BirthPlace AS CHARACTER /* BirthPlace */
       FIELD branch-id AS CHARACTER /* branch-id */
       FIELD branch-list AS CHARACTER /* branch-list */
       FIELD brand-name AS CHARACTER /* brand-name */
       FIELD CMSCUR AS DECIMAL /* CMSCUR */
       FIELD contr_group AS CHARACTER /* contr_group */
       FIELD cont_type AS CHARACTER /* cont_type */
       FIELD country-id2 AS CHARACTER /* country-id2 */
       FIELD country-id3 AS CHARACTER /* country-id3 */
       FIELD CountryCode AS CHARACTER /* CountryCode */
       FIELD CRSCM AS CHARACTER /* CRSCM */
       FIELD date-export AS CHARACTER /* date-export */
       FIELD diasoft-id AS CHARACTER /* diasoft-id */
       FIELD document AS CHARACTER /* document */
       FIELD document-id AS CHARACTER /* document-id */
       FIELD Document4Date_vid AS DATE /* Document4Date_vid */
       FIELD e-mail AS CHARACTER /* e-mail */
       FIELD engl-name AS CHARACTER /* engl-name */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD holding-id AS CHARACTER /* holding-id */
       FIELD iey AS CHARACTER /* iey */
       FIELD IndCode AS CHARACTER /* IndCode */
       FIELD Isn AS CHARACTER /* Isn */
       FIELD issue AS CHARACTER /* issue */
       FIELD LegTerr AS CHARACTER /* LegTerr */
       FIELD lic-sec AS CHARACTER /* lic-sec */
       FIELD LocCustType AS CHARACTER /* LocCustType */
       FIELD mess AS CHARACTER /* mess */
       FIELD NACE AS CHARACTER /* NACE */
       FIELD Netting AS LOGICAL /* Netting */
       FIELD NoExport AS LOGICAL /* NoExport */
       FIELD num_contr AS INT64 /* num_contr */
       FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
       FIELD Prim-ID AS CHARACTER /* Prim-ID */
       FIELD RegDate AS CHARACTER /* RegDate */
       FIELD RegNum AS CHARACTER /* RegNum */
       FIELD RegPlace AS CHARACTER /* RegPlace */
       FIELD RNK AS CHARACTER /* RNK */
       FIELD Soato AS CHARACTER /* Soato */
       FIELD SphereID AS CHARACTER /* SphereID */
       FIELD tel AS CHARACTER /* tel */
       FIELD Telex AS CHARACTER /* Telex */
       FIELD Visa AS CHARACTER /* Visa */
       FIELD VisaNum AS CHARACTER /* VisaNum */
       FIELD VisaType AS CHARACTER /* VisaType */
       FIELD lat_name AS CHARACTER /* lat_name */
       FIELD wembnazv$ AS CHARACTER /* ������� */
       FIELD country-id4 AS CHARACTER /* country-id4 */
       FIELD website AS CHARACTER /* website */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-cust-corp" "" "NO-INDEX"}
       .
DEFINE TEMP-TABLE tt-p-cust-adr NO-UNDO LIKE cust-ident
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
       {ln-tthdl.i "tt-p-cust-adr" "p-cust-adr" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: F-CUST.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 05.04.2006 12:50 ILVI (40632)
     Modified: 29.11.2010 18:46 Kraa (0115851) ��࠭� ����������� ���⪨ ���� ������� ��� �� f8
     Modified: 08/12/2010 kraa (0111435) ��ࠡ�⠭� ��楤�� postSetObject.
     Modified: 15/11/2012 ccc  (0133400) ��ࠡ�⠭� ��ࠡ�⪠ ���
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
&IF DEFINED(SESSION-REMOTE) =  0 &THEN
CREATE WIDGET-POOL.
&ENDIF
/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE MAIN-FRAME fMain
/* �ਣ���� ��� ࠡ��� � ��� */

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

{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get op}       /* ������⥪� ��� ࠡ��� � ���㬥�⠬�. */
{intrface.get exch}     /*  */
{egr-upd.def NEW}       /* ��� ࠡ��� � �ࢨᮬ ���. */
{wordwrap.def}
{fms-chkdoc.i &nofmsprint=yes}
{stoplist.fun}

DEFINE VARIABLE mType        AS CHARACTER NO-UNDO.  /* ⨯ ����⨨䪠�� ����� */
DEFINE VARIABLE mTempVal     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFlagUnk     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mTmpUnk      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctKey     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdrLogic    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mFrmRole     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClient      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mScreenValue AS CHARACTER NO-UNDO. /* ���祭�� �� �࠭�. */
DEFINE VARIABLE vAdrCntry    AS CHARACTER NO-UNDO. /* ���� ��࠭� */
DEFINE VARIABLE mAdrType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAdrCntXattr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUniqCodAdr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLastOkOGRN  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUpdEgr      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mTypeEgr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mErrEgr      AS LOGICAL   NO-UNDO.


DEFINE VARIABLE mProvINNKPP  AS CHARACTER NO-UNDO.

mProvINNKPP = FGetSetting("�஢�ન","����஢������","").


DEFINE BUFFER bcident FOR cust-ident. /* ���������� ����. */

{cust-adr.obj
   &def-vars-gni = YES
}

&GLOBAL-DEFINE FlowFields  tt-cust-corp.unk$
/* �஢����� �ࠢ� �� ��㯯� ४����⮢ */
&GLOBAL-DEFINE CHECK-XATTR-GROUP-PERMISSION

RUN GetTypeMainAdr IN h_cust('�',OUTPUT mAdrType,OUTPUT mAdrCntXattr).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cust-corp

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-cust-corp.unk$ ~
tt-cust-corp.cust-id tt-cust-corp.date-in tt-cust-corp.date-out ~
tt-cust-corp.cust-stat tt-cust-corp.name-corp tt-cust-corp.name-short ~
tt-cust-corp.country-id tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ ~
tt-cust-corp.kodreg$ tt-cust-corp.addr-of-low[2] tt-cust-corp.tel ~
tt-cust-corp.fax tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-cust-corp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-cust-corp
&Scoped-define QUERY-STRING-fMain FOR EACH tt-cust-corp SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-cust-corp SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-cust-corp
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-cust-corp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define ENABLED-TABLES tt-cust-corp
&Scoped-define FIRST-ENABLED-TABLE tt-cust-corp
&Scoped-Define ENABLED-OBJECTS mBankClient separator1 vOblChar vGorChar ~
vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar vAdrIndInt ~
separator2 mFormSobs mWebsite 
&Scoped-Define DISPLAYED-FIELDS tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website ~
tt-cust-corp.benacct 
&Scoped-define DISPLAYED-TABLES tt-cust-corp
&Scoped-define FIRST-DISPLAYED-TABLE tt-cust-corp
&Scoped-Define DISPLAYED-OBJECTS mBankClient separator1 vOblChar vGorChar ~
vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar vAdrIndInt ~
separator2 mFormSobs mWebsite 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id tt-cust-corp.date-in ~
tt-cust-corp.cust-stat tt-cust-corp.name-corp tt-cust-corp.name-short ~
tt-cust-corp.country-id tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.benacct 
&Scoped-define List-2 mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id tt-cust-corp.date-in ~
tt-cust-corp.date-out tt-cust-corp.cust-stat tt-cust-corp.name-corp ~
tt-cust-corp.name-short tt-cust-corp.country-id tt-cust-corp.addr-of-low[1] ~
tt-cust-corp.kodreggni$ tt-cust-corp.addr-of-low[2] tt-cust-corp.tel ~
tt-cust-corp.fax tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type ~
tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.benacct ~
tt-cust-corp.website
&Scoped-define List-3 mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id ~
tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat ~
tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id ~
tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ ~
tt-cust-corp.addr-of-low[2] tt-cust-corp.tel tt-cust-corp.fax ~
tt-cust-corp.tax-insp tt-cust-corp.inn tt-cust-corp.ogrn$ ~
tt-cust-corp.okvwed$ tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ ~
tt-cust-corp.formsobs$ mFormSobs tt-cust-corp.subw%ekt$ ~
tt-cust-corp.bank-code-type tt-cust-corp.bank-code tt-cust-corp.corr-acct ~
tt-cust-corp.benacct tt-cust-corp.website
&Scoped-define List-4 tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreg$ ~
tt-cust-corp.addr-of-low[2] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mBankClient AS LOGICAL FORMAT "������/���":U INITIAL NO 
     LABEL "�⭮襭�� � �����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mFormSobs AS CHARACTER FORMAT "X(256)":U 
     LABEL "��ଠ ᮡ�⢥�����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
     &ELSE SIZE 27 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mWebsite AS CHARACTER FORMAT "X(256)":U 
     LABEL "���� ��������" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 17 BY 1
     &ELSE SIZE 18 BY 1.01 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vAdrIndInt AS INT64 FORMAT "999999":U INITIAL ? 
     LABEL "������" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vDomChar AS CHARACTER FORMAT "X(8)":U 
     LABEL "���" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vGorChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "��த" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vKorpChar AS CHARACTER FORMAT "X(10)":U 
     LABEL "�����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vKvChar AS CHARACTER FORMAT "X(30)":U 
     LABEL "���" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vOblChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "�����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vPunktChar AS CHARACTER FORMAT "X(35)":U 
     LABEL "���.�㭪�" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vStrChar AS CHARACTER FORMAT "X(256)":U 
     LABEL "���." 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
     &ELSE SIZE 5 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vUlChar AS CHARACTER FORMAT "X(61)":U 
     LABEL "����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 62 BY 1
     &ELSE SIZE 62 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-cust-corp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     mBankClient
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 20 COLON-ALIGNED
          &ELSE AT ROW 1 COL 20 COLON-ALIGNED &ENDIF HELP
          "������� �� ��ꥪ� �����⮬ �����."
     tt-cust-corp.unk$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 49 COLON-ALIGNED
          &ELSE AT ROW 1 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-cust-corp.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 49 COLON-ALIGNED
          &ELSE AT ROW 1 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-cust-corp.date-in
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 15 COLON-ALIGNED
          &ELSE AT ROW 2 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-cust-corp.date-out
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 49 COLON-ALIGNED
          &ELSE AT ROW 2 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-cust-corp.cust-stat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 15 COLON-ALIGNED
          &ELSE AT ROW 3 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 37 BY 1
          &ELSE SIZE 37 BY 1 &ENDIF
     tt-cust-corp.name-corp
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 15 COLON-ALIGNED
          &ELSE AT ROW 4 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
          &ELSE SIZE 60 BY 1 &ENDIF
     tt-cust-corp.name-short
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 15 COLON-ALIGNED
          &ELSE AT ROW 5 COL 15 COLON-ALIGNED &ENDIF
          LABEL "��⪮� ������"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
          &ELSE SIZE 60 BY 1 &ENDIF
     tt-cust-corp.country-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 15 COLON-ALIGNED
          &ELSE AT ROW 6 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF NO-LABEL
     tt-cust-corp.addr-of-low[1]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 8 COLON-ALIGNED
          &ELSE AT ROW 8 COL 8 COLON-ALIGNED &ENDIF HELP
          "������,�����,��த,���.�㭪�,��.,���,���.,��."
          LABEL "����" FORMAT "x(300)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 69 BY 1
          &ELSE SIZE 69 BY 1 &ENDIF
     tt-cust-corp.kodreggni$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 11 COLON-ALIGNED
          &ELSE AT ROW 8 COL 11 COLON-ALIGNED &ENDIF HELP
          "��� ॣ���� ���"
          LABEL "������ ���" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-cust-corp.kodreg$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 27 COLON-ALIGNED
          &ELSE AT ROW 8 COL 27 COLON-ALIGNED &ENDIF
          LABEL "������"
          VIEW-AS FILL-IN
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     vOblChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 49 COLON-ALIGNED
          &ELSE AT ROW 8 COL 49 COLON-ALIGNED &ENDIF HELP
          "�����"
     tt-cust-corp.addr-of-low[2]
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 8 COLON-ALIGNED
          &ELSE AT ROW 9 COL 8 COLON-ALIGNED &ENDIF HELP
          "������,�����,��த,���.�㭪�,��.,���,���.,��."
          LABEL ""
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 55 BY 1
          &ELSE SIZE 55 BY 1 &ENDIF
     vGorChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 11 COLON-ALIGNED
          &ELSE AT ROW 9 COL 11 COLON-ALIGNED &ENDIF HELP
          "��த"
     vPunktChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 49 COLON-ALIGNED
          &ELSE AT ROW 9 COL 49 COLON-ALIGNED &ENDIF HELP
          "��ᥫ���� �㭪�"
     vUlChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 11 COLON-ALIGNED
          &ELSE AT ROW 10 COL 11 COLON-ALIGNED &ENDIF HELP
          "����"
     vDomChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 11 COLON-ALIGNED
          &ELSE AT ROW 11 COL 11 COLON-ALIGNED &ENDIF HELP
          "���"
     vStrChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 23 COLON-ALIGNED
          &ELSE AT ROW 11 COL 23 COLON-ALIGNED &ENDIF
     vKorpChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 37 COLON-ALIGNED
          &ELSE AT ROW 11 COL 37 COLON-ALIGNED &ENDIF HELP
          "�����"
     vKvChar
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 49 COLON-ALIGNED
          &ELSE AT ROW 11 COL 49 COLON-ALIGNED &ENDIF HELP
          "���(������)"
     vAdrIndInt
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 67 COLON-ALIGNED
          &ELSE AT ROW 11 COL 67 COLON-ALIGNED &ENDIF HELP
          "NORMAL"
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 1
          &ELSE AT ROW 12 COL 1 &ENDIF NO-LABEL
     tt-cust-corp.tel
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 15 COLON-ALIGNED
          &ELSE AT ROW 13 COL 15 COLON-ALIGNED &ENDIF
          LABEL "����䮭"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-cust-corp.fax
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 49 COLON-ALIGNED
          &ELSE AT ROW 13 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
   tt-cust-corp.tax-insp
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 15 COLON-ALIGNED
          &ELSE AT ROW 14 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-cust-corp.inn
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 49 COLON-ALIGNED
          &ELSE AT ROW 14 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
     tt-cust-corp.ogrn$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 15 COLON-ALIGNED
          &ELSE AT ROW 15 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     tt-cust-corp.okvwed$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 49 COLON-ALIGNED
          &ELSE AT ROW 15 COL 49 COLON-ALIGNED &ENDIF
          LABEL "�����"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-corp.okpo
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 15 COLON-ALIGNED
          &ELSE AT ROW 16 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
          &ELSE SIZE 16 BY 1 &ENDIF
     /*tt-cust-corp.okonx
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 49 COLON-ALIGNED
          &ELSE AT ROW 16 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
         &ELSE SIZE 16 BY 1 &ENDIF*/
     tt-cust-corp.kpp$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 15 COLON-ALIGNED
          &ELSE AT ROW 17 COL 15 COLON-ALIGNED &ENDIF
          LABEL "���"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-corp.formsobs$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 49 COLON-ALIGNED
          &ELSE AT ROW 17 COL 49 COLON-ALIGNED &ENDIF
          LABEL "��� ��଑���"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     mFormSobs
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 49 COLON-ALIGNED
          &ELSE AT ROW 17 COL 49 COLON-ALIGNED &ENDIF
     tt-cust-corp.subw%ekt$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 15 COLON-ALIGNED
          &ELSE AT ROW 18 COL 15 COLON-ALIGNED &ENDIF
          LABEL "��ꥪ�" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-cust-corp.bank-code-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 49 COLON-ALIGNED
          &ELSE AT ROW 18 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     tt-cust-corp.bank-code
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 15 COLON-ALIGNED
          &ELSE AT ROW 19 COL 15 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-cust-corp.corr-acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 49 COLON-ALIGNED
          &ELSE AT ROW 19 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
     tt-cust-corp.website
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 15 COLON-ALIGNED
          &ELSE AT ROW 20 COL 15 COLON-ALIGNED &ENDIF
          LABEL "Web-ᠩ�"
          FORMAT "X(256)":U
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 18 BY 1
          &ELSE SIZE 18 BY 1 &ENDIF
     tt-cust-corp.benacct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 49 COLON-ALIGNED
          &ELSE AT ROW 20 COL 49 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY 1 &ENDIF
     mWebsite
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 21 COL 15 COLON-ALIGNED
          &ELSE AT ROW 21 COL 15 COLON-ALIGNED &ENDIF
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
      TABLE: tt-cust-corp T "?" NO-UNDO bisquit cust-corp
      ADDITIONAL-FIELDS:
          FIELD regorgan$ AS CHARACTER /* ����࣠� */
          FIELD reorginwaz$ AS CHARACTER /* ���ࣈ��� */
          FIELD reorgogrn$ AS CHARACTER /* ���࣎��� */
          FIELD reorginn$ AS CHARACTER /* ���ࣈ�� */
          FIELD BQMail AS CHARACTER /* BQMail */
          FIELD BQSms AS CHARACTER /* BQSms */
          FIELD abwawtik$ AS CHARACTER /* ���騪 */
          FIELD adresp$ AS CHARACTER /* ���� */
          FIELD bankwemitent$ AS CHARACTER /* �������⥭� */
          FIELD bki_naimrf$ AS CHARACTER /* ���_������ */
          FIELD bki_naimwazyk$ AS CHARACTER /* ���_������� */
          FIELD blok$ AS LOGICAL /* ���� */
          FIELD viddewat$ AS CHARACTER /* ������� */
          FIELD vidkli$ AS CHARACTER /* ������ */
          FIELD gvk$ AS CHARACTER /* ��� */
          FIELD grawzd$ AS CHARACTER /* �ࠦ� */
          FIELD gruppakl$ AS CHARACTER /* ��㯯��� */
          FIELD dataogrn$ AS DATE /* ��⠎��� */
          FIELD datareg$ AS DATE /* ��⠐�� */
          FIELD okato_302$ AS CHARACTER /* �����_302 */
          FIELD dko$ AS DECIMAL /* ��� */
          FIELD dkowe$ AS DECIMAL /* ���� */
          FIELD dolruk$ AS CHARACTER /* ����� */
          FIELD iin$ AS CHARACTER /* ��� */
          FIELD istoriwakl$ AS CHARACTER /* ���� */
          FIELD kategklient$ AS CHARACTER /* ��⥣������ */
          FIELD klient$ AS CHARACTER /* ������ */
          FIELD klientuf$ AS LOGICAL /* �����ⓔ */
          FIELD koddokum$ AS CHARACTER /* ������� */
          FIELD kodklienta$ AS CHARACTER /* ��������� */
          FIELD kodpriwcposuwcet$ AS CHARACTER /* �����珮��� */
          FIELD kodreg$ AS CHARACTER /* ������ */
          FIELD kodreggni$ AS CHARACTER /* ��������� */
          FIELD kodsubki$ AS CHARACTER /* ����㡊� */
          FIELD kodyadresa$ AS CHARACTER /* ���뀤�� */
          FIELD koldir$ AS INT64 /* ������ */
          FIELD kolrab$ AS INT64 /* ������ */
          FIELD kop$ AS INT64 /* ��� */
          FIELD kopf$ AS INT64 /* ���� */
          FIELD korpkl$ AS CHARACTER /* ��ொ� */
          FIELD kpp$ AS CHARACTER /* ��� */
          FIELD licvydnaim$ AS CHARACTER /* ���뤍��� */
          FIELD licdatan$ AS CHARACTER /* ��愠⠍ */
          FIELD licdatao$ AS CHARACTER /* ��愠⠎ */
          FIELD licenzorg$ AS CHARACTER /* ��業��� */
          FIELD lictip$ AS CHARACTER /* ��撨� */
          FIELD materinkomp$ AS CHARACTER /* ����������� */
          FIELD migrkart$ AS CHARACTER /* �������� */
          FIELD migrpravprebpo$ AS DATE /* �����ࠢ�ॡ�� */
          FIELD migrpravprebs$ AS DATE /* �����ࠢ�ॡ� */
          FIELD migrprebyvpo$ AS DATE /* �����ॡ뢏� */
          FIELD migrprebyvs$ AS DATE /* �����ॡ뢑 */
          FIELD migrcelw#vizita$ AS CHARACTER /* �������삨��� */
          FIELD nbki_godob$ AS CHARACTER /* ����_����� */
          FIELD nbki_stpowct$ AS CHARACTER /* ����_�⏮�� */
          FIELD nbki_streg$ AS CHARACTER /* ����_�␥� */
          FIELD nomdop$ AS CHARACTER /* ������ */
          FIELD nomerpf$ AS CHARACTER /* ������� */
          FIELD obosobpodr$ AS CHARACTER /* ���ᮡ���� */
          FIELD ogrn$ AS CHARACTER /* ���� */
          FIELD okato-nalog$ AS CHARACTER /* �����-����� */
          FIELD okvwed$ AS CHARACTER /* ����� */
          FIELD okogu$ AS INT64 /* ����� */
          FIELD okopf$ AS CHARACTER /* ����� */
          FIELD orguprav$ AS CHARACTER /* �࣓�ࠢ */
          FIELD osnvidydewat$ AS CHARACTER /* �ᭂ��넥�� */
          FIELD osnova$ AS CHARACTER /* �᭮�� */
          FIELD ofwsor$ AS CHARACTER /* ���� */
          FIELD ocenkariska$ AS CHARACTER /* �業����᪠ */
          FIELD pokrytie$ AS LOGICAL /* �����⨥ */
          FIELD postkontrag$ AS CHARACTER /* ���⊮��ࠣ */
          FIELD predpr$ AS LOGICAL /* �।�� */
          FIELD prim$ AS CHARACTER /* �ਬ */
          FIELD prim1$ AS CHARACTER /* �ਬ1 */
          FIELD prim2$ AS CHARACTER /* �ਬ2 */
          FIELD prim3$ AS CHARACTER /* �ਬ3 */
          FIELD prim4$ AS CHARACTER /* �ਬ4 */
          FIELD prim5$ AS CHARACTER /* �ਬ5 */
          FIELD prim6$ AS CHARACTER /* �ਬ6 */
          FIELD prisutorguprav$ AS CHARACTER /* �����࣓�ࠢ */
          FIELD priwcvnes$ AS CHARACTER /* ��炭�� */
          FIELD reorgdata$ AS DATE /* ���ࣄ�� */
          FIELD reorgegrn$ AS CHARACTER /* ���ࣅ��� */
          FIELD reorgkpp$ AS CHARACTER /* ���࣊�� */
          FIELD reorgokato$ AS CHARACTER /* ���࣎���� */
          FIELD reorgokpo$ AS CHARACTER /* ���࣎��� */
          FIELD reorgpolnoe$ AS CHARACTER /* ���࣏����� */
          FIELD reorgsokr$ AS CHARACTER /* ���࣑��� */
          FIELD reorgfirm$ AS CHARACTER /* ���ࣔ�� */
          FIELD reorgwazykrf$ AS CHARACTER /* ���ࣟ�몐� */
          FIELD riskotmyv$ AS CHARACTER /* ��᪎�� */
          FIELD svedvygdrlica$ AS CHARACTER /* �����룄���� */
          FIELD statusdata$ AS DATE /* ����ᄠ� */
          FIELD statuspredpr$ AS CHARACTER /* �����।�� */
          FIELD struktorg$ AS CHARACTER /* ������ */
          FIELD subw%ekt$ AS CHARACTER /* ��ꥪ� */
          FIELD tipkl$ AS CHARACTER /* ����� */
          FIELD unikkodadresa$ AS CHARACTER /* ����������� */
          FIELD unk$ AS DECIMAL /* ��� */
          FIELD unkg$ AS INT64 /* ���� */
          FIELD ustavkap$ AS CHARACTER /* ��⠢��� */
          FIELD uwcdok$ AS CHARACTER /* �焮� */
          FIELD uwcdokgr$ AS CHARACTER /* �焮��� */
          FIELD uwcdokdata$ AS DATE /* �焮���� */
          FIELD uwcredorg$ AS CHARACTER /* ��।�� */
          FIELD fiobuhg$ AS CHARACTER /* ������ */
          FIELD fioruk$ AS CHARACTER /* ����� */
          FIELD formsobs$ AS CHARACTER /* ��଑��� */
          FIELD formsobs_118$ AS CHARACTER /* ��଑���_118 */
          FIELD wekonsekt$ AS CHARACTER /* �������� */
          FIELD Address1Indeks AS INT64 /* Address1Indeks */
          FIELD BirthDay AS DATE /* BirthDay */
          FIELD BirthPlace AS CHARACTER /* BirthPlace */
          FIELD branch-id AS CHARACTER /* branch-id */
          FIELD branch-list AS CHARACTER /* branch-list */
          FIELD brand-name AS CHARACTER /* brand-name */
          FIELD CMSCUR AS DECIMAL /* CMSCUR */
          FIELD contr_group AS CHARACTER /* contr_group */
          FIELD cont_type AS CHARACTER /* cont_type */
          FIELD country-id2 AS CHARACTER /* country-id2 */
          FIELD country-id3 AS CHARACTER /* country-id3 */
          FIELD CountryCode AS CHARACTER /* CountryCode */
          FIELD CRSCM AS CHARACTER /* CRSCM */
          FIELD date-export AS CHARACTER /* date-export */
          FIELD diasoft-id AS CHARACTER /* diasoft-id */
          FIELD document AS CHARACTER /* document */
          FIELD document-id AS CHARACTER /* document-id */
          FIELD Document4Date_vid AS DATE /* Document4Date_vid */
          FIELD e-mail AS CHARACTER /* e-mail */
          FIELD engl-name AS CHARACTER /* engl-name */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD holding-id AS CHARACTER /* holding-id */
          FIELD iey AS CHARACTER /* iey */
          FIELD IndCode AS CHARACTER /* IndCode */
          FIELD Isn AS CHARACTER /* Isn */
          FIELD issue AS CHARACTER /* issue */
          FIELD LegTerr AS CHARACTER /* LegTerr */
          FIELD lic-sec AS CHARACTER /* lic-sec */
          FIELD LocCustType AS CHARACTER /* LocCustType */
          FIELD mess AS CHARACTER /* mess */
          FIELD NACE AS CHARACTER /* NACE */
          FIELD Netting AS LOGICAL /* Netting */
          FIELD NoExport AS LOGICAL /* NoExport */
          FIELD num_contr AS INT64 /* num_contr */
          FIELD PlaceOfStay AS CHARACTER /* PlaceOfStay */
          FIELD Prim-ID AS CHARACTER /* Prim-ID */
          FIELD RegDate AS CHARACTER /* RegDate */
          FIELD RegNum AS CHARACTER /* RegNum */
          FIELD RegPlace AS CHARACTER /* RegPlace */
          FIELD RNK AS CHARACTER /* RNK */
          FIELD Soato AS CHARACTER /* Soato */
          FIELD SphereID AS CHARACTER /* SphereID */
          FIELD tel AS CHARACTER /* tel */
          FIELD Telex AS CHARACTER /* Telex */
          FIELD Visa AS CHARACTER /* Visa */
          FIELD VisaNum AS CHARACTER /* VisaNum */
          FIELD VisaType AS CHARACTER /* VisaType */
          FIELD lat_name AS CHARACTER /* lat_name */
          FIELD wembnazv$ AS CHARACTER /* ������� */
          FIELD country-id4 AS CHARACTER /* country-id4 */
          FIELD website AS CHARACTER /* website */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-cust-corp" "" }
          
      END-FIELDS.
      TABLE: tt-p-cust-adr T "?" NO-UNDO bisquit cust-ident
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
          {ln-tthdl.i "tt-p-cust-adr" "p-cust-adr" }
          
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
         HEIGHT             = 25.93
         WIDTH              = 103
         MAX-HEIGHT         = 25.93
         MAX-WIDTH          = 103
         VIRTUAL-HEIGHT     = 25.93
         VIRTUAL-WIDTH      = 103
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
/* SETTINGS FOR FILL-IN tt-cust-corp.addr-of-low[1] IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.addr-of-low[2] IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-HELP                                           */
/* SETTINGS FOR FILL-IN tt-cust-corp.bank-code IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.bank-code-type IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.benacct IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.corr-acct IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.country-id IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.cust-id IN FRAME fMain
   3                                                                    */
ASSIGN 
       tt-cust-corp.cust-id:READ-ONLY IN FRAME fMain        = TRUE.
/* SETTINGS FOR FILL-IN tt-cust-corp.cust-stat IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.date-in IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.date-out IN FRAME fMain
   2 3                                                                  */
/* SETTINGS FOR FILL-IN tt-cust-corp.fax IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.formsobs$ IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-cust-corp.inn IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.kodreg$ IN FRAME fMain
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-cust-corp.kodreggni$ IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-cust-corp.kpp$ IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN mBankClient IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN mFormSobs IN FRAME fMain
   3                                                                    */
/* SETTINGS FOR FILL-IN tt-cust-corp.name-corp IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.name-short IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-cust-corp.ogrn$ IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.okonx IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.okpo IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.okvwed$ IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN tt-cust-corp.subw%ekt$ IN FRAME fMain
   1 2 3 EXP-LABEL EXP-FORMAT                                           */
/* SETTINGS FOR FILL-IN tt-cust-corp.tax-insp IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-cust-corp.tel IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-cust-corp.unk$ IN FRAME fMain
   1 2 3                                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-cust-corp"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-cust-corp.bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.bank-code TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.bank-code IN FRAME fMain /* ������. ����� */
DO:
   IF iMode =  {&MOD_VIEW} THEN DO:
     IF SELF:SCREEN-VALUE <> "" THEN DO:
        mTempVal = GetNeedBankCode(tt-cust-corp.bank-code-type,
                                   SELF:SCREEN-VALUE,
                                   "bank-id").
        RUN banks#.p (mTempVal,iLevel).
     END.
   END.
   ELSE DO:
      IF tt-cust-corp.bank-code-type:SCREEN-VALUE <> "" AND
         tt-cust-corp.bank-code-type:SCREEN-VALUE <> "���"
      THEN DO:
         ASSIGN 
            tt-cust-corp.bank-code-type.
         DO TRANSACTION:
            RUN bankscod.p (INPUT-OUTPUT tt-cust-corp.bank-code-type, iLevel + 1).
         END.
      END.
      ELSE
      DO TRANSACTION:
         RUN banks.p (iLevel + 1).
      END.

      IF     LASTKEY =  10 
         AND pick-value <> ? 
      THEN DO:
         DISPLAY 
            ENTRY(2,pick-value) WHEN NUM-ENTRIES(pick-value) >  1 @ tt-cust-corp.bank-code
            ENTRY(1,pick-value) @ tt-cust-corp.bank-code-type
         WITH FRAME {&FRAME-NAME}.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.tel TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.tel IN FRAME fMain /* ������. ����� */
DO:
   {&BEG_BT_LEAVE}

   IF FGetSetting("�஢�ન","�஢����䮭","") = "��" THEN
      IF tt-cust-corp.tel:SCREEN-VALUE = "" THEN
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "����䮭 ������ �� 㪠���").
 
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.bank-code TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.bank-code IN FRAME fMain /* ������. ����� */
DO:
   {&BEG_BT_LEAVE}
   DEFINE VARIABLE flag-error AS INT64 NO-UNDO.

   IF     tt-cust-corp.bank-code-type:SCREEN-VALUE <> ""
      AND tt-cust-corp.bank-code:SCREEN-VALUE <> "" THEN
   DO:
      IF tt-cust-corp.bank-code:SCREEN-VALUE <> "���" THEN
      DO:
         {getbank.i "banks" "INPUT tt-cust-corp.bank-code" "INPUT tt-cust-corp.bank-code-type"}
      END.
      ELSE
      DO:
         FIND FIRST cust-ident WHERE cust-ident.cust-cat       =  "�"
                               AND   cust-ident.cust-code-type =  "���"
                               AND   cust-ident.cust-code      =  tt-cust-corp.bank-code:SCREEN-VALUE
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
            FIND FIRST banks WHERE banks.bank-id =  cust-ident.cust-id
               NO-LOCK NO-ERROR.
      END.

      IF NOT AVAILABLE(banks) THEN
      DO:
         MESSAGE COLOR ERROR
            "���� � ����� �����䨪��� ~"" + tt-cust-corp.bank-code-type:SCREEN-VALUE + "~"" SKIP
            "� �����䨪��஬ ~"" + tt-cust-corp.bank-code:SCREEN-VALUE + "~" �� ������� " SKIP
         VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY {&RET-ERROR}.
      END.
                                                 /* �஢���� ����������� ࠡ��� � ������        */
      RUN Check-Bank IN h_op (BUFFER banks, YES, OUTPUT flag-error).
      IF NOT flag-error =  0 THEN
         RETURN NO-APPLY {&RET-ERROR}.

      FIND FIRST banks-corr WHERE banks-corr.bank-corr =  banks.bank-id NO-LOCK NO-ERROR.
      IF AVAILABLE banks-corr THEN
         tt-cust-corp.corr-acct = banks-corr.corr-acct.

      DISPLAY 
         tt-cust-corp.corr-acct 
      WITH FRAME {&FRAME-NAME}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.unk$ TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.unk$ IN FRAME fMain /* ��� */
DO:

   DEF VAR vOk AS LOGICAL  NO-UNDO. /* ������� ��८�।������ ��ꥪ� */

   IF iMode =  {&MOD_VIEW} THEN 
      RUN xview.p    ("cust-corp", tt-cust-corp.cust-id).  
   ELSE IF  iMode =  {&MOD_EDIT}
         OR iMode =  {&MOD_ADD} THEN 
   DO:
      DO TRANSACTION:
         RUN browseld.p ("cust-corp", "", "", "", iLevel + 1).
      END.
      IF LAST-EVENT:FUNCTION <> "END-ERROR"
      THEN DO:
                        /* ��४��砥� ०�� � ����� �� ।���஢����. */
         ASSIGN
            iMode       = {&MOD_EDIT}
            iSurrogate  = pick-value
         .

         /* ��९���祭�� ��ꥪ� ��� ���� �맮�� �� �࠭���樨 */
         REGET:
         DO ON ERROR  UNDO, LEAVE
            ON ENDKEY UNDO, LEAVE: 

            IF mOnlyForm THEN
            DO:                     
               RUN UnbindInstance   IN h_data (mInstance, 
                                               iMode).

               RUN GetBaseAttrs     IN h_data ("cust-corp", 
                                               iSurrogate, 
                                               mInstance, 
                                               OUTPUT vOk).
               IF NOT vOk THEN
                  UNDO REGET, LEAVE REGET.
   
               RUN GetExtAttrs      IN h_data ("cust-corp", 
                                               iSurrogate, 
                                               mInstance, 
                                               OUTPUT vOk).
               IF NOT vOk THEN
                  UNDO REGET, LEAVE REGET.
   
               RUN GetAggrInstances IN h_data (mInstance,
                                               OUTPUT vOk).
               IF NOT vOk THEN
                  UNDO REGET, LEAVE REGET.
   
               RUN SetInstanceProp IN h_data (mInstance,
                                              "__template",
                                              "NO",
                                              OUTPUT vOk).
               IF NOT vOk THEN
                  RUN DelEnptyInstance IN h_data (mInstance).
   
               RUN PrepareInstanceEx IN h_data (?, ?, ?, ?, ?).
            END.
         END.

         RUN MainAction.
      END.
   END.

   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.bank-code-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.bank-code-type TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.bank-code-type IN FRAME fMain /* ��� */
DO:
   IF {assigned SELF:SCREEN-VALUE} THEN 
      RUN shifr.p("��������",SELF:SCREEN-VALUE,iLevel + 1).
   ELSE DO:
      DO TRANSACTION:
         RUN pclass.p ("��������", "��������", "���� ������", iLevel + 1).
      END.
      IF     LASTKEY =  10 
         AND pick-value <> ? THEN 
         DISPLAY 
            pick-value @ tt-cust-corp.bank-code-type 
         WITH FRAME {&FRAME-NAME}.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.benacct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.benacct TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.benacct IN FRAME fMain /* ������ ��� */
DO:
   {&BEG_BT_LEAVE}
   IF     tt-cust-corp.bank-code-type:SCREEN-VALUE <> ""
      AND tt-cust-corp.bank-code:SCREEN-VALUE <> "" THEN
   DO:
      RUN key-tst.p(tt-cust-corp.benacct:SCREEN-VALUE,
                    tt-cust-corp.bank-code:SCREEN-VALUE,
                    OUTPUT mAcctKey).
      IF     mAcctKey <> ? 
         AND mAcctKey <> SUBSTR(tt-cust-corp.benacct:SCREEN-VALUE, 9, 1) THEN
      DO:
         MESSAGE "��������: ������ ����!" 
            VIEW-AS ALERT-BOX ERROR.
/*          RETURN NO-APPLY {&RET-ERROR}. */
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.corr-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.corr-acct TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.corr-acct IN FRAME fMain /* ������ � ��� */
DO:
   IF iMode <> {&MOD_VIEW} THEN DO:
      IF     tt-cust-corp.bank-code-type:SCREEN-VALUE <> ""
         AND tt-cust-corp.bank-code:SCREEN-VALUE      <> "" 
      THEN DO:
         IF tt-cust-corp.bank-code-type:SCREEN-VALUE <> "���" THEN
            mTempVal = GetValueByQuery("banks-code",
                                       "bank-id",
                                       "     banks-code.bank-code-type  EQ '" + tt-cust-corp.bank-code-type:SCREEN-VALUE + "'" +
                                       " AND banks-code.bank-code       EQ '" + tt-cust-corp.bank-code:SCREEN-VALUE       + "'"
                                       ).
         ELSE
            mTempVal = GetValueByQuery("cust-ident",
                                       "cust-id",
                                       "     cust-ident.cust-cat        EQ '�' " +
                                       " AND cust-ident.cust-code-type  EQ '" + tt-cust-corp.bank-code-type:SCREEN-VALUE + "'" +
                                       " AND cust-ident.cust-code       EQ '" + tt-cust-corp.bank-code:SCREEN-VALUE       + "'"
                                       ).
         IF mTempVal <> ? THEN
            mTempVal = GetValueByQuery("banks",
                                       "bank-id",
                                       "banks.bank-id  EQ " + mTempVal
                                       ).
         IF mTempVal <> ? THEN
            RUN  banksch2.p (INT64(mTempVal), iLevel + 1).
         IF     LASTKEY =  10 
            AND pick-value <> ? THEN
            DISPLAY 
               pick-value @ tt-cust-corp.corr-acct
            WITH FRAME {&FRAME-NAME}.
      END.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.cust-stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.cust-stat TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.cust-stat IN FRAME fMain /* ����� */
DO:
   IF iMode =  {&Mod_View} THEN DO:
      IF {assigned SELF:SCREEN-VALUE} THEN 
         RUN shifr.p("����।�",GetCodeVal("����।�",SELF:SCREEN-VALUE),iLevel + 1).
   END.
   ELSE DO:
      DO TRANSACTION:
         RUN codelay.p ("����।�", "����।�", "���� �����������", iLevel + 1).
      END.
      IF     LASTKEY =  10 
         AND pick-value <> ? THEN 
         DISPLAY 
            GetCode("����।�",pick-value) @ tt-cust-corp.cust-stat 
         WITH FRAME {&FRAME-NAME}.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.cust-stat TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.cust-stat IN FRAME fMain /* ����� */
DO:
   {&BEG_BT_LEAVE}
   IF SELF:SCREEN-VALUE <> "" THEN
   DO:
      IF GetCodeVal("����।�", SELF:SCREEN-VALUE) =  ? THEN DO:
         RUN Fill-SysMes IN h_tmess ("","comm01","", "%s=����।�" + "%s=" + SELF:SCREEN-VALUE). 
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.date-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.date-in TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.date-in IN FRAME fMain /* ��� ��������� */
,tt-cust-corp.date-out
DO:
   {&BEG_BT_LEAVE}
   /* �஢�ઠ �������� ����, �⮡� �� ��������� �ண�ᮢ�� �訡�� */
   DEFINE VARIABLE vdtTmpDt AS DATE NO-UNDO.
   vdtTmpDt = DATE(SELF:SCREEN-VALUE) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) 
         VIEW-AS ALERT-BOX.
      RETURN NO-APPLY {&RET-ERROR}.
   END.

   /* �஢�ઠ ᮮ⢥��⢨� ��� ������/������� */
   IF INPUT tt-cust-corp.date-out <  INPUT tt-cust-corp.date-in THEN
   DO:
      MESSAGE "��� ��������� ������ ������ ���� ����� ���� �������"
         VIEW-AS ALERT-BOX.
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.inn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.inn TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.inn IN FRAME fMain /* ��� */
DO:
   DEFINE VAR vI      AS INT64    NO-UNDO.
   DEFINE VAR vLen    AS INT64    NO-UNDO.
   DEFINE VAR isAlpha AS LOGICAL  NO-UNDO.
   DEFINE VARIABLE vFl AS LOGICAL NO-UNDO.

   DEFINE BUFFER b-cust-corp FOR cust-corp.
   vFl = YES.
   {&BEG_BT_LEAVE}
   IF SELF:SCREEN-VALUE <> "" THEN
   DO:
     vLen = LENGTH(SELF:SCREEN-VALUE).
     DO vI = 1 TO vLen:
      IF ASC(SUBSTRING(SELF:SCREEN-VALUE,vI,1)) <  48 OR 
         ASC(SUBSTRING(SELF:SCREEN-VALUE,vI,1)) >  57 THEN isAlpha = YES.
      ELSE isAlpha = NO.
      IF isAlpha THEN LEAVE.
     END.
     IF NOT isAlpha THEN
     DO:
        IF INT64(SELF:SCREEN-VALUE) =  0 THEN
        DO:
          MESSAGE '� ��� ����饭� ⮫쪮 �㫨.' VIEW-AS ALERT-BOX.
          RETURN NO-APPLY {&RET-ERROR}.
        END.
     END.
     IF (vLen <> 5) AND (vLen <> 10) AND (vLen <> 12) THEN
     DO:
       MESSAGE '� ��� ������ ���� 5, 10 ��� 12 ���, ������� ' + TRIM(STRING(vLen)) + '.' 
         VIEW-AS ALERT-BOX TITLE " �������� ".
       RETURN NO-APPLY {&RET-ERROR}.
     END.
     IF vLen <> 5 AND
       NOT fValidInnSignature(SELF:SCREEN-VALUE, mTempVal) THEN DO:
       IF vLen = 10 THEN 
           MESSAGE ("��᫥���� ��� ��� (����) ������ ����: ~"" + mTempVal + "~"")
             SKIP "OK - ��ࠢ���, Cancel - �ய�����"
           VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " �������� " UPDATE vfl.
       IF vLen = 12 THEN 
           MESSAGE ("��᫥���� 2 ���� ��� (����) ������ ����: ~"" + mTempVal + "~"")
             SKIP "OK - ��ࠢ���, Cancel - �ய�����"
           VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " �������� " UPDATE vfl.
       IF vfl OR vfl =  ? THEN 
              RETURN NO-APPLY {&RET-ERROR}.
     END.
     mTempVal = GetValueByQuery("cust-corp",
                    "name-corp",
                    "     cust-corp.inn        eq '" + SELF:SCREEN-VALUE + "'" +
                    " AND cust-corp.country-id eq '" + tt-cust-corp.country-id:SCREEN-VALUE + "'" +
                    " AND RECID(cust-corp)  NE " + (IF tt-cust-corp.local__rowid <> ? 
                    THEN STRING(Rowid2Recid('cust-corp',tt-cust-corp.local__rowid)) 
                    ELSE "0")
                  ).
     IF mTempVal <> ? THEN DO:
       MESSAGE "������ � ⠪��� ४����⠬� 㦥 ������� � ⠡���:" 
                SUBSTR(mTempVal, 1, 80) "!" 
          SKIP "OK - ��ࠢ���, Cancel - �ய�����"
       VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL TITLE " �������� " UPDATE vfl.
       IF vfl OR vfl =  ? THEN 
            RETURN NO-APPLY {&RET-ERROR}.
     END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kpp$ TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.kpp$ IN FRAME fMain 
DO:
   {&BEG_BT_LEAVE}

   IF mProvINNKPP <> "��" THEN
      RETURN.

   DEFINE VARIABLE vFl AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vRecid AS Recid NO-UNDO.

   DEFINE BUFFER b-cust-corp FOR cust-corp.
   vFl = YES.

   IF tt-cust-corp.local__rowid <> ?  THEN
       vRecid = Rowid2Recid('cust-corp',tt-cust-corp.local__rowid).
   FIND FIRST b-cust-corp WHERE b-cust-corp.inn =tt-cust-corp.inn:SCREEN-VALUE
                           AND b-cust-corp.country-id = tt-cust-corp.country-id:SCREEN-VALUE
                           AND RECID(b-cust-corp) <> vRecid 
   NO-LOCK NO-ERROR.
   IF AVAIL b-cust-corp THEN
   DO:
   /*�஢�ઠ �� ᮢ������� ��� � ���*/
      IF GetXAttrValueEx("cust-corp", 
         STRING(b-cust-corp.cust-id), "���", "" ) = tt-cust-corp.kpp$:SCREEN-VALUE  THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", 
            "������ � ⠪��� ४����⠬� ��� � ��� 㦥 ������� � ⠡���:~n" + 
             SUBSTR(b-cust-corp.name-corp, 1, 80)  + "!").

           RETURN NO-APPLY {&RET-ERROR}.

      END.
   END.

   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.kodreggni$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-cust-corp.kodreggni$ IN FRAME fMain /* ������ ��� */
,tt-cust-corp.addr-of-low[1]
DO:
   RETURN NO-APPLY {&RET-ERROR}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON ENTRY OF tt-cust-corp.kodreggni$ IN FRAME fMain /* ������ ��� */
DO:
   &SCOPED-DEFINE KodRegGniHelpStr "F1 �᭮���� ����"
   IF NUM-ENTRIES(mHelpStrAdd,{&KodRegGniHelpStr}) = 0 THEN DO:
      SetHelpStrAdd(TRIM(mHelpStrAdd + "�" + {&KodRegGniHelpStr},"�")).
      RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.kodreggni$ IN FRAME fMain /* ������ ��� */
,tt-cust-corp.addr-of-low[1]
DO:
   DEF VAR vHInst    AS HANDLE NO-UNDO. /* �����⥫� �� ID ���짮��⥫�. */
   DEF VAR vTemplate AS LOG    NO-UNDO. /* ���祭�� __template ��� cust-ident.*/
   DEF VAR vOk       AS LOG    NO-UNDO. /* ���� �訡��. */

   IF iMode =  {&MOD_EDIT}
   THEN DO TRANSACTION:
      FIND FIRST bcident WHERE bcident.cust-cat       = "�"
                           AND bcident.cust-id        = tt-cust-corp.cust-id
                           AND bcident.cust-code      = mUniqCodAdr
                           AND bcident.cust-code-type = mAdrType
      NO-LOCK NO-ERROR.
      RUN browseld.p("p-cust-adr", 
                     "cust-cat~001cust-id~001cust-code-type",
                     "�" + CHR(1) + STRING(tt-cust-corp.cust-id) + CHR(1) + mAdrType,
                     "cust-cat~001cust-id~001cust-code-type",
                     iLevel + 1).
      IF LASTKEY =  10
      THEN DO:
         FIND FIRST bcident WHERE
                    bcident.cust-cat       =  "�"
                AND bcident.cust-id        =  tt-cust-corp.cust-id
                AND bcident.cust-code-type =  ENTRY(1,pick-value)
                AND bcident.cust-code      =  ENTRY(2,pick-value)
         NO-LOCK NO-ERROR.
      END.
      IF AVAIL bcident THEN DO:
         {cust-adr.obj 
            &addr-to-vars     = YES
            &addr-to-vars-gni = YES
            &tablefield       = "TRIM(bcident.issue)" 
            &fieldgni         = "GetXattrValue('cust-ident',
                                                bcident.cust-code-type + ',' + 
                                                bcident.cust-code      + ',' + 
                                                STRING(bcident.cust-type-num),
                                                '���뀤��'
                                                )"
         }
         ASSIGN
            vAdrIndInt                 :SCREEN-VALUE = STRING(vAdrIndInt,"999999")           
            vOblChar                   :SCREEN-VALUE = vOblChar    
            vGorChar                   :SCREEN-VALUE = vGorChar  
            vPunktChar                 :SCREEN-VALUE = vPunktChar
            vUlChar                    :SCREEN-VALUE = vUlChar   
            vDomChar                   :SCREEN-VALUE = vDomChar  
            vKorpChar                  :SCREEN-VALUE = vKorpChar 
            vKvChar                    :SCREEN-VALUE = vKvChar
            vStrChar                   :SCREEN-VALUE = vStrChar
            tt-cust-corp.addr-of-low[1]:SCREEN-VALUE = bcident.issue
            tt-cust-corp.kodreggni$    :SCREEN-VALUE = GetXattrValue('cust-ident',
                                                                     bcident.cust-code-type + ',' + 
                                                                     bcident.cust-code      + ',' + 
                                                                     STRING(bcident.cust-type-num),
                                                                     '���������'
                                                                     )
            vAdrCntry                                = GetXattrValue('cust-ident',
                                                                     bcident.cust-code-type + ',' + 
                                                                     bcident.cust-code      + ',' + 
                                                                     STRING(bcident.cust-type-num),
                                                                     'country-id'
                                                                     ) 
            tt-cust-corp.kodreg$       :SCREEN-VALUE = GetXattrValue('cust-ident',
                                                                      bcident.cust-code-type + ',' + 
                                                                      bcident.cust-code      + ',' + 
                                                                      STRING(bcident.cust-type-num),
                                                                      '������'
                                                                      )  
            mUniqCodAdr                               = bcident.cust-code                                                                                                                                           
         .
      END.
   END.
   ELSE IF iMode =  {&MOD_ADD}
   THEN DO:
      /* ����祭�� 㪠��⥫� �� �����. */
      ASSIGN
         vHInst      = WIDGET-HANDLE (GetInstanceProp2 (mInstance,"p-cust-adr"))
         vTemplate   = LOGICAL (GetInstanceProp2 (vHInst,"__template"))
      .
      RUN SetInstanceProp (vHInst,"cust-cat"      ,"�"     ,OUTPUT vOk) NO-ERROR.
      RUN SetInstanceProp (vHInst,"cust-code-type",mAdrType,OUTPUT vOk) NO-ERROR.
      RUN formld.p (STRING (vHInst) + "~003p-cust-adr",
                    "",
                    "�~003~003" + mAdrType,
                    {&MOD_ADD},
                    iLevel + 6
                  ).
      IF LAST-EVENT:FUNCTION <> "END-ERROR"
      THEN DO:
         TEMP-TABLE tt-p-cust-adr:DEFAULT-BUFFER-HANDLE:BUFFER-COPY (vHInst:DEFAULT-BUFFER-HANDLE).
         {cust-adr.obj 
            &addr-to-vars     = YES
            &addr-to-vars-gni = YES
            &tablefield       = "TRIM(tt-p-cust-adr.issue)" 
            &fieldgni         = "tt-p-cust-adr.kodyadresa$"
         }
         ASSIGN
            vAdrIndInt                 :SCREEN-VALUE = STRING(vAdrIndInt,"999999")           
            vOblChar                   :SCREEN-VALUE = vOblChar    
            vGorChar                   :SCREEN-VALUE = vGorChar  
            vPunktChar                 :SCREEN-VALUE = vPunktChar
            vUlChar                    :SCREEN-VALUE = vUlChar   
            vDomChar                   :SCREEN-VALUE = vDomChar  
            vKorpChar                  :SCREEN-VALUE = vKorpChar 
            vKvChar                    :SCREEN-VALUE = vKvChar
            vStrChar                   :SCREEN-VALUE = vStrChar
            tt-cust-corp.addr-of-low[1]:SCREEN-VALUE = tt-p-cust-adr.issue
            tt-cust-corp.kodreggni$    :SCREEN-VALUE = tt-p-cust-adr.kodreggni$
            vAdrCntry                                = tt-p-cust-adr.country-id
            tt-cust-corp.kodreg$       :SCREEN-VALUE = tt-p-cust-adr.kodreg$
            mUniqCodAdr                              = tt-p-cust-adr.cust-code                                                                                                                                           
         .
      END.
      ELSE RUN SetInstanceProp (vHInst, "__template", STRING (vTemplate), OUTPUT vOk) NO-ERROR.
   END.
   ELSE IF iMode = {&MOD_VIEW} THEN DO:
      FOR FIRST bcident WHERE bcident.cust-cat       = "�"
                          AND bcident.cust-id        = tt-cust-corp.cust-id
                          AND bcident.cust-code      = mUniqCodAdr
                          AND bcident.cust-code-type = mAdrType
      NO-LOCK:
         RUN formld.p ("p-cust-adr",
                       GetSurrogateBuffer("cust-ident",(BUFFER bcident:HANDLE)),
                       "",
                       {&MOD_VIEW},
                       iLevel + 1
                      ).
      END.
   END.

   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON CLEAR OF tt-cust-corp.kodreggni$ IN FRAME fMain /* ������ ��� */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON DELETE-CHARACTER OF tt-cust-corp.kodreggni$ IN FRAME fMain /* ������ ��� */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON BACKSPACE OF tt-cust-corp.kodreggni$ IN FRAME fMain /* ������ ��� */
DO:
    RETURN NO-APPLY.  
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.kodreggni$ TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.kodreggni$ IN FRAME fMain /* ������ ��� */
DO:
   SetHelpStrAdd(TRIM(REPLACE(mHelpStrAdd,{&KodRegGniHelpStr},""),"�")).
   RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
END.
&ANALYZE-RESUME
/* _UIB-CODE-BLOCK-END */

&Scoped-define SELF-NAME mBankClient
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mBankClient TERMINAL-SIMULATION
ON F1 OF mBankClient IN FRAME fMain /* �⭮襭�� � ����� */
DO:
   IF     iMode =  {&MOD_ADD} 
      OR  iMode =  {&MOD_EDIT}
    THEN SELF:SCREEN-VALUE = IF SELF:SCREEN-VALUE =  ENTRY (1, SELF:FORMAT, "/")
                               THEN ENTRY (2, SELF:FORMAT, "/")
                               ELSE ENTRY (1, SELF:FORMAT, "/").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mBankClient TERMINAL-SIMULATION
ON LEAVE OF mBankClient IN FRAME fMain /* �⭮襭�� � ����� */
DO:
   {&BEG_BT_LEAVE}
   IF iMode =  {&MOD_EDIT}
      AND SELF:SCREEN-VALUE <> ENTRY (1, SELF:FORMAT, "/")
   THEN DO:
      IF GetBufferValue (
         "acct",
         "WHERE " +
         "     acct.cust-cat  EQ '�' " +
         "AND  acct.cust-id   EQ '" + STRING (tt-cust-corp.cust-id) + "'",
         "acct") <> ?
      THEN DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "0",
            "� ��ꥪ� ���� ���.~n����� ���� �ਧ��� ""������""."
         ).
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.name-short
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.name-short TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.name-short IN FRAME fMain /* ��⪮� ������ */
DO:
   IF iMode =  {&MOD_VIEW}  THEN
      RUN xview.p("cust-corp",tt-cust-corp.cust-id).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.ogrn$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.ogrn$ TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.ogrn$ IN FRAME fMain /* ogrn$ */
DO:
   DEFINE VARIABLE mOGRN2   AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE mMessage AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE mYesNo   AS LOGICAL   INITIAL YES NO-UNDO.
   DEFINE VARIABLE mNumb    AS INT64                 NO-UNDO.
   DEFINE VARIABLE mSurr    AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE mIsReady AS LOGICAL               NO-UNDO.
   DEFINE VARIABLE mPredPr  AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE vProvOGRN AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vProvOGRNUL AS CHARACTER            NO-UNDO.

   DEFINE VARIABLE mItem AS INT64 NO-UNDO.
   {&BEG_BT_LEAVE}

   vProvOGRN = FGetSetting("�஢�ન","����஢����","").
   vProvOGRNUL = FGetSetting("�஢�ન","�஢������","").

   IF     tt-cust-corp.ogrn$:SCREEN-VALUE <> "" 
      AND tt-cust-corp.ogrn$:SCREEN-VALUE <> "?" 
      THEN
   DO:
      mIsReady = NO.

      IF iMode =  {&MOD_EDIT} THEN
      DO:
         mPredPr = GetXAttrValueEx(iClass, STRING(tt-cust-corp.cust-id), "�।��", "").
      END.

      IF iMode   <> {&MOD_EDIT} 
      OR mPredPr <> "�।��"
      THEN
      DO:
         RUN chk-ogrn.p(tt-cust-corp.ogrn$:SCREEN-VALUE, YES, OUTPUT mOGRN2) .

         IF tt-cust-corp.ogrn$:SCREEN-VALUE =  mOGRN2 THEN
            ASSIGN
               mMessage = "��諮 ���祢���� ���� ��� �ਤ��᪮�� ���"
               mIsReady = YES
            .
      END.

      IF  NOT mIsReady
      AND NOT (    iMode <> {&MOD_EDIT}
               AND {assigned mOGRN2}
               AND TRIM(mOGRN2,"1234567890") = "")
      AND (   iMode   <> {&MOD_EDIT}
           OR mPredPr =  "�।��")
      THEN
      DO:
         RUN chk-ogrn.p(tt-cust-corp.ogrn$:SCREEN-VALUE, NO, OUTPUT mOGRN2) .

         IF tt-cust-corp.ogrn$:SCREEN-VALUE =  mOGRN2 THEN
            ASSIGN
               mMessage = "��諮 ���祢���� ���� ��� �।�ਭ���⥫�"
               mIsReady = YES
            .
      END.

      IF NOT mIsReady THEN
      DO:

         IF mOGRN2 =  "** �� ��諮 **" OR (iMode <> {&MOD_EDIT} AND mOGRN2 BEGINS "** ") THEN
            mMessage = "�訡�� � ���祢���� ����~n   ���� = " + tt-cust-corp.ogrn$:SCREEN-VALUE.
         ELSE IF mOGRN2 BEGINS "** " THEN
            mMessage = "�訡�� � ���祢���� ����~n"  + SUBSTRING(mOGRN2,4).
         ELSE
            mMessage = "�訡�� � ���祢���� ����~n ������ ���� " + mOGRN2 + " ����� " + tt-cust-corp.ogrn$:SCREEN-VALUE.

         IF vProvOGRN = "��" AND INDEX(mOGRN2,"����ୠ� �����") > 0 THEN
         DO:
            IF mPredPr =  "�।��" THEN 
               RUN Fill-SysMes IN h_tmess ("","","-1",
                                           "��ଠ� ���� ��� �।�ਭ���⥫� 15 ᨬ�����").
            ELSE 
               RUN Fill-SysMes IN h_tmess ("","","-1","��ଠ� ���� ��� ��.��� 13 ᨬ�����").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
         ELSE 
         DO:
            MESSAGE 
               mMessage + "~n�㤥� ��ࠢ����?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mYesNo.
   
            IF mYesNo <> NO THEN
               RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
      ELSE
         IF iMode <> {&MOD_EDIT} THEN 
         DO:
            IF mLastOkOGRN <> tt-cust-corp.ogrn$:SCREEN-VALUE THEN 
               MESSAGE mMessage VIEW-AS ALERT-BOX.
            mLastOkOGRN = tt-cust-corp.ogrn$:SCREEN-VALUE.

         END.
      IF vProvOGRNUL = "��" THEN
         IF LENGTH(tt-cust-corp.ogrn$:SCREEN-VALUE) <> 13 THEN 
         DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","��ଠ� ���� ��� ��.��� 13 ᨬ�����").
            RETURN NO-APPLY {&RET-ERROR}.
         END.

      IS_EXIST:
      DO mItem = 1 TO 2:
         FOR FIRST signs NO-LOCK WHERE signs.file-name =  ENTRY(mItem, "banks,cust-corp")
                           AND signs.code      =  "����"
                           AND signs.code-val  =  tt-cust-corp.ogrn$:SCREEN-VALUE
                           AND (   signs.file-name <> "cust-corp"
                                OR iMode           =  {&MOD_ADD}
                                OR (    iMode           =  {&MOD_EDIT}
                                    AND signs.surrogate <> STRING(tt-cust-corp.cust-id))
                               ):
      
            MESSAGE 
               "������ ����=" + tt-cust-corp.ogrn$:SCREEN-VALUE + " 㦥 �������~n�㤥� ��ࠢ����?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mYesNo.

            IF mYesNo <> NO THEN
               RETURN NO-APPLY {&RET-ERROR}.
            ELSE
               LEAVE IS_EXIST.
         END.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-cust-corp.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.unk$ TERMINAL-SIMULATION
ON LEAVE OF tt-cust-corp.unk$ IN FRAME fMain /* ��� */
DO:
{&BEG_BT_LEAVE}
   IF (FGetSetting("������", "", "���") = "��" AND mBankClient) 
       OR FGetSetting("������", "", "���") = "���"
   THEN DO:
      RUN ChkUpdUnk$ IN h_cust("cust-corp",
                               STRING(tt-cust-corp.cust-id),
                               tt-cust-corp.unk$:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "0",
            RETURN-VALUE
         ).
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
{&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/*&Scoped-define SELF-NAME tt-cust-corp.okonx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.okonx TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.okonx IN FRAME fMain /* ����� */
DO:
   IF {assigned SELF:SCREEN-VALUE} THEN 
      RUN shifr.p("�����",SELF:SCREEN-VALUE,iLevel + 1).
   ELSE DO:
      DO TRANSACTION:
         RUN pclass.p ("�����", "�����", "���� �����", iLevel + 1).
      END.
      IF     LASTKEY EQ 10 
         AND pick-value NE ? THEN 
         DISPLAY 
            pick-value @ tt-cust-corp.okonx
         WITH FRAME {&FRAME-NAME}.
   END.

   {&END_BT_F1}
END.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.okvwed$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.okvwed$ TERMINAL-SIMULATION
ON F1 OF tt-cust-corp.okvwed$ IN FRAME fMain /* ����� */
DO:
   DEF VAR mI        AS INT64  NO-UNDO.
   DEF VAR vOldVal   AS CHAR   NO-UNDO.
   DEF VAR vOldFmt   AS CHAR   NO-UNDO.
   DEF VAR vDateSign AS DATE   NO-UNDO.
   DEF VAR vDatePer  AS DATE   NO-UNDO.
   DEF VAR vCodeName AS CHAR   NO-UNDO.

   DEFINE BUFFER   tmpsigns FOR tmpsigns.

   IF iMode =  {&MOD_VIEW} THEN DO:
      IF  {assigned "SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}"}
      THEN DO:
         vDatePer = DATE(FGetSetting("��⠎����2","",?)) NO-ERROR.

         FOR LAST tmpsigns WHERE                        
                  tmpsigns.code        =  '�����'                    
              AND tmpsigns.file-name   =  'cust-corp'       
              AND tmpsigns.surrogate   =  STRING(tt-cust-corp.cust-id)  
              AND tmpsigns.xattr-value =  SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         NO-LOCK:
            vDateSign = tmpsigns.since.
         END.
                        /* ��।������ �����䨪��� � ����ᨬ��� �� ���� �� */
         vCodeName = IF vDateSign >=  vDatePer
                     THEN "�����2"
                     ELSE "�����".
         RUN formld.p("code",
                      STRING(vCodeName + ',' +  
                             SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                      "",
                      3,
                      5) NO-ERROR.

      END.
   END.
   ELSE DO:
      RUN LookupXattr IN h_xclass ("cust-corp",
                                   "�����",iLevel + 1).
      IF     LASTKEY =  10 
         AND pick-value <> ? 
      THEN
         SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = pick-value.
   END.

   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-cust-corp.unk$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-cust-corp.unk$ TERMINAL-SIMULATION
ON ENTRY OF tt-cust-corp.unk$ IN FRAME fMain /* unk$ */
DO:
   IF iMode              =  {&MOD_EDIT}
   AND tt-cust-corp.unk$ =  ? 
   AND mFlagUnk
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "4", "��᢮��� ����� ���祭�� ���?").
      IF pick-value =  "YES" THEN
         tt-cust-corp.unk$:SCREEN-VALUE = STRING(NewUnk("cust-corp")).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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

ON F6 OF FRAME {&MAIN-FRAME} ANYWHERE DO:
   DEFINE VAR vMandAddrs AS CHAR NO-UNDO.
   DEFINE BUFFER xxcode FOR code.
   IF iMode = {&MOD_EDIT}
   OR iMode = {&MOD_VIEW} THEN DO:
      FOR EACH xxcode WHERE xxcode.class = "������"
                        AND CAN-DO(xxcode.misc[2],"�") NO-LOCK:
         {additem.i vMandAddrs xxcode.code}
      END.
      RUN browseld.p("p-cust-adr", 
                     "cust-cat~001cust-id~001cust-code-type",
                     "�" + CHR(1) + STRING(tt-cust-corp.cust-id) + CHR(1) + vMandAddrs,
                     "cust-cat~001cust-id~001cust-code-type",
                     iLevel + 1).
   END.
   {&END_BT_F1}
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

   {getflagunk.i &class-code="'cust-corp'" &flag-unk="mFlagUnk"}
   IF mFlagUnk THEN DO:
      mTempVal = GetXAttrEx("cust-corp","���","data-format").
      IF mTempVal <> FILL("9", LENGTH(mTempVal)) THEN DO:
         MESSAGE "��ଠ� ���, ������� � ����奬� �.�. ~"999...~"!"
            VIEW-AS ALERT-BOX ERROR.
         UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
      END.
   END.

   /* Commented by KSV: ���樠������ ��⥬��� ᮮ�饭�� */
   RUN Init-SysMes("","","").

   /* Commented by KSV: ���४��㥬 ���⨪����� ������ �३�� */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.
   
   RUN MainAction NO-ERROR.
   IF ERROR-STATUS:ERROR
      THEN LEAVE MAIN-BLOCK.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
END. /* MAIN-BLOCK */

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
  DISPLAY mBankClient separator1 vOblChar vGorChar vPunktChar vUlChar vDomChar 
          vStrChar vKorpChar vKvChar vAdrIndInt separator2 mFormSobs 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-cust-corp THEN 
    DISPLAY tt-cust-corp.unk$ tt-cust-corp.cust-id tt-cust-corp.date-in 
          tt-cust-corp.date-out tt-cust-corp.cust-stat tt-cust-corp.name-corp 
          tt-cust-corp.name-short tt-cust-corp.country-id 
          tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ 
          tt-cust-corp.kodreg$ tt-cust-corp.addr-of-low[2] tt-cust-corp.tel 
          tt-cust-corp.fax tt-cust-corp.tax-insp tt-cust-corp.inn 
          tt-cust-corp.ogrn$ tt-cust-corp.okvwed$ tt-cust-corp.okpo 
          /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ tt-cust-corp.formsobs$ 
          tt-cust-corp.subw%ekt$ tt-cust-corp.bank-code-type 
          tt-cust-corp.bank-code tt-cust-corp.corr-acct tt-cust-corp.website 
          tt-cust-corp.benacct 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE mBankClient tt-cust-corp.unk$ tt-cust-corp.cust-id 
         tt-cust-corp.date-in tt-cust-corp.date-out tt-cust-corp.cust-stat 
         tt-cust-corp.name-corp tt-cust-corp.name-short tt-cust-corp.country-id 
         separator1 tt-cust-corp.addr-of-low[1] tt-cust-corp.kodreggni$ 
         tt-cust-corp.kodreg$ vOblChar tt-cust-corp.addr-of-low[2] vGorChar 
         vPunktChar vUlChar vDomChar vStrChar vKorpChar vKvChar vAdrIndInt 
         separator2 tt-cust-corp.tel tt-cust-corp.fax tt-cust-corp.tax-insp 
         tt-cust-corp.inn tt-cust-corp.ogrn$ tt-cust-corp.okvwed$ 
         tt-cust-corp.okpo /*tt-cust-corp.okonx*/ tt-cust-corp.kpp$ 
         tt-cust-corp.formsobs$ mFormSobs tt-cust-corp.subw%ekt$ 
         tt-cust-corp.bank-code-type tt-cust-corp.bank-code 
         tt-cust-corp.corr-acct tt-cust-corp.website tt-cust-corp.benacct 
         mWebsite 
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
   DO WITH FRAME {&FRAME-NAME}: 
      ASSIGN
         tt-cust-corp.unk$     :HIDDEN = NOT mFlagUnk
         tt-cust-corp.cust-id  :HIDDEN = mFlagUnk
         mFormSobs             :HIDDEN = iMode <> {&MOD_View}
         tt-cust-corp.formsobs$:HIDDEN = iMode =  {&MOD_View}
         &IF DEFINED( MANUAL-REMOTE ) &THEN
            tt-cust-corp.Website  :HIDDEN = iMode =  {&MOD_View}
            mWebsite              :HIDDEN = iMode <> {&MOD_View}
            mWebsite              :SENSITIVE = NO
         &ENDIF
         
         
      .
      CASE mFrmRole:
         WHEN "addr_struct" THEN
            ASSIGN
               tt-cust-corp.addr-of-low[1]:HIDDEN = YES
               tt-cust-corp.addr-of-low[2]:HIDDEN = YES
               tt-cust-corp.kodreggni$    :HIDDEN = NO
               vAdrIndInt                 :HIDDEN = NO
               vOblChar                   :HIDDEN = NO   
               vGorChar                   :HIDDEN = NO
               vPunktChar                 :HIDDEN = NO
               vUlChar                    :HIDDEN = NO
               vDomChar                   :HIDDEN = NO
               vKorpChar                  :HIDDEN = NO
               vKvChar                    :HIDDEN = NO
               vStrChar                   :HIDDEN = NO
            .
         WHEN "addr_nostruct" THEN
            ASSIGN
               tt-cust-corp.addr-of-low[1]:HIDDEN    = NO
               tt-cust-corp.addr-of-low[2]:HIDDEN    = NO
               tt-cust-corp.kodreggni$    :HIDDEN    = YES
               vAdrIndInt                 :HIDDEN    = YES
               vOblChar                   :HIDDEN    = YES   
               vGorChar                   :HIDDEN    = YES
               vPunktChar                 :HIDDEN    = YES
               vUlChar                    :HIDDEN    = YES
               vDomChar                   :HIDDEN    = YES
               vKorpChar                  :HIDDEN    = YES
               vKvChar                    :HIDDEN    = YES
               vStrChar                   :HIDDEN    = YES
            .

      END CASE.
      IF iMode =  {&MOD_ADD} OR
        (iMode =  {&MOD_EDIT} AND
         NOT CAN-FIND(FIRST cust-role WHERE
                           cust-role.cust-cat   =  "�"
                       AND cust-role.cust-id    =  STRING(tt-cust-corp.cust-id)
                       AND cust-role.Class-Code =  "ImaginClient"
                       AND cust-role.file-name  =  "branch"                                  
                       AND cust-role.surrogate  <> shFilial
                           USE-INDEX cust-id 
                           NO-LOCK)) THEN DO:
          mBankClient:SENSITIVE = YES.

      END.
      ELSE mBankClient:SENSITIVE = NO.

      IF   iMode =  {&MOD_ADD} AND   
           NUM-ENTRIES (iInstanceList, CHR (3)) >  3
      THEN tt-cust-corp.inn:SCREEN-VALUE = ENTRY(4, iInstanceList,CHR(3)).

   END.        


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
   /* �����뢠�� ���祭�� ��६����� ���� � ���� ���� */
   {cust-adr.obj 
      &vars-to-addr = YES
      &tablefield   = "tt-cust-corp.addr-of-low[1]"
      &AddCond      = "tt-cust-corp.addr-of-low[2] = '__FORM~001'"
   }
   
   tt-cust-corp.kodyadresa$ = fChkDopGni(tt-cust-corp.kodreggni$:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                         vCodeOblChar,
                                         vCodeGorChar,
                                         vCodePunktChar,
                                         vCodeUlChar).

   
   tt-cust-corp.unikkodadresa$ = mUniqCodAdr.

   ASSIGN
      tt-cust-corp.kodreg$.

   SetFormDefList('tt-cust-corp.kodyadresa$,tt-cust-corp.unikkodadresa$,tt-cust-corp.kodreg$').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainAction TERMINAL-SIMULATION 
PROCEDURE MainAction PRIVATE :
   
   IF     NUM-ENTRIES(iInstanceList,CHR(3)) >  1
      AND ENTRY(2, iInstanceList,CHR(3)) >  ""
   THEN mFrmRole = ENTRY(2, iInstanceList,CHR(3)).
   ELSE mFrmRole = "addr_struct".

   IF GetCode("������", mAdrType) =  "0"  THEN
      mFrmRole = "addr_nostruct".

   IF NUM-ENTRIES(iInstanceList,CHR(3)) >  2 
   THEN mClient = NOT (ENTRY(3, iInstanceList,CHR(3)) =  'no').
   ELSE mClient = YES.

   /* Commented by KSV: ��⠥� ����� */
   RUN GetObject IN THIS-PROCEDURE NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN ERROR "".

   &IF DEFINED( MANUAL-REMOTE ) &THEN
      mWebsite :SCREEN-VALUE IN FRAME {&FRAME-NAME} = GetXAttrValue("cust-corp",STRING(tt-cust-corp.cust-id),"website").
   &ENDIF
   IF iMode = {&MOD_EDIT} OR iMode = {&MOD_VIEW}
   THEN SetHelpStrAdd("F6 ��易⥫�� ����").
        
   IF mFlagUnk
   THEN tt-cust-corp.unk$:FORMAT IN FRAME {&FRAME-NAME} = mTempVal.
   ELSE tt-cust-corp.unk$:FORMAT IN FRAME {&FRAME-NAME} = FILL("9",20).

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
   LEAVE MAIN-BLOCK.
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
   DEF BUFFER cust-ident FOR cust-ident.

   DEFINE VARIABLE vHs      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vHfl     AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vAddress AS CHARACTER NO-UNDO.

   IF      ((FGetSetting("�����녃�","���_������","") =  "��"
        OR FGetSetting("�����녃�","���_������","")   =  "��")
       AND iMode =  {&MOD_ADD})
        OR ((FGetSetting("�����녃�","���_������","") =  "��"
        OR FGetSetting("�����녃�","���_������","")   =  "��")
       AND iMode =  {&MOD_EDIT}) THEN
   DO:
      RUN Fill-SysMes IN h_tmess(
         "","",3,
         "������� ᢥ����� �� �����/�����?:|��⪨� ᢥ�����,�믨᪠,�� ����訢���").
      IF    pick-value =  "1"
         OR pick-value =  "2" THEN
      DO:
         IF iMode =  {&MOD_ADD} THEN
         DO:
            RUN SetSysConf IN h_base("EgrMode","Add").
            {egr-client.i &ogrn = YES}
         END.
         IF iMode =  {&MOD_EDIT} THEN
         DO:
            RUN SetSysConf IN h_base("EgrClId",STRING(tt-cust-corp.cust-id)).
            RUN SetSysConf IN h_base("EgrMode","Edit"). 
            IF LENGTH(TRIM(tt-cust-corp.inn)) =  10 THEN
            DO:
               RUN SetSysConf IN h_base("EgrField","���,��").
               RUN SetSysConf IN h_base("EgrValue",tt-cust-corp.inn).
            END.
            ELSE IF LENGTH(TRIM(tt-cust-corp.inn)) =  12 THEN
            DO:
               RUN SetSysConf IN h_base("EgrField","���,��,cust-corp").
               RUN SetSysConf IN h_base("EgrValue",tt-cust-corp.inn).
            END.
            ELSE
               RUN Fill-SysMes IN h_tmess("","",1,"����ୠ� ����� ��� ��� ��ࠢ�� �����!").
         END.
         IF     LASTKEY <> KEYCODE("ESC")
            AND NUM-ENTRIES(GetSysConf("EgrField")) >= 2 THEN
         DO:
            {intrface.get egr}      /*������⥪� ����ᮢ ���*/
            IF GetBaseOpDate() =  ? THEN
               RUN InitBaseLibrary IN h_pbase (?,TODAY,?).
            IF pick-value =  "1" THEN
            DO:
               IF ENTRY(2,GetSysConf("EgrField")) =  "��" THEN
               DO:
                  mTypeEgr = "1".
                  RUN RunTransaction IN h_pbase ("e-zaprksul").
                  RUN TimeOutEgr IN h_egr ("i-egrksul", OUTPUT mErrEgr) .
                  IF mErrEgr THEN
                     RUN Fill-SysMes IN h_tmess("","",1,"��� �⢥� �� ����!").
                  ELSE                  
                  DO:
                     RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                        "ClassSurrogate", 
                        "cust-corp").     
                     RUN RunTransaction IN h_pbase ("i-egrksul").                                     
                  END.                  
               END.
               ELSE IF ENTRY(2,GetSysConf("EgrField")) =  "��" THEN
               DO:
                  mTypeEgr = "2".
                  RUN RunTransaction IN h_pbase ("e-zaprksip").
                  RUN TimeOutEgr IN h_egr ("i-egrksip", OUTPUT mErrEgr) .
                  IF mErrEgr THEN
                     RUN Fill-SysMes IN h_tmess("","",1,"��� �⢥� �� ����!").
                  ELSE
                  DO:
                     RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                        "ClassSurrogate", 
                        "cust-corp").     
                     RUN RunTransaction IN h_pbase ("i-egrksip").                                                        
                  END.                     
               END.
            END.
            ELSE IF pick-value =  "2" THEN
            DO:
               IF ENTRY(2,GetSysConf("EgrField")) =  "��" THEN
               DO:
                  mTypeEgr = "3".
                  RUN RunTransaction IN h_pbase ("e-zaprvipul").
                  RUN TimeOutEgr IN h_egr ("i-egrvipul", OUTPUT mErrEgr) .
                  IF mErrEgr THEN
                     RUN Fill-SysMes IN h_tmess("","",1,"��� �⢥� �� ����!").
                  ELSE
                  DO:
                     RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                        "ClassSurrogate", 
                        "cust-corp").     
                     RUN RunTransaction IN h_pbase ("i-egrvipul").                                                       
                  END.                                      
               END.
               ELSE IF ENTRY(2,GetSysConf("EgrField")) =  "��" THEN
               DO:
                  mTypeEgr = "4".
                  RUN RunTransaction IN h_pbase ("e-zaprvipip").
                  RUN TimeOutEgr IN h_egr ("i-egrvipip", OUTPUT mErrEgr) .
                  IF mErrEgr THEN
                     RUN Fill-SysMes IN h_tmess("","",1,"��� �⢥� �� ����!").
                  ELSE
                  DO:
                     RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, 
                        "ClassSurrogate", 
                        "cust-corp").     
                     RUN RunTransaction IN h_pbase ("i-egrvipip").                                                       
                  END.                   
               END.
            END.
            IF NOT mErrEgr THEN
            DO:
               RUN Fill-SysMes IN h_tmess("","",4,"����� ᢥ����� � ������ ������?").
               IF pick-value =  "YES" THEN
               DO:
                  mUpdEgr = YES.
                  RUN SetSysConf IN h_base("EgrUpdate","YES").
               END.
            END.
         END.
         RUN SetSysConf IN h_base("EgrValue","").
         RUN SetSysConf IN h_base("EgrClId","").
         RUN SetSysConf IN h_base("EgrMode","").
      END.
   END.
   
   IF iMode <> {&MOD_VIEW} THEN DO:
      IF iMode =  {&MOD_ADD} THEN
         ASSIGN
            mBankClient = mClient
            tt-cust-corp.date-in = TODAY
         .
      
      tt-cust-corp.last-date = TODAY.
      
      IF  tt-cust-corp.unk$       =  ? 
      AND mFlagUnk
      THEN DO:
         pick-value = "YES".
         IF iMode =  {&MOD_EDIT} THEN
            RUN Fill-SysMes IN h_tmess ("", "", "4", "��᢮��� ����� ���祭�� ���?").
         IF pick-value =  "YES" THEN
         DO:
            IF (FGetSetting("������", "", "���") =  "��" AND mBankClient) 
               OR FGetSetting("������", "", "���") =  "���" THEN
               tt-cust-corp.unk$ = NewUnk("cust-corp").
            IF  iMode =  {&MOD_EDIT} THEN
               RUN Fill-SysMes IN h_tmess ("", "", "1", "������� ��᢮��� ����� ���祭�� ��� - " + STRING(tt-cust-corp.unk$,mTempVal)).
         END.
      END.

      IF     (iMode =  {&MOD_ADD}
          OR iMode =  {&MOD_EDIT})
         AND mUpdEgr THEN
      DO:
         CASE mTypeEgr:
            WHEN "1" THEN
            DO:
               FIND FIRST ttRespKSUL NO-LOCK NO-ERROR.
               IF AVAIL ttRespKSUL THEN
               DO:
                  ASSIGN
                     tt-cust-corp.name-corp      = ttRespKSUL.fNameCorp
                     tt-cust-corp.name-short     = ttRespKSUL.fShortName
                     tt-cust-corp.inn            = ttRespKSUL.fInn
                     tt-cust-corp.ogrn$          = ttRespKSUL.fOGRN
                     tt-cust-corp.kpp$           = ttRespKSUL.fKPP
                     tt-cust-corp.tax-insp       = ttRespKSUL.fCodeNo
                  .
                  RUN SetSysConf IN h_base("EgrField",
                                           ttRespKSUL.fRegGni + ";" 
                                         + ttRespKSUL.fAddr   + ";"
                                         + ttRespKSUL.fNumReg).
               END.
            END.
            WHEN "2" THEN
            DO:
               FIND FIRST ttRespKSIP NO-LOCK NO-ERROR.
               IF AVAIL ttRespKSIP THEN
               DO:
                  ASSIGN
                     tt-cust-corp.cust-stat      = GetCodeEx("����।�",
                                                             ttRespKSIP.fCuststat,
                                                             tt-cust-corp.cust-stat)
                     tt-cust-corp.name-corp      = ttRespKSIP.fLastName + " " 
                                                 + ttRespKSIP.fFirstName
                     tt-cust-corp.inn            = ttRespKSIP.fInn
                     tt-cust-corp.ogrn$          = ttRespKSIP.fOGRN
                     tt-cust-corp.tax-insp       = ttRespKSIP.fCodeNo
                  .
                  RUN SetSysConf IN h_base("EgrField",
                                           ttRespKSIP.fRegGni + ";" 
                                         + ttRespKSIP.fAddr   + ";"
                                         + ttRespKSIP.fNumReg).
               END.
            END.
            WHEN "3" THEN
            DO:
               FIND FIRST ttRespVIPUl NO-LOCK NO-ERROR.
               IF AVAIL ttRespVIPUl THEN
               DO:
                  ASSIGN
                     tt-cust-corp.cust-stat      = GetCodeEx("����।�",
                                                             ttRespVIPUl.fCuststat,
                                                             tt-cust-corp.cust-stat)
                     tt-cust-corp.inn            = ttRespVIPUl.fInn  
                     tt-cust-corp.ogrn$          = ttRespVIPUl.fOGRN
                     tt-cust-corp.name-corp      = ttRespVIPUl.fNameCorp
                     tt-cust-corp.name-short     = ttRespVIPUl.fShortName
                     tt-cust-corp.tax-insp       = ttRespVIPUl.fCodeNo
                     tt-cust-corp.kpp$           = ttRespVIPUl.fKPP
                  .
                  RUN SetSysConf IN h_base("EgrField",
                                           ttRespVIPUl.fRegGni + ";" 
                                         + ttRespVIPUl.fAddr   + ";"
                                         + ttRespVIPUl.fNumReg).

                  FOR EACH ttRespVIPOkved WHERE ttRespVIPOkved.fNum =  ttRespVIPUl.fNum 
                  NO-LOCK:
                     tt-cust-corp.okvwed$ = tt-cust-corp.okvwed$ + "," + 
                     SUBSTRING(ttRespVIPOkved.fOkved,1,LENGTH(ttRespVIPOkved.fOkved) - 1).
                  END.
                  tt-cust-corp.okvwed$ = TRIM(tt-cust-corp.okvwed$,",").
               END.
            END.
            WHEN "4" THEN
            DO:
               FIND FIRST ttRespVIPIP NO-LOCK NO-ERROR.
               IF AVAIL ttRespVIPIP THEN
               DO:
                  ASSIGN
                     tt-cust-corp.cust-stat      = GetCodeEx("����।�",
                                                             ttRespVIPIP.fCuststat,
                                                             tt-cust-corp.cust-stat)
                     tt-cust-corp.inn            = ttRespVIPIP.fInn  
                     tt-cust-corp.ogrn$          = ttRespVIPIP.fOGRN
                     tt-cust-corp.country-id     = ttRespVIPIP.fCountry
                     tt-cust-corp.name-corp      = 
                        ttRespVIPIP.fLastName + " " + ttRespVIPIP.fFirstName
                     tt-cust-corp.tax-insp       = ttRespVIPIP.fCodeNo
                  .
                  RUN SetSysConf IN h_base("EgrField",
                                           ttRespVIPIP.fRegGni + ";" 
                                         + ttRespVIPIP.fAddr   + ";"
                                         + ttRespVIPIP.fNumReg).

                  FOR EACH ttRespVIPOkved WHERE ttRespVIPOkved.fNum = ttRespVIPIP.fNum 
                  NO-LOCK:
                     tt-cust-corp.okvwed$ = tt-cust-corp.okvwed$ + "," + 
                     SUBSTRING(ttRespVIPOkved.fOkved,1,LENGTH(ttRespVIPOkved.fOkved) - 1).
                  END.
                  tt-cust-corp.okvwed$ = TRIM(tt-cust-corp.okvwed$,",").
               END. 
            END.
         END CASE.
         vAddress = TRIM(GetSysConf("EgrField")).
         IF {assigned vAddress} THEN DO:
            ASSIGN
               tt-cust-corp.kodreggni$ = ENTRY(1,vAddress,";")
               tt-cust-corp.addr-of-low[1] = ENTRY(2,vAddress,';')
               tt-cust-corp.kodreg$ = ENTRY(3,vAddress,";")
            .
            {cust-adr.obj 
               &addr-to-vars     = YES
               &tablefield       = "tt-cust-corp.addr-of-low[1]" 
            }
            ASSIGN
               vAdrIndInt :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = STRING(vAdrIndInt,"999999")           
               vOblChar   :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vOblChar    
               vGorChar   :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vGorChar  
               vPunktChar :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vPunktChar
               vUlChar    :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vUlChar   
               vDomChar   :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vDomChar  
               vKorpChar  :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vKorpChar 
               vKvChar    :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vKvChar
               vStrChar   :SCREEN-VALUE IN FRAME {&MAIN-FRAME} = vStrChar
               tt-cust-corp.addr-of-low[1]:SCREEN-VALUE IN FRAME {&MAIN-FRAME} = 
               tt-cust-corp.addr-of-low[1]
               tt-cust-corp.kodreggni$:SCREEN-VALUE IN FRAME {&MAIN-FRAME} = tt-cust-corp.kodreggni$
               tt-cust-corp.kodreg$:SCREEN-VALUE IN FRAME {&MAIN-FRAME} = tt-cust-corp.kodreg$
            . 
            FIND LAST cust-ident WHERE cust-ident.class-code      =  "p-cust-adr"
                             AND cust-ident.cust-code-type  =  "�����"
                             AND cust-ident.cust-cat        =  "�"
                             AND cust-ident.cust-id         =  tt-cust-corp.cust-id
                             AND cust-ident.open-date       <= gend-date
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAIL cust-ident THEN DO:
               cust-ident.issue = tt-cust-corp.addr-of-low[1].
               UpdateSigns("cust-ident", 
                            cust-ident.cust-code-type + "," + 
                            cust-ident.cust-code + "," + 
                            STRING(cust-ident.cust-type-num),
                            "���������", tt-cust-corp.kodreggni$, YES).
               UpdateSigns("cust-ident", 
                            cust-ident.cust-code-type + "," + 
                            cust-ident.cust-code + "," + 
                            STRING(cust-ident.cust-type-num),
                            "������", tt-cust-corp.kodreg$, YES).
            END.
            ELSE IF iMode = {&MOD_ADD} THEN DO:

               FIND FIRST tt-p-cust-adr EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
               IF NOT AVAIL(tt-p-cust-adr) THEN
               CREATE tt-p-cust-adr.
               ASSIGN
                  tt-p-cust-adr.class-code     =  "p-cust-adr"
                  tt-p-cust-adr.cust-cat       =  "�"
                  tt-p-cust-adr.open-date      = gend-date
                  tt-p-cust-adr.issue          = tt-cust-corp.addr-of-low[1]
                  tt-p-cust-adr.kodreggni$     = tt-cust-corp.kodreggni$
                  tt-p-cust-adr.kodreg$        = tt-cust-corp.kodreg$
                  tt-p-cust-adr.cust-code-type =  "�����"
                  tt-p-cust-adr.country-id     = tt-cust-corp.country-id
               NO-ERROR.

               /* �⮡� ��࠭����� �� */
               vHs = GetProperty(mInstance,"p-cust-adr","").

               IF VALID-HANDLE(vHs) THEN DO:
                 ASSIGN
                    vHs = vHs:BUFFER-VALUE
                    vHfl = vHs:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD("kodreg$")
                  .
                 IF VALID-HANDLE(vHfl) THEN
                    vHfl:PRIVATE-DATA = {&FORM-DEF} NO-ERROR.

                 vHfl = vHs:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD("kodreggni$").
                 IF VALID-HANDLE(vHfl) THEN
                    vHfl:PRIVATE-DATA = {&FORM-DEF} NO-ERROR.

                 vHfl = vHs:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD("country-id").
                 IF VALID-HANDLE(vHfl) THEN
                    vHfl:PRIVATE-DATA = {&FORM-DEF} NO-ERROR.
               END.
            END.
            FIND LAST cust-ident WHERE cust-ident.class-code     =  "p-cust-adr"
                                   AND cust-ident.cust-code-type =  "�����"
                                   AND cust-ident.cust-cat       =  "�"
                                   AND cust-ident.cust-id        =  tt-cust-corp.cust-id
                                   AND cust-ident.open-date      <= gend-date
            NO-LOCK NO-ERROR.
         END.
      END.
   END.

            /* ��� ०��� ��ᬮ�� ���ﭨ� �� ���� �饬 ���㠫��
            ** �� ��� ���� ���� �ய�᪨ */
   IF mMOD_VIEW_DATE
   THEN
      FIND LAST cust-ident WHERE cust-ident.class-code      =  "p-cust-adr"
                             AND cust-ident.cust-code-type  =  "�����"
                             AND cust-ident.cust-cat        =  "�"
                             AND cust-ident.cust-id         =  tt-cust-corp.cust-id
                             AND cust-ident.open-date       <= gend-date
      NO-LOCK NO-ERROR.
            /* �᫨ ���㠫�� ���� �ய�᪨ ������, �ᯮ��㥬 ���
            ** � �ଥ ��� �⮡ࠦ���� ���ﭨ� ����窨 �� ���� */
   IF AVAIL cust-ident THEN
   DO:
      /* ������ ���祭�� ���� �� �࠭�. */
      {cust-adr.obj 
         &addr-to-vars     = YES
         &addr-to-vars-gni = YES
         &tablefield       = "TRIM(cust-ident.issue)"
         &fieldgni         = "GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'���뀤��')"    
         
      }
      tt-cust-corp.kodreg$    = GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'������').
      tt-cust-corp.kodreggni$ = GetXAttrValue('cust-ident',cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),'���������').
   END.
   ELSE DO:
      /* ���뢠�� ���� ���� � ��६���� */
      {cust-adr.obj 
         &addr-to-vars     = YES
         &addr-to-vars-gni = YES
         &fieldgni         = "tt-cust-corp.kodyadresa$"    
         &tablefield       = "(IF ~{assigned TRIM(tt-cust-corp.addr-of-low[1])~} THEN TRIM(tt-cust-corp.addr-of-low[1]) ELSE TRIM(tt-cust-corp.addr-of-low[2]))"
      }
   END.

   IF iMode <> {&MOD_ADD} THEN 
      ASSIGN 
         vAdrCntry   = GetXattrValue("cust-corp",
                                     STRING(tt-cust-corp.cust-id),
                                     mAdrCntXattr)
         mUniqCodAdr = tt-cust-corp.unikkodadresa$
      .
   
   IF    iMode =  {&MOD_VIEW}
      OR iMode =  {&MOD_EDIT}
      THEN mBankClient = GetValueByQuery (
         "cust-role",
         "class-code",
         "        cust-role.cust-cat   EQ '�'" + 
         "  AND   cust-role.cust-id    EQ '" + STRING (tt-cust-corp.cust-id) + "'" +
         "  AND   cust-role.class-code EQ 'ImaginClient'"
      ) <> ?.
            
   mTmp = GetXattrEx("cust-corp","��଑���","Domain-Code").
   IF mTmp =  "" THEN
      mTmp = "��଑���".  
   mFormSobs = IF availCode(mTmp,tt-cust-corp.formsobs$) 
               THEN GetCodeName(mTmp,tt-cust-corp.formsobs$)
               ELSE "".    

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
   DEFINE VAR vCustId       LIKE cust-corp.cust-id NO-UNDO.
   DEFINE VAR vFNName       AS CHARACTER NO-UNDO.
   DEFINE BUFFER xcust-corp FOR cust-corp.
   DEFINE VAR vRowId        AS ROWID                  NO-UNDO.
   DEFINE VAR vXattrCode    LIKE xattr.xattr-code     NO-UNDO.
   DEFINE VAR vXattrName    LIKE xattr.name           NO-UNDO.
   DEFINE VAR vIsProgrField LIKE xattr.Progress-Field NO-UNDO.
   DEFINE VAR vMode         AS CHARACTER              NO-UNDO.
   DEFINE VAR vTmp          AS CHARACTER              NO-UNDO.
   DEFINE VAR vStatIP       AS CHARACTER              NO-UNDO.
   DEFINE VAR vChkByStopList AS LOGICAL               NO-UNDO.   
   DEFINE VAR vChkAdresses  AS CHARACTER              NO-UNDO.
   DEFINE VAR vTerrAdrType  AS CHARACTER              NO-UNDO.

   DEFINE BUFFER xcust      FOR cust-corp.
   DEFINE BUFFER cust-ident FOR cust-ident.
   DEFINE BUFFER cust-role  FOR cust-role.
   DEFINE BUFFER bcident    FOR cust-ident.
   
   IF LOGICAL(fGetSetting("CLI-EXPORT", "Cust-Corp", "NO")) =  YES THEN
      RUN createxmlorg.p(tt-cust-corp.cust-id).

   vCustId = INT64(GetSurrogate("cust-corp", TO-ROWID(GetInstanceProp2(mInstance,"__rowid")))).

   /* (������ �� ����) ���⠭���� ��砫쭮�� ����� */
   UpdateSigns("cust-corp",
               STRING(vCustId),
               "status",
               FGetSetting("���₢����", "�������", ?),
               ?).

   UpdateSigns("cust-corp",
               STRING(vCustId),
               mAdrCntXattr,
               vAdrCntry,
               ?).
   IF mUpdEgr THEN
   DO:
      CASE mTypeEgr:
         WHEN "1" THEN
         DO:
            FIND FIRST ttRespKSUL NO-LOCK NO-ERROR.
            IF AVAIL ttRespKSUL THEN
            DO:
               UpdateSigns("cust-corp", STRING(vCustId), "��⠎���", ttRespKSUL.fDateOGRN, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "��⠢���", ttRespKSUL.fUstSum, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "����࣠�", ttRespKSUL.fNameNO, ?).
            END.
         END.
         WHEN "2" THEN
         DO:
            FIND FIRST ttRespKSIP NO-LOCK NO-ERROR.
            IF AVAIL ttRespKSIP THEN
            DO:
               UpdateSigns("cust-corp", STRING(vCustId), "��⠎���", ttRespKSIP.fDateOGRN, ?).
               IF ttRespKSIP.fTypeIP THEN
                  UpdateSigns("cust-corp", STRING(vCustId), "�।��", "�।��", ?).
            END.
         END.
         WHEN "3" THEN
         DO:
            FIND FIRST ttRespVIPUl NO-LOCK NO-ERROR.
            IF AVAIL ttRespVIPUl THEN
            DO:
               UpdateSigns("cust-corp", STRING(vCustId), "��⠎���",    ttRespVIPUl.fDateOGRN, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "�焮���",     ttRespVIPUl.fSvidet, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "brand-name",  ttRespVIPUl.fBrandName, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "RegDate",     ttRespVIPUl.fRegDate, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "����࣠�",    ttRespVIPUl.fNameNO, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "RegNum",      ttRespVIPUl.fRegNum, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "���ࣈ��",    ttRespVIPUl.fReorgInn, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "���࣏�����", ttRespVIPUl.fReorgName, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "���࣎���",   ttRespVIPUl.fReorgOgrn, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "��⠢���",    ttRespVIPUl.fUstSum, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "�焮����",   ttRespVIPUl.fDateNO, ?).
               FOR EACH ttRespVIPLicence WHERE ttRespVIPLicence.fNum =  ttRespVIPUl.fNum NO-LOCK:
                  CREATE cust-ident.
                  ASSIGN
                     cust-ident.class-code     = "cust-lic"
                     cust-ident.cust-cat       = "�"
                     cust-ident.cust-id        = vCustId
                     cust-ident.cust-code-type = IF {assigned ttRespVIPLicence.fVid} THEN 
                                                 ttRespVIPLicence.fVid ELSE "0"
                     cust-ident.open-date      = ttRespVIPLicence.fDateBeg
                     cust-ident.close-date     = ttRespVIPLicence.fDateEnd
                     cust-ident.issue          = ttRespVIPLicence.fIssue
                     cust-ident.cust-code      = ttRespVIPLicence.NumbLic
                  .
                  FIND FIRST cust-ident WHERE cust-ident.cust-cat   =  "�"
                                          AND cust-ident.cust-id    =  vCustId
                                          AND cust-ident.class-code =  "cust-lic" 
                                          AND cust-ident.cust-code  =  ttRespVIPLicence.NumbLic
                                          AND cust-ident.open-date  =  ttRespVIPLicence.fDateBeg
                     NO-LOCK NO-ERROR.
                  IF AVAIL cust-ident THEN
                     UpdateSigns ("cust-lic",
                                  STRING(cust-ident.cust-code-type) + ',' +
                                  STRING(cust-ident.cust-code) + ',' +
                                  STRING(cust-ident.cust-type-num),
                                  "�����業�", ttRespVIPLicence.NameLic,?).

               END.
               FOR EACH ttRespVIPPred WHERE ttRespVIPPred.fNum =  ttRespVIPUl.fNum NO-LOCK:
                  CREATE cust-role. 
                  CASE ttRespVIPPred.fType:
                     WHEN "��ࠢ���" THEN
                        ASSIGN
                           cust-role.Class-Code     = "�।�⠢�⥫�"
                           cust-role.surrogate      = STRING(vCustId)
                           cust-role.file-name      = "cust-corp"
                           cust-role.open-date      = 
                              IF ttRespVIPPred.fDateBeg =  ? THEN TODAY
                              ELSE ttRespVIPPred.fDateBeg
                           cust-role.cust-cat       = "�"
                           cust-role.inn            = ttRespVIPPred.fInnFL
                           cust-role.cust-name      = ttRespVIPPred.fName
                           cust-role.cust-code-type = ttRespVIPPred.fCodeDoc
                           cust-role.cust-code      = ttRespVIPPred.fSerDoc
                           cust-role.address        = 
                              IF {assigned ttRespVIPPred.fAddrRus} THEN 
                                   ttRespVIPPred.fAddrRus
                              ELSE ttRespVIPPred.fAddrIn
                        . 
                     WHEN "��ࠢ���" THEN
                        ASSIGN
                           cust-role.Class-Code = "�।�⠢�⥫�"
                           cust-role.surrogate  = STRING(vCustId)
                           cust-role.file-name  = "cust-corp"
                           cust-role.open-date  = 
                              IF ttRespVIPPred.fDateBeg =  ? THEN TODAY
                              ELSE ttRespVIPPred.fDateBeg
                           cust-role.cust-cat   = "�"
                           cust-role.inn        = ttRespVIPPred.fInnFL
                           cust-role.cust-name  = ttRespVIPPred.fName
                           cust-role.country-id = ttRespVIPPred.fCountry
                           cust-role.address    = ttRespVIPPred.fAddrRus
                        .                       
                     WHEN "��ࠢ����" THEN
                        ASSIGN
                           cust-role.Class-Code     = "�।�⠢�⥫�"
                           cust-role.surrogate      = STRING(vCustId)
                           cust-role.file-name      = "cust-corp"
                           cust-role.open-date      = 
                              IF ttRespVIPPred.fDateBeg =  ? THEN TODAY
                              ELSE ttRespVIPPred.fDateBeg
                           cust-role.cust-cat       = "�"
                           cust-role.inn            = ttRespVIPPred.fInnFL
                           cust-role.cust-name      = ttRespVIPPred.fName
                           cust-role.cust-code-type = "����"
                           cust-role.cust-code      = ttRespVIPPred.fOGRN
                        .  
                     WHEN "��।�┋" THEN
                     DO:
                        ASSIGN
                           cust-role.Class-Code     = "��।�⥫�"
                           cust-role.surrogate      = STRING(vCustId)
                           cust-role.file-name      = "cust-corp"
                           cust-role.open-date      = 
                              IF ttRespVIPPred.fDateBeg =  ? THEN TODAY
                              ELSE ttRespVIPPred.fDateBeg
                           cust-role.cust-cat       = "�"
                           cust-role.inn            = ttRespVIPPred.fInnFL
                           cust-role.cust-name      = ttRespVIPPred.fName
                           cust-role.cust-code-type = ttRespVIPPred.fCodeDoc
                           cust-role.cust-code      = ttRespVIPPred.fSerDoc
                           cust-role.address        = 
                              IF {assigned ttRespVIPPred.fAddrRus} THEN 
                                   ttRespVIPPred.fAddrRus
                              ELSE ttRespVIPPred.fAddrIn
                        .
                        UpdateSigns (cust-role.Class-Code,
                                     STRING(cust-role.cust-role-id),
                                     "����",ttRespVIPPred.fProcPart,?). 
                     END.
                     WHEN "��।�∍" THEN
                     DO:
                        ASSIGN
                           cust-role.Class-Code = "��।�⥫�"
                           cust-role.surrogate  = STRING(vCustId)
                           cust-role.file-name  = "cust-corp"
                           cust-role.open-date  = 
                              IF ttRespVIPPred.fDateBeg =  ? THEN TODAY
                              ELSE ttRespVIPPred.fDateBeg
                           cust-role.cust-cat   = "�"
                           cust-role.inn        = ttRespVIPPred.fInnFL
                           cust-role.cust-name  = ttRespVIPPred.fName
                           cust-role.country-id = ttRespVIPPred.fCountry
                        .  
                        UpdateSigns (cust-role.Class-Code,
                                     STRING(cust-role.cust-role-id),
                                     "����",ttRespVIPPred.fProcPart,?).
                     END.
                     WHEN "��।�␮�" THEN
                     DO:
                        ASSIGN
                           cust-role.Class-Code     = "��।�⥫�"
                           cust-role.surrogate      = STRING(vCustId)
                           cust-role.file-name      = "cust-corp"
                           cust-role.open-date      = 
                              IF ttRespVIPPred.fDateBeg =  ? THEN TODAY
                              ELSE ttRespVIPPred.fDateBeg
                           cust-role.cust-cat       = "�"
                           cust-role.inn            = ttRespVIPPred.fInnFL
                           cust-role.cust-name      = ttRespVIPPred.fName
                           cust-role.cust-code-type = "����"
                           cust-role.cust-code      = ttRespVIPPred.fOGRN
                        .
                        UpdateSigns (cust-role.Class-Code,
                                     STRING(cust-role.cust-role-id),
                                     "����",ttRespVIPPred.fProcPart,?).
                     END.
                  END CASE.
               END.
            END.
         END.
         WHEN "4" THEN
         DO:
            FIND FIRST ttRespVIPIP NO-LOCK NO-ERROR.
            IF AVAIL ttRespVIPIP THEN
            DO:
               UpdateSigns("cust-corp", STRING(vCustId), "��⠎���",    ttRespVIPIP.fDateOGRN, ?).
               IF ttRespVIPIP.fTypeIP THEN
                  UpdateSigns("cust-corp", STRING(vCustId), "�।��", "�।��", ?).
               UpdateSigns("cust-corp", STRING(vCustId), "�焮���",     ttRespVIPIP.fSvidet, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "RegDate",     ttRespVIPIP.fRegDate, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "����࣠�",    ttRespVIPIP.fRegName, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "RegNum",      ttRespVIPIP.fRegNum, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "�焮����",   ttRespVIPIP.fDateNo, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "birthday",    ttRespVIPIP.fDateBir, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "birthplace",  ttRespVIPIP.fPlaceBir, ?).
               UpdateSigns("cust-corp", STRING(vCustId), "engl-name",   ttRespVIPIP.fEngName, ?).
               FOR EACH ttRespVIPLicence WHERE ttRespVIPLicence.fNum =  ttRespVIPIP.fNum NO-LOCK:
                  CREATE cust-ident.
                  ASSIGN
                     cust-ident.class-code     = "cust-lic"
                     cust-ident.cust-cat       = "�"
                     cust-ident.cust-id        = vCustId
                     cust-ident.cust-code-type = ttRespVIPLicence.fVid
                     cust-ident.open-date      = ttRespVIPLicence.fDateBeg
                     cust-ident.close-date     = ttRespVIPLicence.fDateEnd
                     cust-ident.issue          = ttRespVIPLicence.fIssue
                     cust-ident.cust-code      = ttRespVIPLicence.NumbLic
                  .
                  FIND FIRST cust-ident WHERE cust-ident.cust-cat   =  "�"
                                          AND cust-ident.cust-id    =  vCustId
                                          AND cust-ident.class-code =  "cust-lic" 
                                          AND cust-ident.cust-code  =  ttRespVIPLicence.NumbLic
                                          AND cust-ident.open-date  =  ttRespVIPLicence.fDateBeg
                     NO-LOCK NO-ERROR.
                  IF AVAIL cust-ident THEN
                     UpdateSigns ("cust-lic",
                                  STRING(cust-ident.cust-code-type) + ',' +
                                  STRING(cust-ident.cust-code) + ',' +
                                  STRING(cust-ident.cust-type-num),
                                  "�����業�", ttRespVIPLicence.NameLic,?).

               END.
               IF {assigned ttRespVIPIP.fDocNum} THEN
               DO:
                  CREATE cust-ident.
                  ASSIGN
                     cust-ident.class-code     = "p-cust-ident"
                     cust-ident.cust-cat       = "�"
                     cust-ident.cust-id        = vCustId
                     cust-ident.cust-code-type = ttRespVIPIP.fDocCodeVd
                     cust-ident.cust-code      = ttRespVIPIP.fDocNum
                     cust-ident.open-date      = ttRespVIPIP.fDocDateOp
                     cust-ident.issue          = ttRespVIPIP.fDocIssue
                  .
                  FIND FIRST cust-ident WHERE cust-ident.cust-cat   =  "�"
                                          AND cust-ident.cust-id    =  vCustId
                                          AND cust-ident.class-code =  "p-cust-ident" 
                                          AND cust-ident.cust-code  =  ttRespVIPIP.fDocNum
                                          AND cust-ident.open-date  =  ttRespVIPIP.fDocDateOp
                     NO-LOCK NO-ERROR.
                  IF AVAIL cust-ident THEN
                     UpdateSigns ("p-cust-ident",
                                  STRING(cust-ident.cust-code-type) + ',' +
                                  STRING(cust-ident.cust-code) + ',' +
                                  STRING(cust-ident.cust-type-num),
                                  "���ࠧ�", ttRespVIPIP.fDocBranch,?).
               END.
            END.
         END.
      END CASE.
      RUN SetSysConf IN h_base("EgrUpdate","").
      RUN SetSysConf IN h_base("EgrField","").
   END.

   IF CAN-FIND(FIRST cust-role WHERE
                     cust-role.cust-cat   =  "�"
                 AND cust-role.cust-id    =  STRING(tt-cust-corp.cust-id)
                 AND cust-role.Class-Code =  "ImaginClient"
                 AND cust-role.file-name  =  "branch"                                  
                 AND cust-role.surrogate  <> shFilial
                     USE-INDEX cust-id 
                     NO-LOCK) AND
   NOT CAN-FIND(FIRST cust-role WHERE
                     cust-role.cust-cat   =  "�"
                 AND cust-role.cust-id    =  STRING(tt-cust-corp.cust-id)
                 AND cust-role.Class-Code =  "ImaginClient"
                 AND cust-role.file-name  =  "branch"                                  
                 AND cust-role.surrogate  =  shFilial
                     USE-INDEX cust-id 
                     NO-LOCK) THEN DO:

      pick-value = "no".
      RUN Fill-SysMes ("", "", "4", "������� ஫� ImaginClient � ⥪�饬 䨫����?").
      IF pick-value =  "yes" THEN
      RUN SetClientRole IN h_cust (
         mInstance:DEFAULT-BUFFER-HANDLE, "�", mBankClient)NO-ERROR.
   END.
   ELSE  /* ��⠭�������� ஫� ������. */
   RUN SetClientRole IN h_cust (mInstance:DEFAULT-BUFFER-HANDLE, 
                                "�", 
                                mBankClient) NO-ERROR.               
               
   vRowId = TO-ROWID(GetInstanceProp(mInstance,"__rowid")).
   FIND FIRST xcust WHERE ROWID(xcust) = vRowId NO-LOCK NO-ERROR.
   IF AVAIL xcust THEN DO:   
      /* �஢�ઠ �᭮���� ४����⮢ */ 
      RUN GetFirstUnassignedFieldManByRole IN h_cust ("cust-corp",
                                                      mInstance:DEFAULT-BUFFER-HANDLE,
                                                      "main",
                                                      OUTPUT vXattrCode,
                                                      OUTPUT vXattrName).
      IF     {assigned vXattrCode} THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0",
                  "�� �������� �᭮���� ��易⥫�� ४����� """ + vXattrCode +
                  """ (" + vXattrName + ")."
                  ).
         RETURN ERROR.
      END.
      
   /* �஢�ઠ �������⥫��� ४����⮢ */ 
      RUN GetFirstUnassignedFieldManByRole IN h_cust ("cust-corp",
                                                      mInstance:DEFAULT-BUFFER-HANDLE,
                                                      "dp",
                                                      OUTPUT vXattrCode,
                                                      OUTPUT vXattrName).   
       IF     {assigned vXattrCode} THEN DO:                                                    
          mRetValFlag = '1'.
       END.      
   END.            

   {chk_frm_mand_adr.i
      &cust-type = "'�'"
      &cust-id   = "vCustId"
   }

   RUN GetCustNameFormatted IN h_cust ("�",STRING(tt-cust-corp.cust-id),OUTPUT vFNName).
   IF LENGTH(vFNName) > 160 THEN DO:
      RUN Fill-SysMes IN h_tmess ("","","1","��������! � ������� ������ ������ ������������ ����� �ॢ���� 160 ᨬ�����. ��⠭���� �ࠢ���� ������ �ନ஢���� ������������ �� ����� �������⥫쭮�� ४����� ""��ଠ⍠��""!").
   END.

   /*�஢�ઠ �� �⮯ ���⠬*/
   /*��饭�� ⠡��� �� �஢�થ �� �⮯-���⠬*/
   {empty tt-view-sl}
   {empty ttFindSL}
   {empty t-obj-sl N}
   {empty ttSLAction}

   IF FGetSetting("�⮯-�����", "ContrSub","") = "��" THEN 
   DO:      
/*      IF ChkClientByStopList_new ("cust-corp",                                                   */
/*                              STRING(vCustId),                                                   */
/*                              TRIM(tt-cust-corp.cust-stat) + " " + TRIM(tt-cust-corp.name-corp), */
/*                              TRIM(tt-cust-corp.cust-stat) + " " + TRIM(tt-cust-corp.name-short),*/
/*                              tt-cust-corp.inn,                                                  */
/*                              "",                                                                */
/*                              tt-cust-corp.birthday,                                             */
/*                              tt-cust-corp.birthplace,                                           */
/*                              "������������_��",                                                 */
/*                              "CLNT") THEN                                                       */
/*         vChkByStopList = YES.                                                                   */
/*                                                                                                 */
/*      IF vChkByStopList THEN                                                                     */
/*      DO:                                                                                        */
/*         {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170          */
/*                    &nodef   = "/**/"}                                                             */
/*         RUN PrintClientSLRep IN THIS-PROCEDURE.                                                 */
/*         {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"}                     */
/*         vMode =   IF iMode =  {&MOD_ADD} THEN "create" ELSE "edit".                             */
/*         IF NOT StopListAction("cust-corp",STRING(vCustId), vMode) THEN                          */
/*            RETURN ERROR.                                                                        */
/*      END.                                                                                       */
   END.
   ELSE DO:
   IF ChkClientByStopList ("cust-corp",
                           STRING(vCustId),
                           TRIM(tt-cust-corp.cust-stat) + " " + TRIM(tt-cust-corp.name-corp),
                           TRIM(tt-cust-corp.cust-stat) + " " + TRIM(tt-cust-corp.name-short),
                           tt-cust-corp.inn,
                           "",
                           tt-cust-corp.birthday,
                           tt-cust-corp.birthplace,
                           "������������_��",
                           "CLNT") THEN
      vChkByStopList = YES.

   IF vChkByStopList THEN 
   DO: 
      {setdest.i &stream = "stream rep" &filename = "'chkstoplist.tmp'"  &cols = 170 &nodef   = "/*"}
      RUN PrintClientSLRep. 
      {preview.i  &stream = "stream rep" &filename = "'chkstoplist.tmp'"}
      vMode =   IF iMode =  {&MOD_ADD} THEN "create" ELSE "edit".      
      IF NOT StopListAction("cust-corp",STRING(vCustId), vMode) THEN
         RETURN ERROR. 
   END.    
   END.

   IF  iMode =  {&MOD_ADD} THEN
   DO:
      IF ENTRY(1,fGetSetting("Cl_TerrAdr","AddrRedact","")) =  "��"   
      THEN DO:
         vTerrAdrType = fGetSetting("Cl_TerrAdr","AddrType","").
   
         FOR EACH bcident WHERE 
                  bcident.cust-cat       =  "�"
              AND bcident.cust-id        =  tt-cust-corp.cust-id
              AND bcident.Class-code     =  "p-cust-adr"
              AND (   bcident.close-date =  ? 
                   OR bcident.close-date >= TODAY)
              AND CAN-DO(vTerrAdrType,bcident.cust-code-type)
         NO-LOCK:
           {additem.i vChkAdresses bcident.cust-code-type}
         END.
         
         RUN chkterradred.p ("�",
                             STRING(tt-cust-corp.cust-id),
                             vChkAdresses).
      END.
   END.

   /* �஢�ઠ ����⢨⥫쭮�� ��ᯮ�� �� ������ �� ���� ��� */
   ASSIGN
      vTmp    = GetXattrValueEX("cust-corp", STRING(vCustId), "�।��", "")
      vStatIP = FGetSetting("�⠭���","����ᔋ��","")
   .
   IF FGetSetting("��������","������_���","") =  "��" AND
      (vTmp <> "" OR CAN-DO(vStatIP,tt-cust-corp.cust-stat)) 
   THEN DO:
      {cl-fmschk.i
         "'cust-corp'"
         "tt-cust-corp.cust-id"
         "GetXattrValueEX('cust-corp', STRING(vCustId), 'document-id', '')"
         "GetXattrValueEX('cust-corp', STRING(vCustId), 'document', '')"
         "GetXattrValueEX('cust-corp', STRING(vCustId), 'Document4Date_vid', '')"
         "tt-cust-corp.name-corp"
      }
      IF FGetSetting("��������","�������_���","") = "��" THEN
      DO:
         IF AVAILABLE tt-res THEN DO:
            IF CAN-DO({&KodIsNotValidDoc},tt-res.fChkKodSt) THEN
            DO:   
               APPLY "ENTRY" TO mBankClient IN FRAME fMain.
               RETURN ERROR.
            END.    
         END. 
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateObjectLocal TERMINAL-SIMULATION 
PROCEDURE ValidateObjectLocal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR vRecId AS RECID NO-UNDO. /* ��� �맮�� ��⮤� CHKUPD. */

   vRecId = Rowid2Recid ("cust-corp",
                        TO-ROWID(GetInstanceProp(mInstance,"__rowid"))).            
   
   /* ����� ��⮤� chkupd. */
   RUN RunClassMethod IN h_xclass (
      "cust-corp",
      "chkupd",
      "",
      "",
      "cust-req",
      STRING (vRecId)
   ) NO-ERROR.
   IF    ERROR-STATUS:ERROR
      OR RETURN-VALUE <> ""
      THEN RETURN ERROR.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTFILE='f-cust.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/' */
/* $LINTUSER='kuds' */
/* $LINTDATE='22/01/2018 13:37:58.650+03:00' */
/*prosignlaeJSrSz7eE05CVaPKjn7w*/