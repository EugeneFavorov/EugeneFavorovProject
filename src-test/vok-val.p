&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-op-entry1 NO-UNDO LIKE op-entry
       FIELD datanal$ AS DATE /* ��⠍�� */
       FIELD zks$ AS INT64 /* ��� */
       FIELD nomdog$ AS CHARACTER /* ������ */
       FIELD oblsumm$ AS DECIMAL /* ����㬬 */
       FIELD spod$ AS LOGICAL /* ���� */
       FIELD spodpribubyt$ AS CHARACTER /* �����ਡ���� */
       FIELD strana$ AS CHARACTER /* ��࠭� */
       FIELD sumnds$ AS DECIMAL /* �㬍�� */
       FIELD f401swcet$ AS CHARACTER /* �401��� */
       FIELD cenamc$ AS DECIMAL /* ������ */
       FIELD CenterCR AS CHARACTER /* CenterCR */
       FIELD CenterDR AS CHARACTER /* CenterDR */
       FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
       FIELD DetailsCR AS CHARACTER /* DetailsCR */
       FIELD DetailsDR AS CHARACTER /* DetailsDR */
       FIELD exc_603 AS LOGICAL /* exc_603 */
       FIELD f134 AS CHARACTER /* f134 */
       FIELD form-type-code AS CHARACTER /* form-type-code */
       FIELD op-entry-link AS CHARACTER /* op-entry-link */
       FIELD VDateCR AS DATE /* VDateCR */
       FIELD VDateDR AS DATE /* VDateDR */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-op-entry1" "op-entry1" }
       .
DEFINE TEMP-TABLE tt-op-entry2 NO-UNDO LIKE op-entry
       FIELD datanal$ AS DATE /* ��⠍�� */
       FIELD zks$ AS INT64 /* ��� */
       FIELD nomdog$ AS CHARACTER /* ������ */
       FIELD oblsumm$ AS DECIMAL /* ����㬬 */
       FIELD spod$ AS LOGICAL /* ���� */
       FIELD spodpribubyt$ AS CHARACTER /* �����ਡ���� */
       FIELD strana$ AS CHARACTER /* ��࠭� */
       FIELD sumnds$ AS DECIMAL /* �㬍�� */
       FIELD f401swcet$ AS CHARACTER /* �401��� */
       FIELD cenamc$ AS DECIMAL /* ������ */
       FIELD CenterCR AS CHARACTER /* CenterCR */
       FIELD CenterDR AS CHARACTER /* CenterDR */
       FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
       FIELD DetailsCR AS CHARACTER /* DetailsCR */
       FIELD DetailsDR AS CHARACTER /* DetailsDR */
       FIELD exc_603 AS LOGICAL /* exc_603 */
       FIELD f134 AS CHARACTER /* f134 */
       FIELD form-type-code AS CHARACTER /* form-type-code */
       FIELD op-entry-link AS CHARACTER /* op-entry-link */
       FIELD VDateCR AS DATE /* VDateCR */
       FIELD VDateDR AS DATE /* VDateDR */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-op-entry2" "op-entry2" }
       .
DEFINE TEMP-TABLE tt-vok-val NO-UNDO LIKE op
       FIELD CardStatus AS CHARACTER /* CardStatus */
       FIELD akkrvid$ AS CHARACTER /* ������� */
       FIELD akkrno$ AS CHARACTER /* ������ */
       FIELD osnzawc$ AS CHARACTER /* �ᭇ�� */
       FIELD vswcetobesp$ AS LOGICAL /* ���⎡�� */
       FIELD f350_dec AS LOGICAL /* f350_dec */
       FIELD adres$ AS CHARACTER /* ���� */
       FIELD vidkursa$ AS CHARACTER /* ������� */
       FIELD vidopnalv$ AS CHARACTER /* ��������� */
       FIELD vkl113i$ AS LOGICAL /* ���113� */
       FIELD kodreggni$ AS CHARACTER /* ��������� */
       FIELD vklad$ AS CHARACTER /* ����� */
       FIELD dataotmbpol$ AS DATE /* ��⠎⬁��� */
       FIELD datapldok$ AS DATE /* ��⠏���� */
       FIELD datapomewteniwavkart$ AS DATE /* ��⠏���饭���� */
       FIELD dover$ AS LOGICAL /* ����� */
       FIELD dovlico$ AS CHARACTER /* ������ */
       FIELD dokum$ AS CHARACTER /* ���� */
       FIELD kbk$ AS CHARACTER /* ��� */
       FIELD kmpr$ AS LOGICAL /* ���� */
       FIELD kodneobywc$ AS CHARACTER /* ��������� */
       FIELD kodopval117$ AS CHARACTER /* ��������117 */
       FIELD kodopotmyv$ AS CHARACTER /* �������� */
       FIELD kodstsm$ AS CHARACTER /* ����⑬ */
       FIELD kop$ AS CHARACTER /* ��� */
       FIELD nomdog$ AS CHARACTER /* ������ */
       FIELD nomdok$ AS CHARACTER /* ������ */
       FIELD nomerpawcki$ AS CHARACTER /* �������窨 */
       FIELD nomerreestra$ AS INT64 /* ����������� */
       FIELD nomreestr$ AS CHARACTER /* ��������� */
       FIELD nomwcpl$ AS INT64 /* ������ */
       FIELD okato-nalog$ AS CHARACTER /* �����-����� */
       FIELD ostpl$ AS DECIMAL /* ��⏫ */
       FIELD p106n_statplat$ AS CHARACTER /* �106�_��⏫�� */
       FIELD pasportsdelki$ AS CHARACTER /* ��ᯮ�⑤���� */
       FIELD platelw#wtik$ AS CHARACTER /* ���⥫�騪 */
       FIELD platinstr$ AS CHARACTER /* ���∭��� */
       FIELD podrazd$ AS  CHARACTER /* ���৤ */
       FIELD podozdokument$ AS CHARACTER /* ��������㬥�� */
       FIELD pokdd$ AS CHARACTER /* ����� */
       FIELD poknd$ AS CHARACTER /* ����� */
       FIELD poknp$ AS CHARACTER /* ����� */
       FIELD pokop$ AS CHARACTER /* ����� */
       FIELD pokst$ AS CHARACTER /* ����� */
       FIELD poktp$ AS CHARACTER /* ����� */
       FIELD poluwcatelw#$ AS CHARACTER /* �����⥫� */
       FIELD predplatelw#wtika$ AS CHARACTER /* �।���⥫�騪� */
       FIELD predpoluwcatelwa$ AS CHARACTER /* �।�����⥫� */
       FIELD pfwepd$ AS LOGICAL /* ����� */
       FIELD pcstatus$ AS CHARACTER /* ������� */
       FIELD rasporwaditelw#$ AS CHARACTER /* ��ᯮ�廊⥫� */
       FIELD sisdenper$ AS CHARACTER /* ��ᄥ���� */
       FIELD spisswceta$ AS DATE /* ������ */
       FIELD srkakc$ AS INT64 /* �઀�� */
       FIELD statrash$ AS CHARACTER /* ��␠�� */
       FIELD summavalkontr$ AS CHARACTER /* �㬬��������� */
       FIELD summaobwazprodawzi$ AS DECIMAL /* �㬬���紐த��� */
       FIELD tipavansa$ AS CHARACTER /* �������� */
       FIELD tipoper$ AS CHARACTER /* ������� */
       FIELD uis-wed$ AS CHARACTER /* ���-�� */
       FIELD uslopl$ AS CHARACTER /* �ᫎ�� */
       FIELD uwcastnik$ AS CHARACTER /* ���⭨� */
       FIELD fio$ AS CHARACTER /* ��� */
       FIELD wsifrpl$ AS CHARACTER /* ������ */
       FIELD weksport$ AS LOGICAL /* ��ᯮ�� */
       FIELD acct-cr AS CHARACTER /* acct-cr */
       FIELD acct-db AS CHARACTER /* acct-db */
       FIELD acct-rec AS CHARACTER /* acct-rec */
       FIELD acct-send AS CHARACTER /* acct-send */
       FIELD acctbal AS CHARACTER /* acctbal */
       FIELD acctcorr AS CHARACTER /* acctcorr */
       FIELD amt-cur AS DECIMAL /* amt-cur */
       FIELD amt-cur-debt AS DECIMAL /* amt-cur-debt */
       FIELD amt-rub AS DECIMAL /* amt-rub */
       FIELD ARDBI AS DATE /* ARDBI */
       FIELD bank-country-rec AS CHARACTER /* bank-country-rec */
       FIELD bank-country-send AS CHARACTER /* bank-country-send */
       FIELD card-id AS CHARACTER /* card-id */
       FIELD commission AS CHARACTER /* commission */
       FIELD Contract-Code AS CHARACTER /* Contract-Code */
       FIELD country-pers AS CHARACTER /* country-pers */
       FIELD country-rec AS CHARACTER /* country-rec */
       FIELD country-send AS CHARACTER /* country-send */
       FIELD ctrans-id AS CHARACTER /* ctrans-id */
       FIELD cust-cat-rec AS CHARACTER /* cust-cat-rec */
       FIELD cust-doc-who AS CHARACTER /* cust-doc-who */
       FIELD DBI AS CHARACTER /* DBI */
       FIELD dealing-id AS DECIMAL /* dealing-id */
       FIELD descr AS CHARACTER /* descr */
       FIELD diasoft-id AS CHARACTER /* diasoft-id */
       FIELD document-id AS CHARACTER /* document-id */
       FIELD dpr-id AS INTEGER /* dpr-id */
       FIELD EL-DOC-DATE AS DATE /* EL-DOC-DATE */
       FIELD exc_407 AS LOGICAL /* exc_407 */
       FIELD exp-date AS CHARACTER /* exp-date */
       FIELD f102_cur AS CHARACTER /* f102_cur */
       FIELD f401_op AS CHARACTER /* f401_op */
       FIELD F407 AS CHARACTER /* F407 */
       FIELD f646-648 AS CHARACTER /* f646-648 */
       FIELD file-name AS CHARACTER /* file-name */
       FIELD flag-comm AS LOGICAL /* flag-comm */
       FIELD form-num AS CHARACTER /* form-num */
       FIELD form-seria AS CHARACTER /* form-seria */
       FIELD FormBehavior AS CHARACTER /* FormBehavior */
       FIELD HistoryFields AS CHARACTER /* HistoryFields */
       FIELD inn-rec AS CHARACTER /* inn-rec */
       FIELD inn-send AS CHARACTER /* inn-send */
       FIELD Isn AS CHARACTER /* Isn */
       FIELD KNFRoleName AS CHARACTER /* KNFRoleName */
       FIELD Kpp-rec AS CHARACTER /* Kpp-rec */
       FIELD Kpp-send AS CHARACTER /* Kpp-send */
       FIELD LegTerr AS CHARACTER /* LegTerr */
       FIELD link-op AS CHARACTER /* link-op */
       FIELD link-op-sum AS INT64 /* link-op-sum */
       FIELD Mark-Cli-Bank AS LOGICAL /* Mark-Cli-Bank */
       FIELD name-rec AS CHARACTER /* name-rec */
       FIELD name-send AS CHARACTER /* name-send */
       FIELD NUM-RKC AS CHARACTER /* NUM-RKC */
       FIELD op-bal AS CHARACTER /* op-bal */
       FIELD op-reference AS CHARACTER /* op-reference */
       FIELD Opcion AS CHARACTER /* Opcion */
       FIELD OperNum AS INT64 /* OperNum */
       FIELD reference-mci AS CHARACTER /* reference-mci */
       FIELD sec-code-type AS CHARACTER /* sec-code-type */
       FIELD ServInst-Ref AS CHARACTER /* ServInst-Ref */
       FIELD SettNbr AS CHARACTER /* SettNbr */
       FIELD signature AS CHARACTER /* signature */
       FIELD SLINF AS CHARACTER /* SLINF */
       FIELD sprate AS CHARACTER /* sprate */
       FIELD swift-acct-op AS CHARACTER /* swift-acct-op */
       FIELD swift-det-cod AS CHARACTER /* swift-det-cod */
       FIELD swift-det-inf AS CHARACTER /* swift-det-inf */
       FIELD swift-det-pay AS CHARACTER /* swift-det-pay */
       FIELD swift-mes-ben AS CHARACTER /* swift-mes-ben */
       FIELD tarif-RKC AS CHARACTER /* tarif-RKC */
       FIELD TicketNum AS CHARACTER /* TicketNum */
       FIELD time-stamp AS CHARACTER /* time-stamp */
       FIELD trans-id AS CHARACTER /* trans-id */
       FIELD TrCodeCr AS CHARACTER /* TrCodeCr */
       FIELD TrCodeDB AS CHARACTER /* TrCodeDB */
       FIELD u1557_exc AS LOGICAL /* u1557_exc */
       FIELD u1557_SysPer AS LOGICAL /* u1557_SysPer */
       FIELD vop-code AS CHARACTER /* vop-code */
       FIELD Birthday AS DATE /* Birthday */
       FIELD Document4Date_vid AS DATE /* Document4Date_vid */
       FIELD BirthPlace AS CHARACTER /* BirthPlace */
       FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
       FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
       FIELD local__id       AS INT64   /* �����䨪��� �����     */
       FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
       FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
       /* Additional fields you should place here                      */
       
       /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
       {ln-tthdl.i "tt-vok-val" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: VOK-VAL.P
      Comment: 26301
   Parameters:
         Uses:
      Used by:
      Created: 27.02.2004 18:07 kolal
     Modified: 02.03.2004 10:11 kolal    26301
     Modified: 06.03.2004 17:58 rija     24148: ����������� ���ᮢ�� ������ "���㯪� ������".
     Modified: 09.03.2004 15:56 kolal    ��ࠢ���� ���� ������ �� leave. ��� 26301.
     Modified: 09.03.2004 16:01 kolal    26301
     Modified: 12.03.2004 12:42 rija     24152 : ����������� ���ᮢ�� ������ "��������".
     Modified: 09.04.2004 17:14 kolal    26301
     Modified: 13.04.2004 16:38 rija     24152: ����������� ���ᮢ�� ������ "��������".
     Modified: 14.04.2004 12:51 kolal    26301
     Modified: 23.08.2004 15:31 ligp     32410: ���������� ���� "����७�����" � �࠭��� �ଠ�
                                         ����� �� ���ᮢ� ������
     Modified: 31.08.2004 15:45 ligp     34485: �㡫�஢���� ���. ४����⮢
     Modified: 07.09.2004 17:36 ligp     31282: 113-� �࠭�筠� �㬬� ����権
     Modified: 08.09.2004 16:36 ligp     35398: �����祭�� �ଠ� �㬬 � ��࠭��� �ଠ� ���ᮢ��
                                         ���㬥�⮢
     Modified: 10.09.2004 11:57 ligp     31282: �������� 113-� (�࠭�筠� �㬬� ����権)
     Modified: 23.10.2004 13:38 ligp     35309: ���� � �࠭������ ���㯪� � �த��� ������
     Modified: 02.11.2004 13:26 ligp     32294: ����������� �࠭����� �� �믮������ ������   
                                         �����⥦��� ����� (1446-�)
     Modified: 01.12.2004 16:00 ligp     36618: ����ன�� �࠭���権 �� �ਥ�� � �뤠� �।�� ��
                                         ��⠬ �� �� ����� ��堭����
     Modified: 16.12.2004 13:54 ligp     40289: �� �뢮����� ���⮪ �� ������⭮�� ���� ������ �
                                         �࠭������ �믏���� � ��珮���
     Modified: 18.01.2005 18:48 ligp     39221: ��� ���������� ।���஢���� ���祭�� ���� "�����"
                                         ��� vok-kas
     Modified: 26.01.2005 18:38 ligp     40289: �� �뢮����� ���⮪ �� ���ᮢ�� ��� � �࠭������
                                         ���㯪� � �த��� ������
     Modified: 03.12.2005 14:11 rija      
     Modified: 03.12.2005 18:35 rija     42301: ����������� ����������� ����� ᯥ樠�쭮�� ���� ��
                                         �믮������ �������� ����.
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

&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
CREATE WIDGET-POOL.
&ENDIF

{globals.i}
{sh-defs.i}
{ksh-defs.i}

DEFINE VARIABLE vLogChkOnLeave1 AS LOGICAL INITIAL NO NO-UNDO. /* �ਧ��� ���饭�� ���� */
DEFINE VARIABLE vLogChkOnLeave2 AS LOGICAL INITIAL NO NO-UNDO. /* ���� ��।�����, �⮡ ����� �뫮 室��� � ������ ������ */
DEFINE VARIABLE mDpr-Id AS INT64 NO-UNDO. /* ��� ᬥ�� ��� */
DEFINE VARIABLE mOpDate AS DATE NO-UNDO. /* ��� ���भ� */

DEFINE VARIABLE mFirstRun AS LOGICAL NO-UNDO. /* ���� ��ࢮ�� ����᪠ - �⮡� �� ��࣠�� ����� */

DEFINE VARIABLE mOpKind AS CHARACTER NO-UNDO. /* ��� �࠭���樨 */
DEFINE VARIABLE mBaseTmplId AS INT64 NO-UNDO.  /* �� �������� 蠡���� */

DEFINE VARIABLE mAcctDbTmplLnId AS INT64 NO-UNDO. /* �� ��ਡ�� acct-db */
DEFINE VARIABLE mAcctDbTmplId AS INT64 NO-UNDO. /* �� 蠡���� ४����� acct-db */
DEFINE VARIABLE mFormulaeAcctDb AS CHARACTER NO-UNDO. /* ����ୠ� ��㫠 ��� ����� */
DEFINE VARIABLE mPrepFormulaeAcctDb AS CHARACTER NO-UNDO. /* �⪮�����஢����� ����ୠ� ��㫠 ��� ����� */

DEFINE VARIABLE mAcctCrTmplLnId AS INT64 NO-UNDO. /* �� ��ਡ�� acct-db */
DEFINE VARIABLE mAcctCrTmplId AS INT64 NO-UNDO. /* �� 蠡���� ४����� acct-db */
DEFINE VARIABLE mFormulaeAcctCr AS CHARACTER NO-UNDO. /* ����ୠ� ��㫠 ��� �।�� */
DEFINE VARIABLE mPrepFormulaeAcctCr AS CHARACTER NO-UNDO. /* �⪮�����஢����� ����ୠ� ��㫠 ��� �।�� */

DEFINE VARIABLE mCurrencyDbTmplLnId AS INT64 NO-UNDO. /* �� ��ਡ�� currency �� ������ */
DEFINE VARIABLE mCurrencyDbTmplId AS INT64 NO-UNDO. /* �� 蠡���� ४����� curency �� ������ */

DEFINE VARIABLE mCurrencyCrTmplLnId AS INT64 NO-UNDO. /* �� ��ਡ�� currency �� �।��� */
DEFINE VARIABLE mCurrencyCrTmplId AS INT64 NO-UNDO. /* �� 蠡���� ४����� curency �� �।��� */

DEFINE VARIABLE mFirstTmplId AS INT64 NO-UNDO.            /* �� ��ࢮ�� 蠡���� � �ॡ㥬�� ஫�� ��ꥪ� */
DEFINE VARIABLE mVidOpNalTmplLnId AS INT64 NO-UNDO.       /* �� ��ਡ�� "���������" */
DEFINE VARIABLE mVidOpNalTmplId AS INT64 NO-UNDO.         /* �� 蠡���� ४����� "���������" */
DEFINE VARIABLE mFormulaeVidOpNal AS CHARACTER NO-UNDO.     /* ����ୠ� ��㫠 ��� "��� ����樨" */
DEFINE VARIABLE mPrepFormulaeVidOpNal AS CHARACTER NO-UNDO. /* �⪮�����஢����� ����ୠ� ��㫠 ��� "��� ����樨" */

DEFINE VARIABLE mTypeOper AS CHARACTER NO-UNDO. /* ��� ����樨 (��६����� �� �࠭���樨) */

DEFINE VARIABLE mlgError AS LOGICAL     NO-UNDO INIT NO.

DEFINE VARIABLE vLogSpecialRate AS LOGICAL INITIAL NO   NO-UNDO.
DEFINE VARIABLE SetQuestion     AS LOGICAL INITIAL NO   NO-UNDO.
DEFINE VARIABLE vLogN_R       AS LOGICAL           NO-UNDO.
DEFINE VARIABLE mHandInput    AS LOGICAL           NO-UNDO.
DEFINE VARIABLE mValueChange1 AS LOGICAL           NO-UNDO.
DEFINE VARIABLE mValueChange2 AS LOGICAL           NO-UNDO.
DEFINE VARIABLE mValChCurrDb  AS LOGICAL           NO-UNDO.
DEFINE VARIABLE mValChCurrCr  AS LOGICAL           NO-UNDO.
DEFINE VARIABLE mEnablePers   AS LOGICAL           NO-UNDO.
DEFINE VARIABLE mHCurrDb      AS HANDLE            NO-UNDO.
DEFINE VARIABLE mHCurrCr      AS HANDLE            NO-UNDO.

{intrface.get tparam}
{intrface.get pbase}
{intrface.get instrum} /* �����㬥��� ��� ���ᮢ � �����*/
{intrface.get fnum}
{intrface.get vok}
{intrface.get acct}   /* ����䥩�� ��� ࠡ��� � ��⠬� */
{intrface.get parsr} 
{intrface.get trans}                                        

{intrface.get dps} /* �����㬥��� ��� ࠡ��� � ���묨 ��������. */

{vok-pars.fun}
{rep1433y.fun}

mDpr-id = INT64(tGetParam("dpr-id", "vok", "")) NO-ERROR.

mOpKind = GetBaseOpkind().
mBaseTmplId = GetBaseTemplate().
mHandInput  = GetXAttrValue("op-kind",mOpKind,"��炢���㬬�") EQ "��".

RUN GetAttrTmplLnId(mBaseTmplId, "op-entry1", "acct-cr",
                    OUTPUT mAcctCrTmplId, OUTPUT mAcctCrTmplLnId).
RUN GetAttrTmplLnId(mBaseTmplId, "op-entry1", "currency",
                    OUTPUT mCurrencyCrTmplId, OUTPUT mCurrencyCrTmplLnId).

RUN GetAttrTmplLnId(mBaseTmplId, "op-entry2", "acct-db",
                    OUTPUT mAcctDbTmplId, OUTPUT mAcctDbTmplLnId).
RUN GetAttrTmplLnId(mBaseTmplId, "op-entry2", "currency",
                    OUTPUT mCurrencyDbTmplId, OUTPUT mCurrencyDbTmplLnId).

RUN GetFormula(mAcctDbTmplLnId,
               OUTPUT mFormulaeAcctDb, OUTPUT mPrepFormulaeAcctDb).
RUN GetFormula(mAcctCrTmplLnId,
               OUTPUT mFormulaeAcctCr, OUTPUT mPrepFormulaeAcctCr).


/* ��� ����権 � �����⥦�묨 ������묨 �।�⢠�� ���� ��� ����樨 �롨��� �� ���� */
mFirstTmplId = GetFirstTemplate(mOpKind,"���� ������").
RUN GetAttrTmplLnId(mFirstTmplId, "���� ������", "���������",
                    OUTPUT mVidOpNalTmplId, OUTPUT mVidOpNalTmplLnId).

RUN GetFormula(mVidOpNalTmplLnId,
               OUTPUT mFormulaeVidOpNal, OUTPUT mPrepFormulaeVidOpNal).

DEFINE VARIABLE vLogOff113i AS LOGICAL    NO-UNDO.
/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE MAIN-FRAME fMain
/* ��� vok-pers.i */
&GLOBAL-DEFINE tt-op  tt-vok-val
&GLOBAL-DEFINE OpEntry 1
&GLOBAL-DEFINE ChekSaldo YES

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
&GLOBAL-DEFINE SmartFrame vok-pers.i

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-vok-val tt-op-entry1 tt-op-entry2

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain tt-op-entry1.acct-cr ~
tt-op-entry2.acct-db tt-op-entry1.currency tt-vok-val.TrCodeCr ~
tt-op-entry2.currency tt-vok-val.TrCodeDB tt-vok-val.sprate ~
tt-op-entry1.amt-cur tt-op-entry2.amt-cur tt-vok-val.vidopnalv$ ~
tt-vok-val.vkl113i$ 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain tt-op-entry1.acct-cr ~
tt-op-entry2.acct-db tt-op-entry1.currency tt-vok-val.TrCodeCr ~
tt-op-entry2.currency tt-vok-val.TrCodeDB tt-vok-val.sprate ~
tt-op-entry1.amt-cur tt-op-entry2.amt-cur tt-vok-val.vidopnalv$ ~
tt-vok-val.vkl113i$ 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain tt-op-entry1 tt-op-entry2 ~
tt-vok-val
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain tt-op-entry1
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-fMain tt-op-entry2
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-fMain tt-vok-val
&Scoped-define QUERY-STRING-fMain FOR EACH tt-vok-val SHARE-LOCK, ~
      EACH tt-op-entry1 OF tt-vok-val SHARE-LOCK, ~
      EACH tt-op-entry2 OF tt-vok-val SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH tt-vok-val SHARE-LOCK, ~
      EACH tt-op-entry1 OF tt-vok-val SHARE-LOCK, ~
      EACH tt-op-entry2 OF tt-vok-val SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-vok-val tt-op-entry1 tt-op-entry2
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-vok-val
&Scoped-define SECOND-TABLE-IN-QUERY-fMain tt-op-entry1
&Scoped-define THIRD-TABLE-IN-QUERY-fMain tt-op-entry2


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-op-entry1.acct-cr tt-op-entry2.acct-db ~
tt-op-entry1.currency tt-vok-val.TrCodeCr tt-op-entry2.currency ~
tt-vok-val.TrCodeDB tt-vok-val.sprate tt-op-entry1.amt-cur ~
tt-op-entry2.amt-cur tt-vok-val.vidopnalv$ tt-vok-val.vkl113i$ 
&Scoped-define ENABLED-TABLES tt-op-entry1 tt-op-entry2 tt-vok-val
&Scoped-define FIRST-ENABLED-TABLE tt-op-entry1
&Scoped-define SECOND-ENABLED-TABLE tt-op-entry2
&Scoped-define THIRD-ENABLED-TABLE tt-vok-val
&Scoped-Define ENABLED-OBJECTS mLabelOut mLabelIn mCont-Code mLoanAcct ~
vChVidOpNal Separator1 curr2-name RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS tt-op-entry1.acct-cr tt-op-entry2.acct-db ~
tt-op-entry1.currency tt-vok-val.TrCodeCr tt-op-entry2.currency ~
tt-vok-val.TrCodeDB tt-vok-val.sprate tt-op-entry1.amt-cur ~
tt-op-entry2.amt-cur tt-vok-val.vidopnalv$ tt-vok-val.vkl113i$ 
&Scoped-define DISPLAYED-TABLES tt-op-entry1 tt-op-entry2 tt-vok-val
&Scoped-define FIRST-DISPLAYED-TABLE tt-op-entry1
&Scoped-define SECOND-DISPLAYED-TABLE tt-op-entry2
&Scoped-define THIRD-DISPLAYED-TABLE tt-vok-val
&Scoped-Define DISPLAYED-OBJECTS mLabelOut mLabelIn mCont-Code mLoanAcct ~
curr1-name SpecialRate mTCode2Name vChVidOpNal mUser-Id mDpr-Num mBranch-Id ~
Separator1 curr2-name mTCode1Name mPos1 mPos2 mRateDateTime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-op-entry1.acct-cr tt-op-entry2.acct-db mCont-Code ~
mLoanAcct tt-op-entry1.currency tt-vok-val.TrCodeCr tt-op-entry2.currency ~
tt-vok-val.TrCodeDB tt-vok-val.sprate tt-op-entry1.amt-cur ~
tt-op-entry2.amt-cur tt-vok-val.vidopnalv$ tt-vok-val.vkl113i$ 
&Scoped-define List-2 tt-op-entry1.acct-cr tt-op-entry2.acct-db mCont-Code ~
mLoanAcct tt-op-entry1.currency tt-vok-val.TrCodeCr tt-op-entry2.currency ~
tt-vok-val.TrCodeDB tt-vok-val.sprate tt-op-entry1.amt-cur ~
tt-op-entry2.amt-cur tt-vok-val.vidopnalv$ tt-vok-val.vkl113i$ 
&Scoped-define List-4 mCont-Code mLoanAcct tt-op-entry1.currency ~
tt-vok-val.TrCodeCr tt-op-entry2.currency tt-vok-val.TrCodeDB curr1-name ~
tt-vok-val.sprate tt-op-entry1.amt-cur tt-op-entry2.amt-cur ~
tt-vok-val.vidopnalv$ tt-vok-val.vkl113i$ curr2-name 
&Scoped-define List-5 vChVidOpNal 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE curr1-name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE curr2-name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
     &ELSE SIZE 23 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mBranch-Id AS CHARACTER FORMAT "X(256)":U 
     LABEL "��� ���" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
     &ELSE SIZE 8 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mCont-Code AS CHARACTER FORMAT "X(256)":U 
     LABEL "�������" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
     &ELSE SIZE 25 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mDpr-Num AS INT64 FORMAT "99999":U INITIAL 0 
     LABEL "�����" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
     &ELSE SIZE 5 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mLabelIn AS CHARACTER FORMAT "x(12)":U INITIAL "[ �������� ]" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mLabelOut AS CHARACTER FORMAT "x(10)":U INITIAL "[ ������ ]" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mLoanAcct AS CHARACTER FORMAT "X(25)":U 
     LABEL "����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
     &ELSE SIZE 25 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPos1 AS CHARACTER FORMAT "X(19)":U INITIAL "0" 
     LABEL "�������" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 26 BY 1
     &ELSE SIZE 26 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mPos2 AS CHARACTER FORMAT "X(19)":U INITIAL "0" 
     LABEL "�������" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 26 BY 1
     &ELSE SIZE 26 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mRateDateTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "��" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
     &ELSE SIZE 24 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mTCode1Name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mTCode2Name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mUser-Id AS CHARACTER FORMAT "X(256)":U 
     LABEL "������" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE Separator1 AS CHARACTER FORMAT "X(256)":U INITIAL "==============================================================================" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE SpecialRate AS DECIMAL FORMAT ">>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "����" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE vChVidOpNal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 1
     &ELSE SIZE 60 BY 1 &ENDIF NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 39 BY 6
     &ELSE SIZE 39 BY 6 &ENDIF.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 39 BY 6
     &ELSE SIZE 39 BY 6 &ENDIF.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-vok-val, 
      tt-op-entry1, 
      tt-op-entry2 SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     mLabelOut
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 13 COLON-ALIGNED
          &ELSE AT ROW 6 COL 13 COLON-ALIGNED &ENDIF NO-LABEL
     mLabelIn
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 52 COLON-ALIGNED
          &ELSE AT ROW 6 COL 52 COLON-ALIGNED &ENDIF NO-LABEL
     tt-op-entry1.acct-cr
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 12
          &ELSE AT ROW 5 COL 12 &ENDIF HELP
          "�।�� ���" NO-LABEL FORMAT "xxxxx-xxx-x-xxxx-xxxxxxx"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
          &ELSE SIZE 25 BY 1 &ENDIF
     tt-op-entry2.acct-db
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 46 COLON-ALIGNED
          &ELSE AT ROW 5 COL 46 COLON-ALIGNED &ENDIF HELP
          "����� ���" NO-LABEL FORMAT "xxxxx-xxx-x-xxxx-xxxxxxx"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
          &ELSE SIZE 25 BY 1 &ENDIF
     mCont-Code
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 10 COLON-ALIGNED
          &ELSE AT ROW 4 COL 10 COLON-ALIGNED &ENDIF
     mLoanAcct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 46 COLON-ALIGNED
          &ELSE AT ROW 4 COL 46 COLON-ALIGNED &ENDIF HELP
          "��� ������"
     tt-op-entry1.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 10 COLON-ALIGNED
          &ELSE AT ROW 7 COL 10 COLON-ALIGNED &ENDIF HELP
          "�����"
          LABEL "������" FORMAT "xxx"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-vok-val.TrCodeCr
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 10 COLON-ALIGNED
          &ELSE AT ROW 8 COL 10 COLON-ALIGNED &ENDIF HELP
          "��� 業���� �� �।���"
          LABEL "���" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-op-entry2.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 49 COLON-ALIGNED
          &ELSE AT ROW 7 COL 49 COLON-ALIGNED &ENDIF HELP
          "�����"
          LABEL "������" FORMAT "xxx"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-vok-val.TrCodeDB
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 49 COLON-ALIGNED
          &ELSE AT ROW 8 COL 49 COLON-ALIGNED &ENDIF HELP
          "��� 業���⥩ �� ������"
          LABEL "���" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     curr1-name
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 14 COLON-ALIGNED
          &ELSE AT ROW 7 COL 14 COLON-ALIGNED &ENDIF NO-LABEL
     tt-vok-val.sprate
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 10 COLON-ALIGNED
          &ELSE AT ROW 12 COL 10 COLON-ALIGNED &ENDIF HELP
          "���� ����樨"
          LABEL "����" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     SpecialRate
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 10 COLON-ALIGNED
          &ELSE AT ROW 12 COL 10 COLON-ALIGNED &ENDIF
     tt-op-entry1.amt-cur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 10 COLON-ALIGNED
          &ELSE AT ROW 10 COL 10 COLON-ALIGNED &ENDIF HELP
          "�㬬� � �������"
          LABEL "�����" FORMAT "->>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 26 BY 1
          &ELSE SIZE 26 BY 1 &ENDIF
     tt-op-entry2.amt-cur
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 49 COLON-ALIGNED
          &ELSE AT ROW 10 COL 49 COLON-ALIGNED &ENDIF HELP
          "�㬬� � �������"
          LABEL "�����" FORMAT "->>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 26 BY 1
          &ELSE SIZE 26 BY 1 &ENDIF
     mTCode2Name
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 55 COLON-ALIGNED
          &ELSE AT ROW 8 COL 55 COLON-ALIGNED &ENDIF NO-LABEL
     tt-vok-val.vidopnalv$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 13 COLON-ALIGNED
          &ELSE AT ROW 1 COL 13 COLON-ALIGNED &ENDIF HELP
          "��� ���� ����⭮� ����樨 ��� 113-� ����� ॥��� ��  �஢���"
          LABEL "��������" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 2 BY 1
          &ELSE SIZE 2 BY 1 &ENDIF
     vChVidOpNal
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 18
          &ELSE AT ROW 1 COL 18 &ENDIF NO-LABEL
     tt-vok-val.vkl113i$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 2
          &ELSE AT ROW 5 COL 2 &ENDIF HELP
          "�ᯮ�짮����� ������樨 113�" NO-LABEL FORMAT "yes/no"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     mUser-Id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 13 COLON-ALIGNED
          &ELSE AT ROW 2 COL 13 COLON-ALIGNED &ENDIF
     mDpr-Num
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 37 COLON-ALIGNED
          &ELSE AT ROW 2 COL 37 COLON-ALIGNED &ENDIF
     mBranch-Id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 65 COLON-ALIGNED
          &ELSE AT ROW 2 COL 65 COLON-ALIGNED &ENDIF
     Separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 1
          &ELSE AT ROW 3 COL 1 &ENDIF NO-LABEL
     curr2-name
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 53 COLON-ALIGNED
          &ELSE AT ROW 7 COL 53 COLON-ALIGNED &ENDIF NO-LABEL
     mTCode1Name
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 16 COLON-ALIGNED
          &ELSE AT ROW 8 COL 16 COLON-ALIGNED &ENDIF NO-LABEL
     mPos1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 10 COLON-ALIGNED
          &ELSE AT ROW 9 COL 10 COLON-ALIGNED &ENDIF
     mPos2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 49 COLON-ALIGNED
          &ELSE AT ROW 9 COL 49 COLON-ALIGNED &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     mRateDateTime
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 49 COLON-ALIGNED
          &ELSE AT ROW 12 COL 49 COLON-ALIGNED &ENDIF
     RECT-1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 1
          &ELSE AT ROW 6 COL 1 &ENDIF
     RECT-2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 40
          &ELSE AT ROW 6 COL 40 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20
        TITLE "��������: ��������".

DEFINE FRAME fPerson
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 1 ROW 14
         SIZE 78 BY 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS

/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-op-entry1 T "?" NO-UNDO bisquit op-entry
      ADDITIONAL-FIELDS:
          FIELD datanal$ AS DATE /* ��⠍�� */
          FIELD zks$ AS INT64 /* ��� */
          FIELD nomdog$ AS CHARACTER /* ������ */
          FIELD oblsumm$ AS DECIMAL /* ����㬬 */
          FIELD spod$ AS LOGICAL /* ���� */
          FIELD spodpribubyt$ AS CHARACTER /* �����ਡ���� */
          FIELD strana$ AS CHARACTER /* ��࠭� */
          FIELD sumnds$ AS DECIMAL /* �㬍�� */
          FIELD f401swcet$ AS CHARACTER /* �401��� */
          FIELD cenamc$ AS DECIMAL /* ������ */
          FIELD CenterCR AS CHARACTER /* CenterCR */
          FIELD CenterDR AS CHARACTER /* CenterDR */
          FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
          FIELD DetailsCR AS CHARACTER /* DetailsCR */
          FIELD DetailsDR AS CHARACTER /* DetailsDR */
          FIELD exc_603 AS LOGICAL /* exc_603 */
          FIELD f134 AS CHARACTER /* f134 */
          FIELD form-type-code AS CHARACTER /* form-type-code */
          FIELD op-entry-link AS CHARACTER /* op-entry-link */
          FIELD VDateCR AS DATE /* VDateCR */
          FIELD VDateDR AS DATE /* VDateDR */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-op-entry1" "op-entry1" }
          
      END-FIELDS.
      TABLE: tt-op-entry2 T "?" NO-UNDO bisquit op-entry
      ADDITIONAL-FIELDS:
          FIELD datanal$ AS DATE /* ��⠍�� */
          FIELD zks$ AS INT64 /* ��� */
          FIELD nomdog$ AS CHARACTER /* ������ */
          FIELD oblsumm$ AS DECIMAL /* ����㬬 */
          FIELD spod$ AS LOGICAL /* ���� */
          FIELD spodpribubyt$ AS CHARACTER /* �����ਡ���� */
          FIELD strana$ AS CHARACTER /* ��࠭� */
          FIELD sumnds$ AS DECIMAL /* �㬍�� */
          FIELD f401swcet$ AS CHARACTER /* �401��� */
          FIELD cenamc$ AS DECIMAL /* ������ */
          FIELD CenterCR AS CHARACTER /* CenterCR */
          FIELD CenterDR AS CHARACTER /* CenterDR */
          FIELD ClassLibrary AS CHARACTER /* ClassLibrary */
          FIELD DetailsCR AS CHARACTER /* DetailsCR */
          FIELD DetailsDR AS CHARACTER /* DetailsDR */
          FIELD exc_603 AS LOGICAL /* exc_603 */
          FIELD f134 AS CHARACTER /* f134 */
          FIELD form-type-code AS CHARACTER /* form-type-code */
          FIELD op-entry-link AS CHARACTER /* op-entry-link */
          FIELD VDateCR AS DATE /* VDateCR */
          FIELD VDateDR AS DATE /* VDateDR */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-op-entry2" "op-entry2" }
          
      END-FIELDS.
      TABLE: tt-vok-val T "?" NO-UNDO bisquit op
      ADDITIONAL-FIELDS:
          FIELD CardStatus AS CHARACTER /* CardStatus */
          FIELD akkrvid$ AS CHARACTER /* ������� */
          FIELD akkrno$ AS CHARACTER /* ������ */
          FIELD osnzawc$ AS CHARACTER /* �ᭇ�� */
          FIELD vswcetobesp$ AS LOGICAL /* ���⎡�� */
          FIELD f350_dec AS LOGICAL /* f350_dec */
          FIELD adres$ AS CHARACTER /* ���� */
          FIELD vidkursa$ AS CHARACTER /* ������� */
          FIELD vidopnalv$ AS CHARACTER /* ��������� */
          FIELD vkl113i$ AS LOGICAL /* ���113� */
          FIELD kodreggni$ AS CHARACTER /* ��������� */
          FIELD vklad$ AS CHARACTER /* ����� */
          FIELD dataotmbpol$ AS DATE /* ��⠎⬁��� */
          FIELD datapldok$ AS DATE /* ��⠏���� */
          FIELD datapomewteniwavkart$ AS DATE /* ��⠏���饭���� */
          FIELD dover$ AS LOGICAL /* ����� */
          FIELD dovlico$ AS CHARACTER /* ������ */
          FIELD dokum$ AS CHARACTER /* ���� */
          FIELD kbk$ AS CHARACTER /* ��� */
          FIELD kmpr$ AS LOGICAL /* ���� */
          FIELD kodneobywc$ AS CHARACTER /* ��������� */
          FIELD kodopval117$ AS CHARACTER /* ��������117 */
          FIELD kodopotmyv$ AS CHARACTER /* �������� */
          FIELD kodstsm$ AS CHARACTER /* ����⑬ */
          FIELD kop$ AS CHARACTER /* ��� */
          FIELD nomdog$ AS CHARACTER /* ������ */
          FIELD nomdok$ AS CHARACTER /* ������ */
          FIELD nomerpawcki$ AS CHARACTER /* �������窨 */
          FIELD nomerreestra$ AS INT64 /* ����������� */
          FIELD nomreestr$ AS CHARACTER /* ��������� */
          FIELD nomwcpl$ AS INT64 /* ������ */
          FIELD okato-nalog$ AS CHARACTER /* �����-����� */
          FIELD ostpl$ AS DECIMAL /* ��⏫ */
          FIELD p106n_statplat$ AS CHARACTER /* �106�_��⏫�� */
          FIELD pasportsdelki$ AS CHARACTER /* ��ᯮ�⑤���� */
          FIELD platelw#wtik$ AS CHARACTER /* ���⥫�騪 */
          FIELD platinstr$ AS CHARACTER /* ���∭��� */
          FIELD podozdokument$ AS CHARACTER /* ��������㬥�� */
          FIELD pokdd$ AS CHARACTER /* ����� */
          FIELD poknd$ AS CHARACTER /* ����� */
          FIELD poknp$ AS CHARACTER /* ����� */
          FIELD pokop$ AS CHARACTER /* ����� */
          FIELD pokst$ AS CHARACTER /* ����� */
          FIELD poktp$ AS CHARACTER /* ����� */
          FIELD poluwcatelw#$ AS CHARACTER /* �����⥫� */
          FIELD predplatelw#wtika$ AS CHARACTER /* �।���⥫�騪� */
          FIELD predpoluwcatelwa$ AS CHARACTER /* �।�����⥫� */
          FIELD pfwepd$ AS LOGICAL /* ����� */
          FIELD pcstatus$ AS CHARACTER /* ������� */
          FIELD rasporwaditelw#$ AS CHARACTER /* ��ᯮ�廊⥫� */
          FIELD sisdenper$ AS CHARACTER /* ��ᄥ���� */
          FIELD spisswceta$ AS DATE /* ������ */
          FIELD srkakc$ AS INT64 /* �઀�� */
          FIELD statrash$ AS CHARACTER /* ��␠�� */
          FIELD summavalkontr$ AS CHARACTER /* �㬬��������� */
          FIELD summaobwazprodawzi$ AS DECIMAL /* �㬬���紐த��� */
          FIELD tipavansa$ AS CHARACTER /* �������� */
          FIELD tipoper$ AS CHARACTER /* ������� */
          FIELD uis-wed$ AS CHARACTER /* ���-�� */
          FIELD uslopl$ AS CHARACTER /* �ᫎ�� */
          FIELD uwcastnik$ AS CHARACTER /* ���⭨� */
          FIELD fio$ AS CHARACTER /* ��� */
          FIELD wsifrpl$ AS CHARACTER /* ������ */
          FIELD weksport$ AS LOGICAL /* ��ᯮ�� */
          FIELD acct-cr AS CHARACTER /* acct-cr */
          FIELD acct-db AS CHARACTER /* acct-db */
          FIELD acct-rec AS CHARACTER /* acct-rec */
          FIELD acct-send AS CHARACTER /* acct-send */
          FIELD acctbal AS CHARACTER /* acctbal */
          FIELD acctcorr AS CHARACTER /* acctcorr */
          FIELD amt-cur AS DECIMAL /* amt-cur */
          FIELD amt-cur-debt AS DECIMAL /* amt-cur-debt */
          FIELD amt-rub AS DECIMAL /* amt-rub */
          FIELD ARDBI AS DATE /* ARDBI */
          FIELD bank-country-rec AS CHARACTER /* bank-country-rec */
          FIELD bank-country-send AS CHARACTER /* bank-country-send */
          FIELD card-id AS CHARACTER /* card-id */
          FIELD commission AS CHARACTER /* commission */
          FIELD Contract-Code AS CHARACTER /* Contract-Code */
          FIELD country-pers AS CHARACTER /* country-pers */
          FIELD country-rec AS CHARACTER /* country-rec */
          FIELD country-send AS CHARACTER /* country-send */
          FIELD ctrans-id AS CHARACTER /* ctrans-id */
          FIELD cust-cat-rec AS CHARACTER /* cust-cat-rec */
          FIELD cust-doc-who AS CHARACTER /* cust-doc-who */
          FIELD DBI AS CHARACTER /* DBI */
          FIELD dealing-id AS DECIMAL /* dealing-id */
          FIELD descr AS CHARACTER /* descr */
          FIELD diasoft-id AS CHARACTER /* diasoft-id */
          FIELD document-id AS CHARACTER /* document-id */
          FIELD dpr-id AS INTEGER /* dpr-id */
          FIELD EL-DOC-DATE AS DATE /* EL-DOC-DATE */
          FIELD exc_407 AS LOGICAL /* exc_407 */
          FIELD exp-date AS CHARACTER /* exp-date */
          FIELD f102_cur AS CHARACTER /* f102_cur */
          FIELD f401_op AS CHARACTER /* f401_op */
          FIELD F407 AS CHARACTER /* F407 */
          FIELD f646-648 AS CHARACTER /* f646-648 */
          FIELD file-name AS CHARACTER /* file-name */
          FIELD flag-comm AS LOGICAL /* flag-comm */
          FIELD form-num AS CHARACTER /* form-num */
          FIELD form-seria AS CHARACTER /* form-seria */
          FIELD FormBehavior AS CHARACTER /* FormBehavior */
          FIELD HistoryFields AS CHARACTER /* HistoryFields */
          FIELD inn-rec AS CHARACTER /* inn-rec */
          FIELD inn-send AS CHARACTER /* inn-send */
          FIELD Isn AS CHARACTER /* Isn */
          FIELD KNFRoleName AS CHARACTER /* KNFRoleName */
          FIELD Kpp-rec AS CHARACTER /* Kpp-rec */
          FIELD Kpp-send AS CHARACTER /* Kpp-send */
          FIELD LegTerr AS CHARACTER /* LegTerr */
          FIELD link-op AS CHARACTER /* link-op */
          FIELD link-op-sum AS INT64 /* link-op-sum */
          FIELD Mark-Cli-Bank AS LOGICAL /* Mark-Cli-Bank */
          FIELD name-rec AS CHARACTER /* name-rec */
          FIELD name-send AS CHARACTER /* name-send */
          FIELD NUM-RKC AS CHARACTER /* NUM-RKC */
          FIELD op-bal AS CHARACTER /* op-bal */
          FIELD op-reference AS CHARACTER /* op-reference */
          FIELD Opcion AS CHARACTER /* Opcion */
          FIELD OperNum AS INT64 /* OperNum */
          FIELD reference-mci AS CHARACTER /* reference-mci */
          FIELD sec-code-type AS CHARACTER /* sec-code-type */
          FIELD ServInst-Ref AS CHARACTER /* ServInst-Ref */
          FIELD SettNbr AS CHARACTER /* SettNbr */
          FIELD signature AS CHARACTER /* signature */
          FIELD SLINF AS CHARACTER /* SLINF */
          FIELD sprate AS CHARACTER /* sprate */
          FIELD swift-acct-op AS CHARACTER /* swift-acct-op */
          FIELD swift-det-cod AS CHARACTER /* swift-det-cod */
          FIELD swift-det-inf AS CHARACTER /* swift-det-inf */
          FIELD swift-det-pay AS CHARACTER /* swift-det-pay */
          FIELD swift-mes-ben AS CHARACTER /* swift-mes-ben */
          FIELD tarif-RKC AS CHARACTER /* tarif-RKC */
          FIELD TicketNum AS CHARACTER /* TicketNum */
          FIELD time-stamp AS CHARACTER /* time-stamp */
          FIELD trans-id AS CHARACTER /* trans-id */
          FIELD TrCodeCr AS CHARACTER /* TrCodeCr */
          FIELD TrCodeDB AS CHARACTER /* TrCodeDB */
          FIELD u1557_exc AS LOGICAL /* u1557_exc */
          FIELD u1557_SysPer AS LOGICAL /* u1557_SysPer */
          FIELD vop-code AS CHARACTER /* vop-code */
          FIELD Birthday AS DATE /* Birthday */
          FIELD Document4Date_vid AS DATE /* Document4Date_vid */
          FIELD BirthPlace AS CHARACTER /* BirthPlace */
          FIELD local__template AS LOGICAL   /* �ਧ��� 蠡���/�� 蠡��� */
          FIELD local__rowid    AS ROWID     /* ROWID ����� � ��        */
          FIELD local__id       AS INT64   /* �����䨪��� �����     */
          FIELD local__upid     AS INT64   /* ��뫪� �� ������ � ���ॣ����饩 ⠡��� */
          FIELD user__mode      AS INT64   /* ���� �ࠢ����� ������� � �� */
          /* Additional fields you should place here                      */
          
          /* �����뢠�� ��뫪� �� �६����� ⠡���� � ᯥ樠���� ⠡���� */
          {ln-tthdl.i "tt-vok-val" "" }
          
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
         HEIGHT             = 19.99
         WIDTH              = 80
         MAX-HEIGHT         = 19.99
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 19.99
         VIRTUAL-WIDTH      = 80
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
{vok-trig.i}
{vok-op.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME fPerson:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fPerson:MOVE-AFTER-TAB-ITEM (tt-vok-val.vkl113i$:HANDLE IN FRAME fMain)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN tt-op-entry1.acct-cr IN FRAME fMain
   ALIGN-L 1 2 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR FILL-IN tt-op-entry2.acct-db IN FRAME fMain
   1 2 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-op-entry2.amt-cur IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-op-entry1.amt-cur IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN curr1-name IN FRAME fMain
   NO-ENABLE 4                                                          */
ASSIGN 
       curr1-name:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN curr2-name IN FRAME fMain
   4                                                                    */
ASSIGN 
       curr2-name:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-op-entry2.currency IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-op-entry1.currency IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN mBranch-Id IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mBranch-Id:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mCont-Code IN FRAME fMain
   1 2 4                                                                */
/* SETTINGS FOR FILL-IN mDpr-Num IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mDpr-Num:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mLoanAcct IN FRAME fMain
   1 2 4                                                                */
/* SETTINGS FOR FILL-IN mPos1 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mPos1:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mPos2 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mPos2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mRateDateTime IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mRateDateTime:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mTCode1Name IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mTCode1Name:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mTCode2Name IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mTCode2Name:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mUser-Id IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mUser-Id:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN Separator1 IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN SpecialRate IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-vok-val.sprate IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-vok-val.TrCodeCr IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-vok-val.TrCodeDB IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN vChVidOpNal IN FRAME fMain
   ALIGN-L 5                                                            */
ASSIGN 
       vChVidOpNal:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-vok-val.vidopnalv$ IN FRAME fMain
   1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-vok-val.vkl113i$ IN FRAME fMain
   ALIGN-L 1 2 4 EXP-LABEL EXP-FORMAT EXP-HELP                          */
/* SETTINGS FOR FRAME fPerson
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "Temp-Tables.tt-vok-val,Temp-Tables.tt-op-entry1 OF Temp-Tables.tt-vok-val,Temp-Tables.tt-op-entry2 OF Temp-Tables.tt-vok-val"
     _TblOptList       = ",,"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-op-entry1.amt-cur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry1.amt-cur TERMINAL-SIMULATION
ON ENTRY OF tt-op-entry1.amt-cur IN FRAME fMain /* ����� */
DO:
   mgHAmtCur = IF NOT VALID-HANDLE(mgHAmtCur) THEN SELF ELSE mgHAmtCur.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry1.amt-cur TERMINAL-SIMULATION
ON LEAVE OF tt-op-entry1.amt-cur IN FRAME fMain /* ����� */
DO:
   {&BT_LEAVE}
   /* �����⠥� �㬬� ��ன ������ */
   RUN CalcSumm(SELF:SCREEN-VALUE,tt-op-entry2.amt-cur:HANDLE).
   RUN CheckPers NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RETURN NO-APPLY.
   mValueChange1 = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry1.amt-cur TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-op-entry1.amt-cur IN FRAME fMain /* ����� */
DO:
   mValueChange1 = YES. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-op-entry2.amt-cur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry2.amt-cur TERMINAL-SIMULATION
ON ENTRY OF tt-op-entry2.amt-cur IN FRAME fMain /* ����� */
DO:
   mgHAmtCur = IF NOT VALID-HANDLE(mgHAmtCur) THEN SELF ELSE mgHAmtCur.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry2.amt-cur TERMINAL-SIMULATION
ON LEAVE OF tt-op-entry2.amt-cur IN FRAME fMain /* ����� */
DO:
   {&BT_LEAVE}
   /* �����⠥� �㬬� ��ன ������ */
   RUN CalcSumm(SELF:SCREEN-VALUE,tt-op-entry1.amt-cur:HANDLE).
   RUN CheckPers NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RETURN NO-APPLY.
   mValueChange2 = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry2.amt-cur TERMINAL-SIMULATION
ON RETURN OF tt-op-entry2.amt-cur IN FRAME fMain /* ����� */
DO:
   /* �����⠥� �㬬� ��ன ������ */
   RUN CalcSumm(SELF:SCREEN-VALUE,tt-op-entry1.amt-cur:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry2.amt-cur TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-op-entry2.amt-cur IN FRAME fMain /* ����� */
DO:
   mValueChange2 = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-op-entry2.currency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry2.currency TERMINAL-SIMULATION
ON LEAVE OF tt-op-entry2.currency IN FRAME fMain /* ������ */
DO:
   IF vLogChkOnLeave2 EQ YES AND 
      NOT SELF:MODIFIED THEN /* ��� ������ ᬥ����� */
      RETURN.

   {&BT_LEAVE}

   &IF DEFINED(MANUAL-REMOTE) &THEN
      ASSIGN
         mHCurrDb = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE, "tt-op-entry1.currency")
         mHCurrCr = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE, "tt-op-entry2.currency")
         .
   &ENDIF

   IF iMode = {&MOD_ADD} OR iMode = {&MOD_EDIT} THEN
   DO:

      RUN CurrencyLeave(SELF:SCREEN-VALUE, 
                        "Curr2-name", 
                        "mPos2",
                        &IF DEFINED(MANUAL-REMOTE) &THEN 
                           mHCurrDb:MODIFIED OR mHCurrCr:MODIFIED OR mValChCurrDb OR mValChCurrCr
                        &ELSE 
                           mValChCurrDb OR mValChCurrCr 
                        &ENDIF).

      IF RETURN-VALUE = {&RET-ERROR} THEN
         RETURN NO-APPLY {&RET-ERROR}.

/* �����⠥� �㬬� ��ன ������ */
/* �᫨ ������� �㬬� � ���� "����祭�" */
      IF tt-op-entry2.amt-cur:SENSITIVE EQ YES THEN
         RUN CalcSumm(tt-op-entry2.amt-cur:SCREEN-VALUE,
                      tt-op-entry1.amt-cur:HANDLE).
      ELSE /* �᫨ ������� �㬬� � ���� "�뤠��" */
         IF tt-op-entry1.amt-cur:SENSITIVE EQ YES THEN
            RUN CalcSumm(tt-op-entry1.amt-cur:SCREEN-VALUE,
                         tt-op-entry2.amt-cur:HANDLE).

      IF tt-op-entry2.currency NE ? THEN 
         vLogChkOnLeave2 = YES.
   END.
   mValChCurrCr = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry2.currency TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-op-entry2.currency IN FRAME fMain /* ������ */
DO:
   mValChCurrCr = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-op-entry1.currency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry1.currency TERMINAL-SIMULATION
ON LEAVE OF tt-op-entry1.currency IN FRAME fMain /* ������ */
DO:
   IF vLogChkOnLeave1 EQ YES AND 
      NOT SELF:MODIFIED THEN /* ��� ������ ᬥ����� */
      RETURN.
   {&BT_LEAVE}
   &IF DEFINED(MANUAL-REMOTE) &THEN
      ASSIGN
         mHCurrDb = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE, "tt-op-entry1.currency")
         mHCurrCr = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE, "tt-op-entry2.currency")
         .
   &ENDIF

   IF iMode = {&MOD_ADD} OR iMode = {&MOD_EDIT} THEN
   DO:
      RUN CurrencyLeave(SELF:SCREEN-VALUE, 
                        "Curr1-name", 
                        "mPos1", 
                        &IF DEFINED(MANUAL-REMOTE) &THEN 
                           mHCurrDb:MODIFIED OR mHCurrCr:MODIFIED OR mValChCurrDb OR mValChCurrCr
                        &ELSE 
                           mValChCurrDb OR mValChCurrCr 
                        &ENDIF).

      IF RETURN-VALUE = {&RET-ERROR} THEN
         RETURN NO-APPLY {&RET-ERROR}.

/* �����⠥� �㬬� ��ன ������ */
/* �᫨ ������� �㬬� � ���� "�뤠��" */
      IF tt-op-entry1.amt-cur:SENSITIVE EQ YES THEN
         RUN CalcSumm(tt-op-entry1.amt-cur:SCREEN-VALUE,
                      tt-op-entry2.amt-cur:HANDLE).
      ELSE  /* �᫨ ������� �㬬� � ���� "����祭�" */
         IF tt-op-entry2.amt-cur:SENSITIVE EQ YES THEN
            RUN CalcSumm(tt-op-entry2.amt-cur:SCREEN-VALUE,
                         tt-op-entry1.amt-cur:HANDLE).

      IF tt-op-entry1.currency NE ? THEN 
         vLogChkOnLeave1 = YES.
   END.
   mValChCurrDb = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-op-entry1.currency TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-op-entry1.currency IN FRAME fMain /* ������ */
DO:
   mValChCurrDb = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mCont-Code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mCont-Code TERMINAL-SIMULATION
ON F1 OF mCont-Code IN FRAME fMain /* ������� */
DO:
   &SCOPED-DEFINE PersMenuItems '������ ���,������ �����,�������'

   RUN messmenu.p (FRAME {&MAIN-FRAME}:ROW + SELF:ROW - 1,
                   "",
                   "",
                   {&PersMenuItems}
                  ).
   IF LASTKEY = 27 THEN
      RETURN NO-APPLY.

   RUN FindClient (ENTRY(INT64(pick-value), {&PersMenuItems}),
                   mCont-Code:HANDLE,
                   mCont-Code:HANDLE
                  ).

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mCont-Code TERMINAL-SIMULATION
ON LEAVE OF mCont-Code IN FRAME fMain /* ������� */
DO:
   DEF VAR vLoanAcct  AS CHAR  NO-UNDO. /* ��� ������� */
   DEF VAR vContCode  AS CHAR  NO-UNDO. /* ����� ������� */
   DEF VAR vPersonId  AS INT64   NO-UNDO. /* ID ������ */
   DEFINE VARIABLE StrWhereLoan AS CHARACTER   NO-UNDO.
   StrWhereLoan = "WHERE loan.contract = 'dps' AND loan.cont-code = " + QUOTER(SELF:INPUT-VALUE + IF shmode THEN ("@" + ShFilial) ELSE "").

   {&BEG_BT_LEAVE}

   IF {assigned SELF:INPUT-VALUE} THEN
   DO:
      vContCode = GetBufferValue("loan", StrWhereLoan, "cont-code").
      IF vContCode = ? THEN
      DO:
         MESSAGE "������� " + QUOTER(SELF:SCREEN-VALUE) + " �� ������."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   ELSE 
   DO:
      MESSAGE "������ ������� ��� ��ᯮ������" SKIP
              "��� �롮� �����襩 F1."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY {&RET-ERROR}.
   END.

   RUN GetBaseAcct IN h_dps("dps",vContCode,mOpDate,OUTPUT vLoanAcct).

   IF {assigned vLoanAcct} THEN
   DO:
      mLoanAcct:SCREEN-VALUE = ENTRY(1,vLoanAcct) NO-ERROR. 
      DISABLE mLoanAcct WITH FRAME {&MAIN-FRAME}.
      APPLY "LEAVE" TO mLoanAcct.
   END.
   ELSE 
      ENABLE mLoanAcct WITH FRAME {&MAIN-FRAME}.

   vPersonId = INT64(GetBufferValue("loan",
                                  "WHERE loan.contract = 'dps' AND loan.cont-code = " + QUOTER(vContCode),
                                  "cust-id"
                                 ) 
                  ) NO-ERROR.

   IF vPersonId <> ? THEN
      RUN DispPerson(vPersonId).

   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mLoanAcct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mLoanAcct TERMINAL-SIMULATION
ON F1 OF mLoanAcct IN FRAME fMain /* ���� */
DO:
   RUN browseld.p("acct",
                  "acct-cat",
                  "b",
                  "acct-cat",
                  iLevel + 1).
   IF LASTKEY EQ 10 THEN
   DO:
      SELF:SCREEN-VALUE = ENTRY(1,pick-value).
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mLoanAcct TERMINAL-SIMULATION
ON LEAVE OF mLoanAcct IN FRAME fMain /* ���� */
DO:
   DEFINE VARIABLE vStringLA AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vLoanAcct AS CHARACTER NO-UNDO. /* ��� ������� */
   DEFINE VARIABLE vAcctCurr AS CHARACTER NO-UNDO. /* ����� ��� */
   DEFINE VARIABLE vCurrName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vMess AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE StrWhereAcct AS CHARACTER   NO-UNDO.

   StrWhereAcct = IF shmode THEN 
                     "WHERE number = " + QUOTER(SELF:INPUT-VALUE) + " AND filial-id = " +  QUOTER(ShFilial)
                  ELSE
                     "WHERE acct = " + QUOTER(SELF:INPUT-VALUE).

   {&BEG_BT_LEAVE}
   IF {assigned SELF:INPUT-VALUE} THEN
   DO:
      vStringLA = 
         GetBufferValue("acct",
                        StrWhereAcct,
                        "acct,currency").
      vAcctCurr = ENTRY(2,vStringLA,CHR(2)) NO-ERROR.
      vLoanAcct = ENTRY(1,vStringLA,CHR(2)) NO-ERROR.

      IF {assigned vLoanAcct} THEN ASSIGN
         tt-op-entry2.acct-db:SCREEN-VALUE  = vLoanAcct
         tt-op-entry2.currency:SCREEN-VALUE = vAcctCurr
         curr2-name:SCREEN-VALUE = 
            GetBufferValue("currency",
                           "where currency.currency = " + QUOTER(vAcctCurr),
                           "name-currenc").

      /* � ��砥 ��������� ������ ᬥ��� ���� � �����⠥� �㬬�*/
      APPLY "LEAVE" TO tt-op-entry2.currency. 

      /* �����뢠�� �࠭��� ���祭�� � ���� �࠭���樨 */
      RUN AddAttr2TableEx(?,
                          mCurrencyDbTmplId,
                          ?,
                          ?,
                          mCurrencyDbTmplLnId,
                          "currency",
                          vAcctCurr).

    /* ���⮪ �� ������⭮�� ���� ������ */
      RUN ViewAcctPos(vLoanAcct,
                      vAcctCurr,
                      mDpr-id,
                      mPos2:HANDLE). 


      IF tt-op-entry2.currency:SCREEN-VALUE EQ "" THEN
      DO:
         DISABLE tt-op-entry2.amt-cur WITH FRAME fMain.
         ENABLE  tt-op-entry1.amt-cur WITH FRAME fMain.
      END.
      ELSE
      DO:
         ENABLE  tt-op-entry2.amt-cur WITH FRAME fMain.
         DISABLE tt-op-entry1.amt-cur WITH FRAME fMain.
      END.

      IF vLoanAcct EQ ? THEN
         vMess = "��� " + QUOTER(SELF:SCREEN-VALUE) + " �� ������".
      ELSE 
         vMess = "".
      IF vMess NE "" THEN
      DO:
         MESSAGE vMess
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SpecialRate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SpecialRate TERMINAL-SIMULATION
ON LEAVE OF SpecialRate IN FRAME fMain /* ���� */
DO:
   {&BEG_BT_LEAVE}
   IF DEC(SELF:SCREEN-VALUE) <= 0 THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      "-1",  
                      "���� �� ����� ���� ����� ��� ࠢ�� 0.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   tt-vok-val.sprate:SCREEN-VALUE = TRIM(SELF:SCREEN-VALUE).
   IF tt-op-entry2.amt-cur:SENSITIVE = YES THEN
      APPLY "LEAVE" TO tt-op-entry2.amt-cur IN FRAME fMain.
   ELSE IF tt-op-entry1.amt-cur:SENSITIVE = YES THEN
      APPLY "LEAVE" TO tt-op-entry1.amt-cur IN FRAME fMain.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-vok-val.TrCodeCr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-vok-val.TrCodeCr TERMINAL-SIMULATION
ON LEAVE OF tt-vok-val.TrCodeCr IN FRAME fMain /* ��� */
DO:
   {&BT_LEAVE}
   RUN TCode_Leave(SELF:SCREEN-VALUE,"mTCode1Name").

   IF RETURN-VALUE = {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-vok-val.TrCodeDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-vok-val.TrCodeDB TERMINAL-SIMULATION
ON LEAVE OF tt-vok-val.TrCodeDB IN FRAME fMain /* ��� */
DO:
   {&BT_LEAVE}
   RUN TCode_Leave(SELF:SCREEN-VALUE,"mTCode2Name").

   IF RETURN-VALUE = {&RET-ERROR} THEN
      RETURN NO-APPLY {&RET-ERROR}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-vok-val.vidopnalv$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-vok-val.vidopnalv$ TERMINAL-SIMULATION
ON F1 OF tt-vok-val.vidopnalv$ IN FRAME fMain /* �������� */
DO:
   mCall_F1_IN_Frame = YES. 
   APPLY "F1" TO FRAME {&MAIN-FRAME}.
   mCall_F1_IN_Frame = NO.
   APPLY "LEAVE" TO tt-vok-val.vidopnalv$ IN FRAME {&MAIN-FRAME}.
   RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-vok-val.vidopnalv$ TERMINAL-SIMULATION
ON LEAVE OF tt-vok-val.vidopnalv$ IN FRAME fMain /* �������� */
DO:
   {&BT_LEAVE}
   IF tt-vok-val.vidopnalv$:SCREEN-VALUE NE "" THEN
   DO:
      vChVidOpNal = GetCodeName("���������",tt-vok-val.vidopnalv$:SCREEN-VALUE).
      DISPLAY 
         vChVidOpNal
         WITH FRAME {&MAIN-FRAME}.
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
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: ���樠������ ��⥬��� ᮮ�饭�� */
   RUN Init-SysMes("","","").

   /* Commented by KSV: ���४��㥬 ���⨪����� ������ �३�� */
   iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
   FRAME fMain:ROW = iLevel.
   /* ������ TITLE COLOR bright-white */
   FRAME fMain:TITLE-DCOLOR = {&bright-white}.

   /* Commented by KSV: ��⠥� ����� */
   RUN GetObject.

   {on113i.i &off113i = "tt-vok-val.vkl113i$"}

   /* ������塞 COMBO-BOX'� ����묨 �� ����奬� */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* ���ᢥ⪠ ����� �� LIST-5 (����ந�� ��� ᥡ�) */
   RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                    REPLACE("{&LIST-5}"," ",","),
                    "bright-green"
                   ).

   /* Commented by KSV: �����뢠�� �࠭��� ��� */
   STATUS DEFAULT "".
&IF DEFINED(SESSION-REMOTE) EQ 0 &THEN
   RUN enable_UI.
&ENDIF

   /* Commented by KSV: ���뢠�� � ����, ����� ࠧ�襭� ��������
   ** � ����ᨬ��� �� ०��� ������ */
   RUN EnableDisable.
   IF mlgError THEN
      LEAVE MAIN-BLOCK.

&IF DEFINED(SESSION-REMOTE) &THEN
   LEAVE MAIN-BLOCK.
&ENDIF

   /* Commented by KSV: ���㥬 ࠧ����⥫�. �������⥫� �������� ��� FILL-IN
   ** � �����䨪��஬ SEPARATOR# � ��ਡ�⮬ VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").
   RUN PutHelp("F1�Shift+F1�F9-४�.��",FRAME {&MAIN-FRAME}:HANDLE).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.
END.

/* Commented by KSV: ����뢠�� �㦡� ��⥬��� ᮮ�饭�� */
RUN End-SysMes.

&IF DEFINED(SESSION-REMOTE) = 0 &THEN
RUN disable_ui.
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcSumm TERMINAL-SIMULATION 
PROCEDURE CalcSumm :
/*------------------------------------------------------------------------------
  Purpose:    ����� �뤠����/�ਭ�⮩ �㬬� 
  Parameters: iSumm  - �㬬� �ਭ�⮩/�뤠���� ������ 
              iHSumm - ��� �㬬� �뤠����/�ਭ�⮩ ������
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER iSumm  AS CHARACTER NO-UNDO. /* �㬬� �ਭ�⮩/�뤠���� ������ */  
   DEFINE INPUT PARAMETER iHSumm AS HANDLE    NO-UNDO. /* ��� �㬬� �뤠����/�ਭ�⮩ ������ */

   DEFINE VARIABLE vRate         AS DECIMAL   NO-UNDO. /* ���� */

   IF iSumm NE ? AND VALID-HANDLE(iHSumm) THEN
   DO:
      &IF DEFINED(MANUAL-REMOTE) &THEN
         ASSIGN
            mHCurrDb = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE, "tt-op-entry1.currency")
            mHCurrCr = GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE, "tt-op-entry2.currency")
            .
      &ENDIF
      
      IF mHandInput AND &IF DEFINED(MANUAL-REMOTE) &THEN NOT mHCurrDb:MODIFIED AND NOT mHCurrCr:MODIFIED &ELSE NOT mValueChange1 AND NOT mValueChange2 &ENDIF THEN
         RETURN.
   
      vRate = DECIMAL(tt-vok-val.sprate:SCREEN-VALUE IN FRAME {&MAIN-FRAME}) NO-ERROR.
   
      /* ��ࠡ�⠥� ���� � ����ᨬ��� �� �㬬� */
      DEFINE VARIABLE vCrossRate AS DECIMAL     NO-UNDO.
      DEFINE VARIABLE vRateDate  AS DATE        NO-UNDO.
      DEFINE VARIABLE vRateTime  AS INT64       NO-UNDO.
   
      IF mgLogUBound AND VALID-HANDLE(mgHAmtCur) THEN
      DO:
         RUN CrossRateTimeR(mgRate-Type,
                            mgInCurr,   
                            mgOutCurr,  
                            mgBranch-Id,
                            mgRDate,    
                            mgRTime,    
                            IF VALID-HANDLE(mgHAmtCur) THEN DEC(mgHAmtCur:SCREEN-VALUE) ELSE 0,
                            OUTPUT vRate,
                            OUTPUT vRateDate,
                            OUTPUT vRateTime).
         mDiffRate = IF RETURN-VALUE NE "" THEN RETURN-VALUE ELSE "".
         IF VALID-HANDLE(mgHSprate) AND VALID-HANDLE(mgHDTSprate) THEN
         DO:
            mgHSprate:SCREEN-VALUE   = STRING(vRate).
            mgHDTSprate:SCREEN-VALUE = STRING(vRateDate) + " " + STRING(vRateTime, "HH:MM").
         END.
      END.
   
      IF vRate EQ 0 THEN
         vRate = 1.
   
      iHSumm:SCREEN-VALUE = STRING(IF mHandInput AND (iHSumm:TABLE EQ ("tt-op-entry1")) THEN (DECIMAL(iSumm) / vRate) ELSE (DECIMAL(iSumm) * vRate)).
   
      DO WITH FRAME {&MAIN-FRAME}:
         ASSIGN
            tt-op-entry2.amt-cur = DEC(tt-op-entry2.amt-cur:SCREEN-VALUE)  
            tt-op-entry1.amt-cur = DEC(tt-op-entry1.amt-cur:SCREEN-VALUE)
            . 
      END.
   END.   
/* ������ ���ᨬ����� �㡫���� �㬬� ����樨 (limitsum.chk) */
   mMaxAmtRub = MaxSumInOp(mOpDate,
                           tt-op-entry2.currency:SCREEN-VALUE,
                           tt-op-entry2.amt-cur:SCREEN-VALUE,
                           tt-op-entry1.currency:SCREEN-VALUE,
                           tt-op-entry1.amt-cur:SCREEN-VALUE).

   RUN MaxSumInOp2(mOpDate,
                   "���������~001���~001document-id~001����",
                   tt-vok-val.vidopnalv$:INPUT-VALUE + CHR(1) + tt-vok-val.fio$:INPUT-VALUE IN FRAME {&PERS-FRAME} + CHR(1) + tt-vok-val.document-id:INPUT-VALUE + CHR(1) + tt-vok-val.dokum$:INPUT-VALUE,
                   OUTPUT mSvodAmtRub).
   mSvodAmtRub = mSvodAmtRub + mMaxAmtRub.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CurrencyLeave TERMINAL-SIMULATION 
PROCEDURE CurrencyLeave :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  iCur          - ��� ������
               iCurNameField - ������������ ������
               iPosNameField - ���⮪
  Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER iCur AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER iCurNameField AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iPosNameField AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iRateCalc     AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE vHTmp  AS HANDLE NO-UNDO.

   DEFINE VARIABLE vResult AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFlagErr AS INT64 NO-UNDO.

   DEFINE VARIABLE vTmplId AS INT64 NO-UNDO.
   DEFINE VARIABLE vTmplLnId AS INT64 NO-UNDO.
   DEFINE VARIABLE vCurr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFormulae AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPrepFormulae AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vCurrIn    AS CHARACTER NO-UNDO. /* ���� �ਭ�� */
   DEFINE VARIABLE vCurrOut   AS CHARACTER NO-UNDO. /* ���� �뤠�� */
   DEFINE VARIABLE vCrossRate AS DECIMAL NO-UNDO.  /* ���� ����樨 */
   DEFINE VARIABLE vRateDate  AS DATE NO-UNDO.     /* ��� ���� */
   DEFINE VARIABLE vRateTime  AS INT64 NO-UNDO.  /* �६� ���� */

   DEFINE BUFFER bAcct FOR acct.
   DEFINE BUFFER currency FOR currency.

   IF iCur EQ ? THEN
      RETURN.

   FIND FIRST currency WHERE currency.currency = iCur
      NO-LOCK
      NO-ERROR.

   IF NOT AVAIL currency THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      "-1",  
                      "��� ������ � ����� " +  iCur).
      RETURN {&RET-ERROR}.
   END.

   IF iPosNameField = "mPos1" THEN
      ASSIGN
         vTmplId = mCurrencyCrTmplId
         vTmplLnId = mCurrencyCrTmplLnId
         vFormulae = mFormulaeAcctCr
         vPrepFormulae = mPrepFormulaeAcctCr
      .
   ELSE
      ASSIGN
         vTmplId = mCurrencyDbTmplId
         vTmplLnId = mCurrencyDbTmplLnId
         vFormulae = mFormulaeAcctDb
         vPrepFormulae = mPrepFormulaeAcctDb
      .
   

   vHTmp = GetWidgetHandle(FRAME fMain:HANDLE,iCurNameField).

   IF VALID-HANDLE(vHTmp) THEN
      vHTmp:SCREEN-VALUE = currency.name-currenc.

   IF iMode NE {&MOD_VIEW} THEN
   DO:
      IF NOT mFirstRun THEN
      DO:

         /* ����᪠�� ���᫥��� ��⮢ */
         /* ��� �⮣� �����뢠�� �࠭��� ���祭�� � ���� �࠭���樨 */
         RUN AddAttr2TableEx(?,
                             vTmplID,
                             ?,
                             ?,
                             vTmplLnID,
                             "currency",
                             iCur).

         /* ����� ����� */
         IF {assigned vFormulae} THEN
         DO:
            RUN ParsMain(vFormulae, vPrepFormulae,
                         GetXAttrValueEx("op-kind", mOpKind, "ParsLib", ?),
                         OUTPUT vFlagErr, OUTPUT vResult).
            IF vFlagErr >= 0 THEN
            DO:
               {find-act.i &bact=bAcct &acct=vResult &curr=iCur}
               IF iPosNameField = "mPos1" THEN
               DO:
                  IF AVAIL bAcct THEN
                  DO:
                     ASSIGN
                        tt-op-entry1.acct-cr:SCREEN-VALUE = bAcct.acct
                        tt-op-entry1.acct-cr              = bAcct.acct
                        .
                  END.
               END.
               ELSE
               DO:
                  IF AVAIL bAcct THEN
                  DO:
                     ASSIGN
                        tt-op-entry2.acct-db:SCREEN-VALUE = bAcct.acct
                        tt-op-entry2.acct-db              = bAcct.acct
                        .
                  END.
               END.
            END. /* IF vFlagErr >= 0 THEN */
            ELSE
            DO:
               mlgError = YES.
               APPLY "END-ERROR" TO FRAME {&MAIN-FRAME}.
               RETURN NO-APPLY {&RET-ERROR}.
            END.
         END. /* IF {assigned vFormulae} THEN */

/* ����㥬 �� �࠭� �� ��� ����樨, ����� ����஬ � 蠡���� �࠭���樨 ��।������ 
   ��� ����樨 ������ �� ���� �������� ������  (32294) */
         IF iPosNameField EQ "mPos1" OR 
         (CAN-DO("�믫��,���᫥���",mTypeOper) AND 
          iPosNameField EQ "mPos2")   THEN
         DO:
            ASSIGN
               vFormulae = mFormulaeVidOpNal
               vPrepFormulae = mPrepFormulaeVidOpNal
            .
            IF {assigned vFormulae} THEN
            DO:
               RUN ParsMain(vFormulae, vPrepFormulae,
                            GetXAttrValueEx("op-kind", mOpKind, "ParsLib", ?),
                            OUTPUT vFlagErr, OUTPUT vResult).
               IF vFlagErr >= 0 THEN
               DO:
                  tt-vok-val.vidopnalv$:SCREEN-VALUE = vResult.
                  IF tt-vok-val.vidopnalv$:SCREEN-VALUE NE "" THEN
                  DO:
                     vChVidOpNal = GetCodeName("���������",tt-vok-val.vidopnalv$:SCREEN-VALUE).
                     vChVidOpNal:DCOLOR = GetDCOLOR("bright-green").
                  END.
                  ELSE 
                     vChVidOpNal = "".
                  DISPLAY 
                     vChVidOpNal
                     WITH FRAME {&MAIN-FRAME}.
               END. /* IF vFlagErr >= 0 THEN */
            END. /* IF {assigned vFormulae} THEN */
         END. /* ⮫쪮 ��� iPosNameField = "mPos1" */
      END. /* IF NOT mFirstRun THEN */
      ELSE
      DO:
         IF iPosNameField = "mPos1" THEN
         DO:
            {find-act.i &bact=bAcct &acct=tt-op-entry1.acct-cr &curr=iCur}
         END.
         ELSE
         DO:
            {find-act.i &bact=bAcct &acct=tt-op-entry2.acct-db &curr=iCur}
         END.
      END.
      IF AVAILABLE bAcct THEN
      DO:
       /* ���⮪ �� ������ */
         vHTmp = GetWidgetHandle(FRAME fMain:HANDLE, iPosNameField).
         RUN ViewAcctPos(bAcct.acct,
                         bAcct.currency,
                         mDpr-id,
                         vHTmp). 

      END.
   END.

   /* ���� */
   DEFINE VARIABLE vRateType AS CHARACTER NO-UNDO. /* ⨯ ���� */
   DEFINE VARIABLE vS        AS CHARACTER NO-UNDO. /* ���-�६� ���� */

   DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN
         tt-op-entry1.currency = tt-op-entry1.currency:SCREEN-VALUE
         tt-op-entry2.currency = tt-op-entry2.currency:SCREEN-VALUE
         .
   END.

   IF tt-op-entry1.currency:SCREEN-VALUE NE "?" AND 
      tt-op-entry2.currency:SCREEN-VALUE NE "?" AND 
      iRateCalc THEN
   DO: 
      IF tt-op-entry1.currency:SCREEN-VALUE EQ 
         tt-op-entry2.currency:SCREEN-VALUE THEN
      DO: /* ������ ��������� */
         /* ��祣� �� ���� �� ���� */
         vCrossRate = 1.
         vRateDate  = TODAY.
         vRateTime  = TIME.
      END. /* ������ ��������� */
      ELSE
      DO:  /* ������ �⫨����� ��� �� ��㣠 */
         /* ��।���� ⨯ ���� */
         CASE tt-vok-val.class-code: /* �� ������� � �����⥦ ���죠�� */
            WHEN "vok-val-nonpaym-buy" THEN 
            DO:
               IF tt-op-entry1.currency:SCREEN-VALUE EQ "" OR
                  tt-op-entry2.currency:SCREEN-VALUE EQ "" THEN
               /* ��� ����樨 � �㡫ﬨ ��ᬮ�ਬ �� �� �࠭���樨 */
                  vRateType = GetXattrValueEx("op-kind",mOpKind,"�������","�������").
               ELSE
               /* ��� ����樨 � ����� ⨯ ���� ��।���� �� �� �뤠������ ������ */
                  vRateType = GetXattrValueEx("currency",
                                              tt-op-entry1.currency:SCREEN-VALUE,
                                              "�������",
                                              "�������").
                  vCurrIn  = tt-op-entry2.currency:SCREEN-VALUE. /* iInCurr */
                  vCurrOut = tt-op-entry1.currency:SCREEN-VALUE. /* iOutCurr */
            END.
            OTHERWISE /* �� ������� � ��ଠ��묨 ���죠�� */
            DO:
               IF CAN-DO("�믫��,���᫥���",mTypeOper) THEN  /* � 蠡����� ������ ��६����� @_TypeOper */ 
               DO:
   /* 
      � �� �㦭� ��� ����権 � ��⠬� 䨧. ���. 
      � �࠭���樨 "�믮����� ��" ��⠭���� ��६�����, 
      ���ਬ��, � �믏���� @_TypeOper = "�믫��" 
   */ 
                  IF tt-op-entry1.currency:SCREEN-VALUE NE "" AND
                     tt-op-entry2.currency:SCREEN-VALUE NE "" THEN
                  DO:
                  /* �᫨ ��� ������ �� �㡫�, � ⨯ ���� ��।���� �� �� �뤠������ ������ */
                     vRateType = GetXattrValueEx("currency",
                                                 tt-op-entry1.currency:SCREEN-VALUE,
                                                 "�������",
                                                 "�������").
                     CASE mTypeOper:
                        WHEN "�믫��" THEN 
                        DO:
                           vCurrIn   = tt-op-entry2.currency:SCREEN-VALUE.
                           vCurrOut  = tt-op-entry1.currency:SCREEN-VALUE.
                        END.
                        WHEN "���᫥���" THEN 
                        DO:
                           vCurrIn   = tt-op-entry1.currency:SCREEN-VALUE.
                           vCurrOut  = tt-op-entry2.currency:SCREEN-VALUE.
                        END.               
                     END CASE. /* mTypeOper */
                  END.
                  ELSE
                     CASE mTypeOper:
                        WHEN "�믫��" THEN 
                     DO:
                        IF tt-op-entry1.currency:SCREEN-VALUE EQ "" AND /* ���� */
                           tt-op-entry2.currency:SCREEN-VALUE NE "" THEN /* ������� */
                        DO:
                           vRateType = "���㯪�".
                           vCurrIn   = tt-op-entry2.currency:SCREEN-VALUE.
                           vCurrOut  = tt-op-entry1.currency:SCREEN-VALUE.
                        END.

                        IF tt-op-entry1.currency:SCREEN-VALUE NE "" AND
                           tt-op-entry2.currency:SCREEN-VALUE EQ "" THEN
                        DO:
                           vRateType = "�த���".
                           vCurrIn   = tt-op-entry1.currency:SCREEN-VALUE.
                           vCurrOut  = tt-op-entry2.currency:SCREEN-VALUE.
                        END.
                     END.
                     WHEN "���᫥���" THEN 
                     DO:
                        IF tt-op-entry1.currency:SCREEN-VALUE EQ "" AND /* ���� */
                           tt-op-entry2.currency:SCREEN-VALUE NE "" THEN /* ������� */
                        DO:
                           vRateType = "�த���".
                           vCurrIn   = tt-op-entry2.currency:SCREEN-VALUE.
                           vCurrOut  = tt-op-entry1.currency:SCREEN-VALUE.
                        END.

                        IF tt-op-entry1.currency:SCREEN-VALUE NE "" AND
                           tt-op-entry2.currency:SCREEN-VALUE EQ "" THEN
                        DO:
                           vRateType = "���㯪�".
                           vCurrIn   = tt-op-entry1.currency:SCREEN-VALUE.
                           vCurrOut  = tt-op-entry2.currency:SCREEN-VALUE.
                        END.
                     END.
                  END CASE. /* mTypeOper */
               END. /* � 蠡����� ������ @_TypeOper */
               ELSE /* � 蠡����� �� ������ ��६����� @_TypeOper */
               DO: /* �� ���㯪�, �த���, �������� */
                  /* ���砫� ��ᬮ�ਬ �� �� �࠭���樨 */
                  vRateType = GetXattrValueEx("op-kind",mOpKind,"�������","FromCurrency").
                  /* �᫨ �� �࠭���樨 �� �����, � ⨯ ���� ��।���� �� �� �뤠������ ������ */
                  IF vRateType EQ "FromCurrency" THEN 
                     vRateType = GetXattrValueEx("currency",
                                                 tt-op-entry1.currency:SCREEN-VALUE,
                                                 "�������",
                                                 "�������").
                  IF tt-op-entry2.currency:SCREEN-VALUE EQ "" THEN
                  DO:
                     vCurrIn  = tt-op-entry1.currency:SCREEN-VALUE.
                     vCurrOut = tt-op-entry2.currency:SCREEN-VALUE.
                  END.
                  ELSE 
                  DO:
                     vCurrIn  = tt-op-entry2.currency:SCREEN-VALUE. /* iInCurr */
                     vCurrOut = tt-op-entry1.currency:SCREEN-VALUE. /* iOutCurr */
                  END.  
               END. /* � 蠡����� �� ������ ��६����� @_TypeOper */
            END. /* OTHERWISE */
         END CASE. /* class-code */

         /* ������� */
         DEFINE VARIABLE mMessQ AS CHARACTER   NO-UNDO.
         IF NOT SetQuestion THEN
         DO:
            vLogSpecialRate = IF FGetSetting("�������","","���") EQ "���" THEN NO ELSE YES.  
            mgLogUBound     = IF FGetSetting("����㬬�","","���") EQ "���" THEN NO ELSE YES. 

            IF vLogSpecialRate THEN
               mMessQ = "���� ᯥ樠�쭮�� ����".
            IF mgLogUBound THEN
               mMessQ = mMessQ + IF mMessQ NE "" THEN ("," + "���� � ����ᨬ��� �� �㬬� ����樨") ELSE "���� � ����ᨬ��� �� �㬬� ����樨".
            mMessQ = mMessQ + IF mMessQ NE "" THEN ("," + "�⬥��") ELSE "�⬥��".
     
            pick-value = "". 
            IF (vLogSpecialRate OR mgLogUBound) THEN
            DO:
               SetQuestion  = YES.
               RUN messmenu.p (10, 
                               "", 
                               "", 
                               mMessQ
                               ).
            END.
            pick-value = IF (pick-value EQ "0" OR pick-value EQ "") THEN STRING(NUM-ENTRIES(mMessQ)) ELSE pick-value.
            mgLogUBound     = IF ENTRY(INT64(pick-value),mMessQ) EQ "���� � ����ᨬ��� �� �㬬� ����樨"  THEN YES ELSE NO.
            vLogSpecialRate = IF ENTRY(INT64(pick-value),mMessQ) EQ "���� ᯥ樠�쭮�� ����" THEN YES ELSE NO.
         END.

         IF vLogSpecialRate THEN
         DO:
               ASSIGN
                  SpecialRate:HIDDEN        IN FRAME {&MAIN-FRAME} = NO
                  SpecialRate:SENSITIVE     IN FRAME {&MAIN-FRAME} = YES
                  &IF DEFINED(MANUAL-REMOTE) &THEN
                  tt-vok-val.sprate:HIDDEN  IN FRAME {&MAIN-FRAME} = YES
                  &ENDIF
                  .
               vRateDate  = TODAY.
               vRateTime  = TIME.
         END.
         ELSE
         DO:
            IF ChkRateTime(vRateType) = ?THEN 
            DO:
               RUN Fill-SysMes("",
                               "",
                               "-1",  /* �訡�� */
                               "��� ���� """ + vRateType + """ � �ࠢ�筨�� ���ᮢ �� ������.").
               RETURN NO-APPLY {&RET-ERROR}.
            END.  
           
            /* ��।���� ���� ����樨 � ��� ����-�६� */
            RUN CrossRateTimeR(vRateType,
                              vCurrIn, /* iInCurr */
                              vCurrOut, /* iOutCurr */
                              mBranch-Id,
                              {&OP_TRANSACTION_BEG_DATE},
                              {&OP_TRANSACTION_BEG_TIME},
                              DEC(0),
                              OUTPUT vCrossRate,
                              OUTPUT vRateDate,
                              OUTPUT vRateTime).
            mDiffRate = IF RETURN-VALUE NE "" THEN RETURN-VALUE ELSE "".
         END. /* ������ �⫨����� ��� �� ��㣠 */

         IF GetAttrValue2("",0,"tmp_sprite") EQ {&RET-ERROR} THEN
            tt-vok-val.sprate:SCREEN-VALUE = STRING(vCrossRate).
            vS    = STRING(vRateDate) + " " + STRING(vRateTime, "HH:MM").
            
            vHTmp = GetWidgetHandle(FRAME fMain:HANDLE, "mRateDateTime").
            
            /* �������� ��ࠬ���� ���� ��� �ᯮ�짮����� ���� � ����ᨬ��� �� �㬬� */
            ASSIGN
               mgRate-Type = vRateType
               mgInCurr    = vCurrIn
               mgOutCurr   = vCurrOut
               mgBranch-Id = mBranch-Id
               mgRDate     = {&OP_TRANSACTION_BEG_DATE}
               mgRTime     = {&OP_TRANSACTION_BEG_TIME}
               mgHSprate   = GetWidgetHandle(FRAME fMain:HANDLE, "tt-vok-val.sprate")
               mgHDTSprate = vHTmp
               .
           
            IF VALID-HANDLE(vHTmp) THEN
               vHTmp:SCREEN-VALUE = vS.
            END.
         
         /* ����� ������� */
   END. /* */
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
  HIDE FRAME fPerson.
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
  DISPLAY mLabelOut mLabelIn mCont-Code mLoanAcct curr1-name SpecialRate 
          mTCode2Name vChVidOpNal mUser-Id mDpr-Num mBranch-Id Separator1 
          curr2-name mTCode1Name mPos1 mPos2 mRateDateTime 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-op-entry1 THEN 
    DISPLAY tt-op-entry1.acct-cr tt-op-entry1.currency tt-op-entry1.amt-cur 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-op-entry2 THEN 
    DISPLAY tt-op-entry2.acct-db tt-op-entry2.currency tt-op-entry2.amt-cur 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-vok-val THEN 
    DISPLAY tt-vok-val.TrCodeCr tt-vok-val.TrCodeDB tt-vok-val.sprate 
          tt-vok-val.vidopnalv$ tt-vok-val.vkl113i$ 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE mLabelOut mLabelIn tt-op-entry1.acct-cr tt-op-entry2.acct-db 
         mCont-Code mLoanAcct tt-op-entry1.currency tt-vok-val.TrCodeCr 
         tt-op-entry2.currency tt-vok-val.TrCodeDB tt-vok-val.sprate 
         tt-op-entry1.amt-cur tt-op-entry2.amt-cur tt-vok-val.vidopnalv$ 
         vChVidOpNal tt-vok-val.vkl113i$ Separator1 curr2-name RECT-1 RECT-2 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW FRAME fPerson IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fPerson}
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
    /* �������� �३� � ����묨 � ������ */


    IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES, "PersED") THEN
       RUN PersED NO-ERROR.

    /* ��⠭�������� ��������� ���� */
    DEFINE VARIABLE vTitle AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vOpKind AS CHARACTER NO-UNDO.

    DEFINE BUFFER bOpKind FOR op-kind.
    DEFINE BUFFER bSessions FOR sessions.

    FIND FIRST bSessions WHERE bSessions.dpr-id = mDpr-Id
       NO-LOCK
       NO-ERROR.
    IF AVAIL bSessions THEN
       ASSIGN
          mDpr-Num   = bSessions.dpr-num
          mBranch-Id = bSessions.branch-id
          mUser-Id   = bSessions.user-id
          mOpDate    = bSessions.op-date
       .

    DISPLAY mUser-Id mDpr-Num mBranch-Id WITH FRAME fMain.

    vTitle = "���ᮢ�� ������ ���".
    vOpKind = GetBaseOpkind().
    IF vOpKind <> ""
       AND vOpKind <> ? THEN
    DO:
       FIND FIRST bOpKind WHERE bOpKind.op-kind = vOpKind
          NO-LOCK
          NO-ERROR.
       IF AVAIL bOpKind THEN
          vTitle = bOpKind.name-opkind.
    END.
    vTitle = "[ " + CAPS(vTitle + ": ����� " + STRING(mDpr-Num, "99999") + " �� " + STRING(mOpDate)) + " ]".

    FRAME fMain:TITLE = vTitle.

    SpecialRate:HIDDEN IN FRAME {&MAIN-FRAME} = YES.

    IF tt-op-entry1.currency:SCREEN-VALUE <> "?" AND tt-op-entry2.currency:SCREEN-VALUE <> "?" THEN
    ASSIGN
       mValChCurrDb = YES
       mValChCurrCr = YES
       .

    IF tt-op-entry1.currency:SCREEN-VALUE <> "?" THEN
       APPLY "LEAVE" TO tt-op-entry1.currency IN FRAME fMain.
    IF tt-op-entry2.currency:SCREEN-VALUE <> "?" THEN
       APPLY "LEAVE" TO tt-op-entry2.currency IN FRAME fMain.
    
    HIDE tt-op-entry1.acct-cr.
    HIDE tt-op-entry2.acct-db.

    IF tt-op-entry1.currency:SCREEN-VALUE EQ "" THEN
    DO:
       DISABLE tt-op-entry1.amt-cur WITH FRAME fMain.
       DISABLE tt-op-entry1.currency WITH FRAME fMain.
    END.
    IF tt-op-entry2.currency:SCREEN-VALUE EQ "" THEN
    DO:
       IF NOT mHandInput THEN
       DO:
          DISABLE tt-op-entry2.amt-cur WITH FRAME fMain.
       END.
       DISABLE tt-op-entry2.currency WITH FRAME fMain.
    END.
    IF     tt-op-entry1.currency:SCREEN-VALUE NE ""
       AND tt-op-entry2.currency:SCREEN-VALUE NE "" THEN
    DO:
       DISABLE tt-op-entry1.amt-cur WITH FRAME fMain.
    END.
    DISABLE tt-vok-val.sprate WITH FRAME fMain. /* ���� �� �ࠢ�� �㪠�� */
    
    IF vLogOff113i THEN
    DO:
       IF tt-vok-val.vidopnalv$:SCREEN-VALUE NE "" THEN
       DO:
          APPLY "LEAVE" TO tt-vok-val.vidopnalv$ IN FRAME {&MAIN-FRAME}.
          tt-vok-val.vidopnalv$:SENSITIVE = NO.
       END.
    END.

/* �᫨ �㦭�, � � �࠭���樨 "�믮����� ��" ��⠭���� ��६�����, 
  ���ਬ��, � �믏���� @_TypeOper = "�믫��" */
    mTypeOper = GetAttrValue2("",0,"_TypeOper").
              
    IF mTypeOper EQ {&RET-ERROR} THEN
       mTypeOper = "".

    ASSIGN
       mCont-Code:HIDDEN = YES
       mLoanAcct:HIDDEN  = YES
       .

    CASE mTypeOper:
       WHEN "�믫��" OR WHEN "���᫥���" THEN
       DO:
          ASSIGN
             mLabelOut:SCREEN-VALUE = "[ �����  ]"
             mLabelIn:SCREEN-VALUE  = "[ �������  ]"
             mCont-Code:HIDDEN = NO
             mLoanAcct:HIDDEN  = NO
             .
          DISABLE 
             tt-op-entry2.currency  /* ������ ��� �� �ࠢ�� �㪠�� */
             tt-op-entry1.amt-cur 
             tt-op-entry2.amt-cur 
             mLoanAcct 
          WITH FRAME {&MAIN-FRAME}.
       END.
       WHEN "���������" THEN
       DO:
          /* ��⠢�塞 ⮫쪮 �㬬� */
          DISABLE
             tt-op-entry2.currency 
             tt-vok-val.TrCodeCr
             tt-vok-val.TrCodeDB
             WITH FRAME {&MAIN-FRAME}.

          DISABLE
             tt-vok-val.fio$
             tt-vok-val.dover$
             tt-vok-val.country-pers
             tt-vok-val.document-id
             tt-vok-val.dokum$
             tt-vok-val.cust-doc-who
             tt-vok-val.Document4Date_vid
             tt-vok-val.adres$
             tt-vok-val.BirthPlace
             tt-vok-val.Birthday
             WITH FRAME {&PERS-FRAME}.

          /* �᫨ ��᢮��� ࠭�� ��� � 蠡�����, 
               � ��������� �஡���� � �ਣ�ࠬ� */
          ASSIGN
             tt-op-entry2.amt-cur:SCREEN-VALUE = GetAttrValue2("",0,"amt-conv")
             tt-op-entry1.amt-cur:SCREEN-VALUE = STRING(DEC(GetAttrValue2("",0,"amt-conv")) * DEC(GetAttrValue2("",0,"tmp_sprite")))
             .
       END.
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
   
   IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES, "PersSO") THEN
      RUN PersSO NO-ERROR.
   RETURN.

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
   RUN CalcSumm(?,?).
{vlocalgo.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TCode_Leave TERMINAL-SIMULATION 
PROCEDURE TCode_Leave :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER iCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iCodeNameField AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vHTmp  AS HANDLE NO-UNDO.
   DEFINE VARIABLE vCodeName AS CHARACTER NO-UNDO.

   vCodeName = GetCodeName("����������", iCode).

   IF vCodeName = ? THEN
   DO:
      MESSAGE "��� 業���� � ����� " iCode
         VIEW-AS ALERT-BOX ERROR.
      RETURN {&RET-ERROR}.
   END.

   vHTmp = GetWidgetHandle(FRAME fMain:HANDLE,iCodeNameField).

   IF VALID-HANDLE(vHTmp) THEN
      vHTmp:SCREEN-VALUE = vCodeName.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTFILE='vok-val.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='stre' */
/* $LINTDATE='29/06/2017 15:40:19.920+03:00' */
/*prosignbjphm/0cY8g/jf89BXk+bQ*/