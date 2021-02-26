/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: XATTR-ED.P
      Comment: ���� ���७��� ४����⮢
   Parameters: in-class-code  - ����� ��ꥪ�
               in-surrogate   - ���ண�� ��ꥪ�
               in-title       - ��������� ���
               in-create      - ०�� ࠡ���
               level          - Y-���न��� ���
      Created: 11/09/97 Serge
-------------------------------------------------------------------------------
     Modified: 11/09/1997 Serge  -
     Modified: 01/10/1997 eagle  -
     Modified: 23/02/1998 Olenka - �஢�ઠ class.progress-table
     Modified: 13/05/1998 mkv    - �᫨ ����� �����, ��騩�� �����䨪��஬
                                   � ���� code.misc[3] �� ����, � ��楤�� ������
                                   �� �⮣� ����, ���� pclass
     Modified: 19/01/1999 Serge  - ᤥ��� ����������� ��।�� in-surrogate �
                                   ��楤��� ������樨 �१ ��ࠬ��� "id"
                                   ���஢�� �� ������������
                                   temp-table - shared
     Modified: 12/03/1999 Dima   - XAttrList ⥯��� ᮤ�ন� ᯨ᮪ Xattr'��,
                                   ����� ����� �������� ᠬ�� _User'��.
     Modified: 09/04/1999 eagle  - �뢮� Xattr-code,
     Modified: 12/07/2001 yakv   - global shared XATTR_Params = ���ଠ�� �
                                   ।����㥬�� ����� (class,surrogate)
-----------------------------------------------------------------------------
     Modified: 05.06.02 LER 5766 - �맮� ��㧥஢ _CODE_ {call-brc.i}
     Modified: 24.07.02 GunK     - xattr-value.mandatory ����� xattr.mandatory
     Modified: 24.10.02 ler 8029 - 㭨䨪���� �롮� � ������樨 ���祭�� ���.४�����,
                                   �롮� � �������� ��砫쭮�� ���祭�� ���.४����� (xattr.initial).
     Modified: 18.11.02 LER 11551 ���䨪��� ��ࠡ�⪨ ४����⮢ ��� �᭮���� � ����ୠ⨢��� ��.
                                  � ����ୠ⨢��� �ଠ� ��ᬮ�� ⥯��� �믮������ �஢�ઠ ��易⥫쭮��
                                  ���. ४����⮢ (��� ��易⥫��� ���. ४����⮢ ���� �� ����� ��⠢����� �����).
                                  ���祭�� ४�. �� ������ � ��. � domain-code �� �஢������ �� ���⮬ ����.
     Modified: 06.04.04 ligp    -   � ����� _user ����� ��������, ���஬� ⮦� ����� ������ 
                                   ᠬ��� ᥡ� ���. ४������ (��� 24593)
     Modified: 23/05/2004 Om  ��ࠡ�⪠:
                                 1. �� �室� � ०��� "ᮧ�����" ��ࠦ��� ��:
                                 - ��᢮����,
                                 - ��易⥫��,
                                 - �� ��⠫��, � ������ ��砫쭮� ���祭��
                                   �⫨筮 �� ���⮩ ��ப�.
                                 2. �����뢠�� ��, ⮫쪮 ��᫥ ।���஢����.
     Modified: 23/05/2004 Om  �訡��:
                                 ���࠭���� �訡�� "�� ������ ������ xattr-value",
                                 �� �室� � ���⮩ ��� (०�� ।���஢����).
    Modified: 19/11/2004 Om  ��ࠡ�⪠:
                              1. �� �室� � ०��� "।���஢����",
                                 ����୮ �⮡ࠦ����� �������� �������.
                              2. �� ᮮ�饭�� ॠ�������� �१ �㦡� ᮮ�饭��.
                              3. �� �⬥�� ।���஢���� �� �ந���������
                                 ���������� �࠭�.
    Modified: 10/02/2005 Om  ��ࠡ�⪠:
                              ���짮��⥫� ���ॡ������� ����� ४�����
                              �ଠ⮬ x(3000). ��࠭�祭�� ���� �뫮 655.
   Modified: 09/03/2005 Om  ��ࠡ�⪠.
                        ������祭 ��堭�� ���樠����樨 ��.
   Modified: 01/04/2005 Xaro ��ࠡ�⪠.
                        ������祭 �맮� ��⮤� �஢�ન ��쥪� 楫����
   Modified: 10/08/2005 Om  ��ࠡ�⪠.
                        �⪫�祭 ��堭�� �஢�ન ��ꥪ� 楫���� �� ॠ����樨 ��������.
   Modified: 23/09/2005 Om  ��ࠡ�⪠.
                        ��������� �������� ������� 䨫���.
   Modified: 04/05/2006 Shib  ��� 0058996.
                        ��������� �஢�ઠ �� ����稥 �ࠢ� ।���஢���� ���.४����⮢.    
   Modified: 31/10/2006 Ariz ��ࠡ�⪠ (0055955)
                        ��������� ��ࠡ�⪠ ��⮤� GetXValDesc ��� ����祭�� �易����
                        ���ଠ樨 �� ���祭�� ���४�����.
                        ����������� 㪠���� �� ���४�����:
                           ��⮤ GetXValDesc - ᮡ�⢥��� ��⮤ ����祭�� ���ᠭ�� ���祭�� ��,
                        �뢮������ � ����� "����.��ࠬ"/"����.��ࠬ".
                        ��楤��, �������� � �⮬ ��⮤�, ������ �������� � pick-value
                        "����.��ࠬ"/"����.��ࠬ" � ࠧ����⥫�� CHR(2) ("����.��ࠬ" + CHR(2) + "����.��ࠬ")
                        ��� ���� pick-value = "����.��ࠬ".
     Modified: 17.07.2007 20:43 KSV      (0078824) �����஢�� ��� ���ᬠ��
     Modified: 30.08.2007 16:39 Daru 
     Modified: 10.09.2007 10:56 KSV      (0080717) ��ࠢ���� �訡�� ��࠭����
                                         ���. ४�� ���짮��⥫�
     Modified: 12.02.2009 17:16 KSV      (0106088) ������. ��ࠢ�����
     Modified: 03.03.2009 12:55 ariz     ������஢���� ४������ ���ᢥ稢�����
                                         梥⮬ ������� �� TempXAttrColor,
                                         �᫨ �� ����, �� ���ᢥ稢�����.
     Modified: 15.04.2009 20:03 KSV      (0108589) ��������� �����প� ⥬���.
                                         ���४�� � QBIS
     Modified: 07.07.2009 19:59 ariz     0128939: � �⠭���� 0128212
     Modified: 11.09.2009 18:27:46 ksv   (0111396) �����প� ��㯯�஢�� � QBIS
     Modified: 25.03.2010 16:53 ksv      (0110628) + ��� ᥬ��� CloseOnGo     
     Modified: 01.04.2010 16:12 solm     (0125834) + ���᪠ ⥬���஢�����
                                          ���. ४����⮢ ��� QBIS          
*/
/******************************************************************************/

DEF INPUT PARAM in-class-code LIKE class.class-code NO-UNDO.
DEF INPUT PARAM in-surrogate  LIKE signs.surrogate  NO-UNDO.
DEF INPUT PARAM in-title      AS CHAR NO-UNDO.
DEF INPUT PARAM in-create     AS CHAR NO-UNDO.
DEF INPUT PARAM level         AS INT64  NO-UNDO.

{globals.i}             /* �������� ��६���� ��ᨨ. */
{form.def}              /* ����⠭�� ���짮��⥫�᪮�� ����䥩�*/
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{usform.i}              /* ��楤��� ��� �롮� ���祭�� �� ࠡ�� � extedit.p */

                        /* ���樠������ ����� ��⮪���஢����. */
RUN Init-SysMes ("", "", "").
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */
{intrface.get data}     /* ������⥪� �㭪権, ࠡ����� � ��易��� ����஬ ������
                           (Instance) �� �᭮�� ���ଠ樨, �࠭�饩�� � ����奬�. */
{intrface.get hist} 
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */
{xattr-ed.def NEW}      /* ������ ��� �⮡ࠦ���� ����ᥩ. */
{userconf.def NEW}      /* ���� ��� ���᪠ ����ன�� 䨫���. */
                        /* ��ନ஢���� ���� ��� ����㧪� 䨫��஢. */
&GLOBAL-DEFINE no_userconf_get   YES

DEFINE VARIABLE params_   AS CHARACTER NO-UNDO.
DEF VAR proc-name         AS CHAR  NO-UNDO.    /* ��� ��楤���. */
DEF VAR vProcNameLook     AS CHAR NO-UNDO.     /*��楤�� ��ᬮ��*/
DEF VAR vParamLook        AS CHAR NO-UNDO.     /*��ࠬ���� ��楤��� ��ᬮ��*/
DEFINE VARIABLE vNoCheck  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE vTempControl  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vChoice   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vPrevVal  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mStatusMandatFld AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mStatusEditFld AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vStatus     AS CHARACTER NO-UNDO.

DEF VAR vSignsDt    AS DATE   NO-UNDO. /* ���, �� ������ ��।��塞 ���祭�� ��. */
   vSignsDt = IF gend-date NE ? THEN gend-date ELSE gend-hdate.
   
/* ������� �����᪨ �� ��⮤� */
SUBSCRIBE TO "GetTTXattrValue" ANYWHERE.

SUBSCRIBE TO "SetTTXattrValue" ANYWHERE.

/* ��⠭�������� ᥬ��� CloseOnGo � ���祭�� YES, ���஥ �������� �ᥬ ��㧥ࠬ
** ��뢠�騬�� ���, ����뢠���� �� ᮡ��� GO, �� �᫮���, �� �� ����筮
** � ��� �।�ᬮ�७� ����砫쭮 */
RUN SetCloseOnGoSemaphore IN h_base ( THIS-PROCEDURE, YES ).
 
/* ���. ����ன�� 䨫��� ��� ��⮤� browse ����� ��ꥪ� */
RUN GetClassMethod IN h_xclass (
    in-class-code, /* ����� ��ꥪ�. */               
    "browse",    /* ��� ��⮤�. */                  
    "","",
    OUTPUT proc-name,
    OUTPUT params_). 
/* ��ࠡ�⪠ �訡��. */
IF    proc-name EQ ? 
   OR proc-name EQ '""' 
   OR proc-name EQ "''" 
   THEN 
      user-config-info = "XATTR-ED.P," + in-class-code + ",?,". 
   ELSE 
      user-config-info = "XATTR-ED.P," + proc-name + ",?,".

{flt-file.i NEW}        /* ��।������ �������� �������᪮�� 䨫���. */


DEF VAR list-class     AS CHAR NO-UNDO. /* ����室�� flt-file.end. */
DEF VAR num-class      AS INT64  NO-UNDO. /* ���稪. */
                        /* ��।��塞 ���� ��� 䨫���樨. */
{flt-file.add
   &cat     = 1
   &labelt  = "('�������⥫�� ४������')"
   &tablef  = in-class-code
   &include = "'1,IsEndDte,2,sc-1,Name'"
}
{flt-file.atr
   &asgn          = YES
   &xcode         = "'IsEndDte'"
   &a-code        = "'IsEndDte'"
   &a-datatype    = "'LOGICAL'"
   &a-label       = "'��������:'"
   &a-help        = "'F1 - �롥�� ०�� �⮡ࠦ����'"
   &a-format      = "'�� ४������/��᢮���� ४������'"
   &a-initial     = "'NO'"
   &a-HotKey      = "KEY-CODE ('Ctrl-F3')"
   &a-HotSet      = "YES"
}
{flt-file.atr
   &asgn          = YES
   &xcode         = "'sc-1'"
   &a-code        = "'sc-1'"
   &a-multi       = "YES"
}
{flt-file.atr
   &asgn          = YES
   &xcode         = "'Name'"
   &a-code        = "'Name'"
   &a-datatype    = "'CHARACTER'"
   &a-label       = "'�������� ४�����:'"
   &a-help        = "'�������� ४�����.'"
   &a-procename   = "'no-proc'"
   &a-param       = "'1'"
}
{emptyfld.atr 1}
{emptyfld.atr 2}
{flt-file.end}          /* ����砭�� �ନ஢���� �������� 䨫���. */

&GLOBAL-DEFINE XattrWhere                                            ~
   WHERE                                                             ~
           (IF GetFltVal ("sc-1")   EQ "*"                           ~
               THEN YES                                              ~
               ELSE CAN-DO (GetFltVal ("sc-1"), xattr-value.code))   ~
      AND  (IF GetFltVal ("name")   EQ "*"                           ~
               THEN YES                                              ~
               ELSE CAN-DO (GetFltVal ("name"), xattr-value.name))   ~
      AND  (IF vall                                                  ~
               THEN YES                                              ~
               ELSE (   xattr-value.code-value GT ''                 ~
                     OR xattr-value.since NE ?                       ~
                     OR xattr-value.mandatory                        ~
                     OR xattr-value.lastattr                         ~
                     OR (mByGroup                                    ~
                         AND xattr-value.code-value  EQ ""           ~
                         AND xattr-value.CLASS       EQ ""           ~
                         AND xattr-value.xattr-group NE "") ))       ~
      AND  (IF mGroupList EQ "*"                                     ~
               THEN YES                                              ~
               ELSE xattr-value.xattr-group EQ mGroupList)

&GLOBAL-DEFINE ByGroupSort BY xattr-value.xattr-group BY xattr-value.NAME

&GLOBAL-DEFINE BTNF9    "F9-।���஢���"
&GLOBAL-DEFINE BTNESC   "ESC - �������� ।-���"

DEF NEW GLOBAL SHARED VAR clip-buffer AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE browse-height 9
&GLOBAL-DEFINE mainmess "F6-䨫���"

DEF VAR mWhereSurr    AS CHAR  NO-UNDO.
DEF VAR mWhereSurrOld AS CHAR  NO-UNDO.
DEF VAR mTMP          AS CHAR  NO-UNDO.

mTMP = GetNewSurrogate(GetXclassProgress(in-class-code),
                       in-surrogate,
                       OUTPUT mWhereSurr,
                       OUTPUT mWhereSurrOld).
IF {assigned mTMP} THEN
   in-surrogate = mTMP.

level = 4.

DEF NEW GLOBAL SHARED VAR XATTR_Params AS CHAR INIT "" NO-UNDO.
ASSIGN XATTR_Params = in-class-code + "," + in-surrogate.

/* ��६����  */    /* DEF VAR tmp_log AS log NO-UNDO. */
DEF VAR any-entered AS LOG    NO-UNDO. /* �������� �� ��-�����. */
DEF VAR vparam      AS CHAR   NO-UNDO.
DEF VAR vall        AS LOG    NO-UNDO.
DEF VAR num-row     AS INT64  NO-UNDO.
DEF VAR vclass      LIKE class.class-code NO-UNDO.
DEF VAR cn          AS CHAR   NO-UNDO. /* �������� �����. */
DEF VAR mXattrCode  AS CHAR   NO-UNDO. /* ��� ����樮��஢���� �� ������. */

DEF VAR vsep        AS CHAR   NO-UNDO FORMAT "x".
DEF VAR vDN         LIKE xattr-value.displ-name. /* AS CHAR FORM "x" NO-UNDO. */
DEF VAR vl          AS LOG    NO-UNDO.
DEF VAR edit-mode   AS log    NO-UNDO.
DEF VAR cclass      LIKE class.class-code NO-UNDO.
DEF VAR vdcolor     AS INT64  NO-UNDO.
DEF VAR mStartMode  AS LOG    NO-UNDO. /* ����� � ����� ���뢠���� ���. */
DEF VAR mTempColor  AS CHAR   NO-UNDO. /* ���� �⮡ࠦ���� ⥬���஢����� �� */
/* ��६���� ��� ���᪠ � ।���� �� */
DEF VAR vCodeFnd    AS CHAR   NO-UNDO. /* ���� ��� �� */
DEF VAR vNameFnd    AS CHAR   NO-UNDO. /* ���� ����. �� */
DEF VAR vCurrROWID  AS ROWID  NO-UNDO. /* ROWID ⥪�饩 ����� */
DEF VAR mGroupList  AS CHAR   NO-UNDO. /* ���᮪ ��㯯 ४����⮢ �� �⮬ ����� */
DEF VAR mi          AS INT64  NO-UNDO.
DEF VAR mTmpStr     AS CHAR   NO-UNDO.
DEF VAR mByGroup    AS LOG    NO-UNDO. /* �⮡ࠦ���� �� ��㯯��/��� ��㯯 */
DEF VAR vIsCode     AS LOG    NO-UNDO.  
DEF VAR vTable      AS CHAR   NO-UNDO.
DEF VAR vHBL        AS HANDLE NO-UNDO. /* ��� �� ���� ��� �����஢�� ।���஢���� */
DEF VAR vDBufferMutex AS HANDLE NO-UNDO.
/* ���� */
DEF BUFFER bclass  FOR class.
DEF BUFFER ccclass FOR class.

DEFINE TEMP-TABLE ttProc NO-UNDO
   FIELD fCode     AS CHARACTER
   FIELD fNameProc AS CHARACTER
   FIELD fHProc    AS HANDLE
   .
/* �६����� ⠡��� ��� ��࠭���� � ����⠭������� ��������� ���祭�� ���४����⮢ */
DEF TEMP-TABLE xattr-value-copy NO-UNDO LIKE xattr-value.

/* ����� �� ���祭�� � ⥬���஢����� �� */
FUNCTION CheckMultiTempXAttr RETURN LOG (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR
):
   DEF VAR vIsMulti   AS LOG INIT FALSE  NO-UNDO. 

   DEF BUFFER tmpsigns FOR tmpsigns.

   FOR EACH tmpsigns WHERE tmpsigns.file-name  EQ in-FileName
                           AND tmpsigns.code       EQ in-Code
                           AND tmpsigns.surrogate  EQ in-Surr
   NO-LOCK: 
      IF  vIsMulti THEN
         RETURN vIsMulti.
      vIsMulti = TRUE.
   END.

   RETURN FALSE.
END.

FUNCTION GetTempXAttrDate RETURN DATETIME (
   INPUT in-FileName AS CHAR,
   INPUT in-Surr     AS CHAR,
   INPUT in-Code     AS CHAR,
   INPUT in-Date     AS DATE
):
   DEF VAR vDate     AS DATETIME   NO-UNDO.
   DEF VAR vValue    AS CHAR       NO-UNDO.
   DEF VAR vResult   AS DATETIME   NO-UNDO. /* ��� ��. */

   DEF BUFFER tmpsigns FOR tmpsigns.

   FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ in-FileName
                           AND tmpsigns.code       EQ in-Code
                           AND tmpsigns.surrogate  EQ in-Surr
                           AND tmpsigns.since      LE in-Date
      NO-LOCK NO-ERROR.
      IF AVAIL tmpsigns THEN 
      DO:
         vValue = IF tmpsigns.code-value NE "" THEN tmpsigns.code-value ELSE tmpsigns.xattr-value.
         vDate = tmpsigns.since.
      END.

      vResult  =  IF     AVAIL tmpsigns
                     AND {assigned vValue}
                     THEN vDate
                     ELSE ?.
   
   RETURN vResult.
END.


/* 8-�筠� �ॡ����� */
DEFINE QUERY q1 FOR xattr-value.
DEFINE BROWSE b1 QUERY q1
   DISPLAY

&IF DEFINED( MANUAL-REMOTE ) &THEN
   /* � QBIS �� ���� �㦭� ��� ��㯯�஢�� */
   xattr-value.xattr-group FORMAT "x(100)" WIDTH 1
   /* � QBIS �� ���� �㦭� ��� ���᪨ ⥬���஢����� ���. ४����⮢ */
   xattr-value.name-dcolor WIDTH 1
&ENDIF   
   xattr-value.displ-name /* @ vDN */

   /* � QBIS �� ���� ��� : */   
   &IF DEFINED( MANUAL-REMOTE ) = 0 &THEN   
      string( xattr-value.temp-mul , "*/:") @ vsep WIDTH 1 
   &ELSE
      string( xattr-value.temp-mul , "*/ ") @ vsep WIDTH 1 
   &ENDIF 
   xattr-value.code-value
      FORMAT "x(20000)"
&IF DEFINED( MANUAL-REMOTE ) &THEN   
      WIDTH 27
&ELSE
      WIDTH 31
&ENDIF  
   ENABLE xattr-value.code-value
   WITH {&browse-height} DOWN no-box /*    separators */
        NO-ROW-MARKERS NO-LABELS.
&IF DEFINED( MANUAL-REMOTE ) &THEN
   RUN BloddSetFormHelpLabel("F6 ������^find").
&ENDIF 
DEFINE BUTTON bAllSome  LABEL "^F3-��/���".
DEFINE BUTTON bF3  LABEL "F3-���".
DEFINE BUTTON bByGroup  LABEL "^F4-�� ��㯯��".
DEFINE BUTTON bflt      LABEL {&mainmess}.
DEFINE BUTTON bedd      LABEL "".
IF screen-lines - level < {&browse-height} + 10 THEN
   level = screen-lines - {&browse-height} - 10.

DEFINE FRAME f-xattr
   mGroupList
      VIEW-AS COMBO-BOX SIZE 59 BY 1
      AT 1
      FORMAT "x(60)"
      LABEL "��㯯� ४����⮢"
   "------------------------------------------------------------------------------"
   b1
   "------------------------------------------------------------------------------"
   xattr-value.since
      AT 1
      LABEL "��� �����"  
   code.name
      VIEW-AS FILL-IN SIZE 32 BY 1 
      AT 34
      LABEL "����.��ࠬ"
   SKIP
   xattr-value.class
      FORMAT "x(20)"
      LABEL "����� ४�"
   code.val
      VIEW-AS FILL-IN SIZE 32 BY 1 
      AT 34
      LABEL "����.��ࠬ" 
   SKIP
   vDN
      VIEW-AS FILL-IN SIZE 60 BY 1
      AT 3
      FORMAT "x(80)"
      LABEL "��� ४�"
   xattr-value.description
      VIEW-AS EDITOR INNER-LINES 3 INNER-CHARS 65
      AT 3
      LABEL "���ᠭ��"
   bF3
   bAllSome
   bByGroup
   bflt
   bedd
WITH ROW 2 CENTERED OVERLAY SIDE-LABELS TITLE DCOLOR 9 "".
/* **************** initial *************** */

RUN SetTitle.           /* ��⠭���� ��������� ���. */

num-row = 0.

CASE in-create:
                        /* ����� ᮧ�����.
                        ** �⮡ࠦ����� �� �� ��ꥪ�
                        ** � ��, ����騥 ��砫쭮� ���祭�� � ����奬�. */   
   WHEN ?      THEN edit-mode = YES.

                        /* ����� ��ᬮ��.
                        ** �⮡ࠦ����� �� �� ��ꥪ�. */
   WHEN "NO"   THEN edit-mode = NO.

                        /* ����� ।���஢����.
                        ** �⮡ࠦ����� �� �� ��ꥪ�
                        ** � ������������ ��, ����騥 ��砫쭮� ���祭�� � ����奬�. */
   WHEN "YES"  THEN edit-mode = YES.
   OTHERWISE DO:        /* ����� ०�� �।�����祭 ��� �뤠� ᮮ�饭�� �
                        ** ࠡ�� � ०��� ��ᬮ��. */
      RUN Fill-SysMes ("", "", "1", in-create).

      ASSIGN            /* ����뢠�� ०��, ��� ����� ०��� ।���஢����. */
         edit-mode   = ?
                        /* ����ࠥ� ⮫쪮 �������騥 ���祭�� ��. */
         in-create   = "NO"
      .
   END.
END CASE.

IF edit-mode EQ YES THEN
   edit-mode = GetPermission(in-class-code,in-surrogate,"w").

ASSIGN 
   mStartMode     = edit-mode    /* ���������� ०�� � ����� ���뢠���� ���. */
   vdcolor        = xattr-value.code-value:column-dcolor in browse b1
   bflt:label     = {&mainmess}
   bflt:VISIBLE   = NO
                     /* �⥭�� ��, ��।����饣� 梥� �⮡ࠦ���� ������������ ⥬���஢����� �� */
   mTempColor     = FGetSetting("Temporal","TempXAttrColor","")
   vTempControl   = FGetSetting("Temporal","TemXAttrContr","") EQ "��".
.


IF NOT edit-mode
THEN bedd:LABEL = {&BTNF9}.
ELSE DO:
   bedd:VISIBLE = FALSE.
   IF edit-mode
      THEN xattr-value.code-value:column-dcolor in browse b1 = 1.
   bedd:LABEL = {&BTNESC}.
END.

FIND FIRST class WHERE
   class.class EQ in-class-code
NO-LOCK NO-ERROR.
IF NOT AVAIL class
THEN DO:

   RUN Fill-SysMes ("", "", "0",
                    "�� ������ ����� ��뫪� �� ���������騩 ����� � ����奬�." +
                    "~n��� ����� '"+ (IF in-class-code EQ ?
                                       THEN "?"
                                       ELSE in-class-code) +
                    "'.~n������� � ������������ ��⥬�.").

   RUN End-SysMes.         /* �����襭�� ����� ��⮪���஢����. */
   {intrface.del}          /* ���㧪� �����㬥����. */
   RETURN.
END.

/* ���⠥� ����� �७� ��� �஢�ન �ࠢ ।���஢���� � ����� */
RUN InitStatusCheckData.

/* ����ࠥ� ᯨ᮪ ��㯯 ४����⮢ ��� combo-box'� */
mTmpStr = GetXAttrGroupList(in-class-code).
mGroupList = "��,*,��� ��㯯,-".
DO mi = 1 TO NUM-ENTRIES(mTmpStr):
            /* ������塞 ⮫쪮 � ��㯯�, �� ����� � ���짮��⥫� ���� �ࠢ� �⥭�� */
   IF GetXAttrGroupPermission(in-class-code,ENTRY(mi,mTmpStr),in-surrogate,"r") THEN
      mGroupList = mGroupList + "," + GetXAttrEx(in-class-code,ENTRY(mi,mTmpStr),"name") + "," + ENTRY(mi,mTmpStr).
END.
mGroupList:LIST-ITEM-PAIRS = mGroupList.
mGroupList = "*".

ASSIGN
   cn       = class.NAME
   cclass   = in-class-code
.
RUN ffparent IN h_xclass (input-output cclass,buffer class).

/* ������� ����஫� �஢�ન 㤮�⮢�७�� ��� 䨧��᪨� ��� */

IF cclass EQ "op" THEN
   RUN SetSysConf IN h_base ("PROCESS_OP-EDIT","��").

IF cclass <> ? THEN
   FIND FIRST ccclass WHERE ccclass.class = cclass NO-LOCK NO-ERROR.

IF GetSysConf("param-list") NE ? THEN
   vall = YES.
/* ��ନ஢���� ������� ᯨ᪠ ४����⮢. */
RUN CreateXattrValue IN h_xclass (
   OUTPUT table xattr-value,
   vAll,
   in-create,
   in-class-code,
   cclass,
   in-surrogate,
   OUTPUT num-row,
   OUTPUT any-entered).

/******************************************************************************/
&IF DEFINED(SESSION-REMOTE) &THEN
ON CHOOSE OF bAllSome DO:
   RUN SetFltFieldList("IsEndDte",string(NOT LOGICAL(GetFltVal ("IsEndDte")))).
   APPLY "choose" TO bflt IN FRAME f-xattr.
END.
   
bAllSome:SENSITIVE = TRUE.
   /* ������ <^F4> ����㯭� ⮫쪮, ����� � �����-���� "��㯯� ४����⮢" ��࠭� ���祭�� "��" */
bByGroup:SENSITIVE = mGroupList:SCREEN-VALUE EQ "*".
&ENDIF

/* ����⨥ �� ����⨨ ������ <^F4-�� ��㯯��>/<^F4-��� ��㯯> */
ON CHOOSE OF bByGroup IN FRAME f-xattr
DO:
   &IF DEFINED(SESSION-REMOTE) &THEN
   RUN UpdateBrowser(b1:HANDLE). /* blodd.p */
   &ENDIF
         /* �᫨ � �����-���� "��㯯� ४����⮢" ��࠭� ���祭�� �⫨筮� �� ���祭�� "��" */
   IF mGroupList:SCREEN-VALUE NE "*" THEN RETURN NO-APPLY.
   mByGroup = NOT mByGroup.
   bByGroup:LABEL = IF mByGroup THEN "^F4-��� ��㯯" ELSE "^F4-�� ��㯯��".
   RUN CreateXattrValuePrivate (
      OUTPUT table xattr-value,
      vAll,
      edit-mode,
      mByGroup,
      in-class-code,
      cclass,
      in-surrogate,
      OUTPUT num-row,
      OUTPUT any-entered).
   CLOSE query q1.
   IF mByGroup
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
   APPLY "ENTRY" TO b1.
END.

ON CHOOSE OF bflt DO:

   &IF DEFINED(SESSION-REMOTE) &THEN
   RUN UpdateBrowser(b1:HANDLE).
   &ENDIF
                        /* ����᪠�� �⮡ࠦ���� 䨫���. */
   RUN flt-file.p (LASTKEY).  
                        /* ��⠭���� ��������� ���. */
   RUN SetTitle.

   vall =   IF GetFltVal ("IsEndDte") EQ "YES"
               THEN YES
               ELSE NO.

   RUN CreateXattrValuePrivate (
      OUTPUT table xattr-value,
      vAll,
      edit-mode,
      /* ०�� "�� ��㯯��" �ਬ��塞, ⮫쪮 �᫨ ��࠭� ���祭�� "��" � �����-���� "��㯯� ४����⮢" */
      IF mGroupList EQ "*" THEN mByGroup ELSE NO,
      in-class-code,
      cclass,
      in-surrogate,
      OUTPUT num-row,
      OUTPUT any-entered).

   CLOSE query q1.
            /* �᫨ ����祭 ०�� "�� ��㯯��", */
   IF     mByGroup
      AND mGroupList EQ "*"
               /* �ਬ��塞 ���஢�� �� ��㯯�, ��⥬ �� ������������, */
   THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
               /* ���� ⮫쪮 �� ������������ */
   ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
   APPLY "ENTRY" TO b1.
END.

ON F5 OF xattr-value.code-value IN BROWSE b1
DO:
   DEFINE VARIABLE vEditOk AS LOGICAL NO-UNDO.

   RUN CheckForEdit (BUFFER xattr-value, OUTPUT vEditOk).
   IF NOT vEditOk THEN RETURN NO-APPLY.

                        /* � ०��� ��ᬮ�� ����頥� ।���஢����. */
   IF NOT edit-mode
      THEN SELF:READ-ONLY = YES.
                        /* ��ᬮ�� / ।���஢���� � �⤥�쭮� ����. */

   RUN extedit (SELF, THIS-PROCEDURE).
                        /* ����⠭�������� ��ਡ��. */
   SELF:READ-ONLY = NO.
   ASSIGN
      xattr-value.code-value  = SELF:INPUT-VALUE
      any-entered             = yes
   .
   /* ��⠭���� ���祭��.
   ** ����� ��⠭�������� ���祭�� � EXTEDIT.P). */
   RUN UsSetFldVal (SELF, SELF:INPUT-VALUE, 1).

                        /* �᫨ �ந����� ����䨪��� ���祭��,
                        ** � ��⠭�������� �ਧ���. */
   IF GetTegDAta (SELF:PRIVATE-DATA, "MODIFIED") EQ "YES"
      THEN any-entered = YES.
END.

ON F5 OF xattr-value.description IN FRAME f-xattr
DO:
   RUN extedit.p (SELF).
   RETURN NO-APPLY.
END.


ON "F6" , "CTRL-F6", "Ctrl-F3" OF FRAME f-xattr ANYWHERE
   APPLY "CHOOSE" TO bflt IN FRAME f-xattr.

ON CHOOSE OF bedd DO:
                        /* ����頥� ।���஢���� � ०��� "⮫쪮 �⥭��" ����� "�����������������" */
   IF     work-module EQ "admin"
      AND NOT IsUserAdm (USERID ('bisquit'))
   THEN RETURN NO-APPLY.

   IF GetPermission(in-class-code,in-surrogate,"w") NE YES THEN
      RETURN NO-APPLY.
                        /* �஢�ઠ �ࠢ �� ��㯯� ४����⮢ � QBIS �१ ANY-PRINTABLE �� ��ࠠ�뢠�� ���४⭮!, ���⮬� ������� �஢��� ����� */
   IF {assigned xattr-value.xattr-group}
      AND NOT GetXAttrGroupPermissionEx (xattr-value.class, GetXAttrEx (xattr-value.class, xattr-value.code, "xattr-group"), in-surrogate, "w")
   THEN RETURN NO-APPLY.

   RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* ����� ��ꥪ�                 */
                                   INPUT "CheckEdtSign",    /* id ��⮤�                     */
                                   INPUT "",                /* id ��� ४�����              */
                                   INPUT "",                /* ��� ��⮤�                    */
                                   INPUT ?,                 /* ��楤�� �� 㬮�砭��        */
                                   INPUT ""                 /* ���᮪ �室��� ��ࠬ��஢     */
   ).   
   IF RETURN-VALUE NE "" AND RETURN-VALUE NE "no-method" THEN 
   DO:
      RUN Fill-SysMes ("", "", "0", RETURN-VALUE). 
      ASSIGN
         edit-mode   = NO
         bedd:LABEL  = {&BTNF9}
      .
      xattr-value.code-value:COLUMN-DCOLOR IN BROWSE b1 = 0.
      RETURN NO-APPLY.
   END.

   IF NOT AVAIL xattr-value
      OR xattr-value.code EQ ?
      OR edit-mode        EQ ? THEN 
      RETURN.

   &IF DEFINED(SESSION-REMOTE) &THEN
   IF AVAIL xattr-value THEN
   DO:
      DEFINE VARIABLE vCode AS CHARACTER      NO-UNDO.
      vCode = xattr-value.code.

      RUN LockRecord (cclass,in-surrogate) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         RETURN NO-APPLY.

      edit-mode   = YES.
      RUN f-signs.p(INPUT BUFFER xattr-value:HANDLE,
                    INPUT xattr-value.code-value:HANDLE IN BROWSE b1,
                    THIS-PROCEDURE,
                    OUTPUT any-entered).

      IF any-entered THEN
      DO:
         /* ���뢠�� ���� � qbis ��� ����� ����� */
         xattr-value.since = ? .
         /* Commented by KSV: �⪫�砥� �஢��� ���祭�� ���� ���. ४����⮢,
         ** �.�. ������ ⮫쪮 ���� */
         vNoCheck = YES.
         RUN SaveXattrValues.
         vNoCheck = NO.
      END.
      
      RUN CreateXattrValuePrivate (
            OUTPUT TABLE xattr-value,
            vAll,
            edit-mode,
            /* ०�� "�� ��㯯��" �ਬ��塞, ⮫쪮 �᫨ ��࠭� ���祭�� "��" � �����-���� "��㯯� ४����⮢" */
            IF mGroupList EQ "*" THEN mByGroup ELSE NO,
            in-class-code,
            cclass,
            in-surrogate,
            OUTPUT num-row,
            OUTPUT any-entered).

      
      DEFINE VARIABLE vRowid AS ROWID      NO-UNDO.

      CLOSE QUERY q1.
               /* �᫨ ����祭 ०�� "�� ��㯯��", */
      IF     mByGroup
         AND mGroupList EQ "*"
                  /* �ਬ��塞 ���஢�� �� ��㯯�, ��⥬ �� ������������, */
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                  /* ���� ⮫쪮 �� ������������ */
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

      FIND FIRST xattr-value WHERE xattr-value.code = vCode NO-ERROR.
      REPOSITION q1 TO ROWID ROWID(xattr-value) NO-ERROR.
      
      APPLY "ENTRY" TO b1.
      edit-mode   = NO.
   END.
   RUN UpdateBrowser(b1:HANDLE).
   RUN UnLockRecord (cclass, in-surrogate).
   RETURN .
   &ENDIF

   IF NOT mStartMode THEN 
   DO:
      IF NOT edit-mode THEN 
      DO: /* ।����㥬 */
         IF cclass = "" THEN 
         DO:
            RUN Fill-SysMes ("", "", "0", "��� ����� ��த�⥫�.").
            RETURN NO-APPLY.
         END.
         IF  class.progress-table AND (NOT tst-rght-TBL(cclass, 'w') OR
            (cclass EQ "_user" AND NOT tst-rght-TBL(cclass, 'c'))) THEN 
            RETURN NO-APPLY.

         RUN LockRecord (cclass,in-surrogate) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            RETURN NO-APPLY.

         ASSIGN
            edit-mode   = YES
            mXattrCode  = xattr-value.code
         .

         RUN CreateXattrValuePrivate (
               OUTPUT TABLE xattr-value,
               vAll,
               edit-mode,
               /* ०�� "�� ��㯯��" �ਬ��塞, ⮫쪮 �᫨ ��࠭� ���祭�� "��" � �����-���� "��㯯� ४����⮢" */
               IF mGroupList EQ "*" THEN mByGroup ELSE NO,
               in-class-code,
               cclass,
               in-surrogate,
               OUTPUT num-row,
               OUTPUT any-entered).
         
         ASSIGN
            bedd:LABEL  = {&BTNESC}
            xattr-value.code-value:COLUMN-DCOLOR IN BROWSE b1 = 1
         .
                           /* ��८��뢠�� �����. */
         CLOSE QUERY q1.
                  /* �᫨ ����祭 ०�� "�� ��㯯��", */
         IF     mByGroup
            AND mGroupList EQ "*"
                     /* �ਬ��塞 ���஢�� �� ��㯯�, ��⥬ �� ������������, */
         THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                     /* ���� ⮫쪮 �� ������������ */
         ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
                           /* ����樮���㥬�� �� ������. */
         FIND FIRST xattr-value WHERE
            xattr-value.CODE EQ mXattrCode
         NO-ERROR.
         IF AVAIL xattr-value
            THEN REPOSITION q1 TO RECID RECID (xattr-value).
      END.
      ELSE 
      DO: /* �஢��塞 �⪠� */
         ASSIGN INPUT BROWSE b1 xattr-value.code-value.
         IF any-entered THEN 
         DO:
            pick-value = "NO".
            RUN Fill-SysMes ("", "", "4",
                             "���࠭��� �������� ���祭�� ४����⮢?").
            IF pick-value EQ "YES" THEN 
            DO:
               RUN SaveXattrValues.
               IF RETURN-VALUE EQ "NO-APPLY" THEN 
               DO:
                  APPLY "entry" TO b1.
                  RETURN NO-APPLY.
               END.
            END.
            ELSE
               any-entered = NO.
            RUN RestoreInitXattrValues.

            /* ��ନ஢���� ������� ᯨ᪠ ४����⮢. */
            RUN CreateXattrValuePrivate (
               OUTPUT table xattr-value,
               YES,                       /* ������� �� �����. */
               in-create,
               /* ०�� "�� ��㯯��" �ਬ��塞, ⮫쪮 �᫨ ��࠭� ���祭�� "��" � �����-���� "��㯯� ४����⮢" */
               IF mGroupList EQ "*" THEN mByGroup ELSE NO,
               in-class-code,
               cclass,
               in-surrogate,
               OUTPUT num-row,
               OUTPUT any-entered).

            CLOSE QUERY q1.
                     /* �᫨ ����祭 ०�� "�� ��㯯��", */
            IF     mByGroup
               AND mGroupList EQ "*"
                        /* �ਬ��塞 ���஢�� �� ��㯯�, ��⥬ �� ������������, */
            THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                        /* ���� ⮫쪮 �� ������������ */
            ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
            APPLY "ENTRY" TO b1.
            any-entered = NO.
         END.
         ASSIGN
            edit-mode   = NO
            bedd:LABEL  = {&BTNF9}
         .
         xattr-value.code-value:COLUMN-DCOLOR IN BROWSE b1 = 0.

         RUN UnLockRecord (cclass, in-surrogate).
      END.
   END.
END.
/*----------------------------------------------------------------------------*/
ON F9 OF FRAME f-xattr ANYWHERE APPLY "CHOOSE" TO bedd IN FRAME f-xattr.

/* ���樠������ ४����� �� �ॡ������ ���짮��⥫�. */
ON F3 OF xattr-value.code-value IN BROWSE b1
DO:
   IF edit-mode NE YES THEN 
   DO: 
      APPLY "CHOOSE" TO bF3 IN FRAME f-xattr.
      RETURN NO-APPLY.
   END.
   ASSIGN
      xattr-value.code-value = GetXattrInit (xattr-value.class, xattr-value.code)
      xattr-value.code-value:SCREEN-VALUE in browse b1 = xattr-value.code-value
      any-entered = yes
   .
   RETURN NO-APPLY.
END.

ON F3 OF FRAME f-xattr ANYWHERE DO:
   APPLY "CHOOSE" TO bF3 IN FRAME f-xattr.
   RETURN NO-APPLY.
END.

ON CHOOSE OF bF3 DO:

   &IF DEFINED(SESSION-REMOTE) &THEN
      RUN UpdateBrowser(b1:HANDLE).
   &ENDIF

   IF AVAIL xattr-value THEN
      vCodeFnd = xattr-value.CODE. 
   vIsCode = NOT vIsCode.

   IF vIsCode THEN 
      ASSIGN
         vDN:LABEL = "��� ४�"
         bF3:LABEL = "F3-���"
      .
   ELSE
      ASSIGN
         vDN:LABEL = "��� ४�"
         bF3:LABEL = "F3-���"
      .
   
   RUN CreateXattrValuePrivate (
         OUTPUT table xattr-value,
         vAll,
         edit-mode,
         mByGroup,
         in-class-code,
         cclass,
         in-surrogate,
         OUTPUT num-row,
         OUTPUT any-entered).
   CLOSE query q1.
   IF mByGroup
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

   FIND FIRST xattr-value WHERE xattr-value.CODE = vCodeFnd NO-LOCK NO-ERROR.

   vCurrROWID = ROWID(xattr-value).       /* ⥪��� ������ */

   REPOSITION q1 TO ROWID vCurrROWID NO-ERROR.  /* �����頥��� � �����, �� ��४��祭�� */
   
   APPLY "ENTRY" TO FRAME f-xattr.

   APPLY "ROW-ENTRY" TO BROWSE b1. /* IN FRAME f-xattr. */

END.


/* �⮡ࠦ���� ���祭�� ⥬��ࠫ��� ��. */
ON SHIFT-F3 OF xattr-value.code-value IN BROWSE b1
DO:
   FIND FIRST xattr where
            xattr.xattr-code = xattr-value.code
      AND   xattr.class-code = xattr-value.class
   NO-LOCK NO-ERROR.
   IF       AVAIL Xattr
      AND   xattr.temporal
   THEN DO:
      RUN browseld.p (
         "tmpsigns",
         "file-name"       + "~001" +
         "code"            + "~001" +
         "surrogate",
         cclass            + "~001" +
         xattr-value.CODE  + "~001" +
         in-surrogate,
         "file-name~001code~001surrogate",
         4
      ) NO-ERROR.
      
      xattr-value.temp-mul = CheckMultiTempXAttr(cclass,in-surrogate,xattr-value.code). 
      APPLY "ROW-ENTRY" TO BROWSE b1. 

      /* �뭥� �� ����� �஢�ન edit-mode ��⮬� �� ��㧥� ���祭�� �� �������� ��������� � 㤠���� � ०��� ��ᬮ�� */
      xattr-value.code-value = GetTempXAttrValueEx2(
                                    cclass,in-surrogate,xattr-value.code,
                                    IF gend-date EQ ?
                                    THEN gend-hdate
                                    ELSE gend-date,
                                    "",YES).
         
      xattr-value.since      = GetTempXAttrDate(cclass,in-surrogate,xattr-value.code,
                                                      IF gend-date EQ ? THEN gend-hdate ELSE gend-date).

      APPLY "ROW-ENTRY" TO BROWSE b1.

      IF       edit-mode
      THEN DO:
                           /* ��⠭���� ���祭��.
                           ** ����� ��⠭�������� ���祭�� � EXTEDIT.P). */
         RUN UsSetFldVal (SELF, xattr-value.code-value, 1).
      END.
      /* Commented by KSV: ������塞 ��㧥� � QBIS */
      &IF DEFINED(SESSION-REMOTE) &THEN
      RUN UpdateBrowser(b1:HANDLE IN FRAME f-xattr).
      &ENDIF /* DEFINED(SESSION-REMOTE) */
   END.
APPLY "ENTRY" TO BROWSE b1. 
   RETURN NO-APPLY.
END.

&IF DEFINED(SESSION-REMOTE) &THEN
ON F1 OF b1 ANYWHERE
DO:
   APPLY "F1" TO xattr-value.code-value in BROWSE b1.
   RETURN NO-APPLY.
END.

ON SHIFT-F3 OF b1 ANYWHERE
DO:
   APPLY "SHIFT-F3" TO xattr-value.code-value in BROWSE b1.
   RETURN NO-APPLY.
END.
&ENDIF

PROCEDURE CheckForEdit:
   DEFINE PARAMETER BUFFER xattr-value FOR xattr-value.
   DEFINE OUTPUT PARAMETER oOk AS LOGICAL INIT NO.
   
   IF  xattr-value.code  <> ?
   AND xattr-value.CLASS <> "" THEN DO:
      FIND FIRST xattr WHERE xattr.xattr-code = xattr-value.code
                         AND xattr.class-code = xattr-value.class
      NO-LOCK NO-ERROR.
      IF AVAIL xattr THEN DO:
         IF edit-mode THEN DO:
            IF xattr-value.constant
            THEN RUN Fill-SysMes ("", "", "0", "��� ४����� ����ﭭ�.~n��� ����� ��������.").
            ELSE IF     {assigned xattr-value.xattr-group}
                    AND NOT GetXAttrGroupPermissionEx (xattr-value.class, GetXAttrEx (xattr-value.class, xattr-value.code, "xattr-group"), in-surrogate, "w")
            THEN .
            ELSE oOk = YES.

               /* �஢�ઠ �ࠢ �� ������ */
            IF mStatusEditFld NE "" THEN
               IF NOT CAN-DO(mStatusEditFld, xattr-value.code) THEN
               DO:
                   RUN Fill-SysMes IN h_tmess ("", "", "0", "�� �� ����� �ࠢ� �������� ४����� " + xattr-value.CODE + " � ����� " + vStatus).
                   oOk = NO.
               END.

            /* �맮� ᮡ�⢥���� ��楤��� �஢�ન */            
            RUN RunClassMethod IN h_xclass 
                                 (xattr-value.class,
                                  "ChkEdtXattr",
                                   xattr-value.code,
                                   "",?,
                                   xattr-value.code)    NO-ERROR.
            IF RETURN-VALUE NE "" AND RETURN-VALUE NE "no-method" THEN 
               oOk = NO.
         END.
         ELSE oOk = YES.
      END.
      ELSE DO:
         RUN Fill-SysMes ("", "", "0",
            "��� ⠪��� ४����� (" +
            (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
            (IF xattr-value.CODE  EQ ? THEN "?" ELSE xattr-value.CODE ) + ")."
         ).
      END.
   END.

   RETURN.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
ON F1 OF xattr-value.code-value in browse b1 DO:

   DEFINE VARIABLE vEditOk AS LOGICAL NO-UNDO.

   RUN CheckForEdit (BUFFER xattr-value, OUTPUT vEditOk).
   IF NOT vEditOk THEN RETURN NO-APPLY.

   pick-value = ?.

   IF edit-mode THEN DO:

      RUN SetSysConf in h_base ("XAttrF1IsOK","NO").

      IF xattr.data-type EQ "logical"
      THEN DO:
         any-entered = YES.
         RUN UsSetFldVal (
            SELF,
            STRING (NOT (UsGetFldVal (SELF) EQ SUBSTR (xattr.data-format, 1, INDEX (xattr.data-format,"/") - 1)),
            xattr.data-format),
            1
         ).
      END.
      ELSE IF xattr.data-type = "date" THEN RUN calend.

      ELSE IF xattr.data-type = "decimal" THEN RUN calc.

      ELSE DO TRANSACTION:
        /*  �롮� ���� � ������� ��� ��ᬮ�� (�� F1) */
        RUN LookupField IN h_xclass (BUFFER xattr, 
                                     UsGetFldVal (SELF), 
                                     YES,
                                     level).
      END.
      IF (   LASTKEY EQ 10
          OR (LASTKEY             EQ 13
              AND xattr.data-type EQ "decimal")
          OR GetSysConf("XAttrF1IsOK") = "YES")
      AND pick-value NE ?
      THEN DO:
         ASSIGN
            xattr-value.code-value:SCREEN-VALUE in browse b1 = pick-value
            any-entered             = yes
         .
                        /* ��⠭���� ���祭��.
                        ** ����� ��⠭�������� ���祭�� � EXTEDIT.P). */
         RUN UsSetFldVal (SELF, pick-value, 1).
      END.
   END. /* edit-mode */
   ELSE DO: /* look */

      /* �맮� ᮡ�⢥���� ��� ��ᬮ�� */
      {xattr-ed.nav &val    = xattr-value.code-value
                    &pXattr = Yes
                    &pSurr  = in-surrogate
      }

   END.
   RETURN NO-APPLY.
END.

/*------------------------------------------------------------------------------------------*/
ON ANY-PRINTABLE, CLEAR, DELETE-CHARACTER, CTRL-V, BACKSPACE,
   CTRL-SHIFT-Z OF xattr-value.code-value IN BROWSE b1
DO:
   DEFINE VARIABLE vEditOk AS LOGICAL NO-UNDO.

   RUN CheckForEdit (BUFFER xattr-value, OUTPUT vEditOk).
   IF NOT vEditOk THEN RETURN NO-APPLY.

   IF xattr-value.code = ? THEN RETURN NO-APPLY.
   IF edit-mode THEN DO:
      any-entered = YES.
      IF KEYLABEL(LASTKEY) = "ctrl-v" THEN DO:
         xattr-value.code-value:SCREEN-VALUE IN BROWSE b1 = xattr-value.code-value:SCREEN-VALUE + clip-buffer.
      END.
      IF LASTKEY EQ KEYCODE ("Ctrl-Shift-z") THEN
      DO:
         IF VALID-HANDLE(FOCUS) AND CAN-DO("EDITOR,FILL-IN",FOCUS:TYPE) THEN
            clip-buffer = FOCUS:SCREEN-VALUE.
         ELSE
            clip-buffer = FRAME-VALUE.
         RUN chk-terr.p (clip-buffer).
      END.
   END.
   ELSE RETURN NO-APPLY.
END.

ON ROW-ENTRY OF b1 IN FRAME f-xattr DO:
   IF NOT AVAIL xattr-value
      OR xattr-value.code EQ ?
      THEN RETURN NO-APPLY.

   vPrevVal =    xattr-value.code-value.
   IF xattr-value.CODE EQ "�����������" THEN
      RUN SetSysConf IN h_base ("MC_BEFORE_VAL",vPrevVal).
   IF xattr-value.CODE EQ "�㡫��㯯�" THEN
      RUN SetSysConf IN h_base ("GR_BEFORE_VAL",vPrevVal).

   DEF VAR dt LIKE xattr-value.since NO-UNDO.
   dt = IF gend-date EQ ? THEN gend-hdate ELSE gend-date.
   IF xattr-value.since GT dt THEN xattr-value.since = ?.

   DISPLAY
      xattr-value.description
      xattr-value.class 

      xattr-value.NAME WHEN     vIsCode @ vDN 
      xattr-value.CODE WHEN NOT vIsCode @ vDN 

            xattr-value.since
      ""
         @ code.val
      ""
         @ code.name
   WITH FRAME f-xattr.

   DISPLAY
      xattr-value.displ-name /* @ vDN */

      &IF DEFINED( MANUAL-REMOTE ) = 0 &THEN   
         string( xattr-value.temp-mul , "*/:") @ vsep 
      &ELSE
         string( xattr-value.temp-mul , "*/ ") @ vsep 
      &ENDIF   

      xattr-value.code-value
   WITH BROWSE b1.
   
   APPLY "ENTRY" TO xattr-value.code-value IN BROWSE b1.

   FIND FIRST xattr WHERE
            xattr.xattr-code EQ xattr-value.code
      AND   xattr.class-code EQ xattr-value.class
   NO-LOCK NO-ERROR.

   IF AVAIL xattr THEN
   DO:
      /* ��������� �믮����� ��楤��� ��⮤� ����祭�� �易���� ���ଠ樨 */
      pick-value = ?.
      RUN RunClassMethod IN h_xclass 
                         (xattr-value.class,
                         "GetXValDesc",
                         xattr-value.code,
                         "",?,
                         CHR(1) + REPLACE(STRING(xattr-value.code-value),",",";"))
      NO-ERROR.
      IF pick-value NE ? THEN
         DISPLAY 
            ENTRY(1,pick-value,CHR(2)) @ code.name
            IF NUM-ENTRIES(pick-value,CHR(2)) GT 1 THEN ENTRY(2,pick-value,CHR(2)) ELSE "" @ code.val
         WITH FRAME f-xattr.
      IF     RETURN-VALUE        EQ "no-method"
         AND xattr.domain-code   NE ""
      THEN DO:
         /* �饬 ������ � tmp-code ��� ⮣�, �⮡� ��।�����
         ** ⥬���஢���� �� �����䨪��� ��� ����� */
         FIND LAST tmp-code WHERE tmp-code.class      EQ xattr.domain-code
                              AND tmp-code.code       EQ xattr-value.code-value
                              AND tmp-code.beg-date   LE gend-date
         NO-LOCK NO-ERROR.
         /* �᫨ �����䨪��� ⥬���஢����, ᮡ�ࠥ� ᮮ�. ���ண�� */
         IF AVAIL tmp-code
         THEN DISPLAY tmp-code.val  @ code.val
                      tmp-code.name @ code.name
         WITH FRAME f-xattr.
         /* ���� ��⠥��� ���� ���筮� ���祭�� � �����䨪��� */
         ELSE DO:
            FIND FIRST CODE WHERE
                     code.class EQ xattr.domain-code
               AND   code.code  EQ xattr-value.code-value
            NO-LOCK NO-ERROR.

            IF AVAIL code
               THEN DISPLAY code.val code.name with frame f-xattr.
         END.
      END.
   END.
END.
    
ON ROW-LEAVE OF b1 IN FRAME f-xattr DO:
   &IF DEFINED(SESSION-REMOTE) &THEN
   RETURN .
   &ENDIF

   vPrevVal =    xattr-value.code-value.
               /* �ய�᪠�� ��㯯� */
   IF xattr-value.CLASS EQ "" THEN RETURN. 
   IF KEYFUNC(LASTKEY) = "end-error" THEN RETURN /* NO-APPLY*/ .
   IF xattr-value.code = ? or edit-mode NE YES THEN RETURN.
   ASSIGN INPUT browse b1 xattr-value.code-value.
   RUN CheckXattrValue.
   IF RETURN-VALUE = "NO-APPLY" THEN RETURN NO-APPLY. /* undo, RETURN. */
   DISPLAY xattr-value.code-value with browse b1.

   IF vTempControl AND IsTemporal(cclass,xattr-value.CODE) THEN DO:

      IF xattr-value.code-value  NE GetXattrValue (cclass, in-surrogate, xattr-value.CODE) AND xattr-value.code-value NE vPrevVal THEN DO:

         {getdate.i
           &DispBeforeDate = "xattr-value.code"
           &DateLabel      = "��� ��砫�"
           &DateHelp       = "��� ��砫� ����⢨� ४����� (F1 - ���������)"}

         vTable = GetXclassFile (in-class-code).
         IF end-date NE ? AND 
            CAN-FIND (FIRST tmpsigns WHERE
                            tmpsigns.file-name EQ vTable
                        AND tmpsigns.CODE      EQ xattr-value.code
                        AND tmpsigns.surrogate EQ in-surrogate
                        AND tmpsigns.since > end-date) THEN DO:

            RUN Fill-SysMes ("", "", "4", "�����㦥�� ���祭�� � ����� ������� ��⮩. ��� ��ᬮ�� ������ Shift+F3. ���࠭��� ��������� ���祭��?").           
            vChoice = (pick-value = "YES").
            IF vChoice NE YES THEN DO:
               xattr-value.code-value:SCREEN-VALUE = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, ?).
               RETURN NO-APPLY.
            END.

         END.
         IF end-date NE ? THEN
            xattr-value.since = end-date.
      END.
   END.
   IF xattr-value.CODE EQ "�����������" THEN
      RUN SetSysConf IN h_base ("MC_AFTER_VAL",xattr-value.code-value).
   IF xattr-value.CODE EQ "�㡫��㯯�" THEN
      RUN SetSysConf IN h_base ("GR_AFTER_VAL",xattr-value.code-value).
   IF xattr-value.lastattr and KEYFUNC(LASTKEY) = "return" THEN DO:
      APPLY "GO" TO SELF.
      RETURN NO-APPLY.
   END.
END.

ON GO OF FRAME f-xattr DO:
   IF NOT AVAIL xattr-value
      OR xattr-value.code EQ ?
      THEN RETURN.

   IF LASTKEY = 10 THEN
      ASSIGN INPUT BROWSE b1 xattr-value.code-value.
   RUN SaveXattrValues.
   IF RETURN-VALUE = "NO-APPLY" then
   DO:
      APPLY "entry":U TO b1.
      RETURN NO-APPLY.
   END.
   RUN UnLockRecord (cclass, in-surrogate).
END.

ON END-ERROR, ENDKEY OF FRAME f-xattr  
&IF DEFINED(SESSION-REMOTE) &THEN 
   anywhere  
&ENDIF
   DO:
&IF DEFINED(SESSION-REMOTE) &THEN 
   RUN UnLockRecord (cclass, in-surrogate).
&ELSE   
   IF NOT AVAIL xattr-value
   OR xattr-value.code = ? THEN RETURN.

   IF mStartMode THEN RETURN.

   IF edit-mode THEN DO:
      APPLY "CHOOSE" TO bedd IN FRAME f-xattr.
      RETURN NO-APPLY.
   END.
&ENDIF
END.

/* ���� � ।���� �� */
ON F7 OF BROWSE b1 ANYWHERE
DO:
   /* �⮡ࠦ���� ��� ���᪠ */
   DO WITH FRAME fnd OVERLAY 1 COL CENTERED ROW 8
   COLOR messages SIDE-LABELS TITLE "[ ����� ]"
   ON ENDKEY UNDO, LEAVE:
      PAUSE 0.
      UPDATE
         vCodeFnd  FORMAT "X(100)" LABEL "���"
                   VIEW-AS FILL-IN SIZE 26 BY 1
         vNameFnd  FORMAT "X(100)" LABEL "������������"
                   VIEW-AS FILL-IN SIZE 26 BY 1.
   END.
   HIDE FRAME fnd.

   IF    LAST-EVENT:FUNCTION EQ "END-ERROR"  /* Esc */
      OR (vCodeFnd            EQ ""          /* �� ���� ���� �� ��������� */
      AND vNameFnd            EQ "")
   THEN RETURN NO-APPLY.
   
   vCurrROWID = ROWID(xattr-value).       /* ⥪��� ������ */
   ASSIGN INPUT BROWSE b1 xattr-value.code-value .
   RUN SaveTmpXattrValues.
   QUERY q1:GET-FIRST().
   REPEAT:                                /* ��ॡ�� ����ᥩ ����� */
      IF NOT AVAILABLE xattr-value THEN LEAVE.
      IF  xattr-value.code MATCHES (IF vCodeFnd EQ "" THEN "*" ELSE vCodeFnd)
      AND xattr-value.name MATCHES (IF vNameFnd EQ "" THEN "*" ELSE vNameFnd)
      THEN LEAVE.
      QUERY q1:GET-NEXT().
   END.
   
   IF AVAILABLE xattr-value
   THEN DO:                               /* ������ ������� */
      /* ����樮���㥬 ����� �� ��������� ������ */
      REPOSITION q1 TO ROWID ROWID(xattr-value).
      /* �������� ��� */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.
   ELSE DO:                               /* ������ �� ������� - ᮮ�饭��, ��室 */
      BELL.
      MESSAGE "������ �� �������" VIEW-AS ALERT-BOX.
      REPOSITION q1 TO ROWID vCurrROWID.  /* �����頥��� � �����, �� ���᪠ */
      RETURN NO-APPLY.
   END.
   RUN RestoreTmpXattrValues.
   RETURN NO-APPLY.
END.

/* ���� ᫥������ ������, 㤮���⢮������ ������ ���᪠ */
ON SHIFT-F7 OF  BROWSE b1  ANYWHERE
DO:
   vCurrROWID = ROWID(xattr-value).       /* ⥪��� ������ */
   REPEAT:                                /* ��ॡ�� ����ᥩ ����� */
      QUERY q1:GET-NEXT().
      IF NOT AVAILABLE xattr-value THEN LEAVE.
      IF     xattr-value.code MATCHES (IF vCodeFnd EQ "" THEN "*" ELSE vCodeFnd)
         AND xattr-value.name MATCHES (IF vNameFnd EQ "" THEN "*" ELSE vNameFnd)
      THEN LEAVE.
   END.
   IF AVAILABLE xattr-value
   THEN DO:                               /* ������ ������� */
      /* ����樮���㥬 ����� �� ��������� ������ */
      REPOSITION q1 TO ROWID ROWID(xattr-value).
      /* �������� ��� */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.
   ELSE DO:                               /* ������ �� ������� - ᮮ�饭��, ��室 */
      BELL.
      MESSAGE "������ �� �������" VIEW-AS ALERT-BOX.
      REPOSITION q1 TO ROWID vCurrROWID.  /* �����頥��� � �����, �� ������ Shift-F7 */
      RETURN NO-APPLY.
   END.
   RETURN NO-APPLY.
END.

/* ���� �।����� ������, 㤮���⢮������ ������ ���᪠ */
ON CTRL-F7 OF  BROWSE b1 ANYWHERE
DO:
   vCurrROWID = ROWID(xattr-value).       /* ⥪��� ������ */
   REPEAT:                                /* ��ॡ�� ����ᥩ ����� */
      QUERY q1:GET-PREV().
      IF NOT AVAILABLE xattr-value THEN LEAVE.
      IF     xattr-value.code MATCHES (IF vCodeFnd EQ "" THEN "*" ELSE vCodeFnd)
         AND xattr-value.name MATCHES (IF vNameFnd EQ "" THEN "*" ELSE vNameFnd)
      THEN LEAVE.
   END.
   IF AVAILABLE xattr-value
   THEN DO:                               /* ������ ������� */
      /* ����樮���㥬 ����� �� ��������� ������ */
      REPOSITION q1 TO ROWID ROWID(xattr-value).
      /* �������� ��� */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.
   ELSE DO:                               /* ������ �� ������� - ᮮ�饭��, ��室 */
      BELL.
      MESSAGE "������ �� �������" VIEW-AS ALERT-BOX.
      REPOSITION q1 TO ROWID vCurrROWID.  /* �����頥��� � �����, �� ������ Ctrl-F7 */
      RETURN NO-APPLY.
   END.
   RETURN NO-APPLY.
END.

ON CTRL-G GO.
ON CTRL-G OF FRAME f-xattr ANYWHERE DO:
   DEF VAR rid AS RECID NO-UNDO.
   IF AVAIL xattr-value THEN rid = RECID(xattr-value).
   ELSE rid = ?.
   FORM HEADER
      "             ���. ��������� " + in-title + " �� " + STRING(vSignsDt) FORMAT "x(100)" SKIP
      "             �����: " + in-class-code + " (" + cn + ") (""" + ENTRY(3, user-config-info) + """)"
      FORMAT "x(100)"
   WITH FRAME a NO-BOX WIDTH 200.
   {setdest.i &nodef="/*"}
   GET FIRST q1.
   DO WHILE AVAIL xattr-value WITH FRAME a:
      DISP
         xattr-value.code LABEL "��� ���������" FORMAT "x(20)"
         xattr-value.since
         string( xattr-value.temp-mul , "*/ ") @ vsep   LABEL "�" FORMAT "x(1)"
         xattr-value.name  LABEL "������������"       FORMAT "x(40)"
         xattr-value.class
         xattr-value.code-value  FORMAT "x(60)"
      WITH FRAME a DOWN.
      DOWN.
      GET NEXT q1.
   END.
   &SCOP def_telefon YES
   {signatur.i &user-only=1}
   {preview.i}.
   FIND xattr-value WHERE RECID(xattr-value) = rid NO-LOCK NO-ERROR.
   RETURN NO-APPLY.
END.

ON ROW-DISPLAY OF b1 IN FRAME f-xattr
DO:
   DEFINE VARIABLE vTmpFld AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vColor  AS INT64    NO-UNDO.

            /* �᫨ 梥� ⥬���஢����� �� ��।����, ���訢��� �� � ��� 梥� */
   IF {assigned mTempColor}
   THEN DO:
      COLOR DISPLAY VALUE(mTempColor) vTmpFld FORMAT "x(1)" WITH FRAME tmp-frm.
      vColor = vTmpFld:DCOLOR. 
      HIDE FRAME tmp-frm.
               /* �᫨ �� - ⥬���஢����, ���訢��� */
      IF IsTemporal(GetXclassProgress(xattr-value.class), xattr-value.code)
      THEN DO:
         DEF VAR dt LIKE xattr-value.since NO-UNDO.
         dt = IF gend-date EQ ? THEN gend-hdate ELSE gend-date.
         IF xattr-value.since GT dt THEN xattr-value.since = ?.

         xattr-value.displ-name:DCOLOR  IN BROWSE b1 = vColor.
         &IF DEFINED( MANUAL-REMOTE ) &THEN
            xattr-value.name-dcolor = vColor.
         &ENDIF
      END.
   END.

            /* ����訢���� ����� ��㯯 */
   IF     xattr-value.CLASS EQ ""
      AND xattr-value.xattr-group NE ""
   THEN DO:
      COLOR DISPLAY "bright-white" vTmpFld FORMAT "x(1)" WITH FRAME tmp-frm.
      vColor = vTmpFld:DCOLOR. 
      HIDE FRAME tmp-frm.
      xattr-value.displ-name:DCOLOR IN BROWSE b1 = vColor.
   END.

   RETURN.
END.

ON VALUE-CHANGED OF mGroupList IN FRAME f-xattr
DO:
   ASSIGN mGroupList.
   /* � ⠡��� xattr-value ४������ ��� ��㯯 ����� ४�����
   ** xattr-group ࠢ�� ����।�������� ���祭��. ��� ᤥ���� ���
   ** ���஢�� - �⮡� ४������ ��� ��㯯 �������� � �����
   ** ᯨ᪠ � ०��� "�� ��㯯��". ���⮬� �����⢫���� �������
   ** ���祭�� �ਧ���� ��㯯�, �� ���ன �ந�������� 䨫�����,
   ** � ���⮣� (�.�. ����� ��࠭� "��� ��㯯") �� ����।������� */
   IF mGroupList EQ "-" THEN mGroupList = ?.

   RUN CreateXattrValuePrivate (
      OUTPUT table xattr-value,
      vAll,
      edit-mode,
      /* ०�� "�� ��㯯��" �ਬ��塞, ⮫쪮 �᫨ ��࠭� ���祭�� "��" � �����-���� "��㯯� ४����⮢" */
      IF mGroupList EQ "*" THEN mByGroup ELSE NO,
      in-class-code,
      cclass,
      in-surrogate,
      OUTPUT num-row,
      OUTPUT any-entered).
   CLOSE query q1.

   CLOSE QUERY q1.
            /* �᫨ ����祭 ०�� "�� ��㯯��", */
   IF     mByGroup
      AND mGroupList EQ "*"
               /* �ਬ��塞 ���஢�� �� ��㯯�, ��⥬ �� ������������, */
   THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
               /* ���� ⮫쪮 �� ������������ */
   ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.
   APPLY "ENTRY" TO b1.
   /* Commented by KSV: ������塞 ��㧥� � QBIS */
   &IF DEFINED(SESSION-REMOTE) &THEN
   RUN UpdateBrowser(b1:HANDLE IN FRAME f-xattr). /* blodd.p */
   &ENDIF /* DEFINED(SESSION-REMOTE) */
   RETURN.
END.

ON CTRL-F4 OF FRAME f-xattr ANYWHERE
DO:
   APPLY "choose" TO bByGroup IN FRAME f-xattr.
   RETURN.
END.
/*------------------------------------------------------------------------------------------*/
/********   �᭮���� ����   **********/
IF GetSysConf("param-list") NE ? THEN DO:
   RUN SetFltFieldLIST ("sc-1",GetSysConf("param-list")).
   RUN SetFltFieldLIST ("IsEndDte","YES").
END.
OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

PAUSE 0.
FRAME f-xattr:VISIBLE = YES.

XATTRBLK:
DO
ON ERROR  UNDO XATTRBLK, LEAVE XATTRBLK
ON ENDKEY UNDO XATTRBLK, LEAVE XATTRBLK
WITH FRAME f-xattr:

   PAUSE 0.

   mGroupList:SCREEN-VALUE = "*".
   ENABLE
      mGroupList
      b1
      bflt
      bedd when mStartMode
   WITH FRAME f-xattr.

   ASSIGN
      xattr-value.description:read-only   = YES
      xattr-value.description:pfcolor     = 0
      xattr-value.description:SENSITIVE   = YES
   .

&IF DEFINED(SESSION-REMOTE) &THEN
   /* Commented by KSV: APPLY ����室�� ��� ���ᬠ�� */
   APPLY "ENTRY" TO b1.
&ENDIF

   /* Commented by KSV: �� ����᪠�� ��ॣ���� ��譨� ������ � ��㧥� */
   &IF DEFINED(SESSION-REMOTE) &THEN
   b1:PRIVATE-DATA = {&BLODD_BROWSE_NO_REFRESH}.
   &ENDIF
   

   WAIT-FOR GO, END-ERROR OF FRAME f-xattr FOCUS b1.
END.
ASSIGN FRAME f-xattr:visible = NO.

RUN DeleteOldDataProtocol IN h_base ("AllowChkDialog").

RUN SetSysConf IN h_base ("MC_BEFORE_VAL",?).
RUN SetSysConf IN h_base ("GR_BEFORE_VAL",?).
RUN SetSysConf IN h_base ("MC_AFTER_VAL",?).
RUN SetSysConf IN h_base ("GR_AFTER_VAL",?).
RUN SetSysConf IN h_base ("PROCESS_OP-EDIT",?).
RUN End-SysMes.         /* �����襭�� ����� ��⮪���஢����. */
{intrface.del}          /* ���㧪� �����㬥����. */
RETURN.

/********  ����� �᭮����� �����  **********/
PROCEDURE CheckXattrValue.             /* �� ON F1 */

   DEF VAR ferr    AS LOG  NO-UNDO.
   DEF VAR err-msg AS CHAR NO-UNDO.

   DEF BUFFER xattr FOR xattr. /* ���������� ����� */
   /* ������ �஢�� ���筥� ४�����. */
   RUN CheckFullFieldValue IN h_xclass (
      xattr-value.class,      /* ��� �����. */
      xattr-value.code,       /* ��� ४�����. */
      in-surrogate,           /* �����䨪��� ��ꥪ�. */
      xattr-value.code-value  /* ���祭�� ४�����. */
   ).
   IF RETURN-VALUE NE ""
   THEN DO:
      RUN Fill-SysMes ("", "", "0", RETURN-VALUE).
      RETURN "NO-APPLY".
   END.

   /* ������ �஢�ઠ ����室��� ��� ��, � ������ �� ����奬�
   ** ��ਡ�� "Mandatory" ��⠭����� � ���, �� ��楤�� ������樨 ������ ��
   ** ��������� �� ��⠭����� �� �६����� ⠡��� ��ਡ� "Mandatory" � ��.
   ** ����� ���ॡ����� ���� �� ᮧ����� ���� ��, ����� �� �����
   ** ����⢮���� ���� ��� ��㣮��.*/
   IF       (xattr-value.MANDATORY OR CAN-DO(mStatusMandatFld,xattr-value.code))
      AND   xattr-value.code-value EQ ""
   THEN DO:
      RUN Fill-SysMes (
         "", "", "0",
         "�� ��易�� ����� ���祭�� ४����� (" +
         (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
         (IF xattr-value.code  EQ ? THEN "?" ELSE xattr-value.code) + ").").
      RETURN "NO-APPLY".
   END.
   
   IF {assigned xattr-value.code-value}
   THEN DO:

      RUN GetXattr IN h_xclass (
            xattr-value.class,
            xattr-value.code,
            BUFFER xattr
         ).

      IF NOT AVAIL xattr
      THEN DO:
         RUN Fill-SysMes (
            "", "", "0",
            "��� ⠪��� ४����� (" +
            (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
            (IF xattr-value.code  EQ ? THEN "?" ELSE xattr-value.code) + ")."
         ).
         RETURN "NO-APPLY".
      END.

      err-msg = "४����� " + STRING(xattr.xattr-code).
                        /* �ਢ������ � �ଠ�� �� ����᪠�� ��� ���४����⮢, � ������ ��⠭����� ����� */
      IF NOT {assigned xattr.Domain-Code} THEN
         RUN SetValue IN h_base(
               INPUT-OUTPUT xattr-value.code-value,
               INPUT xattr.data-type,
               INPUT xattr.data-format,
               INPUT-OUTPUT err-msg,
               OUTPUT ferr
            ).
      xattr-value.code-value = TRIM(xattr-value.code-value).
   END.
END.

/* ���࠭���� ���祭�� ��. */
PROCEDURE SaveXattrValues.

   DEFINE VARIABLE mInstance     AS HANDLE     NO-UNDO.
   DEFINE VARIABLE mOk           AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE mRETURN-VALUE AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vXattrVal     AS CHARACTER  NO-UNDO. /* ���祭�� ��. */
   DEFINE VARIABLE XattrList     AS CHARACTER  NO-UNDO
                                 /* ����� 㠧뢠���� ���� ��, �⢥��騥 �� �ࠢ� ����㯠,
                                 ** ����� ���짮��⥫� ����� �������� ᠬ ᥡ�. */
                                 INITIAL '�ਭ��,����䮭'.
   DEFINE VARIABLE vStrCS AS CHARACTER   NO-UNDO CASE-SENSITIVE .
   DEFINE VARIABLE vListCS AS CHARACTER   NO-UNDO.
   vListCS = GetHistoryFieldsCS ( cclass ). 

   /* �஢�ઠ ���祭�� �� �� ���४⭮��� ��������� ���祭��. */
   OPEN QUERY q1 FOR EACH xattr-value.
   GET FIRST q1.
   /* Commented by KSV: ��� ���ᬠ�� ������ ��楤�� ��࠭�� ���祭�� ���.
   ** ४�, ���⮬� ����室����� � �஢�થ ���祭�� ��� ४����⮢ ��� */
   IF vNoCheck <> YES THEN
      REPEAT WHILE AVAIL xattr-value:
         IF xattr-value.class EQ "" THEN
         DO:
            GET NEXT q1.
            NEXT.
         END.

         RUN CheckXattrValue.
   
         IF RETURN-VALUE EQ "NO-APPLY"
         THEN DO:
            REPOSITION q1 TO RECID RECID (xattr-value).
            RETURN "NO-APPLY".
         END.
         GET NEXT q1.
      END.

   /* �᫨ ��-� ।���஢�����... */
   IF any-entered THEN
   XattrTRANS:
   DO TRANSACTION:

      NextXattr:
      FOR EACH xattr-value,
      FIRST xattr WHERE
               xattr.class-code EQ xattr-value.class
         AND   xattr.xattr-code EQ xattr-value.code
      NO-LOCK:

         /* ����砥� ���祭�� ��. */
         vXattrVal = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, ?).
         vStrCS = vXattrVal. 

         /* �᫨ ��-� ���﫨, � ����ᨬ ��������� � ����. */
         IF    (     vXattrVal               EQ ?
               AND   xattr-value.code-value  NE "")
            OR (     vXattrVal               NE ?
               AND   (  xattr-value.code-value  NE (IF CAN-DO(vListCS,xattr-value.CODE) THEN vStrCS ELSE vXattrVal)
                  OR    xattr.temporal))
         THEN DO:

            /* �஢������� �ࠢ� �� ।���஢���� ᢮�� ᮡ�⢥���� ��.
            ** ����蠥��� �������� ⮫쪮 ��. 㪠����� � XAttrList. */
            IF     CAN-DO(GetXclassAllChildsEx('_user'),in-class-code)     
               AND userid('bisquit') EQ in-surrogate
               AND NOT CAN-DO (XAttrList, xattr-value.code)
            THEN DO:
               RUN Fill-SysMes (
                  "", "", "0",
                  "����� �������� ४����� (" +
                  (IF xattr-value.class EQ ? THEN "?" ELSE xattr-value.class) + "." +
                  (IF xattr-value.code  EQ ? THEN "?" ELSE xattr-value.code) + ") " +
                  "ᠬ��� ᥡ�."
               ).
               UNDO NextXattr, NEXT NextXattr.
            END.

            IF     CAN-DO(GetXclassAllChildsEx('�࠭���'),in-class-code)
               AND CAN-FIND(FIRST cust-role WHERE 
                                  cust-role.cust-role-id EQ INT64(in-surrogate) 
                              AND cust-role.surrogate    EQ USERID('bisquit')) THEN
            DO:
               RUN Fill-SysMes ("", "", "0",
                                "����� �������� ४������ ᢮�� ஫�� ����� �࠭���"
                               ).
               UNDO NextXattr, NEXT NextXattr.
            END.

            IF vTempControl AND xattr.temporal AND xattr-value.code-value  NE vXattrVal AND xattr-value.since EQ ? THEN DO:

               {getdate.i
                 &DispBeforeDate = "xattr-value.code"
                 &DateLabel      = "��� ��砫�"
                 &DateHelp       = "��� ��砫� ����⢨� ४����� (F1 - ���������)"}
                 
                vTable = GetXclassFile (in-class-code).
                 
                IF end-date NE ? AND 
                   CAN-FIND (FIRST tmpsigns WHERE
                                   tmpsigns.file-name EQ vTable
                               AND tmpsigns.CODE      EQ xattr-value.code
                               AND tmpsigns.surrogate EQ in-surrogate
                               AND tmpsigns.since > end-date) THEN DO:
              
                   RUN Fill-SysMes ("", "", "4", "�����㦥�� ���祭�� � ����� ������� ��⮩. ��� ��ᬮ�� ������ Shift+F3. ���࠭��� ��������� ���祭��?").           
                   vChoice = (pick-value = "YES").
                   IF vChoice NE YES THEN DO:
                      xattr-value.code-value = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, ?).
                      RETURN NO-APPLY.
                   END.
              
                END.
                IF end-date NE ? THEN
                   xattr-value.since = end-date.
            END.  
            IF vTempControl AND xattr.Temporal AND xattr-value.since NE ? THEN DO:
               UpdateTempSignsEx(in-class-code,
                                 in-surrogate,
                                 xattr-value.code,
                                 xattr-value.since,
                                 xattr-value.code-value,
                                 ?).
            END.
            ELSE 
            /* ���࠭塞 ���祭�� � ��, ��� �஢�ન �� ��������� �訡��. */
            UpdateSigns (
               in-class-code,          /* ����� ��ꥪ�. */
               in-surrogate,           /* �����䨪��� ��ꥪ�. */
               xattr-value.code,       /* ��� ��. */
               xattr-value.code-value, /* ���祭�� ��. */
               ?).                     /* �������㥬����. */              
         END.
      END.
      RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* ����� ��ꥪ�                 */
                                      INPUT "xattrFmsChk",     /* id ��⮤�                     */
                                      INPUT "",                /* id ��� ४�����              */
                                      INPUT "",                /* ��� ��⮤�                    */
                                      INPUT ?,                 /* ��楤�� �� 㬮�砭��        */
                                      INPUT REPLACE(in-surrogate,",",";") 
      ) NO-ERROR.

/* �஢�ઠ ��쥪� 楫���� ���� ������,
** �.�. �� ।���஢���� ��ꥪ� �����᢫���� ������� ����� �஢�ப.

      /* �⪫�砥� �� ��ॣ�樨 */
      RUN PrepareInstance ("").

      /* C������� ��쥪� � ���������� ����묨 �� �� */
      RUN GetInstance IN h_data (INPUT  in-class-code, /* ����� ��ꥪ�            */
                                 INPUT  in-surrogate,  /* �����䨪��� ��ꥪ�    */
                                 OUTPUT mInstance,     /* ��� ᮧ������� ��쥪� */
                                 OUTPUT mOK).          /* ���� १����          */
      IF mOK <> YES                  OR
         NOT VALID-HANDLE(mInstance) THEN
         LEAVE XattrTRANS.

      /* �믮������ ��⮤� "validate" */
      RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* ����� ��ꥪ�                 */
                                      INPUT "validate",        /* id ��⮤�                     */
                                      INPUT "",                /* id ��� ४�����              */
                                      INPUT "",                /* ��� ��⮤�                    */
                                      INPUT ?,                 /* ��楤�� �� 㬮�砭��        */
                                      INPUT in-class-code +    /* ���᮪ �室��� ��ࠬ��஢     */
                                            CHR (1)       +
                                            STRING (mInstance)).

      /* ��ࠡ�⪠ �訡�� ��⮤� "validate"  */
      IF RETURN-VALUE = {&RET-ERROR} THEN DO:
         RUN Fill-SysMes ("", "", "0", "���������� ��࠭��� ��������� �������⥫��� ४����⮢." +
                                       "��稭�: �訡�� �� �஢�થ "                            +
                                       in-title + " (" + in-class-code + ").").
         ASSIGN mRETURN-VALUE = "NO-APPLY".
         UNDO XattrTRANS, LEAVE XattrTRANS.
      END.
*/
   END. 

   /* �������� ��쥪� 
   IF VALID-HANDLE (mInstance) THEN
      RUN DelEmptyInstance (mInstance). 
   */

   /* �� �訡�� ����樮��஢���� �� ����� ������*/
   IF mRETURN-VALUE = "NO-APPLY" THEN DO:
      OPEN QUERY q1 FOR EACH xattr-value.
      GET FIRST q1.
      IF AVAILABLE xattr-value THEN
         REPOSITION q1 TO RECID RECID (xattr-value).
   END.
   ELSE
      RUN RunClassMethod IN h_xclass (INPUT in-class-code,     /* ����� ��ꥪ�                 */
                                      INPUT "copyAttr",        /* id ��⮤�                     */
                                      INPUT "",                /* id ��� ४�����              */
                                      INPUT "",                /* ��� ��⮤�                    */
                                      INPUT ?,                 /* ��楤�� �� 㬮�砭��        */
                                      INPUT REPLACE(in-surrogate,",",";")       /* ���᮪ �室��� ��ࠬ��஢     */
      ) NO-ERROR.

   RETURN mRETURN-VALUE.
END.

/* ��⠭���� ��������� ���. */
PROCEDURE SetTitle.
   
   frame f-xattr:TITLE =   "[�.�. " + in-title + "(" +
                           in-class-code + ") (~"" + ENTRY(3, user-config-info) +
                           "~") �� " + STRING(vSignsDt) + "]".
END PROCEDURE.

/* ����䨪��� CreateXattrValue �� pp-xclass ��� ��㯯�஢�� ४����⮢ */
PROCEDURE CreateXattrValuePrivate PRIVATE.
   DEF OUTPUT PARAM TABLE        FOR xattr-value.     /* �६����� ⠡��� ४����⮢ ��ꥪ� */
   DEF INPUT  PARAM iAll         AS LOG    NO-UNDO.   /* �� ४������/⮫쪮 ��᢮���� */
   DEF INPUT  PARAM iCreate      AS CHAR   NO-UNDO.   /* �।��⠭�������� �� �� ��砫�� ���祭��� */
   DEF INPUT  PARAM iGroup       AS LOG    NO-UNDO.   /* ������� �� ��㯯� */
   DEF INPUT  PARAM iClass       AS CHAR   NO-UNDO.   /* ����� ��ꥪ� */
   DEF INPUT  PARAM iFile        AS CHAR   NO-UNDO.   /* ⠡��� */
   DEF INPUT  PARAM iSurrogate   AS CHAR   NO-UNDO.   /* ���ண�� ��ꥪ� */
   DEF OUTPUT PARAM oNumRow      AS INT64    NO-UNDO.
   DEF OUTPUT PARAM oAnyEntered  AS LOG    NO-UNDO.

   DEFINE VARIABLE mProcName AS CHARACTER  NO-UNDO.

                        /* ���࠭塞 ⥪�饥 ���ﭨ� ���祭�� ���४����⮢ �� �६.⠡��� */
   ASSIGN INPUT BROWSE b1 xattr-value.code-value NO-ERROR.
   RUN SaveTmpXattrValues.

   RUN CreateXattrValueEx in h_xclass (
      OUTPUT table xattr-value,
      vIsCode,
      iAll,
      iCreate,
      iClass,
      iFile,
      iSurrogate,
      OUTPUT oNumRow,
      OUTPUT oAnyEntered).
   oAnyEntered = oAnyEntered OR any-entered.
         /* �᫨ ����祭 ०�� "�� ��㯯��" */
   IF iGroup
   THEN DO:
            /* ������塞 ��㯯�, ���ᠭ�� �� ����� */
      CR_GR:
      FOR EACH xattr WHERE xattr.Class-Code EQ iClass
                       AND xattr.DATA-TYPE  EQ "group"
      NO-LOCK:
                  /* ������塞 ⮫쪮 � ��㯯�, �� ����� � ���짮��⥫� ���� �ࠢ� �⥭�� */
         IF NOT GetXAttrGroupPermission(xattr.Class-Code,xattr.Xattr-Code,iSurrogate,"r") THEN NEXT CR_GR.

         CREATE xattr-value.
         ASSIGN xattr-value.CLASS         = ""
                xattr-value.CODE          = xattr.Xattr-Code
                xattr-value.xattr-group   = xattr.Xattr-Code
                xattr-value.NAME          = CAPS(" " + xattr.NAME)
                xattr-value.displ-name    = IF vIsCode THEN CAPS(" " + xattr.xattr-code) ELSE CAPS(" " + xattr.NAME)
                xattr-value.DESCRIPTION   = "��㯯� ४����⮢ " + xattr.NAME + " (" + xattr.Xattr-Code + ")"
                xattr-value.MANDATORY     = NO
                xattr-value.constant      = YES
                xattr-value.code-value    = "       "
         .
      END.
            /* ������塞 �ᥢ����㯯� "��� ��㯯" */
      CREATE xattr-value.
      ASSIGN xattr-value.CLASS         = ""
             xattr-value.CODE          = "��� ��㯯"
             xattr-value.xattr-group   = ?
             xattr-value.NAME          = CAPS(" ��� ��㯯")
             xattr-value.displ-name    = CAPS(" ��� ��㯯")
             xattr-value.DESCRIPTION   = "��� ��㯯"
             xattr-value.MANDATORY     = NO
             xattr-value.constant      = YES
             xattr-value.code-value    = "       "
      .
   END.

   FOR EACH xattr-value WHERE xattr-value.xattr-group NE "":
      xattr-value.xattr-group = ENTRY (1, xattr-value.xattr-group).
   END.

                        /* ����⠭�������� ���祭�� ���४����⮢ */
   RUN RestoreTmpXattrValues.
  
/* CHR(8) - backspace
   CHR(16) -
   CHR(21) - 
     */
   RETURN.
END PROCEDURE.



PROCEDURE InitStatusCheckData PRIVATE.
   DEFINE VARIABLE vFieldStat  AS CHARACTER NO-UNDO.
/*   MESSAGE "1 mStatusMandatFld=" mStatusMandatFld SKIP "mStatusEditFld=" mStatusEditFld
      SKIP GetXattrEx(in-class-code,"StatModelOn","Initial")
      SKIP ENTRY(1,GetXattrEx(in-class-code,"StatModelOn","Data-Format"),"~/")
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
   mStatusEditFld = "".
   mStatusMandatFld = "".
      /* ��।��塞 ����祭� �� ������ ���ﭨ� �� ����� ��ꥪ� */
   IF GetXattrEx(in-class-code,"StatModelOn","Initial") NE ENTRY(1,GetXattrEx(in-class-code,"StatModelOn","Data-Format"),"~/")
   THEN  RETURN.
      /* ��।��塞 ��� ����, � ���஬ �࠭���� ����� */
   vFieldStat = DYNAMIC-FUNCTION ("GetFieldStat" IN h_stdtrg,GetXclassProgress(in-class-code)).
   IF NOT {assigned vFieldStat} THEN   RETURN.
      /* ��।��塞 ����� */
   vStatus = GetValueAttr(GetXclassProgress(in-class-code),in-surrogate,vFieldStat).
   IF vStatus = "" THEN   RETURN.
      /* �饬 ���ᠭ�� ����� �� ����� */
   FIND FIRST xstatus 
      WHERE xstatus.class-code  EQ in-class-code
        AND xstatus.status-code EQ vStatus
   NO-LOCK NO-ERROR.
   IF NOT AVAIL xstatus THEN  RETURN.
      /* ��࠭塞 ᯨ᮪ ��易⥫��� ��� ����� ����� */
   mStatusMandatFld = xstatus.mandatory-fields. 
      /* ��� ��࠭�祭�� �� ।���஢���� ����� */
   IF xstatus.edit-fields EQ "*" THEN  RETURN.
   mStatusEditFld = xstatus.edit-fields .
END PROCEDURE.


/* ����樮��஢���� ��㧥� �� 㪠������ ������ ४�����.
** ����� �ᯮ�짮���� �� ���⥪�� �맢����� ��楤�� (���ਬ��, ��⮤� chkupd ४�����)
** ����� ��।����� � ����⢥ 㪠��⥫� �� ������ ��� ����樮��஢���� ��� ROWID
** �����, ⠪ � ��� ४����� */
PROCEDURE RepositionCursor.
   DEF INPUT  PARAM iRowID     AS ROWID  NO-UNDO.
   DEF INPUT  PARAM iXAttrCode AS CHAR   NO-UNDO.

                        /* �᫨ �� ��।�� ROWID, �饬 ������ �� ���� ४����� */
   IF iRowID EQ ? THEN
   DO:
      FIND FIRST xattr-value WHERE xattr-value.code EQ iXAttrCode NO-LOCK NO-ERROR.
      IF AVAIL xattr-value THEN
         iRowID = ROWID (xattr-value).
   END.
                        
   IF iRowID NE ? THEN
   DO:
      CLOSE QUERY q1.
               /* �᫨ ����祭 ०�� "�� ��㯯��", */
      IF     mByGroup
         AND mGroupList EQ "*"
                  /* �ਬ��塞 ���஢�� �� ��㯯�, ��⥬ �� ������������, */
      THEN OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere} {&ByGroupSort}.
                  /* ���� ⮫쪮 �� ������������ */
      ELSE OPEN QUERY q1 FOR EACH xattr-value {&XattrWhere}.

      /* ����樮���㥬 ����� */
      REPOSITION q1 TO ROWID iRowID NO-ERROR.

      /* �������� ��� */
      APPLY "ROW-ENTRY" TO BROWSE b1.
   END.

   RETURN.
END PROCEDURE.

/* ��⮤� �� �����᪥ */
PROCEDURE GetTTXattrValue.
   DEF OUTPUT PARAM TABLE FOR xattr-value.
   DEF OUTPUT PARAM oXAEdHdl AS HANDLE NO-UNDO.

   oXAEdHdl = THIS-PROCEDURE.

   RETURN.
END PROCEDURE.

PROCEDURE SetTTXattrValue.
   DEF INPUT PARAM TABLE FOR xattr-value.
   RETURN.
END PROCEDURE.

/* ��࠭塞 ��������, �� �� ��࠭���� ���祭�� ���४����⮢ */
PROCEDURE SaveTmpXattrValues:
DEF BUFFER xattr-value FOR xattr-value.
   EMPTY TEMP-TABLE xattr-value-copy.
   FOR EACH xattr-value:
      CREATE xattr-value-copy.
      BUFFER-COPY xattr-value TO xattr-value-copy.
   END.
END PROCEDURE.

/* ����⠭�������� ��������, �� �� ��࠭���� ���祭�� ���४����⮢ */
PROCEDURE RestoreTmpXattrValues:
   DEF BUFFER xattr-value FOR xattr-value.
   IF any-entered THEN
   FOR EACH xattr-value-copy:
      FIND FIRST xattr-value WHERE xattr-value.class EQ xattr-value-copy.class
                               AND xattr-value.code  EQ xattr-value-copy.code
      NO-ERROR.
      IF NOT AVAIL xattr-value THEN
      DO:
         CREATE xattr-value.
         BUFFER-COPY xattr-value-copy TO xattr-value.
      END.
      ELSE
         IF xattr-value.code-value NE xattr-value-copy.code-value THEN
            xattr-value.code-value = xattr-value-copy.code-value.
   END.
END PROCEDURE.

/* ����⠭�������� ��砫�� ���祭�� ���४����⮢ */
PROCEDURE RestoreInitXattrValues:
DEF BUFFER xattr-value FOR xattr-value.
   FOR EACH xattr-value:
      xattr-value.code-value = GetXattrValueEx (cclass, in-surrogate, xattr-value.CODE, "").
   END.
END PROCEDURE.

/* �����஢���� ����� ��� ������������ ।���஢���� �� �����६���� ��᪮�쪨�� ���짮��⥫ﬨ */
PROCEDURE LockRecord PRIVATE.
   DEF INPUT  PARAM iFileName  AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSurrogate AS CHAR   NO-UNDO.

   DEF VAR vOK AS LOG    NO-UNDO.

   FIND FIRST signs WHERE signs.file-name EQ iFileName
                      AND signs.surrogate EQ iSurrogate
   NO-LOCK NO-ERROR.
   IF NOT AVAIL signs THEN
   DO:
      FIND FIRST tmpsigns WHERE tmpsigns.FILE-NAME EQ iFileName
                            AND tmpsigns.surrogate EQ iSurrogate
      NO-LOCK NO-ERROR.
      IF NOT AVAIL tmpsigns THEN 
      DO:
                        /* �᫨ �� ��� �� ������ ���祭�� �� �� ��ꥪ�,
                        ** ᮧ����� �㦥���� ���祭�� ��� ⮣�, �⮡� �뫮 �� �������஢��� */
                        /* �⪫�祭�� �ਣ��஢, �⮡� �� ��ᠫ��� ����� �� �㦥����� �� */
         ON CREATE OF signs OVERRIDE DO: END.
         ON WRITE  OF signs OVERRIDE DO: END.
         DO TRANS:
            CREATE signs.
            ASSIGN signs.FILE-NAME  = iFileName
                   signs.surrogate  = iSurrogate
                   signs.code       = "$lock"
                   signs.code-value = "$lock"
            .
         END.
                        /* ����⠭�������� �ਣ���� */
         ON CREATE OF signs REVERT.
         ON WRITE  OF signs REVERT.
      END.
   END.

   DO TRANS:
      CREATE BUFFER vHBL FOR TABLE (IF AVAIL signs THEN "signs" ELSE  "tmpsigns").

                        /* ����⪠ �����஢�� ����� */
      &IF DEFINED(ORACLE) &THEN
         CREATE BUFFER vDBufferMutex FOR TABLE "mutex"
            &IF DEFINED(SESSION-REMOTE) = 0 &THEN 
            IN WIDGET-POOL "DATA"
            &ENDIF 
         .
         vHBL:FIND-BY-ROWID(IF AVAIL signs THEN ROWID(signs) ELSE ROWID(tmpsigns),NO-LOCK,NO-WAIT) NO-ERROR.
         {mutex-lock-dyn.i
            &RES-FIND   = vOK
            &BUFFER     = vDBufferMutex
            &filename   = (vHBL:TABLE) 
            &recid      = (vHBL:RECID)
            &for-lock   = "EXCLUSIVE"
         }
         IF vOK THEN
            DELETE OBJECT vHBL NO-ERROR.
      &ELSE
         vOK = vHBL:FIND-BY-ROWID(IF AVAIL signs THEN ROWID(signs) ELSE ROWID(tmpsigns), EXCLUSIVE-LOCK, NO-WAIT) NO-ERROR.
      &ENDIF
                        /* �᫨ ����⪠ �� 㤠���� - ����� ���.४������ 㦥 ��-� ।������ */
      IF NOT vOK THEN
      DO:
                        /* �뢮� ᮮ�饭�� */
         RUN Fill-SysMes IN h_tmess ("", "", "", 
                            "�訡�� �����஢�� �� ��������� �������⥫��� ४����⮢ ��ꥪ� '" + 
                            iSurrogate + "' " + ". ���஡�� �����!").
         &IF DEFINED (ORACLE) &THEN
            vDBufferMutex:FIND-FIRST("where rec-id EQ " + STRING (vHBL:RECID) +
                   "  and filename EQ ~"" +  vHBL:TABLE + "~"",NO-LOCK) NO-ERROR . 
         IF vDBufferMutex:AVAIL AND vDBufferMutex:RECID > 0 THEN
         RUN wholocks2.p (vDBufferMutex:RECID, "mutex", "").
         DELETE OBJECT vHBL NO-ERROR.
         &ELSE
         vHBL:FIND-BY-ROWID(IF AVAIL signs THEN ROWID(signs) ELSE ROWID(tmpsigns), NO-LOCK) NO-ERROR.
         IF vHBL:AVAIL THEN
            RUN wholocks2.p (vHBL:RECID, IF AVAIL signs THEN "signs" ELSE "tmpsigns", "").
         &ENDIF
                        /* ��室 � �訡��� */
         RETURN ERROR.
      END.
   END.
   RETURN.
END PROCEDURE.

/* ��������஢���� ����� �� ����砭�� ।���஢���� */
PROCEDURE UnLockRecord PRIVATE.
   DEF INPUT  PARAM iFileName  AS CHAR   NO-UNDO.
   DEF INPUT  PARAM iSurrogate AS CHAR   NO-UNDO.
   DEF BUFFER signs FOR signs.

   IF VALID-HANDLE (vHBL) THEN
      DELETE OBJECT vHBL NO-ERROR.

   &IF DEFINED(ORACLE) &THEN
   IF VALID-HANDLE (vDBufferMutex) THEN
      DELETE OBJECT vDBufferMutex NO-ERROR.
   &ENDIF
                        /* �⪫�祭�� �ਣ��஢, �⮡� �� ��ᠫ��� ����� �� �㦥����� �� */
   ON DELETE OF signs OVERRIDE DO: END.
   ON WRITE  OF signs OVERRIDE DO: END.
   DO TRANS:
                        /* ���� �㦥���� ����� ��� �����஢�� */
      FIND FIRST signs WHERE signs.file-name  EQ iFileName
                         AND signs.surrogate  EQ iSurrogate
                         AND signs.code       EQ "$lock"
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                        /* �᫨ ��諨 ⠪��, 㤠�塞 */
      IF AVAIL signs THEN DELETE signs.
   END.
                        /* ����⠭�������� �ਣ���� */
   ON DELETE OF signs REVERT.
   ON WRITE  OF signs REVERT.
   RETURN.
END PROCEDURE.
/* $LINTUSER='SHOI' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='28/01/2015 22:37:42.311+04:00' */
/* $LINTFILE='xattr-ed.p' */
/*prosignzxalI+jeNlsjPag40awFJQ*/