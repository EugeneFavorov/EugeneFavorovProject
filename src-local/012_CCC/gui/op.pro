
/* +++ op.pro was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:32am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: OP.PRO
      Comment: ����䥩�� ��⮤� ��� ���㬥�⮢
   Parameters:
         Uses:
      Used by:
      Created: 26/03/01   Serge
     Modified: 15/08/2001 Nik
               1. ������� �㭪�� ClueAcctCurr - ��� "᪫�������" ��� � ������, �� ������
                  �������� �� �맮�� (if then else)
               2. "��஢���" ��室�� ��� � ��楤��� Check-op-Entry � err-event
               3. �஢�ઠ ����⮢ ���⪮� ��������� ���묨 ���祭�ﬨ ���� "contract"
                  ⥯��� �� �����⨬� ���祭�� �⮣� ���� ����᫥��
                  � ��६����� "check-contract" � "check-event"
               4. ��᪮�쪮 �������� �孮����� �஢�ન ����� ��� ��� �� �।���
               5. �맮�� {message} �������� �� message.
               6. �������� ���� ��ਥ�� ����⢨� - ���⠭���� �� ����⥪� �������襭���
                  ���⮢
               7. �������� ����⠭�� ADD_* - ��� �롮� ��������� ����⢨�
     Modified: 07/09/2001 Nik
     Modified: 10/10/2001 Nik ��� ���� ������� �������
     Modified: 25/10/2001 Nik ��������� �㭪�� �஢�ન �����
     Modified:  1/11/2001 Nik ������ ���� ��।������ ����� ���⪠ �㭪樥�
                              ��ࠢ����� �訡�� ��ᯨᠭ�� � ����⥪�-2
     Modified:  2/11/2001 Nik ����७�� ᯨ᪠ ⨯�� �����஢���� ��⮢
     Modified:  1/12/2001 Nik ������ 䠩�� kautrig.p ��楤�ன Kau-Trigger
     Modified: 16/02/2002 kostik 0006313 ����室��� ���樠����஢��� cur-op-trans
     Modified: 21/03/2002 NIK ���७� ��ࠡ�⪠ �����஢�� ���
                           1. ����祭� ��楤�� Op-Entry-Delete
                           2. ������� �㭪�� CheckAcctStatusEx ���७��� ��ࠡ�⪨
                              �����஢��
     Modified: 11/04/2002 kolal ��������� �ࠢ����� �뢮�� ᮮ�饭��: �� �࠭ ���
                                �१ ᯥ�. ��ࠡ��稪 �訡��. ��� 5929.
     Modified: 23/04/2002 NIK ����饭� ������� �஢���� �� �� ����, ����⮬�
                              � ᫥���饬 ����.
     Modified: 13/05/2002 kolal ��������� ��।������ ��६����� gLogProc � gLogMessage ���
                                �ࠢ����� �뢮�� ᮮ�饭��. ��������� ��楤�� GetLogParam,
                                ���樠��������� �� ��६���� �� sysconf. ��� 5929.
     modified: 04/06/2002 kostik  0006902 ����室��� �㫮 ���� �஢��� �� �����஢�� ��ᨢ���� ��� � ��.
                                          (g-crd1`.p)
     Modified: 23/04/2002 NIK �믮������ ����஫� ����⮢ �� �ᥬ ��⠬ ��� ����ᨬ���
                              �� �����祭��
     Modified: 03/09/2002 Gunk ����஫� ��ࠬ��� "������஢"
     Modified: 02/10/2002 NIK  ���⪠� �����஢�� ��᭮�� ᠫ줮
     Modified: 03/10/2002 Gunk �맮� � ��ࠡ�⪠ �����㬥�� kau
     Modified: 09/10/2002 shin ��᢮���� � kau-trigger pick-value ��� ��ࠡ�⪨ � ��ᬠથ
     Modified: 29/01/2003 ���� ��������� �஢�ઠ ������ ����⥪�2 �� �������
     Modified: 06/02/2003 SAP   ��।������ ���� ��� �������� ���㬥�⮢ � �� ��� ���������� ��� (9741)
     Modified: 27/03/2003 NIK  ��������� ���������᪨� ᮮ�饭��
     Modified: 26/05/2003 fedm ��������� �㭪�� GetDocTypeName,
                               �����頥� ������������ �� ⨯� ���㬥��.
     Modified: 22/07/2003 NIK  ����஫� ���⭨��� �� �����⮢
     Modified: 30/09/2003 TSL  0016217 - �뢮� ᮮ�饭�� �१ �㦡� ᮮ�饭��
     Modified: 01/10/2003 Mike �㭪�� CheckAcctStatusEx ���७��� ��ࠡ�⪨
                               �����஢�� ࠧ��� �� 2 c ������������ �맮�� ��� ��뫪� �� op.
     Modified: 01/10/2003 Mike ��������� ���. Check-BankInf - ���ଠ樮���� -  ��� ��६����� flag-error.
     Modified: 07/04/2004 Abko ����⮢�� ᠫ줮 ����⭮�� ��� �뢮����� � ����� 0025160.
     Modified: 13/04/2004 kolal �������� ����஫� �� ࠡ��� � �����⮩ ᬥ��� ���. ��� 28313.
     Modified: 03/09/2004 Abko 0035245 ��।������ ४����⮢ ���㬥�� �� �����. 117 .
     Modified: 04/12/2004 kostik 0021328  �� ᬥ�� ����� ���� ��易⥫쭮 
                                          �஢����� � ���㬥�� ����襭�� ������
                                          (�� Check-op �뭥ᥭ� �஢�ઠ ������
                                           � �⤥��� �����㬥�� Check-Op-balance)
     Modified: 17.12.2004 abko   0038777 ����祭��/�⪫�祭�� ����஫� 117�
     Modified: 29.07.2005 gorm   0048274 ��ࠢ���� �訡�� ��।������ ��⠇���
     Modified: 19.09.2005 19:17 KSV      (0046989) ��९஥��஢��� ��楤���
                                         Check-Op, Check-Op-Entry �
                                         Check-balance. ����� ��� ��⠢����
                                         �����誨, ����� �������� � �����
                                         ��⥬� ������樨.
     Modified: 06.10.2005 13:04 KSV      (0051030) ����஭�����
     Modified: 10.01.2008 MUTA 0084518 �᫨ ��� �����஢�� ��� ᮤ�ন��� � ��
                               ���끫���, � ����� ���⠭���� �� ����⥪� 2 �믮������
                               ���⠭���� �� ����⥪� ���⁫�� 
     Modified: 25.03.2008 MUTA 0087581  ��� ��।���� ���⥦�. �� ���끫��� ��७�ᥭ � ��㯯� ���⁫��.                                 
     Modified: 01.04.2008 MUTA 0088445  ��� �����஢�� �㬬� � ����� ���⪮� �� ᯨᠭ�� � ����⥪�.
     Modified: 06.06.2008 MUTA 0088417  �㭪�� Chksgnopint
     Modified: 10/07/2008 kraw (0091871) �⪫�祭�� �஢�ન ᮡ�⨩ �� {&op_kind_no_test_error} � {&cod-event_no_test_error}
     Modified: 16/10/2008 MUTA 0089310 � �࠭���樨 _CANLBOT �� ����� ��� �����㬬 �믮������ ���⠭���� �� ����⥪� 2 
     Modified: 03/04/2009 kraw (0100898) "� ������ ���� ���㬥��� �� ���"
     Modified: 15/04/2009 kraw (0108603) ����� ��⥫�� �⡮� � CheckCard2Entry
     Modified: 27/04/2009 kraw (0107055) �⪫�祭�� �஢�ப �� ��८業��.
     Modified: 30/04/2009 kraw (0103092) ��ࠢ����� �訡�� � 0108603
     Modified: 13/07/2010 kraw (0116619) ����� ᯨ�뢠�� � ����⥪� 2, �� �५������ ᭮�� �� ��� ���⠢���.
     Modified: 22/12/2010 krok (0133053) ������� ⨯����� ����� ���㬥�⮢ �� �����䨪�樨 ��.
                                         GetDocType, GetDocTypeDigital, AvailDocType, AvailDocTypeD, GetDocTypeName
                                         �८�ࠧ����� � ��楤���.
     Modified: 20/05/2011 kraw (0145067) �ᯮ��㥬 GetBlkType
     Modified: 31/08/2011 kraw (0145966) ���⠭���� �� �2 �� �� �������2

   �㭪樨:
     GetLimitPosition
     CheckAcctStatusEx
     CheckAcctStatEx
     OpIsSvod          - ���㬥�� ���� ᢮���
     Chksgnopint       - �஢����, ���� �� ��뫪� �� ���㬥�� � ���. ४������ ������47423 ��� ������47423
     cashCBCodeType    - ��।���� ⨯ ���� �� ��� ���㬥�� ����� �� ��஭� ���� ����� � �஢����
     
   ��楤���:
     Check-Op-Entry    - op-enupd.p
     Op-Entry-Delete   -
     Anl-Stb           - anl-stb.p
     Err-Event         - errevent.p
     Check-Op          - op.upd
     Check-Op-balance  - �஢�ઠ ������ �஢���� � ���㬥��
     Check-Bank        - ࠧ�� ����
     Kau-Trigger       - ࠧ�� ����
     UpdStatusAfterFin - ������ ����� ��᫥ 䨭�⪠��
     GetDocTypeDigital   - ��।������ ���� �� ���㬥�� �� ⨯� ���㬥��
                           � ⨯� ���� ��
     GetDocTypeDigitalEx - �������筠 �।��饩 ��楤��, �� �� ������⢨�
                           �ॡ㥬��� ���� �� �����頥� ��室�� ����७��� ���
     AvailDocTypeDigital - �஢�ઠ ����⢮����� � ��⥬� ��������� ����
                           ���㬥�� �� �����䨪�樨 ��
     GetDocType          - ��।������ ����७���� ���� ���㬥��
                           �� ���� �� ���㬥�� � ⨯� ���� ��
     AvailDocType        - �஢�ઠ ����⢮����� � ��⥬� ��������� ����७����
                           ���� ���㬥�� �� �����䨪�樨 �������
     GetDocTypeName      - ��।������ ������������ ⨯� ���㬥��
                           � �����䨪�樨 �������
     GetCashDocTypeDigital - ��।������ ���� �� ���㬥�� ����� �� �஢����
                             � ��࠭���� ����
     GetLinkedOps        - ��।������ ᯨ᪠ �����䨪��஢ ���㬥�⮢, ���⪮
                           �易���� � �������                             
*/

{globals.i}
{sh-defs.i}
{zo-defs.i}
{intrface.get xclass}
{intrface.get kau}
{intrface.get instrum}
{intrface.get count}    /* ������⥪� ��� ࠡ��� � ���稪���. */
{intrface.get acct}
{intrface.get tmess}                             /* ��㦡� ᮮ�饭�� */
{intrface.get crd}   /* ������⥪� �����㬥�⮢ ࠡ��� � ����⥪�� */
{intrface.get blkob}    
{topkind.def}

&global  ADD_DEPO 1                              /* ����९��� � �������                        */
&global  ADD_CRED 2                              /* �뤠�� �।��                                */
&global  ADD_CRD2 3                              /* ���⠢��� �� ����⥪� 2                     */
&global  ADD_CRD3 4                              /* ���⠢��� �� ����⥪� �������襭��� ���⮢*/
&global  ADD_CONT 5                              /* �த������ �믮������                        */
&global  ADD_RJCT 6                              /* �⬥���� �믮������                          */
&GLOBAL  ADD_CRDB 7                              /* ���⠢��� �� ����⥪� ���⁫��              */

&global  saldo_no   "�।�०�����"
&global  saldo_yes  "�����"

&GLOBAL-DEFINE OpMess             RUN Fill-SysMes("",
&GLOBAL-DEFINE FillProgressError  RUN Fill-ProgressErr("").
&GLOBAL-DEFINE RelOp              RELEASE op NO-ERROR.                             ~
                                  IF ERROR-STATUS:ERROR THEN DO:                   ~
                                     IF flager = 0 THEN flager = 1.                ~
                                     ~{&OpMess~}"comm10","","%s=" + {&RETURN_VALUE}). ~
                                     RETURN.                                       ~
                                  END.

&GLOBAL-DEFINE op_kind_no_test_error "_CANLBOP"
&GLOBAL-DEFINE cod-event_no_test_error "d_dspb,d_crp"

/* ��ॢ� �࠭���樨 ��� fGetTrnFirstOp*/
DEFINE TEMP-TABLE tt-tree NO-UNDO
   FIELD level    AS INT64 /* �஢��� ᠡ�࠭���樨 ��ॢ� */
   FIELD op-trans AS INT64 /* ��뫪� �� ᮮ�. ᠡ�࠭����� */
.

DEF VAR check-event    as char init "d_blp,d_dsp,d_dspr,k_ksp,d_crp" NO-UNDO.  /*"d_blp,d_dsp,k_ksp,k_ksa"*/

DEF VAR type-bal as logical NO-UNDO.

{lim-pos.i}

{get-fmt.i &obj=B-Acct-Fmt}
{get-fmt.i &obj=O-Acct-Fmt &pref=O-}

DEF VAR settA              AS CHAR NO-UNDO.
DEF VAR settP              AS CHAR NO-UNDO.
DEF VAR data-zo            AS CHAR NO-UNDO.
DEF VAR data-zo-nu         AS CHAR NO-UNDO.
DEF VAR zaklobor           AS CHAR NO-UNDO.
DEF VAR setrassh           AS CHAR NO-UNDO.
DEF VAR Cart2Flag          AS LOG  NO-UNDO.
DEF VAR saldo_check_sett   AS LOG  INIT NO   NO-UNDO.
DEFINE VARIABLE mchar AS CHAR NO-UNDO. /*�ᯮ����⥫쭠� ��६����� ���
                                         �ନ஢���� ᮮ�饭��*/
DEFINE VARIABLE vChkAvtoKart  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vChkAvtDep    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vChkAvtOv     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vKontrKopp    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vMaskDohRash  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAccessMask   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAccessStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAccessContAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE vCartName     AS CHARACTER NO-UNDO.

ASSIGN
  vChkAvtOv        = FGetSetting("��⎢",?,?) = "��"
  vChkAvtDep       = FGetSetting("��℥�",?,?) = "��"
  vChkAvtoKart     = FGetSetting("��⮊���",?,?) = "��"
  settA            = FGetSettingEx("������������⮢","��⨢���",  "*", no)
  settP            = FGetSettingEx("������������⮢","���ᨢ���", "*", no)
  data-zo          = FGetSettingEx("��⠇�",?,?, yes)
  data-zo-nu       = FGetSettingEx("��⠇���",?,?, yes)
  zaklobor         = FGetSettingEx("��������",        ?,            ? , no)
  setrassh         = FGetSettingEx("���莡�",         ?,            ? , yes)
  type-bal         = FGetSettingEx("������",          ?,            "", no) EQ "��"
  Cart2Flag        = FGetSettingEx("����2㊫����",   ?,         "���", no) EQ "��"
  saldo_check_sett = (TRIM(FGetSetting("�������줮",?,"")) EQ {&saldo_yes})
  vKontrKopp       = FGetSetting("�⠭���","���⊮��","")
  vMaskDohRash     = FGetSetting("�⠭���","��⠄���","")
  vAccessMask      = FGetSetting("�⠭���", "AccessAcct", "")
  vAccessStatus    = FGetSetting("�⠭���", "AccessStatus", CHR(251))
  vAccessContAcct  = FGetSetting("�⠭���", "AccessContAcct", "")
.

IF NUM-ENTRIES(vMaskDohRash,";") GT 1 THEN
   vMaskDohRash = REPLACE(vMaskDohRash,";",",").

{chk_use.i}
{kautools.lib &Noacctread=YES &user-rights=yes}

DEF NEW SHARED VAR lim-pos   AS DECIMAL   NO-UNDO. /* �뫮 new shared - ��� errevent */
DEF NEW SHARED VAR mbl-pos   AS DECIMAL   NO-UNDO.

DEFINE NEW SHARED VARIABLE mAcctStatCode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE mAcctStatType AS CHARACTER NO-UNDO.
{defoptr.i new}                                       /* �뫮 new shared - ��� errevent */

/*�஢�ઠ ������ ����⥪� �� ������� ��� �����*/
{ch_cart.i
   &USE_KAU  = yes
   &ChkDate  = yes }

/* ����, �몫���騩 �뢮� ᮮ�饭�� �� �࠭
   ��� OP.UPD � OP-ENTRY.UPD */
DEFINE VARIABLE gLogMessage AS LOG   NO-UNDO.
/* ��� ��楤���, ᮤ�ঠ騩 ��ࠡ��稪 �訡�� */
DEFINE VARIABLE gLogProc AS HANDLE     NO-UNDO.
/* ��⮪ ��� �뢮�� ᮮ�饭�� �� �訡��� �� �����஢���� */
DEF STREAM o-err.


/*
   ��ଠ�஢���� ���⪠ �� ���� 
   ������⥫�� - 1,000.00��
   ����⥫�� - 1,000.00��
   ����          - 0.00 

*/
FUNCTION fmt-summ RETURNS CHARACTER (INPUT vSumm AS DECIMAL):

   DEFINE VARIABLE mCount  AS INT64     NO-UNDO.
   DEFINE VARIABLE mFormat AS CHARACTER   NO-UNDO.
   /* ������ �ਠ�� */
   DO mCount = 1 TO LENGTH(ENTRY(1,STRING(ABS(vSumm)),'.')):
      mFormat = "z" + mFormat.
      IF (mCount MODULO 3) EQ 0 THEN
         mFormat = "," + mFormat.
   END.
   mFormat = TRIM(mFormat,",") + ".99".
   IF      vSumm GT 0 THEN RETURN (STRING(ABS(vSumm),mFormat) + "��").
   ELSE IF vSumm LT 0 THEN RETURN (STRING(ABS(vSumm),mFormat) + "��").
   ELSE RETURN STRING(vSumm,mFormat).
END FUNCTION.

/*���㬥�� ���� ᢮���*/
FUNCTION OpIsSvod RETURNS LOG
         (INPUT ipRecOp AS RECID):

   DEF BUFFER op FOR op.

   FIND FIRST op WHERE RECID(op) EQ ipRecOp NO-LOCK NO-ERROR.
   IF AVAIL op THEN
      RETURN GetXAttrValue("op",STRING(op.op),"link-op-sum") NE "".
   ELSE
      RETURN NO.
END FUNCTION.

/*-----------------------------------------------------------------------------------------------*/
/* ��।���� ���祭�� ��ࠬ��஢ gLogMessage � gLogProc �� ⠡��� sysconf                      */
/*-----------------------------------------------------------------------------------------------*/
PROCEDURE GetLogParam:
   {getlogp.i}
END.

/*-----------------------------------------------------------------------------------------------*/
/* ��������� ����� ��� � ��� ������ ��� �뢮�� ᮮ�饭��                                       */
/*-----------------------------------------------------------------------------------------------*/
FUNCTION ClueAcctCurr RETURNS CHARACTER (INPUT in-acct AS CHARACTER, INPUT in-curr AS CHARACTER).
   RETURN in-acct + ( IF in-curr NE ""
                     THEN "/" + in-curr
                     ELSE ""). /* ���� �� ��� �⮡ࠦ����� ��� ������ */
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �����誠 ��� ����� �맮���. ����஫� ���४⭮�� �஢����.   
  Parameters:  op-entry   - ���� � �஢�����
               in-op-date - �� �ᯮ������
               onlycheck  - �� �ᯮ������
               action     - ���祭�� ���. ��ࠬ��� ��।�������� ��⮤� CHKUPD
               off-corracct - ���祭�� "" ����砥� �஢��� ����ᯮ����樨 ��⮢
               flager     - RC:  0 - ��� �訡��, <> 0 - �訡�� 
------------------------------------------------------------------------------*/
PROCEDURE Check-Op-Entry:
   DEFINE PARAMETER BUFFER op-entry   FOR op-entry.
   DEFINE INPUT  PARAMETER in-op-date AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER onlycheck  AS LOG       NO-UNDO.
   DEFINE INPUT  PARAMETER action     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER off-corracct AS  CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER flager       AS  INTEGER   NO-UNDO.

   DEFINE VARIABLE vParams AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vValues AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOInfo  AS CHARACTER  NO-UNDO.

   IF (op-entry.currency NE "" AND op-entry.amt-cur EQ 0 AND op-entry.qty EQ 0) THEN
      RETURN.
  
   /* Commented by KSV: �몫�砥� �஢��� ४����⮢ �� ����奬� */
   RUN SetLstParam IN h_strng ({&VP_ATTR_CHECK} + op-entry.Class-Code,"NO",
                               INPUT-OUTPUT vParams,
                               INPUT-OUTPUT vValues,
                               {&VP_PARAM_DLM}).

   /* �᫨ ������, � ����砥� �஢��� ����ᯮ����樨 ��⮢ */
   RUN SetLstParam IN h_strng ({&VP_CORRACCT_CONTROL},
                               IF {assigned off-corracct} THEN "NO" ELSE "YES",
                               INPUT-OUTPUT vParams,
                               INPUT-OUTPUT vValues,
                               {&VP_PARAM_DLM}).

   /* Commented by KSV: ��।��� ���. ��ࠬ��� ��� ��⮤� CHKUPD */
   RUN SetLstParam IN h_strng ({&VP_EXT_PARAM} + op-entry.Class-Code,action,
                               INPUT-OUTPUT vParams,
                               INPUT-OUTPUT vValues,
                               {&VP_PARAM_DLM}).

   /* Commented by KSV: �������㥬 �஢���� */
   RUN ValidateObject IN h_valid (op-entry.Class-Code,?,?,
                                  STRING(op-entry.op) + "," + STRING(op-entry.op-entry),
                                  ?,?,vParams,vValues,
                                  OUTPUT vOInfo).
   /* Commented by KSV: ������ �訡�� */
   IF {assigned RETURN-VALUE} THEN
   DO:
      flager = INT64(vOInfo) NO-ERROR.
      IF flager = 0 THEN flager = 1.
      RETURN.
   END.

   RETURN .
END.

/*-----------------------------------------------------------------------------------------------*/
/*                                                                                               */
/*-----------------------------------------------------------------------------------------------*/
PROCEDURE Anl-Stb.
   /*
                  ������᪠� ��⥣�஢����� ��⥬� �������
       Copyright: (C) 1992-1996 ��� "������᪨� ���ଠ樮��� ��⥬�"
        Filename: anl-stb.p
         Comment: ��楤�� ࠡ��� � �����⨪�� �� �����ᮢ� ��⠬�
                  �� ᬥ�� �����.
      Parameters:
            Uses:
         Used by:
         Created: 19/05/1997 eagle
        Modified: 04/06/1997 nata
   */

  DEF PARAM BUFFER op-entry FOR op-entry.
  DEF OUTPUT PARAM flager AS INT64 NO-UNDO. /* 0 - ��� �訡��, <> 0 - �訡�� */

  DEF VAR vKauID AS CHAR  NO-UNDO.
  DEF VAR rid    AS RECID NO-UNDO.
  DEFINE VARIABLE vMult   AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE vCreate AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE vCnt    AS INT64     NO-UNDO.
  DEFINE VARIABLE vStat   AS CHARACTER   NO-UNDO.

   IF (op-entry.currency NE "" AND op-entry.amt-cur EQ 0) THEN
      RETURN.

  rid = RECID(op-entry).

  DEF VAR str-st AS CHAR NO-UNDO.
  DEF BUFFER ycode FOR code .

  flager = 1.
  FIND ycode WHERE ycode.class EQ "�����"
               AND ycode.code  EQ op-entry.op-status NO-LOCK NO-ERROR.
  {&FillProgressError}

  /* �஢�ઠ 蠡���� ��� �� ������ ��� */
  {anl-stb.i &db-cr=-db &name-dc="�����" &idc=2 &op-entry=op-entry &log=yes}

  /* �஢�ઠ 蠡���� ��� �� �।��� ��� */
  {anl-stb.i &db-cr=-cr &name-dc="�।��" &idc=1 &op-entry=op-entry &log=no}

  flager = 0.
END.

/*-----------------------------------------------------------------------------------------------*/
/*    ��ࠡ�⪠ �訡��, �����㦥���� � ��楤�� Check-Op-Entry                                  */
/*-----------------------------------------------------------------------------------------------*/
PROCEDURE err-event.
/*
   *ksa - �।�⮢�� ᠫ줮 ��⨢���� ���
   *ksp - �।�⮢�� ᠫ줮 ���ᨢ���� ���
   *dsa - ����⮢�� ᠫ줮 ��⨢���� ���
   *dsp - ����⮢�� ᠫ줮 ���ᨢ���� ���
   *crp - ���� ����⥪� � ������ ���ᨢ���� ���
   d_*  - �� ����⮢�� ��஭� �஢����
   k_*  - �� �।�⮢�� ��஭� �஢����
*/
   DEF PARAM BUFFER op-entry FOR op-entry.
   DEF PARAM BUFFER acct     FOR acct.
   DEF INPUT  PARAM cod-event AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM flager    AS INT64   NO-UNDO.

   DEF VAR fl-poddep    AS   LOG INIT NO          NO-UNDO.
   DEF VAR fl-cardinc   AS   LOG INIT NO          NO-UNDO.
   DEF VAR fl-ovdr      AS   LOG INIT NO          NO-UNDO.
   DEF VAR mess-saldo   AS   CHAR FORMAT "X(20)"  NO-UNDO.
   DEF VAR mes-dop      AS   CHAR FORMAT "X(20)"  NO-UNDO.
   DEF VAR shbal        LIKE  sh-bal              NO-UNDO.
   DEF VAR shval        LIKE  sh-val              NO-UNDO.
   DEF VAR recalc       AS   LOG INIT TRUE        NO-UNDO.
   DEF VAR choose-list  AS   CHAR FORMAT "X(120)" NO-UNDO.
   DEF VAR trans-list   AS   CHAR FORMAT "X(30)"  NO-UNDO.
   DEF VAR str1         AS   CHAR                 NO-UNDO.
   DEF VAR summ-sp      LIKE  op-entry.amt-rub    NO-UNDO.
   DEF VAR saldo_warn   AS   LOG                  NO-UNDO.
   DEF VAR vStatusValue AS   CHAR                 NO-UNDO.
   DEF VAR vNotBlokAcctDb AS LOG                  NO-UNDO. /* �⪫�祭�� �஢�ન */
   DEF VAR vNotBlokAcctCr AS LOG                  NO-UNDO. /* �� ��⠬. */
   DEF VAR mBlTypes       AS CHARACTER            NO-UNDO.
   DEF VAR mBlSum         AS DECIMAL              NO-UNDO.
   DEF VAR mLimSum        AS DECIMAL              NO-UNDO.
   DEF VAR mErrMsg        AS CHARACTER            NO-UNDO.
   DEF VAR mOK            AS LOG INITIAL NO       NO-UNDO.
   DEF VAR mRes           AS LOGICAL              NO-UNDO.
   DEF VAR mPrevStat      AS CHARACTER            NO-UNDO.
   DEF VAR vNoOrdBlkLst   AS CHARACTER            NO-UNDO.
   DEF VAR vIsBudget      AS LOGICAL              NO-UNDO.
   DEF VAR mIsCRDB        AS LOGICAL              NO-UNDO.
   DEF VAR mIsCRD2        AS LOGICAL              NO-UNDO.
   DEF VAR mLstBlk        AS CHARACTER            NO-UNDO.

   DEF VAR vShQtyStr	AS CHARACTER FORMAT "->>>,>>>,>>>,>>>,>>9.9999999".


   DEFINE VARIABLE vNotChkChar AS CHAR NO-UNDO. /* ��� �࠭���樨 + "!" +
                                                   cur-op-trans + "#" +
                                                   ���᮪ ᮡ�⨩, ��� ������
                                                   �� ����᪠���� �஢�ઠ: d_blp,d_bla
                                                   �ਬ�� in-crd!RECID(op-entry)#d_blp */
   DEF BUFFER buf-acct  FOR acct.
   DEF BUFFER buf-acct1 FOR acct.
   DEF BUFFER cacct     FOR acct.
   DEF BUFFER xop       FOR op.
   DEF BUFFER xop-entry FOR op-entry.
   DEF BUFFER xop-kind  FOR op-kind.
   DEF BUFFER code      FOR code.

   DEF VAR choose-debit AS CHAR FORMAT "X(20)"
                           EXTENT 7 INIT ["����९��� � �������",
                                          "�뤠�� �।�� �� �㬬� �������",
                                          "���⠢��� �� ����⥪� 2",
                                          "���⠢��� �� ����⥪� ��",
                                          "�த������",
                                          "�⬥����",
                                          "���⠢��� �� ����⥪� ���⁫��"] NO-UNDO.

   DEF VAR choose-tran AS CHAR FORMAT "X(10)"
                          EXTENT 7 INIT ["��������",
                                         "����",
                                         "981",
                                         "������",
                                         "",
                                         "",
                                         ""]          NO-UNDO.

   DEF VAR choose-credit AS CHAR FORMAT "X(20)"
                            EXTENT 6 INIT ["����९��� �������",
                                           "������� �।��-�������",
                                           "������ � ����⥪� 2",
                                           "������ � ����⥪� ��",
                                           "�த������",
                                           "�⬥����"] NO-UNDO.

   DEF VAR choose-tran-cr AS CHAR FORMAT "X(10)"
                             EXTENT 6 INIT ["�������",
                                            "�������",
                                            "991",
                                            "������",
                                            "",
                                            ""]        NO-UNDO.

   &SCOPED-DEFINE BAD-MENU-CHOICE                              ~
         {&KEY_FUNCTION} ({&LAST_KEY})   EQ "END-ERROR"                ~
      OR INT64 (pick-value)        EQ NUM-ENTRIES (choose-list)  ~
      OR INT64 (pick-value)        EQ 0

   &SCOPED-DEFINE CONT-MENU-CHOICE ENTRY(INT64(pick-value),choose-list) EQ choose-debit[{&ADD_CONT}] OR ~
                                   ENTRY(INT64(pick-value),choose-list) EQ choose-credit[{&ADD_CONT}] 
   pick-value = "".

   IF acct.acct-cat = "d" THEN
      vShQtyStr=STRING(sh-qty).

   FIND op OF op-entry NO-LOCK NO-ERROR.
   {&FillProgressError}
   cur-op-trans = op.op-transaction.

   ASSIGN
      vNotChkChar    = GetSysConf("�⬥����஢��㑮��⨩")
      vNotBlokAcctDb = (GetSysConf("NotCheckBlockAcctDb") EQ "��")
      vNotBlokAcctCr = (GetSysConf("NotCheckBlockAcctCr") EQ "��")
   .

   IF     CAN-DO({&op_kind_no_test_error}, op.op-kind)
      AND CAN-DO({&cod-event_no_test_error}, cod-event) THEN
   DO:
      flager = 0.
      {&RelOp}
      RETURN.
   END.

   IF     vNotChkChar                  NE ""
      AND vNotChkChar                  NE ?
      AND NUM-ENTRIES(vNotChkChar,"#") GE 2
      AND ENTRY(1,vNotChkChar,"#")     EQ op.op-kind + "!" + STRING(RECID(op-entry))
      AND CAN-DO(entry(2,vNotChkChar,"#"),cod-event) THEN
   DO:

      flager = 0.
      {&RelOp}
      RETURN.
   END.
   
    IF FGetSetting("��������", "", "���") NE "��" 
      AND acct.contr-acct NE "" 
      AND (   (    INDEX(cod-event,"ksa") NE 0
               AND acct.side              EQ "�")
           OR (    INDEX(cod-event,"dsp") NE 0
              AND acct.side EQ "�")
           ) THEN
   DO:
      flager = 0.
      {&RelOp}
      RETURN.
   END.

   vStatusValue = GetCodeMisc("�����",op-entry.op-status,2).

/* �� �஢�ન �� ����������� ���⠭���� �� ����⥪� �몫�祭� �஢�ઠ
   �� ���.४. ��ᬮ������ can-do(TmpSt,acct.user-id) -> chk_use.i */

   IF setrassh            NE ""            AND /* ����஫�� �����         */
                                               /* ��⠭�����                 */
      op-entry.op-status  GE setrassh      AND
      (vStatusValue       EQ  ""           OR
       vStatusValue       EQ  "��")        AND /* ��楤�� ᮥ������� ?     */
      CAN-DO(check-event,cod-event)        AND /* ��� �訡�� �室�� � ᯨ᪮�*/
                                               /*"��������"               */
      CAN-DO(check-contract,acct.contract)     /* �����祭�� � ᯨ᪮�       */
                                               /* "��������"              */
   THEN DO: /* �������� �⠭����� �࠭���樨 ? */
      {chktrans.i}
   END.

   flager = 1.

/*----------------------------------------------------------------------------------------------*/
   /* �����஢�� ��⨢���� ��� �� ����⮢����  */
   IF cod-event EQ "d_bla"
      AND NOT vNotBlokAcctDb 
   THEN DO:
      {&OpMess}"core16","","%s=" + ClueAcctCurr(acct.acct,acct.currency) +
                           "%s=" + GetCodeName("acct-status",mAcctStatCode)).
      flager = 2.
      {&RelOp}
      RETURN.
   END.

   /* �����஢�� ���ᨢ���� ��� �� ����⮢����  */
   IF cod-event EQ "d_blp"
      AND NOT vNotBlokAcctDb 
   THEN DO:

      mBlTypes = FGetSetting ("���⁫��", "���끫���", "").

      IF fl-cardinc 
         AND CAN-DO(mAcctContCrd2,acct.contract)
         AND CAN-DO(mAcctMaskCrd2, acct.acct)
         AND acct.acct-cat EQ "b" THEN  
      DO:

         RUN CheckCard2Entry(BUFFER cacct, BUFFER op).

         IF {&RETURN_VALUE} = "" THEN /* �������� �஢���� �� �易�� �      */
         DO:                       /* ��������ᮢ�� ᯨᠭ�� � ����⥪� ?*/
            vNoOrdBlkLst = BlockAcctNoOrderPay(acct.acct + "," + acct.currency,
                                               DATETIME(TODAY, MTIME)).
            IF {assigned mBlTypes} AND CAN-DO(mBlTypes,mAcctStatType) AND ( {assigned mAcctStatCode}
               OR Chk_AcctRec_For_CBLACCT(IF {assigned op.ben-acct} THEN op.ben-acct ELSE op-entry.acct-cr)
                                           OR {assigned vNoOrdBlkLst}) THEN
               {additem.i choose-list choose-debit[{&ADD_CRDB}]}
            ELSE
            DO:

               IF AVAILABLE op THEN
               DO:

                  IF CAN-DO(FGetSetting("�������2", "", "*"), op.doc-type) THEN
                  DO:
                     {additem.i choose-list choose-debit[{&ADD_CRD2}]}
                  END.
               END.
            END.
         END.
      END.

      {additem.i choose-list choose-debit[{&ADD_RJCT}]}    /* �⬥����    */

      pick-value = STRING(NUM-ENTRIES(choose-list)). /* �� 㬮�砭�� - �⬥����*/

        {&OpMess} IF LOOKUP(choose-debit[{&ADD_CRDB}],choose-list) EQ 0 THEN "core17" ELSE "core56",
                             "","%s=" + ClueAcctCurr(acct.acct,acct.currency) +
                             "%s=" + GetCodeName("acct-status",mAcctStatCode)  +
                             "|" + choose-list). 

      IF {&BAD-MENU-CHOICE} 
      THEN DO:
         flager = 2.
         {&RelOp}
         RETURN.
      END.
      ELSE.
      DO: 
         IF {assigned mBlTypes} AND CAN-DO(mBlTypes,mAcctStatType) AND ( {assigned mAcctStatCode}
                                           OR Chk_AcctRec_For_CBLACCT(IF {assigned op.ben-acct} THEN op.ben-acct ELSE op-entry.acct-cr)
                                           OR {assigned vNoOrdBlkLst}) THEN DO:

            RUN SetSysConf IN h_base ("�⬥����஢��㑮��⨩",
                           op.op-kind + "!" + STRING(RECID(op-entry)) + "#" + "d_blp,d_dsp").

            {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */

            ASSIGN
               mRes = TDAddParam ("iAcct", acct.acct) AND TDAddParam ("ibCurrency", acct.currency)
                  AND TDAddParam("iOp",string(op-entry.op)) AND TDAddParam("iRegimAuto","yes") AND
		  TDAddParam("iCurrency","") AND TDAddParam("iOp-entry",string(op-entry.op-entry))
            NO-ERROR.
         
            IF NOT mRes THEN 
               {&OpMess}"","","��� " + acct.acct +  ". �訡�� ��।�� ��ࠬ��஢ � �࠭����� ON_CBLC.").
          
            ELSE
               RUN ex-trans.p ("ON_CBLC", gend-date, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg) NO-ERROR. 

            IF NOT mOK THEN DO: 
               {&OpMess}"","","��� " + acct.acct +   ". �訡��: " + mErrMsg).
               flager = 2.
            END.
            ELSE flager = 0.

            RUN SetSysConf IN h_base ("�⬥����஢��㑮��⨩",vNotChkChar).

         END.
         ELSE DO:
            FIND op-kind WHERE
                 op-kind.op-kind EQ choose-tran[{&ADD_CRD2}] NO-LOCK NO-ERROR.
            {&FillProgressError}

            RUN cli-pos IN h_base (acct.acct,
                                   acct.currency,
                                   gend-date,
                                   gend-date,
                                   "�").
            RUN VALUE(op-kind.proc + ".p")(OUTPUT flager,
                                           op-entry.op-date,
                                           RECID(op-entry),
                                           RECID(acct),
                                           RECID(op-kind)).
         END.
         {&RelOp}
         RETURN.
      END.
   END.

   IF cod-event EQ "d_ksa" THEN              /* �।�⮢�� ᠫ줮 �� ����⮢����           */
   DO:                                       /* ��⨢���� ���                             */
      {additem.i choose-list choose-debit[{&ADD_CONT}]}    /* �த������                        */
      {additem.i choose-list choose-debit[{&ADD_RJCT}]}    /* �⬥����                          */

      str1 = "%d=" + fmt-summ(sh-bal) +
             "%s=" + ClueAcctCurr(acct.acct,acct.currency) +
             "%s=" + ( IF NOT {assigned acct.check-op}
                         THEN ""
                         ELSE "(����஫� - " + acct.check-op + ")")/* +
             "|" + choose-list*/ .

      {chksaldo.i &flager  = 2                   /* ���⪨� ����஫� ᠫ줮                     */
                  &MesCode = "core18"
                  &cont=YES}

      IF {&BAD-MENU-CHOICE} THEN
      DO:
         flager = 2.
         {&RelOp}
         RETURN.
      END.
      ELSE IF {&CONT-MENU-CHOICE} THEN
      DO:
         flager = 0.
         {&RelOp}
         RETURN.
      END.
   END.

   IF cod-event EQ "d_dspr" AND op-entry.op-status LT vAccessStatus OR cod-event EQ "k_dspr" THEN
   DO:
      {additem.i choose-list choose-debit[{&ADD_CONT}]}    /* �த������                        */
      {additem.i choose-list choose-debit[{&ADD_RJCT}]}    /* �⬥����                          */
      {&OpMess}"core54","","%s=" + ClueAcctCurr(acct.acct,acct.currency)).
      IF {&BAD-MENU-CHOICE} THEN
      DO:
         flager = 2.
         {&RelOp}
         RETURN.
      END.
      ELSE IF {&CONT-MENU-CHOICE} THEN
      DO:
         flager = 0.
         {&RelOp}
         RETURN.
      END.
   END.

   IF    cod-event EQ "d_dsp"
      OR cod-event EQ "d_dspb"
      OR cod-event EQ "d_crp" THEN
   DO: /* ���⠭���� �� ����⥪�  � ����९����� c ������� */

      IF     cod-event EQ "d_dspb"
         AND CAN-DO(mAcctContCrd2,acct.contract) THEN
         fl-cardinc = TRUE.
      IF fl-cardinc THEN
      DO:                    /* �� ᯨᠭ�� � ����⥪� ?                   */

         RUN CheckCard2Entry(BUFFER cacct, BUFFER op).

         IF {&RETURN_VALUE} <> "" THEN
         DO:                 /* �������� �஢���� �易�� � ��������ᮢ�� */
            fl-cardinc = NO. /* ᯨᠭ�� � ����⥪� ?                    */
            mes-dop = mes-dop + " �� ᯨᠭ�� ���㬥�� � ����⥪� ".
         END.
         ELSE
         IF fl-o  = 0  THEN
            mes-dop = "".

      END.

CHECK_SALDO1:
      REPEAT WHILE recalc ON ENDKEY UNDO, RETURN
                          ON ERROR  UNDO, RETURN:

         ASSIGN
            choose-list = ""
            trans-list  = ""
            recalc      = FALSE
            mess-saldo  = IF cod-event EQ "d_dspb" THEN
                             "����� ᯨ��� �����஢����� �㬬�.~n"  +
                             "�����஢��� " + fmt-summ(mbl-pos) + " " +
                             ( IF sh-val NE 0 THEN Get_Val_Name(INPUT acct.currency,
                                                               INPUT mbl-pos) + "~n" +
                                                  ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) 
                                                   THEN "����㯭� ���⮪ " + fmt-summ(sh-bal - mbl-pos) ELSE "���⮪ " + fmt-summ(sh-bal))
                                                   + " " +
                                                  Get_Val_Name(INPUT acct.currency,
                                                               INPUT sh-val)
                                              ELSE Get_Val_Name("",
                                                               INPUT mbl-pos) + "~n" +
                                               ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) 
                                                   THEN "����㯭� ���⮪ " ELSE "���⮪ ")
                                                   + fmt-summ(sh-bal) + " " +
                                                  Get_Val_Name("",
                                                               INPUT sh-bal)
                              )
                          ELSE IF lim-pos EQ 0 THEN
                             ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) 
                                 THEN "��᭮� ����㯭�� ᠫ줮 " ELSE "ᠫ줮 ") +
                             ( IF acct.acct-cat = "d" THEN vShQtyStr /* ���� �� ࠡ�⠥� ��� ���� ��⮢ */
                              ELSE IF acct.currency NE ""
                              THEN fmt-summ(sh-val) + " " +
                                   Get_Val_Name(INPUT acct.currency,
                                                INPUT sh-val) + "~n" +
                                                "(" + fmt-summ(sh-bal) + " " +
                                                Get_Val_Name("", INPUT sh-bal) + ")"
                              ELSE fmt-summ(sh-bal) + " " +
                                   Get_Val_Name("", INPUT sh-bal)
                             )
                          ELSE
                             "�ॢ�襭 ����� ���⪠.~n"  +
                             "�����   " + fmt-summ(lim-pos) +
                             ( IF mbl-pos NE 0
                              THEN
                                 "~n�����஢��� " + fmt-summ(mbl-pos) + " "
                              ELSE " " ) +
                             ( IF acct.currency NE ""
                              THEN Get_Val_Name(INPUT acct.currency,
                                                INPUT lim-pos) +
                                   "~n" +
                                   ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) 
                                    THEN "����㯭� ���⮪ " ELSE "���⮪ ")
                                    + fmt-summ(sh-val) + " " +
                                    Get_Val_Name(INPUT acct.currency,
                                                 INPUT sh-val)
                              ELSE Get_Val_Name("",
                                                INPUT lim-pos) + "~n" +
                                   ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) 
                                    THEN "����㯭� ���⮪ " ELSE "���⮪ ")
                                    + fmt-summ(sh-bal) + " " +
                                   Get_Val_Name("",INPUT sh-bal)
                              )
         .
         IF     fl-poddep
            AND cod-event <> "d_crp" THEN
         DO: /* ���� 祬 ����९. � ������� */
            ASSIGN
               shval = sh-val
               shbal = sh-bal
            .
            RUN cli-pos IN h_base (buf-acct.acct,
                                   buf-acct.currency,
                                   gend-date,
                                   gend-date,
                                   "�").
            IF (sh-val GE 0 AND sh-bal GE 0) THEN
               fl-poddep = NO.
            ASSIGN
               sh-val = shval
               sh-bal = shbal
            .
         END.
SUGGEST1:
         DO ON ERROR UNDO, RETRY:
            IF     CAN-DO(mAcctContCrd2,acct.contract)
               AND acct.acct-cat EQ "b"      THEN
            DO:
               IF     fl-poddep
                  AND cod-event <> "d_crp" THEN
               DO:
                  {additem.i choose-list choose-debit[{&ADD_DEPO}]}
                  {additem.i trans-list choose-tran[{&ADD_DEPO}]}
               END.
               IF     fl-ovdr
                  AND cod-event <> "d_crp" THEN
               DO:
                  {additem.i choose-list choose-debit[{&ADD_CRED}]}
                  {additem.i trans-list choose-tran[{&ADD_CRED}]}
               END.
               IF fl-cardinc THEN
               DO:
                  RUN IsBudgetPayment(op.op, OUTPUT vIsBudget).
                  mLstBlk  = BlckAcctOrdPay(acct.acct + "," + acct.currency,DATETIME(gend-date,MTIME), IF vIsBudget THEN "-1" ELSE op.order-pay). 

                  mBlTypes = FGetSetting ("���⁫��", "���끫���", "").
                  
                  vNoOrdBlkLst = BlockAcctNoOrderPay(acct.acct + "," + acct.currency,
                                                     DATETIME(TODAY, MTIME)).
                  IF CAN-DO(mLstBlk,mAcctStatType) OR
                     Chk_AcctRec_For_CBLACCT(IF {assigned op.ben-acct} THEN op.ben-acct ELSE op-entry.acct-cr)  
                  THEN ASSIGN mIsCRDB =     {assigned mBlTypes} AND CAN-DO(mBlTypes,mAcctStatType)
                              mIsCRD2 = NOT {assigned mBlTypes} OR NOT CAN-DO(mBlTypes,mAcctStatType).
                   
                  ELSE ASSIGN mIsCRDB = no
                              mIsCRD2 = yes.


                  IF     (mIsCRDB OR {assigned vNoOrdBlkLst}) 
                     AND op.op-kind NE "_CANLBOT" THEN
                  DO:
                     {additem.i choose-list choose-debit[{&ADD_CRDB}]}
                     {additem.i trans-list choose-tran[{&ADD_CRDB}]}
                  END.
                  ELSE IF mIsCRD2 THEN
                  DO:
                      IF CAN-DO(FGetSetting("�������2", "", "*"), op.doc-type) AND
                         CAN-DO(mAcctMaskCrd2, acct.acct) THEN
                      DO:
                         {additem.i choose-list choose-debit[{&ADD_CRD2}]}
                         {additem.i trans-list choose-tran[{&ADD_CRD2}]}
                      END.
                  END.
                  ELSE.
               END.
            END.

            IF cod-event NE "d_dspb" THEN
            DO:
               {additem.i choose-list choose-debit[{&ADD_CONT}]}
            END.
            {additem.i choose-list choose-debit[{&ADD_RJCT}]}

            IF cod-event = "d_crp" THEN
            DO:
               if fl-cardinc then do:
                  pick-value = STRING(NUM-ENTRIES(choose-list)). /* �� 㬮�砭�� - �⬥����*/
                  {&OpMess}"core20","","%s=" + ClueAcctCurr(acct.acct,acct.currency) +
                                       "%s=" + ( IF vCartName EQ "����2" THEN "2" ELSE ( IF vCartName EQ "���⁫��" THEN "�����஢����� ��⮢" ELSE "")) + 
                                       "|" + choose-list).
               end.
               else pick-value = "1".
                  
               END.
            ELSE 
            DO:
               str1 = "%s=" + ClueAcctCurr(acct.acct,acct.currency) +
                      "%s=" + ( IF NOT {assigned acct.check-op}
                                  THEN ""
                                  ELSE "(����஫� - " + acct.check-op + ")") +
                              mess-saldo  + ' ' +  mes-dop.
               str1 = str1 +
                      ( IF SUBSTRING(TRIM(str1),LENGTH(TRIM(str1))) EQ "."
                       THEN ""
                       ELSE ".") +
                      ( IF GetSysConf("�_������_����_����⥪�") = "��"
                       THEN " �� ������� ��� ���� ���㬥��� �� ����⥪� 2"
                       ELSE IF GetSysConf("�_������_����_���") = "��"
                       THEN " �� ������� ��� ���� ���㬥��� �� ����⥪� �����஢����� ��⮢"
                       ELSE "") +
                      "|" + choose-list.
               IF mIsOverLimit THEN
               DO:
                  IF INDEX(ENTRY(1, str1, "|"),{&mess_no_limit}) <= 0 THEN  
                     ENTRY(1, str1, "|") = ENTRY(1, str1, "|") + " " + {&mess_no_limit}.
               END.    

               IF cod-event = "d_dspb" AND LOOKUP(choose-debit[{&ADD_CRDB}],choose-list) NE 0 THEN DO:
                  IF vNotBlokAcctDb THEN DO:
                     choose-list = choose-debit[{&ADD_CONT}] + "," + choose-list.
                     pick-value = "1".

                  END.
                  ELSE DO:  

                     IF {assigned vAccessMask} AND CAN-DO(vAccessMask,acct.acct) AND
                        {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
                     THEN DO: 
                        IF  {assigned vAccessStatus} AND op-entry.op-status GE vAccessStatus THEN DO:

                           {chksaldo.i &flager  = 2        
                                       &MesCode = "core55"
                                       &cont = NO}  /* �� ������ �뢮������ ��ਠ�� "�த������"*/
                        END.
                        ELSE DO:                       
                           {chksaldo.i &flager  = 2        
                                       &MesCode = "core55"
                                       &cont = YES}  
                        END.            
                     END.
                     ELSE DO:                                                                     

                       {chksaldo.i &flager  = 2        
                                   &MesCode = "core55"}
                     END.
                  END.            
               END.
               ELSE DO: /* ����⥪� 2 */ 
                  IF {assigned vAccessMask} AND CAN-DO(vAccessMask,acct.acct) AND
                     {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
                  THEN DO:

                     IF {assigned vAccessStatus} AND op-entry.op-status GE vAccessStatus  THEN DO:
                    
                        {chksaldo.i &flager  = 2        
                                    &MesCode = "core21"
                                    &cont = NO}  /* �� ������ �뢮������ ��ਠ�� "�த������"*/
                     END.
                     ELSE DO:
                        {chksaldo.i &flager  = 2        
                                    &MesCode = "core21"
                                    &cont = YES}  
                     END.            
                  END.
                  ELSE DO: 

                     {chksaldo.i &flager  = 2            /* ���⪨� ����஫� ᠫ줮 */
                                 &MesCode = "core21"}
                  END.
               END.
                        /* &choose  = choose-list */
            END.

            IF {&BAD-MENU-CHOICE} THEN
            DO:
               flager = 2.
               {&RelOp}
               RETURN.
            END.
            ELSE IF {&CONT-MENU-CHOICE} THEN
            DO:          
               flager = 0.
               {&RelOp}
               RETURN.
            END.
            ELSE
            DO:
               IF  ENTRY(INT64(pick-value),choose-list) EQ choose-debit[{&ADD_CRDB}]  THEN DO:

                  RUN SetSysConf IN h_base ("�⬥����஢��㑮��⨩",
                          op.op-kind + "!" + STRING(RECID(op-entry)) + "#d_dsp," + cod-event).

                  {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
                
                  ASSIGN
                     mRes = TDAddParam ("iAcct", acct.acct) AND TDAddParam ("ibCurrency", acct.currency)
                        AND TDAddParam("iOp",string(op-entry.op)) AND TDAddParam("iRegimAuto","yes") AND
                        TDAddParam("iCurrency","") AND TDAddParam("iOp-entry",string(op-entry.op-entry))
                  NO-ERROR.
                  
                  IF NOT mRes THEN 
                     {&OpMess}"","","��� " + acct.acct +  ". �訡�� ��।�� ��ࠬ��஢ � �࠭����� ON_CBLC.").
                
                  ELSE
                     RUN ex-trans.p ("ON_CBLC", gend-date, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg) NO-ERROR. 

                  RUN SetSysConf IN h_base ("�⬥����஢��㑮��⨩",vNotChkChar).

                  IF NOT mOK THEN DO: 
                     {&OpMess}"","","��� " + acct.acct +   ". �訡��: " + mErrMsg).
                     flager = 2.
                     {&RelOp}
                     RETURN.
                  END.
                  ELSE DO:                  
                     flager = -1.
                     {&RelOp}
                     RETURN.
                  END.
               END.
               ELSE DO:

                  FIND op-kind WHERE
                       op-kind.op-kind EQ ENTRY(INT64(pick-value),trans-list)
                  NO-LOCK NO-ERROR.
                  {&FillProgressError}
                  IF SearchPFile(op-kind.proc) THEN
                  DO:
                     RUN VALUE(op-kind.proc + ".p") (OUTPUT flager,
                                                     op-entry.op-date,
                                                     RECID(op-entry),
                                                     RECID(acct),
                                                     RECID(op-kind)).
                     IF flager NE 0 THEN
                     DO:
                        {&RelOp}
                        RETURN.
                     END.
                     RUN cli-pos IN h_base (acct.acct,
                                            acct.currency,
                                            gend-date,
                                            gend-date,
                                            "�").
                     IF    (acct.currency NE "" AND sh-val GT lim-pos)
                        OR (acct.currency EQ "" AND sh-bal GT lim-pos) THEN
                     DO:
                        recalc = YES.
                        NEXT CHECK_SALDO1.
                     END.
                  END.
                  ELSE
                  DO:
                     {&OpMess}"core22","","%s=" + ENTRY(INT64(pick-value),choose-list)).
                     recalc = YES.
                     NEXT CHECK_SALDO1.
                  END.
               END.
            END.
         END.
      END.
   END.

/*------------------------------------------------- ��ࠡ�⪨ �� �।�⮢���� ��⮢ -----------*/

   /* ��ࠡ�⪠ �� �����஢�� ��� �� �।�⮢���� */
   IF cod-event EQ "k_bl" 
      AND NOT vNotBlokAcctCr
   THEN DO:               
      {&OpMess}"core16","","%s=" + ClueAcctCurr(acct.acct,acct.currency) +
                           "%s=" + GetCodeName("acct-status",mAcctStatCode)).
      flager = 3.
      {&RelOp}
      RETURN.
   END.

   IF cod-event EQ "k_ksa" THEN              /* �।�⮢�� ᠫ줮 �� ��⨢��� ��� �� �।�⮢���� */
   DO:
check_saldo2:
      REPEAT WHILE recalc ON ENDKEY UNDO,RETURN ON ERROR UNDO,RETURN:

         ASSIGN
            choose-list = ""
            trans-list  = ""
            recalc      = FALSE
         .

         IF lim-pos EQ 0 THEN

            mess-saldo = 
            ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) AND
                {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
             THEN "����㯭�� ᠫ줮 " ELSE "ᠫ줮 ") +             
            ( IF acct.acct-cat = "d" THEN
                vShQtyStr
            ELSE
                fmt-summ(sh-bal) + " ��." +
            " ").
         ELSE mess-saldo = "�ॢ�襭 ����� ���⪠.~n"  +
                           "�����   " + fmt-summ(lim-pos) +
                           ( IF mbl-pos NE 0
                            THEN
                               "~n�����஢��� " + fmt-summ(mbl-pos) + " "
                            ELSE " " ) +
                           ( IF acct.currency NE "" 
                            THEN Get_Val_Name(INPUT acct.currency,
                                              INPUT lim-pos) + "~n" +
                           ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) AND
                               {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
                            THEN "����㯭� ���⮪ " ELSE "���⮪ ")
                                  + fmt-summ(sh-val) + " " +                                  
                                 Get_Val_Name(INPUT acct.currency,INPUT sh-val)
                            ELSE Get_Val_Name("",INPUT lim-pos) + "~n" +
                           ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) AND
                               {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
                            THEN "����㯭� ���⮪ " ELSE "���⮪ ")
                                  + fmt-summ(sh-bal)  +  " " + 
                                 Get_Val_Name("",INPUT sh-bal)
                            ).

suggest2:
         DO ON ERROR UNDO, RETRY:
            IF     acct.contract EQ "�����"
               AND acct.acct-cat EQ "b" THEN
            DO:
               IF fl-cardinc THEN
               DO:
                  {additem.i choose-list choose-debit[{&ADD_CRD3}]}
                  {additem.i trans-list choose-tran[{&ADD_CRD3}]}
               END.
            END.
            {additem.i choose-list choose-debit[{&ADD_CONT}]}
            {additem.i choose-list choose-debit[{&ADD_RJCT}]}
            
            str1 = "%s=" + ClueAcctCurr(acct.acct,acct.currency) +
                   "%s=" + ( IF NOT {assigned acct.check-op}
                               THEN ""
                               ELSE "(����஫� - " + acct.check-op + ")") +
                           mess-saldo  + mes-dop +
                   "|" + choose-list.

               IF {assigned vAccessMask} AND CAN-DO(vAccessMask,acct.acct) AND
                  {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
               THEN DO:

                  IF {assigned vAccessStatus} AND op-entry.op-status GE vAccessStatus THEN DO:
                 
                     {chksaldo.i &flager  = 3        
                                 &MesCode = "core23"
                                 &cont = NO}  /* �� ������ �뢮������ ��ਠ�� "�த������"*/
                  END.
                  ELSE DO:
                     {chksaldo.i &flager  = 3        
                                 &MesCode = "core23"
                                 &cont = YES}  
                  END.            
               END.
               ELSE DO: 

                  {chksaldo.i &flager  = 3
                              &MesCode = "core23"}   /* ���⪨� ����஫� ᠫ줮  */
                           /* &choose  = choose-list */ 
               END.
            IF {&BAD-MENU-CHOICE} THEN
            DO:
               flager = 3.
               {&RelOp}
               RETURN.
            END.
            ELSE IF {&CONT-MENU-CHOICE} THEN
            DO:
               flager = 0.
               {&RelOp}
               RETURN.
            END.
            ELSE DO:
               FIND op-kind WHERE op-kind.op-kind EQ ENTRY(INT64(pick-value),trans-list)
                   NO-LOCK NO-ERROR.
               {&FillProgressError}

               IF     AVAIL op-kind
                  AND SEARCH(op-kind.proc + ".r") <> ? 
                  OR  SEARCH(op-kind.proc + ".p") <> ? THEN
               DO:

                  RUN VALUE(op-kind.proc + ".p")(OUTPUT flager,
                                                 op-entry.op-date,
                                                 RECID(op-entry),
                                                 RECID(acct),
                                                 RECID(op-kind)).
                  IF flager NE 0 THEN
                  DO:
                     {&RelOp}
                     RETURN.
                  END.

                  RUN cli-pos IN h_base (acct.acct,acct.currency,gend-date,gend-date,"�").

                  IF    (acct.currency EQ ""  AND sh-bal LT lim-pos)
                     OR (acct.currency NE ""  AND sh-val LT lim-pos)
                     OR (acct.acct-cat EQ "d" AND sh-bal LT lim-pos) THEN
                  DO:
                     recalc = YES.
                     NEXT check_saldo2.
                  END.

               END.
               ELSE DO:
                  {&OpMess}"core22","","%s=" + ENTRY(INT64(pick-value),choose-list)).
                /*"��� ����樨 - '" + ENTRY(INT64(pick-value),choose-list) + "'" 
                  "�� ������� ��楤�� �⠭���⭮� �࠭���樨 !" */
                  
                  recalc = YES.
                  NEXT check_saldo2.
               END.
            END.             /* if {&BAD-MENU-CHOICE} then do: � �� ��稥 IF'�                */
         END.                /* do on error undo, retry:                                        */
      END.                   /* repeat while recalc on endkey undo,RETURN on error undo,RETURN: */
   END.


   IF cod-event EQ "k_dsp" THEN             /* ����⮢�� ᠫ줮 ���ᨢ���� ��� �।��   */
   DO:
      mPrevStat = GetSysConf("�।�����").

      IF lim-pos EQ 0 THEN
         mess-saldo  = ( IF {assigned vAccessMask} AND CAN-DO(vAccessMask,Acct.acct) AND
                       {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
                        AND Acct.side = "�" AND {assigned mPrevStat} AND mPrevStat > op-entry.op-status    /* ��������� ����� �।������ ���ᨢ�� ��� ����権 */
                        THEN "����㯭�� " ELSE "") + 
                       "ᠫ줮 " + 
                       ( IF sh-val NE 0 THEN fmt-summ(sh-val) + " " + 
                                            Get_Val_Name(INPUT acct.currency, 
                                                         INPUT sh-val) + "~n" +
                                            "(" + fmt-summ(sh-bal) + " " +
                                            Get_Val_Name("", INPUT sh-bal) + ")"
                                       ELSE IF acct.acct-cat = "d" THEN vShQtyStr
                                       ELSE fmt-summ(sh-bal) + " " +
                                            Get_Val_Name("", INPUT sh-bal)
                       ).
      ELSE mess-saldo = "�ॢ�襭 ����� ���⪠.~n"  +
                        "�����   " + fmt-summ(lim-pos) +
                        ( IF mbl-pos NE 0
                         THEN
                            "~n�����஢��� " + fmt-summ(mbl-pos) + " "
                         ELSE " " ) +
                        ( IF acct.currency NE "" 
                         THEN Get_Val_Name(INPUT acct.currency,
                                           INPUT lim-pos) + "~n" +
                                           "���⮪ " + fmt-summ(sh-val) + " " +
                              Get_Val_Name(INPUT acct.currency,
                                           INPUT sh-val)
                         ELSE Get_Val_Name("",INPUT lim-pos) + "~n" +
                              "���⮪ " + fmt-summ(sh-bal) + " " +
                              Get_Val_Name("",INPUT sh-bal)
                        ).

      {additem.i choose-list choose-debit[{&ADD_CONT}]}
      {additem.i choose-list choose-debit[{&ADD_RJCT}]}
      
      str1 = "%s=" + ClueAcctCurr(acct.acct,acct.currency) +
             "%s=" + ( IF NOT {assigned acct.check-op}
                      THEN ""
                      ELSE "(����஫� - " + acct.check-op + ")") +
                     mess-saldo + 
             "|" + choose-list.

      IF {assigned vAccessStatus} AND op-entry.op-status LT vAccessStatus AND
         {assigned vAccessMask}   AND CAN-DO(vAccessMask,Acct.acct) AND           
         {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,acct.contract)
     AND Acct.side = "�" AND {assigned mPrevStat} AND mPrevStat > op-entry.op-status     
         AND mPrevStat GE vAccessStatus
          THEN DO:      
         {chksaldo.i &flager  = 2        
                     &MesCode = "core21"
                     &cont = NO}  /* �� ������ �뢮������ ��ਠ�� "�த������"*/
      END.
      ELSE DO: 
      {chksaldo.i &flager  = 3
                  &MesCode = "core21"
                  &cont=YES
      }         /* ���⪨� ����஫� ᠫ줮 */
      END.
                  
               /* &choose = choose-list */

      IF {&BAD-MENU-CHOICE} THEN
      DO:
         flager = 3.
         {&RelOp}
         RETURN.
      END.
      ELSE IF {&CONT-MENU-CHOICE} THEN
      DO:
         flager = 0.
         {&RelOp}
         RETURN.
      END.
   END.


/*------------------- ����९����� �������,������ �।�� �� �������,ᯨᠭ�� � ����⥪�.*/
   IF cod-event EQ "k_ksp" THEN              /* �।�⮢�� ᠫ줮 ���ᨢ���� ��� � �।��*/
   DO:
      IF fl-cardinc THEN
      DO:
         IF AVAIL cacct THEN
         DO:
            fl-cardinc = YES.
            RELEASE xop.
         END.
         ELSE
            fl-cardinc = NO.
      END.
      IF fl-poddep THEN
      DO:
         FIND FIRST xop WHERE xop.op-transaction EQ op.op-transaction
                          AND xop.acct-cat       EQ "b"
                          AND CAN-FIND(FIRST op-entry 
                                          WHERE op-entry.op       EQ xop.op
                                            AND op-entry.acct-cr  EQ buf-acct.acct
                                            AND op-entry.currency EQ buf-acct.currency)
               NO-LOCK NO-ERROR.
         IF AVAIL xop THEN fl-poddep = NO.
         RELEASE xop.
      END.
      IF fl-ovdr THEN
      DO:
         FIND FIRST xop WHERE xop.op-transaction EQ op.op-transaction
                          AND xop.acct-cat       EQ "b"
                          AND CAN-FIND(FIRST op-entry 
                                          WHERE op-entry.op       EQ xop.op
                                            AND op-entry.acct-cr  EQ buf-acct1.acct
                                            AND op-entry.currency EQ buf-acct1.currency)
               NO-LOCK NO-ERROR.
         IF AVAIL xop THEN fl-ovdr = no.
         RELEASE xop.
      END.
      IF NOT fl-poddep AND NOT fl-ovdr AND NOT fl-cardinc THEN
      DO:
         flager = 0.
         {&RelOp}
         RETURN.
      END.

check_saldo3:
      REPEAT WHILE recalc ON ENDKEY UNDO,RETURN ON ERROR UNDO,RETURN:

         ASSIGN
            choose-list = ""
            trans-list  = ""
            recalc      = FALSE
         .

         IF fl-poddep THEN
         DO:
            {additem.i choose-list choose-credit[{&ADD_DEPO}]}
            {additem.i trans-list choose-tran-cr[{&ADD_DEPO}]}
         END.
         IF fl-ovdr THEN
         DO:
            shval = sh-val.
            shbal = sh-bal.
            RUN cli-pos IN h_base (buf-acct1.acct,buf-acct1.currency,gend-date,gend-date,"�").
            IF sh-bal LE 0 AND sh-val LE 0 THEN
               fl-ovdr = no.
            sh-val = shval .
            sh-bal = shbal .
            IF fl-ovdr THEN
            DO :
                fl-ovdr = NO.
                {&OpMess}"core24","","").
            END.
         END.
         IF fl-cardinc THEN
         DO:
            shval = sh-val.
            shbal = sh-bal.
            RUN cli-pos IN h_base (cacct.acct,cacct.currency,gend-date,gend-date,"�").
            IF sh-bal LE 0 AND sh-val LE 0 THEN
               fl-cardinc = no.
            sh-val = shval .
            sh-bal = shbal .
            mBlSum = 0.
            mLimSum = GetLimitPosition(BUFFER Acct, gend-date).  
            IF mBlSum + mLimSum - sh-bal LE 0.0 THEN fl-cardinc = NO.

            IF fl-cardinc THEN
            DO:

               IF AVAILABLE op THEN
               DO:

                  IF CAN-DO(FGetSetting("�������2", "", "*"), op.doc-type) THEN
                  DO:
                     {additem.i choose-list choose-credit[{&ADD_CRD2}]}
                     {additem.i trans-list choose-tran-cr[{&ADD_CRD2}]}
                  END.
               END.
            END.
         END.
         IF NOT fl-poddep AND NOT fl-ovdr AND NOT fl-cardinc THEN
         DO:
            flager = 0.
            {&RelOp}
            RETURN.
         END.
         {additem.i choose-list choose-debit[{&ADD_CONT}]}
         {additem.i choose-list choose-debit[{&ADD_RJCT}]}

         {&OpMess}"core25","","%s=" + ClueAcctCurr(acct.acct,acct.currency) +
                              "%s=" + ( IF NOT {assigned acct.check-op}
                                          THEN ""
                                          ELSE "(����஫� - " + acct.check-op + ")") +
                              "|" + choose-list ).

         IF {&BAD-MENU-CHOICE} THEN
         DO:
            flager = 3.
            {&RelOp}
            RETURN.
         END.
         ELSE IF {&CONT-MENU-CHOICE} THEN
         DO:
            flager = 0.
            {&RelOp}
            RETURN.
         END.
         ELSE DO:
            FIND op-kind WHERE op-kind.op-kind EQ ENTRY(INT64(pick-value),trans-list)
               NO-LOCK NO-ERROR.
            {&FillProgressError}
            IF    SEARCH(op-kind.proc + ".r") <> ?
               OR SEARCH(op-kind.proc + ".p") <> ? THEN
            DO:

               RUN VALUE(op-kind.proc + ".p")(OUTPUT flager,
                                              op-entry.op-date,
                                              RECID(op-entry),
                                              RECID(acct),
                                              RECID(op-kind)).
               IF flager NE 0 THEN
               DO: 
                  {&RelOp} 
                  RETURN. 
               END.

               RUN cli-pos IN h_base (acct.acct,acct.currency,gend-date,gend-date,"�").

               IF    (acct.currency NE "" AND sh-val LT lim-pos)
                  OR (acct.currency EQ "" AND sh-bal LT lim-pos) THEN
               DO:
                  recalc = YES.
                  NEXT check_saldo3.
               END.
            END.
            ELSE DO:
               {&OpMess}"core22","","%s=" + ENTRY(INT64(pick-value),choose-list)).

               recalc = YES.
               NEXT check_saldo3.
            END.
         END.
      END.
   END.

   flager = 0.
   {&RelOp}
END.


/*------------------------------------------------------------------------------
  Purpose:     �����誠 ��� ����� �맮���. �믮���� �஢��� ������ �஢����
               ���㬥��
  Parameters:  rid         - RECID op (���㬥��)
               OFcur-bal   - �� �ᯮ������
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Check-Op-balance:
   DEF INPUT PARAM rid        AS RECID       NO-UNDO.
   DEF INPUT PARAM OFcur-bal  AS CHAR        NO-UNDO.

   DEFINE VARIABLE vOInfo AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCode  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vArgs  AS CHARACTER  NO-UNDO.

   FIND FIRST op WHERE RECID(op) = rid NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN RETURN "error".

   RUN CheckBalance (op.op,NOT type-bal,YES,OUTPUT vOInfo).
   IF {assigned RETURN-VALUE} THEN
   DO:
      vArgs = {&RETURN_VALUE}.
      vCode = TRIM(ENTRY(1,vArgs,"("),"_ ").
      ENTRY(1,vArgs,"(") = "".
      vArgs = TRIM(vArgs,"() ").
      RUN Fill-SysMes IN h_tmess ("",vCode,"",vArgs).
      RETURN "error".
   END.
   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �����誠 ��� ����� �맮���. �맮� �������� ���㬥��
  Parameters:  rid         - RECID op (���㬥��) 
               i871        - �� �ᯮ������ 
               OffKNF      - �� �ᯮ������ 
               OFcur-bal   - �� �ᯮ������ 
               chkupd      - ���祭�� ���. ��ࠬ��� ��� ��।�� ��⮤� chkupd 
               iOff-Vo     - ���祭�� "" ����砥� �஢��� ����⭮�� ����஫�
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Check-Op:
   DEFINE INPUT PARAMETER rid       AS RECID     NO-UNDO.
   DEFINE INPUT PARAMETER i871      AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER OffKNF    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER OFcur-bal AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER chkupd    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iOff-Vo   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vParams AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vValues AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOInfo  AS CHARACTER  NO-UNDO.

   FIND op WHERE RECID(op) = rid NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN RETURN "error".

   /* Commented by KSV: �몫�砥� �஢��� ४����⮢ �� ����奬� */
   RUN SetLstParam IN h_strng ({&VP_ATTR_CHECK} + op.Class-Code,"NO",
                               INPUT-OUTPUT vParams,
                               INPUT-OUTPUT vValues,
                               {&VP_PARAM_DLM}).
   /* Commented by KSV: �몫�砥� �஢��� �஢���� */
   RUN SetLstParam IN h_strng ({&VP_LINK_CHECK} + op.Class-Code,"NO",
                               INPUT-OUTPUT vParams,
                               INPUT-OUTPUT vValues,
                               {&VP_PARAM_DLM}).
   /* Commented by KSV: ��।��� �������⥫�� ��ࠬ��� ��� ��⮤� CHKUPD */
   RUN SetLstParam IN h_strng ({&VP_EXT_PARAM} + op.Class-Code,chkupd,
                               INPUT-OUTPUT vParams,
                               INPUT-OUTPUT vValues,
                               {&VP_PARAM_DLM}).
   /* Commented by KSV: �᫨ ������, �몫�砥� �஢��� ����⭮�� ����஫� */
   IF iOff-Vo <> "" THEN
      RUN SetLstParam IN h_strng ({&VP_VO_CONTROL},"NO",
                                  INPUT-OUTPUT vParams,
                                  INPUT-OUTPUT vValues,
                                  {&VP_PARAM_DLM}).
   /* Commented by KSV: �������㥬 ���㬥�� */
   RUN ValidateObject IN h_valid (op.Class-Code,?,?,op.op,?,?,vParams,vValues,
                                  OUTPUT vOInfo).
   /* Commented by KSV: ������ �訡�� */
   IF {assigned RETURN-VALUE} THEN
      RETURN "error".

   RETURN .
END PROCEDURE.

/*-----------------------------------------------------------------------------------------------*/
/*                                                                                               */
/*-----------------------------------------------------------------------------------------------*/
PROCEDURE MessageEmptyAcct:
   DEFINE INPUT  PARAMETER ipDocNum  AS CHAR     NO-UNDO.
   DEFINE INPUT  PARAMETER ipOpEntry AS INT64  NO-UNDO.
   DEFINE INPUT  PARAMETER ipSide    AS CHAR     NO-UNDO.
   DEFINE OUTPUT PARAMETER flager    AS INT64  NO-UNDO.

   {&OpMess}"core26","","%s=" + ipDocNum + 
                        "%i=" + STRING(ipOpEntry) +
                        "%s=" + ipSide).
                
   /*IF gLogMessage NE YES THEN
      MESSAGE COLOR ERROR
              "� ���㬥�� �����"       ipDocNum
              "�஢���� �����"          ipOpEntry
              "����� ���⮩ ��� �� " + ipSide + "." SKIP
              VIEW-AS ALERT-BOX ERROR.
   ELSE
      RUN LogMessage IN gLogProc ("� ���㬥�� ����� " + ipDocNum +
                                  "�஢���� �����"     + STRING(ipOpEntry) +
                                  "����� ���⮩ ��� �� " + ipSide + ".",
                                  "","no",OUTPUT flager) NO-ERROR.*/
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*                 ��楤�� �஢�ન �����⨬��� �ᯮ�짮����� �����        */
/*----------------------------------------------------------------------------*/
FUNCTION CheckBankReal RETURN CHAR (BUFFER chk-bank FOR banks):
   IF NOT AVAIL chk-bank THEN RETURN "".

   DEFINE var bank-real AS CHAR     NO-UNDO.

   bank-real = GetXAttrValue("banks",STRING(chk-bank.bank-id),"real").
   
   RETURN GetCodeMisc("bnk-real",bank-real,3).

END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                 ��楤�� �஢�ન �����⨬��� �ᯮ�짮����� �����        */
/*----------------------------------------------------------------------------*/
PROCEDURE Check-Bank:
   DEFINE        PARAMETER BUFFER chk-bank   FOR banks.
   DEFINE INPUT  PARAMETER        msg-mode   AS  LOGICAL         NO-UNDO.
   DEFINE OUTPUT PARAMETER        flag-error AS  INT64 INIT -1 NO-UNDO.

   DEFINE VAR bank-dis  AS LOGICAL        NO-UNDO.
   DEFINE VAR bank-real AS CHAR           NO-UNDO.
   DEFINE VAR real-name AS CHAR           NO-UNDO.
   DEFINE VAR vDay      AS INT64        NO-UNDO.
   DEFINE VAR vDateChk  AS DATE  INIT ?   NO-UNDO.
   DEFINE VAR vText     AS CHAR           NO-UNDO.
   DEFINE VAR vSurr     AS CHAR           NO-UNDO.

   IF NOT AVAILABLE(chk-bank) THEN 
   DO:
      IF msg-mode THEN 
         {&OpMess}"core35","","").
         /* "���� �� ������� !" */

         /*IF gLogMessage <> YES THEN DO:
            BELL.
            MESSAGE "���� �� ������� !".
         END.
         ELSE
            RUN LogMessage IN gLogProc("���� �� ������� !",
                                       "","NO",OUTPUT flag-error) NO-ERROR.*/
      flag-error = 10.
   END.
   ELSE DO:
      vSurr = string(chk-bank.bank-id).
      bank-real = GetXAttrValue("banks",vSurr,"real").

      ASSIGN
         bank-dis  = GetCodeMisc("bnk-real",bank-real,2) EQ "��"
         vDay      = INT64(GetCodeMisc("bnk-real",bank-real,4))
         vDateChk  = DATE(GetXAttrValue("banks",
                                        vSurr,
                                        "��⠊���஫�"))
      NO-ERROR.
      
      IF    bank-dis 
         OR (    vDateChk NE ?  
             AND vDateChk LE gend-date + vDay) THEN /* ��� ᪮� �������        */
      DO:

         flag-error = 14.                        /* ���� REAL �� ���⮥       */

         real-name = GetCodeName("bnk-real",bank-real).
         IF NOT {assigned real-name} THEN
            real-name = "��� �ਧ���� ~"" + bank-real + "~".".

         /*vText = "���� ~"" + chk-bank.name + "~"~n"                              +
                 "����� �ਧ��� ��࠭�祭�� ����� � ���������᪨� �����.~n" +
                 real-name                                                       +
                 IF vDay NE 0 AND vDateChk NE ? THEN
                     "~n��� ����஫�: " + STRING(vDateChk,"99/99/9999")     +
                     "~n��� �॥�����: " + GetXAttrValue("banks",
                                                         vSurr,
                                                         "�॥����")
                 ELSE "".*/
         
         {&OpMess}"core36","","%s=" + chk-bank.NAME + 
                              "%s=" + real-name +
                                    IF vDateChk NE ? THEN
                                       "~n��� ����஫�: " + STRING(vDateChk,"99/99/9999") +
                                       "~n��� �॥�����: " + GetXAttrValue("banks",
                                                                           vSurr,
                                                                           "�॥����")
                                    ELSE "" ).
         IF (pick-value = "yes") THEN
               flag-error = 0.
         /*
         IF gLogMessage <> YES THEN
         DO:
            MESSAGE vText                                SKIP
                    "�த������ ���� ?"                  SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    SET vChoice AS LOGICAL.
            IF vChoice THEN flag-error = 0.
         END.
         ELSE DO:
            RUN LogMessage IN gLogProc (vText,
                                        "",
                                        "YES",
                                        OUTPUT flag-error) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN flag-error = 0.
         END.*/

         RETURN.
      END.

      flag-error = 0.
   END.                                          /* if not available(chk-bank)*/

END PROCEDURE.
/*-----------------------------------------------------------------------------------------------*/
/*        ���ଠ樮���� ��楤�� �஢�ન �����⨬��� �ᯮ�짮����� �����                     */
/*-----------------------------------------------------------------------------------------------*/

PROCEDURE Check-BankInf:
   DEFINE PARAMETER BUFFER chk-bank   FOR banks.
   DEFINE VAR              flag-error AS  INT64 NO-UNDO.

   RUN Check-Bank (BUFFER chk-bank, yes, OUTPUT flag-error).

END PROCEDURE.

/*-----------------------------------------------------------------------------------------------*/
/*                   ��楤�� ��ࠡ�⪨ �㡠����⨪� �� �஢����                                */
/*-----------------------------------------------------------------------------------------------*/
PROCEDURE Kau-Trigger:
   DEFINE INPUT  PARAMETER in-oprid     AS RECID   NO-UNDO.
   DEFINE OUTPUT PARAMETER flag-error   AS INT64 NO-UNDO.
   DEFINE INPUT  PARAMETER mess-mode    AS LOGICAL NO-UNDO.

   DEFINE VARIABLE vErrCode AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrInfo AS CHARACTER   NO-UNDO.

   RUN Run_KauTrigger IN h_kau (in-oprid, "������", OUTPUT vErrStr).

   ASSIGN
      vErrCode = ENTRY (1, vErrStr)
      vErrInfo = ENTRY (2, vErrStr)
      NO-ERROR.

   CASE vErrCode:
      WHEN "" THEN
         flag-error = 0.
      WHEN "kau01" THEN
         flag-error = 3.
      WHEN "kau02" THEN
         flag-error = 2.
      WHEN "kau03" THEN
         flag-error = 1.
      WHEN "kau04" THEN
         flag-error = 4.
      WHEN "kau05" THEN
         flag-error = 5.
      WHEN "kau06" THEN
         flag-error = 6.
      OTHERWISE
      DO:
         flag-error = -1.
         vErrCode   = "kau07".
      END.
   END CASE.

   IF mess-mode AND (flag-error NE 0) THEN
      RUN Fill-SysMes IN h_tmess ("", vErrCode, "", vErrInfo).

   pick-value = STRING (flag-error EQ 0).

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                      ��楤�� 㤠����� �஢����                           */
/*----------------------------------------------------------------------------*/
PROCEDURE Check-Entry-Delete:
   DEFINE OUTPUT PARAMETER        flager     AS  INT64 NO-UNDO.
   DEFINE        PARAMETER BUFFER bop-entry  FOR op-entry.
   DEFINE INPUT  PARAMETER        in-op-date AS  DATE    NO-UNDO.

   DEF BUFFER dacct FOR acct.
   DEF BUFFER cacct FOR acct.
   DEF VAR summ-t AS DECIMAL INIT 0 NO-UNDO.

   DEFINE VARIABLE vAcctStat  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlckRecid AS RECID     NO-UNDO.

   flager = 1.
   {op-endel.i &bentry=bop-entry}
   flager = 0.

END.
/*---------------------------------------------------------------------------*/
/*                      ��楤�� �����஢���� ᮮ�饭�� �� �訡���          */
/*---------------------------------------------------------------------------*/
/*PROCEDURE LogMessage :
   DEF INPUT  PARAMETER iMess AS CHAR NO-UNDO. /* ᮮ�饭�� �� �訡�� */
   DEF INPUT  PARAMETER iEvnt AS CHAR NO-UNDO. /* ᮡ�⨥ �맢��襥 �訡�� */
   DEF INPUT  PARAMETER iCont AS CHAR NO-UNDO. /* ����������� �த������� ��ࠡ�⪨
                                              (���筮��� �訡�� */
   DEF OUTPUT PARAMETER opOk  AS INT64  NO-UNDO. /* 䫠� �訡�� � ������ ��楤��
                                              �� ������� */

   DEF VAR err-action AS CHAR NO-UNDO.

   OUTPUT STREAM o-err TO "./open-err.log" APPEND.

   REPLACE ("pmess", "|", " ").

   IF ENTRY(1, iCont) = "no" THEN
   DO: /* ����᪠� �訡�� */

      PUT STREAM o-err UNFORMATTED
         "����᪠� �訡��: " iMess " ����⢨�: �⬥���� " SKIP.

      OUTPUT STREAM o-err CLOSE.
      RETURN.

   END.
   ELSE IF ENTRY(1, iCont) = "yes" THEN
   DO:
      opOk = 0. /* �訡�� �� ����᪠� ����� �த������ */
      IF NUM-ENTRIES(iCont) > 1 THEN
      DO: /* ���� ᯨ᮪ ����⢨� �� �訡�� */

         IF     iEvnt = ""
            AND CAN-DO(iCont, "�த������") THEN
         DO: /* ��ਠ��� ⮫쪮 �த������ ��� �⬥���� */

            PUT STREAM o-err UNFORMATTED
               "�訡��: " iMess " ����⢨�: �த������ " SKIP.

            pick-value = "1".

            OUTPUT STREAM o-err CLOSE.
            RETURN.

         END.
         IF iEvnt <> "" THEN
         DO:

            err-action = GetSysConf(iEvnt) .

            /*���� - ��������� ���祭��*/
            IF    err-action = ?
               OR NOT CAN-DO(iCont, err-action) THEN
               err-action = "�த������".

            PUT STREAM o-err UNFORMATTED
                  "�訡��: " iMess " ����⢨� " err-action SKIP.

            pick-value = STRING(LOOKUP(err-action, iCont) - 1).

            OUTPUT STREAM o-err CLOSE.
            RETURN.

         END.
      END.
      ELSE DO:

         PUT STREAM o-err  UNFORMATTED
            "�訡��: " iMess " ����⢨�: �த������ " SKIP.

         pick-value = "1".

         OUTPUT STREAM o-err CLOSE.
         RETURN.

      END.
   END.

END PROCEDURE. /* logmessage */
*/
/*
  ��楤�� �஢�ન ������ �஢���� ᯨᠭ�� � ����⥪�2 � ���㬥��
  ᮧ�������� ⥪�饩 �࠭���樥�
*/
PROCEDURE CheckCard2Entry.

   DEF PARAMETER BUFFER cacct FOR acct. /*����� ��� �� ����⥪�2*/
   DEF PARAMETER BUFFER op    FOR op.   /*����� ���㬥��*/

   DEFINE VARIABLE vAmtRub AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vAmtCur AS DECIMAL NO-UNDO.

   DEF BUFFER xop FOR op.
find first op-entry of op no-lock no-error.
   IF AVAILABLE op-entry THEN
      ASSIGN
         vAmtRub = op-entry.amt-rub
         vAmtCur = op-entry.amt-cur
      .

   IF NOT AVAIL cacct THEN RETURN.

   FIND FIRST xop WHERE
              xop.op-transaction = op.op-transaction
          AND CAN-FIND(FIRST op-entry WHERE
                             op-entry.op       = xop.op
                         AND op-entry.acct-cr  = cacct.acct
                         AND op-entry.currency = cacct.currency
                         AND (op-entry.amt-rub = vAmtRub OR vAmtRub EQ 0)
                         AND (op-entry.amt-cur = vAmtCur OR vAmtCur EQ 0)
                      )
   NO-LOCK NO-ERROR.

   IF AVAILABLE xop AND AVAILABLE op-entry THEN
   DO:

      FIND FIRST xop WHERE xop.op-transaction EQ op.op-transaction
                       AND xop.op-template    LT op.op-template
                       AND xop.acct-cat       EQ "b"
                       AND CAN-FIND(FIRST op-entry WHERE op-entry.op       EQ xop.op
                                                     AND op-entry.currency EQ cacct.currency
                                                     AND op-entry.amt-rub  EQ vAmtRub
                                                     AND op-entry.amt-cur  EQ vAmtCur
                                   )
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE xop THEN
         RETURN "exist op-entry". /* �������� �஢���� �易�� �          */
   END.

   RETURN.

END PROCEDURE. /* CheckCard2Entry */

/*************************************************************************************************/

/*������ ����� ��᫥ 䨭�⪠��*/
PROCEDURE UpdStatusAfterFin:
   DEF INPUT  PARAM ipRecOp    AS RECID NO-UNDO.
   DEF INPUT  PARAM ipOpStatus AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM opError    AS CHAR  NO-UNDO.

   DEF VAR vStDenia AS CHAR NO-UNDO.
   DEF VAR rightass AS CHAR NO-UNDO.

   DEF BUFFER op FOR op.
   
   IF NOT {assigned ipOpStatus} THEN
      ipOpStatus = "�".

   ASSIGN 
      vStDenia = FGetSetting("st-denial",?,ipOpStatus)
      rightass = getThisUserXAttrValue('�������')
   .
   
   IF vStDenia NE "" THEN
   bl:
   DO:
      FIND FIRST op WHERE RECID(op) EQ ipRecOp EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL op THEN
      DO:
         IF NOT CAN-DO(op.op-error,"m300") THEN DO:
            {additem.i "op.op-error" ""m300""}
         END.
         IF vStDenia <> op.op-status THEN
         DO:
            RUN CheckOpRight IN h_base(RECID(op),?,"ChgSts") NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               IF {&RETURN_VALUE} NE "" THEN
                  opError = {&RETURN_VALUE}.
               LEAVE bl.
            END.

            IF op.user-id EQ USERID("bisquit") THEN
               rightass = GetxattrValueEx("_user",USERID("bisquit"),'������ᢂ���',rightass).

            IF NOT CAN-DO(rightass,vStDenia) THEN DO:
               opError =  "� ��� ��� �ࠢ ��᢮��� ����� " + vStDenia + " !".
               LEAVE bl.
            END.
            
            RUN chst-op.p (RECID(op), vStDenia) NO-ERROR.

            IF ERROR-STATUS:ERROR OR
               {&RETURN_VALUE} <> "" THEN
               opError = IF {&RETURN_VALUE} NE ""
                            THEN {&RETURN_VALUE}
                         ELSE
                            ERROR-STATUS:GET-MESSAGE(ERROR-STATUS:NUM-MESSAGES).
         END.
      END.
      ELSE opError = "���㬥�� �ᯮ������ ��㣨� ���짮��⥫�� !".
   END.
END PROCEDURE.

/*************************************************************************************************/

/* �����㬥�� ��� ��।������ ४����⮢ ���㬥�� �� �����. 117
�� ����� op ��।���� �������⭮�����樨 � ��ᯮ�⑤���� 
*/
PROCEDURE pGet117Reqv:
/*   DEF PARAMETER BUFFER op     FOR op.   /*����� ���㬥��*/ */
   DEF INPUT  PARAM in-opop    AS INT64   NO-UNDO.
   DEF OUTPUT PARAM opKodVO    AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM opKodPS    AS CHAR  NO-UNDO.

   DEF BUFFER op FOR op.

   DEF VAR vI           AS INT64  NO-UNDO.
   DEF VAR vI1          AS INT64  NO-UNDO.
   DEF VAR vI2          AS INT64  NO-UNDO.

   ASSIGN
      opKodVO = ""
      opKodPS = ""
   .
   FIND FIRST op WHERE op.op EQ in-opop NO-LOCK NO-ERROR. 
   IF NOT AVAIL op THEN
      RETURN.

   ASSIGN
      opKodVO = GetXAttrValueEx("op",
                                STRING(op.op),
                                "��������117",
                                "")
      opKodPS = GetXAttrValueEx("op",
                                  STRING(op.op),
                                  "��ᯮ�⑤����",
                                  "")
   .
   vI      = INDEX(op.details,"~{VO") + 3.
   IF vI GT 3 THEN
   DO:
      ASSIGN
         vI1 = INDEX(op.details,"PS",vI)
         vI2 = INDEX(op.details,"}",vI)
      .
      IF vI1 EQ 0 THEN
         vI1 = vI2.
      IF     vI2 GT 0
         AND vI2 GT vI1 THEN
         opKodPS = SUBSTRING(op.details,vI1 + 2,vI2 - vI1 - 2).
      IF vI1 GT 0 THEN
         opKodVO = SUBSTRING(op.details,vI,vI1 - vI).
   END.
END PROCEDURE.

/********************************************************************
 ���� �����㬥�� ��� ��।������ ४����⮢ ���㬥�� �� �����. 117
�� ����� op ��।���� �������⭮�����樨 � ��ᯮ�⑤���� � ��. 
� ��।������ TEMP-TABLE, HANDLE �� ������ �㤥� ����饭
��� ������� ���㬥�⮢ TEMP-TABLE ����� ᮤ�ঠ�� ��᪮�쪮 ����ᥩ �
ࠧ�묨 ������ �����117. ����⠥� ��� � ����, ⠪ � � ���� �ଠ⮬
४����� ��������117. ��楤�� �����頥� "0" �᫨ ���祭�� �������.
��� �㡫���� ���㬥�⮢ �������⭮�����樨 � ��ᯮ�⑤���� ��४�� �� 
���ᠭ�� ���㬥�� {VO...[PS...]}
*********************************************************************/

DEF TEMP-TABLE tt-kod117 NO-UNDO
   FIELD KodVO AS CHAR    /*��⥣���*/
   FIELD KodPS AS CHAR    /*��� ��� ����*/
   FIELD op-su AS DECIMAL /*�㬬� � ����� �� �஢����/��ࢮ� ����஢����*/
   FIELD VC-su AS DECIMAL /*�㬬� � ����� ����ࠪ�*/
   FIELD VC-cu AS CHAR    /*����� ����ࠪ�*/
   FIELD op2su AS DECIMAL /*�㬬� � ����� �� �஢����/��ன ����஢����*/
   FIELD op-db AS INT64 /*� �஢���� �����*/
   FIELD op-cr AS INT64 /*� �஢���� �।��*/
   FIELD db-cr AS CHAR    /*�� ������ ��� �।��� �뫮 ࠧ������*/
   FIELD num-cont  AS CHAR /*����� ����ࠪ�*/ 
   FIELD date-cont AS DATE /*��� ����ࠪ�*/
.

PROCEDURE pGet117ReqN:
   DEF INPUT  PARAM in-opop    AS INT64    NO-UNDO.
   DEF OUTPUT PARAM itt-handle AS HANDLE NO-UNDO.
   
   DEF VAR vtt-handle AS HANDLE NO-UNDO.
   DEF VAR mBalExs    AS CHARACTER NO-UNDO.

   mBalExs   = FGetSetting("�믨᪨","��⠊�","").
   itt-handle = TEMP-TABLE tt-kod117:HANDLE.

   vtt-handle = itt-handle:DEFAULT-BUFFER-HANDLE.
   vtt-handle:EMPTY-TEMP-TABLE().
  
   DEF BUFFER op FOR op.
   DEF BUFFER tmpop-entry FOR op-entry.
   
   DEF VAR vnofig       AS LOG  INITIAL FALSE NO-UNDO.
   DEF VAR vI           AS INT64  INITIAL 0     NO-UNDO.
   DEF VAR vI1          AS INT64  INITIAL 0     NO-UNDO.
   DEF VAR vI2          AS INT64  INITIAL 0     NO-UNDO.
   DEF VAR opKodVO      AS CHAR NO-UNDO.
   DEF VAR opKodPS      AS CHAR NO-UNDO.
   DEF VAR opSumValCon  AS CHAR NO-UNDO.

   FIND FIRST op WHERE op.op EQ in-opop NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN    /* ��� ���㬥�� �⢠�� */
      RETURN "1".
   FIND FIRST op-entry OF OP WHERE (    op-entry.acct-db NE ?
                                    AND NOT CAN-DO(mBalExs,op-entry.acct-db))
                                OR (    op-entry.acct-cr NE ?
                                    AND NOT CAN-DO(mBalExs,op-entry.acct-cr)) NO-LOCK NO-ERROR.
   IF NOT AVAIL op-entry THEN  /* ��� ������� �⢠�� */
      RETURN "2".
   opSumValCon = GetXAttrValueEx("op",
                                 STRING(op.op),
                                 "�㬬���������",
                                 "").
   opKodVO = GetXAttrValueEx("op",
                             STRING(op.op),
                             "��������117",
                             "").
   IF    opKodVO EQ "" /* ��� ������� ⮦� ����� 㪠���� � �����祭�� */
      OR NUM-ENTRIES(opKodVO) LE 5 THEN
   DO:
      IF opKodVO EQ "" THEN
      DO:
         vI = INDEX(op.details,"~{VO") + 3.
         IF vI LE 3 THEN
            ASSIGN
               vnofig = TRUE /* ᪮��� �� 䨣�ୠ� */
               vI     = INDEX(op.details,"(VO") + 3
            .
         IF vI GT 3 THEN
         DO:
            ASSIGN
               vI1 = INDEX(op.details,"PS",vI)
               vI2 = IF vnofig THEN INDEX(op.details,")",vI)
                               ELSE INDEX(op.details,"}",vI)
            .
            IF    vI1 EQ 0          /* ��� PS          */
               OR vI2 LT vI1 THEN   /* ��� �� ᪮����� */
               vI1 = vI2.
            IF vI2 GT 0 THEN        /* ���� �� ����� */
            DO:
               ASSIGN
                  opKodVO = SUBSTRING(op.details,vI,vI1 - vI)
                  opKodPS = IF vI2 GT vI1 THEN SUBSTRING(op.details,vI1 + 2,vI2 - vI1 - 2)
                                          ELSE ""
               .
            END.
         END.
         /* �஢�ઠ �� ������⢨� ४����� ��� �����४⭮�� ���� ���.����樨 */
         INT64(opKodVO) NO-ERROR.
         IF vI2 LE 0 OR ERROR-STATUS:ERROR OR LENGTH(opKodVO) NE 5 THEN
            RETURN "3".
      END.
      ELSE DO:
         vtt-handle:BUFFER-CREATE.
         ASSIGN
            opKodPS = IF NUM-ENTRIES(opKodVO) GT 1 THEN
                         ENTRY(2,opKodVO)
                      ELSE GetXAttrValueEx("op",
                                           STRING(op.op),
                                           "��ᯮ�⑤����",
                                           "")
            opKodVO = ENTRY(1,opKodVO)
         .
      END.
      
      RUN pGet117ReqB (op.op,
                       opKodVO,
                       opKodPS,
                       opSumValCon,
                       OUTPUT itt-handle).

      RETURN {&RETURN_VALUE}.

   END.
   ELSE DO: /* ��� ������ ����� ���� ����� ����ᥩ � ४������ */
      IF opKodVO EQ "" THEN
         RETURN "4".
      DO vI = 1 TO NUM-ENTRIES(opKodVO,"|"):
         opKodPS = ENTRY(vI,opKodVO,"|").
         vtt-handle:BUFFER-CREATE.
         IF NUM-ENTRIES(opKodPS) GE 5 THEN
         DO:
            ASSIGN
               vtt-handle:BUFFER-FIELD("KodVO"):BUFFER-VALUE = ENTRY(1,opKodPS)
               vtt-handle:BUFFER-FIELD("KodPS"):BUFFER-VALUE = ENTRY(2,opKodPS)
               vtt-handle:BUFFER-FIELD("op-su"):BUFFER-VALUE = DECIMAL(ENTRY(3,opKodPS))
               vtt-handle:BUFFER-FIELD("VC-su"):BUFFER-VALUE = DECIMAL(ENTRY(4,opKodPS))
               vtt-handle:BUFFER-FIELD("VC-cu"):BUFFER-VALUE = ENTRY(5,opKodPS)
               vtt-handle:BUFFER-FIELD("op2su"):BUFFER-VALUE = DECIMAL(ENTRY(3,opKodPS)).
            .
            IF     vtt-handle:BUFFER-FIELD("VC-cu"):BUFFER-VALUE EQ ""
               AND vtt-handle:BUFFER-FIELD("VC-su"):BUFFER-VALUE NE 0 THEN
               vtt-handle:BUFFER-FIELD("VC-cu"):BUFFER-VALUE = "643".
            IF NUM-ENTRIES(opKodPS) GT 5 THEN
               vtt-handle:BUFFER-FIELD("op2su"):BUFFER-VALUE = DECIMAL(ENTRY(6,opKodPS)).
            IF NUM-ENTRIES(opKodPS) GT 6 THEN
               ASSIGN
                  vtt-handle:BUFFER-FIELD("op-db"):BUFFER-VALUE = DECIMAL(ENTRY(7,opKodPS))
                  vtt-handle:BUFFER-FIELD("op-cr"):BUFFER-VALUE = DECIMAL(ENTRY(8,opKodPS))
                  vtt-handle:BUFFER-FIELD("db-cr"):BUFFER-VALUE = ENTRY(9,opKodPS)
               .
            ELSE DO:
               FIND FIRST tmpop-entry OF op WHERE tmpop-entry.acct-db NE ?
                                              AND NOT CAN-DO(mBalExs,tmpop-entry.acct-db)
                    NO-LOCK NO-ERROR.
               vtt-handle:BUFFER-FIELD("op-db"):BUFFER-VALUE =
                  IF AVAIL tmpop-entry THEN tmpop-entry.op-entry ELSE op-entry.op-entry.
               FIND FIRST tmpop-entry OF op WHERE tmpop-entry.acct-cr NE ?
                                              AND NOT CAN-DO(mBalExs,tmpop-entry.acct-cr)
                    NO-LOCK NO-ERROR.
               vtt-handle:BUFFER-FIELD("op-cr"):BUFFER-VALUE =
                  IF AVAIL tmpop-entry THEN tmpop-entry.op-entry ELSE op-entry.op-entry.

               vtt-handle:BUFFER-FIELD("db-cr"):BUFFER-VALUE = "�".
            END.
         END.
         IF NUM-ENTRIES(opKodPS) GE 10 THEN
         DO:
            ASSIGN
               vtt-handle:BUFFER-FIELD("num-cont"):BUFFER-VALUE  = SUBSTRING(ENTRY(10,opKodPS),1,30) 
               vtt-handle:BUFFER-FIELD("date-cont"):BUFFER-VALUE = DATE(SUBSTRING(ENTRY(10,opKodPS),31,8)) NO-ERROR.
         END.
      END.
      RETURN "0".
   END.
END PROCEDURE.
/************************************************************************
 �������⥫�� �����㬥�� ��� ���⠢����� ४����⮢ ���㬥�� �� �����. 117
�������⭮�����樨 � ��ᯮ�⑤���� � ��. ��।����� ��� ��ࠬ���� 
�����頥� HANDLE �� TEMP-TABLE. ����ন� ����� ��� ��� �஢���� ����㬥��
************************************************************************/
PROCEDURE pGet117ReqB:
   DEF INPUT  PARAM in-opop     AS INT64    NO-UNDO.
   DEF INPUT  PARAM opKodVO     AS CHAR NO-UNDO.
   DEF INPUT  PARAM opKodPS     AS CHAR NO-UNDO.
   DEF INPUT  PARAM opSumValCon AS CHAR NO-UNDO.
   DEF OUTPUT PARAM itt-handle  AS HANDLE NO-UNDO.
   
   DEF VAR vtt-handle AS HANDLE NO-UNDO.
   DEF VAR mBalExs    AS CHARACTER NO-UNDO.
   DEF VAR vAcctCurr    AS CHAR NO-UNDO.
   DEF VAR macctcountry AS CHAR NO-UNDO.
   DEF VAR mCustCat     AS CHAR NO-UNDO.

   mBalExs   = FGetSetting("�믨᪨","��⠊�","").
   itt-handle = TEMP-TABLE tt-kod117:HANDLE.

   vtt-handle = itt-handle:DEFAULT-BUFFER-HANDLE.
   vtt-handle:EMPTY-TEMP-TABLE().
  
   DEF BUFFER op FOR op.
   
   FIND FIRST op WHERE op.op EQ in-opop NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN    /* ��� ���㬥�� �⢠�� */
      RETURN "1".
   FIND FIRST op-entry OF OP WHERE (    op-entry.acct-db NE ?
                                    AND NOT CAN-DO(mBalExs,op-entry.acct-db))
                                OR (    op-entry.acct-cr NE ?
                                    AND NOT CAN-DO(mBalExs,op-entry.acct-cr)) NO-LOCK NO-ERROR.
   IF NOT AVAIL op-entry THEN  /* ��� ������� �⢠�� */
      RETURN "2".
      
   FOR EACH op-entry OF OP WHERE (    op-entry.acct-db NE ?
                                  AND NOT CAN-DO(mBalExs,op-entry.acct-db))
                              OR (    op-entry.acct-cr NE ?
                                  AND NOT CAN-DO(mBalExs,op-entry.acct-cr)) NO-LOCK:
      vtt-handle:BUFFER-CREATE.
      ASSIGN
         vtt-handle:BUFFER-FIELD("KodVO"):BUFFER-VALUE = opKodVO
         vtt-handle:BUFFER-FIELD("KodPS"):BUFFER-VALUE =  "" /* opKodPS ⮫쪮 ��� �����⮢ */
         vtt-handle:BUFFER-FIELD("VC-su"):BUFFER-VALUE =
            DECIMAL(ENTRY(1,opSumValCon))
         vtt-handle:BUFFER-FIELD("VC-cu"):BUFFER-VALUE =
            IF NUM-ENTRIES(opSumValCon) GT 1 THEN ENTRY(2,opSumValCon)
                                             ELSE ""
         vtt-handle:BUFFER-FIELD("db-cr"):BUFFER-VALUE =
            IF op-entry.acct-db NE ? THEN "�"
                                     ELSE "�"
      .
      IF     op-entry.acct-db NE ?
         AND NOT CAN-DO(mBalExs,op-entry.acct-db) THEN
      DO:
         IF op-entry.acct-cr EQ ? THEN
            vtt-handle:BUFFER-FIELD("op-cr"):BUFFER-VALUE = ?.
         vtt-handle:BUFFER-FIELD("op-db"):BUFFER-VALUE = op-entry.op-entry.
         vtt-handle:BUFFER-FIELD("op-su"):BUFFER-VALUE = op-entry.amt-rub.
         vAcctCurr = op-entry.currency.
         IF op-entry.currency NE "" THEN     /*ᬮ�ਬ �� �㡫��� �� ��� */
         DO:
            {find-act.i
               &acct = op-entry.acct-db
               &curr = op-entry.currency
            }
            IF NOT(    AVAIL acct
                   AND acct.currency EQ "") THEN
               vtt-handle:BUFFER-FIELD("op-su"):BUFFER-VALUE = op-entry.amt-cur.
            ELSE
               vAcctCurr = "".
         END.
         RUN GetCountryIdCli IN h_Acct (op-entry.acct-db,
                                        vAcctCurr,
                                        OUTPUT macctcountry,
                                        OUTPUT mCustCat).
         IF     mCustCat NE "�"
            AND (   macctcountry EQ "RUS"
                 OR macctcountry EQ "643") THEN
            vtt-handle:BUFFER-FIELD("KodPS"):BUFFER-VALUE = opKodPS.

      END.
      IF     op-entry.acct-cr NE ?
         AND NOT CAN-DO(mBalExs,op-entry.acct-cr) THEN
      DO:
         IF op-entry.acct-db EQ ? THEN
            vtt-handle:BUFFER-FIELD("op-db"):BUFFER-VALUE = ?.
         vtt-handle:BUFFER-FIELD("op-cr"):BUFFER-VALUE = op-entry.op-entry.
         vtt-handle:BUFFER-FIELD("op2su"):BUFFER-VALUE = op-entry.amt-rub.
         vAcctCurr = op-entry.currency.
         IF op-entry.currency NE "" THEN
         DO:
            {find-act.i
               &acct = op-entry.acct-cr
               &curr = op-entry.currency
            }
             IF NOT(    AVAIL acct
                        AND acct.currency EQ "") THEN
               vtt-handle:BUFFER-FIELD("op2su"):BUFFER-VALUE = op-entry.amt-cur.
            ELSE
               vAcctCurr = "".
         END.
         IF vtt-handle:BUFFER-FIELD("KodPS"):BUFFER-VALUE EQ "" THEN
         DO:
            RUN GetCountryIdCli IN h_Acct (op-entry.acct-cr,
                                           vAcctCurr,
                                           OUTPUT macctcountry,
                                           OUTPUT mCustCat).
            IF     mCustCat NE "�"
               AND (   macctcountry EQ "RUS"
                    OR macctcountry EQ "643") THEN
               vtt-handle:BUFFER-FIELD("KodPS"):BUFFER-VALUE = opKodPS.
         END.
      END.
   END.
      
   RETURN "0".
END PROCEDURE.


/****************************************************************************/
/* �����頥� ����� ��ࢮ� �����襩�� ����樨 �� ⮣� �� ��ॢ� �࠭���樨 */
/****************************************************************************/

FUNCTION fGetTrnFirstOp RETURNS INT64
   (INPUT iOp AS INT64)
:
   DEFINE VARIABLE mParent   AS INT64   NO-UNDO.
   DEFINE VARIABLE mNumNodes AS INT64   NO-UNDO.
   DEFINE VARIABLE mLevel    AS INT64   NO-UNDO.
   DEFINE VARIABLE mResult   AS INT64   NO-UNDO.

   DEFINE BUFFER tt-xtree FOR tt-tree.

   mResult = ?.

   /* ��।������ ��୥��� ᠡ�࠭���樨 �࠭���樨 */
   mParent = 0.
   FOR FIRST op WHERE op.op EQ iOp NO-LOCK,
       FIRST op-trans OF op NO-LOCK:
      mParent = op-trans.parent.
   END.

   DO WHILE mParent NE 0:
      FIND FIRST op-trans WHERE op-trans.op-trans EQ mParent NO-LOCK NO-ERROR.
      IF AVAILABLE op-trans THEN DO:
         mParent = op-trans.parent.
      END.
      ELSE DO:
         mParent = 0.
      END.
   END.

   IF AVAILABLE op-trans THEN DO:
      /* ����஥��� ��ॢ� �࠭���樨, �᫨ ������ ��� ��७� */
      {empty tt-tree}.
      CREATE tt-tree.
      ASSIGN tt-tree.level = 1
             tt-tree.op-trans = op-trans.op-trans.
      mLevel = 0.

      DO WHILE TRUE:
         mLevel = mLevel + 1.
         mNumNodes = 0.
         FOR EACH tt-xtree WHERE tt-xtree.level EQ mLevel:
            FOR EACH op-trans WHERE op-trans.parent EQ tt-xtree.op-trans NO-LOCK:
               mNumNodes = mNumNodes + 1.
               CREATE tt-tree.
               ASSIGN tt-tree.op-trans = op-trans.op-trans
                      tt-tree.level = mLevel + 1.
            END.
         END.
         IF mNumNodes EQ 0 THEN LEAVE.
      END.

      /* ᮡ�⢥��� ���� �� ��㣮� ����樨, �ਭ������饣� ��ॢ� */
      find-another-op:
      FOR EACH tt-tree:
         FOR EACH op WHERE op.op-trans EQ tt-tree.op-trans
                       AND op.op       NE iOp
         NO-LOCK:
            mResult = op.op.
            LEAVE find-another-op.
         END.
      END.
   END.
   RETURN mResult.
END FUNCTION.

/****************************************************************************************/
/* �஢����, ���� �� ��뫪� �� ���㬥�� � ���. ४������ ������47423 ��� ������47423  */
/****************************************************************************************/
FUNCTION Chksgnopint RETURNS LOGICAL
   (INPUT iOp AS INT64):

   FIND FIRST signs WHERE
              signs.FILE-NAME EQ "op-int"
          AND signs.code EQ "������47423"
          AND signs.code-value BEGINS STRING(iOp) + ","
              NO-LOCK NO-ERROR.

   IF NOT AVAIL(signs) THEN
      FIND FIRST signs WHERE
                 signs.FILE-NAME EQ "op-int"
             AND signs.code EQ "������47423"
             AND signs.code-value BEGINS STRING(iOp) + ","
                 NO-LOCK NO-ERROR.
          
   IF AVAIL(signs) THEN RETURN YES.
   ELSE RETURN NO.

END FUNCTION.


/* �஢����, ���� �� ���㬥�� ������ᨮ���? �᫨ ���� �����頥�
   �����㥬� � �।��㥬� ��� ������ᨮ���� ����樨, �� �㬬� � ������
*/
FUNCTION IsOpConv RETURNS LOGICAL (INPUT  iOp     AS INT64,
                                   OUTPUT oAcctDb AS CHARACTER,
                                   OUTPUT oCurrDb AS CHARACTER,
                                   OUTPUT oAmtDb  AS DECIMAL,    
                                   OUTPUT oAcctCr AS CHARACTER,
                                   OUTPUT oCurrCr AS CHARACTER,
                                   OUTPUT oAmtCr  AS DECIMAL):
   
   DEFINE VARIABLE IsOpConv     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vMaskAccept  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vMaskDecline AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCurrency    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNum         AS INT64   NO-UNDO.
   DEFINE VARIABLE vFlgPolu     AS LOGICAL   NO-UNDO. /* 䫠� ����஢���� */
   
   DEFINE BUFFER dacct    FOR acct.     /* ���������� ����. */
   DEFINE BUFFER cacct    FOR acct.     /* ���������� ����. */
   DEFINE BUFFER op-entry FOR op-entry. /* ���������� ����. */

   vMaskAccept  = FGetSetting("�������","����⡮�","20202*,20206*,20207*,20208*").
   vMaskDecline = FGetSetting("�������","�����ᥢ","").
   vCurrency    = ?.
   
   FOR EACH op-entry WHERE 
            op-entry.op EQ iOp NO-LOCK:
      vNum = vNum + 1.
      IF vNum GT 2 THEN 
         LEAVE. 

      IsOpConv = YES.      
      IF op-entry.acct-db = ? THEN DO:
         vFlgPolu = YES.
         {find-act.i
            &bact     = cacct
            &acct     = op-entry.acct-cr
            &curr     = op-entry.currency
            &AddWhere = "AND CAN-DO(vMaskAccept,cacct.acct) AND NOT CAN-DO(vMaskDecline,cacct.acct)"
         }
         /* ��� �� ����襫 �� ��᪠� */
         IF NOT AVAIL cacct THEN DO:
            IsOpConv = NO.
            LEAVE.
         END.
         /* ����� �஢���� ᮢ������ � ����⮩ �।��饩 �஢���� */
         IF vCurrency EQ op-entry.currency THEN DO:
            IsOpConv = NO.
            LEAVE.
         END.
         ASSIGN 
            oAcctCr   = op-entry.acct-cr
            oCurrCr   = op-entry.currency
            vCurrency = op-entry.currency
            oAmtCr    = IF op-entry.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub
         .
      END.
      IF op-entry.acct-cr = ? THEN DO:
         vFlgPolu = YES.
         {find-act.i
            &bact     = dacct
            &acct     = op-entry.acct-db
            &curr     = op-entry.currency
            &AddWhere = "AND CAN-DO(vMaskAccept,dacct.acct) AND NOT CAN-DO(vMaskDecline,dacct.acct)"
         }
         /* ��� �� ����襫 �� ��᪠� */
         IF NOT AVAIL dacct THEN DO:
            IsOpConv = NO.
            LEAVE.
         END.
         /* ����� �஢���� ᮢ������ � ����⮩ �।��饩 �஢���� */
         IF vCurrency EQ op-entry.currency THEN DO:
            IsOpConv = NO.
            LEAVE.
         END.
         ASSIGN 
            oAcctDb   = op-entry.acct-db
            oCurrDb   = op-entry.currency
            vCurrency = op-entry.currency
            oAmtDb    = IF op-entry.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub
         .
      END.
   END.
   
   IF    NOT IsOpConv 
      OR vNum = 1
      OR NOT vFlgPolu 
   THEN
      ASSIGN
         oAcctDb  = ?
         oCurrDb  = ?
         oAmtDb   = ?
         oAcctCr  = ?
         oCurrCr  = ?
         oAmtCr   = ?
         IsOpConv = NO
      .

   RETURN IsOpConv.
END FUNCTION.

/*�㬥��� */
FUNCTION CountDocNum RETURNS CHARACTER (INPUT iDefCount AS INT64, 
                                        INPUT iDefValue AS CHARACTER,
                                        INPUT iDateOp   AS DATE):
   DEFINE VAR mNameCount AS CHARACTER NO-UNDO.
   DEFINE VAR mCountVal  AS INT64   NO-UNDO.
   DEFINE VAR mPar       AS CHAR     NO-UNDO.

   IF NUM-ENTRIES(iDefValue,"@") >= 2
   THEN mPar = ENTRY (2, iDefValue, "@").

   IF {assigned mPar}
   THEN mNameCount = mPar.
   ELSE DO:
      /*��⮭㬨��� ���㬥�� ����饭�*/
      IF NOT autonumdoc THEN RETURN ENTRY (1,iDefValue,"@").
      mNameCount = FGetSetting("�⠭���","���㬄��","").
   END.

   /* �᫨ ��� ����஥����� ���稪� ��� ���㬥�⮢, � �����頥� 
      ���祭�� ���稪� �� 㬠�砭�� (� auto-num.i ��� �ࠢ��� op.op
   */
   IF mNameCount EQ "" THEN RETURN STRING(iDefCount).

   mCountVal = GetCounterNextValue(mNameCount,iDateOp).

   /*�� ����饭 �ࢥ� ��� ���稪� (0037349) ��� �� ����஥� ���稪 � ����஥筮��
     ��ࠬ��� "�⠭���/���㬄��" � �����䨪��� COUNTERS */
   IF mCountVal EQ ? THEN RETURN "ERROR".
   
   RETURN STRING(mCountVal).

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* �஢����, �������� �� ��᭮� ᠫ줮                                      */
/* ������ ����� ����������, �᫨ ����楯⮢���� ���㬥�� ��室� �� ����  */
/* �㤥� ���㫨஢��                                                          */
/*----------------------------------------------------------------------------*/

PROCEDURE CheckProbSaldo.
   DEFINE INPUT  PARAMETER iSide   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAmtRub AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iAmtCur AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iOpDate AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iStat   AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oError  AS LOGICAL     NO-UNDO.

   DEFINE BUFFER acct     FOR acct.
   DEFINE BUFFER acct-pos FOR acct-pos.

   DEFINE VARIABLE vContMask  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcctMask  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCheck     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vBal       AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vClBal     AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vLastClDay AS DATE        NO-UNDO.
   DEFINE VARIABLE vDbP       AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vDbA       AS DECIMAL     NO-UNDO.

MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   vCheck = (fGetSetting ("�⠭���", "CheckRedSaldo", "���") EQ "��") NO-ERROR.
   IF NOT vCheck THEN
      LEAVE MAIN.
   {find-act.i 
      &acct = iAcct 
      &curr = iCurr}
   IF NOT AVAIL acct THEN
      LEAVE MAIN.

   vContMask = fGetSetting ("�⠭���", "ContAcctSaldo", "") NO-ERROR.
   vAcctMask = fGetSetting ("�⠭���", "AcctSaldo", "") NO-ERROR.
   IF NOT ((CAN-DO(vContMask, acct.contract)) OR (CAN-DO(vAcctMask, acct.acct))) THEN
      LEAVE MAIN.
   IF (acct.side     EQ "�"     AND iSide         EQ "db" AND 
      acct.check-op NE "�����" AND acct.check-op NE "�������") OR
     (acct.side     EQ "�"     AND iSide         EQ "cr" AND
      acct.check-op NE "�����" AND acct.check-op NE "�������")               
       THEN
   DO:
      vLastClDay = Get_Date_Cat(acct.acct-cat, ?).
      IF vLastClDay EQ ? THEN
         vLastClDay = iOpDate - 1.

      /* 䠪��᪨� ���⮪ */
      RUN acct-pos IN h_base (acct.acct, acct.currency, vLastClDay + 1, iOpDate, CHR(251)).
      vBal = IF acct.currency EQ "" THEN sh-bal ELSE sh-val.
      IF iStat GE CHR(251) THEN
         vBal = IF acct.currency EQ "" THEN (vBal + iAmtRub) ELSE (vBal + iAmtCur).
      vDbA = IF acct.currency EQ "" THEN sh-db ELSE sh-vdb.
      IF iStat GE CHR(251) THEN
         vDbA = IF acct.currency EQ "" THEN (vDbA + iAmtRub)  ELSE (vDbA + iAmtCur).
      RUN acct-pos IN h_base (acct.acct, acct.currency, vLastClDay + 1, iOpDate, "�").
      vDbP = IF acct.currency EQ "" THEN sh-db ELSE sh-vdb.
      IF iStat GE "�" THEN
         vDbP = IF acct.currency EQ "" THEN (vDbP + iAmtRub) ELSE (vDbP + iAmtCur).
      ASSIGN
         vDbP = vDbP - vDbA   /* ����楯⮢���� ���㬥��� ��室� */
         vBal = vBal + vDbP.  /* ���⮪ � ��⮬ ����楯⮢������ ��室� */
      oError = (vBal GT 0).
   END.
END.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* ��।���� ����㯭� ���⮪ (�㬬� ��樯⮢����� ���㬥�⮢ ���᫥��� -  */
/* �㬬� ���㬥�⮢ ᯨᠭ�� � ����ᮬ ��� � - �㬬� �� ���㬥�⠬ ��      */
/*   ����⥪� 2)                                                             */
/*----------------------------------------------------------------------------*/

PROCEDURE CalcAvailPos.
   DEFINE INPUT  PARAMETER iAcct   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iDate   AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iDate2  AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iStat   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iStat2  AS CHARACTER   NO-UNDO. /* ����� ��� ����祭�� ���⪠ �� ��⠬, �� 㤮���⢮���騬 ��᪥ �� �� AccessAcct */
   DEFINE INPUT  PARAMETER iProc   AS CHARACTER   NO-UNDO. /* acct-pos ��� cli-pos */
   DEFINE INPUT  PARAMETER iCrd    AS LOGICAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iSeq    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iChkBlk AS LOGICAL     NO-UNDO.

   DEFINE OUTPUT PARAMETER oBal    LIKE acct-pos.balance NO-UNDO.
   DEFINE OUTPUT PARAMETER oVal    LIKE acct-pos.balance NO-UNDO.

   DEFINE BUFFER acct     FOR acct.
   DEFINE BUFFER oacct    FOR acct.

   DEFINE VARIABLE vOAcct    AS RECID     NO-UNDO.
   DEFINE VARIABLE vBegDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE sh-db0    LIKE sh-db   NO-UNDO.
   DEFINE VARIABLE sh-cr0    LIKE sh-cr   NO-UNDO.
   DEFINE VARIABLE sh-vdb0   LIKE sh-vdb  NO-UNDO.
   DEFINE VARIABLE sh-vcr0   LIKE sh-vcr  NO-UNDO.
   DEFINE VARIABLE vBldb     AS DECIMAL INIT 0.0 NO-UNDO.
   DEFINE VARIABLE vBlcr     AS DECIMAL INIT 0.0 NO-UNDO.
   DEFINE VARIABLE vSumCrd2  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vBlkMask  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlkList  AS CHARACTER NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, LEAVE MAIN:

      {find-act.i 
         &acct = iAcct 
         &curr = iCurr}
         
      IF NOT AVAIL acct THEN
         LEAVE MAIN.
    
      IF NOT CAN-DO(vAccessMask,acct.acct) AND NOT CAN-DO(vAccessContAcct,acct.contract) THEN DO:

         RUN VALUE(iProc) IN h_base (acct.acct, acct.currency, iDate, iDate2, iStat2).

         ASSIGN
            oBal = sh-bal
            oVal = sh-val
          .
      END.
      ELSE DO:

         vBegDate = FGetLastClsDate(iDate2,acct.acct-cat).

         RUN VALUE(iProc) IN h_base (acct.acct, acct.currency, vBegDate, iDate2, CHR(251)).
         ASSIGN
            sh-db0  = sh-db
            sh-cr0  = sh-cr
            sh-vdb0 = sh-vdb
            sh-vcr0 = sh-vcr
         .
        
         RUN VALUE(iProc) IN h_base (acct.acct, acct.currency, vBegDate, iDate2, iStat).
        
         IF iCrd THEN DO:
            /* ��� ���㬥�⮢ �� ����⥪� 2 */
            RUN card-acct(recid(acct), OUTPUT vOAcct, "����2�����").
            IF vOAcct NE ? THEN 
               FIND FIRST oacct WHERE RECID(oacct) EQ vOAcct NO-LOCK NO-ERROR. 
            IF AVAIL(oacct) THEN 
               RUN calc-crd2(oacct.acct,acct.acct,acct.currency, OUTPUT vSumCrd2).
         END.

         vBlkList = IF iSeq = "-1"
                    THEN BlockAcctNoOrderPay(acct.acct + "," + acct.currency,
                                             iDate2)
                    ELSE BlockAcct(acct.acct + "," + acct.currency,
                                   iDate2).

         IF CAN-DO(vBlkList,"����") THEN 
         ASSIGN
               oBal = 0
               oVal = 0
            .
         ELSE DO: 
            IF iChkBlk THEN DO:         
               vBlkMask = fGetSetting ("�⠭���", "AccessBlock", "").
               RUN CalcBlkMask IN h_blkob(INPUT acct.acct + "," + acct.currency, INPUT iDate2, INPUT vBlkMask, INPUT iSeq, OUTPUT vBldb, OUTPUT vBlcr).
            END.
            ASSIGN
               oBal = sh-bal + IF acct.side NE "�" THEN (sh-db0  - sh-db - vSumcrd2 - vBldb) ELSE (sh-cr  - sh-cr0 + vSumCrd2 - vBlcr) /* ⮫쪮 ��楯⮢����� ���㬥��� ���᫥��� (����� ��⨢��� ��⮢, �।�� ���ᨢ���)*/
               oVal = sh-val + IF acct.side NE "�" THEN (sh-vdb0 - sh-vdb - vBldb) ELSE (sh-vcr - sh-vcr0 - vBlcr) 
          .
      END.
      END.   
   END.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �����頥� ��஢�� ��� ���㬥�� �� �����䨪�樨 �� �� ���� ���㬥��    */
/* � ⨯� ��஢��� ���� ��. ��ࠬ����:                                       */
/*  iDocType  - ��� ���㬥�� (doc-type.doc-type).                            */
/*  iCodeType - ⨯ ���� �� (�� �����䨪��� CBCodeTypes).                  */
/*  oDigital  - ��� ���㬥�� �� �����䨪�樨 ��.                            */
/* �᫨ iCodeType <> ?, ��� ��ࠬ��� �����䨪��� CBCodes ��� ����         */
/* ���㬥�� iDocType � ⨯� ���� �� iCodeType. �᫨ ��ࠬ��� ������,         */
/* �����頥� ��� ���祭��, � ��⨢��� ��砥 ��� �᫨ iCodeType = ?         */
/* ��।���� ��஢�� ��� �� �� �ࠢ�筨�� doc-type.                        */
/*----------------------------------------------------------------------------*/

PROCEDURE GetDocTypeDigital.
    DEFINE INPUT  PARAMETER iDocType  LIKE doc-type.doc-type NO-UNDO.
    DEFINE INPUT  PARAMETER iCodeType LIKE code.code         NO-UNDO.
    DEFINE OUTPUT PARAMETER oDigital  LIKE doc-type.digital  NO-UNDO.

    DEFINE BUFFER code     FOR code.
    DEFINE BUFFER doc-type FOR doc-type.

    IF {assigned iCodeType} THEN
        FIND FIRST code WHERE
            code.class          = "CBCodes" AND
            code.parent         = "CBCodes" AND
            ENTRY(1, code.code) = iDocType AND
            ENTRY(2, code.code) = iCodeType
        NO-LOCK NO-ERROR.
    IF AVAILABLE code THEN
        oDigital = code.val.
    ELSE DO:
        FIND FIRST doc-type WHERE
            doc-type.doc-type = iDocType
        NO-LOCK NO-ERROR.
        IF AVAILABLE doc-type THEN
            oDigital = doc-type.digital.
    END.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* ��ਠ�� �।��饩 ��楤���, ����� �� ������⢨� ���� �� ��� ��������� */
/* ⨯� ���㬥�� �����頥� ��室�� ⨯ ���㬥�� �� �����䨪�樨 �������. */
/*----------------------------------------------------------------------------*/

PROCEDURE GetDocTypeDigitalEx.
    DEFINE INPUT  PARAMETER iDocType  LIKE doc-type.doc-type NO-UNDO.
    DEFINE INPUT  PARAMETER iCodeType LIKE code.code         NO-UNDO.
    DEFINE OUTPUT PARAMETER oResult   AS   CHARACTER         NO-UNDO.

    RUN GetDocTypeDigital(iDocType, iCodeType, OUTPUT oResult).
    IF oResult = "" THEN
        oResult = iDocType.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �஢���� ����⢮����� � �����䨪��� CBCodes ����� ��� ��������       */
/* ��஢��� ���� ���㬥�� �� �����䨪�樨 �� (iDigital) � ⨯� ��஢���   */
/* ���� �� (iCodeType), �᫨ ��� ��᫥����� ��।��� ��।��񭭮� ���祭��.   */
/* � ��⨢��� ��砥 ����� �⮣� �஢���� ����⢮����� ���� �� �          */
/* �ࠢ�筨�� ���⭮-�������� ���㬥�⮢ doc-type.                         */
/*----------------------------------------------------------------------------*/

PROCEDURE AvailDocTypeDigital.
    DEFINE INPUT  PARAMETER iDigital  LIKE doc-type.digital NO-UNDO.
    DEFINE INPUT  PARAMETER iCodeType LIKE code.code        NO-UNDO.
    DEFINE OUTPUT PARAMETER oAvail    AS   LOGICAL          NO-UNDO.

    oAvail = {assigned iCodeType}
             AND
             CAN-FIND(FIRST code WHERE
                          code.class          = "CBCodes" AND
                          code.parent         = "CBCodes" AND
                          ENTRY(2, code.code) = iCodeType AND
                          code.val            = iDigital)
             OR
             CAN-FIND(FIRST doc-type WHERE
                          doc-type.digital = iDigital).
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �����頥� ����७��� ��� ���㬥�� �� �����䨪�樨 ������� �� ����       */
/* ���㬥�� �� �����䨪�樨 �� � ⨯� ���� ��. �� ������ ����        */
/* ���⭮� �� �⭮襭�� � GetDocTypeDigital.                                 */
/*----------------------------------------------------------------------------*/

PROCEDURE GetDocType.
    DEFINE INPUT  PARAMETER iDigital  LIKE doc-type.digital  NO-UNDO.
    DEFINE INPUT  PARAMETER iCodeType LIKE code.code         NO-UNDO.
    DEFINE OUTPUT PARAMETER oDocType  LIKE doc-type.doc-type NO-UNDO.

    DEFINE BUFFER code     FOR code.
    DEFINE BUFFER doc-type FOR doc-type.

    IF {assigned iCodeType} THEN
        FIND FIRST code WHERE
            code.class          = "CBCodes" AND
            code.parent         = "CBCodes" AND
            ENTRY(2, code.code) = iCodeType AND
            code.val            = iDigital
        NO-LOCK NO-ERROR.
    IF AVAILABLE code THEN
        oDocType = ENTRY(1, code.code).
    ELSE DO:
        FIND FIRST doc-type WHERE
            doc-type.digital = iDigital
        NO-LOCK NO-ERROR.
        IF AVAILABLE doc-type THEN
            oDocType = doc-type.doc-type.
    END.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �஢���� ����⢮����� � �����䨪��� CBCodes ����� ��� ��������       */
/* ����७���� ���� ���㬥�� �� �����䨪�樨 ������� (iDocType) � ⨯�      */
/* ��஢��� ���� �� (iCodeType), �᫨ ��� ��᫥����� ��।��� ��।��񭭮�   */
/* ���祭��. � ��⨢��� ��砥 ����� �⮣� �஢���� ����⢮�����          */
/* ����७���� ���� ���㬥�� �� �����䨪�樨 ������� � �ࠢ�筨��          */
/* ���⭮-�������� ���㬥�⮢ doc-type.                                     */
/*----------------------------------------------------------------------------*/

PROCEDURE AvailDocType.
    DEFINE INPUT  PARAMETER iDocType  LIKE doc-type.doc-type NO-UNDO.
    DEFINE INPUT  PARAMETER iCodeType LIKE code.code         NO-UNDO.
    DEFINE OUTPUT PARAMETER oAvail    AS   LOGICAL           NO-UNDO.

    oAvail = {assigned iCodeType}
             AND
             CAN-FIND(FIRST code WHERE
                          code.class          = "CBCodes" AND
                          code.parent         = "CBCodes" AND
                          ENTRY(1, code.code) = iDocType AND
                          ENTRY(2, code.code) = iCodeType)
             OR
             CAN-FIND(FIRST doc-type WHERE
                          doc-type.doc-type = iDocType).
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �����頥� ������������ ⨯� ���㬥�� �� �ࠢ�筨�� doc-type �� ���      */
/* ����७���� ���� �� �����䨪�樨 �������.                                 */
/*  iDocType - ��� ���㬥�� (doc-type.doc-type).                             */
/*  oNameDoc - ������������ ⨯� ���㬥�� (doc-type.name-doc).               */
/*----------------------------------------------------------------------------*/

PROCEDURE GetDocTypeName.
    DEFINE INPUT  PARAMETER iDocType LIKE doc-type.doc-type NO-UNDO.
    DEFINE OUTPUT PARAMETER oNameDoc LIKE doc-type.name-doc NO-UNDO.

    DEFINE BUFFER doc-type FOR doc-type.

    FIND FIRST doc-type WHERE
        doc-type.doc-type = iDocType
    NO-LOCK NO-ERROR.
    IF AVAILABLE doc-type THEN
        oNameDoc = doc-type.name-doc.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �����頥� ⨯� ���� �� ���㬥�� ����� ��� �������� ��஭�, � ���ன    */
/* ��� � �����祭��� "����" ������ � �஢����.                           */
/*  isDb - YES: ���� ����� � �����, ����: � �।��.                        */
/*----------------------------------------------------------------------------*/

FUNCTION cashCBCodeType RETURN CHARACTER (INPUT isDb AS LOGICAL):
    RETURN "�." + IF isDb = YES THEN "��" ELSE "��".
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* �����頥� ��஢�� ��� ���ᮢ��� ���㬥�� �� �����䨪�樨 ��.           */
/*  op-entry  - ���� �஢���� ���ᮢ��� ���㬥��.                           */
/*  iAcct     - ����� ��� ����� �� �஢���� ���㬥��. �����祭�� ��        */
/*              �஢������, �ணࠬ���� ������ ᤥ���� �� ��࠭��.          */
/*  oDigital  - ��� ���㬥�� �� �����䨪�樨 ��.                            */
/* ��楤�� ࠡ�⠥� �१ GetDocTypeDigital. ���� ����� �� ��।������� �� */
/* ����� cashCBCodeType.                                                     */
/*----------------------------------------------------------------------------*/

PROCEDURE GetCashDocTypeDigital.
    DEFINE PARAMETER BUFFER op-entry FOR  op-entry.
    DEFINE INPUT  PARAMETER iAcct    LIKE acct.acct         NO-UNDO.
    DEFINE OUTPUT PARAMETER oDigital LIKE doc-type.digital  NO-UNDO.

    DEFINE BUFFER op FOR op.

    IF AVAILABLE op-entry AND
       (op-entry.acct-db = iAcct OR op-entry.acct-cr = iAcct)
    THEN
        FIND FIRST op WHERE op.op = op-entry.op NO-LOCK NO-ERROR.
    IF AVAILABLE op THEN
        RUN GetDocTypeDigital(op.doc-type,
                              cashCBCodeType(op-entry.acct-db = iAcct),
                              OUTPUT oDigital).
END.

/*----------------------------------------------------------------------------*/
/* �����頥� ᯨ᮪ �����䨪��஢ ���㬥�⮢, ���⪮ �易���� � �������  */
/* �१ ��饥 ���祭�� ����� ����樨 (op.op-transaction).                  */
/*  iOp          - �����䨪��� ��室���� ���㬥��                          */
/*  iCheckOpKind - YES: �ନ஢��� ᯨ᮪ �易���� ���㬥�⮢ ⮫쪮 � ⮬  */
/*                 ��砥, �᫨ �࠭�����, ���ன �� ᮧ��� ��室��       */
/*                 ���㬥��, �������, � ���祭�� ���. ४����� "���섮�" */
/*                 �� ��� ࠢ�� "��". �� ����襭�� ��� �᫮��� १���⮬  */
/*                 ࠡ��� ��楤��� �㤥� ����।��񭭮� ���祭��.            */
/*                 NO: �� �஢����� �࠭�����, �������� ᯨ᮪ �易����   */
/*                 ���㬥�⮢ � �� ��砥.                                 */
/*  oResult      - ᯨ᮪ �����䨪��஢ �易���� ���㬥�⮢, ����� ��ப� */
/*                 �� ������⢨� ⠪����. ����� �. ���ᠭ�� iCheckOpKind.   */
/*----------------------------------------------------------------------------*/

PROCEDURE GetLinkedOps.
    DEFINE INPUT PARAMETER iOp LIKE op.op NO-UNDO.
    DEFINE INPUT PARAMETER iCheckOpKind AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oResult AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vOpTrans LIKE op.op-transaction NO-UNDO.

    DEFINE BUFFER op FOR op.
    DEFINE BUFFER op-kind FOR op-kind.

    FIND FIRST op WHERE op.op = iOp NO-LOCK NO-ERROR.

    IF AVAILABLE op THEN
        FIND FIRST op-kind WHERE op-kind.op-kind = op.op-kind NO-LOCK NO-ERROR.

    IF iCheckOpKind AND
       (NOT AVAILABLE op-kind OR
        GetXAttrValue("op-kind", op-kind.op-kind, "���섮�") <> "��")
    THEN DO:
        oResult = ?.
        RETURN.
    END.

    ASSIGN
        oResult  = ""
        vOpTrans = op.op-transaction
    .

    FOR EACH op WHERE
        op.op-transaction =  vOpTrans AND
        op.op             <> iOp
    NO-LOCK:
        {additem.i oResult STRING(op.op)}
    END.
END PROCEDURE.

PROCEDURE IsBudgetPayment.
   DEFINE INPUT  PARAMETER iOp     LIKE op.op   NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS   LOGICAL NO-UNDO INITIAL NO.

   DEFINE BUFFER op FOR op.
   
   DEFINE VARIABLE vBalBudget    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBalBudgetKBK AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBalBudgetAcc AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKBK          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSubKBK       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKBKPosLst    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKBKValLst    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNumPosLst    AS INT64       NO-UNDO.
   DEFINE VARIABLE vNumValLst    AS INT64       NO-UNDO.
   DEFINE VARIABLE vI            AS INT64       NO-UNDO.
   DEFINE VARIABLE vI1           AS INT64     NO-UNDO.
   DEFINE VARIABLE vI2           AS INT64     NO-UNDO.
   DEFINE VARIABLE vIOch         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBalIskl1     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBalIskl2     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIAcct        AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE
      op.op = iOp
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE op THEN
      RETURN.
   
   ASSIGN
      vBalBudget    = FGetSetting("���","bal-budget","")
      vBalBudgetKBK = FGetSetting("���","bal-budget-kbk","")
      vKBKPosLst    = FGetSetting("���","kbk-pos","")
      vBalIskl1     = FGetSetting("���","bal-iskl","").
               
   IF CAN-DO(vBalBudget,op.ben-acct) THEN
      oResult = YES.
   ELSE
   DO:
      ASSIGN
         vKBK          = TRIM(GetXAttrValueEx("op",STRING(op.op),"���",""))
         vBalBudgetAcc = ENTRY(1,vBalBudgetKBK,"|")
         vKBKValLst    = ENTRY(2,vBalBudgetKBK,"|")
         vNumValLst    = NUM-ENTRIES(vKBKValLst,";")
         vNumPosLst    = NUM-ENTRIES(vKBKPosLst,";").
      DO vI = 1 TO MIN(vNumPosLst,vNumValLst):
         IF CAN-DO(vBalBudgetAcc,op.ben-acct) THEN
         DO:
            vSubKBK = SUBSTRING(vKBK,INT64(ENTRY(1,ENTRY(vI,vKBKPosLst,";"),",")),
                                     INT64(ENTRY(2,ENTRY(vI,vKBKPosLst,";"),","))).
            oResult = CAN-DO(ENTRY(vI,vKBKValLst,";"),vSubKBK).
            IF oResult THEN LEAVE.
         END.
      END.
      IF oResult EQ NO 
         AND {assigned vBalIskl1} THEN
      DO vI1 = 1 TO NUM-ENTRIES(vBalIskl1,";"):
         vBalIskl2 = ENTRY(vI1,vBalIskl1,";").
         vIAcct    = ENTRY(1,vBalIskl2,"|").
         IF {assigned vIAcct} 
            AND CAN-DO(vIAcct,op.ben-acct) THEN
         DO:
            vIOch = ENTRY(2,vBalIskl2,"|").
            oResult = YES.
            DO vI2 = 1 TO NUM-ENTRIES(vIOch,","):
               IF ENTRY(vI2,vIOch) EQ op.order-pay THEN 
               DO:
                  oResult = NO.
                  LEAVE.
               END.
            END.
         END.
      END.
   END.
END PROCEDURE.

PROCEDURE IsBudgetPaymentEx.
   DEFINE INPUT  PARAMETER iBenAcct AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iKBK     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult  AS LOGICAL NO-UNDO INITIAL NO.

   DEFINE VARIABLE vBalBudget    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBalBudgetKBK AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBalBudgetAcc AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSubKBK       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKBKPosLst    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKBKValLst    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNumPosLst    AS INT64       NO-UNDO.
   DEFINE VARIABLE vNumValLst    AS INT64       NO-UNDO.
   DEFINE VARIABLE vI            AS INT64       NO-UNDO.

   ASSIGN
      vBalBudget    = FGetSetting("���","bal-budget","")
      vBalBudgetKBK = FGetSetting("���","bal-budget-kbk","")
      vKBKPosLst    = FGetSetting("���","kbk-pos","").
               
   IF CAN-DO(vBalBudget,iBenAcct) THEN
      oResult = YES.
   ELSE DO:
      ASSIGN
         vBalBudgetAcc = ENTRY(1,vBalBudgetKBK,"|")
         vKBKValLst    = ENTRY(2,vBalBudgetKBK,"|")
         vNumValLst    = NUM-ENTRIES(vKBKValLst,";")
         vNumPosLst    = NUM-ENTRIES(vKBKPosLst,";").
      DO vI = 1 TO MIN(vNumPosLst,vNumValLst):
         IF CAN-DO(vBalBudgetAcc,iBenAcct) THEN
         DO:
            vSubKBK = SUBSTRING(iKBK,INT64(ENTRY(1,ENTRY(vI,vKBKPosLst,";"),",")),
                                     INT64(ENTRY(2,ENTRY(vI,vKBKPosLst,";"),","))).
            oResult = CAN-DO(ENTRY(vI,vKBKValLst,";"),vSubKBK).
            IF oResult THEN LEAVE.
         END.
      END.
   END.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='20/10/2015 12:33:36.457+04:00' */
/* $LINTUSER='ches' */
/* $LINTMODE='1' */
/* $LINTFILE='op.pro' */
/*prosignpH8MdCaosUAh0iv+3SRWOQ*/
/* --- op.pro was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:32am --- */
