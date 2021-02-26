/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-ITAX.P
      Comment: ������ 䠩��� �襭�� � �����஢��/ࠧ�����஢�� ��⮢
   Parameters: NO-PARAMETRS
      Created: 16.04.2009 15:33 Vasov   
     Modified: 16.04.2009 15:33 Vasov   
     Modified: 12/05/2009 kraw (0103135) TAXBlockAcct, TAXUnblockAcct
     Modified: 24/07/2009 kraw (0114610) � �����஢�� �㬂��� � ����� ������
     Modified: 20/10/2009 kraw (0118788) ������ ��� - �㬬� �� �����
*/

&GLOBAL-DEFINE NO-BASE-PROC YES

{globals.i}             /* �������� ��६���� ��ᨨ. */
&GLOBAL-DEFINE ExtBase      YES
&IF DEFINED(ExtBase) &THEN
{extexch.def} /* ����ন� ���ᠭ�� �६����� ⠡���� ttExtAcct */
{intrface.get plbnk} 
&ENDIF

{form.def}
{tmprecid.def}
{sh-defs.i}
{g-trans.equ}
{exchange.equ}

{intrface.get tmess} 
{intrface.get strng}
{intrface.get exch}
{intrface.get trans}
{intrface.get xclass} 
{intrface.get pack} 
{intrface.get filex}
{intrface.get cust}
{intrface.get instrum}

{intrface.get pack}
{intrface.get rfrnc}

{intrface.get data}
{intrface.get pbase}
{intrface.get db2l}
{intrface.get acct}
{intrface.get blkob}
{ttretval.def}

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "ITAX"
   &LIBNAME       = "������⥪� ����� TAX"
   &DESCRIPTION   = "����� � ����묨 TAX"
   }

DEFINE VARIABLE mSpooling  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mTagCount  AS INT64     NO-UNDO.
DEFINE STREAM   sImport.

DEFINE VARIABLE mCount     AS INT64     NO-UNDO.
DEFINE VARIABLE mCountProc AS INT64     NO-UNDO.
DEFINE VARIABLE mResult    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSummVZ    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mReqTrans  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAnsTrans  AS CHARACTER NO-UNDO.
DEFINE VARIABLE oOp        AS INT64     NO-UNDO.
DEFINE VARIABLE mMess      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFile1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountAll  AS INT64     NO-UNDO.
DEFINE VARIABLE mRetryError AS CHAR      NO-UNDO.

DEFINE VARIABLE oCustCat  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE oCustId   AS INT64      NO-UNDO.

DEFINE VARIABLE mSuperPacketID   AS INT64      NO-UNDO.

DEFINE VARIABLE m365_Join      AS CHARACTER NO-UNDO.
DEFINE VARIABLE m365_BalAcct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE m365_Contract  AS CHARACTER NO-UNDO.
DEFINE VARIABLE m365_RValBlk   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDSD_365       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVBO_365       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPrArch_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mArchSr_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMnozhC_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIgnorKPP_365  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAllFil_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKontrDP_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKontrOP_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKolDoc_365    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTipeOst_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mProvOvrd_365  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mZamRVSO_365   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mShift_365     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mINN           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPP           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMFO           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBank          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNCCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNaznSchKas    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNaznSchMBR    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate_NR       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKorrACCT      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcNSpis       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParSchStN     AS CHARACTER NO-UNDO.

m365_Join     = FGetSetting("����ன��_365�", "��ꥤ�����", "�").
m365_BalAcct  = FGetSetting("����ன��_365�", "�᪫��",     "").
m365_Contract = FGetSetting("����ன��_365�", "�᪫����",   "").
m365_RValBlk  = FGetSetting("����ன��_365�", "����������", "���").

DEFINE TEMP-TABLE ttBlockAcct
   FIELD num           AS INT64
   FIELD acct          AS CHARACTER
   FIELD currency      AS CHARACTER
   FIELD cont-name     AS CHARACTER
   FIELD acct-status   AS CHARACTER
   FIELD msg           AS CHARACTER
   FIELD error-code    AS CHARACTER
   FIELD type          AS CHARACTER
   FIELD open-date     AS CHARACTER
   FIELD close-date     AS CHARACTER
.

DEFINE TEMP-TABLE ttLevel
   FIELD Fmt   AS CHARACTER
   FIELD Level AS INT64
   FIELD Point AS INT64
   FIELD Kind  AS CHARACTER.

DEFINE TEMP-TABLE ttKvt  NO-UNDO
   FIELD AttrCode   AS CHARACTER
   FIELD AttrValue  AS CHARACTER.

DEFINE VARIABLE oHTable2 AS HANDLE      NO-UNDO. /* ��室�騥 ���*/
DEFINE VARIABLE oHTable3 AS HANDLE      NO-UNDO. /* ��室�騥 ���㬥���*/

DEFINE VARIABLE mRsSrv  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRsInf  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRsAcc  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRsDoc  AS CHARACTER NO-UNDO.

DEFINE STREAM sReq.

{365p.def}
{core365p.pro}
{spec365p.pro}
/*{core440p.pro}*/
/*{spec440p.pro} ��� */
{pno365p2.pro}
{zno365p2.pro}
{log365p.def}

ASSIGN
   mINN          = FGetSetting("���", "", "")
   mKPP          = FGetSetting("�������", "", "")
   mMFO          = FGetSetting("�������", "", "")
   mBank         = FGetSetting("����", "", "") 
   mNCCode       = FGetSetting("�����悠�0406007", "", "")
   mDSD_365      = FGetSetting("����ன��_365�","��������넮�","")
   mVBO_365      = FGetSetting("����ன��_365�","��火�����","���")
   mPrArch_365   = FGetSetting("����ன��_365�", "�஢��娢","")
   mArchSr_365   = FGetSetting("����ன��_365�", "��娢�ப","0")
   mNaznSchKas   = FGetSetting("�����犠�", "", "����") 
   mNaznSchMBR   = FGetSetting("�����猁�", "", "")   
   mMnozhC_365   = FGetSetting("����ன��_365�", "������", "") 
   mIgnorKPP_365 = FGetSetting("����ன��_365�", "����������", "���") 
   mAllFil_365   = FGetSetting("����ன��_365�", "�ᥔ������", "���") 
   mDate_NR      = FGetSetting("���_��", "", "") 
   mKontrDP_365  = FGetSetting("����ன��_365�", "��������돮���", "")
   mKontrOP_365  = FGetSetting("����ன��_365�", "����������", "*")   
   mKorrACCT     = FGetSetting("�����", "", "") 
   mKolDoc_365   = FGetSetting("����ன��_365�", "������", "0")
   mTipeOst_365  = FGetSetting("����ன��_365�", "������⪠", "")   
   mProvOvrd_365 = FGetSetting("����ன��_365�","�஢���","���")
   mZamRVSO_365  = FGetSetting("����ன��_365�","���������������",{&SEPARATOR-365P})
   mShift_365    = FGetSetting("����ன��_365�","�����","0")
   mAcNSpis      = FGetSetting("�獑���", "", "")                
   mParSchStN    = FGetSetting("����ன��_365�","��ࠬ��⍮�","���_������")
NO-ERROR.

DEF TEMP-TABLE ttop-poruch NO-UNDO
   FIELD f1 as char column-label "��� � �६�"         FORMAT "x(10)"
   FIELD f2 as char column-label "������������ ������" FORMAT "x(20)"
   FIELD f3 as char column-label "���"                  FORMAT "x(12)"
   FIELD f4 as char column-label "���"                 FORMAT "x(20)"
   FIELD f5 as char column-label "�����"                FORMAT "x(6)"
   FIELD f6 as date column-label "���"                 FORMAT "99/99/9999"
   FIELD f7 as dec  column-label "�㬬�"                FORMAT ">>>>>>>>>>>>>>9.99"
   FIELD f8 as char column-label "���"               FORMAT "x(10)"
   FIELD f9 as char column-label "����"                 FORMAT "x(20)"
   FIELD {&order-field} AS char /*��� ���஢��*/
.

DEF TEMP-TABLE ttop-treb NO-UNDO
   FIELD f1 as char column-label "��� � �६�"         FORMAT "x(10)"
   FIELD f2 as char column-label "������������ ������" FORMAT "x(20)"
   FIELD f3 as char column-label "���"                  FORMAT "x(12)"
   FIELD f4 as char column-label "�����"                FORMAT "x(6)"
   FIELD f5 as date column-label "���"                 FORMAT "99/99/9999"
   FIELD f6 as dec  column-label "�㬬�"                FORMAT ">>>>>>>>>>>>>>9.99"
   FIELD f7 as char column-label "���"               FORMAT "x(10)"
   FIELD f8 as char column-label "����"                 FORMAT "x(20)"
   FIELD {&order-field} AS char /*��� ���஢��*/
.


DEF TEMP-TABLE ttop-zapr NO-UNDO
   FIELD f1 as char column-label "��� � �६�"         FORMAT "x(10)"
   FIELD f2 as char column-label "������������ ������" FORMAT "x(20)"
   FIELD f3 as char column-label "���"                  FORMAT "x(12)"
   FIELD f4 as char column-label "��� �����"          FORMAT "x(11)"
   FIELD f5 as char column-label "���"               FORMAT "x(10)"
   FIELD f6 as char column-label "����"                 FORMAT "x(20)"
   FIELD {&order-field} AS char /*��� ���஢��*/
.

&GLOB RETRY-ERROR mRetryError

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-bv
   FIELD SeanceID  AS INT64
   FIELD ReqPackID AS INT64
   FIELD BVPart    AS CHARACTER
.

FUNCTION TAXGetFormat RETURNS LOGICAL (INPUT-OUTPUT ioFormat AS CHARACTER,
                                       INPUT-OUTPUT ioLevel  AS INT64,
                                       INPUT-OUTPUT ioPoint  AS INT64,
                                       INPUT-OUTPUT ioFooter AS CHARACTER
                                       ) FORWARD.

FUNCTION GetFmtByLev   RETURNS CHARACTER (INPUT iLevel AS INT64)  FORWARD.
FUNCTION GetExchByLev  RETURNS CHARACTER (INPUT iLevel AS INT64) FORWARD.
FUNCTION GetPntByLev   RETURNS INT64   (INPUT iLevel AS INT64) FORWARD.

{pfuncdef
   &DefLib="itax" 
   &Description="������ 䠩��� �襭�� � �����஢��/ࠧ�����஢�� ��⮢"}

/*----------------------------------------------------------------------------*/
PROCEDURE ReportTextKeep: /* -0161924-- � PacketText ��⮪�� ����� �� ��襬 */
   DEFINE INPUT        PARAMETER iPacketID   AS INT64  NO-UNDO.
   DEFINE INPUT        PARAMETER iText       AS CHAR     NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER pResult     AS CHAR     NO-UNDO.
   /* ... � ��襬 �� �६����� sysconf */
   RUN SaveDataProtocol IN h_base ("PacketText_" + STRING(iPacketID), iText).
   pResult = pResult + iText.
END PROCEDURE.

FUNCTION TAXGetFormat RETURNS LOGICAL (INPUT-OUTPUT ioFormat AS CHARACTER,
                                       INPUT-OUTPUT ioLevel  AS INT64,
                                       INPUT-OUTPUT ioPoint  AS INT64,
                                       INPUT-OUTPUT ioFooter AS CHARACTER
                                       ).
   DEFINE BUFFER   bCode FOR code.
   DEFINE BUFFER   Xattr FOR Xattr.

   DEFINE VARIABLE vOK AS LOGICAL     NO-UNDO.

   &IF DEFINED (IS-DEBUG) &THEN
      RUN dbgprint.p ("TAXGetFormat", ioFormat + " " + GetNullInt(ioLevel) + " " + GetNullInt(ioPoint)).    
   &ENDIF

   IF NOT {assigned ioFormat} THEN
      RETURN NO.

   ioFooter = GetXattrInit (ioFormat, "footer__").

   IF ioPoint EQ 0 THEN
   DO:
      CREATE ttLevel.
      ASSIGN 
         ioLevel        = ioLevel + 1
         ttLevel.Fmt    = ioFormat
         ttLevel.Level  = ioLevel
      .
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("TAXGetFormat","CREATE ttLevel:" + STRING (ttLevel.Level) + " " + ttLevel.Fmt + " " + STRING (ttLevel.Point)).    
      &ENDIF
   END.

   FIND FIRST bCode WHERE bCode.Class EQ "EXCH-MSG"
                      AND bCode.Code  EQ ioFormat
      NO-LOCK NO-ERROR.
   IF AVAIL bCode THEN
   DO:
      ttLevel.Kind = bCode.misc[1].
      vOK = YES.
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("TAXGetFormat","RESULT:" + ioFormat + " " + STRING (ioLevel) + " " + 
                                      STRING (ioPoint) + " " + ttLevel.Kind).    
      &ENDIF
   END.
   ELSE 
   DO:
      FIND FIRST Xattr WHERE  xattr.Class-Code EQ ioFormat
                         AND (xattr.Description EQ "OBJECT" OR xattr.Description EQ "SPOOL")
                         AND (xattr.order GT ioPoint)
         NO-LOCK NO-ERROR.
      IF AVAIL Xattr THEN
      DO:
         ASSIGN
            ttLevel.Point = IF xattr.Description EQ "SPOOL" /* SPOOL �㤥� ��ࠡ��뢠���� ���४⭮, */ 
                              THEN Xattr.order - 1          /* ⮫쪮 �᫨ �� ��᫥���� �����     */
                              ELSE Xattr.order
            ioPoint  = 0
            ioFormat = xattr.xattr-label
            mSpooling = (xattr.Description EQ "SPOOL")
         .
         vOK = TAXGetFormat (INPUT-OUTPUT ioFormat, 
                             INPUT-OUTPUT ioLevel, 
                             INPUT-OUTPUT ioPoint, 
                             INPUT-OUTPUT ioFooter).
      END.
      ELSE
      DO:
         vOK = NO.
         &IF DEFINED (IS-DEBUG) &THEN
            RUN dbgprint.p ("TAXGetFormat","NO RESULT").    
         &ENDIF
      END.
   END.

   RETURN vOK.

END FUNCTION.

FUNCTION GetFmtByLev RETURNS CHARACTER (INPUT iLevel AS INT64).
   FIND FIRST ttLevel WHERE ttLevel.Level EQ iLevel NO-LOCK NO-ERROR.
   IF AVAIL ttLevel 
      THEN RETURN ttLevel.Fmt.
      ELSE RETURN "".
END FUNCTION.

FUNCTION GetExchByLev RETURNS CHARACTER (INPUT iLevel AS INT64).
   FIND FIRST ttLevel WHERE ttLevel.Level EQ iLevel NO-LOCK NO-ERROR.
   IF AVAIL ttLevel 
      THEN 
      DO:
         RETURN ttLevel.Kind.
      END.
      ELSE RETURN "".
END FUNCTION.

FUNCTION GetPntByLev RETURNS INT64 (INPUT iLevel AS INT64).
   FIND FIRST ttLevel WHERE ttLevel.Level EQ iLevel NO-LOCK NO-ERROR.
   IF AVAIL ttLevel 
      THEN RETURN ttLevel.Point.
      ELSE RETURN ?.
END FUNCTION.

/*----------------------------------------------------------------------------*/
PROCEDURE TAXLoadExchFromFile.
   DEFINE INPUT  PARAMETER iFName  AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vSFlag     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vFormat    AS CHARACTER   NO-UNDO.  /* ⥪�騩 �ଠ� */
   DEFINE VARIABLE vLevel     AS INT64     NO-UNDO.
   DEFINE VARIABLE vPoint     AS INT64     NO-UNDO.
   DEFINE VARIABLE vFooter    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBuffer    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParentFot AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOK        AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vIgnore    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vProcImp   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vProcPrm   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hDataExch  AS HANDLE      NO-UNDO.

   DEFINE VAR hProc  AS handle    NO-UNDO.
   DEFINE VAR vInter AS CHAR      NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   IF RETRY THEN DO:
      IF vSFlag THEN
         INPUT STREAM sImport CLOSE.
   END.

   {empty ttLevel}
   {empty ttBlockAcct}

   vFormat = iFormat.

   INPUT STREAM sImport FROM VALUE(iFName).
   vSFlag = YES.
   vOK = TAXGetFormat (INPUT-OUTPUT vFormat,
                       INPUT-OUTPUT vLevel,
                       INPUT-OUTPUT vPoint,
                       INPUT-OUTPUT vFooter).
   mMess = "".
   vIgnore = NO.
   REPEAT:
      IMPORT STREAM sImport UNFORMATTED vBuffer.
      IF NOT {assigned vBuffer} THEN NEXT.

      IF vIgnore THEN NEXT.
      
      mMess = mMess + (if mMess ne "" then "~n" else "") + vBuffer.
      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("TAXFileImport","vFormat:" + vFormat + " vLevel:" + GetNullInt(vLevel) +
                                        " vBuffer:" + vBuffer + " vFooter:" + GetNullStr(vFooter) +
                                      " mSpooling:" + STRING (mSpooling)).    
      &ENDIF

      IF vBuffer EQ vFooter THEN
      DO:
         RUN GetClassMethod IN h_xclass (vFormat,
                                         "Import",
                                         "",
                                         "",
                                         OUTPUT vProcImp,
                                         OUTPUT vProcPrm).
         &IF DEFINED (IS-DEBUG) &THEN
            RUN dbgprint.p ("TAXFileImport","vFormat:" + GetNullStr(vFormat) + 
                                          " vProcImp:" + GetNullStr(vProcImp) + 
                                            " vLevel:" + GetNullInt(vLevel)).    
         &ENDIF
         
         IF {assigned vProcImp} AND vLevel GT 1 THEN
         DO:
            hDataExch = ObjectValueHandle (GetExchByLev(vLevel)) NO-ERROR.
            IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE (hDataExch) THEN
               UNDO MAIN, RETRY MAIN.
            {exch-run.i &Proc = vProcImp
                        &Parm = "vFormat, hDataExch"
                        &RunNoDef = YES}
         END.
         FIND FIRST ttLevel WHERE ttLevel.Level EQ vLevel
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR. {&ON-ERROR}
         DELETE ttLevel.
         vLevel  = vLevel - 1.
         vFormat = GetFmtByLev(vLevel).
         vPoint  = GetPntByLev(vLevel).
         vOK = TAXGetFormat (INPUT-OUTPUT vFormat, 
                             INPUT-OUTPUT vLevel, 
                             INPUT-OUTPUT vPoint,
                             INPUT-OUTPUT vFooter).

         IF vLevel EQ 0 THEN /* ���� ���稫�� */
            vIgnore = YES.

      END.
      ELSE
      DO:
         IF mSpooling THEN                      /* ����஫� ����砭�� ����७�� */
         DO:
            vParentFot = GetXAttrInit (GetFmtByLev(vLevel - 1), "footer__").
            IF vBuffer EQ vParentFot THEN       /* 墮�⮢�� த�⥫�᪮�� ����� */
            DO:
               vLevel = vLevel - 2.
               vFormat = GetFmtByLev(vLevel).
               vPoint  = GetPntByLev(vLevel).
               vOK = TAXGetFormat (INPUT-OUTPUT vFormat, 
                                   INPUT-OUTPUT vLevel, 
                                   INPUT-OUTPUT vPoint,
                                   INPUT-OUTPUT vFooter).
               mSpooling = NO. 
               NEXT.
            END.          
         END.  
         IF vLevel GT 1 THEN
         DO:
            hDataExch = ObjectValueHandle (GetExchByLev(vLevel)).
            &IF DEFINED (IS-DEBUG) &THEN
               RUN dbgprint.p ("itax.p","SET TAG " + vBuffer + " TO " + GetNullStr(hDataExch:NAME)).    
            &ENDIF            
            RUN TAXSetTag(hDataExch, vBuffer).          
         END.          
      END.

   END. /* REPEAT */
   INPUT STREAM sImport CLOSE.
   vFlagSet = YES.
END. /* MAIN */

{doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/

PROCEDURE TAXFileImport.
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL INIT ?  NO-UNDO.

   DEFINE VARIABLE hExch      AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vPacketID  AS INT64       NO-UNDO.
   DEFINE VARIABLE vFileID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vFormat    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vProcImp   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vProcPrm   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mRunProc   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSeanceID  AS INT64       NO-UNDO.

   DEFINE VAR hProc  AS handle    NO-UNDO.
   DEFINE VAR vInter AS CHAR      NO-UNDO.

   DEFINE BUFFER FileExch FOR FileExch.
   DEFINE BUFFER Packet   FOR Packet.


MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      hExch      = hIns:DEFAULT-BUFFER-HANDLE
      vFormat    = hExch:BUFFER-FIELD("mail-format"):BUFFER-VALUE
      vPacketID  = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      vFileID    = hExch:BUFFER-FIELD("FileExchID"):BUFFER-VALUE
      vSeanceID  = hExch:BUFFER-FIELD("SeanceID"):BUFFER-VALUE
      mRunProc   = TRNSettingValue("","Run-Proc","")
      mReqTrans  = TRNSettingValue("","ReqTrans","")
      mAnsTrans  = TRNSettingValue("","AnsTrans","")
      mCount     = 0
      mCountProc = 0
      mSummVZ    = 0
   NO-ERROR. {&ON-ERROR}

   FIND FIRST Seance WHERE Seance.SeanceID EQ vSeanceID NO-LOCK NO-ERROR.
   FIND FIRST FileExch WHERE FileExch.FileExchID EQ vFileID
      NO-LOCK NO-ERROR. {&ON-ERROR}

   IF     {assigned mRunProc}
      AND mRunProc NE {&RET-ERROR}
          THEN RUN VALUE(mRunProc + ".p") (FileExch.Path) NO-ERROR.

   RUN Fill-SysMes IN h_tmess ("","","1","����������� 䠩� " + FileExch.Name).

   mFile1 = FileExch.Name.

/*----------------------------------------------------------------------------*/   
   RUN TAXLoadExchFromFile(FileExch.Path, vFormat).

/*----------------------------------------------------------------------------*/
   DO TRANSACTION:
      FOR FIRST Packet WHERE Packet.PacketID EQ vPacketID EXCLUSIVE-LOCK:
         ASSIGN
            Packet.Kind  = hExch:NAME
            Packet.State = "����".
      END.
   END.

   ASSIGN
      vFormat = hExch::mail-format.
   RUN GetClassMethod IN h_xclass (vFormat,
                                   "Import",
                                   "",
                                   "",
                                   OUTPUT vProcImp,
                                   OUTPUT vProcPrm).
   IF {assigned vProcImp} THEN
   DO:
      {exch-run.i &Proc = vProcImp
                  &Parm = "vFormat, hExch"
                  &RunNoDef = YES}
   END.

   RUN FileMoveArchive IN h_filex (vPacketID, "ImpArch") NO-ERROR.

   vFlagSet = YES.
END.

{doreturn.i vFlagSet}
END PROCEDURE.


PROCEDURE TAXSetTag.
   DEFINE INPUT  PARAMETER hExch   AS HANDLE      NO-UNDO.
   DEFINE INPUT  PARAMETER iBuffer AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE hFld AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vFld AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVal AS CHARACTER   NO-UNDO.

   ASSIGN
      mTagCount = mTagCount + 1
      vFld = GetEntries (1, iBuffer, ":", "")
      vVal = TRIM (SUBSTR(iBuffer, INDEX(iBuffer, ":") + 1)).
      
   hFld = hExch:BUFFER-FIELD(GetMangledName(vFld)) NO-ERROR.
   IF NOT VALID-HANDLE (hFld) THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "0", "�訡�� �ଠ�. �� ������ �� " + vFld + " � ����� " + hExch:NAME).
      RETURN.
   END.

   RUN SetValue IN h_exch (hExch,
                           vFld,
                           vVal,
                           mTagCount) NO-ERROR.

END PROCEDURE.

PROCEDURE ClearExch.
   DEFINE INPUT  PARAMETER hExch AS HANDLE      NO-UNDO.
   DEFINE INPUT  PARAMETER iExc  AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE hFld AS HANDLE      NO-UNDO.

   FOR EACH Xattr WHERE Xattr.Class-Code   EQ hExch:NAME
                    AND Xattr.sign-inherit EQ "�"
      NO-LOCK:
      IF CAN-DO(iExc, Xattr.Xattr-Code) THEN
         NEXT.
      hFld = hExch:BUFFER-FIELD(GetMangledName (Xattr.Xattr-Code)).
      CASE Xattr.Data-Type:
         WHEN "CHARACTER" THEN
            hFld:BUFFER-VALUE = "".
         WHEN "INTEGER" THEN
            hFld:BUFFER-VALUE = 0.
         WHEN "DECIMAL" OR
         WHEN "DATE" THEN
            hFld:BUFFER-VALUE = ?.
      END CASE.
   END.
END PROCEDURE.

PROCEDURE TAXConfirmCreate.
   DEFINE INPUT  PARAMETER iPacketID AS INT64       NO-UNDO.
   DEFINE INPUT  PARAMETER iErrCode  AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iErrText  AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iDateTime AS DATETIME    NO-UNDO.

   DEFINE VARIABLE vSeanceID AS INT64       NO-UNDO.
   DEFINE VARIABLE vPacketID AS INT64       NO-UNDO.
   DEFINE VARIABLE vText     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vFlagSet AS LOGICAL     NO-UNDO    INIT ?.

   DEFINE BUFFER Packet    FOR Packet.
   DEFINE BUFFER bPack     FOR Packet.
   DEFINE BUFFER Seance    FOR Seance.
   DEFINE BUFFER mail-user FOR mail-user.
   DEFINE BUFFER FileExch  FOR FileExch.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   FIND FIRST Packet WHERE Packet.PacketID EQ iPacketID
      NO-LOCK NO-ERROR. {&ON-ERROR}
   FIND FIRST Seance WHERE Seance.SeanceID EQ Packet.SeanceID
      NO-LOCK NO-ERROR. {&ON-ERROR}
   FIND FIRST mail-user WHERE mail-user.op-kind-exp EQ Seance.op-kind
                          AND mail-user.filial-id   EQ ShFilial
      NO-LOCK NO-ERROR. {&ON-ERROR}

   FIND FIRST FileExch WHERE FileExch.FileExchID EQ Packet.FileExchID
      NO-LOCK NO-ERROR. {&ON-ERROR}
   RUN PacketCreate IN h_pack (Seance.SeanceID,
                               -1,
                               mail-user.mail-user-num,
                               Packet.Kind,
                               OUTPUT vPacketID). {&ON-ERROR}
   IF NOT UpdateSigns("Packet",
                      string(vPacketID),
                      "FileName",
                      "PB" + 
                       (IF NOT {assigned iErrCode} OR iErrCode EQ "10" THEN "1"
                        ELSE "2") +  "_" +
                       ENTRY(1, GetPureName(FileExch.Name), ".") + ".txt",
                      no) THEN
      UNDO MAIN, RETRY MAIN.
   RUN PacketCreateLink (iPacketID,
                         "Packet",
                          vPacketID,
                         "TTAXConf"). {&ON-ERROR}

   iErrText = TRIM(REPLACE(REPLACE(iErrText, "~r", " "), "~n", " ")).

   vText = ENTRY(1, GetPureName(FileExch.Name), ".") + "###~n" + 
           (IF NOT {assigned iErrCode} OR iErrCode EQ "10" 
            THEN "10" 
            ELSE iErrCode + ";" + iErrText) + 
           "@@@~n"  +
           ISO-DATE(DATE(iDateTime)) + "@@@~n" +
           SUBSTR (STRING(iDateTime), 12, 8) + "@@@~n" +
           "===~n".
      RUN PacketTextSave (vPacketID, vText).
   vFlagSet = YES.
END.
{doreturn.i vFlagSet}

END PROCEDURE.
/*----------------------------------------------------------------------------*/

PROCEDURE TAXAcctBlkFileImport.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch    AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vInfoType  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hDataExch  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vBIK       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBankMFO   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSeanceID  AS INT64       NO-UNDO.
   DEFINE VARIABLE vBlkType   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUserSel   AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vStrTMP    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPacketID  AS INT64       NO-UNDO.
   DEFINE VARIABLE vErrCode   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrClass  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrText   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNaimNP    AS CHARACTER   NO-UNDO     EXTENT 5.
   DEFINE VARIABLE vI         AS INT64       NO-UNDO.

   DEFINE VARIABLE vFlagSet   AS LOGICAL     NO-UNDO     INIT ?.

   DEFINE BUFFER Seance FOR Seance.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      vInfoType  = hExch:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE
      vSeanceID  = hExch:BUFFER-FIELD("SeanceID"):BUFFER-VALUE
      vPacketID  = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      hDataExch  = ObjectValueHandle ("ETAXResolutn")
      vBIK       = hDataExch:BUFFER-FIELD(GetMangledName("���")):BUFFER-VALUE
      vBlkType   = TRNSettingValue("","BlockType","�����㬬")
      vStrTMP    = TRNSettingValue("", "UserSelType","��")
      vErrClass  = TRNSettingValue("", "TaxErrClass", "")
      vUserSel   = (vStrTMP EQ "��") OR (vStrTMP EQ "YES")
   .
   FIND FIRST Seance WHERE Seance.SeanceID EQ vSeanceID NO-LOCK NO-ERROR.
   IF mSummVZ GT 0 THEN
      vBlkType = "�����㬬".
   ELSE
      vBlkType = "������".
 
   IF {assigned vErrClass} THEN DO:
      RUN TAXBlockAcctCheck(vErrClass, hDataExch, TABLE ttBlockAcct, OUTPUT vErrCode, OUTPUT vErrText).
      RUN Fill-SysMes IN h_tmess ("", "", "1", "������� �஢�ન ������: ��� " + vErrCode + ";" + vErrText).
   END.
   RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "ReqType", 
                                      "NOT").
   IF vErrCode EQ "" OR             /* �஢�ઠ �� �믮��﫠�� */
      vErrCode EQ "10" THEN DO:     /* �஢�ઠ ��諠 �ᯥ譮 */
      FOR EACH ttBlockAcct WHERE NOT {assigned ttBlockAcct.error-code} NO-LOCK:
          CASE vInfoType:
              WHEN "�����������" THEN
                  RUN TAXBlockAcct(hDataExch,
                                   BUFFER ttBlockAcct,
                                   vBlkType,
                                   vUserSel)
                  NO-ERROR.
              WHEN "�����������" THEN
                  RUN TAXUnblockAcct(hDataExch,
                                     BUFFER ttBlockAcct)
                  NO-ERROR.
          END.
      END.
      IF {assigned vErrClass} AND
         CAN-FIND(FIRST ttBlockAcct WHERE NOT {assigned ttBlockAcct.error-code})
      THEN DO:
          mSuperPacketID = vPacketID.
          CASE vInfoType:
              WHEN {&INFO-TYPE-RPO} THEN DO:
                  RUN CreateBOSExch_RPO(hDataExch) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN DO:
                      ASSIGN
                          vErrCode = "35"
                          vErrText = RETURN-VALUE
                      .
                      RUN AddAttr2TableEx IN h_trans ("",
                                                      0,
                                                      -1,
                                                      "",
                                                      0,
                                                      "ReqType",
                                                      "").
                  END.
                  ELSE IF FGetSetting("����ன��_365�", "����1", "���") NE "�⪫" THEN
                         RUN TAXConfirmCreate(vPacketID, "10", "", NOW).
              END.
              WHEN {&INFO-TYPE-ROO} THEN DO:
                  IF FGetSetting("����ன��_365�", "����1", "���") NE "�⪫" THEN
                     RUN TAXConfirmCreate(vPacketID, "10", "", NOW).
              END.
          END.
      END.
   END.
   IF {assigned vErrClass} THEN DO:
      IF NOT {assigned vErrCode} OR vErrCode = "10" THEN
          RUN TAXBlockAcctError(TABLE ttBlockAcct,
                                OUTPUT vErrCode,
                                OUTPUT vErrText).
      IF {assigned vErrCode} AND vErrCode <> "10" THEN DO:
         RUN TAXConfirmCreate(vPacketID, vErrCode, vErrText, NOW).
         IF FGetSetting("����ன��_365�", "����1", "���") = "��" THEN
            RUN TAXConfirmCreate(vPacketID, "10", "", NOW).

      END.
   END.

   /* -0161924-- ��⮪�� ��襬 �� �६����� sysconf */
   RUN PacketTextClear IN h_pack (vPacketID).
   mResult = "".
   RUN GetCharAttrValue365p(hDataExch, "������", OUTPUT vNaimNP[1]) NO-ERROR.
   IF ERROR-STATUS:ERROR OR NOT {assigned vNaimNP[1]} THEN
       RUN GetCharAttrValue365p(hDataExch, "�����", OUTPUT vNaimNP[1]) NO-ERROR.
   vNaimNP[1] = GetNullStr(vNaimNP[1]).
   {wordwrap.i 
      &s = vNaimNP
      &n = 5
      &l = 50 }
   RUN ReportTextKeep (vPacketID, 
                                 GetNullDat(Seance.SeanceDate) + " " + GetNullStr(STRING(Seance.SeanceTime,"HH:MM:SS")) + "~n",
                                 INPUT-OUTPUT mResult).
   RUN ReportTextKeep (vPacketID, 
                                 fStrPad(GetNullStr(hDataExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE),12,YES) + " " +
                                 fStrPad(vNaimNP[1],50,YES) + " " +
                                 fStrPad(GetNullStr(hDataExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE),4,YES) + " " +
                                 fStrPad(GetNullStr(hDataExch:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE),10,YES) + " " +
                                 fStrPad(GetNullStr(STRING (hDataExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE)),10,YES) + " " +
                                 fStrPad(GetNullStr(hExch:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE),15,YES) + " " +
                                 fStrPad(GetNullNum(mSummVZ),20,NO) + " " +
                                 fStrPad(GetNullStr(hDataExch:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE),10,YES) + " " +
                                 fStrPad(GetNullStr(STRING (hDataExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE)),10,YES) + " " +
                                 fStrPad(GetNullInt(mCount),10,NO) + "~n",
                                 INPUT-OUTPUT mResult).
   DO vI = 2 TO 5 :
      IF {assigned vNaimNP[vI]} THEN
         RUN ReportTextKeep (vPacketID, FILL(" ", 13) + vNaimNP[vI] + "~n", INPUT-OUTPUT mResult).
   END.
   RUN ReportTextKeep (vPacketID, "~n", INPUT-OUTPUT mResult).
   FOR EACH ttBlockAcct NO-LOCK:
      RUN ReportTextKeep (vPacketID, GetNullStr(DelFilFromAcct(ttBlockAcct.acct)) + "    " +
                                               ttBlockAcct.msg + "~n", INPUT-OUTPUT mResult).
   END.
   RUN ReportTextKeep (vPacketID, "��ࠡ�⠭� ��⮢: " + GetNullInt(mCountProc), INPUT-OUTPUT mResult).
   /* -0161924-- ᮤ�ন��� 䠩�� ������ ��襬 � PacketText.Contents */
   RUN PacketTextSave IN h_pack (vPacketID, mMess).
   
   /* ���⪠ �� */
   RUN ClearExch (hExch,     "ExchMain,mail-format").
   RUN ClearExch (hDataExch, "ExchMain").

   vFlagSet = YES.

END.     /* MAIN */
{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE TAXAcctBlkImport.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch    AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vBegDate    AS DATE        NO-UNDO. /* ��� ��砫� �����       */
   DEFINE VARIABLE vFindAcct   AS LOG         NO-UNDO INIT   NO. /* ��� ������       */
   DEFINE VARIABLE vCur        AS CHAR        NO-UNDO. 
   DEFINE VARIABLE hFile       AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hMain       AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vPacketID   AS INT64     NO-UNDO.
   DEFINE VARIABLE vAcct       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vInfoType   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCreateLink AS LOGICAL     NO-UNDO.
   
   DEFINE BUFFER bAcct FOR acct.

   ASSIGN
      hMain = ObjectValueHandle (hExch:BUFFER-FIELD("ExchMain"):BUFFER-VALUE)
      hFile = ObjectValueHandle (hMain:BUFFER-FIELD("ExchMain"):BUFFER-VALUE)
      vPacketID = hFile:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      vInfoType = hFile:BUFFER-FIELD("tipinf$"):BUFFER-VALUE
      vAcct     = hExch:BUFFER-FIELD("nomswc$"):BUFFER-VALUE
      vCreateLink = LOGICAL(TRNSettingValue("", "CreateAcctLink", "YES"))
      vBegDate    = DATE(hMain:BUFFER-FIELD(GetMangledName("��⠍��")):BUFFER-VALUE)
   NO-ERROR.

   RUN SetMFMode365p IN THIS-PROCEDURE (hMain).
   RUN FindAcct365p IN THIS-PROCEDURE (BUFFER bAcct, vAcct) NO-ERROR.

   IF AVAIL bAcct THEN DO:
      IF vCreateLink THEN DO:
          vAcct = AddFilToAcct(vAcct, shFilial).
          IF NOT CAN-FIND(FIRST ttBlockAcct WHERE
                              ttBlockAcct.acct = vAcct)
          THEN
              RUN PacketCreateLink IN h_pack (vPacketID,
                                              "acct",
                                              bAcct.acct + "," + bAcct.currency,
                                              vInfoType).
      END.
      ASSIGN
         vAcct     = AddFilToAcct (vAcct, shFilial)
         vCur      = bAcct.currency
         vFindAcct = YES
      .
   END.
   ELSE
   DO: 
      &IF DEFINED(ExtBase) &THEN
      IF RunExtQuery(vBegDate) THEN
      DO:
         /* ����� ����砥� ��ਮ� �� ���� ��砫쭮�� �襭��       */
         vFindAcct = IsAcctEists(vAcct, OUTPUT vCur).
      END. /* IF     IsLessDateNR(vBegDate) */

      IF vFindAcct NE YES THEN
      &ENDIF
      RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������ ��� " + vAcct + ". ���������� ��⠭����� ���.").
   END.
   mCount = mCount + 1.

/* mkv �᫨ ���� ४����� � ��������� */
   CREATE ttBlockAcct.
   ASSIGN
      ttBlockAcct.num        = mCount
      ttBlockAcct.acct       = vAcct
      ttBlockAcct.currency   = IF vFindAcct THEN vCur ELSE ""
      ttBlockAcct.cont-name  = hExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE
      ttBlockAcct.msg        = IF NOT vFindAcct THEN "��� �� ������." ELSE ""
      ttBlockAcct.error-code = IF NOT vFindAcct THEN "32" ELSE ""
      ttBlockAcct.open-date  = hExch:BUFFER-FIELD(GetMangledName("��⠎����")):BUFFER-VALUE
      ttBlockAcct.close-date = hExch:BUFFER-FIELD(GetMangledName("��⠇�����")):BUFFER-VALUE
   NO-ERROR.
   VALIDATE ttBlockAcct.

   /* ���⪠ �� - ����室��� �� ����㧪� ��᪮�쪨� ��⮢, �.�. �ᯮ������ ⮫쪮 ���� ���⠭� */
   ASSIGN
      hExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE = ""
      hExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE = ""
   NO-ERROR.

END PROCEDURE.


PROCEDURE TAXResoltnImport.
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch    AS HANDLE      NO-UNDO.

   DEFINE VARIABLE hFile     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vPacketID AS INT64     NO-UNDO.

   ASSIGN
      hFile       = ObjectValueHandle (hExch:BUFFER-FIELD("ExchMain"):BUFFER-VALUE)
      vPacketID   = hFile:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      mSummVZ     = hExch:BUFFER-FIELD(GetMangledName("�㬂���")):BUFFER-VALUE
   .
   IF mSummVZ EQ ? 
      THEN mSummVZ = 0.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* ��楤��� �����஢��/ࠧ�����஢�� ��⮢ (103135)                         */
/*----------------------------------------------------------------------------*/

PROCEDURE TAXBlockAcct.
   DEFINE INPUT  PARAMETER hRExch   AS HANDLE      NO-UNDO.  /* ��뫪� �� �६. ⠡���� ����� ETAXResolutn */
   DEFINE        PARAMETER BUFFER   ttBlockAcct    FOR ttBlockAcct.
   DEFINE INPUT  PARAMETER iBlkType AS CHARACTER   NO-UNDO. /*  ��� �����஢��  */
   DEFINE INPUT  PARAMETER iUserSel AS LOGICAL     NO-UNDO.

   DEFINE VARIABLE vQuant AS INT64   NO-UNDO.
   DEFINE VARIABLE vAddr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vINN   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKPP   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vType  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcct  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBegPeriod AS DATETIME NO-UNDO.
   DEFINE VARIABLE vEndPeriod AS DATETIME NO-UNDO.
   DEFINE VARIABLE vStr       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPriority  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustTable AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vContrOrg AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vBegDateBl AS CHARACTER NO-UNDO.   
   DEFINE VARIABLE vTmpDT     AS DATETIME  NO-UNDO.

   &IF DEFINED(IS-DEBUG) > 0 &THEN
   RUN dbgprint.p ("TAXBlockAcct", "iBlkType:" + GetNullStr(iBlkType)).
   &ENDIF

   IF NOT (AVAILABLE ttBlockAcct AND VALID-HANDLE(hRExch)) THEN
       RETURN.

   ASSIGN
       vQuant    = 0
       vContrOrg = (TRIM(TRNSettingValue("", "ContrOrg", "")) = "���")
       vPriority = GetCode("acct-status", iBlkType)
   .

   IF INDEX(vPriority, "�珫��(") > 0 THEN
       ASSIGN
           vPriority = SUBSTRING(vPriority,  INDEX(vPriority, "�珫��(") + 7)
           vPriority = SUBSTRING(vPriority, 1, INDEX(vPriority, ")") - 1)
           vPriority = TRIM(vPriority)
       .

   FIND FIRST acct WHERE
       acct.acct     = ttBlockAcct.acct AND
       acct.currency = ttBlockAcct.currency
   NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN DO:
      IF (m365_Join EQ "�" AND
                 (    CAN-DO(m365_BalAcct, STRING(acct.acct)) 
                  AND CAN-DO(m365_Contract, acct.contract)))
         OR (m365_Join EQ "���" AND
                 (    CAN-DO(m365_BalAcct, STRING(acct.acct)) 
                   OR CAN-DO(m365_Contract, acct.contract)))
       THEN DO:
          ASSIGN
              ttBlockAcct.error-code = "35"
              ttBlockAcct.msg        = "��� "
                                     + STRING(acct.acct, GetAcctFmt(acct.acct-cat)) 
                                     + " ���� "
                                     + QUOTER(acct.contract)
                                     + ". �����஢�� �� ��ࠡ�⠭�."
          .
          RETURN.
       END.
       IF acct.close-date <> ?
          AND
          acct.close-date <= gend-date
          AND
          FGetSetting("����ன��_365�", "�����������", "���") <> "��"
       THEN DO:
           ASSIGN
               ttBlockAcct.error-code = "35"
               ttBlockAcct.msg        = "��� ������ �� ���� " +
                                        date2str(gend-date)    +
                                        ". �����஢�� �� ��ࠡ�⠭�."
           .
           RETURN.
       END.
       RUN CheckBankrupcy365p(acct.cust-cat,
                              acct.cust-id,
                              gend-date,
                              OUTPUT ttBlockAcct.error-code,
                              OUTPUT ttBlockAcct.msg).
       IF ttBlockAcct.error-code <> "10" THEN
           RETURN.

      GetCliName (acct.cust-cat,
                  STRING(acct.cust-id),
                  OUTPUT       vAddr,
                  OUTPUT       vINN ,
                  OUTPUT       vKPP ,
                  INPUT-OUTPUT vType,
                  OUTPUT       vCode,
                  OUTPUT       vAcct).

      IF vINN NE hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE THEN
      DO:
         ttBlockAcct.error-code = "33".
         ttBlockAcct.msg = "��ᮢ������� ��� ������ ��� "  + vINN +
            " � ��� � 䠩�� ������ " + hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE + 
            ". �����஢�� �� ��ࠡ�⠭�.".
         RETURN.
      END.
      vBegPeriod = DATE(hRExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE).
      vEndPeriod = gend-date + 1.
      vEndPeriod = vEndPeriod - 1.

      vStr  =  hRExch:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE
               + ";"
               + hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE.
      FOR EACH BlockObject WHERE BlockObject.class-code   EQ "blockacct"
                             AND BlockObject.file-name    EQ "acct"
                             AND BlockObject.surrogate    EQ ttBlockAcct.acct + "," + ttBlockAcct.currency
                             AND BlockObject.block-type   EQ iBlkType
                             AND BlockObject.txt[4]       EQ vStr
      NO-LOCK:
         /*
            0200763: �᫮��� ��� ������ ��砫� ����⢨� �����஢��
            ��७�ᥭ� ������ 横��. � ��⨢��� ��砥 �� ��������
            ������ OE DataServer �������� �訡�� (Memory Violation).
         */
         IF BlockObject.beg-datetime >= vBegPeriod AND
            BlockObject.beg-datetime <= vEndPeriod
         THEN
            LEAVE.
      END.

      IF NOT AVAIL BlockObject THEN
      DO:
         vBegDateBl = TRNSettingValue("ExchSET-TAX", "BegDateBl","���थ��").
         IF vBegDateBl EQ "�� 䠩��" AND 
            DATE(vBegPeriod) < gend-date
         THEN DO:
            ttBlockAcct.error-code = "35".
            ttBlockAcct.msg = "��������! ������ �����஢�� �� ��襤��� ���� " + STRING(vBegPeriod) + " ����������.".
             MESSAGE ttBlockAcct.msg VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN.
         END.
         ELSE DO:
            CREATE BlockObject.
            ASSIGN
               BlockObject.file-name     = "acct"
               BlockObject.surrogate     = ttBlockAcct.acct + "," + ttBlockAcct.currency
               BlockObject.block-type    = iBlkType
               BlockObject.class-code    = "blockacct"
               BlockObject.user-id       = IF iUserSel THEN USERID("bisquit") ELSE acct.user-id
               BlockObject.txt[1]        = vPriority
               BlockObject.txt[3]        = "��襭��"
               BlockObject.txt[5]        = ""
               BlockObject.txt[6]        = ""
               BlockObject.txt[7]        = ""
               BlockObject.txt[8]        = "������"
               BlockObject.txt[9]        = ""
               BlockObject.txt[10]       = ""
               BlockObject.val[1]        = 0.0
               BlockObject.val[2]        = 0.0
               BlockObject.val[4]        = 0.0 
               BlockObject.val[5]        = 0.0 
               BlockObject.val[6]        = 0.0 
               BlockObject.val[7]        = 0.0 
               BlockObject.val[8]        = 0.0 
               BlockObject.val[9]        = 0.0 
               BlockObject.val[10]       = 0.0 
            .
            BlockObject.txt[2]        = IF vContrOrg THEN hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE ELSE "����".
            IF NOT {assigned vBegDateBl} OR vBegDateBl EQ "���थ��" THEN
                vTmpDT = DATETIME(gend-date,mtime).
            ELSE IF vBegDateBl EQ "�� 䠩��" THEN DO:
                vTmpDT = vBegPeriod.
            END. 
            ELSE IF vBegDateBl = "���蠭��" THEN
                vTmpDT = IF DATE(vBegPeriod) <= gend-date THEN
                             DATETIME(gend-date, MTIME)
                         ELSE
                             vBegPeriod.
            RUN SetBlockObjectBegDT(BUFFER BlockObject, vTmpDT) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE BlockObject.
                ASSIGN
                    ttBlockAcct.error-code = "35"
                    ttBlockAcct.msg        = "���������� ᮧ���� �����஢�� " +
                                             "⨯� " + BlockObject.block-type +
                                             " � ��⮩ ��砫� "               +
                                             STRING(DATE(vTmpDT))             +
                                             ". ���⨣��� ���ᨬ��쭮� "     +
                                             "������⢮ �����஢�� ������� " +
                                             "���� �� 㪠������ ����"
                .
                RETURN.
            END.
            BlockObject.txt[4]        = hRExch:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE + ";" + hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE.

            IF ttBlockAcct.currency EQ "" THEN
               BlockObject.val[3]        = (-1) * mSummVZ.
            ELSE DO:
               BlockObject.val[3] = (-1) * CurFromBase("�������", ttBlockAcct.currency, gend-date, mSummVZ).
               IF BlockObject.val[3] = 0 AND
                  m365_RValBlk = "��"    AND
                  iBlkType = "�����㬬"   THEN
                    BlockObject.val[3] = (-1) * 0.01.
            END.

            BlockObject.val[4] = (-1) * mSummVZ.
            UpdateSigns("blockacct",
                        STRING(BlockObject.BlockObjectID),
                        "��⠐���",
                        STRING(hRExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE,"99/99/9999"),
                        ?).
            UpdateSigns("acct",
                        ttBlockAcct.acct + "," + ttBlockAcct.currency,
                        "������",
                        "����",
                        isXAttrIndexed(acct.class-code, "������")).
            vQuant = vQuant + 1.
            mCountProc = mCountProc + 1.
            ttBlockAcct.error-code = "".
            ttBlockAcct.msg = "��� �����஢�� c " + STRING(BlockObject.beg-datetime).
            RELEASE BlockObject.

            IF      acct.cust-cat = "�" THEN vCustTable = "cust-corp".
            ELSE IF acct.cust-cat = "�" THEN vCustTable = "person".
            IF {assigned vCustTable} THEN 
               UpdateSigns(vCustTable,
                           STRING(acct.cust-id),
                           "���������",
                           "��",
                           ?).             
         END.                            
      END.
      ELSE
         ASSIGN
            ttBlockAcct.error-code = "35"
            ttBlockAcct.msg        = "�����஢�� 㦥 ��⠭������."
         .
   END.

END PROCEDURE.

PROCEDURE TAXUnblockAcct.
   DEFINE INPUT  PARAMETER hRExch   AS HANDLE      NO-UNDO.  /* ��뫪� �� �६. ⠡���� ����� ETAXResolutn */
   DEFINE        PARAMETER BUFFER   ttBlockAcct    FOR ttBlockAcct.

   DEFINE VARIABLE vQuant   AS INT64   NO-UNDO.
   DEFINE VARIABLE vAddr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vINN     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKPP     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vType    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcct    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDate    AS DATE      NO-UNDO .
   DEFINE VARIABLE vPrDate  AS DATETIME  NO-UNDO .
   DEFINE VARIABLE vStr     AS CHARACTER NO-UNDO .
   DEFINE VARIABLE vIsBlck    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vCustTable AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vContrOrg  AS LOGICAL     NO-UNDO.
   DEFINE BUFFER bAcct FOR acct.

   &IF DEFINED(IS-DEBUG) > 0 &THEN
   RUN dbgprint.p ("TAXUnblockAcct", "START").
   &ENDIF
   
   IF NOT (AVAILABLE ttBlockAcct AND VALID-HANDLE(hRExch)) THEN
       RETURN.

   ASSIGN
       vQuant    = 0
       vContrOrg = (TRIM(TRNSettingValue("", "ContrOrg", "")) = "���")
   .

   FIND FIRST acct WHERE
       acct.acct     = ttBlockAcct.acct AND
       acct.currency = ttBlockAcct.currency
   NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN DO:
      IF (m365_Join EQ "�" AND
                 (    CAN-DO(m365_BalAcct, STRING(acct.acct)) 
                  AND CAN-DO(m365_Contract, acct.contract)))
         OR (m365_Join EQ "���" AND
                 (    CAN-DO(m365_BalAcct, STRING(acct.acct)) 
                   OR CAN-DO(m365_Contract, acct.contract)))
       THEN DO:
          ASSIGN
              ttBlockAcct.error-code = "35"
              ttBlockAcct.msg        = "��� "
                                       + STRING(acct.acct, GetAcctFmt(acct.acct-cat)) +
                                       " ���� "
                                       + QUOTER(acct.contract) +
                                       ". �����஢�� �� ��ࠡ�⠭�."
          .
          RETURN.
       END.

       IF acct.close-date <> ?
          AND
          acct.close-date <= gend-date
          AND
          FGetSetting("����ன��_365�", "�����������", "���") <> "��"
          AND
          FGetSetting("����ன��_365�", "�⬁�������", "���") <> "��"
       THEN DO:
           ASSIGN
               ttBlockAcct.error-code = "35"
               ttBlockAcct.msg        = "��� ������ �� ���� " +
                                        date2str(gend-date)    +
                                        ". �����஢�� �� ��ࠡ�⠭�."
           .
           RETURN.
       END.

      GetCliName (acct.cust-cat,
                  STRING(acct.cust-id),
                  OUTPUT       vAddr,
                  OUTPUT       vINN ,
                  OUTPUT       vKPP ,
                  INPUT-OUTPUT vType,
                  OUTPUT       vCode,
                  OUTPUT       vAcct).

      IF vINN NE hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE THEN
      DO:
         ttBlockAcct.error-code = "33".
         ttBlockAcct.msg = "��ᮢ������� ��� ������ ��� "  + vINN +
            " � ��� � 䠩�� ������ " + hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE + 
            ". �����஢�� �� ��ࠡ�⠭�.".
         RETURN.
      END.

      vPrDate = hRExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE .
      vStr  =  trim(hRExch:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE).

      FOR EACH BlockObject WHERE BlockObject.class-code   EQ "blockacct"
                               AND BlockObject.file-name    EQ "acct"
                               AND BlockObject.surrogate    EQ ttBlockAcct.acct + "," + ttBlockAcct.currency
                               AND ENTRY(1, BlockObject.txt[4], ";") EQ vStr
      NO-LOCK:
         vDate = DATE(GetXattrValueEx("BlockObject",STRING(BlockObject.BlockObjectID),"��⠐���","")) NO-ERROR.
         IF vDate NE ? AND vDate EQ DATE(hRExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE) THEN
         LEAVE.
      END.

      IF NOT AVAILABLE BlockObject THEN DO:
         ttBlockAcct.error-code = "35".
         ttBlockAcct.msg = "�� ������� �����஢�� ��� ���. " +
                           "�����஢�� �� ��ࠡ�⠭�."         + "~n" + 
                           "��襭�� � �ਮ�⠭������� " + 
            hRExch:BUFFER-FIELD(GetMangledName("�������" )):BUFFER-VALUE + " �� " + 
            hRExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE +
                           " ��襭�� �� �⬥�� " +        
            hRExch:BUFFER-FIELD(GetMangledName("�������" )):BUFFER-VALUE + " �� " + 
            hRExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE.
         RETURN.
      END.

      IF BlockObject.END-datetime NE ? THEN DO:
         ttBlockAcct.error-code = "35".
         ttBlockAcct.msg = "�����஢�� �� ���� 㦥 ������.".
         RETURN.
      END.

      vDate = hRExch:BUFFER-FIELD(GetMangledName("��⠐���")):BUFFER-VALUE .
      DO TRANSACTION:
         FIND CURRENT BlockObject EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         ASSIGN
            BlockObject.END-datetime  = DATETIME(gend-date,mtime)
            BlockObject.txt[6]        = "��襭��"
         .
         BlockObject.txt[5]        = IF vContrOrg THEN  hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE ELSE "����".
         BlockObject.txt[7]        = hRExch:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE + ";" + hRExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE.

         UpdateSigns("blockacct",
                     STRING(BlockObject.BlockObjectID),
                     "��⠐���",
                     STRING(vDate,"99/99/9999"),
                     ?).

         UpdateSigns("acct",
                     ttBlockAcct.acct + "," + ttBlockAcct.currency,
                     "������",
                     "�������",
                     isXAttrIndexed(acct.class-code, "������")).
      END.
      vQuant = vQuant + 1.
      mCountProc = mCountProc + 1.
      ttBlockAcct.error-code = "".
      ttBlockAcct.msg = "��� ࠧ�����஢�� � " + STRING(BlockObject.end-datetime).

      vIsBlck = NO.
      FOR EACH bAcct WHERE
               bAcct.cust-cat EQ acct.cust-cat 
           AND bAcct.cust-id  EQ acct.cust-id
               NO-LOCK,
          EACH BlockObject WHERE BlockObject.class-code   EQ "blockacct"
                             AND BlockObject.file-name    EQ "acct"
                             AND BlockObject.surrogate    EQ bAcct.acct + "," + bAcct.currency
      NO-LOCK:
         /*
            0200763: �᫮��� ��� ������ ����砭�� ����⢨� �����஢��
            ��७�ᥭ� ������ 横��. � ��⨢��� ��砥 �� ��������
            ������ OE DataServer �������� �訡�� (Memory Violation).
         */
         IF BlockObject.end-datetime  = ? OR
            BlockObject.end-datetime >= DATETIME(gend-date, MTIME)
         THEN DO:
            vIsBlck = YES.
            LEAVE.
         END.
      END.
      IF NOT vIsBlck THEN DO:

         IF      acct.cust-cat = "�" THEN vCustTable = "cust-corp".
         ELSE IF acct.cust-cat = "�" THEN vCustTable = "person".
         IF {assigned vCustTable} THEN 
            UpdateSigns(vCustTable,
                        STRING(acct.cust-id),
                        "���������",
                        "���",
                        ?).

      END. 
   END.

END PROCEDURE.

PROCEDURE TAXBlockAcctCheck.
    DEFINE INPUT  PARAMETER iErrClass AS  CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iHExch    AS  HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER TABLE     FOR ttBlockAcct.
    DEFINE OUTPUT PARAMETER oErrCode  AS  CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrText  AS  CHARACTER NO-UNDO.

    DEFINE VARIABLE vBIK       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vBankMFO   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vNaimNP    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vFIOIP     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vINNNP     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vKPPNP     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCustName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCustName2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vINN       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vKPP       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCheck33   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCorpNameCorrect AS LOGICAL NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    IF NOT VALID-HANDLE(iHExch) THEN DO:
        ASSIGN
            oErrCode = "35"
            oErrText = GetCodeName(iErrClass, oErrCode)
        .
        RETURN.
    END.
    {365p.get &obj = "iHExch"
              &fld = "���"
              &val = "vBIK"}
    vBankMFO = FGetSetting("�������", "", "").
    IF vBIK <> vBankMFO THEN DO:
        ASSIGN
            oErrCode = "31"
            oErrText = GetCodeName(iErrClass, oErrCode)
        .
        RETURN.
    END.
    vCheck33 = TRNSettingValue("ExchSET-TAX", "Check33", "����").
    FOR EACH ttBlockAcct EXCLUSIVE-LOCK:
        {find-act.i &acct = ttBlockAcct.acct
                    &curr = ttBlockAcct.currency}
        IF NOT AVAILABLE acct THEN DO:
            ASSIGN
                ttBlockAcct.error-code = "32"
                ttBlockAcct.msg        = GetCodeName(iErrClass, "32")
            .
            NEXT.
        END.
        IF acct.open-date > gend-date THEN DO:
            ASSIGN
                ttBlockAcct.error-code = "35"
                ttBlockAcct.msg        = "��� �� ����� �� ���� " +
                                         date2str(gend-date)       +
                                        ". �����஢�� �� ��ࠡ�⠭�."
            .
            NEXT.
        END.
        RUN GetCustName IN h_base (acct.cust-cat,
                                   acct.cust-id,
                                   acct.acct,
                                   OUTPUT vCustName,
                                   OUTPUT vCustName2,
                                   INPUT-OUTPUT vINN).
        IF vCheck33 = "����" THEN DO:
            {365p.get &obj = "iHExch"
                      &fld = "������"
                      &val = "vNaimNP"}
            {365p.get &obj = "iHExch"
                      &fld = "�����"
                      &val = "vFIOIP"}
            vCustName = TRIM((IF vCustName = "��" THEN ""
                                                  ELSE (vCustName + " ")) +
                             vCustName2).
            IF {assigned vNaimNP} THEN DO:
                RUN VerifyCorpName365p(acct.cust-id,
                                       vNaimNP,
                                       OUTPUT vCorpNameCorrect)
                NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    ASSIGN
                        ttBlockAcct.error-code = "35"
                        ttBlockAcct.msg        = RETURN-VALUE
                    .
                    NEXT.
                END.
                IF NOT vCorpNameCorrect THEN DO:
                    ASSIGN
                        ttBlockAcct.error-code = "33"
                        ttBlockAcct.msg        = GetCodeName(iErrClass, "33")
                    .
                    NEXT.
                END.
            END.
            ELSE IF {assigned vFIOIP} THEN DO:
                RUN FixCustName365p(INPUT-OUTPUT vCustName).
                IF vCustName <> vFIOIP THEN DO:
                    ASSIGN
                        ttBlockAcct.error-code = "33"
                        ttBlockAcct.msg        = GetCodeName(iErrClass, "33")
                    .
                    NEXT.
                END.
            END.
            ELSE DO:
                ASSIGN
                    ttBlockAcct.error-code = "33"
                    ttBlockAcct.msg        = GetCodeName(iErrClass, "33")
                .
                NEXT.
            END.
        END.
        ELSE DO:
            {365p.get &obj = "iHExch"
                      &fld = "�����"
                      &val = "vINNNP"}
            IF {assigned vINNNP} AND vINN <> vINNNP THEN DO:
                ASSIGN
                    ttBlockAcct.error-code = "35"
                    ttBlockAcct.msg        = "��� ������ 㪠��� ����୮"
                .
                NEXT.
            END.
        END.
    END.
    IF NOT {assigned oErrCode} THEN
        ASSIGN
            oErrCode = "10"
            oErrText = GetCodeName(iErrClass, oErrCode)
        .
END PROCEDURE.

PROCEDURE TAXBlockAcctError.
    DEFINE INPUT  PARAMETER TABLE    FOR ttBlockAcct.
    DEFINE OUTPUT PARAMETER oErrCode AS  CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrText AS  CHARACTER NO-UNDO.

    DEFINE BUFFER ttBlockAcct  FOR ttBlockAcct.
    DEFINE BUFFER ttBlockAcct1 FOR ttBlockAcct.

    DEFINE VARIABLE vErrText  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vMultiple AS LOGICAL   NO-UNDO.

    ASSIGN
        oErrCode = ""
        oErrText = ""
        vErrText = ""
    .
    FOR EACH ttBlockAcct WHERE
        {assigned ttBlockAcct.error-code} AND
        ttBlockAcct.error-code <> "10"
    NO-LOCK
    BREAK BY ttBlockAcct.error-code
          BY ttBlockAcct.msg:
        IF ttBlockAcct.error-code = "35" THEN DO:
            IF FIRST-OF(ttBlockAcct.error-code) THEN
                oErrCode = oErrCode + "," + ttBlockAcct.error-code.
            IF FIRST-OF(ttBlockAcct.msg) THEN
                oErrText = oErrText              +
                           " "                   +
                           (IF {assigned ttBlockAcct.msg} THEN
                                TRIM(ttBlockAcct.msg)
                            ELSE
                                "��稥 �訡��") +
                           ": ".
            oErrText = oErrText                                         +
                       (IF FIRST-OF(ttBlockAcct.msg) THEN "" ELSE ", ") +
                       DelFilFromAcct(ttBlockAcct.acct).
            IF LAST-OF(ttBlockAcct.msg) THEN
                oErrText = oErrText + ".".
            NEXT.
        END.
        IF FIRST-OF(ttBlockAcct.error-code) THEN DO:
            vMultiple = CAN-FIND(FIRST ttBlockAcct1 WHERE
                                     ttBlockAcct1.error-code =  ttBlockAcct.error-code AND
                                     ttBlockAcct1.acct       <> ttBlockAcct.acct).
            CASE ttBlockAcct.error-code:
                WHEN "32" THEN
                    vErrText = IF vMultiple THEN "�� ������� ���"
                                            ELSE "�� ������ ���".
                WHEN "33" THEN
                    vErrText = IF vMultiple THEN "������ ������������ �����⮢"
                                            ELSE "����୮� ������������ ������".
            END.
            ASSIGN
                oErrCode = oErrCode + "," + ttBlockAcct.error-code
                oErrText = oErrText + " " + vErrText + ": "
            .
        END.
        oErrText = oErrText                                                +
                   (IF FIRST-OF(ttBlockAcct.error-code) THEN "" ELSE ", ") +
                   DelFilFromAcct(ttBlockAcct.acct).
        IF LAST-OF(ttBlockAcct.error-code) THEN
            oErrText = oErrText + ".".
    END.
    ASSIGN
        oErrCode = TRIM(oErrCode, ",")
        oErrText = TRIM(oErrText)
    .
    IF NUM-ENTRIES(oErrCode) > 1 THEN
       oErrCode = "35".
    IF LENGTH(oErrText) > 512 THEN
       oErrText = SUBSTRING(oErrText, 1, 512 - LENGTH("[...]")) + "[...]".
END PROCEDURE.
          
PROCEDURE TAXOpPoruchFileImport:
   DEFINE INPUT  PARAMETER iFormat AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hExch    AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vInfoType  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hDataExch  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hDataAcct  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hServExch  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hAnsExchD  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hOutExch   AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vBIK       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBankMFO   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSeanceID  AS INT64       NO-UNDO.
   DEFINE VARIABLE vBlkType   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUserSel   AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vStrTMP    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPacketID  AS INT64       NO-UNDO.
   DEFINE VARIABLE vErrCode   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrClass  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrText   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mReqType   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vHCust     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE oHTable    AS HANDLE      NO-UNDO. /* �室�騥 ���*/

   DEFINE VARIABLE vCheckCorpName AS LOGICAL              NO-UNDO.
   DEFINE VARIABLE vVParams       AS CHARACTER INITIAL "" NO-UNDO.

   DEFINE VARIABLE vFlagSet   AS LOGICAL     NO-UNDO     INIT ?.

   DEFINE BUFFER Seance FOR Seance.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      vInfoType  = hExch:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE
      vSeanceID  = hExch:BUFFER-FIELD("SeanceID"):BUFFER-VALUE
      vPacketID  = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      hDataExch  = ObjectValueHandle (ENTRY(1,mReqTrans))
      vBlkType   = TRNSettingValue("","BlockType","�����㬬")
      vStrTMP    = TRNSettingValue("", "UserSelType","��")
      vErrClass  = TRNSettingValue("", "TaxErrClass", "")
      vUserSel   = (vStrTMP EQ "��") OR (vStrTMP EQ "YES")
      hDataExch:BUFFER-FIELD("mail-user-num"):BUFFER-VALUE = hExch:BUFFER-FIELD("mail-user-num"):BUFFER-VALUE 
      vCheckCorpName = (TRNSettingValue("ExchSET-TAX", "Check33", "����") <> "���")
   NO-ERROR. {&ON-ERROR}

   FIND FIRST Seance WHERE Seance.SeanceID EQ vSeanceID NO-LOCK NO-ERROR.

   RUN PacketTextSave (vPacketID, mMess).
   RUN SetValidationParameter365p(INPUT-OUTPUT vVParams,
                                  "CheckCorpName",
                                  STRING(vCheckCorpName)).
   RUN SetMFMode365p IN THIS-PROCEDURE (hDataExch).
   CASE vInfoType: /*��� ���ଠ樨*/
      WHEN  "�����������" THEN DO:
         RUN ValidateRequest365p(hDataExch,
                                 ?,
                                 vVParams,
                                 OUTPUT vErrCode,
                                 OUTPUT vErrText).
        CREATE ttop-poruch.
        ASSIGN
           ttop-poruch.f1 = SUBSTR(STRING(DATETIME(TODAY, MTIME)),1,19)
           ttop-poruch.f2 = hDataExch:BUFFER-FIELD(GetMangledName("���⥫��")):BUFFER-VALUE  
           ttop-poruch.f3 = hDataExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE

           ttop-poruch.f4 = IF hDataExch:BUFFER-FIELD(GetMangledName("����珫")):BUFFER-VALUE EQ ? OR
                               hDataExch:BUFFER-FIELD(GetMangledName("����珫")):BUFFER-VALUE EQ "" THEN 
                             hDataExch:BUFFER-FIELD(GetMangledName("��������")):BUFFER-VALUE
                            ELSE hDataExch:BUFFER-FIELD(GetMangledName("����珫")):BUFFER-VALUE
           ttop-poruch.f5 = hDataExch:BUFFER-FIELD(GetMangledName("��������")):BUFFER-VALUE
           ttop-poruch.f6 = DATE(hDataExch:BUFFER-FIELD(GetMangledName("��⠏����")):BUFFER-VALUE)
           ttop-poruch.f7 = DEC(hDataExch:BUFFER-FIELD(GetMangledName("�㬏����")):BUFFER-VALUE) / 100.0
           ttop-poruch.f8 = IF CAN-DO("10,",vErrCode) THEN "10"
                            ELSE vErrCode + " - " + vErrText
           ttop-poruch.f9  = mFile1
           ttop-poruch.{&order-field} = STRING(ttop-poruch.f1) + "," + ttop-poruch.f2
        .

        RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "Hdl1", 
                                        STRING(TEMP-TABLE ttop-poruch:HANDLE)).
        IF vErrCode = "10" OR NOT {assigned vErrCode} THEN DO:
            RUN CreateOpPNO(hDataExch, gend-date, OUTPUT oOp) NO-ERROR.
            IF oOp > 0 THEN DO:
                RUN PacketCreateLink(vPacketID, "op", STRING(oOp), "TTAXConf").
                {&ON-ERROR}
            END.
        END.

        IF (vErrCode EQ "10" AND
            FGetSetting("����ன��_365�", "����1", "���") NE "�⪫"
           ) OR
           vErrCode NE "10" THEN
           RUN TAXConfirmCreate (vPacketID, vErrCode, vErrText, NOW).
        IF vErrCode NE "10" AND
           FGetSetting("����ன��_365�", "����1", "���") EQ "��" THEN
           RUN TAXConfirmCreate(vPacketID, "10", "", NOW).
      END.
      WHEN  "������������" OR WHEN "������������" THEN DO:

         RUN ValidateRequest365p(hDataExch,
                                 ?,
                                 vVParams,
                                 OUTPUT vErrCode,
                                 OUTPUT vErrText).
         CREATE ttop-treb.
         ASSIGN
            ttop-treb.f1 = SUBSTR(STRING(DATETIME(TODAY, MTIME)),1,19)
            ttop-treb.f2 = IF hDataExch:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE EQ ? OR
                              hDataExch:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE EQ "" THEN
                              hDataExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE
                           ELSE hDataExch:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE
            ttop-treb.f3 = hDataExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE
            ttop-treb.f4 = IF vInfoType EQ "������������" THEN 
                              hDataExch:BUFFER-FIELD(GetMangledName("��������")):BUFFER-VALUE
                           ELSE hDataExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE
            ttop-treb.f5 = IF vInfoType EQ "������������" THEN 
                              DATE(hDataExch:BUFFER-FIELD(GetMangledName("��⠏����")):BUFFER-VALUE)
                           ELSE DATE(hDataExch:BUFFER-FIELD(GetMangledName("��⠁�")):BUFFER-VALUE)
            ttop-treb.f6 = DEC(hDataExch:BUFFER-FIELD(GetMangledName("�㬬�����")):BUFFER-VALUE)
            ttop-treb.f7 = IF CAN-DO("10,",vErrCode) THEN "10"
                             ELSE vErrCode + " - " + vErrText
            ttop-treb.f8  = mFile1
            ttop-treb.{&order-field} = STRING(ttop-treb.f1) + "," + ttop-treb.f2
         .


         RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "Hdl2", 
                                         STRING(TEMP-TABLE ttop-treb:HANDLE)).

         IF (vErrCode EQ "10" AND
             FGetSetting("����ன��_365�", "����1", "���") NE "�⪫"
            ) OR
            vErrCode NE "10" THEN
            RUN TAXConfirmCreate (vPacketID, vErrCode, vErrText, NOW).
         IF vErrCode NE "10" AND
            FGetSetting("����ன��_365�", "����1", "���") EQ "��" THEN
            RUN TAXConfirmCreate(vPacketID, "10", "", NOW).

         IF CAN-DO("10,",vErrCode) THEN DO:
             RUN GetCust365p(hDataExch, ?, vCheckCorpName, OUTPUT vHCust) NO-ERROR.
             IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(vHCust) THEN
                 RUN LinkCust365p(vHCust, vPacketID, "TTAXConf") NO-ERROR.
                 {&ON-ERROR}
             RUN DeleteObject365p(vHCust).
         END.

      END.
      WHEN  "��������" THEN DO:

         /*
         ������� - ���� 䠩��
         ReqType - ���� �࠭ᯮ�� �ᯮ��
         ����ன��_365�
           ��_���1  �࠭����� ����� � ����稨 � e-txavla
           ��_���2  �࠭����� ����� �� ���⪠� e-txposa
           ��_���3  �࠭����� ����� �믨᪨ ��  e-txopac
         1 - ����� � ����稨 ��⮢ � �����; 
         2 - ����� �� ���⪠� �������� �।�� �� ���;  ���
         3 - ����� �믨᪨ �� ������ �� ���
         */
         mReqType = hDataExch:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE.

         IF NOT {assigned mReqType} THEN mReqType = "1".

         RUN CreateBExch_ZNO1 (hDataExch, mReqType, OUTPUT oHTable). /* �������� � ���������� �� �⢥� */

         RUN ValidateRequest365p(hDataExch,
                                 oHTable,
                                 vVParams,
                                 OUTPUT vErrCode,
                                 OUTPUT vErrText).
         IF vErrCode = {&ERR-SPECIAL} THEN DO:
            RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "ReqType", "").
            RUN Fill-SysMes IN h_tmess ("", "", "0", vErrText).
            RUN PacketSetState IN h_pack (vPacketID, {&STATE-ERR}).
         END.
         ELSE DO:
            RUN Add2ZNOLog365p(mFile1,
                               mReqType,
                               hDataExch,
                               vErrCode,
                               vErrText).
            RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "Hdl3", 
                                            STRING(TEMP-TABLE ttop-zapr:HANDLE)).
            IF CAN-DO("10,",vErrCode) THEN DO:
               mSuperPacketID = vPacketID.
               RUN CreateBExch_ZNO2 (hDataExch, ?, oHTable, mReqType, vPacketID, 
                                     OUTPUT mCountAll,
                                     INPUT-OUTPUT vErrCode,
                                     INPUT-OUTPUT vErrText). /* �������� � ���������� �� �⢥� */
               IF vErrCode EQ "35" THEN
                  ttop-zapr.f5 = vErrCode + " - " + vErrText.
            END.
            ELSE
               RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "ReqType", "").
            IF (vErrCode EQ "10" AND
                FGetSetting("����ன��_365�", "����1", "���") NE "�⪫"
               ) OR
               vErrCode NE "10" THEN
            DO:
               RUN TAXConfirmCreate (vPacketID, vErrCode, vErrText, NOW) NO-ERROR.
               {&ON-ERROR}
            END.
            IF vErrCode NE "10" AND
               FGetSetting("����ன��_365�", "����1", "���") EQ "��" THEN
            DO:
               RUN TAXConfirmCreate(vPacketID, "10", "", NOW) NO-ERROR.
               {&ON-ERROR}
            END.
         END.
      END.
   END CASE.


/*
      RUN DumpObject(hOutExch).

      RUN DumpObject(hExch).
      RUN DumpObject(hDataExch).
*/
   /* ���⪠ �� */

   RUN ClearExch (hExch,     "ExchMain,mail-format").
   RUN ClearExch (hDataExch, "ExchMain").

   vFlagSet = YES.

END.     /* MAIN */
{doreturn.i vFlagSet}

END PROCEDURE.


PROCEDURE TAXFileIzv:
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE hExch      AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vPacketID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vFileID    AS INT64     NO-UNDO.
   DEFINE VARIABLE vFormat    AS CHARACTER   NO-UNDO.  /* ⥪�騩 �ଠ� */
   DEFINE VARIABLE vLevel     AS INT64     NO-UNDO.
   DEFINE VARIABLE vPoint     AS INT64     NO-UNDO.
   DEFINE VARIABLE vFooter    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBuffer    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParentFot AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOK        AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE hDataExch  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vProcImp   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vProcPrm   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mRunProc   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSeanceID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vIgnore    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vSFlag     AS LOGICAL     NO-UNDO.

   DEFINE VAR hProc  AS handle    NO-UNDO.
   DEFINE VAR vInter AS CHAR      NO-UNDO.

   DEFINE VARIABLE vMsg    AS CHARACTER   NO-UNDO.  /* ⥪�騩 �ଠ� */

   DEFINE VARIABLE vFlagSet   AS LOGICAL     NO-UNDO     INIT ?.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   IF RETRY THEN DO:
      IF vSFlag THEN
         INPUT STREAM sImport CLOSE.
   END.
   {do-retry.i MAIN}

   ASSIGN
      hExch      = hIns:DEFAULT-BUFFER-HANDLE
      vFormat    = hExch:BUFFER-FIELD("mail-format"):BUFFER-VALUE
      vPacketID  = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      vFileID    = hExch:BUFFER-FIELD("FileExchID"):BUFFER-VALUE
      vSeanceID  = hExch:BUFFER-FIELD("SeanceID"):BUFFER-VALUE
      mRunProc   = TRNSettingValue("","Run-Proc","")
      mReqTrans  = TRNSettingValue("","ReqTrans","")
      mAnsTrans  = TRNSettingValue("","AnsTrans","")
      mCount     = 0
      mCountProc = 0
      mSummVZ    = 0
   NO-ERROR. {&ON-ERROR}

   FIND FIRST Seance WHERE Seance.SeanceID EQ vSeanceID NO-LOCK NO-ERROR.
   FIND FIRST FileExch WHERE FileExch.FileExchID EQ vFileID
      NO-LOCK NO-ERROR. {&ON-ERROR}

   IF     {assigned mRunProc}
      AND mRunProc NE {&RET-ERROR}
          THEN RUN VALUE(mRunProc + ".p") (FileExch.Path) NO-ERROR.


   INPUT STREAM sImport FROM VALUE(FileExch.Path).
   vSFlag = YES.
   vIgnore = NO.

   REPEAT:
      IMPORT STREAM sImport UNFORMATTED vBuffer.
      IF NOT {assigned vBuffer} THEN NEXT.

      vMsg = vMsg + (if vMsg ne "" then "~n" else "") + vBuffer.

      &IF DEFINED (IS-DEBUG) &THEN
         RUN dbgprint.p ("TAXFileImport","vFormat:" + vFormat + " vLevel:" + GetNullInt(vLevel) +
                                        " vBuffer:" + vBuffer + " vFooter:" + GetNullStr(vFooter) +
                                      " mSpooling:" + STRING (mSpooling)).    
      &ENDIF


   END. /* REPEAT */
   INPUT STREAM sImport CLOSE.
   RUN PacketSaveAll(vPacketID, hExch:Name, "����", vMsg).

   ASSIGN
      vFormat = hExch::mail-format.

   RUN FileMoveArchive IN h_filex (vPacketID, "ImpArch") NO-ERROR.

   vFlagSet = YES.
END.     /* MAIN */
{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE TAXExpUvd:
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

   DEFINE BUFFER Packet       FOR Packet.

   DEFINE VARIABLE mFlagSet       AS LOGICAL     NO-UNDO INIT ?.
   DEFINE VARIABLE hExch          AS HANDLE      NO-UNDO.
   DEFINE VARIABLE mFmt           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mSep           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mPacketID      AS INT64       NO-UNDO.
   DEFINE VARIABLE mKind          AS CHAR        NO-UNDO.
   DEFINE VARIABLE mSeanceID      AS INT64       NO-UNDO.
   DEFINE VARIABLE mResult        AS CHAR        NO-UNDO.
   DEFINE VARIABLE mBuf           AS CHAR        NO-UNDO.
   DEFINE VARIABLE mSrcCP         AS CHARACTER   NO-UNDO.
   DEF VAR vFile AS CHAR NO-UNDO.
   DEF VAR vText AS CHAR NO-UNDO.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      hExch       = hIns:DEFAULT-BUFFER-HANDLE                        
      mSrcCP      = TRNSettingValue("", "CodePage", "DOS")            
      mSep        = TRNSettingValue("", "TagSep", ":")                
      mFmt        = hExch::mail-format                                
      mPacketID   = hExch::PacketID                                   
      mKind       = hExch::Kind                                       
      mSeanceID   = INT64(hExch::SeanceID)                            
      mResult     = ""                                                
   NO-ERROR. {&ON-ERROR}


   ASSIGN
      vFile = hExch:buffer-field("FileName"):buffer-value
      vText = SUBSTR(FGetSetting("�������",?,""),3) + "@@@~n" +
              FGetSetting("����",?,"") + "@@@~n" +
              "11@@@~n" +
              "<����� 䨫����>:<���>;<������������>~n" +
              "###~n" +
              "<����� 䨫����>:<���>;<������������>~n" +
              "###~n@@@~n" +
              STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") +
              "@@@~n===~n"
   .

   FORM
     " ����:" vFile format "x(78)" help "��� 䠩�� �ᯮ��"  
     "�����:" vText VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 18
   WITH FRAME f1 OVERLAY CENTERED NO-LABEL ROW 2 TITLE COLOR bright-white
     "[ ������������ ����������� ]".

   UPDATE
      vFile
      vText
   WITH FRAME f1.

   hExch:buffer-field("FileName"):buffer-value = vFile.
   mResult = vText.

   FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID
                       AND (IF   shMode
                           THEN Seance.Filial-ID EQ shFilial
                           ELSE YES)
      NO-LOCK NO-ERROR.
   FIND FIRST Mail-User WHERE Mail-User.op-kind-exp  EQ Seance.op-kind
                          AND (IF   shMode
                               THEN Mail-User.Filial-ID EQ shFilial
                               ELSE YES)
   NO-LOCK NO-ERROR.

   RUN PacketCreate     (INPUT  Seance.SeanceID,
                         INPUT  -1,
                         INPUT  Mail-User.Mail-User-num,
                         INPUT  mKind,
                         OUTPUT mPacketID) NO-ERROR. {&ON-ERROR}

   IF NOT  UpdateSigns("Packet",
                      STRING(mPacketID),
                      "FileName",
                      hExch:buffer-field("FileName"):buffer-value, 
                      ?)  THEN
      UNDO MAIN, RETRY MAIN.

   RUN PacketTextSave (mPacketID, mResult).


   mFlagSet = YES.

END.     /* MAIN */
{doreturn.i mFlagSet}
END PROCEDURE.

PROCEDURE TAXFileKwt:
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE hExch      AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vPacketID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vSeanceID  AS INT64     NO-UNDO.
   DEFINE VARIABLE vFileID    AS INT64     NO-UNDO.
   DEFINE VARIABLE vLine       AS INTEGER    NO-UNDO.
   DEFINE VARIABLE vFileName   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResultCode AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResultMes  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAvail      AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vBuffer    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMsg    AS CHARACTER   NO-UNDO.  /* ⥪�騩 �ଠ� */

   DEFINE VARIABLE vFlagSet   AS LOGICAL     NO-UNDO     INIT ?.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:

   {do-retry.i MAIN}

   ASSIGN
      hExch      = hIns:DEFAULT-BUFFER-HANDLE
      vPacketID  = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      vFileID    = hExch:BUFFER-FIELD("FileExchID"):BUFFER-VALUE
      vSeanceID  = hExch:BUFFER-FIELD("SeanceID"):BUFFER-VALUE 
   NO-ERROR. {&ON-ERROR}

   FIND FIRST Seance WHERE Seance.SeanceID EQ vSeanceID NO-LOCK NO-ERROR.
   FIND FIRST FileExch WHERE FileExch.FileExchID EQ vFileID
      NO-LOCK NO-ERROR. {&ON-ERROR}

   INPUT STREAM sImport FROM VALUE(FileExch.Path).

   vLine = 0.
   REPEAT:
      IMPORT STREAM sImport UNFORMATTED vBuffer.
      vLine = vLine + 1.
      IF NOT {assigned vBuffer} THEN NEXT.

      IF vLine = 1  THEN vFileName = ENTRY(1,vBuffer,"#").
      IF vLine = 2 THEN 
      ASSIGN
         vResultCode = ENTRY(1,ENTRY(1,vBuffer,";"),"@")
         vResultMes  = ENTRY(1,ENTRY(2,vBuffer,";"),"@")
      NO-ERROR.

      vMsg = vMsg + (if vMsg ne "" then "~n" else "") + vBuffer.

   END. /* REPEAT */
   INPUT STREAM sImport CLOSE.

   IF {assigned vFileName} THEN DO:

       FOR EACH FileExch WHERE
                FileExch.NAME EQ ENTRY(1,vFileName) + ".txt"
                NO-LOCK,
           EACH Packet WHERE 
                Packet.FileExchID EQ FileExch.FileExchID
            AND CAN-DO("����,����",Packet.State)  
                EXCLUSIVE-LOCK:

          IF vResultCode <> '20' THEN
          Packet.State = "����".
          ELSE Packet.State = "����".

          RUN PacketCreateLink IN h_pack (vPacketID,
                                          "Packet",
                                          Packet.PacketID,
                                          "TTaxKWT"). {&ON-ERROR}
          
          RUN PacketCreateLink IN h_pack (Packet.PacketID,
                                          "Packet",
                                          vPacketID,
                                          "TTaxKWT"). {&ON-ERROR}

          vAvail = YES.
          LEAVE.
       END.       
   END.
   IF NOT vAvail THEN
      RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������� ��室��� ᮮ�饭�� (" + vFileName + "). ").

   RUN PacketSaveAll(vPacketID,
                     hExch:Name,
                     IF vAvail THEN "����" ELSE "����",
                     vMsg).

   RUN FileMoveArchive IN h_filex (vPacketID, "ImpArch") NO-ERROR.

   vFlagSet = YES.
END.     /* MAIN */
{doreturn.i vFlagSet}
END PROCEDURE.

/*    ��ନ஢���� �⢥⮢                                                    */
{taxrsexp.i }

/*    ����ନ஢���� �⢥⮢-�ࠢ��                                        */
PROCEDURE TAXSpravReform.
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vFlagSet      AS LOGICAL     NO-UNDO    INIT ?.
   DEFINE VARIABLE hExch         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vSprPackID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vSprFileID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vSprFileName  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vReqPackID    AS INT64       NO-UNDO.
   DEFINE VARIABLE vReqFileName  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSFlag        AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE hFile         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hResl         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hAcct         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hZapr         AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vFormat       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFileNum      AS INT64       NO-UNDO.
   DEFINE VARIABLE oHTable       AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vReqType      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPart         AS INT64       NO-UNDO.
   DEFINE VARIABLE vPartS        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSkip         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vErrCode      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrText      AS CHARACTER   NO-UNDO.

   DEFINE BUFFER rPacket    FOR Packet.
   DEFINE BUFFER PackObject FOR PackObject.
   DEFINE BUFFER FileExch   FOR FileExch.
   DEFINE BUFFER PacketText FOR PacketText.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      hExch = hIns:DEFAULT-BUFFER-HANDLE
      vSprPackID = hExch::SpravPacketID
      vSprFileID = hExch::SpravFileExchID
      hFile      = ObjectValueHandle("ETAXFile")
      hResl      = ObjectValueHandle("ETAXResolutn")
      hAcct      = ObjectValueHandle("ETAXAcctBlk")
      hZapr      = ObjectValueHandle("ETAXAcctZNO")
   NO-ERROR. {&ON-ERROR}

   FIND FIRST FileExch WHERE FileExch.FileExchID EQ vSprFileID
      NO-LOCK NO-ERROR. {&ON-ERROR}
   vSprFileName = FileExch.Name.

   vReqFileName = SUBSTR(vSprFileName, INDEX(vSprFileName, "_") + 1).
   vReqPackID   = ?.
   FIND FIRST PackObject WHERE PackObject.PacketID  EQ vSprPackID
                           AND PackObject.file-name EQ "Packet"
                           AND PackObject.Kind      EQ "TTAXConf"

      NO-LOCK NO-ERROR.

   IF AVAIL PackObject THEN
      vReqPackID = INT64(PackObject.Surrogate).
   ELSE 
   FOR EACH FileExch WHERE FileExch.Name EQ vReqFileName
      NO-LOCK,
      FIRST rPacket WHERE rPacket.FileExchID EQ FileExch.FileExchID
      NO-LOCK:
      vReqPackID = rPacket.PacketID.
      LEAVE.
   END.
   IF vReqPackID EQ ? THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "�� 㤠���� ���� ᮮ�饭�� ��室���� ����� RPO/ZNO").
      hExch::ExpSpr = NO.
      LEAVE MAIN.
   END.

   hExch::ReqFileName = vReqFileName.
   mSuperPacketID     = vReqPackID.

   IF vSprFileName BEGINS "BV" THEN DO:
      vPartS = SUBSTR(vSprFileName, 4, 2).
      vPart  = INT64(vPartS) NO-ERROR.
      vSkip  = CAN-FIND (FIRST tt-bv WHERE tt-bv.SeanceID  EQ hExch::SeanceID
                                       AND tt-bv.ReqPackID EQ vReqPackID
                         NO-LOCK).
      CREATE tt-bv.
      ASSIGN 
         tt-bv.SeanceID  = hExch::SeanceID
         tt-bv.ReqPackID = vReqPackID
         tt-bv.BVPart    = vPartS.
      IF vSkip THEN DO:
         hExch::ExpSpr = NO.
         LEAVE MAIN.
      END.
   END.

   /* ����뢠�� ᮤ�ন��� ��室���� ᮮ�饭�� �� �६���� 䠩� */
   OUTPUT STREAM sReq TO VALUE(vReqFileName).
   vSFlag = YES.
   FOR EACH PacketText WHERE PacketText.PacketID EQ vReqPackID
      NO-LOCK:
      PUT STREAM sReq UNFORMATTED PacketText.Contents SKIP.
   END.
   OUTPUT STREAM sReq CLOSE.
   vSFlag = NO.

   IF vReqFileName BEGINS "RPO" THEN
      ASSIGN
         vFormat            = "TTAXFile-AcctBlk"
         hAcct::ExchMain    = "ETAXResolutn"
         mReqTrans          = "ETAXResolutn,ETAXAcctBlk"
      .
   ELSE
      ASSIGN
         vFormat         = "TTAXFile-AcctZNO"
         hAcct::ExchMain = "ETAXAcctZNO"
         mReqTrans       = "ETAXAcctZNO,ETAXAcctBlk"
      .

   RUN TAXLoadExchFromFile(vReqFileName, vFormat).

   IF vReqFileName BEGINS "RPO" THEN DO:
      hExch::RsFormat = "TTAXRsPosAFile".
      hExch::PreF     = "BOS".
      vFileNum = INT64(GetXAttrValueEx("Packet", STRING(vReqPackID), "BOSNum", "1")) + 1.
      UpdateSigns("PTax", STRING(vReqPackID), "BOSNum", STRING(vFileNum), ?).
   END.
   ELSE DO:
      vReqType = hZapr:BUFFER-FIELD(GetMangledName("�������")):BUFFER-VALUE.
      CASE vReqType:
         WHEN "1" THEN DO:
            hExch::RsFormat = "TTAXRsAvAcFile".
            hExch::PreF     = "BNS".
            vFileNum = INT64(GetXAttrValueEx("Packet", STRING(vReqPackID), "BNSNum", "1")) + 1.
            UpdateSigns("PTax", STRING(vReqPackID), "BNSNum", STRING(vFileNum), ?).
         END.
         WHEN "2" THEN DO:
            hExch::RsFormat = "TTAXRsPosAFile".
            hExch::PreF     = "BOS".
            vFileNum = INT64(GetXAttrValueEx("Packet", STRING(vReqPackID), "BOSNum", "1")) + 1.
            UpdateSigns("PTax", STRING(vReqPackID), "BOSNum", STRING(vFileNum), ?).
         END.
         WHEN "3" THEN DO:
            hExch::RsFormat = "TTAXRsVypFile".
            hExch::PreF     = "BV".
            vFileNum = INT64(GetXAttrValueEx("Packet", STRING(vReqPackID), "BVNum", "1")) + 1.
            UpdateSigns("PTax", STRING(vReqPackID), "BVNum", STRING(vFileNum), ?).
         END.
      END CASE.
   END.
   hExch::NumF        = STRING(vFileNum).

&IF DEFINED (IS-DEBUG) &THEN
   RUN DumpObject(hFile).
   RUN DumpObject(hResl).
   FOR EACH ttBlockAcct NO-LOCK:
      RUN dbgprint.p ("ttBlockAcct", ttBlockAcct.acct).    
   END.
&ENDIF

   IF vReqFileName BEGINS "RPO" THEN
      RUN CreateBOSExch_RPO (hResl).
   ELSE DO:
      RUN CreateBExch_ZNO1(hZapr, vReqType, OUTPUT oHTable).
      RUN CreateBExch_ZNO2(hZapr, ?, oHTable, vReqType, vReqPackID, 
                           OUTPUT mCountAll,
                           INPUT-OUTPUT vErrCode,
                           INPUT-OUTPUT vErrText).
   END.

   vFlagSet = YES.
   hExch::ExpSpr = YES.
END.

IF vSFlag THEN DO:
   OUTPUT STREAM sReq CLOSE.
END.
OS-DELETE VALUE(vReqFileName).

vFlagSet = YES.
{doreturn.i vFlagSet}
END PROCEDURE.

PROCEDURE SetBlockObjectBegDT.
    DEFINE PARAMETER BUFFER BlockObject FOR  BlockObject.
    DEFINE INPUT  PARAMETER iDateTime   LIKE BlockObject.beg-datetime NO-UNDO.

    DEFINE BUFFER xBlockObject FOR BlockObject.

    DEFINE VARIABLE vDTInf LIKE BlockObject.beg-datetime NO-UNDO.
    DEFINE VARIABLE vDTSup LIKE BlockObject.beg-datetime NO-UNDO.
    DEFINE VARIABLE vDTM   LIKE BlockObject.beg-datetime NO-UNDO.
    DEFINE VARIABLE vDTP   LIKE BlockObject.beg-datetime NO-UNDO.

    ASSIGN
        vDTP = iDateTime
        vDTM = iDateTime
    .
    FOR EACH xBlockObject WHERE
        ROWID(xBlockObject)             <> ROWID(BlockObject)     AND
        xBlockObject.class-code         =  BlockObject.class-code AND
        xBlockObject.file-name          =  BlockObject.file-name  AND
        xBlockObject.surrogate          =  BlockObject.surrogate  AND
        xBlockObject.block-type         =  BlockObject.block-type AND
        DATE(xBlockObject.beg-datetime) =  DATE(iDateTime)
    NO-LOCK
    BY ABSOLUTE(xBlockObject.beg-datetime - iDateTime):
        IF xBlockObject.beg-datetime >= iDateTime THEN DO:
            vDTSup = xBlockObject.beg-datetime.
            IF vDTP < vDTSup THEN DO:
                vDTM = ?.
                LEAVE.
            END.
            vDTP = vDTP + 1.
        END.
        IF xBlockObject.beg-datetime <= iDateTime THEN DO:
            vDTInf = xBlockObject.beg-datetime.
            IF vDTM > vDTInf THEN DO:
                vDTP = ?.
                LEAVE.
            END.
            vDTM = vDTM - 1.
        END.
    END.
    IF vDTP <> ? AND vDTM <> ? THEN DO:
        ASSIGN
            vDTSup = DATETIME(DATE(iDateTime) + 1, 0) - 1
            vDTInf = DATETIME(DATE(iDateTime)    , 0)
        .
        IF vDTP > vDTSup THEN
            vDTP = ?.
        ELSE IF vDTM < vDTInf THEN
            vDTM = ?.
        ELSE DO:
            IF ABSOLUTE(vDTP - iDateTime) < ABSOLUTE(vDTM - iDateTime) THEN
                vDTM = ?.
            ELSE
                vDTP = ?.
        END.
    END.
    ASSIGN
        iDateTime = ?
        iDateTime = vDTP WHEN vDTP <> ?
        iDateTime = vDTM WHEN vDTM <> ?
    .
    IF iDateTime = ? THEN
        RETURN ERROR.
    BlockObject.beg-datetime = iDateTime.
END PROCEDURE.

PROCEDURE Add2ZNOLog365p.
    DEFINE INPUT PARAMETER iFileName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iReqType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iHReqInfo  AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iErrorCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iErrorText AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vCustName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCustINN   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vReqName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vErrorDesc AS CHARACTER NO-UNDO.

    IF NOT {assigned iErrorCode} THEN
        iErrorCode = {&ERR-SUCCESS}.
    RUN GetCharAttrValue365p(iHReqInfo, "������", OUTPUT vCustName) NO-ERROR.
    IF NOT {assigned vCustName} THEN
        RUN GetCharAttrValue365p(iHReqInfo, "�����", OUTPUT vCustName) NO-ERROR.
    RUN GetCharAttrValue365p(iHReqInfo, "�����", OUTPUT vCustINN) NO-ERROR.
    ASSIGN
        vReqName = "��ࠢ�� � ����稨 ��⮢"      WHEN iReqType = "1"
        vReqName = "��ࠢ�� �� ���⪠� �� ����" WHEN iReqType = "2"
        vReqName = "�믨᪠ �� ��⠬"             WHEN iReqType = "3"
    .
    CREATE ttop-zapr.
    ASSIGN
        ttop-zapr.f1             = SUBSTRING(STRING(DATETIME(TODAY, MTIME)),
                                             1,
                                             19)
        ttop-zapr.f2             = vCustName
        ttop-zapr.f3             = vCustINN
        ttop-zapr.f4             = vReqName
        ttop-zapr.f5             = IF iErrorCode = {&ERR-SUCCESS} THEN
                                       iErrorCode
                                   ELSE
                                       (iErrorCode + " - " + iErrorText)
        ttop-zapr.f6             = iFileName
        ttop-zapr.{&order-field} = STRING(ttop-zapr.f1) + "," + ttop-zapr.f2
  .
END PROCEDURE.

PROCEDURE CheckBankrupcy365p.
    DEFINE INPUT  PARAMETER iCustCat   LIKE acct.cust-cat NO-UNDO.
    DEFINE INPUT  PARAMETER iCustId    LIKE acct.cust-id  NO-UNDO.
    DEFINE INPUT  PARAMETER iDate      AS   DATE          NO-UNDO.
    DEFINE OUTPUT PARAMETER oErrorCode AS   CHARACTER     NO-UNDO INITIAL "10".
    DEFINE OUTPUT PARAMETER oErrorText AS   CHARACTER     NO-UNDO.

    DEFINE BUFFER code  FOR code.
    DEFINE BUFFER loan  FOR loan.
    DEFINE BUFFER signs FOR signs.

    DEFINE VARIABLE vBStage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCustName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vS        AS CHARACTER NO-UNDO.

    IF NOT CAN-DO({&BANKRUPCY-CUST-CATS}, iCustCat) THEN
        RETURN.
    vBStage = ClientXAttrVal(iCustCat, iCustId, "������").
    FIND FIRST code WHERE
        code.class = "����_���������" AND
        code.name  = vBStage
    NO-LOCK NO-ERROR.
    IF NOT (AVAILABLE code AND code.val = "1") THEN
        RETURN.
    FOR EACH loan WHERE
        loan.cust-cat   = iCustCat   AND
        loan.cust-id    = iCustId    AND
        loan.class-code = "bankrupt" AND
        (loan.end-date = ? OR loan.end-date <= iDate)
    NO-LOCK,
    FIRST signs WHERE
        signs.file-name  = "loan"                               AND
        signs.surrogate  = loan.contract + "," + loan.cont-code AND
        signs.code       = "��������"                           AND
        signs.code-value = code.code
    NO-LOCK
    BY loan.end-date DESCENDING:
        ASSIGN
            vCustName  = GetCliName(iCustCat,
                                    STRING(iCustId),
                                    OUTPUT vS,
                                    OUTPUT vS,
                                    OUTPUT vS,
                                    INPUT-OUTPUT vS,
                                    OUTPUT vS,
                                    OUTPUT vS)
            vS         = "�襭�� �" + loan.doc-num +
                         " �� " + STRING(loan.end-date, "99.99.9999") +
                         " " + loan.user-o[1]
            oErrorCode = "35"
            oErrorText = "�� " + (IF {assigned vCustName} THEN vCustName
                                                          ELSE "�������") +
                         " ������� " + vBStage +
                         (IF {assigned vS} THEN (" �� �᭮����� " + vS)
                                           ELSE "")
        .
        LEAVE.
    END.
END PROCEDURE.

PROCEDURE PacketSaveAll.
   DEFINE INPUT PARAMETER iPacketId LIKE Packet.PacketID NO-UNDO.
   DEFINE INPUT PARAMETER iKind     LIKE Packet.Kind     NO-UNDO.
   DEFINE INPUT PARAMETER iState    LIKE Packet.State    NO-UNDO.
   DEFINE INPUT PARAMETER iText     AS   CHARACTER       NO-UNDO.

   DEFINE BUFFER Packet FOR Packet.

   MAIN:
   DO TRANSACTION
   ON ERROR  UNDO MAIN, LEAVE MAIN
   ON ENDKEY UNDO MAIN, LEAVE MAIN:
      FOR FIRST Packet WHERE
         Packet.PacketID = iPacketId
      EXCLUSIVE-LOCK:
         ASSIGN
            Packet.Kind  = iKind
            Packet.State = iState
         .
         RUN PacketTextSave(iPacketId, iText).
      END.
   END.
END PROCEDURE.

PROCEDURE TAXExpCnf:
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

   DEFINE BUFFER Packet       FOR Packet.

   DEFINE VARIABLE mFlagSet       AS LOGICAL     NO-UNDO INIT ?.
   DEFINE VARIABLE hExch          AS HANDLE      NO-UNDO.
   DEFINE VARIABLE mFmt           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mPacketID      AS INT64       NO-UNDO.
   DEFINE VARIABLE mKind          AS CHAR        NO-UNDO.
   DEFINE VARIABLE mSeanceID      AS INT64       NO-UNDO.
   DEFINE VARIABLE mResult        AS CHAR        NO-UNDO.
   DEFINE VARIABLE mBuf           AS CHAR        NO-UNDO.
   DEFINE VARIABLE mSrcCP         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mxmlns         AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE mIdConf        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mFileName      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mDTimeChck     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mCodeChck      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mDetails       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCnt           AS INT64       NO-UNDO.

   DEFINE VARIABLE vFile          AS CHAR NO-UNDO.
   DEFINE VARIABLE vText          AS CHAR NO-UNDO.
   DEFINE VARIABLE vPackId        AS CHAR NO-UNDO.
   DEFINE VARIABLE vQuot          AS CHAR NO-UNDO.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      hExch       = hIns:DEFAULT-BUFFER-HANDLE                        
      mSrcCP      = TRNSettingValue("", "CodePage", "DOS")            
      mFmt        = hExch::mail-format                                
      mPacketID   = hExch::PacketID                                   
      mKind       = hExch::Kind                                       
      mSeanceID   = INT64(hExch::SeanceID)                            
      mResult     = ""                                                
      mxmlns      = hExch::xmlns  
      mIdConf     = hExch:BUFFER-FIELD(GetMangledName("������")):BUFFER-VALUE
      mFileName   = hExch:BUFFER-FIELD(GetMangledName("�����")):BUFFER-VALUE
      mDTimeChck  = hExch:BUFFER-FIELD(GetMangledName("��⠂६�஢�ન")):BUFFER-VALUE
      mCodeChck   = hExch:BUFFER-FIELD(GetMangledName("�������஢�ન")):BUFFER-VALUE
      mDetails    = hExch:BUFFER-FIELD(GetMangledName("���᭥���")):BUFFER-VALUE
      vQuot       = "&quot;"
   NO-ERROR. {&ON-ERROR}

   mIdConf = "BQ" + STRING(INT64(shFilial),"999999") + STRING(mSeanceID,"999999999").

   ASSIGN
      vFile = hExch:buffer-field("FileName"):buffer-value
      vText =  '<?xml version="1.0" encoding="windows-1251"?>~n' +
               "<���⢥ত���� xmlns=" + CHR(34) + mxmlns     + CHR(34) + 
               " ������="             + CHR(34) + mIdConf    + CHR(34) + 
               " �����="            + CHR(34) + mFileName  + CHR(34) + 
               " ��⠂६�஢�ન="   + CHR(34) + mDTimeChck + CHR(34) + " >~n".

   DO vCnt = 1 TO NUM-ENTRIES(mCodeChck,CHR(1)):
      ASSIGN
         vText = vText + "<����஢�ન �������஢�ન=" + CHR(34) 
         vText = vText + ENTRY(vCnt,mCodeChck,CHR(1)) + CHR(34)  
         vText = vText + " ���᭥���=" + CHR(34) + ENTRY(vCnt,REPLACE(mDetails,'"',vQuot), CHR(1))
         vText = vText + CHR(34) + " />~n"
      NO-ERROR.
   END.            
   vText =  vText + "<~/���⢥ত����>~n".            
   vText = CODEPAGE-CONVERT(vText,"1251",SESSION:CHARSET).            


   hExch:buffer-field("FileName"):buffer-value = vFile.
   mResult = vText.

   FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID
                       AND (IF   shMode
                           THEN Seance.Filial-ID EQ shFilial
                           ELSE YES)
      NO-LOCK NO-ERROR.
   FIND FIRST Mail-User WHERE Mail-User.op-kind-exp  EQ Seance.op-kind
                          AND (IF   shMode
                               THEN Mail-User.Filial-ID EQ shFilial
                               ELSE YES)
   NO-LOCK NO-ERROR.

   RUN PacketCreate     (INPUT  Seance.SeanceID,
                         INPUT  -1,
                         INPUT  Mail-User.Mail-User-num,
                         INPUT  mKind,
                         OUTPUT mPacketID) NO-ERROR. {&ON-ERROR} 

   /* �ਢ離� � ᮮ�饭�� ������ */
   vPackId = GetAttrValue2("", 0, "PacketIdImp").
   INT64(vPackId) NO-ERROR.
   IF {assigned vPackId} AND
      NOT ERROR-STATUS:ERROR THEN
   DO:
      RUN PacketCreateLink IN h_pack (INT64(vPackId),
                                      "Packet",
                                      STRING(mPacketID),
                                      "TTAXConf"). {&ON-ERROR}
   END.    
   IF NOT  UpdateSigns("Packet",
                      STRING(mPacketID),
                      "FileName",
                      hExch:buffer-field("FileName"):buffer-value, 
                      ?)  THEN
      UNDO MAIN, RETRY MAIN.

   RUN PacketTextSave (mPacketID, mResult).

   mFlagSet = YES.

END.     /* MAIN */
{doreturn.i mFlagSet}
END PROCEDURE.

PROCEDURE TAXFileKvt:
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER hIns   AS HANDLE      NO-UNDO.

   DEFINE VARIABLE hExch       AS HANDLE      NO-UNDO.

   DEFINE VARIABLE vPacketID   AS INT64     NO-UNDO.
   DEFINE VARIABLE vSeanceID   AS INT64     NO-UNDO.
   DEFINE VARIABLE vFileID     AS INT64     NO-UNDO.
   DEFINE VARIABLE vLine       AS INTEGER    NO-UNDO.
   DEFINE VARIABLE vFileName   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResultCode AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResultMes  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAvail      AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vFind       AS LOGICAL    NO-UNDO INIT NO.
   DEFINE VARIABLE vMsg        AS CHARACTER  NO-UNDO. 
   DEFINE VARIABLE vBuffer     AS LONGCHAR   NO-UNDO.

   DEFINE BUFFER   bFExch     FOR FileExch.
   DEFINE BUFFER   Packet     FOR Packet.
   DEFINE BUFFER   PackObject FOR PackObject.

   DEFINE VARIABLE vFlagSet    AS LOGICAL     NO-UNDO     INIT ?.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:

   {do-retry.i MAIN}

   ASSIGN
      hExch      = hIns:DEFAULT-BUFFER-HANDLE
      vPacketID  = hExch:BUFFER-FIELD("PacketID"):BUFFER-VALUE
      vFileID    = hExch:BUFFER-FIELD("FileExchID"):BUFFER-VALUE
      vSeanceID  = hExch:BUFFER-FIELD("SeanceID"):BUFFER-VALUE 
   NO-ERROR. {&ON-ERROR}

   FOR FIRST Seance WHERE Seance.SeanceID EQ vSeanceID NO-LOCK,
       FIRST FileExch WHERE FileExch.FileExchID EQ vFileID
       NO-LOCK:

      COPY-LOB FROM FILE FileExch.Path TO vBuffer  
               NO-CONVERT NO-ERROR.

      vMsg = STRING(SUBSTRING(vBuffer,1,30000)).

      {empty ttKvt}

      RUN ReadXMlFile(FileExch.Path).

      FOR EACH ttKvt:
         hExch:BUFFER-FIELD(GetMangledName(ttKvt.AttrCode)):BUFFER-VALUE = ttKvt.AttrValue.
         CASE ttKvt.AttrCode:
            WHEN "�����" THEN 
              vFileName = ttKvt.AttrValue.
            WHEN "�������஢�ન" THEN 
              vResultCode = ttKvt.AttrValue.
         END CASE.
         &IF DEFINED (IS-DEBUG) &THEN
            RUN dbgprint.p ("TAXFileKvt", GetNullStr(ttKvt.AttrCode) + ":" + GetNullStr(ttKvt.AttrVAlue)).    
         &ENDIF
      END.

      &IF DEFINED (IS-DEBUG) &THEN
         RUN DumpObject(hExch).
      &ENDIF
      RUN PacketSaveAll(vPacketID,
                        hExch:Name,
                        IF vResultCode EQ "01" THEN "����" ELSE "����",
                        vMsg).

      RUN FileMoveArchive IN h_filex (vPacketID, "ImpArch") NO-ERROR.

      &IF DEFINED (IS-DEBUG) &THEN
          RUN dbgprint.p ("TAXFileKvt", "vFileName:" + GetNullStr(vFileName)).    
      &ENDIF


      IF {assigned vFileName} THEN DO:
         FOR FIRST bFExch WHERE
            bFExch.NAME EQ vFileName
            NO-LOCK,
         EACH Packet WHERE 
            Packet.FileExchID EQ bFExch.FileExchID AND
            Packet.Class-Code EQ "PFTS"
         EXCLUSIVE-LOCK:
            vFind = YES.
         &IF DEFINED (IS-DEBUG) &THEN
             RUN dbgprint.p ("TAXFileKvt", "vResultCode:" + vResultCode).    
         &ENDIF

            IF vResultCode EQ '01' THEN
               Packet.State = "����".
            ELSE 
               Packet.State = "����".

            RUN PacketCreateLink IN h_pack (vPacketID,
                                            "Packet",
                                            Packet.PacketID,
                                            "TTaxKVT"). {&ON-ERROR}
          
            RUN PacketCreateLink IN h_pack (Packet.PacketID,
                                            "Packet",
                                            vPacketID,
                                            "TTaxKVT"). {&ON-ERROR}

            FOR FIRST PackObject WHERE
               PackObject.file-name EQ "acct" AND
               PackObject.PacketID EQ Packet.PacketID
            EXCLUSIVE-LOCK:

            &IF DEFINED (IS-DEBUG) &THEN
                RUN dbgprint.p ("TAXFileKvt", "acct:" + PackObject.Surrogate).    
            &ENDIF

               RUN PacketCreateLink IN h_pack (vPacketID,
                                               "acct",
                                               PackObject.Surrogate,
                                               "TTaxKVT"). {&ON-ERROR}
            END.
         END.
      END.
      IF NOT vFind OR 
         NOT {assigned vFileName} THEN DO:
         RUN PacketSetState IN h_pack (vPacketID, {&STATE-ERR}).
         RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������� ��室��� ᮮ�饭�� (" + 
                                                   vFileName + "). ").
      END.
   END.

   vFlagSet = YES.
END.     /* MAIN */
{doreturn.i vFlagSet}
END PROCEDURE.

{pfuncdef 
   &DefProc="TAXExpNff"
   &PARAMETERS    = "iClass,hIns"
   &Description="��ᯮ��  ���⠭樨 PB2" }

PROCEDURE TAXExpNff:
   DEFINE INPUT  PARAMETER iClass AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iHIns   AS HANDLE      NO-UNDO.

   DEFINE BUFFER Packet       FOR Packet.
   DEFINE BUFFER bPacket      FOR Packet.

   DEFINE VARIABLE vFlagSet       AS LOGICAL     NO-UNDO INIT ?.
   DEFINE VARIABLE vHExch          AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vFmt           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSep           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPacketID      AS INT64       NO-UNDO.
   DEFINE VARIABLE vKind          AS CHAR        NO-UNDO.
   DEFINE VARIABLE vSeanceID      AS INT64       NO-UNDO.
   DEFINE VARIABLE vResult        AS CHAR        NO-UNDO.
   DEFINE VARIABLE vSrcCP         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFile          AS CHAR        NO-UNDO.
   DEFINE VARIABLE vText          AS CHAR        NO-UNDO.
   DEFINE VARIABLE vI             AS INT64       NO-UNDO.
   DEFINE VARIABLE vPreFile       AS CHAR        NO-UNDO.

   DEF BUFFER FileExch  FOR FileExch.
   DEF BUFFER Seance    FOR Seance.
   DEF BUFFER Mail-User FOR Mail-User.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      vHExch       = iHIns:DEFAULT-BUFFER-HANDLE                        
      vSrcCP      = TRNSettingValue("", "CodePage", "DOS")            
      vSep        = TRNSettingValue("", "TagSep", ":")                
      vPreFile    = TRNSettingValue("", "Pref", "PB2")                
      vFmt        = vHExch::mail-format                                
      vPacketID   = vHExch::PacketID                                   
      vKind       = vHExch::Kind                                       
      vSeanceID   = INT64(vHExch::SeanceID)                            
      vResult     = ""                                                
   NO-ERROR. {&ON-ERROR}

USR-SEL:
   REPEAT:
      RUN browseld.p ("Packet",
                      "PackDate1"    + CHR(1) + "PackDate2"   + 
                      CHR(1) +  "PackClass"  + 
                      CHR(1) + "PackKind"  + CHR(1) + "RetRcp" +
                      CHR(1) + "RetType",
                      STRING(TODAY)  + CHR(1) + STRING(TODAY) + 
                      CHR(1) +  "PTax*"      + 
                      CHR(1) + "ETax*"     + CHR(1) +  STRING(TEMP-TABLE ttRetVal:HANDLE) + 
                      CHR(1) + "Multi",
                      "",
                      4).

      IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN DO:
         mRetryError = "�⪠� �� �믮������ �ᯮ��".
         vFlagSet    = NO.
         LEAVE MAIN.
      END.

      IF NOT CAN-FIND(FIRST ttRetVal) THEN DO:
         mRetryError = "��� ᮮ�饭�� ��� �ᯮ��".
         vFlagSet  = NO.
         LEAVE MAIN.
      END.

      vI = 0.
      FOR EACH ttRetVal:
         vI = vI + 1.
      END.

      IF vi GT 1 THEN DO:
         mRetryError = '����室��� ����� 1 ᮮ�饭�� � ���ࠢ������ "������"'.
         vFlagSet  = NO.
         LEAVE MAIN.
      END.
      ELSE DO:
         RUN messmenu.p (10,
                         "���������",
                         "�믮����� ᮧ����� ����ᮢ ��� �⬥祭���� ����饭�� ?",
                         "��,���").
         CASE pick-value:
            WHEN "2"  THEN DO:
               mRetryError = "�⪠� �� �믮������ �ᯮ��".
               vFlagSet    = NO.
               LEAVE MAIN.
            END.
            WHEN "0" THEN NEXT  USR-SEL.
            WHEN "1" THEN DO:
               FOR EACH ttRetVal,
                  FIRST Packet WHERE
                     ROWID(Packet) EQ ttRetVal.FileRowId
               NO-LOCK:
                  LEAVE USR-SEL.
               END.
            END.
         END CASE.
      END.
   END.                                       /* USR-SEL: REPEAT:          */

   IF NOT AVAIL Packet THEN DO:
      mRetryError  = "�� ������� ᮮ�饭�� ������".
         vFlagSet  = NO.
         LEAVE MAIN.
   END.


   FOR FIRST bPacket WHERE
             bPacket.PacketID EQ Packet.PacketID
       NO-LOCK,  
       FIRST FileExch WHERE
             FileExch.FileExchID EQ bPacket.FileExchID
             NO-LOCK:
      vFile = FileExch.NAME.
   END.

   FORM
     "�����:" vText VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 18
   WITH FRAME f1 OVERLAY CENTERED NO-LABEL ROW 2 TITLE COLOR bright-white
     "[ ������� ������������� ���������� ]".

ON F1 OF vText IN FRAME f1 DO:
   DO:
      pick-value = "".
      DO TRANSACTION:
         RUN pclass.p ("��獥��365","��獥��365","��稭� ���ᯮ������", 3).
      END.
      IF KEYFUNCTION(LASTKEY) NE "END-ERROR"
         AND {assigned pick-value} THEN 
         SELF:SCREEN-VALUE = GetCodeName("��獥��365", pick-value).
      RETURN.
   END.
END.


   UPDATE
      vText
   WITH FRAME f1.

   ASSIGN
      vResult = ENTRY(1,vFile,".")    + "###~n" +
                "35;" + vText                    + "@@@~n" +
                STRING(YEAR(TODAY),"9999") + "-" + 
                STRING(MONTH(TODAY),"99")  + "-" + 
                STRING(DAY(TODAY),"99")          + "@@@~n" +
                STRING(TIME,"hh:mm:ss")          + "@@@~n===~n"
      vFile   = vPreFile + ENTRY(1,vFile,".")    + ".txt"
   .

   vHExch:buffer-field("FileName"):buffer-value = vFile.

   FIND FIRST Seance WHERE Seance.SeanceID EQ vSeanceID
                       AND (IF   shMode
                           THEN Seance.Filial-ID EQ shFilial
                           ELSE YES)
      NO-LOCK NO-ERROR.

   FIND FIRST Mail-User WHERE Mail-User.op-kind-exp  EQ Seance.op-kind
                          AND (IF   shMode
                               THEN Mail-User.Filial-ID EQ shFilial
                               ELSE YES)
   NO-LOCK NO-ERROR.

   RUN PacketCreate IN h_pack (INPUT  Seance.SeanceID,
                               INPUT  -1,
                               INPUT  Mail-User.Mail-User-Num,
                               INPUT  vKind,
                               OUTPUT vPacketID) NO-ERROR. {&ON-ERROR}

   IF NOT  UpdateSigns("Packet",
                      STRING(vPacketID),
                      "FileName",
                      vHExch:buffer-field("FileName"):buffer-value, 
                      ?)  THEN
      UNDO MAIN, RETRY MAIN.

   RUN PacketTextSave IN h_pack (vPacketID, vResult).
   RUN Fill-SysMes IN h_tmess ("","","1","�࠭����� " + GetBaseOpKind() + " �믮����� �ᯥ譮").
   RUN Fill-SysMes IN h_tmess ("","","1","������� ᮮ�饭�� �ᯮ�� � ������ 䠩�� " + vFile).

   vFlagSet = YES.

END.     /* MAIN */
{doreturn.i vFlagSet mRetryError}
END PROCEDURE.


PROCEDURE ReadXMlFile.
   DEFINE INPUT  PARAMETER iFileName  AS CHAR        NO-UNDO.

   DEFINE VAR h_sphere     AS handle               NO-UNDO.
   DEFINE VAR hSAX         AS handle               NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      CREATE sax-reader hSAX.
      hSAX:HANDLER = THIS-PROCEDURE.
      hSAX:SET-INPUT-SOURCE("file",iFileName).
      hSAX:SAX-PARSE().
   END.

   hSAX:STOP-PARSING().
   IF valid-handle(hSAX)   THEN DELETE object    hSAX.
END.

PROCEDURE StartElement:
   DEFINE INPUT PARAMETER iNameSpaceURI AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iLocalName    AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iQName        AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iAttributes   AS HANDLE  NO-UNDO.

   DEFINE VARIABLE vCnt                 AS INT64   NO-UNDO.
   DEFINE VARIABLE vAttrCode            AS CHAR    NO-UNDO.
   DEFINE VARIABLE vAttrValue           AS CHAR    NO-UNDO.

    DO vCnt =  1 TO iAttributes:NUM-ITEMS:            /* ��ਡ���        */
       CREATE ttKvt.
       ASSIGN
          ttKvt.AttrCode  = iAttributes:GET-QNAME-BY-INDEX(vCnt).  /* ���        */
          ttKvt.AttrValue = iAttributes:GET-VALUE-BY-INDEX(vCnt).  /* ���祭��   */

       &IF DEFINED(IS-DEBUG) &THEN
       RUN dbgprint.p ("StartElement","vAttrCode:"   + ttKvt.AttrCode +
                                      " vAttrValue:" + ttKvt.AttrValue).
       &ENDIF
   END.
END.

PROCEDURE Characters:
   DEFINE INPUT PARAMETER iCharData AS MEMPTR  NO-UNDO.
   DEFINE INPUT PARAMETER iNumChars AS INTEGER NO-UNDO.
END.

PROCEDURE EndElement:
   DEFINE INPUT PARAMETER iNameSpaceURI AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iLocalName    AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER iQName        AS CHAR    NO-UNDO.
END.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/07/2015 16:18:24.729+04:00' */
/* $LINTUSER='trig' */
/* $LINTMODE='1' */
/* $LINTFILE='pp-itax.p' */
/*prosignEqSZs4kxAGjR56LL6/n7Zw*/