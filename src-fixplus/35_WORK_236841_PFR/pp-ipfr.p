/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-ipfr.p
      Comment: 0161576 ������� �㭪樮���. ����� � ��� - ������� � ���⠢�� ���ᨩ
   Parameters: ���
         Uses:
      Used by:
      Created: 16/05/2013 KMBIS 0161576 ������� �㭪樮���. ����� � ��� - ������� � ���⠢�� ���ᨩ
                                        ������⥪� ��� ᮧ����� ���㬥�⮢ �� ����祭�� ����� �� 
                                        ��� (�ଠ� ���)
     Modified: 03/11/2015 KMBIS 0236841 ������. ����㧪� ॥��� ���᫥��� ���ᨩ �� ��
                                        ����� � �� ���. ��⠫�� �ଠ�� 㤠����.
     Modified: 
*/
{globals.i}

{g-trans.equ}
{intrface.get pbase}
{intrface.get op}
{intrface.get exch}
{intrface.get pack}
{intrface.get count}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get filex}
{intrface.get trans}
{intrface.get strng} 
{intrface.get xclass} 

{intrface.get tmess}
{intrface.get instrum}

DEF VAR mCount     AS INT64 NO-UNDO.   
DEF VAR mCountDoc  AS INT64 NO-UNDO.   /* ���稪 ���㬥�⮢ � ����ᮬ ��� �                      */
DEF VAR mCountErr  AS INT64 NO-UNDO.   /* ���稪 ���㬥�⮢ � ����� �                             */
DEF VAR mCountAnn  AS INT64 NO-UNDO.   /* ������⢮ ���㫨஢����� ���㬥�⮢                       */
DEF VAR mContDef   AS INT64 NO-UNDO.   /* ����� � ��䥪⭮� ��������                                */
DEF VAR mTotalCnt  AS INT64 NO-UNDO.   /* ������⢮ ��ࠡ�⠭��� �����⮢                           */
DEF VAR mAmt       AS DEC   NO-UNDO.   /* �㬬� ���᫥���� ���㬥�⮢                               */
DEF VAR mAmtErr    AS DEC   NO-UNDO.   /* �㬬� ���㬥�⮢ ᮧ������ � ����� �                     */
DEF VAR mFileName  AS CHAR  NO-UNDO.                                                                
DEF VAR mFileLog   AS CHAR  NO-UNDO.                                                                
DEF VAR mVedDef    AS CHAR  NO-UNDO.   /* ���� �訡�� ��� ��䥪⭮� ��������                        */
DEF VAR mVedClose  AS CHAR  NO-UNDO.   /* ���� �訡�� ��� �������� �����᫥���� �㬬 (��� ������) */

ASSIGN
   mVedDef    = fGetSetting("��������","�����䥪�", "" )
   mVedClose  = fGetSetting("��������","�������",   "" )
.

/*----------------------------------------------------------------------------*/
/* ������塞 �� ����� ४������ � ᮮ⢥�ᢨ� � �����⮬                     */
/*----------------------------------------------------------------------------*/

PROCEDURE UpdPacketSigns:
   DEF INPUT PARAM iHBuff AS HANDLE NO-UNDO.

DEF VAR vPackSur  AS CHAR  NO-UNDO.
DEF VAR vSeanceID AS INT64 NO-UNDO.
DEF VAR vPackId   AS CHAR  NO-UNDO.
DEF VAR vClass    AS CHAR  NO-UNDO.
DEF VAR vDefErr   AS CHAR  NO-UNDO.
DEF VAR vDefClose AS CHAR  NO-UNDO.

DEF BUFFER bPackObject  FOR PackObject.
DEF BUFFER bPpfr        FOR Packet.

   ASSIGN
       vPackSur  = SUBST("&1,1", TRIM(iHBuff::op))
       vSeanceID = INT64(TRIM(iHBuff::SeanceID))
   NO-ERROR.

   FOR FIRST bPackObject WHERE bPackObject.SeanceID  EQ vSeanceId
                           AND bPackObject.file-name EQ "op-entry"  
                           AND bPackObject.Surrogate EQ vPackSur
                         NO-LOCK,
      FIRST bPpfr WHERE bPpfr.PacketId EQ bPackObject.PacketId
                  NO-LOCK:
      ASSIGN
         vPackId            = STRING(bPpfr.PacketId)
         vClass             = bPpfr.class-code
         vDefErr            = ""
         vDefClose          = ""
         /*=== �������� �� ��直� ��砩 ����� ===*/
         iHBuff::RetPacketId = vPackId
      NO-ERROR.

      UpdateSignsEx(vClass, vPackId, "Hash",        TRIM(iHBuff::Hash)).
      UpdateSignsEx(vClass, vPackId, "RegNumPFR",   TRIM(iHBuff::RegNumPFR)).
      UpdateSignsEx(vClass, vPackId, "Year",        TRIM(iHBuff::Year)).
      UpdateSignsEx(vClass, vPackId, "Month",       TRIM(iHBuff::Month)).
      UpdateSignsEx(vClass, vPackId, "InsurNum",    TRIM(iHBuff::InsurNum)).
      UpdateSignsEx(vClass, vPackId, "name-rec",    TRIM(iHBuff::name-rec)).
      UpdateSignsEx(vClass, vPackId, "amt-rub",     TRIM(iHBuff::amt-rub)).
      UpdateSignsEx(vClass, vPackId, "Code",        TRIM(iHBuff::Code)).
      UpdateSignsEx(vClass, vPackId, "CodeOpr",     TRIM(iHBuff::CodeOpr)).
      UpdateSignsEx(vClass, vPackId, "CodeInf",     TRIM(iHBuff::CodeInf)).
      UpdateSignsEx(vClass, vPackId, "V_BNK",       TRIM(iHBuff::V_BNK)).
      UpdateSignsEx(vClass, vPackId, "Acct",        TRIM(iHBuff::Acct)).
      UpdateSignsEx(vClass, vPackId, "acct-cr",     TRIM(iHBuff::acct-cr)).
      UpdateSignsEx(vClass, vPackId, "details",     TRIM(iHBuff::details)).
      UpdateSignsEx(vClass, vPackId, "RegPFR",      TRIM(iHBuff::RegPFR)).
      UpdateSignsEx(vClass, vPackId, "ImpPacketId", TRIM(iHBuff::ImpPacketId)).
      UpdateSignsEx(vClass, vPackId, "RetCodePFR",  TRIM(iHBuff::RetCodePFR)).

      IF {assigned bPpfr.PackError} THEN
      DO:
         vDefClose = ListsCrossing(bPpfr.PackError, mVedClose, ",").
         IF NOT {assigned vDefClose} THEN
            vDefErr = ListsCrossing(bPpfr.PackError, mVedDef  , ",").

         IF {assigned vDefErr} THEN
         DO:
            mContDef = mContDef + 1.
            UpdateSignsEx(vClass, vPackId, "VedNum", STRING(mContDef)).

         END. /* IF {assigned vDefClose} THEN */
      END. /* IF {assigned bPpfr.PackError} THEN */
   END. /* FOR FIRST bPackObject WHERE bPackObject.SeanceID  EQ vSeanceId */

END PROCEDURE. /* UpdPacketSigns */

/*===============================================================================================*/

/*----------------------------------------------------------------------------*/
/* ��襬 � ���-䠩�                                                           */
/*----------------------------------------------------------------------------*/

PROCEDURE WriteLog:
   DEF INPUT PARAM iStr AS CHAR NO-UNDO.

   OUTPUT TO VALUE(mFileLog) APPEND.
   IF iStr EQ "" THEN
      PUT UNFORMATTED SKIP(1).

   IF {assigned iStr}THEN
      PUT UNFORMATTED iStr SKIP. 

   OUTPUT CLOSE.

END PROCEDURE. /* WriteLog */

/*===============================================================================================*/

/*----------------------------------------------------------------------------*/
/* ���� ��������� ��६����� �� ���祭�� �� 㬮�砭��                       */
/*----------------------------------------------------------------------------*/

PROCEDURE NulMainVar:

   ASSIGN
      mCount    = 0
      mCountDoc = 0
      mCountErr = 0
      mCountAnn = 0
      mContDef  = 0
      mAmt      = 0
      mAmtErr   = 0
      mTotalCnt = 0  
      mFileLog  = ""
      mFileName = ""
   .

END PROCEDURE. /* NulMainVar */

/*===============================================================================================*/


/*===============================================================================================*/
/*=== ���樠������ ������ 䠩��: ���. ��⮤ �� ��������� format-FH ==========================*/
PROCEDURE PFRInitOmsFH:
   DEF INPUT PARAM iClass AS CHAR   NO-UNDO.
   DEF INPUT PARAM iHBuff  AS HANDLE NO-UNDO.

DEF VAR vFlagSet   AS LOG   NO-UNDO INIT ?.
DEF VAR vTimeStamp AS CHAR  NO-UNDO. 
DEF VAR vLogName   AS CHAR  NO-UNDO. 

   MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      /*=== ����塞 ��६���� ===*/
      RUN NulMainVar IN THIS-PROCEDURE.

      ASSIGN
         /*=== ��� ����㦠����� 䠩�� ===*/
         mFileName  = FileGetPath(INT64(iHBuff::FileExchID))
         mFileName  = GetPureName(mFileName)

         /*=== �६����� �⬥⪠ ����㧪� ===*/
         vTimeStamp = REPLACE(STRING(TIME, "HH:MM:SS"), ":", "")

         /*=== ��� ���-䠩�� */
         vLogName   = SUBST("&1_&2-&3.log", mFileName, iHBuff::SeanceID, vTimeStamp)

         /*=== ����� ���� � ���-䠩�� ===*/
         mFileLog   = CatalogGetPath(INT64(iHBuff::mail-user-num), "LogArch", "Path")
         mFileLog   = MakeFileName(mFileLog, vLogName)
      NO-ERROR.

      IF NOT ({assigned mFileLog} AND {assigned mFileName}) THEN  
      DO:
         IF NOT {assigned mFileLog} THEN  
            RUN Fill-SysMes IN h_tmess ("","","-1", "�� ����� ��娢�� ��⠫�� ��� 䠩���").

         IF NOT {assigned mFileName} THEN  
            RUN Fill-SysMes IN h_tmess ("","","-1", "��� �������㥬��� 䠩�� �� ��।�����").
      END. /* IF NOT ({assigned mFileLog} AND {assigned mFileName}) THEN */
      ELSE
      DO:
         /*=== ��襬 � ��� ��� �������㥬��� 䠩�� ===*/
         RUN WriteLog IN THIS-PROCEDURE(SUBST("����: &1", mFileName)).
         RUN WriteLog IN THIS-PROCEDURE("") .
         vFlagSet = YES.
      END. /* IF NOT ({assigned mFileLog} AND {assigned mFileName}) THEN ... ELSE */ 

   END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

   {doreturn.i vFlagSet}

END PROCEDURE. /* PFRInitOmsFH */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== �믮���� ������ ������ ���㬥��: ��� ===================================================*/
PROCEDURE PFRImportOmsDT:
   DEF INPUT PARAM iClass AS CHAR   NO-UNDO.
   DEF INPUT PARAM iHBuff  AS HANDLE NO-UNDO.

DEF VAR vFlagSet     AS LOG    NO-UNDO INIT ?.
                              
DEF VAR vYear        AS CHAR   NO-UNDO.
DEF VAR vMonth       AS INT64  NO-UNDO.
DEF VAR vFIO         AS CHAR   NO-UNDO.
DEF VAR vAcct        AS CHAR   NO-UNDO.
DEF VAR vRegPfr      AS CHAR   NO-UNDO.
DEF VAR vDetails     AS CHAR   NO-UNDO.
DEF VAR vCounter     AS CHAR   NO-UNDO.
DEF VAR vAmt         AS DEC    NO-UNDO.
DEF VAR vDoc-Num     AS INT64  NO-UNDO.
DEF VAR vTmpStr      AS CHAR   NO-UNDO.

DEF BUFFER bOP FOR op.

   MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         /*=== ��⠥� ��ப� ===*/
         mTotalCnt      = mTotalCnt + 1

         vMonth         = INT64(TRIM(iHBuff::Month))
         vYear          = SUBST("20&1", TRIM(iHBuff::Year))
         vAcct          = TRIM(iHBuff::acct)
         vFIO           = SUBST("&1 &2 &3", iHBuff::fam, iHBuff::name, iHBuff::surname)
         vFIO           = RemoveDoubleChars(TRIM(vFIO), " ")
         vAmt           = DEC(iHBuff::Amt)

         /*=== C��稪 �㬥�樨 ���㬥�⮢ ===*/
         vCounter       = iHBuff::counter

         /* ��⠥� ����� �㬬� �� ॥���� ===*/
         mAmt           = mAmt + vAmt
      NO-ERROR. {&ON-ERROR}

      RUN WriteLog IN THIS-PROCEDURE(SUBST("��ப� &1 &2 &3",
                                           STRING(mTotalCnt, ">>>>"),
                                           vAcct,
                                           STRING(vFIO, "x(30)"))).

      IF {assigned vCounter} THEN
      DO:
         /*=== ���稪 - �饬 ���祭�� �� �����䨪���� Counters ===*/
         vDoc-Num = SetCounterValue(vCounter, ?, gend-date) NO-ERROR. 
         IF vDoc-Num EQ ? THEN
         DO:
            vTmpStr = SUBST("�訡�� ����祭��/��᢮���� ���祭�� ��� ���稪� [&1].", vCounter).
            RUN WriteLog IN THIS-PROCEDURE(vTmpStr).
            RUN WriteLog IN THIS-PROCEDURE("").
            RUN Fill-SysMes IN h_tmess ("", "", "-1", vTmpStr).
            RETURN ERROR vTmpStr.
         END.
      END.
      ELSE /*=== ��筠� ������� ����� ���㬥�� ===*/
         vDoc-Num = mCountDoc + mCountAnn + mCountErr + 1.

      /*=== ���࠭�� ࠩ�� ��� ===*/
      IF LENGTH(mFileName) GE 8 THEN 

      ASSIGN
         vRegPFR       = SUBSTR(mFileName, 7,2)    WHEN LENGTH(mFileName) GE 8
         vDetails      = SUBST("����᫥��� ���ᨨ �� &1 &2 ���� ᮣ��᭮ ॥��� &3",
                               {rsdate.i vMonth}, /* ����� �ய���� */
                               vYear,
                               STRING(gend-date, "99.99.9999"))
         iHBuff::acct-cr  = vAcct
         iHBuff::acct-rec = vAcct
         iHBuff::name-rec = vFIO
         iHBuff::RegPFR   = vRegPFR
         iHBuff::amt-rub  = STRING(vAmt)
         iHBuff::details  = vDetails
         iHBuff::doc-num  = STRING(vDoc-Num)
      .

      /*=== ������� ���㬥�� ===*/
      RUN RunTransaction IN h_pbase(iHBuff::op-kind) NO-ERROR. 

      FIND FIRST bOP WHERE bOP.op EQ INT64(iHBuff::op) 
                       AND bOP.op NE 0
                     NO-LOCK NO-ERROR.

      IF AVAIL(bOP) THEN
      DO:
         /*=== ���࠭�� ����� ���⥦� �� �� ����� ===*/
         RUN UpdPacketSigns IN THIS-PROCEDURE(iHBuff). 

         /*=== ��⠥� �ᯥ譮 ᮧ����� ���㬥��� ===*/
         mCountDoc = mCountDoc + 1.

      END. /* IF AVAIL(op) THEN */
      ELSE
      DO:
         /*=== �᫨ �� ��ப� �� ᮧ���� �஢����, ᮧ����� ����� ᠬ� ===*/
         vTmpStr = "�� 㤠���� ᮧ���� ���㬥�".
         RUN WriteLog IN THIS-PROCEDURE(vTmpStr).
         RUN WriteLog IN THIS-PROCEDURE("").
         RUN Fill-SysMes IN h_tmess ("", "", "-1", vTmpStr).
         RETURN ERROR vTmpStr.
      END. /* IF AVAIL(op) THEN */

      /* �ᮡ������� crdiresp: ��᫥ ⮣� ��� ��ப� ��ࠡ�⠭� � �� �ॡ����, �㦭� 㤠���� ��. */
      /* ���� ����� ���� ᫮���� ��।������ PacketId �� ������ ��᪮�쪨� 䠩��� �����६���� */
      RUN InstanceJunk IN h_exch(iHBuff, 0).

      vFlagSet = YES.

   END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN. */

   {doreturn.i vFlagSet}

END PROCEDURE. /* PFRImportOmsDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== �믮���� ������ 墮�⮢���: ��� =========================================================*/
PROCEDURE PFRImportOmsFF:
   DEF INPUT PARAM iClass AS CHAR   NO-UNDO.
   DEF INPUT PARAM iHBuff  AS HANDLE NO-UNDO.

DEF VAR vFlagSet         AS LOG      NO-UNDO INIT ?.
DEF VAR vPackID          AS INT64      NO-UNDO.

DEF VAR vAmtOp           AS DEC      NO-UNDO.
DEF VAR vCntOp           AS INT64    NO-UNDO.

DEF BUFFER bPacket      FOR Packet.
DEF BUFFER bPackObject  FOR PackObject.
DEF BUFFER bOp          FOR op.
DEF BUFFER bOpEntry     FOR op-entry.

   MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vPackID = INT64(TRIM(iHBuff::PacketID))
      NO-ERROR.

      RUN WriteLog IN THIS-PROCEDURE("").

      FOR EACH bPacket WHERE bPacket.ParentID EQ vPackID
                         AND vPackID          GT 0
                       NO-LOCK,
         FIRST bPackObject WHERE bPackObject.PacketId  EQ bPacket.PacketId
                             AND bPackObject.file-name EQ "op-entry"  
                            NO-LOCK,
         FIRST bOp WHERE bOp.op EQ INT64(ENTRY(1, bPackObject.Surrogate))
                   NO-LOCK,
         FIRST bOpEntry of bOp NO-LOCK:

         IF NOT (bOp.op-status GT "�") THEN
         DO:
            ASSIGN
               mCountErr = mCountErr + 1
               mAmtErr   = mAmtErr   + bOpEntry.amt-rub
            .

            RUN WriteLog IN THIS-PROCEDURE(SUBST("���㬥�� ����� &1 �㬬� &2 �訡��:",
                                                 bOp.doc-num,
                                                 STRING(bOpEntry.amt-rub, ">>>,>>>,>>9.99"))).
            {empty ttError}
            RUN FillErrorTable IN h_exch(bPacket.ClassError, 
                                         bPacket.PackError, 
                                         OUTPUT TABLE ttError).
            FOR EACH ttError NO-LOCK:
               RUN WriteLog IN THIS-PROCEDURE(SUBST("     &1", ttError.name)).

            END. /* FOR EACH ttError NO-LOCK: */
         END. /* IF NOT (bOp.op-status GT "�") THEN */
         ELSE
         DO:
            ASSIGN
               vCntOp = vCntOp + 1               
               vAmtOp = vAmtOp + bOpEntry.amt-rub
            .
         END. /* IF NOT (bOp.op-status GT "�") THEN ... ELSE */
      END. /* FOR FIRST bPackObject */

      FOR FIRST bPacket WHERE bPacket.PacketId EQ vPackID
                          AND vPackID          GT 0
                        NO-LOCK:
         UpdateSignsEx(bPacket.Class-code, STRING(vPackId), "TotalCnt", STRING(mTotalCnt)).
         FILE-INFO:FILE-NAME = mFileLog.
         UpdateSignsEx(bPacket.Class-code, STRING(vPackId), "LogName", FILE-INFO:FULL-PATHNAME).
      END. /* FOR FIRST bPacket WHERE bPacket.PacketId EQ vPackID */

      RUN WriteLog IN THIS-PROCEDURE("").
      RUN WriteLog IN THIS-PROCEDURE(SUBST("�ᥣ� ��ப:      &1", STRING(mTotalCnt))).
      RUN WriteLog IN THIS-PROCEDURE(SUBST("�ᥣ� �� ॥����: &1", STRING(mAmt))).
      RUN WriteLog IN THIS-PROCEDURE("").

      RUN WriteLog IN THIS-PROCEDURE(SUBST("���㬥�⮢ ��� �訡��: &1 � �㬬� &2", 
                                     STRING(vCntOp, ">>>9"), 
                                     STRING(vAmtOp,   ">>>,>>>,>>9.99"))).
      RUN WriteLog IN THIS-PROCEDURE(SUBST("���㬥�⮢ � �訡����: &1 � �㬬� &2", 
                                     STRING(mCountErr, ">>>9"), 
                                     STRING(mAmtErr,">>>,>>>,>>9.99"))).
      RUN WriteLog IN THIS-PROCEDURE("").

      vFlagSet = YES.
   END.

   {doreturn.i vFlagSet}

END PROCEDURE. /* PFRImportOmsFF */

/*===============================================================================================*/
