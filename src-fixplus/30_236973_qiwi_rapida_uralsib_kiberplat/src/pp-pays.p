/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-pays.p
      Comment: ������⥪� ��楤�� ������ � ���⥦�묨 ��⥬��� ������
   Parameters: ���
         Uses:
      Used BY:
      Created: 23.04.2015 KMBIS TT:0236973 ������.�ਥ� ���⥦�� QIWI, ������, �ࠫᨡ, ���������
     Modified: 
*/
{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "pays"
   &LIBNAME       = "������ ����� � ���⥦�묨 ��⥬���"
   &DESCRIPTION   = "��⮤� ������ ������"
}
{globals.i}
{exchange.equ}

&SCOP NO-BASE-PROC YES

DEF VAR mFileID  AS  INT64  NO-UNDO.

DEF TEMP-TABLE ttErrPay NO-UNDO
   FIELD PayRef    AS CHAR  /* �������� ����� ��ॢ��� � ��� */
   FIELD amt-rub   AS DEC   /* �㬬� ��ॢ���                  */
   FIELD OrderAcct AS CHAR  /* ����� ���                     */
   FIELD PayErr    AS CHAR  /* ��� ��稭� ������            */
.

{intrface.get kau}
{intrface.get card}
{intrface.get exch}
{intrface.get pack}
{intrface.get xrkc}
{intrface.get blkob}
{intrface.get count}
{intrface.get pbase}
{intrface.get refer}
{intrface.get rfrnc}
{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get xclass}

/* ��楤�� 㤠����� ���㬥�⮢ */
{pck-op-del.pro}

{pays.fun}
{pays.pro}


/*===============================================================================================*/
/*=== ������ ������ ����� �� ��������� ========================================================*/
{pfuncdef 
   &DEFPROC="CPlatOnlImport"
   &DESCRIPTION="������ ������ ����� �� ���������"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE CPlatOnlImport:
   DEF INPUT PARAM iClass     AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iInstance  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG     NO-UNDO INIT ?.
DEF VAR vExch      AS  HANDLE  NO-UNDO.
DEF VAR vReq       AS  HANDLE  NO-UNDO. /* �����⥫� �� ⠡���� � ����묨 ���㬥�� */                            

DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatOnlImport", "RUN").
   &ENDIF

   ASSIGN
      vExch = iInstance:DEFAULT-BUFFER-HANDLE
      vReq  = WIDGET-HANDLE(GetAttrValue2(GetBaseOpKind(),0,"CPlatHandle"))
   NO-ERROR. {&ON-ERROR}

   RUN InstanceCreateEx IN h_exch (?, vExch, 0).

   /*=== ������塞 �������騥 ���� � ���⥦� ===*/
   ASSIGN
      /* ���� �����⥫�     */
      vExch::bank-code-rec      = vReq::bank-code-rec
      vExch::bank-corr-acct-rec = vReq::bank-corr-acct-rec

      /* �࠭���� ���     */
      vExch::acct-cr            = vReq::acct-cr    WHEN {assigned vReq::acct-cr}

      /* ����� �����⥫�   */
      vExch::acct-rec           = vReq::acct-rec   

      /* �㬬� � ���᫥���  */
      vExch::amt-rub            = vReq::amt-rub

      /* ��ࠢ�⥫� ���⥦� */

      /* ���७� ���㬥��  */
      vExch::SendREF            = vReq::SendREF


      /* ���࠭塞 � ��ࢮ��砫쭮� ⠡��� ����� ������ */
      vReq::SeanceID            = vExch::SeanceID
      vReq::SendREF             = SUBST("&1|&2", vExch::SendId, vExch::SendREF)

      /* ����ঠ��� ����樨  */
      vExch::details            = ParsDetails(vExch, vExch::details)

      /* �㬥��� ���㬥�⮢ */
      vExch::doc-num            = SetCounterValue(vExch::CounterName, ?, gend-date)

      /* ����᪠�� ������⨧��� ���㬥�� */
      vExch::op-kind            = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user)
   NO-ERROR. {&ON-ERROR}



   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatOnlImport", SUBST("SeanceId: &1, SendRef: &2, op-kind: &3",
                                           vExch::SeanceID,
                                           vReq::SendREF,
                                           vExch::op-kind)).
   RUN dbgprint.p ("CPlatOnlImport", SUBST("RunTransaction(&1)", vExch::op-kind)).
   &ENDIF

   /* �������� ���㬥�� */
   RUN RunTransaction IN h_pbase (vExch::op-kind) NO-ERROR. 
   /* ���࠭塞 op ���㬥�� */
   vReq::OpId = vExch::op.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN DumpObject IN h_exch(vExch).
   &ENDIF

   vFlagSet = YES.
   
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatOnlImport", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatOnlImport */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== �஢�ઠ �� 㭨���쭮��� ��७� ���㬥�� ==============================================*/
{pfuncdef 
   &DEFPROC="ChkOpRef"
   &DESCRIPTION="�஢�ઠ �� 㭨���쭮��� ��७� ���㬥��"
   &PARAMETERS="�����⥫� �� ���㬥��,ᯨ᮪ �訡�� ���㬥��"
   &RESULT="������� ����樨"
}
PROCEDURE ChkOpRef:
   DEF INPUT PARAM iWOP   AS HANDLE NO-UNDO.
   DEF INPUT PARAM iList  AS CHAR   NO-UNDO.

DEF VAR vFlagSet  AS  LOG     NO-UNDO INIT ?.
DEF VAR vSendREF  AS  CHAR    NO-UNDO.
DEF VAR vSendID   AS  CHAR    NO-UNDO.
DEF VAR vMailFmt  AS  CHAR    NO-UNDO.

DEF BUFFER bRef  FOR Reference.
DEF BUFFER bPack FOR packet.
DEF BUFFER bCode FOR code.

MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ChkOpRef", "RUN").
   &ENDIF

   ASSIGN
      vSendREF = iWOP::SendREF
      vSendID  = iWOP::SendID
      vMailFmt = iWOP::mail-format
   NO-ERROR. {&NO-ERROR}

   IF EXCH-MSGBuff(vMailFmt, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ChkOpRef", SUBST("vSendREF = &1; vSendID = &2; Op.op = &1; RefClass = &4", 
                                     vSendREF, 
                                     vSendID,
                                     STRING(iWOP::op),
                                     bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /* �஢��塞 ��७� ��� ��� ��� */
   FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}]
                    AND bRef.RefValue    EQ SUBST("&1|&2", vSendID, vSendREF)
                  NO-LOCK,
      FIRST bPack WHERE bPack.PacketID EQ bRef.PacketID
                  NO-LOCK:

      RUN AddErrorFormat IN h_exch (iWOP,iList).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ChkOpRef", SUBST("PackDate: &1; PAckTime: &2",
                                        STRING(bPack.PackDate, "99/99/9999"),
                                        STRING(bPack.PackTime, "HH:MM:SS"))).
      &ENDIF

   END. /* FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}] */

   vFlagSet = YES.
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ChkOpRef", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* ChkOpRef */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���樠������ �� ������ ��ப� �� ॥��� ===============================================*/
{pfuncdef 
   &DEFPROC="CPlatReeIni"
   &DESCRIPTION="���樠������ �� ������ ��ப� �� ॥���"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE CPlatReeIni:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vFileName AS  CHAR  NO-UNDO. /* ��� ����㦠����� 䠩�� */
DEF VAR vSeanceId AS  INT64 NO-UNDO. /* ����� ᥠ�� ������    */
DEF VAR vOpKind   AS  CHAR  NO-UNDO.
DEF VAR vMsg      AS  CHAR  NO-UNDO.

DEF BUFFER bFileExch FOR FileExch.
DEF BUFFER bSeance   FOR Seance.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeIni", "RUN").
   &ENDIF

   IF mFileID NE iExch::FileExchID THEN
   DO:
      /* ����஫� �� ������� ����㧪� */
      ASSIGN
         vOpKind   = iExch::op-kind
         mFileID   = INT64(iExch::FileExchID)
         vSeanceId = INT64(iExch::SeanceId)
      NO-ERROR. {&ON-ERROR}

      /* �饬 ��� 䠩�� */
      FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ mFileID 
                          NO-LOCK:
         vFileName = bFileExch.Name.
      END. /* FOR FIRST bFileExch WHERE bFileExch EQ mFileID  */

      IF {assigned vFileName} THEN
      DO:
         /* �஢��塞 �� �� �� ����㦥� 䠩� � ⥬ �� ������ */
         FOR EACH bFileExch WHERE bFileExch.Name     EQ vFileName 
                              AND bFileExch.SeanceId NE vSeanceId
                            NO-LOCK,
            FIRST bSeance WHERE bSeance.SeanceId EQ bFileExch.SeanceId
                            AND bSeance.op-kind  EQ vOpKind
                          NO-LOCK:
            vMsg = SUBST("���� '&1' 㦥 ����㦥�, ����� ᥠ�� ������ '&2' �� '&3'",
                         vFileName,
                         bSeance.Number,
                         STRING(bSeance.SeanceDate,"99/99/9999")).
            RUN Fill-SysMes IN h_tmess("", "", "0", vMsg).
         END. /* FOR EACH bFileExch WHERE bFileExch.Name     EQ vFileName  */
      END. /* IF {assigned vFileName} THEN */
   END. /* IF mFileID NE iExch::FileExchID THEN */

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeIni", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatReeIni */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ��ࠡ�⪠ ��ப� �⮣����� ॥��� ========================================================*/
{pfuncdef 
   &DEFPROC="CPlatReeImp"
   &DESCRIPTION="��ࠡ�⪠ ��ப� �⮣����� ॥���"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE CPlatReeImp:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG     NO-UNDO INIT ?.
DEF VAR vFlagPub   AS  LOG     NO-UNDO INIT NO.
DEF VAR vExchOp    AS  HANDLE  NO-UNDO. /* �����⥫� �� �࠭ᯮ���� ��� ���㬥�� �� ॥��� */
DEF VAR vSendREF   AS  CHAR    NO-UNDO. /* ���७� ���㬥�� �� ॥���                        */
DEF VAR vOpDate    AS  DATE    NO-UNDO. /* ��� ���㬥�� �� ॥���                            */
DEF VAR vSendID    AS  CHAR    NO-UNDO. /* �����䨪��� ��ࠢ�⥫� �� �࠭���樨              */
DEF VAR vPackId    AS  INT64   NO-UNDO. /* ����� ᮧ��������� �����                            */
DEF VAR vFilialId  AS  CHAR    NO-UNDO. /* ��� ���ࠧ������� �����⥫� ���⥦�                 */
DEF VAR vCorr      AS  CHAR    NO-UNDO. /* ����.��� ����� �����⥫� ���⥦�                   */
DEF VAR vAcctCr    AS  CHAR    NO-UNDO. /* �࠭���� ��� ���䨫������ ����⮢              */
DEF VAR vSaveFile  AS  LOG     NO-UNDO. /* ���࠭��� �� ��ࠡ��뢠��� ��ப�                   */
DEF VAR vBuffer    AS  CHAR    NO-UNDO. /* ��ࠡ��뢠���� ��ப� �� 䠩��                       */
DEF VAR vOpKind    AS  CHAR    NO-UNDO. /* ��� �࠭���樨 ��� ���᫥���                        */
DEF VAR vTmpStr    AS  CHAR    NO-UNDO.

DEF BUFFER bRef      FOR Reference.   
DEF BUFFER bPack     FOR packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOp       FOR op.
DEF BUFFER bPackInt  FOR packet.
DEF BUFFER bCode     FOR code.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   ASSIGN
      vTmpStr   = TRNSettingValue("","SaveFile","NO").
      vSaveFile = (vTmpStr EQ "��") OR (vTmpStr EQ "YES")
   .

   IF vSaveFile THEN
   DO:
      /* ����稬 ����㦥���� ��ப� � ��࠭�� �� �� ����� */
      PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).
      IF vFlagPub EQ YES THEN 
         RUN PacketTextSave IN h_pack(iExch::PacketID, vBuffer + "~n").

   END. /* IF vSaveFile THEN */

   IF GetSysConf("ErrorString") NE "NoError" THEN
   DO:
      IF NOT vSaveFile THEN 
         PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CPlatReeImp", SUBST("ERROR LINE '&1'", vBuffer)).
      &ENDIF
      IF NOT CAN-DO("�����: ���-�� ���⥦�� * �� �㬬� * ��.", vBuffer) THEN
         RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("��ப� �� ᮮ⢥���� �ଠ��: '&1'",
                                                       vBuffer)).

      vFlagSet = YES.
      LEAVE MAIN.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", "ASSIGN VALUES").
   &ENDIF

   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      iExch::DTPay         = TRIM(iExch::DTPay)
      iExch::number        = TRIM(SUBSTR(iExch::number, 1, 20))
      iExch::bank-code-rec = TRIM(iExch::bank-code-rec)
      iExch::abtype        = TRIM(iExch::abtype)
      iExch::amt-rub       = TRIM(iExch::amt-rub)
      iExch::receipt       = TRIM(iExch::receipt)
      iExch::SendID        = TRIM(iExch::SendID)
      vSendREF             = iExch::receipt
      vSendID              = iExch::SendID
      vTmpStr              = ENTRY(1, iExch::DTPay, "T")
      vTmpStr              = SUBST("&1/&2/&3",
                                   ENTRY(3, vTmpStr, "-"),
                                   ENTRY(2, vTmpStr, "-"),
                                   ENTRY(1, vTmpStr, "-"))
      vOpDate              = DATE(vTmpStr)
   NO-ERROR. {&NO-ERROR}

   RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("���⥦ ����� '&1' �� '&2'", 
                                                 vSendREF, 
                                                 STRING(vOpDate))).
   /* �饬 ࠭�� ����㦥��� ���㬥�� */
   lOpOnlineFnd:
   FOR EACH bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}]
                   AND bRef.RefValue    EQ SUBST("&1|&2", vSendID, vSendREF)
                 NO-LOCK,
      FIRST bPack WHERE bPack.PacketID EQ bRef.PacketID
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketID  EQ bPack.PacketID
                       AND bPackObj.file-name EQ 'op-entry'
                     NO-LOCK,
      FIRST bOp WHERE bOp.op        EQ INT64(ENTRY(1, bPackObj.Surrogate))
                  AND bOp.op-date   EQ vOpDate
                  AND bOp.op-status GE "�"
                NO-LOCK:

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CPlatReeImp", SUBST("FIND op: &1; ref: &2",
                                           STRING(bOp.op),
                                           bRef.RefValue)).
      &ENDIF

      /* ���뢠�� ���㬥�� � ��訬 ᥠ�ᮬ ������ */
      {pack-crt.i
         &Packet     = bPackInt
         &PacketID   = vPackId
         &SeanceID   = iExch::SeanceID
         &AbonentID  = -1
         &MailUserID = iExch::mail-user-num
         &State      = "{&STATE-CNF}"
         &Kind       = bCode.Misc[{&RKC-KIND}]
         &Format     = iClass
         &ClassCode  = bCode.Misc[{&RKC-CLASS}]
         &ParentID   = iExch::PacketID
      }

      RUN PacketCreateLink IN h_pack(vPackId,
                                     "op-entry",
                                     bPackObj.Surrogate,
                                     "ICyberRee").
      RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("������ � ��⥬�, op.op = &1", 
                                                    STRING(bOp.op))).
      LEAVE lOpOnlineFnd.
   END. /* FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}] */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", SUBST("FIND op: &1", STRING(vPackId GT 0))).
   &ENDIF

   IF vPackId LE 0 THEN
   DO:
      /* ���㬥�� �� ��諨, �㦭� ᮧ���� */
      RUN Fill-SysMes IN h_tmess("", "", "0", "�� ������ � ��⥬�").

      vExchOp = GetTransObject("EXCHCyber").
      IF NOT VALID-HANDLE(vExchOp) THEN
      DO:
         RUN Fill-SysMes IN h_tmess("", "", "-1", "�訡�� ᮧ����� ���㬥�� ���᫥���.").
         LEAVE MAIN.
      END.
      vExchOp = vExchOp:DEFAULT-BUFFER-HANDLE.

      /*=== ��।��塞 ����� ���ࠧ������� �����⥫� ===*/
      RUN CorrBranch IN THIS-PROCEDURE(iExch::bank-code-rec,
                                       "�������",
                                       gend-date,
                                       OUTPUT vFilialId,
                                       OUTPUT vCorr,
                                       OUTPUT vAcctCr).

      IF {assigned vFilialId} THEN
      DO:
         /*=== �饬 ��� �����⥫� � ���� ===*/
         {find-act.i &filial = vFilialId
                     &acct   = iExch::number
                     &curr   = "''"
                     &bact   = bAcct
                     &NoFindInNatCurr = YES}
      END. /* IF {assigned vFilialId} THEN */

      IF NOT AVAIL(bAcct) THEN
      DO:
         vTmpStr = SUBST("��� '&1': �� 㤠���� ��⠭����� �����⥫� ���⥦�.", iExch::number).
         RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
         LEAVE MAIN.

      END. /* IF NOT AVAIL(bAcct) THEN */

      /* ������ �墠�뢠�騩 ����� */
      vExchOp::PacketId = iExch::PacketID.
      RUN InstanceCreateEx IN h_exch(?, vExchOp, 0).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CPlatReeImp", "ASSIGN VALUES new_op").
      &ENDIF

      /*=== ������塞 �������騥 ���� � ���⥦� ===*/
      ASSIGN
         /* ���� �����⥫�      */
         vExchOp::bank-code-rec      = iExch::bank-code-rec
         vExchOp::bank-corr-acct-rec = vCorr                 WHEN {assigned vAcctCr}

         /* �࠭���� ���      */
         vExchOp::acct-cr            = vAcctCr               WHEN {assigned vAcctCr}

         /* ����� �����⥫�    */
         vExchOp::acct-rec           = bAcct.acct
         vExchOp::acct-rec           = bAcct.number          WHEN {assigned vAcctCr}

         /* �㬬� � ���᫥���   */
         vExchOp::amt-rub            = iExch::amt-rub

         /* ��ࠢ�⥫� ���⥦�  */


         /* ���७� ���㬥��   */
         vExchOp::SendREF            = iExch::receipt

         /* ���ଠ�� �� ������ */
         vExchOp::SeanceID           = iExch::SeanceID
         vExchOp::mail-user-num      = iExch::mail-user-num

         /* ����ঠ��� ����樨  */
         vExchOp::details            = ParsDetails(vExchOp, vExchOp::details)

         /* �㬥��� ���㬥�⮢ */
         vExchOp::doc-num            = SetCounterValue(vExchOp::CounterName, ?, gend-date)
      NO-ERROR. {&NO-ERROR}

      /*=== �஢�ઠ �ࠢ��쭮�� ��᢮���� ���稪� ===*/
      IF NOT {assigned vExchOp::doc-num} THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","","-1","�訡�� ����祭�� ���祭�� ��� ���稪� ���㬥�⮢").
         LEAVE MAIN.

      END. /* IF NOT {assigned iExch::doc-num} THEN */

      /*=== ������⨧��� ���㬥�� ===*/
      vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
      IF    vExchOp::op-kind EQ vOpKind   /* ��������� � ⥪�饩                     */
         OR ERROR-STATUS:ERROR          /* �������� �訡�� � �㭪樨 ������⨧�樨 */
         OR NOT {assigned vOpKind}      /* ��� �࠭���樨 ����                     */
      THEN
      DO:
    
         vTmpStr = "�� ��।����� �࠭����� ᮧ����� ��� ���⥦� � ����஬ '&1'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vExchOp::SendREF)).
         LEAVE MAIN.
      END. /* IF    iExch::op-kind EQ vOpKind */
      ELSE
         vExchOp::op-kind = vOpKind.


      /*=== ����᪠�� �࠭����� ���᫥��� ===*/
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("USIBReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
      &ENDIF
      &IF DEFINED(IS-DEBUG) &THEN
      RUN DumpObject IN h_exch(vExchOp).
      &ENDIF

      RUN RunTransaction IN h_pbase (vExchOp::op-kind) NO-ERROR. 

      &IF DEFINED(IS-DEBUG) &THEN
      RUN DumpObject IN h_exch(vExchOp).
      &ENDIF

      /*=== �஢�ઠ ᮧ����� ���㬥�� ===*/
      IF vExchOp::op EQ "0" THEN
      DO:
         vTmpStr = "�訡�� ᮧ����� ���㬥�� � ����஬ '&1'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vExchOp::SendREF)).
         LEAVE MAIN.
      END. /* IF iExch::op EQ "0" THEN */

      RUN Fill-SysMes IN h_tmess("", "", "0", "������.").

   END. /* IF vPackId LE 0 THEN */

   RUN Fill-SysMes IN h_tmess("", "", "0", "").
   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

IF vFlagSet NE YES THEN
DO:
   RUN Fill-SysMes IN h_tmess("","","-1", "").
   RUN Fill-SysMes IN h_tmess("","","-1", "������: �⬥�� ������.").
   RUN Fill-SysMes IN h_tmess("","","-1", "").
END. /* IF vFlagSet NE YES THEN */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatReeImp */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ ���᫥���� �㬬 �� ॥���� =======================================================*/
{pfuncdef 
   &DEFPROC="CPlatReeCalc"
   &DESCRIPTION="������ ���᫥���� �㬬 �� ॥����"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE CPlatReeCalc:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vCount    AS  INT64 NO-UNDO.         /* ������⢮ ᮧ������ ���㬥�⮢  */
DEF VAR vSum      AS  DEC   NO-UNDO.         /* ���� �㬬� ᮧ������ ���㬥�⮢ */
DEF VAR vIntFmt   AS  CHAR  NO-UNDO.         /* ��஢����� �� ������ �ଠ�     */
DEF VAR vSumFmt   AS  CHAR  NO-UNDO.         /* ��஢����� �� ������ �ଠ�     */

DEF BUFFER bPacket   FOR Packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOpEntry  FOR op-entry.
DEF BUFFER bCode     FOR code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeCalc", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   /*=== ��⠥� �㬬� ᮧ������ ���㬥�⮢ ===*/
   FOR EACH bPacket WHERE bPacket.ParentID   EQ INT64(iExch::PacketID)
                    NO-LOCK,
      EACH bPackObj WHERE bPackObj.PacketID  EQ bPacket.PacketID
                      AND bPackObj.file-name EQ 'op-entry'
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op        EQ INT64(ENTRY(1, bPackObj.Surrogate))
                       AND bOpEntry.op-entry  GE INT64(ENTRY(2, bPackObj.Surrogate))
                       AND bOpEntry.op-status GE "�"
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END. /* FOR EACH bPacket WHERE bPacket.ParentID EQ INT64(iExch::PacketID) */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeCalc", SUBST("Reestr: &3, &4",
                                         STRING(vCount),
                                         STRING(vSum))).
   &ENDIF

   ASSIGN
      vIntFmt = IntFmt(vCount, 0)
      vSumFmt = DecFmt(vSum,   0)
   .

   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� �� ॥���� ���⥦��: &1 �� �㬬� &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0","").

   /* ���࠭塞 �㬬� �� ॥���� */
   vFlagSet = UpdateSigns(bCode.Misc[{&RKC-CLASS}], iExch::PacketID, "SummAmt", STRING(vSum), ?).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeCalc", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatReeCalc */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ ��ப ॥��� �ࠫᨡ� =============================================================*/
{pfuncdef 
   &DEFPROC="USIBReeImpDT"
   &DESCRIPTION="������ ��ப ॥��� �ࠫᨡ�"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE USIBReeImpDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vBicMask  AS  CHAR  NO-UNDO.  /* ��᪠ �����⨬�� ���                                    */
DEF VAR vAcctMask AS  CHAR  NO-UNDO.  /* ��᪠ ����㯭�� ��⮢                                  */
DEF VAR vFilialId AS  CHAR  NO-UNDO.  /* ��� ���ࠧ������� �����⥫� ���⥦�                    */
DEF VAR vCorr     AS  CHAR  NO-UNDO.  /* ����.��� ����� �����⥫� ���⥦�                      */
DEF VAR vAcctCr   AS  CHAR  NO-UNDO.  /* �࠭���� ��� ���䨫������ ����⮢                 */
DEF VAR vOpKind   AS  CHAR  NO-UNDO.  /* ��� �࠭���樨 ��� ���᫥���                           */
DEF VAR vTmpStr   AS  CHAR  NO-UNDO.

DEF BUFFER bAcct     FOR acct.
DEF BUFFER bCode     FOR Code.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iExch::mail-format, BUFFER bCode) NE YES THEN
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", "ASSIGN VALUES").
   &ENDIF

   /*=== ���樠������ ��६����� ===*/
   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      iExch::SendREF       = TRIM(iExch::SendREF)
      iExch::OrderDate     = TRIM(iExch::OrderDate)
      iExch::Amt           = TRIM(iExch::Amt)
      iExch::AmtCom        = TRIM(iExch::AmtCom)
      iExch::OrderAcct     = TRIM(iExch::OrderAcct)
      iExch::bank-code-rec = TRIM(iExch::bank-code-rec)
      iExch::name-rec      = TRIM(iExch::name-rec) 
      iExch::name-send     = TRIM(iExch::name-send)
      iExch::name-rec      = DelDoubleChars(iExch::name-rec,  " ")
      iExch::name-send     = DelDoubleChars(iExch::name-send, " ")
      iExch::tel-send      = TRIM(iExch::tel-send)
      iExch::name-send     = TRIM(iExch::name-send)

      /* ��᪠ �����⨬�� ��⮢ ��� ����������  */
      vAcctMask            = TRNSettingValue("","AcctMask","!*")

      /* ��᪠ �����⨬�� ��� ������ �����⥫�� */
      vBicMask             = TRNSettingValue("","BICMask","!*")
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                         iExch::SendRef, 
                                         iExch::SendId,
                                         bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== �஢�ઠ ����୮� ����㧪� ���⥦� ===*/
   IF NOT {assigned iExch::SendREF} THEN 
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1", "� ॥��� ���� ���⥦� ��� 㭨���쭮�� �����.").
      LEAVE MAIN.
   END. /* IF NOT {assigned iExch::SendREF} THEN  */

   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendREF),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("���㬥�� 㦥 ����㦥�, 㭨����� �����: '&1'", 
                                       iExch::SendREF)).
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("USIBReeImpDT", SUBST("���⥦ 㦥 ����㦥�. &1",
                                            vTmpStr)).
      &ENDIF
      LEAVE MAIN.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("BIC = &1; BicMask = &2", 
                                         iExch::bank-code-rec, 
                                         vBicMask)).
   RUN dbgprint.p ("USIBReeImpDT", SUBST("Acct = &1; AcctMask = &2", 
                                         iExch::OrderAcct, 
                                         vAcctMask)).
   &ENDIF

   /*=== �஢�ઠ ��� ����� �����⥫� ===*/
   IF NOT (CAN-DO(vBicMask, iExch::bank-code-rec) AND {assigned iExch::bank-code-rec}) THEN
   DO:
      vTmpStr = SUBST("��� &1 ��� ��� '&2' �� ᮮ⢥����� ��᪥ ��� ������ �����⥫��.", 
                      iExch::bank-code-rec,
                      iExch::OrderAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END.

   /*=== �஢�ઠ ��� �����⥫� ===*/
   IF NOT (CAN-DO(vAcctMask, iExch::OrderAcct) AND {assigned iExch::OrderAcct}) THEN
   DO:
      vTmpStr = SUBST("��� '&1' �� ᮮ⢥����� ��᪥ �����⨬�� ��⮢.", iExch::OrderAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END.

   /*=== ��।��塞 ����� ���ࠧ������� �����⥫� ===*/
   RUN CorrBranch IN THIS-PROCEDURE(iExch::bank-code-rec,
                                    "�ᨡ��",
                                    gend-date,
                                    OUTPUT vFilialId,
                                    OUTPUT vCorr,
                                    OUTPUT vAcctCr).

   IF {assigned vFilialId} THEN
   DO:
      /*=== �饬 ��� �����⥫� � ���� ===*/
      {find-act.i &filial = vFilialId
                  &acct   = iExch::OrderAcct
                  &curr   = "''"
                  &bact   = bAcct
                  &NoFindInNatCurr = YES}
   END. /* IF {assigned vFilialId} THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   vTmpStr = IF AVAIL(bAcct) THEN bAcct.acct
                             ELSE "not found".
   RUN dbgprint.p ("USIBReeImpDT", SUBST("acct = &1", vTmpStr)).
   &ENDIF                        

   IF AVAIL(bAcct) THEN
   DO:
      /*=== ������塞 �������騥 ���� � ���⥦� ===*/
      ASSIGN
         /* ���� �����⥫�      */
         iExch::bank-corr-acct-rec = vCorr          WHEN {assigned vAcctCr}

         /* �࠭���� ���      */
         iExch::acct-cr            = vAcctCr        WHEN {assigned vAcctCr}

         /* ����� �����⥫�    */
         iExch::acct-rec           = bAcct.acct
         iExch::acct-rec           = bAcct.number   WHEN {assigned vAcctCr}

         /* �㬬� � ���᫥���   */
         iExch::amt-rub            = iExch::Amt
     
         /* ��ࠢ�⥫� ���⥦�  */

         /* ����ঠ��� ����樨  */
         iExch::details            = ParsDetails(iExch, iExch::details)

         /* �㬥��� ���㬥�⮢ */
         iExch::doc-num            = SetCounterValue(TRIM(iExch::CounterName), ?, gend-date)
      .

      /*=== �஢�ઠ �ࠢ��쭮�� ��᢮���� ���稪� ===*/
      IF NOT {assigned iExch::doc-num} THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","","-1","�訡�� ����祭�� ���祭�� ��� ���稪� ���㬥�⮢").
         LEAVE MAIN.

      END. /* IF NOT {assigned iExch::doc-num} THEN */
   END. /* IF AVAIL(bAcct) THEN */
   ELSE 
   DO:
      vTmpStr = SUBST("��� '&1': �� 㤠���� ��⠭����� �����⥫� ���⥦�.", iExch::OrderAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.

   END. /* IF AVAIL(bAcct) THEN ... ELSE */


   /*=== ������⨧��� ���㬥�� ===*/
   vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
   IF    iExch::op-kind EQ vOpKind   /* ��������� � ⥪�饩                     */
      OR ERROR-STATUS:ERROR          /* �������� �訡�� � �㭪樨 ������⨧�樨 */
      OR NOT {assigned vOpKind}      /* ��� �࠭���樨 ����                     */
   THEN
   DO:

      vTmpStr = "�� ��।����� �࠭����� ᮧ����� ��� ���⥦� � ����஬ '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF    iExch::op-kind EQ vOpKind */
   ELSE
      iExch::op-kind = vOpKind.

   /*=== ����᪠�� �࠭����� ���᫥��� ===*/
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
   &ENDIF

   RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

   &IF DEFINED(IS-DEBUG) &THEN
   RUN DumpObject IN h_exch(iExch).
   &ENDIF

   /*=== �஢�ઠ ᮧ����� ���㬥�� ===*/
   IF iExch::op EQ "0" THEN
   DO:
      vTmpStr = "�訡�� ᮧ����� ���㬥�� � ����஬ '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF iExch::op EQ "0" THEN */

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* USIBReeImpDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ����஫� �⮣���� ��ப� � ���᫥��� �� ॥���� �ࠫᨡ� =================================*/
{pfuncdef 
   &DEFPROC="USIBReeImpFF"
   &DESCRIPTION="����஫� �⮣���� ��ப� � ���᫥��� �� ॥���� �ࠫᨡ�"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE USIBReeImpFF:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vTotCount AS  INT64 NO-UNDO.         /* ������⢮ ���⥦�� � ॥���    */
DEF VAR vCount    AS  INT64 NO-UNDO.         /* ������⢮ ᮧ������ ���㬥�⮢  */
DEF VAR vTotSum   AS  DEC   NO-UNDO.         /* ���� �㬬� ����᫥���         */
DEF VAR vSum      AS  DEC   NO-UNDO.         /* ���� �㬬� ᮧ������ ���㬥�⮢ */
DEF VAR vIntFmt   AS  CHAR  NO-UNDO.         /* ��஢����� �� ������ �ଠ�     */
DEF VAR vSumFmt   AS  CHAR  NO-UNDO.         /* ��஢����� �� ������ �ଠ�     */

DEF BUFFER bPack     FOR Packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOpEntry  FOR op-entry.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpFF", "RUN").
   &ENDIF

   ASSIGN
      iExch::TotCount = TRIM(SUBSTR(iExch::TotCount ,2))
      iExch::TotAmt   = TRIM(iExch::TotAmt)
      vTotCount       = DEC(iExch::TotCount)
      vTotSum         = DEC(iExch::TotAmt)
   NO-ERROR. {&NO-ERROR}

   /*=== ��⠥� �㬬� � ������⢮ ᮧ������ ���㬥�⮢ ===*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketId EQ bPack.PacketId
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op       EQ INT64(ENTRY(1, bPackObj.surrogate))
                       AND bOpEntry.op-ENTRY EQ INT64(ENTRY(2, bPackObj.surrogate))
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END.

   ASSIGN
      vIntFmt = IntFmt(vCount, vTotCount)
      vSumFmt = DecFmt(vSum,   vTotSum)
   .
   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� �� ॥���� ���⥦��: &1 �� �㬬� &2",
                                               STRING(vTotCount, vIntFmt),
                                               STRING(vTotSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� ᮧ���� ���㬥�⮢:  &1 �� �㬬� &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", SUBST("Reestr: &1, &2; Create: &3, &4",
                                          STRING(vTotCount),
                                          STRING(vTotSum),
                                          STRING(vCount),
                                          STRING(vSum))).
   &ENDIF

   IF (vSum EQ vTotSum) AND (vCount EQ vTotCount) THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","0","���宦����� � �⮣���� ��ப�� ॥��� ���.").
      vFlagSet = YES.
   END.
   ELSE /* IF (vSum EQ vTotSum) AND (vCount NE vTotCount) THEN ... ELSE */
      RUN Fill-SysMes IN h_tmess("","", "-1","������: ���宦����� � �⮣���� ��ப�� ॥���.").

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpFF", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

RUN Fill-SysMes IN h_tmess("","","0","").

{doreturn.i vFlagSet}

END PROCEDURE. /* USIBReeImpFF */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���樠������ ��ࢮ� ��ப� ॥��� �ࠫᨡ� ==============================================*/
{pfuncdef 
   &DEFPROC="USIBReeIniFH"
   &DESCRIPTION="���樠������ ��ࢮ� ��ப� ॥��� �ࠫᨡ�"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE USIBReeIniFH:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeIniFH", "RUN").
   &ENDIF

   /* ������ �᭮��� �� crdiucs, ��� ��������� */
   /* ��ࢠ� ��ப� ᮤ���� ����� ���⥦�, ���⮬� ��஦���� ���� ��ꥪ� */
   RUN InstanceCreateEx IN h_exch (?, iExch, 0).
   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeIniFH", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* USIBReeImpFF */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ ��ࢮ� ��ப� ॥��� ������ =======================================================*/
{pfuncdef 
   &DEFPROC="RapidReeImpFH"
   &DESCRIPTION="������ ��ࢮ� ��ப� ॥��� ������"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE RapidReeImpFH:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.
DEF VAR vTmpStr   AS  CHAR NO-UNDO.

DEF BUFFER bCode     FOR Code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", "ASSIGN VALUES").
   &ENDIF

   /*=== ���樠������ ��६����� ===*/
   ASSIGN
      iExch::CliSys      = TRIM(iExch::CliSys)
      iExch::ReeRef      = TRIM(iExch::ReeRef)
      iExch::DTReeBeg    = TRIM(iExch::DTReeBeg)
      iExch::DTReeEnd    = TRIM(iExch::DTReeEnd)
      iExch::TotCount    = TRIM(iExch::TotCount)
      iExch::TotAmt      = TRIM(iExch::TotAmt)
      iExch::TotAmtWOCom = TRIM(iExch::TotAmtWOCom)
      iExch::ReeDate     = GetEntries(1, iExch::DTReeBeg, " ", "")
      iExch::ReeTime     = GetEntries(2, iExch::DTReeBeg, " ", "")
      iExch::ReeDate     = SUBST("&1.&2.&3", 
                                 ENTRY(3, iExch::ReeDate,"-"),
                                 ENTRY(2, iExch::ReeDate,"-"),
                                 ENTRY(1, iExch::ReeDate,"-"))
      iExch::SendRef     = iExch::ReeRef
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== �饬 ����㦥��� ���㬥�� �१ ��७� ��� ��� ���� ����㧪� ===*/
   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendRef),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("���� 㦥 ����㦥�, 㭨����� ����� ���뫪�: '&1'", 
                                       iExch::SendRef)).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpFH", SUBST("���� 㦥 ����㦥�. &1",
                                             vTmpStr)).
      &ENDIF
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).
      UNDO MAIN, LEAVE MAIN.
   END.

   /*=== ���࠭塞 ��뫪� �� 䠩� ������ ===*/
   RUN PacketCreateRef IN h_rfrnc (gend-date, 
                                   INT64(TRIM(iExch::PacketId)), 
                                   bCode.Misc[{&RKC-REPLY}],
                                   SUBST("&1|&2",TRIM(iExch::SendId),TRIM(iExch::SendRef))).

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeImpFH */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ ��ப � ���⥦��� �� ॥��� ������ ================================================*/
{pfuncdef 
   &DEFPROC="RapidReeImpDT"
   &DESCRIPTION="������ ��ப � ���⥦��� �� ॥��� ������"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE RapidReeImpDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vNonCrit  AS  CHAR  NO-UNDO. /* ��᪠ �����⨬�� �訡�� ���⥦��                     */
DEF VAR vBicMask  AS  CHAR  NO-UNDO. /* ��᪠ �����⨬�� ���                                 */
DEF VAR vAcctMask AS  CHAR  NO-UNDO. /* ��᪠ ����㯭�� ��⮢                               */
DEF VAR vFilialId AS  CHAR  NO-UNDO. /* ��� ���ࠧ������� �����⥫� ���⥦�                 */
DEF VAR vCorr     AS  CHAR  NO-UNDO. /* ����.��� ����� �����⥫� ���⥦�                   */
DEF VAR vAcctCr   AS  CHAR  NO-UNDO. /* �࠭���� ��� ���䨫������ ����⮢              */
DEF VAR vPackId   AS  INT64 NO-UNDO. /* ����� �墠�뢠�饣� �����                           */
DEF VAR vOpKind   AS  CHAR  NO-UNDO. /* ��� �࠭���樨 ��� ���᫥���                        */
DEF VAR vTmpStr   AS  CHAR  NO-UNDO.

DEF BUFFER bCode     FOR Code.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER bPackInt  FOR packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "ASSIGN VALUES").
   &ENDIF

   /*=== ���樠������ ��६����� ===*/
   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      vPackId              = 0
      iExch::DTPay         = TRIM(iExch::DTPay)
      iExch::SendRef       = TRIM(iExch::SendRef)
      iExch::amt-rub       = TRIM(iExch::amt-rub)
      iExch::OrderAcct     = TRIM(iExch::OrderAcct)
      iExch::bank-code-rec = TRIM(iExch::bank-code-rec)
      iExch::name-rec      = TRIM(iExch::name-rec, " ")
      iExch::name-send     = TRIM(iExch::name-send, " ")
      iExch::name-rec      = DelDoubleChars(iExch::name-rec, " ")
      iExch::name-send     = DelDoubleChars(iExch::name-send, " ")
      iExch::addr-send     = TRIM(iExch::addr-send)

      /* ��᪠ �����⨬�� ��⮢ ��� ����������  */
      vBicMask             = TRNSettingValue("","BICMask","!*")

      /* ��᪠ �����⨬�� ��� ������ �����⥫�� */
      vAcctMask            = TRNSettingValue("","AcctMask","!*")

      /* ��᪠ �����⨬�� �訡�� ���⥦��                     */
      vNonCrit             = TRNSettingValue("","NonError","!*")
      vNonCrit             = SUBST("&1,", vNonCrit)
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF                        

   /*=== �஢�ઠ ����୮� ����㧪� ���⥦� ===*/
   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendRef),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("���⥦ 㦥 ����㦥�, 㭨����� �����: '&1'", 
                                       iExch::SendRef)).
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("RapidReeImpDT", SUBST("���⥦ 㦥 ����㦥�. &1",
                                             vTmpStr)).
      &ENDIF
      LEAVE MAIN.
   END. /* IF FndRef(bCode.Misc[{&RKC-REPLY}],  */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("BIC = &1; BicMask = &2", 
                                          iExch::bank-code-rec, 
                                          vBicMask)).
   RUN dbgprint.p ("RapidReeImpDT", SUBST("Acct = &1; AcctMask = &2", 
                                          iExch::OrderAcct,
                                          vAcctMask)).
   &ENDIF

   /* ���� ��।������ ��ࠬ��஢ ���⥦�. � ������ ���� ������� ॥��� - ���⮬� �� ���뢠�� */
   /* ��������� ������.                                                                           */
   lOpCreate:
   DO:
      /*=== �஢�ઠ ��� ����� �����⥫� ===*/
      IF NOT (CAN-DO(vBicMask, iExch::bank-code-rec) AND {assigned iExch::bank-code-rec}) THEN
      DO:
         /* �� �����⨬� ��� ����� �����⥫� */
         vTmpStr = SUBST("��� &1 ��� ��� '&2': �� ᮮ⢥����� ��᪥ ��� ������ �����⥫��.",
                         iExch::bank-code-rec,
                         iExch::OrderAcct).
         RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
         LEAVE lOpCreate.
      END. /* IF NOT (CAN-DO(vBicMask, vBic) AND {assigned vBic}) THEN */

      /*=== �஢�ઠ ��� �����⥫� ===*/
      IF NOT (CAN-DO(vAcctMask, iExch::OrderAcct) AND {assigned iExch::OrderAcct}) THEN
      DO:
         /* �� �����⨬� ��� �����⥫� */
         vTmpStr = SUBST("��� '&1' � ���⥦� c ����஬ '&2': �� ᮮ⢥����� ��᪥ �����⥫��.", 
                         iExch::OrderAcct,
                         iExch::SendRef).
         RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
         LEAVE lOpCreate.
      END.

      /*=== ��।��塞 ����� ���ࠧ������� �����⥫� ===*/
      RUN CorrBranch IN THIS-PROCEDURE(iExch::bank-code-rec,
                                       "��������",
                                       gend-date,
                                       OUTPUT vFilialId,
                                       OUTPUT vCorr,
                                       OUTPUT vAcctCr).
      
      IF {assigned vFilialId} THEN
      DO:
         /*=== �饬 ��� �����⥫� � ���� ===*/
         {find-act.i &filial = vFilialId
                     &acct   = iExch::OrderAcct
                     &curr   = "''"
                     &bact   = bAcct
                     &NoFindInNatCurr = YES}
      END. /* IF {assigned vFilialId} THEN */

      &IF DEFINED(IS-DEBUG) &THEN
      vTmpStr = IF AVAIL(bAcct) THEN bAcct.acct
                                ELSE "not found".
      RUN dbgprint.p ("RapidReeImpDT", SUBST("acct = &1", vTmpStr)).
      &ENDIF                        

      IF AVAIL(bAcct) THEN
      DO:
         /*=== ������塞 �������騥 ���� � ���⥦� ===*/
         ASSIGN
            /* ���� �����⥫�      */
            iExch::bank-corr-acct-rec = vCorr

            /* �࠭���� ���      */
            iExch::acct-cr            = vAcctCr               WHEN {assigned vAcctCr}

            /* ����� �����⥫�    */
            iExch::FilialId           = bAcct.filial-id
            iExch::acct-rec           = bAcct.acct
            iExch::acct-rec           = bAcct.number          WHEN {assigned vAcctCr}

            /* �㬬� � ���᫥���   */

            /* ��ࠢ�⥫� ���⥦�  */

            /* ����ঠ��� ����樨  */
            iExch::details            = ParsDetails(iExch, iExch::details)

            /* �㬥��� ���㬥�⮢ */
            iExch::doc-num            = SetCounterValue(iExch::CounterName, ?, gend-date)
         NO-ERROR. {&NO-ERROR}

         /*=== �஢�ઠ �ࠢ��쭮�� ��᢮���� ���稪� ===*/
         IF NOT {assigned iExch::doc-num} THEN
         DO:
            RUN Fill-SysMes IN h_tmess("","","-1","�訡�� ����祭�� ���祭�� ��� ���稪� ���㬥�⮢").
            LEAVE MAIN.
         
         END. /* IF NOT {assigned iExch::doc-num} THEN */

         /*=== ������⨧��� ���㬥�� ===*/
         vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
         IF    iExch::op-kind EQ vOpKind   /* ��������� � ⥪�饩                     */
            OR ERROR-STATUS:ERROR          /* �������� �訡�� � �㭪樨 ������⨧�樨 */
            OR NOT {assigned vOpKind}      /* ��� �࠭���樨 ����                     */
         THEN
         DO:
       
            vTmpStr = "�� ��।����� �࠭����� ᮧ����� ��� ���⥦� � ����஬ '&1'".
            RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
            LEAVE MAIN.
         END. /* IF    iExch::op-kind EQ vOpKind */
         ELSE
            iExch::op-kind = vOpKind.

      END. /* IF AVAIL(bAcct) THEN */
   END. /* lOpCreate */

   /*=== �஢�ઠ �⠭������ �訡�� ������ ===*/
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "RUN RapidaChkErr").
   &ENDIF                        

   RUN RapidaChkErr IN THIS-PROCEDURE(iExch).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("RapidaErr: &1", iExch::RapidaErr)).
   &ENDIF                        

   IF {assigned iExch::RapidaErr} THEN
   DO:
      /* �� ������ ॥��� ���� �訡�� */
      IF NOT {assigned iExch::SendRef} THEN
         vTmpStr = SUBST("�� '&1' �� �㬬� &2", iExch::DTPay, iExch::amt-rub).
      ELSE
         vTmpStr = SUBST("����� '&1'", iExch::SendRef).

      vTmpStr = SUBST("���⥦ &1 �訡��: &2",
                      vTmpStr, 
                      GetCodeNameEx("�衪������", iExch::RapidaErr, iExch::RapidaErr)).
      RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

   END. /* IF {assigned iExch::RapidaErr} THEN */

   IF CAN-DO(vNonCrit, iExch::RapidaErr) THEN
   DO:
      /*=== ����᪠�� �࠭����� ���᫥��� ===*/
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("RapidReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
      &ENDIF

      /* ����᪠�� �࠭����� ���᫥��� */
      RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

      &IF DEFINED(IS-DEBUG) &THEN
      RUN DumpObject IN h_exch(iExch).
      &ENDIF

      IF iExch::op EQ "0" THEN
      DO:
         vTmpStr = "�訡�� ᮧ����� ���㬥�� � ����஬ '&1'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendRef)).
         UNDO MAIN, LEAVE MAIN.
      END.

      /* �饬 �易��� � �������㥬� 䠩��� ����� */
      FOR FIRST bPackObj WHERE bPackObj.SeanceId  EQ     iExch::SeanceId
                           AND bPackObj.file-name EQ     "op-entry"
                           AND bPackObj.surrogate BEGINS SUBST("&1,", iExch::op)
                         NO-LOCK:
         vPackId = bPackObj.PacketId.
      END. /* FOR FIRST bPackObj WHERE bPackObj.SeanceId  EQ     iExch::SeanceId */

   END. /* IF CAN-DO(NonCrit, iExch::RapidaErr) THEN */
   ELSE
   DO:
      /* ��ப� �� �������� ���᫥���   */
      /* ������� ����� ��� ��ப� ������ */
      {pack-crt.i
         &Packet     = bPackInt
         &PacketID   = vPackId
         &SeanceID   = iExch::SeanceID
         &AbonentID  = -1
         &MailUserID = iExch::mail-user-num
         &State      = "{&STATE-IMP}"
         &Kind       = bCode.Misc[{&RKC-KIND}]
         &Format     = iClass
         &ClassCode  = bCode.Misc[{&RKC-CLASS}]
         &ParentID   = iExch::PacketID
      }

   END. /* IF CAN-DO(NonCrit, iExch::RapidaErr) THEN ... ELSE */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("PacketID = '&1'", STRING(vPackId))).
   &ENDIF

   IF vPackId NE 0 THEN
   DO:
      /* ���࠭塞 ����� �� ��ப� */
      RUN SaveAttrRapida IN THIS-PROCEDURE (iExch, bCode.Misc[{&RKC-CLASS}], vPackId).
   END.
   ELSE
   DO:
      vTmpStr = "�訡�� ��࠭���� ४����⮢ ���㬥�� � ����஬ '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendRef)).
      UNDO MAIN, LEAVE MAIN.
   END.

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeImpDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ 墮�⮢��� ॥��� ������ ==========================================================*/
{pfuncdef 
   &DEFPROC="RapidReeImpFF"
   &DESCRIPTION="������ 墮�⮢��� ॥��� ������"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE RapidReeImpFF:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG   NO-UNDO INIT ?.
DEF VAR vTotCount  AS  INT64 NO-UNDO. /* ������⢮ ���⥦�� � ॥���    */
DEF VAR vCount     AS  INT64 NO-UNDO. /* ������⢮ ᮧ������ ���㬥�⮢  */
DEF VAR vTotSum    AS  DEC   NO-UNDO. /* ���� �㬬� ����᫥���         */
DEF VAR vSum       AS  DEC   NO-UNDO. /* ���� �㬬� ᮧ������ ���㬥�⮢ */
DEF VAR vIntFmt    AS  CHAR  NO-UNDO. /* ��஢����� �� ������ �ଠ�     */
DEF VAR vSumFmt    AS  CHAR  NO-UNDO. /* ��஢����� �� ������ �ଠ�     */
DEF VAR vReestrErr AS  CHAR  NO-UNDO. /* �訡�� ॥���                   */
DEF VAR vPayErr    AS  CHAR  NO-UNDO. /* �訡�� ���⥦�                   */
DEF VAR vNonCrit   AS  CHAR  NO-UNDO. /* ���᮪ �� ����᪨� �訡��     */
DEF VAR vTmpStr    AS  CHAR  NO-UNDO.
DEF VAR vTmpAmt    AS  DEC   NO-UNDO.

DEF BUFFER bPack     FOR packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bFileExch FOR FileExch.
DEF BUFFER bOpEntry  FOR op-entry.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", "RUN").
   &ENDIF

   ASSIGN
      vTotCount = INT64(GetDecVal(iExch::TotCount, 0))
      vTotSum   = GetDecVal(iExch::TotAmt, 0)

      /* ��᪠ �����⨬�� �訡�� ���⥦�� */
      vNonCrit  = SUBST("&1,", TRNSettingValue("","NonError","!*"))
   .

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", "Counting values").
   &ENDIF

   /*=== ��⠥� �㬬� � ������⢮  �� �ᥬ ��ப�� ॥��� ==============================================*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                 NO-LOCK:

      ASSIGN
         vPayErr    = GetXAttrValueEx("Packet", STRING(bPack.PacketId), "RapidaErr", "")
         vTmpStr    = GetXAttrValueEx("Packet", STRING(bPack.PacketId), "amt-rub", "0")
         vCount     = vCount + 1
         vTmpAmt    = GetDecVal(vTmpStr, 0)
         vSum       = vSum + vTmpAmt
      .

      /* 0200 - ����᪠� (������ �ᥣ� ॥���), ��⠫�� �訡�� ���⥦�� ���ଠ樮��� */
      IF CAN-DO("0200", vPayErr) THEN
         vReestrErr = vPayErr.

   END. /* FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId) */

   /*=== ����塞 ������⢮ ��ࠡ�⠭���� � �⮣���� ��ப�� ॥��� ===*/
   ASSIGN
      /*=== 0300 ������⢮ ��ப, � ���������, �� ᮮ⢥����� �������� ��ப ������ =======*/
      vReestrErr = "0300"                 WHEN vCount NE vTotCount
      /*=== 0400 �㬬�, 㪠������ � ��ࢮ� ��ப�, �� ᮮ⢥����� �㬬� ��� ��ப ������ ====*/
      vReestrErr = "0400"                 WHEN vSum   NE vTotSum
   .

   /*=== ���࠭塞 �⮣��� ����� ===*/
   FOR FIRST bPack WHERE bPack.PacketId EQ INT64(iExch::PacketId)
                   NO-LOCK:
      /* ���࠭�� ����� �� ॥���� */
      UpdateSigns(bPack.Class-code, iExch::PacketId, "CliSys",   iExch::CliSys,   ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "ReeRef",   iExch::ReeRef,   ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "TotCount", iExch::TotCount, ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "TotAmt",   iExch::TotAmt,   ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "ReeError", vReestrErr,      ?).

   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", SUBST("Reestr: &1, &2; Packet: &3, &4",
                                          STRING(vTotCount),
                                          STRING(vTotSum),
                                          STRING(vCount),
                                          STRING(vSum))).
   RUN dbgprint.p ("RapidReeImpFF", SUBST("RapidaErr: '&1'; NonCritical: '&2'", 
                                          vReestrErr, 
                                          vNonCrit)).
   &ENDIF

   IF {assigned vReestrErr} THEN
   DO:
      /*=== ����᪠� �訡�� �� ॥���� (������ �ᥣ� ॥���) ===*/
      vTmpStr = "".
      FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ INT64(iExch::FileExchID)
                          NO-LOCK:
         vTmpStr = SUBST(" (䠩� '&1')", bFileExch.name).

      END. /* FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ INT64(iExch::FileExchID) */

      vTmpStr = SUBST("������! ������ ����� '&2'&1: &3",
                      vTmpStr, 
                      iExch::ReeRef,
                      GetCodeNameEx("�衪������", vReestrErr, vReestrErr)).

      RUN Fill-SysMes IN h_tmess("","","0", "").
      RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

      /*=== ������ �� ᮧ����� ���㬥��� ===*/
      FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                     NO-LOCK,
         EACH bPackObj WHERE bPackObj.PacketId  EQ     bPack.PacketId
                         AND bPackObj.file-name BEGINS "op"
                       NO-LOCK:
         RUN OperationDelete IN THIS-PROCEDURE(INT64(ENTRY(1, bPackObj.surrogate))) NO-ERROR.

      END. /* FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId) */
   END. /* IF {assigned vReestrErr} THEN */

   ASSIGN
      vIntFmt = IntFmt(vCount, vTotCount)
      vSumFmt = DecFmt(vSum,   vTotSum)
   .

   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� �� ॥���� ���⥦��: &1 �� �㬬� &2",
                                               STRING(vTotCount, vIntFmt),
                                               STRING(vTotSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� ��ࠡ�⠭� ���⥦��: &1 �� �㬬� &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).
   ASSIGN
      vSum   = 0
      vCount = 0
   .

   /*=== ��⠥� �㬬� � ������⢮ ᮧ������ ���㬥�⮢ ===*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketId EQ bPack.PacketId
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op       EQ INT64(ENTRY(1, bPackObj.surrogate))
                       AND bOpEntry.op-ENTRY EQ INT64(ENTRY(2, bPackObj.surrogate))
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END.

   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� ᮧ���� ���㬥�⮢:  &1 �� �㬬� &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", SUBST("Create: &1, &2", STRING(vCount), STRING(vSum))).
   &ENDIF

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeImpFF */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== �������� �����⭮�� ॥��� ��� ������ ===================================================*/
{pfuncdef 
   &DEFPROC="RapidReeOutDT"
   &DESCRIPTION="�������� �����⭮�� ॥��� ��� ������"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE RapidReeOutDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG    NO-UNDO INIT ?.
DEF VAR vFileName  AS  CHAR   NO-UNDO. /* ��� ����㦥����� 䠩��                    */
DEF VAR vImportId  AS  INT64  NO-UNDO. /* ����� ������ ॥��� �� ������           */
DEF VAR vBankRef   AS  CHAR   NO-UNDO. /* �������� ��� ���⭨�� ���⥬� � ���    */
DEF VAR vReeRef    AS  CHAR   NO-UNDO. /* �������� ����� ॥��� ���              */
DEF VAR vCount     AS  INT64  NO-UNDO. /* ������⢮ ����ᥩ, ��襤�� � ॥���     */
DEF VAR vAmt       AS  DEC    NO-UNDO. /* ���� �㬬� ���ਭ���� ��ॢ���� � �㡫�� */
DEF VAR vReeErr    AS  CHAR   NO-UNDO. /* �訡�� ���᫥��� �ᥣ� ॥���           */
DEF VAR vPayErr    AS  CHAR   NO-UNDO. /* �訡�� ���᫥��� ���⥦�                 */
DEF VAR vNonCrit   AS  CHAR   NO-UNDO. /* ���᮪ �� ����᪨� �訡��     */
DEF VAR vPackId    AS  INT64  NO-UNDO. /* ����� ᮧ��������� �����                 */
DEF VAR vPayId     AS  CHAR   NO-UNDO. 
DEF VAR vTmpStr    AS  CHAR   NO-UNDO. 

DEF BUFFER bCode     FOR Code.
DEF BUFFER bPack     FOR packet.
DEF BUFFER bPay      FOR packet.
DEF BUFFER bErrPay   FOR ttErrPay.
DEF BUFFER bFileExch FOR FileExch.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeOutDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iExch::mail-format, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   ASSIGN
      /* ��᪠ �����⨬�� �訡�� ���⥦�� */
      vNonCrit  = SUBST("&1,", TRNSettingValue("","NonError","!*"))

      vImportId = INT64(TRNSettingValue("","ImpSeanceID","0"))

   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeOutDT", SUBST("ImpSeance: &1, NonCritical: &2", 
                                          vImportId,
                                          vNonCrit)).
   &ENDIF

   /*=== ��ॡ�ࠥ� ����㦥��� 䠩�� ॥��஢ � ��।����� ᥠ�� ===*/
   FOR EACH bPack WHERE bPack.SeanceId EQ vImportId
                    AND bPack.ParentId EQ 0
                  NO-LOCK:

      ASSIGN
         vPayId   = STRING(bPack.PacketId)
         vBankRef = GetXAttrValueEx("Packet", vPayId, "CliSys",   "")
         vReeRef  = GetXAttrValueEx("Packet", vPayId, "ReeRef",   "")
         vReeErr  = GetXAttrValueEx("Packet", vPayId, "ReeError", "")
         vCount   = 0
         vAmt     = 0
         vFileName= ""
      .
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("RapidReeOutDT", SUBST("PacketId: &1, ReeRef: &2, RapidaErr: &3", 
                                             vPayId,
                                             vReeRef,
                                             vReeErr)).
      &ENDIF

      FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ bPack.FileExchID
                          NO-LOCK:
         vFileName = SUBST("���� &1:", bFileExch.Name).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("RapidReeOutDT", SUBST("FileName: '&1'", bFileExch.Name)). 
         &ENDIF

      END. /* FOR FIRST bFileExch WHERE bFileExch EQ mFileID  */

      {empty bErrPay}

      IF LENGTH(vReeRef) NE 8 THEN
      DO:
         vTmpStr = "&1�������� ����� ॥��� '&2' �� ᮮ⢥���� �ଠ�� 'YYYYMMDD'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vFileName, vReeRef)).
         UNDO MAIN, LEAVE MAIN.
      END.

      /* ��ॡ�ࠥ� �訡��� ��ப� */
      lPayFnd:
      FOR EACH bPay WHERE bPay.SeanceId EQ bPack.SeanceId
                      AND bPay.ParentId EQ bPack.PacketId
                    NO-LOCK:

         vPayId = STRING(bPay.PacketId).

         /*=== �᫨ �� ॥��� ���� �訡��, � ������� �ନ�㥬 �� �ᥬ ��ப�� ===*/
         IF NOT {assigned vReeErr} THEN
         DO:
            vPayErr = GetXAttrValueEx("Packet", vPayId, "RapidaErr", "").

            /*=== �� ����᪨� �訡�� ��� �� �������� ===*/
            IF CAN-DO(vNonCrit, vPayErr) THEN
               NEXT lPayFnd.

         END. /* IF NOT {assigned vReeErr} THEN */

         /* ������ � �訡��묨 ���⥦��� */
         CREATE bErrPay.
         ASSIGN
            /* ��� �訡�� */
            bErrPay.PayErr    = vPayErr   WHEN NOT {assigned vReeErr}
            bErrPay.PayErr    = vReeErr   WHEN {assigned vReeErr}

            /* �������� ����� ���⥦� */
            bErrPay.PayRef    = GetXAttrValueEx("Packet", vPayId, "SendRef", "")

            /* ��� �����⥫� ���⥦� */
            bErrPay.OrderAcct = GetXAttrValueEx("Packet", vPayId, "OrderAcct", "")

            /* �㬬� ���⥦� */
            vTmpStr           = GetXAttrValueEx("Packet", vPayId, "amt-rub", "0")
            bErrPay.amt-rub   = GetDecVal(vTmpStr, 0)

            /* ������ �⮣�� */
            vCount            = vCount + 1
            vAmt              = vAmt   + bErrPay.amt-rub
         .
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("RapidReeOutDT", SUBST("PayId: &1, PayRef: &2, amt-rub: &3, PayErr: &4", 
                                                vPayId,
                                                bErrPay.PayRef,
                                                STRING(bErrPay.amt-rub),
                                                bErrPay.PayErr)).
         &ENDIF
      END. /* lPayFnd: FOR EACH bPay WHERE bPay.SeanceId EQ bPack.SeanceId */

      /* ���� ����� ��� ���㧪� */
      IF vCount NE 0 THEN
      DO:
         /* ������� ����� */
         RUN PacketCreateF IN h_pack(INPUT  INT64(iExch::SeanceID),
                                     BUFFER bCode,
                                     OUTPUT vPackId).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("RapidReeOutDT", SUBST("CREATE PACKET: &1", STRING(vPackId))).
         &ENDIF

         IF NOT (vPackId GT 0) THEN
         DO:
            vTmpStr = "�訡�� ᮧ����� ����� �ᯮ�� ��� ॥��� '&1' �� ����� '&2'".
            RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vReeRef, bErrPay.PayRef)).
            UNDO MAIN, LEAVE MAIN.
         END.

         /*=== ���࠭塞 ��������� �����⭮�� ॥��� ===*/
         vTmpStr = SUBST("sum;&1;&2;&3;&4~n",
                         vBankRef,
                         vReeRef,
                         STRING(vCount),
                         TRIM(STRING(vAmt, DecFmt(vAmt, 0)))).
         RUN PacketTextSave IN h_pack(vPackId, vTmpStr).

         FOR EACH bErrPay NO-LOCK:

            /*=== ���࠭塞 ��ப� � �����⠬� ===*/
            vTmpStr = SUBST("pay;&1;&2;&3;&4~n",
                            bErrPay.PayRef,
                            TRIM(STRING(bErrPay.amt-rub, DecFmt(bErrPay.amt-rub, 0))),
                            bErrPay.OrderAcct,
                            bErrPay.PayErr).
            RUN PacketTextSave IN h_pack(vPackId, vTmpStr).

         END. /* FOR EACH bErrPay NO-LOCK: */

         /*=== ���࠭塞 ��� 䠩�� ===*/
         vTmpStr = SUBST("reestr&1.&2.&3.txt",
                         SUBSTR(vReeRef, 1, 4),
                         SUBSTR(vReeRef, 5, 2),
                         SUBSTR(vReeRef, 7, 2)).
         UpdateSigns(bCode.Misc[{&RKC-CLASS}], STRING(vPackId), "FileName", vTmpStr, ?).

         RUN Fill-SysMes IN h_tmess("","","0","").
         RUN Fill-SysMes IN h_tmess("","","0",SUBST("��ନ஢�� ������� ॥���: &1", vTmpStr)).
         RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� �� �����⭮�� ॥����: &1 �� �㬬� &2", 
                                                    STRING(vCount),
                                                    STRING(vAmt, DecFmt(vAmt, 0)))).

         RUN Fill-SysMes IN h_tmess("","","0","").
      END. /* IF vCount NE 0 THEN */
      
   END. /* FOR EACH bPack WHERE bPack.SeanceId EQ vImpSeance */
   
   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeOutDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeOutDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ ��ࢮ� ��ப� ॥��� ����⮩ ��஭� ===============================================*/
{pfuncdef 
   &DEFPROC="CRownReeImpFH"
   &DESCRIPTION="������ ��ࢮ� ��ப� ॥��� ����⮩ ��஭�"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE CRownReeImpFH:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.
DEF VAR vTmpStr   AS  CHAR NO-UNDO.

DEF BUFFER bCode     FOR Code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", "ASSIGN VALUES").
   &ENDIF

   /*=== ���樠������ ��६����� ===*/
   ASSIGN
      iExch::ReeNom      = TRIM(iExch::ReeNom)
      iExch::ReeAmt      = TRIM(iExch::ReeAmt)
      iExch::ReePeny     = TRIM(iExch::ReePeny)
      iExch::ReeHoldAmt  = TRIM(iExch::ReeHoldAmt)
      iExch::ReePayAmt   = TRIM(iExch::ReePayAmt)
      iExch::ReeCountTot = TRIM(iExch::ReeCountTot)
      iExch::ReeAgent    = TRIM(iExch::ReeAgent)
      iExch::ReeNomUsl   = TRIM(iExch::ReeNomUsl)
      iExch::ReeDate     = TRIM(iExch::ReeDate)
      iExch::ReeBegDate  = TRIM(iExch::ReeBegDate)
      iExch::ReeEndDate  = TRIM(iExch::ReeEndDate)
      iExch::ReePrim     = TRIM(iExch::ReePrim)
      iExch::SendId      = TRIM(iExch::SendId)
      iExch::SendRef     = iExch::ReeNom
      iExch::ReeDate     = ENTRY(1, iExch::ReeDate, " ")
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== �饬 ����㦥��� ���㬥�� �१ ��७� ��� ��� ���� ����㧪� ===*/
   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendRef),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("���� 㦥 ����㦥�, 㭨����� ����� ���뫪�: '&1'", 
                                       iExch::SendRef)).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpFH", SUBST("���� 㦥 ����㦥�. &1",
                                             vTmpStr)).
      &ENDIF
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).
      UNDO MAIN, LEAVE MAIN.
   END.

   /*=== ���࠭塞 ��뫪� �� 䠩� ������ ===*/
   RUN PacketCreateRef IN h_rfrnc (gend-date, 
                                   INT64(TRIM(iExch::PacketId)), 
                                   bCode.Misc[{&RKC-REPLY}],
                                   SUBST("&1|&2",TRIM(iExch::SendId),TRIM(iExch::SendRef))).

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CRownReeImpFH */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ ��ப � ���⥦��� ॥��� ����⮩ ��஭� ===========================================*/
{pfuncdef 
   &DEFPROC="CRownReeImpDT"
   &DESCRIPTION="������ ��ப � ���⥦��� ॥��� ����⮩ ��஭�"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE CRownReeImpDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.
DEF VAR vBicMask  AS  CHAR NO-UNDO.          /* ��᪠ �����⨬�� ���                             */
DEF VAR vAcctMask AS  CHAR NO-UNDO.          /* ��᪠ ����㯭�� ��⮢                           */
DEF VAR vDopInfo  AS  CHAR NO-UNDO EXTENT 8. /* �������⥫�� ���� � ᮮ�饭��                  */
DEF VAR vAcct     AS  CHAR NO-UNDO.          /* ��� �����⥫�                                  */
DEF VAR vCurr     AS  CHAR NO-UNDO.          /* ����� ��� �����⥫�                          */
DEF VAR vBIC      AS  CHAR NO-UNDO.          /* ��� ����� �����⥫�                             */
DEF VAR vFilialId AS  CHAR NO-UNDO.          /* ��� ���ࠧ������� �����⥫� ���⥦�             */
DEF VAR vCorr     AS  CHAR NO-UNDO.          /* ����.��� ����� �����⥫� ���⥦�               */
DEF VAR vAcctCr   AS  CHAR NO-UNDO.          /* �࠭���� ��� ���䨫������ ����⮢          */
DEF VAR vOpKind   AS  CHAR NO-UNDO.          /* ��� �࠭���樨 ��� ���᫥���                    */
DEF VAR vTmpStr   AS  CHAR NO-UNDO.

DEF BUFFER bCode  FOR Code.
DEF BUFFER bCard  FOR loan.
DEF BUFFER bAcct  FOR acct.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", "ASSIGN VALUES").
   &ENDIF

   /*=== ���樠������ ��६����� ===*/
   ASSIGN
      vOpKind        = TRIM(iExch::op-kind)
      iExch::FIOCli  = TRIM(iExch::FIOCli)
      iExch::AddrCli = TRIM(iExch::AddrCli)
      iExch::AcctCli = TRIM(iExch::AcctCli)
      iExch::Amt     = TRIM(iExch::Amt)
      iExch::DopInfo = TRIM(iExch::DopInfo)
      iExch::SendRef = TRIM(iExch::SendRef)
      iExch::Peny    = TRIM(iExch::Peny)
      vDopInfo[1]    = GetEntries(1, iExch::DopInfo, ":", "") /* ���.��� ����                    */
      vDopInfo[2]    = GetEntries(2, iExch::DopInfo, ":", "") /* ��� �����⥫�                 */
      vDopInfo[3]    = GetEntries(3, iExch::DopInfo, ":", "") /* ��� �����⥫�                  */
      vDopInfo[4]    = GetEntries(4, iExch::DopInfo, ":", "") /* ��� ���⥫�騪�                 */
      vDopInfo[5]    = GetEntries(5, iExch::DopInfo, ":", "") /* ???                             */
      vDopInfo[6]    = GetEntries(6, iExch::DopInfo, ":", "") /* ���� ���⥫�騪�               */
      vDopInfo[7]    = GetEntries(7, iExch::DopInfo, ":", "") /* ����� ⥫�䮭� ���⥫�騪�      */
      vDopInfo[8]    = GetEntries(8, iExch::DopInfo, ":", "") /* ��� ����� �����⥫�            */
      vBIC           = vDopInfo[8]

      /* ��᪠ �����⨬�� ��⮢ ��� ����������  */
      vBicMask       = TRNSettingValue("","BICMask","!*")

      /* ��᪠ �����⨬�� ��� ������ �����⥫�� */
      vAcctMask      = TRNSettingValue("","AcctMask","!*")
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== �஢�ઠ ����୮� ����㧪� ���⥦� ===*/
   IF NOT {assigned iExch::SendREF} THEN 
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1", "� ॥��� ���� ���⥦� ��� 㭨���쭮�� �����.").
      LEAVE MAIN.
   END. /* IF NOT {assigned iExch::SendREF} THEN  */

   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendREF),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("���⥦ 㦥 ����㦥�, 㭨����� �����: '&1'", 
                                       iExch::SendREF)).
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpFH", SUBST("���⥦ 㦥 ����㦥�. &1",
                                             vTmpStr)).
      &ENDIF
      LEAVE MAIN.
   END. /* IF FndRef(bCode.Misc[{&RKC-REPLY}],  */

   /*=== ��।��塞 ���, �.�. ����� ��।������� ⮫쪮 ����� ����� ===*/
   IF LENGTH(iExch::AcctCli) EQ 16 THEN
   DO:
      /* 16 ᨬ�����, ����祭 ����� ����� */
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpDT", SUBST("CardNum = &1",
                                             iExch::AcctCli)).
      &ENDIF
      FOR FIRST bCard WHERE bCard.contract EQ "card"
                        AND bCard.doc-num  EQ iExch::AcctCli
                      NO-LOCK:
         /* ��諨 ����� */
         /* �� filial-id ������ ��� �����*/
         vBIC = GetXAttrValueEx("branch", bCard.filial-id, "�������", "").
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CRownReeImpDT", SUBST("contract = '&1'; cont-code = '&2'",
                                                bCard.contract,
                                                bCard.cont-code)).
         RUN dbgprint.p ("CRownReeImpDT", SUBST("parent-contract = '&1'; parent-cont-code = '&2'",
                                                bCard.parent-contract,
                                                bCard.parent-cont-code)).
         &ENDIF

         /* �饬 ��� ��� */
         RUN GetRoleAcct IN h_card (SUBST("&1,&2", bCard.parent-contract, bCard.parent-cont-code),
                                    gend-date,
                                    "SCS",
                                    "",
                                    OUTPUT vAcct,
                                    OUTPUT vCurr).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CRownReeImpDT", SUBST("Acct = &1; Curr = &2",
                                                vAcct,
                                                vCurr)).
         &ENDIF
         /* ��ॢ��� � �� ������ ��� �� �ਭ����� */
         IF {assigned vCurr} THEN
            ASSIGN
               vAcct = ""
               vCurr = ""
               vBIC  = ""
            .
      END. /* FOR FIRST bCard WHERE bCard.contract EQ "card" */
   END. /* IF LENGTH(iExch::AcctCli) EQ 16 THEN */
   ELSE IF LENGTH(iExch::AcctCli) EQ 20 THEN
   DO:
      /* 20 ᨬ�����, ����祭 ����� ��� */
      vAcct = iExch::AcctCli.

   END. /* ELSE IF LENGTH(iExch::AcctCli) EQ 20 THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", SUBST("BIC = &1; BicMask = &2",   vBic,  vBicMask)).
   RUN dbgprint.p ("CRownReeImpDT", SUBST("Acct = &1; AcctMask = &2", vAcct, vAcctMask)).
   &ENDIF

   /*=== �஢�ઠ ��� ����� �����⥫� ===*/
   IF NOT (CAN-DO(vBicMask, vBic) AND {assigned vBic}) THEN
   DO:
      /* �� �����⨬� ��� ����� �����⥫� */
      vTmpStr = SUBST("��� &1 ��� ��� '&2': �� ᮮ⢥����� ��᪥ ��� ������ �����⥫��.", 
                      vBic,
                      vAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END. /* IF NOT (CAN-DO(vBicMask, vBic) AND {assigned vBic}) THEN */

   /*=== �஢�ઠ ��� �����⥫� ===*/
   IF NOT (CAN-DO(vAcctMask, vAcct) AND {assigned vAcct}) THEN
   DO:
      /* �� �����⨬� ��� �����⥫� */
      vTmpStr = SUBST("��� '&1' � ���⥦� c ����஬ '&2': �� ᮮ⢥����� ��᪥ �����⥫��.", 
                      vAcct,
                      iExch::SendRef).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END.

   /*=== ��।��塞 ����� ���ࠧ������� �����⥫� ===*/
   RUN CorrBranch IN THIS-PROCEDURE(vBic,
                                    "��������",
                                    gend-date,
                                    OUTPUT vFilialId,
                                    OUTPUT vCorr,
                                    OUTPUT vAcctCr).

   IF {assigned vFilialId} THEN
   DO:
      /*=== �饬 ��� �����⥫� � ���� ===*/
      {find-act.i &filial = vFilialId
                  &acct   = vAcct
                  &curr   = "''"
                  &bact   = bAcct
                  &NoFindInNatCurr = YES}
   END. /* IF {assigned vFilialId} THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   vTmpStr = IF AVAIL(bAcct) THEN bAcct.acct
                             ELSE "not found".
   RUN dbgprint.p ("CRownReeImpDT", SUBST("acct = &1", vTmpStr)).
   &ENDIF                        

   IF AVAIL(bAcct) THEN
   DO:
      /*=== ������塞 �������騥 ���� � ���⥦� ===*/
      ASSIGN
         /* ���� �����⥫�      */
         iExch::bank-code-rec      = vBic                  
         iExch::bank-corr-acct-rec = vCorr                 WHEN {assigned vAcctCr}

         /* �࠭���� ���      */
         iExch::acct-cr            = vAcctCr               WHEN {assigned vAcctCr}

         /* ����� �����⥫�    */
         iExch::acct-rec           = bAcct.acct
         iExch::acct-rec           = bAcct.number          WHEN {assigned vAcctCr}
         iExch::name-rec           = DelDoubleChars(iExch::FIOCli, " ")

         /* �㬬� � ���᫥���   */
         iExch::amt-rub            = iExch::Amt

         /* ��ࠢ�⥫� ���⥦�  */
         iExch::name-send          = DelDoubleChars(vDopInfo[4], " ")

         /* ����ঠ��� ����樨  */
         iExch::details            = ParsDetails(iExch, iExch::details)

         /* �㬥��� ���㬥�⮢ */
         iExch::doc-num            = SetCounterValue(iExch::CounterName, ?, gend-date)
      NO-ERROR. {&NO-ERROR}

      /*=== �஢�ઠ �ࠢ��쭮�� ��᢮���� ���稪� ===*/
      IF NOT {assigned iExch::doc-num} THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","","-1","�訡�� ����祭�� ���祭�� ��� ���稪� ���㬥�⮢").
         LEAVE MAIN.

      END. /* IF NOT {assigned iExch::doc-num} THEN */
   END. /* IF AVAIL(bAcct) THEN */
   ELSE 
   DO:
      vTmpStr = SUBST("��� '&1': �� 㤠���� ��⠭����� �����⥫� ���⥦�.", vAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.

   END. /* IF AVAIL(bAcct) THEN ... ELSE */

   /*=== ������⨧��� ���㬥�� ===*/
   vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
   IF    iExch::op-kind EQ vOpKind   /* ��������� � ⥪�饩                     */
      OR ERROR-STATUS:ERROR          /* �������� �訡�� � �㭪樨 ������⨧�樨 */
      OR NOT {assigned vOpKind}      /* ��� �࠭���樨 ����                     */
   THEN
   DO:

      vTmpStr = "�� ��।����� �࠭����� ᮧ����� ��� ���⥦� � ����஬ '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF    iExch::op-kind EQ vOpKind */
   ELSE
      iExch::op-kind = vOpKind.

   /*=== ����᪠�� �࠭����� ���᫥��� ===*/
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
   &ENDIF

   RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

   &IF DEFINED(IS-DEBUG) &THEN
   RUN DumpObject IN h_exch(iExch).
   &ENDIF

   /*=== �஢�ઠ ᮧ����� ���㬥�� ===*/
   IF iExch::op EQ "0" THEN
   DO:
      vTmpStr = "�訡�� ᮧ����� ���㬥�� � ����஬ '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF iExch::op EQ "0" THEN */

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CRownReeImpDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ����⮢��: ������ �⮣���� ������ ॥��� ================================================*/
{pfuncdef 
   &DEFPROC="CRownReeImpFF"
   &DESCRIPTION="������ ��ப � ���⥦��� ॥��� ����⮩ ��஭�"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}
PROCEDURE CRownReeImpFF:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vTotCount AS  INT64 NO-UNDO.         /* ������⢮ ���⥦�� � ॥���    */
DEF VAR vCount    AS  INT64 NO-UNDO.         /* ������⢮ ᮧ������ ���㬥�⮢  */
DEF VAR vTotSum   AS  DEC   NO-UNDO.         /* ���� �㬬� ����᫥���         */
DEF VAR vSum      AS  DEC   NO-UNDO.         /* ���� �㬬� ᮧ������ ���㬥�⮢ */
DEF VAR vIntFmt   AS  CHAR  NO-UNDO.         /* ��஢����� �� ������ �ଠ�     */
DEF VAR vSumFmt   AS  CHAR  NO-UNDO.         /* ��஢����� �� ������ �ଠ�     */

DEF BUFFER bPack     FOR Packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOpEntry  FOR op-entry.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", "RUN").
   &ENDIF

   ASSIGN
      vTotCount = INT64(iExch::ReeCountTot)
      vTotSum   = DEC(iExch::ReePayAmt)
   NO-ERROR. {&NO-ERROR}

   /*=== ��⠥� �㬬� � ������⢮ ᮧ������ ���㬥�⮢ ===*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketId EQ bPack.PacketId
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op       EQ INT64(ENTRY(1, bPackObj.surrogate))
                       AND bOpEntry.op-ENTRY EQ INT64(ENTRY(2, bPackObj.surrogate))
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END.

   ASSIGN
      vIntFmt = IntFmt(vCount, vTotCount)
      vSumFmt = DecFmt(vSum,   vTotSum)
   .
   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� �� ॥���� ���⥦��: &1 �� �㬬� &2",
                                               STRING(vTotCount, vIntFmt),
                                               STRING(vTotSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("�ᥣ� ᮧ���� ���㬥�⮢:  &1 �� �㬬� &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", SUBST("Reestr: &1, &2; Create: &3, &4",
                                          STRING(vTotCount),
                                          STRING(vTotSum),
                                          STRING(vCount),
                                          STRING(vSum))).
   &ENDIF

   IF (vSum EQ vTotSum) AND (vCount EQ vTotCount) THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","0","���宦����� � �⮣���� ��ப�� ॥��� ���.").
      vFlagSet = YES.
   END.
   ELSE /* IF (vSum EQ vTotSum) AND (vCount NE vTotCount) THEN ... ELSE */
      RUN Fill-SysMes IN h_tmess("","", "-1","������: ���宦����� � �⮣���� ��ப�� ॥���.").

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

RUN Fill-SysMes IN h_tmess("","","0","").

{doreturn.i vFlagSet}

END PROCEDURE. /* CRownReeImpFF */

/*===============================================================================================*/