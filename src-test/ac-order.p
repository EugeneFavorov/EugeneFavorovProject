/*              
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ac-order.p
      Comment: ����஢�� � �⡮� ��⮢ ��� �ᯮ�⠢ ���
               ������� ��⮤�� (��� UPDATE) �����, 㭠᫥�������� �� Filter

               �믮��塞� ����⢨�:

               � ������� �맮�� ������ ����⢮���� ���⠭��:

   Parameters: iClass      - ��� �����
               iInstance   - ᮤ�ন��� ����� iClass
         Uses:
      Used BY:
      Created: 25.03.2004 NIK
     Modified: 09.03.2005 Mike - ���� FX-ORDER.P
     Modified: 07.10.2005 NIK "��८�ଫ����" ३ᮢ �ᯮ��
     Modified: 29.07.2008 12:48 MUTA 0096028 �ᯮ�짮�����  RetRcp
*/
{globals.i}
DEFINE NEW GLOBAL SHARED STREAM debug-stream.

DEFINE INPUT PARAMETER iClass       AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER iInstance    AS handle   NO-UNDO.

{form.def}
{g-trans.equ}
{exchange.equ}
{intrface.get xclass}

{intrface.get tmess}
{intrface.get pbase}
{intrface.get trans}
{intrface.get data}

{intrface.get pack}
{intrface.get exch}
{intrface.get edeal}
{intrface.get flt}
{intrface.get strng}

{sh-defs.i}

{debug.equ}

DEFINE VAR hFilterTable AS handle            NO-UNDO.
DEFINE VAR hFilter      AS handle            NO-UNDO.

DEFINE VAR hBuffer      AS handle            NO-UNDO.
DEFINE VAR mRetryErr    AS CHAR              NO-UNDO.
DEFINE VAR mFlagSet     AS LOGICAL  INIT ?   NO-UNDO.
DEFINE VAR mFileFind    AS LOGICAL           NO-UNDO.

DEFINE VAR mOpKind      AS CHAR              NO-UNDO.
DEFINE VAR mTmplID      AS INT64             NO-UNDO.
DEFINE VAR mRoleMess    AS CHAR              NO-UNDO.
DEFINE VAR mAttrList    AS CHAR              NO-UNDO.
DEFINE VAR mValList     AS CHAR              NO-UNDO.
DEFINE VAR mSeanceID    AS INT64             NO-UNDO.
DEFINE VAR mReForm      AS LOGICAL           NO-UNDO.
DEFINE VAR mRefusal     AS LOGICAL           NO-UNDO.
DEFINE VAR mIsCorrect   AS LOGICAL           NO-UNDO.
DEFINE VAR mOpCl        AS CHAR              NO-UNDO.
DEFINE VAR mFirstFilter AS LOGICAL           NO-UNDO.
DEFINE VAR mBanExp      AS LOGICAL           NO-UNDO.

DEFINE VARIABLE mSilent      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mCodeLstChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mValLstChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateModif   AS DATE      NO-UNDO.

DEF BUFFER Seance       FOR Seance.

DEFINE temp-table ttObj no-undo
    FIELD Acct    AS CHAR
    FIELD CustId  AS INT64
    FIELD FName   AS CHAR
    FIELD DateExp AS DATE
    FIELD TimeExp AS INT64
    FIELD StateExp AS CHAR
    INDEX CustId CustId.

   
{ttretval.def}
&GLOB RETRY-ERROR  mRetryErr                     /* ��� do-retry.i            */
{ac-ordsni.fun}
/*============================================================================*/
FUNCTION LastTaxImp RETURN CHAR (INPUT iAcct      AS CHAR,
                                 INPUT iCurrency  AS CHAR):

   DEF VAR vResult AS CHAR INIT "" NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.
   DEF BUFFER Packet     FOR Packet.
   DEF BUFFER FileExch   FOR FileExch.

   FOR EACH PackObject WHERE
            PackObject.file-name EQ 'acct'
        AND PackObject.Surrogate EQ iAcct + "," + iCurrency  NO-LOCK,
       EACH Packet     WHERE
            Packet.PacketID      EQ PackObject.PacketID
            NO-LOCK,
      FIRST FileExch   WHERE
            FileExch.FileExchID  EQ packet.FileExchID        NO-LOCK
            BY Packet.PackDate DESCENDING
            BY Packet.PackTime DESCENDING:

      IF  Packet.Kind EQ "TaxImp" THEN
         vResult =  SUBSTRING(FileExch.name,3,1).
         LEAVE.
   END.

   RETURN vResult.
END FUNCTION.

/*�ந� �।���� ᮮ�饭��*/
FUNCTION LastTaxImp2 RETURN CHAR PRIVATE (
   INPUT iAcct      AS CHAR,  /* ���. */
   INPUT iCurrency  AS CHAR,  /* ����� ���. */
   INPUT iKindExp   AS CHAR,
   INPUT iKindImp   AS CHAR
):
   DEF VAR vResult   AS CHAR  INIT ""   NO-UNDO.
   DEF VAR vDate     AS DATE            NO-UNDO.
   DEF VAR vTime     AS INT64           NO-UNDO.
   DEF VAR vOpCl     AS CHAR INIT ""    NO-UNDO.
   DEF VAR vSign     AS CHAR INIT ""    NO-UNDO. /*�ਧ��� �. ᮮ�饭��:
                                                   3 - �⬥���饥*/
   DEF VAR vFileName AS CHAR INIT ""    NO-UNDO.
   DEF VAR vStat     AS CHAR INIT ""    NO-UNDO.

   DEF BUFFER PackObject   FOR PackObject.   /* ��������� ����. */
   DEF BUFFER Packet       FOR Packet.       /* ��������� ����. */
   DEF BUFFER Seance       FOR Seance.       /* ��������� ����. */
   DEF BUFFER FileExch     FOR FileExch.     /* ��������� ����. */

                        /* ���� ��᫥����� ᮮ�饭�� �ᯮ��. */
   BLCK_GET_LST_EXP:
   /* SORT-ACCESS Packet ��㦤���� */
   FOR EACH PackObject  WHERE
            PackObject.file-name EQ "acct"
      AND   PackObject.Surrogate EQ iAcct + "," + iCurrency
   NO-LOCK,
   EACH Packet          WHERE
            Packet.PacketID      EQ PackObject.PacketID
      AND   Packet.Kind          EQ iKindExp
   NO-LOCK,
   FIRST Seance         WHERE 
            Seance.SeanceID      EQ Packet.SeanceID
      AND   Seance.Direct        EQ "��ᯮ��"
   NO-LOCK,
   FIRST FileExch       WHERE
            FileExch.FileExchID  EQ packet.FileExchID
   NO-LOCK
      BY Packet.PackDate DESC
      BY Packet.PackTime DESC
   :
      ASSIGN
         vDate  = Packet.PackDate
         vTime  = Packet.PackTime
         vOpCl = SUBSTRING (FileExch.name, 4, 1)
         vSign = SUBSTRING (FileExch.name, 5, 1)
         vFileName = "*" + SUBSTRING(FileExch.name,4,LENGTH(FileExch.name) - 3)
         vStat = Packet.State
      .
      LEAVE BLCK_GET_LST_EXP.
   END.
                        /* ���� ᮮ�饭�� ������
                        ** �� ���������� ᮮ�饭�� �ᯮ��. */
   /* SORT-ACCESS Packet ��㦤���� */
   IF AVAIL Packet THEN
      FOR EACH PackObject  WHERE
               PackObject.file-name EQ "acct"
         AND   PackObject.Surrogate EQ iAcct + "," + iCurrency
      NO-LOCK,
      EACH Packet          WHERE
               Packet.PacketID      EQ PackObject.PacketID
         AND   Packet.Kind          EQ iKindImp
         AND   Packet.PackDate      GE vDate
         AND  (IF Packet.PackDate   EQ vDate
                  THEN Packet.PackTime GT vTime
                  ELSE YES
              )
      NO-LOCK,
      FIRST Seance         WHERE 
               Seance.SeanceID      EQ Packet.SeanceID
         AND   Seance.Direct        EQ "������"
      NO-LOCK,
      FIRST FileExch       WHERE
               FileExch.FileExchID  EQ packet.FileExchID AND
               FileExch.name   MATCHES vFileName
      NO-LOCK
         BY Packet.PackDate DESC
         BY Packet.PackTime DESC
      :
         vResult = vResult + SUBSTRING (FileExch.name, 3, 1).
      END.

   vResult = (IF {assigned vOpCl} THEN vOpCl ELSE "") + CHR(1) + vResult + CHR(1) + 
             (IF {assigned vSign} THEN vSign ELSE "") + CHR(1) +
             vStat.

   RETURN vResult.
END FUNCTION.

/* ----------------------------------------------------------------------------------------- */
FUNCTION LastTaxImp3 RETURN LOGICAL PRIVATE (
   INPUT iAcct      AS CHAR,  /* ���. */
   INPUT iCurrency  AS CHAR,  /* ����� ���. */
   INPUT iKindExp   AS CHAR,
   INPUT iKindImp   AS CHAR,
   INPUT iSNIKORR   AS DATE   
):
   DEF VAR vResult   AS CHAR  INIT ""   NO-UNDO.
   DEF VAR vDate     AS DATE            NO-UNDO.
   DEF VAR vTime     AS INT64           NO-UNDO.

   DEFINE VARIABLE vFlagFind AS LOGICAL INIT NO NO-UNDO.

   DEF BUFFER PackObject    FOR PackObject.   /* ��������� ����. */
   DEF BUFFER Packet        FOR Packet.       /* ��������� ����. */
   DEF BUFFER xSeance       FOR Seance.       /* ��������� ����. */
   DEF BUFFER FileExch      FOR FileExch.     /* ��������� ����. */
   DEF BUFFER bPackObject   FOR PackObject.   /* ��������� ����. */
   DEF BUFFER bPacket       FOR Packet.       /* ��������� ����. */
   DEF BUFFER bSeance       FOR Seance.       /* ��������� ����. */
   DEF BUFFER bFileExch     FOR FileExch.     /* ��������� ����. */

   &IF DEFINED(IS-DEBUG) &THEN
      run dbgprint.p(program-name(1) + " {&line-number} ===== iAcct", iAcct).
      run dbgprint.p(program-name(1) + " {&line-number} iCurrency", iCurrency).
      run dbgprint.p(program-name(1) + " {&line-number} iKindExp", iKindExp).
      run dbgprint.p(program-name(1) + " {&line-number} iKindImp", iKindImp).
      run dbgprint.p(program-name(1) + " {&line-number} iSNIKORR", string(iSNIKORR)).
   &ENDIF 
                        /* ���� ��᫥����� ᮮ�饭�� �ᯮ��. */
   BLCK_GET_LST_EXP:
   /* SORT-ACCESS Packet */
   FOR EACH bPackObject  WHERE
      bPackObject.file-name EQ "acct" AND
      bPackObject.Surrogate EQ iAcct + "," + iCurrency
         NO-LOCK,
      EACH bPacket WHERE
         bPacket.PacketID      EQ bPackObject.PacketID AND
         bPacket.Kind          MATCHES iKindExp
            NO-LOCK,
      FIRST bSeance WHERE 
         bSeance.SeanceID      EQ bPacket.SeanceID AND
         bSeance.Direct        EQ "��ᯮ��"
            NO-LOCK,
      FIRST bFileExch WHERE
         bFileExch.FileExchID  EQ bpacket.FileExchID
            NO-LOCK
      BY bPacket.PackDate DESC
      BY bPacket.PackTime DESC:

      ASSIGN
         vDate  = bPacket.PackDate
         vTime  = bPacket.PackTime
      NO-ERROR.

      LEAVE BLCK_GET_LST_EXP.
   END.
                        /* ���� ᮮ�饭�� ������
                        ** �� ���������� ᮮ�饭�� �ᯮ��. */
   IF AVAIL bPacket THEN DO:
      &IF DEFINED(IS-DEBUG) &THEN
         run dbgprint.p(program-name(1) + " {&line-number} vDate", string(vDate)).
         run dbgprint.p(program-name(1) + " {&line-number} vTime", string(vTime,"hh:mm:ss")).
         run dbgprint.p(program-name(1) + " {&line-number} bPacket.PacketID", 
            string(bPacket.PacketID)). 
         run dbgprint.p(program-name(1) + " {&line-number} bSeance.Number", string(bSeance.Number)). 
      &ENDIF
   END.

   IF AVAIL bPacket THEN DO:
      /* SORT-ACCESS Packet */
      FOR EACH PackObject  WHERE
         PackObject.file-name EQ "acct" AND
         PackObject.Surrogate EQ iAcct + "," + iCurrency
            NO-LOCK,
         EACH Packet WHERE
            Packet.PacketID      EQ PackObject.PacketID AND
            Packet.Kind          EQ iKindImp AND
            Packet.PackDate      GE vDate AND
            (IF Packet.PackDate  EQ vDate
                  THEN Packet.PackTime GT vTime
                  ELSE YES)
            NO-LOCK,
      FIRST xSeance WHERE 
         xSeance.SeanceID      EQ Packet.SeanceID AND
         xSeance.Direct        EQ "������"
            NO-LOCK,
      FIRST FileExch WHERE
         FileExch.FileExchID  EQ packet.FileExchID
            NO-LOCK
      BY Packet.PackDate DESC
      BY Packet.PackTime DESC:
         vResult = SUBSTRING (FileExch.name, 3, 1).

         &IF DEFINED(IS-DEBUG) &THEN
            run dbgprint.p(program-name(1) + " {&line-number} vResult", vResult).
            run dbgprint.p(program-name(1) + " {&line-number} FileExch.name", FileExch.name).
            run dbgprint.p(program-name(1) + " {&line-number} Packet.PackDate", 
               string(Packet.PackDate)).
            run dbgprint.p(program-name(1) + " {&line-number} Packet.PackTime", 
               string(Packet.PackTime,"hh:mm:ss")).
         &ENDIF

         IF vResult EQ 'K' THEN
            vFlagFind = YES.

         ELSE IF (vResult EQ 'F') AND (vDate LT iSNIKORR) THEN
            vFlagFind = YES.
      END.
   END.

   IF iAcct EQ "42301810505160012371     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810905160012138     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810105090013138     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810105090012901     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810705120013466     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810405120013240     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810005110014379     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810605110013801     @0500" THEN vFlagFind = YES.

   IF iAcct EQ "42306810005980016984     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810205980017568     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42301810905980017567     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810305980016985     @0500" THEN vFlagFind = YES.
   
   IF iAcct EQ "42307810205120010057     @0500" THEN vFlagFind = YES.
   
   IF iAcct EQ "42301810605160011544     @0500" THEN vFlagFind = YES.
   IF iAcct EQ "42306810005160011372     @0500" THEN vFlagFind = YES.



   &IF DEFINED(IS-DEBUG) &THEN
      run dbgprint.p(program-name(1) + " {&line-number} vFlagFind", string(vFlagFind)).
   &ENDIF

   RETURN vFlagFind.
END FUNCTION.
/* ----------------------------------------------------------------------------------------- */

{empty ttObj}
MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      hFilter     = iInstance:default-buffer-handle
      mTmplID     = INT64(hFilter:buffer-field("__tmpl-ID"):buffer-value)
      mOpKind     = GetBaseOpKind()
      mRoleMess   = GetAttrValue(mOpKind, mTmplID, "$RoleMessage")
      mReForm     = TRNSettingValue("","SeanceReform","���") EQ "��" 
      mRefusal    = TRNSettingValue("","Refusal","���") EQ "��"
      mIsCorrect  = TRNSettingValue("","Correction","���") EQ "YES"
      mOpCl       = GetAttrValue(mOpKind, mTmplID, "$OpCl")
      mBanExp     = FGetSetting("����⎤�����","���","���") EQ "��"                
      mSeanceID   = INT64(GetAttrValue2(mOpKind,mTmplID,"$SeanceID"))
      mSilent     = CAN-DO("��,YES", GetAttrValue2(mOpKind, mTmplID, "$Silent"))   
      mDateModif  = DATE(FGetSetting("���","���311","01/07/2014")) 
   NO-ERROR.
   IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.   
   
   RUN AddAttr2TableEx IN h_trans ("", 0, -1, "", 0, "OpKindKind", SUBSTRING(mRoleMess,14)).

   mReForm = NO. 
   MESSAGE COLOR NORMAL
      "�믮����� ����ନ஢���� ᥠ�� "                SKIP
      "�� ⥪�饬� 䨫���� " + ENTRY(3,hFilter::FiltInfo)  + " ? "       SKIP
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE mReForm.

   RUN GetParentXMethod (iClass,"update", BUFFER class-method).
   IF AVAILABLE(class-method) THEN DO:

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ac-order.p","class-method:" + class-method.PROCEDURE).
   &ENDIF

      {exch-run.i &Proc = class-method.PROCEDURE
                  &Parm = "iClass, iInstance"}
      IF ERROR-STATUS:ERROR THEN DO:
         mRetryErr = "���������� �믮����� ��⮤ """ + iClass +
                                                 "." + class-method.PROCEDURE + """".
         mFlagSet  = NO.
         UNDO MAIN, RETRY MAIN.
      END.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ac-order.p","END:" + class-method.PROCEDURE).
   &ENDIF

   ASSIGN
      hFilterTable = widget-handle(hFilter:buffer-field("FilterTable"):buffer-value)
   NO-ERROR.

   IF NOT valid-handle(hFilterTable) THEN DO:
      mRetryErr = "��� ������ ��� �ᯮ��".
      mFlagSet  = YES.
      UNDO MAIN, LEAVE MAIN.
   END.

   hBuffer = hFilterTable:default-buffer-handle.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ac-order.p","mIsCorrect:" + STRING(mIsCorrect)).
   &ENDIF
/*   message GetAttrValue(mOpKind, mTmplID, "$VersForm") mOpKind mTmplID view-as alert-box. */
   IF GetAttrValue(mOpKind, mTmplID, "$VersForm") GE "5.10" THEN
   DO:
      IF NOT mRefusal THEN       
         IF mIsCorrect THEN 
            RUN AcctDeleteNoSend(hBuffer,mRoleMess).
         ELSE
            RUN AcctDelete510(hBuffer).           
      ELSE
         RUN AcctDeleteRefusal(hBuffer).
      mFirstFilter =
       GetAttrValue2(GetBaseOpKind(),GetBaseTemplate(),"$FirstFilter") EQ "��".

      IF mFirstFilter THEN
      DO:
         FIND FIRST Seance WHERE
                    Seance.SeanceID EQ mSeanceID NO-LOCK NO-ERROR.
         IF AVAIL(Seance) THEN 
            RUN snc-cncl-exh.p (BUFFER Seance, NO).
      END.
      IF mBanExp THEN
         RUN AcctDeleteBan(hBuffer).
   END.
   ELSE
      IF mIsCorrect THEN 
         RUN AcctDeleteNoSend(hBuffer,mRoleMess).
      ELSE
         RUN AcctDeleteSend(hBuffer,mRoleMess).

   hBuffer:find-first() NO-ERROR.         
   IF ERROR-STATUS:ERROR THEN DO:
      mRetryErr = "��� ������ ��� �ᯮ��".
      mFlagSet  = YES.
      UNDO MAIN, LEAVE MAIN.
   END.
   
   mAttrList = GetInstanceProp(iInstance,"AttrList").
   IF {assigned mAttrList} THEN
      ASSIGN
         mValList  = CHR(1) + GetInstanceProp(iInstance,"ValList")
         mAttrList = CHR(1) + mAttrList.

   ASSIGN
       mCodeLstChar = "FilterTable" + CHR(1) +
                      "RidRest"     + CHR(1) +
                      "RetRcp"      + CHR(1) +
                      "RetFld"      + CHR(1) +
                      "RetType"              +
                      mAttrList
       mValLstChar  = STRING(hFilterTable)               + CHR(1) +
                      "YES"                              + CHR(1) +
                      STRING(TEMP-TABLE ttRetVal:HANDLE) + CHR(1) +
                      "ROWID"                            + CHR(1) +
                      "Multi"                                     +
                      mValList
   .

USR-SEL:
   REPEAT:
   {empty ttRetVal}

   IF mSilent THEN DO:
       RUN SelectFltObject IN h_flt ("acct", mCodeLstChar, mValLstChar, "").
       LEAVE USR-SEL.
   END.
   ELSE
      RUN browseld.p ("acct", mCodeLstChar, mValLstChar, "", 4).

      IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN DO:
         IF CAN-FIND(FIRST ttRetVal) THEN DO:

            MESSAGE COLOR NORMAL
                    "�믮����� ᮧ����� ᮮ�饭�� ��� �⬥祭��� ��⮢ ?" SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            SET vChoice AS LOGICAL.
            CASE vChoice:
               WHEN YES THEN DO:
                  RUN KeepSelectFilter(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN NO  THEN DO:
                  RUN DelFilterTable(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN ? THEN NEXT  USR-SEL.
            END CASE.
         END.
         ELSE DO:
            MESSAGE COLOR NORMAL
                    "�� �⬥祭  �� ���� ��� ��� �ᯮ��." SKIP
                    "�த������ �믮������ �࠭���樨 ?" SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  SET vChoice.
            CASE vChoice:
               WHEN YES THEN DO:
                  RUN DelFilterTable(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN NO THEN DO:
                  RUN DelFilterTable(hBuffer).
                  mRetryErr = "�⪠� �� �믮������ �ᯮ��".
                  mFlagSet  = NO.
                  UNDO MAIN, RETRY MAIN.
               END.
               WHEN ?  THEN NEXT  USR-SEL.
            END CASE.
         END.
      END.
      ELSE IF KEYFUNCTION(LASTKEY) EQ "GO" THEN DO:
         IF CAN-FIND(FIRST ttRetVal) THEN DO:
            RUN KeepSelectFilter(hBuffer).
            LEAVE USR-SEL.
         END.
         ELSE DO:
            MESSAGE COLOR NORMAL
                    "�믮����� �ᯮ�� ��� ��⮢ ?" SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            SET vChoice.
            CASE vChoice:
               WHEN YES THEN DO:
                  IF GetAttrValue(mOpKind, mTmplID, "$VersForm") GE "5.10" THEN
                     RUN KeepSelectFilterForAll(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN NO  THEN DO:
                  RUN DelFilterTable(hBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN ? THEN NEXT  USR-SEL.
            END CASE.
         END.
      END.
   END.                                          /* USR-SEL: REPEAT:          */

   mFlagSet = YES.
END.                                             /* MAIN:  DO:                */

&IF DEFINED(IS-DEBUG) &THEN
RUN PrintFilterLog (hBuffer).
&ENDIF

{intrface.del}

{doreturn.i mFlagSet}
/*----------------------------------------------------------------------------*/
/* ���࠭���� �⮡࠭�� ���祭��                                              */
/*----------------------------------------------------------------------------*/
PROCEDURE KeepSelectFilter:
   DEFINE INPUT PARAMETER hBuffer AS handle NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vacct     AS CHAR              NO-UNDO.
   DEFINE VAR vcurrnecy AS CHAR              NO-UNDO.

   CREATE QUERY hQuery.
   hQuery:ADD-BUFFER(hBuffer).
   IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
      OR NOT hQuery:QUERY-OPEN()  THEN RETURN.

   hQuery:GET-FIRST().
   DO WHILE hBuffer:AVAILABLE:
      ASSIGN
         vacct     = hBuffer:buffer-field("acct"):buffer-value
         vcurrnecy = hBuffer:buffer-field("currency"):buffer-value
      NO-ERROR.

      FIND FIRST acct WHERE acct.acct     EQ vacct
                        AND acct.currency EQ vCurrnecy NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         IF NOT CAN-FIND(FIRST ttRetVal WHERE
                               ttRetVal.FileRowId EQ ROWID(acct)) THEN
            hBuffer:BUFFER-DELETE().
         ELSE
            IF GetAttrValue(mOpKind, mTmplID, "$VersForm") GE "5.10" THEN
               IF NOT isSNI (vacct,vCurrnecy) THEN 
                  hBuffer:BUFFER-DELETE().
      hQuery:GET-NEXT().
   END.
   hQuery:QUERY-CLOSE().

   DELETE OBJECT hQuery.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                                                                                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteSend:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.
   DEFINE INPUT PARAMETER iDisable  AS CHAR     NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vDisable  AS CHAR              NO-UNDO.
   DEFINE VAR vEnable   AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

         vItem = LastTaxImp(vAcct,vCurrency).

   &IF DEFINED(IS-DEBUG) &THEN
   run dbgprint.p ("AcctDeleteSend","vAcct:"      + vAcct  +
                                    " vItem:"     + vItem  +
                                    " mRoleMess:" +  mRoleMess).
   &ENDIF
  
         IF mRoleMess MATCHES "*close*"
         THEN DO:
            IF vItem EQ "K"  OR
               vItem EQ "N"  OR
               vItem EQ "T"  OR
               vItem EQ "E" 
               THEN DO:
                  hQuery:GET-NEXT() NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                  NEXT LOOP.
               END.
         END.
         ELSE DO:
            IF vItem EQ "E"  
               THEN DO:
                  hQuery:GET-NEXT() NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                  NEXT LOOP.
               END.
         END.

         IF mRefusal THEN DO:
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vacct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                     NO-LOCK:
               vFlagFind = YES.
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         ELSE IF NOT mReForm THEN DO:

            IF NOT PacketEnableExhange ("acct",
                                        vacct + "," + vCurrency,
                                        iDisable,
                                        {&NOTHING},
                                        0) THEN DO:

               hBuffer:BUFFER-DELETE() NO-ERROR.
               IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.
         END.
         ELSE DO:                      /* ������� �� ᮮ�饭��, �஬� ⥪�饣� ३� */
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vacct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                 AND Packet.SeanceID EQ mSeanceID
                     NO-LOCK:

               vFlagFind = YES.    
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().

      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */

   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ���४��                                                                  */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteNoSend:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.
   DEFINE INPUT PARAMETER iDisable  AS CHAR     NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vDisable  AS CHAR              NO-UNDO.
   DEFINE VAR vEnable   AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vSNIKORR  AS DATE              NO-UNDO.
   DEFINE VAR vAcct-Surr AS CHAR             NO-UNDO.      

   DEF BUFFER Packet     FOR Packet.
   DEF BUFFER PackObject FOR PackObject.
   DEF BUFFER FileExch   FOR FileExch.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
            vAcct-Surr = SUBSTITUTE("&1,&2",vAcct,vCurrency)
            vSNIKORR  = DATE(GetXAttrValueEX("acct",vAcct-Surr,"�������",?))            
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

         ASSIGN
            vFlagFind = LastTaxImp3(vAcct, vCurrency, "TaxExp*5*", "TaxImp", vSNIKORR)
         NO-ERROR.

         IF NOT vFlagFind THEN DO:          
            &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("AcctDeleteSend","acct:" + vacct + "->delete").
            &ENDIF
            
            hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().

      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */

   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*�⡮� ��⮢ � ��⮬ 㦥 ���㦥���� ᮮ�饭��                             */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDelete510:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vOpCl     AS CHAR              NO-UNDO.
   DEFINE VAR vSign     AS CHAR              NO-UNDO.
   DEFINE VAR vStat     AS CHAR     INIT ""  NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.
   &IF DEFINED(IS-DEBUG) &THEN
      run dbgprint.p(program-name(1) + " {&line-number} AcctDelete510", "START").
   &ENDIF

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).
         &IF DEFINED(IS-DEBUG) &THEN
            run dbgprint.p(program-name(1) + " {&line-number} ===== vAcct", vAcct).
         &ENDIF
         {find-act.i &acct = vAcct
                     &curr = vCurrency
         }
         IF mReform THEN                /*�᫨ ���짮��⥫� �⢥⨫ ��*/
         DO:                            /*�� ����ନ஢���� �� 䨫����*/
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vAcct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                 AND Packet.SeanceID EQ mSeanceID
                     NO-LOCK:
               vFlagFind = YES.
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         ELSE
         DO:
            {empty ttObj}
            ASSIGN
               vItem = LastTaxImp2(vAcct,           vCurrency,
                                    "TaxExpCommon5X","TaxImp")
            .
            &IF DEFINED(IS-DEBUG) &THEN
               run dbgprint.p(program-name(1) + " {&line-number} vItem", vItem).
            &ENDIF

            ASSIGN
               vOpCl = ENTRY(1,vItem,CHR(1))
               vSign = ENTRY(3,vItem,CHR(1))
               vStat = GetENTRies(4,vItem,CHR(1),"")
               vItem = ENTRY(2,vItem,CHR(1))
            .

            &IF DEFINED(IS-DEBUG) &THEN
               run dbgprint.p(program-name(1) + " {&line-number} mOpCl", mOpCl).
               run dbgprint.p(program-name(1) + " {&line-number} vOpCl", vOpCl).
               run dbgprint.p(program-name(1) + " {&line-number} vSign", vSign).
               run dbgprint.p(program-name(1) + " {&line-number} vItem", vItem).
               run dbgprint.p(program-name(1) + " {&line-number} vStat", vStat).
            &ENDIF

            IF {assigned vOpCl} THEN        /*�᫨ ���� �ᯮ�� �*/
            DO:
               &IF DEFINED(IS-DEBUG) &THEN
                  run dbgprint.p(program-name(1) + " {&line-number} ", "AcctDelete510 1").
               &ENDIF
               IF vItem EQ ""  OR       /*��� ������*/
                  (
                   (INDEX(vItem,"F") NE 0 OR       /*��� ������ ���� � ⨯��� F, R ��� K*/
                    INDEX(vItem,"R") NE 0 OR
                    INDEX(vItem,"P") NE 0 OR
                    INDEX(vItem,"N") NE 0 OR
                    INDEX(vItem,"K") NE 0
                   )                      AND
                    INDEX(vItem,"E") EQ 0          /*�� �� �⮬ ��� ������ � ⨯�� E*/
                  )            OR
                   (INDEX(vItem,"E") NE 0   AND  /*��� ���� c ⨯�� E, �� */
                    mOpCl            EQ "0" AND  /*�࠭����� ������*/
                    vOpCl            NE "0"      /*� ��� ᮮ�� (� �訡 �����⮬)  - �� ����⨥*/
                   ) THEN
               DO:                   /*� 㤠�塞 ��� �� �롮ન �� �᫮���:*/
                  &IF DEFINED(IS-DEBUG) &THEN
                     run dbgprint.p(program-name(1) + " {&line-number} ", "AcctDelete510 2").
                  &ENDIF
                /*�᫨ �࠭����� ������ (0) � ���. ᮮ�饭�� �ᯮ�� ��
                  ���
                  �࠭�. ������� � ���. ᮮ�饭�� �ᯮ�� ⮦� �����⨥ (2)
                    */
                  IF mOpCl EQ "0"    OR 
                    (mOpCl EQ "2"    AND
                     mOpCl EQ vOpCl) THEN
                  DO:
                     &IF DEFINED(IS-DEBUG) &THEN
                        run dbgprint.p(program-name(1) + " {&line-number} ", "AcctDelete510 3").
                     &ENDIF
                     IF
                        ((mOpCl EQ "0"        OR /*�᫨ �࠭����� ������ � */
                          mOpCl EQ "2")       AND /*���. ᮮ�饭�� �� ����⨨*/
                         mOpCl EQ vOpCl)      AND /*��� �᫨ �࠭����� ������� � */ 
                                                  /*���. ᮮ�饭�� � �����⨨*/
                        vSign EQ "3"          AND  /*�� �⮬ �⬥���饥 � */
                        {assigned vItem}      AND  /*᪢�⮢�����*/
                        INDEX(vItem,"E") EQ 0 THEN /*�ᯥ譮*/
                     .   /*� �� 㤠�塞 �� �롮ન*/
                     ELSE
                     DO:
                        &IF DEFINED(IS-DEBUG) &THEN
                           run dbgprint.p(program-name(1) + " {&line-number} AcctDelete510",
                                          "DELETE !").
                        &ENDIF
                        hBuffer:BUFFER-DELETE() NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                     END.
                  END.
                  ELSE IF mOpCl EQ "2" THEN
                  DO:
                     &IF DEFINED(IS-DEBUG) &THEN
                        run dbgprint.p(program-name(1) + " {&line-number} 2 vOpCl ",  vOpCl).
                        run dbgprint.p(program-name(1) + " {&line-number} 2 vItem ",  vItem).
                     &ENDIF 
                     IF ((vOpCl EQ "0" OR vOpCl EQ "1") AND
                         (NOT {assigned vItem} OR
                          (INDEX(vItem,"F") EQ 0 AND
                           INDEX(vItem,"E") EQ 0 
                          )
                         ) AND NOT CAN-DO("����,����",vStat)
                        ) OR vOpCl EQ "" THEN
                     DO:
                        &IF DEFINED(IS-DEBUG) &THEN
                           run dbgprint.p(program-name(1) + " {&line-number} ",
                                          "�����⨥ � ����⨥ �� ᪢�⮢���").
                        &ENDIF
                        /*��।����� ��� ������ � ������� � ���� ����*/  
                        /*hBuffer:BUFFER-DELETE() NO-ERROR.*/
                        IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
                     END.
                  END.
               END.
            END.
            ELSE IF mOpCl EQ "2" AND
               NOT {assigned vOpCl} AND
               acct.open-date >= mDateModif THEN
            DO:
               RUN dbgprint.p(program-name(1) + " {&line-number} ",
                              "��� ��祣�").
               IF  shFilial NE "0500" 
               AND shFilial NE "0300" 
               AND shFilial NE "0000" THEN
                  hBuffer:BUFFER-DELETE() NO-ERROR.
               IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.

         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().
      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */
   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*�⡮� ��⮢ ��� �⬥�����: ���� �ᯮ�� � ������ �� ���� �� �訡���     */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteRefusal:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.
   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vOpCl     AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}
      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.
      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).
         IF mReform THEN                /*�᫨ ���짮��⥫� �⢥⨫ ��*/
         DO:                            /*�� ����ନ஢���� �� 䨫����*/
            vFlagFind = NO.
            FOR EACH PackObject WHERE
                     PackObject.file-name EQ "acct"
                 AND PackObject.surrogate EQ vAcct + "," + vCurrency
                     NO-LOCK,
               FIRST Packet WHERE
                     Packet.PacketID EQ PackObject.PacketID
                 AND Packet.SeanceID EQ mSeanceID
                     NO-LOCK:
               vFlagFind = YES.
               LEAVE.
            END.
            IF NOT vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.
         END.
         ELSE
         DO:
            ASSIGN
               vItem = LastTaxImp2(vAcct,           vCurrency,
                                    "TaxExpCommon5X","TaxImp")
               vOpCl = ENTRY(1,vItem,CHR(1))
               vItem = ENTRY(2,vItem,CHR(1))
            .
            IF NOT {assigned vOpCl} THEN    /*�᫨ ��� �ᯮ�� ���*/
            DO:
              hBuffer:BUFFER-DELETE() NO-ERROR.   /*� 㤠�塞 ��� �� �롮ન*/
              IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.
            ELSE
               IF      {assigned vOpCl} AND    /*�ᯮ�� ����, �� */
                  (NOT {assigned vItem} OR     /*������ ��� ���  */
                       INDEX(vItem,"E") EQ LENGTH(vItem)) /*������ � �訡��  */
            THEN   
            DO:                                /*(SFE,SBE),       */
              hBuffer:BUFFER-DELETE() NO-ERROR.   /*� 㤠�塞 ��� �� �롮ન*/
              IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
            END.
         END.
         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().
      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */
   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* ����� ࠧ��� ᮮ�饭�� �� ������ ���� � ���� ����                        */
/*----------------------------------------------------------------------------*/
PROCEDURE AcctDeleteBan:
   DEFINE INPUT PARAMETER hBuffer   AS HANDLE   NO-UNDO.

   DEFINE VAR hQuery    AS HANDLE            NO-UNDO.
   DEFINE VAR vFlagSet  AS LOGICAL  INIT ?   NO-UNDO.
   DEFINE VAR vAcct     AS CHAR              NO-UNDO.
   DEFINE VAR vCurrency AS CHAR              NO-UNDO.
   DEFINE VAR vItem     AS CHAR              NO-UNDO.
   DEFINE VAR vOpCl     AS CHAR              NO-UNDO.
   DEFINE VAR vFlagFind AS LOGICAL           NO-UNDO.

   DEF BUFFER PackObject FOR PackObject.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY hQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      hQuery:ADD-BUFFER(hBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:name)
         OR NOT hQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      hQuery:GET-FIRST().
LOOP:
      DO WHILE hBuffer:AVAILABLE:
         ASSIGN
            vAcct     = hBuffer:buffer-field("acct"):buffer-value
            vCurrency = hBuffer:buffer-field("currency"):buffer-value
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

/*       C��, �� ����� � ⥪�饬 ��� �뫠 ��ࠢ�� ��㣮�� ���� ᮮ�饭��, ������ ���� �᪫�祭� �� �⡮�. */

         vFlagFind = NO.
         FOR EACH PackObject WHERE
                  PackObject.file-name EQ "acct"
              AND PackObject.surrogate EQ vAcct + "," + vCurrency
                  NO-LOCK,
            FIRST Packet WHERE
                  Packet.PacketID   EQ PackObject.PacketID
              AND Packet.Kind       EQ "TaxExpCommon5X"
                  NO-LOCK,
             EACH Seance WHERE 
                  Seance.SeanceID   EQ Packet.SeanceID
              AND Seance.op-kind    NE mOpKind   
              AND Seance.SeanceDate EQ TODAY
                  NO-LOCK,
            FIRST FileExch   WHERE
                  FileExch.FileExchID EQ packet.FileExchID
              AND FileExch.name BEGINS "SFC"
            NO-LOCK:

               ASSIGN
                  vFlagFind = YES
               NO-ERROR.
               LEAVE.
         END.
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ac-order.p","vAcct:" + vAcct + 
                                      " mOpKind:" + mOpKind +
                                      " vFlagFind:" + STRING(vFlagFind)).
         &ENDIF
         IF vFlagFind THEN hBuffer:BUFFER-DELETE() NO-ERROR.

         hQuery:GET-NEXT() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.
      END.
      hQuery:QUERY-CLOSE().
      vFlagSet = YES.
   END.                                          /* MAIN: DO ...              */
   IF valid-handle(hQuery) THEN DELETE OBJECT hQuery.
   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
&IF DEFINED(IS-DEBUG) &THEN
PROCEDURE PrintFilterLog:
   DEFINE INPUT PARAMETER hBuffer AS handle NO-UNDO.
   DEFINE VAR vItm   AS INT64  NO-UNDO.
   DEFINE VAR hField AS handle   NO-UNDO.

   DEFINE VAR hQuery  AS handle NO-UNDO.

   CREATE QUERY hQuery.
   hQuery:add-buffer(hBuffer).
   IF NOT hQuery:query-prepare("for EACH " + hBuffer:name) THEN MESSAGE "ERROR 1" ERROR-STATUS:get-message(1) VIEW-AS ALERT-BOX.
   IF NOT hQuery:query-open()                              THEN MESSAGE "ERROR 2" ERROR-STATUS:get-message(1) VIEW-AS ALERT-BOX.
   hQuery:get-first().
   DO WHILE hBuffer:AVAILABLE:
      DO vItm = 1 TO hBuffer:num-fields:
         hField = hBuffer:buffer-field(vItm).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("PrintFilterLog",string(hField:Name,"x(30)") + " " +
                                          hField:buffer-value).
         &ENDIF
      END.
      hQuery:get-next().
   END.
   hQuery:query-close().

   DELETE object hQuery.
END PROCEDURE.
&ENDIF
/******************************************************************************/


PROCEDURE DelFilterTable.
   DEFINE INPUT PARAMETER hBuffer AS handle NO-UNDO.

   IF valid-handle(hBuffer) THEN 
      hBuffer:empty-temp-table() NO-ERROR.

END PROCEDURE.
/* $LINTFILE='ac-order.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTUSER='mkv' */
/* $LINTDATE='15/12/2016 11:53:43.227+03:00' */
/*prosignIoZAWuY4+Ri1eG0vLLgbWA*/