/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: CHST-OPA.P
      Comment: ����� ����� ���㬥��
   Parameters: ���   (? - ��� �㤥� ����襭�)
               ����� (YES - ����� �㤥� ����襭,
                       NO  - ����� ��⠭���� ��� ���������)
         Uses:
      Used BY:
      Created: 06/03/1996 eagle
     Modified: 28/11/1997 Dima - smarter history IN Chst(Op.I
     Modified: 29/11/1997 nata -  ����� �஢������ �� �����������
                                  �஢������ ��ࠡ�⮪
     Modified: 24/07/2002 Gunk - ��⮭㬥��� � ������ ��������� �����.
     Modified: 14/10/2002 Gunk - ����䥩� kau
     Modified: 02/12/2002 Olenka - ����� ����� ࠧ�襭� ⮫쪮 ��� ��㯯�
                                   ���㬥�⮢ � ��������묨 ��⠬�, ����ᠬ�
                                   � ��⥣��ﬨ
     Modified: 05/03/2003 Olenka - (0014583) �������� �஢�ન �� ���㬥�⠬.
     Modified: 09.06.2003 NIK    ����஫� ���㬥�� � 楫��
     Modified: 25.11.2005 kraw (0047185) ����������� ��⮭㬥�樨 �� ����� Counters
     Modified: 17/09/2009 kraw (0116286) �� �㤥� �஢����� �����஢��
*/

DEF INPUT PARAM in-op-date   AS DATE NO-UNDO. /* ��� */
DEF INPUT PARAM in-op-status AS LOG  NO-UNDO. /* �ਧ��� ����� ����� �
                                                 �஢�ப - ����� YES */

{defoptr.i new}

{globals.i}             /* �������� ��६���� ��ᨨ. */
{def-wf.i new} 
{history.def}
{intrface.get op}       /* ������⥪� ��� ࠡ��� � ���㬥�⠬�. */
{intrface.get kau}      /* ������⥪� ��� ࠡ��� � ����⠬�. */
{intrface.get instrum}  /* ������⥪� ��� ࠡ��� � 䨭. �����㬥�⠬�. */
{intrface.get separate} /* ������⥪� ��� ࠡ��� � ��⥣��ﬨ. */
{intrface.get acct}     /* ������⥪� ��� ࠡ��� � ��⠬�. */
{intrface.get count}    /* ������⥪� ��� ࠡ��� � ���稪���. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get sessions} /* �����㬥��� ��ࠡ�⪨ ᬥ� ���. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */
{dps-open-lnk.pro}
{op-115fl.def}
{history.def}

{status.tt new}

/* ��⠢�� ���� ���� */
{pqvrn.fun}
/* ����� ��⠢�� ���� ���� */
DEF VAR vstatus     LIKE op.op-status NO-UNDO.
DEF VAR cur-op-date AS DATE NO-UNDO.
DEF VAR close-date  AS DATE NO-UNDO. /* ��� ������� ��⥣�ਨ ���㬥�� */
DEF VAR summa       AS DEC NO-UNDO.
DEF VAR summa1      LIKE op-entry.amt-rub NO-UNDO.

DEF VAR rid    AS RECID EXTENT 50 NO-UNDO.
DEF VAR j      AS INT64 NO-UNDO.
DEF VAR flager AS INT64 NO-UNDO.
DEF VAR allsum AS LOG INIT YES NO-UNDO.

DEFINE VARIABLE vTmpStatus    LIKE op.op-status NO-UNDO.
DEFINE VARIABLE vDiffStatuses AS   LOGICAL      NO-UNDO.
DEFINE VARIABLE mDprIdList    AS   CHARACTER    NO-UNDO.
DEFINE VARIABLE mKauDprId     AS   CHARACTER    NO-UNDO.
DEFINE VARIABLE mI            AS   INT64        NO-UNDO.
DEFINE VARIABLE mCorrChk      AS   CHARACTER    INIT "off-corracct"
                                                NO-UNDO.

{tmprecid.def}
{tmprecid.def &PREF = "op-"}
{tmprecid.def &pref = "cl-"}

DEFINE BUFFER cl-tmprecid-1 FOR cl-tmprecid.

DEF BUFFER bufcode FOR code.
DEF BUFFER xop     FOR op.
DEF BUFFER cop     FOR op.
DEF BUFFER cop-en  FOR op-entry.
DEF BUFFER bkau-en FOR kau-entry.

DEF STREAM fout.

DEF VAR lastmess AS CHAR NO-UNDO.
DEF VAR iall     AS INT64  NO-UNDO.
DEF VAR iok      AS INT64  NO-UNDO.
DEF VAR ierr     AS INT64  NO-UNDO.
DEF VAR ierr_prc_cntrl AS INT64  NO-UNDO.

DEFINE VARIABLE vLinkOpStat   LIKE op.op-status NO-UNDO. /* ����� ��� �易���� ���㬥�⮢ */

DEF VAR ch       AS CHAR NO-UNDO.
DEF VAR rightass AS CHAR NO-UNDO.
DEF VAR rightann AS CHAR NO-UNDO.
DEF VAR rightchg AS CHAR NO-UNDO.
DEF VAR vOpNum   AS INT64  NO-UNDO.
DEF VAR vOpTran  AS INT64  NO-UNDO.
DEF VAR vOpKind  AS CHAR  NO-UNDO.
DEF VAR vStr     AS CHAR NO-UNDO.
DEF VAR vCateg   AS CHAR NO-UNDO.
DEF VAR mAvtStrSmnStatus AS CHARACTER NO-UNDO.

DEFINE VARIABLE mOrigStatus LIKE op.op-status NO-UNDO.

DEFINE BUFFER xop-tmprecid FOR op-tmprecid.
DEFINE BUFFER xlock-op  FOR op.
DEFINE BUFFER bOp       FOR op.

DEFINE QUERY qOp FOR op-tmprecid, bOp.

/*
   ����砥� � chst(op.i ��९஢��� ����� ���㬥�� �����।�⢥���
   ��। ����������, �⮡� �������� ���� ��஭��� ���������
*/
&GLOBAL-DEFINE recheck-op-status YES

/* �஢�ન � ����� ������ ����� */
ASSIGN 
   rightass = getThisUserXAttrValue('�������')
   rightann = getThisUserXAttrValue('����ိ��')
   rightchg = getThisUserXAttrValue('����ሧ�')
.

IF FGetSetting("��������ᯑ��","","") = "��" THEN mCorrChk = "".

tt1:
DO ON ERROR UNDO,  RETURN
   ON ENDKEY UNDO, RETURN :
  {chstopa.bup}
END.


{setdest.i &cols=168 &stream="stream fout"}

/* ����⢥��� ᬥ�� ����� � ���㬥�⮢ */

/* ���樠������ ����� ��⮪���஢����. */
RUN Init-SysMes IN h_tmess ("����℮�", "", "").

mAvtStrSmnStatus = GetXAttrValueEX( "_user",USERID( "bisquit" ),"�����•�","<���>" ).

IF mAvtStrSmnStatus <> "<���>" THEN 
RUN PresetAnswers IN h_tmess (mAvtStrSmnStatus).

FIND FIRST code WHERE
           code.class =  "�����" AND
           code.code =  vstatus NO-LOCK NO-ERROR.
{status.set &1=tt-status &2=code}
FIND FIRST tt-status WHERE
           tt-status.class =  "�����" AND
           tt-status.code =  vstatus NO-LOCK NO-ERROR.

RUN rid-rest.p (OUTPUT TABLE op-tmprecid).
{empty tmprecid}
{empty cl-tmprecid}

RUN CreateHardLinkClosure(INPUT TABLE op-tmprecid, OUTPUT TABLE cl-tmprecid).
vDiffStatuses = NO.
FOR FIRST cl-tmprecid NO-LOCK,
    FIRST op WHERE RECID(op) = cl-tmprecid.id NO-LOCK:
    ASSIGN
        vTmpStatus    = op.op-status
        vDiffStatuses = YES.
    .
END.
IF vDiffStatuses THEN DO:
    vDiffStatuses = NO.
    FOR EACH cl-tmprecid NO-LOCK,
        FIRST op WHERE
            RECID(op)    =  cl-tmprecid.id AND
            op.op-status <> vTmpStatus
        NO-LOCK:
        RUN Fill-SysMes IN h_tmess ("",
                                    "",
                                    "0",
                                    "���㬥���, ᮧ����� �࠭���樥� �" +
                                    STRING(op.op-transaction) +
                                    ", ��室���� � ࠧ��� ������. " +
                                    "����஭��� ᬥ�� ����� ����������").
        vDiffStatuses = YES.
        LEAVE.
    END.
END.

IF NOT vDiffStatuses THEN DO:
    {empty op-tmprecid}
    FOR EACH cl-tmprecid NO-LOCK:
        CREATE op-tmprecid.
        op-tmprecid.id = cl-tmprecid.id.
    END.
END.

_chst_tran:
DO TRANSACTION ON ERROR  UNDO _chst_tran, LEAVE _chst_tran
               ON ENDKEY UNDO _chst_tran, LEAVE _chst_tran:

   FOR EACH xop-tmprecid:
      ASSIGN
         lastmess = ""
         iall     = iall + 1
      .
      FIND FIRST xlock-op WHERE RECID(xlock-op) = xop-tmprecid.id NO-LOCK NO-ERROR.
      IF NOT AVAIL xlock-op THEN DO:
         vStr = "������ 㤠���� ��㣨� ���짮��⥫��".
         RUN putlog(vStr).
      END.
   END.

   OPEN QUERY qOp FOR EACH op-tmprecid,
        FIRST bOp WHERE RECID(bOp) =  op-tmprecid.id
           EXCLUSIVE-LOCK.
   GET FIRST qOp NO-WAIT.

   DO WHILE AVAIL(op-tmprecid):
      IF LOCKED(bOp) THEN DO: 
         vStr = "������ ।�������� ��㣨� ���짮��⥫��".
         WhoLocks2(RECID(bop), (BUFFER bOp:TABLE) , INPUT-OUTPUT vStr).
         RUN putlog(vStr).
         DELETE op-tmprecid.
      END.
      GET NEXT qOp NO-WAIT.
   END.  

   GET FIRST qOp.
   tt:
      DO WHILE AVAILABLE op-tmprecid
      ON ERROR  UNDO tt, LEAVE tt
      ON ENDKEY UNDO tt, LEAVE tt:

      IF RETRY THEN DO:
         GET NEXT qOp.
         NEXT tt.
      END.

      lastmess = "".

      RUN CheckOpRight IN h_base(op-tmprecid.id,?,"ChgSts") NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         FIND FIRST op WHERE
              RECID(op) =  op-tmprecid.id NO-LOCK NO-ERROR.
         RUN putlog(RETURN-VALUE).      
         UNDO tt, RETRY tt.
      END.

      FIND FIRST op WHERE RECID(op) = op-tmprecid.id NO-LOCK NO-ERROR.
      
      IF NOT IsOsnLinkDoc(op.op) THEN DO:
         FIND FIRST op-kind WHERE op-kind.op-kind = op.op-kind NO-LOCK NO-ERROR.
         IF AVAIL op-kind AND GetXAttrValueEx("op-kind", op-kind.op-kind, "���섮�", "���") = "��" THEN 
         DO:
            vStr = "� ���㬥�⮬ " + 
                   (IF {assigned op.doc-num} THEN op.doc-num
                                             ELSE STRING(op.op)) +
                   " �易� �᭮���� ���㬥��. �஢���� �����⢫���� ⮫쪮 �१ �᭮���� ���㬥��!".
            RUN Fill-SysMes IN h_tmess ("",
                                        "",
                                        "0",
                                        vStr).
            RUN PutLog(vStr).
            UNDO tt, RETRY tt.
         END.
      END.
      
      IF GetStatusSessions(INT64(GetXAttrValueEx("op",STRING(op.op),"Dpr-Id",?))) =  "�������" THEN
      DO:
         RUN PutLog("� �����⮩ ᬥ�� ������ ������ �� ��������!").
         UNDO tt, RETRY tt.
      END.

      mDprIdList = "".
      IF (FGetSetting("��⍨�", "", "���") >  vStatus) THEN
      DO:
         FOR EACH bkau-en OF op
            WHERE CAN-DO("��������*,����,�������",bkau-en.kau-id)
         NO-LOCK:
            mI = IF bkau-en.kau-id BEGINS "��������" THEN 1 ELSE 2.
            mKauDprId = STRING(INT64(ENTRY(mI,bkau-en.kau))) NO-ERROR.
            IF (mKauDprId >  '0') AND 
               (NOT CAN-DO(mDprIdList, mKauDprId))
            THEN
               {additem.i mDprIdList mKauDprId}
         END.
         DO mI = 1 TO NUM-ENTRIES(mDprIdList):
            IF GetStatusSessions(INT64(ENTRY(mI,mDprIdList))) <> "�������" THEN
            DO:
               RUN PutLog("������ ������ �������� ⮫쪮 �� ����⮩~nᬥ�� �����, ��।��襣� ���⪨!").
               UNDO tt, RETRY tt.
            END.
         END.
      END.

      /*------------------------------------------------------------------------------*/
      IF vStatus = "�" AND NOT CAN-DO(rightann,op.op-status) THEN
      DO:
         RUN PutLog("� ��� ��� �ࠢ ࠡ���� � ���㬥�⠬� ⠪��� �����.").
         UNDO tt, RETRY tt.
      END.

      IF vStatus           = "�"                 AND
         op.user-inspector = USERID( "bisquit" ) AND
         GetXAttrValue( "_user",USERID( "bisquit" ),"���㫄��" ) = "���" THEN
      DO:
         RUN PutLog("�� �� ����� ���㫨஢��� ���㬥���, " +
                    "����� �� ����஫����.").
         UNDO tt, RETRY tt.
      END.
      /*------------------------------------------------------------------------------*/

      FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
      
/* ��⠢�� ���� ���� */
      DEFINE VARIABLE mMaxSum      AS DECIMAL    NO-UNDO.
      DEFINE VARIABLE mMaskAcct    AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE mResident    AS LOGICAL    NO-UNDO.
      ASSIGN
         mMaxSum   = DECIMAL(GetCode("GT100","MaxSum"))
         mMaskAcct = GetCode("GT100","MaskAcct").
      IF AVAIL(op) 
         AND AVAIL(op-entry) 
         AND vstatus GT "���" 
         AND vstatus GT op.op-status THEN
      DO:
         {find-act.i
		      &acct = op-entry.acct-db
		      &curr = op-entry.currency
		   }   
         
         mResident = IsResident(acct.cust-cat,acct.cust-id).
         
         
         IF AVAIL(acct) 
            AND acct.contract EQ "�����"
            AND acct.cust-cat EQ "�"
            AND mResident     EQ YES
            AND op-entry.amt-rub GT mMaxSum
            AND CAN-DO(mMaskAcct,op-entry.acct-cr) THEN
         DO: 
            IF USERID("bisquit") EQ "O0400MGK" 
            OR USERID("bisquit") EQ "TMN_VAA"
            OR USERID("bisquit") EQ "OKO_AMR"
            OR USERID("bisquit") EQ "K0400MVS"
            OR USERID("bisquit") EQ "OKO_REV"
            OR USERID("bisquit") EQ "I0400STS" THEN
            DO:
               pick-value = "1". 
               RUN messmenu.p(10 ,
                  "[ ��������� ]",
                  "�㬬� �ॢ�蠥� " + TRIM(STRING(mMaxSum,">>>,>>>,>>>,>>9.99")) + ".~n" +
                  "���室��� ᮮ���� � �㦡� ��!",
                  "�⬥����,�த������").
               IF pick-value EQ "1" THEN
               DO:
                  UNDO tt, RETRY tt.
               END.
            END.
            ELSE            
            DO:
               RUN PutLog("��ࠡ�⪠ �⬥����, �㬬� �ॢ�蠥� " + TRIM(STRING(mMaxSum,">>>,>>>,>>>,>>9.99")) + "!").
               UNDO tt, RETRY tt.
            END.
         END.         
      END.
/* ����� ��⠢�� ���� ���� */
      IF (NOT CAN-DO(tt-status.doc-type,op.doc-type) AND
                      tt-status.doc-type  <> ""     )
      OR (NOT CAN-DO(tt-status.op-kind,op.op-kind  ) AND
                      tt-status.op-kind   <> ""     )
      OR ((IF AVAIL op-entry THEN NOT CAN-DO(tt-status.type,op-entry.type)
                              ELSE TRUE)  AND tt-status.type <> "") THEN DO:
         RUN putlog("��᪨ ����, �࠭���樨 ��� ��. ���⥦� ����� �� ᮮ⢥������ ����, �࠭���樨 ��� ��. ���⥦� ���.!").
         UNDO tt, RETRY tt.
      END.
      IF vstatus =  "�" AND
          OpIsSvod(RECID(op)) THEN
      DO:
          RUN putlog("���㬥�� �室�� � ��㯯� ᢮����. ��� ����� ���㫨஢���!").
          UNDO tt, RETRY tt.
      END.

      IF NOT allsum               AND
          in-op-status             AND
          close-date < cur-op-date THEN DO:

         FIND op-entry OF op NO-LOCK NO-ERROR.
         IF ambig op-entry THEN DO:
            RUN putlog("���������� ���㬥�� ����� ��ॢ����� ���筮!").
            UNDO tt, RETRY tt.
         END.
         IF AVAIL op-entry THEN
smf:
         DO ON ERROR UNDO, LEAVE smf ON ENDKEY UNDO , LEAVE smf WITH FRAME edt:
            summa  = IF op-entry.curr =  "" THEN op-entry.amt-rub ELSE op-entry.amt-cur.
            summa1 = summa.
            DISPLAY "�஢����� ��� �㬬�?"
                     summa1
               WITH FRAME edt CENTERED ROW 10 OVERLAY no-label.
            UPDATE  summa1
                     VALIDATE (summa1 <> 0 AND
                               (IF summa >  0 THEN summa1 <= summa AND summa1 > 0
                                              ELSE summa1 >= summa AND summa1 < 0),
                     "�㬬� ������ ���� ����� ��� ࠢ�� �㬬� �஢���� � �� ࠢ�� 0")
                WITH FRAME edt.
            HIDE FRAME edt.
         END.
      END.

      IF summa = summa1 THEN DO:
         FIND FIRST op WHERE recid(op) = op-tmprecid.id EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         {chst(op.i &proc_cntrl_undo = "DO: RUN putlog_1('�訡�� �஢����').     UNDO tt, RETRY tt. END."
                    &open-undo = "DO: IF RETURN-VALUE NE 'no_mess' THEN RUN putlog('�訡�� �஢����: ' + (IF RETURN-VALUE > '' THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1)) ). UNDO tt, RETRY tt. END. "
                    &kau-undo  = "DO: RUN putlog('�訡�� �㡠����⨪�'). UNDO tt, RETRY tt. END. "
                    &xattr-undo="run putlog('� ���㬥�� �� ��������� ��易⥫�� ���. ४������.'). undo tt, RETRY tt "
                    &undo      = "DO: RUN putlog('�訡�� ���㬥��').    UNDO tt, RETRY tt. END. "
                    &del-undo  = "UNDO tt, RETRY tt"
                    &visa      = yes
         }                                                                      
         RUN AnumStat.
      END.
      ELSE DO: /* ��饯�塞 */
         FIND cop WHERE recid(cop) =  recid(op) EXCLUSIVE-LOCK no-wait NO-ERROR.
         IF NOT AVAIL cop THEN DO:
            RUN putlog("������ ।�������� ��㣨� ��� 㤠����.").
            UNDO tt, RETRY tt.
         END.
         FIND FIRST cop-en OF cop EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF cop-en.curr =  "" THEN
         ASSIGN
            cop-en.amt-rub = summa - summa1
            summa = 0
         .
         ELSE DO:
            cop-en.amt-cur = summa - summa1.
            cop-en.amt-rub = CurToBase("�������",
                                       op-entry.currency,
                                       (IF op-entry.value-date <> ? THEN op-entry.value-date
                                                                    ELSE op-entry.op-date),
                                        cop-en.amt-cur).
            summa  = summa1.
            summa1 = CurToBase("�������",
                               op-entry.currency,
                               (IF op-entry.value-date <> ? THEN op-entry.value-date
                                                            ELSE op-entry.op-date),
                               summa ).
         END.
         RUN kauopdel IN h_kau (RECID (cop-en)).
         flager = 1.

         RUN SetSysConf IN h_base ("nokbs", "nokbs").
         RUN Check-Op-Entry IN h_op (BUFFER cop-en,cop-en.op-date,NO,"",mCorrChk,OUTPUT flager).
         RUN SetSysConf IN h_base ("nokbs", "").
         IF flager > 0 THEN DO:
            RUN putlog("�訡�� � op-enupd.p").
            UNDO tt, RETRY tt.
         END.
         IF flager >= 0 THEN DO:
            RUN SetSysConf IN h_base ("nokbs", "nokbs").
            RUN Kau-Trigger IN h_op (recid(cop-en),OUTPUT flager,YES).
            RUN SetSysConf IN h_base ("nokbs", "").
            IF flager <> 0 THEN DO:
               RUN putlog("�訡�� � Kau-Trigger, ��� �訡�� = " + string(flager)).
               UNDO tt, RETRY tt.
            END.
         END.

         CREATE op.
         {op(sess).cr &op-status=vstatus}
         ASSIGN
            op.op-status     = vstatus
            op.ben-acct      = cop.ben-acct
            op.branch-id     = cop.branch-id
            op.details       = cop.details
            op.doc-date      = cop.doc-date
            op.doc-kind      = cop.doc-kind
            op.doc-num       = cop.doc-num
            op.doc-type      = cop.doc-type
            op.misc[1]       = cop.misc[1]
            op.misc[2]       = cop.misc[2]
            op.inn           = cop.inn
            op.name-ben      = cop.name-ben
            op.op-kind       = cop.op-kind
            op.due-date      = cop.due-date
            op.op-value-date = cop.op-value-date
            op.due-date      = cop.due-date
            op.acct-cat      = cop.acct-cat
         .
         /* op-bank op-exp-imp */
         CREATE op-entry.
         buffer-copy cop-en TO op-entry ASSIGN
            op-en.op        = op.op
            op-en.op-date   = op.op-date
            op-en.user-id   = op.user-id
            op-en.op-status = op.op-status
            op-en.acct-cat  = op.acct-cat
            op-en.amt-cur   = summa
            op-en.amt-rub   = summa1
         .

         FIND FIRST code WHERE
              code.class =  "�����" AND
              code.code =  vstatus NO-LOCK NO-ERROR.
         IF code.misc[1]       =  ""   OR
            TRIM(code.misc[1]) =  "��" THEN DO:
            {op-entry.upd
               &open-undo = "DO: RUN putlog('�訡�� �஢����').     UNDO tt, RETRY tt. END. "
               &kau-undo  = "DO: RUN putlog('�訡�� �㡠����⨪�'). UNDO tt, RETRY tt. END. "
               &undo      = "DO: RUN putlog('�訡�� ���㬥��').    UNDO tt, RETRY tt. END. "
               &Ofnext    = "/*"
            }
         END.
      END.
      ASSIGN 
         vOpNum  = op.op
         vOpKind = op.op-kind
         vOpTran = op.op-transaction 
      .

      RELEASE op NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RUN putlog('�訡�� ���㬥��: ' + RETURN-VALUE). 
         UNDO tt, RETRY tt.
      END.

      FIND FIRST op-kind WHERE op-kind.op-kind = vOpKind NO-LOCK NO-ERROR.
      IF AVAIL op-kind AND GetXAttrValueEx("op-kind", op-kind.op-kind, "���섮�", "���") = "��" THEN 
      DO:
         /* ��।��塞, � ����� ����� ������ ���� �易��� ���㬥��� */
         vLinkOpStat = ''.
         vTmpStatus = FGetSetting("�����஢",?,""). 
         IF vStatus BEGINS '�' THEN vLinkOpStat = vStatus.
         ELSE IF vStatus >= vTmpStatus THEN vLinkOpStat = '��'.

         vTmpStatus = vStatus. /* ���������� ����� �᭮����� ���-� */
         vStatus = vLinkOpStat.
         FOR EACH cop WHERE cop.op-transaction = vOpTran NO-LOCK:
            IF IsOsnLinkDoc(cop.op) THEN NEXT.
            /* �᫨ �㦭� ��ॢ��� � ��ࢮ��砫�� ����� �⮣� ���-� */
            IF vLinkOpStat = '' THEN DO:  
               FIND FIRST history WHERE history.file-name = 'op' 
                                    AND history.field-ref = string(cop.op) 
                                    AND history.field-val MATCHES "*op-status*" 
                                    AND history.modify = '{&hi-w}' 
               NO-LOCK NO-ERROR.
               IF AVAIL history
                  THEN vStatus = ENTRY(LOOKUP("op-status",history.field-val) + 1,history.field-value).
                  ELSE vStatus = cop.op-status.  /* ����� ���㬥�� �� ������ */
            END.
            IF cop.op-status <> vStatus THEN DO:
               FIND FIRST op WHERE op.op = cop.op EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
               {chst(op.i &proc_cntrl_undo = "DO: RUN putlog_1('�訡�� �஢���� �易����� ���㬥��').     UNDO tt, RETRY tt. END."
                          &open-undo = "DO: RUN putlog('�訡�� �஢���� �易����� ���㬥��').     UNDO tt, RETRY tt. END. "
                          &kau-undo  = "DO: RUN putlog('�訡�� �㡠����⨪� �易����� ���㬥��'). UNDO tt, RETRY tt. END. "
                          &xattr-undo="run putlog('� ���㬥�� �� ��������� ��易⥫�� ���. ४������.'). undo tt, RETRY tt "
                          &undo      = "DO: RUN putlog('�訡�� ᬥ�� ����� �易����� ���㬥��').    UNDO tt, RETRY tt. END. "
                          &del-undo  = "UNDO tt, RETRY tt"
               }
               ELSE DO:
                  IF LOCKED(op) THEN DO:
                     vStr = "������ ।�������� ��㣨� ���짮��⥫��".           
                     WhoLocks2(RECID(cop), (BUFFER cop:TABLE) , INPUT-OUTPUT vStr).
                     RUN putlog(vStr).                                             
                  END.
                  ELSE RUN putlog("������ 㤠���� ��㣨� ���짮��⥫��.").                                             
                  NEXT.
               END.
            END.
         END.  /* FOR EACH cop */
         vStatus = vTmpStatus.
      END.

      /* � �� ०��� �� ����ன�� ᬥ��� ����� � �易���� ���㬥�⮢ */
      IF     shMode 
      AND fGetSetting("�������⌔���",?,"���") = '��' THEN
      /* �।���������� �� �離� ���䨫������ ���㬥�⮢ ��室���� � obj-transcation */
      FOR EACH obj-transaction WHERE
               obj-transaction.op-transaction =  vOpTran 
           AND obj-transaction.FILE-NAME      =  "op" 
           AND obj-transaction.surrogate      <> STRING(vOpNum) USE-INDEX op-transaction
      NO-LOCK:
         FIND FIRST op WHERE 
                    op.op = INT64(obj-transaction.surrogate) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF NOT AVAIL op THEN DO:
            IF LOCKED(op) THEN DO:
               FIND FIRST op WHERE 
                          op.op = INT64(obj-transaction.surrogate) NO-LOCK NO-ERROR.
               IF AVAIL op  THEN DO:
                  RUN putlog('�訡�� ᬥ�� ����� �易����� ���㬥��').    
                  UNDO tt, RETRY tt.
               END.
            END.
         END. 
         ELSE DO:
            IF NOT(op.op-status BEGINS "�") AND 
               op.acct-cat = vCateg THEN DO:
               {chst(op.i &proc_cntrl_undo = "DO: RUN putlog_1('�訡�� �஢���� �易����� ���㬥��').     UNDO tt, RETRY tt. END."
                       &open-undo = "DO: RUN putlog('�訡�� �஢���� �易����� ���㬥��').     UNDO tt, RETRY tt. END. "
                       &kau-undo  = "DO: RUN putlog('�訡�� �㡠����⨪� �易����� ���㬥��'). UNDO tt, RETRY tt. END. "
                       &xattr-undo="run putlog('� ���㬥�� �� ��������� ��易⥫�� ���. ४������.'). undo tt, RETRY tt "
                       &undo      = "DO: RUN putlog('�訡�� ᬥ�� ����� �易����� ���㬥��').    UNDO tt, RETRY tt. END. "
                       &del-undo  = "UNDO tt, RETRY tt"
               } 
               RUN AnumStat.
            END.
         END.
      END.
/* ��⠢�� ���� ����. ����� ��楤�� ����-��ࠡ�⪨ �� �ᯥ譮� ᬥ�� ����� */
      IF (bOp.op-status EQ vstatus)
      THEN RUN pb_poststatus.p (BUFFER bOp).
/* ����� ��⠢�� ���� ���� */
      iok = iok + 1.
      IF AVAIL op-tmprecid THEN
         DELETE op-tmprecid.
      RUN DeleteOldDataProtocol in h_base ("PrevSelectedDprId").
      GET NEXT qOp.
   END. 
END.
CLOSE QUERY qOp.
HIDE FRAME dateframe NO-PAUSE.
HIDE FRAME edt NO-PAUSE.



/* �뢮� ��⮪��� */
IF iall = 1 THEN DO:
   OUTPUT STREAM fout CLOSE.
   IF lastmess <> "" THEN
      MESSAGE lastmess VIEW-AS ALERT-BOX ERROR.
END.
ELSE DO:
   ierr = iall - iok.
   PUT STREAM fout UNFORMATTED
      SKIP(1) "        �����:" SKIP
      "���㬥�⮢ ��� ��ॢ���   :" iall SKIP
      "���� �����              :" vstatus " (" tt-status.name ")" SKIP
      "���㬥�⮢ ��ॢ�����     :" iok SKIP
      "�訡�� ��ॢ��� ���㬥�⮢:" ierr SKIP
      "�訡�� ��楤��� ����஫� :" ierr_prc_cntrl SKIP.
   {preview.i &stream="stream fout"}
END.

/* �����襭�� ����� ��⮪���஢����. */
RUN End-SysMes IN h_tmess.
RUN DeleteOldDataProtocol in h_base ("PrevSelectedDprId").

{intrface.del}          /* ���㧪� �����㬥����. */ 

/* ����७��� ��楤��� */

DEFINE FRAME ferr header "������ �������� ����������" WITH DOWN WIDTH 168.

PROCEDURE putlog.
  DEF INPUT PARAM in-mess AS CHAR NO-UNDO.
  lastmess = in-mess.
  DISPLAY STREAM fout
     iall LABEL "N" FORMAT ">>>9"
     op.doc-num WHEN AVAIL op FORMAT "x(12)"
     "id " + string(op-tmprecid.id) WHEN NOT AVAIL op @ op.doc-num
     op.op-status WHEN AVAIL op
     op.op-date WHEN AVAIL op
     op.user-id WHEN AVAIL op
     lastmess LABEL "������" FORMAT "x(120)"
     WITH FRAME ferr.
  DOWN STREAM fout 1 WITH FRAME ferr.
  ierr = ierr + 1.
END PROCEDURE.

PROCEDURE putlog_1.
  DEF INPUT PARAM in-mess AS CHAR NO-UNDO.
  lastmess = in-mess.
  DISPLAY STREAM fout
     iall LABEL "N" FORMAT ">>>9"
     op.doc-num WHEN AVAIL op FORMAT "x(12)"
     "id " + string(op-tmprecid.id) WHEN NOT AVAIL op @ op.doc-num
     op.op-status WHEN AVAIL op
     op.op-date WHEN AVAIL op
     op.user-id WHEN AVAIL op
     lastmess LABEL "������" FORMAT "x(120)"
     WITH FRAME ferr.
  DOWN STREAM fout 1 WITH FRAME ferr.
  ierr_prc_cntrl = ierr_prc_cntrl + 1.
END PROCEDURE.


PROCEDURE AnumStat:
   {anumstat.i}
END PROCEDURE.

/*
  ���᫥��� ���몠��� op-tmprecid �⭮�⥫쭮 ���⪮� �裡
  ����� ���㬥�⠬� � ���⮬ ���. ४����� �࠭���権 "���섮�"
*/
PROCEDURE CreateHardLinkClosure.
    DEFINE INPUT  PARAMETER TABLE FOR op-tmprecid.
    DEFINE OUTPUT PARAMETER TABLE FOR cl-tmprecid.

    DEFINE VARIABLE vOpList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vI      AS INT64     NO-UNDO.

    DEFINE BUFFER op  FOR op.
    DEFINE BUFFER cop FOR op.

    FOR EACH op-tmprecid NO-LOCK,
        FIRST op WHERE RECID(op) = op-tmprecid.id NO-LOCK:
        RUN GetLinkedOps IN h_op (op.op, YES, OUTPUT vOpList).
        vOpList = STRING(op.op) +
                  (IF {assigned vOpList} THEN ("," + vOpList) ELSE "").
        DO vI = 1 TO NUM-ENTRIES(vOpList):
            FIND FIRST cop WHERE
                cop.op = INT64(ENTRY(vI, vOpList))
            NO-LOCK NO-ERROR.
            IF AVAILABLE cop AND
               NOT CAN-FIND(FIRST cl-tmprecid WHERE cl-tmprecid.id = RECID(cop))
            THEN DO:
                CREATE cl-tmprecid.
                cl-tmprecid.id = RECID(cop).
            END.
        END.
    END.
END PROCEDURE.

/* ���������� RETURN-VALUE ��� �뢮�� �訡�� � ��⮪�� */
PROCEDURE SetReturnValue PRIVATE:
   DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.

   RETURN iStr.
END PROCEDURE.
/* $LINTFILE='chst-opa.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.316+03:00' */
/*prosignWsS760WksS8WhsJhOqW28Q*/