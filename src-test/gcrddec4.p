/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    copyright: (c) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     filename: gcrddec3.p
      comment: ���ᠭ�� ���㬥�⮢ � ����⥪�
      comment:
   parameters:
         uses:
      used by:
      created: 27/07/2002 kostik
     modified:
  last change:
*/

{globals.i}
{tmprecid.def}
{g-defs.i}
{pick-val.i}
{sh-defs.i new}
{def-wf.i new}
{defframe.i new}
{g-docnum.def}    /* ��� �奬 ��⮭㬥�樨. */ 


DEFINE INPUT  PARAMETER in-op-date    AS DATE      NO-UNDO. /*��� ���� ��� */
DEFINE INPUT  PARAMETER ipOpKindRecId AS RECID     NO-UNDO. /*�࠭����� */

DEFINE VARIABLE vMethodTempl      AS   CHARACTER       NO-UNDO. /*����� �㭪権 ��� ��ࠡ�⪨
                                                                 � ����஫� ���㬥��.
                                                                 ��� ४������: �믄� �믏�᫥*/
DEFINE VARIABLE vMethodParamTempl AS   CHARACTER      NO-UNDO. /*��ࠬ���� ��� �㭪樨 ��ࠡ�⪨
                                                                 � ����஫� ���㬥��.
                                                                 ��� ४������: �믄� �믏�᫥*/

DEFINE VARIABLE vFlagErrlog  AS   LOGICAL              NO-UNDO.  /*���� �訡�� ��� ��ࠡ�⪨
                                                                  ࠧ����� ���権*/
DEFINE VARIABLE vMessChar    AS   CHARACTER            NO-UNDO. /*����饭�� �� �訡���*/
DEFINE VARIABLE vLogFlag     AS   LOGICAL              NO-UNDO.
DEFINE VARIABLE fler         AS   LOGICAL              NO-UNDO.  /*��� ������� details.def*/

DEFINE VARIABLE in-cont-code AS   CHARACTER            NO-UNDO. /*��� g-acctv1.i*/
DEFINE VARIABLE dval         LIKE op-entry.value-date  NO-UNDO. /*��� �����஢���� (asswop.i)*/
DEFINE VARIABLE vDetailsChar AS   CHARACTER            NO-UNDO. /*��� ��ࠡ�⪨ ���� "��⠫� ���⥦�"*/
DEFINE VARIABLE vAutolog     AS   LOGICAL              NO-UNDO. /*���� ��⮬��*/

DEFINE VARIABLE CopyXattrStr AS   CHARACTER            NO-UNDO. /*��� ����஢���� ��� ४����⮢ */
/* ��⠢�� ���� ���� */
DEFINE VARIABLE cCommision   AS   CHARACTER    NO-UNDO INIT "". /* ���᮪ op.op ��� ���᫥��� �����ᨩ */
/* ����� ��⠢�� ���� ���� */

DEFINE BUFFER xwop           FOR wop.                      /*��� g-acctv1.i*/

vAutoLog = NO.
/*�஢�ઠ �ࠢ �� �࠭�����*/
{gcrddec3.i}               /*��楤��� ��ࠡ�⪨ ����⥪*/
{details.def}
{kautools.lib}
{tmprecid.def &NGSH=YES &PREF="old_"}
{tmprecid.def &NGSH=YES &PREF="kau_"}
/*�롮� ���㬥�� ��� ᯨᠭ��
  �������� "��楤�� ��뢠���� �� ᯨᠭ��":

  ���᮪ ��楤�� ࠧࠡ�⠭��� ࠭��:
      ��ᬮ����� - ��楤�� ��� �롮� ���㬥�� �� ����.
                    �室��� ��ࠬ��� ����� ���.
                         - ��� ��।������ ��� ����� �ᯮ�짮����
                           ��楤��� �롮� ���: ��ᬮ�����[shwacct1()]
                         - �ய���� ����� ��� �: ��ᬮ�����[<���>,<�����>]
      --------------------------------------------------------------------------
      showacct1  -  ��楤��� ��� �롮� ���.
      showacct2     �室���  ��ࠬ��� �������� ����⥪�. (�������� ������� ���)
      showacct3     ��室��� ��ࠬ��� ���.
      acct(crd
      --------------------------------------------------------------------------
      showdoc1   - ��ᬮ�� ���㬥�⮢ �� ����⥪�.
                   �室���  ��ࠬ��� �������� ����⥪�. (�������� ������� ���)
                   ��室��� ��ࠬ��� ���㬥�� ��� ᯨᠭ��
                   ���ࠡ��뢠���� ��� ᯨᠭ�� � ��ࢮ� ����⥪�.
      --------------------------------------------------------------------------

  ��࠭�� ����� ��������� � tmprecid.
*/
DEFINE VARIABLE mKauRID AS RECID NO-UNDO.
FIND FIRST op-kind WHERE RECID(op-kind) EQ ipOpKindRecId NO-LOCK.

RUN rid-keep.p(INPUT TABLE old_tmprecid).

FOR EACH tmprecid :
   DELETE tmprecid.
END.

FIND FIRST op-templ OF op-kind NO-LOCK NO-ERROR.
if avail op-templ then RUN setsysconf IN h_base ("��������",op-templ.currency).

RUN SelectKau(BUFFER op-kind,OUTPUT mKauRid).

FIND FIRST kau  WHERE RECID(kau) EQ mKauRid
                           NO-LOCK NO-ERROR.
IF AVAIL kau AND NOT CAN-FIND(FIRST tmprecid WHERE tmprecid.id = RECID(kau))
THEN DO:
   CREATE tmprecid.
   tmprecid.id = RECID(kau).
END. 

RUN rid-rest.p(OUTPUT TABLE kau_tmprecid).

/* **************************************************************************** */
/*����஢���� ࠡ��� ��楤��� � ��⮬���᪮� ०��� */
{gcrddec3.log &DEFINELOG=YES}
/* ***************************************************** */
{g-currv1.i &ofbase="/*"} /*��ࠡ�⪠ ������ ���㬥��*/
P_KAU:
FOR EACH kau_tmprecid NO-LOCK TRANSACTION:  /*���� �� ⠡��� KAU*/
   RUN MessTool("","HEAD",vAutoLog,OUTPUT vLogFlag).
   FOR EACH wop:
      DELETE wop.
   END.
   GEN_DOC:
   FOR EACH op-template OF op-kind NO-LOCK:
      CopyXattrStr = GetXAttrValueEx("op-template",
                                     STRING(op-templ.op-kind) + "," +
                                     STRING(op-templ.op-templ),
                                     "����������","").
      RUN SetSysConf in h_base ("����2�������",CopyXattrStr).
      RUN MessTool("������ N" + STRING(op-template.op-template,">9"),"",vAutoLog,OUTPUT vLogFlag).
      /*�������� ���㬥�� */
      cur-op-date = in-op-date.
      CREATE op.
      {op(sess).cr}
      {g-op.ass}
      CREATE op-entry.
      {g-en.ass}

      ASSIGN
         op-entry.value-date = in-op-date
         op-entry.currency   = IF op-templ.currency <> ? THEN GetCurr(op-templ.currency)
                                                         ELSE op-entry.currency
      .
      CREATE wop.
      ASSIGN
         wop.currency = op-entry.currency
         dval         = op-entry.value-date
         wop.op-templ = op-templ.op-templ
         wop.op-kind  = op-kind.op-kind
         tcur         = op-entry.currency
         wop.op-recid = RECID(op)
      .

      {asswop.i}

      /* ����砥� ���祭�� ���� �奬� ��⮭㬥�樨. */
      docnum-tmpl = GetXattrValue ("op-template",
                                    op-templ.op-kind + "," + STRING (op-templ.op-templ),
                                   "DocCounter").
      IF docnum-tmpl EQ "" THEN
         docnum-tmpl = GetXattrValue ("op-template",
                                       op-templ.op-kind + "," + STRING (op-templ.op-templ),
                                      "��������").
      IF docnum-tmpl NE "" THEN
      DO:
         DocNum-OP = STRING (RECID (Op)).
         op.doc-num = STRING(GetCounterNextValue(docnum-tmpl, gend-date)).
         IF op.doc-num EQ ?
             THEN RUN Fill-SysMes IN h_tmess ("", "", "0","���������� ������� ���祭�� ���稪�.").
      END.

      /********************���㬥�� ᮧ���*************************************/
      vMethodTempl = GetXAttrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "�믄�",
                                     "").
      vMethodParamTempl = "".
      IF INDEX(vMethodTempl,"((") NE 0 THEN
      ASSIGN
         vMethodParamTempl = SUBSTRING(vMethodTempl,INDEX(vMethodTempl,"((") + 2)
         vMethodTempl      = SUBSTRING(vMethodTempl,1,INDEX(vMethodTempl,"((") - 1)
      .
      IF vMethodTempl NE "" AND vMethodTempl NE ? THEN DO: /*��楤�� ��ࠡ�⪨ 蠡����*/

         IF LOOKUP(vMethodTempl, this-procedure:internal-entries) = 0 THEN DO:
            RUN MessTool("�� ������ ��⮤ ��ࠡ�⪨ 蠡����: " + vMethodTempl,
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU, NEXT P_KAU.
         END.

         RUN VALUE(vMethodTempl) (kau_tmprecid.id,vMethodParamTempl,OUTPUT vFlagErrlog,OUTPUT vMessChar).
         IF vFlagErrlog THEN DO:
            RUN MessTool(vMessChar,
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU, NEXT P_KAU.
         END.
         ASSIGN
            wop.currency = op-entry.currency
            tcur         = op-entry.currency
         .
      END.
      {g-acctv1.i &OFbase    = YES
                  &vacct     = op-entry.acct
                  &nodisplay = YES
                  &OFsrch    = YES
      }

      ASSIGN
         wop.acct-db = op-entry.acct-db
         wop.acct-cr = op-entry.acct-cr
      .
      IF (op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "") OR
         (op-templ.prep-amt-natcur <> ? AND op-templ.prep-amt-natcur <> "") THEN DO:
         RUN parssen.p (RECID(wop), in-op-date, OUTPUT vFlagErrlog).
         IF vFlagErrlog THEN DO:
            RUN MessTool("�訡�� �� ���� �㬬�",
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU,NEXT P_KAU.
         END.
         IF wop.amt-rub = 0 THEN 
              UNDO GEN_DOC,  NEXT GEN_DOC. 

         ASSIGN
            op-entry.amt-rub = wop.amt-rub
            op-entry.amt-cur = IF wop.currency NE "" AND wop.currency NE ? THEN
                               wop.amt-cur
                               ELSE
                               0
         .
      END.

      RUN parssign.p (in-op-date,
                      "op-template",
                      op-kind.op-kind + "," + string(op-templ.op-templ),
                      op-templ.class-code,
                      "op",
                      STRING(op.op),
                      op.class-code,
                      RECID(wop)).

      IF NOT vAutoLog THEN DO:
         ASSIGN
            op.op-status       = "�"
            op-entry.op-status = "�"
         .
         RUN "edit(op).p" (in-op-date,
                           RECID(op),
                           RECID(op-entry),
                           RECID(op-kind),
                           op-template.op-template,
                           NO
                          ).
         IF RETURN-VALUE EQ "ERROR,UNDO" THEN UNDO P_KAU, NEXT P_KAU.
         ASSIGN
            op.op-status       = op-template.op-status
            op-entry.op-status = op-template.op-status
         .
      END.
      ELSE DO:
         vDetailsChar = op-template.details.
         RUN ProcessDetails (RECID(wop), INPUT-OUTPUT vDetailsChar).
         op.details = vDetailsChar.
      END.

      /*��楤�� ����஫� ��������� ���祭��*/
      vMethodTempl = GetXAttrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "�믏�᫥",
                                     "").
      vMethodParamTempl = "".
      IF INDEX(vMethodTempl,"((") NE 0 THEN
      ASSIGN
         vMethodParamTempl = SUBSTRING(vMethodTempl,INDEX(vMethodTempl,"((") + 2)
         vMethodTempl      = SUBSTRING(vMethodTempl,1,INDEX(vMethodTempl,"((") - 1)
      .

      IF vMethodTempl NE "" AND vMethodTempl NE ? THEN DO:
         IF LOOKUP(vMethodTempl, this-procedure:internal-entries) = 0 THEN DO:
            RUN MessTool("�� ������ ��⮤ ��ࠡ�⪨ 蠡����: " + vMethodTempl,
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU, NEXT P_KAU.
         END.

         RUN VALUE(vMethodTempl)(kau_tmprecid.id,
                                 vMethodParamTempl,
                                 OUTPUT vFlagErrlog,
                                 OUTPUT vMessChar,
                                 BUFFER op-entry).
         IF vFlagErrLog THEN UNDO GEN_DOC, RETRY GEN_DOC.
      END.

      IF op.doc-type EQ "016" THEN RUN inipoxtr.p (RECID(op),?). /*+ ���.४�*/

      /* ************************************** */
      FIND FIRST kau WHERE RECID(kau) EQ kau_tmprecid.id NO-LOCK NO-ERROR.
      IF op-entry.acct-db EQ kau.acct OR op-entry.acct-cr EQ kau.acct THEN
      RUN setKauDocSysConf(op.op-kind,RECID(op-entry),kau_tmprecid.id).
      {op-entry.upd &871=yes}
      RUN EditXattr.

/* ��⠢�� ���� ���� */
      IF (op-template.op-template NE 1)
      THEN cCommision = cCommision + (IF (cCommision EQ "") THEN "" ELSE ",") + STRING(op.op).
/* ����� ��⠢�� ���� ���� */
   END.
END.

RUN rid-rest.p(OUTPUT TABLE old_tmprecid).
/* {g-print1.i} */

/* ��⠢�� ���� ���� */
/* RUN SetSysconf IN h_base("����⠑����⥪�",cCommision). */
{topkind.def}
DEFINE VARIABLE cTranz      AS CHARACTER NO-UNDO INIT "sbk_comm".
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER NO-UNDO.

{empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
ASSIGN
    lOk =   TDAddParam("__oplst", cCommision)
    NO-ERROR.
IF NOT lOk
THEN MESSAGE "�������� �訡�� �� ��।�� ��ࠬ��஢ � �࠭����� " + cTranz
             ".~n�����ᨨ �� ���᫥��."
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
ELSE DO:
    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).

    IF NOT lOk
    THEN MESSAGE cErrMsg + "~n�����ᨨ �� ���᫥��."
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.
/* ����� ��⠢�� ���� ���� */

{gcrddec3.log &SHOWLOG=YES}
{intrface.del}          /* ���㧪� �����㬥����. */ 



