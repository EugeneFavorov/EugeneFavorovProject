/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: TW-OP.P
      Comment: �ਣ��� �� ������ � ⠡���� OP
   Parameters: ���
         Uses:
      Used by:
     Modified: 17.08.2004 15:50 KSV      (0033194) ��ॢ���� �� std-trig.i
     Modified: 19.09.2005 19:34 KSV      (0046989) ��⠢��� ������
                                         terrchck.i.
     Modified: 07.12.2005 kraw (0055178) op.filial-id
     Modified: 28/11/2007 kraw (0085391) ����饭� ᮧ����� ���㬥�� ����� op
     Modified: 28/05/2008 kraw (0093208) �᫨ ����� op, � ��⥬�� "op" + op.acct-cat     
     Modified: 0163393 miol
*/

TRIGGER PROCEDURE FOR WRITE OF op OLD BUFFER oldOp.

{globals.i}
{xsms-mq.i}

DEFINE NEW GLOBAL SHARED VARIABLE shfilial AS CHARACTER NO-UNDO.

DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKomDoc AS INT64   NO-UNDO.
DEFINE VARIABLE mDpr-Id AS INT64     NO-UNDO.
DEFINE VARIABLE mStatusAnnul AS CHARACTER    NO-UNDO.

DEF BUFFER bop    FOR op.
DEF BUFFER bsigns FOR signs.
DEF BUFFER xxcode FOR code.

DEFINE BUFFER class FOR class.
DEFINE BUFFER bop-entry FOR op-entry.
DEFINE BUFFER bkau-entry FOR kau-entry.

DO TRANSACTION:
   {statmodel.i op oldop}
   /* ���४�஢�� ����ᨬ�� ���㬥�⮢ ��,
   ** �᫨ ���������� �������� ���.
   ** ����ᨬ�� ���-� �� ����� ���� ��᪮�쪮 -
   ** ���⮬� ������ �࠭����� ����室���!
   */
   /* ��।���� ⠪ ��� �஫��� �� ��᪠� */
   IF op.op-value-date EQ ? THEN
      ASSIGN op.op-value-date = op.op-date .

   mStatusAnnul = FGetSetting("�����뀭��", ?, "").

   IF NOT {assigned op.filial-id} THEN
      op.filial-id = shfilial.

   IF op.class-code EQ "op" THEN
   DO:
      mStrTMP = "op" + op.acct-cat.
      FIND FIRST class WHERE class.class-code EQ mStrTMP NO-LOCK NO-ERROR.

      IF NOT AVAILABLE class THEN
         RETURN ERROR "����� ᮧ������ ��ꥪ�� ����� op".

      op.class-code = mStrTMP.
   END.

   IF Op.contract-date <> oldOp.contract-date THEN
   FOR /* ��ॡ�ࠥ� �� ���-�� ��, ������騥 �� �����񭭮�� ���㬥�� */
      EACH  bsigns WHERE
            bsigns.file-name  = "Op"
        AND bsigns.code       = "���஢����"
        AND bsigns.code-value BEGINS STRING(op.op) + ","
         NO-LOCK,

      FIRST bop    WHERE
            bop.op            = INT64(ENTRY(1, bsigns.surrogate))
        AND bop.op-status    <> "�1"
         NO-LOCK:

      RUN chst-op.p (RECID(bop), "�1") NO-ERROR.

      IF ERROR-STATUS:ERROR OR
         RETURN-VALUE <> "" THEN
      DO:
         MESSAGE
            " �� 㤠���� �������� ����� ����ᨬ��� ���㬥�� �� ��." SKIP
            RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN ERROR.
      END.
   END.
   IF CAN-DO(mStatusAnnul, op.op-STATUS) THEN 
   op.op-DATE = ?.
   FOR EACH op-entry OF op
      NO-LOCK:

      FIND FIRST bop-entry EXCLUSIVE-LOCK WHERE RECID(bop-entry) = RECID(op-entry) NO-WAIT NO-ERROR.

      IF LOCKED bop-entry THEN 
      DO:
         RUN wholocks2.p (RECID(op-entry), "op-entry", "������ � op-entry �������஢���").
         RETURN ERROR "������ � op-entry �������஢���". 
      END.

      ASSIGN
         bop-entry.op-status = op.op-status
         bop-entry.op-date = op.op-date
      .
   END.

   FOR EACH kau-entry OF op
      NO-LOCK:

      FIND FIRST bkau-entry EXCLUSIVE-LOCK WHERE RECID(bkau-entry) = RECID(kau-entry) NO-WAIT NO-ERROR.

      IF LOCKED bkau-entry THEN 
      DO:
         RUN wholocks2.p (RECID(kau-entry), "kau-entry", "������ � kau-entry �������஢���").
         RETURN ERROR "������ � kau-entry �������஢���". 
      END.

      ASSIGN
         bkau-entry.op-status = op.op-status
         bkau-entry.op-date = op.op-date
         bkau-entry.contract-date = op.contract-date
      .
   END.

   IF op.op-status <> oldop.op-status THEN DO:
      {intrface.get xclass}
      IF CAN-DO(GetXclassAllChildsEx("opb-pay"),op.class-code) THEN DO:
         FIND FIRST xxcode WHERE xxcode.class = "��⏫��"
                             AND CAN-DO(xxcode.misc[1],op.op-status)
         NO-LOCK NO-ERROR.
         IF  AVAIL xxcode
         AND {assigned xxcode.code} THEN DO:
            IF NOT UpdateSignsEx(op.class-code, 
                                 STRING(op.op),
                                 "������Ꮻ�⥦�",
                                 xxcode.code)
            THEN RETURN ERROR "�� 㤠���� ��⠭����� ���.४����� ������Ꮻ�⥦� � ���㬥�� �室�饣� � �������� opb-pay.".
         END.
         IF op.op-STATUS LE "�" THEN DO:

            mKomDoc = INT64(DYNAMIC-FUNCTION("GetXattrValue" IN h_base,
                                               "op",STRING(op.op),"��������㬥�⠊����ᨨ")) NO-ERROR.
            IF  mKomDoc > 0 THEN DO:
                FIND FIRST bOp WHERE bOp.op EQ mKomDoc EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF AVAIL(bOp) THEN 
                   bOp.op-status = op.op-status. 
            END.
         END. 
      END.
      {intrface.del xclass}
   END.

   IF NEW op THEN 
   DO:
      DEFINE VARIABLE vRight AS CHARACTER   NO-UNDO.
      vRight = getThisUserXAttrValue('����ᑮ��').
      IF vRight = "" THEN 
      DO:
&IF DEFINED( MANUAL-REMOTE ) &THEN
         {intrface.get tmess}
         RUN Fill-AlertSysMes("","","-1","�� �� ����� �ࠢ� ������ �����!").
         {intrface.del tmess}
&ELSE
         MESSAGE "�� �� ����� �ࠢ� ������ �����!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
&ENDIF
         RETURN ERROR "�� �� ����� �ࠢ� ������ �����!".
      END.
      IF NOT CAN-DO(vRight,op.op-status) THEN 
      DO:
&IF DEFINED( MANUAL-REMOTE ) &THEN
         {intrface.get tmess}
         RUN Fill-AlertSysMes("","","-1", 
            SUBSTITUTE( "��᢮���� ����� &1  ����������. � ��� ��� �ࠢ ᮧ������ ���㬥��� ⠪��� �����.", op.op-status ) 
         ).
         {intrface.del tmess}
&ELSE
         MESSAGE "��᢮���� ����� " + op.op-status + " ����������. � ��� ��� �ࠢ ࠡ���� � ���㬥�⠬� ⠪��� �����." VIEW-AS ALERT-BOX INFO BUTTONS OK.
&ENDIF
         RETURN ERROR "��᢮���� ����� " + op.op-status + " ����������. � ��� ��� �ࠢ ᮧ������ ���㬥��� ⠪��� �����.".
      END.
      {intrface.get xclass}
      DEFINE VARIABLE mCFOUser AS CHARACTER NO-UNDO.
      DEFINE VARIABLE mOfficeUser AS CHARACTER NO-UNDO.
      mCFOUser = GetXattrValueEx("_user",USERID('bisquit'),"cfo-user","").
      IF {assigned mCFOUser} THEN
         UpdateSigns("op",STRING(op.op),"cfo-oper",mCFOUser,isXAttrIndexed(op.class-code,"cfo-oper")).
      mOfficeUser = GetXattrValueEx("_user",USERID('bisquit'),"office","").
      IF {assigned mOfficeUser} THEN
         UpdateSigns("op",STRING(op.op),"office",mOfficeUser,isXAttrIndexed(op.class-code,"office")).
      {intrface.del xclass}
   END.

   IF op.branch-id EQ ? OR op.branch-id EQ "���" THEN
      op.branch-id = GetXAttrValueEx("_user", 
                       STRING(USERID("bisquit")), 
                       "�⤥�����", 
                       FGetSetting("������",?,"")).

   mDpr-Id = INT64(GetXattrValueEx("op",STRING(op.op),"dpr-id","0")) NO-ERROR.
   
   FIND FIRST sessions WHERE 
              sessions.dpr-id EQ mDpr-Id
      NO-LOCK NO-ERROR.
   IF     AVAIL sessions 
      AND sessions.op-date NE op.op-date 
      AND op.op-date NE ? THEN
      RETURN ERROR "����. ���� ���㬥�� �� ����� �⫨����� �� ����. ��� ᬥ�� � ���஬� �� �ਢ易�".
                      
   RUN XSMSNotify-Op(BUFFER oldOp, MTIME).
   RUN XSMSNotify-Op(BUFFER op   , MTIME).

   /*****************************************************************
   * ������,�믮����騩 �⠭������ ��ࠡ��� �ਣ��� �������� *
   * ������ ��ப� ������ ��⠢����� ��᫥���� � �⮬ 䠩��!       *
   ****************************************************************/
   {std-trig.i op oldop} 
END.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='05/10/2015 18:34:15.690+04:00' */
/* $LINTUSER='sados' */
/* $LINTMODE='1' */
/* $LINTFILE='tw-op.p' */
/*prosignfqZgUaXMa5Os64iK/EHMng*/
