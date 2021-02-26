/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: trnfrc_2.p
      Comment: ��楤�� ᯨᠭ�� ���㬥�⮢ � ����⥪� ���⁫�� �� ����⥪� 2.
   Parameters: tmprecid �� ��⠬
         Uses:
      Used by:
      Created: 17/01/2007 muta 0084518
     Modified: 25.03.2008 MUTA 0087581 �� ���끫��� ��७�ᥭ � ��㯯� ���⁫��.      
     Modified: 15.01.2009 kraw (0095716) �맮� �� trnfrc_3.p
     Modified: 23.12.2009 irbis ���뢠�� ��� 90902 � 墮�⮬, ⠪�� �� ��� � � ���⭮�� ��� ������. 
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{tmprecid.def}
{tmprecid.def &PREF=TMP_}
{topkind.def}
{intrface.get tmess}
{intrface.get blkob}
{intrface.get xclass}
{ch_cart.i}

DEFINE VARIABLE mBlTypes   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mErrMsg    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOK        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mRes       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mCrtAcct2  AS CHARACTER NO-UNDO.      /* ��� ����⥪� 2 */
DEFINE VARIABLE mAcctBl    AS CHARACTER NO-UNDO.
DEFINE VARIABLE h_kau      AS HANDLE    NO-UNDO.
DEFINE VARIABLE vRid       AS RECID     NO-UNDO.
DEFINE VARIABLE vOutRid    AS RECID     NO-UNDO.
DEFINE VARIABLE mAmt       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mBlockSumm AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mOrderPayH AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mOrderPayL AS CHARACTER  NO-UNDO.
DEF BUFFER bAcct FOR acct. /* ���������� ����. */
DEFINE STREAM sErr.

mBlTypes =  FGetSetting ("���⁫��", "���끫���", "").

{setdest.i &STREAM="STREAM sErr"}

PUT STREAM sErr UNFORMATTED "��७�� ���㬥�⮢ � ����⥪� ���⁫�� �� ����⥪� 2." SKIP.
PUT STREAM sErr UNFORMATTED "���: " STRING(gend-date) SKIP.

RUN fill-sysmes ("","","3","�믮����� ��७�� ���㬥�⮢ ��� ��࠭��� ��⮢?|��,���").

IF pick-value NE "1" THEN RETURN.

{empty TMP_tmprecid}

FOR EACH tmprecid:
   CREATE TMP_tmprecid.
   TMP_tmprecid.id = tmprecid.id.
END.

FOR EACH TMP_tmprecid NO-LOCK,
   FIRST acct WHERE RECID (acct) EQ TMP_tmprecid.id
         NO-LOCK:

   IF ChkBlkCrd(acct.acct + "," + acct.currency, DATETIME(gend-date), NO, NO) THEN
   DO:
      PUT STREAM sErr UNFORMATTED "��� " + acct.number
                                + " ����� �����஢�� ("
                                + BlockAcct(acct.acct + "," + acct.currency,
                                            DATETIME(gend-date))
                                + ") �� ����஥筮�� ��ࠬ��� ���끫��� ("
                                + mBlTypes + ")" SKIP.
      NEXT.    
   END. 

   ASSIGN
      mCrtAcct2  = ENTRY(1,GetXattrValue("acct", acct.acct + "," + acct.currency, "����2�����"),",")    
      mAcctBl    = ENTRY(1,GetXattrValue("acct", acct.acct + "," + acct.currency, "���⁂����"),",")
      .

   IF NOT {assigned mCrtAcct2} AND getsysconf("trnfrc_3") NE "YES" THEN
      tr:
      DO ON ENDKEY UNDO tr,RETURN
      ON ERROR  UNDO tr,RETURN:
 
         vRid = RECID(Acct).
         RUN fill-sysmes ("","","","�����ᮢ� ��� " + Acct.number + " �� �易� � ��⮬ ����⥪� 2.").

         RUN kauproc.p PERSISTENT SET h_kau.

/* IRBIS ������� ������ ������ ��������ᮢ��� ��� 90902 �� ��७�� ��� -> �2  */
/*         RUN Create_acct IN h_kau (vRid,
                                   "����2",
                                   gend-date,
                                   ?,
                                   OUTPUT vOutRid) . 
*/
   find first op-template where op-template.class-code eq "acct-templ" 
          and op-template.op-kind eq "981" no-lock no-error.

  
   RUN MakeAcctByCust IN h_kau (RECID (acct), "����2", gend-date, NO, (if avail op-template then recid(op-template) else ?), OUTPUT vOutRid) .
  

         DELETE PROCEDURE(h_kau) NO-ERROR.
         IF RETURN-VALUE EQ "ERROR" THEN UNDO tr, RETURN.
         
         FIND FIRST bacct WHERE RECID(bacct) EQ vOutRid NO-LOCK NO-ERROR.
         IF NOT AVAIL bacct THEN UNDO tr, RETURN.

         mCrtAcct2  = bAcct.acct.

   END.

   IF ({assigned mCrtAcct2} OR getsysconf("trnfrc_3") EQ "YES") AND {assigned mAcctBl} THEN DO:

      {empty tOpKindParams}     /* ������ ⠡���� ��ࠬ��஢ */
      mBlockSumm = GetBlockPosition(acct.acct,
                                    acct.currency,
                                    "5",
                                    gend-date).
      
      RUN GetCartOrdPay(INPUT Acct.acct,
                        INPUT Acct.currency,
                        INPUT "����⥪�2",
                        INPUT gend-date,
                        OUTPUT mOrderPayH,
                        OUTPUT mOrderPayL).

      ASSIGN
         mRes = TDAddParam ("iCrtAcct", mAcctBl) AND TDAddParam ("iCurrency", acct.currency)
         AND TDAddParam("iBlAcct",mCrtAcct2) AND TDAddParam("iRegimAuto","yes") AND
         TDAddParam("iOpKind","_CBLC2_2") AND TDAddParam("iAcct",acct.acct)
         AND TDAddParam("iBlockSumm", STRING(mBlockSumm))
         AND TDAddParam("iMaxOrderPay", STRING(mOrderPayH))
      NO-ERROR.
      
      IF NOT mRes THEN 
      DO:
         PUT STREAM sErr UNFORMATTED "��� " + acct.number +  ". �訡�� ��।�� ��ࠬ��஢ � �࠭����� TRCBLC2a." SKIP.
         NEXT.
      END.

      IF getsysconf("trnfrc_3") EQ "YES" THEN
      DO:
         mAmt = CalcFreeOstOnBlockAcct(acct.acct,
                                        acct.currency,
                                        gend-date,
                                        0).
          
         ASSIGN
            mRes = TDAddParam ("iAmt", STRING(mAmt)) NO-ERROR.
         
         IF NOT mRes THEN 
         DO:
            PUT STREAM sErr UNFORMATTED "��� " + acct.number +  ". �訡�� ��।�� ��ࠬ��஢ � �࠭����� TRCBLC2a." SKIP.
            NEXT.
         END.
      END.
      
      RUN ex-trans.p ("TRCBLC2a", gend-date, TABLE tOpKindParams, OUTPUT mOK, OUTPUT mErrMsg). 
   
      IF NOT mOK THEN
           PUT STREAM sErr UNFORMATTED  "��� " + acct.number + ". �訡��: " + mErrMsg  SKIP.

   END.

   ELSE PUT STREAM sErr UNFORMATTED "�� ��� " + acct.number +  " �� �������� ���� �� �������⥫��� ४����⮢ ����2����� � ���⁂����. " SKIP.

END.

PUT STREAM sErr UNFORMATTED SKIP(1) "��७�� ���㬥�⮢ �����襭."  SKIP.
{preview.i &STREAM="STREAM sErr"}

{intrface.del} 
/* $LINTUSER='BIS' */
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='14/10/2014 15:08:31.484+04:00' */
/* $LINTFILE='trnfrc_2.p' */
/*prosignElpMEsihbj6M1JNN56QaFw*/