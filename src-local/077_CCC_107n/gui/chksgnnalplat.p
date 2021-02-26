/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: chksgnnalplat.p
      Comment: ��楤�� �஢�ન ४����⮢ ���⥫�騪� � �����⥫� ��� ���������, ⠬�������
               � ����� ���⥦��
   Parameters: iOp - ���㬥��, oOk - १����
         Uses:
      Used by:
      Created: 
     Modified:
*/
{globals.i}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get tmess}
{intrface.get op}
 DEFINE INPUT  PARAMETER iOp AS INT64   NO-UNDO.
 DEFINE OUTPUT PARAMETER oOk AS LOGICAL NO-UNDO.

{148n.i}

 DEFINE VARIABLE mBalNalog AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mBalSmev  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mKRDNP    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mINNSend  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mINNRec   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mUIN      AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mPokST    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mPokND    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mKPPSend  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mKPPRec   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mErrTxt   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mAcctSend AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mNameSend AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mAcctRec  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mNameRec  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mNalContrVal AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE mBalNalogEx AS CHARACTER   NO-UNDO.

 DEFINE BUFFER bOp      FOR op.
 DEFINE BUFFER bOpEntry FOR op-entry .

  FIND FIRST bop WHERE 
             bop.op = iOp 
  NO-LOCK NO-ERROR.
  IF NOT AVAIL bop THEN 
     RETURN.

  FIND FIRST bOpEntry WHERE bOpEntry.op = bop.op NO-LOCK NO-ERROR.
  IF NOT AVAIL bOpEntry THEN 
     RETURN.

  ASSIGN
   mBalNalog = FGetSetting("���","bal-nalog","")
   mBalSmev  = FGetSetting("���","bal-smev","")
   mKRDNP    = FGetSetting("���","�����","")
   mBalNalogEx = FGetSetting("���","bal-nalog-ex ","")
  .
  
/*MESSAGE "chksgnnalplat"*/
/*VIEW-AS ALERT-BOX.     */
  
/* �஢�ઠ ��� ���⥫�騪� */
FUNCTION ChkINNSend RETURN CHARACTER PRIVATE (INPUT iINNSend AS CHAR,
                                              INPUT iAcctDb  AS CHAR,
                                              INPUT iOpOp    AS INT64):
 DEFINE VARIABLE vAcctSend AS CHARACTER NO-UNDO.
 DEFINE VARIABLE vNameSend AS CHARACTER NO-UNDO.
 DEFINE BUFFER bacct      FOR acct.
 DEFINE BUFFER buf-op     FOR op.

  FIND FIRST buf-op WHERE 
             buf-op.op = iOpOp
  NO-LOCK NO-ERROR.

 {find-act.i
     &bact   = bacct
     &acct   = iAcctDb
 }
 IF AVAILABLE bacct THEN DO:

     IF NOT {assigned iINNSend} AND
        LOOKUP(bacct.cust-cat,"�,�,�") > 0 THEN
        iINNSend = getValueAttr(getCustClass(bacct.cust-cat),
                                STRING(bacct.cust-id),
                                "inn").
     IF NOT {assigned iINNSend} THEN DO:
       ASSIGN
        vAcctSend = GetXAttrValueEx("op",STRING(buf-op.op),"acct-send","")
        vNameSend = GetXAttrValueEx("op",STRING(buf-op.op),"name-send","").

        IF {assigned vAcctSend} AND
           {assigned vNameSend} THEN
            UpdateSigns("op",
                        STRING(buf-op.op),
                        "inn-send",
                        "0",
                        isXAttrIndexed(buf-op.class-code, "inn-send")).
        ELSE
          RETURN "��� ���⥫�騪� ������ ���� ��������".
     END.

   /* �஢�ન */
   IF (LENGTH(iINNSend) = 10 OR
       LENGTH(iINNSend) = 12) AND
      SUBSTR(iINNSend,1,2) = "00" THEN
     RETURN SUBSTITUTE("���� ��� ���� ��� ���⥫�騪� '&1' �� ������ ���� ࠢ�� 00",iINNSend).

   /* �� - ��१����� */
   IF bacct.cust-cat = "�" AND
      getValueAttr(getCustClass(bacct.cust-cat),
                   STRING(bacct.cust-id),
                   "country-id") <> FGetSetting("������", ?, "RUS") 
      AND REPLACE(iINNSend,"0","") = ""
      AND iINNSend <> "0"  THEN
     RETURN SUBSTITUTE("�� ���� ��� ���⥫�騪� '&1' �� ����� ���� ��ﬨ",iINNSend).
   END.
END FUNCTION.

/* �஢�ઠ ��� ���⥫�騪� */
FUNCTION ChkKPPSend RETURN CHARACTER PRIVATE (INPUT iKPPSend AS CHAR):
   IF  iKPPSend <> "0" AND
       (LENGTH(iKPPSend) <> 9 OR
       SUBSTR(iKPPSend,1,2) = "00") THEN
     RETURN SUBSTITUTE("������ �ଠ� ४����� ��� ���⥫�騪� '&1'",iKPPSend).
END FUNCTION.

/* �஢�ઠ ��� �����⥫� */
FUNCTION ChkINNRec RETURN CHARACTER PRIVATE (INPUT iINNRec AS CHAR):
   IF LENGTH(iINNRec) <> 10 OR
      SUBSTR(iINNRec,1,2) = "00" THEN
     RETURN SUBSTITUTE("������ �ଠ� ४����� ��� �����⥫� '&1'",iINNRec).
END FUNCTION.

/* �஢�ઠ ��� �����⥫� */
FUNCTION ChkKPPRec RETURN CHARACTER PRIVATE (INPUT iKPPRec AS CHAR):
   IF LENGTH(iKPPRec)     <> 9 OR
      SUBSTR(iKPPRec,1,2) = "00"
   THEN
     RETURN SUBSTITUTE("������ �ଠ� ४����� ��� �����⥫� '&1'",iKPPRec).
END FUNCTION.

/* �������� ���� �����*/
FUNCTION ValidatePokSt RETURN LOGICAL PRIVATE (INPUT iPokSt AS CHARACTER, 
                                               INPUT iOpRowid AS ROWID):

   DEFINE VARIABLE vRet  AS LOGICAL INIT YES NO-UNDO.
   DEFINE VARIABLE vDateMCI96 AS DATE NO-UNDO.
   DEFINE VARIABLE vDateDoc   AS DATE NO-UNDO.
   DEFINE VARIABLE vBal401 AS CHARACTER NO-UNDO.

   DEFINE BUFFER bop        FOR op.
   DEFINE BUFFER bop-bank   FOR op-bank.
   DEFINE BUFFER banks-code FOR banks-code.
   DEFINE BUFFER banks      FOR banks.
   DEFINE BUFFER      bcode FOR       code.

   IF {assigned iPokSt} AND CAN-DO("14",iPokSt) THEN 
   DO: 
      RUN Fill-SysMes IN h_tmess("", "", -1, "����� 14 �᪫�祭 �� 107�." ).
      vRet = NO.
      RETURN vRet.
   END.
   
/*   IF iPokSt EQ "" THEN*/
/*   DO:                 */
/*      vRet = NO.       */
/*      RETURN vRet.     */
/*   END.                */
   
   IF {assigned iPokSt} AND NOT CAN-DO('0,00',iPokSt) THEN RETURN vRet.
   vDateMCI96 = DATE(FGetSetting("���", "��⠌��96","")) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN vRet.
   
   FIND FIRST bop WHERE ROWID(bop) = iOpRowid NO-LOCK NO-ERROR.
   IF NOT AVAIL bop THEN RETURN vRet.

/*   vDateDoc = DATE(GetXAttrValueEx("op",STRING(bop.op),"��⠏����",STRING(bop.doc-date))) NO-ERROR.*/
/*   IF ERROR-STATUS:ERROR OR vDateDoc < vDateMCI96 THEN RETURN vRet.                                */

   vBal401 = FGetSetting("���","bal-401","").
   IF NOT {assigned bop.ben-acct} OR NOT CAN-DO(vBal401,bop.ben-acct) THEN RETURN vRet.
   FOR FIRST bop-bank WHERE bop-bank.op = bop.op AND 
                            bop-bank.bank-code-type = "���-9" AND
                            bop-bank.op-bank-type = "" NO-LOCK,
       FIRST banks-code WHERE banks-code.bank-code-type = "���-9"
                          AND banks-code.bank-code = bop-bank.bank-code
                       NO-LOCK,
       FIRST banks WHERE banks.bank-id = banks-code.bank-id NO-LOCK,
       FIRST bcode  WHERE bcode.class = "PZN-���"
                     AND bcode.code  = banks.bank-type 
                   NO-LOCK:
       IF NOT {assigned iPokSt} OR CAN-DO('0,00',iPokSt) THEN
          vRet = NO.
   END.
   
   RETURN vRet.

END FUNCTION.

/*Begin*/
IF (CAN-DO(mBalNalog,bop.ben-acct) OR
    CAN-DO(mBalSmev,bop.ben-acct)) AND
    CAN-DO(mKRDNP,bop.doc-type) THEN 
DO:
   ASSIGN
      mUIN     = GetXattrValueEx("op",STRING(iOp), "���",     "0")
      mPOKST   = GetXattrValueEx("op",STRING(iOp), "�����",   "")
      mPOKND   = GetXattrValueEx("op",STRING(iOp), "�����",   "")
      mKPPSend = GetXAttrValueEx("op",STRING(bop.op),"kpp-send","")
      mKPPRec  = GetXAttrValueEx("op",STRING(bop.op),"kpp-rec","")
      mAcctSend = GetXAttrValueEx("op",STRING(bop.op),"acct-send","")
      mNameSend = GetXAttrValueEx("op",STRING(bop.op),"name-send","")
      mAcctRec  = GetXAttrValueEx("op",STRING(bop.op),"acct-rec","")
      mNameRec  = GetXAttrValueEx("op",STRING(bop.op),"name-rec","").

/*   MESSAGE mPOKST ";" mBalNalogEx ";" bop.ben-acct ";" bOpEntry.acct-cr*/
/*   VIEW-AS ALERT-BOX.                                                  */

   IF /*(
      CAN-DO(mBalNalogEx,bop.ben-acct) OR
      CAN-DO(mBalNalogEx,bOpEntry.acct-cr)
      )
      AND*/ 
      (mPokST = "0"  OR
       mPokST = "00" OR
       mPokST = "")
   THEN 
   DO:
      RUN Fill-SysMes IN h_tmess ("","","-1",
         "���� (101) ������ ���� ���������!").
         oOk = YES.
      RETURN.
   END.
   
   /*
   �᫨ ����� (101) �� ��������� ��� ᮤ�ন� "0",
   ���� �� �⪫�祭�� ����஫� ���।�⢮� �� ���.��������148,
   ������� �஢�ન �� �ந��������
   */
   
   mNalContrVal = FGetSetting("���", "��������96","���").
   IF LOOKUP(mNalContrVal,"��,��") > 0 THEN
   DO:
     IF NOT ValidatePokSt(mPokST,ROWID(bop))
     THEN DO:
       RUN Fill-SysMes IN h_tmess ("","",IF mNalContrVal = "��" THEN "-1"
                                                                 ELSE "4",
       "���� (101) ������ ���� ���������!").
      IF   pick-value = "yes" 
         OR pick-value = "2" 
         OR pick-value = ? THEN
         oOk = YES.
      END.
   END.
   
   /*MESSAGE mNalContrVal ";" mPokST ";" ValidatePokSt(mPokST,ROWID(bop))*/
   /*VIEW-AS ALERT-BOX.                                                  */
   
   /*
      �᫨ ����� (101) �� ��������� ��� ᮤ�ন� "0",
      ���� �� �⪫�祭�� ����஫� ���।�⢮� �� ���.��������148,
      ������� �஢�ન �� �ந��������
   */
   
   IF NOT isAssigned148n(mPokST) OR
      FGetSetting("���", "��������148", "��") = "���"
   THEN DO:
      {intrface.del}
      RETURN.
   END.
   
   IF {assigned mAcctSend} AND {assigned mNameSend} THEN
        mINNSend = GetXAttrValueEx("op",STRING(bop.op),"inn-send","").
     ELSE mINNSend = bop.inn.

     IF {assigned mAcctRec} AND {assigned mNameRec} THEN
        mINNRec = GetXAttrValueEx("op",STRING(bop.op),"inn-rec","").
     ELSE mINNRec = bop.inn.

     /* �஢�ઠ ��� ���⥫�騪� */
     mErrTxt = ChkINNSend (INPUT mINNSend,
                           INPUT bOpEntry.acct-db,
                           INPUT bOp.op).
     IF {assigned mErrTxt} THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1",mErrTxt).
        oOk = YES.
     END.
     /* �஢�ઠ ��� ���⥫�騪� */
     mErrTxt = ChkKPPSend (mKPPSend).
     IF {assigned mErrTxt} THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1",mErrTxt).
        oOk = YES.
     END.
     /* �஢�ઠ ��� �����⥫� */
     mErrTxt = ChkINNRec (mINNRec).
     IF {assigned mErrTxt} THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1",mErrTxt).
        oOk = YES.
     END.
     /* �஢�ઠ ��� �����⥫� */
     mErrTxt = ChkKPPRec (mKPPRec).
     IF {assigned mErrTxt} THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1",mErrTxt).
        oOk = YES.
     END.

     /* �᫨ ���� (101) 㤮���⢮��� ᯨ�� �� �����108_22, ���� (22) ��� � ���� (108) ���������
     ���祭��� 0, ������ ���� �������� ��� (12 ࠧ�冷�) */
     mNalContrVal = GetCode("���:�����","108-22").
     IF   LOOKUP(mNalContrVal,"��,��") > 0
      AND CAN-DO(fGetSetting("���","�����108_22",""),mPokST)
      AND mUIN = "0" AND mPokND = "0" AND 
     (NOT {assigned mInnSend} OR LENGTH(mInnSend) <> 12) 
     THEN DO:
        RUN Fill-SysMes IN h_tmess ("","",IF mNalContrVal = "��" THEN "-1"
                                                                 ELSE "4",
        "�� ���������� ���� (101) ���祭��� " + mPOKST + 
        ", ����� (22) ��� � (108) ���祭��� ���� ��� ���⥫�騪� ������ ���� �������� (12 ࠧ�冷�)!").
        IF   pick-value = "yes" 
          OR pick-value = "2" 
          OR pick-value = ? THEN
         oOk = YES.
     END.   
END.
  

{intrface.del}
/* $LINTFILE='chksgnnalplat.p' */
/* $LINTMODE='1,3,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='osov' */
/* $LINTDATE='16/02/2017 10:08:47.132+03:00' */
/*prosigntFf7YmC4Hm246HvNZ86mHg*/