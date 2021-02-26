   {globals.i}
   {sh-defs.i new}
   {justasec}
   {intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
   {intrface.get date}

   DEFINE INPUT  PARAMETER bLoan   AS recid  NO-UNDO.
   DEFINE OUTPUT PARAMETER bError  AS CHARACTER  NO-UNDO.


   DEFINE VARIABLE mTxt       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSum1      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vSum2      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vDateTime  AS DATETIME   NO-UNDO.
   DEFINE BUFFER oacct       FOR acct.
   DEFINE BUFFER bAcct       FOR acct.
   DEFINE VARIABLE vOpdate    AS DATE       NO-UNDO.
   DEFINE VARIABLE bSurr      AS CHARACTER  NO-UNDO.



   FIND FIRST loan WHERE RECID(loan) EQ bLoan NO-LOCK NO-ERROR. 
   IF not AVAIL loan  THEN 
   DO:
      bError = "������� �� ������.".
   END.
   find last loan-acct where loan-acct.contract = loan.contract
                         and loan-acct.cont-code = loan.cont-code
                         and loan-acct.acct-type = "�������"
                         no-lock no-error.
   if not avail loan-acct then 
   do:
      bError = "���� ������� �� ������.".
   END.

   bError = "".
   {find-act.i
      &bact   = bAcct
      &acct   = loan-acct.acct
      &filial = shFilial
   }   
   
   IF NOT AVAILABLE bAcct THEN
   DO:
      bError = "�� ������ ���� � ⠡��� ��⮢".
   END.
   bSurr = loan-acct.acct + "," + loan-acct.curr.

   vOAcct = entry(1,GetXattrValueEx("acct",bSurr,"����2�����",?)).
   IF vOAcct NE ? THEN 
   FIND FIRST oacct WHERE oacct.acct EQ vOAcct NO-LOCK NO-ERROR. 
   IF AVAIL(oacct) THEN 
   DO:
      RUN acct-pos in h_base (oacct.acct, 
                              oacct.currency, 
                              vOpDate, 
                              vOpDate, 
                              gop-status).
      vSum1 = IF oacct.currency EQ ""
              THEN sh-bal
              ELSE sh-val.
      RELEASE oacct.
   END.

   bSurr = loan-acct.acct + "," + loan-acct.curr.
   vOAcct = entry(1,GetXattrValueEx("acct",bSurr,"���⁂����",?)).
   IF vOAcct NE ? THEN 
   FIND FIRST oacct WHERE oacct.acct EQ vOAcct NO-LOCK NO-ERROR. 
   IF AVAIL(oacct) THEN 
   DO:
      RUN acct-pos in h_base (oacct.acct, 
                              oacct.currency, 
                              vOpDate, 
                              vOpDate, 
                              gop-status).
      vSum2 = IF oacct.currency EQ ""
              THEN sh-bal
              ELSE sh-val.
      RELEASE oacct.
   END.
   IF {assigned mTxt} OR 
      vSum1 NE 0       OR 
      vSum2 NE 0       THEN
   DO:
      bError = " ������� " + loan.cont-code + " ���� " + loan-acct.acct + " �㬬� ����2����� = " + string(vSum1) +  " �㬬� ���⁂���� =  " + string(vSum2) .
   END.
   return.
