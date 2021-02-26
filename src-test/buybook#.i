/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2005 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: BUYBOOK#.I
      Comment: ����� ����� ���㯮� � �த��
   Parameters:
         Uses:
      Used by:
      Created: 23.01.2005 15:55 gorm     (39683)
     Modified: 09.02.2006 13:34 gorm     (56070) �㬬� ��� ��� ⥯��� �� ����塞, 
                                         � �࠭�� � ���� amount-of-payment
     Modified: 13.02.2006 15:30 gorm     
     Modified: 07.12.2007 15:33 koch     <comment>
*/

ASSIGN
   txt[9]  = ""
   txt[10] = ""
   txt[11] = loan.cont-code
   txt[12] = loan.branch-id 
   txt[13] = mDopList 
.

IF GetXattrValueEx("loan", loan.contract + "," 
                         + loan.cont-code, "��ࠢ","") NE ""  THEN
   ASSIGN
      txt[9]  = "��ࠢ"
      txt[10] = GetXattrValueEx("loan", loan.contract + ","
                                      + loan.cont-code,"��ࠢ","")
   .
   ELSE
      txt[9] = IF loan.loan-status EQ "���㫨�" THEN "���㫨�"
                                                ELSE "".

mSimI = mSimI + 1.
CREATE tDataLine.
ASSIGN
   tDAtaLine.data-id = DataBlock.data-id
   tDataLine.Sym1    = IF loan.open-date <> ? 
                       THEN STRING(loan.open-date) /* ��� ॣ����樨 */ 
                       ELSE ""
   tDataLine.Sym2    = IF loan.end-date <> ? 
                       THEN STRING(loan.end-date)  /* ��� ������ */
                       ELSE ""
   tDataLine.Sym3    = loan.cont-code + STRING(mSimI)
.

/* ��।������ ��ਡ�⮢ ����ࠣ��� */
RUN SFAttribs_Seller(loan.contract,
                     loan.cont-code,
                     loan.cust-cat,
                     loan.cust-id,
                     OUTPUT mSalerName,   /* ������������ ����ࠣ��� */
                     OUTPUT mSalerAddres, /* ���� ����ࠣ��� */
                     OUTPUT mSalerInn,    /* ��� ����ࠣ��� */
                     OUTPUT mSalerKPP).   /* ��� ����ࠣ��� */
ASSIGN
   Txt[6] = IF loan.conf-date <> ? 
            THEN STRING(loan.conf-date) /* ��� ���-䠪���� */
            ELSE ""
        
   Txt[1] = mSalerName
   Txt[2] = mSalerInn             
   Txt[3] = mSalerKPP      
   Txt[8] = IF loan.doc-num <> ? 
            THEN loan.doc-num 
            ELSE "" 
.

ASSIGN
   Txt[4] = ""
   Txt[5] = ""
   Txt[7] = ""
.

trm:
FOR EACH term-obl WHERE 
         term-obl.contract  = loan.contract
     AND term-obl.cont-code = loan.cont-code 
     AND term-obl.idnt      = 1
NO-LOCK:

   /* �⠢�� ��� �� ��㣥, 㪠������ � ���-䠪��� */
   CASE term-obl.rate:
      WHEN 18.00 THEN mNum = 1.
      WHEN 10.00 THEN mNum = 3.
      WHEN 20.00 THEN mNum = 7.       

      /*�㫥��� �⠢�� ��� ��� �� ���������� ���*/
      WHEN 0.00 THEN DO:

         /* ��।������ - �� ���������� ��� ��� ��� = 0, 
         ** �஢��塞 �������� ����� �⠢�� ��� �� ��㣥, 
         ** �᫨ ��� - � ��� �� ���������� */
         RUN AssetNDS(loan.filial-id, term-obl.symbol,loan.open-date,OUTPUT mNds).
               
         mNum = IF mNds = "" THEN 9 ELSE 5.
      END.
      /*�᫨ �� ���室�� �� ���� ��業⭠� �⠢��, � ����� �� ��ࠦ���*/
      OTHERWISE NEXT trm.       
   END CASE.

   /* ��।������ ��࠭� */
   mCntryName = SFAssetCountry(term-obl.contract         + "," 
                             + term-obl.cont-code        + ","
                             + STRING(term-obl.idnt)     + ","
                             + STRING(term-obl.end-date) + ","
                             + STRING(term-obl.nn), 
                               term-obl.symbol).
       
   /* ��।������ ����� ��� */
   mNumGTD = GetXattrValueEx ("term-obl",
                               term-obl.contract         + "," 
                             + term-obl.cont-code        + "," 
                             + STRING(term-obl.idnt)     + "," 
                             + STRING(term-obl.end-date) + "," 
                             + STRING(term-obl.nn),
                               "declare",
                               "").
  
   ASSIGN 
      tDataLine.Val[11]       = tDataLine.Val[11] + (IF loan.currency EQ "" THEN term-obl.amt-rub
                                                                            ELSE CurToBase("����",loan.currency,loan.open-date,term-obl.amt-rub))
      tDataLine.Val[mNum]     = tDataLine.Val[mNum] + (IF loan.currency EQ "" THEN term-obl.amount-of-payment
                                                                              ELSE CurToBase("����",loan.currency,loan.open-date,term-obl.amount-of-payment))
      tDataLine.Val[mNum + 1] = tDataLine.Val[mNum + 1] + (IF loan.currency EQ "" THEN term-obl.int-amt                                                   
                                                                                  ELSE CurToBase("����",loan.currency,loan.open-date,term-obl.int-amt))
   .

   IF {assigned mCntryName} THEN
      {additem.i Txt[4] mCntryName}.

   IF {assigned mNumGTD} THEN
      {additem.i Txt[5] mNumGTD}.

   IF ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) LT (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)) THEN
      tDataLine.Val[mNum] = TRUNC(tDataLine.Val[mNum], 2).
   ELSE 
      IF ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) GT (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)) THEN
         tDataLine.Val[mNum + 1] = TRUNC(tDataLine.Val[mNum + 1], 2) + ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) - (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)).

END.

IF NOT {assigned Txt[4]} THEN Txt[4] = '-'.
IF NOT {assigned Txt[5]} THEN Txt[5] = '-'.

ASSIGN
   Txt[1]  = REPLACE(Txt[1], "~n", " ")
   Txt[2]  = REPLACE(Txt[2], "~n", " ")
   Txt[3]  = REPLACE(Txt[3], "~n", " ")
   Txt[4]  = REPLACE(Txt[4], "~n", " ")
   Txt[5]  = REPLACE(Txt[5], "~n", " ")
   Txt[6]  = REPLACE(Txt[6], "~n", " ")
   Txt[7]  = REPLACE(Txt[7], "~n", " ")
   Txt[8]  = REPLACE(Txt[8], "~n", " ")
   Txt[9]  = REPLACE(Txt[9], "~n", " ")
   Txt[10] = REPLACE(Txt[10], "~n", " ")
   Txt[11] = REPLACE(Txt[11], "~n", " ")
   Txt[12] = REPLACE(Txt[12], "~n", " ")
   Txt[13] = REPLACE(Txt[13], "~n", " ")
.

tDataLine.Txt =  Txt[1]  + '~n' + Txt[2]  + '~n' + Txt[3]  + '~n' + 
                 Txt[4]  + '~n' + Txt[5]  + '~n' + Txt[6]  + '~n' + 
                 Txt[7]  + '~n' + Txt[8]  + '~n' + Txt[9]  + '~n' + 
                 Txt[10] + '~n' + Txt[11] + '~n' + Txt[12] + '~n' + Txt[13].


/* �஢��塞 �� �ࠢ��쭮��� �⠢�� */
IF tDataLine.Val[11] = 0 AND 
   CAN-FIND(FIRST term-obl  WHERE 
                      term-obl.contract  = loan.contract
                  AND term-obl.cont-code = loan.cont-code 
                  AND term-obl.idnt      = 1
                  AND term-obl.amt-rub   > 0) THEN DO:
   DELETE tDataLine.
   &IF DEFINED(RETURN1) &THEN
      {&RETURN1}.
   &ELSE
      RETURN.
   &ENDIF
         
END.
IF GetCode("��������",loan.cont-type) EQ "�����"
THEN
   ASSIGN
      tDataLine.Sym1              = ""
/*
      tDataLine.Val[1]            = 0
*/
      ENTRY(4,tDataLine.Txt,"~n") = "�����"
   .
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='25/09/2015 12:05:42.257+04:00' */
/* $LINTUSER='stre' */
/* $LINTMODE='1' */
/* $LINTFILE='buybook#.i' */
/*prosignf3FpZyxGr4zj7YKrVYX2Mg*/