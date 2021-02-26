/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: BUYBOOK1.I
      Comment: ����� ����� ���㯮� � �த��
   Parameters: ���
         Uses:
      Used by:
      Created: 23.01.2005 15:55 gorm     (39683)
     Modified: 09.02.2006 13:34 gorm     (56070) ���������� ��室�� ��ࠬ���� � SFAmtServs
                                         - ⥯��� �� �� ����砥� �㬬� ��� ���
     Modified: 09.02.2006 13:44 gorm     
     Modified: 07.12.2007 17:33 koch     <comment>
*/
   {intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
   ASSIGN
      Txt[9] = ""
      Txt[10] = ""
      Txt[11] = loan.cont-code
      Txt[12] = loan.branch-id
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
      txt[9]  = IF loan.loan-status EQ "���㫨�" THEN "���㫨�"
                                                  ELSE "".
   ASSIGN
      mNds       = "18.00"
      mNum       = ?
      mCntryName = ""
      mNumGTD    = ""
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

   FIND FIRST term-obl WHERE 
              term-obl.contract  = loan.contract
          AND term-obl.cont-code = loan.cont-code 
          AND term-obl.idnt      = 1
   NO-LOCK NO-ERROR.
   IF AVAIL term-obl THEN
   DO:
      mNds = STRING(term-obl.rate).

      /* �⠢�� ��� �� ��㣥, 㪠������ � ���-䠪��� */
      CASE term-obl.rate:
         WHEN 18.00 THEN mNum = 1.
         WHEN 10.00 THEN mNum = 3.
         WHEN 20.00 THEN mNum = 7.       

         /*�㫥��� �⠢�� ��� ��� �� ���������� ���*/
         WHEN 0.00 THEN
         DO:
             /* ��।������ - �� ���������� ��� ��� ��� = 0, 
             ** �஢��塞 �������� ����� �⠢�� ��� �� ��㣥, 
             ** �᫨ ��� - � ��� �� ���������� */
            RUN AssetNDS(loan.filial-id, term-obl.symbol,loan.open-date,OUTPUT mNds).
         
            mNum = IF mNds = "" THEN 9 ELSE 5.
         END.
         /*�᫨ �� ���室�� �� ���� ��業⭠� �⠢��, � ����� �� ��ࠦ���*/
         OTHERWISE      
         &IF DEFINED(RETURN1) &THEN
            {&RETURN1}.
         &ELSE
            RETURN.
         &ENDIF
              
      END CASE.

   END.

   FOR EACH term-obl WHERE 
            term-obl.contract  = loan.contract
        AND term-obl.cont-code = loan.cont-code 
        AND term-obl.idnt      = 1
   NO-LOCK:

      /* ��।������ ��࠭� */
      mCntryName = TRIM(mCntryName + "," + 
                        SFAssetCountry(term-obl.contract + "," 
                                + term-obl.cont-code        + ","
                                + STRING(term-obl.idnt)     + ","
                                + STRING(term-obl.end-date) + ","
                                + STRING(term-obl.nn), 
                                  term-obl.symbol),',').

      /* ��।������ ����� ��� */
      mNumGTD = TRIM(mNumGTD + "," + 
                     GetXattrValueEx ("term-obl",
                                 term-obl.contract + "," 
                                 + term-obl.cont-code + "," 
                                 + STRING(term-obl.idnt) + "," 
                                 + STRING(term-obl.end-date) + "," 
                                 + STRING(term-obl.nn),
                                 "declare",
                                 ""),',').
   END.

   /* ��।������ �㬬� ���-䠪���� � �㬬� ��� 
   ** (��� ⮣�, �� �� ������, ���筠� �� ����� ��� ������) */
   RUN SFAmtServs(loan.contract,
                  loan.cont-code,
                  OUTPUT mAmountSF,
                  OUTPUT mNdsAmtSF,
                  OUTPUT mAmtNoNdsSF).

   /* 横� �� �ᥬ �ਢ易��� ���⥦�� */
   
   mSgPart = GetXattrVAlueEx("loan",
                             loan.contract + "," + loan.cont-code,
                             "��������",
                             "������").
   mSummPlat = GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "�㬬돫��"
                            ).
   IF     mSgPart   EQ "�����������" 
      AND mSummPlat NE ""
   THEN DO:
      Block-Summ:
      DO mI = 1 TO NUM-ENTRIES(mSummPlat):
         ASSIGN
            mDate = ?
            mDate = DATE(ENTRY(2,ENTRY(mI,mSummPlat),"|"))
            mSumm = 0
            mSumm = DEC(ENTRY(1,ENTRY(mI,mSummPlat),"|"))
         NO-ERROR.
         IF mDate EQ ? 
         THEN
            NEXT Block-Summ.
         IF (mOnBook EQ "���") THEN
         IF NOT DatePeriod(DataBlock.beg-date, DataBlock.end-date, STRING(mDate) + ",") THEN NEXT Block-Summ.
   
         RUN CrtDataLine (mDate,
                          "",
                          mSumm).
      END.
   END.
   ELSE   
   tdl:
   DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
      
      FIND FIRST op-entry WHERE 
                 op-entry.op       = INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
             AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI,mSurrOp,";")))
      NO-LOCK NO-ERROR.
      IF AVAIL op-entry THEN
      DO:
         IF (mOnBook EQ "���") THEN
         IF NOT DatePeriod(DataBlock.beg-date, DataBlock.end-date, STRING(op-entry.op-date) + ",") THEN NEXT tdl.
         mSimI = mSimI + 1.
         RUN CrtDataLine (op-entry.op-date,
                          ENTRY(mI,mSurrOp,";"),
                          op-entry.amt-rub).
         

      END.
   END.
   
&IF DEFINED(CrtDataLine) EQ 0
&THEN   
&GLOBAL-DEFINE CrtDataLine
PROCEDURE CrtDataLine:
   DEFINE INPUT  PARAMETER iDate   AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iOpSurr AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iSumm   AS DECIMAL     NO-UNDO.
   CREATE tDataLine.
   ASSIGN
      tDAtaLine.data-id       = DataBlock.data-id
      tDataLine.Sym1          = STRING(iDate)  /* ��� ॣ����樨 */
      tDataLine.Sym2          = STRING(iDate)  /* ��� ������ */
      tDataLine.Sym3          = loan.cont-code + STRING(mSimI)            /* ����� ���-䠪���� */
      tDataLine.Sym4          = iOpSurr     /* ���ண�� ���⥦� */
      Txt[8]                  = IF loan.doc-num <> ? 
                                THEN loan.doc-num 
                                ELSE ""
      Txt[6]                  = IF loan.conf-date <> ? 
                                THEN STRING(loan.conf-date)    /* ��� ���-䠪���� */
                                ELSE ""
  
      Txt[1]                  = mSalerName
      Txt[2]                  = mSalerInn             
      Txt[3]                  = mSalerKPP             
      Txt[4]                  = mCntryName
      Txt[5]                  = mNumGTD

      Txt[7]                  = IF mAmount < mAmountSF THEN "(���筠� �����)" ELSE ""

      tDataLine.Txt           = Txt[1]  + '~n' + Txt[2]  + '~n' + Txt[3]  + '~n' + Txt[4]  + '~n' 
                              + Txt[5]  + '~n' + Txt[6]  + '~n' + Txt[7]  + '~n' + Txt[8]  + '~n' 
                              + Txt[9]  + '~n' + Txt[10] + '~n' + Txt[11] + '~n' + Txt[12] + '~n' 
                              + Txt[13] 
   .

   IF mSF-OP-Links EQ "sf-op-nds" THEN
   ASSIGN
      mAmount                 = 0
      mNdsAmt                 = 0
      tDataLine.Val[mNum]     = ((iSumm / DEC(mNds)) * (100 + DEC(mNds)) - iSumm)
      tDataLine.Val[mNum + 1] = iSumm
      tDataLine.Val[11]       = (iSumm / DEC(mNds)) * (100 + DEC(mNds))
   .
   ELSE IF mSF-OP-Links EQ "sf-op-dr" THEN
   ASSIGN
      tDataLine.Val[mNum]     = iSumm
      tDataLine.Val[mNum + 1] = ROUND(iSumm * DEC(mNds) / 100, 2)
      tDataLine.Val[11]       = tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]
   .
   ELSE
   ASSIGN
      mAmount                 = iSumm /*�㬬� � ���*/
      mNdsAmt                 = IF mAmount = mAmountSF 
                                THEN mNdsAmtSF
                                ELSE ((mAmount * DEC(mNds)) / (100 + DEC(mNds)))
      tDataLine.Val[mNum]     = IF mAmount = mAmountSF 
                                THEN mAmtNoNdsSF
                                ELSE (tDataLine.Val[mNum] + (mAmount - mNdsAmt))
      tDataLine.Val[mNum + 1] = mNdsAmt                     

      tDataLine.Val[11]       = mAmount /* �㬬� ���-䠪���� */
   .
   IF loan.currency NE "" THEN
      ASSIGN
         tDataLine.Val[mNum]     = CurToBase("����",loan.currency,op-entry.op-date,tDataLine.Val[mNum])
         tDataLine.Val[mNum + 1] = CurToBase("����",loan.currency,op-entry.op-date,tDataLine.Val[mNum + 1])
         tDataLine.Val[11]       = CurToBase("����",loan.currency,op-entry.op-date,tDataLine.Val[11])
      .
   IF ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) LT (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)) THEN 
      tDataLine.Val[mNum] = TRUNC(tDataLine.Val[mNum], 2). 
   ELSE 
      IF ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) GT (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)) THEN 
         tDataLine.Val[mNum + 1] = TRUNC(tDataLine.Val[mNum + 1], 2) + ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) - (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)).

   IF GetCode("��������",loan.cont-type) EQ "�����"
   THEN
      ASSIGN
         tDataLine.Sym1              = ""
/*
         tDataLine.Val[1]            = 0
*/
         ENTRY(4,tDataLine.Txt,"~n") = "�����"
      .
END PROCEDURE.
&ENDIF
/* $LINTFILE='buybook1.i' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='boam' */
/* $LINTDATE='04/03/2016 11:58:44.229+04:00' */
/*prosignDbNWatlPH/pEJcV85MNobw*/