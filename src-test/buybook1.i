/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: BUYBOOK1.I
      Comment: Расчет кники покупок и продаж
   Parameters: нет
         Uses:
      Used by:
      Created: 23.01.2005 15:55 gorm     (39683)
     Modified: 09.02.2006 13:34 gorm     (56070) Изменились выходные параметры у SFAmtServs
                                         - теперь мы еще получаем сумму без НДС
     Modified: 09.02.2006 13:44 gorm     
     Modified: 07.12.2007 17:33 koch     <comment>
*/
   {intrface.get xclass}   /* Библиотека инструментов метасхемы. */
   ASSIGN
      Txt[9] = ""
      Txt[10] = ""
      Txt[11] = loan.cont-code
      Txt[12] = loan.branch-id
      txt[13] = mDopList
   .

   IF GetXattrValueEx("loan", loan.contract + "," 
                            + loan.cont-code, "Исправ","") NE ""  THEN
      ASSIGN
         txt[9]  = "Исправ"
         txt[10] = GetXattrValueEx("loan", loan.contract + ","
                                         + loan.cont-code,"Исправ","")
      .
   ELSE  
      txt[9]  = IF loan.loan-status EQ "Аннулир" THEN "Аннулир"
                                                  ELSE "".
   ASSIGN
      mNds       = "18.00"
      mNum       = ?
      mCntryName = ""
      mNumGTD    = ""
   .

   /* Определение атрибутов контрагента */
   RUN SFAttribs_Seller(loan.contract,
                        loan.cont-code,
                        loan.cust-cat,
                        loan.cust-id,
                        OUTPUT mSalerName,   /* Наименование контрагента */
                        OUTPUT mSalerAddres, /* адрес контрагента */
                        OUTPUT mSalerInn,    /* ИНН контрагента */
                        OUTPUT mSalerKPP).   /* КПП контрагента */

   FIND FIRST term-obl WHERE 
              term-obl.contract  = loan.contract
          AND term-obl.cont-code = loan.cont-code 
          AND term-obl.idnt      = 1
   NO-LOCK NO-ERROR.
   IF AVAIL term-obl THEN
   DO:
      mNds = STRING(term-obl.rate).

      /* Ставка НДС по услуге, указанная в счет-фактуре */
      CASE term-obl.rate:
         WHEN 18.00 THEN mNum = 1.
         WHEN 10.00 THEN mNum = 3.
         WHEN 20.00 THEN mNum = 7.       

         /*нулевая ставка НДС или не облагается НДС*/
         WHEN 0.00 THEN
         DO:
             /* определение - не облагается НДС или НДС = 0, 
             ** проверяем заданали вообще ставка НДС на услуге, 
             ** если нет - то НДС не облагается */
            RUN AssetNDS(loan.filial-id, term-obl.symbol,loan.open-date,OUTPUT mNds).
         
            mNum = IF mNds = "" THEN 9 ELSE 5.
         END.
         /*если не подходит ни одна процентная ставка, то вообще не отражаем*/
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

      /* Определение страны */
      mCntryName = TRIM(mCntryName + "," + 
                        SFAssetCountry(term-obl.contract + "," 
                                + term-obl.cont-code        + ","
                                + STRING(term-obl.idnt)     + ","
                                + STRING(term-obl.end-date) + ","
                                + STRING(term-obl.nn), 
                                  term-obl.symbol),',').

      /* Определение номера ГТД */
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

   /* Определение суммы счет-фактуры и суммы НДС 
   ** (для того, что бы понять, частичная это оплата или полная) */
   RUN SFAmtServs(loan.contract,
                  loan.cont-code,
                  OUTPUT mAmountSF,
                  OUTPUT mNdsAmtSF,
                  OUTPUT mAmtNoNdsSF).

   /* цикл по всем привязанным платежам */
   
   mSgPart = GetXattrVAlueEx("loan",
                             loan.contract + "," + loan.cont-code,
                             "КнигаОтр",
                             "Полное").
   mSummPlat = GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "СуммыПлат"
                            ).
   IF     mSgPart   EQ "Разделенное" 
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
         IF (mOnBook EQ "Нет") THEN
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
         IF (mOnBook EQ "Нет") THEN
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
      tDataLine.Sym1          = STRING(iDate)  /* Дата регистрации */
      tDataLine.Sym2          = STRING(iDate)  /* Дата оплаты */
      tDataLine.Sym3          = loan.cont-code + STRING(mSimI)            /* Номер счет-фактуры */
      tDataLine.Sym4          = iOpSurr     /* Суррогат платежа */
      Txt[8]                  = IF loan.doc-num <> ? 
                                THEN loan.doc-num 
                                ELSE ""
      Txt[6]                  = IF loan.conf-date <> ? 
                                THEN STRING(loan.conf-date)    /* Дата счет-фактуры */
                                ELSE ""
  
      Txt[1]                  = mSalerName
      Txt[2]                  = mSalerInn             
      Txt[3]                  = mSalerKPP             
      Txt[4]                  = mCntryName
      Txt[5]                  = mNumGTD

      Txt[7]                  = IF mAmount < mAmountSF THEN "(частичная оплата)" ELSE ""

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
      mAmount                 = iSumm /*Сумма с НДС*/
      mNdsAmt                 = IF mAmount = mAmountSF 
                                THEN mNdsAmtSF
                                ELSE ((mAmount * DEC(mNds)) / (100 + DEC(mNds)))
      tDataLine.Val[mNum]     = IF mAmount = mAmountSF 
                                THEN mAmtNoNdsSF
                                ELSE (tDataLine.Val[mNum] + (mAmount - mNdsAmt))
      tDataLine.Val[mNum + 1] = mNdsAmt                     

      tDataLine.Val[11]       = mAmount /* Сумма счет-фактуры */
   .
   IF loan.currency NE "" THEN
      ASSIGN
         tDataLine.Val[mNum]     = CurToBase("Учетный",loan.currency,op-entry.op-date,tDataLine.Val[mNum])
         tDataLine.Val[mNum + 1] = CurToBase("Учетный",loan.currency,op-entry.op-date,tDataLine.Val[mNum + 1])
         tDataLine.Val[11]       = CurToBase("Учетный",loan.currency,op-entry.op-date,tDataLine.Val[11])
      .
   IF ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) LT (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)) THEN 
      tDataLine.Val[mNum] = TRUNC(tDataLine.Val[mNum], 2). 
   ELSE 
      IF ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) GT (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)) THEN 
         tDataLine.Val[mNum + 1] = TRUNC(tDataLine.Val[mNum + 1], 2) + ROUND((tDataLine.Val[mNum] + tDataLine.Val[mNum + 1]), 2) - (ROUND(tDataLine.Val[mNum], 2) + ROUND(tDataLine.Val[mNum + 1], 2)).

   IF GetCode("ТипОплат",loan.cont-type) EQ "Аванс"
   THEN
      ASSIGN
         tDataLine.Sym1              = ""
/*
         tDataLine.Val[1]            = 0
*/
         ENTRY(4,tDataLine.Txt,"~n") = "Аванс"
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