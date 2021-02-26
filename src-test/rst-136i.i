   vVidOpNalV = GetXattrValue("op",STRING(op.op),"ВидОпНалВ").
   IF    NOT {assigned vVidOpNalV} 
         OR CAN-DO(vOperIskl,vVidOpNalV)
   THEN
      NEXT.
   vOpTime = INT64(GetXattrValueEx("op",STRING(op.op),"rst-time","?")) NO-ERROR.
   vOpDate = DATE(GetXattrValue("op",STRING(op.op),"rst-date")) NO-ERROR.
   IF vOpTime EQ ? OR vOpDate EQ ? THEN
   &IF DEFINED (leg407re) EQ 0 &THEN
      RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
   &ELSE 
   RUN GetDateTimeOpTr407re(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
   &ENDIF

   RUN GetCodeCurAndSumm(op.op,
                         vPrSumm[1],
                         OUTPUT vCurrDb, 
                         OUTPUT vSummaDb,
                         OUTPUT vCurrCr, 
                         OUTPUT vSummaCr,
                         OUTPUT vQtyCheq,
                         OUTPUT vCurrCheq,
                         OUTPUT vSumCheq,
                         OUTPUT vSumRub,
                         OUTPUT vAcctCli).

   RUN GetCountry(OUTPUT vCountry, OUTPUT vResident).

   IF    vSumRub >= mSumLimit2 
      OR (    vSumRub < mSumLimit2 
          AND NOT CAN-DO(vOpGr15,vVidOpNalV))
      OR vPechFIO 
      OR GetXattrValue("op",STRING(op.op),"КодНеобыч")      NE ""
      OR GetXattrValue("op",STRING(op.op),"ПодозДокумент")  NE ""
      OR GetXattrValueEx("op",STRING(op.op),"ПечФИО","Нет") NE "Нет"
      THEN
      DO:
      END.
      ELSE
         ASSIGN
            vCountry  = ""
         .

   IF    CAN-DO(vOperRate,vVidOpNalV) 
      OR vCurrDb EQ vCurrCr THEN
         vRate = 0.
   ELSE
   DO:
      vRate = DECIMAL(GetXAttrValue("op",STRING(op.op),"sprate")) NO-ERROR.
      IF vRate EQ 0 THEN
      DO:
         FIND FIRST bop-entry WHERE 
                    bop-entry.op       EQ op.op
                AND bop-entry.currency NE ""
            NO-LOCK NO-ERROR.
         IF AVAILABLE bop-entry THEN 
            vRate = FindRateSimple("Учетный",
                                   bop-entry.currency,
                                   op.op-date).
      END.
   END.
   ASSIGN
      vChrRate = IF (vRate EQ 1 OR vRate EQ 0) THEN vSpaceRate ELSE STRING(vRate,">9.9999")
      vDover   = GetXAttrValueEx("op",STRING(op.op),"Довер","") EQ "Да"
      .
   /*
   FOR EACH bop WHERE 
            bop.op-transaction EQ op.op-transaction
      NO-LOCK,
       EACH form-op WHERE 
            form-op.op EQ bop.op NO-LOCK:
      vForminfo = (IF vForminfo EQ "" THEN "" ELSE (vForminfo + ",")) + STRING(form-op.form-series,"x(12)") + "," + STRING(form-op.form-num-first,GetBlancFmt()).
      IF form-op.form-num-last NE 0 THEN
         vForminfo = vForminfo + ",- " + STRING(form-op.form-num-last,GetBlancFmt()).
   END.
   */
   CREATE ttRegister.
   ASSIGN
      ttRegister.op       = op.op
      ttRegister.doc-num  = op.doc-num
      ttRegister.doc-time = vOpTime
      ttRegister.doc-date = vOpDate
      ttRegister.doc-kind = vVidOpNalV
      ttRegister.rate     = vRate
      ttRegister.ChRate   = vChrRate
      ttRegister.curr-db  = vCurrDb
      ttRegister.amt-db   = vSummaDb 
      ttRegister.curr-cr  = vCurrCr  
      ttRegister.amt-cr   = vSummaCr 
      ttRegister.card     = CAN-DO(vOperCard,vVidOpNalV)
      ttRegister.cheq-qty = vQtyCheq  
      ttRegister.currcheq = vCurrCheq 
      ttRegister.amt-cheq = vSumCheq  
      ttRegister.acct     = vAcctCli WHEN CAN-DO(vOperAcct,vVidOpNalV)
      ttRegister.dover    = vDover
      ttRegister.country  = vCountry
      ttRegister.forminfo = vForminfo
      ttRegister.dpr-id   = INT64(vCurDprId)
      ttRegister.resident = vResident
      ttRegister.op-date  = op.op-date
    
      NO-ERROR.
 &IF DEFINED (leg407re) EQ 0 &THEN
      /* Комисии НДС и прочее */
   IF mLogComm THEN
   DO:
      FOR EACH bop WHERE 
               bop.op-transaction EQ op.op-transaction 
         NO-LOCK,         
          EACH op-entry OF bop WHERE 
                   CAN-DO(vPrSumm[1], SUBSTRING(op-entry.acct-cr,1,5))
           AND NOT CAN-DO(vAcctComm, op-entry.acct-cr)
         NO-LOCK,
         FIRST acct WHERE 
               acct.acct     EQ op-entry.acct-db
           AND acct.contract BEGINS "Касса"
         NO-LOCK:
   
         ASSIGN
            vCurrCom = IF op-entry.currency EQ "" THEN 
                          mCodOurCur
                       ELSE                             
                          op-entry.currency
            vSummCom = IF op-entry.currency EQ "" THEN
                          op-entry.amt-rub
                       ELSE
                          op-entry.amt-cur
            vSummComR = op-entry.amt-rub
            vCommSign = ENTRY(LOOKUP(SUBSTRING(op-entry.acct-cr,1,5),vPrSumm[1]),vPrSumm[2])
            .
         ASSIGN
            ttRegister.curr-com = vCurrCom
            ttRegister.amt-com  = ttRegister.amt-com  + vSummCom
         .
         FIND FIRST ttReg-Det WHERE
                    ttReg-Det.Op       EQ ttRegister.Op
                AND ttReg-Det.Currency EQ vCurrCom
                AND ttReg-Det.SummSign EQ vCommSign
            EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE ttReg-Det THEN
         DO:
            CREATE ttReg-Det.
            ASSIGN
               ttReg-Det.Op       = ttRegister.Op /* Номер операции */
               ttReg-Det.Currency = vCurrCom      /* Код валюты */
               ttReg-Det.SummSign = vCommSign
               ttReg-Det.Summa    = vSummCom      /* Сумма */
               ttReg-Det.SummaR   = vSummComR     /* Рублевый эквивалент */
               .
         END.
         ELSE
            ASSIGN                                                                                                                
               ttReg-Det.Summa    = ttReg-Det.Summa  + vSummCom  /* Сумма */
               ttReg-Det.SummaR   = ttReg-Det.SummaR + vSummComR /* Рублевый эквивалент */
               .
      END. /* FOR EACH bop */

      IF mCommCol THEN
      DO:

         FIND FIRST op-entry OF op NO-LOCK.

         FIND FIRST acct WHERE acct.acct     EQ op-entry.acct-db
                           AND acct.contract BEGINS "Касса"
            NO-LOCK NO-ERROR.

         IF        CAN-DO(vPrSumm[1], SUBSTRING(op-entry.acct-cr,1,5))
           AND NOT CAN-DO(vAcctComm, op-entry.acct-cr)
           AND AVAILABLE acct THEN
         DO:
            DELETE ttRegister.
         END.
      END.
   END.

      /* Итоги */
   IF mFullItg THEN
   DO:
      RUN CrItog(vChrRate).
      IF vChrRate NE vSpaceRate THEN
         RUN CrItog(vSpaceRate).
   END.
   ELSE
   DO:
      ASSIGN
         vChrRate = vSpaceRate WHEN NOT mByRate
         .
      RUN CrItog(vChrRate).
   END.
&ENDIF
/* $LINTFILE='rst-136i.i' */
/* $LINTMODE='1' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTUSER='stre' */
/* $LINTDATE='30/05/2016 12:36:59.174+03:00' */
/*prosign+0L2WPqsNh9FVgYv4OLyDg*/