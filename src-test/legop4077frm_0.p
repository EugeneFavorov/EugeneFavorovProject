/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: legop4077frm_0.p
      Comment: класс данных LegOp
   Parameters: ipDataID;iNumFrame
         Uses:
      Used by:
      Created: 10.10.2016 09:48 ANBA     
     Modified: 10.10.2016 09:48 BIS      <comment>
*/

DEFINE INPUT PARAMETER iReqID    AS INT64 NO-UNDO.
DEFINE INPUT PARAMETER iNumFrame AS INT64 NO-UNDO.

{globals.i}
{intrface.get strng}  
{intrface.get tmess}
{intrface.get count}
{tmprecid.def}
{navigate.def}
{formula.pro}
{legop4077.def}

DEFINE VARIABLE mNewDl       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mHfrm        AS HANDLE      NO-UNDO.
DEFINE VARIABLE mOk          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mREGN        AS CHARACTER   NO-UNDO.

{path-cmp.get DataLine iReqID  YES}
{legop4077_0.fnd}

FIND FIRST DataBlock WHERE 
           DataBlock.Data-Id = DataLine.Data-Id
NO-LOCK NO-ERROR.
mHfrm = FRAME edit1:HANDLE.
PUT SCREEN COLOR messages 
   COLUMN mhfrm:COLUMN + 1
   ROW (mhfrm:ROW + mhfrm:HEIGHT-CHARS)
   " F11 - ДОБАВИТЬ ОСНОВАНИЕ И ПОДТВЕРЖДЕНИЕ ОПЕРАЦИИ".

ON ENTER OF FRAME edit1, FRAME edit2 ANYWHERE  DO: 
   APPLY "TAB" TO SELF.
   RETURN NO-APPLY.
END.

ON F1 OF mDocDate   IN FRAME edit1 DO:
   pick-value = "".
   RUN  calend.p.
   IF {assigned pick-value} THEN
      SELF:SCREEN-VALUE = pick-value.
END.
ON F1 OF mRejectDate IN FRAME edit1 DO:
   pick-value = "".
   RUN  calend.p.
   IF {assigned pick-value} THEN
      SELF:SCREEN-VALUE = pick-value.
END.
ON F1 OF mMessageDate  IN FRAME edit1 DO:
   pick-value = "".
   RUN  calend.p.
   IF {assigned pick-value} THEN
      SELF:SCREEN-VALUE = pick-value.
END.

ON F11 OF FRAME edit1 ANYWHERE DO:
   DEF BUFFER docDataLine FOR DataLine.

   pick-value = "".
   FIND CURRENT DataLine NO-LOCK NO-ERROR .
   RUN  legop4077brw_11.p 
    (DataLine.Data-ID, DataLine.Sym1,  1).
END.

ON "F1" OF mDocNum IN FRAME edit1 DO:
   DEF  BUFFER bop FOR op.
   DEF  BUFFER DelDataLine FOR DataLine.
   RUN browseld.p ("op", 
                   "" ,
                   "" ,
                   "" ,3 ).
   FOR FIRST bop WHERE  
             bop.op = INT64(pick-value)
   NO-LOCK:
      SELF:SCREEN-VALUE = bop.doc-num.
      mOP = bop.op.
      FOR EACH DelDataLine WHERE 
               DelDataLine.Data-Id = DataLine.Data-Id
           AND DelDataLine.Sym1    = DataLine.Sym1
           AND DelDataLine.Sym2    <> {&MAIN-LINE}
           AND DelDataLine.Sym2    <> {&BANK-LINE}
      EXCLUSIVE-LOCK:
         DELETE DelDataLine.
      END.

      RUN RefreshOpData IN THIS-PROCEDURE (BUFFER bop).
   END.
  
END.

ON "F1" OF mStatus IN FRAME edit1 DO:
   IF mStatus = "СОЗД" THEN
     RUN Fill-SysMes IN h_tmess 
         ("","","",
          "'СОЗД' - Создана запись по операции").
   ELSE 
      RUN Fill-SysMes IN h_tmess 
           ("","","",
            "'СОЗД' - Создана запись по операции").
END.

ON F1 OF mUserId  IN FRAME edit2 DO:
   pick-value = "".
   RUN   browseld.p("_user","", "", ?,4).
   mUserId = pick-value.
  
   RUN GetUserData IN THIS-PROCEDURE 
             (mUserId,
              OUTPUT mPost,
              OUTPUT mFirstName,
              OUTPUT mLastName,
              OUTPUT mMiddleName,
              OUTPUT mPhone,
              OUTPUT mEmail).
   DISPLAY {&fld2}
   WITH FRAME EDIT2.
END.

ON LEAVE  OF mUserId  IN FRAME edit2 DO:
   IF mUserId = TRIM(SELF:SCREEN-VAL)
   THEN 
      RETURN.
   ASSIGN mUserId.


   FIND FIRST _User WHERE 
              _User._Userid = mUserID
   NO-LOCK NO-ERROR.
   IF NOT AVAIL _User THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","","Нет такого пользователя!").
      RETURN NO-APPLY.
   END.
   RUN GetUserData IN THIS-PROCEDURE 
          (mUserId,
           OUTPUT mPost,
           OUTPUT mFirstName,
           OUTPUT mLastName,
           OUTPUT mMiddleName,
           OUTPUT mPhone,
           OUTPUT mEmail).
   DISPLAY
      {&fld2}
   WITH FRAME EDIT2.
   PAUSE 0 .
END.
ON F1 OF mCurrency IN FRAME edit1 DO:
   RUN currency.p ("", 5).                                                 
   IF KEYFUNC (LASTKEY) <> "end-error" THEN 
      SELF:SCREEN-VAL = pick-value.  
   ASSIGN mCurrency.                                             
   RETURN NO-APPLY.                                                        
END.
mNewDl = NEW(DataLine).
IF NEW(DataLine) THEN
DO:
   RUN find_formula IN THIS-PROCEDURE
        ("Legop",
         "ВерсПрог",
         TODAY,
         NO, 
         BUFFER formula).
   mProgramVersion  = IF AVAIL formula 
                      THEN ENTRY(1,formula.formula,"~~")  
                      ELSE "".
   RUN find_formula IN THIS-PROCEDURE
        ("Legop",
         "ЛогинУполнСотр",
         TODAY,
         NO, 
         BUFFER formula).
   IF AVAIL formula  THEN DO:
      mUserId  = ENTRY(1,formula.formula,"~~").

      RUN GetUserData IN THIS-PROCEDURE 
             (INPUT mUserId,
              OUTPUT mPost,
              OUTPUT mFirstName,
              OUTPUT mLastName,
              OUTPUT mMiddleName,
              OUTPUT mPhone,
              OUTPUT mEmail).
   END.
            
   mREGN = GetXattrValueEX("branch",STRING(DataBlock.Branch-id), "REGN", "").    

   ASSIGN
      mFilial   = IF INDEX(mREGN,"/") > 0  
                  THEN ENTRY(2,mREGN,"/")
                  ELSE ""        
      mBranchId = DataBlock.Branch-Id.

   { getbrnch.i
      &BRANCH-ID = "mBranchId"
      &REGN      = mRegNumb
      &BANKCODE  = mBnkCode
      &OKATO     = mOKATO
   }
   mBankData = GetSelfData(mRegNumb,mOKATO,mBnkCode,mBranchId).

   mRecordNum = STRING(YEAR(TODAY),"9999") + "_" +                       
                STRING(INT64(ENTRY(1,mBankData)),"9999")  + "_" +         
                STRING(INT64(ENTRY(6,mBankData)),"9999")  + "_" + "02_" + 
                STRING(GetCounterNextValue("Номер4077УОП",TODAY), "9999999999"). 
END.
{legop4077frm.eh}
mStatus:READ-ONLY  IN FRAME edit1 = TRUE.
mOk = ShowFrame(2).
IF mOk THEN
   RUN StoreDataLine IN THIS-PROCEDURE.

PUT SCREEN COLOR bright
    ROW SCREEN-LINES + 1 
    COL 1 Fill(" ",79).

PROCEDURE RefreshOpData PRIVATE:
   DEF  PARAMETER BUFFER bfOP FOR op.

   DEFINE VARIABLE vCurrency AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAmtRub   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vAmtCur   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vTmpStr   AS CHARACTER   NO-UNDO.

   DEF BUFFER op-entry FOR op-entry.
   DEF BUFFER acct     FOR acct.

   FOR EACH op-entry WHERE op-entry.op = bfop .op
   NO-LOCK:
      ASSIGN 
         vAmtRub   = vAmtRub + op-entry.amt-rub
         vAmtCur   = vAmtCur + op-entry.amt-Cur
         vCurrency = IF {assigned op-entry.currency } 
                     THEN op-entry.currency
                     ELSE "643"
         vTmpStr   = mCodeCash:LIST-ITEMS 
      IN FRAME edit1.
      IF   CAN-DO("20202*",op-entry.acct-db)
        OR CAN-DO("20202*",op-entry.acct-cr)
      THEN 
         mCodeCash:SCREEN-VALUE = ENTRY(2,vTmpStr).
         
      ELSE 
      IF   CAN-DO("40903*",op-entry.acct-db)
        OR CAN-DO("40903*",op-entry.acct-cr)
      THEN 
         mCodeCash:SCREEN-VALUE  = 
                  ENTRY(4, vTmpStr).
      ELSE 
         mCodeCash:SCREEN-VALUE = ENTRY(3,vTmpStr).

      {empty tt-cust}
      ChkClientAcct(op-entry.acct-cr,"cr").
      ChkClientAcct(op-entry.acct-db,"db").
      ChkClientOp(bfop.op,"Плательщик").
      ChkClientOp(bfop.op,"Получатель").

   END.
   ASSIGN 
      mCurrency    = vCurrency
      mAmtCur      = vAmtCur
      mAmtRub      = vAmtRub
      mOpUnusual   = GetXattrValueEx("op", STRING(bfop.op),"КодНеобыч","")
      mOpCharacter = GetXattrValueEx("op", STRING(bfop.op),"descr","")
      mDocDate     = bfop.doc-date
      mOp          = bfop.op 
      mCodeCash
   .
   
   DISPLAY 
      mCurrency
      mAmtRub
      mAmtCur
      mOpUnusual
      mOpCharacter
      mCodeCash
      mDocDate
   WITH FRAME edit1.
END PROCEDURE.


PROCEDURE StoreDataLine PRIVATE:
   DEFINE VARIABLE vTxt    AS LONGCHAR NO-UNDO.
   DEFINE BUFFER   DataLine FOR DataLine.

   DO WITH  FRAME edit1: 
      ASSIGN {&fld1}.
   END.
   ASSIGN   
      vTxt = 
                 FStrNVL(mFilial,"")
       +  "~n" + FStrNVL(mStatus,"")     
       +  "~n" + FStrNVL(mDocNum,"")         
       +  "~n" + FStrNVL(STRING(mDocDate,"99/99/9999"),"")        
       +  "~n" + FStrNVL(mRecordType:SCREEN-VALUE,"")     
       +  "~n" + FStrNVL(mFormatVersion,"") 
       +  "~n" + FStrNVL(mProgramVersion,"")
       +  "~n" + FStrNVL(STRING(mMessageDate,"99/99/9999"),"")     
       +  "~n" + FStrNVL(mInfoType:SCREEN-VALUE ,"")      
       +  "~n" + FStrNVL(mRejectCode:SCREEN-VALUE,"")
       +  "~n" + FStrNVL(mCurrency,"")   
       +  "~n" + FStrNVL(mOpCodeSign:SCREEN-VALUE,"")    
       +  "~n" + FStrNVL(mCodeCash:SCREEN-VALUE,"")    
       +  "~n" + FStrNVL(mOpCharacter,"")  
       +  "~n" + FStrNVL(mOpUnusual,"")    
       +  "~n" + FStrNVL(mComment,"")    

       +  "~n" + FStrNVL(mUserId,"")       
       +  "~n" + FStrNVL(REPLACE(mPost, "~n", ""),"")        
       +  "~n" + FStrNVL(REPLACE(mFirstName,"~n", ""),"")     
       +  "~n" + FStrNVL(REPLACE(mLastName,"~n", ""),"")    
       +  "~n" + FStrNVL(REPLACE(mMiddleName,"~n", ""),"") 
       +  "~n" + FStrNVL(REPLACE(mPhone,"~n", ""),"")        
       +  "~n" + FStrNVL(REPLACE(mEmail,"~n", ""),"")       
   NO-ERROR.
   
   FIND FIRST DataLine WHERE RECID(DataLine) = iReqID
   EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAIL DataLine THEN
   DO:
      ASSIGN 
         DataLine.Sym3 = mRecordNum
         DataLine.Sym4 = STRING(mRejectDate, "99/99/9999") 

         DataLine.Val[1] = mOp
         DataLine.Val[2] = mAmtCur
         DataLine.Val[3] = mAmtRub

         DataLine.Txt    = vTxt
     NO-ERROR.
     CreateDLSubjects(DataLine.Data-Id,
                      DataLine.Sym1).
   END.

END PROCEDURE.
/* $LINTFILE='legop4077frm_0.p' */
/* $LINTMODE='1,3,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='anba' */
/* $LINTDATE='10/03/2017 09:56:20.169+03:00' */
/*prosignLPchZTpoSsoYRJRfZfaztQ*/