/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ЗАО "Банковские информационные системы"
     Filename: SVOD-BS.P
      Comment: <comment>
   Parameters:
      Created: 01.04.2014 09:44 STRE    
     Modified: 01.04.2014 09:44 STRE    
*/

{globals.i}
{intrface.get vok}
{intrface.get instrum}    
{intrface.get xclass}

{chkacces.i}

{prn-doc.def
   &with_proc = YES
}

DEFINE TEMP-TABLE tt-report NO-UNDO
   FIELD op          LIKE op.op
   FIELD op-date     AS   DATE
   FIELD op-realdate AS   DATE
   FIELD op-realtime AS   INT64
   FIELD VidOpNalV   AS   CHARACTER
   FIELD rate        AS   DECIMAL
   FIELD curr-b      AS   CHARACTER
   FIELD summ-b      AS   DECIMAL
   FIELD curr-s      AS   CHARACTER
   FIELD summ-s      AS   DECIMAL
   FIELD country     AS   CHARACTER
   FIELD cl-name     AS   CHARACTER
   FIELD cl-docum    AS   CHARACTER
   FIELD cl-addr     AS   CHARACTER
   FIELD user-name   AS   CHARACTER
   FIELD RateType    AS   CHARACTER

   INDEX iOp         op
   INDEX idxDateTime op-date op-realdate op-realtime
.

DEFINE TEMP-TABLE tt-total NO-UNDO
   FIELD currency    AS   CHARACTER
   FIELD summ-b      AS   DECIMAL
   FIELD summ-s      AS   DECIMAL

   index icur        currency
.

DEFINE VARIABLE mRptBegDate      AS DATE      NO-UNDO.
DEFINE VARIABLE mRptEndDate      AS DATE      NO-UNDO.
DEFINE VARIABLE mRptBranch       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpTime          AS INT64     NO-UNDO.
DEFINE VARIABLE mOpDate          AS DATE      NO-UNDO.
DEFINE VARIABLE mVidOpNalV       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpPokProd       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKodNacVal       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRateD           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mRateC           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mRate            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCurrRateType1   AS CHARACTER NO-UNDO. /* Покупка */
DEFINE VARIABLE mCurrRateType2   AS CHARACTER NO-UNDO. /* Продажа */
DEFINE VARIABLE mChekRateType1   AS CHARACTER NO-UNDO. /* Покупка */
DEFINE VARIABLE mChekRateType2   AS CHARACTER NO-UNDO. /* Продажа */
DEFINE VARIABLE mRateType1       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRateType2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCodCen          AS CHARACTER NO-UNDO.

DEFINE VARIABLE mDover           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mFio             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCountry         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocum           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocumentId      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustDocWho      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocumDate       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAddress         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDiffRate        AS CHARACTER NO-UNDO.
                                
DEFINE VARIABLE mText            AS LONGCHAR  NO-UNDO INIT "".
                                
DEFINE VARIABLE mBCur            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mICur            AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCrossRate       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mRateDate        AS DATE      NO-UNDO.
DEFINE VARIABLE vRateTime        AS INT64     NO-UNDO.
          
DEFINE VARIABLE flag          AS LOGICAL   NO-UNDO.
                               
DEFINE BUFFER   bOp              FOR op.
DEFINE BUFFER   bOp-trans        FOR op-transaction.
DEFINE BUFFER   bSigns           FOR signs.
DEFINE BUFFER   bKau-entry       FOR kau-entry.

ASSIGN
   mRptBegDate = gBeg-Date
   mRptEndDate = gEnd-Date
   mRptBranch  = GetUserXAttrValue(USERID("BISQUIT"), "Отделение")
.

PAUSE 0 BEFORE-HIDE.
RUN GetRepData NO-ERROR.

IF ERROR-STATUS:ERROR THEN
DO:
  RETURN "".
END.

{empty tt-report}
{empty tt-total}

ASSIGN
   mOpPokProd     = FGetSetting("ОпПокПрод","","01,02,03")
   mKodNacVal     = FGetSetting("КодНацВал","","810")
   mCurrRateType1 = ENTRY(1,FGetSetting("ЦенТипКурс","Валюта",""))
   mCurrRateType2 = IF NUM-ENTRIES(FGetSetting("ЦенТипКурс","Валюта","")) > 1 THEN 
                       ENTRY(2,FGetSetting("ЦенТипКурс","Валюта","")) ELSE ""
   mChekRateType1 = ENTRY(1,FGetSetting("ЦенТипКурс","ПлатДок",""))
   mChekRateType2 = IF NUM-ENTRIES(FGetSetting("ЦенТипКурс","ПлатДок","")) > 1 THEN 
                       ENTRY(2,FGetSetting("ЦенТипКурс","ПлатДок","")) ELSE "" 
.

IF (mRptBranch = 'СВОД') OR (mRptBranch = '') OR (mRptBranch = '*') THEN flag = TRUE.

FOR EACH bOp-trans WHERE
         bop-trans.op-date GE mRptBegDate
   AND   bop-trans.op-date LE mRptEndDate 
   NO-LOCK,

   FIRST bOp of bOp-trans WHERE 
         ((bOp.branch-id EQ mRptBranch) OR flag) 
    AND  bOp.op-kind NE "vok-svod"
   NO-LOCK,
   
   FIRST  bSigns WHERE
          bSigns.file-name EQ "op"
      AND bSigns.surrogate EQ STRING(bop.op)
      AND bSigns.code      EQ "ВидОпНалВ"
      AND CAN-DO(mOpPokProd,bSigns.code-val)
   NO-LOCK,
   
   EACH bKau-entry OF bOp
   NO-LOCK:

      ASSIGN
         mRate   = DECIMAL(GetXAttrValue("op",STRING(bKau-entry.op),"sprate")) NO-ERROR.
         mCodCen = GetXAttrValue("acct",bKau-Entry.acct + "," + bKau-entry.currency,"form-type-code").
         mOpDate = bOp-trans.beg-date.
         mOpTime = bOp-trans.beg-time.
      .
      ASSIGN
         mRateType1 = IF mCodCen EQ "" THEN mCurrRateType1 ELSE mChekRateType1.
         mRateType2 = IF mCodCen EQ "" THEN mCurrRateType2 ELSE mChekRateType2.
      .
      
      FIND FIRST tt-report WHERE
                 tt-report.op EQ bKau-entry.op 
         NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE tt-report THEN
      DO:
         RUN GetPersonAttr(bOp.op, NO,
                       OUTPUT mDover,
                       OUTPUT mCountry,
                       OUTPUT mFio,
                       OUTPUT mDocum,
                       OUTPUT mDocumentId,
                       OUTPUT mCustDocWho,
                       OUTPUT mDocumDate,
                       OUTPUT mAddress).
   
         CREATE tt-report.
         ASSIGN
            tt-report.op          = bOp.op
            tt-report.op-date     = bOp.doc-date
            tt-report.op-realdate = mOpDate
            tt-report.op-realtime = mOpTime
            tt-report.VidOpNalV   = bSigns.code-value
            tt-report.rate        = mRate
            tt-report.curr-b      = IF bKau-entry.debit THEN bKau-entry.currency ELSE ""
            tt-report.summ-b      = IF bKau-entry.debit THEN 
                                       (IF bKau-entry.currency GT "" THEN 
                                           bKau-entry.amt-cur 
                                        ELSE
                                           bKau-entry.amt-rub)
                                    ELSE 0
            tt-report.curr-s      = IF NOT bKau-entry.debit THEN bKau-entry.currency ELSE ""
            tt-report.summ-s      = IF NOT bKau-entry.debit THEN 
                                          (IF bKau-entry.currency GT "" THEN 
                                              bKau-entry.amt-cur 
                                           ELSE
                                              bKau-entry.amt-rub) 
                                    ELSE 0
            tt-report.country     = mCountry
            tt-report.cl-name     = mFio
            tt-report.cl-docum    = GetCodeNameEx("КодДокум",mDocumentId,mDocumentId) + 
                                    IF mDocum GT ""
                                    THEN "," + mDocum
                                    ELSE "" + 
                                    IF mCustDocWho GT ""
                                    THEN "," + mCustDocWho
                                    ELSE "" + 
                                    IF mDocumDate GT ""
                                    THEN "," + mDocumDate
                                    ELSE ""
            tt-report.cl-addr     = mAddress
            tt-report.user-name   = GetTempXAttrValueEx("_user",bOp.user-id,"_User-Name",
                                                         bOp.doc-date,GetUserName(bOp.user-id))
         .
      END.
      ELSE
      DO:
         IF bKau-entry.debit THEN 
            ASSIGN
               tt-report.curr-b = bKau-entry.currency
               tt-report.summ-b = IF bKau-entry.currency GT "" THEN bKau-entry.amt-cur ELSE bKau-entry.amt-rub
            .
         ELSE
            ASSIGN
               tt-report.curr-s = bKau-entry.currency
               tt-report.summ-s = IF bKau-entry.currency GT "" THEN bKau-entry.amt-cur ELSE bKau-entry.amt-rub
            .
         IF     tt-report.curr-b NE "" 
            AND tt-report.curr-s NE "" THEN
               tt-report.ratetype = "Учетный".
         ELSE
            IF     tt-report.curr-b EQ "" 
            AND tt-report.curr-s NE "" THEN
               tt-report.ratetype = mRateType2.
         ELSE
            IF     tt-report.curr-b NE "" 
               AND tt-report.curr-s EQ "" THEN
               tt-report.ratetype = mRateType1.
      END.
END.

mText = mText + "ReportHeader:" + "Сводный отчет по покупке-продаже~n"
              + "ReportPeriod:на период с "
              + STRING(mRptBegDate, "99.99.9999") + " г. по " 
              + STRING(mRptEndDate, "99.99.9999") + " г.~n"
              + "~n[table]:1~n" .

FOR EACH tt-report NO-LOCK 
   USE-INDEX idxDateTime:
   IF tt-report.rate EQ 0 THEN DO:
      CASE tt-report.VidOpNalV:
         WHEN "01" THEN
         ASSIGN
            mBCur = tt-report.curr-b
            mICur = tt-report.curr-s
         .
         WHEN "02" THEN
            ASSIGN
               mBCur = tt-report.curr-s
               mICur = tt-report.curr-b
            . 
         OTHERWISE
            ASSIGN
               mBCur = tt-report.curr-s
               mICur = tt-report.curr-b
            . 
      
      END CASE.
      
      RUN CrossRateTimeR(tt-report.RateType,
                         mBCur,
                         mICur,
                         mRptBranch,
                         tt-report.op-realdate,
                         tt-report.op-realtime,
                         DEC(0),
                         OUTPUT mCrossRate,
                         OUTPUT mRateDate,
                         OUTPUT vRateTime) NO-ERROR.
   END.
   ELSE
      mCrossRate = tt-report.rate.

   mText = mText  + "[row]:1~n[@]~n" 
                  + STRING(tt-report.op-date,"99.99.9999")              + "~n"
                  + (IF tt-report.op-date NE tt-report.op-realdate THEN
                        (STRING(tt-report.op-realdate,"99.99.9999")     + " ") 
                    ELSE "")
                  + STRING(tt-report.op-realtime, "HH:MM:SS")           + "~n"
                  + tt-report.VidOpNalV                                 + "~n[@/]~n"
                  + STRING(mCrossRate)                                  + "~n[@]~n" 
                  + (IF tt-report.curr-b EQ "" THEN                    
                        mKodNacVal                                      
                     ELSE tt-report.curr-b)                             + "~n[@/]~n"
                  + STRING(tt-report.summ-b)                            + "~n[@]~n" 
                  + (IF tt-report.curr-s EQ "" THEN                     
                        mKodNacVal                                      
                     ELSE tt-report.curr-s)                             + "~n[@/]~n"
                  + STRING(tt-report.summ-s)                            + "~n[@]~n"
                  + tt-report.country                                   + "~n"
                  + tt-report.cl-name                                   + "~n"
                  + tt-report.cl-docum                                  + "~n"
                  + tt-report.cl-addr                                   + "~n"
                  + tt-report.user-name                                 + "~n[@/]~n"
   .

   RUN AccumTotal(tt-report.curr-b,YES,tt-report.summ-b).
   RUN AccumTotal(tt-report.curr-s,NO, tt-report.summ-s).
END.

mText = mText + "[table/]:1~n[table]:2~n".
FOR EACH tt-total NO-LOCK:
   mText = mText  + "[row]:1~n[@]~n"
                  + tt-total.currency       + "~n[@/]~n"
                  + STRING(tt-total.summ-b) + "~n[@]~n"
                  + tt-total.currency       + "~n[@/]~n"
                  + STRING(tt-total.summ-s) + "~n"
   .
END.
mText = mText + "[table/]:2~n".

SUBSCRIBE TO "get-ttnames" ANYWHERE RUN-PROCEDURE "get-ttnames".
RUN INSERT_TTNAME("teg", "").
UNSUBSCRIBE TO "get-ttnames".

RUN printvd.p ("svod-bs1", INPUT TABLE ttNames).

{intrface.del}

RETURN "".

PROCEDURE get-ttnames:
   DEFINE PARAMETER BUFFER bttnames FOR ttnames.
   IF bttnames.tvalue EQ "" THEN
   COPY-LOB mText TO bttnames.tlong.
END PROCEDURE.


PROCEDURE AccumTotal:

DEFINE INPUT PARAMETER iCurr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iDb   AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER iAmt  AS DECIMAL   NO-UNDO.

DEFINE VARIABLE vCurr        AS CHARACTER NO-UNDO.
 
   vCurr = IF iCurr EQ "" THEN mKodNacVal ELSE iCurr.
   FIND FIRST tt-total WHERE
      tt-total.currency EQ vCurr
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tt-total THEN
   DO:
      CREATE tt-total.
      ASSIGN 
         tt-total.currency = vCurr
         tt-total.summ-b   = IF iDb THEN iAmt ELSE 0
         tt-total.summ-s   = IF iDb THEN 0 ELSE iAmt
      .
   END.
   ELSE
   DO:
      IF iDb THEN
         tt-total.summ-b   = tt-total.summ-b + iAmt.
      ELSE
         tt-total.summ-s   = tt-total.summ-s + iAmt.
   END.

END PROCEDURE.


PROCEDURE GetRepData:

DEFINE VARIABLE vHStartField AS HANDLE NO-UNDO.

   FORM 
      mRptBranch
         LABEL "Подразделение"
         FORMAT "x(8)"
         AT ROW 2 COL 2
      "Период отчета" AT ROW 4 COL 2
      mRptBegDate
         LABEL "C"
         FORMAT "99/99/9999"
         AT ROW 5 COL 2
      mRptEndDate
         LABEL "ПО"
         FORMAT "99/99/9999"
         AT ROW 5 COL 17
      SKIP(1)
      WITH FRAME fr1 WIDTH 36 SIDE-LABELS OVERLAY CENTERED
      TITLE "[ ВВОД ДАННЫХ ]".
      
   ON GO OF FRAME fr1 ANYWHERE
   DO:
      ASSIGN FRAME fr1
         mRptBegDate
         mRptEndDate
         mRptBranch 
      .
   END.

   ON RETURN OF FRAME fr1 ANYWHERE 
   DO:
       APPLY "TAB" TO SELF.
       RETURN NO-APPLY.
   END.

   ENABLE
      mRptBegDate
      mRptEndDate
      mRptBranch 
      WITH FRAME fr1
   .

   DISPLAY 
      mRptBegDate
      mRptEndDate
      mRptBranch 
      WITH FRAME fr1
   .

   ON F1 OF mRptBegDate, mRptEndDate IN FRAME fr1
   DO:
       RUN calend.p.
       IF (pick-value NE "") AND (LASTKEY NE 27) THEN
       DO:
          SELF:SCREEN-VALUE = pick-value.
       END.
       RETURN NO-APPLY.
   END.

   ON F1 OF mRptBranch IN FRAME fr1
   DO:
      DO TRANSACTION:
         RUN browseld.p ("branch",
                     "",
                     "",
                     ?, 
                     5).
         IF pick-value NE ? THEN
            SELF:SCREEN-VALUE = pick-value.
      END.
      RETURN NO-APPLY.        
   END.

   vHStartField = mRptBranch:HANDLE IN FRAME fr1.

   WAIT-FOR GO, END-ERROR OF CURRENT-WINDOW OR WINDOW-CLOSE OF CURRENT-WINDOW FOCUS vHStartField.
   HIDE FRAME fr1 NO-PAUSE.
   
   IF LASTKEY NE 10 AND LASTKEY NE 13 THEN 
   DO:
      RETURN ERROR.
   END.

END PROCEDURE. /* GetReportData */

{persdata.pro}
