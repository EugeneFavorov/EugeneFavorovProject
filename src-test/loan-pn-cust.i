&IF DEFINED(PNCustCorpBuf) = 0 &THEN
   &SCOPED-DEFINE PNCustCorpBuf xcust-corp
&ENDIF
&IF DEFINED(PNCustName) = 0 &THEN
   &SCOPED-DEFINE PNCustName mCustName
&ENDIF
&IF DEFINED(PNCustINN) = 0 &THEN
   &SCOPED-DEFINE PNCustINN mCustINN
&ENDIF
&IF DEFINED(PNCustKPP) = 0 &THEN
   &SCOPED-DEFINE PNCustKPP mCustKPP
&ENDIF


{nextnum.i &nextnumpref=loan-pn-cust-i-}
DEFINE VAR vLPCICustName{&loan-pn-cust-i-num}-1 AS CHAR NO-UNDO.
DEFINE VAR vLPCICustName{&loan-pn-cust-i-num}-2 AS CHAR NO-UNDO.
DEFINE VAR vLPCIINN{&loan-pn-cust-i-num}        AS CHAR NO-UNDO.


IF {&file}.cust-id > 0 THEN DO:
   FIND FIRST {&PNCustCorpBuf} WHERE {&PNCustCorpBuf}.cust-id = {&file}.cust-id NO-LOCK NO-ERROR.
   IF AVAIL {&PNCustCorpBuf} THEN DO:
      vLPCIINN{&loan-pn-cust-i-num} = "".
      RUN GetCustName IN h_base ("", {&PNCustCorpBuf}.cust-id, ?, OUTPUT vLPCICustName{&loan-pn-cust-i-num}-1, OUTPUT vLPCICustName{&loan-pn-cust-i-num}-2, INPUT-OUTPUT vLPCIINN{&loan-pn-cust-i-num}).
      ASSIGN
         {&PNCustName} = TRIM(vLPCICustName{&loan-pn-cust-i-num}-1 + " " + vLPCICustName{&loan-pn-cust-i-num}-2)
         {&PNCustINN}  = (IF vLPCIINN{&loan-pn-cust-i-num} = ? THEN "" ELSE vLPCIINN{&loan-pn-cust-i-num})
         {&PNCustKPP}  = GetXattrValueEX("cust-corp",STRING({&PNCustCorpBuf}.cust-id),"","")
      .
   END.
   ELSE ASSIGN
      {&PNCustName} = ""
      {&PNCustINN}  = ""
      {&PNCustKPP}  = ""
   .
END.
ELSE DO:
   {&PNCustName} = GetXAttrValueEx("loan",{&file}.contract + "," + {&file}.cont-code,"®«γη β¥«μ ¨¬¥­®Ά ­¨¥","").
   {&PNCustINN}  = GetXAttrValueEx("loan",{&file}.contract + "," + {&file}.cont-code,"®«γη β¥«μ","").
   {&PNCustKPP}  = GetXAttrValueEx("loan",{&file}.contract + "," + {&file}.cont-code,"®«γη β¥«μ","").
END.
/* $LINTFILE='loan-pn-cust.i' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='shoi' */
/* $LINTDATE='10/12/2016 12:59:56.956+03:00' */
/*prosignITJw2eJyh9olj1vOXirfxw*/