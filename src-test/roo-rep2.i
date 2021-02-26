/**/

   mResultPB1    = IF AVAIL(xPacketPB1Text)    THEN GetFileVar440(STRING(xPacketPB1Text.Contents),"КодРезПроверки","") ELSE "".
   mResultKWTPB1 = IF AVAIL(xPacketKWTPB1Text) THEN GetFileVar440(STRING(xPacketKWTPB1Text.Contents),"КодРезПроверки","") ELSE "".
   mResultKWTBVS = IF AVAIL(xPacketKWTBVSText) THEN GetFileVar440(STRING(xPacketKWTBVSText.Contents),"КодРезПроверки","") ELSE "".
   
   PUT UNFORMATTED
      '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultPB1    + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultKWTPB1 + '</Data></Cell>~n'.
END.
ELSE
DO:
   PUT UNFORMATTED
      '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
      '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n'.
END.

mPB2Acct   = "".
mResultPB2 = "".

mPB2Acct   = IF AVAIL(xPacketPB2Text) THEN GetFileAllVar440(STRING(xPacketPB2Text.Contents),"ЗначРекв","") ELSE "".
mTmpResult = IF AVAIL(xPacketPB2Text) THEN GetFileAllVar440(STRING(xPacketPB2Text.Contents),"Пояснение","") ELSE "".

RUN dbgprint.p ("roo-rep440","~nmPB2Acct = "   + mPB2Acct).
RUN dbgprint.p ("roo-rep440","~nmTmpResult = " + mTmpResult).
mResultPB2 = "".

IF mAcctFound EQ YES THEN
DO mInt2 = 1 TO NUM-ENTRIES(mPB2Acct):
   IF ENTRY(mInt2,mPB2Acct) EQ acct.number 
   THEN mResultPB2 = ENTRY(mInt2,mTmpResult).
END.

mResultPB2 = IF mResultPB2 EQ "" THEN mTmpResult ELSE mResultPB2.

PUT UNFORMATTED
   '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultPB2 + '</Data></Cell>~n' +
   '   </Row>'
SKIP.
