/**/

   mResultPB1    = IF AVAIL(xPacketPB1Text)    THEN GetFileVar440(STRING(xPacketPB1Text.Contents),"КодРезПроверки","") ELSE "".
   mResultKWTPB1 = IF AVAIL(xPacketKWTPB1Text) THEN GetFileVar440(STRING(xPacketKWTPB1Text.Contents),"КодРезПроверки","") ELSE "".
   mResultKWTBVS = IF AVAIL(xPacketKWTBVSText) THEN GetFileVar440(STRING(xPacketKWTBVSText.Contents),"КодРезПроверки","") ELSE "".
   
   PUT UNFORMATTED
      '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultPB1    + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultKWTPB1 + '</Data></Cell>~n' +
      '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultKWTBVS + '</Data></Cell>~n'.
END.
ELSE
DO:
   PUT UNFORMATTED
      '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
      '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
      '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n'.
END.

mPB2Acct   = "".
mResultPB2 = "".

mPB2Acct   = IF AVAIL(xPacketPB2Text) THEN GetFileAllVar440(STRING(xPacketPB2Text.Contents),"ЗначРекв","") ELSE "".
mTmpResult = IF AVAIL(xPacketPB2Text) THEN GetFileAllVar440(STRING(xPacketPB2Text.Contents),"Пояснение","") ELSE "".

mResultPB2 = "".
IF AVAIL(acct) THEN
DO mInt2 = 1 TO NUM-ENTRIES(mPB2Acct):
   IF ENTRY(mInt2,mPB2Acct) EQ acct.number 
   THEN mResultPB2 = ENTRY(mInt2,mTmpResult).
   ELSE mResultPB2 = mTmpResult.
END.

IF mResultPB2 EQ "" AND mTmpResult NE "" THEN mResultPB2 = mTmpResult.

PUT UNFORMATTED
   '    <Cell ss:StyleID="' + rowStyle78  + '"><Data ss:Type="String">' + mResultPB2 + '</Data></Cell>~n' +
   '   </Row>'
SKIP.
