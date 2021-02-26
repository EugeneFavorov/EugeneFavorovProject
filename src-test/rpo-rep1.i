   PUT UNFORMATTED
   '   <Row ss:AutoFitHeight="1">~n' +
	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimeRPO + '</Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNameRPO + '</Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyleStateRPO + '"><Data ss:Type="String">' + mStateRPO + '</Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimeBOS + '</Data></Cell>~n' +
   '    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNameBOS + '</Data></Cell>~n' +
   '    <Cell ss:StyleID="' + rowStyleStateBOS + '"><Data ss:Type="String">' + mStateBOS + '</Data></Cell>~n'.            
END.
ELSE
DO:
   PUT UNFORMATTED
   '   <Row ss:AutoFitHeight="1">~n' +
	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
   '    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
   '    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n'.            
END.

IF AVAIL(acct) 
THEN mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").
ELSE mGroup = "".  

IF INT64(mGroup) EQ 599 OR INT64(mGroup) EQ 596 
THEN 
ASSIGN
   rowStyleGroup = "s78yellow"
   rowStyleAcct  = "s77yellow". 
ELSE 
ASSIGN
   rowStyleGroup = "s78"
   rowStyleAcct  = "s77". 

IF      shFilial EQ "0000" THEN mLocalTime = 0. 
ELSE IF shFilial EQ "0300" THEN mLocalTime = 7200.
ELSE IF shFilial EQ "0500" THEN mLocalTime = 10800.

IF AVAIL(Packet) AND AVAIL(acct) THEN
DO:
   mOstFormLocal = PosOnTime(acct.acct,acct.currency,DATETIME(Packet.PackDate,(Packet.PackTime + mLocalTime) * 1000)).
   mOstForm      = PosOnTime(acct.acct,acct.currency,DATETIME(Packet.PackDate,(Packet.PackTime) * 1000)).
END.
ELSE
DO:
   mOstFormLocal = 0.
   mOstForm      = 0.
END.

mOstFile = ''.
mCnt = 1.
mTmpAcct = IF AVAIL(xPacketBVSText) THEN GetFileAllVar440(STRING(xPacketBVSText.Contents),"®¬‘η","")   ELSE "".
mTmpOst  = IF AVAIL(xPacketBVSText) THEN GetFileAllVar440(STRING(xPacketBVSText.Contents),"αβ β®ª","") ELSE "".

mTmpAcct = IF mTmpAcct NE "" THEN mTmpAcct ELSE zAcct.  

RUN dbgprint.p ("rpo-rep440 " + " {&line-number}","~nmTmpAcct = " + mTmpAcct).
RUN dbgprint.p ("rpo-rep440 " + " {&line-number}","~nmTmpOst = "  + mTmpOst).

IF AVAIL(acct) THEN
DO mInt2 = 1 TO NUM-ENTRIES(mTmpAcct):
   IF ENTRY(mInt2,mTmpAcct) EQ acct.number
   THEN
   DO: 
      IF mInt2 LE NUM-ENTRIES(mTmpOst) 
      THEN mOstFile = ENTRY(mInt2,mTmpOst).
      ELSE mOstFile = "0".
   END.
END.

IF DEC(mOstFile) NE - mOstFormLocal
THEN rowStyleOst = "s79red".
ELSE rowStyleOst = "s79".

IF AVAIL(acct) THEN
   PUT UNFORMATTED
	'    <Cell ss:StyleID="' + rowStyleAcct  + '"><Data ss:Type="String">' + acct.number + '</Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyleGroup + '"><Data ss:Type="String">' + mGroup      + '</Data></Cell>~n'.
ELSE
   PUT UNFORMATTED
	'    <Cell ss:StyleID="' + rowStyleAcct  + '"><Data ss:Type="String">' + mTmpAcct + '</Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyleGroup + '"><Data ss:Type="String">' + mGroup      + '</Data></Cell>~n'.
	
PUT UNFORMATTED
   '    <Cell ss:StyleID="' + rowStyleOst + '"><Data ss:Type="Number">' 
   + TRIM(STRING( - mOstFormLocal,"->>>,>>>,>>>,>>9.99"))
   + '</Data></Cell>~n' + 
   '    <Cell ss:StyleID="' + rowStyleOst + '"><Data ss:Type="Number">' 
   + TRIM(STRING(DEC(mOstFile),"->>>,>>>,>>>,>>9.99")) 
   + '</Data></Cell>~n'.

IF AVAIL(xPacketPB2Text) THEN rowStylePB2 = "s77red". ELSE rowStylePB2 = "s77".
