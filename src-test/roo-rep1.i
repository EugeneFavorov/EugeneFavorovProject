   PUT UNFORMATTED
   '   <Row ss:AutoFitHeight="1">~n' +
	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mDateTimeROO + '</Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + mFileNameROO + '</Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyleStateROO + '"><Data ss:Type="String">' + mStateROO + '</Data></Cell>~n'.
END.
ELSE
DO:
   PUT UNFORMATTED
   '   <Row ss:AutoFitHeight="1">~n' +
	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle77 + '"><Data ss:Type="String"></Data></Cell>~n' +
	'    <Cell ss:StyleID="' + rowStyle78 + '"><Data ss:Type="String"></Data></Cell>~n'.
END.

IF mAcctFound EQ YES THEN
DO:
   PUT UNFORMATTED
   '    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + acct.number + '</Data></Cell>~n'.
   mGroup = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"groupOABS","").
END.
ELSE
DO:
   PUT UNFORMATTED
   '    <Cell ss:StyleID="' + rowStyle77       + '"><Data ss:Type="String">' + zAcct + '</Data></Cell>~n'.
   mGroup = "".
END.

IF INT64(mGroup) EQ 599 OR INT64(mGroup) EQ 596 
THEN 
ASSIGN
   rowStyleGroup = "s78yellow"
   rowStyleAcct  = "s77yellow". 
ELSE 
ASSIGN
   rowStyleGroup = "s78"
   rowStyleAcct  = "s77". 

IF AVAIL(xPacketPB2Text) THEN rowStylePB2 = "s77red". ELSE rowStylePB2 = "s77".
