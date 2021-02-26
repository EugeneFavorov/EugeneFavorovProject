/*
Банковская интегрированная система БИСквит
*/

{globals.i}
{wordwrap.def}
{bank-id.i}
{op-ident.i}
{intrface.get op}
{intrface.get netw}
{intrface.get olap}

DEFINE VARIABLE mFileNameTmp AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRet         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsSendFile  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mIsSaveFile  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mFileName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBytes       AS INT64   NO-UNDO.
DEFINE VARIABLE mIsExist     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mRid         AS INT64   NO-UNDO.
DEFINE VARIABLE mDir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPath        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mExtFile     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mSpin        AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE mDateOp      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateDog     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDateSpr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCnt         AS INT64   NO-UNDO.
DEFINE VARIABLE mRegNum      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMFO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBIC         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankINN     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClKPP       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecCntry    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBCType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBCode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBRCodeC     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSVC         AS CHARACTER NO-UNDO.

{ree-138i.sel}
{ree-138i.i &cb = YES}

mFileName = "reestr-138i-" + 
   STRING(YEAR(TODAY),"9999") + 
   STRING(MONTH(TODAY),"99") + 
   STRING(DAY(TODAY),"99") + "-" +
   TRIM(STRING(TIME,"hh:mm:ss")) + ".xml".
mFileName = REPLACE(mFileName,":","").

OUTPUT TO VALUE(mFileName) UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

mSpin[1] = ".   ".
mSpin[2] = "..  ".
mSpin[3] = "... ".
mSpin[4] = "....".
mSpin[5] = " ...".
mSpin[6] = "  ..".
mSpin[7] = "   .".
mSpin[8] = ".  .".

{ree-138i.hdr}

mCnt = 0.

/*FOR EACH tt-117 NO-LOCK:*/
/*   EXPORT tt-117.       */
/*END.                    */


FOR EACH tt-117 WHERE TRUE
   /*AND tt-117.c-acc EQ "40703810300300010116     @0500"*/
   AND CAN-DO("407*,406*,40802*,40807*,40821*",tt-117.c-acc)
	/*   BREAK BY tt-117.c-cat */
	/*         BY tt-117.c-nam */
	/*         BY tt-117.c-id  */
	/*         BY tt-117.c-acc */
	/*         BY tt-117.op-dt:*/
	/*   BREAK BY tt-117.op-dt */
	/*         BY tt-117.c-cat */
	/*         BY tt-117.c-nam */
	/*         BY tt-117.c-id  */
	/*         BY tt-117.c-acc:*/
   NO-LOCK:   
   mCnt = mCnt + 1.
   
   mBCType  = IF tt-117.bctype EQ ?  THEN "" ELSE tt-117.bctype. 
   mBCode   = IF tt-117.bcode  EQ ?  THEN "" ELSE tt-117.bcode.
   
   mBRCodeC = IF tt-117.brcode EQ ?  THEN "" ELSE tt-117.brcode.
   mBRCodeC = IF mBRCodeC       EQ "" THEN "643" ELSE mBRCodeC.
   
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.bctype = " + mBCType +  " tt-117.bcode = " + mBCode).
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 +  " tt-117.pname2 = " + tt-117.pname2).

   ASSIGN
      mRegNum     = FGetSetting("REGN",?,"")
      mBname[1]   = TRIM(tt-117.bname) 
      mAdr[1]     = TRIM(tt-117.adr)
      mPName[1]   = TRIM(tt-117.pname1 + " " + tt-117.pname2)
      mCliName[1] = TRIM(tt-117.f-nam)
      mDateOp     = IF tt-117.op-dt EQ ? THEN "" ELSE STRING(tt-117.op-dt,"99/99/9999")
      mDateDog    = tt-117.datdog
      mDateSpr    = tt-117.dt-spr
      mMFO        = IF (mBCType EQ "МФО-9" OR  mBCType EQ "") THEN mBCode ELSE ""
      mBIC        = IF (mBCType NE "МФО-9" AND mBCType NE "") THEN mBCode ELSE ""
      mBankINN    = FGetSetting("ИНН",?,"")
      mSVC        = IF tt-117.r-SVC EQ 0.00 THEN "" ELSE TRIM(STRING(tt-117.r-SVC,">>>>>>>>>>>9.99"))
      .
      
   /*столбец T пусто*/
   mBankINN = "".
   
   IF NUM-ENTRIES(tt-117.ckpp) EQ 1 THEN mClKPP = tt-117.ckpp.
   ELSE IF NUM-ENTRIES(tt-117.ckpp) GT 1 THEN mClKPP = ENTRY(1,tt-117.ckpp).
   ELSE mClKPP = "".
      
   /*mDateDog    = IF tt-117.op-dt EQ ? THEN "" ELSE STRING(DATE(tt-117.datdog),"99/99/9999") */
   /*mDateSpr    = IF tt-117.op-dt EQ ? THEN "" ELSE STRING(DATE(tt-117.datdog),"99/99/9999").*/

   PUT UNFORMATTED
   	'   <Row ss:AutoFitHeight="0">~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mRegNum + '</Data></Cell>~n' +
/*   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + STRING(tt-117.op) + '</Data></Cell>~n' +*/
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + TRIM(STRING(tt-117.c-acc,"x(20)")) + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mDateOp + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-117.napr + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-117.r-KOV + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-117.op-cu + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s73"><Data ss:Type="Number">' + TRIM(STRING(tt-117.op-su,">>>>>>>>>>>9.99")) + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s74"><Data ss:Type="String">' + TRIM(tt-117.numdog) + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mDateDog + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-117.r-PSd + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-117.r-VCO + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mSVC + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s74"><Data ss:Type="String">' + mCliName[1] + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-117.cinn + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mClKPP + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String"></Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + tt-117.ccode + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s74"><Data ss:Type="String">' + mPName[1] + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + (IF tt-117.pcode EQ "" THEN "643" ELSE tt-117.pcode) + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mBankINN + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s74"><Data ss:Type="String">' + mBname[1] + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mMFO + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mBIC + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mBRCodeC + '</Data></Cell>~n' +
   	'    <Cell ss:StyleID="s71"><Data ss:Type="String">' + mDateSpr + '</Data></Cell>~n' +   
   	'   </Row>~n'.
END.

{ree-138i.ftr}

OUTPUT CLOSE.

RUN sndbispc ("file=" + mFileName + ";class=bq").

{intrface.del}          /* Выгрузка инструментария. */ 
