
&IF DEFINED(DataID) = 0 &THEN
   &SCOPED-DEFINE DataID iDataID
&ENDIF
&IF DEFINED(BirthDay) = 0 &THEN
   &SCOPED-DEFINE BirthDay mBirthDay
&ENDIF
&IF DEFINED(BirthPlace) = 0 &THEN
   &SCOPED-DEFINE BirthPlace mBirthPlace
&ENDIF
&IF DEFINED(DataCls) = 0 &THEN
   &SCOPED-DEFINE DataCls mDataCls
&ENDIF


mCnt = 0.
IF    {assigned code.misc[3]}
   OR {assigned code.misc[5]}
   OR {assigned code.misc[6]}
THEN
   RUN CreateStopListDL (
      {&DataID},
      TRIM(code.code),
      CODE.misc[3],
      CODE.NAME,
      CODE.misc[5],
      CODE.misc[6],
      {&BirthDay},
      {&BirthPlace},
      {&DataCls},
      INPUT-OUTPUT mCnt).
/* $LINTFILE='stoplistln.i' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:45.657+03:00' */
/*prosignrXBgqLEvB84NpQRtLksicA*/