/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 2000-2016 АО "Банковские информационные системы"
     Filename: stoplst-look.p
      Comment: Браузер
   Parameters: table tt-look-sl
         Uses:
      Used by:
      Created: 23.08.2013 12:31 ANBA    
     Modified: 23.08.2013 12:31 ANBA     <comment>
*/
DEFINE TEMP-TABLE tt-look-sl  NO-UNDO
            FIELD rec         AS ROWID
            FIELD matchstr    AS CHAR
INDEX rec rec.

DEFINE INPUT-OUTPUT PARAMETER table FOR tt-look-sl.

{globals.i}
{form.def}
{intrface.get xclass}
{navigate.def}
{intrface.get cust}
{intrface.get tmess}
{wordwrap.def}
{stoplist.fun }

DEF VAR in-class     AS CHAR  NO-UNDO. 
DEF VAR in-parent    AS CHAR  NO-UNDO. 
DEF VAR in-title     AS CHAR  NO-UNDO. 
DEF VAR ch           AS CHAR  NO-UNDO.
DEF VAR mDataRez     AS DATE  NO-UNDO.
DEF VAR mDataAdd     AS DATE  NO-UNDO.
DEF VAR mDataCls     AS DATE  NO-UNDO.
DEF VAR mBirthDay    AS DATE  NO-UNDO.
DEF VAR mBirthPlace  AS CHAR  NO-UNDO.
DEF VAR sv-DataRez   AS DATE  NO-UNDO.
DEF VAR sv-DataAdd   AS DATE  NO-UNDO.
DEF VAR mDataID      AS INT64 NO-UNDO.
DEF VAR mCnt         AS INT64 NO-UNDO.

DEFINE BUFFER bufcode FOR code.
&GLOBAL-DEFINE LOOK-STOPLIST YES

&GLOB defquery DEFINE QUERY qry0 FOR tt-look-sl, code scrolling.
&GLOB oqry0    OPEN QUERY qry0    FOR EACH tt-look-sl,  FIRST code WHERE ROWID(code) =  tt-look-sl.rec     NO-LOCK.

&GLOB avfile "tt-look-sl "
&GLOB qry         qry0
&GLOB tmprecid    YES
&GLOB local-recid YES



{cl-stoplst.frm }   

{navigate.cqr
   &maxoq     = 1
   &workfile  = "/* "
   &file      = tt-look-sl
   &files     = "tt-look-sl code"
   &bf1       = "tt-look-sl.matchstr code.description[1] code.misc[1] code.misc[2]  code.misc[3] code.misc[4] code.misc[5] CODE.misc[6] mBirthDay mBirthPlace CODE.misc[7] code.name code.val  mDataRez mDataAdd code.misc[8] "
   &nodel     = "/* "     
   &look      = "cl-stoplst-brw.nav " 
 }
{intrface.del}
/* $LINTFILE='stoplst-look.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:45.659+03:00' */
/*prosignbG9NrKJWDdaUToYNdSw5AQ*/