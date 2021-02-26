/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: flt_op_not_kau.p
      Comment: предустановленный фильтр отбора документов по следующим критериям:
              -	Дата документа = дате операционного дня, в котором открыта смена
              - В документе присутствуют проводки по счетам, установленным на субаналитику по ШаблКау:
                    КодСменыВНБ  Код смены ВОК для внебаланса
                    КодСменыВОК  Код смены ВОК для баланса 
              -	В документе не проставлено значение доп. реквизита "код смены" (dpr-id)
   Parameters:
         Uses:
      Used by:
      Created: 24.05.2007 15:16 rija    
     Modified: 24.05.2007 16:14 rija     <comment>
*/

DEFINE INPUT  PARAMETER iOpDate AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER iType   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iLevel  AS CHARACTER   NO-UNDO.

{globals.i}
{intrface.get xclass}
{intrface.get kau}
{intrface.get cm}
{intrface.get tparam}
{intrface.get tmess}

DEFINE TEMP-TABLE ttop NO-UNDO LIKE op.

DEFINE BUFFER op       FOR op.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER acct-db  FOR acct.
DEFINE BUFFER acct-cr  FOR acct.
DEFINE BUFFER sessions FOR sessions.   

DEFINE VARIABLE mKau-IdDb     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKau-IdCr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDpr-id       AS INT64     NO-UNDO.
DEFINE VARIABLE mBranchId     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBranchId0500 AS CHARACTER NO-UNDO.
DEFINE VARIABLE IsMultiHran   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mZavKas       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mPeredDokZk   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mDepository   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDepoCr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDepoDb       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSkipMask     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVech-cr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVech-db      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVech         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKontrPrSmen  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mFrameNum     AS CHARACTER NO-UNDO.

ASSIGN
   IsMultiHran  = FGetSetting("МножХран",?,"Нет") EQ "Да"
   mSkipMask    = FGetSetting("СчПереоц","","")
   mPeredDokZk  = FGetSetting("ПередДокЗК",?,"Да") EQ "Да"
   mDepository  = STRING(GetDepository(?,?,?))
   mKontrPrSmen = FGetSetting("КонтрПрСмены",?,"Да") EQ "Да" 
   mDpr-id      = INT64(tGetParam("dpr-id", "vok", ""))
   mFrameNum    = FGetSetting("AcceptFrmNum",?, "11")
   NO-ERROR.

FIND FIRST sessions WHERE
           sessions.dpr-id EQ mDpr-id
   NO-LOCK NO-ERROR.
IF AVAIL sessions AND sessions.dpr-status NE "ОТКРЫТА" THEN
DO:
   RUN Fill-SysMes ("","","0",'Смена не находится в статусе "ОТКРЫТА"').
   {intrface.del}
   RETURN.
END.

IF AVAIL sessions THEN 
   ASSIGN
      mBranchId = sessions.branch-id
      mZavKas   = GetXattrValue("sessions",STRING(sessions.dpr-id),"ЗавКассой") EQ "Да"
      mVech     = GetXattrValue("sessions",STRING(sessions.dpr-id),"ПрСмены")
   .
ELSE
   ASSIGN
      mBranchId = GetXattrValueEx("_user",USERID("bisquit"),"Отделение",shfilial)
      mZavKas   = ?      
      mVech     = ?   
   .

/* MESSAGE iOpDate mBranchID view-as alert-box. */
IF mBranchID = "0518"
   THEN mBranchID0500 = "0500".
   ELSE mBranchID0500 = mBranchID.

xxx:
FOR EACH op WHERE op.op-date EQ iOpDate
              AND op.filial-id EQ shFilial
              AND op.op-status LT CHR(251) 
              AND ((op.branch-id EQ mBranchId) OR (op.branch-id EQ mBranchId0500))
   NO-LOCK,
   EACH op-entry OF op 
   WHERE
       NOT CAN-DO(mSkipMask,op-entry.acct-db)
   AND NOT CAN-DO(mSkipMask,op-entry.acct-cr)
   NO-LOCK:
/*if op.doc-num eq '373478' then message shfilial op.branch-id op.doc-num view-as alert-box.*/
   IF    GetXAttrValue("op",STRING(op.op),"dpr-id") NE "" 
      OR CAN-DO(mSkipMask,op-entry.acct-db) 
      OR CAN-DO(mSkipMask,op-entry.acct-cr) 
      OR CAN-FIND(FIRST ttop WHERE ttop.op EQ op.op) THEN
      NEXT xxx.

   FIND FIRST acct-db WHERE
              acct-db.acct EQ op-entry.acct-db
      NO-LOCK NO-ERROR.
   IF AVAILABLE acct-db THEN
   DO:
      RUN get-kau-id IN h_kau (acct-db.acct,
                               acct-db.currency,
                               OUTPUT mKau-IdDb).
      mDepoDb  = GetXattrValueEx("acct",acct-db.acct + "," + acct-db.currency,"depository","0"). 
      mVech-db = IF GetXattrValueEx("acct",acct-db.acct + "," + acct-db.currency,"ВечерКас","Нет") EQ "Да" THEN "В"
                                                                                                           ELSE "Д".                              
   END.
   ELSE
      ASSIGN
         mKau-IdDb = ?
         mDepoDb   = ?
         mVech-db  = ?
      .

   FIND FIRST acct-cr WHERE 
              acct-cr.acct EQ op-entry.acct-cr
      NO-LOCK NO-ERROR.
   IF AVAILABLE acct-cr THEN
   DO:
      RUN get-kau-id IN h_kau (acct-cr.acct,
                               acct-cr.currency,
                               OUTPUT mKau-IdCr).
      mDepoCr  = GetXattrValueEx("acct",acct-cr.acct + "," + acct-cr.currency,"depository","0"). 
      mVech-cr = IF GetXattrValueEx("acct",acct-cr.acct + "," + acct-cr.currency,"ВечерКас","Нет") EQ "Да" THEN "В"
                                                                                                           ELSE "Д".                                
   END.
   ELSE
      ASSIGN
         mKau-IdCr = ?
         mDepoCr   = ? 
         mVech-cr  = ?
      .
/* комент не удалять

MESSAGE "mBranchId " mBranchId SKIP 
        "mVech " mVech SKIP
        "mDepository " mDepository SKIP
        "IsMultiHran " IsMultiHran SKIP(1)
        "AVAIL acct-db " AVAIL acct-db SKIP
        "mKau-IdDb " mKau-IdDb SKIP
        "acct-db.branch-id " (IF AVAIL acct-db THEN acct-db.branch-id ELSE ? ) SKIP
        "mVech-db " mVech-db SKIP(1)
        
        "AVAIL acct-cr " AVAIL acct-cr SKIP
        "mKau-IdCr " mKau-IdCr SKIP
        "acct-cr.branch-id " (IF AVAIL acct-cr THEN acct-cr.branch-id ELSE ? ) SKIP
        "mVech-cr " mVech-cr
   VIEW-AS ALERT-BOX.
*/

   IF (    AVAIL acct-db 
       AND mKau-IdDb         BEGINS "КодСмены"
       AND acct-db.branch-id EQ mBranchId
       AND (   mVech-db EQ mVech
            OR mVech    EQ ?
            OR NOT mKontrPrSmen) 
       AND (   NOT IsMultiHran
            OR (mDepoDb EQ mDepository)))
       OR
      (    AVAIL acct-cr 
       AND mKau-IdCr         BEGINS "КодСмены" 
       AND acct-cr.branch-id EQ mBranchId
       AND (   mVech-cr EQ mVech
            OR mVech    EQ ?
            OR NOT mKontrPrSmen)
       AND (   NOT IsMultiHran 
            OR (mDepoCr EQ mDepository))) THEN
   DO:
      IF     AVAIL acct-db 
         AND AVAIL acct-cr
         AND mKau-IdDb BEGINS "КодСмены"
         AND mKau-IdCr BEGINS "КодСмены"
         AND NOT mZavKas
         AND mPeredDokZk
         AND (   acct-db.branch-id NE acct-cr.branch-id
              OR (    mKontrPrSmen
                  AND mVech-db     NE mVech-cr)
              OR (    IsMultiHran 
                  AND mDepoDb      NE mDepoCr)) THEN
      DO: 
         NEXT xxx.
      END.
   END.
   ELSE
   DO:
      NEXT xxx.
   END.   

   CREATE ttop.
   ASSIGN 
      ttop.op = op.op
      .
END.

CASE iType: 
   WHEN "op" THEN
   DO:
      RUN browseld.p ("op",
                      "FilterTable~001BrwRole~001OpF8F12",
                      STRING(TEMP-TABLE ttop:HANDLE) + "~001vok~001yes",
                      "",
                      iLevel).
      
   END.
   WHEN "op-entry" THEN
   DO:
      RUN browseld.p (
         "op",
         "FilterTable~001SetFirstFrm~001BrwRole~001FieldSort",
         STRING(TEMP-TABLE ttop:HANDLE) + "~001" + mFrameNum + "~001" + "vok" + "~001doc-num",
         "",
         iLevel
      ).
   END.
END CASE.

RETURN.
