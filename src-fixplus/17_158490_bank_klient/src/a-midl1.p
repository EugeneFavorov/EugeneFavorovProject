/*
                Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename:  a-midl1.p
      Comment:  Ввод не сложных, однострочных документов (g-midl.p)
                с возможностью задания формул расчета эквивалента
   Parameters: нет
         Uses:  -
      Used BY:  opkindnav.p
      Created:  03/01/1998 Peter FROM g-midl.p
     Modified: 11.11.2002 15:24 SEMA     по заявке 0011932 ф.CreateFrmField вынесена в frmfield.fun
     Modified: 
*/

&GLOBAL-DEFINE def-stream-log YES /* временно */
&GLOBAL-DEFINE NoDefCounter   YES
&GLOBAL-DEFINE ie             YES 
&GLOBAL-DEFINE no-disp        YES 

DEFINE INPUT  PARAM in-op-date LIKE op.op-date NO-UNDO.
DEFINE INPUT  PARAM oprid      AS recid  NO-UNDO.
DEFINE OUTPUT PARAM orid       AS CHAR   NO-UNDO.

DEF VAR mCntNew   AS INT64   NO-UNDO.
DEF VAR vOrderPay AS CHAR    NO-UNDO.
DEF BUFFER b-op1 FOR op.
DEF BUFFER b-op2 FOR op.
DEF BUFFER b-op-entry1 FOR op-entry.
DEF BUFFER b-op-entry2 FOR op-entry.

{globals.i}
{form.def}
{g-trans.equ}
{debug.equ}
{intrface.get xclass}
{intrface.get swi} 
{intrface.get import}
{intrface.get op}
{intrface.get tmcod}
{intrface.get msg}
{intrface.get tag}
{intrface.get inf}
{intrface.get next}
{intrface.get blkob}
{intrface.get instrum}
{intrface.get trans}
{intrface.get pbase}
{intrface.get data}
{intrface.get db2l}
{intrface.get strng}

{g-defs.i}
{g-error.def}
{def-wf.i new}
{defframe.i new}
{wordwrap.def}
{imp-tt.def}   /* для реестра импорта */
{g-cycle.def}
{chkacces.i}

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("a-midl1.p","START:").
&ENDIF
DEF SHARED VAR debug AS INT64 NO-UNDO.               /* > 0 включена отладка */

DEF VAR acctkey        AS INT64    NO-UNDO.
DEF VAR buf2           AS CHAR   NO-UNDO.
DEF VAR counter        AS INT64    NO-UNDO. /*для delitem.i*/
DEF VAR dval           LIKE op-entry.value-date NO-UNDO.
DEF VAR err-class      AS CHAR   NO-UNDO INIT "mess-error".
DEF VAR fmt            AS CHAR   NO-UNDO.
DEF VAR fler           AS log    NO-UNDO.
DEF VAR fl-err         AS INT64    NO-UNDO INIT -1.
DEF VAR hproc          AS handle NO-UNDO.
DEF VAR in-Cont-Code   AS CHAR   NO-UNDO.
DEF VAR in-Contract    AS CHAR   NO-UNDO.
DEF VAR mCount         AS INT64    NO-UNDO.   /* счетчик */
DEF VAR mStrSigns      AS CHAR   NO-UNDO.
DEF VAR mTmpDet        AS CHAR   NO-UNDO.  /*назначение платежа*/
DEF VAR msg            AS CHAR   NO-UNDO FORMAT "x(40)".
DEF VAR mforeq         AS log    NO-UNDO.
DEF VAR mFlagTrans     AS LOG    NO-UNDO. /*Флаг транзитного документа*/
DEF VAR l-currency     LIKE currency.currency NO-UNDO.
DEF VAR l-acct         LIKE acct.acct         NO-UNDO.
DEF VAR l-course       AS dec    NO-UNDO.
DEF VAR l-course2      AS CHAR   NO-UNDO.
DEF VAR l-course-init  AS CHAR   NO-UNDO INIT "norate".
DEF VAR l-Plateg       AS CHAR   NO-UNDO.
DEF VAR l-amt-cur      LIKE op-entry.amt-cur  NO-UNDO.
DEF VAR l-amt-rub      LIKE op-entry.amt-rub  NO-UNDO.
DEF VAR l-contract-cr2 AS CHAR NO-UNDO.
DEF VAR nacct-db       AS CHAR   NO-UNDO.
DEF VAR nacct-cr       AS CHAR   NO-UNDO.
DEF VAR need-valdate   AS log    NO-UNDO FORMAT "Дата валютирования/".
DEF VAR pre-op-kind    AS CHAR   NO-UNDO.
DEF VAR OutStrCode     AS CHAR   NO-UNDO.
DEF VAR OutStrVal      AS CHAR   NO-UNDO.
DEF VAR ok             AS log    NO-UNDO.
DEF VAR result         AS INT64    NO-UNDO.
DEF VAR std-fmt        AS CHAR   NO-UNDO.
DEF VAR t-details      LIKE op.details NO-UNDO.
DEF VAR temp-acct      AS CHAR   NO-UNDO.
DEF VAR TmpStr         AS CHAR   NO-UNDO.
DEF VAR TmpFormat      AS CHAR   NO-UNDO.
DEF VAR top-list       AS CHAR   NO-UNDO.
DEF VAR top-code       AS CHAR   NO-UNDO.
DEF VAR vordpay        AS CHAR   NO-UNDO.
DEF VAR vmfo           LIKE op-bank.bank-code NO-UNDO.
DEF VAR vcorr-acct     LIKE op-bank.corr-acct NO-UNDO.
DEF VAR vDtValEqDtOp   AS CHAR   NO-UNDO.        
DEF VAR vStError       AS CHAR   NO-UNDO.
DEF VAR vDigital LIKE doc-type.digital NO-UNDO.
DEF VAR main-first AS LOGICAL NO-UNDO.

DEFINE VARIABLE mRelBlk   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mGDate    AS DATE        NO-UNDO.
DEFINE VARIABLE mGDateBlk AS DATE        NO-UNDO.
DEFINE VARIABLE mGCats    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mGCatsBlk AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mAlong    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mNumOp    AS INT64       NO-UNDO.
DEFINE VARIABLE mBaseCurr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpCurr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTransComm AS CHARACTER   NO-UNDO.
DEFINE VARIABLE bacct-db2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE bacct-cr2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mFirstFlag AS LOGICAL     NO-UNDO.
DEFINE STREAM lock-log.

DEF BUFFER xxop      FOR op .
DEF BUFFER xwop      FOR wop.

{mutex-buff.def}

FIND FIRST op-kind WHERE recid(op-kind) = oprid NO-LOCK NO-ERROR.

ASSIGN
   in-contract = work-module
   top-list    = FGetSetting("МЦИ","top-list","НМ")
   top-code    = FGetSetting("МЦИ","top-code","НЕ")
   pre-op-kind = get-tt-inf-doc("1","op-kind")
   vStError    = FGetSetting("SWIFT","st-err","В")
   mAlong      = GetXAttrValueEx ("op-kind",op-kind.op-kind,"along","") = "один документ"
   mTransComm  =  GetXattrValueEx("op-kind", op-kind.op-kind, "TransComm", "")
   mFirstFlag   = YES
.

{sw-error.fnd
       &pre-op-kind   = pre-op-kind
       &pre-op-kind2  = "op-kind.op-kind"
       &pre-err-class = err-class
}

RUN "g-func.p" persistent SET hproc.


{g-currv1.i} 
{a-midl.frm &doframe=yes &row=2}
RELEASE dacct.
RELEASE cacct.

{g-trig.i &recalc-acct=Yes &wrapname=Yes}

{g-cycle.ini}

cycle:
DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
   IF RETRY THEN LEAVE.
   IF cur-op-trans <> ? AND
      NOT is_cycle
      THEN LEAVE.
   cur-op-trans = ?.
   gen:
   DO TRANS WITH FRAME opreq ON ENDKEY UNDO cycle, RETRY cycle ON ERROR UNDO, LEAVE:
      debugparser = INT64(GetXattrValueex('op-kind', op-kind.op-kind,
                                           'debugparser', '0')).
      ASSIGN
         tcur     = ?
         tacct-db = ?
         tacct-cr = ?
         tamt     = 0
         std-fmt  = op.doc-num:FORMAT IN FRAME opreq
      .
      doc:
      FOR EACH op-template OF op-kind NO-LOCK WITH FRAME opreq
      ON ENDKEY UNDO gen, LEAVE gen
      ON ERROR UNDO gen, LEAVE gen:

         mTmpDet = get-tt-inf-doc("1","details").
         RUN DelInDet IN h_swi
            (op-templ.op-kind + "," + STRING(op-templ.op-templ),
             INPUT-OUTPUT mTmpDet).
         RUN DelNoPrtDet IN h_swi (INPUT-OUTPUT mTmpDet).
         fill-tt-inf-doc("1","details",TRIM(mTmpDet)).
         ASSIGN
            op.doc-num:FORMAT IN FRAME opreq =
               GetXattrValueex('op-template',
                                  op-kind.op-kind + ',' +
                                     string(op-templ.op-templ),
                                  'doc-num', std-fmt)
            mforeq = op-templ.mfo-needed OR
                     (GetXattrValueEx('op-template',
                                       op-kind.op-kind + ',' +
                                          string(op-templ.op-templ),
                                       'МежБанк',?) = 'Да'
                     )
            need-valdate = GetXattrValueEx('op-template',
                                            op-kind.op-kind + ',' +
                                               string(op-templ.op-templ),
                                            'ДатаВал',?) = 'Да'
         .

         {a-midl.frm &dobefore=yes &wrapname=Yes}
         {transit.i}
         {a-midl.frm &dodisp=yes}
         RUN Copy4Cycle.

         IF mFirstFlag THEN 
            mFirstFlag = NO.
         ELSE
            ASSIGN op.op-error = ? NO-ERROR.

         sset:
         DO ON ERROR  UNDO, RETRY
            ON ENDKEY UNDO cycle, RETRY cycle:
            {a-midl.frm &doset=yes}
           
            IF GetXattrValueEx("op-template",
                               op-templ.op-kind + "," +
                                  STRING(op-templ.op-templ),
                               "Сотрудник",?)                  NE ? THEN
            DO:
               /*   определение сотрудника из шаблона тразакции              */
               op.user-id = GetXattrValue("op-template",
                                          op-templ.op-kind + "," +
                                             STRING(op-templ.op-templ),
                                          "Сотрудник").
               RUN op-en-stdt(?,op.op-date,op.user-id).
            END.
            RUN neod.

            {find-act.i &acct = op-entry.acct-db}
            IF AVAILABLE acct THEN 
               op.branch-id = acct.branch-id.

            vDtValEqDtOp = GetXattrValue("op-template",pre-op-kind + "," +
                           get-tt-inf-doc("1","op-template"),"DtVal-Eq-DtOp").
            
            IF NOT (op.op-date NE ?                                   AND
                    op.op-date > in-op-date                           AND
                    NOT CAN-FIND(FIRST op-date
                                 WHERE op-date.op-date EQ op.op-date) AND
                    (vDtValEqDtOp EQ "EQ" OR vDtValEqDtOp EQ "GE") 
                   ) 
            THEN
            DO:
               RUN gdateimp-blk (OUTPUT mGDateBlk, 
                                 OUTPUT mGCatsBlk).
               mRelBlk = YES.
            END.
            RUN gdateimp (OUTPUT mGDate,
                          OUTPUT mGCats).
            IF op.op-date NE ? THEN
            ASSIGN
               op.ins-date        = ChangeInsDate(op.op-date, DATE(get-tt-inf-doc("1","ins-date")))
               op.contract-date   = op.op-date
                  WHEN FGetSetting("OpDateInit", "ChangeContDate", "")  EQ "Да"
               op.op-value-date   = op.op-date
                  WHEN FGetSetting("OpDateInit", "ChangeValueDate", "") EQ "Да"
            .

            IF AVAIL op-entry THEN DO:
               {aft-temp.i &AFT-UNDO = "."}
            END.

            IF AVAIL op-entry OR
               CAN-FIND(FIRST xxop WHERE
                           xxop.op-transaction EQ op.op-transaction AND
                           recid(xxop)         NE recid(op)) THEN
               RUN Post.
            
            {rel-date.i &in-op-date = mGDate &cats = mGCats}
            IF mRelBlk AND (mGDate NE mGDateBlk) THEN
            DO:
               {rel-date.i &in-op-date = mGDateBlk &cats = mGCatsBlk}
            END.
         END. 

         {g-docnu0.i}
         RUN Prepare4Cycle.
      END. 

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("a-midl1.p","mTransComm:" + GetNullStr(mTransComm)).
      &ENDIF

      /* Документ комиссии */
      IF {assigned mTransComm} THEN DO:
         RUN InitBaseLibrary IN h_pbase ("", in-op-date, ?). /* Для транзакций вне опердня */
         IF AVAIL op THEN DO:
            RUN AddAttr2TableEx("", 0, -1, "", 0, "mAcctDb", get-tt-inf-sub-doc("1,1","acct-db")).
            RUN AddAttr2TableEx("", 0, -1, "", 0, "mOpTrans", STRING(op.op-transaction)).
            RUN AddAttr2TableEx("", 0, -1, "", 0, "mOpop", STRING(op.op)).
            RUN AddAttr2TableEx("", 0, -1, "", 0, "mSwiftDetPay", GetXattrValueEx("op", STRING(op.op), "swift-det-pay", "")).
         END.
         RUN RunTransaction(mTransComm).
      END.

      IF NUM-ENTRIES(orid) = 2 THEN DO:
         find first b-op1 where RECID(b-op1) EQ INT64(ENTRY(1,orid)) no-lock NO-ERROR.
         find first b-op2 where RECID(b-op2) EQ INT64(ENTRY(2,orid)) no-lock NO-ERROR.

         IF avail b-op1 then
            find first b-op-entry1 of b-op1 EXCLUSIVE-LOCK no-wait NO-ERROR.
         IF avail b-op2 then
            find first b-op-entry2 of b-op2 EXCLUSIVE-LOCK no-wait NO-ERROR.

         if avail b-op1 and avail b-op2 and
            avail b-op-entry1 and avail b-op-entry2 and
            GetXattrValueEx("op", STRING(b-op1.op), "swift-det-pay", "") BEGINS "BEN" AND
            b-op-entry1.currency NE "" THEN
         DO:
            assign
               b-op-entry1.amt-rub = b-op-entry1.amt-rub - b-op-entry2.amt-rub
               b-op-entry1.amt-cur = CurFromBase("Учетный",
                                                 b-op-entry1.currency,
                                                 b-op-entry1.op-date,
                                                 b-op-entry1.amt-rub)
            .
         END.

      END.

      {g-docnum.i}
   END. 
END.

{g-cycle.pro}

HIDE FRAME opreq NO-PAUSE.
DELETE PROCEDURE(hproc).
{intrface.del}          /* Выгрузка инструментария. */ 

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE Post:
   RUN parssign.p (in-op-date,
                   "op-template",
                   op-kind.op-kind + "," + string(op-templ.op-templ),
                   op-templ.class-code,
                   "op",
                   STRING(op.op),
                   op.class-code,
                   RECID(wop)).

   IF AVAIL op-entry THEN DO :
      IF tcur = ? THEN
         tcur = op-entry.currency.
      ASSIGN
         wop.op-recid = recid(op)
         wop.acct-db  = op-entry.acct-db
         wop.acct-cr  = op-entry.acct-cr
         wop.currency = op-entry.currency
         wop.amt-cur  = IF op-entry.currency <> "" THEN
                           op-entry.amt-cur
                        ELSE op-entry.amt-rub
         wop.amt-rub  = op-entry.amt-rub
      .

      RUN parssign2.p ("PARSSEN_ENTRY_",
        in-op-date,
        "op-template",
        op-kind.op-kind + "," + string(op-templ.op-templ),
        op-templ.class-code,
        "op-entry",
        STRING(op-entry.op) + "," + STRING(op-entry.op-entry),
        op-entry.class-code,
        ?).

     RUN dopclbch.p (RECID(op),in-op-date).

   END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE neod:
   ASSIGN
      buf2 = GetSysConf("telex-bss-neo2")
      buf2 = IF {assigned buf2} THEN
                buf2
             ELSE GetSysConf("telex-bss-neo1").
   IF {assigned buf2} AND
      auto AND
      GetXattrValue(IF {assigned "GetSysConf('telex-bss-neo2')"} THEN
                          "mail-user"
                       ELSE "op-kind",
                       buf2,
                       "defopdate") MATCHES "*неопред*" THEN
      DO:
         op.op-date = ?.
         RUN op-en-stdt(?,op.op-date,?).
   END.
   buf2 = "".
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE gdateimp-blk:
   DEFINE OUTPUT PARAMETER oDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oCats AS CHARACTER   NO-UNDO.
   {gdateimp.i 
      &v-op-date        = op.op-date
      &no-chk-defopdate = YES
      &add-error        = YES
      &add-to-log       = YES
      &op-error         = op.op-error
      &error-code-cls   = "err-class + ':m106'"
      &error-code-blk   = "err-class + ':m107'"
      &no-errmessage    = YES
      &rel-date         = oDate
   } 
   oCats = mCats.
   IF AVAIL op AND
      AVAIL op-entry THEN
      RUN op-en-stdt(?,op.op-date,?).
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE gdateimp:
   DEFINE OUTPUT PARAMETER oDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oCats AS CHARACTER   NO-UNDO.

   DEF BUFFER lop-date FOR op-date. /* TSL - для daylock.i*/
   DEF BUFFER lop-date-cat-lock FOR op-date-cat-lock.
   DEF VAR mCats  AS CHAR NO-UNDO.
   DEF VAR vICats AS INT64  NO-UNDO.

   IF auto AND
      GetXAttrValueEx("op-kind",op.op-kind,"defopdate","") NE "" THEN
   DO:
      FIND FIRST op-kind OF op NO-LOCK NO-ERROR.
      {gdateimp.i &v-op-date     = op.op-date
                  &no-errmessage = yes
                  &rel-date      = oDate
      }
      IF NOT AVAIL op-date OR
         NOT ok THEN
      DO:
         MESSAGE "Нет ни одного открытого дня !" VIEW-AS ALERT-BOX ERROR. PAUSE 0.
         MESSAGE "Документ создан в неопреденном дне !" VIEW-AS ALERT-BOX ERROR. PAUSE 0.
         IF AVAIL op AND
            AVAIL op-entry THEN
         DO:
            ASSIGN
               op.op-status = "А"
               op.op-date   = ?.
            RUN op-en-stdt(op.op-status,?,?).
         END.
      END.
      ELSE
      DO:
         oCats = mCats.
         op.op-date = in-op-date.
         RUN op-en-stdt(?,in-op-date,?).
      END.
   END.
END PROCEDURE.
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*---------------------------------------------------------------------------*/
PROCEDURE op-en-stdt:
   DEF INPUT PARAM ipChStat AS CHAR NO-UNDO.
   DEF INPUT PARAM ipChDate AS DATE NO-UNDO.
   DEF INPUT PARAM ipChUser AS CHAR NO-UNDO.

   DEF BUFFER xop-entry FOR op-entry.

   FOR EACH xop-entry OF op EXCLUSIVE-LOCK:
      ASSIGN
         xop-entry.op-status = IF ipChStat NE ? THEN
                                  ipChStat
                               ELSE xop-entry.op-status
         xop-entry.op-date   = ipChDate
         xop-entry.user-id   = IF ipChUser NE ? THEN
                                  ipChUser
                               ELSE xop-entry.user-id.
   END.
END PROCEDURE.
/*****************************************************************************/
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='06/03/2015 14:07:34.056+04:00' */
/* $LINTUSER='MKV' */
/* $LINTMODE='1' */
/* $LINTFILE='a-midl1.p' */
/*prosignTmNqxA1lPWJofHIiQ2ksdg*/