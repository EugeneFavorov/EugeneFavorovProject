/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: crereestr.p
      Comment: Формирование  реестров платежей
   Parameters: iOpTableHdl   Неоплаченные документы
               iReesTableHdl Созданные реестры
               iOpDate       Дата опердня
               iContType     Тип реестра
         Uses:
      Used by:
      Created: 25.09.2009 ushd 110129
     Modified: 
*/
DEFINE INPUT PARAMETER iOpTableHdl   AS CHAR NO-UNDO. /* Неоплаченные документы */
DEFINE INPUT PARAMETER iReesTableHdl AS CHAR NO-UNDO. /* Созданные реестры */
DEFINE INPUT PARAMETER iOpDate       AS DATE NO-UNDO. /* Дата опердня */
DEFINE INPUT PARAMETER iContType     AS CHAR NO-UNDO. /* Тип реестра */

{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get pbase}
{exchange.equ}

&GLOBAL-DEFINE NO-BASE-PROC YES

DEFINE VAR mOpTable         AS HANDLE  NO-UNDO.
DEFINE VAR mOpBuffer        AS HANDLE  NO-UNDO.
DEFINE VAR mReesTable       AS HANDLE  NO-UNDO.
DEFINE VAR mReesBuffer      AS HANDLE  NO-UNDO.
DEFINE VAR mQuery           AS HANDLE  NO-UNDO.
DEFINE VAR mOpKind          AS CHAR    NO-UNDO.
DEFINE VAR mIsOk            AS LOGICAL.
DEFINE VAR mCustId          LIKE loan.cust-id NO-UNDO.
DEFINE VAR mIdAcct          AS CHAR           NO-UNDO.
DEFINE VAR mBranchId        LIKE branch.branch-id NO-UNDO.
DEFINE VAR mFilialId        LIKE branch.branch-id NO-UNDO.
DEFINE VAR mOp              LIKE op.op NO-UNDO.
DEFINE VAR mId              AS INT64 NO-UNDO.
DEFINE VAR mNewContCode     LIKE loan.cont-code NO-UNDO.
DEFINE VAR mNewDocNum       LIKE loan.doc-num   NO-UNDO.
DEFINE VAR mLinkSurr        AS CHAR             NO-UNDO.
DEFINE VAR mLinkCode        AS CHAR             NO-UNDO.
DEFINE VAR mReeOrder        AS INT64          NO-UNDO.
DEFINE VAR mOrder           AS INT64          NO-UNDO.
DEFINE VAR mLASurr          AS CHAR             NO-UNDO.
DEFINE VAR mLARwd           AS ROWID            NO-UNDO.
DEFINE VAR mAcct            AS CHAR             NO-UNDO.
DEFINE VAR mAcct-cr         AS CHAR             NO-UNDO.
DEFINE VAR mReceiver        AS CHAR             NO-UNDO.
DEFINE VAR mCustName        AS CHAR             NO-UNDO.
DEFINE VAR mCustINN         AS CHAR             NO-UNDO.
DEFINE VAR mLinkedOps       AS CHAR             NO-UNDO.
DEFINE VAR mInt             AS INT64          NO-UNDO.
DEFINE VAR mReeDocCnt       LIKE op-entry.amt-rub NO-UNDO.
DEFINE VAR mReeDocTrSum     LIKE op-entry.amt-rub NO-UNDO.
DEFINE VAR mReeDocComSum    LIKE op-entry.amt-rub NO-UNDO.
DEFINE VAR mTotReeDocCnt    LIKE op-entry.amt-rub NO-UNDO.
DEFINE VAR mTotReeDocTrSum  LIKE op-entry.amt-rub NO-UNDO.
DEFINE VAR mTotReeDocComSum LIKE op-entry.amt-rub NO-UNDO.
DEFINE VARIABLE mCustKpp         AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttReestr
   FIELD Order         AS INTEGER
   FIELD cust-id       LIKE mCustId
   FIELD id-acct       LIKE mIdAcct
   FIELD branch-id     LIKE mBranchId
   FIELD filial-id     LIKE mBranchId
   FIELD acct-cr       LIKE mBranchId
   FIELD contract      LIKE loan.contract
   FIELD cont-code     LIKE loan.cont-code
INDEX ByPar IS PRIMARY UNIQUE cust-id id-acct filial-id acct-cr
INDEX ByMain IS UNIQUE contract cont-code.
DEFINE BUFFER xloan-reestr FOR loan.
DEFINE BUFFER xloan-main   FOR loan.
DEFINE BUFFER xloan-acct   FOR loan-acct.
DEFINE BUFFER xop          FOR op.
DEFINE BUFFER xop-entry    FOR op-entry.
DEFINE BUFFER xlinks       FOR links.
DEFINE BUFFER xxlink       FOR xlink.
DEFINE BUFFER xcust-corp   FOR cust-corp.
DEF var iBranch  AS CHAR  NO-UNDO.
DEF var user_   AS CHAR  NO-UNDO.

user_ =  USERID("bisquit").   
iBranch = GetXattrValueEx("_user",user_,"Отделение",?).


MAIN:
DO TRANSACTION ON ERROR  UNDO MAIN, RETRY MAIN
               ON ENDKEY UNDO MAIN, RETRY MAIN
               ON STOP   UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   mOpKind = GetBaseOpKind().
   mOpTable = WIDGET-HANDLE(iOpTableHdl) NO-ERROR. {&ON-ERROR}
   mOpBuffer = mOpTable:DEFAULT-BUFFER-HANDLE.
   mReesTable = WIDGET-HANDLE(iReesTableHdl) NO-ERROR. {&ON-ERROR}
   mReesBuffer = mReesTable:DEFAULT-BUFFER-HANDLE.
   CREATE QUERY mQuery.
   mQuery:SET-BUFFERS(mOpBuffer).
   mQuery:QUERY-PREPARE("FOR EACH " + mOpBuffer:NAME).
   mQuery:QUERY-OPEN().
   mQuery:GET-FIRST().
   REPEAT:
      IF mQuery:QUERY-OFF-END THEN LEAVE.

      ASSIGN
         mCustId   = mOpBuffer:BUFFER-FIELD("cust-id"):BUFFER-VALUE
         mIdAcct   = mOpBuffer:BUFFER-FIELD("id-acct"):BUFFER-VALUE
         mBranchId = mOpBuffer:BUFFER-FIELD("branch-id"):BUFFER-VALUE
         mOp       = mOpBuffer:BUFFER-FIELD("op"):BUFFER-VALUE
      NO-ERROR. {&ON-ERROR}


if mBranchId EQ iBranch then do:
      find first op-entry where op-entry.op = mOp no-lock no-error.
      if not avail op-entry then next.
      mAcct-cr = op-entry.acct-cr.
      mFilialId = shFilial.
      FIND FIRST ttReestr WHERE ttReestr.cust-id   = mCustId
                            AND ttReestr.id-acct   = mIdAcct
                            AND ttReestr.Filial-id = mFilialId
                            AND ttReestr.Acct-cr   = mAcct-cr
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      IF NOT AVAILABLE ttReestr THEN DO:
         /* Новый реестр */
         {getfreenumber.i
            &file      = "loan"
            &field     = "cont-code"
            &prefix    = "'PNREES-'"
            &condition = " AND contract = 'pn-reestr'"
            &OutVal    = "mNewContCode"
         }
         {getfreenumber.i
            &file      = "loan"
            &field     = "doc-num"
            &numformat = "99999"
            &ordered   = YES
            &condition = " AND contract = 'pn-reestr'"
            &OutVal    = "mNewDocNum"
         }

         CREATE xloan-reestr.
         ASSIGN
            xloan-reestr.contract    = "pn-reestr"
            xloan-reestr.cont-code   = mNewContCode
            xloan-reestr.doc-num     = mNewDocNum
            xloan-reestr.class-code  = "pn-reestr"
            xloan-reestr.open-date   = TODAY
            xloan-reestr.open-time   = TIME
            xloan-reestr.cont-type   = iContType
            xloan-reestr.end-date    = iOpDate
            xloan-reestr.loan-status = "АННП"       WHEN iContType = "Аннулир"
            xloan-reestr.loan-status = "ВВЕД"       WHEN iContType = "Перечисл"
            xloan-reestr.cust-id     = mCustId      WHEN iContType = "Перечисл"
            xloan-reestr.branch-id   = mBranchId
            xloan-reestr.op-kind     = mOpKind
         NO-ERROR. {&ON-ERROR}
         VALIDATE xloan-reestr NO-ERROR. {&ON-ERROR}

         IF iContType <> "Аннулир" THEN
            UpdateSignsEx("pn-reestr", 
                          GetSurrogateBuffer("loan",(BUFFER xloan-reestr:HANDLE)),
                          "ПНСчет",
                          mIdAcct).

         mReesBuffer:FIND-LAST("USE-INDEX id", NO-LOCK) NO-ERROR.
         IF mReesBuffer:AVAILABLE
         THEN mId = mReesBuffer:BUFFER-FIELD("__filterid"):BUFFER-VALUE.
         ELSE mId = 0.
         mReesBuffer:BUFFER-CREATE() NO-ERROR. {&ON-ERROR}
         ASSIGN
            mReeOrder = mReeOrder + 1
            mReesBuffer:BUFFER-FIELD("__filterid"):BUFFER-VALUE = mId + 1
            mReesBuffer:BUFFER-FIELD("contract"):BUFFER-VALUE   = xloan-reestr.contract
            mReesBuffer:BUFFER-FIELD("cont-code"):BUFFER-VALUE  = xloan-reestr.cont-code
            mReesBuffer:BUFFER-FIELD("Order"):BUFFER-VALUE      = mReeOrder
         NO-ERROR. {&ON-ERROR}
         mReesBuffer:BUFFER-RELEASE() NO-ERROR. {&ON-ERROR}

         CREATE ttReestr.
         ASSIGN
            ttReestr.Order     = mReeOrder
            ttReestr.cust-id   = mCustId
            ttReestr.id-acct   = mIdAcct
            ttReestr.branch-id = mBranchId
            ttReestr.filial-id = mFilialId
            ttReestr.Acct-cr   = mAcct-cr
            ttReestr.contract  = xloan-reestr.contract
            ttReestr.cont-code = xloan-reestr.cont-code
         .
         RELEASE xloan-reestr.

      END.

      FIND FIRST xloan-reestr WHERE xloan-reestr.contract  = ttReestr.contract
                                AND xloan-reestr.cont-code = ttReestr.cont-code
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      ASSIGN
          mLinkCode = "reestr_pay" WHEN iContType = "Перечисл"
          mLinkCode = "reestr_ann" WHEN iContType = "Аннулир"
      .

      RUN CreateLinksRetSurr IN h_xclass ("opb-pay",
                                          mLinkCode,
                                          STRING(mOp),
                                          GetSurrogateBuffer("loan",(BUFFER xloan-reestr:HANDLE)),
                                          xloan-reestr.open-date,
                                          ?,
                                          "",
                                          OUTPUT mLinkSurr).
      IF NOT {assigned mLinkSurr} THEN DO:
         RUN Fill-SysMes("","","-1","Невозможно создать связь платежа с реестром (" + mLinkCode + ").").
         UNDO MAIN, LEAVE MAIN.
      END.
end.
      mQuery:GET-NEXT().
   END.

   mIsOk = YES.
END.
IF VALID-HANDLE(mQuery) THEN DELETE OBJECT mQuery.

/* Протокол */
IF mIsOk = YES THEN DO:
   DEFINE STREAM sProt.
   {setdest.i &stream="STREAM sProt" &cols=111}

   PUT STREAM sProt UNFORMATTED
     "┌───┬───────┬────────────────────┬────────────────────┬────────┬─────────────┬──────────┐" SKIP
     "│№  │№      │ Получатель         │ Счет получателя    │Кол-во  │   Сумма     │Сумма     │" SKIP
     "│п/п│реестра│                    │                    │платежей│   платежей  │комиссии с│" SKIP
     "│   │       │                    │                    │        │             │получателя│" SKIP
     "├───┼───────┼────────────────────┼────────────────────┼────────┼─────────────┼──────────┤"
   SKIP.

   FOR EACH ttReestr NO-LOCK,
   FIRST xloan-reestr WHERE xloan-reestr.contract   = ttReestr.contract
                        AND xloan-reestr.cont-code  = ttReestr.cont-code
                        AND xloan-reestr.class-code = "pn-reestr"
   NO-LOCK
   BY ttReestr.Order:

      /* Получатель Счет */
      ASSIGN
         mAcct     = ""
         mReceiver = ""
      .
      mIdAcct = GetXAttrValueEx("loan",xloan-reestr.contract + "," + xloan-reestr.cont-code,"ПНСчет","").
      IF {assigned mIdAcct} THEN DO:
         RUN FindSignsByVal IN h_xclass ("loan-acct", "УникКодСчета", mIdAcct, OUTPUT mOrder, OUTPUT mLASurr).
         RELEASE xloan-acct.
         IF {assigned mLASurr} THEN DO:
            mLARwd = GetRowidBySurrogate("loan-acct",mLASurr).
            FIND FIRST xloan-acct WHERE ROWID(xloan-acct) = mLARwd NO-LOCK NO-ERROR.
            IF AVAIL xloan-acct THEN mAcct = xloan-acct.acct.
         END.
         IF AVAIL xloan-acct THEN DO:
            FIND FIRST xloan-main WHERE xloan-main.contract   = xloan-acct.contract
                                    AND xloan-main.cont-code  = xloan-acct.cont-code
                                    AND xloan-main.class-code = "loan-pn"
                                    AND xloan-main.cust-id    = xloan-reestr.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL xloan-main THEN DO:
               {loan-pn-cust.i &file=xloan-main}
               mReceiver = mCustName.
            END.
         END.
      END.

      /* Количества и суммы документов */
      ASSIGN
         mReeDocCnt    = 0
         mReeDocTrSum  = 0.0
         mReeDocComSum = 0.0
      .
      mLinkedOps = GetLinks(xloan-reestr.class-code,
                            GetSurrogateBuffer("loan",(BUFFER xloan-reestr:HANDLE)),
                            "t",
                            mLinkCode,
                            CHR(1),
                            {&BQ-MAX-DATE}).
      DO mInt = 1 TO NUM-ENTRIES(mLinkedOps,CHR(1)):
         FIND FIRST xop WHERE xop.op = INT64(ENTRY(mInt,mLinkedOps,CHR(1))) NO-LOCK NO-ERROR.
         IF AVAIL xop THEN DO:
            mReeDocCnt = mReeDocCnt + 1.
            FOR EACH xop-entry WHERE xop-entry.op = xop.op NO-LOCK:
               mReeDocTrSum = mReeDocTrSum + xop-entry.amt-rub.
            END.
            /* Сумма комиссии с получателя */
            FOR EACH xxlink WHERE xxlink.class-code     = xop.class-code
                              AND xxlink.link-code      = "pn_doc_com"
                              AND xxlink.link-direction = "s"
            NO-LOCK,
            FIRST xlinks WHERE xlinks.link-id   = xxlink.link-id
                           AND xlinks.source-id = STRING(xop.op)
            NO-LOCK:
               mReeDocComSum = mReeDocComSum + DECIMAL(xlinks.link-val) NO-ERROR.
            END.
         END.
      END.

      PUT STREAM sProt UNFORMATTED
         "│" ttReestr.Order       FORMAT ">>9"
         "│" xloan-reestr.doc-num FORMAT "x(7)"
         "│" mReceiver            FORMAT "x(20)"
         "│" mAcct                FORMAT "x(20)"
         "│" mReeDocCnt           FORMAT ">>>>>>>9"
         "│" mReeDocTrSum         FORMAT ">>>>>>>>>9.99"
         "│" mReeDocComSum        FORMAT ">>>>>>9.99"
         "│"
      SKIP.

      ASSIGN
         mTotReeDocCnt    = mTotReeDocCnt    + mReeDocCnt
         mTotReeDocTrSum  = mTotReeDocTrSum  + mReeDocTrSum
         mTotReeDocComSum = mTotReeDocComSum + mReeDocComSum
      .

   END.

   PUT STREAM sProt UNFORMATTED
      "├───┴───────┴────────────────────┴────────────────────┼────────┼─────────────┼──────────┤" SKIP

      "│ Итого:                                              "
      "│" mTotReeDocCnt           FORMAT ">>>>>>>9"
      "│" mTotReeDocTrSum         FORMAT ">>>>>>>>>9.99"
      "│" mTotReeDocComSum        FORMAT ">>>>>>9.99"
      "│" SKIP

      "└─────────────────────────────────────────────────────┴────────┴─────────────┴──────────┘"
   SKIP.

   {preview.i &stream="STREAM sProt"}
END.

{intrface.del}

IF mIsOk = YES
THEN RETURN.
ELSE RETURN ERROR.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='14/04/2015 10:39:59.060+04:00' */
/* $LINTUSER='paus' */
/* $LINTMODE='1' */
/* $LINTFILE='crereestr.p' */
/*prosigngHCog9FPWnubxWW/2Z7h1w*/