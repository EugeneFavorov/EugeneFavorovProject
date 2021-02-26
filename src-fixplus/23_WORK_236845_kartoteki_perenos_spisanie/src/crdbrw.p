/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"
     Filename: crdbrw.p
      Comment: Браузер для класса crd (kau)
   Parameters:
         Uses:
      Used by:
      Created: 23.09.2008 ushd 85168
     Modified: 06/07/2010 kraw (0102360) op-order
     Modified: 04/02/2011 kraw (0116612) Подсвечиваем красным приостановленные
*/
{globals.i}
{flt-file.i}
{intrface.get op}
{navigate.def}          /* Переменные для navigate.cqr. */

DEFINE VAR kau1        AS CHAR FORMAT "x(11)" NO-UNDO.
DEFINE VAR kau2        LIKE kau1              NO-UNDO.
DEFINE VAR kau3        LIKE kau1              NO-UNDO.
DEFINE VAR kau-proc    LIKE acct.kau-id INIT ? NO-UNDO.
DEFINE VAR temp-date   AS CHAR                NO-UNDO.
DEFINE VAR sort-label  AS CHAR INIT ""        NO-UNDO.
DEFINE VAR mDocDate    LIKE op.doc-date       NO-UNDO.
DEFINE VAR mDocNum     LIKE op.doc-num        NO-UNDO.
DEFINE VAR mAcct       LIKE op-entry.acct-db  NO-UNDO.
DEFINE VAR in-currency LIKE op-entry.currency NO-UNDO.
DEFINE VAR mKauRec     AS RECID               NO-UNDO.
DEFINE VAR mViewDate   AS DATE                NO-UNDO.
DEFINE VARIABLE mAmountForWriteOff AS DECIMAL NO-UNDO.
DEFINE VARIABLE mBlockType  AS CHARACTER      NO-UNDO.
DEFINE VARIABLE mHelpLabel  AS CHARACTER      NO-UNDO.

DEFINE VARIABLE mOrderPay AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpBal    AS INT64   NO-UNDO.

DEFINE VARIABLE mFixedWhere AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBY         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mNF         AS INT64   NO-UNDO.

DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.

DEFINE BUFFER buf-code FOR code.
DEFINE BUFFER tmp-code FOR code.
DEFINE BUFFER opb      FOR op.
DEFINE BUFFER opbb     FOR op.

DEFINE TEMP-TABLE ttOp
    FIELD op AS INTEGER
    FIELD sortOrder AS INTEGER.

mAcct       = GetFltValEx("acct","").
mViewDate   = DATE(GetFltValEx("view-date","?")) NO-ERROR.
mKauRec     = INT64(GetFltValEx("p-kau-rec","0")) NO-ERROR.
in-currency = GetFltValEx("currency","").

{find-act.i
   &acct = mAcct
   &curr = in-currency
}   
IF NOT AVAIL acct THEN RETURN ERROR.
IF acct.kau-id = ?
OR acct.kau-id = "" THEN DO:
   FIND bal-acct OF acct NO-LOCK.
   IF  AVAIL bal-acct
   AND bal-acct.kau-id <> ?
   AND bal-acct.kau-id <> ""
      THEN kau-proc = bal-acct.kau-id.
END.
ELSE kau-proc = acct.kau-id.

FIND tmp-code WHERE tmp-code.class = "ШаблКау"
                AND tmp-code.code  = kau-proc NO-LOCK NO-ERROR.

mBY         = IF (FGetSetting("Карт2", "ОчерСписК2", "Нет") EQ "Нет" OR kau-proc NE "Карт-ка2") 
              THEN NO ELSE YES.
mNF         = IF mBY THEN 2
                     ELSE 1.


{crdbrw.frm}

DO :

IF NUM-ENTRIES(tmp-code.misc[6]) ge 1 THEN DO:
  kau1:LABEL IN FRAME browse2  = ENTRY(1, tmp-code.misc[6]).
  IF kau1:LABEL IN FRAME browse2 = "ДатаПлан"
     THEN sort-label = "│F5 даты".
END.
IF NUM-ENTRIES(tmp-code.misc[6]) >= 2 THEN DO:
  kau2:LABEL IN FRAME browse2  = ENTRY(2, tmp-code.misc[6]).
  IF kau2:LABEL IN FRAME browse2  = "ДатаПлан"
     THEN sort-label = "│F5 даты".
END.
IF NUM-ENTRIES(tmp-code.misc[6]) >= 3 THEN DO:
  kau3:LABEL IN FRAME browse2  = ENTRY(3, tmp-code.misc[6]).
  IF kau3:LABEL IN FRAME browse2  = "ДатаПлан"
     THEN sort-label = "│F5 даты".
END.

IF in-currency = ""
   THEN kau.balance:LABEL IN FRAME browse2 = SUBSTRING("{&in-UA-AmtNC}", 1, 17).
   ELSE kau.balance:LABEL IN FRAME browse2 = SUBSTRING("{&in-UA-AmtFC}", 1, 17).

  kau.balance:LABEL IN FRAME browse2 = "Остаток для списания".
END.

IF kau1:LABEL IN FRAME browse2 NE "ДатаПлан" THEN
DO:     /* заполнение таблицы ttOp */
    FOR EACH kau WHERE
             kau.acct     EQ acct.acct
         AND kau.currency EQ acct.currency
         AND kau.zero-bal EQ NO
        NO-LOCK,
        FIRST op WHERE 
              op.op      EQ INT64(ENTRY(1,kau.kau)) 
          AND op.op-date LE gend-date
        NO-LOCK,                    
        FIRST signs WHERE
              signs.file-name EQ 'op'
          AND signs.surrogate EQ STRING(op.op)
          AND signs.code      EQ 'op-bal' 
        NO-LOCK,
        FIRST opb WHERE 
              opb.op EQ INT64(signs.xattr-value)
        NO-LOCK:

        kau1 = TRIM(ENTRY(1, kau.sort)).
        IF  SUBSTR(ENTRY(1, kau.sort),5,1) = "."
        AND SUBSTR(ENTRY(1, kau.sort),8,1) = "."
           THEN kau1 = SUBSTR(ENTRY(1, kau.sort),9,2) + "/"
                     + SUBSTR(ENTRY(1, kau.sort),6,2) + "/"
                     + SUBSTR(ENTRY(1, kau.sort),3,2).
/*
        RUN CheckSortOrder(kau1).  */
    END.
END.

{sh-defs.i}

/*-------------------------------------------------*/
/* Возвращает саму приоритетную блокировку счета */
/*-------------------------------------------------*/
FUNCTION GetMainBlock RETURNS CHARACTER (
    INPUT iKau        AS RECID,
    INPUT iOpDateTime AS DATETIME):

    DEFINE VARIABLE vBlockList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vOpBalChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vAcctDB    AS CHARACTER NO-UNDO.

    DEFINE BUFFER kau FOR kau.
    DEFINE BUFFER op-entry FOR op-entry.
    DEFINE BUFFER op FOR op.
    DEFINE BUFFER bop FOR op.
    DEFINE BUFFER acct FOR acct.
    DEFINE BUFFER BlockObject FOR BlockObject.

    FIND FIRST kau WHERE
               RECID(kau) EQ iKau
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE kau THEN
        RETURN "".

    FIND FIRST op-entry WHERE
               op-entry.op       EQ INT64(ENTRY(1,kau.kau))
           AND op-entry.op-entry EQ INT64(ENTRY(2,kau.kau))
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE op-entry THEN
        RETURN "".
    FIND FIRST op OF op-entry 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL op THEN
        RETURN "".

    /*БАЛАНСОВЫЙ ДОКУМЕНТ КАРТОТЕКИ*/
    vOpBalChar = GetXAttrValueEx("op",
                                STRING(op.op),
                                "op-bal",
                                "").

    IF vOpBalChar NE "" THEN
       FIND FIRST bop WHERE
                  bop.op EQ INT64(vOpBalChar) 
           NO-LOCK NO-ERROR.
    ELSE
      FIND FIRST bop WHERE
                 bop.op-transaction EQ op.op-transaction
             AND bop.acct-cat       EQ "b"
             AND RECID(bop)         NE RECID(op)
         NO-LOCK NO-ERROR.

    vAcctDB = GetXattrValueEX("op",STRING(IF AVAIL bop THEN bop.op ELSE op.op),"acctbal","").

    FIND FIRST acct WHERE
               acct.acct EQ vAcctDB
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE acct THEN
        RETURN "".

    /* из полученной суммы вычитаем блокированную сумму */
    FOR EACH  BlockObject WHERE
              BlockObject.class-code   EQ "BlockAcct"
         AND  BlockObject.file-name    EQ "acct"
         AND  BlockObject.surrogate    EQ acct.acct + "," + acct.currency
         AND  BlockObject.beg-datetime LE iOpDateTime
         AND (BlockObject.end-datetime EQ ?
           OR BlockObject.end-datetime GE iOpDateTime)
         AND (BlockObject.txt[1] EQ "" OR NOT CAN-DO(BlockObject.txt[1],bop.order-pay))
        NO-LOCK:
        vBlockList = SUBSTITUTE("&1,&2",vBlockList,BlockObject.block-type).
    END.

    IF CAN-DO(vBlockList,"Блок") THEN
        RETURN "Блок".
    IF CAN-DO(vBlockList,"БлокДб") AND CAN-DO(vBlockList,"БлокКр") THEN
        RETURN (IF acct.side EQ "П" THEN "БлокДб" ELSE "БлокКр").
    IF CAN-DO(vBlockList,"БлокДб") THEN
        RETURN "БлокДб".
    IF CAN-DO(vBlockList,"БлокКр") THEN
        RETURN "БлокКр".
    IF CAN-DO(vBlockList,"БлокСумм") THEN
        RETURN "БлокСумм".

    RETURN "".

END FUNCTION.

{getrest.fun}

mHelpLabel = "F8 Приоритет".

ASSIGN
  kau1:LABEL IN FRAME browse1        = kau1:LABEL IN FRAME browse2       
  kau2:LABEL IN FRAME browse1        = kau2:LABEL IN FRAME browse2       
  kau3:LABEL IN FRAME browse1        = kau3:LABEL IN FRAME browse2       
  kau.balance:LABEL IN FRAME browse1 = kau.balance:LABEL IN FRAME browse2
.

{crdbrw.qry
   &OQ = OpenQuery}

{navigate.cqr
   &file       = "kau"
   &avfile     = "kau"
   &filt       = YES
   &tmprecid   = YES

   &bf1        = " "
   &bf2        = " "
   &cf1        = "kau1 kau2 kau3 mDocDate mDocNum kau.balance "
   &cf2        = "str-recid opb.Order-Pay kau1 kau2 kau3 mDocDate mDocNum kau.balance mAmountForWriteOff mBlockType "
   &maxfrm     = 2
   &first-frm  = mNF

   &startup    = "crdbrw.st "
   &postfind   = "crdbrw.fnd "
   &look       = "crdbrw.nav "
   &join       = "anal-v.joi "
   &oth2       = "crdbrw.f2 "
   &oh5        = """ + sort-label + """
   &oth5       = "crdbrw.sw "
   &oth6       = "flt-file.f6 "
   &oth8       = "crdbrw.f8 "
   &delete     = "crdbrw.del "
   &repaint    = "crdbrw.rpt "

   &total      = "total.cqr "
   &cfld       = "kau.balance "

   &return     = "crdbrw.ret "
   &help-label = "mHelpLabel "
}

{crdbrw.qry
   &SQ = PostSelectQuery}

{intrface.del}

/*---------------------------------------*/
/* Запись значения в kau */
/*---------------------------------------*/
PROCEDURE AssignKau:
    DEFINE INPUT PARAMETER iKau   AS RECID     NO-UNDO.
    DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

    DEFINE BUFFER kau FOR kau.

    FIND FIRST kau WHERE
               RECID(kau) EQ iKau
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE kau THEN
    DO:
        MESSAGE "Запиcь в таблице kau заблокирована. " SKIP
                "Изменить приоритет невозможно"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    /* в таком кривом формате приоритет задумано хранить... */
    ENTRY(1,kau.sort) = iValue + (IF LENGTH(iValue) LE 9 THEN
                                      FILL(" ", 9 - LENGTH(iValue))
                                  ELSE
                                      "").
 /*   RUN CheckSortOrder(iValue).  */

END PROCEDURE.

/*---------------------------------------*/
/* Создание или изменение порядка сортировки (ttOp.sortOrder) */
/*---------------------------------------*/
/*PROCEDURE CheckSortOrder:
    DEFINE INPUT PARAMETER iPriority AS CHARACTER NO-UNDO.

    DEFINE BUFFER ttOp FOR ttOp.

    FIND FIRST ttOp WHERE
               ttOp.op EQ op.op
        NO-ERROR.
    IF NOT AVAILABLE ttOp THEN
    DO:
        CREATE ttOp.
        ttOp.op = op.op.
    END.
    IF iPriority EQ "Кредит" THEN
        ttOp.sortOrder = 0.
    ELSE IF iPriority EQ "Прочие" THEN
        ttOp.sortOrder = 10.
    ELSE
        ttOp.sortOrder = 20.

END PROCEDURE.
*/