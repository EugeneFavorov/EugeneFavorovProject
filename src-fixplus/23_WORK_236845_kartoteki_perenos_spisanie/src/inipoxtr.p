/*
               Банковская интегрированная система БИСквит
    COPYRIGHT: (C) 1992-2001 ТОО "Банковские информационные системы"
     FILENAME: INIPOXTR.P
      COMMENT: Создание дополнительных реквизитов, при списании с картотеки №2
      COMMENT: Реквизиты печатаются в платежном ордере и используются в ОЭД
   PARAMETERS:
         USES:
      USED BY:
      CREATED: 05/12/2003 KOSTIK
     MODIFIED:
  LAST CHANGE:
     Modified: 11.01.2006 kraw (0052869) 
     Modified: 21.02.2006 kraw (0057869) в ДР "ДатаПлДок" дата балансового документа
     Modified: 07/12/2006 kraw (0058871) НомЧПЛ и ОстПл если не заполнен Карт2Date (т. е. при частичном списании)
     Modified: 21.12.2006 kraw (0071243) НомЧПл в случае постановки на К2 из групповой транзакции 
                                            (несколько балансовых документов в транзакции)
     Modified: 02/10/2007 kraw (0077977) используем карт2date с учетом карт2kau (чтобы не взять от другой картотеки, 
                                         актуально когда списываем документ с к1 и ставим его частично на к2)
     Modified: 20/10/2008 MUTA  0089310  определение ОстПл для транзакции _CANLBOT 
     Modified: 27/08/2010 kraw (0131758) ДР "op-doc-part"
*/

{globals.i}
{intrface.get "xclass"}

DEFINE INPUT  PARAMETER iRecOp  AS RECID     NO-UNDO. /*Ссылка на балансовый
                                                        документ списания    */
DEFINE INPUT  PARAMETER iRecKau AS RECID     NO-UNDO. /*Ссылка на KAU, которое
                                                        списывается*/

DEFINE VAR numPartPayment  AS INT64     NO-UNDO.
DEFINE VAR codePayDoc      AS CHAR        NO-UNDO.
DEFINE VAR numPayDoc       AS CHAR        NO-UNDO.
DEFINE VAR DatePayDoc      AS DATE        NO-UNDO.
DEFINE VAR DatePayDocO     AS DATE        NO-UNDO.
DEFINE VAR sum-balance     AS DECIMAL     NO-UNDO.
DEFINE VAR DestPay         AS CHARACTER   NO-UNDO.
DEFINE VAR mSumm           AS DECIMAL     NO-UNDO.
DEFINE VAR mBegAmt         AS DECIMAL     NO-UNDO.
DEFINE VAR mSummSp         AS DECIMAL     NO-UNDO.
DEFINE VAR mCountSp        AS INT64       NO-UNDO.
DEFINE VAR mI              AS INT64       NO-UNDO.
DEFINE VAR mCount          AS INT64     NO-UNDO.

DEFINE VARIABLE mAcctBal  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctCorr AS CHARACTER NO-UNDO.

DEFINE VAR mCodePayDoc     AS CHARACTER NO-UNDO. /*Тип документа поставленного на картотеку.*/
DEFINE VAR mOpStatus       AS CHARACTER NO-UNDO. /*Статус, по которому отбираются документы списания*/
DEFINE VAR mDocTypeCB      AS CHARACTER NO-UNDO. /*Тип документа ЦБ балансового списания*/
DEFINE VARIABLE mCrtAcct   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpAmt AS DECIMAL NO-UNDO.

DEFINE BUFFER tmp-entry FOR op-entry.

DEFINE BUFFER bOp FOR op.
DEFINE BUFFER b-entry FOR op-entry.

DEFINE BUFFER oOp FOR op.
DEFINE BUFFER xOp FOR op.
DEFINE BUFFER o-entry FOR op-entry.
DEFINE BUFFER bf_attr FOR xattr.

DEFINE BUFFER mBlAcct  FOR acct.

   FIND FIRST op WHERE RECID(op) EQ iRecOp NO-LOCK NO-ERROR.
   FIND FIRST op-entry OF op               NO-LOCK NO-ERROR.
   FIND FIRST doc-type OF op NO-LOCK NO-ERROR.
   IF AVAIL doc-type THEN
   mDocTypeCB = doc-type.digital.
   IF NOT AVAIL op OR NOT AVAIL op-entry THEN
   RETURN.

/*Ищем суб счет по которому требуется собрать информацию */
   IF iRecKau NE 0 AND iRecKau NE ? THEN
      FIND FIRST kau WHERE RECID(kau) EQ iRecKau NO-LOCK NO-ERROR.
   ELSE
      RUN GetKau(iRecOp,BUFFER kau).
   IF NOT AVAIL kau THEN
   RETURN.

IF iRecKau NE 0 AND
   iRecKau NE ? THEN
ASSIGN
   mSumm  = IF op-entry.currency EQ "" THEN op-entry.amt-rub
                                       ELSE op-entry.amt-cur
   mCount = 1
.

/********************************************************/

/*Статус, по которому будем считать документы списания   */
mOpStatus = GetCodeMisc("ШаблКау",kau.kau-id,4).
IF NUM-ENTRIES(mOpStatus) GE 2 THEN
mOpStatus = ENTRY(2,mOpStatus).
IF mOpStatus EQ "" OR mOpStatus EQ ? THEN
mOpStatus = gop-status.
/********************************************************/

/*Балансовые и внебалансовые документы постановки       */
/********************************************************/
RUN GetOpPost(RECID(kau),BUFFER oOp,BUFFER bOp).
IF NOT AVAIL oOp THEN
RETURN.
IF NOT AVAIL bOp THEN
RETURN.

IF bOp.op-status GT "П" THEN
mCount = mCount + 1.

IF     op.op-date        EQ oOp.op-date 
   AND op.op-transaction EQ oOp.op-transaction
THEN
   mSumm  = 0.

ASSIGN
    mAcctBal  = GetXAttrValueEx("op", STRING(bOp.op), "acctbal", "--------")
    mAcctCorr = GetXAttrValueEx("op", STRING(bOp.op), "acctcorr", "--------")
.

FOR EACH xop WHERE xop.op-date        EQ oOp.op-date 
               AND xop.op-transaction EQ oOp.op-transaction
               AND xop.doc-num        EQ bOp.doc-num
               AND xop.op             NE Op.op
               AND xop.op             NE bOp.op
               AND xop.acct-cat       EQ "b" 
               AND CAN-FIND(FIRST signs WHERE signs.file        EQ "op"
                                          AND signs.surrogate   EQ STRING(xop.op)
                                          AND signs.code        EQ "НомДок"
                                          AND signs.xattr-value EQ op.doc-num) 
   NO-LOCK,
   EACH tmp-entry OF xop WHERE tmp-entry.acct-db EQ mAcctBal
                           AND tmp-entry.acct-cr EQ mAcctCorr NO-LOCK:
   mCount = mCount + 1.
   LEAVE.
END.

FIND FIRST o-entry OF oOp NO-LOCK NO-ERROR.
IF NOT AVAIL o-entry THEN
RETURN.
mBegAmt = IF o-entry.currency EQ ""
          THEN o-entry.amt-rub
          ELSE o-entry.amt-cur.

/*************************************************************************/

   FIND FIRST mBlacct WHERE mBlacct.acct     EQ o-entry.acct-db 
                        AND mBlacct.currency EQ o-entry.currency NO-LOCK NO-ERROR.
   mCrtAcct = "".
   /*  Документ был поставлен на КартБлСч, а после переведен на Картотеку2 */
   IF AVAIL(mBlAcct) AND CAN-DO("КартБлСч,Карт-ка2", mBlacct.kau-id) THEN DO:

      FIND FIRST signs WHERE
                 signs.FILE-NAME  EQ "acct"
             AND signs.code       EQ (IF mBlacct.kau-id EQ "КартБлСч" THEN "КартБВнСчет"
                                                                      ELSE "Карт2ВнСчет")
             AND mBlacct.acct + "," + mBlacct.currency EQ (IF mBlacct.kau-id EQ "КартБлСч" THEN signs.code-value 
                                                                                           ELSE signs.xattr-value)
             NO-LOCK NO-ERROR.

      IF AVAIL(signs) THEN
         mCrtAcct = IF mBlacct.kau-id EQ "КартБлСч" THEN 
                       ENTRY(1,GetXattrValue("acct", signs.surrogate, "Карт2ВнСчет"),",")
                    ELSE
                       ENTRY(1,GetXattrValue("acct", signs.surrogate, "КартБВнСчет"),",").
   END.

/*Подсчет кол-ва списаний и суммы остатка */
   mCrtAcct = mCrtAcct                             + 
              (IF mCrtAcct EQ "" THEN "" ELSE ",") +
              o-entry.acct-db.

   DO mI = 1 TO NUM-ENTRIES(mCrtAcct):
      FOR EACH tmp-entry
         WHERE tmp-entry.acct-cr   EQ ENTRY(mI, mCrtAcct)
           AND tmp-entry.kau-cr    EQ o-entry.kau-db
           AND tmp-entry.op-date   GT o-entry.op-date - 1
           AND tmp-entry.op-status GE mOpStatus
           AND tmp-entry.kau-db    NE o-entry.kau-db   /* не учитываем проводки переноса с картотеки 2 на КартБлСч или с КартБлСч на Картотеку 2*/
 /*        AND tmp-entry.currency  EQ "" */
           AND tmp-entry.op        LE op.op
       NO-LOCK: 
          mTmpAmt = IF tmp-entry.currency EQ ""
                    THEN tmp-entry.amt-rub
                    ELSE tmp-entry.amt-cur.

         mSummSp = mSummSp + mTmpAmt.

         IF tmp-entry.op-date GT DATE(12,31,1997) THEN
            mCountSp = mCountSp + 1.
      END.
   END.

   FIND FIRST doc-type OF bOp NO-LOCK NO-ERROR.

/*Сбор требуемой информации                              */
   ASSIGN
      sum-balance     = DEC(GetXattrValue("op",STRING(bOp.op),"ОстПл")) +
                        mBegAmt -
                        mSumm   -
                        mSummSp                    
      numPartPayment  = mCount +
                        mCountSp
      DestPay         = "Частичная оплата"
      codePayDoc      = doc-type.digital WHEN AVAIL     doc-type
      codePayDoc      = bOp.doc-type     WHEN NOT AVAIL doc-type
      numPayDoc       = bOp.doc-num
      DatePayDoc      = bOp.doc-date     WHEN bOp.doc-date NE ?
      DatePayDocO     = oOp.op-date      WHEN oOp.op-date NE ?
   .
   RUN GetXattr IN h_xclass (op.class-code,"ДатаПомещенияВКарт",BUFFER bf_attr).
   IF AVAIL bf_attr THEN DO:
      UpdateSigns("op",
                  STRING(op.op),
                  "ДатаПомещенияВКарт",
                  STRING(DatePayDocO,"99/99/9999"),
                  bf_attr.indexed).
   END.

   /*Обрабатываем только платежный ордер (частичное списание с картотеки2)*/
   IF mDocTypeCB NE "16" THEN
   RETURN.

   RUN GetXattr IN h_xclass (op.class-code,"ШифрПЛ",BUFFER bf_attr).
   IF AVAIL bf_attr THEN DO:
      UpdateSigns("op",
                  STRING(op.op),
                  "ШифрПЛ",
                  STRING(codePayDoc),
                  bf_attr.indexed).
   END.
   RUN GetXattr IN h_xclass (op.class-code,"НомДок",BUFFER bf_attr).
   IF AVAIL bf_attr THEN DO:

      UpdateSigns(op.class-code,
                  STRING(op.op),
                  "op-doc-part",
                  GetXAttrValueEx("op",
                                  STRING(bop.op),
                                  "op-doc-part",
                                  STRING(bop.op)),
                  ?).

      UpdateSigns("op",
                  STRING(op.op),
                  "НомДок",
                  STRING(numPayDoc),
                  bf_attr.indexed).
   END.

   RUN GetXattr IN h_xclass (op.class-code,"ДатаПлДок",BUFFER bf_attr).
   IF AVAIL bf_attr THEN DO:
      UpdateSigns("op",
                  STRING(op.op),
                  "ДатаПлДок",
                  STRING(DatePayDoc,"99/99/9999"),
                  bf_attr.indexed).
   END.

   IF GetSysConf("Карт2Date") EQ "" OR GetSysConf("Карт2Date") EQ ? OR GetSysConf('Карт2kau') NE kau.kau-id THEN
   DO:
      RUN GetXattr IN h_xclass (op.class-code,"НомЧПЛ",BUFFER bf_attr).
      IF AVAIL bf_attr THEN
      DO:
         UpdateSigns("op",
                     STRING(op.op),
                     "НомЧПЛ",
                     STRING(numPartPayment),
                     bf_attr.indexed).
      END.
   END.

   IF GetSysConf("Карт2Date") EQ "" OR GetSysConf("Карт2Date") EQ ? OR GetSysConf('Карт2kau') NE kau.kau-id THEN
   DO:
      RUN GetXattr IN h_xclass (op.class-code,"ОстПл",BUFFER bf_attr).
      IF AVAIL bf_attr THEN
      DO:
         UpdateSigns("op",
                     STRING(op.op),
                     "ОстПл",
                     TRIM(STRING(sum-balance,"->>>>>>>>>>>>>9.99")),
                     bf_attr.indexed).
      END.
   END.


/*************************************************************************/
/*   Создание дополнительных реквизитов                  */
/*************************************************************************/

PROCEDURE GetKau:

   DEFINE INPUT PARAMETER  iRecOp AS  RECID NO-UNDO.
   DEFINE PARAMETER BUFFER bf_kau FOR kau.

   DEFINE BUFFER bf-op     FOR op.
   DEFINE BUFFER bfo-op    FOR op.
   DEFINE BUFFER bf-entry  FOR op-entry.
   DEFINE BUFFER bfo-entry FOR op-entry.
/*Поиск балансового документа списания*/
   FIND bf-op
      WHERE RECID(bf-op)   EQ iRecOp
        AND bf-op.acct-cat EQ "b"
   NO-LOCK NO-ERROR.

   IF NOT AVAIL bf-op THEN RETURN.

/*Находим внебалансовый документ списания*/
   FIND FIRST bf-entry OF bf-op NO-LOCK NO-ERROR.
   FIND FIRST bfo-op
        WHERE bfo-op.op-transaction EQ bf-op.op-transaction
          AND bfo-op.acct-cat       EQ "o"
          AND CAN-FIND(FIRST bfo-entry OF bfo-op
                       WHERE bfo-entry.amt-rub EQ bf-entry.amt-rub
                      )
   NO-LOCK NO-ERROR.
   IF NOT AVAIL bfo-op THEN RETURN.

/*Поиск документов постановки*/
/*Для поиска внебалансового документа постановки требуется найти внебалансовую проводку*/
   FIND FIRST bfo-entry OF bfo-op NO-LOCK NO-ERROR.
   IF NOT AVAIL bfo-entry THEN RETURN.
   IF bfo-entry.kau-cr EQ "" OR
      bfo-entry.kau-cr EQ ?  OR
      NUM-ENTRIES(bfo-entry.kau-cr) NE 2
   THEN
   RETURN.

   &IF DEFINED(oracle) &THEN
   FIND LAST bf_kau
        WHERE bf_kau.acct         EQ     bfo-entry.acct-cr
          AND (bf_kau.currency    EQ     bfo-entry.currency
           OR  bf_kau.currency    EQ     "")
          AND bf_kau.kau          EQ     bfo-entry.kau-cr
   NO-LOCK NO-ERROR.
   &ELSE
   FIND LAST bf_kau
        WHERE bf_kau.acct         EQ     bfo-entry.acct-cr
          AND bfo-entry.currency  BEGINS bf_kau.currency
          AND bf_kau.kau          EQ     bfo-entry.kau-cr
   NO-LOCK NO-ERROR.
   &ENDIF

END PROCEDURE.


PROCEDURE GetOpPost:
   DEFINE INPUT PARAMETER  iRecKau  AS  RECID NO-UNDO. /*Ссылка на КАУ*/
   DEFINE PARAMETER BUFFER bfo-op   FOR op.          /*Внебалансовый документ постановки*/
   DEFINE PARAMETER BUFFER bfb-op   FOR op.          /*Балансовый документ постановки*/
   DEFINE VARIABLE mOpPost AS CHARACTER NO-UNDO.     /*Номер OP балансового
                                                       документа постановки*/
   DEFINE BUFFER bf-kau     FOR kau.

   FIND FIRST bf-kau
        WHERE RECID(bf-kau) EQ iRecKau
   NO-LOCK NO-ERROR.
/* Поиск внебалансового документа постановки*/
   FIND FIRST bfo-op
      WHERE bfo-op.op EQ INT64(ENTRY(1,bf-kau.kau))
   NO-LOCK NO-ERROR.

   IF NOT AVAIL bfo-op THEN RETURN.
/* Поиск балансового документа постановки*/
   mOpPost = GetXattrValueEX("op",STRING(bfo-op.op),"op-bal","").
   IF mOpPost EQ "" THEN
      FIND LAST bfb-op
         WHERE bfb-op.op-transaction EQ bfo-op.op-transaction
           AND bfb-op.acct-cat       EQ "b"
           AND RECID(bfb-op)         NE RECID(bfo-op)
      NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST bfb-op WHERE bfb-op.op EQ INT64(mOpPost)
                                     NO-LOCK NO-ERROR.
END PROCEDURE.
/*$LINTUSER="BIS"*/
/*$LINTENV="*"*/
/*$LINTVSS="$/ws3-dpl/"*/
/*$LINTFILE="inipoxtr.p"*/
/*prosignc0GYB+CmRfPn9w69BnMsYg*/