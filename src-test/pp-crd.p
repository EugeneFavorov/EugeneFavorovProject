/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: PP-CRD.P
      Comment: Персистентная процедура работы с картотекой
   Parameters: Нет
         Uses:
      Used by:
      Created: 10.05.2006 ILVI (55429)   
     Modified: 03/03/2008 kraw (0089297) ChkAcctOnComm
     Modified: 25.03.2008 muta 0087581  Функция Chk_OrderPay_For_CBLACCT - проверяет,
                               содержится ли очередность платежа в НП ОчПлКБС
     Modified: 28.03.2008 muta 0088445  Функция CalcRealAcctPos определяет сумму
                               для списания с картотеки с учетом лимита остатка и блокировки суммы.
                               Процедура fnd_acct_op перенесена из kautools.lib.
     Modified: 25/06/2008 kraw (0094806) В CalcRealAcctPos счет может быть активным
*/

{pfuncdef
 &DefLib="CRD" 
 &Description="Персистентная процедура работы с картотекой"}

{globals.i}             /* Глобальные переменные сессии. */
{sh-defs.i}  
{intrface.get acct}  
{intrface.get kau}  
{lim-pos.i}
{debug.equ}

/* Получение из маски назначений для картотеки временной таблицы*/
PROCEDURE MakeTTContCrd.
   DEFINE INPUT  PARAMETER iLstMask AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ttContCrd.
   
   DEFINE  VARIABLE mWhere     AS CHARACTER  NO-UNDO.
   DEFINE  VARIABLE i          AS INT64    NO-UNDO.

   {empty ttContCrd}
   IF INDEX (iLstMask, "!") <> 0 THEN
   DO:
      RUN addTTCont(iLstMask). 
      RETURN "".
   END.

   DO i = 1 TO NUM-ENTRIES(iLstMask):
      mWhere = ENTRY (i, iLstMask).
      IF INDEX (mWhere, ".") <> 0 THEN
      DO:
         RUN addTTCont(mWhere). 
         NEXT.
      END.
   
      IF INDEX (mWhere, "*") =  0 THEN        /* Одно назначение   */
         RUN addCont(mWhere).
      ELSE /* Звездочка посередине */
         RUN addTTCont(mWhere). 
   END.
END PROCEDURE.

PROCEDURE addCont PRIVATE.
   DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.
   FIND FIRST ttContCrd WHERE 
              ttContCrd.contract =  iValue NO-LOCK NO-ERROR.
   IF NOT AVAIL ttContCrd THEN
   DO:
      CREATE ttContCrd.
      ttContCrd.contract = iValue.
   END.
END PROCEDURE.
   
PROCEDURE addTTCont PRIVATE. 
   DEFINE INPUT PARAMETER iMask AS CHARACTER NO-UNDO.

   DEF BUFFER contract FOR contract. /* Локализация буфера. */
      
   FOR EACH contract WHERE
            contract.contract >  ""
      AND   CAN-DO (iMask, contract.contract) NO-LOCK:
         RUN addCont(contract.contract).
   END.
END PROCEDURE.

/* Поиск балансового счета поставленного на картотеку, внебалансового счета, 
*/

PROCEDURE fnd_acct_op:
   DEFINE INPUT  PARAMETER iKau      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER bAcct     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oAcct     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oCurrency AS CHARACTER NO-UNDO.

   DEFINE VAR vIdBalDoc              AS CHARACTER NO-UNDO.

   DEFINE BUFFER buf_op        FOR op.
   DEFINE BUFFER buf_op-entry  FOR op-entry.
   DEFINE BUFFER buf_bop       FOR op.
   DEFINE BUFFER buf_bop-entry FOR op-entry.
   DEFINE BUFFER kau           FOR kau.
   
   IF NUM-ENTRIES(iKau) <> 2  THEN RETURN.

   BLKAU:
   FOR EACH kau WHERE 
            kau.kau        =  iKau
      AND  (kau.balance    >  0
      OR    kau.curr-bal   >  0)  
      NO-LOCK:
      LEAVE BLKAU.
   END.
   IF NOT AVAIL(kau) THEN RETURN.
   FIND FIRST buf_op
      WHERE buf_op.op =  INT64(ENTRY(1,iKau)) NO-LOCK NO-ERROR.
   IF NOT AVAIL buf_op THEN RETURN.

   FIND FIRST buf_op-entry OF buf_op 
      WHERE buf_op-entry.op-entry =  INT64(ENTRY(2,iKau)) NO-LOCK NO-ERROR.

   IF NOT AVAIL buf_op-entry THEN RETURN.

   vIdBalDoc = getxattrvalueex("op",
                               STRING(buf_op.op),
                               "op-bal",
                               "").
   
   FIND FIRST buf_bop WHERE 
      buf_bop.op =  INT64(vIdBalDoc) 
   NO-LOCK NO-ERROR.
   
   IF vIdBalDoc =  "" OR NOT AVAIL buf_bop THEN
   FIND FIRST buf_bop WHERE 
            buf_bop.op-transaction =  buf_op.op-transaction
      AND   buf_bop.acct-cat       =  "b"
   NO-LOCK NO-ERROR.

   FIND FIRST buf_bop-entry OF buf_bop NO-LOCK NO-ERROR.
   
   IF AVAIL buf_bop-entry THEN
      bacct = buf_bop-entry.acct-db.
   ELSE IF AVAIL buf_bop THEN
      bacct = getxattrvalueex("op",
                               STRING(buf_bop.op),
                               "acctbal",
                               "").
   ELSE 
      bacct = getxattrvalueex("op",
                               STRING(buf_op.op),
                               "acctbal",
                               "").
   ASSIGN
      oacct     = kau.acct
      oCurrency = kau.currency
   .
   IF NOT CAN-FIND(acct WHERE acct.acct     =  bacct 
                          AND acct.currency =  ocurrency) 

   THEN
      bacct = "".

   IF NOT CAN-FIND(acct WHERE acct.acct     =  oacct 
                          AND acct.currency =  ocurrency) 

   THEN
      oacct = "".

END PROCEDURE.
 
/* Определяет сумму для списания с картотеки с учетом лимита остатка и
блокировки суммы */
FUNCTION CalcRealAcctPos  RETURNS DECIMAL (
   INPUT iKau      AS CHARACTER, 
   INPUT iKauID    AS CHARACTER,
   INPUT iBAcct    AS CHARACTER,
   INPUT iBCurr    AS CHARACTER,
   INPUT iOrderPay AS CHARACTER,
   INPUT iDate     AS DATE):

   DEFINE VARIABLE mOAcct    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mBacct    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mCurrency AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mKauID    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mBlSum    AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mLimSum   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mRealPos  AS DECIMAL     NO-UNDO.
   DEFINE BUFFER bAcct FOR acct.
   DEFINE BUFFER acct  FOR acct.
   DEFINE BUFFER bop-entry FOR op-entry.

   IF {assigned iBAcct} THEN
      {find-act.i
          &acct  = iBAcct
          &curr  = iBCurr 
          &bact  = bacct
      }

   RUN fnd_acct_op(iKau,
                   OUTPUT mBAcct,
                   OUTPUT mOAcct,
                   OUTPUT mCurrency).
   IF {assigned mOAcct} AND iKauID =  "КартБлСч" THEN 
   DO:
      RUN get-kau-id in h_kau (mOAcct, mCurrency, OUTPUT mKauID).
      IF mKauID <> iKauID THEN
      DO:
         FOR FIRST bop-entry WHERE 
                  bop-entry.acct-cr >  ""
            AND   bop-entry.kau-cr  =  iKau
            AND   bop-entry.op-date <> ? 
            AND   CAN-FIND(FIRST acct WHERE 
                        acct.acct   =  bop-entry.acct-db
                  AND   acct.kau-id =  iKauID)                                  
         NO-LOCK:
            mOAcct = bop-entry.acct-db.
         END.
      END.
   END.

   IF NOT AVAIL(bacct) THEN 
      {find-act.i
          &acct  = mBAcct
          &curr  = mCurrency
          &bact  = bacct
      }

   IF AVAIL(bacct) THEN DO: 
      IF iKauID = "КартБлСч" 
         THEN mBlSum = GetBlockPosition(bacct.acct,bacct.currency, iOrderPay, iDate). 
         ELSE mBlSum = 0.

      mLimSum = GetLimitPosition(BUFFER bAcct, iDate). 

      RUN acct-pos IN h_base (bacct.acct,
                              bacct.currency,
                              iDate,
                              iDate,
                              'П').

      mRealPos = mBlSum + mLimSum - (IF bacct.currency =  "" THEN sh-bal ELSE sh-val).

      IF bacct.side =  "А" THEN
         mRealPos = -1 * mRealPos.
   END.
   IF mRealPos <= 0.0 THEN RETURN 0.0.

   IF {assigned mOAcct} THEN
   FIND FIRST kau WHERE kau.acct     =  mOAcct 
                    AND kau.currency =  mCurrency 
                    AND kau.kau      =  iKau NO-LOCK NO-ERROR.

   IF AVAIL(kau) THEN
      IF mRealPos > (IF mCurrency =  "" THEN kau.balance ELSE kau.curr-bal) THEN
         RETURN (IF mCurrency =  "" THEN kau.balance ELSE kau.curr-bal).
      ELSE RETURN mRealPos.
   ELSE RETURN 0.0.

END FUNCTION. 


FUNCTION ChkAcctOnComm RETURNS LOGICAL (INPUT iAcct AS CHARACTER):

   RETURN CAN-DO(FGetSetting("СтандТр", "СчКом", "-- пусто --"), iAcct).

END FUNCTION.

/* Если очередность платежа указана в НП ОчПлКБС, то возвращает YES, иначе NO */
FUNCTION Chk_OrderPay_For_CBLACCT RETURNS LOGICAL (INPUT iOrderPay AS CHARACTER):

   RETURN CAN-DO(FGetSetting("КартБлСч", "ОчПлКБС", ""), iOrderPay).

END FUNCTION.

/* Определяет принадлежит ли счет к картотеке 1,2,блокированных счетов */
FUNCTION ChkAcctCart RETURNS LOGICAL (
   INPUT iHAcct   AS HANDLE     /* Указатель на буффер счета. */
):
   DEF VAR vSurr   AS CHAR   NO-UNDO.
   DEF VAR vReturn AS LOG    NO-UNDO.
   DEF VAR vSchet  AS INT64    NO-UNDO.
   DEF VAR vKau    AS CHAR   NO-UNDO
               INIT "КартБВнСчет,Карт1ВнСчет,Карт2ВнСчет".
   DEF BUFFER kau FOR kau. /* Локализация буфера. */
   Block-Kau:
   DO vSchet = 1 TO NUM-ENTRIES(vKau):
      vSurr = GetXAttrValue(
                 "acct",                           
                 iHAcct:BUFFER-FIELD ("acct"):BUFFER-VALUE + "," 
                 + iHAcct:BUFFER-FIELD ("currency"):BUFFER-VALUE,
                 ENTRY(vSchet,vKau)).
      IF      vSurr <> ""
          AND CAN-FIND(FIRST kau WHERE kau.acct     =  ENTRY(1,vSurr)
                                   AND kau.currency =  ENTRY(2,vSurr)
                                   AND NOT kau.zero-bal) 
      THEN DO:
         vReturn = TRUE.
         LEAVE Block-Kau.
      END.
   END.
   RETURN vReturn.

END FUNCTION.

/* Проверка счета получателя по настроечному параметру */
FUNCTION Chk_AcctRec_For_CBLACCT RETURNS LOGICAL (INPUT iAcctRec   AS CHAR): 
   RETURN CAN-DO(FGetSetting("КартБлСч", "МаскиБлСч", ""), iAcctRec).                   
END FUNCTION.

/*Проверка картотеки при импорте ED275
на входе RECID балансового документа
на выходе 1 - можно отозвать, 2 - нельзя отзывать
*/
FUNCTION CheckED275K2 RETURN CHAR
   (INPUT inRecID AS RECID):

   DEFINE BUFFER vbop       FOR op.
   DEFINE BUFFER vbop-entry FOR op-entry.

   DEFINE VARIABLE flager AS INT64 NO-UNDO.
   DEFINE VARIABLE mTrans AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE RECID(op) =  inRecID NO-LOCK NO-ERROR.
   IF AVAIL(op) THEN
   DO:
      FIND FIRST signs WHERE signs.file-name  =  "op" AND 
                             signs.code       =  "op-bal" AND
                             signs.code-value =  STRING(op.op)
      NO-LOCK NO-ERROR.
      IF AVAIL(signs) THEN 
      DO:
         FIND FIRST vbop WHERE vbop.op =  INT64(signs.surrogate) NO-LOCK NO-ERROR.
      END.
      &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CheckK2 ", "AVAIL(vbop)" + STRING(AVAIL(vbop))).
      &ENDIF
      IF NOT AVAIL(vbop) THEN RETURN "1".
      ELSE
      DO:
         FIND FIRST vbop-entry OF vbop NO-LOCK NO-ERROR.
         IF AVAIL(vbop-entry) THEN
         DO:
            FIND FIRST kau WHERE kau.acct =  vbop-entry.acct-db AND kau.currency =  vbop-entry.currency 
                           AND INT64(ENTRY(1,kau.kau)) =  vbop.op NO-LOCK NO-ERROR.
            IF AVAIL(kau) THEN
            DO:
               &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("CheckK2 ", "kau AVAIL - kau.balance: " + STRING(kau.balance) + " " +
                                                       "kau.curr-bal: " + STRING(kau.curr-bal)).
               &ENDIF
/* Замена Плюс банк
               IF vbop-entry.currency =  "" AND vbop-entry.amt-rub >  kau.balance THEN 
               DO:
                  RETURN "По документу уже были списания.".
               END.
               ELSE IF vbop-entry.currency <> "" AND vbop-entry.amt-cur >  kau.curr-bal THEN 
               DO:
                  RETURN "По документу уже были списания.".
               END.
               IF (vbop-entry.currency =  "" AND vbop-entry.amt-rub =  kau.balance) OR 
                  (vbop-entry.currency <> "" AND vbop-entry.amt-cur =  kau.curr-bal) THEN
*/             IF TRUE THEN
/* Конец замены Плюс банк */
               DO:
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("CheckK2 ", "vbop-entry.currency:"  + vbop-entry.currency +
                                              " kau.balance:" + STRING(kau.balance) + 
                                              " kau.curr-bal:" + STRING(kau.curr-bal) + 
                                              " vbop-entry.amt-rub:" + STRING(vbop-entry.amt-rub) + 
                                              " vbop-entry.amt-cur:" + STRING(vbop-entry.amt-cur)).
                  &ENDIF
                  mTrans = FGetSetting("УФЭБС","ТрОтзыва","").
                  FIND FIRST op-kind WHERE op-kind.op-kind =  mTrans NO-LOCK NO-ERROR.
                  IF AVAIL(op-kind) THEN
                  DO:
                     FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
                     FIND FIRST acct WHERE acct.acct =  op-entry.acct-db NO-LOCK NO-ERROR.
                     IF AVAIL(acct) THEN
                     DO:
                        IF op-kind.proc =  "gcrddec1" THEN
                        DO:
                           RUN SetSysConf IN h_base ("ImpED107","Да").
                           RUN SetSysConf IN h_base ("ImpED107Acct",vbop-entry.acct-db).
                           RUN SetSysConf IN h_base ("ImpED107Curr",vbop-entry.currency).
                           RUN SetSysConf IN h_base ("ImpED107VBOP",vbop.op).
                           RUN SetSysConf IN h_base ("ImpED107Date",vbop.op-date).
                           RUN VALUE(op-kind.proc + ".p") (gend-date,RECID(op-kind)).
                           flager = INT64(GetSysConf("ImpED107Flag")).
                           &IF DEFINED(IS-DEBUG) &THEN
                           RUN dbgprint.p ("CheckK2 ", "flager: " + STRING(flager)).
                           &ENDIF
                           IF flager =  0 THEN RETURN "1".
                           ELSE RETURN "Не удалось выполнить транзакцию " + mTrans + ".".
                        END.
                        ELSE RETURN "Транзакция " + mTrans + " должна быть на процедуре gcrddec1.".
                     END.
                     ELSE RETURN "Не найден счет дебета.".
                  END.
                  ELSE
                  DO:
                     RETURN "Не найдена транзакция " + mTrans + ".".
                  END.

               END.
               ELSE
               DO:
                  &IF DEFINED(IS-DEBUG) &THEN
                  RUN dbgprint.p ("CheckK2 ", "vbop-entry.currency:"  + vbop-entry.currency +
                                              " kau.balance:" + STRING(kau.balance) + 
                                              " kau.curr-bal:" + STRING(kau.curr-bal) + 
                                              " vbop-entry.amt-rub:" + STRING(vbop-entry.amt-rub) + 
                                              " vbop-entry.amt-cur:" + STRING(vbop-entry.amt-cur)).
                  &ENDIF
                  RETURN "По документу уже были списания.".
               END.
            END.
            ELSE
            DO:
               &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("CheckK2 ", "kau not avail").
               &ENDIF
               RETURN "1".
            END.
         END.
         ELSE RETURN "1".
      END.
   END.  
   ELSE RETURN "Документ не найден.".
END.
/* $LINTFILE='pp-crd.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.955+03:00' */
/*prosignOZQfVKrg/aehLvriFN84pQ*/