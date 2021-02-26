/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: spr-cash318p.p
      Comment: Отчетная справка 318-П
   Parameters: Типы документов
         Uses:
      Used by:
      Created: 04.08.2008 20:28 elus
     Modified: 04.08.2008 20:28 elus
*/

{globals.i}
{wordwrap.def}
{intrface.get tparam}
{intrface.get sessions}
{intrface.get db2l}
{intrface.get vok}     /* Инструменты для работы с обьектами ВОК pp-vok.p */
{intrface.get tmess}    /* Инструменты обработки сообщений. */

                        /* Инициализация процесса протоколирования. */
RUN Init-SysMes IN h_tmess ("", "", "").

DEFINE INPUT PARAMETER iDocType AS CHARACTER NO-UNDO.

{svodord.def}

DEFINE VARIABLE mHeadDocType     AS CHARACTER NO-UNDO. /* типы документов включаемые в шапку */
DEFINE VARIABLE mBodyDocType     AS CHARACTER NO-UNDO. /* типы документов включаемые в тело */
DEFINE VARIABLE mChSetCodNacVal  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChCodNacVal     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLines           AS CHARACTER NO-UNDO EXTENT 14. /* Табличка */
DEFINE VARIABLE mCols            AS INT64     NO-UNDO. /* Ширина отчета */
DEFINE VARIABLE mHeadAndFootCols AS INT64     NO-UNDO.
DEFINE VARIABLE mDate            AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOtstup          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSprCash0        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mPodschDocOtch   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mRecvType        AS CHARACTER NO-UNDO EXTENT 2.

IF NUM-ENTRIES(iDocType,";") >= 2 AND NUM-ENTRIES(iDocType,";") <= 4 THEN
   ASSIGN
      mHeadDocType = ENTRY(1,iDocType,";")
      mBodyDocType = ENTRY(2,iDocType,";")
      .
ELSE
DO:
   RUN Fill-SysMes("", "", 0, "Не верное количество параметров процедуры печати.").
   RETURN.
END.

{ksh-defs.i}

DEFINE TEMP-TABLE ttSprCash NO-UNDO /* Данные для справки о наличности */
   FIELD Acct     LIKE Acct.acct              /* Лицевой счет */
   FIELD Bal-acct LIKE Acct.bal-acct          /* Cчет 2-го порядка */
   FIELD Currency LIKE Currency.currency      /* Код валюты */
   FIELD CurrName LIKE Currency.name-currenc  /* Наименование валюты */
   FIELD ICurrency LIKE Currency.i-currency   /* ISO-Код валюты */
   FIELD SummDb   AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* Приход */
   FIELD QtyDb    AS INT64                  /* Кол-во документов в приходе */
   FIELD SummCr   AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* Расход */
   FIELD QtyCr    AS INT64                  /* Кол-во документов в расходе */
   FIELD Balance  AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* Остаток по субсчету исх */
   FIELD In-Bal   AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* Остаток по субсчету вх */
   FIELD Dpr-Id   AS INTEGER                  /* Код смены */
   FIELD User-Id  AS CHARACTER                /* Код  User-а */
   FIELD UserName AS CHARACTER                /* Имя  User-а */
   FIELD vChBlank AS CHARACTER
   FIELD RecvOnP  AS INT64

   FIELD DocDate  AS CHARACTER
   FIELD OpDate   AS DATE

   INDEX idxCurrency Currency
   INDEX idxAcct Acct
   INDEX idxU IS UNIQUE RecvOnP Acct Bal-acct Currency Dpr-Id User-Id vChBlank
                        DocDate
   .

&GLOBAL-DEFINE FLD1 ttSprCash.Dpr-Id
&GLOBAL-DEFINE BY-FLD1 BY {&FLD1}

&GLOBAL-DEFINE FLD2 ttSprCash.Currency
&GLOBAL-DEFINE BY-FLD2 BY {&FLD2}

&GLOBAL-DEFINE FLD5 ttSprCash.RecvOnP
&GLOBAL-DEFINE BY-FLD5 BY {&FLD5}

&GLOBAL-DEFINE FLD3 ttSprCash.User-Id
&GLOBAL-DEFINE BY-FLD3 BY {&FLD3}

&GLOBAL-DEFINE FLD4 ttSprCash.vChBlank
&GLOBAL-DEFINE BY-FLD4 BY {&FLD4}

{agr-beg.def 
   &NameTitle = "ОТЧЕТНАЯ СПРАВКА"
   &TypeDoc   = "iDocType"
   &NameRep   = "'ОтчСправ'"}

ASSIGN
   mChSetCodNacVal = IF NUM-ENTRIES(iDocType,";") > 2 THEN ENTRY(3,iDocType,";") ELSE ?
   mChCodNacVal    = fGetSetting(mChSetCodNacVal,"","810") 
   mCodOurCur      = mChCodNacVal
   vOtstup         = IF NUM-ENTRIES(iDocType,";") > 3 THEN FILL(" ",INT(ENTRY(4,iDocType,";"))) ELSE ""   
   mSprCash0       = fGetSetting("spr-cash0","","Да") EQ "Да"
   mPodschDocOtch  = fGetSetting("ПодсчДокОтч","","Нет") EQ "Да"
   NO-ERROR.

{agr-beg.i}

&SCOPED-DEFINE format-cur-name 16

ASSIGN
   mLines[ 1] = "    ┌─" + FILL("─",{&format-cur-name})                   + 
   "─┬──────────────────────────────────┬──────────────────────────────────┬──"
    + "───────────────────────────────┐"
   mLines[ 2] = "    │ " + PADC("Наименование",{&format-cur-name}) + 
   " │              Приход              │               Расход             │  "
    + "      Фамилии, инициалы        │"
   mLines[ 3] = "    │ " + PADC("валюты",{&format-cur-name})                   + 
   " ├──────────┬───────────────────────┼──────────┬───────────────────────┤  "
    + "          и подписи            │"
   mLines[ 4] = "    │ " + FILL(" ",{&format-cur-name})                   + 
   " │количество│     сумма цифрами     │количество│     сумма цифрами     │  "
    + "        бухгалтерских          │"
   mLines[ 5] = "    │ " + FILL(" ",{&format-cur-name})                   + 
   " │ кассовых │      с указанием      │ кассовых │      с указанием      │  "
    + "          работников           │"
   mLines[ 6] = "    │ " + FILL(" ",{&format-cur-name})                   + 
   " │документов│  наименования валюты  │документов│  наименования валюты  │  "
    + "                               │"
   mLines[ 7] = "    ├─" + FILL("─",{&format-cur-name})                   + 
   "─┼──────────┼───────────────────────┼──────────┼───────────────────────┼──"
    + "───────────────────────────────┤"
   mLines[10] = "    │ " + PADC("1",{&format-cur-name})                   + 
   " │     2    │           3           │     4    │           5           │  "
    + "               6               │"
   mLines[11] = "    ├─" + FILL("─",{&format-cur-name})                   + 
   "─┼──────────┼───────────────────────┼──────────┼───────────────────────┼──"
    + "───────────────────────────────┤"
   mLines[12] = "    └─" + FILL("─",{&format-cur-name})                   + 
   "─┴──────────┴───────────────────────┴──────────┴───────────────────────┴──"
    + "───────────────────────────────┘"
   mLines[13] = "    ├─" + FILL("─",{&format-cur-name})                   + 
   "─┴──────────┴───────────────────────┴──────────┴───────────────────────┴──"
    + "───────────────────────────────┤"
   mLines[14] = "    ├─" + FILL("─",{&format-cur-name})                   + 
   "─┬──────────┬───────────────────────┬──────────┬───────────────────────┬──"
    + "───────────────────────────────┤"

   mCols            = LENGTH(vOtstup + mLines[1])
   mHeadAndFootCols = mCols - 6
   mRecvType[1] = "по документам, составленным на бумажном носителе:"
   mRecvType[2] = "по документам в электронном виде:"
   .
&GLOBAL-DEFINE cols mCols

RUN CreateTTRecIdOp.
RUN CreateTemp-Table.

/* Остаток по счету */

FOR EACH ttSprCash
NO-LOCK:
   {find-act.i &acct=ttSprCash.acct &curr=ttSprCash.currency}
   IF AVAILABLE acct THEN
      ttSprCash.Balance = IF acct.side EQ "А" THEN
                             (ttSprCash.In-Bal + ttSprCash.SummDb - ttSprCash.SummCr)
                          ELSE
                             (ttSprCash.In-Bal - ttSprCash.SummDb + ttSprCash.SummCr).
END.

{agr-end.i 
   &OnePageRep  = "YES"
   &OnePageName = "PrintSprCash"}

RUN End-SysMes IN h_tmess.

{intrface.del}

/* ========================== Procedure block ============================ */

PROCEDURE PrintSprCash.

   DEFINE VARIABLE vCodOurCur AS CHARACTER INITIAL "810" NO-UNDO. /* Код национальной валюты */

   DEFINE BUFFER bttSprCash FOR ttSprCash.
   DEFINE BUFFER bTTSvodOrd FOR TTSvodOrd.

/* Параметры вывода суммы прописью */
   &GLOB   STR_SUM_NUM 12  /* Кол-во строк */
   DEFINE VARIABLE vIntStr AS CHARACTER /* целая часть суммы прописью */
      EXTENT {&STR_SUM_NUM} NO-UNDO.
   DEFINE VARIABLE vDecStr AS CHARACTER FORMAT "x(2)" NO-UNDO. /* дробная часть суммы прописью */
   DEFINE VARIABLE vQtyDb  AS INT64 NO-UNDO.
   DEFINE VARIABLE vQtyCr  AS INT64 NO-UNDO.
   DEFINE VARIABLE vRepDate AS CHARACTER NO-UNDO.

   FOR EACH ttSprCash NO-LOCK:
      IF NOT CAN-DO(mUsDprIdLst,STRING(ttSprCash.Dpr-Id)) THEN
         DELETE ttSprCash.
   END.

   FOR EACH bttSprCash
      NO-LOCK
      BREAK BY bttSprCash.OpDate:
      IF FIRST-OF(bttSprCash.OpDate) THEN
      DO:
         vRepDate = IF {assigned mDateOtc} THEN bttSprCash.DocDate
                                           ELSE mChrDate. 
      /* ========================== Заголовок отчёта =========================== */
         {head-318p.i
            &CodForm = "'0402112'"
         }
         {orgname318p.i
            &CurBranchName = mBranchName
         }
      
         PUT UNFORMATTED
            PADC("ОТЧЕТНАЯ СПРАВКА",{&Cols}) SKIP
            PADC(vRepDate          ,{&Cols}) SKIP(1).
      /* Заполняем получено для совершения операций */
         PUT UNFORMATTED 
            vOtstup SPACE(3) "Получено для совершения операций в сумме "  SKIP
            vOtstup SPACE(3) "(суммах) с указанием наименования валюты:" SKIP
            SKIP(1). 
         
         FOR EACH ttSprCash WHERE ttSprCash.DocDate EQ bttSprCash.DocDate 
            NO-LOCK
            BREAK {&BY-FLD4} {&BY-FLD2}:
      
            ACCUMULATE 
               ttSprCash.In-Bal (TOTAL {&BY-FLD2})
               .
            IF LAST-OF({&FLD2}) THEN
            DO:
               /* Создание строк суммы прописью */
               RUN x-amtstr.p (ABSOLUTE(ACCUM TOTAL  {&BY-FLD2} ttSprCash.In-Bal),
                               ttSprCash.Currency,
                               YES,
                               YES,
                               OUTPUT vIntStr[1],
                               OUTPUT vDecStr).
              
               /* Вместо '' печатаем код нац. валюты */
               IF ttSprCash.Currency EQ "" THEN
                  vCodOurCur = mCodOurCur.
               ELSE
                  vCodOurCur = ttSprCash.Currency.
              
               /* Собираем в одну строку все, что хотим вывести на печать */
               vIntStr[1] = STRING(ttSprCash.CurrName,"x({&format-cur-name})") + " " + 
                            STRING(ACCUM TOTAL {&BY-FLD2} 
                            ttSprCash.In-Bal,"->,>>>,>>>,>>9.99") +
                            " " + ttSprCash.icurrency + " / " + 
                            vIntStr[1] + " " + vDecStr.
              
               {wordwrap.i
                 &s = vIntStr
                 &n = {&STR_SUM_NUM}
                 &l = mHeadAndFootCols
               }
               DO i = 1 TO {&STR_SUM_NUM}:
                  IF vIntStr [i] NE "" THEN
                     PUT UNFORMATTED vOtstup SPACE(3) vIntStr [i] SKIP.
               END.
               PUT UNFORMATTED vOtstup SPACE(3) FILL("─", mHeadAndFootCols) SKIP.
            END.
         END. /* FOR EACH ttSprCash */
      
         PUT UNFORMATTED PADC("(цифрами и прописью)",{&Cols}) SKIP(3).
      
      
      /* ========================== Шапка таблички ============================= */
         DO i = 1 TO 10:
            IF mLines[i] NE "" THEN
            PUT UNFORMATTED vOtstup mLines[i] SKIP.
         END.

         /* Заполняем справку */
         FOR EACH ttSprCash WHERE 
                  ttSprCash.DocDate   EQ bttSprCash.DocDate
                  AND (   ttSprCash.QtyDb NE 0
                   OR ttSprCash.QtyCr NE 0
                   OR mSprCash0 
				   OR NOT mPodschDocOtch )
            NO-LOCK
            BREAK {&BY-FLD5} {&BY-FLD4} {&BY-FLD1} {&BY-FLD2} {&BY-FLD3}:
            IF FIRST-OF( {&FLD5} ) THEN
               PUT UNFORMATTED
                  vOtstup mLines[13] SKIP 
                  vOtstup "    │ " PADR(mRecvType[{&FLD5}],LENGTH(mLines[1]) - 7) 
                  "│" SKIP
                  vOtstup mLines[14] SKIP.
            ACCUMULATE
               ttSprCash.QtyDb  (TOTAL {&BY-FLD3})
               ttSprCash.SummDb (TOTAL {&BY-FLD3})
               ttSprCash.QtyCr  (TOTAL {&BY-FLD3})
               ttSprCash.SummCr (TOTAL {&BY-FLD3})
               .
            IF LAST-OF({&FLD3}) THEN
            DO:
               /* Вместо '' печатаем код нац. валюты */
               IF ttSprCash.Currency EQ "" THEN
                  vCodOurCur = mCodOurCur.
               ELSE
                  vCodOurCur = ttSprCash.Currency.

               vQtyDb = (ACCUM TOTAL {&BY-FLD3} ttSprCash.QtyDb).
               vQtyCr = (ACCUM TOTAL {&BY-FLD3} ttSprCash.QtyCr).

               IF mCashOrd THEN
               DO:
                  {empty RecIdOp}
                  FOR EACH RecIdOpDate WHERE 
                           RecIdOpDate.DocDate EQ bttSprCash.DocDate
                     NO-LOCK:
                     CREATE
                        RecIdOp
                        .
                     ASSIGN
                        RecIdOp.op = RecIdOpDate.op 
                        .
                  END.
                  RUN getcashtt.p(TABLE RecIdOp,OUTPUT TABLE TTSvodOrd,mUsDprIDLst).
				  TTSvod:
                  FOR EACH TTSvodOrd WHERE 
                           TTSvodOrd.DocCur EQ ttSprCash.currency
                     NO-LOCK:
                     IF TTSvodOrd.OrdType EQ "приходный" THEN
                        vQtyDb = vQtyDb - TTSvodOrd.qty.
                     ELSE
                     DO:
                        IF     mPodschDocOtch 
                           AND CAN-FIND(FIRST bTTSvodOrd WHERE 
                                              bTTSvodOrd.OrdType EQ "приходный"
                                          AND bTTSvodOrd.OpLst   EQ TTSvodOrd.OpLst) THEN
                           NEXT TTSvod.
                        vQtyCr = vQtyCr - TTSvodOrd.qty.
                     END.
                  END.
               END.
      
               IF NOT FIRST-OF( {&FLD5} ) THEN 
                  PUT UNFORMATTED vOtstup mLines[11] SKIP.              
               PUT UNFORMATTED vOtstup "    │ " ttSprCash.CurrName FORMAT "x({&format-cur-name})"  " │ "
                  vQtyDb                                      FORMAT ">>>>>>>9" " │ "
                  (ACCUM TOTAL {&BY-FLD3} ttSprCash.SummDb)   
                  FORMAT "->>>>>,>>>,>>9.99" " " ttSprCash.icurrency " │ " 
                  vQtyCr                                      FORMAT ">>>>>>>9" " │ "
                  (ACCUM TOTAL {&BY-FLD3} ttSprCash.SummCr)   
                  FORMAT "->>>>>,>>>,>>9.99" " " ttSprCash.icurrency " │ "
                  STRING(IF ttSprCash.user-id NE "" THEN GetUserName(ttSprCash.User-Id) ELSE "","x(31)")  " │ " SKIP.
            END.
         END. /* FOR EACH ttSprCash */
      
         PUT UNFORMATTED 
            vOtstup mLines[12] SKIP(2)
            vOtstup SPACE(3) "Остаток на конец дня в сумме " SKIP
            vOtstup SPACE(3) "(суммах) с указанием наименования валюты: " 
            SKIP(1).
      /* Заполняем остаток на конец дня */
         FOR EACH ttSprCash WHERE ttSprCash.DocDate EQ bttSprCash.DocDate
            NO-LOCK
            BREAK {&BY-FLD4} {&BY-FLD2}:
            ACCUMULATE 
               ttSprCash.Balance (TOTAL {&BY-FLD2})
               .
            IF LAST-OF({&FLD2}) THEN
            DO:
               /* Создание строк суммы прописью */ 
               RUN x-amtstr.p (ABSOLUTE(ACCUM TOTAL {&BY-FLD2} ttSprCash.Balance), 
                               ttSprCash.Currency, 
                               YES, 
                               YES,
                               OUTPUT vIntStr[1],
                               OUTPUT vDecStr).
              
               /* Вместо '' печатаем код нац. валюты */
               IF ttSprCash.Currency EQ "" THEN
                  vCodOurCur = mCodOurCur.
               ELSE
                  vCodOurCur = ttSprCash.Currency.
      
               /* Собираем в одну строку все, что хотим вывести на печать */
               vIntStr[1] = STRING(ttSprCash.CurrName,"x({&format-cur-name})") + " " + 
                            STRING(ACCUM TOTAL {&BY-FLD2} 
                            ttSprCash.Balance,"->,>>>,>>>,>>9.99") +
                            " " + ttSprCash.icurrency + " / " + 
                            vIntStr[1] + " " + vDecStr.
      
               {wordwrap.i
                 &s = vIntStr
                 &n = {&STR_SUM_NUM}
                 &l = mHeadAndFootCols
               }
               DO i = 1 TO {&STR_SUM_NUM}:
                  IF vIntStr [i] NE "" THEN
                     PUT UNFORMATTED vOtstup SPACE(3) vIntStr [i] SKIP.
               END.
               PUT UNFORMATTED vOtstup SPACE(3) FILL("─", mHeadAndFootCols) SKIP.
            END.
         END. /* FOR EACH ttSprCash */
         
         PUT UNFORMATTED PADC("(цифрами и прописью)",{&Cols}) SKIP(3).
         RUN GetRepFioByRef(ENTRY(1,PROGRAM-NAME(2), "."),mCuBrchID,?,mCuDprID).
      
         RUN PrintFioAndPost(mFioInRep[1],mPostInRep[1],4).
         RUN PrintFioAndPost(mFioInRep[2],mPostInRep[2],4).
      END.      
   END.
END PROCEDURE. /* PrintSprCash */

PROCEDURE CreateTTRecIdOp:

   DEFINE VARIABLE i         AS INT64     NO-UNDO.
   DEFINE VARIABLE vChDpr-Id AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpTime   AS INT64     NO-UNDO.
   DEFINE VARIABLE vOpDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE vDate     AS CHARACTER NO-UNDO.

   DO i = 1 TO NUM-ENTRIES(mUsDprIDLst):
      vChDpr-Id = ENTRY(i,mUsDprIDLst).
      FOR EACH kau-entry WHERE kau-entry.kau       EQ vChDpr-Id
                           AND kau-entry.kau-id    BEGINS "КодСмены" 
                           AND kau-entry.op-status GE CHR(251)
                           AND kau-entry.op-date   EQ mCuDate
      NO-LOCK,
      FIRST op OF kau-entry
      NO-LOCK
      BREAK BY kau-entry.op:
         IF FIRST-OF(kau-entry.op) THEN
         DO:
            RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
            
            CASE mDateOtc:
               WHEN "" THEN vDate = "*".
               WHEN "*" THEN
               DO:
                  vDate = STRING(vOpDate, "99/99/9999").
                  IF NOT CAN-DO(mDateLst, vDate) THEN NEXT.            
               END.
               OTHERWISE
               DO:
                  vDate = STRING(vOpDate, "99/99/9999").
                  IF mDateOtc NE vDate THEN NEXT.
               END.
            END CASE.            
            
            FIND FIRST RecIdOpDate WHERE RecIdOpDate.op EQ op.op 
               NO-LOCK NO-ERROR.
            IF NOT AVAILABLE RecIdOpDate THEN
            DO:
               CREATE RecIdOpDate.
               ASSIGN
                  RecIdOpDate.op      = op.op
                  RecIdOpDate.DocDate = vDate
                  .
            END. /* IF NOT AVAILABLE RecIdOp */
         END.   
      END. /* FOR EACH kau-entry */
   END. /* DO i = 1 */
END PROCEDURE. /* CreateTTRecIdOp */

FUNCTION CharFTC RETURNS CHARACTER (iAcct     AS CHARACTER,
                                    iCurrency AS CHARACTER):

   DEFINE VARIABLE vChFTC     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChAcctFTC AS CHARACTER NO-UNDO.
   DEFINE BUFFER currency FOR currency.

   vChAcctFTC = GetXAttrValue("acct", iAcct + "," + iCurrency,"form-type-code").
   IF vChAcctFTC NE "" THEN
   DO:
   FIND FIRST currency WHERE currency.currency EQ iCurrency
      NO-LOCK NO-ERROR.
   vChFTC     = GetCodeName("КодТипаБланков",vChAcctFTC).

   /* Если не нашли, то ищем в sec-code (дорожные чеки) */
   IF NOT {assigned vChFTC} THEN
      vChFTC = GetSecCodeName(vChAcctFTC).
   IF (NOT {assigned vChFTC} OR vChFTC = "?") THEN
      vChFTC = "".
   ELSE
      vChFTC = (IF AVAIL currency THEN currency.name-currenc + "/ДЧ " ELSE "") + vChFTC.
   END.      
   RETURN vChFTC.

END FUNCTION. /* CharFTC */

PROCEDURE CreateTemp-Table:

   DEFINE VARIABLE vFlagOpDb     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vFlagOpCr     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vFlagOp       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vIntDpr-Id    AS INT64   NO-UNDO.
   DEFINE VARIABLE vChFTC        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChAcctFTC    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vUsDprID      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE i             AS INT64   NO-UNDO.
   DEFINE VARIABLE vLogPereschet AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vByUserId     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSesUserId    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDate         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRecvOnPaper  AS INT64     NO-UNDO.
   DEFINE BUFFER bsigns FOR signs.

   FOR EACH RecIdOpDate:
      /* Шапка */
      CASE mDateOtc:
         WHEN "" THEN vDate = "*".
         WHEN "*" THEN
         DO:
            vDate = RecIdOpDate.DocDate.
            IF NOT CAN-DO(mDateLst, vDate) THEN NEXT.
         END.
         OTHERWISE
         DO:
            vDate = RecIdOpDate.DocDate.
            IF mDateOtc NE vDate THEN NEXT.
         END.
      END CASE.
      
      FOR FIRST op WHERE op.op EQ RecIdOpDate.op
                     AND CAN-DO(mHeadDocType, op.doc-type)
      NO-LOCK,
      FIRST signs WHERE signs.file-name EQ "op" 
                    AND signs.surrogate EQ STRING(op.op)     
                    AND signs.code      EQ "Dpr-Id"
                    AND signs.code-val  NE ""
      NO-LOCK,
      EACH kau-entry OF op WHERE /*kau-entry.debit EQ YES     sku нужны расходные операции
                             AND*/ kau-entry.kau EQ (IF NOT CAN-DO(mUsDprIDLst,TRIM(signs.code-val)) THEN 
                                                     mUsDprIDLst 
                                                  ELSE 
                                                     TRIM(signs.code-val)
                                                  )
                             AND kau-entry.op-status GE CHR(251)
      NO-LOCK BY kau-entry.op:
		FIND FIRST bsigns WHERE bsigns.file-name EQ 'sessions' AND
					bsigns.surrogate EQ kau-entry.kau AND 
					bsigns.code eq 'ЗавКассой' AND
					bsigns.code-val EQ 'Да' 
			NO-LOCK NO-ERROR.
		/* sku если смена зав.кассы, то не учитываем прием от подчин. кассира внутри дня */	
		IF AVAIL bsigns AND kau-entry.op-date = op.contract-date AND kau-entry.debit EQ YES THEN NEXT.
		/* если смена зав.кассы то расходные операции учитываем только внутри дня другому кассиру */
		IF kau-entry.debit EQ NO AND 
		(AVAIL bsigns AND kau-entry.debit EQ NO AND kau-entry.op-date EQ op.contract-date) EQ NO
		THEN NEXT.
		
         ASSIGN
            mAgreeYes = TRUE
            vChFTC    = ?
            .
         
         {find-act.i &acct=kau-entry.acct &curr=kau-entry.currency}
         IF AVAILABLE acct THEN
         DO:
            vRecvOnPaper = INT64(GetXAttrValue("op",STRING(op.op),
                                 "СпособПолуч") NE "") + 1.
            vChFTC = CharFTC(acct.acct,acct.currency).
            IF NOT (vChFTC NE "" AND NOT mChBlank) THEN
            DO:
               FIND FIRST currency WHERE 
                          currency.currency EQ kau-entry.currency
               NO-LOCK NO-ERROR.
               IF AVAIL currency THEN
               DO:
                  vIntDpr-Id = INT64(kau-entry.kau).
                  IF mByInsp THEN
                  DO:
                     vByUserId  = "".
                     vSesUserId = GetBufferValue("sessions","WHERE sessions.dpr-id EQ " + GetXattrValue("op",STRING(op.op),"dpr-id"),"user-id").
					 /*ayv изменено, чтобы подтягивался кассир*/
					 vByUserId  = op.user-id.
                     /*IF vSesUserId NE op.user-id THEN
                        vByUserId  = op.user-id.
                     IF vSesUserId NE op.user-inspector THEN
                        vByUserId  = op.user-inspector.*/
					 IF vSesUserId NE op.user-inspector and op.user-inspector NE "" THEN
			            vByUserId  = op.user-inspector.
						else vByUserId  = op.user-id .
					/**/
                  END.
                  ELSE
                  DO:
                     vByUserId  = /*IF mChBlank THEN*/ op.user-id
                                              /*ELSE ""*/.
                  END.
                  FIND FIRST ttSprCash WHERE ttSprCash.Acct     EQ acct.acct  
                                         AND ttSprCash.Currency EQ acct.currency
                                         AND ttSprCash.Dpr-Id   EQ vIntDpr-Id
                                         AND ttSprCash.User-Id  EQ vByUserId 
                                         AND ttSprCash.vChBlank EQ vChFTC
                                         AND ttSprCash.DocDate  EQ vDate
                                         AND ttSprCash.RecvOnP  EQ vRecvOnPaper
                     NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE ttSprCash THEN
                  DO:
                     CREATE ttSprCash.
                     ASSIGN
                        ttSprCash.Acct     = acct.acct 
                        ttSprCash.Bal-acct = acct.bal-acct 
                        ttSprCash.Currency = currency.currency 
                        ttSprCash.RecvOnP  = vRecvOnPaper  
                        ttSprCash.CurrName = IF {assigned vChFTC} AND vChFTC NE "?" THEN 
                                                vChFTC
                                             ELSE 
                                                currency.name-currenc
                        ttSprCash.iCurrency = currency.i-currency
                     /* ttSprCash.In-Bal   = IF currency.currency EQ "" THEN 
                                                kau-entry.amt-rub
                                             ELSE
                                                kau-entry.amt-cur */
					 /* ayv исправлено в соответствии со старой версией, для верного подсчета шапки в соответствии с передачей наличных между кассирами */
						ttSprCash.In-Bal   = (IF currency.currency EQ "" THEN 
                                                kau-entry.amt-rub
                                             ELSE
                                                kau-entry.amt-cur) * (IF kau-entry.debit EQ YES THEN 1 ELSE -1)
					 /**/
                        ttSprCash.Dpr-Id   = INT64(kau-entry.kau) 
                        ttSprCash.User-Id  = vByUserId 
                        ttSprCash.vChBlank = vChFTC
                        ttSprCash.DocDate  = vDate
                        ttSprCash.OpDate   = DATE(vDate)
                        NO-ERROR.
                  END.
                  ELSE
                  DO:
                     ASSIGN
                      /*ttSprCash.In-Bal   = ttSprCash.In-Bal + (IF currency.currency EQ "" THEN 
                                                                    kau-entry.amt-rub
                                                                 ELSE
                                                                    kau-entry.amt-cur) */
					  /* ayv исправлено в соответствии со старой версией, для верного подсчета шапки в соответствии с передачей наличных между кассирами */
						ttSprCash.In-Bal   = ttSprCash.In-Bal + (IF currency.currency EQ "" THEN 
                                                                    kau-entry.amt-rub
                                                                 ELSE
                                                                    kau-entry.amt-cur) * (IF kau-entry.debit EQ YES THEN 1 ELSE -1)
					  /* ayv */
                        .
                  END. /* IF NOT AVAILABLE ttSprCash */
               END. /* IF AVAIL curreny */
            END. /* IF NOT (vChFTC NE "" AND NOT mChBlank) */
         END. /* IF AVAIL acct */
      END. /* FOR EACH op */

      /* тело */
      FOR FIRST op WHERE 
                op.op EQ RecIdOpDate.op
            AND CAN-DO(mBodyDocType, op.doc-type)
         NO-LOCK,
      FIRST signs WHERE signs.file-name EQ "op" 
                    AND signs.surrogate EQ STRING(op.op)     
                    AND signs.code      EQ "Dpr-Id"
                    AND signs.code-val  NE ""
         NO-LOCK,
      EACH kau-entry OF op WHERE kau-entry.debit EQ (IF CAN-DO(mHeadDocType, op.doc-type) THEN
                                                        NO
                                                     ELSE
                                                        kau-entry.debit
                                                    )
                             AND kau-entry.kau EQ (IF NOT CAN-DO(mUsDprIDLst,TRIM(signs.code-val)) THEN 
                                                      kau-entry.kau
                                                   ELSE 
                                                      TRIM(signs.code-val)
                                                  )
                             AND kau-entry.op-status GE CHR(251)
         NO-LOCK 
      BREAK BY kau-entry.op:
      
         IF NOT Pereschet(op.op,kau-entry.op-entry,kau-entry.debit) THEN
         DO:
            /* для подсчета количества документов */
            IF FIRST-OF(kau-entry.op) OR NOT mPodschKolDoc THEN 
               ASSIGN
                  vFlagOpDb = YES
                  vFlagOpCr = YES
				  vFlagOp   = YES
               .
            ASSIGN
               mAgreeYes = TRUE /* данные для отчета есть */
               vChFTC    = ?
               .

            {find-act.i &acct=kau-entry.acct &curr=kau-entry.currency}
            
            IF AVAILABLE acct THEN
            DO:
               vRecvOnPaper = INT64(GetXAttrValue("op",STRING(op.op),
                                    "СпособПолуч") NE "") + 1.
               vChFTC = CharFTC(acct.acct,acct.currency).
               IF NOT (vChFTC NE "" AND NOT mChBlank) THEN /* если это дч, а справка без них, то пропускаем */
               DO:
                  FIND FIRST currency WHERE 
                             currency.currency EQ kau-entry.currency
                     NO-LOCK NO-ERROR.
                  IF AVAIL currency THEN
                  DO:
                     vIntDpr-Id = INT64(kau-entry.kau).
                     IF mByInsp THEN
                     DO:
                        vByUserId  = "".
                        vSesUserId = GetBufferValue("sessions","WHERE sessions.dpr-id EQ " + GetXattrValue("op",STRING(op.op),"dpr-id"),"user-id").
						/*ayv изменено, чтобы подтягивался кассир*/
						vByUserId  = op.user-id.
						/*IF vSesUserId NE op.user-id THEN
							vByUserId  = op.user-id.
						IF vSesUserId NE op.user-inspector THEN
							vByUserId  = op.user-inspector.*/
						IF vSesUserId NE op.user-inspector and op.user-inspector NE "" THEN
							vByUserId  = op.user-inspector.
							else vByUserId  = op.user-id .
						/**/
					 END.
                     ELSE
                     DO:
                        vByUserId  = /*IF mChBlank THEN*/ op.user-id
                                                 /*ELSE ""*/.
                     END.
                     FIND FIRST ttSprCash WHERE ttSprCash.Acct     EQ acct.acct
                                            AND ttSprCash.Currency EQ acct.currency
                                            AND ttSprCash.Dpr-Id   EQ vIntDpr-Id
                                            AND ttSprCash.User-Id  EQ vByUserId
                                            AND ttSprCash.vChBlank EQ vChFTC
                                            AND ttSprCash.DocDate  EQ vDate
                                            AND ttSprCash.RecvOnP  EQ 
                                                vRecvOnPaper
                        NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE ttSprCash THEN
                     DO:
                        CREATE ttSprCash.
                        ASSIGN
                           ttSprCash.Acct     = acct.acct
                           ttSprCash.Bal-acct = acct.bal-acct
                           ttSprCash.Currency = currency.currency
                           ttSprCash.iCurrency = currency.i-currency
                           ttSprCash.RecvOnP   = vRecvOnPaper  
                           ttSprCash.CurrName = IF {assigned vChFTC} AND vChFTC NE "?" THEN
                                                   vChFTC
                                                ELSE
                                                   currency.name-currenc
                           ttSprCash.SummDb   = IF kau-entry.debit EQ YES THEN
                                                   (IF currency.currency EQ "" THEN
                                                       kau-entry.amt-rub
                                                    ELSE
                                                       kau-entry.amt-cur)
                                                ELSE
                                                   ttSprCash.SummDb
                           ttSprCash.SummCr   = IF kau-entry.debit EQ NO THEN
                                                   (IF currency.currency EQ "" THEN
                                                       kau-entry.amt-rub
                                                    ELSE
                                                       kau-entry.amt-cur)
                                                 ELSE
                                                    ttSprCash.SummCr
                           ttSprCash.QtyDb    = IF kau-entry.debit EQ YES AND vFlagOpDb THEN 1
                                                                                        ELSE 0
                           ttSprCash.QtyCr    = IF kau-entry.debit EQ NO AND vFlagOpCr  THEN 1
                                                                                        ELSE 0
                           ttSprCash.Dpr-Id   = INT64(kau-entry.kau) 
                           ttSprCash.User-Id  = vByUserId
                           ttSprCash.vChBlank = vChFTC
                           ttSprCash.DocDate  = vDate
                           ttSprCash.opDate   = DATE(vDate)
                           NO-ERROR.
                     END.
                     ELSE
                     DO:
                        ASSIGN
                           ttSprCash.SummDb   = IF kau-entry.debit THEN
                                                   (IF currency.currency EQ "" THEN
                                                       (ttSprCash.SummDb + kau-entry.amt-rub)
                                                    ELSE
                                                       (ttSprCash.SummDb + kau-entry.amt-cur))
                                                ELSE
                                                   ttSprCash.SummDb
                           ttSprCash.SummCr   = IF kau-entry.debit EQ NO THEN
                                                   (IF currency.currency EQ "" THEN
                                                       ttSprCash.SummCr + kau-entry.amt-rub
                                                    ELSE
                                                       ttSprCash.SummCr + kau-entry.amt-cur)
                                                ELSE
                                                   ttSprCash.SummCr
                           ttSprCash.QtyDb    = IF kau-entry.debit AND vFlagOpDb THEN ttSprCash.QtyDb + 1
                                                                                 ELSE ttSprCash.QtyDb
                           ttSprCash.QtyCr    = IF kau-entry.debit EQ NO AND vFlagOpCr THEN ttSprCash.QtyCr + 1
                                                                                       ELSE ttSprCash.QtyCr
                           .
                     END. /* IF NOT AVAILABLE ttSprCash */
                     IF kau-entry.debit     AND vFlagOpDb THEN vFlagOpDb = NO.
                     IF NOT kau-entry.debit AND vFlagOpCr THEN vFlagOpCr = NO.
					 IF     NOT vFlagOpCr 
                        AND NOT vFlagOpDb 
                        AND mPodschDocOtch
                        AND vFlagOp THEN
                        ASSIGN
                           ttSprCash.QtyCr = ttSprCash.QtyCr - 1
                           vFlagOp         = NO
                        .
                  END. /* IF AVAIL currency */
               END. /* IF NOT (vChFTC NE "" AND NOT mChBlank) */
            END. /* IF AVAIL acct */
         END. /* IF NOT vLogPereschet */
      END. /* FOR EACH op */
   END. /* FOR EACH RecIdOpDate */
END PROCEDURE. /* CreateTemp-Table */
/* $LINTUSER='STRE' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='22/10/2014 12:08:37.000+04:00' */
/* $LINTFILE='spr-cash318p.p' */
/*prosignONK4up3X61N1YPsLyeuVcQ*/