/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: cas-svodspr.p
      Comment: СВОДНАЯ СПРАВКА О КАССОВЫХ ОБОРОТАХ
   Parameters: Маска, код нац.валюты
         Uses:
      Used by:
      Created: 02.08.2008 16:02 elus
     Modified: 02.08.2008 16:02 elus
*/

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.

{globals.i}
{sh-defs.i}
{wordwrap.def}
{intrface.get tparam}
{intrface.get sessions}
{intrface.get instrum}
{intrface.get vok} /* Инструмент работы с обьектами ВОК */

{ksh-defs.i} /* Расчет оборотов по kau */

{svodord.def}

DEFINE TEMP-TABLE ttSprCash NO-UNDO /* Данные для справки о оборотах по счетам */
   FIELD Acct       LIKE Acct.acct              /* Лицевой счет */
   FIELD Bal-acct   LIKE Acct.bal-acct          /* Cчет 2-го порядка */
   FIELD Currency   LIKE Currency.currency      /* Код валюты */
   FIELD CurrName   LIKE Currency.name-currenc  /* Наименование валюты */
   FIELD ICurrency  LIKE Currency.i-currency    /* ISO-Код валюты */
   FIELD SummDb     AS DECIMAL                  /* Приход */
   FIELD SummRateDb AS DECIMAL                  /* Приход в пересчете*/
   FIELD QtyDb      AS INT64                    /* Кол-во документов в приходе */
   FIELD SummCr     AS DECIMAL                  /* Расход */
   FIELD SummRateCr AS DECIMAL                  /* Расход в пересчете*/
   FIELD QtyCr      AS INT64                    /* Кол-во документов в расходе */
   FIELD Balance    AS DECIMAL                  /* Остаток по субсчету исх */
   FIELD In-Bal     AS DECIMAL                  /* Остаток по субсчету вх */
   FIELD Op         AS INTEGER                  /* Номер операции */
   FIELD Debit      AS LOGICAL                  /* дб/кр */
   FIELD DocDate    AS CHARACTER
   FIELD OpDate     AS DATE
   FIELD RecvOnP    AS INT64
   FIELD DocNum     LIKE op.doc-num				/* номер документа */
   FIELD CheckM     AS LOGICAL                  /* проводка пренадлежит одному чеку*/

   INDEX idxCurrency Currency
   INDEX idxAcct     Acct
   INDEX idxOpDebit  Op debit
   INDEX idxRecvOnP RecvOnP
   INDEX idxdate DocDate
.

DEFINE TEMP-TABLE ttSvodSpr NO-UNDO /* Данные для сводной справки о кассовых оборотах */
   FIELD Currency   LIKE Currency.currency      /* Код валюты */
   FIELD ICurrency  LIKE Currency.i-currency    /* ISO-Код валюты */
   FIELD CurrName   LIKE Currency.name-currenc  /* Наименование валюты */
   FIELD SummDb     AS DECIMAL                  /* Приход */
   FIELD SummRateDb AS DECIMAL                  /* Приход в пересчете*/
   FIELD QtyDb      AS INT64                    /* Кол-во документов в приходе */
   FIELD SummCr     AS DECIMAL                  /* Расход */
   FIELD SummRateCr AS DECIMAL                  /* Расход в пересчете */
   FIELD QtyCr      AS INT64                    /* Кол-во документов в расходе */
   FIELD DocDate    AS CHARACTER
   FIELD OpDate     AS DATE
   FIELD RecvOnP    AS INT64

   INDEX idxCurrency RecvOnP Currency
   INDEX idxdate RecvOnP DocDate
.

DEFINE BUFFER bttSvodSpr FOR ttSvodSpr.
DEFINE BUFFER bTTSvodOrd FOR TTSvodOrd.
DEFINE BUFFER bCurrency  FOR Currency.

{agr-beg.def 
   &NameTitle = "СВОДНАЯ СПРАВКА О КАССОВЫХ ОБОРОТАХ"
   &TypeDoc   = '"spr"'
   &NameRep   = '"СправОбор"'} 
   
DEFINE VARIABLE mChMask         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChSetCodNacVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChCodNacVal    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLines          AS CHARACTER NO-UNDO EXTENT 16. /* Табличка */
DEFINE VARIABLE mCols           AS INT64     NO-UNDO. /* Ширина отчета */
DEFINE VARIABLE mCurDprID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOpTime         AS INT64     NO-UNDO.
DEFINE VARIABLE vOpDate         AS DATE      NO-UNDO.
DEFINE VARIABLE vDate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRecvType       AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE mNacISO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctOm			AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPodschDocOtch  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOtstup         AS CHARACTER NO-UNDO.

mOtstup = FILL(" ",14).

ASSIGN
   mLines[ 1] = "┌─" + FILL("─",{&format-cur-name})                   + 
   "─┬───────────────────────────────────────┬────────────────────────────" + 
   "───────────┐"
   mLines[ 2] = "│ " + PADC("Наименование валюты",{&format-cur-name}) + " │                Приход                 │                 Расход                │"
   mLines[ 3] = "│ " + FILL(" ",{&format-cur-name})                   + 
   " ├────────────┬──────────────────────────┼────────────┬───────────────" + 
   "───────────┤"
   mLines[ 4] = "│ " + FILL(" ",{&format-cur-name})                   + " │ количество │      сумма цифрами       │ количество │      сумма цифрами       │"
   mLines[ 5] = "│ " + FILL(" ",{&format-cur-name})                   + 
   " │  кассовых  │ с указанием наименования │  кассовых  │ с указанием на" + 
   "именования │"
   mLines[ 6] = "│ " + FILL(" ",{&format-cur-name})                   + 
   " │ документов │         валюты           │ документов │         валюты" + 
   "           │"
   mLines[ 7] = "├─" + FILL("─",{&format-cur-name})                   + 
   "─┼────────────┼──────────────────────────┼────────────┼───────────────" + 
   "───────────┤"
   mLines[10] = ""
   mLines[11] = "│ " + PADC("1",{&format-cur-name})                   + " │     2      │            3             │     4      │            5             │"
   mLines[12] = "├─" + FILL("─",{&format-cur-name})                   + 
   "─┼────────────┼──────────────────────────┼────────────┼───────────────" + 
   "───────────┤"
   mLines[13] = ""
   mLines[14] = "└─" + FILL("─",{&format-cur-name})                   + 
   "─┴────────────┴──────────────────────────┴────────────┴───────────────" + 
   "───────────┘"
   mLines[15] = "├─" + FILL("─",{&format-cur-name})                   + 
   "─┴────────────┴──────────────────────────┴────────────┴───────────────" + 
   "───────────┤"
   mLines[16] = "├─" + FILL("─",{&format-cur-name})                   + 
   "─┬────────────┬──────────────────────────┬────────────┬───────────────" + 
   "───────────┤"

   mRecvType[1] = "по документам, составленным на бумажном носителе:"
   mRecvType[2] = "по документам в электронном виде:"
   .

mCols = LENGTH(mLines[1]).
&GLOBAL-DEFINE cols mCols

ASSIGN 
   mPodschDocOtch  = fGetSetting("ПодсчДокОтч","","Нет") EQ "Да"
   mChMask         = IF NUM-ENTRIES(iParams,";") > 1 THEN ENTRY(1,iParams,";") ELSE iParams
   mChSetCodNacVal = IF NUM-ENTRIES(iParams,";") > 1 THEN ENTRY(NUM-ENTRIES(iParams,";"),iParams,";") ELSE ?
   mChCodNacVal    = fGetSetting(mChSetCodNacVal,"","810")
   mChCodNacVal    = IF CAN-DO("643,810",mChCodNacVal) THEN "" ELSE mChCodNacVal
   mCodOurCur      = mChCodNacVal
/*   mAcctOm         = "20202398205001000000*,20202810505001000000*,20202840805001000000*,20202978405001000000*"*/
   mAcctOm         = "20202*"
   .

{getdate.i}
 
FIND FIRST bCurrency WHERE 
           bCurrency.currency EQ ""
NO-LOCK NO-ERROR.
mNacIso = IF AVAILABLE bCurrency THEN 
             (IF mChSetCodNacVal EQ "КодНацВал0406007" 
                 AND bCurrency.i-currency EQ "RUR"
              THEN "RUB"
              ELSE bCurrency.i-currency)
          ELSE "???".

/**/

FOR EACH op-entry
 WHERE op-entry.op-date   EQ end-date
	 AND op-entry.filial-id EQ shFilial
	 AND (   CAN-DO(mAcctOm,op-entry.acct-db)
           OR CAN-DO(mAcctOm,op-entry.acct-cr))
  NO-LOCK,
  FIRST op WHERE op.op EQ op-entry.op
		  AND CAN-DO(mChMask,op.doc-type) 
  NO-LOCK
  BY op-entry.op:
	
  RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
							  
  vDate = "*".

   RUN AddRecInTemp-Table IN THIS-PROCEDURE(vDate,op.op).
END.

/**/
FOR EACH ttSprCash NO-LOCK:
   FIND FIRST ttSvodSpr WHERE 
              ttSvodSpr.Currency EQ ttSprCash.Currency 
          AND ttSvodSpr.DocDate  EQ ttSprCash.DocDate
          AND ttSvodSpr.RecvOnP  EQ ttSprCash.RecvOnP

      NO-LOCK NO-ERROR.
/* Во врем. табличку сгруппируем все по валюте */
   IF AVAILABLE ttSvodSpr THEN
   DO:
      ASSIGN 
         ttSvodSpr.SummDb     = ttSvodSpr.SummDb     + ttSprCash.SummDb  /* Приход */
         ttSvodSpr.SummRateDb = ttSvodSpr.SummRateDb + ttSprCash.SummRateDb  /* Приход */
         ttSvodSpr.QtyDb      = ttSvodSpr.QtyDb      + ttSprCash.QtyDb   /* Кол-во документов в приходе */
         ttSvodSpr.SummCr     = ttSvodSpr.SummCr     + ttSprCash.SummCr  /* Расход */
         ttSvodSpr.SummRateCr = ttSvodSpr.SummRateCr + ttSprCash.SummRateCr  /* Расход */
         ttSvodSpr.QtyCr      = ttSvodSpr.QtyCr      + ttSprCash.QtyCr   /* Кол-во документов в расходе */
      .                
   END.
   ELSE
   DO:
      CREATE ttSvodSpr.
      ASSIGN 
         ttSvodSpr.Currency   = ttSprCash.Currency
         ttSvodSpr.iCurrency  = IF mRCurrency THEN mNacISO 
                                ELSE ttSprCash.iCurrency
         ttSvodSpr.CurrName   = ttSprCash.CurrName
         ttSvodSpr.RecvOnP    = ttSprCash.RecvOnP
         ttSvodSpr.SummDb     = ttSprCash.SummDb    /* Приход */
         ttSvodSpr.SummRateDb = ttSprCash.SummRateDb /* Приход */
         ttSvodSpr.QtyDb      = ttSprCash.QtyDb     /* Кол-во документов в приходе */
         ttSvodSpr.SummCr     = ttSprCash.SummCr    /* Расход */
         ttSvodSpr.SummRateCr = ttSprCash.SummRateCr /* Расход */
         ttSvodSpr.QtyCr      = ttSprCash.QtyCr     /* Кол-во документов в расходе */
         ttSvodSpr.DocDate    = ttSprCash.DocDate
         ttSvodSpr.OpDate     = DATE(ttSprCash.DocDate)
      NO-ERROR.                
   END.
END. /* for each ttSprCash */
   
&IF DEFINED(Cols) &THEN
   {setdest.i &Cols=" + {&Cols}"}
&ELSE
   {setdest.i &Cols=" + {&LengthPrinter}"}
&ENDIF
   RUN PrintOnePageSvodSpr.
   
{preview.i}   

{intrface.del}

/* ========================== Procedure block ============================ */
PROCEDURE PrintOnePageSvodSpr.

   DEFINE VARIABLE vDateStr AS CHARACTER     NO-UNDO.

   DEFINE BUFFER bttSvodSpr FOR ttSvodSpr.

   FOR EACH bttSvodSpr
   BREAK BY bttSvodSpr.OpDate:
   
      IF FIRST-OF(bttSvodSpr.OpDate) THEN
      DO:
         vDateStr = STRING(DAY(end-date), "99 ") + ENTRY(MONTH(end-date),'января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря') + STRING(YEAR(end-date), " 9999 года").

/* ========================== Заголовок отчёта =========================== */
         {head-318p.i
            &CodForm = "'0402114'"
         }
         PUT UNFORMATTED
	        mOtstup mOtstup mOtstup 'ПАО "Плюс Банк"' SKIP
			mOtstup FILL('─', 82) SKIP.
     	 PUT UNFORMATTED
			mOtstup " полное фирменное (сокращенное фирменное) наименование кредитной организации или  " SKIP
			mOtstup "  полное (сокращенное) наименование филиала, или наименование и (или) номер ВСП   " SKIP
			mOtstup "(при наличии) либо иные идентифицирующие признаки ВСП (при отсутствии наименования" SKIP
			mOtstup "   и номера) с указанием на его принадлежность кредитной организации (филиалу)    " SKIP(1).
         PUT UNFORMATTED
            PADC("СПРАВКА О КАССОВЫХ ОБОРОТАХ",{&Cols}) SKIP
            PADC(vDateStr,{&Cols})                      SKIP(1).

/* ========================== Шапка таблички ============================= */
         DO i = 1 TO 11:
            IF i <> 10 AND mLines[i] <> "" THEN
               PUT UNFORMATTED mLines[i] SKIP.
         END.

/* ==================== Создание таблички с суммами ====================== */
         FOR EACH ttSvodSpr WHERE 
                  ttSvodSpr.DocDate EQ bttSvodSpr.DocDate NO-LOCK
         BREAK BY ttSvodSpr.RecvOnP:

            IF FIRST-OF(ttSvodSpr.RecvOnP) THEN 
               PUT UNFORMATTED
                  mLines[15] SKIP
                  "│ " PADR(mRecvType[ttSvodSpr.RecvOnP],LENGTH(mLines[1]) - 3)
                  "│" SKIP
                  mLines[16] SKIP.
            ELSE  
               PUT UNFORMATTED mLines[12] SKIP.              
            PUT UNFORMATTED "│ "
               ttSvodSpr.CurrName FORMAT "x({&format-cur-name})"  " │ "
               ttSvodSpr.QtyDb    FORMAT ">>,>>>,>>9" " │  " 
               (IF mRCurrency THEN ttSvodSpr.SummRateDb
               ELSE ttSvodSpr.SummDb) FORMAT "->>>,>>>,>>>,>>9.99" " " 
               ttSvodSpr.iCurrency " │ " 
               ttSvodSpr.QtyCr    FORMAT ">>,>>>,>>9" " │  "
               (IF mRCurrency THEN ttSvodSpr.SummRateCr
               ELSE ttSvodSpr.SummCr) FORMAT "->>>,>>>,>>>,>>9.99" " " 
               ttSvodSpr.iCurrency " │ " SKIP.
         END. /* FOR EACH ttSvodSpr */
   
         PUT UNFORMATTED mLines[14] SKIP(1).
/* ==================== Печатаем подписи под справкой ==================== */
         RUN GetRepFioByRef('cas-svod318p','0500',?,?).
         PUT UNFORMATTED SKIP(1).
         RUN PrintFioAndPost(mFioInRep[1],mPostInRep[1],0).
         PUT UNFORMATTED SKIP(1)
            "С данными бухгалтерского учета сверено:" SKIP(1).
         RUN PrintFioAndPost(mFioInRep[2],mPostInRep[2],0).
      END.
   END.

END PROCEDURE. /* PrintOnePageBookVal */

/*------------------------------------------------------------------------------
   Назначение: Добавляет во временную таблицу записи о субсчетах 
------------------------------------------------------------------------------*/
PROCEDURE AddRecInTemp-Table:

   DEFINE INPUT  PARAMETER iOpDate AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iOp AS INT64 NO-UNDO.

   DEFINE VARIABLE vDebit       AS LOGICAL.             
   DEFINE VARIABLE vDprId       AS CHARACTER NO-UNDO.  /* Смена */
   DEFINE VARIABLE vCurrName    AS CHARACTER NO-UNDO. /* Наименование валюты */
   DEFINE VARIABLE vOp          AS INT64     NO-UNDO. /* Документ */
   DEFINE VARIABLE vRate        AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vCurr        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRecvOnPaper AS INT64     NO-UNDO.
   DEFINE VARIABLE vChN         AS INT64     NO-UNDO INIT 1. /* для подсчета проводок для чека*/

   DEFINE BUFFER bCurrency  FOR Currency.
   DEFINE BUFFER bttSprCash FOR ttSprCash.
   DEFINE BUFFER bop-entry  FOR op-entry.
   DEFINE BUFFER bop        FOR op.
   DEFINE BUFFER bbop       FOR op.
   
      mAgreeYes = TRUE. /* данные для отчёта есть */
      FIND FIRST bop WHERE bop.op EQ iOp NO-LOCK NO-ERROR.
      vRecvOnPaper = INT64(GetXAttrValue("op",STRING(bop.op),
                           "СпособПолуч") NE "") + 1.
      FIND FIRST ttSprCash WHERE
			           ttSprCash.Op       EQ op-entry.op
             AND ttSprCash.Currency EQ op-entry.currency
             AND ttSprCash.DocDate  EQ iOpDate
             AND ttSprCash.RecvOnP  EQ vRecvOnPaper
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttSprCash THEN
      DO:
         vCurr = IF op-entry.currency EQ "" THEN mChCodNacVal ELSE op-entry.currency.
         /* Узнаем наименование валюты */
         FIND FIRST bCurrency WHERE 
                    bCurrency.currency EQ vCurr
            NO-LOCK NO-ERROR.

         vCurrName = IF AVAILABLE bCurrency THEN bCurrency.name-currenc ELSE "?".
         CREATE ttSprCash.
         ASSIGN
			      ttSprCash.op        = op-entry.op
            ttSprCash.Currency  = op-entry.currency
            ttSprCash.Acct      = op-entry.acct-db
            ttSprCash.CurrName  = vCurrName
            ttSprCash.RecvOnP   = vRecvOnPaper
			      ttSprCash.DocNum    = bop.doc-num
            ttSprCash.iCurrency = IF AVAILABLE bCurrency THEN
                                     (IF mChSetCodNacVal EQ "КодНацВал0406007"
                                         AND bcurrency.i-currency EQ "RUR"
                                      THEN "RUB"
                                      ELSE bcurrency.i-currency)
                                  ELSE ?
            ttSprCash.DocDate   = iOpDate
            ttSprCash.OpDate    = DATE(iOpDate)
			      ttSprCash.CheckM    = FALSE
         NO-ERROR.
      END.

      IF CAN-DO(mAcctOm,op-entry.acct-db) THEN
         ASSIGN
            ttSprCash.SummDb = ttSprCash.SummDb + (IF op-entry.currency EQ "" THEN
                                  op-entry.amt-rub
                               ELSE
                                  op-entry.amt-cur)
         .
      ELSE
         ASSIGN
            ttSprCash.SummCr = ttSprCash.SummCr + (IF op-entry.Currency EQ "" THEN
                                  op-entry.amt-rub
                               ELSE
                                  op-entry.amt-cur)
         .
      vDebit = IF CAN-DO(mAcctOm,op-entry.acct-db) THEN TRUE ELSE FALSE.
	  
      FIND FIRST bttSprCash WHERE bttSprCash.Op       EQ op-entry.op
                              AND bttSprCash.debit    EQ vDebit
                              AND bttSprCash.currency EQ op-entry.currency
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bttSprCash OR NOT mPodschKolDoc THEN
         IF CAN-DO(mAcctOm,op-entry.acct-db) THEN
            ttSprCash.QtyDb = ttSprCash.QtyDb + 1.
         ELSE
         DO:
            IF     NOT mPodschDocOtch 
               OR  NOT CAN-FIND(FIRST bop-entry WHERE 
                                      bop-entry.Op     EQ op-entry.op
                                  AND CAN-DO(mAcctOm,bop-entry.acct-db)) THEN
               ttSprCash.QtyCr = ttSprCash.QtyCr + 1.
         END.
	  
	  /* проверка на чек из омской АБС */
	  FIND FIRST bttSprCash WHERE bttSprCash.DocNum EQ bop.doc-num
							  AND bttSprCash.op     NE op-entry.op
                AND bttSprCash.Acct   EQ op-entry.acct-db
		NO-LOCK NO-ERROR.
	  
	   IF AVAIL bttSprCash AND bttSprCash.CheckM AND op.doc-type EQ '04' THEN DO:
			ttSprCash.QtyCr = ttSprCash.QtyCr - 1.
		END.
	  
	  IF op.doc-type EQ '04' THEN DO:
		FOR EACH bop-entry WHERE bop-entry.op-date   EQ op-entry.op-date
							 AND bop-entry.filial-id EQ '0500'
							 AND bop-entry.acct-db   EQ op-entry.acct-db
							 AND bop-entry.acct-cr   EQ op-entry.acct-cr,
		   FIRST bbop of bop-entry WHERE bbop.doc-type EQ '04'
						             AND bbop.doc-num  EQ op.doc-num
		   BY bop-entry.op-date:
				vChN = vChN + 1.
		END.
    IF vChN > 1 THEN DO:
			ttSprCash.CheckM = TRUE.
      vChN = 0.
		END.
	  END.
	  /**/	  
		  
      ASSIGN
         ttSprCash.debit = IF CAN-DO(mAcctOm,op-entry.acct-db) THEN TRUE ELSE FALSE
      .

END PROCEDURE. /* AddRecInTemp-Table */