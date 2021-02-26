/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "Плюс Банк"
     Filename: fill-graphp.p
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created: 10/05/2013
*/

{globals.i}

{svarloan.def new}

{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get instrum}  /* Библиотека для работы с фин. инструментами. */
{intrface.get loan}     /* Инструменты для работы с loan. */
{intrface.get loanc}
{intrface.get corr}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get date}
{intrface.get pogcr}    /* Библиотека инструментов для работы с графиками погашения в КиД. */

{fill-graphp.def}           /* Объявление вр.таблицы ttReportTable */
{pp-corr.p}

DEF INPUT PARAM iContract  AS CHAR NO-UNDO.    /* Назначение договора */
DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.    /* Номер договора */
DEF INPUT PARAM iDate      AS DATE NO-UNDO.    /* Дата */
DEF INPUT PARAM fullKasko as decimal no-undo.
DEF INPUT PARAM typeTable as char no-undo.     /* 1- Авто, 2- Авто+Каско, 3 - МБ+ */
DEF OUTPUT PARAM TABLE FOR ttReportTable.      /* Временная таблица */

/* Объявление переменных */
DEF VAR mSettList      AS CHAR  NO-UNDO.            /* Список НП */
DEF VAR mSettings      AS CHAR  NO-UNDO EXTENT 5.   /* Настроечные параметры */
DEF VAR mID            AS INT64 NO-UNDO INIT 1.     /* Порядковый номер строки (для расчета формул) */
DEF VAR mIsReal        AS LOG   NO-UNDO.            /* Признак копирования обязательств "как есть" */
DEF VAR mIsRealTr      AS LOG   NO-UNDO.            /* Признак копирования обязательств "как есть" для транша*/
DEF VAR mIsOneTerm     AS LOG   NO-UNDO.            /* Признак единственного обязательства на условии */
DEF VAR mIsVirtual     AS LOG   NO-UNDO.            /* Признак необходимости создания виртуального графика */
DEF VAR mI             AS INT64 NO-UNDO.
DEF VAR mJ             AS INT64 NO-UNDO.
DEF VAR mTrshAmnt      AS DEC   NO-UNDO EXTENT 3.   /* Сумма ОД договора(1), траншей (2), транша (3) */
DEF VAR mDateTr        AS DATE  NO-UNDO.            /*Дата окончания последнего транша по договору*/

DEF VAR mCopyOD        AS LOG   NO-UNDO.        /* Копировать или строить заново график ОД по вирт. договору */
DEF VAR mSumOper       AS DEC   NO-UNDO.        /* Сумма обязательств по возврату (выдач) по договору\траншу, для расчета не выданного остатка */
DEF VAR mSumVyd        AS DEC   NO-UNDO.        /* Сумма всех операций (в частном случае выдач) по договору, указанных в НП ЭПС4Мин */
DEF VAR mIsVyd         AS LOG   NO-UNDO.        /* Признак наличия выдачи по договору */
DEF VAR mStrProp       AS CHAR  NO-UNDO.        /* НП ЭПССтрПроп */
DEF VAR mSumTov        AS CHAR  NO-UNDO.        /* ДР СумТовВСчКред */
DEF VAR mCoefTov       AS DEC   NO-UNDO.        /* Коэф. товара */
DEF VAR mCoefSrok      AS DEC   NO-UNDO.        /* Коэф. срока страхования */
DEF VAR mNumDCred      AS INT64 NO-UNDO.
DEF VAR mNumDIns       AS INT64 NO-UNDO.
def var procSt as dec no-undo. /* процентная ставка */
def var datePlat as date no-undo. /* дата платежа */
def var myIntDate as dec no-undo. /* день платежа */

DEF VAR rko11_price as decimal no-undo.

   /* Локализация буферов */
DEF BUFFER loan        FOR loan.
DEF BUFFER b-loan      FOR loan.
DEF BUFFER ins-loan    FOR loan.
DEF BUFFER loan-cond   FOR loan-cond.
DEF BUFFER b-loan-cond FOR loan-cond.
DEF BUFFER term-obl    FOR term-obl.
DEF BUFFER b-term-obl  FOR term-obl.
DEF BUFFER x-term-obl  FOR term-obl.
DEF BUFFER loan-acct   FOR loan-acct.
DEF BUFFER b-loan-acct FOR loan-acct.
DEF BUFFER loan-int    FOR loan-int.
DEF BUFFER chowhe      FOR chowhe.
DEF BUFFER comm-rate   FOR comm-rate.

/* Объявляем дополнительные временные таблицы */
DEF TEMP-TABLE tt-term-obl  LIKE term-obl.  /* Для idnt = 3 основной долг и idnt = 1 %% */

   /* Отладочная информация */
IF SESSION:DEBUG-ALERT THEN DO:
   DEF STREAM vStream.
   OUTPUT STREAM vStream TO VALUE("epscalc_debug.tmp") APPEND.
END.

{fill-graphp.i}             /* Инструменты для рассчета ЭПС */

mFormGrKom = FGetSetting("ГрафКомН","ФормГрКом",?).

   /* Очищаем вр.таблицу */
{empty tt-term-obl}

   /* Отладочная информация */
IF SESSION:DEBUG-ALERT THEN DO:
   PUT STREAM vStream UNFORMATTED
      "Договор №: " iContCode SKIP
      "(1 - %%, 2 - Остатки, 3 - ОД, 4 - Доп.тр., комиссии, 5 - Страховые взносы)"
   SKIP.
END.

rko11_price = 0.
FIND FIRST loan WHERE
           loan.contract  EQ iContract
   AND     loan.cont-code EQ iContCode
NO-LOCK NO-ERROR.

IF AVAIL loan THEN
DO:
	mSumVyd = 0.
	if loan.cont-type = "Течение" then do: /* кредитная линия (все платежи придумываем) */
/*		/* Поиск действующего условия на дату расчета */
		FIND LAST loan-cond WHERE
            loan-cond.contract    EQ loan.contract
			AND loan-cond.cont-code   EQ loan.cont-code
			AND loan-cond.since       LE iDate
			NO-LOCK NO-ERROR.
		/* Нет условия - пропускаем */
		IF AVAIL loan-cond THEN
		DO:
			find first term-obl where term-obl.cont-code = loan.cont-code and term-obl.idnt = 2 and term-obl.nn  = 0 no-lock no-error.
			if avail term-obl then do:
				mSumVyd = term-obl.amt-rub.
				procSt = 0.
				find first comm-rate where commission = '%Кред' and kau begins 'Кредит,' + loan.cont-code no-lock no-error.
				if avail comm-rate then do:
						procSt = rate-comm.
						datePlat = term-obl.end-date.
						/* Сохраняем сумму лимита во временной таблице */
						RUN CrtRepTbl(2,   /* Остатки */
						loan.open-date,
                        - mSumVyd,
						0).
						myIntDate = loan-cond.int-date.
						if myIntDate = 0 then myIntDate = day(loan.open-date).
						
						
						FUNCTION FindNextWorkDate RETURNS date
   (iDay AS Decimal,
    iDate AS DATE):
						
						
						
						
						
						
						
				end.
			end.
		END.
		/* Отладочная информация */
		IF SESSION:DEBUG-ALERT THEN DO:
			OUTPUT STREAM vStream CLOSE.
		END.

		RUN DeleteOldDataProtocol IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ").
		RUN DeleteOldDataProtocol IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ").

		{intrface.del}
		return.
*/		
	end.

   /* Учет страховых платежей (обязательств)
   ** Поиск договора страхования */
      FOR EACH ins-loan WHERE
               ins-loan.contract         EQ "Страх"
         AND   ins-loan.parent-cont-code EQ iContCode
         AND   ins-loan.parent-contract  EQ iContract
         AND   ins-loan.open-date        LE iDate
         AND  (ins-loan.close-date       GE iDate
            OR ins-loan.close-date       EQ ?)
      NO-LOCK,
            EACH term-obl OF ins-loan WHERE term-obl.idnt EQ 1
            NO-LOCK 
            BREAK BY term-obl.end-date:
         /* Сохраняем сумму во временной таблице */
         RUN CrtRepTbl(5,
                       term-obl.end-date,
                       term-obl.amt-rub,
                       mID).
      END. /* Учет страховых платежей */

      /* Поиск действующего условия на дату расчета */
   FIND LAST loan-cond WHERE
             loan-cond.contract    EQ loan.contract
      AND    loan-cond.cont-code   EQ loan.cont-code
      AND    loan-cond.since       LE iDate
   NO-LOCK NO-ERROR.
      /* Нет условия - пропускаем */
   IF AVAIL loan-cond THEN
   DO:
      /* Если договор не "Течение"*/
      IF loan.cont-type NE "Течение" THEN
         mIsReal = TRUE. /* Копируем обязательства с договора "как есть" */
     
         /* Копируем обязательства "как есть" */
      IF mIsReal THEN
      DO:
            /* Отладочная информация */
         IF SESSION:DEBUG-ALERT THEN DO:
             PUT STREAM vStream UNFORMATTED
                'Копируем обязательства "как есть":'
             SKIP.
         END.
         DO mI = 1 TO 3:
            RUN CopyTTData(loan.contract,
                           loan.cont-code,
                           mI,
                           loan.end-date).
         END.
      END.

      /* Отладочная информация */
      IF SESSION:DEBUG-ALERT THEN DO:
          PUT STREAM vStream UNFORMATTED
             "Перенос данных в таблицу отчета ttReportTable: "
          SKIP.
      END.

      /* Добавляем данные в таблицу отчета ttReportTable из tt-term-obl  */
      FOR EACH tt-term-obl WHERE
               tt-term-obl.idnt GE 1
         AND   tt-term-obl.idnt LE 3:
         
         RUN CrtRepTbl(tt-term-obl.idnt,
                       tt-term-obl.end-date,
                       tt-term-obl.amt-rub,
                       mID).
         mID = mID + 1.
      END.
      
      /* Отладочная информация */
      IF SESSION:DEBUG-ALERT THEN DO:
          PUT STREAM vStream UNFORMATTED
             'Суммы по видам операций:'
          SKIP.
      END.
     
            /* Если график комиссий ведется на договоре, то берем данные из графика*/
      IF mFormGrKom EQ "Да" THEN
      DO:
         FOR EACH x-term-obl WHERE x-term-obl.contract  EQ loan.contract
                               AND x-term-obl.cont-code EQ loan.cont-code
                               AND x-term-obl.idnt      EQ 10
         NO-LOCK:
               /* Переносим полученные результаты в таблицу отчета */
               RUN CrtRepTbl (4,
                              x-term-obl.end-date,
                              x-term-obl.amt-rub,
                              mID).
         END.
      END.

      FIND FIRST signs WHERE signs.file-name EQ "loan"
                         AND signs.surrogate EQ loan.contract + "," + loan.cont-code
                         AND signs.code      EQ "rko_comiss" NO-LOCK NO-ERROR.
      IF AVAIL signs THEN DO:
       
         /* Переносим полученные результаты в таблицу отчета */
         RUN CrtRepTbl (400,
                        loan.open-date,
                        dec(signs.code-value),
                        mID).
		 /*rko11_price = dec(signs.code-value).*/
      END.
      ELSE DO:
         
         FIND LAST comm-rate WHERE comm-rate.commission EQ "%РКО"
                               AND CAN-DO(comm-rate.kau, loan.cont-code)
                               AND CAN-DO(comm-rate.kau, loan.contract)
                               NO-LOCK NO-ERROR.
         IF AVAIL comm-rate THEN
             /* Переносим полученные результаты в таблицу отчета */
            RUN CrtRepTbl (400,
                           loan.open-date,
                           comm-rate.rate-comm,
                           mID).
			/*rko11_price = comm-rate.rate-comm.*/
      END.   
   END.  /* AVAIL loan-cond */
END.     /* AVAIL loan      */

   /* При определении полной стоимости кредита все сборы (комиссии),
   ** предшествующие дате перечисления денежных средств заемщику (например,
   ** за рассмотрение заявки по кредиту), включаются в состав платежей,
   ** осуществляемых заемщиком на дату начального денежного потока (платежа)) */

def var myMonth as int64 no-undo.
def var myLastDate as date no-undo.
def var pred_tf_rest-debts as decimal no-undo.
def var tmpRko11 as char no-undo.

tmpRko11 = getxattrvalue("loan","Кредит," + loan.cont-code,"rko11_price").

if tmpRko11 <> ? and tmpRko11 <> "" then do:
	rko11_price = decimal(replace(tmpRko11,",","")).
end.
else rko11_price = 0.

/* if loan.cont-type = 'АвтоПлюс' then do: */
 /*if can-do('Авто+М*,Авто+Р*', loan.cont-type) then do:   */
 if typeTable = "2" then do:
	FOR EACH ttReportTable BY ttReportTable.tf_payment-date DESCENDING:
		myLastDate = ttReportTable.tf_payment-date.
		leave.
	END.
	
	create ttReportTable.
	assign
		ttReportTable.tf_id = -1
		ttReportTable.tf_payment-date = myLastDate.
    release ttReportTable.
	
def var countPlat as int no-undo.
	
   FOR EACH ttReportTable BY ttReportTable.tf_payment-date:
      IF ttReportTable.tf_id EQ 0 THEN
      DO:
		 pred_tf_rest-debts = ttReportTable.tf_rest-debts.
		 /* fullKasko = ttReportTable.tf_actual-payment.  */
         ACCUMULATE ttReportTable.tf_additional-charge1 (TOTAL). /* 4,400. Суммы дополнительных требований, комиссии */
         ACCUMULATE ttReportTable.tf_additional-charge2 (TOTAL).
         ACCUMULATE ttReportTable.tf_actual-payment (TOTAL).     /* 5. Суммы страховых взносов */
		 myMonth = month(ttReportTable.tf_payment-date).
		 countPlat = 0.
         ASSIGN
             ttReportTable.tf_additional-charge1 = 0
             ttReportTable.tf_additional-charge2 = 0
         .
         DELETE ttReportTable.
      END.
      ELSE
      DO:
	  countPlat = countPlat + 1.
         ASSIGN
            ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ACCUM TOTAL ttReportTable.tf_additional-charge1
            ttReportTable.tf_additional-charge2 = ttReportTable.tf_additional-charge2 + ACCUM TOTAL ttReportTable.tf_additional-charge2
            ttReportTable.tf_actual-payment     = ttReportTable.tf_actual-payment     + ACCUM TOTAL ttReportTable.tf_actual-payment
         .
      
         IF ttReportTable.tf_id EQ 1 THEN
         DO:
			/* fullKasko = ttReportTable.tf_actual-payment / 2.*/
             ttReportTable.tf_sum-payment = ttReportTable.tf_sum-payment + 
                                            ttReportTable.tf_additional-charge1 + 
                                            ttReportTable.tf_additional-charge2.
             ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ttReportTable.tf_sum-percent.
			 /* if rko11_price <> 0 and myMonth = month(ttReportTable.tf_payment-date) then */
			 if rko11_price <> 0 and countPlat = 12 then do:
				ttReportTable.tf_actual-payment = fullKasko / rko11_price * pred_tf_rest-debts.
				countPlat = 0.
			end.
			 else 
				ttReportTable.tf_actual-payment = 0.
             ttReportTable.tf_sum-percent = 0.
			 pred_tf_rest-debts = ttReportTable.tf_rest-debts.
         END.
         else 
         DO:
			 /* if rko11_price <> 0 and myMonth = month(ttReportTable.tf_payment-date) and myLastDate <> ttReportTable.tf_payment-date then */
			 if rko11_price <> 0 and countPlat = 12 and myLastDate <> ttReportTable.tf_payment-date then do:
			 	ttReportTable.tf_actual-payment = fullKasko / rko11_price * pred_tf_rest-debts.
				countPlat = 0.
			end.
			 else 
				ttReportTable.tf_actual-payment = 0.
			 ttReportTable.tf_additional-charge2 = 0.
			 ttReportTable.tf_additional-charge1 = 0.
			 pred_tf_rest-debts = ttReportTable.tf_rest-debts.
         END.

      END.
   END.
end. /* if loan.cont-type = 'Авто+М' or loan.cont-type = 'Авто+Р' */

else 
do:
/* IF CAN-DO("Avto*,Авто*",loan.cont-type) and loan.cont-type <> 'Авто+М' and loan.cont-type <> 'Авто+Р' THEN  */
if typeTable = '1' then
DO:
   SUMM:
   FOR EACH ttReportTable BY ttReportTable.tf_payment-date:
   
      IF ttReportTable.tf_id EQ 0 THEN
      DO:
         ACCUMULATE ttReportTable.tf_additional-charge1 (TOTAL). /* 4,400. Суммы дополнительных требований, комиссии */
         ACCUMULATE ttReportTable.tf_additional-charge2 (TOTAL).
         ACCUMULATE ttReportTable.tf_actual-payment (TOTAL).     /* 5. Суммы страховых взносов */
      
         ASSIGN
             ttReportTable.tf_additional-charge1 = 0
             ttReportTable.tf_additional-charge2 = 0
         .
         DELETE ttReportTable.
      END.
      ELSE
      DO:
         ASSIGN
            ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ACCUM TOTAL ttReportTable.tf_additional-charge1
            ttReportTable.tf_additional-charge2 = ttReportTable.tf_additional-charge2 + ACCUM TOTAL ttReportTable.tf_additional-charge2
            ttReportTable.tf_actual-payment     = ttReportTable.tf_actual-payment     + ACCUM TOTAL ttReportTable.tf_actual-payment
            ttReportTable.tf_actual-payment     = 0. 
         .
      
         IF ttReportTable.tf_id EQ 1 THEN
         DO:
/* message 'ttReportTable.tf_sum-payment ' string(ttReportTable.tf_sum-payment) +
		 ' ttReportTable.tf_additional-charge1 ' string(ttReportTable.tf_additional-charge1) view-as alert-box. */
             ttReportTable.tf_sum-payment = ttReportTable.tf_sum-payment + 
                                            ttReportTable.tf_additional-charge1 + 
                                            ttReportTable.tf_additional-charge2.
             ttReportTable.tf_additional-charge1 = ttReportTable.tf_additional-charge1 + ttReportTable.tf_sum-percent.
             ttReportTable.tf_sum-percent = 0.
         END.
         
         LEAVE SUMM.
      END.
   END.
END.
 /* run instview.p(TEMP-TABLE ttReportTable:HANDLE). */
/* IF CAN-DO("MБ+(ФИЗ)",loan.cont-type) THEN  */
if typeTable = '3' then
DO:
   SUMM:
   FOR EACH ttReportTable BY ttReportTable.tf_payment-date:  
      IF ttReportTable.tf_id EQ 0 THEN
      DO:
		pred_tf_rest-debts = ttReportTable.tf_additional-charge1.
         DELETE ttReportTable.
      END.
      ELSE
      DO:
         IF ttReportTable.tf_id EQ 1 THEN
         DO:
             ttReportTable.tf_additional-charge1 = pred_tf_rest-debts.
         END.
	  END.
   END.
END.
end. /* if loan.cont-type = 'Авто+М' or loan.cont-type = 'Авто+Р'  ... else do: */

   /* Отладочная информация */
IF SESSION:DEBUG-ALERT THEN DO:
   OUTPUT STREAM vStream CLOSE.
END.

RUN DeleteOldDataProtocol IN h_base ("ПЛАТЕЖИ ПО ПРОЦЕНТАМ СДВИГ").
RUN DeleteOldDataProtocol IN h_base ("ОБЯЗАТЕЛЬСТВА ПО ВОЗВРАТУ СДВИГ").

{intrface.del}

/*
FUNCTION FindNextWorkDate RETURNS date
   (iDay AS Decimal,
    iDate AS DATE):
	def var nextDate as date no-undo.
	nextDate = date_correct(Month(iDate),1,iDay,Year(iDate)).
	DO WHILE holiday(nextDate):
      nextDate = nextDate - 1.
	END.
	return nextDate.
END FUNCTION.
*/









