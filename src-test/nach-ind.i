/* Обработка комиссий "Пеня" (Комментарий по примеру "ПеняКомСч") */
   
/* Расчет остатка по параметру на ДР Interest */
   /* Определим на каком interest сохраняется пеня (надо тащить из настройки) */


mVar205  = INT64(ENTRY(mI, mIdk)).
mPenyPar = INT64(GetCodeMisc("СхемНачКом", 
                           ENTRY(mI, mCommSpec) + ":" + mSchemCls, 
                           2)) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
   mPenyPar = 0.

ASSIGN
   mSummDob = 0
   mDateEnd = mDateEnd - 1
.
   /* Если пересчет назад */
IF iCalcBack THEN
   ASSIGN
      mDateBeg = loan.open-date  /* Считаем с начала действия договора */
   .
   /* Если пересчет вперед */
ELSE
   IF mPenyPar GT 10 THEN     /* Тянем накопленную сумму с параметра */
      mSummDob = LoadPar(mPenyPar, loan.contract + "," + loan.cont-code).

   /* Вычисляем пени за период */
RUN lncommsh.p (iContract,
                iContCode,
                ENTRY(mI, mCommSpec),
                mDateBeg,
                mDateEnd,
                NO).

   /* Суммируем все начисленные за период пени */
FOR EACH otch1:
   mSummDob = mSummDob + otch1.summ_pr.
END.

   /* Ищем операции оплаты пени (827), попадающие в период */
FOR EACH loan-int WHERE
         loan-int.contract  EQ iContract
   AND   loan-int.cont-code EQ iContCode
   AND   loan-int.id-d      EQ 5                     /* 5 */
   AND   loan-int.id-k      EQ mVar205   /* 205 */
   AND   loan-int.mdate     GE mDateBeg - 1  
   AND   loan-int.mdate     LE mDateEnd - 1
NO-LOCK:
   mSummDob = mSummDob - loan-int.amt-rub.
END.

   /* Сохраняем что осталось в ДР interest[mPenyPar] (interest[16] для 204) */
IF mPenyPar > 10 THEN
DO:
   mSummDob = (IF iRound THEN ROUND(mSummDob, 0) ELSE mSummDob).
   SavePar(mPenyPar, loan.contract + "," + loan.cont-code, STRING(mSummDob)).
END.
/* eof Расчет остатка по параметру на ДР Interest */


/* Обработка автоматических операций */
   /* Если считаем назад... */
IF iCalcBack THEN
DO:
      /* Если считаем назад, то удаляем автоматические операции 826 (204 - 5) */
   RUN DelAutoOpers (iContract,
                     iContCode,
                     iBegDate,
                     INT64(ENTRY(mI, mIddk)),  /* 204 */
                     5).
      /* Если считаем назад, то удаляем автоматические операции 828 (205 - 204) */
   RUN DelAutoOpers (iContract,
                     iContCode,
                     iBegDate,
                     mVar205,    /* 205 */
                     INT64(ENTRY(mI, mIddk))).  /* 204 */
END.
ELSE
DO:
      /* Ищем операции оплаты пени (827), попадающие в период */
   FOR EACH loan-int WHERE
            loan-int.contract  EQ iContract
      AND   loan-int.cont-code EQ iContCode
      AND   loan-int.id-d      EQ 5                     /* 5 */
      AND   loan-int.id-k      EQ mVar205  /* 205 */
      AND   loan-int.mdate     GE iBegDate - 1
      AND   loan-int.mdate     LE iEndDate - 1
   NO-LOCK:
         /* Создаем авт.операцию по начислению пени (826) */
      RUN CreateAutoOpers (iContract,
                           iContCode,
                           loan-int.mdate,
                           loan-int.amt-rub,
                           INT64(ENTRY(mI, mIddk)),   /* 204 */
                           5).
         /* Создаем авт.операцию по начислению пени (828) */
      RUN CreateAutoOpers (iContract,
                           iContCode,
                           loan-int.mdate,
                           loan-int.amt-rub,
                           mVar205,                     /* 205 */
                           INT64(ENTRY(mI, mIddk))).    /* 204 */
   END.
END.
/* eof Обработка автоматических операций */
