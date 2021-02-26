/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2005 ЗАО "Банковские информационные системы"
     Filename: SF-CALC.P
      Comment: Расчет кники покупок и продаж
   Parameters:
         Uses:
      Used by:
      Created: 23.01.2005 15:55 gorm     (39642)
     Modified: 22.02.2005 17:52 gorm     (39643)
     Modified: 22.03.2006 17:52 gorm     (51989) Введены статусы для счетов-фактур.
                                         Теперь счета-фактуры отбираются в книгу 
                                         покупок/продаж с учетом типов оплат и статусов
     Modified: 15.12.2006 18:01 laav     (62990) В условия отбора счетов-фактур добавлено условие
                                         на принадлежность счета-фактуры к отделению DataBlock или
                                         к одному зи потомков этого отделения
*/
{globals.i}
{sv-calc.i}
{intrface.get xclass}
{intrface.get axd}
{intrface.get asset}

DEF VAR mAmount       AS DEC NO-UNDO.  /* Сумма счет-фактуры (или частичной оплаты) */
DEF VAR mNDSAmt       AS DEC NO-UNDO.  /* НДС счет-фактуры (или частичной оплаты) */
DEF VAR mAmtNoNdsSF   AS DEC NO-UNDO.  /* Сумма счет-фактуры без НДС */
DEF VAR mAmountSF     AS DEC NO-UNDO.  /* Сумма счет-фактуры включая НДС */
DEF VAR mNDSAmtSF     AS DEC NO-UNDO.  /* Сумма НДС счет-фактуры */

DEF VAR txt           AS CHAR NO-UNDO EXTENT 20.
DEF VAR mNDS          AS CHAR NO-UNDO. /* Ставка НДС по услуге */
DEF VAR mNum          AS INT64  NO-UNDO. /* Номер для определения, 
                                       ** к какому виду НДС относить сумму услуги */
DEF VAR mCntryName    AS CHAR NO-UNDO. /* Наименование страны */
DEF VAR mNumGTD       AS CHAR NO-UNDO. /* Номер ГТД */
DEF VAR mContract     AS CHAR NO-UNDO. /* Назначение счет-фактуры */
DEF VAR mSalerName    AS CHAR NO-UNDO. /* Наименование контрагента */
DEF VAR mSalerAddres  AS CHAR NO-UNDO. /* Адрес контрагента */
DEF VAR mSalerInn     AS CHAR NO-UNDO. /* ИНН контрагента */
DEF VAR mSalerKPP     AS CHAR NO-UNDO. /* КПП контрагента */

DEF VAR mSgUch        AS CHAR NO-UNDO. /*доп. рек. КнигаУч */
DEF VAR mSgPart       AS CHAR NO-UNDO. /*доп. рек. КнигаОтр */
DEF VAR mAntiContract AS CHAR NO-UNDO. /*для авансовых счетов-фактур*/

DEF VAR mListUch      AS CHAR NO-UNDO. /*Список возможных значений КнигаУч*/
DEF VAR mSurrOp       AS CHAR NO-UNDO. /* Список суррогатов платежей, связанных со 
                                       ** счетом-фактурой */
DEF VAR mSF-OP-Links  AS CHAR NO-UNDO. /* Список кодов связи */

DEF VAR mI            AS INT64  NO-UNDO. /*Счетчик*/
DEF VAR mSimI         AS INT64  NO-UNDO. /*Счетчик*/
DEF VAR mBrList       AS CHAR NO-UNDO. /*Список отделений*/
DEF VAR mLstDate      AS CHAR NO-UNDO. /* Список дат */
DEF VAR mOnBook       AS CHAR NO-UNDO. /* НП ВклКниг */
DEF VAR mKnigaOtr     AS CHAR NO-UNDO. /* ДР КнигаОтр */
DEF VAR mDopList      AS CHAR NO-UNDO. /* ДР ДопЛист */
DEF VAR mDopListDate  AS DATE NO-UNDO. /* ДР ДопЛист */
DEF VAR mSummPlat     AS CHAR NO-UNDO. /* ДР СуммыПлат */
DEF VAR mDate         AS DATE NO-UNDO.
DEF VAR mSumm         AS DEC  NO-UNDO.

DEF BUFFER bloan FOR loan.

/* Вычисляет попадает ли дата из списка в период */
FUNCTION DatePeriod RETURNS LOGICAL (INPUT iDtBeg  AS DATE,
                                     INPUT iDtEnd  AS DATE,
                                     INPUT iList   AS CHARACTER).
      DEFINE VARIABLE i       AS INT64  NO-UNDO.
      DEFINE VARIABLE oResult AS LOGICAL  NO-UNDO.
      oResult = NO.
      
      IF iList NE "" THEN
      DO i = 1 TO NUM-ENTRIES(iList) - 1:
         IF ENTRY(i, iList) > "" THEN
         IF       DATE(ENTRY(i, iList)) >= iDtBeg
              AND DATE(ENTRY(i, iList)) <= iDtEnd THEN
         oResult = YES.
      END.
      RETURN oResult.

END FUNCTION.

FUNCTION IsEmpty RETURN LOG (iValue AS CHAR):
   iValue = TRIM(iValue).
   RETURN (iValue = "" OR iValue = ? OR iValue = "?").
END FUNCTION. /* IsEmpty */

/* Возвращает список суррогатов объектов,
** привязанных к указанному объекту
** по указанным кодам связи с учетом направления. */
FUNCTION VerifyLinks RETURNS CHAR
   (iClass-Code      AS CHAR, /* ID класса                            */
    iSurrogate       AS CHAR, /* ID(cуррогат) объекта                 */
    iLink-Direction  AS CHAR, /* Направление связи: s | t | ?         */
    iLink-Codes      AS CHAR, /* Список кодов линков в CAN-DO формате */
    iDlm             AS CHAR /* Разделитель результирующего списка   */
   ):

   DEF VAR vList  AS CHAR  NO-UNDO.

   DEF BUFFER xlink  FOR xlink.
   DEF BUFFER links  FOR links.

   IF IsEmpty(iLink-Direction) THEN
      iLink-Direction = ?.

   FOR EACH xlink WHERE
            xlink.class-code = iClass-Code
        AND CAN-DO(iLink-Codes, xlink.link-code)
      NO-LOCK:
      
      IF  iLink-Direction = ? OR
         (iLink-Direction = "s" AND xlink.link-direction BEGINS "s")
      THEN
         FOR EACH links WHERE
                  links.link-id   = xlink.link-id
              AND links.source-id = iSurrogate
              AND LOOKUP(links.target-id, vList, iDlm) = 0
            NO-LOCK:
            vList = vList + (IF vList = "" THEN "" ELSE iDlm) + links.target-id.
         END.

      IF  iLink-Direction = ? OR
         (iLink-Direction = "t" AND xlink.link-direction BEGINS "t")
      THEN
         FOR EACH links WHERE
                  links.link-id   = xlink.link-id
              AND links.target-id = iSurrogate
              AND LOOKUP(links.source-id, vList, iDlm) = 0
            NO-LOCK:
            vList = vList + (IF vList = "" THEN "" ELSE iDlm) + links.source-id.
         END.
   END.

   RETURN vList.
END.


DEF VAR aaa AS CHARACTER.

Main_Block:
DO ON ERROR  UNDO Main_Block, LEAVE Main_Block
   ON ENDKEY UNDO Main_Block, LEAVE Main_Block:

   ASSIGN
      mListUch  = GetXAttrEx("axd-sf","КнигаУч","data-format")    /* "Аванс/Не отображать" */
      mOnBook   = FGetSetting("ВклКниг","","")                    /* Настроечный параметр "ВклКниг" */
   .
   
   /* Определяем - книга продаж или покупок */
   FIND FIRST Formula WHERE
              Formula.Var-ID       EQ "sf-kind"
          AND Formula.DataClass-ID EQ DataClass.DataClass-id 
      NO-LOCK NO-ERROR.
   
   IF AVAIL formula 
      THEN mContract = ENTRY(1,formula.formula,'~~').
      ELSE mContract = "sf-in". /* по умолчанию - книга покупок */
   
   mAntiContract = IF mContract = "sf-in" 
                   THEN "sf-out" 
                   ELSE "sf-in".
   
   /*Собираем в mBrList все дочерние отделения для DataBlock (включая текущий)*/
   {getbrlst.i DataBlock.Branch-Id mBrList}

   /* Отбираем все счета-фактуры (выставленные или принятые),
      зарегестрированные за период расчета */
   lnfr:
   FOR EACH loan WHERE 
            loan.filial-id  = shFilial
/*        AND CAN-DO(mBrList, loan.branch-id)*/
        AND loan.contract   = mContract
/*        AND loan.open-date <= DataBlock.end-date
        AND loan.doc-ref     = loan.doc-ref*/
   NO-LOCK USE-INDEX op-l-fil:

      /* Счет-фактуры без типа или с начальным статусом, 
      ** а так же текущие - не отображаем */
      IF loan.cont-type   = ""         OR
         loan.loan-status = "Введен"   OR
         loan.loan-status = "Текущий"
      THEN NEXT lnfr.

      mLstDate = "".
      mSgUch = GetXattrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "КнигаУч",
                                "").

      /* Если указано, что не надо отображать - не отображаем */
      IF mSgUch = ENTRY(2,mListUch,"/") THEN NEXT lnfr.
      mDopList = GetxAttrValueEx("loan",
                                       loan.contract + "," + loan.cont-code,
                                       "ДопЛист",
                                       "").
      IF mDopList NE "" THEN
      DO:
         mDopListDate = DATE(mDopList) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            mDopList = "".
      END.

      IF (mOnBook EQ "Да") THEN
      DO:
         /* Пролверяем проводки по связи sf-op-dr, если ни одной нет, то проверяем на дату счет-фактуры,
            иначе добавляем записи по проводкам если дата проводки входит в искомый период. */
         ASSIGN
            mLstDate = ""
            mSF-OP-Links = "sf-op-dr"
            mSurrOp = VerifyLinks(loan.class-code,                      /* ID класса     */
                                  loan.contract + "," + loan.cont-code, /* ID(cуррогат) объекта   */
                                  ?,                                    /* Направление связи: s | t | ? */
                                  "sf-op-dr",                           /* Список кодов линков в CAN-DO формате */
                                  ";").                                 /* Разделитель результирующего списка   */
         
         IF TRIM(mSurrOp) NE ""  THEN
         DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
            FIND FIRST op-entry WHERE 
                       op-entry.op       = INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
                   AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI,mSurrOp,";")))
            NO-LOCK NO-ERROR.

            IF AVAIL op-entry THEN
            DO:
               IF    op-entry.op-date >= DataBlock.beg-date
               AND  op-entry.op-date <= DataBlock.end-date THEN
               /* В данном случае в mLstDate запихиваем подходящий op.op */
               mLstDate = IF mLstDate EQ "" THEN
                                             ENTRY(1,ENTRY(mI,mSurrOp,';')) + "," + ENTRY(2,ENTRY(mI,mSurrOp,';')) 
                       ELSE "," + ENTRY(1,ENTRY(mI,mSurrOp,';')) + "," +  ENTRY(2,ENTRY(mI,mSurrOp,';')).
            END.

         END.

         IF mLstDate > "" THEN
         DO:
            mKnigaOtr = GetXattrValueEx ("loan",
                                         loan.contract + "," + loan.cont-code,
                                         "КнигаОтр",
                                         "Полное").

            IF mKnigaOtr EQ "Полное" THEN RUN VerifyStat.
            ELSE
            DO:
               mSurrOp = mLstDate.
               {buybook1.i &RETURN1 = " return"}
            END.
         END.
         IF TRIM(mSurrOp) EQ "" THEN
         DO:
            IF       loan.open-date >= DataBlock.beg-date 
                 AND loan.open-date <= DataBlock.end-date THEN
            DO:
               IF mDopList NE "" THEN
                  IF       mDopListDate GE DataBlock.beg-date 
                       AND mDopListDate LE DataBlock.end-date THEN
                     mDopList = "".
                  ELSE
                     mDopList = "Нет".
               RUN VerifyStat.
            END.
            ELSE
            DO:
               IF     mDopList     NE ""
                  AND mDopListDate GE DataBlock.beg-date 
                  AND mDopListDate LE DataBlock.end-date THEN
               DO:
                  mDopList = "Да".
                  RUN VerifyStat.
               END.
            END.
         END.
      END.
      ELSE
      DO:
         IF  loan.loan-status EQ "Отчетный"
         AND mOnBook          EQ "Нет" THEN
         DO:
            ASSIGN
               mLstDate = ""
               mSF-OP-Links = "sf-op-nds"
            .
            /* Определяем все связанные с ней проводки учета НДС */
            mSurrOp = GetLinks(loan.class-code,                      /* ID класса */
                               loan.contract + "," + loan.cont-code, /* ID(cуррогат) объекта */
                               ?,                                    /* Направление связи: s | t | ? */
                               "sf-op-nds",                          /* Список кодов линков в CAN-DO формате */
                               ";",                                  /* Разделитель результирующего списка */
                               ?).

            IF mSurrOp > ""  THEN
            DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
               FIND FIRST op-entry WHERE 
                          op-entry.op       = INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
                      AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI,mSurrOp,";")))
               NO-LOCK NO-ERROR.
               IF AVAIL op-entry THEN
                  mLstDate = mLstDate + STRING(op-entry.op-date) + ",".
            END.

            /* Попадание даты регистрации в период блок данных */
            IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate)) THEN
            RUN VerifyStat.
            ELSE
            DO:
               mLstDate = "".
               mSF-OP-Links = "sf-op-pay".
               /* Определяем все связанные с ней проводки sf-op-pay */
               mSurrOp = GetLinks(loan.class-code,                      /* ID класса */
                                  loan.contract + "," + loan.cont-code, /* ID(cуррогат) объекта */
                                  ?,                                    /* Направление связи: s | t | ? */
                                  "sf-op-pay",                          /* Список кодов линков в CAN-DO формате */
                                  ";",                                  /* Разделитель результирующего списка */
                                  ?).
               IF mSurrOp > ""  THEN
               DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
                  FIND FIRST op-entry WHERE 
                             op-entry.op       = INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
                         AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI,mSurrOp,";")))
                  NO-LOCK NO-ERROR.
                  IF AVAIL op-entry THEN
                     mLstDate = mLstDate + STRING(op-entry.op-date) + ",".
               END.
               /* Попадание даты регистрации в период блок данных */
               IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate)) THEN
               RUN VerifyStat.
            END.
         END.
         ELSE 
         RUN VerifyStat.
      END.
   END.
   mDopList = "". /* Для того чтобы далее была верная обработка,
                     Возможно необходимо данную переменную вычислять для каждого договора. */
   teot:
   FOR EACH loan WHERE 
            loan.filial-id   EQ shFilial  
        AND loan.contract    EQ mContract
        AND loan.open-date   <= DataBlock.end-date
        AND (loan.loan-status EQ "Текущий"                /* данный статус возможен только для авансового типа оплаты */
        OR   loan.loan-status EQ "Отчетный")
/*        AND CAN-DO(mBrList, loan.branch-id)*/
   NO-LOCK:
      
      mSgUch = GetXattrValueEx("loan",
                          loan.contract + "," + loan.cont-code,
                          "КнигаУч",
                          "").
      
      IF mSgUch = ENTRY(2,mListUch,"/") THEN NEXT teot.

      mLstDate = "".

      IF loan.loan-status EQ "Текущий" THEN
      DO:
         mSF-OP-Links = "sf-op-nds".
         /*определяем все связанные с ней проводки учета НДС */
         mSurrOp = GetLinks(loan.class-code,                      /* ID класса */
                            loan.contract + "," + loan.cont-code, /* ID(cуррогат) объекта */
                            ?,                                    /* Направление связи: s | t | ? */
                            "sf-op-nds",                          /* Список кодов линков в CAN-DO формате */
                            ";",                                  /* Разделитель результирующего списка   */
                            ?).
      
         IF mSurrOp > "" THEN
         DO mI = 1 TO NUM-ENTRIES(mSurrOp, ";"):
            FIND FIRST op-entry WHERE 
                       op-entry.op       = INT64(ENTRY(1,ENTRY(mI, mSurrOp, ";")))
                   AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI, mSurrOp, ";")))
            NO-LOCK NO-ERROR.
            IF AVAIL op-entry THEN
            mLstDate = mLstDate + STRING(op-entry.op-date) + ",".
         END.
      
         /* Попадание даты регистрации в период блок данных */
         IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate) EQ NO) THEN
         DO:
            /* Ищем связанный договор АХД */
            FIND FIRST bloan WHERE
                       bloan.filial-id EQ loan.filial-id
                   AND bloan.contract  EQ loan.parent-contract
                   AND bloan.cont-code EQ loan.parent-cont-code
            NO-LOCK NO-ERROR.
      
            IF AVAIL bloan THEN
            DO:
               /* Определяем, есть ли в графике по учету НДС запись, с датой, попадающей в период блока данных */
               FOR EACH term-obl WHERE 
                        term-obl.contract  EQ bloan.contract
                    AND term-obl.cont-code EQ bloan.cont-code 
                    AND term-obl.idnt      EQ 4
                    AND term-obl.end-date  >= DataBlock.beg-date
                    AND term-obl.end-date  <= DataBlock.end-date
                    AND term-obl.nn        EQ term-obl.nn
               NO-LOCK:
                  mLstDate = mLstDate + STRING(term-obl.end-date) + ",".
               END.
      
               IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate)) THEN
               DO:
                  {buybook#.i &RETURN1 = " return"}
               END.
            END.
         END.
         ELSE
         DO:
            {buybook1.i &RETURN1 = " return"}
         END.
      END.

      IF loan.loan-status EQ "Отчетный" THEN
      IF mSgUch = ENTRY(1,mListUch,"/") THEN RUN VerifyStat.

   END.
END. /*Main_Block*/
{intrface.del}
   
STATUS DEFAULT "".
RETURN "".

/*Обработка счета-фактуры (заполнение основных атрибутов счета-фактуры)*/
PROCEDURE CalcSF:
   DEF PARAM BUFFER loan FOR loan.

   IF NOT AVAIL loan THEN RETURN.

   /* Если доп. рекв. КнигаУч не равен "Не отображать", 
   ** то отражаем в счет-фактуре */
   IF mSgUch <> ENTRY(2,mListUch,"/") THEN
   DO:
      /* Проверяем ДР КнигаОтр */
      mSgPart = GetXattrVAlueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "КнигаОтр",
                                "Полное"). /* по умолчанию - "полное" */

      /* если частичное, то нужен цикл */
      IF    mSgPart EQ "Частичное" 
         OR mSgPart EQ "Разделенное"
      THEN DO:
         /* Изменить впоследствии запрос Если Да или Если нет и не а/о */
         IF mOnBook EQ "Да"
         OR (loan.cont-type NE "а/о"
             AND mOnBook EQ "Нет")
         THEN
         DO:

            mSF-OP-Links = "sf-op-pay".
            mSurrOp = GetLinks(loan.class-code,                      /* ID класса     */
                               loan.contract + "," + loan.cont-code, /* ID(cуррогат) объекта   */
                               ?,                                    /* Направление связи: s | t | ? */
                               "sf-op-pay",                          /* Список кодов линков в CAN-DO формате */
                               ";",                                  /* Разделитель результирующего списка   */
                               ?).
            
            IF    TRIM(mSurrOp) NE ""
               OR GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "СуммыПлат" ) NE ""  THEN
            DO:
                {buybook1.i &RETURN1 = " return"}.
            END.
         END.
         ELSE
         DO:
            mSF-OP-Links = "sf-op-nds".
            mSurrOp = GetLinks(loan.class-code,                      /* ID класса     */
                               loan.contract + "," + loan.cont-code, /* ID(cуррогат) объекта   */
                               ?,                                    /* Направление связи: s | t | ? */
                               "sf-op-nds",                          /* Список кодов линков в CAN-DO формате */
                               ";",                                 /* Разделитель результирующего списка   */
                               ?).
            
            IF    mSurrOp > ""
               OR GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "СуммыПлат" ) NE ""  THEN
            DO:
               {buybook1.i &RETURN1 = " return"}.
            END.
         END.
      END.
      ELSE
      DO:
         IF mOnBook EQ "Да" THEN
         DO:
            {buybook#.i &RETURN1 = " return"}
         END.
         ELSE
         IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, STRING(loan.end-date) + ",")) THEN
         DO:
            {buybook#.i &RETURN1 = " return"}
         END.
      END.
   END. 
END PROCEDURE.


PROCEDURE VerifyStat.

   /* Обработка статуса Отчетный */
   IF loan.loan-status EQ "Отчетный" THEN
   IF (mOnBook EQ "Да") THEN
   RUN CalcSf(BUFFER loan).
   ELSE
   DO:
      /* Проверяем ДР КнигаОтр */
      mSgPart = GetXattrVAlueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "КнигаОтр",
                                "Полное"). /* по умолчанию - "полное" */

      /* если частичное, то нужен цикл */
      IF mSgPart     EQ "Частичное"
      OR mSgPart     EQ "Разделенное"
      OR mContract   EQ "sf-in" THEN
      DO:
         mSF-OP-Links = "sf-op-nds".
         /* Определяем все связанные с ней проводки учета НДС */
         mSurrOp = GetLinks(loan.class-code,                      /* ID класса */
                            loan.contract + "," + loan.cont-code, /* ID(cуррогат) объекта */
                            ?,                                    /* Направление связи: s | t | ? */
                            "sf-op-nds",                          /* Список кодов линков в CAN-DO формате */
                            ";",                                  /* Разделитель результирующего списка */
                            ?).
         IF    mSurrOp > ""
            OR GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "СуммыПлат" ) NE ""  THEN
         DO:
            {buybook1.i &RETURN1 = " RETURN"}
         END.
      END.
      ELSE
      DO:
         {buybook#.i &RETURN1 = " return"}
      END.
   END.
   
   /* Отбираем все счет-фактуры, являющиеся исправлением данной счет-фактуры */
   IF loan.loan-status EQ "Аннулирован" THEN
   FOR EACH signs WHERE
            signs.CODE       EQ "Исправ"
        AND signs.FILE-NAME  EQ "loan" 
        AND signs.code-val   EQ STRING(loan.contract) + "," + 
                                STRING(loan.cont-code)
   NO-LOCK,
   FIRST bloan WHERE 
         bloan.contract   EQ ENTRY(1,signs.surrogate)
     AND bloan.cont-code  EQ ENTRY(2,signs.surrogate)
   NO-LOCK:
      /*Обработка счет-фактуры*/
      RUN CalcSf(BUFFER bloan).
   END.
   
   IF     loan.loan-status NE "Отчетный" 
      AND loan.loan-status NE "Аннулирован"
      AND loan.loan-status NE "Текущий" THEN
      RUN CalcSF(BUFFER loan).


END PROCEDURE.

/*prosignshkemL4Ql9AYnIoBV+P6OQ*/