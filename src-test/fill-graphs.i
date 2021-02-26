/* =================================-==-===
** Инструменты для рассчета графиков ЭПС */

/* =========================================-==-===
** Возвращает реквизит из "сложного" списка
** комиссия1=вид оп.1,комиссия2=вид оп.2  и т.д. */
FUNCTION GetParsSett RETURNS CHAR
  (INPUT iPar AS INT64,         /* 1 Комиссия, 2 Вид опер. */
   INPUT iQty  AS INT64,        /* Порядковый номер в строке iStr */
   INPUT iStr AS CHAR).         /* Строка со списком */
   RETURN ENTRY(iPar, ENTRY(iQty, iStr), "=").
END FUNCTION.

/* =============================-===
** True  - Одна запись в Term-obl */
PROCEDURE IsOneRecOnTerm-Obl.
    DEF INPUT  PARAM iContract AS CHAR NO-UNDO.    /* Назначение договора */
    DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.    /* Номер договора */
    DEF INPUT  PARAM iDate     AS DATE NO-UNDO.    /* На дату */
    DEF OUTPUT PARAM oIsRealTr AS LOG  NO-UNDO.    /* Признак единственного обязательства ОД */
    DEF OUTPUT PARAM oAmntTr   AS DEC  NO-UNDO.    /* Обяз-во по ОД  */

    DEF VAR vCount   AS INT64 NO-UNDO.     /* Кол-во записей условий */

    /* Выясняем количество записей в графике ОД равно 1 или нет */
    FOR EACH b-term-obl
        WHERE b-term-obl.contract  EQ iContract
          AND b-term-obl.cont-code EQ iContCode
          AND b-term-obl.idnt      EQ 3
          AND b-term-obl.end-date  LE iDate
        NO-LOCK:

        ASSIGN
            vCount  = vCount + 1
            oAmntTr = oAmntTr + b-term-obl.amt-rub
            .
    END.

    /* Если запись только одна, то необходимо строить виртуальный график.
    ** В противном случае копировать один-в-один, т.е. как есть */
    IF vCount EQ 1
    THEN oIsRealTr = TRUE.
END PROCEDURE.

/* ============================================================-==-===
** Копирование графика платежей по ОД во временную таблицу tt-term-obl
** без корректировки */
PROCEDURE CopyTTData.
    DEF INPUT PARAM iContract AS CHAR   NO-UNDO. /* Назначение договора */
    DEF INPUT PARAM iContCode AS CHAR   NO-UNDO. /* Номер договора */
    DEF INPUT PARAM iIdnt     AS INT64  NO-UNDO. /* Тип обязательства */
    DEF INPUT PARAM iDatR     AS DATE   NO-UNDO. /* Дата перевода графика на план */
    DEF INPUT PARAM iDate     AS DATE   NO-UNDO. /* Дата окончания договора */

    /* Пробежка по обязательствам после даты расчета */
    FOR EACH b-term-obl
        WHERE b-term-obl.contract  EQ iContract
          AND b-term-obl.cont-code EQ iContCode
          AND b-term-obl.idnt      EQ iIdnt
          AND b-term-obl.end-date  GE iDatR
          AND b-term-obl.end-date  LE iDate
          AND b-term-obl.amt-rub   NE 0
        NO-LOCK:

        FIND FIRST tt-term-obl OF b-term-obl
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL tt-term-obl
        THEN tt-term-obl.amt-rub = tt-term-obl.amt-rub + b-term-obl.amt-rub.
        ELSE DO:
            CREATE tt-term-obl.
            BUFFER-COPY b-term-obl TO tt-term-obl.
        END.
    END.
END PROCEDURE.

/* ==================================-==-===
** Сохранение данных во временной таблице */
PROCEDURE CrtRepTbl.
    DEF INPUT PARAM iNum   AS INT64 NO-UNDO.    /* Код (номер) операции */
    DEF INPUT PARAM iDate  AS DATE  NO-UNDO.    /* Дата операции */
    DEF INPUT PARAM iSumma AS DEC   NO-UNDO.    /* Сумма операции */

    IF (iSumma EQ 0) THEN RETURN.
/*
IF (iDate EQ 07/14/2016)
THEN DO:
    MESSAGE "OP2 - " iNum iDate iSumma
        VIEW-AS ALERT-BOX QUESTION BUTTONS OK.
    
END.
*/
    /* Ищем запись во временной таблице графика по дате */
    FIND FIRST ttReport
        WHERE ttReport.tf_payment-date EQ iDate
        NO-LOCK NO-ERROR.
    /* Если не нашли, тогда надо создать запись на дату */
    IF NOT AVAIL ttReport
    THEN DO:
        CREATE ttReport.
        ttReport.tf_id = 0.    /* Потом перенумеруем */
        tf_payment-date = iDate.
    END.

    /* Проценты присутствуют в каждой строке, остальные могут отсутствовать * /
    IF iNum EQ 1
    THEN ttReport.tf_id = iID. */

    /* Разбираемся с видом операции по ее коду и накапливаем сумму */
    CASE iNum:
        WHEN 1 THEN     /* Проценты */
            ttReport.tf_sum-percent        = ttReport.tf_sum-percent        + iSumma.
        WHEN 2 THEN     /* Остатки */
            ttReport.tf_rest-debts         = ttReport.tf_rest-debts         + iSumma.
        WHEN 3 THEN     /* Обязательства по погашению основного долга */
            ttReport.tf_basic-sum-loan     = ttReport.tf_basic-sum-loan     + iSumma.
    END CASE.

    myLastDate  = MAXIMUM(myLastDate, iDate).
END PROCEDURE.