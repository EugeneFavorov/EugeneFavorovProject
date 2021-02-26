/**
Авторские права принадлежат: ПАО "Плюс Банк"
Что делает:     Процедуры для урегулирования К2 - КБС
Создан:         30.03.2016 Борисов А.В.
*/

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}
{intrface.get strng}    /** Функции для работы со строками */
{intrface.get op}       /* проц. IsBudgetPayment */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/* **************************************************************************** */
/* Вычисляет массив блокировок на заданном счете для каждой очередности (0 - 5) */
/* Результат:   <Бл-ки для очер-ти 0>,...,<Бл-ки для очер-ти 5>;<все бл-ки>     */
/*              где <Блокировки для очередности n> :                            */
/*            - <пусто> - блокировок для очередности нет,                       */
/*            - КонкПр  - конкурсное производство,                              */
/*            - Дб      - очередность блокирована (Блок или БлокДб),            */
/*            - <сумма> - очередность блокирована на указанную сумму            */
/*                        (БлокСумм или БлокТамож)                              */
/* **************************************************************************** */
FUNCTION BlockArr   RETURNS CHARACTER
   (INPUT  iSurr    AS CHARACTER ).    /* суррогат счета <acct,curr> */

    DEFINE VARIABLE cBlOrd      AS CHARACTER NO-UNDO EXTENT 6 INIT "". /* Результат */
    DEFINE VARIABLE I           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBlTyp      AS CHARACTER NO-UNDO.           /* тип блокировки из классификатора acct-status, misc[1] */
    DEFINE VARIABLE nBlSum      AS DECIMAL   NO-UNDO.           /* сумма, блокированная blockobject */
    DEFINE VARIABLE dNow        AS DATETIME  NO-UNDO.           /* TODAY */
    DEFINE VARIABLE cBlockType  AS CHARACTER NO-UNDO.           /* Типы блкировок для переноса на КБС */
    DEFINE VARIABLE cBlOrdL     AS CHARACTER NO-UNDO.           /* Список очередностей блокировки */
    DEFINE VARIABLE lKonkPr     AS LOGICAL   NO-UNDO INIT NO.   /* Признак конкурсного производства */
    DEFINE VARIABLE lBankrot    AS LOGICAL   NO-UNDO INIT NO.   /* Признак банкротства */

    DEFINE VARIABLE cBlockPrt   AS CHARACTER NO-UNDO EXTENT 6 INIT "". /* Блокировки для протокола: */
                                                                /* Блок; БлокДб; БлокСумм Арест; БлокСумм 1,2; БлокСумм 1,2,3; Остальные */
    DEFINE VARIABLE nSumA       AS DECIMAL   NO-UNDO EXTENT 3 INIT 0.    /* Сумма по всем БлокСумм Арест/ 1,2/ 1,2,3 */
    DEFINE VARIABLE cAllBl      AS CHARACTER NO-UNDO INIT "".   /* Список всех блокировок для протокола */

    cBlockType = FGetSetting("КартБлСч", "ТипыБлокК", "") + ",КонкПр,Банкрот".
    dNow = DATETIME(TODAY, MTIME).

    /* Дергаем ORACLE чтобы избежать "Memory violation" */
    FIND FIRST blockobject
        NO-LOCK NO-ERROR.
    
    FOR EACH blockobject
        WHERE (blockobject.file-name     EQ 'acct')
          AND (blockobject.class-code    EQ 'BlockAcct')
          AND (blockobject.beg-datetime  LE dNow)
          AND ((blockobject.end-datetime EQ ?)
            OR (blockobject.end-datetime GE dNow))
          AND (blockobject.surrogate     EQ iSurr)
/*        AND (blockobject.txt[1]        NE "*") */
        NO-LOCK
        BY blockobject.beg-datetime:

        /* тип блокировки и блокированная сумма */
        cBlTyp  = GetCodeMisc("acct-status", blockobject.block-type, 1).
        nBlSum  = IF ((ENTRY(2, iSurr) EQ "") AND (blockobject.val[3] EQ 0)) THEN blockobject.val[4] ELSE blockobject.val[3].

        /* Добавляем блокировку в список для протокола */
        IF      (blockobject.txt[1] EQ "")              /* Пустая Очередность и "Арест" в Типе постановления */
            AND (INDEX(blockobject.txt[3], "Арест") NE 0)
        THEN cBlOrdL = "Арест".
        ELSE cBlOrdL = blockobject.txt[1].

        CASE blockobject.block-type:
            WHEN "КонкПр"   THEN .
            WHEN "Банкрот"  THEN .
            WHEN "Блок"     THEN cBlockPrt[1] = "Блок".
            WHEN "БлокДб"   THEN cBlockPrt[2] = "БлокДб".
            OTHERWISE DO:
                IF (cBlTyp EQ "БлокСумм")
                THEN DO:
                    IF (cBlOrdL EQ "Арест")
                    THEN ASSIGN
                            cBlockPrt[3] = "БлокСумм Арест"
                            nSumA[1]     = nSumA[1] + nBlSum.
                    IF (cBlOrdL EQ "1,2")
                    THEN ASSIGN
                            cBlockPrt[4] = "БлокСумм 1,2"
                            nSumA[2]     = nSumA[2] + nBlSum.
                    IF (cBlOrdL EQ "1,2,3")
                    THEN ASSIGN
                            cBlockPrt[5] = "БлокСумм 1,2,3"
                            nSumA[3]     = nSumA[3] + nBlSum.
                END.
                ELSE DO:
                    IF   NOT CAN-DO(cBlockPrt[6], blockobject.block-type)
                    THEN {additem.i cBlockPrt[6]  blockobject.block-type}
                END.
            END.
        END CASE.

        nBlSum  = ABSOLUTE(nBlSum).

        IF NOT CAN-DO(cBlockType, blockobject.block-type)   /* Эта блокировка на КБС не ставит */
        THEN NEXT.

        /* Конкурсное производство и Банкротство перекрывают все */
        IF (blockobject.block-type EQ "КонкПр")
        THEN lKonkPr = YES.
        IF (blockobject.block-type EQ "Банкрот")
        THEN lBankrot = YES.
        IF (lKonkPr OR lBankrot) THEN NEXT.

        IF (cBlTyp NE ?)   /* неизвестные блокировки пропускаем, хотя вряд ли они попадут в НП КартБлСч/ТипыБлокК */
        THEN DO:
            DO I = 1 TO 5:
                IF (cBlTyp EQ "БлокСумм")
                THEN DO:
                    IF NOT CAN-DO(blockobject.txt[1], STRING(I))    /* Очередность подпадает под эту блокировку */
                    THEN
                        CASE cBlOrd [I + 1]:
                            WHEN ""    THEN cBlOrd[I + 1] = STRING(nBlSum).                             /* не было блокировок => сумма        */
                            WHEN "Дб"  THEN .                                                           /* уже стоит более сильная блокировка */
                            OTHERWISE       cBlOrd[I + 1] = STRING(DECIMAL(cBlOrd[I + 1]) + nBlSum).    /* еще одна сумма => суммируем        */
                        END CASE.
                END.
                ELSE DO:
                    IF NOT CAN-DO(blockobject.txt[1], STRING(I))    /* Очередность подпадает под эту блокировку */
                    THEN cBlOrd [I + 1] = "Дб".                     /* Перекрывает все, что было                */
                END.
            END.
        END.
        cBlOrd[1] = cBlOrd[2]. /* Блокировки для очередности 0 такие же, как и для очередности 1 */
    END.

    /* Объединяем блокировки для протокола */
    DO I = 1 TO 6:
        IF (cBlockPrt[I] NE "")
        THEN DO:
            CASE I:
                WHEN 1 OR WHEN 2 OR WHEN 6 THEN
                    {additem2.i cAllBl cBlockPrt[I] "|"}
                WHEN 3 OR WHEN 4 OR WHEN 5 THEN DO:
                    cBlockPrt[I] = cBlockPrt[I] + " (" + STRING(nSumA[I - 2]) + ")".
                    {additem2.i cAllBl cBlockPrt[I] "|"}
                END.
            END CASE.
        END.
    END.

    IF lKonkPr
    THEN RETURN "КонкПр,КонкПр,КонкПр,КонкПр,КонкПр,КонкПр;КонкПр|" + cAllBl.
    ELSE IF lBankrot
    THEN RETURN "Банкрот,Банкрот,Банкрот,Банкрот,Банкрот,Банкрот;Банкрот|" + cAllBl.
    ELSE RETURN cBlOrd[1] + "," + cBlOrd[2] + "," + cBlOrd[3] + "," + cBlOrd[4] + "," + cBlOrd[5] + "," + cBlOrd[6] + "," + cBlOrd[6] + ";" + cAllBl.
        /* Еще одна cBlOrd[6] в конце - на всякий случай, если где-то завалялись документы 6-й очередности */
END FUNCTION.


/* **************************************************************************** */
/* Формируем новую запись в таблице ttKauCrt                                    */
/* **************************************************************************** */
PROCEDURE New_ttKauCrt:
    DEFINE INPUT  PARAMETER iCrt    AS CHARACTER    NO-UNDO.    /* Картотека: K2 или KBS */

    DEFINE VARIABLE cOrd        AS CHARACTER    NO-UNDO.    /* Очередность исходная     */
    DEFINE VARIABLE iOrdP       AS INTEGER      NO-UNDO.    /* Очередность для переноса */
    DEFINE VARIABLE cDet        AS CHARACTER    NO-UNDO.    /* Назначение платежа  */
    DEFINE VARIABLE cAcc        AS CHARACTER    NO-UNDO.    /* л/с исх.документа   */
    DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cCrtCur     AS CHARACTER    NO-UNDO.    /* валюта картотеки    */
    DEFINE VARIABLE lNal        AS LOGICAL      NO-UNDO.    /* Бюджетный платеж    */
    DEFINE BUFFER op-bal FOR op.
    DEFINE BUFFER oe-bal FOR op-entry.

    /* По первичному документу выясняем : очередность (cOrd), назначение платежа (cDet) и л/с аналитики (cAcc) */
    cCrtCur = IF (iCrt EQ "K2") THEN cCurrK2 ELSE cCurrKBS.
    cDet = "".
    cAcc = "".

    /* Если не найдем исх.документ, то очередность - по документу постановки на К2 */
    FOR FIRST op-bal
        WHERE (op-bal.op EQ INT(ENTRY(1, kau.kau)))
        NO-LOCK:

        cOrd  = op-bal.order-pay.
        iOrdP = INT(cOrd).
    END.

    cDR  = GetXAttrValue("op", ENTRY(1, kau.kau), "op-bal").    /* Исходный документ, поставленный на картотеку */
    IF (cDR NE "")
    THEN DO:
        FIND FIRST op-bal
            WHERE (op-bal.op    EQ INT64(cDR))
            NO-LOCK NO-ERROR.

        IF (AVAIL op-bal)
        THEN DO:
            cOrd  = op-bal.order-pay.
            iOrdP = INT(cOrd).

            RUN IsBudgetPayment IN h_op(op-bal.op, OUTPUT lNal).   /* Бюджетные платежи по 3 очередности */    
            IF lNal AND (iOrdP GE 4)
            THEN iOrdP = 3.

            cDet = op-bal.details.
            cAcc = GetXAttrValue("op", cDR, "acctbal").

            /* Если ДР acctbal пустой, то берем счет с acct-db проводки */
            FIND FIRST oe-bal OF op-bal
                NO-LOCK NO-ERROR.
            IF (cAcc EQ "") AND (AVAIL oe-bal)
            THEN cAcc = oe-bal.acct-db.
        END.
    END.

    /* Если счет Дб исх.док-та не совпадает с л/с, то аналитику пропускаем */
    IF    ((cAcc NE "") AND (cAcc EQ iAcct))
        OR (cAcc EQ "")
    THEN DO:
        CREATE ttKauCrt.
        ASSIGN
            ttKauCrt.crt        = iCrt
            ttKauCrt.crtn       = iCrt
            ttKauCrt.kau        = kau.kau
            ttKauCrt.sort       = ENTRY(1, kau.sort) + "," + ENTRY(2, kau.sort)
            ttKauCrt.opdate     = DATE(INT64(SUBSTRING(ENTRY(2,kau.sort),6,2)),
                                       INT64(SUBSTRING(ENTRY(2,kau.sort),9,2)),
                                       INT64(SUBSTRING(ENTRY(2,kau.sort),1,4)))
            ttKauCrt.numop      = kau.numop
            ttKauCrt.balance    = (IF (cCrtCur EQ "") THEN kau.balance      ELSE kau.curr-bal)
            ttKauCrt.firstsum   = DEC(ENTRY(3,kau.sort))
            ttKauCrt.summa      = 0
            ttKauCrt.order-pay  = iOrdP
            ttKauCrt.order-sym  = IF (CAN-DO("4,5", cOrd) AND lNal) THEN (cOrd + "(налог)") ELSE cOrd
            ttKauCrt.order-new  = IF  CAN-DO("4,5", cOrd)           THEN (iOrdP + 1)        ELSE iOrdP
            ttKauCrt.details    = cDet
            ttKauCrt.action     = ""
            ttKauCrt.eerror     = ""
            ttKauCrt.block      = ENTRY(iOrdP + 1, cBlOrdL)
            ttKauCrt.blocksum   = (IF CAN-DO("КонкПр,Банкрот,Дб,", ttKauCrt.block) THEN 0 ELSE DEC(ttKauCrt.block))
            ttKauCrt.priost     = (GetXAttrValue('op', ENTRY(1, kau.kau), "ПриостСпис") EQ "Да")
            .
    END.
END PROCEDURE.

/* **************************************************************************** */
/* Записываем информацию по счету в протокол XL                                 */
/* **************************************************************************** */
PROCEDURE XLProtokol:
    DEFINE INPUT  PARAMETER cPrt    AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER lFirst  AS LOGICAL      NO-UNDO.

    OUTPUT TO VALUE(cPrt) APPEND.

    DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.

    IF lFirst
    THEN DO:
        IF (shFilial EQ "0500")
        THEN PUT UNFORMATTED XLHead("ul", "CCNCCDNCCC", "213,53,103,200,96,71,103,89,122,171").
        ELSE PUT UNFORMATTED XLHead("ul", "CNCCDNCCC",  "213,103,200,96,71,103,89,122,171").

        cXL = XLCellHead("Счет",0,0,0)
            + (IF (shFilial EQ "0500") THEN XLCellHead("Группа счета",0,0,0) ELSE "")
            + XLCellHead("Остаток на р/с",0,0,0)
            + XLCellHead("Блокировки",0,0,0)
            + XLCellHead("Очередность по документу",0,0,0)
            + XLCellHead("Док дата",0,0,0)
            + XLCellHead("Остаток",0,0,0)
            + XLCellHead("Находился в картотеке",0,0,0)
            + XLCellHead("Выполнена операция",0,0,0)
            + XLCellHead("Комментарии (ошибки)",0,0,0)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.
    ELSE DO:
        PUT UNFORMATTED XLRow(0) XLEmptyCell() XLRowEnd().  /* Л/с разделяем в протоколе пустой строкой */
    END.

    FOR EACH ttKauCrt
            WHERE (IF (iPrtAll EQ "ДА") THEN YES ELSE (ttKauCrt.action NE ""))
        NO-LOCK
        BY ttKauCrt.order-new
        BY ttKauCrt.sort:

        cXL = XLCell(iAcct)
            + (IF (shFilial EQ "0500") THEN XLCell(GetLinks("acct", iAcct + "," + iCurr, "s", "acct-group", ",", TODAY)) ELSE "")
            + XLNumCell(nAcPos)
            + XLCellWrap(REPLACE(cAllOrd, "|", "&#10;"))
            + XLCell(ttKauCrt.order-sym)
            + XLDateCell(ttKauCrt.opdate)
            + XLNumCell(ttKauCrt.balance)
            + XLCell(ttKauCrt.crt)
            + XLCellWrap(REPLACE(ttKauCrt.action, "|", "&#10;"))
            + XLCellWrap(ttKauCrt.eerror)
            .
        PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.

    OUTPUT CLOSE.
END PROCEDURE.
