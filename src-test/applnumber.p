/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Проставляет на договорах ДР НБКИ_НомерЗаявки и НБКИ_ДатаЗаявки
Как работает:   По выделенным договорам ФЛ
Место запуска:  Список договоров - Ctrl-G
Создан:         30.01.2018 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{tmprecid.def}          /** Используем информацию из броузера */
{intrface.get xclass}   /** Функции для работы с метасхемой */
{intrface.get tmess}

DEFINE VARIABLE cDR         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iK          AS INTEGER      NO-UNDO INIT 0.

FOR EACH tmprecid 
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan)      = tmprecid.id)
      AND (loan.cust-cat    = "Ч")
    NO-LOCK:

    cDR = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "PLDealID").
    IF (cDR <> "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "На договоре " + loan.doc-ref + " проставлен ДР 'ПайпЛайн: Номер заявки' = " + cDR).
        NEXT.
    END.

    cDR = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "PLDealDate").
    IF (cDR <> "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "На договоре " + loan.doc-ref + " проставлен ДР 'ПайпЛайн: Дата заявки' = " + cDR).
        NEXT.
    END.

    cDR = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "НБКИ_НомерЗаявки").
    IF (cDR <> "")
    THEN DO:
        RUN Fill-SysMes IN h_tmess ("", "", "0", "На договоре " + loan.doc-ref + " проставлен ДР 'НБКИ: Номер заявки' = " + cDR).
        NEXT.
    END.

    UpdateSigns("loan", loan.contract + "," + loan.cont-code, "НБКИ_НомерЗаявки", "З" + loan.doc-ref, NO).
    UpdateSigns("loan", loan.contract + "," + loan.cont-code, "НБКИ_ДатаЗаявки", STRING(loan.open-date), YES).
    iK = iK + 1.
END.

RUN Fill-SysMes IN h_tmess ("", "", "0", "Доп.реквизиты установлены на " + STRING(iK) + " договорах").
{intrface.del}
