/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      Письмо Нестеркиной
Что делает:     Ставит ДР groupOABS на 2-м и последующих р/с = 1-ому р/с
Как работает:   Метод ExtraChkUpd на acctb
Параметры:      
Место запуска:  
Создан:         11.09.2017 Борисов А.В.
*/

{globals.i}
{intrface.get xclass}
{pb_logit.i}

DEFINE INPUT PARAMETER iSurr AS CHARACTER NO-UNDO.

DEFINE VARIABLE cGR         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAccLst     AS CHARACTER    NO-UNDO INIT "407*,406*,40802*,40807*,40821*".
DEFINE VARIABLE cConLst     AS CHARACTER    NO-UNDO INIT "Транз1,Текущ,Расчет,СпецПА,КонЗдт,КонРез,Кон138".
DEFINE BUFFER ac1           FOR acct.
DEFINE VARIABLE cLog        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lNewLog     AS LOGICAL      NO-UNDO.
DEFINE VARIABLE tCorr       AS INTEGER      NO-UNDO.    /* Разница во времени */
tCorr = (TIMEZONE - TIMEZONE("+03:00")) * 60.

cLog = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99").
cLog = "/home2/bis/quit41d/log/chkgroup/pb_chkgroup-" + cLog + ".log".
lNewLog = (SEARCH(cLog) = ?).
RUN LogIt('Новый счет: "' + iSurr + '" - ' + USERID("bisquit") + ", дельта=" + STRING(tCorr, "HH:MM:SS") + ", TIME-60-дельта=" + STRING(TIME - 60 - tCorr, "HH:MM:SS"), cLog).
IF lNewLog THEN OS-COMMAND SILENT VALUE("chmod 777 " + cLog).   /* Если лог только что создан, разрешаем запись всем */

FIND LAST history
    WHERE (history.modify = "C")
      AND (history.file-name    = 'acct')
      AND (history.field-ref    = iSurr)
    NO-LOCK NO-ERROR.
IF (AVAIL history)
THEN RUN LogIt("  Создан: " + STRING(history.modif-date, "99.99.9999") + " "
             + STRING(history.modif-time, "HH:MM:SS"), cLog).
ELSE RUN LogIt("  Дата создания не найдена", cLog).

FOR FIRST acct
    WHERE (acct.acct            = ENTRY(1,iSurr))
      AND (acct.currency        = ENTRY(2,iSurr))
      AND (acct.filial-id       = "0500")
      AND CAN-DO(cAccLst, acct.acct)
      AND CAN-DO(cConLst, acct.contract)
      AND (acct.cust-cat       <> "В")
      AND (acct.class-code     <> "acctw4")
      AND CAN-DO("!0599,!0598,*", acct.branch-id)
    EXCLUSIVE-LOCK,
LAST history
    WHERE (history.modify = "C")
      AND (history.file-name    = 'acct')
      AND (history.field-ref    = iSurr)
      AND (history.modif-date   = TODAY)
      AND (history.modif-time   > TIME - 60 - tCorr)    /* Счет создан только что */
    NO-LOCK:

    cGR = "".
    FOR EACH ac1
        WHERE (ac1.filial-id    = acct.filial-id)
          AND (ac1.cust-cat     = acct.cust-cat)
          AND (ac1.cust-id      = acct.cust-id)
          AND CAN-DO(cAccLst, ac1.acct)
          AND CAN-DO(cConLst, ac1.contract)
          AND (ac1.close-date   = ?)
          AND (ac1.acct        <> ENTRY(1,iSurr))
        NO-LOCK
        BY ac1.open-date:

        cGR = GetXAttrValue("acct", ac1.acct + "," + ac1.currency, "groupOABS").
        IF (LENGTH(cGR) = 3) AND CAN-DO("!599,!598,*", cGR)
        THEN LEAVE.
        ELSE cGR = "".
    END.

    IF (cGR = "")
    THEN DO:    /* Старый счет не найден */
        DO WHILE TRUE:
            RUN g-prompt.p ("integer", "Группа сопровождения", "999", "",
                            "Укажите группу нового счета", 40, ?, ? /* iProcs */,
                            ?,?,OUTPUT cGR).
            IF      (cGR <> ?)
                AND CAN-DO(FGetSetting("ОткрСчетов", "ГруппаОФ", "581,582,583,584,585,586"), cGR)
            THEN LEAVE.
        END.
        RUN LogIt("  Старый счет не найден. Выбрана группа: " + cGR, cLog).
        UpdateSigns("acct", iSurr, "groupOABS", cGR, YES).
    END.
    ELSE DO:
        /* Перенос параметров с ac1 на acct */
        UpdateSigns("acct", iSurr, "groupOABS", cGR, YES).
        acct.branch-id = ac1.branch-id.
        RUN LogIt("  Старый счет найден: " + ac1.number + ". Группа=" + cGR + ", branch-id=" + ac1.branch-id, cLog).
        LEAVE.
    END.
END.

{intrface.del}          /* Выгрузка инструментария. */ 
