/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      СЗ ОСРКО.1252
Что делает:     Создает регулярные платежи по классификатору РегулПлат
Место запуска:  Планировщик
Создан:         21.05.2018 Борисов А.В.
*/

{intrface.get date}
{intrface.get tmess}
{topkind.def}
IF HolidayRu(TODAY) THEN RETURN.

DEFINE VARIABLE dDat        AS DATE      NO-UNDO.
DEFINE VARIABLE cTranz      AS CHARACTER NO-UNDO INIT "РегулПлат".
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubj       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMail       AS CHARACTER NO-UNDO.

RUN Fill-SysMes IN h_tmess ("", "", "1", "Запуск pb_regulplat.p").
FOR EACH code
    WHERE (code.class   = 'РегулПлат')
      AND (code.parent  = 'РегулПлат')
      AND NOT (code.name BEGINS "-")
    NO-LOCK:

    dDat  = DATE(MONTH(TODAY), INT(ENTRY(3, code.val)), YEAR(TODAY)).
    cMail = code.description[3].

    DO WHILE HolidayRu(dDat):
        dDat = dDat + 1.
    END.

    IF (dDat = TODAY)
    THEN DO:
        /* Запуск транзакции  УТ */
        RUN Fill-SysMes IN h_tmess ("", "", "1", "Запуск УТ " + cTranz + "для правила " + code.code).
        {empty tOpKindParams}     /* очистить таблицу параметров */
        ASSIGN
            lOk =   TDAddParam("__icode" , code.code)
            NO-ERROR.
        IF NOT lOk
        THEN RUN LogIt("возникла ошибка при передаче параметров в транзакцию " + cTranz).
        ELSE DO:
            RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).

            IF NOT lOk
            THEN DO:
                RUN Fill-SysMes IN h_tmess ("", "", "1", "Ошибка в УТ " + cTranz + " : " + cErrMsg).
                cErrMsg = "Возникла ошибка в транзакции " + cTranz + " : " + cErrMsg
                        + "~n~rПравило " + code.code + " для счета " + code.name.
                cSubj   = "Regular payment : ERROR".
            END.
            ELSE DO:
                RUN Fill-SysMes IN h_tmess ("", "", "1", "УТ " + cTranz + " отработала без ошибок").
                cErrMsg = "Создан регулярный платеж со счета " + code.name.
                cSubj   = "Regular payment : OK".
            END.

            OUTPUT TO VALUE("/tmp/date.txt").
            PUT UNFORMATTED CODEPAGE-CONVERT(cErrMsg, "1251") SKIP.
            OUTPUT CLOSE.
            RUN pb_mail.p (cMail, cSubj, "", "/tmp/date.txt")).
            RUN Fill-SysMes IN h_tmess ("", "", "1", "Письмо отправлено").
        END.
    END.
END.
RUN Fill-SysMes IN h_tmess ("", "", "1", "Окончание pb_regulplat.p").
{intrface.del}
