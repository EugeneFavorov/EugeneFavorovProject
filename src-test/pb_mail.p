/**
Авторские права принадлежат: Связной Банк (ЗАО)
Что делает:     отправка электронной почты из Бисквита
Как работает:   вызов unix-команды mail
Параметры:      1. Список п/я через "," и без сервера (до @)
                2. Тема письма
                3. Текст письма
                4. Список вложенных файлов
Пример:         RUN pb_mail.p ("a.borisov,s.strahov", "Ошибка в ...", cMess, SEARCH("sysmess.log") + "," + SEARCH("_spoolm.tmp")).
Создан:         13.05.2016 Борисов А.В.
*/

DEF INPUT PARAM iMail   AS CHARACTER NO-UNDO.   /* Список п/я */
DEF INPUT PARAM iSubj   AS CHARACTER NO-UNDO.   /* Тема */
DEF INPUT PARAM iMess   AS CHARACTER NO-UNDO.   /* Текст письма */
DEF INPUT PARAM iAdd    AS CHARACTER NO-UNDO.   /* Список вложенных файлов */

DEFINE VARIABLE cTmp    AS CHARACTER NO-UNDO INIT "".
DEFINE VARIABLE I       AS INTEGER   NO-UNDO.

iMail = replace(iMail,"^",",").
DO I = 1 TO NUM-ENTRIES(iAdd):
    cTmp = cTmp + ' -a ' + ENTRY(I, iAdd).
END.

cTmp = 'echo "' + iMess + '" | mail -s "' + iSubj + '" -r bis@plus-bank.ru' + cTmp
     + ' ' + REPLACE(iMail, ",", "@bankom.omsk.su ") + "@bankom.omsk.su".
OS-COMMAND SILENT VALUE(cTmp).
