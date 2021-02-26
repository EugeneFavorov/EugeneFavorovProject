/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Проверяет, на каком сервере запущена программа
Как работает:   Запускает команду Linux: uname -a
Параметры:      Возвращает имя сервера
Создан:         15.02.2018 Борисов А.В.
*/

DEFINE VARIABLE cText       AS CHARACTER    NO-UNDO.
DEFINE STREAM sUname.

OS-COMMAND SILENT uname -a > /tmp/uname.txt.
INPUT  STREAM sUname FROM /tmp/uname.txt.
IMPORT STREAM sUname UNFORMATTED cText.
INPUT  STREAM sUname CLOSE.
OS-DELETE /tmp/uname.txt.

RETURN ENTRY(2, cText, " ").
