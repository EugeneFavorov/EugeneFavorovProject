/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      
Что делает:     Изменение статуса пакета
Место запуска:  СООБЩЕНИЯ - Ctrl-G
Создан:         24.10.2017 Борисов А.В.
*/

{globals.i}
{tmprecid.def}          /** Используем информацию из броузера */

DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

FOR FIRST tmprecid 
    NO-LOCK,
FIRST Packet
    WHERE (RECID(Packet)        EQ tmprecid.id)
    EXCLUSIVE-LOCK:

    RUN g-prompt.p ("CHARACTER", "Статус", "x(20)", "СОЗД", "Введите новый статус", 25, ",", "", ?,?,OUTPUT cTmp).
    IF (cTmp EQ ?) THEN RETURN.

    Packet.State = cTmp.
END.
