/**
Авторские права принадлежат: ПАО "Плюс Банк"
Что делает:     Процедура урегулирования К2 - КБС на помеченных л/с
Как работает:   
                Должен существовать каталог: /home2/bis/quit41d/log/k2-kbs/
Место запуска:  Список счетов - Ctrl-G
Создан:         31.03.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{tmprecid.def}          /** Используем информацию из броузера */
{intrface.get filex}
{intrface.get netw}     /* Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

{pb_k2init.i}

FOR EACH tmprecid 
    NO-LOCK,
FIRST acct
    WHERE (RECID(acct)  EQ tmprecid.id)
    NO-LOCK:

    RUN pb_k2kbs-1.p(acct.acct, acct.currency, "ДА", "", cLog, cPrt, INPUT-OUTPUT lPrtFirst).
END.

{pb_k2prot.i}
{intrface.del}
