/* **************************************************************************** */
/* Отправка протокола на комп.пользователя по BisPC                             */
/* **************************************************************************** */

IF NOT lPrtFirst
THEN DO:
    /* Завершаем протокол */
    OUTPUT TO VALUE(cPrt) APPEND.
    PUT UNFORMATTED XLEnd().
    OUTPUT CLOSE.

    /* Перед отправкой протокола проверим, запущен ли bispc */
    DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
    DO WHILE (mRet EQ ""):
        RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
        IF mRet EQ "" THEN
           MESSAGE 
              "Запустите программу bispc и нажмите ОК"
           VIEW-AS ALERT-BOX.
    END.

    /* Отправляем протокол */
    RUN sndbispc.p ("file=" + cPrt + ";class=bq").

    OS-COPY VALUE(cPrt) "".
END.

MESSAGE "Закончено урегулирование К2 - КБС."
    IF lPrtFirst THEN "~nНи один счет не обработан."
                 ELSE "~nФайл протокола  " + cPrtF + " сформирован." /* "~nнаходится в каталоге  C:/BisPC/." */
    VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
