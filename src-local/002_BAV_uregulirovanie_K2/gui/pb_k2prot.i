
/* +++ pb_k2prot.i was humbly modified by (c)blodd converter v.1.09 on 8/23/2016 11:28am +++ */

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
           RUN Fill-AlertSysMes IN h_tmess("","",1,"Запустите программу bispc и нажмите ОК").

    END.

    /* Отправляем протокол */
    RUN sndbispc.p ("file=" + cPrt + ";class=bq").

    OS-COPY VALUE ( {&RELATIVE_2_ABSOLUTE}( cPrt ) ) "".
END.

RUN Fill-AlertSysMes IN h_tmess("","",1,"Закончено урегулирование К2 - КБС." + CHR(32) + IF lPrtFirst THEN "~nНи один счет не обработан." ELSE "~nФайл протокола " + STRING(cPrtF) + " сформирован." /* "~nнаходится в каталоге C:/BisPC/." */).


/* --- pb_k2prot.i was humbly modified by (c)blodd converter v.1.09 on 8/23/2016 11:28am --- */
