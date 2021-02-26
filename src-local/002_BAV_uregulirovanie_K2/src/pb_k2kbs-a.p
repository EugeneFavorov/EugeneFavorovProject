/**
Авторские права принадлежат: ПАО "Плюс Банк"
Что делает:     Процедура урегулирования К2 - КБС на всех л/с
Как работает:   На всех л/с с К2 запускается урегулирование картотек К2 - КБС.
                Должен существовать каталог: /home2/bis/quit41d/log/k2-kbs/
Место запуска:  Транзакция 
Создан:         08.04.2016 Борисов А.В.

*/

{globals.i}             /** Глобальные определения */
{intrface.get netw}     /* Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

{pb_k2init.i}
DEF VAR mGroup      AS CHAR NO-UNDO INIT "*".

IF (shFilial EQ "0500")
THEN DO:
   RUN g-prompt.p ("char", "Группы ", "x(100)", "*", "Введите группы счетов (F1 - выбор)", 50, ",", "F1=pb_getgroup.p",?,?,OUTPUT mGroup).
   IF (mGroup EQ ?) THEN RETURN.

    FOR EACH acct
        WHERE (acct.acct            BEGINS "40")
          AND (acct.filial-id       EQ shFilial)
          AND (acct.close-date      EQ ?)
       NO-LOCK,
    EACH xlink
       WHERE (xlink.class-code      EQ 'acctb')
         AND (xlink.link-code       EQ 'acct-group')
         AND (xlink.link-direction  EQ 's')
       NO-LOCK,
    EACH links
       WHERE (links.link-id         EQ xlink.link-id)
         AND (links.source-id       EQ acct.acct + "," + acct.currency)
         AND (links.end-date        EQ ?)
         AND CAN-DO(mGroup, links.target-id)
        NO-LOCK
        BREAK BY acct.acct:

        IF   (GetXAttrValue("acct", acct.acct + "," + acct.currency, "Карт2ВнСчет")
            + GetXAttrValue("acct", acct.acct + "," + acct.currency, "КартБВнСчет") NE "")
        THEN RUN pb_k2kbs-1.p(acct.acct, acct.currency, "НЕТ", "", cLog, cPrt, INPUT-OUTPUT lPrtFirst).
    END.
END.
ELSE DO:
    FOR EACH acct
        WHERE (acct.acct            BEGINS "40")
          AND (acct.filial-id       EQ shFilial)
          AND (acct.close-date      EQ ?)
        NO-LOCK
        BREAK BY acct.acct:

        IF   (GetXAttrValue("acct", acct.acct + "," + acct.currency, "Карт2ВнСчет")
            + GetXAttrValue("acct", acct.acct + "," + acct.currency, "КартБВнСчет") NE "")
        THEN RUN pb_k2kbs-1.p(acct.acct, acct.currency, "НЕТ", "", cLog, cPrt, INPUT-OUTPUT lPrtFirst).
    END.
END.

{pb_k2prot.i}
{intrface.del}
