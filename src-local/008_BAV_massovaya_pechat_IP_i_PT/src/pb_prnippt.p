/**
Авторские права принадлежат: ПАО Плюс банк
Базируется:     
Основание:      СЗ ОСРКО.252
Что делает:     Массовая печать ПТ и ИП
Как работает:
Место запуска:
Создан:         12.05.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{getdates.i}

DEFINE VARIABLE cOldPP      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cFileName   AS CHARACTER    NO-UNDO.

cOldPP       = PackagePrint.
PackagePrint = YES.
FirstPrint   = NO.
/*
{setdest.i &filename = 'op.txt'}
*/
cFileName = "oppt.txt".
IF (SEARCH(cFileName) NE ?)
THEN OS-DELETE VALUE(cFileName).

FOR EACH op
    WHERE (op.doc-type      EQ "015")
      AND CAN-DO("В,ФБК", op.op-status)
      AND (op.op-date       GE beg-date)
      AND (op.op-date       LE end-date)
      AND (op.filial-id     EQ shFilial)
    NO-LOCK,
FIRST op-entry OF op
    WHERE (op-entry.acct-db NE ?)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        EQ op-entry.acct-db)
      AND (acct.cust-cat    NE "В")
    NO-LOCK:

    put screen col 1 row 24 "Печать ИП N " + op.doc-num + " от " + STRING(op.op-date, "99.99.9999") + "        ".
    OUTPUT TO VALUE(cFileName) APPEND PAGED PAGE-SIZE 100.
    PAGE.
    RUN in-uni_new_1.p(RECID(op)).
    OUTPUT CLOSE.
END.

FOR EACH op
    WHERE (op.doc-type      EQ "02")
      AND CAN-DO("В,ФБК", op.op-status)
      AND (op.op-date       GE beg-date)
      AND (op.op-date       LE end-date)
      AND (op.filial-id     EQ shFilial)
    NO-LOCK,
FIRST op-entry OF op
    WHERE (op-entry.acct-db NE ?)
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        EQ op-entry.acct-db)
      AND (acct.cust-cat    NE "В")
    NO-LOCK:

    put screen col 1 row 24 "Печать ПТ N " + op.doc-num + " от " + STRING(op.op-date, "99.99.9999") + "        ".
    OUTPUT TO VALUE(cFileName) APPEND PAGED PAGE-SIZE 100.
    PAGE.
    RUN pt-uni_ops.p(RECID(op)).
    OUTPUT CLOSE.
END.

PackagePrint = cOldPP.
put screen col 1 row 24 color normal STRING(" ","X(80)").

IF (SEARCH(cFileName) NE ?)
THEN DO:
    {preview.i &filename=cFileName}
END.
ELSE MESSAGE "Нет документов для печати."
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
