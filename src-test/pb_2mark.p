/**
Что делает:     Переводит выделенные проводки в статус "крыж"
Место запуска:  Список документов - Ctrl-G
Создан:         14.02.2018 Борисов А.В.
*/

{tmprecid.def}          /** Используем информацию из броузера */

FOR EACH tmprecid 
    NO-LOCK,
FIRST op
    WHERE (RECID(op) EQ tmprecid.id)
    EXCLUSIVE-LOCK:

    MESSAGE "Документ - " op.doc-num op.op-status
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
    op.op-status = "√".
END.
