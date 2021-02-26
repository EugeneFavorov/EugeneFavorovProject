/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      СЗ А.1588
Что делает:     Выводит список сотрудников
Как работает:   Отбирает ФЛ, у которых есть карточный счет 550-й группы
Параметры:      
Место запуска:  
Создан:         21.03.2017 Борисов А.В.
*/

{globals.i}

DEFINE TEMP-TABLE ttFl      NO-UNDO
    FIELD persid    AS INTEGER
    FIELD pname     AS CHARACTER
    .

FOR EACH signs
    WHERE (signs.file-name  EQ 'acct')
      AND (signs.code       EQ 'groupOABS')
      AND (signs.code-value EQ '599')
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        EQ ENTRY(1, signs.surrogate))
      AND (acct.currency    EQ ENTRY(2, signs.surrogate))
      AND (acct.acct    BEGINS "40817")
      AND (acct.cust-cat    EQ "Ч")
    NO-LOCK,
FIRST person
    WHERE (person.person-id EQ acct.cust-id)
    NO-LOCK
/*  BREAK BY person.person-id */ :

/*  IF FIRST-OF(person.person-id)
    THEN DO:
*/      CREATE ttFl.
        ASSIGN
            ttFl.persid = person.person-id
            ttFl.pname  = person.name-last + " " + person.first-names
            .
/*  END. */
END.

/* Визуализация списка сотрудников ============================================ */
DEFINE VARIABLE num-line AS INT64 INITIAL 0 NO-UNDO. /* Требует navigate.cqr */
FORM
    ttFl.pname      FORMAT 'x(50)'      COLUMN-LABEL 'Фамилия Имя Отчество' HELP "Фамилия Имя Отчество"
    ttFl.persid     FORMAT '>>>>>>9'    COLUMN-LABEL 'КОД'                  HELP "Порядковый номер"
WITH FRAME browse1
TITLE COLOR BRIGHT-WHITE 'НОВЫЙ КЛИЕНТСКИЙ МЕНЕДЖЕР'
SCROLLABLE.

{qrdef.i
    &buff-list = "ttFl"
    &Sortby    = "'by ttFl.pname'"
}

{navigate.cqr
    &file       = "ttFl"
    &workfile   = "/*"
    &avfile     = "ttFl"

    &maxfrm     = 1
    &bf1        = "ttFl.pname ttFl.persid"
    &help-label = "'F1 Просмотр|ESC Выход'"

    &nodel      = yes
    &look       = "pb_sotr.f1 "
    &return     = "return2.cqr "
        &rfld   = persid
}
{intrface.del}
