/**
Авторские права принадлежат: ОАО "Плюс Банк"
Базируется:     print-reestr.p
Основание:      СЗ УСКО.036
Что делает:     Печать реестра (для отправки справок о задолженности) по отмеченным договорам
Параметры:      templ=<шаблон XL>;dogtype=<тип договора>;payments=ALL/доЧДГ  (templ=graphm0914;dogtype=4;payments=ALL) 
Место запуска:  Кредиты - Ctrl-G - ПЕЧАТЬ ДОГОВОРОВ В ФОРМАТЕ EXCEL
Создан:         04.05.2018 Борисов А.В.
*/

&GLOB nodate YES

{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get netw}     /** Отправка в bispc */

DEFINE VARIABLE cFl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTxt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRee_s  AS CHARACTER NO-UNDO.
DEFINE VARIABLE I       AS INTEGER   NO-UNDO.
DEFINE VARIABLE Nree    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cN1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cN2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cINN    AS CHARACTER NO-UNDO.
DEFINE VARIABLE Rezult    AS CHARACTER NO-UNDO.
DEFINE STREAM sreestr.

/* Читаем шаблон */
FUNCTION TmplImport RETURNS CHARACTER
   (INPUT  iTmplNam AS CHARACTER ).

    DEFINE VARIABLE cTmpl       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    INPUT FROM VALUE(SEARCH(iTmplNam)).
    cTmpl = "".
    DO WHILE TRUE
        ON ENDKEY UNDO, LEAVE:

        IMPORT UNFORMATTED cTmp.
        cTmpl = cTmpl + cTmp + "~r~n".
    END.
    INPUT  CLOSE.

    RETURN cTmpl.
END FUNCTION.

/* Вывод заголовка реестра с заменой */
PROCEDURE OutReeBeg:
    DEFINE INPUT  PARAMETER iTmpl   AS CHARACTER    NO-UNDO.    /* Шаблон */
    DEFINE INPUT  PARAMETER iVal    AS CHARACTER    NO-UNDO.    /* Номер реестра */

    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    INPUT FROM VALUE(SEARCH(iTmpl)).
    DO WHILE TRUE
        ON ENDKEY UNDO, LEAVE:

        IMPORT UNFORMATTED cTmp.
        cTmp = REPLACE(cTmp, "numtabl", iVal).
        PUT STREAM sreestr UNFORMATTED (cTmp + "~r~n").
    END.
    INPUT  CLOSE.
END PROCEDURE.

/* Читаем шаблон */
cRee_s = TmplImport("pb_respr_str.rtf").

cFl  = "./reespr-"
     + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")
     + "-" + shFilial + ".rtf".
OUTPUT STREAM sreestr TO VALUE(cFl).

/* По отмеченным договорам */
Nree = 0.
FOR EACH tmprecid
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan)  = tmprecid.id)
    NO-LOCK
    BY loan.doc-ref:

    Nree = Nree + 1.
END.
RUN OutReeBeg("pb_respr_beg.rtf", STRING(Nree)).

I = 0.
FOR EACH tmprecid
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan)      = tmprecid.id)
    NO-LOCK
    BY loan.doc-ref:

    /* Строка реестра */
    I = I + 1.
    cTxt = REPLACE(cRee_s, "numstr",       STRING(I)).
    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, ?, OUTPUT cN1, OUTPUT cN2, INPUT-OUTPUT cINN).
    cTxt = REPLACE(cTxt,   "klientfio",    CODEPAGE-CONVERT(TRIM(cN1 + " " + cN2), "1251")).
    RUN pb_newadr.p (loan.cust-cat, loan.cust-id,OUTPUT Rezult).
    cTxt = REPLACE(cTxt,   "klientadress", " " + CODEPAGE-CONVERT(RETURN-VALUE, "1251")).
    PUT STREAM sreestr UNFORMATTED cTxt.
END.

RUN OutReeBeg("pb_respr_end.rtf", STRING(Nree)).
OUTPUT STREAM sreestr CLOSE.

/* Перед отправкой отчета проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
END.
/* Отправляем протокол */
RUN sndbispc.p ("file=" + cFl + ";class=bq").
{intrface.del}
