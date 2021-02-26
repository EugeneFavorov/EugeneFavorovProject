/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Перекодирует формат счетов WAY4 в теговый формат Бисквита
Параметры:      file=<Вх.файл>;arch-dir=<архив>;ans-dir=<вых.каталог>
Место запуска:  Бисмарк
Создан:         07.04.2017 Борисов А.В.
*/

DEFINE INPUT PARAMETER iParam       AS CHARACTER    NO-UNDO.

{globals.i}           /** Глобальные определения */
{parsin.def}
{intrface.get filex}
{pb_logit.i}

DEFINE VARIABLE cInFile     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cOutFile    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cArcDir     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cStr        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cAcct       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cClose      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lSend       AS LOGICAL      NO-UNDO INIT NO.
DEFINE VARIABLE lCorp       AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cCust       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCnt        AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE cSep        AS CHARACTER    NO-UNDO INIT "~t".
DEFINE TEMP-TABLE ttinf     NO-UNDO
    FIELD str       AS CHARACTER
    .
DEFINE STREAM imp.
DEFINE STREAM txt.

/* Архив ********************************************************************** */
cInFile = GetParamByNameAsChar(iParam, "file", "").
cArcDir = GetParamByNameAsChar(iParam, "arch-dir", "./")
        + STRING( YEAR(TODAY))       + "-"
        + STRING(MONTH(TODAY), "99") + "-"
        + STRING(  DAY(TODAY), "99") + "/".
IF NOT ExistFolder(cArcDir)
THEN DO:
    SurelyCreateFolder(cArcDir).
    OS-COMMAND SILENT VALUE("chmod 777 " + cArcDir).
END.

INPUT  STREAM imp FROM VALUE(cInFile).
IMPORT STREAM imp UNFORMATTED cStr.     /* В первой строке - заголовки столбцов */
REPEAT:
    IMPORT STREAM imp UNFORMATTED cStr.
    CREATE ttinf.
    ttinf.str = cStr.
    iCnt = iCnt + 1.
END.
INPUT  STREAM imp CLOSE.

IF (iCnt NE 0) THEN OS-COPY VALUE(cInFile)  VALUE(cArcDir).
cInFile  = SUBSTRING(cInFile, R-INDEX(cInFile, "/") + 1).

IF (iCnt NE 0)
THEN DO:
    cOutFile = cArcDir + SUBSTRING(cInFile, 1, LENGTH(cInFile) - 4) + ".txt".
    OUTPUT STREAM txt TO VALUE(cOutFile).

    FOR EACH ttinf
        NO-LOCK:

        cStr   = ttinf.str.
        cAcct  = ENTRY(3, cStr, cSep).
        cClose = IF (NUM-ENTRIES(cStr, cSep) >= 5) THEN ENTRY(5, cStr, cSep) ELSE "".
        cCust  = IF (cAcct BEGINS "40802") THEN SUBSTRING(ENTRY(2, cStr, cSep), 4) /* ИП считается ЮЛ и его номер передается с буквой "U" */
                                           ELSE SUBSTRING(ENTRY(2, cStr, cSep), 3).
        lCorp  = (cAcct BEGINS "407") AND (SUBSTRING(cAcct,10,4) = "0577").
        IF (cClose = "") THEN lSend = YES.

        IF      (cClose = "")
            AND (DATE(ENTRY(4, cStr, cSep)) <> TODAY)
        THEN DO:
            OUTPUT TO VALUE("date.txt").
            PUT UNFORMATTED CODEPAGE-CONVERT("Дата открытия счета " + cAcct + " = " + ENTRY(4, cStr, cSep)
                + " не совпадает с текущей: " + STRING(TODAY, "99.99.9999"), "1251") SKIP.
            OUTPUT CLOSE.
            RUN mail-add.p ("pb_w4acct").
            RUN pb_mail.p (RETURN-VALUE, "WAY4 - BIS: Acct open date!", "", "date.txt")).
        END.

        PUT STREAM txt UNFORMATTED
            (IF (cAcct BEGINS "9") THEN "%СЧЕТВБW4" ELSE "%СЧЕТW4") + "~n" +
            "ФИЛИАЛ   :0500"                                    + "~n" +
            "ВАЛЮТА   :" + SUBSTRING(cAcct, 6, 3)               + "~n" +
            "СИН/СЧЕТ :" + SUBSTRING(cAcct, 1, 5)               + "~n" +
            "СЧЕТ     :" + cAcct                                + "~n" +
            "КЛИЕНТ1  :" + cCust                                + "~n" +
            "ИМЯ1     :" + (IF (NUM-ENTRIES(cStr, cSep) GE 8)
                           THEN ENTRY(8, cStr, cSep) ELSE "")   + "~n" +
            "ОТКРЫТ   :" + ENTRY(4, cStr, cSep)                 + "~n" +
            (IF (cClose = "") THEN "" ELSE (
            "ЗАКРЫТ   :" + cClose                               + "~n" )) +
            "ОБЛАСТЬ  :" + (IF (cAcct BEGINS "9")
                           THEN "Внебаланс" ELSE "Баланс")      + "~n" +
            "ТИП      :"                                        + "~n" +
            (IF (cClose <> "") THEN "" ELSE (
            "groupOABS:" + (IF (SUBSTRING(cAcct,10,4) = "0577")
                           THEN "599" ELSE "598")               + "~n" +
            (IF (cAcct BEGINS "40") THEN (
            "ДогОткрЛС:" + ENTRY(4, cStr, cSep) + ","
                         + (IF lCorp THEN (SUBSTRING(cCust,2) + "/КК") ELSE
                           (IF (cCust BEGINS "U")
                            THEN GetXAttrValue("cust-cat", SUBSTRING(cCust,2), "CID")
                            ELSE GetXAttrValue("person", cCust, "CID"))) + "~n" +
            (IF lCorp THEN (
            "КорпКарт:Да"                                       + "~n" ) ELSE "")) ELSE "") +
            "СотрОткрСч:" + USERID("bisquit")                   + "~n" +
            (IF (NUM-ENTRIES(cStr, cSep) GE 6) AND (ENTRY(6, cStr, cSep) NE "") THEN (
            "W4basicacct:" + ENTRY(6, cStr, cSep)               + "~n" ) ELSE "") +
            (IF (NUM-ENTRIES(cStr, cSep) GE 7) AND (ENTRY(7, cStr, cSep) NE "") THEN (
            "W4tarif:" + ENTRY(7, cStr, cSep)                   + "~n" ) ELSE "")
            )) +
            "%END"                                              + "~n"
            .
    END.

    OUTPUT STREAM txt CLOSE.
    IF lSend THEN
/*  RUN pb_mail.p ("a.borisov,v.ignatchenko,t.stenina", "WAY4 - acct control", "", cOutFile)). */
    RUN pb_mail.p ("a.borisov", "WAY4 - acct control", "", cOutFile)).
    OS-COPY VALUE(cOutFile) VALUE(GetParamByNameAsChar(iParam, "ans-dir", ".")).
END.

RUN LogIt(cInFile + " - " + (IF (iCnt EQ 0) THEN "пустой" ELSE STRING(iCnt)), cArcDir + "Account.log").
{intrface.del}
