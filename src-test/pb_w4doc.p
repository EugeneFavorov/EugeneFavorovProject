/**
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Импорт документов процессинга WAY4
Как работает:   Перекодирует формат документов WAY4 в теговый формат Бисквита
Параметры:      file=<файл xml>;arch-dir=<архив>;ans-dir=<ответ>
Место запуска:  Бисмарк
Создан:         28.09.2016 Борисов А.В.
*/

DEFINE INPUT PARAMETER iParam       AS CHARACTER    NO-UNDO.

{globals.i}
{parsin.def}
{intrface.get filex}
/*
{pb_logit.i}
RUN LogIt(iParam, "/home/aborisov/pb_w4doc.log").
*/
/* ============================================================================ */
FUNCTION GetTextXML RETURNS CHARACTER
   (INPUT  iNode    AS HANDLE ).

    DEFINE VARIABLE i           AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cTxt        AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE hChild      AS HANDLE       NO-UNDO.

    CREATE X-NODEREF    hChild.
    REPEAT i = 1 TO iNode:NUM-CHILDREN:
        IF iNode:GET-CHILD(hChild, i)
        THEN DO:
            cTxt = hChild:NODE-VALUE.
/*          RUN LogIt("T : " 
                    + hChild:SUBTYPE  + " - " 
                    + hChild:NAME     + " - |" 
                    + hChild:NODE-VALUE + "|", "/home/aborisov/pb_w4doc.log").
*/      END.
    END.

    DELETE OBJECT hChild.
    RETURN cTxt.
END FUNCTION.

/* ============================================================================ */

DEFINE VARIABLE cInFile     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cOutFile    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cArcDir     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iTmp        AS INTEGER      NO-UNDO.
DEFINE VARIABLE hDoc        AS HANDLE       NO-UNDO.
DEFINE VARIABLE hRoot       AS HANDLE       NO-UNDO.
CREATE X-DOCUMENT   hDoc.
CREATE X-NODEREF    hRoot.
DEFINE STREAM txt.

DEFINE VARIABLE cPackage    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lDbCr       AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lBord       AS LOGICAL      NO-UNDO.    /* Банковский ордер - есть счет клиента */
/*
DEFINE VARIABLE lKass       AS LOGICAL      NO-UNDO.    /* Кассовая операция - есть 20202 */
DEFINE VARIABLE cKC         AS CHARACTER    NO-UNDO.
*/
DEFINE TEMP-TABLE ttDoc     NO-UNDO
    FIELD dat       AS CHARACTER
    FIELD num       AS CHARACTER
    FIELD dscr      AS CHARACTER
    FIELD db        AS CHARACTER
    FIELD cr        AS CHARACTER
    FIELD dcur      AS CHARACTER
    FIELD ccur      AS CHARACTER
    FIELD dsum      AS CHARACTER
    FIELD csum      AS CHARACTER
    FIELD gref      AS CHARACTER
    FIELD drn       AS CHARACTER
    .
DEFINE VARIABLE mHandle     AS INT64        NO-UNDO.
DEFINE VARIABLE iStat       AS INT64        NO-UNDO.
DEFINE VARIABLE cKartCur    AS CHARACTER    NO-UNDO.    /* Валюта карточного счета */
DEFINE VARIABLE cBuyCur     AS CHARACTER    NO-UNDO.    /* Валюта покупки */
DEFINE VARIABLE cKartAcct   AS CHARACTER    NO-UNDO.    /* Карточный счет */
DEFINE VARIABLE cBuyAcct    AS CHARACTER    NO-UNDO.    /* Счет 302 */
DEFINE VARIABLE c706Acct    AS CHARACTER    NO-UNDO.    /* Счет 706 */
DEFINE VARIABLE lKartDb     AS LOGICAL      NO-UNDO.    /* Карточный счет в Дб */
DEFINE VARIABLE cKartSum    AS CHARACTER    NO-UNDO.    /* Сумма списания с карты */
DEFINE VARIABLE cBuySum     AS CHARACTER    NO-UNDO.    /* Сумма покупки в вал */
DEFINE VARIABLE c706Sum     AS CHARACTER    NO-UNDO.    /* Сумма комиссии */
DEFINE VARIABLE cDescr      AS CHARACTER    NO-UNDO.    /* Назначение платежа */
DEFINE VARIABLE lConv       AS LOGICAL      NO-UNDO INIT NO. /* Выгружать конвертацию */
DEFINE VARIABLE cOvrd       AS CHARACTER    NO-UNDO.    /* Счета технического овердрафта */
cOvrd    = FGetSetting("WAY4", "acc47423", "").

cInFile  = GetParamByNameAsChar(iParam, "file", "").
cOutFile = SUBSTRING(cInFile, R-INDEX(cInFile, "/") + 1).

/* Контроль дубликатов ******************************************************** */
iTmp = R-INDEX(cOutFile, "_").
cTmp = SUBSTRING(cOutFile, iTmp - 8, INDEX(cOutFile, ".", iTmp) - iTmp + 8).
FIND FIRST code
    WHERE (code.class   EQ 'W4docin')
      AND (code.parent  EQ 'W4docin')
      AND (code.code    EQ cTmp)
    NO-LOCK NO-ERROR.
IF (AVAIL code)
THEN DO:        /* Дубликат */
    RUN mail-add.p ("i-mg-acc-w4").
    RUN pb_mail.p (RETURN-VALUE, "Dublikat importa dokumentov iz WAY4", "", cInFile)).
    RETURN.
END.
ELSE DO:
    CREATE code.
    ASSIGN
        code.class   = 'W4docin'
        code.parent  = 'W4docin'
        code.code    = cTmp
        code.name    = cOutFile
        code.val     = STRING(NOW, "99.99.9999 HH:MM:SS")
        .
END.

/* Архив ********************************************************************** */
cArcDir = GetParamByNameAsChar(iParam, "arch-dir", "./")
        + STRING( YEAR(TODAY))       + "-"
        + STRING(MONTH(TODAY), "99") + "-"
        + STRING(  DAY(TODAY), "99") + "/".
IF NOT ExistFolder(cArcDir)
THEN DO:
    SurelyCreateFolder(cArcDir).
    OS-COMMAND SILENT VALUE("chmod 777 " + cArcDir).
END.
OS-COPY VALUE(cInFile) VALUE(cArcDir).

/* Импорт документов ********************************************************** */
hDoc:LOAD("FILE", cInFile, FALSE).
hDoc:GET-DOCUMENT-ELEMENT(hRoot).
RUN ParsXML(hRoot, 1).

/* Формирование выходного файла *********************************************** */
cOutFile = SUBSTRING(cOutFile, 1, LENGTH(cOutFile) - 4).
cOutFile = cArcDir + cOutFile + ".txt".
OUTPUT STREAM txt TO VALUE(cOutFile). /* CONVERT TARGET "IBM866" SOURCE "UTF-8". */

FOR EACH ttDoc
    NO-LOCK
    BREAK BY ENTRY(2, ttDoc.gref, "_"):

    IF     (ttDoc.db BEGINS "47407") OR (ttDoc.db BEGINS "47408")
        OR (ttDoc.cr BEGINS "47407") OR (ttDoc.cr BEGINS "47408")
    THEN DO:
        lConv = YES.
        /* Ищем параметры конверсионной операции */
        IF      (ttDoc.db BEGINS "40")
            OR  (ttDoc.cr BEGINS "40")
        THEN DO:
            cKartCur  = ttDoc.dcur.
            lKartDb   = (ttDoc.db BEGINS "40").
            cKartAcct = IF lKartDb THEN ttDoc.db ELSE ttDoc.cr.
            cKartSum  = ttDoc.dsum.
            cDescr    = ttDoc.dscr.
        END.
        ELSE IF (ttDoc.cr BEGINS "302")
            OR  (ttDoc.db BEGINS "302")
        THEN DO:
            cBuyCur   = ttDoc.dcur.
            cBuyAcct  = IF (ttDoc.db BEGINS "302") THEN ttDoc.db ELSE ttDoc.cr.
            cBuySum   = ttDoc.dsum.
        END.
        ELSE DO:
            c706Acct  = IF (ttDoc.db BEGINS "706") THEN ttDoc.db ELSE ttDoc.cr.
            c706Sum   = ttDoc.dsum.
        END.
    END.
    ELSE DO:
        /* Обычная неконверсионная проводка */
        /* Замена в комиссионных проводках */
        IF (ttDoc.db BEGINS "40") AND (ttDoc.cr BEGINS "70601")
        THEN DO:
            cTmp = SUBSTRING(ttDoc.db, 6, 3).
            cTmp = IF (cTmp = "810") THEN "" ELSE cTmp.
            cTmp = GetXAttrValue("acct", ttDoc.db + "     @0500," + cTmp, "acct-47423").

            IF (cTmp <> "")
            THEN DO:
                RUN STORED-PROCEDURE GET_OVERDRAFT_ID mHandle = PROC-HANDLE
                    (INPUT  PARAM P_ACC            = ttDoc.db,
                     INPUT  PARAM P_TYPE_ACC       = 1,
                     INPUT  PARAM P_DATE           = TODAY,
                     OUTPUT PARAM GET_OVERDRAFT_ID = ?).
                CLOSE STORED-PROC GET_OVERDRAFT_ID iStat = PROC-STATUS.

                IF (iStat = 0) AND (GET_OVERDRAFT_ID > 0)
                THEN ttDoc.db = cTmp.
            END.
        END.

        ttDoc.dcur = IF (ttDoc.dcur EQ "810") THEN "" ELSE ttDoc.dcur.
        ttDoc.ccur = IF (ttDoc.ccur EQ "810") THEN "" ELSE ttDoc.ccur.
        lBord = (ttDoc.db BEGINS "40")  OR (ttDoc.cr BEGINS "40").
    /*  lKass = (ttDoc.db BEGINS "202") OR (ttDoc.cr BEGINS "202").
        cKC   = IF (lKass AND (ttDoc.dcur EQ "") AND (ttDoc.ccur EQ "")) THEN
                (IF (ttDoc.db BEGINS "20202") THEN "31" ELSE "51") ELSE "".
    */
        cTmp = "".
        IF (ttDoc.db BEGINS "40")
        THEN DO:
            FOR FIRST acct
                WHERE (acct.acct     = ttDoc.db + "     @0500")
                  AND (acct.currency = ttDoc.dcur)
                NO-LOCK:

                cTmp = acct.cust-cat + "," + STRING(acct.cust-id).
            END.
        END.

        PUT STREAM txt UNFORMATTED
             (IF (ttDoc.db BEGINS "9")   THEN "%ВБОРДЕР" ELSE
             ((IF lBord THEN "%БАНКОРД" ELSE "%МЕМОРД")
            + (IF CAN-DO(cOvrd, ttDoc.db) OR CAN-DO(cOvrd, ttDoc.cr) THEN "В" ELSE "ЕР"))) + "~n" /* по счетам технического овердрафта - в статусе В */
            + "ОБЛАСТЬ   :" + (IF (ttDoc.db BEGINS "9")
                          THEN "Внебаланс" ELSE "Баланс")   + "~n"
            + "ФИЛИАЛ    :0500"                             + "~n"
/*          + "ДАТА      :" + SUBSTRING(ttDoc.dat,9,2) + "/" + SUBSTRING(ttDoc.dat,6,2) + "/" + SUBSTRING(ttDoc.dat,1,4) + "~n"
*/          + "ДАТА      :" + STRING(TODAY,"99/99/9999")    + "~n"
            + "ПАЧКА     :" + cPackage                      + "~n"
    /*      + "ОПЕРАЦИЯ  :" + (IF lBord THEN "017" ELSE "09")   + "~n"
    */      + "ВАЛЮТА    :" + ttDoc.dcur                    + "~n"
            + "НОМЕР     :" + ttDoc.num                     + "~n"
            + "ДЕБЕТ     :" + ttDoc.db                      + "~n"
            + "КРЕДИТ    :" + ttDoc.cr                      + "~n"
            + "СУММА     :" + ttDoc.dsum                    + "~n"
    /*      + (IF lKass       THEN ("СУММАКАСС :" + ttDoc.dsum  + "~n") ELSE "")
            + (IF (cKC NE "") THEN ("СИМКАСС   :" + cKC         + "~n") ELSE "")
            + "ДАТАПЛАТ  :" + SUBSTRING(ttDoc.dat,9,2) + "/" + SUBSTRING(ttDoc.dat,6,2) + "/" + SUBSTRING(ttDoc.dat,1,4) + "~n"
    */      + "ДАТАПЛАТ  :" + STRING(TODAY,"99/99/9999")    + "~n"
            + "ОТВЕТОИСП :BIS"                              + "~n"
            + "ПРИМ1     :" + (IF (ttDoc.db BEGINS "40807810") THEN
                                (IF (ttDoc.cr BEGINS "70601")  THEN "~{VO80050} " ELSE
                                (IF (ttDoc.cr BEGINS "20208")  THEN "~{VO60090} " ELSE
                                (IF (ttDoc.cr BEGINS "30232")  THEN "~{VO60200} " ELSE "")))
                               ELSE "")
                            + ttDoc.dscr                    + "~n"
            + "W4_import :" + STRING(NOW, "99.99.9999 HH:MM:SS")             + "~n"
            + "W4_ref    :" + ttDoc.gref + "," + ttDoc.num + "," + ttDoc.drn + "~n"
            + (IF (cTmp = "") THEN "" ELSE (
              "Плательщик:" + cTmp                          + "~n"
            + "Получатель:" + cTmp                          + "~n"))
            + "%END"                                        + "~n"
            .
    END.

    IF LAST-OF(ENTRY(2, ttDoc.gref, "_")) AND lConv
    THEN DO:
        cTmp = "".
        IF lKartDb
        THEN DO:
            FOR FIRST acct
                WHERE (acct.acct     = cKartAcct + "     @0500")
                  AND (acct.currency = cKartCur)
                NO-LOCK:

                cTmp = acct.cust-cat + "," + STRING(acct.cust-id).
            END.
        END.

        PUT STREAM txt UNFORMATTED
              "%КОНВЕРТАЦ"                                  + "~n"
            + "ОБЛАСТЬ   :Баланс"                           + "~n"
            + "ФИЛИАЛ    :0500"                             + "~n"
/*          + "ДАТА      :" + SUBSTRING(ttDoc.dat,9,2) + "/" + SUBSTRING(ttDoc.dat,6,2) + "/" + SUBSTRING(ttDoc.dat,1,4) + "~n"
*/          + "ДАТА      :" + STRING(TODAY,"99/99/9999")    + "~n"
            + "ПАЧКА     :" + cPackage                      + "~n"
            + "ВАЛЮТА1   :" + (IF lKartDb THEN cKartCur  ELSE cBuyCur)   + "~n"
            + "ВАЛЮТА2   :" + (IF lKartDb THEN cBuyCur   ELSE cKartCur)  + "~n"
            + "СУММА1    :" + (IF lKartDb THEN cKartSum  ELSE cBuySum)   + "~n"
            + "СУММА2    :" + (IF lKartDb THEN cBuySum   ELSE cKartSum)  + "~n"
            + "НОМЕРДОК1 :" + ttDoc.num                     + "~n"
            + "СЧЕТИСТОЧ :" + (IF lKartDb THEN cKartAcct ELSE cBuyAcct)  + "~n"
            + "СЧЕТПРИЕМ :" + (IF lKartDb THEN cBuyAcct  ELSE cKartAcct) + "~n"
            + "СЧЕТКОМП  :" + c706Acct                      + "~n"
            + "ПРИМ1     :Конвертация по платежной системе Visa по курсу "
                            + TRIM(STRING(DEC(cKartSum) / DEC(cBuySum),">>>9.9999")) + " "
                            + ENTRY(LOOKUP(cKartCur,"810,840,978"), "руб,USD,EUR")   + "/"
                            + ENTRY(LOOKUP(cBuyCur, "810,840,978"), "руб,USD,EUR")   + ". "
                            + cDescr                                                 + "~n"
            + "W4_import :" + STRING(NOW, "99.99.9999 HH:MM:SS")                     + "~n"
            + "W4_ref    :" + ttDoc.gref + "," + ttDoc.num  + "," + ttDoc.drn        + "~n"
            + "ОТВЕТОИСП :BIS"                              + "~n"
            + (IF (cTmp = "") THEN "" ELSE (
              "Плательщик:" + cTmp                          + "~n"
            + "Получатель:" + cTmp                          + "~n"))
            + "%END"                                        + "~n"
            .
        lConv = NO.
    END.
END.
OUTPUT STREAM txt CLOSE.
DELETE OBJECT hDoc.
DELETE OBJECT hRoot.

OS-COPY VALUE(cOutFile) VALUE(GetParamByNameAsChar(iParam, "ans-dir", ".")).
{intrface.del}

/* ============================================================================ */
PROCEDURE ParsXML:
    DEFINE INPUT  PARAMETER iNode   AS HANDLE   NO-UNDO.
    DEFINE INPUT  PARAMETER iLevel  AS INTEGER  NO-UNDO.

    DEFINE VARIABLE i           AS INTEGER  NO-UNDO.
    DEFINE VARIABLE lChld       AS LOGICAL  NO-UNDO INIT NO.
    DEFINE VARIABLE hChild      AS HANDLE   NO-UNDO.

    IF (iNode:SUBTYPE EQ "TEXT") THEN RETURN.
/*
    RUN LogIt(STRING(iLevel) + " : " 
            + iNode:SUBTYPE  + " - " 
            + iNode:NAME     + " - |" 
            + iNode:NODE-VALUE + "|", "/home/aborisov/pb_w4doc.log").
*/
    CASE iNode:NAME:
        WHEN "Analytic" THEN DO:
            CREATE ttDoc.
            lChld = YES.
        END.
        WHEN "Credit" OR WHEN "Debit" THEN DO:
            lDbCr = (iNode:NAME EQ "Debit").
            lChld = YES.
        END.
        WHEN "DocInfo"                      OR
        WHEN "DocRefSet"                    OR
        WHEN "Parm"                         OR
        WHEN "AnalyticAccount"              OR
        WHEN "AccountingTransactionFile"    OR
        WHEN "FileHeader"                   OR
        WHEN "AccountingTransactionList"    THEN lChld = YES.
        WHEN "FileSeqNumber"                THEN cPackage   = GetTextXML(iNode).
        WHEN "PostingDate"                  THEN ttDoc.dat  = GetTextXML(iNode).
        WHEN "AnalyticRefN"                 THEN ttDoc.num  = GetTextXML(iNode).
        WHEN "TransferDescription"          THEN ttDoc.dscr = GetTextXML(iNode).
        WHEN "GroupRefN"                    THEN ttDoc.gref = GetTextXML(iNode).
        WHEN "ParmCode"                     THEN IF (GetTextXML(iNode) = "DRN") THEN ttDoc.drn = "DRN".
        WHEN "Value"                        THEN IF (ttDoc.drn         = "DRN") THEN ttDoc.drn = GetTextXML(iNode).
        WHEN "AccountNumber" THEN DO:
            IF lDbCr 
            THEN ttDoc.db   = GetTextXML(iNode).
            ELSE ttDoc.cr   = GetTextXML(iNode).
        END.
        WHEN "Currency"      THEN DO:
            IF lDbCr 
            THEN ttDoc.dcur = GetTextXML(iNode).
            ELSE ttDoc.ccur = GetTextXML(iNode).
        END.
        WHEN "Amount"        THEN DO:
            IF lDbCr 
            THEN ttDoc.dsum = GetTextXML(iNode).
            ELSE ttDoc.csum = GetTextXML(iNode).
        END.
    END CASE.

    IF lChld
    THEN DO:
        CREATE X-NODEREF    hChild.

        REPEAT i = 1 TO iNode:NUM-CHILDREN:
            IF iNode:GET-CHILD(hChild, i)
            THEN RUN ParsXML(hChild, iLevel + 1).
        END.
        DELETE OBJECT hChild.
    END.
END PROCEDURE.
