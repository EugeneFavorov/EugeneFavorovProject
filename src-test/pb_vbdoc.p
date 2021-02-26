/*
Авторские права принадлежат: ПАО Плюс банк
Что делает:     Перекодирует формат документов WAY4 в теговый формат Бисквита
Как работает:   
Параметры:      
Место запуска:  Бисмарк
Создан:         28.09.2016 Борисов А.В.
*/

DEFINE INPUT PARAMETER iParam       AS CHARACTER    NO-UNDO.
{globals.i}
{parsin.def}
{intrface.get filex}
{topkind.def}


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
        end.
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
DEFINE TEMP-TABLE ttRECORDS     NO-UNDO
    FIELD fQuantity       AS CHARACTER
    FIELD fPrice          AS CHARACTER
    FIELD fBuySell        AS CHARACTER
    FIELD fValue          AS CHARACTER
    FIELD fTradeNo        AS CHARACTER
    FIELD fTradeGroup     AS CHARACTER
    FIELD fCoCurrencyId   AS CHARACTER
    FIELD fCurrencyId     AS CHARACTER
    FIELD fReportDate     AS CHARACTER
    FIELD fDOC_DATE       AS CHARACTER
    FIELD fDOC_TIME       AS CHARACTER
    FIELD fDOC_NO         AS CHARACTER
    FIELD fSENDER_ID      AS CHARACTER
    FIELD fRECEIVER_ID    AS CHARACTER
    .


DEFINE TEMP-TABLE ttCURRPAIR     NO-UNDO
    FIELD fCurrencyId    AS CHARACTER
    FIELD fCoCurrencyId  AS CHARACTER
    .

DEFINE TEMP-TABLE ttCCX03     NO-UNDO
    FIELD fReportDate  AS CHARACTER
    .

DEFINE TEMP-TABLE ttDOC_REQUISITES     NO-UNDO
    FIELD fDOC_DATE     AS CHARACTER
    FIELD fDOC_TIME     AS CHARACTER
    FIELD fDOC_NO       AS CHARACTER
    FIELD fSENDER_ID    AS CHARACTER
    FIELD fRECEIVER_ID  AS CHARACTER
    .

DEFINE VARIABLE cTranz      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStr        AS CHARACTER NO-UNDO.
DEFINE VARIABLE user_       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBranch     AS CHARACTER NO-UNDO.


cInFile  = GetParamByNameAsChar(iParam, "file", "").
cOutFile = SUBSTRING(cInFile, R-INDEX(cInFile, "/") + 1).


/* Контроль дубликатов ******************************************************** */
iTmp = R-INDEX(cOutFile, "_").
cTmp = SUBSTRING(cOutFile, iTmp - 8, INDEX(cOutFile, ".", iTmp) - iTmp + 8).

/*
FIND FIRST code
    WHERE (code.class   EQ 'W4docin')
      AND (code.parent  EQ 'W4docin')
      AND (code.code    EQ cTmp)
    NO-LOCK NO-ERROR.
IF (AVAIL code)
THEN DO:        /* Дубликат */
/*
    RUN mail-add.p ("i-mg-acc-w4").
    RUN pb_mail.p (RETURN-VALUE, "Дубликат импорта документов WAY4", "", cInFile)).
*/
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
*/
/* Архив ********************************************************************** */
cArcDir = GetParamByNameAsChar(iParam, "arch-dir", "./")
        + STRING( YEAR(TODAY))       + "-"
        + STRING(MONTH(TODAY), "99") + "-"
        + STRING(  DAY(TODAY), "99") + "/".

SurelyCreateFolder(cArcDir).
OS-COPY VALUE(cInFile) VALUE(cArcDir).
/* Импорт документов ********************************************************** */
hDoc:LOAD("FILE", cInFile, false).
hDoc:GET-DOCUMENT-ELEMENT(hRoot).
RUN ParsXML(hRoot, 1).
/* Формирование выходного файла *********************************************** */
cOutFile = SUBSTRING(cOutFile, 1, LENGTH(cOutFile) - 4).
cOutFile = cArcDir + cOutFile + ".txt".
OUTPUT STREAM txt TO VALUE(cOutFile). /* CONVERT TARGET "IBM866" SOURCE "UTF-8". */

FOR EACH ttRECORDS
    NO-LOCK:
    user_ =  USERID("bisquit").   
    iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
    output to "/home/vignatchenko/out/qqqq.txt" append.
    export ttRECORDS.
    output close.
    {empty tOpKindParams}     /* очистить таблицу параметров */

    if ttRECORDS.fCoCurrencyId = "RUB" then do:
       ttRECORDS.fCoCurrencyId = "".
    end.
    else do:
       find first currency where currency.i-currency = ttRECORDS.fCoCurrencyId no-lock no-error.
       if avail currency then do:
          ttRECORDS.fCoCurrencyId = currency.currency.
       end.
    end.
    if ttRECORDS.fCurrencyId = "RUB" then do:
       ttRECORDS.fCurrencyId = "".
    end.
    else do:
       find first currency where currency.i-currency = ttRECORDS.fCurrencyId no-lock no-error.
       if avail currency then do:
          ttRECORDS.fCurrencyId = currency.currency.
       end.
    end.

    ASSIGN
        lOk =   TDAddParam("Price"       , ttRECORDS.fPrice) 
            and TDAddParam("BuySell"     , ttRECORDS.fBuySell) 
            and TDAddParam("ReportDate"  , ttRECORDS.fReportDate) 
            and TDAddParam("filial-id"   , shFilial) 
            and TDAddParam("branch-id"   , iBranch) 
            and TDAddParam("DOC_DATE"   , ttRECORDS.fDOC_DATE   ) 
            and TDAddParam("DOC_TIME"   , ttRECORDS.fDOC_TIME   ) 
            and TDAddParam("DOC_NO"     , ttRECORDS.fDOC_NO     ) 
            and TDAddParam("SENDER_ID"  , ttRECORDS.fSENDER_ID  ) 
            and TDAddParam("RECEIVER_ID", ttRECORDS.fRECEIVER_ID) 
            and TDAddParam("TradeNo"    , ttRECORDS.fTradeNo) 
            and TDAddParam("TradeGroup"    , ttRECORDS.fTradeGroup) 
            and TDAddParam("currency1"   , ttRECORDS.fCurrencyId) 
            and TDAddParam("currency2"   , ttRECORDS.fCoCurrencyId) 
            and TDAddParam("Quantity"    , ttRECORDS.fQuantity) 
            and TDAddParam("Value"       , ttRECORDS.fValue) 
    
    NO-ERROR.
    IF NOT lOk
    THEN RUN LogIt("возникла ошибка при передаче параметров в транзакцию " + cTranz).
    ELSE DO:
    END.

    /* Проверка, что это не межвалютная операция */
    if ttRECORDS.fCoCurrencyId = "" or ttRECORDS.fCurrencyId = "" then do:
       /*Проверка, что это продажа валюты */
       if ttRECORDS.fBuySell = "B" then do:
          /* Проверка что валюта покупки - рубли */
          if ttRECORDS.fCoCurrencyId = "" then do:
             /* Проверка что сделка Today */
             if ttRECORDS.fTradeGroup = "T" then do:
                cTranz = "05!00001".
                RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                IF NOT lOk
                THEN DO:
                END.
                ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
             end.
             /*Сделка SWAP*/
             else do:
                cTranz = "05!00001".
                RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                IF NOT lOk
                THEN DO:
                END.
                ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
             end.
          end.
          else do:
          end.
       end.
       /* Покупка валюты */
       else do:
          /* Проверка что валюта покупки рубли */
          if ttRECORDS.fCoCurrencyId = "" then do:
             /* Проверка что сделка Today */
             if ttRECORDS.fTradeGroup = "T" then do:
                cTranz = "05!00002".
                RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                IF NOT lOk
                THEN DO:
                END.
                ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
             end.
             /*Сделка SWAP*/
             else do:
                cTranz = "05!00002".
                RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
                IF NOT lOk
                THEN DO:
                END.
                ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
             end.
          end.
          else do:
          end.
       end.
    end.
    /*Межвалютная операция*/
    else do:
       /* Покупка валюты */
       if ttRECORDS.fBuySell = "B" then do:
          /* Проверка что сделка Today */
          if ttRECORDS.fTradeGroup = "T" then do:
             cTranz = "05!00003".
             RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
             IF NOT lOk
             THEN DO:
             END.
             ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
          end.
          /*Сделка SWAP*/
          else do:
             cTranz = "05!00003".
             RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
             IF NOT lOk
             THEN DO:
             END.
             ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
          end.
       end.
       /* Продажа валюты */
       else do:
          /* Проверка что сделка Today */
          if ttRECORDS.fTradeGroup = "T" then do:
             cTranz = "05!00004".
             RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
             IF NOT lOk
             THEN DO:
             END.
             ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
          end.
          /*Сделка SWAP*/
          else do:
             cTranz = "05!00004".
             RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
             IF NOT lOk
             THEN DO:
             END.
             ELSE /* RUN LogIt("Выполнена транзакция " + cTranz) */ .
          end.
       end.
    end.
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
    DEFINE VARIABLE j           AS INTEGER  NO-UNDO.
    DEFINE VARIABLE lChld       AS LOGICAL  NO-UNDO INIT NO.
    DEFINE VARIABLE hChild      AS HANDLE   NO-UNDO.
    DEFINE VARIABLE entries     AS Char     NO-UNDO.
    DEFINE VARIABLE aname       AS Char     NO-UNDO.
    DEFINE VARIABLE adetails    AS Char     NO-UNDO.
    DEFINE VARIABLE bdetails    AS Char     NO-UNDO.
    IF (iNode:SUBTYPE EQ "TEXT") THEN RETURN.
    CASE iNode:NAME:
        WHEN "MICEX_DOC"             OR
        WHEN "SETTLE"    THEN lChld = YES.
        WHEN "RECORDS"   THEN do :
           find last ttCCX03 no-lock no-error.
           if not avail ttCCX03 then do:
              message "Error CCX03" view-as alert-box.
           end.
           find last ttCURRPAIR no-lock no-error.
           if not avail ttCURRPAIR then do:
              message "Error CURRPAIR" view-as alert-box.
           end.
           find last ttDOC_REQUISITES no-lock no-error.
           if not avail ttDOC_REQUISITES then do:
              message "Error DOC_REQUISITES" view-as alert-box.
           end.
           CREATE ttRECORDS.
           entries = iNode:ATTRIBUTE-NAMES.
           adetails = "".
           ttRECORDS.fCoCurrencyId   = ttCURRPAIR.fCoCurrencyId.
           ttRECORDS.fCurrencyId     = ttCURRPAIR.fCurrencyId.
           ttRECORDS.fReportDate     = ttCCX03.fReportDate.
           ttRECORDS.fDOC_DATE       = ttDOC_REQUISITES.fDOC_DATE.
           ttRECORDS.fDOC_TIME       = ttDOC_REQUISITES.fDOC_TIME.
           ttRECORDS.fDOC_NO         = ttDOC_REQUISITES.fDOC_NO.
           ttRECORDS.fSENDER_ID      = ttDOC_REQUISITES.fSENDER_ID.
           ttRECORDS.fRECEIVER_ID    = ttDOC_REQUISITES.fRECEIVER_ID.
           REPEAT j = 1 TO NUM-ENTRIES(entries):
              aname = ENTRY(j, entries).
              adetails = iNode:GET-ATTRIBUTE(aname). 
              CASE aname:
                 WHEN "Quantity"      THEN  ttRECORDS.fQuantity   = adetails.
                 WHEN "Price"         THEN  ttRECORDS.fPrice      = adetails.
                 WHEN "BuySell"       THEN  ttRECORDS.fBuySell    = adetails.
                 WHEN "Value"         THEN  ttRECORDS.fValue      = adetails.
                 WHEN "TradeNo"       THEN  ttRECORDS.fTradeNo    = adetails.
                 WHEN "TradeGroup"    THEN  ttRECORDS.fTradeGroup = adetails.
              END CASE.
           END.
        end. 
        WHEN "CURRPAIR"  THEN do :
           lChld = YES.
           CREATE ttCURRPAIR.
           entries = iNode:ATTRIBUTE-NAMES.
           adetails = "".
           REPEAT j = 1 TO NUM-ENTRIES(entries):
              aname = ENTRY(j, entries).
              adetails = iNode:GET-ATTRIBUTE(aname). 
              CASE aname:
                 WHEN "CoCurrencyId"   THEN  ttCURRPAIR.fCoCurrencyId = adetails.
                 WHEN "CurrencyId"     THEN  ttCURRPAIR.fCurrencyId   = adetails.
              END CASE.
           END.
        end.
        WHEN "CCX03"  THEN do :
           lChld = YES.
           CREATE ttCCX03.
           entries = iNode:ATTRIBUTE-NAMES.
           adetails = "".
           REPEAT j = 1 TO NUM-ENTRIES(entries):
              aname = ENTRY(j, entries).
              adetails =  iNode:GET-ATTRIBUTE(aname). 
              CASE aname:
                 WHEN "ReportDate"   THEN  ttCCX03.fReportDate = adetails.
              END CASE.
           END.
        end.
        WHEN "DOC_REQUISITES"  THEN do :
           lChld = YES.
           CREATE ttDOC_REQUISITES.
           entries = iNode:ATTRIBUTE-NAMES.
           adetails = "".
           REPEAT j = 1 TO NUM-ENTRIES(entries):
              aname = ENTRY(j, entries).
              adetails = iNode:GET-ATTRIBUTE(aname). 
              CASE aname:
                 WHEN "DOC_DATE"     THEN  ttDOC_REQUISITES.fDOC_DATE    = adetails.
                 WHEN "DOC_TIME"     THEN  ttDOC_REQUISITES.fDOC_TIME    = adetails.
                 WHEN "DOC_NO"       THEN  ttDOC_REQUISITES.fDOC_NO      = adetails.
                 WHEN "SENDER_ID"    THEN  ttDOC_REQUISITES.fSENDER_ID   = adetails.
                 WHEN "RECEIVER_ID"  THEN  ttDOC_REQUISITES.fRECEIVER_ID = adetails.
              END CASE.
           END.
        end.
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
