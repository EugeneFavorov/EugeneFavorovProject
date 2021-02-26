/*
               ОАО "Плюс Банк"
     Filename: pack-usl-kom.p
      Comment: процедура расчета превышения комиссии по пакетам услуг
   Parameters: 
      Used by: extrpars.fun
      Created: 21.10.2014 KAU   перенес из extrpars.fun для использования темп-табл
     Modified: 
*/

{globals.i}
{pp-corr.p}

DEF INPUT PARAMETER vAcct   AS CHAR. /* входящий параметр счет клиента */
DEF INPUT PARAMETER vDate   AS DATE. /* входящий параметр дата опердня */
DEF INPUT PARAMETER vSumKS  AS DEC.  /* входящий параметр сумма кассовой операции */
DEF INPUT-OUTPUT PARAMETER vKom AS CHAR.

DEF VAR vDateN      AS DATE NO-UNDO. /* дата начала месяца */
DEF VAR vCust-cat   AS CHAR NO-UNDO.
DEF VAR vCust-id    AS DEC  NO-UNDO.
DEF VAR vCust-OP    AS DATE NO-UNDO. /* дата заведения клиента */
DEF VAR vProd       AS CHAR NO-UNDO. /* название пакета услуг клиента */
DEF VAR vDateProd   AS DATE NO-UNDO. /* дата начала действия пакета услуг */
DEF VAR vDateX      AS DATE NO-UNDO.
DEF VAR vKDoc       AS DEC  NO-UNDO. /* Количество документов за месяц */  
DEF VAR vKDocT      AS DEC  NO-UNDO. /* Количество документов за опердень */
DEF VAR vSumVn      AS DEC  NO-UNDO. /* Сумма внесения средств на счет за месяц */
DEF VAR vSumIz      AS DEC  NO-UNDO. /* Сумма изъятия средств через кассу за месяц */
DEF VAR cNull       AS CHAR NO-UNDO. cNull = 'Нулевая1'.



/*
DEF TEMP-TABLE tt-packUsl  NO-UNDO
    FIELD      namePack    AS CHAR
    FIELD      kolDoc      AS DEC
    FIELD      summVn      AS DEC
    FIELD      summIz      AS DEC
    FIELD      kolSpr      AS DEC
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "Стартовый+"
       tt-packUsl.kolDoc = 10  tt-packUsl.summVn = 0       tt-packUsl.summIz = 100000
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "Базовый+"
       tt-packUsl.kolDoc = 20  tt-packUsl.summVn = 30000   tt-packUsl.summIz = 50000
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "Оптимальный+"
       tt-packUsl.kolDoc = 40  tt-packUsl.summVn = 50000   tt-packUsl.summIz = 200000
.
CREATE tt-packUsl.
ASSIGN tt-packUsl.namePack = "Премиум+"
       tt-packUsl.kolDoc = 80  tt-packUsl.summVn = 70000   tt-packUsl.summIz = 250000
.



/*==================== Второй счёт клиента, на него пакет не распространяется ====================*/
IF vAcct EQ "40702810800000127648     @0000" THEN RETURN.   /* ООО "Управляющая компания "Эксперт" */
/*================================================================================================*/



vDateN = DATE( '01' + SUBSTRING(STRING(vDate),3) ).



vKDocT = 0.



IF vKom EQ 'K08TAR' OR vKom EQ 'K34TAR' OR vKom EQ 'K01TAR' OR vKom EQ 'K19TAR' OR vKom EQ 'ЮЛКлбДо16' OR vKom EQ 'ЮЛКлбПосле16' OR vKom EQ 'ЮЛБумДо16' OR vKom EQ 'ЮЛБумПосле16'
THEN RUN SetSysConf IN h_base ("колдокподлопл", STRING(vKDocT)).



FIND FIRST acct WHERE acct.acct BEGINS vAcct NO-LOCK NO-ERROR.
IF AVAIL acct THEN DO:
    vCust-cat = acct.cust-cat.
    vCust-id  = acct.cust-id.
END.
ELSE MESSAGE "Счет не найден " vAcct VIEW-AS ALERT-BOX.



IF vCust-cat EQ 'Ю' THEN DO: FIND FIRST cust-corp WHERE cust-corp.cust-id EQ vCust-id NO-LOCK NO-ERROR.
                             vCust-op = cust-corp.date-in.
                             FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ 'Пакет'
                                                  AND tmpsigns.since     <  vDateN
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                  AND tmpsigns.code      EQ 'Пакет'
                                                                                  AND tmpsigns.since     EQ vCust-op NO-LOCK NO-ERROR.
                                                             IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                        vDateProd = tmpsigns.since.
                                                                                    END.    
                                                                               ELSE DO: FIND FIRST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                                                                              AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                                              AND tmpsigns.code      EQ 'ТарифныйПлан' NO-LOCK NO-ERROR.
                                                                                        IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                                                   vDateProd = tmpsigns.since.
                                                                                                               END.      
                                                                                    END.
                                                         END.
                         END.



IF vCust-cat EQ 'Ч' THEN DO: FIND FIRST person WHERE person.person-id EQ vCust-id NO-LOCK NO-ERROR.
                             vCust-op = person.date-in.
                             FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ 'Пакет'
                                                  AND tmpsigns.since     <  vDateN
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                  AND tmpsigns.code      EQ 'Пакет'
                                                                                  AND tmpsigns.since     EQ vCust-op NO-LOCK NO-ERROR.
                                                             IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                        vDateProd = tmpsigns.since.
                                                                                    END.    
                                                                               ELSE DO: FIND FIRST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                                                                              AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                                                                              AND tmpsigns.code      EQ 'ТарифныйПлан' NO-LOCK NO-ERROR.
                                                                                        IF AVAIL tmpsigns THEN DO: vProd     = tmpsigns.xattr-value.
                                                                                                                   vDateProd = tmpsigns.since.
                                                                                                               END.      
                                                                                    END.
                                                         END.
                         END.



/*============================================= action-date ===============================================*/
IF vKom EQ "VAcct" OR vKom EQ "VAcctB" OR vKom EQ "K27TAR"
THEN 
   DO:
      IF can-do("405*,406*,407*,40807*", vAcct)
         AND acct.currency = ""  
         AND GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date") <> ""
         AND (
             (
                (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) <> 11)
                AND
                (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) <> 12)
                AND
                (INT(MONTH(DATE(TODAY))) < INT(MONTH(ADD-INTERVAL(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")), +3, "months"))))
                AND
                (INT(YEAR(DATE(TODAY))) = INT(YEAR(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))))
             )
             OR
             (
                (
                   (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) = 11)
                   OR
                   (INT(MONTH(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) = 12)
                ) 
                AND
                (INT(MONTH(DATE(TODAY))) < INT(MONTH(ADD-INTERVAL(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")), +3, "months"))))
                AND
                (INT(YEAR(DATE(TODAY))) = INT(YEAR(DATE(GetXAttrValue("acct", vAcct + "," + acct.currency, "action-date")))) + 1)
             )
             )
      THEN
         DO: 
            vKom = cNull.
            RETURN.
         END.
   END.
/*================================================= end ===================================================*/



/*========================================= Пакет услуг Стартовый+ ========================================*/
IF vKom EQ "VAcct" OR vKom EQ "VAcctB" OR vKom EQ "K27TAR"
THEN 
   DO:
      IF vProd NE "" 
      THEN
         DO:
            vDateX = date_correct(month(tmpsigns.since), 3, 1, year(tmpsigns.since)).

            IF tmpsigns.xattr-value = "Стартовый+"
               AND vDate < vDateX
               AND tmpsigns.since <= date("31/12/2016")
            THEN 
               DO:
                  vKom = cNull.
                  RETURN.
               END.
         END.
   END.
/*=================================================== end =================================================*/
*/



/*======================================== Тарифный план Тест-драйв =========================================*/
FIND FIRST acct WHERE acct.acct BEGINS vAcct NO-LOCK NO-ERROR.
IF AVAIL acct THEN DO: vCust-cat = acct.cust-cat.
                       vCust-id  = acct.cust-id.
                   END.
              ELSE MESSAGE "Счет не найден " vAcct VIEW-AS ALERT-BOX.

IF vCust-cat EQ 'Ю' THEN DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ 'ТарифныйПлан'
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE RETURN.
                         END.

IF vCust-cat EQ 'Ч' THEN DO: FIND LAST tmpsigns WHERE tmpsigns.file-name EQ 'person'
                                                  AND tmpsigns.surrogate EQ STRING(vCust-id)
                                                  AND tmpsigns.code      EQ 'ТарифныйПлан'
                                  NO-LOCK NO-ERROR.
                                  IF AVAIL TMPSIGNS THEN DO: vProd     = tmpsigns.xattr-value.
                                                             vDateProd = tmpsigns.since.
                                                         END.
                                                    ELSE RETURN.
                         END.

IF vProd EQ "Тест-драйв" AND (vKom EQ "ЮЛКлбДо16"    OR 
                              vKom EQ "ЮЛКлбПосле16" OR 
                              vKom EQ "VAcct"        OR 
                              vKom EQ "VAcctB"       OR 
                              vKom EQ "K27TAR")
THEN
     DO:
         IF today <= vDateProd + 90
         THEN DO:
                  vKom = cNull.
                  RETURN.
              END.

     END.
/*==================================================== end ==================================================*/



/*
FIND FIRST tt-packUsl WHERE tt-packUsl.namePack EQ vProd NO-ERROR.
IF NOT AVAIL tt-packUsl THEN DO:
   RETURN.
END.



/* Проверяем у клиента дата начала пакета не совпадает с датой открытия счета + клиент написал заявление в этом месяце, тогда тариф прежний. */
IF     vDateProd NE vCust-op
   AND vDateProd >= vDateN
THEN DO:
   RETURN.
END.    



/*============================================ 1 Открытие счета =============================================*/
IF vKom EQ 'OtkSch' 
THEN DO:
    vKom = cNull.
    RETURN.
END.    



/*============================================ 2 Ежемесячное ведение счета ==================================*/
IF vKom EQ 'VAcct' OR vKom EQ 'VAcctB'
THEN DO:
    IF acct.currency = '' THEN DO: 
        vKom = cNull.
        RETURN.
    END.
    ELSE
        RETURN.
END.



/*============================================ 3 Ежемесячное обслуживание КЛБАНК ============================*/
IF vKom EQ 'K27TAR'
THEN DO:
    IF acct.currency = '' THEN DO: 
        vKom = cNull.
        RETURN.
    END.
    ELSE
        RETURN.
END.



/*============================================ 4 Перечисление ден. ср-в со счета ============================*/
IF vKom EQ 'K08TAR' OR vKom EQ 'K34TAR' OR vKom EQ 'K01TAR' OR vKom EQ 'K19TAR'
THEN DO:

    /*===================== fev =====================*/
    IF vAcct BEGINS "40821" OR vAcct BEGINS "40701"
    THEN
    DO:
    FIND FIRST acct WHERE acct.acct BEGINS vAcct
                      AND can-do('СпецПА,СпБрок', acct.contract)
                    NO-LOCK NO-ERROR.
    IF AVAIL acct THEN RETURN. 
    END.
    /*===================== end =====================*/

    vKDoc = 0.
    FOR EACH acct WHERE acct.cust-cat = vCust-cat
                    AND acct.cust-id = vCust-id
                    AND acct.contract = 'Расчет'
    NO-LOCK:
        FOR EACH op-entry WHERE op-entry.acct-db EQ acct.acct
                            AND can-do('√,√√,ФБП,ФДД', op-entry.op-status)
                            AND op-entry.op-date >= vDateN 
                            AND op-entry.op-date <= vDate
                            AND can-do('30102*,30301*,30223*1', op-entry.acct-cr)
        NO-LOCK:
        FIND op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
        IF AVAIL op 
             AND op.doc-type BEGINS '01' 
             AND NOT op.ben-acct BEGINS '40101'
             AND NOT op.ben-acct BEGINS '40102'
             AND NOT op.ben-acct BEGINS '40201'
             AND NOT op.ben-acct BEGINS '40204'
             AND NOT op.ben-acct BEGINS '40402'
        THEN DO:
            FIND FIRST op-bank WHERE op-bank.op EQ op.op 
                                 AND op-bank.bank-code-type EQ 'МФО-9'

                              /* AND op-bank.bank-code NE '045209783'
                                 AND op-bank.bank-code NE '047106641' */

                                 AND op-bank.bank-code NE '044525129'
                                 AND op-bank.bank-code NE '047106641'
                                 AND op-bank.bank-code NE '045209884'
            NO-LOCK NO-ERROR.
            IF NOT AVAIL op-bank THEN NEXT.
        END.
        ELSE NEXT.
            vKDoc = vKDoc + 1.
            IF op-entry.op-date EQ vDate THEN vKDocT = vKDocT + 1.
        END.

    END.

    IF vKDoc - vKDocT <= tt-packUsl.kolDoc
    THEN DO:
        IF vKDoc > tt-packUsl.kolDoc
        THEN DO:
            vKDocT = vKDoc - tt-packUsl.kolDoc.
        END.
        ELSE DO:
            vKDocT = 0.
            vKom = cNull.
        END.
        RUN SetSysConf IN h_base ("колдокподлопл",STRING(vKDocT)).
        RETURN.
    END.
    ELSE DO:
        MESSAGE "У счета " vAcct SKIP
                "Превышено максимальное количество документов по счету" SKIP
                STRING(vKDoc) " > " STRING(tt-packUsl.kolDoc) VIEW-AS ALERT-BOX.
        RETURN.
    END.

END.



/*============================================ 5 Перечисление ден. ср-в со счета КЛБ и БУМ ===================*/
IF vKom EQ 'ЮЛКлбДо16' OR vKom EQ 'ЮЛКлбПосле16' OR vKom EQ 'ЮЛБумДо16' OR vKom EQ 'ЮЛБумПосле16'
THEN DO:

    vKDoc = 0.
    FOR EACH acct WHERE acct.cust-cat = vCust-cat
                    AND acct.cust-id = vCust-id
                    AND acct.contract = 'Расчет'
    NO-LOCK:
        FOR EACH op-entry WHERE op-entry.acct-db EQ acct.acct
                            AND can-do('√,√√,ФБК,ФБН,ФБО,ФБП,ФДД', op-entry.op-status)
                            AND op-entry.op-date >= vDateN 
                            AND op-entry.op-date <= vDate
                            AND can-do('30102*,30301*,30223*1', op-entry.acct-cr)
        NO-LOCK:
        FIND op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
        IF AVAIL op 
             AND op.doc-type BEGINS '01' 
             AND can-do('!40101*,!40102*,!40201*,!40204*,!40402*,*', op.ben-acct)
        THEN DO:
            FIND FIRST op-bank WHERE op-bank.op EQ op.op 
                                 AND op-bank.bank-code-type EQ 'МФО-9'
                                 AND can-do('!044525129,!047106641,!045209884,!"",*', op-bank.bank-code)                        
            NO-LOCK NO-ERROR.
            IF NOT AVAIL op-bank THEN NEXT.
        END.
        ELSE NEXT.
            vKDoc = vKDoc + 1.
            IF op-entry.op-date EQ vDate THEN vKDocT = vKDocT + 1.
        END.

    END.

    vKDoc = vKDoc + 1.

    /* message string(vKDoc) view-as alert-box. */

    IF vKDoc - vKDocT <= tt-packUsl.kolDoc
    THEN 
       DO:
          IF vKDoc > tt-packUsl.kolDoc
             THEN 
                DO:
                   vKDocT = vKDoc - tt-packUsl.kolDoc.
                   MESSAGE "У счета " vAcct SKIP
                           "Превышено максимальное количество документов по счету" SKIP
                           STRING(vKDoc) " > " STRING(tt-packUsl.kolDoc) VIEW-AS ALERT-BOX.

                END.
             ELSE 
                DO:
                   vKDocT = 0.
                   vKom = cNull.
                   RUN SetSysConf IN h_base ("fflag", 0).
                END.
          RUN SetSysConf IN h_base ("колдокподлопл", STRING(vKDocT)).
          RETURN.
       END.
    ELSE 
       DO:
          MESSAGE "У счета " vAcct SKIP
                  "Превышено максимальное количество документов по счету" SKIP
                  STRING(vKDoc) " > " STRING(tt-packUsl.kolDoc) VIEW-AS ALERT-BOX.
          RETURN.
       END.
END.



/*============================================ 6 Прием, пересчет и зачисление на расчетные счет нал. ден. ср-в ===*/
IF (vKom BEGINS 'PrZachUr') AND (vProd <> "Стартовый+")
THEN DO:

    /*===================== fev =====================*/
    IF vAcct BEGINS "40821" OR vAcct BEGINS "40701"
    THEN
    DO:
    FIND FIRST acct WHERE acct.acct BEGINS vAcct
                      AND can-do('СпецПА,СпБрок', acct.contract)
                    NO-LOCK NO-ERROR.
    IF AVAIL acct THEN RETURN. 
    END.
    /*===================== end =====================*/

    vSumVn = 0.
    FOR EACH acct WHERE acct.cust-cat = vCust-cat
            AND acct.cust-id = vCust-id
            AND acct.contract = 'Расчет'
    NO-LOCK:
        FOR EACH op-entry WHERE op-entry.acct-cr EQ acct.acct
                AND op-entry.acct-db BEGINS '20202'
                AND op-entry.op-date >= vDateN 
                AND op-entry.op-date <= vDate
        NO-LOCK:
            MESSAGE op-entry.op-date " " op.doc-num VIEW-AS ALERT-BOX.
            vSumVn = vSumVn + op-entry.amt-rub.
        END.
    END.
    
    DEF VAR vOverVn AS DEC INIT 0.00 NO-UNDO.

    IF tt-packUsl.summVn >= vSumVn + vSumKS THEN DO:    /* если сумма внесения по пакету не исчерпана */
        vKom = cNull.
        MESSAGE "Лимит для тарифа " tt-packUsl.namePack " не исчерпан." SKIP
                "Комиссия 0 рублей." VIEW-AS ALERT-BOX.
    END.
    ELSE
        IF tt-packUsl.summVn <= vSumVn THEN           /* если сумма внесения по пакету исчерпана */
            MESSAGE "Лимит для тарифа " tt-packUsl.namePack " исчерпан." SKIP
                    "Комиссия в соответствии с тарифами банка." VIEW-AS ALERT-BOX.
        ELSE DO:                                      /* если сумма внесения по пакету исчерпана частично */
            vOverVn = vSumKS + vSumVn - tt-packUsl.summVn.
            MESSAGE "Лимит внесения для тарифа " tt-packUsl.namePack " исчерпан." SKIP
                    "Для " vOverVn " руб. будет списана комиссия." VIEW-AS ALERT-BOX.
        END.

    RUN SetSysConf IN h_base ("сумсверхпак",STRING(vOverVn)).
    RETURN.
    
END.



/*============================================ 7 Выдача чек.книжки 50 листов ================================*/
IF vKom BEGINS 'ЧекКн50'
THEN DO:
    MESSAGE 'Выдавалась ли клиенту чековая книжка(50 л.)'
            SKIP(1)
            ' за время действия пакета?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE chs AS LOGICAL.
    IF chs THEN
        RETURN.
    ELSE DO:
        vKom = cNull.
        RETURN.
    END.
    /*vKom = cNull.
    MESSAGE 'Комиссия для счета ' + vAcct + ' включена в тариф ' + vProd VIEW-AS ALERT-BOX.
    RETURN.*/
END.



/*============================================ 8 Выдача дубликатов выписок ==================================*/
IF (vKom EQ 'DublOD' OR vKom EQ 'DublZD') AND (vProd EQ 'Оптимальный+' OR vProd EQ 'Премиум+')
THEN DO:
        vKom = cNull.
        MESSAGE 'Комиссия для счета ' + vAcct + ' включена в тариф ' + vProd VIEW-AS ALERT-BOX.
        RETURN.
END.



/*============================================ 9 Услуга SMS =================================================*/
IF vKom EQ 'SMS' AND (vProd EQ 'Премиум+' OR vProd EQ 'Оптимальный+')
THEN DO:
    vKom = cNull.
    RETURN.
END.
*/