/*
Банковская интегрированная система БИСквит
Copyright:
Filename:   getomskacct2.p
Comment:    Ищем клиента в базе BANK и создаем в БИСе
Parameters:
Uses:
Used by:
Created: kam
Modified:
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{intrface.get cust}
{intrface.get xclass}
{getacct2bis.i}

DEFINE INPUT PARAMETER vAcctMask AS CHAR.
/*{setdest.i}*/

DEF VAR iCnt AS INT NO-UNDO.
DEF VAR iCntLoaded AS INT NO-UNDO.
DEF VAR tmpStr AS CHAR NO-UNDO.
DEF VAR vCust-id AS INT NO-UNDO.
DEF VAR shF AS CHAR NO-UNDO.
DEF VAR vStartTime AS DATETIME NO-UNDO.

DEFINE VARIABLE mCreAcct AS LOGICAL NO-UNDO.
DEFINE VARIABLE mCreUpd AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMFRFilial AS CHARACTER NO-UNDO.

DEF VAR acctF AS CHAR NO-UNDO.
DEF BUFFER links1 FOR bank.accounts_link.
DEF BUFFER links2 FOR bank.accounts_link.
DEF BUFFER acct1 FOR acct.
DEF BUFFER acct2 FOR acct.
DEF VAR mLinkId      AS INT64    NO-UNDO.
DEF VAR mUpd         AS LOGICAL  NO-UNDO.
DEF VAR mOk          AS LOGICAL  NO-UNDO.
DEF VAR mLinkSurr    AS CHARACTER NO-UNDO.

/*MESSAGE vAcctMask VIEW-AS ALERT-BOX.*/
iCnt = 0.
iCntLoaded = 0.
shF = shFilial.
vStartTime = NOW.
etime(TRUE).
/*FOR EACH acct-mfr
  WHERE acct-mfr.acct MATCHES vAcctMask
  AND (acct-mfr.close_date EQ ? OR acct-mfr.close_date >= DATE( 1,1, 2014))
/*  AND acct-mfr.filial_id EQ '0300' */
  NO-LOCK:
  MESSAGE acct-mfr.acct VIEW-AS ALERT-BOX.*/

FOR EACH bank.accounts_change
    WHERE accounts_change.status_ EQ 0
    /*  AND accounts_change.account MATCHES "47423810*10000*" */
    EXCLUSIVE-LOCK BY accounts_change.date_modify QUERY-TUNING ( NO-INDEX-HINT ):
  
  DO ON ERROR UNDO, THROW:
  
     PUT UNFORMATTED "accounts_change.account = " accounts_change.account SKIP.
     /* связь счетов подотчета 60308 с ПК 40817 */
     IF accounts_change.account begins '03001' THEN
     DO:
        FIND FIRST links1
           WHERE STRING(links1.linktype) MATCHES '200'
           AND links1.account2 EQ accounts_change.account NO-LOCK NO-ERROR.
        IF AVAIL links1 THEN
           FIND FIRST links2
              WHERE STRING(links2.linktype) MATCHES '212'
              AND links2.account1 EQ links1.account1 NO-LOCK NO-ERROR.
        ELSE RELEASE links2.
        IF AVAIL links2 THEN 
        DO:
           FIND FIRST acct1 WHERE
              acct1.number EQ links2.account1 NO-LOCK NO-ERROR.
           FIND FIRST acct2 WHERE
              acct2.number EQ links2.account2 NO-LOCK NO-ERROR.
        END. 
        ELSE 
        DO:
           RELEASE acct1.
           RELEASE acct2.
        END.
        IF AVAIL acct1 AND AVAIL acct2 THEN 
        DO:
           mLinkId = GetXLinkID("acctb","СчетПодПК").
           FIND FIRST qbis.links
              WHERE qbis.links.link-id EQ mLinkId
              AND qbis.links.source-id EQ acct1.acct + ',' + acct1.currency
              AND qbis.links.target-id EQ acct2.acct + ',' + acct2.currency
              NO-LOCK NO-ERROR.
           IF NOT AVAIL qbis.links THEN 
           DO:
              CREATE qbis.links.
              ASSIGN
                 qbis.links.link-id = mLinkId
                 qbis.links.source-id = acct1.acct + ',' + acct1.currency
                 qbis.links.target-id = acct2.acct + ',' + acct2.currency
                 qbis.links.beg-date  = today
                 .
              VALIDATE qbis.links.
              PUT UNFORMATTED
                 "добавили связь " acct1.acct " " acct2.acct SKIP.
           END.
        END.
     END.
  
     IF LENGTH( accounts_change.account) EQ 20 AND
        SUBSTR( accounts_change.account, 1, 1) NE '0' THEN 
     DO:

        FIND FIRST bank.acct-mfr
           WHERE accounts_change.account EQ acct-mfr.acct NO-LOCK NO-ERROR.
        IF NOT AVAIL acct-mfr THEN 
        DO:
        /* удаляем счет */
        /*  IF NOT AVAIL acct-mfr THEN
            UNDO, THROW NEW Progress.Lang.AppError( "счет не найден.").*/
        END. 
        ELSE 
        DO:
/*           IF (CAN-DO("0500",acct-mfr.filial_id) AND                                                   */
/*              (acct-mfr.close_date EQ ? OR acct-mfr.close_date >= DATE("01/01/2015"))                  */
/*              )  OR (                                                                                  */
/*              CAN-DO("0000", acct-mfr.filial_id) AND acct-mfr.acct begins '30221840301100010013XXX' AND*/
/*              (acct-mfr.close_date EQ ? OR acct-mfr.close_date >= DATE( 11, 17, 2014))                 */
/*              )                                                                                        */
/*              THEN                                                                                     */
/*           DO:                                                                                         */

           
           mMFRFilial = acct-mfr.filial_id.

           PUT UNFORMATTED 
              "acct-mfr.acct = " acct-mfr.acct 
              "; shFilial = " shFilial 
              "; acct-mfr.filial_id = " acct-mfr.filial_id
              "; acct-mfr.close_date = " acct-mfr.close_date
           SKIP.

           IF CAN-DO("0500",acct-mfr.filial_id)
/*           AND acct-mfr.close_date EQ ?*/
           AND (acct-mfr.grp EQ 599 OR acct-mfr.grp EQ 596)
/*           AND (acct-mfr.close_date EQ ? OR acct-mfr.close_date >= DATE("01/01/2015"))*/
           THEN 
           DO:
              
              acctF = acct-mfr.filial_id.
              shFilial = acctF.
              
              PUT UNFORMATTED "acct-mfr.acct = " acct-mfr.acct "; shFilial = " shFilial SKIP.
              
              FIND bal-acct WHERE bal-acct.bal-acct = int64(substring(acct-mfr.acct,1,5)) NO-ERROR.
              IF NOT AVAIL bal-acct 
                 THEN UNDO, THROW NEW Progress.Lang.AppError( "Не найден счет второго порядка " + substring(acct-mfr.acct,1,5) + " в плане счетов!").
              IF bal-acct.side NE acct-mfr.side AND (NOT CAN-DO("30218*,61209*,61210*,61212*,61214*", acct-mfr.acct))
                 THEN  UNDO, THROW NEW Progress.Lang.AppError( "у счета не верный признак А/П (" + (IF acct-mfr.side EQ ? THEN "?" ELSE acct-mfr.side) + ") согласно плана счетов (" + bal-acct.side + ") !").

              DEF VAR mKey AS INT NO-UNDO.
              DEF VAR bmfo9 AS CHAR NO-UNDO.
              
/*              IF shFilial EQ '0400' THEN bmfo9 = '044599129'.     */
/*              ELSE IF shFilial EQ '0000' THEN bmfo9 = '045209783'.*/
/*              ELSE IF shFilial EQ '0300' THEN bmfo9 = '047106641'.*/
/*              ELSE IF shFilial EQ '0500' THEN bmfo9 = '045209884'.*/
/*              ELSE bmfo9 = '044599129'.                           */
              
              IF      shFilial EQ '0000' THEN bmfo9 = '044525129'.
              ELSE IF shFilial EQ '0300' THEN bmfo9 = '047106641'.
              ELSE IF shFilial EQ '0400' THEN bmfo9 = '044599129'.
              ELSE IF shFilial EQ '0500' THEN bmfo9 = '045209884'.
              ELSE bmfo9 = '044525129'.
              
              DEFINE VARIABLE mNewAcct AS CHARACTER NO-UNDO.
              DEFINE VARIABLE mNewKey  AS INT64     NO-UNDO.
              
              mNewKey  = INT64(SUBSTRING(acct-mfr.acct,9,1)) - 1.
              mNewKey  = IF mNewKey EQ (- 1) THEN 9 ELSE mNewKey. 
              mNewAcct = SUBSTRING(acct-mfr.acct,1,8) + 
                         STRING(mNewKey) +
                         SUBSTRING(acct-mfr.acct,10). 

              IF acct-mfr.acct EQ "40702810606600010110" THEN bmfo9 = '047106641'.
              
              IF acct-mfr.acct EQ "40817810204000010249" 
              OR acct-mfr.acct EQ "40817810004000010067" 
              OR acct-mfr.acct EQ "40817810004000010193"
              OR acct-mfr.acct EQ "40817810504000010198"
              OR acct-mfr.acct EQ "40817810604000010182"
              THEN
              DO:
                 RUN key-tst.p (mNewAcct,bmfo9,OUTPUT mKey).
                 PUT UNFORMATTED "bmfo9 = " bmfo9 "; mKey = " mKey "; acct = " substr(mNewAcct,9,1) SKIP.
                 IF mKey NE ? AND
                 mKey NE INT64(substr(mNewAcct,9,1)) THEN 
                 DO:
                    UNDO, THROW NEW Progress.Lang.AppError( "Ошибка ключа лицевого счета").
                 END.
              END.   
              ELSE
              DO: 
                 RUN key-tst.p (acct-mfr.acct,bmfo9,OUTPUT mKey).
                 PUT UNFORMATTED "bmfo9 = " bmfo9 "; mKey = " mKey "; acct = " substr(acct-mfr.acct,9,1) SKIP.
                 IF mKey NE ? AND
                 mKey NE INT64(substr(acct-mfr.acct,9,1)) THEN 
                 DO:
                    UNDO, THROW NEW Progress.Lang.AppError( "Ошибка ключа лицевого счета").
                 END.
              END.
              
              IF acct-mfr.acct BEGINS '3030' THEN
                 UNDO, THROW NEW Progress.Lang.AppError( "Счета кор.счетов не трогаем").
              
              vCust-id = ?.
              IF acct-mfr.cust_cat EQ 'Ч' THEN 
              DO:
                 FIND FIRST signs
                    WHERE signs.file-name = 'person'
                    AND signs.CODE EQ 'CID' AND signs.dec-value EQ acct-mfr.cid NO-LOCK NO-ERROR.
                 IF NOT AVAIL signs THEN 
                    FIND FIRST signs
                       WHERE signs.file-name = 'person'
                       AND signs.CODE EQ 'CIDIP' AND signs.dec-value EQ acct-mfr.cid NO-LOCK NO-ERROR.
                 IF AVAIL signs THEN vCust-id = INTEGER(signs.surrogate).
                 ELSE 
                 DO:
                    UNDO, THROW NEW Progress.Lang.AppError( "Не найден person CID=" + STRING( acct-mfr.cid)).
                 END.
              END.
              IF acct-mfr.cust_cat EQ 'Ю' THEN 
              DO:
                 FIND FIRST signs
                    WHERE signs.file-name = 'cust-corp'
                    AND signs.CODE EQ 'CID' AND signs.dec-value EQ acct-mfr.cid NO-LOCK NO-ERROR.
                 IF AVAIL signs THEN vCust-id = INTEGER(signs.surrogate).
                 ELSE 
                 DO:
                    UNDO, THROW NEW Progress.Lang.AppError( "Не найден cust-corp CID=" + STRING( acct-mfr.cid)).
                 END.
              END.
              IF acct-mfr.cust_cat EQ 'Б' THEN 
              DO:
                 DEF VAR BICbnk AS CHAR NO-UNDO.
                 FIND FIRST bank.clients-abs WHERE clients-abs.cid = acct-mfr.cid NO-LOCK NO-ERROR.
                 IF NOT AVAIL bank.clients-abs THEN
                    UNDO, THROW NEW Progress.Lang.AppError( "Не найден клиент CID=" + STRING( acct-mfr.cid)).
                 FIND FIRST expdoc
                    WHERE expdoc.did EQ bank.clients-abs.did
                    AND expdoc.field_ EQ 120
                    AND (expdoc.volume EQ ? OR expdoc.volume EQ 1) NO-LOCK NO-ERROR.
                 BICbnk = ?.
                 IF NOT AVAIL bank.expdoc OR expdoc.contain EQ '' THEN  /*047102645*/
                 DO:
                    IF      acct-mfr.cid EQ 9473   THEN BICbnk = '044525495'. 
                    ELSE IF acct-mfr.cid EQ 18316  THEN BICbnk = '045209673'. 
                    ELSE IF acct-mfr.cid EQ 22458  THEN BICbnk = '044579918'. 
                    ELSE IF acct-mfr.cid EQ 85534  THEN BICbnk = '045209777'. 
                    ELSE IF acct-mfr.cid EQ 219791 THEN BICbnk = '045004832'. 
                    ELSE IF acct-mfr.cid EQ 289738 THEN BICbnk = '044030814'. 
                    ELSE IF acct-mfr.cid EQ 291382 THEN BICbnk = '045004728'. 
                    ELSE IF acct-mfr.cid EQ 314678 THEN BICbnk = '046551976'. 
                    ELSE IF acct-mfr.cid EQ 337032 THEN BICbnk = '044525213'. 
                    ELSE IF acct-mfr.cid EQ 376511 THEN BICbnk = '044583209'. 
                    ELSE IF acct-mfr.cid EQ 386273 THEN BICbnk = '046027213'. 
                    ELSE IF acct-mfr.cid EQ 16992  THEN BICbnk = '044525555'.
                    ELSE IF acct-mfr.cid EQ 288298 THEN BICbnk = '044525767'.
                    ELSE IF acct-mfr.cid EQ 110547 THEN BICbnk = '044525716'.
                    ELSE IF acct-mfr.cid EQ 19496  THEN BICbnk = '047102645'.
                    ELSE IF acct-mfr.cid EQ 35423  THEN BICbnk = 'COBADEFFXXX'. 
                    ELSE IF acct-mfr.cid EQ 22864  THEN BICbnk = 'TSESKZKAXXX'. 
                    ELSE IF acct-mfr.cid EQ 35424  THEN BICbnk = 'ABKZKZKX'.
                    ELSE UNDO, THROW NEW Progress.Lang.AppError( "У клиента CID=" + STRING( acct-mfr.cid) + ' не найден БИК').
                 END. 
                 ELSE BICbnk = expdoc.contain.

                 IF BICbnk EQ '047102645' THEN
                 DO:
                    vCust-id = 3595.
                 END.
                 ELSE
                 DO:    
                    FIND FIRST banks-code
                       WHERE banks-code.bank-code = BICbnk
                       AND banks-code.bank-code-type = "МФО-9" NO-LOCK NO-ERROR.
                    IF NOT AVAIL banks-code THEN
                       FIND FIRST banks-code
                          WHERE banks-code.bank-code = BICbnk
                          AND banks-code.bank-code-type = "BIC" NO-LOCK NO-ERROR.
                    IF NOT AVAIL banks-code THEN
                       UNDO, THROW NEW Progress.Lang.AppError( "Не найден банк с БИК=" + BICbnk).
                    vCust-id = banks-code.bank-id.
                 END.
                 RELEASE bank.clients-abs.
                 RELEASE bank.expdoc.
                 RELEASE banks-code.
              END.
              IF acct-mfr.cust_cat EQ "Ч" THEN 
              DO:
                 FIND FIRST person WHERE person-id = vCust-id.
                 RUN SetClientRole IN h_cust (STRING(BUFFER person:HANDLE), "Ч", YES).
              END.
              IF acct-mfr.cust_cat EQ "Ю" THEN 
              DO:
                 FIND FIRST cust-corp WHERE cust-corp.cust-id = vCust-id.
                 RUN SetClientRole IN h_cust (STRING(BUFFER cust-corp:HANDLE), "Ю", YES).
              END.

              IF acct-mfr.acct EQ "40817810204000010249"
              OR acct-mfr.acct EQ "40817810004000010067" 
              OR acct-mfr.acct EQ "40817810004000010193"
              OR acct-mfr.acct EQ "40817810504000010198"
              OR acct-mfr.acct EQ "40817810604000010182"
              THEN
              DO:
                 FIND FIRST acct
                    WHERE acct.acct EQ AddFilToAcct(mNewAcct,acctF) EXCLUSIVE-LOCK NO-ERROR.
                 IF NOT AVAIL acct THEN 
                 DO:
                    CREATE acct.
                    ASSIGN
                       acct.acct = AddFilToAcct(mNewAcct,acctF)
                       acct.bal-acct = bal-acct.bal-acct
                       acct.user-id = "SYNC"
                       acct.acct-cat = acct-mfr.acct_cat
                       acct.class-code = 'acct' + acct.acct-cat
                       acct.number = mNewAcct.
                    mCreAcct = YES. 
                 END.
              END.
              ELSE
              DO:              
                 FIND FIRST acct
                    WHERE acct.acct EQ AddFilToAcct( acct-mfr.acct, acctF) EXCLUSIVE-LOCK NO-ERROR.
                 IF NOT AVAIL acct THEN 
                 DO:
                    CREATE acct.
                    ASSIGN
                       acct.acct = AddFilToAcct( acct-mfr.acct, acctF)
                       acct.bal-acct = bal-acct.bal-acct
                       acct.user-id = "SYNC"
                       acct.acct-cat = acct-mfr.acct_cat
                       acct.class-code = 'acct' + acct.acct-cat
                       acct.number = acct-mfr.acct.
                    mCreAcct = YES. 
                 END.
              END.           
              
              DEF VAR vDetails AS CHAR NO-UNDO.
              vDetails = Details2BisStyle(acct-mfr.details).
              IF acct-mfr.acct begins '10207' OR acct-mfr.acct begins '40802' THEN 
              DO:
                 FIND FIRST bank.clients-abs WHERE clients-abs.cid = acct-mfr.cid NO-LOCK NO-ERROR.
                 vDetails = (IF AVAIL clients-abs THEN clients-abs.fname ELSE "").
                 RELEASE clients-abs.
              END.

/*              PUT UNFORMATTED                                     */
/*                 acct-mfr.acct ";"                                */
/*                 Contract2BisStyle(acct-mfr.acctype,acct-mfr.acct)*/
/*              SKIP.                                               */
              
              DEF VAR acctBr AS CHAR NO-UNDO.
              acctBr = acctF.
              IF acctF EQ '0300' AND CAN-DO("2020*,91202*,91207*",acct.acct) THEN 
              DO:
                 CASE SUBSTR(acct.acct,10,4):
                    WHEN '1420' THEN 
                       acctBr = '0301'.
                    WHEN '1480' THEN 
                       acctBr = '0302'.
                 END.
              END.
              ASSIGN
                 acct.contract = Contract2BisStyle( acct-mfr.acctype, acct-mfr.acct)
                 WHEN acct.contract NE Contract2BisStyle( acct-mfr.acctype, acct-mfr.acct)
                 acct.details = vDetails
                 WHEN acct.details NE vDetails
                 acct.currency = Curr2BisStyle(acct-mfr.acct, acct-mfr.currency)
                 WHEN acct.currency NE Curr2BisStyle(acct-mfr.acct, acct-mfr.currency)
                 acct.rate-type = (if acct.currency = "" or acct.bal-acct EQ 60313 or acct.bal-acct EQ 60314 then "" else "Учетный")
                 WHEN acct.rate-type NE (if acct.currency = "" or acct.bal-acct EQ 60313 or acct.bal-acct EQ 60314 then "" else "Учетный")
                 acct.side = (IF CAN-DO("61209*,61210*,61212*", acct-mfr.acct) THEN 'АП' ELSE bal-acct.side)
                 WHEN acct.side NE (IF CAN-DO("61209*,61210*,61212*", acct-mfr.acct) THEN 'АП' ELSE bal-acct.side)
                 acct.cust-cat = acct-mfr.cust_cat
                 WHEN acct.cust-cat NE acct-mfr.cust_cat
                 acct.cust-id = vCust-id
                 WHEN acct.cust-id NE vCust-id
                 acct.open-date = acct-mfr.open_date
                 WHEN acct.open-date NE acct-mfr.open_date
                 acct.branch-id = acctBr
                 WHEN     acct.branch-id NE acctBr
                 acct.close-date = acct-mfr.close_date
                 WHEN acct.close-date NE acct-mfr.close_date
                 .
                 
              IF acct.currency NE "" THEN 
              DO:
                 IF acct.bal-acct EQ 60313 or acct.bal-acct EQ 60314 
                 THEN acct.rate-type = "". 
                 ELSE acct.rate-type = "Учетный".
              END.

              PUT UNFORMATTED "acct.currency = " acct.currency "; acct.rate-type = " acct.rate-type SKIP.
              
              VALIDATE acct.
              
              

               /*КорпКарт*/
               IF CAN-DO("407*,40802*,40807*",acct-mfr.acct)
               AND acct-mfr.grp EQ 599 THEN
               DO:
                  mUpd = UpdateSigns("acctb",acct.acct + "," + acct.curr,"КорпКарт","Да",?).
                  PUT UNFORMATTED "UpdateSigns КорпКарт " mUpd SKIP.
               END.
               
               /*ДогОткрЛС*/
               IF mCreAcct EQ YES THEN
               DO:
                  IF CAN-DO("407*,40802*,40807*",acct-mfr.acct)
                  AND acct-mfr.grp EQ 599 THEN
                     mUpd = UpdateSigns("acctb",acct.acct + "," + acct.curr,"ДогОткрЛС",STRING(acct-mfr.open_date,"99/99/9999") + "," + TRIM(STRING(acct.cust-id)) + "/КК" ,?).
                  ELSE mUpd = UpdateSigns("acctb",acct.acct + "," + acct.curr,"ДогОткрЛС",STRING(acct-mfr.open_date,"99/99/9999") + "," + TRIM(STRING(acct.cust-id)),?).
                  PUT UNFORMATTED "UpdateSigns ДогОткрЛС " mUpd SKIP.
               END.
               
               /*СотрОткрСч*/
               IF mCreAcct EQ YES THEN
               DO:
                  IF acct-mfr.grp EQ 599 THEN
                     mUpd = UpdateSigns("acctb",acct.acct + "," + acct.curr,"СотрОткрСч","SYNC",?).
                  PUT UNFORMATTED "UpdateSigns СотрОткрСч " mUpd SKIP.
               END.
               
               /*groups*/
               mOk = UpdateSigns("acctb",
                     acct.acct + ',' + acct.currency,
                     "groupOABS",
                     TRIM(STRING(acct-mfr.grp,">>>>>9")),
                     ?).
               PUT UNFORMATTED "UpdateSigns groupOABS " mOk SKIP.
                    
               FIND FIRST qbis.links WHERE
                      qbis.links.link-id   EQ 190
                  AND qbis.links.source-id EQ acct.acct + "," + acct.currency
               NO-LOCK NO-ERROR.
               
               IF AVAIL(qbis.links) THEN    /*update*/
               FOR EACH qbis.links WHERE
                      qbis.links.link-id   EQ 190
                  AND qbis.links.source-id EQ acct.acct + "," + acct.currency
                  EXCLUSIVE-LOCK:
                  ASSIGN
                     qbis.links.target-id = TRIM(STRING(acct-mfr.grp,">>>>>9")).
                  PUT UNFORMATTED "update qbis.links " + acct.acct + " " + TRIM(STRING(acct-mfr.grp,">>>>>9")) SKIP.   
               END.
               ELSE
               DO:
                  /*create*/      
                  RUN CreateLinksRetSurr(
                      "acct",
                      "acct-group",
                      acct.acct + "," + acct.currency,
                      TRIM(STRING(acct-mfr.grp,">>>>>9")),
                      DATE("01/01/1900"),
                      ?,
                      "",
                      OUTPUT mLinkSurr) NO-ERROR.
                  PUT UNFORMATTED "create qbis.links " + acct.acct + " " + TRIM(STRING(acct-mfr.grp,">>>>>9")) SKIP.
               END.
               /*groups*/
               
              IF acct.acct-cat EQ 'd' AND NOT CAN-FIND(FIRST sec-code WHERE sec-code.sec-code EQ acct.currency NO-LOCK) THEN 
              DO:
                 PUT UNFORMATTED " добавляем ЦБ " + acct.currency SKIP.
                 CREATE sec-code.
                 ASSIGN
                    sec-code.sec-code = acct.currency.
                 VALIDATE sec-code.
              END.
              
              mCreUpd = IF mCreAcct EQ YES THEN " создан." ELSE " обновлен.".
              PUT UNFORMATTED "Счет " + bank.accounts_change.account + mCreUpd SKIP.
              
              iCntLoaded = iCntLoaded + 1.
              
              /*IF acct.filial-id NE acctF THEN DO:
                ASSIGN
                  acct.filial-id = acctF.
                VALIDATE acct.
              END.*/
/*              PUT UNFORMATTED "Link acct-reserve" acct.acct SKIP.*/
              /* читаем линки по счету */
              IF CAN-DO("47423*,60312*,60323*,60406*,60408*,61011*", acct.acct) THEN 
              DO:
                 FIND FIRST links1
                    WHERE STRING(links1.linktype) MATCHES '84'
                    AND links1.account1 EQ acct.number NO-LOCK NO-ERROR.
                 IF AVAIL links1 THEN 
                 DO:
                    FIND FIRST acct2 WHERE
                       acct2.number EQ links1.account2 NO-LOCK NO-ERROR.
                 END. 
                 ELSE 
                 DO:
                    RELEASE acct2.
                 END.
                 IF AVAIL acct2 THEN 
                 DO:
                    mLinkId = GetXLinkID("acctb","acct-reserve").
                    FIND FIRST qbis.links
                       WHERE qbis.links.link-id EQ mLinkId
                       AND qbis.links.source-id EQ acct.acct + ',' + acct.currency
                       EXCLUSIVE-LOCK NO-ERROR.
                    IF NOT AVAIL qbis.links THEN 
                    DO:
                       CREATE qbis.links.
                       ASSIGN
                          qbis.links.link-id = mLinkId
                          qbis.links.source-id = acct.acct + ',' + acct.currency
                          qbis.links.target-id = acct2.acct + ',' + acct2.currency
                          qbis.links.beg-date  = acct.open-date
                          .
                       VALIDATE qbis.links.
                       PUT UNFORMATTED
                          "добавили связь acct-reserve " acct.acct " " acct2.acct SKIP.
                    END. 
                    ELSE 
                    DO:
                       ASSIGN
                          qbis.links.target-id = acct2.acct + ',' + acct2.currency
                          qbis.links.beg-date  = acct.open-date
                          .
                       VALIDATE qbis.links.
                       PUT UNFORMATTED
                          "обновили связь acct-reserve " acct.acct " " acct2.acct SKIP.
                    END.
                 END.
              END.
/*              PUT UNFORMATTED "Link acct47423" acct.acct SKIP.*/
              /* читаем линки по счету */
              IF      CAN-DO("47423........1*", acct.acct) 
              AND NOT CAN-DO("47423.....591*", acct.acct) THEN 
              DO:
                 FIND FIRST links1
                    WHERE STRING(links1.linktype) MATCHES '159'
                    AND links1.account2 EQ acct.number NO-LOCK NO-ERROR.
                 IF AVAIL links1 THEN 
                 DO:
                    FIND FIRST acct2 WHERE
                       acct2.number EQ links1.account1 NO-LOCK NO-ERROR.
                 END. 
                 ELSE 
                 DO:
                    RELEASE acct2.
                 END.
                 IF AVAIL acct2 THEN 
                 DO:
                    mLinkId = GetXLinkID("acctb","acct47423").
                    FIND FIRST qbis.links
                       WHERE qbis.links.link-id EQ mLinkId
                       AND qbis.links.target-id EQ acct.acct + ',' + acct.currency
                       EXCLUSIVE-LOCK NO-ERROR.
                    IF NOT AVAIL qbis.links THEN 
                    DO:
                       CREATE qbis.links.
                       ASSIGN
                          qbis.links.link-id = mLinkId
                          qbis.links.source-id = acct2.acct + ',' + acct2.currency
                          qbis.links.target-id = acct.acct  + ',' + acct.currency
                          qbis.links.beg-date  = acct.open-date
                          .
                       VALIDATE qbis.links.
                       PUT UNFORMATTED
                          "добавили связь acct47423    " acct2.acct " " acct.acct SKIP.
                    END. 
                    ELSE 
                    DO:
                       ASSIGN
                          qbis.links.source-id = acct2.acct + ',' + acct2.currency
                          qbis.links.beg-date  = acct.open-date
                          .
                       VALIDATE qbis.links.
                       PUT UNFORMATTED
                          "обновили связь acct47423    " acct2.acct " " acct.acct SKIP.
                    END.
                 END.
              END.
              /* читаем линки по расчетному счету */
              IF CAN-DO("405*,406*,407*,40802*,40817*,40820*,40821*,423*",acct.acct) THEN 
              DO:
                 FOR EACH links1 WHERE
                    STRING(links1.linktype) MATCHES '209'
                    AND links1.account1 EQ acct.number
                 NO-LOCK: 
                    FIND FIRST acct2 WHERE
                           acct2.number    EQ links1.account2
                       AND acct2.filial-id EQ "0500"
                    NO-LOCK NO-ERROR.
                    IF AVAIL(acct2) THEN 
                    DO:
                       mLinkId = GetXLinkID("acctb","ds-akc").
                       FIND FIRST qbis.links WHERE
                              qbis.links.link-id   EQ mLinkId
                          AND qbis.links.source-id EQ acct.acct + ',' + acct.currency
                       EXCLUSIVE-LOCK NO-ERROR.
                       IF NOT AVAIL qbis.links THEN 
                       DO:
      	                 CREATE qbis.links.
      	                 ASSIGN
      	                    qbis.links.link-id   = mLinkId
      	                    qbis.links.source-id = acct.acct + ',' + acct.currency
      	                    qbis.links.target-id = acct2.acct  + ',' + acct2.currency
      	                    qbis.links.beg-date  = acct2.open-date.
      	                 VALIDATE qbis.links.
                          PUT UNFORMATTED
                             "добавили связь ds-akc " acct.acct " " acct2.acct SKIP.
                       END. 
                       ELSE 
                       DO:
      	                 ASSIGN
      	                    qbis.links.target-id = acct2.acct + ',' + acct2.currency
      	                    qbis.links.beg-date  = acct2.open-date.
      	                 VALIDATE qbis.links.
                          PUT UNFORMATTED
                             "обновили связь ds-akc " acct.acct " " acct2.acct SKIP.
                       END.
                    END.
                 END.
              END.
              /* привязываем счет картотеки к расчетному счету */
              IF acct.contract EQ 'КартБл' THEN 
              DO:
                 FIND FIRST links1
                    WHERE STRING(links1.linktype) MATCHES '2'
                    AND links1.account2 EQ acct.number NO-LOCK NO-ERROR.
                    DEF BUFFER rs-acct FOR acct.
                 FIND FIRST rs-acct WHERE rs-acct.filial-id EQ acct.filial-id AND rs-acct.number = links1.account1 NO-LOCK NO-ERROR.
                 IF AVAIL links1 AND AVAIL rs-acct THEN
                    UpdateSigns( "acctb", rs-acct.acct + ',' + rs-acct.currency, "КартБВнСчет", acct.acct + ',' + acct.currency, ?).
                 RELEASE rs-acct.
                 RELEASE links1.
              END.
           END. /* IF CAN-DO("0500", acct-mfr.filial_id) AND */
           ELSE
           DO:
              PUT UNFORMATTED "Счет " + bank.accounts_change.account + " (" + mMFRFilial + ") не филиала 0500 или группа не 599 и не 596." SKIP.
           END.
        END. /* IF NOT AVAIL acct-mfr */
        
     END. /* IF LENGTH( accounts_change.account) EQ 20 AND */
     
     /*ASSIGN
          bank.accounts_change.status_ = 1.
          VALIDATE bank.accounts_change.*/
     DELETE bank.accounts_change.
  
     CATCH eAnyError AS Progress.Lang.Error:
       IF LENGTH( bank.accounts_change.account) EQ 20 AND
          INDEX( RETURN-VALUE + " " + eAnyError:GetMessage(1), 'filial') < 1 AND
          SUBSTR( bank.accounts_change.account, 1, 1) <> '0'
         THEN
   	 PUT UNFORMATTED 'счет ' bank.accounts_change.account ": " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
       ASSIGN
   	    bank.accounts_change.status_ = -1.
       VALIDATE bank.accounts_change.
     END CATCH.
  END. /*DO ON ERROR UNDO, THROW:*/
  iCnt = iCnt + 1.
  IF iCnt > 1000 THEN LEAVE.
  /*IF INTERVAL( NOW, vStartTime, 'seconds') > 45 THEN LEAVE.*/
END. /*FOR EACH bank.accounts_change*/
shFilial = shF.

IF iCnt GT 0
THEN
   PUT UNFORMATTED
      STRING(NOW,"99/99/9999 HH:MM:SS") +
      ' обработано счетов ' + STRING(iCnt) +
      ', загружено '        + STRING(iCntLoaded) + 
      ', время '            + STRING(etime(false)) 
      + ' мсек.' 
   SKIP.
ELSE
   PUT UNFORMATTED
      STRING(NOW,"99/99/9999 HH:MM:SS")
   SKIP.

/*{preview.i}*/
{intrface.del}
