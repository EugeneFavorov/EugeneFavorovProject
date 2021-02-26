/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-fillop.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Поиск счета во внешней системе.
     Modified: 
*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

DEF INPUT  PARAM iAcct       AS  CHAR   NO-UNDO.    /* Номер счета                               */
DEF INPUT  PARAM iAllFilials AS  LOG    NO-UNDO.    /* По всем филиалам или только текущий       */ 
DEF OUTPUT PARAM TABLE       FOR ttExtAcct.         /* Таблица по найденным счетам               */

DEF VAR vCust-id AS INT64 NO-UNDO.
DEF VAR vDetails AS CHAR  NO-UNDO.

/*
   Если счет найден в старой системе, ищем его владельца в БИС
   Определение cust-cat, cust-id:
   1. cust-cat = "Ч", если клиент находится в таблице person
      cust-cat = "Ю", если клиент находится в таблице cust-corp
      cust-cat = "Б", если клиент находится в таблице banks
      cust-cat = "" (пусто), если клиент из старой системы не найден в БИС

   2. cust-id = person.person-id/cust-corp.cust-id/banks.bank-id - соответвенно
      cust-id = -1, если клиент не найден

   3. Если найденно несколько счетов, то для каждого создать свою запись в таблице ttExtAcct
*/

FUNCTION Curr2BisStyle RETURNS CHARACTER (INPUT vAcct AS CHARACTER, INPUT iStr AS CHARACTER):
    DEF VAR cdval AS CHAR.
    cdval = "1*,2*,3*,4*,5*,6*,7*,8*,9*,0*".
    if iStr = '810' THEN iStr = " ".
    if (CAN-DO(cdval,iStr) OR iStr = '' OR iStr = ' ' OR CAN-DO( "98*",vAcct)) AND CAN-DO( "!706*,!70701*,!70706*,*", vAcct)
    THEN RETURN iStr.
    ELSE DO:
        iStr = " ".
        RETURN iStr.
    END.
END FUNCTION.

FUNCTION Details2BisStyle RETURNS CHARACTER (INPUT iStr AS CHARACTER):
    if iStr EQ '?' then iStr = ''.
    if length(iStr) EQ ? then iStr = ''.
    RETURN iStr.
END FUNCTION.

FUNCTION Contract2BisStyle RETURNS CHARACTER (INPUT vContract AS INTEGER, INPUT BalAc as CHAR):
    
  DEF VAR rez AS CHAR NO-UNDO.
  rez = ''.
  if vContract EQ 1      then rez = 'Расчет'.
  if vContract EQ 2      then rez = 'СпецТр'.
  if vContract EQ 3      then rez = 'Накоп'.
  if vContract EQ 101    then rez = 'Текущ'.
  if vContract EQ 201    then rez = 'Депоз'.
  if vContract EQ 203    then rez = 'НРФ'.
  if vContract EQ 204    then rez = 'Текущ'.
  if vContract EQ 211    then rez = 'Депоз'.
  if vContract EQ 231    then rez = 'Транз1'.
  if vContract EQ 232    then rez = 'Транз1'.
  if vContract EQ 241    then rez = 'Транз1'.
  if vContract EQ 271    then rez = 'Доход'.
  if vContract EQ 292    then rez = 'ЛороФП'.
  if vContract EQ 292 AND SUBSTR(BalAc,10,4) EQ '0110' then rez = 'НостФП'.

  if vContract EQ 301 AND NOT BalAc BEGINS '3200'
  then rez = 'Кредит'.
  if vContract EQ 251 AND NOT BalAc BEGINS '32502'
  then rez = 'КредПр'.

  if vContract EQ 401    then rez = 'Касса'.
  if vContract EQ 491    then rez = 'Ностро'.
  if vContract EQ 492    then rez = 'НостФА'.
  if vContract EQ 492 AND SUBSTR(BalAc,10,4) EQ '0530' then rez = 'ЛороФА'.

  if vContract EQ 501    then rez = 'Ностро'.
  if rez = ''
   then DO:
      find first bal-acct where bal-acct.bal-acct EQ INT64(SUBSTRING(BalAc,1,5)) NO-LOCK NO-ERROR.
      if AVAIL bal-acct then rez = bal-acct.contract.
  END.
  IF BalAc BEGINS '90901' AND SUBSTR( BalAc, 14, 1) EQ '1' THEN rez = 'КартБл'.
  IF BalAc BEGINS '90901' AND SUBSTR( BalAc, 14, 1) EQ '0' THEN rez = 'Карт1'.
  RETURN rez.
END FUNCTION.

IF shFilial EQ '0300' OR shFilial EQ '0500' THEN
DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, LEAVE:
  /* Находим запись в таблице acct, что бы не заполнять все поля вручную *
  FOR EACH acct 
    WHERE acct.number EQ DelFilFromAcct( iAcct)
    NO-LOCK:
     IF (NOT iAllFilials) THEN DO:
      IF (NUM-ENTRIES( iAcct, '@') > 1 AND acct.filial-id NE ENTRY( 2, iAcct, '@'))
       OR (NUM-ENTRIES( iAcct, '@') < 2 AND acct.filial-id NE shFilial)  
       THEN NEXT.
     END.
     * Создаем запись временную таблицу *
     CREATE ttExtAcct.

     * Копируем данные из таблицы *
     BUFFER-COPY acct TO ttExtAcct.

     * Ряд полей переопределим 
     ASSIGN
       ttExtAcct.cust-cat = "Ю" 
       ttExtAcct.cust-id  = 212  * Номер тестового клиента в базе *
       ttExtAcct.acct     = iAcct
       ttExtAcct.number   = iAcct

       ttExtAcct.open-date  = DATE("01.01.2000")
       ttExtAcct.close-date = DATE("01.11.2015")
     . *
     * Сохраняем изменения в таблицу *
     VALIDATE ttExtAcct NO-ERROR.
  END. 

  ELSE DO: IF AVAIL(acct) THEN  *
    * счет в БИС не нашли, ищем счет в АБС 

       как быть со счетами с кривым ключом ?

     */

  FIND FIRST  acct 
     WHERE acct.number EQ DelFilFromAcct( iAcct)
       AND acct.filial-id EQ shFilial NO-LOCK NO-ERROR.
  IF NOT AVAIL acct THEN DO:
    IF NOT CONNECTED("bank")
      THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
    FOR EACH acct-mfr
     WHERE acct-mfr.acct EQ DelFilFromAcct( iAcct) 
       AND CAN-DO( shFilial, acct-mfr.filial_id) NO-LOCK:

      FIND bal-acct WHERE bal-acct.bal-acct = int64(substring(acct-mfr.acct,1,5)).
      FIND FIRST bank.clients-abs WHERE clients-abs.cid = acct-mfr.cid NO-LOCK NO-ERROR.

      vDetails = Details2BisStyle(acct-mfr.details).
      IF acct-mfr.acct begins '10207' OR acct-mfr.acct begins '40802' THEN DO:
        vDetails = (IF AVAIL clients-abs THEN clients-abs.fname ELSE "").
        /* RELEASE clients-abs. */
      END.
      vCust-id = ?.
      /* ищем этого клиента в БИС */
      IF acct-mfr.cust_cat EQ 'Ч' THEN DO:
        FIND FIRST signs
         WHERE signs.file-name = 'person'
           AND signs.CODE EQ 'CID'
           AND signs.dec-value EQ acct-mfr.cid 
         NO-LOCK NO-ERROR.
        IF NOT AVAIL signs THEN
          FIND FIRST signs
           WHERE signs.file-name = 'person'
             AND signs.CODE EQ 'CIDIP'
             AND signs.dec-value EQ acct-mfr.cid
           NO-LOCK NO-ERROR.
        IF AVAIL signs 
         THEN DO:
          vCust-id = INTEGER(signs.surrogate).
          FIND FIRST person
           WHERE person.person-id = vCust-id NO-LOCK.
          IF person.inn NE clients-abs.taxpayerno
           THEN RETURN ERROR "Не совпадает ИНН клиент CID=" + STRING( acct-mfr.cid) + " с данными БИС по счету " + iAcct.
        END.   
      END.
      IF acct-mfr.cust_cat EQ 'Ю' THEN DO:
        FIND FIRST signs
           WHERE signs.file-name = 'cust-corp'
             AND signs.CODE EQ 'CID' AND signs.dec-value EQ acct-mfr.cid NO-LOCK NO-ERROR.
        IF AVAIL signs
         THEN DO:
          vCust-id = INTEGER(signs.surrogate).
          FIND FIRST cust-corp 
           WHERE cust-corp.cust-id = vCust-id NO-LOCK.
          IF cust-corp.inn NE clients-abs.taxpayerno
           THEN RETURN ERROR "Не совпадает ИНН клиент CID=" + STRING( acct-mfr.cid) + " с данными БИС по счету " + iAcct.
        END.  
      END.
      IF vCust-id EQ ? THEN RETURN ERROR "Не найден клиент CID=" + STRING( acct-mfr.cid).
      /* сверяем ИНН клиента в БИС и АБС */

      CREATE ttExtAcct.
      ASSIGN
        ttExtAcct.bal-acct   = bal-acct.bal-acct
        ttExtAcct.user-id    = "SYNC"
        ttExtAcct.acct-cat   = acct-mfr.acct_cat
        ttExtAcct.class-code = 'acct' + acct-mfr.acct_cat

        ttExtAcct.filial-id = acct-mfr.filial_id
        ttExtAcct.acct      = AddFilToAcct( acct-mfr.acct, acct-mfr.filial_id)
        ttExtAcct.number    = acct-mfr.acct
        ttExtAcct.currency  = Curr2BisStyle(acct-mfr.acct, acct-mfr.currency)
        ttExtAcct.contract  = Contract2BisStyle( acct-mfr.acctype, acct-mfr.acct)
        ttExtAcct.details   = vDetails
        ttExtAcct.rate-type = (if acct.currency = "" or acct.bal-acct EQ 60313 or acct.bal-acct EQ 60314 then "" else "Учетный")
        ttExtAcct.side      = (IF CAN-DO("61209*,61210*,61212*", acct-mfr.acct) THEN 'АП' ELSE bal-acct.side)
        ttExtAcct.cust-cat  = acct-mfr.cust_cat
        ttExtAcct.open-date = acct-mfr.open_date
        ttExtAcct.close-date = acct-mfr.close_date
        ttExtAcct.branch-id  = ttExtAcct.filial-id
        ttExtAcct.cust-id   = vCust-id
        .
    END.
  END.
END.
