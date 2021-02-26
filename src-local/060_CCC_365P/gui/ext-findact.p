/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-findact.p
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
DEF INPUT-OUTPUT PARAM TABLE FOR ttExtAcct.         /* Таблица по найденным счетам               */

DEF VAR vCust-id AS INT64 NO-UNDO.
DEF VAR vDetails AS CHAR  NO-UNDO.
DEFINE VARIABLE mMesErr AS CHARACTER NO-UNDO.

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
   ELSE 
   DO:
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
      then 
   DO:
      find first bal-acct where bal-acct.bal-acct EQ INT64(SUBSTRING(BalAc,1,5)) NO-LOCK NO-ERROR.
      if AVAIL bal-acct then rez = bal-acct.contract.
   END.
   IF BalAc BEGINS '90901' AND SUBSTR( BalAc, 14, 1) EQ '1' THEN rez = 'КартБл'.
   IF BalAc BEGINS '90901' AND SUBSTR( BalAc, 14, 1) EQ '0' THEN rez = 'Карт1'.
   RETURN rez.
END FUNCTION.
/*MESSAGE "ext-findact.p: shFilial = " shFilial*/
/*VIEW-AS ALERT-BOX.                           */
IF shFilial EQ '0000' OR shFilial EQ '0300' OR shFilial EQ '0500' THEN
DO ON ERROR UNDO, RETURN ERROR
ON STOP UNDO, LEAVE:

   FIND FIRST  acct WHERE
          acct.number    EQ DelFilFromAcct( iAcct)
      AND acct.filial-id EQ shFilial
   NO-LOCK NO-ERROR.

   IF NOT AVAIL(acct) THEN 
   DO:

      mMesErr = "".
      FOR EACH acct-mfr WHERE
             acct-mfr.acct EQ DelFilFromAcct(iAcct) 
         AND CAN-DO("0000,0300,0500",acct-mfr.filial_id)
      NO-LOCK:

/*MESSAGE "ext-findact.p: iAcct = " iAcct ";" acct-mfr.filial_id*/
/*VIEW-AS ALERT-BOX.                                            */
         
         FIND FIRST bal-acct WHERE 
            bal-acct.bal-acct EQ int64(substring(acct-mfr.acct,1,5))
         NO-LOCK NO-ERROR.
         FIND FIRST bank.clients-abs WHERE
            clients-abs.cid EQ acct-mfr.cid
         NO-LOCK NO-ERROR.

         vDetails = Details2BisStyle(acct-mfr.details).
         IF acct-mfr.acct begins '10207' OR acct-mfr.acct begins '40802' THEN 
         DO:
            vDetails = (IF AVAIL clients-abs THEN clients-abs.fname ELSE "").
         /* RELEASE clients-abs. */
         END.

         vCust-id = ?.
         /* ищем этого клиента в БИС */
         IF acct-mfr.cust_cat EQ 'Ч' THEN 
         DO:
            FIND FIRST signs WHERE
                   signs.file-name = 'person'
               AND signs.CODE EQ 'CID'
               AND signs.dec-value EQ acct-mfr.cid 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL(signs) THEN
               FIND FIRST signs
                  WHERE signs.file-name = 'person'
                  AND signs.CODE EQ 'CIDIP'
                  AND signs.dec-value EQ acct-mfr.cid
               NO-LOCK NO-ERROR.
            IF AVAIL(signs) THEN 
            DO:
               vCust-id = INTEGER(signs.surrogate).
               FIND FIRST person
                  WHERE person.person-id = vCust-id NO-LOCK.
               IF person.inn NE clients-abs.taxpayerno
                  THEN mMesErr = "Не совпадает ИНН клиент CID=" + STRING( acct-mfr.cid) + " с данными БИС по счету " + iAcct.
            END.   
         END.
         IF acct-mfr.cust_cat EQ 'Ю' THEN 
         DO:
            FIND FIRST signs
               WHERE signs.file-name = 'cust-corp'
               AND signs.CODE EQ 'CID' AND signs.dec-value EQ acct-mfr.cid NO-LOCK NO-ERROR.
            IF AVAIL signs
               THEN  
            DO:
               vCust-id = INTEGER(signs.surrogate).
               FIND FIRST cust-corp 
                  WHERE cust-corp.cust-id = vCust-id NO-LOCK.
               IF cust-corp.inn NE clients-abs.taxpayerno
                  THEN mMesErr = "Не совпадает ИНН клиент CID=" + STRING( acct-mfr.cid) + " с данными БИС по счету " + iAcct.
            END.  
         END.
         IF vCust-id EQ ? THEN mMesErr = "Не найден клиент CID=" + STRING( acct-mfr.cid).
/*MESSAGE "ext-findact.p: " mMesErr*/
/*VIEW-AS ALERT-BOX.               */
         IF     mMesErr  EQ "" 
            AND /*"0300" EQ acct-mfr.filial_id*/ CAN-DO("0000,0300,0500",acct-mfr.filial_id) THEN
         DO:
            
/*MESSAGE "ext-findact.p: " AddFilToAcct( acct-mfr.acct, acct-mfr.filial_id) ";" acct-mfr.currency*/
/*VIEW-AS ALERT-BOX.                                                                              */

/*            FIND FIRST ttExtAcct WHERE                                                    */
/*                    ttExtAcct.acct     EQ AddFilToAcct( acct-mfr.acct, acct-mfr.filial_id)*/
/*                AND ttExtAcct.currency EQ Curr2BisStyle(acct-mfr.acct, acct-mfr.currency) */
/*            NO-LOCK NO-ERROR.                                                             */
            FIND FIRST ttExtAcct WHERE
                    ttExtAcct.acct     EQ AddFilToAcct( acct-mfr.acct, acct-mfr.filial_id)
                AND ttExtAcct.currency EQ Curr2BisStyle(acct-mfr.acct, acct-mfr.currency)
            NO-LOCK NO-ERROR.
            IF NOT AVAIL(ttExtAcct) THEN
            DO:
               CREATE ttExtAcct.
               ASSIGN
                  ttExtAcct.bal-acct   = bal-acct.bal-acct
                  ttExtAcct.user-id    = "SYNC"
                  ttExtAcct.acct-cat   = acct-mfr.acct_cat
                  ttExtAcct.class-code = 'acct' + acct-mfr.acct_cat
                  ttExtAcct.filial-id  = acct-mfr.filial_id
                  ttExtAcct.acct       = AddFilToAcct( acct-mfr.acct, acct-mfr.filial_id)
                  ttExtAcct.number     = acct-mfr.acct
                  ttExtAcct.currency   = Curr2BisStyle(acct-mfr.acct, acct-mfr.currency)
                  ttExtAcct.contract   = Contract2BisStyle( acct-mfr.acctype, acct-mfr.acct)
                  ttExtAcct.details    = vDetails
                  ttExtAcct.rate-type  = (if ttExtAcct.currency = "" or bal-acct.bal-acct EQ 60313 or bal-acct.bal-acct EQ 60314 then "" else "Учетный")
                  ttExtAcct.side       = (IF CAN-DO("61209*,61210*,61212*", acct-mfr.acct) THEN 'АП' ELSE bal-acct.side)
                  ttExtAcct.cust-cat   = acct-mfr.cust_cat
                  ttExtAcct.open-date  = acct-mfr.open_date
                  ttExtAcct.close-date = acct-mfr.close_date
                  ttExtAcct.branch-id  = acct-mfr.filial_id
                  ttExtAcct.cust-id    = vCust-id.
/*MESSAGE "ext-findact.p: " ttExtAcct.acct*/
/*VIEW-AS ALERT-BOX.                      */
            END.
         END.
      END.
   END.
END.

/*For Each ttExtAcct No-Lock:                                                     */
/*   MESSAGE "ext-findact.p: For Each ttExtAcct: ttExtAcct.acct = " ttExtAcct.acct*/
/*   VIEW-AS ALERT-BOX.                                                           */
/*End.                                                                            */

CATCH eAnyError AS Progress.Lang.Error:
   MESSAGE RETURN-VALUE + " " + eAnyError:GetMessage(1)
   VIEW-AS ALERT-BOX.
   RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
END CATCH.

