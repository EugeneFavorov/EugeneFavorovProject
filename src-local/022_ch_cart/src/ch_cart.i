/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: ch_cart.i
      Comment: общесистемные функции работы с картотекой 2
   Parameters:
         Uses:
      Used by: g-trig.i, i-autosp.p,g_doloan.p,g-drow.p,g-intpay.p,g_crdep3.p,
               g-dcl1.p,g-cr.p,g-cr1.p,g-dcl.p,g-crlin.p,g-nachb_.p,g-call1.p,
               g-cash1.p,g-midl1.p,gprfclm.p,g-crd1.p,g-midlds.p,g-ent.i
      Created: 14/03/2000 Nata
     Modified: 14/03/2000 Nata
     Modified: 05/09/2000 Om    Ошибка: не обнулялась return-value.
     Modified: 25/02/2003 Илюха - добавлен входной параметр iOpDate, если
                                определен препроцессор ChkDate
                                - при передаче препроцессора USE_KAU, ищется
                                счет с шаблоном КАУ = "карт-ка2" и реквизиты
                                клиентов проверяемого счета и картотечного
                                совпадают
     Modified: 06/03/2003 kraw (0012006) Карт(x)ВНСчет может содержать валюту
     Modified: 01.06.2004 abko 0029139 поиск счета на карт2 с учетом подразделения
     Modified: 24/09/2004 ABKO (0035793) инструмент для поиска внебалансового счета
     Modified: 19.09.2005 19:28 KSV      (0046989) Оптимизирована работа     
                                         процедуры GetCar_2 за счет
                                         использования кэша.
     Modified: 03/04/2009 kraw (0100898) CheckCardA, GetCar_A
     Modified: 05/11/2013 sasa (0185495) Подъем заявки 0179426 (формирование резерва 
                                         по неоплаченным комиссиям 47423) в стандарт
*/

/* Возвращает стандартное сообщение для g-midl*.p при наличии картотеки 2 у
** клиента иначе возвращает ''.
** Зависит от НСИ/СтандТр/Карт2Влад/Without Checking проверяется
** картотека - если не установлен НСИ или проверяемый счет пользователя
** или его подчиненных

можно использовать с препроцессорами

&USE_KAU - клиент счета на картотеке должен совпадать с клиентом исходного
           счета, а так же назначение счета = "Карт2", шаблон кау = "Карт-ка2"
&ChkDate - добавляет в процедуру входной параметр дату, ищем счет
           открытый раньше этого дня
*/
{intrface.get cache}
{intrface.get crd}   /* Библиотека инструментов работы с картотекой */

DEFINE VAR vChkOwner AS LOGICAL         NO-UNDO.
DEFINE VAR vChkBrnch AS LOGICAL         NO-UNDO.
DEFINE VAR vBalKart2 AS LOGICAL         NO-UNDO.
   
ASSIGN
   vChkOwner = FGetSetting("СтандТр","Карт2Влад","Да") = "Да"
/* проверять принадлежность к одному подразделению бал. и карт. счетов */
   vChkBrnch = FGetSetting("СтандТр","Карт2Подр", "Нет") = "Да"
.

FIND bal-acct where
     bal-acct.acct-cat EQ "o" AND
     bal-acct.contract EQ "Карт2" NO-LOCK NO-ERROR.
vBalKart2 = AVAIL bal-acct.
RELEASE bal-acct.

PROCEDURE CheckCardA:

   DEFINE INPUT PARAM iAcct       AS CHAR NO-UNDO. /* Счет */
   DEFINE INPUT PARAM iCurrency   AS CHAR NO-UNDO. /* Валюта */
   DEFINE INPUT PARAM iCartType   AS CHAR NO-UNDO.
&IF DEFINED(ChkDate) NE 0 &THEN
   DEFINE INPUT PARAM iOpDate     AS DATE NO-UNDO. /*дата проводки*/
&ENDIF

   DEF VAR oRecid     AS RECID NO-UNDO. /*на внебалансовый счет*/
   DEF VAR oAmbig     AS LOG   NO-UNDO. /*много счетов*/

   RUN GetCar_A(iAcct,
                iCurrency,
                iCartType,
   &IF DEFINED(ChkDate)
   &THEN
                iOpDate,
   &ELSE
                end-date,
   &ENDIF
                OUTPUT oRecid,
                OUTPUT oAmbig).
   
    RETURN RETURN-VALUE.

END PROCEDURE.

PROCEDURE CheckCard2:

   DEFINE INPUT PARAM iAcct       AS CHAR NO-UNDO. /* Счет */
   DEFINE INPUT PARAM iCurrency   AS CHAR NO-UNDO. /* Валюта */
   &IF DEFINED(ChkDate)
   &THEN
   DEFINE INPUT PARAM iOpDate     AS DATE NO-UNDO. /*дата проводки*/
   &ENDIF

   RUN CheckCardA(iAcct,
                  iCurrency,
                  "Картотека2"
&IF DEFINED(ChkDate) NE 0 &THEN
                  , iOpDate
&ENDIF
                 ).
   
    RETURN RETURN-VALUE.

END PROCEDURE.
/****************************************
 Для балансового счета по acct и currency
 находит внебалансовый счет (возвращает его RECID)
 если у клиента более одного счета, то oAmbig = TRUE,
 а RECID - указатель на последний из счетов.
****************************************/
PROCEDURE GetCar_2:

   DEFINE INPUT  PARAM iAcct      AS CHAR NO-UNDO. /* Счет */
   DEFINE INPUT  PARAM iCurrency  AS CHAR NO-UNDO. /* Валюта */
   DEFINE INPUT  PARAM iOpDate    AS DATE NO-UNDO. /*дата проводки*/
   DEFINE OUTPUT PARAM oRecid     AS RECID NO-UNDO. /*на внебалансовый счет*/
   DEFINE OUTPUT PARAM oAmbig     AS LOG   NO-UNDO. /*Много счетов*/

   RUN GetCar_A(iAcct,
                iCurrency,
                "Картотека2",
                iOpDate,
                OUTPUT oRecid,
                OUTPUT oAmbig).
END PROCEDURE.

PROCEDURE GetCar_A:

   DEFINE INPUT  PARAM iAcct      AS CHAR NO-UNDO. /* Счет */
   DEFINE INPUT  PARAM iCurrency  AS CHAR NO-UNDO. /* Валюта */
   DEFINE INPUT  PARAM iCartType  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iOpDate    AS DATE NO-UNDO. /*дата проводки*/
   DEFINE OUTPUT PARAM oRecid     AS RECID NO-UNDO. /*на внебалансовый счет*/
   DEFINE OUTPUT PARAM oAmbig     AS LOG   NO-UNDO. /*Много счетов*/

   DEFINE VAR vOacct   AS CHARACTER  NO-UNDO.
   DEFINE VAR vOcurr   AS CHARACTER  NO-UNDO.
   DEFINE VAR vAcctCat AS CHARACTER  NO-UNDO.
   DEFINE VAR vResult  AS CHARACTER  NO-UNDO.
   DEFINE VAR vKauId   AS CHARACTER  NO-UNDO.
   DEFINE VAR vProc    AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vBalAcct AS INT64   NO-UNDO.
   DEFINE VARIABLE vBrnId   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKauIDM  AS CHARACTER NO-UNDO.

   DEFINE BUFFER oacct FOR acct.

   IF iCartType EQ "Картотека2" THEN
      ASSIGN
         vProc  = "GetCar_2"
         vKauId = "Карт-ка2"
         vBalAcct = 90902
      .
   ELSE
      ASSIGN
         vProc  = "GetCar_A"
         vKauId = "КартБлСч"   
         vBalAcct = 90901
      .
/*   IF GetFastCache(vProc,iAcct + iCurrency + {unknown STRING(iOpdate)},
                   OUTPUT vResult) THEN
   DO:
      ASSIGN
         oRecid  = INT64(ENTRY(1,vResult))
         oAmbig  = ENTRY(2,vResult) = "YES"
         vResult = ENTRY(3,vResult)
      .
      RETURN vResult.
   END.*/

   ASSIGN
      oRecid   = 0
      oAmbig   = FALSE
      vResult  = ""
   .
   
   IF iCurrency EQ ? THEN
      iCurrency = "".

   {find-act.i
      &acct = iAcct
      &curr = iCurrency
   }
   IF NOT AVAIL acct THEN RETURN.

   vBrnId = IF vChkBrnch THEN acct.branch-id
                         ELSE "*".

   FIND FIRST bal-acct WHERE bal-acct.bal-acct  EQ vBalAcct
                         AND bal-acct.kau-id    EQ vKauId
      NO-LOCK NO-ERROR.

   IF AVAILABLE bal-acct THEN
      vKauIDM = "*".
   ELSE
      vKauIDM = vKauID.

   vAcctCat = GetCodeMisc('ШаблКАУ',vKauId,8).
   IF CAN-DO(mAcctContCrd2,acct.contract) THEN
/*      (NOT vChkOwner            OR                                     */
/*       CAN-DO(USERID('bisquit')+ "," + GetSlaves(), acct.user-id)) THEN*/
   DO:
      /*  вначале поищем жестко связанный счет */
      vOacct = GetXattrValue("acct",
                             acct.acct + "," + acct.currency,
                             IF iCartType EQ "Картотека2" THEN "Карт2ВНСчет"
                                                          ELSE "КартБВНСчет").
      IF vOacct <> ? AND vOacct <> "" THEN
      DO:      

         ASSIGN            
            vOcurr = ENTRY(2,vOacct) WHEN NUM-ENTRIES(vOacct) GT 1
            vOacct = ENTRY(1,vOacct)            
         .

         FIND FIRST oacct where
                    oacct.acct      = vOacct
                AND oacct.currency  = vOcurr
                AND
                &IF DEFINED(USE_KAU) <> 0
                &THEN
                    ( oAcct.kau-id   EQ vKauId OR
                     (oAcct.Kau-id   NE vKauId AND
                      CAN-FIND(FIRST bal-acct WHERE
                                     bal-acct.bal-acct  = oAcct.bal-acct
                                 AND bal-acct.kau-id    = vKauId)
                    ))
                AND oacct.cust-cat   EQ acct.cust-cat
                AND oacct.cust-id    EQ acct.cust-id
                AND
                &ENDIF
                &IF DEFINED(ChkDate)
                &THEN
                    oacct.open-date  LE iOpDate
                AND
                &ENDIF
                    CAN-FIND(FIRST kau WHERE
                                   kau.acct     EQ oacct.acct
                               AND kau.currency EQ oacct.currency
                               AND NOT kau.zero-bal)
         NO-LOCK NO-ERROR.
      END.
      ELSE
      DO:

         IF iCartType EQ "Картотека2" THEN
            FIND oacct USE-INDEX acct-cust
                 WHERE oacct.bal-acct   EQ vBalAcct
                   AND oacct.contract   EQ 'Карт2'
                   AND oacct.cust-cat   EQ acct.cust-cat
                   AND oacct.cust-id    EQ acct.cust-id
                   AND oacct.acct-cat   EQ vAcctCat
                   AND oacct.close-date EQ ?
                   AND oacct.currency   EQ acct.currency
                   AND oacct.filial-id  EQ ShFilial
                   AND
                   &IF DEFINED(USE_KAU) <> 0
                   &THEN
                       CAN-DO(vKauIDM, oAcct.kau-id)
                   AND
                   &ENDIF
                   &IF DEFINED(ChkDate)
                   &THEN
                       oacct.open-date LE iOpDate
                   AND
                   &ENDIF
                       CAN-FIND(FIRST kau WHERE
                                      kau.acct     EQ oacct.acct
                                  AND kau.currency EQ oacct.currency
                                  AND NOT kau.zero-bal)
                   AND CAN-DO(vBrnId, oacct.branch-id)
            NO-LOCK NO-ERROR.
         ELSE
         DO:

            FIND FIRST oAcct USE-INDEX acct-cust WHERE 
                       oacct.bal-acct   EQ vBalAcct
                   AND oacct.cust-cat   EQ acct.cust-cat
                   AND oacct.cust-id    EQ acct.cust-id
                   AND oacct.acct-cat   EQ vAcctCat
                   AND oacct.close-date EQ ?
                   AND oacct.currency   EQ acct.currency
                   AND oacct.filial-id  EQ ShFilial
                   &IF DEFINED(ChkDate)
                   &THEN
                   AND oacct.open-date  LE iOpDate
                   &ENDIF
                   AND CAN-DO(vKauIDM, oAcct.kau-id)
                   AND CAN-FIND(FIRST kau WHERE
                                      kau.acct     EQ oacct.acct
                                  AND kau.currency EQ oacct.currency
                                  AND NOT kau.zero-bal)
                   AND CAN-DO(vBrnId, oacct.branch-id)
                     NO-LOCK NO-ERROR.
         END.
      END.

      IF AVAIL oacct THEN
         oRecid = RECID(oacct).
      IF AMBIGUOUS oacct THEN
      DO:
         FIND LAST oacct USE-INDEX acct-cust
              WHERE oacct.contract   EQ 'Карт2'
                AND oacct.bal-acct   EQ vBalAcct
                AND oacct.cust-cat   EQ acct.cust-cat
                AND oacct.cust-id    EQ acct.cust-id
                AND oacct.acct-cat   EQ vAcctCat
                AND oacct.close-date EQ ?
                AND oacct.currency   EQ acct.currency
                AND oacct.filial-id  EQ ShFilial
                AND
                &IF DEFINED(USE_KAU) <> 0
                &THEN
                    CAN-DO(vKauIDM, oAcct.kau-id)
                AND
                &ENDIF
                &IF DEFINED(ChkDate)
                &THEN
                    oacct.open-date LE iOpDate
                AND
                &ENDIF
                    CAN-FIND(FIRST kau WHERE
                                   kau.acct     EQ oacct.acct
                               AND kau.currency EQ oacct.currency
                               AND NOT kau.zero-bal)
                AND CAN-DO(vBrnId, oacct.branch-id)
         NO-LOCK NO-ERROR.
         ASSIGN
            oAmbig = TRUE
            oRecid = RECID(oacct)
         .
      END.

      IF AVAIL oacct THEN
      DO:
         vResult = 'У  клиента счета ' + iAcct + '/' + iCurrency +
                   ' есть картотека'.

         IF iCartType EQ "КБС" THEN
            vResult = vResult + " блокированных счетов!".

         ELSE IF iCartType EQ "Картотека2" THEN
            vResult = vResult + " 2!".
         ELSE
            vResult = vResult + "!".
      END.
/*     vResult = "".*/
   END.

/*   RUN SetFastCache IN h_cache (vProc,
                                iAcct + iCurrency + {unknown STRING(iOpdate)},
                                STRING(oRecid) + "," + STRING(oAmbig) + "," + 
                                vResult).*/

   RETURN vResult.   /* Обнуляет содержимое return-value */

END PROCEDURE.

/* Определяет максимальную и минимальную очередность платежа документов на картотеке по клиенту 
   (анализируются все счета клиента) */
PROCEDURE GetCartOrdPay:
   DEFINE INPUT  PARAM iAcct      AS CHAR NO-UNDO. /* Счет */
   DEFINE INPUT  PARAM iCurrency  AS CHAR NO-UNDO. /* Валюта */
   DEFINE INPUT  PARAM iCartType  AS CHAR NO-UNDO. /* Тип картотеки */
   DEFINE INPUT  PARAM iOpDate    AS DATE NO-UNDO. /* Дата проводки*/
   DEFINE OUTPUT PARAM oOrdPayH   AS CHAR NO-UNDO. /* Самая высокая очередность док-та на картотеке */
   DEFINE OUTPUT PARAM oOrdPayL   AS CHAR NO-UNDO. /* Самая низкая очередность док-та на картотеке */

   DEFINE VAR vAcctCat AS CHARACTER  NO-UNDO.
   DEFINE VAR vKauId   AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vBalAcct AS INT64   NO-UNDO.
   DEFINE VARIABLE vBrnId   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKauIDM  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmp     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOrdPay  AS INT64   NO-UNDO.

   DEFINE BUFFER oacct FOR acct.
   DEFINE BUFFER bop   FOR op.

   IF iCartType EQ "Картотека2" THEN
      ASSIGN
         vKauId = "Карт-ка2"
         vBalAcct = 90902
      .
   ELSE
      ASSIGN
         vKauId = "КартБлСч"   
         vBalAcct = 90901
      .

   ASSIGN
      oOrdPayH = "99"
      oOrdPayL = "0"
   .
   
   IF iCurrency EQ ? THEN
      iCurrency = "".

   {find-act.i
      &acct = iAcct
      &curr = iCurrency
   }

   IF NOT AVAIL acct THEN RETURN.

   vBrnId = IF vChkBrnch THEN acct.branch-id
                         ELSE "*".

   FIND FIRST bal-acct WHERE bal-acct.bal-acct  EQ vBalAcct
                         AND bal-acct.kau-id    EQ vKauId
      NO-LOCK NO-ERROR.

   IF AVAILABLE bal-acct THEN
      vKauIDM = "*".
   ELSE
      vKauIDM = vKauID.

   vAcctCat = GetCodeMisc('ШаблКАУ',vKauId,8).

   FOR EACH   oacct USE-INDEX acct-cust                             
        WHERE oacct.bal-acct   EQ vBalAcct                          
          AND ((iCartType      EQ "Картотека2"                             
          AND oacct.contract   EQ 'Карт2')
           OR iCartType        NE "Картотека2")                                                    
          AND oacct.cust-cat   EQ acct.cust-cat                     
          AND oacct.cust-id    EQ acct.cust-id                      
          AND oacct.acct-cat   EQ vAcctCat                          
          AND oacct.close-date EQ ?                                 
          AND oacct.currency   EQ acct.currency                     
          AND oacct.filial-id  EQ ShFilial                          
          AND CAN-DO(vKauIDM, oAcct.kau-id)                         
          AND oacct.open-date LE iOpDate                            
          AND CAN-DO(vBrnId, oacct.branch-id)                       
   NO-LOCK,                                                         
       EACH kau                                                     
        WHERE kau.acct     EQ oacct.acct                            
          AND kau.currency EQ oacct.currency                        
          AND NOT kau.zero-bal                                      
   NO-LOCK,                                                         
       FIRST op WHERE op.op EQ INT64(entry(1,kau.kau)) NO-LOCK: 
    
      vOrdPay = -1.  

      /* Ищем балансовый док-т, берем очередность */                                               
      vTmp = GetXAttrValueEx("op",string(op.op),"op-bal","").       
      IF vTmp ne "" THEN DO:                                        
         FOR FIRST bop WHERE bop.op EQ INT64(vTmp) NO-LOCK:         
            IF bop.order-pay <> ? AND bop.order-pay <> "" THEN      
               vOrdPay = INT64(bop.order-pay) NO-ERROR.             
            IF ERROR-STATUS:ERROR THEN vOrdPay = -1.                
         END.                                                       
      END.   
      /* Если не получилось, ищем внебалансовый док-т, берем очередность */                                                                                                      
      IF vOrdPay < 0 THEN DO:                                       
         IF op.order-pay <> ? AND op.order-pay <> "" THEN           
            vOrdPay = INT64(op.order-pay) NO-ERROR.                 
         IF ERROR-STATUS:ERROR THEN vOrdPay = -1.                   
      END.         
                                                 
      IF vOrdPay < 0 THEN NEXT.                                     
      IF vOrdPay < INT64(oOrdPayH) THEN oOrdPayH = STRING(vOrdPay). 
      IF vOrdPay > INT64(oOrdPayL) THEN oOrdPayL = STRING(vOrdPay). 
   END.             
   IF oOrdPayH EQ "99" THEN oOrdPayH = "0".                                             
                                                                       
   RETURN.
END PROCEDURE.

/* Определяет наличие К2 по клиенту */
PROCEDURE CliChkCart2:
   DEFINE INPUT PARAMETER  iCustCat LIKE acct.cust-cat NO-UNDO.
   DEFINE INPUT PARAMETER  iCustID  LIKE acct.cust-id  NO-UNDO.
   DEFINE INPUT PARAMETER  iDate    AS   DATE          NO-UNDO.
   DEFINE OUTPUT PARAMETER oRes     AS   LOGICAL       NO-UNDO INIT no.

   DEFINE VARIABLE mUsrMsk AS CHAR NO-UNDO.
   DEFINE BUFFER acct FOR acct.

   IF iCustCat EQ "В" THEN
      RETURN.

   IF iDate = ? THEN iDate = TODAY.
   mUsrMsk = USERID('bisquit')+ "," + GetSlaves().
   FOR EACH acct WHERE acct.cust-cat = iCustCat
                   AND acct.cust-id  = iCustId
                   AND CAN-DO(mAcctContCrd2,acct.contract) 
                   AND (NOT vChkOwner OR CAN-DO(mUsrMsk, acct.user-id)) 
   NO-LOCK:
      RUN CheckCard2(acct.acct,acct.currency,iDate). 
      oRes = RETURN-VALUE <> "".  
      IF oRes THEN LEAVE.                                                                           
   END.

END PROCEDURE.
/* $LINTUSER='MUTA' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='*' */
/* $LINTDATE='07/09/2014 18:53:37.932+04:00' */
/* $LINTFILE='ch_cart.i' */
/*prosign96CZbv6uPMTVNWmpEDMrfQ*/