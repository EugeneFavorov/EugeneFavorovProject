/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: leg407pr1.fun
      Comment: Функции для работы с классом leg407pr1
   Parameters:
         Uses:
      Used by:
      Created: anba
     Modified: anba         
*/
{globals.i}
{intrface.get xclass}
{intrface.get strng}
{intrface.get cust}
{intrface.get tmess}
{intrface.get db2l}
{intrface.get date}
{intrface.get lgadr}
{intrface.get lic}      /* Библиотека для работы с лицензиями. */

DEFINE TEMP-TABLE tt-code NO-UNDO
   FIELD fcode AS INT64.

FUNCTION f-SD         CHARACTER (i-Document    AS CHARACTER,         /* серия */
                                 i-document-id AS CHARACTER):
   DEFINE VARIABLE vTmp AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE i    AS INT64     NO-UNDO.
   DO WHILE INDEX(i-Document, "  ") NE 0:
      i-Document = REPLACE(i-Document, "  ", " ").
   END.
   IF NUM-ENTRIES(i-Document, " ") LE 1 THEN RETURN "0".
   CASE i-document-id:
      WHEN "Документ"     OR 
      WHEN "ВоенныйБилет" OR 
      WHEN "ЗагрПаспорт"  OR 
      WHEN "А_права"         THEN vTmp = GetEntries(1, i-Document, " ", "").

      WHEN "Паспорт"         THEN vTmp = GetEntries(1, i-Document, " ", "") + " " 
                                                       + GetEntries(2, i-Document, " ", "").
      OTHERWISE
      DO:
         IF NUM-ENTRIES(i-Document, " ") GT 1 THEN /* взять ВСЕ элементы КРОМЕ последнего */
         DO i = 1 TO NUM-ENTRIES(i-Document, " ") - 1 :
            vTmp = vTmp + (IF vTmp NE "" THEN " " ELSE "") + GetEntries(i, i-Document, " ", "") .
         END.
      END.
   END CASE.

   RETURN vTmp. 
END FUNCTION.                    

FUNCTION f-VD_1       CHARACTER (i-Document    AS CHARACTER,  /* номер док-та */
                                 i-document-id AS CHARACTER):
DEFINE VARIABLE vTmp AS CHARACTER   NO-UNDO.

   DO WHILE INDEX(i-Document, "  ") NE 0:
      i-Document = REPLACE(i-Document, "  ", " ").
   END.
   IF NUM-ENTRIES(i-Document, " ") LE 1 THEN vTmp = i-Document.
   ELSE
      CASE i-document-id:
         WHEN "Документ"     OR 
         WHEN "ВоенныйБилет"    THEN vTmp = SUBSTRING(GetEntries(2, i-Document, " ", ""), 1, 6).
         WHEN "Паспорт"         THEN vTmp = SUBSTRING(GetEntries(3, i-Document, " ", ""), 1, 6).
         WHEN "А_права"         THEN vTmp = GetEntries(2, i-Document, " ", "").
         WHEN "ЗагрПаспорт"     THEN vTmp = SUBSTRING(GetEntries(2, i-Document, " ", ""), 1, 7).
         OTHERWISE /* взять ПОСЛЕДНИЙ элемент из значения реквизита ╓document· */
            IF NUM-ENTRIES(i-Document, " ") GT 1 THEN
               vTmp = IF NUM-ENTRIES(i-Document, " ") GT 1 
                      THEN GetEntries(NUM-ENTRIES(i-Document, " "), i-Document, " ", "") 
                      ELSE i-Document. /* если в ╓document· один элемент, то считать его номером документа */
      END CASE.
   RETURN vTmp. 
END FUNCTION.   

FUNCTION GetAdrr CHARACTER (iCustCat AS CHARACTER,
                            iCustId  AS INT64,
                            iType    AS INT64):

   DEFINE VARIABLE vTypeAdr     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStr         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRezultStr   AS CHARACTER NO-UNDO.
   DEFINE BUFFER   cust-ident   FOR cust-ident.
   DEFINE BUFFER   country      FOR country.

   IF iType EQ 1 
   THEN
      vTypeAdr = IF iCustCat EQ "Ч" THEN "АдрПроп" ELSE  "АдрЮр".
   ELSE
      vTypeAdr = "АдрФакт".

   FIND FIRST cust-ident WHERE
              cust-ident.Class-code     EQ "p-cust-adr"
          AND cust-ident.cust-cat       EQ iCustCat
          AND cust-ident.cust-id        EQ INT64(iCustID)
          AND (   cust-ident.close-date >= TODAY
               OR cust-ident.close-date EQ ? )
          AND cust-ident.cust-code-type EQ vTypeAdr 
   NO-LOCK NO-ERROR.
   vRezultStr = "".
   IF AVAIL cust-ident THEN
   DO:
      vStr = GetXattrValueEx("cust-ident",
                             STRING(cust-ident.cust-code-type) + ',' +  
                             STRING(cust-ident.cust-code) + ',' + 
                             STRING(cust-ident.cust-type-num),
                             "country-id","").
      
      FIND FIRST country WHERE country.country-id EQ  vStr NO-LOCK NO-ERROR.
   
      vRezultStr = ( IF AVAIL country THEN STRING(country.country-alt-id) ELSE "") + "," +
                     ENTRY(1, cust-ident.issue) + ","  +
                     LEFT-TRIM(GetXattrValueEx("cust-ident",
                     STRING(cust-ident.cust-code-type) + ',' +
                     STRING(cust-ident.cust-code) + ',' + 
                     STRING(cust-ident.cust-type-num),
                     "КодРег",""),"0") + 
                     REPLACE(cust-ident.issue,(IF LENGTH(ENTRY(1,cust-ident.issue)) EQ 0 THEN " " ELSE ENTRY(1,cust-ident.issue)),"").
   END.
   RETURN vRezultStr.
END FUNCTION .

FUNCTION GetAdrrGni CHARACTER (iCustCat AS CHARACTER,
                            iCustId  AS INT64,
                            iType    AS INT64):

   DEFINE VARIABLE vTypeAdr     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStr         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRezultStr   AS CHARACTER NO-UNDO.
   DEFINE BUFFER   cust-ident   FOR cust-ident.
   DEFINE BUFFER   country      FOR country.

   IF iType EQ 1 
   THEN
      vTypeAdr = IF iCustCat EQ "Ч" THEN "АдрПроп" ELSE  "АдрЮр".
   ELSE
      vTypeAdr = "АдрФакт".

   FIND FIRST cust-ident WHERE
              cust-ident.Class-code     EQ "p-cust-adr"
          AND cust-ident.cust-cat       EQ iCustCat
          AND cust-ident.cust-id        EQ INT64(iCustID)
          AND (   cust-ident.close-date >= TODAY
               OR cust-ident.close-date EQ ? )
          AND cust-ident.cust-code-type EQ vTypeAdr 
   NO-LOCK NO-ERROR.
   vRezultStr = "".
   IF AVAIL cust-ident THEN
   DO:
      vStr = GetXattrValueEx("cust-ident",
                             STRING(cust-ident.cust-code-type) + ',' +  
                             STRING(cust-ident.cust-code) + ',' + 
                             STRING(cust-ident.cust-type-num),
                             "country-id","").
      
      FIND FIRST country WHERE country.country-id EQ  vStr NO-LOCK NO-ERROR.
   
      vRezultStr = ( IF AVAIL country THEN STRING(country.country-alt-id) ELSE "") + "," +
                     ENTRY(1, cust-ident.issue) + ","  +
                     LEFT-TRIM(GetXattrValueEx("cust-ident",
                     STRING(cust-ident.cust-code-type) + ',' +
                     STRING(cust-ident.cust-code) + ',' + 
                     STRING(cust-ident.cust-type-num),
                     "КодРегГНИ",""),"0") + 
                     REPLACE(cust-ident.issue,(IF LENGTH(ENTRY(1,cust-ident.issue)) EQ 0 THEN " " ELSE ENTRY(1,cust-ident.issue)),"").
   END.
   RETURN vRezultStr.
END FUNCTION .

/*----------------------------------------------------------------------------*/
/*Возврящает любой основной реквизит клиента                                  */
/*----------------------------------------------------------------------------*/
FUNCTION ClientBaseValue RETURN CHARACTER (iCustCat AS CHARACTER,
                                           iCustId  AS INT64,
                                           iField   AS CHARACTER):

   DEFINE VARIABLE vWhere    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vString   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTable    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vQuery    AS HANDLE    NO-UNDO.
   DEFINE BUFFER   cust-corp FOR cust-corp.
   DEFINE BUFFER   person    FOR person.
   DEFINE BUFFER   banks     FOR banks.

   CASE iCustCat:
      WHEN "Ю" THEN
      ASSIGN
         vTable  =  BUFFER cust-corp:HANDLE
         vWhere  = "FOR EACH cust-corp WHERE cust-corp.cust-id EQ ".
      WHEN "Ч" THEN
      ASSIGN
         vTable  = BUFFER person:HANDLE 
         vWhere  = "FOR EACH person WHERE person.person-id EQ ".
      WHEN "Б" THEN
      ASSIGN
         vTable  =  BUFFER banks:HANDLE
         vWhere  = "FOR EACH banks WHERE banks.bank-id EQ ".
      OTHERWISE 
         RETURN "".
   END CASE.

   CREATE QUERY vQuery.
   vQuery:SET-BUFFERS(vTable).
   vQuery:QUERY-PREPARE(vWhere + STRING(iCustId) + " NO-LOCK ").
   vQuery:QUERY-OPEN.
   vQuery:GET-FIRST().

   FND-BlOCK:
   REPEAT WHILE NOT vQuery:QUERY-OFF-END
   TRANSACTION
   ON ERROR UNDO FND-BlOCK, RETRY FND-BlOCK:
      IF RETRY THEN
      DO:
         LEAVE FND-BlOCK.
      END.

      vString = STRING(vTable:BUFFER-FIELD(iField):BUFFER-VALUE) NO-ERROR.
         
      IF NOT ERROR-STATUS:ERROR THEN
      vQuery:GET-NEXT().
   END.


   /* закрываем и удаляем выборку (если нужно) */
   IF vQuery:IS-OPEN THEN
      vQuery:QUERY-CLOSE().
   IF VALID-HANDLE (vQuery) THEN
      DELETE OBJECT vQuery.

   RETURN vString.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/*   Возвращает значения атрибутов клиента                                    */
/*----------------------------------------------------------------------------*/
FUNCTION GetChckAttrByClnt RETURN CHARACTER  (INPUT ipCustCat AS CHARACTER,
                                              INPUT ipCustID  AS INT64,
                                              INPUT ipRqst    AS CHARACTER):

   DEFINE VARIABLE vPredp     AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vFLP       AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vISPredp   AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vI         AS INT64      NO-UNDO.
   DEFINE VARIABLE vStr       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKodMK     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vName      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDoc       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDocId     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vFDate     AS DATE       NO-UNDO.
   DEFINE VARIABLE vCode      AS CHARACTER  NO-UNDO.

   DEFINE BUFFER cust-ident FOR cust-ident.
   IF NOT CAN-DO("Ю,Ч,Б",ipCustCat) THEN 
      RETURN "".

   vPredp   =  ClientXattrVal (ipCustCat,ipCustID,"Предпр")  GT "".
   vFLP     =  ClientXattrVal (ipCustCat,ipCustID,"Субъект") EQ "ФЛП".
   vISPredp =  vPredp OR vFLP.

   CASE ipRqst:
        WHEN "ТипКлиент"   THEN  
        DO:
           CASE  ipCustCat: 
              WHEN  "Ч"  THEN
                 vStr = IF vPredp OR vFLP  THEN "3" ELSE "2".
              WHEN  "Ю"  THEN
                 vStr = IF vPredp OR vFLP THEN "3" ELSE "1".
              WHEN  "Б"  THEN
                 vStr =  "1".
           END CASE.

           RETURN vStr.
        END.

        WHEN "ПризнРез"  OR WHEN "ДокУдЛичн"  THEN 
        DO:  
           RETURN IF ClientBaseValue(ipCustCat,ipCustID, "country-id") EQ "RUS" THEN "1" ELSE  "2". 
        END.

        WHEN "Фам"         THEN 
        DO:
           IF    ipCustCat EQ "Ю" 
             AND (vPredp OR vFLP) THEN
           DO:    
              vStr = ClientBaseValue(ipCustCat,ipCustID, "name-corp").
              vName = "".
              IF NUM-ENTRIES(vStr, " ") GT 3 THEN
              DO vI = 1 TO NUM-ENTRIES(vStr, " ") - 2:
                  
                 vName = vName + " " + ENTRY(vI, vStr, " ").
              END .
              
              ELSE IF NUM-ENTRIES(vStr, " ") EQ 2 THEN 
                 vName = ENTRY(1, vStr, " ").
              ELSE 
                 vName = vStr. 
              RETURN vName. 
           END.
           IF ipCustCat EQ "Ч" THEN
              RETURN ClientBaseValue(ipCustCat,ipCustID, "name-last").
        END.
          
        WHEN "Имя"         THEN 
        DO:    
           IF    ipCustCat EQ "Ю" 
             AND (vPredp OR vFLP) THEN
           DO:    
              vStr = ClientBaseValue(ipCustCat,ipCustID, "name-corp").
              IF NUM-ENTRIES(vStr, " ") GT 2 
              THEN 
                 RETURN ENTRY(NUM-ENTRIES(vStr, " ") - 1,vStr," "). 
              IF NUM-ENTRIES(vStr, " ") EQ 2 
              THEN 
                 RETURN ENTRY(2,vStr," ").
           END.
           IF ipCustCat EQ "Ч" THEN
           DO:   
              vStr = ClientBaseValue(ipCustCat,ipCustID, "first-names").
              IF NUM-ENTRIES(vStr, " ") GE 2 THEN
                 RETURN ENTRY(1,vStr," ").
              ELSE
                 RETURN vStr.
           END.
        END. 
        WHEN "Отч"         THEN  
        DO:
           IF    ipCustCat EQ "Ю"
             AND (vPredp OR vFLP)   THEN
           DO:    
              vStr = ClientBaseValue(ipCustCat,ipCustID, "name-corp").
              IF NUM-ENTRIES(vStr, " ") GE 3 
              THEN 
                 RETURN ENTRY(NUM-ENTRIES(vStr, " "),vStr, " ") .
           END.
           IF ipCustCat EQ "Ч" THEN
           DO:   
              vStr = ClientBaseValue(ipCustCat,ipCustID, "first-names").
              IF NUM-ENTRIES(vStr, " ") GE 2 THEN
              RETURN TRIM(SUBSTRING(vStr,INDEX(vStr," "))).
           END.                         
        END.   

        WHEN "НаимЮЛ"      THEN
        DO:
           IF ipCustCat EQ "Ю" THEN
              RETURN  TRIM(   ClientBaseValue(ipCustCat,ipCustID, "cust-stat")
                      + " " + ClientBaseValue(ipCustCat,ipCustID, "name-corp")).
           ELSE IF ipCustCat EQ "Б" THEN
              RETURN ClientBaseValue(ipCustCat,ipCustID, "name"). 
          
        END.
        WHEN "НаимЮЛСокр"      THEN
        DO:
           IF ipCustCat EQ "Ю" THEN
              RETURN ClientBaseValue(ipCustCat,ipCustID, "name-short").
           ELSE IF ipCustCat EQ "Б" THEN
              RETURN ClientBaseValue(ipCustCat,ipCustID, "short-name"). 
        END.
        WHEN "НаимЮЛИностр"      THEN
        DO:
           IF ipCustCat EQ "Ю" THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID,"engl-name").
           ELSE IF ipCustCat EQ "Б" THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID,"engl-name").
        END.
        WHEN "ИНН"         THEN 
        DO:  
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              RETURN ClientBaseValue(ipCustCat,ipCustID, "inn").
           ELSE IF ipCustCat EQ "Б" THEN
           DO:
              FIND FIRST banks WHERE banks.bank-id EQ ipCustID NO-LOCK NO-ERROR.
              IF AVAIL(banks) THEN
              DO:
                 FIND FIRST banks-code WHERE banks-code.bank-id EQ banks.bank-id AND 
                                             banks-code.bank-code-type EQ "ИНН" NO-LOCK NO-ERROR.
                 IF AVAIL(banks-code) THEN RETURN banks-code.bank-code.
              END.
           END.
           ELSE 
           DO:
              FIND FIRST cust-ident   WHERE 
                         cust-ident.cust-cat        EQ "Б"
                    AND  cust-ident.cust-id         EQ ipCustID
                    AND  cust-ident.cust-code-type  EQ "ИНН"
              NO-LOCK NO-ERROR.
              IF AVAIL cust-ident  THEN
                 RETURN  cust-ident.cust-code.
           END.
        END.
        WHEN "ОГРН"        THEN 
        DO:  
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID, "ОГРН").
        END.
        WHEN "ДатаРег"     THEN 
        DO:
           IF CAN-DO("Ю,Б",ipCustCat)  THEN
           DO:
              vStr = ClientXattrVal(ipCustCat,ipCustID, "RegDate").
              IF vStr EQ "" OR DATE(vStr) EQ ? THEN
                 RETURN "".
              ELSE
                 RETURN STRING(DATE(vStr),"99/99/9999").
           END.
           ELSE 
           DO:
              vStr = ClientXattrVal(ipCustCat,ipCustID, "ДатаВПред").
              IF vStr EQ "" OR DATE(vStr) EQ ? THEN
                 RETURN "".
              ELSE
                 RETURN STRING(DATE(vStr),"99/99/9999").
           END.
        END.
        WHEN "РегОрган"     THEN 
        DO:
           IF ipCustCat EQ "Ю"  THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID, "РегОрган").
           ELSE 
              RETURN "".
        END.
        WHEN "ОПФорма"     THEN 
        DO:
           IF ipCustCat EQ "Ю"  THEN
              vStr = ClientBaseValue(ipCustCat,ipCustID, "cust-stat").
           ELSE IF ipCustCat EQ "Б"  THEN
              vStr = ClientXattrVal(ipCustCat,ipCustID, "bank-stat").
           ELSE 
              vStr = "".
           vCode = GetCodeVal("КодПредп",vStr).
           RETURN GetCodeName("КодПредп",vCode).
        END.
        WHEN "ОргУправ"     THEN 
        DO:
           IF CAN-DO("Ю,Б",ipCustCat)  THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID, "СтруктОрг") + " " + 
                     ClientXattrVal(ipCustCat,ipCustID, "ОргУправ").
           ELSE
              RETURN "".
        END.
        WHEN "УставКап"     THEN 
        DO:
           IF CAN-DO("Ю,Б",ipCustCat)  THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID, "УставКап").
           ELSE
              RETURN "".
        END.
        WHEN "Страна"     THEN 
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              vStr = ClientBaseValue(ipCustCat,ipCustID, "country-id").
           ELSE
              vStr =  "".
           FIND FIRST country WHERE country.country-id EQ vStr NO-LOCK NO-ERROR.
           IF AVAIL(country) THEN
              RETURN STRING(country.country-alt-id,"999").
        END.
        WHEN "Гражд"     THEN 
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              vStr = ClientXattrVal(ipCustCat,ipCustID, "country-id2").
           ELSE
              vStr =  "".
           FIND FIRST country WHERE country.country-id EQ vStr NO-LOCK NO-ERROR.
           IF AVAIL(country) THEN
              RETURN STRING(country.country-alt-id,"999").
        END.
        WHEN "ПрисутАдр"     THEN 
        DO:
           IF CAN-DO("Ю,Б",ipCustCat)  THEN
              vStr = ClientXattrVal(ipCustCat,ipCustID, "ПрисутОргУправ").
           IF vStr EQ "" THEN RETURN "?".
           IF     (vStr EQ "Да" OR vStr BEGINS "Присут") THEN RETURN "1".
           IF NOT (vStr EQ "Да" OR vStr BEGINS "Присут") THEN RETURN "0".
        END.
        WHEN "ДолжнЛицо"     THEN 
        DO:
           vStr = "".
           IF ipCustCat EQ "Ч"  THEN
           DO:
              vStr = "2".
              IF ClientXattrVal(ipCustCat,ipCustID, "Статус_ИПДЛ")    NE "" THEN vStr = "3".
              IF ClientXattrVal(ipCustCat,ipCustID, "СтепРодст_ИПДЛ") NE "" THEN vStr = "4".
              IF ClientXattrVal(ipCustCat,ipCustID, "Статус_ДЛПМО")   NE "" THEN vStr = "5".
           END.
              RETURN vStr.
        END.
        WHEN "ТелФакс"     THEN 
        DO:
           IF ipCustCat EQ "Ч"  THEN
           DO:
              RETURN ClientBaseValue(ipCustCat,ipCustID, "phone") + "," + 
                     ClientBaseValue(ipCustCat,ipCustID, "fax").
           END.
           IF ipCustCat EQ "Ю"  THEN
           DO:
              RETURN ClientXattrVal(ipCustCat,ipCustID,  "tel") + "," + 
                     ClientBaseValue(ipCustCat,ipCustID, "fax").
           END.
           IF ipCustCat EQ "Б"  THEN
           DO:
              RETURN ClientXattrVal(ipCustCat,ipCustID, "tel") + "," + 
                     ClientXattrVal(ipCustCat,ipCustID, "fax").
           END.
        END.
        WHEN "Тел"     THEN 
        DO:
           IF ipCustCat EQ "Ч"  THEN
           DO:
              RETURN ClientBaseValue(ipCustCat,ipCustID, "phone").
           END.
           IF ipCustCat EQ "Ю"  THEN
           DO:
              RETURN ClientXattrVal(ipCustCat,ipCustID,  "tel").
           END.
           IF ipCustCat EQ "Б"  THEN
           DO:
              RETURN ClientXattrVal(ipCustCat,ipCustID, "tel").
           END.
        END.
        WHEN "ТелМоб" THEN
        DO:     
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
           RETURN ClientXattrVal(ipCustCat,ipCustID,"cell-phone").
        END.
        WHEN "EMail" THEN
        DO:     
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
           RETURN ClientXattrVal(ipCustCat,ipCustID,"e-mail").
        END.
        WHEN "ДатаРожд"    THEN 
        DO:
           IF ipCustCat EQ "Ю"  THEN
           DO:
              vStr = ClientXattrVal(ipCustCat,ipCustID, "Birthday").
              IF vStr EQ "" OR DATE(vStr) EQ ? THEN
                 RETURN "".
              ELSE
                 RETURN STRING(DATE(vStr),"99/99/9999").
           END. 
           IF ipCustCat EQ "Ч"  THEN
           DO:
              vStr = ClientBaseValue(ipCustCat,ipCustID, "Birthday").
              IF vStr EQ "" OR DATE(vStr) EQ ? THEN
                 RETURN "".
              ELSE
                 RETURN STRING(DATE(vStr),"99/99/9999").
           END.
        END.
        WHEN "МестоРожд" THEN 
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat) THEN
           DO:
              vStr = ClientXattrVal(ipCustCat,ipCustID,"BirthPlace").
              RETURN vStr.
           END. 
        END.
        WHEN "СтранаР"    THEN 
        DO:
           IF  CAN-DO("Ю,Ч",ipCustCat)   THEN
           DO:  
              vStr  = ClientXattrVal(ipCustCat,ipCustID, "BirthPlace"). 
              IF NUM-ENTRIES(vStr) GE 2 THEN
                 RETURN ENTRY(NUM-ENTRIES(vStr) - 1 , vStr).
           END.
           
        END.
        WHEN "ПунктР"    THEN 
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)   THEN
           DO:  
              vStr = ClientXattrVal(ipCustCat,ipCustID, "BirthPlace").
              IF NUM-ENTRIES(vStr) GT 0
              THEN 
                 RETURN ENTRY(NUM-ENTRIES(vStr), vStr).
              ELSE 
                 RETURN vStr.
           END.
        END.
        WHEN "РегионР"    THEN 
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)   THEN
           DO:  
              vStr = ClientXattrVal(ipCustCat,ipCustID, "BirthPlace").
              IF NUM-ENTRIES(vStr) GE 2 
              THEN
                RETURN ENTRY(NUM-ENTRIES(vStr) - 1, vStr).
              ELSE
                RETURN "".
           END.
        END.
        WHEN "ВидДокКод" THEN
        DO:
           IF ipCustCat EQ "Ю"  THEN
              RETURN  GetCodeMisc("КодДокум",ClientXattrVal(ipCustCat,ipCustID, "document-id"),6).
           IF ipCustCat EQ "Ч"  THEN
              RETURN  GetCodeMisc("КодДокум",ClientBaseValue(ipCustCat,ipCustID, "document-id"),6).
        END.
        WHEN "ВидДокНаимен" THEN
        DO:
           IF ipCustCat EQ "Ю"  THEN
              RETURN  GetCodeName("КодДокум",ClientXattrVal(ipCustCat,ipCustID, "document-id")).
           IF ipCustCat EQ "Ч"  THEN
              RETURN  GetCodeName("КодДокум",ClientBaseValue(ipCustCat,ipCustID, "document-id")).
        END.

        WHEN "СерияДок" THEN
        DO:
           IF ipCustCat EQ "Ю"  THEN
              RETURN  f-SD(ClientXattrVal(ipCustCat,ipCustID, "document"),ClientXattrVal(ipCustCat,ipCustID, "document-id")).
           IF ipCustCat EQ "Ч"  THEN
              RETURN  f-SD(ClientBaseValue(ipCustCat,ipCustID, "document"),ClientBaseValue(ipCustCat,ipCustID, "document-id")).
        END.

        WHEN "НомДок" THEN
        DO:
           IF ipCustCat EQ "Ю"  THEN
              RETURN  f-VD_1(ClientXattrVal(ipCustCat,ipCustID, "document"),ClientXattrVal(ipCustCat,ipCustID, "document-id")).
           IF ipCustCat EQ "Ч"  THEN
              RETURN  f-VD_1(ClientBaseValue(ipCustCat,ipCustID, "document"),ClientBaseValue(ipCustCat,ipCustID, "document-id")).
        END.

        WHEN "КемВыданДок" THEN
        DO:
           IF ipCustCat EQ "Ю"  THEN
           DO:
              vStr = ClientXattrVal(ipCustCat,ipCustID, "issue").
              IF NUM-ENTRIES(vStr) GE 2 THEN
              RETURN ENTRY(NUM-ENTRIES(vStr) - 1, vStr).
           END.
           IF ipCustCat EQ "Ч"  THEN
           DO:   
              vStr = ClientBaseValue(ipCustCat,ipCustID, "issue").
              IF NUM-ENTRIES(vStr) GE 2 THEN
              RETURN ENTRY(NUM-ENTRIES(vStr) - 1, vStr).
           END.
        END.   
        WHEN "ДатаВыдачи" THEN
        DO:
           IF ipCustCat EQ "Ю"  THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID, "Document4Date_vid").
           IF ipCustCat EQ "Ч"  THEN
           DO:  
              ASSIGN
              vDoc   = ClientBaseValue(ipCustCat,ipCustID, "document")
              vDocId = ClientBaseValue(ipCustCat,ipCustID, "document-id").
              FIND FIRST cust-ident WHERE 
                         cust-ident.cust-code      EQ vDoc  
                     AND cust-ident.cust-code-type EQ vDocID
              NO-LOCK NO-ERROR.
              RETURN IF AVAIL cust-ident
                THEN STRING(cust-ident.open-date, "99/99/9999")
                ELSE "01/01/2099".
           END.
        END.
        WHEN "КодПодр" THEN
        DO:
           IF ipCustCat EQ "Ю"  THEN
           DO:
              vStr = ClientXattrVal(ipCustCat,ipCustID, "issue").
              IF NUM-ENTRIES(vStr) GE 2 THEN
                 RETURN ENTRY(2, vStr).
           END.
           IF ipCustCat EQ "Ч"  THEN
           DO:   
              vStr = ClientBaseValue(ipCustCat,ipCustID, "issue").
              IF NUM-ENTRIES(vStr) GE 2 THEN
                 RETURN ENTRY(2, vStr).
           END.
        END.

        WHEN "ВидДокКодПП" THEN
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              RETURN GetCodeEx("VisaType", ClientXattrVal(ipCustCat,ipCustID, "VisaType"), "").
        END.

        WHEN "ВидДокНаименПП" THEN
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              RETURN GetCodeName("VisaType", ClientXattrVal(ipCustCat,ipCustID, "VisaType")).
        END.

        WHEN "НачПравПреб" THEN
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID, "МигрПравПребС").
        END.

        WHEN "ОкончПравПреб" THEN
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
              RETURN ClientXattrVal(ipCustCat,ipCustID, "МигрПравПребПо").
        END.

        WHEN "СерияМигрКарт" THEN
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
           DO:   
              vKodMK = "".
              vStr = ClientXattrVal(ipCustCat,ipCustID, "VisaNum").
              
              IF NUM-ENTRIES(vStr, " ") GE 2 THEN
              DO vI = 1 TO NUM-ENTRIES(vStr, " ") - 1:
                 vKodMK = vKodMK +  ENTRY(vI, vStr, " ").
              END.
              ELSE 
                 vKodMK = vStr.
              RETURN vKodMK.
           END.             
        END.
        WHEN "НомМигрКарт" THEN
        DO:
           IF CAN-DO("Ю,Ч",ipCustCat)  THEN
           DO:
              vKodMK = "".
              vStr = ClientXattrVal(ipCustCat,ipCustID, "VisaNum").
              
              IF NUM-ENTRIES(vStr, " ") GE 2 THEN
                 RETURN ENTRY(NUM-ENTRIES(vStr, " ") , vStr, " ").
           END.
        END.  
      WHEN "МигрПребывС" THEN
      DO:     
         IF CAN-DO("Ю,Ч",ipCustCat)  THEN
            RETURN ClientXattrVal(ipCustCat,ipCustID, "МигрПребывС").
       END.
      WHEN "МигрПребывПо" THEN
      DO:     
         IF CAN-DO("Ю,Ч",ipCustCat)  THEN
            RETURN ClientXattrVal(ipCustCat,ipCustID, "МигрПребывПо").
       END.

      WHEN "АдресРег" THEN
          RETURN  GetAdrr(ipCustCat,ipCustID,1).
      WHEN "АдресФакт" THEN
          RETURN  GetAdrr(ipCustCat,ipCustID,2).
      WHEN "АдресРегГНИ" THEN
          RETURN  GetAdrrGni(ipCustCat,ipCustID,1).
      WHEN "АдресФактГНИ" THEN
          RETURN  GetAdrrGni(ipCustCat,ipCustID,2).
      /*v4*/
      WHEN "СтепРиска" THEN
      DO:     
         IF CAN-DO("Ю,Ч,Б",ipCustCat)  THEN
            RETURN ClientXattrVal(ipCustCat,ipCustID, "РискОтмыв") + "," + ClientXattrVal(ipCustCat,ipCustID, "ОценкаРиска").
      END.
      WHEN "ДатаНачОтнош" THEN
      DO:     
         vFDate = TODAY.
         FOR EACH acct WHERE acct.cust-cat EQ ipCustCat AND 
                             acct.cust-id  EQ ipCustID  NO-LOCK:
            vFDate = IF acct.open-date LT vFDate THEN acct.open-date ELSE vFDate.
         END.
         RETURN STRING(vFDate).
      END.
      WHEN "ДатАнкет" THEN
      DO:     
         vFDate = TODAY.
         FOR EACH acct WHERE acct.cust-cat EQ ipCustCat AND 
                             acct.cust-id  EQ ipCustID  NO-LOCK:
            vFDate = IF acct.open-date LT vFDate THEN acct.open-date ELSE vFDate.
         END.
         RETURN STRING(vFDate).
      END.
      WHEN "ДатОбнАнкет" THEN
      DO:     
         IF CAN-DO("Ю,Ч,Б",ipCustCat)  THEN
            RETURN ClientXattrVal(ipCustCat,ipCustID, "ДатаПослОбАнк").
      END.
      WHEN "Куратор" THEN
      DO:     
         FIND FIRST DataBlock WHERE DataBlock.Data-ID EQ DataLine.Data-ID NO-LOCK NO-ERROR.
         IF AVAIL(DataBlock) 
            THEN vFDate = DataBlock.End-Date.
            ELSE vFDate = TODAY.
         FOR EACH acct WHERE acct.cust-cat   EQ ipCustCat AND 
                             acct.cust-id    EQ ipCustID  AND
                             acct.open-date  LE vFDate    AND 
                             acct.close-date EQ ?         NO-LOCK:
            IF (acct.close-date GT vFDate OR acct.close-date EQ ?) THEN
               RETURN acct.user-id.
         END.
         RETURN "".
      END.
      END CASE.
      RETURN "".
END FUNCTION.

FUNCTION GetBankNameByREGN RETURN CHAR (iRegn AS CHAR):

   DEFINE BUFFER   banks-code FOR banks-code.
   DEFINE BUFFER   Banks      FOR banks.
   DEFINE VARIABLE vReturn    AS CHARACTER NO-UNDO.

   FIND FIRST banks-code WHERE 
              banks-code.bank-code-type EQ "REGN"
          AND banks-code.bank-code      EQ iRegn
   NO-LOCK NO-ERROR.
   IF AVAIL banks-code THEN
   DO:   
      FIND FIRST banks OF banks-code NO-LOCK NO-ERROR.
      IF AVAIL banks THEN
         vReturn = banks.name.
   END. 
   RETURN vReturn.

END FUNCTION.

PROCEDURE GetBranchInfo.
   DEFINE INPUT  PARAMETER iBranch     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER vNameKO     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER vRegNumKO   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER vNumFilial  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER vBikKO      AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE vREGN       AS CHARACTER NO-UNDO.

   FIND FIRST branch WHERE branch.branch-id EQ iBranch NO-LOCK NO-ERROR.
   IF AVAIL branch THEN
   DO:
      
      ASSIGN 
         vREGN = GetXattrValueEX("branch",STRING(branch.branch-id), "REGN", "").       
         IF INDEX(vREGN,"/") GT 0  THEN
            ASSIGN
               vRegNumKO  =  ENTRY(1,vREGN, "/")
               vNumFilial =  ENTRY(2,vREGN, "/")
               vNameKO =  GetBankNameByREGN(vRegNumKO) +  ", филиала " +   GetBankNameByREGN(vREGN).
         ELSE 
            ASSIGN
               vRegNumKO  = vREGN 
               vNumFilial =  ""
               vNameKO =  GetBankNameByREGN(vRegNumKO).
     
      
      vBikKO = GetXattrValueEX("branch",STRING(branch.branch-id), "БанкМФО", "").
      IF NOT {assigned vBikKO} THEN
         vBikKO =  FGetSetting("БанкМФО",?,"").
    
   END.   
END PROCEDURE.

PROCEDURE UpdateInfoVid:           
   DEFINE INPUT PARAMETER iDataId   AS INT64 NO-UNDO.
   DEFINE INPUT PARAMETER iSym1     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iNewCode  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vCode       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE vIndex      AS INT64     NO-UNDO.
   
   DEFINE BUFFER bMainDataLine FOR Dataline.
   DEFINE BUFFER bDataLine     FOR Dataline.
   
   FIND FIRST bMainDataLine WHERE 
              bMainDataLine.Data-Id EQ iDataId
          AND bMainDataLine.Sym1    EQ iSym1
          AND bMainDataLine.Sym2    EQ "Общие данные"
   NO-LOCK NO-ERROR NO-WAIT.
   IF    AVAIL  bMainDataLine 
     AND NUM-ENTRIES(bMainDataLine.txt, "~n")  GE 9  
   THEN DO:
      vCode = ENTRY(9,bMainDataLine.txt, "~n").

      IF  LOOKUP(iNewCode, vCode) EQ 0 THEN
      DO:
         vCode = vCode  + (IF {assigned vCode} THEN  "," ELSE "" ) +   iNewCode.
         DO vIndex = 1 TO NUM-ENTRIES(vCode):
            IF {assigned ENTRY(vIndex,vCode)} THEN DO:
            
            CREATE tt-code.
            ASSIGN 
               tt-code.fcode = INT64(ENTRY(vIndex,vCode))
            NO-ERROR.
            END.
         END.
         
         vCode = "".
         FOR  EACH tt-code
         BY tt-code.fcode:
            vCode = vCode                                   + 
                   (IF {assigned vCode} THEN  "," ELSE "" ) + 
                   STRING(tt-code.fcode,"99").
         END.
         
         FIND FIRST bDataLine WHERE 
                    bDataLine.Data-Id EQ DataLine.Data-Id
                AND bDataLine.Sym1    EQ DataLine.Sym1
                AND bDataLine.Sym2    EQ "Общие данные"
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF    AVAIL  bDataLine 
           AND NOT LOCKED(bDataLine)
           AND NUM-ENTRIES(bDataLine.txt, "~n")  GE 9
         THEN 
             ENTRY(9,bDataLine.txt, "~n") = vCode.
     END.
   END.

END PROCEDURE.




PROCEDURE CreateVigBenLines:
   DEFINE INPUT PARAMETER iType       AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iCustCat    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iCustId     AS INT64     NO-UNDO.
   DEFINE INPUT PARAMETER iMainLineId AS ROWID     NO-UNDO.

   DEFINE BUFFER bMainLine FOR DataLine.
   DEFINE BUFFER bDataLine FOR DataLine.
   DEFINE BUFFER DataLine  FOR DataLine.


   DEFINE VARIABLE vClntType     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIsRes        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUSvedPr      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDolzhL       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDocUdL4      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIsLic9       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClntLNam     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClntFNam     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClntMNam     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUFName       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUSName       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUEName       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOPForma      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUOrgU        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUUstCap      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTelFax       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOGRN         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRegDate      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRONam        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBIKKOCl      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vGrazhd       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBirthDay     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCountry2     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIndex2       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOKATO2       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRegion2      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPunkt2       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStreet2      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDom2         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKorpus2      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOffice2      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCountry3     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOKATO3       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRegion3      AS CHARACTER   NO-UNDO.  
   DEFINE VARIABLE vPunkt3       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVidDocKod4   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNamDoc4      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSeriaDoc4    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNomerDoc4    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKemVyd4      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDateVyd4     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKodPodr4     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVidDocKod5   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVidDocNam5   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSeriaDoc5    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNomerDoc5    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegDate5     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vEndDate5     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKodMigrK6    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSerMigrK6    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNomMigrK6    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegDate6     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vEndDate6     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAdress1      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAdress2      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCountry7     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIndex7       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOKATO7       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRegion7      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPunkt7       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStreet7      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDom7         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKorpus7      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOffice7      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCountry8     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIndex8       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOKATO8       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRegion8      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPunkt8       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStreet8      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDom8         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKorpus8      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOffice8      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vLicence      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVidLic9      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNomLic9      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDatVydLic9   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKemVydLic9   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSrokLic9     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVidLicD9     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI            AS INT64       NO-UNDO.
   DEFINE VARIABLE vCustCatInt   AS CHARACTER   NO-UNDO.

   FIND FIRST bMainLine WHERE
              ROWID(bMainLine) EQ iMainLineId 
   NO-LOCK NO-ERROR.
   IF NOT AVAIL(bMainLine) THEN
      RETURN.

   ASSIGN
      vI = 1
      vCustCatInt =  STRING(LOOKUP(iCustCat,"Ч,Ю,Б")).

   FOR EACH bDataLine WHERE
            bDataLine.Data-ID EQ bMainLine.Data-ID
        AND bDataLine.Sym1    EQ bMainLine.Sym1
        AND bDataLine.Sym2    EQ iType
   NO-LOCK:
     IF    ENTRY(1,bDataLine.Txt,"~n") EQ vCustCatInt
       AND ENTRY(2,bDataLine.Txt,"~n") EQ STRING(iCustId,"9999999")  
     THEN
        RETURN.

     vI = vI + 1.
   END.


   CREATE DataLine.
   ASSIGN
      DataLine.Data-ID = bMainLine.Data-ID
      DataLine.Sym1    = bMainLine.Sym1
      DataLine.Sym2    = iType
      DataLine.Sym3    = STRING(vI)
   .
   
   ASSIGN   
      vClntType   = GetChckAttrByClnt(iCustCat,iCustID,"ТипКлиент")
      vIsRes      = GetChckAttrByClnt(iCustCat,iCustID,"ПризнРез")
      vDolzhL     = GetChckAttrByClnt(iCustCat,iCustID,"ДолжнЛицо")
                       
      vDolzhL     = IF vDolzhL EQ "" 
                    THEN "1" 
                    ELSE vDolzhL
                     
      vUSvedPr    = GetChckAttrByClnt(iCustCat,iCustID,"ПРИСУТАДР")
                     
      vUSvedPr    = IF vUSvedPr EQ "1" THEN "2" ELSE  "1"
                  
      vClntLNam   = GetChckAttrByClnt(iCustCat,iCustId,"ФАМ")
      vClntFNam   = GetChckAttrByClnt(iCustCat,iCustId,"ИМЯ")
      vClntMNam   = GetChckAttrByClnt(iCustCat,iCustId,"ОТЧ")
      vUFName     = GetChckAttrByClnt(iCustCat,iCustId,"НаимЮЛ")
      vUSName     = GetChckAttrByClnt(iCustCat,iCustId,"НаимЮЛСокр")
      vUEName     = GetChckAttrByClnt(iCustCat,iCustId,"НаимЮЛИностр")
      vOPForma    = GetChckAttrByClnt(iCustCat,iCustId,"ОПФорма")
      vINN        = GetChckAttrByClnt(iCustCat,iCustId,"ИНН")
      vOGRN       = GetChckAttrByClnt(iCustCat,iCustId,"ОГРН")
      vRegDate    = STRING(DATE(GetChckAttrByClnt(iCustCat,
                                                   iCustId,
                                                   "ДатаРег")),
                           "99/99/9999")
      vRONam      = GetChckAttrByClnt(iCustCat,iCustId,"РегОрган")
      vBIKKOCl    = ""
      vUOrgU      = GetChckAttrByClnt(iCustCat,iCustId,"ОргУправ")
      vUUstCap    = GetChckAttrByClnt(iCustCat,iCustId,"УставКап")
      vGrazhd     = GetChckAttrByClnt(iCustCat,iCustId,"Гражд")
      
      vBirthDay   = STRING(DATE(GetChckAttrByClnt(iCustCat,
                                                 iCustId,
                                                 "ДатаРожд")),
                           "99/99/9999")
      vTelFax     = GetChckAttrByClnt(iCustCat,iCustId,"ТелФакс")

     
      vRegion3    = GetChckAttrByClnt(iCustCat,iCustId,"РегионР")
      
      vPunkt3     = GetChckAttrByClnt(iCustCat,iCustId,"ПунктР")
      vDocUdL4    = IF vGrazhd EQ "643" THEN "1" ELSE "2"
      vVidDocKod4 = GetChckAttrByClnt(iCustCat,iCustId,"ВидДокКод")
      vNamDoc4    = GetChckAttrByClnt(iCustCat,iCustId,"ВидДокНаимен")
    
      vSeriaDoc4  = GetChckAttrByClnt(iCustCat,iCustId,"СерияДок")
      vNomerDoc4  = GetChckAttrByClnt(iCustCat,iCustId,"НомДок")
      vKemVyd4    = GetChckAttrByClnt(iCustCat,iCustId,"КемВыданДок")
      vDateVyd4   = GetChckAttrByClnt(iCustCat,iCustId,"ДатаВыдачи")
      vKodPodr4   = GetChckAttrByClnt(iCustCat,iCustId,"КодПодр")
      vVidDocKod5 = GetChckAttrByClnt(iCustCat,iCustId,"ВидДокКодПП")
      vVidDocNam5 = GetChckAttrByClnt(iCustCat,iCustId,"ВидДокНаименПП")
      vSeriaDoc5  = ""
      vNomerDoc5  = ""
      vBegDate5   = GetChckAttrByClnt(iCustCat,iCustId,"МигрПравПребС")
      vEndDate5   = GetChckAttrByClnt(iCustCat,iCustId,"МигрПравПребПо")
      vKodMigrK6  = "39"
      vSerMigrK6  = GetChckAttrByClnt(iCustCat,iCustId,"СерияМигрКарт")
      vNomMigrK6  = GetChckAttrByClnt(iCustCat,iCustId,"НомМигрКарт")
      vBegDate6   = GetChckAttrByClnt(iCustCat,iCustId,"МигрПребывС")
      vEndDate6   = GetChckAttrByClnt(iCustCat,iCustId,"МигрПребывПо")
      vCountry7   = GetEntries(1,vAdress1,",","")
      vIndex7     = GetEntries(2,vAdress1,",","")    
      vOKATO7     = GetEntries(3,vAdress1,",","")   
      vPunkt7     = GetEntries(5,vAdress1,",","")   
      vPunkt7     = IF NOT {assigned vPunkt7} 
                    THEN GetEntries(6,vAdress1,",","") 
                    ELSE vPunkt7     
      vStreet7    = GetEntries(7,vAdress1,",","")     
      vDom7       = GetEntries(7,vAdress1,",","")
      vKorpus7    = GetEntries(9,vAdress1,",","")      
      vOffice7    = GetEntries(10,vAdress1,",","")     
      vCountry8   = GetEntries(1,vAdress2,",","")
      vIndex8     = GetEntries(2,vAdress2,",","")    
      vOKATO8     = GetEntries(3,vAdress2,",","")   
      vPunkt8     = GetEntries(5,vAdress2,",","")   
      vPunkt8     = IF NOT {assigned vPunkt8} 
                    THEN GetEntries(6,vAdress2,",","") 
                    ELSE vPunkt8     
      vStreet8    = GetEntries(7,vAdress2,",","")     
      vDom8       = GetEntries(8,vAdress2,",","")      
      vKorpus8    = GetEntries(9,vAdress2,",","")
      vOffice8    = GetEntries(10,vAdress2,",","")
      vLicence    = GetChckAttrByClnt(iCustCat,iCustId,"Лицензия")
      vIsLic9     = GetEntries(1,vLicence,CHR(1),"")
      vVidLic9    = GetEntries(2,vLicence,CHR(1),"|1| |2| |3| |4| |5|")
      vNomLic9    = GetEntries(3,vLicence,CHR(1),"|1| |2| |3| |4| |5|")
      vDatVydLic9 = GetEntries(4,vLicence,CHR(1),"|1| |2| |3| |4| |5|")
      vKemVydLic9 = GetEntries(5,vLicence,CHR(1),"|1| |2| |3| |4| |5|")
      vSrokLic9   = GetEntries(6,vLicence,CHR(1),"|1| |2| |3| |4| |5|")
      vVidLicD9   = GetEntries(7,vLicence,CHR(1),"|1| |2| |3| |4| |5|")
  
   .

   DataLine.Txt =  FStrNVL(vCustCatInt ,"0")                     + "~n" +       
                   FStrNVL(STRING(iCustId,"9999999"),"")         + "~n" +       
                   FStrNVL(vClntType,"")                         + "~n" +       
                   FStrNVL(vIsRes,"")                            + "~n" +       
                   FStrNVL(REPLACE(vClntLNam,"~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(REPLACE(vClntFNam,"~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(REPLACE(vClntMNam,"~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(REPLACE(vUFName,  "~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(REPLACE(vUSName,  "~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(REPLACE(vUEName,  "~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(REPLACE(vOPForma, "~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(vINN,"")                              + "~n" +       
                   FStrNVL(vOGRN,"")                             + "~n" +       
                   FStrNVL(STRING(DATE(vRegDate),"99/99/9999"),"")     + "~n" +       
                   FStrNVL(vRONam,"")                            + "~n" +       
                   FStrNVL(vBIKKOCl,"")                          + "~n" +       
                   FStrNVL(REPLACE(vUOrgU,   "~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(REPLACE(vUUstCap, "~n",CHR(1)),"")    + "~n" +       
                   FStrNVL(vUSvedPr,"")                          + "~n" +       
                   FStrNVL(STRING(vGrazhd,"999"),"")             + "~n" +       
                   FStrNVL(vDolzhL,"")                           + "~n" +       
                   FStrNVL(STRING(DATE(vBirthDay),"99/99/9999"),"")    + "~n" +       
                   FStrNVL(REPLACE(vTelFax,  "~n",CHR(1)),"")    + "~n" +       

                   FStrNVL(STRING(vCountry2,"999"),"")           + "~n" +       
                   FStrNVL(STRING(vIndex2,"999999"),"")          + "~n" +       
                   FStrNVL(STRING(vOKATO2,"99"),"")              + "~n" +       
                   FStrNVL(REPLACE(vRegion2,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(REPLACE(vPunkt2, "~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(REPLACE(vStreet2,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(vDom2,"")                             + "~n" +       
                   FStrNVL(vKorpus2,"")                          + "~n" +       
                   FStrNVL(vOffice2,"")                          + "~n" +       

                   FStrNVL(STRING(vCountry3,"999"),"")           + "~n" +       
                   FStrNVL(STRING(vOKATO3,"99"),"")              + "~n" +       
                   FStrNVL(REPLACE(vRegion3,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(REPLACE(vPunkt3, "~n",CHR(1)),"")     + "~n" +       

                   FStrNVL(STRING(vDocUdL4,"9"),"")              + "~n" +       
                   FStrNVL(vVidDocKod4,"")                       + "~n" +       
                   FStrNVL(REPLACE(vNamDoc4,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(vSeriaDoc4,"")                        + "~n" +       
                   FStrNVL(vNomerDoc4,"")                        + "~n" +       
                   FStrNVL(REPLACE(vKemVyd4,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(STRING(vDateVyd4),"")                 + "~n" +       
                   FStrNVL(vKodPodr4,"")                         + "~n" +       

                   FStrNVL(vVidDocKod5,"")                       + "~n" +       
                   FStrNVL(REPLACE(vVidDocNam5,"~n",CHR(1)),"")  + "~n" +       
                   FStrNVL(vSeriaDoc5,"")                        + "~n" +       
                   FStrNVL(vNomerDoc5,"")                        + "~n" +       
                   FStrNVL(STRING(vBegDate5),"")                 + "~n" +       
                   FStrNVL(STRING(vEndDate5),"")                 + "~n" +       

                   FStrNVL(vKodMigrK6,"")                        + "~n" +       
                   FStrNVL(vSerMigrK6,"")                        + "~n" +       
                   FStrNVL(vNomMigrK6,"")                        + "~n" +       
                   FStrNVL(STRING(vBegDate6),"")                 + "~n" +       
                   FStrNVL(STRING(vEndDate6),"")                 + "~n" +       

                   FStrNVL(STRING(vCountry7,"999"),"")           + "~n" +       
                   FStrNVL(STRING(vIndex7,"999999"),"")          + "~n" +       
                   FStrNVL(STRING(vOKATO7,"99"),"")              + "~n" +       
                   FStrNVL(REPLACE(vRegion7,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(REPLACE(vPunkt7, "~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(REPLACE(vStreet7,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(vDom7,"")                             + "~n" +       
                   FStrNVL(vKorpus7,"")                          + "~n" +       
                   FStrNVL(vOffice7,"")                          + "~n" +       

                   FStrNVL(STRING(vCountry8,"999"),"")           + "~n" +       
                   FStrNVL(STRING(vIndex8,"999999"),"")          + "~n" +       
                   FStrNVL(STRING(vOKATO8,"99"),"")              + "~n" +       
                   FStrNVL(REPLACE(vRegion8,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(REPLACE(vPunkt8, "~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(REPLACE(vStreet8,"~n",CHR(1)),"")     + "~n" +       
                   FStrNVL(vDom8,"")                             + "~n" +       
                   FStrNVL(vKorpus8,"")                          + "~n" +       
                   FStrNVL(vOffice8,"")                          + "~n" +       

                   FStrNVL(vIsLic9,"")                           + "~n" +       
                   FStrNVL(REPLACE(vVidLic9,   "~n",CHR(1)),"")  + "~n" +       
                   FStrNVL(REPLACE(vNomLic9,   "~n",CHR(1)),"")  + "~n" +       
                   FStrNVL(REPLACE(vDatVydLic9,"~n",CHR(1)),"")  + "~n" +       
                   FStrNVL(REPLACE(vKemVydLic9,"~n",CHR(1)),"")  + "~n" +       
                   FStrNVL(REPLACE(vSrokLic9,  "~n",CHR(1)),"")  + "~n" +       
                   FStrNVL(REPLACE(vVidLicD9,  "~n",CHR(1)),"")  + "~n" .       

END PROCEDURE.

FUNCTION GetLic CHARACTER (iCustCat AS CHARACTER,
                            iCustId  AS INT64):

   DEFINE VARIABLE vI           AS INT64     NO-UNDO.
   DEFINE VARIABLE vStr1        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStr2        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStr3        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStr4        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStr5        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStr6        AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER   cust-ident   FOR cust-ident.
   
   ASSIGN
      vStr1 = ""
      vStr2 = ""
      vStr3 = ""
      vStr4 = ""
      vStr5 = ""
      vStr6 = ""
      vI = 0.
   FOR EACH cust-ident WHERE cust-ident.Class-code EQ "cust-lic"   AND
                             cust-ident.cust-cat EQ iCustCat       AND 
                             cust-ident.cust-id  EQ INT64(iCustID) AND 
                            (cust-ident.close-date >= TODAY OR cust-ident.close-date EQ ?) NO-LOCK:
      vI = vI + 1.
      ASSIGN
         vStr1 = vStr1 + "|" + TRIM(STRING(vI,">>9")) + "|" + IF TRIM(GetXattrValueEx("cust-ident",SURROGATE(BUFFER cust-ident:HANDLE),"ВидЛиценз","")) EQ "" THEN " " ELSE TRIM(GetXattrValueEx("cust-ident",SURROGATE(BUFFER cust-ident:HANDLE),"ВидЛиценз",""))
         vStr2 = vStr2 + "|" + TRIM(STRING(vI,">>9")) + "|" + IF TRIM(cust-ident.cust-code) EQ "" THEN " " ELSE TRIM(cust-ident.cust-code)
         vStr3 = vStr3 + "|" + TRIM(STRING(vI,">>9")) + "|" + IF FStrNVL(STRING(cust-ident.open-date),"") EQ "" THEN " " ELSE FStrNVL(STRING(cust-ident.open-date, "99/99/9999"),"")
         vStr4 = vStr4 + "|" + TRIM(STRING(vI,">>9")) + "|" + IF TRIM(cust-ident.issue) EQ "" THEN " " ELSE TRIM(cust-ident.issue)
         vStr5 = vStr5 + "|" + TRIM(STRING(vI,">>9")) + "|" + IF FStrNVL(STRING(cust-ident.close-date),"") EQ "" THEN " " ELSE FStrNVL(STRING(cust-ident.close-date, "99/99/9999"),"")
         vStr6 = vStr6 + "|" + TRIM(STRING(vI,">>9")) + "|" + IF TRIM(GetCodeDesc("ВидЛицДеят",cust-ident.cust-code-type,1,"")) EQ "" THEN " " ELSE TRIM(GetCodeDesc("ВидЛицДеят",cust-ident.cust-code-type,1,"")).
   END.

   RETURN (IF vI > 0 THEN "2" ELSE "1") + CHR(1) + vStr1 + CHR(1) + vStr2 + CHR(1) + vStr3 + CHR(1) + vStr4 + CHR(1) + vStr5 + CHR(1) + vStr6.
END FUNCTION .