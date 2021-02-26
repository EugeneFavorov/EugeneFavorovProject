/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: 148n.i
      Comment: Общие функции контроля реквизитов документов
               согласно Приказу 148н.
   Parameters:
      Created: 24.03.2016 krok
     Modified: 02.08.2016 17:25 BIS     
*/

&IF DEFINED(DEF_148N_I_) = 0 &THEN

DEFINE TEMP-TABLE ttNalRec NO-UNDO 
   FIELD ben-acct LIKE op.ben-acct
   FIELD PokST    AS CHARACTER  /* (101) */
   FIELD kpp-send AS CHARACTER  /* (102) */
   FIELD kpp-rec  AS CHARACTER  /* (103) */
   FIELD KBK      AS CHARACTER  /* (104) */
   FIELD OKTMO    AS CHARACTER  /* (105) */
   FIELD PokOP    AS CHARACTER  /* (106) */
   FIELD PokNP    AS CHARACTER  /* (107) */
   FIELD PokND    AS CHARACTER  /* (108) */
   FIELD PokDD    AS CHARACTER  /* (198) */
.

&GLOBAL-DEFINE DEF_148N_I_ YES

/*
   Проверка того, что полю присвоено значение "в смысле налоговых реквизитов".
   Т.е. "0" приравнивается к пустому значению.
*/
FUNCTION isAssigned148n RETURN LOGICAL (INPUT iStr AS CHARACTER):
   RETURN {assigned iStr} AND iStr <> "0".
END FUNCTION.

/*
   Проверка того, что строка iStr состоит только из цифр. Символы-разделители
   в конце строки игнорируются при проверке.
   Если iNumDigits <> ?, строка должна содержать ровно iNumDigits символов.
   Если iNonZero = YES, строка не может состоять из одних нулей.
*/
FUNCTION isDigital148n RETURN LOGICAL PRIVATE (INPUT iStr       AS CHARACTER,
                                               INPUT iNumDigits AS INT64,
                                               INPUT iNonZero   AS LOGICAL):
   IF iStr = ? THEN
      RETURN ?.
   iStr = RIGHT-TRIM(iStr).
   RETURN (iNumDigits = ? OR iNumDigits = LENGTH(iStr)) AND
          TRIM(iStr, "0123456789") = ""                 AND
          NOT (iNonZero = YES AND TRIM(iStr, "0") = "").
END FUNCTION.

/*
   Проверка того, что строка является датой в формате ДД.ММ.ГГГГ.
   Символы-разделители в конце строки игнорируются при проверке.
*/
FUNCTION isDate148n RETURN LOGICAL PRIVATE (INPUT iStr AS CHARACTER):
   DEFINE VARIABLE vDate AS DATE NO-UNDO.

   IF iStr = ? THEN
      RETURN ?.
   iStr = RIGHT-TRIM(iStr).
   IF LENGTH(iStr) = 10           AND
      SUBSTRING(iStr, 3, 1) = "." AND
      SUBSTRING(iStr, 6, 1) = "."
   THEN DO:
      vDate = DATE(iStr) NO-ERROR.
      RETURN (NOT ERROR-STATUS:ERROR AND vDate <> ?).
   END.
   ELSE
      RETURN NO.
END FUNCTION.

/*
   Определение контрольной суммы значимой области УИН (первые 19 разрядов)
   относительно заданного сдвига последовательности весовых коэффициентов
*/
FUNCTION getUINChecksum RETURN INT64 PRIVATE (INPUT iStr   AS CHARACTER,
                                              INPUT iShift AS INT64):
   DEFINE VARIABLE vI AS INT64 NO-UNDO.
   DEFINE VARIABLE vN AS INT64 NO-UNDO.
   DEFINE VARIABLE vW AS INT64 NO-UNDO.
   DEFINE VARIABLE vS AS INT64 NO-UNDO.

   DO vI = 1 TO LENGTH(iStr):
      ASSIGN
         vN = INT64(SUBSTRING(iStr, vI, 1))
         vW = 1 + (iShift + vI - 1) MODULO 10
      .
      vS = vS + vN * vW.
   END.
   RETURN vS MODULO 11.
END FUNCTION.

/*
   Определение контрольного разряда УИН. Результат уже приведён к
   строковому типу. При передаче некорректного значения (с длиной,
   отличной от 20, или содержащего недопустимые символы) возвращает ?.
*/
FUNCTION getUINKey RETURN CHARACTER PRIVATE (INPUT iUIN AS CHARACTER):
   DEFINE VARIABLE vS AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vK AS INT64     NO-UNDO.

   IF isDigital148n(iUIN, 20, NO) THEN DO:
      vS = SUBSTRING(iUIN, 1, 19).
      vK = getUINChecksum(vS, 0).
      IF vK = 10 THEN
         vK = getUINChecksum(vS, 2).
      IF vK = 10 THEN
         vK = 0.
   END.
   ELSE
      vK = ?.
   RETURN STRING(vK).
END FUNCTION.

/*
   Проверка контрольного разряда УИН. Возвращает значение логического типа,
   соответствующее правильности контрольного разряда, либо ? в случае, если
   было передано некорректное значение УИН (с длиной, отличной от 20, или
   содержащее недопустимые символы)
*/
FUNCTION checkUINKey RETURN LOGICAL PRIVATE (INPUT iUIN AS CHARACTER):
   DEFINE VARIABLE vK AS CHARACTER NO-UNDO.

   vK = getUINKey(iUIN).
   RETURN IF vK = ?
          THEN ?
          ELSE vK = SUBSTRING(iUIN, LENGTH(iUIN)).
END FUNCTION.

/* 1 */
FUNCTION check148n-pokst-uin-pninn RETURN CHARACTER PRIVATE (INPUT iPokST AS CHARACTER,
                                                             INPUT iUIN   AS CHARACTER,
                                                             INPUT iPNINN AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("ГНИ", "СтатусИП", ""), iPokST) AND
      NOT ({assigned iUIN} OR {assigned iPNINN})
   THEN
      vErrorMsg = "Для введенного значения ПокСТ " +
                  "в документе должны быть заданы УИН или ИНН плательщика".
   RETURN vErrorMsg.
END FUNCTION.

FUNCTION check148n-pninn RETURN CHARACTER PRIVATE (INPUT iPNINN AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF {assigned iPNINN} AND
      NOT isDigital148n(iPNINN, 12, YES)
   THEN
      vErrorMsg = "В поле ИНН плательщика необходимо указать 12 цифр, " +
                  "не являющихся нулями одновременно".
   RETURN vErrorMsg.
END FUNCTION.

/* 2 */
FUNCTION check148n-kbk-pokop-pokdd RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                             INPUT iPokOP AS CHARACTER,
                                                             INPUT iPokDD AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAffinity AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("ГНИ", "КБКНалог", ""), iKBK) THEN
      vAffinity = "н".
   ELSE IF CAN-DO(FGetSetting("ГНИ", "КБКТамож", ""), iKBK) THEN
      vAffinity = "т".
   IF {assigned vAffinity} AND
      {assigned iPokOP}    AND
      iPokDD = "0"
   THEN
      vErrorMsg = "Для данного значения поля КБК поле ПокДД не может " +
                  "содержать значение ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.

/* 3 */
FUNCTION check148n-kbk-pokop-pnpoluwcatelwinn RETURN CHARACTER PRIVATE (INPUT iKBK              AS CHARACTER,
                                                                        INPUT iPokOP            AS CHARACTER,
                                                                        INPUT iPNPoluwcatelwINN AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAffinity AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("ГНИ", "КБКНалог", ""), iKBK) THEN
      vAffinity = "н".
   ELSE IF CAN-DO(FGetSetting("ГНИ", "КБКТамож", ""), iKBK) THEN
      vAffinity = "т".
   IF {assigned vAffinity} AND
      {assigned iPokOP}    AND
      NOT isDigital148n(iPNPoluwcatelwINN, 10, YES)
   THEN
      vErrorMsg = "Для данного значения поля КБК поле ИНН получателя должно " +
                  "содержать 10 цифр, не равных ~"0~" одновременно".
   RETURN vErrorMsg.
END FUNCTION.

/* 4 */
FUNCTION check148n-kbk RETURN CHARACTER PRIVATE (INPUT iKBK AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF iKBK = FILL("0", 20) THEN
      vErrorMsg = "Недопустимое значение поля КБК".
   RETURN vErrorMsg.
END FUNCTION.

/* 5 */
FUNCTION check148n-okatonalog RETURN CHARACTER PRIVATE (INPUT iOkato-Nalog AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF NOT (iOkato-Nalog = "0"                   OR
           isDigital148n(iOkato-Nalog, 8 , YES) OR
           isDigital148n(iOkato-Nalog, 11, YES))
   THEN
      vErrorMsg = "В поле ОКТМО необходимо указать либо ~"0~", либо строку " +
                  "из 8 или 11 цифр, не состоящую только из нулей".
   RETURN vErrorMsg.
END FUNCTION.

/* 6 */
FUNCTION check148n-kbk-pokst RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                       INPUT iPokST AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF iKBK   <> "0" AND
      iPokST =  "0"
   THEN
      vErrorMsg = "При указании КБК, отличного от ~"0~", поле ПокСТ " +
                  "не может принимать значение ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.

/* 7 */
FUNCTION check148n-kbk-pokop RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                       INPUT iPokOP AS CHARACTER):
   DEFINE BUFFER code FOR code.

   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAffinity AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("ГНИ", "КБКНалог", ""), iKBK) THEN
      vAffinity = "н".
   ELSE IF CAN-DO(FGetSetting("ГНИ", "КБКТамож", ""), iKBK) THEN
      vAffinity = "т".
   FIND FIRST code WHERE
      code.class = "Нал:ОП" AND
      code.code  = iPokOP
   NO-LOCK NO-ERROR.
   IF {assigned vAffinity} THEN DO:
      IF AVAILABLE code AND
         NOT CAN-DO(code.description[1], vAffinity)
      THEN
         vErrorMsg = SUBSTITUTE("Указанное основание платежа должно иметь " +
                                "признак принадлежности &1 в классификаторе &2",
                                QUOTER(vAffinity),
                                code.class).
   END.
   ELSE DO:
      IF iPokOP <> "0" AND
         {assigned code.description[1]}
      THEN
         vErrorMsg = SUBSTITUTE("Для данного значения &1 в поле &2 необходимо " +
                                "указать &3 либо основание, не имеющее " +
                                "признака принадлежности в классификаторе &4",
                                QUOTER("КБК"),
                                QUOTER("Основание платежа"),
                                QUOTER("0"),
                                code.class).
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* 8 */
FUNCTION check148n-pokop-poknd-pokdd RETURN CHARACTER PRIVATE (INPUT iPokOP AS CHARACTER,
                                                               INPUT iPokND AS CHARACTER,
                                                               INPUT iPokDD AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vError    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vI        AS INT64     NO-UNDO.

   IF iPokOP = "ТП" THEN DO:
      IF iPokND <> "0" OR NOT isDate148n(iPokDD) THEN
         vErrorMsg = "Для данного значения ПокОП в поле ПокНД " +
                     "необходимо указать значение ~"0~", а в "  +
                     "поле ПокДД - дату в формате ДД.ММ.ГГГГ".
   END.
   ELSE DO:
      IF LENGTH(iPokND)            > 0   AND
         LENGTH(iPokND)           <  16  AND
         iPokND                   <> "0" AND
         NUM-ENTRIES(iPokND, ";") =  1
      THEN DO:
         IF NOT isDate148n(iPokDD) THEN
            vErrorMsg = "Для такого значения ПокНД в поле ПокДД " +
                        "необходимо указать дату в формате ДД.ММ.ГГГГ".
      END.
      ELSE DO:
         IF iPokND = "0" THEN
            vError = YES.
         ELSE IF NUM-ENTRIES(iPokND, ";") > 1 THEN DO:
            vI = INT64(ENTRY(1, iPokND, ";")) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR AND
               vI > 0                 AND
               CAN-FIND(FIRST person WHERE
                           person.person-id = vI
                        NO-LOCK)
            THEN
               vError = YES.
         END.
         IF vError AND iPokDD <> "0" THEN
            vErrorMsg = "Для такого значения ПокНД в поле ПокДД " +
                        "необходимо указать ~"0~"".
      END.
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* 9 */
FUNCTION check148n-pokop-poknd RETURN CHARACTER PRIVATE (INPUT iPokOP AS CHARACTER,
                                                         INPUT iPokND AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("ГНИ", "Осн106ПокНД0", ""), iPokOP) AND
      iPokND <> "0"
   THEN
      vErrorMsg = "Для данного значения ПокОП в поле ПокНД " +
                  "необходимо указать значение ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.

/* 10 */
FUNCTION check148n-uin-kbk RETURN CHARACTER PRIVATE (INPUT iUIN AS CHARACTER,
                                                     INPUT iKBK AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vError    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vS        AS CHARACTER NO-UNDO.

   IF iUIN <> "0" THEN DO:
      vS = TRIM(iUIN, "0").
      CASE LENGTH(iUIN):
         WHEN 20 THEN
            vError = NOT {assigned vS} OR iUIN = iKBK.
         WHEN 25 THEN
            vError = NOT {assigned vS}.
         OTHERWISE
            vError = YES.
      END CASE.
      IF vError THEN
         vErrorMsg = "В поле УИН должно быть указано значение ~"0~", либо "  +
                     "оно должно содержать 20 или 25 символов, причём при "  +
                     "длине 20 его значение не может совпадать со значением" +
                     "поля ~"КБК~"".
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* ? */
FUNCTION check148n-kbk-pokop-poknp RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                             INPUT iPokOP AS CHARACTER,
                                                             INPUT iPokNP AS CHARACTER):
   DEFINE BUFFER code FOR code.

   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vError    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vS        AS CHARACTER NO-UNDO.

   IF CAN-DO(GetCodeDesc("Нал:ОП", iPokOP, 1, ""), "н") AND
      CAN-DO(FGetSetting("ГНИ", "КБКНалог", ""), iKBK)
   THEN DO:
      /* налоговый платёж */
      IF CAN-DO(FGetSetting("ГНИ", "Н106-107Дата", ""), iPokOP) THEN DO:
         IF NOT isDate148n(iPokNP) THEN
            vErrorMsg = "Для налоговых платежей с таким основанием в поле " +
                        "(107) необходимо указать дату в формате ДД.ММ.ГГГГ".
      END.
      ELSE IF CAN-DO(FGetSetting("ГНИ", "Н106-107Ноль", ""), iPokOP) THEN DO:
         IF iPokNP <> "0" THEN
            vErrorMsg = "Для налоговых платежей с таким основанием в поле " +
                        "(107) необходимо указать ~"0~"".
      END.
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* ? */
FUNCTION check148n-_personid-pninn RETURN CHARACTER PRIVATE (INPUT iPersonId AS INT64,
                                                             INPUT iPNINN    AS CHARACTER):
   DEFINE BUFFER person FOR person.

   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   FIND FIRST person WHERE
      person.person-id = iPersonId
   NO-LOCK NO-ERROR.
   IF AVAILABLE person          AND
      NOT {assigned person.inn} AND
      iPNINN <> "0"
   THEN
      vErrorMsg = "У плательщика отсутствует ИНН, в поле ИНН плательщика " +
                  "необходимо указать ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.


/* Перекрестный контроль (106) и (107) полей */
PROCEDURE check148n-106-107:
   DEFINE INPUT  PARAMETER TABLE FOR ttNalRec BIND.
   DEFINE OUTPUT PARAMETER oErrorMsg AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vIsBudget AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vKBKNalog  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKCustom AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDescPokOP AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPassN     AS LOGICAL   NO-UNDO INITIAL YES.
   DEFINE VARIABLE vPassT     AS LOGICAL   NO-UNDO INITIAL YES.
   DEFINE VARIABLE vS         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vT         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI         AS INT64     NO-UNDO.


   FOR FIRST ttNalRec NO-LOCK:

      ASSIGN
         vKBKNalog  = FGetSetting("ГНИ","КБКНалог","")
         vKBKCustom = FGetSetting("ГНИ","КБКТамож","")
         vDescPokOP = GetCodeDesc("Нал:ОП",ttNalRec.PokOP,1,"").

      IF CAN-DO(vKBKNalog, ttNalRec.KBK) OR 
         CAN-DO(vKBKCustom,ttNalRec.KBK) 
      THEN vIsBudget = NO.
      ELSE RUN IsBudgetPaymentEx IN h_op(INPUT ttNalRec.ben-acct,
                                         INPUT ttNalRec.KBK,
                                         OUTPUT vIsBudget).

/*      /* бюджетный платеж */                             */
/*      IF vIsBudget AND (ttNalRec.PokOp <> "0"            */
/*                     OR ttNalRec.PokNP <> "0") THEN      */
/*         oErrorMsg =  "Для бюджетных платежей в полях " +*/
/*                      "(106) Основание платежа и " +     */
/*                      "(107) Налоговый период " +        */
/*                      "должен быть указан ~"0~"".        */

      /* налоговый платеж */
      IF vIsBudget OR (CAN-DO(vDescPokOP, "н") AND CAN-DO(vKBKNalog,ttNalRec.KBK)) THEN 
      DO: 
         IF ttNalRec.PokNP = "0" THEN
            vPassN = YES.
         ELSE 
         DO:
            IF NUM-ENTRIES(ttNalRec.PokNP, ".")      = 3 AND
               LENGTH(ENTRY(1, ttNalRec.PokNP, ".")) = 2 AND
               LENGTH(ENTRY(2, ttNalRec.PokNP, ".")) = 2 AND
               LENGTH(ENTRY(3, ttNalRec.PokNP, ".")) = 4
            THEN 
            DO:
               ASSIGN
                  vS = GetCode("Нал:НП", ENTRY(1, ttNalRec.PokNP, "."))
                  vT = ENTRY(2, ttNalRec.PokNP, ".")
               .
               IF {assigned vS} THEN 
               DO:
                  IF LENGTH(TRIM(vT)) = 2 THEN 
                  DO:
                     vI = INT64(vT) NO-ERROR.
                     vPassN = NOT ERROR-STATUS:ERROR 
                              AND vI >= 0                 
                              AND vI <= INT64(vS).
                  END.
               END.
               ELSE
                  vPassN = YES.
               vS = ttNalRec.PokNP.
               IF vPassN THEN
                  OVERLAY(vS, 1, 5, "CHARACTER") = "01.01".
               vPassN = isDate148n(vS).
            END.
            ELSE
               vPassN = NO.
         END.
         IF NOT vPassN THEN 
         DO:
            oErrorMsg = "Для налоговых платежей поле &1 должно иметь "         +
                        "значение ~"0~" либо формат ~"&2.&3.ГГГГ~". В случае," +
                        " если &2 присутствует в классификаторе &4, значение " +
                        "из этой записи классификатора определяет "            +
                        "максимальное значение &3. В противном случае формат " +
                        "поля в целом должен соответствовать корректной дате".
            oErrorMsg = SUBSTITUTE(oErrorMsg,
                                   "(107) Период",
                                   "XX",
                                   "ММ",
                                   "Нал:НП").
         END.
      END.
      /* таможенный платеж */
      ELSE IF CAN-DO(vDescPokOP, "т") AND CAN-DO(vKBKCustom,ttNalRec.KBK) THEN 
      DO:
         vPassT = isDigital148n(ttNalRec.PokNP, 8, NO).
         IF NOT vPassT THEN
            oErrorMsg = "Для таможенных платежей поле (107) Период " +
                        "должно содержать 8-значный цифровой код".
      END.

      IF NOT {assigned oErrorMsg} THEN
         oErrorMsg = check148n-kbk-pokop-poknp(ttNalRec.KBK, 
                                               ttNalRec.PokOP, 
                                               ttNalRec.PokNP).

   END.

   RETURN oErrorMsg.
END PROCEDURE.

PROCEDURE check148n-106-109:
   DEFINE INPUT  PARAMETER iPokOp     AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iPokDD     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrorText AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrorType AS CHAR NO-UNDO.
 
   DEFINE VARIABLE vNalContrVal AS CHARACTER   NO-UNDO.
   vNalContrVal = GetCode("Нал:Контр","106-109").

   IF     LOOKUP(vNalContrVal,"Да,Пр") > 0
      AND iPokOp EQ "ЗД"
      AND iPokDD NE "0"
   THEN
      ASSIGN 
         oErrorText = "Не пройден перекрестный контроль полей " +
                      "(106) Основание платежа и (109) Дата."
         oErrorType = IF vNalContrVal EQ "Да"
                      THEN "-1"
                      ELSE "4"
      .

   RETURN.
   
END PROCEDURE.
&ENDIF /* DEF_148N_I_ */
/* $LINTFILE='148n.i' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='12/10/2016 13:47:17.523+03:00' */
/*prosignD9h1obJsLsz1HMxN5MIq5Q*/