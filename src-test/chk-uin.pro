/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: CHK-UIN.PRO
      Comment: Процедуры и функции для контроля реквизита УИН для неналоговых платежей
   Parameters: Нет
         Uses:
      Used by:
      Created: 09/09/2014
*/
/* Процедура проверки документа на необходимость контроля реквизита УИН 
   Если счет получателя входит в список, указанный в НП "СчетУИН" и
   клиент дебетого счета - ЮЛ или ФЛ-Предприниматель,
   то контроль необходим.
   Дополнительно возвращает счет получателя и УИН для дальнейшей проверки ключа. */
PROCEDURE ChkOpUIN:
 DEF INPUT  PARAM rid  AS INT64 NO-UNDO.
 DEF OUTPUT PARAM oResult  AS LOG  NO-UNDO INIT NO.
 DEF OUTPUT PARAM oAcctRec AS CHAR NO-UNDO.
 DEF OUTPUT PARAM oUIN     AS CHAR NO-UNDO.

 /* Локализация буферов */
 DEF BUFFER op FOR op.
 DEF BUFFER op-entry FOR op-entry.

   DEFINE VARIABLE vCustCats AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIsIB     AS LOGICAL   NO-UNDO.

  FOR FIRST op WHERE
            op.op =  rid
    NO-LOCK:
     FIND FIRST op-entry WHERE
                op-entry.op  =  op.op
        NO-LOCK NO-ERROR.
       IF AVAIL op-entry THEN DO:
          FIND FIRST acct WHERE
                     acct.acct = op-entry.acct-db
             NO-LOCK NO-ERROR.
            IF AVAIL acct THEN DO:
     
             oAcctRec = GetXAttrValueEx("op",STRING(op.op),"acct-rec",?).
             
             
&IF DEFINED(uin-xattr-value) <> 0 &THEN
             IF mTmpacct-rec <> "" THEN
                oAcctRec = mTmpacct-rec.
&ENDIF
             IF NOT {assigned oAcctRec} THEN
                oAcctRec = op.ben-acct.
             IF NOT {assigned oAcctRec} THEN
                oAcctRec = op-entry.acct-cr.

             vCustCats = FGetSetting("ПлатДок", "КлУИН", "").
             RUN CheckIB(acct.cust-cat, acct.cust-id, OUTPUT vIsIB).

             IF (NOT {assigned vCustCats} OR
                 ((NOT CAN-DO(vCustCats, acct.cust-cat) OR
                   CAN-DO("Б,Ч,Ю", acct.cust-cat)) OR
                  (NOT CAN-DO(vCustCats, "ИП") OR vIsIB))
                ) AND
                CAN-DO(FGetSetting("ПлатДок", "СчетУИН", ""),SUBSTR(oAcctRec,1,5))
             THEN DO:
               ASSIGN
                oResult = YES
                oUIN    = GetXAttrValueEx("op",STRING(op.op),"УИН","").
&IF DEFINED(uin-xattr-value) <> 0 &THEN
             IF mTmpUin <> "" THEN
                oUIN = mTmpUIN.
&ENDIF
             END.
            END.
       END.
  END.
  
END PROCEDURE.

PROCEDURE CheckIB.
   DEFINE INPUT  PARAMETER iCustCat LIKE acct.cust-cat NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId  LIKE acct.cust-id  NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult  AS   LOGICAL       NO-UNDO.

   IF iCustCat = "Ч" THEN
      oResult = GetXAttrValueEx("person",
                                STRING(iCustId),
                                "Предпр",
                                "") BEGINS "Пред"
                OR
                GetXAttrValueEx("person",
                                STRING(iCustId),
                                "ВидИТД",
                                "") <> "".
END PROCEDURE.

/* Процедура проверки ключа реквизита УИН
   Входные параметры:
   1- Балансовый счет получателя средств
   2- УИН 
   Выходной параметр:
   Результат проверки - yes\no */
PROCEDURE ChkKey:
  DEFINE INPUT  PARAMETER iNumAcct AS CHAR NO-UNDO.
  DEFINE INPUT  PARAMETER iCodeUIN AS CHAR NO-UNDO.
  DEFINE OUTPUT PARAMETER oResult  AS LOG  NO-UNDO INIT NO. /* YES - контроль пройден */

  DEFINE VARIABLE i         AS INT64 NO-UNDO.
  DEFINE VARIABLE mSumMult  AS INT64 NO-UNDO.
  DEFINE VARIABLE mVesKoef  AS CHAR  NO-UNDO.
  DEFINE VARIABLE mStr      AS CHAR  NO-UNDO.
  DEFINE VARIABLE mIntOfStr AS INT64 NO-UNDO.

  ASSIGN
     mVesKoef = "371373713737137371373"
     mSumMult = 0
     mStr     = SUBSTR(iNumAcct,1,5).

  /* Длина УИН должна быть 25 знаков */
  IF LENGTH(iCodeUIN) <> 25 THEN
     RETURN.

  DO i = 1 TO 16:
     mStr = mStr + IF i <= LENGTH(iCodeUIN) THEN SUBSTR(iCodeUIN,i,1)
                                            ELSE "0".
  END.

  /* нужны только цифры */
  IF TRIM(mStr,"0123456789") <> "" THEN
     RETURN.

  DO i = 1 TO LENGTH(mVesKoef):
     mSumMult = mSumMult + INT64(substr(mStr, i, 1)) * INT64(substr(mVesKoef, i, 1)).
  END.

  IF (mSumMult mod 10) =  0 THEN
     oResult = YES.
END PROCEDURE.

/* Валидация значения УИН в соответствии с регулярным выражением из НП ПлатДок.УИНШаблон */
PROCEDURE Validate-UIN-regexp PRIVATE:
   DEFINE INPUT  PARAMETER iUIN    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INITIAL YES.

   DEFINE VARIABLE vRegExp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErrMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRes    AS CHARACTER NO-UNDO.

   vRegExp = FGetSetting("ПлатДок", "УИНШаблон", "").
   IF {assigned vRegExp} THEN DO:
      oResult = ereg-from-perl(vRegExp, iUIN, OUTPUT vErrMsg, OUTPUT vRes).
      IF oResult <> YES THEN DO:
         IF NOT {assigned vErrMsg} THEN
            vErrMsg = SUBSTITUTE("Значение реквизита &1 не соответствует " +
                                 "выражению валидации, указанному в НП &2",
                                 QUOTER("УИН"),
                                 "ПлатДок.УИНШаблон").
         oResult = NO.
      END.
   END.
   IF {assigned vErrMsg} THEN
      RETURN vErrMsg.
END PROCEDURE.

&SCOPED-DEFINE UIN-HOUSING-CHARS "0123456789-abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"

/* Проверка структуры значения УИН для ГИС ЖКХ */
PROCEDURE Validate-UIN-Housing PRIVATE:
   DEFINE INPUT  PARAMETER iUIN    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO.

   IF iUIN EQ "0" 
   THEN oResult = YES.
   ELSE oResult = (NOT {assigned iUIN} OR
                  (LENGTH(iUIN) = 18 AND
                   TRIM(iUIN, {&UIN-HOUSING-CHARS}) = "")).
END PROCEDURE.
/* $LINTFILE='chk-uin.pro' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.215+03:00' */
/*prosign6hvJIb1hSVWCQTtxQqJEYQ*/