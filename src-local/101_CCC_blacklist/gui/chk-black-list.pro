/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename:
      Comment: инструментарий для проверки  black-list
   Parameters: нет
         Uses:
      Used by:
      Created: 10/06/15 ches
     Modified: 10/06/15 ches
*/

DEFINE TEMP-TABLE tt-black-list NO-UNDO
   FIELD fio        AS CHARACTER
   FIELD typedoc    AS CHARACTER
   FIELD numdoc     AS CHARACTER
   FIELD cause      AS CHARACTER
 INDEX idxx typedoc numdoc.

/* Общая процедура запуска проверки документа */
PROCEDURE chk-black-all :
   DEFINE INPUT PARAMETER iOp     AS INT64 NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tt-black-list. /* Таблица значений */

   DEFINE VARIABLE mTypeDoc       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mNumDoc        AS CHARACTER NO-UNDO.
/* Вставка Плюс банк */
   DEFINE VARIABLE mInt           AS INT64     NO-UNDO.
   DEFINE VARIABLE mMess          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mAnswer        AS CHARACTER NO-UNDO.
/* Конец вставки Плюс банк */

   FIND FIRST op WHERE
              op.op =  iOp
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE op THEN
      RETURN.

   IF mDbgPrint EQ YES THEN
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
      "~niOp = " + STRING(iOp)).

   mTypeDoc = GetXAttrValueEx("op",STRING(op.op),"document-id","").
   mNumDoc  = GetXAttrValueEx("op",STRING(op.op),"Докум","").
   /* Вставка Плюс банк */
   mMess = "".

   IF mTypeDoc EQ "Паспорт" THEN
   DO mInt = 1 TO LENGTH(mNumDoc) - 9:
      mAnswer = SUBSTRING(mNumDoc,mInt,12).
      IF TRIM(mAnswer,"1234567890 ") EQ ""
         AND (LENGTH(TRIM(mAnswer," ")) EQ 12
           OR LENGTH(TRIM(mAnswer," ")) EQ 10) THEN mMess = mAnswer.
   END.

   IF {assigned mMess} THEN mNumDoc = mMess.
/* Конец вставки Плюс банк */
   IF {assigned mTypeDoc} AND {assigned mNumDoc} THEN
   DO:
      IF mDbgPrint EQ YES THEN
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nЗапуск chk-black" + 
         "~nmTypeDoc = " + mTypeDoc +
         "~nmNumDoc  = " + mNumDoc).
      RUN chk-black IN THIS-PROCEDURE(mTypeDoc,mNumDoc).
   END.
   /* Проверяем клиентов на счетах проводок */
   FOR EACH op-entry OF op
      NO-LOCK:
      IF NOT CAN-DO("015,02",op.doc-type) THEN
      DO:
         IF op-entry.acct-db <> ? THEN
         DO:
            IF mDbgPrint EQ YES THEN
            RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
               "~nЗапуск chk-acct" + 
               "~nop-entry.acct-db = " + op-entry.acct-db).
            RUN chk-acct IN THIS-PROCEDURE(op-entry.acct-db).
         END.
      END.
      IF NOT (op.doc-type BEGINS "01") THEN
      DO:
         IF NOT {assigned op.name-ben} THEN
         DO: 
            IF op-entry.acct-cr <> ? THEN
            DO:
               IF mDbgPrint EQ YES THEN
               RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
                  "~nЗапуск chk-acct" + 
                  "~nop-entry.acct-cr = " + op-entry.acct-cr).
               RUN chk-acct IN THIS-PROCEDURE(op-entry.acct-cr).
            END.
         END.
      END.
   END.

END PROCEDURE.

/* Процедура проверки клиента счета */
PROCEDURE chk-acct :
   DEFINE INPUT PARAMETER iAcct     AS CHARACTER NO-UNDO.
    FIND FIRST acct WHERE
               acct.acct =  iAcct
          NO-LOCK NO-ERROR.
      IF AVAILABLE acct THEN DO:
         IF acct.cust-cat =  "Ч" THEN DO:
            FIND FIRST person WHERE
                       person.person-id =  acct.cust-id
                 NO-LOCK NO-ERROR.
              IF AVAILABLE person THEN
                 RUN chk-black IN THIS-PROCEDURE(person.document-id, person.document).
         END.
      END.
END PROCEDURE.

/* Процедура проверки документа клиента */
PROCEDURE chk-black :
   DEFINE INPUT PARAMETER iTypeDoc AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iNumDoc  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCause  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNumDoc AS CHARACTER NO-UNDO .
   DEFINE VARIABLE vrecid  AS RECID NO-UNDO .
   DEFINE VARIABLE vStr    AS CHARACTER NO-UNDO.
/* Вставка Плюс банк */
   DEFINE VARIABLE vNumber AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAnswer AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vMess   AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE vChkPersDover AS CHARACTER NO-UNDO.

   IF iTypeDoc EQ "Паспорт" THEN
   DO:
      vChkPersDover = GetSysConf("ChkPersDover").
      IF mDbgPrint EQ YES THEN
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nvChkPersDover = " + vChkPersDover).
      IF vChkPersDover NE "YES" THEN
      DO:
         RUN chk-pipe IN THIS-PROCEDURE
            (INPUT  iNumDoc,
             OUTPUT vAnswer,
             OUTPUT vMess).
         IF mDbgPrint EQ YES THEN
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
            "~nvAnswer = " + vAnswer).
         IF vAnswer EQ "1" THEN 
         DO: 
            FIND FIRST tt-black-list WHERE
                       tt-black-list.typedoc   = TRIM(iTypeDoc)
                   AND tt-black-list.numdoc    = TRIM(iNumDoc)
                   NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-black-list THEN
            DO:
               CREATE tt-black-list.
               ASSIGN tt-black-list.fio   = IF AVAIL(person) THEN
                                                    (person.name-last + " " + 
                                                     person.first-names)
                                            ELSE "На основании доп.реквизитов"
                  tt-black-list.typedoc   = TRIM(iTypeDoc)
                  tt-black-list.numdoc    = TRIM(iNumDoc)
                  tt-black-list.cause     = "Паспорт недействителен.".
            END.
         END.
      END.
   END.
   ELSE DO:
/* Конец вставки Плюс банк */
   ASSIGN
      vNumDoc  = REPLACE(iNumDoc," ","")
      vStr     = SUBSTRING(vNumDoc,1,2)
      iTypeDoc = TRIM(iTypeDoc)
      vRecid   = ?
   .

   IF iTypeDoc =  "Паспорт" THEN DO:
      vStr = SUBSTRING(vNumDoc,1,10).
      FIND FIRST code WHERE code.class =  "black-list"
                        AND code.code  =  vStr
                        AND code.name  =  iTypeDoc
      NO-LOCK NO-ERROR.
      IF NOT AVAIL code THEN DO:
         vStr = SUBSTRING(vNumDoc,1,2) + " " + SUBSTRING(vNumDoc,3,2) + " " + SUBSTRING(vNumDoc,5,6).
         FIND FIRST code WHERE code.class =  "black-list"
                           AND code.code  =  vStr
                           AND code.name  =  iTypeDoc
         NO-LOCK NO-ERROR.
      END.
      IF AVAIL code THEN
         vrecid  = recid(code).
   END.
   ELSE
      IF FGetSetting("black-list", "КонтрТипДок", "") <> "Паспорт" THEN
         FOR EACH    code WHERE
                     code.class      =  "black-list"
                 AND code.code   BEGINS vStr
                 AND code.name       =  iTypeDoc
                NO-LOCK :
             IF vNumDoc BEGINS REPLACE(code.code," ","") THEN DO:
               vrecid  = recid(code) .
               LEAVE.
             END.
         END.

   IF vRecid <> ? THEN
      FIND FIRST code where recid(code) = vrecid NO-LOCK NO-ERROR .

   IF AVAILABLE code THEN DO:
   FIND FIRST tt-black-list WHERE
               tt-black-list.typedoc   = TRIM(code.name)
            AND tt-black-list.numdoc    = TRIM(code.code)
            NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tt-black-list THEN DO:
         CREATE tt-black-list.
         ASSIGN tt-black-list.fio       = IF AVAIL(person) THEN
                                 (person.name-last + " " + person.first-names)
                                          ELSE ""
               tt-black-list.typedoc   = TRIM(code.name)
               tt-black-list.numdoc    = TRIM(code.code)
               tt-black-list.cause     = TRIM(code.val).
      END.
   END.
/* Вставка Плюс банк */
   END.
/* Конец вставки Плюс банк */
END PROCEDURE.
/* Вставка Плюс банк */
/*
Процедура проверки документа клиента in pipe
В схеме IGP создана процедура на боевом сервере БИС.
IGP.Is_Passp_Wanted_P
В случае нахождения паспорта в розыске возвращает 1, иначе - 0.
Пример: 65 11 109019
*/
PROCEDURE chk-pipe:
   DEFINE INPUT  PARAMETER iNumber AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oAnswer AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess   AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE mHandle  AS INT64     NO-UNDO.
   DEFINE VARIABLE iStat    AS INT64     NO-UNDO.

   DEFINE VARIABLE vTmpNum  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSeria   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNumber  AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE P_SERIES AS CHARACTER NO-UNDO.
   DEFINE VARIABLE P_NUMBER AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vInt     AS INT64     NO-UNDO.

   iNumber = REPLACE(iNumber," ","").
   DO vInt = 1 TO LENGTH(iNumber) - 9:
      vTmpNum = SUBSTRING(iNumber,vInt,10).
      IF TRIM(vTmpNum,"1234567890") EQ ""
         AND LENGTH(TRIM(vTmpNum)) EQ 10 THEN
      DO:
         ASSIGN
            vSeria  = SUBSTRING(vTmpNum,1,4)
            vNumber = SUBSTRING(vTmpNum,5,6).
         LEAVE.
      END.
   END.

   RUN STORED-PROCEDURE IS_PASSP_WANTED_P mHandle = PROC-HANDLE
      (
      INPUT  PARAM P_SERIES = vSeria,   /*"5208",*/
      INPUT  PARAM P_NUMBER = vNumber,  /*"628886",*/
      OUTPUT PARAM P_REZULT = ?         /*-1*/
      ).
   CLOSE STORED-PROC IS_PASSP_WANTED_P iStat = PROC-STATUS.

   IF iStat = 0 THEN
   ASSIGN
      oAnswer = STRING(P_REZULT)
      oMess   = "".
   ELSE
   ASSIGN
      oAnswer = STRING(P_REZULT)
      oMess   = ERROR-STATUS:GET-MESSAGE(1).

   IF mDbgPrint EQ YES THEN      
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nvSeria  = " + vSeria +
         "~nvNumber = " + vNumber +
         "~noAnswer = " + oAnswer + 
         "~noMess   = " + oMess).

END PROCEDURE.
/* Конец вставки Плюс банк */
/* $LINTFILE='chk-black-list.pro' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.202+03:00' */
/*prosignY42w8H6V8GG1/CSJeM9aSg*/