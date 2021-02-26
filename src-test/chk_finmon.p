/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 ТОО "Банковские информационные системы"
*/

DEFINE PARAMETER BUFFER Op FOR op.
DEFINE INPUT  PARAMETER iParam  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER oResult AS LOGICAL NO-UNDO INIT YES.

{globals.i}
{intrface.get db2l}
{intrface.get xclass}
{intrface.get terr}

DEFINE VARIABLE mDbCli      AS INT64     NO-UNDO.
DEFINE VARIABLE mCrCli      AS INT64     NO-UNDO.
DEFINE VARIABLE mDbCat      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCrCat      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mIs550      AS INT64     NO-UNDO.
DEFINE VARIABLE mListFil    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mListTer    AS CHARACTER NO-UNDO.

DEF BUFFER op2       FOR op.
DEF BUFFER op-entry2 FOR op-entry.

DEFINE TEMP-TABLE t-obj NO-UNDO
            FIELD rec AS recid.

oResult  = YES.
mListFil = "0000,0300,0500".

RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}","Begin").

IF CAN-DO("ВКЛБ",op.op-status) THEN 
DO:
   oResult = YES.
   {intrface.del}
   RETURN.
END.

/*IF CAN-DO("40101*,40302*",op.ben-acct) THEN RETURN.*/

IF CAN-DO("40101*",op.ben-acct)  THEN RETURN.
IF NOT CAN-DO(mListFil,shFilial) THEN RETURN.

/*Пропускаем Дельта Телеком*/
mDbCat = "".
mDbCli = 0.
FOR EACH op-entry OF op
   NO-LOCK,
   EACH acct WHERE TRUE
   AND acct.acct EQ op-entry.acct-db
   NO-LOCK:
   ASSIGN
      mDbCat = acct.cust-cat
      mDbCli = acct.cust-id.
END.
IF mDbCat EQ "Ю" AND mDbCli EQ 762 THEN RETURN.

/*Begin*/

FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*",  op-entry.acct-db)
   AND CAN-DO("40817*,40820*,423*,426*",op-entry.acct-cr)
   AND op-entry.amt-rub GT 200000.00
   NO-LOCK:

   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "4"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " Внутренний перевод на физика > 200,000.00 руб").
	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM1".
	   VALIDATE op.
	   oResult = NO.
	END.
	IF oResult EQ NO THEN LEAVE.
END.

FOR EACH op-entry OF op WHERE TRUE
   AND CAN-DO("405*,406*,407*,40802*",op-entry.acct-db)
   AND CAN-DO("301*,302*,303*",       op-entry.acct-cr)
   AND op-entry.amt-rub GT 200000.00
   NO-LOCK,
   EACH op-bank OF op WHERE TRUE
   AND NOT CAN-DO("044525129,047106641,045209884",op-bank.bank-code)
   NO-LOCK:
   
   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "4"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type)
	AND CAN-DO("40817*,40820*,423*,426*",op.ben-acct)
	THEN
	DO:
	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " Внешний перевод на физика > 200,000.00 руб").
	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM2".
	   VALIDATE op.
	   oResult = NO.
	END. 
   IF oResult EQ NO THEN LEAVE.
END.

FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*",  op-entry.acct-db)
   AND CAN-DO("40817*,40820*,423*,426*",op-entry.acct-cr)
   AND op-entry.amt-rub GE 150000.00
   AND op-entry.amt-rub LE 200000.00
   NO-LOCK:
      
   FOR EACH op-entry2 WHERE TRUE
      AND op-entry2.op        NE op-entry.op
      AND op-entry2.acct-db   EQ op-entry.acct-db
      AND op-entry2.op-date   EQ op-entry.op-date
      AND CAN-DO("40817*,40820*,423*,426*",op-entry2.acct-cr)
      AND op-entry2.amt-rub   GE 150000.00
      AND op-entry2.amt-rub   LE 200000.00
      AND op-entry2.op-status GE "ФБН"
      NO-LOCK,
      EACH op2 OF op-entry2 BREAK BY op-entry2.acct-db:
      
       RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",STRING(op2.op-date,"99/99/9999") + " " + STRING(op2.op)).
      
      IF FIRST-OF(op-entry2.acct-db) THEN mCnt = 0.
      
      IF TRUE
      AND op2.acct-cat  EQ "b"
   	AND op2.order-pay GT "4"
   	AND CAN-DO("01,01КЛ,01БУМ",op2.doc-type) 
   	AND CAN-DO("40817*,40820*,423*,426*",op-entry.acct-cr)
   	THEN
   	DO:
   	   mCnt = mCnt + 1.
   	   IF mCnt GE 1 THEN
         DO:
      	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " Два и больше внутренних перевода на физика в один день").
      	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM3".
      	   VALIDATE op.
      	   oResult = NO.
      	END.
      	IF oResult EQ NO THEN LEAVE.
   	END.
   END.
END.

FOR EACH op-entry OF op WHERE TRUE
   AND CAN-DO("405*,406*,407*,40802*",op-entry.acct-db)
   AND CAN-DO("301*,302*,303*",       op-entry.acct-cr)
   AND op-entry.amt-rub GE 150000.00
   AND op-entry.amt-rub LE 200000.00
   NO-LOCK,
   EACH op-bank OF op WHERE TRUE
   AND NOT CAN-DO("044525129,047106641,045209884",op-bank.bank-code)
   NO-LOCK BREAK BY op-entry.acct-db:
      
   IF NOT CAN-DO("40817*,40820*,423*,426*",op.ben-acct) THEN NEXT.

   FOR EACH op-entry2 WHERE TRUE
      AND op-entry2.op        NE op-entry.op
      AND op-entry2.acct-db   EQ op-entry.acct-db
      AND op-entry2.amt-rub   GE 150000.00
      AND op-entry2.amt-rub   LE 200000.00
      AND op-entry2.op-status GE "ФБН"
      AND op-entry2.op-date   EQ op-entry.op-date
      NO-LOCK,
      EACH op2 OF op-entry2 BREAK BY op-entry2.acct-db:
   
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",STRING(op2.op-date,"99/99/9999") + " " + STRING(op2.op)).
   
      IF FIRST-OF(op-entry2.acct-db) THEN mCnt = 0. 
      
      IF TRUE
      AND op2.acct-cat  EQ "b"
   	AND op2.order-pay GT "4"
   	AND CAN-DO("01,01КЛ,01БУМ",op2.doc-type)
   	AND CAN-DO("40817*,40820*,423*,426*",op2.ben-acct)
   	THEN
   	DO:
   	   mCnt = mCnt + 1.
   	   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",STRING(op2.op-date,"99/99/9999") + " " + STRING(op2.op) + " " + STRING(mCnt)).
   	   IF mCnt GE 1 THEN
         DO:
      	   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " Два и больше внешних перевода на физика в один день").
      	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM4".
      	   VALIDATE op.
      	   oResult = NO.
   	   END.
   	   IF oResult EQ NO THEN LEAVE.
   	END.
   END. 
END.

FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*,40817*,40820*,423*,426*",op-entry.acct-db)
   AND op-entry.amt-rub GT 3000000.00
   NO-LOCK:
   
   mDbCli = 0.
   mDbCat = "".
   FOR EACH acct WHERE TRUE
      AND acct.acct EQ op-entry.acct-db
      AND CAN-DO("423*,426*",op-entry.acct-db)
      NO-LOCK:
      ASSIGN
         mDbCat = acct.cust-cat
         mDbCli = acct.cust-id.
   END.
   
   mCrCli = 0.
   mCrCat = "".
   FOR EACH acct WHERE TRUE
      AND acct.acct EQ op-entry.acct-cr
      AND CAN-DO("423*,426*",op-entry.acct-cr)
      NO-LOCK:
      ASSIGN
         mCrCat = acct.cust-cat
         mCrCli = acct.cust-id.
   END.
   
   /*604793*/
   mIs550 = INDEX(GetXAttrValueEx("person",STRING(mDbCli),"ОценкаРиска",""),"550").
   
   RUN dbgprint.p ("chk_finmon ","mDbCli = " + STRING(mDbCli) + " mCrCli = " + STRING(mCrCli) + " mIs550 = " + STRING(mIs550)).
   
   IF mDbCli EQ mCrCli AND mDbCli NE 0 AND mCrCli NE 0 AND mIs550 EQ 0 THEN
   DO:
      RUN dbgprint.p ("chk_finmon ",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " Перевод > 3,000,000.00 руб самому себе без Риска550.").
   END.
   ELSE
   DO:
      IF TRUE
      AND op.acct-cat  EQ "b"
   	AND op.order-pay GT "4"
   	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
   	DO:
   	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " Перевод > 3,000,000.00 руб").
   	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM5".
   	   VALIDATE op.
   	   oResult = NO.
   	END.
	END. 
   IF oResult EQ NO THEN LEAVE.
END.

/*Стоп-листы по дебету*/
FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*,40807*,40821*",op-entry.acct-db)
   NO-LOCK:
      
   mDbCli = 0.
   mDbCat = "".
   FOR EACH acct WHERE TRUE
      AND acct.acct EQ op-entry.acct-db
      NO-LOCK:
      ASSIGN
         mDbCat = acct.cust-cat
         mDbCli = acct.cust-id.
   END.
   
   FOR EACH code WHERE TRUE
      AND code.class   EQ "StopList"
      AND code.parent  EQ "StopList"
      AND code.misc[1] EQ mDbCat
      AND code.misc[2] EQ STRING(mDbCli)
      NO-LOCK:
      LEAVE.      
   END.
   
   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "4"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
      IF AVAIL(code)
      AND (INDEX(code.misc[7],"УФМ") > 0
        OR INDEX(code.misc[7],"ОФМ") > 0
        OR INDEX(code.misc[7],"КФМ") > 0) THEN
      DO:
         IF DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) NE ? AND 
            DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) LT TODAY THEN
         DO: END.
         ELSE
         DO:  
      	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " СтопЛист: " + code.misc[7]).
      	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM6".
      	   VALIDATE op.
      	   oResult = NO.
   	   END.
   	END.
      IF oResult EQ NO THEN LEAVE.
   END.
   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay EQ "3"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
      IF AVAIL(code)
      AND (INDEX(code.misc[7],"УФМ") > 0
        OR INDEX(code.misc[7],"ОФМ") > 0
        OR INDEX(code.misc[7],"КФМ") > 0) THEN
      DO:
         IF DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) NE ? AND 
            DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) LT TODAY THEN
         DO: END.
         ELSE
         DO:
            IF op-entry.amt-rub GE 10000.00 THEN
            /*IF op-entry.amt-rub GE 50000.00 THEN*/
            DO:
         	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " СтопЛист: " + code.misc[7]).
         	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM6".
         	   VALIDATE op.
         	   oResult = NO.
      	   END.
   	   END.
   	END.
      IF oResult EQ NO THEN LEAVE.
	END.
END.
/*Стоп-листы по кредиту*/
FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*,40807*,40821*",op-entry.acct-db)
   NO-LOCK:

   mDbCli = 0.
   mDbCat = "".
   FOR EACH acct WHERE TRUE
      AND acct.acct EQ op-entry.acct-cr
      NO-LOCK:
      ASSIGN
         mDbCat = acct.cust-cat
         mDbCli = acct.cust-id.
   END.
   
   FOR EACH code WHERE TRUE
      AND code.class   EQ "StopList"
      AND code.parent  EQ "StopList"
      AND code.misc[1] EQ mDbCat
      AND code.misc[2] EQ STRING(mDbCli)
      NO-LOCK:
      LEAVE.      
   END.
      
   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "4"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
      IF AVAIL(code)
      AND (INDEX(code.misc[7],"УФМ") > 0
        OR INDEX(code.misc[7],"ОФМ") > 0
        OR INDEX(code.misc[7],"КФМ") > 0) THEN
      DO:
         IF DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) NE ? AND 
            DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) LT TODAY THEN
         DO: END.
         ELSE
         DO:
      	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " СтопЛист: " + code.misc[7]).
      	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM6".
      	   VALIDATE op.
      	   oResult = NO.
   	   END.
   	END. 
      IF oResult EQ NO THEN LEAVE.
   END.
   
   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "3"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
      IF AVAIL(code)
      AND (INDEX(code.misc[7],"УФМ") > 0
        OR INDEX(code.misc[7],"ОФМ") > 0
        OR INDEX(code.misc[7],"КФМ") > 0) THEN
      DO:
         IF DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) NE ? AND 
            DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) LT TODAY THEN
         DO: END.
         ELSE
         DO:
            IF op-entry.amt-rub GE 50000.00 THEN
            DO:
         	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " СтопЛист: " + code.misc[7]).
         	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM6".
         	   VALIDATE op.
         	   oResult = NO.
      	   END.
   	   END.
   	END. 
      IF oResult EQ NO THEN LEAVE.
   END.
END.
/*Стоп-листы по ИНН*/
FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*,40807*,40821*",op-entry.acct-db)
   NO-LOCK:
      
   FOR EACH code WHERE TRUE
      AND code.class   EQ "StopList"
      AND code.parent  EQ "StopList"
      AND code.misc[5] EQ op.inn
      NO-LOCK:
      LEAVE.      
   END.

   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "4"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
      IF AVAIL(code)
      AND (INDEX(code.misc[7],"УФМ") > 0
        OR INDEX(code.misc[7],"ОФМ") > 0
        OR INDEX(code.misc[7],"КФМ") > 0) THEN
      DO:
         IF DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) NE ? AND 
            DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) LT TODAY THEN
         DO: END.
         ELSE
         DO:
      	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " СтопЛист: " + code.misc[7]).
      	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM6".
      	   VALIDATE op.
      	   oResult = NO.
   	   END.
   	END. 
      IF oResult EQ NO THEN LEAVE.
   END.
   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "3"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
      IF AVAIL(code)
      AND (INDEX(code.misc[7],"УФМ") > 0
        OR INDEX(code.misc[7],"ОФМ") > 0
        OR INDEX(code.misc[7],"КФМ") > 0) THEN
      DO:
         IF DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) NE ? AND 
            DATE(GetXattrValueEx("code",code.class + "," + code.code,"CloseDate","")) LT TODAY THEN
         DO: END.
         ELSE
         DO:
            IF op-entry.amt-rub GE 50000.00 THEN
            DO:
         	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " СтопЛист: " + code.misc[7]).
         	   ASSIGN op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM6".
         	   VALIDATE op.
         	   oResult = NO.
      	   END.
   	   END.
   	END. 
      IF oResult EQ NO THEN LEAVE.
   END.
END.

FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*,40807*,40821*",op-entry.acct-db)
   NO-LOCK:

   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "4"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
	   mDbCli = 0.
      mDbCat = "".
      FOR EACH acct WHERE TRUE
         AND acct.acct EQ op-entry.acct-db
         NO-LOCK:
         ASSIGN
            mDbCat = acct.cust-cat
            mDbCli = acct.cust-id.
      END.
      
      IF mDbCat EQ "Ю" THEN
      DO:
         FOR EACH cust-corp WHERE TRUE
            AND cust-corp.cust-id EQ mDbCli
            NO-LOCK,
            EACH signs WHERE TRUE
            AND signs.file-name EQ "cust-corp"
            AND signs.surrogate EQ STRING(cust-corp.cust-id)
            AND signs.code      EQ "АнализОФМ"
            AND signs.dec-value EQ 3
            NO-LOCK:
            LEAVE.
         END.
         IF AVAIL(signs) THEN
         DO:
            RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " АнализОФМ = 3:").
      	   IF op.op-date EQ op.ins-date THEN
      	   ASSIGN 
         	   op.op-date  = op.op-date  + 1
     	         op.due-date = op.due-date + 1.
      	   op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM7".
      	   VALIDATE op.
      	   oResult = NO.
      	END.
         IF oResult EQ NO THEN LEAVE.
      END.
      ELSE IF mDbCat EQ "Ч" THEN 
      DO:
         FOR EACH person WHERE TRUE
            AND person.person-id EQ mDbCli
            NO-LOCK,
            EACH signs WHERE TRUE
            AND signs.file-name EQ "person"
            AND signs.surrogate EQ STRING(person.person-id)
            AND signs.code      EQ "АнализОФМ"
            AND signs.dec-value EQ 3
            NO-LOCK:
            LEAVE.
         END.
         IF AVAIL(signs) THEN
         DO:
      	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " АнализОФМ = 3:").
      	   IF op.op-date EQ op.ins-date THEN
      	   ASSIGN 
         	   op.op-date  = op.op-date  + 1
     	         op.due-date = op.due-date + 1.
      	   op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM7".
      	   VALIDATE op.
      	   oResult = NO.
      	END. 
         IF oResult EQ NO THEN LEAVE.
      END.
   END.
   
   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "3"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
	   mDbCli = 0.
      mDbCat = "".
      FOR EACH acct WHERE TRUE
         AND acct.acct EQ op-entry.acct-db
         NO-LOCK:
         ASSIGN
            mDbCat = acct.cust-cat
            mDbCli = acct.cust-id.
      END.
      
      IF mDbCat EQ "Ю" THEN
      DO:
         FOR EACH cust-corp WHERE TRUE
            AND cust-corp.cust-id EQ mDbCli
            NO-LOCK,
            EACH signs WHERE TRUE
            AND signs.file-name EQ "cust-corp"
            AND signs.surrogate EQ STRING(cust-corp.cust-id)
            AND signs.code      EQ "АнализОФМ"
            AND signs.dec-value EQ 3
            NO-LOCK:
            LEAVE.
         END.
         IF AVAIL(signs) THEN
         DO:
            IF op-entry.amt-rub GE 50000.00 THEN
            DO:
               RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " АнализОФМ = 3:").
         	   IF op.op-date EQ op.ins-date THEN
         	   ASSIGN 
            	   op.op-date  = op.op-date  + 1
        	         op.due-date = op.due-date + 1.
         	   op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM7".
         	   VALIDATE op.
         	   oResult = NO.
      	   END.
      	END.
         IF oResult EQ NO THEN LEAVE.
      END.
      ELSE IF mDbCat EQ "Ч" THEN 
      DO:
         FOR EACH person WHERE TRUE
            AND person.person-id EQ mDbCli
            NO-LOCK,
            EACH signs WHERE TRUE
            AND signs.file-name EQ "person"
            AND signs.surrogate EQ STRING(person.person-id)
            AND signs.code      EQ "АнализОФМ"
            AND signs.dec-value EQ 3
            NO-LOCK:
            LEAVE.
         END.
         IF AVAIL(signs) THEN
         DO:
            IF op-entry.amt-rub GE 50000.00 THEN
            DO:
         	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " АнализОФМ = 3:").
         	   IF op.op-date EQ op.ins-date THEN
         	   ASSIGN 
            	   op.op-date  = op.op-date  + 1
        	         op.due-date = op.due-date + 1.
         	   op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM7".
         	   VALIDATE op.
         	   oResult = NO.
      	   END.
      	END. 
         IF oResult EQ NO THEN LEAVE.
      END.
   END.
   
END.

FOR EACH op-entry OF op WHERE TRUE 
   AND CAN-DO("405*,406*,407*,40802*,40807*,40821*",op-entry.acct-db)
   NO-LOCK:

   IF TRUE
   AND op.acct-cat  EQ "b"
	AND op.order-pay GT "4"
	AND CAN-DO("01,01КЛ,01БУМ",op.doc-type) THEN
	DO:
	   mDbCat = "".
      mDbCli = 0.
      FOR EACH acct WHERE TRUE
         AND acct.acct EQ op-entry.acct-db
         NO-LOCK:
         ASSIGN
            mDbCat = acct.cust-cat
            mDbCli = acct.cust-id.
      END.
      
      IF mDbCat EQ "Ю" THEN
      DO:
         FOR EACH cust-corp WHERE TRUE
            AND cust-corp.cust-id EQ mDbCli
            NO-LOCK:
            LEAVE.
         END.
         IF AVAIL(cust-corp) THEN
         DO:
            {empty t-obj}

/*            RUN CompareFast IN h_terr (cust-corp.name-corp,'plat',INPUT-OUTPUT TABLE t-obj).*/
            
            mListTer = GetXAttrValue("op",STRING(op.op),"LegTerr").
            
            IF ({assigned mListTer} AND mListTer NE "NO") OR INDEX(op.op-error,"t0") GT 0 OR CAN-FIND(FIRST t-obj) THEN
            DO:
         	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " TerrBlack:").
         	   ASSIGN 
         	      op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM8".
         	   VALIDATE op.
         	   oResult = NO.
      	   END.
      	END.
         IF oResult EQ NO THEN LEAVE.
      END.
      ELSE IF mDbCat EQ "Ч" THEN 
      DO:
         FOR EACH person WHERE TRUE
            AND person.person-id EQ mDbCli
            NO-LOCK:
            LEAVE.
         END.
         IF AVAIL(person) THEN
         DO:
            {empty t-obj}
            
/*            RUN CompareFast IN h_terr (person.name-last + " " + person.first-names,'plat',INPUT-OUTPUT TABLE t-obj).*/
            
            mListTer = GetXAttrValue("op",STRING(op.op),"LegTerr").
            
            IF ({assigned mListTer} AND mListTer NE "NO") OR INDEX(op.op-error,"t0") GT 0 OR CAN-FIND(FIRST t-obj) THEN
            DO:
         	   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",STRING(op.op-date,"99/99/9999") + " " + STRING(op.op) + " TerrBlack:").
         	   ASSIGN 
         	      op.op-error = op.op-error + (IF op.op-error NE "" THEN "," ELSE "") + "mess-error:FM8".
         	   VALIDATE op.
         	   oResult = NO.
         	END.
      	END. 
         IF oResult EQ NO THEN LEAVE.
      END.
   END.
END.

RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}","oResult = " + STRING(oResult)).

{intrface.del}
