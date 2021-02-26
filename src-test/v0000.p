{globals.i}
{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{intrface.get blkob}
{parsin.def}
{sh-defs.i}
   
DEFINE VARIABLE mFilial AS CHARACTER NO-UNDO.

DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mNum   AS INT64     NO-UNDO.
DEFINE VARIABLE mNom   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymb  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFIO   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate  AS DATE      NO-UNDO.
DEFINE VARIABLE mAmt1  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAmt3  AS DECIMAL NO-UNDO.

DEFINE VARIABLE mContCode AS CHARACTER NO-UNDO.

DEFINE BUFFER b-acct FOR acct.
DEFINE BUFFER b-loan FOR loan.

ETIME(YES).

DEFINE VARIABLE mNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMess   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAnswer AS CHARACTER NO-UNDO.

DEFINE VARIABLE mBlSumm  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mListBlk AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrDb  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrCr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpList  AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-acct NO-UNDO
   FIELD acct AS CHARACTER
   FIELD indb AS DECIMAL
   FIELD incr AS DECIMAL.

mInt = 0.

/*FIND FIRST acct WHERE                           */
/*   acct.acct EQ "99999978405000090902     @0500"*/
/*EXCLUSIVE-LOCK NO-ERROR.                        */
/*                                                */
/*IF AVAIL(acct) THEN                             */
/*   ASSIGN acct.currency = "".                   */
/*                                                */
/*RETURN.                                         */


DEFINE VARIABLE mSumm1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumm2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumm3 AS DECIMAL NO-UNDO.

DEFINE VARIABLE mSumAcctOstIN1  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumAcctOstOUT1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumAcctOstIN2  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumAcctOstOUT2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAcctOstIN      AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAcctOstOUT     AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstInDb     AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstInCr     AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstOutDb    AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstOutCr    AS DECIMAL NO-UNDO.

/*
/*Есть остаток по закрытому счету*/
FOR EACH acct WHERE
       acct.close-date LT mDate
   AND acct.close-date GE (mDate - 10)
   NO-LOCK,
   LAST acct-pos WHERE
       acct-pos.acct     EQ acct.acct
   AND acct-pos.currency EQ acct.currency
/*   AND acct-pos.since    LT (mDate - 1)*/
   AND acct-pos.filial-id EQ "0500"
   NO-LOCK BY acct-pos.since:
         
   IF acct-pos.balance NE 0 THEN
      PUT UNFORMATTED "acct-pos.balance NE 0;" 
         acct.acct ";" 
         acct-pos.balance ";" 
         mDate ";" 
         acct.close-date SKIP.
END.

RETURN.
*/


/*Поиск отсутствующего счета*/

{getdate.i}

mDate = end-date.

{setdest.i &filename = "'v1.log'"}

PUT UNFORMATTED "beg" SKIP.



DO mInt2 = 1 TO 5:
mDate = mDate + 1.

mInt   = 0.
mSumm1 = 0.
mSumm2 = 0.
mSumm3 = 0.

PUT UNFORMATTED STRING(mDate,"99/99/9999") "   ----------------" SKIP.

mFilial = "0000".

/*Счета проводок*/
FOR EACH op WHERE
       op.filial-id EQ mFilial
   AND op.op-date EQ mDate
   AND op.acct-cat EQ "o"
   NO-LOCK,
   EACH op-entry OF op
   NO-LOCK: 

/*   IF op-entry.acct-db EQ ? OR op-entry.acct-cr EQ ? THEN*/
/*      PUT UNFORMATTED                                    */
/*         "acct = ?;"                                     */
/*         op.op ";"                                       */
/*         op.filial-id ";"                                */
/*         op.doc-date ";"                                 */
/*         op.doc-num ";"                                  */
/*         op.doc-type ";"                                 */
/*         op-entry.acct-db ";"                            */
/*         op-entry.acct-cr ";"                            */
/*         op.order-pay ";"                                */
/*         op.details ";"                                  */
/*         op-entry.amt-rub                                */
/*      SKIP.                                              */

   {find-act.i &acct = op-entry.acct-db}

   IF acct.acct-cat NE "o" THEN NEXT.
   
   IF NOT AVAIL(acct) 
   AND op-entry.acct-db NE ? THEN
   
      PUT UNFORMATTED
         "Db Not Found;" 
         op.op ";"
         op.filial-id ";"
         op.doc-date ";"
         op.doc-num ";"
         op.doc-type ";"
         op-entry.acct-db ";"
         op.order-pay ";"
         op.details ";"
         op-entry.amt-rub
      SKIP.

   IF AVAIL(acct) 
   AND acct.filial-id NE mFilial THEN
   
      PUT UNFORMATTED
         "DbFil <> " mFilial ";" 
         acct.acct ";"
         acct.open-date ";"
         acct.close-date ";"
         acct.side ";"
         op-entry.amt-rub
      SKIP.
   
   IF AVAIL(acct)
   AND acct.open-date GT mDate THEN
   
      PUT UNFORMATTED
         "DbDateOpen > " mDate ";" 
         acct.acct ";"
         acct.open-date ";"
         acct.close-date ";"
         acct.side ";"
         op-entry.amt-rub
      SKIP.

   IF AVAIL(acct)
   AND acct.close-date NE ?
   AND acct.close-date LT mDate THEN
   
      PUT UNFORMATTED
         "DbDateClose < " mDate ";" 
         mDate ";"
         acct.acct ";"
         acct.open-date ";"
         acct.close-date ";"
         acct.side ";"
         op-entry.amt-rub
      SKIP.

   IF AVAIL(acct) THEN mCurrDb = acct.currency.
   
   {find-act.i &acct = op-entry.acct-cr}
 
   IF NOT AVAIL(acct) 
   AND op-entry.acct-cr NE ? THEN
   
      PUT UNFORMATTED
         "Cr Not Found;"
         op.op ";"
         op.filial-id ";"
         op.doc-date ";"
         op.doc-num ";"
         op.doc-type ";"
         op-entry.acct-cr ";"
         op.order-pay ";"
         op.details ";"
         op-entry.amt-rub
      SKIP.
   
   IF AVAIL(acct) 
   AND acct.filial-id NE mFilial THEN
   
      PUT UNFORMATTED
         "CrFil <> " mFilial ";" 
         acct.acct ";"
         acct.open-date ";"
         acct.close-date ";"
         acct.side ";"
         op-entry.amt-rub
      SKIP.

   IF AVAIL(acct)
   AND acct.open-date GT mDate THEN
   
      PUT UNFORMATTED
         "CrDateOpen > " mDate ";" 
         acct.acct ";"
         acct.open-date ";"
         acct.close-date ";"
         acct.side ";"
         op-entry.amt-rub
      SKIP.
      
   IF AVAIL(acct)
   AND acct.close-date NE ?
   AND acct.close-date LT mDate THEN
   
      PUT UNFORMATTED
         "CrDateClose < " mDate ";"
         mDate ";"
         acct.acct ";"
         acct.open-date ";" 
         acct.close-date ";"
         acct.side ";"
         op-entry.amt-rub
      SKIP.
   
   IF AVAIL(acct) THEN mCurrCr = acct.currency.
   
   IF (mCurrDB NE ""
   OR mCurrCr NE "")
   AND op-entry.currency EQ "" THEN

      PUT UNFORMATTED
         "op.currency NE acct.currency ;"
         op.op ";"
         op.filial-id ";"
         op.doc-date ";"
         op.doc-num ";"
         op.doc-type ";"
         op-entry.acct-db ";"
         op-entry.acct-cr ";"
         op.order-pay ";"
         op.details ";"
         op-entry.amt-rub ";"
         op-entry.amt-cur ";"
      SKIP.
END.

END.


PUT UNFORMATTED "end" SKIP.

{preview.i &filename = "'v1.log'"}

RETURN.


/*FOR EACH op-entry WHERE                                       */
/*       op-entry.filial-id EQ "0500"                           */
/*   AND op-entry.op EQ 26240135                                */
/*   EXCLUSIVE-LOCK:                                            */
/*                                                              */
/*   ASSIGN op-entry.acct-db = "40817810604000010257     @0500".*/
/*                                                              */
/*END.                                                          */

/*INPUT FROM VALUE("acct.csv").   */
/*REPEAT:                         */
/*   mInt = mInt + 1.             */
/*	IMPORT UNFORMATTED mString.    */
/*	IF mString EQ "end" THEN LEAVE.*/
/*                                */
/*	CREATE tt-acct.                */
/*	ASSIGN tt-acct.acct = mString. */
/*                                */
/*END.                            */
/*                                */
/*MESSAGE "Загружены"             */
/*VIEW-AS ALERT-BOX.              */

/* Если клиент создаем УНКг, иначе удаляем */
/*IF IsSubjClient("Ю",cust-corp.cust-id) THEN                      */
/*   unkg = UNKg("cust-corp",STRING(cust-corp.cust-id)).           */
/*ELSE                                                             */
/*   UpdateSigns("cust-corp",STRING(cust-corp.cust-id),'УНКг',?,?).*/


/*
mDate = DATE("20/05/2016").

DEFINE VARIABLE mSumm1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumm2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumm3 AS DECIMAL NO-UNDO.

DEFINE VARIABLE mSumAcctOstIN1  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumAcctOstOUT1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumAcctOstIN2  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumAcctOstOUT2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAcctOstIN      AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAcctOstOUT     AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstInDb     AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstInCr     AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstOutDb    AS DECIMAL NO-UNDO.
DEFINE VARIABLE mSumOstOutCr    AS DECIMAL NO-UNDO.

mSumm1 = 0.
mSumm2 = 0.
mSumm3 = 0.

mInt   = 0.
mDate = DATE("20/05/2016").
mSumOstInDb  = 0.
mSumOstInCr  = 0.
mSumOstOutDb = 0.
mSumOstOutCr = 0.

FOR EACH acct WHERE
   acct.acct-cat EQ "o"
   AND  acct.filial-id EQ "0500"
   AND (acct.close-date EQ ? OR acct.close-date GE mDate - 1)
   AND acct.open-date  LE mDate
   NO-LOCK:

   RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).
   
   CREATE tt-acct.
   ASSIGN
      tt-acct.acct = acct.acct.
      
   IF sh-in-bal > 0 THEN
      ASSIGN
         tt-acct.indb = sh-in-bal
         mSumOstInDb  = mSumOstInDb  + sh-in-bal.
   IF sh-in-bal < 0 THEN 
      ASSIGN
         tt-acct.incr = sh-in-bal
         mSumOstInCr  = mSumOstInCr  + sh-in-bal.
   IF sh-bal > 0 THEN 
      mSumOstOutDb  = mSumOstOutDb  + sh-bal.
   IF sh-bal < 0 THEN 
      mSumOstOutCr  = mSumOstOutCr  + sh-bal.
END.

PUT UNFORMATTED mInt ";" mSumOstInDb  SKIP.
PUT UNFORMATTED mInt ";" mSumOstInCr  SKIP.
PUT UNFORMATTED mInt ";" mSumOstOutDb SKIP.
PUT UNFORMATTED mInt ";" mSumOstOutCr SKIP.
*/

/*
mInt   = 0.
mDate = DATE("20/05/2016").
mSumOstInDb  = 0.
mSumOstInCr  = 0.
mSumOstOutDb = 0.
mSumOstOutCr = 0.

FOR EACH acct WHERE
   acct.acct-cat EQ "o"
   AND  acct.filial-id EQ "0500"
   AND (acct.close-date EQ ? OR acct.close-date GE mDate)
   AND acct.open-date  LE mDate
   NO-LOCK:

   RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).
   
   FIND FIRST tt-acct WHERE
      tt-acct.acct EQ acct.acct
   NO-LOCK NO-ERROR.
   
   IF AVAIL(tt-acct) THEN
   DO:
      IF sh-in-bal > 0 THEN
      DO:
         IF sh-in-bal NE tt-acct.indb THEN
            PUT UNFORMATTED acct.acct ";" sh-in-bal ";" tt-acct.indb SKIP.
         mSumOstInDb  = mSumOstInDb  + sh-in-bal.
      END.
      IF sh-in-bal < 0 THEN 
      DO:
         IF sh-in-bal NE tt-acct.incr THEN
            PUT UNFORMATTED acct.acct ";" sh-in-bal ";" tt-acct.incr SKIP.
         mSumOstInCr  = mSumOstInCr  + sh-in-bal.
      END.
   END.
   ELSE
   DO:
      IF sh-in-bal > 0 THEN
      DO:
         PUT UNFORMATTED acct.acct ";" sh-in-bal SKIP.
         mSumOstInDb  = mSumOstInDb  + sh-in-bal.
      END.
      IF sh-in-bal < 0 THEN 
      DO:
         PUT UNFORMATTED acct.acct ";" sh-in-bal SKIP.
         mSumOstInCr  = mSumOstInCr  + sh-in-bal.
      END.
   END.
END.

PUT UNFORMATTED mInt ";" mSumOstInDb  SKIP.
PUT UNFORMATTED mInt ";" mSumOstInCr  SKIP.
PUT UNFORMATTED mInt ";" mSumOstOutDb SKIP.
PUT UNFORMATTED mInt ";" mSumOstOutCr SKIP.
*/


/*
mInt   = 0.
mDate = DATE("20/05/2016").
mSumOstInDb  = 0.
mSumOstInCr  = 0.
mSumOstOutDb = 0.
mSumOstOutCr = 0.

FOR EACH acct WHERE
   acct.acct-cat EQ "o"
   AND  acct.filial-id EQ "0500"
   AND (acct.close-date EQ ? OR acct.close-date GE mDate)
   AND acct.open-date  LE mDate
   NO-LOCK:

   RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).
   
   IF sh-in-bal > 0 THEN 
      mSumOstInDb  = mSumOstInDb  + sh-in-bal.
   IF sh-in-bal < 0 THEN 
      mSumOstInCr  = mSumOstInCr  + sh-in-bal.
   IF sh-bal > 0 THEN 
      mSumOstOutDb  = mSumOstOutDb  + sh-bal.
   IF sh-bal < 0 THEN 
      mSumOstOutCr  = mSumOstOutCr  + sh-bal.
END.
*/




/*
FOR EACH acct WHERE
   acct.acct-cat EQ "o"
   AND  acct.filial-id EQ "0500"
/*   AND  acct.acct EQ mNumber*/
/*   AND (acct.close-date EQ ? OR acct.close-date GE mDate)*/
/*   AND acct.close-date EQ ?*/
/*   AND acct.close-date GT mDate*/
/*   AND acct.open-date  GE DATE("01/05/2016")*/
   AND acct.open-date  LE mDate
   NO-LOCK:

/*   RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).*/
/*                                                                 */
/*   mInt           = mInt + 1.                                    */
/*   mSumAcctOstIN  = mSumAcctOstIN  + sh-in-bal.                  */
/*   mSumAcctOstOUT = mSumAcctOstOUT + sh-bal.                     */
      
   CREATE tt-acct.
   ASSIGN tt-acct.acct = acct.acct.

END.

PUT UNFORMATTED mInt ";" mDate ";" mSumAcctOstIN ";" mSumAcctOstOUT SKIP.

mInt   = 0.
mDate = DATE("20/05/2016").
mSumAcctOstIN = 0.
mSumAcctOstOUT = 0.

FOR EACH acct WHERE
   acct.acct-cat EQ "o"
   AND  acct.filial-id EQ "0500"
   AND (acct.close-date EQ ? OR acct.close-date GE mDate)
/*   AND acct.close-date EQ ?*/
/*   AND acct.close-date GT mDate*/
/*   AND acct.open-date  GE DATE("01/05/2016")*/
   AND acct.open-date  LE mDate
   NO-LOCK:
      
/*   RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).*/

   mInt           = mInt + 1.
   mSumAcctOstIN  = mSumAcctOstIN  + sh-in-bal.
   mSumAcctOstOUT = mSumAcctOstOUT + sh-bal.
   
   FIND FIRST tt-acct WHERE
      tt-acct.acct EQ acct.acct
   NO-LOCK NO-ERROR.

   IF NOT AVAIL(tt-acct) THEN
   DO:

/*      RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).    */
/*      mSumAcctOstIN  = mSumAcctOstIN  + sh-in-bal.                      */
/*      mSumAcctOstOUT = mSumAcctOstOUT + sh-bal.                         */

      PUT UNFORMATTED acct.acct ";" acct.open-date ";" acct.close-date ";" sh-in-bal ";" sh-bal SKIP.
   END.

END.

PUT UNFORMATTED mInt ";" mDate ";" mSumAcctOstIN ";" mSumAcctOstOUT SKIP.
*/

/*
mSumAcctOstIN = 0.
mSumAcctOstOUT = 0.
mNumber = "90901810605811010152     @0500".

FOR EACH acct WHERE
   acct.acct-cat EQ "o"
   AND  acct.filial-id EQ "0500"
   AND  acct.acct NE mNumber
/*   AND (acct.close-date EQ ? OR acct.close-date GE mDate)*/
/*   AND acct.close-date EQ ?*/
/*   AND acct.close-date GT mDate*/
   AND acct.open-date  LE mDate
   AND acct.open-date  GE DATE("01/01/2015")
   NO-LOCK:
   
   mInt = mInt + 1.   

   RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).

/*   IF acct.close-date EQ mDate THEN                                                                  */
/*      PUT UNFORMATTED acct.acct ";" acct.open-date ";" acct.close-date ";" sh-in-bal ";" sh-bal SKIP.*/
   
   mSumAcctOstIN  = mSumAcctOstIN  + sh-in-bal.
   mSumAcctOstOUT = mSumAcctOstOUT + sh-bal.

END.

PUT UNFORMATTED mInt ";" mDate ";" mSumm1 ";" mSumm2 ";" mSumm3 ";" mSumAcctOstIN ";" mSumAcctOstOUT SKIP.
*/

/*
mSumAcctOstIN = 0.
mSumAcctOstOUT = 0.

FOR EACH acct WHERE
   acct.acct-cat EQ "o"
   AND  acct.filial-id EQ "0500"
   AND (acct.close-date EQ ? OR acct.close-date GE mDate)
/*   AND acct.close-date EQ ?*/
/*   AND acct.close-date GT mDate*/
   AND acct.open-date  LE mDate
   NO-LOCK:
   
   mInt = mInt + 1.   

   RUN acct-pos IN h_base(acct.acct,acct.currency,mDate,mDate,?).

   IF acct.close-date EQ mDate THEN
      PUT UNFORMATTED acct.acct ";" acct.open-date ";" acct.close-date ";" sh-in-bal ";" sh-bal SKIP.
   
   mSumAcctOstIN  = mSumAcctOstIN  + sh-in-bal.
   mSumAcctOstOUT = mSumAcctOstOUT + sh-bal.

END.

PUT UNFORMATTED mInt ";" mDate ";" mSumm1 ";" mSumm2 ";" mSumm3 ";" mSumAcctOstIN ";" mSumAcctOstOUT SKIP.
*/

/*
FOR EACH acct-pos WHERE
       acct-pos.acct-cat  EQ "o"
   AND acct-pos.filial-id EQ "0500"
   AND acct-pos.since     EQ mDate
   AND NOT (acct-pos.acct BEGINS("9999"))
   NO-LOCK:
   
   mInt = mInt + 1.
   PUT UNFORMATTED acct-pos.balance SKIP.
   mSumm3 = mSumm3 + acct-pos.balance.   
   IF acct-pos.balance GT 0
   THEN mSumm1 = mSumm1 + acct-pos.balance.
   ELSE mSumm2 = mSumm2 + (-1 * acct-pos.balance).

END.
*/



/*                                                  */
/*FOR EACH acct-cur                                 */
/*   WHERE                                          */
/*       acct-cur.filial-id EQ "0500"               */
/*   AND acct-cur.acct-cat  EQ "o"                  */
/*   AND acct-cur.since     EQ mDate                */
/*   NO-LOCK:                                       */
/*                                                  */
/*PUT UNFORMATTED acct-cur.balance SKIP.            */
/*                                                  */
/*   IF acct-cur.balance GT 0                       */
/*   THEN mSumm1 = mSumm1 + acct-cur.balance.       */
/*   ELSE mSumm2 = mSumm2 + (-1 * acct-cur.balance).*/
/*                                                  */
/*END.                                              */

/*PUT UNFORMATTED mSumm1 ";" mSumm2 SKIP.*/



/*


/*                                             */
/*                                             */
/*FOR EACH op WHERE                            */
/*   op.op EQ 27629450                         */
/*   EXCLUSIVE-LOCK:                           */
/*                                             */
/*                                             */
/*                                             */
/*   ASSIGN op.doc-num = "1".                  */
/*                                             */
/*   PUT UNFORMATTED op.op ";" op.doc-num SKIP.*/
/*                                             */
/*END.                                         */

/*FOR EACH op-entry WHERE             */
/*       op-entry.filial-id EQ "0500" */
/*   AND op-entry.op EQ 20726809      */
/*   EXCLUSIVE-LOCK:                  */
/*                                    */
/*   ASSIGN op-entry.currency = "398".*/
/*                                    */
/*END.                                */

/*
FOR EACH cust-role WHERE
       cust-role.file-name  NE "loan"
   AND cust-role.cust-cat   EQ "Ч"
   AND cust-role.Class-Code EQ "ImaginClient"
   AND cust-role.surrogate  EQ shFilial
   NO-LOCK,
   FIRST person WHERE
   person.person-id EQ INT64(cust-role.cust-id)
   NO-LOCK:
*/      


/*      
   FIND FIRST person WHERE
      person.person-id EQ 227051
   NO-LOCK NO-ERROR.
         
   FOR EACH history WHERE
/*       history.modify EQ "W"*/
       history.modify NE "RUN"
   AND history.modify NE "PRINT"
   AND history.modify NE "SAVE"
   AND NOT (history.FIELD-ref BEGINS "_system_")
   AND history.field-ref EQ STRING(227051)             /*STRING(person.person-id)*/
   AND history.file-name EQ "person"
   AND CAN-DO('*first-names*,*name-last*',history.field-value)
   NO-LOCK BY history.modif-date BY history.modif-time:
      
      LEAVE.
   END.

      mInt = mInt + 1.
      IF AVAIL(history) THEN
      DO:   
      PUT UNFORMATTED
         
/*         mInt ";"                                 */
/*         cust-role.file-name ";"                  */
/*         cust-role.Class-Code ";"                 */
/*         cust-role.surrogate ";"                  */
/*         cust-role.cust-id ";"                    */
/*         cust-role.short-name ";"                 */
         history.modify ";"
         history.modif-date ";"
         STRING(history.modif-time,"hh:mm:ss") ";"
         "person.first-names = " person.first-names ";"
         "person.name-last = " person.name-last ";"
         history.FIELD-ref ";"
         history.FIELD-value
      SKIP.
   


      DO mNum = 1 TO NUM-ENTRIES(history.field-value) / 2:
         IF LEFT-TRIM(ENTRY(2 * mNum - 1,history.field-value),"*") EQ "first-names"
         OR LEFT-TRIM(ENTRY(2 * mNum - 1,history.field-value),"*") EQ "name-last"
         THEN 
            PUT UNFORMATTED
   	         /*CAN-DO("op-status",LEFT-TRIM(ENTRY(2 * mNum - 1,history.field-value),"*"))*/
   	         
               LEFT-TRIM(ENTRY(2 * mNum - 1,history.field-value),"*") ";"
               LEFT-TRIM(ENTRY(2 * mNum,history.field-value),"*")
               
            SKIP.
      END.
      END.

*/
   
/*   END.*/


/*   
   FIND FIRST history WHERE history.file-name = 'op' ^
                                    AND history.field-ref = string(cop.op) ^
                                    AND history.field-val MATCHES "*op-status*" ^
                                    AND history.modify = '{&hi-w}' ^
               NO-LOCK NO-ERROR.^
               IF AVAIL history^
                  THEN vStatus = ENTRY(LOOKUP("op-status",history.field-val) + 1,history.field-value).^
                  ELSE vStatus = cop.op-status.  /* Статус документа не менялся */^
   
   
   vPos = LOOKUP("State",history.field-value).^
      IF vPos GT 0 THEN DO:^
         vState = ENTRY(vPos + 1,history.field-value).

   IF CAN-DO(history.field-value,"*msfo-acct")


      ^
^
         IF NOT CAN-DO("op-status",LEFT-TRIM(ENTRY(2 * mNum - 1,history.field-value),"*")) OR CAN-DO(iParam,op.op-status) THEN DO:^

*/

/*   
END.   
*/
   
/*   
FOR EACH history WHERE
       history.modify NE "RUN"
   AND history.modify NE "PRINT"
   AND history.modify NE "SAVE"
   AND NOT (history.FIELD-ref BEGINS "_system_")
/*   AND history.field-ref EQ '675303'*/
   AND history.file-name EQ 'person'
   AND CAN-DO('*first-names*,*name-last*',history.field-value)
   NO-LOCK:
   
      
   PUT UNFORMATTED
      history.modify ";"
      history.modif-date ";"
      STRING(history.modif-time,"hh:mm:ss") ";"
      history.FIELD-ref ";"
      history.FIELD-value
   SKIP.      
      
END.      
*/



/*   26795049*/
/*   26809504*/

/*14270386*/
/*14270405*/


/*
/*mOpList = "5434745,17242638,5958954,17242580,5036087,17242639,6594400,17242581,6223490,17242637".  5*/

/*mOpList = "7364217,17242578,10370632,17242579" 4.*/

mOpList = "".

DO mInt = 1 TO NUM-ENTRIES(mOpList,","):       
   FOR EACH op WHERE
      op.op EQ INT64(ENTRY(mInt,mOpList))
      EXCLUSIVE-LOCK:
   
      ASSIGN op.order-pay = "4".
   END.
END.
*/

/*FOR EACH op WHERE                                                                                                                                                                                                                                                                       */
/*   op.op EQ 27228874                                                                                                                                                                                                                                                                    */
/*   EXCLUSIVE-LOCK:                                                                                                                                                                                                                                                                      */
/*                                                                                                                                                                                                                                                                                        */
/*   ASSIGN op.details = "Исправительная проводка по м/о № 7816944839 от 01.04.2016. Перечисление денежных средств, поступивших через систему Золотая Корона, согласно реестра №68103069 от 31/03/2016 09:24:51 г. Отправитель средств МИЛЮКОВА ЕКАТЕРИНА ЛЬВОВНА согласно СЗ № ОПО.281.".*/
/*                                                                                                                                                                                                                                                                                        */
/*END.                                                                                                                                                                                                                                                                                    */

/*FOR EACH op-entry WHERE             */
/*       op-entry.filial-id EQ "0500" */
/*   AND op-entry.op EQ 17390374      */
/*   EXCLUSIVE-LOCK:                  */
/*                                    */
/*   ASSIGN op-entry.currency = "398".*/
/*                                    */
/*END.                                */

FOR EACH op WHERE
          op.filial-id EQ "0300"
      AND op.order-pay EQ "4"
      AND op.doc-type NE "037"
      AND op.doc-type NE "032"
      NO-LOCK,
      EACH op-entry OF op
      NO-LOCK:
   
      PUT UNFORMATTED
         op.filial-id ";"
         op.doc-date ";"
         op.doc-num ";"
         op.doc-type ";"
         op-entry.acct-db ";"
         op-entry.acct-cr ";"
         op.order-pay ";"
         op.details ";"
      SKIP.
   
/*      ASSIGN op.order-pay = "3".*/
   END.

FOR EACH op-entry WHERE
       op-entry.filial-id EQ "0500"
   AND op-entry.op EQ 14938016
   EXCLUSIVE-LOCK:

   ASSIGN op-entry.currency = "840".

END.


FOR EACH op WHERE
   op.op EQ 26809504
   EXCLUSIVE-LOCK:

   ASSIGN op.order-pay = "1".

END.

   mAcct = "40702978304000005218     @0000".
      
   {find-act.i
	    &acct = mAcct  
	}
    
   IF AVAIL(acct) THEN      
   DO:
            mBlSumm = GetBlockPosition(acct.acct, 
                                 acct.currency, 
                                 "*",
		            	            gend-date).

      mListBlk = BlockAcct(acct.acct + "," + acct.currency,DATETIME(TODAY,MTIME)).

      PUT UNFORMATTED
         mBlSumm ";"
         mListBlk ";"
      SKIP.   


/*                           DATETIME(date(gend-date) + 1) - 1).*/

/*
         IF mOAmt                       NE 0 AND
            ( LOOKUP("Блок",    mListBlk) NE 0 OR 
              LOOKUP("БлокКр",  mListBlk) NE 0 OR
              LOOKUP("БлокДб",  mListBlk) NE 0 OR
              LOOKUP("БлокСумм",mListBlk) NE 0 AND
/*              mAmt LE abs(mbl-pos)             AND   */
*/


FOR each history WHERE
   1 = 1
   AND history.modify NE "RUN"
   AND history.modify NE "PRINT"
   AND NOT history.FIELD-ref BEGINS "_system_"
   AND history.modify NE "SAVE"
   AND history.modif-date EQ DATE("04/04/2016")
/*   AND history.field-ref EQ '15872'*/
/*   AND CAN-DO('*15872*',history.field-value)*/
   AND CAN-DO('*40702840700000005034*',history.field-value)
   AND history.file-name EQ 'acct'
   NO-LOCK QUERY-TUNING(NO-INDEX-HINT):

   PUT UNFORMATTED
      history.modify ";"
      history.modif-date ";"
      STRING(history.modif-time,"hh:mm:ss") ";"
      history.FIELD-ref ";"
      history.FIELD-value
   SKIP.

END.



/*
FOR EACH history WHERE 1 = 1
/*       history.modify EQ "C"*/
/*   AND history.modify NE "RUN"                */
/*   AND history.modify NE "PRINT"              */
/*   AND NOT history.FIELD-ref BEGINS "_system_"*/
/*   AND history.modify NE "SAVE"               */
   AND history.file-name EQ 'op-entry'
   AND history.modif-date = DATE("22/07/2015") 
   /*AND CAN-DO('*42306810100000231007*',history.field-value)*/ 
   NO-LOCK QUERY-TUNING(NO-INDEX-HINT):

/*            BY history.file-name DESC               */
/*            BY history.modif-date DESC              */
/*            BY history.modif-time DESC              */
/*            BY (IF history.modify = "C"  THEN 1 ELSE*/
/*            (IF history.modify = "W"  THEN 2 ELSE   */
/*            (IF history.modify = "D"  THEN 3 ELSE   */
/*            (IF history.modify = "L"  THEN 4 ELSE 0 */
/*            ) ) ) ) QUERY-TUNING(NO-INDEX-HINT):    */


   IF CAN-DO('*42306810100000231007*',history.field-value) THEN
      PUT UNFORMATTED 
         history.modify ";"
         history.field-value ";"
         history.user-id ";"
         history.file-name ";"
         history.modif-date ";"
         STRING(history.modif-time,"hh:mm:ss") ";"
      SKIP.
   IF CAN-DO('*42306810000000331007*',history.field-value) THEN
      PUT UNFORMATTED 
         history.modify ";"
         history.field-value ";"
         history.user-id ";"
         history.file-name ";"
         history.modif-date ";"
         STRING(history.modif-time,"hh:mm:ss") ";"
      SKIP.

/*42306810000000331007*/
/*42306810100000231007*/

END.
*/



FOR EACH loan WHERE
       loan.contract EQ 'dps'
   AND loan.close-date EQ ?
   AND loan.cust-cat EQ "Ч"
/*   AND loan.cust-id EQ 612516*/
/*   AND loan.cont-code EQ "42301810401090000971@0000"*/
/*   AND CAN-DO("0300,0301,0302",loan.filial-id)*/
   NO-LOCK:

   IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"corr-acct","") NE "" THEN
   


mNumber = "5208628886".
mNumber = "паспорт гражданина РФ 60 14 587192 МЕЖРАЙОННЫМ ОТДЕЛОМ УФМС РОССИИ ПО РОСТОВСКОЙ ОБЛ. В СТ. РОМАНОВСКАЯ,610-025 26/06/2014".

FOR each code WHERE
       code.class  EQ 'CFO' 
   AND code.parent EQ 'CFO'EXCLUSIVE-LOCK:

   DELETE code. 
END.
return.

{setdest.i &file-name = "111.log"}

PUT UNFORMATTED mNumber ";" LENGTH(mNumber) SKIP.

DO mInt = 1 TO LENGTH(mNumber) - 9:  
   mAnswer = SUBSTRING(mNumber,mInt,12).
   IF TRIM(mAnswer,"1234567890 ") EQ "" 
      AND (LENGTH(TRIM(mAnswer," ")) EQ 12 
        OR LENGTH(TRIM(mAnswer," ")) EQ 10) THEN mMess = mAnswer. 
END.

PUT UNFORMATTED "mMess = " mMess SKIP.

/*                                                 */
/*MESSAGE mNumber ";" TRIM(mNumber,"1234567890 ")  */
/*VIEW-AS ALERT-BOX.                               */




/*RUN chk-pipe IN THIS-PROCEDURE*/
/*   (INPUT  mNumber,           */
/*    OUTPUT mAnswer,           */
/*    OUTPUT mMess).            */
/*                              */
/*MESSAGE mAnswer ";" mMess     */
/*VIEW-AS ALERT-BOX.            */


{preview.i &file-name = "111.log"}


*/




/*
В схеме IGP создана оберточная функция на боевом сервере БИС.

IGP.Is_Passp_Wanted (p_series varchar2, p_number varchar2) return pls_integer

В случае нахождения паспорта в розыске возвращает 1, иначе - 0.

Для проверки паспортов.
Права на запуск пользователю QBIS даны.
65 11 109019



DEFINE VARIABLE mHandle AS INT64     NO-UNDO.
DEFINE VARIABLE mSeria  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMess   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAnswer AS CHARACTER NO-UNDO.

mSeria = "65 11".
mNumber = "109019". 
/*" + mSeria + "," + mNumber + "*/
RUN STORED-PROC send-sql-statement mHandle = PROC-HANDLE
     ("SELECT IGP.IS_PASSP_WANTED('" + mSeria + "','" + mNumber + "') FROM dual").

FOR EACH proc-text-buffer WHERE PROC-HANDLE = mHandle:
   MESSAGE proc-text
   VIEW-AS ALERT-BOX.
   mAnswer = proc-text.
END.
IF ERROR-STATUS:ERROR
THEN DO:
   MESSAGE "Не доступен классификатор недействительных паспортов"
   VIEW-AS ALERT-BOX.
  mMess = "Не доступен классификатор недействительных паспортов".
END.
ELSE CLOSE STORED-PROCEDURE send-sql-statement WHERE PROC-HANDLE = mHandle.
*/



/*
DEFINE VAR intvar AS INTEGER.
DEFINE VAR handle1 AS INTEGER.


/*
В схеме IGP создана оберточная функция на боевом сервере БИС.

IGP.Is_Passp_Wanted (p_series varchar2, p_number varchar2) return pls_integer

В случае нахождения паспорта в розыске возвращает 1, иначе - 0.

Для проверки паспортов.
Права на запуск пользователю QBIS даны.



mSeria = "5203".
mNumber = "123456". 
*/

RUN STORED-PROC send-sql-statement handle1 = PROC-HANDLE
     ("SELECT IGP.IS_PASSP_WANTED('5203', '123999') from dual").

FOR EACH proc-text-buffer WHERE PROC-HANDLE = handle1:
   MESSAGE proc-text
   VIEW-AS ALERT-BOX.
END.
IF ERROR-STATUS:ERROR
THEN DO:
   MESSAGE "Не доступен классификатор недействительных паспортов" 
   VIEW-AS ALERT-BOX.
END.
ELSE CLOSE STORED-PROCEDURE send-sql-statement WHERE PROC-HANDLE = handle1.
*/



/*                                                                                                                 */
/*RUN STORED-PROCEDURE IGP.Is_Passp_Wanted intvar = PROC-HANDLE (INPUT PARAM "5205",INPUT PARAM "167768") NO-ERROR.*/
/*FOR EACH proc-text-buffer WHERE PROC-HANDLE = intvar:                                                            */
/*  DISPLAY proc-text-buffer.                                                                                      */
/*END.                                                                                                             */
/*IF ERROR-STATUS:ERROR                                                                                            */
/*THEN DO:                                                                                                         */
/*  MESSAGE "Stored Procedure failed to run".                                                                      */
/*END.                                                                                                             */
/*ELSE CLOSE STORED-PROCEDURE IGP.Is_Passp_Wanted WHERE PROC-HANDLE = intvar.*/



/*PROCEDURE chk-pipe:                                                                     */
/*   DEFINE INPUT PARAMETER iNumber  AS CHARACTER   NO-UNDO.                              */
/*   DEFINE OUTPUT PARAMETER oAnswer AS CHARACTER   NO-UNDO.                              */
/*   DEFINE OUTPUT PARAMETER oMess   AS CHARACTER   NO-UNDO.                              */
/*                                                                                        */
/*   DEFINE VARIABLE mHandle AS INT64     NO-UNDO.                                        */
/*   DEFINE VARIABLE vSerNum AS CHARACTER NO-UNDO.                                        */
/*   DEFINE VARIABLE vSeria  AS CHARACTER NO-UNDO.                                        */
/*   DEFINE VARIABLE vNumber AS CHARACTER NO-UNDO.                                        */
/*                                                                                        */
/*   vSerNum = REPLACE(iNumber," ","").                                                   */
/*   IF LENGTH(vSerNum) EQ 10 THEN                                                        */
/*   DO:                                                                                  */
/*      ASSIGN                                                                            */
/*         vSeria  = SUBSTRING(vSerNum,1,4)                                               */
/*         vNumber = SUBSTRING(vSerNum,5,6).                                              */
/*      RUN STORED-PROCEDURE send-sql-statement mHandle = PROC-HANDLE                     */
/*           ("SELECT IGP.IS_PASSP_WANTED('" + vSeria + "','" + vNumber + "') FROM dual").*/
/*                                                                                        */
/*      FOR EACH proc-text-buffer WHERE PROC-HANDLE = mHandle:                            */
/*         ASSIGN                                                                         */
/*            oAnswer = TRIM(proc-text)                                                   */
/*            oMess   = "".                                                               */
/*      END.                                                                              */
/*      IF ERROR-STATUS:ERROR                                                             */
/*      THEN                                                                              */
/*      ASSIGN                                                                            */
/*         oAnswer = ""                                                                   */
/*         oMess   = "Не доступен Справочник недействительных паспортов.".                */
/*      ELSE CLOSE STORED-PROCEDURE send-sql-statement WHERE PROC-HANDLE = mHandle.       */
/*   END.                                                                                 */
/*   ELSE                                                                                 */
/*   ASSIGN                                                                               */
/*      oAnswer = ""                                                                      */
/*      oMess   = "Неправильное количество символов в номере паспорта.".                  */
/*END PROCEDURE.                                                                          */
         
/*
EXCLUSIVE-LOCK:      
КредРасч
*/



/*
ON 'F1':U OF vCustID IN FRAME edit DO:
   ASSIGN vCustCat.
   CASE vCustCat:
      WHEN "Ю" THEN
         RUN browseld.p ("cust-corp",                              /* Класс объекта                */
                         "crClass-Code", "*",                      /* Коды. Поля для предустановки */
                         ?,                                        /* Поля для блокировки          */
                         4).                                       /* Строка отображения фрейма    */
      WHEN "Ч" THEN 
         RUN browseld.p ("person",                                 /* Класс объекта                */
                         "crClass-Code" + CHR(1) + "SetFirstFrm",  /* Коды. Поля для предустановки */
                         "*" + CHR(1) + "1",     
                         ?,                                        /* Поля для блокировки          */
                         4).                                       /* Строка отображения фрейма    */
      WHEN "Б" THEN RUN bank-cli.p (4).
      OTHERWISE DO:
         APPLY "entry" TO vCustCat.
         RETURN NO-APPLY.
      END.
   END CASE.
   IF KEYFUNCTION (LASTKEY) NE "end-error" AND
      pick-value            NE ?           AND
      RefreshClientData (vCustCat,INT64 (pick-value))
   THEN
      vCustID = INT64 (pick-value).
   RETURN NO-APPLY.
END.

DEFINE VARIABLE mCustID AS INT64 NO-UNDO.

RUN browseld.p ("person",                                 /* Класс объекта                */
                         "",  /* Коды. Поля для предустановки */
                         "",    
                         ?,                                        /* Поля для блокировки          */
                         4).                                       /* Строка отображения фрейма    */
                         
IF KEYFUNCTION (LASTKEY) NE "end-error" AND
   pick-value            NE ? THEN
   mCustID = INT64(pick-value).

*/


/*
{tmpobj.def}

FOR EACH loan WHERE
      (loan.close-date EQ ? OR 
       loan.close-date GE DATE("23/11/2015"))
   AND loan.contract   EQ 'КРЕДИТ' 
   AND loan.cust-cat   EQ 'Ч' 
   AND loan.cust-id    EQ 510971 
   AND loan.filial-id  EQ shFilial
   NO-LOCK,
   LAST loan-acct WHERE
        loan-acct.contract  EQ loan.contract
   AND  loan-acct.cont-code EQ loan.cont-code
   AND  loan-acct.acct-type EQ "КредРасч"
   AND  loan-acct.since     LE loan.since
/*   AND  loan-acct.acct BEGINS "40817"*/
   NO-LOCK,
   FIRST acct WHERE
       acct.acct     EQ loan-acct.acct
   AND acct.currency EQ loan-acct.currency
   NO-LOCK:

   /*      
   /* поиск расчетного счета - роль счета 'КредРасч'*/
   FIND LAST loan-acct
      AND loan-acct.acct-type eq "КредРасч"
      AND loan-acct.since le loan.since
   NO-LOCK NO-ERROR.      
   */
   
   FIND FIRST term-obl WHERE
       term-obl.contract  EQ loan.contract
   AND term-obl.cont-code EQ loan.cont-code
   AND term-obl.end-date  GE TODAY
   AND term-obl.idnt      EQ 1
   NO-LOCK NO-ERROR.
   
   IF AVAIL(term-obl) THEN mAmt1 = term-obl.amt-rub.
   
   FIND FIRST term-obl WHERE
       term-obl.contract  EQ loan.contract
   AND term-obl.cont-code EQ loan.cont-code
   AND term-obl.end-date  GE TODAY
   AND term-obl.idnt      EQ 3
   NO-LOCK NO-ERROR.
   
   IF AVAIL(term-obl) THEN mAmt3 = term-obl.amt-rub.
   
   MESSAGE mAmt1 + mAmt3  
   VIEW-AS ALERT-BOX.
      
   FIND FIRST TmpObj WHERE
      TmpObj.rid = RECID(acct) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TmpObj THEN
   DO:
     CREATE TmpObj.
     TmpObj.rid = RECID(acct).
   END.
       
   PUT UNFORMATTED 
      loan.cont-code ";" loan-acct.acct ";" loan-acct.acct-type ";" acct.acct
   SKIP.
         
END.      


FOR EACH TmpObj NO-LOCK:

   PUT UNFORMATTED 
      STRING(TmpObj.rid) 
   SKIP.
   
END.

*/

/*FOR EACH employees NO-LOCK:*/
/*                           */
/*   PUT UNFORMATTED         */
/*      employees.USERID_ ";"*/
/*      employees.USER_NAME  */
/*   SKIP.                   */
/*                           */
/*END.                       */
/*                           */
/*
{preview.i &file-name = "111.log"}

mTmpObjHand = TEMP-TABLE TmpObj:HANDLE.

MESSAGE pick-value
VIEW-AS ALERT-BOX.                

RETURN.
*/

/*FOR EACH kau WHERE                          */
/*   kau.acct EQ '90902810606600010287  @0300'*/
/*   AND kau.currency EQ ''                   */
/*   AND kau.zero-bal EQ no                   */
/*   NO-LOCK,                                 */
/*   FIRST op WHERE op.op EQ INT64(ENTRY(1,   */
/*   kau.kau)) NO-LOCK:                       */


/*
DEFINE VARIABLE vI1           AS INT64     NO-UNDO.
DEFINE VARIABLE vI2           AS INT64     NO-UNDO.
DEFINE VARIABLE vBalIskl1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vBalIskl2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vIAcct        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vIOch         AS CHARACTER NO-UNDO.
DEFINE VARIABLE out_Result    AS LOGICAL NO-UNDO.

ASSIGN
   vBalIskl1 = FGetSetting("ГНИ","bal-iskl","").
vBalIskl1 = "40302*|4;40402*|3".   
IF {assigned vBalIskl1} THEN
DO:
   DO vI1 = 1 TO NUM-ENTRIES(vBalIskl1,";"):
      vBalIskl2 = ENTRY(vI1,vBalIskl1,";").
      vIAcct    = ENTRY(1,vBalIskl2,"|").
      IF     {assigned vIAcct} 
         AND (  CAN-DO(vIAcct,"40302810100000000001")
             OR CAN-DO(vIAcct,"40302810100000000001")
         ) THEN
      DO:
         vIOch = ENTRY(2,vBalIskl2,"|").
         out_Result = YES.
         DO vI2 = 1 TO NUM-ENTRIES(vIOch,","):
            IF ENTRY(vI2,vIOch) EQ "4" THEN 
            DO:
               out_Result = NO.
               MESSAGE 1 out_Result
               VIEW-AS ALERT-BOX. 
               RETURN.
            END.
         END.
      END.
   END.
END.

MESSAGE 2 out_Result
VIEW-AS ALERT-BOX.
*/

/*
FIND FIRST Packet where
   Packet.PacketID EQ 13081502
   EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL(Packet) THEN
   ASSIGN Packet.State = "ОПТР".
*/

/*
FOR EACH op WHERE
       op.op-date EQ DATE("27/10/2015")
   AND op.doc-num BEGINS "400"
   NO-LOCK,
   EACH op-entry OF op WHERE
        op-entry.amt-rub EQ 18.88
   NO-LOCK:
      
/*   ASSIGN op.details = REPLACE(op.details,"0,","Жуков Иван Анатольевич,").*/
   
   PUT UNFORMATTED op.op ";" op.doc-num ";" op-entry.amt-rub SKIP.
         
END.

/*FOR EACH op WHERE               */
/*   op.op EQ 19355581            */
/*   EXCLUSIVE-LOCK:              */
/*                                */
/*   ASSIGN op.doc-num = "400/1г".*/
/*                                */
/*END.                            */
*/

/*FOR EACH _user NO-LOCK:               */
/*                                      */
/*   PUT UNFORMATTED _user._userid SKIP.*/
/*                                      */
/*END.                                  */

/*FOR EACH acct WHERE                                                                                                          */
/*   CAN-DO("407*,40802*,40807*",acct.acct)                                                                                    */
/*/*   AND acct.filial-id EQ "0000"*/                                                                                          */
/*   AND acct.close-date EQ ? NO-LOCK,                                                                                         */
/*   EACH b-acct WHERE                                                                                                         */
/*      b-acct.bal-acct EQ 47426                                                                                               */
/*      AND b-acct.cust-cat EQ acct.cust-cat                                                                                   */
/*      AND b-acct.cust-id  EQ acct.cust-id                                                                                    */
/*      AND b-acct.close-date EQ ? NO-LOCK:                                                                                    */
/*                                                                                                                             */
/*      PUT UNFORMATTED acct.acct ";" b-acct.acct ";" acct.filial-id ";" acct.cust-cat ";" acct.cust-id ";" ETIME " msec" SKIP.*/
/*                                                                                                                             */
/*END.                                                                                                                         */

/*
mDate = DATE("06/11/2015").
mContCode = "99-АПК".

IF CAN-DO( "*-АП*",mContCode) THEN
DO:
   IF mDate GE DATE("05/11/2015") THEN 
   DO:
     MESSAGE 3 ";" 11000
     VIEW-AS ALERT-BOX.
   END.
   ELSE
   DO:
      MESSAGE 2.5 ";" 9500
      VIEW-AS ALERT-BOX.
   END.
END.
ELSE
DO:
   MESSAGE 2.5 ";" 9500
   VIEW-AS ALERT-BOX.
END.

*/

/*
FOR EACH loan WHERE
          loan.contract EQ "dps"
      AND loan.filial-id EQ "0300"
      AND CAN-DO("*",loan.cont-code)
      AND CAN-DO("*@0300*",loan.doc-ref)
      AND loan.cont-type NE "ДВ"
      NO-LOCK:
/*      EXCLUSIVE-LOCK:*/
         
/*      ASSIGN loan.doc-ref = "50-26/55-765".*/
      
      PUT UNFORMATTED /*loan.contract ";"*/ loan.cont-code ";" loan.doc-ref ";" loan.cont-type ";" loan.loan-status SKIP.

   END.
*/

/*
FOR EACH acct WHERE
   acct.acct BEGINS "4"
   NO-LOCK:
      
   PUT UNFORMATTED acct.acct ";" ETIME " msec" SKIP.
      
END.      

/*DO mInt = 1 to 50000:                                 */
/*                                                      */
/*   FIND FIRST acct WHERE                              */
/*   acct.acct EQ "47423810000000005224     @0000"      */
/*   NO-LOCK NO-ERROR.                                  */
/*                                                      */
/*   PUT UNFORMATTED AVAIL(acct) ";" ETIME " msec" SKIP.*/
/*                                                      */
/*END.                                                  */
*/

/*
FOR EACH loan WHERE
       loan.contract EQ 'dps'
   AND loan.close-date EQ ?
   AND loan.cust-cat EQ "Ч"
/*   AND loan.cust-id EQ 612516*/
   AND loan.cont-code EQ "1-617727/RUB@0300"
   AND CAN-DO("0300,0301,0302",loan.filial-id)
   EXCLUSIVE-LOCK:
      
   ASSIGN loan.cust-id = 108174.
   
   PUT UNFORMATTED loan.cust-id ";" loan.contract ";" loan.cont-code SKIP.
   
END.
*/


/*
FOR EACH acct WHERE
   acct.acct BEGINS "4"
   NO-LOCK:
      
   PUT UNFORMATTED acct.acct ";" ETIME " msec" SKIP.
      
END.      

/*DO mInt = 1 to 50000:                                 */
/*                                                      */
/*   FIND FIRST acct WHERE                              */
/*   acct.acct EQ "47423810000000005224     @0000"      */
/*   NO-LOCK NO-ERROR.                                  */
/*                                                      */
/*   PUT UNFORMATTED AVAIL(acct) ";" ETIME " msec" SKIP.*/
/*                                                      */
/*END.                                                  */
*/


/*FOR each signs WHERE                                      */
/*       signs.file-name EQ 'loan'                          */
/*   AND signs.code eq 'drower-id'                          */
/*   AND signs.dec-value = 115874                           */
/*   NO-LOCK,                                               */
/*   each loan WHERE                                        */
/*       loan.contract EQ 'proxy'                           */
/*   AND loan.filial-id EQ "0300"                           */
/*   AND loan.contract EQ entry(1,signs.surrogate)          */
/*   AND loan.cont-code EQ entry(2,signs.surrogate) NO-LOCK:*/



/*
FOR   each loan WHERE
       loan.contract EQ 'proxy'
   AND loan.filial-id EQ "0300"
   AND loan.cont-code EQ "PROXY0000000000012312" EXCLUSIVE-LOCK:
      ASSIGN
         loan.close-date = ?
         loan.loan-status = "ДСТВ".
/*   AND loan.cont-code EQ "PROXY0000000000013538" EXCLUSIVE-LOCK:*/
/*   ASSIGN loan.doc-num = "96-36/427/464903".                    */
   
   PUT UNFORMATTED loan.cust-id ";" loan.contract ";" loan.cont-code ";" loan.loan-status ";" loan.doc-num ";" loan.open-date ";" loan.close-date SKIP.    
      
END.
*/

/*
/*          
В √    ВБО   521236    от клиента: Мякшина Юлия Владимировна, 19130925
В √    ВБО   521237    от клиента: Камалтдинов Фанис Габделахатович, 19130926
В √    ВБО   521238    от клиента: Скороспелов Евгений Васильевич 19130927
В √    ВБО   521239    от клиента: Айвазов Алексей Османович 19130928
В √    ВБО   521240    от клиента: Олейник Владимир Владимирович 19130929
В √    ВБО   521241    от клиента: Жуков Иван Анатольевич 19130930
*/

FOR EACH op WHERE
   op.op EQ 19130930
   EXCLUSIVE-LOCK:
      
   ASSIGN op.details = REPLACE(op.details,"0,","Жуков Иван Анатольевич,").      
END.
*/

/*
FOR EACH loan WHERE
       loan.contract EQ 'dps'
   AND loan.close-date EQ ?
   AND loan.cust-cat EQ "Ч"
/*   AND loan.cust-id EQ 612516*/
   AND loan.cont-code EQ "42301810103010012516@0300"
   AND CAN-DO("0300,0301,0302",loan.filial-id)
   EXCLUSIVE-LOCK:
      
   ASSIGN loan.cust-id = 486340.
   
   PUT UNFORMATTED loan.cust-id ";" loan.contract ";" loan.cont-code SKIP.
   
END.      
*/

/*
DISABLE TRIGGERS FOR LOAD OF kau.

for each kau where kau.kau eq "17242247,1" exclusive-lock:
   kau.zero-bal = yes.
end.

for each kau where kau.kau eq "17242247,1" no-lock:
   PUT UNFORMATTED kau.kau ";" kau.zero-bal SKIP.
end.
*/

/*
FIND FIRST Packet where
   Packet.PacketID EQ 12205922
   EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL(Packet) THEN
   ASSIGN Packet.State = "ОТПР".
*/

/*
DISABLE TRIGGERS FOR LOAD OF kau.

/*for each kau where kau.kau eq "17242431,1" exclusive-lock:*/
/*   kau.zero-bal = yes.                                    */
/*   PUT UNFORMATTED kau.kau ";" kau.zero-bal SKIP.         */
/*end.                                                      */

for each kau where kau.kau eq "17242431,1" no-lock:
/*   kau.zero-bal = yes.*/
   PUT UNFORMATTED kau.kau ";" kau.zero-bal SKIP.
end.
*/

/*
mInt = 0.
FOR EACH acct WHERE
   CAN-DO("40701*,40702*,40703*,40802*,40821*",acct.acct)
   AND acct.filial-id EQ "0000"
   AND acct.close-date EQ ? NO-LOCK,
   EACH BlockObject WHERE
       BlockObject.class-code   EQ "BlockAcct"
   AND BlockObject.file-name    EQ "acct"
   AND BlockObject.surrogate    EQ acct.acct + "," + acct.currency
   AND BlockObject.end-datetime EQ ?
   AND BlockObject.txt[1]       EQ "1,2,3,4"
   AND BlockObject.beg-datetime LE NOW NO-LOCK
   BREAK BY acct.acct:
   
   IF FIRST-OF(acct.acct) THEN
   DO:
      mInt = mInt + 1.
      PUT UNFORMATTED mInt ";" acct.acct SKIP.
   END.
END.
*/


/*FIND FIRST Packet where         */
/*   Packet.PacketID EQ 12205917  */
/*   EXCLUSIVE-LOCK NO-ERROR.     */
/*                                */
/*IF AVAIL(Packet) THEN           */
/*   ASSIGN Packet.State = "ОТПР".*/

/*
MESSAGE STRING(DATETIME(DATE("13.01.2015"),32328 * 1000),"99/99/99 HH:MM:SS") 
VIEW-AS ALERT-BOX.
*/
/*
FOR EACH cust-ident WHERE
       cust-ident.class-code EQ 'p-cust-ident'
   AND cust-ident.cust-cat  EQ 'Ч' 
   AND cust-ident.cust-id   EQ 175318
   AND cust-ident.cust-code EQ "65 02 924149" EXCLUSIVE-LOCK:
   
   PUT UNFORMATTED cust-ident.cust-code ";" cust-ident.close-date SKIP.
   ASSIGN cust-ident.close-date = DATE("29/08/2015").
   PUT UNFORMATTED cust-ident.cust-code ";" cust-ident.close-date SKIP.   
END. 
*/

/*FOR each history WHERE                          */
/*   (                                            */
/*       history.modify NE "RUN"                  */
/*   AND history.modify NE "PRINT"                */
/*   AND NOT history.FIELD-ref BEGINS "_system_"  */
/*   AND history.modify NE "SAVE"                 */
/*   AND history.field-ref EQ '18840301'          */
/*   AND history.file-name EQ 'op'                */
/*   )                                            */
/*   OR                                           */
/*   (                                            */
/*       history.modify NE "RUN"                  */
/*   AND history.modify NE "PRINT"                */
/*   AND NOT history.FIELD-ref BEGINS "_system_"  */
/*   AND history.modify NE "SAVE"                 */
/*   AND history.field-ref BEGINS '18840301,'     */
/*   AND history.file-name eq 'op-entry'          */
/*   )                                            */
/*   OR                                           */
/*   (                                            */
/*       history.modify NE "RUN"                  */
/*   AND history.modify NE "PRINT"                */
/*   AND NOT history.FIELD-ref BEGINS "_system_"  */
/*   AND history.modify NE "SAVE"                 */
/*   AND history.field-ref BEGINS '18840301,'     */
/*   AND history.file-name eq 'op-bank' ) NO-LOCK:*/
/*                                                */
/*   PUT UNFORMATTED                              */
/*      history.modify ";"                        */
/*      history.modif-date ";"                    */
/*      history.modif-time ";"                    */
/*      history.FIELD-ref                         */
/*   SKIP.                                        */
/*                                                */
/*END.                                            */




/*FOR EACH history                                                                                 */
/*   WHERE history.modify NE "RUN"                                                                 */
/*   AND history.modify NE "PRINT"                                                                 */
/*   AND NOT history.FIELD-ref BEGINS "_system_"                                                   */
/*   AND history.modify NE "SAVE"                                                                  */
/*   AND history.file-name EQ 'op-date' NO-LOCK                                                    */
/*            BY history.file-name DESC                                                            */
/*            BY history.modif-date DESC                                                           */
/*            BY history.modif-time DESC                                                           */
/*            BY (IF history.modify = "C"  THEN 1 ELSE                                             */
/*            (IF history.modify = "W"  THEN 2 ELSE                                                */
/*            (IF history.modify = "D"  THEN 3 ELSE                                                */
/*            (IF history.modify = "L"  THEN 4 ELSE 0                                              */
/*            ) ) ) ) QUERY-TUNING(NO-INDEX-HINT):                                                 */
/*   IF CAN-DO('*Закрытие опердня*', history.field-value) THEN                                     */
/*   DO:                                                                                           */
/*      /* MESSAGE STRING( DATE(iTime) ) + ' ' + STRING( history.field-value) view-as alert-box. */*/
/*      cldt = DATETIME( history.modif-date, history.modif-time).                                  */
/*      LEAVE.                                                                                     */
/*   END.                                                                                          */
/*END.                                                                                             */





/*FOR EACH user-proc /*WHERE                                                                                           */
/*   user-proc.parent EQ 117213*/ NO-LOCK:                                                                             */
/*                                                                                                                     */
/*   PUT UNFORMATTED user-proc.module ";" user-proc.name-proc ";" user-proc.procedure ";" user-proc.public-number SKIP.*/
/*                                                                                                                     */
/*END.                                                                                                                 */
      
/*FOR EACH kau WHERE                                                    */
/*       kau.acct BEGINS '90902810400000002502'                         */
/*   AND kau.currency EQ ''                                             */
/*   AND kau.zero-bal EQ no EXCLUSIVE-LOCK:                             */
/*/*,                                                                   */
/*   FIRST op WHERE op.op EQ INT64(ENTRY(1,kau.kau)) NO-LOCK:*/         */
/*   ASSIGN kau.balance = 1800.                                         */
/*   PUT UNFORMATTED kau.acct ";" kau.zero-bal ";" kau.balance ";" SKIP.*/
/*                                                                      */
/*END.                                                                  */



/*

/*
CODE = 'Карт2ВнСчет'
AND FILE_NAME = 'acct'
 AND XATTR_VALUE = '90902810304000001010     @0000,'
 AND SURROGATE = '40702810004000002491     @0000,'
*/

FOR EACH op WHERE
   op.op EQ 11959940
   NO-LOCK:
      
    MESSAGE op.op ";" op.doc-num ";" op.filial-id
    VIEW-AS ALERT-BOX.
      
END.

FOR EACH signs WHERE
         signs.code      EQ "amt-rub"
   AND   signs.file-name EQ "op"
   AND   signs.xattr-value  EQ "90902810304000001010     @0000,"
   AND   signs.surrogate EQ "40702810004000002491     @0000,"
NO-LOCK:
   MESSAGE 1
   VIEW-AS ALERT-BOX.
   LEAVE.
END.      

MESSAGE "end" 
    VIEW-AS ALERT-BOX.

return.

FOR EACH signs WHERE
         signs.code      EQ "Карт2ВнСчет"
   AND   signs.file-name EQ "acct"
   AND   signs.xattr-value  EQ "90902810304000001010     @0000,"
   AND   signs.surrogate EQ "40702810004000002491     @0000,"
NO-LOCK:
   MESSAGE 1
   VIEW-AS ALERT-BOX.
   LEAVE.
END.

MESSAGE 0
VIEW-AS ALERT-BOX.

return.                  
*/


/*FOR EACH loan WHERE                           */
/*       loan.contract EQ 'dps'                 */
/*   AND loan.close-date EQ ?                   */
/*   AND loan.cust-cat EQ "Ч"                   */
/*   AND loan.cust-id EQ 173049                 */
/*   AND CAN-DO("0300,0301,0302",loan.filial-id)*/
/*   NO-LOCK,                                   */
/*   EACH loan-acct WHERE                       */
/*        loan-acct.contract  EQ loan.contract  */
/*   AND  loan-acct.cont-code EQ loan.cont-code */
/*   AND  loan-acct.acct BEGINS "42301"         */
/*   NO-LOCK:                                   */
/*                                              */
/*   PUT UNFORMATTED                            */
/*      loan.contract  ";"                      */
/*      loan.cont-code ";"                      */
/*      loan.cust-cat ";"                       */
/*      loan.cust-id ";"                        */
/*      loan-acct.acct ";"                      */
/*   SKIP.                                      */
/*                                              */
/*END.                                          */

/*DEFINE VARIABLE mAcct      AS CHARACTER NO-UNDO.*/
/*DEFINE VARIABLE mContCodes AS CHARACTER NO-UNDO.*/
/*DEFINE VARIABLE mPerson    AS CHARACTER NO-UNDO.*/
/*DEFINE VARIABLE mCID       AS CHARACTER NO-UNDO.*/
/*DEFINE VARIABLE mPID       AS CHARACTER NO-UNDO.*/

/*FOR EACH loan-acct WHERE                                                                                                                     */
/*   loan-acct.acct BEGINS "42301"                                                                                                             */
/*   NO-LOCK,                                                                                                                                  */
/*   EACH loan WHERE                                                                                                                           */
/*       loan.contract EQ 'dps'                                                                                                                */
/*   AND loan.contract  EQ loan-acct.contract                                                                                                  */
/*   AND loan.cont-code EQ loan-acct.cont-code                                                                                                 */
/*   AND loan.close-date EQ ?                                                                                                                  */
/*/*   AND loan.cust-cat EQ "Ч"  */                                                                                                            */
/*/*   AND loan.cust-id EQ 173049*/                                                                                                            */
/*   AND CAN-DO("0300,0301,0302",loan.filial-id)                                                                                               */
/*   NO-LOCK,                                                                                                                                  */
/*   FIRST person WHERE                                                                                                                        */
/*      person.person-id EQ loan.cust-id                                                                                                       */
/*   NO-LOCK BREAK BY loan-acct.acct:                                                                                                          */
/*                                                                                                                                             */
/*   IF FIRST-OF(loan-acct.acct) THEN                                                                                                          */
/*   ASSIGN                                                                                                                                    */
/*      mAcct     = loan-acct.acct                                                                                                             */
/*      mContCodes = ""                                                                                                                        */
/*      mPerson = person.name-last + " " + person.first-names.                                                                                 */
/*                                                                                                                                             */
/*   mContCodes = IF mContCodes EQ "" THEN loan.cont-type + "=" + loan.cont-code ELSE mContCodes + ";" + loan.cont-type + "=" + loan.cont-code.*/
/*                                                                                                                                             */
/*   IF LAST-OF(loan-acct.acct) THEN                                                                                                           */
/*      PUT UNFORMATTED                                                                                                                        */
/*         mPerson ";" mAcct ";" mContCodes                                                                                                    */
/*      SKIP.                                                                                                                                  */
/*                                                                                                                                             */
/*END.                                                                                                                                         */

/*
mCID = "329251,331320,331624,332291,348465,351104,351661,352344,352902,353832,354102,355918,356517,360358,397305,405878,408855,411200,411845,412693,414836,415979,422813,428814,433332,440201,440316,446449,451259,467753,469169,481147,493641,501969,523321,524595,533037,540969,566528,572036,573091,573520,576654,577494".
mPID = "169819,555022,121831,169981,170346,170428,170458,170484,175192,170564,170572,170658,170676,170834,171113,171305,171423,171561,171607,171615,171747,171803,176379,172479,172759,173049,176411,112860,113486,116286,116443,91412,97978,104534,140505,163056,236511,484479,528991,538434,540436,541222,546989,548532".

DO mInt = 1 TO NUM-ENTRIES(mPID):
   FOR EACH person WHERE
         person.person-id EQ INT64(ENTRY(mInt,mPID))
      NO-LOCK,
      EACH loan WHERE
           loan.contract   EQ 'dps'
      AND  loan.close-date EQ ?
	   AND  loan.cust-cat   EQ "Ч"
	   AND  loan.cust-id    EQ person.person-id 
      AND  CAN-DO("0300,0301,0302",loan.filial-id)
      NO-LOCK,
      EACH loan-acct WHERE
           loan-acct.contract  EQ loan.contract
      AND  loan-acct.cont-code EQ loan.cont-code
      AND  loan-acct.acct BEGINS "42301"
      NO-LOCK BREAK BY loan-acct.acct:

         ASSIGN
            mAcct      = loan-acct.acct
            mContCodes = loan.cont-type + "=" + loan.cont-code
            mPerson    = person.name-last + " " + person.first-names.

         PUT UNFORMATTED
            ENTRY(mInt,mCID) ";"
            ENTRY(mInt,mPID) ";"
            mPerson ";"
            mContCodes ";"
            mAcct
         SKIP.
         
   
/*      IF FIRST-OF(loan-acct.acct) THEN                                                               */
/*      ASSIGN                                                                                         */
/*         mAcct      = loan-acct.acct                                                                 */
/*         mContCodes = ""                                                                             */
/*         mPerson    = person.name-last + " " + person.first-names.                                   */
/*                                                                                                     */
/*      mContCodes = IF mContCodes EQ "" THEN loan.cont-type + "=" + loan.cont-code                    */
/*                                       ELSE mContCodes + ";" + loan.cont-type + "=" + loan.cont-code.*/
/*                                                                                                     */
/*      IF LAST-OF(loan-acct.acct) THEN                                                                */
/*         PUT UNFORMATTED                                                                             */
/*            ENTRY(mInt,mCID) ";"                                                                     */
/*            ENTRY(mInt,mPID) ";"                                                                     */
/*            mPerson ";"                                                                              */
/*            mAcct ";"                                                                                */
/*            mContCodes                                                                               */
/*         SKIP.                                                                                       */
   END.
END.

/*   PUT UNFORMATTED ENTRY(mInt,mCID) ";" signs.xattr-value ";" signs.surrogate SKIP.*/
   
PUT UNFORMATTED ETIME " msec" SKIP.



{intrface.del}

RETURN.
*/
