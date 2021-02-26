/*
     Filename: izvestnr.p
      Comment: Уведомление о поступлении средства от нерезидентов 
   Parameters:
         Uses:
      Used by:
      Created: 17.28.2012 KMBIS TT:0174437 Выборка и создание печатной формы по созданию уведомлений по рублям  

*/
DEFINE INPUT PARAM iParam     AS CHAR    NO-UNDO.

{globals.i}

{tmpobj.def}
{tmprecid.def}


{intrface.get cust}      /* Библиотека для работы с клиентами. */
{intrface.get count}     /* Библиотека для работы с клиентами. */
{intrface.get xclass}

{prn-doc.def  &with_proc=YES}

DEF VAR mName     AS CHAR    EXTENT 3    NO-UNDO. /* Наименование клиента            */
DEF VAR mFlag     AS LOG                 NO-UNDO. /* Флаг формирования сообщений     */
DEF VAR mDocNum   AS CHAR                NO-UNDO. /* Номер извещения                 */

DEF VAR mAcctF    AS LOG                 NO-UNDO. /* Флаг формирования сообщений     */
DEF VAR mAcctNr   AS CHAR                NO-UNDO. /* Счета 2-го порядка нерезидентов */

DEF VAR mStat     AS CHAR                NO-UNDO. /* Статус документа для отбора     */
DEF VAR mAcctM    AS CHAR                NO-UNDO. /* Маска клиентских счетов         */

DEFINE VARIABLE mAmtStr   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDecStr   AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttOp-entry
    FIELD acct-id     AS RECID
    FIELD op-entry-id AS RECID
.
IF iParam EQ "" OR NUM-ENTRIES(iParam, ";") NE 2 THEN
DO:
   MESSAGE COLOR WHITE/RED
           "Не верно заданы входящие параметры"        SKIP
           "Формат: <маска_счетов>;<статус_документа>" SKIP
   VIEW-AS ALERT-BOX ERROR.
   RETURN.

END.
            
{getdate.i}

{empty TmpObj}
{empty ttOp-entry}

ASSIGN
   mAcctM  = ENTRY(1,iParam, ";" )
   mStat   = ENTRY(2,iParam, ";" ).
   mAcctNr = ""
.

{justasec}

/* Список балансовых счетов второго порядка нерезединтов */
FOR EACH signs WHERE signs.file-name  EQ "bal-acct"
                 AND signs.code       EQ "СчетНерез"
                 AND signs.code-value EQ "Да"
               NO-LOCK:
   IF mAcctNr EQ "" THEN 
      mAcctNr = signs.surrogate + "*".
   ELSE
      mAcctNr = mAcctNr + "," + signs.surrogate + "*".
END.

FOR EACH op-entry WHERE op-entry.op-date   EQ end-date
                    AND op-entry.currency  EQ ""
                    AND op-entry.op-status GE mStat
                    AND CAN-DO(mAcctM, op-entry.acct-cr)
                  NO-LOCK,
    FIRST op OF op-entry WHERE op.op-date   EQ end-date 
                           AND op.op-status GE mStat
                         NO-LOCK,
    FIRST acct WHERE acct.acct EQ op-entry.acct-cr
                 AND acct.currency EQ ""      
                 AND (acct.filial EQ ShFilial OR
                      shModeMulty) 
                NO-LOCK
                BREAK BY acct.acct:


   IF FIRST-OF(acct.acct) THEN mAcctF = NO.

   IF  (op.details   MATCHES '*~{VO*~}*') THEN 
   DO: 
      /* Отбираем документы с "*{VO*" в содержании операции */
      RUN CreateTTop(RECID(acct), RECID(op-entry)).
      mAcctF = YES.

   END.
   ELSE IF op.ben-acct NE ? AND op.ben-acct NE "" THEN
   DO: 

      IF CAN-DO(mAcctNr, op.ben-acct) THEN
      DO: 
         /* Счет отправилетля - счет нерезидента */
         RUN CreateTTop(RECID(acct), RECID(op-entry)).
         mAcctF = YES.
      END.

   END.
   ELSE IF op-entry.acct-db NE ? THEN
   DO: 

      IF CAN-DO(mAcctNr, op-entry.acct-db) THEN
      DO: 
         /* внутренний счет отправителя - счет нерезидента */
         RUN CreateTTop(RECID(acct), RECID(op-entry)).
         mAcctF = YES.
      END.

   END.

   IF LAST-OF(acct.acct) AND mAcctF THEN 
   DO: 
      CREATE TmpObj.
      ASSIGN TmpObj.rid = RECID(acct).
      VALIDATE TmpObj.
   END.

END. /* FOR EACH op-entry WHERE op-entry.op-date   EQ end-date */

/* Браузер счетов для выбора */
RUN browseld.p("acct",
               "UseTmpObjInQuery",
               string(mTmpObjHand),
               "",
               3).

IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN RETURN.

MESSAGE "Печатать уведомления, по уже отправленым документам?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ans1 AS LOGICAL.

RUN BeginCircle_TTName("izvest").

FOR EACH tmprecid,
   EACH ttOp-entry WHERE ttOp-entry.acct-id EQ tmprecid.id,
   EACH op-entry WHERE RECID(op-entry) EQ ttOp-entry.op-entry-id
                 NO-LOCK,
   FIRST op OF op-entry NO-LOCK
                        BREAK BY op-entry.acct-cr:

   IF FIRST-OF(op-entry.acct-cr) THEN
   DO:
      FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr
                      NO-LOCK NO-ERROR.
      RELEASE cust-corp.
      RELEASE person.
 
      mName = "".
      
      IF AVAIL(acct) THEN 
      DO:
         RUN GetCustName IN h_base(acct.cust-cat,
                                   acct.cust-id,
                                   ?,
                                   OUTPUT mName[1],
                                   OUTPUT mName[2],
                                   INPUT-OUTPUT mName[3]).
         RUN GetCustAdr(acct.cust-cat,acct.cust-id,gend-date,"АдрПочт,АдрФакт,АдрЮр",OUTPUT TABLE ttCustAddress).

         FIND LAST ttCustAddress WHERE 
                   ttCustAddress.fTypeAdr EQ "АдрПочт" NO-LOCK NO-ERROR.
      
         IF NOT AVAIL ttCustAddress THEN 
            FIND LAST ttCustAddress WHERE                
                      ttCustAddress.fTypeAdr EQ "АдрФакт" NO-LOCK NO-ERROR.
      
         IF NOT AVAIL(ttCustAddress) THEN 
            FIND LAST ttCustAddress  WHERE 
                      ttCustAddress.fTypeAdr EQ "АдрЮр" NO-LOCK NO-ERROR.

         IF AVAIL ttCustAddress THEN 
            RUN Insert_TTName("adr_klnt[izvest]",ttCustAddress.fAdrStr).

      END.
   END.

   /* делалось ли уведомление */
   mDocNum = GetXAttrValueEx("op",STRING(op.op),"izvest","").

   IF (ans1 EQ NO) AND (mDocNum NE "") THEN NEXT.

   IF mDocNum EQ "" THEN
      DO TRANSACTION:
         CreateCounterIfNotExist("УведТранНР", "Номер уведомления по суммам от нерезидентов", 1).
         mDocNum = STRING(GetCounterNextValue("УведТранНР", gend-date)).
         IF NOT {assigned mDocNum} THEN mDocNum = "1".
         UpdateSigns("op",STRING(op.op),"izvest",mDocNum,?).
      END.

   RUN Insert_TTName("name_klnt[izvest]", TRIM(TRIM(mName[1]) + " " + TRIM(mName[2]))).

   RUN Insert_TTName("docnum[izvest]", mDocNum).

   RUN Insert_TTName("date1[izvest]", STRING(end-date, "99.99.9999")).

   RUN Insert_TTName("acct[izvest]", DelFilFromAcct(op-entry.acct-cr)).

   RUN Insert_TTName("doc-num[izvest]", op.doc-num).

   RUN Insert_TTName("amt[izvest]", TRIM(STRING(op-entry.amt-rub,"->>,>>>,>>>,>>>,>>9.99"))).

   RUN x-amtstr.p (op-entry.amt-rub,
                   op-entry.currency,
                   YES,
                   YES,
                   OUTPUT mAmtStr,     /* строка суммы прописью */
                   OUTPUT mDecStr).    /* строка кол-во копеек цифрами */

   RUN Insert_TTName("amtl[izvest]", mAmtStr + " " + mDecStr).


   RUN NextCircle_TTName("izvest").

   mFlag = YES.

END. /* FOR EACH tmprecid */

RUN EndCircle_TTName("izvest").
                      
IF mFlag EQ YES
THEN               
    RUN printvd.p("izvestnr",INPUT TABLE ttnames). 
ELSE
    MESSAGE "По данным документам, уведомления уже создавались."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*============================================================================*/
/*=== Создание временной таблицы, с сылками на отобранные документы ==========*/

PROCEDURE CreateTTop:

DEF INPUT PARAM iAcctId       AS RECID.
DEF INPUT PARAM iOpEntryId    AS RECID.

   CREATE ttOp-entry.
   ASSIGN
     ttOp-entry.acct-id     = iAcctId
     ttOp-entry.op-entry-id = iOpEntryId
   .
   VALIDATE ttOp-entry.

END PROCEDURE.

/*============================================================================*/
