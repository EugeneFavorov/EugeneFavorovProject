/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pfrotch1.p
      Comment: 0236841 Миграция. Загрузка реестра зачисления пенсий из ПФ + отчет по пенсионерам
   Parameters: Счет ПФР
         Uses:
      Used by:
      Created: 05/11/2015 KMBIS 0236841 Миграция. Загрузка реестра зачисления пенсий из ПФ
                                        Отчет по пенсионерам
     Modified: 
                                    
*/
DEF INPUT PARAM iParams   AS CHAR  NO-UNDO.
{globals.i}

{intrface.get date}
{intrface.get prnvd}
{intrface.get strng}
{intrface.get xclass}

DEF TEMP-TABLE ttPensAcct NO-UNDO
   FIELD acct-pfr AS CHAR  /* Номер счета                   */
   FIELD proxy    AS LOG   /* были списания по доверенности */
   FIELD print    AS LOG   /* Включать счет в отчет         */
   FIELD pers-id  AS INT64 /* person.person-id              */
   FIELD op-id    AS INT64 /* op.op документа из ПФР        */
.

DEF VAR mPfrCode  AS CHAR  NO-UNDO. /* Код классификатора ПФР */
DEF VAR mPFRAcct  AS CHAR  NO-UNDO. /* Счета ПФР              */
DEF VAR mI        AS INT64 NO-UNDO.
DEF VAR mAcct     AS CHAR  NO-UNDO.
DEF VAR mFIO      AS CHAR  NO-UNDO.
DEF VAR mBirth    AS CHAR  NO-UNDO.
DEF VAR mProxy    AS CHAR  NO-UNDO.
DEF VAR mPrBeg    AS CHAR  NO-UNDO.
DEF VAR mPrEnd    AS CHAR  NO-UNDO.
DEF VAR mPrFIO    AS CHAR  NO-UNDO.
DEF VAR mOpDate   AS CHAR  NO-UNDO.
DEF VAR mPFR      AS CHAR  NO-UNDO.
DEF VAR mDateNxt  AS DATE  NO-UNDO.
DEF VAR mPermFnd  AS LOG   NO-UNDO.
DEF VAR mProxyRID AS ROWID NO-UNDO.


DEF BUFFER bAcct     FOR acct.
DEF BUFFER bPensAcct FOR ttPensAcct.

{pfrotch1.fun}
{pfrotch1.pro}

ASSIGN
   mPFRAcct = ENTRY(1, iParams, ";") 
   mPfrCode = ENTRY(2, iParams, ";")  WHEN NUM-ENTRIES(iParams, ";") GE 2
.

{getdates.i &AddPostUpd = "IF beg-date EQ ? OR end-date EQ ? THEN 
                           DO:
                              MESSAGE 'Неверный период!' VIEW-AS ALERT-BOX.
                              UNDO, RETRY.
                           END."}

/*=== Перебор счетов с которых производится списание пенсий ===*/
DO mI = 1 TO NUM-ENTRIES(mPFRAcct):

   mAcct = ENTRY(mI, mPFRAcct).
   {find-act.i &acct = mAcct}

   /* Строим таблицу счетов на которые были зачисления со счета ПФ */
   IF AVAIL(acct) THEN
      RUN InitPensTT(acct.acct, beg-date, end-date).

END. /* DO mI = 1 TO NUM-ENTRIES(mPFRAcct): */


FOR EACH ttPensAcct NO-LOCK:
   /*=== Отфильтруем счета подлежащие выводу в отчет ===*/
   RUN FiltPensTT(ROWID(ttPensAcct), beg-date, end-date).

END. /* FOR EACH ttPensAcct NO-LOCK: */

/*=== Формируем отчет ===*/
RUN BeginCircle_TTName IN h_prnvd("line").
FOR EACH ttPensAcct WHERE ttPensAcct.print EQ YES
                    NO-LOCK,
   FIRST person WHERE person.person-id EQ ttPensAcct.pers-id
                NO-LOCK
                BREAK BY person.name-last
                      BY person.first-names
                      BY person.person-id
                      BY ttPensAcct.proxy:  /* первыми идут счета без списаний по доверенности */

   IF FIRST-OF(person.person-id) THEN
   DO:
      ASSIGN
         /*=== ФИО ===*/ 
         mFIO   = SUBST("&1 &2", TRIM(person.name-last), TRIM(person.first-names))
         mFIO   = RemoveDoubleChars(mFIO, " ")
         /*=== Дата рождения ===*/ 
         mBirth = STRING(person.birthday, "99.99.9999")
      .
   END. /*IF FIRST-OF(person.person-id) THEN */

   ASSIGN
      /*=== Дата последнего обращения для совершения операций по счетам ===*/ 
      mOpDate= LastMove(ttPensAcct.acct-pfr)
      /*=== Отделение ПФ ===*/ 
      mPFR   = GetPfrName(ttPensAcct.op-id, mPfrCode)
   .

   IF FIRST-OF(ttPensAcct.proxy) THEN
      mProxy = STRING(ttPensAcct.proxy, "Да/Нет"). /* Наличие доверенности */ 

   IF ttPensAcct.proxy EQ YES THEN
   DO:
      FOR EACH loan WHERE loan.cust-cat  EQ "Ч"
                      AND loan.cust-id   EQ person.person-id
                      AND loan.contract  EQ "proxy"
                      AND loan.open-date LE end-date
                      AND loan.end-date  GE beg-date
                    NO-LOCK:

         /*=== Проверяем довереноость на соответсвие условия отбора ===*/
         IF ChkProxy(ROWID(loan), ttPensAcct.acct-pfr) THEN
         DO:
            ASSIGN
               /*=== Дата выдачи доверенности ===*/ 
               mPrBeg = STRING(loan.open-date, "99.99.9999")
               /*=== Дата окончания срока доверенности ===*/ 
               mPrEnd = STRING(loan.end-date, "99.99.9999")
               /*=== Доверенное лицо ===*/ 
               mPrFIO = AgentName(loan.cont-code)
            .
            RUN Insert_TTName IN h_prnvd("fio[line]"     , mFIO).
            RUN Insert_TTName IN h_prnvd("birth[line]"   , mBirth).
            RUN Insert_TTName IN h_prnvd("proxy[line]"   , mProxy).
            RUN Insert_TTName IN h_prnvd("proxybeg[line]", mPrBeg).
            RUN Insert_TTName IN h_prnvd("proxyend[line]", mPrEnd).
            RUN Insert_TTName IN h_prnvd("proxyfio[line]", mPrFIO).
            RUN Insert_TTName IN h_prnvd("lastop[line]"  , mOpDate).
            RUN Insert_TTName IN h_prnvd("pfr[line]"     , mPFR).
            RUN NextCircle_TTName IN h_prnvd("line").

            ASSIGN
               mFIO   = ""
               mBirth = ""
               mProxy = ""
               mOpDate= ""
               mPFR   = ""
            .
         END. /* IF mPermFnd THEN */
      END. /* FOR EACH loan WHERE loan.cust-cat EQ "Ч" */
   END. /* IF ttPensAcct.proxy EQ YES THEN */
   ELSE 
   DO:
      RUN Insert_TTName IN h_prnvd("fio[line]"     , mFIO).
      RUN Insert_TTName IN h_prnvd("birth[line]"   , mBirth).
      RUN Insert_TTName IN h_prnvd("proxy[line]"   , mProxy).
      RUN Insert_TTName IN h_prnvd("proxybeg[line]", "").
      RUN Insert_TTName IN h_prnvd("proxyend[line]", "").
      RUN Insert_TTName IN h_prnvd("proxyfio[line]", "").
      RUN Insert_TTName IN h_prnvd("lastop[line]"  , mOpDate).
      RUN Insert_TTName IN h_prnvd("pfr[line]"     , mPFR).
      RUN NextCircle_TTName IN h_prnvd("line").
      /* Если по клиенту есть еще счета, то повторяющиеся данные обнуляются */
      ASSIGN
         mFIO   = ""
         mBirth = ""
         mProxy = ""
      .
   END. /* IF ttPensAcct.proxy EQ YES THEN ... ELSE */


END. /* FOR EACH ttPensAcct WHERE ttPensAcct.print EQ YES */

RUN EndCircle_TTName IN h_prnvd("line").
RUN prnvd IN h_prnvd("pfrotch1").
