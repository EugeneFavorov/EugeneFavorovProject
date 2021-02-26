/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: KSVOK.I
      Comment: Создание субаналитики по кассирам ВОК.
   Parameters:
         Uses:
      Used by:
      Created: 15.01.2004 15:41 kolal
     Modified: 15.01.2004 16:06 kolal    Заявка 23445.
     Modified: 11.02.2004 17:41 kolal    Определение кода смены через tparam
                                         Заявка 23448.
     Modified: 20.02.2004 16:20 kolal    Find last заменен на for each/leave.
                                         Добавлен код пользователя. Заявка 23445.
     Modified: 17.03.2004 14:11 kolal    Добавлена инициализация КАУ для операций
                                         номерного учета. Заявка 24170.
     Modified: 17.03.2004 15:36 kolal    24170
     Modified: 04.01.2005 12:21 rija     40572: Ограничить фильтр в Браузере смен по коду
                                         подразделения в операциях .
     Modified: 16.01.2005 17:09 rija     40572: Ограничить фильтр в Браузере смен по коду
                                         подразделения в операциях .
     Modified: 04.02.2005 10:07 rija     42014: Не проставляется код смены в документы выполненные по
                                         суб. счетам из др. модулей
     Modified: 04.02.2005 15:40 rija     40572: Ограничить фильтр в Браузере смен по коду
                                         подразделения в операциях .
     Modified: 03.06.2005 14:49 rija     46862: Предоставить возможность установки вида ДЧ для
                                         внебалансового счета
     Modified: 03.06.2005 15:17 rija     46862: Предоставить возможность установки вида ДЧ для
                                         внебалансового счета
     Modified: 03.06.2005 17:09 rija     46452: Модуль ВОК. Ограничения по привязкам документов к
                                         сменам.
     Modified: 12.07.2005 16:43 rija     48722: Модуль ВОК. Ошибка привязки документов
     Modified: 10.09.2005 11:42 rija     50437: ВОК. Неверное создание субаналитики для
                                         мультивалютной проводки
     Modified: 12.09.2005 09:59 rija     50437: ВОК. Неверное создание субаналитики для
                                         мультивалютной проводки
     Modified: 12.09.2005 10:13 rija     50437: ВОК. Неверное создание субаналитики для
                                         мультивалютной проводки
     Modified: 13.09.2005 14:16 rija     50437: ВОК. Неверное создание субаналитики для
                                         мультивалютной проводки
     Modified: 15.01.2015 17:10 KSV      <comment>
*/

{globals.i}
{form.def}
{intrface.get xclass}
{intrface.get tparam}
{intrface.get vok}
{intrface.get pbase}
{intrface.get cm}
{intrface.get kau}
{intrface.get trans}

DEFINE INPUT PARAMETER rid   AS RECID   NO-UNDO.
DEFINE INPUT PARAMETER in-db AS LOGICAL NO-UNDO.

DEFINE BUFFER bop  FOR op.
DEFINE BUFFER acct-cr FOR acct.
DEFINE BUFFER acct-db FOR acct.

DEFINE VARIABLE mUserList        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vChCurrKauEntry  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDpr-id          AS INT64      NO-UNDO. /* Номер текущей смены */
DEFINE VARIABLE mPrevSess        AS INT64      NO-UNDO. /* Номер текущей смены */
DEFINE VARIABLE mStr             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAnalitSmen      AS LOGICAL    NO-UNDO. /* НП АналитСмен */
DEFINE VARIABLE mCmOp            AS LOGICAL    NO-UNDO. /* Документ по Ценным монетам */
DEFINE VARIABLE mCode            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mVal             AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mRstTime         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mRstDate         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOpDprId         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mError           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mOpKind          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mPeredDokZk      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mKontrPrSmen     AS LOGICAL     NO-UNDO.

DEFINE VARIABLE mTwoDpr   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOtrDoc   AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE mDepo-cr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDepo-db  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBrnch-cr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBrnch-db AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDepo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSessDepo AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSessVech AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVech-cr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVech-db  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVech     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCrKauId  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDbKauId  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPrintLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPrintOp  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFltTable AS HANDLE    NO-UNDO.
DEFINE VARIABLE i         AS INT64     NO-UNDO.

DEFINE TEMP-TABLE tt-Sess    NO-UNDO LIKE sessions.


mPeredDokZk  = FGetSetting("ПередДокЗК",?,"Да")   EQ "Да".
mKontrPrSmen = FGetSetting("КонтрПрСмены",?,"Да") EQ "Да".

/*MESSAGE mPeredDokZk ";" mKontrPrSmen 
VIEW-AS ALERT-BOX.*/

&IF DEFINED(BUFF-BOP-ENTRY) &THEN
&ELSE
   &GLOBAL-DEFINE BUFF-BOP-ENTRY YES
   DEFINE BUFFER bop-entry1 FOR op-entry.
   DEFINE BUFFER bkau-entry1 FOR kau-entry.
&ENDIF

FIND op-entry WHERE RECID(op-entry) EQ rid
   EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

IF LOCKED op-entry  THEN 
DO:
   RUN wholocks2.p (rid, "op-entry", "Запись в op-entry заблокирована").
   pick-value = "no".
   RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
   RETURN.
END.

FIND FIRST op OF op-entry
   NO-LOCK NO-ERROR.

IF    NOT AVAILABLE op-entry 
   OR NOT AVAILABLE op THEN
DO:
   /* Не найдена проводка, уходим */
   pick-value = "no".
   RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
   RETURN.
END.

IF op-entry.currency NE "" AND op-entry.amt-cur EQ 0 THEN
DO:
   /* исключим обработку проводок переоценки */
   pick-value = "yes".
   RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
   RETURN.
END.

{find-act.i
   &acct = "op-entry.acct-cr"
   &bact = "acct-cr" 
}
{find-act.i
   &acct = "op-entry.acct-db"
   &bact = "acct-db" 
}

IF AVAIL acct-cr THEN
DO:
   mBrnch-cr = acct-cr.branch-id.
   RUN Get-Kau-Id in h_kau (acct-cr.acct, acct-cr.currency, OUTPUT mCrKauId).
   mDepo-cr = GetXattrValueEx("acct",acct-cr.acct + "," + acct-cr.currency,"depository","0").
   mVech-cr = IF GetXattrValueEx("acct",acct-cr.acct + "," + acct-cr.currency,"ВечерКас","Нет") EQ "Да" THEN "В"
                                                                                                        ELSE "Д".
/*   MESSAGE mDepo-cr ";" mVech-cr  
   VIEW-AS ALERT-BOX.                                                                                                        */
END.

IF AVAIL acct-db THEN
DO:
   mBrnch-db = acct-db.branch-id.
   RUN Get-Kau-Id in h_kau (acct-db.acct, acct-db.currency, output mDbKauId).
   mDepo-db = GetXattrValueEx("acct",acct-db.acct + "," + acct-db.currency,"depository","0").
   mVech-db = IF  GetXattrValueEx("acct",acct-db.acct + "," + acct-db.currency,"ВечерКас","Нет") EQ "Да" THEN "В"
                                                                                                         ELSE "Д".
/*   MESSAGE mDepo-cr ";" mVech-cr  
   VIEW-AS ALERT-BOX.                                                                                                         */
END.

IF in-db THEN
   ASSIGN
      mDepo  = mDepo-db
      mVech  = mVech-db
   .   
ELSE
   ASSIGN
      mDepo  = mDepo-cr
      mVech  = mVech-cr   
   .
mTwoDpr =    mCrKauId BEGINS "КодСмены"
         AND mDbKauId BEGINS "КодСмены"
         AND (   mDepo-cr  NE mDepo-db     
              OR mVech-cr  NE mVech-db
              OR mBrnch-cr NE mBrnch-db).
IF mTwoDpr EQ ? THEN mTwoDpr = NO.
{find-act.i &acct=op-entry.acct{&db-cr}}
IF AVAILABLE acct THEN
   ASSIGN
      vChCurrKauEntry = acct.Currency
   .

ASSIGN
   mAnalitSmen = fGetSetting("АналитСмен","","") EQ "Да"
   mOtrDoc     = fGetSetting("ОтрДок","","Нет") EQ "Да"
   mCmOp       = (GetXattrValue("op",STRING(op.op),"cont-code") NE "") AND op.class-code BEGINS "opbmn" 
.
/*MESSAGE mAnalitSmen ";" mOtrDoc ";" mCmOp 
VIEW-AS ALERT-BOX.*/
IF     NOT mAnalitSmen
   AND mCmOp THEN
DO:
   pick-value = "yes".
   {intrface.del}
   RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
   RETURN.
END.

pick-value = "no".

TR:
DO ON ERROR UNDO TR, LEAVE TR 
   ON STOP  UNDO TR, LEAVE TR:

   DEFINE BUFFER xattr FOR xattr.   /* Локализация буфера. */

   /* Поиск буфера ДР. */
   RUN GetXAttr (op.Class-Code,
              "dpr-id",
              BUFFER xattr).
   IF NOT AVAIL xattr THEN
   DO:
      MESSAGE "На классе документа~"" + op.Class-Code + "~"" SKIP
              "отсутствует дополнительный реквизит ~"dpr-id~"" SKIP
              "Счет ~"" + acct.acct + "~" не удалось провести по субаналитике."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      UNDO TR.
   END.

   /* Если на проводке есть ДР dpr-id-cr или dpr-id-db тогда берём значение из него */
   mDpr-id  = INT64(GetXattrValueEx("op-entry",STRING(op-entry.op) + "," + STRING(op-entry.op-entry),"dpr-id{&db-cr}",?)) NO-ERROR.
   mOpDprId = GetXattrValue("op",STRING(op.op),"dpr-id").
   /* Иначе смотрим, не находимся ли мы в смене */
   IF mDpr-id EQ ? THEN
      mDpr-id = INT64(tGetParam("dpr-id", "vok", "")) NO-ERROR.
   FIND FIRST sessions WHERE
              sessions.dpr-id EQ mDpr-id
      NO-LOCK NO-ERROR.
   /* Если нет, то смотрим нет ли документов созданых этой транзакцией и привязанных к смене */
   IF NOT AVAIL sessions THEN
   DO:
      FOR EACH bop WHERE
               bop.op-transaction EQ op.op-transaction
         NO-LOCK:
         mDpr-id = INT64(GetXattrValueEx("op",STRING(bop.op),"dpr-id",?)) NO-ERROR.
         IF mDpr-id NE ? THEN
         DO:
            FIND FIRST sessions WHERE
                       sessions.dpr-id EQ mDpr-id
               NO-LOCK NO-ERROR.
            IF AVAIL sessions THEN
               LEAVE.
         END.
      END.
   END.
   IF NOT AVAIL sessions THEN
   DO:
      mDpr-id = INT64(GetAttrValue2("",0,"mDpr-Id{&db-cr}")) NO-ERROR.
      FIND FIRST sessions WHERE
                 sessions.dpr-id EQ mDpr-id
         NO-LOCK NO-ERROR.
   END.
   IF AVAIL sessions THEN
   DO:
      mSessDepo  = GetXattrValueEx("sessions",STRING(sessions.dpr-id),"КасУзел","0").
      mSessVech  = GetXattrValueEx("sessions",STRING(sessions.dpr-id),"ПрСмены","Д").
      IF    mSessDepo          NE mDepo
         OR (    mSessVech     NE mVech 
             AND mKontrPrSmen)
         OR sessions.branch-id NE acct.branch-id
      THEN
         RELEASE sessions.
   END.   


   def var ii as int no-undo.
   def var mLock as char no-undo.

   /* Если нет, то даём выбрать смену руками */
   IF NOT AVAIL sessions THEN
   DO:
      mUserList = GetUserXAttrValue(USERID('bisquit'),"Кассиры").
      mUserList = IF     LOOKUP(USERID('bisquit'),mUserList) EQ 0
                     AND GetUserXAttrValue(USERID('bisquit'),"VOK") NE ""  THEN
                    (IF mUserList EQ "" THEN "*"
                     ELSE (mUserList + "," + USERID('bisquit'))
                     )
                  ELSE
                     mUserList.
      mUserList   = IF mUserList EQ "" THEN "*" ELSE mUserList.
      IF mKontrPrSmen THEN
         ASSIGN
            mCode = CHR(1) + "sc-3"    + CHR(1) + "sv-3" 
            mVal  = CHR(1) + "ПрСмены" + CHR(1) + mVech
         .
      IF mDepo NE "0" THEN
         ASSIGN
            mCode = mCode + CHR(1) + "sc-2"    + CHR(1) + "sv-2"
            mVal  = mVal  + CHR(1) + "КасУзел" + CHR(1) + mDepo
         .
      IF mTwoDpr AND mPeredDokZk THEN
         ASSIGN
            mCode = mCode + CHR(1) + "sc-4"      + CHR(1) + "sv-4"
            mVal  = mVal  + CHR(1) + "ЗавКассой" + CHR(1) + "Да"  
         .
      mLock = mCode.

      do ii = 1 to num-entries(mCode,chr(1)):
         if entry(ii,mCode,chr(1)) begins "sc-" then
         do:
            mCode = mCode + chr(1) + "SignsOrd".
            mVal = mVal + chr(1) + replace(entry(ii,mCode,chr(1)),"sc-","sv-") + ",yes".
         end.
      end.

      {empty tt-Sess}
      mPrevSess = INT64(GetSysConf ("PrevSelectedDprId")) NO-ERROR.
      IF mPrevSess EQ ? THEN
         mPrevSess = 0.
mPrevSess = 0.         
/*MESSAGE "mPrevSess = " mPrevSess
VIEW-AS ALERT-BOX.*/
      IF mPrevSess NE 0 THEN
      DO:
         FOR EACH sessions WHERE
            sessions.dpr-id NE mPrevSess
         NO-LOCK:
            CREATE tt-Sess.
            BUFFER-COPY sessions TO tt-Sess.
         END.
         hFltTable = TEMP-TABLE tt-Sess:HANDLE.
         RUN browseld.p("sessions",
                       "FilterTable"     + CHR(1) + "Branch-Id"    + CHR(1) + "dpr-status" + CHR(1) + "op-date1"         + CHR(1) + "op-date2"         + CHR(1) + "user-id" + mCode,
                       STRING(hFltTable) + CHR(1) + acct.branch-id + CHR(1) + "ОТКРЫТА"    + CHR(1) + STRING(op.op-date) + CHR(1) + STRING(op.op-date) + CHR(1) + mUserList + mVal,
                       "Branch-Id"       + CHR(1) + "dpr-status"   + CHR(1) + "op-date1"   + CHR(1) + "op-date2"         + CHR(1) + "user-id" + mLock,
                       4).
      END.
      ELSE
      DO:
         RUN browseld.p("sessions",
                       "Branch-Id"    + CHR(1) + "dpr-status" + CHR(1) + "op-date1"         + CHR(1) + "op-date2"         + CHR(1) + "user-id" + mCode,
                       acct.branch-id + CHR(1) + "ОТКРЫТА"    + CHR(1) + STRING(op.op-date) + CHR(1) + STRING(op.op-date) + CHR(1) + mUserList + mVal,
                       "Branch-Id"    + CHR(1) + "dpr-status"   + CHR(1) + "op-date1"   + CHR(1) + "op-date2"         + CHR(1) + "user-id" + mLock,
                       4).
      END.
      IF LASTKEY = 10 AND
         pick-value <> "" THEN
         mDpr-id = INT64(pick-value).
      ELSE
      DO:
         RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
         pick-value = STRING(mAnalitSmen AND mCmOp).
         {intrface.del}
         RETURN.
      END.
   END.
   IF NOT SessionIsOpen(mDpr-id) AND getThisUserXattrValue("ИзмДокум") NE "Имеет" THEN
   DO:
      RUN Fill-SysMes ("", 
                       "", 
                       "-1",
                       "Смена отсутствует или закрыта").
      pick-value = "no".
      {intrface.del}
      UNDO TR.
      RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
      RETURN.
   END.
   IF mOpDprId NE STRING(mDpr-Id) THEN
   DO:
      RUN BreakOnRedSaldo(op-entry.acct{&db-cr},?,IF vChCurrKauEntry EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur,(IF "{&db-cr}" EQ "-cr" THEN "-" ELSE "+"),STRING(mDpr-Id),OUTPUT mError).
      IF mError THEN
      DO:
         pick-value = "no".
         {intrface.del}
         UNDO TR.
         RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
         RETURN.
      END.
   END.
   DEFINE BUFFER xkauen FOR kau-entry.
   DEFINE BUFFER bkau1 FOR kau.

   /* ищем нужное kau */
   FIND bkau1 WHERE bkau1.acct EQ op-entry.acct{&db-cr}
              AND bkau1.currency EQ vChCurrKauEntry
              AND bkau1.kau EQ STRING(mDpr-Id)
      NO-LOCK NO-ERROR.

   FIND kau WHERE RECID(kau) = RECID(bkau1) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF LOCKED kau THEN 
   DO:
      RUN wholocks2.p (RECID(bkau1), "kau", "Запись в kau заблокирована").
      UNDO TR.
      RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
      RETURN.
   END.

   IF NOT AVAILABLE kau THEN
   DO:
      {kau(op).cr &op-entry="op-entry"}
   END.

   FIND bop-entry1 WHERE RECID(bop-entry1) = RECID(op-entry) NO-LOCK NO-ERROR.

   IF LOCKED op-entry  THEN 
   DO:
      RUN wholocks2.p (RECID(bop-entry1), "op-entry", "Запись в op-entry заблокирована").
      RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
      RETURN.
   END.

   ASSIGN
      op-entry.kau{&db-cr} = STRING(mDpr-id)
      kau.kau = STRING(mDpr-id)
      kau.currency = vChCurrKauEntry
      .

   {kau(off).cal
      &ssum=" {&side} op-entry.amt-rub" &inc=1
      &scur=" {&side} IF vChCurrKauEntry EQ '' THEN 0 ELSE op-entry.amt-cur"
   }

   /* Создаем kau-entry */
   FOR EACH xkauen WHERE xkauen.op = {&op-entry}.op
                  AND xkauen.op-entry = {&op-entry}.op-entry
      NO-LOCK
      BY xkauen.kau-entry DESC:
      LEAVE.
   END.

   CREATE kau-entry.
   ASSIGN
      kau-entry.op         = {&op-entry}.op
      kau-entry.op-entry   = {&op-entry}.op-entry
      kau-entry.kau-entry  = IF AVAILABLE xkauen THEN xkauen.kau-entry + 1 ELSE 1
      kau-entry.amt-rub    = op-entry.amt-rub
      kau-entry.amt-cur    = IF vChCurrKauEntry EQ "" THEN 0 ELSE op-entry.amt-cur
      kau-entry.acct-cat   = {&op-entry}.acct-cat
      kau-entry.op-date    = {&op-entry}.op-date
      kau-entry.acct       = {&op-entry}.acct{&db-cr}
      kau-entry.currency   = vChCurrKauEntry
      kau-entry.kau        = STRING(mDpr-Id)
      kau-entry.op-code    = {&op-entry}.op-cod
      kau-entry.op-status  = {&op-entry}.op-status
      kau-entry.value-date = {&op-entry}.value-date
      kau-entry.debit      = "{&db-cr}" = "-db"
      kau-entry.kau-id     = kau.kau-id
      kau-entry.qty        = {&op-entry}.qty
      op-entry.kau{&db-cr} = STRING(mDpr-Id)
      pick-value           = "yes"
      kau-entry.user-id    = USERID("bisquit")
      .

   IF mTwoDpr THEN
   DO:
      IF mOtrDoc NE in-db THEN 
         IF NOT UpdateSigns("op",STRING(op.op),"dpr-id",STRING(mDpr-Id),YES) THEN
            UNDO TR.                         
   END.
   ELSE
   DO:
      IF mOpDprId NE STRING(mDpr-Id) THEN
      DO:
         IF NOT UpdateSigns("op",STRING(op.op),"dpr-id",STRING(mDpr-Id),YES) THEN
            UNDO TR.   
         RUN SetRstNum(op.op).
      END.
   END.

   mOpKind = GetBaseOpkind().
   IF CAN-FIND(FIRST op-kind WHERE 
                     op-kind.op-kind    EQ mOpKind
                 AND op-kind.class-code EQ "common-op-kind") THEN      
      RUN AddAttr2TableEx ("",0,-1,"",0,"mDpr-Id{&db-cr}",STRING(mDpr-Id)).

   /* Номерной учет */
   FOR EACH form-op WHERE form-op.op = kau-entry.op:
      form-op.kau = STRING(mDpr-Id).
   END.
   IF mPrevSess EQ 0 THEN
      RUN SetSysConf in h_base ("PrevSelectedDprId",STRING(mDpr-Id)).
   ELSE
      RUN SetSysConf in h_base ("PrevSelectedDprId","") NO-ERROR.
END.
{intrface.del}

PROCEDURE SetFileName.
   DEFINE OUTPUT PARAMETER oFileName AS CHARACTER NO-UNDO.
   oFileName = mPrintOp + ".tmp".
END PROCEDURE.   
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='03/02/2015 20:00:03.700+04:00' */
/* $LINTUSER='BIS' */
/* $LINTMODE='1' */
/* $LINTFILE='ksvok.i' */
/*prosign4V4ozbFpeRC0A83fL7zyFg*/