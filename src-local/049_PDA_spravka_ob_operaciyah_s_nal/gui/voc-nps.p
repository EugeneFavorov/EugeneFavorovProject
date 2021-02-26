{globals.i}

/* +++ voc-nps.p was humbly modified by (c)blodd converter v.1.09 on 1/18/2017 1:13pm +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: voc-nps.p
      Comment: Процедура печати справки об осуществлении операций
               с наличной валютой и чеками. (ВОК)
               (Открытая система печати)
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: 
      Used by:
      Created: 11.09.2004 16:37 ligp     31278: Доработка в модуле ВОК в свете Инструкции 113-И (замена справки)
     Modified: 14.09.2004 15:18 ligp     31278: Инструкция 113-И (замена справки)
     Modified: 15.09.2004 13:23 ligp     31278: Добавлено поле "Кем выдан документ"
     Modified: 24.09.2004 16:03 ligp     32092: Общие с другими отчетами переменные из agree.def
     Modified: 05.10.2004 13:20 ligp     32095: Указание № 1433-У (Приложение 4)╓Книга учета ден.
                                         наличности и др, ценностей·
     Modified: 09.12.2004 12:30 rija     
     Modified: 25.03.2005 18:08 rija     42675: Требования банка Солидарность. Оценить трудоемкость
                                         работ.
     Modified: 31.05.2005 SChurkin       0045001 В функцию GetDateTimeOpTr добавлен параметр op.op
     Modified: 14.08.2006 15:37 ELUS     0064978
     Modified: 25.08.2006 16:58 ELUS     0064978: В справке выдаваемой на документы продажи ДЧ сумма
                                         принятых наличных удваивается
     Modified: 05.09.2006 15:05 ELUS     0064978: В справке выдаваемой на документы продажи ДЧ сумма
                                         принятых наличных удваивается
     Modified: 16.09.2008 14:39 elus      0125593: Ошибка печати реестра по кассе
     
*/

DEFINE INPUT PARAMETER iRID AS RECID NO-UNDO.

{globals.i}                     /* глобальные переменные */
{intrface.get xclass}
{intrface.get tparam}
{intrface.get sessions} /* pp-sessi.p - Инструмент для работы со сменами (таблица sessions). */
{intrface.get vok}     /* Инструменты для работы с обьектами ВОК pp-vok.p */
{intrface.get tmess}
{intrface.get db2l}
{limitsum.chk}

{prn-doc.def &with_proc=YES}

DEFINE VARIABLE vLogPrnCommm AS LOGICAL FORMAT "YES/NO"  NO-UNDO.
vLogPrnCommm = LOGICAL(GetSysConf ("vok-print-commission")).
RUN DeleteOldDataProtocol IN h_base ("vok-print-commission").

DEFINE VARIABLE mOpCurrCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE mContrAct   AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFIO        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocum      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocId      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocWho     AS CHARACTER NO-UNDO. /* Кем выдан документ */
DEFINE VARIABLE mKP         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrDb     AS CHARACTER INITIAL "---" NO-UNDO.   /* Код принятой валюты */
DEFINE VARIABLE mSummaDb    AS DECIMAL   INITIAL 0     NO-UNDO.   /* Получено наличности */
DEFINE VARIABLE mCurrCr     AS CHARACTER INITIAL "---" NO-UNDO.   /* Код выданной валюты */
DEFINE VARIABLE mSummaCr    AS DECIMAL   INITIAL 0     NO-UNDO.   /* Выдано наличности */
DEFINE VARIABLE mCurrCheq   AS CHARACTER INITIAL "---" NO-UNDO.   /* Код валюты чека */
DEFINE VARIABLE mQtyCheq    AS INT64   INITIAL 0     NO-UNDO.   /* Количество чеков */
DEFINE VARIABLE mSumCheq    AS DECIMAL   INITIAL 0     NO-UNDO.   /* Сумма по чекам */ 
DEFINE VARIABLE mDoc-num    AS CHARACTER NO-UNDO. /* Номер документа */

DEFINE VARIABLE mOpTime     AS INT64   NO-UNDO. /* Время начала транзакции */
DEFINE VARIABLE mOpDate     AS DATE      NO-UNDO. /* Дата начала транзакции */
DEFINE VARIABLE mIsCheckBuy AS LOGICAL   NO-UNDO. /* Yes - ежели чеков купили */
DEFINE VARIABLE mIsCheckSel AS LOGICAL   NO-UNDO. /* Yes - ежели чеков продали */
DEFINE VARIABLE mAdrPodrSpr AS LOGICAL   NO-UNDO. /* АдрПодрСпр */
DEFINE VARIABLE mTimeSep    AS CHARACTER NO-UNDO INIT ":".
DEFINE VARIABLE mTimeFormat AS CHARACTER NO-UNDO.

/* Буферы для нужных нам таблиц */
DEFINE BUFFER bOp   FOR op.   
DEFINE BUFFER bCode FOR code.   
DEFINE BUFFER bCurrency FOR currency.   

FIND FIRST bOp WHERE RECID(bOp) EQ iRID NO-LOCK NO-ERROR.

IF NOT AVAILABLE bOp THEN 
DO:
   RUN Fill-AlertSysMes IN h_tmess("","",-1,"Документ не найден").

   RETURN.
END.

{wordwrap.def} /* Нужен для wordwrap.i, котовый использ. в agr-beg.def */
{agr-beg.def 
   &NameTitle = "РЕЕСТР ОПЕРАЦИЙ С ВАЛЮТОЙ И ЧЕКАМИ"
   &TypeDoc   = '"spr"'} /* лишь бы список смен был неформатированный */   

/* Обычно в отчетах я вызывал {agr-beg.i} для HeadInRep и FootInRep,
 а здесь mCuUserID и mCuBrchID нужно определить по документу */

ASSIGN
   mOpDprID    = GetXAttrValueEx("op",STRING(bOp.op),"dpr-id","?") /* ID смены в документе */
   mCuUserID   = GetUserIDFromSessions(INT64(mOpDprID))
   mCuBrchID   = GetBranchIDFromSessions(INT64(mOpDprID))
   mAdrPodrSpr = fGetSetting("АдрПодрСпр","","") EQ "Да"
   mTimeFormat = fGetSetting("ФорматВремСправ","","Да|:")
   .

IF NUM-ENTRIES(mTimeFormat,"|") GE 2 THEN
   mTimeSep = ENTRY(2,mTimeFormat,"|").
mTimeFormat = IF ENTRY(1,mTimeFormat,"|") EQ "Нет" THEN "HH:MM" ELSE "HH:MM:SS".

RUN HeadInRep. 
RUN FootInRep. 
RUN GetDateTimeOpTr(bOp.op-transaction, bOp.op,OUTPUT mOpTime,OUTPUT mOpDate).

DEFINE VARIABLE vREGN       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vAdresPch   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vBank       AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE vBranchAttr AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE mHeadLine   AS CHARACTER   NO-UNDO EXTENT 10.
DEFINE VARIABLE vFilialType AS CHARACTER   NO-UNDO.

ASSIGN
   vFilialType    = fGetSetting("Filial","FilialType","10,11")
   vREGN          = FGetSetting("REGN",?,"")
   vREGN          = vREGN + ( IF {assigned mBranchREGN} AND mBranchREGN NE vREGN THEN ("/" + mBranchREGN) ELSE "")
   vAdresPch      = FGetSetting("Адрес_пч",?,?)
   vBank[1]       = mBank
   .
IF NOT CAN-DO(vFilialType,GetBufferValue("branch","where branch.branch-id EQ '" + bop.branch-id + "'","branch-type")) THEN
   vBranchAttr[1] = mBranchName + " " + mVOKAddr[1].

{wordwrap.i
  &s = vBank
  &n = 2
  &l = 60
}
{wordwrap.i
  &s = vBranchAttr
  &n = 2
  &l = 60
}

ASSIGN
   mHeadLine[1] = "Полное (сокращенное) фирменное наименование"         + FILL(" ",11) + vBank[1]
   mHeadLine[2] = "уполномоченного банка"                               + FILL(" ",33) + vBank[2]
   mHeadLine[3] = "(наименование филиала)"
   mHeadLine[4] = "Регистрационный номер уполномоченного банка/"        + FILL(" ",10) + STRING(vREGN)
   mHeadLine[5] = "порядковый номер филиала"
   mHeadLine[6] = "Местонахождение (адрес) уполномоченного банка"       + FILL(" ",9) + vAdresPch     
   mHeadLine[7] = "(филиала)"
   mHeadLine[8] = "Наименование внутреннего структурного подразделения" + FILL(" ",3) + vBranchAttr[1]
   mHeadLine[9] = "уполномоченного банка и его местонахождение (адрес)" + FILL(" ",3) + vBranchAttr[2]
   .

DO i = 1 TO 9:
   RUN Insert_TTName("HeadLine" + STRING(i),mHeadLine[i]).
END.

IF mAdrPodrSpr THEN
DO:
   RUN Insert_TTName("Bank",mBank).
   RUN Insert_TTName("Regn",FGetSetting("REGN",?,"")).
   RUN Insert_TTName("Address",FGetSetting("Адрес_пч",?,?)).
   RUN Insert_TTName("BranchName", mBranchName).
   RUN Insert_TTName("BranchRegn", mBranchREGN).
   RUN Insert_TTName("BranchAddress", mVOKAddr[1]).
END.
ELSE
DO:
   RUN Insert_TTName("Bank", mParentName).
   RUN Insert_TTName("Regn", mBranchREGN).
   RUN Insert_TTName("Address", mVOKAddr [1]).
END.
 
FIND FIRST _user WHERE _user._userid EQ userid("bisquit") NO-LOCK NO-ERROR.

RUN Insert_TTName("op-date", STRING(mOpDate)).
RUN Insert_TTName("op-time", REPLACE(STRING(mOpTime,mTimeFormat),":",mTimeSep) + IF CAN-DO('0516,0517', STRING(GetXattrValueEx("_user",STRING(_user._userid),"Отделение",""))) THEN ' (время Омское)' ELSE '').

mDoc-num = GetXAttrValueEx("op", STRING(bOp.op), "НомРеестр", "").
IF mDoc-num EQ "" THEN
   mDoc-num = bOp.doc-num.

RUN Insert_TTName("doc-num", TRIM(mDoc-num)).

mOpCurrCode = GetXAttrValueEx("op", STRING(bOp.op), "ВидОпНалВ", "").

RUN Insert_TTName("OpCurrCode", mOpCurrCode).
RUN Insert_TTName("OpCurrName", GetCodeName("ВидОпНалВ", mOpCurrCode)).

ASSIGN
   mFIO    = GetXAttrValueEx("op", STRING(bOp.op), "ФИО", "")
   mDocum  = GetXAttrValueEx("op", STRING(bOp.op), "Докум", "")
   mDocId  = GetCodeName("КодДокум",GetXAttrValue("op",STRING(bOp.op),"document-id")) 
   mDocWho = GetXAttrValueEx("op", STRING(bOp.op), "cust-doc-who", "")
   mKP     = GetXAttrValueEx("op", STRING(bOp.op), "подразд", "")
.

/* Это пока не мешает, и даже, видимо, пригодится позже, после реализации 32412*/
/* mDocWho тогда же пропишем */
IF    mDocum  EQ ""
   OR mDocId  EQ ""
   OR mFIO    EQ ""
   OR mDocWho EQ "" THEN
DO:
   FOR EACH op-entry OF bOp NO-LOCK:
      FIND FIRST acct WHERE 
                 acct.acct     EQ op-entry.acct-db
             AND acct.cust-cat EQ "Ч" 
         NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         FIND FIRST person WHERE 
                    person.person-id EQ acct.cust-id 
            NO-LOCK NO-ERROR.
      IF AVAIL person THEN 
         LEAVE.
      FIND FIRST acct WHERE 
                 acct.acct     EQ op-entry.acct-cr
             AND acct.cust-cat EQ "Ч" 
         NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         FIND FIRST person WHERE 
                    person.person-id EQ acct.cust-id 
            NO-LOCK NO-ERROR.
      IF AVAIL person THEN 
         LEAVE.
   END.
   IF AVAIL person THEN
      ASSIGN
         mDocum  = IF mDocum  EQ "" THEN person.document  ELSE mDocum
         mDocId  = IF mDocId  EQ "" THEN GetCodeName("КодДокум",person.document-id) ELSE mDocId
         mFIO    = IF mFIO    EQ "" THEN person.name-last + " " + person.first-names ELSE mFIO
         mDocWho = IF mDocWho EQ "" THEN person.issue ELSE mDocWho
      .
   IF NUM-ENTRIES(mDocWho) > 1 THEN
      ENTRY(2,mDocWho) = " К\П " + ENTRY(2,mDocWho).      
END.
ELSE
   IF {assigned mDocWho} THEN
   IF {assigned mKp} THEN 
      IF NUM-ENTRIES(mDocWho) > 1 THEN
         ENTRY(2,mDocWho) = " К\П " + mKp.
      ELSE
         mDocWho = mDocWho + ", К\П " + mKp.
   ELSE
      IF NUM-ENTRIES(mDocWho) > 1 THEN
         ENTRY(2,mDocWho) = " К\П " + ENTRY(2,mDocWho).
         
IF FGetSetting("СпрашПечФИО", "", "Нет") EQ "Да" THEN DO:
   RUN Fill-SysMes("", 
                   "", 
                   "4",
                   "Печать реквизиты клиента на справке?").  
   IF pick-value NE "YES" THEN
      ASSIGN
      mFIO    = ""
      mDocum  = ""
      mDocId  = ""
      mDocWho = ""
      .
END.
         
IF FGetSetting("ПечПСПВыдан", "", "Да") EQ "Нет" THEN mDocWho = "".

mDocum = mDocId + " " + mDocum.
RUN Insert_TTName("FIO",    mFIO).
RUN Insert_TTName("docum",  mDocum).
RUN Insert_TTName("sprate", STRING(DECIMAL(GetXAttrValueEx("op", 
                                                           STRING(bOp.op), 
                                                           "sprate", 
                                                           "")),
                                    ">,>>9.9999")).
mDocWho = "Выдан: " + mDocWho.
RUN Insert_TTName("CustDocWho",  mDocWho).


/* А далее применим инструмент из единого реестра, чтоб все было одинаково, даже ошибки */

DEFINE VARIABLE mSumRub   AS DECIMAL   INITIAL 0  NO-UNDO.   /* Рублевый эквивалент суммы операции  */
DEFINE VARIABLE mAcctCli  AS CHARACTER INITIAL "" NO-UNDO.   /* Счет клиента */


DEFINE VARIABLE mPrSumm AS CHARACTER INITIAL "" NO-UNDO. /* маски счетов, заданных в ПрСуммы */

FOR EACH bCode WHERE bCode.class EQ "ПрСуммы" NO-LOCK:
   IF bCode.val NE "" THEN
      {additem.i mPrSumm SUBSTRING(bCode.val,1,5)}
END.

{rst-tab.fun}

RUN GetCodeCurAndSumm(bOp.op,
                      mPrSumm,
                      OUTPUT mCurrDb,               /* 5 */
                      OUTPUT mSummaDb,              /* 6 */
                      OUTPUT mCurrCr,               /* 7 */
                      OUTPUT mSummaCr,              /* 8 */
                      OUTPUT mQtyCheq,              /* 10 */
                      OUTPUT mCurrCheq,             /* 11 */
                      OUTPUT mSumCheq,              /* 12 */
                      OUTPUT mSumRub,
                      OUTPUT mAcctCli).             /* 13 */

{vprncomm.i bOp}
/* Инструмент выдает "" если в этот раздел ничего не надо писать */
IF mCurrDb NE "" THEN 
DO:
   /* Инструмент выдает вместо "" код нац. валюты. Приходится переопределять */
   IF mCurrDb EQ mCodOurCur THEN
      mCurrDb = "".

   FIND FIRST bCurrency WHERE bCurrency.currency = mCurrDb NO-LOCK NO-ERROR.

   IF AVAILABLE bCurrency THEN
      RUN Insert_TTName("CurrName", bCurrency.name-currenc). /* принято денег в валюте */
   IF mCurrDb EQ "" THEN
      mCurrDb = mCodOurCur.             
END. /* IF mCurrDb NE "" THEN */

IF mCurrCr NE "" THEN
DO:
   IF mCurrCr EQ mCodOurCur THEN
      mCurrCr = "".

   FIND FIRST bCurrency WHERE bCurrency.currency = mCurrCr NO-LOCK NO-ERROR.

   IF AVAILABLE bCurrency THEN
      RUN Insert_TTName("CurrName1", bCurrency.name-currenc). /* выдано денег в валюте */
   IF mCurrCr EQ "" THEN
      mCurrCr = mCodOurCur.
END. /* IF mCurrCr NE "" THEN */

IF mCurrCheq NE "" THEN
DO:
   IF mCurrCheq EQ mCodOurCur THEN
      mCurrCheq = "".

   FIND FIRST bCurrency WHERE bCurrency.currency = mCurrCheq NO-LOCK NO-ERROR.
   
   IF AVAILABLE bCurrency THEN
   DO:
      IF mIsCheckBuy EQ TRUE THEN
      DO:
         RUN Insert_TTName("CurrNameC", bCurrency.name-currenc). /* принято чеков в валюте */
         RUN Insert_TTName("CurrCodeC", mCurrCheq).
         RUN Insert_TTName("AmtC",      STRING(mSumCheq,">>>,>>>,>>>,>>9.99") 
                                        + "  /  " + STRING(mQtyCheq)).  


         RUN Insert_TTName("CurrNameC1", ""). /* выдано чеков в валюте */
         RUN Insert_TTName("CurrCodeC1", "").
         RUN Insert_TTName("AmtC1",      "").

         RUN Insert_TTName("TrCodeDb2", GetXAttrValue("op",STRING(bOp.op),"TrCodeDb")).
         RUN Insert_TTName("TrCodeCr2", "").

      END.                                                                                   
      IF mIsCheckSel EQ TRUE THEN
      DO:
         RUN Insert_TTName("CurrNameC", ""). /* принято чеков в валюте */
         RUN Insert_TTName("CurrCodeC", "").
         RUN Insert_TTName("AmtC",      "").  

         RUN Insert_TTName("CurrNameC1", bCurrency.name-currenc). /* выдано чеков в валюте */
         RUN Insert_TTName("CurrCodeC1", mCurrCheq).
         RUN Insert_TTName("AmtC1",      STRING(mSumCheq,">>>,>>>,>>>,>>9.99") 
                                         + "  /  " + STRING(mQtyCheq)).

         RUN Insert_TTName("TrCodeCr2", GetXAttrValue("op",STRING(bOp.op),"TrCodeCr")).
         RUN Insert_TTName("TrCodeDb2", "").
      END.                                                                                   
   END.
   IF mCurrCheq EQ "" THEN
      mCurrCheq = mCodOurCur.
END. /* IF mCurrCheq NE "" THEN */

RUN Insert_TTName("TrCodeDb", IF mSummaDb EQ 0 THEN
                                 ""
                              ELSE
                                 GetXAttrValue("op",STRING(bOp.op),"TrCodeDb")).
RUN Insert_TTName("TrCodeCr", IF mSummaCr EQ 0 THEN
                                 ""
                              ELSE
                                 GetXAttrValue("op",STRING(bOp.op),"TrCodeCr")).

RUN Insert_TTName("CurrCode",   mCurrDb).
RUN Insert_TTName("CurrCode1",  mCurrCr).

RUN Insert_TTName("Amt",      IF mSummaDb EQ 0 THEN 
                                 "" 
                              ELSE  
                                 STRING(mSummaDb,">>>,>>>,>>>,>>9.99")).   
RUN Insert_TTName("Amt1",     IF mSummaCr EQ 0 THEN
                                 ""
                              ELSE 
                                 STRING(mSummaCr,">>>,>>>,>>>,>>9.99")).
RUN Insert_TTName("post", mPostInRep[1]).
RUN Insert_TTName("user", mFIOInRep[1]).
RUN Insert_TTName("empty", FILL(" ",LENGTH(mPostInRep[1] + "  " + mFIOInRep[1]))).

{intrface.del} /* Не нашел, м.б еще где это делается, встравил сюда */

RUN printvd.p(IF vLogPrnCommm THEN "ВОК-СООНВЧ" ELSE "СООНВЧ", INPUT TABLE ttnames).
/* $LINTUSER='STRE' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/4.1d/1ut/src' */
/* $LINTDATE='29/09/2014 13:14:46.896+04:00' */
/* $LINTFILE='voc-nps.p' */
/*prosignFSXE+qFZu/pmGFYizSKoOg*/
/* --- voc-nps.p was humbly modified by (c)blodd converter v.1.09 on 1/18/2017 1:13pm --- */
