/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"
     Filename: safeprn1.p
      Comment: СЕЙФИНГ - Отчет "Договор на аренду ячейки"
   Parameters:
         Uses:
      Used by:
      Created: 12.05.2008 10:27 TURIN
     Modified: 15.07.2007 13:48 KAA     Доработка для распечатки договора в Word
     Modified: 11/11/2008 kraw (0094516) подъем в основную версию
*/

/* мыслим глобально */
{globals.i}
/* подключаем ttnames */
{prn-doc.def &with_proc=YES}
/* подключаем tmprecid */
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}
{dpsproc.def}
{intrface.get si}
{intrface.get crole}
{intrface.get xclass}
{loan_sn.i}




DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.

DEFINE VARIABLE mName        AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mInn        AS CHARACTER          NO-UNDO. /* Ю Ч*/
DEFINE VARIABLE mAdrReg       AS CHARACTER          NO-UNDO. /*АдрЮр*/
DEFINE VARIABLE mAdrFact        AS CHARACTER          NO-UNDO. /*АдрФакт*/
DEFINE VARIABLE mCustName    AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mValName     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress1    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress2    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAmtStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vDecStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mError       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSex         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mWord        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mMausumbaeva AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mShaboldina  AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mText        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDocV        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE phone        AS CHARACTER          NO-UNDO INIT ''.
DEFINE VARIABLE mSignsVal    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDatTMP      AS DATE               NO-UNDO.
DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mCommRate    AS DEC                NO-UNDO.
DEFINE VARIABLE mAmtStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDecStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE out_proc     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE i            AS INT                NO-UNDO.
DEFINE VARIABLE ost          AS DEC                NO-UNDO.
DEFINE VARIABLE vDayNum      AS int                NO-UNDO.
DEFINE VARIABLE mrub         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mtoday       AS DATE               NO-UNDO.
DEF VAR vOtherB       AS CHAR    NO-UNDO.
DEF VAR vSettlInstrID AS INT64   NO-UNDO.
DEFINE VARIABLE tDepRasch    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mOst         AS DEC                NO-UNDO.
DEFINE VARIABLE tCurrName    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE tStatus      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE tNameShort   AS CHARACTER          NO-UNDO init "".
DEFINE VARIABLE tFace        AS CHARACTER          NO-UNDO init "".

DEFINE VARIABLE mNBuh1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh3 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh4 AS CHARACTER   NO-UNDO.



DEFINE VARIABLE mben-acct         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mben-inn          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mben-cust-name    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mben-bank-code    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mben-bank-name    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDepRasch         AS LOGICAL            NO-UNDO init yes.

DEF BUFFER bCustRole FOR cust-role.

PAUSE 0.

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"
&GLOB Days "первое,второе,третье,четвертое,пятое,шестое,седьмое,восьмое,девятое,десятое,одиннадцатое,двенадцатое,тринадцатое,четырнадцатое,~
пятнадцатое,шестнадцатое,семнадцатое,восемнадцатое,девятнадцатое,двадцатое,двадцать первое,двадцать второе,двадцать третье,~
двадцать четвертое,двадцать пятое,двадцать шестое,двадцать седьмое,двадцать восьмое,двадцать девятое,тридцатое,тридцать первое"
&GLOB Years "первого,второго,третьего,четвертого,пятого,шестого,седьмого,восьмого,девятого,десятого,одинадцатого,двенадцатого,тринадцатого,~
четырнадцатого,пятнадцатого,шестнадцатого,семнадцатого,восемнадцатого,девятнацатого,двадцатого,двадцать первого,двадцать первого,~
двадцать второго,двадцать третьего,двадцать четвертого,двадцать пятого,шестого,двадцать седьмого,двадцать восьмого,двадцать девятого,тридцатого,тридцать первого,~
тридцать первого,тридцать второго,тридцать третьего,тридцать четвертого,тридцать пятого,тридцать шестого,тридцать седьмого,тридцать восьмого,тридцать девятого"

DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").
iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
str_title = "[ Выберите подписанта (" + user_ + " " + iBranch + ")]".

  run signat.p ("depprnu","UserName",str_title,OUTPUT mUser).
  RUN Insert_TTName("Pod-user-dolg", GetXAttrValueEx("_User", mUser, "Должность", "")).
  RUN Insert_TTName("Pod-user-fio", GetXAttrValueEx("_User", mUser, "ФИОП", "")).
  RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "ДокОснТипРП", "")).
  RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "Телефон", "")).
  RUN Insert_TTName("Pod-user-otdel-rp", GetXAttrValueEx("_User", mUser, "ОтделРП", "")).

  RUN Insert_TTName("Pod-isp-dolg", GetXAttrValueEx("_User", User_, "Должность", "")).
  RUN Insert_TTName("Pod-isp-fio", GetXAttrValueEx("_User", User_, "ФИОП", "")).
  RUN Insert_TTName("Pod-isp-dov-rp", GetXAttrValueEx("_User", User_, "ДокОснТипРП", "")).
  RUN Insert_TTName("Pod-isp-telefon", GetXAttrValueEx("_User", User_, "Телефон", "")).

  RUN Insert_TTName("TToday",today).
  RUN Insert_TTName("TTodayT",string(today,"99.99.9999")).
  RUN Insert_TTName("TTodayStr",  STRING(DAY(today))  + " " +  ENTRY(MONTH(today),{&Months}) +  " " + STRING(YEAR(today)) + "г.").
  RUN Insert_TTName("Time",string(time,"hh:mm:ss")).
  RUN Insert_TTName("gend-date", term2str(gend-date, gend-date)).
/*наименование и адрес банка*/
FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("go-bank-name",    branch.name).
   RUN Insert_TTName("go-bank-address", branch.Address).
   RUN Insert_TTName("go-gorod", entry(2,branch.Address)).
   RUN Insert_TTName("dps_bank", GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "dps_bank")).
END.

FIND FIRST branch WHERE branch.Branch-Id EQ shfilial NO-LOCK NO-ERROR.

IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("bank-name",    branch.name).
   RUN Insert_TTName("bank-address", branch.Address).
   RUN Insert_TTName("gorod", entry(2,branch.Address)).
   RUN Insert_TTName("bank-inn",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ИНН")).
   RUN Insert_TTName("bank-bik",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "БанкМФО")).
   RUN Insert_TTName("bank-kpp",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "КПП")).
   RUN Insert_TTName("bank-ks",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "КорСч")).
   RUN Insert_TTName("bank-KS-GDE",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ПечИзвГдеОткр")).
   RUN Insert_TTName("bank-tel",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "Телефон")).
   RUN Insert_TTName("bank-ogrn",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ОГРН")).
   RUN Insert_TTName("bank-addr-post",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ПечАдрПочт")).
   RUN Insert_TTName("bank-addr-kor",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ПечАдрКор")).
   RUN Insert_TTName("bank-dps_bank",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "dps_bank")).
   RUN Insert_TTName("bank-name-rp",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "БанкНаимРП")).
   RUN Insert_TTName("bank-name-pp",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "БанкНаимПП")).
  if dec(branch.Branch-Type) <> 10 then do:
   tFace  = " в лице " +  GetXattrValue("branch", STRING(branch.Branch-Id), "БанкНаимРП").
   RUN Insert_TTName("tFace",tFace  ).
  end.
END.

/* цикл по рецидам выбранных договоров */
FOR EACH tmprecid
   NO-LOCK,
FIRST loan WHERE RECID(loan) EQ tmprecid.id
   NO-LOCK :

   mSignsVal = GetXAttrValue("loan",
                           loan.contract + "," + loan.cont-code,
                           "ДатаСогл").

   mtoday = date(mSignsVal).
   RUN Insert_TTName("Today",mtoday).
   RUN Insert_TTName("TodayT",string(mtoday,"99.99.9999")).
   RUN Insert_TTName("TodayStr",  STRING(DAY(mtoday))  + " " +  ENTRY(MONTH(mtoday),{&Months}) +  " " + STRING(YEAR(mtoday)) + "г.").
   RUN Insert_TTName("loan-num", loan.doc-ref).
   mSignsVal = getxattrvalue ("loan",loan.contract + "," + loan.cont-code,"ДатаСогл").
   RUN Insert_TTName("open-date", mSignsVal).
   RUN Insert_TTName("ЮЛ", "1").
   mSignsVal = STRING(DAY(loan.open-date),"99") + " " + ENTRY(MONTH(loan.open-date),{&Months}) + " " + STRING(YEAR(loan.open-date)).
   RUN Insert_TTName ("date",mSignsVal).
   mSignsVal = getxattrvalue ("loan",loan.contract + "," + loan.cont-code,"acct-dep").
   RUN Insert_TTName("acct-dep", SUBSTRING(mSignsVal, 1, 20)).
   mSignsVal = STRING(DAY(loan.end-date),"99") + " " + ENTRY(MONTH(loan.end-date),{&Months}) + " " + STRING(YEAR(loan.end-date)).
   RUN Insert_TTName ("end-date",mSignsVal).
   FIND FIRST code WHERE
              code.class EQ "ТипДогП"
          AND code.code  EQ loan.cont-type
   NO-LOCK NO-ERROR.
   IF AVAIL code AND code.parent NE "" THEN
   DO:
      RUN Insert_TTName("cont-type",code.description[1]).
   END.

   /* Счета */
   if iStr <> "depSSPIP" then do:
      FIND FIRST loan-acct WHERE loan-acct.contract   EQ loan.contract
                             and loan-acct.cont-code  EQ loan.cont-code
                             and loan-acct.acct-type  EQ "Депоз"
                             NO-LOCK NO-ERROR.
      if avail loan-acct then do:
         RUN Insert_TTName("AcctSsuda", entry(1, IF (AVAIL loan-acct) THEN loan-acct.acct ELSE mSignsVal,"@")).
      end.
      else do:
         mSignsVal = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "acct-dep").
         RUN Insert_TTName("AcctSsuda", SUBSTRING(mSignsVal, 1, 20)).
      end.
      FIND FIRST loan-acct WHERE loan-acct.contract   EQ loan.contract
                             and loan-acct.cont-code  EQ loan.cont-code
                             and loan-acct.acct-type  EQ "ДепРасч"
                             NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-acct THEN do:
         mDepRasch = no.
/*       if iStr <> "univ" then do:
            message "Для договора  " loan.cont-code " не найдей счёт с ролью ДепРасч"   view-as alert-box.
            RETURN.
         end.
*/
         RUN Insert_TTName("SimAcct1", "").
         RUN Insert_TTName("SimAcct2", "").
         RUN Insert_TTName("SimAcct3", "").
         RUN Insert_TTName("SimAcct4", "").
         RUN Insert_TTName("SimAcct5", "").
         RUN Insert_TTName("SimAcct6", "").
         RUN Insert_TTName("SimAcct7", "").
         RUN Insert_TTName("SimAcct8", "").
         RUN Insert_TTName("SimAcct9", "").
         RUN Insert_TTName("SimAcct10", "").
         RUN Insert_TTName("SimAcct11", "").
         RUN Insert_TTName("SimAcct12", "").
         RUN Insert_TTName("SimAcct13", "").
         RUN Insert_TTName("SimAcct14", "").
         RUN Insert_TTName("SimAcct15", "").
         RUN Insert_TTName("SimAcct16", "").
         RUN Insert_TTName("SimAcct17", "").
         RUN Insert_TTName("SimAcct18", "").
         RUN Insert_TTName("SimAcct19", "").
         RUN Insert_TTName("SimAcct20", "").
         tDepRasch = "".
         RUN Insert_TTName("uacct", "").
         RUN Insert_TTName("NameOrg","").
         RUN Insert_TTName("AdrUr","").
         RUN Insert_TTName("AdrFact","").
         RUN Insert_TTName("AdrPoht","").
      end.
      else do:
         RUN Insert_TTName("SimAcct1", substr(entry(1,loan-acct.acct,"@"),1,1)).
         RUN Insert_TTName("SimAcct2", substr(entry(1,loan-acct.acct,"@"),2,1)).
         RUN Insert_TTName("SimAcct3", substr(entry(1,loan-acct.acct,"@"),3,1)).
         RUN Insert_TTName("SimAcct4", substr(entry(1,loan-acct.acct,"@"),4,1)).
         RUN Insert_TTName("SimAcct5", substr(entry(1,loan-acct.acct,"@"),5,1)).
         RUN Insert_TTName("SimAcct6", substr(entry(1,loan-acct.acct,"@"),6,1)).
         RUN Insert_TTName("SimAcct7", substr(entry(1,loan-acct.acct,"@"),7,1)).
         RUN Insert_TTName("SimAcct8", substr(entry(1,loan-acct.acct,"@"),8,1)).
         RUN Insert_TTName("SimAcct9", substr(entry(1,loan-acct.acct,"@"),9,1)).
         RUN Insert_TTName("SimAcct10", substr(entry(1,loan-acct.acct,"@"),10,1)).
         RUN Insert_TTName("SimAcct11", substr(entry(1,loan-acct.acct,"@"),11,1)).
         RUN Insert_TTName("SimAcct12", substr(entry(1,loan-acct.acct,"@"),12,1)).
         RUN Insert_TTName("SimAcct13", substr(entry(1,loan-acct.acct,"@"),13,1)).
         RUN Insert_TTName("SimAcct14", substr(entry(1,loan-acct.acct,"@"),14,1)).
         RUN Insert_TTName("SimAcct15", substr(entry(1,loan-acct.acct,"@"),15,1)).
         RUN Insert_TTName("SimAcct16", substr(entry(1,loan-acct.acct,"@"),16,1)).
         RUN Insert_TTName("SimAcct17", substr(entry(1,loan-acct.acct,"@"),17,1)).
         RUN Insert_TTName("SimAcct18", substr(entry(1,loan-acct.acct,"@"),18,1)).
         RUN Insert_TTName("SimAcct19", substr(entry(1,loan-acct.acct,"@"),19,1)).
         RUN Insert_TTName("SimAcct20", substr(entry(1,loan-acct.acct,"@"),20,1)).

         FIND FIRST acct WHERE acct.acct   EQ loan-acct.acct
                               NO-LOCK NO-ERROR.
         IF NOT AVAIL acct THEN do:
            message "Счёт " loan-acct.acct  " не найдей"   view-as alert-box.
            RETURN.
         end.
         tDepRasch =loan-acct.acct.
         RUN Insert_TTName("uacct", entry(1,loan-acct.acct,"@")).

         RUN Insert_TTName("NameOrg",acct.details).

         tNameShort = acct.details.

         RUN RetAdr.p(acct.cust-id,  "Ч", "АдрПроп", ?, OUTPUT mAdrReg).
         RUN Insert_TTName("AdrUr",mAdrReg).

         RUN RetAdr.p(acct.cust-id,  "Ч", "АдрФакт", ?, OUTPUT mAdrFact).
         RUN Insert_TTName("AdrFact",mAdrFact).

         RUN RetAdr.p(acct.cust-id,  "Ч", "АдрПочт", ?, OUTPUT mAdrFact).
         RUN Insert_TTName("AdrPoht",mAdrFact).
      end.
   end.

   /* Договора */

   IF loan.cust-cat EQ "Ю" THEN
   DO:
      FIND FIRST cust-corp WHERE
               cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "ФИОРук",?).
      RUN Insert_TTName("FIORuk", mSignsVal).
      /* разделение на Ф, И и О */
      RUN Insert_TTName("Ф", ENTRY(1,mSignsVal,"")).
      RUN Insert_TTName("И", ENTRY(2,mSignsVal,"")).
      IF NUM-ENTRIES(mSignsVal,"") GE 3 THEN
         RUN Insert_TTName("О", SUBSTRING(mSignsVal,INDEX(mSignsVal,ENTRY(3,mSignsVal,"")))).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "ДолРук",?).
      RUN Insert_TTName("DolRuk", mSignsVal).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "CID",?).
      RUN Insert_TTName("CID", mSignsVal).
      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "основа",
                        ?).
      RUN Insert_TTName("Osnova", mSignsVal).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "МестСведПред",
                        "").
      RUN Insert_TTName("MestSvedPred", mSignsVal).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "ОргСведПред",
                        "").
      RUN Insert_TTName("OrgSvedPred", mSignsVal).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "ДолРукРП",?).
      IF mSignsVal EQ ? THEN DO:
         mSignsVal = GetXAttrValueEx("cust-corp",
                           STRING(cust-corp.cust-id),
                           "ДолРук",?).
         IF mSignsVal MATCHES "*енеральный директор" THEN RUN Insert_TTName("DolRukRP", "Генерального директора").
      END.
      RUN Insert_TTName("DolRukRP", mSignsVal).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "ФИОРукРП",?).
      IF mSignsVal EQ ? THEN DO:
         mSignsVal = GetXAttrValueEx("cust-corp",
                           STRING(cust-corp.cust-id),
                           "ФИОРук",?).
      END.
      RUN Insert_TTName("FIORukRP", mSignsVal).

      mNBuh1 = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "фиобухг", "").
      IF mNBuh1 ="" THEN
         mNBuh3="Лицо,   наделенное правом второй подписи, отсутствует".
      ELSE
         mNBuh3=mNBuh1.
      RUN Insert_TTName("FIOBuhg", mNBuh3).

      mNBuh2 = GetXAttrValueEx("cust-corp",
                       STRING(cust-corp.cust-id),
                       "фиобухг",
                       "").
      IF mNBuh2 ="" THEN
         mNBuh4="Нет".
      ELSE
         mNBuh4=mNBuh2.
      RUN Insert_TTName("FIObg", mNBuh4).

      mSignsVal = GetXAttrValueEx("cust-corp",
                         STRING(cust-corp.cust-id),
                         "ОГРН",
                         "").
      RUN Insert_TTName("OGRN", mSignsVal).
      mSignsVal = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "КПП", "").
      RUN Insert_TTName("KPP", mSignsVal).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "RegPlace",
                        "").
      RUN Insert_TTName("RegPlace", mSignsVal).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "УчДокГр",
                        "").
      IF NUM-ENTRIES(mSignsVal) GE 2 THEN
         RUN Insert_TTName("UhDokGr", ENTRY(1,mSignsVal) + " №" + ENTRY(2,mSignsVal)).

      mDatTMP = DATE(GetXAttrValueEx("cust-corp",
                           STRING(cust-corp.cust-id),
                           "ДатаОГРН",
                           "")) NO-ERROR.
      RUN Insert_TTName("DataOGRN", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " г.").

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "УчДок",
                        "").
      IF NUM-ENTRIES(mSignsVal) GE 2 THEN
         RUN Insert_TTName("UcDoc", ENTRY(1,mSignsVal) + " №" + ENTRY(2,mSignsVal)).

      mDatTMP = DATE(GetXAttrValueEx("cust-corp",
                           STRING(cust-corp.cust-id),
                           "УчДокДата",
                           "")) NO-ERROR.
      RUN Insert_TTName("UhDokData", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " г.").

      mDatTMP = DATE(GetXAttrValueEx("cust-corp",
                     STRING(cust-corp.cust-id),
                           "RegDate",
                           "")) NO-ERROR.
      RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")).

      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "tel",
                        "").
      RUN Insert_TTName("tel", mSignsVal).
      mSignsVal = GetXAttrValueEx("cust-corp",
                        STRING(cust-corp.cust-id),
                        "fax",
                        "").
      RUN Insert_TTName("fax", mSignsVal).
      RUN Insert_TTName("INN",cust-corp.inn).

      RUN GetCustName IN h_Base (loan.cust-cat,
                        loan.cust-id,
                        "",
                        OUTPUT mName[1],
                        OUTPUT mName[2],
                        INPUT-OUTPUT mInn) NO-ERROR.


      RUN Insert_TTName("stat",cust-corp.cust-stat).

      /* Поиск полной формы Субъект */
      FIND FIRST code WHERE
                 code.class =  "КодПредп"
             AND code.val   =  cust-corp.cust-stat
      NO-LOCK NO-ERROR.
      tStatus = "".
      IF AVAIL code THEN do:
         tStatus = trim(code.name)  + " " .
         RUN Insert_TTName("StatAll",trim(code.name)).
      end.
      RUN Insert_TTName("NameFull",tStatus + cust-corp.name-corp).
      RUN Insert_TTName("NameOrg",cust-corp.name-corp).
      RUN Insert_TTName("okpo",cust-corp.okpo).
      tNameShort = cust-corp.name-short.
      RUN RetAdr.p(loan.cust-id,  "Ю", "АдрЮр", "", OUTPUT mAdrReg) NO-ERROR.
      RUN Insert_TTName("AdrUr",mAdrReg).
      RUN RetAdr.p(loan.cust-id,  "Ю", "АдрФакт", "", OUTPUT mAdrFact) NO-ERROR.
      RUN Insert_TTName("AdrFact",mAdrFact).
      RUN Insert_TTName("mAdrFact",mAdrFact).

      RUN RetAdr.p(loan.cust-id,  "Ю", "АдрПочт", "", OUTPUT mAdrFact) NO-ERROR.
      RUN Insert_TTName("AdrPoht",mAdrFact).
   END.
   IF loan.cust-cat = "Ч" THEN
   DO:

      /* Данные клиента */
      FIND FIRST person WHERE
               person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL person THEN RETURN.

      mSignsVal = person.name-last + " " + person.first-names.

      RUN Insert_TTName("FIORuk"    ,mSignsVal).
      RUN Insert_TTName("StatAll"   ,"Индивидуальный прадприниматель").
      RUN Insert_TTName("NameOrg"   ,"Индивидуальный прадприниматель" + " " + mSignsVal).

      tNameShort = "ИП " + mSignsVal.

      RUN Insert_TTName("NameFull"  ,"Индивидуальный прадприниматель" + " " + mSignsVal).

      /* разделение на Ф, И и О */
      RUN Insert_TTName("Fam", ENTRY(1,mSignsVal,"")).
      RUN Insert_TTName("Nam", ENTRY(2,mSignsVal,"")).
      IF NUM-ENTRIES(mSignsVal,"") GE 3 THEN
      RUN Insert_TTName("Fat", SUBSTRING(mSignsVal,INDEX(mSignsVal,ENTRY(3,mSignsVal,"")))).
      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "ДолРук",?).
      RUN Insert_TTName("DolRuk", mSignsVal).

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "CID",?).
      RUN Insert_TTName("CID", mSignsVal).
      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "основа",
                        ?).
      RUN Insert_TTName("Osnova", mSignsVal).

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "МестСведПред",
                        "").
      RUN Insert_TTName("MestSvedPred", mSignsVal).

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "ОргСведПред",
                        "").
      RUN Insert_TTName("OrgSvedPred", mSignsVal).

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "ДолРукРП",?).
      IF mSignsVal EQ ? THEN DO:
         mSignsVal = GetXAttrValueEx("person",
                           STRING(person.person-id),
                           "ДолРук",?).
         IF mSignsVal MATCHES "*енеральный директор" THEN RUN Insert_TTName("DolRukRP", "Генерального директора").
      END.
      RUN Insert_TTName("DolRukRP", mSignsVal).

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "ФИОРукРП",?).
      IF mSignsVal EQ ? THEN DO:
         mSignsVal = GetXAttrValueEx("person",
                           STRING(person.person-id),
                           "ФИОРук",?).
      END.
      RUN Insert_TTName("FIORukRP", mSignsVal).
      mNBuh1 = GetXAttrValueEx("person",
                 STRING(person.person-id),
                 "фиобухг",
                 "").
      IF mNBuh1 ="" THEN
         mNBuh3="Лицо,   наделенное правом второй подписи, отсутствует".
      ELSE
         mNBuh3=mNBuh1.
      RUN Insert_TTName("FIOBuhg", mNBuh3).

      mNBuh2 = GetXAttrValueEx("person",
                       STRING(person.person-id),
                       "фиобухг",
                       "").
      IF mNBuh2 ="" THEN
         mNBuh4="Нет".
      ELSE
         mNBuh4=mNBuh2.
      RUN Insert_TTName("FIObg", mNBuh4).

      mSignsVal = GetXAttrValueEx("person",
                         STRING(person.person-id),
                         "ОГРН",
                         "").
      RUN Insert_TTName("OGRN", mSignsVal).
      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "КПП",
                  "").
      RUN Insert_TTName("KPP", mSignsVal).

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "МестСведПред",
                        "").
      RUN Insert_TTName("RegPlace", mSignsVal).

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "УчДокГр",
                        "").
      IF NUM-ENTRIES(mSignsVal) GE 2 THEN
         RUN Insert_TTName("UhDokGr", ENTRY(1,mSignsVal) + " №" + ENTRY(2,mSignsVal)).

      mDatTMP = DATE(GetXAttrValueEx("person",
                           STRING(person.person-id),
                           "ДатаОГРН",
                           "")) NO-ERROR.
      RUN Insert_TTName("DataOGRN", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " г.").

      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "УчДок",
                        "").
      IF NUM-ENTRIES(mSignsVal) GE 2 THEN
         RUN Insert_TTName("UcDoc", ENTRY(1,mSignsVal) + " №" + ENTRY(2,mSignsVal)).

      mDatTMP = DATE(GetXAttrValueEx("person",
                           STRING(person.person-id),
                           "УчДокДата",
                           "")) NO-ERROR.
      RUN Insert_TTName("UhDokData", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".") + " г.").

      mDatTMP = DATE(GetXAttrValueEx("person",
                     STRING(person.person-id),
                           "RegDate",
                           "")) NO-ERROR.
      RUN Insert_TTName("RegDate", REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")).

      mSignsVal = person.Phone[1].
      RUN Insert_TTName("tel", mSignsVal).
      mSignsVal = GetXAttrValueEx("person",
                        STRING(person.person-id),
                        "fax",
                        "").
      RUN Insert_TTName("fax", mSignsVal).
      RUN Insert_TTName("INN",person.inn).
      RUN Insert_TTName("tax-insp",person.tax-insp).
      mSignsVal = "Индивидуальный предприниматель " + person.name-last + " " + person.first-names + ", " + string(person.birthday,"99.99.9999") + " г.р.".
      RUN Insert_TTName("client", mSignsVal).
      mSignsVal = mAdrReg.
      RUN Insert_TTName("mAdrReg",mSignsVal).
   END. /*Физическое лицо*/


/*  term-obl */

   FIND FIRST term-obl WHERE term-obl.contract  EQ loan.contract
                         AND term-obl.cont-code EQ loan.cont-code
                         AND term-obl.end-date  EQ loan.open-date
                         AND term-obl.idnt EQ 2 NO-LOCK NO-ERROR.
      mOst = 0.
      IF AVAILABLE term-obl THEN do:
         mOst = term-obl.amt-rub.
         RUN Insert_TTName("mOst",mOst).
         RUN x-amtstr.p(mOst,loan.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
         i = num-entries(SummaStr[1]," ").
         mSignsVal = entry(i,SummaStr[1]," ").
         SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
         RUN Insert_TTName("ost00dog", string(TRUNC(mOst, 0),'>>>>>>>>>>>9')).
         SummaStr[1] = '(' + SummaStr[1] + ') ' + mSignsVal + ' ' + SummaStr[2].
         RUN Insert_TTName("SummaStrDog", SummaStr[1]).
      end.
/* валюта */
   find first currency where currency.currency eq loan.currency no-lock no-error.
   tCurrName = "".
   if avail currency then do:
      tCurrName = trim(currency.name-currenc).
   end.
   RUN Insert_TTName("tCurrName", STRING(tCurrName)).
/* Ставка */

   find first comm-rate where comm-rate.filial-id  = shfilial
                       and comm-rate.branch-id  = ""
                       and comm-rate.commission = "%Деп"
                       and comm-rate.acct       = "0"
                       and comm-rate.currency   = loan.currency
                       and comm-rate.kau        = loan.contract + "," + loan.cont-code
                       and comm-rate.min-value  = 0
                       and comm-rate.period     = 0
                       and comm-rate.since      = loan.open-date
                       no-error.
   if avail comm-rate then do:
      mCommRate = comm-rate.rate-comm.
      RUN Insert_TTName("CommPrc", STRING(mCommRate, '>9.99')).
      RUN x-amtstr.p(mCommRate, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
      out_proc = STRING(mAmtStr) + "целых ".
      RUN x-amtstr.p (DEC(mDecStr),"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
      out_proc = out_proc + LC(STRING(mAmtStr)) + "сотых ".
      RUN Insert_TTName("CommPrcStr", out_proc).
   end.

   find first comm-rate where comm-rate.filial-id  = shfilial
                       and comm-rate.branch-id  = ""
                       and comm-rate.commission = "НеснОст"
                       and comm-rate.acct       = "0"
                       and comm-rate.currency   = loan.currency
                       and comm-rate.kau        = loan.contract + "," + loan.cont-code
                       and comm-rate.min-value  = 0
                       and comm-rate.period     = 0
                       and comm-rate.since      = loan.open-date
                       no-error.
   if avail comm-rate then do:
      ost = comm-rate.rate-comm.
      mSignsVal = string(comm-rate.rate-comm).
      RUN Insert_TTName("stavkaNO",mSignsVal).
      RUN x-amtstr.p(comm-rate.rate-comm,loan.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
      i = num-entries(SummaStr[1]," ").
      mrub = entry(i,SummaStr[1]," ").
      SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
      RUN Insert_TTName("ost00", string(TRUNC(ost, 0),'>>>>>>>>>>>9')).
      IF TRUNC(ost, 0) = ost THEN do:
         SummaStr[2] = ''.
      end.
      SummaStr[1] = '(' + SummaStr[1] + ') ' + mrub + ' ' + SummaStr[2].
      RUN Insert_TTName("SummaStr", SummaStr[1]).

   end.

/* Срок */
   FIND FIRST loan-cond
      WHERE loan-cond.contract EQ loan.contract
          AND loan-cond.cont-code EQ loan.Cont-Code
          EXCLUSIVE-LOCK NO-ERROR.
   mSignsVal = GetXAttrValue("loan-cond",
                           loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                           "NDays").
   vDayNum = int(substr(mSignsVal,length(mSignsVal),1)).
   IF vDayNum EQ 1 THEN
       mSignsVal = mSignsVal + " день ".
   IF CAN-DO("2,3,4,",string(vDayNum)) THEN
       mSignsVal = mSignsVal + " дня ".
   IF NOT CAN-DO("0,1,2,3,4",STRING(vDayNum)) THEN
       mSignsVal = mSignsVal + " дней ".

   RUN Insert_TTName("NDays",mSignsVal).

   FIND FIRST code WHERE
              code.class EQ "int-period"
          AND code.code  EQ loan-cond.int-period
   NO-LOCK NO-ERROR.
   IF AVAIL code AND code.parent NE "" THEN
   DO:
      RUN Insert_TTName("int-period",code.description[1]).
   END.

/* Платёжные инструкции */
   mben-acct        = "".
   mben-inn         = "".
   mben-cust-name   = "".
   mben-bank-code   = "".
   mben-bank-name   = "".
   vOtherB = GetXattrValueEx("loan",
                             loan.contract + "," + loan.cont-code,
                             "ДрБанк",
                             ?).
   IF vOtherB EQ ? THEN
      RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                     loan.open-date,
                                     loan.open-date,
                                     "ДрБанк",
                                     output vOtherB).
   IF vOtherB EQ "ДА" THEN DO:

      vSettlInstrID = GetSettlInstr("loan",
                                    loan.contract + "," + loan.cont-code,
                                    loan.currency,
                                    {&DIRECT_OBL},
                                    loan.open-date,
                                    loan.open-date,
                                    ?).
      IF vSettlInstrID <> ? THEN do:

         FIND FIRST settl-instr WHERE settl-instr.settl-instr-id EQ vSettlInstrID
            NO-LOCK NO-ERROR.

         RUN RE_C_ROLE IN h_cRole ("settl-instr",
                                   STRING(vSettlInstrID),
                                   "Benef-Cust",
                                   loan.open-date,
                                   BUFFER bCustRole).
         IF AVAIL bCustRole THEN
         DO:
            mben-acct        = bCustRole.corr-acct.
            mben-inn         = bCustRole.inn.
            mben-cust-name   = bCustRole.cust-name.
         END.
         RUN RE_C_ROLE IN h_cRole ("settl-instr",
                                   STRING(vSettlInstrID),
                                   "Acct-With-Inst",
                                   loan.open-date,
                                   BUFFER bCustRole).
         IF AVAIL bCustRole THEN
         DO:
            mben-bank-code   = bCustRole.cust-code.
            mben-bank-name   = bCustRole.cust-name.
         END.
      end.
   end. /* платёжные инструкции */
   else do:
      mben-acct        = entry(1,tDepRasch,"@").
      find first acct where acct.acct = tDepRasch no-lock no-error.
      if avail acct then do:
         IF acct.cust-cat = "Ч" THEN do:
            FIND FIRST person WHERE person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
            IF AVAIL person THEN do:
               mben-inn         = person.inn.
               mben-cust-name   = "ИП " + person.name-last + " " + person.first-names.
               find first branch where branch.branch-id = acct.branch-id.
               if avail branch then do:
                  mben-bank-code = GetXattrValue("branch",STRING(branch.Branch-Id),"БанкМФО").
                  mben-bank-name = branch.name.
               end.
            end.
         end.
         IF acct.cust-cat = "Ю" THEN do:
            FIND FIRST cust-corp WHERE
                     cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL cust-corp THEN do:
               mben-inn         = cust-corp.inn.
               mben-cust-name   = cust-corp.name-short.
               find first branch where branch.branch-id = acct.branch-id.
               if avail branch then do:
                  mben-bank-code = GetXattrValue("branch",STRING(branch.Branch-Id),"БанкМФО").
                  mben-bank-name = branch.name.
               end.
            end.
         end.
      end.
   end.
   RUN Insert_TTName("ben-acct",mben-acct).
   RUN Insert_TTName("ben-inn" ,mben-inn).
   RUN Insert_TTName("ben-cust-name",mben-cust-name).
   RUN Insert_TTName("ben-bank-code",mben-bank-code).
   RUN Insert_TTName("ben-bank-name",mben-bank-name).
END.
/* Если переменная может вычисляться несколькими способами, то добавлять во временную таблицу её НУЖНО только ОДИН раз,
поскольку переменная с одним и те же именем добавляется во временную таблицу столько раз, сколько раз её добавляют,
а не изменяет предыдущее значение. А при выводе на печать берётся первое значение. */

RUN Insert_TTName("NameShort" ,tNameShort).


IF mError EQ "" THEN
DO:
  RUN printvd.p (iStr,INPUT TABLE ttnames).
END.
ELSE
   RUN Fill-SysMes("","","0",mError).
