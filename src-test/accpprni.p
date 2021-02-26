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

{globals.i}
/* подключаем ttnames */
{prn-doc.def &with_proc=YES}
/* подключаем tmprecid */
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{dpsproc.def}
{ksh-defs.i NEW}


FUNCTION strwrap RETURN CHAR (INPUT iStr AS CHAR , INPUT iFirst AS INT, INPUT iAll AS INT):
   DEFINE VAR  iStr1  AS CHARACTER NO-UNDO.
   DEFINE VAR  iStr2  AS CHARACTER NO-UNDO.
   DEFINE VAR  iStrF  AS CHARACTER NO-UNDO.
   DEFINE VAR  iStrS  AS CHARACTER NO-UNDO.
   DEFINE VAR  i1     AS int       NO-UNDO.
   DEFINE VAR  i2     AS int       NO-UNDO.
   DEFINE VAR  i3     AS int       NO-UNDO.
   DEFINE VAR  i4     AS int       NO-UNDO.
   DEFINE VAR  i5     AS int       NO-UNDO.
   iStr = trim(iStr).
   i1 = num-entries(iStr," ").
   iStr2 = "".
   i3 = 1.
   do i2 = 1 to i1 :
      i4 = INDEX(iStr," ",i3).
      if i4 = 0 then i4 = iAll + i3.
      iStr1 = substr(iStr,i3,i4 - i3).
      if length(iStr2 + " " + iStr1) > iFirst then leave.
      iStr2 = trim(iStr2 + " " + iStr1).     
      i3 = i4 + 1.
   end.
   iStrF = iStr2.
   iStr = trim(substr(iStr,i3)).
   i1 = num-entries(iStr," ").
   iStr2 = "".
   i3 = 1.
   do i2 = 1 to i1 :
      i4 = INDEX(iStr," ",i3).
      if i4 = 0 then i4 = iAll + i3.
      iStr1 = substr(iStr,i3,i4 - i3).
      if length(iStr2 + " " + iStr1) > iAll then do:
         iStrS = iStrS + "|" + iStr2 .
         iStr2 = iStr1.  
         i3 = i4 + 1.
      end.
      else do:
         iStr2 = trim(iStr2 + " " + trim(iStr1)).     
         i3 = i4 + 1.
      end.
   end.
   iStrS =  trim(iStrS) + "|" + trim(iStr2).
   iStr1 = iStrF +  iStrS.
   return iStr1.
END FUNCTION.
   

DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.

DEFINE VARIABLE mName	     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mInn	     AS CHARACTER 			NO-UNDO. /* Ю Ч*/
DEFINE VARIABLE mAdrReg      AS CHARACTER 			NO-UNDO. /*АдрЮр*/
DEFINE VARIABLE mAdrFact     AS CHARACTER 			NO-UNDO. /*АдрФакт*/
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
DEFINE VARIABLE mList_Acct   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mCurrency    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE shfilial_    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE ost          AS decimal            NO-UNDO.
DEFINE VARIABLE mBegDate     AS date               NO-UNDO.   
DEFINE VARIABLE mEndDate     AS date               NO-UNDO.   
DEFINE VARIABLE i            AS int                NO-UNDO.   
DEFINE VARIABLE mSh-Db       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-Cr       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vDb      AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vCr      AS dec                NO-UNDO.   
DEFINE VARIABLE mList_cont   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mBalance     AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK1   AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK2   AS dec                NO-UNDO.   
DEFINE VARIABLE StrBalanceK1 AS char               NO-UNDO.   
DEFINE VARIABLE StrBalanceK2 AS char               NO-UNDO.   
DEFINE VARIABLE namestat     AS char               NO-UNDO.   
DEFINE VARIABLE mtoday       AS date               NO-UNDO.   
DEFINE VARIABLE mSumma       AS dec                NO-UNDO.   
DEFINE VARIABLE mBlock       AS dec                NO-UNDO.   
DEFINE VARIABLE sBlock      AS char               NO-UNDO.   

DEFINE VARIABLE mNBuh1       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh3       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh2       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mNBuh4       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mIniFio      AS CHARACTER   NO-UNDO.


DEFINE VARIABLE var-respect-b-var    AS CHARACTER   NO-UNDO.  
DEFINE VARIABLE var-respect-s-var    AS CHARACTER   NO-UNDO.  
DEFINE VARIABLE var-dead-var         AS CHARACTER   NO-UNDO.  
DEFINE VARIABLE var-resid-var        AS CHARACTER   NO-UNDO.  

DEF VAR vDovTDoc   AS CHAR NO-UNDO.
DEF VAR vDovNDoc   AS CHAR NO-UNDO.
DEF VAR vTmpDat    AS Date NO-UNDO.
DEF VAR vDovDDoc   AS CHAR NO-UNDO.
DEF VAR vDovKDoc   AS CHAR NO-UNDO.


DEF BUFFER bacct for acct.

PAUSE 0.

&GLOB Months-i "январь,февраль,март,апрель,май,июнь,июль,август,сентябрь,~
октябрь,ноябрь,декабрь"
&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"
&GLOB Days "первое,второе,третье,четвертое,пятое,шестое,седьмое,восьмое,девятое,десятое,одиннадцатое,двенадцатое,тринадцатое,четырнадцатое,~
пятнадцатое,шестнадцатое,семнадцатое,восемнадцатое,девятнадцатое,двадцатое,двадцать первое,двадцать второе,двадцать третье,~
двадцать четвертое,двадцать пятое,двадцать шестое,двадцать седьмое,двадцать восьмое,двадцать девятое,тридцатое,тридцать первое"
&GLOB Years "первого,второго,третьего,четвертого,пятого,шестого,седьмого,восьмого,девятого,десятого,одинадцатого,двенадцатого,тринадцатого,~
четырнадцатого,пятнадцатого,шестнадцатого,семнадцатого,восемнадцатого,девятнацатого,двадцатого,двадцать первого,двадцать первого,~
двадцать второго,двадцать третьего,двадцать четвертого,двадцать пятого,шестого,двадцать седьмого,двадцать восьмого,двадцать девятого,тридцатого,тридцать первого,~
тридцать первого,тридцать второго,тридцать третьего,тридцать четвертого,тридцать пятого,тридцать шестого,тридцать седьмого,тридцать восьмого,тридцать девятого"

mtoday = today.

DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").   

find first _user where _user._userid = user_ no-lock no-error.
RUN Insert_TTName("Pod-isp-f", _user._User-Name).

iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
str_title = "[ Выберите подписанта (" + user_ + " " + iBranch + ")]".
mUser = "".


run signat.p ("accpprni","UserName",str_title,OUTPUT mUser).

if mUser = ? then return.

RUN Insert_TTName("Pod-user-dolg", GetXAttrValueEx("_User", mUser, "Должность", "")).
RUN Insert_TTName("Pod-user-fio", GetXAttrValueEx("_User", mUser, "ФИОП", "")).
RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "ДокОснТипРП", "")).
RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "Телефон", "")).
RUN Insert_TTName("Pod-user-podr", GetXAttrValueEx("_User", mUser, "ОтделРП", "")).
RUN Insert_TTName("Pod-user-otdel-rp", GetXAttrValueEx("_User", mUser, "ОтделРП", "")).
mIniFio = GetXAttrValueEx("_User", mUser, "ФИОП", "").
mIniFio = Caps(Substr(trim(entry(2,mIniFio," ")),1,1)) + "." + Caps(Substr(trim(entry(3,mIniFio," ")),1,1)) + "." + trim(entry(1,mIniFio," ")).
RUN Insert_TTName("Pod-user-ini-fio",  mIniFio ).

find first _user where _user._userid = mUser no-lock no-error.
RUN Insert_TTName("Pod-user-f", _user._User-Name).
RUN Insert_TTName("Pod-isp-dolg", GetXAttrValueEx("_User", User_, "Должность", "")).
RUN Insert_TTName("Pod-isp-fio", GetXAttrValueEx("_User", User_, "ФИОП", "")).
RUN Insert_TTName("Pod-isp-dov-rp", GetXAttrValueEx("_User", User_, "ДокОснТипРП", "")).
RUN Insert_TTName("Pod-isp-telefon", GetXAttrValueEx("_User", User_, "Телефон", "")).
mIniFio = GetXAttrValueEx("_User", User_, "ФИОП", "").
mIniFio = Caps(Substr(trim(entry(2,mIniFio," ")),1,1)) + "." + Caps(Substr(trim(entry(3,mIniFio," ")),1,1)) + "." + trim(entry(1,mIniFio," ")).
RUN Insert_TTName("Pod-isp-ini-fio",  mIniFio ).


RUN Insert_TTName("gend-date", term2str(gend-date, gend-date)).
/*наименование и адрес банка*/
FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("go-bank-name",    branch.name). 
   RUN Insert_TTName("go-bank-address", branch.Address). 
   RUN Insert_TTName("go-gorod", entry(2,branch.Address)). 
END.

shfilial_ = GetXAttrValueEx("_User", mUser, "filial-id", "").

FIND FIRST branch WHERE branch.Branch-Id EQ shfilial_ NO-LOCK NO-ERROR.

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
END.

/* запрос даты */
if iStr = "AcctMom" or iStr = "AcctMomC" then do: 
   {getdates.i &beglabel="Дата С" &endlabel="Дата ПО"}
   if end-date - beg-date > 397  then do:
      message "Отчёт можно получить не более чем за год" view-as alert-box.
      return.
   end.
end.
if iStr = "AcctRest" or iStr = "AcctIs"  or iStr = "AcctPar" then do: 
   {getdate.i}
   mtoday = end-date.
end.
RUN Insert_TTName("Today",mtoday).
RUN Insert_TTName("TodayT",string(mtoday,"99.99.9999")).
RUN Insert_TTName("TodayStr",  STRING(DAY(mtoday))  + " " +  ENTRY(MONTH(mtoday),{&Months}) +  " " + STRING(YEAR(mtoday)) + " г.").



/* цикл по рецидам выбранных счетов */
FOR EACH tmprecid NO-LOCK, 
   FIRST acct WHERE RECID(acct) EQ tmprecid.id NO-LOCK :
/* Счет */
      RUN Insert_TTName("uacct", entry(1,acct.acct,"@")).
      RUN Insert_TTName("Date-in",acct.open-date).
      RUN Insert_TTName("Date-in-pr",  STRING(DAY(acct.open-date))  + " " +  ENTRY(MONTH(acct.open-date),{&Months}) +  " " + STRING(YEAR(acct.open-date)) + " года.").
      RUN Insert_TTName("Date-cl",REPLACE(STRING(acct.close-date, "99/99/9999"), "/", ".") + " г.").

/* Остатки */
/* Остаток исходящий */
     run acct-pos in h_base (acct.acct,
                             acct.currency,
                             mtoday, mtoday, ?).
     ost = abs((if acct.currency = "" then sh-bal else sh-val)).
     if iStr = "AcctPar" then do: 
        FORM
          " " skip
          mSumma form ">>>>>>>>>>>9.99" LABEL " Сумма взноса   " skip
          " "
          WITH FRAME d1
               CENTERED SIDE-LABELS
               title " Начальная информация ".
        
        update mSumma with frame d1 overlay row 4.
        hide frame d1.
        {on-esc LEAVE}
        ost = mSumma.
     end.
     RUN x-amtstr.p(ost,acct.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
     i = num-entries(SummaStr[1]," ").
     mSignsVal = entry(i,SummaStr[1]," ").
     SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
     RUN Insert_TTName("ost00", string(TRUNC(ost, 0),'>>>>>>>>>>>9')).
     SummaStr[1] = '(' + SummaStr[1] + ') ' + mSignsVal + ' ' + SummaStr[2].
     RUN Insert_TTName("SummaStr00", SummaStr[1]).

/* Остаток входящий */
     run acct-pos in h_base (acct.acct,
                             acct.currency,
                             mtoday - 1, mtoday - 1, ?).
     ost = abs((if acct.currency = "" then sh-bal else sh-val)).
     if iStr = "AcctPar" then do: 
        FORM
          " " skip
          mSumma form ">>>>>>>>>>>9.99" LABEL " Сумма взноса   " skip
          " "
          WITH FRAME d1
               CENTERED SIDE-LABELS
               title " Начальная информация ".
        
        update mSumma with frame d1 overlay row 4.
        hide frame d1.
        {on-esc LEAVE}
        ost = mSumma.
     end.
     RUN x-amtstr.p(ost,acct.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
     i = num-entries(SummaStr[1]," ").
     mSignsVal = entry(i,SummaStr[1]," ").
     SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
     RUN Insert_TTName("ost00v", string(TRUNC(ost, 0),'>>>>>>>>>>>9')).
     SummaStr[1] = '(' + SummaStr[1] + ') ' + mSignsVal + ' ' + SummaStr[2].

     RUN Insert_TTName("SummaStr00v", SummaStr[1]).
     RUN Insert_TTName("SummaStr", SummaStr[1]).
 
/* обороты */


   if iStr = "AcctMom" or iStr = "AcctMomC" then do: 
      if acct.curr <> "" then do:
         find first currency where currency.currency = acct.curr no-lock no-error.
         if avail currency then do:
            RUN Insert_TTName("i-currency", currency.i-currency).
         end.
      end.
      mBegDate = beg-date.
      mSh-Db = 0.
      mSh-Cr = 0.
      mSh-vDb = 0.
      mSh-vCr = 0.
      do i = 1 to 13 .
         if MONTH(mBegDate) <> 12 then do:
            mEndDate = date("01/" + string(MONTH(mBegDate) + 1) + "/" + STRING(YEAR(mBegDate))) - 1.
            mBegDate = date("01/" + string(MONTH(mBegDate)) + "/" + STRING(YEAR(mBegDate))).
         end.
         else do:
            mEndDate = date("01/01/" + STRING(YEAR(mBegDate) + 1)) - 1.
            mBegDate = date("01/12/" + STRING(YEAR(mBegDate))).
         end.
         if mEndDate > end-date then mEndDate = end-date.
         if mBegDate < beg-date then mBegDate = beg-date.
         mSignsVal = ENTRY(MONTH(mBegDate),{&Months-i}) +  " " + STRING(YEAR(mBegDate)) + "г." .
         RUN acct-pos IN h_base ( acct.acct,
                                  acct.currency,
                                  mBegDate,
                                  mEndDate,
                                  gop-status
                                  ).
         if mBegDate > end-date then do:
            RUN Insert_TTName("sh-db" + string(i), "").
            RUN Insert_TTName("sh-cr" + string(i), "").
            RUN Insert_TTName("sh-vdb" + string(i), "").
            RUN Insert_TTName("sh-vcr" + string(i), "").
            RUN Insert_TTName("MONTH" + string(i), "").
         end.
         else do:
            mSh-Db = mSh-Db + sh-db.
            mSh-Cr = mSh-Cr + sh-cr.
            mSh-vDb = mSh-vDb + sh-vdb.
            mSh-vCr = mSh-vCr + sh-vcr.
            RUN Insert_TTName("sh-db" + string(i), string(sh-db,'>>>>>>>>>>>9.99')).
            RUN Insert_TTName("sh-cr" + string(i), string(sh-cr,'>>>>>>>>>>>9.99')).
            RUN Insert_TTName("sh-vdb" + string(i), string(sh-vdb,'>>>>>>>>>>>9.99')).
            RUN Insert_TTName("sh-vcr" + string(i), string(sh-vcr,'>>>>>>>>>>>9.99')).
            RUN Insert_TTName("MONTH" + string(i), mSignsVal).
         end.
         mBegDate = mEndDate + 1.
         if mEndDate >= end-date then leave.
      end.
      RUN Insert_TTName("msh-db", string(msh-db,'>>>>>>>>>>>9.99')).
      RUN Insert_TTName("msh-cr", string(msh-cr,'>>>>>>>>>>>9.99')).
      RUN Insert_TTName("msh-vdb", string(msh-vdb,'>>>>>>>>>>>9.99')).
      RUN Insert_TTName("msh-vcr", string(msh-vcr,'>>>>>>>>>>>9.99')).
      RUN Insert_TTName("end-date", string(end-date) + "г.").
      RUN Insert_TTName("beg-date", string(beg-date) + "г.").


   end.
   if iStr = "AcctRep" or iStr = "AcctIs" or iStr = "AcctRep1" then do: 

   /* Картотеки */
      mBalance = 0.
      mBalanceK1 = 0.
      mBalanceK2 = 0.
      FOR EACH bacct WHERE bacct.cust-cat EQ acct.cust-cat 
                       AND bacct.cust-id EQ acct.cust-id
                       AND bacct.close-date EQ ? 
                       AND (bacct.contract BEGINS "Карт2" 
                        or bacct.contract BEGINS "Карт1"
                        or bacct.contract BEGINS "КартБл")
                       no-lock.
         RUN acct-pos IN h_base (bacct.acct,bacct.currency,mtoday,mtoday,gop-status).
         ASSIGN
            mBalance = mBalance + sh-bal
         .
         if bacct.contract BEGINS "Карт1" or bacct.contract BEGINS "КартБл" then do:
            mBalanceK1 = mBalanceK1 + sh-bal .
         end.
         if bacct.contract BEGINS "Карт2" then do:
            mBalanceK2 = mBalanceK2 + sh-bal .
         end.
      end.

      if mBalance = 0 then do:
         RUN Insert_TTName("Kartoteki","").
      end.
      else do:
         RUN Insert_TTName("Kartoteki", " (ЕСТЬ ОСТАТКИ ПО СЧЕТАМ КАРТОТЕК) ").
      end.
      if mBalanceK1 = 0 then do:
         RUN Insert_TTName("StrBalanceK1","отсутствует").
      end.
      else do:
         ost = mBalanceK1.
         RUN x-amtstr.p(ost,acct.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
         i = num-entries(SummaStr[1]," ").
         mSignsVal = entry(i,SummaStr[1]," ").
         SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
         RUN Insert_TTName("mBalanceK1", string(TRUNC(ost, 0),'>>>>>>>>>>>9')).
         IF TRUNC(ost, 0) = ost THEN
         ASSIGN
            SummaStr[2] = ''
         .
         SummaStr[1] = ' составляет ' + string(TRUNC(ost, 0),">>>>>>>>>>>9") + ' (' + SummaStr[1] + ') '+ mSignsVal + ' ' + SummaStr[2].
         RUN Insert_TTName("StrBalanceK1", SummaStr[1]).
      end.
      if mBalanceK2 = 0 then do:
         RUN Insert_TTName("StrBalanceK2","отсутствует").
      end.
      else do:
         ost = mBalanceK2.
         RUN x-amtstr.p(ost,acct.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
         i = num-entries(SummaStr[1]," ").
         mSignsVal = entry(i,SummaStr[1]," ").
         SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
         RUN Insert_TTName("mBalanceK1", string(TRUNC(ost, 0),'>>>>>>>>>>>9')).
         IF TRUNC(ost, 0) = ost THEN
         ASSIGN
            SummaStr[2] = ''
         .
         SummaStr[1] = ' составляет ' + string(TRUNC(ost, 0),">>>>>>>>>>>9") + ' (' + SummaStr[1] + ') '+ mSignsVal + ' ' + SummaStr[2].
         RUN Insert_TTName("StrBalanceK2", SummaStr[1]).
      end.
   end.


   /* ссудная задолженность */
   if iStr = "AcctSsud" or iStr = "AcctRep" or iStr = "AcctRep1" then do: 

      for each loan where loan.contract = "Кредит"
                      and loan.cust-cat = acct.cust-cat
                      and loan.cust-id = acct.cust-id
                      no-lock.
         for EACH loan-acct  WHERE loan-acct.contract = loan.contract
                                and loan-acct.cont-code = loan.cont-code
                                and CAN-DO ("Кредит,КредПр", loan-acct.acct-type) 
                                NO-LOCK.
            find  FIRST bacct where bacct.acct = loan-acct.acct NO-LOCK no-error.
            if avail bacct then do:
      
               RUN acct-pos IN h_base (bacct.acct,bacct.currency,mtoday,mtoday,gop-status).
               ASSIGN
                  mBalance = mBalance + sh-bal
               .
            end.
         end.
      end.
      if mBalance = 0 then do:
         RUN Insert_TTName("Ssuda","").
      end.
      else do:
         RUN Insert_TTName("Ssuda", " (ЕСТЬ ОСТАТКИ ПО СЧЕТАМ ССУДНОЙ ЗАДОЛЖЕННОСТИ) ").
      end.
   end.
/* Список всех счетов */
   if iStr = "AcctOpen" then do: 
     
      mList_Acct = "".
      mList_cont = "Текущ,Расчет,Расч1".
     
      i = 0.
      for each bacct where bacct.cust-cat EQ acct.cust-cat
                       and bacct.cust-id eq acct.cust-id 
                       AND bacct.close-date EQ ? 
                       and CAN-DO(mList_cont,bacct.contract) 
                       no-lock.
     
          find first contract where contract.contract = bacct.contract no-lock no-error.
          mSignsVal = "".                     
          if avail contract then mSignsVal = contract.name-contract.

          if bacct.curr = "" then do:
             mCurrency = "Российский рубль".
          end. 
          else do:
             find first curr where curr.currency = bacct.currency no-lock no-error.
             if avail curr then do: 
                mCurrency = curr.name-currenc.
             end. 
             else do:
                mCurrency = " ".
             end.
          end.
          i = i + 1.                                   
          if i > 5 then do:
             mList_Acct = mList_Acct  + "№ " + trim(bacct.number) + ", " + mSignsVal + ", валюта-" + mCurrency + ", " + string(bacct.open-date).
             i = 5.
          end.
          else do:
             mList_Acct = "№ " + trim(bacct.number) + ", " + mSignsVal + ", валюта-" + mCurrency + ", " + string(bacct.open-date).
          end.
          RUN Insert_TTName("mList_Acct" + string(i), mList_Acct).
      end.
   end.
   if iStr = "AcctOpenFL" then do: 
     
      mList_Acct = "".
      mList_cont = "40817*,40820*,423*,426*".
     
      i = 0.
      for each bacct where bacct.cust-cat EQ acct.cust-cat
                       and bacct.cust-id eq acct.cust-id 
                       AND bacct.close-date EQ ? 
                       and CAN-DO(mList_cont,string(bacct.bal-acct)) 
                       no-lock.

          find first contract where contract.contract = bacct.contract no-lock no-error.
          mSignsVal = "".                     
          if avail contract then mSignsVal = contract.name-contract.

     
          if bacct.curr = "" then do:
             mCurrency = "Российский рубль".
          end. 
          else do:
             find first curr where curr.currency = bacct.currency no-lock no-error.
             if avail curr then do: 
                mCurrency = curr.name-currenc.
             end. 
             else do:
                mCurrency = " ".
             end.
          end.
          i = i + 1.                                   
          if i > 5 then do:
             mList_Acct = mList_Acct  + "№ " + trim(bacct.number) + ", " + mSignsVal + ", валюта-" + mCurrency + ", " + string(bacct.open-date).
             i = 5.
          end.
          else do:
             mList_Acct = "№ " + trim(bacct.number) + ", " + mSignsVal + ", валюта-" + mCurrency + ", " + string(bacct.open-date).
          end.
          RUN Insert_TTName("mList_Acct" + string(i), mList_Acct).
      end.
   end.
   if iStr = "AcctRep1" then do: 
     
      mList_cont = "Текущ,Расчет,Расч1".
      mList_Acct = "".
      i = 0.
      mBlock = 0.
      for each bacct where bacct.cust-cat EQ acct.cust-cat
                       and bacct.cust-id eq acct.cust-id 
                       AND bacct.close-date EQ ? 
                       and CAN-DO(mList_cont,bacct.contract) 
                       no-lock.
         i = i + 1.
         if i = 1 then do:
            mSignsVal = "открыт расчетный счет № " .
            mList_Acct =  trim(bacct.number).
         end.
         else do:
            mSignsVal = "открыты расчетные счета № ". 
            mList_Acct =  mList_Acct + ", " + trim(bacct.number).
         end.

/* Блокировки */
       
         FOR EACH blockobject WHERE                                           
            blockobject.end-datetime EQ ?                                  
            AND blockobject.class-code EQ 'BlockAcct'                         
            AND blockobject.file-name EQ 'acct'                               
            AND blockobject.surrogate EQ bacct.acct + "," + bacct.curr        
            NO-LOCK.                                                           

            mBlock = mBlock +  abs(BlockObject.val[3]) .

         END. 
      end.
      if mBlock = 0 then do:
         sBlock = "".
      end.
      else do:
         sBlock = "ЕСТЬ БЛОКИРОВКИ".
      end.
      RUN Insert_TTName("mBlock", sBlock).
      RUN Insert_TTName("mList_Acct", mSignsVal + mList_Acct).
   end.


   IF acct.cust-cat = "Ю" THEN
   DO:

/* Данные клиента */      
      FIND FIRST cust-corp WHERE
      		   cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN RETURN.
   
      mSignsVal = GetXAttrValueEx("cust-corp",
      						STRING(cust-corp.cust-id),
      						"ФИОРук",?).   
      RUN Insert_TTName("FIORuk", mSignsVal).
      /* разделение на Ф, И и О */
      RUN Insert_TTName("Fam", ENTRY(1,mSignsVal,"")).
      RUN Insert_TTName("Nam", ENTRY(2,mSignsVal,"")).
      IF NUM-ENTRIES(mSignsVal,"") GE 3 THEN
      	RUN Insert_TTName("Fat", SUBSTRING(mSignsVal,INDEX(mSignsVal,ENTRY(3,mSignsVal,"")))).
      
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
      
      
      mNBuh1 = GetXAttrValueEx("cust-corp", 
					  STRING(cust-corp.cust-id), 
					  "фиобухг", 
					  "").
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
      mSignsVal = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"КПП", 
						"").
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
   
   
      RUN Insert_TTName("stat",cust-corp.cust-stat).

      find first code where code.parent = "КодПредп"
                        and code.val = cust-corp.cust-stat no-lock no-error.
      if avail code then do:
         namestat = code.name. 
         RUN Insert_TTName("namestat",namestat).
          
      end.
      RUN Insert_TTName("NameOrg",cust-corp.name-corp).
      RUN Insert_TTName("okpo",cust-corp.okpo).
      RUN Insert_TTName("NameShort",cust-corp.name-short).

      RUN RetAdr.p(acct.cust-id,  "Ю", "АдрЮр", ?, OUTPUT mAdrReg).
      RUN Insert_TTName("AdrUr",mAdrReg). 
      
      RUN RetAdr.p(acct.cust-id,  "Ю", "АдрФакт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrFact",mAdrFact). 
      
      RUN RetAdr.p(acct.cust-id,  "Ю", "АдрПочт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrPoht",mAdrFact).
      RUN Insert_TTName("tax-insp",tax-insp).

      if iStr = "AcctLic" then do: 
         mSignsVal = namestat + " " + cust-corp.name-corp.
         mSignsVal = strwrap(mSignsVal,34,56).
         do i = 1 to num-entries(mSignsVal,"|").
               RUN Insert_TTName("client" + string(i), entry (i,mSignsVal,"|")).
         end.
         mSignsVal = mAdrReg.
         mSignsVal = strwrap(mSignsVal,22,56).
         do i = 1 to num-entries(mSignsVal,"|").
               RUN Insert_TTName("mAdrReg" + string(i), entry (i,mSignsVal,"|")).
         end.
      end.
   

   END. /*Юридическое лицо*/   
   IF acct.cust-cat = "Ч" THEN
   DO:

/* Данные клиента */      
      FIND FIRST person WHERE
      		   person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL person THEN RETURN.
   
      mSignsVal = person.name-last + " " + person.first-names.   
      RUN Insert_TTName("FIORuk", mSignsVal).
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
      
      mSignsVal = entry(2,person.Phone[2]).
      RUN Insert_TTName("tel", mSignsVal). 
      mSignsVal = GetXAttrValueEx("person", 
      						STRING(person.person-id), 
      						"fax", 
      						"").
      RUN Insert_TTName("fax", mSignsVal).                
      RUN Insert_TTName("INN",person.inn).
   
   

      RUN Insert_TTName("NameOrg",acct.details).
      RUN Insert_TTName("NameShort",acct.details).

      RUN RetAdr.p(acct.cust-id,  "Ч", "АдрПроп", ?, OUTPUT mAdrReg).

      RUN Insert_TTName("AdrUr",mAdrReg). 
      
      RUN RetAdr.p(acct.cust-id,  "Ч", "АдрФакт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrFact",mAdrFact). 
      
      RUN RetAdr.p(acct.cust-id,  "Ч", "АдрПочт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrPoht",mAdrFact).
      RUN Insert_TTName("tax-insp",person.tax-insp).

      if iStr = "AcctLic" then do: 
         mSignsVal = "Индивидуальный предприниматель " + person.name-last + " " + person.first-names + ", " + string(person.birthday,"99.99.9999") + " г.р.".
         mSignsVal = strwrap(mSignsVal,34,56).
         do i = 1 to num-entries(mSignsVal,"|").
               RUN Insert_TTName("client" + string(i), entry (i,mSignsVal,"|")).
         end.
         mSignsVal = mAdrReg.
         mSignsVal = strwrap(mSignsVal,22,56).
         do i = 1 to num-entries(mSignsVal,"|").
               RUN Insert_TTName("mAdrReg" + string(i), entry (i,mSignsVal,"|")).
         end.
      end.
      if person.gender then do:
         var-dead-var  = "умершего".
         var-resid-var = "проживавшего".
         var-respect-b-var  = "Уважаемый".
         var-respect-s-var  = "уважаемый".
      end.
      else do:
         var-dead-var  = "умершей".
         var-resid-var = "проживавшей".
         var-respect-b-var  = "Уважаемая".
         var-respect-s-var  = "уважаемая".
      end.  
      RUN Insert_TTName("var-dead-var",var-dead-var).
      RUN Insert_TTName("var-resid-var",var-resid-var).
      RUN Insert_TTName("var-respect-b-var",var-respect-b-var).
      RUN Insert_TTName("var-respect-s-var",var-respect-s-var).
      vDovTDoc = person.document-id.
      vDovNDoc = person.document.
      RUN Insert_TTName ("ddnu",vDovNDoc).
      vTmpDat = DATE(getxattrvalue ("person",string(person.person-id), "Document4Date_vid")).
      vDovDDoc = STRING(DAY(vTmpDat)) + " " + ENTRY(MONTH(vTmpDat),{&Months}) + " " + STRING(YEAR(vTmpDat)).
      RUN Insert_TTName ("ddvy",vDovDDoc).                       
      vDovKDoc = REPLACE(person.issue,",",", к/п").
      RUN Insert_TTName ("ddkem",vDovKDoc).

   END. /*Физическое лицо*/   
END.

/*     Сомнительный прием   */

IF mError EQ "" THEN
DO:
  RUN printvd.p (iStr,INPUT TABLE ttnames).
END.
ELSE
   RUN Fill-SysMes("","","0",mError).





