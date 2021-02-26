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

DEFINE VARIABLE mName		  AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mInn		  AS CHARACTER 			NO-UNDO. /* Ю Ч*/
DEFINE VARIABLE mAdrReg       AS CHARACTER 			NO-UNDO. /*АдрЮр*/
DEFINE VARIABLE mAdrFact   	  AS CHARACTER 			NO-UNDO. /*АдрФакт*/
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
DEFINE VARIABLE mtoday       AS date               NO-UNDO.   
DEFINE VARIABLE mNBuh1       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mNBuh2       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mNBuh3       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mNBuh4       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE namestat     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE i            AS Int                NO-UNDO.
DEFINE VARIABLE mOst         AS DEC                NO-UNDO.
DEFINE VARIABLE tCurrName    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vDayNum      AS int                NO-UNDO.
DEFINE VARIABLE mCommRate    AS DEC                NO-UNDO.
DEFINE VARIABLE out_proc     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAmtStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDecStr      AS CHARACTER          NO-UNDO.


                
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

run signat.p ("deprko","UserName",str_title,OUTPUT mUser).

  RUN Insert_TTName("Pod-user-dolg", GetXAttrValueEx("_User", mUser, "Должность", "")).
  RUN Insert_TTName("Pod-user-fio", GetXAttrValueEx("_User", mUser, "ФИОП", "")).
  RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "ДокОснТипРП", "")).
  RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "Телефон", "")).

/* запрос даты */
if iStr = "AMom" or iStr = "AMomC" then do: 
   {getdates.i &beglabel="Дата С" &endlabel="Дата ПО"}
   if end-date - beg-date > 397  then do:
      message "Отчёт можно получить не более чем за год" view-as alert-box.
      return.
   end.
end.
mtoday = today.
if iStr = "AcctDS" then do: 
   {getdate.i}
   mtoday = end-date.
end.
RUN Insert_TTName("Today",mtoday).
RUN Insert_TTName("TodayT",string(mtoday,"99.99.9999")).
RUN Insert_TTName("TodayStr",  STRING(DAY(mtoday))  + " " +  ENTRY(MONTH(mtoday),{&Months}) +  " " + STRING(YEAR(mtoday)) + "г.").





RUN Insert_TTName("gend-date", term2str(gend-date, gend-date)).
/*наименование и адрес банка*/
FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("go-bank-name",    branch.name). 
   RUN Insert_TTName("go-bank-address", branch.Address). 
   RUN Insert_TTName("go-gorod", entry(2,branch.Address)). 
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
END.

/* цикл по рецидам выбранных договоров */
FOR EACH tmprecid NO-LOCK, 
   FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK :

/* валюта */
   find first currency where currency.currency eq loan.currency no-lock no-error.
   tCurrName = "".
   if avail currency then do:
      tCurrName = trim(currency.name-currenc).
   end.
   RUN Insert_TTName("tCurrName", STRING(tCurrName)).


/* начало данных с условия */
   if iStr <> "zayv_univ" then do:
      find first loan-cond where loan-cond.contract = loan.contract
                        and loan-cond.cont-code = loan.cont-code
                        and loan-cond.since = mtoday
                        no-lock no-error.
      IF NOT AVAIL loan-cond THEN do:
         message "Для договора  " loan.cont-code " не найдено ДС на дату " mtoday   view-as alert-box.
         RETURN.
      end.
   end.
   else do:
      find last loan-cond where loan-cond.contract = loan.contract
                        and loan-cond.cont-code = loan.cont-code
                        no-lock no-error.
      IF NOT AVAIL loan-cond THEN do:
         message "Для договора  " loan.cont-code " не найдено ДС "  view-as alert-box.
         RETURN.
      end.
   end.


   mOst = dec(GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "СуммаДог",?)).     

   RUN Insert_TTName("SummaDS", mOst).
   RUN Insert_TTName("mOst", mOst).
   /* Сумма неснижаемого остатка прописью */
   RUN x-amtstr.p(mOst,loan.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
   i = num-entries(SummaStr[1]," ").
   mSignsVal = entry(i,SummaStr[1]," ").
   SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
   RUN Insert_TTName("ost00dog", string(TRUNC(mOst, 0),'>>>>>>>>>>>9')).
   SummaStr[1] = '(' + SummaStr[1] + ') ' + mSignsVal + ' ' + SummaStr[2].
   RUN Insert_TTName("SummaStrDog", SummaStr[1]).

   mSignsVal  = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "ФактСтавка",?).     
   RUN Insert_TTName("StavkaDS", mSignsVal).
   mCommRate = dec(mSignsVal).
   RUN Insert_TTName("CommPrc", STRING(mCommRate, '>9.99')).
   RUN x-amtstr.p(mCommRate, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
   out_proc = STRING(mAmtStr) + "целых ".
   RUN x-amtstr.p (DEC(mDecStr),"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
   out_proc = out_proc + LC(STRING(mAmtStr)) + "сотых ".
   RUN Insert_TTName("CommPrcStr", out_proc).






   mSignsVal  = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "НомерСогл",?).     
   RUN Insert_TTName("DS-num", mSignsVal).
   mSignsVal  = string(loan-cond.int-date).     
   RUN Insert_TTName("DS-srok", mSignsVal).

   RUN Insert_TTName("loan-num", loan.doc-ref).

   RUN Insert_TTName("open-date", string(loan.open-date,"99.99.9999") + "г.").

   mSignsVal = getxattrvalue ("loan",loan.contract + "," + loan.cont-code,"acct-dep").
   RUN Insert_TTName("acct-dep", entry(1,mSignsVal,"@")).
   RUN Insert_TTName("ЮЛ", "1").

/* дата */
   mSignsVal = STRING(DAY(loan-cond.since)) + " " + ENTRY(MONTH(loan-cond.since),{&Months}) + " " + STRING(YEAR(loan-cond.since)).
   RUN Insert_TTName ("date",mSignsVal).
   mSignsVal = STRING(DAY(loan-cond.since + loan-cond.int-date),"99") + " " 
               + ENTRY(MONTH(loan-cond.since + loan-cond.int-date),{&Months}) + " " 
               + STRING(YEAR(loan-cond.since + loan-cond.int-date)).
   RUN Insert_TTName ("end-date",mSignsVal).

/* Срок */
   mSignsVal = string(loan-cond.int-date).
   vDayNum = int(substr(mSignsVal,length(mSignsVal),1)).
   IF vDayNum EQ 1 THEN
       mSignsVal = mSignsVal + " день ".
   IF CAN-DO("2,3,4,",string(vDayNum)) THEN
       mSignsVal = mSignsVal + " дня ".
   IF NOT CAN-DO("0,1,2,3,4",STRING(vDayNum)) THEN
       mSignsVal = mSignsVal + " дней ".

   RUN Insert_TTName("NDays",mSignsVal).
   RUN Insert_TTName("int-period","В конце срока").



/* конец данных с условия */
   if iStr <> "zayv_univ" then do:
      FIND FIRST loan-acct WHERE loan-acct.contract   EQ loan.contract
                             and loan-acct.cont-code  EQ loan.cont-code
                             and loan-acct.acct-type  EQ "Расчет"
                             and loan-acct.since      le mtoday
                             NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-acct THEN do:
         message "Для договора  " loan.cont-code " не найдей счёт с ролью Расчет"   view-as alert-box.
         RETURN.
      end.
   end.
   else do:
      FIND FIRST loan-acct WHERE loan-acct.contract   EQ loan.contract
                             and loan-acct.cont-code  EQ loan.cont-code
                             and loan-acct.acct-type  EQ "Расчет"
                             NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-acct THEN do:
         message "Для договора  " loan.cont-code " не найдей счёт с ролью Расчет"   view-as alert-box.
         RETURN.
      end.
   end.
   RUN Insert_TTName("uacct", entry(1,loan-acct.acct,"@")).
   find first acct where acct.acct = loan-acct.acct no-lock no-error.
   IF loan.cust-cat = "Ю" THEN
   DO:

/* Данные клиента */      
      FIND FIRST cust-corp WHERE
      		   cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
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

      RUN RetAdr.p(loan.cust-id,  "Ю", "АдрЮр", ?, OUTPUT mAdrReg).
      RUN Insert_TTName("AdrUr",mAdrReg). 
      
      RUN RetAdr.p(loan.cust-id,  "Ю", "АдрФакт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrFact",mAdrFact). 
      
      RUN RetAdr.p(loan.cust-id,  "Ю", "АдрПочт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrPoht",mAdrFact).
      RUN Insert_TTName("tax-insp",tax-insp).

/*      if iStr = "AcctLic" then do: 
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
*/   

   END. /*Юридическое лицо*/   
   IF loan.cust-cat = "Ч" THEN
   DO:

/* Данные клиента */      
      FIND FIRST person WHERE
      		   person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
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
      
      mSignsVal = person.Phone[1].
      RUN Insert_TTName("tel", mSignsVal). 
      mSignsVal = GetXAttrValueEx("person", 
      						STRING(person.person-id), 
      						"fax", 
      						"").
      RUN Insert_TTName("fax", mSignsVal).                
      RUN Insert_TTName("INN",person.inn).
   
   

      RUN Insert_TTName("NameOrg",acct.details).
      RUN Insert_TTName("NameShort",acct.details).

      RUN RetAdr.p(loan.cust-id,  "Ч", "АдрПроп", ?, OUTPUT mAdrReg).
      RUN Insert_TTName("AdrUr",mAdrReg). 
      
      RUN RetAdr.p(loan.cust-id,  "Ч", "АдрФакт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrFact",mAdrFact). 
      
      RUN RetAdr.p(loan.cust-id,  "Ч", "АдрПочт", ?, OUTPUT mAdrFact).
      RUN Insert_TTName("AdrPoht",mAdrFact).
      RUN Insert_TTName("tax-insp",person.tax-insp).

/*      if iStr = "AcctLic" then do: 
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
*/

   END. /*Физическое лицо*/   

END.

/*     Сомнительный прием   */

IF mError EQ "" THEN
DO:
  RUN printvd.p (iStr,INPUT TABLE ttnames).
END.
ELSE
   RUN Fill-SysMes("","","0",mError).
