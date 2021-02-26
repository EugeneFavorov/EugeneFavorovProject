{globals.i}
{prn-doc.def &with_proc=YES}
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{dpsproc.def}
{ksh-defs.i NEW}
{justasec}
{intrface.get refer}    /* Библиотека службы справочников. */
{intrface.get date}


FUNCTION str-repl RETURN CHAR (INPUT buf-str AS CHAR , INPUT iStr AS CHAR , INPUT iStr0 AS CHAR):
   DEFINE VARIABLE iStr1 AS CHARACTER	NO-UNDO.
   iStr1 = substr(buf-str,1,index(buf-str,iStr) - 1) + 
           codepage-convert(iStr0, "1251") + 
           substr(buf-str,index(buf-str,iStr) + length(iStr)).
   return iStr1.
END FUNCTION.

/*
DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.
*/
DEFINE VARIABLE iStr         AS CHARACTER          NO-UNDO.

DEFINE VARIABLE file-name-out          AS CHARACTER          NO-UNDO. 
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
DEFINE VARIABLE n            AS int                NO-UNDO.   
DEFINE VARIABLE i            AS int                NO-UNDO.   
DEFINE VARIABLE i3           AS int                NO-UNDO.   
DEFINE VARIABLE i4           AS int                NO-UNDO.   
DEFINE VARIABLE j            AS int                NO-UNDO.   
DEFINE VARIABLE i1           AS int                NO-UNDO.   
DEFINE VARIABLE j2           AS int                NO-UNDO.   
DEFINE VARIABLE j4           AS int                NO-UNDO.   
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
DEFINE VARIABLE iDate        AS date               NO-UNDO.   
DEFINE VARIABLE datebeg_     AS date               NO-UNDO.   
DEFINE VARIABLE beg-dt       AS date               NO-UNDO.   
DEFINE VARIABLE vWork        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE List-bal     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE bal_         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE file-name    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE var-op-date    AS char   NO-UNDO.   

DEFINE VARIABLE  var-Pod-user-dolg      AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-user-fio       AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-user-dov-rp    AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-user-telefon   AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-user-f         AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-isp-dolg       AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-isp-fio        AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-isp-dov-rp     AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-isp-telefon    AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-gend-date          AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Today              AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-TodayT             AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-TodayStr           AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-day_               AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-months_            AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-year_              AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-acct-var           AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Date-in            AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Date-in-pr         AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Date-cl            AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-file-name          AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-stat               AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-namestat           AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-NameOrg            AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-NameShort          AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-AdrUr              AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-AdrFact            AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-AdrPoht            AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-FIORuk             AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Fam                AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Nam                AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-tax-insp           AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-msh-vcr            AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-SummaStr           AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Sum-Str-var        AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Sum                AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Summ00             AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-day_z              AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-months_z           AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-year_z             AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Number             AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-i-currency         AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  mCust-Cat              AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  file-name-in           AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  buf-str                AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-mCurrency          AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-inn                AS char   NO-UNDO.   
DEFINE VARIABLE var-doc-num    AS char   NO-UNDO.   
DEFINE VARIABLE var-srok       AS char   NO-UNDO.   
DEFINE VARIABLE var-bankname   AS char   NO-UNDO.   
DEFINE VARIABLE var-go-bank-name  AS char   NO-UNDO.   
DEFINE VARIABLE var-bankname-pp   AS char   NO-UNDO.   



DEF STREAM in-data1.
        
DEF BUFFER bacct for acct.

PAUSE 0.

list-bal = "40702,40802".

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

beg-dt = today.
do i= 1 to 45:
   beg-dt = beg-dt - i.
   if not holidayru(beg-dt) then do:
      leave.
   end.
end.
mtoday = beg-dt.



DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").   

/*
find first _user where _user._userid = user_ no-lock no-error.
RUN Insert_TTName("Pod-isp-f", _user._User-Name).
iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
str_title = "[ Выберите подписанта (" + user_ + " " + iBranch + ")]".
mUser = "BIS".
run signat-a.p ("uvd-pos","UserName",str_title,OUTPUT mUser).
if mUser = ? then return.
*/

if mUser = ?  or mUser = "" then mUser = "BIS".

/* цикл по рецидам выбранных счетов */
n = 0.
j = num-entries(list-bal).

/* наименование и адрес банка головного банка */

FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   var-go-bank-name =  branch.name. 
END.

FOR each branch WHERE                                                
   lookup("СВОД",branch.parent-id,";") GT 0                          
   AND (branch.close-date EQ ? OR branch.close-date >= today)     
   AND branch.isbank EQ yes                                          
   NO-LOCK                                                           
   BY branch.order                                                   
   BY branch.branch-id 
   .
   run signat-a.p ("uvd-pos",trim(Branch.Branch-Id),str_title,OUTPUT mUser).
   if mUser = ? then mUser = "BIS".

   /*наименование и адрес банка*/
   
   var-bankname    = branch.name. 
   var-bankname-pp =  GetXattrValue("branch",STRING(branch.Branch-Id),"БанкНаимПП"). 

   /*    */

   do i = 1 to j.  
      bal_ = entry(i,list-bal).
      FOR EACH acct where acct.contract begins "Тран" 
                      and acct.filial-id = branch.branch-id
                      and acct.bal-acct = int(bal_)
                      and acct.curr <> "" 
                      and acct.close-date = ?
                      NO-LOCK. 
         run acct-pos in h_base (acct.acct,
                                 acct.currency,
                                 mtoday, mtoday, ?).
         ost = abs(sh-vcr).
   
         if ost = 0 then next.
         {empty ttnames}
         var-Pod-user-dolg    = GetXAttrValueEx("_User", mUser, "Должность", "").
         var-Pod-user-fio     = GetXAttrValueEx("_User", mUser, "ФИОП", "").
         var-Pod-user-dov-rp  = GetXAttrValueEx("_User", mUser, "ДокОснТипРП", "").
         var-Pod-user-telefon = GetXAttrValueEx("_User", mUser, "Телефон", "").
         find first _user where _user._userid = mUser no-lock no-error.
         var-Pod-user-f       = _user._User-Name.
         var-Pod-isp-dolg     = GetXAttrValueEx("_User", User_, "Должность", "").
         var-Pod-isp-fio      = GetXAttrValueEx("_User", User_, "ФИОП", "").
         var-Pod-isp-dov-rp   = GetXAttrValueEx("_User", User_, "ДокОснТипРП", "").
         var-Pod-isp-telefon  = GetXAttrValueEx("_User", User_, "Телефон", "").
         var-gend-date        = term2str(gend-date, gend-date).
   
         shfilial_ = GetXAttrValueEx("_User", mUser, "filial-id", "").
         var-TodayT     = string(mtoday,"99.99.9999").
         var-TodayStr   = STRING(DAY(mtoday))  + " " +  ENTRY(MONTH(mtoday),{&Months}) +  " " + STRING(YEAR(mtoday)) + "г.".
         var-day_       = STRING(DAY(mtoday),"99").
         var-months_    = ENTRY(MONTH(mtoday),{&Months}).
         var-year_      = STRING(YEAR(mtoday),"9999").
         var-acct-var   = acct.number.
         var-Date-in-pr = STRING(DAY(acct.open-date))  + " " +  ENTRY(MONTH(acct.open-date),{&Months}) +  " " + STRING(YEAR(acct.open-date)) + "года.".
         var-Date-cl    = STRING(acct.close-date, "99.99.9999") + " г.".
         var-op-date    = string(mtoday,"99.99.9999") + " г.".
         var-Number = string(n).
         if acct.curr = "" then do:
            mCurrency = "Российский рубль".
         end. 
         else do:
            find first curr where curr.currency = acct.currency no-lock no-error.
            if avail curr then do: 
               mCurrency = curr.i-currency.
            end. 
            else do:
               mCurrency = " ".
            end.
         end.
         var-mCurrency = mCurrency.
         IF acct.cust-cat = "Ю" THEN
         DO:
      
    /* Данные клиента */      
            FIND FIRST cust-corp WHERE
            		   cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
            IF NOT AVAIL cust-corp THEN RETURN.
         
            file-name = "1"  + "-" + string(cust-corp.cust-id    ,"999999999") + "-" + 
                                     STRING(YEAR(mtoday) ,"9999")      + "-" + 
                                     string(MONTH(mtoday),"99")        + "-" + 
                                     STRING(DAY(mtoday)  ,"99")        + "-" + 
                                     STRING(n,"999").
   
            var-file-name = file-name.
            var-stat = cust-corp.cust-stat.
            var-inn  = cust-corp.inn.

            find first code where code.parent = "КодПредп"
                              and code.val = cust-corp.cust-stat no-lock no-error.
            if avail code then do:
               namestat = code.name. 
               var-namestat = namestat.
                
            end.
   
            var-NameOrg   = cust-corp.name-corp.
            var-NameShort = cust-corp.name-short.
            RUN RetAdr.p(acct.cust-id,  "Ю", "АдрЮр", ?, OUTPUT mAdrReg).
            var-AdrUr = mAdrReg.
            RUN RetAdr.p(acct.cust-id,  "Ю", "АдрФакт", ?, OUTPUT mAdrFact).
            var-AdrFact = mAdrFact.
            RUN RetAdr.p(acct.cust-id,  "Ю", "АдрПочт", ?, OUTPUT mAdrFact).
            var-AdrPoht =  mAdrFact.
   
         END. /*Юридическое лицо*/   
         IF acct.cust-cat = "Ч" THEN
         DO:
      
   /* Данные клиента */      
            FIND FIRST person WHERE
            		   person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
            IF NOT AVAIL person THEN RETURN.
   
   
            file-name = "2" + "-" + string(person.person-id     ,"999999999") + "-" + 
                                    STRING(YEAR(mtoday) ,"9999")      + "-" + 
                                    string(MONTH(mtoday),"99")        + "-" + 
                                    STRING(DAY(mtoday)  ,"99")        + "-" + 
                                    STRING(n,"999").
            RUN Insert_TTName("file-name",file-name).
         
            mSignsVal = person.name-last + " " + person.first-names.   
            var-FIORuk = mSignsVal.
            /* разделение на Ф, И и О */
            var-inn  = person.inn.
            var-Fam = ENTRY(1,mSignsVal,"").
            var-Nam = ENTRY(2,mSignsVal,"").
            var-NameOrg = acct.details.
            mSignsVal = "Индивидуальный предприниматель " + person.name-last + " " + person.first-names.
            var-NameShort = mSignsVal.
            RUN RetAdr.p(person.person-id,  "Ч", "АдрПроп", ?, OUTPUT mAdrFact).
            var-AdrPoht = mAdrFact.
            if var-AdrPoht = "" then do:
               RUN RetAdr.p(person.person-id,  "Ч", "АдрФакт", ?, OUTPUT mAdrFact).
               var-AdrPoht = mAdrFact.
            end.
            if var-AdrPoht = "" then do:
               RUN RetAdr.p(person.person-id,  "Ч", "АдрПочт", ?, OUTPUT mAdrFact).
               var-AdrPoht = mAdrFact.
            end.
            var-tax-insp = person.tax-insp .
         END. /*Физическое лицо*/   

         for each op-entry where op-entry.acct-cr   =  acct.acct
                             and op-entry.op-date   =  mtoday
                             and op-entry.op-status >= chr(251)
                             and op-entry.amt-cur   <> 0
                             no-lock.
            FIND FIRST op OF op-entry
               NO-LOCK NO-ERROR.
            n = n + 1.
            ost = op-entry.amt-cur.
            var-doc-num   = op.doc-num.
            msh-vcr = sh-vcr.
            var-msh-vcr = string(msh-vcr,'>>>>>>>>>>>9.99').
            RUN x-amtstr.p(ost,acct.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
            i4 = num-entries(SummaStr[1]," ").
            mSignsVal       = entry(i4,SummaStr[1]," ").
            SummaStr[1]     = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i4,SummaStr[1]," ")) - 2).
            SummaStr[1]     = SummaStr[1] + ' ' + mSignsVal + ' ' + SummaStr[2].
            var-Sum-Str-var = SummaStr[1].
            var-sum       = REPLACE(trim(string(ost,">,>>>,>>>,>>>.99")),".","-").
            find first currency where currency.currency = acct.curr no-lock no-error.
            if avail currency then do:
               var-i-currency = currency.i-currency.
            end.
            datebeg_ = mtoday.
            j4 = 0.
            do i3 = 1 to 45:
               iDate = datebeg_ + i3.
               vWork =  NOT holidayru(iDate).
               if vWork then do:
                  j4 = j4 + 1.
               end.
               if j4 >= 15 then do:
                  leave.       
               end.
            end.
            var-srok      = string(iDate,"99.99.9999").
            var-day_z    = STRING(DAY(iDate),"99").
            var-months_z = ENTRY(MONTH(iDate),{&Months}).
            var-year_z   = STRING(YEAR(iDate),"9999").
            mCust-Cat = if acct.cust-cat = "Ю" then "1" else "2".
            file-name-out = "./" + STRING(YEAR(mtoday) ,"9999")      + "-" + 
                         string(MONTH(mtoday),"99")        + "-" + 
                         STRING(DAY(mtoday)  ,"99")        + "-" + 
                         STRING(n,"999") + "-" + mCust-Cat + "-" +
                         string(acct.cust-id    ,"999999999") + "-" + string(Branch.Branch-id)
                         + ".doc"
                         .
            file-name-in = "/data/home2/bis/quit41d/src-test/uvd-pos.tpl".
            output to VALUE(file-name-out).
            INPUT STREAM in-data1 FROM VALUE(file-name-in).
            REPEAT:
               IMPORT STREAM in-DATA1  UNFORMATTED buf-str.
               if index(buf-str,"var-inn-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-inn-var",var-inn).
               end.
               if index(buf-str,"var-doc-num-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-doc-num-var",var-doc-num).
               end.
               if index(buf-str,"var-number-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-number-var",var-number).
               end.
               if index(buf-str,"var-day_-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-day_-var",var-day_).
               end.
               if index(buf-str,"var-months_-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-months_-var",var-months_).
               end.
               if index(buf-str,"var-year_-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-year_-var",var-year_).
               end.
               if index(buf-str,"var-NameShort-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-NameShort-var",var-NameShort).
               end.
               if index(buf-str,"var-AdrPoht-var") <> 0 then do:
               buf-str = str-repl(buf-str,"var-AdrPoht-var",var-AdrPoht).
               end.
               if index(buf-str,"var-mCurrency-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-mCurrency-var",var-mCurrency).
               end.
               if index(buf-str,"var-msh-vcr-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-msh-vcr-var",var-msh-vcr).
               end.
               if index(buf-str,"var-TodayT-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-TodayT-var",var-TodayT).
               end.
               if index(buf-str,"var-acct-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-acct-var",var-acct-var).
               end.
               if index(buf-str,"var-day_z-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-day_z-var",var-day_z).
               end.
               if index(buf-str,"var-months_z-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-months_z-var",var-months_z).
               end.
               if index(buf-str,"var-year_z-var") <> 0 then do:
               buf-str = str-repl(buf-str,"var-year_z-var",var-year_z).
               end.
               if index(buf-str,"var-Pod-user-fio-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-Pod-user-fio-var",var-Pod-user-fio).
               end.
               if index(buf-str,"var-Pod-user-dolg-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-Pod-user-dolg-var",var-Pod-user-dolg).
               end.
               if index(buf-str,"var-sum-str-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-sum-str-var",var-Sum-Str-var).
               end.
               if index(buf-str,"var-sum-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-sum-var",var-sum).
               end.
   
               if index(buf-str,"var-op-date-var") <> 0 then do:
                   buf-str = str-repl(buf-str,"var-op-date-var",var-op-date).
               end.
               if index(buf-str,"var-srok-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-srok-var",var-srok).
               end.
               if index(buf-str,"var-bankname-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-bankname-var",var-bankname).
               end.
               if index(buf-str,"var-go-bank-name-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-go-bank-name-var",var-go-bank-name).
               end.
               if index(buf-str,"var-bankname-pp-var") <> 0 then do:
                  buf-str = str-repl(buf-str,"var-bankname-pp-var",var-bankname-pp).
               end.
               put unformatted buf-str skip.    
            end.
            output close.
            RUN pb_mail.p ("v.ignatchenko,l.rusinova,o.shakin,A.Korobova,M.Vasileva,l.rusinova", "Uvedomlenie", "Uvedomlenie", file-name-out).
            OS-COPY VALUE(file-name-out) VALUE("/data/home2/bis/quit41d/imp-exp/cur_cont/"  + file-name-out ). 
         end.
      end.
   END.
end.

/*
         RUN pb_mail.p ("v.ignatchenko", "Uvedomlenie", "Uvedomlenie", file-name-out).
         RUN pb_mail.p ("v.ignatchenko,e.polovinkina,o.shakin,A.Korobova,M.Vasileva", "Uvedomlenie", "Uvedomlenie", file-name-out).
*/



