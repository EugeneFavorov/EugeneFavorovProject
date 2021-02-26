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



DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.

/*
DEFINE VARIABLE iStr	     AS CHARACTER 			NO-UNDO. /* Ю Ч*/
*/

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
DEFINE VARIABLE j            AS int                NO-UNDO.   
DEFINE VARIABLE i1           AS int                NO-UNDO.   
DEFINE VARIABLE j2           AS int                NO-UNDO.   
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
DEFINE VARIABLE vWork        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE List-bal     AS CHARACTER          NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE List-bal-2   AS CHARACTER          NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE List-bal-3   AS CHARACTER          NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE bal_         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE file-name    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE kold         AS int                NO-UNDO.    /* сколько дней перебирать для поиска рабочих дней */
DEFINE VARIABLE koldo        AS int                NO-UNDO.    /* количество рабочих дней */
DEFINE VARIABLE  var-Pod-user-dolg      AS CHARACTER          NO-UNDO. 
DEFINE VARIABLE  var-Pod-user-fio       AS CHARACTER          NO-UNDO. 
        

DEF BUFFER bacct for acct.

PAUSE 0.


FUNCTION str-repl RETURN CHAR (INPUT buf-str AS CHAR , INPUT iStr AS CHAR , INPUT iStr0 AS CHAR):
   DEFINE VARIABLE iStr1 AS CHARACTER	NO-UNDO.
   iStr1 = substr(buf-str,1,index(buf-str,iStr) - 1) + 
           codepage-convert(iStr0, "1251") + 
           substr(buf-str,index(buf-str,iStr) + length(iStr)).
   return iStr1.
END FUNCTION.


list-bal = "40702,40802".
kold = 45.
koldo = 15.

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

Find first tmprecid no-lock.
find FIRST op WHERE RECID(op) EQ tmprecid.id NO-LOCK no-error.


mtoday = op.op-date.
iStr = "UvdPos".
DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").   

DEF STREAM in-data1.

n = 0.
j = num-entries(list-bal).
do i = 1 to j.  
   bal_ = entry(i,list-bal).
   FOR EACH acct where acct.contract begins "Тран" 
                   and acct.filial-id = shfilial
                   and acct.bal-acct = int(bal_)
                   and acct.curr <> "" 
                   and acct.close-date = ?
                   NO-LOCK. 

      run acct-pos in h_base (acct.acct,
                              acct.currency,
                              mtoday, mtoday, ?).

      ost = abs(sh-vcr).
      if ost = 0 then next.
      RUN output_acct(acct.acct).
   end.
end.


return.

PROCEDURE output_acct:
   DEF INPUT  PARAM pacct      AS char   NO-UNDO.

   DEFINE VARIABLE file-name-in   AS char  NO-UNDO.   
   DEFINE VARIABLE file-name-out  AS char  NO-UNDO.   
   DEFINE VARIABLE var-stat       AS char  NO-UNDO.   
   DEFINE VARIABLE var-namestat   AS char  NO-UNDO.   
   DEFINE VARIABLE var-NameOrg    AS char   NO-UNDO.   
   DEFINE VARIABLE var-NameShort  AS char   NO-UNDO.   
   DEFINE VARIABLE var-mAdrReg    AS char   NO-UNDO.   
   DEFINE VARIABLE var-inn        AS char   NO-UNDO.   
   DEFINE VARIABLE var-doc-num    AS char   NO-UNDO.   
   DEFINE VARIABLE var-op-date    AS char   NO-UNDO.   
   DEFINE VARIABLE var-acct       AS char   NO-UNDO.   
   DEFINE VARIABLE var-bankname   AS char   NO-UNDO.   
   DEFINE VARIABLE var-sum        AS char   NO-UNDO.   
   DEFINE VARIABLE var-sum-str    AS char   NO-UNDO.   
   DEFINE VARIABLE var-srok       AS char   NO-UNDO.   
   DEFINE VARIABLE var-dolg-sotr  AS char   NO-UNDO.   
   DEFINE VARIABLE var-go-bank-name  AS char   NO-UNDO.   
   DEFINE VARIABLE var-bankname-pp   AS char   NO-UNDO.   
   DEFINE VARIABLE var-mCurrency-var AS CHARACTER          NO-UNDO.


   DEFINE VARIABLE buf-str        AS char   NO-UNDO.   
   DEFINE VARIABLE mCust-Cat      AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
   DEFINE VARIABLE mSignsVal    AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE i            AS int                NO-UNDO.   
   DEFINE VARIABLE j            AS int                NO-UNDO.   
   DEFINE VARIABLE iDate        AS date               NO-UNDO.   
   DEFINE VARIABLE datebeg_     AS date               NO-UNDO.   
   DEFINE VARIABLE kold         AS int                NO-UNDO.    /* сколько дней перебирать для поиска рабочих дней */
   DEFINE VARIABLE koldo        AS int                NO-UNDO.    /* количество рабочих дней */


   find first acct where acct.acct = pacct no-lock no-error.
   mCust-Cat = if acct.cust-cat = "Ю" then "1" else "2".
   file-name-out =  STRING(YEAR(mtoday) ,"9999")      + "-" + 
                string(MONTH(mtoday),"99")        + "-" + 
                STRING(DAY(mtoday)  ,"99")        + "-" + 
                STRING(n,"999") + "-" + mCust-Cat + "-" +
                string(acct.cust-id    ,"999999999")
                + ".doc"
                .
   file-name-in = "/data/home2/bis/quit41d/src-test/uvd-pos.tpl".


   FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
   IF AVAILABLE branch THEN
   DO:
      var-go-bank-name =  branch.name. 
   END.
   FIND FIRST branch WHERE branch.Branch-Id EQ acct.filial-id NO-LOCK NO-ERROR.
   IF AVAILABLE branch THEN
   DO: 
       var-bankname    = branch.name. 
       var-bankname-pp =  GetXattrValue("branch",STRING(branch.Branch-Id),"БанкНаимПП"). 
       run signat-a.p ("uvd-pos",trim(Branch.Branch-Id),str_title,OUTPUT mUser).
       if mUser = ? then mUser = "BIS".
       var-Pod-user-dolg    = GetXAttrValueEx("_User", mUser, "Должность", "").
       var-Pod-user-fio     = GetXAttrValueEx("_User", mUser, "ФИОП", "").
       var-Pod-user-fio = Caps(Substr(trim(entry(2,var-Pod-user-fio," ")),1,1))
                        + "." + Caps(Substr(trim(entry(3,var-Pod-user-fio," ")),1,1)) 
                        + "." + trim(entry(1,var-Pod-user-fio," ")).

   END.
   datebeg_ = mtoday.
   IF acct.cust-cat = "Ю" THEN
   DO:
 /* Данные клиента */      
         FIND FIRST cust-corp WHERE
         		   cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN RETURN.
      
         var-stat = cust-corp.cust-stat.
         var-inn  = cust-corp.inn.
   
         find first code where code.parent = "КодПредп"
                           and code.val = cust-corp.cust-stat no-lock no-error.
         if avail code then do:
            var-namestat = code.name. 
         end.
         var-NameOrg = cust-corp.name-corp.
         var-NameShort = cust-corp.name-short.
         RUN RetAdr.p(acct.cust-id,  "Ю", "АдрЮр", ?, OUTPUT mAdrReg).
         var-mAdrReg = mAdrReg. 
   END. /*Юридическое лицо*/   
   IF acct.cust-cat = "Ч" THEN
   DO:
 /* Данные клиента */      
         FIND FIRST person WHERE
         		   person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL person THEN RETURN.
      
         var-stat = "".
         var-inn  = person.inn.
         var-NameOrg = "ИП " + person.name-last + " " + person.first-names.
         var-NameShort = var-NameOrg.
         RUN RetAdr.p(acct.cust-id,  "Ч", "АдрПроп", ?, OUTPUT mAdrReg).
         var-mAdrReg = mAdrReg. 
   END. /*Юридическое лицо*/   
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
      j = 0.
      kold = 45.
      koldo = 15.
      do i= 1 to kold:
         iDate = datebeg_ + i.
         vWork = NOT holidayru(iDate).
         if vWork then do:
            j = j + 1.
         end.
         if j >= koldo then do:
            leave.       
         end.
      end.
      var-srok      = string(iDate,"99.99.9999").
      var-sum       = REPLACE(trim(string(ost,">,>>>,>>>,>>>.99")),".","-").
      RUN x-amtstr.p(ost,acct.currency, TRUE, TRUE, OUTPUT SummaStr[1], OUTPUT SummaStr[2]).
      i = num-entries(SummaStr[1]," ").
      mSignsVal = entry(i,SummaStr[1]," ").
      SummaStr[1] = substr(SummaStr[1],1,INDEX(SummaStr[1],ENTRY(i,SummaStr[1]," ")) - 2).
      SummaStr[1] =  SummaStr[1] + ' ' + mSignsVal + ' ' + SummaStr[2].
      var-sum-str = SummaStr[1].
      var-acct      = acct.number.
      var-op-date   = string(mtoday,"99.99.9999").
      if acct.curr = "" then do:
         var-mCurrency-var = "Российский рубль".
      end. 
      else do:
         find first curr where curr.currency = acct.currency no-lock no-error.
         if avail curr then do: 
            var-mCurrency-var = curr.i-currency.
         end. 
         else do:
            var-mCurrency-var = " ".
         end.
      end.
      output to VALUE(file-name-out).
      INPUT STREAM in-data1 FROM VALUE(file-name-in).
      REPEAT:
         IMPORT STREAM in-DATA1  UNFORMATTED buf-str.
         if index(buf-str,"var-NameOrg-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-NameOrg-var",var-NameOrg).
         end.
         if index(buf-str,"var-mCurrency-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-mCurrency-var",var-mCurrency-var).
         end.
         if index(buf-str,"var-inn-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-inn-var",var-inn).
         end.
         if index(buf-str,"var-mAdrReg-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-mAdrReg-var",var-mAdrReg).
         end.
         if index(buf-str,"var-doc-num-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-doc-num-var",var-doc-num).
         end.
         if index(buf-str,"var-op-date-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-op-date-var",var-op-date).
         end.
         if index(buf-str,"var-sum-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-sum-var",var-sum).
         end.
         if index(buf-str,"var-sum-str-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-sum-str-var",var-sum-str).
         end.
         if index(buf-str,"var-acct-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-acct-var",var-acct).
         end.
         if index(buf-str,"var-bankname-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-bankname-var",var-bankname).
         end.
         if index(buf-str,"var-srok-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-srok-var",var-srok).
         end.
         if index(buf-str,"var-go-bank-name-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-go-bank-name-var",var-go-bank-name).
         end.
         if index(buf-str,"var-bankname-pp-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-bankname-pp-var",var-bankname-pp).
         end.
         if index(buf-str,"var-NameShort-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-NameShort-var",var-NameShort).
         end.
         if index(buf-str,"var-Pod-user-fio-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-Pod-user-fio-var",var-Pod-user-fio).
         end.
         if index(buf-str,"var-Pod-user-dolg-var") <> 0 then do:
            buf-str = str-repl(buf-str,"var-Pod-user-dolg-var",var-Pod-user-dolg).
         end.
         put unformatted buf-str skip.    
      end.
      output close.
   end.
/*
   RUN pb_mail.p ("v.ignatchenko,e.polovinkina,o.shakin,A.Korobova,M.Vasileva", "Izveshenie", "Izveshenie", file-name-out). 
*/
   RUN sndbispc ("file=" + file-name-out + ";class=bq").

END PROCEDURE.




