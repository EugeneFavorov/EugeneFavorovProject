/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ТОО "Банковские информационные системы"
     Filename: precrdprint.p
      Comment: 
   Parameters: 
         Uses:
      Used by: 
      Created: 06/08/2008 feok
     Modified: 14/10/2009 Jadv (0108906)
     Modified: 21/10/2013 Koan (0204655) Добавлена поддержка договоров обеспечения.
         при печати в режимах MODE2, MODE3, MODE4.
*/

&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{filleps.def}
{norm.i NEW}
{svarloan.def NEW}
{getdate.i}
{intrface.get loan}
{lshpr.pro}
{def_work.i new}        /* Объявление переменных для работы с
                         начислением процентов. */
{intrface.get date} /* .нструменты для работы с датами */
{sh-defs.i}         /* .пределение переменных, приобретающих значения остатка */                 
/*--------------------*/
{intrface.get comm}
/*----------------------------*/
   /* Строка параметров */
DEF INPUT PARAM iStr AS CHAR NO-UNDO.

/*затычки для  вызова RE_PARAM*/
DEF VAR a1         AS DECIMAL   NO-UNDO.
DEF VAR c1         AS CHARACTER.
DEF VAR c2         AS CHARACTER.
DEF VAR mAcctType  AS CHARACTER NO-UNDO. 
DEF VAR mI         AS INT64     NO-UNDO.
DEF VAR a2         AS DECIMAL NO-UNDO.
DEF VAR par_0      AS DECIMAL NO-UNDO. /* остаток основного долга */
DEF VAR par_2      AS DECIMAL NO-UNDO. 
DEF VAR par_4      AS DECIMAL NO-UNDO. /* сумма % на основной долг 4,29 */
DEF VAR par_7      AS DECIMAL NO-UNDO. /* сумма просроч. основного долга */
DEF VAR par_8      AS DECIMAL NO-UNDO. /* сумма % на просроч. основной долг 8,229,233 */
DEF VAR par_9      AS DECIMAL NO-UNDO. /* сумма пени */ 
DEF VAR par_12     AS DECIMAL NO-UNDO. 
DEF VAR par_10     AS DECIMAL NO-UNDO. /* сумма просроч. % 10,210,48,248 */
DEF VAR par_13     AS DECIMAL NO-UNDO. 
DEF VAR par_26     AS DECIMAL NO-UNDO.
DEF VAR par_29     AS DECIMAL NO-UNDO.
DEF VAR par_32     AS DECIMAL NO-UNDO.
DEF VAR par_33     AS DECIMAL NO-UNDO.
DEF VAR par_34     AS DECIMAL NO-UNDO.
DEF VAR par_35     AS DECIMAL NO-UNDO.
DEF VAR par_48     AS DECIMAL NO-UNDO.
DEF VAR par_82     AS DECIMAL NO-UNDO.
DEF VAR par_109    AS DECIMAL NO-UNDO.
DEF VAR par_173    AS DECIMAL NO-UNDO.
DEF VAR par_209    AS DECIMAL NO-UNDO.
DEF VAR par_210    AS DECIMAL NO-UNDO.
DEF VAR par_229    AS DECIMAL NO-UNDO.
DEF VAR par_233    AS DECIMAL NO-UNDO.
DEF VAR par_248    AS DECIMAL NO-UNDO.
DEF VAR par_301    AS DECIMAL NO-UNDO.
DEF VAR par_377    AS DECIMAL NO-UNDO.
DEF VAR par_509    AS DECIMAL NO-UNDO.
DEF VAR par_519    AS DECIMAL NO-UNDO. /* пеня за непредоставление ПТС */
DEF VAR par_526    AS DECIMAL NO-UNDO.
DEF VAR par_530    AS DECIMAL NO-UNDO.
DEF VAR par_531    AS DECIMAL NO-UNDO.
DEF VAR oper_83    AS DECIMAL NO-UNDO.
DEF VAR oper_283   AS DECIMAL NO-UNDO.
DEF VAR mCommFirst AS DECIMAL NO-UNDO.
DEF VAR mCommRKO   AS DECIMAL NO-UNDO.
DEF VAR xz         AS DECIMAL NO-UNDO.
DEF VAR rproc      AS CHAR    FORMAT "x(120)".
DEF VAR out_proc   AS CHAR    NO-UNDO.
DEF VAR mAmtStr    AS CHAR    NO-UNDO. /* Строка для прописи*/
DEF VAR mDecStr    AS CHAR    NO-UNDO. /* Строка для прописи*/
DEF VAR mOstCr     AS DECIMAL NO-UNDO. 
DEF VAR mCommRate  AS DECIMAL NO-UNDO. /* Значение % ставки */
DEF VAR vTmpDate   AS DATE    NO-UNDO.
DEF VAR mCust-Cat  AS CHAR NO-UNDO.
DEF VAR mCust-id   AS INT  NO-UNDO.
DEF VAR vPolAdr    AS CHAR NO-UNDO.
DEF VAR vDate      AS DATE NO-UNDO.
DEF VAR vDeystv    AS CHAR NO-UNDO.

DEF VAR vDovTDoc   AS CHAR NO-UNDO.
DEF VAR vDovNDoc   AS CHAR NO-UNDO.
DEF VAR vTmpDat    AS Date NO-UNDO.
DEF VAR vDovDDoc   AS CHAR NO-UNDO.
DEF VAR vDovKDoc   AS CHAR NO-UNDO.
DEF VAR mEps       AS CHAR NO-UNDO.
DEF VAR mPsk       AS CHAR NO-UNDO.

DEF VAR mTCVIN     AS CHAR NO-UNDO.
DEF VAR mTCmodel   AS CHAR NO-UNDO.
DEF VAR mTCyear    AS CHAR NO-UNDO.
DEF VAR mTCmotor   AS CHAR NO-UNDO.
DEF VAR mTCcolor   AS CHAR NO-UNDO.
DEF VAR mDogPor    AS CHAR NO-UNDO.
DEF VAR mTCPerson  AS CHAR NO-UNDO.
DEF VAR mTCPersonPor  AS CHAR NO-UNDO.
DEF VAR mDateDog   AS Date NO-UNDO.
DEF VAR Name_      AS CHAR NO-UNDO.







DEFINE VARIABLE mtoday       AS date               NO-UNDO.   
DEF  NEW GLOBAL SHARED VAR sTermRecid AS RECID NO-UNDO. /* Для корректного поиска догюобеспечения в lgarterm.p */
DEF VAR sTermRecidZam    AS INT64 NO-UNDO. /* Для хранения RECID(term-obl) по замене предмета залога*/
DEF  NEW GLOBAL SHARED VAR sStr       AS CHAR  NO-UNDO. /* Для корректной работы loanagval при замене залога */



mtoday = today.


DEFINE TEMP-TABLE tt NO-UNDO
   FIELD NomDog       AS CHAR
   FIELD CodeVal      AS CHAR
   FIELD NomPP        AS INT
   FIELD ChVal        AS CHAR
   FIELD term-obl-id  AS RECID
   FIELD Cust-Cat     AS CHAR
   FIELD Cust-id      AS INT.
DEFINE QUERY q_term FOR tt.
DEFINE BROWSE b_term QUERY q_term NO-LOCK 
DISPLAY
   tt.NomPP   COLUMN-LABEL "#"               FORMAT 99
   tt.NomDog  COLUMN-LABEL "НОМЕР ДОГОВОРА"  FORMAT "x(20)" 
   tt.CodeVal COLUMN-LABEL "ВИД"             FORMAT "x(20)"
   tt.ChVal   COLUMN-LABEL "КЛИЕНТ"          FORMAT "x(45)"
   WITH 5 DOWN WIDTH 73 TITLE "".

DEFINE FRAME f_term 
   b_term SKIP 
   WITH 1 COLUMN SIDE-LABELS ROW 5 CENTERED OVERLAY NO-BOX.



&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"
&GLOB Days "первое,второе,третье,четвертое,пятое,шестое,седьмое,восьмое,девятое,десятое,одиннадцатое,двенадцатое,тринадцатое,четырнадцатое,~
пятнадцатое,шестнадцатое,семнадцатое,восемнадцатое,девятнадцатое,двадцатое,двадцать первое,двадцать второе,двадцать третье,~
двадцать четвертое,двадцать пятое,двадцать шестое,двадцать седьмое,двадцать восьмое,двадцать девятое,тридцатое,тридцать первое"
&GLOB Years "первого,второго,третьего,четвертого,пятого,шестого,седьмого,восьмого,девятого,десятого,одинадцатого,двенадцатого,тринадцатого,~
четырнадцатого,пятнадцатого,шестнадцатого,семнадцатого,восемнадцатого,девятнацатого,двадцатого,двадцать первого,двадцать первого,~
двадцать второго,двадцать третьего,двадцать четвертого,двадцать пятого,шестого,двадцать седьмого,двадцать восьмого,двадцать девятого,тридцатого,тридцать первого,~
тридцать первого,тридцать второго,тридцать третьего,тридцать четвертого,тридцать пятого,тридцать шестого,тридцать седьмого,тридцать восьмого,тридцать девятого"

DEF VAR vTmpStr    AS CHAR    NO-UNDO.
DEF VAR mSignsVal  AS CHAR    NO-UNDO.
DEF VAR mAmt-rub  AS DECIMAL NO-UNDO. /* Для подсчёта начисления */
DEF NEW SHARED VAR rid_loan AS RECID. 
DEF VAR in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEF VAR in-dataClass-id LIKE DataClass.DataClass-id NO-UNDO.
DEF VAR mPointer AS CHARACTER NO-UNDO. /* вектор договора обеспечения */
DEF BUFFER b_user FOR _user.     /* Локализация буфера. */
DEF BUFFER b_person FOR person.     /* Локализация буфера. */
DEF BUFFER b_loan FOR loan.     /* Локализация буфера. */
DEF BUFFER b_term-obl FOR term-obl.     /* Локализация буфера. */

Name_ = entry(1,iStr,"|").

DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").   

find first _user where _user._userid = user_ no-lock no-error.
RUN Insert_TTName("Pod-isp-f", _user._User-Name).
RUN Insert_TTName("Pod-isp-dolg", GetXAttrValueEx("_User", User_, "Должность", "")).
RUN Insert_TTName("Pod-isp-fio", GetXAttrValueEx("_User", User_, "ФИОП", "")).
RUN Insert_TTName("Pod-isp-dov-rp", GetXAttrValueEx("_User", User_, "ДокОснТипРП", "")).
RUN Insert_TTName("Pod-isp-telefon", GetXAttrValueEx("_User", User_, "Телефон", "")).

find first tmprecid NO-LOCK no-error.
if avail tmprecid then do:
   find first loan where RECID(loan) EQ tmprecid.id  NO-LOCK no-error.
   if avail loan then do:
      ASSIGN
         rid-p    = RECID(loan)
         rid_loan = RECID(loan)
      . 
   end.
end.

iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
str_title = "[ Выберите подписанта ]".
mUser = "".
run signat-3p.p ("precrdprvvv02",substr(loan.cont-code,1,2),"0000",str_title,OUTPUT mUser).
if mUser = ? then do:

   message " Не найден подписант для процедуры precrdprvvv02 и переменной " substr(loan.cont-code,1,2) 
           " подразделения " iBranch " определённого по пользователю " user_ view-as alert-box.
   return.
end.


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
   RUN Insert_TTName("bank-name-rp",     GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "БанкНаимРП")). 
END.

  find first b_user where b_user._userid = mUser no-lock no-error.
  RUN Insert_TTName("Pod-user-dolg", GetXAttrValueEx("_User", mUser, "Должность", "")).
  RUN Insert_TTName("Pod-user-dolgRP", GetXAttrValueEx("_User", mUser, "ДолжностьРП", "")).
  RUN Insert_TTName("Pod-user-fio", GetXAttrValueEx("_User", mUser, "ФИОП", "")).
  RUN Insert_TTName("Pod-user-fioRP", GetXAttrValueEx("_User", mUser, "User-nameRP", "")).

  IF NUM-ENTRIES(STRING(b_user._User-Name),".") > 1 THEN vTmpStr = STRING(b_user._User-Name).
  ELSE vTmpStr = ENTRY(1,STRING(b_user._User-Name)," ") + " " +
				   SUBSTRING(ENTRY(2,STRING(b_user._User-Name)," "),1,1,"CHARACTER") + "." +
				   SUBSTRING(ENTRY(3,STRING(b_user._User-Name)," "),1,1,"CHARACTER") + ". ".
  RUN Insert_TTName("Pod-user-fioSok", vTmpStr).
  RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "ДокОснТипРП", "")).
  RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "Телефон", "")).
  RUN Insert_TTName("Pod-user-otdel-rp", GetXAttrValueEx("_User", mUser, "ОтделРП", "")).
  RUN Insert_TTName("Pod-user-fio2", b_user._User-Name).

  vDeystv = "действующего(-ей)". 
  find first cust-role WHERE  cust-role.file-name EQ "_user"                                    
                         AND cust-role.surrogate  EQ mUser                              
                         AND cust-role.class-code EQ "Пользователь"
                         no-lock no-error.
  IF AVAIL cust-role then DO:
     find first b_person where b_person.person-id = int(cust-role.cust-id) no-lock no-error.
     IF AVAIL b_person then DO:
        vDeystv = if b_person.gender then "действующего" else "действующей". 
     end.
  end.
  RUN Insert_TTName ("vDeystv",vDeystv). 

  RUN Insert_TTName("Today",mtoday).
  RUN Insert_TTName("TodayT",string(mtoday,"99.99.9999")).
  RUN Insert_TTName("TodayStr",  STRING(DAY(mtoday))  + " " +  ENTRY(MONTH(mtoday),{&Months}) +  " " + STRING(YEAR(mtoday)) + "г.").

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

/* Получение суммы прописью одним из двух способов: скобки вначале и конце; скобки вначале и после валюты */
PROCEDURE FrmtAmt:
   DEF INPUT  PARAM iAmt      AS DEC   NO-UNDO.
   DEF INPUT  PARAM iCurrency AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iKindAmt  AS INT64 NO-UNDO.
   DEF OUTPUT PARAM oStramt   AS CHAR  NO-UNDO.
   
   DEF VAR vAmtStr AS CHAR  NO-UNDO.
   DEF VAR vDecStr AS CHAR  NO-UNDO.   
   DEF VAR vCnt1   AS INT64 NO-UNDO.
   DEF VAR vCnt2   AS INT64 NO-UNDO.
   
   RUN x-amtstr.p (iAmt, iCurrency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
   IF iKindAmt EQ 2 THEN 
   DO:
      
      oStramt = TRIM(STRING(iAmt, ">>>>>>>>>.99")) + " (" + vAmtStr + " " + vDecStr.
      DO vCnt2 = 0 TO NUM-ENTRIES(oStramt," ") - 1:
         IF ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") NE "" THEN
            vCnt1 = vCnt1 + 1.
            IF vCnt1 EQ 4 THEN
            DO:
               ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") = ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") + ")".
               LEAVE.
            END.
      END.    
   END.
   ELSE
      oStramt = TRIM(STRING(iAmt, ">>>>>>>>>.99")) + " (" + vAmtStr + " " + vDecStr + ")". 
END PROCEDURE.

/* Если договор один, то создаем отметки */
IF     NUM-ENTRIES(iStr)   GE 2 
   AND ENTRY(2, iStr, "|") NE "" THEN
DO:
   {empty tmprecid}
   FIND FIRST loan WHERE
              loan.contract  EQ ENTRY(1, ENTRY(2, iStr, "|")) 
      AND     loan.cont-code EQ ENTRY(2, ENTRY(2, iStr, "|"))
   NO-LOCK NO-ERROR.
   IF AVAIL loan THEN
   DO:
      CREATE tmprecid.
      tmprecid.id = RECID(loan).
   END.
END.

   /* По отмеченным договорам */
FOR EACH tmprecid NO-LOCK: 
   /* Если это договора обеспечения и в строке параметров последний параметр равен mode2 -
   режим печати договоров обеспечения. */
   IF tmprecid.tablename MATCHES "term-obl"
   AND ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE2" THEN 
   DO:
      FIND FIRST term-obl
      WHERE
         RECID(term-obl) EQ tmprecid.id 
      NO-LOCK NO-ERROR.
      mPointer = "|<MODE2>," + STRING(tmprecid.id) + "|".
      FIND FIRST loan WHERE
         loan.contract   = term-obl.contract AND
         loan.cont-code  = term-obl.cont-code
         NO-LOCK NO-ERROR.
      ASSIGN
         rid-p    = RECID(loan)
         rid_loan = RECID(loan)
      .
      {norm-beg.i }
      OUTPUT STREAM fil TO NUL.

      /* Обработка процедурой lgarterm, используется при печати в браузере 
      договоров обеспечения одного или нескольких договоров обеспечения */
      RUN loanagval.p (ENTRY(1, iStr, "|") + mPointer,
                       INPUT-OUTPUT TABLE ttnames).
      /* Заполнение таблицы данными ЭПС */
      RUN FillTables (loan.contract,
                      loan.cont-code).
      OUTPUT STREAM fil CLOSE.
      {norm-end.i &nofil=YES &nopreview=YES} 
         /* Вывод данных по шаблону iStr (до "|") в файл отчета */
      RUN printvd.p (ENTRY(1, iStr, "|"),
                     INPUT TABLE ttnames).
      RUN Clear_TTName.
   END.
   /* Если это договора обеспечения и в строке параметров последний параметр равен mode3 -
   режим печати договора обеспечения с поддержкой множественных обеспечений. */
   ELSE IF tmprecid.tablename MATCHES "term-obl"
   AND ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE3" THEN
   DO:
      FIND FIRST term-obl
      WHERE
         RECID(term-obl) EQ tmprecid.id 
      NO-LOCK NO-ERROR.
      mPointer = mPointer + STRING(tmprecid.id) + ",".
   END.
   ELSE DO:
   /* По отмеченным кредитным договорам */
      FOR EACH loan WHERE 
          RECID(loan) EQ tmprecid.id 
      NO-LOCK:
         ASSIGN
            rid-p    = RECID(loan)
            rid_loan = RECID(loan)
         . 

      FIND LAST loan-acct OF loan WHERE  loan-acct.since <= TODAY 
                                    AND loan-acct.acct-type EQ "КредРасч"
                                  NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN DO:
         RUN Insert_TTName("TKredRasch", entry(1,loan-acct.acct,"@")).
      END.
     /* Поиск наименоваия Суда */

      FIND first code WHERE code.class EQ 'НаименСуда' 
                        AND code.parent EQ 'НаименСуда'  
                        and code.code = substr(loan.cont-code,1,2)
                        NO-LOCK NO-ERROR.     
      IF AVAIL code THEN DO:
         RUN Insert_TTName("NameCourt",  code.description[1]).
         RUN Insert_TTName("NameCity",  code.val).

      END.

      mDateDog = date(GetXAttrValue("loan",loan.contract + "," + loan.cont-code,"ДатаСогл")).
      RUN Insert_TTName ("mDateDogStr",STRING(DAY(mDateDog))  + " " +  ENTRY(MONTH(mDateDog),{&Months}) +  " " + STRING(YEAR(mDateDog)) + " г.").
      
      /* ОСЗ была ли просрочка по кредиту и существует ли задолженность pda 30.07.2016 */ 
      mOstCr    = 0.
      mAcctType = "КредПр,КредПр%,КредПр%ОВ,КредПр%В".

      DO mI = 1 TO NUM-ENTRIES(mAcctType):
         FIND LAST loan-acct OF loan WHERE 
            loan-acct.since <= TODAY 
            AND loan-acct.acct-type EQ ENTRY(mI, mAcctType)
         NO-LOCK NO-ERROR.
            IF AVAIL loan-acct AND mOstCr = 0 THEN DO:
               RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, TODAY, TODAY, ?).
               IF loan-acct.currency = '' THEN 
                  mOstCr = abs(sh-bal).
               ELSE mOstCr = abs(sh-val).
            END.
      END.
      IF mOstCr <> 0 THEN
         RUN Insert_TTName("dolg", 1).   /* была просрочка и есть непогашенная задолженность */
      ELSE RUN Insert_TTName("dolg", 0). /* была просрочка но задолж. погашена */
      
       /*
         {norm-beg.i }
       */
         FIND FIRST loan-cond WHERE
                   loan-cond.contract  EQ loan.contract
            AND    loan-cond.cont-code EQ loan.cont-code
            AND    loan-cond.since     LE gend-date
         NO-LOCK NO-ERROR.
         IF AVAIL loan-cond THEN
            rid-t = RECID(loan-cond).
            /* Обработка процедурами bankinfo,userinfo,dog,lgarterm и др. */
         RUN loanagval.p (ENTRY(1, iStr, "|") + mPointer,
                          INPUT-OUTPUT TABLE ttnames).

            /* Заполнение таблицы данными ЭПС */
         RUN FillTables (loan.contract,
                         loan.cont-code).
      /* справка о ссудной задолжности */
      IF end-date < loan.open-date THEN DO:
         MESSAGE "Дата не может быть меньше даты открытия договора!" VIEW-AS ALERT-BOX.
         RETURN.
      END.
/* реквизиты обеспечения автмобиль */

      mTCVIN   = "".
      mTCmodel = "".
      mTCyear  = "".
      mTCmotor = "".
      mTCcolor = "".
      mTCPerson = "".
      mTCPersonPor = "".

      FOR EACH term-obl WHERE term-obl.contract EQ loan.contract
         AND term-obl.cont-code EQ loan.cont-code
         AND term-obl.idnt EQ 5
         NO-LOCK:
         if GetCodeName("ВидОб",GetXattrValueEx("term-obl", 
      	                                         STRING(term-obl.contract + "," + 
                                                 term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + 
                                                 STRING(term-obl.nn)
                                                ), "ВидОб", "*" )) = "Автомобиль"
         then do:
            mTCVIN   =  GetXattrValueEx("term-obl",STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + STRING(term-obl.nn)),
                                                 "TCVIN","").
            mTCmodel =  GetXattrValueEx("term-obl",STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + STRING(term-obl.nn)),
                                                 "TCmodel","").
            mTCyear  =  GetXattrValueEx("term-obl",STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + STRING(term-obl.nn)),
                                                 "TCyear","").
            mTCmotor =  GetXattrValueEx("term-obl",STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + STRING(term-obl.nn)),
                                                 "TCmotor","").
            mTCcolor =  GetXattrValueEx("term-obl",STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + STRING(term-obl.nn)),
                                                "TCcolor","").
            if term-obl.symbol = "Ч" then do:
               find first b_person where b_person.person-id = term-obl.fop no-lock no-error.
            end.
            if avail b_person then do:
               mTCPerson = b_person.name-last + " " + b_person.first-names.
            end.
         end.
         if GetCodeName("ВидОб",GetXattrValueEx("term-obl", 
      	                                         STRING(term-obl.contract + "," + 
                                                 term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + 
                                                 STRING(term-obl.nn)
                                                ), "ВидОб", "*" )) = "ПОРУЧИТЕЛЬСТВО"
         then do:
            if term-obl.symbol = "Ч" then do:
               find first b_person where b_person.person-id = term-obl.fop no-lock no-error.
            end.
            if avail b_person then do:
               mTCPersonPor = b_person.name-last + " " + b_person.first-names .
            end.
         end.
       end.


       RUN Insert_TTName ("mTCVIN",mTCVIN).             
       RUN Insert_TTName ("mTCmodel",mTCmodel).             
       RUN Insert_TTName ("mTCyear",mTCyear).             
       RUN Insert_TTName ("mTCmotor",mTCmotor).             
       RUN Insert_TTName ("mTCcolor",mTCcolor).             
       RUN Insert_TTName ("mTCPerson",mTCPerson).             
       RUN Insert_TTName ("mTCPersonPor",mTCPersonPor).             

/* конец реквизиты обеспечения автмобиль */

/* реквизиты клиента ФЛ */
      if loan.cust-cat = "Ч" then do:

         find first person where person.person-id = loan.cust-id.
         if avail person then do:
            RUN Insert_TTName("birthday", string(person.birthday,"99.99.9999")).
            RUN Insert_TTName("birthday", string(person.birthday,"99.99.9999")).
            vDovTDoc = person.document-id.
            vDovNDoc = person.document.
            RUN Insert_TTName ("ddnu",vDovNDoc).
            vTmpDat = DATE(getxattrvalue ("person",string(person.person-id), "Document4Date_vid")).
            vDovDDoc = STRING(DAY(vTmpDat)) + " " + ENTRY(MONTH(vTmpDat),{&Months}) + " " + STRING(YEAR(vTmpDat)).
            RUN Insert_TTName ("ddvy",vDovDDoc).   
            vDovKDoc = REPLACE(person.issue,",",", к/п").
            Case Name_ :
               when  "ind-usl-3"   then  vDovKDoc = REPLACE(person.issue,",",", код подразделения ").
               when  "ind-usl-2"   then  vDovKDoc = REPLACE(person.issue,",",", код подразделения ").
               when  "ind-usl-por" then  vDovKDoc = REPLACE(person.issue,",",", код подразделения ").
               otherwise
                  do:
                     vDovKDoc = REPLACE(person.issue,",",", к/п ").
                  end.
            END CASE.
            RUN Insert_TTName ("ddkem",vDovKDoc).
            RUN Insert_TTName ("p-imenu",if person.gender then "именуемый" else "именуемая" ). 
            RUN Insert_TTName ("p-zareg",if person.gender then "зарегистрированный" else "зарегистрированная" ). 
         end.
      end.
/* конец реквизитов клиента ФЛ */

/* определение договора поручительства и реквизитов поручителя. 
   Договор поручительства определяется при наличии последним параметром mode9 */    
   IF ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE9" THEN 
   DO:
      FOR EACH term-obl WHERE term-obl.contract EQ loan.contract
         AND term-obl.cont-code EQ loan.cont-code
         AND term-obl.idnt EQ 5
         NO-LOCK:
         if CAN-DO("ПОРУЧИТЕЛЬСТВО,Автомобиль",GetCodeName("ВидОб",GetXattrValueEx("term-obl", 
      	                                         STRING(term-obl.contract + "," + 
                                                 term-obl.cont-code + ",5," + 
                                                 STRING(term-obl.end-date,"99/99/99") + "," + 
                                                 STRING(term-obl.nn)
                                                ), "ВидОб", "*" )))
         then do:
            if term-obl.symbol = "Ч" then do:
               find first b_person where b_person.person-id = term-obl.fop no-lock no-error.
            end.
            CREATE tt.
            ASSIGN
               tt.NomDog  = GetXattrValueEx("term-obl", 
                                            STRING(term-obl.contract + "," + 
                                                   term-obl.cont-code + ",5," + 
                                                   STRING(term-obl.end-date,"99/99/99") + "," + 
                                                   STRING(term-obl.nn)
                                                   ), 
                                            "НомДогОб", "*"
                                            )
               tt.NomPP   = term-obl.nn
               tt.CodeVal = GetCodeName("ВидОб", 
      	 	                                     GetXattrValueEx("term-obl", 
      	 	                                                     STRING(term-obl.contract + "," + 
                                                               term-obl.cont-code + ",5," + 
                                                               STRING(term-obl.end-date,"99/99/99") + "," + 
                                                               STRING(term-obl.nn)
                                                               ), 
                                                        "ВидОб", "*"
                                                        )
      	 	                                    )
               tt.term-obl-id = recid(term-obl)
               tt.ChVal = if term-obl.symbol = "Ч" then b_person.name-last + " " + b_person.first-names else "".
               tt.Cust-Cat = term-obl.symbol.
               tt.Cust-id  = term-obl.fop.
            .
            ACCUMULATE term-obl.nn (count).
         end.
      END. 
      /* если один договор обеспечения*/
      IF (ACCUM count term-obl.nn) < 2 THEN
      DO:
         FIND FIRST tt NO-LOCK NO-ERROR.
         sTermRecid = IF AVAIL tt THEN tt.term-obl-id ELSE ?.
         sTermRecid = tt.term-obl-id.
         mCust-Cat  = tt.Cust-Cat.
         mCust-id   = tt.Cust-id .

      END.
      /*выбор договора обеспечения только для указанных отчетов*/
      IF sTermRecid = ? AND (ACCUM count term-obl.nn) > 1 THEN 
      DO:
         b_term:NUM-LOCKED-COLUMNS = 2.
         b_term:TITLE = " [ " + DelFilFromLoan(loan.cont-code) + ", ВЫБОР ПОРУЧИТЕЛЯ ] ".
          
         OPEN QUERY q_term FOR EACH tt NO-LOCK BY tt.NomPP.
         PAUSE 0.
         VIEW b_term.
         ENABLE ALL WITH FRAME f_term.
         WAIT-FOR ENTER,ESC OF FRAME f_term.
         HIDE FRAME f_term.
         IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN 
         DO:
            sTermRecid = tt.term-obl-id.
            mCust-Cat  = tt.Cust-Cat.
            mCust-id   = tt.Cust-id .
         END.

         IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN return.
      END.
   END.
   if sTermRecid <> ? then do:
      find first b_term-obl where recid(b_term-obl) = sTermRecid no-lock no-error.
      RUN Insert_TTName ("NumDogPor",tt.NomDog).
      RUN Insert_TTName("DateDogPorStr",  STRING(DAY(b_term-obl.fop-date))  + " " +  ENTRY(MONTH(b_term-obl.fop-date),{&Months}) +  " " + STRING(YEAR(b_term-obl.fop-date)) + "г.").
      RUN Insert_TTName ("DateDogPor",b_term-obl.fop-date).
      if mCust-Cat = "Ч" then do:
         find first b_person where b_person.person-id = mCust-id no-lock no-error.
         IF AVAIL b_person then DO:
            RUN Insert_TTName ("pfio",b_person.name-last + " " + b_person.first-names).
            RUN Insert_TTName ("pbir",STRING(DAY(b_person.birthday)) + " " + ENTRY(MONTH(b_person.birthday),{&Months}) + " " + STRING(YEAR(b_person.birthday))).		
            RUN Insert_TTName ("pbipl",getxattrvalue ("person",string(b_person.person-id), "BirthPlace")).		
            RUN Insert_TTName ("pdnu",b_person.document).		
            vTmpDate = DATE(getxattrvalue ("person",string(b_person.person-id), "Document4Date_vid")).
            RUN Insert_TTName ("pdvy",STRING(DAY(vTmpDate)) + " " + ENTRY(MONTH(vTmpDate),{&Months}) + " " + STRING(YEAR(vTmpDate))).		
            RUN Insert_TTName ("pdkem1",entry(1,b_person.issue)).		
            RUN Insert_TTName ("pdkem2","код подразделения " + entry(2,b_person.issue)).		
            RUN RetAdr.p(b_person.person-id,"Ч","АдрПроп",?,OUTPUT vPolAdr).
            RUN Insert_TTName ("padr",vPolAdr).
            RUN Insert_TTName ("imenu",if b_person.gender then "именуемый" else "именуемая" ). 
            vDate = date(GetXAttrValue("loan",loan.contract + "," + loan.cont-code,"ДатаСогл")).
            RUN Insert_TTName("TDateSogl",STRING(DAY(vDate)) + " " + ENTRY(MONTH(vDate),{&Months}) + " " + STRING(YEAR(vDate))).
         END.
      end.
   end.
   else do:
      IF ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE9" THEN do:
         message "Договора поручительства не найдены." view-as alert-box.
         return.
      end.
   end.

/* конец определения договора поручительства */

/* Отсюда и ниже - параметры договора. Атрибуты договора вставляем выше.*/


 
     /* так как нам не удастся узнать параметры без пересчета*/
      
      RUN l-calc2.p ("Кредит",       /* Назначение договора. */
                     loan.cont-code,      /* Номер договора. */
                     date(end-date),   /* Окончание договора + день для выполнения автом. */
                     FALSE,      /* включать/не включать пересчет течений договора */
                     TRUE
                    ).     /* выводить/ не выводить протокол на экран */
      


     
      /* срочная задолженность */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               0,                /* Код параметра */
                               end-date,
                               OUTPUT par_0,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */
      
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               2,                /* Код параметра */
                               end-date,
                               OUTPUT par_2,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */
      
      /* сумма % на основной долг 4,29 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               4,                /* Код параметра */
                               end-date,
                               OUTPUT par_4,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* сумма просроч. основного долга */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               7,                /* Код параметра */
                               end-date,
                               OUTPUT par_7,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* сумма % на просроч. основной долг 8,229 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               8,                /* Код параметра */
                               end-date,
                               OUTPUT par_8,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */



      /* сумма пени */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               9,                /* Код параметра */
                               end-date,
                               OUTPUT par_9,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* сумма просроч. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               10,               /* Код параметра */
                               end-date,
                               OUTPUT par_10,    /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               12,                /* Код параметра */
                               end-date,
                               OUTPUT par_12,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               13,                /* Код параметра */
                               end-date,
                               OUTPUT par_13,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               26,                /* Код параметра */
                               end-date,
                               OUTPUT par_26,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* сумма % на основной долг 4,29 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               29,               /* Код параметра */
                               end-date,
                               OUTPUT par_29,    /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* ??? */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               32,               /* Код параметра */
                               end-date,
                               OUTPUT par_32,    /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* ??? */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               33,               /* Код параметра */
                               end-date,
                               OUTPUT par_33,    /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */
      
      /* ??? */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               34,               /* Код параметра */
                               end-date,
                               OUTPUT par_34,    /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* ??? */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               35,               /* Код параметра */
                               end-date,
                               OUTPUT par_35,    /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* сумма просроч. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               48,               /* Код параметра */
                               end-date,
                               OUTPUT par_48,    /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */
      
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               82,                /* Код параметра */
                               end-date,
                               OUTPUT par_82,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               173,              /* Код параметра */
                               end-date,
                               OUTPUT par_173,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               109,              /* Код параметра */
                               end-date,
                               OUTPUT par_109,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               209,              /* Код параметра */
                               end-date,
                               OUTPUT par_209,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */


      /* сумма просроч. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               210,              /* Код параметра */
                               end-date,
                               OUTPUT par_210,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* сумма % на просроч. основной долг 8,229 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               229,              /* Код параметра */
                               end-date,
                               OUTPUT par_229,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               233,              /* Код параметра */
                               end-date,
                               OUTPUT par_233,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */
      
      /* сумма просроч. % 10,210,48,248 */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               248,              /* Код параметра */
                               end-date,
                               OUTPUT par_248,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               509,                /* Код параметра */
                               end-date,
                               OUTPUT par_509,     /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */


      /* пеня за непредоставление ПТС */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               519,              /* Код параметра */
                               end-date,
                               OUTPUT par_519,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               526,              /* Код параметра */
                               end-date,
                               OUTPUT par_526,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               530,              /* Код параметра */
                               end-date,
                               OUTPUT par_530,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */
      RUN ALL_PARAM IN h_Loan ("Кредит",         /* Тип договора */
                               loan.cont-code,   /* Номер договора */
                               531,              /* Код параметра */
                               end-date,
                               OUTPUT par_531,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */

      /* операция 83 */
      mAmt-rub = 0.
      oper_83  = 0.
      FOR EACH loan-int WHERE loan-int.cont-code EQ loan.cont-code
                          AND loan-int.contract EQ loan.contract
                          and (loan-int.id-d eq 29 and loan-int.id-k eq 30) 
                          and loan-int.mdate = end-date 
                          NO-LOCK.                                   
         mAmt-rub = mAmt-rub  + loan-int.Amt-rub.
      end.
      oper_83 =  mAmt-rub.


      /* операция 283 */
      mAmt-rub = 0.
      oper_283  = 0.
      FOR EACH loan-int WHERE loan-int.cont-code EQ loan.cont-code
                          AND loan-int.contract EQ loan.contract
                          and (loan-int.id-d eq 229 and loan-int.id-k eq 30) 
                          and loan-int.mdate = end-date 
                          NO-LOCK.                                   
         mAmt-rub = mAmt-rub  + loan-int.Amt-rub.
      end.
      oper_283 =  mAmt-rub.


      RUN Insert_TTName("Loan_End_Date", STRING( loan.end-date, '99.99.9999' )).


      mSignsVal = GetXAttrValue("loan",
                           loan.contract + "," + loan.cont-code,
                           "ДатаСогл").
      RUN Insert_TTName("Loan_Beg_Date", STRING( date(mSignsVal), '99.99.9999' )).
      
      /* комиссия за выдачу */
      /*
      mCommFirst = GET_COMM ("%Выд",                     /* Код комиссии */
               ?,                                    /* RecId счета*/
               loan.currency,                        /* Валюта*/
               loan.contract + "," + loan.cont-code, /* Кау*/
               0.00,                                 /* MIN остаток */
               0,                                    /* Период */
               end-date).                            /* Дата */
      */
      RUN ALL_PARAM IN h_Loan ("Кредит",        /* Тип договора */
                               loan.cont-code,  /* Номер договора */
                               377,             /* Код параметра */
                               end-date,
                               OUTPUT par_377,   /* Сумма параметра */
                               OUTPUT a1,        /* Валюта параметра */
                               OUTPUT a2
                              ).                 /* Сумма параметра в рублях */
      
      IF par_377 <> ? AND par_377 > 0 THEN
         DO:
            RUN FrmtAmt(par_377, loan.currency, 2, OUTPUT rproc).
            RUN Insert_TTName("firstprc", rproc ).
         END.
      
      IF par_377 = ? OR par_377 = 0 THEN
         RUN Insert_TTName("firstprc", 0 ).
      .  
      /* комиссия РКО */
      /*
      mCommRKO = GET_COMM ("%РКО",                    /* Код комиссии */
               ?,                                    /* RecId счета*/
               loan.currency,                        /* Валюта*/
               loan.contract + "," + loan.cont-code, /* Кау*/
               0.00,                                 /* MIN остаток */
               0,                                    /* Период */
               end-date).                            /* Дата */
      */
      RUN ALL_PARAM IN h_Loan ("Кредит",  /* Тип договора */
            loan.cont-code,             /* Номер договора */
            301,           /* Код параметра */
            end-date,
            OUTPUT par_301,            /* Сумма параметра */
            OUTPUT a1,                  /* Валюта параметра */
            OUTPUT a2).                 /* Сумма параметра в рублях */
            
      IF par_9 = ? OR par_9 = 0 THEN
         RUN Insert_TTName("peni", 0).
      
      IF  par_301 <> ? AND par_301 > 0 THEN
         DO:
            RUN FrmtAmt(par_301, loan.currency, 2, OUTPUT rproc).
            RUN Insert_TTName("rkoprc", rproc).
         END.
                          
      IF par_301 = ? OR par_301 = 0 THEN
         RUN Insert_TTName("rkoprc", 0).
            
      /**/
      RUN FrmtAmt(par_0 + par_4 + par_377 + par_33 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("remn_total_str", rproc ).
      RUN Insert_TTName("remn_total", par_0 + par_4 + par_377 + par_33).
      RUN Insert_TTName("remn_date", STRING( end-date, '99.99.9999' )).

      RUN Insert_TTName("remn_base", par_0).
      
      /* остаток задолженности суммарный */
      RUN FrmtAmt(par_0 +  par_2 + par_4 + par_7 + par_8 + par_9  + par_10 +  par_12 + par_13 + par_29 + par_26 + par_32 + par_33 + par_34 + par_35 + 
                  par_48 + par_82 + par_109 + par_173 + par_209 + par_210 + par_229 + par_233 + par_248 + par_301 + par_377 + 
                  par_509 + par_519 + par_526 + par_531 - oper_83 - oper_283 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_total", rproc ).

      /* сумма % на основной долг 4,29,32,33,34,35 */
      RUN FrmtAmt(par_4 + par_29 + par_32 + par_33 + par_34 + par_35 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc_dolg", rproc ).

      /* сумма % на основной долг 4,29,32,33,35 - операция 83 (29,30) (Стенина) */

      RUN FrmtAmt(par_4 + par_29 + par_32 + par_33 + par_35 - oper_83 ,loan.currency,2, OUTPUT rproc).

      RUN Insert_TTName("amt_proc_dolg_2", rproc ).



      /* сумма просроченных процентов по кредиту ___; 10+48+173+377 */
      RUN FrmtAmt(par_10 + par_48 + par_173 + par_377 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_prosr_proc", rproc ).
      
      /* сумма просроч. основного долга */
      RUN FrmtAmt(par_7 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_osn_dolg", rproc ).

      /* сумма просроч. % 10,210,48,248 */
      RUN FrmtAmt(par_10 + par_210 + par_48 + par_248 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc", rproc ).

      /* сумма пени */
      RUN FrmtAmt(par_9 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_peni", rproc ).

      /* сумма пени сумма пени (Стенина). 9+12+82+26+509+531 */
      RUN FrmtAmt(par_9  + par_12 + par_82 + par_26 + par_509 + par_531 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_peni_2", rproc ).

      /* пеня за непредоставление ПТС */
      RUN FrmtAmt(par_519 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("peni_PTS", rproc ).

      /* пеня за непредоставление ПТС 2 ^ 519+526 (Стенина)*/
      RUN FrmtAmt(par_519 + par_526,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("peni_PTS_2", rproc ).

      /* сумма % на просроч. основной долг 8,229,233 */
      RUN FrmtAmt(par_8 + par_229 + par_233 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc_osn", rproc ).

      /* Cумма процентов на просроченный основной долг ;8+210+233+229+248  - операция 283 (229,30)  */
      RUN FrmtAmt(par_8 + par_210 + par_233 + par_229  + par_248 - oper_283 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_prosr_osn", rproc ).

      /* Задолженность по государственная пошлине 530*  (Стенина)*/
      RUN FrmtAmt(par_530,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("dolg_gos_posh", rproc ).

      /* Комиссия за РКО  - 109+209+301 (Стенина) */
      RUN FrmtAmt(par_109 + par_209 + par_301 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("rkoprc_2", rproc ).

      /**/
        FIND FIRST term-obl
        WHERE term-obl.cont-code = loan.cont-code
            and term-obl.contract = loan.contract
            and term-obl.idnt = 2
        NO-LOCK NO-ERROR.

        IF AVAILABLE term-obl THEN
        DO:
        
         RUN FrmtAmt(term-obl.amt,loan.currency,2, OUTPUT rproc).
        END.
        
      RUN Insert_TTName("limit", rproc). 
      RUN x-amtstr.p(term-obl.amt, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
      RUN Insert_TTName("limit01", string(term-obl.amt) + " (" + trim(mAmtStr) + ")"). 



      /*------------------------------------------*/
      
      RUN FrmtAmt(par_0, loan.currency, 2, OUTPUT rproc).
      RUN Insert_TTName("remn_base_str", rproc).

      /* остаток основного долга 0+13+2   */

      RUN FrmtAmt(par_0 + par_13 + par_2 , loan.currency, 2, OUTPUT rproc).
      RUN Insert_TTName("remn_osn_dolg", rproc).


      /**/
      RUN Insert_TTName("remn_prc", par_4 + par_33).
      RUN FrmtAmt(par_4 + par_33, loan.currency, 2, OUTPUT rproc).
      RUN Insert_TTName("remn_prc_str", rproc).
      mCommRate = GET_COMM ("%КрПр",                     /* Код комиссии */
               ?,                                    /* RecId счета*/
               loan.currency,                        /* Валюта*/
               loan.contract + "," + loan.cont-code, /* Кау*/
               0.00,                                 /* MIN остаток */
               0,                                    /* Период */
               end-date).                            /* Дата */
               

      RUN Insert_TTName("CommPrc", STRING(mCommRate, '99.99')).
      RUN x-amtstr.p(mCommRate, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
      out_proc = STRING(mAmtStr) .
      if truncate(mCommRate,0) <> mCommRate then do: 
          RUN x-amtstr.p (DEC(mDecStr),"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
          out_proc = out_proc + " целых " + LC(STRING(mAmtStr)) + "сотых ".
      end.
      RUN Insert_TTName("CommPrcStr", out_proc).
      
      mEps = GetXAttrValue("loan",
                           loan.contract + "," + loan.cont-code,
                           "ЭПС"). 
      RUN Insert_TTName("Eps", STRING(mEps, '99.99')).
      RUN x-amtstr.p(mEps, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
      out_proc = STRING(mAmtStr) .
      if truncate(dec(mEps),0) <> dec(mEps) then do:
      RUN x-amtstr.p ((DEC(mEps) - truncate(DEC(mEps),0)) * 1000,"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
         out_proc = out_proc + " целых " + LC(STRING(mAmtStr)) + "тысячных ".
         out_proc = lc(out_proc).
      end.
      RUN Insert_TTName("EpsStr", out_proc).
      

      mPsk = GetXAttrValue("loan",
                           loan.contract + "," + loan.cont-code,
                           "ПСК"). 
      RUN Insert_TTName("Psk", STRING(mPsk, '99.99')).
      RUN x-amtstr.p(mPsk, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
      out_proc = STRING(mAmtStr) + "целых ".
      RUN x-amtstr.p ((DEC(mPsk) - truncate(DEC(mPsk),0)) * 1000,"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
      out_proc = out_proc + LC(STRING(mAmtStr)) + "тысячных ".
      out_proc = lc(out_proc).
      RUN Insert_TTName("PskStr", out_proc).

      FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
          AND loan-cond.cont-code EQ loan.cont-code
      NO-LOCK NO-ERROR.
     
      xz = DEC(GetXAttrValueEx("loan-cond",loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since, "99/99/99"),
           "АннуитПлат", "0")).
         
      RUN Insert_TTName("AnnPl+", xz).   
      
      RUN FrmtAmt(xz,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("AnnPlStr", rproc).
      /* ------------------------------- */
     

         OUTPUT STREAM fil CLOSE.
         
         {norm-end.i &nofil=YES &nopreview=YES} 
            /* Вывод данных по шаблону iStr (до "|") в файл отчета */
         
         RUN printvd.p (ENTRY(1, iStr, "|"),
                        INPUT TABLE ttnames).
                  
      END.
   END.
END.
/* 0204655 */
IF ENTRY(NUM-ENTRIES(iStr, "|"), iStr, "|") EQ "MODE3" THEN DO:
   mPointer = SUBSTR(mPointer,1,LENGTH(mPointer) - 1).
   mPointer = "|<MODE3>," + mPointer + "|".
   FIND FIRST loan WHERE
      loan.contract   = term-obl.contract AND
      loan.cont-code  = term-obl.cont-code
      NO-LOCK NO-ERROR.
   ASSIGN
      rid-p    = RECID(loan)
      rid_loan = RECID(loan)
   . 
   OUTPUT STREAM fil TO NUL.
   RUN loanagval.p (ENTRY(1, iStr, "|") + mPointer,
                    INPUT-OUTPUT TABLE ttnames).
    
      /* Заполнение таблицы данными ЭПС */
   RUN FillTables (loan.contract,
                   loan.cont-code).
   OUTPUT STREAM fil CLOSE.
      /* Вывод данных по шаблону iStr (до "|") в файл отчета */
   RUN printvd.p (ENTRY(1, iStr, "|"),
                  INPUT TABLE ttnames).
END.
/* END of 0204655 */

   /* Заполнение таблицы данными ЭПС */
PROCEDURE FillTables:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
     
   RUN filleps.p (loan.contract, 
                  loan.cont-code, 
                  loan.since, 
                  OUTPUT TABLE ttReportTable).
  
   RUN Insert_TTName ("info", ""). 
   
   FIND FIRST ttNames WHERE
              ttnames.tname EQ 'info'
   NO-LOCK NO-ERROR.
   
   FOR EACH ttReportTable 
   BREAK BY ttReportTable.tf_payment-date:
      ttnames.tvalue = ttnames.tvalue + STRING(ttReportTable.tf_payment-date)   + '\n'
                                      + STRING(ttReportTable.tf_sum-percent)    + '\n'
                                      + STRING(ttReportTable.tf_basic-sum-loan) + '\n'
                                      + STRING(ttReportTable.tf_actual-payment) + '\n' 
                                      + STRING(ttReportTable.tf_sum-percent     +
                                               ttReportTable.tf_basic-sum-loan  +
                                               ttReportTable.tf_actual-payment) + '\n'.
   END.

END PROCEDURE.



/*
      RUN x-amtstr.p (DEC(mDecStr),"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
      if (mCommRate - truncate(mCommRate,0)) <> 0 then do:
         out_proc = out_proc + " целых " + LC(STRING(mAmtStr)) + " сотых ".
      end.
      RUN Insert_TTName("CommPrcStr", out_proc).
      
      mEps = GetXAttrValue("loan",
                           loan.contract + "," + loan.cont-code,
                           "ЭПС"). 
      RUN Insert_TTName("Eps", STRING(mEps, '99.99')).
      RUN x-amtstr.p(mEps, "", NO, NO, OUTPUT mAmtStr, OUTPUT mDecStr).
      out_proc = STRING(mAmtStr) + "целых ".
      RUN x-amtstr.p ((DEC(mEps) - truncate(DEC(mEps),0)) * 1000,"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
      out_proc = out_proc + LC(STRING(mAmtStr)) + "тысячных ".
      out_proc = lc(out_proc).
      RUN Insert_TTName("EpsStr", out_proc).

*/
