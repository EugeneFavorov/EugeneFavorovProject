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
{intrface.get refer} /*для работы со справочниками*/                         
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

DEF VAR chetFil AS CHAR NO-UNDO. /*"счет получателя" для печатных видов*/
DEFINE VARIABLE podOtdel    AS CHAR  NO-UNDO. /* отделение подписанта */
DEF VAR mSignsVal  AS CHAR    NO-UNDO.
 
DEF VAR mAmt-rub  AS DECIMAL NO-UNDO. /* Для подсчёта начисления */

DEF NEW SHARED VAR rid_loan AS RECID. 

DEF VAR in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEF VAR in-dataClass-id LIKE DataClass.DataClass-id NO-UNDO.
DEF VAR mPointer AS CHARACTER NO-UNDO. /* вектор договора обеспечения */

DEF BUFFER b_user FOR _user.     /* Локализация буфера. */


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

/*вызов подписантов zss*/
CASE iStr:
WHEN 'tr_dosr' THEN run signat.p ("sogl_otst","UserName",str_title,OUTPUT mUser).
WHEN 'otv_tar_pp' THEN run signat.p ("spotven","UserName",str_title,OUTPUT mUser).
WHEN 'soglotst2' THEN run signat.p ("sogl_otst","UserName",str_title,OUTPUT mUser).
OTHERWISE run signat.p ("precrdprvvv02","UserName",str_title,OUTPUT mUser).
END.




if mUser = ? then return.

  RUN Insert_TTName("Pod-user-dolg", GetXAttrValueEx("_User", mUser, "Должность", "")).
  RUN Insert_TTName("Pod-user-fio", GetXAttrValueEx("_User", mUser, "ФИОП", "")).
  RUN Insert_TTName("Pod-user-fio-rp",  GetXAttrValueEx("_User",mUser,"User-NameRP","")).
  RUN Insert_TTName("Pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "ДокОснТипРП", "")).
  RUN Insert_TTName("Pod-user-otdel-rp",GetXAttrValueEx("_User",mUser,"ОтделРП","")).
  RUN Insert_TTName("Pod-user-dolg-rp", GetXAttrValueEx("_User",mUser,"ДолжностьРП","")).
  RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User", mUser, "Телефон", "")).
  RUN Insert_TTName("Pod-user-otdel-rp", GetXAttrValueEx("_User", mUser, "ОтделРП", "")).


  find first b_user where b_user._userid = mUser no-lock no-error.

/*zss вывод фамилии в формате И.О. Фамилия*/
FUNCTION getShortSur RETURN CHAR (INPUT vUserName AS CHAR).

DEF VAR vSurname AS CHAR NO-UNDO.

IF NUM-ENTRIES(vUserName," ") > 2 THEN 
DO:
   vSurname = SUBSTRING(ENTRY(2,vUserName," "),1,1,"CHARACTER") + "." +
                SUBSTRING(ENTRY(3,vUserName," "),1,1,"CHARACTER") + ". " + 
                ENTRY(1,vUserName," ").
END.
ELSE vSurname = vUserName. 

RETURN vSurname.
END FUNCTION.

/*zss определение счета получателя в печатных видах Например:кредиты - word омск - ответы на запросы кл -Счет получателя */
  chetFil =  GetRefVal("ДохРасх", TODAY, shFilial + ",810,Ч,pechat,Д"). /* получение счета из справочника "ДохРасх"*/
 /*-------------------*/
RUN Insert_TTName("Filial-chet-dohod", chetFil).

RUN Insert_TTName("Pod-user-fio3", getShortSur(GetXAttrValueEx("_User", mUser,"ФИОП",""))).


  RUN Insert_TTName("Pod-user-fio2", b_user._User-Name).


  RUN Insert_TTName("Pod-isp-dolg", GetXAttrValueEx("_User", User_, "Должность", "")).
  RUN Insert_TTName("Pod-isp-fio", GetXAttrValueEx("_User", User_, "ФИОП", "")).
  RUN Insert_TTName("Pod-isp-dov-rp", GetXAttrValueEx("_User", User_, "ДокОснТипРП", "")).
  RUN Insert_TTName("Pod-isp-telefon", GetXAttrValueEx("_User", User_, "Телефон", "")).

  /*zss из sign_select.i*/
  podOtdel  = GetXAttrValueEx("_User", mUser, "Отделение", "").
  IF podOtdel EQ "0518" THEN podOtdel = "0500".

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
/* реквизиты клиента ФЛ */
      if loan.cust-cat = "Ф" then do:

         find first person where person.person-id = loan.cust-id.
         if avail person then do:
            RUN Insert_TTName("birthday", string(person.birthday,"99.99.9999")).
         end.
      end.
/* конец реквизитов клиента ФЛ */
/* определяем реквизиты филиала, по коду филиала на договоре */

/*   FGetSettingMF (ipCode, ipSubCode, ipDefValue, ipFilial, ipNoCashe). */

FIND FIRST branch WHERE branch.Branch-Id EQ loan.filial-id NO-LOCK NO-ERROR.

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

/*по отделению подписанта*/
FIND FIRST branch WHERE branch.Branch-Id EQ podOtdel NO-LOCK NO-ERROR.

IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("gorodOtd", entry(2,branch.Address)). 
   IF branch.branch-type EQ "23" AND branch.Address NE "" THEN
   DO:
      RUN Insert_TTName("otdel-name",    branch.name). 
      RUN Insert_TTName("otdel-address", "Место нахождения: " + branch.Address). 
   END.
END.

/* конец реквизитов по филиалу */


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

      RUN Insert_TTName("remn_date_GoMonth", STRING(GoMonth(end-date,1), '99.99.9999' )). /*zss прибавление одного месяца к введенной дате*/

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
      RUN Insert_TTName("amt_proc_dolg_2_chislo", par_4 + par_29 + par_32 + par_33 + par_35 - oper_83 ).
      RUN Insert_TTName("amt_proc_dolg_2", rproc ).

      /* сумма просроченных процентов по кредиту ___; 10+48+173+377 */
      RUN FrmtAmt(par_10 + par_48 + par_173 + par_377 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_prosr_proc", rproc ).
      
      /* сумма просроч. основного долга */
      RUN FrmtAmt(par_7 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_osn_dolg_chislo", STRING(par_7, ">>>>>>>>>.99")).
      RUN Insert_TTName("amt_osn_dolg", rproc ).

      /* сумма просроч. % 10,210,48,248 */
      RUN FrmtAmt(par_10 + par_210 + par_48 + par_248  ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc", rproc ).

      /*общий размер просроченной задолжности по кредитному договору zss*/
      RUN FrmtAmt(par_10 + par_210 + par_48 + par_248 + par_9  + par_12 + par_82 + par_26 + par_509 + par_531 + par_7 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_prosr_obh_chislo", par_10 + par_210 + par_48 + par_248 + par_9  + par_12 + par_82 + par_26 + par_509 + par_531 + par_7).
      RUN Insert_TTName("amt_prosr_obh", rproc).


      /*сумма просроч % за пользование кредитом 10,210,48,248,29  zss */
   	  RUN FrmtAmt(par_10 + par_210 + par_48 + par_248 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_proc_kredit_chislo", STRING(par_10 + par_210 + par_48 + par_248, ">>>>>>>>>.99")).
      RUN Insert_TTName("amt_proc_kredit", rproc ).

      /* сумма пени */
      RUN FrmtAmt(par_9 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_peni", rproc ).

      /* сумма пени сумма пени (Стенина). 9+12+82+26+509+531 zss*/
      RUN FrmtAmt(par_9  + par_12 + par_82 + par_26 + par_509 + par_531 ,loan.currency,2, OUTPUT rproc).
      RUN Insert_TTName("amt_peni_2_chislo",STRING( par_9  + par_12 + par_82 + par_26 + par_509 + par_531, ">>>>>>>>>.99")).
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
      out_proc = STRING(mAmtStr) + "целых ".
      RUN x-amtstr.p (DEC(mDecStr),"",NO,NO,OUTPUT mAmtStr,OUTPUT mDecStr).
      out_proc = out_proc + LC(STRING(mAmtStr)) + "сотых ".
      
      RUN Insert_TTName("CommPrcStr", out_proc).
      
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

