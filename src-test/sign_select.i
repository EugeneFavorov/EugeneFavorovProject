/*выбор подписантов*/

DEFINE VARIABLE mUser       AS CHAR  NO-UNDO.
DEFINE VARIABLE mProcName   AS CHAR  NO-UNDO.
DEFINE VARIABLE mUserProcId AS INT64 NO-UNDO.
DEFINE VARIABLE mSignProc   AS CHAR  NO-UNDO.

DEFINE VARIABLE iBranch     AS CHAR  NO-UNDO.
DEFINE VARIABLE user_       AS CHAR  NO-UNDO.
DEFINE VARIABLE shfilial_   AS CHAR  NO-UNDO. /* филиал подписанта */
DEFINE VARIABLE podOtdel    AS CHAR  NO-UNDO. /* отделение подписанта */
DEFINE VARIABLE str_title   AS CHAR  NO-UNDO.
user_ = USERID("bisquit").   

/*pda Функция возвращает имя в формате Фамилия И.О.*/
FUNCTION getShortName RETURN CHAR (INPUT vUserName AS CHAR).
   DEF VAR vShortName AS CHAR NO-UNDO.
   IF NUM-ENTRIES(vUserName," ") > 2 THEN
   DO:
      vShortName = ENTRY(1,vUserName," ") + " " +
                   SUBSTRING(ENTRY(2,vUserName," "),1,1,"CHARACTER") + "." +
                   SUBSTRING(ENTRY(3,vUserName," "),1,1,"CHARACTER") + ".".
   END.
   ELSE vShortName = vUserName.
   RETURN vShortName.
END FUNCTION.

/*zss ф-ция возвращает подписанта в формате И.О. Фамилия*/

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



FIND FIRST _user WHERE _user._userid = user_ NO-LOCK NO-ERROR.
RUN Insert_TTName("Pod-isp-f", _user._User-Name).

/* узнаем имя процедуры для поиска по классификатору signat */
&IF DEFINED(gdSignProc) 
   &THEN mProcName = {&gdSignProc}. 
   &ELSE mProcName = THIS-PROCEDURE:FILE-NAME. 
&ENDIF

IF NUM-ENTRIES(mProcName, ".") > 1 THEN 
   mProcName = ENTRY(1, mProcName, ".").

iBranch   = GetXattrValueEx("_user", user_, "Отделение",?).
str_title = "[ Выберите подписанта (" + user_ + " " + iBranch + ")]".
mUser     = "".

/* проверяем по ДР нужно ли выводить список подписантов */
mUserProcId = INT64(GetSysConf("user-proc-id")). 
FIND FIRST user-proc 
     WHERE RECID(user-proc) EQ mUserProcId 
NO-LOCK NO-ERROR.
IF AVAILABLE user-proc THEN
   mSignProc = GetXAttrValue("user-proc", STRING(user-proc.public-number), "Подписи").

IF mSignProc NE  "БезПодп" THEN
   RUN signat.p (mProcName, "UserName", str_title, OUTPUT mUser).

IF mUser = ? OR mUser = "" 
   THEN mUser = user_.
   RUN Insert_TTName("Pod-user-dolg",    GetXAttrValueEx("_User",mUser,"Должность","")).
   RUN Insert_TTName("Pod-user-dolg-rp", GetXAttrValueEx("_User",mUser,"ДолжностьРП","")).
   RUN Insert_TTName("Pod-user-otdel-rp",GetXAttrValueEx("_User",mUser,"ОтделРП","")).
   RUN Insert_TTName("Pod-user-fio",     GetXAttrValueEx("_User",mUser,"ФИОП","")).
   RUN Insert_TTName("Pod-user-fioshort",getShortName(GetXAttrValueEx("_User",mUser,"ФИОП",""))).
   RUN Insert_TTName("Pod-user-fio-rp",  GetXAttrValueEx("_User",mUser,"User-NameRP","")).
   RUN Insert_TTName("Pod-user-dov-rp",  GetXAttrValueEx("_User",mUser,"ДокОснТипРП","")).
   RUN Insert_TTName("Pod-user-telefon", GetXAttrValueEx("_User",mUser,"Телефон","")).
   RUN Insert_TTName("Pod-user-fio3", getShortSur(GetXAttrValueEx("_User", mUser,"ФИОП",""))).
   FIND FIRST _user WHERE _user._userid = mUser NO-LOCK NO-ERROR.
      RUN Insert_TTName("Pod-user-f", _user._User-Name).
      RUN Insert_TTName("Pod-isp-fio",     GetXAttrValueEx("_User",User_,"ФИОП","")).
      RUN Insert_TTName("Pod-isp-fioshort",getShortName(GetXAttrValueEx("_User",User_,"ФИОП",""))).
      RUN Insert_TTName("Pod-isp-dolg",    GetXAttrValueEx("_User",User_,"Должность","")).
      RUN Insert_TTName("Pod-isp-otdel-rp",GetXAttrValueEx("_User",User_,"ОтделРП","")).
      RUN Insert_TTName("Pod-isp-dov-rp",  GetXAttrValueEx("_User",User_,"ДокОснТипРП","")).
      RUN Insert_TTName("Pod-isp-telefon", GetXAttrValueEx("_User",User_,"Телефон","")).
       RUN Insert_TTName("Pod-user-fio3", getShortSur(GetXAttrValueEx("_User",User_,"ФИОП",""))).

/* наименование и адрес банка */
FIND FIRST branch WHERE branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("go-bank-name",     branch.name). 
   RUN Insert_TTName("go-bank-address",  branch.Address). 
   RUN Insert_TTName("go-gorod", entry(2,branch.Address)). 
END.

/* реквизиты банка определяются по shFilial */
IF GetXAttrValue("user-proc", STRING(user-proc.public-number), "ExParam") EQ "РеквФилиал" THEN
    shfilial_ = shFilial.
ELSE shfilial_ = GetXAttrValueEx("_User", mUser, "filial-id", "").

podOtdel  = GetXAttrValueEx("_User", mUser, "Отделение", "").
IF podOtdel EQ "0518" THEN podOtdel = "0500".

RUN Insert_TTName("spb", IF CAN-DO("0101,0102,0106,0109", podOtdel) THEN "1" ELSE "").
RUN Insert_TTName("filial", shfilial_).

/*IF USERID("bisquit") = "i0400pda" THEN 
DO:
MESSAGE podOtdel VIEW-AS ALERT-BOX.
END.*/
/* по филиалу */
FIND FIRST branch WHERE branch.Branch-Id EQ shfilial_ NO-LOCK NO-ERROR.

IF AVAILABLE branch THEN
DO:
   RUN Insert_TTName("bank-name",    branch.name). 
   RUN Insert_TTName("bank-address", branch.Address). 
   RUN Insert_TTName("gorod", entry(2,branch.Address)). 
   RUN Insert_TTName("bank-inn",       GetXattrValue("branch", 
                                                   STRING(branch.Branch-Id), 
                                                   "ИНН")). 
   RUN Insert_TTName("bank-bik",       GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "БанкМФО")). 
   RUN Insert_TTName("bank-kpp",       GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "КПП")). 
   RUN Insert_TTName("bank-ks",        GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "КорСч")). 
   RUN Insert_TTName("bank-KS-GDE",    GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ПечИзвГдеОткр")). 
   RUN Insert_TTName("bank-ks-ksgde",  GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "КорСч") +
                                       " " + GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ПечИзвГдеОткр")). 
   RUN Insert_TTName("bank-tel",       GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "Телефон")). 
   RUN Insert_TTName("bank-ogrn",      GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ОГРН")). 
   RUN Insert_TTName("bank-addr-post", GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ПечАдрПочт")). 
   RUN Insert_TTName("bank-addr-kor",  GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "ПечАдрКор")). 
   RUN Insert_TTName("bank-dps_bank",  GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "dps_bank")).
   RUN Insert_TTName("bank-name-rp",  GetXattrValue("branch",
                                                   STRING(branch.Branch-Id),
                                                   "БанкНаимРП")). 
END.

/* по отделению подписанта */
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
/*-----------------*/