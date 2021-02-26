
/* +++ crtagval.i was humbly modified by (c)blodd converter v.1.11 on 6/9/2017 12:05pm +++ */

/*
               KSV Editor
    Copyright: (C) 2000-2005 Serguey Klimoff (bulklodd)
     Filename: CRTAGVAL.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 18.04.2008 12:11 fEAk    
     Modified: 18.04.2008 12:11 fEAk     <comment>
*/
{intrface.get xclass}

/* Список функций, которые будут обрабатываются данной процедурой */                                                       
vOtherTags = "docnumber,docvid,birthdate,face,sotr_fio,numdover,datadover,postemp,postfio,дата_дог," + 
             "номер_прикр(loan-dps-dv)," + 
             "Параметр@Банк,Параметр@Адрес_юр,Параметр@ИНН,Параметр@БанкМФО,Параметр@КорСч," +
             "dolgIP,dolgRP,bankID,cityName,nameGB,adrGb,adrkor,nameFil,adrFil,innkpp,bik,korsch,ogrn,telfax,nameOO,adrOO,acctper,dateper".

/* Support functions */

PROCEDURE Run-PrnProc.
    DEFINE INPUT  PARAMETER iProc   AS CHARACTER.
    DEFINE INPUT  PARAMETER iDate1  AS DATE.
    DEFINE INPUT  PARAMETER iDate2  AS DATE.
    DEFINE INPUT  PARAMETER iStrPar AS CHARACTER.
    DEFINE OUTPUT PARAMETER oResult AS CHARACTER.

    RUN VALUE(LC(iProc) + ".p") (OUTPUT oResult, iDate1, iDate2, iStrPar).
    IF {assigned RETURN-VALUE} THEN
        oResult = {&RETURN_VALUE}.
    ELSE
        ASSIGN
            oResult   = printtext
            printtext = ""
        .
    ASSIGN
        oResult = FILL("_", 20) WHEN iRet AND NOT {assigned oResult}
        oResult = TRIM(oResult)
    .
END PROCEDURE.

FUNCTION date2text RETURNS CHAR (INPUT iDate AS DATE, INPUT iAdd AS LOGICAL):
   DEF VAR mont_h  AS CHAR
INIT "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря" NO-UNDO.

   IF iAdd THEN
      RETURN '"' + STRING(DAY(iDate)) + '" ' + 
                 ENTRY(MONTH(iDate), mont_h) +
                 STRING(YEAR(iDate), " 9999 г.").
   ELSE
      RETURN STRING(DAY(iDate)) + " " + 
                 ENTRY(MONTH(iDate), mont_h) +
                 STRING(YEAR(iDate), " 9999 г.").

END FUNCTION.

/* Main procedures */

PROCEDURE docnumber:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".

   FIND FIRST person WHERE person.person EQ loan.cust-id NO-LOCK NO-ERROR.             
   IF NOT AVAIL person THEN
      RETURN "Err".

   ASSIGN oRes = person.document.
                
END PROCEDURE.

PROCEDURE acctper:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".
   REStmp = ENTRY(1,GETXATTRVALUEEX('loan',loan.contract + ',' + loan.cont-code,'acct_trans',' '),' ').


   ASSIGN oRes = REStmp.
                
END PROCEDURE.

PROCEDURE dateper:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".
   REStmp = date2text(DATE(ENTRY(1,GETXATTRVALUEEX('loan',loan.contract + ',' + loan.cont-code,'date_trans',' '),' ')),NO).

   ASSIGN oRes = REStmp.
                
END PROCEDURE.

PROCEDURE docvid:                                              
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".

   FIND FIRST person WHERE person.person EQ loan.cust-id NO-LOCK NO-ERROR.             
   IF NOT AVAIL person THEN
      RETURN "Err".
   ASSIGN 
    oRes = GetCodeNameEx("КодДокум", person.document-id, person.document-id)
   .
                
END PROCEDURE.

PROCEDURE birthdate:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".

   FIND FIRST person WHERE person.person EQ loan.cust-id NO-LOCK NO-ERROR.             
   IF NOT AVAIL person THEN
      RETURN "Err".

   ASSIGN oRes = date2text(person.birthday, NO).
                
END PROCEDURE.

PROCEDURE face:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   ASSIGN oRes = FGetSetting("ДолжнРук",?,?) + " " + FGetSetting("ФИОРук",?,?).             
                
END PROCEDURE.

PROCEDURE sotr_fio:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.


   FIND FIRST cust-role WHERE cust-role.file-name  EQ "_user"
                                AND cust-role.Class-Code EQ "Пользователь"
                                AND cust-role.surrogate  EQ USERID('bisquit')
         NO-LOCK NO-ERROR.
   ASSIGN oRes = ( IF AVAIL cust-role THEN cust-role.cust-name ELSE "").
   IF NOT AVAIL cust-role THEN DO:
     RUN Fill-AlertSysMes IN h_tmess("","",1,"У текущего пользователя не настроена привязка к физ.лицу.").

     FIND first _user where _user._userid eq user('bisquit') no-lock no-error.
     IF avail _user then
	oRes = _user._user-name.
   END.

END PROCEDURE.

PROCEDURE numdover:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'Отделение','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   REStmp = GETXATTRVALUEEX('_user',nachOperu,'ДокОснНомер','не определено').
  
   if GetXAttrValue('_user',USERID('bisquit'),'ДокОснНомер') = '' THEN
   oRes = REStmp.
   ELSE
   oRes = GetXAttrValue('_user',USERID('bisquit'),'ДокОснНомер').
                
END PROCEDURE.

PROCEDURE datadover:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'Отделение','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   REStmp = GETXATTRVALUEEX('_user',nachOperu,'ДокОснДата','не определено').
  
   if GetXAttrValue('_user',USERID('bisquit'),'ДокОснДата') = '' THEN
   oRes = REStmp.
   ELSE
   oRes = GetXAttrValue('_user',USERID('bisquit'),'ДокОснДата').
                
END PROCEDURE.

PROCEDURE postemp:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   DEF VAR REStmp1 AS CHARACTER NO-UNDO.
   DEF VAR REStmp2 AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'Отделение','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   REStmp = GETXATTRVALUEEX('_user',nachOperu,'ДолжностьРП','не определено').
   REStmp1 = GETXATTRVALUEEX('_user',user('bisquit'),'ДокОснДата','').
   REStmp2 = GETXATTRVALUEEX('_user',user('bisquit'),'ДокОснНомер','').

   if REStmp1 = '' OR REStmp2 = '' THEN
   oRes = REStmp.
   ELSE
   oRes = GetXAttrValue('_user',USERID('bisquit'),'ДолжностьРП').
                
END PROCEDURE.

PROCEDURE postfio:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   DEF VAR REStmp1 AS CHARACTER NO-UNDO.
   DEF VAR REStmp2 AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'Отделение','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   find first _user where _user._userid eq user('bisquit') no-lock no-error.
	if avail _user then
		REStmp = _user._user-name.	
   REStmp1 = GETXATTRVALUEEX('_user',user('bisquit'),'ДокОснДата','').
   REStmp2 = GETXATTRVALUEEX('_user',user('bisquit'),'ДокОснНомер','').

   if REStmp1 = '' OR REStmp2 = '' THEN DO:
   find first _user where _user._userid eq NACHOPERU no-lock no-error.
	if avail _user then
		oRes = _user._user-name.

	END.                
	ELSE 
   oRes = REStmp.


END PROCEDURE.

PROCEDURE дата_дог:
   DEF INPUT PARAM iRid  AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   ASSIGN oRes = Date2Text(iDate, YES).             
                
END PROCEDURE.

/*должность пользователя в ИП*/
PROCEDURE dolgIP:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  
    oRes = GetXAttrValueEx("_user",USERID("bisquit"),"Должность","").
    
END PROCEDURE.

/*должность пользователя в РП*/
PROCEDURE dolgRP:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  
    oRes = GetXAttrValueEx("_user",USERID("bisquit"),"ДолжностьРП","").
    
END PROCEDURE.

PROCEDURE bankID:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
        oRes = GetXAttrValueEx("branch",
                               STRING(branch.branch-id),
                               "InternalID",
                               "").
        ELSE oRes = GetXAttrValueEx("branch",
                                    STRING(branch.parent-id),
                                    "InternalID",
                                    "").
      END.
    ELSE
      LEAVE.
  END.             
END PROCEDURE.

PROCEDURE cityName:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.
  DEF VAR tmpVar        AS CHARACTER NO-UNDO.
  DEF VAR tmp           AS CHARACTER NO-UNDO.
  DEF VAR i             AS INTEGER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        tmpVar = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "ПечАдрПочт",
                                 "").
        DO i = 1 TO NUM-ENTRIES(tmpVar):
          IF TRIM(ENTRY(i, tmpVar)) BEGINS "г." THEN  /*ищем элемент списка содержащий "г."*/
          DO:
            tmp = ENTRY(i, tmpVar).
          END.
        END.
        oRes = LEFT-TRIM(tmp, "г. ").
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*Имя ГБ*/
PROCEDURE nameGB:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   oRes = GetXAttrValueEx("branch",
                          "0000",
                          "dps_bank",
                          "").
END PROCEDURE.

/*Адрес ГБ*/
PROCEDURE adrGB:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   oRes = 'Место нахождения: ' + GetXAttrValueEx("branch",
                                                 "0000",
                                                 "ПечАдрПочт",
                                                 "").
END PROCEDURE.

/*Адрес направления корреспонденции*/
PROCEDURE adrkor:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

    oRes = 'Адрес направления корреспонденции: ' + GetXAttrValueEx("branch",
                                                                  "0500",
                                                                  "ПечАдрКор",
                                                                  "").
END PROCEDURE.

/*Наименование филиала*/
PROCEDURE nameFil:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("11", branch.branch-type) THEN
          oRes = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "dps_bank",
                                 "").
        ELSE oRes = GetXAttrValueEx("branch",
                                    STRING(branch.parent-id),
                                    "dps_bank",
                                    "").
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*Адрс филиала*/
PROCEDURE adrFil:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("11", branch.branch-type) THEN
        oRes = 'Место нахождения: ' + GetXAttrValueEx("branch",
                                                      STRING(branch.branch-id),
                                                      "ПечАдрПочт",
                                                      "").
        ELSE oRes = 'Место нахождения: ' + GetXAttrValueEx("branch",
                                                           STRING(branch.parent-id),
                                                           "ПечАдрПочт",
                                                           "").
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*ИНН и КПП*/
PROCEDURE innkpp:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
          DO:
            oRes = 'ИНН/КПП ' + GetXAttrValueEx("branch",
                                                STRING(branch.branch-id),
                                                "ИНН",
                                                "").
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                STRING(branch.branch-id),
                                                "КПП",
                                                "").
          END.   
        ELSE 
          DO:
            oRes = 'ИНН/КПП ' + GetXAttrValueEx("branch",
                                                STRING(branch.parent-id),
                                                "ИНН",
                                                "").
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                STRING(branch.parent-id),
                                                "КПП",
                                                "").
          END.        
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*БИК*/
PROCEDURE bik:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
        oRes = 'БИК ' + GetXAttrValueEx("branch",
                                        STRING(branch.branch-id),
                                        "БанкМФО",
                                        "").
        ELSE oRes = 'БИК ' + GetXAttrValueEx("branch",
                                             STRING(branch.parent-id),
                                             "БанкМФО",
                                             "").
      END.
    ELSE
      LEAVE.
  END.             
END PROCEDURE.

/*корсчет + где открыт корсчет*/
PROCEDURE korsch:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
          DO:  
            oRes = 'к/с № ' + GetXAttrValueEx("branch",
                                              STRING(branch.branch-id),
                                              "КорСч",
                                              "").
            oRes = oRes + ' ' + GetXAttrValueEx("branch",
                                                STRING(branch.branch-id),
                                                "ПечИзвГдеОткр",
                                                "").
          END. 
        ELSE 
          DO:
            oRes = 'к/с № ' + GetXAttrValueEx("branch",
                                              STRING(branch.parent-id),
                                              "КорСч",
                                              "").
            oRes = oRes + ' ' + GetXAttrValueEx("branch",
                                                STRING(branch.parent-id),
                                                "ПечИзвГдеОткр",
                                                "").
          END.
      END.  
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*ОГРН для ГБ*/
PROCEDURE ogrn:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10", branch.branch-type) THEN
        oRes = 'ОГРН ' + GetXAttrValueEx("branch",
                                         STRING(branch.branch-id),
                                         "ОГРН",
                                         "").
      END.
    ELSE
      LEAVE.
  END.             
END PROCEDURE.

/*телефон и факс*/
PROCEDURE telfax:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
          DO:
            IF GetXAttrValueEx("branch", STRING(branch.branch-id), "Телефон", "") NE "" THEN
            oRes = 'Тел/факс ' + GetXAttrValueEx("branch",
                                                 STRING(branch.branch-id),
                                                 "Телефон",
                                                 "").
            IF GetXAttrValueEx("branch", STRING(branch.branch-id), "Факс", "") NE "" THEN
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                 STRING(branch.branch-id),
                                                 "Факс",
                                                 "").
          END.   
        ELSE 
          DO:
            IF GetXAttrValueEx("branch", STRING(branch.parent-id), "Телефон", "") NE "" THEN
            oRes = 'Тел/факс ' + GetXAttrValueEx("branch",
                                                 STRING(branch.parent-id),
                                                 "Телефон",
                                                 "").
            IF GetXAttrValueEx("branch", STRING(branch.parent-id), "Факс", "") NE "" THEN
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                 STRING(branch.parent-id),
                                                 "Факс",
                                                 "").
          END.
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*Наименование операционного офиса*/
PROCEDURE nameOO:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("23", branch.branch-type) THEN
          oRes = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "dps_bank",
                                 ""). 
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*Адрес опер.офиса*/
PROCEDURE adrOO:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
    NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("23", branch.branch-type) THEN
          oRes = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "ПечАдрПочт",
                                 "").
          oRes = IF oRes EQ "0518" THEN "0500" ELSE "".
        IF oRes NE "" THEN
          oRes = "Место нахождения: " + oRes.
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*PROCEDURE <имя>:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   <тело>             
                
END PROCEDURE.*/

/* --- crtagval.i was humbly modified by (c)blodd converter v.1.11 on 6/9/2017 12:05pm --- */
