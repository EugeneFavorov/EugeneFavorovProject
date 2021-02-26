/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ЗАО "Банковские информационные системы"
     Filename: 0215735.p
      Comment: 
      Created: 

*/

{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{exchange.equ}

&GLOBAL-DEFINE NO-BASE-PROC YES

def var f-name as char init "окато-октмо.txt" no-undo.
DEF VAR vStr       AS CHARACTER NO-UNDO.
DEF VAR vLogName   AS CHARACTER NO-UNDO.
DEF VAR vIsIndexed AS LOGICAL   NO-UNDO.
DEF VAR vOKATO     AS CHARACTER NO-UNDO.
DEF VAR vList      AS CHARACTER NO-UNDO.
DEF VAR vChooseList AS CHARACTER NO-UNDO.
DEF VAR vSetting    AS CHARACTER NO-UNDO.
DEF VAR vI          AS INT64     NO-UNDO.
DEF VAR vVal        AS CHARACTER NO-UNDO.
DEF VAR vSetVal     AS CHARACTER NO-UNDO.
DEF VAR vMptr       AS MEMPTR    NO-UNDO.

DEFINE BUFFER bSetting FOR setting.

def stream s-imp.
def stream s-rep.

DEFINE TEMP-TABLE ttConv NO-UNDO
  FIELD okato AS CHARACTER
  FIELD oktmo AS CHARACTER
  FIELD fName AS CHARACTER
  INDEX okato okato
.

/* выбираем файл импорта */
{getfile.i &filename="f-name"}
if lastkey = keycode("esc") then return.
f-name = fname.
IF SEARCH(f-name) <> f-name then do:
  RUN Fill-SysMes IN h_tmess("","","-1","Не найден файл для импорта").
  RETURN.
END.

vLogName = "OKATO-OKTMO-" + 
           STRING(YEAR(TODAY),"9999") + "_" + 
           STRING(MONTH(TODAY),"99") + "_" + 
           STRING(DAY(TODAY),"99") + "_" + 
           REPLACE(STRING(TIME,"HH:MM"),":","_") + ".csv".

{setdest.i &stream = " stream s-rep " &filename = "vLogName" &option= " CONVERT TARGET '1251' "}

/* читаем файл импорта */
input stream s-imp from value(f-name) CONVERT SOURCE "1251".  

import stream s-imp unformatted vStr.   

IF TRIM(vStr) <> "ОКАТО;ОКТМО;НАИМЕНОВАНИЕ ОБЪЕКТА;" THEN DO:
  RUN Fill-SysMes IN h_tmess("","","-1","Некорректный заголовок файла").
  RETURN.
END.

i-str:
REPEAT:
  IMPORT STREAM s-imp UNFORMATTED vStr.   
  IF NUM-ENTRIES(vStr, ";") <> 4 THEN NEXT i-str.

   CREATE ttConv NO-ERROR.
   ASSIGN
      ttConv.okato = ENTRY(1,vStr,";")
      ttConv.oktmo = ENTRY(2,vStr,";")
      ttConv.fName = ENTRY(3,vStr,";")
   NO-ERROR.
END.

vIsIndexed = IsXattrIndexed("cust-corp","ОКАТО-НАЛОГ").

MAIN:
DO TRANSACTION ON ERROR  UNDO MAIN, RETRY MAIN
               ON ENDKEY UNDO MAIN, RETRY MAIN
               ON STOP   UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
_CUST:
FOR EACH signs WHERE
         signs.file-name EQ "cust-corp"
     AND signs.code      EQ "ОКАТО-НАЛОГ"
         NO-LOCK,
   FIRST cust-corp WHERE
         cust-corp.cust-id EQ INT64(signs.surrogate)
         NO-LOCK:
   vOKATO = IF vIsIndexed THEN signs.code-value ELSE signs.xattr-value.

   ASSIGN
      vList       = ""
      vChooseList = "".
   FOR EACH ttConv WHERE
            ttConv.okato EQ vOKATO
            NO-LOCK:
      {additem.i vList ttConv.oktmo}
      vChooseList = vChooseList + "," + STRING(ttConv.oktmo,"x(15)") + " " + ttConv.fName.
   END.
   IF NOT {assigned vList} THEN DO:

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(cust-corp.cust-id ) "'"  '";' '"' cust-corp.name-corp '";"Код ОКАТО не найден.";;;'  SKIP.
      NEXT _CUST.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "Тип субъекта: юридическое лицо~n" + 
          "Код субъекта: " + STRING(cust-corp.cust-id) + "~n" + 
          "Наименование: " + cust-corp.name-corp + "~n" + 
          "ОКАТО: " + vOKATO + "~n" + 
          "Адрес: " + cust-corp.addr-of-low[1] + "|" + 
          vChooseList + ",Отмена").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(cust-corp.cust-id ) "'"  '";' '"' cust-corp.name-corp '";"Код ОКАТО оставлен без изменений.";;;'  SKIP.
      NEXT _CUST.
    END.
    ELSE DO:

       IF UpdateSigns("cust-corp",
                      STRING(cust-corp.cust-id),
                      "ОКАТО-НАЛОГ",
                      vList,
                      ?) THEN
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(cust-corp.cust-id ) "'"  '";' '"' cust-corp.name-corp '";"Код ОКАТО заменен на код ОКТМО.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

vIsIndexed = IsXattrIndexed("person","ОКАТО-НАЛОГ").
MAIN1:
DO TRANSACTION ON ERROR  UNDO MAIN1, RETRY MAIN1
               ON ENDKEY UNDO MAIN1, RETRY MAIN1
               ON STOP   UNDO MAIN1, RETRY MAIN1:
_PERS:
FOR EACH signs WHERE
         signs.file-name EQ "person"
     AND signs.code      EQ "ОКАТО-НАЛОГ"
         NO-LOCK,
   FIRST person WHERE
         person.person-id EQ INT64(signs.surrogate)
         NO-LOCK:
   vOKATO = IF vIsIndexed THEN signs.code-value ELSE signs.xattr-value.

   ASSIGN
      vList       = ""
      vChooseList = "".
   FOR EACH ttConv WHERE
            ttConv.okato EQ vOKATO
            NO-LOCK:
      {additem.i vList ttConv.oktmo}
      vChooseList = vChooseList + "," + STRING(ttConv.oktmo,"x(15)") + " " + ttConv.fName.
   END.
   IF NOT {assigned vList} THEN DO:

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(person.person-id) "'"  '";' '"' person.name-last + " " + person.first-names '";"Код ОКАТО не найден.";;;'  SKIP.
      NEXT _PERS.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "Тип субъекта: физическое лицо~n" + 
          "Код субъекта: " + STRING(person.person-id) + "~n" + 
          "Наименование: " + person.name-last + " " + person.first-names + "~n" + 
          "ОКАТО: " + vOKATO + "~n" + 
          "Адрес: " + person.address[1] + "|" + 
          vChooseList + ",Отмена").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(person.person-id) "'"  '";' '"' person.name-last + " " + person.first-names '";"Код ОКАТО оставлен без изменений.";;;'  SKIP.
      NEXT _PERS.
    END.
    ELSE DO:
     
       IF UpdateSigns("person",
                      STRING(person.person-id),
                      "ОКАТО-НАЛОГ",
                      vList,
                      ?) THEN 
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(person.person-id) "'"  '";' '"' person.name-last + " " + person.first-names '";"Код ОКАТО заменен на код ОКТМО.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

vIsIndexed = IsXattrIndexed("banks","ОКАТО-НАЛОГ").
MAIN2:
DO TRANSACTION ON ERROR  UNDO MAIN2, RETRY MAIN2
               ON ENDKEY UNDO MAIN2, RETRY MAIN2
               ON STOP   UNDO MAIN2, RETRY MAIN2:
_BANK:
FOR EACH signs WHERE
         signs.file-name EQ "banks"
     AND signs.code      EQ "ОКАТО-НАЛОГ"
         NO-LOCK,
   FIRST banks WHERE
         banks.bank-id EQ INT64(signs.surrogate)
         NO-LOCK:
   vOKATO = IF vIsIndexed THEN signs.code-value ELSE signs.xattr-value.

   ASSIGN
      vList       = ""
      vChooseList = "".
   FOR EACH ttConv WHERE
            ttConv.okato EQ vOKATO
            NO-LOCK:
      {additem.i vList ttConv.oktmo}
      vChooseList = vChooseList + "," + STRING(ttConv.oktmo,"x(15)") + " " + ttConv.fName.
   END.
   IF NOT {assigned vList} THEN DO:

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(banks.bank-id) "'"  '";' '"' banks.name '";"Код ОКАТО не найден.";;;'  SKIP.
      NEXT _BANK.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "Тип субъекта: банк~n" + 
          "Код субъекта: " + STRING(banks.bank-id) + "~n" + 
          "Наименование: " + banks.name + "~n" + 
          "ОКАТО: " + vOKATO + "~n" + 
          "Адрес: " + banks.law-address + "|" + 
          vChooseList + ",Отмена").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(banks.bank-id) "'"  '";' '"' banks.name '";"Код ОКАТО оставлен без изменений.";;;'  SKIP.
      NEXT _BANK.
    END.
    ELSE DO:
     
       IF UpdateSigns("banks",
                      STRING(banks.bank-id),
                      "ОКАТО-НАЛОГ",
                      vList,
                      ?) THEN 
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(banks.bank-id) "'"  '";' '"' banks.name '";"Код ОКАТО заменен на код ОКТМО.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

vIsIndexed = IsXattrIndexed("branch","ОКАТО-НАЛОГ").
MAIN3:
DO TRANSACTION ON ERROR  UNDO MAIN3, RETRY MAIN3
               ON ENDKEY UNDO MAIN3, RETRY MAIN3
               ON STOP   UNDO MAIN3, RETRY MAIN3:
_BR:
FOR EACH signs WHERE
         signs.file-name EQ "branch"
     AND signs.code      EQ "ОКАТО-НАЛОГ"
         NO-LOCK,
   FIRST branch WHERE
         branch.branch-id EQ signs.surrogate
         NO-LOCK:
   vOKATO = IF vIsIndexed THEN signs.code-value ELSE signs.xattr-value.

   ASSIGN
      vList       = ""
      vChooseList = "".
   FOR EACH ttConv WHERE
            ttConv.okato EQ vOKATO
            NO-LOCK:
      {additem.i vList ttConv.oktmo}
      vChooseList = vChooseList + "," + STRING(ttConv.oktmo,"x(15)") + " " + ttConv.fName.
   END.
   IF NOT {assigned vList} THEN DO:

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(branch.branch-id) "'"  '";' '"' branch.name '";"Код ОКАТО не найден.";;;'  SKIP.
      NEXT _BR.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "Тип субъекта: структурное подразделение~n" + 
          "Код субъекта: " + STRING(branch.branch-id) + "~n" + 
          "Наименование: " + branch.name + "~n" + 
          "ОКАТО: " + vOKATO + "~n" + 
          "Адрес: " + branch.address + "|" + 
          vChooseList + ",Отмена").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(branch.bank-id) "'"  '";' '"' branch.name '";"Код ОКАТО оставлен без изменений.";;;'  SKIP.
      NEXT _BR.
    END.
    ELSE DO:
     
       IF UpdateSigns("branch",
                      STRING(branch.branch-id),
                      "ОКАТО-НАЛОГ",
                      vList,
                      ?) THEN 
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(branch.branch-id) "'"  '";' '"' branch.name '";"Код ОКАТО заменен на код ОКТМО.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

MAIN4:
DO TRANSACTION ON ERROR  UNDO MAIN4, RETRY MAIN4
               ON ENDKEY UNDO MAIN4, RETRY MAIN4
               ON STOP   UNDO MAIN4, RETRY MAIN4:
_SET:
FOR EACH setting WHERE
         setting.code EQ "ГНИ"
     AND setting.sub-code EQ "ОКАТО_НалИнсп"
         NO-LOCK:
  vSetVal     = "".

  _VAL:
  DO vI = 1 TO NUM-ENTRIES(setting.val):

     vOKATO =  ENTRY(vI,setting.val).

     ASSIGN  
        vList       = ""
        vChooseList = "".
     FOR EACH ttConv WHERE
              ttConv.okato EQ vOKATO
              NO-LOCK:
         {additem.i vList ttConv.oktmo}
         vChooseList = vChooseList + "," + STRING(ttConv.oktmo,"x(15)") + " " + ttConv.fName.
      END.
      IF NOT {assigned vList} THEN DO:

         PUT STREAM s-rep UNFORMATTED 'Не удалось сконвертировать значение настроечного параметра ГНИ > ОКАТО_НалИнсп ' + vOKATO SKIP.
         vSetVal = vSetVal + "," + vOKATO.
         NEXT _VAL.
      END.
      ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
         pick-value = "".
         vChooseList = TRIM(vChooseList,",").
         RUN Fill-SysMes IN h_tmess("","","3",
             "Тип субъекта: НП ОКАТО_НалИнсп~n" + 
             "ОКАТО: " + vOKATO + "~n" + "|" + 
             vChooseList + ",Отмена").
          IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
             vList = "".
          ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
       END.
       IF NOT {assigned vList} THEN DO:
          PUT STREAM s-rep UNFORMATTED 'Не удалось сконвертировать настроечный параметр ГНИ > ОКАТО_НалИнсп ' + setting.val SKIP.
          vSetVal = vSetVal + "," + vOKATO.
          NEXT _VAL.
       END.
       ELSE DO:
          vSetVal = vSetVal + "," + vList.
       END.
   END.

   vSetVal = TRIM(vSetVal,",").
   IF {assigned vSetVal} AND vSetVal <> setting.val THEN DO:
      FIND FIRST bSetting WHERE
           RECID(bSetting) EQ RECID(setting)
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL(bsetting) THEN DO:
         bSetting.val = vSetVal NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN 
          PUT STREAM s-rep UNFORMATTED 'Настроечный параметр ГНИ > ОКАТО_НалИнсп сконвертирован ' + bsetting.val SKIP.
   END.
  END.
END.
END.
INPUT  STREAM s-imp CLOSE.
OUTPUT STREAM s-rep CLOSE. 

COPY-LOB FILE vLogName TO vmptr CONVERT SOURCE CODEPAGE "1251" TARGET CODEPAGE "ibm866" NO-ERROR. 
COPY-LOB vMptr TO FILE "_spool.tmp" NO-ERROR.

{preview.i}

{intrface.del}
RETURN.
