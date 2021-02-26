/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2012 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: 0215735.p
      Comment: 
      Created: 

*/

{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{exchange.equ}

&GLOBAL-DEFINE NO-BASE-PROC YES

def var f-name as char init "����-��⬮.txt" no-undo.
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

/* �롨ࠥ� 䠩� ������ */
{getfile.i &filename="f-name"}
if lastkey = keycode("esc") then return.
f-name = fname.
IF SEARCH(f-name) <> f-name then do:
  RUN Fill-SysMes IN h_tmess("","","-1","�� ������ 䠩� ��� ������").
  RETURN.
END.

vLogName = "OKATO-OKTMO-" + 
           STRING(YEAR(TODAY),"9999") + "_" + 
           STRING(MONTH(TODAY),"99") + "_" + 
           STRING(DAY(TODAY),"99") + "_" + 
           REPLACE(STRING(TIME,"HH:MM"),":","_") + ".csv".

{setdest.i &stream = " stream s-rep " &filename = "vLogName" &option= " CONVERT TARGET '1251' "}

/* �⠥� 䠩� ������ */
input stream s-imp from value(f-name) CONVERT SOURCE "1251".  

import stream s-imp unformatted vStr.   

IF TRIM(vStr) <> "�����;�����;������������ �������;" THEN DO:
  RUN Fill-SysMes IN h_tmess("","","-1","�����४�� ��������� 䠩��").
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

vIsIndexed = IsXattrIndexed("cust-corp","�����-�����").

MAIN:
DO TRANSACTION ON ERROR  UNDO MAIN, RETRY MAIN
               ON ENDKEY UNDO MAIN, RETRY MAIN
               ON STOP   UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}
_CUST:
FOR EACH signs WHERE
         signs.file-name EQ "cust-corp"
     AND signs.code      EQ "�����-�����"
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

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(cust-corp.cust-id ) "'"  '";' '"' cust-corp.name-corp '";"��� ����� �� ������.";;;'  SKIP.
      NEXT _CUST.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "��� ��ꥪ�: �ਤ��᪮� ���~n" + 
          "��� ��ꥪ�: " + STRING(cust-corp.cust-id) + "~n" + 
          "������������: " + cust-corp.name-corp + "~n" + 
          "�����: " + vOKATO + "~n" + 
          "����: " + cust-corp.addr-of-low[1] + "|" + 
          vChooseList + ",�⬥��").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(cust-corp.cust-id ) "'"  '";' '"' cust-corp.name-corp '";"��� ����� ��⠢��� ��� ���������.";;;'  SKIP.
      NEXT _CUST.
    END.
    ELSE DO:

       IF UpdateSigns("cust-corp",
                      STRING(cust-corp.cust-id),
                      "�����-�����",
                      vList,
                      ?) THEN
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(cust-corp.cust-id ) "'"  '";' '"' cust-corp.name-corp '";"��� ����� ������� �� ��� �����.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

vIsIndexed = IsXattrIndexed("person","�����-�����").
MAIN1:
DO TRANSACTION ON ERROR  UNDO MAIN1, RETRY MAIN1
               ON ENDKEY UNDO MAIN1, RETRY MAIN1
               ON STOP   UNDO MAIN1, RETRY MAIN1:
_PERS:
FOR EACH signs WHERE
         signs.file-name EQ "person"
     AND signs.code      EQ "�����-�����"
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

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(person.person-id) "'"  '";' '"' person.name-last + " " + person.first-names '";"��� ����� �� ������.";;;'  SKIP.
      NEXT _PERS.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "��� ��ꥪ�: 䨧��᪮� ���~n" + 
          "��� ��ꥪ�: " + STRING(person.person-id) + "~n" + 
          "������������: " + person.name-last + " " + person.first-names + "~n" + 
          "�����: " + vOKATO + "~n" + 
          "����: " + person.address[1] + "|" + 
          vChooseList + ",�⬥��").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(person.person-id) "'"  '";' '"' person.name-last + " " + person.first-names '";"��� ����� ��⠢��� ��� ���������.";;;'  SKIP.
      NEXT _PERS.
    END.
    ELSE DO:
     
       IF UpdateSigns("person",
                      STRING(person.person-id),
                      "�����-�����",
                      vList,
                      ?) THEN 
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(person.person-id) "'"  '";' '"' person.name-last + " " + person.first-names '";"��� ����� ������� �� ��� �����.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

vIsIndexed = IsXattrIndexed("banks","�����-�����").
MAIN2:
DO TRANSACTION ON ERROR  UNDO MAIN2, RETRY MAIN2
               ON ENDKEY UNDO MAIN2, RETRY MAIN2
               ON STOP   UNDO MAIN2, RETRY MAIN2:
_BANK:
FOR EACH signs WHERE
         signs.file-name EQ "banks"
     AND signs.code      EQ "�����-�����"
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

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(banks.bank-id) "'"  '";' '"' banks.name '";"��� ����� �� ������.";;;'  SKIP.
      NEXT _BANK.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "��� ��ꥪ�: ����~n" + 
          "��� ��ꥪ�: " + STRING(banks.bank-id) + "~n" + 
          "������������: " + banks.name + "~n" + 
          "�����: " + vOKATO + "~n" + 
          "����: " + banks.law-address + "|" + 
          vChooseList + ",�⬥��").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(banks.bank-id) "'"  '";' '"' banks.name '";"��� ����� ��⠢��� ��� ���������.";;;'  SKIP.
      NEXT _BANK.
    END.
    ELSE DO:
     
       IF UpdateSigns("banks",
                      STRING(banks.bank-id),
                      "�����-�����",
                      vList,
                      ?) THEN 
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(banks.bank-id) "'"  '";' '"' banks.name '";"��� ����� ������� �� ��� �����.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

vIsIndexed = IsXattrIndexed("branch","�����-�����").
MAIN3:
DO TRANSACTION ON ERROR  UNDO MAIN3, RETRY MAIN3
               ON ENDKEY UNDO MAIN3, RETRY MAIN3
               ON STOP   UNDO MAIN3, RETRY MAIN3:
_BR:
FOR EACH signs WHERE
         signs.file-name EQ "branch"
     AND signs.code      EQ "�����-�����"
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

      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(branch.branch-id) "'"  '";' '"' branch.name '";"��� ����� �� ������.";;;'  SKIP.
      NEXT _BR.
   END.
   ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
      pick-value = "".
      vChooseList = TRIM(vChooseList,",").
      RUN Fill-SysMes IN h_tmess("","","3",
          "��� ��ꥪ�: ������୮� ���ࠧ�������~n" + 
          "��� ��ꥪ�: " + STRING(branch.branch-id) + "~n" + 
          "������������: " + branch.name + "~n" + 
          "�����: " + vOKATO + "~n" + 
          "����: " + branch.address + "|" + 
          vChooseList + ",�⬥��").
       IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
          vList = "".
       ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
    END.
    IF NOT {assigned vList} THEN DO:
      PUT STREAM s-rep UNFORMATTED '"' "'" STRING(branch.bank-id) "'"  '";' '"' branch.name '";"��� ����� ��⠢��� ��� ���������.";;;'  SKIP.
      NEXT _BR.
    END.
    ELSE DO:
     
       IF UpdateSigns("branch",
                      STRING(branch.branch-id),
                      "�����-�����",
                      vList,
                      ?) THEN 
          PUT STREAM s-rep UNFORMATTED '"' "'" STRING(branch.branch-id) "'"  '";' '"' branch.name '";"��� ����� ������� �� ��� �����.";"' + vOKATO + '";"' + vList + '";'  SKIP.
    END.
END.
END.

MAIN4:
DO TRANSACTION ON ERROR  UNDO MAIN4, RETRY MAIN4
               ON ENDKEY UNDO MAIN4, RETRY MAIN4
               ON STOP   UNDO MAIN4, RETRY MAIN4:
_SET:
FOR EACH setting WHERE
         setting.code EQ "���"
     AND setting.sub-code EQ "�����_������"
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

         PUT STREAM s-rep UNFORMATTED '�� 㤠���� ᪮�����஢��� ���祭�� ����஥筮�� ��ࠬ��� ��� > �����_������ ' + vOKATO SKIP.
         vSetVal = vSetVal + "," + vOKATO.
         NEXT _VAL.
      END.
      ELSE IF NUM-ENTRIES(vList) > 1 THEN DO:
         pick-value = "".
         vChooseList = TRIM(vChooseList,",").
         RUN Fill-SysMes IN h_tmess("","","3",
             "��� ��ꥪ�: �� �����_������~n" + 
             "�����: " + vOKATO + "~n" + "|" + 
             vChooseList + ",�⬥��").
          IF pick-value = STRING(NUM-ENTRIES(vChooseList) + 1) OR pick-value EQ "" OR pick-value EQ "0" THEN
             vList = "".
          ELSE vList = ENTRY(INT64(pick-value),vList) NO-ERROR.
       END.
       IF NOT {assigned vList} THEN DO:
          PUT STREAM s-rep UNFORMATTED '�� 㤠���� ᪮�����஢��� ����஥�� ��ࠬ��� ��� > �����_������ ' + setting.val SKIP.
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
          PUT STREAM s-rep UNFORMATTED '����஥�� ��ࠬ��� ��� > �����_������ ᪮�����஢�� ' + bsetting.val SKIP.
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
