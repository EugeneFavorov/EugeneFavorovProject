{intrface.get tmess}
{globals.i}

&scope scriptforcount /home2/bis/quit41d/src-auto/k14schet.sh
&scope scriptforkill /home2/bis/quit41d/src-auto/killllll.sh

RUN Init-SysMes IN h_tmess ("","","").

DEFINE INPUT PARAMETER iparam AS CHARACTER NO-UNDO.

DEFINE VARIABLE mdelim    AS INT64 NO-UNDO.
DEFINE VARIABLE mstart    AS INT64 NO-UNDO.
DEFINE VARIABLE mend      AS INT64 NO-UNDO.
DEFINE VARIABLE mrun      AS INT64 NO-UNDO. 
DEFINE VARIABLE mkillSend AS INT64 NO-UNDO. 
DEFINE VARIABLE mStr      AS CHAR  NO-UNDO. 

IF NUM-ENTRIES(iparam) LT  4 THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "-1","������ ���� 4 ��ࠬ���!").
   RETURN.
END.
ASSIGN
   mstart = int64(ENTRY(1,iparam))
   mend   = int64(ENTRY(2,iparam))
   mdelim = int64(ENTRY(3,iparam))
NO-ERROr.
IF ERROR-STATUS:ERROR THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "-1","�訡�� ��ࠬ��஢ ��⮪��!").
   RETURN.
END.
IF NUM-ENTRIES(ENTRY(4,iparam)," ") LT 2 THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "-1","�� 㪠��� �����䨪��� ����᪠!").
   RETURN.
END.


pick-value = "NO".
RUN Fill-SysMes IN h_tmess ("", "", "4",SUBSTITUTE("����� �࠭���権 � ��᪮�쪮 ��⮪�� (&1). ���७�?",mdelim)).

IF pick-value NE "YES" THEN
  RETURN.


FIND LAST  code WHERE 
           code.class  EQ "��⮪������" 
       AND code.parent EQ "��⮪������" 
       AND code.code   EQ STRING(TODAY)  + "," + TRIM(ENTRY(1,ENTRY(4,iparam)," "))
NO-LOCK NO-ERROR.

IF AVAIL code THEN
DO:
   /*

   IF INT((NOW - DATETIME-TZ(code.misc[1])) / 60000) LT 2 THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "1","�������� ���� �����, ��⮬ ������ ������ ����᪠!").
      RETURN.
   END.

   */

   RUN Fill-SysMes IN h_tmess ("", "", "3",
       SUBSTITUTE("������ ��楤�� 㦥 ����᪠���� &1 �����(-�) ����� ���짮��⥫�� &2 �� ���� &3 䨫��� &4",
                  INT((NOW - DATETIME-TZ(code.misc[1])) / 60000),code.val,code.misc[3],code.misc[4]) +
         ".|�⬥����,�� ࠢ�� ��������").

   IF pick-value EQ "1" THEN
      RETURN.
   ELSE
   DO:
/*
      pick-value = "NO".
      RUN Fill-SysMes IN h_tmess ("", "", "4",SUBSTITUTE("��������!!!! ������ ����� �࠭���権 � ��᪮�쪮 ��⮪��. ���७� ?")).
      IF pick-value NE "YES" THEN
         RETURN.
*/
      FIND CURRENT code EXCLUSIVE-LOCK NO-ERROR.
      IF avail code THEN 
         DELETE code.
   END.
END.

/*
mrun = 2014.
*/
DO WHILE mrun GT 0:

   INPUT THROUGH VALUE("{&scriptforcount}") NO-ECHO.
   REPEAT:
      IMPORT mStr NO-ERROR.
   END.
   INPUT CLOSE.
   mrun = INT64(mStr).

   IF mrun GT 0 THEN
   DO:
      pick-value = "NO".
      RUN Fill-SysMes IN h_tmess ("", "", "4",
          SUBSTITUTE("�������� �� �ࢥ� 㦥 ����饭� &1 ��⮪(�,��)!!!! ~n" + 
                     "   ������ ����� �࠭���権 � ��᪮�쪮 ��⮪��.~n"
                   + "     ���� ��⮪� ���� 㡨�� !!!! ���७� ?",mrun)).
      IF pick-value NE "YES" THEN
      RETURN.
      UNIX SILENT VALUE("{&scriptforkill}" + " " + USERID("bisquit")).
      mkillSend = mkillSend + 1.
      PAUSE 2.
   END.

END.

   /*   RETURN.*/


ASSIGN
   end-date  = TODAY - 1
   gend-date = end-date.

{getdate.i}


CREATE code.
ASSIGN 
   code.class   = "��⮪������" 
   code.parent  = "��⮪������" 
   code.code    = STRING(TODAY)  + "," + ENTRY(1,ENTRY(4,iparam)," ") 
   code.misc[1] = STRING(NOW)
   code.misc[2] = STRING(mdelim)
   code.misc[3] = STRING(end-date)
   code.misc[4] = shFilial
   code.misc[5] = ENTRY(2,ENTRY(4,iparam)," ") 
   code.val     = USERID("bisquit")
NO-ERROR
.

IF ERROR-STATUS:ERROR THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "-1","�訡��! �� �����⨫���!").
   RETURN.
END.

VALIDATE code NO-ERROR. 

IF ERROR-STATUS:ERROR THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "-1","�訡��! �� �����⨫���!").
   RETURN.
END.


RUN potokrun.p(mstart,mend,mdelim,shFilial,TRIM(ENTRY(4,iparam)),USERID("bisquit"),end-date).

RUN Fill-SysMes IN h_tmess ("", "", "1","����饭�!").

