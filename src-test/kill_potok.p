{intrface.get tmess}
{globals.i}

&scope scriptforcount /home2/bis/quit41d/src-auto/k14schet.sh
&scope scriptforkill /home2/bis/quit41d/src-auto/killllll.sh


DEFINE VARIABLE mrun      AS INT64 NO-UNDO. 
DEFINE VARIABLE mStr      AS CHAR  NO-UNDO.

RUN Init-SysMes IN h_tmess ("","","").


INPUT THROUGH VALUE("{&scriptforcount}") NO-ECHO.
REPEAT:
   IMPORT mStr NO-ERROR.
END.
INPUT CLOSE.
mrun = INT64(mStr).

pick-value = "NO".
RUN Fill-SysMes IN h_tmess ("", "", "4",SUBSTITUTE("����饭� &1 ��⮪(�,��) . ���� 㡨��?",mrun)).
IF pick-value NE "YES" OR LASTKEY EQ KEYCODE("ESC") THEN
  RETURN.


pick-value = "NO".
RUN Fill-SysMes IN h_tmess ("", "", "4",SUBSTITUTE("��筮 㡨�� &1 ��⮪(�) ?",mrun)).
IF pick-value NE "YES" OR LASTKEY EQ KEYCODE("ESC") THEN
  RETURN.

UNIX SILENT VALUE("{&scriptforkill}" + " " + USERID("bisquit")).

RUN Fill-SysMes IN h_tmess ("", "", "1","����饭�!").

