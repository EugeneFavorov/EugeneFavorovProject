
DEF VAR vI AS INT64 COLUMN-LABEL "����� ��� BISPC.exe, 㡥����� ��!��� ��室�� ��� ᡮ� ��।�� ������.����� N:" NO-UNDO.


DO vI = 1 TO 10000:
  OUTPUT TO VALUE(STRING(vI) + ".vbs").
  PUT UNFORMATTED "TEST = 1" SKIP.
  OUTPUT CLOSE.
  READKEY PAUSE 0.
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "�� ������ esc, ������ ��� ������!" VIEW-AS ALERT-BOX.
  RUN sndbispc.p ("file=" + STRING(vI) + ".vbs;class=bq").
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "�� ������ esc, ������ ��� ������!" VIEW-AS ALERT-BOX.
  DISP vI WITH 1 DOWN. PAUSE 0.
  OS-DELETE VALUE(STRING(vI) + ".vbs").
END.

MESSAGE "���� ��� �믮���� �ᯥ譮! I =  " vI VIEW-AS ALERT-BOX.

