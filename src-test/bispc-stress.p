
DEF VAR vI AS INT64 COLUMN-LABEL "����� ��� BISPC.exe, 㡥����� ��!��� ��室�� ��� ᡮ� ��।�� ������.����� N:" NO-UNDO.

OUTPUT TO "1.vbs".
PUT UNFORMATTED "TEST = 1" SKIP.
OUTPUT CLOSE.

DO vI = 1 TO 10:
  READKEY PAUSE 0.
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "�� ������ esc, ������ ��� ������!" VIEW-AS ALERT-BOX.
  RUN sndbispc.p ("file=1.vbs;class=bq").
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "�� ������ esc, ������ ��� ������!" VIEW-AS ALERT-BOX.
  DISP vI WITH 1 DOWN. PAUSE 0.
END.

MESSAGE "���� ��� �믮���� �ᯥ譮!" VIEW-AS ALERT-BOX.

