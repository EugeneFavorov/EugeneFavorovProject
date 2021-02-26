
DEF VAR vI AS INT64 COLUMN-LABEL "Стресс тест BISPC.exe, убедитесь что!тест проходит без сбоя передачи данных.Итерация N:" NO-UNDO.

OUTPUT TO "1.vbs".
PUT UNFORMATTED "TEST = 1" SKIP.
OUTPUT CLOSE.

DO vI = 1 TO 10:
  READKEY PAUSE 0.
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "Вы нажали esc, повторите тест заново!" VIEW-AS ALERT-BOX.
  RUN sndbispc.p ("file=1.vbs;class=bq").
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "Вы нажали esc, повторите тест заново!" VIEW-AS ALERT-BOX.
  DISP vI WITH 1 DOWN. PAUSE 0.
END.

MESSAGE "Стрес тест выполнен успешно!" VIEW-AS ALERT-BOX.

