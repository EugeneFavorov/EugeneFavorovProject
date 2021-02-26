
DEF VAR vI AS INT64 COLUMN-LABEL "Стресс тест BISPC.exe, убедитесь что!тест проходит без сбоя передачи данных.Итерация N:" NO-UNDO.


DO vI = 1 TO 10000:
  OUTPUT TO VALUE(STRING(vI) + ".vbs").
  PUT UNFORMATTED "TEST = 1" SKIP.
  OUTPUT CLOSE.
  READKEY PAUSE 0.
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "Вы нажали esc, повторите тест заново!" VIEW-AS ALERT-BOX.
  RUN sndbispc.p ("file=" + STRING(vI) + ".vbs;class=bq").
  IF LASTKEY EQ KEYCODE("ESC") THEN MESSAGE "Вы нажали esc, повторите тест заново!" VIEW-AS ALERT-BOX.
  DISP vI WITH 1 DOWN. PAUSE 0.
  OS-DELETE VALUE(STRING(vI) + ".vbs").
END.

MESSAGE "Стрес тест выполнен успешно! I =  " vI VIEW-AS ALERT-BOX.

