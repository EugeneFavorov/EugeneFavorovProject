{globals.i}
DEF VAR iNumDoc AS CHAR NO-UNDO.
DEF VAR fstr  AS CHAR NO-UNDO.
DEF VAR acct  AS CHAR INIT '' NO-UNDO.

PAUSE 0.

DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune:
  UPDATE
    iNumDoc LABEL "Номер" HELP "Номер файла"   
  WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
  TITLE "[ Параметры Отчета ]".
END.
HIDE FRAME ftune NO-PAUSE.

IF LASTKEY EQ KEYCODE("ESC") THEN
    RETURN.
INPUT FROM VALUE('/home2/bis/quit41d/src-test/chggrp/chggrp' + iNumDoc + '.txt').

REPEAT ON ENDKEY UNDO,LEAVE:
   IMPORT UNFORMATTED fstr.

   acct = acct + ',' + fstr + '*'.

END.

acct = SUBSTRING(acct,2,LENGTH(acct) - 1).

RUN browseld.p ("acct","acct" + chr(1) + 'Filial-id',acct + chr(1) + '0000',?,4).