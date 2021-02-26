/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: brw446psym.st
      Comment: Браузер классификатора "ЦБ446-Симв".
               Начальные действия и триггеры для формы браузера.
   Parameters: Нет
      Created: 19.10.2015 krok
     Modified: <date> <who>
*/

{form.def}

&SCOPED-DEFINE DEF-446P-SYMB-BRW-WIDTH 100

PROCEDURE DisplayBalAcct PRIVATE:
   DEFINE INPUT PARAMETER iSBalAcct AS CHARACTER NO-UNDO.

   RUN formld.p ("bal-acct", iSBalAcct, "", {&MOD_VIEW}, 4).
END PROCEDURE.

PROCEDURE DisplaySymb PRIVATE:
   DEFINE INPUT PARAMETER iSymb AS CHARACTER NO-UNDO.

   /* TBD */
END PROCEDURE.

/* ---------------------------------------------------------------- */


ON "F1" OF code.name IN FRAME browse1 DO:
   RUN DisplayType IN THIS-PROCEDURE (code.name).
END.

ON "F1" OF code.val IN FRAME browse1 DO:
   IF {assigned code.val} THEN DO:
      DISPLAY code.val
         NO-LABEL
         VIEW-AS EDITOR SIZE 32 BY 4
      WITH OVERLAY FRAME sss1 SIDE-LABELS 1 COL CENTERED ROW 12.
      ENABLE code.val WITH FRAME sss1.
      ON "F8" OF FRAME sss1 ANYWHERE DO:
         RETURN NO-APPLY.
      END.
      code.val:READ-ONLY IN FRAME sss1 = YES.
      WAIT-FOR GO OF FRAME sss1 FOCUS code.val.
   END.
END.

/* ---------------------------------------------------------------- */

ASSIGN
   /*
      Необходимо для устранения нежелательной горизонтальной
      прокрутки при переходе на поле "Значение детализации"
   */
   FRAME browse1:WIDTH = 100 /*{&DEF-446P-SYMB-BRW-WIDTH}*/
   /* Убираем все формы просмотра кроме основной, они не нужны */
   mFrmLst             = "1"
.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='22/12/2015 18:36:05.355+04:00' */
/* $LINTUSER='krok' */
/* $LINTMODE='1' */
/* $LINTFILE='brw446psym.st' */
/*prosignCZuXPHEjBLmj5mkrRAjP6Q*/