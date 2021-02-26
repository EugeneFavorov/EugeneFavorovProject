/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: brw446psym.trg
      Comment: Браузер классификатора "ЦБ446-Симв".
               Триггеры формы редактирования.
   Parameters: Нет
      Created: 19.10.2015 krok
     Modified: <date> <who>
*/

ON "GO" OF FRAME edit DO:
   ASSIGN
      code.val
      code.misc[1]
      code.misc[2]     
      code.name     
   .

  /* RUN CheckType(code.name).
   IF RETURN-VALUE = "NO-APPLY" THEN DO:
      APPLY "ENTRY" TO code.name IN FRAME edit.
      RETURN NO-APPLY.
   END.*/
END.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='18/11/2015 18:06:48.617+04:00' */
/* $LINTUSER='soav' */
/* $LINTMODE='1' */
/* $LINTFILE='brw446psym.trg' */
/*prosignNRLYylkk3NBbgwqv2ISzZg*/


