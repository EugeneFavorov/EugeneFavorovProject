
FUNCTION vf_tabldtl RETURN LOGICAL (INPUT iCat AS CHARACTER, INPUT iName AS CHARACTER):

DEFINE VARIABLE mIsNotEmpty AS LOGICAL NO-UNDO.
DEFINE VARIABLE mItog       AS DECIMAL NO-UNDO.

mIsNotEmpty = NO.


FIND FIRST  ttDetail WHERE ttDetail.fAcctCat EQ iCat NO-ERROR.

IF AVAILABLE ttDetail THEN
DO:

   PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
   PUT UNFORMATTED "Ё     " + iName + "                                                     Ё" SKIP.   
   PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
   mItog = 0.

   FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat:
      PUT UNFORMATTED "Ё " + STRING(ttDetail.fDocNum, "x(10)") 
                   + " Ё " + STRING(ttDetail.fAcctDb, "x(20)") 
                   + " Ё " + STRING(ttDetail.fAcctCr, "x(20)") 
                   + " Ё " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " Ё" SKIP.   
      mIsNotEmpty = YES.
      mItog = mItog + ttDetail.fAmt.
   END.

   PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
   PUT UNFORMATTED "Ё ┬Б╝ё╝                                                    Ё " 
                 + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocKas NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:

      PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
      PUT UNFORMATTED "Ё     ┼═АА╝╒К╔ ╓╝╙Ц╛╔╜БК                                                          Ё" SKIP.   
      PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
      mItog = 0.

      FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocKas:
         PUT UNFORMATTED "Ё " + STRING(ttDetail.fDocNum, "x(10)") 
                      + " Ё " + STRING(ttDetail.fAcctDb, "x(20)") 
                      + " Ё " + STRING(ttDetail.fAcctCr, "x(20)") 
                      + " Ё " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " Ё" SKIP.   
         mIsNotEmpty = YES.
         mItog = mItog + ttDetail.fAmt.
      END.

      PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
      PUT UNFORMATTED "Ё ┬Б╝ё╝                                                    Ё " 
                    + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
   END.

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                         AND (NOT ttDetail.fDocRub) 
                         AND (NOT ttDetail.fDocDrg)
      NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:

      PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
      PUT UNFORMATTED "Ё     ▐╝ ╝╞╔Ю═Ф╗О╛ А ╗╜╒═╚НБ╝╘                                                    Ё" SKIP.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (NOT ttDetail.fDocDrg)
                            AND (NOT ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "цддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё │ЦЕё═╚Б╔ЮА╙╗╔                                                      Ё" SKIP.   
         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (NOT ttDetail.fDocDrg) 
                             AND (NOT ttDetail.fDocKas):
            PUT UNFORMATTED "Ё " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " Ё " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " Ё" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
      END.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (NOT ttDetail.fDocDrg)
                            AND (ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "цддддддддддддедддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё ┼═АА╝╒К╔                                                           Ё" SKIP.   
         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (NOT ttDetail.fDocDrg) 
                             AND (ttDetail.fDocKas):
            PUT UNFORMATTED "Ё " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " Ё " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " Ё" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
      END.
   END.

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocDrg NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:
      PUT UNFORMATTED "цддддддддддддадддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
      PUT UNFORMATTED "Ё     ▐╝ ╝╞╔Ю═Ф╗О╛ А ╓Ю═ё. ╛╔Б═╚╚═╛╗                                              Ё" SKIP.   

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (ttDetail.fDocDrg)
                            AND (NOT ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "цддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё │ЦЕё═╚Б╔ЮА╙╗╔                                                      Ё" SKIP.   
         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (ttDetail.fDocDrg) 
                             AND (NOT ttDetail.fDocKas):
            PUT UNFORMATTED "Ё " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " Ё " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " Ё" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                    + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.
      END.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (ttDetail.fDocDrg)
                            AND (ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "цддддддддддддедддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё ┼═АА╝╒К╔                                                           Ё" SKIP.   
         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (ttDetail.fDocDrg) 
                             AND (ttDetail.fDocKas):
            PUT UNFORMATTED "Ё " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " Ё " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " Ё " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " Ё" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
         PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.
      END.
/*      PUT UNFORMATTED "цддддддддддддадддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.*/
   END.
END.


END FUNCTION.







IF mIsDetail THEN
DO:
PAGE.
PUT UNFORMATTED SKIP(2) SPACE(16) "░═АХ╗ДЮ╝╒╙═ ║ЦЕё═╚Б╔ЮА╙╗Е ╓╝╙Ц╛╔╜Б╝╒ ╖═ " end-date Format "99/99/9999" SKIP(1).

PUT UNFORMATTED "зддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддбдддддддддддддддддддддд©" SKIP.
PUT UNFORMATTED "Ё   █╝╛╔Ю    Ё                █╝╛╔Ю═ АГ╔Б╝╒                Ё     ▒Ц╛╛═ (ЮЦ║.)     Ё" SKIP.   
PUT UNFORMATTED "Ё ╓╝╙Ц╛╔╜Б═  цддддддддддддддддддддддбдддддддддддддддддддддд╢                      Ё" SKIP. 
PUT UNFORMATTED "Ё            Ё      ▐╝ ╓╔║╔БЦ       Ё      ▐╝ ╙Ю╔╓╗БЦ      Ё                      Ё" SKIP.   
PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддеддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
PUT UNFORMATTED "Ё     1      Ё           2          Ё           3          Ё          4           Ё" SKIP.   
/*PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.*/

vf_tabldtl("b", "▐╝ ║═╚═╜А╝╒К╛ АГ╔Б═╛   ").
vf_tabldtl("o", "▐╝ ╒╜╔║═╚═╜А╝╒К╛ АГ╔Б═╛").


PUT UNFORMATTED "юддддддддддддадддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддды" SKIP.
END.


