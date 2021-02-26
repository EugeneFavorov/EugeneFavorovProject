/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: U:\GUVA\TITUL\T-DTL.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 28.08.2012 11:31 gva     
     Modified: 28.08.2012 15:06 gva      
     Modified: 28.08.2012 16:22 gva      
     Modified: 28.08.2012 16:25 gva      
     Modified: 28.08.2012 15:17 gva      
     Modified: 28.08.2012 16:40 gva      
     Modified: 28.08.2012 15:23 gva      
     Modified: 28.08.2012 15:24 gva      
     Modified: 28.08.2012 15:25 gva      
     Modified: 28.08.2012 15:27 gva      
     Modified: 28.08.2012 16:48 gva      
     Modified: 28.08.2012 15:34 gva      
     Modified: 28.08.2012 15:37 gva      
     Modified: 28.08.2012 15:59 gva      
     Modified: 28.08.2012 17:13 gva      
     Modified: 28.08.2012 17:12 gva      
     Modified: 30.08.2012 14:30 gva      
     Modified: 
*/

DEFINE VARIABLE mIsNotEmpty AS LOGICAL NO-UNDO.
DEFINE VARIABLE mItog       AS DECIMAL NO-UNDO.

mIsNotEmpty = NO.

DO: /*1*/
PAGE.
PUT UNFORMATTED SKIP(2) SPACE(16) "░═АХ╗ДЮ╝╒╙═ ║ЦЕё═╚Б╔ЮА╙╗Е ╓╝╙Ц╛╔╜Б╝╒ ╖═ " end-date Format "99/99/9999" SKIP(1).

PUT UNFORMATTED "зддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддбдддддддддддддддддддддд©" SKIP.
PUT UNFORMATTED "Ё   █╝╛╔Ю    Ё                █╝╛╔Ю═ АГ╔Б╝╒                Ё     ▒Ц╛╛═ (ЮЦ║.)     Ё" SKIP.   
PUT UNFORMATTED "Ё ╓╝╙Ц╛╔╜Б═  цддддддддддддддддддддддбдддддддддддддддддддддд╢                      Ё" SKIP. 
PUT UNFORMATTED "Ё            Ё      ▐╝ ╓╔║╔БЦ       Ё      ▐╝ ╙Ю╔╓╗БЦ      Ё                      Ё" SKIP.   
PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддеддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
PUT UNFORMATTED "Ё     1      Ё           2          Ё           3          Ё          4           Ё" SKIP.   

FOR EACH tt-kko NO-LOCK
    BREAK BY tt-kko.city-id BY tt-kko.kko
    :
    IF FIRST(tt-kko.city-id) THEN 
    DO: /*2*/
        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё     " + "▐╝ ║═╚═╜А╝╒К╛ АГ╔Б═╛   " + "                                                     Ё" SKIP.   
        PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
        mItog = 0.

        FOR EACH tt-op-day NO-LOCK
            WHERE tt-op-day.acct-cat EQ "b"
            AND tt-op-day.currency EQ "810"
            AND tt-op-day.acct-cat EQ "b"
            AND tt-op-day.razdel EQ "b"
            AND tt-op-day.save-type EQ "p"
            BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
            :
            {dtl-each.i}
        END.

        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё ┬Б╝ё╝                                                    Ё " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
        mItog = 0.
    END. /*2*/
    IF FIRST-OF(tt-kko.kko) AND NOT FIRST(tt-kko.city-id) THEN 
    DO: /*3*/
        FIND FIRST branch WHERE branch.branch-id EQ tt-kko.kko
             NO-LOCK NO-ERROR.
        IF AVAILABLE(branch) THEN
            bank-name = branch.NAME + " ё. " + tt-kko.city.
	ELSE bank-name = tt-kko.city.
        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё " + string( tt-kko.kko + " " + Bank-name, "x(80)") +                           "Ё" SKIP.
        PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё     " + "▐╝ ║═╚═╜А╝╒К╛ АГ╔Б═╛   " + "                                                     Ё" SKIP.   
        PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
        mItog = 0.
    END. /*3*/

/*kau 2801*/
/*buh b*/
    if tt-kko.kko = "0000" then
    DO:
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p" 
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* buh b*/
    PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё     │ЦЕё═╚Б╔ЮА╙╗╔ ╓╝╙Ц╛╔╜БК                                                     Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
    mItog = 0.
    FOR EACH tt-op-day NO-LOCK
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "810"
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "b"
        AND tt-op-day.save-type EQ "p" 
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
        {dtl-each.i}
    END.
        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё ┬Б╝ё╝                                                    Ё " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
        mItog = 0.
    END.
    END.
/*end kau*/

    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k"
/*        AND tt-day-itog.save-type EQ "p" */
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* kas b*/
    PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё     ┼═АА╝╒К╔ ╓╝╙Ц╛╔╜БК                                                          Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
    mItog = 0.
    FOR EACH tt-op-day NO-LOCK
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "810"
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "k"
/*        AND tt-op-day.save-type EQ "p" */
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
        {dtl-each.i}
    END.
        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё ┬Б╝ё╝                                                    Ё " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
        mItog = 0.
    END.
/*----*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.save-type EQ "p"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё     ▐╝ ╝╞╔Ю═Ф╗О╛ А ╗╜╒═╚НБ╝╘                                                    Ё" SKIP.
    END.
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* buh b*/
    PUT UNFORMATTED "цддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё │ЦЕё═╚Б╔ЮА╙╗╔                                                      Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.currency EQ "VAL"
           AND tt-op-day.acct-cat EQ "b"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "p"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
           {dtl-each.i}
       END.

       PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
       PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
       mItog = 0.
    END.
    /* kas val b*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k"
        AND tt-day-itog.save-type EQ "p"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* kas val b*/
    PUT UNFORMATTED "цддддддддддддедддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┼═АА╝╒К╔                                                           Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
    mItog = 0.

    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "VAL"
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "p"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
        {dtl-each.i}
    END.

    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
    mItog = 0.
    END.

    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.save-type EQ "e"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё     ЕЮ═╜ОБАО ╒ М╚╔╙БЮ╝╜╜╝╛ ╒╗╓╔                                                 Ё" SKIP.
    END.
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k"
        AND tt-day-itog.save-type EQ "e"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* kas el b*/
    PUT UNFORMATTED "цддддддддддддедддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┼═АА╝╒К╔                                                           Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
    mItog = 0.

    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "e"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
        {dtl-each.i}
    END.

    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
    mItog = 0.
    END.

    
    /* buh el b*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "e"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* buh el b*/
    PUT UNFORMATTED "цддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё │ЦЕё═╚Б╔ЮА╙╗╔                                                      Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.acct-cat EQ "b"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "e"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
           {dtl-each.i}
       END.

       PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
       PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
       mItog = 0.
    END.
/*------------------------*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "o"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё     " + "▐╝ ╒╜╔║═╚═╜А╝╒К╛ АГ╔Б═╛" + "                                                     Ё" SKIP.   
        PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
        mItog = 0.
        FOR EACH tt-op-day NO-LOCK
            WHERE tt-op-day.acct-cat EQ "o"
            AND tt-op-day.currency EQ "810"
            AND tt-op-day.acct-cat EQ "b"
            AND tt-op-day.razdel EQ "b"
            AND tt-op-day.save-type EQ "p"
            BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
            :
            {dtl-each.i}
        END.

        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё ┬Б╝ё╝                                                    Ё " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
        mItog = 0.
    END.
        mItog = 0.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "810"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "k"
/*            AND tt-day-itog.save-type EQ "p" */
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* kas o*/
    PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё     ┼═АА╝╒К╔ ╓╝╙Ц╛╔╜БК                                                          Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддбддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
    mItog = 0.
    FOR EACH tt-op-day NO-LOCK
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "810"
        AND tt-op-day.acct-cat EQ "o"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "p"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
        {dtl-each.i}
    END.
        PUT UNFORMATTED "цддддддддддддаддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё ┬Б╝ё╝                                                    Ё " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
        mItog = 0.
        END.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "VAL"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "b"
            NO-LOCK NO-ERROR.
       IF AVAILABLE(tt-day-itog) THEN
       DO:
        PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё     ▐╝ ╝╞╔Ю═Ф╗О╛ А ╗╜╒═╚НБ╝╘                                                    Ё" SKIP.
       END.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "VAL"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "b"
            AND tt-day-itog.save-type EQ "p"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* buh o*/
    PUT UNFORMATTED "цддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё │ЦЕё═╚Б╔ЮА╙╗╔                                                      Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.currency EQ "VAL"
           AND tt-op-day.acct-cat EQ "o"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "p"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
           {dtl-each.i}
       END.

       PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
       PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
       mItog = 0.
        END.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "VAL"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "k"
/*            AND tt-day-itog.save-type EQ "p" */
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* kas val o*/
    PUT UNFORMATTED "цддддддддддддедддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┼═АА╝╒К╔                                                           Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
    mItog = 0.

    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "VAL"
        AND tt-op-day.acct-cat EQ "o"
        AND tt-op-day.razdel EQ "k"
/*        AND tt-op-day.save-type EQ "p" */
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
        {dtl-each.i}
    END.

    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
    mItog = 0.
        END.

        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.save-type EQ "e"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
        PUT UNFORMATTED "цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
        PUT UNFORMATTED "Ё     ЕЮ═╜ОБАО ╒ М╚╔╙БЮ╝╜╜╝╛ ╒╗╓╔                                                 Ё" SKIP.
        END.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "k"
            AND tt-day-itog.save-type EQ "e"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* kas el o*/
    PUT UNFORMATTED "цддддддддддддедддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┼═АА╝╒К╔                                                           Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
    mItog = 0.

    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.acct-cat EQ "o"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "e"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
        {dtl-each.i}
    END.

    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
    mItog = 0.
        END.

    
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "b"
            AND tt-day-itog.save-type EQ "e"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* buh el o*/
    PUT UNFORMATTED "цддддддддддддбдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢" SKIP.
    PUT UNFORMATTED "Ё            Ё │ЦЕё═╚Б╔ЮА╙╗╔                                                      Ё" SKIP.   
    PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддбддддддддддддддддддддддбдддддддддддддддддддддд╢" SKIP.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.acct-cat EQ "o"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "e"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
           {dtl-each.i}
       END.

       PUT UNFORMATTED "цддддддддддддеддддддддддддддддддддддаддддддддддддддддддддддедддддддддддддддддддддд╢" SKIP.
       PUT UNFORMATTED "Ё            Ё ┬Б╝ё╝                                       Ё " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " Ё" SKIP.   
       mItog = 0.
        END.

END.

PUT UNFORMATTED "юддддддддддддадддддддддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддды" SKIP.
END. /*1*/

