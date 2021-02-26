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
PUT UNFORMATTED SKIP(2) SPACE(16) "�����஢�� ��壠���᪨� ���㬥�⮢ �� " end-date Format "99/99/9999" SKIP(1).

PUT UNFORMATTED "���������������������������������������������������������������������������������Ŀ" SKIP.
PUT UNFORMATTED "�   �����    �                ����� ��⮢                �     �㬬� (��.)     �" SKIP.   
PUT UNFORMATTED "� ���㬥��  ���������������������������������������������Ĵ                      �" SKIP. 
PUT UNFORMATTED "�            �      �� ������       �      �� �।���      �                      �" SKIP.   
PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
PUT UNFORMATTED "�     1      �           2          �           3          �          4           �" SKIP.   

FOR EACH tt-kko NO-LOCK
    BREAK BY tt-kko.city-id BY tt-kko.kko
    :
    IF FIRST(tt-kko.city-id) THEN 
    DO: /*2*/
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "�     " + "�� �����ᮢ� ��⠬   " + "                                                     �" SKIP.   
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "� �⮣�                                                    � " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
        mItog = 0.
    END. /*2*/
    IF FIRST-OF(tt-kko.kko) AND NOT FIRST(tt-kko.city-id) THEN 
    DO: /*3*/
        FIND FIRST branch WHERE branch.branch-id EQ tt-kko.kko
             NO-LOCK NO-ERROR.
        IF AVAILABLE(branch) THEN
            bank-name = branch.NAME + " �. " + tt-kko.city.
	ELSE bank-name = tt-kko.city.
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "� " + string( tt-kko.kko + " " + Bank-name, "x(80)") +                           "�" SKIP.
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "�     " + "�� �����ᮢ� ��⠬   " + "                                                     �" SKIP.   
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�     ��壠���᪨� ���㬥���                                                     �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "� �⮣�                                                    � " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�     ���ᮢ� ���㬥���                                                          �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "� �⮣�                                                    � " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�     �� ������ � ������⮩                                                    �" SKIP.
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ��壠���᪨�                                                      �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

       PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
       PUT UNFORMATTED "�            � �⮣�                                       � " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ���ᮢ�                                                           �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � �⮣�                                       � " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
    mItog = 0.
    END.

    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.save-type EQ "e"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�     �࠭���� � ���஭��� ����                                                 �" SKIP.
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ���ᮢ�                                                           �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � �⮣�                                       � " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ��壠���᪨�                                                      �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

       PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
       PUT UNFORMATTED "�            � �⮣�                                       � " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
       mItog = 0.
    END.
/*------------------------*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "o"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "�     " + "�� ��������ᮢ� ��⠬" + "                                                     �" SKIP.   
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "� �⮣�                                                    � " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�     ���ᮢ� ���㬥���                                                          �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "� �⮣�                                                    � " 
                      + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "�     �� ������ � ������⮩                                                    �" SKIP.
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ��壠���᪨�                                                      �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

       PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
       PUT UNFORMATTED "�            � �⮣�                                       � " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ���ᮢ�                                                           �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � �⮣�                                       � " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
    mItog = 0.
        END.

        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.save-type EQ "e"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
        PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
        PUT UNFORMATTED "�     �࠭���� � ���஭��� ����                                                 �" SKIP.
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ���ᮢ�                                                           �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � �⮣�                                       � " 
                  + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
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
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
    PUT UNFORMATTED "�            � ��壠���᪨�                                                      �" SKIP.   
    PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

       PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
       PUT UNFORMATTED "�            � �⮣�                                       � " 
                     + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
       mItog = 0.
        END.

END.

PUT UNFORMATTED "�����������������������������������������������������������������������������������" SKIP.
END. /*1*/

