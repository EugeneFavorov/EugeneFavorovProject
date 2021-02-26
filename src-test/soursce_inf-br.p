{globals.i}

DEF INPUT PARAMETER level AS INT64 NO-UNDO.

DEF VAR mStat1 AS CHAR NO-UNDO FORMAT "x(40)". 
 
FORM  
    mStat1 VIEW-AS COMBO-BOX LIST-ITEMS "Самостоятельно из анализа документов","Со слов клиента","Из всех источников" 
    DROP-DOWN-LIST SIZE 40 BY 3 AT ROW 1 COL 3 LABEL "Источник информации" 
    WITH FRAME frame1 WIDTH 46 CENTERED ROW 6 OVERLAY NO-LABELS TITLE "[ ВЫБЕРИТЕ ]".

DO ON ERROR  UNDO, LEAVE
    ON ENDKEY UNDO, LEAVE
    WITH FRAME frame1:
    
    ON 'LEAVE':U, 'GO':U OF mStat1 IN FRAME frame1 
    ASSIGN mStat1.    

    IF LASTKEY = 10 THEN  ASSIGN mStat1.       
    UPDATE  mStat1.         
                 
END.
HIDE FRAME frame1 NO-PAUSE.

pick-value = mStat1.
