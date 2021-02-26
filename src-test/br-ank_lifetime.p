{globals.i}

DEF INPUT PARAMETER level AS INT64 NO-UNDO.

DEF VAR mStat AS CHAR FORMAT "X(30)"  NO-UNDO.

FORM   
            
     mStat label "Срок" view-as combo-box size 30 by 1
           list-items 
                "Более  5 лет",
                "От 1 до 5 лет", 
                "От 6 до 12 месяцев",
                "От 3 до 6 месяцев", 
                "Менее 3 месяцев"
           help " " skip     
        
      
   WITH FRAME frame1  
   ROW level + 2 COL 28  centered
           OVERLAY SIDE-LABELS
           TITLE "[ ВЫБИРИТЕ ЗНАЧЕНИЕ ]".         
       
ASSIGN
   mStat = "Более  5 лет".
 

DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE
   WITH FRAME frame1.

   UPDATE   
      mStat .         
        
END.
HIDE FRAME frame1 NO-PAUSE.

pick-value = mStat.