{globals.i}

DEF INPUT PARAMETER level AS INT64 NO-UNDO.

DEF VAR mStat1 AS CHAR NO-UNDO.

FORM  mStat1 FORMAT "999-999-999-99"
            LABEL "СНИЛС"
            HELP "Страховое свидетельство государственного пенсионного страхования" 
     
   WITH FRAME frame1 ROW level + 2 COL 28 
           OVERLAY SIDE-LABELS
           TITLE "[ ВВЕДИТЕ ]".

ASSIGN
   mStat1 = substring(pick-value,1,3)  + substring(pick-value,5,3) + substring(pick-value,9,3) + substring(pick-value,13,2) .

DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE
   WITH FRAME frame1.

   UPDATE  mStat1.         
        
END.
HIDE FRAME frame1 NO-PAUSE.

pick-value = substring(mStat1,1,3) + "-" + substring(mStat1,4,3) + "-" + substring(mStat1,7,3)+ "-" + substring(mStat1,10,2) .
