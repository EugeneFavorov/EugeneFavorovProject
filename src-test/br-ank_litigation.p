{globals.i}

DEF INPUT PARAMETER level AS INT64 NO-UNDO.

DEF VAR mStat AS CHAR FORMAT "X(30)"  NO-UNDO.

FORM   
            
     mStat label "�㤥��� ᯮ��" view-as combo-box size 30 by 1
           list-items 
                "��",
                "���", 
                "����(㪠����)"                
           help " " skip     
        
      
   WITH FRAME frame1  
   ROW level + 2 COL 28  centered
           OVERLAY SIDE-LABELS
           TITLE "[ �������� �������� ]".         
       
ASSIGN
   mStat = "��".
 

DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE
   WITH FRAME frame1.

   UPDATE   
      mStat .         
        
END.
HIDE FRAME frame1 NO-PAUSE.

pick-value = mStat.