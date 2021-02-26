{globals.i}

DEF INPUT PARAMETER level AS INT64 NO-UNDO.

DEF VAR mStat1 AS CHAR FORMAT "X(40)"  NO-UNDO.

FORM   
            
   mStat1 label "Цель"  view-as combo-box size 50 by 1
           list-items 
                "Открытие и ведение банковского счета",
                "Иное"
           help " " skip  
   WITH FRAME frame1 CENTERED  ROW 10 /*level + 2 COL 40 */
           OVERLAY SIDE-LABELS
           TITLE "[ ВЫБИРИТЕ ЗНАЧЕНИЕ  ]".

ASSIGN
   mStat1 = pick-value.
   


DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE
   WITH FRAME frame1.

   UPDATE  mStat1.         

       
END.
HIDE FRAME frame1 NO-PAUSE.

IF mStat1 EQ "Иное" Then RUN  OpenWin.  

pick-value = mStat1.

PROCEDURE OpenWin :   
DEFINE VARIABLE mstr                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mTmpStr2                AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mTmpStr3                AS CHARACTER NO-UNDO.                
PAUSE 0.

FORM                                            
     mStr VIEW-AS FILL-IN SIZE 40 BY 1 AT ROW 1 COL 1 LABEL  "Иное" FORMAT "x(255)"

    WITH FRAME fM CENTERED ROW 10 OVERLAY SIDE-LABELS
    TITLE "[ ВВЕДИТЕ ЗНАЧЕНИЕ ]" .

UPDATE    
    mStr
    WITH FRAME fM.
    
    
mStat1 ="Иное: " + mStr.
  /* mTmpStr3 = GetXAttrValueEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other","").       
   mTmpStr = entry(1,mTmpStr3,";").   
   
   if  mTmpStr = "" THEN 
        mTmpStr2 = mStr + mTmpStr3.
   else                                                           
        mTmpStr2 =  REPLACE(mTmpStr3,mTmpStr,mStr).   
      
   UpdateSignsEx('cust-corp',STRING(tt-cust-ident.cust-id),"ank_other",mTmpStr2).*/
HIDE FRAME fM.     
END PROCEDURE.

