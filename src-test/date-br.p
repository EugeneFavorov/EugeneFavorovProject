{globals.i}

DEF INPUT PARAMETER level AS INT64 NO-UNDO.

DEF VAR mStat AS CHAR INITIAL "äÇ" NO-UNDO.
DEF VAR mNumb AS CHAR NO-UNDO.

 

FORM   
      mStat  VIEW-AS COMBO-BOX LIST-ITEMS "äÇ", "èã" , "ÉÑ"  INNER-LINES 3 DROP-DOWN-LIST SIZE 10 BY 1 AT ROW 2 COL 2 LABEL "è•‡®Æ§" SKIP 
      mNumb FORMAT "XX.XXXX"
            LABEL "Ñ†‚†"
            HELP "Ñ†‚†"

   WITH FRAME frame1 ROW level + 2 COL 28 
           OVERLAY SIDE-LABELS
           TITLE "[ ÇÇÖÑàíÖ ]".

ASSIGN
   mStat = /* ENTRY(1,pick-value) */ "äÇ"
   mNumb = IF NUM-ENTRIES(pick-value) EQ 2
              THEN  ENTRY(2,pick-value)
              ELSE "".

DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE
   WITH FRAME frame1.

   UPDATE  mStat         
           mNumb. 
END.
HIDE FRAME frame1 NO-PAUSE.

pick-value = mStat + "." + STRING(mNumb, "XX.XXXX").
