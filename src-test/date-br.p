{globals.i}

DEF INPUT PARAMETER level AS INT64 NO-UNDO.

DEF VAR mStat AS CHAR INITIAL "��" NO-UNDO.
DEF VAR mNumb AS CHAR NO-UNDO.

 

FORM   
      mStat  VIEW-AS COMBO-BOX LIST-ITEMS "��", "��" , "��"  INNER-LINES 3 DROP-DOWN-LIST SIZE 10 BY 1 AT ROW 2 COL 2 LABEL "��ਮ�" SKIP 
      mNumb FORMAT "XX.XXXX"
            LABEL "���"
            HELP "���"

   WITH FRAME frame1 ROW level + 2 COL 28 
           OVERLAY SIDE-LABELS
           TITLE "[ ������� ]".

ASSIGN
   mStat = /* ENTRY(1,pick-value) */ "��"
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
