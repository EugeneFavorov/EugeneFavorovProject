/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: terrslct.p
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created: 
     Modified:
*/

DEFINE TEMP-TABLE t-obj NO-UNDO
   FIELD rec AS RECID.

DEFINE INPUT PARAMETER TABLE FOR t-obj.
DEFINE INPUT PARAMETER iNameList AS  CHARACTER NO-UNDO.

/* Вставка Плюс банк */
{globals.i}
IF (GetSysconf("NO_Terr_Check") = "ДА")
THEN DO:
    APPLY "END-ERROR".
    RETURN.
END.
/* Конец вставки Плюс банк */

DEFINE VARIABLE mName     AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE in-title  AS CHARACTER          NO-UNDO.
DEFINE VARIABLE in-parent AS CHARACTER          NO-UNDO.

&IF DEFINED( MANUAL-REMOTE ) &THEN
   DEFINE VARIABLE ttt AS CHARACTER   NO-UNDO.
   DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
        LABEL "Продолжить" SIZE 11 BY 1 .
   DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
        LABEL "OK" 
        &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
        &ELSE SIZE 6 BY 1 &ENDIF.
&ENDIF

FORM
   mName[1] 
      LABEL "Участник 1" 
      FORMAT "x(56)"
   mName[2] 
      LABEL "Участник 2" 
      FORMAT "x(56)"
   mName[3] 
      LABEL "Участник 3" 
      FORMAT "x(56)"
   mName[4] 
      LABEL "Назначение платежа"          
      VIEW-AS EDITOR SIZE 40 BY 6 FORMAT "x(400)"
&IF DEFINED( MANUAL-REMOTE ) &THEN
   ttt VIEW-AS EDITOR SIZE 40 BY 15 FORMAT "x(400)"
   Btn_Cancel
   Btn_Ok
&ENDIF
/* WITH FRAME frTitle OVERLAY PAGE-TOP CENTERED ROW 1 WIDTH 80 SIDE-LABEL */
 WITH FRAME frTitle OVERLAY CENTERED ROW 5 WIDTH 80 SIDE-LABEL 
        TITLE "[ КОНТРОЛЬ УЧАСТНИКОВ ОПЕРАЦИИ ]" 
&IF DEFINED( MANUAL-REMOTE ) &THEN
      CANCEL-BUTTON Btn_Cancel
&ENDIF
      .

&IF DEFINED( MANUAL-REMOTE ) &THEN
Btn_Cancel:VISIBLE = NO .
Btn_Ok:VISIBLE = NO .
ttt:VISIBLE = NO .
&ENDIF

ASSIGN
   mName[1]  = ENTRY(1,iNameList,CHR(1))
   mName[2]  = ENTRY(2,iNameList,CHR(1))
   mName[3]  = ENTRY(3,iNameList,CHR(1))
   mName[4]  = ENTRY(4,iNameList,CHR(1)) WHEN NUM-ENTRIES(iNameList,CHR(1)) GT 3
   in-title  = "СПИСОК ПОДХОДЯЩИХ ТЕРРОРИСТИЧЕСКИХ ОРГАНИЗАЦИЙ"
   in-parent = "TerrBlack"
.

DISPLAY 
   mName[1]
   mName[2]
   mName[3]
   mName[4]
WITH FRAME frTitle.

&IF DEFINED( MANUAL-REMOTE ) &THEN
   frame frTitle:HEIGHT-CHARS = 99 .
      {wait_for.i 
         &THIS_FRAME          = "frTitle" 
         &EXT_LEAVE_OPTIONS   = "OR YES"} . 
&ENDIF

{terrview.p &SCOPE = YES}

&IF DEFINED( MANUAL-REMOTE ) &THEN
IF FRAME frTitle:VISIBLE THEN {hide_frame.i &THIS_FRAME = "frTitle"}
&ELSE
HIDE FRAME frTitle. 
&ENDIF
