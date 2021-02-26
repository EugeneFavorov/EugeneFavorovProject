/*
    Copyright:  (C) 1992-1996 БИС
     Filename: messmenu.p
      Comment: Выводит сообщение на экран и предлагает сделать выбор
                
         Uses:  
      Used by:  
      Created:  13/03/1996 by alex
     Modified:  13/11/2002 Gunk
     Modified:  06/04/2005 kraw (0045065) условная компиляция для messmnu1.p (MESSMENU_WITH_HELP_F1)
     Modified:  28/06/2005 kraw (0046181) изменяемая ширина меню, больше ширина и другой цвет у подсказки
     Modified: 27.06.2007 14:52 KSV      (0078824) Адаптирован под работу с
                                         Биссмарт
     Modified: <date> <who> <comment>
*/
{globals.i}

def input param row-pos as INT64 no-undo.
def input param tit as char no-undo.
def input param txt as char no-undo.
def input param choices as char no-undo.

&IF DEFINED(MESSMENU_WITH_HELP_F1) NE 0 &THEN
DEFINE INPUT PARAMETER iHelp AS CHARACTER NO-UNDO.
&ENDIF

&IF DEFINED(SET_FRAME_POSITION) &THEN
DEFINE INPUT PARAMETER iRow    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iColumn AS INTEGER NO-UNDO.
&ENDIF

&IF DEFINED(mwidth) EQ 0 &THEN
&SCOPED-DEFINE mwidth 65
&SCOPED-DEFINE mwidth2 67
&ENDIF

&IF DEFINED(SESSION-REMOTE) &THEN
   DEFINE VARIABLE vButton    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDefSelInt AS INT64     NO-UNDO.
   DEFINE VARIABLE vDefSelTxt AS CHARACTER NO-UNDO.

   tit = TRIM(tit,"[] ").
   vDefSelInt = INT64(pick-value) NO-ERROR.

   IF vDefSelInt > 0 AND vDefSelInt <= NUM-ENTRIES(choices)
   THEN vDefSelTxt = ENTRY(vDefSelInt,choices).
   ELSE vDefSelTxt = ?.

   RUN bloddAlertBox(txt,{&BLODD_MESSAGE_MENU},tit,choices,vDefSelTxt,OUTPUT vButton).
   IF NOT {assigned vButton} THEN vButton = "0".
   pick-value = vButton.
   {&LAST_KEY} = IF pick-value = "0" THEN 27 ELSE 13.
   RETURN .
&ELSE
DO with frame menu on endkey undo, leave on error undo, leave:
   {wordwrap.def}
   def var numch as INT64 no-undo.
   def var numar as INT64 no-undo.
   DEFINE VARIABLE menu_idx AS INT64 NO-UNDO.
   DEFINE VARIABLE mess_idx AS INT64 NO-UNDO.
   def var message_array as char extent 10 FORMAT "x({&mwidth})" no-undo.
   def var width_string as INT64 initial 0 no-undo.
   def var n-row as INT64 init 1 no-undo.

&IF DEFINED(MESSMENU_WITH_HELP_F1) NE 0 &THEN
   DEFINE VARIABLE mHelpMsg AS CHARACTER NO-UNDO.
   FORM 
      SKIP(1)
      " "
      mHelpMsg VIEW-AS TEXT FORMAT "x(72)"
      SKIP(1)
      WITH NO-LABEL CENTERED COLOR MESSAGES WIDTH 78 ROW 12 OVERLAY FRAME fHelp.
&ENDIF

   assign
     numch = num-entries(choices)
     message_array[1] = txt.
   if txt ne "" then do:
     {wordwrap.i &s=message_array &n=10 &l={&mwidth} &centered=yes}
   end.
   DO menu_idx = 1 TO 10:
     if message_array[menu_idx] ne "" then numar = menu_idx.
   END.

   if txt ne "" and substring(trim(txt),length(trim(txt))) eq "|" then numar = numar + 1.

   if numar gt 10 then numar = 10.

   IF numar > 5 AND row-pos >= 10
   THEN row-pos = row-pos - (10 - numar).

   assign
    n-row = INT64(pick-value) no-error.

   if (n-row <= 0) or (n-row = ?) or (n-row > numch) then n-row = 1.

   FORM
       ""
       with (numch + if txt ne "" then numar + 1 else 0) DOWN
            &IF DEFINED(SET_FRAME_POSITION) &THEN
            ROW iRow COLUMN iColumn
            &ELSE
            row row-pos CENTERED
            &ENDIF
            width {&mwidth2} title tit overlay color messages
            frame common.
   FORM
       message_array_string as char format "x({&mwidth})"
       WITH numar + 1 down
            1 COLUMN NO-LABEL NO-BOX
            &IF DEFINED(SET_FRAME_POSITION) &THEN
            ROW (iRow + 1) COLUMN iColumn
            &ELSE
            row (row-pos + 1) CENTERED
            &ENDIF
            overlay color messages
            frame mess.
   FORM
       menu_choices as char format "x({&mwidth})" help "Выберите пункт меню"
       WITH numch down 1 COLUMN NO-LABEL no-box OVERLAY
            &IF DEFINED(SET_FRAME_POSITION) &THEN
            ROW (iRow  + if txt ne "" then numar + 2 else 1) COLUMN iColumn + 1
            &ELSE
            CENTERED
            row (row-pos + if txt ne "" then numar + 2 else 1)
            &ENDIF
            color messages
            frame menu.

   DO menu_idx = 1 TO numch:
      width_string = maximum (width_string, length(entry(menu_idx, choices))).
   END.
   width_string = ({&mwidth} - width_string) / 2.

   pause 0.
   VIEW frame common.
   pause 0.
   VIEW.


   if txt ne "" then do:
     UP frame-line(mess) with frame mess.
     if txt ne "" then do mess_idx = 1 to numar:
        DISPLAY message_array[mess_idx] @ message_array_string with frame mess.
        DOWN with frame mess.
     end.
     DISPLAY fill("─",{&mwidth}) @ message_array_string with frame mess.
   end.
   UP frame-line(menu) - 1 with frame menu.
   DO menu_idx = 1 TO numch:
      DISPLAY (fill(" ",width_string) + ENTRY(menu_idx,choices)) @ menu_choices
      with frame menu.
      DOWN with frame menu.
   END.
   UP (numch - n-row + 1) with frame menu.

   do while true:
      CHOOSE ROW menu_choices AUTO-RETURN color bright-white no-error with frame menu.
      if lastkey eq keycode("DOWN") then do:
         color display messages menu_choices with frame menu.
         up frame-line(menu) - 1 with frame menu.
         next.
      end.
      else if lastkey eq keycode("UP") then do:
         color display messages menu_choices with frame menu.
         up frame-line(menu) - numch with frame menu.
         next.
      end.
      else if lastkey eq keycode("RETURN") then leave.

&IF DEFINED(MESSMENU_WITH_F2) NE 0 &THEN
      else if lastkey eq keycode("F2") then do:
    /*      def buffer loan for loan. 
         /* find current tmprecid no-lock no-error. */
          
          FIND tmprecid NO-LOCK NO-ERROR.
          if avail tmprecid then do:
            find first loan where recid(loan) = tmprecid.id no-lock no-error. 
            if avail loan then 
            message "F2 " + loan.cont-code view-as alert-box.
          end.
      */
      pick-value = ?.
      hide frame menu.
      hide frame mess.
      hide frame common.
      return.
      end.
&ENDIF  
          
&IF DEFINED(MESSMENU_WITH_HELP_F1) NE 0 &THEN

      ELSE IF LASTKEY EQ KEYCODE("F1") THEN
      DO:

         IF FRAME-LINE(menu) LE NUM-ENTRIES(iHelp) THEN
         DO:
            DISPLAY ENTRY(FRAME-LINE(menu), iHelp) @ mHelpMsg WITH FRAME fHelp.
            READKEY.
            HIDE FRAME fHelp.
         END.
         NEXT.
      END.

&ENDIF
      next.
   end.
   menu_idx = FRAME-LINE(menu).
END.

IF LASTKEY EQ KEYCODE("ESC") THEN
   pick-value = "0".
ELSE
   pick-value = string(menu_idx).
   
hide frame menu.
hide frame mess.
hide frame common.
&ENDIF
