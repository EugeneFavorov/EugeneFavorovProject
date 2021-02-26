/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ЗАО "Банковские информационные системы"
     Filename: vp_otdocfvi.p
      Comment: классификатор "ВП_ОтборДок"
   Parameters:
         Uses:
      Used by:
      Created: 18.03.2013     
     Modified: 18.03.2013   
*/

{vp_otdoc.def }
{globals.i}
{intrface.get xclass}
{ttretval.def}

DEFINE INPUT  PARAMETER ipTitle  AS CHARACTER NO-UNDO. /* заголовок для фрейма */
DEFINE INPUT  PARAMETER ipLevel  AS INT64     NO-UNDO.  /* уровень фрейма */
DEFINE OUTPUT PARAMETER opResult AS INT64     NO-UNDO.

ASSIGN
   opResult = -1 /* выход по ESC */
   mMode    = IF NUM-ENTRIES(ipTitle,"|") GT 1
              THEN ENTRY(2,ipTitle,"|")
              ELSE ""
   ipTitle  = ENTRY(1,ipTitle,"|")
.

IF ipTitle EQ ? OR ipTitle EQ "" THEN 
   ipTitle = "Параметры фильтра".

FORM

   "Балсчет по Дебету: " vBalDb                    
       HELP "Введите маску счетов 2го порядка по дебету"  SKIP
   "И/ИЛИ: " vTypRun   
       FORMAT "И/ИЛИ"                                     SKIP
   "Балсчет по Кредиту: "  vBalCr
       HELP "Введите маску счетов 2го порядка по кредиту" SKIP(1)
   "Содержание операции (учит. рег.): "   vDetailReg      SKIP(1)
   "Содержание операции (не учит. рег.): " vDetailNoReg   SKIP(1) 
   "Сумма от: "                             vSumOt        SKIP(1)
   "Сумма до: "                             vSumDo        SKIP 

WITH FRAME fFilterFrame NO-LABELS TITLE COLOR bright-white "[ " + ipTitle + " ]" CENTERED ROW ipLevel OVERLAY.


ON 'F1' OF vTypRun IN FRAME fFilterFrame 
DO:
   vTypRun = NOT vTypRun.
   DISPLAY vTypRun WITH FRAME fFilterFrame.
   RETURN NO-APPLY.
END.

PAUSE 0.
sss:
DO ON ERROR UNDO sss, LEAVE sss ON ENDKEY UNDO sss, LEAVE sss:
    DO WITH FRAME fFilterFrame:
        DISP vBalDb  vTypRun vBalCr vDetailReg vDetailNoReg vSumOt vSumDo  WITH FRAME fFilterFrame.

        IF mMode NE "modeview" THEN DO:
           ENABLE vBalDb vTypRun vBalCr vDetailReg vDetailNoReg vSumOt vSumDo.
           WAIT-FOR GO, WINDOW-CLOSE OF CURRENT-WINDOW FOCUS vBalDb.
        END.
    END.
END.

ASSIGN vBalDb vBalCr vDetailReg vDetailNoReg vTypRun vSumOt vSumDo.

HIDE FRAME fFilterFrame.
IF LASTKEY NE 10 AND LASTKEY NE 13 THEN RETURN.
opResult = 0.
