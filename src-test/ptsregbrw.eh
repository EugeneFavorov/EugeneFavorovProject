/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ptsregbrw.EH
      Comment: Движение(регистрация) документов - триггеры формы
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:    
*/

DEF BUFFER bfplindocsreg FOR pl_indocsreg.

ON LEAVE OF Branch_ IN FRAME edit
DO:
    IF LASTKEY <> KEYCODE("UP") THEN DO:
        FIND FIRST branch WHERE branch.branch-id = Branch_:SCREEN-VALUE IN FRAME edit NO-LOCK NO-ERROR.
        IF NOT AVAIL branch THEN DO:
            MESSAGE "Нет такого подразделения, выбор по F1" VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY {&RET-ERROR}.
        END.
    END.
END.
 
ON LEAVE OF NameEvent IN FRAME edit
DO:
    CASE EnableDescription(NameEvent:SCREEN-VALUE IN FRAME edit):
        WHEN 0 THEN DO:
            DISABLE Descriptions WITH FRAME edit.
  /*          PrinalFIO:LABEL = "". 
            Descriptions:LABEL = "".  */ 
        END.
        WHEN 1 THEN DO:
            ENABLE Descriptions WITH FRAME edit.
            PrinalFIO:LABEL = "Принял ФИО". 
            Descriptions:LABEL = "Принял". 
        END.
        WHEN 2 THEN DO:
            ENABLE Descriptions WITH FRAME edit.
/*            PrinalFIO:LABEL = "Выдал ФИО".
            Descriptions:LABEL = "Выдал".  */ 
            PrinalFIO:LABEL = "Принял ФИО". 
            Descriptions:LABEL = "Принял".
        END.
    END CASE.
    IF LASTKEY <> KEYCODE("UP") THEN DO:
        IF CheckNameEvent(NameEvent:SCREEN-VALUE IN FRAME edit) = FALSE THEN RETURN NO-APPLY {&RET-ERROR}.
    END.
END.   

ON LEAVE OF Descriptions IN FRAME edit
DO:
    IF LASTKEY <> KEYCODE("UP") THEN DO:
        IF EnableDescription(NameEvent:SCREEN-VALUE IN FRAME edit) > 0 AND CheckUser(Descriptions:SCREEN-VALUE IN FRAME edit) = FALSE THEN DO:
            RETURN NO-APPLY {&RET-ERROR}.
        END.
    END. 
END.
/* отключено в связи с возможностью ввести дату поступления до окончания срока сдачи 
все проверки дат в ptsregbrw.upd
 
ON LEAVE OF DateValue IN FRAME edit
DO:    
    IF CheckDateValue(DATE(DateValue:SCREEN-VALUE IN FRAME edit)) = FALSE THEN RETURN NO-APPLY {&RET-ERROR}.
END.    

*/
