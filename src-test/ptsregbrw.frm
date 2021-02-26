/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ptsregbrw.FRM
      Comment: Движение(регистрация) документов - формы
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:    
*/

FORM
   DateValue
      COLUMN-LABEL "Срок(дата)"
      FORMAT       "99/99/9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "Срок(дата)"
   NameEvent
      COLUMN-LABEL "Событие"
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "Событие"
   Branch_
      COLUMN-LABEL "Офис"
      FORMAT       "x(4)"
      VIEW-AS FILL-IN SIZE 4 BY 1
      HELP         "Офис"
   Details
      COLUMN-LABEL "Комментарии"
      FORMAT       "x(40)"
      VIEW-AS FILL-IN SIZE 40 BY 1
      HELP         "Комментарии"
   Descriptions
      COLUMN-LABEL "Принял"
      FORMAT       "x(40)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      HELP         "Принял"      
   DateChange
      COLUMN-LABEL "Дата изменения"
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "Дата изменения"
   User_Id
      COLUMN-LABEL "Пользов."
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "Пользов."
      
            
WITH FRAME browse1 TITLE COLOR bright-white "[ Учет ПТС ]" WIDTH 120.


FORM
   DateValue
      LABEL "Срок(дата)"
      FORMAT       "99/99/9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "Срок(дата)"
   NameEvent
      LABEL "Событие"
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "Событие"
   Branch_
      LABEL "Офис"
      FORMAT       "x(4)"
      VIEW-AS FILL-IN SIZE 4 BY 1
      HELP         "Офис"
   Details
      LABEL "Комментарии"
      FORMAT       "x(200)"
      VIEW-AS EDITOR SIZE 54 BY 3
      HELP         "Комментарии"
   PrinalFIO
      LABEL "Принял ФИО"
      FORMAT       "x(200)"
      VIEW-AS TEXT SIZE 54 BY 1
      HELP         "Принял ФИО"  
   Descriptions
      LABEL "Принял"
      FORMAT       "x(30)"
      VIEW-AS FILL-IN SIZE 15 BY 1
   DateChange
      LABEL "Дата изменения"
      FORMAT       "x(19)"
      VIEW-AS TEXT SIZE 19 BY 1
      HELP         "Дата изменения"
   User_Id
      LABEL "Пользов."
      FORMAT       "x(10)"
      VIEW-AS TEXT SIZE 10 BY 1 
      HELP         "Пользов."
   User_FIO
      LABEL "ФИО Пользов."
      FORMAT       "x(40)"
      VIEW-AS TEXT SIZE 40 BY 1 
      HELP         "ФИО Пользов."
WITH FRAME edit CENTERED TITLE COLOR bright-white IF LASTKEY EQ KEYCODE("INS") THEN "[ Создание ]" ELSE "[ Редактирование ]".


