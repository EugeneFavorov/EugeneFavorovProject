
/* +++ status.frm was humbly modified by (c)blodd converter v.1.11 on 7/27/2017 7:11am +++ */

   DEFINE RECTANGLE rc  EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 78 BY 22 . 
   DEFINE RECTANGLE rc1 EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 48 BY 5 . 
   DEFINE RECTANGLE rc2 EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 40 BY 8 . 

   form skip(1)
       "Код статуса:" VIEW-AS TEXT  AT ROW 1 COL 3
       code.CODE 
         NO-LABEL 
         help "Код статуса" AT ROW 1 COL 22
       "Наименование:" VIEW-AS TEXT  AT ROW 2 COL 3
       code.name  
         NO-LABEL
         help "Наименование статуса" 
         View-As Fill-In  Size 39 By 1  AT ROW 2 COL 22  
       "Связанные статусы:" VIEW-AS TEXT  AT ROW 3 COL 3
       code.val 
         FORMAT "x(640)" 
         NO-LABEL 
         help "Связанные статусы" 
         View-As Fill-In Size 20 By 1  AT ROW 3 COL 22 
       "Описание:" VIEW-AS TEXT  AT ROW 4 COL 3
       code.description[1] 
         NO-LABEL
         help "Описание статуса" 
         FORMAT "x(45)"  
         View-As EDITOR SIZE 39 BY 2  AT ROW 4 COL 22   SKIP(1)
       "[  Маски  ]" VIEW-AS TEXT  AT ROW 6 COL 10 
       "Код документа:" VIEW-AS TEXT  AT ROW 7 COL 4
       code.misc[5] 
         NO-LABEL
         help "Маска кода документа"
         FORMAT "x(640)" 
         View-As Fill-In Size 25 By 1  AT ROW 7 COL 24 SKIP 
       "Технология платежа:" VIEW-AS TEXT  AT ROW 8 COL 4
       code.misc[7] 
         NO-LABEL 
         help "Маска технологии платежа"
         FORMAT "x(640)" 
         View-As Fill-In Size 25 By 1  AT ROW 8 COL 24 SKIP 
       "Код транзакции:" VIEW-AS TEXT  AT ROW 9 COL 4
       code.misc[6] 
         NO-LABEL
         help "Маска кода транзакции"
         FORMAT "x(640)" 
         View-As Fill-In Size 25 By 1  AT ROW 9 COL 24 skip(2) 
       "[  Опции обработки  ]" VIEW-AS TEXT  AT ROW 11 COL 5
       "Проверять документ:" VIEW-AS TEXT  AT ROW 12 COL 4
       code.misc[1]  
         NO-LABEL 
         help "Признак проверки документа при смене  статуса -Да,Нет,пусто" 
         FORMAT "x(3)"  AT ROW 12 COL 27 skip
       "Расширенная обработка:" VIEW-AS TEXT  AT ROW 13 COL 4
       code.misc[2] 
         NO-LABEL
         help "Признак проведения расширенной обработки документа -Да,Нет,пусто"
         FORMAT "x(3)"    AT ROW 13 COL 27
       "Аналитика:" VIEW-AS TEXT  AT ROW 14 COL 4
       code.misc[3] 
         NO-LABEL
         help "Признак обработки по аналитике -Да,Нет,пусто"
         FORMAT "x(3)"  AT ROW 14 COL 27
       "Процедура контроля (Вх):" VIEW-AS TEXT  AT ROW 15 COL 4
       code.misc[8] 
         NO-LABEL
         help "Процедура контроля (Вх)"
         FORMAT "x(12)"  AT  ROW 15 COL 29 SKIP

       "Процедура контроля (Вых):" VIEW-AS TEXT  AT ROW 16 COL 4
       code.misc[4] 
         NO-LABEL
         help "Процедура контроля (Вых)"
         FORMAT "x(12)"  AT  ROW 16 COL 29 SKIP

       "Процедура пост обработки:" VIEW-AS TEXT  AT ROW 17 COL 4
       code.description[2] 
         NO-LABEL
         help "Процедура пост обработки"
         FORMAT "x(12)"  AT  ROW 17 COL 29 SKIP
         
/* BACKGROUND */
/* rc AT ROW 1  COL 1 */
 rc1 AT ROW 6  COL 2 
 rc2 AT ROW 11  COL 2 
       {&wth} .


/* --- status.frm was humbly modified by (c)blodd converter v.1.11 on 7/27/2017 7:11am --- */
