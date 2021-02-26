/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: polngahen.p
      Comment: Печать соглашения об отступном
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 13/04/2018 zss Обращение Заемщика полное гашение
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "obr_za_gah"     /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc ""      /*для классификатора signat*/




{getdate.i
   &DateLabel="Задайте дату"
   &DateHelp="выберите дату которая будет проставляться в отчете"
   &AddPostUpd=" 
      
               "


}
RUN Insert_TTName("Date_oth", STRING(end-date, '99.99.9999')).
{precrdprint.p {&*}}
