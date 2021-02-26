/*
               Банковская интегрированная система БИСквитpDP
    Copyright: 
     Filename: sogl_otst.p
      Comment: Печать соглашения об отступном
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 13/04/2018 zss обращение заемщика в части ОД
     Modified: ZSS добавление выбора даты
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.
DEFINE VARIABLE myStartDate AS DATE NO-UNDO.

myStartDate = TODAY.

&GLOBAL-DEFINE gdTplName  "obr_zaem"     /*имя шаблона ОСП*/
&GLOBAL-DEFINE gdSignProc ""      /*для классификатора signat*/

{getdate.i
   &DateLabel="Задайте дату"
   &DateHelp="выберите дату которая будет проставляться в отчете"
   &AddPostUpd=" 
      /* IF end-date > TODAY THEN
                DO:
                   MESSAGE 'Дата не может больше текущей'
                   VIEW-AS ALERT-BOX.
                   

                   UNDO, RETRY.
                END.*/
               "


}
RUN Insert_TTName("Date_oth", STRING(end-date, '99.99.9999')).


{precrdprint.p {&*}}

