/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: OPKAUFLT.P
      Comment:
   Parameters:
         Uses:
      Used by:
      Created: ... давным давно
     Modified: 21.02.2003 16:48 SEMA     по заявке 0014217 добавление новых параметров в фильтр по документам
     Modified: 05.05.2003 15:27 SEMA     по заявке 0014217 изменено описание
     Modified: 14.07.2003 kraw (0018600) чтобы or&and не воспринимался как допреквизит
     Modified: 23.07.2003 ilvi (14217) добавлено поле в фильтр PrintProc - процедура печати
     Modified: 09.07.2004 ilvi (21766) Структурирован код. добавлены поля субаналитики
     Modified: 24.04.2005 xaro         по заявке 0034389
     Modified: 09.07.2005 Om  Доработка.
                        В закладку "Импорт/Экспорт" добавлено поле "время экспорта".
                        Для полей "время экспорта" и "время импорта" можно задавать
                        интервал.
     Modified: 09.10.2007 Muta 0080920 Для браузера проводок добавлена возможность отобрать
                                       проводки имеющие/не имеющие связанных субъектов.
*/

DEFINE INPUT PARAMETER iClassCode AS CHAR NO-UNDO.

{globals.i}
{flt-file.i}
{intrface.get tmess}

DEFINE VARIABLE list-class AS CHARACTER NO-UNDO.    /* Необходимо для flt-file.end */
DEFINE VARIABLE num-class  AS INTEGER   NO-UNDO.    /* Счетчик */

{flt-file.add
   &cat     = 10
   &asgn    = YES
   &tablef  = "'op'"
   &labelt  = "'ОПЕРАЦИИ'"
   &include = ""op,mPos,op-date,user-id,user-inspector,doc-date,due-date,op-value-date,ins-date,order-pay,doc-num,doc-type,acct-cat,op-status,op-kind,class-code,ben-acct,name-ben,op-error,details,inn""
   &double  =  ""mPos""
   &classf  = "'opokau'"
   &sortf   = ""mPos""
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'op'"
   &a-code-value = "'*'"
   &a-initial    = "'*'"
   &a-datatype   = "'character'"
   &a-multi      = YES
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'mPos1'"
   &a-basic      = "''"
   &a-datatype   = "'decimal'"
   &a-format     = "'>>>>>>>>9.99'"
   &a-label      = "'Ост. на бал.сч. от:'"
   &a-help       = "'Остаток на балансовом счете от'"
   &a-param      = "'mPos1'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'mPos2'"
   &a-basic      = "''"
   &a-datatype   = "'decimal'"
   &a-format     = "'>>>>>>>>9.99'"
   &a-initial    = "'999999999.99'"
   &a-label      = "'Ост. на бал.сч. до:'"
   &a-help       = "'Остаток на балансовом счете до'"
   &a-param      = "'mPos2'"
}

{flt-file.end}
