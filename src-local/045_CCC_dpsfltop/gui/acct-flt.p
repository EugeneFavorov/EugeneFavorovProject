/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: ACCT-FLT.P
      Comment: Структура фильтра счетов.
   Parameters:
         Uses:
      Used by: 
      Created:  SG   26 Nov 97    6:48 pm
     Modified:  29/10/2003 Om
     Modified:  30/11/2006 Ariz  Добавлены параметры для фильтрации 
                                 по остаткам и оборотам по счетам.
     Modified: 21.01.2010 19:21 ksv      (0121399) + AcctPosStatus                                 
*/

DEFINE INPUT PARAMETER iClassCode AS CHAR NO-UNDO. /* Класс объекта. */

{globals.i}             /* Глобальные переменные сессии. */
{flt-file.i}            /* Определение структуры динамического фильтра. */

DEFINE VARIABLE mProc AS CHARACTER INIT ""   NO-UNDO.
IF fGetSetting("FltAcctByCust","","") = "ДА" THEN mProc = 'custlist'  .

DEF VAR list-class AS CHAR NO-UNDO. /* Список класса и подклассов. */
DEF VAR num-class  AS INT64  NO-UNDO. /* Счетчик. */

{flt-file.add
   &cat     = 1
   &labelt  = "'Осн. реквизиты'"
   &tablef  = "'acct'"
   &include = "'acct-cat,bal-acct,class-code,iClassCode,acct,contract,currency,user-id,side,branch-id,cust-cat,cust-id,details,UserConf,ViewSC,kau-id,RetFld,mPersGr'"
   &hiddenf = "'UserConf,ViewSC,iClassCode,RetFld,mPersGr'"
   &classf  = iClassCode
   &methf   = "'acct-cat'"
   &sortf   = "'*'"
}

/* Верхний класс объекта. */
{flt-file.atr 
   &asgn       = YES
   &xcode      = "'iClassCode'"
   &a-initial  = iClassCode
}

{flt-file.add
   &cat     = 2
   &labelt  = "'Даты'"
   &tablef  = "'acct'"
   &include = "'open-date,1,close-date'"
   &double  = "'open-date,close-date'"
   &sortf   = "'*'"   
}

{flt-file.add
   &cat     = 3
   &tablef  = "''"
   &labelt  = "'Ост. и обороты'"
   &include = "'SldDate,SldType,mPosVal,mPosBal,AcctPosStatus,TurnDate,TurnType,sh-v,sh-b'"
   &double  = "'mPosVal,mPosBal,TurnDate,sh-v,sh-b'"
}

{flt-file.add
   &cat     = 4
   &tablef  = "'blockobject'"
   &labelt  = "'Блокировки'"
   &include = "'avail_blockobject,block-type,beg-datetime,end-datetime,txt[1],v-user-id,txt[2],txt[3],txt[4],txt[5],txt[6],txt[7],txt[8]'"
   &double  = "'beg-datetime,end-datetime'"
   &classf  = "'blockacct'"
}

{flt-file.add 
   &cat     = 5
   &labelt  = "'Доп. реквизит'"
   &tablef  = "'acct'"
   &include = "'sc-1,sv-1,3,sc-2,sv-2,4,sc-3,sv-3,5,sc-4,sv-4,6,sc-5,sv-5'"
}

{flt-file.add
   &cat     = 6
   &labelt  = "'Вид'"
   &tablef  = "'acct'"
   &include = "'view-type,2,sort-type,extra-title'"
   &hiddenf = "'extra-title'"
}

{flt-file.add
   &cat     = 7
   &tablef  = "'xlink'"
   &include = "'link-code,link-direction,link-object-id'"
   &labelt  = "'Связи'"
   &sortf   = "'*'"
}
                        /* Вкладка группы только для админов */
/*IF IsUserAdm(USERID("bisquit")) THEN*/
/*DO:                                 */
   {flt-file.add
      &cat     = 8
      &tablef  = "''"
      &include = "'GroupList,GroupFltType'"
      &labelt  = "'Группы'"
      &sortf   = "'*'"
   }

   {flt-file.atr
      &asgn          = yes
      &xcode         = "'GroupList'"
      &a-basic       = "''"
      &a-label       = "'Группы:'"
      &a-help        = "'Список групп доступа лицевых счетов (в формате CAN-DO)'"
      &a-datatype    = "'CHARACTER'"
      &a-format      = "'x(2000)'"
      &a-op          = "'EQ'"
      &a-procename   = "'browseld'"
      &a-param       = "'acct-group,,,,2'"
      &a-multi       = YES
   }

   {flt-file.atr
      &asgn          = yes
      &xcode         = "'GroupFltType'"
      &a-label       = "'Отбирать счета принадлежащие:'"
      &a-help        = "'Список групп доступа лицевых счетов (в формате CAN-DO)'"
      &a-view-as     = "'RADIO-SET'"
      &a-list        = "'any,all'"
      &a-val-labels  = "'Любой из указанных групп,Только указанным группам'"
      &a-initial     = "'any'"
      &a-code-value  = "'any'"
   }
/*END.*/

{flt-file.atr
   &asgn          = yes
   &xcode         = "'close-date1'"
   &a-code-value  = string(gbeg-date)
   &a-initial     = string(gbeg-date)
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'bal-acct'"
   &a-code-value  = "'*'"
   &a-initial     = "'*'"
   &a-datatype    = "'CHARACTER'"
   &a-op          = "'can-do'"
   &a-op-update   = NO
   &a-multi       = YES
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'cust-id'"
   &a-code-value  = "'*'"
   &a-initial     = "'*'"
   &a-datatype    = "'CHARACTER'"
   &a-multi       = YES
   &a-op          = "'CAN-DO'"
   &a-op-update   = NO
   &a-procename   = mProc
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'acct-cat'"
   &a-sensitive   = "   flt-attr.attr-code-value EQ '*' ~
                     OR flt-attr.attr-code-value EQ ''"
}
/* Необходимо для правильного формирования запроса.
** Если значение равно "", то поиск ведется по EQ.
** По этой причине "*" должно быть начальным значением. */
IF flt-attr.attr-initial EQ ""
   THEN flt-attr.attr-initial = "*".

{flt-file.atr
   &asgn          = yes
   &xcode         = "'side'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'*,А,П,АП'"
   &a-val-labels  = "'Все,Активные,Пассивные,Активно-пассивные'"
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'cust-cat'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'*,Ю,Ч,Б,В'"
   &a-val-labels  = "'Все,Юридические лица,Частные лица,Банки,Внутрибанковские счета'"
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'sort-type'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'1,2,3,4,5'"
   &a-val-labels  = "' сч. 2-го пор. - ном. л/с - вал.,'          +
                     ' сч. 2-го пор. - посл. 11 зн. л/с - вал.,'  +
                     ' первые 8 зн. л/с - посл. 11 зн. л/с,'      +
                     ' последние 7 зн. л/с - код доп. офиса,'     +
                     ' по умолчанию'"
   &a-code-value  = "'5'"
   &a-initial     = "'5'"
   &a-label       = "'Сортировка'"
   &a-help        = "'Сортировка'"
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'view-type'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'0,1,2,4'"
   &a-val-labels  = "' Наименование, Сальдо, Доп. реквизит, Сальдо на дату'"
   &a-code-value  = "'0'"
   &a-initial     = "'0'"
   &a-label       = "'Отображение'"
   &a-help        = "'Отображение'"
}
                        /* Для МФ режима, первым должен быть 4 фрейм. */
IF shModeMulty
THEN ASSIGN
   flt-attr.attr-list         = flt-attr.attr-list + ",3"
   flt-attr.attr-val-labels   = flt-attr.attr-val-labels + ", Филиал"
   flt-attr.attr-initial      = "3"
.

{flt-file.atr
   &asgn          = yes
   &xcode         = "'extra-title'"
   &a-code-value  = """"
   &a-initial     = """"
}

IF iClassCode EQ "acctd" THEN
DO:
   {flt-file.atr
      &asgn        = YES
      &xcode       = "'currency'"
      &a-label     = "'Код ЦБ:'"
      &a-help      = "'Код ценной бумаги'"
      &a-procename = "'browseld'"
      &a-param     = "'sec-code,,,,2'"
   }
END.
ELSE
DO:
   {flt-file.atr
      &asgn        = YES
      &xcode       = "'currency'"
      &a-label     = "'Код валюты:'"
      &a-help      = "'Код валюты'"
      &a-procename = "'browseld'"
      &a-param     = "'currency,,,,2'"
   }
END.

{flt-file.atr
   &asgn          = yes
   &a-table       = "'acct'"
   &xcode         = "'class-code'"
   &a-code        = "'class-code'"
   &a-multi       = "Yes"
   &a-label       = "'Класс счета:'"
   &a-help        = "'F1 - справочник классов счетов'"
   &a-procename   = "'getclass'"
   &a-param       = "'?,acct,Yes,R,6'"
}

{flt-file.atr
   &asgn          = yes
   &a-table       = "'acct'"
   &xcode         = "'kau-id'"
   &a-code        = "'kau-id'"
   &a-multi       = "Yes"
   &a-label       = "'Код КАУ:'"
   &a-help        = "'F1 - справочник шаблонов КАУ'"
   &a-procename   = "'kau-temp'"
   &a-param       = "'ШаблКау,ШаблКау,,2'"
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'mPersGr'"
   &a-basic       = "''"
   &a-initial     = "'*'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'SldDate'"
   &a-datatype   = "'DATE'"   
   &a-format     = "'99/99/9999'"
   &a-initial    = "STRING(gend-date)"
   &a-label      = "'Остатки за:'"
   &a-help       = "'Дата расчета остатков'"
   &a-basic      = "''"
   &a-op-update  = "NO"
   &a-multi      = NO
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'SldType'"
   &a-datatype   = "'CHARACTER'"
   &a-view-as    = "'radio-set'"
   &a-list       = "'Д,К,*'"
   &a-val-labels = "'Дебетовое,Кредитовое,Любое'"   
   &a-initial    = "'*'"
   &a-label      = "'Тип сальдо:'"
   &a-help       = "'Тип сальдо'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosVal1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'Сальдо в ин.вал. ОТ:'"
   &a-help       = "'Сальдо по счетам в иностранной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosVal2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'ДО:'"
   &a-help       = "'Сальдо по счетам в иностранной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosBal1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'Сальдо в нац.вал. ОТ:'"
   &a-help       = "'Сальдо по счетам в национальной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosBal2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'ДО:'"
   &a-help       = "'Сальдо по счетам в национальной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'TurnType'"
   &a-view-as     = "'radio-set'"
   &a-datatype   = "'CHARACTER'"   
   &a-list        = "'Д,К,Л,*'"
   &a-val-labels  = "'Дебетовый,Кредитовый,Любой из,Все'"   
   &a-initial    = "'*'"
   &a-label      = "'Тип оборота:'"
   &a-help       = "'Тип оборота(Кр,Дб,Кр или Дб, Кр и Дб)'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'AcctPosStatus'"
   &a-datatype   = "'CHARACTER'"   
   &a-initial    = "'*'"
   &a-label      = "'Статусы док-тов:'"
   &a-help       = "'Учитываемые статусы документов при расчете остатков/оборотов'"
   &a-basic      = "'op-status'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'TurnDate1'"
   &a-datatype   = "'DATE'"   
   &a-format     = "'99/99/9999'"
   &a-label      = "'Обороты С:'"
   &a-help       = "'Начальная дата расчета оборотов'"
   &a-basic      = "''"
   &a-op-update  = "NO"
   &a-initial    = '?'
   &a-multi      = NO
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'TurnDate2'"
   &a-datatype   = "'DATE'"   
   &a-format     = "'99/99/9999'"
   &a-label      = "'ПО:'"
   &a-help       = "'Конечная дата расчета оборотов'"
   &a-basic      = "''"
   &a-op-update  = "NO"
   &a-initial    = '?'
   &a-multi      = NO
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-v1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'Обороты в ин.вал. ОТ:'"
   &a-help       = "'Обороты по счетам в иностранной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-v2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'ДО:'"
   &a-help       = "'Обороты по счетам в иностранной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-b1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'Обороты в нац.вал. ОТ:'"
   &a-help       = "'Обороты по счетам в национальной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-b2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'ДО:'"
   &a-help       = "'Обороты по счетам в национальной валюте'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'beg-datetime1'"
   &a-label      = "'Начало ОТ:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'beg-datetime2'"
   &a-label      = "'Начало ДО:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'end-datetime1'"
   &a-label      = "'Окончание ОТ:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'end-datetime2'"
   &a-label      = "'Окончание ДО:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_blockobject'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'Наличие записей:'"
   &a-help       = "'F1 - изменить значение'"
   &a-format     = "'Нет/Не учитывать'"
   &a-list       = "'Нет,Не учитывать'"
   &a-initial    = "'no'"
}   

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[1]'"
   &a-label      = "'Очередность'"
   &a-op         = "'='"
   &a-procename  = "'pclass'"
   &a-param      = "'order-pay,order-pay,,4'"
   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'v-user-id'"
   &a-basic      = "'user-id'"
   &a-table      = "'blockobject'"
   &a-label      = "'Ответисполнитель'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[2]'"
   &a-label      = "'Код органа'"
   &a-help       = "'Информация об установлении'"
   &a-procename  = "'pclass'"
   &a-param      = "'КонтрОрган,КонтрОрган,,4'"
   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[3]'"
   &a-label      = "'Тип постановления'"
   &a-help       = "'Информация об установлении'"
   &a-procename  = "'pclass'"
   &a-param      = "'Предписания,Предписания,,4'"
   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[4]'"
   &a-label      = "'Постановление'"
   &a-help       = "'Информация об установлении'"
   &a-procename  = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[5]'"
   &a-label      = "'Код органа'"
   &a-help       = "'Информация о снятии'"
   &a-procename  = "'pclass'"
   &a-param      = "'КонтрОрган,КонтрОрган,,4'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[6]'"
   &a-label      = "'Тип постановления'"
   &a-help       = "'Информация о снятии'"
   &a-procename  = "'pclass'"
   &a-param      = "'Предписания,Предписания,,4'"   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[7]'"
   &a-label      = "'Постановление'"
   &a-help       = "'Информация о снятии'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[8]'"
   &a-label      = "'Доп. информация'"
   &a-help       = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'link-code'"
   &a-label      = "'Код связи:'"
   &a-multi      = NO
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'link-direction'"
   &a-label      = "'Объект:'"
   &a-view-as    = 'radio-set'
   &a-list       = 'S,T,?'
   &a-val-labels = "'Выступает источником связи,Прикреплен,Имеет связи'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'link-object-id'"
   &a-basic      = "'link-object-id'"
   &a-table      = "''"
   &a-label      = "'Идентификатор объекта:'"
}

{emptyfld.atr 1}
{emptyfld.atr 2}
{emptyfld.atr 3}
{emptyfld.atr 4}
{emptyfld.atr 5}
{emptyfld.atr 6}
{emptyfld.atr 7}

{flt-file.end}

RETURN.
