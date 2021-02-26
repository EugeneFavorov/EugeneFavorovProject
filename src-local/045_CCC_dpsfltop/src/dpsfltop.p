/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2020 ЗАО "Банковские информационные системы"
     Filename: dpsfltop.p
      Comment: Фильтр документов дня
   Parameters: in-cat
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
{globals.i}             /* Глобальные переменные сессии. */
{flt-file.i}            /* Определение структуры динамического фильтра. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

DEFINE INPUT PARAMETER in-cat     LIKE op-entry.acct-cat NO-UNDO.

DEFINE VARIABLE list-class    AS CHARACTER  NO-UNDO. /* список класса и подклассов*/
DEFINE VARIABLE num-class     AS INT64    NO-UNDO. /* N класса */

DEFINE VARIABLE mClassCode    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mProgressCode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE xlist-id      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAccessType   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mProc AS CHARACTER INIT ""   NO-UNDO.
DEFINE VARIABLE mParam1 AS CHARACTER INIT ""   NO-UNDO.
DEFINE VARIABLE mParam2 AS CHARACTER INIT ""   NO-UNDO.

{list-id.i}
mClassCode = in-cat.
IF in-cat BEGINS "op" THEN
   in-cat = "".

mProgressCode = GetXclassProgress(mClassCode).
IF fGetSetting("FltOpByAcct","","") = "ДА" AND mProgressCode EQ "op" THEN
DO:
   mProc   = 'acctlist'  .
   mParam1 = 'db'.
   mParam2 = 'cr'.
END.

&SCOPED-DEFINE pref  X

/* Создание фильтра */
{flt-file.add
   &cat        =  1
   &tablef     =  "'op'"
   &include    =  ""PrintProc,SetFirstFrm,CalcProc,flag-date,cust-role-id,VygprInd,op-date,IsEndDte,1,user-id,user-inspector,sv-10,2,doc-date,due-date,op-value-date,ins-date,order-pay,doc-num,doc-type,acct-cat,op-status,op-kind,class-code,ben-acct,op-error,details,sc-9,sv-9,name-ben,inn,op-transaction,op,UserConf,SrcClass,SrcSurr,File-Name,Surrogate,Title,Return,RetFld,branch-id,InsTrans,DelTrans,mPersGrDb,mPersGrCr,RelQFrames,contract-date,mExDblPO,AllowNoSelection,OpF8F12""
   &sortf      =  ""op-date,user-id,user-inspector,doc-date,due-date,op-value-date,ins-date,order-pay,doc-num,doc-type,acct-cat,op-status,op-kind,class-code,ben-acct,op-error,details,name-ben,inn,contract-date""
   &DOUBLE     =  ""op-date,contract-date""
   &classf     =  "'op' + in-cat"
   &hiddenf    =  ""File-Name,Surrogate,PrintProc,SetFirstFrm,CalcProc,sc-9,flag-date,cust-role-id,VygprInd,UserConf,SrcClass,SrcSurr,Title,IsEndDte,Return,RetFld,InsTrans,DelTrans,mPersGrDb,mPersGrCr,RelQFrames,mExDblPO,OpF8F12""
   &sensitivef =  "'PrintProc,SetFirstFrm,CalcProc,SrcSurr,flag-date,cust-role-id,VygprInd,InsTrans,DelTrans,RelQFrames,mExDblPO'"
}

&IF DEFINED(ORACLE) &THEN       
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'details'"
      &a-basic      = "''"
   }
&ENDIF    

{flt-file.atr
   &asgn         = yes
   &xcode        = "'File-Name'"
   &a-basic      = "'File-Name'"
   &a-table      = "'obj-transaction'"
   &a-class      = "'obj-transaction'"
   &a-label      = "'Объект'"
   &a-code-value = "''"
   &a-initial    = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'Surrogate'"
   &a-basic      = "'Surrogate'"
   &a-table      = "'obj-transaction'"
   &a-class      = "'obj-transaction'"
   &a-label      = "'Идентификатор'"
   &a-code-value = "''"
   &a-initial    = "''"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'Return'"
    &a-initial    = "'field'"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'RelQFrames'"
    &a-code-value = "'14'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mExDblPO'"
   &a-basic      = "''"
   &a-table      = "''"
   &a-class      = "''"
   &a-label      = "'Искл. задвоение PackObject'"
   &a-code-value = "'NO'"
   &a-initial    = "'NO'"
}

{flt-file.add
   &cat     = 2
   &tablef  = "'op-entry'"
   &include = "'value-date,prev-year,op-cod,type,3,acct-flt-new,acct-db,or&and,acct-cr,currency,qty,symbol,amt-cur,amt-rub,kau-db,kau-cr,sc-22,sv-22,op-entry'"
   &sortf   = ""*""
   &DOUBLE  = "'qty,amt-cur,amt-rub'"
   &hiddenf = "'op-entry' + (IF mProgressCode NE 'op' THEN ',acct-flt-new' ELSE '')"
   &classf  = "'op-entry'"
}

{flt-file.atr
   &asgn        = YES
   &xcode       = "'acct-flt-new'"
   &a-table     = "''"
   &a-basic     = "''"
   &a-label     = "'Отбор по проводкам:'"
   &a-datatype = "'LOGICAL'"
   &a-format    = "'По-новому/По-старому'"
   &a-initial   = "'NO'"
   &a-help      = "'Способ фильтрации документов по реквизитам проводок'"
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'kau-cr'"
   &a-op    = "'begins'"
   &a-label = "'КАУ по кредиту:'"
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'kau-db'"
   &a-op    = "'begins'"
   &a-label = "'КАУ по дебету:'"
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'acct-db'"
   &a-procename = mProc
   &a-param     = mParam1
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'acct-cr'"
   &a-procename = mProc 
   &a-param     = mParam2
}

{flt-file.add
   &cat     = 3
   &tablef  = "'op-bank'"
   &labelt  = "'Банки'"
   &include = "'op-bank-type,bank-code-type,bank-code,corr-acct,bank-name'"
   &sortf   = ""*""
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'bank-code'"
   &a-label   = "'Значение:'"
}


{flt-file.atr
   &asgn      = yes
   &xcode     = "'bank-code-type'"
   &a-multi      = "no"
   &a-procename  = "'pclass'"
   &a-param      = "'"КодБанка","КодБанка","ВИДЫ_ИДЕНТИФИКАТОРОВ_БАНКА",2'"
}

{flt-file.add
   &cat     = 4
   &labelt  = "'Импорт/Экспорт'"
   &tablef  = "'op-impexp'"
   &include = "'avail_op-impexp,mail-user-num,op-reference,bank-reference,imp-batch,exp-batch,op-date,imp-date,imp-time,exp-date,exp-time'"
   &DOUBLE  = "'imp-time,exp-time'"
   &sortf   = "'*'"
}

{flt-file.add
   &cat     = 7
   &labelt  = "'Доп.рекв. докум.'"
   &tablef  = "'op'"
   &include = "'sc-5,sv-5,31,sc-31,sv-31,32,sc-32,sv-32,33,sc-33,sv-33,34,sc-34,sv-34'"
}

{flt-file.add
   &cat     = 8
   &labelt  = "'Сообщ. имп./эксп.'"
   &tablef  = "'packet'"
   &include = "'avail_packet,State,AbonentID,PackDate,PackTime,PackError,mail-format,Kind,FileExchID,ClassError'"
   &DOUBLE  = "'PackDate,PackTime'"
   &sortf   = "'*'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'AbonentID'"
   &a-label   = "'Идент-р абонента:'"
}

{flt-file.atr
   &asgn    =  yes
   &xcode   =  "'branch-id'"
   &a-label =  "'Подразделение:'"
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'mPersGrDb'"
   &a-basic       = "''"
   &a-initial     = "'*'"
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'mPersGrCr'"
   &a-basic       = "''"
   &a-initial     = "'*'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'ClassError'"
   &a-label      = "'Классиф-р ошибок:'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'ClassError'"
   &a-help       = "'F1-Браузер класcификаторов ошибок, используемых в транзакциях'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mail-format'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'mail-format'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'Kind'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'Kind'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'PackTime1'"
   &a-view-as = "'time'"
   &a-format  = "'<<<<,<<9'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'PackTime2'"
   &a-view-as = "'time'"
   &a-format  = "'<<<<,<<9'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'State'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'state'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'PackError'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'PackError'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_packet'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'Наличие записей:'"
   &a-help       = "'F1 - изменить значение'"
   &a-format     = "'Нет/Не учитывать'"
   &a-list       = "'Нет,Не учитывать'"
   &a-initial    = "'no'"
}

{flt-file.add
    &cat     = 10
    &tablef  = "'Seance'"
    &labelt  = "'Сеансы имп./эксп.'"
    &include = "'avail_seance,Number,SeanceDate,SeanceOpKind,Direct'"
    &sortf   = "'Number,SeanceDate,op-kind,Direct'"

}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_seance'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'Наличие записей:'"
   &a-help       = "'F1 - изменить значение'"
   &a-format     = "'Нет/Не учитывать'"
   &a-list       = "'Нет,Не учитывать'"
   &a-initial    = "'no'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'Number'"
   &a-help      = "'Номер сеанса'"
   &a-help       = "'F1 - Браузер стандартных транзакций'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'SeanceOpKind'"
   &a-basic      = "'op-kind'"
   &a-label      = "'Транзакция:'"
   &a-help       = "'F1 - Браузер стандартных транзакций'"
}

{flt-file.add
    &cat     = 11
    &tablef  = "'Reference'"
    &labelt  = "'Ссылки имп./эксп.'"
    &include = "'RefClass,RefDate,RefValue'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'RefClass'"
   &a-basic      = "'Class-Code'"
   &a-label      = "'Класс:'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'RefClass'"
   &a-help       = "'F1-Браузер ссылок'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'RefDate'"
   &a-basic      = "'op-date'"
   &a-label      = "'Дата:'"
   &a-help       = "'Дата ссылок'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'RefValue'"
   &a-label      = "'Значение:'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = "'sc-22'"
   &a-class = "'op-entry'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'sv-22'"
   &a-class = "'op-entry'"
}
{flt-file.add
    &cat     = 16
    &tablef  = "'PackObject'"
    &labelt  = "'Связ. сообщ.'"
    &include = "'PackObjKind'"
}                    

{flt-file.atr
   &asgn         = yes
   &xcode        = "'PackObjKind'"
   &a-basic      = "'Kind'"
    &a-label     = "'Вид связи'"
   &a-param      = "'PackObjKind'"
}  

{flt-file.add
   &cat     = 17
   &tablef  = "'xlink'"
   &include = "'link-code,link-direction,link-object-id'"
   &labelt  = "'Связи'"
   &sortf   = "'*'"
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

IF work-module EQ "dps" THEN DO:
   /* Реквизиты данной закладки по-разному обрабатываются
   ** в браузере документов и проводок.
   ** В документах через вычисляемые поля.
   ** В проводках через запрос. */
   {flt-file.add
      &cat     = 5
      &tablef  = "'acct'"
      &labelt  = "'Счета'"
      &include = "'sv-1,sv-2,sv-3,sv-4,sv-7,sv-8,5,sv-6'"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-1'"
      &a-label       =  "'Сводный счет дебета:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'СводнСч,СводнСч,,4'"
      &a-op          =  "'EQ'"
      &a-datatype    =  "'CHARACTER'"
      &a-procename   =  "'codelay'"
      &a-param       =  "'СводнСч,СводнСч,,4'"
   } 
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-2'"
      &a-label       =  "'            кредита:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'СводнСч,СводнСч,,4'"
      &a-op          =  "'EQ'"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-3'"
      &a-label       =  "'Код клиента счета дебета:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'КорпКл,КорпКл,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-4'"
      &a-label       =  "'                 кредита:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'КорпКл,КорпКл,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-7'"
      &a-label       =  "'Назначение счета дебета:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'БалНазн,БалНазн,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-8'"
      &a-label       =  "'                кредита:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'БалНазн,БалНазн,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   /* Следующие поля в браузере проводок обрабатываем как запрос. */
   IF mProgressCode  EQ "op-entry"
   THEN DO:
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-1'"
         &a-table       =  "'dbAcct'"
         &a-class       =  "'acctb'"
         &a-basic-signs =  "'СводнСч'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-2'"
         &a-table       =  "'crAcct'"
         &a-class       =  "'acctb'"
         &a-basic-signs =  "'СводнСч'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-3'"
         &a-table       =  "'dbAcct'"
         &a-class       =  "'acct'"
         &a-basic-signs =  "'КорпКл'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-4'"
         &a-table       =  "'crAcct'"
         &a-class       =  "'acct'"
         &a-basic-signs =  "'КорпКл'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-7'"
         &a-basic       =  "'contract'"
         &a-table       =  "'dbAcct'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-8'"
         &a-basic       =  "'contract'"
         &a-table       =  "'crAcct'"
      }
   END.

   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-6'"
      &a-label       =  "'Экспортировать документ:'"
      &a-help        =  "'Да - экспортировать, Нет - не экспортировать, ? - все'"
      &a-datatype    =  "'CHARACTER'"
      &a-format      =  "'Да/Нет'"
      &a-table       =  "'op'"
      &a-class       =  "'op' + in-cat"
      &a-code-value  =  "'?'"
      &a-initial     =  "'?'"
      &a-basic-signs =  "'Экспорт'"
   }
   {emptyfld.atr 5}
END.

IF work-module EQ "incass" THEN
DO:

      {flt-file.add
         &cat        = 9
         &tablef     = "'op'"
         &labelt     = "'Накладные'"
         &include    = "'loan-id,type-client,num-client,inc-acct,sum_nac,n_marsh,n_marsh_id,n_baul,pr_rur_cur,pr_evncash'"
         sensitivef  = "'class-code'"
      }


      {flt-file.atr
         &asgn         = yes
         &xcode        = "'loan-id'"
         &a-datatype   = "'character'"
         &a-label      = "'Номер договора:'"
         &a-help       = "'Номер договора на инкасацию'"
         &a-format     = "'x(9)'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'loaninc'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'type-client'"
         &a-datatype   = "'character'"
         &a-label      = "'Тип клиента:'"
         &a-help       = "'Тип клиента'"
         &a-format     = "'x(1)'"
         &a-basic      = "''"

      }  

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'num-client'"
         &a-datatype   = "'integer'"
         &a-format     = "'>>>>>>>9'"
         &a-label      = "'Клиент №:'"
         &a-help       = "'Клиент №'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'num-client'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'inc-acct'"
         &a-datatype   = "'character'"
         &a-format     = "'x(20)'"
         &a-label      = "'Расчетый счет:'"
         &a-help       = "'Расчетый счет'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'acct'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'sum_nac'"
         &a-datatype   = "'decimal'"
         &a-format     = "'>>>>>>>>9.99'"
         &a-label      = "'Сумма накладной:'"
         &a-help       = "'Сумма накладной'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'sum_nac'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'n_marsh'"
         &a-datatype   = "'integer'"
         &a-format     = "'>>>>>9'"
         &a-label      = "'Номер маршрута (заезда):'"
         &a-help       = "'Номер маршрута (заезда)'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'marsh'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'n_marsh_id'"
         &a-datatype   = "'character'"
         &a-format     = "'x(8)'"
         &a-label      = "'Идент. типового маршрута:'"
         &a-help       = "'Идентификатор типового маршрута'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'marsh_id'"
      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'n_baul'"
         &a-datatype   = "'character'"
         &a-format     = "'x(12)'"
         &a-label      = "'Номер сумки:'"
         &a-help       = "'Номер сумки'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'baul'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'pr_rur_cur'"
         &a-datatype   = "'logical'"
         &a-format     = "'Руб/Вал'"
         &a-initial    = "'yes'"
         &a-label      = "'Тип договора:'"
         &a-help       = "'Тип договора'"
         &a-basic      = "''"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'pr_evncash'"
         &a-datatype   = "'logical'"
         &a-format     = "'Да/Нет'"
         &a-initial    = "'?'"
         &a-label      = "'Вечерняя касса:'"
         &a-help       = "'Вечерняя касса'"
         &a-basic      = "''"

      }

    /* Изменения или дополнения фильтра */


END.


{emptyfld.atr 1}
{emptyfld.atr 2}
{emptyfld.atr 3}
{emptyfld.atr 31}
{emptyfld.atr 32}
{emptyfld.atr 33}
{emptyfld.atr 34}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-transaction'"
   &a-initial    = '0'
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'class-code'"
   &a-label      = "'Класс документа'"
   &a-label      = "'Класс документа'"
   &a-procename  = "'getclass'"
   &a-param      = "'?,op,Yes,R,2'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'or&and'"
   &a-basic      = "''"
   &a-label      = "'И/Или:'"
   &a-help       = "'Или - либо в дебете либо в кредите, И - и в дебете и в кредите'"
   &a-datatype   = "'logical'"
   &a-format     = "'И/Или'"
   &a-table      = "'op-entry'"
   &a-code-value = "'yes'"
   &a-initial    = "'yes'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sv-10'"
   &a-basic      =  "''"
   &a-label      = "'Доступ по:'"
   &a-help       = "'Счет - доступ по счетам, Документ - доступ по документам'"
   &a-datatype   = "'logical'"
   &a-format     = "'Счетам/Документам'"
   &a-list       = "'Счетам,Документам'"
   &a-code-value = "'no'"
   &a-initial    = "'no'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sv-9'"
   &a-label      = "'Весь документ:'"
   &a-help       = "'Да - весь документ, Нет - не весь документ'"
   &a-datatype   = "'logical'"
   &a-format     = "'Да/Нет'"
   &a-table      = "'op'"
   &a-code-value = "'no'"
   &a-initial    = "'no'"
}

IF mClassCode EQ "opb-card" THEN
DO:
{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-date1'"
   &a-code-value = STRING(gbeg-date)
   &a-initial    = "'01/01/1901'"
   &a-sort       = YES
   &a-sort-order = 1
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-date2'"
   &a-code-value = STRING(gend-date)
   &a-initial    = "'01/01/3001'"
}
END.
ELSE
DO:
{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-date1'"
   &a-code-value = STRING(gbeg-date)
   &a-initial    = STRING(gbeg-date)
   &a-sort       = YES
   &a-sort-order = 1
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-date2'"
   &a-code-value = STRING(gend-date)
   &a-initial    = STRING(gend-date)
}
END.

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-entry'"
   &a-initial    = "'?'"
}


{flt-file.atr
    &asgn         = yes
    &xcode        = "'IsEndDte'"
    &a-datatype   = "'logical'"
    &a-label      = "'Без учета дат:'"
    &a-help       = "'F1 - изменить значение'"
    &a-format     = "'Да/Нет'"
    &a-initial    = "'no'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'acct-cat'"
   &a-code-value = in-cat
   &a-initial    = "''"
   &a-sensitive  = "if in-cat = """" then yes else no"
}


{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-status'"
   &a-code-value = rightview
   &a-initial    = rightview
   &a-list       = rightview
   &a-sort       = YES
   &a-sort-order = 2
}
/* > 0133808  */
IF LOOKUP(list-id,'*') EQ 0 THEN xlist-id = "%СПИСОК_ПОДЧИНЕННЫХ%" + "," + list-id.
                            ELSE xlist-id = list-id.
/* < 0133808  */
{flt-file.atr
   &asgn         = YES
   &xcode        = "'user-id'"
   &a-label      = 'Сотрудники:'
   &a-sort       = YES
   &a-sort-order = 3
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'user-inspector'"
   &a-initial    = "'*'"
   &a-label      = 'Контролеры:'
}
                        /* Записываем список подчиненных */
{&pref}flt-attr.attr-code-value = list-id NO-ERROR.
                        /* Если он не помещается в запись, то "*" */
IF ERROR-STATUS:ERROR THEN
   {&pref}flt-attr.attr-code-value = "*".

{flt-file.atr
   &asgn         = YES
   &xcode        = "'op'"
   &a-sort       = YES
   &a-sort-order = 4
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-error'"
   &a-op         = "'matches'"
}

IF in-cat EQ "d" THEN DO:
   {flt-file.atr
      &asgn    = yes
      &xcode   ='"currency"'
      &a-label ='"Код ЦБ:"'
    }
END.

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_op-impexp'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'Наличие записей:'"
   &a-help       = "'F1 - изменить значение'"
   &a-format     = "'Нет/Не учитывать'"
   &a-list       = "'Нет,Не учитывать'"
   &a-initial    = "'no'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = "'op-reference'"
   &a-label = "'Референс в банке:'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'bank-reference'"
   &a-label = "'Референс в РЦ:'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'op-date'"
   &a-code  = "'op-date3'"
   &a-label = "'Дата опер.дня:'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'imp-date'"
   &a-label = "'Дата импорта:'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'imp-time1'"
   &a-label   = "'Время импорта ОТ:'"
   &a-view-as = "'time'"
}
{flt-file.atr
   &asgn      = yes
   &xcode     = "'imp-time2'"
   &a-label   = "'Время импорта ДО:'"
   &a-view-as = "'time'"
}
{flt-file.atr
   &asgn      = yes
   &xcode     = "'exp-time1'"
   &a-label   = "'Время экспорта ОТ:'"
   &a-view-as = "'time'"
}
{flt-file.atr
   &asgn      = yes
   &xcode     = "'exp-time2'"
   &a-label   = "'Время экспорта ДО:'"
   &a-view-as = "'time'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = "'exp-date'"
   &a-label = "'Дата экспорта:'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = '"ins-date"'
   &a-label = '"Дата поступления:"'
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'flag-date'"
   &a-code-value = "'yes'"
   &a-initial    = "'yes'"
   &a-basic      = "'op-date'"
}

/* SetFirstFrm  - при использовании переопределяет номер первого
                  показываемого фрейма.
                  Формат - число от 0 до 5
                  (при указании 0 - идет стандартная обработка,
                   в ином случае указывается конкретный номер фрейма)
*/
{flt-file.atr
   &asgn         = yes
   &a-table      = "'op'"
   &xcode        = "'SetFirstFrm'"
   &a-datatype   = "'integer'"
   &a-format     = ""9""
   &a-initial    = "'0'"
   &a-code-value = "'0'"
   &a-basic      = "'op-date'"
}

/* PrintProc  - при использовании определяет процедуру печати, вызываемую по Ctrl-G
                успользуется для вывода протокола расчета в 402 форме */
{flt-file.atr
   &asgn          =  YES
   &xcode         =  "'PrintProc'"
   &a-initial     =  "''"
}

/* CalcProc  - при использовании определяет процедуру +*/
{flt-file.atr
   &asgn         = yes
   &xcode        = "'CalcProc'"
   &a-initial    = "''"
   &a-code-value = "''"
}

/* Title - дополнение к стандартному заголовку списка */
{flt-file.atr
   &asgn         = yes
   &xcode        = "'Title'"
   &a-initial    = "''"
}

{flt-file.atr
   &asgn       = yes
   &xcode      = "'sc-9'"
   &a-multi    = yes
}

IF mProgressCode EQ "op" THEN
DO:
   {flt-file.add
      &cat     = 6
      &labelt  = "'Журнал изменений'"
      &tablef  = "'history'"
      &include = "'modify,modif-merge,modif-date,modif-time,user-id1'"
      &DOUBLE  = "'modif-date,modif-time'"
      &sortf   = "'modify,modif-date,modif-time,user-id1'"
   }
   FOR EACH flt-attr WHERE flt-attr.attr-code BEGINS "Modif-Time":
      flt-attr.attr-view-as    = "time".
   END.
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'modify'"
      &a-procename  = "'sel-list'"
      &a-param      = "'c;w,modify'"
      &a-help       = "'Вид изменения (c - создание, w - модификация)'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'modif-merge'"
      &a-basic      = "''"
      &a-datatype   = "'logical'"
      &a-label      = "'Дату и время'"
      &a-help       = "'F1 - изменить значение'"
      &a-format     = "'Объединять/Не объединять'"
      &a-list       = "'Объединять,Не объединять'"
      &a-initial    = "'no'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'user-id1'"
      &a-basic      = "'user-id'"
      &a-label      = "'Сотрудник'"
      &a-help       = "'Код сотрудника, вносившего изменения'"
   }

END.

{flt-file.add
      &cat        =  12
      &labelt     =  "'Связ. субъекты'"
      &tablef     =  "'cust-role'"
      &include    =  "'CRClass,cust-cat,cust-id,35,open-date,36,close-date'"
      &sortf      =  "'*'"
      &double     =  "'open-date,close-date'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'CRClass'"
   &a-basic      = "'Class-Code'"
   &a-label      = "'Роль:'"
   &a-help       = "'Роль субъекта'"
   &a-procename  = "'clascode'"
   &a-param      = "'cust-role,op-cust,4'"

}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'cust-cat'"
   &a-basic      = "'cust-cat'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'cust-id'"
   &a-basic      = "'cust-id'"
}


{flt-file.atr
   &asgn         = YES
   &xcode        = "'open-date1'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Начало ОТ:'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'open-date2'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Начало ДО:'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'close-date1'"
   &a-basic      = "'close-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Окончание ОТ:'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'close-date2'"
   &a-basic      = "'close-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Окончание ДО:'"
   &a-procename  = "'calend'"
}

{flt-file.add
      &cat        =  13
      &labelt     =  "'Нарушения'"
      &tablef     =  "'cstrlopentry'"
      &classf     =  "'cstrlopentry'"
      &include    =  "'avail_cstrlopentry,VClass,v-cust-cat,v-cust-id,37,v-open-date,38,issue-date'"
      &sortf      =  "'*'"
      &double     =  "'v-open-date,issue-date'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'VClass'"
   &a-basic      = "'Class-Code'"
   &a-label      = "'Роль:'"
   &a-help       = "'Роль субъекта'"
   &a-procename  = "'clascode'"
   &a-param      = "'cust-role,opent-cust,4'"

}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_cstrlopentry'"
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
   &xcode        = "'v-cust-cat'"
   &a-basic      = "'cust-cat'"
   &a-label      = "'Тип клиента:'"
   &a-help       = "'Тип клиента'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'v-cust-id'"
   &a-basic      = "'cust-id'"
   &a-label      = "'Клиент N:'"
   &a-help       = "'Порядковый номер клиента'"

}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'v-open-date1'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Дата выявления ОТ:'"
   &a-help       = "'Дата выявления нарушения'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'v-open-date2'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Дата выявления ДО:'"
   &a-help       = "'Дата выявления нарушения'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'issue-date1'"
   &a-basic      = "'issue-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Дата нарушения ОТ:'"
   &a-help       = "'Дата нарушения'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'issue-date2'"
   &a-basic      = "'issue-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'Дата нарушения ДО:'"
   &a-help       = "'Дата нарушения'"
   &a-procename  = "'calend'"
}

IF work-module = "pn" THEN DO:
   FIND FIRST flt-attr WHERE flt-attr.attr-code = "due-date" EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAIL flt-attr THEN DELETE flt-attr.

   {flt-file.add
      &cat        =  14
      &tablef     =  "''"
      &labelt     =  "'Платежи населения'"
      &include    =  "'due-date,mPNIsTrans,mPNCodeParam,mPNNaimParam,mPNReceiver,mPNAcct,mPNSum'"
      &sortf      =  "'due-date,mPNIsTrans,mPNCodeParam,mPNNaimParam,mPNReceiver,mPNAcct,mPNSum'"
      &double     =  "'due-date,mPNSum'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'due-date1'"
      &a-basic      = "'due-date'"
      &a-table      = "'op'"
      &a-class      = "''"
      &a-label      = "'Дата платежа ОТ:'"
      &a-initial    = "'?'"
      &a-datatype   = "'date'"
      &a-format     = "'99/99/9999'"
      &a-help       = "'Дата платежа'"
   }
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'due-date2'"
      &a-basic      = "'due-date'"
      &a-table      = "'op'"
      &a-class      = "''"
      &a-label      = "'Дата платежа ДО:'"
      &a-initial    = "'?'"
      &a-datatype   = "'date'"
      &a-format     = "'99/99/9999'"
      &a-help       = "'Дата платежа'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNIsTrans'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'Перечислен:'"
      &a-initial    = "'?'"
      &a-datatype   = "'logical'"
      &a-format     = "'Да/Нет'"
      &a-help       = "'Платеж перечислен или нет'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNCodeParam'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'Вид платежа:'"
      &a-initial    = "'*'"
      &a-procename  = "'browseldvar'"
      &a-param      = "'code,ПНВидплатежа,class,ПНВидплатежа,class,4'"
      &a-help       = "'Вид платежа'"
   }
  
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNNaimParam'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'Наименование:'"
      &a-initial    = "'*'"
      &a-procename  = "'browseldvar'"
      &a-param      = "'name,ПНВидплатежа,class,ПНВидплатежа,class,4'"
      &a-help       = "'Наименование вида платежа'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNReceiver'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'Получатель:'"
      &a-initial    = "'*'"
      &a-procename  = "'pn-sel-recv'"
      &a-param      = "'NO,4'"
      &a-help       = "'Получатель'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNAcct'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'Счет получателя:'"
      &a-initial    = "'*'"
      &a-procename  = "'browseldvar'"
      &a-param      = "'acct,loan-acct-pn,acct-type,ПлатНас,acct-type,4'"
      &a-format     = "'x(20)'"
      &a-help       = "'Счет получателя'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNSum1'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'Сумма ОТ:'"
      &a-initial    = "'0.00'"
      &a-datatype   = "'decimal'"
      &a-format     = "'>>>,>>>,>>9.99'"
      &a-help       = "'Сумма платежа'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNSum2'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'Сумма ДО:'"
      &a-initial    = "'0.00'"
      &a-datatype   = "'decimal'"
      &a-format     = "'>>>,>>>,>>9.99'"
      &a-help       = "'Сумма платежа'"
   }

END.

/* 0174879 AllowNoSelection  - Запрет создания tmprecid в tmprecid.cqr по Ctrl+Enter */
{flt-file.atr
   &asgn         = YES
   &a-table      = "'op'"
   &xcode        = "'AllowNoSelection'"
   &a-datatype   = "'LOGICAL'"
   &a-format     = "'YES/NO'"
   &a-label      = "'Режим просмотра'"
   &a-initial    = "'NO'" 
   &a-hidden     = YES
}


/* Вкладка группы только для админов */
/*IF IsUserAdm(USERID("bisquit")) THEN*/
/*DO:                                 */
   {flt-file.add
      &cat     = 15
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
      &a-basic       = "''"
      &a-label       = "'Отбирать счета принадл.:'"
      &a-help        = "'Список групп доступа лицевых счетов (в формате CAN-DO)'"
      &a-view-as     = "'RADIO-SET'"
      &a-list        = "'any,all'"
      &a-val-labels  = "'Любой из указанных групп,Только указанным группам'"
      &a-initial     = "'any'"
      &a-code-value  = "'any'"
   }
/*END.*/

{emptyfld.atr 35}
{emptyfld.atr 36}
{emptyfld.atr 37}
{emptyfld.atr 38}

{flt-file.end}

            /* ДР ДостДокПоФильтр (если он не задан на пользователе, берется его
            ** начальное значение с метасхемы) определяет, каким значением будет
            ** предустанавливаться поле "Доступ по" браузеров документов и проводок.
            ** Если ДР не задан (на пользователе и пустое начальное значение), то
            ** значение поля определяется правилом: Если пользователь имеет права
            ** на просмотр групп счетов, устанавливается доступ "по счетам", иначе
            ** остается значение по-умолчанию (сейчас "по документам"). Если задано
            ** значение ДР, то поле фильтра устанавливается в заданное значение. */
mAccessType = GetXAttrValueEx ("_user", USERID("bisquit"), "ДостДокПоФильтр", GetXAttrInit ("_user","ДостДокПоФильтр")).
IF    (mAccessType              EQ ""
   AND GetRightAcctGroups ("r") NE "")
    OR mAccessType              EQ "Счетам"
THEN
   RUN SetFltField ('sv-10','yes').
IF mAccessType EQ "Документам" THEN
   RUN SetFltField ('sv-10','no').

RETURN.
/* $LINTFILE='dpsfltop.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='13/04/2016 11:05:00.693+04:00' */
/*prosignMS49Sjpdxcm+8wpO6ZJ/7A*/