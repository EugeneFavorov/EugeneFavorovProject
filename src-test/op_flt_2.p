/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2000 ЗАО "Банковские информационные системы"
     Filename: op_nach.p
      Comment: Генерация проводок по документам, выбранным с помощью фильтра
   Parameters:
         Uses:
      Used by:
      Created: 24/01/00 Om
     Modified: 01/02/00 Om
               Добавление информации об исходном документе в печатную форму.
               Проверка на повторный запуск транзакции.
               Корректировка присвоения исходным документам порядкового номера
     Modified: 16/10/00 Om Короткие транзакции, ввод большого кол-ва документов.
                           Нумерация содаваемых документов.
     Modified: 27/11/00 Om Ошибка:
                                  - system error (40),
                                  - Ускорение группового удаления документов.
     Modified: 21/12/00 Om Доработка:
                                - попытка переводка фильтров к единому
                                  инструментарию.
     Modified: 28/12/00 Om Доработка: исправление ошибок компиляции при
                                      модификации nach2flt.p
     Modified: 04/01/01 Om Доработка: Возможность группировать исходные
                                      документы и получать сумму группы.
     Modified: 01/02/01 Om Доработка: приведения интерфейса фильтов к единому виду.
                           (стандартный ввод инфо, ведомость групп)
     Modified: 07/03/01 Om Доработка: Возможность создавать 1/много документов.
     Modified: 29/03/01 Om Ошибка   : Не формировался счетчик при отсутствии
                                      поля сортировки.
     Modified: 07/09/01 Om Доработка: Формирование одного документа при помощи
                                      поля сортировки.
     Modified: 30.04.2002 16:15 SEMA     по заявке 0006062 Механизм фильтров в допреквизитах шаблонов транзакций
     Modified: 02/06/2002 Om Доработка: Добавлена новая QUERY и
                                        проверка на существование OP.
     Modified: 18/12/02 kraw (0012336) в вызове dpsfltop.p удален лишний пераметр
     Modified: 10/02/03 Olenka (0012527) - добавлены вызовы методов
     Modified: 07/09/01 Om Доработка: Добавлена схема автонумерации "ДокНомер".
     Modified: 01/11/04 laav 35099    Добавлена возможность запуска нескольких транзакций в групповом режиме.
     Modified: 03.07.2007 11:46 KSV      (0078824) Адаптирован для Биссмарт
     Modified: 30.11.2010 14:44 MUTA  0135739 Реализована возможность обрабатывать несколько шаблонов с разными фильтрами
*/

form "~n@(#) OP_FLT.P 1.0 Om 23/09/99"
with frame sccs-id stream-io width 250.

/* Входные переменные */
define input param in-op-date as date  no-undo. /* Дата опер. дня */
define input param oprid      as recid no-undo. /* Recid op-kind */
{globals.i}
{intrface.get tmess}
{chktempl.i}               /* Поиск транзакции и шаблонов */

/*=======================================================================*/
/* Определения */

{g-defs.i} /* Определение вспомогательных переменных */
{a-defs.i}
{def_work.i   new} /* Из all_flt.p */
{flt_var.def  new} /* Из all_flt.i */
{all_note.def new} /* Из all_flt.p Таблица с recid, выбранных по фильтру записей Shared */
{rep_tabl.def new} /* Из all_flt.i Определение индивидуальных таблиц для отчетностей Shared */
{rid_tabl.def new} /* Временная таблица исходных документов для обработки - собственная для opn_flt.p */

{tmprecid.def new} /* Для выбора требуемых записей.*/
&IF DEFINED (oracle) &THEN
   {empty tmprecid}
&ENDIF

{def-wf.i new}
{defframe.i new}  /* Необходим для parssen.p */
{g-docnum.def}    /* Для схем автонумерации. */

/* Буфера */
DEFINE BUFFER src_op       FOR op.
DEFINE BUFFER src_op-entry FOR op-entry.
DEFINE BUFFER xwop         FOR wop. /* для g-currv1.i */
DEFINE BUFFER bOp-template FOR op-template.

/* Локальные переменные */
DEF VAR ope_count  AS INT64     NO-UNDO.
   DEF VAR vOpRecid   AS RECID   NO-UNDO. /* Recid документа */
DEF VAR dval       AS DATE    NO-UNDO. /* Необходимо для parssen.p */
DEF VAR main-first AS LOGICAL INIT YES NO-UNDO. /* Для g-acctv1.i */
DEF VAR fler       AS LOGICAL NO-UNDO. /* Для расчетов */
DEF VAR mParentprocHdl  AS CHAR NO-UNDO. /*хэндл вызвавшей(родительской) процедуры*/
DEFINE  VARIABLE mRVTdRun   AS CHAR      NO-UNDO. /* Резултат процедуры TODAY_RUN. */

/* Для запуска методов */
DEF VAR procname AS CHAR NO-UNDO.
DEF VAR params   AS CHAR NO-UNDO.
DEF VAR ii       AS INT64  NO-UNDO.

DEF VAR f-proc-name LIKE user-config.proc-name NO-UNDO.
DEF VAR f-sub-code  LIKE user-config.sub-code  NO-UNDO.
DEF VAR f-descript  LIKE user-config.descr     NO-UNDO.

cur-op-date = in-op-date.

/*===================================================================*/
/* Библиотеки */

{intrface.get pbase}    /* Базовая библиотека инструментов*/
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get kau}      /* Библиотека для работы с субсчетами. */

                        /* Инициализация процесса протоколирования. */
RUN Init-SysMes (op-kind.op-kind, "", "").

{g-currv1.i}
{currency.def}

DEFINE VAR h_flt AS HANDLE NO-UNDO.
RUN flttool.p PERSISTENT SET h_flt (in-op-date).

/*=================================================================*/

/* Подключение новой библиотеки - др.на транзакции "parslib" */
{plibinit.i}

/*============================================================*/
/* Считываем настройки транзакции */

/* Ввод настроичных параметров транзакции */
RUN LOAD_OP_KIND_SETTING IN h_flt (RECID (op-kind)).
IF RETURN-VALUE NE "" THEN DO:
   MESSAGE RETURN-VALUE
   VIEW-AS ALERT-BOX ERROR BUTTONS OK.

   RUN End-SysMes.         /* Закрыл отчет. */
   {intrface.del}          /* Выгрузка инструментария. */
   RETURN.
END.

/* Проверка на повторный запуск транзакции */
RUN today_run IN h_flt (RECID(op-kind), in-op-date, Yes).
mRVTdRun = RETURN-VALUE.
IF mRVTdRun EQ "-1" THEN DO:
   {setdest2.i}
   RUN disp_err_log IN h_flt.
   {preview2.i}
END.
IF INT64(mRVTdRun) LT 0
THEN DO:
   RUN End-SysMes.         /* Закрыл отчет. */
   {intrface.del}          /* Выгрузка инструментария. */
   RETURN.
END.

DebugParser = INT64(GetXattrValue("op-kind",op-kind.op-kind,"DebugParser"))
              NO-ERROR.

/*============================================================*/
/* Настройки и запрос пользователя */

/*берем и запоминаем хэндл на процедуру, запустившую эту транзакцию*/
mParentprocHdl  = IF GetCallOpkind(2,"PROCHDL") = ?
                  THEN ""
                  ELSE GetCallOpkind(2,"PROCHDL").

RUN setSysConf IN h_base ("NotCheckBlockAcctDb", "").
RUN setSysConf IN h_base ("NotCheckBlockAcctCr", "").
RUN SetSysConf IN h_base ("gLogProc", "?").


OUTPUT TO "./open-err.log".
OUTPUT TO TERMINAL.

/* Ввод начальной информации */
RUN GET_BEG_INFO IN h_flt ("1,1,1,0,0,1,1,1,1" + "," + mParentprocHdl).
IF RETURN-VALUE NE ""
THEN DO:
   RUN setSysConf IN h_base ("NotCheckBlockAcctDb", "").
   RUN setSysConf IN h_base ("NotCheckBlockAcctCr", "").

   RUN End-SysMes.         /* Закрыл отчет. */
   {intrface.del}          /* Выгрузка инструментария. */
   RETURN.
END.

/*
   Ключ к скрытым проводкам!!!
   Номер документа и номер проводки
   сохраняются в отрицательной области целых чисел.
   (зеркальное отражение базы по PU индексу op-entry)
*/
{optr.i &DoBefore=YES}

&GLOBAL-DEFINE OP_HIDDEN        src_op.op * (-1)
&GLOBAL-DEFINE OFSET            500
&GLOBAL-DEFINE OPE_HIDDEN       ope_count + 1  + {&OFSET}

FOR EACH bop-template OF op-kind
          NO-LOCK,
    FIRST signs WHERE
          signs.FILE-NAME EQ "op-template"
      AND signs.surrogate EQ bop-template.op-kind + "," + STRING(bop-template.op-template)
      AND signs.code      EQ 'ФильтрДок'
          NO-LOCK:

  /* Ввод настроичных параметров шаблона */
   RUN LOAD_OP_TMPL_SETTING IN h_flt (RECID (bop-template), 'ФильтрДок').
   IF RETURN-VALUE NE "" THEN DO:
      MESSAGE RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   
      RUN End-SysMes.         /* Закрыл отчет. */
      {intrface.del}          /* Выгрузка инструментария. */
      RETURN.
   END.

   /*==================================================================*/
   /* Обработка фильтра */
   {op_flt.qry}


   /*=================================================================*/
   /* main, цикл по документам */
   
   /*
       Таблица с recid op создана для избежания выбора только
       что созданных документов, подходящих под условия выборки.
   */

NEXT_OP:
   FOR EACH op_rid WHERE
            op_rid.op-template EQ bop-template.op-template, 
   FIRST src_op WHERE
      RECID(src_op) EQ op_rid.op_rid
   NO-LOCK
   BREAK BY op_rid.sort_fld
   TRANSACTION
   ON ERROR  UNDO NEXT_OP, NEXT  NEXT_OP
   ON ENDKEY UNDO NEXT_OP, LEAVE NEXT_OP:

       /*===================================================================*/
       /* обработка в начале итерации  - др ВыпДоИт */
     
       IF all_settings.run_bef_cr > "" and
          SearchPfile(all_settings.run_bef_cr) then do:
     
          run value(all_settings.run_bef_cr + ".p") (op_rid.op_rid).
     
       end.
       /*===================================================================*/
      /* Получение суммы документа. */
      IF FIRST-OF (op_rid.sort_fld) THEN
         ASSIGN
            send-amt   = 0 /* Валютная сумма */
            remove-amt = 0 /* Рублевая сумма */
            bncr       = 0
         .
     
      /* Удаление скрытых проводок по предыдущему документу */
      {wophiden.del}
     
      ope_count = 0.
      FOR EACH src_op-entry OF src_op
         NO-LOCK:
     
         /* Формирование отчета по группе документов. */
         IF all_settings.sort_fld NE ? THEN
            RUN CREATE_SORT_GROUP IN h_flt (
               op_rid.sort_fld,
               BUFFER src_op,
               BUFFER src_op-entry).
     
         /* Создание скрытых исходных документов */
         CREATE wop.
         {wophiden.ass
            &op_pref       = src_
            &op_entry_pref = src_
         }
     
         ope_count = ope_count + 1.
      END. /*   FOR EACH src_op-entry OF src_op */
     
      /* Получение суммы группы документов. */
      FOR EACH wop WHERE wop.op-templ GT {&OFSET}:
         ASSIGN
            send-amt   = send-amt   + wop.amt-cur /* В валюте проводки (Err) */
            remove-amt = remove-amt + wop.amt-rub /* В нац. валюте */
         .
      END.
     
      ASSIGN
         down_op = down_op + 1
         bncr    = bncr    + 1 /* Кол-во исходных документов в группе */
      .
     
      /* Создание документа и проводки */
/*      IF FIRST-OF (op_rid.sort_fld) THEN DO: */
         {op_flt.i
            &op_flt=YES}
/*      END. */
     
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Обработано" down_op "/" tot_rid "исходных документов!".
     
   END.
END.
{optr.i &DoAfter=YES}


IF tot_rid EQ 0
THEN DO:
   MESSAGE "Нет ни одного отобранного документа!"
   VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
   HIDE MESSAGE NO-PAUSE.
   RUN End-SysMes.         /* Закрыл отчет. */
   {intrface.del}          /* Выгрузка инструментария. */ 
   RETURN.
END.

/* Печать ведомостей и сообщений после окончания создания документов */

IF flager NE 9 THEN DO:
{op_flt.prt}
END.

{plibdel.i}
IF VALID-HANDLE (h_flt)
    THEN DELETE PROCEDURE h_flt.

/* перезаписываем sysconf */
RUN setSysConf IN h_base ("gLogMessage", "No")  .
RUN setSysConf IN h_base ("NotCheckBlockAcctDb", "").
RUN setSysConf IN h_base ("NotCheckBlockAcctCr", "").

RUN End-SysMes.         /* Закрыл отчет. */

&IF DEFINED (oracle) &THEN
   {empty tmprecid}
&ENDIF

{intrface.del}          /* Выгрузка инструментария. */
