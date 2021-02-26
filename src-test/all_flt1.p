/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2020 ЗАО "Банковские информационные системы"
     Filename: all_flt.p
      Comment: Генерация проводок по выбранным записям.
   Parameters: дата, транзакция
         Uses:
      Used by:
      Created: 03/03/00 Om
     Modified: 27/11/00 Om Доработка: увеличение скорости группового удаления
                                      документов.
     Modified: 21/12/00 Om Доработка: определение переменных,
                                      объединение инструментария.
     Modified: 28/12/00 Om Доработка: исправление ошибок компиляции, в связи
                                      с правкой nach2flt.p
     Modified: 27/11/01 Om Доработка: обработка плановой даты.
     Modified: 16/05/02    Доработка: добавление запуска методов befor & after transaction.

     Modified: 28/06/02 Илюха:  Небольшые вставки для овердрата
                        (добавлена возможность вазова процедуры обработки отмеченных записей,
                         вешается на доп. реквизит aft-fill-prog транзакции )
     Modified: 05/07/02 Илюха: Избавился от g-func.p
     Modified: 03/09/02 Илюха: Возможность вывода протоколов вынес в доп. реквизиты транзакции,
                               вывод основного протокола доп реквизит print-rep = yes  или не
                               указан для транзакции, возможность вывода "своих" протоколов
                               пусть будет обрабатываться в "своей" процедуре вывода, т.к в ней
                               можно делать некоторые дополнительные действия. Доп. реквизит возможности
                               вывода своих сообщения print-self = "Да" или не указан
                               Добавил проверку на наличие процедур указанных в доп. реквизитах
     Modified: 27/1/2006 mitr tt 0057197 - добавлен вызов RUN NoGarbageCollect IN h_base.
     Modified: 12/7/2006 gorm  (0064663) - убран вызов RUN NoGarbageCollect IN h_base - приводил к ошибкам.
     Modified: 13/9/2006 laav  (0044028) - добалена сборка системных сообщений в темп-таблицу, что позволяет
                                           выводить их в отдельный протокол
.
*/

form "~n@(#) all_flt.p 1.0 Om 03/03/00"
with frame sccs-id stream-io width 250.

&GLOB temp-define YES

/* Входные параметры */
DEF INPUT PARAM in-op-date AS DATE  NO-UNDO. /* Дата опер. дня */
DEF INPUT PARAM oprid      AS RECID NO-UNDO. /* Recid op-kind */

{globals.i}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get loan}
{intrface.get prsfn}
{intrface.get aclog}
{over-def.def new}

RUN NoGarbageCollect IN h_base.

DEF VAR h_flt         AS HANDLE NO-UNDO.
DEF VAR hgar          AS HANDLE NO-UNDO.
DEF VAR nprprog       AS CHAR   NO-UNDO.
DEF VAR l-PrevOpKind  AS CHAR   NO-UNDO.
DEF VAR after         AS CHAR   NO-UNDO.
DEF VAR mAft-fill-prog AS CHAR  NO-UNDO.
DEF VAR mMake-prog    AS CHAR   NO-UNDO.
DEF VAR print-rep     AS LOG    NO-UNDO.
DEF VAR mRVTdRun      AS CHAR   NO-UNDO. /* Резултат процедуры TODAY_RUN. */
DEF VAR quest         AS CHAR   NO-UNDO INIT "false,01/01/1900,false".
DEF VAR mSysMessCntr  AS INT64  NO-UNDO.
DEF VAR mProcname     AS CHAR   NO-UNDO.
DEF VAR mParams       AS CHAR   NO-UNDO.
DEF VAR vEmptyOk      AS LOG    NO-UNDO.
DEF VAR vEmptyOk2     AS LOG    NO-UNDO.
DEF VAR vEmptyOk3     AS LOG    NO-UNDO.
DEF VAR mLogFileName  AS CHAR   NO-UNDO. /* имя лог-файла */
DEF VAR mViewAcctLog  AS LOG    NO-UNDO.
DEF VAR mLdProg       AS CHAR   NO-UNDO.
def var mErr          AS INT64  NO-UNDO.
DEF VAR mAutotest     AS CHAR   NO-UNDO. /* SysConf "AUTOTEST:autotest" при входе в процедуру */

DEF NEW SHARED TEMP-TABLE over NO-UNDO
 FIELD acct           LIKE acct.acct
 FIELD cur            LIKE acct.currency
 FIELD cont-code-agr  LIKE loan.cont-code
 FIELD agr-class-code LIKE loan.class-code
 FIELD cont-code-per  AS CHAR
 FIELD limit          AS DECIMAL
 FIELD bal            AS DECIMAL
 FIELD overtr         AS LOG INIT NO
 .

DEFINE TEMP-TABLE bSysMes  NO-UNDO LIKE tt-SysMes.
DEFINE TEMP-TABLE bProcMes NO-UNDO LIKE tt-ProcMes.

/* Технические дополнительные переменные */
{g-defs.i}
{def_work.i   new} /* Определение таблицы для расчета процентов */
{flt_var.def  new} /* Переменные, общие для всех типов фильтров Shared */
{aux_var.def  new} /* ИНДИВИДУАЛЬНЫЕ переменные, разделенные комментариями */
{all_note.def new} /* Таблица с recid, выбранных по фильтру записей Shared */
{rep_tabl.def new} /* Определение индивидуальных таблиц для отчетностей Shared */
{svarloan.def new}
{def-wf.i new}
/* -------------------------------- ACTIONS -------------------------------- */

{chktempl.i}    /* Поиск транзакции и шаблонов ERROR return */

RUN flttool.p PERSISTENT SET h_flt (in-op-date).
RUN "ga-pfunc.p" PERSISTENT SET hgar.

EMPTY TEMP-TABLE bSysMes.
RUN Get-BufMes(OUTPUT TABLE bProcMes, OUTPUT TABLE bSysMes).

mSysMessCntr = 0.
FOR EACH bSysMes:
   IF bSysMes.Mes-Num > mSysMessCntr
      THEN mSysMessCntr = bSysMes.Mes-Num.
END.

RUN DelLogTbl IN h_aclog.

MainBlock:
DO:
    /* Инициализация библиотеки PBASE
    ** Данный порядок важен (сначала initBaseLibrary затем Init-SysMes), т.к.
    ** в Init-SysMes может использоваться парсер. */
    RUN InitBaseLibrary IN h_pbase (op-kind.op-kind,in-op-date,THIS-PROCEDURE).

    /* Инициализация протокола транзакции */
    RUN Init-SysMes IN h_tmess (op-kind.op-kind,"","").

    /* Сделано для отказа input параметров процедур переделать !!! */
    ASSIGN
        op_rid     = oprid
        in_op_date = in-op-date
    .

    /* Определение типа фильтра */

    IF NOT AvailXattr("op-kind",op-kind.op-kind,"flt_type" ) THEN
    DO:
        RUN Fill-SysMes IN h_tmess ("","","-1","Обязательный реквизит flt_type к транзакции не установлен.").
        LEAVE MainBlock.
    END.

    ASSIGN
        all_settings.flt_type = LC(GetXattrValue("op-kind",op-kind.op-kind,"flt_type" ))
        svPlanDate            = in-op-date
        all_settings.user-res      =  "not-do-op"
        /* выполнение метода before перед выполнением транзакции */
        l-PrevOpKind  = GetXattrValueEx("op-kind", op-kind.op-kind, "PrevOpKind", ?)
        /*процедура ввода исходной информации или затычка для нее*/
        nprprog       = GetXattrValueEx("op-kind", op-kind.op-kind, "nprprog", ?)
        /* процедура получения договоров */
        mLdProg       = GetXattrValueEx("op-kind", op-kind.op-kind, "LdProg", ?)
        /*процедура, вызываемая после заполнения recid выбранных записей*/
        mAft-fill-prog = GetXattrValueEx("op-kind", op-kind.op-kind, "Aft-fill-prog", ?)
        /*своя процедура создания документа*/
        mMake-prog     = GetXattrValueEx("op-kind", op-kind.op-kind, "makeprog", ?)
        /*печатать ли протокол ( стандартные сообщения процедуры all_flt.p ) ?*/
        print-rep     = GetXattrValueEx("op-kind", op-kind.op-kind, "print-rep", "Да") = "Да"
        DebugParser   = INT64(GetXattrValueEx("op-kind",op-kind.op-kind,"debugparser","0"))
        mLogFileName  = "opacct-" + TRIM(op-kind.op-kind) + ".log"
        mViewAcctLog  = LOGICAL(FGetSetting("ViewAcctLog", ?, "NO"))
        all_settings.ZeroTestLog = (IF GetXAttrValueEx("op-kind",
                                                       op-kind.op-kind,
                                                       "ZeroTestLog",
                                                       "Да") EQ "Да"
                                    THEN YES
                                    ELSE NO)
        NO-ERROR
        .
    mAutotest = GetSysConf("AUTOTEST:autotest").
    IF GetXattrValueEx("op-kind", op-kind.op-kind, "СС_ВыводПрткл", "") EQ "Нет" THEN
    DO:
       IF mAutotest NE "YES" THEN
          RUN SetSysConf IN h_base ("AUTOTEST:autotest", "YES").
    END.

    IF nprprog NE ? THEN
    DO:
      IF SEARCH(SUBSTR(nprprog, 1, IF INDEX(nprprog, "((") EQ 0
                                 THEN 255
                                 ELSE INDEX(nprprog, "((") - 1) + ".p") <> ?
      OR SEARCH(SUBSTR(nprprog, 1, IF INDEX(nprprog, "((") EQ 0
                                 THEN 255
                                 ELSE INDEX(nprprog, "((") - 1) + ".r") <> ?
      THEN
      DO:
         RUN VALUE(SUBSTR(nprprog, 1, IF INDEX(nprprog, "((") EQ 0
                                  THEN 255
                                  ELSE INDEX(nprprog, "((") - 1))
               (SUBSTR(nprprog, (IF INDEX(nprprog, "((") EQ 0
                                 THEN ?
                                 ELSE INDEX(nprprog, "((") + 2))) .
         IF KEYFUNCTION(LASTKEY) EQ "END-ERROR"
         THEN
            LEAVE MainBlock.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess ("","","0","Не найдена процедура доп. реквизита before транзакции.").
         LEAVE MainBlock.
      END.
    end.
    {befexpr.i &befopkind = op-kind.op-kind}

    /* метод ввода исходной информации */
    /* временная доработка  для переопределения метода ввода исходной информации - если задан  доп. реквизит - nprprog - он обеспечивает ввод исходной информации
      и возможно делает еще что-то , если не задан, то стандартная работа */
    IF nprprog  = ? THEN
    DO:
       {flt_file.run
           &flt_type="all_settings.flt_type"
           &action_type="frm1"
       }
       svPlanDate            = in-op-date.
      
      IF method_error THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0","Не найдена процедура ввода начальной информации.").
         LEAVE MainBlock.
      END.
      ELSE
         IF RETURN-VALUE EQ "-1" THEN
            LEAVE MainBlock.
    END.
    /* Проверка на повторный запуск транзакции */
    IF NOT {assigned mLdProg} THEN
    DO:
       RUN today_run IN h_flt (RECID(op-kind), in-op-date, YES).
       mRVTdRun = RETURN-VALUE.
       IF mRVTdRun EQ "-1" THEN
       DO:
          {setdest2.i}
          RUN disp_err_log IN h_flt.
          {preview2.i}
       END.
       IF INT64(mRVTdRun) LT 0 THEN
          LEAVE MainBlock.
    END.

    IF {assigned mLdProg} THEN
    DO:
       IF    SEARCH(mLdProg + ".p") <> ? 
          OR SEARCH(mLdProg + ".r") <> ? THEN
       DO:
          RUN VALUE(mLdProg + ".p").
          IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN
             LEAVE MainBlock. 
       END.
       ELSE DO:
           RUN Fill-SysMes IN h_tmess ("","","0",
                                       "Не найдена процедура доп. реквизита LdProc транзакции.").
           LEAVE MainBlock.
       END.    
       RUN RunOp_plus IN THIS-PROCEDURE (OUTPUT mErr).
       IF mErr EQ 1 THEN
          LEAVE MainBlock.    
    END.
    ELSE
    DO:

       /* Загрузка настроек фильтров */
       RUN LoadFltTemplate IN h_flt (op-kind.op-kind,
                                     INPUT-OUTPUT TABLE flt-template,
                                     INPUT-OUTPUT TABLE flt-setup).
       IF NOT CAN-FIND(FIRST flt-setup) THEN
           LEAVE MainBlock.
       FOR EACH flt-setup NO-LOCK BY flt-setup.flt-num:
           ASSIGN
              mFltNum                 = flt-setup.flt-num
              all_settings.flt_user   = flt-setup.user-id
              all_settings.flt_proc   = flt-setup.proc-name
              all_settings.flt_module = flt-setup.sub-code
              all_settings.flt_name   = flt-setup.descr
              all_settings.user-res   =  "not-do-op"
           .

           IF mFltNum > 1 AND all_settings.flt_type = "loan" THEN
              RUN SetSysConf IN h_base ("multiple_loan_filters", "YES").

           /* процедура заполнения recid выбранных записей */
           {flt_file.run
               &flt_type="all_settings.flt_type"
               &action_type="ld"
           }
           IF method_error THEN
           DO:
              RUN Fill-SysMes IN h_tmess ("","","0","Не найдена процедура заполнения выборки.").
              LEAVE MainBlock.
           END.
           ELSE IF RETURN-VALUE EQ "-1" THEN
           DO:
              RUN Fill-SysMes IN h_tmess ("","","0","Не найдена настройка фильтра.").
              LEAVE MainBlock.
           END.
 
           RUN EmptyLPResult IN h_loan  (OUTPUT vEmptyOk).
           RUN EmptyLPResult IN h_prsfn (OUTPUT vEmptyOk2).
           RUN EmptyLPResult IN THIS-PROCEDURE (OUTPUT vEmptyOk3).
           IF NOT vEmptyOk  OR
              NOT vEmptyOk2 OR
              NOT vEmptyOk3 THEN
           DO:
              RUN Fill-SysMes IN h_tmess ("","","0","Произошла ошибка чистки таблицы кеширования данных по выполнению парсерных функций.").
              LEAVE MainBlock.
           END.

           RUN RunOp_plus IN THIS-PROCEDURE (OUTPUT mErr).
           IF mErr EQ 1 THEN
              LEAVE MainBlock.

       END. /* FOR EACH flt-setup */
    END. /* mLdProg */ 
    RUN DeleteOldDataProtocol IN h_base ("multiple_loan_filters").

    /*печатаем протокол если установлен доп. реквизит*/

    EMPTY TEMP-TABLE bSysMes.

    RUN Get-BufMes(OUTPUT TABLE bProcMes, OUTPUT TABLE bSysMes).

    FOR EACH bSysMes WHERE
             bSysMes.mes-num GT mSysMessCntr:

          CREATE loan_qst.
          ASSIGN
          loan_qst.txt     = REPLACE(bSysMes.mes-text,"~n"," ")
          .

       END.

    IF print-rep THEN DO:
      /* процедура печати */
      {flt_file.run
        &flt_type="all_settings.flt_type"
        &action_type="prn"
      }
    END.

    IF method_error THEN
    DO:
       RUN Fill-SysMes IN h_tmess ("","","0","Не найдена процедура печати ведомостей.").
       LEAVE MainBlock.
    END.

    /* Добавить свои процедуры печати
    ** запуск процедуры после выполнения транзакции */
    after = op-kind.after.
    IF {assigned after} THEN
      IF SEARCH(after + '.p') <> ? OR SEARCH(after + '.r') <> ?
      THEN
         RUN VALUE(after + '.p')(op-kind.op-kind) .
      ELSE
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0","Не найдена процедура печати указанная в доп. реквизите after транзакции.").
         LEAVE MainBlock.
      END.
    RUN EmptyLPResult IN THIS-PROCEDURE (OUTPUT vEmptyOk).
    RUN EmptyLPResult IN h_loan  (OUTPUT vEmptyOk).
    RUN EmptyLPResult IN h_prsfn (OUTPUT vEmptyOk2).

    IF mViewAcctLog THEN /* выведем лог */
       RUN PrintLogFile IN h_aclog (mLogFileName).
END.

FINALLY:
   /* чистим лог создания счетов */
   RUN DelLogTbl IN h_aclog.
   /* Сбрасываем контекст транзакции в PBASE */
   RUN InitBaseLibrary IN h_pbase (?,?,?).
   /* Закрываем протоколирование */
   RUN End-SysMes IN h_tmess.
   /* Возвращаем настройку обратно */
   RUN SetSysConf IN h_base ("AUTOTEST:autotest", mAutotest).
 
   PUBLISH 'done'.

   IF VALID-HANDLE (h_flt) THEN
      DELETE PROCEDURE h_flt.
   IF VALID-HANDLE(hgar) THEN
      DELETE PROCEDURE hgar.

   RUN GarbageCollect2 IN h_base.
   {intrface.del}
END FINALLY.

PROCEDURE RunOp_plus:
   DEF OUTPUT PARAM oErr AS INT64 NO-UNDO.
   main:
   do:
       oErr = 1.
       /*запуск процедуры обработки выбранных записей*/
       IF mAft-fill-prog <> ? THEN 
       DO:
          IF INDEX(mAft-fill-prog, "((") > 0 THEN
             ASSIGN
                 mProcname = SUBSTR(mAft-fill-prog, 1, INDEX(mAft-fill-prog, "((") - 1)
                 mParams   = SUBSTR(mAft-fill-prog, INDEX(mAft-fill-prog, "((") + 2)
            .
          ELSE
             ASSIGN
                mProcname = mAft-fill-prog
                mParams   = ""
                 .
      
          IF SearchPfile(mProcname) THEN 
          DO: /* найдена такая процедура */
             ASSIGN
                mParams = STRING(mParams + "," + string(oprid)).
             RUN run_Params in h_xclass (mProcname,mParams,".p",?).
             IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN
                LEAVE Main.
          END.
          ELSE
          DO:
             RUN Fill-SysMes IN h_tmess 
                 ("","","0","Не найдена процедура доп. реквизита mAft-fill-prog транзакции.").
             LEAVE Main.
          END.
          RUN EmptyLPResult IN THIS-PROCEDURE (OUTPUT vEmptyOk).
          RUN EmptyLPResult IN h_loan  (OUTPUT vEmptyOk).
          RUN EmptyLPResult IN h_prsfn (OUTPUT vEmptyOk).
       END.

       IF mMake-prog = ? THEN 
       DO:
          {flt_file.run
             &flt_type="all_settings.flt_type"
             &action_type="op"
          } 
       END.
       /*запускаем свою процедуру создания документов*/
       ELSE IF SEARCH(mMake-prog + ".p") <> ? 
            OR SEARCH(mMake-prog + ".r") <> ? THEN
          RUN VALUE(mMake-prog + '.p').
       ELSE
          method_error = YES.
    
       IF method_error THEN
       DO:
          RUN Fill-SysMes IN h_tmess ("","","0","Не найдена процедура создания документа.").
          LEAVE Main.
       END.
       oErr = 0. 
   end.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='29/02/2016 12:21:13.338+04:00' */
/* $LINTUSER='soav' */
/* $LINTMODE='1' */
/* $LINTFILE='all_flt.p' */
/*prosign0fOGS2ShFJkyAWTgiiUXQQ*/