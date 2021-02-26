/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: chk-black-list.p
      Comment: Процедура контроля с черным списком документов (black-list)
               клиентов платежного документа
   Parameters: Нет
         Uses:
      Used by:
      Created: 25.05.2015 paus
     Modified: 
*/
{globals.i}
{intrface.get tmess}
{chk-black-list.pro}
DEFINE INPUT PARAMETER iOp     AS INT64 NO-UNDO. /* Внутренний номер документа */
DEFINE INPUT PARAMETER iTypMes AS CHARACTER NO-UNDO. /* Способ сообщения */
DEFINE OUTPUT PARAMETER oRes   AS LOGICAL NO-UNDO. /* Результат проверки */
DEFINE STREAM fil.

 /* Запускаем общую процедуру проверки документа */
 RUN chk-black-all IN THIS-PROCEDURE(iOp, OUTPUT TABLE tt-black-list).

 IF CAN-FIND(FIRST tt-black-list)  THEN DO:
    PUT  UNFORMATTED
      "┌──────────────────────────────┬───────────────┬─────────────┬─────────────────┐" SKIP
      "│         ФИО                  │ Вид документа │Серия и номер│Причина занесения│" SKIP
      '│                              │               │             │в "черный список"│' SKIP.
    
    FOR EACH tt-black-list NO-LOCK:
      PUT UNFORMATTED
      "├──────────────────────────────┼───────────────┼─────────────┼─────────────────┤" skip
                                              "│"
         tt-black-list.fio     FORMAT 'x(30)' "│"
         tt-black-list.typedoc FORMAT 'x(15)' "│"
         tt-black-list.numdoc  FORMAT 'x(13)' "│"
         tt-black-list.cause   FORMAT 'x(17)' "│"
      SKIP.
    END.
    PUT  UNFORMATTED
      "└──────────────────────────────┴───────────────┴─────────────┴─────────────────┘" SKIP.

    oRes = YES. /* устанавливаем флаг совпадения участника операции с black-list */
    oRes = NO.
    IF iTypMes EQ "вопрос" THEN DO:
     /* запрос пользователю */
      RUN Fill-SysMes IN h_tmess("","core61","","").
      IF pick-value = "yes" THEN
         oRes = NO.
    END.
    ELSE IF iTypMes EQ "запрет" THEN
      /* вывод сообщения (в протокол) */
      RUN Fill-SysMes IN h_tmess("","core611","","").
 END.

{intrface.del}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='01/07/2015 14:29:31.716+04:00' */
/* $LINTUSER='paus' */
/* $LINTMODE='1' */
/* $LINTFILE='chk-black-list.p' */
/*prosign+AFp2i+5bo8jUwBn8oDEdg*/