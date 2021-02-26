/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: innsgnch.p
      Comment: Проверка контрольного разряда ИНН плательщика или получателя
               для метода ChkUpd стандартной транзакции
   Parameters: iChWhoseINN - м.б. "inn-send" или "inn-rec",
               iChScreenValue - собственно экранное значение ИНН
      Created: 08.06.2004 10:51 sadm
     Modified: 08.06.2004 10:57 sadm
     Modified: <date> <who>
*/

{intrface.get cust}     /* Библиотека для работы с клиентами. */
{intrface.get tmess}

DEF INPUT PARAMETER iChWhoseINN    AS CHAR NO-UNDO.
DEF INPUT PARAMETER iSurrObj       AS CHAR NO-UNDO. /* Идентификатор объекта. */
DEF INPUT PARAMETER iClass         AS CHAR NO-UNDO. /* Класс объекта. */
DEF INPUT PARAMETER iChScreenValue AS CHAR NO-UNDO.

DEFINE VARIABLE mChCorrectInnSign AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMessage          AS CHARACTER NO-UNDO.

IF LENGTH(iChScreenValue) NE 5 THEN
DO:
   IF NOT (iChScreenValue = "0" OR
           fValidInnSignature(INPUT iChScreenValue, OUTPUT mChCorrectInnSign))
   THEN DO:
      iChWhoseINN = IF iChWhoseINN = "inn-send" THEN
                       "плательщика"
                    ELSE
                       "получателя"
      .
      mMessage = IF {assigned mChCorrectInnSign}
                 THEN SUBSTITUTE("Последние цифры ИНН &1 должны быть: &2",
                                 iChWhoseINN,
                                 QUOTER(mChCorrectInnSign))
                 ELSE SUBSTITUTE("Неверная длина ИНН &1",
                                 iChWhoseINN).
	/*   RUN Fill-SysMes IN h_tmess ("", "", "0", mMessage).*/
      RETURN ERROR mMessage.
   END.
END.
RUN chkstoplistdr.p (iChWhoseINN,iSurrObj,iClass,iChScreenValue).
{intrface.del}          /* Выгрузка инструментария. */ 

RETURN "".
/* $LINTFILE='innsgnch.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:41.926+03:00' */
/*prosign2hyRJoF0Iqp6CHTX5G52qQ*/