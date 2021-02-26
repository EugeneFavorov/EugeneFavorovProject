/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ext-acctpos.p
      Comment: TT:0259807 Миграция. 365-П предоставление общей выписки по счетам ТФ и ОФ
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS Переходник для выгрузки данных из внешней системы.
                                      Обороты/остатки по счету.
     Modified: 
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
{extexch.def}

DEF INPUT  PARAM iBegDate  AS  DATE  NO-UNDO.   /* Дата начала выгрузки                          */
DEF INPUT  PARAM iEndDate  AS  DATE  NO-UNDO.   /* Дата окончания выгрузки                       */
DEF INPUT  PARAM iAllFil   AS  LOG   NO-UNDO.   /* По всем филиалам или только текущий           */
DEF INPUT  PARAM TABLE     FOR ttExtAcct.       /* Таблица по найденным счетам                   */
DEF OUTPUT PARAM oAmtIn    AS  DEC   NO-UNDO.   /* Входящий остаток                              */
DEF OUTPUT PARAM oAmbDb    AS  DEC   NO-UNDO.   /* Обороты по дебету                             */
DEF OUTPUT PARAM oAmtCr    AS  DEC   NO-UNDO.   /* Обороты по кредиту                            */
DEF OUTPUT PARAM oAmt      AS  DEC   NO-UNDO.   /* Исходящий остаток                             */

DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

IF NOT CONNECTED("bank")
  THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

RUN ext-acctpos_.p( iBegDate, iEndDate, iAllFil, INPUT TABLE ttExtAcct, 
    OUTPUT oAmtIn, OUTPUT oAmbDb, OUTPUT oAmtCr, OUTPUT oAmt).


CATCH eAnyError AS Progress.Lang.Error:
    message RETURN-VALUE + " " + eAnyError:GetMessage(1) view-as alert-box.
    RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
END CATCH.
END.
