/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: summ-t.p
      Comment: Расчет остатка задолженности

   Parameters: нет
         Uses:
      Used by:
      Created: ??/??/???? ???
      Last change:  SXS  25 Mar 2002    4:59 pm
     Modified:  11/12/02 Lera обязательства должны рассчитываться всегда относительно даты отчета,
                              даже если договор закрыт. при закрытии договора проставляется
                              дата закрытия обязательства и тогда она никогда не учитывается
                              даже если отчет строится на дату когда договор еще не закрыт.
     Modified: 15/03/2004 Илюха  - При расчете суммы на довыяснение при
                                   пересчете договора учитываем только что
                                   прошедшее погашение обязательства, т.к еще
                                   не прошел автоматический вынос на довыяснение
     Modified: 19/03/2004 Илюха  - При расчете вызове расчете разерва через olap,
                                   если договор закрыт (руками в день пересчета или
                                   по  F8 в списке) и дата закрытия меньше
                                   даты отчета , то расчитываем параметры
                                   без разноски и считаем что обязательства
                                   закрытые датой закрытия договора пока
                                   открыты (вот блин маза).
     Modified: 14/03/2004 Илюха  - (19283) Еще одна тупая затычка для отчетности
     Modified: 26/04/2004 Илюха  - (22988) оПЯТЬ одна тупая затычка для отчетности,
                                   хотя от затычки 19.03.04 избавился
     Modified: 23.04.2008 jadv - (75633) подъем из спецверсии заявки 57948
     Modified: 23.04.2008 ches - (0161696) ! скорректирована работа с закрытыми обязательствами (0161969 -date).
                                            Убрала ветку с GetSysConf("УчитыватьПогашенияОбязательств")
                     Выборка
                      (xerm-obl.sop-date  = ?
                       OR xerm-obl.sop-date >= dat-otch
                       OR (mOlapClose AND xerm-obl.sop-date = loan.close-date))
                       теперь означает:  НЕЗАКРЫТЫЕ ОПВ на дату отчета и закрытые в дату закрытия договора.


*/

{globals.i}
{intrface.get loan}
{intrface.get xclass}

DEF OUTPUT PARAM summ-t  LIKE term-obl.amt-rub   NO-UNDO.
DEF INPUT  PARAM incontr LIKE loan.contract  NO-UNDO.
DEF INPUT  PARAM in-code LIKE loan.cont-code NO-UNDO.
DEF INPUT  PARAM l         AS RECID          NO-UNDO.    /* обязательство */
DEF INPUT  PARAM dat-otch  AS DATE           NO-UNDO.    /* дата отчета */

DEF VAR i1          AS INT64              INIT 0 NO-UNDO.
DEF VAR j1          AS INT64              INIT 0 NO-UNDO.
DEF VAR mParamOst LIKE loan-var.balance INIT 0 NO-UNDO.
DEF VAR mTermOst  LIKE loan-var.balance INIT 0 NO-UNDO.
DEF VAR mDiffOst  LIKE loan-var.balance INIT 0 NO-UNDO.

DEF VAR dat-r      AS DATE NO-UNDO .
DEF VAR mDb        AS DEC  NO-UNDO.
DEF VAR mCr        AS DEC  NO-UNDO.
DEF VAR mOst0      AS DEC  NO-UNDO.
DEF VAR mOst7      AS DEC  NO-UNDO.
DEF VAR mOst13     AS DEC  NO-UNDO.
DEF VAR mOst47     AS DEC  NO-UNDO.
DEF VAR mOlapClose AS LOG  NO-UNDO.
DEF VAR mSummDate  AS DATE NO-UNDO.
DEF VAR modeOfCalc AS LOG NO-UNDO.
DEF VAR mModeStr   AS CHAR NO-UNDO.
DEF VAR mIdnt LIKE term-obl.idnt NO-UNDO.

DEF BUFFER xerm-obl    FOR term-obl.
DEF BUFFER tt-term-obl FOR term-obl.


FIND FIRST term-obl WHERE RECID(term-obl) EQ l NO-LOCK NO-ERROR.

mIdnt = term-obl.idnt.

FIND FIRST loan WHERE loan.contract    = incontr AND
                      loan.cont-code   = in-code NO-LOCK NO-ERROR.

/*{{{ Режим расчета - С/без отсрочки погасительных платежей по основному долгу  */
mModeStr = GetSysConf ("РежимОтсрПлатежа").
IF mModeStr EQ ? THEN
   mModeStr = getXAttrValueEx("loan",
                              loan.contract + "," + loan.cont-code, 
                              "РежимОтсрПлатежа", ?).
IF mModeStr EQ ? THEN
   mModeStr = GetXattrInit (loan.class-code, "РежимОтсрПлатежа").

ASSIGN
   modeOfCalc = CAN-DO("да,yes,true,ok", mModeStr).
   mOlapClose =      GetSysConf("ЗакрытыйДоговор") = "Да" 
                AND  loan.close-date <> ?
.

IF term-obl.sop-date <> ?        AND
   term-obl.sop-date <  dat-otch AND
   NOT mOlapClose                THEN
   summ-t = 0.
ELSE
DO:

   RUN RE_PARAM_EX IN h_Loan (0,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst0,
                              OUTPUT mDb,
                              OUTPUT mCr ).


   RUN RE_PARAM_EX IN h_Loan (7,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst7,
                              OUTPUT mDb,
                              OUTPUT mCr).

   IF GetSysConf("РасчетЗадолженностиПоСрокам") = "Да" THEN
      FOR EACH loan-int OF loan WHERE
               loan-int.id-d = 7
           AND loan-int.id-k = 0
           AND loan-int.mdate = dat-otch
      NO-LOCK:
         mOst7 = mOst7 - loan-int.amt-rub.
      END.

   RUN RE_PARAM_EX IN h_Loan (13,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst13,
                              OUTPUT mDb,
                              OUTPUT mCr).

   RUN RE_PARAM_EX IN h_Loan (47,
                              dat-otch,
                              loan.since,
                              incontr,
                              in-code,
                              OUTPUT mOst47,
                              OUTPUT mDb,
                              OUTPUT mCr).
   mParamOst = mOst0 + mOst7 + mOst13 + mOst47.

   IF mParamOst > 0 THEN
   DO:
      FOR EACH xerm-obl WHERE
               xerm-obl.contract  = loan.contract
           AND xerm-obl.cont-code = loan.cont-code
           AND xerm-obl.idnt      = mIdnt
      NO-LOCK :
         IF xerm-obl.sop-date  = ? OR
            xerm-obl.sop-date >= dat-otch OR
            (mOlapClose AND xerm-obl.sop-date = loan.close-date)
         THEN
            ACCUM xerm-obl.amt-rub (TOTAL).
      END.

      mTermOst =  ACCUM TOTAL xerm-obl.amt-rub .

      RELEASE tt-term-obl.

      /* вот тут была ветка - но ее зарезали  */

         mDiffOst = mTermOst - mParamOst.
         
          /* режим с отсрочкой платежа */
         IF modeOfCalc THEN DO:
             FOR EACH tt-term-obl WHERE
                      tt-term-obl.contract  = loan.contract
                 AND  tt-term-obl.cont-code = loan.cont-code
                 AND  tt-term-obl.idnt      = mIdnt
                 AND (tt-term-obl.sop-date  = ?
                   OR tt-term-obl.sop-date >= dat-otch
                   OR (mOlapClose AND tt-term-obl.sop-date = loan.close-date))
        
                NO-LOCK
                BREAK BY tt-term-obl.end-date WHILE mDiffOst > 0:
        
                IF RECID(tt-term-obl) = RECID(term-obl) THEN LEAVE.
        
                mDiffOst = mDiffOst - tt-term-obl.amt-rub.
             END.
            
         END.
         /* режим без отсрочки платежа */
         ELSE DO:
             /* вычитаем плановые платежи из суммы оплаты до даты отчета 
             ** или до плановой даты платежа (если она раньше) */
             FOR EACH tt-term-obl WHERE
                      tt-term-obl.contract  = loan.contract
                 AND  tt-term-obl.cont-code = loan.cont-code
                 AND  tt-term-obl.idnt      = mIdnt
                 AND  tt-term-obl.end-date <= dat-otch
                 AND (tt-term-obl.sop-date  = ?
                   OR tt-term-obl.sop-date >= dat-otch
                   OR (mOlapClose AND tt-term-obl.sop-date = loan.close-date))
        
                NO-LOCK
                BREAK BY tt-term-obl.end-date WHILE mDiffOst > 0:
        
                IF RECID(tt-term-obl) = RECID(term-obl) THEN LEAVE.
        
                mDiffOst = mDiffOst - tt-term-obl.amt-rub.
             END.
             
            /* Если даты планового платежа больше даты отчета и еще осталось 
            ** что-то от оплаты */
            IF mDiffOst > 0 AND
               term-obl.end-date > dat-otch 
            THEN DO:
                /* вычитаем с конца графика плановые платежи до плановой даты */
                FOR EACH  tt-term-obl WHERE
                          tt-term-obl.contract  = loan.contract
                     AND  tt-term-obl.cont-code = loan.cont-code
                     AND  tt-term-obl.idnt      = mIdnt
                     AND  tt-term-obl.end-date > dat-otch
                     AND (tt-term-obl.sop-date  = ?
                       OR tt-term-obl.sop-date >= dat-otch
                       OR (mOlapClose AND tt-term-obl.sop-date = loan.close-date))
            
                    NO-LOCK
                    BY tt-term-obl.end-date DESC:
            
                    IF RECID(tt-term-obl) = RECID(term-obl) THEN LEAVE.

                    mDiffOst = mDiffOst - tt-term-obl.amt-rub.
                    
                END.
            END.                        
         END.

         IF mDiffOst <= 0 THEN
                summ-t = term-obl.amt-rub.
             ELSE
                summ-t = IF mDiffOst - term-obl.amt-rub >= 0 THEN
                            0
                         ELSE
                            term-obl.amt-rub - mDiffOst.
         
   END. /*if e1 > 0 then do:*/
   ELSE
      summ-t = 0.
END.

{intrface.del}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:53:32.569+04:00' */
/* $LINTFILE='summ-t.p' */
/*prosignxPVN7qhxWXXUeWsPKKET0w*/