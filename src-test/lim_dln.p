/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: LIM_DLN.P
      Comment: Контроль возможности довнесения на вклад по заявке 0023476
               Наличие этой процедуры в транзакции довнесения (g_doloan), 
               а именно в доп.рек "ВыпДоИт", позволяет контролировать дату 
               возможного довнесения.
   Parameters: iRec,in-op-date,in-cont-date
         Uses:
      Used by: 
      Created: 26.01.2004 18:57 SAP     
     Modified: 27.01.2004 13:28 SAP       
     Modified: 15.06.2005 13:28 SAP Разводка механизмов ВТБ и Гуты  
*/
 {globals.i}
 {dpsproc.def}
DEFINE INPUT  PARAM iRec         AS RECID NO-UNDO.
DEFINE INPUT  PARAM in-op-date   AS DATE  NO-UNDO.
DEFINE INPUT  PARAM in-cont-date AS DATE  NO-UNDO.
DEFINE OUTPUT PARAM oMess        AS CHAR  NO-UNDO INIT "".
                                    
 DEF VAR mCMetod  AS CHAR NO-UNDO.
                                    
 DEF VAR mPeriod AS CHAR NO-UNDO. /*продолжительность вклада*/
 DEF VAR mLim-doloan AS INT64 NO-UNDO. /*количество дней от закрытия*/
 DEF VAR mLim-date AS DATE NO-UNDO. /*дата последнего довнесения*/

 FIND FIRST loan WHERE RECID(loan) = iRec NO-LOCK NO-ERROR.
 IF NOT AVAILABLE loan THEN DO:
     oMess = "Вклад не найден".
     RETURN.
 END.
 
 FIND FIRST op-kind WHERE op-kind.op-kind EQ loan.op-kind NO-LOCK NO-ERROR.
 IF AVAIL op-kind THEN
 DO:
   /*Контроль сроков довложения*/
   RUN Get_Last_Param in h_dpspc (iRec,
                              in-op-date,
                              in-op-date,
                              'КонтрДовлБезнал',
                              OUTPUT mCMetod).
    /*MESSAGE mCMetod VIEW-AS ALERT-BOX.*/
   IF mCMetod EQ "pl-dv" OR mCMetod EQ "pl-dv1" OR mCMetod EQ "pl-dv2" OR mCMetod EQ "pl-dv3" OR mCMetod EQ "pl-dv0" THEN
   DO:
      RETURN.
   END.
 END.
 
 /*Определение даты окончания периода пополнения для вкладов ВТБ*/
 RUN end_doloan_dps in h_dpspc (iRec,
                                        in-cont-date,
                                        OUTPUT mLim-date,
                                        OUTPUT oMess).
 /*Проверяем для вкладов Гуты ("Универсал") 
   Вклады Гуты неклассифицированы, поэтому применить ВТБ-шный механизм не удастся
   Придется пока держать два классификатора. Один для вкладов Гуты  = lim_doloan
   Другой - для ВТБ = lim_dln
 */

 IF mLim-date = ? THEN DO:
    IF oMess EQ "" OR oMess EQ ? THEN
       RETURN.
    mPeriod = GetXattrValueEx("loan",
                           loan.contract + "," + loan.cont-code,
                           "dep_period",
                           ?).
     IF mPeriod  = ? THEN
        RUN Get_Last_Param in h_dpspc (recid(loan),
                                               in-cont-date,
                                               in-cont-date,
                                               'dep-period', 
                                               output mPeriod).
     IF mPeriod = ? OR mPeriod = "?" THEN 
     DO:
         oMess = "Невозможно определить продолжительность вклада!".
         RETURN.
     END.
     
     FIND FIRST CODE WHERE CODE.class = "lim_doloan" 
         AND CODE.CODE = mPeriod NO-LOCK NO-ERROR.
     IF NOT AVAILABLE CODE THEN
     DO: 
         oMess = "Для вклада продолжительностью " + 
                 mPeriod + 
                 "~n не определено значение ни в классификаторе lim_doloan, ни в классификаторе lim_dln".
         RETURN.
     END.
    
     mLim-doloan = INT64(CODE.val) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
         oMess = "Для вклада продолжительностью " + 
                 mPeriod + 
                 "~n заведено неверное значение в классификаторе lim_doloan".
         RETURN.
     END.
     mLim-date = loan.end-date - mLim-doloan + 1.
 END.
 IF loan.open-date <= date(07/02/15)
 and loan.currency EQ ''
 and
 ((loan.cont-type = "Gold_dv") OR (loan.cont-type = "gold_card") OR (loan.cont-type = "НадПлюсКарНе") OR (loan.cont-type = "НадежныйПлюс") OR (loan.cont-type = "НадПлюсДовНе"))
 Then DO:
 oMess =  "ВНИМАНИЕ: По вкладу данного типа открытого до 02.07.2015 пополнение НЕ ВОЗМОЖНО ".
 RETURN.
 END.
 
 IF in-cont-date GE mLim-date THEN
 DO:
     oMess =  "Вы не можете делать довнесение на этот вклад с " + 
              STRING(mLim-date).
     RETURN.
 END.
 {intrface.del}
/* $LINTFILE='lim_dln.p' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='miam' */
/* $LINTDATE='28/07/2016 12:03:57.218+03:00' */
/*prosign4piSDQrDfe3NqzO7EcicXQ*/