/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: LIM_DLN.P
      Comment: Контроль возможности довнесения на вклад по заявке 0023476
               Наличие этой процедуры в транзакции довнесения (g_doloan), 
               а именно в доп.рек "ВыпДоИт", позволяет контролировать дату 
               возможного довнесения.
   Parameters:
         Uses:
      Used by: 
      Created: 26.01.2004 18:57 SAP     
     Modified: 27.01.2004 13:28 SAP      
     Modified: 15.06.2005 13:28 SAP Разводка механизмов ВТБ и Гуты 
     Modified: 10.02.2015 16:50 PJA Новый контроль за довложениями во вклад.
               после n-го для никаких довнесей. Исключение возврат по налогам и досрочное расторжение договора, периоценка валютных счетов
*/
 {globals.i}
 {dpsproc.def}
DEFINE INPUT  PARAM iRec         AS RECID NO-UNDO.
DEFINE INPUT  PARAM in-op-date   AS DATE  NO-UNDO.
DEFINE INPUT  PARAM in-cont-date AS DATE  NO-UNDO.
DEFINE OUTPUT PARAM oMess        AS CHAR  NO-UNDO INIT "".

                                    
 DEF VAR mPeriod AS CHAR NO-UNDO. /*продолжительность вклада*/
 DEF VAR mLim-doloan AS INT64 NO-UNDO. /*количество дней от закрытия*/
 DEF VAR mLim-date AS DATE NO-UNDO. /*дата последнего довнесения*/
 DEF VAR op-id  AS CHAR NO-UNDO. /*op, op_entry*/
 DEF VAR mOpId AS INT NO-UNDO.
 DEF VAR mOpEntryId AS INT NO-UNDO.
 DEF VAR mAmount AS DEC NO-UNDO. /*сумма проводки*/
 DEF VAR mDeb AS CHAR NO-UNDO. /*счет в кредите*/
 DEF VAR mCharMinAmount AS DEC NO-UNDO.
 DEF VAR mMinAmount AS DEC NO-UNDO. mMinAmount=0.
 DEF VAR mOut AS INT NO-UNDO.   mOut=0.
 DEF VAR mAnswer AS LOGICAL NO-UNDO.
 DEF VAR mIsLim AS INT NO-UNDO. mIsLim=0.
 DEF VAR mBenAcct AS CHAR NO-UNDO. /*продолжительность вклада*/
 DEF VAR mTeller AS INT NO-UNDO. mTeller=0.
 DEF VAR mTeller1 AS INT NO-UNDO. mTeller1=0.
 DEF VAR mMess AS CHAR NO-UNDO.
 
 
 DEF BUFFER acct1 FOR acct.
 DEF BUFFER acct2 FOR acct.
 DEF BUFFER op-entry FOR op-entry.

 FIND FIRST loan WHERE RECID(loan) = iRec NO-LOCK NO-ERROR.
 IF NOT AVAILABLE loan THEN DO:
     oMess = "Вклад не найден".
     RETURN.
 END.

 IF loan.open-date EQ in-op-date THEN DO:
   oMess = "".
   RETURN.
 END.

 /*Определение даты окончания периода пополнения для вкладов ВТБ*/
 RUN end_doloan_dps in h_dpspc (iRec,
                                        in-cont-date,
                                        OUTPUT mLim-date,
                                        OUTPUT oMess).

 IF mLim-date = ? THEN DO:
    IF oMess EQ "" OR oMess EQ ? THEN DO:
       oMess = "Для вклада " + loan.cont-code + " не определено значение в классификаторе lim_dln".
       RETURN.
    END.
    ELSE
       RETURN.      
 END.
 
 RUN Get_Last_Param in h_dpspc (recid(loan),
                                        in-cont-date,
                                        in-cont-date,
                                        'amount-min-dop', 
                                         output mCharMinAmount).
                                         
 IF mCharMinAmount NE ? AND mPeriod NE "?" THEN 
      mMinAmount=DEC(mCharMinAmount) NO-ERROR.                                    
 
 op-id = GetSysConf("op-id").
 
 IF op-id=? THEN DO:
     FOR EACH loan-acct WHERE  
                loan-acct.cont-code=loan.cont-code
                AND loan-acct.acct-type ="loan-dps-t"
                AND in-cont-date GE loan-acct.since
                NO-LOCK:
       FOR EACH op-entry WHERE 
                op-entry.op-date=in-cont-date AND
                op-entry.acct-cr=loan-acct.acct AND
                NOT CAN-DO("N00001,D00001", op-entry.op-cod) NO-LOCK:     
       IF AVAILABLE op-entry THEN DO:  
          mTeller = mTeller + 1.  
          if mTeller GT 1 THEN
             LEAVE.
          mDeb=op-entry.acct-db.
          mOut=1.
          IF op-entry.currency EQ "" THEN
              mAmount=op-entry.amt-rub.
          ELSE
              mAmount=op-entry.amt-cur.
          FIND FIRST op WHERE op.op-transaction=op-entry.op-transaction  
               AND op.op=op-entry.op.
          IF AVAILABLE op THEN    
            mBenAcct=op.ben-acct.
        END.
    END.   
    if mTeller GT 1 THEN
       LEAVE.                      
    END.    
 END.
 
 IF op-id NE ? THEN DO:
    IF NUM-ENTRIES(op-id) EQ 2 THEN DO:
        
        mOpId=INT64(ENTRY(1,op-id)) NO-ERROR.
        mOpEntryId=INT64(ENTRY(2,op-id)) NO-ERROR.
        
        FIND FIRST op-entry WHERE op-entry.op = mOpId 
           AND op-entry.op-entry=mOpEntryId NO-LOCK NO-ERROR.
        IF AVAILABLE op-entry THEN DO:   
          mDeb=op-entry.acct-db.
          IF op-entry.currency EQ "" THEN
              mAmount=op-entry.amt-rub.
          ELSE
              mAmount=op-entry.amt-cur.
          mTeller = mTeller + 1.  
          FIND FIRST op WHERE op.op-transaction=op-entry.op-transaction  
               AND op.op=op-entry.op.
          IF AVAILABLE op THEN    
            mBenAcct=op.ben-acct. 
        END.         
    END.       
 END. 
 
 IF mTeller EQ 0 THEN DO:
    oMess = "Не обнаружен документ для проверки условий пополнения вклада.".
         RETURN.
 END.   

 IF in-cont-date GT mLim-date THEN DO:
    /*когда документ для проверки обнаружен однозначно*/ 
    IF mTeller EQ 1  THEN DO:
        IF CAN-DO("60323*", mDeb) THEN DO:
            RETURN.
        END.             
     END.
     /*24.03.2014 pja любой платеж после 60 дней отвергать, не давать возвожность выбора */
     /*MESSAGE "Нельзя делать довнесение на этот вклад позднее " + 
                  STRING(mLim-date) + ". Пополнить вклад принудительно? Помните, вся ответственность ложится на Вас!" VIEW-AS
                  ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mAnswer.
     IF mAnswer THEN
        RETURN.*/
      oMess =  "Нельзя делать довнесение на этот вклад позднее " + 
           STRING(mLim-date).
      RETURN.     
 END.            
 ELSE DO: 
    /*в день открытия вклада разрешаем приход с любых счетов в любой сумме*/ 
    IF in-cont-date NE loan.open-date THEN  DO:
      /*если документ обнаружен однозначно*/  
      IF mTeller EQ 1 THEN DO:  
        IF mAmount LT mMinAmount THEN DO:
          FIND FIRST acct1 WHERE acct1.acct=mDeb NO-LOCK NO-ERROR.
          IF NOT AVAILABLE acct1 THEN DO:
             mIsLim=1.
          END.  
          ELSE DO:
            IF loan.cust-id NE acct1.cust-id AND
               NOT CAN-DO("60323*", mDeb)
            THEN DO:
               IF CAN-DO("30306*,30302*", mDeb) THEN DO:
                  
                   IF mBenAcct NE ? THEN DO:
                     FOR EACH acct2 WHERE acct2.cust-id=loan.cust-id NO-LOCK:
                         IF SUBSTRING(acct2.acct,1,20) EQ mBenAcct THEN DO:
                             mTeller1 = mTeller1 + 1.
                             LEAVE.
                         END.    
                     END.    
                     IF mTeller1 EQ 0 THEN DO:
                        mIsLim=1.
                     END.
                   END.
                   ELSE 
                     mIsLim=1.                      
               END.
               ELSE     
                 mIsLim=1.
            END.
          END. 
          
        END.
        ELSE
          RETURN.   
      END.
      ELSE   /*иначе мы не знаем к какому именно документу применить проверку*/
        mIsLim=1.    
    END.
    IF mIsLim Ne 0 THEN DO:  
         /*Операция выполнена не с помощью вкладных транзакций*/
         IF mOut EQ 1 THEN DO:
            /*Таких операций в течении дня несколько. Конкретную определить нет возможности*/ 
            IF mTeller GT 1 THEN 
              mMess="Сумма пополнения вклада не должна быть менее " + 
                  STRING(mMinAmount) + ". Продолжить операцию? Помните, что вся ответственность ложится на Вас!".
            ELSE
              mMess="Сумма пополнения вклада не должна быть менее " + 
                  STRING(mMinAmount) + ". Пополнить вклад принудительно? Помните, что вся ответственность ложится на Вас!".      
                  
            MESSAGE mMess VIEW-AS
                  ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mAnswer.
          IF mAnswer THEN
             RETURN.
          oMess =  "Сумма пополнения вклада не должна быть менее " + 
                  STRING(mMinAmount).
          RETURN.           
         END.
         ELSE DO:         
            oMess =  "Сумма пополнения вклада не должна быть менее " + 
                  STRING(mMinAmount).
            RETURN.
         END.
    END.     
END.   
 {intrface.del}


