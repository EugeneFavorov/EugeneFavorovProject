/*               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: markstoplst.p
      Comment: Отбор всех документов по указанному лицу
   Parameters: iParam - параметры польз.процедуры
         Uses:
      Used by:
      Created:  28.08.2013       
     Modified:  28.08.2013   
*/


{globals.i}
{parsin.def}
{intrface.get xclass}

DEF INPUT PARAM iParam AS CHAR NO-UNDO.

DEF VAR mFrameCustCut AS CHAR  NO-UNDO.
DEF VAR mFrameCustId  AS CHAR  NO-UNDO.
DEF VAR mCliLst       AS CHAR  NO-UNDO.
DEF VAR mFio          AS CHAR  NO-UNDO.
DEF VAR mDetails      AS CHAR  NO-UNDO.
DEF VAR mInn          AS CHAR  NO-UNDO.
DEF VAR mNeob         AS CHAR  NO-UNDO.
DEF VAR mPodoz        AS CHAR  NO-UNDO.
DEF VAR mInnREc       AS CHAR  NO-UNDO.
DEF VAR mInnSend      AS CHAR  NO-UNDO.
DEF VAR mNSend        AS CHAR  NO-UNDO.
DEF VAR mNREc         AS CHAR  NO-UNDO.
DEF VAR mStr          AS CHAR  NO-UNDO.
DEF VAR vH            AS HANDLE NO-UNDO.
DEF VAR mAcctCl         AS CHAR  NO-UNDO.
DEF VAR mIsklCorrAcctCl AS CHAR  NO-UNDO.
DEF VAR mIsklRs         AS CHAR  NO-UNDO.

DEF TEMP-TABLE tt-op-stopmark NO-UNDO
    FIELD op LIKE op.op
    FIELD opOtmiv  AS CHAR
    FIELD opNeob   AS CHAR
.
DEF BUFFER op       FOR op.
DEF BUFFER op-entry FOR op-entry.
DEF BUFFER dacct    FOR acct.
DEF BUFFER cacct    FOR acct.


FUNCTION MarkSlPar LOGICAL (iOpPar AS CHAR,
                            iCompStr   AS CHAR): 
IF    {assigned iOpPar} 
  AND {assigned iCompStr} THEN
   RETURN iOpPar EQ iCompStr.

END FUNCTION.

FUNCTION AccCliEq LOGICAL (iAcct AS CHAR):
DEFINE BUFFER acct FOR acct.
   FIND FIRST acct WHERE
              acct.acct EQ iAcct NO-LOCK NO-ERROR.
     IF    AVAIL acct   
        AND acct.cust-cat EQ mFrameCustCut
        AND STRING(acct.cust-id)  EQ mFrameCustId  THEN 
       RETURN YES.
     ELSE
       RETURN NO.

END FUNCTION.

ASSIGN
   mNeob  =   TRIM(GetParamByNameAsChar(iParam,"КодНеобыч","*"),",")
   mPodoz =   TRIM(GetParamByNameAsChar(iParam,"КодОпОтмыв","*"), ",")
   mAcctCl         = FGetSetting("markstoplst", "СчетаКл", "")
   mIsklCorrAcctCl = FGetSetting("markstoplst", "ИсклКоррСчКл", "")
   mIsklRs         = FGetSetting("markstoplst", "ИсклРс", "")
   end-date = 01/01/2100
   beg-date = 01/01/1900.

FORM
   beg-date  
      LABEL "Выборка с"      
   end-date   
      LABEL "по"               AT 30 SKIP(1) 
   mFio
      FORMAT "x(254)"
      VIEW-AS FILL-IN SIZE 30  BY 1 
      LABEL "ФИО/Наименование" AT 4  SKIP
   mInn
      FORMAT "x(254)"
      VIEW-AS FILL-IN SIZE 30  BY 1 
      LABEL "ИНН"              AT 17 SKIP
   mFrameCustCut 
      VIEW-AS COMBO-BOX LIST-ITEMS "","Ю","Ч","Б" 
      LABEL "Тип клиента"  AT 9 
   mFrameCustId  
      FORMAT "x(10)"
      VIEW-AS FILL-IN SIZE 30  BY 1            
      LABEL "Клиент"           AT 14 SKIP
   mDetails
      FORMAT "x(254)"
      VIEW-AS FILL-IN SIZE 30  BY 1 
      LABEL "Содержание операции"    SKIP
   mAcctCl
      FORMAT "x(5000)"
      VIEW-AS FILL-IN SIZE 30  BY 1 
      LABEL "СчетаКл"          AT 13 SKIP
   mIsklCorrAcctCl
      FORMAT "x(5000)"
      VIEW-AS FILL-IN SIZE 30  BY 1 
      LABEL "ИсклКоррСчКл"     AT 8  SKIP
   mIsklRs
      FORMAT "x(5000)"
      VIEW-AS FILL-IN SIZE 30  BY 1 
      LABEL "ИсклРС"           AT 14
WITH FRAME persfrm TITLE "Настройки отчета" CENTERED OVERLAY SIDE-LABEL ROW 10.


ON "F1" OF mAcctCl IN FRAME persfrm
,mIsklCorrAcctCl,mIsklRs DO:
   M1:
   DO ON ERROR  UNDO M1, LEAVE M1
      ON ENDKEY UNDO M1, LEAVE M1: 
      pick-value = SELF:SCREEN-VALUE.
      DISPLAY pick-value NO-LABEL VIEW-AS EDITOR SIZE 60 BY 5
          WITH OVERLAY FRAME sss1 SIDE-LABELS 1 COL
          CENTERED ROW 12 TITLE COLOR bright-white "[ РЕДАКТИРОВАНИЕ ПАРАМЕТРА ]".
      ENABLE pick-value WITH FRAME sss1.
      ON F8 OF FRAME sss1 ANYWHERE DO:
          RETURN NO-APPLY.
      END.
      WAIT-FOR GO OF FRAME sss1 FOCUS pick-value.
   END.
   SELF:SCREEN-VALUE = pick-value:SCREEN-VALUE.
   HIDE FRAME sss1.
END.

ON "F1" OF mFrameCustId IN FRAME persfrm DO :
   DO TRANS:

   pick-value = "".    
    RUN browseld (ENTRY(LOOKUP (mFrameCustCut:SCREEN-VALUE IN FRAME persfrm,"Ч,Ю,Б"), 
                       "person,cust-corp,banks"),"","","",4).
   IF (LASTKEY EQ 13 OR LASTKEY EQ 10) AND pick-value NE "" THEN
      mFrameCustId:SCREEN-VALUE = pick-value.
   END.
END.

ON 'GO' OF FRAME persfrm DO:
   ASSIGN

      mFio          = TRIM(mFio:SCREEN-VALUE)            
      mInn          = mInn:SCREEN-VALUE            
      mFrameCustCut = mFrameCustCut:SCREEN-VALUE   
      mFrameCustId  = mFrameCustId:SCREEN-VALUE    
      mDetails      = mDetails:SCREEN-VALUE        
      beg-date      = DATE(beg-date:SCREEN-VALUE)
      end-date      = DATE(end-date:SCREEN-VALUE)
      mAcctCl         = TRIM(mAcctCl:SCREEN-VALUE)
      mIsklCorrAcctCl = TRIM(mIsklCorrAcctCl:SCREEN-VALUE)
      mIsklRs         = TRIM(mIsklRs:SCREEN-VALUE)
   .

   /*=== После выхода 0312840 файл можно удалить из спецверсии ======*/
   IF     {assigned mFrameCustCut} 
      AND {assigned mFrameCustId} 
   THEN
   DO:
      IF NOT {assigned mAcctCl} THEN
         mAcctCl = "*".

      IF NOT {assigned mDetails} THEN
         mDetails = "*".

      lOpLoop:
      FOR EACH acct WHERE acct.cust-cat = mFrameCustCut
                      AND acct.cust-id  = INT64(mFrameCustId)
                      AND CAN-DO(mAcctCl, acct.acct)
                    NO-LOCK,
         EACH op-entry WHERE (    op-entry.acct-db   =  acct.acct
                              AND op-entry.op-date   >= beg-date
                              AND op-entry.op-date   <= end-date
                             ) 
                          OR (    op-entry.acct-cr   =  acct.acct
                              AND op-entry.op-date   >= beg-date
                              AND op-entry.op-date   <= end-date
                             ) 
                       NO-LOCK,
         FIRST op WHERE op.op EQ op-entry.op
                    AND CAN-DO(mDetails, op.details)
                  NO-LOCK:
        
         /* Если заполнено поле формы 'ИсклРс',
            и есть такие счета в поле Р/с док-та, то пропускаем */
         IF {assigned mIsklRs } AND
             CAN-DO(mIsklRs,op.ben-acct)
         THEN NEXT.
        
         /* Если заполнено поле формы 'ИсклКоррСчКл',
            и есть такие счета в проводке, то пропускаем */
         IF     {assigned mIsklCorrAcctCl } 
            AND (
                 (    {assigned mIsklCorrAcctCl op-entry.acct-db} 
                  AND CAN-DO(mIsklCorrAcctCl, DelFilFromAcct(op-entry.acct-db))
                 )
                 OR 
                 (    {assigned mIsklCorrAcctCl op-entry.acct-cr} 
                  AND CAN-DO(mIsklCorrAcctCl, DelFilFromAcct(op-entry.acct-cr))
                 )
                )
         THEN 
            NEXT.

         /* проверяем счет по кредиту */
         IF {assigned op-entry.acct-cr } THEN
         DO:
            RUN crettop-stopmark(op.op).
            NEXT.
         END.
              
         /*проверяем счет по дебету*/
         IF {assigned op-entry.acct-db } THEN
         DO:
            RUN crettop-stopmark(op.op).
            NEXT.
         END.
        
         IF MarkSlPar (TRIM(OP.inn), TRIM(mInn)) THEN
         DO:   
            RUN crettop-stopmark(op.op).
            NEXT.
         END. /* IF MarkSlPar (TRIM(OP.inn), TRIM(mInn)) THEN */
         ELSE DO:
            mInnREc  = TRIM(GetXattrValueEx("op", STRING(op.op), "inn-rec", "")).
            IF MarkSlPar (TRIM(op.inn), TRIM(mInn)) THEN
            DO:   
               RUN crettop-stopmark(op.op).
               NEXT.
            END.
        
            mInnSend = TRIM(GetXattrValueEx("op", STRING(op.op), "inn-send", "")).
            IF MarkSlPar (mInnSend, TRIM(mInn)) THEN
            DO:   
               RUN crettop-stopmark(op.op).
               NEXT.
            END.
        
            mNRec    = TRIM(GetXattrValueEx("op", STRING(op.op), "name-rec", "")).
            mNSend   = TRIM(GetXattrValueEx("op", STRING(op.op), "name-send",  "")).
            IF   { assigned mFio}  
             AND (   LOOKUP(mFio,mNRec,       " ")  GT 0
                  OR LOOKUP(mFio, mNSend,     " ")  GT 0 
                  OR LOOKUP(mFio,op.name-ben, " ")  GT 0  )
            THEN DO: 
               RUN crettop-stopmark(op.op).
               NEXT.
            END.

         END.
      END. /* lOpLoop: FOR EACH acct WHERE acct.cust-cat = mFrameCustCut */
   END. /* IF {assigned mFrameCustCut}  */
   ELSE
   /*=== Конец изменений по заявке 0312840 ==========================*/
   FOR EACH op WHERE op.op-date GE beg-date
                 AND op.op-date LE END-date
   NO-LOCK:

     FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
     IF AVAILABLE op-entry THEN DO:
       /* Если заполнено поле формы 'СчетаКл',
          и нет таких счетов в проводке или в поле Р/с док-та
          (счет должет быть КЛИЕНТА), то пропускаем */
       IF {assigned mAcctCl } AND
          NOT
          (  (CAN-DO(mAcctCl,DelFilFromAcct(op-entry.acct-db)) AND
              AccCliEq(op-entry.acct-db) AND
              op-entry.acct-db NE ?)
          OR (CAN-DO(mAcctCl,DelFilFromAcct(op-entry.acct-cr)) AND
              AccCliEq(op-entry.acct-cr) AND
              op-entry.acct-cr NE ?)
          OR (CAN-DO(mAcctCl,op.ben-acct) AND
              AccCliEq(op.ben-acct))
          )
       THEN NEXT.
       /* Если заполнено поле формы 'ИсклКоррСчКл',
          и есть такие счета в проводке, то пропускаем */
       IF {assigned mIsklCorrAcctCl } AND
          (  (CAN-DO(mIsklCorrAcctCl,DelFilFromAcct(op-entry.acct-db)) AND
              op-entry.acct-db NE ?)
          OR (CAN-DO(mIsklCorrAcctCl,DelFilFromAcct(op-entry.acct-cr)) AND
              op-entry.acct-cr NE ?))
       THEN NEXT.
     END.
      /* Если заполнено поле формы 'ИсклРс',
         и есть такие счета в поле Р/с док-та, то пропускаем */
      IF {assigned mIsklRs } AND
          CAN-DO(mIsklRs,op.ben-acct)
      THEN NEXT.


       IF {assigned mFrameCustId} THEN
       DO:
          FOR EACH op-entry OF op NO-LOCK:

             /*проверяем счет по кредиту*/
             IF {assigned op-entry.acct-cr } THEN
             DO:
                {find-act.i &acct=op-entry.acct-cr}
                IF    AVAIL acct   
                   AND acct.cust-cat EQ mFrameCustCut
                   AND STRING(acct.cust-id)  EQ mFrameCustId  THEN 
                DO:   
                   RUN crettop-stopmark(op.op).
                   NEXT.
                END.                           
             END.
            
             /*проверяем счет по дебету*/
             IF {assigned op-entry.acct-db } THEN
             DO:
                {find-act.i &acct=op-entry.acct-db}
                IF   AVAIL acct   
                   AND acct.cust-cat EQ mFrameCustCut
                   AND STRING(acct.cust-id)  EQ mFrameCustId  THEN 
                DO:   
                   RUN crettop-stopmark(op.op).
                   NEXT.
                END.                           
             END.
          END.            
       END.

       IF MarkSlPar (TRIM(OP.inn), TRIM(mInn)) THEN
       DO:   
          RUN crettop-stopmark(op.op).
          NEXT.
       END.
       ELSE DO:
          mInnREc  = TRIM(GetXattrValueEx("op", STRING(op.op), "inn-rec", "")).
          IF MarkSlPar (TRIM(op.inn), TRIM(mInn)) THEN
          DO:   
             RUN crettop-stopmark(op.op).
             NEXT.
          END.

          mInnSend = TRIM(GetXattrValueEx("op", STRING(op.op), "inn-send", "")).
          IF MarkSlPar (mInnSend, TRIM(mInn)) THEN
          DO:   
             RUN crettop-stopmark(op.op).
             NEXT.
          END.

          mNRec    = TRIM(GetXattrValueEx("op", STRING(op.op), "name-rec", "")).
          mNSend   = TRIM(GetXattrValueEx("op", STRING(op.op), "name-send",  "")).
          IF   { assigned mFio}  
           AND (   LOOKUP(mFio,mNRec,       " ")  GT 0
                OR LOOKUP(mFio, mNSend,     " ")  GT 0 
                OR LOOKUP(mFio,op.name-ben, " ")  GT 0  )
          THEN DO: 
             RUN crettop-stopmark(op.op).
             NEXT.
          END.
          IF     {assigned mDetails} 
             AND CAN-DO(mDetails, op.details)
          THEN DO: 
             RUN crettop-stopmark(op.op).
             NEXT.
          END.
       END.
   END.       
   vH = BUFFER tt-op-stopmark:HANDLE.
   RUN legstoplst-brw.p(vH,mNeob,mPodoz, 2) .
END.
                          
ON "END-ERROR" OF FRAME persfrm
DO:
   HIDE FRAME persfrm NO-PAUSE.
   RETURN .
END.

PAUSE 0.

DISPLAY
   beg-date 
   end-date
   mFio  
   mInn
   mFrameCustCut
   mFrameCustId
   mDetails
   mAcctCl
   mIsklCorrAcctCl
   mIsklRs
WITH FRAME persfrm.

UPDATE
   beg-date 
   end-date
   mFio  
   mInn
   mFrameCustCut
   mFrameCustId
   mDetails
   mAcctCl
   mIsklCorrAcctCl
   mIsklRs
WITH FRAME persfrm.

HIDE FRAME persfrm.

PROCEDURE crettop-stopmark.
   DEFINE INPUT PARAMETER iOp AS INT64 NO-UNDO.
   FIND FIRST tt-op-stopmark  where
              tt-op-stopmark.op EQ iOp NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-op-stopmark THEN
   DO:
      CREATE tt-op-stopmark. 
      ASSIGN
          tt-op-stopmark.op       = op.op
          tt-op-stopmark.opOtmiv  = GetXattrValueEx("op", STRING(op.op), "КодОпОтмыв", "")
          tt-op-stopmark.opNeob   = GetXattrValueEx("op", STRING(op.op), "КодНеобыч", "").
   END.
END.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='20/04/2015 12:40:10.721+04:00' */
/* $LINTUSER='paus' */
/* $LINTMODE='1' */
/* $LINTFILE='markstoplst.p' */
/*prosign9qTSwRy+MnD9i6vtAcIaCA*/