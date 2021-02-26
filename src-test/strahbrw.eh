/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: strahbrw.EH
      Comment: Получатели страховых премий - триггеры формы
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:    
*/

ON LEAVE OF mCustID IN FRAME edit
DO:
    RUN FillSubj.
    IF mResult = FALSE THEN RETURN NO-APPLY {&RET-ERROR}. 
END.

ON LEAVE OF mCustCat IN FRAME edit
DO:
    IF mCustCat:SCREEN-VALUE IN FRAME edit = "Р" THEN DO:
        IF mCustID:SCREEN-VALUE = "0" THEN
            mCustID:SCREEN-VALUE = STRING(GetCounterNextValue ("strahpol",?)).
        DISABLE mCustID WITH FRAME edit.
    END.
    IF mCustCat:SCREEN-VALUE IN FRAME edit = "Ю" THEN DO:
        /*ENABLE mCustID WITH FRAME edit. */
        DISABLE code.{&DocType} WITH FRAME edit.
        DISABLE code.{&DocCustCode} WITH FRAME edit.
        DISABLE code.{&DocOpenDate} WITH FRAME edit.
        DISABLE code.{&DocIssue} WITH FRAME edit.
        DISABLE code.{&BirthDay} WITH FRAME edit.
        DISABLE code.{&Address} WITH FRAME edit.
    END.
    ELSE DO:
        ENABLE code.{&DocType} WITH FRAME edit.
        ENABLE code.{&DocCustCode} WITH FRAME edit.
        ENABLE code.{&DocOpenDate} WITH FRAME edit.
        ENABLE code.{&DocIssue} WITH FRAME edit.
        ENABLE code.{&BirthDay} WITH FRAME edit.
        ENABLE code.{&Address} WITH FRAME edit.
    END.
    
    
   
 /*   RETURN. */
END.   
    
ON LEAVE OF code.{&DocOpenDate} IN FRAME edit
DO:
   IF code.{&DocOpenDate} <> "" THEN DO:
    ASSIGN end-date = DATE(INT64(SUBSTR(code.{&DocOpenDate}:SCREEN-VALUE,4,2)), /* DATE(month, day, year) */
                  INT64(SUBSTR(code.{&DocOpenDate}:SCREEN-VALUE,1,2)), 
                  INT64(SUBSTR(code.{&DocOpenDate}:SCREEN-VALUE,7,4))) no-error.
    IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Неверный формат даты " SKIP
       "Требуемый формат - DD.MM.YYYY" VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY {&RET-ERROR}.
   END.
  END.
END.       
    
ON LEAVE OF code.{&BirthDay} IN FRAME edit
DO:
   IF code.{&BirthDay} <> "" THEN DO:
    ASSIGN end-date = DATE(INT64(SUBSTR(code.{&BirthDay}:SCREEN-VALUE,4,2)), /* DATE(month, day, year) */
                  INT64(SUBSTR(code.{&BirthDay}:SCREEN-VALUE,1,2)), 
                  INT64(SUBSTR(code.{&BirthDay}:SCREEN-VALUE,7,4))) no-error.
    IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Неверный формат даты " SKIP
       "Требуемый формат - DD.MM.YYYY" VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY {&RET-ERROR}.
   END.
  END.
END.            
       
/*
ON LEAVE OF mRaschAcct IN FRAME edit  /* расчетный счет */
DO:
    FIND FIRST acct WHERE acct.number = string(mRaschAcct:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAIL acct THEN  
        RUN FillAcct(acct.acct).
    END.
    RETURN.
END.
GetCounterNextValue(mDocNumCode, TODAY).
 */

/*
ON ESC OF FRAME edit
DO:
    /* HIDE FRAME edit. */
    RETURN.
END.
*/






