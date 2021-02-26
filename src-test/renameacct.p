/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created: 
*/
{globals.i}
{intrface.get xclass}
{intrface.get ovl}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{tmprecid.def}


DEF VAR mErrorCode AS INT64  NO-UNDO.
DEF VAR mCount     AS INT64  NO-UNDO.
DEF VAR mContract  AS CHAR   NO-UNDO.
DEF VAR mClsLoan   AS CHAR   NO-UNDO.
DEF VAR vOk        AS LOGICAL NO-UNDO.
DEF VAR mSDate     AS DATE   NO-UNDO.




   FOR EACH tmprecid NO-LOCK,
      FIRST loan WHERE RECID(loan) EQ tmprecid.id  
   NO-LOCK :
      find last loan-acct of loan where loan-acct.acct-type = 'КредРезВб' no-lock no-error.
      if avail loan-acct then do:
          find first acct of loan-acct.
          if avail acct then do:
              acct.details = 'Резерв на возможные потери по обязательствам обратного выкупа по КД ' + loan.doc-ref + ' от ' + string(loan.open-date,"99.99.9999").
              
          end.
      end.
   END.


{intrface.del}