{globals.i}
{sh-defs.i}

{intrface.get xclass}

DEFINE VARIABLE mListAcct AS CHARACTER INITIAL "*"   NO-UNDO.

DEFINE VARIABLE mDate     AS DATE      NO-UNDO.
DEFINE VARIABLE mAcct1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSide1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst1     AS INT64     NO-UNDO.
DEFINE VARIABLE mAcct2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSide2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst2     AS INT64     NO-UNDO.

DEFINE BUFFER  acct FOR acct.
DEFINE BUFFER bAcct FOR acct.

{getdate.i}

mDate = end-date.

{setdest.i &filename = "'check-corp.log'"}


PUT UNFORMATTED "Проверка парных счетов" SKIP.
PUT UNFORMATTED "Филиал " shFilial SKIP.
PUT UNFORMATTED STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"hh:mm:ss") SKIP(1).

FOR EACH acct WHERE TRUE
   AND acct.filial-id   EQ shFilial
   AND CAN-DO(mListAcct,acct.acct)
   AND acct.contr-acct  NE ""
   AND acct.acct-cat    EQ "b"
   AND (acct.close-date EQ ? OR acct.close-date GE mDate) 
   NO-LOCK,
   FIRST bAcct WHERE TRUE 
   AND bAcct.acct EQ acct.contr-acct 
   AND bAcct.currency EQ acct.currency
   AND (bacct.close-date EQ ? OR bacct.close-date GE mDate) 
   NO-LOCK:

   RUN acct-pos IN h_base (acct.acct,
                           acct.currency,
                           mDate,
                           mDate,
                           CHR(251)).
   
   ASSIGN
      mAcct1 = acct.number 
      mSide1 = acct.side
      mOst1  = IF acct.currency EQ "" THEN sh-bal ELSE sh-val.
                           
   RUN acct-pos IN h_base (bacct.acct,
                           bacct.currency,
                           mDate,
                           mDate,
                           CHR(251)).
                           
   ASSIGN
      mAcct2 = bacct.number
      mSide2 = bacct.side
      mOst2  = IF bacct.currency EQ "" THEN sh-bal ELSE sh-val.

   IF mOst1 NE 0 AND mOst2 NE 0 THEN
   DO:                            
      PUT UNFORMATTED 
          mAcct1  AT 1
          STRING(mOst1,"->>,>>>,>>>,>>9.99") AT 22
          mAcct2  AT 41
          STRING(mOst2,"->>,>>>,>>>,>>9.99") AT 61
      SKIP.
   END.                        
END.

{preview.i &filename = "'check-corp.log'"}

{intrface.del}

RETURN.
