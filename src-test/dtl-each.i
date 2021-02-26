/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: U:\GUVA\TITUL\DTL-EACH.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 28.08.2012 13:26 gva     
     Modified: 28.08.2012 15:17 gva      
     Modified: 28.08.2012 15:37 gva      
     Modified: 
*/

    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
        NO-LOCK NO-ERROR.
    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
        NO-LOCK NO-ERROR.
    PUT UNFORMATTED "³ " + STRING(op.doc-num, "x(10)") 
        + " ³ " + STRING(op-entry.acct-db, "x(20)") 
        + " ³ " + STRING(op-entry.acct-cr, "x(20)") 
        + " ³ " + STRING(op-entry.amt-rub, ">,>>>,>>>,>>>,>>9.99") + " ³" SKIP.   
    mIsNotEmpty = YES.
    mItog = mItog + op-entry.amt-rub.
