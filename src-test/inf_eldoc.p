{globals.i}

DEF VAR mFilialList AS CHAR NO-UNDO.
DEF VAR mI AS INT64 INIT 0  NO-UNDO.

mFilialList = "0000,0300,0500".

{setdest.i}
PUT UNFORMATTED
"Filial | Last date " SKIP
"-------------------" SKIP
.
DO mI = 1 TO NUM-ENTRIES(mFilialList):
   FIND LAST DataBlock 
       WHERE DataBlock.dataclass-id EQ "safetyp"
         AND DataBlock.branch-id    EQ ENTRY(mI,mFilialList)
         AND DataBlock.Data-Source  EQ "99" 
   NO-LOCK NO-ERROR.
   PUT UNFORMATTED
      STRING(DataBlock.branch-id,"X(7)") + "| " + STRING(DataBlock.beg-date,"99.99.9999") SKIP
   .
END.

{preview.i}