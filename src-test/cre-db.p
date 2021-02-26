{globals.i}
DEFINE INPUT PARAMETER iDate AS DATE NO-UNDO.

DO TRANSACTION:
  CREATE DataBlock.
  ASSIGN
      DataBlock.dataclass-id = "safetyp"
      DataBlock.branch-id = shFilial
      DataBlock.beg-date = iDate
      DataBlock.end-date = iDate
      DataBlock.Data-Source  = "99"
  .
END.

