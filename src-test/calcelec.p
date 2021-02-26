
 
{globals.i} 

{intrface.get xclass}

{tmprecid.def}


DEFINE VARIABLE mI AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mRSBeg AS INT64 INIT 0 NO-UNDO.
DEFINE VARIABLE mAcctDetails   AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE VARIABLE mBegDate AS DATE NO-UNDO.
DEFINE VARIABLE mEndDate AS DATE NO-UNDO.
DEFINE VARIABLE mAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankMFO AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrINN AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrKPP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorrAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtDb AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAmtCr AS DECIMAL NO-UNDO.
DEFINE VARIABLE mKPP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtDbAll AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mAmtCrAll AS DECIMAL INIT 0.0 NO-UNDO.
DEFINE VARIABLE mDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE isCash AS LOGICAL NO-UNDO.
DEF VAR adb     AS CHAR    NO-UNDO. 
DEF VAR acr     AS CHAR    NO-UNDO.

DEFINE BUFFER op FOR op.
DEFINE BUFFER tt-op FOR op.
DEFINE BUFFER bop FOR op.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER bop-entry FOR op-entry.


DEFINE BUFFER datablock FOR datablock.
DEFINE BUFFER dataline FOR dataline.

{getdate.i}
/* переменные для определения кассового документа */
 {op-cash.def} 
/* инициализация справочника эл.док-ов */
 {elhran.def} 

FIND LAST datablock
  WHERE datablock.dataclass-id BEGINS "safetyp"
  AND datablock.branch-id EQ shFilial
  AND datablock.beg-date EQ end-date
  AND datablock.end-date EQ end-date
  NO-LOCK NO-ERROR.
IF NOT AVAILABLE(datablock) THEN
DO:
RUN cre-db.p (end-date).
/*
  CREATE DataBlock.
  ASSIGN
      DataBlock.dataclass-id = "safetyp"
      DataBlock.branch-id = shFilial
      DataBlock.beg-date = end-date
      DataBlock.end-date = end-date
      DataBlock.Data-Source  = "99"
  .
*/
FIND LAST datablock
  WHERE datablock.dataclass-id BEGINS "safetyp"
  AND datablock.branch-id EQ shFilial
  AND datablock.beg-date EQ end-date
  AND datablock.end-date EQ end-date
  NO-LOCK NO-ERROR.

END.
FOR EACH op-entry
    WHERE
    op-entry.filial-id EQ shFilial
    AND op-entry.op-date EQ end-date 
    AND op-entry.op-status GE chr(251)
    NO-LOCK: 
    FIND FIRST op
        WHERE op.op EQ op-entry.op
        AND op.op-date EQ end-date
        AND op.op-status GE chr(251)
        NO-LOCK NO-ERROR.
    IF AVAILABLE(op) THEN
    DO:
        CREATE tmprecid.
        tmprecid.id = RECID(op-entry).
    END.
END.
/*Филиал 0000 */
FOR EACH tmprecid:
    mi = mI + 1.
    FIND FIRST op-entry
        WHERE RECID(op-entry) EQ tmprecid.id
        AND op-entry.acct-db NE ?
        NO-LOCK NO-ERROR.
    IF AVAILABLE(op-entry) THEN
    DO:
      FIND FIRST op
      	WHERE op.op EQ op-entry.op
      	NO-LOCK NO-ERROR.
      IF AVAILABLE(op) THEN
      DO: /* if op */
          ASSIGN
              adb = op-entry.acct-db
              acr = op-entry.acct-cr
          .
  /* определение кассового документа */
        {op-cash.i} 
  /* определение электронного документа */
          FIND FIRST tt-op
              WHERE RECID(tt-op) EQ RECID(op)
          NO-LOCK NO-ERROR.
         {elsafe.i}
          FIND FIRST dataline
              WHERE dataline.data-id EQ datablock.data-id
              AND dataline.val[1] EQ DECIMAL(RECID(op-entry))
              NO-LOCK NO-ERROR.
          IF NOT AVAILABLE(dataline) THEN
          DO:
              RUN cre-dl.p (datablock.data-id,
                  SUBSTRING(op-entry.acct-db, 1, 5),
                  SUBSTRING(op-entry.acct-cr, 1, 5),
                  op-entry.currency,
                  STRING(RECID(op-entry)),
                  DECIMAL(RECID(op-entry)),
                  IF isElH THEN 2.0 ELSE 1.0,
                  mDbCr
                  ).
  /*            CREATE dataline.
              ASSIGN 
                dataline.data-id = datablock.data-id
                dataline.sym1 = SUBSTRING(op-entry.acct-db, 1, 5)
                dataline.sym2 = SUBSTRING(op-entry.acct-cr, 1, 5)
                dataline.sym3 = op-entry.currency
                dataline.sym4 = STRING(RECID(op-entry))
                dataline.val[1] = DECIMAL(RECID(op-entry))
                dataline.val[2] = IF SUBSTRING(op-entry.acct-db, 1, 1) EQ "9" THEN 0.0 ELSE 1.0
                dataline.val[4] = IF isElH THEN 2.0 ELSE 1.0
                dataline.txt = mDbCr
                . */
          END.
      END. /* if op */
    END. /* if op-entry */
END. /* FOR EACH */
MESSAGE end-date SKIP "Расчет закончен." VIEW-AS ALERT-BOX.
{intrface.del}

