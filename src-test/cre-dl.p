{globals.i}
DEFINE INPUT PARAMETER iDataId AS INT64 NO-UNDO.
DEFINE INPUT PARAMETER iAcctDb AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iAcctCr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iCurr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iRecidStr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iRecid AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iTypeSafe AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iDbCr AS CHARACTER NO-UNDO.

            CREATE dataline.
            ASSIGN 
              dataline.data-id = iDataId
              dataline.sym1 = iAcctDb
              dataline.sym2 = iAcctCr
              dataline.sym3 = iCurr
              dataline.sym4 = iRecidStr
              dataline.val[1] = iRecid
              dataline.val[4] = iTypeSafe
              dataline.txt = iDbCr
              .


