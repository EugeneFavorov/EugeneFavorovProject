/*
    	
    Created: kam
    Ищем уже созданные документы и данные для новых
*/

FUNCTION ReplaceNDS RETURN CHAR(INPUT iDetails AS CHARACTER):
    DEFINE VAR strResult AS CHAR NO-UNDO.
    DEFINE VARIABLE i AS INT NO-UNDO.
    DEFINE VARIABLE listNDS AS CHAR NO-UNDO.
    listNDS = 'В том числе НДС 18%,в т.ч. НДС (18% ),в т.ч. НДС (18%),в т.ч. НДС(18%),В т. ч. НДС,В т.ч НДС,в т.ч. НДС-,В т.ч.НДС,Включая НДС,В том числе НДС,НДС в т.ч.'.
    strResult = iDetails.
    DO i = 1 TO num-entries(listNDS):
        strResult = REPLACE(strResult,ENTRY(i,listNDS),'').
    END.
    strResult = TRIM(REPLACE(REPLACE(strResult,'..','.'),'. .','.')).
    RETURN strResult.
END FUNCTION. 
   
{globals.i}
DEFINE INPUT PARAMETER iOp AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER iAcctDb AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iAcctCr AS CHAR NO-UNDO.

DEFINE SHARED VARIABLE pick-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE strOpTrans AS CHAR NO-UNDO.
DEFINE VARIABLE strDetails1 AS CHAR NO-UNDO.
DEFINE VARIABLE strDetails2 AS CHAR NO-UNDO.
DEFINE VARIABLE strSurrCl AS CHAR NO-UNDO.
DEFINE VARIABLE strNameCl AS CHAR NO-UNDO.
DEFINE VARIABLE lOld AS LOGICAL NO-UNDO.
DEFINE VARIABLE strAcct70601 AS CHAR NO-UNDO.
DEFINE VARIABLE strAcct60309 AS CHAR NO-UNDO.
DEFINE VARIABLE strBranch AS CHAR NO-UNDO.
DEF BUFFER b_datablock FOR datablock.
DEF BUFFER b_dataline FOR dataline.
DEF BUFFER b_dataline2 FOR dataline.
DEF BUFFER b_op FOR op.

pick-value = "0".
strAcct70601 = ''.
strSurrCl = ','.
strNameCl = ''.
strBranch = SUBSTRING(iAcctDb,27,4).
FIND FIRST op WHERE op.op = DECIMAL(iOp) NO-LOCK NO-ERROR.
IF AVAIL op THEN DO:
    IF iAcctDb BEGINS '202' OR iAcctDb BEGINS '30' OR iAcctDb BEGINS '70601' THEN DO:
        strNameCl = op.name-ben.
    END.
    ELSE DO:
        FIND FIRST acct WHERE acct.acct = iAcctDb NO-LOCK NO-ERROR.
        IF AVAIL acct THEN DO:
            strSurrCl = acct.cust-cat + ',' + STRING(acct.cust-id).
            IF acct.cust-cat = 'Ч' THEN DO:
                FIND FIRST person WHERE person.person-id = acct.cust-id NO-LOCK NO-ERROR.
                IF AVAIL person THEN DO:
                    strNameCl = TRIM(person.name-last) + ' ' + TRIM(person.first-names).
                    FIND FIRST signs WHERE signs.file-name = 'person'
                        AND signs.code = 'Субъект'
                        AND signs.surrogate = STRING(person.person-id)
                        AND signs.code-value = 'ФЛП' NO-LOCK NO-ERROR.
                    IF AVAIL signs THEN DO:
                        strNameCl = 'ИП ' + TRIM(REPLACE(REPLACE(strNameCl,'ИП',''),'Индивидуальный предприниматель', '')).
                    END.                                        
                END.
            END.
            ELSE IF acct.cust-cat = 'Ю' THEN DO: 
                FIND FIRST cust-corp WHERE cust-corp.cust-id = acct.cust-id NO-LOCK NO-ERROR.
                IF AVAIL cust-corp THEN DO:              
                    strNameCl = cust-corp.cust-stat + ' ' + cust-corp.name-corp.
                END.
            END.
        END.
    END.
    strOpTrans = STRING(op.op-transaction).
    strDetails1 = 'Доходы, полученные по документу №' + STRING(op.doc-num) + ' от ' + STRING(op.doc-date,'99.99.9999')
         + '. ' + ReplaceNDS(op.details)
         + '. ' + strNameCl.
    strDetails2 = 'НДС -18%, полученный по документу №' + STRING(op.doc-num) + ' от ' + STRING(op.doc-date,'99.99.9999')
         + '. ' + ReplaceNDS(op.details) 
         + '. ' + strNameCl.
    strDetails1 = SUBSTRING(strDetails1,1,210).  
    strDetails2 = SUBSTRING(strDetails2,1,210). 
    /* ищем уже созданный документ */
    lOld = FALSE.
    FOR EACH signs WHERE signs.file-name = 'op'
        AND signs.code = 'НДСОплата'
        AND TRIM(signs.xattr-value) = iOp + ',1' NO-LOCK:
        FIND FIRST op WHERE op.op-status >= "√" AND op.op = DECIMAL(signs.surrogate) NO-LOCK NO-ERROR.
        IF AVAIL op THEN  DO:
            lOld = TRUE. /* значит есть ранее созданный документ */
            pick-value = "1".
        END.   
    END.
    IF lOld = FALSE THEN DO:  
        pick-value = '2'.
        FIND LAST datablock WHERE datablock.dataclass-id = 'Дох60322' NO-LOCK NO-ERROR.
        IF AVAIL datablock THEN
        DO:
            FIND LAST dataline WHERE dataline.data-id = datablock.data-id 
                AND dataline.sym2 = SUBSTRING(iAcctCr,1,20) NO-LOCK NO-ERROR.
            IF AVAIL dataline THEN DO:
                strAcct70601 = dataline.txt.
                FIND LAST b_datablock WHERE b_datablock.dataclass-id = 'Счета НДС' NO-LOCK NO-ERROR.
                IF AVAIL b_datablock THEN DO:
                    FIND LAST b_dataline WHERE b_dataline.data-id = b_datablock.data-id 
                    AND b_dataline.sym2 = strBranch NO-LOCK NO-ERROR.
                    IF AVAIL b_dataline THEN DO:
                         pick-value = b_dataline.sym3.
                        FIND LAST b_dataline2 WHERE b_dataline2.data-id = b_datablock.data-id  
                            AND b_dataline2.sym1 = b_dataline.sym3 
                            AND b_dataline2.sym2 = 'YES'   NO-LOCK NO-ERROR.
                        IF AVAIL b_dataline2 THEN DO:
                            strAcct60309 = b_dataline2.txt.

                            pick-value = strOpTrans + ';' + strAcct70601 + ';' + strAcct60309 + ';' + strSurrCl + ';' + strDetails1 + ';' + strDetails2.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.

RETURN pick-value.

