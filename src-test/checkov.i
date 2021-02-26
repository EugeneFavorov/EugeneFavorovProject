/* kam */
/* проверяем допрек */


FUNCTION checkov returns logical (
    BUFFER iloan FOR loan):

    DEF VAR bRes AS LOG NO-UNDO INIT FALSE.
	
    FIND FIRST signs WHERE signs.file-name = 'loan'
	AND signs.surrogate = iloan.contract + ',' + iloan.cont-code
	AND signs.code = 'Priznak'
	AND signs.xattr-value begins 'ОВ' NO-LOCK NO-ERROR.
    IF AVAIL signs THEN bRes = TRUE.
    RETURN bRes.
END.
