{globals.i}
{intrface.get xclass}

DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.

DEFINE BUFFER loan FOR loan.

{getfile.i &filename="'scheta.csv'" &mode=must-exist }

INPUT FROM VALUE(fname) CONVERT TARGET "ibm866"  SOURCE "1251".
/*
INPUT FROM VALUE(fname).
*/
{setdest.i}
REPEAT:
    IMPORT UNFORMATTED mStr.
    IF mStr NE "end" THEN
    DO:
    	FIND FIRST loan
    		WHERE loan.filial-id EQ shFilial
            AND loan.contract EQ "�।��"
            AND loan.cont-code BEGINS ENTRY(3, mStr, ';')
    	NO-LOCK NO-ERROR.
    	IF AVAILABLE(loan) THEN
    	DO:
            IF UpdateSigns(loan.class-code,
            			loan.contract + "," + loan.cont-code,
            			"AcctCesRS",
            			ENTRY(4, mStr, ';'),
                		?) THEN
        		PUT UNFORMATTED
        		ENTRY(1, loan.cont-code, "@") FORMAT "X(33)"
        		"�� AcctCesRS ��������"
        		SKIP.
            ELSE
                PUT UNFORMATTED
                ENTRY(1, loan.cont-code, "@") FORMAT "X(33)"
                "�� AcctCesRS �� ᬮ� ��������"
                SKIP.
                
    	END.
    	ELSE
    		PUT UNFORMATTED
            ENTRY(1, loan.cont-code, "@") FORMAT "X(33)"
    		"������� �� ������"
    		SKIP.
            
    END.

END.    
INPUT CLOSE.
{preview.i}
{intrface.del}
