{globals.i}

DEFINE BUFFER op FOR op.
DEFINE BUFFER op1 FOR op.
DEFINE BUFFER op-kind FOR op-kind.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER op-entry2 FOR op-entry.

{tmprecid.def}

{setdest.i}

FOR EACH tmprecid, 
	FIRST op
	WHERE RECID(op) EQ tmprecid.id
	NO-LOCK:
/* документ 1*/		        
        	FIND FIRST op1
        		WHERE RECID(op1) EQ RECID(op)
        		EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        	IF AVAILABLE(op1) THEN
		DO:
			PUT UNFORMATTED "Yes" SKIP.
/*	        	op1.op-status = CHR(251). */
	        	op1.op-status = "В". 
		END.
		ELSE
			PUT UNFORMATTED "NO" SKIP.
        	FIND FIRST op-entry2
        		WHERE op-entry2.op EQ op.op
        		EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        	IF AVAILABLE(op-entry2) THEN
		DO:
			PUT UNFORMATTED "Yes" SKIP.
/*	        	op-entry2.op-status = CHR(251). */
	        	op-entry2.op-status = "В". 
		END.
		ELSE
			PUT UNFORMATTED "NO" SKIP.
		FIND CURRENT op1 NO-LOCK NO-ERROR.
		FIND CURRENT op-entry2 NO-LOCK NO-ERROR.
END.

{preview.i}
