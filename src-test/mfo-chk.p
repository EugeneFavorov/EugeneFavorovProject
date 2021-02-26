/* ‘¢¥àª  Œ”*/

{globals.i}

{sh-defs.i}

DEFINE VARIABLE mAmount AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAmountCorr AS DECIMAL NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER acct1 FOR acct.
DEFINE BUFFER c-nostro FOR c-nostro.
DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op-entry1 FOR op-entry.
DEFINE BUFFER op FOR op.
DEFINE BUFFER op1 FOR op.

{getdate.i}

{setdest.i}

PUT UNFORMATTED
"‘¢¥àª  áç¥â®¢ Ž‘’Ž ‹ŽŽ §  " 
end-date
SKIP
"‡ ¯ãé¥­  ¨§ ä¨«¨ «  "
shFilial
SKIP
.

FOR EACH acct
	WHERE acct.filial-id EQ shFilial
	AND acct.contract BEGINS "®áâ”"
	AND acct.close-date EQ ?
NO-LOCK,
FIRST c-nostro
	WHERE c-nostro.acct EQ acct.acct
NO-LOCK:

	FIND FIRST acct1
		WHERE acct1.acct BEGINS c-nostro.corr-acct
/*		AND acct1.filial-id EQ "0400" */
	NO-LOCK NO-ERROR.
	IF AVAILABLE(acct1) THEN
	DO:

		RUN acct-pos IN h_base (
			acct.acct,
	        acct.currency,
	        end-date,
	        end-date,
	        "û" 
	        ).
		IF acct.currency NE "" THEN
			mAmount = IF acct.side EQ "€" THEN ABSOLUTE(sh-vdb) ELSE ABSOLUTE(sh-vcr).
		ELSE
			mAmount = IF acct.side EQ "€" THEN ABSOLUTE(sh-db) ELSE ABSOLUTE(sh-cr).

		RUN acct-pos IN h_base (
			acct1.acct,
	        acct1.currency,
	        end-date,
	        end-date,
	        "û" 
	        ).
		IF acct1.currency NE "" THEN
			mAmountCorr = IF acct1.side EQ "€" THEN ABSOLUTE(sh-vdb) ELSE ABSOLUTE(sh-vcr).
		ELSE
			mAmountCorr = IF acct1.side EQ "€" THEN ABSOLUTE(sh-db) ELSE ABSOLUTE(sh-cr).

		IF mAmount NE mAmountCorr THEN
		DO:
			PUT UNFORMATTED SKIP(1)
			acct.acct
			" "
			mAmount
			" "
			acct1.acct
			" "
			mAmountCorr
			" à¢¥â ­  "
			mAmount - mAmountCorr
			SKIP
			.
	    	PUT UNFORMATTED 
	    	"”¨«¨ «"
	    	" "
	    	"op" format "x(15)"
	    	" "
	    	"áâ â" format "x(4)"
	    	" "
	    	"ü ¤®ª  "
	    	" "
	    	"‚ «"
	    	" "
	    	"‘ã¬¬ "
	    	SKIP
	    	.
/*			IF acct.contract EQ "®áâ”" THEN */
			    FOR EACH op-entry
			    	WHERE op-entry.filial-id EQ acct.filial-id
			    	AND op-entry.op-date EQ end-date
			    	AND op-entry.acct-cr EQ acct.acct
			    NO-LOCK, 
			    FIRST op
			    WHERE op.op EQ op-entry.op
		    	AND op.op-date EQ end-date
			    AND op.op-status GE /* "û" */ "”"
			    NO-LOCK:
				    FOR EACH op-entry1
				    	WHERE op-entry1.filial-id EQ acct1.filial-id
				    	AND op-entry1.op-date EQ end-date
				    	AND op-entry1.acct-db EQ acct1.acct
				    	AND (IF acct.currency EQ "" THEN op-entry1.amt-rub EQ op-entry.amt-rub ELSE op-entry1.amt-cur EQ op-entry.amt-cur)
				    NO-LOCK, 
				    FIRST op1
				    WHERE op1.op EQ op-entry1.op
			    	AND op1.op-date EQ end-date
				    AND op1.doc-num EQ op.doc-num
				    AND op1.op-status GE /* "û" */ "”"
				    NO-LOCK:
				    	LEAVE.
				    END.
				    IF NOT AVAILABLE(op-entry1) THEN
				    	PUT UNFORMATTED 
				    	acct.filial-id format "x(6)"
				    	" "
				    	(STRING(op-entry.op) + "," + STRING(op-entry.op-entry)) format "x(15)"
				    	" "
				    	op.op-status format "x(4)"
				    	" "
				    	op.doc-num format "x(7)"
				    	" "
				    	SUBSTRING(acct.acct, 6, 3)
				    	" "
				    	(IF acct.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur) format "->>>,>>>,>>9.99"
				    	SKIP
				    	.
			    END.	
/*			ELSE   */
			    FOR EACH op-entry1
			    	WHERE op-entry1.filial-id EQ acct1.filial-id
			    	AND op-entry1.op-date EQ end-date
			    	AND op-entry1.acct-cr EQ acct1.acct
			    NO-LOCK, 
			    FIRST op1
			    WHERE op1.op EQ op-entry1.op
		    	AND op1.op-date EQ end-date
			    NO-LOCK:
				    FOR EACH op-entry
				    	WHERE op-entry.filial-id EQ acct.filial-id
				    	AND op-entry.op-date EQ end-date
				    	AND op-entry.acct-db EQ acct.acct
				    	AND (IF acct.currency EQ "" THEN op-entry.amt-rub EQ op-entry1.amt-rub ELSE op-entry.amt-cur EQ op-entry1.amt-cur)
				    NO-LOCK, 
				    FIRST op
				    WHERE op.op EQ op-entry.op
			    	AND op.op-date EQ end-date
				    AND op.doc-num EQ op1.doc-num
				    NO-LOCK:
				    	LEAVE.
				    END.
				    IF NOT AVAILABLE(op-entry) THEN
				    	PUT UNFORMATTED 
				    	acct1.filial-id format "x(6)"
				    	" "
				    	(STRING(op-entry1.op) + "," + STRING(op-entry1.op-entry)) format "x(15)"
				    	" "
				    	op1.op-status format "x(4)"
				    	" "
				    	op1.doc-num format "x(7)"
				    	" "
				    	SUBSTRING(acct1.acct, 6, 3)
				    	" "
				    	(IF acct1.currency EQ "" THEN op-entry1.amt-rub ELSE op-entry1.amt-cur) format "->>>,>>>,>>9.99"
				    	SKIP
				    	.
			    END.	
/*  ---------------------- */

			    FOR EACH op-entry
			    	WHERE op-entry.filial-id EQ acct.filial-id
			    	AND op-entry.op-date EQ end-date
			    	AND op-entry.acct-db EQ acct.acct
			    NO-LOCK, 
			    FIRST op
			    WHERE op.op EQ op-entry.op
		    	AND op.op-date EQ end-date
			    AND op.op-status GE /* "û" */ "”"
			    NO-LOCK:
				    FOR EACH op-entry1
				    	WHERE op-entry1.filial-id EQ acct1.filial-id
				    	AND op-entry1.op-date EQ end-date
				    	AND op-entry1.acct-cr EQ acct1.acct
				    	AND (IF acct.currency EQ "" THEN op-entry1.amt-rub EQ op-entry.amt-rub ELSE op-entry1.amt-cur EQ op-entry.amt-cur)
				    NO-LOCK, 
				    FIRST op1
				    WHERE op1.op EQ op-entry1.op
			    	AND op1.op-date EQ end-date
				    AND op1.doc-num EQ op.doc-num
				    AND op1.op-status GE /* "û" */ "”"
				    NO-LOCK:
				    	LEAVE.
				    END.
				    IF NOT AVAILABLE(op-entry1) THEN
				    	PUT UNFORMATTED 
				    	acct.filial-id format "x(6)"
				    	" "
				    	(STRING(op-entry.op) + "," + STRING(op-entry.op-entry)) format "x(15)"
				    	" "
				    	op.op-status format "x(4)"
				    	" "
				    	op.doc-num format "x(7)"
				    	" "
				    	SUBSTRING(acct.acct, 6, 3)
				    	" "
				    	(IF acct.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur) format "->>>,>>>,>>9.99"
				    	SKIP
				    	.
			    END.	
/*			ELSE   */
			    FOR EACH op-entry1
			    	WHERE op-entry1.filial-id EQ acct1.filial-id
			    	AND op-entry1.op-date EQ end-date
			    	AND op-entry1.acct-db EQ acct1.acct
			    NO-LOCK, 
			    FIRST op1
			    WHERE op1.op EQ op-entry1.op
		    	AND op1.op-date EQ end-date
			    NO-LOCK:
				    FOR EACH op-entry
				    	WHERE op-entry.filial-id EQ acct.filial-id
				    	AND op-entry.op-date EQ end-date
				    	AND op-entry.acct-cr EQ acct.acct
				    	AND (IF acct.currency EQ "" THEN op-entry.amt-rub EQ op-entry1.amt-rub ELSE op-entry.amt-cur EQ op-entry1.amt-cur)
				    NO-LOCK, 
				    FIRST op
				    WHERE op.op EQ op-entry.op
			    	AND op.op-date EQ end-date
				    AND op.doc-num EQ op1.doc-num
				    NO-LOCK:
				    	LEAVE.
				    END.
				    IF NOT AVAILABLE(op-entry) THEN
				    	PUT UNFORMATTED 
				    	acct1.filial-id format "x(6)"
				    	" "
				    	(STRING(op-entry1.op) + "," + STRING(op-entry1.op-entry)) format "x(15)"
				    	" "
				    	op1.op-status format "x(4)"
				    	" "
				    	op1.doc-num format "x(7)"
				    	" "
				    	SUBSTRING(acct1.acct, 6, 3)
				    	" "
				    	(IF acct1.currency EQ "" THEN op-entry1.amt-rub ELSE op-entry1.amt-cur) format "->>>,>>>,>>9.99"
				    	SKIP
				    	.
			    END.	
/*  ---------------------- */


		END.
	END.

END.

{preview.i}


