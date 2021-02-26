FOR EACH op WHERE op.op-date EQ TODAY - 1
            AND op.op-status EQ 'Š‡'
EXCLUSIVE-LOCK:
	FOR EACH op-entry WHERE op-entry.op EQ op.op EXCLUSIVE-LOCK:
	   op-entry.op-status = '€'.
	END.
	op.op-status = '€'.
END.