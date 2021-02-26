/* проверяем на полупроводки */
IF op-entry.acct-db EQ ? THEN DO:
	FIND FIRST b-ope WHERE b-ope.op EQ op-entry.op 
					 AND   b-ope.acct-db NE ? NO-ERROR.
	vADB = b-ope.acct-db.
END.
ELSE
	vADB = op-entry.acct-db.
IF op-entry.acct-cr EQ ? THEN DO:
	FIND FIRST b-ope WHERE b-ope.op EQ op-entry.op 
					 AND   b-ope.acct-cr NE ? NO-ERROR.
	vACR = b-ope.acct-cr.
END.
ELSE
	vACR = op-entry.acct-cr.

/*находим счета для определения подразделения*/
FIND FIRST b-acctdb WHERE b-acctdb.acct EQ vADB NO-LOCK NO-ERROR.
FIND FIRST b-acctcr WHERE b-acctcr.acct EQ vACR NO-LOCK NO-ERROR.

IF AVAIL(b-acctcr) AND AVAIL(b-acctdb) THEN
DO:
	/* при Докдн отмечаем все документы, входящие в классификатор ХозДок, кроме исключений */
	/* т.е. при отборе Докдн, нужно отбирать документы с NOT isHoz                         */
	IF {&hoz-type} EQ 'Докдн' THEN
		iTemp = '!Искл,*'.
	ELSE
		iTemp = {&hoz-type}.
	
	/* проверяем по классификатору ХозДок */
	FOR EACH hozdoc WHERE CAN-DO(iTemp,hozdoc.type):
		
		isHoz = FALSE.
	
		IF CAN-DO(hozdoc.branch-db,b-acctdb.branch-id) AND	
	       CAN-DO(hozdoc.branch-cr,b-acctcr.branch-id) AND
	       CAN-DO(hozdoc.acct-db,b-acctdb.acct)        AND	
	       CAN-DO(hozdoc.acct-cr,b-acctcr.acct)        AND
	       CAN-DO(hozdoc.doctype,tt-op.doc-type)          AND
	       CAN-DO(hozdoc.details,tt-op.details) THEN
			isHoz = TRUE.		
	
		IF isHoz THEN LEAVE.
	
	END.
	
	/* проверяем хозяйственные документы на исключения*/
	IF {&hoz-type} = 'Хоздок' THEN
	
		FOR EACH hozdoc WHERE hozdoc.type NE {&hoz-type}:
	
			IF CAN-DO(hozdoc.branch-db,b-acctdb.branch-id) AND	
		       CAN-DO(hozdoc.branch-cr,b-acctcr.branch-id) AND	
		       CAN-DO(hozdoc.acct-db,b-acctdb.acct)        AND
		       CAN-DO(hozdoc.acct-cr,b-acctcr.acct)        AND
		       CAN-DO(hozdoc.doctype,tt-op.doc-type)          AND
		       CAN-DO(hozdoc.details,tt-op.details) THEN
				isHoz = FALSE.
	
			IF NOT isHoz THEN LEAVE.
	
		END.
	
	/* проверяем документы дня на исключения*/
	IF {&hoz-type} = 'Докдн' THEN
	
		FOR EACH hozdoc WHERE hozdoc.type EQ 'Искл':
	
			IF CAN-DO(hozdoc.branch-db,b-acctdb.branch-id) AND	
		       CAN-DO(hozdoc.branch-cr,b-acctcr.branch-id) AND	
		       CAN-DO(hozdoc.acct-db,b-acctdb.acct)        AND
		       CAN-DO(hozdoc.acct-cr,b-acctcr.acct)        AND
		       CAN-DO(hozdoc.doctype,tt-op.doc-type)          AND
		       CAN-DO(hozdoc.details,tt-op.details) THEN
				isHoz = FALSE.
	
			IF NOT isHoz THEN LEAVE.
	
		END.

END.