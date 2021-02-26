/* определяем признаки */
vPar = ''.

/* пришел от взыскателя электронно */
vTmpStr = GetXAttrValueEx('op',STRING(tt-op.op),'способполуч','').

IF vTmpStr EQ 'Электронный' THEN
	{additem.i vPar 'El'}
/**/

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

/* проверка на кассовый документ (op-cash.i/def) */
IF isCash THEN 
	isElH = FALSE.
ELSE DO:

	/* находим счета для проверки branch-id*/
	FIND FIRST b-acctdb WHERE b-acctdb.acct EQ vADB NO-LOCK NO-ERROR.
	FIND FIRST b-acctcr WHERE b-acctcr.acct EQ vACR NO-LOCK NO-ERROR.

	/* проверяем по классификатору elhran */
	FOR EACH elhran WHERE NOT CAN-DO(elhran.paramt,'Искл'):
		
		isElH = TRUE.

		IF NOT CAN-DO(elhran.branch-db,b-acctdb.branch-id)  OR
	       NOT CAN-DO(elhran.branch-cr,b-acctcr.branch-id)  OR
	       NOT CAN-DO(elhran.acct-db,b-acctdb.acct) 	    OR
	       NOT CAN-DO(elhran.acct-cr,b-acctcr.acct) 	    OR
	       NOT CAN-DO(elhran.doctype,tt-op.doc-type) 			OR 
	       NOT CAN-DO(elhran.details,tt-op.details)  		    OR
	       NOT CAN-DO(elhran.currency,op-entry.currency)    OR
	       NOT CAN-DO(elhran.op-kind,tt-op.op-kind) THEN
			isElH = FALSE.

		IF elhran.paramt NE '' AND isElH THEN DO:
			
			IF vPar EQ '' THEN DO:
				isElH = FALSE.
				LEAVE.
			END.

			DO vCnt = 1 TO NUM-ENTRIES(vPar):
				IF NOT CAN-DO(elhran.paramt,ENTRY(vCnt,vPar)) THEN
					isElH = FALSE.
			END.

		END.

		IF isElH THEN LEAVE.

	END.
	
	/* проверка на исключения */
	IF isElH THEN DO:
		
		FOR EACH elhran WHERE CAN-DO(elhran.paramt,'Искл'):

			IF CAN-DO(elhran.branch-db,b-acctdb.branch-id)   AND
		       CAN-DO(elhran.branch-db,b-acctcr.branch-id)   AND
		       CAN-DO(elhran.acct-db,b-acctdb.acct) 	     AND
		       CAN-DO(elhran.acct-cr,b-acctcr.acct) 	     AND
		       CAN-DO(elhran.doctype,tt-op.doc-type) 			 AND 
		       CAN-DO(elhran.details,tt-op.details)             AND
		       CAN-DO(elhran.currency,op-entry.currency)     AND
		       CAN-DO(elhran.op-kind,tt-op.op-kind) THEN
				isElH = FALSE.

			IF NOT isElH THEN LEAVE.

		END.

	END.



END.

mDbCr = " ".
IF isElH THEN
DO:
/*debet*/
	FIND FIRST code 
		WHERE code.class EQ 'elsafe' 
		AND   code.parent EQ 'elsafe'
		AND   code.code EQ SUBSTRING(vADB, 1, 5)
		AND   code.val BEGINS "D"
	NO-LOCK NO-ERROR.
	IF AVAILABLE(code) THEN
	DO: 
		mDbCr = "D".
		FIND FIRST code1 
			WHERE code1.class EQ 'elsafe' 
			AND   code1.parent EQ 'elsafe'
			AND   code1.code EQ SUBSTRING(vACR, 1, 5)
			AND   CAN-DO(code1.val, "C")
		NO-LOCK NO-ERROR.
		IF AVAILABLE(code1) 
			AND SUBSTRING(vADB, 1, 5) NE SUBSTRING(vACR, 1, 5) 
			AND NOT (vACR BEGINS "9999")
			AND NOT (vADB BEGINS "909" AND vACR BEGINS "909")
			THEN
			DO:
				FIND FIRST elhran 
					WHERE NOT CAN-DO(elhran.paramt,'Искл')
					AND CAN-DO(elhran.acct-db,vADB)
					AND NOT CAN-DO(elhran.acct-cr, vACR)
				NO-LOCK NO-ERROR.
				IF AVAILABLE(elhran) THEN
				DO:
					mDbCr = "C".
				END.
			END.
	END.
	ELSE
		mDbCr = "C". 
END.

isAuto = FALSE.

/* проверяем автоматическая ли проводка */
/*IF op.user-id MATCHES '_SERV*,SERV*' THEN
*/
IF CAN-DO('*SERV*',tt-op.user-id) THEN
	isAuto = TRUE.