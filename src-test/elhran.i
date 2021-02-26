/* ��।��塞 �ਧ���� */
vPar = ''.

/* ��襫 �� ���᪠⥫� ���஭�� */
vTmpStr = GetXAttrValueEx('op',STRING(tt-op.op),'ᯮᮡ�����','').

IF vTmpStr EQ '�����஭��' THEN
	{additem.i vPar 'El'}
/**/

/* �஢��塞 �� ����஢���� */
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

/* �஢�ઠ �� ���ᮢ� ���㬥�� (op-cash.i/def) */
IF isCash THEN 
	isElH = FALSE.
ELSE DO:

	/* ��室�� ��� ��� �஢�ન branch-id*/
	FIND FIRST b-acctdb WHERE b-acctdb.acct EQ vADB NO-LOCK NO-ERROR.
	FIND FIRST b-acctcr WHERE b-acctcr.acct EQ vACR NO-LOCK NO-ERROR.

	IF AVAIL(b-acctcr) AND AVAIL(b-acctdb) THEN
	DO:
		/* �஢��塞 �� �����䨪���� elhran */
		FOR EACH elhran WHERE NOT CAN-DO(elhran.paramt,'�᪫'):
			
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
		
		/* �஢�ઠ �� �᪫�祭�� */
		IF isElH THEN DO:
			
			FOR EACH elhran WHERE CAN-DO(elhran.paramt,'�᪫'):
	
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

END.

isAuto = FALSE.

/* �஢��塞 ��⮬���᪠� �� �஢���� */
/*IF op.user-id MATCHES '_SERV*,SERV*' THEN
*/
IF CAN-DO('*SERV*,SYNC',tt-op.user-id) THEN
	isAuto = TRUE.