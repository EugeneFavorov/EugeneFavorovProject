/*
               ОАО "ПЛЮС-БАНК"
     Filename: ak_popolnen.p
      Comment: Отчет по акции "Пополнение"
   Parameters: 
         Uses: 
      Used by: 
      Created: kau 13/03/14
     Modified: kau 24/03/14 добавлено условие в случае изъятий со вклада в период убрать из отчета
*/


{tmprecid.def}
{globals.i}
{client.i}

{sh-defs.i}
{setdest.i}
/*{ksh-defs.i NEW}*/

/*
{g-error.def}
{g-defs.i}
{crdps.def}
{dpsproc.def}
{def-wf.i new}
{defframe.i new}
{invest.num}
{ksh-defs.i new}
{intrface.get "xclass"}
{intrface.get "ltran"}
{intrface.get cust}
{intrface.get tmcod}
{sh-defs.i}
{ksh-defs.i}
*/
DEF VAR dOtkAc		AS DATE 	NO-UNDO.
DEF VAR dZakAc		AS DATE 	NO-UNDO.
DEF VAR vCustID		AS DEC		NO-UNDO.
DEF VAR vCustName	AS CHAR		NO-UNDO.
DEF VAR vOstVklP	AS DEC		NO-UNDO.
DEF VAR vOstVklT	AS DEC		NO-UNDO.






DEF VAR vContType	AS CHAR		NO-UNDO.
DEF VAR vContCode	AS CHAR		NO-UNDO.

DEF VAR vKKO		AS CHAR		NO-UNDO.




DEF VAR vIstPriv	AS CHAR		NO-UNDO.
DEF VAR vPhone		AS CHAR		NO-UNDO.

DEF VAR vDateAk		AS DATE 	NO-UNDO.


DEFINE TEMP-TABLE tt-a NO-UNDO
    FIELD OtkAc AS CHAR
    FIELD ZakAc AS CHAR 
    FIELD CustID like acct.cust-id
    FIELD CustName AS CHARACTER
    FIELD OstPod AS DEC
    FIELD OstTek AS DEC

    FIELD DocNum LIKE op.doc-num
    FIELD DDoc 	LIKE op.op-date
    FIELD acct-db LIKE op-entry.acct-db
    FIELD acct-cr LIKE op-entry.acct-cr
    FIELD amt LIKE op-entry.amt-rub
    FIELD det LIKE op.details
    FIELD ContType AS CHAR
    FIELD ContCode LIKE loan.doc-ref
    FIELD KKO AS CHAR
    FIELD IstPriv	AS CHAR
    FIELD Phone		AS CHAR
    FIELD bol		AS CHAR
    INDEX CustID DocNum DDoc.

/* Выход если нет ни одного отобранного договора */
if not can-find(first tmprecid) then return.


for each tmprecid,
    first loan where recid(loan) eq tmprecid.id
    NO-LOCK:
	FIND LAST loan-acct where loan-acct.cont-code = loan.cont-code
			and (loan-acct.acct-type EQ 'loan-dps-t' OR loan-acct.acct-type EQ 'loan-dps-p') no-lock.

	IF AVAIL loan-acct then DO:
		
		vDateAk = DATE(Entry(1,GetxattrValue("loan","dps," + loan.cont-code, "Акция201403"))).
		if vDateAk = ? THEN NEXT.
		find acct where acct.acct EQ loan-acct.acct NO-LOCK NO-ERROR.
		dOtkAc = acct.open-date.
		dZakAc = acct.close-date.
		vCustId = acct.cust-id.
		RUN RE_CLIENT (acct.cust-cat, acct.cust-id, INPUT-OUTPUT vCustName).

		RUN acct-pos IN h_base (acct.acct, acct.currency, vDateAk, vDateAk, ?).
		vOstVklP = - sh-bal.
		RUN acct-pos IN h_base (acct.acct, acct.currency, today, today, ?).
		vOstVklT = - sh-bal.




		FIND FIRST code where code.class = 'cont-type'
			and code.code =  loan.cont-type 
		no-lock no-error.
		IF AVAIL code then vContType = code.name.
		vContCode = loan.doc-ref.

		FIND FIRST branch where branch.branch-id = loan.branch-id NO-LOCK NO-ERROR.
		IF AVAIL branch then vKKO = branch.name.

		IF loan.cust-cat = 'Ч' THEN
		FIND FIRST person where person.person-id = loan.cust-id NO-LOCK NO-ERROR.
		IF AVAIL person THEN vIstPriv = GetXattrValue ("person",string(person.person-id),"КаналПривл").
		IF AVAIL person THEN vPhone = person.phone[1] + person.phone[2].
		FOR EACH op-entry where op-entry.acct-cr = acct.acct
				 or op-entry.acct-db = acct.acct NO-LOCK:
                        FIND FIRST op where op.op = op-entry.op and op.op-date >= vDateAk NO-LOCK NO-ERROR.
			IF NOT AVAIL op then NEXT.
			CREATE tt-a.
			ASSIGN	tt-a.OtkAc = string(dOtkAc)
				tt-a.ZakAc = if dZakAc = ? then "" else string(dZakAc)
				tt-a.CustId = vCustId
				tt-a.CustName = vCustName
				tt-a.OstPod = vOstVklP
				tt-a.OstTek = vOstVklT
				tt-a.docnum = op.doc-num
				tt-a.dDoc = op.op-date
				tt-a.acct-db = substring(op-entry.acct-db,1,20)
				tt-a.acct-cr = substring(op-entry.acct-cr,1,20)
				tt-a.amt = op-entry.amt-rub
				tt-a.det = op.details
				tt-a.ContType = vContType
				tt-a.ContCode = vContCode
				tt-a.kko = vkko
				tt-a.IstPriv = vIstPriv
				tt-a.Phone = IF vPhone EQ ',,' then '' else vPhone
			.
		/*Ищем изъятие в период акции*/		
			FIND FIRST kau-entry WHERE kau-entry.kau-id = 'loan-dps-t'
				AND kau-entry.debit 
				AND (kau-entry.kau EQ loan.contract + "," + loan.cont-code + "," + 'ОстВклС' OR
					kau-entry.kau EQ loan.contract + "," + loan.cont-code + ",НачПрС1")
				AND kau-entry.op-date >= vDateAk
				NO-LOCK NO-ERROR.
			IF AVAIL kau-entry THEN tt-a.bol = 'ss:StyleID=' + '"s65"'.


/*PUT UNFORMATTED /*MESSAGE*/ string(today) ";" string(tt-a.OtkAc) ";" string(tt-a.ZakAc) "; " string(tt-a.CustId) "; " string(tt-a.CustName) "; " string(tt-a.OstPod) "; " string(tt-a.OstTek) "; " string(tt-a.docnum) "; " string(tt-a.dDoc) "; " string(tt-a.acct-db) "; " string(tt-a.acct-cr) "; " string(tt-a.amt) "; " string(tt-a.det) "; " string(tt-a.ContType) "; " string(tt-a.ContCode) "; " string(tt-a.kko) "; " string(tt-a.IstPriv) "; " string(tt-a.Phone) /*VIEW-AS ALERT-BOX*/ SKIP.*/
		END.


	END.

END.

{ak_popolnen.i}


/*{preview.i}*/
