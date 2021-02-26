/*
		ОАО "Плюс Банк"
     Filename: kassdd.p
      Comment: Новый формат печати документов дня в кассе.
   Parameters:
         Uses:
      Created: 04/04/2014 KAU
*/

{globals.i}
{tmprecid.def}

DEF VAR kolvo 		AS DEC	NO-UNDO.
DEF VAR itogp 		AS DEC	NO-UNDO.
DEF VAR itogr		AS DEC	NO-UNDO.
DEF VAR kolp 		AS DEC	NO-UNDO.
DEF VAR kolr		AS DEC	NO-UNDO.
DEF VAR kop-date	AS DATE	NO-UNDO.
DEF VAR adb		AS CHAR NO-UNDO.
DEF VAR acr		AS CHAR NO-UNDO.
DEF VAR isCash		AS LOG	NO-UNDO.
DEF VAR kolvostr	AS DEC	NO-UNDO.


DEFINE TEMP-TABLE dd-p NO-UNDO
    FIELD op	AS DEC
    FIELD currency AS CHAR
    FIELD bal-acct AS DEC
    FIELD acct5 AS DEC
    FIELD summ AS DEC
    FIELD kass AS LOG
    INDEX op acct5.

DEFINE TEMP-TABLE dd-r NO-UNDO
    FIELD op	AS DEC
    FIELD currency AS CHAR
    FIELD bal-acct AS DEC
    FIELD acct5 AS DEC
    FIELD summ AS DEC
    FIELD kass AS LOG
    INDEX op acct5.

DEFINE TEMP-TABLE doc-val NO-UNDO
	FIELD currency AS CHAR
	FIELD pdoc	AS DEC
	FIELD rdoc	AS DEC
	INDEX currency pdoc rdoc.

DEFINE TEMP-TABLE dd NO-UNDO
    FIELD id	AS DEC
    FIELD currency AS CHAR
    FIELD psumm AS DEC
    FIELD rsumm AS DEC
    FIELD kass AS LOG
    INDEX id currency.



/*
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
*/
{setdest.i}

{op-cash.def}
for each tmprecid,
    first op where recid(op) eq tmprecid.id
    NO-LOCK:
	FOR EACH op-entry WHERE op-entry.op = op.op 
		NO-LOCK:
		{op-cash.i}
	        adb = op-entry.acct-db.
        	acr = op-entry.acct-cr.
		IF op-entry.acct-db BEGINS '202' and NOT op-entry.acct-cr BEGINS '423'
		THEN DO:
			CREATE dd-p.
			ASSIGN dd-p.op = op.op
				dd-p.currency = if op-entry.currency = " " THEN "810" ELSE op-entry.currency
				dd-p.bal-acct = DEC(SUBSTRING(op-entry.acct-cr,1,5))
				dd-p.acct5    = DEC(SUBSTRING(op-entry.acct-cr,15,5))
				dd-p.summ     = op-entry.amt-rub
				dd-p.kass     = isCash
				.
		END.

		IF op-entry.acct-cr BEGINS '202' and NOT op-entry.acct-db BEGINS '423'
		THEN DO:
			CREATE dd-r.
			ASSIGN dd-r.op = op.op
				dd-r.currency = if op-entry.currency = " " THEN "810" ELSE op-entry.currency
				dd-r.bal-acct = DEC(SUBSTRING(op-entry.acct-db,1,5))
				dd-r.acct5    = DEC(SUBSTRING(op-entry.acct-db,15,5))
				dd-r.summ     = op-entry.amt-rub
				dd-r.kass     = isCash
				.
		END.

	END.
	kop-date = op.op-date.
END.


FOR EACH dd-p BREAK BY dd-p.currency by dd-p.bal-acct by dd-p.acct5:
			/*PUT UNFORMATTED dd-p.currency Format "x(3)" "; " dd-p.bal-acct "; " dd-p.acct5 "; " dd-p.summ SKIP.*/
kolvo = kolvo + 1.
IF LAST-OF (dd-p.currency) THEN DO:
		Create doc-val.
		ASSIGN doc-val.currency = dd-p.currency
			doc-val.pdoc = kolvo
			.
		kolvo = 0.
	END.
END.




FOR EACH dd-r BREAK BY dd-r.currency by dd-r.bal-acct by dd-r.acct5:
		/*PUT UNFORMATTED dd-r.currency Format "x(3)" "; " dd-r.bal-acct "; " dd-r.acct5 "; " dd-r.summ SKIP.*/
kolvo = kolvo + 1.
IF LAST-OF (dd-r.currency) THEN DO:
		find first doc-val WHERE doc-val.currency = dd-r.currency NO-ERROR.
		IF AVAIL doc-val THEN doc-val.rdoc = kolvo.
		ELSE DO:
			CREATE doc-val.
			ASSIGN doc-val.currency = dd-r.currency
				doc-val.rdoc = kolvo
				.
		END.
		kolvo = 0.
	END.                      	
END.

/*
PUT UNFORMATTED SKIP(1).
*/

FOR EACH doc-val : 
	for each dd-p WHERE dd-p.currency = doc-val.currency
			and dd-p.kass
		by dd-p.bal-acct by dd-p.acct5:
		kolvo = kolvo + 1.
		CREATE dd.
		ASSIGN  dd.id = kolvo
			dd.currency = dd-p.currency
			dd.psumm = dd-p.summ
			dd.kass  = dd-p.kass
			.
	END.
	kolvo = 0.
	FOR EACH dd-r WHERE dd-r.currency = doc-val.currency
			AND dd-r.kass
		by dd-r.bal-acct by dd-r.acct5:
		kolvo = kolvo + 1.
		FIND FIRST dd where dd.id = kolvo AND dd.kass AND dd.currency EQ dd-r.currency NO-ERROR.
		IF AVAIL dd THEN DO:
			dd.rsumm = dd-r.summ.
		END.
		ELSE DO:
			CREATE dd.
			ASSIGN dd.id = kolvo
				dd.currency = dd-r.currency
				dd.rsumm = dd-r.summ
				dd.kass  = dd-r.kass
				.
		END.
	END.

	kolvo = 0.

	for each dd-p WHERE dd-p.currency = doc-val.currency
			and dd-p.kass EQ NO
		by dd-p.bal-acct by dd-p.acct5:
		kolvo = kolvo + 1.
		CREATE dd.
		ASSIGN  dd.id = kolvo
			dd.currency = dd-p.currency
			dd.psumm = dd-p.summ
			dd.kass  = dd-p.kass
			.
	END.
	kolvo = 0.
	FOR EACH dd-r WHERE dd-r.currency = doc-val.currency
			AND dd-r.kass EQ NO
		by dd-r.bal-acct by dd-r.acct5:
		kolvo = kolvo + 1.
		FIND FIRST dd where dd.id = kolvo AND dd.kass EQ NO AND dd.currency EQ dd-r.currency NO-ERROR.
		IF AVAIL dd THEN DO:
			dd.rsumm = dd-r.summ.
		END.
		ELSE DO:
			CREATE dd.
			ASSIGN dd.id = kolvo
				dd.currency = dd-r.currency
				dd.rsumm = dd-r.summ
				dd.kass  = dd-r.kass
				.
		END.
	END.

	kolvo = 0.
END.



PUT UNFORMATTED "        Информация об итоговых суммах кассовых и мемориальных документов   " SKIP(1).
PUT UNFORMATTED "   ЗА " string(kop-date) "                                       на ___" " листах " SKIP.
PUT UNFORMATTED "-----------------------------------------------------------------------------" SKIP.
PUT UNFORMATTED "|             ПРИХОД                  |            РАСХОД                   |" SKIP.
PUT UNFORMATTED "-----------------------------------------------------------------------------" SKIP.
PUT UNFORMATTED "| Код  |  Кол-во  |  Сумма            | Код  |  Кол-во  |  Сумма            |" SKIP.
PUT UNFORMATTED "|валюты|документов|                   |валюты|документов|                   |" SKIP.
PUT UNFORMATTED "-----------------------------------------------------------------------------" SKIP.
PUT UNFORMATTED "|                             КАССОВЫЕ ДОКУМЕНТЫ                            |" SKIP.
PUT UNFORMATTED "-----------------------------------------------------------------------------" SKIP.

kolvostr = 11.
FOR EACH dd WHERE dd.kass BREAK BY dd.currency:
	IF FIRST-OF (dd.currency) THEN DO:
		PUT UNFORMATTED "| " dd.currency FORMAT "x(4)" " |          |                   | " dd.currency FORMAT "x(4)" " |          |                   | " SKIP.
	END.
	PUT UNFORMATTED "|      |          |" .
	IF dd.psumm > 0 THEN 
		PUT UNFORMATTED dd.psumm FORMAT "->>,>>>,>>>,>>9.99" " |      |          |".
	ELSE PUT UNFORMATTED "                   |      |          |".
	IF dd.rsumm > 0 THEN 
		PUT UNFORMATTED dd.rsumm FORMAT "->>,>>>,>>>,>>9.99" " | " SKIP.
	ELSE PUT UNFORMATTED "                   | " SKIP.

	kolvostr = kolvostr + 1.
	itogp = itogp + dd.psumm.
	itogr = itogr + dd.rsumm.
	if dd.psumm > 0 THEN kolp = kolp + 1. 	
	if dd.rsumm > 0 THEN kolr = kolr + 1.
	IF kolvostr >= 76 THEN DO:
		PUT UNFORMATTED SKIP(2).
		PUT UNFORMATTED "Подпись кассового работника _____________ " SKIP.
		PAGE.
		PUT UNFORMATTED SKIP.
		kolvostr = 1.
	END.


	IF LAST-OF (dd.currency) THEN DO:
		PUT UNFORMATTED "|      | " kolp FORMAT ">>>>>>>9" " |" itogp FORMAT "->>,>>>,>>>,>>9.99" " |      | " kolr FORMAT ">>>>>>>9" " |" itogr FORMAT "->>,>>>,>>>,>>9.99" " | " SKIP.
		PUT UNFORMATTED "-----------------------------------------------------------------------------" SKIP.
	kolvostr = kolvostr + 2.
		itogp = 0.
		itogr = 0.
		kolp = 0.
		kolr = 0.
	END.
	
END.


IF kolvostr >= 74 THEN DO:
	PUT UNFORMATTED SKIP(2).
	PUT UNFORMATTED "Подпись кассового работника _____________ " SKIP.
	PAGE.
	PUT UNFORMATTED SKIP.
	kolvostr = 1.
END.

PUT UNFORMATTED "|                           МЕМОРИАЛЬНЫЕ ДОКУМЕНТЫ                          |" SKIP.
PUT UNFORMATTED "-----------------------------------------------------------------------------" SKIP.
kolvostr = kolvostr + 2.

FOR EACH dd WHERE dd.kass EQ NO BREAK BY dd.currency:
	IF FIRST-OF (dd.currency) THEN DO:
		PUT UNFORMATTED "| " dd.currency FORMAT "x(4)" " |          |                   | " dd.currency FORMAT "x(4)" " |          |                   | " SKIP.
	END.
	PUT UNFORMATTED "|      |          |" .
	IF dd.psumm > 0 THEN 
		PUT UNFORMATTED dd.psumm FORMAT "->>,>>>,>>>,>>9.99" " |      |          |".
	ELSE PUT UNFORMATTED "                   |      |          |".
	IF dd.rsumm > 0 THEN 
		PUT UNFORMATTED dd.rsumm FORMAT "->>,>>>,>>>,>>9.99" " | " SKIP.
	ELSE PUT UNFORMATTED "                   | " SKIP.

	kolvostr = kolvostr + 1.
	itogp = itogp + dd.psumm.
	itogr = itogr + dd.rsumm.
	if dd.psumm > 0 THEN kolp = kolp + 1. 	
	if dd.rsumm > 0 THEN kolr = kolr + 1.
	IF kolvostr >= 76 THEN DO:
		PUT UNFORMATTED SKIP(2).
		PUT UNFORMATTED "Подпись кассового работника _____________ " SKIP.
		PAGE.
		PUT UNFORMATTED SKIP.
		kolvostr = 1.
	END.

	IF LAST-OF (dd.currency) THEN DO:
		PUT UNFORMATTED "|      | " kolp FORMAT ">>>>>>>9" " |" itogp FORMAT "->>,>>>,>>>,>>9.99" " |      | " kolr FORMAT ">>>>>>>9" " |" itogr FORMAT "->>,>>>,>>>,>>9.99" " | " SKIP.
		PUT UNFORMATTED "-----------------------------------------------------------------------------" SKIP(2).
		kolvostr = kolvostr + 2.
		itogp = 0.
		itogr = 0.
		kolp = 0.
		kolr = 0.
	END.
	
END.

		PUT UNFORMATTED "Подпись кассового работника _____________ " SKIP.

{preview.i}

