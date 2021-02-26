{tmprecid.def}
{globals.i}
{client.i}

def temp-table avtoz NO-UNDO
  field NN		AS INT64
  field MM		as char
  field VIN		as char
  field Ndvig		as char
  field year		as char
  field vcolor		as char
  field Nhassi		as char
  field strana		as char
  field city		as char
  field Dzalog		as date
  field FIO		as char
  field NDog		as char
  field CID 		as CHAR
  field DEndZal		as CHAR

.

DEF VAR GNI	AS CHAR		NO-UNDO.
DEF VAR vDataCl	AS INT64	NO-UNDO.
DEF VAR str	AS CHAR		NO-UNDO.
DEF VAR surr	AS CHAR		NO-UNDO.
DEF VAR nn	AS INT64	NO-UNDO.

{getdates.i}

		find first code where code.class = '' and code.code = 'GNICity' NO-LOCK NO-ERROR.
		if avail code then vDataCl = INT64(code.misc[8]).
		FIND FIRST DataBlock WHERE DataBlock.Data-ID EQ vDataCl NO-LOCK NO-ERROR.
		IF vDataCl                EQ 0
		OR vDataCl                EQ ?
		OR NOT AVAILABLE DataBlock THEN DO:
			MESSAGE COLOR ERROR
				"Не существует ссылка на класс данных"	SKIP
				"Или эта ссылка неверна."		SKIP
				"Проверте code.misc[8] GNICity"
				VIEW-AS ALERT-BOX ERROR.
		END.

for each term-obl where
    term-obl.class-code = "term-obl-gar" and
    term-obl.idnt = 5
    AND term-obl.fop-date >= beg-date AND term-obl.fop-date <= end-date 
/*    AND (term-obl.CONT-CODE = '60-00-18892-АПК@0400' OR term-obl.cont-code = '45-00-16720-АПК@0400')*/
	no-lock,
first loan of term-obl where (loan.close-date eq ? or loan.close-date >= today) no-lock:

put screen col 1 row screen-lines + message-lines + 1
                                color bright-blink-normal "(/)".
	FIND FIRST loan-acct where loan-acct.cont-code EQ loan.cont-code and loan-acct.contract EQ loan.contract
				AND loan-acct.acct BEGINS '455' NO-LOCK NO-ERROR.
put screen col 1 row screen-lines + message-lines + 1
                                color bright-blink-normal "(/)/".
	IF not AVAIL loan-acct THEN NEXT.
	ELSE DO:
put screen col 1 row screen-lines + message-lines + 1
                                color bright-blink-normal "(/)-".
		/*FIND FIRST op-entry
		 where op-entry.op-date >= beg-date
		   AND op-entry.op-date <= end-date
		   AND op-entry.op-status BEGINS "√" 
		   AND (op-entry.acct-cr EQ loan-acct.acct OR op-entry.acct-db EQ loan-acct.acct)
		    NO-LOCK NO-ERROR.*/
		FIND FIRST op-entry
		 where op-entry.op-date >= beg-date
		   AND op-entry.op-date <= end-date
		   AND op-entry.op-status BEGINS "√" 
		   AND (op-entry.acct-db EQ loan-acct.acct)
		    NO-LOCK NO-ERROR.
put screen col 1 row screen-lines + message-lines + 1
                                color bright-blink-normal "(/)\\".
		IF not AVAIL op-entry then NEXT.
	END.

	 


	surr = loan.contract + "," + loan.cont-code + ",5," + string(loan.end-date,"99/99/99") + "," + string(term-obl.nn)  .
	str = GetXattrValueEx("term-obl", surr,"ВидОб","*").
	if str = "Автомобиль" then do:
		nn = nn + 1.
		CREATE avtoz.
		ASSIGN avtoz.MM = GetXattrValue("term-obl", surr,"TCmodel")
			avtoz.VIN = GetXattrValue("term-obl", surr,"TCVIN")
			avtoz.Ndvig = GetXattrValue("term-obl", surr,"TCmotor")
			avtoz.year = GetXattrValue("term-obl", surr,"TCyear")
			avtoz.vcolor = GetXattrValue("term-obl", surr,"TCcolor")
			avtoz.Nhassi = GetXattrValue("term-obl", surr,"TCchassis")
			avtoz.Dzalog = term-obl.fop-date
			avtoz.strana = 'РФ'
			avtoz.Ndog = loan.doc-ref
			avtoz.NN = nn
			.
		RUN RE_CLIENT (loan.cust-cat, loan.cust-id, INPUT-OUTPUT avtoz.FIO).

		/*CID*/
		IF loan.cust-cat = 'Ю' then DO:
			avtoz.CID = GetXattrValueEx("cust-corp", string(loan.cust-id),"CID","*").
		END.
		IF loan.cust-cat = 'Ч' then DO:
			avtoz.CID = GetXattrValueEx("person", string(loan.cust-id),"CID","*").
		END.

		/*Вычисляем город выдачи кредита*/
		GNI = GetXattrValue("branch", string(loan.branch-id),"tax-insp").
put screen col 1 row screen-lines + message-lines + 1
                                color bright-blink-normal "-".
		FIND FIRST DataLine WHERE DataLine.Data-id = datablock.data-id and DataLine.txt BEGINS GNI NO-LOCK NO-ERROR.
		if avail DataLine then avtoz.city = string(DataLine.sym2).

		FIND LAST loan-acct WHERE loan-acct.cont-code = loan.cont-code and loan-acct.contract EQ loan.contract
					and loan-acct.acct-type BEGINS 'КредОб'
					AND loan-acct.acct begins '913' NO-LOCK NO-ERROR.
		IF AVAIL loan-acct THEN DO:
			FOR EACH op-entry where op-entry.acct-db = loan-acct.acct 
						AND op-entry.op-status begins '√' NO-LOCK BY op-entry.op-date DESC:
				avtoz.DEndZal = string(op-entry.op-date).
				LEAVE.
			END. /*op-entry*/
		END. /*loan-acct*/
put screen col 1 row screen-lines + message-lines + 1
                                color bright-blink-normal "\\".
	END.
put screen col 1 row screen-lines + message-lines + 1
                                color bright-blink-normal "|".
END.

{avtozalog.i}