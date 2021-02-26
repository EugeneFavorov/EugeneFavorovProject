/* Отчет по перечислению в РИНГ */
/* kam */



{globals.i}
{pp-corr.p}
{prn-doc.def &with_proc=YES}
{date.fun}

DEF VAR nameCl as char no-undo.
DEF VAR tmpdate as date no-undo.
def var commiss as decimal no-undo init 70.
def var AmtString as char no-undo.
def var DecStr as decimal no-undo.
def var AmtString2 as char no-undo.

  
DEFINE TEMP-TABLE otchink
        FIELD op_date      AS DATE                 /* дата проводки */
        FIELD acct_cl      AS CHAR                 /* счет заемщика */
        FIELD name_cl      AS CHAR                 /* наименование заемщика */
        FIELD summ         AS DECIMAL              /* сумма инкасации */
        INDEX acct_cl acct_cl       
    .

{empty otchink}

tmpdate = date(month(today),1,year(today)).
beg-date = tmpdate.
end-date = date_correct(month(today),0,31,year(today)).


pause 0.

define FRAME frame_date_codes 
         beg-date label "Дата С:"
	 end-date label "Дата По:"	
	 commiss label "Коэфф.вознагр"
	 with 1  COL 1 down
	 
width 50 CENTERED OVERLAY ROW 10 TITLE "Данные для отчета ".

  do ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE
	beg-date
	end-date
	commiss

   WITH FRAME frame_date_codes
   EDITING:
   READKEY.
   if LASTKEY EQ KEYCODE("ESC") THEN
	return.
   if LASTKEY EQ KEYCODE("F1")
		THEN do:
			CASE FRAME-FIELD:
			WHEN "beg-date" THEN
				DO:
				run calend.p.
				if (lastkey eq 13 or
				lastkey eq 10) and
				pick-value ne ?
				then frame-value = string(date(pick-value), "99/99/9999").
			END.
			WHEN "end-date" THEN
				DO:
				run calend.p.
				if (lastkey eq 13 or
				lastkey eq 10) and
				pick-value ne ?
				then frame-value = string(date(pick-value), "99/99/9999").
			end.
			END CASE.

		end.
		ELSE APPLY LASTKEY.


   end. /* EDITING: */
  end.  /* do on */	



  for each op where 
        op.op-date >= beg-date 
        and op.op-date <= end-date 
        and (op.ben-acct = '40702810122000011430' OR op.ben-acct = '40702810000000051790')
	and op.name-ben = 'ООО "Ринг-М"'
        and op.op-status begins "√" 
         no-lock,
        each op-entry of op where op-entry.acct-db begins '40817' no-lock:

            nameCl = ''.
            find first acct where acct.acct = op-entry.acct-db no-lock no-error.
            if avail acct then RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT nameCl).
            create otchink.            
            assign
                otchink.op_date = op.op-date
                otchink.acct_cl = op-entry.acct-db
                otchink.name_cl = nameCl
                otchink.summ = abs(op-entry.amt-rub)
                .
            release otchink.
end.

DEF VAR tmpSum AS DECIMAL NO-UNDO.
DEF VAR tmpSumCom AS DECIMAL NO-UNDO.
DEF VAR tmpNum AS INT64 NO-UNDO.
DEF VAR tmpItog AS DECIMAL NO-UNDO.
DEF VAR tmpItogNum AS DECIMAL NO-UNDO.
DEF VAR tmpItogCom AS DECIMAL NO-UNDO.
DEF VAR tmpDateBeg as CHAR NO-UNDO.
DEF VAR tmpDateEnd as CHAR NO-UNDO.

tmpItog = 0.
tmpItogNum = 0.
tmpItogCom = 0.

tmpDateEnd = '"' + STRING(DAY(end-date + 1),"99") + '" ' + getMonthString(MONTH(end-date + 1)) + ' ' + STRING(YEAR(end-date + 1),"9999") + ' г.'.
RUN Insert_TTName("dateot",tmpDateEnd).

tmpDateEnd = '"' + STRING(DAY(beg-date),"99") + '" ' + getMonthString(MONTH(beg-date)) + ' ' + STRING(YEAR(beg-date),"9999") + ' г.'.
RUN Insert_TTName("dateend",tmpDateEnd).


tmpDateBeg = '"' + STRING(DAY(beg-date),"99") + '" ' + getMonthString(MONTH(beg-date)) + ' ' + STRING(YEAR(beg-date),"9999") + ' г.'.
RUN Insert_TTName("datebeg",tmpDateBeg).


RUN BeginCircle_TTName ("Ring1").
	FOR EACH otchink NO-LOCK BREAK BY otchink.op_date:
		IF FIRST-OF (otchink.op_date) THEN DO:
			tmpSum = 0.
			tmpNum = 0.
			tmpSumCom = 0.
		END.
		tmpSum = tmpSum + otchink.summ.
		tmpNum = tmpNum + 1.
		IF LAST-OF (otchink.op_date) THEN DO:
			tmpItog = tmpItog + tmpSum.
			tmpItogNum = tmpItogNum + tmpNum.
			tmpSumCom = tmpSum * commiss / 100.
			tmpItogCom = tmpItogCom + tmpSumCom.
	 	       	RUN Insert_TTName("dater[Ring1]",STRING(otchink.op_date,"99.99.9999")).
	 	       	RUN Insert_TTName("num[Ring1]",STRING(tmpNum)).
 		       	RUN Insert_TTName("summ[Ring1]",STRING(tmpSum,"->>>,>>>,>>>,>>9.99")).
 		       	RUN Insert_TTName("summCom[Ring1]",STRING(tmpSumCom,"->>>,>>>,>>>,>>9.99")).
			RUN NextCircle_TTName ("Ring1").
	        END.
	END.

	 	       	RUN Insert_TTName("dater[Ring1]","ИТОГО:").
	 	       	RUN Insert_TTName("num[Ring1]",STRING(tmpItogNum)).
 		       	RUN Insert_TTName("summ[Ring1]",STRING(tmpItog,"->>>,>>>,>>>,>>9.99")).
 		       	RUN Insert_TTName("summCom[Ring1]",STRING(tmpItogCom,"->>>,>>>,>>>,>>9.99")).
			RUN NextCircle_TTName ("Ring1").





RUN EndCircle_TTName ("Ring1").

run amtstr.p (tmpItogCom, yes, output AmtString, output DecStr).
AmtString2 = StRing(DecStr,"99") + ' коп., НДС не облагается'.

AmtString = AmtString + ' ' + AmtString2.

RUN Insert_TTName("summprop", AmtString).
RUN Insert_TTName("summpropcop", AmtString2).
RUN printvd.p ("Ring1", INPUT TABLE ttNames).
  

PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 
 IF cat = "Ч" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN
    /* ФИО клиента */
    sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN
    /* наименование организации */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
  END.
END.
  
  
