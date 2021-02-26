/* Отчет по перечислению в Центр Ассистанс */                
/* kam */



{globals.i}
{pp-corr.p}
{prn-doc.def &with_proc=YES}
{date.fun}
{intrface.get xclass}   /* Инструменты для работы с метасхемой.  */

/* Проверяет, является ли строка - числом */
FUNCTION ConvertDecimal RETURN DECIMAL (iStr AS CHAR):
   iStr = replace(iStr,' ','').
   iStr = replace(iStr,',','.').
   DEF VAR vRes AS DECIMAL NO-UNDO.
   ERROR-STATUS:ERROR = NO.
   vRes = DECIMAL(iStr) NO-ERROR.
   IF ERROR-STATUS:ERROR = YES THEN vRes = 0.
   RETURN vRes.
END FUNCTION.

DEFINE VARIABLE nameCl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpdate    AS DATE NO-UNDO.
DEFINE VARIABLE commiss    AS DECIMAL NO-UNDO INIT 50.
DEFINE VARIABLE aktnum     AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE AmtString  AS CHARACTER NO-UNDO.
DEFINE VARIABLE DecStr     AS DECIMAL NO-UNDO.
DEFINE VARIABLE AmtString2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE CardName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE CardNum    AS CHARACTER NO-UNDO.
DEFINE VARIABLE CardDate   AS CHARACTER NO-UNDO.
DEFINE VARIABLE CardSumm   AS CHARACTER NO-UNDO.
DEFINE VARIABLE CardSummD  AS DECIMAL NO-UNDO.  

DEFINE TEMP-TABLE otchink
   FIELD op_date  AS DATE                 /* дата проводки */
   FIELD acct_cl  AS CHAR                 /* счет заемщика */
   FIELD name_cl  AS CHAR                 /* наименование заемщика */
   FIELD summ     AS DECIMAL              /* сумма op */
	FIELD CardName	AS CHAR 
	FIELD CardNum	AS CHAR
	FIELD CardDate	AS CHAR
	FIELD CardSumm	AS DECIMAL
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
	 commiss  label "Коэфф.вознагр"
    aktnum   label "Номер акта "
	 with 1 COL 1 down
	 
width 50 CENTERED OVERLAY ROW 10 TITLE "Данные для отчета ".

  do ON ERROR UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE beg-date end-date commiss aktnum

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
HIDE FRAME frame_date_codes.

def var newcode as char no-undo.

tmpdate = beg-date - 5.

  for each op where 
        op.op-date >= beg-date
        and op.op-date <= end-date 
        and (op.ben-acct = '40702810934000000024')
        and op.op-status begins "√" 
         no-lock,
        each op-entry of op where op-entry.acct-db begins '40817' no-lock:
       newcode = ''.
       nameCl = ''.
	    CardNum = ''.
	    CardName = ''.
	    CardSumm = ''.
	    CardDate = ''.
	    CardSummD = 0.
            find last acct where acct.acct = op-entry.acct-db and acct.filial-id <> '0400' and acct.close-date = ? no-lock no-error.
            if avail acct then RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT nameCl).

	    find first otchink where otchink.acct_cl = op-entry.acct-db no-lock no-error.
        if not avail otchink then do:
	    for each loan-acct where loan-acct.acct-type = 'КредРасч' 
			and loan-acct.acct = op-entry.acct-db 
			no-lock by loan-acct.since desc:
             	newcode = loan-acct.cont-code.
             	leave.
            end.
	    if newcode <> '' then do:

	        find last loan where loan.contract = 'Кредит' and loan.cont-code = newcode no-lock no-error.
		if avail loan then do:
                   CardNum = GetXattrValue("loan",loan.contract + ',' + loan.cont-code,"BlackEdit_dogno").
                   CardName = 'Black Edition+'.
                   CardDate = GetXattrValue("loan",loan.contract + ',' + loan.cont-code,"BlackEdit_date").
                   CardSumm = GetXattrValue("loan",loan.contract + ',' + loan.cont-code,"BlackEdit_sum").
		   CardSummD = ConvertDecimal(CardSumm).
		end.
	   end.
	      create otchink.            
         assign
	         otchink.op_date = op.op-date
            otchink.acct_cl = op-entry.acct-db
	         otchink.name_cl = nameCl
            otchink.summ = abs(op-entry.amt-rub)
			   otchink.CardName = CardName
			   otchink.CardNum = CardNum
			   otchink.CardDate = CardDate			
			   otchink.CardSumm = CardSummD			
         .
	      release otchink.

/*	message name_cl view-as alert-box.
	message summ view-as alert-box.
	message cardname view-as alert-box.
	message cardnum view-as alert-box.
	message cardsumm view-as alert-box.
	message op-entry.acct-db view-as alert-box.
*/
	end.
end.

DEF VAR tmpSum AS DECIMAL NO-UNDO.
DEF VAR tmpSumCom AS DECIMAL NO-UNDO.
DEF VAR tmpNum AS INT64 NO-UNDO.
DEF VAR tmpItog AS DECIMAL NO-UNDO.
DEF VAR tmpItogNum AS DECIMAL NO-UNDO.
DEF VAR tmpItogCom AS DECIMAL NO-UNDO.
DEF VAR tmpDateBeg as CHAR NO-UNDO.
DEF VAR tmpDateEnd as CHAR NO-UNDO.
DEF VAR tmpSumNds AS DECIMAL NO-UNDO.

tmpItog = 0.
tmpItogNum = 0.
tmpItogCom = 0.

RUN Insert_TTName("dateend",  STRING(DAY(end-date), "99")).
RUN Insert_TTName("dateend2", getMonthString(MONTH(end-date)) + ' ' + STRING(YEAR(end-date), "9999") + ' г.').
RUN Insert_TTName("datebeg",  STRING(DAY(beg-date), "99")).
RUN Insert_TTName("datebeg2", getMonthString(MONTH(beg-date)) + ' ' + STRING(YEAR(beg-date), "9999") + ' г.').
RUN Insert_TTName("dateot",   STRING(DAY(TODAY), "99")).
RUN Insert_TTName("dateot2",  getMonthString(MONTH(TODAY)) + ' ' + STRING(YEAR(TODAY), "9999") + ' г.').
RUN Insert_TTName("aktnum",   aktnum).

tmpNum = 0.
RUN BeginCircle_TTName ("Ring2").
	FOR EACH otchink NO-LOCK BY otchink.op_date:
		tmpNum = tmpNum + 1.
	 	RUN Insert_TTName("num[Ring2]",STRING(tmpNum)).
	 	RUN Insert_TTName("CardNum[Ring2]",otchink.CardNum).
	 	RUN Insert_TTName("CardName[Ring2]",otchink.CardName).
	 	RUN Insert_TTName("CardDate[Ring2]",otchink.CardDate).
	 	RUN Insert_TTName("namecl[Ring2]",otchink.name_cl).
 		RUN Insert_TTName("summ[Ring2]",STRING(otchink.CardSumm,"->>>,>>>,>>>,>>9.99")).
      tmpSumCom = otchink.CardSumm * commiss / 100.
 		RUN Insert_TTName("summCom[Ring2]",STRING(tmpSumCom,"->>>,>>>,>>>,>>9.99")).
		RUN NextCircle_TTName ("Ring2").
		tmpItog = tmpItog + otchink.CardSumm.
		tmpItogCom = tmpItogCom + tmpSumCom.

	END.
	tmpSumNds = tmpItogCom * 18 / 118.
	RUN Insert_TTName("num[Ring2]",'').
	RUN Insert_TTName("CardNum[Ring2]","ИТОГО:").
	RUN Insert_TTName("CardName[Ring2]",'').
	RUN Insert_TTName("CardDate[Ring2]",'').
	RUN Insert_TTName("namecl[Ring2]",'').
	RUN Insert_TTName("summ[Ring2]",STRING(tmpItog,"->>>,>>>,>>>,>>9.99")).
	RUN Insert_TTName("summCom[Ring2]",STRING(tmpItogCom,"->>>,>>>,>>>,>>9.99")).
	RUN NextCircle_TTName("Ring2").

RUN EndCircle_TTName ("Ring2").

run amtstr.p (tmpItog, yes, output AmtString, output DecStr).
AmtString2 = StRing(DecStr,"99") + ' коп.'.

AmtString = AmtString + ' ' + AmtString2.

def var sum1 as char no-undo.
sum1 = '2. Всего по заключенным Заказчиком с привлеченными Исполнителем Клиентами договорам VIP-assistance (Программа "Black Edition+") в отчетный период сумма оплаты составила ' 
	+ TRIM(STRING(tmpItog,"->>>,>>>,>>>,>>9.99")) + ' (' + AmtString + ')'.

RUN Insert_TTName("summ1", sum1).

sum1 = '3. Итого сумма к перечислению Заказчику составляет ' 
	+ TRIM(STRING(tmpItog,"->>>,>>>,>>>,>>9.99")) + ' (' + AmtString + ')'.

RUN Insert_TTName("summ2", sum1).

run amtstr.p (tmpItogCom, yes, output AmtString, output DecStr).
AmtString2 = StRing(DecStr,"99") + ' коп.'.
AmtString = AmtString + ' ' + AmtString2.

sum1 = '4. Размер причитающегося Исполнителю вознаграждения составляет ' 
	+ TRIM(STRING(tmpItogCom,"->>>,>>>,>>>,>>9.99")) + ' (' + AmtString + ')'
	+ ', в т.ч. НДС '.

run amtstr.p (tmpSumNds, yes, output AmtString, output DecStr).
AmtString2 = StRing(DecStr,"99") + ' коп.'.
AmtString = AmtString + ' ' + AmtString2.
                          
sum1 = sum1 + TRIM(STRING(tmpSumNds,"->>>,>>>,>>>,>>9.99")) + ' (' + AmtString + ')'.

RUN Insert_TTName("summ3", sum1).

RUN printvd.p ("Ring2Black", INPUT TABLE ttNames).

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
  
  
