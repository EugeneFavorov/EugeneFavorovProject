/* Отчет по перечислению в РИНГ */                
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
   DEFINE VARIABLE vRes AS DECIMAL NO-UNDO.
   ERROR-STATUS:ERROR = NO.
   vRes = DECIMAL(iStr) NO-ERROR.
   IF ERROR-STATUS:ERROR = YES THEN vRes = 0.
   RETURN vRes.
END FUNCTION.

DEFINE VARIABLE nameCl as char no-undo.
DEFINE VARIABLE tmpdate as date no-undo.
DEFINE VARIABLE commiss as decimal no-undo init 50.
DEFINE VARIABLE aktnum AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE AmtString as char no-undo.
DEFINE VARIABLE DecStr as decimal no-undo.
DEFINE VARIABLE AmtString2 as char no-undo.
DEFINE VARIABLE CardName as char no-undo.
DEFINE VARIABLE CardNum as char no-undo.
DEFINE VARIABLE CardDate as char no-undo.
DEFINE VARIABLE CardSumm as char no-undo.
DEFINE VARIABLE CardSummD as DECIMAL no-undo.  

DEFINE TEMP-TABLE otchink
   FIELD op_date  AS DATE                 /* дата проводки */
   FIELD acct_cl  AS CHAR                 /* счет заемщика */
   FIELD name_cl  AS CHAR                 /* наименование заемщика */
   FIELD summ     AS DECIMAL              /* сумма op */
	FIELD CardName	AS CHAR 
	FIELD CardNum	AS CHAR
	FIELD CardDate	AS CHAR
	FIELD CardSumm	AS DECIMAL
INDEX acct_cl acct_cl.

{empty otchink}

tmpdate  = date(month(today),1,year(today)).
beg-date = tmpdate.
end-date = date_correct(month(today),0,31,year(today)).

pause 0.

define FRAME frame_date_codes 
   beg-date label "Дата С:"
	end-date label "Дата По:"	
	commiss  label "Коэфф.вознагр"
   aktnum   label "Номер акта "
	with 1  COL 1 down
	 
width 50 CENTERED OVERLAY ROW 10 TITLE "Данные для отчета ".

do ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE beg-date end-date commiss aktnum

   WITH FRAME frame_date_codes
   EDITING:
   READKEY.
   IF LASTKEY EQ KEYCODE("ESC") THEN
	return.
   IF LASTKEY EQ KEYCODE("F1")
		THEN DO:
			CASE FRAME-FIELD:
			WHEN "beg-date" THEN
				DO:
				run calEND.p.
				IF (lastkey eq 13 or
				lastkey eq 10) and
				pick-value ne ?
				THEN frame-value = string(date(pick-value), "99/99/9999").
			END.
			WHEN "end-date" THEN
				DO:
				run calEND.p.
				IF (lastkey eq 13 or
				lastkey eq 10) and
				pick-value ne ?
				THEN frame-value = string(date(pick-value), "99/99/9999").
			END.
			END CASE.
		END.
		ELSE APPLY LASTKEY.
   END. /* EDITING: */
  END.  /* do on */	
HIDE FRAME frame_date_codes NO-PAUSE.

tmpdate = beg-date - 5.

for each op 
   where op.op-date >= beg-date
     and op.op-date <= end-date 
     and op.ben-acct = '40702810000000051790'
	  and op.name-ben = 'ООО "Ринг-М"'
     and op.op-status begins "√" 
   no-lock,
   each op-entry of op WHERE TRUE
   and op-entry.acct-db begins '40817' 
   no-lock:
   nameCl    = ''.
	CardNum   = ''.
	CardName  = ''.
	CardSumm  = ''.
	CardDate  = ''.
	CardSummD = 0.
       
   FIND FIRST acct WHERE acct.acct EQ op-entry.acct-db no-lock no-error.
   IF avail acct THEN RUN GetName(INPUT acct.cust-cat,INPUT acct.cust-id,OUTPUT nameCl).
	
   FIND FIRST otchink WHERE otchink.acct_cl EQ op-entry.acct-db no-lock no-error.
   IF not avail otchink 
   THEN DO:
      FOR EACH loan-acct
           where loan-acct.acct-type EQ 'КредРасч' 
	  	       and loan-acct.acct EQ op-entry.acct-db 
	  	     /*  and loan-acct.since >= tmpdate */
	  	       and loan-acct.since <= end-date
      NO-LOCK:
	     IF avail loan-acct 
         THEN DO:
	        FIND FIRST loan 
                 WHERE loan.contract  EQ loan-acct.contract 
                   and loan.cont-code EQ loan-acct.cont-code 
            no-lock no-error.
	        IF avail loan 
            AND op.details MATCHES "* " + GetXattrValue("loan",loan.contract + ',' + loan.cont-code,"VIP_Assis_cardno") + "*"
            THEN DO: 
            ASSIGN
               CardNum   = GetXattrValue("loan",loan.contract + ',' + loan.cont-code,"VIP_Assis_cardno").
               CardName  = "Concierge+".
               CardDate  = GetXattrValue("loan",loan.contract + ',' + loan.cont-code,"VIP_Assis_date").
               CardSumm  = GetXattrValue("loan",loan.contract + ',' + loan.cont-code,"VIP_Assis_sum").
	           CardSummD = ConvertDecimal(CardSumm).
            .
   
            create otchink.            
            assign
               otchink.op_date  = op.op-date
               otchink.acct_cl  = op-entry.acct-db
               otchink.name_cl  = nameCl
               otchink.summ     = abs(op-entry.amt-rub)
               otchink.CardName = CardName
               otchink.CardNum  = CardNum
               otchink.CardDate = CardDate         
               otchink.CardSumm = CardSummD        
            .
            release otchink.
	        END.
	     END.
      END.
	END.
END.

DEFINE VARIABLE tmpSum     AS DECIMAL NO-UNDO.
DEFINE VARIABLE tmpSumCom  AS DECIMAL NO-UNDO.
DEFINE VARIABLE tmpNum     AS INT64 NO-UNDO.
DEFINE VARIABLE tmpItog    AS DECIMAL NO-UNDO.
DEFINE VARIABLE tmpItogNum AS DECIMAL NO-UNDO.
DEFINE VARIABLE tmpItogCom AS DECIMAL NO-UNDO.
DEFINE VARIABLE tmpDateBeg AS CHAR NO-UNDO.
DEFINE VARIABLE tmpDateEnd AS CHAR NO-UNDO.
DEFINE VARIABLE tmpSumNds  AS DECIMAL NO-UNDO.

tmpItog = 0.
tmpItogNum = 0.
tmpItogCom = 0.

tmpDateEnd = '"' + STRING(DAY(end-date + 1),"99") + '" ' + getMonthString(MONTH(end-date + 1)) + ' ' + STRING(YEAR(end-date + 1),"9999") + ' г.'.
RUN Insert_TTName("dateot",   tmpDateEnd).
RUN Insert_TTName("dateend1", STRING(DAY(end-date), "99")).
RUN Insert_TTName("dateend2", getMonthString(MONTH(end-date)) + ' ' + STRING(YEAR(end-date), "9999") + ' г.').
RUN Insert_TTName("datebeg1", STRING(DAY(beg-date), "99")).
RUN Insert_TTName("datebeg2", getMonthString(MONTH(beg-date)) + ' ' + STRING(YEAR(beg-date), "9999") + ' г.').
RUN Insert_TTName("aktnum",   aktnum).
/*
tmpDateEnd = '"' + STRING(DAY(beg-date),"99") + '" ' + getMonthString(MONTH(beg-date)) + ' ' + STRING(YEAR(beg-date),"9999") + ' г.'.
RUN Insert_TTName("dateend",tmpDateEnd).

tmpDateBeg = '"' + STRING(DAY(beg-date),"99") + '" ' + getMonthString(MONTH(beg-date)) + ' ' + STRING(YEAR(beg-date),"9999") + ' г.'.
RUN Insert_TTName("datebeg",tmpDateBeg).
*/

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
			RUN NextCircle_TTName ("Ring2").

RUN EndCircle_TTName ("Ring2").

run amtstr.p (tmpItog, yes, output AmtString, output DecStr).
AmtString2 = StRing(DecStr,"99") + ' коп.'.

AmtString = AmtString + ' ' + AmtString2.

DEFINE VARIABLE sum1 as char no-undo.
sum1 = '2. Всего по заключенным Заказчиком с привлеченными Исполнителем Клиентами Абонентским договорам на оказание услуг VIP-Аssistance (Программа "Concierge+") в отчетный период сумма оплаты составила ' 
	+ STRING(tmpItog,"->>>,>>>,>>>,>>9.99") + ' (' + AmtString + ')'.


RUN Insert_TTName("summ1", sum1).

sum1 = '3. Итого сумма к перечислению Заказчику составляет ' 
	+ STRING(tmpItog,"->>>,>>>,>>>,>>9.99") + ' (' + AmtString + ')'.

RUN Insert_TTName("summ2", sum1).

run amtstr.p (tmpItogCom, yes, output AmtString, output DecStr).
AmtString2 = StRing(DecStr,"99") + ' коп.'.
AmtString = AmtString + ' ' + AmtString2.


sum1 = '4. Размер причитающегося Исполнителю вознаграждения составляет ' 
	+ STRING(tmpItogCom,"->>>,>>>,>>>,>>9.99") + ' (' + AmtString + ')'
	+ ', в т.ч. НДС '.

run amtstr.p (tmpSumNds, yes, output AmtString, output DecStr).
AmtString2 = StRing(DecStr,"99") + ' коп.'.
AmtString = AmtString + ' ' + AmtString2.
                          
sum1 = sum1 + STRING(tmpSumNds,"->>>,>>>,>>>,>>9.99") + ' (' + AmtString + ')'.

RUN Insert_TTName("summ3", sum1).

RUN printvd.p ("Ring2", INPUT TABLE ttNames).
  
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
  
  
