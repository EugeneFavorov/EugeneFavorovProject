/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: prezent_allow.p
      Comment: проверка на допустимость выдачи золотого слитка
   Parameters:
         Uses:
      Used by:
      Created:
     Modified: 07/04/2014 kau продление акции
            30/10/2014  kau исправил для следующей акции. В принцпие если часто будут и они будут пересекаться можно будет сделать временную таблицу в которую заносить условия всех акций и с ней уже работать.
*/


{globals.i}
{sh-defs.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get blkob}

DEFINE INPUT PARAMETER iPersonId  AS INT64 NO-UNDO.
DEFINE INPUT PARAMETER iDateOp    AS DATE NO-UNDO.

DEF VAR mCrtAcct    AS CHAR     NO-UNDO.
DEF VAR mRid        AS RECID    NO-UNDO.
DEF VAR mOutRid     AS RECID    NO-UNDO.
DEF VAR h_kau       AS HANDLE   NO-UNDO.

DEF VAR pOk         AS LOG      NO-UNDO.
DEF VAR dt          AS DATE     NO-UNDO.
DEF VAR ost         AS DEC      NO-UNDO.
DEF VAR ost1        AS DEC      NO-UNDO.
DEF VAR cred        AS DEC      NO-UNDO.
DEF VAR DpsYear     AS INT      NO-UNDO.
DEF VAR CntPodarkov AS INT      NO-UNDO.
DEF VAR closed-date AS DATE     NO-UNDO.    /*последний закрытый опердень*/
DEF VAR dt1         AS DATE     NO-UNDO.
DEF VAR dt2         AS DATE     NO-UNDO.

DEF VAR cDaysForDT       AS INT     NO-UNDO.
DEF VAR cSummForDT       AS INT     NO-UNDO.
DEF VAR cBegActDate      AS DATE    NO-UNDO.
DEF VAR cEndActDate      AS DATE    NO-UNDO.
DEF VAR cEndVidDate      AS DATE    NO-UNDO.
DEF VAR cBegVidDate      AS DATE    NO-UNDO.
DEF VAR cListProduct     AS CHAR    NO-UNDO.
DEF VAR cListSurrogateOp AS CHAR    NO-UNDO.    cListSurrogateOp = "".

/* количество дней с дня открытия вклада, после которого можно выдать слиток */
DEF VAR cDaysForDTMass   AS INT  EXTENT 4 NO-UNDO INIT [31, 31, 31, 31].
/* сумма для выдачи слитка */
DEF VAR cSummForDTMass   AS DEC  EXTENT 4 NO-UNDO INIT [500000, 300000, 500000, 500000].
/* дата начала действия акции*/
DEF VAR cBegActDateMass  AS DATE EXTENT 4 NO-UNDO.
cBegActDateMass[1] = DATE('05/11/2014').
cBegActDateMass[2] = DATE('19/12/2014').
cBegActDateMass[3] = DATE('14/01/2015').
cBegActDateMass[4] = DATE('16/06/2015').
/* дата окончания акции */
DEF VAR cEndActDateMass  AS DATE EXTENT 4 NO-UNDO.
cEndActDateMass[1] = DATE('18/12/2014').
cEndActDateMass[2] = DATE('13/01/2015').
cEndActDateMass[3] = DATE('29/01/2015').
cEndActDateMass[4] = DATE('31/12/2015').
/* дата начала выдачи слитков */
DEF VAR cEndVidDateMass  AS DATE EXTENT 4 NO-UNDO.
cEndVidDateMass[1] = DATE('31/05/2015').
cEndVidDateMass[2] = DATE('31/05/2015').
cEndVidDateMass[3] = DATE('31/05/2015').
cEndVidDateMass[4] = DATE('29/02/2016').
/*cEndVidDateMass[4] = DATE('29/03/2016').*/
/* типы договоров для акции */
DEF VAR cListProductMass AS CHAR EXTENT 4  NO-UNDO.
cListProductMass[1] = "НадежныйПлюс,НадежПлКарт,Пенсион*,gold_card_st,gold_dv_st".
cListProductMass[2] = "gold_card_po,gold_dv_po,НадежПлюсПов,НадПлПовКарт,НадПлюсДовНе,НадПлюсКарНе,gold_card_no,gold_dv_no,Пенсион*".
cListProductMass[3] = "*".
cListProductMass[4] = "НадежныйПлюс,НадПлюсКарНе,НадежПлКарт".
DEF VAR i AS INT NO-UNDO.

DEFINE BUFFER acct1 FOR acct.
DEFINE BUFFER acct2 FOR acct.

IF iDateOp > cEndVidDateMass [ 1 ] AND
   iDateOp > cEndVidDateMass [ 2 ] AND
   iDateOp > cEndVidDateMass [ 3 ] AND
   iDateOp > cEndVidDateMass [ 4 ]
    THEN DO:
	RETURN "Акции закончены".
END.

/* ищем вкладные договора по клиенту за период действия акции */

DO i=1 TO 4:
       cDaysForDT = cDaysForDTMass[i].
       cSummForDT = cSummForDTMass[i].
       cBegActDate = cBegActDateMass[i].
       cEndActDate = cEndActDateMass[i].
       cEndVidDate = cEndVidDateMass[i].
       cBegVidDate = DATE(INT64(cBegActDateMass[i]) + cDaysForDTMass[i]).
       cListProduct = cListProductMass[i].
       
       CntPodarkov = 0.
       pOk = FALSE.

  FOR EACH acct1 
	  WHERE acct1.cust-cat = "Ч" AND 
		acct1.cust-id EQ iPersonId AND
		acct1.open-date >= cBegActDate AND
		acct1.open-date <= cEndActDate  AND
		CAN-DO( "4230.810*,4260.810*", acct1.acct) AND
		NOT CAN-DO("42301*,42601*", acct1.acct)
	  NO-LOCK,
	  FIRST loan-acct 
	  	WHERE loan-acct.acct EQ acct1.acct NO-LOCK,
	  FIRST loan OF loan-acct 
        WHERE CAN-DO(cListProduct, loan.cont-type) AND
		loan.contract = "dps"
		/* золотой плюс стандарт в 1ю акцию включили только с 25.11.2014 */
		AND ( CAN-DO( "!gold_card_st,!gold_dv_st,*", loan.cont-type) OR loan.open-date >= DATE( '25/11/2014'))
		NO-LOCK:
		    

     /*за период 91 дня до открытия у клиента отсутствуют вкладные счета с ненулевыми остатками*/
     IF i=3 THEN DO:    
       dt1=DATE(INT64(acct1.open-date) - 1).
       dt2=DATE(INT64(acct1.open-date) - 91).
       ost1=0.
       cred=0.
       FOR EACH acct2 
        WHERE acct2.cust-cat = "Ч" AND 
          acct2.cust-id EQ iPersonId AND
          acct2.open-date <= dt1   AND
          acct2.filial-id = acct1.filial-id AND
         (acct2.close-date = ? OR acct2.close-date>=dt2) AND
          CAN-DO( "4230*,4260*", acct2.acct) AND
          NOT CAN-DO( "42301*,42601*", acct2.acct)
          NO-LOCK: 
          RUN acct-pos IN h_base (acct2.acct,acct2.currency,dt2,dt1,?).
               IF acct2.currency EQ "" THEN DO:
                  ost1 = sh-bal.
                  cred = sh-cr.
               END.   
               ELSE DO:
                  ost1 = sh-val. 
                  cred = sh-vcr.
               END.  
                   
          IF ost1 NE 0 OR 
             cred NE 0 THEN DO:
              LEAVE.
          END.             
       END.
       
       IF ost1 NE 0 OR
          cred NE 0 THEN DO:
           LEAVE.
       END.
     END.
      	    

	/* смотрим был ли по счету остаток более cSummForDT более cDaysForDT дня */
	  pOk = FALSE.
	  dt = ?.
	  ost = 0.
	  FOR EACH acct-pos OF acct1 NO-LOCK BY acct-pos.since:
	  	ost = acct-pos.balance.
		IF - acct-pos.balance >= cSummForDT AND dt EQ ? THEN DO:
			dt = acct-pos.since.
		END.
		IF - acct-pos.balance < cSummForDT AND dt NE ? THEN DO:
			IF acct-pos.since - dt >= cDaysForDT THEN DO:
				pOk = TRUE.
				LEAVE.
			END.
			dt = ?.
		END.
	  END.
	  closed-date = FGetLastClsDate(?,'o').

	  IF dt NE ? AND closed-date - dt + 1 >= cDaysForDT THEN DO:
		  pOk = TRUE.
		  LEAVE.
	  END.
	  IF NOT pOk THEN DO:
		/* считаем и проводки по незакрытым дням */
		  FOR EACH op-entry 
		   	WHERE op-entry.op-date > closed-date AND 
				(op-entry.acct-db = acct1.acct OR op-entry.acct-cr = acct1.acct) NO-LOCK BREAK BY op-entry.op-date:
			ost = ost + (IF op-entry.acct-db = acct1.acct THEN op-entry.amt-rub ELSE - op-entry.amt-rub).
			IF LAST-OF(op-entry.op-date) THEN DO:

				IF (- ost) >= cSummForDT AND dt EQ ? THEN DO:
					dt = op-entry.op-date.

				END.
				IF (- ost) < cSummForDT AND dt NE ? THEN DO:
					IF op-entry.op-date - dt >= cDaysForDT THEN DO:
						pOk = TRUE.
						LEAVE.
					END.
					dt = ?.
				END.
			END.
		  END.
		  IF dt NE ? AND iDateOp - dt + 1 >= cDaysForDT THEN DO:
		    	pOk = TRUE.
		    	LEAVE.
		  END.
	  END.
  END.

  IF pOk THEN CntPodarkov = CntPodarkov + 1.
  IF CntPodarkov > 0 THEN DO:
	    /* а теперь ищем выдавали ли этому клиенту слиток */
	    FOR EACH signs
		 WHERE signs.file-name = "op"
		   AND signs.code = "ФЛподарок"
		   AND signs.code-value = STRING(iPersonId)
		   AND NOT CAN-DO(cListSurrogateOp, signs.surrogate) NO-LOCK:
			FIND FIRST op
			 WHERE op.op EQ INT(signs.surrogate)
			   AND op.op-date >= cBegVidDate
			   NO-LOCK.
			IF op.op-date NE ? THEN DO:
				CntPodarkov = CntPodarkov - 1.
				cListSurrogateOp = cListSurrogateOp + ',' + STRING(op.op).
			END.
		END.
  END.
  IF CntPodarkov > 0 THEN DO:
     RETURN "Да".
  END.
END.
  
IF pOk OR LENGTH(cListSurrogateOp) > 0 THEN
   RUN Fill-SysMes IN h_tmess ("", "", "0", "клиенту " + STRING(iPersonId) + " подарки по текущим акциям уже выдавались.").
  
RETURN ("У клиента " + STRING(iPersonId) + " нет вкладов для выдачи подарка."
    ).

{intrface.del}
