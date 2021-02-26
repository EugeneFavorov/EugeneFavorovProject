/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: rsrv-chk.p
      Comment: Исключение из временной таблицы тех договоров, по которым
               нет оплаты в текущий день.

   Parameters:
      Used by:
      Created: 
     Modified: xaro 01.12.2005 заявка 0054846
*/

{globals.i}
{flt-file.i}        /* Объявление фильтра по договорам */
{all_note.def}      /* Таблица с recid, выбранных по фильтру записей Shared
                       и датой текущего опер.дня - in_op_date */
{sh-defs.i}

{intrface.get olap}
{intrface.get loan}
{intrface.get pint}
{checkov.i}

FUNCTION loan-ost returns decimal (
    INPUT vContract AS CHAR,
    INPUT vContCode AS CHAR,
    INPUT vSince AS DATE,
    INPUT vAcctType AS CHAR):
    
    DEF VAR vRes AS DEC NO-UNDO.
    FIND LAST loan-acct
        WHERE loan-acct.contract EQ vContract
          AND loan-acct.cont-code EQ vContCode
          AND loan-acct.acct-type EQ vAcctType
          AND loan-acct.since <= vSince
        NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
      RUN acct-pos IN h_base (loan-acct.acct,
                           loan-acct.currency,
                           vSince,
                           vSince,
                           "√"            /* = статус "крыж" */
                           ).
      find first acct of loan-acct no-lock.
      vRes = (IF acct.side EQ "А" THEN sh-bal ELSE (- sh-bal)).
    END. ELSE vRes = 0.
    RETURN vRes.
end function.

FUNCTION fu-par returns decimal (
    BUFFER iloan FOR loan,
    INPUT vSince AS DATE,
    INPUT vParm AS INT):

    DEF VAR vAmt AS DEC NO-UNDO.
    DEF VAR vAmt2 AS DEC NO-UNDO.
    DEF VAR vAmt3 AS DEC NO-UNDO.
    DEF VAR vDbSumDec AS DEC NO-UNDO.
    DEF VAR vCrSumDec AS DEC NO-UNDO.
    DEF VAR vTmpCurr        AS CHAR   NO-UNDO.

    run STNDRT_PARAM in h_loan (
	iloan.contract, iloan.cont-code, vParm,
	IF iloan.since >= vSince THEN vSince ELSE iloan.since,
                              output vAmt, output vDbSumDec, output vCrSumDec).
    RUN inter_current(BUFFER iloan, vParm, OUTPUT vAmt2).
    vAmt = vAmt + vAmt2.

    IF iloan.since < vSince THEN DO:
	/* надо добавить операции за будущие дни */
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-d = vParm NO-LOCK:
	      vAmt = vAmt + loan-int.amt-rub.
	  END.
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-k = vParm NO-LOCK:
	      vAmt = vAmt - loan-int.amt-rub.
	  END.
    END.
    RETURN vAmt.
END.

DEFINE INPUT PARAMETER iParam  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER iOpRid  AS RECID  NO-UNDO.

DEF VAR mFlDel AS LOG NO-UNDO.
def var vkol-bal-tot as int no-undo.
def var vkol-bal as int no-undo.
DEF VAR vR AS LOG NO-UNDO.

DEF BUFFER bloan FOR loan.
DEF BUFFER xterm-obl FOR term-obl.

/* message "запуск по плановой дате " + STRING(in_op_date) view-as alert-box.*/

/* проверка количества отобранных записей */
FIND LAST all_recids NO-ERROR.
IF NOT AVAIL all_recids THEN
DO:
   {intrface.del }
   RETURN.
END.

IF in_op_date EQ ? THEN
   in_op_date = gend-date.

/* поиск даты предыдущего операционного дня 
FIND LAST op-date WHERE op-date.op-date < in_op_date NO-ERROR.

IF AVAIL op-date THEN end-date = op-date.op-date.
                 ELSE end-date = in_op_date.

{getdate.i
    &DateLabel = "Предыд.урегулир."
    &DateHelp  = "Дата предыдущего урегулирования резерва (F1 - календарь)"
    &NoInit    = "YES" 
}*/

vkol-bal-tot = 0.
FOR EACH all_recids NO-LOCK:
vkol-bal-tot = vkol-bal-tot + 1.
END.
   {bar-beg2.i
        &BarTotal     = vkol-bal-tot
        &BarMessage   = """отбор договоров для начисления и погашения"""
   }

vkol-bal = 0.
FOR EACH all_recids, loan WHERE RECID(loan) = all_recids.rid NO-LOCK:

      mFlDel = TRUE.

      vkol-bal = vkol-bal + 1.
      /* рисуем статус-бар */
      {bar2.i 
          &BarPointer = vkol-bal
      }

      /* плановое погашение основного долга *
      FIND FIRST term-obl           OF loan 
        WHERE term-obl.idnt      EQ 3 
          AND term-obl.end-date  EQ in_op_date
        NO-LOCK NO-ERROR.
      * плановое погашение процентов *
      FIND FIRST xterm-obl           OF loan 
        WHERE xterm-obl.idnt      EQ 1 
          AND xterm-obl.end-date  EQ in_op_date
        NO-LOCK NO-ERROR.*/

      IF loan-ost(loan.contract,loan.cont-code,in_op_date,"кредрасч") > 0 THEN DO:
        /* остаток есть, надо смотреть есть ли просрочка *
	IF loan-ost(loan.contract,loan.cont-code,in_op_date,"кредпр") > 0
	 THEN mFlDel = FALSE.
	IF loan-ost(loan.contract,loan.cont-code,in_op_date,"кредпр%") > 0
	 THEN mFlDel = FALSE.
	IF loan-ost(loan.contract,loan.cont-code,in_op_date,"кредпр%в") > 0
	 THEN mFlDel = FALSE.*/
	IF fu-par(BUFFER loan,in_op_date,7) > 0
	 THEN mFlDel = FALSE.
	IF fu-par(BUFFER loan,in_op_date,10) > 0
	 THEN mFlDel = FALSE.
	IF fu-par(BUFFER loan,in_op_date,210) > 0
	 THEN mFlDel = FALSE.
	IF fu-par(BUFFER loan,in_op_date,48) > 0
	 THEN mFlDel = FALSE.
	IF fu-par(BUFFER loan,in_op_date,248) > 0
	 THEN mFlDel = FALSE.
	IF fu-par(BUFFER loan,in_op_date,229) > 0
	 THEN mFlDel = FALSE.
	IF fu-par(BUFFER loan,in_op_date,233) > 0
	 THEN mFlDel = FALSE.
	IF fu-par(BUFFER loan,in_op_date,9) +
	   fu-par(BUFFER loan,in_op_date,12) +
	   fu-par(BUFFER loan,in_op_date,15) +
	   fu-par(BUFFER loan,in_op_date,26) +
	   fu-par(BUFFER loan,in_op_date,509) +
	   fu-par(BUFFER loan,in_op_date,519)
	    > 0
	 THEN mFlDel = FALSE.
	 ELSE
	/* пени состоянием не посчитать
	 ставка		база (взято из базанач и базастав ln-proc.i)
	 пеня-к		7
	 пеня%к		10+210+48+248
	 если с момента расчета состояния до нашей даты было движение 
	 по базовым параметрам, значит считаем что пени появятся.
	 */ 
	FOR EACH loan-int WHERE
	    loan-int.contract = loan.contract AND
	    loan-int.cont-code = loan.cont-code AND
	    CAN-DO("7,10,48,210,248", string(loan-int.id-d)) AND
	    loan-int.mdate     > loan.since    AND
	    loan-int.mdate     <= in_op_date
	    NO-LOCK:
	    mFlDel = FALSE.
	    LEAVE.
	END.
	
	/* комиссии платятся только в первый платеж 377, 209, 229? */
	FIND FIRST term-obl of loan
	 WHERE (term-obl.idnt = 1 OR term-obl.idnt = 3)
	   AND term-obl.dsc-beg-date <= in_op_date NO-LOCK NO-ERROR.
	IF AVAIL term-obl THEN DO:
		IF fu-par(BUFFER loan,in_op_date,209) > 0 OR
		   fu-par(BUFFER loan,in_op_date,301) > 0 OR
		   fu-par(BUFFER loan,in_op_date,377) > 0
		 THEN mFlDel = FALSE.
	END.
/*
IF NOT mFlDel THEN DO:
message "отобран " + string(loan.cont-code) + " договор"  view-as alert-box.
message 
string(loan-ost(loan.contract,loan.cont-code,in_op_date,"кредрасч")) + " " +
string(loan-ost(loan.contract,loan.cont-code,in_op_date,"кредпр")) + " " +
string(loan-ost(loan.contract,loan.cont-code,in_op_date,"кредпр%")) + " " +
string(loan-ost(loan.contract,loan.cont-code,in_op_date,"кредпр%в")) + " " +
string(loan-ost(loan.contract,loan.cont-code,in_op_date,"кредт")) + " " +
string(loan-ost(loan.contract,loan.cont-code,in_op_date,"кредтв"))
 view-as alert-box.
message 
 fu-par(BUFFER loan,in_op_date,10)
 fu-par(BUFFER loan,in_op_date,210)
 fu-par(BUFFER loan,in_op_date,48)
 fu-par(BUFFER loan,in_op_date,248)
 fu-par(BUFFER loan,in_op_date,233)
 view-as alert-box.
END.*/
      END.
/*fu-par(BUFFER loan,in_op_date,9).
fu-par(BUFFER loan,in_op_date,12).*/
    /* оплата од */
    run _ПланДата in h_pint ( loan.contract,
                              loan.cont-code,
                              in_op_date,
                              TRUE,
                              3,
                              output vR).
    IF vR THEN mFlDel = FALSE.
    /* оплата % */
    run _ПланДата in h_pint ( loan.contract,
                              loan.cont-code,
                              in_op_date,
                              TRUE,
                              1,
                              output vR).
    IF vR THEN mFlDel = FALSE.
    /* начисление % */
    run _ПланДата in h_pint ( loan.contract,
                              loan.cont-code,
                              in_op_date,
                              FALSE,
                              1,
                              output vR).
    IF vR THEN mFlDel = FALSE.

/*   IF NUM-ENTRIES(loan.cont-code," ") EQ 1 THEN
   DO:
      mFlDel = FALSE.
      FOR EACH bloan WHERE bloan.contract  EQ loan.contract
                       AND bloan.cont-code BEGINS loan.cont-code + " "
                       AND NUM-ENTRIES(bloan.cont-code, " ") EQ 2:
         IF RegulationNeed(bloan.contract,bloan.cont-code,end-date,in_op_date) THEN
         DO:
            mFlDel = TRUE.
            LEAVE.
         END.
      END.
   END.
   ELSE
      mFlDel = FALSE.
   IF     NOT RegulationNeed(loan.contract,loan.cont-code,end-date,in_op_date)
      AND NOT mFlDel THEN
   DO:
      DELETE all_recids.
      NEXT.
   END.*/

/* kam */
   IF checkov(BUFFER loan) THEN DO:
	mFlDel = TRUE.
/*	message loan.cont-code view-as alert-box. */
   END.	

   IF mFlDel THEN
   DO:
      DELETE all_recids.
      NEXT.
   END.

END. /*FOR EACH*/
vkol-bal-tot = 0.
FOR EACH all_recids NO-LOCK:
vkol-bal-tot = vkol-bal-tot + 1.
END.
/* message "отобрано " + string(vkol-bal-tot) + " договоров"  view-as alert-box. */

   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES     FILL (" ",79).
   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 FILL (" ",79).
   PAUSE 0.  

{intrface.del}
