{globals.i}
{titul.def}
{flt-val.i}
{intrface.get xclass}
{intrface.get print}
{initstrp.i}

DEF VAR AskDocCount AS LOG   INIT NO  NO-UNDO.

DEF VAR UsrId  	    AS CHAR		      NO-UNDO.
DEF VAR InsType     AS CHAR 		  NO-UNDO.
DEF VAR Tmpl	    AS CHAR	 	      NO-UNDO.
DEF VAR Counter     AS INT   INIT 0   NO-UNDO.

DEF VAR ipCity      AS CHAR  INIT "4" NO-UNDO.

DEF VAR set-target  AS INT   INIT 2   NO-UNDO.
DEF VAR doc-count   AS INT   INIT 1   NO-UNDO.

/* переменные, необходимые для op-print */
DEF VAR idet         AS INT64         NO-UNDO.
DEF VAR mProfile     AS CHAR          NO-UNDO.
DEF VAR mItem        AS INT64         NO-UNDO.
DEF VAR mStrTMP      AS CHAR          NO-UNDO.
DEF VAR vLine        AS CHAR          NO-UNDO.
DEF VAR mIsMacro     AS LOG           NO-UNDO.
DEF VAR mMaxWidthTxt AS INT64         NO-UNDO.

DEF VAR detail      AS CHAR EXTENT 4 FORMAT "x(45)" NO-UNDO.

DEF STREAM sSpoolTXT.
DEF STREAM macro-file.

DEF TEMP-TABLE tt-dprint NO-UNDO
    FIELD contract  AS CHAR
	FIELD cont-code AS CHAR
	FIELD op-kind   AS CHAR
	FIELD num 		AS INT.
	
DEFINE TEMP-TABLE tt-doctypes
   FIELD code AS CHARACTER
   FIELD name AS CHARACTER
   FIELD PROC AS CHARACTER
   FIELD quan AS INT64
INDEX code code.

{wordwrap.def}
	
/* формирование ведомости проводок */
{vedprov_t.i}
{vedprov.i}

/**/
RUN SetSysConf IN h_base("printvd_file","").
RUN SetSysConf IN h_base ("stamp_plat_doc", "*").

PAUSE(0).

{get_set2.i "Принтер" "PCL" "w/o chek"}
RUN SetPrintSysconf IN h_base ("print", ?).      /* обнуление глобальных параметров печати      */
OUTPUT STREAM macro-file TO "_macro.tmp" .

ASSIGN
   FirstPrint   = NO
   PackagePrint = YES
   AskDocCount  = NO
   ToSplit      = ?
.

OS-DELETE VALUE("_spool.tmp").
/**/

FOR EACH tt-op-day
	WHERE tt-op-day.user-id EQ zUsId:

	/* проверяем на необходимость печати*/
	IF LOOKUP(tt-op-day.op-kind,"ВыдОпл2,ВыдСК2") > 0 AND tt-op-day.acct-db BEGINS '40817' THEN
		RUN FillDprint(tt-op-day.op-kind,tt-op-day.acct-db).

	IF tt-op-day.op-kind = 'ВыдАвто+' AND tt-op-day.acct-cr BEGINS '40817' THEN
		RUN FillDprint(tt-op-day.op-kind,tt-op-day.acct-db).		
	
	FIND FIRST op 
		 WHERE RECID(op)   EQ tt-op-day.op-cid 
		 AND   op.doc-type EQ "01"
		NO-LOCK NO-ERROR.
	
	/* распечатываем платежные поручения */
	IF AVAIL(op) THEN DO:
	
		OUTPUT STREAM macro-file CLOSE.
		{op-print.i}

    END.

END.

/**/
PackagePrint = FALSE.
FirstPrint   = YES.

IF set-target NE 1 THEN
DO:
   {setdest.i  &append=" APPEND "}
END.
RUN SetSysConf IN h_base("DocCount",STRING(doc-count)).
{preview.i}
RUN SetSysConf IN h_base("printvd_file","").
RUN SetSysConf IN h_base("printvd_options","").
RUN SetSysConf IN h_base("DocCount","").
RUN SetSysConf IN h_base ("stamp_plat_doc", "").
{intrface.del}
/**/

/* распоряжение на выдачу кредита */
FOR EACH  tt-dprint
	WHERE tt-dprint.op-kind EQ 'ВыдАвто+'
NO-LOCK BY cont-code:

	FIND FIRST loan-int
		 WHERE loan-int.id-d      EQ 0
		 AND   loan-int.contract  EQ tt-dprint.contract
		 AND   loan-int.cont-code EQ tt-dprint.cont-code
		 AND   loan-int.op-date   LE end-date
		NO-LOCK NO-ERROR.
	
	IF AVAIL loan-int THEN 
		RUN precrdprint.p("raspvydap|" + tt-dprint.contract + ',' + tt-dprint.cont-code).

END.

FOR EACH  tt-dprint
	WHERE tt-dprint.op-kind NE 'ВыдАвто+'
NO-LOCK BY cont-code:

	FIND FIRST loan-int
		 WHERE loan-int.id-d      EQ 0
		 AND   loan-int.contract  EQ tt-dprint.contract
		 AND   loan-int.cont-code EQ tt-dprint.cont-code
		 AND   loan-int.op-date   LE end-date
		NO-LOCK NO-ERROR.
	
	IF AVAIL loan-int THEN DO:
	
		/* распоряжение на оплату автомобиля */
		IF tt-dprint.op-kind = 'ВыдОпл2' THEN
			RUN precrdprint.p("raspperapp|" + tt-dprint.contract + ',' + tt-dprint.cont-code).

		/* распоряжение на оплату страховки */
		IF tt-dprint.op-kind = 'ВыдСК2' THEN DO:
			
			Counter = tt-dprint.num.
			
			FOR EACH  loan 
				WHERE loan.contract 		EQ "СТРАХ" 
				AND   loan.parent-cont-code EQ tt-dprint.cont-code 
				AND   loan.open-date LE end-date
			NO-LOCK BY loan.open-date DESCENDING:
				
				IF Counter > 0 THEN DO:

					InsType = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"VidStr","").
					IF InsType BEGINS "КАСКО" THEN
						Tmpl = "raspperapk|".
					ELSE
						Tmpl = "raspperaps|".
					RUN precrdprint.p(Tmpl + tt-dprint.contract + ',' + tt-dprint.cont-code).

					Counter = Counter - 1.
					
				END.
				ELSE
					LEAVE.
				
			END.

		END.
	
	END.
	
END.

/* заполняем таблицу tt-dprint*/
PROCEDURE FillDprint:

DEF INPUT PARAM iOp-kind AS CHAR NO-UNDO.
DEF INPUT PARAM iAcct    AS CHAR NO-UNDO.

DEF VAR 		lParam   AS CHAR NO-UNDO.

RUN FindCred(iAcct, OUTPUT lParam).
	
FIND FIRST tt-dprint
	 WHERE tt-dprint.contract  EQ ENTRY(1,lParam)
	 AND   tt-dprint.cont-code EQ ENTRY(2,lParam)
	 AND   tt-dprint.op-kind   EQ tt-op-day.op-kind
	NO-ERROR.

	IF NOT AVAIL tt-dprint THEN DO:
	
		CREATE tt-dprint.
		ASSIGN
			tt-dprint.contract  = ENTRY(1,lParam)
			tt-dprint.cont-code = ENTRY(2,lParam)
			tt-dprint.op-kind   = tt-op-day.op-kind
			tt-dprint.num       = 1
		.
		RELEASE tt-dprint.
		
	END.
	ELSE
		tt-dprint.num = tt-dprint.num + 1.

END PROCEDURE.

/* находим кредит по счету */
PROCEDURE FindCred:

DEF INPUT PARAM  iAcct AS CHAR NO-UNDO.
DEF OUTPUT PARAM oSrg  AS CHAR NO-UNDO.

FIND FIRST loan-acct WHERE
			loan-acct.acct BEGINS iAcct
		NO-LOCK NO-ERROR.
		
	IF AVAIL loan-acct THEN 
		oSrg = loan-acct.contract + ',' + loan-acct.cont-code.

END PROCEDURE.

{op-print.pro &initstrnoyes=YES}