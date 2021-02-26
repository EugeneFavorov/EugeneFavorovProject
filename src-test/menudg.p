/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ЗАО "Банковские информационные системы"
     Filename: menubagoper.p
      Comment: Меню операци над портфелфми.
   Parameters:
         Uses:
      Used by:
      Created: 23/01/2011 Om
     Modified: 
*/

{pick-val.i} 
/* {globals.i} */
{tmprecid.def &NGSH =  "LOCAL"}  /* Таблица отметок. */
{sh-defs.i}
{ksh-defs.i NEW}
/* {loan.pro} */ 
{intrface.get xclass}   /* Инструменты для работы с метасхемой.  */
{intrface.get loan}
{intrface.get date}
{intrface.get separate}
{topkind.def} 

{client.i}

{param-dog.p}

/* Временная таблица для отчета */
DEFINE TEMP-TABLE ttReportpdg NO-UNDO
   FIELD since       AS DATE LABEL "Срок оплаты" /* Дата пересчета договора */
   FIELD param_id    AS CHAR /* Идентификатор параметра */
   FIELD param_value AS DEC  FORMAT "->>>>>>>>9.99" LABEL "Сумма" /* Значение параметра */
   FIELD name-par    AS CHAR LABEL "Наименование параметра" /* наименование параметра (параметров). Если параметров много, то выводим наименования параметров через "," */
.

/* Функция, прибавляет заданное количество рабочих дней к указанной дате */
FUNCTION AddWorkDay RETURN DATE (INPUT vDateIn AS DATE,INPUT amtWorkDay AS INT64).
   DEF VAR vDate AS DATE  NO-UNDO.
   DEF VAR i     AS INT64 NO-UNDO.
   
   vDate = vDateIn.
   DO i = 1 TO amtWorkDay:
      vDate = vDate + 1. 
      IF HOLIDAY(vDate) OR CAN-DO("1,7",STRING(WEEKDAY(vDate))) THEN i = i - 1.
/*       MESSAGE i skip STRING(vDate) skip HOLIDAY(vDate) VIEW-AS ALERT-BOX.  */
   END.
   RETURN vDate.
END FUNCTION.


function calcppdg returns decimal:
    
def var ipCountTypeChar as char no-undo.
ipCountTypeChar = "0+2+7+8+233+9+10+12+18+26+82+210+16+13+14+15+48+248+29+229+519+509+530+373+777+4".


DEFINE VARIABLE vCountInt   as INT64    INIT 0 no-undo. /* Счетчик */
DEFINE VARIABLE vCustName   AS CHAR     NO-UNDO.   /* Наименование клиента */
DEFINE VARIABLE out_Result  AS DECIMAL  NO-UNDO.
DEFINE VARIABLE vDbOpDec    AS DECIMAL  NO-UNDO.
DEFINE VARIABLE vCrOpDec    AS DECIMAL  NO-UNDO.
DEFINE VARIABLE mSum-prosr  AS DECIMAL  label "" init 0  NO-UNDO.
DEFINE VARIABLE mSum-all    AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mSum-annu   AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mdate       AS DATE     NO-UNDO.
DEFINE VARIABLE mRs-acct    AS CHAR     NO-UNDO.   /* Расчетный счет */
DEFINE VARIABLE mVkl-acct   AS CHAR     NO-UNDO.   /* Расчетный счет */
DEFINE VARIABLE e-date      AS DATE     LABEL "" NO-UNDO.
DEFINE VARIABLE mRs-ost     AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE mVkl-ost    AS DECIMAL  label "" INIT 0  NO-UNDO.
DEFINE VARIABLE i           AS INTEGER  NO-UNDO.
DEFINE VARIABLE iPar        AS CHAR     NO-UNDO.
DEFINE VARIABLE mSumFullComm AS DECIMAL INIT 0  NO-UNDO.
DEFINE VARIABLE mSumComm    AS DECIMAL  INIT 0  NO-UNDO.
DEFINE VARIABLE firstDate   AS DATE     NO-UNDO.
DEFINE VARIABLE not0        AS LOGICAL  NO-UNDO.

DEFINE BUFFER   term-obl    FOR term-obl.
DEFINE BUFFER   bterm-obl   FOR term-obl.
DEFINE BUFFER   bLA         FOR loan-acct.

not0 = TRUE.
{empty ttReportpdg}
    /* Бегущая строка - индикатор работы процесса */
/*      {move-bar.i vLnCountInt vLnTotalInt}
*/
    mdate = loan.since.
    mSum-all = 0.
    mSumComm = 0.
    mSumFullComm = 0.
    mSum-prosr = 0.
    out_result = 0.
    mSum-annu = 0.
    DO vCountInt = 1 TO NUM-ENTRIES (ipCountTypeChar,"+"):

        /* Получение значения параметра */
        iPar = ENTRY(vCountInt, ipCountTypeChar, "+").
        RUN PRM(loan.Contract,          /* Назначение договора */
                loan.Cont-Code,         /* Номер договора */
                INTEGER(iPar),          /* Код параметра  */
                loan.since,             /* Значение параметра на дату пересчета договора */
                TRUE,                   /* считать % */
                OUTPUT out_result).     /* Значение параметра без loan.interest[i] */


        CREATE ttReportpdg.
        ASSIGN
            ttReportpdg.since       = loan.since
            ttReportpdg.param_id    = iPar
            ttReportpdg.param_value = out_result
            .

        /* Получим наименование параметра по справочнику */
        FIND FIRST loan-par
            WHERE loan-par.amt-id EQ INTEGER(iPar)
            NO-LOCK NO-ERROR.
        IF AVAIL loan-par
        THEN ttReportpdg.name-par = ttReportpdg.param_id + " - " + loan-par.NAME.

        /* Корректировка 4 параметра */
        IF (iPar EQ "4")
        THEN DO:
            DO i = 32 TO 35:
                RUN PRM(loan.Contract, loan.Cont-Code, i, loan.since, TRUE, OUTPUT out_result).
                ttReportpdg.param_value = ttReportpdg.param_value + out_result.
            END.
        END.

        /* Корректировка 29 параметра */
        IF (iPar EQ "29")
        THEN DO:
            FOR EACH loan-int OF loan
                WHERE (loan-int.mdate   EQ loan.since)
                NO-LOCK,
            FIRST chowhe
                WHERE (chowhe.id-d      EQ loan-int.id-d)
                  AND (chowhe.id-k      EQ loan-int.id-k)
                  AND (chowhe.id-op     EQ 83)
                NO-LOCK:

                ttReportpdg.param_value = ttReportpdg.param_value - loan-int.amt-rub.
            END.
        END.
        
        /* Корректировка 229 параметра */
        IF (iPar EQ "229")
        THEN DO:
            FOR EACH loan-int OF loan
                WHERE (loan-int.mdate   EQ loan.since)
                NO-LOCK,
            FIRST chowhe
                WHERE (chowhe.id-d      EQ loan-int.id-d)
                  AND (chowhe.id-k      EQ loan-int.id-k)
                  AND (chowhe.id-op     EQ 283)
                NO-LOCK:

                ttReportpdg.param_value = ttReportpdg.param_value - loan-int.amt-rub.
            END.
        END.
    END. /* DO vCountInt = 1 TO ... */

    mSumComm = 0.
    FIND FIRST term-obl
        WHERE term-obl.contract  EQ loan.contract
          AND term-obl.cont-code EQ loan.cont-code
          AND term-obl.idnt      EQ 10
          NO-LOCK NO-ERROR.
    IF AVAIL term-obl
    THEN DO:
        mSumFullComm = term-obl.amt-rub.

        RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract, loan.cont-code, 3, loan.open-date, BUFFER term-obl).
        RUN RE_FIRST_TERM_OBL IN h_loan (loan.contract, loan.cont-code, 1, loan.open-date, BUFFER bterm-obl).

        IF AVAIL term-obl AND
            term-obl.dsc-beg-date <=
            (IF AVAIL bterm-obl THEN bterm-obl.dsc-beg-date ELSE term-obl.dsc-beg-date)
        THEN firstDate   = term-obl.dsc-beg-date.

        IF AVAIL bterm-obl AND
            bterm-obl.dsc-beg-date <=
                (IF AVAIL term-obl THEN term-obl.dsc-beg-date ELSE bterm-obl.dsc-beg-date)
        THEN firstDate   = bterm-obl.dsc-beg-date.

        IF firstDate > loan.since
        THEN DO:
            mSumComm = (mSumFullComm / (firstDate - loan.open-date)) * (loan.since - loan.open-date).
            FOR EACH loan-int OF loan
                WHERE loan-int.id-k = 377
                  AND loan-int.mdate <= loan.since
                  NO-LOCK:
                
                mSumComm = mSumComm - loan-int.amt-rub.
            END.
            not0 = FALSE.
        END.
    END.

    FIND FIRST loan-acct OF loan
        WHERE loan-acct.acct-type = 'КредБудКом'
        NO-LOCK NO-ERROR.
    IF AVAIL loan-acct
    THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, mdate, mdate, ?).
        IF not0 OR mSumComm > ABSOLUTE(sh-bal)
        THEN mSumComm = ABSOLUTE(sh-bal).

        CREATE ttReportpdg.
        ASSIGN
            ttReportpdg.since        = loan.since
            ttReportpdg.param_id     = "777"
            ttReportpdg.param_value  = mSumComm
          /*ttReportpdg.name-par   = " Комиссия со счета " + ENTRY(1, acct.acct, "@") + " - "*/
                                  /* 1234567890123456789012345678901234567890 */
            ttReportpdg.name-par     = " Платеж процентов за первый процентный период"
            .
   END.

    RUN RE_L_ACCT(loan.Contract,loan.Cont-Code,"КредРасч",loan.since,BUFFER bLA).
    IF AVAILABLE bLA
    THEN DO:
        mRs-acct = "   Текущий счет " + ENTRY(1, bLA.acct, "@").
        RUN acct-pos IN h_base (bLA.acct, loan.currency, mdate, mdate, ?).
        mRs-ost = ABSOLUTE(sh-bal).
    END.
    ELSE mRs-acct = "   Расчетный счет НЕ ПРИВЯЗАН К ДОГОВОРУ".

    RUN RE_L_ACCT(loan.Contract,loan.Cont-Code,"КредРасч1",loan.since,BUFFER bLA).
    IF AVAILABLE bLA
    THEN DO:
        mVkl-acct = "Обязательства заемщика вклад " + ENTRY(1, bLA.acct, "@").
        RUN acct-pos IN h_base (bLA.acct, loan.currency, mdate, mdate, ?).
        mVkl-ost = ABSOLUTE(sh-bal).
    END.
    ELSE mVkl-acct = "   Вклад НЕ ПРИВЯЗАН К ДОГОВОРУ".

    FOR EACH ttReportpdg
        SHARE-LOCK
        BY INTEGER(ttReportpdg.param_id): /* 5 */

        IF INTEGER(ttReportpdg.param_id) LT 555
        THEN mSum-prosr = mSum-prosr + ttReportpdg.param_value.

        IF INTEGER(ttReportpdg.param_id) EQ 555
        THEN DO:
            ttReportpdg.param_value = mSum-prosr.
         /*   PUT UNFORMATTED FILL("-",71). */
        END.
        ELSE mSum-all = mSum-all + ttReportpdg.param_value.
      
    END.  /* 5 */

    return mSum-all.
end.



DEF INPUT PARAM TABLE FOR tmprecid BIND.  /* Получаем таблицу по указателю. */

DEFINE VARIABLE mRes     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOK      AS LOGICAL     NO-UNDO.

DEF VAR dateZay AS DATE NO-UNDO.
DEF VAR dateChdg AS DATE NO-UNDO.
DEF VAR typeChdg AS CHAR NO-UNDO INIT "ПДГ". 
DEF VAR typeChdg2 AS CHAR NO-UNDO INIT "Платеж".
/* DEF VAR typeChdg AS LOGICAL NO-UNDO INIT TRUE. */
DEF VAR summChdg AS DECIMAL NO-UNDO FORMAT ">>>>>>>>>>>>>9.99".
DEF BUFFER bloan-cond FOR loan-cond.
DEF VAR vSurrLoanCond AS CHAR NO-UNDO.
DEF VAR vSurrLoanCond2 AS CHAR NO-UNDO.
DEF VAR osnComm AS DECIMAL NO-UNDO.
DEF VAR vCredOffs AS CHAR INIT "->" NO-UNDO.
DEF VAR vIntOffs AS CHAR INIT "->" NO-UNDO.
DEF VAR vAmt AS DECIMAL NO-UNDO.
DEF VAR vAmt2 AS DECIMAL NO-UNDO.
DEF VAR vDbSumDec AS DECIMAL NO-UNDO.
DEF VAR vCrSumDec AS DECIMAL NO-UNDO.
DEF VAR in-op-date AS DATE NO-UNDO.
DEF VAR in-cont-code AS CHAR NO-UNDO.
DEF VAR in-acct-db AS CHAR NO-UNDO.
DEF VAR in-acct-cr AS CHAR NO-UNDO.
DEF VAR in-nn AS DECIMAL NO-UNDO.
def var summPDG as decimal no-undo  FORMAT ">>>>>>>>>>>>>9.99".
def var dateper as date no-undo.
def var perechisl as logical no-undo INIT FALSE.
def var tmpstr as char no-undo.
def var typechdg2Vis As logical no-undo.
def var tmpdate as date no-undo.

BLCK:
DO
ON ERROR    UNDO BLCK, LEAVE BLCK
ON ENDKEY   UNDO BLCK, LEAVE BLCK:

FOR EACH tmprecid NO-LOCK, FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

dateZay = today.
dateChdg = today.
dateper = today.

FIND LAST bloan-cond WHERE bloan-cond.contract = loan.contract 
	and bloan-cond.cont-code = loan.cont-code
	and bloan-cond.class-code = 'cd-cond'	
	no-lock no-error.

if avail bloan-cond then dateChdg = AddWorkDay(today,1).

FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
IF AVAIL loan-acct THEN DO:
	in-acct-db = loan-acct.acct.
	RUN acct-pos IN h_base (loan-acct.acct, '', dateChdg, dateChdg, ?).
	summChdg = abs(sh-bal).

END.
ELSE summChdg = 0.

FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'Кредит' NO-LOCK NO-ERROR.
IF AVAIL loan-acct THEN DO:
	in-acct-cr = loan-acct.acct.
END.

summPDG = calcppdg().

tmpdate = AddWorkDay(today,3).

if summPDG > summChdg then do:
    find first term-obl where
        term-obl.contract = loan.contract
        and term-obl.cont-code = loan.cont-code
        and term-obl.end-date >= tmpdate
        and term-obl.idnt = 1 no-lock no-error.
        if avail term-obl and not avail bloan-cond then dateChdg = term-obl.end-date.
        typeChdg = "ЧДГ".
        typechdg2Vis = YES.
end.
else do:
    typeChdg = "ПДГ".
    typechdg2Vis = NO.
end.

PAUSE 0.

DEFINE FRAME frame_date_codes 
     summPDG LABEL "Сумма для ПДГ"
     dateZay LABEL "Дата подачи заявления"
	 dateChdg LABEL "Дата ДГ"	
	 summChdg LABEL "Сумма гашения"
	 typeChdg LABEL "Тип ДГ"  
	 typeChdg2 LABEL "После ДГ уменьшить" 
	 perechisl LABEL "Перечисление" FORMAT "Да/Нет"
	 WITH 1  COL 1 DOWN
	 
WIDTH 70 CENTERED OVERLAY ROW 10 TITLE "Реквизиты доср.гашения: " + loan.doc-ref.

summPDG:SCREEN-VALUE IN FRAME frame_date_codes = STRING(summPDG).

typeChdg2:VISIBLE in frame frame_date_codes = typechdg2Vis.

  DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE
	dateZay
	dateChdg
	summChdg
	typeChdg
	typeChdg2
	perechisl

   WITH FRAME frame_date_codes
   EDITING:
   READKEY.
   IF LASTKEY EQ KEYCODE("ESC") THEN
	RETURN.
   IF LASTKEY EQ KEYCODE("F1")
		THEN DO:
			CASE FRAME-FIELD:
			WHEN "dateZay" THEN
				DO:
				RUN calend.p.
				IF (LASTKEY EQ 13 OR
				LASTKEY EQ 10) AND
				pick-value NE ?
				THEN FRAME-VALUE = string(date(pick-value), "99/99/9999").
			END.
			WHEN "dateChdg" THEN
				DO:
				RUN calend.p.
				IF (LASTKEY EQ 13 OR
				LASTKEY EQ 10) AND
				pick-value NE ?
				THEN FRAME-VALUE = string(date(pick-value), "99/99/9999").
				FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
					IF AVAIL loan-acct THEN DO:
					RUN acct-pos IN h_base (loan-acct.acct, '', dateChdg, dateChdg, ?).
					summChdg = abs(sh-bal).
/*					summChdg:FRAME-VALUE = abs(sh-bal).  */
			        END.

			END.
			
			WHEN "typeChdg" THEN
                DO:
                    IF FRAME-VALUE = 'ПДГ' THEN do:
                        FRAME-VALUE = 'ЧДГ'.
                        typechdg2Vis = YES.
                        
                    end.
                    ELSE do:
                        FRAME-VALUE = 'ПДГ'.
                        typechdg2Vis = NO.
                    end.
                END.
            /*
           WHEN "typeChdg" THEN
                DO:
                    IF FRAME-VALUE = 'Срок' THEN FRAME-VALUE = 'Аннуитет'.
                    ELSE FRAME-VALUE = 'Срок'.
            END.
            */

            WHEN "typeChdg2" THEN
                DO:
                    IF FRAME-VALUE = 'Платеж' THEN FRAME-VALUE = 'Срок'.
                    ELSE FRAME-VALUE = 'Платеж'.
            END.

            WHEN "perechisl" THEN
                DO:
                    IF FRAME-VALUE = 'Да' THEN DO:
                        FRAME-VALUE = 'Нет'.
                        perechisl = FALSE.
                    END.
                    ELSE DO:
                        FRAME-VALUE = 'Да'.
                        perechisl = TRUE.
                    END.
            END.

            END CASE.
		END.
        ELSE IF (LASTKEY EQ KEYCODE("ENTER") OR LASTKEY EQ 10) AND (FRAME-FIELD = "typeChdg" OR FRAME-FIELD = "typeChdg2" )
        THEN DO:
                RETURN NO-APPLY.
        END.
        
		ELSE APPLY LASTKEY.

ON LEAVE OF dateZay IN FRAME frame_date_codes
DO:

FIND LAST bloan-cond WHERE bloan-cond.contract = loan.contract 
	and bloan-cond.cont-code = loan.cont-code
	and bloan-cond.class-code = 'cd-cond'	
	no-lock no-error.


        tmpdate = date(int(substring(dateZay:SCREEN-VALUE IN FRAME frame_date_codes,4,2)),
            int(substring(dateZay:SCREEN-VALUE IN FRAME frame_date_codes,1,2)),
            2000 + int(substring(dateZay:SCREEN-VALUE IN FRAME frame_date_codes,7,2))).

  if avail bloan-cond then do:
	dateChdg = AddWorkDay(tmpdate,1).
  end.


tmpdate = AddWorkDay(tmpdate,3).

if summPDG > summChdg then do:
    find first term-obl where
        term-obl.contract = loan.contract
        and term-obl.cont-code = loan.cont-code
        and term-obl.end-date >= tmpdate
        and term-obl.idnt = 1 no-lock no-error.
        if avail term-obl and not avail bloan-cond then dateChdg = term-obl.end-date.
end.
        dateChdg:SCREEN-VALUE IN FRAME frame_date_codes = STRING(dateChdg,"99/99/99").

END.

ON LEAVE OF dateChdg IN FRAME frame_date_codes
DO:
		FIND LAST loan-acct OF loan WHERE loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
			IF AVAIL loan-acct THEN DO:
				RUN acct-pos IN h_base (loan-acct.acct, '', (dateChdg:SCREEN-VALUE IN FRAME frame_date_codes), (dateChdg:SCREEN-VALUE IN FRAME frame_date_codes), ?).
				summChdg = abs(sh-bal).
				summChdg:SCREEN-VALUE IN FRAME frame_date_codes = STRING(abs(sh-bal)).
			END.
        dateper = date(int(substring(dateChdg:SCREEN-VALUE IN FRAME frame_date_codes,4,2)),
            int(substring(dateChdg:SCREEN-VALUE IN FRAME frame_date_codes,1,2)),
            2000 + int(substring(dateChdg:SCREEN-VALUE IN FRAME frame_date_codes,7,2))).
        if loan.since <> dateper then do:
            /*
        RUN l-calc2.p ("Кредит",       /* Назначение договора. */
                   loan.cont-code,      /* Номер договора. */
                   dateper,   /* Окончание договора + день для выполнения автом. */
                   FALSE,               /* включать/не включать пересчет течений договора */
                   TRUE).               /* выводить/ не выводить протокол на экран */
        */
        summPDG = calcppdg().
        summPDG:SCREEN-VALUE IN FRAME frame_date_codes = STRING(summPDG).

        end.
        
END.

ON LEAVE OF typeChdg IN FRAME frame_date_codes
DO:
       IF typeChdg:SCREEN-VALUE IN FRAME frame_date_codes <> 'ПДГ' 
       AND typeChdg:SCREEN-VALUE IN FRAME frame_date_codes <> 'ЧДГ' 
       THEN do:
           RUN Fill-SysMes IN h_tmess ("", "", "1", "Нет такого типа \n только ПДГ/ЧДГ").
           RETURN NO-APPLY {&RET-ERROR}.
       end.
       IF LASTKEY EQ KEYCODE("ENTER") THEN RETURN NO-APPLY.  
END.

ON LEAVE OF typeChdg2 IN FRAME frame_date_codes
DO:
       IF typeChdg2:SCREEN-VALUE IN FRAME frame_date_codes <> 'Платеж' 
       AND typeChdg2:SCREEN-VALUE IN FRAME frame_date_codes <> 'Срок' 
       THEN do:
           RUN Fill-SysMes IN h_tmess ("", "", "1", "Нет такого типа \n только Платеж/Срок").
           RETURN NO-APPLY {&RET-ERROR}.
       end.
       IF LASTKEY EQ KEYCODE("ENTER") THEN RETURN NO-APPLY.  
END.
/*
ON LEAVE OF perechisl IN FRAME frame_date_codes
DO:
       IF perechisl:SCREEN-VALUE IN FRAME frame_date_codes <> 'Да' 
       AND perechisl:SCREEN-VALUE IN FRAME frame_date_codes <> 'Нет' 
       THEN do:
           RUN Fill-SysMes IN h_tmess ("", "", "1", "Нет такого типа \n только Да/Нет").
           RETURN NO-APPLY {&RET-ERROR}.
       end.
       IF LASTKEY EQ KEYCODE("ENTER") THEN RETURN NO-APPLY.  
END.
*/
ON LEAVE OF summChdg IN FRAME frame_date_codes
DO:
       IF decimal(summChdg:SCREEN-VALUE IN FRAME frame_date_codes) >=  summPdg
       THEN do:
            typeChdg:SCREEN-VALUE IN FRAME frame_date_codes = 'ПДГ'.
            typeChdg = 'ПДГ'. 
            typechdg2Vis = NO.
       end.
       ELSE do:
            typeChdg:SCREEN-VALUE IN FRAME frame_date_codes = 'ЧДГ'.
            typeChdg = 'ЧДГ'. 
            typechdg2Vis = YES.
       end.    
END.

typeChdg2:VISIBLE in frame frame_date_codes = typechdg2Vis.

   END. /* EDITING: */
  END.  /* do on */	


   IF summChdg <= 0 THEN 
   DO:
      MESSAGE "Сумма досрочного гашения 0" VIEW-AS ALERT-BOX.
/*      RETURN. */		
   END.
   /*
message "UpdateSigns" view-as alert-box.
*/

    find first term-obl where
        term-obl.contract = loan.contract
        and term-obl.cont-code = loan.cont-code
        and term-obl.end-date >= AddWorkDay(dateZay,3)
        and term-obl.amt-rub <> 0
        and term-obl.idnt = 1 no-lock no-error.
/*  if typeChDg <> 'ЧДГ' then do: */
    if avail term-obl then do:
	   if dateChdg <> term-obl.end-date then do:
	        UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_Date_dosr_g", string(dateChdg,"99.99.9999"), ?) NO-ERROR.
	   end.
    end.
    else do:
	   UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_Date_dosr_g", string(dateChdg,"99.99.9999"), ?) NO-ERROR.
    end.	
/*  end. */
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_Date_pod_zay", string(dateZay,"99.99.9999"), ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_summ_dosr_g", string(summChdg), ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_type_dosr_g", string(typeChdg), ?) NO-ERROR.
    IF typeChdg <> 'ПДГ' THEN DO:
        UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_type_dosr_g2", string(typeChdg2), ?) NO-ERROR.
    END.
    IF perechisl THEN tmpstr = "Да". ELSE tmpstr = "Нет".
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_perechisl", string(tmpstr), ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_datetime_in", STRING( NOW, "99.99.9999 HH:MM:SS"), ?) NO-ERROR.
    
LEAVE.
END.
END.

{intrface.del}          /* Выгрузка инструментария. */ 
RETURN.
	


