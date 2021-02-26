/* ведомость по инкассации */
/* kam */



{globals.i}
{pp-corr.p}
{sh-defs.i}
{ksh-defs.i NEW}
{tmprecid.def}
{intrface.get xclass}

DEFINE INPUT PARAMETER iBank AS CHARACTER NO-UNDO.

FUNCTION getfilial returns char
    (input region_cl AS CHAR).
    
    DEF VAR filialSouz AS CHAR NO-UNDO.
    filialSouz = ''.
    
    if region_cl matches '*Самар*' then filialSouz = 'Самара'.
    if region_cl matches '*Татар*' then filialSouz = 'Самара'.
    if region_cl matches '*Казан*' then filialSouz = 'Самара'.
    if region_cl matches '*Удмур*' then filialSouz = 'Самара'.

    if region_cl matches '*Нижни*' then filialSouz = 'Нижний Новгород'.    
    if region_cl matches '*Нижег*' then filialSouz = 'Нижний Новгород'.    
    if region_cl matches '*Нижне*' then filialSouz = 'Нижний Новгород'.    
    
    if region_cl matches '*Свердло*' then filialSouz = 'Екатеринбург'.
    if region_cl matches '*Екатери*' then filialSouz = 'Екатеринбург'.
    if region_cl matches '*Тюмен*' then filialSouz = 'Екатеринбург'.

    if region_cl matches '*Иркут*' then filialSouz = 'Иркутск'.

    if region_cl matches '*Краснод*' then filialSouz = 'Краснодар'.
    if region_cl matches '*Ростов*' then filialSouz = 'Краснодар'.
    if region_cl matches '*Волгогр*' then filialSouz = 'Краснодар'.

    if region_cl matches '*Красноя*' then filialSouz = 'Красноярск'.
    if region_cl matches '*Томск*' then filialSouz = 'Красноярск'.
    
    if region_cl matches '*Санкт*' then filialSouz = 'Санкт-Петербург'.
    if region_cl matches '*С-П*' then filialSouz = 'Санкт-Петербург'.
    if region_cl matches '*Ленингр*' then filialSouz = 'Санкт-Петербург'.
    
    if region_cl matches '*Москв*' then filialSouz = 'Москва'.
    if region_cl matches '*Мос.*' then filialSouz = 'Москва'.
    if region_cl matches '*Москов*' then filialSouz = 'Москва'.
    
    RETURN filialSouz.
END function.    

DEF VAR nameCl as char no-undo.
DEF VAR tmpdate as date no-undo.
DEF VAR tmpDec AS DECIMAL NO-UNDO.

DEFINE VAR fname AS CHAR NO-UNDO.
DEFINE VAR fstr AS CHAR INIT '' NO-UNDO.
DEFINE VAR bFind AS LOGICAL NO-UNDO.

def new shared stream vvs.
  
DEF VAR num          AS int64 NO-UNDO.                /* N пп */
DEF VAR region       AS CHAR NO-UNDO.                 /* Регион выдачи кредита */
DEF VAR region_cl    AS CHAR NO-UNDO.                 /* Регион регистрации Заемщика */
DEF VAR filialSouz   AS CHAR NO-UNDO.                 /* Филиалы Банка СОЮЗ */
DEF VAR name_cl      AS CHAR NO-UNDO.                 /* ФИО заемщика */
DEF VAR DATE_cl      AS DATE NO-UNDO.                 /* Дата рождения Заемщика */
DEF VAR num_cl       AS int64 NO-UNDO.                /* Номер клиента */                
DEF VAR summ_cr      AS DECIMAL NO-UNDO.              /* сумма кредита */
DEF VAR stavka_cr    AS DECIMAL NO-UNDO.              /* Процентная ставка на дату уступки прав (требования) */
DEF VAR date_plat    AS int64 NO-UNDO.                /* Дата платежа по графику */
DEF VAR annuitet     AS DECIMAL NO-UNDO.              /* Размер текущего аннуитетного платежа */
DEF VAR summ_od      AS DECIMAL NO-UNDO.              /* Остаток основного долга на дату перехода прав */ 
DEF VAR summ_proc    AS DECIMAL NO-UNDO.              /* Сумма начисленных процентов на дату перехода прав */         
DEF VAR summ_zad     AS DECIMAL NO-UNDO.              /* Общая сумма задолженности по Кредитному на дату перехода прав (с.15+ с.16) */        
DEF VAR summ_pok     AS DECIMAL NO-UNDO.              /* Стоимость приобретаемых прав (требований) (п.2.1 Договора) */        
DEF VAR summ_zal     AS DECIMAL NO-UNDO.              /* Залоговая стоимость предмета залога на дату выдачи кредита */        
DEF VAR summ_beg_zal AS DECIMAL NO-UNDO.              /* Рыночная стоимость предмета залога на дату выдачи кредита */
DEF VAR model_zal    AS CHAR NO-UNDO.                 /* Марка,модель автотранспортного средства предмета залога */               
DEF VAR year_zal     AS CHAR NO-UNDO.                 /* Год выпуска автотранспортного средства */
DEF VAR vin_zal      AS CHAR NO-UNDO.                 /* VIN */     
DEF VAR pts          AS CHAR NO-UNDO.                 /* ПТС */     
  
DEFINE TEMP-TABLE otchpr
        FIELD num          AS int64                /* N пп */
        FIELD region       AS CHAR                 /* Регион выдачи кредита */
        FIELD region_cl    AS CHAR                 /* Регион регистрации Заемщика */
        FIELD filialSouz   AS CHAR                 /* Филиалы Банка СОЮЗ */
        FIELD name_cl      AS CHAR                 /* ФИО заемщика */
        FIELD date_cl      AS DATE                 /* Дата рождения Заемщика */
        FIELD num_cl       AS int64                /* Номер клиента */                
        FIELD cont_code    AS CHAR                 /* № кредитного договора */                
        FIELD date_cr      AS DATE                 /* Дата выдачи кредита */
        FIELD date_cr_end  AS DATE                 /* Дата истечения срока кредита */
        FIELD summ_cr      AS DECIMAL              /* сумма кредита */
        FIELD stavka_cr    AS DECIMAL              /* Процентная ставка на дату уступки прав (требования) */
        FIELD date_plat    AS int64                /* Дата платежа по графику */
        FIELD annuitet     AS DECIMAL              /* Размер текущего аннуитетного платежа */
        FIELD summ_od      AS DECIMAL              /* Остаток основного долга на дату перехода прав */ 
        FIELD summ_proc    AS DECIMAL              /* Сумма начисленных процентов на дату перехода прав */         
        FIELD summ_zad     AS DECIMAL              /* Общая сумма задолженности по Кредитному на дату перехода прав (с.15+ с.16) */        
        FIELD summ_pok     AS DECIMAL              /* Стоимость приобретаемых прав (требований) (п.2.1 Договора) */        
        FIELD summ_zal     AS DECIMAL              /* Залоговая стоимость предмета залога на дату выдачи кредита */        
        FIELD summ_beg_zal AS DECIMAL              /* Рыночная стоимость предмета залога на дату выдачи кредита */
        FIELD model_zal    AS char                 /* Марка,модель автотранспортного средства предмета залога */               
        FIELD year_zal     AS char                 /* Год выпуска автотранспортного средства */
        FIELD vin_zal      AS char                 /* VIN */       
        FIELD pts          AS char                 /* ПТС */       
    .

{empty otchpr}

    
    fname = "./otchpr"  + "_" + userid('bisquit') + ".csv".


DEF BUFFER loan for loan.
DEF BUFFER loan-cond for loan-cond.
DEF BUFFER loan-acct for loan-acct.
DEF BUFFER term-obl for term-obl.
DEF BUFFER comm-rate for comm-rate.

num = 1. 

end-date = today.
{getdate.i
&NoInit    = "YES"
}

for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:
    
    /* region */
    FIND FIRST code WHERE code.class EQ 'ГородПоКредиту'
        AND code.code EQ SUBSTR(loan.cont-code,1,2) NO-LOCK NO-ERROR.
    IF AVAILABLE code THEN region = code.name.
        ELSE region = ''.
    
    region_cl = ''.
    name_cl = ''.
    num_cl = 0.
    FIND first person where person.person-id EQ loan.cust-id no-lock no-error.
    IF AVAIL person then do:
        /* region_cl */
        region_cl  = Entry(3,person.address[1],",").
        if region_cl = '' then region_cl = Entry(2,person.address[1],",").
        find last signs where signs.file-name = 'person'
            and signs.code = 'кодреггни'
            and signs.surrogate = string(person.person-id) no-lock no-error.
        if avail signs then do:    
            find first code where code.class = 'кодреггни' 
                and code.code = signs.xattr-value no-lock no-error.
            if avail code then region_cl = code.name.
        end.
        /* name_cl */
        name_cl = trim(person.name-last) + ' ' + trim(person.first-names).
        /* date_cl */
        date_cl = person.birthday.
        /* num_cl */
        num_cl = person.person-id.
    END.
    /* filialSouz */
    filialSouz = getfilial(region_cl).
    IF filialSouz = '' then filialSouz = getfilial(region).
    
    /* summ_cr */
    FIND FIRST loan-cond
        WHERE loan-cond.contract EQ loan.contract
        AND loan-cond.cont-code EQ loan.cont-code
    NO-LOCK NO-ERROR.
    IF AVAIL loan-cond then do:
    FIND FIRST term-obl
         WHERE term-obl.contract EQ loan.contract
         AND term-obl.cont-code EQ loan.cont-code
         AND term-obl.idnt = 2
         AND term-obl.fop-date EQ loan-cond.since
         AND term-obl.fop-date EQ term-obl.end-date
    NO-LOCK NO-ERROR.
    IF AVAIL term-obl THEN DO:
        summ_cr = term-obl.amt-rub.
    END.
    end.
    FOR EACH comm-rate
        WHERE comm-rate.kau EQ loan.contract + "," + loan.cont-code
        AND comm-rate.commission = '%Кред'
        AND comm-rate.since <= end-date
        NO-LOCK BY comm-rate.since desc:
        stavka_cr = comm-rate.rate-comm.
        LEAVE.
    END.
    
    annuitet = 0.
    FIND LAST loan-cond WHERE loan-cond.contract = loan.contract
		AND loan-cond.cont-code = loan.cont-code
		AND loan-cond.since <= end-date NO-LOCK NO-ERROR.
		IF AVAIL loan-cond THEN DO:
			FIND FIRST signs WHERE signs.file-name = 'loan-cond'
				AND signs.code = 'аннуитплат'
				AND signs.surrogate = loan.contract + ',' + loan.cont-code + ',' + STRING(DAY(loan-cond.since),"99") + '/' + STRING(MONTH(loan-cond.since),"99") + '/' + SUBSTRING(STRING(YEAR(loan-cond.since),"9999"),3) 
				NO-LOCK NO-ERROR.
				IF AVAIL signs THEN annuitet = signs.dec-value.
           	date_plat = loan-cond.int-date.		
		END.

	summ_od = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'Кредит'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_od = abs(sh-bal).
		ELSE summ_od = abs(sh-val).
	END.

    summ_proc = 0.
    FIND LAST loan-acct OF loan WHERE
        loan-acct.since <= end-date
        AND loan-acct.acct-type = 'КредТ'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
        IF loan-acct.currency = '' THEN
                summ_proc = abs(sh-bal).
        ELSE summ_proc = abs(sh-val).
    END.

	summ_zad = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредПр'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = abs(sh-bal).
		ELSE summ_zad = abs(sh-val).
	END.

	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредПр%'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.

	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредПр%В'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.
    
    FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредТ'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.    
	
    FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредТВ'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.
		
    FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредТВ1'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.    
	
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредТПр'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.
	
	summ_zad = summ_od + summ_proc + summ_zad.
	summ_pok = 0 .  /* summ_od + summ_proc + summ_zad. */
	
	summ_zal = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредОб'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zal = abs(sh-bal).
		ELSE summ_zal = abs(sh-val).
	END.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = 'КредОб1'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zal = summ_zal + abs(sh-bal).
		ELSE summ_zal = summ_zal + abs(sh-val).
	END.	
        ASSIGN
	model_zal = ''
	year_zal = ''
	vin_zal = ''
        pts = ''.
	FOR EACH term-obl WHERE
		term-obl.cont-code EQ loan.cont-code
		AND term-obl.contract EQ loan.contract
		AND term-obl.idnt EQ 5
		AND (term-obl.end-date = ? OR term-obl.end-date >= TODAY) 
		NO-LOCK BY term-obl.fop-date:
	    model_zal = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCmodel",
		"").
		model_zal = model_zal + ' ' + GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCbrand",
		"").
		year_zal = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCyear",
		"").		
		vin_zal = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCVIN",
		"").
	    pts = GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCSER",
		"").
	    pts = pts + ' ' + GetXAttrValueEx("term-obl",
		loan.contract + "," + loan.cont-code + ',5,' + STRING(term-obl.end-date,"99/99/99") + ',' + STRING(term-obl.nn),
		"TCNUMB",
		"").


		
		
    END.
        
    CREATE otchpr.
    ASSIGN
        otchpr.num = num
        otchpr.region = region
        otchpr.region_cl = region_cl
        otchpr.filialSouz = filialSouz
        otchpr.name_cl = name_cl  
        otchpr.date_cl = date_cl
        otchpr.num_cl = num_cl
        otchpr.cont_code = loan.doc-ref
        otchpr.date_cr = loan.open-date
        otchpr.date_cr_end = loan.end-date
        otchpr.summ_cr = summ_cr
        otchpr.stavka_cr = stavka_cr
        otchpr.date_plat = date_plat
        otchpr.annuitet = annuitet
        otchpr.summ_od = summ_od
        otchpr.summ_proc = summ_proc
        otchpr.summ_zad = summ_zad
        otchpr.summ_pok = summ_pok
        otchpr.summ_zal = summ_zal
        otchpr.summ_beg_zal = 0
        otchpr.model_zal = model_zal
        otchpr.year_zal = year_zal
        otchpr.vin_zal = vin_zal
        otchpr.pts = pts
        .
    num = num + 1.    
end.        
  
/*
run instview.p(TEMP-TABLE otchpr:HANDLE).				 
  */


    output stream vvs to value (fname)
        UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".


put stream vvs unformatted 
'№ п/п;Регион выдачи кредита;Регион регистрации Заемщика;Филиалы Банка СОЮЗ;ФИО заемщика;Дата рождения Заемщика;Номер клиента;№ кредитного договора;Дата выдачи кредита;Дата истечения срока кредита;Сумма кредита;Процентная ставка на дату уступки прав (требования);Дата платежа по графику;Размер текущего аннуитетного платежа;Остаток основного долга на дату перехода прав ;Сумма начисленных процентов на дату перехода прав;Общая сумма задолженности по Кредитному на дату перехода прав (с.15+ с.16);Стоимость приобретаемых прав (требований) (п.2.1 Договора);Залоговая стоимость предмета залога на дату выдачи кредита;Рыночная стоимость предмета залога на дату выдачи кредита;Марка, модель автотранспортного средства - предмета залога;Год выпуска автотранспортного средства;VIN;ПТС' skip
.

for each otchpr no-lock:
    put stream vvs unformatted
    string(otchpr.num) 
    ';' string(otchpr.region) 
    ';' string(otchpr.region_cl)
    ';' string(otchpr.filialSouz)
    ';' string(otchpr.name_cl)  
    ';' string(otchpr.date_cl)
    ';' string(otchpr.num_cl)
    ';' string(otchpr.cont_code)
    ';' string(otchpr.date_cr)
    ';' string(otchpr.date_cr_end)
    ';' string(otchpr.summ_cr,"->>>>>>>>>>>9.99")
    ';' string(otchpr.stavka_cr, "->>>>>>>>>>>9.99")
    ';' string(otchpr.date_plat)
    ';' string(otchpr.annuitet, "->>>>>>>>>>>9.99")
    ';' string(otchpr.summ_od, "->>>>>>>>>>>9.99")
    ';' string(otchpr.summ_proc, "->>>>>>>>>>>9.99")
    ';' string(otchpr.summ_zad, "->>>>>>>>>>>9.99")
    ';' string(otchpr.summ_pok, "->>>>>>>>>>>9.99")
    ';' string(otchpr.summ_zal, "->>>>>>>>>>>9.99")
    ';' string(otchpr.summ_beg_zal, "->>>>>>>>>>>9.99")
    ';' string(otchpr.model_zal)
    ';' string(otchpr.year_zal)
    ';' string(otchpr.vin_zal)
    ';' string(otchpr.pts)
    skip.  
    
    
end.

output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").


  

  