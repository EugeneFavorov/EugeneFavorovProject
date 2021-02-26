/* ��������� �� ������樨 */
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
    
    if region_cl matches '*�����*' then filialSouz = '�����'.
    if region_cl matches '*����*' then filialSouz = '�����'.
    if region_cl matches '*�����*' then filialSouz = '�����'.
    if region_cl matches '*�����*' then filialSouz = '�����'.

    if region_cl matches '*�����*' then filialSouz = '������ �����த'.    
    if region_cl matches '*�����*' then filialSouz = '������ �����த'.    
    if region_cl matches '*�����*' then filialSouz = '������ �����த'.    
    
    if region_cl matches '*���फ�*' then filialSouz = '����ਭ���'.
    if region_cl matches '*�����*' then filialSouz = '����ਭ���'.
    if region_cl matches '*��*' then filialSouz = '����ਭ���'.

    if region_cl matches '*����*' then filialSouz = '�����'.

    if region_cl matches '*��᭮�*' then filialSouz = '��᭮���'.
    if region_cl matches '*���⮢*' then filialSouz = '��᭮���'.
    if region_cl matches '*�������*' then filialSouz = '��᭮���'.

    if region_cl matches '*��᭮�*' then filialSouz = '��᭮���'.
    if region_cl matches '*����*' then filialSouz = '��᭮���'.
    
    if region_cl matches '*�����*' then filialSouz = '�����-������'.
    if region_cl matches '*�-�*' then filialSouz = '�����-������'.
    if region_cl matches '*�������*' then filialSouz = '�����-������'.
    
    if region_cl matches '*��᪢*' then filialSouz = '��᪢�'.
    if region_cl matches '*���.*' then filialSouz = '��᪢�'.
    if region_cl matches '*��᪮�*' then filialSouz = '��᪢�'.
    
    RETURN filialSouz.
END function.    

DEF VAR nameCl as char no-undo.
DEF VAR tmpdate as date no-undo.
DEF VAR tmpDec AS DECIMAL NO-UNDO.

DEFINE VAR fname AS CHAR NO-UNDO.
DEFINE VAR fstr AS CHAR INIT '' NO-UNDO.
DEFINE VAR bFind AS LOGICAL NO-UNDO.

def new shared stream vvs.
  
DEF VAR num          AS int64 NO-UNDO.                /* N �� */
DEF VAR region       AS CHAR NO-UNDO.                 /* ������ �뤠� �।�� */
DEF VAR region_cl    AS CHAR NO-UNDO.                 /* ������ ॣ����樨 ����騪� */
DEF VAR filialSouz   AS CHAR NO-UNDO.                 /* ������� ����� ���� */
DEF VAR name_cl      AS CHAR NO-UNDO.                 /* ��� ����騪� */
DEF VAR DATE_cl      AS DATE NO-UNDO.                 /* ��� ஦����� ����騪� */
DEF VAR num_cl       AS int64 NO-UNDO.                /* ����� ������ */                
DEF VAR summ_cr      AS DECIMAL NO-UNDO.              /* �㬬� �।�� */
DEF VAR stavka_cr    AS DECIMAL NO-UNDO.              /* ��業⭠� �⠢�� �� ���� ���㯪� �ࠢ (�ॡ������) */
DEF VAR date_plat    AS int64 NO-UNDO.                /* ��� ���⥦� �� ��䨪� */
DEF VAR annuitet     AS DECIMAL NO-UNDO.              /* ������ ⥪�饣� �����⭮�� ���⥦� */
DEF VAR summ_od      AS DECIMAL NO-UNDO.              /* ���⮪ �᭮����� ����� �� ���� ���室� �ࠢ */ 
DEF VAR summ_proc    AS DECIMAL NO-UNDO.              /* �㬬� ���᫥���� ��業⮢ �� ���� ���室� �ࠢ */         
DEF VAR summ_zad     AS DECIMAL NO-UNDO.              /* ���� �㬬� ������������ �� �।�⭮�� �� ���� ���室� �ࠢ (�.15+ �.16) */        
DEF VAR summ_pok     AS DECIMAL NO-UNDO.              /* �⮨����� �ਮ��⠥��� �ࠢ (�ॡ������) (�.2.1 �������) */        
DEF VAR summ_zal     AS DECIMAL NO-UNDO.              /* ��������� �⮨����� �।��� ������ �� ���� �뤠� �।�� */        
DEF VAR summ_beg_zal AS DECIMAL NO-UNDO.              /* �뭮筠� �⮨����� �।��� ������ �� ���� �뤠� �।�� */
DEF VAR model_zal    AS CHAR NO-UNDO.                 /* ��ઠ,������ ����࠭ᯮ�⭮�� �।�⢠ �।��� ������ */               
DEF VAR year_zal     AS CHAR NO-UNDO.                 /* ��� ���᪠ ����࠭ᯮ�⭮�� �।�⢠ */
DEF VAR vin_zal      AS CHAR NO-UNDO.                 /* VIN */     
DEF VAR pts          AS CHAR NO-UNDO.                 /* ��� */     
  
DEFINE TEMP-TABLE otchpr
        FIELD num          AS int64                /* N �� */
        FIELD region       AS CHAR                 /* ������ �뤠� �।�� */
        FIELD region_cl    AS CHAR                 /* ������ ॣ����樨 ����騪� */
        FIELD filialSouz   AS CHAR                 /* ������� ����� ���� */
        FIELD name_cl      AS CHAR                 /* ��� ����騪� */
        FIELD date_cl      AS DATE                 /* ��� ஦����� ����騪� */
        FIELD num_cl       AS int64                /* ����� ������ */                
        FIELD cont_code    AS CHAR                 /* � �।�⭮�� ������� */                
        FIELD date_cr      AS DATE                 /* ��� �뤠� �।�� */
        FIELD date_cr_end  AS DATE                 /* ��� ���祭�� �ப� �।�� */
        FIELD summ_cr      AS DECIMAL              /* �㬬� �।�� */
        FIELD stavka_cr    AS DECIMAL              /* ��業⭠� �⠢�� �� ���� ���㯪� �ࠢ (�ॡ������) */
        FIELD date_plat    AS int64                /* ��� ���⥦� �� ��䨪� */
        FIELD annuitet     AS DECIMAL              /* ������ ⥪�饣� �����⭮�� ���⥦� */
        FIELD summ_od      AS DECIMAL              /* ���⮪ �᭮����� ����� �� ���� ���室� �ࠢ */ 
        FIELD summ_proc    AS DECIMAL              /* �㬬� ���᫥���� ��業⮢ �� ���� ���室� �ࠢ */         
        FIELD summ_zad     AS DECIMAL              /* ���� �㬬� ������������ �� �।�⭮�� �� ���� ���室� �ࠢ (�.15+ �.16) */        
        FIELD summ_pok     AS DECIMAL              /* �⮨����� �ਮ��⠥��� �ࠢ (�ॡ������) (�.2.1 �������) */        
        FIELD summ_zal     AS DECIMAL              /* ��������� �⮨����� �।��� ������ �� ���� �뤠� �।�� */        
        FIELD summ_beg_zal AS DECIMAL              /* �뭮筠� �⮨����� �।��� ������ �� ���� �뤠� �।�� */
        FIELD model_zal    AS char                 /* ��ઠ,������ ����࠭ᯮ�⭮�� �।�⢠ �।��� ������ */               
        FIELD year_zal     AS char                 /* ��� ���᪠ ����࠭ᯮ�⭮�� �।�⢠ */
        FIELD vin_zal      AS char                 /* VIN */       
        FIELD pts          AS char                 /* ��� */       
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
    FIND FIRST code WHERE code.class EQ '��த���।���'
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
            and signs.code = '���ॣ���'
            and signs.surrogate = string(person.person-id) no-lock no-error.
        if avail signs then do:    
            find first code where code.class = '���ॣ���' 
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
        AND comm-rate.commission = '%�।'
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
				AND signs.code = '����⯫��'
				AND signs.surrogate = loan.contract + ',' + loan.cont-code + ',' + STRING(DAY(loan-cond.since),"99") + '/' + STRING(MONTH(loan-cond.since),"99") + '/' + SUBSTRING(STRING(YEAR(loan-cond.since),"9999"),3) 
				NO-LOCK NO-ERROR.
				IF AVAIL signs THEN annuitet = signs.dec-value.
           	date_plat = loan-cond.int-date.		
		END.

	summ_od = 0.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।��'
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
        AND loan-acct.acct-type = '�।�'
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
		AND loan-acct.acct-type = '�।��'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = abs(sh-bal).
		ELSE summ_zad = abs(sh-val).
	END.

	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।��%'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.

	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।��%�'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.
    
    FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।�'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.    
	
    FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।��'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.
		
    FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।��1'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zad = summ_zad + abs(sh-bal).
		ELSE summ_zad = summ_zad + abs(sh-val).
	END.    
	
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।���'
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
		AND loan-acct.acct-type = '�।��'
	NO-LOCK NO-ERROR.
	IF AVAIL loan-acct THEN DO:
		RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, end-date, end-date, ?).
		IF loan-acct.currency = '' THEN
			summ_zal = abs(sh-bal).
		ELSE summ_zal = abs(sh-val).
	END.
	FIND LAST loan-acct OF loan WHERE 
		loan-acct.since <= end-date 
		AND loan-acct.acct-type = '�।��1'
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
'� �/�;������ �뤠� �।��;������ ॣ����樨 ����騪�;������� ����� ����;��� ����騪�;��� ஦����� ����騪�;����� ������;� �।�⭮�� �������;��� �뤠� �।��;��� ���祭�� �ப� �।��;�㬬� �।��;��業⭠� �⠢�� �� ���� ���㯪� �ࠢ (�ॡ������);��� ���⥦� �� ��䨪�;������ ⥪�饣� �����⭮�� ���⥦�;���⮪ �᭮����� ����� �� ���� ���室� �ࠢ ;�㬬� ���᫥���� ��業⮢ �� ���� ���室� �ࠢ;���� �㬬� ������������ �� �।�⭮�� �� ���� ���室� �ࠢ (�.15+ �.16);�⮨����� �ਮ��⠥��� �ࠢ (�ॡ������) (�.2.1 �������);��������� �⮨����� �।��� ������ �� ���� �뤠� �।��;�뭮筠� �⮨����� �।��� ������ �� ���� �뤠� �।��;��ઠ, ������ ����࠭ᯮ�⭮�� �।�⢠ - �।��� ������;��� ���᪠ ����࠭ᯮ�⭮�� �।�⢠;VIN;���' skip
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


  

  