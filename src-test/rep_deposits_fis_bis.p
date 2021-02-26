/* аналог Rep_Deposits_Fis_Bis + % ставка */

{globals.i}
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
{client.i}
{tmprecid.def}
{wordwrap.def}
{navigate.def}
{flt-file.i}
{sh-defs.i}
{ksh-defs.i NEW}

{intrface.get dps}
{dpsproc.def}

def new shared stream vvs.
def var fname as char no-undo.
DEF VAR mMinOst AS decimal NO-UNDO.
DEF VAR mName AS CHARACTER NO-UNDO.
DEF VAR mAcctN AS CHARACTER NO-UNDO.
DEF VAR mAcctTR AS CHARACTER NO-UNDO.
DEF VAR mAcctCurTR AS CHARACTER NO-UNDO.
DEF VAR mAcctCur AS CHARACTER NO-UNDO.
DEF VAR mAcctBal AS DECIMAL NO-UNDO.
DEF VAR mAcctBalTR AS DECIMAL NO-UNDO.
DEF VAR mAcctProc AS CHARACTER NO-UNDO.
DEF VAR mAcctProcCur AS CHARACTER NO-UNDO.
DEF VAR mAcctProcBal AS DECIMAL NO-UNDO.
DEF VAR mCity AS character NO-UNDO.
DEF VAR mCurr AS CHARACTER NO-UNDO.
DEF VAR mIntPeriod AS character NO-UNDO.
DEF VAR vRateComm AS DEC  NO-UNDO. /*ставка на момент открытия*/
DEF VAR mPenComm AS CHAR  NO-UNDO. 
def var rateclose as dec no-undo. /* ставка досрочки */
DEF VAR vValOst AS DEC  NO-UNDO.
DEF VAR vPeriod AS INT  NO-UNDO.
def var vBegDate as date no-undo.
def var vEndDate as date no-undo.
def var vBegDate1 as date no-undo.
def var dProl as date no-undo.
DEF VAR mSumm AS DECIMAL NO-UNDO.
DEF VAR vComm     AS char NO-UNDO. /*код комисии*/
DEF VAR vInter    AS CHAR NO-UNDO. /*схема начисления*/
def var vSubCod as char no-undo.
DEF VAR vResult   AS DEC  NO-UNDO.
DEF VAR vResult1  AS DEC  NO-UNDO.
DEF VAR in-kau AS CHAR  NO-UNDO.
def var ni as int64 no-undo.
DEF VAR vFlag     AS LOG  NO-UNDO. /*начисления */
def var mCid as char no-undo.

def temp-table tt-dps
    FIELD num as int64
    FIELD city as character
    FIELD cid AS character
    FIELD cli-name AS CHARACTER
    FIELD cont-code AS CHARACTER
    FIELD cont-type AS CHARACTER
	FIELD curr AS character
	FIELD period AS char
	FIELD summ AS DECIMAL
    FIELD comm AS decimal
    FIELD rateclose AS DECIMAL
    FIELD open-date AS date
    FIELD end-date AS date
    FIELD intperiod AS CHARACTER
    FIELD minost AS DECIMAL
    FIELD acct AS CHARACTER
    FIELD acctbal AS DECIMAL
    FIELD acctbaltr AS DECIMAL
    FIELD accttr AS CHARACTER
    FIELD acctproc AS CHARACTER
    FIELD acctprocbal AS DECIMAL
    .

{getdate.i}

fname = "./deposits_" + replace(string(end-date,"99.99.9999"),".","_") + "_" + userid('bisquit') + ".xml".
ni = 1.
FOR EACH tmprecid,
   FIRST loan WHERE RECID(loan) EQ tmprecid.id no-lock,
   FIRST loan-cond where loan-cond.cont-code eq loan.cont-code and loan-cond.contract eq loan.contract
NO-LOCK:
	
	/* CID */
	mCid = GetXattrValueEx("person",string(loan.cust-id),"CID","").
	
	/*   Сумма вклада   */
    RUN get-summ-ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT mSumm).
	  
	/* Мин остаток на вкладе */
    RUN get_last_min_ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT mMinOst).
    if mMinOst = ? then mMinOst=0.

	/*   Наименование клиента   */
    find first cust-role
	    where (loan.cust-cat=cust-role.cust-cat) and (string(loan.cust-id) = cust-role.cust-id) no-lock no-error.
    if avail cust-role then mName = cust-role.short-name. else mName = "".

	/*   Счет депозита   */
    FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract
      			AND loan-acct.cont-code EQ loan.cont-code
			AND loan-acct.acct-type EQ (if loan.end-date <> ? then "loan-dps-t" else "loan-dps-p")
		        NO-LOCK NO-ERROR.

      mAcctN   = IF AVAIL loan-acct THEN loan-acct.acct ELSE "".
      mAcctCur = IF AVAIL loan-acct THEN loan-acct.currency ELSE "".

FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract
			AND loan-acct.cont-code EQ loan.cont-code
			AND loan-acct.acct-type EQ "loan-dps-tr"
		        NO-LOCK NO-ERROR.
  
    mAcctTR   = IF AVAIL loan-acct THEN loan-acct.acct ELSE "".
      mAcctCurTR = IF AVAIL loan-acct THEN loan-acct.currency ELSE "".

	/*   Остаток на депоз. счете   */
    if mAcctN <> "" then do:
		RUN acct-pos IN h_base (mAcctN, mAcctCur, end-date, end-date, ?).
		mAcctBal = IF mAcctCur EQ "" THEN abs(sh-bal)
                   ELSE abs(sh-val).
    end. 
	ELSE mAcctBal = 0.
	/*   Остаток на TR. счете   */
	    if mAcctTR <> "" then do:
	    RUN acct-pos IN h_base (mAcctTR, mAcctCurTR, end-date, end-date, ?).
	    mAcctBalTR = IF mAcctCurTR EQ "" THEN abs(sh-bal)
	                       ELSE abs(sh-val).
	                           end. 
	                ELSE mAcctBalTR = 0.

	/*   Счет процентов   */
    FIND FIRST loan-acct WHERE loan-acct.contract  EQ loan.contract
        AND loan-acct.cont-code EQ loan.cont-code AND loan-acct.acct-type EQ "loan-dps-int"
     	NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
	mAcctProc = loan-acct.acct.
	mAcctProcCur = loan-acct.currency.
		END. ELSE DO:
	mAcctProc = "".
	mAcctProcCur = "".
      END.

	/*   Остаток на счете процентов   */
    if mAcctProc <> "" then do:
	      RUN acct-pos IN h_base (mAcctProc, mAcctProcCur, end-date, end-date, ?).
	      mAcctProcBal = IF mAcctProcCur EQ "" THEN abs(sh-bal)
                   ELSE abs(sh-val).
     end. 
	 ELSE mAcctProcBal = 0.

	/*   Город   */
    FIND FIRST branch WHERE branch.branch-id  EQ loan.branch-id NO-LOCK NO-ERROR.
    IF AVAIL branch THEN DO:
	mCity = branch.name.
    END.
	ELSE mCity = "".

	/*   Валюта   */
     If loan.curr = "" then mCurr = "810". else mCurr = loan.curr.

	/*   Выплата процентов   */
    FIND FIRST code WHERE code.parent = "int-period" and code.code =  loan-cond.int-period NO-LOCK NO-ERROR.
    IF AVAIL code THEN DO:
		mIntPeriod = code.name.
    END.
	ELSE mIntPeriod = "".

	/*   Нач последнего периода   */
    RUN get-beg-date-prol IN h_dpspc (RECID(loan), end-date, OUTPUT vBegDate, OUTPUT vEndDate).
    if vBegDate <> loan.open-date
		then dProl = vBegDate.
	else dProl = ?.

    ASSIGN
    vRateComm = ?
    mPenComm = ?
    vValOst =?
    vPeriod = ?
    rateclose = ?
    .
	
	IF mAcctN <> "" THEN DO:
        FIND FIRST acct WHERE acct.acct EQ mAcctN NO-LOCK NO-ERROR.        
        /*   процент досрочки   */
        RUN Get_Last_Pen-Commi IN h_dpspc (RECID(loan), end-date, end-date, OUTPUT mPenComm).
        /* если нет штрафной комиссии, то берем основную */
        IF NOT {assigned mPenComm} THEN
            RUN Get_Last_Commi in h_dpspc (RECID(loan), vBegDate, end-date, OUTPUT mPenComm).
        in-kau  = loan.contract + "," + 
             loan.cont-code + "," + 
             IF loan.end-date = ? 
               THEN "ОстВклВ" 
               ELSE "ОстВклС".
        /*реальный остаток на вкладе на день открытия */
        run kau-pos.p (acct.acct,
                   acct.currency,
                   vBegDate,
                   vBegDate,
                   gop-status,
                   in-kau).
         vValOst = ABS(if acct.currency eq '' then ksh-bal  else ksh-val) . 
	vPeriod = end-date - vBegDate.

    IF {assigned mPenComm} THEN
       DO:
         {findcom1.i
          &dir       = LAST
          &comm-rate = comm-rate
          &rcom      = mPenComm
          &rsum      = vValOst
          &since1    = "LE vBegDate"
          &vPeriodInt = vPeriod
         }
         rateclose = IF AVAIL comm-rate THEN comm-rate.rate-comm ELSE 0.
    end.
    else rateclose = ?.
	
	/*   Определение кода ставки   */
    RUN Get_Last_Commi in h_dpspc (RECID(loan), vBegDate, end-date, OUTPUT vComm).
    RUN Get_Last_Inter in h_dpspc (RECID(loan), vBegDate, end-date, OUTPUT vInter).
    IF vComm  = ? OR vComm  = "?" OR
           vInter = ? OR vInter = "?" THEN NEXT.
	vSubCod = IF loan.end-date NE ? THEN 'ОстВклС' ELSE 'ОстВклВ'.

        { findsch.i
             &dir    = last
             &sch    = vInter
             &since1 = " le end-date"
        }
        IF AVAIL interest-sch-line
        THEN DO:
          vBegDate1 = end-date - 1.
          RUN nachkin.p(RECID(interest-sch-line),
                        vComm,
                        RECID(acct),
                        end-date,
                        loan.contract + "," + loan.cont-code + "," + vSubCod ,
                        yes,
                        OUTPUT vResult,
                        OUTPUT vResult1,
                        INPUT-OUTPUT vBegDate1,
                        OUTPUT vFlag).
           vRateComm = DEC(ENTRY(4,RETURN-VALUE)) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN vRateComm=?.
        END.
        ELSE DO:
          RUN GetCommRate(BUFFER acct, vInter, vComm, OUTPUT vRateComm ).
        END.
    END.
	
	CREATE tt-dps.
      ASSIGN
     tt-dps.num = ni
     tt-dps.city = mCity
     tt-dps.cid = mCid
     tt-dps.cli-name = mName
     tt-dps.cont-code = loan.cont-code
	 tt-dps.cont-type = loan.cont-type
	 tt-dps.curr = mcurr
	 tt-dps.period = if vEndDate <> ? then string(vEndDate - vBegDate) else ''
     tt-dps.summ = mSumm
     tt-dps.comm = vRateComm
     tt-dps.rateclose = rateclose
     tt-dps.open-date = loan.open-date
     tt-dps.end-date = loan.end-date
     tt-dps.intperiod = mIntPeriod
	 tt-dps.minost = mMinOst
	 tt-dps.acct = substring(mAcctN,1,20)
	 tt-dps.acctbal = mAcctBal
	 tt-dps.accttr = substring(mAcctTR,1,20)
	 tt-dps.acctbaltr = mAcctBalTR
	 tt-dps.acctproc = substring(mAcctProc,1,20)
	 tt-dps.acctprocbal = mAcctProcBal
     .
	ni = ni + 1.
END. 

output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
  put stream vvs unformatted '
<?xml version="1.0"?>\n
<?mso-application progid="Excel.Sheet"?>\n
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"\n
 xmlns:o="urn:schemas-microsoft-com:office:office"\n
 xmlns:x="urn:schemas-microsoft-com:office:excel"\n
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"\n
 xmlns:html="http://www.w3.org/TR/REC-html40">\n
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">\n
  <Version>14.00</Version>\n
 </DocumentProperties>\n
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">\n
  <AllowPNG/>\n
 </OfficeDocumentSettings>\n
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">\n
  <ProtectStructure>False</ProtectStructure>\n
  <ProtectWindows>False</ProtectWindows>\n
 </ExcelWorkbook>\n
 <Styles>\n
  <Style ss:ID="Default" ss:Name="Normal">\n
   <Alignment ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"\n
    ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="s62">\n
   <NumberFormat ss:Format="0"/>\n
  </Style>\n
  <Style ss:ID="s63">\n
   <NumberFormat ss:Format="@"/>\n
  </Style>\n
  <Style ss:ID="s64">\n
   <NumberFormat ss:Format="Fixed"/>\n
  </Style>\n
  <Style ss:ID="s65">\n
   <NumberFormat ss:Format="Short Date"/>\n
  </Style>\n
  <Style ss:ID="s66">\n
   <NumberFormat ss:Format="Standard"/>\n
  </Style>\n
 </Styles>\n
  <Worksheet ss:Name="Rep_Deposits_Fis_Bis">\n
  <Table>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="27"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="54.75"/>\n
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="47.25"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="168.75"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="90"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="103.5"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="37.5"/>\n
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="71.25"/>\n
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="47.25"/>\n
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="81.75"/>\n
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="60.75"/>\n
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="62.25"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="92.25"/>\n
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="78.75"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="117.75"/>\n
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="73.5"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="128.25"/>\n
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="81"/>\n
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="128.25"/>\n
   <Column ss:StyleID="s64" ss:AutoFitWidth="0" ss:Width="81"/>\n^
   <Row ss:AutoFitHeight="0">\n
    <Cell><Data ss:Type="String">N</Data></Cell>\n
    <Cell><Data ss:Type="String">Город</Data></Cell>\n
    <Cell><Data ss:Type="String">CID</Data></Cell>\n
    <Cell><Data ss:Type="String">Клиент</Data></Cell>\n
    <Cell><Data ss:Type="String">Договор</Data></Cell>\n
    <Cell><Data ss:Type="String">Тип вклада</Data></Cell>\n
    <Cell><Data ss:Type="String">Валюта</Data></Cell>\n
    <Cell><Data ss:Type="String">Срок</Data></Cell>\n
    <Cell><Data ss:Type="String">Сумма</Data></Cell>\n
    <Cell><Data ss:Type="String">Ставка</Data></Cell>\n
    <Cell><Data ss:Type="String">Ставка растор.</Data></Cell>\n
    <Cell><Data ss:Type="String">Дата открытия</Data></Cell>\n
    <Cell><Data ss:Type="String">Дата окончания</Data></Cell>\n
    <Cell><Data ss:Type="String">Выплата процентов</Data></Cell>\n
    <Cell><Data ss:Type="String">Неснижаемый остаток</Data></Cell>\n
    <Cell><Data ss:Type="String">Счет</Data></Cell>\n
    <Cell><Data ss:Type="String">Остаток</Data></Cell>\n
    <Cell><Data ss:Type="String">Счет переч.</Data></Cell>\n^
    <Cell><Data ss:Type="String">Остаток переч.</Data></Cell>\n^
    <Cell><Data ss:Type="String">Счет проц.</Data></Cell>\n
    <Cell><Data ss:Type="String">Остаток проц.</Data></Cell>\n
   </Row>\n
    '.

for each tt-dps no-lock by tt-dps.num:
	put stream vvs unformatted '<Row ss:AutoFitHeight="0">\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + string(tt-dps.num) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.city + '</Data></Cell>\n'.
	put stream vvs unformatted '<Cell><Data ss:Type="Number">' + string(tt-dps.cid) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.cli-name + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.cont-code + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.cont-type + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.curr + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.period + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + trim(string(tt-dps.summ, "->>>>>>>>>9.99")) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + trim(string(tt-dps.comm, "->>9.99")) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + trim(string(tt-dps.rateclose, "->>9.99")) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="DateTime">' + string(year(tt-dps.open-date),"9999") + '-' + string(month(tt-dps.open-date),"99") + '-' + string(day(tt-dps.open-date),"99") + 'T00:00:00.000</Data></Cell>\n'.
	if(tt-dps.end-date <> ?) then do:
		put stream vvs unformatted '<Cell><Data ss:Type="DateTime">' + string(year(tt-dps.end-date),"9999") + '-' + string(month(tt-dps.end-date),"99") + '-' + string(day(tt-dps.end-date),"99") + 'T00:00:00.000</Data></Cell>\n'.
	end.
    put stream vvs unformatted '<Cell ss:Index="14"><Data ss:Type="String">' + tt-dps.intperiod + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + trim(string(tt-dps.minost, "->>>>>>>>>9.99")) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.acct + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + trim(string(tt-dps.acctbal, "->>>>>>>>>9.99")) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.accttr + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + trim(string(tt-dps.acctbaltr, "->>>>>>>>>9.99")) + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="String">' + tt-dps.acctproc + '</Data></Cell>\n'.
    put stream vvs unformatted '<Cell><Data ss:Type="Number">' + trim(string(tt-dps.acctprocbal, "->>>>>>>>>9.99")) + '</Data></Cell>\n'.
   put stream vvs unformatted '</Row>\n'.
end.
 put stream vvs unformatted
   '
  </Table>\n
 </Worksheet>\n
</Workbook>\n
'.
  
output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").

