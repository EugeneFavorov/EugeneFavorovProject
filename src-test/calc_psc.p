/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
   Filename:
   Comment: Вычисляем ПСК
   Comment: Функции расчета добавлены в pp-pqres
   Parameters:
         Uses:
      Used by:
      Created: kam
*/

{globals.i}
{tmprecid.def}

{norm.i NEW}

{fill-graphp.def}
{svarloan.def NEW}

{intrface.get pqres}
{intrface.get xclass}

def var vEps as decimal no-undo.
def var count as decimal no-undo init 0.
def var tmpchar as char no-undo.

FOR EACH tmprecid NO-LOCK,
         EACH loan WHERE RECID(loan) = tmprecid.id NO-LOCK:
	RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.since, OUTPUT vEps).
	/* message string(loan.cont-code) + ' ' + string(vEps) view-as alert-box. */
	
	UpdateSigns('loan',
                  loan.contract + "," + loan.cont-code,
                  'ПСК',
                  trim(string(vEps, "->>>>9.999")),
                  no).

    UpdateSigns('loan',
                  loan.contract + "," + loan.cont-code,
                  'ЭПС',
                  trim(string(vEps, "->>>>9.999")),
                  yes).
                  				
    RUN SetSysConf IN h_base ("ПСКБезСтрах","1").  	
    		
	RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.since, OUTPUT vEps).
	   UpdateSigns('loan',
                  loan.contract + "," + loan.cont-code,
                  'ПСКБезСтрах',
                  trim(string(vEps, "->>>>9.999")),
                  yes).			
/* message string(vEps) view-as alert-box. */
	run deleteolddataprotocol in h_base("ПСКБезСтрах") .
	
	count = count + 1.
END.
message 'расчитана ПСК для ' + string(count) + ' договоров' view-as alert-box.

{intrface.del}

