/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created: 
*/
{globals.i}
{tmprecid.def}





   FOR EACH tmprecid NO-LOCK,
      FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK:  

	for each term-obl where term-obl.cont-code = loan.cont-code
	 and term-obl.idnt = 128
	 and term-obl.lnk-contract = 'ПОС'
	 and term-obl.sop-date = date("04/10/2017"):
	 term-obl.sop-date = date("03/10/2017").
	end.
	for each term-obl where term-obl.cont-code = loan.cont-code
	 and term-obl.idnt = 128
	 and term-obl.lnk-contract = 'ПОС'
	 and term-obl.end-date = date("05/10/2017"):
	 term-obl.end-date = date("04/10/2017").
	end.
   END.


{intrface.del}