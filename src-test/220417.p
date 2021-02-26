


{globals.i}
{tmprecid.def}
{intrface.get xclass}
{intrface.get xobj}     /* ¨¡«¨®â¥ª  ¤«ï à ¡®âë á ®¡ê¥ªâ ¬¨. */
{intrface.get dps}
{intrface.get dpspr}

{setdest.i}
{sh-defs.i}
{dpsproc.def}
{getdates.i}
DEF VAR mSumm AS DECIMAL NO-UNDO.
PUT UNFORMATTED
  "ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ " SKIP
  "³ „ â  ®âªà³ „ â  § ªà³  ü ¤®£®¢®à         ³ à®¬®ª®¤ ³ ‘ã¬¬  ¤®£®¢®à   ³ " SKIP
  "ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ " SKIP.


FOR EACH loan WHERE
		 loan.contract EQ 'dps' 
		/* loan.close-date EQ ? AND
		 loan.loan-status EQ "”"*/
                 and loan.Cont-Type EQ 'UdachDv'
and loan.open-date >= beg-date
and loan.open-date <= end-date
	NO-LOCK:
	
	IF GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "promo","") NE "00-000-000"
and GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "promo","") NE ""
  THEN DO:
RUN get-summ-ost IN h_dpspc (RECID (loan),loan.open-date,loan.open-date, OUTPUT mSumm).
/*PUT UNFORMATTED loan.open-date loan.close-date loan.cont-code GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "promo","") mSumm skip.*/
PUT UNFORMATTED
"³" STRING(loan.open-date, "99/99/9999")
"³" (if loan.close-date <> ? THEN STRING(loan.close-date, "99/99/9999") ELSE STRING("","x(10)"))
"³" STRING(loan.cont-code, "x(20)")
"³" STRING(GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "promo",""), "x(10)")
"³" STRING(mSumm, "->,>>>,>>>,>>9.99")
"³" skip.
end.
end.
PUT UNFORMATTED
  "ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" SKIP.

{preview.i}
{intrface.del}