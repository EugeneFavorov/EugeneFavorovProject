/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: RPTEPS_XL.P
      Comment: Отчет "Уведомление о полной стоимости кредита (формат MS EXCEL)"
   Parameters: Нет
         Uses:
      Used by:
      Created: 09.08.2010 18:57 BOES    
     Modified: 09.08.2010 18:57 BOES    
*/

&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{lshpr.pro} 
{t-otch.i NEW}

{svarloan.def NEW}
{loan.pro}
{intrface.get comm}
{intrface.get instrum}
{intrface.get card}

{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{ksh-defs.i NEW}

DEF VAR vNameCl AS CHARACTER NO-UNDO.
DEF VAR vCurStr AS CHARACTER NO-UNDO.
DEF VAR mBalance  AS DECIMAL NO-UNDO.
DEF VAR mStrTable AS CHAR    NO-UNDO.
DEF VAR mSt       AS CHAR    NO-UNDO.
DEF VAR mSrok     AS DECIMAL NO-UNDO.

def var nds as dec.
def var doh as dec.
def var ost-t as dec.
def var ost-r as dec.
def var raz   as dec.

DEF BUFFER bterm-obl for term-obl.

/*
{getdate.i} /*теперь отчет на заданную дату*/
*/
{empty ttnames}


RUN Insert_TTName ("info", ""). 

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'info'
NO-LOCK NO-ERROR.

for each loan where loan.contract begins "Аренда"
                and loan.loan-status = "ВВЕД"
                and loan.filial-id = shfilial 
                no-lock.
    nds = 0.
    doh = 0.

   for each term-obl where term-obl.contract = loan.contract
                       and term-obl.cont-code = loan.cont-code
                       and term-obl.end-date > today
                    .
/*     export term-obl. */
     nds = nds + term-obl.int-amt.
     doh = doh + term-obl.amt-rub.
   end.
/*   export doh nds . */
   find last loan-acct where loan-acct.contract = loan.contract
                         and loan-acct.cont-code = loan.cont-code
                         and loan-acct.acct-type = "АрдБудДох"
                         no-lock no-error.
   if avail loan-acct then do:
      run acct-pos in h_base (loan-acct.acct,loan-acct.currency,today,today,?).
      ost-t = - sh-bal.
      ost-r = nds + doh.
      raz = ost-t - ost-r.
      if raz <> 0 then do:
/*
         output to "ivv.txt" append.
         export loan-acct.cont-code loan-acct.acct  "      " raz "      "  ost-t "      " ost-r "      "  nds "      "  doh.
         output close.
*/
         mStrTable = mStrTable + (IF {assigned mStrTable} THEN "~n"
                                                           ELSE "") +
         TRIM(STRING(loan-acct.cont-code)) + "~n" +  
         "`" + TRIM(STRING(loan-acct.acct)) + "~n" +  
         TRIM(STRING(raz)) + "~n" +  
         TRIM(STRING(ost-t)) + "~n" +  
         TRIM(STRING(ost-r)) + "~n" +  
         TRIM(STRING(nds)) + "~n" +                              
         TRIM(STRING(doh))
         .
      end.
   end.

END.

RUN INSERT_TTNAME ("Info", "").

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'Info'
NO-LOCK NO-ERROR.
ttnames.tvalue = mStrTable.

RUN printvd.p ("safe_repl",INPUT TABLE ttnames).


{intrface.del} 

