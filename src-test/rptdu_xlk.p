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

/*Функция получения ставки по карте*/
{getdate.i} /*теперь отчет на заданную дату*/
{empty ttnames}

RUN Insert_TTName("AddComm", "Отчет  по ДЮЛ на " + string(end-date,"99.99.9999") ).

RUN Insert_TTName ("info", ""). 

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'info'
NO-LOCK NO-ERROR.
for each loan where loan.contract = "Депоз"
                and (loan.close-date = ? or loan.close-date >= end-date)
               /* and loan.filial-id = shfilial */
                NO-LOCK.
      vNameCl = "Не определён".
      if loan.cust-cat = "Ю" Then do:
         find first cust-corp where cust-corp.cust-id = loan.cust-id no-lock no-error.
         if  avail cust-corp then do:
            vNameCl =  cust-corp.name-corp .
         end.
      end.
      if loan.cust-cat = "Ч" Then do:
         find first person where person.person-id = loan.cust-id no-lock no-error.
         if  avail person then do:
            vNameCl =  person.name-last + " " + person.first-names .
         end.
      end.
      find last  loan-acct where loan-acct.contract = loan.contract
                             and loan-acct.cont-code = loan.cont-code
                             and loan-acct.acct-type = "Депоз" no-lock no-error.
      find  FIRST acct where acct.acct = loan-acct.acct NO-LOCK no-error.
      if not avail acct then do:
         mBalance = 0.
      end.
      else do:
         next.
      end.
      find first term-obl where term-obl.contract = loan.contract
                            and term-obl.cont-code = loan.cont-code 
                            and term-obl.idnt = 2
                            no-lock no-error.
      if not avail term-obl then do:
         mBalance = 0.
      end.
      else do:
         mBalance = term-obl.amt-rub.
      end.
      FIND LAST comm-rate WHERE comm-rate.commission EQ "%Деп"
                            AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code 
                            AND comm-rate.since      GE loan.open-date 
                            AND comm-rate.since      LE end-date 
                            NO-LOCK NO-ERROR.
      if not avail comm-rate then do:
         mSt = "Не определена".
      end.                     
      else do:
         mSt = TRIM(STRING(comm-rate.rate-comm,"999.9999")).
      end.        
      mSrok  =    loan.end-date - loan.open-date + 1 .
      mStrTable = mStrTable + (IF {assigned mStrTable} THEN "~n"
                                                        ELSE "") +
      TRIM(STRING(loan.cust-id)) + "~n" +  
      TRIM(STRING(vNameCl)) + "~n" +  
      TRIM(STRING(loan.cont-type)) + "~n" +  
      "`" + TRIM(STRING(loan.doc-ref)) + "~n" +  
      TRIM(STRING(loan.open-date,"99.99.9999")) + "~n" +  
      TRIM(STRING(mBalance)) + "~n" +                              
      mSt + "~n" +  
      TRIM(STRING(loan.end-date - loan.open-date + 1,"9999")) + "~n" +  
      TRIM(STRING(loan.end-date,"99.99.9999"))  + "~n" +
      "`" + TRIM(STRING(loan.filial-id,"9999")) + "~n" +  
      "`" + TRIM(STRING(loan.branch-id,"9999"))
      .
END.

RUN INSERT_TTNAME ("Info", "").

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'Info'
NO-LOCK NO-ERROR.
ttnames.tvalue = mStrTable.

RUN printvd.p ("rptdu_xli",INPUT TABLE ttnames).


{intrface.del} 
