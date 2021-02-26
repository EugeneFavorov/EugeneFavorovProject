 /*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"               
     Filename: open-lrko.p
      Comment: Открытие договоров РКО 
   Parameters:
         Uses:
      Used by:
      Created: 
*/

{globals.i}
{prn-doc.def &with_proc=YES}
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{dpsproc.def}
{ksh-defs.i NEW}


   DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mName	     AS CHARACTER EXTENT 2 NO-UNDO.
   DEF BUFFER bacct for acct.
   
   DEFINE VARIABLE mUser     AS CHAR  NO-UNDO.
   DEFINE VARIABLE iBranch   AS CHAR  NO-UNDO.
   DEFINE VARIABLE tmpdate   as date  no-UNDO.  
   DEFINE VARIABLE Summa     as dec   no-UNDO.  
   DEFINE VARIABLE Srok      as int   no-UNDO.  
   DEFINE VARIABLE tarif     as dec   no-UNDO.  
   DEFINE VARIABLE NCode     as Char  no-UNDO.  
   
   mUser =  USERID("bisquit").   
   find first tmprecid NO-LOCK no-error.
   if not avail tmprecid then do: 
      message " Не отметили счёт." view-as alert-box. 
      return.
   end.
   find FIRST acct WHERE RECID(acct) EQ tmprecid.id NO-LOCK no-error.
   if not avail acct then do:
      message " Не нашли счёт." view-as alert-box.
      return.
   end.
   if acct.close-date <> ? then do:
      message " Счёт закрыт. " view-as alert-box.
      return.
   end.

/* pda */
MESSAGE "Подтвердите открытие Договора РКО" SKIP 
        "для счета № " 
        + STRING(REPLACE(acct.acct, " ", "")) VIEW-AS ALERT-BOX WARNING 
                                              BUTTONS YES-NO 
                                              SET i AS LOG.
IF i EQ no OR i EQ ? 
THEN RETURN.
/**/

   find first loan-acct where loan-acct.acct eq acct.acct
                          and loan-acct.acct-type eq "Расчет"
                          no-lock no-error.
   if avail loan-acct then do:
        message " Для счета " acct.acct " уже открыт договор " loan-acct.cont-code ". Нельзя открыть два договора." view-as alert-box.
        return.
   end.
   NCode = trim(string(acct.cust-id)) + "/RUR".
   create loan.
   assign
      loan.branch-id         = shfilial  
      loan.filial-id         = shfilial  
      loan.Class-Code        = "loanr" 
      loan.conf-date         = today  
      loan.contract          = "Расчет"   
      loan.cont-code         = NCode
      loan.currency          = acct.currency   
      loan.cust-cat          = acct.cust-cat    
      loan.cust-id           = acct.cust-id    
      loan.user-id           = mUser    
      loan.open-date         = today
      loan.parent-cont-code  = ""
      loan.parent-contract   = ""
      loan.since             = ?
      loan.loan-status       = "ВВЕД"  
      loan.op-kind           = "СоздДог"  
      loan.warr-cat          = acct.cust-cat
      loan.comment           = "Расчет"   
      loan.interest[10]      = 1.
      loan.op-template       = 1
   .

   create loan-acct.
   assign
      loan-acct.contract   = loan.contract
      loan-acct.cont-code  = loan.cont-code
      loan-acct.acct-type  = "Расчет"
      loan-acct.acct       = acct.acct
      loan-acct.currency   = acct.currency
      loan-acct.since      = loan.open-date
   .
message "Открыт договор " loan.cont-code view-as alert-box.
return.



