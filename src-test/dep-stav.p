   DEFINE INPUT PARAMETER in-str   as char NO-UNDO.
   {globals.i}
   {justasec}
   {intrface.get refer}    /* Библиотека службы справочников. */
   {intrface.get date}
/*
ПродЛин,БзУдоб+,07/12/15 
*/
   def var iContract     as char    no-undo.
   def var iCont-Code    as char    no-undo.
   def var iProdSt       as char    no-undo.
   def var iBranch       as char    no-undo.
   DEF var user_         AS CHAR    NO-UNDO.                                
   DEF BUFFER bcomm-rate  FOR comm-rate.


   user_ =  USERID("bisquit").     
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
   iContract    = entry(1,in-str,"|").
   iCont-Code   = entry(2,in-str,"|").
   iProdSt      = entry(3,in-str,"|").

   find first loan where loan.contract = iContract
                     and loan.cont-code = iCont-Code
                     and loan.filial-id = shfilial
                     no-lock no-error.
   if not avail loan then return.
   for each code where code.class = "ПродЛин"
                   and code.code  = iProdSt
                     no-lock.
      FOR each bcomm-rate WHERE bcomm-rate.filial-id eq shfilial
                           AND bcomm-rate.since <= today
                           AND bcomm-rate.kau EQ "ПродТрф," + code.misc[7]
                           USE-INDEX kau
                           NO-LOCK.


         find first comm-rate where comm-rate.filial-id  = shfilial
                                and comm-rate.branch-id  = ""
                                and comm-rate.commission = bcomm-rate.commission
                                and comm-rate.acct       = "0"
                                and comm-rate.currency   = bcomm-rate.currency
                                and comm-rate.kau        = loan.contract + "," + loan.cont-code
                                and comm-rate.min-value  = 0
                                and comm-rate.period     = 0
                                and comm-rate.since      = loan.open-date
                                no-error.

         if not avail comm-rate then CREATE comm-rate.
         ASSIGN
            comm-rate.commission = bcomm-rate.commission
            comm-rate.currency   = bcomm-rate.currency
            comm-rate.min-value  = 0
            comm-rate.acct       = "0"
            comm-rate.rate-comm  = bcomm-rate.rate-comm
            comm-rate.since      = loan.open-date
            comm-rate.rate-fixed = bcomm-rate.rate-fixed
            comm-rate.period     = 0
            comm-rate.kau        = loan.contract + "," + loan.cont-code
            comm-rate.branch-id  = ""
            comm-rate.filial-id  = shfilial
         .

      end.
   end.
   return.   
   
   
   
   
   
   
   
   