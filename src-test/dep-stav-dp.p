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
   def var iProdStBase   as char    no-undo.
   def var iBranch       as char    no-undo.
   DEF var user_         AS CHAR    NO-UNDO.                                
   DEF BUFFER bcomm-rate  FOR comm-rate.


   user_ =  USERID("bisquit").     
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
   iContract    = entry(1,in-str,"|").
   iCont-Code   = entry(2,in-str,"|").
   iProdSt      = entry(3,in-str,"|").
   iProdStBase  = "".
   if num-entries(in-str,"|") > 3 then do:
      iProdStBase  = entry(4,in-str,"|").
   end.

   find first loan where loan.contract = iContract
                     and loan.cont-code = iCont-Code
                     and loan.filial-id = shfilial
                     no-lock no-error.
   if not avail loan then return.

   FOR each bcomm-rate WHERE bcomm-rate.filial-id eq shfilial
                        AND bcomm-rate.since <= today
                        AND bcomm-rate.kau EQ "ПродТрф," + iProdSt
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

      if not avail comm-rate then do:
         CREATE comm-rate.
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
   FOR each bcomm-rate WHERE bcomm-rate.filial-id eq shfilial
                        AND bcomm-rate.since <= today
                        AND bcomm-rate.kau EQ "ПродТрф," + iProdStBase
                        USE-INDEX kau
                        NO-LOCK.

      if bcomm-rate.commission begins "Опция" then do:
         find first comm-rate where comm-rate.filial-id  = shfilial
                                and comm-rate.branch-id  = ""
                                and comm-rate.commission = "%Деп"
                                and comm-rate.acct       = "0"
                                and comm-rate.currency   = bcomm-rate.currency
                                and comm-rate.kau        = loan.contract + "," + loan.cont-code
                                and comm-rate.min-value  = 0
                                and comm-rate.period     = 0
                                and comm-rate.since      = loan.open-date
                                no-error.

         if not avail comm-rate then  do: 
            CREATE comm-rate.
            comm-rate.rate-comm  = comm-rate.rate-comm + bcomm-rate.rate-comm.
         end.
         else do:
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
         
            if not avail comm-rate then do: 
               CREATE comm-rate.
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
      end.
   end.
return.                                                                     
   
   
   
   
   
   
   