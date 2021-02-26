   DEFINE INPUT PARAMETER in-str   as char NO-UNDO.

   {globals.i}
   def var iBranch       as char    no-undo.
   DEF var user_         AS CHAR    NO-UNDO.
   def var iCode         as char    no-undo.  
   def var iDelim        as char    no-undo.
   def var iStr          as char    no-undo.
   def var i             as int     no-undo.
   def var len_          as char    no-undo.
   def var newStr        as char    no-undo.
   def var    Str        as char    no-undo.
   def var j             as int     no-undo.
   def var iBIC          as char    no-undo.
   def var iINN          as char    no-undo.
   def var iAcct         as char    no-undo.
   def var bInnDoc       as char    no-undo.
   def var bBICDoc       as char    no-undo.
   
   def buffer bsigns for signs.
   def buffer csigns for signs.
   def buffer dsigns for signs.
   pick-value = "".
   iDelim = entry(1,in-str,chr(1)).
   iStr   = entry(2,in-str,chr(1)).
   j = num-entries(iStr,iDelim) - 1.
   if j <> 21 and j <> 20 then do:
       message "В считанном штрих-коде " j " , а не 20 или 21 разделитель. Расшифровка кода невозможна. "  view-as alert-box.   
       pick-value = "".
       return. 
   end.
   iBIC   = entry(11,iStr,iDelim) .
   iINN   = entry(15,iStr,iDelim) .
   iAcct  = entry(14,iStr,iDelim) .
   if trim(iINN) = ""  then do:
       message "В считанном штрих-коде отсутствует ИНН получателя. Автоматический поиск получателя не возможен. Воспользуйтесь другой транзакцией ввода документов." view-as alert-box.  
       pick-value = "".
       return. 
   end.
   for each   loan-acct where loan-acct.contract   = "pn"
                          and loan-acct.acct-type  = "ПлатНас"
                          and loan-acct.acct       = iAcct
                          and loan-acct.currency   = "" 
                          no-lock. 
       bInnDoc = GetXattrValueEx("loan", loan-acct.contract + "," + loan-acct.cont-code ,"ПНПолучательИНН",?).
       if bInnDoc = iINN then do:
          bBICDoc = GetXattrValueEx("loan-acct", loan-acct.contract + "," + loan-acct.cont-code + "," + "ПлатНас" + "," + string(loan-acct.since,"99/99/99") ,"БИК",?).
          if bBICDoc = iBIC then do:
             find first loan  where loan.class-code EQ 'loan-pn-par'
                                and loan.parent-cont-code = loan-acct.cont-code
                                and loan.filial-id EQ shfilial
                                NO-LOCK no-error.
             if avail loan then do:
                newStr = "Налоги," + iAcct + "," + iBIC + "," + iINN + ",pn-par," + loan.cont-code.
                pick-value = newStr.
                return. 
             end. 
             else do:
                 message "Договор по ИНН получателя не найден. Воспользуйтесь другой транзакцией ввода документов." view-as alert-box.  
                 pick-value = "".
                 return. 
             end.
          end.
       end.
   end.
   pick-value = newStr.
   return. 

   
   
   
   
   
   