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
   def var Str           as char    no-undo.
   def var j             as int     no-undo.
   def var lenStr        as int     no-undo.

   pick-value = "".
   iCode  = entry(1,in-str,chr(1)).
   iDelim = entry(2,in-str,chr(1)).
   iStr   = entry(3,in-str,chr(1)).
   newStr = "".
   do i = 1 to num-entries(iStr,iDelim) :
      find first code where  code.class = "СтруктШтрКод"
                        and  code.parent = iCode
                        and  code.val = string(i * 2 - 1)
                        no-lock no-error.
      if avail code then do:
         len_ = code.misc[1].
         Str = entry(i,iStr,iDelim) .

         j = int(len_) - length(Str) .
         if j < 0 then do:
            message "Длина элемента номер " i " штрих-кода больше , чем в описании штрих-кода на " abs(j) " символов." view-as alert-box.
            pick-value = "".
            return. 
         end.
         newStr = newStr + Str + fill(" ", j) + iDelim.
      end. 
   end.
   pick-value = newStr.
   return. 


   
   
   
   
   
   