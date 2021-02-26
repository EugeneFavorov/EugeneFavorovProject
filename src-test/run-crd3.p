/*def input param in-acct like op-entry.acct-db no-undo.     */
/*def input param in-currency like op-entry.currency no-undo.*/
/*def input param level as INT64 no-undo.                    */

DEFINE VARIABLE in-acct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE in-currency AS CHARACTER NO-UNDO.
DEFINE VARIABLE level       AS INT64     NO-UNDO.

in-acct = "47416810501100000004     @0000".
in-currency = "".
level       = 5.

def new shared var p-kau-rec as recid initial 0 no-undo.
def var kau-proc like acct.kau-id initial ? no-undo.
def var temp-str as char initial ? no-undo.
def var temp-i   as INT64  initial ? no-undo.
def var sort-str as char no-undo.
def new shared var view-date as date no-undo.

find acct where acct.acct eq in-acct and acct.currency eq in-currency no-lock. 
if acct.kau-id eq ? or acct.kau-id eq "" then do:                                                   
   find bal-acct of acct no-lock.                                              
   if avail bal-acct  and bal-acct.kau-id ne ? and bal-acct.kau-id ne "" then                            
      kau-proc = bal-acct.kau-id .                                             
end.                                                                           
else kau-proc = acct.kau-id.
if kau-proc eq ? then do: 
   return.
end.
find code where code.class eq "ШаблКау" and code.code eq kau-proc              
                                             no-lock no-error.             
if avail code then sort-str = code.misc[6].
else do:
     {message                                    
         &text = "Не найден шаблон КАУ "" + acct.kau-id +     
         "" . Возможно он был кем-то халатно удален !"                             
         &alert-box=error                        
         &color=messages                         
     }                                           
end.

view-date = today.
repeat:
 if temp-str eq ? and lookup("ДатаПлан",sort-str) ne 0 then do:
    temp-str = string(year(view-date),"9999") + "." +
    	       string(month(view-date),"99")  + "." +
    	       string(day(view-date),"99").
    temp-i = lookup("ДатаПлан",sort-str). 
 end. 
 else do:
    temp-i = 1.
    temp-str = "".
    view-date = ?.
 end.
 run "crdv3.p" (in-acct, in-currency,temp-i, temp-str,1,"",1,"",level).
 if lastkey ne keycode("F6") then leave.
end.
