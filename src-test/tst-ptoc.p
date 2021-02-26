{all_note.def}
def var bError as char no-undo.
output to "all_recids.err".
for each  all_recids .
   bError = "".
   IF  SEARCH("vb_kart.r") <> ? THEN
   DO:
      run vb_kart(all_recids.rid,OUTPUT bError).
      if bError <> "" then do:
         export all_recids.
         export bError.
         delete all_recids.
      end.
   END.
   ELSE DO:
         bError = "Не найден файл проверки картотек vb_kart.r".
         export all_recids.
         export bError.
         delete all_recids.    
   END.
end.
output close.

