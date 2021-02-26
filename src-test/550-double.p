{globals.i}


RUN poisk("person").
RUN poisk("cust-corp").



PROCEDURE poisk:
    def INPUT PARAMETER tabl   AS char NO-UNDO. 
 
 FOR EACH tmpsigns WHERE 
       tmpsigns.file-name  EQ tabl  
       and tmpsigns.code EQ "ОценкаРиска" 
       and can-do("*550-П*550-П*", tmpsigns.xattr-value)      
       NO-LOCK BY tmpsigns.since:   
            MESSAGE   'Код клиента:'  tmpsigns.surrogate    tmpsigns.xattr-value        VIEW-AS ALERT-BOX .
end.

IF NOT AVAIL tmpsigns THEN
MESSAGE   "Совпадений по " + if tabl eq "person" then "ФЛ/ИП" else "ЮЛ" + " не найдено"   VIEW-AS ALERT-BOX .
 
    
END PROCEDURE.