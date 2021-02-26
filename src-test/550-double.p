{globals.i}


RUN poisk("person").
RUN poisk("cust-corp").



PROCEDURE poisk:
    def INPUT PARAMETER tabl   AS char NO-UNDO. 
 
 FOR EACH tmpsigns WHERE 
       tmpsigns.file-name  EQ tabl  
       and tmpsigns.code EQ "�業����᪠" 
       and can-do("*550-�*550-�*", tmpsigns.xattr-value)      
       NO-LOCK BY tmpsigns.since:   
            MESSAGE   '��� ������:'  tmpsigns.surrogate    tmpsigns.xattr-value        VIEW-AS ALERT-BOX .
end.

IF NOT AVAIL tmpsigns THEN
MESSAGE   "���������� �� " + if tabl eq "person" then "��/��" else "��" + " �� �������"   VIEW-AS ALERT-BOX .
 
    
END PROCEDURE.