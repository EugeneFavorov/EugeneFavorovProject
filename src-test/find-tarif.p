{globals.i}
    def input param summa  as char  no-undo.
    def input param srok   as char  no-undo.
    FIND last  comm-rate WHERE comm-rate.filial-id  eq shfilial
                           AND comm-rate.branch-id  eq ""
                           and comm-rate.commission EQ "çé" 
                           AND comm-rate.acct       EQ "0"                    
                           AND comm-rate.currency EQ ""
                           AND comm-rate.kau EQ 'í†‡®‰Îêäé,çé'               
                           AND comm-rate.min-value <= dec(summa)    
                           AND comm-rate.period <= int(srok)
                           AND comm-rate.since  <= today                   

                           USE-INDEX comm-rate
                           NO-ERROR. 
    if avail comm-rate 
    then do:
       pick-value = string(comm-rate.rate-comm) .
    end.
    else do:
       pick-value = "0".
    end.
return .

