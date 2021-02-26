

for each op-entry where
    substring(op-entry.acct-db,1,5) = '47423'
    and substring(op-entry.acct-cr,1,5) = '30305'
    and year(op-entry.op-date) = 2015
    and op-entry.op-status begins "√"
    and op-entry.user-id <> 'SYNC'
    no-lock, first op of op-entry exclusive-lock:
        op.details = 'СПОД. Возврат процентов при досрочном расторжении Договора банковского вклада по заявлению клиента о досрочном расторжении от ' + string(op-entry.op-date,"99.99.9999").
        
end.