

for each op-entry where
    substring(op-entry.acct-db,1,5) = '47423'
    and substring(op-entry.acct-cr,1,5) = '30305'
    and year(op-entry.op-date) = 2015
    and op-entry.op-status begins "�"
    and op-entry.user-id <> 'SYNC'
    no-lock, first op of op-entry exclusive-lock:
        op.details = '����. ������ ��業⮢ �� ����筮� ���থ��� ������� ������᪮�� ������ �� ������ ������ � ����筮� ���থ��� �� ' + string(op-entry.op-date,"99.99.9999").
        
end.