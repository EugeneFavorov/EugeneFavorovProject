{globals.i}

{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{parsin.def}
{sh-defs.i}

DEFINE VARIABLE mInt          AS INT64      NO-UNDO.
DEFINE VARIABLE mDateAnk    AS CHARACTER  NO-UNDO.

{setdest.i &filename = "'chk_ufm.log'"}

mInt = 0.

FOR EACH op-date WHERE             
   /*op-date.op-date  eq date('26/04/2018')
    AND */   op-date.op-date  eq today        
    NO-LOCK.
    
   FOR EACH op OF op-date 
    where can-do('032,01,01БУМ,01ИНВ', op.doc-type) NO-LOCK,
    EACH op-entry OF op 
    where  can-do('40817*,40820*,423*,426*', op-entry.acct-db) NO-LOCK,   
    first acct where  op-entry.acct-db eq acct.acct 
    and acct.cust-cat eq 'Ч' NO-LOCK
   .

mDateAnk = GetTempXAttrValueEx('person',string(acct.cust-id), "ДатаОбнАнкеты",TODAY,"").

   if (GetXAttrValueEx('person',string(acct.cust-id), "Субъект","") ne "ФЛП")  /*and  ((date(mDateAnk) < (today - 365)) or (mDateAnk eq ""))*/  then
do:
     UpdateTempSignsEx("person",string(acct.cust-id),"ДатаОбнАнкеты",op-date.op-date, string(op-date.op-date),?).
    PUT UNFORMATTED GetXAttrValueEx('person',string(acct.cust-id), "Субъект","") '/' op.doc-type ' сч  ' mDateAnk ' Ф ' GetTempXAttrValueEx('person',string(acct.cust-id), "Субъект",TODAY,"") '  ' string(acct.cust-id) SKIP. 
    mInt = mInt + 1.
    
    end.
      end.
      
      FOR EACH op OF op-date 
    where op.doc-type eq '037' NO-LOCK,
    EACH op-entry OF op 
    where  can-do('40817*,40820*,423*,426*', op-entry.acct-cr) NO-LOCK,   
    first acct where  op-entry.acct-cr eq acct.acct 
    and acct.cust-cat eq 'Ч' NO-LOCK
    .

mDateAnk = GetTempXAttrValueEx('person',string(acct.cust-id), "ДатаОбнАнкеты",TODAY,"").

   if (GetXAttrValueEx('person',string(acct.cust-id), "Субъект","") ne "ФЛП")  /*and  ((date(mDateAnk) < (today - 365)) or (mDateAnk eq ""))*/  then
do:
     UpdateTempSignsEx("person",string(acct.cust-id),"ДатаОбнАнкеты",op-date.op-date, string(op-date.op-date),?).
    PUT UNFORMATTED GetXAttrValueEx('person',string(acct.cust-id), "Субъект","") op.doc-type ' сч  ' mDateAnk ' Ф ' GetTempXAttrValueEx('person',string(acct.cust-id), "Субъект",TODAY,"") SKIP. 
    mInt = mInt + 1.
    
    end.
      end.
end.

PUT UNFORMATTED ETIME " msec" SKIP.
PUT UNFORMATTED "количество  " mInt SKIP.  

{preview.i &filename = "'chk_ufm.log'"}

{intrface.del}

RETURN.