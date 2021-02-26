   {globals.i}
   {tmprecid.def}
   {wordwrap.def}
   {intrface.get xclass}
   {intrface.get cust}

   def var select_item  as logical  no-undo init no.
   message "Выгрузить данные для выделенных документов?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           UPDATE select_item .
   if not select_item or select_item = ? then return.
   DEF  STREAM err.
   def var s1     as char    no-undo.
   def var s2     as char    no-undo.
   def var s3     as char    no-undo.
   def var s4     as char    no-undo.
   output stream err to "dmo_1.csv" .
   for each tmprecid,
       FIRST op WHERE recid(op) EQ tmprecid.id        
       no-lock :
       s1 = GetXattrValueEx("op",STRING(Op.op),"document-id","").
       s2 = GetXattrValueEx("op",STRING(Op.op),"ФИО","").
       s3 = GetXattrValueEx("op",STRING(Op.op),"Докум","").

       for each op-entry where op.op EQ op-entry.op.
          s4 = string(op.op-date) + " ; " + op.doc-type + " ; " + string(op-entry.amt-cur) + " ; " + string(op-entry.amt-rub) + " ; " + op.doc-num + " ; " +
          op-entry.acct-db + " ; " + op-entry.acct-cr + " ; " + op.details + " ; " + s1 + " ; " + s2 + " ; " + s3 + " ; " .
          s4 = replace(s4,chr(10),"").
          s4 = replace(s4,chr(12),"").
          s4 = CODEPAGE-CONVERT(s4,"1251",SESSION:CHARSET).            
          PUT stream err UNFORMATTED s4  SKIP.

      end.
   end.
   message "Данные выгружены" VIEW-AS ALERT-BOX.

   output close.
