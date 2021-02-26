/* процедура переноса КАУ по дебету 904 счета */
def input param rid as recid.
def input param in-dbcr as logical. /* no - cr  yes - db */  
def var ret-value as char no-undo.
def var choice as logical initial yes no-undo.
{globals.i} 
{kautools.lib}
def var h_templ as handle no-undo .
def var kau-rid as recid no-undo .

DEFINE VARIABLE mCurrentKau AS CHARACTER NO-UNDO.

h_templ = session:last-procedure .
do while valid-handle(h_templ):
 if substring(h_templ:private-data,1,1) eq '$' and
  h_templ:file-name eq 'l-type.p' then leave .
 h_templ = h_templ:prev-sibling .
end.
if  valid-handle(h_templ) then kau-rid = INT64(substring(h_templ:private-data,2)) .
else kau-rid = 0 .

if in-dbcr then do:
   {crdex.i &dbcr = db }
end.
else do:
   {crdex.i &dbcr = cr }
end.
