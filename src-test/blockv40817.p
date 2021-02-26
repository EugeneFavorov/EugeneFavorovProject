
/* отчет по блоквозвр 40817 */
/* kam */



{globals.i}
{sh-defs.i}
{ksh-defs.i NEW}

{setdest.i new}
{getdate.i}
FOR EACH blockobject WHERE
   (blockobject.end-datetime EQ ? OR blockobject.end-datetime >= datetime(end-date))
   AND blockobject.class-code EQ 'BlockAcct'
   AND blockobject.block-type EQ 'БлокВозвр'
   AND blockobject.file-name EQ 'acct'
   AND blockobject.surrogate BEGINS '40817'
   NO-LOCK:
       
   FIND FIRST acct WHERE
        acct.acct = ENTRY(1,blockobject.surrogate) NO-lOCK NO-ERROR.
    if avail acct then do:
			RUN acct-pos IN h_base (acct.acct, '', end-date, end-date, ?).
			    if abs(sh-bal) = 0 then do:
    		      FIND LAST loan-acct
	       	          WHERE loan-acct.acct = acct.acct 
		              AND loan-acct.acct-type = 'КредРасч' NO-LOCK NO-ERROR.
		              IF AVAIL loan-acct THEN do:
		                  put unformatted loan-acct.cont-code + ';' + acct.acct + ';' + string(blockobject.val[3]) skip.
		              end.
		        end.  
	end.
	
END.       
    {preview.i}
