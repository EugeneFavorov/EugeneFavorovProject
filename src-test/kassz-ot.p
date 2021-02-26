/*
               ОАО "Плюс Банк"
    Copyright: 
     Filename: kassz-ot.p
      Comment: Отчёт по кассовым заявкам за период
   Parameters: 
         Uses:
      Used by:
      Created: 07/04/2014 kau
     Modified: 
*/

DEF INPUT PARAM iOpkind AS CHAR NO-UNDO.
DEF VAR vNameCli    AS CHAR NO-UNDO.
DEF VAR vBranch		AS CHAR NO-UNDO.
DEF VAR vBranUs		AS CHAR NO-UNDO.
DEF VAR vBranUsID	AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-kassop
  FIELD zdate       AS DATE
  FIELD branch	    AS CHAR
  FIELD name-corp   AS CHAR
  FIELD acct        AS CHAR
  FIELD summ        AS DEC
  FIELD ks          AS CHAR
  FIELD user-name   AS CHAR  
  FIELD branchuser  AS CHAR
  INDEX zdate name-corp acct
.

{globals.i}
{getdates.i}
{client.i}


FOR EACH op WHERE op.op-date >= beg-date
                AND op.op-date <= end-date
                AND op.op-kind EQ iOpKind
NO-LOCK:
    FOR EACH op-entry WHERE op-entry.op EQ op.op NO-LOCK:
        FIND FIRST acct where acct.acct EQ op-entry.acct-db NO-LOCK NO-ERROR.
        FIND FIRST _user WHERE _user._userid EQ op.user-id NO-LOCK NO-ERROR.
        RUN RE_CLIENT (acct.cust-cat, acct.cust-id, INPUT-OUTPUT  vnamecli).

	vBranch = SUBSTRING(op-entry.acct-cr,10,4).
	FIND FIRST branch where branch.branch-id EQ vBranch NO-LOCK NO-ERROR.
	IF AVAIL branch then vBranch = branch.short-name.

	vBranUs = getxattrvalue("_user", _user._userid, "Отделение").
	FIND FIRST branch where branch.branch-id EQ vBranUs NO-LOCK NO-ERROR.
	IF AVAIL branch then vBranUs = branch.short-name.

        CREATE tt-kassop.
        ASSIGN tt-kassop.zdate = op.op-date
	    tt-kassop.branch = vBranch
            tt-kassop.name-corp = vNameCli
            tt-kassop.acct = SUBSTRING(op-entry.acct-db,1,20)
            tt-kassop.summ = op-entry.amt-rub
            tt-kassop.ks = STRING(op-entry.symbol)
            tt-kassop.user-name = _user._user-name
	    tt-kassop.branchuser = vBranUs
        .
    END.
END.
{kassz-ot.i}    
    
    
    
