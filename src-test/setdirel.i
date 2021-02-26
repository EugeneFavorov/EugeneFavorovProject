DEFINE VARIABLE mDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDirElec AS CHARACTER NO-UNDO.
mDir = "/home2/bis/quit41d/imp-exp/elsafe/".
mDirElec = STRING(YEAR(end-date), "9999") + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
IF SEARCH(mDir + mDirElec + "/readme") EQ ? THEN
DO:
	OS-COMMAND VALUE("mkdir " + mDir + mDirElec).
	OS-COMMAND VALUE("chmod -R 777 " + mDir + mDirElec).
	OUTPUT TO VALUE(mDir + mDirElec + "/readme").
	PUT UNFORMATTED
	TODAY
	SKIP.
	OUTPUT CLOSE.
END.




