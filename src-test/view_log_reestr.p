/*
kam
*/

{globals.i}
end-date = today - 1.
{getdate.i 
&noinit = "/*"
&DateLabel = "��� ॥���"
}

def var fname as char no-undo.


fname = "/data/home2/bis/quit41d/log/autoreestr/alex" +
		STRING(YEAR(end-date),"9999") + STRING(MONTH(end-date),"99") + STRING(DAY(end-date),"99") + ".log".

if SEARCH(fname) = ? THEN DO:
	message "�� ������ ��� ࠡ��� ��楤��� �� ���� " + string(end-date) view-as alert-box.
END.
else do:
	{preview.i &filename=fname}.
end.
