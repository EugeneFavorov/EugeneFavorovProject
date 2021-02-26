{globals.i}

{tmprecid.def}
{setdest.i}

DEFINE INPUT PARAMETER iUser AS CHARACTER NO-UNDO.

iUser = TRIM(iUser).
def var i as int init 1.
FOR EACH tmprecid no-lock,
    EACH loan WHERE
    RECID(loan) EQ tmprecid.id
:
i = i + 1.
	loan.user-id = iUser.
put unformatted string(i) skip.
END.
{preview.i}
