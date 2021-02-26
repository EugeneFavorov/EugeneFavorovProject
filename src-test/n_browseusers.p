
{globals.i}

def var isrole as char no-undo. 
def var isblocked as char no-undo.
  
def temp-table tt-user no-undo
field id-user like _user._userid
field name-user like _user._User-Name.

for each tt-user:
delete tt-user.
end.

for each _user no-lock: 
	isrole = GetXattrValueEx("_user",string(_user._userid),"gt-isrole","").
	isblocked = GetXattrValueEx("_user",string(_user._userid),"Blocked","").
	if trim(isblocked) <> 'Блокирован' then do:
		if trim(isrole) = 'Нет' then do:
			create tt-user.
			tt-user.id-user = _user._Userid.
			tt-user.name-user =  _user._User-Name.
		end.
	end.
end.
    FOR each code WHERE code.class EQ 'strahpol' AND code.parent EQ 'strahpol' NO-LOCK:
            FIND FIRST tt-user WHERE tt-user.name-user = TRIM(code.description[1]) NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-user THEN DO:
                create tt-user.
                tt-user.id-user = code.code.
                tt-user.name-user =  TRIM(code.description[1]).
            END.
    end.
  
 
	form
		tt-user.id-user
		column-label "Идентификатор"
		help "Идентификатор"
		format "x(12)"

		tt-user.name-user
		column-label "Наименование"
		help "Наименование"
		format "x(40)"
					
		with frame browse1 width 78 overlay TITLE "[Пользователи/организации]".
					
		{qrdef.i 
		&buff-list = "tt-user"
		&join-list = "each"
		}
	
		{navigate.cqr
		&WORKFILE = "/*"
		&file = tt-user
		&bf1 = "tt-user.id-user tt-user.name-user"
		&cf1 = "tt-user.id-user tt-user.name-user"
		&return = "n_browseusers.ret "
		}
	if LASTKEY EQ KEYCODE("ESC") THEN pick-value = ?.
	return.

