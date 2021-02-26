
def input param tt-notif as temp-table no-undo.
        FORM
        tt-notif.notif FORMAT "x(23)"
        COLUMN-LABEL "Идентификатор"
        HELP "Идентификатор"
        
                    
        WITH FRAME browse1 TITLE "[Сообщения]".
                    
        {qrdef.i 
        &buff-list = "tt-notif"
        &join-list = "each"
        }
        
        {navigate.cqr
        &WORKFILE = "/*"
        &nodel    = "/*"
        &file = tt-notif
        &bf1 = "tt-notif.notif "
        &tmprecid      = YES   
        &need-tmprecid = YES
        &local-recid   = YES
        &local-rest    = YES
        &local-keep    = YES
        &return = "notar44.ret "
        }
        
        


        IF LASTKEY EQ KEYCODE("ESC") THEN pick-value = ?.
        
