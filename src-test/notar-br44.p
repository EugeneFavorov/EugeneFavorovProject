        /*
        FORM
        tt-notif.notif
        COLUMN-LABEL "�����䨪���"
        HELP "�����䨪���"
        FORMAT "x(23)"
                    
        WITH FRAME browse1 WIDTH 78 OVERLAY TITLE "[����饭��]".
                    
        {qrdef.i 
        &buff-list = "tt-notif"
        &join-list = "each"
        }
        
        {navigate.cqr
        &WORKFILE = "/*"
        &file = tt-notif
        &bf1 = "tt-notif.notif"
        &cf1 = "tt-notif.notif"
        &return = "notar44.ret "
        &local-recid   = YES
        @nodel = YES
        }

        IF LASTKEY EQ KEYCODE("ESC") THEN pick-value = ?.
        */
        RETURN.