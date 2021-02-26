&GLOB OK "0|Ok"


PROCEDURE RE_CLIENT.
    
    define input param in_type   as char no-undo. /* Тип клиента */
    define input param in_client as INT64  no-undo. /* Код клиента */
    define input-output param client_name as char no-undo.    /* Наименование клиента */    

    define buffer buf-cust-corp for cust-corp.
    define buffer buf-person    for person.
    define buffer buf-banks     for banks.

    case in_type:
        when "Ю" then do:
            find first buf-cust-corp where
                buf-cust-corp.cust-id eq in_client
            no-lock no-error.
            client_name = if avail buf-cust-corp
                            then trim(buf-cust-corp.name-corp)
                            else "?".
        end.
        when "Ч" then do:
            find first buf-person where
                buf-person.person-id eq in_client
            no-lock no-error.
            client_name = if avail buf-person 
                            then trim(buf-person.name-last) + " " + trim(buf-person.first-names)
                            else "?".
        end.
        when "Б" then do:
            find first buf-banks where
                buf-banks.bank-id eq in_client
            no-lock no-error.
            client_name = if avail buf-banks 
                            then trim(buf-banks.short-name)
                            else "?".
        end.
    end case.

    return {&OK}.
             
END PROCEDURE.

PROCEDURE RE_CLIENT_FULL.

    define input param in_type   as char no-undo. /* Тип клиента */
    define input param in_client as INT64  no-undo. /* Код клиента */
    define input-output param client_name as char no-undo.    /* Наименование клиента */    

    define buffer buf-cust-corp for cust-corp.
    define buffer buf-person    for person.
    define buffer buf-banks     for banks.
    DEFINE VAR mTmpStr AS CHARACTER NO-UNDO.

    case in_type:
        when "Ю" then do:

            find first buf-cust-corp where
                buf-cust-corp.cust-id eq in_client
            no-lock no-error.
                   
               client_name = if avail buf-cust-corp
                                 then trim(buf-cust-corp.cust-stat + " " + buf-cust-corp.name-corp)
                                 else "?".   
        end.

        
        when "Б" then do:

            find first buf-banks where
                buf-banks.bank-id eq in_client
            no-lock no-error.

            client_name = if avail buf-banks 
                              then trim(buf-banks.name)
                              else "?".
        end.
                
        when "Ч" then do:
            find first buf-person where
                buf-person.person-id eq in_client
            no-lock no-error.
            IF AVAIL buf-person THEN DO:
               mTmpStr = trim(buf-person.first-names).
               mTmpStr = SUBSTRING(ENTRY(1, mTmpStr, " "),1,1) + "." 
                  + IF NUM-ENTRIES(mTmpStr, " ") GE 2 THEN SUBSTRING(ENTRY(2, mTmpStr, " "),1,1) + "."
                     ELSE "".
               client_name = mTmpStr + trim(buf-person.name-last).
            END.
            ELSE client_name = "?".
        end.
    end case.

    return {&OK}.
             
END PROCEDURE.

PROCEDURE RE_CLIENT_LARGE.
    define input param in_type   as char no-undo. /* Тип клиента */
    define input param in_client as INT64  no-undo. /* Код клиента */
    define input-output param client_name as char no-undo.    /* Наименование клиента */    

    define buffer buf-cust-corp for cust-corp.
    define buffer buf-person    for person.
    define buffer buf-banks     for banks.
    define buffer buf-code      for CODE.

    case in_type:
        when "Ю" then do:
            find first buf-cust-corp where
                buf-cust-corp.cust-id eq in_client
            no-lock no-error.
            IF AVAIL buf-cust-corp THEN DO:
               FIND FIRST buf-code WHERE buf-code.val = buf-cust-corp.cust-stat
                  NO-LOCK NO-ERROR.
            END.
            client_name = if avail buf-cust-corp
               then trim(buf-cust-corp.name-corp)
               else "?".   
            client_name = if avail buf-code
               then trim(buf-code.NAME) + " " + client_name
               ELSE client_name.
        end.
        
        when "Б" then do:
           find first buf-banks where
               buf-banks.bank-id eq in_client
           no-lock no-error.

           client_name = if avail buf-banks 
                             then trim(buf-banks.name)
                             else "?".
        end.
                
        when "Ч" then do:
           find first buf-person where
               buf-person.person-id eq in_client
           no-lock no-error.
           client_name = if avail buf-person 
                           then trim(buf-person.name-last) + " " + trim(buf-person.first-names)
                           else "?".
        end.
    end case.
    return {&OK}.
END PROCEDURE.

PROCEDURE FIO_CLIENT.
    
    define input param in_type   as char no-undo.              /* Ф-фамилия И-имя О-отчество */
    define input param in_client as INT64  no-undo.            /* Код клиента */
    define input-output param client_name as char no-undo.     /* Наименование клиента */    
    define variable i as int no-undo.

    define buffer buf-cust-corp for cust-corp.
    define buffer buf-person    for person.
    define buffer buf-banks     for banks.

    case in_type:
        when "Ф" then do:
            find first buf-person where
                buf-person.person-id eq in_client
            no-lock no-error.
            client_name = if avail buf-person 
                            then trim(buf-person.name-last)
                            else "?".
        end.
        when "И" then do:
            find first buf-person where
                buf-person.person-id eq in_client
            no-lock no-error.
            client_name = if avail buf-person 
                            then entry(1, trim(buf-person.first-names), " ")
                            else "?".
        end.
        when "О" then do:
            find first buf-person where
                buf-person.person-id eq in_client
            no-lock no-error.
            if avail buf-person 
                then 
                	do i = 2 to NUM-ENTRIES(buf-person.first-names, " "):
                	client_name = client_name + " " + entry(i, buf-person.first-names, " ").
                	end.
                else client_name = "?".
        end.
    end case.

    return {&OK}.
             
END PROCEDURE.