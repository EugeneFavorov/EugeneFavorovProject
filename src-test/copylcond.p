{globals.i}
/*
{setdest.i}
*/
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get pbase}
{intrface.get xclass}
{tmprecid.def}

{getdate.i}

DEF BUFFER lloan-cond FOR loan-cond.
DEF BUFFER lsigns FOR signs.
DEF BUFFER lcomm-rate FOR comm-rate.

DEF VAR ost455 AS DECIMAL NO-UNDO.
DEF VAR DATE_S AS DATE NO-UNDO.
DATE_S = end-date - 1.
/* DATE("24/12/2017"). */

FOR EACH tmprecid, FIRST loan WHERE RECID(loan) EQ tmprecid.id :
    
    loan.end-date = DATE_S + 1.
    ost455 = 0.
    FIND LAST loan-acct OF loan WHERE 
        loan-acct.since <= DATE_S 
        AND loan-acct.acct-type = 'ΰ¥¤¨β'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
        RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, DATE_S, DATE_S, ?).
        IF loan-acct.currency = '' THEN
            ost455 = abs(sh-bal).
        ELSE ost455 = abs(sh-val).
    END.

    FOR EACH loan-cond WHERE loan-cond.cont-code = loan.cont-code
             AND loan-cond.contract = loan.contract
             NO-LOCK BY loan-cond.since DESC:
        CREATE lloan-cond.
        ASSIGN
            lloan-cond.since =          DATE_S
            lloan-cond.disch-type =     loan-cond.disch-type
            lloan-cond.delay =          loan-cond.delay
            lloan-cond.int-date =       loan-cond.int-date
            lloan-cond.contract =       loan-cond.contract
            lloan-cond.int-period =     loan-cond.int-period
            lloan-cond.delay1 =         loan-cond.delay1
            lloan-cond.cont-code =      loan-cond.cont-code
            lloan-cond.cred-date =      loan-cond.cred-date
            lloan-cond.cred-period =    loan-cond.cred-period
            lloan-cond.int-month =      loan-cond.int-month
            lloan-cond.rate =           loan-cond.rate
            lloan-cond.class-code =     loan-cond.class-code
            lloan-cond.cred-month =     loan-cond.cred-month
            .
            VALIDATE lloan-cond NO-ERROR.
          /*
            IF ERROR-STATUS:ERROR THEN DO:
                DEF VAR i AS INT64 NO-UNDO.
                DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                    PUT UNFORMATTED ERROR-STATUS:GET-MESSAGE(i) SKIP.
                END.
            END. 
          */  
        FOR EACH signs WHERE
            signs.file-name = 'loan-cond' AND
            signs.code <> 'PayType' AND
            signs.code <> 'PaySum' AND
            /*  signs.code <> 'CondEndDate' AND */
            signs.surrogate = 'ΰ¥¤¨β,' + loan-cond.cont-code + ',' + STRING(loan-cond.since,"99/99/99") NO-LOCK:
            CREATE lsigns.
            ASSIGN 
                lsigns.code = signs.code
                lsigns.code-value = signs.code-value
                lsigns.surrogate = 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99")
                lsigns.file-name = signs.file-name
                lsigns.xattr-value = signs.xattr-value
                lsigns.dec-value = signs.dec-value
               /* lsigns.date-value = signs.date-value */
                .
                IF signs.code = 'end-date' THEN DO:
                    lsigns.code-value = STRING((DATE_S + 1),"99/99/9999").
                  /*  lsigns.date-value = DATE_S + 1. */
                END.
                ELSE IF signs.code = 'CondEndDate' THEN DO:
                    lsigns.xattr-value = STRING((DATE_S + 1),"99/99/9999").
                  /*  lsigns.date-value = DATE_S + 1. */
                END.
                ELSE IF signs.code = '€­­γ¨β« β' THEN DO:
                    lsigns.code-value = TRIM(STRING(ost455,"->>>,>>>,>>>,>>9.99")).
                    lsigns.dec-value = ost455.
                END.
                ELSE IF signs.code = '®«‹μ£β¥ΰ' THEN DO:
                    lsigns.code-value = "0".
                    lsigns.dec-value = 0.
                END.                
                ELSE IF signs.code = '®«‹μ£β¥ΰΰζ' THEN DO:
                    lsigns.code-value = "0".
                    lsigns.dec-value = 0.
                END.                
                VALIDATE lsigns.
        END.
        
        CREATE lsigns.
            ASSIGN 
                lsigns.code = 'PayType'
                lsigns.code-value = ''
                lsigns.surrogate = 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99")
                lsigns.file-name = 'loan-cond'
                lsigns.xattr-value = 'αβ β®ª'
                .
        VALIDATE lsigns.
        
        CREATE lsigns.
            ASSIGN 
                lsigns.code = 'PaySum'
                lsigns.code-value = TRIM(STRING(ost455,"->>>,>>>,>>>,>>9.99"))
                lsigns.dec-value = ost455
                lsigns.surrogate = 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99")
                lsigns.file-name = 'loan-cond'
                .
        VALIDATE lsigns.
        

    DEF VAR vIntOffs AS CHAR INIT "->" NO-UNDO.
    UpdateSigns(lloan-cond.Class-Code, 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99"), "int-offset", vIntOffs, ?).
    UpdateSigns(lloan-cond.Class-Code, 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99"), "cred-mode",  GetXAttrEx(lloan-cond.class-code,"cred-mode","initial"), ?).

    DEF VAR vCredOffs AS CHAR INIT "->" NO-UNDO.
    UpdateSigns(lloan-cond.Class-Code, 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99"), "cred-offset",      vCredOffs, ?).
    UpdateSigns(lloan-cond.Class-Code, 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99"), "delay-offset",     "->", ?).
    UpdateSigns(lloan-cond.Class-Code, 'ΰ¥¤¨β,' + lloan-cond.cont-code + ',' + STRING(DATE_S,"99/99/99"), "delay-offset-int", "->", ?).


        FOR EACH comm-rate WHERE
            comm-rate.kau = 'ΰ¥¤¨β,' + loan-cond.cont-code AND
            comm-rate.since = loan-cond.since NO-LOCK: 
            CREATE lcomm-rate.
            ASSIGN
                lcomm-rate.commission = comm-rate.commission
                lcomm-rate.currency   = comm-rate.currency
                lcomm-rate.min-value  = comm-rate.min-value
                lcomm-rate.acct       = comm-rate.acct
                lcomm-rate.since      = DATE_S
                lcomm-rate.rate-comm  = comm-rate.rate-comm
                lcomm-rate.rate-fixed = comm-rate.rate-fixed
                lcomm-rate.period     = comm-rate.period
                lcomm-rate.kau        = comm-rate.kau
                lcomm-rate.filial-id  = comm-rate.filial-id
                .
                VALIDATE lcomm-rate.
        END.
        
            RUN DeleteOldDataProtocol IN h_base ("‹€’…†  –…’€ ‘„‚ƒ").
    RUN DeleteOldDataProtocol IN h_base ("‡€’…‹‘’‚€  ‚‡‚€’“ ‘„‚ƒ").
    RUN DeleteOldDataProtocol IN h_base ("‡.  ‚‡‚€’“ ‘„‚ƒ .‘€").
    RUN DeleteOldDataProtocol IN h_base ("‹€’.  –. ‘„‚ƒ .‘€").
    RUN DeleteOldDataProtocol IN h_base ("… ‚›‚„’ ƒ€” € €").


        RUN SetSysConf IN h_base ("‡€’…‹‘’‚€  ‚‡‚€’“ ‘„‚ƒ","1").
        RUN SetSysConf IN h_base ("‹€’…†  –…’€ ‘„‚ƒ","1").
        RUN SetSysConf IN h_base ("‡.  ‚‡‚€’“ ‘„‚ƒ .‘€", "1").
        RUN SetSysConf IN h_base ("‹€’.  –. ‘„‚ƒ .‘€", "1").
        
        RUN SetSysConf IN h_base ("… ‚›‚„’ ƒ€” € €", "„€").
        RUN mm-to.p(RECID(loan),
                  RECID(lloan-cond),
                   ost455,
                   2,
                   YES,
                   YES,
                   YES,
                   YES,
                   ? /*mRisk*/,
                   1) NO-ERROR.

    RUN DeleteOldDataProtocol IN h_base ("‹€’…†  –…’€ ‘„‚ƒ").
    RUN DeleteOldDataProtocol IN h_base ("‡€’…‹‘’‚€  ‚‡‚€’“ ‘„‚ƒ").
    RUN DeleteOldDataProtocol IN h_base ("‡.  ‚‡‚€’“ ‘„‚ƒ .‘€").
    RUN DeleteOldDataProtocol IN h_base ("‹€’.  –. ‘„‚ƒ .‘€").
    RUN DeleteOldDataProtocol IN h_base ("… ‚›‚„’ ƒ€” € €").
                   
        LEAVE. 
    END.

    /*
        PUT UNFORMATTED loan.doc-ref + '    '  SKIP.
        */
END.
    /*
{preview.i}
*/

