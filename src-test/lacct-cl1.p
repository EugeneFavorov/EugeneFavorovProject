/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: X:\TMP\LACCT-CL.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 20.12.2011 17:21 gva     
     Modified: 21.12.2011 09:17 gva      
     Modified: 21.12.2011 09:59 gva      
     Modified: 
*/

{globals.i}
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

DEFINE INPUT  PARAMETER ipAcctRole AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i AS INTEGER    NO-UNDO.
{tmprecid.def}

{getdate.i}

FOR EACH tmprecid:
    FIND FIRST loan WHERE RECID(loan) EQ tmprecid.id
         NO-LOCK NO-ERROR.
/*message loan.cont-code view-as alert-box.*/
    IF AVAILABLE(loan) THEN
    DO i = 1 TO NUM-ENTRIES(ipAcctRole):
        FIND LAST loan-acct 
            WHERE LOAN-acct.cont-code eq loan.cont-code
	    AND loan-acct.acct-type EQ ENTRY(i, ipAcctRole)
            AND loan-acct.since LE end-date
            NO-LOCK NO-ERROR.
        
/*        message  loan-acct.cont-code view-as alert-box.*/
        
            IF AVAILABLE(loan-acct) THEN
            DO:
                FIND FIRST acct 
                    WHERE acct.acct EQ loan-acct.acct 
                    AND acct.close-date EQ ?
                    NO-LOCK NO-ERROR.
                                    
                IF AVAILABLE(acct) AND GetPermission (acct.class-code, acct.acct + "," + acct.currency, "w")
                
                 
                    THEN 
                DO:
/*                message acct.acct view-as alert-box.*/
                
                    RUN acct-cls.p (RECID(acct)) NO-ERROR.
                END.
            END.
    END.
END.

/*   AND GetPermission ({&edtfile}.class-code, {&edtfile}.acct + "," + {&edtfile}.currency, "w") */
