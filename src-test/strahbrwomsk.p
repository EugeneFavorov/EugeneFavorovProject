/*
               Банковская интегрированная система БИСквит
    Copyright: 
     Filename: strahbrwomsk.p
      Comment: Ищем клиента в базе BANK
   Parameters: input - cid в омской АБС,  output - fullname|shortname|acct|telefax|inn|address|birthday|vidPassp|sernomPassp|vydanPassp|datePassp
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

DEFINE INPUT PARAMETER iCustID AS INT64.
DEFINE OUTPUT PARAMETER iClient AS CHAR.
DEFINE VAR did AS INT64 NO-UNDO.
DEFINE VAR handle1 AS INT64.
DEFINE VAR quer AS CHAR NO-UNDO.
DEFINE VAR tmpInt2 AS INT64 NO-UNDO.
DEFINE VAR tmpInt AS INT64 NO-UNDO.
DEFINE VAR tmpProctext AS CHAR NO-UNDO.

iClient = "NOTFOUND".
FIND FIRST bank.clients WHERE bank.clients.cid = iCustID NO-LOCK NO-ERROR.
IF AVAIL bank.clients THEN DO:
    iClient = bank.clients.sname + '|' + bank.clients.fname + '|'.
    did = bank.clients.did.
    FIND FIRST bank.accounts WHERE bank.accounts.cid = iCustID AND (bank.accounts.acctype = 1 OR bank.accounts.acctype = 101)
        AND bank.accounts.clostime = ? NO-LOCK NO-ERROR.
    IF AVAIL bank.accounts THEN iClient = iClient + TRIM(bank.accounts.account).
    iClient = iClient + '|'.
    quer = "Select BANKER.GetClientAttrib(" + string(iCustID)+ ",'PHONE') from dual".
    ERROR-STATUS:ERROR = NO.
    RUN STORED-PROC Bank.send-sql-statement handle1 = PROC-HANDLE NO-ERROR
    (quer).
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Ошибка в вызове BANKER.GetClientAttrib" VIEW-AS ALERT-BOX.
    END. 
    ELSE DO:
        FOR EACH Bank.proc-text-buffer WHERE PROC-HANDLE = handle1:
            iClient = iClient + trim(proc-text).
            LEAVE.
        END.
        CLOSE STORED-PROCEDURE Bank.send-sql-statement WHERE PROC-HANDLE = handle1.
    END.
    iClient = iClient + '|'.
    quer = "Select BANKER.GetClientAttrib(" + string(iCustID)+ ",'INN') from dual".
    ERROR-STATUS:ERROR = NO.
    RUN STORED-PROC Bank.send-sql-statement handle1 = PROC-HANDLE NO-ERROR
    (quer).
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Ошибка в вызове BANKER.GetClientAttrib" VIEW-AS ALERT-BOX.
    END. 
    ELSE DO:
        FOR EACH Bank.proc-text-buffer WHERE PROC-HANDLE = handle1:
            iClient = iClient + trim(proc-text).
            LEAVE.
        END.
        CLOSE STORED-PROCEDURE Bank.send-sql-statement WHERE PROC-HANDLE = handle1.
    END.
    iClient = iClient + '|'.
    
    quer = "Select BANKER.GetClientAttrib(" + string(iCustID)+ ",'ADDRESS_JUR') from dual".
    ERROR-STATUS:ERROR = NO.
    RUN STORED-PROC Bank.send-sql-statement handle1 = PROC-HANDLE NO-ERROR
    (quer).
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Ошибка в вызове BANKER.GetClientAttrib" VIEW-AS ALERT-BOX.
    END. 
    ELSE DO:
        FOR EACH Bank.proc-text-buffer WHERE PROC-HANDLE = handle1:
            iClient = iClient + trim(proc-text).
            LEAVE.
        END.
        CLOSE STORED-PROCEDURE Bank.send-sql-statement WHERE PROC-HANDLE = handle1.
    END.
    iClient = iClient + '|'.
    
    quer = "Select BANKER.GetClientAttrib(" + string(iCustID)+ ",'DATE_BIRTH') from dual".
    ERROR-STATUS:ERROR = NO.
    RUN STORED-PROC Bank.send-sql-statement handle1 = PROC-HANDLE NO-ERROR
    (quer).
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Ошибка в вызове BANKER.GetClientAttrib" VIEW-AS ALERT-BOX.
    END. 
    ELSE DO:
        FOR EACH Bank.proc-text-buffer WHERE PROC-HANDLE = handle1:
            iClient = iClient + trim(proc-text).
            LEAVE.
        END.
        CLOSE STORED-PROCEDURE Bank.send-sql-statement WHERE PROC-HANDLE = handle1.
    END.
    iClient = iClient + '|'.
    
    quer = "Select BANKER.GetClientAttrib(" + string(iCustID)+ ",'DOCUMENT') from dual".
    ERROR-STATUS:ERROR = NO.
    RUN STORED-PROC Bank.send-sql-statement handle1 = PROC-HANDLE NO-ERROR
    (quer).
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Ошибка в вызове BANKER.GetClientAttrib" VIEW-AS ALERT-BOX.
    END. 
    ELSE DO:
        FOR EACH Bank.proc-text-buffer WHERE PROC-HANDLE = handle1:
            tmpProctext = TRIM(proc-text).
            IF tmpProctext = "№  выдан" THEN DO:
                iClient = iClient + '|||'.
            END.
            ELSE DO:
                tmpInt = INDEX(tmpProctext, "серия").
                IF tmpInt > 0 THEN DO:
                    IF tmpInt = 1 THEN iClient = iClient + '|'. /* vidPassp */
                        ELSE iClient = iClient + TRIM(SUBSTRING(tmpProctext,1,tmpInt - 1)) + '|'. /* vidPassp */
                    tmpProctext = SUBSTRING(tmpProctext,tmpInt).
                END.
                ELSE DO:
                    tmpInt = INDEX(tmpProctext, "№").
                    IF tmpInt > 0 THEN DO:
                        IF tmpInt = 1 THEN iClient = iClient + '|'. /* vidPassp */
                            ELSE iClient = iClient + TRIM(SUBSTRING(tmpProctext,1,tmpInt - 1)) + '|'. /* vidPassp */
                        tmpProctext = SUBSTRING(tmpProctext,tmpInt).
                    END. 
                    ELSE iClient = iClient + '|'. /* vidPassp */                   
                END.
                tmpInt = INDEX (tmpProctext, "выдан").
                IF tmpInt > 0 THEN DO:
                    IF tmpInt = 1 THEN iClient = iClient + '|'. /* sernomPassp */
                        ELSE iClient = iClient + TRIM(SUBSTRING(tmpProctext,1,tmpInt - 1)) + '|'. /* sernomPassp */
                    tmpProctext = SUBSTRING(tmpProctext,tmpInt).
                END.
                ELSE DO: 
                    iClient = iClient + '|' + tmpProctext. /* ололо */
                    tmpProctext = "".
                END.
                tmpInt = R-INDEX (tmpProctext, " ").
                IF tmpInt > 0 THEN DO:
                    iClient = iClient + TRIM(SUBSTRING(tmpProctext,1,tmpInt - 1)) + '|'. /* vydanPassp */
                    iClient = iClient + TRIM(SUBSTRING(tmpProctext,tmpInt)). /* datePassp */
                END.
                ELSE iClient = iClient + tmpProctext.
            END.
            LEAVE.
        END.
        CLOSE STORED-PROCEDURE Bank.send-sql-statement WHERE PROC-HANDLE = handle1.
    END.
END. 
    
    
    
    
















