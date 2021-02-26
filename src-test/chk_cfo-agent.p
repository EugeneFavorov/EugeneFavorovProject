DEFINE INPUT PARAMETER iSurr  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iCode  AS CHARACTER NO-UNDO.


IF NUM-ENTRIES(iValue,",") < 2
THEN RETURN ERROR "РЕКВИЗИТ ЗАПОЛНЕН НЕВЕРНО!".

IF ENTRY(1,iValue,",") EQ "Ч"
THEN DO:
    FIND FIRST person where person.person-id EQ DEC(ENTRY(2,iValue,",")) NO-ERROR.
    IF NOT AVAIL person then RETURN ERROR "Нет клиента Ч с данным кодом " + ENTRY(2,iValue,",").
    ELSE RETURN.
END.
ELSE IF ENTRY(1,iValue,",") EQ "Ю"
    THEN DO:
        FIND FIRST cust-corp WHERE cust-corp.cust-id EQ DEC(ENTRY(2,iValue,",")) NO-ERROR.
        IF NOT AVAIL cust-corp THEN RETURN ERROR "Нет клиента Ю с данным кодом " + ENTRY(2,iValue,",").
        ELSE RETURN.
    END.
    ELSE RETURN ERROR "Не известный тип клиента " + ENTRY(1,iValue,",").

         