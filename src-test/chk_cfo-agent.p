DEFINE INPUT PARAMETER iSurr  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iCode  AS CHARACTER NO-UNDO.


IF NUM-ENTRIES(iValue,",") < 2
THEN RETURN ERROR "�������� �������� �������!".

IF ENTRY(1,iValue,",") EQ "�"
THEN DO:
    FIND FIRST person where person.person-id EQ DEC(ENTRY(2,iValue,",")) NO-ERROR.
    IF NOT AVAIL person then RETURN ERROR "��� ������ � � ����� ����� " + ENTRY(2,iValue,",").
    ELSE RETURN.
END.
ELSE IF ENTRY(1,iValue,",") EQ "�"
    THEN DO:
        FIND FIRST cust-corp WHERE cust-corp.cust-id EQ DEC(ENTRY(2,iValue,",")) NO-ERROR.
        IF NOT AVAIL cust-corp THEN RETURN ERROR "��� ������ � � ����� ����� " + ENTRY(2,iValue,",").
        ELSE RETURN.
    END.
    ELSE RETURN ERROR "�� ������� ⨯ ������ " + ENTRY(1,iValue,",").

         