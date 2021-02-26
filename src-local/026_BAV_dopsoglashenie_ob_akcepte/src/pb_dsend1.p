/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОСРКО.330
Что делает:     Досрочное закрытие ДС об акцепте
Место запуска:  меню Ctrl-G
Создан:         06.09.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{tmprecid.def}          /** Используем информацию из броузера */
{intrface.get tmess} 
{topkind.def}

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cNm1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNm2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOk   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErr  AS CHARACTER NO-UNDO.
DEFINE BUFFER   dsakc FOR loan.
DEFINE BUFFER   ac909 FOR loan-acct.

FOR EACH tmprecid 
    NO-LOCK,
FIRST dsakc
    WHERE (RECID(dsakc) EQ tmprecid.id)
    EXCLUSIVE-LOCK,
FIRST loan
    WHERE (loan.class-code          EQ "loanr")
      AND (loan.contract            EQ dsakc.parent-contract)
      AND (loan.cont-code           EQ dsakc.parent-cont-code)
      AND (loan.close-date          EQ ?)
    NO-LOCK,
LAST loan-acct
    WHERE (loan-acct.contract       EQ loan.contract)
      AND (loan-acct.cont-code      EQ loan.cont-code)
      AND (loan-acct.acct-type      EQ loan.contract)
      AND (loan-acct.since          LE TODAY)
    NO-LOCK
    BY loan.cust-cat
    BY loan.cust-id
    BY loan-acct.acct
    BY dsakc.open-date:

    RUN g-prompt.p ("date,char", "Дата закрытия ДС,Причина закрытия", "99.99.9999,x(200)",
                    STRING(TODAY, "99.99.9999") + ",", "Дата и причина закрытия", 65, ",","",
                    ?,?,OUTPUT cErr).
    IF (cErr EQ ?) THEN RETURN.
    end-date = DATE(ENTRY(1, cErr)).

    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, ?, OUTPUT cNm1, OUTPUT cNm2, INPUT-OUTPUT cInn).

    FIND LAST ac909
        WHERE (ac909.contract       EQ dsakc.contract)
          AND (ac909.cont-code      EQ dsakc.cont-code)
          AND (ac909.acct-type      EQ "ac90909")
          AND (ac909.since          LE end-date)
        NO-LOCK NO-ERROR.
    
    IF (AVAIL ac909)
    THEN DO:
        /* Создаем проводку на 1 руб и закрываем счет 90909 */
        {empty tOpKindParams}     /* очистить таблицу параметров */
        ASSIGN
            lOk =  TDAddParam("__acct", ac909.acct)
               AND TDAddParam("__det" , "Расторжение ДС от " + STRING(dsakc.open-date, "99.99.9999") + "г р/с "
                                      + DelFilFromAcct(loan-acct.acct) + "  " + TRIM(cNm1) + " " + TRIM(cNm2)
                                      + " с " + dsakc.comment + " на основании: " + ENTRY(2, cErr) + ". Без НДС.")
            NO-ERROR.
        RUN ex-trans.p ("_inCrd4ac", end-date, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErr).
        IF NOT lOk
        THEN RUN Fill-SysMes IN h_tmess ("", "", "-1", "Счет 90909 не закрыт").
    END.

    dsakc.close-date = end-date.
    RUN Fill-SysMes IN h_tmess ("", "", "1", "Доп.соглашение закрыто").
END.
{intrface.del}
