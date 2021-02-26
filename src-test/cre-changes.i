{enbki.def}
/*&GLOB LN-ECLS     "LN-ECLS"
&GLOB LN-LEAVE    "LN-LEAVE"
&GLOB LN-STATE    "LN-STATE"
&GLOB LN-FIRST    "LN-FIRST"
&GLOB LN-CLOSE    "LN-CLOSE"
&GLOB LN-COND     "LN-COND"
&GLOB LN-PAY      "LN-PAY"
&GLOB LN-OPER     "LN-OPER"
&GLOB LN-ACCT     "LN-ACCT"
&GLOB LN-PAR      "LN-PAR"
&GLOB LN-MOP      "LN-MOP"
&GLOB LN-COMM     "LN-COMM"
&GLOB LN-OVER     "LN-OVER"
&GLOB LN-COURT    "LN-COURT"
&GLOB LN-OFFIC    "LN-OFFIC"*/
DEF VAR ParVar AS CHAR NO-UNDO.
DEF VAR ParIntD AS CHAR NO-UNDO.
DEF VAR ParIntK AS CHAR NO-UNDO.

FUNCTION LoanChange DATE
   (BUFFER bLoan      FOR Loan,
    INPUT  iDatePrev  AS DATE ):

    DEFINE VAR vFlagSet AS LOGICAL  NO-UNDO INIT YES.
    DEFINE VAR vSurr    AS CHAR     NO-UNDO.
    DEFINE VAR vSost    AS CHAR     NO-UNDO.
    DEFINE VAR outDate  AS DATE     NO-UNDO.
    DEFINE VAR statDate AS DATE     NO-UNDO.

    IF iDatePrev EQ ? THEN
        RETURN (IF ((bLoan.open-date < TODAY) /* AND (bLoan.since > bLoan.open-date) */ ) THEN bLoan.open-date ELSE ?).

    vSost   = GetXAttrValue("loan", vSurr, "НБКИ_Состояние").
    outDate = TODAY + 1.
    DO ON ERROR UNDO, THROW:
        vSurr = bLoan.Contract + "," + bLoan.Cont-Code.

        /* Договор закрыт */
        IF      bLoan.close-date <> ?
            AND bLoan.close-date > iDatePrev
            AND bLoan.close-date < outDate
            AND vSost            <> "14"
        THEN outDate = bLoan.close-date.

        FOR EACH loan-cond
            WHERE loan-cond.contract  EQ bLoan.contract
              AND loan-cond.cont-code EQ bLoan.cont-code
              AND loan-cond.since     > iDatePrev
            NO-LOCK
            BY loan-cond.since:

            IF outDate > loan-cond.since THEN outDate = loan-cond.since.
            LEAVE.
        END.

        FOR EACH loan-var OF bLoan
            WHERE CAN-DO( ParVar, STRING(loan-var.amt-id))
              AND loan-var.since     >  iDatePrev
              AND loan-var.amt-id    <>  29
              AND loan-var.amt-id    <>  33
              AND loan-var.amt-id    <> 233
              AND loan-var.amt-id    <> 229
            NO-LOCK
            BY loan-var.since:

            IF outDate > loan-var.since THEN outDate = loan-var.since.
            LEAVE.
        END.

        FOR EACH loan-int OF bLoan
            WHERE loan-int.mdate     > iDatePrev
              AND (CAN-DO( ParIntD, STRING(loan-int.id-d))
                OR CAN-DO( ParIntK, STRING(loan-int.id-k))
                OR     loan-int.id-k = 229)
              AND NOT loan-int.avt
              AND NOT (loan-int.id-d =  33 AND loan-int.id-k = 32)
              AND NOT (loan-int.id-d =   4 AND loan-int.id-k =  5)
              AND NOT (loan-int.id-d =  32 AND loan-int.id-k =  4)
              AND NOT (loan-int.id-d =  29 AND loan-int.id-k = 30)
              AND NOT (loan-int.id-d = 229 AND loan-int.id-k = 30)
              AND NOT (loan-int.id-d = 233 AND loan-int.id-k =  8)
              AND NOT (loan-int.id-d =   5 AND loan-int.id-k =  8)
              AND NOT (loan-int.id-d =   8 AND loan-int.id-k =  5)
            NO-LOCK
            BY loan-int.mdate:

            IF outDate > loan-int.mdate THEN outDate = loan-int.mdate.
            LEAVE.
        END.

      /*FOR FIRST loan-acct WHERE
                loan-acct.contract  EQ bLoan.contract
            AND loan-acct.cont-code EQ bLoan.cont-code
            AND loan-acct.acct-type EQ "Кредит"
            AND loan-acct.since     GE iDatePrev
            AND loan-acct.since     LT bDateRep
                NO-LOCK:
         RETURN {&LN-ACCT}.
      END.*/
      /* Своевременность платежей *
      IF ListGetSummByIntVar(BUFFER bLoan,
                             end-date,
                             "ЗадолжОД,ЗадолжПр") GT 0.00 THEN DO:
         RETURN {&LN-OVER}.
      END.*/
      /*FOR FIRST comm-rate WHERE
                comm-rate.kau   EQ vSurr
            AND comm-rate.since GE iDatePrev
            AND comm-rate.since LT bDateRep
                NO-LOCK:
         RETURN {&LN-COMM}.
      END.*/

        /* Споры по договору */
        FOR EACH term-obl OF bLoan
            WHERE /* term-obl.contract  EQ bLoan.contract
              AND term-obl.cont-code EQ bLoan.cont-code
              AND */ term-obl.idnt      EQ 7
              AND term-obl.end-date  >  iDatePrev
            NO-LOCK
            BY term-obl.end-date:

            IF outDate > term-obl.end-date THEN outDate = term-obl.end-date.
            LEAVE.
        END.

        /* Официальная информация */
        FOR EACH term-obl OF bLoan
            WHERE /* term-obl.contract  EQ bLoan.contract
              AND term-obl.cont-code EQ bLoan.cont-code
              AND */ term-obl.idnt      EQ 8
              AND term-obl.end-date  >  iDatePrev
            NO-LOCK
            BY term-obl.end-date:

            IF outDate > term-obl.end-date THEN outDate = term-obl.end-date.
            LEAVE.
        END.

        /* Дата обратного выкупа */
        IF (vSost = "00")
        THEN DO:
            statDate = DATE(GetXAttrValue("loan", vSurr, "НБКИ_СостДата")) NO-ERROR.
            IF (statDate NE ?) AND (outDate > statDate) AND (statDate > iDatePrev)
            THEN outDate = statDate.
        END.
    END.

    IF outDate EQ bLoan.close-date THEN RETURN outDate.

    RETURN (IF outDate >= TODAY /* - 1 */
        /*  OR outDate >= bLoan.since */ THEN ? ELSE outDate).
END FUNCTION.

FUNCTION NeedExport DATE
   (BUFFER bLoan        FOR Loan ,
    INPUT  vDatePrev    AS DATE ):

    DEFINE BUFFER bPackObject  FOR PackObject.
    DEFINE BUFFER bSeance      FOR Seance.
    DEFINE BUFFER tLoan        FOR Loan.
    DEFINE VAR vFlagSet        AS LOGICAL INIT YES  NO-UNDO.
    DEFINE VAR vLoanUpd        AS DATE              NO-UNDO.
    DEFINE VAR vTrnsUpd        AS DATE              NO-UNDO.
    DEFINE VAR vSurr           AS CHAR              NO-UNDO.
    DEFINE VAR mOpKind AS CHAR INIT "" NO-UNDO.

    DO ON ERROR UNDO, THROW:
        vSurr = bLoan.contract + "," + bLoan.cont-code.
        IF (vDatePrev EQ ?)
        THEN vDatePrev = MINIMUM(DATE(GetXattrValue("loan", vSurr, "CREsince")), TODAY - 4).
        /* 3 дня выгружаем повторно, чтобы учесть правки задним числом */

        vLoanUpd = LoanChange(BUFFER bLoan, vDatePrev).

        FOR EACH tLoan
            WHERE tLoan.contract  EQ bLoan.contract
              AND tLoan.cont-code BEGINS bLoan.cont-code
              AND tLoan.cont-code NE bLoan.cont-code
            NO-LOCK:

            vTrnsUpd = LoanChange(BUFFER tLoan, vDatePrev).

            IF (vLoanUpd EQ ?) THEN vLoanUpd = vTrnsUpd.
            ELSE
            IF (vTrnsUpd NE ?) THEN vLoanUpd = MIN( vLoanUpd, vTrnsUpd).
        END.

/* kam временно совсем не выгружаем проданные кредиты * /
        DEF VAR tmps AS CHAR NO-UNDO.
        tmps = GetXattrValue("loan", vSurr, "НБКИ_Состояние").
        IF tmps = '14' THEN vLoanUpd = ?.
/ * end of kam */
        RETURN vLoanUpd.
    END.
END FUNCTION.

PROCEDURE PrepareVars:
    DEFINE BUFFER Code   FOR Code.
    DEFINE VAR vOne      AS INT64 NO-UNDO.
    DEFINE VAR vItm      AS INT64 NO-UNDO.

    ParVar  = "".
    ParIntD = "".
    ParIntK = "".
    FOR EACH Code
        WHERE Code.Class EQ "НБКИ_КодОпер"
        NO-LOCK:

        LPAR:
        DO vItm = 1 TO NUM-ENTRIES(Code.Misc[3]):
            ASSIGN vOne = INT64(ENTRY(vItm,Code.Misc[3])) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN NEXT LPAR.

            IF NOT CAN-DO(ParVar, STRING(vOne)) THEN DO:
                ParVar = ParVar + (IF ParVar NE "" THEN "," ELSE "") + STRING(vOne).
            END.
/*
            FOR EACH chowhe
                WHERE chowhe.amt-id EQ vOne
                NO-LOCK:

                IF      chowhe.id-d NE ?
                    AND NOT CAN-DO(ParIntD, STRING(chowhe.id-d)) THEN DO:
                    ParIntD = ParIntD + (IF ParIntD NE "" THEN "," ELSE "") + STRING(chowhe.id-d).
                END.
                IF chowhe.id-k NE ?                                   AND
                    NOT CAN-DO(ParIntK, STRING( chowhe.id-k)) THEN DO:
                    ParIntK = ParIntK + (IF ParIntK NE "" THEN "," ELSE "") + STRING(chowhe.id-k).
                END.
            END.
*/
        END.

        LINT:
        DO vItm = 1 TO NUM-ENTRIES(Code.Misc[1]):
            ASSIGN vOne = INT64(ENTRY(vItm,Code.Misc[1])) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN NEXT LINT.

            FOR FIRST chowhe
                WHERE chowhe.id-op EQ vOne
                NO-LOCK:

                IF chowhe.id-d NE ?                                   AND
                    NOT CAN-DO( ParIntD, STRING( chowhe.id-d)) THEN DO:
                    ParIntD = ParIntD + (IF ParIntD NE "" THEN "," ELSE "") + STRING(chowhe.id-d).
                END.
                IF chowhe.id-k NE ?                                   AND
                    NOT CAN-DO(ParIntK, STRING( chowhe.id-k)) THEN DO:
                    ParIntK = ParIntK + (IF ParIntK NE "" THEN "," ELSE "") + STRING(chowhe.id-k).
                END.
            END.
        END.
    END.
END PROCEDURE.
