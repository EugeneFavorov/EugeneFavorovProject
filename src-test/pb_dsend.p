/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОСРКО.330
Что делает:     Автоматическое закрытие ДС об акцепте
Параметры:      Список п/я для рассылки протокола
Место запуска:  Планировщик
Создан:         06.09.2016 Борисов А.В.
*/

DEFINE INPUT  PARAMETER iMail   AS CHARACTER NO-UNDO.   /* Список п/я для рассылки протокола */

RUN pb_tstwork.p.
IF NOT (RETURN-VALUE BEGINS "bis-work") THEN RETURN.

{globals.i}             /** Глобальные определения */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{intrface.get refer}
{intrface.get op}
{intrface.get date}

end-date = TODAY.
IF HolidayRu(end-date) THEN RETURN.

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE dEnd  AS DATE      NO-UNDO.
DEFINE VARIABLE cfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNm1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNm2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTmp  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cComm AS CHARACTER NO-UNDO.
DEFINE VARIABLE I     AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE lOk   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErr  AS CHARACTER NO-UNDO.
DEFINE BUFFER   dsakc FOR loan.
DEFINE BUFFER   ac909 FOR loan-acct.
DEFINE STREAM   sXL.

/* **************************************************************************** */
dEnd = end-date.
DO WHILE(HolidayRu(dEnd + 1)):
    dEnd = dEnd + 1.
END.

cComm = "ДС закрыто".
FOR EACH dsakc
    WHERE (dsakc.contract           EQ "КассСогАкц")
      AND (dsakc.parent-contract    EQ 'Расчет')
      AND (dsakc.close-date         EQ ?)
      AND (dsakc.end-date           NE ?)
      AND (dsakc.end-date           LE dEnd)
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
      AND (loan-acct.since          LE dEnd)
    NO-LOCK
    BY dsakc.filial-id
    BY loan.cust-cat
    BY loan.cust-id
    BY loan-acct.acct
    BY dsakc.open-date:

    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, ?, OUTPUT cNm1, OUTPUT cNm2, INPUT-OUTPUT cTmp).

    FIND LAST ac909
        WHERE (ac909.contract       EQ dsakc.contract)
          AND (ac909.cont-code      EQ dsakc.cont-code)
          AND (ac909.acct-type      EQ "ac90909")
          AND (ac909.since          LE dEnd)
        NO-LOCK NO-ERROR.

    IF (AVAIL ac909)
    THEN DO:
         /* Создаем проводку на 1 руб и закрываем счет 90909 */
        CREATE op.
        {auto-num.i &DefCount  = "op.op"
                    &DefNum    = "''"
                    &DateCount = "end-date"
                    &AssignVar = "op.doc-num"
        }
        ASSIGN
            op.class-code       = "opo"
            op.doc-type         = "ВБО"
            op.order-pay        = "5"
            op.acct-cat         = "o"
            op.op-date          = end-date
            op.doc-date         = end-date
            op.ins-date         = end-date
            op.due-date         = end-date
            op.contract-date    = end-date
            op.op-value-date    = end-date
            op.op-status        = "√"
            op.details          = "Списано по истечению срока дополнительного соглашения от " + STRING(dsakc.open-date, "99.99.9999")
                                + "г р/с " + DelFilFromAcct(loan-acct.acct) + "  " + TRIM(cNm1) + " " + TRIM(cNm2)
                                + " с " + dsakc.comment + ". Без НДС."
            op.filial-id        = dsakc.filial-id
            op.op-transaction   = NEXT-VALUE(op-transaction-id)
            op.op-kind          = "_inCrd3ac"
        NO-ERROR.

        CREATE op-entry.
        ASSIGN
            op-entry.op         = op.op
            op-entry.amt-rub    = 1.0
            op-entry.amt-cur    = 0.0
            op-entry.acct-cat   = "o"
            op-entry.op-cod     = "000000"
            op-entry.currency   = ""
            op-entry.acct-db    = GetRefVal("99999", end-date, "99999" + "," + dsakc.filial-id)
            op-entry.acct-cr    = ac909.acct
            op-entry.op-status  = op.op-status
            op-entry.op-date    = op.op-date
            op-entry.filial-id  = op.filial-id
         NO-ERROR.

         FIND FIRST acct OF ac909
            EXCLUSIVE-LOCK NO-ERROR.
         acct.close-date = end-date.
    END.

    dsakc.close-date = end-date.
    RUN OneDS.
END.

cComm = "Счет закрыт".
FOR EACH dsakc
    WHERE (dsakc.contract           EQ "КассСогАкц")
      AND (dsakc.parent-contract    EQ 'Расчет')
      AND (dsakc.close-date         EQ ?)
    NO-LOCK,
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
      AND (loan-acct.since          LE dEnd)
    NO-LOCK,
FIRST acct OF loan-acct
    WHERE (acct.close-date          NE ?)
    NO-LOCK
    BY dsakc.filial-id
    BY loan.cust-cat
    BY loan.cust-id
    BY loan-acct.acct
    BY dsakc.open-date:

    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, ?, OUTPUT cNm1, OUTPUT cNm2, INPUT-OUTPUT cTmp).

    FIND LAST ac909
        WHERE (ac909.contract       EQ dsakc.contract)
          AND (ac909.cont-code      EQ dsakc.cont-code)
          AND (ac909.acct-type      EQ "ac90909")
        NO-LOCK NO-ERROR.
    RUN OneDS.
END.

/* **************************************************************************** */
IF (I EQ 0)
THEN RUN pb_mail.p (iMail, "Segodnja net zakrytyh DS ob akcepte", "", "").
ELSE DO:
    PUT STREAM sXL UNFORMATTED XLEnd().
    OUTPUT STREAM sXL CLOSE.
    RUN pb_mail.p (iMail, "Zakrytye DS ob akcepte", "", cfile).
/*  OS-DELETE VALUE(cfile). */
END.

{intrface.del}

PROCEDURE OneDS:
    I   = I + 1.
    IF (I EQ 1)
    THEN DO:
        cTmp  = STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
        cfile = "/home2/bis/quit41d/log/dsend/AKC-" + cTmp + ".xml".
        OUTPUT STREAM sXL TO VALUE(cfile).

        PUT STREAM sXL UNFORMATTED XLHead(STRING(end-date, "99.99.9999"), "ICCCCCCDDC", "28,59,62,250,150,150,250,90,90,141").
        PUT STREAM sXL UNFORMATTED XLRowH(0, 34) XLCellHat("Ведомость закрытых Дополнительных соглашений по сроку", 9) XLRowEnd().
        cXL = XLCellHead("№ пп",0,0,0)
            + XLCellHead("Филиал",0,0,0)
            + XLCellHead("Код клиента",0,0,0)
            + XLCellHead("Наименование",0,0,0)
            + XLCellHead("Номер р/счета",0,0,0)
            + XLCellHead("Номер счета 90909",0,0,0)
            + XLCellHead("Наименование получателя",0,0,0)
            + XLCellHead("Начало действия ДС",0,0,0)
            + XLCellHead("Окончание ДС",0,0,0)
            + XLCellHead("Комментарий",0,0,0)
            .
        PUT STREAM sXL UNFORMATTED XLRow(0) cXL XLRowEnd().
    END.

    cXL = XLNumCell(I)
        + XLCell(dsakc.filial-id)
        + XLCell(loan.cust-cat + "_" + STRING(loan.cust-id))
        + XLCell(TRIM(cNm1) + " " + TRIM(cNm2))
        + XLCell(DelFilFromAcct(loan-acct.acct))
        + XLCell(IF (AVAIL ac909) THEN DelFilFromAcct(ac909.acct) ELSE "")
        + XLCell(dsakc.comment)
        + XLDateCell(dsakc.open-date)
        + XLDateCell(IF (dsakc.close-date = ?) THEN ? ELSE dsakc.end-date)
        + XLCell(cComm)
        .
    PUT STREAM sXL UNFORMATTED XLRow(0) cXL XLRowEnd().
END PROCEDURE.