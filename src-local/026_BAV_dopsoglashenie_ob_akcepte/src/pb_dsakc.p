/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОСРКО.330
Что делает:     Отчет по доп.соглашениям об акцепте
Место запуска:  печать -> выходные формы -> отчеты по картотекам
Создан:         05.09.2016 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{intrface.get netw}     /* Отправка в bispc */
{intrface.get xclass}

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNm1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNm2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE I     AS INTEGER   NO-UNDO INIT 0.
DEFINE BUFFER   dsakc FOR loan.
DEFINE BUFFER   ac909 FOR loan-acct.

cXL = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + "-" + shFilial.
cXL = "./DS-" + cXL + ".xml".
REPEAT:
    {getfile.i &filename = cXL &mode = create}
    LEAVE.
END.

/******************************************* Реализация */
IF (shFilial EQ "0500")
THEN PUT UNFORMATTED XLHead(shFilial, "ICCCCCCDD", "28,53,62,250,150,150,250,90,90").
ELSE PUT UNFORMATTED XLHead(shFilial, "ICCCCCDD",  "28,62,250,150,150,250,90,90").

cXL = XLCellHead("№ пп",0,0,0)
    + (IF (shFilial EQ "0500") THEN XLCellHead("Группа счета",0,0,0) ELSE "")
    + XLCellHead("Код клиента",0,0,0)
    + XLCellHead("Наименование",0,0,0)
    + XLCellHead("Номер р/счета",0,0,0)
    + XLCellHead("Номер счета 90909",0,0,0)
    + XLCellHead("Наименование получателя",0,0,0)
    + XLCellHead("Начало действия ДС",0,0,0)
    + XLCellHead("Окончание ДС",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH dsakc
    WHERE (dsakc.contract           EQ "КассСогАкц")
      AND (dsakc.parent-contract    EQ 'Расчет')
      AND (dsakc.close-date         EQ ?)
/*    AND (dsakc.end-date           EQ ?
        OR dsakc.end-date           GE TODAY)
*/  NO-LOCK,
LAST ac909
    WHERE (ac909.contract           EQ dsakc.contract)
      AND (ac909.cont-code          EQ dsakc.cont-code)
      AND (ac909.acct-type          EQ "ac90909")
      AND (ac909.since              LE TODAY)
    NO-LOCK,
EACH loan
    WHERE (loan.class-code          EQ "loanr")
      AND (loan.contract            EQ dsakc.parent-contract)
      AND (loan.cont-code           EQ dsakc.parent-cont-code)
      AND (loan.filial-id           EQ shFilial)
      AND (loan.close-date          EQ ?)
    NO-LOCK,
LAST loan-acct
    WHERE (loan-acct.contract       EQ loan.contract)
      AND (loan-acct.cont-code      EQ loan.cont-code)
      AND (loan-acct.acct-type      EQ loan.contract)
      AND (loan-acct.since          LE TODAY)
      AND CAN-DO("405*,406*,407*,40802*", loan-acct.acct)
    NO-LOCK
    BY loan.cust-cat
    BY loan.cust-id
    BY loan-acct.acct
    BY dsakc.open-date:

    RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, ?, OUTPUT cNm1, OUTPUT cNm2, INPUT-OUTPUT cInn).

    I   = I + 1.
    cXL = XLNumCell(I)
        + (IF (shFilial EQ "0500") THEN XLCell(GetLinks("acct", loan-acct.acct + "," + loan-acct.currency, "s", "acct-group", ",", TODAY)) ELSE "")
        + XLCell(loan.cust-cat + "_" + STRING(loan.cust-id))
        + XLCell(TRIM(cNm1) + " " + TRIM(cNm2))
        + XLCell(loan-acct.acct)
        + XLCell(ac909.acct)
        + XLCell(dsakc.comment)
        + XLDateCell(dsakc.open-date)
        + XLDateCell(dsakc.end-date)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* Перед отправкой протокола проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF mRet EQ "" THEN
       MESSAGE 
          "Запустите программу bispc и нажмите ОК"
       VIEW-AS ALERT-BOX.
END.
/* Отправляем протокол */
RUN sndbispc.p ("file=" + fname + ";class=bq").
{intrface.del}
