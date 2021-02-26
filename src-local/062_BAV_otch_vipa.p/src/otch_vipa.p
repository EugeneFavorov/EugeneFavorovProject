/* Отчет по перечислению в ООО "Премиум Ассистанс" */
/* kam */

DEFINE INPUT PARAMETER  iParam      AS CHARACTER    NO-UNDO.

{globals.i}
{prn-doc.def &with_proc=YES}
{date.fun}

FIND FIRST code
    WHERE (code.class   EQ 'DogInfUL')
      AND (code.parent  EQ 'DogInfUL')
      AND (code.code    EQ iParam)
    NO-LOCK NO-ERROR.
IF (NOT AVAIL code) THEN RETURN.

DEFINE VARIABLE commiss     AS DECIMAL      NO-UNDO INIT 70.
DEFINE VARIABLE aktnum      AS CHARACTER    NO-UNDO INIT "1".
DEFINE VARIABLE AmtString   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE DecStr      AS DECIMAL      NO-UNDO.

DEFINE TEMP-TABLE otchink
    FIELD op_date      AS DATE                 /* дата проводки */
    FIELD summ         AS DECIMAL              /* сумма инкасации */
    .

/* Ввод параметоров */
end-date = TODAY - WEEKDAY(TODAY) + 2.  /* Прошедший понедельник */
beg-date = end-date - 6.                /* Прошлый вторник */
commiss  = DEC(ENTRY(1, code.val, "|")).

define FRAME frame_date_codes
    beg-date label "Дата С "
    end-date label "Дата По "
    commiss  label "Коэфф.вознагр"
    aktnum   label "Номер акта "
    with 1  COL 1 down
    width 50 CENTERED OVERLAY ROW 10 TITLE "Введите данные для отчета : ".

pause 0.
DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

    UPDATE beg-date end-date commiss aktnum
        WITH FRAME frame_date_codes
        EDITING:

        READKEY.
        IF LASTKEY EQ KEYCODE("ESC") THEN return.

        IF LASTKEY EQ KEYCODE("F1")
        THEN DO:
            CASE FRAME-FIELD:
                WHEN "beg-date" OR WHEN "end-date" THEN DO:
                    RUN calend.p.
                    IF (LASTKEY EQ 13 OR LASTKEY EQ 10) AND (pick-value NE ?)
                    THEN FRAME-VALUE = STRING(DATE(pick-value), "99/99/9999").
                END.
            END CASE.
        END.
        ELSE APPLY LASTKEY.
    END.
END.
HIDE FRAME frame_date_codes NO-PAUSE.

/* Сбор документов */
FOR EACH op
    WHERE (op.op-date       GE beg-date)
      AND (op.op-date       LE end-date)
      AND CAN-DO(ENTRY(2, code.val, "|"), op.ben-acct)
      AND (op.name-ben      EQ code.name)
      AND (op.op-status     GE "√")
    NO-LOCK,
EACH op-entry of op
    WHERE (op-entry.acct-db BEGINS '40817')
    NO-LOCK:

    CREATE otchink.
    ASSIGN
        otchink.op_date = op.op-date
        otchink.summ    = op-entry.amt-rub
        .
END.

DEFINE VARIABLE tmpSum      AS DECIMAL      NO-UNDO.
DEFINE VARIABLE tmpSumCom   AS DECIMAL      NO-UNDO.
DEFINE VARIABLE tmpNum      AS INT64        NO-UNDO.
DEFINE VARIABLE tmpItog     AS DECIMAL      NO-UNDO.
DEFINE VARIABLE tmpItogNum  AS DECIMAL      NO-UNDO.
DEFINE VARIABLE tmpItogCom  AS DECIMAL      NO-UNDO.

tmpItog     = 0.
tmpItogNum  = 0.
tmpItogCom  = 0.

RUN Insert_TTName("dateot1",  STRING(DAY(end-date + 1),"99")).
RUN Insert_TTName("dateot2",  getMonthString(MONTH(end-date + 1)) + ' ' + STRING(YEAR(end-date + 1),"9999") + ' г.').
RUN Insert_TTName("dateend1", STRING(DAY(end-date), "99")).
RUN Insert_TTName("dateend2", getMonthString(MONTH(end-date)) + ' ' + STRING(YEAR(end-date), "9999") + ' г.').
RUN Insert_TTName("datebeg1", STRING(DAY(beg-date), "99")).
RUN Insert_TTName("datebeg2", getMonthString(MONTH(beg-date)) + ' ' + STRING(YEAR(beg-date), "9999") + ' г.').
RUN Insert_TTName("aktnum",   aktnum).

RUN BeginCircle_TTName ("DogInfUL").
FOR EACH otchink
    NO-LOCK
    BREAK BY otchink.op_date:

    IF FIRST-OF (otchink.op_date)
    THEN DO:
        tmpSum    = 0.
        tmpNum    = 0.
        tmpSumCom = 0.
    END.

    tmpSum = tmpSum + otchink.summ.
    tmpNum = tmpNum + 1.

    IF LAST-OF (otchink.op_date)
    THEN DO:
        tmpItog     = tmpItog    + tmpSum.
        tmpItogNum  = tmpItogNum + tmpNum.
        tmpSumCom   = tmpSum * commiss / 100.
        tmpItogCom  = tmpItogCom + tmpSumCom.
        RUN Insert_TTName("dater[DogInfUL]",  STRING(otchink.op_date, "99.99.9999")).
        RUN Insert_TTName("num[DogInfUL]",    STRING(tmpNum)).
        RUN Insert_TTName("summ[DogInfUL]",   STRING(tmpSum,   "->>>,>>>,>>>,>>9.99")).
        RUN Insert_TTName("summCom[DogInfUL]",STRING(tmpSumCom,"->>>,>>>,>>>,>>9.99")).
        RUN NextCircle_TTName("DogInfUL").
    END.
END.

RUN Insert_TTName("dater[DogInfUL]",  "ИТОГО:").
RUN Insert_TTName("num[DogInfUL]",    STRING(tmpItogNum)).
RUN Insert_TTName("summ[DogInfUL]",   STRING(tmpItog,   "->>>,>>>,>>>,>>9.99")).
RUN Insert_TTName("summCom[DogInfUL]",STRING(tmpItogCom,"->>>,>>>,>>>,>>9.99")).
RUN NextCircle_TTName("DogInfUL").
RUN EndCircle_TTName ("DogInfUL").

RUN amtstr.p (tmpItogCom, yes, OUTPUT AmtString, OUTPUT DecStr).
AmtString  = AmtString + ' ' + StRing(DecStr,"99") + ' коп., НДС не облагается'.
RUN Insert_TTName("summprop", AmtString).

/* Параметры из справочника */
RUN Insert_TTName("Klient",     ENTRY(1, code.description[1], "|")).
RUN Insert_TTName("Vlice",      ENTRY(2, code.description[1], "|")).
RUN Insert_TTName("PodpisKl",   ENTRY(3, code.description[1], "|")).
RUN Insert_TTName("dogot",      code.description[2]).
RUN Insert_TTName("RekvizityKl",code.description[3]).

RUN printvd.p ("DogInfUL", INPUT TABLE ttNames).
