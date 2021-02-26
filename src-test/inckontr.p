/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: ikontved-318p.p
      Comment: Печать контрольной ведомости
   Parameters: НЕТ
         Uses:
      Used by:
      Created: 

     Печать контрольной ведомости, формы определены в файле ikontved-318p.frm
*/

{globals.i}
{intrface.get xclass}
{wordwrap.def}
{signature.pro}
def var vD as date no-undo.
def var vDs as char no-undo.
def var mKontrol as char no-undo.
DEF VAR mSumkV  AS INT NO-UNDO.
DEF VAR mSummVs AS DEC NO-UNDO.
DEF VAR mSumkD  AS INT NO-UNDO.
DEF VAR mSummDs AS DEC NO-UNDO.
DEF VAR mSumkO  AS INT NO-UNDO.
DEF VAR mSummOs AS DEC NO-UNDO.
DEF VAR mSumkT  AS INT NO-UNDO.
DEF VAR mSummTs AS DEC NO-UNDO.

def temp-table tt-nac NO-UNDO
         field opr as recid
         field nsumki as char
         field kassir as char
         field amt-rub like op-entry.amt-rub
         field amt-izlish as dec
         field amt-nedost as dec
         index opr opr.

{tmprecid.def}
find first tmprecid no-lock no-error.
if not avail tmprecid then do:
    message "не выделен ни один документ" view-as alert-box.
    return.
end.
find first op where RECID( op) EQ tmprecid.id no-lock.
vD  = op.op-date.
vDs = STRING(DAY(vD), "99 ") + ENTRY(MONTH(vD),'января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря') + STRING(YEAR(vD), " 9999 года").

for each op where op.op-date EQ vD and op.op-kind begins '0303i' no-lock:
    create tt-nac.
    assign
       tt-nac.opr    = RECID(op)
       tt-nac.nsumki = GetXattrValueEx('op',string( op.op), 'Номер_сумки', '')
       tt-nac.kassir = GetXattrValueEx('op',string( op.op), 'КассирИнкас', '')
       tt-nac.amt-rub = 0
       tt-nac.amt-izlish = 0
       tt-nac.amt-nedost = 0
       .
    for each op-entry of op no-lock:
       tt-nac.amt-rub    = tt-nac.amt-rub + op-entry.amt-rub.
       tt-nac.amt-izlish = tt-nac.amt-izlish + DEC( GetXattrValueEx('op',string( op.op), 'СуммаИзлиш', '')) NO-ERROR.
       tt-nac.amt-nedost = tt-nac.amt-nedost + DEC( GetXattrValueEx('op',string( op.op), 'СуммаНедостачи', '')) NO-ERROR.
       IF op.doc-date NE op.op-date THEN DO:
        /* mSumkV = mSumkV + 1. */
        mSummVs = mSummVs + op-entry.amt-rub.
       END. ELSE DO:
        /* mSumkD = mSumkD + 1. */
        mSummDs = mSummDs + op-entry.amt-rub.
       END.
       /* mSumkT = mSumkT + 1. */
       mSummTs = mSummTs + op-entry.amt-rub.
    end.

end.

mSumkV = 0.
mSumkD = 0.
mSumkT = 0.
for each op where op.op-date EQ vD and op.op-kind begins '0303i' no-lock:
       mSumkT = mSumkT + 1.
       if op.doc-date NE op.op-date then do: mSumkV = mSumkV + 1. end.
                                    else do: mSumkD = mSumkD + 1. end.
end.

find first _user where _user._userid = USERID('bisquit') no-lock no-error.
mKontrol = _user._user-name.
release _user.
DEF VAR mCounter AS INT NO-UNDO.
DEF VAR DolInc11 AS CHAR NO-UNDO.
DEF VAR DolInc1  AS CHAR NO-UNDO.
DEF VAR DolInc21 AS CHAR NO-UNDO.
DEF VAR DolInc2  AS CHAR NO-UNDO.
DEF VAR FioRep1 AS CHAR NO-UNDO.
DEF VAR FioRep2 AS CHAR NO-UNDO.
DEF VAR mTotSumF AS DEC NO-UNDO.
DEF VAR mTotRazI AS DEC NO-UNDO.
DEF VAR mTotRazN AS DEC NO-UNDO.
/*DEF VAR mTotRazS
DEF VAR mTotRazV
DEF VAR mTotRazP*/
DEF VAR mISummIn AS DEC NO-UNDO.
DEF VAR mASummIz AS DEC NO-UNDO.
DEF VAR mASummNe AS DEC NO-UNDO.

{setdest.i &cols=109 }
put unformatted
"┌───────────────────────┐" at 80 skip
"│ Код формы документа по│" at 80 skip
"│          ОКУД         │" at 80 skip
"├───────────────────────┤" at 80 skip
"│        0402010        │" at 80 skip
"└───────────────────────┘" at 80 skip

       name-bank format "x(60)" at 40
       "__________________________________________________________________________________" AT 16 SKIP
       " полное фирменное (сокращенное фирменное) наименование кредитной организации или  " AT 16 SKIP
       "  полное (сокращенное) наименование филиала, или наименование и (или) номер ВСП   " AT 16 SKIP
       "(при наличии) либо иные идентифицирующие признаки ВСП (при отсутствии наименования" AT 16 SKIP
       "   и номера) с указанием на его принадлежность кредитной организации (филиалу)    " AT 16  SKIP(1)

                                    "КОНТРОЛЬНАЯ  ВЕДОМОСТЬ" at 47 skip
                                        vDs at 48 skip(1)
         mKontrol format "x(50)" at 3 skip
         "(фамилия и инициалы контролирующего)" at 2 skip(2)
       "Принято на обработку в начале рабочего дня" at 1 mSumkV format ">>>9" at 58          " сумок на объявленную сумму" at 62 mSummVs format ">>>,>>>,>>9.99" at 90 skip
       "Принято на обработку в течение рабочего дня" at 1 mSumkD format ">>>9" at 58         " сумок на объявленную сумму" at 62 mSummDs format ">>>,>>>,>>9.99" at 90 skip
       "Передано другим работникам, контролирующим пересчет" at 1 mSumkO format ">>>9" at 58 " сумок на объявленную сумму" at 62 mSummOs format ">>>,>>>,>>9.99" at 90 skip(1)
       "Всего пересчитано" at 5 mSumkT format ">>>9" at 58                                   " сумок на объявленную сумму" at 62 mSummTs format ">>>,>>>,>>9.99" at 90 skip(2)
  .


mCounter = 1.
mISummIn = 0.
mASummIz = 0.
mASummNe = 0.
FOR EACH tt-nac
 BREAK BY tt-nac.kassir BY tt-nac.nsumki:
    IF FIRST-OF(tt-nac.kassir) THEN DO:
        PUT UNFORMATTED
        "┌─────┬──────────────────────────────────────────────────────────────────────────────────────────────────┐" at 1 skip
        "│N п/п│ Фамилия, инициалы кассового работника  " at 1 tt-nac.kassir format "x(58)" "│" skip
        "│     ├───────┬───────────────┬──────────────┬────────────┬────────────┬─────────────────┬───────────────┤" at 1 skip
        "│     │N сумки│Сумма вложения,│Сумма излишка,│   Сумма    │   Сумма    │Cумма неплатеже- │ Сумма имеющих │" at 1 skip
        "│     │       │    цифрами,   │   цифрами,   │ недостачи, │сомнительных│способных, не    │   признаки    │" at 1 skip
        "│     │       │ в руб., коп.  │ в руб., коп. │  цифрами,  │  денежных  │имеющих признаков│   подделки    │" at 1 skip
        "│     │       │               │              │в руб., коп.│знаков Банка│подделки денежных│денежных знаков│" at 1 skip
        "│     │       │               │              │            │  России,   │знаков Банка     │ Банка России, │" at 1 skip
        "│     │       │               │              │            │  цифрами,  │ России, цифрами,│ цифрами,      │" at 1 skip
        "│     │       │               │              │            │в руб., коп.│ в руб., коп.    │  в руб., коп. │" at 1 skip
        "├─────┼───────┼───────────────┼──────────────┼────────────┼────────────┼─────────────────┼───────────────┤" at 1 skip
        "│  1  │   2   │       3       │      4       │     5      │     6      │         7       │       8       │" at 1 skip
        "├─────┼───────┼───────────────┼──────────────┼────────────┼────────────┼─────────────────┼───────────────┤" at 1 skip
      .
      mTotSumF = 0.
      mTotRazI = 0.
      mTotRazN = 0.
    END.
    /* Форма для цикла "body" */
    PUT UNFORMATTED
       "│" at 1 mCounter format ">>>9" " │" tt-nac.nsumki format "x(7)" "│" tt-nac.amt-rub format ">>>,>>>,>>9.99" " │" 
        tt-nac.amt-izlish format ">>,>>>,>>9.99" " │ " tt-nac.amt-nedost format ">>>,>>9.99" " │" "       0.00" format "x(11)" " │"
       "            0.00" format "x(16)" " │" "          0.00" format "x(14)" " │" skip
        .
    mCounter = mCounter + 1.
    mTotSumF = mTotSumF + tt-nac.amt-rub.
    mTotRazI = mTotRazI + tt-nac.amt-izlish.
    mTotRazN = mTotRazN + tt-nac.amt-nedost.
    mISummIn = mISummIn + tt-nac.amt-rub.
    mASummIz = mASummIz + tt-nac.amt-izlish.
    mASummNe = mASummNe + tt-nac.amt-nedost.

    IF LAST-OF(tt-nac.kassir) THEN DO:
	/* Форма для цикла "подвал" */
	PUT UNFORMATTED
	    "├─────┴───────┼───────────────┼──────────────┼────────────┼────────────┼─────────────────┼───────────────┤" at 1 skip
	    "│      Итого: " at 1
                        "│" mTotSumF format ">>>,>>>,>>9.99" " │" mTotRazI format ">>,>>>,>>9.99" " │ "
                         mTotRazN format ">>>,>>9.99" " │" "       0.00" format "x(11)" " │"
                         "            0.00" format "x(16)" " │"  "          0.00" format "x(14)" " │" skip
            "└─────────────┴───────────────┴──────────────┴────────────┴────────────┴─────────────────┴───────────────┘" at 1 skip(1)
                 DolInc11 format "x(35)" at 3 skip
                 DolInc1 format "x(35)" at 3 FioRep1 format "x(33)" at 57 skip
                 "___________________________________ ____________ _________________________________" at 3 skip
                      "(наименование должности)         (личная                   (ФИО)" at 8 skip
                                                       "подпись)" at 41 skip(1)
                 DolInc21 format "x(35)" at 3 skip
                 DolInc2 format "x(35)" at 3 FioRep2 format "x(33)" at 57 skip
                 "___________________________________ ____________ _________________________________" at 3 skip
                      "(наименование должности)         (личная                   (ФИО)" at 8 skip
                                                       "подпись)" at 41 skip
            .
    END.
END.

/* IF mCounter > 43 AND mCounter < 54 THEN PAGE. */
PUT UNFORMATTED SKIP(1)
     "┌─────────────┬───────────────┬──────────────┬────────────┬────────────┬─────────────────┬───────────────┐" at 1 skip
     "│             │Сумма вложения,│Сумма излишка,│   Сумма    │   Сумма    │Cумма неплатеже- │ Сумма имеющих │" at 1 skip
     "│             │    цифрами,   │   цифрами,   │ недостачи, │сомнительных│способных, не    │   признаки    │" at 1 skip
     "│             │ в руб., коп.  │ в руб., коп. │  цифрами,  │  денежных  │имеющих признаков│   подделки    │" at 1 skip
     "│             │               │              │в руб., коп.│знаков Банка│подделки денежных│денежных знаков│" at 1 skip
     "│             │               │              │            │  России,   │знаков Банка     │ Банка России, │" at 1 skip
     "│             │               │              │            │  цифрами,  │ России, цифрами,│ цифрами,      │" at 1 skip
     "│             │               │              │            │в руб., коп.│ в руб., коп.    │  в руб., коп. │" at 1 skip
     "├─────────────┼───────────────┼──────────────┼────────────┼────────────┼─────────────────┼───────────────┤" at 1 skip
     "│Итого за день│" at 1 mISummIn format ">>>,>>>,>>9.99" " │" mASummIz format ">>,>>>,>>9.99" " │ "
        mASummNe format ">>>,>>9.99" " │" "       0.00" format "x(12)" "│"
        "            0.00" format "x(16)" " │" "          0.00" format "x(15)" "│"
     "└─────────────┴───────────────┴──────────────┴────────────┴────────────┴─────────────────┴───────────────┘" at 1 skip
.

{preview.i}
