/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: pp-new.frm
      Comment: Форма платежного поручения
   Parameters:
         Uses:
      Used by: pp-new.p pp-new1.p
      Created: 09.11.1999 Kostik
     Modified: 10.02.2000 Kostik Инструкия ЦБ РФ 691-У
     Modified: 18/12/2003 kraw 0023749 для электронного инкассового поручения
     Modified: 17/11/2010 kraa 0132710 Исправленна неверная ширина формы 
     Modified: 27/05/2014 kraw (0227236) УИН может быть больше 20 символов
*/
form
&IF DEFINED(in-el) NE 0 &THEN 
/* для электронного инкассового поручения */
{{&in-el} &in-frame=YES}
&ENDIF
           op.ins-date FORMAT "99.99.9999" AT 5 mSpisPl FORMAT "x(10)" AT 27      "┌─────────┐" at 75  skip
     "────────────────────" AT 1        "────────────────────"                    "│" at 75 NumberForm format "x(7)" "│"     skip
     "Поступ. в банк плат." AT 1        "Cписано со сч. плат."                    "└─────────┘" AT 75 skip(2)

                                                                                                                 "┌──┐" AT 75
     NameOrder Format "x(22)" op.doc-num Format "x(7)" theDate Format "x(10)" AT 35 PayType Format "x(16)" AT 52 "│" AT 75 SPACE(0) mPokST FORMAT "x(2)" SPACE(0) "│"
     "                               ────────────────    ─────────────────"                                       "└──┘" AT 75
     "                                     Дата             Вид платежа "             SKIP
     "Сумма    │" AmtStr[1] Format "x(71)"  skip
     "прописью │" AmtStr[2] Format "x(71)"  skip
     "         │" AmtStr[3] Format "x(71)"  skip
     "         │"                           skip
     "─────────┴─────────────┬───────────────────────┬──────┬───────────────────────────────" skip
     "ИНН" PlINN FORMAT "x(18)" "│КПП" plKPP FORMAT "x(18)"    "│      │" skip
     "───────────────────────┴───────────────────────┤      │"                               SKIP
        PlName[1] Format "x(46)"                    "│Сумма │" Rub Format "x(15)" "" skip
        PlName[2] Format "x(46)"                    "├──────┼───────────────────────────────" skip
        PlName[3] Format "x(46)"                    "│Сч.N  │" PlLAcct Format "x(25)" "" skip
        PlName[4] Format "x(46)"                    "│      │" skip
     "Плательщик                                     │      │" skip
     "───────────────────────────────────────────────┼──────┤" skip
        PlRKC[1] Format "x(46)"          "│БИК   │" PlMFO Format "x(25)" "" skip
        PlRKC[2] Format "x(46)"          "├──────┤" skip
     "Банк плательщика                               │Сч.N  │" PlCAcct Format "x(25)" "" skip
     "───────────────────────────────────────────────┼──────┼───────────────────────────────" skip
        PoRKC[1] Format "x(46)"          "│БИК   │" PoMFO Format "x(25)" "" skip
        PoRKC[2] Format "x(46)"          "├──────┤" skip
     "Банк получателя                                │Сч.N  │" PoCAcct Format "x(25)" "" skip
     "───────────────────────┬───────────────────────┼──────┤" skip
     "ИНН" PoINN FORMAT "x(18)" "│КПП" poKPP FORMAT "x(18)"    "│      │" skip
     "───────────────────────┴───────────────────────┤      │"                                SKIP
      PoName[1] Format "x(46)"                      "│Сч.N  │" PoAcct Format "x(25)" "" skip
      PoName[2] Format "x(46)"                      "├──────┼───────────┬─────────┬─────────" skip
      PoName[3] Format "x(46)"                      "│Вид оп│" op.doc-type Format "x(2)" "       │Очер.плат│" op.order-pay Format "x(2)" "" skip
     "                                               ├──────┤           │         │" SKIP
      PoName[4] Format "x(46)"                      "│Наз.пл│           ├─────────┤" SKIP
     "                                               ├──────┤           │         │" SKIP
     "                                               │      │" mUIN1 format "x(9)" "│         │" skip
     "                                               │Код   │" mUIN2 format "x(9)" "│Рез.поле │" skip
     "Получатель                                     │      │" mUIN3 format "x(9)" "│         │" skip
     "────────────────────┬─────────────┬────┬───────┴────┬─┴───────────┴─┬───────┴──┬──────" skip
     mKBK FORMAT "x(20)│" mOKATO FORMAT "x(11)" "│" mPokOp FORMAT "x(2)" "│"  mPokNP FORMAT "x(10)"  "│" SPACE(0) mPokND FORMAT "x(15)" SPACE(0) "│" SPACE(0) mPokDD FORMAT "x(10)" SPACE(0) "│" SPACE(0) mPokTP FORMAT "x(2)"
     "────────────────────┴─────────────┴────┴────────────┴───────────────┴──────────┴──────"

        Detail[1] Format "x(80)" "" skip
        Detail[2] Format "x(80)" "" skip
        Detail[3] Format "x(80)" "" skip
        Detail[4] Format "x(80)" "" skip
        Detail[5] Format "x(80)" "" skip
     "Назначение платежа" skip
     "──────────────────────────────────────────────────────────────────────────────────────" skip
     "                        Подписи                          Отметки банка" 
/*&IF DEFINED(in-el) NE 0 &THEN */
"получателя" skip
mDateMarcRec FORMAT "x(10)" AT 65
/*&ELSE
skip(1)
&ENDIF*/
skip
     mSposobPoluch FORMAT "x(20)" AT 27 SKIP
     "                 ─────────────────────────────" skip
     "      М.П." skip(1)
     "                 ─────────────────────────────" skip
     with width 90 no-labels frame out-doc.

FORM
  "────┬──────┬──────────┬─────────────┬─────────────┬───────    Дата помещения" at 1 skip
  "N ч.│N плат│Дата      │Сумма частич-│Сумма остатка│Подпись      в картотеку" at 1 skip
  "плат│ордера│плат.     │ного         │платежа      │" at 1 skip
  "    │      │ордера    │платежа      │             │             " AT 1 mDateCart FORMAT "99.99.9999" SKIP
  "────┼──────┼──────────┼─────────────┼─────────────┼───────" at 1 skip
  "    │      │          │             │             │        Отметки банка плательщика:" at 1 skip
  "    │      │          │             │             │" at 1 skip
  "    │      │          │             │             │       " at 1 mPrnStr-El-Doc[1] skip
  "    │      │          │             │             │       " at 1 mPrnStr-El-Doc[2] skip
  "    │      │          │             │             │       " at 1 mPrnStr-El-Doc[3] skip
  "    │      │          │             │             │       " at 1 mPrnStr-El-Doc[4] skip
  "    │      │          │             │             │       " at 1 mPrnStr-El-Doc[5] skip
  "    │      │          │             │             │       " at 1 mPrnStr-El-Doc[6] skip
  "    │      │          │             │             │       " at 1 mPrnStr-El-Doc[7] skip
with frame plattreb-end down no-labels no-underline no-box width 86.

/*prosignMrjAJJT+6RY+KoI6xsO1lA*/