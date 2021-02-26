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
     Modified: 2r2.10.2003 kraw (0017952) мелкие исправления внешнего вида
     Modified: 23/08/2012 kraw (0132548) между подписями 15 мм
*/.
&IF DEFINED(a107h)  &THEN 
   {pp-uni_1-2.frm {&*}}
&ELSE
Form "~n@(#) pp-new.frm 1.0 Kostik 09/11/1999 Kostik 09/11/1999 Форма платежного поручения" with frame sccs-id width 250.

form
&IF DEFINED(uni1) NE 0 &THEN
     theHeader skip(1)
&ENDIF
&IF DEFINED(uni2) NE 0 &THEN
     "┌────────────────────────┐" skip
     "│        ПРИНЯТО         │" skip
     "│" theBank "│" skip
     "│" theCity "│" skip
     "│" theUserName format "x(22)" "│" skip
     "│     " op.op-date "г.     │" skip
     "└────────────────────────┘" skip
&ENDIF
     &IF DEFINED(ELECTROPP) NE 0 &THEN
     "Порядковый номер                        Дата составления         "                  SKIP
     "электронного документа" elec-doc-num   "электронного документа" AT 41 elec-doc-date SKIP
     SKIP(1)
     "Уникальный"                                SKIP
     "идентификатор составителя " uni-ident-send SKIP(3)
     &ENDIF

           op.ins-date FORMAT "99.99.9999" AT 5 mSpisPl FORMAT "x(10)" AT 31      "┌─────────┐" at 75  skip
     "────────────────────" AT 1        "    ────────────────────"                 "│         │" at 75 skip
     "Поступ. в банк плат." AT 1        "    Cписано со сч. плат."                 "└─────────┘" AT 75 skip(2)

                                                                                                                 "┌──┐" AT 75
     NameOrder[1] Format "x(24)" op.doc-num Format "x(7)" theDate Format "x(10)" AT 35 PayType Format "x(16)" AT 52 "│" AT 75 SPACE(0) mPokST FORMAT "x(2)" SPACE(0) "│"
     NameOrder[2] Format "x(22)" "         ────────────────    ─────────────────"                                       "└──┘" AT 75
     "                                     Дата             Вид платежа "             SKIP(1)
     "Сумма    │" AmtStr[1] Format "x(71)"  skip
     "прописью │" AmtStr[2] Format "x(71)"  skip
     "         │" AmtStr[3] Format "x(71)"  skip
     "─────────┴─────────────┬───────────────────────┬──────┬──────────────────────────────" skip
     "ИНН" PlINN FORMAT "x(18)" "│КПП" plKPP FORMAT "x(18)"    "│      │"                    SKIP
     "───────────────────────┴───────────────────────┤      │"                               SKIP
        PlName[1] Format "x(46)"                    "│Сумма │" Val Format "x(15)" " /" skip
        PlName[2] Format "x(46)"                    "│      │" Rub Format "x(15)" skip
        PlName[3] Format "x(46)"                    "├──────┼──────────────────────────────" skip
        PlName[4] Format "x(46)"                    "│      │" skip
        PlName[5] Format "x(46)"                    "│Сч.N  │" PlLAcct Format "x(25)" "" skip
     "                                               │      │" skip
     "Плательщик                                     │      │" skip
     "───────────────────────────────────────────────┼──────┤" skip
        PlRKC[1] Format "x(46)"          "│БИК   │" PlMFO Format "x(25)" "" skip
        PlRKC[2] Format "x(46)"          "├──────┤" skip
     "Банк плательщика                               │Сч.N  │" PlCAcct Format "x(25)" "" skip
     "───────────────────────────────────────────────┼──────┼──────────────────────────────" skip
        PoRKC[1] Format "x(46)"          "│БИК   │" PoMFO Format "x(25)" "" skip
        PoRKC[2] Format "x(46)"          "├──────┤" skip
     "Банк получателя                                │Сч.N  │" PoCAcct Format "x(25)" "" skip
     "───────────────────────┬───────────────────────┼──────┤" skip
     "ИНН" PoINN FORMAT "x(18)" "│КПП" poKPP FORMAT "x(18)"    "│      │" skip
     "───────────────────────┴───────────────────────┤      │"                               SKIP

      PoName[1] Format "x(46)"           "│Сч.N  │" PoAcct Format "x(25)" "" skip
      PoName[2] Format "x(46)"           "│      │" skip
      PoName[3] Format "x(46)"           "│      │" skip
      PoName[4] Format "x(46)"           "├──────┼────────┬─────────┬───────────" skip
      PoName[5] Format "x(46)"           "│Вид оп│        │Срок плат│" SPACE(0) op.due-date Format "99.99.9999"skip
     "                                               │Наз.пл│        │Очер.плат│ " SPACE(0) op.order-pay Format "x(2)" "" skip
     "Получатель                                     │Код   │        │Рез.поле │" SPACE(0) mPPDate skip
     "────────────────────┬─────────────┬────┬───────┴────┬─┴────────┴────┬────┴─────┬─────" skip
/*    1234567890123456789 | 12345678901 | 12 | 1234567890 |123456789012345|1234567890| 12 |*/
     mKBK FORMAT "x(19)" "│" mOKATO FORMAT "x(11)" "│" mPokOp FORMAT "x(2)" "│"  mPokNP FORMAT "x(10)"  "│" SPACE(0) mPokND FORMAT "x(15)" SPACE(0) "│" SPACE(0) mPokDD FORMAT "x(10)" SPACE(0) "│" SPACE(0) mPokTP FORMAT "x(2)"
     "────────────────────┴─────────────┴────┴────────────┴───────────────┴──────────┴─────"
        Detail[1] Format "x(80)" "" skip
        Detail[2] Format "x(80)" "" skip
        Detail[3] Format "x(80)" "" skip
        Detail[4] Format "x(80)" "" skip
        Detail[5] Format "x(80)" "" skip(2)
     "Назначение платежа" skip
     "─────────────────────────────────────────────────────────────────────────────────────" skip
&IF DEFINED(uni1) NE 0 &THEN
     "                                                    ┌────────────────────────┐" skip
     "                                                    │        ПРИНЯТО         │" skip
     " Ответисполнитель " _user._user-name format "x(30)" "  │" theBank "│" skip
     "─────────────────────────────────────────────────── │" theCity "│" skip
     "                                                    │     " op.op-date "г.     │" skip
     "                                                    └────────────────────────┘" skip
&ELSE
     "                        Подписи                          Отметки банка"  SKIP
                                                           {pp-uni.not &FRAME-TRIG=YES &NUM-STR=1 &AT-NUM = 54 } SKIP
                                                           {pp-uni.not &FRAME-TRIG=YES &NUM-STR=2 &AT-NUM = 54 } SKIP
                                                           {pp-uni.not &FRAME-TRIG=YES &NUM-STR=3 &AT-NUM = 54 } SKIP
     "                 ─────────────────────────────"      {pp-uni.not &FRAME-TRIG=YES &NUM-STR=4 &AT-NUM = 54 } SKIP
     "      М.П."                                           SKIP(2)
     "                 ─────────────────────────────"       SKIP
                                                            SKIP
&ENDIF
/*-------------------------------------- Штамп, для СПБИ ---------------------------------------*/
&IF DEFINED(uni3) NE 0 &THEN
     "                                                    " BankName[1] FORMAT "x(30)"  SKIP 
     "                                                    " BankName[2] FORMAT "x(30)"  SKIP 
     "                                                    " BankName[3] FORMAT "x(30)"  SKIP 
     "                                                    " BankAcct[1] FORMAT "x(30)"  SKIP 
     "                                                                                " SKIP 
     "                                                              " op.op-date        SKIP
     "                                                                                " SKIP
     "                                                    " BankMFO[1] FORMAT   "x(30)" SKIP
     "                                                    " BankMFO[2] FORMAT   "x(30)" SKIP
     "                                                                ПРОВЕДЕНО       " SKIP
     "                                                    " Inspector[1] FORMAT "x(30)" SKIP
&ENDIF
/*----------------------------------------------------------------------------------------------*/
     with width 88 no-labels frame out-doc.
&ENDIF