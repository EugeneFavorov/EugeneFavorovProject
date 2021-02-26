/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ЗАО "Банковские информационные системы"
     Filename: SFACTPRN.I
      Comment: Собственно печать счетов-фактур
   Parameters:
         Uses:
      Used by: SFACTPRN.I
      Created: 06.05.2006 ZIAL
     Modified: 18/05/2006 ZIAL (0059334) АХД. Создание Акта на выполнение работ/услуг 
               по счету-фактуре
     Modified: 24/07/2006 ZIAL (0059334) АХД. Создание Акта на выполнение работ/услуг 
               по счету-фактуре
     Modified: 20.03.2007 17:38 OZMI     (0070614)
*/

mIspolnit = "Мы, нижеподписавшиеся, " + (IF mSFSellerFIO NE "" THEN mSFSellerFIO
                                                               ELSE mSFSeller) +
            " представитель ИСПОЛНИТЕЛЯ, с одной стороны".

IF LENGTH(mIspolnit) GT 100 THEN
   mIspolnit = SplitStr(mIspolnit,
                        100,
                        "~n").

mContrAgent = IF mSFBuyerFIO NE "" THEN mSFBuyerFIO
                                   ELSE mSFBuyer.
mContrAgent = mContrAgent + " представитель ЗАКАЗЧИКА с другой стороны, составили настоящий акт, ~
в том, что ИСПОЛНИТЕЛЬ выполнил, а ЗАКАЗЧИК принял следующие работы: ".

IF LENGTH(mContrAgent) GT 100 THEN
   mContrAgent = SplitStr(mContrAgent,
                          100,
                          "~n").

PUT STREAM sfact UNFORMATTED
/*   SKIP(5) */
   SPACE(20) "Акт №" + STRING(mSFNum,"x(14)") + 
   "от " + 
   (IF mSfDate NE ? THEN 
       STRING(DAY(mSFDate),"99") + " " + 
       STRING(ENTRY(MONTH(mSFDate),Monthes)) + " " + 
       STRING(YEAR(mSFDate)) + "г." 
    ELSE "") 
   SKIP 
   SPACE(25) "на выполнение работ-услуг." SKIP(1)
   SPACE(5) "Основание  : " + 

   IF mAxdNum EQ ? OR mAxdNum EQ "" THEN 
      ("Счет-фактура №" + 
      mSFNum +
      "  от " + 
      (IF mSFDate NE ? THEN 
          STRING(mSFDate)
       ELSE ""))
   ELSE 
      ("Договор №" + 
      mAxdNum + 

      "  от " + 
      (IF mAxdDate NE ? THEN 
          STRING(mAxdDate)
       ELSE ""))

   SKIP

   SPACE(5) mIspolnit SKIP
   "и " + mContrAgent SKIP.


IF CAN-FIND(FIRST ttServ NO-LOCK) 
THEN DO:

   PUT STREAM sfact UNFORMATTED

      "┌───┬────────────────────────────────────────┬──────────────────┬─────────────┬─────────┬──────────────────┐" SKIP
      "│   │                                        │                  │             │         │                  │" SKIP
      "│  №│              Наименование              │       Цена       │   Кол.-во   │ Ед.изм. │       Сумма      │" SKIP
      "│   │                                        │                  │             │         │                  │" SKIP
      "├───┼────────────────────────────────────────┼──────────────────┼─────────────┼─────────┼──────────────────┤" SKIP
   .

   ASSIGN
      mNalogSumm = 0
      mTotalSumm = 0

      mSummOut = 0
      mN = 0.
   .
   FOR EACH ttServ 
      NO-LOCK
      BREAK BY ttServ.NameServ:
      ASSIGN   
         mTotalSumm = mTotalSumm + ttServ.TotalSumm
         mSummOut = mSummOut + ttServ.SummOut
         mN = mN + 1
         mSummNalogSumm = mSummNalogSumm + ttServ.NalogSumm.
      .
      mNameStr[1] = TRIM(REPLACE(ttServ.NameServ, "~n", " ")).

      {wordwrap.i &s = mNameStr &l = 40 &n = 10}

      /* печатаем данные по услуге */
      PUT STREAM sfact UNFORMATTED
             "│" + STRING(mN,">>9") +
             "│" + STRING(mNameStr[1], "x(40)") +
             "│" + STRING(ttServ.Price,">>>,>>>,>>>,>>9.99") +
             "│" + STRING(ttServ.Quant,">>>>>>>>>9.99") +
             "│" + STRING(ttServ.Edin, "x(9)") +
             "│" + STRING(ttServ.SummOut,">>>,>>>,>>>,>>9.99") +
             "│" SKIP.

      /* печатаем наименование услуги на следующих строчках, 
      ** если оно оказалось длинным */
      DO mI = 2 TO 10:
         IF NOT {assigned mNameStr[mI]} THEN LEAVE.

         PUT STREAM sfact UNFORMATTED
            "│" + FILL(" ",3)   +
            "│" + STRING(mNameStr[mI], "x(40)") +
            "│" + FILL(" ",18)  +
            "│" + FILL(" ",13)  +
            "│" + FILL(" ",9)   +
            "│" + FILL(" ",18)  +
            "│" SKIP
         .
      END.

      IF NOT LAST(ttServ.NameServ) 
      THEN DO:
         PUT STREAM sfact UNFORMATTED 
      "├───┼────────────────────────────────────────┼──────────────────┼─────────────┼─────────┼──────────────────┤" SKIP
         .

      END.
      ELSE DO:

         RUN x-amtstr.p (
                         INPUT mTotalSumm,
                         INPUT "",
                         INPUT NO,
                         INPUT NO,
                         OUTPUT mSummOutRub,
                         OUTPUT mSummOutCop
                        ).


         PUT STREAM sfact UNFORMATTED 
      "└───┴────────────────────────────────────────┴──────────────────┴─────────────┴─────────┼──────────────────┤" SKIP
      "                                                                                  Итого:│" + STRING(mSummOut,">>>,>>>,>>>,>>9.99") + "│" SKIP
      "                                                                                        ├──────────────────┤" SKIP
      "                                                                              Итого НДС:│" + STRING(mSummNalogSumm,">>>,>>>,>>>,>>9.99") + "│" SKIP
      "                                                                                        ├──────────────────┤" SKIP
      "                                                                   Всего (с учетом НДС):│" + STRING(mTotalSumm,">>>,>>>,>>>,>>9.99") + "│" SKIP
      "                                                                                        └──────────────────┘" SKIP
      "Итоговая сумма к оплате: " + mSummOutRub + "рублей  " + mSummOutCop + "копеек , в том числе, " SKIP
         .
      END.
   END.

   mSummNalogSumm = 0.

   FOR EACH ttServ 
      NO-LOCK
      BREAK BY ttServ.Nlog:
      mSummNalogSumm = mSummNalogSumm + ttServ.NalogSumm.
      IF LAST-OF(ttServ.Nlog) THEN
      DO:
         PUT STREAM sfact UNFORMATTED
            "НДС " + STRING(ttServ.Nlog) + "% - " + 
            TRIM(REPLACE(STRING(mSummNalogSumm,"->>>>>>>>9.99"),"~."," руб ")) + 
            " коп" + (IF NOT LAST(ttServ.Nlog) THEN ", " ELSE ".") .
         ASSIGN 
            mNalogSumm = 0
            mSummNalogSumm = 0
         .

      END.

   END.

   PUT STREAM sfact UNFORMATTED
      SKIP
      "Работы выполнены в полном объеме, в установленные сроки и с надлежащим качеством. Стороны" SKIP
      "претензий друг к другу не имеют.".

   DISPLAY  STREAM sfact
      mSFSeller VIEW-AS EDITOR SIZE 30 BY 2 LABEL "Исполнитель"  SPACE(10)
      mSFBuyer  VIEW-AS EDITOR SIZE 30 BY 2 LABEL "Заказчик" SKIP
   WITH FRAME a WIDTH 110 SIDE-LABELS
   .

   DISPLAY  STREAM sfact
      mSFSellerAddr LABEL "Адрес" VIEW-AS EDITOR SIZE 35 BY 2 SPACE(11)
      mSFBuyerAddr LABEL "Адрес" VIEW-AS EDITOR SIZE 35 BY 2 SKIP
   WITH FRAME a WIDTH 110 SIDE-LABELS.

   PUT STREAM sfact UNFORMATTED
      "ИНН: " + mSFSellerINN  SPACE(30)
      "        ИНН: " STRING(mSFBuyerINN,"x(22)") 
    /*  "       КПП: " STRING(mSFBuyerKPP,"x(22)") SKIP*/

    /*  "        КПП: " STRING(mSFSellerKPP,"x(22)")*/ 
      SKIP(2)
   .

   PUT STREAM sfact UNFORMATTED
      "       ___________________" mSFSellerFIO SPACE(34)
      "___________________" mSFBuyerFIO
      SKIP(1)
      .
   /*
   DISPLAY  STREAM sfact
      "       ___________________" mSFSellerFIO VIEW-AS EDITOR SIZE 22 BY 2 NO-LABELS SPACE(8)
      "___________________" mSFBuyerFIO VIEW-AS EDITOR SIZE 22 BY 2 NO-LABELS 
   WITH FRAME b WIDTH 110 SIDE-LABELS.
   */

END.
