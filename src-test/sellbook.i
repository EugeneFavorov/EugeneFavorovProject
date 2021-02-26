/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2005 ЗАО "Банковские информационные системы"
     Filename: BOOKPRN.I
      Comment: Инклюд для пеечати книги покупок и книги продаж.
               Определяет переменные и для шапки отчёта
   Parameters:
         Uses: 
      Used by:
      Created: 27.01.2005 Dasu 0041881
     Modified: 10/09/2013 kraw (0191849) Изменение формы 
*/

  
{globals.i}
{intrface.get xclass}
{intrface.get instrum}
{intrface.get tparam}
{intrface.get axd}
{intrface.get asset}
{intrface.get strng }
  
  
DEF INPUT PARAM iDataBlock AS CHAR NO-UNDO.

DEF TEMP-TABLE tmpDataLine LIKE DataLine
   FIELD sf        AS CHARACTER
   FIELD Date1     AS DATE
   FIELD Date2     AS DATE
   FIELD isprav    AS CHARACTER /*Статус (аннулированная или исправительная)*/
   FIELD cont-code AS CHARACTER /*Номер счет-фактуры, которую исправляет
                                 данная счет-фактура.*/  
   FIELD k1 AS CHARACTER
   FIELD k2 AS CHARACTER
   FIELD k3 AS CHARACTER
   FIELD nums AS CHARACTER
   INDEX dateprim Data-ID Date1 Date2
   INDEX isprav cont-code.

DEF BUFFER bDB       FOR DataBlock.
DEF BUFFER bDataLine FOR DataLine.
DEF BUFFER bDL       FOR tmpDataLine.

DEF VAR mBuyer      AS CHAR NO-UNDO.
DEF VAR mINN        AS CHAR NO-UNDO.
DEF VAR mKPP        AS CHAR NO-UNDO.
DEF VAR mAdrs       AS CHAR NO-UNDO.
                    
DEF VAR mStrNum     AS INT64  NO-UNDO.
DEF VAR mOutStr     AS CHAR NO-UNDO.
                    
DEF VAR i           AS INT64  NO-UNDO.
DEF VAR mNamePostav AS CHAR NO-UNDO.
DEF VAR mNums       AS CHAR NO-UNDO.
DEF VAR mBuhName    AS CHAR NO-UNDO.
DEF VAR mAmt        AS CHAR NO-UNDO.

DEFINE VARIABLE mCntryDigCode AS CHARACTER NO-UNDO.

DEFINE STREAM sfact.
/*
{setdest.i &cols=120 &STREAM="stream sfact"}
*/
OUTPUT STREAM sfact TO VALUE("./sellbook.txt") CONVERT TARGET "1251".

/*ищем блок данных для получения дат начала и конца*/
FIND FIRST bDB WHERE bDB.Data-ID EQ INT64(IDATABLOCK)
   NO-LOCK NO-ERROR.

FIND FIRST branch WHERE branch.Branch-Id EQ bDB.Branch-Id NO-LOCK NO-ERROR.
IF AVAIL branch THEN mBuhName = branch.CFO-name.

/*получаем данные о покупателе (банк), данные берутся из настроечных параметров*/
/*Требования 0250274 - для новых форм с 01.10.2014. */
IF CAN-DO("*_new",ENTRY(1, PROGRAM-NAME(1), ".")) THEN
DO:
   RUN BookAttribs_Buyer("",
                         "", 
                         bDB.Branch-Id,
                         bDB.end-date,
                         OUTPUT mBuyer,
                         OUTPUT mAdrs,
                         OUTPUT mINN,
                         OUTPUT mKPP).

   IF     FGetSetting("СчФБанкРек",?,"") EQ "Да"
      AND FGetSetting("СчФПодраз",?,"")  EQ "Да" 
      AND FGetSetting("КнигБанкРек",?,"") EQ "Да"
   THEN
      mKPP  = GetXAttrValue("branch", bDB.Branch-Id, "КПП").
END.
ELSE
DO:
   RUN SFAttribs_Buyer("",
                       "", 
                       bDB.Branch-Id,
                       OUTPUT mBuyer,
                       OUTPUT mAdrs,
                       OUTPUT mINN,
                       OUTPUT mKPP).

   IF     FGetSetting("СчФБанкРек",?,"") EQ "Да"
      AND FGetSetting("СчФПодраз",?,"")  EQ "Да" 
   THEN
      mKPP  = GetXAttrValue("branch", bDB.Branch-Id, "КПП").
END.

/*Копируем во временную таблицу, что бы сделать правильную сортировку по датам*/
{empty bDL}

FOR EACH bDataLine WHERE 
         bDataLine.data-id = INT64(IDATABLOCK)
   NO-LOCK:
   CREATE bDL.
   BUFFER-COPY bDataLine TO bDL.
   ASSIGN
      bDL.Date1 = DATE(bDataLine.Sym1)
      bDL.Date2 = DATE(bDataLine.Sym2)
      bDL.sf    = Entry(11,bDataLine.txt,"~n")
   .
END.
/*Печать дополнительных листов книги покупок*/
PROCEDURE BuyAddPage:

   DEFINE INPUT PARAMETER inDataBlock  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inPagenumber AS INT64     NO-UNDO. /*Номер страницы*/
   /*ШАПКА КНИГИ*/
   DEFINE VARIABLE iVal_m     AS DEC  NO-UNDO EXTENT 9. /*не аннулирован.*/
   DEFINE VARIABLE iVal   AS DECIMAL NO-UNDO EXTENT 9. /*Итого по аннулирован.
                                                         счет-фактурам */ 
   DEFINE VARIABLE iVal_a AS DECIMAL NO-UNDO EXTENT 9. /*Итого по аннулирован.
                                                         счет-фактурам */ 
   DEFINE VARIABLE iVal_d   AS DECIMAL NO-UNDO EXTENT 9. /*Итого по счет-фактурам с ДР ДопЛист */ 
   DEFINE VARIABLE ii       AS INT64   NO-UNDO.
   DEFINE VARIABLE vAdvance AS LOGICAL NO-UNDO.
   /*Расчёт Итого для счет-фактур (не аннулированных) */
   RUN CalcTotalsIn(inDataBlock, INPUT-OUTPUT iVal_m).
   
   PUT STREAM sfact UNFORMATTED
      FILL(" ",40)  + "ДОПОЛНИТЕЛЬНЫЙ ЛИСТ К КНИГЕ ПОКУПОК №" + 
      STRING(inPagenumber) SKIP(3)
      "Покупатель " + STRING(mBuyer,"x(35)")              SKIP
      "           " + FILL("─",35)                        SKIP(2)
      "Идентификационный номер и код причины постановки на учет налогоплательщика-покупателя " + TRIM(STRING(mINN)) + "/" + STRING(mKPP,"x(15)") SKIP
      "                                                                                      " + FILL("─",35) SKIP(2)
      "Налоговый период (месяц, квартал), год, в котором зарегистрирован счет-фактура до внесения в него исправления: "
      term2str(bdb.Beg-Date,bdb.End-Date) SKIP
      "                                                                                                               ────────────────" SKIP(2)
   
      "Дополнительный лист оформлен " STRING(TODAY,"99/99/9999") SKIP
       "                             ──────────" SKIP(3)
   .
   /*ШАПКА ТАБЛИЧКИ*/
   PUT STREAM sfact UNFORMATTED
      "┌───┬───────────────┬──────────┬──────────┬─────────────────────┬──────────────┬──────────────┬─────────────────┬─────────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────┐" SKIP
      "│ № │ Дата и номер  │  Дата    │   Дата   │Наименование продавца│ ИНН продавца │ КПП продавца │      Страна     │   Всего     │                                        В том числе                                                            │" SKIP
      "│   │ счета-фактуры │ оплаты   │ принятия │                     │              │              │  происхождения  │  покупок,   ├─────────────────────────────────────────────────────────────────────────────────────────────────┬─────────────┤" SKIP
      "│п/п│   продавца    │ счета-   │ на учет  │                     │              │              │  товара (цифро- │ включая НДС │                 покупки, облагаемые налогом по ставке                                           │  покупки    │" SKIP
      "│   │               │ фактуры  │ товаров  │                     │              │              │ вой код), номер │             ├───────────────────────────┬───────────────────────────┬─────────────┬───────────────────────────┤освобождаемые│" SKIP
      "│   │               │ продавца │ (работ,  │                     │              │              │    таможенной   │             │       18 процентов        │       10 процентов        │ 0 процентов │       20 процентов*       │  от налога  │" SKIP   
      "│   │               │          │  услуг), │                     │              │              │    декларации   │             ├─────────────┬─────────────┼─────────────┬─────────────┼─────────────┼─────────────┬─────────────┤             │" SKIP
      "│   │               │          │ имущест- │                     │              │              │                 │             │стоимость по-│ сумма НДС   │стоимость по-│ сумма НДС   │             │стоимость по-│ сумма НДС   │             │" SKIP
      "│   │               │          │ твенных  │                     │              │              │                 │             │купки без НДС│             │купки без НДС│             │             │купки без НДС│             │             │" SKIP
      "│   │               │          │   прав   │                     │              │              │                 │             │             │             │             │             │             │             │             │             │" SKIP
      "├───┼───────────────┼──────────┼──────────┼─────────────────────┼──────────────┼──────────────┼─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│ 1 │       2       │     3    │    4     │          5          │       5а     │      5б      │        6        │      7      │      8а     │      8б     │      9а     │     9б      │      10     │     11а     │     11б     │      12     │" SKIP
      "├───┼───────────────┼──────────┼──────────┼─────────────────────┼──────────────┼──────────────┼─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   .
   /*ДАННЫЕ ПО СТРОКАМ*/
   /*ИТОГО*/
   PUT STREAM sfact UNFORMATTED
      "├───┴───────────────┴──────────┴──────────┴─────────────────────┴──────────────┴──────────────┴─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│                                                                                                         ИТОГО:│" + 
      STRING(iVal_m[1],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[2],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[3],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[4],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[5],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[6],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[7],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[8],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[9],">>,>>>,>>9.99") + "│" SKIP
      "├───┬───────────────┬──────────┬──────────┬─────────────────────┬──────────────┬──────────────┬─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   .
   mStrNum = 0.
   FOR EACH bDL WHERE bDL.Data-Id EQ INT64(INDATABLOCK)
   NO-LOCK,

   FIRST DataLine WHERE DataLine.Data-ID EQ bDL.Data-Id
                    AND DataLine.Sym1    EQ bDL.Sym1
                    AND DataLine.Sym2    EQ bDL.Sym2
                    AND DataLine.Sym3    EQ bDL.Sym3
                    AND DataLine.Sym4    EQ bDL.Sym4
   NO-LOCK
      BREAK BY DataLine.Data-ID:
      
      IF ENTRY(9,DataLine.Txt,"~n") EQ "Аннулир" THEN
      DO:
         
         ASSIGN
            mStrNum = mStrNum + 1.
            mNamePostav =  SplitStr(ENTRY(1,DataLine.Txt,"~n"),
                                          21,
                                          "~n").
   
            mNums       =  SplitStr(ENTRY(6,DataLine.Txt,"~n") + " " 
                                  + ENTRY(8,DataLine.Txt,"~n"),
                                          15,
                                          "~n").
      
            mAmt        =  SplitStr(STRING(DataLine.Val[11],">>,>>>,>>9.99") + " " 
                                  + ENTRY(7,DataLine.Txt,"~n"),
                                    13,
                                    "~n")       .
           vAdvance    = ENTRY(4,bDL.Txt,"~n") EQ "Аванс" 
         .

         mCntryDigCode = ENTRY(4,bDL.Txt,"~n").

         FIND FIRST country WHERE country.country-name EQ mCntryDigCode NO-LOCK NO-ERROR.

         IF AVAILABLE country THEN
            mCntryDigCode = STRING(country.country-alt-id).
         ELSE
            mCntryDigCode = "".
   
         PUT STREAM sfact UNFORMATTED
            "│" +
            STRING(mStrNum,"999") + "│" +
            STRING(ENTRY(1,mNums,"~n"),"x(15)") + "│" +
            STRING(DataLine.Sym2,"x(10)") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",8) + " ")
             ELSE STRING(bDL.Sym1,"x(10)")) + "│" +
            STRING(ENTRY(1,mNamePostav,"~n"),"x(21)") + "│" +
            STRING(ENTRY(2,DataLine.Txt,"~n"),"x(14)") + "│" +
            STRING(ENTRY(3,DataLine.Txt,"~n"),"x(14)") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",15) + " ")
             ELSE (STRING(mCntryDigCode,"x(10)") + " " + STRING(ENTRY(5,bDL.Txt,"~n"),"x(6)"))) + "│" +
            STRING(DEC(ENTRY(1,mAmt,"~n")),">>,>>>,>>9.99") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[1],">>,>>>,>>9.99")) + "│" +
            STRING(bDL.Val[2],">>,>>>,>>9.99") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[3],">>,>>>,>>9.99")) + "│" +
            STRING(DataLine.Val[4],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[5],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[7],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[8],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[9],">>,>>>,>>9.99") + "│"
            SKIP
         .
         IF MAX(NUM-ENTRIES(mNamePostav,"~n"),
            NUM-ENTRIES(mNums,"~n"),
            NUM-ENTRIES(mAmt,"~n")) GE 2 
         THEN DO i = 2 TO MAX(NUM-ENTRIES(mNamePostav,"~n"),
                              NUM-ENTRIES(mNums,"~n"),
                              NUM-ENTRIES(mAmt,"~n")):
            PUT STREAM sfact UNFORMATTED
               "│" +
               STRING("   ") + "│" +
               STRING((IF i LE NUM-ENTRIES(mNums,"~n") 
                                THEN ENTRY(i,mNums,"~n") 
                                ELSE " "),"x(15)") + "│" +
               STRING(" ","x(10)") + "│" +
               STRING(" ","x(10)") + "│" +
               STRING((IF i LE NUM-ENTRIES(mNamePostav,"~n") 
                                THEN ENTRY(i,mNamePostav,"~n") 
                                ELSE " "),"x(21)") + 
               "│              │              │                 │" + 
               STRING((IF i LE NUM-ENTRIES(mAmt,"~n") 
                         THEN ENTRY(i,mAmt,"~n") 
                         ELSE " "),"x(13)") +
               "│             │             │             │             │             │             │             │             │"                  
               SKIP
            .
         END.
         ACCUMULATE DataLine.Val[11] (TOTAL).
         ACCUMULATE DataLine.Val[1] (TOTAL).
         ACCUMULATE DataLine.Val[2] (TOTAL).
         ACCUMULATE DataLine.Val[3] (TOTAL).
         ACCUMULATE DataLine.Val[4] (TOTAL).
         ACCUMULATE DataLine.Val[5] (TOTAL).
         ACCUMULATE DataLine.Val[7] (TOTAL).
         ACCUMULATE DataLine.Val[8] (TOTAL).
         ACCUMULATE DataLine.Val[9] (TOTAL).  

         IF NOT LAST-OF(DataLine.Data-ID) 
            THEN PUT STREAM sfact UNFORMATTED
               "├───┼───────────────┼──────────┼──────────┼─────────────────────┼──────────────┼──────────────┼─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
            .
      END.
   END.
   ASSIGN
      iVal_a[1] = ACCUM TOTAL DataLine.Val[11]
      iVal_a[2] = ACCUM TOTAL DataLine.Val[1]
      iVal_a[3] = ACCUM TOTAL DataLine.Val[2]
      iVal_a[4] = ACCUM TOTAL DataLine.Val[3]
      iVal_a[5] = ACCUM TOTAL DataLine.Val[4]
      iVal_a[6] = ACCUM TOTAL DataLine.Val[5]
      iVal_a[7] = ACCUM TOTAL DataLine.Val[7]
      iVal_a[8] = ACCUM TOTAL DataLine.Val[8]
      iVal_a[9] = ACCUM TOTAL DataLine.Val[9]
   .
   
   /* Печатаем Итого исправлено */

   PUT STREAM sfact UNFORMATTED
      "├───┴───────────────┴──────────┴──────────┴─────────────────────┴──────────────┴──────────────┴─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│                                                                                            Итого аннулировано:│" + 
      STRING(ABS(iVal_a[1]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[2]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[3]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[4]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[5]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[6]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[7]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[8]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_a[9]),">>,>>>,>>9.99") + "│" SKIP
      "├───┬───────────────┬──────────┬──────────┬─────────────────────┬──────────────┬──────────────┬─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   .

   mStrNum = 0.
   FOR EACH bDL WHERE bDL.Data-Id EQ INT64(INDATABLOCK)
   NO-LOCK,

   FIRST DataLine WHERE DataLine.Data-ID EQ bDL.Data-Id
                    AND DataLine.Sym1    EQ bDL.Sym1
                    AND DataLine.Sym2    EQ bDL.Sym2
                    AND DataLine.Sym3    EQ bDL.Sym3
                    AND DataLine.Sym4    EQ bDL.Sym4
   NO-LOCK
      BREAK BY DataLine.Data-ID:
      
      IF NUM-ENTRIES(DataLine.Txt,"~n") GT 12
         AND ENTRY(13,DataLine.Txt,"~n") EQ "ДА" THEN
      DO:
         ASSIGN
            mStrNum     = mStrNum + 1
            mNamePostav =  SplitStr(ENTRY(1,DataLine.Txt,"~n"),
                                          21,
                                          "~n")
   
            mNums       =  SplitStr(ENTRY(6,DataLine.Txt,"~n") + " " 
                                  + ENTRY(8,DataLine.Txt,"~n"),
                                          15,
                                          "~n")
      
            mAmt        =  SplitStr(STRING(DataLine.Val[11],">>,>>>,>>9.99") + " " 
                                  + ENTRY(7,DataLine.Txt,"~n"),
                                    13,
                                    "~n")
            vAdvance    = ENTRY(4,bDL.Txt,"~n") EQ "Аванс"   
         .
   
         PUT STREAM sfact UNFORMATTED
            "│" +
            STRING(mStrNum,"999") + "│" +
            STRING(ENTRY(1,mNums,"~n"),"x(15)") + "│" +
            STRING(DataLine.Sym2,"x(10)") + "│" +
            STRING(DataLine.Sym1,"x(10)") + "│" +
            STRING(ENTRY(1,mNamePostav,"~n"),"x(21)") + "│" +
            STRING(ENTRY(2,DataLine.Txt,"~n"),"x(14)") + "│" +
            STRING(ENTRY(3,DataLine.Txt,"~n"),"x(14)") + "│" +
            STRING(ENTRY(4,DataLine.Txt,"~n"),"x(10)") + " " + 
            STRING(ENTRY(5,DataLine.Txt,"~n"),"x(6)") + "│" +
            STRING(DEC(ENTRY(1,mAmt,"~n")),">>,>>>,>>9.99") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[1],">>>>,>>>,>>9.99")) + "│" +
            STRING(bDL.Val[2],">>>>,>>>,>>9.99") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[3],">>,>>>,>>9.99")) + "│" +
            STRING(DataLine.Val[4],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[5],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[7],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[8],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[9],">>,>>>,>>9.99") + "│"
            SKIP
         .
         IF MAX(NUM-ENTRIES(mNamePostav,"~n"),
            NUM-ENTRIES(mNums,"~n"),
            NUM-ENTRIES(mAmt,"~n")) GE 2 
         THEN DO i = 2 TO MAX(NUM-ENTRIES(mNamePostav,"~n"),
                              NUM-ENTRIES(mNums,"~n"),
                              NUM-ENTRIES(mAmt,"~n")):
            PUT STREAM sfact UNFORMATTED
               "│" +
               STRING("   ") + "│" +
               STRING((IF i LE NUM-ENTRIES(mNums,"~n") 
                                THEN ENTRY(i,mNums,"~n") 
                                ELSE " "),"x(15)") + "│" +
               STRING(" ","x(10)") + "│" +
               STRING(" ","x(10)") + "│" +
               STRING((IF i LE NUM-ENTRIES(mNamePostav,"~n") 
                                THEN ENTRY(i,mNamePostav,"~n") 
                                ELSE " "),"x(21)") + 
               "│              │              │                 │" + 
               STRING((IF i LE NUM-ENTRIES(mAmt,"~n") 
                         THEN ENTRY(i,mAmt,"~n") 
                         ELSE " "),"x(13)") +
               "│             │             │             │             │             │             │             │             │"                  
               SKIP
            .
         END.
         ACCUMULATE DataLine.Val[11] (TOTAL).
         ACCUMULATE DataLine.Val[1] (TOTAL).
         ACCUMULATE DataLine.Val[2] (TOTAL).
         ACCUMULATE DataLine.Val[3] (TOTAL).
         ACCUMULATE DataLine.Val[4] (TOTAL).
         ACCUMULATE DataLine.Val[5] (TOTAL).
         ACCUMULATE DataLine.Val[7] (TOTAL).
         ACCUMULATE DataLine.Val[8] (TOTAL).
         ACCUMULATE DataLine.Val[9] (TOTAL).  

         IF NOT LAST-OF(DataLine.Data-ID) 
            THEN PUT STREAM sfact UNFORMATTED
               "├───┼───────────────┼──────────┼──────────┼─────────────────────┼──────────────┼──────────────┼─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
            .
      END.
   END.
   ASSIGN
      iVal_d[1] = ACCUM TOTAL DataLine.Val[11]
      iVal_d[2] = ACCUM TOTAL DataLine.Val[1]
      iVal_d[3] = ACCUM TOTAL DataLine.Val[2]
      iVal_d[4] = ACCUM TOTAL DataLine.Val[3]
      iVal_d[5] = ACCUM TOTAL DataLine.Val[4]
      iVal_d[6] = ACCUM TOTAL DataLine.Val[5]
      iVal_d[7] = ACCUM TOTAL DataLine.Val[7]
      iVal_d[8] = ACCUM TOTAL DataLine.Val[8]
      iVal_d[9] = ACCUM TOTAL DataLine.Val[9]
   .

   /* Печатаем Итого добавлено */

   PUT STREAM sfact UNFORMATTED
      "├───┴───────────────┴──────────┴──────────┴─────────────────────┴──────────────┴──────────────┴─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│                                                                                               Итого добавлено:│" + 
      STRING(iVal_d[1],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[2],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[3],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[4],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[5],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[6],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[7],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[8],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[9],">>,>>>,>>9.99") + "│" SKIP
      "├───┬───────────────┬──────────┬──────────┬─────────────────────┬──────────────┬──────────────┬─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   .

   /*Рассчитываем вывод*/
   DO ii = 1 TO 9:
      iVal_m[ii] = iVal_m[ii] - iVal_a[ii] + iVal_d[ii].
   END.

   PUT STREAM sfact UNFORMATTED
      "├───┴───────────────┴──────────┴──────────┴─────────────────────┴──────────────┴──────────────┴─────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│                                                                                                         Всего:│" + 
      STRING(iVal_m[1],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[2],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[3],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[4],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[5],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[6],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[7],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[8],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[9],">>,>>>,>>9.99") + "│" SKIP
      "└───────────────────────────────────────────────────────────────────────────────────────────────────────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┘" SKIP
   .
   
   {signatur.i
      &stream="STREAM sfact"}
END PROCEDURE.


/*Печать дополнительных листов книги продаж*/
PROCEDURE SellAddPage:
   DEFINE INPUT PARAMETER inDataBlock  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inPagenumber AS INT64     NO-UNDO. /*Номер страницы*/
       /*Итого по счет-фактурам*/
   DEFINE VARIABLE iVal_m     AS DEC     NO-UNDO EXTENT 9. /*не аннулирован.*/
   DEFINE VARIABLE iVal_a     AS DEC     NO-UNDO EXTENT 9. /*аннулирован. */
   DEFINE VARIABLE iVal_i     AS DEC     NO-UNDO EXTENT 9. /*Исправительным*/
   DEFINE VARIABLE iVal_i-a   AS DEC     NO-UNDO EXTENT 9. /*сумма. */
   DEFINE VARIABLE iVal_d     AS DEC     NO-UNDO EXTENT 9. /*Итого по счет-фактурам с ДР ДопЛист */ 
   DEFINE VARIABLE iValStr    AS CHAR    NO-UNDO.
   DEFINE VARIABLE vAdvance   AS LOGICAL NO-UNDO.

   DEFINE VARIABLE ii     AS INT64 NO-UNDO.

   DEFINE BUFFER iTmpDataline FOR TmpDataLine. 
   /*Расчёт Итого для счет-фактур (не аннулированных) */
   RUN CalcTotalsIn(inDataBlock, INPUT-OUTPUT iVal_m).
   FOR EACH DataLine
   WHERE DataLine.Data-ID EQ INT64(inDataBlock)
   NO-LOCK:

      /*Расчёт суммы всех аннулированных и исправительных счет-фактур*/
      IF NUM-ENTRIES(DataLine.Txt,"~n") GE 9 THEN
      DO:           
         IF      ENTRY(9,DataLine.Txt,"~n") EQ "Исправ" THEN
         DO:         
            CREATE iTmpDataLine.
            BUFFER-COPY DataLine TO iTmpDataLine.
            ASSIGN
               itmpDataLine.isprav    = "Исправ"
               itmpDataLine.cont-code = IF NUM-ENTRIES(DataLine.Txt,"~n") GT 9 THEN 
                                          IF NUM-ENTRIES(ENTRY(10,DataLine.Txt,"~n")) GE 2 THEN 
                                             ENTRY(2,ENTRY(10,DataLine.Txt,"~n")) 
                                          ELSE ""
                                       ELSE ""         
            .
         END.
         ELSE IF ENTRY(9,DataLine.Txt,"~n") EQ "Аннулир" THEN
         DO:
            CREATE tmpDataLine.
            BUFFER-COPY DataLine TO tmpDataLine.
            ASSIGN 
               tmpDataLine.isprav    = "Аннулир"
               iVal_a[1]             = iVal_a[1] +  DataLine.Val[11]
               iVal_a[2]             = iVal_a[2] +  DataLine.Val[1]
               iVal_a[3]             = iVal_a[3] +  DataLine.Val[2]
               iVal_a[4]             = iVal_a[4] +  DataLine.Val[3]
               iVal_a[5]             = iVal_a[5] +  DataLine.Val[4]
               iVal_a[6]             = iVal_a[6] +  DataLine.Val[5]
               iVal_a[7]             = iVal_a[7] +  DataLine.Val[7]
               iVal_a[8]             = iVal_a[8] +  DataLine.Val[8]
               iVal_a[9]             = iVal_a[9] +  DataLine.Val[9]
            .
         END.
      END.
   END.

   PUT STREAM sfact UNFORMATTED
      FILL(" ",40)  + "ДОПОЛНИТЕЛЬНЫЙ ЛИСТ К КНИГЕ ПРОДАЖ №" + 
      STRING(inPagenumber) SKIP(3)
      "Продавец " + STRING(mBuyer,"x(35)")                 SKIP
      "         " + FILL("─",35)                           SKIP(2)
      "Идентификационный номер и код причины постановки на учет налогоплательщика-продавца " + TRIM(STRING(mINN)) + "/" + STRING(mKPP,"x(15)") SKIP
      "                                                                                    " + FILL("─",35) SKIP(2)
      "Налоговый период (месяц, квартал), год, в котором зарегистрирован счет-фактура до внесения в него исправления: " 
       term2str(bdb.Beg-Date,bdb.End-Date) SKIP
      "                                                                                                               ────────────────" SKIP(2)
   
      "Дополнительный лист оформлен " STRING(TODAY,"99/99/9999") SKIP
      "                             ──────────" SKIP(3)
   .
/*ШАПКА ТАБЛИЧКИ*/
PUT STREAM sfact UNFORMATTED
   "┌───────────────┬───────────────┬───────────────┬───────────────┬─────────────────────┬────────────┬────────────┬────────┬───────────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐" SKIP
   "│ Дата и номер  │ Номер и дата  │ Номер и дата  │ Номер и дата  │    Наименование     │    ИНН     │    КПП     │ Дата   │ Всего продаж  │                                          В том числе                                                              │" SKIP
   "│ счета-фактуры │ исправления   │ корректиро-   │ исправления   │     покупателя      │ покупателя │ покупателя │оплаты  │  включая НДС  ├─────────────────────────────────────────────────────────────────────────────────────────────────────┬─────────────┤" SKIP
   "│   продавца    │ счета-фактуры │ вочного       │ корректиро-   │                     │            │            │счета-  │               │                              продажи, облагаемые налогом по ставке                                  │  продажи,   │" SKIP
   "│               │               │ счета-фактуры │ вочного       │                     │            │            │фактуры │               ├───────────────────────────────┬───────────────────────────┬─────────────┬───────────────────────────┤освобождаемые│" SKIP
   "│               │               │               │ счета-фактуры │                     │            │            │продавца│               │         18 процентов          │       10 процентов        │ 0 процентов │       20 процентов*       │  от налога  │" SKIP
   "│               │               │               │               │                     │            │            │        │               ├───────────────┬───────────────┼─────────────┬─────────────┼─────────────┼─────────────┬─────────────┤             │" SKIP
   "│               │               │               │               │                     │            │            │        │               │ стоимость пр- │  сумма НДС    │стоимость пр-│ сумма НДС   │             │стоимость пр-│ сумма НДС   │             │" SKIP
   "│               │               │               │               │                     │            │            │        │               │ одаж без НДС  │               │одаж без НДС │             │             │одаж без НДС │             │             │" SKIP
   "├───────────────┼───────────────┼───────────────┼───────────────┼─────────────────────┼────────────┼────────────┼────────┼───────────────┼───────────────┼───────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   "│      1        │      1а       │      1б       │      1в       │          2          │     3      │     3а     │   3б   │       4       │       5а      │       5б      │      6а     │      6б     │      7      │      8а     │      8б     │      9      │" SKIP
/*
   "├───────────────┼───────────────┼───────────────┼───────────────┼─────────────────────┼────────────┼────────────┼────────┼───────────────┼───────────────┼───────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
*/
.
   /*ИТОГО*/
   PUT STREAM sfact UNFORMATTED
   "├───────────────┴───────────────┴───────────────┴───────────────┴─────────────────────┴────────────┴────────────┴────────┼───────────────┼───────────────┼───────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   "│                                                                                                                  ИТОГО:│" + 
      STRING(iVal_m[1],">>>>,>>>,>>9.99") + "│" +
      STRING(iVal_m[2],">>>>,>>>,>>9.99") + "│" +
      STRING(iVal_m[3],">>>>,>>>,>>9.99") + "│" +
      STRING(iVal_m[4],">,>>>,>>9.99") + "│" +
      STRING(iVal_m[5],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[6],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[7],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[8],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[9],">>,>>>,>>9.99") + "│" SKIP
   "├───────────────┬───────────────┬───────────────┬───────────────┬─────────────────────┬────────────┬────────────┬────────┼───────────────┼───────────────┼───────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   .
   FOR EACH tmpDataLine WHERE tmpDataLine.isprav EQ "Аннулир"
   NO-LOCK,

   FIRST bDL WHERE bDL.Data-ID EQ tmpDataLine.Data-Id
               AND bDL.Sym1    EQ tmpDataLine.Sym1
               AND bDL.Sym2    EQ tmpDataLine.Sym2
               AND bDL.Sym3    EQ tmpDataLine.Sym3
               AND bDL.Sym4    EQ tmpDataLine.Sym4
   NO-LOCK
      BREAK BY tmpDataLine.Data-ID:

      ASSIGN
         mNamePostav =  SplitStr(ENTRY(1,tmpDataLine.Txt,"~n"),
                                 21,
                                "~n")
   
         mNums       =  SplitStr(ENTRY(6,tmpDataLine.Txt,"~n") + " " 
                               + ENTRY(8,tmpDataLine.Txt,"~n"),
                                 15,
                                 "~n")
         mAmt        = SplitStr(STRING(tmpDataLine.Val[11],">>,>>>,>>9.99") + " " 
                              + ENTRY(7,tmpDataLine.Txt,"~n"),
                                13,
                                "~n")
         
         vAdvance    = ENTRY(4,bDL.Txt,"~n") EQ "Аванс"
      .

      PUT STREAM sfact UNFORMATTED
         "│" +
         STRING(ENTRY(1,mNums,"~n"),"x(15)") + "│" +
         STRING(ENTRY(1,mNamePostav,"~n"),"x(21)") + "│" +
         STRING(ENTRY(2,tmpDataLine.Txt,"~n"),"x(14)") + "│" +
         STRING(ENTRY(3,tmpDataLine.Txt,"~n"),"x(14)") + "│" +
         STRING(tmpDataLine.Sym2,"x(10)") + "│" +
         STRING(DEC(ENTRY(1,mAmt,"~n")),">>,>>>,>>9.99") + "│" +
         (IF vAdvance 
          THEN (" " + FILL("-",11) + " ")
          ELSE STRING(bDL.Val[1],">>,>>>,>>9.99")) + "│" +
         STRING(bDL.Val[2],">>,>>>,>>9.99") + "│" +
         (IF vAdvance 
          THEN (" " + FILL("-",11) + " ")
          ELSE STRING(bDL.Val[3],">>,>>>,>>9.99")) + "│" +
         STRING(tmpDataLine.Val[4],">>,>>>,>>9.99") + "│" +
         STRING(tmpDataLine.Val[5],">>,>>>,>>9.99") + "│" +
         STRING(tmpDataLine.Val[7],">>,>>>,>>9.99") + "│" +
         STRING(tmpDataLine.Val[8],">>,>>>,>>9.99") + "│" +
         STRING(tmpDataLine.Val[9],">>,>>>,>>9.99") + "│"
         SKIP
      .
   
      IF MAX(NUM-ENTRIES(mNamePostav,"~n"),
         NUM-ENTRIES(mNums,"~n"),
         NUM-ENTRIES(mAmt,"~n")) GE 2 
      THEN DO i = 2 TO MAX(NUM-ENTRIES(mNamePostav,"~n"),
                           NUM-ENTRIES(mNums,"~n"),
                           NUM-ENTRIES(mAmt,"~n")):
         PUT STREAM sfact UNFORMATTED
            "│" +
            STRING((IF i LE NUM-ENTRIES(mNums,"~n") 
                    THEN ENTRY(i,mNums,"~n") 
                    ELSE " "),"x(15)") + "│" +
            STRING((IF i LE NUM-ENTRIES(mNamePostav,"~n") 
                    THEN ENTRY(i,mNamePostav,"~n") 
                    ELSE " "),"x(21)") + 
            "│              │              │          │" + 
            STRING((IF i LE NUM-ENTRIES(mAmt,"~n") 
                     THEN ENTRY(i,mAmt,"~n") 
                     ELSE " "),"x(13)") + 
            "│             │             │             │             │             │             │             │             │" SKIP
            SKIP
         .
      END.

      /*Отбираем все счет-фактуры исправляющие данный счет-фактуру*/            
      FOR EACH iTmpDataLine WHERE iTmpDataLine.cont-code EQ ENTRY(11,TmpDataLine.Txt,"~n") NO-LOCK BREAK BY iTmpDataLine.Data-ID:
         
         PUT STREAM sfact UNFORMATTED
            "├───────────────┼─────────────────────┼──────────────┼──────────────┼──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
         .

         mNamePostav =  SplitStr(ENTRY(1,iTmpDataLine.Txt,"~n"),
                                 21,
                                "~n").
         mNums       =  SplitStr(ENTRY(6,iTmpDataLine.Txt,"~n") + " " 
                               + ENTRY(8,iTmpDataLine.Txt,"~n"),
                                 15,
                                 "~n").
         mAmt = SplitStr(STRING(iTmpDataLine.Val[11],">>,>>>,>>9.99") + " " 
                              + ENTRY(7,iTmpDataLine.Txt,"~n"),
                                13,
                                "~n").

         PUT STREAM sfact UNFORMATTED
            "│" +
            STRING(ENTRY(1,mNums,"~n"),"x(15)") + "│" +
            STRING(ENTRY(1,mNamePostav,"~n"),"x(21)") + "│" +
            STRING(ENTRY(2,iTmpDataLine.Txt,"~n"),"x(14)") + "│" +
            STRING(ENTRY(3,iTmpDataLine.Txt,"~n"),"x(14)") + "│" +
            STRING(iTmpDataLine.Sym2,"x(10)") + "│" +
            STRING(ENTRY(1,mAmt,"~n"),"x(13)") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[1],">>,>>>,>>9.99")) + "│" +
            STRING(bDL.Val[2],">>,>>>,>>9.99") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[3],">>,>>>,>>9.99")) + "│" +
            STRING(iTmpDataLine.Val[4],">>,>>>,>>9.99") + "│" +
            STRING(iTmpDataLine.Val[5],">>,>>>,>>9.99") + "│" +
            STRING(iTmpDataLine.Val[7],">>,>>>,>>9.99") + "│" +
            STRING(iTmpDataLine.Val[8],">>,>>>,>>9.99") + "│" +
            STRING(iTmpDataLine.Val[9],">>,>>>,>>9.99") + "│"
            SKIP
         .  
         ASSIGN 
            iVal_i[1] = iVal_i[1] +  iTmpDataLine.Val[11]
            iVal_i[2] = iVal_i[2] +  iTmpDataLine.Val[1]
            iVal_i[3] = iVal_i[3] +  iTmpDataLine.Val[2]
            iVal_i[4] = iVal_i[4] +  iTmpDataLine.Val[3]
            iVal_i[5] = iVal_i[5] +  iTmpDataLine.Val[4]
            iVal_i[6] = iVal_i[6] +  iTmpDataLine.Val[5]
            iVal_i[7] = iVal_i[7] +  iTmpDataLine.Val[7]
            iVal_i[8] = iVal_i[8] +  iTmpDataLine.Val[8]
            iVal_i[9] = iVal_i[9] +  iTmpDataLine.Val[9]
         .
         IF MAX(NUM-ENTRIES(mNamePostav,"~n"),
            NUM-ENTRIES(mNums,"~n"),
            NUM-ENTRIES(mAmt,"~n")) GE 2 
         THEN DO i = 2 TO MAX(NUM-ENTRIES(mNamePostav,"~n"),
                              NUM-ENTRIES(mNums,"~n"),
                              NUM-ENTRIES(mAmt,"~n")):
            PUT STREAM sfact UNFORMATTED
               "│" +
               STRING((IF i LE NUM-ENTRIES(mNums,"~n") 
                       THEN ENTRY(i,mNums,"~n") 
                       ELSE " "),"x(15)") + "│" +
               STRING((IF i LE NUM-ENTRIES(mNamePostav,"~n") 
                       THEN ENTRY(i,mNamePostav,"~n") 
                       ELSE " "),"x(21)") + 
               "│              │              │          │" + 
               STRING((IF i LE NUM-ENTRIES(mAmt,"~n") 
                     THEN ENTRY(i,mAmt,"~n") 
                     ELSE " "),"x(13)") + 
               "│             │             │             │             │             │             │             │             │" SKIP
               SKIP
            .

         END.
         IF NOT LAST-OF(iTmpDataLine.Data-ID) THEN 
            PUT STREAM sfact UNFORMATTED
               "├───────────────┼─────────────────────┼──────────────┼──────────────┼──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
            .

      END.
      IF NOT LAST-OF(TmpDataLine.Data-ID) THEN 
         PUT STREAM sfact UNFORMATTED
            "├───────────────┼─────────────────────┼──────────────┼──────────────┼──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
         .
   END.

   /*Рассчитываем вывод*/
   DO ii = 1 TO 9:
      iVal_i-a[ii] = iVal_i[ii] - iVal_a[ii].
   END.

   /* Печатаем Итого исправлено */

   PUT STREAM sfact UNFORMATTED
      "├───────────────┴─────────────────────┴──────────────┴──────────────┴──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│                                                             Итого исправлено:│" + 
      STRING(ABS(iVal_i-a[1]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[2]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[3]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[4]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[5]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[6]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[7]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[8]),">>,>>>,>>9.99") + "│" +
      STRING(ABS(iVal_i-a[9]),">>,>>>,>>9.99") + "│" SKIP
      "├───────────────┬─────────────────────┬──────────────┬──────────────┬──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   .

   mStrNum = 0.

   FOR EACH bDL WHERE bDL.Data-Id EQ INT64(INDATABLOCK)
   NO-LOCK,

   FIRST DataLine WHERE DataLine.Data-ID EQ bDL.Data-Id
                    AND DataLine.Sym1    EQ bDL.Sym1
                    AND DataLine.Sym2    EQ bDL.Sym2
                    AND DataLine.Sym3    EQ bDL.Sym3
                    AND DataLine.Sym4    EQ bDL.Sym4
   NO-LOCK
      BREAK BY DataLine.Data-ID:
      
      IF NUM-ENTRIES(DataLine.Txt,"~n") GT 12
         AND ENTRY(13,DataLine.Txt,"~n") EQ "ДА" THEN
      DO:
         ASSIGN
            mStrNum     = mStrNum + 1
            mNamePostav =  SplitStr(ENTRY(1,DataLine.Txt,"~n"),
                                          21,
                                          "~n")
   
            mNums       =  SplitStr(ENTRY(6,DataLine.Txt,"~n") + " " 
                                  + ENTRY(8,DataLine.Txt,"~n"),
                                          15,
                                          "~n")
      
            mAmt        =  SplitStr(STRING(DataLine.Val[11],">>,>>>,>>9.99") + " " 
                                  + ENTRY(7,DataLine.Txt,"~n"),
                                    13,
                                    "~n")
            vAdvance    = ENTRY(4,bDL.Txt,"~n") EQ "Аванс"   
         .

         PUT STREAM sfact UNFORMATTED
            "│" +
            STRING(ENTRY(1,mNums,"~n"),"x(15)") + "│" +
            STRING(ENTRY(1,mNamePostav,"~n"),"x(21)") + "│" +
            STRING(ENTRY(2,DataLine.Txt,"~n"),"x(14)") + "│" +
            STRING(ENTRY(3,DataLine.Txt,"~n"),"x(14)") + "│" +
            STRING(DataLine.Sym2,"x(10)") + "│" +
            STRING(DEC(ENTRY(1,mAmt,"~n")),">>,>>>,>>9.99") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[1],">>,>>>,>>9.99")) + "│" +
            STRING(bDL.Val[2],">>,>>>,>>9.99") + "│" +
            (IF vAdvance 
             THEN (" " + FILL("-",11) + " ")
             ELSE STRING(bDL.Val[3],">>,>>>,>>9.99")) + "│" +
            STRING(DataLine.Val[4],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[5],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[7],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[8],">>,>>>,>>9.99") + "│" +
            STRING(DataLine.Val[9],">>,>>>,>>9.99") + "│"
            SKIP
         .
         
         IF MAX(NUM-ENTRIES(mNamePostav,"~n"),
            NUM-ENTRIES(mNums,"~n"),
            NUM-ENTRIES(mAmt,"~n")) GE 2 
         THEN DO i = 2 TO MAX(NUM-ENTRIES(mNamePostav,"~n"),
                              NUM-ENTRIES(mNums,"~n"),
                              NUM-ENTRIES(mAmt,"~n")):
            PUT STREAM sfact UNFORMATTED
               "│" +
               STRING((IF i LE NUM-ENTRIES(mNums,"~n") 
                       THEN ENTRY(i,mNums,"~n") 
                       ELSE " "),"x(15)") + "│" +
               STRING((IF i LE NUM-ENTRIES(mNamePostav,"~n") 
                       THEN ENTRY(i,mNamePostav,"~n") 
                       ELSE " "),"x(21)") + 
               "│              │              │          │" + 
               STRING((IF i LE NUM-ENTRIES(mAmt,"~n") 
                     THEN ENTRY(i,mAmt,"~n") 
                     ELSE " "),"x(13)") + 
               "│             │             │             │             │             │             │             │             │" SKIP
               SKIP
            .

         END.
         
         ACCUMULATE DataLine.Val[11] (TOTAL).
         ACCUMULATE DataLine.Val[1] (TOTAL).
         ACCUMULATE DataLine.Val[2] (TOTAL).
         ACCUMULATE DataLine.Val[3] (TOTAL).
         ACCUMULATE DataLine.Val[4] (TOTAL).
         ACCUMULATE DataLine.Val[5] (TOTAL).
         ACCUMULATE DataLine.Val[7] (TOTAL).
         ACCUMULATE DataLine.Val[8] (TOTAL).
         ACCUMULATE DataLine.Val[9] (TOTAL).  

         IF NOT LAST-OF(DataLine.Data-ID) THEN
            PUT STREAM sfact UNFORMATTED
               "├───────────────┼─────────────────────┼──────────────┼──────────────┼──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
            .
      END.
   END.
   ASSIGN
      iVal_d[1] = ACCUM TOTAL DataLine.Val[11]
      iVal_d[2] = ACCUM TOTAL DataLine.Val[1]
      iVal_d[3] = ACCUM TOTAL DataLine.Val[2]
      iVal_d[4] = ACCUM TOTAL DataLine.Val[3]
      iVal_d[5] = ACCUM TOTAL DataLine.Val[4]
      iVal_d[6] = ACCUM TOTAL DataLine.Val[5]
      iVal_d[7] = ACCUM TOTAL DataLine.Val[7]
      iVal_d[8] = ACCUM TOTAL DataLine.Val[8]
      iVal_d[9] = ACCUM TOTAL DataLine.Val[9]
   .

   /* Печатаем Итого добавлено */

   PUT STREAM sfact UNFORMATTED
      "├───────────────┴─────────────────────┴──────────────┴──────────────┴──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│                                                              Итого добавлено:│" + 
      STRING(iVal_d[1],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[2],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[3],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[4],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[5],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[6],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[7],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[8],">>,>>>,>>9.99") + "│" +
      STRING(iVal_d[9],">>,>>>,>>9.99") + "│" SKIP
      "├───────────────┬─────────────────────┬──────────────┬──────────────┬──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
   .

   /*Рассчитываем вывод*/
   DO ii = 1 TO 9:
      iVal_m[ii] = iVal_m[ii] + iVal_i-a[ii] + iVal_d[ii].
   END.

   PUT STREAM sfact UNFORMATTED
      "├───────────────┴─────────────────────┴──────────────┴──────────────┴──────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤" SKIP
      "│                                                                        Всего:│" + 
      STRING(iVal_m[1],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[2],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[3],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[4],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[5],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[6],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[7],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[8],">>,>>>,>>9.99") + "│" +
      STRING(iVal_m[9],">>,>>>,>>9.99") + "│" SKIP
      "└──────────────────────────────────────────────────────────────────────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┘" SKIP
   .
   &UNDEFINE signatur_i
   {signatur.i
      &stream="STREAM sfact"}
END PROCEDURE.


/*Расчёт итого (суммы счетов-фактур)*/
PROCEDURE CalcTotals:
   DEFINE INPUT  PARAMETER inDataBlock AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oValStr     AS CHARACTER NO-UNDO. /*Сумма
                                                              счет-фактур*/
   DEF VAR vVal_m      AS DECIMAL   NO-UNDO EXTENT 9. /*Сумма
                                                        счет-фактур*/

   RUN CalcTotalsIn(inDataBlock,INPUT-OUTPUT vVal_m).
   
   oValStr = STRING(vVal_m[1]) + "," + STRING(vVal_m[2]) + "," + STRING(vVal_m[3]) + "," + 
             STRING(vVal_m[4]) + "," + STRING(vVal_m[5]) + "," + STRING(vVal_m[6]) + "," + 
             STRING(vVal_m[7]) + "," + STRING(vVal_m[8]) + "," + STRING(vVal_m[9]).

END PROCEDURE.

/*Расчёт итого (суммы счетов-фактур)*/
PROCEDURE CalcTotalsIn:
   DEF INPUT        PARAM inDataBlock AS CHAR NO-UNDO.
   DEF INPUT-OUTPUT PARAM vVal_m      AS DEC  NO-UNDO EXTENT 9. /*Сумма счет-фактур*/
   
   FOR EACH DataLine
      WHERE DataLine.Data-ID EQ INT64(INDATABLOCK)
      NO-LOCK:
      IF NUM-ENTRIES(DataLine.Txt,"~n") < 9 OR ENTRY(9,DataLine.Txt,"~n") <> "Исправ" AND NUM-ENTRIES(DataLine.Txt,"~n") GT 12 AND ENTRY(13,DataLine.Txt,"~n") EQ "" THEN
      DO:
         ACCUMULATE DataLine.Val[11] (TOTAL).
         ACCUMULATE DataLine.Val[1] (TOTAL).
         ACCUMULATE DataLine.Val[2] (TOTAL).
         ACCUMULATE DataLine.Val[3] (TOTAL).
         ACCUMULATE DataLine.Val[4] (TOTAL).
         ACCUMULATE DataLine.Val[5] (TOTAL).
         ACCUMULATE DataLine.Val[7] (TOTAL).
         ACCUMULATE DataLine.Val[8] (TOTAL).
         ACCUMULATE DataLine.Val[9] (TOTAL).
      END.
   END.
   ASSIGN
      vVal_m[1] = ACCUM TOTAL DataLine.Val[11]
      vVal_m[2] = ACCUM TOTAL DataLine.Val[1]
      vVal_m[3] = ACCUM TOTAL DataLine.Val[2]
      vVal_m[4] = ACCUM TOTAL DataLine.Val[3]
      vVal_m[5] = ACCUM TOTAL DataLine.Val[4]
      vVal_m[6] = ACCUM TOTAL DataLine.Val[5]
      vVal_m[7] = ACCUM TOTAL DataLine.Val[7]
      vVal_m[8] = ACCUM TOTAL DataLine.Val[8]
      vVal_m[9] = ACCUM TOTAL DataLine.Val[9]
   .
END PROCEDURE.

procedure EndStrike.
   DEFINE INPUT  PARAM iNumStrike   AS INT64   NO-UNDO.   /* Номер строки в отчете */
   DEFINE INPUT  PARAM iNumPosition AS INT64   NO-UNDO.   /* Позиция отображения строки "Страница" */
   DEFINE INPUT  PARAM iLastPage    AS LOGICAL NO-UNDO.   /* Печать номнра стр. на последней странице */
   DEFINE OUTPUT PARAM oNumStrike   AS INT64   NO-UNDO.   /* Номер строки в отчете */

   PUT STREAM sfact UNFORMATTED " " SKIP(printer.page-lines - (iNumStrike + 1)).
   PUT STREAM sfact UNFORMATTED "Стр. " AT printer.page-cols + iNumPosition
                    STRING(PAGE-NUMBER(sfact)) AT printer.page-cols + 5 + iNumPosition SKIP.
   IF iLastPage THEN PAGE STREAM sfact.
   iNumStrike = 0.
   oNumStrike = iNumStrike.
end procedure.

/* Процедура подсчета строк и заполнения номера страниц */
PROCEDURE CalcStrike.
   DEFINE INPUT  PARAM iNumStrike   AS INT64   NO-UNDO.   /* Номер строки в отчете */
   DEFINE INPUT  PARAM iNumPosition AS INT64   NO-UNDO.   /* Позиция отображения строки "Страница" */
   DEFINE INPUT  PARAM iLastPage    AS LOGICAL NO-UNDO.   /* Печать номнра стр. на последней странице */
   DEFINE OUTPUT PARAM oNumStrike   AS INT64   NO-UNDO.   /* Номер строки в отчете */

   IF iLastPage THEN DO:
      PUT STREAM sfact UNFORMATTED " " SKIP(printer.page-lines - (iNumStrike + 5)).
      PUT STREAM sfact UNFORMATTED "Стр. " AT printer.page-cols + iNumPosition
                       STRING(PAGE-NUMBER(sfact)) AT printer.page-cols + 5 + iNumPosition SKIP.
      iNumStrike = 0.
   END.
   IF iNumStrike + 1 EQ printer.page-lines THEN DO:
      PUT STREAM sfact UNFORMATTED "Стр. " AT printer.page-cols + iNumPosition
                       STRING(PAGE-NUMBER(sfact)) AT printer.page-cols + 5 + iNumPosition SKIP.
      PAGE STREAM sfact.
      iNumStrike = 0.
   END.
   ELSE oNumStrike = iNumStrike.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='21/05/2015 13:50:22.732+04:00' */
/* $LINTUSER='stre' */
/* $LINTMODE='1' */
/* $LINTFILE='bookprn-new.i' */
/*prosignHSuyrOQ+UPhgk1uG8wgzLQ*/