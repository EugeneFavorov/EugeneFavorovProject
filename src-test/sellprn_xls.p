/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: sellprn_new.p
      Comment: Печать книги продаж
   Parameters: нет
         Uses:
      Used by:
      Created: 21.02.2005 Dasu 0041882
     Modified: 10/09/2013 kraw (0191849) Изменение формы 
*/
{globals.i}
{intrface.get xclass }
{intrface.get tmess }
{intrface.get axd }

DEF VAR mAdvance   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mTab AS CHARACTER NO-UNDO.

mTab = CHR(9).

{sellbook.i }


PROCEDURE Sign-Prn:
   /*ПОДВАЛ*/
   &UNDEFINE signatur_i
   {signatur.i
      &stream="STREAM sfact"}
END PROCEDURE.
/*PUT STREAM sfact UNFORMATTED
    PADL("Приложение N 5", 254) SKIP
    PADL("к постановлению Правительства", 254) SKIP
    PADL("Российской Федерации", 254) SKIP
    PADL("от 26.12.11  № 1137", 254) SKIP
    PADL("────────    ────", 254) SKIP
    SKIP(2).
*/
/*ШАПКА КНИГИ*/
/*PUT STREAM sfact UNFORMATTED */
/*   FILL(" ",40) + "I. ФОРМА КНИГИ ПРОДАЖ, ПРИМЕНЯЕМОЙ ПРИ РАСЧЕТАХ ПО НАЛОГУ НА ДОБАВЛЕННУЮ СТОИМОСТЬ" SKIP(3)*/
/*   FILL(" ",40) + "I. КНИГА ПРОДАЖ" SKIP(3)
   "Продавец " + STRING(mBuyer,"x(230)") SKIP
   "         " + FILL("─",LENGTH(mBuyer)) SKIP(2)
   "Идентификационный номер и код причины постановки "  SKIP
   "на учет налогоплательщика-продавца               " + TRIM(STRING(mINN)) + "/" + 
   STRING(mKPP,"x(15)") SKIP
   "                                                 " + FILL("─",35) SKIP(2)
   "Продажа за период с " + string(bDB.Beg-Date,"99/99/9999") + " по " + 
   STRING(bDB.end-Date,"99/99/9999") SKIP
   "                    ──────────    ──────────" SKIP(3)
. */
/*ШАПКА ТАБЛИЧКИ*/
PUT STREAM sfact UNFORMATTED
/*   "┌────┬───────┬───────────────┬───────────────┬───────────────┬───────────────┬─────────────────────┬─────────────────────────┬──────────────────────────────────────────────┬───────────────┬────────────────────────┬───────────────────────────────┬───────────────────────────────────────────┬─────────────────────────────┬──────────────────┐" SKIP
   "│  № │ Код   │ Номер и дата  │ Номер и дата  │ Номер и дата  │ Номер и дата  │    Наименование     │         ИНН/КПП         │                 Сведения о                   │ Номер и дата  │      Наименование      │Стоимость продаж по счету-     │  Стоимость продаж, облагаемых налогом,    │  Сумма НДС по счету-        │ Стоимость продаж │" SKIP
   "│    │ вида  │ счета-фактуры │ исправления   │ корректиро-   │ исправления   │     покупателя      │       покупателя        │                 посреднике                   │   документа,  │      и код валюты      │фактуре,  разница стоимости    │  по счету-фактуре,разница стоимости по    │  фактуре, разница стоимос-  │ освобождаемых от │" SKIP
   "│ п/п│ опе-  │   продавца    │ счета-фактуры │ вочного       │ корректиро-   │                     │                         │            (комиссионере,агенте)             │ подтвержающего│                        │по корректировочному счету-    │  корректировочному счету-фактуре (без     │  ти по корректировочному    │ налога, по счету-│" SKIP
   "│    │ рации │               │ продавца      │ счета-фактуры │ вочного       │                     │                         ├────────────────────┬─────────────────────────┤    оплату     │                        │фактуре (включая НДС) в валюте │   НДС) в рублях и копейках, по ставке     │  счету-фактуре в рублях и   │ фактуре, разница │" SKIP
   "│    │       │               │               │ продавца      │ счета-фактуры │                     │                         │    наименование    │        ИНН/КПП          │               │                        │     счета-фактуры             │                                           │  копейках, по ставке        │ стоимости по кор-│" SKIP
   "│    │       │               │               │               │ продавца      │                     │                         │     посредника     │       посредника        │               │                        ├───────────────┬───────────────┤                                           │                             │ ректировочному   │" SKIP
   "│    │       │               │               │               │               │                     │                         │                    │                         │               │                        │ в валюте      │  в рублях и   │                                           │                             │ счету-фактуре    │" SKIP
   "│    │       │               │               │               │               │                     │                         │                    │                         │               │                        │ счета-фактуры │  копейках     ├───────────────┬─────────────┬─────────────┼───────────────┬─────────────┤ в рублях         │" SKIP
   "│    │       │               │               │               │               │                     │                         │                    │                         │               │                        │               │               │ 18 процентов  │10 процентов │ 0 процентов │ 18 процентов  │10 процентов │ и копейках       │" SKIP
   "├────┼───────┼───────────────┼───────────────┼───────────────┼───────────────┼─────────────────────┼─────────────────────────┼────────────────────┼─────────────────────────┼───────────────┼────────────────────────┼───────────────┼───────────────┼───────────────┼─────────────┼─────────────┼───────────────┼─────────────┼──────────────────┤" SKIP*/
   " 1  " CHR(9) "   2   " CHR(9) "      3        " CHR(9) "       4       " CHR(9) "       5       " CHR(9) "       6       " CHR(9) "          7          " CHR(9) "            8            " CHR(9) "         9          " CHR(9) "            10           " CHR(9) "      11       " CHR(9) "           12           " CHR(9) "      13а      " CHR(9) "      13б      " CHR(9) "       14      " CHR(9) "      15     " CHR(9) "     16      " CHR(9) "       17      " CHR(9) "     18      " CHR(9) "        19        " CHR(9) "" SKIP
/*   "├────┼───────┼───────────────┼───────────────┼───────────────┼───────────────┼─────────────────────┼─────────────────────────┼────────────────────┼─────────────────────────┼───────────────┼────────────────────────┼───────────────┼───────────────┼───────────────┼─────────────┼─────────────┼───────────────┼─────────────┼──────────────────┤" SKIP*/
.

def var mIspr as char no-undo.
def var mKorr1 as char no-undo.
def var mKorr2 as char no-undo.
def var mKorr3 as char no-undo.
def buffer sf for loan .
def buffer t1 for term-obl.
def buffer t2 for term-obl.

DEF VAR mPayDocNum AS CHAR  NO-UNDO.
DEF VAR mLastDate  AS DATE  NO-UNDO.
DEF VAR mSurrOp    AS CHAR  NO-UNDO.
DEF VAR mI         AS INT64 NO-UNDO.
DEF VAR mComment   AS CHAR  NO-UNDO.
DEF VAR mCurCode   AS CHAR  NO-UNDO.
DEF VAR mCurName   AS CHAR  NO-UNDO.
DEF VAR mPosrName  AS CHAR  NO-UNDO.
DEF VAR mPosrINN   AS CHAR  NO-UNDO.
DEF VAR mAmtC      AS DEC   NO-UNDO.
DEF VAR mIntAmt    AS DEC   NO-UNDO.
DEF VAR mAmtNoNds  AS DEC   NO-UNDO.

def buffer b_dl for bdl.
/*ДАННЫЕ ПО СТРОКАМ*/
FOR EACH bDL
   WHERE bDL.Data-ID EQ INT64(iDataBlock)
   NO-LOCK
   BREAK BY bDl.Data-ID
         BY bDl.Date1
         BY bDl.Date2:
/*
   IF ( NUM-ENTRIES(bDL.Txt,"~n") < 9 OR ENTRY(9,bDL.Txt,"~n") <> "Исправ") AND
      /* Или 13 есть и пустой,  или его нет */
      ( NUM-ENTRIES(bDL.Txt,"~n") < 13  OR
       ( NUM-ENTRIES(bDL.Txt,"~n") >= 13  AND  ENTRY(13,bDL.Txt,"~n") EQ "" ))
      THEN
*/
   DO:
      ASSIGN
         mStrNum     = mStrNum + 1
         mNamePostav = SplitStr(ENTRY(1,bDL.Txt,"~n"),
                                21,
                                "~n")

         mNums       = SplitStr(ENTRY(8,bDL.Txt,"~n") + " "
                              + ENTRY(6,bDL.Txt,"~n"),
                                15,
                                "~n")
         mAmt        = SplitStr(STRING(bDL.Val[11],">>>>,>>>,>>9.99") + " "
                              + ENTRY(7,bDL.Txt,"~n"),
                                15,
                                "~n")
         mAdvance    = ENTRY(4,bDL.Txt,"~n") EQ "Аванс"
         mAmtC       = 0
         mIntAmt     = 0
         mAmtNoNds   = 0
      .
      find first sf where sf.contract = "sf-out" and sf.cont-code = bDl.sf no-lock no-error.

      {selprn_newf.i}
      
      mIspr = GetXattrValue("loan","sf-out," + bDl.sf,"Исправ").
      
      find FIRST loan where 
                 loan.contract  = entry(1,mIspr) 
             and loan.cont-code = entry(2,mIspr) no-lock no-error.
      mKorr1 = "".
      mKorr2 = "".
      mKorr3 = "".
      if avail loan then
      do:

         mNums       = SplitStr(loan.doc-num + " " + string(loan.conf-date,"99/99/99"),15,"~n").
         mKorr1      = SplitStr(GetXattrValueEx("loan",
                                                sf.contract + "," + sf.cont-code,
                                                "НомДатКорр",""),15,"~n").
         mKorr2      = SplitStr(string(sf.conf-date,"99/99/99") + " " + sf.doc-num,15,"~n").
         if loan.loan-status = "Аннулир" then
         do:
            mIspr  = GetXattrValue("loan",sf.contract + "," + sf.cont-code,"Исправ").
            mKorr3 = SplitStr(GetXattrValueEx("loan",mIspr,"НомДатКорр",""),15,"~n").
         end.

         for each t1 of sf no-lock:
             accumulate t1.amount-of-payment (total).
             accumulate t1.int-amt (total).
         end.
         for each t2 of loan  no-lock:
             accumulate t2.amount-of-payment (total).
             accumulate t2.int-amt (total).
         end.
         bDL.Val[11] = (accum total t1.amount-of-payment) + 
                       (accum total t1.int-amt) - 
                       (accum total t2.amount-of-payment) - 
                       (accum total t2.int-amt).
         mAmt        = SplitStr(STRING(bDL.Val[11],"->>>,>>>,>>9.99") + " "
                              + ENTRY(7,bDL.Txt,"~n"),
                                15,
                                "~n").
         for each t1 of sf where t1.rate = 18 no-lock:
             accumulate t1.amount-of-payment (total).
             accumulate t1.int-amt (total).
         end.
         for each t2 of loan where t2.rate = 18  no-lock:
             accumulate t2.amount-of-payment (total).
             accumulate t2.int-amt (total).
         end.

         bDL.Val[1] = (accum total t1.amount-of-payment) - (accum total t2.amount-of-payment).
         bDL.Val[2] = (accum total t1.int-amt) - (accum total t2.int-amt).

         for each t1 of sf where t1.rate = 10 no-lock:
             accumulate t1.amount-of-payment (total).
             accumulate t1.int-amt (total).
         end.
         for each t2 of loan where t2.rate = 10  no-lock:
             accumulate t2.amount-of-payment (total).
             accumulate t2.int-amt (total).
         end.
         bDL.Val[3] = (accum total t1.amount-of-payment) - (accum total t2.amount-of-payment).
         bDL.Val[4] = (accum total t1.int-amt) - (accum total t2.int-amt).

         for each t1 of sf where t1.rate = 20 no-lock:
             accumulate t1.amount-of-payment (total).
             accumulate t1.int-amt (total).
         end.
         for each t2 of loan where t2.rate = 20  no-lock:
             accumulate t2.amount-of-payment (total).
             accumulate t2.int-amt (total).
         end.
         bDL.Val[7] = (accum total t1.amount-of-payment) - (accum total t2.amount-of-payment).
         bDL.Val[8] = (accum total t1.int-amt) - (accum total t2.int-amt).

         for each t1 of sf where t1.rate = 0 no-lock:
             accumulate t1.amount-of-payment (total).
             accumulate t1.int-amt (total).
         end.
         for each t2 of loan where t2.rate = 0  no-lock:
             accumulate t2.amount-of-payment (total).
             accumulate t2.int-amt (total).
         end.
         bDL.Val[9] = (accum total t1.amount-of-payment) - (accum total t2.amount-of-payment).
      end.
      /*
      else
      do:
         for each t1 of sf where t1.rate = 18 no-lock:
             accumulate t1.amount-of-payment (total).
             accumulate t1.int-amt (total).
         end.
         bDL.Val[1] = (accum total t1.amount-of-payment) - (accum total t1.int-amt).
      end.
      */
/*
message sf.doc-num SKIP 
bDL.Val[1] bDL.Val[2] bDL.Val[3] SKIP
bDL.Val[4] bDL.Val[5] bDL.Val[6] SKIP
bDL.Val[7] bDL.Val[8] bDL.Val[9] SKIP
bDL.Val[10] bDL.Val[11] bDL.Val[12] SKIP
bDL.Sym1 bDL.Sym2 bDL.Sym3 bDL.Sym4
view-as alert-box.                 */

      IF mAdvance THEN ASSIGN
         bDL.Val[1] = 0
         bDL.Val[3] = 0
      .
      PUT STREAM sfact UNFORMATTED
/*         "│" + */
         STRING(mStrNum,">>>9") + CHR(9) +   /*1*/
         STRING(mComment,"x(7)") + CHR(9) +  /*2*/
/*         REPLACE(mNums,"~n"," ") + CHR(9) +  /*3*/ */
      (IF NUM-ENTRIES(mNums,"~n") EQ 1 THEN
         TRIM(ENTRY(1,mNums," ")) + "; " +  TRIM(REPLACE(ENTRY(2,mNums," "), "/", ".")) /*3*/
       ELSE
         TRIM(ENTRY(1,mNums,"~n")) + "; " + TRIM(REPLACE(ENTRY(2,mNums,"~n"), "/", ".")) /*3*/
      )   
/*      DO i = 2 TO NUM-ENTRIES(mNums,"~n"):
         IF ENTRY(i,mNums,"~n") NE ? AND ENTRY(i,mNums,"~n") NE "" THEN
           PUT STREAM sfact UNFORMATTED
           TRIM(REPLACE(ENTRY(i,mNums,"~n"), "/", ".")).  /*3*/
      END.   
         PUT STREAM sfact UNFORMATTED
         CHR(9)
*/         
         STRING(mKorr1) + CHR(9) +  /*4*/
         STRING(mKorr2) + CHR(9) +   /*5*/
         STRING(mKorr3) + CHR(9) + " " + CHR(9) +    /*6*/


         REPLACE(mNamePostav,"~n", " ") + CHR(9) +   /*7*/
         STRING(TRIM(ENTRY(2,bDL.Txt,"~n") + "/" +
                ENTRY(3,bDL.Txt,"~n"),"/"),"x(25)") + CHR(9) +   /*8*/

         STRING(ENTRY(1,mPosrName,"~n"),"x(20)") + CHR(9) +   /*9*/
         STRING(mPosrINN,"x(25)") + CHR(9) +   /*10*/
/*         STRING(bDL.Sym2,"x(8)") + "│" +*/
         STRING(ENTRY(1,mPayDocNum,"~n"),"x(15)") + CHR(9) +    /*11*/
         STRING(mCurName,"x(24)") + CHR(9) +   /*12*/
         (IF AVAILABLE sf THEN (IF sf.currency EQ "" THEN 
         STRING(" ","x(15)") ELSE STRING(mAmtC,"->>>,>>>,>>9.99") /*13a*/
         ) ELSE STRING(" ","x(15)")) + CHR(9) +   /*13b*/
         STRING(ENTRY(1,mAmt,"~n"),"x(15)") + CHR(9) + /*14*/

         (IF mAdvance
          THEN (" " + FILL("-",13) + " ")
          ELSE 
          STRING(bDL.Val[1],"->>>,>>>,>>9.99")) + CHR(9) + /*15*/
         (IF mAdvance
          THEN (" " + FILL("-",11) + " ")
          ELSE 
          STRING(bDL.Val[3],"->,>>>,>>9.99")) + CHR(9) + /*15*/
         STRING(bDL.Val[5],"->,>>>,>>9.99") + CHR(9) + /*16*/

         STRING(bDL.Val[2],"->>>,>>>,>>9.99") + CHR(9) +  /*17*/

         STRING(bDL.Val[4],"->,>>>,>>9.99") + CHR(9) +  /*18*/
/*         STRING(bDL.Val[7],"->,>>>,>>9.99") + "│" +
         STRING(bDL.Val[8],"->,>>>,>>9.99") + "│" +*/
         STRING(bDL.Val[9],"->>,>>>,>>>,>>9.99") + CHR(9) /*19*/
         SKIP
      .
      /*
      RUN CalcStrike(mStrOfList + 1, 63, NO, OUTPUT mStrOfList).
      */
/*   del */
/*
      IF MAX(NUM-ENTRIES(mNamePostav,"~n"),

             NUM-ENTRIES(mKorr1,"~n"),
             NUM-ENTRIES(mKorr2,"~n"),
             NUM-ENTRIES(mKorr3,"~n"),

             NUM-ENTRIES(mNums,"~n"),
             NUM-ENTRIES(mAmt,"~n"),
             NUM-ENTRIES(mPayDocNum,"~n"),
             NUM-ENTRIES(mPosrName,"~n")) GE 2
      THEN DO i = 2 TO MAX(NUM-ENTRIES(mNamePostav,"~n"),

                           NUM-ENTRIES(mKorr1,"~n"),
                           NUM-ENTRIES(mKorr2,"~n"),
                           NUM-ENTRIES(mKorr3,"~n"),

                           NUM-ENTRIES(mNums,"~n"),
                           NUM-ENTRIES(mAmt,"~n"),
                           NUM-ENTRIES(mPayDocNum,"~n"),
                           NUM-ENTRIES(mPosrName,"~n")):
         PUT STREAM sfact UNFORMATTED
            "│    │       │" +
            STRING((IF i LE NUM-ENTRIES(mNums,"~n")
                   THEN ENTRY(i,mNums,"~n")
                   ELSE " "),"x(15)") + "│" +


            STRING((IF i LE NUM-ENTRIES(mKorr1,"~n")
                   THEN ENTRY(i,mkorr1,"~n")
                   ELSE " "),"x(15)") + "│" +
            STRING((IF i LE NUM-ENTRIES(mKorr2,"~n")
                   THEN ENTRY(i,mkorr2,"~n")
                   ELSE " "),"x(15)") + "│" +
            STRING((IF i LE NUM-ENTRIES(mKorr3,"~n")
                   THEN ENTRY(i,mkorr3,"~n")
                   ELSE " "),"x(15)") + "│" +



            STRING((IF i LE NUM-ENTRIES(mNamePostav,"~n")
                   THEN ENTRY(i,mNamePostav,"~n")
                   ELSE " "),"x(21)") + "│" +
            STRING(" ","x(25)")       + "│" +
            STRING((IF i LE NUM-ENTRIES(mPosrName,"~n")
                   THEN ENTRY(i,mPosrName,"~n")
                   ELSE " "),"x(20)") + "│" +
            STRING(" ","x(25)")       + "│" +
            
            STRING((IF i LE NUM-ENTRIES(mPayDocNum,"~n")
                   THEN ENTRY(i,mPayDocNum,"~n")
                   ELSE " "),"x(15)") + "│" +
            STRING(" ","x(24)")       + "│" + 
            STRING((IF i LE NUM-ENTRIES(mAmt,"~n")
                   THEN ENTRY(i,mAmt,"~n")
                   ELSE " "),"x(15)") +
            "│               │               │             │             │"
            "               │             │                  │" SKIP
            SKIP
         .
         
/*   del */

         /*
         RUN CalcStrike(mStrOfList + 1, 63, NO, OUTPUT mStrOfList).
         */
      END. */

      ACCUMULATE bDL.Val[11] (TOTAL).
      ACCUMULATE bDL.Val[1] (TOTAL).
      ACCUMULATE bDL.Val[2] (TOTAL).
      ACCUMULATE bDL.Val[3] (TOTAL).
      ACCUMULATE bDL.Val[4] (TOTAL).
      ACCUMULATE bDL.Val[5] (TOTAL).
      ACCUMULATE bDL.Val[7] (TOTAL).
      ACCUMULATE bDL.Val[8] (TOTAL).
      ACCUMULATE bDL.Val[9] (TOTAL).
      IF AVAILABLE sf THEN IF sf.currency NE "" THEN ACCUMULATE mAmtC (TOTAL).

      IF NOT LAST-OF(bDl.Data-ID) THEN DO:
/*         PUT STREAM sfact UNFORMATTED
            "├────┼───────┼───────────────┼───────────────┼───────────────┼─────"
            "──────────┼─────────────────────┼─────────────────────────┼───────"
            "─────────────┼─────────────────────────┼───────────────┼──────────"
            "──────────────┼───────────────┼───────────────┼───────────────┼───"
            "──────────┼─────────────┼───────────────┼─────────────┼───────────"
            "───────┤" SKIP
         . */
         /*
         RUN CalcStrike(mStrOfList + 1, 63, NO, OUTPUT mStrOfList).
         */
      END.
   END.
END.
/*ИТОГО*/
/*PUT STREAM sfact UNFORMATTED
            "├────┴───────┴───────────────┴───────────────┴───────────────┴─────"
            "──────────┴─────────────────────┴─────────────────────────┴───────"
            "─────────────┴─────────────────────────┴───────────────┴──────────"
            "──────────────┼───────────────┼───────────────┼───────────────┼───"
            "──────────┼─────────────┼───────────────┼─────────────┼───────────"
            "───────┤" SKIP. */
/*
RUN CalcStrike(mStrOfList + 1, 63, NO, OUTPUT mStrOfList).
*/

PUT STREAM sfact UNFORMATTED
   CHR(9) + " " + 
   CHR(9) + " " + 
   "Всего:  " + CHR(9) +
   CHR(9) + " " + 
   CHR(9) + " " + 
   CHR(9) + " " + 
   CHR(9) + " " + 
   CHR(9) + " " + 
   CHR(9) + " " + 
   CHR(9) + " " + 
   CHR(9) + " " + 
   CHR(9) + " " + 
   STRING((ACCUM TOTAL mAmtC),"->>>,>>>,>>9.99") + CHR(9) +
   STRING((ACCUM TOTAL bDL.Val[11]),"->>>,>>>,>>9.99") + CHR(9) +
   STRING((ACCUM TOTAL bDL.Val[1]),"->>>,>>>,>>9.99") + CHR(9) +
   STRING((ACCUM TOTAL bDL.Val[3]),"->,>>>,>>9.99") + CHR(9) +
   STRING((ACCUM TOTAL bDL.Val[5]),"->,>>>,>>9.99") + CHR(9) +
   STRING((ACCUM TOTAL bDL.Val[2]),"->>>,>>>,>>9.99") + CHR(9) +
   STRING((ACCUM TOTAL bDL.Val[4]),"->,>>>,>>9.99") + CHR(9) +
   
   STRING((ACCUM TOTAL bDL.Val[7]),"->,>>>,>>9.99") + CHR(9) 
/*   +
   STRING((ACCUM TOTAL bDL.Val[8]),"->,>>>,>>9.99") + CHR(9) +
   STRING((ACCUM TOTAL bDL.Val[9]),"->>,>>>,>>>,>>9.99") + CHR(9) 
*/   
   SKIP. 

/*
RUN CalcStrike(mStrOfList + 1, 63, NO, OUTPUT mStrOfList).
*/
/*PUT STREAM sfact UNFORMATTED
            "└──────────────────────────────────────────────────────────────────"
            "──────────────────────────────────────────────────────────────────"
            "──────────────────────────────────────────────────────────────────"
            "──────────────┴───────────────┴───────────────┴───────────────┴───"
            "──────────┴─────────────┴───────────────┴─────────────┴───────────"
            "───────┘" SKIP
. */
/*
RUN CalcStrike(mStrOfList + 1, 63, NO, OUTPUT mStrOfList).
*/
/* RUN Sign-Prn IN THIS-PROCEDURE. */
/*
IF printer.page-lines < mStrOfList + 14 THEN DO:
   RUN EndStrike(mStrOfList + 2, 63, YES, OUTPUT mStrOfList).
   RUN Sign-Prn.
   RUN EndStrike(mStrOfList + 15, 63, YES, OUTPUT mStrOfList). /* Возможно здесь - задница */
END.
ELSE DO:
   RUN Sign-Prn.
   RUN EndStrike(mStrOfList + 15, 63, YES, OUTPUT mStrOfList).
END.
*/
/*
IF CAN-FIND(FIRST bDl WHERE entry(9,bDL.Txt,"~n") EQ "Аннулир") THEN
DO:
   pick-value = 'yes'. /* поведение по умолчанию */
   RUN Fill-SysMes IN h_tmess (
      "", "", "4",
      "Печатать аннулированные счет-фактуры?"
   ).
   IF pick-value EQ "yes" THEN
   DO:
      RUN SellAddPage(iDataBlock, PAGE-NUMBER(sfact)).
   END.
END.
*/
/*
{preview.i &STREAM="STREAM sfact"}
*/
OUTPUT STREAM sfact CLOSE.
RUN sndbispc ("file=" + "sellbook.txt" + ";class=xls").  
{intrface.del}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/02/2015 11:53:14.456+04:00' */
/* $LINTUSER='STRE' */
/* $LINTMODE='1' */
/* $LINTFILE='sellprn_new.p' */
/*prosign+qbtf0/kXYOAmb16n2qTdw*/