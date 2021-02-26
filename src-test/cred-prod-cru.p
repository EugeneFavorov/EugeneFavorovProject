/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"
     Filename: cred-prod-cr.p
      Comment: Форма ввода продукта в КиД
   Parameters:
         Uses:
      Used by:
      Created: 18.07.2008 11:38 Fepa
*/

{globals.i}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get tmess}
{intrface.get refer}

/*DEF INPUT  PARAM iProdCode AS CHAR NO-UNDO.
DEF INPUT  PARAM iDate     AS DATE NO-UNDO.
DEF INPUT  PARAM iValParam AS CHAR NO-UNDO.
DEF OUTPUT PARAM oResult   AS CHAR NO-UNDO.
*/
DEFINE INPUT PARAMETER in-str   as char NO-UNDO.


DEF var iProdCode AS CHAR NO-UNDO.
DEF var iDate     AS DATE NO-UNDO.
DEF var iValParam AS CHAR NO-UNDO.
DEF var oResult   AS CHAR NO-UNDO.

iProdCode    = entry(1,in-str,"|").
iDate        = date(entry(2,in-str,"|")).
iValParam    = entry(3,in-str,"|").
oResult      = entry(4,in-str,"|").


DEF VAR mProduct    AS CHAR   NO-UNDO.
DEF VAR mCurrency   AS CHAR   NO-UNDO.
DEF VAR mSumma      AS DEC    NO-UNDO.
DEF VAR mSrok       AS CHAR   NO-UNDO.
DEF VAR mRate       AS CHAR   NO-UNDO.
DEF VAR mKolDok     AS CHAR   NO-UNDO.
DEF VAR mTarif      AS CHAR   NO-UNDO.
DEF VAR field-fr    AS CHAR   NO-UNDO.
DEF VAR mHanEdProd1 AS HANDLE NO-UNDO.
DEF VAR mHanEdProd2 AS HANDLE NO-UNDO.
DEF VAR mHanEdProd3 AS HANDLE NO-UNDO.
DEF VAR mHanEdProd4 AS HANDLE NO-UNDO.
DEF VAR mHanEdProd5 AS HANDLE NO-UNDO.
DEF VAR mHanEdProd6 AS HANDLE NO-UNDO.
DEF VAR mGlobSumm   AS CHAR   NO-UNDO.
DEF VAR mProdTmpC   AS CHAR   NO-UNDO. /* Код Справочника */
DEF VAR mProdTmpT   AS CHAR   NO-UNDO. /* Код типа справочника */
DEF VAR mContType   AS CHAR   NO-UNDO.
DEF VAR mFrameName  AS CHAR   NO-UNDO.
DEF VAR mPoruchit   AS LOG    NO-UNDO.
DEF VAR mVznos      AS DEC    NO-UNDO.
DEF VAR mVznosProc  AS DEC    NO-UNDO.
DEF VAR mStrah      AS LOG    NO-UNDO.
DEF VAR mIndeks     AS CHAR   NO-UNDO.
DEF VAR mMinSumm    AS CHAR   NO-UNDO.
DEF VAR mSrokMin    AS CHAR   NO-UNDO.
DEF VAR mSrokMax    AS CHAR   NO-UNDO.
DEF VAR mVznosMin   AS CHAR   NO-UNDO.
DEF VAR mVznosMax   AS CHAR   NO-UNDO.
DEF VAR mCount      AS INT64    NO-UNDO.
DEF VAR mProdCode   AS CHAR   NO-UNDO.
DEF VAR mAvtoPrice  AS DEC    NO-UNDO.
DEF VAR mFirstPer   AS INT    NO-UNDO.
DEF VAR mPartAmount AS DEC    NO-UNDO.
DEF VAR mPartAmtMin AS CHAR   NO-UNDO.
DEF VAR mPartAmtMax AS CHAR   NO-UNDO.
DEF VAR mStrahLife  AS LOG    NO-UNDO.

DEF BUFFER btmp-code FOR tmp-code.

/* "Приемник" возвращаемых значений. */
{ttretval.def}

   /* Описание фреймов */
{cred-prod-cr.frm}

ASSIGN
   mHanEdProd1           = mProduct:HANDLE IN FRAME edit-prod-necel
   mHanEdProd1:READ-ONLY = YES
   mHanEdProd2           = mProduct:HANDLE IN FRAME edit-prod-avto
   mHanEdProd2:READ-ONLY = YES
   mHanEdProd3           = mProduct:HANDLE IN FRAME edit-prod-cel
   mHanEdProd3:READ-ONLY = YES
   mHanEdProd4           = mProduct:HANDLE IN FRAME edit-prod-akc
   mHanEdProd4:READ-ONLY = YES
   mHanEdProd5           = mProduct:HANDLE IN FRAME edit-prod-depvcc
   mHanEdProd5:READ-ONLY = YES
   mHanEdProd6           = mProduct:HANDLE IN FRAME edit-prod-depvcc
   mHanEdProd6:READ-ONLY = YES
.

mProdCode = iProdCode.
   /* Ищем темпорированный классификатор. Ищем напрямую, т.к. 
   ** getTCodeFld отказывается работать с массивами */
FIND LAST btmp-code WHERE 
          btmp-code.class      EQ "ПродЛин"
   AND    btmp-code.code       EQ mProdCode
   AND    btmp-code.beg-date   LE iDate
   AND   (btmp-code.end-date   GE iDate
      OR  btmp-code.end-date   EQ ?)
NO-LOCK NO-ERROR.
   /* Если на подпродукте не задан базовый тариф, то ищем на родителях */
REPEAT WHILE AVAIL btmp-code AND btmp-code.misc[7] EQ "":
   FIND FIRST code WHERE 
              code.class EQ "ПродЛин"
          AND code.code  EQ mProdCode 
   NO-LOCK NO-ERROR.
   IF AVAIL code AND code.parent NE "" THEN
   DO:
      mProdCode = code.parent.
      FIND LAST btmp-code WHERE 
                btmp-code.class      EQ "ПродЛин"
         AND    btmp-code.code       EQ mProdCode
         AND    btmp-code.beg-date   LE iDate
         AND   (btmp-code.end-date   GE iDate
            OR  btmp-code.end-date   EQ ?)
      NO-LOCK NO-ERROR.
   END.
   ELSE
      RELEASE btmp-code.
END.
IF AVAIL btmp-code THEN
   ASSIGN
      mProdTmpC = btmp-code.code       /* Код продукта */
      mProdTmpT = btmp-code.val        /* Описание продукта из indicate на основе которого выбирается фрейм ввода */  
      mContType = btmp-code.misc[3]    /* Тип договора (классификатор ТипДогА) */
   .

   /* Определеяем справочник. */
FIND LAST DataBlock WHERE 
          DataBlock.DataClass-Id EQ mProdTmpC
NO-LOCK NO-ERROR.

   /* Описание триггеров всех форм */
{cred-prod-cr.i}
CASE mProdTmpT:
   WHEN "КредПрод" THEN
   DO:
      BLCK:
      DO
      WITH FRAME edit-prod-necel
      ON ERROR  UNDO BLCK, LEAVE BLCK
      ON ENDKEY UNDO BLCK, LEAVE BLCK:
         ENABLE ALL EXCEPT mProduct mTarif WITH FRAME edit-prod-necel.
         WAIT-FOR ENDKEY, GO OF FRAME edit-prod-necel FOCUS mHanEdProd1.
         oResult = mProduct:SCREEN-VALUE + ";" + mSumma:SCREEN-VALUE + ";" + mCurrency:SCREEN-VALUE + ";" + mSrok:SCREEN-VALUE + ";" + mRate:SCREEN-VALUE + ";" + mKolDok:SCREEN-VALUE + ";" + mTarif:SCREEN-VALUE + ";" + mContType + ";" + mPoruchit:SCREEN-VALUE + ";;;" + "Нет" + ";;".
      END.
   END.
   WHEN "АвтоПрод" THEN
   DO:
      BLCK:
      DO
      WITH FRAME edit-prod-avto
      ON ERROR  UNDO BLCK, LEAVE BLCK
      ON ENDKEY UNDO BLCK, LEAVE BLCK:
         ENABLE ALL EXCEPT mProduct mTarif mVznosProc WITH FRAME edit-prod-avto.
         WAIT-FOR ENDKEY, GO OF FRAME edit-prod-avto FOCUS mHanEdProd2.
         oResult = mProduct:SCREEN-VALUE + ";" + mSumma:SCREEN-VALUE + ";" + mCurrency:SCREEN-VALUE + ";" + mSrok:SCREEN-VALUE + ";" + mRate:SCREEN-VALUE + ";" + mKolDok:SCREEN-VALUE + ";" + mTarif:SCREEN-VALUE + ";" + mContType + ";" + mPoruchit:SCREEN-VALUE + ";" + mVznos:SCREEN-VALUE + ";" + mVznosProc:SCREEN-VALUE + ";" + mStrah:SCREEN-VALUE + ";;;".
      END.
   END.
   WHEN "ПотребПрод" THEN
   DO:
      BLCK:
      DO
      WITH FRAME edit-prod-cel
      ON ERROR  UNDO BLCK, LEAVE BLCK
      ON ENDKEY UNDO BLCK, LEAVE BLCK:
         ENABLE ALL EXCEPT mProduct mTarif mVznosProc WITH FRAME edit-prod-cel.
         WAIT-FOR ENDKEY, GO OF FRAME edit-prod-cel FOCUS mHanEdProd3.
         oResult = mProduct:SCREEN-VALUE + ";" + mSumma:SCREEN-VALUE + ";" + mCurrency:SCREEN-VALUE + ";" + mSrok:SCREEN-VALUE + ";" + mRate:SCREEN-VALUE + ";" + mKolDok:SCREEN-VALUE + ";" + mTarif:SCREEN-VALUE + ";" + mContType + ";;" + mVznos:SCREEN-VALUE + ";" + mVznosProc:SCREEN-VALUE + ";" + "Нет" + ";" + mIndeks:SCREEN-VALUE + ";".
      END.
   END.
   WHEN "АкцПрод" THEN
   DO:
      BLCK:
      DO
      WITH FRAME edit-prod-akc
      ON ERROR  UNDO BLCK, LEAVE BLCK
      ON ENDKEY UNDO BLCK, LEAVE BLCK:
         ENABLE ALL EXCEPT mProduct mTarif mVznosProc WITH FRAME edit-prod-akc.
         WAIT-FOR ENDKEY, GO OF FRAME edit-prod-akc FOCUS mHanEdProd4.                                                     
            /* т.к. в этой форме нет рейтинга и кол.документов, то просто пропускаем, что бы не нарушать последоватнльность возвращаемых entry */
         oResult = mProduct:SCREEN-VALUE + ";" + mSumma:SCREEN-VALUE + ";" + mCurrency:SCREEN-VALUE + ";" + mSrok:SCREEN-VALUE + ";;;" + mTarif:SCREEN-VALUE + ";" + mContType + ";" + "Нет" + ";" + mVznos:SCREEN-VALUE + ";" + mVznosProc:SCREEN-VALUE + ";;" + "Нет" + ";;".
      END.
   END.
   WHEN "ДепВСС" THEN
   DO:
      BLCK:
      DO
      WITH FRAME edit-prod-depvcc
      ON ERROR  UNDO BLCK, LEAVE BLCK
      ON ENDKEY UNDO BLCK, LEAVE BLCK:
         ENABLE ALL EXCEPT mProduct mTarif WITH FRAME edit-prod-depvcc.
         WAIT-FOR ENDKEY, GO OF FRAME edit-prod-depvcc FOCUS mHanEdProd5.                                                     
         oResult = mProduct:SCREEN-VALUE  + ";" +  /* 1.Код продукта  */
                   mSumma:SCREEN-VALUE    + ";" +  /* 2.Сумма */
                   mCurrency:SCREEN-VALUE + ";" +  /* 3.Валюта */
                   mSrok:SCREEN-VALUE     + ";" +  /* 4.Срок */
                                            ";" +  /* 5.Рейтинг */
                                            ";" +  /* 6.Кол.документов */
                   mTarif:SCREEN-VALUE    + ";" +  /* 7.Тариф */
                   mContType              + ";" +  /* 8. */
                   "Нет"                  + ";" +  /* 9.Поручитель */
                                            ";" +  /* 10.Взнос */
                                            ";" +  /* 11.%% взноса */
                                            ";" +  /* 12.Страхование */
                   "Нет"                  + ";" +  /* 13.Индекс */
                                            ";".   /* 14. */
      END.
   END.
   WHEN "НадеждаОД" THEN
   DO:
      BLCK:
      DO
      WITH FRAME edit-prod-hope
      ON ERROR  UNDO BLCK, LEAVE BLCK
      ON ENDKEY UNDO BLCK, LEAVE BLCK:
         ENABLE ALL EXCEPT mProduct mTarif mVznosProc WITH FRAME edit-prod-hope.
         WAIT-FOR ENDKEY, GO OF FRAME edit-prod-hope FOCUS mHanEdProd6.
         
         oResult = mProduct:SCREEN-VALUE   + ";" + /* 1.Код продукта  */ 
                   mSumma:SCREEN-VALUE     + ";" + /* 2.Сумма */         
                   mCurrency:SCREEN-VALUE  + ";" + /* 3.Валюта */                        
                   mSrok:SCREEN-VALUE      + ";" + /* 4.Срок */                          
                   ""      + ";" + /* 5.Рейтинг */       
                   ""    + ";" + /* 6.Кол.документов */
                   mTarif:SCREEN-VALUE     + ";" + /* 7.Тариф */         
                   mContType               + ";" + /* 8. Тип */                       
                   ""  + ";" + /* 9.Поручитель */    
                   mVznos:SCREEN-VALUE     + ";" +   /* 10.Взнос */        
                   mVznosProc:SCREEN-VALUE + ";" +   /* 11.%% взноса */   
                   mStrah:SCREEN-VALUE     + ";;;" + /* 12.Страхование */
                   mStrahLife:SCREEN-VALUE + ";" +   /* 15. Страхование жизни*/   
                   mAvtoPrice:SCREEN-VALUE + ";" +   /* 16. Цена авто */   
                   mFirstPer:SCREEN-VALUE  + ";" +   /* 17. Период уменьшения */
                   mPartAmount:SCREEN-VALUE + ";"   /* 18. Доля */
                   .
      END.                                                
   END.
   OTHERWISE
      RUN Fill-SysMes IN h_tmess ("","","","Ошибка: в классификаторе 'ПродЛин' для справочника '" + mProdTmpC + 
                                  "' не верно указано значение параметра классификатора '" + mProdTmpT + "'").
END CASE.

pick-value = oResult.

HIDE FRAME edit-prod-necel.
HIDE FRAME edit-prod-cel.
HIDE FRAME edit-prod-avto.
HIDE FRAME edit-prod-akc.
HIDE FRAME edit-prod-depvcc.

{intrface.del}
RETURN .
