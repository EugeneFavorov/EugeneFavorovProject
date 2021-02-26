/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ЗАО "Банковские информационные системы"
     Filename: LGARTERM.P
      Comment: Инструменты для печати обектов обеспечения
   Parameters: 
         Uses:
      Used by: 
      Created: 22/08/2003 Илюха (0007460)
     Modified: 17/04/2008 jadv  (0078661)
     Modified:

ОбСумма    - сумма
ОбВалюта   - валюта
ОбДатаЗакл - дата заключения
ОбДатаОкон - дата окончания
ОбДатаВыб  - дата выбытия

*/

/*
	aa4 - изменен формат ареса "ОбКлАдр"
	aa4 - добавлен тэг "ОбВид1" - 27.11.2013 
*/




{globals.i}             /* Глобальные переменные сессии. */
{svarloan.def}          /* Shared переменные модуля "Кредиты и депозиты". */
{client.i}          /* Формирование имени клиента */
{norm.i}
{wordwrap.def}
{intrface.get xclass}
{intrface.get cust}
{intrface.get i254}
{sh-defs.i} /* для проц-ки acct-pos */
{loan.pro} /* Для процедуры получения значений таблицы term-obl */ 

DEF OUTPUT PARAM iResult AS DECIMAL NO-UNDO.
DEF INPUT  PARAM iDate1  AS DATE    NO-UNDO.
DEF INPUT  PARAM iDate   AS DATE    NO-UNDO.
DEF INPUT  PARAM iStrPar AS CHAR    NO-UNDO.

DEF VAR mParam    AS CHAR  NO-UNDO.
DEF VAR mPutStr   AS CHAR  NO-UNDO.
DEF VAR mSummaStr AS CHAR  NO-UNDO.
DEF VAR mCurrStr  AS CHAR  NO-UNDO.
DEF VAR mDate     AS DATE  NO-UNDO.
DEF VAR mCounter  AS INT64   NO-UNDO.
DEF VAR mLength   AS INT64   NO-UNDO.
DEF VAR mInd      AS INT64   NO-UNDO.
DEF VAR mInd2     AS CHAR  NO-UNDO.
DEF VAR mTmpStr   AS CHAR  NO-UNDO.
DEF VAR mSrcChar  AS CHAR  NO-UNDO.
DEF VAR mRecId    AS RECID NO-UNDO. /* RECID term */
DEF VAR vTmpRec   AS RECID NO-UNDO.
DEF VAR vCountInt AS INT64   NO-UNDO. /* Только для Счетчик */
DEF VAR mSort     AS CHAR  NO-UNDO.
DEF VAR mSurr     AS CHAR  NO-UNDO.
DEF VAR mRecidStr AS CHAR  NO-UNDO.
DEF VAR vFormat   AS CHAR  NO-UNDO.
DEF VAR vCurrStr  AS CHAR  NO-UNDO.
DEF VAR vCurrSrt  AS CHAR  NO-UNDO.
DEF VAR digit     AS INT64 EXTENT 12 NO-UNDO.
DEF VAR vCounStr  AS INT64 NO-UNDO.
DEF VAR vStinstr  AS CHAR NO-UNDO.
DEF VAR mTemp-Str AS CHAR NO-UNDO. /*список способов оценки*/
DEF VAR mExtStr   AS CHAR EXTENT 52 NO-UNDO.
DEF VAR mParamId  AS INT64 INIT 0 NO-UNDO.
DEF VAR vCountTo  AS INT64 NO-UNDO INIT 0.
DEF VAR logTrim   AS LOG  NO-UNDO INIT NO.
DEF VAR chTrim    AS CHAR NO-UNDO.
DEF VAR iIndex    AS INT64  NO-UNDO.
DEF VAR vName     AS CHAR NO-UNDO EXTENT 2. /* Используется для формирования наименования счета */
DEF VAR vDRCode   AS CHAR NO-UNDO. /* Код ДР */
DEF VAR vPhone    AS CHAR NO-UNDO.

DEFINE TEMP-TABLE t-sort  /* Для ("Счетчик|*") */
  FIELD order  AS CHAR
  FIELD rec-id AS RECID
   INDEX order IS UNIQUE PRIMARY order
.

DEF NEW GLOBAL SHARED VAR sTermRecid AS RECID NO-UNDO.
DEF NEW GLOBAL SHARED VAR sStr       AS CHAR  NO-UNDO. /*для корректной работы lgarterm при замене залога*/

DEF BUFFER loan      FOR loan.
DEF BUFFER term-obl  FOR term-obl.
DEF BUFFER loan-cond FOR loan-cond.
DEF BUFFER currency  FOR currency.

FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) FORWARD.

/* для печати наименования месяца*/
&GLOB mMonths "января,февраля,марта,апреля,мая,июня,~
июля,августа,сентября,октября,ноября,декабря"

/* Для функции Trim - урезание не значащих пробелов */
FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) :
   CASE FormatTrim:
      WHEN "trim"  THEN cValue = TRIM(cValue).
      WHEN "ltrim" THEN cValue = LEFT-TRIM(cValue).
      WHEN "rtrim" THEN cValue = RIGHT-TRIM(cValue).
   END CASE.
   RETURN cValue.
END.

printres = NO. /*не выводить значение xrezult */

   /*MESSAGE  'lgarterm.p параметр'  iStrPar SKIP 'sTermRecid' sTermRecid
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF NUM-ENTRIES(iStrPar) GE 1 THEN DO:
   mInd = INDEX(iStrPar,"###") .
   IF mInd NE 0 THEN DO:   
      /* последним в списке параметров передан recid(term-obl) : */
      mParamId = INT64( SUBSTRING( ENTRY (NUM-ENTRIES(iStrPar), iStrPar), 4) ) .
      IF mParamId GT 0 THEN
         mRecId = mParamId .  /* recid(term-obl) */
      /* удаление recid(term-obl) из списка параметров : */
      iStrPar = SUBSTRING(iStrPar, 1, mInd - 2) .
   END.
END.

IF sTermRecid EQ ? THEN
   RETURN ''.
   
/* ищем сущности */
FIND FIRST term-obl WHERE
     RECID(term-obl)     = mRecId
NO-LOCK NO-ERROR.

/*IF AVAIL term-obl  THEN
   MESSAGE iStrPar sTermRecid
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

FIND FIRST loan WHERE
      loan.contract   = term-obl.contract AND
      loan.cont-code  = term-obl.cont-code
     /* RECID(loan) = rid-p */
NO-LOCK NO-ERROR.

FIND FIRST loan-cond WHERE
     RECID(loan-cond) = rid-t
NO-LOCK NO-ERROR.

DO TRANSACTION ON ENDKEY UNDO, LEAVE
               ON ERROR  UNDO, LEAVE:

   mParam = ENTRY(1,iStrPar).

   IF mParam MATCHES "*trim*" THEN 
   DO:
      logTrim = YES.
      iIndex  = INDEX(iStrPar,",").
      chTrim  = SUBSTRING(iStrPar,1,iIndex - 1).
      mParam  = SUBSTRING(iStrPar,iIndex + 1,LENGTH(iStrPar) - iIndex).
   END.

   /*
   Сумма обеспечения, выводит сумму в формате "->>>,>>>,>>>,>>9.99" или
   в формате указанным 2 параметром ф-ции + в скобках выводится строкой
   */

   IF mParam MATCHES "ОбСумма*" THEN
   DO:
      RUN "x-amtstr.p" (term-obl.amt,
                        term-obl.currency,
                        YES,
                        YES,
                        OUTPUT mSummaStr,
                        OUTPUT mCurrStr).
      mPutStr = STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99") + " (" + TRIM(mSummaStr) + " " + TRIM(mCurrStr) + ")".

      /*mPutStr = (IF NUM-ENTRIES(iStrPar,"|") = 1 THEN
                    STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99")
                 ELSE STRING(term-obl.amt-rub,ENTRY(2,iStrPar,"|"))
                ) + " (" + TRIM(mSummaStr) + " " + TRIM(mCurrStr) + ")".*/

      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).

   END.

/* 
Оценка транспортного средства как предмета залога
Выводит цену залога в формате "->>>,>>>,>>>,>>9.99" или
в формате указанным 2 параметром ф-ции + в скобках выводится строкой
*/
   IF mParam MATCHES "ОбСуммЧс*" THEN
   DO:
      RUN "x-amtstr.p" (term-obl.amt,
                        term-obl.currency,
                        NO,
                        YES,
                        OUTPUT mSummaStr,
                        OUTPUT mCurrStr).

    FIND FIRST currency WHERE currency.currency EQ term-obl.currency NO-LOCK NO-ERROR.

    vStinstr = STRING(IF term-obl.amt GE 0 THEN term-obl.amt ELSE (- term-obl.amt), "999999999999.99").
    DO vCounStr = 1 TO 12:
       digit[vCounStr] = INT64(SUBSTR(vStinstr, 13 - vCounStr, 1)).
    END.

      vCurrStr = (IF digit[1] = 1 AND digit[2] <> 1                    THEN IF AVAIL(currency) THEN currency.curr-form1 ELSE "???"
                  ELSE IF digit[2] = 1 OR digit[1] > 4 OR digit[1] = 0 THEN IF AVAIL(currency) THEN currency.curr-form5 ELSE "???"
                  ELSE                                                      IF AVAIL(currency) THEN currency.curr-form2 ELSE "???" ).
                  
      mPutStr = (IF NUM-ENTRIES(iStrPar,'|') = 1 THEN
                    STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99")
                 ELSE
                    STRING(term-obl.amt-rub,ENTRY(2,iStrPar,"|"))
					) + " (" + TRIM(mSummaStr) + " " + TRIM(vCurrStr) + " " + TRIM(mCurrStr) + ")".
                
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).

   END.
   /*
   валюта обеспечения
   */
   IF mParam = "ОбВалюта" THEN
   DO:
      IF term-obl.currency = "" THEN
         mPutStr = " в {&in-LP-C6} ".
      ELSE
      DO:
         FIND FIRST currency WHERE
                    currency.currency = term-obl.currency
         NO-LOCK NO-ERROR.
         mPutStr = "в валюте: " + currency.name-currenc.
      END.
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,STRING(mPutStr))
                                     ELSE STRING(mPutStr)).
   END.

   /*
   Дата заключения/окончания/выбытия
    Примеры использования
      [lgarterm(ОбДатаЗакл)]            - дата заключения формат 99/99/9999
                                          по умолчанию
      [lgarterm(ОбДатаЗакл,пр)]         - дата  заключения
                                          (число + месяц прописью + год)
      [lgarterm(ОбДатаЗакл,99.99.9999)] - дата заключения в формате 2 параметра
   */

   IF mParam = "ОбДатаЗакл" OR
      mParam = "ОбДатаОкон" OR
      mParam = "ОбДатаВыб"  THEN
   DO:
      ASSIGN
         mDate = IF mParam = "ОбДатаЗакл" THEN
                    term-obl.fop-date
                 ELSE
                 IF mParam = "ОбДатаОкон" THEN
                    term-obl.end-date
                 ELSE
                    term-obl.sop-date

         mPutStr = IF NUM-ENTRIES(iStrPar) = 1 THEN
                      STRING(mDate,"99/99/9999")
                   ELSE
                   IF ENTRY(2,iStrPar) = "пр" THEN
                      STRING(DAY(mDate)) + " "
                       + ENTRY(MONTH(mDate),{&mMonths})
                       + STRING(YEAR(mDate), " 9999 г.")
                   ELSE
                      STRING(mDate,ENTRY(2,iStrPar))
         .

      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.
   /* короткое наименование клиента */
   IF mParam = "ОбКлиент" THEN
   DO:
      mPutStr = "".
      RUN RE_CLIENT(term-obl.symbol,
                    term-obl.fop,
                    INPUT-OUTPUT mPutStr).
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.
   
   /* полное наименование клиента */
   IF mParam = "ОбКлиентП" THEN
   DO:
      mPutStr = "".
      RUN RE_CLIENT_FULL(term-obl.symbol,
                    term-obl.fop,
                    INPUT-OUTPUT mPutStr).
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.
   /* полное наименование статуса ЮЛ клиента */
   IF mParam = "ОбКлиентСтат" THEN
   DO:
      mPutStr = "".
          IF term-obl.symbol EQ "Ю" THEN DO:
             FIND FIRST cust-corp WHERE cust-corp.cust-id EQ term-obl.fop NO-LOCK NO-ERROR.
             IF AVAIL(cust-corp) THEN DO:
                IF {assigned cust-corp.cust-stat} THEN DO:
					FIND code WHERE code.val EQ cust-corp.cust-stat
					AND code.class EQ "КодПредп".
					mPutStr = code.name.
				END.
                  /* mPutStr = GetCodeVal("КодПредп", cust-corp.cust-stat). */
                IF mPutStr = ? THEN mPutStr = "".
             END.
          END.
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.

/* IF mParam EQ "ОбКлДок|Тип" THEN    - тип документа поручителя */
/* IF mParam EQ "ОбКлДок|Номер" THEN  - номер документа поручителя */
/* IF mParam EQ "ОбКлДок|Выдан" THEN  - кем и когда выдан документ поручителя */
/* IF mParam EQ "ОбКлАдр" THEN        - адрес поручителя */
/* IF mParam matches "ОбКлДР|<код ДР>" THEN  - доп.реквизит поручителя */
/* IF mParam EQ "ОбКлИНН" THEN               - ИНН поручителя */
/* IF mParam matches "КлДР|<код ДР>" THEN    - доп. реквизит заемщика*/
/* IF mParam EQ "КлИНН" THEN                 - ИНН заемщика*/

   /* Паспортные данные поручителя (физического лица) : */
   /* тип документа поручителя : */
   IF mParam EQ "ОбКлДок|Тип" THEN
      IF term-obl.symbol EQ "Ч" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.document-id .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   /* номер документа поручителя : */
   IF mParam EQ "ОбКлДок|Номер" THEN
      IF term-obl.symbol EQ "Ч" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.document .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   /* кем и когда выдан документ поручителя : */
   IF mParam EQ "ОбКлДок|Выдан" THEN
      IF term-obl.symbol EQ "Ч" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
                 mPutStr = REPLACE(fGetDocIssue(person.person-id),",",", код подразделения ").
				 SUBSTRING(mPutStr,LENGTH(mPutStr, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
				 mPutStr = REPLACE(mPutStr,"/",".").
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .

   /* Адрес поручителя : */
   IF mParam MATCHES "ОбКлАдр*" THEN DO :                   /* aa4 - изменен формат адреса */
      IF term-obl.symbol EQ "Ч" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = "".
			IF NUM-ENTRIES(mParam,"|")	= 1 THEN DO :
				RUN RetAdr.p(person.person-id,  "Ч", "АдрПроп", ?, OUTPUT mPutStr).
			END.
			IF NUM-ENTRIES(mParam,"|")	> 1 THEN DO:
				RUN RetAdr.p(person.person-id,  "Ч", "АдрПроп", ?, OUTPUT mPutStr).
				IF NOT RetString THEN
					PUT STREAM fil UNFORMATTED TRIM(mPutStr,",").
				ELSE
					RETURN TRIM(mPutStr,",").
			END.
			mTmpStr = mPutStr.
			mPutStr = "".
			DO mCounter = 2 TO 8 :
				IF TRIM(ENTRY(mCounter,mTmpStr)) NE "" THEN
					mPutStr = mPutStr + ENTRY(mCounter, mTmpStr) + ",".
			END.
            mPutStr = SUBSTR(mPutStr,1,LENGTH(mPutStr) - 1) . /* delete last comma */
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END.
      END.
      IF term-obl.symbol EQ "Ю" THEN
      DO :
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ term-obl.fop
            NO-LOCK NO-ERROR.
		 IF AVAILABLE cust-corp THEN DO:
			IF NUM-ENTRIES(mParam,"|")	= 1 THEN DO :
				RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрЮр", ?, OUTPUT mPutStr).
				PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
			END.
			IF NUM-ENTRIES(mParam,"|")	> 1 THEN DO:
				RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрЮр", ?, OUTPUT mPutStr).
				IF NOT RetString THEN
					PUT STREAM fil UNFORMATTED TRIM(mPutStr,",").
				ELSE
					RETURN TRIM(mPutStr,",").
			END.
		 END.
		 
      END.
      IF term-obl.symbol EQ "Б" THEN
      DO :
         FIND FIRST banks WHERE banks.bank-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE banks THEN DO :
            mPutStr = banks.law-address .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   END.

  
   /* Доп.реквизит поручителя : */
   IF mParam MATCHES ("ОбКлДР|*") THEN
   DO:
      mSrcChar = ENTRY(2,mParam,"|") . /* код ДР */

      IF term-obl.symbol EQ "Б" THEN
         mPutStr = GetXAttrValueEx("banks",STRING(term-obl.fop),mSrcChar,"").
      ELSE IF term-obl.symbol EQ "Ю" THEN
         mPutStr = GetXAttrValueEx("cust-corp",STRING(term-obl.fop),mSrcChar,"").
      ELSE IF term-obl.symbol EQ "Ч" THEN DO:
		IF mParam MATCHES ("ОбКлДР|phone-home")
		OR mParam MATCHES ("ОбКлДР|cell-phone") THEN DO:
			FIND FIRST person WHERE person.person-id EQ term-obl.fop NO-LOCK NO-ERROR.
			IF AVAILABLE person THEN DO:
				IF person.phone[1] NE "," THEN mPutStr = mPutStr + person.phone[1].
				IF person.phone[2] NE "," THEN mPutStr = mPutStr + person.phone[2].
			END.
		END.
		ELSE IF mParam MATCHES ("ОбКлДР|birthday") THEN DO:
			FIND FIRST person WHERE person.person-id EQ term-obl.fop NO-LOCK NO-ERROR.
			IF AVAILABLE person THEN
				IF person.birthday NE ? THEN mPutStr = STRING(person.birthday).
        END.
		mPutStr = mPutStr + GetXAttrValueEx("person",STRING(term-obl.fop),mSrcChar,"").	 
	  END.
      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.

   /* Доп.реквизит заемщика : */
    IF mParam MATCHES ("КлДР|*") THEN
   DO:
      mSrcChar = ENTRY(2,mParam,"|"). 

      IF loan.cust-cat EQ "Ю" THEN
         mPutStr = GetXAttrValueEx("cust-corp",STRING(loan.cust-id),mSrcChar,"").
      ELSE IF loan.cust-cat EQ "Ч" THEN
         mPutStr = GetXAttrValueEx("person",STRING(loan.cust-id),mSrcChar,"").
      ELSE IF loan.cust-cat EQ "Б" THEN
         mPutStr = GetXAttrValueEx("banks",STRING(loan.cust-id),mSrcChar,"").

      PUT STREAM Fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,mPutStr)
                                     ELSE mPutStr).
   END.

   /* ИНН поручителя : */
   IF mParam MATCHES ("ОбКлИНН") THEN
   DO:
      IF term-obl.symbol EQ "Ч" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF term-obl.symbol EQ "Ю" THEN
      DO :
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE cust-corp THEN DO :
            mPutStr = cust-corp.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF term-obl.symbol EQ "Б" THEN
      DO :
         FIND FIRST banks WHERE banks.bank-id EQ term-obl.fop
            NO-LOCK NO-ERROR .
         IF AVAILABLE banks THEN DO :
            mPutStr = banks.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   END.

   /* ИНН заемщика : */
   IF mParam MATCHES ("КлИНН") THEN
   DO:
      IF loan.cust-cat EQ "Ч" THEN
      DO :
         FIND FIRST person WHERE person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR .
         IF AVAILABLE person THEN DO :
            mPutStr = person.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF loan.cust-cat EQ "Ю" THEN
      DO :
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id
            NO-LOCK NO-ERROR .
         IF AVAILABLE cust-corp THEN DO :
            mPutStr = cust-corp.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
      ELSE IF loan.cust-cat EQ "Б" THEN
      DO :
         FIND FIRST banks WHERE banks.bank-id EQ loan.cust-id
            NO-LOCK NO-ERROR .
         IF AVAILABLE banks THEN DO :
            mPutStr = banks.inn .
            PUT STREAM Fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,mPutStr)
                                           ELSE mPutStr).
         END .
      END .
   END.
   
   /*
   Возвращает дополнительный реквизит по договору
   */
   IF mParam MATCHES ("ДРО|*") THEN
   DO:
      
      mPutStr = GetXAttrValueEx ("term-obl",
                                 Loan.Contract + "," + Loan.Cont-Code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                 ENTRY(2, mParam, '|'),
                                 "").
      
      /*MESSAGE 'ДРО' Loan.Contract + "," + Loan.Cont-Code SKIP 'mPutStr' mPutStr
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      IF NUM-ENTRIES(mParam,"|") EQ 4 AND
         ENTRY(3,mParam,"|") EQ "99.99.9999" THEN
         mPutStr = REPLACE(mPutStr,"/",".").

      IF mPutStr <> "" THEN
      DO:
         /* Если ДР определен на классификаторе, то по коду этого
            классификатора возвращаем его наименование */
         IF GetCodeName(ENTRY(2, mParam, '|'),mPutStr) <> ? THEN
            mPutStr = GetCodeName(ENTRY(2, mParam, '|'),mPutStr).
      END.

      mLength = IF NUM-ENTRIES(mParam,"|") = 3 THEN
                   INT64(ENTRY(2,mParam,"|"))
                ELSE
                   80.
      IF LENGTH(mPutStr) > mLength THEN
      DO:
         mExtStr[1] = mPutStr.
         {wordwrap.i
            &s = mExtStr
            &n = 52
            &l = mLength
         }
         mPutStr = mExtStr[1].

         PUT STREAM Fil UNFORMATTED (IF logTrim 
                                        THEN TrimFormula(chTrim,mPutStr)
                                        ELSE mPutStr).

         DO mCounter = 2 TO 52:

            IF mExtStr[mCounter] = "" THEN
                LEAVE.

            PUT STREAM Fil UNFORMATTED
               SKIP
               mExtStr[mCounter].
         END.
      END.
      ELSE
         PUT STREAM Fil UNFORMATTED (IF logTrim 
                                        THEN TrimFormula(chTrim,mPutStr)
                                        ELSE mPutStr).

   END.
   
   IF mParam MATCHES ("СчетОб|*") THEN
   DO:
      mTmpStr = ENTRY(2,mParam,"|").
      
      ASSIGN
         mSrcChar  = GetXAttrValueEx("term-obl",
                                     loan.contract + "," + loan.cont-code + ",5,"
                                     + STRING(term-obl.end-date) + ","
                                     + STRING(term-obl.nn),
                                     "ВидДогОб",
                                     "")
         mInd = INT64(GetXAttrValueEx ("term-obl",
                                     loan.contract + "," + loan.cont-code + ",5,"
                                     + STRING(term-obl.end-date) + ","
                                     + STRING(term-obl.nn),
                                     "НомерПП",
                                     ?))
         .
      
      ASSIGN 
         mInd2 = IF mInd EQ ? OR mInd = 0 
                    THEN ""
                    ELSE string(mInd).
      
      FIND FIRST loan-acct WHERE
             loan-acct.contract  = term-obl.Contract
         AND loan-acct.cont-code = term-obl.Cont-Code
         AND loan-acct.acct-type = mSrcChar + string(mInd2)
        NO-LOCK NO-ERROR.

      IF NOT AVAIL loan-acct AND mTmpStr NE "Роль" THEN LEAVE.

      FIND FIRST acct WHERE
                 acct.acct     = loan-acct.acct
             AND acct.currency = loan-acct.currency
        NO-LOCK NO-ERROR.
      FIND FIRST code WHERE code.parent EQ "ВидДогОб" AND
                            code.code EQ mSrcChar NO-LOCK NO-ERROR.
                           
      CASE mTmpStr:
          WHEN "Роль" THEN
              mPutStr = code.name.
          WHEN "Ном" THEN 
              mPutStr = delFilFromAcct(acct.acct).
          WHEN "НаимСчета" THEN
          DO:
             {getcust.i
                &name    = vName
                &Offinn  = {comment}
             }
             vName[1] = TRIM (vName[1] + " " + vName[2]).
             mPutStr = vName[1].
          END.
          WHEN "Ост" THEN
          DO:
              RUN acct-pos IN h_base (acct.acct,acct.currency,idate,idate,gop-status ).
              mPutStr = STRING(IF acct.currency EQ "" THEN sh-bal ELSE sh-val).
          END.
      END CASE.
      IF NUM-ENTRIES(mParam,'|') > 2 AND ENTRY(3,mParam,'|') NE "" THEN
      DO:           
         vFormat = ENTRY(3,mParam,'|').
         vFormat = REPLACE(vFormat,"_",",").
         IF mTmpStr EQ "Ост" THEN
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,string(dec(mPutStr),vFormat))
                                           ELSE string(dec(mTmpStr),vFormat)).
         ELSE
         DO:
            vFormat = "x(" + vFormat + ")".
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,string(mPutStr,vFormat))
                                           ELSE string(mTmpStr,vFormat)).
         END.
         mPutStr = "".
      END.
      PUT STREAM fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,STRING(mPutStr))
                                     ELSE STRING(mTmpStr)).
   END.

   IF mParam MATCHES ("Счетчик|*") THEN
   DO:
      mPutStr = ENTRY(2,mParam,"|").

      vTmpRec = RECID(term-obl).
      FOR EACH term-obl OF loan WHERE term-obl.idnt EQ 5:
         vCountTo = vCountTo + 1.
         mSurr = term-obl.contract + "," + term-obl.cont-code  + "," +
                    STRING (term-obl.idnt)     + "," +
                    STRING (term-obl.end-date) + "," +
                    STRING (term-obl.nn).

         CREATE t-sort.
         ASSIGN
            t-sort.order = STRING(vCountTo, "999")
            t-sort.rec-id = RECID(term-obl)
         .
      END.

      FOR EACH t-sort :
          vCountInt = vCountInt + 1.
          IF rec-id EQ vTmpRec THEN LEAVE.
      END.

      vCountInt = vCountInt - INT64(mPutStr) + 1.
      IF vCountInt >= 1 THEN
      PUT STREAM fil UNFORMATTED STRING(vCountInt).
   END.
   
   IF mParam MATCHES ("Счет") THEN
   DO:

      vTmpRec = RECID(term-obl).
      FOR EACH term-obl OF loan WHERE term-obl.idnt EQ 5:
         vCountTo = vCountTo + 1.
         mSurr = term-obl.contract + "," + term-obl.cont-code  + "," +
                    STRING (term-obl.idnt)     + "," +
                    STRING (term-obl.end-date) + "," +
                    STRING (term-obl.nn).

         CREATE t-sort.
         ASSIGN
            t-sort.order = STRING(vCountTo, "999")
            t-sort.rec-id = RECID(term-obl)
         .
      END.

      FOR EACH t-sort :
          vCountInt = vCountInt + 1.
          IF rec-id EQ vTmpRec THEN LEAVE.
      END.
	  
      mPutStr = STRING(vCountInt).
   END.   

   IF mParam MATCHES ("ОбКлДок|*") THEN
   DO:
      mSrcChar = ENTRY(2,mParam,'|').

      CASE mSrcChar:
          WHEN "СпОценки" THEN DO:
             mTemp-Str = "".
             FOR EACH code WHERE code.class  = "ОценкаОбесп" AND
                                 code.parent = "ОценкаОбесп"
             NO-LOCK:
                IF mTemp-Str = ""
                THEN  mTemp-Str = code.name.
                ELSE  mTemp-Str = mTemp-Str + ',' + code.name.
             END.
             mTmpStr  = ENTRY(term-obl.fop-offbal + 1, mTemp-Str).

          END.
          WHEN "КатКач" THEN DO:
             mTmpStr = Get_QualityGar ("comm-rate",
                                       loan.Contract + "," + 
                                       loan.Cont-Code + ",5," + 
                                       STRING(term-obl.end-date) + "," + 
                                       STRING(term-obl.nn), 
                                       iDate).
             IF    mTmpStr EQ ? 
                OR mTmpStr EQ "?"
             THEN 
                mTmpStr = "".
          END.
          WHEN "Количество" THEN DO:
             mTmpStr = string(term-obl.sop-offbal).
          END.
      END CASE.

      IF NUM-ENTRIES(mParam,'|') > 2 THEN
      DO:
         vFormat = ENTRY(3,mParam,'|').
         IF mSrcChar EQ "Количество" THEN 
         DO:
            vFormat = REPLACE(vFormat,"_",",").
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,STRING(INT64(mTmpStr),vFormat))
                                           ELSE STRING(INT64(mTmpStr),vFormat)).
         END.
         ELSE
         DO:
            vFormat = "x(" + vFormat + ")".
            PUT STREAM fil UNFORMATTED (IF logTrim 
                                           THEN TrimFormula(chTrim,STRING(mTmpStr,vFormat))
                                           ELSE STRING(mTmpStr,vFormat)).
         END.
         mTmpStr = "".
      END.
      PUT STREAM fil UNFORMATTED (IF logTrim 
                                     THEN TrimFormula(chTrim,STRING(mTmpStr))
                                     ELSE STRING(mTmpStr)).
   END.
      /* Информация по договору: (второй параметр составной - разделитель ;)
                                 1 параметр - код ДР на договоре
                                 2 параметр - код тега в ДР
                                 (необязательные)
                                 3 параметр - код классификатора, где хранится значение по умолчанию 
                                 4 параметр - код параметра верхнего уровня, если в классификаторе организована иерархия
      */
   IF mParam MATCHES ("ИнформацияПоДоговору|*;*") THEN
   DO:
      vDRCode = ENTRY(2, mParam, "|").
      CASE NUM-ENTRIES(vDRCode, ";"):
         WHEN 2 THEN
            RUN infoloan.p ((BUFFER term-obl:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            "",
                            "",
                            OUTPUT mTmpStr
                            ).
         WHEN 3 THEN
            RUN infoloan.p ((BUFFER term-obl:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            ENTRY (3, vDRCode, ";"),
                            "",
                            OUTPUT mTmpStr
                            ).
         WHEN 4 THEN
            RUN infoloan.p ((BUFFER term-obl:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            ENTRY (3, vDRCode, ";"),
                            ENTRY (4, vDRCode, ";"),
                            OUTPUT mTmpStr
                            ).
      END CASE.
      mPutStr = mTmpStr.
      PUT STREAM fil UNFORMATTED mTmpStr.
      mTmpStr = "".
   END.
   /* вид договора обеспечения */
   IF mParam EQ "ОбВидДог" THEN
   DO:
       mTmpStr = GetXAttrValueEx("term-obl",
                                  loan.contract + "," + loan.cont-code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                  "ВидДогОб",
                                  "").
       mTmpStr = GetCodeName ("ВидДогОб", mTmpStr).
       mPutStr = mTmpStr.
       PUT STREAM fil UNFORMATTED mTmpStr.
       mTmpStr = "".
   END.
   /* вид предмета обеспечения */
   IF mParam EQ "ОбВид" THEN
   DO:
       mTmpStr = GetXAttrValueEx("term-obl",
                                  loan.contract + "," + loan.cont-code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                  "ВидОб",
                                  "").
       mTmpStr = GetCodeName ("ВидОб", mTmpStr).
       mPutStr = mTmpStr.
       PUT STREAM fil UNFORMATTED mTmpStr.
       mTmpStr = "".
   END.   
   IF mParam EQ "ОбВид1" THEN
   DO:
	  mPutStr = "".
      FOR EACH term-obl WHERE term-obl.contract   EQ loan.contract
      AND term-obl.cont-code  EQ loan.cont-code
      AND term-obl.class-code EQ "term-obl-gar" 
      NO-LOCK:   
		   mTmpStr = GetXAttrValueEx("term-obl",
									  loan.contract + "," + loan.cont-code + ",5,"
									  + STRING(term-obl.end-date) + ","
									  + STRING(term-obl.nn),
									  "ВидОб",
									  "").
		   mTmpStr = GetCodeName ("ВидОб", mTmpStr).
		   
		   
		  RUN "x-amtstr.p" (term-obl.amt,
							term-obl.currency,
							NO,
							YES,
							OUTPUT mSummaStr,
							OUTPUT mCurrStr).
		  FIND FIRST currency WHERE currency.currency EQ term-obl.currency NO-LOCK NO-ERROR.
		  vStinstr = STRING(IF term-obl.amt GE 0 THEN term-obl.amt ELSE (- term-obl.amt), "999999999999.99").
		  DO vCounStr = 1 TO 12:
			 digit[vCounStr] = INT64(SUBSTR(vStinstr, 13 - vCounStr, 1)).
		  END.
		  vCurrStr = (IF digit[1] = 1 AND digit[2] <> 1                    THEN IF AVAIL(currency) THEN currency.curr-form1 ELSE "???"
					  ELSE IF digit[2] = 1 OR digit[1] > 4 OR digit[1] = 0 THEN IF AVAIL(currency) THEN currency.curr-form5 ELSE "???"
					  ELSE                                                      IF AVAIL(currency) THEN currency.curr-form2 ELSE "???" ).              
		  vCurrSrt = (IF NUM-ENTRIES(iStrPar,'|') = 1 THEN
						STRING(term-obl.amt-rub,"->>>,>>>,>>>,>>9.99")
					 ELSE
						STRING(term-obl.amt-rub,ENTRY(2,iStrPar,"|"))
						) + " (" + TRIM(mSummaStr) + " " + TRIM(vCurrStr) + " " + TRIM(mCurrStr) + ")".
                
		   
		   
		   IF mPutStr EQ "" THEN
			  mPutStr = mTmpStr + " на сумму " + LEFT-TRIM(vCurrSrt).
		   ELSE
			  mPutStr = mPutStr + ". " + mTmpStr + " на сумму " + LEFT-TRIM(vCurrSrt).
		   mTmpStr = "".
	  END.
	  PUT STREAM fil UNFORMATTED mTmpStr.
	END.
END.
{intrface.del}
RETURN mPutStr.