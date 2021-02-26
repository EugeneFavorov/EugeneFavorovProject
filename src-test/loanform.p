/*                          
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2000 ТОО "Банковские информационные системы"
     Filename: LOANFORM.P
      Comment: Формулы для печати документов из модуля "Кредиты и депозиы".
   Parameters:
      Created: Om 15/02/00
     Modified: Om 31/10/00 Просклонял :) слово "процент".
     Modified: Om 03/09/2001 "АДРЕСЮ" -0 теперь работает не только с юриками,
                             но и с физиками.
    Modified: 15/04/2003 Илюха - формитирование by Becom
                                  comm-rate.acct = "0"
                                  comm-rate.kau  = суррогат договора
    Modified: 26/12/2005 (0053859) ошибка в LOANFORM.P при расчете формулы 
                                   [телефон]
    Modified: 19/07/2007 muta 0075116  Теперь результаты выводятся не только в поток. 
                                       Добавлена обработка формул
                                        - НомДогСтрах                                                
                                       - назван.страх.компании
                                       - ДатДогСтрах
                                       - ФИОЗастрах
                                       - ДатПродажЗаклад
                                       - номер.ГосРег.заклад
                                       - дата.ГосРег.закладной
                                       - РО.НаимРП
    Modified: 19/07/2007 muta 0075117
    Modified: 17/04/2008 jadv 0078661
*/

FORM "~n@(#) loanform.p 1.0 Om 15/02/00"
   WITH FRAME sccs-id STREAM-IO WIDTH-CHARS 250.

{norm.i}
{globals.i}
{svarloan.def}          /* Shared переменные модуля "Кредиты и депозиты". */
{amtsclon.i}
{intrface.get loan}
{intrface.get comm}
{intrface.get cust}     /* Библиотека для работы с клиентами. */
{intrface.get xclass}
{intrface.get strng}
{client.i}

printres = NO.

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"

DEFINE OUTPUT PARAMETER Xresult AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER Xdate1  AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER Xdate   AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER strpar  AS CHARACTER NO-UNDO.

DEF VAR command   AS CHAR NO-UNDO. /* Необходимая функция */
DEF VAR max_len   AS INT64  NO-UNDO. /* Максимальная длина поля */
DEF VAR offset    AS INT64  NO-UNDO. /* Смещение относительно левого края */
DEF VAR mSignsVal AS CHAR NO-UNDO. /* Значение реквизита из cust-corp */
DEF VAR mCommRate AS DEC  NO-UNDO. /* % ставка */
DEF VAR logTrim   AS LOG  NO-UNDO. /* Признак - надо делать trim или нет. Для всех функций. */
DEF VAR chTrim    AS CHAR NO-UNDO. /* Используется в trim (trim,ltrim,rtrim) */
DEF VAR iIndex    AS INT64  NO-UNDO. /* Используется в trim */
DEF VAR mUnit     AS CHAR NO-UNDO.
DEF VAR mRetValue AS CHAR NO-UNDO.
DEF VAR mString   AS CHAR NO-UNDO. /* Переменная для операций со строкой */
DEF VAR is-ok     AS INT64  NO-UNDO.
DEF VAR mAddres   AS CHAR NO-UNDO.
DEF VAR mINN      AS CHAR NO-UNDO.
DEF VAR mKPP      AS CHAR NO-UNDO.
DEF VAR mType     AS CHAR NO-UNDO.
DEF VAR mCode     AS CHAR NO-UNDO.
DEF VAR mAcct     AS CHAR NO-UNDO.
DEF VAR mName     AS CHAR NO-UNDO.
DEF VAR mPhone    AS CHAR NO-UNDO.
DEF VAR mCommRateFixed AS LOGICAL NO-UNDO .
DEF VAR mRes      AS CHAR  NO-UNDO.
DEF VAR mIstStrah AS LOG   NO-UNDO.
DEF VAR mStrahComp AS CHAR NO-UNDO.
DEF VAR mDate   AS DATE NO-UNDO. 
DEF VAR mTemp     AS CHAR NO-UNDO. 
DEF VAR mTemp2    AS CHAR NO-UNDO.



DEFINE  VARIABLE out_str AS CHARACTER NO-UNDO. /* Буфер вывода на печать */

DEF SHARED VAR rid_loan  AS RECID.    /* для закладной */
DEF BUFFER mort-loan  FOR loan.     /* Локализация буфера. */
DEF BUFFER loan-obj   FOR loan.     /* Локализация буфера. */
DEF BUFFER b-tmp-code FOR tmp-code. /* Локализация буфера. */

/*-------------------------------------------------------------------------
** Для функции Trim - урезание не значащих пробелов 
*/
FUNCTION TrimFormula RETURNS CHAR(
   INPUT logTrim    AS LOG, 
   INPUT FormatTrim AS CHAR, 
   INPUT cValue     AS CHAR
):
   IF logTrim THEN
   DO: 
      CASE FormatTrim:
         WHEN "trim"  THEN cValue = TRIM(cValue).
         WHEN "ltrim" THEN cValue = LEFT-TRIM(cValue).
         WHEN "rtrim" THEN cValue = RIGHT-TRIM(cValue).
      END CASE.
   END.
   RETURN cValue.
END.


FIND FIRST loan WHERE
      RECID(loan) EQ rid-p NO-LOCK NO-ERROR.
IF NOT AVAILABLE loan THEN
   IF NOT RetString THEN
   PUT STREAM fil "договор не найден" FORM "x(18)".

{fndinsur.i}

FIND loan-cond WHERE
      RECID(loan-cond) EQ rid-t NO-LOCK NO-ERROR.

IF strpar MATCHES "*trim*" THEN 
   ASSIGN
      logTrim = YES
      iIndex  = INDEX(strpar,",")
      chTrim  = SUBSTRING(strpar,1,iIndex - 1)
      strpar  = SUBSTRING(strpar,iIndex + 1,LENGTH(strpar) - iIndex)
   .

ASSIGN
   COMMAND = ENTRY(1,STRPAR)
   max_len = INT64(IF NUM-ENTRIES(STRPAR) GE 2 THEN ENTRY(1,STRPAR) ELSE '0')
   offset  = INT64(IF NUM-ENTRIES(STRPAR) GE 3 THEN ENTRY(1,STRPAR) ELSE '0')
   .

/* if COMMAND eq "фиорук1" then COMMAND = "фиорук". */

CASE COMMAND:
   WHEN "СчетРасч" THEN
   DO:
      FIND LAST loan-acct OF loan WHERE
                loan-acct.acct-type EQ (IF loan.contract EQ "кредит" THEN
                                           "КредРасч"
                                        ELSE
                                           "ДепРасч")
            AND loan-acct.since     LE Xdate NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-acct THEN
      DO:

         FIND FIRST cust-corp WHERE
                    cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAILABLE cust-corp THEN
            RETURN.

         FIND FIRST acct WHERE
                    acct.contract EQ "Расчет"
                AND acct.cust-cat EQ loan.cust-cat
                AND acct.cust-id  EQ loan.cust-id
                AND acct.currency EQ loan.currency
                AND acct.acct-cat EQ "b"
                AND (acct.close-date EQ ? OR
                     acct.close-date GT Xdate) NO-LOCK NO-ERROR.
      END.
      mRetValue = IF AVAIL loan-acct 
            THEN loan-acct.acct 
            ELSE IF AVAIL acct
                 THEN acct.acct
                               ELSE "".
      mRetValue = TRIM(ENTRY(1, mRetValue, "@")).
      IF NOT RetString THEN 
         PUT STREAM fil TrimFormula(logTrim, chTrim, mRetValue) FORM "x(20)".
      ELSE RETURN mRetValue.
   END.
   WHEN "телефон" THEN
   DO:
      IF loan.cust-cat EQ "Ч" THEN
      DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.

         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF AVAIL person THEN (person.phone[1] + (IF person.phone[2] NE "" THEN ", " + person.phone[2] ELSE ""))
                            ELSE "клиент не найден").
         ELSE RETURN IF AVAILABLE person THEN
               (person.phone[1] + (IF person.phone[2] NE "" THEN
                                      ", " + person.phone[2]
                                   ELSE
                                      ""))
            ELSE "".
      END.
      ELSE
      DO:
         FIND FIRST cust-corp WHERE
                    cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN
            RETURN.

         mSignsVal = GetXAttrValueEx("cust-corp",
                                     STRING(cust-corp.cust-id),
                                     "tel",
                                     ?).
         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF mSignsVal <> ? THEN mSignsVal ELSE "не определен").
         ELSE RETURN (IF mSignsVal <> ? THEN
               mSignsVal ELSE "").
      END.
   END.
   WHEN "факс" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.

      IF NOT RetString THEN
      PUT STREAM fil TrimFormula(logTrim, chTrim, 
         IF AVAIL cust-corp THEN cust-corp.fax
                            ELSE "клиент не найден") FORM "x(20)".
      ELSE RETURN (IF AVAILABLE cust-corp THEN
             cust-corp.fax
          ELSE "").
   END.
   WHEN "email" THEN
   DO:
    IF loan.cust-cat EQ "Ч" THEN
      DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.

         IF NOT AVAIL person THEN
            RETURN.

         mSignsVal = GetXAttrValueEx("person",
                                     STRING(person.person-id),
                                     "e-mail",
                                     ?).
         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF mSignsVal <> ? THEN mSignsVal ELSE "не определен").
         ELSE RETURN (IF mSignsVal <> ? THEN
               mSignsVal ELSE "").
      END.
      ELSE
      DO:
         FIND FIRST cust-corp WHERE
                    cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN
            RETURN.

         mSignsVal = GetXAttrValueEx("cust-corp",
                                     STRING(cust-corp.cust-id),
                                     "e-mail",
                                     ?).
         IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TrimFormula(logTrim, chTrim, 
            IF mSignsVal <> ? THEN mSignsVal ELSE "не определен").
         ELSE RETURN (IF mSignsVal <> ? THEN
               mSignsVal ELSE "").
      END.
   END.
   WHEN "адресю" THEN
   DO:
      IF loan.cust-cat EQ "Ч" THEN
      DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
         IF AVAIL person THEN
            RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрПроп",?,OUTPUT mRes).
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim,
               IF AVAIL person THEN mRes
                               ELSE "клиент не найден") FORM "x(60)".
         ELSE
         DO:
            RETURN (IF AVAILABLE person THEN TRIM(mRes)
                    ELSE "").
         END.
      END.
      ELSE
         IF loan.cust-cat EQ "Ю" THEN
         DO:
            FIND FIRST cust-corp WHERE
                       cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
            IF AVAIL cust-corp THEN
               RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрЮр",?,OUTPUT mRes).
            IF NOT RetString THEN
               PUT STREAM fil TrimFormula(logTrim, chTrim,
                  IF AVAIL cust-corp THEN mRes
                                     ELSE "клиент не найден") FORM "x(60)".
            ELSE RETURN (IF AVAILABLE cust-corp THEN
                            TRIM(mRes)
                         ELSE "").
         END.
   END.
   WHEN "рекв_юл" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.

      IF NOT RetString THEN
         PUT STREAM fil TrimFormula(logTrim, chTrim, 
            IF AVAIL cust-corp THEN "ИНН " + TRIM(cust-corp.inn)
                               ELSE "") FORM "x(20)".
      ELSE RETURN (IF AVAILABLE cust-corp THEN
                      ("ИНН " + TRIM(cust-corp.inn))
                   ELSE "").

   END.
   WHEN "рекв_б" THEN
   DO:
         /* Тэг сделан только для юриков, добавим проверочку, иначе по физикам дает ложные данные */
      IF loan.cust-cat NE "Ю" THEN
         RETURN.
      FIND LAST loan-acct OF loan WHERE
                loan-acct.acct-type EQ (IF loan.contract EQ "кредит" THEN
                                           "КредРасч"
                                        ELSE
                                           "ДепРасч")
            AND loan-acct.since     LE Xdate NO-LOCK NO-ERROR.

      IF AVAILABLE loan-acct THEN
         FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.

      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cust-corp THEN
         RETURN.

      IF NOT AVAILABLE loan-acct OR NOT AVAILABLE acct THEN
      DO:
         FIND FIRST acct WHERE
                    acct.contract EQ "Расчет"
                AND acct.cust-cat EQ loan.cust-cat
                AND acct.cust-id  EQ loan.cust-id
                AND acct.currency EQ loan.currency
                AND acct.acct-cat EQ "b"
                AND (acct.close-date EQ ? OR
                     acct.close-date GT Xdate) NO-LOCK NO-ERROR.
      END.

      IF AVAILABLE acct THEN
      DO:
         {bank-id.i}

         FIND FIRST banks OF banks-corr WHERE
                    banks.flag-rkc NO-LOCK NO-ERROR.
         IF NOT AVAILABLE banks THEN
            RETURN.

         out_str = "р/с " + delFilFromAcct(acct.acct) + ", к/с " + delFilFromAcct(bank-acct) + "~nв " +
            TRIM(banks.name) + " г. " + banks.town + ", БИК " + bank-mfo-9.

         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, out_str) FORM "x(130)".
         ELSE RETURN out_str.
      END.
      ELSE
      DO:
         IF cust-corp.bank-code-type NE "ИНН" THEN
         DO:
            FIND banks-code WHERE
                 banks-code.bank-code-type EQ  cust-corp.bank-code-type
             AND banks-code.bank-code      EQ cust-corp.bank-code
            NO-LOCK NO-ERROR.

            IF NOT AVAILABLE banks-code THEN
               RETURN.

            FIND FIRST banks WHERE
                       banks.bank-id EQ banks-code.bank-id NO-LOCK NO-ERROR.
            IF NOT AVAILABLE banks THEN
               RETURN.
         END.
         ELSE
         DO:
            FIND FIRST cust-ident WHERE cust-ident.cust-cat       EQ "Б"
                                  AND   cust-ident.cust-code-type EQ "ИНН"
                                  AND   cust-ident.cust-code      EQ cust-corp.bank-code
               NO-LOCK NO-ERROR.
            IF NOT AVAIL cust-ident THEN
               RETURN.
            FIND FIRST banks WHERE banks.bank-id EQ cust-ident.cust-id NO-LOCK NO-ERROR.
            IF NOT AVAIL banks THEN
               RETURN.
         END.

         {getcode.i banks "МФО-9"}
         IF NOT AVAILABLE banks-code THEN
            RETURN.
         out_str = "р/с " + delFilFromAcct(cust-corp.benacct) + ", к/с " + delFilFromAcct(cust-corp.corr-acct)
            + " в " + TRIM(banks.name) + " г. " + banks.town + ",~n БИК " +
               banks-code.bank-code.

         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, out_str) FORM "x(100)".
         ELSE RETURN out_str.
      END.
   END.
   WHEN "долрук" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "ДолРук",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(20)".
         ELSE RETURN TRIM(mSignsVal).
      END.
   END.
   WHEN "фиорук" THEN
   DO:
      FIND FIRST cust-corp WHERE
            cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "ФИОРук",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.
   END.
   WHEN "основа" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "основа",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "адресп" THEN
   DO:
      RUN GetCustAdr("Ю",loan.cust-id,xdate,"АдрПочт",OUTPUT TABLE ttCustAddress).
      FIND LAST ttCustAddress NO-LOCK NO-ERROR.
      mSignsVal = IF AVAIL ttCustAddress THEN ttCustAddress.fAdrStr ELSE ?.

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.
   END.
   WHEN "фиобухг" THEN
   DO:
      FIND FIRST cust-corp WHERE
                 cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
      IF NOT AVAIL cust-corp THEN
         RETURN.

      mSignsVal = GetXAttrValueEx("cust-corp",
                                  STRING(cust-corp.cust-id),
                                  "фиобухг",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(32)".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   /*фактическая дата выдачи кредита*/
   WHEN "датавыд" THEN
   DO:
      mSignsVal = STRING(loan.open-date, "99.99.9999").
      find first loan-int of loan 
           where loan-int.id-k = 3
             and loan-int.id-d = 0
             and loan-int.mdate >= loan.open-date
             and loan-int.amt-rub <> 0
      no-lock no-error.
      if avail loan-int then mSignsVal = STRING(loan-int.mdate, "99.99.9999").
      RETURN TRIM(mSignsVal).
   END.
   WHEN "датасогл" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "датасогл",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(10)".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "датасогл2" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "датасогл",
                                  ?).

      IF mSignsVal <> ? THEN DO:
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "99.99.9999".
         ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   /*добавлены теги "деньсогл" и "этцсогл" для вывода даты соглашения в формате 99 месяц 9999*/
   WHEN "деньсогл" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "датасогл",
                                  ?).

    IF mSignsVal <> ? THEN DO:

    mDate = DATE(mSignsVal).
      mSignsVal = STRING(DAY(mDate)).
    IF NOT RetString THEN
      PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(2)".
    ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "этцсогл" THEN
   DO:
      mSignsVal = GetXAttrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "датасогл",
                                  ?).

    IF mSignsVal <> ? THEN DO:

    mDate = DATE(mSignsVal).
      mSignsVal = ENTRY(MONTH(mDate),{&Months}) + " " + STRING(YEAR(mDate)).
    IF NOT RetString THEN
      PUT STREAM fil TrimFormula(logTrim, chTrim, mSignsVal) FORM "x(10)".
    ELSE RETURN TRIM(mSignsVal).
      END.

   END.
   WHEN "К%КрПр" THEN
   DO:
      IF NOT AVAILABLE loan-cond THEN
         RETURN.

      mCommRate = GET_COMM ("%КрПр",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).
      mCommRateFixed = GET_COMM_TYPE ("%КрПр",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).


      IF mCommRate <> ? THEN
      DO:
         RUN "amt.p" (mCommRate, OUTPUT out_str).
         out_str = out_str + "|".
         RUN persent ( mCommRate , loan.currency , INPUT-OUTPUT out_str).
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(mCommRate,">>99.99999") +
               "(" + ENTRY(1,out_str, "|") + ")" + ENTRY(2, out_str, "|")) FORMAT "x(60)".
         ELSE RETURN (STRING(mCommRate,">>99.99999") +
               "(" + ENTRY(1,out_str, "|") + ")" +
               ENTRY(2, out_str, "|")).
      END.
   END.
   WHEN "К%КрПр%" THEN
   DO:
      IF NOT AVAILABLE loan-cond THEN
         RETURN.

      mCommRate = GET_COMM ("%КрПр%",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).
      mCommRateFixed = GET_COMM_TYPE ("%КрПр%",
                            ?,
                            loan.currency,
                            loan.contract + "," + loan.cont-code,
                            0.0,
                            0,
                            loan-cond.since).


      IF mCommRate <> ? THEN
      DO:
         RUN "amt.p" (mCommRate, OUTPUT out_str).
         RUN persent (mCommRate, mCommRateFixed , loan.currency ,  INPUT-OUTPUT out_str).
         IF NOT RetString THEN
            PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(mCommRate,">>99.99999") +
               "(" + out_str + ")") FORMAT "x(64)".
            ELSE RETURN (STRING(mCommRate,">>99.99999") +
                     "(" + out_str + ")").
      END.
   END.
   WHEN "НомДогСтрах" THEN
   DO:
      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, 
         IF loan.class-code EQ "insurance" AND loan.contract   EQ 'СТРАХ' THEN loan.cont-code ELSE "").
      ELSE RETURN 
         (IF loan.class-code EQ "insurance" AND loan.contract   EQ 'СТРАХ' THEN loan.cont-code ELSE "").

   END. 
   WHEN "назван.страх.компании" THEN
   DO:
      IF loan.class-code EQ "insurance" AND loan.contract   EQ 'СТРАХ' THEN DO:

         mIstStrah = FGetSetting("ИстСтрах", "", "ЮЛ") EQ "ЮЛ".
         IF mIstStrah THEN
            RUN RE_CLIENT (loan.cust-cat,
                           loan.cust-id,
                           INPUT-OUTPUT mName).
         ELSE
            ASSIGN
               mStrahComp  = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"СтраховКомп","")
               mName       = GetCodeName('СтраховКомп', mStrahComp).
         IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, mName).
         ELSE RETURN mName.
      END.
   END.
   WHEN "ДатДогСтрах" THEN
   DO:
      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, 
        IF loan.class-code EQ "insurance" AND loan.contract   EQ 'СТРАХ' THEN string(loan.open-date) ELSE "").
     ELSE RETURN 
        (IF loan.class-code EQ "insurance" AND loan.contract   EQ 'СТРАХ' THEN string(loan.open-date) ELSE "").
   END.
   WHEN "ФИОЗастрах" THEN

   FOR FIRST cust-role WHERE 
             cust-role.FILE-NAME  EQ "loan"   
         AND cust-role.surrogate  EQ "СТРАХ," + loan.cont-code
         AND cust-role.class-code EQ "Застрахованный"
             NO-LOCK:
 
      IF {assigned cust-role.cust-name}
         THEN mName = cust-role.cust-nam.
         ELSE  RUN RE_CLIENT (cust-role.cust-cat,
                              cust-role.cust-id,
                              INPUT-OUTPUT mName).     

      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, mName).
      ELSE RETURN mName.
   END.
   WHEN "ДатПродажЗаклад" THEN
   DO: 
      FOR  FIRST mort-loan WHERE
           RECID(mort-loan) EQ rid_loan
            AND  mort-loan.class-code eq "loan-mort"
            AND  mort-loan.contract EQ 'Заклад' 
                 NO-LOCK,
           FIRST loan-obj WHERE 
                 loan-obj.class-code EQ "deal-mort"
             AND loan-obj.sec-code   EQ mort-loan.cont-code 
             AND loan-obj.contract   EQ 'КредРО'  
             and loan-obj.deal-type  EQ NO               /* Сделка продажи */
                 NO-LOCK:
      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(loan-obj.open-date)).
         ELSE RETURN STRING(loan-obj.open-date).
      END.
   END.
   WHEN "номер.ГосРег.заклад" THEN
   DO:
      FOR  FIRST mort-loan WHERE
           RECID(mort-loan) EQ rid_loan
            AND  mort-loan.class-code eq "loan-mort"
            AND  mort-loan.contract EQ 'Заклад' 
                 NO-LOCK,
           FIRST loan-obj WHERE 
                 loan-obj.class-code EQ "mort-obj"
             AND loan-obj.contract EQ 'Залог' 
             AND  loan-obj.parent-cont-code EQ mort-loan.cont-code 
             AND loan-obj.parent-contract  EQ 'Заклад' NO-LOCK:

      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, loan-obj.reg-num).
         ELSE RETURN loan-obj.reg-num.

      END.
   END.
   WHEN "дата.ГосРег.закладной" THEN
   DO:
       FOR  FIRST mort-loan WHERE
           RECID(mort-loan) EQ rid_loan
            AND  mort-loan.class-code eq "loan-mort"
            AND  mort-loan.contract EQ 'Заклад' 
                 NO-LOCK,
           FIRST loan-obj WHERE 
                 loan-obj.class-code EQ "mort-obj"
             AND loan-obj.contract EQ 'Залог' 
             AND loan-obj.parent-cont-code EQ mort-loan.cont-code
             AND loan-obj.parent-contract  EQ 'Заклад' NO-LOCK:

      IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, STRING(loan-obj.open-date)).
         ELSE RETURN STRING(loan-obj.open-date).

      END.
   END.  
   WHEN "КредДог.ДатаСогл" THEN
   DO:
      FOR FIRST mort-loan WHERE
                mort-loan.contract EQ 'Кредит'
            AND mort-loan.cont-code EQ loan.parent-cont-code
                NO-LOCK:
         IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, 
            GetXattrValue("loan",mort-loan.contract + "," + mort-loan.cont-code,"ДатаСогл")).
         ELSE RETURN GetXattrValue("loan",mort-loan.contract + "," + mort-loan.cont-code,"ДатаСогл").
      END.
   END.
   WHEN "КредДог.Номер" THEN
   DO:
      IF loan.parent-contract EQ 'Кредит' THEN
         IF NOT RetString THEN PUT STREAM fil TrimFormula(logTrim, chTrim, loan.parent-cont-code).
         ELSE RETURN loan.parent-cont-code.
   END.
       /* Формулы по региональному оператору можно будет написать, когда
          будет реализованно ведение договоров с РО в кредитном модуле. */
   WHEN "РО.НаимРП" THEN
   DO:

   END.
   WHEN "БанкРС" THEN
   DO:

   END.
   WHEN "РО.ЮрАдрес" THEN
   DO:

   END.
   WHEN "РО.ИНН" THEN
   DO:

   END.
   WHEN "РО.РС" THEN
   DO:

   END.
   WHEN "День" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil STRING (DAY(TODAY)).
      ELSE RETURN STRING (DAY(TODAY)).
   END.
   WHEN "Год" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil STRING (YEAR(TODAY)).
      ELSE RETURN STRING (YEAR(TODAY)).
   END.
   WHEN "МесяцПроп" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil ENTRY(MONTH(TODAY),
                       {&Months}).
      ELSE RETURN ENTRY(MONTH(TODAY),
                       {&Months}).
   END.
   WHEN "ДатаС" THEN
   DO:
      IF NOT RetString THEN
            PUT STREAM fil STRING(TODAY,"99.99.9999").
      ELSE RETURN STRING(TODAY,"99.99.9999").
   END.
   WHEN "ДогВал" THEN
   DO:
      FIND FIRST currency WHERE
         currency.currency EQ loan.currency
      NO-LOCK NO-ERROR.
      IF AVAIL currency THEN
         IF NOT RetString THEN
            PUT STREAM fil STRING(currency.name-currenc).
         ELSE
            RETURN string(currency.name-currenc).
   END.
   WHEN "ДогСум" THEN
   DO:                       
      FIND FIRST term-obl WHERE
         term-obl.contract  EQ loan.contract  AND
         term-obl.cont-code EQ loan.cont-code AND
         term-obl.idnt      EQ 2
      NO-LOCK NO-ERROR.
      IF AVAILABLE term-obl THEN
         IF NOT RetString THEN
            PUT STREAM fil TRIM(STRING(term-obl.amt,'->>>,>>>,>>>,>>9.99')).
         ELSE
            RETURN TRIM(STRING(term-obl.amt,'->>>,>>>,>>>,>>9.99')).
   END.
   WHEN "КредПДП" THEN
   DO:
      RUN GET_COMM_BUF("КредПДП" , 
                        ?,         
                        loan.currency,
                        loan.contract + "," + loan.cont-code, 
                        0.00,                                 
                        0,                                    
                        Xdate,
                        BUFFER comm-rate).
      IF AVAIL comm-rate THEN
      DO:
         IF comm-rate.rate-fixed EQ YES THEN
         DO:
            FIND FIRST currency WHERE
                    currency.currency EQ loan.currency
            NO-LOCK NO-ERROR.
            IF AVAIL currency THEN DO:
            
               mUnit = currency.name-currenc.
            END.
            ELSE mUnit = "%". 
         END.
         ELSE mUnit = "%".
         mRetValue = STRING(comm-rate.rate-comm) + mUnit.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue. 
   END.
   WHEN "КредЧДП" THEN
   DO:
      RUN GET_COMM_BUF("КредЧДП" , 
                        ?,         
                        loan.currency,
                        loan.contract + "," + loan.cont-code, 
                        0.00,                                 
                        0,                                    
                        Xdate,
                        BUFFER comm-rate).
      IF AVAIL comm-rate THEN
      DO:
         IF comm-rate.rate-fixed EQ YES THEN
         DO:
            FIND FIRST currency WHERE
                    currency.currency EQ loan.currency
            NO-LOCK NO-ERROR.
            IF AVAIL currency THEN
               mUnit = currency.name-currenc.
            ELSE mUnit = "%". 
         END.
         ELSE mUnit = "%".
         mRetValue = STRING(comm-rate.rate-comm) + mUnit.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue. 
   END.
   WHEN "ПеняПрКр" THEN
   DO:
      mCommRate = GET_COMM("КрПр%" ,
                           ?,         
                           loan.currency,
                           loan.contract + "," + loan.cont-code, 
                           0.00,                                 
                           0,                                    
                           Xdate).               
      IF mCommRate EQ ? THEN
         mCommRate = GET_COMM("Пеня-К" ,
                              ?,         
                              loan.currency,
                              loan.contract + "," + loan.cont-code, 
                              0.00,                                 
                              0,                                    
                              Xdate).   
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
      ELSE
         RETURN trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
   END.
   WHEN "ПеняПр%%" THEN
   DO:
      mCommRate = GET_COMM("%КрПр%" , 
                           ?,         
                           loan.currency,
                           loan.contract + "," + loan.cont-code, 
                           0.00,                                 
                           0,                                    
                           Xdate).
      IF mCommRate EQ ? THEN
         mCommRate = GET_COMM("Пеня%К" ,
                              ?,         
                              loan.currency,
                              loan.contract + "," + loan.cont-code, 
                              0.00,                                 
                              0,                                    
                              Xdate).   
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
      ELSE
         RETURN trim(string(mCommRate,">>>>>>>>>>>>>>9.99")).
   END.
   WHEN "ПродНаим" THEN
   DO:
         /* Ищем запись по продукту в классификаторе ПродЛин и возвращает его наименование,
         ** Иначе - пусто. */
      mString = GetXAttrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "ПродКод",
                                "").
      FIND LAST tmp-code WHERE
                tmp-code.class    EQ "ПродЛин"
         AND    tmp-code.code     EQ mString
         AND    tmp-code.beg-date LE Xdate
      NO-LOCK NO-ERROR.
      mRetValue = IF AVAIL tmp-code 
                     THEN  tmp-code.name 
                     ELSE  ?.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue.
   END.
   WHEN "ПродНаимВыш" THEN
   DO:
         /* Ищем по коду продукта кредита в классификаторе ПродЛин запись продукта, 
         ** для этой записи - родителя и возвращаем наименование родительской записи. 
         ** Иначе - пусто. */
      ASSIGN
         mRetValue = ?
         mString   = GetXAttrValueEx("loan",
                                     loan.contract + "," + loan.cont-code,
                                     "ПродКод",
                                     "")
      .
      FOR FIRST tmp-code WHERE
                tmp-code.class    EQ 'ПродЛин'
         AND    tmp-code.code     EQ mString
         AND    tmp-code.beg-date LE Xdate
         NO-LOCK,
         FIRST code OF tmp-code
         NO-LOCK,
            FIRST b-tmp-code WHERE
                  b-tmp-code.class    EQ 'ПродЛин'
            AND   b-tmp-code.code     EQ code.parent
            AND   b-tmp-code.beg-date LE Xdate
      NO-LOCK:
         mRetValue = b-tmp-code.name.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED mRetValue.
      ELSE
         RETURN mRetValue.
   END.
   WHEN "ПродТариф" THEN
   DO:
      mName = GetXAttrValueEx("loan",
                               loan.contract + "," + loan.cont-code,
                               "ПродТариф",
                               "").
      FIND FIRST code WHERE 
         code.code EQ mName
      NO-LOCK NO-ERROR.
      IF AVAIL code THEN
         IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED code.name.
         ELSE
            RETURN code.name.
      ELSE RETURN "".
   END.
   WHEN "Сум%Откр" THEN
   DO:
      FIND FIRST loan-int WHERE loan-int.contract  EQ loan.contract                            AND loan-int.cont-code EQ loan.cont-code 
                            AND loan-int.id-d      EQ 410
                            AND loan-int.id-k      EQ 5
      NO-LOCK NO-ERROR.

      FIND FIRST op-entry OF loan-int NO-LOCK NO-ERROR.

      IF AVAIL op-entry THEN
      DO:
         IF loan.currency EQ "" THEN
            mRetValue = trim(string(op-entry.amt-rub,">>>>>>>>>>>>>>9.99")).
         ELSE
            mRetValue = trim(string(op-entry.amt-cur,">>>>>>>>>>>>>>9.99")).
      END.
      IF mRetValue NE "" THEN
         IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED STRING(mRetValue).
         ELSE
         RETURN STRING(mRetValue).
      ELSE 
      DO:
         FIND FIRST term-obl WHERE
            term-obl.contract  EQ loan.contract  AND
            term-obl.cont-code EQ loan.cont-code AND
            term-obl.idnt      EQ 2
         NO-LOCK NO-ERROR.
         IF AVAIL term-obl THEN
         DO:
            RUN CalcCommStart IN h_loan ("%Откр",term-obl.amt,loan.contract,loan.cont-code,loan.currency,"?",OUTPUT mCommRate,OUTPUT is-ok).
            /* Если не успешно, то не ругаемся в самой функции, а считаем комиссию равной нулю */
            IF is-ok EQ 0 THEN
               mCommRate = 0.
            IF NOT RetString THEN
               PUT STREAM fil UNFORMATTED STRING(mCommRate).
            RETURN TRIM(STRING(mCommRate,">>>>>>>>>>>>>>9.99")).
         END.
         ELSE
            RETURN "".
      END.
   END.
   WHEN "Сум%Выд" THEN
   DO:
      BLCK:
      DO:
         FIND FIRST loan-int WHERE loan-int.contract  EQ loan.contract 
                               AND loan-int.cont-code EQ loan.cont-code 
                               AND loan-int.id-d      EQ 173
                               AND loan-int.id-k      EQ 5
         NO-LOCK NO-ERROR.
    
         IF AVAIL loan-int THEN
            mRetValue = STRING(loan-int.amt-rub).
   
         FIND FIRST op-entry OF loan-int NO-LOCK NO-ERROR.
   
         IF AVAIL op-entry THEN
         DO:
            IF loan.currency EQ "" THEN
               mRetValue = trim(string(op-entry.amt-rub,">>>>>>>>>>>>>>9.99")).
            ELSE
               mRetValue = trim(string(op-entry.amt-cur,">>>>>>>>>>>>>>9.99")).
         END.
         IF mRetValue NE "" THEN
            LEAVE BLCK.
         ELSE 
         DO:
            FIND FIRST term-obl WHERE
               term-obl.contract  EQ loan.contract  AND
               term-obl.cont-code EQ loan.cont-code AND
               term-obl.idnt      EQ 2
            NO-LOCK NO-ERROR.
            IF AVAIL term-obl THEN
            DO:
               RUN CalcCommStart IN h_loan ("%Выд",term-obl.amt,loan.contract,loan.cont-code,loan.currency,"?",OUTPUT mCommRate,OUTPUT is-ok).
               /* Если не успешно, то не ругаемся в самой функции, а считаем комиссию равной нулю */
               IF is-ok EQ 0 THEN
                  mCommRate = 0.
               mRetValue = STRING(mCommRate).
            END.
         END.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
      RETURN TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
   END.
   WHEN "Сум%ПрСтрах" THEN
   DO:
      BLCK:
      DO:
         FIND FIRST loan-int WHERE loan-int.contract  EQ loan.contract 
                               AND loan-int.cont-code EQ loan.cont-code 
                               AND loan-int.id-d      EQ 175
                               AND loan-int.id-k      EQ 5
         NO-LOCK NO-ERROR.
    
         IF AVAIL loan-int THEN
            mRetValue = STRING(loan-int.amt-rub).
   
         FIND FIRST op-entry OF loan-int NO-LOCK NO-ERROR.
   
         IF AVAIL op-entry THEN
         DO:
            IF loan.currency EQ "" THEN
               mRetValue = string(op-entry.amt-rub).
            ELSE
               mRetValue = string(op-entry.amt-cur).
         END.
         IF mRetValue NE "" THEN
            LEAVE BLCK.
         ELSE 
         DO:
            FIND FIRST term-obl WHERE
               term-obl.contract  EQ loan.contract  AND
               term-obl.cont-code EQ loan.cont-code AND
               term-obl.idnt      EQ 2
            NO-LOCK NO-ERROR.
            IF AVAIL term-obl THEN
            DO:
               mRetValue = STRING(term-obl.amt-rub * GET_COMM_LOAN(loan.contract,loan.cont-code,"%ПрСтрах",loan.open-date) / 100).
            END.
         END.
      END.
      IF NOT RetString THEN
         PUT STREAM fil UNFORMATTED TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
      ELSE
         RETURN TRIM(STRING(DEC(mRetValue),">>>>>>>>>>>>>>9.99")).
   END.   




/*Новый формат адреса zss*/
  WHEN "аАДУ" THEN
    DO:
 

 RUN pb_newadr.p(loan.cust-cat,loan.cust-id,OUTPUT mString).
       	
   IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString.


  END.
   


   /*добавлен тег чАдрПропО для добавления области в адрес прописки*/
   WHEN "чАдрПропО" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрПроп",Xdate,OUTPUT mString).
     /*message mString view-as alert-box.*/
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString.
   END.
   /*--------------------------------------------------*/
   WHEN "чАдрПроп" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрПроп",Xdate,OUTPUT mString).
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString. 
   END.
   WHEN "чАдрФакт" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрФакт",Xdate,OUTPUT mString).
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString. 
   END.
   /* ayv - при отсутствии фактического адреса вписывать адрес прописки */
   WHEN "чАдрФактП" THEN
   DO:
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрФакт",Xdate,OUTPUT mString).
      IF mString EQ "" THEN
        RUN RetAdr.p(loan.cust-id,loan.cust-cat,"АдрПроп",Xdate,OUTPUT mString).  
    IF NOT RetString THEN
            PUT STREAM fil UNFORMATTED mString.
         ELSE
            RETURN mString. 
   END.
{lnfrmoms.i}
END CASE.
{intrface.del comm}