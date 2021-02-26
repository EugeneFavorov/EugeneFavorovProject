{globals.i}
{intrface.get tmess}

/* +++ pp-strng.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: pp-strng.p
      Comment: Основные функции и процедуры для работы со строками и списками.
               Обращение к процедурам через handle h_str
   Parameters: Нет
      Created: 10/09/2003   fedm
     Modified: 14/08/2003 Om Доработка.
                             Объединены библиотеки по работе со строками.
     Modified: 19.09.2005 19:20 KSV      (0046989) Добавлена функция                             
                                         SortDelimList, для сортировки списков
                                         строк.
     Modified: 29.09.2005 19:23 KSV      (0050131) Добавлены функции BQEncode,
                                         BQDecode и GetUniqueFileName.
     Modified: 06.05.2006 15:48 VASOV    (0061624) Добавлена функция
                                         DelDoubleChars

   F/P  Name                   Comment
   ───  ─────────────────────  ──────────────────────────────────────────
    F   FStrEmpty              Строка пустая?

    F   FStrNVL                Строка пустая (результат - строка)?

    F   FStrCat                Конкатенация строк asSrc || asCat
                               с разделителем asDelim
    F   FStrPad                Дополнение строки asStr пробелами
                               до длины anLen справа или слева
    F   FStrPadC               Дополнение строки asStr символами
                               до длины anLen справа или слева
    F   FStrCenter             Центрирование строки asStr
                               по ширине anLen символов
    F   GetEntries             Надежное получение элемента списка

    F   SetEntries             Надежное присвоение значения для элемента списка
    
    F   PADR                   Дополнение пробелами справа

    F   PADL                   Дополнение пробелами слева

    F   PADС                   Центрирует строку

    P   PutScreen              Вывод информации на экран


    F   ConvMatch2Beg          Получить BEGINS-шаблон по MATCHES-шаблону.

    P   SetLstParam            Установить значение параметра с указанным кодом.

    P   DelLstParam            Удалить параметры с указанными кодами.

    F   GetLstParam            Получить значение параметра с указанным кодом.

    F   GetLstParamS           По списку параметров возвращает список значений.

    F   GetEqParamList         Получить список параметров с указанным значением.

    F   DelDoubleChars         Удалить из строки повторы заданного символа

    F   GetValueRange          Получение заданного диапазона из строки,
                               выход её за пределы с какой-либо стороны
                               дополняет результат специальным символом

    P   ParseIntRange          Синтаксический разбор выражения, задающего 
                               целочисленный интервал
    P   CompareAsInt           Сравнение двух строк как целых чисел
*/

{pfuncdef
   &DefLib="strng"
   &Description="Библиотека работы со строками и списками"}

&GLOBAL-DEFINE pp-str

/* Commented by KSV: Библиотека функций для обработки регулярных выражений
** !!! Подключена как инклюд, т.к. файл может использоваться не только в
** в БИСКВИТе  */
{pp-re.p}

/* Commented by KSV: Библиотека функций для обработки строк */
{strings.fun}

/* Процедура разбора реквизитов person.document и opb.Докум */
{docum-sn.pro}

/*----------------------------------------------------------------------------*/
/* По шаблону для MATCHES возвращает шаблон для BEGINS                        */
/*----------------------------------------------------------------------------*/
FUNCTION ConvMatch2Beg  RETURNS CHAR
   (iMatch AS CHAR):

   &SCOPED-DEFINE SpecChar "*,."

   /* Счётчик */
   DEF VAR vCnt  AS INT64  NO-UNDO.
   /* Позиция в строке */
   DEF VAR vPos  AS INT64  NO-UNDO.

   DO vCnt = 1 TO NUM-ENTRIES({&SpecChar}):
      vPos = INDEX(iMatch, ENTRY(vCnt, {&SpecChar})).

      IF vPos > 0 THEN
        iMatch = SUBSTR(iMatch, 1, vPos - 1).
   END.

   RETURN iMatch.

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Устанавливает значение параметра списка с указанным кодом                  */
/*----------------------------------------------------------------------------*/
PROCEDURE SetLstParam:
   DEF INPUT        PARAMETER iCod      AS CHAR  NO-UNDO.  /* Код параметра   */
   DEF INPUT        PARAMETER iVal      AS CHAR  NO-UNDO.  /* Значение пар-ра */
   DEF INPUT-OUTPUT PARAMETER ioCodLst  AS CHAR  NO-UNDO.  /* Список кодов    */
   DEF INPUT-OUTPUT PARAMETER ioValLst  AS CHAR  NO-UNDO.  /* Список значений */
   DEF INPUT        PARAMETER iDlm      AS CHAR  NO-UNDO.  /* Разделитель     */

   /* Счётчик */
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.

   vNum = NUM-ENTRIES(ioCodLst, iDlm).

   DO vCnt = 1 TO vNum:
      IF ENTRY(vCnt, ioCodLst, iDlm) = iCod THEN
      DO:
         ENTRY(vCnt, ioValLst, iDlm) = iVal.
         RETURN.
      END.
   END.

   IF ioCodLst = "" THEN
      ASSIGN
         ioCodLst = iCod
         ioValLst = iVal.
   ELSE
      ASSIGN
         ioCodLst = iCod + iDlm + ioCodLst
         ioValLst = iVal + iDlm + ioValLst.

   RETURN.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Удалить параметры с указанными кодами                                      */
/* (первый параметр - список удаляемых кодов через ЗАПЯТУЮ)                   */
/*----------------------------------------------------------------------------*/
PROCEDURE DelLstParam:
   DEF INPUT        PARAMETER iDelCodes AS CHAR  NO-UNDO.  /* Удаляемые коды  */
   DEF INPUT-OUTPUT PARAMETER ioCodLst  AS CHAR  NO-UNDO.  /* Список кодов    */
   DEF INPUT-OUTPUT PARAMETER ioValLst  AS CHAR  NO-UNDO.  /* Список значений */
   DEF INPUT        PARAMETER iDlm      AS CHAR  NO-UNDO.  /* Разделитель     */

   /* Счётчик */
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.
   /* Новый список кодов */
   DEF VAR vCodLst AS CHAR NO-UNDO.
   /* Новый список значений */
   DEF VAR vValLst AS CHAR NO-UNDO.

   vNum = NUM-ENTRIES(ioCodLst, iDlm).
   DO vCnt = 1 TO vNum:
      IF LOOKUP(ENTRY(vCnt, ioCodLst, iDlm), iDelCodes) = 0 THEN
         ASSIGN
            vCodLst = vCodLst + iDlm + ENTRY(vCnt, ioCodLst, iDlm)
            vValLst = vValLst + iDlm + ENTRY(vCnt, ioValLst, iDlm).
   END.

   ASSIGN
      ioCodLst = SUBSTR(vCodLst, LENGTH(iDlm) + 1)
      ioValLst = SUBSTR(vValLst, LENGTH(iDlm) + 1).

   RETURN.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Возвращает значение параметра списка с указанным кодом                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetLstParam     RETURNS CHAR
   (iCod     AS CHAR, /* Код параметра      */
    iCodLst  AS CHAR, /* Список кодов       */
    iValLst  AS CHAR, /* Список значений    */
    iDlm     AS CHAR  /* Разделитель        */
   ):

   /* Счётчик */
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.

   vNum = NUM-ENTRIES(iCodLst, iDlm).

   DO vCnt = 1 TO vNum:
      IF ENTRY(vCnt, iCodLst, iDlm) = iCod THEN
         RETURN ENTRY(vCnt, iValLst, iDlm).
   END.

   RETURN ?.

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* По списку параметров возвращает список их значений                         */
/* (первый параметр - список кодов через ЗАПЯТУЮ)                             */
/*----------------------------------------------------------------------------*/
FUNCTION GetLstParamS  RETURNS CHAR
   (iCod     AS CHAR, /* Коды параметров    */
    iCodLst  AS CHAR, /* Список кодов       */
    iValLst  AS CHAR, /* Список значений    */
    iDlm     AS CHAR  /* Разделитель        */
   ):

   /* Счётчик */
   DEF VAR vCnt  AS INT64   NO-UNDO.
   /* Возвращаемый список значений */
   DEF VAR vLst  AS CHAR  NO-UNDO.

   vLst = REPLACE(iCod, ",", iDlm).

   DO vCnt = 1 TO NUM-ENTRIES(iCod):
      ENTRY(vCnt, vLst, iDlm) = GetLstParam(ENTRY(vCnt, iCod),
                                            iCodLst,
                                            iValLst,
                                            iDlm
                                           ) NO-ERROR.
   END.

   RETURN vLst.

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Возвращает список кодов параметров (через ЗАПЯТУЮ),                        */
/* имеющих указанное значение                                                 */
/*----------------------------------------------------------------------------*/
FUNCTION GetEqParamList     RETURNS CHAR
   (iVal     AS CHAR, /* Значение параметра */
    iCodLst  AS CHAR, /* Список кодов       */
    iValLst  AS CHAR, /* Список значений    */
    iDlm     AS CHAR  /* Разделитель        */
   ):

   /* Счётчик                  */
   DEF VAR vCnt           AS INT64   NO-UNDO.
   /* Код элемента списка      */
   DEF VAR vCod           AS CHAR  NO-UNDO.
   /* Значение элемента списка */
   DEF VAR vVal           AS CHAR  NO-UNDO.
   /* Возвращаемое значение    */
   DEF VAR vRetVal        AS CHAR  NO-UNDO.

   /* Обратный перебор здесь обязателен! */
   DO vCnt = NUM-ENTRIES(iCodLst, iDlm) TO 1 BY -1:

      ASSIGN
         vCod = ENTRY(vCnt, iCodLst, iDlm)
         vVal = ENTRY(vCnt, iValLst, iDlm).

      IF vVal = iVal THEN
      DO:
         {additem.i vRetVal vCod}
      END.
   END.

   RETURN vRetVal.

END FUNCTION.

/* Заменяет вхождения спецсимволов в строку на их коды для корректного 
** работы prepare  выражения в динамических query. */
FUNCTION RepSpecSym RETURNS CHAR (
   INPUT iStr  AS CHAR, /* Аргумент для WHERE выражения. */
   INPUT iOper AS CHAR  /* Операция. */
): 
   DEF VARI vSymList AS CHAR   NO-UNDO.
   DEF VARI vNum     AS INT64    NO-UNDO.
   DEF VARI vInd     AS INT64    NO-UNDO.
   DEF VAR  vOperLst AS CHAR   NO-UNDO. /* Список операций. */

   ASSIGN
      /* " - 34, ' - 39, { - 123 , } - 125 , \ - 92 */
      vSymList = "126,34,39,123,125,92"
      vOperLst = "EQ,GE,GT,LE,LT,BEGINS,=,>=,>,<=,<"
   .
   DO vNum = 1 TO NUM-ENTRIES (vSymList):
      vInd = INDEX (iStr, CHR (INT64 (ENTRY (vNum, vSymList)))).

      IF vInd NE 0 THEN   /* если спецсимвол присутствует заменяем на ~спецсимвол */
         iStr = REPLACE(iStr,
                        CHR(INT64(ENTRY(vNum,vSymList))),
                        /* Дубляж необходим только для операций
                        ** НЕ входящих в список vOperLst. */
                        IF       ENTRY  (vNum,vSymList)     EQ "92"
                           AND   LOOKUP (iOper, vOperLst)   EQ 0
                           THEN CHR(126) + CHR(INT64(ENTRY(vNum,vSymList))) + 
                                CHR(126) + CHR(INT64(ENTRY(vNum,vSymList)))
                           ELSE CHR(126) + CHR(INT64(ENTRY(vNum,vSymList)))
                        ).
   END.
   RETURN iStr.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Проверяет корректность формата для указанного типа данных                  */
/*----------------------------------------------------------------------------*/
FUNCTION IsValidFormat RETURNS LOGICAL 
   (INPUT iDataType AS CHAR,
    INPUT iFormat   AS CHAR):
   
   DEFINE VARIABLE vRetValue AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vTestChar AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTestDec  AS DECIMAL    NO-UNDO.
   
   /* Для анализа формата достаточно 3 буквы */
   CASE SUBSTR (iDataType,1,3):
      /* Логические */
      WHEN "LOG" THEN vRetValue = (NUM-ENTRIES (iFormat,"/") NE 2).
      /* Остальные ВИДИМЫЕ */
      WHEN "DEC" OR
      WHEN "INT" THEN
      DO:
         ASSIGN vTestChar = STRING (0,iFormat) NO-ERROR.
         vRetValue = ERROR-STAT:ERROR. 
      END.
      WHEN "CHA" THEN
      DO:
         ASSIGN vTestChar = STRING (FILL ("9",500),iFormat) NO-ERROR.
         vRetValue = ERROR-STAT:ERROR.
      END.
      WHEN "DAT" THEN
      DO:
         ASSIGN vTestChar = STRING (TODAY, iFormat) NO-ERROR.
         vRetValue = ERROR-STAT:ERROR.
      END.
      /* остальные НЕВИДИМЫЕ */
      OTHERWISE vRetValue = NO.
   END CASE.
   /* Результат работы */
   RETURN NOT (vRetValue).
END FUNCTION.



/*----------------------------------------------------------------------------*/
/* Разбивка длинной строки на подстроки                                       */
/*----------------------------------------------------------------------------*/
FUNCTION SplitStr RETURNS CHAR
   (iVal   AS CHAR, /**/
    iLen   AS INT64,  /*длина строки*/
    iSep   AS CHAR  /*символ-разделитель*/
   ):

   {wordwrap.def}

   /* Счётчик */
   DEF VAR vCnt  AS INT64   NO-UNDO.
   /* Наименование поставщика */
   DEF VAR vStr  AS CHAR  NO-UNDO EXTENT 20.

   ASSIGN
      vStr[1] = iVal
      iVal = "".

   {wordwrap.i
      &s = vStr
      &n = EXTENT(vStr)
      &l = iLen
   }

   DO vCnt = 1 TO EXTENT(vStr) WHILE vStr[vCnt] <> "":
      iVal = iVal
           + ( IF iVal = "" THEN "" ELSE iSep)
           + STRING(vStr[vCnt], "x(" + STRING(iLen) + ")").
   END.
  
   RETURN iVal.
  
END FUNCTION.

DEFINE TEMP-TABLE tSort NO-UNDO
   FIELD fItem AS CHARACTER
   INDEX iItem fItem
   .
/*------------------------------------------------------------------------------
  Purpose:     Возвращает отсортированный список
  Parameters:  iList - список для сортировки
               iDlm  - разделитель списка
               iDsc  - YES - сортировка по возрастанию
  Notes:       
------------------------------------------------------------------------------*/
FUNCTION SortDelimList RETURNS CHARACTER (iList   AS CHARACTER,
                                          iDlm    AS CHARACTER,
                                          iDsc    AS LOGICAL):
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vStr AS CHARACTER  NO-UNDO.
   
   IF NOT {assigned iDlm} THEN iDlm = ",".

   EMPTY TEMP-TABLE tSort.

   vNum = NUM-ENTRIES(iList,iDlm).
   DO vCnt = 1 TO vNum:
      CREATE tSort.
      tSort.fItem = ENTRY(vCnt,iList,iDlm).
   END.
   
   IF iDsc THEN
      FOR EACH tSort BY tSort.fItem DESC:
         vStr = vStr + ( IF vStr = "" THEN "" ELSE iDlm) + tSort.fItem.
      END. /* FOR EACH */
   ELSE
      FOR EACH tSort BY tSort.fItem:
         vStr = vStr + ( IF vStr = "" THEN "" ELSE iDlm) + tSort.fItem.
      END. /* FOR EACH */

   RETURN vStr.

END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Кодирует строку
  Parameters:  iString - строка
  Notes:       Функция должна использоваться для получения хэш-строки, 
               которая не содержит никаких символов, кроме печатных, что 
               позволяет  ее использовать в тех местах, где недопустимы любые
               непечатные символы, например XML
------------------------------------------------------------------------------*/
FUNCTION BQEncode RETURNS CHARACTER (iString AS CHARACTER):
   DEFINE VARIABLE vRaw AS RAW      NO-UNDO.
   PUT-STRING(vRaw,1,LENGTH(iString)) = iString.
   RETURN STRING(vRaw).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает уникальное имя файла, которое можно использовать для
               временных файлов, имена которых должны быть уникальными.
               Реализация подсмотрена у PSC файл adecomm/_tmpfile.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FUNCTION GetUniqueFileName RETURNS CHARACTER (iPrefix    AS CHARACTER,
                                              iExtension AS CHARACTER):
   DEFINE VARIABLE vFName AS CHARACTER  NO-UNDO.
   DO WHILE YES:
      vFname = iPrefix + STRING(( TIME * 1000 + ETIME ) MODULO 100000,"99999") +
               "." + iExtension.
      IF SEARCH(vFName) = ? THEN RETURN vFName.
   END.
   RETURN vFName.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Декодирует строку, закодированную через BQEncode
  Parameters:  iString - закодированная строка
               oString - раскодированная строка
  Notes:       Декодирование выполнено с использование INPUT/OUTPUT поэтому
               оформлено в виде процедуры. Если есть другой способ реализации
               просьба об этом сообщить.
------------------------------------------------------------------------------*/
PROCEDURE BQDecode:
   DEFINE INPUT  PARAMETER iString AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oString AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vRaw    AS RAW         NO-UNDO.
   DEFINE VARIABLE vFName  AS CHARACTER   NO-UNDO.
   
   /* Commented by KSV: Вычисляем уникальное имя файла */
   vFName = GetUniqueFileName("bq","tmp").
   /* Commented by KSV: Пишем в файл закодированную строку */
   OUTPUT TO VALUE(vFName).
   PUT UNFORMATTED iString SKIP.
   OUTPUT CLOSE.
   /* Commented by KSV: Читаем строку из файла в raw */
   INPUT FROM VALUE(vFName).
   IMPORT vRaw.
   INPUT CLOSE.
   /* Commented by KSV: Декодируем строку */
   oString = GET-STRING(vRaw,1).
   /* Commented by KSV: Удаляем файл */
   OS-DELETE VALUE ( {&RELATIVE_2_ABSOLUTE}( vFName ) ).
END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose:     удаляет из строки повторы заданного символа
  Parameters:  iString - строка,
               iChar   - удаляемый символ
  Notes:       Функция может использоваться для форматирования адресов
------------------------------------------------------------------------------*/
FUNCTION DelDoubleChars RETURNS CHARACTER (iString AS CHARACTER, iChar AS CHARACTER):
   DEFINE VARIABLE vStr AS CHARACTER NO-UNDO.
   vStr = iString.
   IF iChar = "," THEN
      DO WHILE INDEX (vStr, ", ") > 0 :
         vStr = REPLACE (vStr, ", ", ",").
      END.
   DO WHILE INDEX (vStr, iChar + iChar) > 0 :
      vStr = REPLACE (vStr, iChar + iChar, iChar).
   END.
   vStr = TRIM (vStr, iChar).
   vStr = TRIM (vStr).
   RETURN vStr.
END FUNCTION.

/*----------------------------------------------------------------------------
   Проверяет строку на наличие недопустимых символов. 

   iStr           проверяемая строка
   iParam         критерии проверки:
                  1 - кирилица
                  2 - латиница
                  3 - цыфры
                  4 - латиница верхний регистр
                  есть возможность указывать список: 1,2 1,2,3 
   iOthersSymbol  перечень прочих допустимых символов
   oResult        результат проверки
                  да если в строке только допустимые символы
----------------------------------------------------------------------------*/
PROCEDURE Check-Ascii-Set.

   DEFINE INPUT  PARAMETER iStr          AS CHARACTER   NO-UNDO. /* проверяемая строка */
   DEFINE INPUT  PARAMETER iParam        AS CHARACTER   NO-UNDO. /* критерии проверки 1,2,3 */
   DEFINE INPUT  PARAMETER iOthersSymbol AS CHARACTER   NO-UNDO. /* перечень прочих допустимых символов */
   DEFINE OUTPUT PARAMETER oResult       AS LOGICAL     NO-UNDO INIT NO.

   DEFINE VARIABLE vCorrectlyStr AS CHARACTER   NO-UNDO. /* Список кодов допустимых значений */
   DEFINE VARIABLE vTmpChar      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE i             AS INT64     NO-UNDO.
   DEFINE VARIABLE j             AS INT64     NO-UNDO.

   iOthersSymbol = iOthersSymbol + " ".

   /* ibm866 */
   DO j = 1 TO NUM-ENTRIES(iParam):
      CASE ENTRY(j,iParam):
         WHEN "1" THEN /* 1. Проверка строки на кириллицу. */
         DO: 
            DO i = 128 TO 175:
               {additem.i vCorrectlyStr STRING(i)}
            END.
            DO i = 224 TO 241:
               {additem.i vCorrectlyStr STRING(i)}
            END.
         END.
         WHEN "2" THEN /* 2. Проверка строки на латиницу. */
         DO:
            DO i = 65 TO 90:
               {additem.i vCorrectlyStr STRING(i)}
            END.
            DO i = 97 TO 122:
               {additem.i vCorrectlyStr STRING(i)}
            END.
         END.
         WHEN "3" THEN /* 3. Проверка на цифры. */
         DO i = 48 TO 57:
            {additem.i vCorrectlyStr STRING(i)}
         END.
         WHEN "4" THEN /* 4. Латиница верхний регистр */
         DO i = 65 TO 90:
            {additem.i vCorrectlyStr STRING(i)}
         END.
      END CASE.
   END.
   /* + Прочие допустимые символы */
   DO i = 1 TO LENGTH(iOthersSymbol):
      vTmpChar = STRING(ASC(SUBSTRING(iOthersSymbol,i,1))).
      {additem.i vCorrectlyStr vTmpChar}
   END.
   /* Сама проверка */
   DO i = 1 TO LENGTH(iStr):
      IF CAN-DO(vCorrectlyStr,STRING(ASC(SUBSTRING(iStr,i,1)))) THEN
         NEXT.
      ELSE DO:                   /* Недопустимые символы  */
         oResult = YES.
         LEAVE.
      END.
   END.
END PROCEDURE.

PROCEDURE Check-Cyrillic-Roman.

   DEFINE INPUT  PARAMETER iStr          AS CHARACTER   NO-UNDO. /* проверяемая строка */
   DEFINE OUTPUT PARAMETER oCyrillic     AS LOGICAL     NO-UNDO INIT NO.
   DEFINE OUTPUT PARAMETER oRoman        AS LOGICAL     NO-UNDO INIT NO.

   DEFINE VARIABLE vCurAscii AS INT64     NO-UNDO.

   DEFINE VARIABLE i             AS INT64     NO-UNDO.

   /* Сама проверка */
   DO i = 1 TO LENGTH(iStr):
      vCurAscii = ASC(SUBSTRING(iStr,i,1)).
      IF    (    vCurAscii >= 128 
             AND vCurAscii <= 175)
         OR (    vCurAscii >= 224 
             AND vCurAscii <= 241) THEN
         oCyrillic = YES.
      IF    (    vCurAscii >= 65 
             AND vCurAscii <= 90) 
         OR (    vCurAscii >= 97 
             AND vCurAscii <= 122) THEN
         oRoman = YES.
      IF oCyrillic AND oRoman THEN
         LEAVE.
   END.
END PROCEDURE.

/*----------------------------------------------------------------------------
   Читаем в строку определенное количество байт из файла, удаляя недопустимые 
   символы и повторы 

   iFile          Имя файла
   iSize          Количество символов
   oStr           Результирующая строка
----------------------------------------------------------------------------*/


PROCEDURE Read-Str-FromFile.
   DEFINE INPUT  PARAMETER iFile   AS CHARACTER   NO-UNDO. 
   DEFINE INPUT  PARAMETER iSize   AS INT64       NO-UNDO. 
   DEFINE OUTPUT PARAMETER oStr    AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vLen  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE vI    AS INT64     NO-UNDO.
   DEFINE VARIABLE vJ    AS INT64     NO-UNDO.
   DEFINE VARIABLE vStr  AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE vWord AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE vBrk  AS LONGCHAR NO-UNDO.

   vBrk = "√ | │ ├ ┼ ┤ ┐ ┌ └ ┴ ┘ ─ ┬ ╔ ╦ ╗ ╚  ═ ╣ ╝ ║ ╟ ╢ ╪ ╤ ╧ ~n ; : = ~*~  + % @ _ . , - / ~\ ! ? ( ) [ ] < > ~"~ ~'~ 0 1 2 3 4 5 6 7 8 9".
   FILE-INFORMATION:FILE-NAME = iFile.
   vLen = FILE-INFORMATION:FILE-SIZE.

   DO ON ERROR UNDO, LEAVE:
      COPY-LOB FILE iFile FOR vLen TO vStr NO-CONVERT.
   END.
   DO vI = 1 TO NUM-ENTRIES(vStr," "):
      IF ENTRY(vI,vStr," ") NE "" AND ENTRY(vI,vStr," ") NE "~n" THEN DO:
         vWord = TRIM(ENTRY(vI,vStr," "),STRING(vBrk)).
         oStr = REPLACE(oStr," " + vWord + " ", " ") + " " + vWord.      
      END.
   END.

   IF iSize < LENGTH(oStr) THEN DO:
      DO vJ = iSize TO LENGTH(oStr):
         IF INDEX(" ",SUBSTRING(oStr,vJ,1)) > 0 THEN LEAVE.
      END.
      oStr = TRIM(SUBSTRING(oStr,1,vJ)).
   END.
   ELSE
      oStr = TRIM(SUBSTRING(oStr,1,iSize)).
END PROCEDURE.
{pfuncdef
   &DEFPROC     = "CompareAsInt"
   &DESCRIPTION = "Сравнение двух строк как целых чисел"
   &PARAMETERS  = "iStr1, iStr2, iOp, OUTPUT oResult"
   &RESULT      = "имя параметра, содержащего ошибочные данные, или пусто"
   &SAMPLE      = "CompareAsInt IN h_strng ("-11", "22", "GE", OUTPUT vResult)"}
/*------------------------------------------------------------------------------
  Purpose:     Сравнение двух строк как целых чисел.
  Parameters:  iStr1   - первая строка.
               iStr2   - вторая строка.
               iOp     - операция сравения в рамках синтаксиса Progress ABL.
               oResult - выходной параметр, истинность выражения
                         (INT64(iStr1) iOp INT64(iStr2)).
                         При любых проблемах приведения входных строк
                         к типу INT64 или указании недопустимой операции
                         будет содержать неопределённое значение, также
                         в этом случае после вызова процедуры будет
                         заполнено RETURN-VALUE.
  Notes:       RETURN-VALUE = "iStr1" - первая строка не является целым числом.
               RETURN-VALUE = "iStr2" - вторая строка не является целым числом.
               RETURN-VALUE = "iOp"   - указана недопустимая операция сравнения.
               RETURN-VALUE = ""      - в остальных случаях.
------------------------------------------------------------------------------*/
PROCEDURE CompareAsInt:
   DEFINE INPUT  PARAMETER iStr1   AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iStr2   AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iOp     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INITIAL ?.

   DEFINE VARIABLE vRetVal AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vInt1   AS INT64     NO-UNDO.
   DEFINE VARIABLE vInt2   AS INT64     NO-UNDO.

   vInt1 = INT64(iStr1) NO-ERROR.
   IF NOT {assigned iStr1} OR ERROR-STATUS:ERROR OR vInt1 = ? THEN
      vRetVal = "iStr1".
   ELSE DO:
      vInt2 = INT64(iStr2) NO-ERROR.
      IF NOT {assigned iStr2} OR ERROR-STATUS:ERROR OR vInt2 = ? THEN
         vRetVal = "iStr2".
      ELSE
         CASE iOp:
            WHEN "="  OR WHEN "EQ" THEN oResult = (vInt1 =  vInt2).
            WHEN "<>" OR WHEN "NE" THEN oResult = (vInt1 <> vInt2).
            WHEN "<"  OR WHEN "LT" THEN oResult = (vInt1 <  vInt2).
            WHEN ">"  OR WHEN "GT" THEN oResult = (vInt1 >  vInt2).
            WHEN "<=" OR WHEN "LE" THEN oResult = (vInt1 <= vInt2).
            WHEN ">=" OR WHEN "GE" THEN oResult = (vInt1 >= vInt2).
            OTHERWISE                   vRetVal = "iOp".
         END CASE.
   END.
   IF vRetVal <> "" THEN
      RETURN vRetVal.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='$/ws1-dvp/bq/' */
/* $LINTDATE='12/11/2014 10:21:40.871+04:00' */
/* $LINTFILE='pp-strng.p' */
/*prosignTTUFhbkf0tDGNumP+051Fg*/
/* --- pp-strng.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am --- */
