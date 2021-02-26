/*-----------------------------------------------------------------------------
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: STRINGS.FUN
      Comment: Функции работы со строками
         Uses:
      Used BY:
      Created: 11/05/2001 yakv
     Modified: 16/05/2001 yakv - Добавлены FStrPad и FStrCenter
     Modified: 30/05/2001 yakv - Добавлена FStrPadC
     Modified: 30/05/2002 NIK  - Добавлена GetEntries
     Modified: 05.09.2003 15:10 KSV      (0019702) Добавлены функции
                                         GetMangledName и GetOriginalName,
                                         осущетсвляющие взаимные преобразования
                                         строк на русском языке. Добавлена
                                         функция RemoveDoubleChars, удаляющая
                                         из строки поторяющиеся символы.
     Modified: 30.09.2003 19:07 KSV      (0019702) Добавлены функции
                                         определяющих является ли символ:
                                         IsAlpha - буквой, IsDigit - цифрой и
                                         IsAlphaNum - буквой или цифрой.
     Modified: 01.11.2003 18:13 KSV      (0020852) Изменена работа функций
                                         GetMangledName и GetOriginalName с
                                         учетом того, что в в реквизитах могут
                                         встречаться разделители '_' и '-'.
     Modified: 01.12.2003 19:04 KSV      (0020852) Исправлена компиляция
                                         CheckIDName.
     Modified: 10.12.2003 14:36 KSV      (0020852) Исправлена функция
                                         GetOriginalName, чтобы она точно была
                                         обратной по отношению GetMangledName.
     Modified: 19.01.2004 20:47 KSV      (0019947) В функции GetMangledName и
                                         GetOriginalName добавлена обработка
                                         ключевых слов 4GL.
     Modified: 30.01.2004 18:37 KSV      (0019947) Добавлена функция PairTrim,
                                         которая занимается попарным отсечением
                                         хвостовых и лидирующих символов.
     Modified: 31.03.2004 18:37 NIK      Добавлена функции GetNullStr
                                                           GetNullNum
                                                           GetNullDat
     Modified: 08.05.2004 19:00 KSV      (0019947) Исправлена функция
                                         GetMangledName, предотвращающая
                                         манглирование смешанных
                                         идентификаторов.
     Modified: 11.05.2004 09:30 KSV      (0019947) Исправлена GetMangledName.
     Modified: 12.11.2004 13:09 Om       
     Modified: 19.01.2005 09:30 NIK      Ускорена GetMangledName.
     Modified: 11.05.2005 15:33 REVV     Добавлена функция SetEntries
     Modified: 14.05.2005 18:00 ХАRO     (0034340) Изменена работа функций
                                         GetMangledName и GetOriginalName с
                                         учетом того, что в реквизитах могут
                                         встречаться кирилица и латиница вперемежку.
                                         Предыдущие функции GetMangledName и GetOriginalName
                                         переименованы в GetMangledNameS и GetOriginalNameS
     Modified: 25.11.2005 18:37 NIK      Добавлена функция GetNullInt
     Modified: 30.03.2010 16:39 ariz     Добавлена функция ListsCrossing - находит
                                         пересечение элементов двух списков
-------------------------------------------------------------------------------
Перечень функций/процедур:
    FStrEmpty
    FStrNVL
    FStrCat
    FStrPad
    FStrPadC
    FStrCenter
    GetEntries
    SetEntries
-----------------------------------------------------------------------------*/
&IF DEFINED( FILE_strings_fun ) = 0 &THEN &GLOBAL-DEFINE FILE_strings_fun

&IF DEFINED(pp-str) = 0 &THEN
   &MESSAGE ** ЗАМЕЧАНИЕ - вызван STRINGS.FUN, используйте intrface.get strng
&ENDIF
/* Признак что манглирование полей Instance в pp-data.p производится с учетом возможности передачи через XML */
DEF NEW GLOBAL SHARED VAR gInstanceXml AS LOGICAL NO-UNDO. 

DEFINE VAR mMangled AS CHAR EXTENT 255 NO-UNDO.
DEFINE TEMP-TABLE ttMangled NO-UNDO
   FIELD i_name AS CHAR
   FIELD o_name AS CHAR
INDEX i_name i_name
.

RUN PrepareMangledName.

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetPartStr  CHAR (INPUT iText AS CHAR,
                           INPUT iLen  AS INT64,
                           INPUT iNum  AS INT64):

   {wordwrap.def}
   DEFINE VAR vText AS CHAR      EXTENT 2 NO-UNDO.
   DEFINE VAR vCnt  AS INT64   INIT   0 NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetPartStr","iText:" + iText +
                                " iNum:" + string(iNum)).
   &ENDIF

   vText[1] = iText.
SPLIT:
   DO WHILE TRUE:
      vCnt = vCnt + 1.
      {wordwrap.i &s = vText &l = iLen &n = 2}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("GetPartStr","vCnt:" + string(vCnt) +
                              " vText[1]:" + vText[1]     +
                              " vText[2]:" + vText[2]).
      &ENDIF

      IF vCnt EQ iNum THEN LEAVE SPLIT.
      vText[1] = vText[2].
   END.

   RETURN vText[1].
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION FStrEmpty returns LOGICAL (INPUT asValue AS CHAR).
    RETURN (asValue = "" OR asValue = ?).
END.

/*---------------------------------------------------------------------------*/
FUNCTION FStrNVL RETURN CHAR (
    INPUT  asValTrue    AS CHAR,
    INPUT  asValFalse   AS CHAR
).
/*---------------------------------------------------------------------------*/
    RETURN IF( FStrEmpty( asValTrue ) ) THEN asValFalse ELSE asValTrue.
END.

/*----------------------------------------------------------------------------*/
/*  Конкатенация строк "asSrc" || "asCat" с разделителем "asDelim"            */
/*----------------------------------------------------------------------------*/
FUNCTION FStrCat returns CHAR (
    INPUT-OUTPUT asSrc    AS CHAR,
    INPUT        asCat    AS CHAR,
    INPUT        asDelim  AS CHAR
).
    IF( asSrc <> "" ) THEN ASSIGN asSrc = asSrc + asDelim.
    ASSIGN asSrc = asSrc + asCat.
    RETURN asSrc.
END.

/*----------------------------------------------------------------------------*/
/*  Дополнение строки "asStr" пробелами до длины "anLen" справа или слева     */
/*----------------------------------------------------------------------------*/
FUNCTION FStrPad returns CHAR (
    INPUT  asStr    AS CHAR,
    INPUT  anLen    AS INT64,
    INPUT  abRight  AS LOGICAL
).
    DEF VAR sSpaces AS CHAR INIT "" NO-UNDO.
    ASSIGN sSpaces = IF( abRight ) THEN "" ELSE FILL( " ", anLen - length( asStr ) ).
    RETURN string( sSpaces + asStr, "x(" + string( anLen ) + ")" ).
END.

/*----------------------------------------------------------------------------*/
/* Дополнение строки  символами до длины "anLen" справа или слева             */
/*----------------------------------------------------------------------------*/
FUNCTION FStrPadC returns CHAR (
    INPUT  asStr    AS CHAR,
    INPUT  anLen    AS INT64,
    INPUT  abRight  AS LOGICAL,
    INPUT  asFill   AS CHAR
).
    DEF VAR sFills AS CHAR INIT "" NO-UNDO.
    ASSIGN sFills = FILL( asFill, anLen - length( asStr ) ).
    RETURN string( IF( abRight ) THEN (asStr + sFills) ELSE (sFills + asStr), "x(" + string( anLen ) + ")" ).
END.

/*----------------------------------------------------------------------------*/
/*      Центрирование строки "asStr" по ширине "anLen" символов               */
/*----------------------------------------------------------------------------*/
FUNCTION FStrCenter returns CHAR (INPUT asStr AS CHAR, INPUT anLen AS INT64).
    DEF VAR nSpaces AS INT64  NO-UNDO.
    ASSIGN nSpaces = ( anLen - length( asStr ) ) / 2.
    RETURN string( FILL( " ", nSpaces ) + asStr, "x(" + string( anLen ) + ")" ).
END.

/*----------------------------------------------------------------------------*/
/*          Надежное получение элемента списка                                */
/*----------------------------------------------------------------------------*/
FUNCTION GetEntries RETURN CHAR (INPUT ipItem      AS INT64,
                                 INPUT ipText      AS CHAR,
                                 INPUT ipSplit     AS CHAR,
                                 INPUT ipDefault   AS CHAR):
   IF NUM-ENTRIES(ipText,ipSplit) GE ipItem
      THEN RETURN ENTRY(ipItem,ipText,ipSplit).
      ELSE RETURN ipDefault.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*          Надежное присвоение значения для элемента списка                  */
/*----------------------------------------------------------------------------*/
FUNCTION SetEntries RETURN CHAR (INPUT ipItem      AS INT64, /* позиция     */
                                 INPUT ipText      AS CHAR,    /* список      */
                                 INPUT ipSplit     AS CHAR,    /* разделитель */
                                 INPUT ipValue     AS CHAR):   /* значение    */
   DEFINE VAR vI AS INT64 NO-UNDO.

   DO vI = NUM-ENTRIES(ipText,ipSplit) TO ipItem - 1:
      ipText =  ipText + ipSplit.
   END.
   ENTRY(ipItem,ipText,ipSplit) = ipValue.

   RETURN ipText.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION PADR RETURN CHAR (INPUT ipText AS CHAR,
                           INPUT ipLen  AS INT64):
   RETURN TRIM(SUBSTRING(ipText,1,ipLen)) +
          FILL(" ",max(ipLen - length(ipText),0)).
END FUNCTION.

FUNCTION PADL RETURN CHAR (INPUT ipText AS CHAR,
                           INPUT ipLen  AS INT64):
   RETURN FILL(" ",max(ipLen - length(ipText),0)) +
          TRIM(SUBSTRING(ipText,1,ipLen)).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                Центрирует строку                                           */
/*----------------------------------------------------------------------------*/
FUNCTION PADC RETURN CHAR (INPUT ipText AS CHAR,
                           INPUT ipLen  AS INT64):
   DEFINE VAR vLen  AS INT64 NO-UNDO.
   DEFINE VAR vLeft AS INT64 NO-UNDO.
   vLen  = length(TRIM(ipText)).
   vLeft = max(0,truncate((ipLen - vLen) / 2,0)).
   RETURN FILL(" ",vLeft)                                +
          TRIM(SUBSTRING(ipText,1,ipLen))                +
          FILL(" ",max(ipLen - length(ipText) - vLeft,0)).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Возвращает строку с учетом пустого значения                                */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullStr CHAR (INPUT iVal AS CHAR):
   RETURN IF iVal EQ ? THEN "?" ELSE iVal.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Возвращает число (как строку) с учетом пустого значения                    */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullNum CHAR (INPUT iVal AS DECIMAL):
   RETURN IF iVal EQ ? THEN "?" ELSE string(iVal).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Возвращает число (как строку) с учетом пустого значения                    */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullInt CHAR (INPUT iVal AS INT64):
   RETURN IF iVal EQ ? THEN "?" ELSE string(iVal).
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Возвращает дату (как строку) с учетом пустого значения                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullDat CHAR (INPUT iVal AS DATE):
   RETURN IF iVal EQ ? THEN "?" ELSE string(iVal).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                Вывод информации на экран                                   */
/*----------------------------------------------------------------------------*/
PROCEDURE PutScreen:
   DEFINE INPUT PARAMETER ipCol  AS INT64  NO-UNDO.
   DEFINE INPUT PARAMETER ipText AS CHAR     NO-UNDO.

   PUT screen ROW screen-lines + 1 COL ipCol ipText.

END PROCEDURE.

/*******************************************************************************
 * 4.1D SECTION                                                              * *
 ******************************************************************************/
/*------------------------------------------------------------------------------
  Purpose:     Заменяет в строке задвоенные символы на одиночные.
  Parameters:  iStr  - строка
               iChr  - задвоенный символ
  Notes:       Полезно, например, для удаления пустых элементов списка после
               выполнения оператора ENTRY(<list>) = "", например,
               a,,,b, -> a,b
------------------------------------------------------------------------------*/
FUNCTION RemoveDoubleChars RETURNS CHARACTER (iStr AS CHARACTER,
                                              iChr AS CHARACTER):
   DEFINE VARIABLE vStr AS CHARACTER  NO-UNDO.
   vStr = REPLACE(iStr,iChr + iChr,iChr).
   DO WHILE iStr <> vStr:
      iStr = vStr.
      vStr = REPLACE(iStr,iChr + iChr,iChr).
   END.
   IF iStr = iChr THEN iStr = "".
   RETURN TRIM(iStr,iChr).
END FUNCTION.


/*------------------------------------------------------------------------------
  Purpose:     Опредляет является ли символ буквой
  Parameters:  iChr  - символ
  Notes:
------------------------------------------------------------------------------*/
FUNCTION IsAlpha RETURNS LOGICAL (iChr AS CHARACTER):
   RETURN (ASC(iChr) >= ASC("A") AND ASC(iChr) <= ASC("Z")) OR
          (ASC(iChr) >= ASC("a") AND ASC(iChr) <= ASC("z")) OR
          (ASC(iChr) >= ASC("А") AND ASC(iChr) <= ASC("Я")) OR
          (ASC(iChr) >= ASC("а") AND ASC(iChr) <= ASC("п")) OR
          (ASC(iChr) >= ASC("р") AND ASC(iChr) <= ASC("я")) OR
           ASC(iChr)  = ASC("ё") OR  ASC(iChr)  = ASC("Ё").

END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Опредляет является ли символ цифрой
  Parameters:  iChr  - символ
  Notes:
------------------------------------------------------------------------------*/
FUNCTION IsDigit RETURNS LOGICAL (iChr AS CHARACTER):
   RETURN ASC(iChr) >= ASC("0") AND ASC(iChr) <= ASC("9").
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Опредляет является ли символ буквой или цифрой
  Parameters:  iChr  - символ
  Notes:
------------------------------------------------------------------------------*/
FUNCTION IsAlphaNum RETURNS LOGICAL (iChr AS CHARACTER):
   RETURN IsAlpha(iChr) OR IsDigit(iChr).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Проверяет правильность имени объекта
  Parameters:  pName - имя объекта (Class,Xattr,Setting,Op-Kind)
  Notes:
------------------------------------------------------------------------------*/
FUNCTION CheckIdName RETURN LOGICAL (pName AS CHAR):
   DEFINE VARIABLE vRes AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErrMess AS CHARACTER NO-UNDO.
   DEFINE VARIABLE IsOnlyLat AS LOG NO-UNDO.
   DEFINE VARIABLE IsOnlyRus AS LOG NO-UNDO.

   IsOnlyLat = DYNAMIC-FUNCTION("ereg":U,"^[0-9_~\-A-Za-z]+$",
                    pName,
                    OUTPUT vRes,
                    INPUT-OUTPUT vErrMess) EQ YES.
   IsOnlyRus = DYNAMIC-FUNCTION("ereg":U,"^[0-9_~\-А-пр-я]+$",
                    pName,
                    OUTPUT vRes,
                    INPUT-OUTPUT vErrMess) EQ YES.
   IF pName EQ "" OR pName EQ "?" THEN
      RETURN NO.
   IF NOT IsAlpha(SUBSTR(pName,1,1)) OR
      (NOT IsOnlyLat AND NOT IsOnlyRus) THEN
      RETURN NO.
   RETURN YES.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Удаляет попарно лидирующие и хвостовые символы из строки.
  Parameters:  iStr   - строка
               iChrs  - символы для удаления.
  Notes:       Например: PairTrim("'xyz''") = "xyz'". Основное использование
               в dopars.p.
------------------------------------------------------------------------------*/
FUNCTION PairTrim RETURNS CHARACTER (iStr    AS CHARACTER,
                                     iChrs   AS CHARACTER):
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen AS INT64    NO-UNDO.
   DEFINE VARIABLE vChr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmp AS CHARACTER  NO-UNDO.

   vLen = LENGTH(iChrs).

   DO WHILE YES:
      vTmp = iStr.
      DO vCnt = 1 TO vLen:
         vChr = SUBSTR(iChrs,vCnt,1).
         /* Commented BY KSV: Если количество символов в строке больше 2-х и
         ** первый символ равен последнему, то усекаем строку */
         IF LENGTH(iStr) >= 2  AND
            iStr BEGINS  vChr AND
            iStr MATCHES "*" + vChr THEN iStr = SUBSTR(iStr,2,LENGTH(iStr) - 2).
      END.
      /* Commented BY KSV: Делаем пока строка усекается */
      IF vTmp = iStr THEN RETURN iStr.
   END.
END FUNCTION.



&GLOBAL-DEFINE MANGLED_CHR "$"

PROCEDURE PrepareMangledName:

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PrepareMangledName","START").
   &ENDIF

   ASSIGN
      mMangled[asc("а")] = "a"
      mMangled[asc("А")] = "a"
      mMangled[asc("б")] = "b"
      mMangled[asc("Б")] = "b"
      mMangled[asc("в")] = "v"
      mMangled[asc("В")] = "v"
      mMangled[asc("г")] = "g"
      mMangled[asc("Г")] = "g"
      mMangled[asc("д")] = "d"
      mMangled[asc("Д")] = "d"
      mMangled[asc("е")] = "e"
      mMangled[asc("Е")] = "e"
      mMangled[asc("ё")] = "wo"
      mMangled[asc("Ё")] = "wo"
      mMangled[asc("ж")] = "wz"
      mMangled[asc("Ж")] = "wz"
      mMangled[asc("з")] = "z"
      mMangled[asc("З")] = "z"
      mMangled[asc("и")] = "i"
      mMangled[asc("И")] = "i"
      mMangled[asc("й")] = "wi"
      mMangled[asc("Й")] = "wi"
      mMangled[asc("к")] = "k"
      mMangled[asc("К")] = "k"
      mMangled[asc("л")] = "l"
      mMangled[asc("Л")] = "l"
      mMangled[asc("м")] = "m"
      mMangled[asc("М")] = "m"
      mMangled[asc("н")] = "n"
      mMangled[asc("Н")] = "n"
      mMangled[asc("о")] = "o"
      mMangled[asc("О")] = "o"
      mMangled[asc("п")] = "p"
      mMangled[asc("П")] = "p"
      mMangled[asc("р")] = "r"
      mMangled[asc("Р")] = "r"
      mMangled[asc("с")] = "s"
      mMangled[asc("С")] = "s"
      mMangled[asc("т")] = "t"
      mMangled[asc("Т")] = "t"
      mMangled[asc("у")] = "u"
      mMangled[asc("У")] = "u"
      mMangled[asc("ф")] = "f"
      mMangled[asc("Ф")] = "f"
      mMangled[asc("х")] = "h"
      mMangled[asc("Х")] = "h"
      mMangled[asc("ц")] = "c"
      mMangled[asc("Ц")] = "c"
      mMangled[asc("ч")] = "wc"
      mMangled[asc("Ч")] = "wc"
      mMangled[asc("ш")] = "ws"
      mMangled[asc("Ш")] = "ws"
      mMangled[asc("щ")] = "wt"
      mMangled[asc("Щ")] = "wt"
      mMangled[asc("ъ")] = "w%"
      mMangled[asc("Ъ")] = "w%"
      mMangled[asc("ь")] = "w#"
      mMangled[asc("Ь")] = "w#"
      mMangled[asc("ы")] = "y"
      mMangled[asc("Ы")] = "y"
      mMangled[asc("э")] = "we"
      mMangled[asc("Э")] = "we"
      mMangled[asc("ю")] = "wu"
      mMangled[asc("Ю")] = "wu"
      mMangled[asc("я")] = "wa"
      mMangled[asc("Я")] = "wa"
   .

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PrepareMangledName","FINIS").
   &ENDIF
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Функция преобразовывает строку,содержащую кирилицу в латиницу.
               В конец замененной строки  добавляется символ $.
               Например, АБВ -> ABV$. Также функция распознает ключевые слова
               и добавляет к строке два символа $.
  Parameters:  iStr - строка для преобразования
  Notes:       Например, поле временной таблицы не может содержать кириллицу
               в идентифкаторе.
------------------------------------------------------------------------------*/
FUNCTION GetMangledNameS RETURNS CHARACTER (iStr AS CHAR):
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen AS INT64    NO-UNDO.
   DEFINE VARIABLE vDst AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vChr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMng AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCn2 AS INT64    NO-UNDO.

   vLen = LENGTH(iStr).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","iStr:" + GetNullStr(iStr)).
   &ENDIF

   DO vCnt = 1 TO vLen:

      vChr = SUBSTR(iStr,vCnt,1).
      vMng = mMangled[asc(vChr)].

      IF NOT {assigned vMng} THEN DO:            /* Не русская буква          */
         vMng = vChr.

         /* Commented BY KSV: Если первая буква не русская, то не выполняем
         ** манглирования                                                 */
         IF vCnt = 1 THEN DO:
            /* Commented BY KSV: Если в качестве строки передано ключевое
            ** слово, манглируем его двойным символом манглирования */
            IF KEYWORD(iStr) = ? THEN DO:
               /* Проверка на русскую букву, начиная со второй  */
               IF vLen > 1 THEN
               DO vCn2 = 2 TO vLen:
                  vChr = SUBSTR(iStr,vCn2,1).
                  vMng = mMangled[asc(vChr)].
                  IF {assigned vMng} THEN
                     RETURN ?.
               END.
               RETURN iStr.
            END.
            ELSE
               RETURN iStr + {&MANGLED_CHR} + {&MANGLED_CHR}.
         END.
         ELSE DO:
            IF IsAlpha(vMng) THEN RETURN ?.
         END.
      END.

      vDst = vDst + vMng.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","vDst:" + GetNullStr(vDst)).
   &ENDIF

   RETURN vDst + {&MANGLED_CHR}.
END FUNCTION.


/*------------------------------------------------------------------------------
  Purpose:     Возвращает оригинальную строку из "мангловой" строки. Выполняет
               обратное преобразование значения функции GetMangledName.
  Parameters:  iMangledStr - "мангловая" строка.
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetOriginalNameS RETURNS CHARACTER (iMangledStr AS CHAR):
   DEFINE VARIABLE vLen  AS INT64    NO-UNDO.
   DEFINE VARIABLE vCnt  AS INT64    NO-UNDO.
   DEFINE VARIABLE vChr  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDst AS CHARACTER  NO-UNDO.

   IF NOT iMangledStr MATCHES "*" + {&MANGLED_CHR} THEN
      RETURN iMangledStr.

   /* Commented BY KSV: Если строка завершается двойным символом манглирования,
   ** то это ключевое слово, деманглируем его, откидывая двой символ
   ** манглирования */
   IF iMangledStr MATCHES "*" + {&MANGLED_CHR} + {&MANGLED_CHR} THEN
      RETURN TRIM(iMangledStr,{&MANGLED_CHR}).

   vLen = LENGTH(iMangledStr) - 1.
   DO vCnt = 1 TO vLen:
      vChr = SUBSTR(iMangledStr,vCnt,1).
      IF vChr = "w" THEN
      DO:
         vChr = vChr + SUBSTR(iMangledStr,vCnt + 1,1).
         vCnt = vCnt + 1.
      END.
      CASE vChr:
         WHEN "a"   THEN vChr = "а".
         WHEN "b"   THEN vChr = "б".
         WHEN "v"   THEN vChr = "в".
         WHEN "g"   THEN vChr = "г".
         WHEN "d"   THEN vChr = "д".
         WHEN "e"   THEN vChr = "е".
         WHEN "wo"  THEN vChr = "ё".
         WHEN "wz"  THEN vChr = "ж".
         WHEN "z"   THEN vChr = "з".
         WHEN "i"   THEN vChr = "и".
         WHEN "wi"  THEN vChr = "й".
         WHEN "k"   THEN vChr = "к".
         WHEN "l"   THEN vChr = "л".
         WHEN "m"   THEN vChr = "м".
         WHEN "n"   THEN vChr = "н".
         WHEN "o"   THEN vChr = "о".
         WHEN "p"   THEN vChr = "п".
         WHEN "r"   THEN vChr = "р".
         WHEN "s"   THEN vChr = "с".
         WHEN "t"   THEN vChr = "т".
         WHEN "u"   THEN vChr = "у".
         WHEN "f"   THEN vChr = "ф".
         WHEN "h"   THEN vChr = "х".
         WHEN "c"   THEN vChr = "ц".
         WHEN "wc"  THEN vChr = "ч".
         WHEN "ws"  THEN vChr = "ш".
         WHEN "wt"  THEN vChr = "щ".
         WHEN "w%"  THEN vChr = "ъ".
         WHEN "w#"  THEN vChr = "ь".
         WHEN "y"   THEN vChr = "ы".
         WHEN "we"  THEN vChr = "э".
         WHEN "wu"  THEN vChr = "ю".
         WHEN "wa"  THEN vChr = "я".
      END CASE.
      vDst = vDst + vChr.
   END.
   RETURN vDst.
END FUNCTION.

FUNCTION GetNameForXml RETURN CHARACTER
   (INPUT iFieldName AS CHAR):

   IF NOT gInstanceXml THEN RETURN iFieldName.

   IF SUBSTR(iFieldName,1,1) >= "0" AND
      SUBSTR(iFieldName,1,1) <= "9" THEN
      iFieldName = "_dig_" + iFieldName.
   /* [ - _quadr_ ] - _backquadr_ */
   iFieldName = REPLACE(REPLACE(iFieldName,"[","_quadr_"),"]","_backquadr_").
   /* #  - _reschetka_ */
   iFieldName = REPLACE(iFieldName,"#","_reschetka_").
   /* $  - _rusmangl_ */
   iFieldName = REPLACE(iFieldName,"$","_rusmangl_").
   /* %  - _perc_ */
   iFieldName = REPLACE(iFieldName,"%","_perc_").

   RETURN iFieldName.
END FUNCTION.

FUNCTION GetNameForProgress RETURN CHARACTER
   (INPUT iFieldName AS CHAR):

   IF NOT gInstanceXml THEN RETURN iFieldName.

   iFieldName = REPLACE(iFieldName,"_dig_","").
   /* [ - _quadr_ ] - _backquadr_ */
   iFieldName = REPLACE(REPLACE(iFieldName,"_quadr_","["),"_backquadr_","]").
   /* #  - _reschetka_ */
   iFieldName = REPLACE(iFieldName,"_reschetka_","#").
   /* $  - _rusmangl_ */
   iFieldName = REPLACE(iFieldName,"_rusmangl_","$").
   /* %  - _perc_ */
   iFieldName = REPLACE(iFieldName,"_perc_","%").

   RETURN iFieldName.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Функция преобразовывает строку,содержащую кирилицу в латиницу.
               В конец замененной строки  добавляется символ $.
               Например, АБВ -> ABV$. Также функция распознает ключевые слова
               и добавляет к строке два символа $.
               Если кирилицу и латиница идут вперемежку, символ $ добавляется
               в конце группы символо кирилицы или латиницы.
               Например, АБВRTАБ -> ABV$RTAB$. Если строка заканчивается
               латиницей символ не $ добавляется.
  Parameters:  iStr - строка для преобразования
  Notes:       Например, поле временной таблицы не может содержать кириллицу
               в идентифкаторе.
------------------------------------------------------------------------------*/
FUNCTION GetMangledName RETURNS CHARACTER (iStr AS CHAR):
   DEFINE VARIABLE vCnt  AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen  AS INT64    NO-UNDO.
   DEFINE VARIABLE vDst  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vChr  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMng  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vRus  AS LOGICAL    NO-UNDO. /* Признак кирилицы */
   DEFINE VARIABLE vMngl AS CHARACTER  NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","iStr:" + GetNullStr(iStr)).
   &ENDIF
   FIND FIRST ttMangled WHERE 
              ttMangled.i_name EQ iStr 
      NO-LOCK NO-ERROR.
   IF AVAILABLE ttMangled THEN RETURN ttMangled.o_name.
   
   /* Вызов быстрой функции манглирования
   ** в большинстве случаев в строке нет смеси латиницы и кирилицы */
   vMngl = GetMangledNameS (iStr).

   /* Если строка отманглирована без ошибки, то возврат */
   IF vMngl NE ? THEN
   DO:
      CREATE ttMangled.
      ASSIGN
         ttMangled.o_name = GetNameForXml(vMngl)
         ttMangled.i_name = iStr
      .
      RETURN ttMangled.o_name.
   END.

   /* Во входной строке смесь латиницы и кирилицы
   ** выполним соответствующее преобразование */
   vLen = LENGTH(iStr).

   /* Commented BY KSV: Если в качестве строки передано ключевое
   ** слово, манглируем его двойным символом манглирования */
   IF KEYWORD(iStr) <> ? THEN
   DO:
      CREATE ttMangled.
      ASSIGN
         ttMangled.o_name = GetNameForXml(iStr + {&MANGLED_CHR} + {&MANGLED_CHR})
         ttMangled.i_name = iStr
      .
      RETURN ttMangled.o_name.
   END.

   DO vCnt = 1 TO vLen:

      vChr = SUBSTR(iStr,vCnt,1).
      /* Если символ не является буквой,
      ** просто добавляем его в выходную строку */
      IF NOT IsAlpha(vChr) THEN DO:
         vDst = vDst + vChr.
         NEXT.
      END.

      vMng = mMangled[asc(vChr)].

      /* Добавляем символом манглирования в конец группы
      ** кирилицы или латиницы */
      IF NOT {assigned vMng} THEN DO:   /* Не кирилица */
         vMng = vChr.
         IF vCnt > 1 AND
            vRus     THEN
            vMng = {&MANGLED_CHR} + vMng.
         vRus = NO.
      END.
      ELSE DO:
         IF vCnt > 1 AND NOT
            vRus     THEN
            vMng = {&MANGLED_CHR} + vMng.
         vRus = YES.
      END.

      vDst = vDst + vMng.
   END.

   /* Манглируем строку оканчивающуюся кирилицей */
   IF vRus THEN
      vDst = vDst + {&MANGLED_CHR}.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","vDst:" + GetNullStr(vDst)).
   &ENDIF

   CREATE ttMangled.
   ASSIGN
      ttMangled.o_name = GetNameForXml(vDst)
      ttMangled.i_name = iStr
   .
   RETURN ttMangled.o_name.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает оригинальную строку из "мангловой" строки. Выполняет
               обратное преобразование значения функции GetMangledName.
  Parameters:  iMangledStr - "мангловая" строка.
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetOriginalName RETURNS CHARACTER (iMangledStr AS CHAR):
   DEFINE VARIABLE vLen  AS INT64    NO-UNDO.
   DEFINE VARIABLE vCnt  AS INT64    NO-UNDO.
   DEFINE VARIABLE vChr  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDst  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vRus  AS LOGICAL    NO-UNDO. /* Признак кирилицы */
   DEFINE VARIABLE vMngl AS CHARACTER  NO-UNDO.

   iMangledStr = GetNameForProgress(iMangledStr).
   IF NOT iMangledStr MATCHES "*" + {&MANGLED_CHR} + "*" THEN
      RETURN iMangledStr.

   /* Commented BY KSV: Если строка завершается двойным символом манглирования,
   ** то это ключевое слово, деманглируем его, откидывая двойной символ
   ** манглирования */
   IF iMangledStr MATCHES "*" + {&MANGLED_CHR} + {&MANGLED_CHR} THEN
      RETURN TRIM(iMangledStr,{&MANGLED_CHR}).

   /* Вызов быстрой функции деманглирования
   ** в большинстве случаев в строке нет смеси латиницы и кирилицы */
   vMngl = GetOriginalNameS (iMangledStr).

   /* Проверим деманглированую строку
   ** в ней не должно остаться символов манглирования */
   IF NOT vMngl MATCHES "*" + {&MANGLED_CHR} + "*" THEN
      RETURN vMngl.

   /* Во входной строке смесь латиницы и кирилицы
   ** выполним соответствующее преобразование */
   vLen = LENGTH(iMangledStr).
   /* Перебор входной строки справа налево */
   DO vCnt = vLen TO 1 BY -1:
      vChr = SUBSTR(iMangledStr,vCnt,1).

      /* Отбрасываем символ манглирования */
      IF vChr = {&MANGLED_CHR} THEN
      DO:
         vRus = NOT vRus.
         NEXT.
      END.

      /* Если символ не кирилица
      ** просто добавляем его в выходную строку */
      IF NOT vRus          OR
         NOT IsAlpha(vChr) THEN DO:
         vDst = vChr + vDst.
         NEXT.
      END.
      /* Преобразование кирилицы */
      IF vCnt                           >  1  AND
         SUBSTR(iMangledStr,vCnt - 1,1) = "w" THEN
      DO:
         vChr = SUBSTR(iMangledStr,vCnt - 1,1) + vChr.
         vCnt = vCnt - 1.
      END.
      CASE vChr:
         WHEN "a"   THEN vChr = "а".
         WHEN "b"   THEN vChr = "б".
         WHEN "v"   THEN vChr = "в".
         WHEN "g"   THEN vChr = "г".
         WHEN "d"   THEN vChr = "д".
         WHEN "e"   THEN vChr = "е".
         WHEN "wo"  THEN vChr = "ё".
         WHEN "wz"  THEN vChr = "ж".
         WHEN "z"   THEN vChr = "з".
         WHEN "i"   THEN vChr = "и".
         WHEN "wi"  THEN vChr = "й".
         WHEN "k"   THEN vChr = "к".
         WHEN "l"   THEN vChr = "л".
         WHEN "m"   THEN vChr = "м".
         WHEN "n"   THEN vChr = "н".
         WHEN "o"   THEN vChr = "о".
         WHEN "p"   THEN vChr = "п".
         WHEN "r"   THEN vChr = "р".
         WHEN "s"   THEN vChr = "с".
         WHEN "t"   THEN vChr = "т".
         WHEN "u"   THEN vChr = "у".
         WHEN "f"   THEN vChr = "ф".
         WHEN "h"   THEN vChr = "х".
         WHEN "c"   THEN vChr = "ц".
         WHEN "wc"  THEN vChr = "ч".
         WHEN "ws"  THEN vChr = "ш".
         WHEN "wt"  THEN vChr = "щ".
         WHEN "w%"  THEN vChr = "ъ".
         WHEN "w#"  THEN vChr = "ь".
         WHEN "y"   THEN vChr = "ы".
         WHEN "we"  THEN vChr = "э".
         WHEN "wu"  THEN vChr = "ю".
         WHEN "wa"  THEN vChr = "я".
      END CASE.
      vDst = vChr + vDst.
   END.
   RETURN vDst.
END FUNCTION.

/* По номеру элемента в списке определяет позицию
** с которой начиснется данный элемент.*/
FUNCTION GetPosEntry RETURN INT64 (
   INPUT iStr   AS CHAR, /* Строка  */
   INPUT iEntry AS INT64,  /* Елемент, позицию которого необходимо определить. */
   INPUT iDel   AS CHAR  /* Разделитель элиментов в строке. */
):
   DEF VAR vCnt AS INT64 NO-UNDO. /* Счетчик. */
   DEF VAR vPos AS INT64 NO-UNDO. /* Позиция с которой начинается элемент в списке. */

   DO vCnt = 1 TO NUM-ENTRIES (iStr, iDel):
      IF vCnt EQ iEntry
         THEN RETURN vPos + 1.
      vPos = INDEX (iStr, iDel, vPos + 1).
   END.
   RETURN 0.
END FUNCTION.

&ENDIF

/* Находит пересечение двух списков */
FUNCTION ListsCrossing RETURN CHAR (
   INPUT iList1 AS CHARACTER,
   INPUT iList2 AS CHARACTER,
   INPUT iDelim AS CHARACTER
):
   DEF VAR vi        AS INT64    NO-UNDO.
   DEF VAR vResult   AS CHAR   NO-UNDO.
   DEF VAR vEntryVal AS CHAR   NO-UNDO.
   DEF VAR vTmpList AS CHAR   NO-UNDO.

   IF iDelim EQ ? THEN iDelim = ",".   /* разделитель по-умолчанию - "," */

         /* находим более короткий список */
   IF NUM-ENTRIES(iList2,iDelim) LT NUM-ENTRIES(iList1,iDelim) THEN
   ASSIGN
      vTmpList = iList1
      iList1   = iList2
      iList2   = vTmpList
   .
         /* перебираем более короткий список */
   DO vi = 1 TO NUM-ENTRIES(iList1,iDelim):
      vEntryVal = ENTRY(vi,iList1,iDelim).
            /* находим вхождение элемента первого списка во второй */
      IF LOOKUP(vEntryVal,iList2) GT 0
            /* если вхождение найдено, добавлем элемент в результирующий вписок */
      THEN {additem.i vResult vEntryVal}
   END.
   RETURN vResult.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Получение заданного диапазона из строки, выход её за пределы
               с какой-либо стороны дополняет результат специальным символом.
  Parameters:  iValue   - исходная строка.
               iStart   - начало диапазона, неопределённое значение соответству-
                          ет началу строки.
               iEnd     - конец диапазона, неопределённое значение соответствует
                          концу строки.
               iDefault - символ, которым дополняется результат при выходе 
                          запрошенного диапазона за границу строки
  Notes:       Примеры:
               GetValueRange("abcdefgh", 3, 7, "*") = "cdefg"
               GetValueRange("abcdefgh", -4, 5, "*") = "*****abcde"
               GetValueRange("abcdefgh", ?, 10, "*") = "abcdefgh**"
               GetValueRange("abcdefgh", 70, 73, "*") = "****"
------------------------------------------------------------------------------*/
FUNCTION GetValueRange RETURN CHARACTER (INPUT iValue   AS CHARACTER,
                                         INPUT iStart   AS INT64,
                                         INPUT iEnd     AS INT64,
                                         INPUT iDefault AS CHARACTER):
   DEFINE VARIABLE vN AS INT64 NO-UNDO.
   DEFINE VARIABLE vM AS INT64 NO-UNDO.
   DEFINE VARIABLE vL AS INT64 NO-UNDO.
   DEFINE VARIABLE vR AS INT64 NO-UNDO.

   IF iValue = ? THEN
      RETURN ?.
   ASSIGN
      vN = LENGTH(iValue)
      vM = LENGTH(iDefault)
   .
   ASSIGN
      iStart = 1  WHEN iStart = ?
      iEnd   = vN WHEN iEnd   = ?
   .
   IF iStart > iEnd THEN
      RETURN ?.
   IF (iStart < 1 OR iEnd > vN) AND (iDefault = ? OR vM > 1) THEN
      RETURN ?.
   IF iStart > vN OR iEnd < 1 THEN
      RETURN FILL(iDefault, 1 + ABSOLUTE(iEnd - iStart)).
   ASSIGN
      vL = 1 - iStart WHEN iStart < 1
      vR = iEnd - vN  WHEN iEnd > vN
   .
   RETURN FILL(iDefault, vL) +
          SUBSTRING(iValue,
                    MAXIMUM(1, iStart),
                    MINIMUM(vN, iEnd - iStart + (vR - vL) * vM + 1)) +
          FILL(iDefault, vR).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Синтаксический разбор выражения, описывающего целочисленный
               интервал. Выражение должно иметь вид
               "SXSaS[ZSbS]YS",
               где S - любое количество символов-разделителей, a и b - целые
               числа, X и Y - открывающаяся и закрывающаяся скобка соотв., 
               Z - разделитель диапазона, часть в квадратных скобках может
               отсутствовать.
  Parameters:  iStrRange - выражение.
               iABC      - строка из трёх сиволов, 1-й и 2-й - открыващаяся и
                           закрывающаяся скобки соответственно, 3-й - разде-
                           литель диапазона.
               oStart    - начало диапазона, при ошибках разбора выражения 
                           будет возвращено неопределённое значение.
               oEnd      - конец диапазона, неопределённое значение означает, 
                           что конец диапазона не задан (не ошибка).
  Notes:       Диапазоны без "скобок" не поддерживаются.
               Разделитель не должен совпадать с "-" в тех случаях, когда 
               начало или конец могут быть отрицательными.
------------------------------------------------------------------------------*/
PROCEDURE ParseIntRange.
   DEFINE INPUT  PARAMETER iStrRange AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iABC      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oStart    AS INT64     NO-UNDO INITIAL -1.
   DEFINE OUTPUT PARAMETER oEnd      AS INT64     NO-UNDO INITIAL -1.

   DEFINE VARIABLE vStart AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEnd   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSep   AS CHARACTER NO-UNDO.

   IF iStrRange                        =  ? OR
      LENGTH(iABC)                     <> 3 OR
      LENGTH(TRIM(iABC, "0123456789")) <  3
   THEN
      RETURN.
   ASSIGN
      vStart = SUBSTRING(iABC, 1, 1)
      vEnd   = SUBSTRING(iABC, 2, 1)
      vSep   = SUBSTRING(iABC, 3, 1)
   .
   IF vStart = vEnd OR
      vStart = vSep OR
      vEnd   = vSep
   THEN
      RETURN.
   iStrRange = TRIM(iStrRange).
   IF INDEX(iStrRange, vStart) = 1 AND
      INDEX(iStrRange, vEnd)   = LENGTH(iStrRange)
   THEN DO:
      iStrRange = SUBSTRING(iStrRange, 2, LENGTH(iStrRange) - 2).
      IF NUM-ENTRIES(iStrRange, vSep) < 3 THEN DO:
         ASSIGN
            vStart = ?
            vEnd   = ?
         .
         ASSIGN
            vStart = ENTRY(1, iStrRange, vSep)
            vEnd   = ENTRY(2, iStrRange, vSep)
                     WHEN NUM-ENTRIES(iStrRange, vSep) = 2
         .
         IF vStart <> ? THEN DO:
            ASSIGN
               oStart = INT64(TRIM(vStart))
               oEnd   = INT64(TRIM(vEnd))
                        WHEN vEnd <> ?
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               RETURN.
         END.
      END.
   END.
END PROCEDURE.

FUNCTION toLogical RETURN LOGICAL (INPUT iStr AS CHARACTER):
   DEFINE VARIABLE vResult AS LOGICAL NO-UNDO.

   IF CAN-DO("YES,ДА", iStr) THEN
      vResult = YES.
   ELSE IF CAN-DO("NO,НЕТ", iStr) THEN
      vResult = NO.
   ELSE
      vResult = ?.
   RETURN vResult.
END FUNCTION.
/* $LINTUSER='BIS' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='$/ws1-dvp/bq/' */
/* $LINTDATE='12/11/2014 10:09:45.058+04:00' */
/* $LINTFILE='strings.fun' */
/*prosign1hFaDQCexw+hhbdz/s62ew*/