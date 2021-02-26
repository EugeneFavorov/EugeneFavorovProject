/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2000 ТОО "Банковские информационные системы"
     Filename: SIGNS.FUN
      Comment: инклюдник, содержащий технологические внутренние
               функции и процедуры работы с signs и code
   Parameters:
         Uses:
      Used BY:
      Created: 25/04/2000 Olenka
     Modified: 19/07/2000 Om      Перенос функции GetCodeName.
     Modified: 30/01/2001 Kostik  Добавил проверку на повторное объявление функций.
                                  Это потребовалось в transit.i
     Modified: 10/04/2001 serge   оптимизация
     Modified: 18/04/2001 yakv    Create/UpdateSigns: параметр "inindexed"
     Modified: 21/02/2002 serge   много чего
     Modified: 19/03/2002 GunK    Процедура поиска по code-val
     Modified: 26/01/2003 Om      Добавлена функция DelSignsCodeVal.
     Modified: 28/04/2003 koag    Добавлена функция GetCodeEx.
     Modified: 08/05/2003 Om      Из xclass.pro перенесена процедура GetXAttr.
     Modified: 05/04/2004 NIK     Добавлена функция GetCodeBuff.
     Modified: 22/08/2005 NIK     У кого-то очень кривые руки...
     Modified: 03.09.2005 17:39 serge    0051030 Оптимизация использования временных таблиц
                                         и настроечных параметров. Восстановил сигнатуру
                                         вызова UpdateSigns, первый параметр должен всегда
                                         содержать file-name, если isIndexed <> ?
     Modified: 23.09.2008 11:58 KSV      (0097155) Выделена процедура
                                         CopySignsEx
     Modified: 13.10.2008 11:36 ariz     Исправлена ошибка: не удалялись значения
                                  темпорированных допреквизитов

   Функции:
      AvailCode     - проверяет есть ли запись в классификаторе
      AvailXattr    - проверяет есть ли определенная привязка у объекта
      AvailAnySigns     - проверяет есть ли определенная привязка по всем объектам
      GetSigns      - возвращает значение допреквизита
      GetCode       - возвращает значение классификатора (code.val)
      GetCodeEx     - возвращает значение классификатора (code.val), либо
                        указанное 3-м параметром значение (в случае
                        отсутствия классификатора)
      GetCodeName   - возвращает название классификатора (code.name)
      GetCodeMisc   - возвращает дополнительное значение (code.misc[i])
      CreateSigns   - создает новый допреквизит (использовать только при
                          создании нового объекта)
      UpdateSigns   - модификация допреквизита, если известны индексируемость
                      и имя таблицы
      UpdateSignsEx   - модификация допреквизита по имени класса
      UpdateTempSignsEx - Создание/редактирование ТЕМПОРИРОВАННОГО ДР на определенную дату
      UpdateTempSigns   - Создание/редактирование ТЕМПОРИРОВАННОГО ДР на глобальную дату
      isXAttrIndexed    - проверяет, индексированный ли реквизит
      AvailSignsCode    - проверяет есть ли допреквизит по параметру классификатора
      AvailSignsAnyCode - проверяет есть ли допреквизит по классификатору
      GetXclassFile - по классу возвращает Прогресcную таблицу, к которой
                          привязываются допреквизиты (для signs.file-name)
      GetXAttrValueEx   - возвращет значение допреквизита с учетом значения
                          по умолчанию
      GetXAttrInit      - возвращет начальное значение реквизита класса

   Процедуры:
      DelSigns          - удаляет все допреквизиты объекта
      DelSignsCode      - удаляет все допреквизиты по параметру классификатора
      DelSignsAllCode   - удаляет все допреквизиты по классификатору
      FindSignsByVal    - Осуществляет поиск по code-val,code и списку
                          возможных файлов. Возвращает суррогат записи и
                          номер найденного signs в списке файлов.
      DelSignsCodeVal   - Удаляет со всех объектов выбранной таблицы
                          заданное значение дополнительного реквизита
      CopySigns         - копирование доп.реквизитов одного объекта на другой
      GetXAttr          - возвращает буффер с указанным реквизитом класса
*/

{read-only.fun}

&IF DEFINED (PP-XCLAS) EQ 0
&THEN
   &MESSAGE ** ЗАМЕЧАНИЕ - вызван signs.fun, используйте INTRFACE.GET xclass
&ENDIF

&IF DEFINED(MethodSignsAlredyDefined) EQ 0 &THEN

{xattrtmp.def}          /* Таблица содержит перечень темпоральных ДР. */
 
/* Собирает список темпоральных ДР. */
PROCEDURE GetAllXattrTmp PRIVATE.
   DEF BUFFER class  FOR   class.   /* Локализация буфера. */
   DEF BUFFER xattr  FOR   xattr.   /* Локализация буфера. */
                        /* Перебираем все классы метасхемы для формирования,
                        ** темпорального списка. */
   EMPTY TEMP-TABLE ttXattrTemp.

   BLCK_CLASS:
   FOR EACH Xattr WHERE xattr.temporal
                    AND xattr.sign-inherit EQ "с"
                    AND xattr.DATA-TYPE NE "class"
   NO-LOCK,
   FIRST CLASS WHERE CLASS.Class-Code     EQ xattr.Class-Code
                 AND class.Progress-Table
   NO-LOCK:
                        /* Реквизит может быть темпоральным в срезе таблицы. */
      FIND FIRST ttXattrTemp WHERE ttXattrTemp.fTable   EQ class.Progress-Code
                               AND ttXattrTemp.fXattr   EQ xattr.xattr-code
      NO-ERROR.
      IF AVAIL ttXattrTemp
         THEN NEXT BLCK_CLASS.
      CREATE ttXattrTemp.
      ASSIGN
         ttXattrTemp.fTable   =  class.Progress-Code
         ttXattrTemp.fXattr   =  xattr.xattr-code
         ttXattrTemp.fBasic   =  xattr.Progress-Field
         ttXattrTemp.fIndexed =  xattr.Indexed
      .
   END.
   RETURN.
END PROCEDURE.

/* "Выгружает" временную таблицу с кодами темпорированными реквизитами */
PROCEDURE GetXattrTmp.
   DEFINE OUTPUT PARAMETER TABLE FOR ttXattrTemp.
   RETURN.
END PROCEDURE.

/* Определяет темпорированный ли реквизит */
FUNCTION IsTemporal RETURN LOGICAL (
   INPUT iTable AS CHARACTER,
   INPUT iXAttr AS CHARACTER
):
   RETURN CAN-FIND(FIRST ttXattrTemp WHERE ttXattrTemp.fTable EQ iTable
                                       AND ttXattrTemp.fXAttr EQ iXAttr
               NO-LOCK).
END FUNCTION.

/* по классу возвращает Прогресcную таблицу, к которой
   привязываются допреквизиты (для signs.file-name)
*/
FUNCTION GetXclassFile RETURN CHAR
        (INPUT io-class AS CHAR):

   FIND class WHERE class.class-code EQ io-class NO-LOCK NO-ERROR.
   RETURN IF AVAIL class THEN class.progress-code ELSE ?.

END.

/* Получение класса объекта. */
FUNCTION GetClassObj RETURN CHAR (
   INPUT iBH AS HANDLE  /* Указатель на буффер объекта. */
):
   DEF VAR mClassCode AS CHAR NO-UNDO. /* Класс объекта. */

   DEF BUFFER xattr FOR xattr. /* Локализация буфера. */
                        /* Исключения из правил. */
   IF CAN-DO (
      "class,xattr,class-method,xlink,xstatus,xtrans-link,xtrans-status,xlink-valid",
      iBH:TABLE)
      THEN RETURN iBH:TABLE.
                        /* Если есть поле "class-code", то берем из него. */
   mClassCode = GetValue (iBH, "class-code").
                        /* Если такого поля нет,то класс объекта
                        ** либо таблица, либо значение ДР "class-code". */
   IF NOT  {assigned mClassCode} 
   THEN DO:
                        /* По умолчанию, код таблицы и есть класс. */
      mClassCode = iBH:TABLE.
                        /* Получаем описание ДР. */
      RUN GetXattr (mClassCode, "class-code", BUFFER xattr).
                        /* Если указан ДР "class-code",
                        ** то его значение и есть класс объекта. */

                        /* Берем значение ДР "class-code" с объекта.
                        ** Если значние реквизита не определено,
                        ** то по умолчанию класс - имя таблицы.*/
      IF     AVAIL xattr              
         AND NOT xattr.progress-field
      THEN ASSIGN
                        /* Получаем значени класса из ДР объекта. */
         mClassCode = GetXAttrValueEx (
                        iBH:TABLE,
                        GetSurrogateBuffer(iBH:TABLE, iBH),
                        "class-code",
                        mClassCode)
                        /* Если полученное значение отличается от имени таблицы,
                        ** то проверяем наличие класса в метасхеме. */
         mClassCode = IF      iBH:TABLE NE mClassCode
                        AND   CAN-FIND (FIRST class WHERE
                                          class.class-code EQ mClassCode
                                        NO-LOCK)
                        /* Если описание найдено, то mClassCode - класс,
                        ** иначе код таблицы и есть класс объекта. */
                        THEN mClassCode
                        ELSE iBH:TABLE
      .
   END.
   RETURN mClassCode.
END FUNCTION.

FUNCTION AvailXattr RETURN LOGICAL
        (INPUT infile AS CHAR,
         INPUT insurr AS CHAR,
         INPUT inclass-code AS CHAR):

   IF inclass-code = ?
   THEN DO:
               /* Поиск значения темпорированного ДР по заданным параметрам на текущую дату */
      FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ infile
                           AND tmpsigns.surrogate  EQ insurr
                           AND tmpsigns.since      LE gend-date
      NO-LOCK NO-ERROR.
                     /* есть обычный ДР или */
      RETURN    CAN-FIND(FIRST signs WHERE signs.file-name EQ infile
                                       AND signs.surrogate EQ insurr)
                     /* есть темпорированный ДР */
             OR       (AVAIL tmpsigns
                  AND (tmpsigns.code-value     NE ""
                   OR  tmpsigns.xattr-value    NE "")).
   END.
   ELSE IF NUM-ENTRIES(inclass-code) GT 1
   THEN DO:
               /* Поиск значения темпорированного ДР по заданным параметрам на текущую дату */
      FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ infile
                           AND tmpsigns.code       EQ ENTRY(1,inclass-code)
                           AND tmpsigns.surrogate  EQ insurr
                           AND tmpsigns.since      LE gend-date
      NO-LOCK NO-ERROR.
                     /* есть обычный ДР или */
      RETURN    CAN-FIND(FIRST signs WHERE signs.file-name  EQ infile
                                       AND signs.surrogate  EQ insurr
                                       AND signs.code       EQ ENTRY(1,inclass-code)
                                       AND signs.code-value EQ ENTRY(2,inclass-code))
                     /* есть темпорированный ДР */
             OR       (AVAIL tmpsigns
                  AND (tmpsigns.code-value   EQ ENTRY(2,inclass-code)
                   OR  tmpsigns.xattr-value  EQ ENTRY(2,inclass-code))).
   END.
   ELSE DO:
               /* Поиск значения темпорированного ДР по заданным параметрам на текущую дату */
      FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ infile
                           AND tmpsigns.code       EQ inclass-code
                           AND tmpsigns.surrogate  EQ insurr
                           AND tmpsigns.since      LE gend-date
      NO-LOCK NO-ERROR.
                     /* есть обычный ДР или */
      RETURN    CAN-FIND(FIRST signs WHERE signs.file-name  EQ infile
                                       AND signs.surrogate  EQ insurr
                                       AND signs.code       EQ inclass-code)
                     /* есть темпорированный ДР */
             OR       (AVAIL tmpsigns
                  AND (tmpsigns.code-value     NE ""
                   OR  tmpsigns.xattr-value    NE "")).
   END.

END.

FUNCTION AvailAnySigns RETURN LOGICAL
        (INPUT infile AS CHAR,
         INPUT inclass-code AS CHAR):

   RETURN    CAN-FIND(FIRST signs WHERE signs.file-name EQ infile
                                 AND signs.code EQ inclass-code)
          OR CAN-FIND(LAST  tmpsigns WHERE tmpsigns.file-name  EQ infile
                                       AND tmpsigns.code       EQ inclass-code
                                       AND tmpsigns.since      LE gend-date
                                       AND tmpsigns.code-value NE "").
END.

/* проверяет есть ли запись в классификаторе */
FUNCTION AvailCode RETURN LOGICAL (INPUT ipClass AS CHAR, INPUT ipCode AS CHAR):
   RETURN CAN-FIND(FIRST code WHERE code.class EQ ipClass
                                AND code.code EQ ipCode NO-LOCK).
END.


/* Проверяет, индексированный ли дополнительный реквизит (ДР).
** Возвращаемые значения:
** ?     - ДР не найден.
** YES   - ДР   индексируемый.
** NO    - ДР НЕиндексируемый.
** */
FUNCTION IsXAttrIndexed RETURNS LOG (
   INPUT cname AS CHAR,    /* Класс объекта. */
   INPUT ccode AS CHAR     /* Код дополнительного реквизита. */
):

   DEFINE BUFFER xattr FOR xattr.   /* Локализация буфера. */

   /* Поиск буфера ДР. */
   RUN GetXAttr (cname,
                 ccode,
                 BUFFER xattr).

   RETURN IF AVAIL xattr
             THEN xattr.indexed
             ELSE ?.
END.

FUNCTION GetCode RETURN CHAR
        (INPUT inclass AS CHAR,
         INPUT incode AS CHAR):

   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ inclass AND
                         code.code EQ incode NO-LOCK NO-ERROR.
   RETURN IF NOT AVAIL code THEN ? ELSE code.val.
END.

FUNCTION GetCodeEx RETURN CHAR
        (INPUT inclass AS CHAR,
         INPUT incode AS CHAR,
         INPUT inReturn AS CHAR):

   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ inclass AND
                         code.code EQ incode NO-LOCK NO-ERROR.
   RETURN IF NOT AVAIL code THEN inReturn ELSE code.val.
END.

FUNCTION GetCodeNameEx RETURN CHAR
        (INPUT iClass   AS CHAR,  /* Класс классификатора. */
         INPUT iCode    AS CHAR,  /* Код классификатора. */  
         INPUT iReturn  AS CHAR): /* Значение по умолчанию */

   DEFINE BUFFER code FOR code.

   FIND FIRST code WHERE 
              code.class EQ iClass 
          AND code.code  EQ iCode 
      NO-LOCK NO-ERROR.

   RETURN IF NOT AVAIL code THEN iReturn ELSE code.name.
END.

FUNCTION GetCodeName RETURN CHAR
        (INPUT inclass AS CHAR,
         INPUT incode AS CHAR):
   RETURN GetCodeNameEx(inclass,incode,?).
END.

/* Возвращает дополнительное значение классификатора
** code.misc[i] */
FUNCTION GetCodeMiscEx RETURN CHARACTER
        (INPUT iClass  AS CHARACTER, /* Класс классификатора. */
         INPUT iCode   AS CHARACTER, /* Код классификатора. */
         INPUT iPos    AS INT64,
         INPUT iReturn AS CHARACTER): /* Порядковый номер расширения. */

   DEFINE BUFFER code FOR code. /* Локализация буфера. */

   FIND FIRST code WHERE
              code.class EQ iClass
          AND code.code  EQ iCode
      NO-LOCK NO-ERROR.

   RETURN IF NOT AVAIL code THEN iReturn ELSE code.misc[iPos].
END.

/* Возвращает дополнительное значение классификатора
** code.misc[i] */
FUNCTION GetCodeMisc RETURN CHAR
        (INPUT ipClassChar AS CHAR, /* Класс классификатора. */
         INPUT ipCodeChar  AS CHAR, /* Код классификатора. */
         INPUT ipPosInt    AS INT64): /* Порядковый номер расширения. */

   RETURN GetCodeMiscEx(ipClassChar,ipCodeChar,ipPosInt,?).
END.

/* Возвращает описание классификатора
** code.description[i] */
FUNCTION GetCodeDesc RETURN CHAR
        (INPUT ipClassChar AS CHAR, /* Класс классификатора. */
         INPUT ipCodeChar  AS CHAR, /* Код классификатора. */
         INPUT ipPosInt    AS INT64,  /* Порядковый номер расширения. */
         INPUT inReturn    AS CHAR):

   DEFINE BUFFER code FOR code. /* Локализация буфера. */

   FIND FIRST code WHERE
              code.class EQ ipClassChar
          AND code.code  EQ ipCodeChar
              NO-LOCK NO-ERROR.

   IF AVAILABLE(code)
      THEN RETURN code.description[ipPosInt].
      ELSE RETURN inReturn.
END.

/*----------------------------------------------------------------------------*/
/* Возвращает буфер SIGNS                                                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetCodeBuff LOGICAL (INPUT  iClass AS  CHAR,
                              INPUT  iCode  AS  CHAR,
                              BUFFER bCode  FOR Code):
   FIND FIRST bCode WHERE
              bCode.Class EQ iClass
          AND bCode.Code  EQ iCode
              NO-LOCK NO-ERROR.
   RETURN AVAILABLE(bCode).
END FUNCTION.

/* Непосредственно сохранение значения ТЕМПОРИРОВАННОГО ДР
** без проверки темпорированности допреквизита,
** без вычисления имени таблицы, признака индексируемости и т.п.
** В infile должен передаваться код таблицы, а не класса. */
PROCEDURE UpdateTempSignsExDirect.
   DEF INPUT  PARAM infile AS CHAR   NO-UNDO.   /* Наименование таблицы (код класса). */         
   DEF INPUT  PARAM insurr AS CHAR   NO-UNDO.   /* Идентификатор (суррогат) записи в таблице. */ 
   DEF INPUT  PARAM incode AS CHAR   NO-UNDO.   /* Код ДР. */                                    
   DEF INPUT  PARAM indate AS DATE   NO-UNDO.   /* Дата ДР */                                    
   DEF INPUT  PARAM inval  AS CHAR   NO-UNDO.   /* Значение ДР. */                               
   DEF INPUT  PARAM inindx AS LOG    NO-UNDO.   /* Признак индексируемости */
   DEF OUTPUT PARAM oFlag  AS LOG    NO-UNDO.   /* Флаг успешного создания ДР */

   DEFINE VARIABLE vListCS AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStrCS AS CHARACTER   NO-UNDO CASE-SENSITIVE .

   vListCS = GetHistoryFieldsCS ( infile ). 

   DEF BUFFER class        FOR class.
   DEF BUFFER tmpsigns     FOR tmpsigns.
   DEF BUFFER b-tmpsigns   FOR tmpsigns.

   /* старый суррогат "commission,acct,currency,kau,min-value,period,since" меняем на новый "comm-rate-id" */
   DEF BUFFER comm-rate FOR comm-rate.
   IF GetXclassFile (infile) EQ "comm-rate" AND NUM-ENTRIES(insurr) > 3 THEN
   DO:
      IF ENTRY(4,insurr) <> "" THEN
         FIND FIRST comm-rate WHERE
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
         NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST comm-rate WHERE
                    comm-rate.filial-id = shfilial AND
                    comm-rate.branch-id = "" AND
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
            NO-LOCK NO-ERROR.
         IF AVAIL comm-rate THEN
            insurr = STRING(comm-rate.comm-rate-id).
   END.

   BLCK:
   DO
   ON ERROR  UNDO BLCK, LEAVE BLCK
   ON ENDKEY UNDO BLCK, LEAVE BLCK:
                        /* Поиск значения темпорированного допреквизита на дату */
      FIND LAST tmpsigns  WHERE tmpsigns.file-name   EQ infile
                            AND tmpsigns.code        EQ incode
                            AND tmpsigns.surrogate   EQ insurr
                            AND tmpsigns.since       LE indate
      NO-LOCK NO-ERROR.
                        /* Если передано ?, то создаем запись с пустым значением ДР */
      IF inval EQ ? THEN inval = "".
                        /* Переданное значение равно предыдущему существующему -
                        ** не создаем новую запись */
      IF CAN-DO(vListCS,incode) THEN
      DO:
         vStrCS = inval .
         /* требуется проверка с учетом регистра */
         IF     AVAIL tmpsigns
            AND ((    inindx
                  AND tmpsigns.code-value EQ vStrCS)
                 OR 
                 (    NOT inindx
                  AND tmpsigns.xattr-value EQ vStrCS))
         THEN DO:
            RELEASE tmpsigns.
            oFlag = YES.
            LEAVE BLCK.
         END.
      END.
      ELSE 
      DO:
         IF     AVAIL tmpsigns
            AND ((    inindx
                  AND tmpsigns.code-value EQ inval)
                 OR 
                 (    NOT inindx
                  AND tmpsigns.xattr-value EQ inval))
         THEN DO:
            RELEASE tmpsigns.
            oFlag = YES.
            LEAVE BLCK.
         END.
      END.
                        /* Если найденное значение не на требуемую дату - оно нам больше не понадобится */
      IF AVAIL tmpsigns AND tmpsigns.since NE indate THEN
         RELEASE tmpsigns.

      TR:
      DO TRANSACTION
      ON ERROR  UNDO TR, LEAVE TR
      ON ENDKEY UNDO TR, LEAVE TR:
                     /* Если не найдено существующее значение темпорированного
                     ** ДР на заданную дату, создаем новое, иначе редактируем. */
         IF NOT AVAIL tmpsigns THEN
         DO:
            CREATE tmpsigns.
            ASSIGN
               tmpsigns.file-name   = infile
               tmpsigns.code        = incode
               tmpsigns.surrogate   = insurr
               tmpsigns.since       = indate
            .
         END.
         ELSE
            FIND CURRENT tmpsigns EXCLUSIVE-LOCK NO-ERROR.
                     /* Изменение значение темпорированного ДР */
         ASSIGN
            tmpsigns.code-value  = IF inindx THEN inval ELSE ""
            tmpsigns.xattr-value = IF inindx THEN ""    ELSE inval
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO TR, LEAVE TR.
         tmpsigns.dec-value   = IF NOT {assigned inval} THEN ? ELSE DEC(inval) NO-ERROR.
         &IF DEFINED (ORACLE) &THEN
         IF tmpsigns.dec-value > 999999999999999.99 THEN tmpsigns.dec-value = ?. /* ограничение формата поля ORACLE */
         &ENDIF
         tmpsigns.date-value  = DATE(inval) NO-ERROR.
         RELEASE tmpsigns.
         oFlag =  YES.
      END.
   END.
END PROCEDURE.

/* Создание/редактирование ТЕМПОРИРОВАННОГО ДР на определенную дату
** Функция возвращает:
**    YES - ДР успешно создан
**    NO  - ДР не обработан
**    ?   - ДР не темпорированный */
FUNCTION UpdateTempSignsEx RETURN LOG (
   INPUT infile       AS CHAR,   /* Наименование таблицы (код класса). */
   INPUT insurr       AS CHAR,   /* Идентификатор (суррогат) записи в таблице. */
   INPUT incode       AS CHAR,   /* Код ДР. */
   INPUT indate       AS DATE,   /* Дата ДР */
   INPUT inval        AS CHAR,   /* Значение ДР. */
   INPUT inindexed    AS LOG    /* Признак индексируемости */
):
   DEF VAR vFlag     AS LOG    NO-UNDO. /* Результат обработки ДР. */
   DEF VAR vTable    AS CHAR   NO-UNDO. /* Таблица класса. */

   DEF BUFFER comm-rate FOR comm-rate.

   MAIN_BLOCK:
   DO:
                        /* Определение имени таблицы. */
      vTable = GetXclassFile (infile).
      IF vTable EQ ?
      THEN DO:
         vFlag = ?.
         LEAVE MAIN_BLOCK.
      END.
                        /* старый суррогат "commission,acct,currency,kau,min-value,period,since" 
                        ** меняем на новый "comm-rate-id" */
      IF vTable EQ "comm-rate" AND NUM-ENTRIES(insurr) > 3 THEN
      DO:
         IF ENTRY(4,insurr) <> "" THEN
            FIND FIRST comm-rate WHERE
                       comm-rate.commission = ENTRY(1,insurr) AND
                       comm-rate.acct = ENTRY(2,insurr) AND
                       comm-rate.currency = ENTRY(3,insurr) AND
                       comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                       comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                       comm-rate.period = INT64(ENTRY(6,insurr)) AND
                       comm-rate.since = DATE(ENTRY(7,insurr)) 
            NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST comm-rate WHERE
                       comm-rate.filial-id = shfilial AND
                       comm-rate.branch-id = "" AND
                       comm-rate.commission = ENTRY(1,insurr) AND
                       comm-rate.acct = ENTRY(2,insurr) AND
                       comm-rate.currency = ENTRY(3,insurr) AND
                       comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                       comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                       comm-rate.period = INT64(ENTRY(6,insurr)) AND
                       comm-rate.since = DATE(ENTRY(7,insurr)) 
               NO-LOCK NO-ERROR.
            IF AVAIL comm-rate THEN
               insurr = STRING(comm-rate.comm-rate-id).
      END.

                        /* Определеяем, является ли ДР темпорированным. */
      IF CAN-FIND (FIRST ttXattrTemp WHERE ttXattrTemp.fTable   EQ vTable
                                       AND ttXattrTemp.fXattr   EQ inCode)
      THEN DO:          /* ДР - темпорированный.*/
         IF inindexed EQ ?
         THEN DO:
                        /* Определения индексируемости ДР. */
            inindexed = IsXAttrIndexed(infile,   /* Класс объекта. */
                                       incode).  /* Код ДР. */
            IF inindexed EQ ? THEN LEAVE MAIN_BLOCK.    /* ДР с таким кодом не найден. */
         END.

         RUN UpdateTempSignsExDirect(vTable,insurr,incode,indate,inval,inindexed,OUTPUT vFlag).
      END.
      ELSE vFlag = ?.   /* ДР не темпорированный */
   END.
   RETURN vFlag.
END.


/* Создание/редактирование ТЕМПОРИРОВАННОГО ДР на глобальную дату
** Функция возвращает:
**    YES - ДР успешно создан
**    NO  - ДР не обработан
**    ?   - ДР не темпорированный */
FUNCTION UpdateTempSigns RETURN LOG (
   INPUT infile       AS CHAR,   /* Наименование таблицы (код класса). */
   INPUT insurr       AS CHAR,   /* Идентификатор (суррогат) записи в таблице. */
   INPUT incode       AS CHAR,   /* Код ДР. */
   INPUT inval        AS CHAR,  /* Значение ДР. */
   INPUT inindexed    AS LOG    /* Признак индексируемости */
):
   RETURN UpdateTempSignsEx (infile,insurr,incode,(IF gend-date = ? THEN gend-hdate ELSE gend-date),inval,inindexed).
END.


/* Создание / редактирование / удаление значения дополнительного реквизита (ДР).
**
** СОЗДАНИЕ или РЕДАКТИРОВАНИЕ:
** значение ДР должно отличаться от:
** "" (пустая строка) или
** ? (неопределенное значение).
**
** УДАЛЕНИЕ
** значение ДР должно быть:
** либо "" (пустая строка) или
** либо ? (неопределенное значение).
**
** Если вы не можете во внешней процедуре определить индексируемость ДР, и
** передаете в параметр ININDEXED ? (неопределенное значение), то
** INFILE должен содержать КЛАСС ОБЪЕКТА.
** иначе INFILE ДОЛЖЕН содержать имя таблицы!
**
** Сделано это для ускорения, и огромная просьба заранее определять индексируемость
** и имя таблицы, если данная функция должна вызываться в цикле тысячи раз
**
** Возвращаемые значения:
** YES   - ДР обработан успешно.
** NO    - ДР не обработан.
** */
FUNCTION UpdateSigns RETURN LOG (
   INPUT infile       AS CHAR,   /* Наименование таблицы (код класса). */
   INPUT insurr       AS CHAR,   /* Идентификатор (суррогат) записи в таблице. */
   INPUT incode       AS CHAR,   /* Код ДР. */
   INPUT inval        AS CHAR,   /* Значение ДР. */
   INPUT inindexed    AS LOGICAL /* Признак   индексируемый ДР (YES)
                                 **         НЕиндексируемый ДР (NO). */
):
   DEF VAR vFlag     AS LOG    NO-UNDO. /* Результат обработки ДР. */
   DEF VAR vTempFlag AS LOG    NO-UNDO. /* Флаг создания темпорированного ДР */
   DEF VAR vClassLst AS CHAR   NO-UNDO. /* Список классов */
   DEF VAR vi        AS INT64  NO-UNDO. /* Счетчик */
   DEF VAR vClass    AS CHAR   NO-UNDO. /* Код класса, если передан класс, а не имя файла (inindexed = ?) */
   DEF VAR vTmpDate  AS DATE   NO-UNDO. /* Дата создания значения темпорированного ДР */
   DEF VAR vDecVal   AS DEC    NO-UNDO INIT ?. /* Значение типа INTEGER,INT64,DECIMAL */
   DEF VAR vDateVal  AS DATE   NO-UNDO INIT ?. /* Значение типа DATE */
   DEF VAR vMandatory AS LOG   NO-UNDO.

   DEF BUFFER signs     FOR signs.     /* Локализация буфера. */
   DEF BUFFER tmpsigns  FOR tmpsigns.  /* Локализация буфера. */
   DEF BUFFER xattr     FOR xattr.     /* Локализация буфера. */
   DEF BUFFER class     FOR class.     /* Локализация буфера. */
      
   DEFINE VARIABLE vStrCS AS CHARACTER   NO-UNDO CASE-SENSITIVE .
   DEFINE VARIABLE vListCS AS CHARACTER   NO-UNDO.

   IF NOT DataBaseReadOnly() THEN
   DO:
   vListCS = GetHistoryFieldsCS ( infile ). 

   ASSIGN vDecVal  = DEC (inval) WHEN {assigned inval} NO-ERROR.  /* пустое значение не считаем подходящим значение для числовых типов */
   &IF DEFINED (ORACLE) &THEN
   IF vDecVal > 999999999999999.99 THEN vDecVal = ?. /* ограничение формата поля ORACLE */
   &ENDIF
   ASSIGN vDateVal = DATE (inval) NO-ERROR.
                        /* Поиск описания доп.реквизита. На данный момент необходимо
                        ** для определения индексируемости. Поэтому
                        ** следующий блок выполняем только, если индексируемость не
                        ** определена. В остальных
                        ** случаях описание доп.реквизита не требуется. */
   IF    inindexed EQ ?
   THEN DO:
                        /* Если индексируемость ДР не определена (inindexed = ?), то в infile
                        ** должен передаваться код класса, поэтому ищем ДР в этом классе */
      BLK:
      FOR FIRST class WHERE class.Class-code EQ infile NO-LOCK,
      FIRST xattr WHERE xattr.Class-Code EQ class.Class-Code
                    AND xattr.Xattr-Code EQ incode
      NO-LOCK:
         ASSIGN
                        /* Определение имени таблицы. */
            infile    = CLASS.Progress-Code
                        /* Определения индексируемости ДР. */
            inindexed = xattr.Indexed.
         LEAVE BLK.
      END.
                     /* Если не найдено описание класса или реквизита - выход с ошибкой */
      IF NOT AVAIL Xattr THEN RETURN NO.
   END.

   /* старый суррогат "commission,acct,currency,kau,min-value,period,since" меняем на новый "comm-rate-id" */
   DEF BUFFER comm-rate FOR comm-rate.
   IF infile EQ "comm-rate" AND NUM-ENTRIES(insurr) > 3 THEN
   DO:
      IF ENTRY(4,insurr) <> "" THEN
         FIND FIRST comm-rate WHERE
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
         NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST comm-rate WHERE
                    comm-rate.filial-id = shfilial AND
                    comm-rate.branch-id = "" AND
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
            NO-LOCK NO-ERROR.
         IF AVAIL comm-rate THEN
            insurr = STRING(comm-rate.comm-rate-id).
   END.

                        /* Определеяем, является ли ДР темпорированным. */
   IF CAN-FIND (FIRST ttXattrTemp WHERE ttXattrTemp.fTable   EQ infile
                                    AND ttXattrTemp.fXattr   EQ inCode)
   THEN                 /* I. ДР - темпорированный.*/
      UPDSIG:
      DO:
                        /* Опредеяем текущую дату */
      vTmpDate = IF gend-date EQ ? THEN gend-hdate ELSE gend-date.
                        /* Поиск значения темпорированного допреквизита на дату */
      FIND LAST tmpsigns  WHERE tmpsigns.file-name   EQ infile
                            AND tmpsigns.code        EQ incode
                            AND tmpsigns.surrogate   EQ insurr
                            AND tmpsigns.since       LE vTmpDate
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED tmpsigns THEN DO:
         RUN wholocks2.p (RECID(tmpsigns), "tmpsigns", "Запись в tmpsigns заблокированна").
         RETURN ERROR. 
      END.

                        /* Переданное значение равно предыдущему существующему -
                        ** не создаем новую запись */
      IF CAN-DO(vListCS,incode) THEN
      DO:
         vStrCS = inval .
         /* требуется проверка с учетом регистра */
         IF     AVAIL tmpsigns
            AND ((    inindexed
                  AND tmpsigns.code-value EQ vStrCS)
                 OR 
                 (    NOT inindexed
                  AND tmpsigns.xattr-value EQ vStrCS))
         THEN DO:
            RELEASE tmpsigns.
            vFlag = YES.
            LEAVE UPDSIG.
         END.
      END.
      ELSE 
      DO:
         IF     AVAIL tmpsigns
            AND ((    inindexed
                  AND tmpsigns.code-value EQ inval)
                 OR 
                 (    NOT inindexed
                  AND tmpsigns.xattr-value EQ inval))
         THEN DO:
            RELEASE tmpsigns.
            vFlag = YES.
            LEAVE UPDSIG.
         END.
      END.
                        /* Если найденное значение не на требуемую дату - оно нам больше не понадобится */
      IF AVAIL tmpsigns AND tmpsigns.since NE vTmpDate THEN RELEASE tmpsigns.
                        /* если передано ? , то создаем запись с пустым значением ДР */
      IF inval EQ ? THEN inval = "".
      IF     vMandatory
         AND inval EQ ""
      THEN DO:
         RELEASE tmpsigns.
         vFlag = YES.
         LEAVE UPDSIG.
      END.
                        /* Если не найдено существующее значение темпорированного
                        ** ДР на заданную дату, создаем новое, иначе редактируем. */
      IF NOT AVAIL tmpsigns
      THEN DO:
         CREATE tmpsigns.
         ASSIGN
            tmpsigns.file-name   = infile
            tmpsigns.code        = incode
            tmpsigns.surrogate   = insurr
            tmpsigns.since       = vTmpDate
            tmpsigns.code-value  = IF inindexed THEN inval ELSE ""
            tmpsigns.xattr-value = IF inindexed THEN ""    ELSE inval
            tmpsigns.dec-value   = vDecVal
            tmpsigns.date-value  = vDateVal
         NO-ERROR.
      END.
      ELSE DO:
      /* Изменение значение темпорированного ДР */
      ASSIGN
         tmpsigns.code-value  = IF inindexed THEN inval ELSE ""
         tmpsigns.xattr-value = IF inindexed THEN ""    ELSE inval
         tmpsigns.dec-value   = vDecVal
         tmpsigns.date-value  = vDateVal
      NO-ERROR.
         END.
         IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
         RELEASE tmpsigns NO-ERROR.
         IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
      vFlag =  YES.
   END.
   ELSE                 /* II. ДР не темпорированный */
   UPDSIG:
   DO:
                        /* Поиск значения ДР. */
      FIND FIRST signs WHERE
               signs.file-name   EQ infile
         AND   signs.surrogate   EQ insurr
         AND   signs.code        EQ incode
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED signs THEN DO:
         RUN wholocks2.p (RECID(signs), "signs", "Запись в signs заблокированна").
         RETURN ERROR. 
      END.
                        /* Удаление значения ДР. */
      IF    inval EQ ""
         OR inval EQ ? THEN
      DO:
         IF AVAIL signs THEN 
            DELETE signs NO-ERROR.
      END.
   
      /* Создание / редактирование значения ДР. */
      ELSE
      DO:
         /* если допреквизит на объекте не найден, создаем его */
         IF NOT AVAIL signs
         THEN DO:
            CREATE signs.
            ASSIGN
               signs.file-name   = infile
               signs.surrogate   = insurr
               signs.code        = incode
                  signs.code-value  = IF inindexed THEN inval  ELSE ""
                  signs.xattr-value = IF inindexed THEN ""     ELSE inval
                  signs.dec-value   = vDecVal
                  signs.date-value  = vDateVal
               NO-ERROR.
         END.
         ELSE DO:
            /* Если значение не изменилось, выходим, т.к. перезапись может
            ** привести к появлению ненужной истории изменений */
            IF     (inindexed 
               AND signs.code-value  EQ inval)
                OR (NOT inindexed
               AND signs.xattr-value EQ inval)
            THEN DO:
               RELEASE signs.
               vFlag = YES.
               LEAVE UPDSIG.
            END.

         /* записываем значение для созданного или редактируемого допреквизита
         ** и дублируем значение в полях dec-value, date-value */
         ASSIGN
            signs.code-value  = IF inindexed THEN inval  ELSE ""
            signs.xattr-value = IF inindexed THEN ""     ELSE inval
            signs.dec-value   = vDecVal
            signs.date-value  = vDateVal
         NO-ERROR.
         END.
         IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
            RELEASE signs NO-ERROR.
            IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
      END.
      vFlag =  YES.
   END.
   END.

   RETURN vFlag.
END.

/* Создание / редактирование / удаление значения дополнительного реквизита (ДР).
**
** СОЗДАНИЕ или РЕДАКТИРОВАНИЕ:
** значение ДР должно отличаться от:
** "" (пустая строка) или
** ? (неопределенное значение).
**
** УДАЛЕНИЕ
** значение ДР должно быть:
** либо "" (пустая строка) или
** либо ? (неопределенное значение).
**
** Данная функция осуществляет все проверки, и поэтому выполняется медленно и
** ее нельзя вызывать многократно по одниму и тому же реквизиту!
** Если она будет вызываться в цикле, то необходимо сначала определить имя
** таблицы и индексируемость, а затем с этими параметрами вызывать UpdateSigns
**
** Возвращаемые значения:
** YES   - ДР обработан успешно.
** NO    - ДР не обработан.
** */
FUNCTION UpdateSignsEx RETURN LOG (
   INPUT infile       AS CHAR,   /* Код класса */
   INPUT insurr       AS CHAR,   /* Идентификатор (суррогат) записи в таблице. */
   INPUT incode       AS CHAR,   /* Код ДР. */
   INPUT inval        AS CHAR    /* Значение ДР. */
):

   RETURN UpdateSigns(infile,insurr,incode,inval,?).
END.


/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION CreateSigns RETURN log
        (INPUT infile       AS CHAR,      /* Наименование таблицы (код класса). */
         INPUT insurr       AS CHAR,      /* Идентификатор (суррогат) записи в таблице. */
         INPUT incode       AS CHAR,      /* Код ДР. */
         INPUT inval        AS CHAR,      /* Значение ДР. */
         INPUT inindexed    AS LOGICAL):

   RETURN UpdateSigns (infile,   
                       insurr,   
                       incode,   
                       inval,   
                       inindexed).
END.


/* удаляет все допреквизиты объекта */
PROCEDURE DelSigns.
   DEF INPUT PARAM infile       AS CHAR NO-UNDO.
   DEF INPUT PARAM insurr       AS CHAR NO-UNDO.

   DEFINE BUFFER signs     FOR signs.     /* Локализация буфера. */
   DEFINE BUFFER tmpsigns  FOR tmpsigns.  /* Локализация буфера. */

   IF DataBaseReadOnly() THEN
      LEAVE.
   /* удаление нетемпорированных значений допреквизитов */
   FOR EACH signs WHERE signs.file-name EQ infile
                    AND signs.surrogate EQ insurr
                  EXCLUSIVE-LOCK:
      DELETE signs.
   END.
   /* удаление темпорированных значений допреквизитов */
   FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ infile
                       AND tmpsigns.surrogate   EQ insurr
   EXCLUSIVE-LOCK:
      DELETE tmpsigns.
   END.
END.

/* удаляет все допреквизиты по параметру классификатора */
PROCEDURE DelSignsCode.
   DEF INPUT PARAM incode       AS CHAR NO-UNDO.
   DEF INPUT PARAM inval        AS CHAR NO-UNDO.
   DEF VAR infile AS CHAR NO-UNDO.

   DEFINE BUFFER signs     FOR signs.     /* Локализация буфера. */
   DEFINE BUFFER xattr     FOR xattr.     /* Локализация буфера. */
   DEFINE BUFFER tmpsigns  FOR tmpsigns.  /* Локализация буфера. */

   FOR EACH xattr WHERE xattr.domain-code = incode
                    AND xattr.indexed NO-LOCK:
      infile = GetXclassFile (xattr.class-code).
      IF infile <> ?
      THEN DO:
         /* удаление значений нетемпорированных допреквизитов */
         FOR EACH signs WHERE signs.file-name   EQ infile
                          AND signs.code        EQ incode
                          AND signs.code-value  EQ inval
         EXCLUSIVE-LOCK:
            DELETE signs.
         END.
         /* удаление значений темпорированных допреквизитов */
         FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ infile
                             AND tmpsigns.code        EQ incode
                             AND tmpsigns.code-value  EQ inval
         EXCLUSIVE-LOCK:
            DELETE tmpsigns.
         END.
      END.
   END.
END.

/* удаляет все допреквизиты по классификатору */
PROCEDURE DelSignsAllCode.
   DEF INPUT PARAM incode       AS CHAR NO-UNDO.
   DEF VAR infile AS CHAR NO-UNDO.

   DEFINE BUFFER signs     FOR signs.     /* Локализация буфера. */
   DEFINE BUFFER xattr     FOR xattr.     /* Локализация буфера. */
   DEFINE BUFFER tmpsigns  FOR tmpsigns.  /* Локализация буфера. */

   FOR EACH xattr WHERE xattr.domain-code = incode NO-LOCK:
      infile = GetXclassFile (xattr.class-code).
      IF infile <> ?
      THEN DO:
         /* удаление значений нетемпорированных допреквизитов */
         FOR EACH signs WHERE signs.file-name EQ infile AND
                              signs.code      EQ incode
         EXCLUSIVE-LOCK:
            DELETE signs.
         END.
         /* удаление значений темпорированных допреквизитов */
         FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ infile
                             AND tmpsigns.code        EQ incode
         EXCLUSIVE-LOCK:
            DELETE tmpsigns.
         END.
      END.
   END.
END.

/* проверяет есть ли допреквизит по классификатору */
FUNCTION AvailSignsAnyCode RETURN LOGICAL
   (INPUT incode AS CHAR):
   DEF VAR infile AS CHAR NO-UNDO.

   DEFINE BUFFER xattr FOR xattr. /* Локализация буфера. */

   FOR EACH xattr WHERE xattr.domain-code = incode NO-LOCK:
      infile = GetXclassFile (xattr.class-code).
      IF infile <> ? AND AvailAnySigns(infile, incode) THEN
         RETURN YES.
   END.
   RETURN NO.
END.

/* проверяет есть ли допреквизит по параметру классификатора */
FUNCTION AvailSignsCode RETURN LOGICAL (
   INPUT incode   AS CHAR, /* Код классификатора*/
   INPUT inval    AS CHAR  /* Значение параметра классификатора */
):
   DEF VAR infile AS CHAR NO-UNDO. /* имя файла (таблицы),
                                     которой принадлежит данный класс.*/

   DEFINE BUFFER xattr FOR xattr.  /* Локализация буфера */

   FOR EACH xattr WHERE
      xattr.domain-code EQ incode
   NO-LOCK: 
      infile = GetXclassFile (xattr.class-code).  
      IF     infile <> ?
         AND (CAN-FIND(FIRST signs WHERE signs.file-name   EQ infile
                                     AND signs.code        EQ xattr.Xattr-Code
                                     AND signs.code-value  EQ inval)
         OR   CAN-FIND(LAST  tmpsigns WHERE tmpsigns.file-name    EQ infile
                                        AND tmpsigns.code         EQ xattr.Xattr-Code
                                        AND tmpsigns.since        LE gend-date
                                        AND tmpsigns.code-value   EQ inval))
      THEN RETURN YES.
   END.
   RETURN NO.
END.

FUNCTION DelAllSigns RETURN log
        (INPUT infile       AS CHAR,
         INPUT insurr       AS CHAR
         ):
  RUN DelSigns(infile,insurr).
  FIND FIRST signs WHERE signs.file-name EQ infile AND
                         signs.surrogate EQ insurr NO-LOCK NO-ERROR.
  IF NOT AVAIL signs THEN RETURN YES.
  ELSE RETURN NO .

END FUNCTION .

PROCEDURE FindSignsByVal.
  DEFINE INPUT PARAM i-file-list   AS CHAR NO-UNDO. /* Список возможных файлов */
  DEFINE INPUT PARAM i-code        AS CHAR NO-UNDO.
  DEFINE INPUT PARAM i-code-val    AS CHAR NO-UNDO.
  DEFINE OUTPUT PARAM o-file-order AS INT64 NO-UNDO INIT 0. /* Порядковый номер в списке */
  DEFINE OUTPUT PARAM o-surrogate  AS CHAR NO-UNDO INIT "".

  DEFINE VARIABLE i AS INT64 NO-UNDO.

  block-1:
  DO i = 1 TO NUM-ENTRIES(i-file-list):
     FOR FIRST signs
         WHERE signs.file-name = ENTRY(i,i-file-list)
           AND signs.code      = i-code
           AND signs.code-val  = i-code-val NO-LOCK:
              ASSIGN o-file-order = i
                     o-surrogate  = signs.surrogate.
              LEAVE block-1.
     END.

     FOR LAST tmpsigns WHERE tmpsigns.file-name    EQ ENTRY(i,i-file-list)
                         AND tmpsigns.code         EQ i-code
                         AND tmpsigns.since        LE gend-date
                         AND tmpsigns.code-value   EQ i-code-val
     NO-LOCK:
        ASSIGN o-file-order = i
               o-surrogate  = tmpsigns.surrogate.
        LEAVE block-1.
     END.
  END.

END PROCEDURE.

/* Удаляет со всех объектов выбранной таблицы (ipTableChar)
** заданное значение (ipValueChar) дополнительного реквизита (ipXattrChar). */
PROCEDURE DelSignsCodeVal.

   DEF INPUT PARAM ipTableChar AS CHAR NO-UNDO. /* Нименование таблицы. */
   DEF INPUT PARAM ipXattrChar AS CHAR NO-UNDO. /* Код ДР. */
   DEF INPUT PARAM ipValueChar AS CHAR NO-UNDO. /* Значение ДР. */

   DEF BUFFER signs     FOR signs.     /* Локализация буфера. */
   DEF BUFFER tmpsigns  FOR tmpsigns.  /* Локализация буфера. */

   /* Перебор всех значений нетемпорированных ДР по таблице. */
   FOR EACH signs WHERE
      signs.file-name  EQ ipTableChar AND
      signs.code       EQ ipXattrChar
   EXCLUSIVE-LOCK:
      /* Проверка значения. */
      IF ipValueChar EQ (IF signs.code-val NE ''
                         THEN signs.code-val
                         ELSE signs.xattr-val)
      THEN DELETE signs.
   END.
   /* удаление темпорированных значений допреквизитов */
   FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ ipTableChar
                       AND tmpsigns.code        EQ ipXattrChar
   EXCLUSIVE-LOCK:
      /* Проверка значения. */
      IF ipValueChar EQ (IF tmpsigns.code-value NE ''
                         THEN tmpsigns.code-value
                         ELSE tmpsigns.xattr-value)
      THEN DELETE tmpsigns.
   END.

   RETURN.

END PROCEDURE.

PROCEDURE CopySigns:
   /*  Класс и суррогат объекта-источника */
   DEF INPUT PARAMETER iSrcClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iSrcSurr  AS CHAR NO-UNDO.
   /*  Класс и суррогат объекта-приемника */
   DEF INPUT PARAMETER iTrgClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iTrgSurr  AS CHAR NO-UNDO.
   
   RUN CopySignsEx(iSrcClass,iSrcSurr,iTrgClass,iTrgSurr,"*","!*").
END PROCEDURE.

/* Копирование доп.реквизитов с одного объекта на другой.
** Значения уже существующих реквизитов обновляются!
** Пока запрещено копировать реквизиты на объект другого класса!
*/
PROCEDURE CopySignsEx:
   /*  Класс и суррогат объекта-источника */
   DEF INPUT PARAMETER iSrcClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iSrcSurr  AS CHAR NO-UNDO.
   /*  Класс и суррогат объекта-приемника */
   DEF INPUT PARAMETER iTrgClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iTrgSurr  AS CHAR NO-UNDO.
   /* Commented by KSV: Маска реквизитов, значения которых копировать */
   DEFINE INPUT  PARAMETER iInclList AS CHARACTER  NO-UNDO.
   /* Commented by KSV: Маска реквизитов, значения которых НЕ копировать */
   DEFINE INPUT  PARAMETER iExclList AS CHARACTER  NO-UNDO.

   /*  Таблица объекта-источника */
   DEF VAR   vSrcFileName   AS CHAR      NO-UNDO.
   /*  Таблица объекта-приемника */
   DEF VAR   vTrgFileName   AS CHAR      NO-UNDO.
   /*  Реквизит индексированный? */
   DEF VAR   vIndexed       AS LOGICAL   NO-UNDO.

   DEF BUFFER signs     FOR signs.
   DEF BUFFER tmpsigns  FOR tmpsigns.
   DEF BUFFER btmpsigns FOR tmpsigns.

   ASSIGN
      vSrcFileName = GetXclassFile (iSrcClass)
      vTrgFileName = GetXclassFile (iTrgClass).

   /* Если по указанному классу не удалось определить имя таблицы,
   ** то выходим без ошибки (возможно, класс не описан в метасхеме и,
   ** следовательно, не должен иметь доп.реквизитов).
   */
   IF vSrcFileName = ? OR
      vTrgFileName = ? THEN
      RETURN.

   /* Копировать реквизиты на объекты другой таблицы нельзя! */
   IF vSrcFileName <> vTrgFileName THEN
      RETURN ERROR.

   FOR EACH signs WHERE
            signs.file-name = vSrcFileName
        AND signs.surrogate = iSrcSurr
      NO-LOCK:

      IF NOT CAN-DO(iInclList,signs.code) OR
             CAN-DO(iExclList,signs.code) THEN NEXT.
      
      vIndexed = isXAttrIndexed(iTrgClass, signs.code).
      /* Если isXAttrIndexed вернула ?,
      ** значит такого реквизита у класса нет.
      */
      IF vIndexed <> ? THEN
         UpdateSigns (vTrgFileName,
                      iTrgSurr,
                      signs.code,
                      (IF isXAttrIndexed(iSrcClass, signs.code)
                       THEN signs.code-value
                       ELSE signs.xattr-value),
                      vIndexed
                     ).
   END.

   /* копирование темпорированных ДР */
   FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ vSrcFileName
                       AND tmpsigns.surrogate   EQ iSrcSurr
   NO-LOCK,
   LAST btmpsigns WHERE btmpsigns.file-name  EQ vSrcFileName
                    AND btmpsigns.code       EQ tmpsigns.code
                    AND btmpsigns.surrogate  EQ iSrcSurr
                    AND btmpsigns.since      LE gend-date
                    AND (btmpsigns.code-value  NE ""
                     OR  btmpsigns.xattr-value NE "")
   NO-LOCK,
   FIRST symbol WHERE ROWID(btmpsigns) EQ ROWID(tmpsigns)
   NO-LOCK:
      /* фильтрация по списку копируемых и исключаемых ДР */
      IF NOT CAN-DO(iInclList,btmpsigns.code) OR
             CAN-DO(iExclList,btmpsigns.code) THEN NEXT.
      /* определение признака индексированности */
      vIndexed = isXAttrIndexed(iTrgClass, btmpsigns.code).
      /* Если isXAttrIndexed вернула ?,
      ** значит такого реквизита у класса нет. */
      IF vIndexed <> ? THEN
         UpdateSigns (vTrgFileName,
                      iTrgSurr,
                      btmpsigns.code,
                      (IF isXAttrIndexed(iSrcClass, btmpsigns.code)
                       THEN btmpsigns.code-value
                       ELSE btmpsigns.xattr-value),
                      vIndexed).
   END.

   RETURN.
END PROCEDURE.

PROCEDURE GetXAttr.
/* возвращает буффер с указанным реквизитом класса, */

   DEF INPUT PARAM io-class LIKE class.class-code NO-UNDO.
   DEF INPUT PARAM io-xattr-code LIKE xattr.xattr-code NO-UNDO.
   DEF PARAM BUFFER xattr FOR xattr.

   FIND FIRST xattr WHERE
              xattr.class-code = io-class AND
              xattr.xattr-code = io-xattr-code
              NO-LOCK NO-ERROR.

END.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает коды классификатора в виде списка
  Parameters:  iClass   - класс классификатора
               iParent  - ссылка на код родителя
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetCodeList RETURNS CHARACTER (iClass  AS CHARACTER,
                                        iParent AS CHARACTER):
   DEFINE VARIABLE vList AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bCode FOR CODE.

   IF {assigned iParent} THEN
      FOR EACH bCode WHERE
               bCode.class    EQ iClass
           AND bCode.parent   EQ iParent
               NO-LOCK:
         {additem.i vList bCode.code}
      END.                                                      /* END OF FOR */
   ELSE
      FOR EACH bCode WHERE
               bCode.class    EQ iClass
               NO-LOCK:
         {additem.i vList bCode.code}
      END.                                                      /* END OF FOR */

   RETURN vList.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает код классификатора по значению
  Parameters:  iClass   - класс классификатора
               iVal  - значение
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetCodeVal RETURN CHAR
        (INPUT iClass AS CHAR,
         INPUT iVal AS CHAR):

   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ iClass AND
                         code.val EQ iVal NO-LOCK NO-ERROR.
   RETURN IF NOT AVAIL code THEN ? ELSE code.CODE.
END.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает коды классификатора по значению в виде списка
  Parameters:  iClass   - класс классификатора
               iVal  - значение
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetCodeValList RETURN CHAR
        (INPUT iClass AS CHAR,
         INPUT iVal AS CHAR):

   DEFINE VARIABLE vList AS CHARACTER  NO-UNDO.
   
   DEFINE BUFFER bCode FOR CODE.
   
   FOR EACH bcode WHERE 
            bcode.class EQ iClass AND
            bcode.val EQ iVal 
      NO-LOCK:
      {additem.i vList bCode.code}
   END.

   RETURN vList.
END.

/*Функция для поиска максимального значения доп реквизита,
объявленного на нескольких классах*/
FUNCTION GetMaxSigns RETURNS CHARACTER (ipFileLstChar AS CHARACTER,  /*Список классов*/
                                        ipCodeChar    AS CHARACTER,  /*Код реквизита*/
                                        ipBegValStr   AS CHARACTER   /*Начальное значение для поиска*/
                                        ):

   DEFINE VAR vIndInt            AS INT64   NO-UNDO.
   DEFINE VAR vMaxStr            AS CHARACTER NO-UNDO.
   DEFINE BUFFER buf_signs    FOR signs.
   DEFINE BUFFER tmpsigns     FOR tmpsigns.
   DO vIndInt = 1 TO NUM-ENTRIES(ipFileLstChar):
      /* Определеяем, является ли ДР темпорированным. */
      FIND FIRST ttXattrTemp WHERE
               ttXattrTemp.fTable   EQ ENTRY(vIndInt,ipFileLstChar)
         AND   ttXattrTemp.fXattr   EQ ipCodeChar
      NO-ERROR.
      /* ДР - не темпорированный.*/
      IF NOT AVAIL ttXattrTemp
      THEN DO:
         FIND LAST buf_signs WHERE buf_signs.file-name EQ     ENTRY(vIndInt,ipFileLstChar)
                               AND buf_signs.code      EQ     ipCodeChar
                               AND buf_signs.code-val  BEGINS ipBegValStr
                               AND buf_signs.code-val  GE     vMaxStr
                                                                 NO-LOCK NO-ERROR.
         IF AVAIL buf_signs THEN vMaxStr = buf_signs.code-val.
      END.
      ELSE DO:
         FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ     ENTRY(vIndInt,ipFileLstChar)
                              AND tmpsigns.code       EQ     ipCodeChar
                              AND tmpsigns.since      LE     gend-date
                              AND tmpsigns.code-value BEGINS ipBegValStr
                              AND tmpsigns.code-value GE     vMaxStr
         NO-LOCK NO-ERROR.
         IF AVAIL tmpsigns THEN vMaxStr = tmpsigns.code-value.
      END.
   END.
   RETURN vMaxStr.
END FUNCTION.

/* Получить уровень иерархии параметра классификатора */
FUNCTION GetCodeLevel RETURNS INT64
   (iClass  AS CHAR,
    iCode   AS CHAR
   ):

   DEF BUFFER code FOR code.

   DEF VAR vLevel  AS INT64   NO-UNDO.
   DEF VAR vParent AS CHAR  NO-UNDO.

   FIND FIRST code    WHERE
              code.class EQ iClass
          AND code.code  EQ iCode
   NO-LOCK NO-ERROR.

   DO WHILE AVAILABLE code:
      ASSIGN
         vParent = code.parent
         vLevel  = vLevel + 1
      .

      IF code.parent EQ iClass THEN
         RELEASE code.
      ELSE
         FIND FIRST code WHERE
                    code.class  EQ iClass
                AND code.code   EQ vParent
         NO-LOCK NO-ERROR.
   END.

   RETURN vLevel.

END FUNCTION.

/* Список всех подпараметров указанного параметра классификатора */
FUNCTION GetCodeChildren RETURNS CHAR
   (iClass AS CHAR,
    iCode  AS CHAR
   ):

   DEF VAR vList  AS CHAR  NO-UNDO.

   DEF BUFFER code  FOR code.

   IF GetCode(iClass, iCode) NE ? THEN
   DO:
      vList = iCode.

      FOR EACH code     WHERE
               code.class  EQ iClass
           AND code.parent EQ iCode
         NO-LOCK:

         vList = vList + "," + GetCodeChildren(code.class,
                                               code.code
                                              ).
      END.
   END.

   RETURN vList.
END FUNCTION.

/* Список всех подпараметров по указанному списку параметров классификатора */
FUNCTION List-Item-Of-Code RETURNS CHAR
   (iClass AS CHAR,
    iCodes AS CHAR
   ):

   DEF VAR vItem     AS CHAR  NO-UNDO.
   DEF VAR vList     AS CHAR  NO-UNDO.
   DEF VAR vRetList  AS CHAR  NO-UNDO.

   DEF BUFFER code  FOR code.

   DEF VAR vI     AS INT64   NO-UNDO.
   DEF VAR vJ     AS INT64   NO-UNDO.

   DO vI = 1 TO NUM-ENTRIES(iCodes):

      vList = GetCodeChildren (iClass,
                               ENTRY(vI, iCodes)
                              ).
      DO vJ = 1 TO NUM-ENTRIES(vList):

         vItem = ENTRY(vJ, vList).

         IF NOT CAN-DO(vRetList, vItem) THEN
            {additem.i vRetList vItem}
      END.
   END.

   RETURN vRetList.
END FUNCTION.


&GLOB MethodSignsAlredyDefined YES
&ENDIF

/*----------------------------------------------------------------------------*/
/* Возвращает всех потомков для определенного кода классификатора без родителя*/
/*----------------------------------------------------------------------------*/

FUNCTION GetOnlyChildCode RETURNS CHARACTER ( INPUT iClass AS CHARACTER,
                                              INPUT iCode  AS CHARACTER):
   DEFINE VARIABLE mI            AS INT64     NO-UNDO.
   DEFINE VARIABLE mParentChild  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mChild        AS CHARACTER   NO-UNDO.

   mParentChild = GetCodeChildren(iClass,iCode).
   IF NUM-ENTRIES(mParentChild) > 1 THEN
   DO:
      mChild = SUBSTRING(mParentChild,INDEX(mParentChild,",") + 1).
   END.
   RETURN mChild.
END FUNCTION.


/* Актуализация значения темпорированного реквизита */
PROCEDURE SetValueTempAttr.
   DEFINE INPUT  PARAMETER iFile AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iSurr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE      NO-UNDO.

   DEFINE VARIABLE vClassObj AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vValue    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBuffer   AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vXattrSgn AS LOGICAL   NO-UNDO.
   
   IF iDate EQ ?  THEN
      iDate = gend-date.

   FIND FIRST ttXattrTemp WHERE
              ttXattrTemp.fTable   EQ iFile
         AND  ttXattrTemp.fXattr   EQ iCode NO-LOCK NO-ERROR.

   IF AVAIL ttXattrTemp THEN DO:
      FIND FIRST Xattr WHERE
                 xattr.Class-Code EQ iFile
             AND xattr.Xattr-Code EQ iCode NO-LOCK NO-ERROR.
      IF    NOT AVAIL Xattr
         OR NOT Xattr.Progress-Field THEN
         RETURN.
      
      CREATE BUFFER vBuffer FOR TABLE iFile NO-ERROR.
      IF NOT VALID-HANDLE(vBuffer) THEN RETURN ERROR "Ошибка создания буфера объекта!".
      vBuffer:FIND-FIRST("WHERE " + GetWhereSurr(iFile,iSurr),NO-LOCK) NO-ERROR.
      IF NOT vBuffer:AVAIL THEN DO:
         DELETE OBJECT vBuffer.
         RETURN ERROR "Ошибка поиска записи!".
      END.
      
      vClassObj = GetClassObj(vBuffer).
      IF NOT {assigned vClassObj} THEN DO:
         DELETE OBJECT vBuffer.
         RETURN ERROR "Не найден класс объекта!".
      END.

      FIND LAST tmpsigns WHERE 
                tmpsigns.file-name  EQ iFile
            AND tmpsigns.code       EQ iCode
            AND tmpsigns.surrogate  EQ iSurr
            AND tmpsigns.since      LE iDate
      NO-LOCK NO-ERROR.

      vValue = IF AVAIL tmpsigns 
               THEN (IF tmpsigns.code-value NE "" 
                     THEN tmpsigns.code-value 
                     ELSE tmpsigns.xattr-value)
               ELSE "".

      RUN CheckFullFieldValue (vClassObj,
                               iCode,
                               iSurr, 
                               vValue       
                               ).
      IF RETURN-VALUE NE "" THEN DO:
         DELETE OBJECT vBuffer.
         RETURN ERROR RETURN-VALUE.
      END.

      TR_BL:
      DO TRANS ON ERROR UNDO TR_BL,LEAVE TR_BL:
         IF ENTRY(1,PutValueByQuery(iFile,
                                    iCode,
                                    vValue,
                                    "ROWID(" + iFile + ") EQ TO-ROWID('" +
                                     STRING(vBuffer:ROWID) + "')"
                                    )) NE '1' THEN DO:
            DELETE OBJECT vBuffer.
            RETURN ERROR RETURN-VALUE.
         END.
      END.
   END.

END PROCEDURE.

/* Возвращает "Да" если хотя бы один из реквизитов iAttrList был изменён **
** в период с iBegDate до iEndDate включительно                          */

FUNCTION ChkChangedAttrs RETURNS LOGICAL (INPUT iFileName  AS CHARACTER,
                                          INPUT iSurrogate AS CHARACTER,
                                          INPUT iBegDate   AS DATE, 
                                          INPUT iEndDate   AS DATE, 
                                          INPUT iAttrList  AS CHARACTER):

   DEFINE VARIABLE i       AS INT64     NO-UNDO.
   DEFINE VARIABLE vAttr   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vReturn AS LOGICAL     NO-UNDO.

   DO i = 1 TO NUM-ENTRIES(iAttrList):
      vAttr = ENTRY(i,iAttrList).
      IF CAN-FIND(FIRST tmpsigns WHERE
                        tmpsigns.file-name  EQ iFileName
                    AND tmpsigns.surrogate  EQ iSurrogate
                    AND tmpsigns.code       EQ vAttr
                    AND (    tmpsigns.since >= iBegDate
                         AND tmpsigns.since <= iEndDate)) THEN
      DO:
         vReturn = YES.
         LEAVE.
      END.
   END.
   RETURN vReturn.

END FUNCTION.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/07/2015 18:40:48.249+04:00' */
/* $LINTUSER='ariz' */
/* $LINTMODE='1' */
/* $LINTFILE='signs.fun' */
/*prosign+iOhWi3X+7PGwbZBlJYbhA*/