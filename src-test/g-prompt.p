/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: G-PROMPT.P
      Comment: (0033031) Создает экран с полями ввода
               для запроса информации у пользователя
               и возвращает спсиок введенных
               значений.
   Parameters: iDataTypes - список типов данных полей ввода (обязательно) 
               iLabels    - список меток полей ввода
               iFormats   - список форматов полей ввода
               iInitials  - список начальных значений полей ввода
               iTitle     - заголовок фрейма
               iWidth     - ширина фрейма
               iDelimiter - разделитель для ВСЕХ списков (по умолчанию - ',')
               iProcs     - список процедур обработчиков полей ввода.
                            Допустима обработка нажатия F1 в поле ввода и 
                            обработка выхода из поля. Формат задания процедур 
                            обработки следующий: [F1=<procname>[;LEAVE=<procname>]].
                            Процедуры обработчики должны объявить один входной
                            параметр, тип которого совпадает с типом поля ввода.
                            Подробнее работу обработчиков см в описаниях 
                            процедур ON_LEAVE и ON_F1
               iExtended1 - зарезервированный параметр
               iExtended2 - зарезервированный параметр
               oValues    - список значений, введенных в поля ввода 
         Uses:
      Used by:
      Created: 16.07.2004 09:14 KSV     
     Modified: 20.07.2004 16:22 KSV      (0033031) Создает экран с полями ввода
                                         для запроса информации у пользователя
                                         и возвращает спсиок введенных
                                         значений.
     Modified: 15.06.2006  Om Ошибка.
                        Скорректирована процедура формирования значения GetData.
     Modified: 06.07.2007 13:21 KSV      (0078824) Адаптация под Биссмарт
     Modified: 21.07.2010 16:18 BIS      <comment>
*/


/* ***************************  Parameters  *************************** */
DEFINE INPUT  PARAMETER iDataTypes  AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iLabels     AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iFormats    AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iInitials   AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iTitle      AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iWidth      AS INT64           NO-UNDO.
DEFINE INPUT  PARAMETER iDelimiter  AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iProcs      AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iExtended1  AS CHARACTER         NO-UNDO.
DEFINE INPUT  PARAMETER iExtended2  AS CHARACTER         NO-UNDO.
DEFINE OUTPUT PARAMETER oValues     AS CHARACTER  INIT ? .
                                                   
/* ************************  Global Definitions  ********************** */
{globals.i}

/* Commented by KSV: Создание виджет пула гарантирует нам корректное
** удаление всех динамических виджетов после завершения процедуры */
CREATE WIDGET-POOL.
/* ************************  Local Definitions  *********************** */
DEFINE VARIABLE mHFrame       AS HANDLE     NO-UNDO.
DEFINE VARIABLE mNumObjects   AS INT64    NO-UNDO.
DEFINE VARIABLE mI            AS INT64    NO-UNDO.
DEFINE VARIABLE mJ            AS INT64    NO-UNDO.
DEFINE VARIABLE mMaxLabel     AS INT64    NO-UNDO.
DEFINE VARIABLE mHLabel       AS HANDLE     NO-UNDO.
DEFINE VARIABLE mProc         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mEvent        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mProcName     AS CHARACTER  NO-UNDO.

/* Commented by KSV: Временная таблица, описывающая структуру фрейма с полями
** ввода */
DEFINE TEMP-TABLE tObject NO-UNDO
   FIELD fId     AS INT64      /* Уникальный идентифкатор виджета */
   FIELD fObject AS HANDLE       /* Хэндл виджета */
   FIELD fF1     AS CHARACTER    /* Обработчик F1 */
   FIELD fLeave  AS CHARACTER    /* Обработчик LEAVE */
   INDEX iID IS PRIMARY fId
   INDEX iObject fObject.

/* *****************************  Frames  ***************************** */



/* ****************************  Functions  *************************** */



/* ****************************  Triggers  **************************** */



/* ***************************  Main Block  *************************** */
MAIN-BLOCK:
DO 
   ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* Commented by KSV: Обрабатываем необяательные параметры */
   IF NOT {assigned iDelimiter} THEN iDelimiter = ",".
   mNumObjects = NUM-ENTRIES(iDataTypes,iDelimiter).

   IF mNumObjects > 0 THEN. ELSE UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

   IF NOT {assigned iLabels}   THEN iLabels   = FILL(iDelimiter,mNumObjects - 1).
   IF NOT {assigned iFormats}  THEN iFormats  = FILL(iDelimiter,mNumObjects - 1).
   IF NOT {assigned iInitials} THEN iInitials = "".
   
   IF iWidth = ? OR iWidth > 78 OR iWidth = 0 THEN iWidth = 50.
   
   IF NOT {assigned iTitle} THEN iTitle = "[ ВВЕДИТЕ ДАННЫЕ ]".

   DO mI = 1 TO mNumObjects:
      mMaxLabel = (IF LENGTH(ENTRY(mI,iLabels,iDelimiter)) > mMaxLabel 
                   THEN LENGTH(ENTRY(mI,iLabels,iDelimiter))
                   ELSE mMaxLabel).
      IF mMaxLabel > 30 THEN
      DO:
         mMaxLabel = 30.
         ENTRY(mI,iLabels,iDelimiter) = SUBSTR(ENTRY(mI,iLabels,iDelimiter),1,mMaxLabel).
      END.
   END.
   
   /* Commented by KSV: Создаем фрейм */
   CREATE FRAME mHFrame
      ASSIGN
         NAME        = "fPrompt"
         ROW         = (25 - mNumObjects - 2) / 2
         COLUMN      = 1
         WIDTH       = iWidth
         HEIGHT      = mNumObjects + 2
         SENSITIVE   = YES
         OVERLAY     = YES
         BOX         = YES
         SIDE-LABELS = YES
         SCROLLABLE  = NO
         VISIBLE     = NO
         CENTERED    = YES
         TITLE       = iTitle 
         DCOLOR      = 2
      TRIGGERS:
         ON GO PERSISTENT RUN GetData.
      END
      .
   
   /* Commented by KSV: Создаем поля ввода и метки, если нужны */
   DO mI = 1 TO mNumObjects:
      CREATE tObject.
      tObject.fId = mI.

      /* Commented by KSV: Есть метка - создаем */
      IF {assigned ENTRY(mI,iLabels,iDelimiter)} THEN
      DO:
         CREATE TEXT mHLabel
            ASSIGN
               FRAME          = mHFrame
               ROW            = mI
               COLUMN         = mMaxLabel - LENGTH(ENTRY(mI,iLabels,iDelimiter)) + 2
               WIDTH          = LENGTH(ENTRY(mI,iLabels,iDelimiter)) + 1
               FORMAT         = "x(" + STRING(mMaxLabel + 1) + ")" 
               SCREEN-VALUE   = ENTRY(mI,iLabels,iDelimiter) + ":"
               .
      END.
      ELSE 
         mHLabel = ?.
      
      /* Commented by KSV: Создаем поля ввода */
      CREATE FILL-IN tObject.fObject
         ASSIGN
            FRAME          = mHFrame
            DATA-TYPE      = ENTRY(mI,iDataTypes,iDelimiter)
            ROW            = mI
            COLUMN         = mMaxLabel + 4
            WIDTH          = iWidth - mMaxLabel - 6
            SENSITIVE      = YES
            VISIBLE        = YES
         TRIGGERS:
            ON F1    PERSISTENT RUN ON_F1    (mI).
            ON LEAVE PERSISTENT RUN ON_LEAVE (mI).
         END.

      IF VALID-HANDLE(mHLabel) THEN
         tObject.fObject:SIDE-LABEL-HANDLE = mHLabel.

      IF {assigned ENTRY(mI,iFormats,iDelimiter)} THEN
         tObject.fObject:FORMAT = ENTRY(mI,iFormats,iDelimiter).
      

      IF NUM-ENTRIES(iInitials,iDelimiter) >= mI THEN
         tObject.fObject:SCREEN-VALUE = ENTRY(mI,iInitials,iDelimiter).
      
      /* Commented by KSV: Устанавливаем обработчики событий */
      IF NUM-ENTRIES(iProcs,iDelimiter) >= mI THEN
      DO:
         mProc = ENTRY(mI,iProcs,iDelimiter).
         DO mJ = 1 TO NUM-ENTRIES(mProc,";"):
            mEvent = ENTRY(mJ,mProc,";").
            mProcName = ENTRY(2,mEvent,"=") NO-ERROR.
            IF ERROR-STATUS:ERROR THEN NEXT.
            mEvent = ENTRY(1,mEvent,"=").
            CASE mEvent:
               WHEN "F1"      THEN tObject.fF1     = mProcName.
               WHEN "LEAVE"   THEN tObject.fLeave  = mProcName.
            END CASE.
         END.
      END.

      IF  tObject.fObject:DATA-TYPE = "date"
      AND NOT {assigned tObject.fF1} THEN
         tObject.fF1 = "calend".
   END.

   FIND FIRST tObject WHERE NO-ERROR.

   mHFrame:VISIBLE = YES.

   /* WAIT-FOR GO OF mHFrame FOCUS tObject.fObject. */

   {wait_for.i 
      &WIDGET_HANDLE = "mHFrame" 
      &EXTEXT        = "GO OF mHFrame FOCUS tObject.fObject" {&*}} .
         
   
END. /* MAIN-BLOCK: */

mHFrame:VISIBLE = NO.


/* ***************************  Procedures  *************************** */
/*------------------------------------------------------------------------------
  Purpose:     Выполняет проверку данных, введенных в поля и сохраняет значения
               полей в списке oValues
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE GetData:
   DEF VAR vDelimiter AS CHAR   NO-UNDO. /* Разделитель. */
   DEF VAR vTmpValue  AS CHAR   NO-UNDO. /* Разделитель. */
   DEFINE BUFFER tObject FOR tObject.

   ASSIGN
      oValues     = ""
      vDelimiter  = ""
   .
   
   FOR EACH tObject:
      RUN ON_LEAVE(tObject.fID).
      IF RETURN-VALUE = "ERROR" THEN  RETURN NO-APPLY.

      IF NUM-ENTRIES(tObject.fObject:INPUT-VALUE,"|") GT 2 
         AND ENTRY(2,tObject.fObject:INPUT-VALUE,"|") EQ "CP1251"
      THEN 
      DO:
         vTmpValue = TRIM(CODEPAGE-CONVERT(tObject.fObject:INPUT-VALUE,SESSION:CHARSET,"1251")).
         pause 1.
      END.
      ELSE 
         vTmpValue = TRIM(tObject.fObject:INPUT-VALUE).   
               
      oValues = oValues + vDelimiter +
               (IF tObject.fObject:INPUT-VALUE = ? 
                THEN "?" 
                ELSE vTmpValue /*tObject.fObject:INPUT-VALUE*/).
      vDelimiter = iDelimiter.
   END. /* End of FOR */
   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Обработчик события: выход из поля
  Parameters:  iObjectID - идентификатор поля ввода
  Notes:       Значение поля считается некорректным, если обработчик поля
               вернул ERROR.
------------------------------------------------------------------------------*/
PROCEDURE ON_LEAVE:
   DEFINE INPUT  PARAMETER iObjectID AS INT64    NO-UNDO.

   DEFINE BUFFER tObject FOR tObject.

   FIND FIRST tObject WHERE tObject.fId = iObjectID  NO-ERROR.

   IF AVAILABLE tObject             AND
      VALID-HANDLE(tObject.fObject) AND
      {assigned tObject.fLeave}     THEN
   DO:
      APPLY "ENTRY" TO tObject.fObject.
      RUN VALUE(tObject.fLeave)(tObject.fObject:INPUT-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY "ERROR".
   END.
   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Обработчик события: F1 (выбор значения из множества)
  Parameters:  iObjectID - идентификатор поля ввода
  Notes:       Выбранное в обработчике значение должно передаваться через
               переменную pick-value.
------------------------------------------------------------------------------*/
PROCEDURE ON_F1:
   DEFINE INPUT  PARAMETER iObjectID AS INT64    NO-UNDO.

   DEFINE BUFFER tObject FOR tObject.

   FIND FIRST tObject WHERE tObject.fId = iObjectID  NO-ERROR.

   IF AVAILABLE tObject             AND
      VALID-HANDLE(tObject.fObject) AND
      {assigned tObject.fF1}        THEN
   DO TRANSACTION:
      pick-value = ?.

      IF tObject.fObject:DATA-TYPE = "date" THEN
         RUN calend.p.
      ELSE
      DO:
         IF NUM-ENTRIES(tObject.fF1) EQ 2 THEN
            RUN VALUE(ENTRY(1,tObject.fF1)) (ENTRY(2,tObject.fF1),
                                             tObject.fObject:INPUT-VALUE) NO-ERROR.
         ELSE
            RUN VALUE(tObject.fF1)(tObject.fObject:INPUT-VALUE) NO-ERROR.
      END.

      IF pick-value <> ? THEN 
         tObject.fObject:SCREEN-VALUE = pick-value NO-ERROR.
   END.
   RETURN NO-APPLY.
END PROCEDURE.
