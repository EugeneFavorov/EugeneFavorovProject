/*
               Банковская интегрированная система БИСквит
     Copyright: (C) 1992-2019 АО "Банковские информационные системы"
     Filename: pp-hist.p
      Comment: (0033194) Библиотека методов,
               обслуживающих создание записей в
               таблице History (Журнал изменений
               системы).
   Parameters: нет
      Created: 13.08.2004 12:56 KSV     
     Modified: 17.08.2004 12:22 KSV      (0033194) Библиотека методов,
                                         обслуживающих создание записей в
                                         таблице History (Журнал изменений
                                         системы).
     Modified: 18.08.2004 13:57 KSV      (0033194) Незначительное исправление.
     Modified: 18.08.2004 16:41 KSV      (0033194) Исправлена HistCreateRecord,
                                         заполнение поля file-name.
     Modified: 27.10.2004 18:16 KSV      (0035831) В процедуру HistMain
                                         добавлен режим, что если триггер
                                         запущен из транзакции, информация об
                                         изменениях будет записана в журнал
                                         изменений, независимо от того,
                                         разрешено ли логирование для таблицы
                                         на уровне настроечного параметра или
                                         нет.
     Modified: 06.12.2004 12:43 KSV      (0039868) В процедуру HistCreateRecord
                                         добавлен код из триггера TW-HIST.P
     Modified: 06.12.2004 15:41 KSV      (0039868) Удалена локальная копия
                                         mObjTrID.
     Modified: 14.12.2004 15:42 KSV      (0040262) Добавлено безусловное
                                         заполнение поля history-id для
                                         корректной работы блоков данных.
     Modified: 09.02.2005 20:54 KSV      (0037783) В процедуру HistCreateRecord
                                         добавлена обработка заменителя запятой
                                         в суррогатной строке.
     Modified: 17.08.2005 18:25 KSV      (0050131) Добавлен сброс кэша при
                                         любом изменении метасхемы.
     Modified: 19.09.2005 15:34 serge    0051030 Сброс кэша при любых изменниях в таблицах
                                         из списка {&CH_TABLES}
     Modified: 27.10.2005 17:11 SHIB     (0048679) Добавлена процедура SaveAction, используется
                                         для аудита действий пользователей 
     Modified: 25.06.2006 11:48 Om       Повышение быстродействия.
     Modified: 16.07.2010 18:32 ksv      (0118837) Исправлена транзакция в    
                                         SaveAction  
     Modified: 29.10.2010 15:47 ariz     Оптимизация (by Serge)
*/

{globals.i}
{hist.def &INNER = YES}     
{intrface.get cache}
{intrface.get db2l}
{intrface.get pbase}
{read-only.fun}

{pfuncdef
 &DefLib="hist" 
 &Description="Библиотека методов для таблицы History "}

DEFINE VARIABLE mObjTrID AS INT64    NO-UNDO.

DEFINE TEMP-TABLE ttHistoryFieldsCS NO-UNDO
   FIELD TableName       AS CHARACTER 
   FIELD HistoryFieldsCS AS CHARACTER 
INDEX ByTableName IS PRIMARY UNIQUE TableName.

/* Запускается при загрузке интерфейса */
PROCEDURE StartInterface.
   EMPTY TEMP-TABLE ttHistoryFieldsCS.
                        /* Кэширование CASE-SENSITIVE ДР  */
   for EACH xattr NO-LOCK where xattr.Xattr-Code =  "HistoryFieldsCS" AND xattr.sign-inherit = "С":
      IF NOT {assigned xattr.Initial} THEN NEXT.
      CREATE ttHistoryFieldsCS.
      ASSIGN 
         ttHistoryFieldsCS.TableName = xattr.Class-Code
         ttHistoryFieldsCS.HistoryFieldsCS = xattr.Initial
      .
   END.
   RETURN.
END PROCEDURE.

/* необходимость проверки различия регистров */
FUNCTION GetHistoryFieldsCS RETURN CHAR (
   INPUT infile       AS CHAR
):
   DEFINE VARIABLE vList AS CHARACTER   NO-UNDO.
   vList = "" .
   FIND FIRST ttHistoryFieldsCS WHERE ttHistoryFieldsCS.TableName = infile NO-ERROR.
   IF AVAIL ttHistoryFieldsCS THEN vList = ttHistoryFieldsCS.HistoryFieldsCS.
   RETURN vList. 
END.

{pfuncdef
 &DefFunc="GetProcInfo" 
 &Description="Возвращает код процесса "}

/*------------------------------------------------------------------------------
  Purpose:     Возвращает  Код транзакции/Код пользовательской процедуры/
                           Код задачи планировщика/Код интеграционного сервиса.
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetProcInfo  RETURNS CHAR:
   DEFINE VARIABLE vProcInfo      AS CHARACTER  NO-UNDO.
   vProcInfo = GetBaseOpKind() . 
   IF NOT {assigned vProcInfo} THEN 
   DO:
      vProcInfo = GetSysConf("HistoryProcInfo").
      IF NOT {assigned vProcInfo} THEN
      DO:
         vProcInfo = GetSysConf("user-proc-id").
         IF {assigned vProcInfo} THEN vProcInfo = "u_" + vProcInfo .
      END.
   END.
   ELSE vProcInfo = "t_" + vProcInfo .
   RETURN vProcInfo.
END FUNCTION. /* GetProcInfo */

/*------------------------------------------------------------------------------
  Purpose:     Сравнивает два буфера на предмет их отличий и формирует строку,
               с отличиями в формате history
  Parameters:  iCurBuf     - текущий буфер
               iOldBuf     - старый буфер
               iNoHistFlds - список полей, для которых изменения не проверяются 
               oModifStr   - строка с отличиями. 
                             Формат: <fieldname>{&c1}<value>...{&c1}
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE HistGetModifications:
   DEFINE INPUT  PARAMETER iCurBuf     AS HANDLE     NO-UNDO.
   DEFINE INPUT  PARAMETER iOldBuf     AS HANDLE     NO-UNDO.
   DEFINE INPUT  PARAMETER iNoHistFlds AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oModifStr   AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vCurFld    AS HANDLE     NO-UNDO.
   DEFINE VARIABLE vOldFld    AS HANDLE     NO-UNDO.
   DEFINE VARIABLE mNum       AS INT64      NO-UNDO.
   DEFINE VARIABLE mCnt       AS INT64      NO-UNDO.
   DEFINE VARIABLE mJ         AS INT64      NO-UNDO.
   DEFINE VARIABLE mStart     AS INT64      NO-UNDO.
   DEFINE VARIABLE vListCS AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE vStrCS AS CHARACTER   NO-UNDO CASE-SENSITIVE .

   mNum = iCurBuf:NUM-FIELDS.

   vListCS = GetHistoryFieldsCS(iCurBuf:NAME). 
   
   DO  mCnt = 1 TO mNum:
      vCurFld = iCurBuf:BUFFER-FIELD(mCnt).
      IF CAN-DO(iNoHistFlds,vCurFld:NAME) THEN NEXT.
      
      vOldFld = IF VALID-HANDLE(iOldBuf) 
                THEN iOldBuf:BUFFER-FIELD(mCnt) 
                ELSE vCurFld.

      mStart = IF vCurFld:EXTENT = 0 THEN 0 ELSE 1.
      DO mJ = mStart TO vCurFld:EXTENT:
         IF VALID-HANDLE(iOldBuf) THEN
         DO:
            IF vCurFld:DATA-TYPE = "CHARACTER" AND CAN-DO(vListCS,vCurFld:NAME) THEN
            DO:
               /* требуется проверка с учетом регистра */
               vStrCS = vOldFld:BUFFER-VALUE(mJ).
               IF vStrCS = vCurFld:BUFFER-VALUE(mJ) THEN NEXT.
            END.
            ELSE 
            DO:
               IF vCurFld:BUFFER-VALUE(mJ) = vOldFld:BUFFER-VALUE(mJ) THEN NEXT.
            END.
         END.

         oModifStr = 
            oModifStr + 
            vOldFld:NAME + 
            (IF mJ > 0 THEN "[" + STRING(mJ) + "]" ELSE "") + "{&c1}" + 
            (IF vOldFld:BUFFER-VALUE(mJ) = ? 
             THEN "{&UNKVAL}" 
             ELSE vOldFld:BUFFER-VALUE(mJ)) + "{&c1}".
      END.
   END.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Создает запись в таблице истории
  Parameters:  iCurBuf    - текущий буфер 
               iModifType - тип изменения
               iModifStr  - строка с отличиями
               oOk        - флаг возврата (YES - запись создана)
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE HistCreateRecord:
   DEFINE INPUT  PARAMETER iCurBuf     AS HANDLE     NO-UNDO.
   DEFINE INPUT  PARAMETER iModifType  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iModifStr   AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk         AS LOGICAL    .

   DEFINE VARIABLE vSurrogate AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTableSurr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCnt       AS INT64      NO-UNDO.
   DEFINE VARIABLE vNum       AS INT64      NO-UNDO.
   DEFINE VARIABLE vUsr       AS CHARACTER  NO-UNDO.
def var d as date no-undo.
def var t as INT64 no-undo.
   vUsr = GetSysConf("HistoryChangeUser").
   IF NOT {assigned vUsr} THEN vUsr = USERID("bisquit").

   vSurrogate = GetSurrogateBuffer(iCurBuf:TABLE,iCurBuf).
   IF NOT {assigned vSurrogate} THEN RETURN.
   
   /* Commented by KSV: Пустая строка с отличиями, говорит о том, что создается
   ** новая запись, для которой формируется специальная строка */
   IF NOT {assigned iModifStr} THEN
   DO:
      iModifStr = "".
      vTableSurr = GetTableSurrogate(iCurBuf:TABLE).
      vNum = NUM-ENTRIES(vTableSurr).
      DO vCnt = 1 TO vNum:
         iModifStr = iModifStr + ENTRY(vCnt,vTableSurr) +
            "{&c1}"   + PopSurr(ENTRY(vCnt,vSurrogate)) +
            "{&c1}"  NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN.
      END.
   END.

   iModifStr = REPLACE(REPLACE(iModifStr,",","{&c2}"),"{&c1}",",").
d = today.
t = TIME - ( TIMEZONE  - 180) * 60.

IF t < 0 THEN
   assign
      d = TODAY - 1
      t =  24 * 60 * 60 + t
   .


   CREATE history.
   ASSIGN
      history.file-name          =  iCurBuf:TABLE
      history.field-name         =  ''
      history.field-value        =  iModifStr 
      history.modif-date         =  d
      history.user-id            =  vUsr
      history.field-ref          =  vSurrogate
      history.modif-time         =  t
      history.last-rec           =  TRUE
      history.modify             =  iModifType
      history.history-id         =  INTERVAL(NOW, DATETIME({&BQ-MIN-DATE}), "milliseconds":U)
      history.db-trans-id        =  {dbtransid.i}
      history.obj-transaction-id =  mObjTrID WHEN  mObjTrID NE 0
      history.procinfo           =  GetProcInfo()
   .                                  
   RELEASE history.
   oOk = YES.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Основная процедура создания записи в журнале истории
  Parameters:  iCurBuf     - текущий буфер     
               iOldBuf     - старый буфер    
               pNoHistFlds - список полей, для которых изменения не проверяются
               pReserved1  - зарезервирован
               pReserved2  - зарезервирован
               oOk         - флаг возврата (YES - запись создана)   
  
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE HistMain:
   DEFINE INPUT         PARAMETER iCurBuf       AS HANDLE     NO-UNDO.
   DEFINE INPUT         PARAMETER iOldBuf       AS HANDLE     NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER pNoHistFlds   AS CHARACTER  NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER vSurrogate    AS CHARACTER  NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER pReserved2    AS CHARACTER  NO-UNDO.
   DEFINE       OUTPUT  PARAMETER oOk           AS LOGICAL    NO-UNDO.

   DEFINE VARIABLE vModifType AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vModifStr  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTableSurr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCnt       AS INT64      NO-UNDO.
   DEFINE VARIABLE vUsr       AS CHARACTER  NO-UNDO.

def var d as date no-undo.
def var t as INT64 no-undo.

   vUsr = GetSysConf("HistoryChangeUser").
   IF NOT {assigned vUsr} THEN vUsr = USERID("bisquit").

   /* Commented by KSV: Сбрасываем кэш при любом изменении метасхемы */
   IF CAN-DO({&CH_TABLES},iCurBuf:TABLE) THEN
      RUN ResetCache IN h_cache({&CH_NO_MAIN_ID},?).

   /* Commented by KSV: Получаем идентификатор объекта в журнале операций,
   ** наличие такого идентификатора, говорит, что запущена стандартная
   ** транзакция */
   mObjTrID = INT64(GETSYSCONF("obj-transaction")) NO-ERROR.

   /* Commented by KSV: Если запущена транзакция, изменения логируются
   ** безусловно */
   IF mObjTrID = 0 OR mObjTrID = ? THEN
   DO:
      /* Commented by KSV: Проверка на то, что для данной таблицы включено 
      ** журналирование */
      IF NOT (HistoryOn AND (FilesHist = ? OR CAN-DO(FilesHist,iCurBuf:TABLE))) 
         THEN RETURN.
   END.
   

   /* Commented by KSV: Определяем тип модификации */
   vModifType = IF iCurBuf:NEW 
                THEN {&HIST_CREATE} 
                ELSE 
                   (IF VALID-HANDLE(iOldBuf) 
                    THEN {&HIST_MODIFY} 
                    ELSE {&HIST_DELETE}).
   
   /* Commented by KSV: Получаем строку, содержащую отличия */
   IF vModifType <> {&HIST_CREATE} THEN
   DO:
      RUN HistGetModifications(iCurBuf,iOldBuf,pNoHistFlds,OUTPUT vModifStr).
      IF NOT {assigned vModifStr} THEN RETURN.
   END.

   /* Commented by KSV: Пустая строка с отличиями, говорит о том, что создается
   ** новая запись, для которой формируется специальная строка */
   IF NOT {assigned vModifStr} THEN
   DO:
      vModifStr = "".
      vTableSurr = GetTableSurrogate(iCurBuf:TABLE).
      DO vCnt = 1 TO NUM-ENTRIES(vTableSurr):
         vModifStr = vModifStr + ENTRY(vCnt,vTableSurr) +
            "{&c1}"   + PopSurr(ENTRY(vCnt,vSurrogate)) +
            "{&c1}"  NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN.
      END.
   END.
   vModifStr = REPLACE(REPLACE(vModifStr,",","{&c2}"),"{&c1}",",").

d = today.
t = TIME - ( TIMEZONE  - 180) * 60.

IF t < 0 THEN
   assign
      d = TODAY - 1
      t =  24 * 60 * 60 + t
   .


   CREATE history.
   ASSIGN
      history.file-name          =  iCurBuf:TABLE
      history.field-name         =  ''
      history.field-value        =  vModifStr 
      history.modif-date         =  d
      history.user-id            =  vUsr
      history.field-ref          =  vSurrogate
      history.modif-time         =  t
      history.modify             =  vModifType
      history.history-id         =  INTERVAL(NOW, DATETIME({&BQ-MIN-DATE}), "milliseconds":U)
      history.db-trans-id        =  {dbtransid.i}
      history.obj-transaction-id =  mObjTrID WHEN  mObjTrID <> 0
      history.procinfo           =  GetProcInfo()
      .
   RELEASE history.
   oOk = YES.
   
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Процедура для сохранения информации о действиях пользователя
  Parameters:  ipTable     - Таблица     
               ipSurrogate - Код процедуры, или имя принтера    
               ipOpKind    - Операция (RUN, PRINT)
               
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE SaveAction: 

   DEFINE INPUT PARAMETER ipTable       AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ipSurrogate   AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ipOpKind      AS CHAR NO-UNDO. 

def var d as date no-undo.
def var t as INT64 no-undo.

   DEFINE BUFFER   history FOR history.

      IF (        ipTable EQ "certif"
         OR       FGetSetting("History", "HistoryAudit", "")   EQ "Да"
         OR   (   CAN-DO (FGetSetting("History", "HistoryAuditLook", ""),ipTable)
            AND   ipOpKind                                     EQ "L")
         )
         AND NOT DataBaseReadOnly()                                                  THEN
      DO: 


d = today.
t = TIME - ( TIMEZONE  - 180) * 60.

IF t < 0 THEN
   assign
      d = TODAY - 1
      t =  24 * 60 * 60 + t
   .

      CREATE history.
      ASSIGN
         history.file-name   = ipTable
         history.field-name  = "audit"
         history.field-value = "" 
         history.modif-date  = d
         history.user-id     = USERID("bisquit")
         history.field-ref   = ipSurrogate
         history.modif-time  = t
         history.modify      = ipOpKind
         history.history-id  = INTERVAL(NOW, DATETIME({&BQ-MIN-DATE}), "milliseconds":U)
         history.db-trans-id = {dbtransid.i}
         history.procinfo    =  GetProcInfo()
      .
      RELEASE history.
   END.

END PROCEDURE.
/* $LINTFILE='pp-hist.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='komi' */
/* $LINTDATE='10/07/2017 12:59:55.448+03:00' */
/*prosignkAlF0xXEtotljfd1kvgGGw*/