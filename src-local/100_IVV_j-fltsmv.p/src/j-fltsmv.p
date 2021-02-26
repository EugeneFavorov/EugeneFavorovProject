/*              
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: j-fltsmv.p
      Comment: Сортировка и отбор документов
   Parameters: iClass      - код класса
               iInstance   - содержимое класса iClass
         Uses:
      Used BY:
      Created: 05.05.2016 ivrg
*/
{globals.i}
{debug.equ}

DEFINE INPUT PARAMETER iClass    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iInstance AS HANDLE    NO-UNDO.

{form.def}
{g-trans.equ}
{exchange.equ}

{intrface.get xclass}

{intrface.get tmess}
{intrface.get pbase}
{intrface.get trans}
{intrface.get data}

{intrface.get pack}
{intrface.get exch}
{intrface.get edeal}
{intrface.get flt}

DEFINE VARIABLE mFilterTable AS HANDLE NO-UNDO.
DEFINE VARIABLE mFilter      AS HANDLE NO-UNDO.

DEFINE VARIABLE mBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE mFlagSet   AS LOGICAL   INITIAL ? NO-UNDO.
DEFINE VARIABLE mFiltClass AS CHARACTER NO-UNDO.

DEFINE VARIABLE mAttrList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mValList      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRetryErr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCodeLstChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mValLstChar   AS CHARACTER NO-UNDO.

{ttretval.def}

&GLOB RETRY-ERROR  mRetryErr                     /* для do-retry.i            */

/*============================================================================*/

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
      os-delete value("debug.txt").
   &ENDIF

   ASSIGN
      mFiltClass  = GetInstanceProp(iInstance,"FiltClass")
      mFilter     = iInstance:default-buffer-handle
   NO-ERROR.

   &IF DEFINED(IS-DEBUG) &THEN
      run dbgprint.p(program-name(1) + " {&line-number} auto", STRING(auto)).
      run dbgprint.p(program-name(1) + " {&line-number} mFiltClass", mFiltClass).
   &ENDIF

   IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

   RUN GetParentXMethod IN h_xclass (iClass,"update", BUFFER class-method).

   IF AVAILABLE(class-method) THEN DO:
      &IF DEFINED(IS-DEBUG) &THEN
         run dbgprint.p(program-name(1) + " {&line-number} class-method.PROCEDURE", 
            class-method.PROCEDURE).
      &ENDIF

      {exch-run.i &Proc = class-method.PROCEDURE
                  &Parm = "iClass, iInstance"}

      IF ERROR-STATUS:ERROR THEN DO:
         ASSIGN
            mRetryErr = SUBSTITUTE( "Невозможно выполнить метод &1.&2", 
               iClass, class-method.PROCEDURE )
            mFlagSet  = NO
         NO-ERROR.
         UNDO MAIN, RETRY MAIN.
      END.
   END.
   ELSE DO:
      ASSIGN
         mRetryErr = "Отсутствуют данные о фильтре"
         mFlagSet  = YES
      NO-ERROR.
      UNDO MAIN, LEAVE MAIN.
   END.

   ASSIGN
      mFilterTable = widget-handle(mFilter:buffer-field("FilterTable"):buffer-value)
   NO-ERROR.

   IF NOT valid-handle(mFilterTable) THEN DO:
      ASSIGN
         mRetryErr = "Отсутствуют данные для экспорта"
         mFlagSet  = YES
      NO-ERROR.
      UNDO MAIN, LEAVE MAIN.
   END.

   mBuffer = mFilterTable:default-buffer-handle.

   &IF DEFINED (IS-DEBUG) &THEN
      run PrintFilterLog(mBuffer).
   &ENDIF

   IF class-method.PROCEDURE =  "u-flttmp" THEN
      RUN OpDelete IN THIS-PROCEDURE (mBuffer).
   ELSE
      RUN OpAddAuto IN THIS-PROCEDURE (mBuffer).
   IF NOT valid-handle(mBuffer) THEN DO:
      ASSIGN
         mRetryErr = "Данные для экспорта отсутствуют"
         mFlagSet  = YES
      NO-ERROR.
      UNDO MAIN, LEAVE MAIN.
   END.

   RUN OpDeleteSend IN THIS-PROCEDURE (mBuffer).

   mBuffer:find-first() NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      ASSIGN
         mRetryErr = "Нет данных для экспорта"
         mFlagSet  = YES
      NO-ERROR.
      UNDO MAIN, LEAVE MAIN.
   END.

   mAttrList = GetInstanceProp(iInstance,"AttrList").
   IF {assigned mAttrList} THEN
      ASSIGN
         mValList  = CHR(1) + GetInstanceProp(iInstance,"ValList")
         mAttrList = CHR(1) + mAttrList
         mCodeLstChar = "FilterTable~001RidRest~001RetRcp~001RetFld~001RetType" + mAttrList
         mValLstChar  = SUBSTITUTE("&1~001YES~001&2~001ROWID~001Multi&3",
                           STRING(mFilterTable), STRING(TEMP-TABLE ttRetVal:HANDLE), mValList)
   .

   IF auto <> YES AND class-method.PROCEDURE <> "u-flttmp" THEN DO:
USR-SEL:
   REPEAT:
      {empty ttRetVal}

      &IF DEFINED (IS-DEBUG) &THEN
         run dbgprint.p(program-name(1) + " {&line-number} mCodeLstChar", mCodeLstChar). 
         run dbgprint.p(program-name(1) + " {&line-number} mValLstChar", mValLstChar). 
      &ENDIF

      /* запуск браузера */
      RUN SetSysConf IN h_base ("OpDay-ArchDay", "ДА").
      RUN browseld.p (mFiltClass,
                      mCodeLstChar,
                      mValLstChar,
                      "",
                      4)
                      NO-ERROR.
      RUN DeleteOldDataProtocol IN h_base ("OpDay-ArchDay").                  

      IF KEYFUNCTION(LASTKEY) =  "END-ERROR" THEN DO:
         IF CAN-FIND(FIRST ttRetVal) THEN DO:
            RUN Fill-AlertSysMes IN h_tmess ("", "", "4", 
               "Выполнить создание сообщений для отмеченных документов ?").

            CASE pick-value:
               WHEN "YES" THEN DO:
                  RUN KeepSelectFilter IN THIS-PROCEDURE (mBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN "NO" THEN DO:
                  RUN DelFilterTable IN THIS-PROCEDURE (mBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN ? THEN NEXT  USR-SEL.
            END CASE.
         END.
         ELSE DO:
            RUN Fill-AlertSysMes IN h_tmess ("", "", "4", 
               "Не отмечен  ни один документ для экспорта.~nПродолжить выполнение транзакции?").

            CASE pick-value:
               WHEN "YES" THEN DO:
                  RUN DelFilterTable IN THIS-PROCEDURE (mBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN "NO" THEN DO:
                  RUN DelFilterTable IN THIS-PROCEDURE (mBuffer).
                  ASSIGN
                     mRetryErr = "Отказ от выполнения экспорта"
                     mFlagSet  = NO
                  NO-ERROR.
                  UNDO MAIN, RETRY MAIN.
               END.
               WHEN ?  THEN NEXT  USR-SEL.
            END CASE.
         END.
      END.
      ELSE IF KEYFUNCTION(LASTKEY) =  "GO" THEN DO:
         IF CAN-FIND(FIRST ttRetVal) THEN DO:
            RUN KeepSelectFilter IN THIS-PROCEDURE (mBuffer).
            LEAVE USR-SEL.
         END.
         ELSE DO:
            RUN Fill-AlertSysMes IN h_tmess ("", "", "4", 
               "Выполнить экспорт всех документов ?").

            CASE pick-value:
               WHEN "YES" THEN DO:
                  LEAVE USR-SEL.
               END.
               WHEN "NO" THEN DO:
                  RUN DelFilterTable IN THIS-PROCEDURE (mBuffer).
                  LEAVE USR-SEL.
               END.
               WHEN ? THEN NEXT  USR-SEL.
            END CASE.
         END.
      END.
   END.                                          /* USR-SEL: REPEAT:          */
   END. /* Запускаем браузер */
   
   mFlagSet = YES.
END.                                             /* MAIN:  DO:                */

&IF DEFINED(IS-DEBUG) &THEN
RUN PrintFilterLog (mBuffer).
&ENDIF

{intrface.del}

{doreturn.i mFlagSet}
/*----------------------------------------------------------------------------*/
/* добавление при режиме auto                                                 */
/*----------------------------------------------------------------------------*/
PROCEDURE OpAddAuto PRIVATE:
   DEFINE INPUT PARAMETER iBuffer   AS HANDLE   NO-UNDO.

   DEFINE VARIABLE vQuery    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vFlagSet  AS LOGICAL   INITIAL ? NO-UNDO.

   DEFINE VARIABLE vStatus    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vLastDays  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vLast      AS INT64     NO-UNDO.
   DEFINE VARIABLE vDateFrom  AS DATE      NO-UNDO.
   DEFINE VARIABLE vDateTo    AS DATE      NO-UNDO.
   DEFINE VARIABLE vId        AS INT64     NO-UNDO.

   DEFINE BUFFER op       FOR op.
   DEFINE BUFFER tmpsigns FOR tmpsigns.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      ASSIGN
         vStatus   = GetAttrValue2("",0,"Status")
         vLastDays = GetAttrValue2("",0,"LastDays")
         vLast     = IF {assigned vLastDays} THEN INT64(vLastDays) ELSE 0
         vDateTo   = DATE(GetAttrValue2("",0,"DateTo"))
         vDateFrom = IF auto =  YES THEN vDateTo - vLast ELSE DATE(GetAttrValue2("",0,"DateFrom"))
         vId       = 1
      NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
         run dbgprint.p(program-name(1) + " {&line-number} vStatus", vStatus). 
         run dbgprint.p(program-name(1) + " {&line-number} vLast", STRING(vLast)).
         run dbgprint.p(program-name(1) + " {&line-number} vDateFrom", STRING(vDateFrom)).
         run dbgprint.p(program-name(1) + " {&line-number} vDateTo", STRING(vDateTo)).
      &ENDIF

      RUN DelFilterTable IN THIS-PROCEDURE (iBuffer).
      FOR EACH tmpsigns WHERE
         tmpsigns.file-name =  "op"    AND
         tmpsigns.code =  "ИННЖКУ"     AND
         tmpsigns.since >= vDateFrom   AND 
         tmpsigns.since <= vDateTo
            NO-LOCK,
         EACH op WHERE 
            op.op =  INT64(tmpsigns.surrogate) AND
            CAN-DO(vStatus,op.op-status)  and 
            not CAN-DO("03*",op.doc-type)
            NO-LOCK:
         &IF DEFINED(IS-DEBUG) &THEN
            run dbgprint.p(program-name(1) + " {&line-number} tmpsigns.since", 
               STRING(tmpsigns.since)).
            run dbgprint.p(program-name(1) + " {&line-number} op.op", STRING(op.op)).
            run dbgprint.p(program-name(1) + " {&line-number} op.op-status", op.op-status).
         &ENDIF

         iBuffer:BUFFER-CREATE().
         ASSIGN
            iBuffer:BUFFER-FIELD("__filterid"):BUFFER-VALUE = vId
            iBuffer:BUFFER-FIELD("op"):BUFFER-VALUE = op.op
            vId = vId + 1
         NO-ERROR.
         iBuffer:BUFFER-RELEASE().
      END.
   END.                                          /* MAIN: DO ...              */
   vFlagSet = YES.

   IF valid-handle(vQuery) THEN DELETE OBJECT vQuery.

   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* удаление ранее отправленных                                                */
/*----------------------------------------------------------------------------*/
PROCEDURE OpDeleteSend PRIVATE:
   DEFINE INPUT PARAMETER iBuffer   AS HANDLE   NO-UNDO.

   DEFINE VARIABLE vQuery    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vFlagSet  AS LOGICAL   INITIAL ? NO-UNDO.
   DEFINE VARIABLE vOp       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFlagFind AS LOGICAL   NO-UNDO.

   DEFINE BUFFER PackObject FOR PackObject .
   DEFINE BUFFER Packet FOR Packet.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY vQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      vQuery:ADD-BUFFER(iBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT vQuery:QUERY-PREPARE("FOR EACH " + iBuffer:name)
         OR NOT vQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      vQuery:GET-FIRST().
      DO_WHILE_iBuffer:
      DO WHILE iBuffer:AVAILABLE:
         ASSIGN
            vOp    = iBuffer:buffer-field("op"):buffer-value
         NO-ERROR.

         &IF DEFINED (IS-DEBUG) &THEN
            run dbgprint.p(program-name(1) + " {&line-number} OpDeleteSend vOp", STRING(vOp)). 
         &ENDIF

         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

         vFlagFind = NO.

         FOR_EACH_PackObject:
         FOR EACH PackObject WHERE
                  PackObject.file-name =  "op"
              AND PackObject.surrogate =  STRING(vOp)
                  NO-LOCK,
            FIRST Packet WHERE
                  Packet.PacketID =  PackObject.PacketID
              AND Packet.mail-format =  "XSMEVPayJKH"
              AND Packet.state <> {&STATE-ERR}
                  NO-LOCK:
            vFlagFind = YES.
            LEAVE FOR_EACH_PackObject.
         END.

         IF vFlagFind THEN DO:
            RUN Op-error IN THIS-PROCEDURE (vOp, "был отправлен ранее" ).

            iBuffer:BUFFER-DELETE() NO-ERROR.
         END.

         IF NOT valid-handle(iBuffer) THEN 
            LEAVE DO_WHILE_iBuffer.

         vQuery:GET-NEXT() NO-ERROR.
      END.
      vQuery:QUERY-CLOSE().
      DELETE OBJECT vQuery.
   END.                                          /* MAIN: DO ...              */
   vFlagSet = YES.

   IF valid-handle(vQuery) THEN DELETE OBJECT vQuery.

   {doreturn.i vFlagSet}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Сохранение отобраных значений                                              */
/*----------------------------------------------------------------------------*/
PROCEDURE KeepSelectFilter PRIVATE:
   DEFINE INPUT PARAMETER iBuffer AS handle NO-UNDO.

   DEFINE VARIABLE vQuery AS HANDLE NO-UNDO.
   DEFINE VARIABLE vOp    AS INT64  NO-UNDO.

   DEFINE BUFFER Op FOR Op.

   CREATE QUERY vQuery.
   vQuery:ADD-BUFFER(iBuffer).
   IF    NOT vQuery:QUERY-PREPARE("FOR EACH " + iBuffer:name)
      OR NOT vQuery:QUERY-OPEN()  THEN RETURN.

   vQuery:GET-FIRST().
   DO WHILE iBuffer:AVAILABLE:
      ASSIGN
         vOp     = iBuffer:buffer-field("op"):buffer-value
      NO-ERROR.

      FIND FIRST op WHERE op.op  =  vOp NO-LOCK NO-ERROR.
      IF     AVAIL op
         AND NOT CAN-FIND(FIRST ttRetVal WHERE
                                ttRetVal.FileRowId =  ROWID(op)) THEN DO:
         iBuffer:BUFFER-DELETE().
      END.
      vQuery:GET-NEXT().
   END.
   vQuery:QUERY-CLOSE().

   DELETE OBJECT vQuery.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                                                                                                        */
/*----------------------------------------------------------------------------*/
&IF DEFINED(IS-DEBUG) &THEN
PROCEDURE PrintFilterLog:
   DEFINE INPUT PARAMETER iBuffer AS handle NO-UNDO.
   DEFINE VAR vItm   AS INT64  NO-UNDO.
   DEFINE VAR hField AS handle   NO-UNDO.

   DEFINE VAR vQuery  AS handle NO-UNDO.

   run dbgprint.p("{&file-name} {&line-number} ===================================== ", ""). 

   CREATE QUERY vQuery.
   vQuery:add-buffer(iBuffer).
   IF NOT vQuery:query-prepare("for EACH " + iBuffer:name) THEN 
      MESSAGE "ERROR 1" ERROR-STATUS:get-message(1) VIEW-AS ALERT-BOX.
   IF NOT vQuery:query-open() THEN 
      MESSAGE "ERROR 2" ERROR-STATUS:get-message(1) VIEW-AS ALERT-BOX.
   vQuery:get-first().
   DO WHILE iBuffer:AVAILABLE:
      DO vItm = 1 TO iBuffer:num-fields:
         hField = iBuffer:buffer-field(vItm).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("PrintFilterLog",string(hField:Name,"x(30)") + " " +
                                          hField:buffer-value).
         &ENDIF
      END.
      vQuery:get-next().
   END.
   vQuery:query-close().

   DELETE object vQuery.
   run dbgprint.p("{&file-name} {&line-number} ------------------------------------- ", ""). 
END PROCEDURE.
&ENDIF
/*----------------------------------------------------------------------------*/
/*                                                                                                                                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE DelFilterTable PRIVATE:
   DEFINE INPUT PARAMETER iBuffer AS handle NO-UNDO.

   IF valid-handle(iBuffer) THEN 
      iBuffer:empty-temp-table() NO-ERROR.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* проверка ИНН получателя - удовлетворяет справочнику ПоставщикиЖКУ          */
/*----------------------------------------------------------------------------*/
FUNCTION CheckSpravJKH LOGICAL PRIVATE (INPUT iInn AS CHAR):

   DEFINE BUFFER DataBlock FOR DataBlock.
   DEFINE BUFFER DataLine  FOR DataLine.

   FOR EACH DataBlock WHERE
      DataBlock.DataClass-Id =  'PostJKU' AND 
      ((NOT shMode) OR (shMode AND DataBlock.Branch-Id =  shFilial)) AND
      DataBlock.End-Date     <= TODAY
         NO-LOCK,
      EACH DataLine WHERE 
         DataLine.Data-Id =  DataBlock.Data-Id
            NO-LOCK:

         IF DataLine.Sym3 =  iInn THEN
            RETURN YES.
   END.

   RETURN NO.

END FUNCTION.

/*----------------------------------------------------------------------------*/
FUNCTION OpCheckManual CHAR PRIVATE (INPUT iOp AS INT64):

   DEFINE VARIABLE vInn       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDateFrom  AS DATE        NO-UNDO.
   DEFINE VARIABLE vDateTo    AS DATE        NO-UNDO.

   DEFINE BUFFER op        FOR op.
   DEFINE BUFFER tmpsigns  FOR tmpsigns.

   FOR FIRST op WHERE op.op =  iOp NO-LOCK:
      ASSIGN
         vDateFrom = DATE(GetAttrValue2("",0,"DateFrom"))
         vDateTo   = DATE(GetAttrValue2("",0,"DateTo"))
      NO-ERROR.

      &IF DEFINED(IS-DEBUG) &THEN
         run dbgprint.p(program-name(1) + " {&line-number} vDateFrom", STRING(vDateFrom)).
         run dbgprint.p(program-name(1) + " {&line-number} vDateTo", STRING(vDateTo)).
      &ENDIF      

      FOR_FIRST_tmpsigns:
      FOR FIRST tmpsigns WHERE
         tmpsigns.file-name =  "op"          AND
         tmpsigns.surrogate =  STRING(op.op) AND
         tmpsigns.code =  "ИННЖКУ"
            NO-LOCK:

            IF tmpsigns.since >= vDateFrom AND tmpsigns.since <= vDateTo THEN
               RETURN "".
            ELSE
               LEAVE FOR_FIRST_tmpsigns.
      END.

      ASSIGN
         vInn = GetXattrValue("op",STRING(op.op),"inn-rec")
         vInn = IF {assigned vInn} THEN vInn ELSE op.inn
      NO-ERROR.

      IF CheckSpravJKH(vInn) <> YES THEN
         RETURN SUBSTITUTE ("ИНН &1 не найден в справочнике ПоставщикиЖКУ", vInn).
   END.

   RETURN "".      

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* удаление несоответствующих условию отбора                                  */
/*----------------------------------------------------------------------------*/
PROCEDURE OpDelete PRIVATE:
   DEFINE INPUT PARAMETER iBuffer   AS HANDLE   NO-UNDO.

   DEFINE VARIABLE vQuery    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vFlagSet  AS LOGICAL   INITIAL ? NO-UNDO.
   DEFINE VARIABLE vOp       AS INT64     NO-UNDO.
   DEFINE VARIABLE vErr      AS CHAR      NO-UNDO.

MAIN:
   DO ON ERROR UNDO MAIN, RETRY MAIN:
      {do-retry.i MAIN}

      CREATE QUERY vQuery NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      vQuery:ADD-BUFFER(iBuffer) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

      IF    NOT vQuery:QUERY-PREPARE("FOR EACH " + iBuffer:name)
         OR NOT vQuery:QUERY-OPEN() THEN UNDO MAIN, RETRY MAIN.

      vQuery:GET-FIRST().

      DO_WHILE_iBuffer:
      DO WHILE iBuffer:AVAILABLE:
         ASSIGN
            vOp = iBuffer:buffer-field("op"):buffer-value
         NO-ERROR.

         &IF DEFINED (IS-DEBUG) &THEN
            run dbgprint.p(program-name(1) + " {&line-number} OpDelete vOp", STRING(vOp)). 
         &ENDIF

         IF ERROR-STATUS:ERROR THEN RETURN ERROR ERROR-STATUS:get-message(1).

         vErr = OpCheckManual(vOp).
         
         IF {assigned vErr} THEN DO:
            RUN Op-error IN THIS-PROCEDURE (vOp, "не удовлетворяет условиям отбора: " + vErr).

            iBuffer:BUFFER-DELETE() NO-ERROR. 
            IF NOT valid-handle(iBuffer) THEN 
               LEAVE DO_WHILE_iBuffer.
         END.

         vQuery:GET-NEXT() NO-ERROR.
      END.

      vQuery:QUERY-CLOSE().
      DELETE OBJECT vQuery.
   END.                                          /* MAIN: DO ...              */
   vFlagSet = YES.

   IF valid-handle(vQuery) THEN DELETE OBJECT vQuery.

   {doreturn.i vFlagSet}
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* сообщение об ошибке                                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE Op-error PRIVATE:
   DEFINE INPUT PARAMETER iOp   AS INT64   NO-UNDO.
   DEFINE INPUT PARAMETER iMess AS CHAR    NO-UNDO.

   DEFINE BUFFER op FOR op.

&IF DEFINED (IS-DEBUG) &THEN
   FOR FIRST op WHERE op.op =  iOp NO-LOCK:
      RUN Fill-SysMes IN h_tmess ("", "", "0", 
         SUBSTITUTE("Документ &1 от &2 - &3", op.doc-num, STRING(op.doc-date), iMess)).
   END.
&ENDIF

END PROCEDURE.

/******************************************************************************/
/* $LINTFILE='j-fltsmv.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='07/04/2017 12:16:07.505+03:00' */
/*prosignm5dGokFhWsXngx/VY865AA*/