/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pays.pro
      Comment: Библиотека процедур обмена с платежными системами онлайн
   Parameters: нет
         Uses:
      Used BY: pp-pays.p
      Created: 23.04.2015 KMBIS TT:0236973 Миграция. Прием платежей QIWI, Рапида,Уралсиб, КиберПлат
     Modified: 
*/

/*===============================================================================================*/
/*=== Гарантированное преобразование строки в десятичное значение ===============================*/
FUNCTION GetDecVal RETURNS DEC PRIVATE (INPUT iVal AS  CHAR,
                                        INPUT iDef AS  DEC):

DEF VAR vRes AS  DEC  NO-UNDO.

   vRes = DEC(iVal) NO-ERROR.

   IF ERROR-STATUS:ERROR OR (vRes EQ ?) THEN
     vRes = iDef.

   RETURN vRes.

END FUNCTION. /* GetDecVal */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Поиск референса без учета даты ============================================================*/
FUNCTION FndRef RETURNS LOG (INPUT  iClass   AS CHAR,
                             INPUT  iRef     AS CHAR,
                             OUTPUT oInfoMsg AS CHAR):
DEF VAR vFndRef  AS  LOG   NO-UNDO INIT NO.

DEF BUFFER bRef      FOR Reference.
DEF BUFFER bPack     FOR Packet.
DEF BUFFER bSeance   FOR Seance.

   oInfoMsg = "".
   /* Проверяем референс без учета дня */
   lFndRef:
   FOR FIRST bRef WHERE bRef.Class-Code  EQ iClass
                    AND bRef.RefValue    EQ iRef
                  NO-LOCK:
      vFndRef = YES.

      /* Все пакеты связаны с 1 сеансом в независимости от вложенности */
      FOR FIRST bPack WHERE bPack.PacketId EQ bRef.PacketId
                      NO-LOCK,
         FIRST bSeance WHERE bSeance.SeanceID EQ bPack.SeanceID
                       NO-LOCK:

         oInfoMsg = SUBST("Сеанс обмена номер '&1' от &2 &3 (&4)", 
                          STRING(bSeance.Number),
                          STRING(bSeance.SeanceDate),
                          STRING(bSeance.SeanceTime, "HH:MM:SS"),
                          STRING(bSeance.op-kind)).
      END.
      LEAVE lFndRef.

   END. /* FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}] */

   RETURN vFndRef.

END FUNCTION. /* FndRef */
/*===============================================================================================*/

/*===============================================================================================*/
/*=== Максимальный формат для целого типа по двум переменным ====================================*/
FUNCTION IntFmt RETURNS CHAR (INPUT iVal01  AS INT64,
                              INPUT iVal02  AS INT64):

DEF VAR vRes    AS  CHAR  NO-UNDO.
DEF VAR vMaxInt AS  INT64 NO-UNDO.

   ASSIGN
      iVal01 = 0 WHEN iVal01 EQ ?
      iVal02 = 0 WHEN iVal02 EQ ?
   .
   vMaxInt = MAX(LENGTH(STRING(iVal01)), 
                 LENGTH(STRING(iVal02))).

   IF vMaxInt LT 3 OR vMaxInt EQ ? THEN
      vMaxInt = 3.

   vRes = SUBST("&19", FILL(">", vMaxInt - 1)).

   RETURN vRes.

END FUNCTION. /* IntFmt */
/*===============================================================================================*/

/*===============================================================================================*/
/*=== Максимальный формат для десятичного типа по двум переменным ===============================*/
FUNCTION DecFmt RETURNS CHAR (INPUT iVal01  AS DEC,
                              INPUT iVal02  AS DEC):

DEF VAR vRes    AS  CHAR  NO-UNDO.


   vRes = SUBST("&1.99", IntFmt(INT64(iVal01), INT64(iVal02))). 

   RETURN vRes.

END FUNCTION. /* IntFmt */
/*===============================================================================================*/


/*===============================================================================================*/
/*=== Меняем тэги в переменной на значения из класса ============================================*/
FUNCTION ReplTag RETURNS CHAR PRIVATE (INPUT iExch  AS  HANDLE,
                                       INPUT iStr   AS  CHAR,
                                       INPUT iPref  AS  CHAR):

DEF VAR vVal      AS  CHAR   NO-UNDO.        /* Значение тэга   */
DEF VAR vTag      AS  CHAR   NO-UNDO.        /* Имя тэга        */

DEF BUFFER bAttr FOR xattr.

   FOR EACH bAttr WHERE bAttr.Class-Code EQ iExch::Class-code
                    AND NOT CAN-DO('class,group', bAttr.data-type)
                  NO-LOCK:

      /* Получаем значение реквизита из класса */
      ASSIGN
         vVal = ""
         vVal = iExch:BUFFER-FIELD(bAttr.Xattr-Code):BUFFER-VALUE 
      NO-ERROR.

      /* Заменяем тэг в переменной на найденное значение */
      ASSIGN
         vVal = fStrNvl(vVal, "")                             /* Значение */
         vTag = SUBST("<#&1&2#>", iPref, bAttr.Xattr-Code)    /* Тэг      */
         iStr = REPLACE(iStr, vTag, vVal)
         vVal = ""
      .
   END. /* FOR EACH bAttr WHERE bAttr.Class-Code EQ iExch::Class-code */

   RETURN fStrNvl(iStr, "").

END FUNCTION. /* ReplTag */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Заполнение содержание операции по значением из транспортной формы =========================*/
FUNCTION ParsDetails RETURNS CHAR (INPUT iExch  AS  HANDLE,
                                   INPUT iStr   AS  CHAR):

DEF VAR vKindMain AS  CHAR   NO-UNDO.        /* Строка с указателем на класс с данными заголовка */
DEF VAR vOBJMain  AS  HANDLE NO-UNDO INIT ?. /* Указатель класса с данными заголовка             */

DEF BUFFER bAttr FOR xattr.

   IF VALID-HANDLE(iExch) THEN 
   DO:
      /* Получаем указатель на заголовок */
      vKindMain = iExch::ExchMain NO-ERROR.
     
      /* Если он есть, то присваиваем указатель переменной */
      IF {assigned vKindMain} THEN
         vOBJMain = WIDGET-HANDLE(vKindMain).
     
      /* Если указатель успешно получен, то пробежимся по реквизитам заголовочного класса */
      IF VALID-HANDLE(vOBJMain) THEN 
         iStr = ReplTag(vOBJMain, iStr, "HEAD:").
     
      /* Пробежимся по реквизитам переданного класса */
      iStr = ReplTag(iExch, iStr, "").

   END. /* IF VALID-HANDLE(iExch) THEN */

   RETURN fStrNvl(iStr, "").

END FUNCTION. /* ParsDetails */

/*===============================================================================================*/

