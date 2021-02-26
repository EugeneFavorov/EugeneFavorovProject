/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2020 ЗАО "Банковские информационные системы"
     Filename: nwsocket.cls
      Comment: <A.Krajuschkin>
   Parameters: нет
         Uses: 
      Used by: netw.cls
      Created: 28.04.2014 14:11 Krag    
     Modified: 28.04.2014 15:21 Krag     <0228509 обновление bispc>
     Modified: 30.09.2014 16:23 Krag     парсер не поддерживает abstract.
*/
/*----------------------------------------------------------------
File: nwsocket.cls
Package:
Description: Класс-декоратор для сокета.
----------------------------------------------------------------*/
CLASS nwsocket: 
   {pfuncdef &defclass="nwsocket"} 
   /*----------------------------------------------------------------
   Поля                                               
   ----------------------------------------------------------------*/
   /* хендл сокет */
   DEFINE PROTECTED VARIABLE mSocket            AS HANDLE NO-UNDO.
   /* процедура обратного вызова для сокета */
   DEFINE PROTECTED VARIABLE mCallback          AS HANDLE NO-UNDO.
   /*----------------------------------------------------------------
   Purpose:     Свойства       
   ----------------------------------------------------------------*/
   /* Таймаут сокета при ожидании ответа*/
   DEFINE PUBLIC PROPERTY ResponceTimeout AS INTEGER NO-UNDO 
                                             GET. PROTECTED SET.
   /* Таймаут сокета при чтении */
   DEFINE PUBLIC PROPERTY ReadTimeout AS INTEGER NO-UNDO
                                         GET. PROTECTED SET.

   /*----------------------------------------------------------------
   Конструкторы
   ----------------------------------------------------------------*/
   /*----------------------------------------------------------------
   Конструктор по-умолчанию
   ----------------------------------------------------------------*/
   CONSTRUCTOR PUBLIC nwsocket():
      CREATE SOCKET mSocket NO-ERROR.
      ASSIGN 
         ReadTimeout     = 8
         ResponceTimeout = 12.
   END CONSTRUCTOR.
   /*----------------------------------------------------------------
   Деструктор
   ----------------------------------------------------------------*/
   DESTRUCTOR nwsocket():
      /* закрываем все соединения и удаляем сокет */
       mSocket:DISABLE-CONNECTIONS() NO-ERROR.
       mSocket:DISCONNECT() NO-ERROR.
       DELETE OBJECT mSocket NO-ERROR.
       /* закрываем процедуру чтения */
       IF VALID-HANDLE(mCallback) THEN
          DELETE PROCEDURE mCallback NO-ERROR.
   END DESTRUCTOR.
   /*----------------------------------------------------------------
   Методы
   ----------------------------------------------------------------*/
   /*----------------------------------------------------------------
   Устанавливает соединение.
   Parameters: iHost - адрес
               iPort - порт
               iSecure - использовать защищенное соединение
   ----------------------------------------------------------------*/
   METHOD PROTECTED LOGICAL CONNECT(INPUT iHost AS CHARACTER,
                                    INPUT iPort AS INTEGER,
                                    INPUT iSecure AS LOGICAL):
      DEFINE VARIABLE vSecureArgs AS CHARACTER NO-UNDO.
      /* добавляем аргументы к строке на случай ssl соединения */
      IF iSecure THEN vSecureArgs = "-ssl -nohostverify".
      mSocket:CONNECT(SUBSTITUTE("-H &1 -S &2 &3",
                                 iHost,
                                 STRING(iPort),
                                 vSecureArgs)) NO-ERROR.
      IF ERROR-STATUS:ERROR AND ERROR-STATUS:NUM-MESSAGES GT 0 THEN 
         RETURN FALSE.
      /* подключаем процедуру чтения данных */
      IF NOT VALID-HANDLE(mCallback) THEN
         RUN nwsocketproc.p PERSISTENT SET mCallback (INPUT THIS-OBJECT).
      mSocket:SET-READ-RESPONSE-PROCEDURE("responce",mCallback).
      /* задаем таймаут чтения данных */
      mSocket:SET-SOCKET-OPTION("SO-RCVTIMEO", STRING(ReadTimeout)).
      RETURN mSocket:CONNECTED().
   END METHOD.
   /*----------------------------------------------------------------
   Purpose:     Закрывает соедниение
   ----------------------------------------------------------------*/
   METHOD PROTECTED VOID DISCONNECT():
      /* разрываем соединение */
      IF VALID-HANDLE(mSocket) THEN
         mSocket:DISCONNECT() NO-ERROR.
      /* закрываем процедуру чтения */
      IF VALID-HANDLE(mCallback) THEN
         DELETE PROCEDURE mCallback NO-ERROR.
   END METHOD.
   /*----------------------------------------------------------------
   Purpose:     Возвращает данные из сокета
   ----------------------------------------------------------------*/
   {pfuncdef 
    &defmeth="readresponseprocedure" 
    &description="возвращает данные из сокета"}
   METHOD PUBLIC VOID ReadResponseProcedure(): END METHOD.
   /*----------------------------------------------------------------
   Purpose:     Ждет ответа от клиента 
   ----------------------------------------------------------------*/
   METHOD PROTECTED INT64 AwaitResponce():

      DO ON END-KEY UNDO, RETRY:  
         IF VALID-HANDLE(mSocket) THEN
            WAIT-FOR READ-RESPONSE OF mSocket PAUSE ResponceTimeout.
         IF LASTKEY EQ 27 THEN UNDO, RETRY.
      END.

      RETURN 0.
   END METHOD.
   {pfuncdef &reflection} 
END CLASS.
/* $LINTFILE='nwsocket.cls' */
/* $LINTMODE='1,7,7,7' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='$/ws1-dvp/bq/' */
/* $LINTUSER='mete' */
/* $LINTDATE='27/07/2016 14:42:39.389+03:00' */
/*prosignbSotpsaZqcSW727btRBiUw*/