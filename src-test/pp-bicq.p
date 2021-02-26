/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: PP-BICQ.P
      Comment: Методы работы с внутренними сообщениями
   Parameters:
 ------------------------------------------------------------------------------
         Uses:
      Used by: BICQ*.P, SUBMENU.P
 ------------------------------------------------------------------------------
      Created: 18.01.2001 13:29 KSV
     Modified: 25.07.2003 11:42 Serge    
     Modified: 30.07.2003 16:46 Serge    0016219 перевод на использование стандарной процедуры setThisUserXattrValue
     Modified: 31.07.2003 15:52 gsa      0016219 перевод на UpdateSigns
     Modified: 18.02.2008 14:59 ariz
     Modified: 08.06.2009 14:49 SOLM     0103520 Добавление Bicq-портов 
                                         для отправки сообщений в QBIS
     Modified: 18.01.2010 12:49 SOLM     0121912 ClearWrongBICQPorts 
                                         Процедура очистки недействительных Bicq-портов 
     
 ------------------------------------------------------------------------------
    Inventory: 27.03.2001 13:55 KSV
       Number: 1
       Module: Обмен документами
      Service: Внутренний обмен
     Repeated:
      LogType: B
       Future:
     Priority:            
       Remark:
*/
Form "~n@(#) PP-BICQ.P 1.0 KSV 27.03.2001 13:55 KSV 27.03.2001 13:55 comment"
with frame sccs-id stream-io width 250.


/*&GLOBAL-DEFINE BICQMess fill-tt-user-mess(BICQMessID,"BICQ [" + ENTRY(1,PROGRAM-NAME(1)," ") + "] " +*/
&GLOBAL-DEFINE BICQMess RUN Fill-SysMes ("pp-bicq.p",
&GLOBAL-DEFINE MAX_PORT_LEN 200
&GLOBAL-DEFINE TIMEOUT_BICQ_DETECT 30

{globals.i}
{intrface.get xclass}

{intrface.get tmess}
{intrface.get db2l}
{intrface.get widg}
{intrface.get mess}

{bicqstat.def}
DEFINE VARIABLE BICQServer AS HANDLE  NO-UNDO. /* Сервер сообщений */
DEFINE VARIABLE BICQPort AS CHARACTER NO-UNDO. /* Порт, на котором работает сервер */
DEFINE VARIABLE BICQHeader AS CHARACTER INIT "~{~&~@BICQ~}" NO-UNDO.  /* Заголовок сообщения */
DEFINE VARIABLE BICQDelim AS CHARACTER  NO-UNDO. /* Разделитель элементов сообщения */
DEFINE VARIABLE BICQNumEntries AS INT64 INIT 6 NO-UNDO. /* Количество элементов в сообщении */
DEFINE VARIABLE vRetry AS INT64 INIT 10  NO-UNDO. /* Макс. кол-во времени (сек.), в течении которого должно поступить сообщение  */
DEFINE VARIABLE BICQNum AS INT64    NO-UNDO. /* Счетчик вложенных вызовов */

BICQDelim = CHR(127).

FUNCTION BICQDetect RETURN INT64 (pUser AS CHAR,pMode AS CHAR,OUTPUT TABLE FOR tUser) FORWARD.

 /* Check: Are BICQ modules runnin'? */
FUNCTION BICQRun RETURN LOG (pExceptList AS CHAR):
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  RUN GetRunProcs IN h_base (OUTPUT vS).
  LOOK:                          
  DO vI = 1 TO NUM-ENTRIES(vS):
    IF NUM-ENTRIES(ENTRY(vI,vS)," ") = 1 AND ENTRY(vI,vS) MATCHES "bicq*p" AND 
      LOOKUP(ENTRY(vI,vS),pExceptList) = 0 THEN 
    DO:
      RETURN YES.
    END.
  END.
  RETURN NO.
END.

FUNCTION AddPortNum RETURN CHAR (iStr AS CHAR, iPortNum AS CHAR):
  IF LENGTH(iStr) LE {&MAX_PORT_LEN} THEN DO:
     {additem.i iStr iPortNum}
  END.
  ELSE iStr = iPortNum.
  RETURN iStr.
END.

FUNCTION IsPortOk RETURN LOG (pPortNum AS CHAR):
  DEFINE VARIABLE vH       AS HANDLE   NO-UNDO.
  DEFINE VARIABLE vL       AS LOGICAL  NO-UNDO.
  DEFINE VARIABLE vS       AS CHAR     NO-UNDO.
  DEFINE VARIABLE counter  AS INT64    NO-UNDO.

  DO ON STOP UNDO, RETRY:
     IF RETRY THEN DO:
        vS = getThisUserXAttrValue("PortNum").
        IF LOOKUP(pPortNum,vS) NE 0 THEN DO:
           {delitem.i vS pPortNum}
           UpdateSigns("_user", userid("bisquit"), "PortNum", vS, YES).
        END.
        LEAVE.
     END.
     CREATE SERVER-SOCKET vH.

     vL = vH:ENABLE-CONNECTIONS("-S " + pPortNum) NO-ERROR.
     IF vL THEN vH:DISABLE-CONNECTIONS().
     DELETE OBJECT vH.
  END.
  RETURN vL.
END.

PROCEDURE RemoveQbisPorts:
   DEFINE INPUT  PARAMETER iSessionID AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE vS         AS CHAR     NO-UNDO.
   DEFINE VARIABLE vResult    AS CHAR     NO-UNDO.
   DEFINE VARIABLE vTmpStr    AS CHAR     NO-UNDO.
   DEFINE VARIABLE vSession   AS CHAR     NO-UNDO.
   DEFINE VARIABLE n          AS INT64  NO-UNDO.
   DEFINE VARIABLE i          AS INT64  NO-UNDO.
   
   TR:
   DO TRANSACTION 
   ON ERROR UNDO TR, LEAVE TR
   ON STOP  UNDO TR, LEAVE TR:
      vSession = "QBIS_" + iSessionID.
      vS = getThisUserXAttrValue("PortNum").
      DO:
         n = NUM-ENTRIES(vS).
         DO i = 1 TO n:
            vTmpStr = ENTRY(i, vS).
            IF NOT vTmpstr begins vSession + ";" THEN
            DO:
               {additem.i vResult vTmpStr}
            END.
         END.

         IF vResult <> vS THEN
         DO:
            UpdateSigns("_user", userid("bisquit"), "PortNum", vResult, YES).
         END.
      END.
   END. /* TR: */
END PROCEDURE.

PROCEDURE UpdateUserPort:
   DEFINE INPUT  PARAMETER iPortNum AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vS AS CHAR       NO-UNDO.
   TR:
   DO TRANSACTION 
   ON ERROR UNDO TR, LEAVE TR
   ON STOP  UNDO TR, LEAVE TR:
      vS = getThisUserXAttrValue("PortNum").

      IF LOOKUP(iPortNum,vS) = 0 THEN 
      DO:
         vS = AddPortNum(vs,iPortNum).
         UpdateSigns("_user", userid("bisquit"), "PortNum", vS, YES).
      END.
   END. /* TR: */
END PROCEDURE.

/* SOLM: Удаляем недействительные порты из дополнительного атрибута 
** пользователя 'PortNum' */
PROCEDURE ClearWrongBICQPorts:
    BICQDetect(USERID("bisquit"),"LOCK",OUTPUT TABLE tUser).
END PROCEDURE.

FUNCTION GetHostName RETURN CHAR:
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  vS = DBPARAM("bisquit").
  DO vI = 1 TO NUM-ENTRIES(vS):
    IF ENTRY(vI,vS) BEGINS "-H" THEN RETURN TRIM(ENTRY(2,ENTRY(vI,vS)," ")).
  END.
  vS = "".
  OS-COMMAND SILENT VALUE("hostname>bicqhost").
  IF SEARCH("bicqhost") <> ? THEN
  DO:
    INPUT FROM VALUE(SEARCH("bicqhost")).
    IMPORT UNFORMATTED vS.
    INPUT CLOSE.
    OS-DELETE VALUE(SEARCH("bicqhost")).
  END.
  RETURN vS.
END.

FUNCTION GetFreePort RETURN CHAR:
  DEFINE VARIABLE vPRange AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DEFINE VARIABLE vJ AS INT64    NO-UNDO.
  DEFINE VARIABLE vL AS INT64    NO-UNDO.
  DEFINE VARIABLE vH AS INT64    NO-UNDO.
  DEFINE VARIABLE vK AS INT64    NO-UNDO.
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.

  vPRange = FGetSetting("PortRange",?,"").
  IF vPRange = "" THEN 
  DO:
    {&BICQMess}"bicq01","","").
   /*"Не установлен параметр PortRange."*/
    RETURN ?.
  END.
    
  DO vI = 1 TO NUM-ENTRIES(vPRange):
    vS = ENTRY(vI,vPRange).
    IF NUM-ENTRIES(vS,"-") > 1 THEN
    DO:
      vL = INT64(ENTRY(1,vS,"-")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN vL = ?.
      vH = INT64(ENTRY(2,vS,"-")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN vH = ?.
    END. ELSE
    DO:
      vL = INT64(ENTRY(1,vS,"-")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN vL = ?.
      vH = INT64(ENTRY(1,vS,"-")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN vH = ?.
    END.
    IF vH = ? OR vL = ? THEN
    DO:
      {&BICQMess}"bicq02","","").
      /*"Параметр PortRange задан в неверном формате."*/
      RETURN ?.
    END.
    DO vJ = vL TO vH:
      SG:
      FOR EACH signs WHERE signs.file-name EQ '_user'
                       AND signs.code      EQ 'PortNum'
      NO-LOCK:
         DO vK = 1 TO NUM-ENTRIES (signs.code-value):
            IF TRIM (ENTRY (1, ENTRY (vK,signs.code-value), ";")) EQ STRING(vJ) THEN LEAVE SG.
         END.
      END.
      IF     NOT AVAIL signs        /* Порт не занят */
         AND IsPortOK(STRING(vJ)) 
      THEN RETURN STRING(vJ).
    END.
  END.  
  {&BICQMess}"bicq03","","").
  /*"Все порты заняты или недоступны. Увеличьте диапазон портов."*/
  RETURN ?.
END.

FUNCTION GetBICQPort RETURN CHAR:
  RETURN IF NOT IsEmpty(BICQPort) AND VALID-HANDLE(BICQServer) THEN BICQPort ELSE ?.
END.

FUNCTION IsBICQEnabled RETURN LOGICAL:
   RETURN IF FGetSetting("BICQMode",?,"") = "1" AND getThisUserXAttrValue("BICQMode") <> "0" THEN YES ELSE NO.
END FUNCTION.

FUNCTION BICQOpenServer RETURN LOG:
  DEFINE VARIABLE vPort AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vL AS LOGICAL    NO-UNDO.
  /* DEFINE VARIABLE vS AS CHARACTER  NO-UNDO. */
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  RUN ResetAllServers.
  /*del-tt-user-mess(BICQMessID).*/
  IF IsBICQEnabled() = NO THEN RETURN NO.

  CREATE SERVER-SOCKET vH.
  vH:PRIVATE-DATA = BICQMessID.

  LOOP:
  DO vI = 1 TO vRetry:
    BICQPort = ?.
    vPort = GetFreePort().
    IF vPort = ? THEN RETURN NO.
    vL = vH:ENABLE-CONNECTIONS("-S " + vPort) NO-ERROR.
    IF vL THEN LEAVE LOOP.
  END.

  IF NOT vL THEN
  DO:
    {&BICQMess}"bicq04","","").
    /*"Сервер сообщений не запущен."*/
    DELETE OBJECT vH.
    RETURN NO.
  END.

   FIND FIRST _MyConnection NO-LOCK NO-ERROR.
   vPort = vPort + ";" + STRING(_MyConnection._MyConn-Pid).

  vL = NO.
  TR:
  DO TRANSACTION 
  ON ERROR UNDO TR,LEAVE TR
  ON STOP  UNDO TR,LEAVE TR:
    RUN UpdateUserPort( vPort ).
    BICQDetect(USERID("bisquit"),"LOCK",OUTPUT TABLE tUser).
    vL = YES.
  END.  /* End of TR BLOCK */
  IF NOT vL THEN
  DO:
    DELETE OBJECT vH.
    RETURN NO.
  END.
  BICQServer = vH.
  BICQPort = vPort.
  vH:SET-CONNECT-PROCEDURE("SoxResponse").
  RETURN YES.
END.

FUNCTION BICQCloseServer RETURN LOG:
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vC AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
  DEFINE VARIABLE counter AS INT64   NO-UNDO.


  IF NOT VALID-HANDLE(BICQServer) THEN RETURN YES.

  BICQServer:DISABLE-CONNECTIONS().
  DELETE OBJECT BICQServer.

  vS = getThisUserXAttrValue("PortNum").
  IF {assigned BICQPort} AND LOOKUP(BICQPort,vS) NE 0 THEN DO:
     {delitem.i vS BICQPort}
     UpdateSigns("_user", userid("bisquit"), "PortNum", vS, YES).
  END.

  RETURN YES.
END.

PROCEDURE SoxResponse:
  DEFINE INPUT  PARAMETER pH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vM AS MEMPTR     NO-UNDO.
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vSize AS INT64    NO-UNDO.
  DEFINE VARIABLE vFrom AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DO vI = 1 TO vRetry:
    vSize = pH:GET-BYTES-AVAILABLE().
    IF vSize <> 0 THEN LEAVE.
    PAUSE 1 NO-MESSAGE.
  END.
  IF vSize = 0 THEN RETURN.
  

  SET-SIZE(vM) = vSize.
  pH:READ(vM,1,vSize,1) NO-ERROR.
  vS = TRIM(GET-STRING(vM,1)).
  SET-SIZE(vM) = 0.
  IF NOT (vS BEGINS BICQHeader) THEN RETURN.
  IF NUM-ENTRIES(vS,BICQDelim) < BICQNumEntries THEN RETURN.
  RUN BICQTreat(vS).
END.

FUNCTION OpenSox RETURN LOG (pHost AS CHAR,pPort AS CHAR, pMess AS CHAR,pRetry AS INT64,pOptions AS CHAR):
  DEFINE VARIABLE vH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vL AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE vM AS MEMPTR     NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  IF pRetry <= 0 OR pRetry = ? THEN pRetry = 1.
  IF NOT IsEmpty(pOptions) THEN pMess = pMess + BICQDelim + pOptions.
  pMess = pMess + BICQDelim.
  SET-SIZE(vM) = LENGTH(pMess) + 100.
  PUT-STRING(vM,1) = pMess.

  CREATE SOCKET vH.
  LOOP:
  DO vI = 1 TO pRetry:
    vH:CONNECT((IF NOT IsEmpty(pHost) THEN "-H " + pHost ELSE "") + " -S " + pPort) NO-ERROR.
    IF vH:CONNECTED() THEN LEAVE LOOP.
  END.
  IF vH:CONNECTED() THEN
  DO:
    vH:WRITE(vM,1,LENGTH(pMess)).
    vH:DISCONNECT().
    vL = YES.
  END.
  DELETE OBJECT vH.
  SET-SIZE(vM) = 0.
  RETURN vL.
END.

FUNCTION GetMessageText RETURN CHAR (pText AS CHAR):
  RETURN ENTRY(3,pText,BICQDelim).
END.

FUNCTION GetAddresat RETURN CHAR (pText AS CHAR):
  RETURN ENTRY(2,pText,BICQDelim).
END.

FUNCTION GetAddresatID RETURN CHAR (pUser AS CHAR):
  RETURN SUBSTR(ENTRY(2,pUser,"["),1,LENGTH(ENTRY(2,pUser,"[")) - 1).
END.

FUNCTION GetDateTime RETURN CHAR (pText AS CHAR):
  RETURN ENTRY(4,pText,BICQDelim).
END.

FUNCTION GetMessageID RETURN INT64 (pText AS CHAR):
  DEFINE VARIABLE vID AS INT64    NO-UNDO.
  vID = INT64(ENTRY(5,pText,BICQDelim)) NO-ERROR.
  IF vID = 0 THEN
  DO:
    {&BICQMess}"bicq01","","%s=" + ENTRY(5,pText,BICQDelim)). 
    /*"Сообщение имеет неверный идентификатор [ ... ]."*/
    RETURN ?.
  END.
  RETURN vID.
END.

FUNCTION GetMessageDate RETURN DATE (pText AS CHAR):
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  vS = GetDateTime(pText).
  RETURN DATE(ENTRY(1,vS," ")).
END.

FUNCTION BICQAddresat RETURN CHAR (pUserID AS CHAR):
  RETURN GetValueByQuery("_user","_user-name","_userid = '" + pUserID + "'") + 
    " [" + CAPS(pUserID) + "]".
END.

FUNCTION GetTOAddresat RETURN CHAR (pID AS INT64):
  RETURN BICQAddresat(GetValueByQuery("packet","user-id","PacketId = " + STRING(pID))).
END.

FUNCTION GetMessageOptions RETURN CHAR (pText AS CHAR):
  RETURN (IF NUM-ENTRIES(pText,BICQDelim) > BICQNumEntries THEN ENTRY(6,pText,BICQDelim) ELSE ?).
END.

/* Преобразовывает список BICQ адресатов в список обычных пользователей*/
FUNCTION GetUserList RETURNS CHAR (pBICQUserList AS CHAR):
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DEFINE VARIABLE vUserList AS CHARACTER  NO-UNDO.
  DO vI = 1 TO NUM-ENTRIES(pBICQUserList):
    vUserList = vUserList + (IF vUserList = "" THEN "" ELSE ",") + GetAddresatID(ENTRY(vI,pBICQUserList)).
  END.
  RETURN vUserList.
END.

FUNCTION GetMessageTime RETURN INT64 (pText AS CHAR):
  DEFINE VARIABLE vC AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vH AS INT64    NO-UNDO.
  DEFINE VARIABLE vM AS INT64    NO-UNDO.
  DEFINE VARIABLE vS AS INT64    NO-UNDO.

  vC = GetDateTime(pText).
  vC = ENTRY(2,vC," ").
  vH = INT64(ENTRY(1,vC,":")).
  vM = INT64(ENTRY(2,vC,":")).
  vS = INT64(ENTRY(3,vC,":")).
  RETURN vS + vM * 60 + vH * 3600.
END.

FUNCTION SaveMessage RETURN INT64 (pText AS CHAR,pUser AS CHAR,pStatus AS CHAR):
  DEFINE VARIABLE vStat AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE vID AS INT64    NO-UNDO.
  
  vID = CreateSimpleMessage(BICQMessID + "," + BICQAddresat(USERID("bisquit")) + "," + pUser + "," + pStatus ,pText).
  IF vID = ? THEN {&BICQMess}"bicq06","",""). 
                  /*"Сообщение не было сохранено."*/
  RETURN IF vID <> ? THEN vID ELSE -1.
END.

FUNCTION BICQCheckMessages RETURN INT64:
  RETURN CountObjects("packet","mail-format = '" + BICQMessID + "' and user-id = '" + USERID("bisquit") + 
                    "' and lookup(state,'" + {&NotSent} + "," + {&Sent} + "') > 0").
END.

FUNCTION BICQChangeStatus RETURN LOG (pID AS INT64,pStatus AS CHAR):
  DEFINE VARIABLE vL AS LOGICAL    NO-UNDO.
  vL = ChangeMsgStatus(pID,pStatus).
  IF NOT vL THEN {&BICQMess}"bicq07","","&s=" + STRING(pID)). 
        /*"Статус сообщения с идентификатором " + STRING(pID) + " не изменен."*/
  RETURN vL.
END.

FUNCTION ConvertPacket RETURN CHAR (pID AS INT64):
  DEFINE VARIABLE vStr AS CHARACTER  NO-UNDO.
  vStr = ConvertMsg2Str(pID,BICQDelim).
  IF vStr = ? THEN
  DO:
    {&BICQMess}"bicq08","","&s=" + STRING(pID)). 
    /*"Сообщение с идентификатором " + STRING(pID) + " не было сохранено."*/
    RETURN vStr.
  END.
  RETURN BICQHeader + BICQDelim + vStr.
END.

FUNCTION BICQSendMessage RETURN INT64 (pUser AS CHAR,pComp AS CHAR, pMess AS CHAR, pOptions AS CHAR):
  DEFINE VARIABLE vPorts AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DEFINE VARIABLE vJ AS INT64    NO-UNDO.
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vTotal AS INT64    NO-UNDO.
  DEFINE VARIABLE vPacket AS CHARACTER  NO-UNDO.
  DEFINE VAR vUser AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vID AS INT64    NO-UNDO.
  DEFINE VARIABLE vL AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE vOkPorts AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vHostList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vHost AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vPortNumber AS CHARACTER  NO-UNDO.

    
  vUser = GetAddresatID(pUser).
  vPorts = getUserXAttrValue(vUser,"PortNum").
  vID = SaveMessage(pMess,vUser,{&NotSent}).

  IF vID = -1 THEN RETURN -1.
  IF NOT gRemote THEN
     RUN SetDialog("Отправка сообщения пользователю " + CAPS(pUser),"BICQ").

  vPacket = ConvertPacket(vID).
  IF vPacket = ? THEN 
  DO:
    IF NOT gRemote THEN
       RUN DelDialog.
    RETURN -1.
  END.
  
  IF IsEmpty(pComp) THEN
  DO:
    pComp = "".
    vHostList = GetValueByQueryEx("_connect","_connect-type,_connect-device",
                                  "_connect-name = '" + vUser + "' AND LOOKUP(_connect-type,'remc,self') > 0",",").
    DO vI = 1 TO NUM-ENTRIES(vHostList) - 1 BY 2:
      vS = IF ENTRY(vI,vHostList) = "remc" THEN ENTRY(vI + 1,vHostList) ELSE GetHostName().
      IF IsEmpty(vS) THEN NEXT.
      IF LOOKUP(vS,pComp) > 0 THEN NEXT.
      pComp = pComp + (IF pComp = "" THEN "" ELSE ",") + vS.
    END.
  END.
  
  IF IsEmpty(vPorts) THEN 
  DO:
    IF NOT IsEmpty(pOptions) THEN RUN TreatSendOptions(pOptions,pComp,pMess).
    IF NOT gRemote THEN
       RUN DelDialog.
    RETURN 0.
  END.

  DO vI = 1 TO NUM-ENTRIES(vPorts):
    vS = ENTRY(vI,vPorts).
    IF NUM-ENTRIES(vS,";") = 3 AND vS BEGINS "QBIS_" THEN
    DO:
       /*solm: шлем сообщение клиенту QBIS*/
       IF OpenSox(ENTRY(2,vS,";"),ENTRY(3,vS,";"),vPacket,1,pOptions) THEN
       DO:
          vTotal = vTotal + 1.
       END.
    END.
    ELSE
    DO:
       DO vJ = 1 TO NUM-ENTRIES(pComp):
         /*solm: шлем сообщение клиенту Бисквит*/
         IF OpenSox(ENTRY(vJ,pComp),ENTRY(1,vS,";"),vPacket,1,pOptions) THEN
         DO:
            vTotal = vTotal + 1.
         END.
       END.
    END.
  END.
  
  IF vTotal > 0 THEN
  DO:
    vL = BICQChangeStatus(vID,{&Sent}).
    IF NOT vL THEN
    DO:
      IF NOT gRemote THEN
         RUN DelDialog.
      RETURN -1.
    END.
  END.
  IF NOT IsEmpty(pOptions) THEN RUN TreatSendOptions(pOptions,pComp,pMess).
  IF NOT gRemote THEN
     RUN DelDialog.
  RETURN vTotal.
END.

PROCEDURE BICQTreat:
  DEFINE INPUT  PARAMETER pText AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE vMode AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vL AS LOGICAL INIT YES NO-UNDO.
  DEFINE VARIABLE vTitle AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  
  vH = FOCUS.
  
  BELL.
  vMode = getThisUserXAttrValue("TreatMessage").
  RUN MarkHeader.
  
  IF GetMessageOptions(pText) = {&HardSend} THEN vMode = "666".
  
  /*plus.vvv*/
  IF GetMessageOptions(pText) = "currency" THEN vMode = "currency".
  /**/
  
  /* To prevent BICQ modules rerun */
  IF vMode = "2" AND BICQRun("") THEN vMode = "3".
    
  CASE vMode:
    WHEN "1" THEN 
    DO:
    END.
    WHEN "2" THEN
    DO:
      MESSAGE "Вам поступило сообщение от пользователя " + GetAddresat(pText) SKIP
        "Открыть сообщение?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vL.
      IF vL THEN RUN bicqin.p(pText,1).
    END.
    WHEN "666" THEN
    DO:
      vTitle = "Сообщение от пользователя " + GetAddresat(pText).
      MESSAGE STRING("ОЧЕНЬ ВАЖНОЕ СООБЩЕНИЕ:","x(" + STRING(LENGTH(vTitle) + 2) + ")")  SKIP GetMessageText(pText)
        VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE vTitle.
      BICQChangeStatus(GetMessageID(pText),{&READ}).
      IF BICQCheckMessages() = 0 THEN RUN RemHeaderMark.
    END.
	/**/
	WHEN "currency" THEN
		DO:
			RUN ChangeRateCur(GetMessageText(pText)).
		END.
	/**/	
    OTHERWISE
    DO:
      RUN SetDialog("Вам поступило сообщение от пользователя " + GetAddresat(pText),"BICQ").
      PAUSE 3 NO-MESSAGE.
      RUN DelDialog.
    END.
  END CASE.
  APPLY "U5" TO CURRENT-WINDOW.
  APPLY "entry" TO vH.
END.

PROCEDURE MarkHeader:
  PUT SCREEN COLOR BLINK-BRIGHT-RED COL 77 ROW 2 "BICQ".
END.

PROCEDURE RemHeaderMark:
  PUT SCREEN COLOR BLACK/GRAY COL 77 ROW 2 dept.logo[2].
END.

FUNCTION AddUserPort RETURN LOGICAL (pHost AS CHAR, pPortNum AS CHAR, INPUT-OUTPUT TABLE FOR tUser BIND):
   IF OpenSox(pHost, pPortNum, "ping", 1, ?) THEN
   DO:
     FIND FIRST tUser WHERE tUser.fHost = pHost NO-ERROR.
     IF NOT AVAIL tUser THEN
     DO:
       CREATE tUser.
       tUser.fHost = CAPS(pHost).
     END.
     IF LOOKUP(pPortNum, tUser.fPort, " ") > 0 THEN RETURN FALSE.
     tUser.fPort = tUser.fPort + (IF IsEmpty(tUser.fPort) THEN "" ELSE " ") + pPortNum.
     RETURN TRUE.
   END.
   RETURN FALSE.
END FUNCTION.
                
FUNCTION BICQDetect RETURN INT64 (pUser AS CHAR,pMode AS CHAR,OUTPUT TABLE FOR tUser):
  DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vNewVS AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DEFINE VARIABLE vJ AS INT64    NO-UNDO.
  DEFINE VARIABLE vH AS HANDLE     NO-UNDO.
  DEFINE VARIABLE vHost AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vPorts AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vPortNum AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vCurrentPort AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vTotal AS INT64    NO-UNDO.
  DEFINE VARIABLE vHostList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vTimeoutTime AS DATETIME NO-UNDO.
  DEFINE VARIABLE vConnType AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vPingNeeded AS LOGICAL  NO-UNDO.
  
  RUN SetDialog("Проверка сетевых подключений...","BICQ").
  EMPTY TEMP-TABLE tUser.

  TR:
  DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
    ON STOP UNDO TR,LEAVE TR:
    IF pMode = "LOCK" THEN
    DO:
      IF NOT SetLockByQuery("signs","file-name = '_user' and surrogate = '" + pUser + "' and code = 'PortNum'") THEN
        UNDO TR,LEAVE TR.
    END.

    /* solm: Таймаут {&TIMEOUT_BICQ_DETECT} определяет интервал времени,
    ** в течение которого идет проверка всех BICQ портов текущего пользователя.
    ** В результате работы функции  из дополнительного параметра "PortNum" 
    ** будут удалены те недействительные порты, которые будут проверены. 
    ** В итоге в "PortNum" останутся действительные проверенные порты и те порты, 
    ** которые не успеем проверить. Непроверенные порты будут проверяться при 
    ** следующем вызове BICQDetect */
    vTimeoutTime = ADD-INTERVAL(NOW,{&TIMEOUT_BICQ_DETECT}, "seconds").

    vS = getUserXAttrValue(pUser,"PortNum").
    IF IsEmpty(vS) THEN UNDO TR,LEAVE TR.
    vHostList = GetValueByQueryEx("_connect","_connect-type,_connect-device",
                                  "_connect-name = '" + pUser + "' AND LOOKUP(_connect-type,'remc,self') > 0",",").

    DO vI = 1 TO NUM-ENTRIES(vS):
      vCurrentPort = ENTRY(vI, vS).
      IF NUM-ENTRIES(vCurrentPort,";") = 3 AND vCurrentPort BEGINS "QBIS_" THEN
      DO:
        /*solm: выполняем проверку портов QBIS */
        vHost = ENTRY(2, vCurrentPort, ";").
        vPortNum = ENTRY(3, vCurrentPort, ";").
        IF  NOW > vTimeoutTime OR AddUserPort(vHost, vPortNum, INPUT-OUTPUT TABLE tUser) = TRUE THEN 
        DO:
          IF LOOKUP(vPortNum, vPorts) > 0 THEN NEXT.
          vPorts = vPorts + (IF vPorts = "" THEN "" ELSE ",") + vCurrentPort.
        END.
      END.
      ELSE
      DO:
        /*solm: Порт не QBIS, помещаем его в отдельный список*/
        vNewVS = AddPortNum(vNewVS, vCurrentPort).
      END.
    END.

    vS = vNewVS.
    /*solm: Выполняем проверку оставшихся портов*/
    DO vJ = 1 TO NUM-ENTRIES(vHostList) - 1 BY 2:
      vConnType = ENTRY(vJ,vHostList).
      vHost = IF vConnType EQ "SELF" THEN GetHostName() ELSE ENTRY(vJ + 1,vHostList).
      DO vI = 1 TO NUM-ENTRIES(vS):
         vCurrentPort = ENTRY(vI,vS).
                        /* Требуется ли проверка коннекта через текущий порт */
         vPingNeeded = NOT (vConnType EQ "SELF"
                        AND NUM-ENTRIES (vCurrentPort, ";") GT 1
                        AND NOT CAN-FIND (FIRST _connect WHERE _connect._Connect-Pid EQ INT64 (ENTRY (2,vCurrentPort,";")))).
        IF     NOW > vTimeoutTime    /* таймаут - время на проверку портов истекло */
           OR (vPingNeeded
           AND AddUserPort(vHost, ENTRY(1,vCurrentPort,";"), INPUT-OUTPUT TABLE tUser)) THEN 
        DO:
          IF LOOKUP(vCurrentPort, vPorts) > 0 THEN NEXT.
          vPorts = vPorts + (IF vPorts = "" THEN "" ELSE ",") + vCurrentPort.
        END.
      END.
    END.
    IF pMode = "LOCK" THEN
       UpdateSigns("_user",pUser,"PortNum",vPorts, YES).
  END.  /* End of TR BLOCK */
  RUN DelDialog.

  FOR EACH tSends:
    vTotal = vTotal + 1.
  END.

  RETURN vTotal.
END.

PROCEDURE ResetAllServers:
  DEFINE VARIABLE vHList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DEFINE VARIABLE vH AS HANDLE     NO-UNDO.

  vH = SESSION:FIRST-SERVER-SOCKET.

  DO WHILE VALID-HANDLE(vH):
    IF vH:PRIVATE-DATA = BICQMessID THEN
    DO:
      vHList = vHList + (IF vHList = "" THEN "" ELSE ",") + STRING(vH).
    END.
    vH = vH:NEXT-SIBLING.
  END.

  DO vI = 1 TO NUM-ENTRIES(vHList):
    vH = WIDGET-HANDLE(ENTRY(vI,vHList)).
    vH:DISABLE-CONNECTIONS().
    DELETE OBJECT vH.
  END.
END.

PROCEDURE TreatSendOptions:
  DEFINE INPUT  PARAMETER pOptions AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pComp AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pMess AS CHARACTER  NO-UNDO.
  
  CASE pOptions:
    WHEN {&HardSend} THEN RUN HardSend(pComp,pMess).
  END CASE.
END.

PROCEDURE HardSend:
  DEFINE INPUT  PARAMETER pComp AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pMess AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  pMess = CODEPAGE-CONVERT(" ~"Сообщение от " + BICQAddresat(USERID("bisquit")) + ": " + pMess + "~"","1251").
  DO vI = 1 TO NUM-ENTRIES(pComp):
    IF OPSYS = "WIN32" THEN
    DO:
      OS-COMMAND SILENT VALUE("net send " + ENTRY(vI,pComp) + pMess).
    END.
  END.
END.

PROCEDURE BICQGroupSend:
  DEFINE INPUT  PARAMETER pUserList AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pComp AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pMess AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pOptions AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER TABLE FOR tSends. 

  DEFINE VARIABLE vI AS INT64    NO-UNDO.
  DEFINE VARIABLE vAddr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vRes AS INT64    NO-UNDO.

  IF pUserList = "*" THEN pUserList = GetValueByQueryEx("_user","_userid","",",").

  EMPTY TEMP-TABLE tSends.

  DO vI = 1 TO NUM-ENTRIES(pUserList):
    vAddr = BICQAddresat(ENTRY(vI,pUserList)). 
    IF IsEmpty(vAddr) THEN
    DO:
      FIND FIRST tSends WHERE tSends.fUser = ENTRY(vI,pUserList) NO-ERROR.
      IF NOT AVAIL tSends THEN 
      DO:
        CREATE tSends.
        tSends.fUser = ENTRY(vI,pUserList).
      END.
      tSends.fErr = tSends.fErr + 1.
      NEXT.
    END.
    FIND FIRST tSends WHERE tSends.fUser = vAddr NO-ERROR.
    IF NOT AVAIL tSends THEN
    DO:
      CREATE tSends.
      tSends.fUser = vAddr.
    END.
    vRes = BICQSendMessage(vAddr,pComp,pMess,pOptions).
    IF vRes > 0 THEN tSends.fOnline = tSends.fOnline + vRes.
    IF vRes = 0 THEN tSends.fOffline = tSends.fOffline + 1.
    IF vRes < 0 THEN tSends.fErr = tSends.fErr + 1.
  END.
END.

/*----------------------------------------------------------------*/
PROCEDURE MyGroupSendMess:
	DEFINE INPUT  PARAMETER pUserList AS CHARACTER  NO-UNDO.
	DEFINE INPUT  PARAMETER pComp AS CHARACTER  NO-UNDO.
	DEFINE INPUT  PARAMETER pMess AS CHARACTER  NO-UNDO.
	DEFINE INPUT  PARAMETER pOptions AS CHARACTER  NO-UNDO.
	/**/	
	DEFINE VARIABLE vI AS INT64 NO-UNDO.
	DEFINE VARIABLE vS AS CHARACTER  NO-UNDO.
	DEFINE VARIABLE vJ AS INT64    NO-UNDO.
	DEFINE VARIABLE vK AS INT64    NO-UNDO.
	DEFINE VAR vUser AS CHARACTER  NO-UNDO.
	DEFINE VARIABLE vAddr AS CHARACTER NO-UNDO.
	DEFINE VARIABLE vRes AS INT64 NO-UNDO.
	DEFINE VARIABLE vID AS INT64    NO-UNDO.
	DEFINE VARIABLE vTotal AS INT64    NO-UNDO.
	DEFINE VARIABLE vHostList AS CHARACTER  NO-UNDO. 
	DEFINE VARIABLE vPacket AS CHARACTER  NO-UNDO.
	DEFINE VARIABLE vPorts AS CHARACTER  NO-UNDO.	
	DEFINE VARIABLE vL AS LOGICAL    NO-UNDO.
	/**/
	
	vID = CreateSimpleMessage(BICQMessID + "," + BICQAddresat(USERID("bisquit")) + "," + USERID("bisquit") + "," + {&READ} ,pMess).
	
	vPacket = ConvertPacket(vID).
	/*
	vL = ChangeMsgStatus(vID,pStatus).
	*/
	
	/* определимся со списком пользователей */
	IF pUserList = "*" THEN 
		pUserList = GetValueByQueryEx("_user","_userid","",",").
	
	/* определимся с хостом */
	IF IsEmpty(pComp) THEN
		DO:
			pComp = "".
			vHostList = GetValueByQueryEx("_connect","_connect-type,_connect-device",
										  "_connect-name = '" + USERID("bisquit") + "' AND LOOKUP(_connect-type,'remc,self') > 0",",").
			DO vI = 1 TO NUM-ENTRIES(vHostList) - 1 BY 2:
			  vS = IF ENTRY(vI,vHostList) = "remc" THEN ENTRY(vI + 1,vHostList) ELSE GetHostName().
			  IF IsEmpty(vS) THEN NEXT.
			  IF LOOKUP(vS,pComp) > 0 THEN NEXT.
			  pComp = pComp + (IF pComp = "" THEN "" ELSE ",") + vS.
			END.
		END.		
	/*  */
	
	DO vI = 1 TO NUM-ENTRIES(pUserList):
		/* найдем порт */
		vPorts = getUserXAttrValue(ENTRY(vI,pUserList),"PortNum").
		/* по всем портам */
		DO vK = 1 TO NUM-ENTRIES(vPorts):
			vS = ENTRY(vK,vPorts).
			/**/
			DO vJ = 1 TO NUM-ENTRIES(pComp):
			 /*solm: шлем сообщение клиенту Бисквит*/
			 IF OpenSox(ENTRY(vJ,pComp),ENTRY(1,vS,";"),vPacket,1,pOptions) THEN
				 DO:
					vTotal = vTotal + 1.
				 END.
			END.
		END.
	END.		
	
END.
/*----------------------------------------------------------------*/
PROCEDURE ChangeRateCur:
  DEFINE INPUT  PARAMETER sText AS CHARACTER  NO-UNDO.
  

  DEF VAR HeadStr	AS CHAR INIT "|   Валюта    |  Тип курса  |  Время  |  Значение  |" NO-UNDO.
  DEF VAR Delt 		AS CHAR INIT "|-------------|-------------|---------|------------|" NO-UNDO.
	
  DEF VAR jjSTR		AS CHAR INIT "" NO-UNDO.
  DEF VAR vK 		AS INT64    NO-UNDO.
  
  jjSTR = Delt + "\n" + HeadStr + "\n" + Delt.
  
  /* сформируем строку вывода */
  DO vK = 1 TO NUM-ENTRIES(sText, "@") - 1:
 	jjSTR = jjSTR + "\n" + ENTRY(vK,sText,"@") + "\n" + Delt.
	/*
	jjSTR = jjSTR + "\n" + (IF ENTRY(vK,sText,"@") = "" THEN ENTRY(vK,sText,"@") + "\n" + Delt ELSE "").
	*/
  END.

  MESSAGE 
	COLOR Magenta
	jjSTR
	VIEW-AS ALERT-BOX
	TITLE "Изменение курсов валют".	
  /*
	MESSAGE
		COLOR Magenta
		SKIP
		HeadStr + "\n" + Delt + "\n"
		ENTRY(1,sText,"@")
		SKIP
		Delt
		SKIP
		ENTRY(2,sText,"@")
		SKIP
		VIEW-AS ALERT-BOX
		TITLE "Изменение курсов валют".
	*/
	
		
END.		
/*----------------------------------------------------------------*/