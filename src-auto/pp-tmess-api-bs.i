/*
    Copyright: (C) 2004 Dmitry Malinovsky (MDY)
     Filename: pp-tmess-api-bs.i 
      Comment: процедуры и функции бизнес-логики службы системных сообщений.
   Parameters:
         Uses:
      Used by:
      Created: 04.11.2004 13:09 MDY     
     Modified: 04.11.2004 13:09 MDY     
     Modified: 12.05.2005 14:40 KSV      (0044952) Добавлены процедуры
                                         PresetAnswers и GetAnswers.
     Modified: 06.08.2006 Om  Ошибка.
                        Дублировались ответы на вопросы.
*/

/* Возвращает Return-value + стек сообщений  Progress 
** Used in : WAS-ERR.I */
FUNCTION GetErrMsg RETURNS CHAR ():
    
    DEF VAR vErrCnt AS INT64 INITIAL 0 NO-UNDO.
    DEF VAR vLocErrStr AS CHAR INITIAL 0 NO-UNDO.
   
    vLocErrStr  = PROGRAM-NAME(2) + "<-" .

    IF RETURN-VALUE <> "" AND 
       RETURN-VALUE <> ? 
    THEN vLocErrStr = vLocErrStr + 
                   RETURN-VALUE + 
                   CHR(10) .
    
    DO vErrCnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
        IF INDEX(vLocErrStr, ERROR-STATUS:GET-MESSAGE(vErrCnt)) = 0 THEN
        vLocErrStr  = vLocErrStr + 
                   CHR(10) + 
                   ERROR-STATUS:GET-MESSAGE(vErrCnt).
        IF vErrCnt > 8  THEN LEAVE.
    END.
    RETURN vLocErrStr.
    
END.

/* Использовать для возврата ошибки из функции после оператора NO-ERROR */
PROCEDURE ReturnError .
   DEFINE INPUT PARAMETER iErrStr AS CHAR.
   RETURN ERROR iErrStr.
END PROCEDURE.

/* Процедура установки параметров сообщения. */
PROCEDURE SetMsgPrm.
   DEF INPUT PARAM iMsgId  AS CHAR NO-UNDO. /* Код сообщения. */
   DEF INPUT PARAM iPrm    AS CHAR NO-UNDO. /* Код параметра. */
   DEF INPUT PARAM iValue  AS CHAR NO-UNDO. /* Значение параметра. */

   DEF BUFFER tt-MsgSet    FOR tt-MsgSet. /* Локализация буфера. */
   DEF BUFFER Alt-MsgSet   FOR tt-MsgSet. /* Для альтернативного ответа. */
   DEF BUFFER Dubl-MsgSet  FOR tt-MsgSet. /* Для поиска дубликата ответа. */

                     /* Поиск индивидуального поведения сообщения в процессе.  */
   FIND FIRST tt-MsgSet WHERE
            tt-MsgSet.Proc-Id    EQ tt-ProcMes.Proc-Id
      AND   tt-MsgSet.Mes-Code   EQ iMsgId
   NO-ERROR.
                     /* Создаем персональный код настройки. */
   IF NOT AVAIL tt-MsgSet
      AND iPrm EQ "MsgAnsw"
   THEN DO:
      CREATE tt-MsgSet.
      ASSIGN
         tt-MsgSet.Proc-Id    = tt-ProcMes.Proc-Id
         tt-MsgSet.Mes-Code   = iMsgId
         tt-MsgSet.MsgAnsw    = iValue
      .
   END.
   ELSE DO:
                     /* Поиск дубликата. */
      FIND FIRST Dubl-MsgSet WHERE
               Dubl-MsgSet.Proc-Id  EQ tt-MsgSet.Proc-Id
         AND   Dubl-MsgSet.Mes-Code EQ tt-MsgSet.Mes-Code
         AND   Dubl-MsgSet.MsgAnsw  EQ iValue
      NO-ERROR.
      IF NOT AVAIL Dubl-MsgSet
      THEN DO:
         CREATE Alt-MsgSet.
         ASSIGN
            Alt-MsgSet.Proc-Id   = tt-ProcMes.Proc-Id
            Alt-MsgSet.Mes-Code  = iMsgId
            Alt-MsgSet.MsgAnsw   = iValue
            Alt-MsgSet.Mes-Order = tt-MsgSet.Mes-Order + 1
         .
      END.
   END.
   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Устанавливает ответы пользователя на сист. сообщения для текущего
               процесса
  Parameters:  iPresets - ответы на сист. сообщения
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE PresetAnswers:
   DEFINE INPUT  PARAMETER iPresets AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vNum       AS INT64    NO-UNDO.
   DEFINE VARIABLE vCnt       AS INT64    NO-UNDO.
   DEFINE VARIABLE vMessage   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAnswer    AS CHARACTER  NO-UNDO.

   vNum = NUM-ENTRIES(iPresets,{&PRST_MSGS_DELIM}).
   DO vCnt = 1 TO vNum:
      vMessage = ENTRY(vCnt,iPresets,{&PRST_MSGS_DELIM}).
      IF NUM-ENTRIES(vMessage,{&PRST_ANSW_DELIM}) <> 2 THEN NEXT.
      vAnswer  = ENTRY(2,vMessage,{&PRST_ANSW_DELIM}).
      vMessage = ENTRY(1,vMessage,{&PRST_ANSW_DELIM}).
      RUN SetMsgPrm (vMessage, "MsgAnsw", vAnswer).
   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Возвращает ответы пользователя на сист. сообщения
  Parameters:  oPresets - ответы на сист. сообщения
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE GetAnswers:
   DEFINE OUTPUT PARAMETER oPresets AS CHARACTER  NO-UNDO.
   
   FOR EACH tt-MsgSet WHERE
      tt-MsgSet.Proc-Id EQ tt-ProcMes.Proc-id:
      oPresets = oPresets + (IF oPresets = "" THEN "" ELSE {&PRST_MSGS_DELIM}) +
         tt-MsgSet.Mes-Code + {&PRST_ANSW_DELIM} + tt-MsgSet.MsgAnsw.
   END.
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Получение настройки процесса по коду настройки.                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetProcSettingByCode RETURNS CHARACTER (
   INPUT iCode AS CHARACTER    /* Код настройки */
):
   CASE iCode:
      WHEN "СС_ВыводНаЭкран"  THEN RETURN vToScreen.
      WHEN "СС_ВыводВФайл"    THEN RETURN vToFile.
      WHEN "СС_ВыводПрткл"    THEN RETURN vViewLog.
      WHEN "СС_УдалятьПрткл"  THEN RETURN vDelLog.
      WHEN "СС_ФайлПрткл"     THEN RETURN vLogFile.
      WHEN "СС_АвтоОтвет"     THEN RETURN vAnswers.
      WHEN "СС_УровеньОтладк" THEN RETURN STRING(vDebugLev).
      WHEN "СС_КлТиповСообщ"  THEN RETURN vMesTypeClassCode.
      OTHERWISE RETURN ?.
   END CASE.
END FUNCTION.
