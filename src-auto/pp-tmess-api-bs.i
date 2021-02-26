/*
    Copyright: (C) 2004 Dmitry Malinovsky (MDY)
     Filename: pp-tmess-api-bs.i 
      Comment: ��楤��� � �㭪樨 ������-������ �㦡� ��⥬��� ᮮ�饭��.
   Parameters:
         Uses:
      Used by:
      Created: 04.11.2004 13:09 MDY     
     Modified: 04.11.2004 13:09 MDY     
     Modified: 12.05.2005 14:40 KSV      (0044952) ��������� ��楤���
                                         PresetAnswers � GetAnswers.
     Modified: 06.08.2006 Om  �訡��.
                        �㡫�஢����� �⢥�� �� ������.
*/

/* �����頥� Return-value + �⥪ ᮮ�饭��  Progress 
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

/* �ᯮ�짮���� ��� ������ �訡�� �� �㭪樨 ��᫥ ������ NO-ERROR */
PROCEDURE ReturnError .
   DEFINE INPUT PARAMETER iErrStr AS CHAR.
   RETURN ERROR iErrStr.
END PROCEDURE.

/* ��楤�� ��⠭���� ��ࠬ��஢ ᮮ�饭��. */
PROCEDURE SetMsgPrm.
   DEF INPUT PARAM iMsgId  AS CHAR NO-UNDO. /* ��� ᮮ�饭��. */
   DEF INPUT PARAM iPrm    AS CHAR NO-UNDO. /* ��� ��ࠬ���. */
   DEF INPUT PARAM iValue  AS CHAR NO-UNDO. /* ���祭�� ��ࠬ���. */

   DEF BUFFER tt-MsgSet    FOR tt-MsgSet. /* ���������� ����. */
   DEF BUFFER Alt-MsgSet   FOR tt-MsgSet. /* ��� ����ୠ⨢���� �⢥�. */
   DEF BUFFER Dubl-MsgSet  FOR tt-MsgSet. /* ��� ���᪠ �㡫���� �⢥�. */

                     /* ���� �������㠫쭮�� ��������� ᮮ�饭�� � �����.  */
   FIND FIRST tt-MsgSet WHERE
            tt-MsgSet.Proc-Id    EQ tt-ProcMes.Proc-Id
      AND   tt-MsgSet.Mes-Code   EQ iMsgId
   NO-ERROR.
                     /* ������� ���ᮭ���� ��� ����ன��. */
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
                     /* ���� �㡫����. */
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
  Purpose:     ��⠭�������� �⢥�� ���짮��⥫� �� ���. ᮮ�饭�� ��� ⥪�饣�
               �����
  Parameters:  iPresets - �⢥�� �� ���. ᮮ�饭��
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
  Purpose:     �����頥� �⢥�� ���짮��⥫� �� ���. ᮮ�饭��
  Parameters:  oPresets - �⢥�� �� ���. ᮮ�饭��
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
/* ����祭�� ����ன�� ����� �� ���� ����ன��.                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetProcSettingByCode RETURNS CHARACTER (
   INPUT iCode AS CHARACTER    /* ��� ����ன�� */
):
   CASE iCode:
      WHEN "��_�뢮�����࠭"  THEN RETURN vToScreen.
      WHEN "��_�뢮������"    THEN RETURN vToFile.
      WHEN "��_�뢮���⪫"    THEN RETURN vViewLog.
      WHEN "��_��������⪫"  THEN RETURN vDelLog.
      WHEN "��_������⪫"     THEN RETURN vLogFile.
      WHEN "��_��⮎⢥�"     THEN RETURN vAnswers.
      WHEN "��_�஢���⫠��" THEN RETURN STRING(vDebugLev).
      WHEN "��_������������"  THEN RETURN vMesTypeClassCode.
      OTHERWISE RETURN ?.
   END CASE.
END FUNCTION.
