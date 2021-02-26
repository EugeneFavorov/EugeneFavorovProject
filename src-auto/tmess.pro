/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: TMESS.PRO
      Comment: ��楤��� ����䥩� ��⥬��� ᮮ�饭��
   Parameters:
         Uses:
      Used BY:
      Created: 21.08.2003 TSL
     Modified: 12.05.2005 14:39 KSV      (0044952) ��������� �����������
                                         ��������� �⢥�� ���짮��⥫� �� ���.
                                         ᮮ�饭�� � ����� ��⮪���஢����,
                                         � ��⥬ ��⮬���᪨ �ਬ����� �� ��
                                         ������������� ���. ᮮ�饭��.
     Modified: 19.05.2005 14:39 NIK      �뢮� ��⮪��� �� "���ࠬ".
     Modified: 08.06.2005 14:39 SADM     (0017074) ����������� �⪫�祭��
                                         ����� � ��࠭���� �⢥⮢ ��
                                         �࠭���樨 � �� ��_��⮎⢥�.
     Modified: 23.07.2005 Om  ��ࠡ�⪠.
                        �����祭 �ଠ� ���� ��� �뢮�� ����� ᮮ�饭��.
     Modified: 14.02.2006 Om  �訡��.
                        �����४⭮� �ࠢ����� ���ᠭ�ﬨ ����ᮢ.
     Modified: 29.09.2008 17:37 KSV      (0098571) ��������� �㭪��
                                         Fill-AlertSysMes.
     Modified: 30.09.2008 15:46 KSV      (0098571) ��ࠢ����� c PROGRAM-NAME
     Modified: 29.01.2009 17:52 buan     �뤠� ���⮢ �� ������� (_spoolm.tmp)
                                         � �ଠ� LANDSCAPE (126 ᨬ�����) ���
                                         QBIS (�� 0098399)
     Modified: 11.06.2010 17:56 ksv      (0129243) ��ࠢ���� ࠡ�� 
                                         Fill-AlertSysMes

   Init-SysMes       ���樠������
      ��ࠬ����:
         iProcBaseId    - �����⥫� �� ������ <��� ⠡����>,<��� �����>
         iProcName      - �������� ����� (����易⥫��,�᫨ ���� 㪠��⥫�)
         iMesClassCode  - ��� �����䨪��� ᮮ�饭��

   Fill-SysMes       �������� � �뢮� ᮮ�饭��
      ��ࠬ����:
         iAuthor     - �����䨪��� ������-�㦡�
         iMesCode    - ��� ᮮ�饭�� �� �ࠢ�筨��
         iMesType    - ��� ᮮ�饭�� (����易⥫�� �� ����稨 ����)
         iMesText    - ����� ᮮ�饭�� ��� ���祭�� ��� 蠡���� ᮮ�饭��
                       (����易⥫�� �� ����稨 ����)

   Fill-ProgressErr  ���࠭�� � �뢮��� �ண��ᮢ� ᮮ�饭�� �� �訡���
      ��ࠬ����:
         iAuthor     - �����䨪��� ������-�㦡�

   End-SysMes          �뢮� ��⮪���
   Log-AutorMes        �뢮� ��⮪��� �� ���ࠬ

*/
/* ���樠������ ����� ��⮪���஢����. */
PROCEDURE Init-SysMes.
   DEFINE INPUT PARAMETER iProcBaseId   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iProcName     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesClassCode AS CHARACTER NO-UNDO.

   DEF VAR vPClass      AS CHAR    NO-UNDO. /* ����� �����. */
   DEF VAR vPID         AS CHAR    NO-UNDO. /* �����䨪��� �����. */
   DEF VAR vProcBaseId  AS CHAR    NO-UNDO.
   DEF VAR vLogBIS      AS LOGICAL NO-UNDO.

   DEFINE BUFFER OldStack FOR tt-SettStack. /* ���������� ����. */
   DEFINE BUFFER op-kind  FOR op-kind.

   /* ��� �����䨪��� ᮮ�饭��. */
   vMesClassCode =   IF iMesClassCode GT ""
                        THEN iMesClassCode
                        ELSE vMesClassCodeDef.
   /* �᫨ ��⮪���஢���� ����� 㦥 ����,
   ** � ����室��� ᪮�४�ࢮ��� ���ᠭ�� ᢮���.*/
   IF vLogFlag
   THEN DO:
      /* ���� ���ᠭ�� �।��饣� ����. */
      FIND LAST OldStack NO-ERROR.
      IF AVAIL OldStack
      THEN DO:
         CREATE tt-SettStack.
         BUFFER-COPY OldStack EXCEPT OldStack.ProcDesc TO tt-SettStack.
         /* ���࠭塞 ���� ��� �����䨪��� ᮮ�饭��. */
         ASSIGN
            tt-SettStack.ProcDesc      = OldStack.ProcDesc + 1
            tt-SettStack.MesClassCode  = vMesClassCode
         .
      END.
      RETURN.
   END.
   IF (iProcBaseId EQ ""
   AND iProcName   EQ "")
    OR iProcBaseId EQ "AUTO,�������������"
      /* �����⥬�� ��⮪�� (⨯� ����� �������) */
   THEN
      vLogBIS = YES.
      /* ��⮪���஢���� ������ �����. */
   ELSE
      ASSIGN
         vLogFlag = YES
         vLogBIS  = NO
      .
   /* ���������� ��砫��� ��楤��� ����� */
   vCurProc = PROGRAM-NAME(2).
   ASSIGN
      /* ��।������ ����� �����. */
      vPClass  =  ENTRY(1, iProcBaseId)
      /* ��।������ �����䨪��� �����. */
      vPID     =  SUBSTRING(iProcBaseId, LENGTH(vPClass) + 2)
   .
   /* ��� ᮢ��饭�� � �।��騬� ����ன����
   ** ���४��㥬 ⥪�騩 ������. */
   IF  NUM-ENTRIES(iProcBaseId) EQ 1
   AND vPClass                  NE ""
   THEN ASSIGN
      vPID     = vPClass
      vPClass  = "op-kind"
   .
   /* ��ନࢮ���� �������� �����. */
   mProcName = iProcName.
   /* ����祭�� ����஥� �����. */
   RUN GetProcSetting (vPClass, vPID).
   /* �࠭����� �㦭� ��� �ନ஢���� ������������ �����. */
   FIND FIRST op-kind WHERE op-kind.op-kind EQ vPID NO-LOCK NO-ERROR.
   IF vLogBIS
   THEN DO:
      FIND FIRST tt-ProcMes WHERE
         tt-ProcMes.Proc-Id EQ 0
      NO-ERROR.
      IF NOT AVAILABLE tt-ProcMes
         THEN mProcName = "�����⥬�� �����.".
   END.

   IF    (vLogBIS
         AND NOT AVAILABLE tt-ProcMes)
      OR vLogFlag THEN
   DO:
      /* �������� ����� � ����� */
      CREATE tt-ProcMes.
      ASSIGN tt-ProcMes.Proc-Id      = (IF vLogBIS THEN 0 ELSE SOURCE-PROCEDURE:UNIQUE-ID)
             tt-ProcMes.Proc-Date    = TODAY
             tt-ProcMes.Proc-TimeBeg = TIME
             tt-ProcMes.Op-Kind      = IF AVAILABLE op-kind
                                          THEN op-kind.op-kind
                                          ELSE ""
             tt-ProcMes.Proc-Name    = mProcName.
      CREATE tt-SettStack.
      /* ���������� ����� ����� */
      ASSIGN
         tt-SettStack.Proc-Id    = tt-ProcMes.Proc-Id
         tt-SettStack.ProcDesc   = 1
         tt-SettStack.CurProc    = vCurProc
         tt-SettStack.ToScreen   = vToScreen
         tt-SettStack.ToFile     = vToFile
         tt-SettStack.DebugLev   = vDebugLev
         tt-SettStack.DelLog     = vDelLog
         tt-SettStack.LogFile    = vLogFile
         tt-SettStack.ViewLog    = vViewLog
         tt-SettStack.AutoAnswer = vAutoAnswer
         tt-SettStack.Answers    = vAnswers
         tt-SettStack.ProcHdl    = SOURCE-PROCEDURE:HANDLE
         tt-SettStack.Stck2Fl    = vStck2Fl
         /* �� ����⨨ ������ ����� ����塞 ���稪. */
         vMesNum                 = 0
         vLogProcMes             = YES
      .
   END.
   /* ���樠�����㥬 �⢥�� �� ���. ᮮ�饭�� */
   RUN PresetAnswers(vAnswers).
   /* �뢮� ��������� ����� � 䠩� ��⮪��� */
   RUN Log-ProcMes.
   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ����頥� ᮮ�饭�� � ��⥬�� ��� � �� ����室����� �뢮���
               ��� �� �࠭. 
  Parameters:  iAuthor   - ��� �����, �㡫����饣� ᮮ�饭�� 
                           ("" - ����� �� 㬮�砭��)
               iMesCode  - ��� ᮮ�饭�� � �����䨪��� ᮮ�饭�� (�����)
               iMesType  - ⨯ ᮮ�饭�� 
                           < 0 - �訡��
                             0 - �।�०�����
                           1,2 - ���ଠ樮��� ᮮ�饭��
                           3,4 - ������  
               iMesText  - ⥪�� ᮮ�饭��, ���� �᫨ 㪠��� iMesCode, ��ࠬ����
                           � ᮮ�饭�� 㪠������� � �����䨪���
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Fill-SysMes:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesText AS CHARACTER NO-UNDO.
   
   RUN Fill-SysMes-Basic(iAuthor,iMesCode,iMesType,iMesText,NO).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ����頥� ᮮ�饭�� � ��⥬�� ��� � ������ �뢮��� ��� �� �࠭.
               ����室��� ��� QBIS, � ����⢥ ������ ALERT-BOX 
  Parameters:  iAuthor   - ��� �����, �㡫����饣� ᮮ�饭�� 
                           ("" - ����� �� 㬮�砭��)
               iMesCode  - ��� ᮮ�饭�� � �����䨪��� ᮮ�饭�� (�����)
               iMesType  - ⨯ ᮮ�饭�� 
                           < 0 - �訡��
                             0 - �।�०�����
                           1,2 - ���ଠ樮��� ᮮ�饭��
                           3,4 - ������  
               iMesText  - ⥪�� ᮮ�饭��, ���� �᫨ 㪠��� iMesCode, ��ࠬ����
                           � ᮮ�饭�� 㪠������� � �����䨪���
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Fill-AlertSysMes:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesText AS CHARACTER NO-UNDO.
   
   RUN Fill-SysMes-Basic(iAuthor,iMesCode,iMesType,iMesText,YES).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ����頥� ᮮ�饭�� � ��⥬�� ��� � �� ����室����� �뢮���
               ��� �� �࠭.
  Parameters:  iAuthor   - ��� �����, �㡫����饣� ᮮ�饭�� 
                           ("" - ����� �� 㬮�砭��)
               iMesCode  - ��� ᮮ�饭�� � �����䨪��� ᮮ�饭�� (�����)
               iMesType  - ⨯ ᮮ�饭�� 
                           < 0 - �訡��
                             0 - �।�०�����
                           1,2 - ���ଠ樮��� ᮮ�饭��
                           3,4 - ������  
               iMesText  - ⥪�� ᮮ�饭��, ���� �᫨ 㪠��� iMesCode, ��ࠬ����
                           � ᮮ�饭�� 㪠������� � �����䨪���
               iMesShow  - yes - �ᥣ�� �뢮���� ᮮ�饭�� �� �࠭          
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE Fill-SysMes-Basic PRIVATE:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesText AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMesShow AS LOGICAL   NO-UNDO.
   
   DEF VAR vMesCode     AS CHAR     NO-UNDO.
   DEF VAR vMesText     AS CHAR     NO-UNDO.
   DEF VAR vMesDesc     AS CHAR     NO-UNDO.
   DEF VAR vMesType     AS CHAR     NO-UNDO.
   DEF VAR vproc-name   AS CHAR     NO-UNDO.
   DEF VAR vparam       AS CHAR     NO-UNDO.
   DEF VAR vI           AS INT64      NO-UNDO.
   DEF VAR vtmp_sh      AS CHAR     NO-UNDO.
   DEF VAR vtmp_cs      AS CHAR     NO-UNDO.
   DEF VAR vMenuRow     AS CHAR     NO-UNDO.
   DEF VAR vMenuTitle   AS CHAR     NO-UNDO.
   DEF VAR vMenuChoises AS CHAR     NO-UNDO.
   DEF VAR vPickValue   AS CHAR     NO-UNDO.
   DEF VAR hNewMess     AS HANDLE   NO-UNDO.
   DEF VAR vScreen      AS LOG      NO-UNDO. /* �ਧ��� �뢮�� ᮮ�饭�� �� �࠭. */
   

   ASSIGN
      vMesText     = ENTRY(1,iMesText,"|")
      vMesType     = iMesType
      vMenuChoises = IF NUM-ENTRIES(iMesText,"|") > 1
                        THEN ENTRY(2,iMesText,"|")
                        ELSE ""
      iMesText     = ENTRY(1,iMesText,"|")
      .

   RELEASE code.
   IF iMesCode > "" THEN
   DO:
      /* �⥭�� ⥪�� ᮮ�饭�� �� �����䨪��� ᮮ�饭��*/
      IF NUM-ENTRIES(iMesCode) > 1 THEN
      DO:
         vMesCode = ENTRY(2,iMesCode).
         /* �饬 �� 㪠������� �����䨪���� */
         FIND FIRST code WHERE code.class = ENTRY(1,iMesCode)
                           AND code.code  = vMesCode
            NO-LOCK NO-ERROR.
      END.
      ELSE
         vMesCode = iMesCode.

      IF NOT AVAILABLE code AND
         vMesClassCodeDef <> vMesClassCode THEN
         /* ... � �᫨ ����䨪���, 㪠����� �� ���樠����樨
            �⫨砥��� �� �����䨪��� �� 㬮�砭��, � �饬
            �� �����䨪����, 㪠������� �� ���樠����樨 */
         FIND FIRST code WHERE code.class = vMesClassCode
                           AND code.code  = vMesCode
            NO-LOCK NO-ERROR.

      IF NOT AVAILABLE code THEN
         /* �饬 �� �����䨪���� �� 㬮�砭�� */
         FIND FIRST code WHERE code.class = vMesClassCodeDef
                           AND code.code  = vMesCode
            NO-LOCK NO-ERROR.

      /* �뤠�� ᮮ�饭�� � ����୮� ���� �訡��. */
      IF NOT AVAILABLE code
      THEN DO:
         RUN Fill-SysMes ("iAuthor", "", "-1",
                          "���� ᮮ�饭�� '" + vMesCode +
                          "', ��� � �����䨪��� ����� ᮮ�饭��.").
         RETURN.
      END.

      ASSIGN
         vMesText = REPLACE(code.description[1],"~~n","~n")
         vMesDesc = code.description[2]
         vMesType = IF vMesType > "" THEN vMesType ELSE code.val.

      /* ��ࠡ�⪠ 蠡����� */
      IF INDEX(vMesText,"%") > 0 AND
         INDEX(iMesText,"%") > 0 THEN
      DO vI = 2 TO NUM-ENTRIES(iMesText,"%"):
         vtmp_sh = SUBSTRING(ENTRY(vI,iMesText,"%"),3).
         vtmp_cs = "%" + SUBSTRING(ENTRY(vI,iMesText,"%"),1,1).
         IF INDEX(vMesText,vtmp_cs) > 0 THEN
            SUBSTRING(vMesText,INDEX(vMesText,vtmp_cs),2) = vtmp_sh.
      END.

   END.
   ELSE vMesCode = "".

   /* ��ᥪ��� ᮮ�饭�� �� �஢�� �⫠��� � �� �᫮���, ��
   ** �몫�祭 ��易⥫�� ����� ᮮ�饭�� */
   IF NOT iMesShow AND 
      INT64(vMesType) GT vDebugLev AND 
      NOT CAN-DO ("3,4", vMesType)
      THEN RETURN.

   /* ����� ᮮ�饭�� */
   vMesNum = vMesNum + 1.

   /* ������塞 � ᮮ�饭�� ��� ᮮ�饭�� */
   IF iMesCode > "" AND
      INT64(vMesType) < 3 THEN
      vMesText = vMesText + " (" + iMesCode + ")".

   /* �������� ����� ᮮ�饭�� */
   CREATE tt-SysMes.
   ASSIGN tt-SysMes.Proc-Id    = tt-ProcMes.Proc-Id
          tt-SysMes.Mes-Num    = vMesNum
          tt-SysMes.Mes-Date   = TODAY
          tt-SysMes.Mes-Time   = TIME
          tt-SysMes.Mes-Class-Code = IF AVAIL code
                                       THEN code.class
                                       ELSE ""
          tt-SysMes.Mes-Code   = vMesCode
          tt-SysMes.Mes-Text   = vMesText
          tt-SysMes.Mes-Author = IF iAuthor > ""
                                    THEN iAuthor
                                    ELSE PROGRAM-NAME(3)
          tt-SysMes.Mes-Stack  = GetStack()
          tt-SysMes.Mes-Type   = vMesType
   .
   /* �������� ���譨� ��ࠡ��稪 ��� ᮮ�饭�� */
   hNewMess = BUFFER tt-SysMes:HANDLE.
   PUBLISH "tmess-fill-sysmes-event" (hNewMess).
   
   /* �뢮� ᮮ�饭�� � 䠩� */
   RUN Log-ProcMes.
   IF vToFile = "��" THEN
   DO:
      vMesText = REPLACE(vMesText,"~n","").
      OUTPUT STREAM LogStream TO VALUE(vLogFile) APPEND.
      PUT STREAM LogStream UNFORMATTED
         STRING(tt-SysMes.Mes-Num,">>>>>>>9") " "
         /*tt-SysMes.Mes-Date " " */
         STRING(tt-SysMes.Mes-Time,"HH:MM:SS") " "
         tt-SysMes.Mes-Type   FORMAT "x(2)" " "
         vMesText             /*FORMAT "X(100)" */ " "
         FILL(" ",62 - LENGTH(vMesText)) "<"
         tt-SysMes.Mes-Author /*FORMAT "x(40)"*/ ">"  +  
         (IF vStck2Fl
            THEN (" �⥪: " + QUOTER (tt-SysMes.Mes-Stack))
            ELSE ""
         )
         SKIP.
      OUTPUT STREAM LogStream CLOSE.
   END.
                        /* ��ନ�㥬 �ਧ��� �뢮�� ᮮ�饭�� �� �࠭. */
   vScreen =   iMesShow OR      
               (vToScreen EQ "��" AND NOT SESSION:REMOTE).
   
   PUBLISH "tmess-fill-sysmes-event-no-screen" (hNewMess, INPUT-OUTPUT vScreen).
   
                        /* ���� �ନ஢��� ��⮮⢥⮢
                        ** � �뢮�� �� �࠭. */
   BLCK_VIEW:
   DO
   ON ERROR    UNDO BLCK_VIEW, LEAVE BLCK_VIEW
   ON ENDKEY   UNDO BLCK_VIEW, LEAVE BLCK_VIEW:
      RELEASE code.
                        /* ��।������ ⨯� �뢮�� �� �࠭. */
      IF vMesTypeClassCode NE vMesTypeClassCodeDef
                        /* �饬 � �������� �����䨪��� ⨯�� ᮮ�饭��. */
         THEN FIND FIRST code WHERE
                  code.class  EQ vMesTypeClassCode
            AND   code.code   EQ vMesType
         NO-LOCK NO-ERROR.
                        /* �饬 � �����䨪��� ⨯�� ᮮ�饭�� �� 㬮�砭�� */
      IF NOT AVAILABLE code
         THEN FIND FIRST code WHERE
                  code.class  EQ vMesTypeClassCodeDef
            AND   code.code   EQ vMesType
         NO-LOCK NO-ERROR.
                        /* ��� �뢮�� �� �࠭ �� ��।����,
                        ** �뤠�� ᮮ�饭�� � ����ன���� �� 㬮�砭��. */
      IF NOT AVAILABLE code
      THEN DO:
         IF vScreen
            THEN RUN 
            message.p (tt-SysMes.Mes-Text, "BRIGHT-", "INFO", "", "", NO).
         LEAVE BLCK_VIEW.
      END.
                        /* ��� "ᮮ�饭��". */
      IF code.val BEGINS "message"
      THEN DO:
                        /* ���� �।��⠭��������� �⢥�. */
         RUN SetAction (
            tt-ProcMes.Proc-Id,  /* ID �����. */
            vMesType,            /* ��� ⨯� ᮮ�饭��.  */
            vMesCode,            /* ��� ᮮ�饭��. */
            ""                   /* ���᮪ �⢥⮢. */
         ).
                        /* �᫨ ����祭 �⢥�,
                        ** ��� �뢮���� �� �࠭ ����饭�, � ��室��. */
         IF NOT iMesShow AND (RETURN-VALUE EQ "" OR NOT vScreen) THEN 
            LEAVE BLCK_VIEW.
         RUN message.p (tt-SysMes.Mes-Text,
                        ENTRY(2,code.val),
                        ENTRY(3,code.val),
                        ENTRY(4,code.val),
                        ENTRY(5,code.val),
                        ENTRY(6,code.val)).
                        /* �।������ ���짮��⥫� �ਬ����� ��� �⢥�
                        ** �� �ᥬ ⠪�� ᮮ�饭�ﬨ */
         RUN SetAnswer(vMesType,vMesCode,tt-SysMes.Mes-Text,"").
      END.
                        /* ��� "����". */
      ELSE IF code.val BEGINS "messmenu"
      THEN DO:
         ASSIGN
            vMenuRow     = ENTRY(1,vMesDesc,";")
            vMenuTitle   = IF NUM-ENTRIES(vMesDesc,";") > 1
                           THEN ENTRY(2,vMesDesc,";")
                           ELSE "".

         IF vMenuChoises = "" THEN
            vMenuChoises = IF NUM-ENTRIES(vMesDesc,";") > 2
                           THEN ENTRY(3,vMesDesc,";")
                           ELSE "".
         IF vMenuRow = "" THEN
            vMenuRow = ENTRY(3,ENTRY(1,code.val),"(").
         IF vMenuRow = "" THEN
            vMenuRow = "9".
         IF vMenuTitle = "" THEN
            vMenuTitle = ENTRY(2,code.val).
         IF vMenuTitle = "" THEN
            vMenuTitle = "[����]".
                        /* ���� �।��⠭��������� �⢥�. */
         RUN SetAction (
            tt-ProcMes.Proc-Id,  /* ID �����. */
            vMesType,            /* ��� ⨯� ᮮ�饭��.  */
            vMesCode,            /* ��� ᮮ�饭��. */
            vMenuChoises         /* ���᮪ �⢥⮢. */
         ).
                        /* �᫨ ����祭 �⢥�,
                        ** ��� �뢮���� �� �࠭ ����饭�, � ��室��. */
         IF NOT iMesShow AND (RETURN-VALUE EQ "" OR NOT vScreen) THEN 
            LEAVE BLCK_VIEW.
         
         RUN messmenu.p (INT64(vMenuRow),
                         vMenuTitle,
                         tt-SysMes.Mes-Text,
                         vMenuChoises).

         vPickValue =   IF INT64(pick-value) > 0
                           THEN ENTRY(INT64(pick-value),vMenuChoises)
                           ELSE "�⪠� �� �롮�".
         RUN LogAnswer (vPickValue).
         /* Commented BY KSV: �।������ ���짮��⥫� �ਬ����� ��� �⢥�
         ** �� �ᥬ ⠪�� ᮮ�饭�ﬨ */
         RUN SetAnswer(vMesType,vMesCode,tt-SysMes.Mes-Text,vMenuChoises).
      END.
                        /* �᫨ ����� �뢮���� ᮮ�饭�� �� �࠭,
                        ** ⮫쪮 � �⮬ ��砥 ����᪠�� ��⮤. */
      ELSE IF vScreen
      THEN DO:
         RUN procname  IN h_xclass  (INPUT  code.val,
                                    OUTPUT vproc-name,
                                    OUTPUT vparam,
                                    INPUT  "",
                                    INPUT  "").
         IF vproc-name > "" THEN
            RUN run_params IN h_xclass (vproc-name,vparam,"",?) NO-ERROR.
      END.
   END.
   RETURN.
END PROCEDURE. /* Fill-SysMes */

/*----------------------------------------------------------------------------*/
/* ���࠭���� �ண��ᮢ�� �訡��                                             */
/*----------------------------------------------------------------------------*/
PROCEDURE Fill-ProgressErr:
   DEFINE INPUT PARAMETER iAuthor  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vI AS INT64    NO-UNDO.

   IF iAuthor = "" THEN
      iAuthor = PROGRAM-NAME(2).

   DO vI = 1 TO ERROR-STATUS:NUM-MESSAGES:
      RUN Fill-SysMes (iAuthor,"","-1",ERROR-STATUS:GET-MESSAGE(vI)).
   END.

END PROCEDURE. /* Fill-ProgressErr */

/************************************************************************/
/* �����頥� ��� ��᫥����� ᮮ�饭��                                  */
/************************************************************************/
PROCEDURE Get-LastSysMes.
DEFINE OUTPUT PARAMETER oMesCode   AS CHARACTER  NO-UNDO.

   FIND LAST tt-SysMes NO-ERROR.
   IF AVAILABLE tt-SysMes THEN
      oMesCode = tt-SysMes.Mes-Code.

END PROCEDURE. /* Get-LastSysMes */

/******************************************************************************/
/* �뢮� ��⮪���                                                            */
/******************************************************************************/
PROCEDURE End-SysMes-Local:
   IF AVAIL tt-ProcMes THEN
   DO:
      {setdest.i
         &filename = "'_spoolm.tmp'"
         &stream   = "STREAM ProtStream"
         &nodef    = "/*"}
   
      PUT STREAM ProtStream UNFORMATTED
         "                             �������� ���������"            SKIP
         tt-ProcMes.Proc-Date                                         " "
         tt-ProcMes.Op-Kind   FORMAT "X(10)"
         tt-ProcMes.Proc-Name                                         SKIP
         "����� ����: " STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS")
         " ����祭: " STRING(tt-ProcMes.Proc-TimeEnd,"HH:MM:SS")      SKIP.
   
      FOR EACH tt-SysMes WHERE tt-SysMes.Proc-Id = tt-ProcMes.Proc-id:
         PUT STREAM ProtStream UNFORMATTED
            STRING(tt-SysMes.Mes-Num,">>>>>>>9")    " "
            STRING(tt-SysMes.Mes-Time,"HH:MM:SS") " "
            tt-SysMes.Mes-Type   FORMAT "x(2)"    " "
            tt-SysMes.Mes-Text                    " "
            FILL(" ",62 - LENGTH(tt-SysMes.Mes-Text))
            "<" tt-SysMes.Mes-Author  ">"         SKIP.
      END.
      {preview.i
         &filename = "'_spoolm.tmp'"
         &stream = "STREAM ProtStream"}
   END.
END PROCEDURE.                                   /* End-SysMes-Local          */

/******************************************************************************/
/* �뢮� ��⮪���, �����襭�� ��⮪���஢���� �����                      */
/************************************************************************&*****/
PROCEDURE End-SysMes:
   DEFINE VARIABLE vStack     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmp       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vInClosing AS LOGICAL    NO-UNDO.

   DEFINE BUFFER SameDescr FOR tt-SettStack. /* ���������� ����. */
   DEFINE BUFFER op-kind   FOR op-kind.

   /* �஢�ઠ �� ����砭�� �����. */
   vStack = GetStack().
   IF LOOKUP(vCurProc,vStack) LT NUM-ENTRIES(vStack)
   THEN DO:
      /* ����⠭�������� ����� � �����䨪��� ᮮ�饭��
      ** ��� �।��饩 ��楤���. */
      FIND LAST tt-SettStack NO-ERROR.
      IF AVAILABLE tt-SettStack
         THEN vMesClassCode = tt-SettStack.MesClassCode.
         /* �.�. ����� �� �����祭,
         ** �����稫�� ⮫쪮 ४��ᨢ�� �맮�. */
      RETURN.
   END.

   /* ����뢠�� ��⮪���஢���� � �� ��������� ��⮪���஢���� */
   _prot_closing:
   REPEAT:
      FIND LAST tt-ProcMes WHERE tt-ProcMes.Proc-Id >= SOURCE-PROCEDURE:UNIQUE-ID
                             AND CAN-FIND(FIRST tt-SettStack WHERE tt-SettStack.Proc-Id = tt-ProcMes.Proc-Id)
      NO-LOCK NO-ERROR.
      IF AVAIL tt-ProcMes THEN DO:
         vInClosing = YES.

         FIND FIRST tt-SettStack WHERE tt-SettStack.Proc-Id = tt-ProcMes.Proc-Id.
         
         /* Commented BY KSV: �᫨ ����祭 ०�� ���. �⢥� �� ���. ᮮ�饭��,
         ** ��⠥��� ��࠭��� �⢥�� �� ���짮��⥫� �� ᮮ�饭�� � ⥪�饩 ��ᨨ
         ** � �.�. �࠭���樨 */
         /* Commented by SHIB: ��⠥��� ��࠭��� �� �⢥��, 
         ** ⮫쪮 �᫨ ���짮��⥫� ����� �ࠢ� �� �� ����⢨� (GetSurrPermission)*/
         IF  tt-SettStack.AutoAnswer
         AND {assigned tt-ProcMes.Op-Kind} THEN
         autoans:
         DO:
            /* Commented BY KSV: ����砥� ᯨ᮪ �⢥⮢ �� ���. ᮮ�饭�� */
            RUN GetAnswers (OUTPUT vTmp).
            /* Commented BY KSV: �᫨ ᯨ᮪ �⢥⮢ �� ����, ����� ��⮪���஢����
            ** �易� � �࠭���樥� � ᯨ᪨ �⢥⮢ �� � ��᫥ �࠭���樨 �⫨�����
            ** �����뢠�� �⢥�� � ���.४����� */
            IF  vTmp <> "" AND
                vTmp <> vAnswers AND
                NOT CAN-DO(vAnswers, "NOTSAVE")
            THEN DO:
               FIND FIRST op-kind WHERE
                          op-kind.op-kind EQ tt-ProcMes.Op-Kind NO-LOCK NO-ERROR.
               IF  AVAIL op-kind 
               AND 
                   /* �஢�ઠ ����㯠 � ⠡���. */
                   (NOT GetTablePermission ("op-kind", "w")
                   /* �஢�ઠ ����㯠 � ��ꥪ⠬ �����. */
                   OR NOT GetSurrPermission ("class", op-kind.class-code, "w"))
               THEN
                  LEAVE autoans.
               /* Commented BY KSV: ���訢��� ���짮��⥫� � ������� ��࠭��� �⢥��
               ** */
               pick-value = "NO".

/*
               RUN message.p("�ਬ����� ��࠭���� �⢥�� �� ��⥬�� ᮮ�饭��~n" +
                             "��� ��� ����᪮� �࠭���樨 [" + tt-ProcMes.Op-Kind + "]?",
                             "","QUESTION","YES-NO","",NO).
*/

               /* Commented BY KSV: ���࠭塞 �⢥��, �᫨ ���짮��⥫� ᮣ��ᥭ */
               IF pick-value = "YES" THEN
               DO TRANSACTION
                  ON ERROR UNDO,LEAVE
                  ON STOP  UNDO,LEAVE:
/*
                  UpdateSigns("op-kind",tt-ProcMes.Op-Kind,"��_��⮎⢥�",vTmp,NO).
*/
               END.  /* END OF BLOCK */
            END.
         END.
         
         /*------------------------------ �������� ���ᮭ����� ����஥� �� ������ --*/
         FOR EACH tt-MsgSet WHERE
                  tt-MsgSet.Proc-Id EQ tt-ProcMes.Proc-id:
            DELETE tt-MsgSet.
         END.
         
         IF tt-ProcMes.Proc-Id > 0 THEN
            tt-ProcMes.Proc-TimeEnd = TIME.
         
         /*--------------------------------------------------------- �뢮� ��⮪��� --*/
         IF  tt-SettStack.ViewLog NE "���"
         AND tt-ProcMes.Proc-Id   GT 0 /* ��� ��⮪�� �� ������⥬�� */
         THEN DO:
            IF tt-SettStack.ViewLog NE "��" THEN
               RUN Log-AutorMes(tt-SettStack.ViewLog). /* ����᪨� ᮮ�饭�� */
         
            IF LOOKUP("��",tt-SettStack.ViewLog) NE 0 AND 
               NOT SESSION:REMOTE         THEN   /* �� AppServer                    */
            DO:
               {setdest.i
                  &filename = "'_spoolm.tmp'"
                  &stream   = "STREAM ProtStream"
                  &cols     = 126
                  &option   = "LANDSCAPE"
                  &nodef    = "/*"}
         
               PUT STREAM ProtStream UNFORMATTED
                  "                             �������� ���������"            SKIP
                  tt-ProcMes.Proc-Date                                         " "
                  tt-ProcMes.Op-Kind   FORMAT "X(10)"
                  tt-ProcMes.Proc-Name                                         SKIP
                  "����� ����: " STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS")
                  " ����祭: " STRING(tt-ProcMes.Proc-TimeEnd,"HH:MM:SS")      SKIP.
         
               FOR EACH tt-SysMes WHERE tt-SysMes.Proc-Id = tt-ProcMes.Proc-id:
                  PUT STREAM ProtStream UNFORMATTED
                     STRING(tt-SysMes.Mes-Num,">>>>>>>9")    " "
                     STRING(tt-SysMes.Mes-Time,"HH:MM:SS") " "
                     tt-SysMes.Mes-Type   FORMAT "x(2)"    " "
                     tt-SysMes.Mes-Text                    " "
                     FILL(" ",62 - LENGTH(tt-SysMes.Mes-Text))
                     "<" tt-SysMes.Mes-Author  ">"         SKIP.
               END.
               {preview.i
                  &filename = "'_spoolm.tmp'"
                  &stream = "STREAM ProtStream"}
            END.                                       /* IF LOOKUP("��",vViewLog)  */
         END.                                          /* IF vViewLog NE "���"      */

         ASSIGN
            vLogFlag    = NO
            vLogProcMes = tt-ProcMes.Proc-Id NE 0
         .
         /*------------------------------------------------------ 㤠����� ��⮪��� --*/
         IF tt-SettStack.DelLog = "��" THEN DO:
            FOR EACH tt-SysMes WHERE tt-SysMes.Proc-Id = tt-ProcMes.Proc-id:
               DELETE tt-SysMes.
            END.
            DELETE tt-ProcMes.
         END.
         
         /*----------------------------------------------- �����⨥ ��⮪���஢���� --*/
         IF AVAILABLE tt-SettStack THEN
         DO:
            FOR EACH SameDescr WHERE
               SameDescr.Proc-Id EQ tt-SettStack.Proc-Id:
               DELETE SameDescr.
            END.
         END.
      END.
      ELSE vInClosing = NO.

      /* ����ன�� ��᫥����� ��⮪���஢���� */
      FIND LAST tt-SettStack NO-ERROR.
      IF AVAIL tt-SettStack THEN
      DO:
         FIND FIRST tt-ProcMes WHERE
                    tt-ProcMes.Proc-Id = tt-SettStack.Proc-Id
         NO-ERROR.
         ASSIGN
            vMesNum     = tt-SettStack.MesNum
            vCurProc    = tt-SettStack.CurProc
            vToScreen   = tt-SettStack.ToScreen
            vToFile     = tt-SettStack.ToFile
            vDebugLev   = tt-SettStack.DebugLev
            vDelLog     = tt-SettStack.DelLog
            vLogFile    = tt-SettStack.LogFile
            vViewLog    = tt-SettStack.ViewLog
            vAutoAnswer = tt-SettStack.AutoAnswer
            vAnswers    = tt-SettStack.Answers
            vStck2Fl    = tt-SettStack.Stck2Fl
         .
      END.

      IF NOT vInClosing THEN LEAVE _prot_closing.
   END.

   RETURN.
END PROCEDURE.                                   /* End-SysMes                */
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE Log-AutorMes:
   DEFINE INPUT PARAMETER iAutorList AS CHAR NO-UNDO.

   DEFINE VAR vAutorOne AS CHAR                    NO-UNDO.
   DEFINE VAR vAutorItm AS INT64                 NO-UNDO.
   DEFINE VAR vAll      AS CHAR                    NO-UNDO.
   DEFINE VAR vItm      AS INT64                 NO-UNDO.
   DEFINE VAR printer-c AS CHAR     case-sensitive NO-UNDO.

   IF NOT SESSION:REMOTE THEN /* �� AppServer   */
   DO:
      {setdest.i  &filename = "'_spoola.tmp'"
                  &stream   = "STREAM ProtStream"
                  &nodef    = "/*"}
   
      DO vAutorItm = 1 TO NUM-ENTRIES(iAutorList):
         vAutorOne = ENTRY(vAutorItm,iAutorList).
   
         IF vAutorOne EQ "��" OR vAutorOne EQ "���" THEN NEXT.
   
         PUT STREAM ProtStream UNFORMATTED
            "                             �������� ���������"    SKIP
            tt-ProcMes.Proc-Date                 " "
            tt-ProcMes.Op-Kind   FORMAT "X(10)"  " "
            "����� ���������: "  vAutorOne                       SKIP(2).
   
         FOR EACH tt-SysMes WHERE
                  tt-SysMes.Proc-Id    EQ tt-ProcMes.Proc-id
              AND tt-SysMes.Mes-Author EQ vAutorOne:
   
            vAll = replace(tt-SysMes.Mes-Text,"(" + tt-SysMes.Mes-Code + ")","").
   
            DO vItm = 1 TO NUM-ENTRIES(vAll,"~n"):
               PUT STREAM ProtStream UNFORMATTED
                  string("<" + tt-SysMes.Mes-Code + ">","x(12)") " "
                  ENTRY(vItm,vAll,"~n")                          SKIP.
            END.
         END.
   
         PUT STREAM ProtStream UNFORMATTED SKIP(2).
   
      END.
   
      {preview.i &filename = "'_spoola.tmp'"
                 &stream   = "STREAM ProtStream"
      }
   END.
END PROCEDURE.
/*============================================================================*/
/*      ��楤��� ��� ����७���� �ᯮ�짮�����                               */
/*============================================================================*/
PROCEDURE Get-BufMes:
   DEFINE OUTPUT PARAMETER TABLE FOR tt-ProcMes.
   DEFINE OUTPUT PARAMETER TABLE FOR tt-SysMes.
END PROCEDURE.                                   /*                           */

/* ��楤�� �������� ����� ��।����� �室�� ��ࠬ��஬ ⠡����
** � ⠡���� tt-SysMes ��� ���� */
PROCEDURE Add-BufMes.
   DEFINE INPUT PARAMETER TABLE FOR tt-SysMes APPEND. /* ⠡��� � ������塞묨 ᮮ�饭�ﬨ */
END PROCEDURE.

/* ��楤�� �������� ����� ��।����� �室�� ��ࠬ��஬ ⠡����
** � ⠡���� tt-SysMes � ������� �����䨪��� ����� Proc-Id */
PROCEDURE Add-BufMesEx.
   DEFINE INPUT PARAMETER TABLE FOR tt-AddSysMes.     /* ⠡��� � ������塞묨 ᮮ�饭�ﬨ */
   DEFINE INPUT PARAMETER iProcID AS INT64     NO-UNDO. /* �����䨪��� ����� */

   FOR EACH tt-AddSysMes:
      tt-AddSysMes.Proc-Id = iProcID.
   END.
   RUN Add-BufMes(TABLE tt-AddSysMes).
END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �뢮� ��������� ����� � 䠩� ��⮪���                                  */
/*----------------------------------------------------------------------------*/
PROCEDURE Log-ProcMes:

   IF       vToFile  EQ "��"
      AND   vLogProcMes       THEN
   DO:
                        /* �� AppServer. */
      IF NOT SESSION:REMOTE THEN
      DO:
         OUTPUT STREAM LogStream TO VALUE(vLogFile) APPEND.
         PUT STREAM LogStream UNFORMATTED
            (IF auto THEN "-" ELSE FILL("-",150)) SKIP
            tt-ProcMes.Proc-Date " "
            STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS") " "
            tt-ProcMes.Op-Kind FORMAT "X(10)"
            tt-ProcMes.Proc-Name SKIP.

         IF vStck2Fl THEN
         DO:
            PUT STREAM LogStream UNFORMATTED
            tt-ProcMes.Proc-Date " "
            STRING(tt-ProcMes.Proc-TimeBeg,"HH:MM:SS") " "
            tt-ProcMes.Op-Kind FORMAT "X(10)"
            "����祭 ०�� �뢮�� �⥪� � 䠩� ~"��_�⥪�����~"."
            SKIP.
         END.
         OUTPUT STREAM LogStream CLOSE.
      END.
      vLogProcMes = NO.
   END.
END PROCEDURE.                                   /*                           */
/*----------------------------------------------------------------------------*/
/* ����祭�� ����஥� �����.                                               */
/*----------------------------------------------------------------------------*/
PROCEDURE GetProcSetting.

   DEF INPUT PARAM iPType AS CHAR NO-UNDO. /* ����� �����. */
   DEF INPUT PARAM iPID   AS CHAR NO-UNDO. /* �����䨪���  �����. */

   DEF VAR vFileName AS CHAR   NO-UNDO. /* ��� 䠩�� ��⮪���. */

   DEF BUFFER op-kind FOR op-kind. /* ���������� ����. */

   /* ��⠥� ��⥬�� ����ன�� ��⮪���஢����. */
   ASSIGN
      vToScreen         =  FGetSetting("���⑮��饭��","��_�뢮�����࠭","���")
      vToFile           =  FGetSetting("���⑮��饭��","��_�뢮������","���")
      vDebugLev         =  INT64(FGetSetting("���⑮��饭��","��_�஢���⫠��","0"))
      vViewLog          =  FGetSetting("���⑮��饭��","��_�뢮���⪫","���")
      vDelLog           =  FGetSetting("���⑮��饭��","��_��������⪫","��")
      vMesTypeClassCode =  FGetSetting("���⑮��饭��", "��_������������","����蒨�")
      vStck2Fl          =  FGetSetting("���⑮��饭��","��_�⥪�����","���")  EQ "��"
   .
   /* �᫨ ��।�� �����䨪��� �����,
   ** � �஡㥬 ���� ��� ���ᠭ��. */
   IF LENGTH (iPID) GT 0
      THEN FIND FIRST op-kind WHERE
         op-kind.op-kind EQ iPID
      NO-LOCK NO-ERROR.
   /* �᫨ ������ �����䨪��� �����,
   ** � ��६ ���ᠭ�� �����. */
   IF AVAIL op-kind
   THEN DO:
      ASSIGN
         vToScreen         =  GetXattrValueEx("op-kind", iPID, "��_�뢮�����࠭", vToScreen)
         vToFile           =  GetXattrValueEx("op-kind", iPID, "��_�뢮������",   vToFile)
         vViewLog          =  GetXattrValueEx("op-kind", iPID, "��_�뢮���⪫",   vViewLog)
         vDelLog           =  GetXattrValueEx("op-kind", iPID, "��_��������⪫", vDelLog)
         vFileName         =  GetXattrValueEx("op-kind", iPID, "��_������⪫",    vLogFile)
         vAnswers          =  GetXattrValueEx("op-kind", iPID, "��_��⮎⢥�",    "")
         vDebugLev         =  INT64 (GetXattrValueEx("op-kind", iPID, "��_�஢���⫠��", STRING (vDebugLev)))
         vMesTypeClassCode =  GetXattrValueEx("op-kind", iPID, "��_������������",vMesTypeClassCode)
         vStck2Fl          =  GetXattrValueEx("op-kind", iPID, "��_�⥪�����", STRING (vStck2Fl,"��/���")) EQ "��"
      .
      /* �᫨ � ����ன�� 㪠��� ����୮� ��ࠦ����,
      ** � ����᪠�� ��ࠡ��稪 �����.
                        ** ���� ��ࠡ��뢠���� ⮫쪮 �������� 䠩��. */
      IF INDEX (vFileName, "<") GT 0
         THEN RUN pptmess.p (vFileName, OUTPUT vLogFile).
         ELSE vLogFile = vFileName.
   END.
   /* �����ࠥ� ⨯ �����. */
   CASE iPType:
      /* �����: "�⠭���⭠� �࠭�����". */
      WHEN "op-kind"
      THEN DO:
         IF NOT AVAILABLE op-kind
            THEN RETURN.
                        /* ���४��㥬 �������� 䠩��. �ᯮ������ � APPL-SRV. */
         IF       LENGTH   (mPrefLog)  GT 0
            AND   NOT (vLogFile        BEGINS mPrefLog)
            THEN vLogFile =  mPrefLog + vLogFile.
                        /* �᫨ � ४����� ��_��⮎⢥� ���祭�� ���/NO
                        ** �몫�砥� ����������� ���. �⢥⮢, �� �⮬ ���뢠����
                        ** ����������� �뢮�� ᮮ�饭�� �� �࠭. */
         vAutoAnswer = NOT CAN-DO ("NO,���",vAnswers) AND vToScreen EQ "��".
         IF mProcName EQ ""
            THEN mProcName = op-kind.name-opkind.
      END.
      /* ����ன�� "batch" ०���. */
      WHEN "AUTO"
      THEN ASSIGN
         vToScreen   = "���"
         vViewLog    = "���"
         mProcName   = "��ࢥ� ���⮢�� ᮮ�饭�� (" + iPID  + ")."
      .
   END CASE.
   RETURN.
END PROCEDURE.

/* ���࠭���� �⢥� � ⠡��� � �뢮� � 䠩�. */
PROCEDURE LogAnswer.
   DEF INPUT PARAM iAnswer AS CHAR NO-UNDO. /* �⢥� �� ᮮ�饭��. */

   tt-SysMes.Mes-Text = tt-SysMes.Mes-Text + " ��࠭�: " + iAnswer.

   IF vToFile = "��"
   THEN DO:
      OUTPUT STREAM LogStream TO VALUE(vLogFile) APPEND.
      PUT STREAM LogStream
         UNFORMATTED
         SPACE(28) " ��࠭�: " + iAnswer FORMAT "X(90)"
         SKIP
      .
      OUTPUT STREAM LogStream CLOSE.
   END.
   RETURN.
END PROCEDURE.

/* �ந������ ��⠭���� �⢥�, �᫨ ⠪���� ����. */
PROCEDURE SetAction.
   DEF INPUT PARAM iPId       AS INT64   NO-UNDO. /* �����䨪��� �����. */
   DEF INPUT PARAM iMsgType   AS CHAR  NO-UNDO. /* ��� ⨯� ᮮ�饭��. */
   DEF INPUT PARAM iMsgCode   AS CHAR  NO-UNDO. /* ��� ᮮ�饭��. */
   DEF INPUT PARAM iMsgAnsw   AS CHAR  NO-UNDO. /* �⢥�� ��� ᮮ�饭��. */

   DEF VAR vAnswChar AS CHAR NO-UNDO. /* �⢥� ��� ����஢����. */

   DEF BUFFER tt-MsgSet FOR tt-MsgSet. /* ���������� ����. */

   /* �᫨ ᮮ�饭�� �ॡ��� �⢥� � �⢥� �।��⠭�����,
   ** � �ନ�㥬 �⢥� � ��室��. ���砫� �饬 �⢥�� � ��� ᮮ⢥��⢨��
   ** ���� ᮮ�饭�� � �⢥� � ⥪�饣� ���� ᮮ�饭��, �᫨ ���室�騩
   ** �⢥� �� ������, �饬 �।� ���� �⢥⮢ (��� = '*') */
   IF CAN-DO ("3,4", iMsgType) THEN
   FOR EACH tt-MsgSet WHERE
      (tt-MsgSet.Proc-Id    EQ iPId  AND
       tt-MsgSet.Mes-Code   EQ iMsgCode) OR
      (tt-MsgSet.Proc-Id    EQ iPId  AND
       tt-MsgSet.Mes-Code   EQ "*")
      BY tt-MsgSet.Mes-Code DESC BY tt-MsgSet.Mes-Order:
      
      /* ��⠭�������� �⢥� ��� ����஢����. */
      IF iMsgAnsw EQ ""
      THEN IF tt-MsgSet.MsgAnsw EQ "YES"
         THEN ASSIGN
            vAnswChar   = "��"
            pick-value  = "YES"
         .
         ELSE ASSIGN
            vAnswChar   = IF tt-MsgSet.MsgAnsw EQ "NO" THEN "���" ELSE "�⪠� �� �롮�"
            pick-value  = IF tt-MsgSet.MsgAnsw EQ "NO" THEN "NO"  ELSE ?
         .
      ELSE ASSIGN
         vAnswChar   =  IF LOOKUP (tt-MsgSet.MsgAnsw, iMsgAnsw) NE 0
                           THEN tt-MsgSet.MsgAnsw
                           ELSE "�⪠� �� �롮�"
         pick-value  = STRING (LOOKUP (tt-MsgSet.MsgAnsw, iMsgAnsw))
      .
      IF vAnswChar NE "�⪠� �� �롮�"
         THEN LEAVE.
   END.
   IF vAnswChar NE ""
   THEN DO:
      /* ���࠭���� �⢥� � 䠩� �⢥�. */
      RUN LogAnswer (vAnswChar).
      RETURN.
   END.
   RETURN "�⢥� �� ������.".
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ����訢��� ���짮��⥫� � �ਬ���� ��� �⢥� �� ��᫥����
               ᮮ�饭�� �� �ᥬ ᮮ�饭�� ⠪��� ⨯�
  Parameters:  iMessType   - ⨯ ᮮ�饭��, ��ࠡ��뢠�� ⮫쪮 ᮮ�饭�� �
                             ⨯��� 3 � 4.
               iMessCode   - ��� ᮮ�饭��
               iMessText   - ⥪�� ᮮ�饭��
               iAnswers    - ��ਠ��� �⢥⮢
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE SetAnswer:
   DEFINE INPUT  PARAMETER iMessType AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iMessCode AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iMessText AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAnswers  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vPickValue    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAnswer       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDispAnswer   AS CHARACTER  NO-UNDO.

   /* Commented BY KSV: �᫨ ��� ������� ����� ����������� ��࠭��� �⢥� ��
   ** ����� ����饭� ��� �� ᮮ�饭�� �� ���� ����ᮬ, ��室�� */
   IF vAutoAnswer <> YES OR NOT CAN-DO("3,4",iMessType)  THEN RETURN.

   /* Commented BY KSV: �᫨ ᮮ�饭�� �� ����� ����, ���� ��᫥���� �⢥⮬
   ** �뫮 ����⨥ ESC, � ��室 */
   IF pick-value = ? OR NOT {assigned iMessCode} THEN RETURN.
   /* Commented BY KSV: ����砥� ⥪�� �⢥�, ��� ᮮ�饭�� � ⨯�� 4, ���
   ** ��।������ ��� ENTRY(INT64(pick-value),iAnswers), ��� ᮮ�饭�� 3, ����
   ** pick-value */
   IF {assigned iAnswers} THEN
   DO:
      vAnswer = ENTRY(INT64(pick-value),iAnswers) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN.
      vDispAnswer = vAnswer.
   END.
   ELSE
   DO:
      vAnswer = pick-value.
      vDispAnswer = (IF pick-value = "YES" THEN "��" ELSE "���").
   END.

   /* Commented BY KSV: ���������� ⥪�騩 �⢥� ���짮��⥫� �� ���. ᮮ�饭��
   ** */
   vPickValue = pick-value.
   /* Commented BY KSV: ���訢��� ���짮��⥫�, �� ������ � ��� �⢥⮬ */
   pick-value = "1".
/*
   RUN messmenu.p (9,"[�����]",
                   "�ਬ����� �⢥� [" + vDispAnswer +
                   "] �� �ᥬ ᮮ�饭�� [" + iMessCode + "]" + "~n" +
                   "� ⥪�饩 �࠭���樨?",
                   "�ਬ�����,�� �ਬ�����,����� �� �।������").
*/
   CASE pick-value:
      /* Commented BY KSV: ���������� �⢥� ���짮��⥫� �� ���. ᮮ�饭�� */
      WHEN "1" THEN
         RUN SetMsgPrm (iMessCode, "MsgAnsw", vAnswer).
      /* Commented BY KSV: �⪫�砥� ����� �⮩ ��楤��� */
      WHEN "3" THEN
         vAutoAnswer = NO.
   END CASE.
   /* Commented BY KSV: ����⠭�������� �⢥� ���짮��⥫� �� ���. ᮮ�饭�� */
   pick-value = vPickValue.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='07/08/2015 17:39:24.839+04:00' */
/* $LINTUSER='ariz' */
/* $LINTMODE='1' */
/* $LINTFILE='tmess.pro' */
/*prosignRiGbMsPvhxKPZyT2483EYA*/