/*
               ������᪠� ��⥣�஢����� ��⥬� QBIS
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: 148n.i
      Comment: ��騥 �㭪樨 ����஫� ४����⮢ ���㬥�⮢
               ᮣ��᭮ �ਪ��� 148�.
   Parameters:
      Created: 24.03.2016 krok
     Modified: 02.08.2016 17:25 BIS     
*/

&IF DEFINED(DEF_148N_I_) = 0 &THEN

DEFINE TEMP-TABLE ttNalRec NO-UNDO 
   FIELD ben-acct LIKE op.ben-acct
   FIELD PokST    AS CHARACTER  /* (101) */
   FIELD kpp-send AS CHARACTER  /* (102) */
   FIELD kpp-rec  AS CHARACTER  /* (103) */
   FIELD KBK      AS CHARACTER  /* (104) */
   FIELD OKTMO    AS CHARACTER  /* (105) */
   FIELD PokOP    AS CHARACTER  /* (106) */
   FIELD PokNP    AS CHARACTER  /* (107) */
   FIELD PokND    AS CHARACTER  /* (108) */
   FIELD PokDD    AS CHARACTER  /* (198) */
.

&GLOBAL-DEFINE DEF_148N_I_ YES

/*
   �஢�ઠ ⮣�, �� ���� ��᢮��� ���祭�� "� ��᫥ ��������� ४����⮢".
   �.�. "0" ��ࠢ�������� � ���⮬� ���祭��.
*/
FUNCTION isAssigned148n RETURN LOGICAL (INPUT iStr AS CHARACTER):
   RETURN {assigned iStr} AND iStr <> "0".
END FUNCTION.

/*
   �஢�ઠ ⮣�, �� ��ப� iStr ��⮨� ⮫쪮 �� ���. �������-ࠧ����⥫�
   � ���� ��ப� ����������� �� �஢�થ.
   �᫨ iNumDigits <> ?, ��ப� ������ ᮤ�ঠ�� ஢�� iNumDigits ᨬ�����.
   �᫨ iNonZero = YES, ��ப� �� ����� ������ �� ����� �㫥�.
*/
FUNCTION isDigital148n RETURN LOGICAL PRIVATE (INPUT iStr       AS CHARACTER,
                                               INPUT iNumDigits AS INT64,
                                               INPUT iNonZero   AS LOGICAL):
   IF iStr = ? THEN
      RETURN ?.
   iStr = RIGHT-TRIM(iStr).
   RETURN (iNumDigits = ? OR iNumDigits = LENGTH(iStr)) AND
          TRIM(iStr, "0123456789") = ""                 AND
          NOT (iNonZero = YES AND TRIM(iStr, "0") = "").
END FUNCTION.

/*
   �஢�ઠ ⮣�, �� ��ப� ���� ��⮩ � �ଠ� ��.��.����.
   �������-ࠧ����⥫� � ���� ��ப� ����������� �� �஢�થ.
*/
FUNCTION isDate148n RETURN LOGICAL PRIVATE (INPUT iStr AS CHARACTER):
   DEFINE VARIABLE vDate AS DATE NO-UNDO.

   IF iStr = ? THEN
      RETURN ?.
   iStr = RIGHT-TRIM(iStr).
   IF LENGTH(iStr) = 10           AND
      SUBSTRING(iStr, 3, 1) = "." AND
      SUBSTRING(iStr, 6, 1) = "."
   THEN DO:
      vDate = DATE(iStr) NO-ERROR.
      RETURN (NOT ERROR-STATUS:ERROR AND vDate <> ?).
   END.
   ELSE
      RETURN NO.
END FUNCTION.

/*
   ��।������ ����஫쭮� �㬬� ���稬�� ������ ��� (���� 19 ࠧ�冷�)
   �⭮�⥫쭮 ��������� ᤢ��� ��᫥����⥫쭮�� ��ᮢ�� �����樥�⮢
*/
FUNCTION getUINChecksum RETURN INT64 PRIVATE (INPUT iStr   AS CHARACTER,
                                              INPUT iShift AS INT64):
   DEFINE VARIABLE vI AS INT64 NO-UNDO.
   DEFINE VARIABLE vN AS INT64 NO-UNDO.
   DEFINE VARIABLE vW AS INT64 NO-UNDO.
   DEFINE VARIABLE vS AS INT64 NO-UNDO.

   DO vI = 1 TO LENGTH(iStr):
      ASSIGN
         vN = INT64(SUBSTRING(iStr, vI, 1))
         vW = 1 + (iShift + vI - 1) MODULO 10
      .
      vS = vS + vN * vW.
   END.
   RETURN vS MODULO 11.
END FUNCTION.

/*
   ��।������ ����஫쭮�� ࠧ�鸞 ���. ������� 㦥 �ਢ��� �
   ��ப����� ⨯�. �� ��।�� �����४⭮�� ���祭�� (� ������,
   �⫨筮� �� 20, ��� ᮤ�ঠ饣� �������⨬� ᨬ����) �����頥� ?.
*/
FUNCTION getUINKey RETURN CHARACTER PRIVATE (INPUT iUIN AS CHARACTER):
   DEFINE VARIABLE vS AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vK AS INT64     NO-UNDO.

   IF isDigital148n(iUIN, 20, NO) THEN DO:
      vS = SUBSTRING(iUIN, 1, 19).
      vK = getUINChecksum(vS, 0).
      IF vK = 10 THEN
         vK = getUINChecksum(vS, 2).
      IF vK = 10 THEN
         vK = 0.
   END.
   ELSE
      vK = ?.
   RETURN STRING(vK).
END FUNCTION.

/*
   �஢�ઠ ����஫쭮�� ࠧ�鸞 ���. �����頥� ���祭�� �����᪮�� ⨯�,
   ᮮ⢥�����饥 �ࠢ��쭮�� ����஫쭮�� ࠧ�鸞, ���� ? � ��砥, �᫨
   �뫮 ��।��� �����४⭮� ���祭�� ��� (� ������, �⫨筮� �� 20, ���
   ᮤ�ঠ饥 �������⨬� ᨬ����)
*/
FUNCTION checkUINKey RETURN LOGICAL PRIVATE (INPUT iUIN AS CHARACTER):
   DEFINE VARIABLE vK AS CHARACTER NO-UNDO.

   vK = getUINKey(iUIN).
   RETURN IF vK = ?
          THEN ?
          ELSE vK = SUBSTRING(iUIN, LENGTH(iUIN)).
END FUNCTION.

/* 1 */
FUNCTION check148n-pokst-uin-pninn RETURN CHARACTER PRIVATE (INPUT iPokST AS CHARACTER,
                                                             INPUT iUIN   AS CHARACTER,
                                                             INPUT iPNINN AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("���", "����ሏ", ""), iPokST) AND
      NOT ({assigned iUIN} OR {assigned iPNINN})
   THEN
      vErrorMsg = "��� ���������� ���祭�� ����� " +
                  "� ���㬥�� ������ ���� ������ ��� ��� ��� ���⥫�騪�".
   RETURN vErrorMsg.
END FUNCTION.

FUNCTION check148n-pninn RETURN CHARACTER PRIVATE (INPUT iPNINN AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF {assigned iPNINN} AND
      NOT isDigital148n(iPNINN, 12, YES)
   THEN
      vErrorMsg = "� ���� ��� ���⥫�騪� ����室��� 㪠���� 12 ���, " +
                  "�� ������ ��ﬨ �����६����".
   RETURN vErrorMsg.
END FUNCTION.

/* 2 */
FUNCTION check148n-kbk-pokop-pokdd RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                             INPUT iPokOP AS CHARACTER,
                                                             INPUT iPokDD AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAffinity AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("���", "��������", ""), iKBK) THEN
      vAffinity = "�".
   ELSE IF CAN-DO(FGetSetting("���", "��������", ""), iKBK) THEN
      vAffinity = "�".
   IF {assigned vAffinity} AND
      {assigned iPokOP}    AND
      iPokDD = "0"
   THEN
      vErrorMsg = "��� ������� ���祭�� ���� ��� ���� ����� �� ����� " +
                  "ᮤ�ঠ�� ���祭�� ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.

/* 3 */
FUNCTION check148n-kbk-pokop-pnpoluwcatelwinn RETURN CHARACTER PRIVATE (INPUT iKBK              AS CHARACTER,
                                                                        INPUT iPokOP            AS CHARACTER,
                                                                        INPUT iPNPoluwcatelwINN AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAffinity AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("���", "��������", ""), iKBK) THEN
      vAffinity = "�".
   ELSE IF CAN-DO(FGetSetting("���", "��������", ""), iKBK) THEN
      vAffinity = "�".
   IF {assigned vAffinity} AND
      {assigned iPokOP}    AND
      NOT isDigital148n(iPNPoluwcatelwINN, 10, YES)
   THEN
      vErrorMsg = "��� ������� ���祭�� ���� ��� ���� ��� �����⥫� ������ " +
                  "ᮤ�ঠ�� 10 ���, �� ࠢ��� ~"0~" �����६����".
   RETURN vErrorMsg.
END FUNCTION.

/* 4 */
FUNCTION check148n-kbk RETURN CHARACTER PRIVATE (INPUT iKBK AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF iKBK = FILL("0", 20) THEN
      vErrorMsg = "�������⨬�� ���祭�� ���� ���".
   RETURN vErrorMsg.
END FUNCTION.

/* 5 */
FUNCTION check148n-okatonalog RETURN CHARACTER PRIVATE (INPUT iOkato-Nalog AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF NOT (iOkato-Nalog = "0"                   OR
           isDigital148n(iOkato-Nalog, 8 , YES) OR
           isDigital148n(iOkato-Nalog, 11, YES))
   THEN
      vErrorMsg = "� ���� ����� ����室��� 㪠���� ���� ~"0~", ���� ��ப� " +
                  "�� 8 ��� 11 ���, �� ������� ⮫쪮 �� �㫥�".
   RETURN vErrorMsg.
END FUNCTION.

/* 6 */
FUNCTION check148n-kbk-pokst RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                       INPUT iPokST AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF iKBK   <> "0" AND
      iPokST =  "0"
   THEN
      vErrorMsg = "�� 㪠����� ���, �⫨筮�� �� ~"0~", ���� ����� " +
                  "�� ����� �ਭ����� ���祭�� ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.

/* 7 */
FUNCTION check148n-kbk-pokop RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                       INPUT iPokOP AS CHARACTER):
   DEFINE BUFFER code FOR code.

   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAffinity AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("���", "��������", ""), iKBK) THEN
      vAffinity = "�".
   ELSE IF CAN-DO(FGetSetting("���", "��������", ""), iKBK) THEN
      vAffinity = "�".
   FIND FIRST code WHERE
      code.class = "���:��" AND
      code.code  = iPokOP
   NO-LOCK NO-ERROR.
   IF {assigned vAffinity} THEN DO:
      IF AVAILABLE code AND
         NOT CAN-DO(code.description[1], vAffinity)
      THEN
         vErrorMsg = SUBSTITUTE("��������� �᭮����� ���⥦� ������ ����� " +
                                "�ਧ��� �ਭ��������� &1 � �����䨪��� &2",
                                QUOTER(vAffinity),
                                code.class).
   END.
   ELSE DO:
      IF iPokOP <> "0" AND
         {assigned code.description[1]}
      THEN
         vErrorMsg = SUBSTITUTE("��� ������� ���祭�� &1 � ���� &2 ����室��� " +
                                "㪠���� &3 ���� �᭮�����, �� ����饥 " +
                                "�ਧ���� �ਭ��������� � �����䨪��� &4",
                                QUOTER("���"),
                                QUOTER("�᭮����� ���⥦�"),
                                QUOTER("0"),
                                code.class).
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* 8 */
FUNCTION check148n-pokop-poknd-pokdd RETURN CHARACTER PRIVATE (INPUT iPokOP AS CHARACTER,
                                                               INPUT iPokND AS CHARACTER,
                                                               INPUT iPokDD AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vError    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vI        AS INT64     NO-UNDO.

   IF iPokOP = "��" THEN DO:
      IF iPokND <> "0" OR NOT isDate148n(iPokDD) THEN
         vErrorMsg = "��� ������� ���祭�� ����� � ���� ����� " +
                     "����室��� 㪠���� ���祭�� ~"0~", � � "  +
                     "���� ����� - ���� � �ଠ� ��.��.����".
   END.
   ELSE DO:
      IF LENGTH(iPokND)            > 0   AND
         LENGTH(iPokND)           <  16  AND
         iPokND                   <> "0" AND
         NUM-ENTRIES(iPokND, ";") =  1
      THEN DO:
         IF NOT isDate148n(iPokDD) THEN
            vErrorMsg = "��� ⠪��� ���祭�� ����� � ���� ����� " +
                        "����室��� 㪠���� ���� � �ଠ� ��.��.����".
      END.
      ELSE DO:
         IF iPokND = "0" THEN
            vError = YES.
         ELSE IF NUM-ENTRIES(iPokND, ";") > 1 THEN DO:
            vI = INT64(ENTRY(1, iPokND, ";")) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR AND
               vI > 0                 AND
               CAN-FIND(FIRST person WHERE
                           person.person-id = vI
                        NO-LOCK)
            THEN
               vError = YES.
         END.
         IF vError AND iPokDD <> "0" THEN
            vErrorMsg = "��� ⠪��� ���祭�� ����� � ���� ����� " +
                        "����室��� 㪠���� ~"0~"".
      END.
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* 9 */
FUNCTION check148n-pokop-poknd RETURN CHARACTER PRIVATE (INPUT iPokOP AS CHARACTER,
                                                         INPUT iPokND AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   IF CAN-DO(FGetSetting("���", "��106�����0", ""), iPokOP) AND
      iPokND <> "0"
   THEN
      vErrorMsg = "��� ������� ���祭�� ����� � ���� ����� " +
                  "����室��� 㪠���� ���祭�� ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.

/* 10 */
FUNCTION check148n-uin-kbk RETURN CHARACTER PRIVATE (INPUT iUIN AS CHARACTER,
                                                     INPUT iKBK AS CHARACTER):
   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vError    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vS        AS CHARACTER NO-UNDO.

   IF iUIN <> "0" THEN DO:
      vS = TRIM(iUIN, "0").
      CASE LENGTH(iUIN):
         WHEN 20 THEN
            vError = NOT {assigned vS} OR iUIN = iKBK.
         WHEN 25 THEN
            vError = NOT {assigned vS}.
         OTHERWISE
            vError = YES.
      END CASE.
      IF vError THEN
         vErrorMsg = "� ���� ��� ������ ���� 㪠���� ���祭�� ~"0~", ���� "  +
                     "��� ������ ᮤ�ঠ�� 20 ��� 25 ᨬ�����, ���� �� "  +
                     "����� 20 ��� ���祭�� �� ����� ᮢ������ � ���祭���" +
                     "���� ~"���~"".
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* ? */
FUNCTION check148n-kbk-pokop-poknp RETURN CHARACTER PRIVATE (INPUT iKBK   AS CHARACTER,
                                                             INPUT iPokOP AS CHARACTER,
                                                             INPUT iPokNP AS CHARACTER):
   DEFINE BUFFER code FOR code.

   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vError    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vS        AS CHARACTER NO-UNDO.

   IF CAN-DO(GetCodeDesc("���:��", iPokOP, 1, ""), "�") AND
      CAN-DO(FGetSetting("���", "��������", ""), iKBK)
   THEN DO:
      /* �������� ����� */
      IF CAN-DO(FGetSetting("���", "�106-107���", ""), iPokOP) THEN DO:
         IF NOT isDate148n(iPokNP) THEN
            vErrorMsg = "��� ��������� ���⥦�� � ⠪�� �᭮������ � ���� " +
                        "(107) ����室��� 㪠���� ���� � �ଠ� ��.��.����".
      END.
      ELSE IF CAN-DO(FGetSetting("���", "�106-107����", ""), iPokOP) THEN DO:
         IF iPokNP <> "0" THEN
            vErrorMsg = "��� ��������� ���⥦�� � ⠪�� �᭮������ � ���� " +
                        "(107) ����室��� 㪠���� ~"0~"".
      END.
   END.
   RETURN vErrorMsg.
END FUNCTION.

/* ? */
FUNCTION check148n-_personid-pninn RETURN CHARACTER PRIVATE (INPUT iPersonId AS INT64,
                                                             INPUT iPNINN    AS CHARACTER):
   DEFINE BUFFER person FOR person.

   DEFINE VARIABLE vErrorMsg AS CHARACTER NO-UNDO.

   FIND FIRST person WHERE
      person.person-id = iPersonId
   NO-LOCK NO-ERROR.
   IF AVAILABLE person          AND
      NOT {assigned person.inn} AND
      iPNINN <> "0"
   THEN
      vErrorMsg = "� ���⥫�騪� ��������� ���, � ���� ��� ���⥫�騪� " +
                  "����室��� 㪠���� ~"0~"".
   RETURN vErrorMsg.
END FUNCTION.


/* ��४���� ����஫� (106) � (107) ����� */
PROCEDURE check148n-106-107:
   DEFINE INPUT  PARAMETER TABLE FOR ttNalRec BIND.
   DEFINE OUTPUT PARAMETER oErrorMsg AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vIsBudget AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vKBKNalog  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKCustom AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDescPokOP AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPassN     AS LOGICAL   NO-UNDO INITIAL YES.
   DEFINE VARIABLE vPassT     AS LOGICAL   NO-UNDO INITIAL YES.
   DEFINE VARIABLE vS         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vT         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI         AS INT64     NO-UNDO.


   FOR FIRST ttNalRec NO-LOCK:

      ASSIGN
         vKBKNalog  = FGetSetting("���","��������","")
         vKBKCustom = FGetSetting("���","��������","")
         vDescPokOP = GetCodeDesc("���:��",ttNalRec.PokOP,1,"").

      IF CAN-DO(vKBKNalog, ttNalRec.KBK) OR 
         CAN-DO(vKBKCustom,ttNalRec.KBK) 
      THEN vIsBudget = NO.
      ELSE RUN IsBudgetPaymentEx IN h_op(INPUT ttNalRec.ben-acct,
                                         INPUT ttNalRec.KBK,
                                         OUTPUT vIsBudget).

/*      /* ���� ���⥦ */                             */
/*      IF vIsBudget AND (ttNalRec.PokOp <> "0"            */
/*                     OR ttNalRec.PokNP <> "0") THEN      */
/*         oErrorMsg =  "��� ����� ���⥦�� � ����� " +*/
/*                      "(106) �᭮����� ���⥦� � " +     */
/*                      "(107) �������� ��ਮ� " +        */
/*                      "������ ���� 㪠��� ~"0~"".        */

      /* �������� ���⥦ */
      IF vIsBudget OR (CAN-DO(vDescPokOP, "�") AND CAN-DO(vKBKNalog,ttNalRec.KBK)) THEN 
      DO: 
         IF ttNalRec.PokNP = "0" THEN
            vPassN = YES.
         ELSE 
         DO:
            IF NUM-ENTRIES(ttNalRec.PokNP, ".")      = 3 AND
               LENGTH(ENTRY(1, ttNalRec.PokNP, ".")) = 2 AND
               LENGTH(ENTRY(2, ttNalRec.PokNP, ".")) = 2 AND
               LENGTH(ENTRY(3, ttNalRec.PokNP, ".")) = 4
            THEN 
            DO:
               ASSIGN
                  vS = GetCode("���:��", ENTRY(1, ttNalRec.PokNP, "."))
                  vT = ENTRY(2, ttNalRec.PokNP, ".")
               .
               IF {assigned vS} THEN 
               DO:
                  IF LENGTH(TRIM(vT)) = 2 THEN 
                  DO:
                     vI = INT64(vT) NO-ERROR.
                     vPassN = NOT ERROR-STATUS:ERROR 
                              AND vI >= 0                 
                              AND vI <= INT64(vS).
                  END.
               END.
               ELSE
                  vPassN = YES.
               vS = ttNalRec.PokNP.
               IF vPassN THEN
                  OVERLAY(vS, 1, 5, "CHARACTER") = "01.01".
               vPassN = isDate148n(vS).
            END.
            ELSE
               vPassN = NO.
         END.
         IF NOT vPassN THEN 
         DO:
            oErrorMsg = "��� ��������� ���⥦�� ���� &1 ������ ����� "         +
                        "���祭�� ~"0~" ���� �ଠ� ~"&2.&3.����~". � ��砥," +
                        " �᫨ &2 ��������� � �����䨪��� &4, ���祭�� " +
                        "�� �⮩ ����� �����䨪��� ��।���� "            +
                        "���ᨬ��쭮� ���祭�� &3. � ��⨢��� ��砥 �ଠ� " +
                        "���� � 楫�� ������ ᮮ⢥��⢮���� ���४⭮� ���".
            oErrorMsg = SUBSTITUTE(oErrorMsg,
                                   "(107) ��ਮ�",
                                   "XX",
                                   "��",
                                   "���:��").
         END.
      END.
      /* ⠬������ ���⥦ */
      ELSE IF CAN-DO(vDescPokOP, "�") AND CAN-DO(vKBKCustom,ttNalRec.KBK) THEN 
      DO:
         vPassT = isDigital148n(ttNalRec.PokNP, 8, NO).
         IF NOT vPassT THEN
            oErrorMsg = "��� ⠬������� ���⥦�� ���� (107) ��ਮ� " +
                        "������ ᮤ�ঠ�� 8-����� ��஢�� ���".
      END.

      IF NOT {assigned oErrorMsg} THEN
         oErrorMsg = check148n-kbk-pokop-poknp(ttNalRec.KBK, 
                                               ttNalRec.PokOP, 
                                               ttNalRec.PokNP).

   END.

   RETURN oErrorMsg.
END PROCEDURE.

PROCEDURE check148n-106-109:
   DEFINE INPUT  PARAMETER iPokOp     AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER iPokDD     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrorText AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrorType AS CHAR NO-UNDO.
 
   DEFINE VARIABLE vNalContrVal AS CHARACTER   NO-UNDO.
   vNalContrVal = GetCode("���:�����","106-109").

   IF     LOOKUP(vNalContrVal,"��,��") > 0
      AND iPokOp EQ "��"
      AND iPokDD NE "0"
   THEN
      ASSIGN 
         oErrorText = "�� �ன��� ��४���� ����஫� ����� " +
                      "(106) �᭮����� ���⥦� � (109) ���."
         oErrorType = IF vNalContrVal EQ "��"
                      THEN "-1"
                      ELSE "4"
      .

   RETURN.
   
END PROCEDURE.
&ENDIF /* DEF_148N_I_ */
/* $LINTFILE='148n.i' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='12/10/2016 13:47:17.523+03:00' */
/*prosignD9h1obJsLsz1HMxN5MIq5Q*/