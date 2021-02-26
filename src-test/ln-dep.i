/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: LN-DEP.I
      Comment: ��ନ஢���� ���� ���᫥���.
               ��६ �� ���. ४����� ������� � �᫨ ���, � �� ��砫쭮�� ���祭��
               ४�����.  ���祭��(���� ��ࠬ��஢) �࠭���� �१ �������,����᪠����
               㪠�����  ��᪮�쪨� ��ࠬ��஢ �१ "+"
               ����� 㪠�뢠�� ��᪮�쪮 ��㯯 ��ࠬ��஢ ࠧ�������� "&" �� �⮬ १-��
               ���᫥��� �㤥� �㬬� ���᫥��� �� ������ ��㯯� ��ࠬ��஢ �� �⠢�� ��
               ४����� �����⠢, ��� 㪠�뢠���� �⠢�� ��� ������ ��㯯� �१ "&.
               ���� ���ᨬ��쭮 10 ��㯯.
            
               ������� - 0+7,....,0&7+13
               �����⠢ = %�।,..,%�����&%����
   Parameters:
         Uses:
      Used by:
      Created: 07.08.2008 15:13 Fepa 93525
*/

PROCEDURE chk-credit.

   DEF INPUT  PARAM iContract        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate            AS DATE NO-UNDO.
   DEF INPUT  PARAM iAcct            AS CHAR NO-UNDO.
   DEF INPUT  PARAM iamt-in-cur-acct AS DEC  NO-UNDO.
   DEF INPUT  PARAM iCorrAcct        AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oResult          AS CHAR NO-UNDO.

   DEF VAR vStopDopVRazresh AS CHAR NO-UNDO. /* �� �⮯��������� */
   DEF VAR vCorrCurr        AS CHAR NO-UNDO. /* ����� ����ᯮ������饣� ��� */

   DEF BUFFER bloan      FOR loan.      /* ���������� ����� */
   DEF BUFFER bloan-acct FOR loan-acct. /* ���������� ����� */
   DEF BUFFER loan-acct  FOR loan-acct. /* ���������� ����� */

   mb:
   DO ON ERROR UNDO, LEAVE:
      
      /* ��室�� ������� */
      FIND FIRST bloan WHERE bloan.contract  EQ iContract
                         AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
      DO:
         oResult = "�� ������ ������� � �����祭��� " + iContract + " ����஬ " + iContCode.
         LEAVE mb.
      END.

      /* ��室�� - �ਢ易� �� ��� � ������⭮�� �������� � ஫�� "�����" */
      FIND LAST bloan-acct WHERE bloan-acct.contract  EQ iContract
                             AND bloan-acct.cont-code EQ iContCode
                             AND bloan-acct.acct      EQ iAcct
                             AND bloan-acct.acct-type EQ "�����"
                             AND bloan-acct.since     LE iDate
      NO-LOCK NO-ERROR.

      /* �᫨ �� �ਢ易� ���, � �ய�᪠�� � �� �㣠���� */
      IF NOT AVAIL bloan-acct THEN
         LEAVE mb.

      /* �⠢�� ������� �஢��塞 ⮫쪮 � ��砥 �।�⮢���� ��� � ஫�� "�����" 
      ** � bloan-acct � ���� �������騩 ��� � ஫�� "�����", � ��।����� �।��㥬� ���
      ** �᫨ ��� �� ࠢ��, ����� �� ��㣮� ��� (� ஫��, �⫨筮� �� �����) */
      IF bloan-acct.acct EQ iAcct THEN
      DO:
         /* ��室�� �⠢�� ������� */
         FIND LAST comm-rate WHERE comm-rate.commission EQ "�������"
                               AND comm-rate.acct       EQ "0"
                               AND comm-rate.currency   EQ bloan.currency
                               AND comm-rate.kau        EQ iContract + "," + iContCode
                               AND comm-rate.min-value  EQ 0
                               AND comm-rate.period     EQ 0
                               AND comm-rate.since      LE iDate
         NO-LOCK NO-ERROR.
   
         /* �᫨ �⠢�� ��������, � �ࠢ������ �㬬� ���������� � �⠢���,
         ** �᫨ �㬬� ����. ����� �⠢�� � �㣠����, ���� �ய�᪠�� � �� �㣠���� */
         IF    AVAIL comm-rate 
           AND iamt-in-cur-acct < comm-rate.rate-comm 
           and bloan.open-date < iDate
         THEN DO:
            IF bloan.currency EQ "" THEN
               oResult = "�訡��: �㬬� ����� ������ ���� �� ����� " + STRING(comm-rate.rate-comm).
            ELSE
               oResult = "�訡��: �㬬� ����� ������ ���� �� ����� " + STRING(comm-rate.rate-comm) + "(" + bloan.currency + ")".
            LEAVE mb.
         END.
            /* �஢��塞, �㦭� �� �ந������� �஢���) */
         ASSIGN 
            vStopDopVRazresh = FGetSetting("�⮯���������", "", "")
            vCorrCurr = SUBSTRING(iCorrAcct, 6 ,3)
         .
         ASSIGN
            vCorrCurr = "" WHEN vCorrCurr EQ "810".
         FIND FIRST loan-acct WHERE 
                    loan-acct.contract  EQ iContract
                AND loan-acct.cont-code EQ iContCode
                AND loan-acct.acct      EQ iCorrAcct
                AND loan-acct.currency  EQ vCorrCurr
                AND CAN-DO (ENTRY(1, vStopDopVRazresh, ";"), loan-acct.acct-type)
                AND loan-acct.since     LE iDate
         NO-LOCK NO-ERROR.
         IF AVAIL loan-acct THEN
         DO:
            FIND FIRST bloan-acct WHERE 
                       bloan-acct.contract  EQ iContract
                   AND bloan-acct.cont-code EQ iContCode
                   AND bloan-acct.acct-type EQ loan-acct.acct-type
                   AND bloan-acct.since     GT loan-acct.since
                   AND bloan-acct.since     LE iDate
            NO-LOCK NO-ERROR.
            IF NOT AVAIL bloan-acct THEN
               LEAVE mb.
         END.
         IF     NUM-ENTRIES (vStopDopVRazresh, ";") GT 1 
            AND CAN-DO (ENTRY(2, vStopDopVRazresh, ";"), iCorrAcct) THEN
            LEAVE mb.
          /* ������ �஢�ਬ �⮯���� */
         FIND LAST comm-rate WHERE comm-rate.commission EQ "�⮯����"
                               AND comm-rate.acct       EQ "0"
                               AND comm-rate.currency   EQ bloan.currency
                               AND comm-rate.kau        EQ iContract + "," + iContCode
                               AND comm-rate.min-value  EQ 0
                               AND comm-rate.period     EQ 0
                               AND comm-rate.since      LE iDate
         NO-LOCK NO-ERROR.
    
         /* �᫨ ����, � �ய�᪠��, �� �㣠���� */
         IF NOT AVAIL comm-rate THEN
            LEAVE mb.
          
         IF iDate > bloan.end-date - comm-rate.rate-comm THEN
         DO:
            oResult = "�訡��: �������⥫�� ������ �� �������� ࠧ�襭� �� " + STRING(bloan.end-date - comm-rate.rate-comm).
            LEAVE mb.
         END.
      END.
   END. /* mb: */
END PROCEDURE. /*chk-credit*/

PROCEDURE chk-debet.

   DEF INPUT  PARAM iContract        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode        AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate            AS DATE NO-UNDO.
   DEF INPUT  PARAM iAcct            AS CHAR NO-UNDO.
   DEF INPUT  PARAM iamt-in-cur      AS DEC  NO-UNDO.
   DEF INPUT  PARAM iRidEntry        AS RECID NO-UNDO. /* �����䨪��� �஢���� */
   DEF OUTPUT PARAM oResult          AS CHAR NO-UNDO.

   DEF VAR vAcctOst AS DEC NO-UNDO. /* ���⮪ �� ��� */
   
   DEF BUFFER bloan FOR loan.           /* ���������� ����� */
   DEF BUFFER bloan-acct FOR loan-acct. /* ���������� ����� */
   DEF BUFFER bop-entry FOR op-entry. /* ���������� ����� */

   mb:
   DO ON ERROR UNDO, LEAVE:
      /* ��室�� ������� */
      FIND FIRST bloan WHERE bloan.contract  EQ iContract
                         AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
      DO:
         oResult = "�� ������ ������� � �����祭��� " + iContract + " ����஬ " + iContCode.
         LEAVE mb.
      END.

      /* �஢��塞 ���� ����砭�� �������. �᫨ ��� �஢���� ����� ��� ࠢ�� ���� ����砭��, 
      ** � �� �믮��塞 �஢��� */
      IF iDate >= bloan.end-date THEN
         LEAVE mb.

      /* ��室�� - �ਢ易� �� ��� � ������⭮�� �������� � ஫�� "�����" */
      FIND LAST bloan-acct WHERE bloan-acct.contract  EQ iContract
                             AND bloan-acct.cont-code EQ iContCode
                             AND bloan-acct.acct      EQ iAcct
                             AND bloan-acct.acct-type EQ "�����"
                             AND bloan-acct.since     LE iDate
      NO-LOCK NO-ERROR.

      IF NOT AVAIL bloan-acct THEN
         LEAVE mb.

      /* �஢�ઠ ���� ���筮�� ������   ivv */

      FIND LAST comm-rate WHERE comm-rate.commission EQ "�⮯����"
                            AND comm-rate.acct       EQ "0"
                            AND comm-rate.currency   EQ bloan.currency
                            AND comm-rate.kau        EQ iContract + "," + iContCode
                            AND comm-rate.min-value  EQ 0
                            AND comm-rate.period     EQ 0
                            AND comm-rate.since      LE iDate
      NO-LOCK NO-ERROR.
 
      IF AVAIL comm-rate THEN DO:
 
        IF iDate < bloan.open-date + comm-rate.rate-comm THEN
         DO:
            oResult = "�訡��: ���筮� ����⨥ ����᪠����, ��稭�� � " + STRING(bloan.open-date + comm-rate.rate-comm).
            LEAVE mb.
         END.
      END.

      /* �஢�ઠ �������쭮� �㬬� ���筮�� ������ ivv */

      FIND LAST comm-rate WHERE comm-rate.commission EQ "�������"
                            AND comm-rate.acct       EQ "0"
                            AND comm-rate.currency   EQ bloan.currency
                            AND comm-rate.kau        EQ iContract + "," + iContCode
                            AND comm-rate.min-value  EQ 0
                            AND comm-rate.period     EQ 0
                            AND comm-rate.since      LE iDate
      NO-LOCK NO-ERROR.
 
      /* �᫨ ����, � �ய�᪠��, �� �㣠���� */
      IF AVAIL comm-rate THEN DO:
        IF iamt-in-cur <= comm-rate.rate-comm THEN
         DO:
            oResult = "�訡��: �������쭠� �㬬� ���� " + STRING(comm-rate.rate-comm).
            LEAVE mb.
         END.
      END.




      /* �⠢�� ��᭎�� �஢��塞 ⮫쪮 � ��砥 ����⮢���� ��� � ஫�� "�����" 
      ** � bloan-acct � ��� �������騩 ��� � ஫�� "�����", � ��।����� �����㥬� ���
      ** �᫨ ��� �� ࠢ��, ����� �� ��㣮� ��� (� ஫��, �⫨筮� �� �����) */
      IF bloan-acct.acct EQ iAcct THEN
      DO:
         /* ��室�� �⠢�� ��᭎�� */
         FIND LAST comm-rate WHERE comm-rate.commission EQ "��᭎��"
                               AND comm-rate.acct       EQ "0"
                               AND comm-rate.currency   EQ bloan.currency
                               AND comm-rate.kau        EQ iContract + "," + iContCode
                               AND comm-rate.min-value  EQ 0
                               AND comm-rate.period     EQ 0
                               AND comm-rate.since      LE iDate
         NO-LOCK NO-ERROR.
   
         /* �᫨ ����, � �ய�᪠��, �� �㣠���� */
         IF NOT AVAIL comm-rate THEN
            LEAVE mb.
   
         RUN acct-pos IN h_base (iAcct, bloan.currency,
                                 iDate, iDate, CHR(251)).
         vAcctOst = (IF bloan.currency = "" THEN sh-bal ELSE sh-val ).
/*�஢�ઠ ��� �஢���� ����� ��� �� �㬬� �� �㦭� ��� ������ � sh-bal*/
         FIND FIRST bop-entry WHERE recid(bop-entry) EQ iRidEntry NO-LOCK NO-ERROR.
         IF AVAIL bop-entry THEN
            IF bop-entry.op-status GE CHR(251) THEN
               iamt-in-cur = 0.
   
         IF ABS(vAcctost) - iamt-in-cur < comm-rate.rate-comm THEN
         DO:
            IF bloan.currency EQ "" THEN
               oResult = "�訡��: ��᭨����� ���⮪ �� �������� " + STRING(comm-rate.rate-comm).
            ELSE
               oResult = "�訡��: ��᭨����� ���⮪ �� �������� " + STRING(comm-rate.rate-comm) + "(" + bloan.currency + ")".
            LEAVE mb.
         END.
      END.
   END. /* mb: */

END PROCEDURE. /*chk-debet*/

FUNCTION is-depmax RETURNS CHARACTER ( iContract AS CHAR,
                                       iContCode AS CHAR,
                                       iDate     AS DATE ):

   DEF VAR oResult AS INT64 NO-UNDO.

   DEF BUFFER bloan FOR loan. /* ���������� ����� */

   mb:
   DO ON ERROR UNDO, LEAVE:

      /* ��室�� ������� */
      FIND FIRST bloan WHERE bloan.contract  EQ iContract
                         AND bloan.cont-code EQ iContCode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bloan THEN
         LEAVE mb.

      /* �஢��塞, �������� �� �⠢�� %������� */
      FIND FIRST comm-rate WHERE comm-rate.commission EQ "%�������"
                             AND comm-rate.acct       EQ "0"
                             AND comm-rate.currency   EQ bloan.currency
                             AND comm-rate.kau        EQ iContract + "," + iContCode
                             AND comm-rate.min-value  EQ 0
                             AND comm-rate.period     EQ 0
      NO-LOCK NO-ERROR.

      IF AVAIL comm-rate THEN
         oResult = 1.
   END. /* mb: */

   RETURN STRING(oResult).

END FUNCTION. /*is-depmax*/
/* $LINTUSER='BIS' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='*' */
/* $LINTDATE='09/09/2014 15:54:22.639+04:00' */
/* $LINTFILE='ln-dep.i' */
/*prosignTz5oLJ3PU4G51A1hzaifoA*/