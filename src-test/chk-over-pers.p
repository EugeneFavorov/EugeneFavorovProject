{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get blkob}

{sh-defs.i}

DEFINE INPUT  PARAMETER iPersID AS INT64   NO-UNDO.
DEFINE OUTPUT PARAMETER iOk   AS LOGICAL NO-UNDO.

DEFINE VARIABLE mCustID AS INT64   NO-UNDO.

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","Begin").

RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","iPersID = " + STRING(iPersID)).

RUN ChkOver(iPersID).

iOk = YES.

{intrface.del}   

PROCEDURE ChkOver:
   DEFINE INPUT PARAMETER iCustID AS INT64 NO-UNDO.
   
   DEFINE VARIABLE vAnswer AS LOGICAL NO-UNDO.
      
   FOR EACH code WHERE TRUE
      AND code.class  EQ "Over-Mir"
      AND code.parent EQ "Over-Mir"
      AND code.val    EQ STRING(iCustID)
      AND code.description[2] NE ""
      AND code.description[3] NE ""
      NO-LOCK:
      LEAVE.
   END.
   
   IF AVAIL(code)
   AND code.misc[1] EQ "NO" THEN
   DO:
      MESSAGE
         "�������: " + TRIM(code.name) + ", �����: " + TRIM(code.val) + " CID: " + TRIM(code.code) +
         "~n����室��� �������� �ᯮ�殮��� �� ᯨᠭ�� ����㯫����" +
         "~n � ����� ��� � ��� ��襭�� �������, ����� �������: " + code.description[1] + "." +
         "~n��ᯥ�⠩� ��ᯮ�殮��� � Payment 10 � ������� � ������."
         "~n��᫥ �� ������� �࠭����� r90909."
      VIEW-AS ALERT-BOX.
   END.
END PROCEDURE.
