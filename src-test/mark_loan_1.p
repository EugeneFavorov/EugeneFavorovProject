/*
  ���客
*/

{globals.i}
{intrface.get xclass}

DEFINE VARIABLE mStr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPriznak AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE mDr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI       AS INT64     NO-UNDO.

DEFINE BUFFER loan FOR loan.





{getfile.i &filename="'/data/home/bq41d/samba/192.168.137.217/expo/loan.csv'" &mode=must-exist }

mPriznak = "���".

DEFINE FRAME iParam 
   mPriznak   LABEL "�ਧ��� "
   WITH CENTERED SIDE-LABELS ROW 10 1 COL OVERLAY TITLE COLOR bright-white "[ ��⠭���� �� ���� ]".

PAUSE 0.
UPDATE mPriznak
   WITH FRAME iParam. 
IF LAST-EVENT:FUNCTION  EQ "END-ERROR" THEN 
DO:
   HIDE FRAME iParam NO-PAUSE.
   RETURN.
END.



INPUT FROM VALUE(fname) CONVERT SOURCE "1251".
{setdest.i &filename='mark_loan.log'}
REPEAT:
   IMPORT UNFORMATTED mStr.
   FIND FIRST loan
      WHERE loan.filial-id EQ shFilial
      AND loan.contract EQ "�।��"
      AND loan.cont-code EQ AddFilToLoan(mStr, shFilial)
      NO-LOCK NO-ERROR.
   IF AVAILABLE(loan) THEN
   DO:
      mDr = GetXattrValueEx("loan",
         loan.contract + "," + loan.cont-code,
         "����",
         "").
      IF  mDr NE "" THEN
      DO:
         PUT UNFORMATTED
            ENTRY(1, loan.cont-code, "@") FORMAT "X(33)"
            "�訡��! ��� ���� �ਧ��� " mDr SKIP.
      END.
      ELSE
      DO:
         IF UpdateSigns(loan.class-code,
            loan.contract + "," + loan.cont-code,
            "����",
            mPriznak,
            ?)
         THEN
            mI = mI  + 1.
         ELSE
            PUT UNFORMATTED
               ENTRY(1, loan.cont-code, "@") FORMAT "X(33)"
               "�訡��!  �ਧ��� �� 㤠���� ��⠭�����"
               SKIP.
      END.    
   END.
   ELSE
      PUT UNFORMATTED
         mStr FORMAT "X(33)"
         "�訡��!  ������� �� ������"
         SKIP.
            
END.    
INPUT CLOSE.
IF mI GT 0 THEN
   PUT UNFORMATTED "�ਧ����� �ᯥ譮 ��⠭������ " mI  SKIP.

{preview.i &filename='mark_loan.log'}
{intrface.del}
