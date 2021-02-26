{globals.i}
{sh-defs.i new}
{intrface.get trans}
{intrface.get pbase}


{globals.i}

DEFINE VARIABLE mTmpStr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSpisok  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTransh  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI       AS INT64     NO-UNDO.
DEFINE VARIABLE mDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mOst     AS DECIMAL   NO-UNDO.

DEFINE BUFFER uploan    FOR loan.
DEFINE BUFFER oldtransh FOR loan.
DEFINE BUFFER newtransh FOR loan.
DEFINE BUFFER oldacct FOR loan-acct.
DEFINE BUFFER newacct FOR loan-acct.

FUNCTION transh1 RETURNS CHARACTER (INPUT iCC AS CHARACTER) FORWARD.

DEFINE STREAM err.
{setdest.i &stream   = "stream err" &filename = "'move_ost.log'" &nodef="/*"}

ASSIGN
   mDate = TODAY
   mSpisok = "047-��;048-��;051-��;052-��;053-��;055-��"
   .
mc:
DO mI = 1 TO NUM-ENTRIES(mSpisok,";"):
   ENTRY(mI,mSpisok,";") = TRIM(ENTRY(mi,mSpisok,";")) + "@0000".
   FIND FIRST uploan WHERE uploan.contract  EQ "�।��"
      AND uploan.cont-code EQ ENTRY(mI,mSpisok,";")
      NO-LOCK NO-ERROR.
   IF AVAILABLE uploan THEN
   DO:

      mTransh = transh1(uploan.cont-code).

      FIND LAST newacct WHERE newacct.contract  EQ uploan.contract
         AND newacct.cont-code EQ mTransh
         AND newacct.acct-type EQ uploan.contract
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE newacct THEN
      DO:
         PUT STREAM err UNFORMATTED mTransh " ��㤭�� �� ������!" SKIP.
         NEXT mc.
      END.
      PUT STREAM err UNFORMATTEd SUBSTITUTE("������� &1. ��७�ᨬ �� ���⪨ �� �࠭� &2",uploan.cont-code,mTransh) SKIP.
      FOR EACH oldtransh WHERE oldtransh.contract EQ uploan.contract
         AND oldtransh.cont-code BEGINS uploan.cont-code + " "
         AND oldtransh.cont-code NE newacct.cont-code
         AND oldtransh.close-date EQ ?
         NO-LOCK:

         FIND LAST oldacct WHERE oldacct.contract  EQ oldtransh.contract
            AND oldacct.cont-code EQ oldtransh.cont-code
            AND oldacct.acct-type EQ uploan.contract
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE oldacct THEN
         DO:
            PUT STREAM err UNFORMATTEd oldtransh.cont-code " ��㤭�� �� ������!" SKIP.
            NEXT mc.
         END.
         RUN acct-pos IN h_base (
            oldacct.acct,
            oldtransh.currency,
            mDate,
            mDate, 
            "�").

         most = (IF newacct.currency = "" THEN sh-bal ELSE sh-val ) .
         RUN opcreate IN THIS-PROCEDURE.
                                      
      END.

   END.
   ELSE
      PUT STREAM err UNFORMATTEd ENTRY(mI,mSpisok,";") " �� ������!" SKIP.
END.

{preview.i &stream   = "stream err" &filename = "'move_ost.log'"}

PROCEDURE opcreate.
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-acctdb",
      newacct.acct).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-acctcr",
      oldacct.acct).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-op-details",
      SUBSTITUTE("��७�� ��㤭�� ������������ � �࠭� &1 �� �࠭� &2 � �裡 � �������ਧ�楩  �� 16.09.16",oldacct.cont-code,newacct.cont-code)).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-op-date",
      STRING(mDate)).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-filial",
      shFilial).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-curr",
      uploan.currency).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-ssum",
      STRING(most)).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-cont1",
      newacct.cont-code).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,
      "in-cont2",
        oldacct.cont-code).

   IF GetBaseOpDate() EQ ? THEN RUN InitBaseLibrary IN h_pbase (?,mDate,?).

   RUN SetSysconf IN h_Base ("�������������� ����������","��").

   RUN RunTransaction IN h_pbase ("simple_op") NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      PUT STREAM err UNFORMATTEd SUBSTITUTE("�訡�� ��७�� &1 c &2 �� &3",most,oldacct.acct,newacct.acct) SKIP.
   ELSE
      PUT STREAM err UNFORMATTEd SUBSTITUTE("��७�ᥭ� &1 c &2 �� &3",most,oldacct.acct,newacct.acct) SKIP.

END PROCEDURE.

FUNCTION transh1 RETURNS CHARACTER (INPUT iCC AS CHARACTER):
DEFINE BUFFER loan FOR loan.
   DEFINE VARIABLE vTransh AS CHARACTER NO-UNDO.
   FOR EACH loan WHERE loan.contract EQ "�।��" AND loan.cont-code BEGINS iCC + " "
      AND loan.close-date EQ ? BY loan.open-date:
      vTransh = loan.cont-code.
      LEAVE.  
   END.
   RETURN vTransh.
END FUNCTION.
