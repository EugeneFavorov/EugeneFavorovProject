/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-POP.P
      Comment: ������⥪� �㭪権 ����� ��� ���㬥�⮢
   Parameters: ���
         Uses:
      Used by:
      Created: 25.07.2007 10:21 koch
     Modified: 25.07.2007 10:21 koch     <comment>
*/


{globals.i}
{tmprecid.def}

/* ������⥪� ��� ࠡ��� � ���஬ �⠭���⭮� �࠭���樨. */
{intrface.get trans}
/* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get xclass}   
{intrface.get tmess}

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "POP"
   &LIBNAME       = "������⥪� �㭪権 ����� ��� ���㬥�⮢"
   &DESCRIPTION   = "����ন� �㭪樨 ����� ���  ���㬥�⮢: 
                     ����� ���㬥�⮢ "
   }


{pfuncdef
   &NAME          = "��������_��������"
   &DESCRIPTION   = "��室�� ���㬥��, �஢����, �� ��� ���㬥��
                     � ��楤�� ᮮ⢥������ ��� ���� (⠡��� doc-type
                     '����⭮-������� ���㬥���'), ����᪠�� ��楤���
                     � ��ࠬ��஬ RECID(op)."
   &PARAMETERS    = "iOp,iProc"
   &RESULT        = "1/0"
   &SAMPLE        = "��������_��������(2854874,'def_proc')"
   }

   DEFINE INPUT  PARAMETER iOp         AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iProc       AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.

   /* �ᯮ����⥫쭠� */
   DEFINE VARIABLE         vI           AS INT64     NO-UNDO. 

   DEFINE BUFFER op       FOR op.
   DEFINE BUFFER doc-type FOR doc-type.
   
   FIND FIRST op WHERE
      op.op EQ iOp NO-LOCK NO-ERROR.
   IF AVAIL op THEN
   DO:
      iOp = recid(op).

      FIND FIRST doc-type OF op NO-LOCK NO-ERROR.
      IF AVAIL doc-type
         AND   doc-type.printout NE "" THEN
      DO:
         IF    iProc = ?
            OR iProc = "" THEN
               iProc = trim(doc-type.printout).

         IF doc-type.printout NE iProc THEN
         DO vI = 1 TO 8:
            IF CAN-DO(doc-type.proc-print[vI], iProc) THEN
            DO:
               ASSIGN
                  out_Result = "YES"
                  is-ok      = 1
               .

               CREATE tmprecid.
               tmprecid.id = iOp.

               RUN VALUE(iProc + ".p") (iOp).
            END.
         END.
         ELSE
         DO:
            CREATE tmprecid.
            tmprecid.id = iOp.

            RUN VALUE(iProc + ".p") (iOp).
            ASSIGN
               out_Result = "YES"
               is-ok      = 1
            .
         END.
      END.
      ELSE out_Result = "NO".
      RETURN out_Result.
   END.
{empty tmprecid}
END PROCEDURE.

{pfuncdef
   &NAME          = "�����_���"
   &DESCRIPTION   = "��⠭�������� �������⥫�� ४����� ���㬥�� ��� ~
   �࠭���権 � filtertable"
   &PARAMETERS    = "��� �������⥫쭮�� ४�����, 
                     ���祭�� �������⥫쭮�� ४�����"
   &SAMPLE        = "�����_���(CardStatus,���)"
   }
   DEFINE INPUT  PARAMETER iCode          AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iValue         AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result     AS CHARACTER  NO-UNDO INIT ?.
   DEFINE OUTPUT PARAMETER is-ok          AS INT64    NO-UNDO INIT -1.
   
   DEFINE BUFFER op       FOR op.
   DEFINE BUFFER op-entry FOR op-entry.

   DEF VAR vInstance    AS HANDLE NO-UNDO.
   DEF VAR vOpenROWID   AS ROWID  NO-UNDO.
   DEF VAR vBuffer      AS HANDLE NO-UNDO.

   PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:
      ASSIGN
         vInstance = GetTransObject("FilterProc")
         vBuffer   = vInstance:DEFAULT-BUFFER-HANDLE
      .

      vBuffer:FIND-FIRST().

      ASSIGN
         vInstance = vBuffer  :BUFFER-FIELD("filtertable"):BUFFER-VALUE
         vBuffer   = vInstance:DEFAULT-BUFFER-HANDLE
      .
      vBuffer:FIND-FIRST().
      vOpenROWID  = vBuffer:BUFFER-FIELD("__rowid"):BUFFER-VALUE NO-ERROR.

      FIND FIRST op-entry WHERE ROWID(op-entry) EQ vOpenROWID
      NO-LOCK NO-ERROR.

      IF NOT AVAIL op-entry THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������� �஢����.").
         LEAVE PROC.
      END.

      FIND FIRST op WHERE op.op EQ op-entry.op
      NO-LOCK NO-ERROR.

      IF NOT AVAIL op THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", "�� ������ ���㬥��.").
         LEAVE PROC.
      END.

      IF NOT UpdateSigns(op.class-code, STRING(op.op), iCode, iValue, ?) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", 
            "�訡�� ��⠭���� �������⥫쭮�� ४����� " + iCode).
         LEAVE PROC.
      END.

      ASSIGN
         out_Result  = ""
         is-ok       = 0
      .
   END.

   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "�����������"
   &DESCRIPTION   = " "
   &PARAMETERS    = "iOp,iClass"
   &RESULT        = " "
   &SAMPLE        = " "
   }

   DEFINE INPUT  PARAMETER iOp         AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iClass      AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS LOG   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
   DEF BUFFER op FOR op.
   FIND FIRST op WHERE op.op = iOp NO-LOCK NO-ERROR.
   IF AVAIL(op) THEN
   DO:
      out_Result = CAN-DO(GetXclassAllChildsEx(op.class-code),iClass).
      IF out_Result THEN
      DO:
         FIND CURRENT op EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         ON WRITE OF op OVERRIDE DO: END.
         op.class-code = iClass.
         RELEASE op.
         ON WRITE OF op REVERT.
      END.
      ELSE
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "����� " + QUOTER(iClass) +
           " �� ���� �������騬 ����ᮬ ����� " + QUOTER(op.class-code)).
         is-ok = - 1.
      END.
   END.
   ELSE
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "���㬥�� �� ������!").
      is-ok = - 1.
   END.
END PROCEDURE.

{pfuncdef
   &NAME          = "�����"
   &DESCRIPTION   = "��楤�� ��㯯�஢�� �஢���� � ���� ���㬥�� 
                     �� �������� ��᪥ � ��஭� �஢����"
   &PARAMETERS    = "iOp-Kind,iOp-Date,iMaskAcct,iSide"
   &RESULT        = " "
   &SAMPLE        = "�����("094011",DATE("31/03/2012"),"70601",no)"
   }

   DEFINE INPUT  PARAMETER iOp-Kind    AS CHAR   NO-UNDO.
   DEFINE INPUT  PARAMETER iOp-Date    AS DATE   NO-UNDO.
   DEFINE INPUT  PARAMETER iMaskAcct   AS CHAR   NO-UNDO.
   DEFINE INPUT  PARAMETER iSide       AS LOG    NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS LOG    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64  NO-UNDO.

   RUN obdoc.p (iOp-Kind,
                iOp-Date,
                iMaskAcct,
                iSide).

 END PROCEDURE.

{pfuncdef
   &NAME          = "�㬬��"
   &DESCRIPTION   = "�㬬� �஢���� ���㬥�⮢, �⮡࠭��� �� 䨫���� 
                    (� �㡫���� ��������)"
   &PARAMETERS    = "䨫���"
   &RESULT        = "DEC"
   &SAMPLE        = "�㬬��(@filtertable(10))"
   }

   DEFINE INPUT  PARAMETER iFilterTbl  AS CHAR         NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DEC  INIT 0  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT  INIT -1 NO-UNDO.

   DEFINE BUFFER op       FOR op.
   DEFINE BUFFER op-entry FOR op-entry.

   DEFINE VARIABLE vFilterTable AS HANDLE NO-UNDO.

   IF {assigned iFilterTbl} THEN
      vFilterTable = WIDGET-HANDLE(iFilterTbl) NO-ERROR.

   IF NOT VALID-HANDLE(vFilterTable) THEN DO:
      out_Result = 0.
      RETURN.
   END.

   /* �⡮� ���㬥�⮢ �� 䨫���� */
   RUN fltrecid.p (vFilterTable, "op") NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN.

   FOR EACH tmprecid NO-LOCK,
   FIRST op WHERE recid(op) = tmprecid.id NO-LOCK,
   EACH op-entry OF op NO-LOCK:
      ACCUMULATE op-entry.amt-rub (total).
   END.

   out_Result = ACCUM total op-entry.amt-rub.
   is-ok = 1.

END PROCEDURE.
{pfuncdef
   &NAME          = "����_������"
   &DESCRIPTION   = "�����頥� �ਧ���, ���� ���⥦ ���� ��� ���"
   &PARAMETERS    = "iOp"
   &RESULT        = "LOG"
   &SAMPLE        = "����_������(@Op)"
   }

   DEFINE INPUT  PARAMETER iOp         AS INT64     NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS LOG       NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
   DEFINE VARIABLE vBalBudgetKBK AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBalBudget    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBK          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKPos       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKValue     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKPItem     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKValItem   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKItem      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKBKValIt     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBenAcct      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBAcct        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBalIskl      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIAcct        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIOch         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBegDiap      AS INT64     NO-UNDO.
   DEFINE VARIABLE vEndDiap      AS INT64     NO-UNDO.
   DEFINE VARIABLE vKBKItInt     AS INT64     NO-UNDO.
   DEFINE VARIABLE vI            AS INT64     NO-UNDO.
   DEFINE VARIABLE vJ            AS INT64     NO-UNDO.
   
   
   DEF BUFFER op FOR op.
   
   FIND FIRST op WHERE op.op = iOp NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN RETURN.
   
   vBalBudget    = FGetSetting("���","bal-budget","").
   vBalBudgetKBK = FGetSetting("���","bal-budget-kbk", "").
   vBalIskl      = FGetSetting("���","bal-iskl","").
   
   vBAcct = ENTRY(1,vBalBudgetKBK,"|").
   vIAcct = ENTRY(1,vBalIskl,"|").
   
   vBenAcct = TRIM(GetXAttrValueEx("op", STRING(op.op), "acct-rec", "")).
   
/*   IF NOT {assigned vBalBudgetKBK} THEN RETURN.*/
/*   IF NOT {assigned vBAcct} THEN RETURN.       */

   IF    CAN-DO(vBalBudget,op.ben-acct)
      OR CAN-DO(vBalBudget,vBenAcct) THEN
      out_Result = YES.
   ELSE 
   DO:
      IF     {assigned vBAcct}
         AND {assigned vBalBudgetKBK}
         AND 
         (      
            CAN-DO(vBAcct,vBenAcct)
         OR CAN-DO(vBAcct,op.ben-acct)
         ) THEN
      DO:
         ASSIGN
            vKBKValue = ENTRY(2, vBalBudgetKBK, "|")
            vKBKPos   = FGetSetting("���", "kbk-pos", "")
            vKBK      = TRIM(GetXAttrValueEx("op", STRING(op.op), "���", ""))
         .
         IF    NOT {assigned vKBKValue}
            OR NOT {assigned vKBKPos}
            OR NOT {assigned vKBK} THEN RETURN.
         
         DO vI = 1 TO NUM-ENTRIES(vKBKPos,";"):
            ASSIGN
               vKBKPItem   = ENTRY(vI,vKBKPos,";")
               vKBKValItem = ENTRY(vI,vKBKValue,";")     
               vKBKItem    = SUBSTRING(vKBK, INT64(ENTRY(1, vKBKPItem)), 
                                             INT64(ENTRY(2, vKBKPItem)))
            NO-ERROR.
            
            IF ERROR-STATUS:ERROR THEN NEXT.
            
            IF CAN-DO(vKBKValItem,vKBKItem) THEN 
            DO:   
               ASSIGN 
                  out_Result = YES
                  is-ok      = 1
               .
               RETURN.
            END.
            ELSE
            DO vJ = 1 TO NUM-ENTRIES(vKBKValItem):
               ASSIGN
                  vKBKValIt = ENTRY(vJ,vKBKValItem)
                  vKBKItInt = INT64(vKBKItem)
                  vBegDiap  = INT64(ENTRY(1, vKBKValIt, "-"))
                  vEndDiap  = INT64(ENTRY(2, vKBKValIt, "-"))
               NO-ERROR.

               IF ERROR-STATUS:ERROR THEN NEXT.

               IF INDEX(vKBKValIt, "-") GT 0 
                  AND vKBKItInt >= vBegDiap 
                  AND vKBKItInt <= vEndDiap THEN
               DO:
                  ASSIGN 
                     out_Result = YES
                     is-ok      = 1
                  .
                  RETURN.
               END.
            END.
         END.
         IF     {assigned vBalIskl}
            AND {assigned vIAcct} 
            AND (  CAN-DO(vIAcct,vBenAcct)
                OR CAN-DO(vIAcct,op.ben-acct)
            ) THEN
         DO:
            vIOch = ENTRY(2,vBalIskl,"|").
            out_Result = YES.
            DO vI = 1 TO NUM-ENTRIES(vIOch,","):
               IF ENTRY(vI,vIOch) EQ op.order-pay THEN 
               DO:
                  out_Result = NO. 
                  LEAVE.
               END.
            END.
         END.
      END.
   END.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='20/10/2014 11:55:11.700+04:00' */
/* $LINTFILE='pp-pop.p' */
/*prosign5yYbX+AjLoqfzceGUFULGg*/