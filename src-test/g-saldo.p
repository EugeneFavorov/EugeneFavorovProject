/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright:  (C) 1992-1996 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename:  g-saldo.p
      Comment:  ���줨஢���� ���⪮�. ���ᠭ�� ⥪�饩 �㬬�
                ���室���.
      Created:  12/05/2003 NIK
*/
DEFINE INPUT PARAMETER iOpDate   LIKE  op.op-date  NO-UNDO.
DEFINE INPUT PARAMETER iKindRID  AS    RECID       NO-UNDO.

RUN g-saldo2.p (iOpDate,iKindRID,NO) NO-ERROR.   /* ��� ��� ���� ��        */
IF ERROR-STATUS:ERROR
   THEN RETURN ERROR "error".
   ELSE RETURN.
/******************************************************************************/
