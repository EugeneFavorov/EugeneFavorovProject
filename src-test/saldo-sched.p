/*
    Copyright:  (C) 1990-2016 ��� "���� ����"
    Filename:  saldo-plus.p
    Comment:  ���줨஢���� ���⪮�. ���ᠭ�� ⥪�饩 �㬬�
*/
{globals.i}
{sh-defs.i}

DEFINE VARIABLE mKindRID AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFilial  AS CHARACTER NO-UNDO.

mFilial = shFilial.

FIND FIRST op-kind WHERE
   op-kind.op-kind EQ "���줮���"
NO-LOCK NO-ERROR.

shFilial = "0000".
IF AVAIL(op-kind) THEN
   RUN g-saldo-plus.p (TODAY - 1,RECID(op-kind),NO) NO-ERROR.

shFilial = "0300".
IF AVAIL(op-kind) THEN
   RUN g-saldo-plus.p (TODAY - 1,RECID(op-kind),NO) NO-ERROR.

shFilial = "0500".
IF AVAIL(op-kind) THEN
   RUN g-saldo-plus.p (TODAY - 1,RECID(op-kind),NO) NO-ERROR.

shFilial = mFilial.

IF ERROR-STATUS:ERROR
   THEN RETURN ERROR "error".
   ELSE RETURN.
/******************************************************************************/
