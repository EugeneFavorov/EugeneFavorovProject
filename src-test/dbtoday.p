/*
               ��� "���� ����"
    filename: dbtoday.p
      comment: ����� ����᪠ �� �࠭���樨 � ���饬 ����樮���� ���.
      comment: �⠢���� ����� ��楤��� ����᪠ �࠭���樨. 
                ���� ��楤�� ����᪠ ��६�頥��� � �믮����� ��.
      Created: 27/05/2014
     Modified: ...
*/
{globals.i}
DEF INPUT  PARAM iDate    AS DATE        NO-UNDO.
DEF INPUT  PARAM iOprid   AS RECID       NO-UNDO.

DEF VAR iopkind as char no-undo.

FIND FIRST op-kind WHERE RECID(op-kind) EQ iOprid NO-LOCK NO-ERROR.

IF iDate <= TODAY THEN 
DO:
	iopkind = getxattrvalue ("op-kind", op-kind.op-kind, "��������").
    RUN VALUE(iopkind + '.p') (iDate,iOpRid).
END.
ELSE MESSAGE ('����� ����᪠�� ������ �࠭����� � ���饬 ���') VIEW-AS ALERT-BOX.

RETURN "NO".