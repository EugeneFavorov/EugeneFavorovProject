/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: LISTAVAL.P
      Comment: �� ��� 13529
   Parameters:
         Uses:
      Used by:
      Created: 14.04.2003 17:33 ilvi    
*/

DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

DEFINE  VARIABLE i AS INT64 NO-UNDO.
{intrface.get xclass}


DO i = 1 TO NUM-ENTRIES(iValue):
   IF GetCode(iParam,ENTRY(i,iValue)) EQ ? THEN 
   DO:
      MESSAGE ENTRY(i,iValue)
      VIEW-AS ALERT-BOX.
      RETURN ERROR.
   END.
END.

{intrface.del}
