DEFINE INPUT PARAMETER iCode AS CHARACTER NO-UNDO.
/*
-��ଠ� �.�.   0500GYS
-����� �.�.   0500SSA
-��娭� �.�.   0500MVY 
-����������� �.�. 0500PEV  
-����ન�� �.�.  0500NLA 
*/


{globals.i}
{intrface.get tmess}

IF iCode EQ "�������㬊���" AND NOT CAN-DO("0500NLA,0500PEV,0500MVY,0500GYS,0500SSA,0000GAA",userid('bisquit')) THEN
DO:
      RUN Fill-SysMes("","","","� ��� ��� �ࠢ �� ��筮� ��������� ४����� " + iCode). 
      RETURN "ERROR".
END.

{intrface.del}

RETURN.
