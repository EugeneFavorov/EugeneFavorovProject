DEFINE INPUT PARAMETER iCode AS CHARACTER NO-UNDO.
/*
-Герман Ю.С.   0500GYS
-Шаруха С.А.   0500SSA
-Мухина В.Ю.   0500MVY 
-Половинкина Е.В. 0500PEV  
-Нестеркина Л.А.  0500NLA 
*/


{globals.i}
{intrface.get tmess}

IF iCode EQ "НеБлокСумКарт" AND NOT CAN-DO("0500NLA,0500PEV,0500MVY,0500GYS,0500SSA,0000GAA",userid('bisquit')) THEN
DO:
      RUN Fill-SysMes("","","","У вас нет прав на ручное изменение реквизита " + iCode). 
      RETURN "ERROR".
END.

{intrface.del}

RETURN.
