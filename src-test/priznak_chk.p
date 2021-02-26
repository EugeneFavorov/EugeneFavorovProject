DEFINE INPUT PARAMETER iCode AS CHARACTER NO-UNDO.
/*
AKO_BNV		Бадзюн Наталья Владиславовна 
K0400IEV	Боровская Елена Владимировна 
AKO_VNV		Васильева Наталья Валентиновна 
OIK_KEA		Копыца Евгения Александровна 
0000KDN		Кунц Дина Николаевна 
U0400LEP	Новоселова Елена Павловна
AKO_CKS		Чередова Ксения Сергеевна 
AKO_SEL		Шмеляк Елизавета Леонидовна 
*/


{globals.i}
{intrface.get tmess}

IF iCode EQ "Priznak" AND NOT CAN-DO("AKO_BNV,K0400IEV,AKO_VNV,OIK_KEA,0000KDN,U0400LEP,AKO_CKS,AKO_SEL,0000GAA,I0400kam,I0400STS,I0400FEV",userid('bisquit')) THEN
DO:
      RUN Fill-SysMes("","","","У вас нет прав на ручное изменение реквизита " + iCode). 
      RETURN "ERROR".
END.

{intrface.del}

RETURN.
