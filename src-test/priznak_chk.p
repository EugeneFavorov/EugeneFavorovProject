DEFINE INPUT PARAMETER iCode AS CHARACTER NO-UNDO.
/*
AKO_BNV		����� ��⠫�� �����᫠����� 
K0400IEV	��஢᪠� ����� �������஢�� 
AKO_VNV		��ᨫ쥢� ��⠫�� �����⨭���� 
OIK_KEA		����� ������� ����ᠭ�஢�� 
0000KDN		��� ���� ���������� 
U0400LEP	����ᥫ��� ����� ��������
AKO_CKS		��।��� �ᥭ�� ��ࣥ���� 
AKO_SEL		����� �������� ���������� 
*/


{globals.i}
{intrface.get tmess}

IF iCode EQ "Priznak" AND NOT CAN-DO("AKO_BNV,K0400IEV,AKO_VNV,OIK_KEA,0000KDN,U0400LEP,AKO_CKS,AKO_SEL,0000GAA,I0400kam,I0400STS,I0400FEV",userid('bisquit')) THEN
DO:
      RUN Fill-SysMes("","","","� ��� ��� �ࠢ �� ��筮� ��������� ४����� " + iCode). 
      RETURN "ERROR".
END.

{intrface.del}

RETURN.
