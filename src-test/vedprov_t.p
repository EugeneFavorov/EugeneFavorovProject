/*

     Filename: vedprov.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 27.12.2013 KAU
     Modifier: 14.10.2014 KAU ������� ��।������ ���짮��⥫� � ���஬� �ਢ�뢠�� ���㬥���     

*/
DEFINE INPUT PARAM iParam AS CHARACTER NO-UNDO.
DEFINE VAR ipCity   AS CHARACTER NO-UNDO.

ipCity = ENTRY (1, iParam, ";").


{titul.def}
{globals.i}

{vedprov_t.i}

/*�� �⮬ ����� �����祭 ������ �뢮�*/
if entry(2,iParam,";") <> '���' then
	DO:
	{vedprov.i}
	END.
else DO:
	{vedprovse.i}
	END.


/*{endout3.i &nofooter=yes}*/


