/*

     Filename: vedprov.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 27.12.2013 KAU
     Modifier: 14.10.2014 KAU изменил определение пользователя к которому привязывать документы     

*/
DEFINE INPUT PARAM iParam AS CHARACTER NO-UNDO.
DEFINE VAR ipCity   AS CHARACTER NO-UNDO.

ipCity = ENTRY (1, iParam, ";").


{titul.def}
{globals.i}

{vedprov_t.i}

/*На этом расчёт закончен делаем вывод*/
if entry(2,iParam,";") <> 'ВСЕ' then
	DO:
	{vedprov.i}
	END.
else DO:
	{vedprovse.i}
	END.


/*{endout3.i &nofooter=yes}*/


