DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.

MESSAGE "Внимание! Для начала действия пакета с сегодняшнего дня." SKIP 
	"Дата заведения клиента должна быть" today 
	VIEW-AS ALERT-BOX.