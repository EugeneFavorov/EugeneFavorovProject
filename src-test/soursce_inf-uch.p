DEFINE INPUT PARAMETER iValue AS CHARACTER NO-UNDO.
 
 if not can-do("Со слов клиента,Самостоятельно из анализа документов,Из всех источников",iValue) then 
RETURN ERROR   "Неверное заполнение реквизита" . 