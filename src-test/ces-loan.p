{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE INPUT PARAMETER iTag AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFileName AS CHARACTER NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-acct FOR loan-acct.

{tmprecid.def}

CASE iTag:
/* Выгружаем данные по клиенту договора в файл tag0.txt*/
WHEN "0" THEN 
   RUN ces-tag0.p (BUFFER tmprecid,INPUT iTag).  
WHEN "0N" THEN 
   RUN ces-tag0N.p (BUFFER tmprecid,INPUT "0").  
END CASE.

{intrface.del}

