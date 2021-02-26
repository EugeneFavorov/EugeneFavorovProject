/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: op-user1.p
      Comment: Выбор пользователей для поля "Контролер"
   Parameters:
         Uses:
      Used by:
      Created: 01/04/2002 Olenka 
     Modified: 13/05/04 Ilvi (25887) Переведен на browseld.p
     Modified: 08/09/2004 Om Доработка: Отметка выбранных записей.
*/

{globals.i}

DEFINE INPUT PARAMETER iLevel     AS INT64 NO-UNDO.
DEFINE INPUT PARAMETER mBranchID  AS CHAR  NO-UNDO.

DEFINE SHARED VARIABLE list-id AS CHARACTER NO-UNDO.

{tmprecid.def}
{empty tmprecid}

def var tBranch as char no-undo.
tBranch = mBranchID.
if tBranch begins "05" then do:
   RUN browseld.p("_user","FilialId" + CHR(1) + "sc-1"  + CHR(1) + "sv-1" +  CHR(1) + "sv-3" ,
                           shFilial  + CHR(1) + "ОтделениеТемп" + CHR(1) + tBranch + CHR(1) + "no","",4).
end.
else do:
   RUN browseld.p("_user","FilialId" + CHR(1) + "sc-1"  + CHR(1) + "sv-1" +  CHR(1) + "sv-3" ,
                           shFilial  + CHR(1) + "Отделение" + CHR(1) + tBranch + CHR(1) + "no","",4).
end.
IF     LASTKEY EQ 10
   AND pick-value NE ? THEN
list-id = pick-value.
{empty tmprecid}
