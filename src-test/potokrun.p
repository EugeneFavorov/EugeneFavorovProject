/*
#  1 - начальный поток
#  2 - конечный  поток
#  3 - количество потоков
#  4 - филиал
#  5 - список транзакций через ;
#  6 - на всякий случай
*/
&scope potokscript /home2/bis/quit41d/src-auto/k14.sh

DEF INPUT PARAM  iBegPotok AS INT64 NO-UNDO.
DEF INPUT PARAM  iEndPotok AS INT64 NO-UNDO.
DEF INPUT PARAM  iDelim    AS INT64 NO-UNDO.
DEF INPUT PARAM  iFilial   AS CHAR  NO-UNDO.
DEF INPUT PARAM  iTrans    AS CHAR  NO-UNDO.
DEF INPUT PARAM  iDop      AS CHAR  NO-UNDO.
DEF INPUT PARAM  iopDate   AS CHAR  NO-UNDO.

IF OPSYS = "UNIX" THEN UNIX SILENT VALUE ("{&potokscript}" + " " + STRING(iBegPotok) + " " +
                            STRING(iEndPotok) + " " +
                            STRING(iDelim   ) + " " +
                            STRING(iFilial  ) + " " +
                            QUOTER(STRING(iTrans )) + " " +
                            QUOTER(STRING(iDop   )) + " " +
                            QUOTER(STRING(iopDate))  ).

