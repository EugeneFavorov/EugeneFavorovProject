/*
#  1 - ��砫�� ��⮪
#  2 - ������  ��⮪
#  3 - ������⢮ ��⮪��
#  4 - 䨫���
#  5 - ᯨ᮪ �࠭���権 �१ ;
#  6 - �� ��直� ��砩
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

