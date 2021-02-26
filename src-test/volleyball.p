/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 ЗАО "Банковские информационные системы"
     Filename: importpol.p
      Comment: Проверка кредитов
   Parameters:
         Uses:
      Used by:
      Created: fev
     Modified: 
*/



{globals.i}
{intrface.get refer}
{intrface.get tmess} /* Инструменты обработки сообщений. */
{pick-val.i}
{refer.i}
{tmprecid.def}

DEF TEMP-TABLE Players   NO-UNDO
    FIELD      Name      AS CHAR
    FIELD      Gender    AS DEC
    FIELD      MyReiting AS DEC
    FIELD      Reiting   AS DEC
    FIELD      Nomer     AS DEC
.

DEF TEMP-TABLE Komanda1  NO-UNDO
    FIELD      Name      AS CHAR
    FIELD      Gender    AS DEC
    FIELD      MyReiting AS DEC
    FIELD      Reiting   AS DEC
    FIELD      Nomer     AS DEC
.

DEF TEMP-TABLE Komanda2  NO-UNDO
    FIELD      Name      AS CHAR
    FIELD      Gender    AS DEC
    FIELD      MyReiting AS DEC
    FIELD      Reiting   AS DEC
    FIELD      Nomer     AS DEC
.

DEF VAR fname AS CHAR NO-UNDO.
DEF STREAM ws.

DEF VAR pName10 AS CHAR NO-UNDO INIT "ВАСЯНИН Олег".          DEF VAR pMyReiting10 AS INT NO-UNDO INIT 7.    DEF VAR pReiting10 AS INT NO-UNDO INIT 0.    DEF VAR p2Name10 AS CHAR NO-UNDO.    p2Name10 = pName10.
DEF VAR pName11 AS CHAR NO-UNDO INIT "ГИЛЬДЕРМАН Денис".      DEF VAR pMyReiting11 AS INT NO-UNDO INIT 8.    DEF VAR pReiting11 AS INT NO-UNDO INIT 0.    DEF VAR p2Name11 AS CHAR NO-UNDO.    p2Name11 = pName11.
DEF VAR pName12 AS CHAR NO-UNDO INIT "ДЮСЕНОВ Ануар".         DEF VAR pMyReiting12 AS INT NO-UNDO INIT 7.    DEF VAR pReiting12 AS INT NO-UNDO INIT 0.    DEF VAR p2Name12 AS CHAR NO-UNDO.    p2Name12 = pName12.
DEF VAR pName13 AS CHAR NO-UNDO INIT "СКОРИН Андрей".         DEF VAR pMyReiting13 AS INT NO-UNDO INIT 8.    DEF VAR pReiting13 AS INT NO-UNDO INIT 0.    DEF VAR p2Name13 AS CHAR NO-UNDO.    p2Name13 = pName13.
DEF VAR pName14 AS CHAR NO-UNDO INIT "ЧИЖОВ Олег".            DEF VAR pMyReiting14 AS INT NO-UNDO INIT 4.    DEF VAR pReiting14 AS INT NO-UNDO INIT 0.    DEF VAR p2Name14 AS CHAR NO-UNDO.    p2Name14 = pName14.
DEF VAR pName15 AS CHAR NO-UNDO INIT "ЧЕРМЕНСКИЙ Константин". DEF VAR pMyReiting15 AS INT NO-UNDO INIT 9.    DEF VAR pReiting15 AS INT NO-UNDO INIT 0.    DEF VAR p2Name15 AS CHAR NO-UNDO.    p2Name15 = pName15.
DEF VAR pName16 AS CHAR NO-UNDO INIT "КНЯЗЕВ Дмитрий".        DEF VAR pMyReiting16 AS INT NO-UNDO INIT 6.    DEF VAR pReiting16 AS INT NO-UNDO INIT 0.    DEF VAR p2Name16 AS CHAR NO-UNDO.    p2Name16 = pName16.
DEF VAR pName17 AS CHAR NO-UNDO INIT "МИШОНИН Николай".       DEF VAR pMyReiting17 AS INT NO-UNDO INIT 9.    DEF VAR pReiting17 AS INT NO-UNDO INIT 0.    DEF VAR p2Name17 AS CHAR NO-UNDO.    p2Name17 = pName17.
DEF VAR pName18 AS CHAR NO-UNDO INIT "ЖЕНАТОВ Ермек".         DEF VAR pMyReiting18 AS INT NO-UNDO INIT 8.    DEF VAR pReiting18 AS INT NO-UNDO INIT 0.    DEF VAR p2Name18 AS CHAR NO-UNDO.    p2Name18 = pName18.
DEF VAR pName19 AS CHAR NO-UNDO INIT "СУЛИМА Трофим".         DEF VAR pMyReiting19 AS INT NO-UNDO INIT 9.    DEF VAR pReiting19 AS INT NO-UNDO INIT 0.    DEF VAR p2Name19 AS CHAR NO-UNDO.    p2Name19 = pName19.
DEF VAR pName20 AS CHAR NO-UNDO INIT "ГЛЕБОВ Андрей".         DEF VAR pMyReiting20 AS INT NO-UNDO INIT 9.    DEF VAR pReiting20 AS INT NO-UNDO INIT 0.    DEF VAR p2Name20 AS CHAR NO-UNDO.    p2Name20 = pName20.
DEF VAR pName21 AS CHAR NO-UNDO INIT "ФАВОРОВ Евгений".       DEF VAR pMyReiting21 AS INT NO-UNDO INIT 9.    DEF VAR pReiting21 AS INT NO-UNDO INIT 0.    DEF VAR p2Name21 AS CHAR NO-UNDO.    p2Name21 = pName21.

DEF VAR dName22 AS CHAR NO-UNDO INIT "ПОСКОТИНА Анна".        DEF VAR dMyReiting22 AS INT NO-UNDO INIT 5.    DEF VAR dReiting22 AS INT NO-UNDO INIT 0.    DEF VAR d2Name22 AS CHAR NO-UNDO.    d2Name22 = dName22.
DEF VAR dName23 AS CHAR NO-UNDO INIT "МИШОНИНА Олеся".        DEF VAR dMyReiting23 AS INT NO-UNDO INIT 5.    DEF VAR dReiting23 AS INT NO-UNDO INIT 0.    DEF VAR d2Name23 AS CHAR NO-UNDO.    d2Name23 = dName23.
DEF VAR dName24 AS CHAR NO-UNDO INIT "Людмила".               DEF VAR dMyReiting24 AS INT NO-UNDO INIT 5.    DEF VAR dReiting24 AS INT NO-UNDO INIT 0.    DEF VAR d2Name24 AS CHAR NO-UNDO.    d2Name24 = dName24.
DEF VAR dName25 AS CHAR NO-UNDO INIT "Екатерина".             DEF VAR dMyReiting25 AS INT NO-UNDO INIT 5.    DEF VAR dReiting25 AS INT NO-UNDO INIT 0.    DEF VAR d2Name25 AS CHAR NO-UNDO.    d2Name25 = dName25.
DEF VAR dName26 AS CHAR NO-UNDO INIT "АРТЕМЬЕВА Кристина".    DEF VAR dMyReiting26 AS INT NO-UNDO INIT 3.    DEF VAR dReiting26 AS INT NO-UNDO INIT 0.    DEF VAR d2Name26 AS CHAR NO-UNDO.    d2Name26 = dName26.
DEF VAR dName27 AS CHAR NO-UNDO INIT "КИСЛОВА Елена".         DEF VAR dMyReiting27 AS INT NO-UNDO INIT 5.    DEF VAR dReiting27 AS INT NO-UNDO INIT 0.    DEF VAR d2Name27 AS CHAR NO-UNDO.    d2Name27 = dName27.
DEF VAR dName28 AS CHAR NO-UNDO INIT "СЕДАШ Ольга".           DEF VAR dMyReiting28 AS INT NO-UNDO INIT 6.    DEF VAR dReiting28 AS INT NO-UNDO INIT 0.    DEF VAR d2Name28 AS CHAR NO-UNDO.    d2Name28 = dName28.
DEF VAR dName29 AS CHAR NO-UNDO INIT "ЧЕРМЕНСКАЯ Анастасия".  DEF VAR dMyReiting29 AS INT NO-UNDO INIT 3.    DEF VAR dReiting29 AS INT NO-UNDO INIT 0.    DEF VAR d2Name29 AS CHAR NO-UNDO.    d2Name29 = dName29.
DEF VAR dName30 AS CHAR NO-UNDO INIT "РУСИНОВА Дарья".        DEF VAR dMyReiting30 AS INT NO-UNDO INIT 6.    DEF VAR dReiting30 AS INT NO-UNDO INIT 0.    DEF VAR d2Name30 AS CHAR NO-UNDO.    d2Name30 = dName30.
DEF VAR dName31 AS CHAR NO-UNDO INIT "КРИВКО Оксана".         DEF VAR dMyReiting31 AS INT NO-UNDO INIT 5.    DEF VAR dReiting31 AS INT NO-UNDO INIT 0.    DEF VAR d2Name31 AS CHAR NO-UNDO.    d2Name31 = dName31.
DEF VAR dName32 AS CHAR NO-UNDO INIT "Галина".                DEF VAR dMyReiting32 AS INT NO-UNDO INIT 5.    DEF VAR dReiting32 AS INT NO-UNDO INIT 0.    DEF VAR d2Name32 AS CHAR NO-UNDO.    d2Name32 = dName32.
DEF VAR dName33 AS CHAR NO-UNDO INIT "".                      DEF VAR dMyReiting33 AS INT NO-UNDO INIT 0.    DEF VAR dReiting33 AS INT NO-UNDO INIT 0.    DEF VAR d2Name33 AS CHAR NO-UNDO.    d2Name33 = dName33.

DEF VAR pi   AS INT NO-UNDO.
DEF VAR di   AS INT NO-UNDO.
DEF VAR k1i  AS INT NO-UNDO.
DEF VAR k2i  AS INT NO-UNDO.
DEF VAR g1i  AS INT NO-UNDO.
DEF VAR g2i  AS INT NO-UNDO.
DEF VAR k1r  AS INT NO-UNDO.
DEF VAR k2r  AS INT NO-UNDO.
DEF VAR k1mr AS INT NO-UNDO.
DEF VAR k2mr AS INT NO-UNDO.

DEF VAR i   AS INT NO-UNDO.
DEF VAR i2  AS INT NO-UNDO.
DEF VAR j   AS INT NO-UNDO INIT 1.
DEF VAR k   AS INT NO-UNDO.
DEF VAR l   AS INT NO-UNDO.

DEF VAR pickvalue AS CHAR NO-UNDO INIT "NO".




fname = "/home2/bis/quit41d/src-test/volleyball.txt".        
OUTPUT STREAM ws TO VALUE (fname) APPEND UNBUFFERED CONVERT TARGET "1251" SOURCE "IBM866".

/* форма установки данных */

DEFINE FRAME fGet  
	"                                                                              " SKIP
	"                                                                              " SKIP
	"                       ПАРНИ | КЭФ                          ДЕВУШКИ | КЭФ     " SKIP
	"    -------------------------------        -------------------------------    " SKIP
	"     1" pName10 NO-LABEL FORMAT "x(21)" "|"pReiting10 NO-LABEL FORMAT "9" "           1" dName22 NO-LABEL FORMAT "x(21)" "|"dReiting22 NO-LABEL FORMAT "9" SKIP
	"     2" pName11 NO-LABEL FORMAT "x(21)" "|"pReiting11 NO-LABEL FORMAT "9" "           2" dName23 NO-LABEL FORMAT "x(21)" "|"dReiting23 NO-LABEL FORMAT "9" SKIP
	"     3" pName12 NO-LABEL FORMAT "x(21)" "|"pReiting12 NO-LABEL FORMAT "9" "           3" dName24 NO-LABEL FORMAT "x(21)" "|"dReiting24 NO-LABEL FORMAT "9" SKIP
	"     4" pName13 NO-LABEL FORMAT "x(21)" "|"pReiting13 NO-LABEL FORMAT "9" "           4" dName25 NO-LABEL FORMAT "x(21)" "|"dReiting25 NO-LABEL FORMAT "9" SKIP
	"     5" pName14 NO-LABEL FORMAT "x(21)" "|"pReiting14 NO-LABEL FORMAT "9" "           5" dName26 NO-LABEL FORMAT "x(21)" "|"dReiting26 NO-LABEL FORMAT "9" SKIP
	"     6" pName15 NO-LABEL FORMAT "x(21)" "|"pReiting15 NO-LABEL FORMAT "9" "           6" dName27 NO-LABEL FORMAT "x(21)" "|"dReiting27 NO-LABEL FORMAT "9" SKIP
	"     7" pName16 NO-LABEL FORMAT "x(21)" "|"pReiting16 NO-LABEL FORMAT "9" "           7" dName28 NO-LABEL FORMAT "x(21)" "|"dReiting28 NO-LABEL FORMAT "9" SKIP
	"     8" pName17 NO-LABEL FORMAT "x(21)" "|"pReiting17 NO-LABEL FORMAT "9" "           8" dName29 NO-LABEL FORMAT "x(21)" "|"dReiting29 NO-LABEL FORMAT "9" SKIP
	"     9" pName18 NO-LABEL FORMAT "x(21)" "|"pReiting18 NO-LABEL FORMAT "9" "           9" dName30 NO-LABEL FORMAT "x(21)" "|"dReiting30 NO-LABEL FORMAT "9" SKIP
	"    10" pName19 NO-LABEL FORMAT "x(21)" "|"pReiting19 NO-LABEL FORMAT "9" "          10" dName31 NO-LABEL FORMAT "x(21)" "|"dReiting31 NO-LABEL FORMAT "9" SKIP
	"    11" pName20 NO-LABEL FORMAT "x(21)" "|"pReiting20 NO-LABEL FORMAT "9" "          11" dName32 NO-LABEL FORMAT "x(21)" "|"dReiting32 NO-LABEL FORMAT "9" SKIP
	"    12" pName21 NO-LABEL FORMAT "x(21)" "|"pReiting21 NO-LABEL FORMAT "9" "          12" dName33 NO-LABEL FORMAT "x(21)" "|"dReiting33 NO-LABEL FORMAT "9" SKIP
	"                                                                              " SKIP
	"                                                                              " SKIP
	"                                                                              " SKIP
	"                                                         F8 - Удалить Игрока  " SKIP
	"                                                 CTRL+ENTER - Запуск Лохотрона" SKIP
	WITH WIDTH 80 CENTERED ROW 1
        TITLE COLOR BRIGHT-WHITE "[ Введите игроков ]".



RUN ClearScreen.

i = 0. DO WHILE i < 3000000: i = i + 1. END.
PUT SCREEN COLOR 4 COLUMN  4 ROW  5 'ВВВ  '.
PUT SCREEN COLOR 4 COLUMN  4 ROW  6 'В  В '.
PUT SCREEN COLOR 4 COLUMN  4 ROW  7 'ВВВВ  '.
PUT SCREEN COLOR 4 COLUMN  4 ROW  8 'В   В '.
PUT SCREEN COLOR 4 COLUMN  4 ROW  9 'ВВВВ  '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 10 ROW  5 ' ООО '.
PUT SCREEN COLOR 4 COLUMN 10 ROW  6 'О   О'.
PUT SCREEN COLOR 4 COLUMN 10 ROW  7 'О   О'.
PUT SCREEN COLOR 4 COLUMN 10 ROW  8 'О   О'.
PUT SCREEN COLOR 4 COLUMN 10 ROW  9 ' ООО '.
i = 0. DO WHILE i < 750000: i = i + 1. END.
                            
PUT SCREEN COLOR 4 COLUMN 16 ROW  5 '    Л'.
PUT SCREEN COLOR 4 COLUMN 16 ROW  6 '   ЛЛ'.
PUT SCREEN COLOR 4 COLUMN 16 ROW  7 '  Л Л'.
PUT SCREEN COLOR 4 COLUMN 16 ROW  8 ' Л  Л'.
PUT SCREEN COLOR 4 COLUMN 16 ROW  9 'Л   Л'.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 22 ROW  5 'ЕЕЕЕ '.
PUT SCREEN COLOR 4 COLUMN 22 ROW  6 'Е    '.
PUT SCREEN COLOR 4 COLUMN 22 ROW  7 'ЕЕЕЕ '.
PUT SCREEN COLOR 4 COLUMN 22 ROW  8 'Е    '.
PUT SCREEN COLOR 4 COLUMN 22 ROW  9 'ЕЕЕЕЕ'. 
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 28 ROW  5 'Й   Й'.
PUT SCREEN COLOR 4 COLUMN 28 ROW  6 'Й  ЙЙ'.
PUT SCREEN COLOR 4 COLUMN 28 ROW  7 'Й Й Й'.
PUT SCREEN COLOR 4 COLUMN 28 ROW  8 'ЙЙ  Й'.
PUT SCREEN COLOR 4 COLUMN 28 ROW  9 'Й   Й'.
i = 0. DO WHILE i < 750000: i = i + 1. END.
                                          
PUT SCREEN COLOR 4 COLUMN 34 ROW  5 'ББББ '.
PUT SCREEN COLOR 4 COLUMN 34 ROW  6 'Б    '.
PUT SCREEN COLOR 4 COLUMN 34 ROW  7 'ББББ '.
PUT SCREEN COLOR 4 COLUMN 34 ROW  8 'Б   Б'.
PUT SCREEN COLOR 4 COLUMN 34 ROW  9 'ББББ '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 40 ROW  5 ' ООО '.
PUT SCREEN COLOR 4 COLUMN 40 ROW  6 'О   О'.
PUT SCREEN COLOR 4 COLUMN 40 ROW  7 'О   О'.
PUT SCREEN COLOR 4 COLUMN 40 ROW  8 'О   О'.
PUT SCREEN COLOR 4 COLUMN 40 ROW  9 ' ООО '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 46 ROW  5 '    Л'.
PUT SCREEN COLOR 4 COLUMN 46 ROW  6 '   ЛЛ'.
PUT SCREEN COLOR 4 COLUMN 46 ROW  7 '  Л Л'.
PUT SCREEN COLOR 4 COLUMN 46 ROW  8 ' Л  Л'.
PUT SCREEN COLOR 4 COLUMN 46 ROW  9 'Л   Л'.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 52 ROW  5 'Ь    '.
PUT SCREEN COLOR 4 COLUMN 52 ROW  6 'Ь    '.
PUT SCREEN COLOR 4 COLUMN 52 ROW  7 'ЬЬЬЬ '.
PUT SCREEN COLOR 4 COLUMN 52 ROW  8 'Ь   Ь'.
PUT SCREEN COLOR 4 COLUMN 52 ROW  9 'ЬЬЬЬ '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 58 ROW  5 'Н   Н'.
PUT SCREEN COLOR 4 COLUMN 58 ROW  6 'Н   Н'.
PUT SCREEN COLOR 4 COLUMN 58 ROW  7 'ННННН'.
PUT SCREEN COLOR 4 COLUMN 58 ROW  8 'Н   Н'.
PUT SCREEN COLOR 4 COLUMN 58 ROW  9 'Н   Н'.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 64 ROW  5 'Ы     Ы'.
PUT SCREEN COLOR 4 COLUMN 64 ROW  6 'Ы     Ы'.
PUT SCREEN COLOR 4 COLUMN 64 ROW  7 'ЫЫЫЫ  Ы'.
PUT SCREEN COLOR 4 COLUMN 64 ROW  8 'Ы  Ы  Ы'.
PUT SCREEN COLOR 4 COLUMN 64 ROW  9 'ЫЫЫЫ  Ы'.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 72 ROW  5 'Й   Й'.
PUT SCREEN COLOR 4 COLUMN 72 ROW  6 'Й  ЙЙ'.
PUT SCREEN COLOR 4 COLUMN 72 ROW  7 'Й Й Й'.
PUT SCREEN COLOR 4 COLUMN 72 ROW  8 'ЙЙ  Й'.
PUT SCREEN COLOR 4 COLUMN 72 ROW  9 'Й   Й'.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 30 ROW  4 'Й'. 
i = 0. DO WHILE i < 500000: i = i + 1. END.
PUT SCREEN COLOR 4 COLUMN 31 ROW  4 'Й'.
i = 0. DO WHILE i < 750000: i = i + 1. END.
PUT SCREEN COLOR 4 COLUMN 74 ROW  4 'Й'. 
i = 0. DO WHILE i < 500000: i = i + 1. END.
PUT SCREEN COLOR 4 COLUMN 75 ROW  4 'Й'.
i = 0. DO WHILE i < 2000000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 16 ROW 12 '    Л'.
PUT SCREEN COLOR 4 COLUMN 16 ROW 13 '   ЛЛ'.
PUT SCREEN COLOR 4 COLUMN 16 ROW 14 '  Л Л'.
PUT SCREEN COLOR 4 COLUMN 16 ROW 15 ' Л  Л'.
PUT SCREEN COLOR 4 COLUMN 16 ROW 16 'Л   Л'.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 22 ROW 12 ' ООО '.
PUT SCREEN COLOR 4 COLUMN 22 ROW 13 'О   О'.
PUT SCREEN COLOR 4 COLUMN 22 ROW 14 'О   О'.
PUT SCREEN COLOR 4 COLUMN 22 ROW 15 'О   О'.
PUT SCREEN COLOR 4 COLUMN 22 ROW 16 ' ООО '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 28 ROW 12 'Х   Х'.
PUT SCREEN COLOR 4 COLUMN 28 ROW 13 ' Х Х '.
PUT SCREEN COLOR 4 COLUMN 28 ROW 14 '  Х  '.
PUT SCREEN COLOR 4 COLUMN 28 ROW 15 ' Х Х '.
PUT SCREEN COLOR 4 COLUMN 28 ROW 16 'Х   Х'.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 34 ROW 12 ' ООО '.
PUT SCREEN COLOR 4 COLUMN 34 ROW 13 'О   О'.
PUT SCREEN COLOR 4 COLUMN 34 ROW 14 'О   О'.
PUT SCREEN COLOR 4 COLUMN 34 ROW 15 'О   О'.
PUT SCREEN COLOR 4 COLUMN 34 ROW 16 ' ООО '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 40 ROW 12 'ТТТТТ'.
PUT SCREEN COLOR 4 COLUMN 40 ROW 13 '  Т  '.
PUT SCREEN COLOR 4 COLUMN 40 ROW 14 '  Т  '.
PUT SCREEN COLOR 4 COLUMN 40 ROW 15 '  Т  '.
PUT SCREEN COLOR 4 COLUMN 40 ROW 16 '  Т  '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 46 ROW 12 'РРРР '.
PUT SCREEN COLOR 4 COLUMN 46 ROW 13 'Р   Р'.
PUT SCREEN COLOR 4 COLUMN 46 ROW 14 'РРРР '.
PUT SCREEN COLOR 4 COLUMN 46 ROW 15 'Р    '.
PUT SCREEN COLOR 4 COLUMN 46 ROW 16 'Р    '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 52 ROW 12 ' ООО '.
PUT SCREEN COLOR 4 COLUMN 52 ROW 13 'О   О'.
PUT SCREEN COLOR 4 COLUMN 52 ROW 14 'О   О'.
PUT SCREEN COLOR 4 COLUMN 52 ROW 15 'О   О'.
PUT SCREEN COLOR 4 COLUMN 52 ROW 16 ' ООО '.
i = 0. DO WHILE i < 750000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 58 ROW 12 'Н   Н'.
PUT SCREEN COLOR 4 COLUMN 58 ROW 13 'Н   Н'.
PUT SCREEN COLOR 4 COLUMN 58 ROW 14 'ННННН'.
PUT SCREEN COLOR 4 COLUMN 58 ROW 15 'Н   Н'.
PUT SCREEN COLOR 4 COLUMN 58 ROW 16 'Н   Н'.
i = 0. DO WHILE i < 4000000: i = i + 1. END.

DO WHILE j < 8000:
   k = random(1,24).
   l = random(1,80).
   PUT SCREEN COLOR 4 COLUMN l ROW k ' '.
   j = j + 1.
   i = 0. DO WHILE i < 700: i = i + 1. END.
END.

RUN ClearScreen.

i = 0. DO WHILE i < 3000000: i = i + 1. END.

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  PAUSE 0.
		UPDATE
			pName10
			pName11
			pName12
			pName13
			pName14
			pName15
			pName16
			pName17
			pName18
			pName19
			pName20
			pName21
			pReiting10
			pReiting11
			pReiting12
			pReiting13
			pReiting14
			pReiting15
			pReiting16
			pReiting17
			pReiting18
			pReiting19
			pReiting20
			pReiting21
			dName22
			dName23
			dName24
			dName25
			dName26
			dName27
			dName28
			dName29
			dName30
			dName31
			dName32
			dName33
			dReiting22
			dReiting23
			dReiting24
			dReiting25
			dReiting26
			dReiting27
			dReiting28
			dReiting29
			dReiting30
			dReiting31
			dReiting32
			dReiting33
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
END.



DO WHILE pickvalue EQ "NO":
   pi   = 0.
   di   = 0.
   k1i  = 0.
   k2i  = 0.
   g1i  = 0.
   g2i  = 0.
   k1r  = 0.
   k2r  = 0.
   k1mr = 0.
   k2mr = 0.
   Empty TEMP-TABLE Players.
   Empty TEMP-TABLE Komanda1.
   Empty TEMP-TABLE Komanda2.

   IF (length(dName22) > 0) AND (dName22 EQ d2Name22)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName22     Players.Gender = 0     Players.MyReiting = dMyReiting22     Players.Reiting = dReiting22     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName22) > 0) AND (dName22 NE d2Name22) THEN DO: CREATE Players.     ASSIGN Players.Name = dName22     Players.Gender = 0     Players.MyReiting = dReiting22       Players.Reiting = dReiting22     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName23) > 0) AND (dName23 EQ d2Name23)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName23     Players.Gender = 0     Players.MyReiting = dMyReiting23     Players.Reiting = dReiting23     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName23) > 0) AND (dName23 NE d2Name23) THEN DO: CREATE Players.     ASSIGN Players.Name = dName23     Players.Gender = 0     Players.MyReiting = dReiting23       Players.Reiting = dReiting23     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName24) > 0) AND (dName24 EQ d2Name24)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName24     Players.Gender = 0     Players.MyReiting = dMyReiting24     Players.Reiting = dReiting24     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName24) > 0) AND (dName24 NE d2Name24) THEN DO: CREATE Players.     ASSIGN Players.Name = dName24     Players.Gender = 0     Players.MyReiting = dReiting24       Players.Reiting = dReiting24     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName25) > 0) AND (dName25 EQ d2Name25)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName25     Players.Gender = 0     Players.MyReiting = dMyReiting25     Players.Reiting = dReiting25     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName25) > 0) AND (dName25 NE d2Name25) THEN DO: CREATE Players.     ASSIGN Players.Name = dName25     Players.Gender = 0     Players.MyReiting = dReiting25       Players.Reiting = dReiting25     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName26) > 0) AND (dName26 EQ d2Name26)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName26     Players.Gender = 0     Players.MyReiting = dMyReiting26     Players.Reiting = dReiting26     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName26) > 0) AND (dName26 NE d2Name26) THEN DO: CREATE Players.     ASSIGN Players.Name = dName26     Players.Gender = 0     Players.MyReiting = dReiting26       Players.Reiting = dReiting26     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName27) > 0) AND (dName27 EQ d2Name27)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName27     Players.Gender = 0     Players.MyReiting = dMyReiting27     Players.Reiting = dReiting27     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName27) > 0) AND (dName27 NE d2Name27) THEN DO: CREATE Players.     ASSIGN Players.Name = dName27     Players.Gender = 0     Players.MyReiting = dReiting27       Players.Reiting = dReiting27     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName28) > 0) AND (dName28 EQ d2Name28)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName28     Players.Gender = 0     Players.MyReiting = dMyReiting28     Players.Reiting = dReiting28     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName28) > 0) AND (dName28 NE d2Name28) THEN DO: CREATE Players.     ASSIGN Players.Name = dName28     Players.Gender = 0     Players.MyReiting = dReiting28       Players.Reiting = dReiting28     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName29) > 0) AND (dName29 EQ d2Name29)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName29     Players.Gender = 0     Players.MyReiting = dMyReiting29     Players.Reiting = dReiting29     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName29) > 0) AND (dName29 NE d2Name29) THEN DO: CREATE Players.     ASSIGN Players.Name = dName29     Players.Gender = 0     Players.MyReiting = dReiting29       Players.Reiting = dReiting29     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName30) > 0) AND (dName30 EQ d2Name30)                                                                                                                                 
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName30     Players.Gender = 0     Players.MyReiting = dMyReiting30     Players.Reiting = dReiting30     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName30) > 0) AND (dName30 NE d2Name30) THEN DO: CREATE Players.     ASSIGN Players.Name = dName30     Players.Gender = 0     Players.MyReiting = dReiting30       Players.Reiting = dReiting30     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName31) > 0) AND (dName31 EQ d2Name31)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName31     Players.Gender = 0     Players.MyReiting = dMyReiting31     Players.Reiting = dReiting31     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName31) > 0) AND (dName31 NE d2Name31) THEN DO: CREATE Players.     ASSIGN Players.Name = dName31     Players.Gender = 0     Players.MyReiting = dReiting31       Players.Reiting = dReiting31     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName32) > 0) AND (dName32 EQ d2Name32)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName32     Players.Gender = 0     Players.MyReiting = dMyReiting32     Players.Reiting = dReiting32     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName32) > 0) AND (dName32 NE d2Name32) THEN DO: CREATE Players.     ASSIGN Players.Name = dName32     Players.Gender = 0     Players.MyReiting = dReiting32       Players.Reiting = dReiting32     Players.Nomer = random(1,2).     di = di + 1. END.
   IF (length(dName33) > 0) AND (dName33 EQ d2Name33)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = dName33     Players.Gender = 0     Players.MyReiting = dMyReiting33     Players.Reiting = dReiting33     Players.Nomer = random(1,2).     di = di + 1. END.
      ELSE IF (length(dName33) > 0) AND (dName33 NE d2Name33) THEN DO: CREATE Players.     ASSIGN Players.Name = dName33     Players.Gender = 0     Players.MyReiting = dReiting33       Players.Reiting = dReiting33     Players.Nomer = random(1,2).     di = di + 1. END.



   IF (length(pName10) > 0) AND (pName10 EQ p2Name10)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName10     Players.Gender = 1     Players.MyReiting = pMyReiting10     Players.Reiting = pReiting10     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName10) > 0) AND (pName10 NE p2Name10) THEN DO: CREATE Players.     ASSIGN Players.Name = pName10     Players.Gender = 1     Players.MyReiting = pReiting10       Players.Reiting = pReiting10     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName11) > 0) AND (pName11 EQ p2Name11)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName11     Players.Gender = 1     Players.MyReiting = pMyReiting11     Players.Reiting = pReiting11     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName11) > 0) AND (pName11 NE p2Name11) THEN DO: CREATE Players.     ASSIGN Players.Name = pName11     Players.Gender = 1     Players.MyReiting = pReiting11       Players.Reiting = pReiting11     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName12) > 0) AND (pName12 EQ p2Name12)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName12     Players.Gender = 1     Players.MyReiting = pMyReiting12     Players.Reiting = pReiting12     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName12) > 0) AND (pName12 NE p2Name12) THEN DO: CREATE Players.     ASSIGN Players.Name = pName12     Players.Gender = 1     Players.MyReiting = pReiting12       Players.Reiting = pReiting12     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName13) > 0) AND (pName13 EQ p2Name13)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName13     Players.Gender = 1     Players.MyReiting = pMyReiting13     Players.Reiting = pReiting13     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName13) > 0) AND (pName13 NE p2Name13) THEN DO: CREATE Players.     ASSIGN Players.Name = pName13     Players.Gender = 1     Players.MyReiting = pReiting13       Players.Reiting = pReiting13     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName14) > 0) AND (pName14 EQ p2Name14)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName14     Players.Gender = 1     Players.MyReiting = pMyReiting14     Players.Reiting = pReiting14     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName14) > 0) AND (pName14 NE p2Name14) THEN DO: CREATE Players.     ASSIGN Players.Name = pName14     Players.Gender = 1     Players.MyReiting = pReiting14       Players.Reiting = pReiting14     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName15) > 0) AND (pName15 EQ p2Name15)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName15     Players.Gender = 1     Players.MyReiting = pMyReiting15     Players.Reiting = pReiting15     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName15) > 0) AND (pName15 NE p2Name15) THEN DO: CREATE Players.     ASSIGN Players.Name = pName15     Players.Gender = 1     Players.MyReiting = pReiting15       Players.Reiting = pReiting15     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName16) > 0) AND (pName16 EQ p2Name16)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName16     Players.Gender = 1     Players.MyReiting = pMyReiting16     Players.Reiting = pReiting16     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName16) > 0) AND (pName16 NE p2Name16) THEN DO: CREATE Players.     ASSIGN Players.Name = pName16     Players.Gender = 1     Players.MyReiting = pReiting16       Players.Reiting = pReiting16     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName17) > 0) AND (pName17 EQ p2Name17)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName17     Players.Gender = 1     Players.MyReiting = pMyReiting17     Players.Reiting = pReiting17     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName17) > 0) AND (pName17 NE p2Name17) THEN DO: CREATE Players.     ASSIGN Players.Name = pName17     Players.Gender = 1     Players.MyReiting = pReiting17       Players.Reiting = pReiting17     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName18) > 0) AND (pName18 EQ p2Name18)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName18     Players.Gender = 1     Players.MyReiting = pMyReiting18     Players.Reiting = pReiting18     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName18) > 0) AND (pName18 NE p2Name18) THEN DO: CREATE Players.     ASSIGN Players.Name = pName18     Players.Gender = 1     Players.MyReiting = pReiting18       Players.Reiting = pReiting18     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName19) > 0) AND (pName19 EQ p2Name19)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName19     Players.Gender = 1     Players.MyReiting = pMyReiting19     Players.Reiting = pReiting19     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName19) > 0) AND (pName19 NE p2Name19) THEN DO: CREATE Players.     ASSIGN Players.Name = pName19     Players.Gender = 1     Players.MyReiting = pReiting19       Players.Reiting = pReiting19     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName20) > 0) AND (pName20 EQ p2Name20)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName20     Players.Gender = 1     Players.MyReiting = pMyReiting20     Players.Reiting = pReiting20     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName20) > 0) AND (pName20 NE p2Name20) THEN DO: CREATE Players.     ASSIGN Players.Name = pName20     Players.Gender = 1     Players.MyReiting = pReiting20       Players.Reiting = pReiting20     Players.Nomer = random(1,2).     pi = pi + 1. END.
   IF (length(pName21) > 0) AND (pName21 EQ p2Name21)
      THEN DO:                                                         CREATE Players.     ASSIGN Players.Name = pName21     Players.Gender = 1     Players.MyReiting = pMyReiting21     Players.Reiting = pReiting21     Players.Nomer = random(1,2).     pi = pi + 1. END.
      ELSE IF (length(pName21) > 0) AND (pName21 NE p2Name21) THEN DO: CREATE Players.     ASSIGN Players.Name = pName21     Players.Gender = 1     Players.MyReiting = pReiting21       Players.Reiting = pReiting21     Players.Nomer = random(1,2).     pi = pi + 1. END.



   FOR EACH Players WHERE Players.Nomer > 0 NO-LOCK:
       IF Players.Nomer = 1 THEN DO: CREATE Komanda1.     ASSIGN Komanda1.Name = Players.Name     Komanda1.Gender = Players.Gender     Komanda1.MyReiting = Players.MyReiting     Komanda1.Reiting = Players.Reiting     Komanda1.Nomer = Players.Nomer     k1i = k1i + 1.     k1mr = k1mr + Players.MyReiting.     k1r = k1r + Players.Reiting.     IF Players.Gender = 0 THEN g1i = g1i + 1. END.
       IF Players.Nomer = 2 THEN DO: CREATE Komanda2.     ASSIGN Komanda2.Name = Players.Name     Komanda2.Gender = Players.Gender     Komanda2.MyReiting = Players.MyReiting     Komanda2.Reiting = Players.Reiting     Komanda2.Nomer = Players.Nomer     k2i = k2i + 1.     k2mr = k2mr + Players.MyReiting.     k2r = k2r + Players.Reiting.     IF Players.Gender = 0 THEN g2i = g2i + 1. END.
   END.

   k1r = (k1mr + k1r) / 2.
   k2r = (k2mr + k2r) / 2.

   IF ((k1i EQ k2i) OR (k1i = k2i + 1) OR (k2i = k1i + 1)) AND   /* количество игроков */
      ((g1i EQ g2i) OR (g1i = g2i + 1) OR (g2i = g1i + 1)) AND   /* количество девушек */
      ((k1r EQ k2r) OR (((k1r - k2r) <= 1) AND ((k1r - k2r) > 0)) OR (((k2r - k1r) <= 1) AND ((k2r - k1r) > 0)))   /* коэффициенты игроков */
      THEN pickvalue = "YES".
      ELSE pickvalue = "NO".

END.



IF pi + di <  6 THEN DO: message "Слишком Мало Игроков! Шевелите Народ!"  view-as alert-box. LEAVE. END.
IF pi + di > 12 THEN DO: message "Слишком Много Игроков! Выгоните Кого-Нибудь!" view-as alert-box. LEAVE. END.



{setdest.i &col=10}

i = 0. DO WHILE i < 2000000: i = i + 1. END.
PUT SCREEN COLOR 4 COLUMN 30 ROW  3 'Результаты Лохотрона'.
PUT SCREEN COLOR 4 COLUMN 29 ROW  4 '----------------------'.
PUT STREAM ws UNFORMATTED string(today, "99/99/9999") + " " string(time, "hh:mm:ss") + " " + string(USERID('bisquit')) chr(13) + chr(10).
i = 0. DO WHILE i < 3000000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 21 ROW  7 'Команда 1'.
PUT SCREEN COLOR 4 COLUMN 21 ROW  8 '---------'.
PUT STREAM ws UNFORMATTED "Команда 1: ".
i = 0. DO WHILE i < 3000000: i = i + 1. END.

i2 = 0.
FOR EACH Komanda1 WHERE Komanda1.Nomer > 0 NO-LOCK:
    PUT SCREEN COLOR 4 COLUMN 30 - LENGTH(Komanda1.Name) ROW 9 + i2 Komanda1.Name.
    PUT STREAM ws UNFORMATTED string(Komanda1.Name) + "(" + string(Komanda1.MyReiting) + "/" + string(Komanda1.Reiting) + "), ".
    i2 = i2 + 1.
    i = 0. DO WHILE i < 1000000: i = i + 1. END.
END.
PUT STREAM ws UNFORMATTED chr(13) + chr(10).
i = 0. DO WHILE i < 2000000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 34 ROW  9 'V   V  SSS   '.
PUT SCREEN COLOR 4 COLUMN 34 ROW 10 ' V  V S      '.
PUT SCREEN COLOR 4 COLUMN 34 ROW 11 '  V V SSSSS  '.
PUT SCREEN COLOR 4 COLUMN 34 ROW 12 '   VV     S  '.
PUT SCREEN COLOR 4 COLUMN 34 ROW 13 '    V SSSS  '.
i = 0. DO WHILE i < 3000000: i = i + 1. END.

PUT SCREEN COLOR 4 COLUMN 50 ROW  7 'Команда 2'.
PUT SCREEN COLOR 4 COLUMN 50 ROW  8 '---------'.
PUT STREAM ws UNFORMATTED "Команда 2: ".
i = 0. DO WHILE i < 3000000: i = i + 1. END.

i2 = 0.
FOR EACH Komanda2 WHERE Komanda2.Nomer > 0 NO-LOCK:
    PUT SCREEN COLOR 4 COLUMN 50 ROW 9 + i2 Komanda2.Name.
    PUT STREAM ws UNFORMATTED string(Komanda2.Name) + "(" + string(Komanda2.MyReiting) + "/" + string(Komanda2.Reiting) + "), ".
    i2 = i2 + 1.
    i = 0. DO WHILE i < 1000000: i = i + 1. END.
END.
PUT STREAM ws UNFORMATTED chr(13) + chr(10).
PUT STREAM ws UNFORMATTED "----------------------------" + chr(13) + chr(10).
i = 0. DO WHILE i < 1000000: i = i + 1. END.

OUTPUT STREAM ws CLOSE.
{preview.i &col=170}

{intrface.del} /* Выгрузка инструментария. */



PROCEDURE ClearScreen.
    DEF VAR i AS INT NO-UNDO INIT 1.
    DO WHILE i < 25:
       PUT SCREEN COLOR 4 COLUMN 1 ROW i '                                                                                '.
       i = i + 1.
    END.
END PROCEDURE.
