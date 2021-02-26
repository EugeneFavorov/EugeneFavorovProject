/* kam */

DEF VAR incx AS INT NO-UNDO.
DEF VAR incy AS INT NO-UNDO.

DEF VAR oldtime AS DATETIME NO-UNDO.
DEF VAR mypause AS INT NO-UNDO.

DEF VAR x AS INT NO-UNDO.
DEF VAR y AS INT NO-UNDO.
DEF VAR xd AS INT NO-UNDO. /* дичь */
DEF VAR yd AS INT NO-UNDO. /* дичь */
DEF VAR xx AS INT NO-UNDO EXTENT 2000.
DEF VAR yy AS INT NO-UNDO EXTENT 2000.
DEF VAR kapindos AS LOGICAL NO-UNDO.
DEF VAR curLength AS INT NO-UNDO INIT 3. /* текущая длина */
DEF VAR scrLength AS INT NO-UNDO INIT 1. /* отрисовано */
DEF VAR curindex AS INT NO-UNDO INIT 1. /* координаты головы */
DEF VAR tmpindex AS INT NO-UNDO.
DEF VAR ii AS INT NO-UNDO INIT 1.
DEF VAR monstr AS INT NO-UNDO INIT 0. /* дичь */

incx = 1.
incy = 0.
mypause = 100. /* мчимся */
kapindos = FALSE.


xx[1] = 41.
yy[1] = 10.
xx[2] = 40.
yy[2] = 10.

RUN clearscreen.

PUT SCREEN COLOR 4 COLUMN xx[1]
       ROW yy[1]
       '#'.

RUN CreateMonstr(OUTPUT monstr,OUTPUT xd,OUTPUT yd).

REPEAT:

oldtime = NOW.

IF scrLength >= curLength THEN DO:
    tmpindex = curindex + curLength.
    IF tmpindex > 2000 THEN tmpindex = tmpindex - 2000.
    PUT SCREEN COLOR 4 COLUMN xx[tmpindex] ROW yy[tmpindex] ' '.
    scrLength = scrLength - 1.
END.        
       
PUT SCREEN COLOR 4 COLUMN xx[curindex] ROW yy[curindex] '#'.
tmpindex = curindex + 1.
IF tmpindex > 2000 THEN tmpindex = tmpindex - 2000.
PUT SCREEN COLOR 4 COLUMN xx[tmpindex] ROW yy[tmpindex] '0'. 
scrLength = scrLength + 1.

 DO WHILE (NOW < (oldtime + mypause)) : 

     READKEY PAUSE 0.
     
       CASE LASTKEY:
          WHEN KEYCODE("ESC") THEN
          RETURN.
         
          WHEN KEYCODE("up") THEN DO:
             IF incy = 1 THEN kapindos = TRUE.  
             incy = -1.
             incx = 0.
          END.

          WHEN KEYCODE("down") THEN DO:
             IF incy = -1 THEN kapindos = TRUE.
             incy = 1.
             incx = 0.
          END.

          WHEN KEYCODE("left") THEN DO:
             IF incx = 1 THEN kapindos = TRUE.
             incx = -1.
             incy = 0.
          END.

          WHEN KEYCODE("right") THEN DO:
             IF incx = -1 THEN kapindos = TRUE.
             incx = 1.
             incy = 0.
          END.
       END CASE.
  END.
      x = xx[curindex] + incx.
      y = yy[curindex] + incy.
      RUN AMAM(x,y,OUTPUT kapindos).
      IF kapindos THEN DO: 
          RUN clearscreen.
          MESSAGE "КАПИНДОС! длина " STRING(curLength) VIEW-AS ALERT-BOX.
          RETURN.
      END.
      ELSE DO:
          curindex = curindex - 1.
          IF curindex <= 0 THEN curindex = curindex + 2000.
          xx[curindex] = x.
          yy[curindex] = y.
          IF xd = x AND yd = y THEN DO:
              curLength = curLength + monstr. 
              RUN CreateMonstr(OUTPUT monstr,OUTPUT xd,OUTPUT yd).
          END.
      END.
END.

PROCEDURE clearscreen.
    DEF VAR i AS INT NO-UNDO INIT 3.
    DO WHILE i < 25:
        PUT SCREEN COLOR 4 COLUMN 1
       ROW i
       '                                                                                '.
        i = i + 1.
    END.
END PROCEDURE.

PROCEDURE CreateMonstr.
DEF OUTPUT PARAMETER monstr AS INT.
DEF OUTPUT PARAMETER xd AS INT.
DEF OUTPUT PARAMETER yd AS INT.
    monstr = RANDOM(1,9).
    xd = RANDOM(1,80).
    yd = RANDOM(3,24).
    PUT SCREEN COLOR 100 COLUMN xd ROW yd string(monstr).
END PROCEDURE.

PROCEDURE AMAM.
DEF INPUT PARAMETER x AS INT NO-UNDO.
DEF INPUT PARAMETER y AS INT NO-UNDO.
DEF OUTPUT PARAMETER kapindos AS LOGICAL.
DEF VAR l AS INT INIT 1.
    IF x < 0 OR x > 80 OR y < 3 OR y > 24 THEN kapindos = TRUE.
    DO WHILE l < scrLength:
        tmpindex = curindex + l.
        IF tmpindex > 2000 THEN tmpindex = tmpindex - 2000.
        IF x = xx[tmpindex] AND y = yy[tmpindex] THEN DO:
            kapindos = TRUE.
            LEAVE.
        END.   
        l = l + 1.
    END.
END PROCEDURE.