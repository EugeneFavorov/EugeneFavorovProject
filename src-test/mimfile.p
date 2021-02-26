{globals.i}

{exchange.equ}

{intrface.get xclass}
{intrface.get tmess}
{intrface.get strng}
{intrface.get import}
{intrface.get filex}

{tmailusr.def}
{mimfile.i}

DEF NEW SHARED VAR debug AS INT64 NO-UNDO.

DEF STREAM journal.
DEF STREAM FileList.
DEF VAR buf             AS CHAR           NO-UNDO.
DEF VAR FileName        AS CHAR           NO-UNDO.
DEF VAR slash           AS CHAR INIT "/"  NO-UNDO.
DEF VAR i               AS INT64            NO-UNDO.

DEF VAR mFullPath       AS CHAR           NO-UNDO.
DEF VAR mNoSortC        AS CHAR           NO-UNDO.
DEF VAR mNoSortD        AS DATE           NO-UNDO.
DEF VAR mNoSortI        AS INT64            NO-UNDO.
DEF VAR mOk             AS LOG            NO-UNDO. /* проверяет время работы с абонентом */

DEF VAR pos             AS INT64  NO-UNDO.
DEF VAR ext             AS CHAR NO-UNDO.


DEFINE NEW SHARED STREAM debug-stream.

DEF TEMP-TABLE tt NO-UNDO
    FIELD file-name        AS CHAR
    FIELD ShortName        AS CHAR
    FIELD file-create-date AS DATE
    FIELD file-create-time AS INT64.

RUN InitComVar.


ASSIGN
   auto       = YES.

each-tt-dir:
FOR EACH tt-dir NO-LOCK:
/*------------------------------- Чтение списка файлов по маске из каталога --*/
   {empty tt}
   RUN read-dir(tt-dir.dir).
/*----------------------------------------------- Обработка найденых файлов --*/
   FOR EACH tt 
            NO-LOCK,
      FIRST tt-mail-user WHERE
            tt-mail-user.dir EQ tt-dir.dir AND
            CAN-DO(tt-mail-user.mask-file,tt.ShortName) 
            NO-LOCK
         BY (IF tt-dir.SortByName THEN tt.file-name        ELSE mNoSortC)
         BY (IF tt-dir.SortByDate THEN tt.file-create-date ELSE mNoSortD)
         BY (IF tt-dir.SortByDate THEN tt.file-create-time ELSE mNoSortI):

      FileName  = tt.file-name.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("mimfile.p","filename:" + GetNullStr(FileName)).
      &ENDIF

      IF NOT {assigned filename} THEN NEXT.      /* пустое имя пропускаем     */

      mOk = YES.
      RUN CheckTime(INPUT  tt-mail-user.op-kind-imp,
                    INPUT  tt-mail-user.mail-user-num,
                    INPUT-OUTPUT mOk).
      IF NOT mOk THEN NEXT.

      OUTPUT STREAM journal TO VALUE(mOrigin + "/journal.txt") append.
      buf = "mimfile.p " + string(today)                  +
            " " + string(time,"hh:mm:ss") +
            " " + filename. 
      PUT STREAM journal UNFORMATTED SKIP buf SKIP.
      OUTPUT STREAM journal CLOSE.

      {debug.i "Формат_обм польз режим_имп транз_имп"
               "tt-mail-user.mail-format           + ' ' +
                STRING(tt-mail-user.mail-user-num) + ' ' +
                tt-mail-user.exch-mode-imp         + ' ' +
                tt-mail-user.op-kind-imp"
      }

      RUN Init-SysMes ("AUTO," + tt-mail-user.mail-format,
                       "",
                       "").            /* Инициализация протокола   */

                                       /* Для многофилиальной базы */
      IF shMode THEN DO:
         shFilial = tt-mail-user.filial-id. 
         RUN SetEnvironment IN h_base (shFilial).
      END.

              OUTPUT STREAM journal TO VALUE(mOrigin + "/journal.txt") append.
              buf = "mimfile "  +  tt-mail-user.filial-id + "|" + STRING(tt-mail-user.mail-user-num) + "|" + tt-mail-user.mail-format + "|"  + tt-mail-user.dir. 
              PUT STREAM journal UNFORMATTED SKIP buf SKIP.
              OUTPUT STREAM journal CLOSE.

      
/*      RUN dbgprint.p ("mimfile.p","filename:" + STRING(tt-mail-user.mail-user-num) + ";" + GetNullStr(tt-mail-user.mail-format)). */

      CASE tt-mail-user.mail-format:
         WHEN "PN"        THEN DO:               /* Империал                  */
            IF OPSYS EQ "msdos" THEN
               dos  silent quoter VALUE(filename) > VALUE(mOrigin + "\_quoter.cli").
            ELSE IF OPSYS EQ "unix"  THEN
               unix silent quoter VALUE(filename) > VALUE(mOrigin + "/_quoter.cli").
            RUN imp-pn.p (INPUT tt-mail-user.mail-user-num,INPUT filename).
            os-delete VALUE(mOrigin + "/_quoter.cli").
         END.
         WHEN "PTB"       THEN DO:
            IF OPSYS EQ "msdos" THEN
               dos  silent quoter -d , VALUE(filename) > VALUE(mOrigin + "~\_quoter.cli").
            ELSE IF OPSYS EQ "unix" THEN
               unix silent quoter -d , VALUE(filename) > VALUE(mOrigin + "/_quoter.cli").
            RUN imp-ptb.p (INPUT tt-mail-user.mail-user-num).
            os-delete VALUE(mOrigin + "/_quoter.cli").
            OS-DELETE VALUE(filename).
         END.
         WHEN "CLI-BANK"  THEN 
         DO:
               RUN impfilep.p (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "EastLine"  THEN 
         DO:
               RUN i-east-l.p (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "DIASOFT"   THEN 
         DO:
               RUN i-dias.p   (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "DIAS-LN"   THEN 
         DO:
               RUN i-dia-ln.p (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "TELEX-BSS" THEN 
         DO:
              OUTPUT STREAM journal TO VALUE(mOrigin + "/journal.txt") append.
              buf = "mimfile  передаем в i-bss.p " +  filename. 
              PUT STREAM journal UNFORMATTED SKIP buf SKIP.
              OUTPUT STREAM journal CLOSE.
              RUN i-bss.p    (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).

         END.
         WHEN "RSHB"      THEN 
         DO:
              RUN imp-rshb.p (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "RSHB-REQ"  THEN 
         DO:
               RUN imq-rshb.p (INPUT tt-mail-user.file-exp,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "MGTS-TAG"  THEN 
         DO:
               RUN i-mgauto.p (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "CARD-TAG"  THEN 
         DO:
               RUN i-mgauto.p (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         WHEN "PB-PARAM"  THEN 
         DO:
               RUN pb-param.p (INPUT tt-mail-user.mail-user-num,
                                               INPUT filename).
              OS-DELETE VALUE(filename).
         END.
         OTHERWISE DO:
            IF tt-mail-user.mail-format MATCHES "*ARPO*"  THEN DO:    /* Наш К-Б */
               {debug.i "Номер корресп. :" STRING(tt-mail-user.mail-user-num)}
               RUN impfilea.p (INPUT tt-mail-user.mail-user-num,INPUT filename).
               OS-DELETE VALUE(filename).
            END.
         END.
      END CASE.

      RUN End-SysMes.                            /* Завершение  протокола     */
    
      

   END.
END.

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE read-dir:
   DEF INPUT PARAM in-dir  AS CHAR NO-UNDO.

   DEF VAR vName     AS CHAR NO-UNDO.
   DEF VAR vPath     AS CHAR NO-UNDO.
   DEF VAR vType     AS CHAR NO-UNDO.
   DEF VAR pos       AS INT64  NO-UNDO.
   DEF VAR pure-name AS CHAR NO-UNDO.

   file-info:file-name = in-dir  .
   IF FILE-INFO:file-type BEGINS "D" OR
      FILE-INFO:file-type BEGINS "LD" THEN DO:

      INPUT FROM os-dir(in-dir).
      REPEAT:
         IMPORT vName vPath vType.

         IF vType NE "F" THEN NEXT.

         CREATE tt.
         ASSIGN tt.file-name = vPath
                tt.ShortName = REPLACE(vName,".","|").

         file-info:file-name = vPath.

&IF INT64(SUBSTRING(PROVERS,1,1)) GE 9 &THEN
         ASSIGN
            tt.file-create-date = FILE-INFO:FILE-MOD-DATE
            tt.file-create-time = FILE-INFO:FILE-MOD-TIME.
&ELSE
         ASSIGN
            tt.file-create-date = TODAY
            tt.file-create-time = TIME.
&ENDIF

      END.
   END.
END PROCEDURE.
/******************************************************************************/
