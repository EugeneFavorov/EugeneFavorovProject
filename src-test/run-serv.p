/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: RUN-SERV.P
      Comment:
   Parameters:
         Uses:
      Used BY:
      Created:
     Modified: 27.10.2004 Om  Доработка: подключен механизм логирования сообщений.
     Modified: 30.10.2005 NIK Архивирование файла sysmess.log
     Modified:
*/

{globals.i}
DEF VAR delta         AS INT64               NO-UNDO.
DEF VAR vBuf          AS CHAR              NO-UNDO.
DEF VAR slash         AS CHAR INITIAL "~\" NO-UNDO.
DEF VAR vTimeReStart  AS CHAR              NO-UNDO.
DEF VAR vHourReStart  AS CHAR              NO-UNDO.
DEF VAR vMinReStart   AS CHAR              NO-UNDO.
DEF VAR vFlagFirst    AS LOGICAL           NO-UNDO.
DEF VAR vLastTime     AS INT64               NO-UNDO.
DEF VAR mMailUser     AS INT64               NO-UNDO.
DEF VAR mask-chk      AS CHAR              NO-UNDO.
DEF VAR mDirPath      AS CHAR              NO-UNDO.
DEF VAR mOk           AS LOG               NO-UNDO. /* проверяет время работы с абонентом */
DEF VAR mSortByName   AS LOGICAL           NO-UNDO.
DEF VAR mSortByDate   AS LOGICAL           NO-UNDO.
DEF VAR mSortCode     AS CHAR              NO-UNDO. /*код сортировки*/
DEF VAR mSortCode2    AS CHAR              NO-UNDO. /*код сортировки*/
DEF VAR mI            AS INT64               NO-UNDO.

DEFINE VAR mLogSize  AS INT64  NO-UNDO.
DEFINE VAR mLogName  AS CHAR     NO-UNDO.
DEFINE VAR mLogPath  AS CHAR     NO-UNDO.
DEFINE VAR mLogErr   AS INT64    NO-UNDO.
DEFINE VAR vFilialID AS CHAR     NO-UNDO.
DEFINE VAR vUSER_ID  AS CHAR     NO-UNDO.
DEFINE VAR vPASS_ID  AS CHAR     NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.
{bislogin.i}
/* Объявления для сборщика мусора */
{bqgc.def "NEW GLOBAL"}

{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get op}
{intrface.get strng}
{intrface.get import}
{intrface.get filex}
{intrface.get exch}
{intrface.get rights}
{intrface.get osyst}
{intrface.get tmess}    /* Инструменты обработки сообщений. */

{filial.pro}

vUSER_ID = OS-GETENV("USER_ID").
vPASS_ID = OS-GETENV("PASS_ID").
IF vUSER_ID NE ? AND
   vPASS_ID NE ? THEN
   SetUserID(vUSER_ID,vPASS_ID,"bisquit").

vFilialID =  getThisUserXAttrValue("filial-id").

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("run-serv.p", " userid: "  + userid("bisquit") + 
                              " vFilialID: "  + vFilialID).          
&ENDIF

      RUN SetConnectLink (vFilialID).
      RUN SetEnvironment (vFilialID).

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("run-serv.p", " shFilial:  "  + shFilial).
        
&ENDIF


{intrface.get swi}
{exchange.equ}

RUN Init-SysMes ("AUTO," + "ИНИЦИАЛИЗАЦИЯ", "", "").

&SCOP SESS-LIST SessMailServ
{tmailusr.def NEW}
{mimfile.i}

DEF NEW SHARED TEMP-TABLE mci-num NO-UNDO
                          FIELD pref AS INT64
                          FIELD num  AS INT64
                          INDEX num num.

DEF NEW SHARED        VAR flag-first  AS LOGICAL INIT YES  NO-UNDO.
DEF NEW GLOBAL SHARED VAR usr-printer LIKE printer.printer NO-UNDO.
FIND FIRST _user WHERE _user._userid EQ userid('bisquit')  NO-LOCK.
usr-printer = getPrinter().

RUN setuser IN h_base.

/*----------------------------------------------------------------------------*/

FUNCTION CurrentHour RETURN CHAR:
   RETURN ENTRY(1,STRING(TIME,"HH:MM:SS"),":").
END FUNCTION.

FUNCTION CurrentMinute RETURN CHAR:
   RETURN ENTRY(2,STRING(TIME,"HH:MM:SS"),":").
END FUNCTION.

FUNCTION CanRestart RETURN LOGICAL(INPUT iTime AS INT64) :
   DEFINE VARIABLE vCurTime AS INT64 NO-UNDO.
   FILE-INFO:FILE-NAME = mOrigin + "servstop.txt".
   vCurTime = FILE-INFO:FILE-MOD-TIME.
/*   IF vFilialID EQ "0000" THEN vCurTime = FILE-INFO:FILE-MOD-TIME - (3 * 3600).*/
   IF vFilialID EQ "0300" THEN vCurTime = FILE-INFO:FILE-MOD-TIME + (2 * 3600).
   IF vFilialID EQ "0500" THEN vCurTime = FILE-INFO:FILE-MOD-TIME + (3 * 3600).
/*   MESSAGE vFilialID ";" mOrigin + "servstop.txt" ";" vCurTime ";" iTime.*/
   RETURN  FILE-INFO:FILE-MOD-DATE NE TODAY OR
           (FILE-INFO:FILE-MOD-DATE EQ TODAY AND
            vCurTime LT iTime).
END FUNCTION.

FUNCTION LastHistoryTime RETURN INT64:
   DEF VAR vModifTime AS INT64 NO-UNDO.

   vModifTime = 0.
   FIND LAST history WHERE history.file-name  EQ "mail-user" AND
                           history.modif-date EQ TODAY NO-LOCK NO-ERROR.
   IF AVAIL history THEN   vModifTime =  history.modif-time.
   FIND LAST history WHERE history.file-name  EQ "catalog" AND
                           history.modif-date EQ TODAY NO-LOCK NO-ERROR.
   IF AVAIL history THEN   vModifTime =  history.modif-time.

   RETURN  vModifTime.

END FUNCTION.

FUNCTION UnixDate RETURN CHAR:
   DEF VAR vWeekDayName AS CHARACTER
       INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat" NO-UNDO.
   DEF VAR vMonthName AS CHARACTER
       INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" NO-UNDO.

   RETURN ENTRY(WEEKDAY(Today), vWeekDayName) + " " +
          ENTRY(MONTH(Today),   vMonthName)   + " " +
          STRING(DAY(Today),">9")             + " " +
          STRING(TIME, "HH:MM:SS")            + " " +
          "MSD"                               + " " + 
          STRING(YEAR(Today),"9999").
END FUNCTION.



{getpath.fun}
/*----------------------------------------------------------------------------*/

RUN InitComVar.

mLogSize = INT64(FGetSetting("BISMARK","РазмерЛога","0")) * 1000.
mLogPath =         FGetSetting("BISMARK","АрхивЛога","./").

IF mLogSize NE 0 THEN DO:
   mLogErr = SurelyCreateFolder(mLogPath).
   IF mLogErr NE 0 THEN DO:
      MESSAGE "Невозможно создать каталог:" mLogPath SKIP
              "Код ошибки:"                 mLogErr.
      quit.
   END.
END.

IF OPSYS      = "unix" THEN slash = "/".
mOrigin       = mOrigin + slash.

vTimeReStart  =  FGetSetting("BISMARK","ВремяОстанова","").

IF NUM-ENTRIES(VTimeReStart,":") GE 2 THEN ASSIGN
   vHourReStart  =  ENTRY(1,VTimeReStart,":")
   vMinReStart   =  ENTRY(2,VTimeReStart,":").

IF SEARCH(mOrigin + "servstop.txt") NE ? THEN DO:
   MESSAGE STRING(TIME,"hh:mm:ss")
           "Файл servstop.txt уже есть. Сервер НЕ ЗАПУЩЕН !".
   QUIT.
END.

vSessCode = OS-GETENV("SessCode").
vStrMU    = GetCodeMisc("SessMailServ",vSessCode,1).
IF NOT {assigned vStrMU} THEN vStrMU    = "*".

IF {assigned vSessCode} THEN DO:
   DO TRANSACTION:
      FIND FIRST code WHERE
             code.class EQ "{&SESS-LIST}"
         AND code.code  EQ vSessCode
                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAILABLE(code) THEN DO:
         RUN Fill-SysMes ("",
                          "",
                          "",
                          "%s=" +
                          STRING(TIME,"hh:mm:ss")  +
                          "Классификатор ~"{&SESS-LIST}~" с кодом " +
                          vSessCode  +
                          "используестя другой сессией mailserv или не существует." +
                          "~nСервер НЕ ЗАПУЩЕН !").
         QUIT.
      END.
   END.

   FIND FIRST code WHERE
              code.class EQ "{&SESS-LIST}"
          AND code.code  EQ vSessCode
              SHARE-LOCK.
   vBuf = GetCodeMisc("{&SESS-LIST}",vSessCode,2).
   vBuf = SUBSTRING(vBuf,1,2) + ":" + SUBSTRING(vBuf,3,2). /* в классификаторе хранится без двоеточия */
END.

IF NOT {assigned vBuf} THEN DO:
   vBuf = FGetSetting("BISMARK","Период","").
END.
ELSE
   vBuf = "00:" + vBuf.

RUN RunServ.

{intrface.del}          /* Выгрузка инструментария. */

RETURN.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE RunServ.
   DEFINE VARIABLE mInt AS INT64 NO-UNDO.
   ASSIGN
     delta = 3600 * INT64(SUBSTRING(vBuf,1,2)) +
               60 * INT64(SUBSTRING(vBuf,4,2)) +
                    INT64(SUBSTRING(vBuf,7,2)).

   IF delta < 1 THEN delta = 1.

   OUTPUT TO  VALUE(mOrigin + "servstop.txt").
      PUT UNFORMATTED STRING(TODAY,"99/99/99") " " string(time,"hh:mm:ss") SKIP.
   OUTPUT CLOSE.
   MESSAGE STRING(TODAY,"99/99/99") + " " + STRING(TIME,"hh:mm:ss") "Сервер запущен.".

   ASSIGN
      vFlagFirst  = YES
      vLastTime   = LastHistoryTime().

                        /* Делаем snapshot динамических объектов в памяти, 
                        ** чтобы не удалить их в процедуре сборки мусора */
   RUN bqgc.p({&GC_SAVEOBJECTS}).
   gsGC_SAVED = YES.
   mInt = 0. 
   DO WHILE TRUE:
      mInt = mInt + 1.
      IF    vFlagFirst
         OR LastHistoryTime() GT vLastTime THEN DO:

         RUN PrepTtMailUser.
         vFlagFirst = NO.
      END.
      vLastTime     = LastHistoryTime().

      IF FGetSetting("BISMARK","ДатаВремя","") EQ "Да"
         THEN MESSAGE UnixDate().
      MESSAGE STRING(TODAY,"99/99/99") + " " + STRING(TIME,"hh:mm:ss") " Цикл " + TRIM(STRING(mInt)) + ".".
      RUN mimfile.p  NO-ERROR.       /* Сиcтемы КЛБ и к ним приравненные   */
      RUN serv-imp.p NO-ERROR.       /* Импорт из каталогов                */
      RUN serv-exp.p NO-ERROR.       /* Экспорт в каталоги                 */
      RUN serv-snd.p NO-ERROR.       /* Рассылка из каталогов              */

      RUN ReleaseOpDate(TODAY).
      RUN UnLockFormat.

                        /* Удаляем мусор */
      RUN bqgc.p({&GC_DELOBJECTS}).
      IF SEARCH(mOrigin + "servstop.txt") EQ ? THEN DO:
         MESSAGE STRING(TODAY,"99/99/99") + " " + STRING(TIME,"hh:mm:ss") "Сервер остановлен.".
         QUIT.
      END.
      IF mLogSize NE 0 THEN DO:
         file-info:file-name = mOrigin + "sysmess.log".
         IF file-info:file-type BEGINS "F"      AND
            file-info:file-size GE     mLogSize THEN DO:

            mLogName = MakeFileName(mLogPath,
                                    DateToSwift(today,"yymmdd") + "-" +
                                    replace(string(time,"HH:MM"),":","")).

            os-rename  VALUE(mOrigin + "sysmess.log") VALUE(mLogName).
            os-command silent VALUE("gzip -q " + mLogName).
         END.
      END.
      /*  новая сессия mailserv */
      IF {assigned vTimeReStart} THEN DO:
         IF     CurrentHour()   EQ vHourReStart  
            AND CurrentMinute() GE vMinReStart   
            AND CanRestart(3600 * INT64(vHourReStart) + 60 * INT64(vMinReStart)) THEN
         DO:
            MESSAGE "Сервер перезапускается " STRING(TODAY,"99/99/99") + " " + STRING(TIME,"hh:mm:ss") + "..".   
            OS-DELETE VALUE(mOrigin + "servstop.txt").
            
            IF vFilialID EQ "0000" THEN
            DO:
               OS-COMMAND SILENT VALUE("cp " + mOrigin + "mailserv-00.txt" + " " +
                                               mOrigin + "/arch/" + 
                                                         string(year(today),"9999") +
                                                         string(month(today),"99") +
                                                         string(day(today),"99") +
                                                         "-mailserv-00.txt").
               OS-DELETE VALUE(mOrigin + "mailserv-00.txt").
            END.
            IF vFilialID EQ "0300" THEN
            DO:
               OS-COMMAND SILENT VALUE("cp " + mOrigin + "mailserv-03.txt" + " " +
                                               mOrigin + "/arch/" + 
                                                         string(year(today),"9999") +
                                                         string(month(today),"99") +
                                                         string(day(today),"99") +
                                                         "-mailserv-03.txt").
               OS-DELETE VALUE(mOrigin + "mailserv-03.txt").
            END.
            IF vFilialID EQ "0500" THEN
            DO:
               OS-COMMAND SILENT VALUE("cp " + mOrigin + "mailserv-05.txt" + " " +
                                               mOrigin + "/arch/" + 
                                                         string(year(today),"9999") +
                                                         string(month(today),"99") +
                                                         string(day(today),"99") +
                                                         "-mailserv-05.txt").
               OS-DELETE VALUE(mOrigin + "mailserv-05.txt").
            END.
/*            OS-COMMAND SILENT VALUE(FGetSetting("BISMARK","КомандаЗапуска","")) >> VALUE(mOrigin + "mailserv.txt") &.*/
            IF vFilialID EQ "0000" THEN
               OS-COMMAND SILENT VALUE(FGetSetting("BISMARK","КомандаЗапуска","")) >> VALUE(mOrigin + "mailserv.txt") &.
/*               OS-COMMAND SILENT VALUE(FGetSetting("BISMARK","КомандаЗапуска","")) >>*/
/*                                 VALUE(mOrigin + string(year(today),"9999") +        */
/*                                                 string(month(today),"99") +         */
/*                                                 string(day(today),"99") +           */
/*                                             	 "mailserv-00.txt") &.                 */
/*               OS-COMMAND SILENT VALUE("/home2/bis/quit41d/bq41d bismark start") >>*/
/*                                 VALUE(mOrigin + string(year(today),"9999") +      */
/*                                                 string(month(today),"99") +       */
/*                                                 string(day(today),"99") +         */
/*                                                 "mailserv-00.txt") &.             */
            IF vFilialID EQ "0300" THEN
               OS-COMMAND SILENT VALUE(FGetSetting("BISMARK","КомандаЗапуска","")) >> VALUE(mOrigin + "mailserv.txt") &.

            IF vFilialID EQ "0500" THEN
               OS-COMMAND SILENT VALUE(FGetSetting("BISMARK","КомандаЗапуска","")) >> VALUE(mOrigin + "mailserv.txt") &.
/*               OS-COMMAND SILENT VALUE(FGetSetting("BISMARK","КомандаЗапуска","")) >>*/
/*                                 VALUE(mOrigin + string(year(today),"9999") +        */
/*                                                 string(month(today),"99") +         */
/*                                                 string(day(today),"99") +           */
/*                                             	 "mailserv-05.txt") &.                 */
/*               OS-COMMAND SILENT VALUE("/home2/bis/quit41d/bq41d-03 bismark start") >>*/
/*                                 VALUE(mOrigin + string(year(today),"9999") +         */
/*                                                 string(month(today),"99") +          */
/*                                                 string(day(today),"99") +            */
/*                                                 "-mailserv-03.txt") &.               */
            QUIT.
         END.
      END.
      PAUSE delta.
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE PrepTtMailUser.

   DEF VAR vFilialId AS CHAR    NO-UNDO.
   DEF VAR vCanRead  AS LOGICAL NO-UNDO.

   {empty tt-dir}
   {empty tt-mail-user}

   mask-file   = OS-GETENV("MASK_FILE").
   vFilialId   = OS-GETENV("FILIAL_ID").

   IF shMode AND 
      vFilialId NE shFilial THEN DO:
      RUN Fill-SysMes ("","","","*** Не совпадают номера филиалов" +
                                " из глобальной переменной shFilial" +
                                " и из параметра окружения FILIAL_ID").
      RETURN.
   END.
   FOR EACH mail-user
       WHERE (
              (mail-user.exch-mode-imp BEGINS "авто" 
                AND (IF {assigned vFilialId}
                        THEN mail-user.filial-id EQ vFilialId 
                        ELSE YES))
           OR (mail-user.exch-mode-exp BEGINS "авто"
                AND (IF {assigned vFilialId}
                       THEN mail-user.filial-id EQ vFilialId 
                       ELSE YES))
              )
         AND CAN-DO(vStrMU,STRING(mail-user.mail-user-num))
       NO-LOCK
       BREAK BY mail-user.filial-id:

       {mluserpm.i}

       ASSIGN
          mMailUser = mail-user.mail-user-num
          mDirPath  = CatalogGetPath(mMailUser,{&DIR-IMPORT},"Path")
          mask-chk  = CatalogGetPath(mMailUser,{&DIR-IMPORT},"Mask").
          IF      {assigned mask-file} 
             AND  CAN-DO(mStrCliBank, mail-user.mail-format) THEN
             ASSIGN
                mask-chk = mask-file. /* маска - из переменных окружения  */
          IF NOT {assigned mask-chk} THEN 
             mask-chk = "*.*".

       CREATE tt-mail-user.
       Buffer-Copy mail-user TO tt-mail-user NO-ERROR.

       ASSIGN
          mSortByName = NO
          mSortByDate = NO
          tt-mail-user.mail-user-num = mMailUser
          tt-mail-user.mask-file     = mask-chk
          tt-mail-user.mode          = IF CAN-DO(mStrCliBank, mail-user.mail-format) AND
                                          mail-user.exch-mode-imp BEGINS "авто"
                                       THEN "cli"
                                       ELSE IF mail-user.exch-mode-imp BEGINS "авто"
                                       THEN "imp"
                                       ELSE "exp".

          mSortCode   = GetXattrValueEx("op-kind",
                                        mail-user.op-kind-imp,
                                        "ImportSort",
                                        "Name").
       CASE mSortCode:
          WHEN "Name"         THEN
             ASSIGN
                mSortByName = YES.
          WHEN "DateTime"     THEN
             ASSIGN
                mSortByDate = YES.
          WHEN "NameDateTime" THEN
             ASSIGN
                mSortByName = YES
                mSortByDate = YES.
       END CASE.

       FILE-INFO:file-name = mDirPath.
       IF file-info:FULL-PATHNAME NE ?  THEN
          mDirPath = file-info:FULL-PATHNAME.

       tt-mail-user.dir   = mDirPath.

       IF LENGTH(mDirPath) > 180 THEN
          RUN Fill-SysMes ("",
                           "",
                           "","%s=" +
                           "** У правила обмена " +
                           string(tt-mail-user.mail-user-num) +
                           " слишком длинная директория " +
                           mDirPath + " - обработана не будет !").

       IF tt-mail-user.mode EQ "cli"
          AND NOT CAN-FIND(FIRST tt-dir WHERE tt-dir.dir EQ mDirPath) THEN
       DO:
          CREATE tt-dir.
          ASSIGN
             tt-dir.dir         = mDirPath
             tt-dir.SortByName  = mSortByName
             tt-dir.SortByDate  = mSortByDate
          .
          END.
   END.

   /*поиск одинаковых директорий с разными сортировками*/
   FOR EACH tt-dir,
       EACH tt-mail-user WHERE
            tt-mail-user.dir  EQ tt-dir.dir NO-LOCK
       BREAK BY tt-mail-user.dir:

      mSortCode2 = GetXattrValueEx("op-kind",tt-mail-user.op-kind-imp,"ImportSort","Name").
      IF FIRST-OF(tt-mail-user.dir) THEN
         ASSIGN
            mI         = tt-mail-user.mail-user-num
            mSortCode = GetXattrValueEx("op-kind",tt-mail-user.op-kind-imp,"ImportSort","Name").
      ELSE IF mSortCode NE mSortCode2 THEN
      DO:
         MESSAGE "** У правил обмена " + string(tt-mail-user.mail-user-num) + " и " + string(mI) +
                 " с директорией " + tt-dir.dir + " разные сортировки " + mSortCode + " и " +
                 mSortCode2 + " соответсвенно !".
         PAUSE 0.
         DELETE tt-dir.
      END.
   END.
   RUN End-SysMes.                               /* Завершение  протокола     */

END PROCEDURE.


/******************************************************************************/
