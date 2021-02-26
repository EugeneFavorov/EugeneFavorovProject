/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: loanLD.p
      Comment: Заполнение таблици recid отобранных  в фильтр записей.
   Parameters: "-1" - Не найдена настройка фильтра.
   Parameters:
         Uses: all_flt.p
      Used by: 
      Created: 06/03/00 Om 
     Modified: 24/05/2002 Om Доработка: подключение к динамическому фильтру.
                             Недочет  : код ошибки всегда "-1". :-(
     Modified: 08/08/2003 Om Доработка: Поиск настройки фильтра с учетом sub-code.
  Last change:
*/

form "~n@(#) loanLD.p 1.0 Om 06/03/00"
with frame sccs-id stream-io width 250.

{globals.i}         /* Глобальные переменные сессии. */
{flt-file.i new}    /* Определение структуры динамического фильтра. */
{all_note.def}      /* Таблица с recid, выбранных по фильтру записей Shared */
{flt_var.def}       /* Переменные, общие для всех типов фильтров Shared */

{tmpobj.def}      /* Таблица для работы с браузером. */
{tmprecid.def}    /* Для передачи отобраных записей. */

{intrface.get "rights"} /* Загрузка инструментария определения достуа. */
{wclass.i}

def var list-class as char no-undo. /* список всех подклассов текущего класса */
def var num-class  as INT64  no-undo. /* N класса */

DEF VAR sub_user    AS CHAR NO-UNDO. /* Пользователь и его подчиненные. */
DEF VAR class_avail AS CHAR NO-UNDO. /* Перечень доступных классов. */
DEF VAR vContract   AS CHAR NO-UNDO. /* Для QRY     */
DEF VAR incontr     AS CHAR INIT "Кредит" NO-UNDO. /* Назначение договора. */
DEF VAR vClass      AS CHAR   NO-UNDO.
DEF VAR vCounter    AS INT64  NO-UNDO.


DEFINE VARIABLE iPotok  AS INT64 NO-UNDO.
DEFINE VARIABLE iFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDelim  AS INT64 NO-UNDO.
DEFINE VARIABLE mInt AS INTEGER NO-UNDO.

mInt = 0.

/*
#  1 - начальный поток
#  2 - конечный  поток
#  3 -           поток
#  4 - количество потоков
#  5 - филиал
#  6 - список транзакций через ;
#  7 - на всякий случай
*/

IF NUM-ENTRIES(session:parameter) GE 4 THEN
ASSIGN
   iPotok = INT64(ENTRY(3,session:parameter))
   iFilial = ENTRY(5,session:parameter)
   iDelim  = INT64(ENTRY(4,session:parameter)).
ELSE 
ASSIGN
   iPotok = INT64(OS-GETENV("POTOK"))
   iFilial = OS-GETENV("FILIAL")
   iDelim  = INT64(OS-GETENV("DELIM")).

FOR EACH loan WHERE
      (loan.close-date EQ ? OR
       loan.close-date GE TODAY)
   AND loan.contract   EQ "КРЕДИТ"
   AND loan.filial-id  = iFilial
   AND INT64(RECID(loan)) MODULO iDelim EQ iPotok
   NO-LOCK:

   CREATE all_recids.
   CREATE TmpObj.
   CREATE tmprecid.
   ASSIGN
      i = i + 1
      mInt = mInt + 1
      all_recids.count = i
      all_recids.rid   = RECID(loan)
      TmpObj.rid       = RECID(loan)
      tmprecid.id      = RECID(loan)
   .
END.

/* Выгрузка инструментов. */
{intrface.del "rights"}

RETURN.

PROCEDURE SelectRecords.

   DEFINE INPUT  PARAMETER iH    AS HANDLE  NO-UNDO. /* хэндл броузера */
   DEFINE OUTPUT PARAMETER oCont AS LOGICAL NO-UNDO. /* показывать/не показывать броузер */

   DEFINE VARIABLE vHQ     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE vHB     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE vCnt    AS INT64 NO-UNDO.
   DEFINE VARIABLE vbuf-id AS INT64 INIT ? NO-UNDO.

   RUN Open-Query IN iH. /* открыть запрос в броузере */

   vHQ = DYNAMIC-FUNCTION("GetHandleQuery" IN iH).

   IF NOT vHQ:IS-OPEN THEN RETURN.

   DO vCnt = 1 TO vHQ:NUM-BUFFERS:
      IF vHQ:GET-BUFFER-HANDLE(vCnt):TABLE EQ "loan" THEN
      DO:
         vbuf-id = vCnt.
         LEAVE.
      END.
   END.
   {empty all_recids}
   IF vbuf-id <> ? THEN
   DO:
      vHB = vHQ:GET-BUFFER-HANDLE(vbuf-id).
      RUN GetFirstRecord IN iH (vHQ).
      DO WHILE NOT vHQ:QUERY-OFF-END:
         CREATE all_recids.
         CREATE TmpObj.
         CREATE tmprecid.
         ASSIGN
            i = i + 1
            all_recids.count = i
            all_recids.rid   = vHB:RECID
            TmpObj.rid       = vHB:RECID
            tmprecid.id      = vHB:RECID
         .
         RUN GetNextRecord IN iH (vHQ).
      END. 
   END.

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='17/08/2015 07:51:17.810+04:00' */
/* $LINTUSER='glaa' */
/* $LINTMODE='1' */
/* $LINTFILE='loanld.p' */
/*prosign0RTDPKaDd0atZCfeYIApmg*/