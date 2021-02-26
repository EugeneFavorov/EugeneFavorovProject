/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: hist-rep.p
      Comment: Отчет по журналу изменений
   Parameters: -
         Uses: Global.I
      Used by:
      Created: 08/01/98 13:54 Dima
     Modified: 28.05.2006 16:28 KSV      (0059104) Оптимизация индексов
                                         history. Отказ от индекса
                                         date-time-file.
     Modified: 08.11.2006 18:11 Daru     <comment>
     Modified: 16.07.2010 11:20 SOLM     (0126949) Для совместимости работы с
                                         QBIS валидациия введенных значений 
                                         вынесена в триггер ON LEAVE
*/
Form "~n@(#) Hist-Rep.P 1.0 Dima 08/01/98 Dima 13/01/98"
     with frame sccs-id stream-io width 250.

{globals.i}
{history.def}
{intrface.get tmess}    /* Инструменты обработки сообщений. */

DEF VAR f-file-name   LIKE history.file-name  INIT ''         NO-UNDO.
DEF VAR f-user-id     LIKE history.user-id    INIT '*'        NO-UNDO.
DEF VAR f-modif-date1 LIKE history.modif-date INIT 01/01/1990 NO-UNDO.
DEF VAR f-modif-date2 LIKE history.modif-date INIT TODAY      NO-UNDO.
DEF VAR f-modif-time1 LIKE history.modif-time INIT 0          NO-UNDO.
DEF VAR f-modif-time2 LIKE history.modif-time INIT 86400      NO-UNDO.
DEF VAR f-modify      LIKE history.modify     INIT '*'        NO-UNDO.
DEF VAR f-modify-c    AS   LOG                INIT TRUE       NO-UNDO.
DEF VAR f-modify-w    AS   LOG                INIT TRUE       NO-UNDO.
DEF VAR f-modify-d    AS   LOG                INIT TRUE       NO-UNDO.
DEF VAR f-detailed    AS   LOG                INIT TRUE       NO-UNDO.
DEF VAR f-fields      AS   CHAR                               NO-UNDO.
DEF VAR f-values      AS   CHAR                               NO-UNDO.

DEF VAR cPrg          AS   CHAR EXTENT 4 INIT ['-','~\','~|','~/'] NO-UNDO.
DEF VAR iPrg          AS   INT64   INIT 1   NO-UNDO.
DEF VAR nPrg          AS   INT64   INIT 0   NO-UNDO.

DEF VAR mh11          AS   INT64   INIT 0   NO-UNDO.
DEF VAR mh12          AS   INT64   INIT 23  NO-UNDO.
DEF VAR mm11          AS   INT64   INIT 0   NO-UNDO.
DEF VAR mm12          AS   INT64   INIT 59  NO-UNDO.
DEF VAR ms11          AS   INT64   INIT 0   NO-UNDO.
DEF VAR ms12          AS   INT64   INIT 59  NO-UNDO.

DEF VAR i             AS   INT64            NO-UNDO.
DEF VAR nn            AS   INT64            NO-UNDO.
DEF VAR ts            AS   CHAR           NO-UNDO.

DEF VAR f-i           AS   INT64            NO-UNDO. /* f-fields idx */
DEF VAR v-i           AS   INT64            NO-UNDO. /* f-values idx */

DEF VAR f-f           AS   LOG            NO-UNDO. /* f-fields flag */
DEF VAR f-v           AS   LOG            NO-UNDO. /* f-values flag */

DEF VAR tf-f          AS   CHAR           NO-UNDO. /* tmp 4 f-fields */
DEF VAR tf-v          AS   CHAR           NO-UNDO. /* tmp 4 f-values */

DEF VAR mCnt          AS   INT64            NO-UNDO.
DEF VAR mNum          AS   INT64            NO-UNDO.
DEF VARI mTable       AS   CHAR           NO-UNDO.

DEF NEW SHARED VAR list-id AS CHAR NO-UNDO. /* 4 Op-User.P */

FORM
   {hist-rep.frm &toggle-format="VIEW-AS TOGGLE-BOX"}
WITH FRAME in-set OVERLAY CENTERED ROW 5 WIDTH 80 NO-LABELS COLOR messages
   TITLE '[ ВВЕДИТЕ ДАННЫЕ ]'.

FORM
   {hist-rep.frm &toggle-format="FORMAT '√/-'"}
WITH FRAME out-set NO-LABELS
   TITLE '[ ВЫБОРКА ПРОИЗВОДИТСЯ ПО СЛЕДУЮЩИМ КРИТЕРИЯМ ]'.

COLOR DISPLAY INPUT
   f-modify-c
   f-modify-w
   f-modify-d
   f-detailed
WITH FRAME in-set.

FORM
   nn                  FORMAT '>>>>>>>>9' COLUMN-LABEL 'NN'
   history.file-name
   history.user-id
   history.modif-date
   history.modif-time
   history.modify      FORMAT 'x(4)'      COLUMN-LABEL 'ВИД!ИЗМ'
   history.field-name  FORMAT "x(40)"
   history.field-value
HEADER
  'ОТЧЕТ ПО ЖУРНАЛУ ИЗМЕНЕНИЙ'
   TODAY FORMAT '99/99/9999' TO 55
   STRING(TIME,'hh:mm:ss')   TO 65
  'Стр.' TO 73 PAGE-NUMBER FORMAT '>>>,>>9' TO 81 SKIP(1)
WITH FRAME list DOWN WIDTH 255.

FUNCTION modifName RETURN CHAR (
   INPUT mType AS CHAR
):
   IF INDEX('{&hi-all}',mType) EQ 0 THEN
      RETURN '???'.
   ELSE
      RETURN SUBSTR(ENTRY(INDEX('{&hi-all}',mType),'{&hi-modify}'),1,4).
END FUNCTION.

IF NOT CAN-FIND(FIRST history) THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "1", "Журнал изменений пуст!").
   RETURN.
END.

f-file-name = "person".

ASSIGN
   f-modif-date1 = TODAY
   mh11          = 0
   mm11          = 0
   ms11          = 0
   f-modif-date2 = TODAY
   mh12          = 23
   mm12          = 59
   ms12          = 59
  .

ON LEAVE OF FRAME in-set ANYWHERE
DO:
   DEFINE VARIABLE vMsg AS CHARACTER NO-UNDO.
   vMsg = ?.

   CASE SELF:NAME:
      WHEN 'f-file-name' THEN
      DO:
         IF INDEX(SELF:SCREEN-VALUE,"*") > 0 OR SELF:SCREEN-VALUE = "" THEN
         DO:
            vMsg = "Введите имя таблицы или список таблиц, использование маски недопустимо".
         END.
      END.
      WHEN 'mh11' OR WHEN 'mh12' THEN
      DO:
         IF  INT64(SUBSTRING(SELF:SCREEN-VALUE,1,2)) > 23 THEN
         DO:
            vMsg = "Это часы!". 
         END.
      END.
      WHEN 'mm11' OR WHEN 'mm12' THEN
      DO:
         IF  INT64(SUBSTRING(SELF:SCREEN-VALUE,1,2)) > 59 THEN
         DO:
            vMsg = "Это минуты!". 
         END.
      END.
      WHEN 'ms11' OR WHEN 'ms12' THEN
      DO:
         IF  INT64(SELF:SCREEN-VALUE) > 59 THEN
         DO:
            vMsg = "Это секунды!". 
         END.
      END.
   END CASE.

   IF vMsg <> ? THEN
   DO:
      MESSAGE vMsg VIEW-AS ALERT-BOX.
      RETURN NO-APPLY. 
   END.

   RETURN.
END.
f-user-id = "".
ON F1 OF FRAME in-set ANYWHERE
DO:
   CASE FRAME-FIELD:
      WHEN 'f-file-name' THEN 
      DO WITH FRAME in-set:
         ASSIGN f-file-name.
         RUN histfrep.p(f-file-name,4).
         IF LASTKEY EQ 10 THEN 
         DO:
            f-file-name = pick-value.
            DISP f-file-name WITH FRAME in-set.
         END.
      END.
      WHEN 'f-user-id' THEN
      DO:
         ASSIGN
            f-user-id
            list-id   = f-user-id
         .
         DO TRANSACTION:
            RUN op-user.p(4).
         END.
         IF LASTKEY EQ 10 THEN DO:
            f-user-id = list-id.
            DISP f-user-id WITH FRAME in-set.
         END.
      END.
   END CASE.
END.
f-modify-w = false.
f-modify-d = false.
f-detailed = false.

PAUSE 0.
DO 
ON ERROR  UNDO, LEAVE
ON ENDKEY UNDO, LEAVE:
   UPDATE
      f-file-name
      f-user-id
      f-modif-date1
      mh11
      mm11
      ms11

      f-modif-date2
      mh12
      mm12
      ms12

      f-modify-c
      f-modify-w
      f-modify-d

      f-detailed

      f-fields
      f-values
   WITH FRAME in-set.
END.
HIDE FRAME in-set NO-PAUSE.
IF KEYFUNC(LASTKEY) EQ "end-error" THEN RETURN.

ASSIGN
   f-modif-time1 = mh11 * 3600 + mm11 * 60 + ms11
   f-modif-time2 = mh12 * 3600 + mm12 * 60 + ms12
   nn            = 0
.

IF f-fields EQ ? THEN f-fields = ''.
IF f-values EQ ? THEN f-values = ''.

{setdest3.i}

DISP
   f-file-name   FORMAT 'x(80)'
   f-user-id     FORMAT 'x(80)'
   f-modif-date1
   f-modif-date2
   mh11 mm11 ms11
   mh12 mm12 ms12
   f-modify-c
   f-modify-w
   f-modify-d
   f-detailed

   f-fields      FORMAT 'x(80)'
   f-values      FORMAT 'x(80)'
WITH FRAME out-set.
PAGE.

ASSIGN
   f-modify = ''
   f-modify = if f-modify-c then 'c' else ''
   f-modify = f-modify + if f-modify-w then (if f-modify <> '' then ',' else '') + 'w' else ''
   f-modify = f-modify + if f-modify-d then (if f-modify <> '' then ',' else '') + 'd' else ''
         /* Меняем заголовок колонки в зависимости от вида отчета ("простой"/"детальный") */
   history.field-name:LABEL IN FRAME list = IF f-detailed THEN "НАИМЕНОВАНИЕ ПОЛЯ" ELSE "ОБЪЕКТ".
.

mNum = NUM-ENTRIES(f-file-name).
MAIN:
DO mCnt = 1 TO mNum:
   mTable = ENTRY(mCnt,f-file-name).
   BLOCK_HISTORY:
   FOR EACH history WHERE history.file-name   EQ mTable
                      AND CAN-DO(f-user-id, history.user-id)
                      AND history.modif-date  GE f-modif-date1
                      AND history.modif-date  LE f-modif-date2
                      AND CAN-DO(f-modify,  history.modify) 
   NO-LOCK:
      IF nPrg MOD 100 EQ 0 THEN
      DO:
         PUT SCREEN ROW SCREEN-LINES + 3 COL 15
            '(' + cPrg[iPrg MOD 4 + 1] + ') ' +
            STRING(history.modif-date,'99/99/99') +
            ' Стр.' + STRING(PAGE-NUMBER,'>>>,>>9')
         .
         iPrg = iPrg + 1.
      END.
      nPrg = nPrg + 1.
   
      IF     history.modif-date EQ f-modif-date1 
         AND history.modif-time LT f-modif-time1 
         OR  history.modif-date EQ f-modif-date2
         AND history.modif-time GT f-modif-time2
      THEN NEXT BLOCK_HISTORY.
   
      READKEY PAUSE 0.
      IF KEYFUNCTION(LASTKEY) EQ 'END-ERROR' THEN
      DO:
         PUT UNFORMATTED
         SKIP(1)
         '*** ПОЛУЧЕНИЕ ОТЧЕТА ПРЕРВАНО ПОЛЬЗОВАТЕЛЕМ ***'
         SKIP(1).
         LEAVE MAIN.
      END.
   
      /* Check f-fields & f-values */
      IF    f-fields NE '' 
         OR f-values NE '' 
      THEN DO:
         ASSIGN
            f-f = FALSE
            f-v = FALSE
         .
   
         IF f-fields NE '' THEN
         DO:
            tf-f = ''.
            DO f-i = 1 TO NUM-ENTRIES(history.field-val) - 1 BY 2: /* !!! changed in BQ4.2 !!! */
               {additem.i tf-f ENTRY(f-i,history.field-val)}
            END.
            DO f-i = 1 TO NUM-ENTRIES(f-fields):
               IF CAN-DO(ENTRY(f-i,f-fields),tf-f) THEN 
               DO:
                  f-f = TRUE.
                  LEAVE.
               END.
            END.
         END.
         ELSE
            f-f = TRUE.
   
         IF f-values NE '' THEN 
         DO:
            tf-v = ''.
            DO v-i = 2 TO NUM-ENTRIES(history.field-val) - 1 BY 2: /* !!! changed in BQ4.2 !!! */
               {additem.i tf-v ENTRY(v-i,history.field-val)}
            END.
            DO v-i = 1 TO NUM-ENTRIES(f-values):
               IF CAN-DO(ENTRY(v-i,f-values),tf-v)
               THEN DO:
                  f-v = TRUE.
                  LEAVE.
               END.
            END.
         END.
         ELSE
            f-v = TRUE.

         IF NOT (f-f AND f-v) THEN NEXT BLOCK_HISTORY.
      END.
   
      /*----------------------------*/
      nn   = nn   + 1.

      DISP
         nn
         history.file-name
         history.user-id
         history.modif-date
         STRING(history.modif-time,'hh:mm:ss')  @ history.modif-time
         modifName(history.modify)              @ history.modify
         history.field-ref WHEN NOT f-detailed  @ history.field-name
         REPLACE(REPLACE(history.field-value,'~n',' '),'~002',',') 
                           WHEN NOT f-detailed  @ history.field-value
   
         "$" 
            WHEN f-detailed 
             AND NUM-ENTRIES(history.field-value) > 1
                                                @ history.field-name
         history.field-ref 
            WHEN f-detailed 
             AND NUM-ENTRIES(history.field-value) > 1
                                                @ history.field-value
      WITH FRAME list.
      DOWN WITH FRAME list.
   
      IF f-detailed THEN
      DO:
         i = 1.
         DO WHILE i LE NUM-ENTRIES(history.field-value) - 1: /* "1-" since field-value always ends by ',' */
            ts = REPLACE(ENTRY(i + 1,history.field-value),'~n',' ').
            {tolist.i ts}
            DISP
               ENTRY(i,history.field-value) @ history.field-name
               ts                           @ history.field-value
            WITH FRAME list.
            DOWN WITH FRAME list.
            i = i + 2.
         END.
      END.
   END.  /* of BLOCK_HISTORY (FOR EACH) block */
END.  /* of MAIN block */

{preview3.i}

{intrface.del}          /* Выгрузка инструментария. */ 
