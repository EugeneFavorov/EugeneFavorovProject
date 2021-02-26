/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: loanagval.p
      Comment: Осуществляет парсерный разбор шаблона отчета, 
               запускает процедуру, указанную в шаблоне с параметром,
               полученное значение записывает во временную таблицу.
               Работает только по формату имя_процедуры(параметры).
               Не требует описания переменных.
   Parameters:
         Uses:
      Used by: precrdprint.p
      Created: 14.10.2009 09:09 Jadv    
     Modified:
*/

{globals.i}
{intrface.get xclass}
{prn-doc.def &with_proc=YES}
{norm.i}

DEF INPUT PARAM iProc  AS CHAR NO-UNDO.   /* Имя процедуры, генератора отчетов */
DEF INPUT-OUTPUT PARAM TABLE FOR ttnames. /* Таблица для записи результатов */

DEF NEW SHARED VAR rid_loan  AS RECID.
DEF SHARED VAR rid-p     AS RECID NO-UNDO.

rid_loan = rid-p.

DEF VAR mSupportedProc   AS CHAR  NO-UNDO. /* Список обрабатываемых процедур */
DEF VAR mTag             AS CHAR  NO-UNDO. /* Функция */
DEF VAR mBraceBeg        AS INT64 NO-UNDO. /* Позиция начала скобок */
DEF VAR mBraceEnd        AS INT64 NO-UNDO. /* Позиция окончания скобок */
DEF VAR sTermRecidZam    AS INT64 NO-UNDO. /* Для хранения RECID(term-obl) по замене предмета залога*/
DEF VAR mProcName        AS CHAR  NO-UNDO. /* Наименование процедуры */
DEF VAR mProcParm        AS CHAR  NO-UNDO. /* Параметры процедуры */
DEF VAR mRetValue        AS CHAR  NO-UNDO. /* Возвращаемое значение */

DEF  NEW GLOBAL SHARED VAR sTermRecid AS RECID NO-UNDO. /* Для корректного поиска догюобеспечения в lgarterm.p */
DEF  NEW GLOBAL SHARED VAR sStr       AS CHAR  NO-UNDO. /* Для корректной работы loanagval при замене залога */

DEFINE TEMP-TABLE tt NO-UNDO
   FIELD NomDog   AS CHAR
   FIELD CodeVal  AS CHAR
   FIELD NomPP    AS INT
   FIELD ChVal    AS CHAR
   FIELD term-obl-id AS RECID.
   
DEFINE QUERY q_term FOR tt.
DEFINE BROWSE b_term QUERY q_term NO-LOCK 
DISPLAY
   tt.NomPP   COLUMN-LABEL "#"               FORMAT 99
   tt.NomDog  COLUMN-LABEL "НОМЕР ДОГОВОРА"  FORMAT "x(20)" 
   tt.CodeVal COLUMN-LABEL "ВИД"             FORMAT "x(45)"
/*   tt.ChVal   COLUMN-LABEL "ПОДСТАНОВКА"     FORMAT "x(45)" */
   WITH 5 DOWN WIDTH 73 TITLE "".

DEFINE FRAME f_term 
   b_term SKIP 
   WITH 1 COLUMN SIDE-LABELS ROW 5 CENTERED OVERLAY NO-BOX.

   /* Список поддерживаемых процедур */
mSupportedProc = "bankinfo,userinfo,dog,dog2,lgarterm,lcond,loanform,param,dps_prn".
RetString = YES.

sTermRecid = ?. /* sku */

/*RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",*/
/*   "iProc = " + iProc).                                    */

   /* Проверяем есть ли такая процедура в генераторе отчетов */
FIND FIRST user-proc WHERE
           user-proc.procedure EQ ENTRY(1,iProc,"|")
NO-LOCK NO-ERROR.
IF AVAIL user-proc THEN
DO:
   
/*   RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",*/
/*      "AVAIL(user-proc) = " + STRING(AVAIL(user-proc))).      */

   /* Цикл по шаблонам процедуры */
   FOR EACH reports WHERE 
            reports.name EQ ENTRY(1,iProc,"|")
   NO-LOCK:
         /* Обрабатываем только шаблоны содержащие <#Имя#> */
      
      IF NUM-ENTRIES(reports.txt, "#") GE 2 THEN
      DO:
            /* Выделяем процедуру и определяем позиции скобок */
         ASSIGN
            mTag      = ENTRY(2, reports.txt, "#")
            mBraceBeg = INDEX(mTag, "(") 
            mBraceEnd = INDEX(mTag, ")")
         . 
         
/*         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",*/
/*            "mTag = " + mTag).                                      */
         
         /* Если скобки существуют, то обрабатываем дальше */
         IF (mBraceBeg + mBraceEnd) GT 0 THEN
         DO:
            /* Получим имя процедуры и параметр */
            ASSIGN 
               mProcName = SUBSTR(mTag,             1, mBraceBeg - 1)
               mProcParm = SUBSTR(mTag, mBraceBeg + 1, mBraceEnd - mBraceBeg - 1)
            .
            
            /* Если процедура есть в списке поддерживаемых */
            IF CAN-DO(mSupportedProc, mProcName) THEN
            DO:
                  /* То запускаем ее на обработку, проверка на существование процедуры не нужна,
                  ** т.к. в списке поддерживаемых указываются реальные процедуры  */
               RUN VALUE(mProcName + ".p") (OUTPUT mRetValue,
                                            gbeg-date, 
                                            gend-date,
                                            mProcParm).
                  /* Получим результат обработки */
               ASSIGN
                  mRetValue = IF {assigned RETURN-VALUE} THEN RETURN-VALUE ELSE printtext
                  printtext = "" /* Надо обнулить */
               .
               
/*               RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",  */
/*                  "mProcName = " + mProcName + " mRetValue = " + mRetValue).*/
               
                  /* Пишем во временную табличку */
               RUN Insert_TTName(mTag,
                                 IF mRetValue BEGINS "[table" THEN mRetValue
                                                               ELSE REPLACE(mRetValue, "~n", " ")).

            END.
         END.
      END.
   END.  /* FOR EACH reports */
END.  /* AVAIL user-proc */

{intrface.del}
