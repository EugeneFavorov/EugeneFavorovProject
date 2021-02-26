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

DEF SHARED VAR rid_loan  AS RECID.
DEF SHARED VAR rid-p     AS RECID NO-UNDO.

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

message "5" view-as alert-box.

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
mSupportedProc = "bankinfo,userinfo,dog,lgarterm,lcond,loanform,param,dps_prn".
RetString = YES.

sTermRecid = ?. /* sku */

   /* Проверяем есть ли такая процедура в генераторе отчетов */
FIND FIRST user-proc WHERE
           user-proc.procedure EQ ENTRY(1,iProc,"|")
NO-LOCK NO-ERROR.
IF AVAIL user-proc THEN
DO:
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
            /* Если скобки существуют, то обрабатываем дальше */
         IF (mBraceBeg + mBraceEnd) GT 0 THEN
         DO:
               /* Получим имя процедуры и параметр */
            ASSIGN 
               mProcName = SUBSTR(mTag,             1, mBraceBeg - 1)
               mProcParm = SUBSTR(mTag, mBraceBeg + 1, mBraceEnd - mBraceBeg - 1)
            .
               /* Если процедура есть в списке поддерживаемых */

message "6" CAN-DO(mSupportedProc, mProcName)  mProcName view-as alert-box.
            IF CAN-DO(mSupportedProc, mProcName) THEN
            DO:
message "7" view-as alert-box.
               IF mProcName EQ "lgarterm" THEN
               DO:
                  /* sku выбор договора обеспечения */
message "8" view-as alert-box.
                  FIND FIRST loan WHERE
                       RECID(loan) EQ rid-p
                  NO-LOCK.
                  IF sTermRecid = ? THEN
                  DO:
                     FOR EACH term-obl WHERE term-obl.contract EQ loan.contract
                                        AND term-obl.cont-code EQ loan.cont-code
                                        AND term-obl.idnt EQ 5
                                        NO-LOCK:
                        CREATE tt.
                        ASSIGN
                          tt.NomDog  = GetXattrValueEx("term-obl", 
                                                  STRING(term-obl.contract + "," + 
                                                         term-obl.cont-code + ",5," + 
                                                         STRING(term-obl.end-date,"99/99/99") + "," + 
                                                         STRING(term-obl.nn)
                                                         ), 
                                                  "НомДогОб", "*"
                                                  )
                          tt.NomPP   = term-obl.nn
                          tt.CodeVal = GetCodeName("ВидОб", 
                                       GetXattrValueEx("term-obl", 
                                                                  STRING(term-obl.contract + "," + 
                                                                   term-obl.cont-code + ",5," + 
                                                                    STRING(term-obl.end-date,"99/99/99") + "," + 
                                                                 STRING(term-obl.nn)
                                                                 ), 
                                                        "ВидОб", "*"
                                                        )
                                                )
                          tt.term-obl-id = recid(term-obl)
                         .
                         ACCUMULATE term-obl.nn (count).
                     END. 
                     /* если один договор обеспечения*/
                     IF (ACCUM count term-obl.nn) < 2 THEN
                     DO:
                        FIND FIRST tt NO-LOCK NO-ERROR.
                        sTermRecid = IF AVAIL tt THEN tt.term-obl-id ELSE ?.
                     END.
                     /*выбор договора обеспечения только для указанных отчетов*/
                     IF sTermRecid = ? AND (ACCUM count term-obl.nn) > 1 THEN 
                     DO:
                        b_term:NUM-LOCKED-COLUMNS = 2.
                         b_term:TITLE = " [ " + DelFilFromLoan(loan.cont-code) + ", ВЫБОР ДОГОВОРА ОБЕСПЕЧЕНИЯ ] ".
                        OPEN QUERY q_term FOR EACH tt NO-LOCK BY tt.NomPP.
                        PAUSE 0.
                        VIEW b_term.
                        ENABLE ALL WITH FRAME f_term.
                        WAIT-FOR ENTER,ESC OF FRAME f_term.
                        HIDE FRAME f_term.
                        IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN 
                        DO:
                           sTermRecid = tt.term-obl-id.
                           /* если 2 договора обеспечения и печатаем заявление на замену залога, то определеяем sTermRecidZam */
                           IF (ACCUM count term-obl.nn) EQ 2 
                             AND CAN-DO("zay_zam_ts|,zay_zamts2|,spis_zalog|",sStr)
                             AND sTermRecid NE ? THEN
                           DO:
                              FIND FIRST tt WHERE tt.term-obl-id NE sTermRecid 
                              NO-LOCK NO-ERROR.
                             sTermRecidZam = IF AVAIL tt THEN tt.term-obl-id ELSE ?.
                           END.
                        END.
                        ELSE 
                        DO:
                           sTermRecid = ?.
                        END.
                        /* Определение RECID(term-obl) Используется для поиска параметров по замене залога */
                        IF sTermRecidZam = ? 
                           AND CAN-DO("zay_zam_ts|,zay_zamts2|,spis_zalog|",sStr) THEN
                        DO:
                           MESSAGE "ВЫБЕРИТЕ ДОГОВОР ДЛЯ ОПРИХОДОВАНИЯ" 
                           VIEW-AS ALERT-BOX TITLE "ИНФО".
                           b_term:NUM-LOCKED-COLUMNS = 2.
                           b_term:TITLE = " [ " + DelFilFromLoan(loan.cont-code) + ", ВЫБОР ДОГОВОРА ДЛЯ ОПРИХОДОВАНИЯ ] ".
                      
                           OPEN QUERY q_term FOR EACH tt NO-LOCK BY tt.NomPP.
                
                           PAUSE 0.
                           VIEW b_term.
             
                           ENABLE ALL WITH FRAME f_term.
                           WAIT-FOR ENTER,ESC OF FRAME f_term.
                           HIDE FRAME f_term.
                           IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN 
                           DO:
                              sTermRecidZam = tt.term-obl-id.
                           END.
                        END.
                
                     END.
                 END.
                 /* Используется для поиска параметров по замене залога */
                 IF (NUM-ENTRIES(mProcParm, "|") EQ 4 
                    AND ENTRY(4, mProcParm, "|") EQ "ЗамЗал") THEN 
                    mProcParm = mProcParm + ",###" + STRING(sTermRecidZam).
                 ELSE 
                    mProcParm = mProcParm + ",###" + STRING(sTermRecid).  /*RECID(term-obl)*/
                 END.
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
                 /* Пишем во временную табличку */
                 RUN Insert_TTName(mTag,
                                 IF mRetValue BEGINS "[table" THEN mRetValue
                                                               ELSE REPLACE(mRetValue, "~n", " ")).

             END.
message "22" view-as alert-box.
          END.
message "23" view-as alert-box.
      END.
message "24" view-as alert-box.
   END.  /* FOR EACH reports */
END.  /* AVAIL user-proc */
{intrface.del}
