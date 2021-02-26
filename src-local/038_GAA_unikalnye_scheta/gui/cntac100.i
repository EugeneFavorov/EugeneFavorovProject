
/* +++ cntac100.i was humbly modified by (c)blodd converter v.1.09 on 11/10/2016 7:23am +++ */

{intrface.get acct}     /* Библиотека для работы со счетами. */
                        /* Поиск счета второго порядка. */
FIND FIRST bal-acct WHERE
   bal-acct.bal-acct EQ iBalAcct
NO-LOCK.

&IF DEFINED(nodef) = 0 &THEN
   {acct-sh.def &SHARE_MODE="NEW SHARED" &nodefbranch=YES}
&ELSE
   {acct-sh.def &SHARE_MODE="SHARED"}
&ENDIF

&IF DEFINED (nodef) = 0
&THEN
   DEF VAR acctmask     AS CHAR  NO-UNDO  FORMAT "x(25)".
   DEF VAR vTokIdx      LIKE tokidx NO-UNDO.
   DEF VAR vacct-cat    AS CHAR  NO-UNDO.
   DEF VAR vKodDoxRash  AS CHAR  NO-UNDO.
   DEF VAR sTokidx      AS CHAR  NO-UNDO.
   DEF VAR sToklen      AS CHAR  NO-UNDO.
   DEF VAR i            AS INT64   NO-UNDO.

   vacct-cat = bal-acct.acct-cat.

   RUN "FindAcctMask" IN h_acct (
      vclass, 
      inbal-acct,
      INPUT-OUTPUT acctmask,
      INPUT-OUTPUT vKodDoxRash 
   ) NO-ERROR.
   IF    ERROR-STATUS:ERROR
      OR {&RETURN_VALUE} EQ "error"
      THEN RETURN "exit".

   RUN GetAcctMask IN h_acct (acctmask,
                              OUTPUT tokacct,    
                              OUTPUT sTokidx,    
                              OUTPUT sToklen
   ) NO-ERROR.      
   IF ERROR-STATUS:ERROR
   OR {&RETURN_VALUE} EQ "error"
      THEN RETURN "exit".

   DO i = 1 TO EXTENT(vTokIdx):
      vTokIdx[i] = INT64(ENTRY(i,sTokidx)).
      IF i LE EXTENT(toklen)
         THEN toklen[i] = INT64(ENTRY(i,sToklen)).
   END.
   tmp-branch-id = TRIM(GetUserBranchId(USERID("bisquit"))).
&ENDIF
                        /* Формирвоание номера счета. */
RUN counacct (iBalAcct, iCurrency, tokacct, IF toklen[{&TOK_BRANCH_IDX}] NE 0 THEN tmp-branch-id ELSE dept.branch, num).

/* Проверка счета в резерве,
** Если в резерве нет счета, сохранение номера в резерве.
** Возвращает YES - в случае сохранении счета в резерве. */
FUNCTION AcctChckRsrv RETURN LOG PRIVATE (
   INPUT iSurrogate AS CHAR /* Счет резерва. */
):
   DEF VAR vOk AS LOG. /* Признак сохранения счета в резерве NO-UNDO нет. */

   DEF BUFFER bis-temp-table FOR bis-temp-table. /* Локализация буфера. */
   FIND FIRST bis-temp-table WHERE
      bis-temp-table.surrogate-id EQ iSurrogate
   NO-LOCK NO-ERROR.
   IF NOT AVAIL bis-temp-table
   THEN
   BLCK_RSRV:
   DO
   ON ERROR  UNDO BLCK_RSRV, LEAVE BLCK_RSRV
   ON ENDKEY UNDO BLCK_RSRV, LEAVE BLCK_RSRV:
      /* Проверка на блокировку.
      ** В данном алгоритме существует ошибка синхронного доступа.
      ** Если зарезервированных счетов нет (bis-tt пуст), то производим
      ** резервирование внутри транзакции по созданию счета. До тех пор пока
      ** транзакция не будет завершена (счет не буедт создан), созданным
      ** резервом воспользоваться будет нельзя из-за блокировки записи.
      ** Т.о. при создании счета во второй сессии (параллельно), будет производится
      ** повторное резервирование номеров счета.
      ** Раннее, анализ блокировки не производился, что приводило к ошибке совместного
      ** доступа к записям в таблице bis-tt. */
      FIND FIRST bis-temp-table WHERE
         bis-temp-table.surrogate-id EQ ( IF shModeMulty THEN shFilial + "@" ELSE "") + iSurrogate
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF       NOT AVAIL bis-temp-table
         AND   NOT LOCKED (bis-temp-table)
      THEN DO:
         CREATE bis-temp-table.
         ASSIGN
            bis-temp-table.surrogate-id   = ( IF shModeMulty THEN shFilial + "@" ELSE "") + iSurrogate
            vOk                           = YES
         .
      END.
   END.
   RETURN vOk.
END FUNCTION.

/* Процедура формирования номера счета с возможностью заполнения "дырок". */
PROCEDURE counacct PRIVATE.
   DEF INPUT  PARAM inbal-acct   AS INT64    NO-UNDO. /* Счет второго порядка. */
   DEF INPUT  PARAM incur        AS CHAR   NO-UNDO. /* Валюта счета. */
   DEF INPUT  PARAM tokacct      AS CHAR   NO-UNDO. /* Маска счета. */
   DEF INPUT  PARAM iBranch      AS CHAR   NO-UNDO. /* Код подразделения. */
   DEF INPUT  PARAM num          AS INT64    NO-UNDO. /* Количество резервируемых счетов. */

   DEF VAR ac        AS CHAR   NO-UNDO.
   DEF VAR j         AS INT64    NO-UNDO.
   DEF VAR i         AS INT64    NO-UNDO.
   DEF VAR ii        AS DEC    NO-UNDO. /* Счетчик найденных счетов. */
   DEF VAR jj        AS DEC    NO-UNDO. /* Количество зарезервированных счетов. */
   DEF VAR mm        AS DEC    NO-UNDO.
   DEF VAR kk        AS INT64    NO-UNDO. /* Доп. кол-во счетов, для резерва.*/
   DEF VAR curnum    AS DEC    NO-UNDO. /* Значение счетчика из найденного номера. */
   DEF VAR st        AS CHAR   NO-UNDO. /* Прототип счета для резерва. */
                        /* Позиции счетчика в маске счета.
                        ** Разрядность счетчика ограничена 11 позициями. */
   DEF VAR cnt-srt   AS INT64    NO-UNDO EXTENT 11.

   DEF BUFFER acct FOR acct. /* Локализация буфера. */

   ASSIGN
                        /* Заполняем нулями счет, по кол-ву позиций в маске. */
      ac       = FILL ("0", LENGTH (tokacct))
      j        = 0
      cnt-srt  = 100
   .
                        /* Формируем разрядность счетчика, согласно маске. */
   DO i = 1 TO LENGTH (ac):
      IF SUBSTR (tokacct, i, 1) EQ "с"
      THEN ASSIGN
         j           = j + 1
         cnt-srt [j] = i
      .
   END.
                        /* Перебираем все счета
                        ** по счету второго порядка и валюте,
                        ** с сортировкой по позиция счетчика. */
   SRCH_BLCK:
   FOR EACH acct where
            acct.bal-acct  EQ inbal-acct
      AND   acct.currency  EQ incur
      AND   acct.acct      NE ?
   NO-LOCK
   BY SUBSTR (acct.acct, cnt-srt [1],  1)
   BY SUBSTR (acct.acct, cnt-srt [2],  1)
   BY SUBSTR (acct.acct, cnt-srt [3],  1)
   BY SUBSTR (acct.acct, cnt-srt [4],  1)
   BY SUBSTR (acct.acct, cnt-srt [5],  1)
   BY SUBSTR (acct.acct, cnt-srt [6],  1)
   BY SUBSTR (acct.acct, cnt-srt [7],  1)
   BY SUBSTR (acct.acct, cnt-srt [8],  1)
   BY SUBSTR (acct.acct, cnt-srt [9],  1)
   BY SUBSTR (acct.acct, cnt-srt [10], 1)
   BY SUBSTR (acct.acct, cnt-srt [11], 1):

      ASSIGN
                        /* Подсчет количества найденных счетов. */
         ii       = ii + 1
                        /* Формирование текущего значения счетчика в счете. */
         curnum   = DEC (
                     SUBSTR (acct.acct, cnt-srt[1],  1) +
                     SUBSTR (acct.acct, cnt-srt[2],  1) +
                     SUBSTR (acct.acct, cnt-srt[3],  1) + 
                     SUBSTR (acct.acct, cnt-srt[4],  1) +
                     SUBSTR (acct.acct, cnt-srt[5],  1) + 
                     SUBSTR (acct.acct, cnt-srt[6],  1) +
                     SUBSTR (acct.acct, cnt-srt[7],  1) + 
                     SUBSTR (acct.acct, cnt-srt[8],  1) +
                     SUBSTR (acct.acct, cnt-srt[9],  1) + 
                     SUBSTR (acct.acct, cnt-srt[10], 1) +
                     SUBSTR (acct.acct, cnt-srt[11], 1)
                   )
         mm       = IF curnum - ii GT num
                     THEN num
                     ELSE curnum - ii + 1
      NO-ERROR.
                        /* Если текущее значение счетчика больше
                        ** текущего количества найденных счетов, то... */
      IF curnum GT ii
      THEN DO kk = 1 TO IF mm EQ 1 THEN 1 ELSE mm - 1 :
                        /* Формирование номера счета. */
         st =  STRING (inbal-acct)  + "," + 
               incur                + "," + 
               iBranch              + "," + 
               STRING (DECIMAL(ii), FILL("9", toklen[{&TOK_CNT_IDX}]))
         NO-ERROR.
         IF AcctChckRsrv (st)
            THEN jj = jj + 1.
                        /* Если количество зарезервированных счетов превысило
                        ** установленное, то прекращаем формировать резерв. */
         IF jj EQ num
            THEN LEAVE SRCH_BLCK.
         IF kk NE mm
            THEN ii = ii + 1.
      END.
      ELSE IF curnum LT ii
         THEN ii = ii - 1.
   END.
                        /* Увеличиваем счетчик требуемых счетов. */
   mm = jj + 1.
                        /* Если количество зарезервированных счетов
                        ** недостаточно, то... */
   IF jj LT num THEN 
   GEN_NUMBER:
   DO kk = mm TO num:
      ii = ii + 1.
                        /* Проверяем что размерность счетчика соответсвует
                        ** полученному номеру. */
      IF LENGTH(STRING(ii)) GT toklen[{&TOK_CNT_IDX}]
         THEN LEAVE GEN_NUMBER.
                        /* Зафиксирована проблема, поставлен временный обход с
                        ** использованием NO-ERROR,
                        ** при длине счетчика равным 6 в toklen [6] лежит 2. */
      st =  STRING (inbal-acct)  + "," +
            incur                + "," + 
            iBranch              + "," +
            STRING (DECIMAL(ii), FILL("9", toklen[{&TOK_CNT_IDX}]))
      NO-ERROR.
                        /* Если резервирование не удалось,
                        ** то увеличиваем верхний предел. */
      IF NOT AcctChckRsrv (st)
         THEN num = num + 1.
   END.
   RETURN.
END PROCEDURE.


/*prosignPCzG1CM5jEHUjmiMGGkNrw*/
/* --- cntac100.i was humbly modified by (c)blodd converter v.1.09 on 11/10/2016 7:23am --- */
