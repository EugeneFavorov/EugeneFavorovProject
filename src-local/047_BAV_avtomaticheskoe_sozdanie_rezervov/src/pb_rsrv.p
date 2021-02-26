/**
Авторские права принадлежат: ПАО Плюс банк
Базируется:     g-rsrv.p (130d)
Основание:      ОСРКО.428
Что делает:     Начисляет резервы по счетам "за РКО", формирует отчет XL и отправляет его по почте.
Как работает:
Параметры:      дата опердня, транзакция
Место запуска:  Транзакция rsrv_pr51 - Планировщик
Создан:         20.12.2016 Борисов А.В.
*/

{globals.i}
{sh-defs.i}
{intrface.get rsrv}
{intrface.get date}     /* Инструменты для работы с датами. */
{intrface.get tmess}
{intrface.get xclass}
{def-wf.i NEW}
/* Вставка Плюс банк */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{pb_logit.i}
/* Конец вставки Плюс банк */

DEFINE INPUT PARAMETER in-op-date LIKE op.op-date.
DEFINE INPUT PARAMETER oprid      AS   RECID.
/* Вставка Плюс банк */
IF HolidayRu(in-op-date) THEN RETURN.
/* Конец вставки Плюс банк */

DEFINE VARIABLE vCnt         AS INT64   NO-UNDO.
DEFINE VARIABLE vTotal       AS INT64   NO-UNDO. /* общее количество обрабатываемых счетов */
DEFINE VARIABLE vTotalAll    AS INT64   NO-UNDO. /* общее количество выбранных счетов */
DEFINE VARIABLE vNtWrkCnt    AS INT64   NO-UNDO. /* кол-во счетов по которым не требуется урегулирование */
DEFINE VARIABLE vErrCnt      AS INT64   NO-UNDO. /* кол-во счетов по которым урегулирование прошло с ошибками */
DEFINE VARIABLE vOk          AS LOGICAL.
DEFINE VARIABLE vAcctOpen    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vAcct        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vLog         AS CHARACTER NO-UNDO.
/* Вставка Плюс банк */
DEFINE VARIABLE cLog         AS CHARACTER NO-UNDO.
/* Конец вставки Плюс банк */
DEFINE VARIABLE vOkCnt       AS INT64   NO-UNDO. /* количество счетов по которым урегулирование 
                                                      прошло успешно */
DEFINE VARIABLE vAcctClass   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vFRsrv       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vCRsrv       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCont-Code   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct-Risk   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOldAutoNum  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vOpD         AS DATE.
DEFINE VARIABLE vAftProc     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAcctDetails AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUserID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vHandleProc  AS HANDLE NO-UNDO.
DEFINE VARIABLE nprrsrv      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vProcParam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDoc-num     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vSomeBranch  AS LOGICAL   NO-UNDO.    /* Одно подразделение для сч.резерва и риска */
DEFINE VARIABLE mAutoMode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate446P    AS DATE      NO-UNDO.
DEFINE VARIABLE mOldPickValue LIKE pick-value NO-UNDO.
/* Вставка Плюс банк */
DEFINE VARIABLE cMail        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAcct        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTmp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE nTmp         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE I            AS INT64     NO-UNDO.
/* Конец вставки Плюс банк */

DEFINE BUFFER bAcct FOR acct.
DEFINE BUFFER bf_tProv FOR tProv.
DEFINE STREAM sLog.

DEFINE TEMP-TABLE tt-acct-revexp NO-UNDO
   FIELD fNom         LIKE tProv.fNom
   FIELD acct-reserve LIKE acct.acct
   FIELD acct-revenue LIKE acct.acct
   FIELD acct-expense LIKE acct.acct

   INDEX fNom
      fNom
.

{details.def}
/* Вставка Плюс банк */
FIND FIRST op-kind WHERE RECID(op-kind) = oprid NO-LOCK NO-ERROR.
RUN Init-SysMes (op-kind.op-kind, "", "").      /* Учесть ДР СС_ВыводНаЭкран */
/* Конец вставки Плюс банк */

{chkblock.i
  &surr=string(in-op-date)
  &msg="Вы не имеете права работать в заблокированном операционном дне!"
  &action= "return."
}

/* Удалено Плюс банк
FIND FIRST op-kind WHERE RECID(op-kind) = oprid NO-LOCK NO-ERROR.
Конец удаленного фрагмента Плюс банк */

ASSIGN
   vAcctOpen    = GetXAttrValue("op-kind",op-kind.op-kind,"ОткрСч137П") =  "Да"
   vAcctClass   = GetXAttrValue("op-kind",op-kind.op-kind,"ACCT-CLASS-CODE")
   nprrsrv      = GetXattrValueEx("op-kind",op-kind.op-kind,"nprrsrv","")
   vSomeBranch  = GetXattrValueEx("op-kind", op-kind.op-kind, "ОткрСчПодрРиск",
                                  GetXattrInit("base-op-rsrv", "ОткрСчПодрРиск")) =  "Да"
/* Вставка Плюс банк */
   cMail        = GetXAttrValue("op-kind",op-kind.op-kind,"e-mail")     /* адреса рассылки отчета XL */
/* Конец вставки Плюс банк */
.

IF NOT {assigned vAcctClass} THEN vAcctClass = "acctb".

vAftProc = GetXattrValue("op-kind",op-kind.op-kind,"aft-fill-prog").

RUN SetAfterProcedure IN h_rsrv(vAftProc).

ASSIGN
   end-date  = LastWorkDay(in-op-date - 1)
   mDate446P = DATE(FGetSetting("ЦБ446П", "Дата446П", "01/01/2016"))
NO-ERROR.

/* загрузка персистентной библиотеки */
RUN "g-rsrlib.p" PERSISTENT SET vHandleProc NO-ERROR.

mAutoMode = GetXAttrValue("op-kind",op-kind.op-kind,"АвтРежим").
IF mAutoMode <> "Да" THEN
DO:
   bl:
   REPEAT ON ERROR  UNDO,RETURN
          ON ENDKEY UNDO,RETURN:
      {getdate.i
         &DateLabel = "Предыд.урегулир."
         &DateHelp  = "Дата предыдущего урегулирования резерва (F1 - календарь)"
         &noinit    = YES
      }
      IF end-date >= in-op-date THEN DO:
         RUN Fill-sysmes IN h_tmess ("","","0","Дата предыдущего урегулирования должна быть меньше даты опердня!").
         NEXT bl.
      END.
      ELSE
         LEAVE bl.
   END.
END.

/* запуск предварительной процедуры */
IF nprrsrv <> ""
THEN DO:
   /* Если параметры не заданы на ДР, то будет передаваться дата
   ** иначе параметры указанные через "((".
   ** Необходимо для корректного определения остатоков в процедуре setacdrv.p */
   vProcParam = SUBSTR(nprrsrv, (IF INDEX(nprrsrv, "((") =  0
                                 THEN ?
                                 ELSE INDEX(nprrsrv, "((") + 2)).

   RUN VALUE(SUBSTR(nprrsrv, 1, IF INDEX(nprrsrv, "((") =  0
                                THEN 255
                                ELSE INDEX(nprrsrv, "((") - 1))
            (IF vProcParam =  ? THEN STRING(in-op-date)
                                ELSE vProcParam ).
   
   IF RETURN-VALUE <> "yes" THEN 
   DO:
      {intrface.del}
      RETURN.
   END.
END.

IF mAutoMode =  "Да" THEN
DO TRANSACTION:
   RUN FillTProv IN h_rsrv ("Резерв137П",
                            in-op-date,
                            end-date,
                            GetXAttrValue("op-kind",op-kind.op-kind,"МаскаСчета"),
                            GetXAttrValue("op-kind",op-kind.op-kind,"Ответственный") = "Да",
                            GetXAttrValue("op-kind",op-kind.op-kind,"НазнСч"),
                            YES, 
                            OUTPUT TABLE tProv).
END.
ELSE
DO TRANSACTION:
   RUN brwrsrv.p(in-op-date,
                 end-date,
                 YES,
                 GetXAttrValue("op-kind",op-kind.op-kind,"МаскаСчета"),
                 GetXAttrValue("op-kind",op-kind.op-kind,"Ответственный") = "Да",
                 GetXAttrValue("op-kind",op-kind.op-kind,"НазнСч"),
                 OUTPUT TABLE tProv,4).
END.
/* Вставка Плюс банк */
/* Оставить только счета "за РКО", у которых есть связь acct47423 */
FOR EACH tProv:
    IF (GetLinks("acctb", tProv.acct-risk + "," + tProv.currency, "", "acct47423", "!", TODAY) EQ "")    /* нет связи с р/с */
    THEN DELETE tProv.
END.
/* Конец вставки Плюс банк */
IF KEYFUNCTION(LASTKEY) =  "END-ERROR" THEN DO:
   DELETE PROCEDURE vHandleProc NO-ERROR.
   {intrface.del}
   RETURN.
END.

/* Вставка Плюс банк */
IF mAutoMode NE "Да" THEN DO:
/* Конец вставки Плюс банк */
    {logset.i}

    vLog = op-kind.op-kind + ".log".
    OUTPUT STREAM sLog TO VALUE(vLog).
    PUT STREAM sLog UNFORMATTED
       "ПРОТОКОЛ ОПЕРАЦИИ УРЕГУЛИРОВАНИЯ РЕЗЕРВА (137-П).~n".
    PUT STREAM sLog UNFORMATTED STRING(TODAY,"99/99/9999") + " " +
       STRING(TIME,"HH:MM:SS") "~n".
    PUT STREAM sLog UNFORMATTED "~n".

/* Вставка Плюс банк */
END.
ELSE DO:    /* Открываем протокол в XL */
    vLog = STRING(YEAR(in-op-date)) + STRING(MONTH(in-op-date), "99") + STRING(DAY(in-op-date), "99").
    cLog = "/home2/bis/quit41d/log/rsrv/" + op-kind.op-kind + "-" + vLog + ".txt".
    vLog = "/home2/bis/quit41d/log/rsrv/" + op-kind.op-kind + "-" + vLog + "-" + shFilial + ".xml".
    OUTPUT STREAM sLog TO VALUE(vLog).

    IF (shFilial EQ "0500")
    THEN PUT STREAM sLog UNFORMATTED XLHead("rsrv", "CDCCCCNNC", "60,70,65,213,213,213,121,125,130").
    ELSE PUT STREAM sLog UNFORMATTED XLHead("rsrv", "DCCCCNNC", "70,65,213,213,213,121,125,130").

    cXL = (IF (shFilial EQ "0500") THEN XLCellHead("Номер группы",0,0,0) ELSE "")
        + XLCellHead("Дата проводки",0,0,0)
        + XLCellHead("Код клиента",0,0,0)
        + XLCellHead("Номер р/счета",0,0,0)
        + XLCellHead("Номер счета 47423*",0,0,0)
        + XLCellHead("Номер счета 47425*",0,0,0)
        + XLCellHead("% Резервирования",0,0,0)
        + XLCellHead("Сумма созданной проводки",0,0,0)
        + XLCellHead("Примечание",0,0,0)
        .
    PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
    RUN LogIt("╘шышры: " + shFilial, cLog).
    RUN LogIt((IF (shFilial EQ "0500") THEN "├Ёєяяр  " ELSE "")
            + "─рЄр        ╩ышхэЄ     ═юьхЁ Ё/ёўхЄр         ═юьхЁ ёўхЄр 47423*    ═юьхЁ ёўхЄр 47425*    % ╨хч   ╤єььр яЁютюфъш  ╧Ёшьхўрэшх", cLog).
END.
/* Конец вставки Плюс банк */

IF LASTKEY <> 27 THEN
DO:
   TR:
   DO TRANSACTION 
      ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:
      
      FOR EACH tProv NO-LOCK:
               
         IF    tProv.rsrv-amt > 0     
            OR (    tProv.rsrv-amt = 0     
                AND tProv.acct-rsrv-bal > 0) THEN
            vTotal = vTotal + 1.
         vTotalAll = vTotalAll + 1.
      END.

/* Вставка Плюс банк */
      IF mAutoMode NE "Да" THEN DO:
/* Конец вставки Плюс банк */
          PUT STREAM sLog UNFORMATTED
             "ИНФОРМАЦИЯ         ВСЕГО ВЫБРАНО СЧЕТОВ       " vTotalAll SKIP.
          PUT STREAM sLog UNFORMATTED
             "ИНФОРМАЦИЯ         ВСЕГО СЧЕТОВ ДЛЯ ОБРАБОТКИ " vTotal    SKIP(1).

          {bar-beg2.i 
             &BarTotal = "vTotal" 
             &BarMessage = """Обработка данных..."""}
/* Вставка Плюс банк */
      END.
/* Конец вставки Плюс банк */

      mOldPickValue = pick-value.

      LOOP:
      FOR EACH tProv WHERE
               tProv.rsrv-amt > 0     
            OR (    tProv.rsrv-amt = 0     
                AND tProv.acct-rsrv-bal > 0)
      ON ERROR UNDO LOOP, NEXT LOOP 
      ON STOP UNDO LOOP,NEXT LOOP:

/* Вставка Плюс банк */
         IF mAutoMode NE "Да" THEN DO:
/* Конец вставки Плюс банк */
         vCnt = vCnt + 1.
         {bar2.i 
            &BarPointer = "vCnt" 
            &BarBreak = "UNDO TR,LEAVE TR."}
         PUT STREAM sLog UNFORMATTED
            "ИНФОРМАЦИЯ         ОБРАБАТЫВАЕТСЯ СЧЕТ № " tProv.acct-risk SKIP.
/* Вставка Плюс банк */
         END.
/* Конец вставки Плюс банк */

         IF     tProv.acct-reserve = "" 
            AND vAcctOpen THEN
         DO:
/* Вставка Плюс банк */
            IF mAutoMode NE "Да" THEN
/* Конец вставки Плюс банк */
            PUT STREAM sLog UNFORMATTED
               "ИНФОРМАЦИЯ         ДЛЯ СЧЕТА РИСКА ОТКРЫВАЕТСЯ СЧЕТ РЕЗЕРВА" SKIP.

            /* Определение ДР AcctReservName должно быть тут, т.к. иначе первый найденный
            ** проставится всем созданным счетам. А так, каждый раз для каждого счета будет
            ** вычислен на основании парсера. */
            ASSIGN
               vAcctDetails = GetXattrValue("op-kind",op-kind.op-kind,"AcctReservName").
               mUserID = GetXattrValue("op-kind",op-kind.op-kind,"AcctReservUserID").

            IF {assigned mUserID} AND mUserID <> "СчетРиска" THEN DO:
               FIND FIRST _user WHERE _user._userid =  mUserID NO-LOCK NO-ERROR.
               IF AVAIL(_user) THEN DO:
                  IF GetXattrValue("_user",mUserID,"Blocked") =  "Блокирован"
                  THEN mUserID = "".
               END.
               ELSE mUserID = "".
            END.

            /* Формируем значение реквизита "НаименованиеСчета" */
            RUN SetCurrentRec IN h_rsrv (tProv.fNom,TABLE tProv).
            RUN ProcessDetails (?, INPUT-OUTPUT vAcctDetails).
            RUN CrAcctRsrv IN h_rsrv (tProv.fNom,
                                      vAcctClass,
                                      vAcctDetails,
                                      vSomeBranch,
                                      mUserID,
                                      TABLE tProv,
                                      OUTPUT vAcct).
            IF vAcct = ? THEN
            DO:
/* Вставка Плюс банк */
               IF mAutoMode NE "Да" THEN
/* Конец вставки Плюс банк */
               PUT STREAM sLog UNFORMATTED
                  "ОШИБКА             ОШИБКА ОТКРЫТИЯ СЧЕТА РЕЗЕРВА" SKIP.
/* Вставка Плюс банк */
               ELSE DO:
                   cXL = (IF (shFilial EQ "0500") THEN XLEmptyCell() ELSE "")
                       + XLDateCell(in-op-date)
                       + XLEmptyCells(2)
                       + XLCell(Tprov.acct-risk)
                       + XLEmptyCells(3)
                       + XLCell("Ошибка открытия счета резерва")
                       .
                   PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
                   RUN LogIt((IF (shFilial EQ "0500") THEN "        " ELSE "")
                           + STRING(in-op-date, "99.99.9999")  + STRING(" ", "x(35)")
                           + SUBSTRING(Tprov.acct-risk, 1, 20) + STRING(" ", "x(48)")
                           + "╬°шсър юЄъЁ√Єш  ёўхЄр ЁхчхЁтр", cLog).
               END.
/* Конец вставки Плюс банк */
               vErrCnt = vErrCnt + 1.
               NEXT.
            END.
            tProv.acct-reserve = vAcct.
            RUN SaveRsrvParameters IN h_rsrv (tProv.fNom,in-op-date,TABLE tProv).
/* Вставка Плюс банк */
            IF mAutoMode NE "Да" THEN
/* Конец вставки Плюс банк */
            PUT STREAM sLog UNFORMATTED
               "ИНФОРМАЦИЯ         ОТКРЫТ СЧЕТ РЕЗЕРВА № " tProv.acct-reserve SKIP.
/* Вставка Плюс банк */
            ELSE DO:
                cXL = (IF (shFilial EQ "0500") THEN XLEmptyCell() ELSE "")
                    + XLDateCell(in-op-date)
                    + XLEmptyCells(2)
                    + XLCell(Tprov.acct-risk)
                    + XLCell(Tprov.acct-reserve)
                    + XLEmptyCells(2)
                    + XLCell("Открыт счет резерва")
                    .
                PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
                RUN LogIt((IF (shFilial EQ "0500") THEN "        " ELSE "")
                        + STRING(in-op-date, "99.99.9999")     + STRING(" ", "x(35)")
                        + SUBSTRING(Tprov.acct-risk,    1, 20) + "  "
                        + SUBSTRING(Tprov.acct-reserve, 1, 20) + STRING(" ", "x(26)")
                        + "╬ЄъЁ√Є ёўхЄ ЁхчхЁтр", cLog).
            END.
/* Конец вставки Плюс банк */
         END.
            /* 
               Механизм группировки по счетам при создании сводных документов
               будет зависеть от того, вступило ли в силу положение 446-П.
               До даты вступления в силу группировка идёт по старому: только
               по счетам резерва. После вступления в силу - по счетам резерва
               и по счетам доходов-расходов, которые теперь будут зависеть от
               типа клиента.
            */
            CREATE tt-acct-revexp.
            ASSIGN
               tt-acct-revexp.fNom         = tProv.fNom
               tt-acct-revexp.acct-reserve = tProv.acct-reserve
            .
            IF in-op-date >= mDate446P THEN
               RUN GetAcctRevExp446P IN THIS-PROCEDURE
                  (vHandleProc,
                   OUTPUT tt-acct-revexp.acct-revenue,
                   OUTPUT tt-acct-revexp.acct-expense).
            RELEASE tt-acct-revexp.


         ASSIGN 
            vCRsrv = tProv.rsrv-amt
            vFRsrv = 0
            vOpD   = DYNAMIC-FUNCTION("GetOpDate" IN h_rsrv)
         .

         {find-act.i
            &bact = bAcct
            &acct = tProv.acct-reserve
            &curr = ""
         }   

         IF AVAIL bAcct THEN
         DO:
            IF     bAcct.open-date <= vOpD 
               AND (   bAcct.close-date =  ? 
                    OR bAcct.close-date >  vOpD ) THEN DO:
   
               RUN acct-pos IN h_base (bAcct.acct,
                                       bAcct.currency,
                                       vOpD,
                                       vOpD,
                                       CHR(251)
               ).
               vFRsrv = ABS(sh-bal).
            END.
         END. 
         ELSE DO:
/* Вставка Плюс банк */
            IF mAutoMode NE "Да" THEN
/* Конец вставки Плюс банк */
            PUT STREAM sLog UNFORMATTED
               "ОШИБКА             НЕ НАЙДЕН СЧЕТА РЕЗЕРВА " tProv.acct-reserve SKIP.

            vErrCnt = vErrCnt + 1.   
            NEXT.
         END.

         IF vCRsrv = vFRsrv THEN
         DO:
/* Вставка Плюс банк */
            IF mAutoMode NE "Да" THEN
/* Конец вставки Плюс банк */
            PUT STREAM sLog UNFORMATTED
              "ИНФОРМАЦИЯ         ДЛЯ СЧЕТА РИСКА " tProv.acct-risk " УРЕГУЛИРОВАНИЯ НЕ ТРЕБУЕТСЯ." SKIP.
/* Вставка Плюс банк */
            ELSE DO:
                cXL = (IF (shFilial EQ "0500") THEN XLEmptyCell() ELSE "")
                    + XLDateCell(in-op-date)
                    + XLEmptyCells(2)
                    + XLCell(Tprov.acct-risk)
                    + XLCell(Tprov.acct-reserve)
                    + XLNumCell(Tprov.pers-rsrv)
                    + XLEmptyCell()
                    + XLCell("Урегулирования не требуется")
                    .
                PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
                RUN LogIt((IF (shFilial EQ "0500") THEN "        " ELSE "")
                        + STRING(in-op-date, "99.99.9999")     + STRING(" ", "x(35)")
                        + SUBSTRING(Tprov.acct-risk,    1, 20) + "  "
                        + SUBSTRING(Tprov.acct-reserve, 1, 20) + "  "
                        + STRING(Tprov.pers-rsrv, ">>9.99")    + STRING(" ", "x(18)")
                        + "╙ЁхуєышЁютрэш  эх ЄЁхсєхЄё ", cLog).
            END.
/* Конец вставки Плюс банк */
            vNtWrkCnt = vNtWrkCnt + 1.
            NEXT.
         END.


/* Вставка Плюс банк */
         IF mAutoMode NE "Да" THEN
/* Конец вставки Плюс банк */
         PUT STREAM sLog UNFORMATTED
            "ИНФОРМАЦИЯ         СЧЕТ РИСКА № " tprov.acct-risk " УСПЕШНО ОБРАБОТАН" SKIP(1).
         vOkCnt = vOkCnt + 1.
      END.

      ASSIGN
         vOk        = YES
         pick-value = mOldPickValue
      .
   END.  /* End of TR BLOCK */
END.

/* Замена Плюс банк
FIND LAST tProv NO-LOCK NO-ERROR.
vCnt = tProv.fNom + 1.
*/
FOR LAST tProv:
    vCnt = tProv.fNom + 1.
END.
/* Конец замены Плюс банк */
FOR EACH tProv
   WHERE
      tProv.rsrv-amt >  0        OR
      (tProv.rsrv-amt      =  0  AND
       tProv.acct-rsrv-bal >  0)
NO-LOCK,
FIRST tt-acct-revexp WHERE
   tt-acct-revexp.fNom = tProv.fNom
NO-LOCK
BREAK BY tt-acct-revexp.acct-reserve
      BY tt-acct-revexp.acct-revenue
      BY tt-acct-revexp.acct-expense:
   {find-act.i
      &acct = tProv.acct-reserve
      &curr = ""
   }   
   IF NOT AVAIL acct           OR
      tProv.acct-reserve =  ?  OR
      tProv.acct-reserve =  ""
   THEN DO:
      DELETE tProv.
      NEXT.
   END.

   IF FIRST-OF(tt-acct-revexp.acct-reserve) THEN
   ASSIGN
      mAcct-Risk = ""
      mCont-code = "0"
   .

   IF mCont-code      =  "0"  AND
      tProv.cont-code <> ?    AND
      tProv.cont-code <> ""
   THEN
   mCont-code = tProv.cont-code.

   IF NUM-ENTRIES(mAcct-Risk) <= 50 THEN
   {additem.i mAcct-Risk tProv.acct-risk}

   ACCUMULATE tProv.rsrv-amt (TOTAL BY tt-acct-revexp.acct-reserve).

   IF LAST-OF(tt-acct-revexp.acct-reserve) THEN DO:
      CREATE bf_tProv.
      bf_tProv.fNom = vCnt.
      BUFFER-COPY tProv
           EXCEPT fNom
                  rsrv-amt
                  cont-code
           TO bf_tProv.
      ASSIGN
         bf_tProv.rsrv-amt       = ACCUM TOTAL BY tt-acct-revexp.acct-reserve tProv.rsrv-amt
         bf_tProv.cont-code      = mCont-code
         bf_tProv.lst-acct-risk  = mAcct-Risk
         mAcct-Risk              = ""
         mCont-code              = "0"
         vCnt                    = vCnt + 1
      .
   END.
   DELETE tProv.
END.

FOR EACH tProv WHERE
    tProv.rsrv-amt >  0     OR
   (tProv.rsrv-amt =  0     AND
    tProv.acct-rsrv-bal >  0)
NO-LOCK:
   RUN SaveRsrvParameters IN h_rsrv (tProv.fNom,in-op-date,TABLE tProv).
   RUN SetCurrentRec IN h_rsrv (tProv.fNom,TABLE tProv).
   ASSIGN
      vOldAutoNum = autonumdoc
      autonumdoc  = YES
      mDoc-num    = ?
   .
   RUN g-rsrv2.p(in-op-date, 
                 oprid,
                 OUTPUT mDoc-num).
   autonumdoc = vOldAutoNum.

/* Вставка Плюс банк */
    IF mAutoMode NE "Да" THEN DO:
/* Конец вставки Плюс банк */
       IF mDoc-num <> ? THEN
          PUT STREAM sLog UNFORMATTED
             "ИНФОРМАЦИЯ         ДЛЯ СЧЕТА РИСКА № "
             tProv.acct-risk 
             " СОЗДАН ДОКУМЕНТ № "
             mDoc-num 
          SKIP.
       ELSE
          PUT STREAM sLog UNFORMATTED
             "ПРЕДУПРЕЖДЕНИЕ     ДЛЯ СЧЕТА РИСКА № "
             Tprov.acct-risk 
             " ДОКУМЕНТ НЕ СОЗДАН"
          SKIP.
    END.
/* Вставка Плюс банк */
    ELSE DO:    /* Заполнение протокола в XL */
        {find-act.i
            &bact = bAcct
            &acct = tProv.acct-risk
            &curr = tProv.currency
        }
        vAcct = GetLinks("acctb", tProv.acct-risk + "," + tProv.currency, "", "acct47423", "!", TODAY).
        IF (vAcct NE "")
        THEN DO:
            DO I = 1 TO NUM-ENTRIES(vAcct, "!"):
                cTmp = ENTRY(I, vAcct, "!").
                IF (ENTRY(2, cTmp) EQ "")
                THEN DO:
                    vAcct = cTmp.
                    LEAVE.
                END.
            END.

            IF (NUM-ENTRIES(vAcct, "!") NE 1)
            THEN vAcct = ENTRY(1, vAcct, "!").
        END.

        IF (mDoc-num NE ?)
        THEN
            FOR FIRST op
                WHERE (op.op-date   EQ in-op-date)
                  AND (op.op-kind   EQ op-kind.op-kind)
                  AND (op.doc-num   EQ mDoc-num)
                NO-LOCK,
            FIRST op-entry OF op
                NO-LOCK:

                nTmp = IF (tProv.currency EQ "") THEN op-entry.amt-rub ELSE op-entry.amt-cur.
            END.

        cTmp = IF (vAcct NE "") THEN GetXAttrValue("acct", vAcct, "groupOABS") ELSE "".
        cXL = (IF (shFilial EQ "0500") THEN XLCell(cTmp) ELSE "")
            + XLDateCell(in-op-date)
            + XLCell(bAcct.cust-cat + "_" + STRING(bAcct.cust-id))
            + XLCell(ENTRY(1, vAcct))
            + XLCell(Tprov.acct-risk)
            + XLCell(Tprov.acct-reserve)
            + XLNumCell(Tprov.pers-rsrv)
            + (IF (mDoc-num NE ?) THEN XLNumCell(nTmp) ELSE XLEmptyCell())
            + XLCell(IF (mDoc-num EQ ?) THEN "Документ не создан" ELSE "")
            .
        PUT STREAM sLog UNFORMATTED XLRow(0) cXL XLRowEnd().
        RUN LogIt((IF (shFilial EQ "0500") THEN STRING(cTmp, "x(8)") ELSE "")
                + STRING(in-op-date, "99.99.9999")     + "  "
                + CODEPAGE-CONVERT(STRING(bAcct.cust-cat + "_" + STRING(bAcct.cust-id), "x(9)"),"IBM866","1251") + "  "
                + SUBSTRING(ENTRY(1, vAcct),    1, 20) + "  "
                + SUBSTRING(Tprov.acct-risk,    1, 20) + "  "
                + SUBSTRING(Tprov.acct-reserve, 1, 20) + "  "
                + STRING(Tprov.pers-rsrv, ">>9.99")    + "  "
                + (IF (mDoc-num NE ?) THEN STRING(nTmp, ">>>,>>>,>>9.99") ELSE "              ") + "  "
                + (IF (mDoc-num EQ ?) THEN "─юъєьхэЄ эх ёючфрэ" ELSE ""), cLog).
    END.
END.

IF mAutoMode NE "Да" THEN DO:
/* Конец вставки Плюс банк */
    IF vOk <> YES THEN
      PUT STREAM sLog UNFORMATTED
       "ОШИБКА             ТРАНЗАКЦИЯ <" op-kind.op-kind
       "> ОТМЕНЕНА ПОЛЬЗОВАТЕЛЕМ" SKIP.
    ELSE
      PUT STREAM sLog UNFORMATTED "ИНФОРМАЦИЯ         ОБРАБОТАНО СЧЕТОВ - "
         vOkCnt + vErrCnt + vNtWrkCnt " ИЗ НИХ С ОШИБКАМИ - " vErrCnt 
         ", УРЕГУЛИРОВАНИЕ НЕ ТРЕБУЕТСЯ - " vNtWrkCnt SKIP.

    {logreset.i}

    MESSAGE "".
    PAUSE 0.

    {setdest.i &STREAM   = "STREAM sLog "
               &APPEND   = " APPEND "
               &FILENAME = vLog
    }
/* Удалено Плюс банк
IF mAutoMode <> "Да" THEN
DO:
Конец удаленного фрагмента Плюс банк */
   {preview.i &STREAM= "STREAM sLog " &FILENAME = vLog}
END.
/* Вставка Плюс банк */
ELSE DO:    /* Отправка протокола по почте */
    PUT STREAM sLog UNFORMATTED XLEnd().
    OUTPUT STREAM sLog CLOSE.
    RUN LogIt(" ", cLog).
    RUN LogIt(" ", cLog).
    RUN pb_mail.p (cMail, "RESERV " + shFilial, "", vLog + "," + cLog).
END.
/* Конец вставки Плюс банк */

DELETE PROCEDURE vHandleProc NO-ERROR.
{intrface.del}

/*------------------------------------------------------------------------------
  Purpose:     Callback - процедура. Логирование сообщений от бизнес служб
  Parameters:  pMess  - текст сообщения
               pEvnt  - код сообщения
               pCont  - признак возможности продолжения выполнения
               opFlag - код возврата
                  0       - продолжить обработку
                  не ноль - прервать обработку
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE LogMessage:
  DEFINE INPUT  PARAMETER pMess AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pEvnt AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pCont AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER opFlag AS INT64    NO-UNDO.

  IF pCont <> "YES" THEN
  DO:
    PUT STREAM sLog UNFORMATTED "ОШИБКА             " pMess SKIP.
    opFlag = 1.
  END. ELSE
  DO:
    PUT STREAM sLog UNFORMATTED "ПРЕДУПРЕЖДЕНИЕ     " pMess SKIP.
    opFlag = 0.
  END.
END PROCEDURE.

/* Вычисление счетов дохода и расхода для установленной tProv согласно 446-П */
PROCEDURE GetAcctRevExp446P PRIVATE:
   DEFINE INPUT  PARAMETER iHRsrLib AS   HANDLE                      NO-UNDO.
   DEFINE OUTPUT PARAMETER oAcctRev LIKE tt-acct-revexp.acct-revenue NO-UNDO.
   DEFINE OUTPUT PARAMETER oAcctExp LIKE tt-acct-revexp.acct-expense NO-UNDO.

   IF VALID-HANDLE(iHRsrLib) THEN DO:
      RUN VALUE("СчДохРасх") IN iHRsrLib (?, ?, ?, "[Д]").
      oAcctRev = pick-value.
      RUN VALUE("СчДохРасх") IN iHRsrLib (?, ?, ?, "[Р]").
      oAcctExp = pick-value.
   END.
END PROCEDURE.
/* $LINTFILE='g-rsrv.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:40.737+03:00' */
/*prosignaF8FvsaUZFIGE+Z6y3CiQA*/