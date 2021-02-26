/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2019 АО "Банковские информационные системы"
     Filename: rep311acct.p
      Comment: Отчёт по отправленным/полученным сообщениям по 311-П об открытии/закрытии счета.
   Parameters: нет
         Uses:
      Used by:
      Created: 21.10.2016 Sami
     Modified:
*/

/* Временная таблица для вывода и сортировки */
DEFINE TEMP-TABLE tt311open NO-UNDO
    FIELD pack_id      AS INT64
    FIELD pack_name    AS CHARACTER
    FIELD direct       AS CHARACTER /* open/close   */
    FIELD acct         AS CHARACTER
    FIELD cust_name    AS CHARACTER
    FIELD acct_open    AS DATE
    FIELD acct_close   AS DATE
    FIELD mess_name    AS CHARACTER
    FIELD mess_date    AS CHARACTER
    FIELD mess_time    AS INT64
    FIELD ni_date      AS CHARACTER /* НИ дата      */
    FIELD ni_state     AS CHARACTER /* НИ статус    */
    FIELD pfr_date     AS CHARACTER /* ПФР дата     */
    FIELD pfr_state    AS CHARACTER /* ПФР статус   */
    FIELD fss_date     AS CHARACTER /* ФСС дата     */
    FIELD fss_state    AS CHARACTER /* ФСС статус   */
    FIELD facct        AS CHARACTER
    FIELD fcurrency    AS CHARACTER
.

/* Основные буферы */
DEF BUFFER acct          FOR acct.
DEF BUFFER cust-corp     FOR cust-corp.
DEF BUFFER PackObject    FOR PackObject.
DEF BUFFER Packet        FOR Packet.
DEF BUFFER Seance        FOR Seance.
DEF BUFFER FileExch      FOR FileExch.

/* Буферы импорта */
DEF BUFFER impPacket     FOR Packet.
DEF BUFFER impSeance     FOR Seance.
DEF BUFFER impFileExch   FOR FileExch.

/* Переменные для расчёта */
DEFINE VARIABLE mUPRecid  AS INT64     NO-UNDO.
DEFINE VARIABLE mParams   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam3   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam4   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam5   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam6   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam7   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam8   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLOk      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mCName1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCName2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCFake    AS CHARACTER NO-UNDO.

{globals.i}
{intrface.get prnvd}

{intrface.get xclass}
{intrface.get xobj}
{intrface.get loan}
/* Вставка Плюс банк */
{intrface.get netw}     /* Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
/* Конец вставки Плюс банк */

/*--------- Функция определяющая значение параметра по имени ----------*/
FUNCTION GetParamByName RETURNS CHAR PRIVATE (INPUT iText AS CHAR,   /* Текст с параметрами       */
                                      INPUT iName AS CHAR,   /* Название тега             */
                                      INPUT iSym1 AS CHAR,   /* Разделить после строки    */
                                      INPUT iSym2 AS CHAR):  /* Разделитель внутри строки */

   DEFINE VARIABLE vIn AS INT64 NO-UNDO.

   DO vIn = 1 TO NUM-ENTRIES(iText, iSym1):
      IF ENTRY(1, entry(vIn, iText, iSym1), iSym2) = iName THEN
         RETURN ENTRY(2, ENTRY(vIn, iText, iSym1), iSym2).
   END.
   RETURN "".
END FUNCTION.
/*----------------- Считывание параметров -----------------*/
mUPRecid = INT64(GetSysConf("user-proc-id")).
FOR FIRST user-proc WHERE RECID(user-proc) = mUPRecid NO-LOCK:
   mParams = user-proc.Params.
END.
mParam1 = GetParamByName(mParams, "Маски", ";", "=").
mParam2 = GetParamByName(mParams, "НазнСч", ";", "=").
mParam3 = GetParamByName(mParams, "СообщОткр", ";", "=").
mParam4 = GetParamByName(mParams, "СообщЗакр", ";", "=").
mParam5 = GetParamByName(mParams, "КвитНИ_Пол", ";", "=").
mParam6 = GetParamByName(mParams, "КвитНИ_Отр", ";", "=").
mParam7 = GetParamByName(mParams, "КвитПФР", ";", "=").
mParam8 = GetParamByName(mParams, "КвитФСС", ";", "=").
/*---------------------------------------------------------*/

beg-date = DATE("01" + SUBSTRING(STRING(NOW), 3)).
end-date = DATE(NOW).
{getdates.i &noinit = YES}

RUN Insert_TTName IN THIS-PROCEDURE ("dateIn",  STRING(beg-date,"99.99.99")).
RUN Insert_TTName IN THIS-PROCEDURE ("dateOut", STRING(end-date,"99.99.99")).

/* --------------- Ищем данные по открытым счетам --------------- */
FOR EACH acct WHERE
         acct.open-date  >= beg-date AND
         acct.open-date  <= end-date AND
         acct.filial-id  = shfilial AND
         CAN-DO(mParam1,acct.acct) AND
         CAN-DO(mParam2,acct.contract)
NO-LOCK:

    mLOk = NO.

    FOR EACH PackObject WHERE
             PackObject.file-name = "acct" AND
             PackObject.Surrogate = acct.acct + "," + acct.currency NO-LOCK,
        EACH Packet WHERE
             Packet.PacketID = PackObject.PacketID NO-LOCK,
       /* Направление - Экспорт */
       FIRST Seance WHERE
             Seance.SeanceID      = Packet.SeanceID AND
             Seance.Direct        = "Экспорт" NO-LOCK,
       /* Связка для получения имени сообщения */
       FIRST FileExch WHERE
             FileExch.FileExchID  = Packet.FileExchID AND
             CAN-DO(mParam3,FileExch.name)
    NO-LOCK:

       CREATE tt311open.
       ASSIGN
          tt311open.direct     = "open"
          tt311open.acct       = STRING(acct.number)
          tt311open.acct_open  = acct.open-date
          tt311open.pack_id    = Packet.PacketID
          tt311open.pack_name  = FileExch.name
          tt311open.mess_name  = STRING(SUBSTRING(FileExch.name, 1, LENGTH(FileExch.name) - 4))
          tt311open.mess_date  = STRING(Packet.PackDate)
          tt311open.mess_time  = Packet.PackTime
          tt311open.facct      = acct.acct
          tt311open.fcurrency  = acct.currency
       .

       /*  Наименование клиента */
       RUN GetCustName IN h_base ( acct.cust-cat,  acct.cust-id,  "",
                             OUTPUT       mCName1,
                             OUTPUT       mCName2,
                             INPUT-OUTPUT mCFake ).

       tt311open.cust_name  = mCName1 + " " + mCName2.

       mLOk = YES.
    END.

    /* Если сообщения не найдено, счет в отчете должен быть */
    IF mLOk = NO
    THEN DO:

       /*  Наименование клиента */
       RUN GetCustName IN h_base ( acct.cust-cat,  acct.cust-id,  "",
                             OUTPUT       mCName1,
                             OUTPUT       mCName2,
                             INPUT-OUTPUT mCFake ).

       CREATE tt311open.
       ASSIGN
          tt311open.direct     = "open"
          tt311open.acct       = STRING(acct.number)
          tt311open.acct_open  = acct.open-date
          tt311open.cust_name  = mCName1 + " " + mCName2
          tt311open.facct      = acct.acct
          tt311open.fcurrency  = acct.currency
       .
    END.
END.

/* --------------- Ищем данные по закрытым счетам --------------- */
FOR EACH acct WHERE
         acct.close-date >= beg-date AND
         acct.close-date <= end-date AND
         acct.filial-id  =  shfilial AND
         CAN-DO(mParam1,acct.acct) AND
         CAN-DO(mParam2,acct.contract)
NO-LOCK:

    mLOk = NO.

    FOR EACH PackObject WHERE
             PackObject.file-name = "acct" AND
             PackObject.Surrogate = acct.acct + "," + acct.currency NO-LOCK,
        EACH Packet WHERE
             Packet.PacketID = PackObject.PacketID NO-LOCK,
       /* Направление - Экспорт */
       FIRST Seance WHERE
             Seance.SeanceID      = Packet.SeanceID AND
             Seance.Direct        = "Экспорт" NO-LOCK,
       /* Связка для получения имени сообщения */
       FIRST FileExch WHERE
             FileExch.FileExchID  = Packet.FileExchID AND
             CAN-DO(mParam4,FileExch.name)
    NO-LOCK:

       CREATE tt311open.
       ASSIGN
          tt311open.direct     = "close"
          tt311open.acct       = STRING(acct.number)
          tt311open.acct_open  = acct.open-date
          tt311open.acct_close = acct.close-date
          tt311open.pack_id    = Packet.PacketID
          tt311open.pack_name  = FileExch.name
          tt311open.mess_name  = STRING(SUBSTRING(FileExch.name, 1, LENGTH(FileExch.name) - 4))
          tt311open.mess_date  = STRING(Packet.PackDate)
          tt311open.mess_time  = Packet.PackTime
          tt311open.facct      = acct.acct
          tt311open.fcurrency  = acct.currency
       .

       /*  Наименование клиента */
       RUN GetCustName IN h_base ( acct.cust-cat,  acct.cust-id,  "",
                             OUTPUT       mCName1,
                             OUTPUT       mCName2,
                             INPUT-OUTPUT mCFake ).

       tt311open.cust_name  = mCName1 + " " + mCName2.
       mLOk = YES.
    END.

    /* Если сообщения не найдено, счет в отчете должен быть */
    IF mLOk = NO
    THEN DO:

       /*  Наименование клиента */
       RUN GetCustName IN h_base ( acct.cust-cat,  acct.cust-id,  "",
                             OUTPUT       mCName1,
                             OUTPUT       mCName2,
                             INPUT-OUTPUT mCFake ).

       CREATE tt311open.
       ASSIGN
          tt311open.direct     = "close"
          tt311open.acct       = STRING(acct.number)
          tt311open.acct_open  = acct.open-date
          tt311open.acct_close = acct.close-date
          tt311open.cust_name  = mCName1 + " " + mCName2
          tt311open.facct      = acct.acct
          tt311open.fcurrency  = acct.currency
       .
    END.
END.

RUN FindImpPacket IN THIS-PROCEDURE (mParam5,"ni","Подтверждено").
RUN FindImpPacket IN THIS-PROCEDURE (mParam6,"ni","Отвергнуто").
RUN FindImpPacket IN THIS-PROCEDURE (mParam7,"pfr","Подтверждено").
RUN FindImpPacket IN THIS-PROCEDURE (mParam8,"fss","Подтверждено").


RUN messmenu.p(9,
        "Отчет",
        "Выводить полный отчет или счета без подтверждения из НИ о приеме файла (краткий)?",
        "Полный,Краткий").
IF KEYFUNC(LASTKEY) = "END-ERROR" THEN RETURN.

/* Вставка Плюс банк */
IF GetParamByName(mParams, "Формат", ";", "=") NE "Excel"
THEN DO:
/* Конец вставки Плюс банк */
IF INT64(pick-value) = 1
THEN DO:
    /* Таблица с сообщениями по открытию счетов */
    RUN BeginCircle_TTName IN THIS-PROCEDURE ("open").
    FOR EACH tt311open WHERE tt311open.direct = "open"
/* Замена Плюс банк
       BREAK BY tt311open.acct_open:
*/      BREAK BY tt311open.acct_open
              BY tt311open.acct
              BY DATE(tt311open.mess_date)
              BY tt311open.mess_time:
/* Конец замены Плюс банк */
       RUN Insert_TTName IN THIS-PROCEDURE ("acct[open]",      tt311open.acct).
       RUN Insert_TTName IN THIS-PROCEDURE ("cust_name[open]", tt311open.cust_name).
       RUN Insert_TTName IN THIS-PROCEDURE ("acct_open[open]", STRING(tt311open.acct_open)).
       RUN Insert_TTName IN THIS-PROCEDURE ("mess_name[open]", tt311open.mess_name).
       RUN Insert_TTName IN THIS-PROCEDURE ("mess_date[open]", tt311open.mess_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("ni_date[open]",   tt311open.ni_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("ni_state[open]",  tt311open.ni_state).
       RUN Insert_TTName IN THIS-PROCEDURE ("pfo_date[open]",   tt311open.pfr_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("pfo_state[open]",  tt311open.pfr_state).
       RUN Insert_TTName IN THIS-PROCEDURE ("fss_date[open]",   tt311open.fss_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("fss_state[open]",  tt311open.fss_state).

       RUN NextCircle_TTName IN THIS-PROCEDURE ("open").
    END.
    RUN EndCircle_TTName IN THIS-PROCEDURE ("open").

    /* Таблица с сообщениями по закрытию счетов */
    RUN BeginCircle_TTName IN THIS-PROCEDURE ("close").
    FOR EACH tt311open WHERE tt311open.direct = "close"
/* Замена Плюс банк
       BREAK BY tt311open.acct_open:
*/      BREAK BY tt311open.acct_open
              BY tt311open.acct
              BY DATE(tt311open.mess_date)
              BY tt311open.mess_time:
/* Конец замены Плюс банк */
       RUN Insert_TTName IN THIS-PROCEDURE ("acct[close]",       tt311open.acct).
       RUN Insert_TTName IN THIS-PROCEDURE ("cust_name[close]",  tt311open.cust_name).
       RUN Insert_TTName IN THIS-PROCEDURE ("acct_close[close]", STRING(tt311open.acct_close)).
       RUN Insert_TTName IN THIS-PROCEDURE ("mess_name[close]",  tt311open.mess_name).
       RUN Insert_TTName IN THIS-PROCEDURE ("mess_date[close]",  tt311open.mess_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("ni_date[close]",    tt311open.ni_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("ni_state[close]",   tt311open.ni_state).
       RUN Insert_TTName IN THIS-PROCEDURE ("pfo_date[open]",   tt311open.pfr_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("pfo_state[open]",  tt311open.pfr_state).
       RUN Insert_TTName IN THIS-PROCEDURE ("fss_date[open]",   tt311open.fss_date).
       RUN Insert_TTName IN THIS-PROCEDURE ("fss_state[open]",  tt311open.fss_state).

       RUN NextCircle_TTName IN THIS-PROCEDURE ("close").
    END.
    RUN EndCircle_TTName IN THIS-PROCEDURE ("close").
END.
ELSE DO:
    /* Таблица с сообщениями по открытию счетов */
    RUN BeginCircle_TTName IN THIS-PROCEDURE ("open").
    FOR EACH tt311open WHERE tt311open.direct = "open" NO-LOCK
            BREAK BY tt311open.acct_open
            BY tt311open.acct
/* Замена Плюс банк
            BY tt311open.mess_date
*/          BY DATE(tt311open.mess_date)
/* Конец замены Плюс банк */
            BY tt311open.mess_time:
       IF LAST-OF(tt311open.acct)
            AND tt311open.ni_state  <> "Подтверждено"
            AND tt311open.pfr_state <> "Подтверждено"
            AND tt311open.fss_state <> "Подтверждено"
       THEN DO:

           RUN Insert_TTName IN THIS-PROCEDURE ("acct[open]",      tt311open.acct).
           RUN Insert_TTName IN THIS-PROCEDURE ("cust_name[open]", tt311open.cust_name).
           RUN Insert_TTName IN THIS-PROCEDURE ("acct_open[open]", STRING(tt311open.acct_open)).
           RUN Insert_TTName IN THIS-PROCEDURE ("mess_name[open]", tt311open.mess_name).
           RUN Insert_TTName IN THIS-PROCEDURE ("mess_date[open]", tt311open.mess_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("ni_date[open]",   tt311open.ni_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("ni_state[open]",  tt311open.ni_state).
           RUN Insert_TTName IN THIS-PROCEDURE ("pfo_date[open]",   tt311open.pfr_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("pfo_state[open]",  tt311open.pfr_state).
           RUN Insert_TTName IN THIS-PROCEDURE ("fss_date[open]",   tt311open.fss_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("fss_state[open]",  tt311open.fss_state).

           RUN NextCircle_TTName IN THIS-PROCEDURE ("open").
       END.
    END.
    RUN EndCircle_TTName IN THIS-PROCEDURE ("open").

    /* Таблица с сообщениями по закрытию счетов */
    RUN BeginCircle_TTName IN THIS-PROCEDURE ("close").
    FOR EACH tt311open WHERE tt311open.direct = "close" NO-LOCK
            BREAK BY tt311open.acct_close
            BY tt311open.acct
/* Замена Плюс банк
            BY tt311open.mess_date
*/          BY DATE(tt311open.mess_date)
/* Конец замены Плюс банк */
            BY tt311open.mess_time:
       IF LAST-OF(tt311open.acct)
            AND tt311open.ni_state  <> "Подтверждено"
            AND tt311open.pfr_state <> "Подтверждено"
            AND tt311open.fss_state <> "Подтверждено"
       THEN DO:

           RUN Insert_TTName IN THIS-PROCEDURE ("acct[close]",       tt311open.acct).
           RUN Insert_TTName IN THIS-PROCEDURE ("cust_name[close]",  tt311open.cust_name).
           RUN Insert_TTName IN THIS-PROCEDURE ("acct_close[close]", STRING(tt311open.acct_close)).
           RUN Insert_TTName IN THIS-PROCEDURE ("mess_name[close]",  tt311open.mess_name).
           RUN Insert_TTName IN THIS-PROCEDURE ("mess_date[close]",  tt311open.mess_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("ni_date[close]",    tt311open.ni_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("ni_state[close]",   tt311open.ni_state).
           RUN Insert_TTName IN THIS-PROCEDURE ("pfo_date[open]",   tt311open.pfr_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("pfo_state[open]",  tt311open.pfr_state).
           RUN Insert_TTName IN THIS-PROCEDURE ("fss_date[open]",   tt311open.fss_date).
           RUN Insert_TTName IN THIS-PROCEDURE ("fss_state[open]",  tt311open.fss_state).

           RUN NextCircle_TTName IN THIS-PROCEDURE ("close").
       END.
    END.
    RUN EndCircle_TTName IN THIS-PROCEDURE ("close").

END.

/* Вызов ОСП */
RUN printvd.p ("rep311acct", INPUT TABLE ttNames).

/* Вставка Плюс банк */
END.
ELSE DO:
    DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFL   AS LOGICAL   NO-UNDO.

    lFL = (mParam1 BEGINS "40817").
    cFl = "./rep311acct.xml".
    OUTPUT TO VALUE(cFl).

    /* Отчет об открытии счетов *********************************************** */
    IF lFL
    THEN PUT UNFORMATTED XLHead("open", "CCDCDDC", "150,300,71,360,110,71,96").                 /* ФЛ */
    ELSE PUT UNFORMATTED XLHead("open", "CCDCDDCDCDC", "150,300,71,360,110,71,96,71,96,71,96"). /* ЮЛ */

    cXL = XLCellHat("Отчет по отправленным/полученным сообщениям об открытии счета (депозита) с "
                  + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999"), IF lFL THEN 6 ELSE 10).
    PUT UNFORMATTED XLRowH(0, 34) cXL XLRowEnd().

    cXL = XLCellHead("Номер счета",0,2,0)
        + XLCellHead("Наименование клиента",0,2,0)
        + XLCellHead("Дата открытия счета",0,2,0)
        + XLCellHead("Имя сообщения",0,2,0)
        + XLCellHead("Дата формирования сообщения",0,2,0)
        + XLCellHead("Квитанции / извещения",0,0, IF lFL THEN 1 ELSE 5)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    cXL = XLCellHead("НИ",6,0,1)
        + IF lFL THEN "" ELSE (
          XLCellHead("ФСС",0,0,1)
        + XLCellHead("ПФР",0,0,1))
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    cXL = XLCellHead("Дата",6,0,0)
        + XLCellHead("Статус",0,0,0)
        + IF lFL THEN "" ELSE (
          XLCellHead("Дата",0,0,0)
        + XLCellHead("Статус",0,0,0)
        + XLCellHead("Дата",0,0,0)
        + XLCellHead("Статус",0,0,0))
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

    FOR EACH tt311open WHERE tt311open.direct = "open"
        NO-LOCK
        BREAK BY tt311open.acct_open
              BY tt311open.acct
              BY DATE(tt311open.mess_date)
              BY tt311open.mess_time:

        IF      (pick-value = "1")
            OR  (LAST-OF(tt311open.acct)
            AND tt311open.ni_state  <> "Подтверждено"
            AND tt311open.pfr_state <> "Подтверждено"
            AND tt311open.fss_state <> "Подтверждено")
        THEN DO:
            cXL = XLCell(tt311open.acct)
                + XLCell(tt311open.cust_name)
                + XLDateCell(tt311open.acct_open)
                + XLCell(tt311open.mess_name)
                + XLDateCell(IF (tt311open.mess_date EQ "") THEN ? ELSE DATE(tt311open.mess_date))
                + XLDateCell(IF (tt311open.ni_date   EQ "") THEN ? ELSE DATE(tt311open.ni_date))
                + XLCell(tt311open.ni_state)
                + IF lFL THEN "" ELSE (
                  XLDateCell(IF (tt311open.fss_date  EQ "") THEN ? ELSE DATE(tt311open.fss_date))
                + XLCell(tt311open.fss_state)
                + XLDateCell(IF (tt311open.pfr_date  EQ "") THEN ? ELSE DATE(tt311open.pfr_date))
                + XLCell(tt311open.pfr_state))
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        END.
    END.

    /* Отчет об закрытии счетов *********************************************** */
    IF lFL
    THEN PUT UNFORMATTED XLNextList("close", "CCDCDDC", "150,300,71,360,110,71,96").                 /* ФЛ */
    ELSE PUT UNFORMATTED XLNextList("close", "CCDCDDCDCDC", "150,300,71,360,110,71,96,71,96,71,96"). /* ЮЛ */

    cXL = XLCellHat("Отчет по отправленным/полученным сообщениям о закрытии счета (депозита) с "
                  + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999"), IF lFL THEN 6 ELSE 10).
    PUT UNFORMATTED XLRowH(0, 34) cXL XLRowEnd().

    cXL = XLCellHead("Номер счета",0,2,0)
        + XLCellHead("Наименование клиента",0,2,0)
        + XLCellHead("Дата закрытия счета",0,2,0)
        + XLCellHead("Имя сообщения",0,2,0)
        + XLCellHead("Дата формирования сообщения",0,2,0)
        + XLCellHead("Квитанции / извещения",0,0, IF lFL THEN 1 ELSE 5)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    cXL = XLCellHead("НИ",6,0,1)
        + IF lFL THEN "" ELSE (
          XLCellHead("ФСС",0,0,1)
        + XLCellHead("ПФР",0,0,1))
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
    cXL = XLCellHead("Дата",6,0,0)
        + XLCellHead("Статус",0,0,0)
        + IF lFL THEN "" ELSE (
          XLCellHead("Дата",0,0,0)
        + XLCellHead("Статус",0,0,0)
        + XLCellHead("Дата",0,0,0)
        + XLCellHead("Статус",0,0,0))
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

    FOR EACH tt311open WHERE tt311open.direct = "close"
        NO-LOCK
        BREAK BY tt311open.acct_close
              BY tt311open.acct
              BY DATE(tt311open.mess_date)
              BY tt311open.mess_time:

        IF      (pick-value = "1")
            OR  (LAST-OF(tt311open.acct)
            AND tt311open.ni_state  <> "Подтверждено"
            AND tt311open.pfr_state <> "Подтверждено"
            AND tt311open.fss_state <> "Подтверждено")
        THEN DO:
            cXL = XLCell(tt311open.acct)
                + XLCell(tt311open.cust_name)
                + XLDateCell(tt311open.acct_close)
                + XLCell(tt311open.mess_name)
                + XLDateCell(IF (tt311open.mess_date EQ "") THEN ? ELSE DATE(tt311open.mess_date))
                + XLDateCell(IF (tt311open.ni_date   EQ "") THEN ? ELSE DATE(tt311open.ni_date))
                + XLCell(tt311open.ni_state)
                + IF lFL THEN "" ELSE (
                  XLDateCell(IF (tt311open.fss_date  EQ "") THEN ? ELSE DATE(tt311open.fss_date))
                + XLCell(tt311open.fss_state)
                + XLDateCell(IF (tt311open.pfr_date  EQ "") THEN ? ELSE DATE(tt311open.pfr_date))
                + XLCell(tt311open.pfr_state))
                .
            PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
        END.
    END.
    PUT UNFORMATTED XLEnd().
    OUTPUT CLOSE.

    /* Перед отправкой протокола проверим, запущен ли bispc */
    DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
    DO WHILE (mRet EQ ""):
        RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
        IF (mRet EQ "")
        THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
    END.
    /* Отправляем протокол */
    RUN sndbispc.p ("file=" + cFl + ";class=bq").
END.
/* Конец вставки Плюс банк */
{intrface.del}

/* Ищем пакеты импорта (ответы) */
PROCEDURE FindImpPacket PRIVATE:
    DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iStat  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vCanDoNi  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vIn       AS INT64 NO-UNDO.

    DEF BUFFER impPackObject FOR PackObject.
    DEF BUFFER impPacket   FOR Packet.
    DEF BUFFER impSeance   FOR Seance.
    DEF BUFFER impFileExch FOR FileExch.


    FOR EACH tt311open WHERE tt311open.pack_name <> "":
    vCanDoNi = "".
       /* маски для быстроты сравнения (чтобы не SUBSTRING в запросе) */
       DO vIn = 1 TO NUM-ENTRIES(iParam):
          vCanDoNi = vCanDoNi + ENTRY(vIn, iParam) + SUBSTRING(tt311open.pack_name, 4).
          IF vIn <> NUM-ENTRIES(iParam) THEN
             vCanDoNi =  vCanDoNi + ",".
       END.
       /* Ищем сами пакеты */
       FOR EACH impPackObject WHERE
             impPackObject.file-name = "acct" AND
             impPackObject.Surrogate = tt311open.facct + "," + tt311open.fcurrency NO-LOCK,
        EACH impPacket WHERE
             impPacket.PacketID = impPackObject.PacketID AND
             impPacket.Class-Code    = "PTax"    AND
             impPacket.Kind          = "TaxImp" NO-LOCK,
           EACH impFileExch WHERE
                impFileExch.FileExchID  = impPacket.FileExchID AND
                CAN-DO(vCanDoNi,impFileExch.name)
       NO-LOCK:
       IF iField = "ni"
       THEN
          ASSIGN
             tt311open.ni_date    = STRING(impPacket.PackDate)
             tt311open.ni_state   = iStat
          .
       ELSE IF iField = "pfr"
       THEN
          ASSIGN
             tt311open.pfr_date    = STRING(impPacket.PackDate)
             tt311open.pfr_state   = iStat
          .
       ELSE IF iField = "fss"
       THEN
          ASSIGN
             tt311open.fss_date    = STRING(impPacket.PackDate)
             tt311open.fss_state   = iStat
          .
       END.
    END.
END PROCEDURE.
/* $LINTFILE='rep311acct.p' */
/* $LINTMODE='1,3,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='miam' */
/* $LINTDATE='26/01/2017 15:55:14.545+03:00' */
/*prosignXpXGmsRW+ZlStFGjnp74NQ*/