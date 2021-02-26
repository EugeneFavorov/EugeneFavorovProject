/**
Авторские права принадлежат: ПАО Плюс банк
Основание:     ОСРКО.801 Создание отчетов по корректирующим и отменяющим сообщениям по 311-П
Что делает:    
Параметры:     Тип отчета: корр / отмен
Место запуска: Сообщения обмена с ФНС - Ctrl-G
Создан:        27.10.2017 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{intrface.get netw}     /** Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{pb_pack440.pro}

DEFINE INPUT PARAMETER iParam    AS CHARACTER NO-UNDO.
{getdates.i}

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMask   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAcct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE fName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE dNIdate AS DATE      NO-UNDO.
DEFINE VARIABLE cNIstat AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFSdate AS DATE      NO-UNDO.
DEFINE VARIABLE cFSstat AS CHARACTER NO-UNDO.
DEFINE BUFFER   impPackObject   FOR PackObject.
DEFINE BUFFER   impPacket       FOR Packet.
DEFINE BUFFER   impFileExch     FOR FileExch.

cFl = STRING(YEAR(end-date)) + STRING(MONTH(end-date), "99") + STRING(DAY(end-date), "99").
cFl = "./" + (IF (iParam = "корр") THEN "corr" ELSE "otm") + "311p-" + cFl + ".xml".
OUTPUT TO VALUE(cFl).

/******************************************* Заголовок отчета */
IF (iParam = "корр")
THEN ASSIGN
        cXL   = "корректирующим"
        cMask = "SFC02*,SFC12*,SFC22*,SBC02*,SBC12*,SBC22*".
ELSE ASSIGN
        cXL   = "отменяющим"
        cMask = "SFC03*,SFC23*,SBC03*,SBC23*".

PUT UNFORMATTED XLHead("311-П", "CCCCCDCDC", "150,284,385,50,145,71,96,71,96").

cXL = XLCellHat("Отчет по отправленным " + cXL + " сообщениям об открытии/закрытии счета (депозита) с "
              + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999"),8).
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("Номер счета",0,2,0)
    + XLCellHead("Наименование /~nФИО клиента",0,2,0)
    + XLCellHead("Имя сообщения",0,2,0)
    + XLCellHead("Статус",0,2,0)
    + XLCellHead("Дата и время~nформирования~nсообщения",0,2,0)
    + XLCellHead("Квитанции / извещения",0,0,3)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("НИ",6,0,1)
    + XLCellHead("ФСС",0,0,1)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("Дата",6,0,0)
    + XLCellHead("Статус",0,0,0)
    + XLCellHead("Дата",0,0,0)
    + XLCellHead("Статус",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

/******************************************* Отчет */
FOR EACH Packet
    WHERE (Packet.Class-Code    BEGINS 'PTAX')
      AND (Packet.Kind          BEGINS 'TAX')
      AND (Packet.ParentID      = 0)
      AND (Packet.filial-id     = shFilial)
      AND (Packet.PackDate      >= beg-date)
      AND (Packet.PackDate      <= end-date)
    NO-LOCK,
FIRST FileExch
    WHERE (FileExch.FileExchID  = Packet.FileExchID)
      AND CAN-DO(cMask, FileExch.Name)
    NO-LOCK:

    RELEASE PackObject.
    RELEASE acct.
    fName   = SUBSTRING(FileExch.Name,4).
    dNIdate = ?.
    cNIstat = "".
    dFSdate = ?.
    cFSstat = "".
    cAcct   = "".
    cName   = "(счет не найден)".

    FIND FIRST PacketText OF Packet
        NO-LOCK NO-ERROR.
    IF (AVAIL PacketText)
    THEN DO:
        cAcct   = GetFileVar440(STRING(PacketText.Contents), "НомСч",  "").
        IF (SUBSTRING(FileExch.Name,2,1) = "B")
        THEN cName  = GetFileVar440(STRING(PacketText.Contents), "НаимОрг","").
        ELSE cName  = GetFileVar440(STRING(PacketText.Contents), "Фамилия","") + " "
                    + GetFileVar440(STRING(PacketText.Contents), "Имя","")     + " "
                    + GetFileVar440(STRING(PacketText.Contents), "Отчество","").
    END.
    ELSE DO:    /* если нет текста, ищем связанный объект */
        FIND FIRST PackObject
            WHERE (PackObject.PacketID  = Packet.PacketID)
              AND (PackObject.file-name = 'acct')
            NO-LOCK NO-ERROR.
        IF (AVAIL PackObject)
        THEN DO:
            cAcct = SUBSTRING(PackObject.Surrogate, 1, 20).

            FIND FIRST acct
                WHERE (acct.acct     = ENTRY(1, PackObject.Surrogate))
                  AND (acct.currency = ENTRY(2, PackObject.Surrogate))
                NO-LOCK NO-ERROR.
            IF (AVAIL acct)
            THEN DO:
                cAcct = acct.number.

                CASE acct.cust-cat:
                    WHEN "Ю" THEN DO:
                        FOR FIRST cust-corp
                            WHERE (cust-corp.cust-id    = acct.cust-id)
                            NO-LOCK:

                            cName = cust-corp.name-short.
                        END.
                    END.
                    WHEN "Ч" THEN DO:
                        FOR FIRST person
                            WHERE (person.person-id     = acct.cust-id)
                            NO-LOCK:

                            cName = person.name-last + " " + person.first-names.
                        END.
                    END.
                    OTHERWISE NEXT.
                END CASE.
            END.
        END.
    END.

    FOR EACH impPacket
        WHERE (impPacket.Class-Code     = "PTax")
          AND (impPacket.Kind           = "TaxImp")
          AND (impPacket.filial-id      = shFilial)
          AND (impPacket.PackDate       >= Packet.PackDate)
        NO-LOCK,
    EACH impFileExch
        WHERE (impFileExch.FileExchID   = impPacket.FileExchID)
          AND CAN-DO("SFF*,SBF*,SFE*,SFK*,SBE*,SBK*,SBR*",impFileExch.name)
          AND (SUBSTRING(impFileExch.name,4) = fName)
        NO-LOCK:

        CASE SUBSTRING(impFileExch.name, 1, 3):
            WHEN "SBR" THEN
                ASSIGN
                    dFSdate = impPacket.PackDate
                    cFSstat = "Подтверждено".
            WHEN "SFF" OR WHEN "SBF" THEN
                ASSIGN
                    dNIdate = impPacket.PackDate
                    cNIstat = "Подтверждено".
            OTHERWISE
                ASSIGN
                    dNIdate = impPacket.PackDate
                    cNIstat = "Отвергнуто".
        END CASE.
    END.

    cXL = XLCell(cAcct)
        + XLCellWrap(cName)
        + XLCell(FileExch.Name)
        + XLCell(Packet.State)
        + XLCell(STRING(Packet.PackDate,"99.99.9999") + " " + STRING(Packet.PackTime,"hh:mm:ss"))
        + XLDateCell(dNIdate)
        + XLCell(cNIstat)
        + XLDateCell(dFSdate)
        + XLCell(cFSstat)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/* Перед отправкой отчета проверим, запущен ли bispc */
DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
DO WHILE (mRet EQ ""):
    RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
    IF (mRet EQ "")
    THEN MESSAGE "Запустите программу bispc и нажмите ОК" VIEW-AS ALERT-BOX.
END.
/* Отправляем протокол */
RUN sndbispc.p ("file=" + cFl + ";class=bq").

{intrface.del}
