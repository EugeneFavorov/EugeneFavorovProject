/**
Авторские права принадлежат: ПАО Плюс банк
Основание:      ОСРКО.521
Что делает:     Отчет по сообщениям BOS
Как работает:   
Параметры:      
Место запуска:  
Создан:         20.04.2017 Борисов А.В.
*/

{globals.i}             /** Глобальные определения */
{intrface.get xclass}   /** Функции для работы с метасхемой */
{intrface.get netw}     /* Отправка в bispc */
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{getdates.i}

FUNCTION PoleTxt    RETURNS CHARACTER
   (INPUT  iTxt     AS CHARACTER,       /* PacketText.Contents */
    INPUT  iPole    AS CHARACTER ).     /* Имя поля */

    DEFINE VARIABLE I1          AS INTEGER      NO-UNDO.

    I1 = INDEX(iTxt, iPole).
    If (I1 = 0) THEN RETURN "".
    I1 = I1 + LENGTH(iPole).
    RETURN SUBSTRING(iTxt, I1, INDEX(iTxt, "~n", I1) - I1).
END FUNCTION.

/******************************************* Определение переменных и др. */
DEFINE VARIABLE cXL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAcc    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNI     AS CHARACTER NO-UNDO.   /* Квитанция из налоговой */
DEFINE VARIABLE cErr    AS CHARACTER NO-UNDO.
DEFINE BUFFER   bPack   FOR Packet.
DEFINE BUFFER   bPTxt   FOR PacketText.

cFL = "./bos-report.xml".
OUTPUT TO VALUE(cFL).

/******************************************* Реализация */
PUT UNFORMATTED XLHead("BOS", "CCCCCC", "127,171,300,85,97,343").
cXL = XLCellHat("Файлы BOS_RBN за "
              + (IF (beg-date EQ end-date)
                 THEN STRING(beg-date, "99.99.9999")
                 ELSE ("период с " + STRING(beg-date, "99.99.9999")
                     + " по "      + STRING(end-date, "99.99.9999"))), 5).
PUT UNFORMATTED XLRowH(0, 34) cXL XLRowEnd().
cXL = XLCellHead("Дата и время формирования файла",0,0,0)
    + XLCellHead("Номер расчетного счета ",0,0,0)
    + XLCellHead("Имя исходящего файла",0,0,0)
    + XLCellHead("Статус исходящего файла",0,0,0)
    + XLCellHead("Квитанция из налоговой на BOS_RBN",0,0,0)
    + XLCellHead("Уведомление об ошибках обработки/формирования",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH Packet
    WHERE (Packet.Class-Code    BEGINS 'PTAX')
      AND (Packet.Kind          BEGINS 'ETAX')
      AND (Packet.PackDate      GE beg-date)
      AND (Packet.PackDate      LE end-date)
      AND (Packet.ParentID      EQ 0)
      AND (Packet.filial-id     EQ shFilial)
    NO-LOCK,
FIRST Seance
    WHERE (Seance.SeanceID      EQ Packet.SeanceID)
      AND (Seance.op-kind       EQ "e-txrsac")
    NO-LOCK,
FIRST PacketText
    WHERE (PacketText.PacketID  EQ Packet.PacketID)
    NO-LOCK:
    
    cAcc = PoleTxt(PacketText.Contents, "НомСч:").
    cNI  = "".
    cErr = "".
    FOR FIRST PackObject
        WHERE (PackObject.PacketID  EQ Packet.PacketID)
          AND (PackObject.file-name EQ "Packet")
        NO-LOCK,
    FIRST bPack
        WHERE (bPack.Class-Code     EQ "PTaxKwt")
          AND (bPack.PacketID       EQ INT64(PackObject.Surrogate))
        NO-LOCK,
    FIRST bPTxt
        WHERE (bPTxt.PacketID       EQ bPack.PacketID)
        NO-LOCK:

        cNI  = IF (INDEX(bPTxt.Contents, "~n20@@@~n") = 0) THEN "Ошибка" ELSE "20".
        cErr = PoleTxt(bPTxt.Contents, "ДОЛЖНПРБ::").
    END.

    cXL = XLCell(STRING(Packet.PackDate, "99.99.9999") + " "
               + STRING(Packet.PackTime, "HH:MM:SS"))
        + XLCell(cAcc)
        + XLCell(GetXAttrValue("Packet", STRING(Packet.PacketID), "FileName"))
        + XLCell(Packet.State)
        + XLCell(cNI)
        + XLCellWrap(cErr)
        .
    PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
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
RUN sndbispc.p ("file=" + cFL + ";class=bq").

{intrface.del}
