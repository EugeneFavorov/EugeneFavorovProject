/**
Авторские права принадлежат: Связной Банк (ЗАО)
Что делает:     Загружает в Бисквит содержимое файлов 440-П
Как работает:   Создает отсутствующие PacketText
Параметры:      
Создан:         30.11.2017 Борисов А.В.
*/

{globals.i}
{pb_logit.i}
{intrface.get pack}

/* Импорт файлов из каталога */
DEFINE VARIABLE cDir        AS CHARACTER    NO-UNDO INIT "/home2/bis/quit41d/imp-exp/0500/440p/old/".
DEFINE VARIABLE cFName      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFFull      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFType      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cLog        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cStr2       AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE cStr        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cText       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iFile       AS INTEGER      NO-UNDO INIT 0.
DEFINE VARIABLE iOK         AS INTEGER      NO-UNDO INIT 0.
DEFINE STREAM inf.

cLog = cDir + "log/"
     + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + "-"
     + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".log".

INPUT FROM OS-DIR(cDir + "in").
REPEAT:
    IMPORT cFName cFFull cFType.

    IF (cFType = "F") AND CAN-DO("*.xml", cFName)
    THEN DO:
        RUN Logit("Найден файл " + cFName, cLog).
        iFile = iFile + 1.

        FOR EACH FileExch
            WHERE FileExch.Name     = cFName
            NO-LOCK,
        FIRST Packet
            WHERE Packet.FileExchID = FileExch.FileExchID
            NO-LOCK:

            FIND FIRST PacketText
                WHERE (PacketText.PacketID  = Packet.PacketID)
                NO-LOCK NO-ERROR.
            IF (AVAIL PacketText)
            THEN DO:
                RUN Logit(" - Найден пакет c текстом, файл не добавлялся.", cLog).
            END.
            ELSE DO:
                cText = "".
                INPUT STREAM inf FROM VALUE(cFFull) BINARY NO-ECHO NO-CONVERT.
                READKEY STREAM inf PAUSE 0.
                DO WHILE (LASTKEY > 0):
                    cText = cText + CHR(LASTKEY).
                    READKEY STREAM inf PAUSE 0.
                END.
                INPUT STREAM inf CLOSE.

                CREATE PacketText.
                ASSIGN
                    PacketText.PacketTextID = next-value(pack-id)
                    PacketText.PacketID     = Packet.PacketID
                    PacketText.Order        = 1
                    PacketText.Contents     = REPLACE(REPLACE(cText, "~r~n", "~r"), "~r", "~r~n")
                    .
                RUN Logit(" + Найден пакет без текста. Файл добавлен.", cLog).
                iOK = iOK + 1.
            END.
        END.

        IF (NOT AVAIL Packet)
        THEN DO:
            RUN Logit(" - Пакет не найден.", cLog).
        END.

        OS-COPY VALUE(cFFull)  VALUE(cDir + "arc").
        OS-DELETE VALUE(cFFull).
    END.
END.

INPUT CLOSE.
MESSAGE "Обработано " + STRING(iFile) + " файлов. Успешно загружено " + STRING(iOK) + " шт."
    VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
{intrface.del}
