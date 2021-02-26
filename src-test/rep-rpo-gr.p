{globals.i}
{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}

{wordwrap.def}
{sh-defs.i}
{flt-file.i}
{parsin.def}

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.    /* mask=RPO*,ROO*;acct=открыт/закрыт */

DEFINE VARIABLE cMask       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOpened     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mInt        AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mContents   AS CHARACTER NO-UNDO EXTENT 30.
DEFINE VARIABLE mGroup      AS CHARACTER NO-UNDO INIT "*".
DEFINE VARIABLE mIsGroup    AS LOGICAL   NO-UNDO.       /* Найден счет нужной группы */

cMask   = GetParamByNameAsChar(iParams,"mask","RPO*,ROO*").
lOpened = GetParamByNameAsChar(iParams,"acct","открыт") EQ "открыт".

{getdates.i}
IF (shFilial EQ "0500") AND lOpened
THEN DO:
   RUN g-prompt.p ("char", "Группы ", "x(100)", "*", "Введите группы счетов (F1 - выбор)", 50, ",", "F1=pb_getgroup.p",?,?,OUTPUT mGroup).
   IF (mGroup EQ ?) THEN RETURN.
END.

{setdest.i &file-name = "rep-rpo.txt" &custom = " IF YES THEN 0 ELSE "}

FOR EACH Packet USE-INDEX DateTime
/*  WHERE (Packet.Class-Code    BEGINS 'PTAX')
      AND (Packet.Kind          BEGINS 'ETAX')
*/  WHERE CAN-DO('PTax440,PTaxPB', TRIM(Packet.Class-Code))
      AND (Packet.PackDate      GE beg-date)
      AND (Packet.PackDate      LE end-date)
      AND (Packet.ParentID      EQ 0)
      AND (Packet.filial-id     EQ shFilial)
    NO-LOCK,
FIRST FileExch
    WHERE FileExch.FileExchID EQ Packet.FileExchID
      AND CAN-DO(cMask, FileExch.Name)
    NO-LOCK:

    /* Отбор счетов */
    mIsGroup  = NOT lOpened.
    FOR EACH PackObject
        WHERE (PackObject.PacketID  EQ Packet.PacketID)
          AND (PackObject.file-name EQ 'acct')
        NO-LOCK:

        FIND FIRST acct
            WHERE (acct.acct        EQ ENTRY(1, PackObject.Surrogate))
              AND (acct.currency    EQ ENTRY(2, PackObject.Surrogate))
              AND CAN-DO("Расчет,Текущ", acct.contract)
            NO-LOCK NO-ERROR.
        IF lOpened
        THEN mIsGroup =   (AVAIL acct)
                      AND (acct.close-date EQ ?)
                      AND CAN-DO(mGroup, GetXAttrValue("acct", PackObject.Surrogate, "groupOABS")).
        ELSE mIsGroup = (NOT AVAIL acct) OR (acct.close-date NE ?).

        IF mIsGroup THEN LEAVE.     /* В пакете нашелся подходящий счет */
    END.
    IF NOT mIsGroup THEN NEXT.

    /* Печать одного сообщения */
    FOR EACH PacketText
        WHERE PacketText.PacketID EQ Packet.PacketID
        NO-LOCK:

        DO mCnt = 1 TO NUM-ENTRIES(PacketText.Contents,"~n"):
            mContents[1] = CODEPAGE-CONVERT(ENTRY(mCnt,PacketText.Contents,"~n"), SESSION:CHARSET, "1251").
            {wordwrap.i
                &s = mContents
                &n = 30
                &l = 75
                }
            PUT UNFORMATTED mContents[1] SKIP.

            DO mInt = 2 TO 30 :
                IF {assigned mContents[mInt]}
                THEN PUT UNFORMATTED mContents[mInt] SKIP.
            END.
        END.
    END.
    PUT UNFORMATTED CHR(12) SKIP.
END.

{preview.i &file-name = "rep-rpo.txt"}
RETURN.
