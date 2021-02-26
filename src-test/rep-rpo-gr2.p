{globals.i}
{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}

{wordwrap.def}
{sh-defs.i}
{flt-file.i}
{parsin.def}

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.    /* mask=RPO*,ROO*;acct=�����/������ */

DEFINE VARIABLE cMask       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOpened     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mInt        AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mContents   AS CHARACTER NO-UNDO EXTENT 5.
DEFINE VARIABLE mGroup      AS CHARACTER NO-UNDO INIT "*".
DEFINE VARIABLE mIsGroup    AS LOGICAL   NO-UNDO.       /* ������ ��� �㦭�� ��㯯� */

cMask   = GetParamByNameAsChar(iParams,"mask","RPO*,ROO*").
lOpened = GetParamByNameAsChar(iParams,"acct","�����") EQ "�����".

{getdates.i}
IF (shFilial EQ "0500")
THEN DO:
   RUN g-prompt.p ("char", "��㯯� ", "x(100)", "*", "������ ��㯯� ��⮢ (F1 - �롮�)", 50, ",", "F1=pb_getgroup.p",?,?,OUTPUT mGroup).
   IF (mGroup EQ ?) THEN RETURN.
END.

{setdest.i &file-name = "rep-rpo.txt" &custom = " IF YES THEN 0 ELSE "}

FOR EACH Packet USE-INDEX DateTime
    WHERE (Packet.Class-Code    BEGINS 'PTAX')
      AND (Packet.Kind          BEGINS 'ETAX')
      AND (Packet.PackDate      GE beg-date)
      AND (Packet.PackDate      LE end-date)
      AND (Packet.ParentID      EQ 0)
      AND (Packet.filial-id     EQ shFilial)
    NO-LOCK,
FIRST FileExch
    WHERE FileExch.FileExchID EQ Packet.FileExchID
      AND CAN-DO(cMask, FileExch.Name)
    NO-LOCK:

    /* �⡮� ��⮢ */
    IF (mGroup NE "*")
    THEN DO:
        mIsGroup  = YES.
        FOR EACH PackObject
            WHERE (PackObject.PacketID  EQ Packet.PacketID)
              AND (PackObject.file-name EQ 'acct')
            NO-LOCK,
        FIRST acct
            WHERE (acct.acct        EQ ENTRY(1, PackObject.Surrogate))
              AND (acct.currency    EQ ENTRY(2, PackObject.Surrogate))
              AND CAN-DO("�����,�����", acct.contract)
            NO-LOCK:

            mIsGroup = CAN-DO(mGroup, GetXAttrValue("acct", PackObject.Surrogate, "groupOABS")).
            IF mIsGroup THEN LEAVE.     /* � ����� ��襫�� ���室�騩 ��� */
        END.
        IF NOT mIsGroup THEN NEXT.
    END.

    /* ����� ������ ᮮ�饭�� */
    FOR EACH PacketText
        WHERE PacketText.PacketID EQ Packet.PacketID
        NO-LOCK:

        DO mCnt = 1 TO NUM-ENTRIES(PacketText.Contents,"~n"):
            mContents[1] = ENTRY(mCnt,PacketText.Contents,"~n").
            {wordwrap.i
                &s = mContents
                &n = 5
                &l = 75
                }
            PUT UNFORMATTED mContents[1] SKIP.

            DO mInt = 2 TO 5 :
                IF {assigned mContents[mInt]}
                THEN PUT UNFORMATTED mContents[mInt] SKIP.
            END.
        END.
    END.
    PUT UNFORMATTED CHR(12) SKIP.
END.

{preview.i &file-name = "rep-rpo.txt"}
RETURN.
