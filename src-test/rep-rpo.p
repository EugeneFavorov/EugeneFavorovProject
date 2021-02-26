{globals.i}
{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}

{wordwrap.def}
{sh-defs.i}
{flt-file.i}

{tmprecid.def}

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.

DEFINE VARIABLE mFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
DEFINE VARIABLE mInt        AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mContents   AS CHARACTER NO-UNDO EXTENT 5.

{setdest.i &file-name = "rep-rpo.txt" &custom = " IF YES THEN 0 ELSE "}


FOR EACH TmpRecID NO-LOCK,
   Packet WHERE
   RECID(Packet) EQ TmpRecID.id
   NO-LOCK:

   FIND FIRST FileExch WHERE
      FileExch.FileExchID EQ Packet.FileExchID
   NO-LOCK NO-ERROR.
   IF AVAIL(FileExch) THEN
   DO:
      mFileName = FileExch.Name.
      IF CAN-DO(iParams,mFileName) THEN
      DO:
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
                  IF {assigned mContents[mInt]} THEN
                  PUT UNFORMATTED mContents[mInt] SKIP.
               END.
            END.
         END.
         PUT UNFORMATTED CHR(12) SKIP.
      END.
   END.
END.

{preview.i &file-name = "rep-rpo.txt"}

RETURN.
