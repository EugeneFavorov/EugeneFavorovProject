/*              
                ­ª®¢áª ï ¨­â¥£à¨à®¢ ­­ ï á¨áâ¥¬  ˆ‘ª¢¨â
    Copyright: (C) 1992-2014 ‡€Ž " ­ª®¢áª¨¥ ¨­ä®à¬ æ¨®­­ë¥ á¨áâ¥¬ë"
     Filename: SNC-ED503-REP.P
      Comment: ‚ë¢®¤ ¯à®â®ª®«  íªá¯®àâ  á®®¡é¥­¨© SWIFT
   Parameters: iSeanceID - ¨¤¥­â¨ä¨ª â®à á¥ ­á 
         Uses:
      Used BY:
      Created: 27.11.2014 VASOV
     Modified: 
*/

{globals.i}

DEFINE INPUT  PARAMETER iSeanceID AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mSeanceID  AS INT64       NO-UNDO.
DEFINE VARIABLE mLogFName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mSeanceNum AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mType      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mFName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mRef       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAmt       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mDocNum    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mInt       AS INT64       NO-UNDO.

DEFINE STREAM mSRep.

DEFINE BUFFER bPacket FOR Packet.

mSeanceID = INT64(iSeanceID) NO-ERROR.
mLogFName = "ED503-" + STRING(mSeanceID, "999999999") + ".log".

FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID NO-LOCK NO-ERROR.
mSeanceNum = Seance.Number.

mInt = 0.
FOR EACH Packet WHERE Packet.SeanceID EQ mSeanceID AND Packet.ParentID EQ 0
   NO-LOCK,
   FIRST signs WHERE signs.file-name EQ "Packet"
                 AND signs.surrogate EQ STRING (Packet.PacketID)
                 AND signs.code      EQ "SWBICRec"
   NO-LOCK,
   FIRST Reference WHERE Reference.PacketID EQ Packet.PacketID
   NO-LOCK:
   FOR EACH bPacket WHERE bPacket.ParentID EQ Packet.PacketID
      NO-LOCK:
      FOR EACH PackObject WHERE PackObject.PacketID  EQ bPacket.PacketID
                            AND PackObject.file-name EQ "op-entry"
         NO-LOCK,
         FIRST op WHERE op.op EQ INT64(ENTRY(1, PackObject.Surrogate))
         NO-LOCK:
         mInt = mInt + 1.
      END.
   END.
END.

IF mInt GT 0 THEN
DO:
   
	{setdest.i &stream = " STREAM mSRep " &filename = mLogFName}
   
   PUT STREAM mSRep UNFORMATTED
      "Ž’ŽŠŽ‹ Š‘Ž’€ ”ˆ€‘Ž‚›• ‘ŽŽ™…ˆ‰ ‚ ‘Ž‘’€‚… ED503 Ž’ " STRING(NOW) SKIP
      "¥©á ü " mSeanceNum SKIP(1)
      "‘ä®à¬¨à®¢ ­ë á®®¡é¥­¨ï ED503 c í«¥ªâà®­­ë¬¨ ­®¬¥à ¬¨ ¢  ¤à¥á á«¥¤ãîé¨å "
      "¡ ­ª®¢" SKIP.
   
   FOR EACH Packet WHERE Packet.SeanceID EQ mSeanceID AND Packet.ParentID EQ 0
      NO-LOCK,
      FIRST signs WHERE signs.file-name EQ "Packet"
                    AND signs.surrogate EQ STRING (Packet.PacketID)
                    AND signs.code      EQ "SWBICRec"
      NO-LOCK,
      FIRST Reference WHERE Reference.PacketID EQ Packet.PacketID
      NO-LOCK:
      
      PUT STREAM mSRep UNFORMATTED SKIP(1)
         "EDNo: " Reference.RefValue SKIP
         "BIC SWIFT  ­ª  ¯®«ãç â¥«ï:" signs.code-value SKIP
         "‘¯¨á®ª á®®¡é¥­¨© SWIFT ¢ á®áâ ¢¥ ED503 " SKIP(1)
         "’ˆ ˆŒŸ ”€‰‹€            …”……‘                ‘“ŒŒ€ ŽŒ… "    SKIP
         "                                                       „ŽŠ“Œ…’€" SKIP
         "----------------------------------------------------------------" SKIP
      .
      FOR EACH bPacket WHERE bPacket.ParentID EQ Packet.PacketID
         NO-LOCK:
         ASSIGN
            mType   = GetXAttrValueEx ("Packet", STRING(bPacket.PacketID), "SWType", "")
            mFName  = GetXAttrValueEx ("Packet", STRING(bPacket.PacketID), "SWFileName", "")
            mRef    = GetXAttrValueEx ("Packet", STRING(bPacket.PacketID), "SWRef", "")
            mAmt    = DECIMAL(
                      GetXAttrValueEx ("Packet", STRING(bPacket.PacketID), "SWAmt", "0"))
            mDocNum = ""
         NO-ERROR.
         FOR EACH PackObject WHERE PackObject.PacketID  EQ bPacket.PacketID
                               AND PackObject.file-name EQ "op-entry"
            NO-LOCK,
            FIRST op WHERE op.op EQ INT64(ENTRY(1, PackObject.Surrogate))
            NO-LOCK:
            mDocNum = op.doc-num.
         END.
         PUT STREAM mSRep UNFORMATTED
            STRING (mType,   "X(3)") " "
            STRING (mFName,  "X(20)") " "
            STRING (mRef,    "X(16)") " "
            STRING (mAmt,    ">>>>>>>>9.99") " "
            STRING (mDocNum, "X(6)") SKIP
         .
   
      END.
   
   END.
   
	{preview.i &stream = " STREAM mSRep " &filename = mLogFName}

END.

{intrface.del}

/******************************************************************************/
/* $LINTUSER='VASOV' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='08/12/2014 12:07:19.832+04:00' */
/* $LINTFILE='snc-ed503-rep.p' */
/*prosignZPa97Z6WlAMfuqOMuaizSw*/