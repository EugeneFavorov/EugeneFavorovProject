/*              
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: SNC-IED503-REP.P
      Comment: �뢮� ��⮪��� ������ ������� ᮮ�饭�� SWIFT
   Parameters: iSeanceID - �����䨪��� ᥠ��
         Uses:
      Used BY:
      Created: 01.12.2014 VASOV
     Modified: 
*/

{globals.i}
{intrface.get filex}

DEFINE INPUT  PARAMETER iSeanceID AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mSeanceID  AS INT64       NO-UNDO.
DEFINE VARIABLE mLogFName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mLogDir    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmp       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mSeanceNum AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mType      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mFName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mRef       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mAmt       AS DECIMAL     NO-UNDO.

DEFINE STREAM mSRep.

DEFINE BUFFER bPacket FOR Packet.

mSeanceID = INT64(iSeanceID) NO-ERROR.
FOR EACH Packet WHERE TRUE 
   AND Packet.SeanceID   EQ mSeanceID
   AND Packet.ParentID   EQ 0
   AND Packet.FileExchID GT 0
   NO-LOCK QUERY-TUNING(NO-INDEX-HINT):
   LEAVE.
END.
/*FIND FIRST Packet WHERE Packet.SeanceID   EQ mSeanceID*/
/*                    AND Packet.ParentID   EQ 0        */
/*                    AND Packet.FileExchID GT 0        */
/*   NO-LOCK NO-ERROR.                                  */
IF AVAIL Packet THEN
DO:
   IF NOT CatalogGetBoth(Packet.mail-user-num,
                         "LogArch",
                         OUTPUT mLogDir,
                         OUTPUT mTmp)
      THEN mLogDir = ".".
   mLogDir   = RIGHT-TRIM(mLogDir, "/") + "/".
   mLogFName = mLogDir + "IED503-" + STRING(mSeanceID, "999999999") + ".log".

   FIND FIRST Seance WHERE Seance.SeanceID EQ mSeanceID NO-LOCK NO-ERROR.
   IF AVAIL Seance THEN
   DO:
      mSeanceNum = Seance.Number.

      {setdest.i &stream = " STREAM mSRep " &filename = mLogFName}

      PUT STREAM mSRep UNFORMATTED
      "�������� ������� ���������� ��������� � ������� ED503 �� " STRING(NOW)
      SKIP
      "���� � " mSeanceNum SKIP(1)
      "����祭� ᮮ�饭�� ED503 c ���஭�묨 ����ࠬ� �� ᫥����� ������"
      SKIP.

      FOR EACH Packet WHERE Packet.SeanceID    EQ mSeanceID 
                     AND Packet.mail-format BEGINS "XML-ED503"
      NO-LOCK,
      FIRST signs WHERE signs.file-name EQ "Packet"
                    AND signs.surrogate EQ STRING (Packet.PacketID)
                    AND signs.code      EQ "SWBICSend"
      NO-LOCK,
      FIRST Reference WHERE Reference.PacketID EQ Packet.PacketID
      NO-LOCK:
   
         PUT STREAM mSRep UNFORMATTED SKIP(1)
         "EDNo: " ENTRY(2, Reference.RefValue, "|")      SKIP
         "BIC SWIFT ����� ��ࠢ�⥫�:" signs.code-value SKIP(1)
         "���᮪ ᮮ�饭�� SWIFT � ��⠢� ED503 " SKIP(1)
         "��� ��� �����            ��������                �����" SKIP
         "------------------------------------------------------" SKIP
         .
         FOR EACH bPacket WHERE bPacket.ParentID EQ Packet.PacketID
         NO-LOCK:
         ASSIGN
            mType   = GetXAttrValueEx
                         ("Packet", STRING(bPacket.PacketID), "SWType", "")
            mFName  = GetXAttrValueEx
                         ("Packet", STRING(bPacket.PacketID), "SWFileName", "")
            mRef    = GetXAttrValueEx
                         ("Packet", STRING(bPacket.PacketID), "SWRef", "")
            mAmt    = DECIMAL(GetXAttrValueEx
                         ("Packet", STRING(bPacket.PacketID), "SWAmt", "0"))
         NO-ERROR.
         PUT STREAM mSRep UNFORMATTED
            STRING (mType,   "X(3)") " "
            STRING (mFName,  "X(20)") " "
            STRING (mRef,    "X(16)") " "
            STRING (mAmt,    ">>>>>>>>9.99") SKIP
         .

         END. /*FOR EACH bPacket*/

      END. /*FOR EACH Packet*/

      OUTPUT STREAM mSRep CLOSE.
   END. /*IF AVAIL Seance*/

END. /*IF AVAIL Packet*/


{intrface.del}

/******************************************************************************/
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='24/12/2014 11:01:25.024+04:00' */
/* $LINTFILE='snc-ied503-rep.p' */
/*prosign8+0ozVh0P/IvHHNkA5beMg*/