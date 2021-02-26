{globals.i}

DEFINE BUFFER op FOR op.
DEFINE BUFFER Packet FOR Packet.
DEFINE BUFFER Packet1 FOR Packet.
DEFINE BUFFER PackObject FOR PackObject.


FOR EACH op 
   WHERE 
/*   op.op EQ 26029061  */
   op.op-date GE TODAY - 2
   AND op.op-date LE TODAY 
   AND op.op-kind BEGINS "i-ed11"
   NO-LOCK:

   FOR EACH PackObject WHERE
      PackObject.file-name EQ 'op-entry'
      AND PackObject.Surrogate EQ STRING(op.op) + ",1"
      NO-LOCK: 
      FIND FIRST Packet 
         WHERE Packet.PacketID EQ PackObject.PacketID
         AND Packet.mail-format EQ "XML-ED104"
         NO-LOCK NO-ERROR.
         IF AVAILABLE(Packet) THEN
         DO:
            FIND FIRST Packet1
               WHERE RECID(Packet1) EQ RECID(Packet)
               EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE(Packet1) THEN
               Packet1.mail-format = "XML-ED11" + (IF op.op-kind EQ "i-ed113" THEN "3" ELSE "4").
         END.
   END.
END.

