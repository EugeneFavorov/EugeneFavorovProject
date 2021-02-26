DEFINE STREAM dirlist.
DEFINE VARIABLE filename AS CHARACTER FORMAT "x(50)" NO-UNDO.
/*DEF VAR str AS longCHAR NO-UNDO.*/
DEF VAR str     AS CHAR NO-UNDO.
DEF VAR packt   AS CHAR NO-UNDO.
DEF VAR dayRead AS DATE NO-UNDO.

DEF TEMP-TABLE d182122
 FIELD direct AS CHAR.
 
DEF VAR fname AS CHAR.
DEF NEW SHARED STREAM vvs.

    
dayRead = TODAY - 3.
CREATE d182122.
d182122.direct = "/home2/bis/quit41d/imp-exp/0400/311p/in/arch/" + STRING(YEAR(dayRead)) + STRING(MONTH(dayRead),"99") + "/" + STRING(DAY(dayRead),"99").
dayRead = TODAY - 2.
CREATE d182122.
d182122.direct = "/home2/bis/quit41d/imp-exp/0400/311p/in/arch/" + STRING(YEAR(dayRead)) + STRING(MONTH(dayRead),"99") + "/" + STRING(DAY(dayRead),"99").
dayRead = TODAY - 1.
CREATE d182122.
d182122.direct = "/home2/bis/quit41d/imp-exp/0400/311p/in/arch/" + STRING(YEAR(dayRead)) + STRING(MONTH(dayRead),"99") + "/" + STRING(DAY(dayRead),"99").
dayRead = TODAY.
CREATE d182122.
d182122.direct = "/home2/bis/quit41d/imp-exp/0400/311p/in/arch/" + STRING(YEAR(dayRead)) + STRING(MONTH(dayRead),"99") + "/" + STRING(DAY(dayRead),"99").

PROCEDURE PacketTextSave:
   DEFINE INPUT PARAMETER iPacketID   AS INT64  NO-UNDO.
   DEFINE INPUT PARAMETER iResult     AS CHAR     NO-UNDO.

   DEFINE BUFFER PacketText FOR PacketText.
   DEFINE VAR vOrder AS INT64 NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PacketTextSave","iResult:" + iResult).
   &ENDIF

   IF iResult EQ "" THEN RETURN.

   DISABLE TRIGGERS FOR LOAD OF PacketText.

   FIND LAST PacketText USE-INDEX PackOrder WHERE
             PacketText.PacketID EQ iPacketID
             NO-LOCK NO-ERROR.
   vOrder = IF AVAILABLE(PacketText)
               THEN PacketText.Order + 1
               ELSE 1.

   CREATE PacketText.
   ASSIGN PacketText.PacketTextID = NEXT-VALUE(pack-id)
          PacketText.PacketID     = iPacketID
          PacketText.Order        = vOrder
          PacketText.Contents     = iResult.

END PROCEDURE.


/*{setdest.i}*/
/*
for each packettext where packettext.packetid = 7963349
EXCLUSIVE-LOCK:
    message packettext.packetid view-as alert-box.
    delete packettext.
END.
*/

fname = "/home2/bis/quit41d/log/311p/" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + ".log".
OUTPUT STREAM vvs TO VALUE (fname) APPEND.    

FOR EACH d182122 :
    INPUT STREAM dirlist FROM OS-DIR(d182122.direct).
    REPEAT:
        IMPORT STREAM dirlist filename.
        IF CAN-DO ("*.xml", filename)
        THEN 
        DO:
            FIND LAST reference WHERE reference.refvalue = filename AND reference.class-code EQ 'RTaxImp' NO-LOCK NO-ERROR.
            IF NOT AVAIL reference THEN NEXT.
            FIND LAST packet WHERE packet.mail-format = 'XMLFNS_R' 
                                AND packet.packetid = reference.packetid
                                NO-LOCK NO-ERROR.
            IF NOT AVAIL packet THEN NEXT.

            packt = ''.
            /*impchkcrd.p*/
            INPUT FROM VALUE (d182122.direct + '/' + filename) /*CONVERT TARGET "IBM866" SOURCE "1251"*/ .
            REPEAT ON ENDKEY UNDO,LEAVE:
                IMPORT UNFORMATTED str NO-ERROR.
                packt = packt + str.
            END.

            FIND FIRST packetText WHERE packetText.packetId EQ packet.packetid NO-LOCK NO-ERROR.
            IF NOT AVAIL packetText THEN
            DO:
                RUN PacketTextSave (packet.packetid,packt).
                PUT STREAM vvs UNFORMATTED "загружена информация по пакету " + STRING(packet.packetid) + " файл " + STRING(filename) SKIP.
            END.
            /*ELSE NEXT и так пройдёт*/
        END.
    END.
    INPUT CLOSE.
END.

OUTPUT STREAM vvs CLOSE.
DEF VAR strCom AS CHAR NO-UNDO.
strCom = "chmod 666 " + fname.
OS-COMMAND VALUE(strCom).






