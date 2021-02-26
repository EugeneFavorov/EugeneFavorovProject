/*              
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: PCK-ESW.P
      Comment: Метод Update класса PRQSW
   Parameters: iClass - класс
               iPack  - хэндл временной таблицы
         Uses:
      Used BY:
      Created: 26.11.2014 VASOV
     Modified: 
*/

{form.def}
{globals.i}
{exchange.equ}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get pbase}
{intrface.get trans}
{intrface.get exch}
{intrface.get epack}
{intrface.get pack}

&SCOPED-DEFINE NO-BASE-PROC YES

DEFINE INPUT PARAMETER iClass AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iPack  AS HANDLE    NO-UNDO.

DEFINE VARIABLE mFlagSet  AS LOGICAL   NO-UNDO INIT ?.
DEFINE VARIABLE mKind     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParentID AS INT64     NO-UNDO.
DEFINE VARIABLE mPacketID AS INT64     NO-UNDO.
DEFINE VARIABLE mHPack    AS HANDLE    NO-UNDO.
DEFINE VARIABLE mHDoc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE mHMain    AS HANDLE    NO-UNDO.
DEFINE VARIABLE mHQry     AS HANDLE    NO-UNDO.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   ASSIGN
      mHPack  = iPack:DEFAULT-BUFFER-HANDLE
      mKind   = mHPack::Kind
      mHDoc   = ObjectValueHandle ("ExchRKCSw")
      mHMain  = ObjectValueHandle (mKind)
   NO-ERROR. {&ON-ERROR}

   RUN PacketXMLCreate IN h_epack (iClass, iPack) NO-ERROR. {&ON-ERROR}
   mParentID = mHPack::PacketID.

   FIND FIRST Packet WHERE Packet.PacketID EQ mParentID
      NO-LOCK NO-ERROR. {&ON-ERROR}
   UpdateSigns(Packet.Class-Code, STRING(mParentID), "SWBICRec", 
               mHMain::bank-code-rec, ?).

   CREATE QUERY mHQry.
   mHQry:ADD-BUFFER(mHDoc).
   mHQry:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE &1.__ID GT 0 " + 
                                                "AND &1.__UpID EQ &2 " +
                                                "AND &1.BadErrors EQ '' " +
                                  "NO-LOCK ",
                                   mHDoc:NAME, STRING(mHMain::__ID))).
   mHQry:QUERY-OPEN().
   mHQry:GET-FIRST().
   DO WHILE NOT mHQry:QUERY-OFF-END :

      RUN PacketCreate IN h_pack (mHPack::SeanceID,
                                  -1,
                                  mHPack::mail-user-num,
                                  mHDoc:NAME,
                                  OUTPUT mPacketID) NO-ERROR. {&ON-ERROR}
     
      RUN PacketInclude IN h_pack (mPacketID, 
                                   mParentID, 
                                   "ОТПР") NO-ERROR. {&ON-ERROR}

      IF mHDoc::Surrogate GT "" THEN DO:
         RUN PacketCreateLink IN h_pack (mPacketID, 
                                         mHDoc::File-Name,
                                         mHDoc::Surrogate,
                                         "ED503") NO-ERROR. {&ON-ERROR}
      END.

      FIND FIRST Packet WHERE Packet.PacketID EQ mPacketID
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR. {&ON-ERROR}
      Packet.mail-format = mHDoc::mail-format.
      UpdateSigns (Packet.Class-Code, STRING(mPacketID), "SWFileName",
                   mHDoc::FName, ?).
      UpdateSigns (Packet.Class-Code, STRING(mPacketID), "SWType",
                   mHDoc::DType, ?).
      UpdateSigns (Packet.Class-Code, STRING(mPacketID), "SWAmt",
                   STRING(mHDoc::amt-rub), ?).
      UpdateSigns (Packet.Class-Code, STRING(mPacketID), "SWRef",
                   mHDoc::Ref, ?).
      RELEASE Packet NO-ERROR.

      mHQry:GET-NEXT().
   END.
   
   mHQry:QUERY-CLOSE().
   DELETE OBJECT mHQry NO-ERROR.

   mFlagSet = YES.

END.

{intrface.del}
{doreturn.i mFlagSet}

/******************************************************************************/
/* $LINTUSER='VASOV' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='08/12/2014 12:04:15.724+04:00' */
/* $LINTFILE='pck-esw.p' */
/*prosign8/BiiCX7l4rQmcYIj3J7jQ*/