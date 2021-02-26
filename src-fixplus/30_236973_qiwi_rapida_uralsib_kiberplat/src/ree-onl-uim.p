/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ree-onl-uim.p
      Comment: Процедура отмены импорта реестра платежей из онлайн-систем
   Parameters: iParam, BUFFER Seance
      Created: 13.05.2015 KMBIS TT:0236973 Миграция.Прием платежей QIWI, Рапида, Уралсиб, КиберПлат
                                Измененная версия pck-op-uim.p, удаляются только документы 
                                созданные в сеансе импорта.
     Modified:
*/
{globals.i}

DEF INPUT PARAM  iParam  AS  CHAR  NO-UNDO.
DEF PARAM BUFFER Seance  FOR Seance.

{exchange.equ}

DEF VAR mClass     AS  CHAR   NO-UNDO.
DEF VAR mPacketID  AS  INT64  NO-UNDO.
DEF VAR mRetVal    AS  CHAR   NO-UNDO.
DEF VAR mStatOBJ   AS  CHAR   NO-UNDO.
DEF VAR mMsg       AS  CHAR   NO-UNDO.
DEF VAR mOp        AS  INT64  NO-UNDO.
DEF VAR mFlagSet   AS  LOG    NO-UNDO INIT ?.

&SCOP NO-BASE-PROC YES

{intrface.get kau}
{intrface.get strng}
{intrface.get tmess}
{intrface.get rights}
{intrface.get xclass}

{pck-op-del.pro}
/*============================================================================*/
MAIN:
DO TRANSACTION ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   IF NUM-ENTRIES(iParam,';') EQ 3 THEN
      ASSIGN
         mClass      = ENTRY(1,iParam,';')
         mPacketID   = INT64(ENTRY(2,iParam,';'))
      NO-ERROR.
   ELSE ASSIGN
           mClass    = iParam
           mPacketID = ?.
     
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ree-onl-uim.p","1 Seance.SeanceID:" + string(Seance.SeanceID) +
                                     " Seance.Direct:" + Seance.Direct +
                                    " Seance.Op-Kind:" + Seance.op-kind +
                                            " iClass:" + mClass).
   &ENDIF

   IF NOT GetTablePermission("op","d") THEN DO:
      RUN Fill-SysMes IN h_tmess("","ExchRKC44","","%s=" + USERID('bisquit')).
      mRetVal = "Нет прав на удаление документов".
      UNDO MAIN, RETRY MAIN.
   END.

IF mPacketID EQ ? THEN
LOOP:
   FOR EACH Packet WHERE
            Packet.SeanceID   EQ Seance.SeanceID
        AND Packet.Class-Code EQ mClass
            NO-LOCK,
       EACH PackObject WHERE
            PackObject.PacketID  EQ     Packet.PacketID
        AND PackObject.File-Name BEGINS "op"
            NO-LOCK,
       EACH op WHERE
            op.op EQ INT64(ENTRY(1,PackObject.Surrogate))
            NO-LOCK:

      mStatOBJ = GetXAttrValue("Packet",         /* статус после импорта      */
                               string(Packet.PacketID),
                               "StateOBJ").

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ree-onl-uim.p","4 op.op:" + string(op.op)           +
                                  " op.op-date:" + string(op.op-date)      +
                                  " op.doc-num:" + string(op.doc-num)      +
                                    " PacketID:" + string(Packet.PacketID) +
                                    " mStatOBJ:" + GetNullStr(mStatOBJ)).
      &ENDIF

      IF {assigned mStatOBJ}       AND          /* Статус не изменен         */
          op.op-status EQ mStatOBJ THEN DO:

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ree-onl-uim.p","5 DELETE op.op:" + string(op.op)).
         &ENDIF
         
         mop = op.op .

         RUN OperationDelete IN THIS-PROCEDURE (mop) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            mMsg = SUBST("%s=&1,%s=&2,%s=&3,%i=&4",
                         Seance.op-kind,
                         op.doc-num,
                         STRING(op.op-date),
                         STRING(PackObject.PacketID)).
            RUN Fill-SysMes IN h_tmess("","ExchRKC08","", mMsg).
            mRetVal = ERROR-STATUS:get-message(1).
            UNDO MAIN, RETRY MAIN.
         END.    
      END.
      ELSE IF {assigned mStatOBJ} THEN /* Статус изменен */
      DO:
         mMsg = SUBST("%s=&1,%s=&2,%s=&3,%s=&4",
                      TRIM(op.doc-num),
                      GetNullDat(op.op-date),
                      mStatOBJ,
                      op.op-status).
         RUN Fill-SysMes IN h_tmess("","ComnExc94","", mMsg).
         mRetVal = "Документ изменил статус".
         LEAVE MAIN.
      END.     
   END.
   ELSE DO:  /* Если передан PacketID, откатываем только для одного сообщения */

      FOR EACH PackObject WHERE
               PackObject.PacketID  EQ     mPacketID
           AND PackObject.File-Name BEGINS "op"
               NO-LOCK,
          EACH op WHERE
               op.op EQ INT64(ENTRY(1,PackObject.Surrogate))
               NO-LOCK:

         mStatOBJ = GetXAttrValue("Packet",         /* статус после импорта      */
                                  string(mPacketID),
                                  "StateOBJ").
 
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("ree-onl-uim.p","4 op.op:" + string(op.op)           +
                                    " op.op-date:" + string(op.op-date)      +
                                    " op.doc-num:" + string(op.doc-num)      +
                                      " PacketID:" + string(mPacketID) +
                                      " mStatOBJ:" + GetNullStr(mStatOBJ)).
         &ENDIF

         IF {assigned mStatOBJ}       AND          /* Статус не изменен         */
            op.op-status EQ mStatOBJ THEN DO:
     
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("ree-onl-uim.p","5 DELETE op.op:" + string(op.op)).
            &ENDIF
            mop = op.op.

            RUN OperationDelete IN THIS-PROCEDURE(mop) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               mMsg = SUBST("%s=&1,%s=&2,%s=&3,%i=&4",
                            Seance.op-kind,
                            op.doc-num,
                            STRING(op.op-date),
                            STRING(PackObject.PacketID)).
               RUN Fill-SysMes IN h_tmess("","ExchRKC08","", mMsg).
               mRetVal = ERROR-STATUS:get-message(1).
               UNDO MAIN, RETRY MAIN.
            END.
         END.
         ELSE IF {assigned mStatOBJ} THEN DO:
            mMsg = SUBST("%s=&1,%s=&2,%s=&3,%s=&4",
                         TRIM(op.doc-num),
                         GetNullDat(op.op-date),
                         mStatOBJ,
                         op.op-status).
            RUN Fill-SysMes IN h_tmess("","ComnExc94","", mMsg).
            mRetVal = "Документ изменил статус".
            LEAVE MAIN.
         END. 
      END. 
   END. 

   mFlagSet = YES.
END.

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("ree-onl-uim.p","6 mFlagSet:" + string(mFlagSet)).
&ENDIF

{intrface.del}
{doreturn.i mFlagSet mRetVal}

