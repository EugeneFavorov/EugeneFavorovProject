
mStatOBJ = GetXAttrValue("Packet",         /* ����� �� �ᯮ��        */
                         string(PackObject.PacketID),
                         "StateOBJ").

&IF DEFINED(IS-DEBUG) &THEN
RUN dbgprint.p ("pck-op-uex.p","op.op:" + string(op.op)               +
                         " op.op-date:" + string(op.op-date)          +
                         " op.doc-num:" + string(op.doc-num)          +
                       " op.op-status:" + string(op.op-status)        +
                           " PacketID:" + string(PackObject.PacketID) +
                           " mStatOBJ:" + mStatOBJ).
&ENDIF

IF {assigned mStatOBJ} THEN DO:
                                           /* ����� ��᫥ �ᯮ��     */
   mStatDone = ENTRY(1,GetCode("�����",mStatOBJ)).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("OperationUndoExport","vStatDone:" + mStatDone).
   &ENDIF

   IF op.op-status EQ mStatDone OR
      (AVAIL bigPacket AND
       CAN-DO("*ED273,*ED274,*ED503,*ED503BSP",bigPacket.mail-format)
      )

   THEN DO:   /* ���㬥�� �� �� �������   */
      {opent-st.i &status=mStatOBJ}        /* ��������� ����ᮢ        */
   END.
   ELSE DO:
      RUN Fill-SysMes("","ExchRKC40","","%s=" + GetNullStr(op.doc-num)   +
                                        "%s=" + GetNullDat(op.op-date)   +
                                        "%s=" + GetNullStr(op.op-status) +
                                        "%s=" + GetNullStr(mStatDone)).
      UNDO MAIN, RETRY MAIN.
   END.
END.
/* $LINTUSER='VASOV' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='08/12/2014 11:56:31.225+04:00' */
/* $LINTFILE='pck-op-uex.i' */
/*prosignT/LM74zMLbzhwUfM2HlTgQ*/