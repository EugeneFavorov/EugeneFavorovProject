/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pfrotch1.fun
      Comment: 0236841 ������. ����㧪� ॥��� ���᫥��� ���ᨩ �� �� + ���� �� ���ᨮ��ࠬ
   Parameters: ���
         Uses:
      Used by: pfrotch1.p
      Created: 05/11/2015 KMBIS 0236841 ������. ����㧪� ॥��� ���᫥��� ���ᨩ �� ��
                                        ���� �� ���ᨮ��ࠬ
     Modified: 
                                    
*/

/*================================================================================================*/
/*=== �� ���㬥��� ��室�� ������������ �� ��ࠢ�⥫� ===========================================*/
FUNCTION GetPfrName RETURNS CHAR(INPUT iOp   AS INT64,
                                 INPUT iCode AS CHAR):

DEF VAR vPFRName  AS  CHAR  NO-UNDO.

DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bPack     FOR Packet.

   IF    iOp EQ ?
      OR iOp LT 1
      OR NOT {assigned iCode} 
   THEN 
      RETURN "".

   FOR FIRST bPackObj WHERE bPackObj.file-name EQ "op-entry"
                        AND bPackObj.Surrogate EQ SUBST("&1,1", STRING(iOp))
                      NO-LOCK:
      vPFRName = GetXAttrValueEx("Packet", STRING(bPackObj.PacketId), "RegPFR", "").
      IF {assigned vPFRName} THEN 
          vPFRName = GetCodeNameEx(iCode, vPFRName, "").

   END. /* FOR FIRST bPackObj WHERE bPackObj.file-name EQ "op-entry" */

   RETURN fStrNvl(vPFRName, "").

END FUNCTION. /* GetPfrName */

/*================================================================================================*/

/*================================================================================================*/
/*=== �஢��塞 ����७����� �� ᮮ⢥�⢨� ����� �⡮� =====================================*/
FUNCTION ChkProxy RETURNS LOG(INPUT iRowId AS ROWID,
                              INPUT iAcct  AS CHAR):

DEF VAR vRes      AS LOG   NO-UNDO INIT NO.
DEF VAR vNextYear AS DATE  NO-UNDO.
DEF VAR vAcctMask AS CHAR  NO-UNDO.

DEF BUFFER bProxy     FOR loan.
DEF BUFFER bLoanTrans FOR loan-transaction.
   lProxyChk:
   FOR FIRST bProxy WHERE ROWID(bProxy) EQ iRowId
                    NO-LOCK:
      ASSIGN
         vNextYear = GoMonth(bProxy.open-date, 12)
      .

      /*=== ����७����� ������ ���� �뤠�� ����� 祬 �� 1 ���  ===*/
      IF bProxy.end-date LE vNextYear THEN 
         LEAVE lProxyChk.

      IF bProxy.class-code EQ "proxy-base" THEN
      DO:
         /*=== ����७����� ��� ===*/
         IF bProxy.deal-type EQ YES THEN
         DO:
            /*=== �뤠�� ����ࠫ쭠� ����७����� ===*/
            vRes = YES.
            LEAVE lProxyChk.
         END. /* IF bProxy.deal-type EQ YES THEN */

         /*=== ����७����� �� ����ࠫ쭠�, �஢��塞 �ਭ���������� � ���� ===*/
         FIND FIRST bLoanTrans WHERE bLoanTrans.contract  EQ 'proxy'     
                                 AND bLoanTrans.cont-code EQ bProxy.cont-code
                                 AND bLoanTrans.prolong   GT 0
                               NO-LOCK NO-ERROR.

         IF NOT AVAIL(bLoanTrans) THEN
         DO:
            /*=== ����७����� ��������� ��� ��� ��⮢ �� ������� ===*/
            /*=== �஢��� �� ��������� ��᫥����⥫쭮 ==============*/
            FOR EACH bLoanTrans WHERE bLoanTrans.contract  EQ "proxy"
                                  AND bLoanTrans.cont-code EQ bProxy.cont-code 
                                NO-LOCK:
               ASSIGN
                  vAcctMask = GetXattrValueEx("loan-transaction",
                                              SUBST("&1,&2,&3",
                                                    bLoanTrans.contract,
                                                    bLoanTrans.cont-code,
                                                    bLoanTrans.trans-code),
                                              "proxy-acct",
                                              "").
                  /*=== ����७����� ���室�� ��� ��� ��� ===*/
                  vRes      = CAN-DO(vAcctMask, iAcct)
               .
               IF vRes THEN 
                  LEAVE lProxyChk.
            END. /* FOR EACH bLoanTrans WHERE bLoanTrans.contract  EQ "proxy" */
         END. /* IF NOT AVAIL(bLoanTrans) THEN */

      END. /* IF bProxy.class-code EQ "proxy-base" THEN */
      ELSE 
      DO:
         /*=== ���筠� ����७����� ===*/
         ASSIGN
            vAcctMask = GetXAttrValueEx("loan", 
                                        SUBST("proxy,&1", bProxy.cont-code), 
                                        "loan-allowed", 
                                        "")
            vRes      = CAN-DO(vAcctMask, iAcct)
         .
      END. /* IF bProxy.class-code EQ "proxy-base" THEN ... ELSE */
   END. /* FOR FIRST bProxy WHERE ROWID(bProxy) EQ iRowId */

   RETURN vRes.

END FUNCTION. /* ChkProxy */

/*================================================================================================*/

/*================================================================================================*/
/*=== ��� ����७���� ��� =======================================================================*/
FUNCTION AgentName RETURNS CHAR(iContCode AS CHAR):

DEF VAR vAgentId  AS INT64  NO-UNDO.
DEF VAR vRes      AS CHAR   NO-UNDO.

DEF BUFFER bPers  FOR person.

   vAgentId = INT64(GetXAttrValueEx("loan", SUBST("proxy,&1",iContCode), "agent-id","0")) NO-ERROR.

   IF vAgentId NE ? AND vAgentId GT 0 THEN
      FOR FIRST bPers WHERE bPers.person-id EQ vAgentId 
                      NO-LOCK:
         ASSIGN
            vRes = SUBST("&1 &2", TRIM(bPers.name-last), TRIM(bPers.first-names))
            vRes = RemoveDoubleChars(vRes, " ")
         .
      END. /* FOR FIRST bPers WHERE bPers.person-id EQ vAgentId */

   RETURN fStrNvl(vRes, "").

END FUNCTION. /* AgentName */

/*================================================================================================*/

/*================================================================================================*/
/*=== ��� ��᫥����� ���饭�� ��� ᮢ��襭�� ����権 �� ��⠬ ================================*/
FUNCTION LastMove RETURNS CHAR(INPUT iAcct AS CHAR):

DEF VAR vRes    AS CHAR  NO-UNDO.
DEF VAR vProxy  AS CHAR  NO-UNDO.

DEF BUFFER bOpEntry FOR op-entry.

   lOpFnd:
   FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ iAcct
                       AND bOpEntry.op-status GE CHR(251)
                     NO-LOCK 
                     BY bOpEntry.op-date DESC:
      vProxy = GetXAttrValueEx("op", STRING(bOpEntry.op), "proxy-code", "").
      IF NOT {assigned vProxy} THEN
      DO:
         vRes = STRING(bOpEntry.op-date, "99.99.9999").
         LEAVE lOpFnd.

      END. /* IF NOT {assigned vProxy} THEN */
   END. /* FOR EACH bOpEntry WHERE bOpEntry.acct-db   EQ iAcct */

   RETURN vRes.

END FUNCTION. /* LastMove */

/*================================================================================================*/
