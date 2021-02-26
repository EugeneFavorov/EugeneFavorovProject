/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pays.pro
      Comment: ������⥪� ��楤�� ������ � ���⥦�묨 ��⥬��� ������
   Parameters: ���
         Uses:
      Used BY: pp-pays.p
      Created: 23.04.2015 KMBIS TT:0236973 ������. �ਥ� ���⥦�� QIWI, ������,�ࠫᨡ, ���������
     Modified: 
*/

/*===============================================================================================*/
/*=== ��࠭�஢����� �८�ࠧ������ ��ப� � �����筮� ���祭�� ===============================*/
FUNCTION GetDecVal RETURNS DEC PRIVATE (INPUT iVal AS  CHAR,
                                        INPUT iDef AS  DEC):

DEF VAR vRes AS  DEC  NO-UNDO.

   vRes = DEC(iVal) NO-ERROR.

   IF ERROR-STATUS:ERROR OR (vRes EQ ?) THEN
     vRes = iDef.

   RETURN vRes.

END FUNCTION. /* GetDecVal */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���� ��७� ��� ��� ���� ============================================================*/
FUNCTION FndRef RETURNS LOG (INPUT  iClass   AS CHAR,
                             INPUT  iRef     AS CHAR,
                             OUTPUT oInfoMsg AS CHAR):
DEF VAR vFndRef  AS  LOG   NO-UNDO INIT NO.

DEF BUFFER bRef      FOR Reference.
DEF BUFFER bPack     FOR Packet.
DEF BUFFER bSeance   FOR Seance.

   oInfoMsg = "".
   /* �஢��塞 ��७� ��� ��� ��� */
   lFndRef:
   FOR FIRST bRef WHERE bRef.Class-Code  EQ iClass
                    AND bRef.RefValue    EQ iRef
                  NO-LOCK:
      vFndRef = YES.

      /* �� ������ �易�� � 1 ᥠ�ᮬ � ������ᨬ��� �� ���������� */
      FOR FIRST bPack WHERE bPack.PacketId EQ bRef.PacketId
                      NO-LOCK,
         FIRST bSeance WHERE bSeance.SeanceID EQ bPack.SeanceID
                       NO-LOCK:

         oInfoMsg = SUBST("����� ������ ����� '&1' �� &2 &3 (&4)", 
                          STRING(bSeance.Number),
                          STRING(bSeance.SeanceDate),
                          STRING(bSeance.SeanceTime, "HH:MM:SS"),
                          STRING(bSeance.op-kind)).
      END.
      LEAVE lFndRef.

   END. /* FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}] */

   RETURN vFndRef.

END FUNCTION. /* FndRef */
/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���ᨬ���� �ଠ� ��� 楫��� ⨯� �� ��� ��६���� ====================================*/
FUNCTION IntFmt RETURNS CHAR (INPUT iVal01  AS INT64,
                              INPUT iVal02  AS INT64):

DEF VAR vRes    AS  CHAR  NO-UNDO.
DEF VAR vMaxInt AS  INT64 NO-UNDO.

   ASSIGN
      iVal01 = 0 WHEN iVal01 EQ ?
      iVal02 = 0 WHEN iVal02 EQ ?
   .
   vMaxInt = MAX(LENGTH(STRING(iVal01)), 
                 LENGTH(STRING(iVal02))).

   IF vMaxInt LT 3 OR vMaxInt EQ ? THEN
      vMaxInt = 3.

   vRes = SUBST("&19", FILL(">", vMaxInt - 1)).

   RETURN vRes.

END FUNCTION. /* IntFmt */
/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���ᨬ���� �ଠ� ��� �����筮�� ⨯� �� ��� ��६���� ===============================*/
FUNCTION DecFmt RETURNS CHAR (INPUT iVal01  AS DEC,
                              INPUT iVal02  AS DEC):

DEF VAR vRes    AS  CHAR  NO-UNDO.


   vRes = SUBST("&1.99", IntFmt(INT64(iVal01), INT64(iVal02))). 

   RETURN vRes.

END FUNCTION. /* IntFmt */
/*===============================================================================================*/


/*===============================================================================================*/
/*=== ���塞 �� � ��६����� �� ���祭�� �� ����� ============================================*/
FUNCTION ReplTag RETURNS CHAR PRIVATE (INPUT iExch  AS  HANDLE,
                                       INPUT iStr   AS  CHAR,
                                       INPUT iPref  AS  CHAR):

DEF VAR vVal      AS  CHAR   NO-UNDO.        /* ���祭�� ��   */
DEF VAR vTag      AS  CHAR   NO-UNDO.        /* ��� ��        */

DEF BUFFER bAttr FOR xattr.

   FOR EACH bAttr WHERE bAttr.Class-Code EQ iExch::Class-code
                    AND NOT CAN-DO('class,group', bAttr.data-type)
                  NO-LOCK:

      /* ����砥� ���祭�� ४����� �� ����� */
      ASSIGN
         vVal = ""
         vVal = iExch:BUFFER-FIELD(bAttr.Xattr-Code):BUFFER-VALUE 
      NO-ERROR.

      /* �����塞 �� � ��६����� �� ��������� ���祭�� */
      ASSIGN
         vVal = fStrNvl(vVal, "")                             /* ���祭�� */
         vTag = SUBST("<#&1&2#>", iPref, bAttr.Xattr-Code)    /* ��      */
         iStr = REPLACE(iStr, vTag, vVal)
         vVal = ""
      .
   END. /* FOR EACH bAttr WHERE bAttr.Class-Code EQ iExch::Class-code */

   RETURN fStrNvl(iStr, "").

END FUNCTION. /* ReplTag */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���������� ᮤ�ঠ��� ����樨 �� ���祭��� �� �࠭ᯮ�⭮� ��� =========================*/
FUNCTION ParsDetails RETURNS CHAR (INPUT iExch  AS  HANDLE,
                                   INPUT iStr   AS  CHAR):

DEF VAR vKindMain AS  CHAR   NO-UNDO.        /* ��ப� � 㪠��⥫�� �� ����� � ����묨 ��������� */
DEF VAR vOBJMain  AS  HANDLE NO-UNDO INIT ?. /* �����⥫� ����� � ����묨 ���������             */

DEF BUFFER bAttr FOR xattr.

   IF VALID-HANDLE(iExch) THEN 
   DO:
      /* ����砥� 㪠��⥫� �� ��������� */
      vKindMain = iExch::ExchMain NO-ERROR.
     
      /* �᫨ �� ����, � ��ᢠ����� 㪠��⥫� ��६����� */
      IF {assigned vKindMain} THEN
         vOBJMain = WIDGET-HANDLE(vKindMain).
     
      /* �᫨ 㪠��⥫� �ᯥ譮 ����祭, � �஡������ �� ४����⠬ ��������筮�� ����� */
      IF VALID-HANDLE(vOBJMain) THEN 
         iStr = ReplTag(vOBJMain, iStr, "HEAD:").
     
      /* �஡������ �� ४����⠬ ��।������ ����� */
      iStr = ReplTag(iExch, iStr, "").

   END. /* IF VALID-HANDLE(iExch) THEN */

   RETURN fStrNvl(iStr, "").

END FUNCTION. /* ParsDetails */

/*===============================================================================================*/

