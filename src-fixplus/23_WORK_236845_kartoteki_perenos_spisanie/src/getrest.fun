/*
               KSV Editor
    Copyright: (C) 2000-2006 Serguey Klimoff (bulklodd)
     Filename: GETREST.FUN
      Comment: �㭪�� ��� �࠭���樨 cart2emb � ��㧥� ���-⮢ �� ����⥪�
   Parameters:
         Uses:
      Used by:
      Created: 22.09.2010 10:50 M.SEMENO
     Modified: 22.09.2010 10:50 M.SEMENO <comment>
*/

/*-------------------------------------------*/
/* �㭪�� ���� ���⪠ �� ���� � ��⮬ �����஢��� ���.
   �᫨ ���⮪ ����� ���, ��� �ந��諠 �訡��, � �����頥��� 0 */
/*-------------------------------------------*/
FUNCTION GetRest RETURNS DECIMAL (
    INPUT iKau        AS RECID,
    INPUT iOpDateTime AS DATETIME):

   DEFINE VARIABLE vCurrency     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAmount       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vAccessMask   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAccessStatus AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlockList    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpBalChar    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcctDB       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vIsBudget     AS LOGICAL   NO-UNDO.

   DEFINE BUFFER acct FOR acct.
   DEFINE BUFFER BlockObject FOR BlockObject.
   DEFINE BUFFER kau FOR kau.
   DEFINE BUFFER op-entry FOR op-entry.
   DEFINE BUFFER op FOR op.
   DEFINE BUFFER bop FOR op.

   FIND FIRST kau WHERE
              RECID(kau) EQ iKau
       NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kau THEN
       RETURN 0.

   FIND FIRST op-entry WHERE 
              op-entry.op       EQ INT(ENTRY(1,kau.kau))
          AND op-entry.op-entry EQ INT(ENTRY(2,kau.kau))
       NO-LOCK NO-ERROR.
   IF NOT AVAILABLE op-entry THEN
       RETURN 0.
   FIND FIRST op OF op-entry 
       NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN
       RETURN 0.

   /*���������� �������� ���������*/
   vOpBalChar = GetXAttrValueEx("op",
                                STRING(op.op),
                                "op-bal",
                                "").
   IF vOpBalChar NE "" THEN
      FIND FIRST bop WHERE 
                 bop.op EQ INTEGER(vOpBalChar) 
       NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST bop WHERE 
                 bop.op-transaction EQ op.op-transaction
             AND bop.acct-cat       EQ "b"
             AND RECID(bop)         NE RECID(op)
         NO-LOCK NO-ERROR.

    vAcctDB = GetXattrValueEX("op",STRING(IF AVAIL bop THEN bop.op ELSE op.op),"acctbal","").

    vCurrency = SUBSTRING(vAcctDB,6,3).
    IF vCurrency EQ "810" THEN
        vCurrency = "".

    FIND FIRST acct WHERE
               acct.acct     EQ vAcctDB
           AND acct.currency EQ vCurrency
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE acct THEN
        RETURN 0.

    vAccessMask   = FGetSetting("�⠭���", "AccessAcct", "").
    vAccessStatus = FGetSetting("�⠭���", "AccessStatus", "�").

    IF {assigned vAccessMask} AND CAN-DO(vAccessMask,acct.acct) THEN
    DO:
        RUN CalcAvailPos(acct.acct,
                         acct.currency,
                         iOpDateTime,
                         iOpDateTime,
                         vAccessStatus,
                         "�",
                         "acct-pos",
                         NO,
                         "*",
                         NO,
                         OUTPUT sh-bal,
                         OUTPUT sh-val).
    END.
    ELSE
    DO:
        RUN acct-pos IN h_base (acct.acct,
                                acct.currency,
                                iOpDateTime,
                                iOpDateTime,
                                "�").
    END.

    vAmount = IF acct.currency EQ "" THEN sh-bal ELSE sh-val.

    IF    (acct.side EQ "�" AND vAmount LT 0)
       OR (acct.side EQ "�" AND vAmount GT 0) THEN 
        vAmount = 0.
    ELSE
        vAmount = ABSOLUTE(vAmount).

    RUN IsBudgetPayment(bop.op, OUTPUT vIsBudget).

       /* �� ����祭��� �㬬� ���⠥� �����஢����� �㬬� */
    FOR EACH  BlockObject WHERE
              BlockObject.class-code   EQ "BlockAcct"
         AND  BlockObject.file-name    EQ "acct"
         AND  BlockObject.surrogate    EQ acct.acct + "," + acct.currency
         AND  BlockObject.beg-datetime LE iOpDateTime
         AND (BlockObject.end-datetime EQ ?
           OR BlockObject.end-datetime GE iOpDateTime)
         AND (BlockObject.txt[1] EQ "" OR (NOT CAN-DO(BlockObject.txt[1],bop.order-pay) AND NOT vIsBudget) )
        NO-LOCK:
       IF CAN-DO("����,������,������",BlockObject.block-type) THEN
           RETURN 0.
       IF BlockObject.block-type EQ "�����㬬" AND BlockObject.val[3] NE ? THEN
           vAmount = vAmount + BlockObject.val[3].    /* � BlockObject.val[3] �࠭���� ����⥫�� ���祭�� */
    END.
       
    IF vAmount LT 0 THEN
        vAmount = 0.

    RETURN vAmount.

END FUNCTION.
