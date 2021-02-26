/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-BLKOB.P
      Comment: �㭪樨 � ��楤��� �� ࠡ�� � �����஢���묨 ��ꥪ⠬�.
   Parameters: ���
         Uses:
      Used by:
      Created: 27.11.2007 ilvi
     Modified: 07.06.2008 MUTA 0094144 �㭪�� ChkBlkCrd    
     Modified: 08/07/2008 kraw (0094567) ������⢥��� �����஢��. �㬬�஢����.
     Modified: 15/01/2009 kraw (0095716) CalcFreeOstOnBlockAcct, GetBlockPositionAll
     Modified: 16/06/2009 kraw (0112596) �࠭��� �᫮��� (ࠢ���⢮ �㬬�) �� �����஢���
     Modified: 20/05/2011 kraw (0145067) GetBlkType
*/

{pfuncdef
 &DefLib="BLKOB" 
 &Description="�㭪樨 � ��楤��� �� ࠡ�� � �����஢���묨 ��ꥪ⠬�"}
 
{globals.i}             /* �������� ��६���� ��ᨨ. */
{sh-defs.i}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get op}
{intrface.get crd}    

&GLOBAL-DEFINE ON-ERROR IF ERROR-STATUS:ERROR THEN UNDO MAIN, RETRY MAIN.

&SCOPED-DEFINE ORDER-PAY-BUDGET "5"

/*
   �����頥� ᯨ᮪ ����� �����஢�� �� ���� � ��⮬ ��।���� ���⥦�.
   �� 㪠����� ���񤭮�� "-1" ������ ⮫쪮 � �����஢��, �����
   �� ����᪠�� �஢������ ���⥦�� �� �� ����� �� ���񤭮��.
*/
FUNCTION BlckAcctOrdPay RETURN CHARACTER (INPUT iSurrogate AS CHARACTER,
                                          INPUT iDateTime  AS DATETIME,
                                          INPUT iOrderPay  AS CHARACTER
                                          ).
   DEFINE VARIABLE vBlockList AS CHARACTER NO-UNDO.
   DEF BUFFER acct FOR acct. /* ���������� ����. */
   
   bl:
   DO:
      {find-act.i
         &acct = ENTRY(1,iSurrogate,',')
         &curr = ENTRY(2,iSurrogate,',')
      } 
      IF NOT AVAIL acct THEN
         LEAVE bl.
      IF iDateTime EQ ? THEN
      iDateTime = NOW.
      
      BLK_OBJ:
      FOR EACH  BlockObject WHERE 
                BlockObject.class-code   EQ 'BlockAcct'
            AND BlockObject.FILE-NAME    EQ 'acct'
            AND BlockObject.surrogate    EQ iSurrogate
      NO-LOCK:
         /*
            0200763: �᫮��� �� ��ਮ� ����⢨� �����஢�� ��७�ᥭ�
            ������ 横��. � ��⨢��� ��砥 �� �������� ������
            OE DataServer �������� �訡�� (Memory Violation).
         */
         IF BlockObject.beg-datetime > iDateTime OR
            BlockObject.end-datetime < iDateTime
         THEN
            NEXT BLK_OBJ.
         IF iOrderPay = "*"
            OR
            iOrderPay = "-1" AND
            NOT {assigned BlockObject.txt[1]}
            OR
            iOrderPay <> "*"  AND
            iOrderPay <> "-1" AND
            NOT ({assigned BlockObject.txt[1]} AND CAN-DO(BlockObject.txt[1], iOrderPay))
         THEN DO:
            
            {additem.i vBlockList BlockObject.block-type}
         END.
      END.
   END.
   RETURN vBlockList.
END FUNCTION.

/* �����頥� ᯨ᮪ ����� �����஢�� �� ���� */
FUNCTION BlockAcct RETURN CHARACTER (INPUT iSurrogate AS CHARACTER,
                                     INPUT iDateTime  AS DATETIME
                                    ).

   DEFINE VARIABLE vBlockList AS CHARACTER NO-UNDO.
   
   vBlockList = BlckAcctOrdPay(iSurrogate,iDateTime,'*').
   RETURN vBlockList.
END FUNCTION.

/*
   �����頥� ᯨ᮪ ����� �����஢�� �� ����, �� ����᪠���
   �஢������ ���⥦�� �� �� ����� �� ���񤭮��.
*/
FUNCTION BlockAcctNoOrderPay RETURN CHARACTER (INPUT iSurrogate AS CHARACTER,
                                               INPUT iDateTime  AS DATETIME
                                              ).

   DEFINE VARIABLE vBlockList AS CHARACTER NO-UNDO.
   
   vBlockList = BlckAcctOrdPay(iSurrogate,iDateTime,'-1').
   RETURN vBlockList.
END FUNCTION.

/* �஢���� �����஢�� ��� �⭮�⥫쭮 �஢���� */
FUNCTION CheckBlockAcct RETURN CHAR    PRIVATE (INPUT  iFileName      AS CHARACTER,
                                                INPUT  iSurrogate     AS CHARACTER,
                                                INPUT  iFileNameCtrl  AS CHARACTER,
                                                INPUT  iSurrogateCtrl AS CHARACTER,
                                                INPUT  iDateTime      AS DATETIME,
                                                OUTPUT oBlockRecid    AS RECID
                                               ).

   DEFINE VARIABLE vBlock    AS CHAR     NO-UNDO.
   DEFINE VARIABLE vDtTmBlk  AS DATETIME NO-UNDO.
   DEFINE VARIABLE vIsBudget AS LOGICAL  NO-UNDO.

   DEF BUFFER acct     FOR acct.     /* ���������� ����. */
   DEF BUFFER op-entry FOR op-entry. /* ���������� ����. */
   DEF BUFFER op       FOR op.       /* ���������� ����. */
    
   bl:
   DO:
      {find-act.i
         &acct = ENTRY(1,iSurrogate,',')
         &curr = ENTRY(2,iSurrogate,',')
      } 
      IF NOT AVAIL acct THEN
         LEAVE bl.

      FOR FIRST op-entry WHERE op-entry.op       EQ INT64(ENTRY(1,iSurrogateCtrl,','))
                           AND op-entry.op-entry EQ INT64(ENTRY(2,iSurrogateCtrl,','))
         NO-LOCK:            
         FIND FIRST op OF op-entry 
            NO-LOCK NO-ERROR.
         IF NOT AVAIL op THEN
            RETURN "".

         IF op.op-date EQ ? THEN
            RETURN "".

         IF {assigned iFileNameCtrl} THEN DO:
            IF op.op-date = DATE(iDateTime) AND
               op.op-date = TODAY
            THEN
               vDtTmBlk = DATETIME(op.op-date, TIME * 1000).
            ELSE 
               vDtTmBlk = DATETIME(op.op-date + 1) - 1.
         END.
         ELSE
            vDtTmBlk = iDateTime.

            RUN IsBudgetPayment IN h_op (op.op, OUTPUT vIsBudget).

         BlkObj:
         FOR EACH  BlockObject WHERE 
                   BlockObject.class-code   EQ 'BlockAcct'
               AND BlockObject.FILE-NAME    EQ 'acct'
               AND BlockObject.surrogate    EQ iSurrogate
         NO-LOCK,
            FIRST code WHERE 
                 code.class EQ "acct-status"
             AND code.code  EQ BlockObject.block-type 
             AND code.misc[1] NE ""
            NO-LOCK BY code.misc[1]:
            /*
               0200763: �᫮��� �� ��ਮ� ����⢨� �����஢�� ��७�ᥭ�
               ������ 横��. � ��⨢��� ��砥 �� �������� ������
               OE DataServer �������� �訡�� (Memory Violation).
            */
            IF BlockObject.beg-datetime > vDtTmBlk OR
               BlockObject.end-datetime < vDtTmBlk
            THEN
               NEXT BlkObj.
            /* �஢�ઠ  */
            IF {assigned BlockObject.txt[1]} AND
               (CAN-DO(BlockObject.txt[1], op.order-pay) OR vIsBudget) AND
               NOT Chk_AcctRec_For_CBLACCT(IF {assigned op.ben-acct} THEN op.ben-acct ELSE op-entry.acct-cr)
            THEN
               NEXT BlkObj.
            CASE code.misc[1]:
               WHEN "������" THEN DO:
                  IF     op-entry.acct-db EQ acct.acct
                     AND (    BlockObject.val[1] = 0 
                          OR  (IF acct.currency = ""
                               THEN op-entry.amt-rub
                               ELSE op-entry.amt-cur) GE BlockObject.val[1]
                         )
                  THEN DO:
                     vBlock      = BlockObject.block-type.
                     oBlockRecid = RECID(BlockObject).
                     LEAVE BlkObj.
                  END.
               END.
               WHEN "������" THEN DO:
                  IF     op-entry.acct-cr EQ acct.acct
                     AND (    BlockObject.val[2] = 0 
                          OR  (IF acct.currency = ""
                               THEN op-entry.amt-rub
                               ELSE op-entry.amt-cur) GE BlockObject.val[2]
                         )
                  THEN DO:
                     vBlock      = BlockObject.block-type.
                     oBlockRecid = RECID(BlockObject).
                     LEAVE BlkObj.
                  END.
               END.
               WHEN "����" THEN DO:
                  IF (    op-entry.acct-cr EQ acct.acct
                      AND (    BlockObject.val[2] = 0 
                           OR  (IF acct.currency = ""
                                THEN op-entry.amt-rub
                                ELSE op-entry.amt-cur) GE BlockObject.val[2]
                          )
                     )
                  OR (   op-entry.acct-db EQ acct.acct
                     AND (    BlockObject.val[1] = 0 
                          OR  (IF acct.currency = ""
                               THEN op-entry.amt-rub
                               ELSE op-entry.amt-cur) GE BlockObject.val[1]
                         )
                     )        
                  THEN DO:
                     vBlock      = BlockObject.block-type.
                     oBlockRecid = RECID(BlockObject).
                     LEAVE BlkObj.
                  END.
               END.
               WHEN "�����㬬" THEN DO:
                  vBlock      = BlockObject.block-type.
                  oBlockRecid = RECID(BlockObject).
                  LEAVE BlkObj.
                  /*
                  IF BlockObject.val[3] GT 0 THEN DO:
                     RUN cli-pos IN h_base (Acct.acct,
                                            Acct.currency,
                                            vDtTmBlk,
                                            vDtTmBlk,"�").


                     IF BlockObject.val[3] LT (IF acct.currency = ""
                                               THEN sh-bal
                                               ELSE sh-val)
                     THEN DO:
                        vBlock      = YES.
                        oBlockRecid = RECID(BlockObject).
                        LEAVE BlkObj.
                     END.
                  END.*/
               END.
               OTHERWISE DO:
                  vBlock      = BlockObject.block-type.
                  oBlockRecid = RECID(BlockObject).
                  LEAVE BlkObj.
               END.
            END CASE. 
         END.
      END.
   END.
   RETURN vBlock.
END FUNCTION.

/* �஢���� �����஢�� ��ꥪ� �⭮�⥫쭮 ��㣮�� */
FUNCTION CheckObject RETURN CHAR    (INPUT iFileName      AS CHARACTER,
                                     INPUT iSurrogate     AS CHARACTER,
                                     INPUT iFileNameCtrl  AS CHARACTER,
                                     INPUT iSurrogateCtrl AS CHARACTER,
                                     INPUT iDateTime      AS DATETIME,
                                     OUTPUT oBlockRecid   AS RECID
                                     ).
   
   DEFINE VARIABLE vBlock AS CHAR NO-UNDO.

   CASE iFileName:
      WHEN "acct" THEN
         vBlock = CheckBlockAcct(iFileName,     
                                 iSurrogate,    
                                 iFileNameCtrl, 
                                 iSurrogateCtrl,
                                 iDateTime,
                                 OUTPUT oBlockRecid
                                 ).


   END CASE.
   RETURN vBlock.
END FUNCTION.

/*
   �����頥� �����஢����� �㬬� �� ���.
   �� 㪠����� ���񤭮�� "-1" ������ ⮫쪮 � �����஢��, �����
   �� ����᪠�� �஢������ ���⥦�� �� �� ����� �� ���񤭮��.
*/
FUNCTION GetBlockPosition RETURN DECIMAL (INPUT iAcct      AS CHAR,
                                          INPUT iCurrency  AS CHAR,
                                          INPUT iOrdrPay   AS CHAR,     
                                          INPUT iOpDate    AS DATE).
   
   DEFINE VARIABLE vBlockSumm AS DECIMAL  NO-UNDO.
   DEFINE VARIABLE vDtTmBlk   AS DATETIME NO-UNDO.
   DEFINE BUFFER acct     FOR acct.     /* ���������� ����. */

   bl:
   DO:
      {find-act.i
         &acct = iAcct
         &curr = iCurrency
      } 
      vBlockSumm = 0.0.
     
     IF iOpDate EQ ?  THEN RETURN 0.0.

     IF iOpDate EQ TODAY THEN
        vDtTmBlk = DATETIME(TODAY,MTIME).
     ELSE 
        vDtTmBlk = DATETIME(iOpDate + 1) - 1.

      IF NOT AVAIL acct THEN
         LEAVE bl.
      BLKOBJ:
      FOR EACH  BlockObject WHERE 
                BlockObject.class-code   EQ 'BlockAcct'
            AND BlockObject.FILE-NAME    EQ 'acct'
            AND BlockObject.surrogate    EQ iAcct + ',' + iCurrency
      NO-LOCK,
            FIRST code WHERE 
                code.class   EQ "acct-status"
            AND code.code    EQ BlockObject.block-type 
            AND code.misc[1] EQ '�����㬬'
            NO-LOCK:
         /*
            0200763: �᫮��� �� ��ਮ� ����⢨� �����஢�� ��७�ᥭ�
            ������ 横��. � ��⨢��� ��砥 �� �������� ������
            OE DataServer �������� �訡�� (Memory Violation).
         */
         IF BlockObject.beg-datetime > DATETIME(vDtTmBlk) OR
            BlockObject.end-datetime < DATETIME(vDtTmBlk)
         THEN
            NEXT BLKOBJ.
         IF {assigned BlockObject.txt[1]} AND
            (iOrdrPay = "-1" OR CAN-DO(BlockObject.txt[1], iOrdrPay))
         THEN
            NEXT BlkObj.
         vBlockSumm = vBlockSumm + BlockObject.val[3].
      END.
   END.
   RETURN vBlockSumm.
END FUNCTION.

PROCEDURE CreateBlockObj.
   DEFINE INPUT PARAM iFileName  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAM iSurrogate AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAM iDateTime  AS DATETIME  NO-UNDO.
   DEFINE INPUT PARAM iBlkType   AS CHARACTER NO-UNDO.

   CASE iFileName:
      WHEN "acct" THEN DO:
         RUN CreateBlockAcct(iFileName, 
                             iSurrogate,
                             iDateTime,
                             iBlkType
                            ) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
      END.
   END CASE.
END PROCEDURE.

PROCEDURE CreateBlockAcct PRIVATE.
   DEFINE INPUT PARAM iFileName  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAM iSurrogate AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAM iDateTime  AS DATETIME  NO-UNDO.
   DEFINE INPUT PARAM iBlkType   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vRetryError   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFlagSet      AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vOrderPay     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStatCode     AS CHARACTER NO-UNDO.

   DEF BUFFER BlockObject FOR BlockObject. /* ���������� ����. */

   Main:
   DO TRANSACTION ON ERROR UNDO,RETRY:
      IF RETRY THEN DO:
         vRetryError = RETURN-VALUE.
         IF vRetryError = "" THEN
            vRetryError = ERROR-STATUS:GET-MESSAGE(1).
         IF vRetryError = "" THEN
            vRetryError = "�� ��।�����".

         RUN Fill-SysMes("","ComnExc01","","%s=" + PROGRAM-NAME(1) +
                                           "%s=" + vRetryError).
         LEAVE Main.
      END.
      IF NOT {assigned iBlkType} THEN DO:
         vFlagSet = YES.
         LEAVE Main.
      END.
      FIND FIRST BlockObject WHERE
                 BlockObject.class-code   = 'BlockAcct'
             AND BlockObject.FILE-NAME    = 'acct'
             AND BlockObject.surrogate    = iSurrogate
             AND BlockObject.block-type   = iBlkType
             AND BlockObject.beg-datetime = iDateTime
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL BlockObject THEN DO:
         CREATE BlockObject.
         ASSIGN
            BlockObject.class-code   = 'BlockAcct'
            BlockObject.FILE-NAME    = 'acct'
            BlockObject.surrogate    = iSurrogate
            BlockObject.block-type   = iBlkType
            BlockObject.beg-datetime = iDateTime
         NO-ERROR. {&ON-ERROR}
      END.
      vStatCode = GetCode("acct-status",iBlkType).
      vOrderPay = "".
      IF INDEX(vStatCode,"�珫��(") NE 0 THEN
         vOrderPay = ENTRY(1,ENTRY(2,vStatCode,"("),")").
      BlockObject.txt[1] = vOrderPay NO-ERROR. {&ON-ERROR}
      VALIDATE BlockObject NO-ERROR. {&ON-ERROR}
      vFlagSet = YES.
   END.
   IF vFlagSet 
   THEN RETURN.
   ELSE RETURN ERROR.
END PROCEDURE.

/***************************************************************************/
/* �஢����, ᮤ�ন��� �� ��� ���� �� �����஢�� � �� ���끫���         */
/*    (��� ࠡ��� � ����⥪�� ���⁫��)                                   */
/***************************************************************************/
FUNCTION ChkBlkCrd RETURNS LOGICAL(INPUT iSurrogate  AS CHARACTER,
                                   INPUT iDateTime   AS DATETIME,
                                   INPUT iBlSum      AS LOGICAL,    /* �������� �� YES ��� �����㬬 */
                                   INPUT iNoOrderPay AS LOGICAL).   /* ⮫쪮 �����஢�� ��� 㪠����� ���񤭮�� */

   DEFINE VARIABLE vBlTypes   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBlockList AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI         AS INT64     NO-UNDO.
   DEFINE VARIABLE vDtTmBlk   AS DATETIME NO-UNDO.

   vBlTypes   = FGetSetting ("���⁫��", "���끫���", "").
  
   IF {assigned vBlTypes} THEN DO: 

      IF DATE(iDateTime) EQ TODAY THEN
         vDtTmBlk = DATETIME(TODAY,MTIME).
      ELSE 
         vDtTmBlk = DATETIME(DATE(iDateTime) + 1) - 1.

      vBlockList = IF iNoOrderPay
                   THEN BlockAcctNoOrderPay(iSurrogate, vDtTmBlk)
                   ELSE BlockAcct(iSurrogate, vDtTmBlk).

      DO vI = 1 TO NUM-ENTRIES(vBlockList):

         IF  (ENTRY(vI,vBlockList) NE "�����㬬" OR (ENTRY(vI,vBlockList) EQ "�����㬬" AND iBlSum)) 
            AND CAN-DO(vBlTypes,ENTRY(vI,vBlockList)) THEN RETURN YES.
      END.
   END.

   RETURN NO.
END FUNCTION.


/* �����頥� �����஢����� �㬬� �� ��� ��� ����ᨬ��� �� ��।���� */

FUNCTION GetBlockPositionAll RETURN DECIMAL (INPUT iAcct      AS CHAR,
                                          INPUT iCurrency  AS CHAR,
                                          INPUT iOpDate    AS DATE).
   
   DEFINE VARIABLE vBlockSumm AS DECIMAL  NO-UNDO.
   DEFINE VARIABLE vDtTmBlk   AS DATETIME NO-UNDO.
   DEFINE BUFFER acct     FOR acct.     /* ���������� ����. */

   bl:
   DO:
      {find-act.i
         &acct = iAcct
         &curr = iCurrency
      } 
      vBlockSumm = 0.0.

     IF iOpDate EQ TODAY THEN
        vDtTmBlk = DATETIME(TODAY,MTIME).
     ELSE 
        vDtTmBlk = DATETIME(iOpDate + 1) - 1.

      IF NOT AVAIL acct THEN
         LEAVE bl.
      BLKOBJ:
      FOR EACH  BlockObject WHERE 
                BlockObject.class-code   EQ 'BlockAcct'
            AND BlockObject.FILE-NAME    EQ 'acct'
            AND BlockObject.surrogate    EQ iAcct + ',' + iCurrency
      NO-LOCK,
            FIRST code WHERE 
                code.class   EQ "acct-status"
            AND code.code    EQ BlockObject.block-type 
            AND code.misc[1] EQ '�����㬬'
            NO-LOCK:
         /*
            0200763: �᫮��� �� ��ਮ� ����⢨� �����஢�� ��७�ᥭ�
            ������ 横��. � ��⨢��� ��砥 �� �������� ������
            OE DataServer �������� �訡�� (Memory Violation).
         */
         IF BlockObject.beg-datetime > DATETIME(vDtTmBlk) OR
            BlockObject.end-datetime < DATETIME(vDtTmBlk)
         THEN
            NEXT BLKOBJ.
         vBlockSumm = vBlockSumm + BlockObject.val[3].
      END.
   END.
   RETURN vBlockSumm.
END FUNCTION.

/* ��।���� �㬬� ��������� ��� ᯨᠭ�� */
FUNCTION CalcFreeOstOnBlockAcct RETURNS DECIMAL(INPUT iAcct     AS CHARACTER,
                                                INPUT iCurrency AS CHARACTER,
                                                INPUT iDate     AS DATE,
                                                INPUT iTime     AS INT64
                                               ).
   DEFINE VARIABLE vAmt AS DECIMAL NO-UNDO.


   IF CAN-DO(BlockAcct(iAcct + "," + iCurrency, DATETIME(iDate, iTime)), "������") THEN
      RETURN 0.0.

   RUN acct-pos IN h_base (iAcct,
                           iCurrency,
                           iDate,
                           iDate,
                           CHR(251)).

   IF iCurrency EQ "" THEN
      vAmt = sh-bal.
   ELSE
      vAmt = sh-val.

   RETURN vAmt - GetBlockPositionAll(iAcct, iCurrency, iDate).
END FUNCTION.                                               

/* ��।���� ⨯ �����஢�� ��� �� ��� ���� */
FUNCTION GetBlkType RETURNS CHARACTER(INPUT iCode AS CHARACTER).

   RETURN GetCodeMiscEx("acct-status", iCode, 1, iCode).
END FUNCTION.

/* ��।���� �㬬� �����஢�� ��� ⨯ ���ன �������� ��� ���� */
PROCEDURE CalcBlkMask.
   DEFINE INPUT  PARAMETER iSur     AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iDate    AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iBlkMask AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iSeq     AS CHARACTER   NO-UNDO.

   DEFINE OUTPUT PARAMETER oBlCr    AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER oBlDb    AS DECIMAL     NO-UNDO.

   ASSIGN 
      oBlCr = 0.0
      oBlDb = 0.0
   .
   BLKOBJ:
   FOR EACH blockobject WHERE  blockobject.class-code EQ "BlockAcct" AND
                               blockobject.file-name  EQ "acct" AND
                               blockobject.surrogate EQ iSur AND 
                               CAN-DO(iBlkMask,blockobject.block-type)

   NO-LOCK:
      /*
         0200763: �᫮��� ��� ������ ����砭�� ����⢨� �����஢��
         ��७�ᥭ� ������ 横��. � ��⨢��� ��砥 �� ��������
         ������ OE DataServer �������� �訡�� (Memory Violation).
      */
      IF BlockObject.end-datetime < DATETIME(iDate) THEN
         NEXT BLKOBJ.
      IF iSeq EQ "*"  OR 
         (iSeq NE "*" AND NOT CAN-DO(BlockObject.txt[1],iSeq))
      THEN DO:
            CASE BlockObject.block-type:
               WHEN "�����㬬" THEN
                  ASSIGN 
                     oBlCr = oBlCr + BlockObject.val[3]
                     oBlDb = oBlDb + BlockObject.val[3]
                  .
               OTHERWISE
                  ASSIGN 
                     oBlCr = oBlCr + BlockObject.val[1]
                     oBlDb = oBlDb + BlockObject.val[2]
                  . 
            END CASE.
                 
      END.
   END.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='11/12/2014 14:47:00.496+04:00' */
/* $LINTFILE='pp-blkob.p' */
/*prosigntor0z+q04mu0xJgwygwFrA*/