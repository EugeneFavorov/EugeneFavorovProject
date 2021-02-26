
/* +++ plbnk.pro was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: plbnk.pro
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ��楤��� �������⥪� pp-plbnk.p
     Modified: 
*/
/*===============================================================================================*/
/*=== ������塞 �������� ��६���� ������ �� 365� ============================================*/
PROCEDURE SetEnv365p:
   DEF INPUT  PARAM iInfType AS  CHAR  NO-UNDO. /* ��� ���ଠ樮����� �����                   */
   DEF INPUT  PARAM iAllFil  AS  LOG   NO-UNDO. /* ���� ���� �� �ᥬ 䨫�����: ��/���           */

   /* ������ � ���譥� ���� ���� ⮫쪮 � ��砥 ��� */
   IF {assigned iInfType} THEN 
      mZaprosNO = CAN-DO({&INFO-TYPE-ZNO}, iInfType).

   /* ���� ��⮢ �� ��� 䨫����� */
   IF iAllFil NE ? THEN
      mAllFil  = iAllFil.

END PROCEDURE. /* SetEnv365p */

/*===============================================================================================*/


/*===============================================================================================*/
/*=== ���� ��� �� ���譥� ��⥬� ============================================================*/
PROCEDURE FindAcct:
   DEF INPUT  PARAM iAcct AS  CHAR  NO-UNDO. /* ����� ���                                      */
   DEF OUTPUT PARAM TABLE FOR ttExtAcct.     /* ������ �� �������� ��⠬                      */

   IF     {assigned mAcctFnd} 
      AND {assigned iAcct}
   THEN
   DO:
      /* ����᪠�� ���� ��� �� ���譥� ⠡��� */
      RUN VALUE(mAcctFnd)(iAcct, 
                          mAllFil, 
                          OUTPUT TABLE ttExtAcct) 
                         {&RAISE-ERROR}.
     
   END. /* IF {assigned mAcctFndExt} */

END PROCEDURE. /* FindAcct */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���� ��� � ��⮬ ४����⮢ ������ ===================================================*/
PROCEDURE FindAcctEx:
   DEF INPUT  PARAM iHReqInfo  AS  HANDLE NO-UNDO. /* �����⥫� �� ���ଠ樮���� ���� �����  */
   DEF INPUT  PARAM iAcct      AS  CHAR   NO-UNDO. /* ����� ���                               */
   DEF OUTPUT PARAM TABLE      FOR ttExtAcct.      /* ������ �� �������� ��⠬                */

DEF VAR vInn    AS  CHAR  NO-UNDO. /* ��� ������                                                */
DEF VAR vKpp    AS  CHAR  NO-UNDO. /* ��� ������                                                */
DEF VAR vName   AS  CHAR  NO-UNDO. /* ������������ ������                                       */

   vInn  = iHReqInfo::innnp$  NO-ERROR.
   vKpp  = iHReqInfo::kppnp$  NO-ERROR.
   vName = iHReqInfo::naimnp$ NO-ERROR.
   ASSIGN
      vInn  = fStrNvl(vInn , "")
      vKpp  = fStrNvl(vKpp , "")
      vName = fStrNvl(vName, "")
   .
   IF     {assigned mAcctFndEx} 
      AND {assigned iAcct}
   THEN
   DO:
      /* ����᪠�� ���� ��� �� ���譥� ⠡��� */
      RUN VALUE(mAcctFndEx)(vInn, 
                            vKpp, 
                            vName,
                            iAcct,
                            mAllFil,
                            OUTPUT TABLE ttExtAcct) 
                           {&RAISE-ERROR}.
   END. /* IF {assigned mAcctFndExtEx} */

END PROCEDURE. /* FindAcctEx */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ����砥� ���⪨ � ������ �� ���� =======================================================*/
PROCEDURE AcctPos:
   DEF INPUT  PARAM iBegDate AS  DATE NO-UNDO.  /* ��� ��砫� ���㧪�                         */
   DEF INPUT  PARAM iEndDate AS  DATE NO-UNDO.  /* ��� ����砭�� ���㧪�                      */
   DEF PARAM BUFFER bExtAcct FOR ttExtAcct.     /* C��                                         */
   DEF OUTPUT PARAM oAmtIn   AS  DEC   NO-UNDO. /* �室�騩 ���⮪                             */
   DEF OUTPUT PARAM oAmbDb   AS  DEC   NO-UNDO. /* ������ �� ������                            */
   DEF OUTPUT PARAM oAmtCr   AS  DEC   NO-UNDO. /* ������ �� �।���                           */
   DEF OUTPUT PARAM oAmt     AS  DEC   NO-UNDO. /* ��室�騩 ���⮪                            */

   IF {assigned mAcctPos} THEN
   DO:
      /* ����砥� ������ � ���⪨  */
      RUN VALUE(mAcctPos)(iBegDate,
                          iEndDate,
                          mAllFil,
                          INPUT TABLE bExtAcct,
                          OUTPUT oAmtIn,
                          OUTPUT oAmbDb,
                          OUTPUT oAmtCr,
                          OUTPUT oAmt) 
                         {&RAISE-ERROR}.
   END. /* IF {assigned mAcctPos} THEN */

END PROCEDURE. /* AcctPos */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���室��� ��� ����祭�� ���⪮� � ������ �� ���� �१ ����� ⠡���� acct ============*/
PROCEDURE AcctPosBuf:
   DEF INPUT  PARAM  iBegDate AS  DATE NO-UNDO.  /* ��� ��砫� ���㧪�                         */
   DEF INPUT  PARAM  iEndDate AS  DATE NO-UNDO.  /* ��� ����砭�� ���㧪�                      */
   DEF PARAM BUFFER  acct     FOR acct.          /* C��                                         */
   DEF OUTPUT PARAM  oAmtIn   AS  DEC   NO-UNDO. /* �室�騩 ���⮪                             */
   DEF OUTPUT PARAM  oAmbDb   AS  DEC   NO-UNDO. /* ������ �� ������                            */
   DEF OUTPUT PARAM  oAmtCr   AS  DEC   NO-UNDO. /* ������ �� �।���                           */
   DEF OUTPUT PARAM  oAmt     AS  DEC   NO-UNDO. /* ��室�騩 ���⮪                            */

DEF BUFFER bExtAcct FOR ttExtAcct.

   IF AVAIL(acct) THEN
   DO:

      CREATE bExtAcct.
      /* �����㥬 ����� �� ⠡���� � ��� १���� */
      BUFFER-COPY acct TO bExtAcct.
      /* ���࠭塞 ��������� � ⠡���� */
      VALIDATE bExtAcct NO-ERROR.

      /* ����砥� ������ � ���⪨  */
      RUN AcctPos IN THIS-PROCEDURE(iBegDate,
                                    iEndDate,
                                    BUFFER bExtAcct,
                                    OUTPUT oAmtIn,
                                    OUTPUT oAmbDb,
                                    OUTPUT oAmtCr,
                                    OUTPUT oAmt) 
                                   {&RAISE-ERROR}.
      DELETE bExtAcct.
   END. /* IF AVAIL(acct) THEN */

END PROCEDURE. /* AcctPosBuf */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���������� ����� ������ �� ���� ========================================================*/
PROCEDURE FillOp:
   DEF INPUT  PARAM iHRepOp  AS  HANDLE NO-UNDO. /* �����⥫� �� ⠡���� � ����묨              */
   DEF INPUT  PARAM iBegDate AS  DATE   NO-UNDO. /* ��� ��砫� ���㧪�                        */ 
   DEF INPUT  PARAM iEndDate AS  DATE   NO-UNDO. /* ��� ����砭�� ���㧪�                     */
   DEF INPUT  PARAM iUpID    AS  INT64  NO-UNDO. /* ID �易����� ���                         */
   DEF PARAM BUFFER bExtAcct FOR ttExtAcct.      /* C��                                        */
   DEF OUTPUT PARAM oNumDocs AS  INT64  NO-UNDO. /* ������⢮ ᮧ����� ���㬥�⮢             */

   IF {assigned mFillOp} THEN
   DO:
      /* ������塞 ����� �� ���譥� ��⥬� */
      RUN VALUE(mFillOp)(iHRepOp,
                         iBegDate,
                         iEndDate,
                         iUpID,
                         mAllFil,
                         INPUT TABLE bExtAcct,
                         OUTPUT oNumDocs) 
                        NO-ERROR.
      IF    oNumDocs EQ ? 
         OR ERROR-STATUS:ERROR NE NO
      THEN
      DO:
         oNumDocs = 0.
         RETURN ERROR {&RETURN_VALUE}.
      END.
   END. /* IF {assigned mFillOp} THEN */

END PROCEDURE. /* FillOp */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ���������� ����� ����権 �� ���� �१ ���� ��� ======================================*/
PROCEDURE FillOpBuf:
   DEF INPUT  PARAM iHRepOp   AS  HANDLE NO-UNDO. /* �����⥫� �� ⠡���� � ����묨              */
   DEF INPUT  PARAM iBegDate  AS  DATE   NO-UNDO. /* ��� ��砫� ���㧪�                        */ 
   DEF INPUT  PARAM iEndDate  AS  DATE   NO-UNDO. /* ��� ����砭�� ���㧪�                     */
   DEF INPUT  PARAM iUpID     AS  INT64  NO-UNDO. /* ID �易����� ���                         */
   DEF PARAM BUFFER acct     FOR acct.           /* C��                                        */
   DEF OUTPUT PARAM oNumDocs  AS  INT64  NO-UNDO. /* ������⢮ ᮧ����� ���㬥�⮢             */

DEF BUFFER bExtAcct FOR ttExtAcct.

   IF AVAIL(acct) THEN
   DO:
      CREATE bExtAcct.
      /* �����㥬 ����� �� ⠡���� � ��� १���� */
      BUFFER-COPY acct TO bExtAcct.
      /* ���࠭塞 ��������� � ⠡���� */
      VALIDATE bExtAcct.

      /* ������塞 ����� �� ���譥� ��⥬� */
      RUN FillOp IN THIS-PROCEDURE(iHRepOp,
                                   iBegDate,
                                   iEndDate,
                                   iUpID,
                                   BUFFER bExtAcct,
                                   OUTPUT oNumDocs) 
                                  {&RAISE-ERROR}.
      DELETE bExtAcct.

   END. /* IF AVAIL(acct) THEN */

END PROCEDURE. /* FillOpBuf */
/*===============================================================================================*/

/*===============================================================================================*/
/*=== �஢�ઠ ��� ��� 365�: �ਭ���������� �������/���� ������ =============================*/
PROCEDURE ExtValidateAcct:
   DEF INPUT        PARAM iAcct      LIKE acct.acct NO-UNDO. /* ����� ���                      */
   DEF INPUT        PARAM iHCust365p AS   HANDLE    NO-UNDO. /* �����⥫� �� ����� ������      */
   DEF INPUT        PARAM iBegDate   AS   DATE      NO-UNDO. /* ��砫� ��ਮ��                   */
   DEF INPUT        PARAM iEndDate   AS   DATE      NO-UNDO. /* ����砭�� ��ਮ��                */
   DEF INPUT        PARAM iStrict    AS   LOG       NO-UNDO.
   DEF INPUT-OUTPUT PARAM oFeature   AS   INT64     NO-UNDO.

/* ��楤�� ᮧ���� �� �᭮�� ValidateAcct365p �� core365p.pro */

DEF VAR vHCustBuffer365p AS HANDLE NO-UNDO.
DEF VAR vTmpStr          AS CHAR   NO-UNDO.

DEF BUFFER bExtAcct FOR ttExtAcct.

   /* ����᪠�� ���� ��� �� ���譥� ⠡��� */
   RUN FindAcct IN THIS-PROCEDURE(iAcct, 
                                  OUTPUT TABLE bExtAcct) 
                                 {&RAISE-ERROR}.

   FIND FIRST bExtAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL(bExtAcct) THEN 
      RETURN. /* ��� �� ������ � �� ���譥� ��⥬� */

   /* �᫨ ������� ��᪮�쪮 ��⮢ ��ॡ�� �� ��  */
   lFndAcct:
   FOR EACH bExtAcct NO-LOCK:
      RUN FindCustRecord365p(iHCust365p,
                             bExtAcct.cust-cat,
                             bExtAcct.cust-id,
                             OUTPUT vHCustBuffer365p)
                            NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
          oFeature = {&FEAT-ERROR}.
          RETURN.
      END.
      IF VALID-HANDLE(vHCustBuffer365p) THEN 
         LEAVE lFndAcct.
  END. /* lFndAcct: FOR EACH bExtAcct NO-LOCK: */
  IF NOT VALID-HANDLE(vHCustBuffer365p) THEN DO:
     /* ��� �� ᮮ⢥���� ������� */
      oFeature = {&FEAT-WRONG-CUST}.
      RETURN.
  END.
  IF iBegDate            <> ? AND
     bExtAcct.close-date <> ? AND
     (iStrict OR
      bExtAcct.close-date <= iBegDate)
  THEN DO:
      /* ��� ������ �� ���� ��砫� ���� */
      oFeature = {&FEAT-CLOSED}.
      RETURN.
  END.
  IF iEndDate           <> ? AND
     bExtAcct.open-date <> ? AND
     bExtAcct.open-date >  iEndDate
  THEN DO:
      /* ��� �� �� ����� �� ���� ����砭�� ���� */
      oFeature = {&FEAT-NOT-OPEN}.
      RETURN.
  END.
  oFeature = {&FEAT-OK}.

END PROCEDURE. /* ExtValidateAcct */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== ������ FillZNOAcctPart365p �� zno365p2.pro ================================================*/
PROCEDURE FillZNOExtAcct:
   DEF INPUT        PARAM iRequestKind AS INT64  NO-UNDO. 
   DEF INPUT        PARAM iHRepAcct    AS HANDLE NO-UNDO.  
   DEF INPUT        PARAM iHReqInfo    AS HANDLE NO-UNDO.
   DEF INPUT        PARAM iHRepOp      AS HANDLE NO-UNDO.
   DEF INPUT        PARAM iAcct        AS CHAR   NO-UNDO.
   DEF INPUT        PARAM iReqDate     AS DATE   NO-UNDO.
   DEF INPUT        PARAM iBegDate     AS DATE   NO-UNDO.
   DEF INPUT        PARAM iEndDate     AS DATE   NO-UNDO.
   DEF INPUT        PARAM iSPacketID   AS INT64  NO-UNDO.
   DEF INPUT-OUTPUT PARAM pAcctCnt     AS INT64  NO-UNDO.
   DEF OUTPUT       PARAM oNumDocs     AS INT64  NO-UNDO.

DEF BUFFER bExtAcct FOR ttExtAcct.

DEF VAR vHRepAcctBuffer AS  HANDLE  NO-UNDO.
DEF VAR vHRepOpBuffer   AS  HANDLE  NO-UNDO.
DEF VAR vAcct           AS  CHAR    NO-UNDO.
DEF VAR vCurrency       AS  CHAR    NO-UNDO.
DEF VAR vNCCode         AS  CHAR    NO-UNDO.
DEF VAR vNumYears       AS  INT64   NO-UNDO.
DEF VAR vAmtIn          AS  DEC     NO-UNDO.
DEF VAR vAmt            AS  DEC     NO-UNDO.
DEF VAR vAmtDb          AS  DEC     NO-UNDO.
DEF VAR vAmtCr          AS  DEC     NO-UNDO.
DEF VAR vTmpDate        AS  DATE    NO-UNDO.

   ASSIGN
      oNumDocs        = 0
      vNCCode         = fGetSetting("�����悠�0406007", "", "")
      vHRepAcctBuffer = iHRepAcct:DEFAULT-BUFFER-HANDLE
      vHRepOpBuffer   = iHRepOp:DEFAULT-BUFFER-HANDLE
   .

   /* ���� ��� �� ������ � ४����⠬ ������ */
   RUN FindAcctEx IN THIS-PROCEDURE(iHReqInfo,
                                    iAcct,
                                    OUTPUT TABLE bExtAcct)
                                   {&RAISE-ERROR}.
   FIND FIRST bExtAcct NO-LOCK NO-ERROR.

   IF NOT AVAIL(bExtAcct) THEN
      RETURN. /* ��� �� ���譥� ��⥬� �� �����㦥� */

   vHRepAcctBuffer:BUFFER-CREATE().
   ASSIGN
      vAcct     = bExtAcct.number
      vCurrency = ( IF {assigned bExtAcct.currency} THEN bExtAcct.currency
                                                   ELSE vNCCode)
      pAcctCnt  = pAcctCnt + 1
   .

   RUN SetAttrValue365p(vHRepAcctBuffer, "�����", vAcct)         {&RAISE-ERROR}.
   RUN SetAttrValue365p(vHRepAcctBuffer, "�����", vCurrency)     {&RAISE-ERROR}.
   RUN SetAttrValue365p(vHRepAcctBuffer, "ID", STRING(pAcctCnt)) {&RAISE-ERROR}.

   IF iRequestKind <> {&REQ-KIND-TICLAM} THEN 
   DO:
      RUN SetAttrValue365p(vHRepAcctBuffer,"�����", bExtAcct.contract) {&RAISE-ERROR}.

   END. /* IF iRequestKind <> {&REQ-KIND-TICLAM} THEN  */

   IF iRequestKind <> {&REQ-KIND-TICLAC} THEN 
   DO:
      /* ����砥� ���⪨/������ �� ���譥� ��⥬� */
      RUN AcctPos IN THIS-PROCEDURE(iBegDate,
                                    iEndDate,
                                    BUFFER bExtAcct,
                                    OUTPUT vAmtIn,
                                    OUTPUT vAmtDb,
                                    OUTPUT vAmtCr,
                                    OUTPUT vAmt) 
                                   {&RAISE-ERROR}.

   END. /* IF iRequestKind <> {&REQ-KIND-TICLAC} THEN */

   CASE iRequestKind:
      WHEN {&REQ-KIND-TICLAC} THEN 
      DO:
          RUN SetAttrValue365p(vHRepAcctBuffer, "��⠎����", date2str(bExtAcct.open-date))
              {&RAISE-ERROR}.
          IF bExtAcct.close-date <> ? THEN 
          DO:
             RUN SetAttrValue365p(vHRepAcctBuffer, "��⠇�����", date2str(bExtAcct.close-date))
                 {&RAISE-ERROR}.

          END. /* IF acct.close-date <> ? THEN */
      END. /* WHEN {&REQ-KIND-TICLAC} THEN */

      WHEN {&REQ-KIND-TICLAS} THEN 
      DO:
         RUN SetAttrValue365p(vHRepAcctBuffer, "���⮪", STRING(ABSOLUTE(vAmt)))
             {&RAISE-ERROR}.
      END. /* WHEN {&REQ-KIND-TICLAS} THEN  */

      WHEN {&REQ-KIND-TICLAM} THEN 
      DO:
         IF FGetSetting("����ன��_365�", "�஢��娢","") = "��" THEN 
         DO:

            vNumYears = INT64(FGetSetting("����ன��_365�", "��娢�ப","0")) NO-ERROR. 
            vTmpDate  = DATE(MONTH(iBegDate), 
                             DAY  (iBegDate), 
                             YEAR (iBegDate) + vNumYears).

            IF     vNumYears <> ? 
               AND vNumYears > 0 
               AND iReqDate  > vTmpDaTe 
            THEN 
            DO:
               RUN TAXConfirmCreate (iSPacketID, "35", "����襭�� ��ਮ� "      + 
                                     "�믨᪨ �ॢ�蠥� ��娢�� �ப �࠭����. " + 
                                     "�믨᪠ �।��⠢���� �� ��᫥���� "        + 
                                     STRING(vNumYears) + "���", NOW) NO-ERROR. 
               iBegDate = DATE(MONTH(iReqDate),
                               DAY(iReqDate), 
                               YEAR(iReqDate) - vNumYears).

            END. /* IF     vNumYears <> ? */
         END. /* IF FGetSetting("����ன��_365�", "�஢��娢","") = "��" THEN  */

         RUN SetAttrValue365p(vHRepAcctBuffer,"��⠍�砫�", date2str(iBegDate))
             {&RAISE-ERROR}.
         RUN SetAttrValue365p(vHRepAcctBuffer,"��⠊���",  date2str(iEndDate))
             {&RAISE-ERROR}.
         RUN SetAttrValue365p(vHRepAcctBuffer,"���⍠�",   STRING(ABS(vAmtIn)))
             {&RAISE-ERROR}.                               
         RUN SetAttrValue365p(vHRepAcctBuffer,"���⊮�",   STRING(ABS(vAmt)))
             {&RAISE-ERROR}.                               
         RUN SetAttrValue365p(vHRepAcctBuffer,"�㬬����",   STRING(ABS(vAmtDb)))
             {&RAISE-ERROR}.
         RUN SetAttrValue365p(vHRepAcctBuffer,"�㬬��।",  STRING(ABS(vAmtCr)))
             {&RAISE-ERROR}.

         /* ������塞 ����� �� ���譥� ��⥬� */
         RUN FillOp IN THIS-PROCEDURE (vHRepOpBuffer,
                                       iBegDate,
                                       iEndDate,
                                       pAcctCnt,
                                       BUFFER bExtAcct,
                                       OUTPUT oNumDocs) 
                                      {&RAISE-ERROR}.
      END. /* WHEN {&REQ-KIND-TICLAM} THEN DO: */
   END CASE. /* iRequestKind: */

END PROCEDURE. /* FillZNOExtAcct */

/*===============================================================================================*/

/* --- plbnk.pro was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am --- */
