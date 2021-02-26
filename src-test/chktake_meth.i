/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2011 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: CHKTAKE_METH.I
      Comment: ��⮤ ����஫� �� ����⨥�
   Parameters:
         Uses:
      Used by:
      Created: 12.05.2011 15:53 DEAS    
     Modified: 12.05.2011 15:53 DEAS    
*/
{intrface.get tmess}

PROCEDURE meth_solid.
   DEFINE INPUT  PARAMETER iRec    AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iSumm   AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate   AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vShtrIz      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vShtrPeresch AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vin-kau      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMinOst      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOst         AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vDateShtrIz  AS DATE        NO-UNDO.
   DEFINE VARIABLE vStartDate   AS DATE        NO-UNDO.
   DEFINE VARIABLE vEnd-date    AS DATE        NO-UNDO.

   DEFINE BUFFER b-loan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iRec
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO Main, LEAVE Main.
      
      vShtrIz = GetXattrValueEx ("loan",
                                 b-loan.contract + "," + b-loan.cont-code,
                                 "���䈧���",
                                 "").
      vDateShtrIz = DATE (vShtrIz) NO-ERROR.
      vShtrPeresch = GetXattrValueEx ("loan",
                                      b-loan.contract + "," + b-loan.cont-code,
                                      "���䏥����",
                                      ?).
      RUN get-beg-date-prol IN h_dpspc (RECID(b-loan),
                                        iDate,
                                        OUTPUT vStartDate,
                                        OUTPUT vEnd-date).

      IF vDateShtrIz LT vStartDate
         OR (vDateShtrIz GE vStartDate 
             AND NOT {assigned vShtrPeresch}) THEN /* � �।���� ����. ����� ������ �� ��� ����襭��.*/
      DO:                               /*   �㦭� �஢���� */
         /* ��� ���⮪ �� ������ */
         RUN get_last_min_ost IN h_dpspc (RECID (b-loan),
                                          iDate,
                                          iDate,
                                          OUTPUT vMinOst).
         /*����騩 ���⮪ �� ������*/
         RUN get-summ-ost IN h_dpspc (RECID (b-loan),
                                      iDate,
                                      iDate,
                                      OUTPUT vOst).
         
         vOst = ABS(IF b-loan.currency EQ '' THEN ksh-bal  ELSE ksh-val).
         /* �஢�ઠ ����襭�� ������ */
         IF vOst - iSumm LT DEC (vMinOst) THEN
         DO:
            UpdateSigns ("loan",
                         b-loan.contract + "," + b-loan.cont-code,
                         "���䈧���",
                         STRING(iDate) ,
                         NO).
         END.
      END.
   END. /* Main */
END PROCEDURE.

PROCEDURE meth_udob.
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iSumm AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vShtrIz      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mShtrPeresch AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vin-kau      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOst         AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vStartDate   AS DATE        NO-UNDO.
   DEFINE VARIABLE vEnd-date    AS DATE        NO-UNDO.
   DEFINE VARIABLE vDateShtrIz  AS DATE        NO-UNDO.
   DEFINE BUFFER b-loan FOR loan.


   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iRec
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO Main, LEAVE Main.
      
      vShtrIz = GetXattrValueEx ("loan",
                                 b-loan.contract + "," + b-loan.cont-code,
                                 "���䈧���",
                                 "").
      vDateShtrIz = DATE (vShtrIz) NO-ERROR.
      mShtrPeresch = GetXattrValueEx ("loan",
                                      b-loan.contract + "," + b-loan.cont-code,
                                      "���䏥����",
                                      ?).
      RUN get-beg-date-prol IN h_dpspc (RECID(b-loan),
                                  iDate,
                                  OUTPUT vStartDate,
                                  OUTPUT vEnd-date).

      IF vDateShtrIz LT vStartDate /* � �।���� ����. ����� ������ �� ��� ����襭��.*/
         OR (vDateShtrIz GE vStartDate 
             AND NOT {assigned mShtrPeresch}) THEN 
      DO:                               
         FIND LAST loan-acct WHERE loan-acct.contract  =  b-loan.contract
                               AND loan-acct.cont-code =  b-loan.cont-code
                               AND loan-acct.since     <= iDate
                               AND loan-acct.acct-type = IF vEnd-date = ? THEN "loan-dps-p" ELSE "loan-dps-t"
                               NO-LOCK NO-ERROR.
         IF NOT AVAILABLE  loan-acct THEN 
         DO:
            oMess = "��������. �訡�� ��।������ ��� ������".
            RUN Fill-SysMes IN h_tmess ("", 
                                        "", 
                                        "-1", 
                                        oMess).
            UNDO MAIN, LEAVE MAIN.
         END.

         /* �㬬� ���᫥���� ��業⮢ */
         vin-kau  = b-loan.contract + "," + b-loan.cont-code + ",�����1".
         RUN  kau-pos.p(loan-acct.acct,
                        b-loan.currency,
                        iDate,
                        iDate,
                        gop-status,
                        vin-kau).
         vOst = ABS(IF b-loan.currency EQ '' THEN ksh-bal  ELSE ksh-val).
      
         IF iSumm GT vOst THEN
         DO:
            UpdateSigns ("loan",
                         b-loan.contract + "," + b-loan.cont-code,
                         "���䈧���",
                         STRING(iDate),
                         NO).
         END.
      END.
   END. /* Main */
END PROCEDURE.

PROCEDURE meth_invst.
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iSumm AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vShtrPereschDop  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vShtrPeresch     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMinOst          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStr-Acct        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTekAcct         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIn-inter        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIn-kau          AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStartDate       AS DATE        NO-UNDO. /*����. ����⨥ ������*/
   DEFINE VARIABLE vEnd-date        AS DATE        NO-UNDO. /*����. �����⨥ ������*/
   DEFINE VARIABLE vBegDate         AS DATE        NO-UNDO. /* ��� ���. ����⢨� ���. ��� */
   DEFINE VARIABLE vEndDate         AS DATE        NO-UNDO. /* ��� �����. ����⢨� ���. ��� */
   DEFINE VARIABLE vSummAcct        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vMinOstDec       AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vAmtPrich        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mAmtSummIz       AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vi               AS INT64       NO-UNDO.
   DEFINE VARIABLE vFlErr           AS INT64       NO-UNDO.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-acct      FOR acct.
   DEFINE BUFFER b-kau-entry FOR kau-entry.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iRec
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO Main, LEAVE Main.

      
      vShtrPereschDop = GetXattrValueEx ("loan",
                                         b-loan.contract + "," + b-loan.cont-code,
                                         "���䏥���℮�",
                                         "").
      IF NOT {assigned vShtrPereschDop} THEN
      DO:
         RUN get-beg-date-prol IN h_dpspc (RECID(b-loan),
                                           iDate,
                                           OUTPUT vStartDate,
                                           OUTPUT vEnd-date).
         /* �᭮���� ��� ������ */
         FIND LAST loan-acct WHERE loan-acct.contract  =  b-loan.contract
                               AND loan-acct.cont-code =  b-loan.cont-code
                               AND loan-acct.since     <= iDate
                               AND loan-acct.acct-type = IF vEnd-date = ? THEN "loan-dps-p" ELSE "loan-dps-t"
                               NO-LOCK NO-ERROR.
         IF NOT AVAILABLE  loan-acct THEN 
         DO:
            oMess = "��������. �訡�� ��।������ ��� ������".
            UNDO MAIN, LEAVE MAIN.
         END.
         RUN acct-pos IN h_base (loan-acct.acct,
                                 loan-acct.currency,
                                 iDate,
                                 iDate,
                                 gop-status).
         IF loan-acct.currency EQ "" THEN
            vSummAcct = ABS (sh-bal).
         ELSE
            vSummAcct = ABS (sh-val). /* �㬬� �� ��. ��� ������ � ���� ������ */

         /* ��� ���⮪ �� ������ */
         RUN get_last_min_ost IN h_dpspc (RECID (b-loan),
                                          iDate,
                                          iDate,
                                          OUTPUT vMinOst).
         vMinOstDec = DEC(vMinOst) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            oMess = "��������. �訡�� ��।������ ������ ������".
            RUN Fill-SysMes IN h_tmess ("", 
                                        "", 
                                        "-1", 
                                        oMess).
            UNDO MAIN, LEAVE MAIN.
         END.
         IF vSummAcct - iSumm LT vMinOstDec  THEN
         DO:  /* ����襭�� ������ */
            UpdateSigns ("loan",
                         b-loan.contract + "," + b-loan.cont-code,
                         "���䈧��℮�",
                         STRING(iDate),
                         NO).
         END.
         ELSE
         DO: /* ����� ������ ���, �஢�ਬ ����襭�� �� ���᫥��� ��業⠬ */
            vShtrPeresch = GetXattrValueEx ("loan",
                                            b-loan.contract + "," + b-loan.cont-code,
                                            "���䏥����",
                                           "").
            IF NOT {assigned vShtrPeresch} THEN
            DO:
               /* ��।���� �㬬� ��珐�1 �� ��ਮ� */
               RUN Get_Interest_KauEntry IN h_dpspr (BUFFER b-loan,
                                                     loan-acct.acct,
                                                     loan-acct.currency,
                                                     b-loan.contract + "," + b-loan.cont-code + ",�����1",
                                                     b-loan.open-date,
                                                     iDate,
                                                     OUTPUT vAmtPrich,
                                                     OUTPUT vFlErr).
/*               IF iSumm GT vAmtPrich THEN*/
               DO:
                  UpdateSigns ("loan",
                               b-loan.contract + "," + b-loan.cont-code,
                               "���䈧���",
                               STRING(iDate),
                               NO).
               END.
            END.
         END. /* ELSE */
      END. /* IF NOT {assigned vShtrIz} THEN */
   END. /* Main */
END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose: ����ୠ⨢�� ��⮤ ��� ��।������ ��࠭�祭�� �� ���ᨬ����� �㬬� ������.
  ����뢠���� � ��⮤������. �㬬� ��ࢮ�� ����� * ���祭�� ४����� �����∭�.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE MaxDepAmount:
   DEFINE INPUT  PARAMETER iRid      AS RECID      NO-UNDO.
   DEFINE INPUT  PARAMETER ibeg      AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iend      AS DATE       NO-UNDO.
   DEFINE OUTPUT PARAMETER oIn-inter AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess     AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vMinOst       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vClassMaksOst AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMaxOst       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vBegDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vEndDate      AS DATE       NO-UNDO.
   define variable vAcctRole     as character  no-undo.
   define variable vKodOst       as character  no-undo.
   define variable vKauSurr      as character  no-undo.
   define variable vAcct         as character  no-undo.
   define variable vAmount       as integer    no-undo.
   /* ***************************  Main Block  *************************** */

   MAIN:
   DO 
      ON ERROR   UNDO MAIN, LEAVE MAIN
      ON END-KEY UNDO MAIN, LEAVE MAIN:
      
      FIND FIRST loan WHERE RECID (loan) EQ iRid
         NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN
      DO:
         oMess =  "�訡�� �� ��।������ ������.".
         UNDO Main, LEAVE Main.   
      END.
      ELSE DO:

         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                          iend,
                                          OUTPUT vBegDate,
                                          OUTPUT vEndDate).

         /*����祭�� ஫� ��� �� ��ਮ� �஫*/ 
         RUN GetbaseAcctRole IN h_dps (INPUT RECID(loan),   
                                       INPUT vBegDate,
                                       OUTPUT vAcctRole).

         /*����祭�� �᭮����� ���*/
         RUN GetBaseAcct IN h_dps (loan.contract,
                                   loan.cont-code,
                                   iend,
                                   OUTPUT vAcct).
         {find-act.i
            &acct = "ENTRY(1, vAcct) "
            &curr = "ENTRY(2, vAcct) "
         }
         IF NOT AVAIL acct THEN
            UNDO MAIN, LEAVE MAIN.

         /*����祭�� ���� ���⪠ �᭮����� ���*/
         RUN GetBaseKodOst IN h_dps (vAcctRole,
                                    OUTPUT vKodOst).  
         vKauSurr = loan.contract + "," + loan.cont-code + "," + vKodOst.

         /* �㬬� ��ࢮ��砫쭮�� ����� */
         IF vBegDate EQ loan.open-date THEN
         DO:
            FIND FIRST kau-entry WHERE kau-entry.acct     EQ acct.acct 
                                   AND kau-entry.currency EQ acct.currency 
                                   AND kau-entry.kau      EQ vKauSurr
                                   AND NOT kau-entry.debit 
                                   AND kau-entry.op-date  GE vBegDate 
               NO-LOCK NO-ERROR.
            IF AVAIL kau-entry THEN
               oIn-inter = IF acct.currency EQ "" THEN kau-entry.amt-rub ELSE kau-entry.amt-cur. 
              ELSE RUN Get_Last_Param IN h_dpspc (iRid,
                                                  iBeg,
                                                  iEnd,
                                                  '������',
                                                  OUTPUT vMaxOst).
         END.
         ELSE
         DO:
            vBegDate = vBegDate - 1. 
            RUN kau-pos.p( acct.acct,
                           acct.currency,
                           vBegDate,
                           vBegDate,
                           "�",
                           vKauSurr ).
            oIn-inter = ABSOLUTE( IF acct.currency EQ "" THEN ksh-bal ELSE ksh-val).
         END.   
         RUN Get_Last_Param IN h_dpspc (RECID(loan),
                                        ibeg,
                                        iend,
                                        "�����∭�",
                                        OUTPUT vAmount).
         
         IF vAmount = ? THEN 
            DO:
               oMess =  "�� ���� ��।����� �����∭� ��� ��⮤� ��⮤������=MaxDepAmount.".
               UNDO Main, LEAVE Main.   
            END.
         ELSE
         oIn-inter = IF oIn-inter NE 0 THEN oIn-inter * vAmount ELSE ?.                
      END.
   END.
   /* *************************  End of Main Block  ********************** */

END PROCEDURE.


/*------------------------------------------------------------------------------
  ��⮤ ����஫� ������� � ������ �� ����� ����� ������� � ���������� ����⨩ �� ��।������ ������ �६���.
  ����뢠���� � ��⋨��∧��� �� ����� ������ ��� �࠭���樨 ������.
  �ᯮ���� ४������:
  ����⋨��� (� ��᮫�⭮� ��ࠦ���� ��稭����� � "=" ��� � % �� ��ࢮ��砫쭮�� ����� ��稭����� � "%")
  ����⏥ਮ� (�ଠ� �=,�=,�=)
  ����⊮����
  �� ����� ����⊮���� ����⨩ �� ��ਮ� ����⏥ਮ�, ��祬 � ��饩 �㬬� �� ��� ����⋨���
------------------------------------------------------------------------------*/
PROCEDURE lim_cnt_per:
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iAmt  AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vAcctLst      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vEndDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vMaxCnt       AS INT64      NO-UNDO.
   DEFINE VARIABLE vPeriod       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKauSurr      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcct         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAmount       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vFAmount      AS DECIMAL    NO-UNDO.  /* ���ᨬ��쭠� ���� �㬬� ����⨩ */
   DEFINE VARIABLE vMaxAmount    AS DECIMAL    NO-UNDO.  /* ���ᨬ��쭠� ���� �㬬� ����⨩ */
   DEFINE VARIABLE vMaxAmtChr    AS CHARACTER  NO-UNDO.  /* ���ᨬ��쭠� ���� �㬬� ����⨩ */
   DEFINE VARIABLE vListDate     AS CHARACTER  NO-UNDO.  /* ᯨ᮪ ��� ����⨩ */
   DEFINE VARIABLE tmp-date AS DATE NO-UNDO.
   DEFINE VARIABLE end-dat1 AS DATE NO-UNDO.
   DEFINE VARIABLE vI AS INT64 NO-UNDO.

   DEFINE BUFFER bkau-entry FOR kau-entry.
   /* ***************************  Main Block  *************************** */

   MAIN:
   DO 
      ON ERROR   UNDO MAIN, LEAVE MAIN
      ON END-KEY UNDO MAIN, LEAVE MAIN:
      
      FIND FIRST loan WHERE RECID (loan) EQ iRec
         NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN
      DO:
         oMess =  "�訡�� �� ��।������ ������.".
         UNDO Main, LEAVE Main.   
      END.
      ELSE DO:

         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                          iDate,
                                          OUTPUT vBegDate,
                                          OUTPUT vEndDate).

         RUN get_acct IN h_dpspc (RECID(loan),vBegDate,iDate,OUTPUT vAcctLst).
         vKauSurr  = loan.contract + "," + loan.cont-code + "," + IF vEndDate = ? THEN "��₪��" ELSE "��₪��".
         /* �� �ᥬ ������� ��⠬ �ந������ ������ ���-�� � �㬬� ����⨩ � ��⠢������ ᯨ᪠ ��� */
         tmp-date = beg-date.
         vListDate = "".
         DO vI = 1 TO NUM-ENTRIES(vAcctLst) :
      
         FIND FIRST acct WHERE acct.acct = ENTRY(vI,vAcctLst) AND acct.currency = loan.currency  NO-LOCK NO-ERROR.
         IF NOT AVAIL acct THEN NEXT.
         tmp-date = IF vI > 1 THEN DATE(ENTRY(vI - 1,vAcctLst)) ELSE vBegDate.
         end-dat1  = IF NUM-ENTRIES(vAcctLst) >= vI + 3 THEN DATE(ENTRY(vI + 3,vAcctLst)) ELSE vEndDate.
         FOR EACH kau-entry WHERE kau-entry.acct = acct.acct AND 
                               kau-entry.currency = acct.currency AND 
                               kau-entry.kau = vKauSurr AND
                               kau-entry.debit AND 
                               kau-entry.op-date >= tmp-date AND
                               kau-entry.op-date <= end-dat1  NO-LOCK,
                               EACH op-entry OF kau-entry 
                               WHERE NOT CAN-FIND(FIRST bkau-entry OF op-entry WHERE bkau-entry.kau = vKauSurr AND NOT bkau-entry.debit):
            vAmount = vAmount + (IF acct.currency = "" THEN op-entry.amt-rub ELSE op-entry.amt-cur).
            {additem.i vListDate STRING(op-entry.op-date)}
         END.

         END.   /*�� �ᥬ vAcctLst*/

         /*����稬 ���ᨬ����� ����� �㬬� ����⨩*/
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           '����⋨���',
                                           OUTPUT vMaxAmtChr).
         IF {assigned vMaxAmtChr} THEN DO: 
            IF vMaxAmtChr BEGINS "=" THEN vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
               oMess =  "�訡�� �� ��।������ ��������� �㬬� ������ ����⋨��� " + vMaxAmtChr.
               UNDO Main, LEAVE Main.
            END.
            IF vMaxAmtChr BEGINS "%" THEN DO:
               IF vBegDate = loan.open-date THEN
                     vFAmount = DEC(GetXAttrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "�㬬���᫊�����",
                                    "0")).
               IF vBegDate > loan.open-date OR vFAmount = 0 THEN DO:
                  /*����祭�� �᭮����� ���*/
                  tmp-date = IF vBegDate > loan.open-date THEN vBegDate - 1 ELSE vBegDate.
                  FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ (IF vEndDate = ? THEN "loan-dps-p" ELSE "loan-dps-t")
                  AND loan-acct.since LE tmp-date NO-LOCK NO-ERROR.
                  IF NOT AVAIL loan-acct THEN
                  DO:
                     oMess =  "�訡�� �� ��।������ �᭮����� ��� ������.".
                     UNDO Main, LEAVE Main.   
                  END.
                  RUN  kau-pos.p(loan-acct.acct,
                     loan-acct.currency,
                     tmp-date,
                     tmp-date,
                     gop-status,
                     vKauSurr).
               vFAmount = ABS(IF loan.currency eq '' then ksh-bal  else ksh-val) .
               END.
               vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                     oMess =  "�訡�� �� ��।������ ��������� �㬬� ������ ����⋨��� " + vMaxAmtChr.
                     UNDO Main, LEAVE Main.
                  END.
               vMaxAmount = vMaxAmount * vFAmount / 100.
            END.
         END.
         ELSE DO:
                     oMess =  "�訡�� �� ��।������ ��������� �㬬� ������ ����⋨���".
                     UNDO Main, LEAVE Main.
         END.
         /*�⠪, � vMaxAmount ���ᨬ��쭮 ��������� �㬬� ������*/
         IF vAmount + iAmt > vMaxAmount THEN DO:
            oMess = "�ॢ�襭�� �����⨬�� �㬬� ������ (�����⨬� " + STRING(vMaxAmount - vAmount) + ")!".
                     UNDO Main, LEAVE Main.
         END.
         /* �஢�ઠ �� ���ᨬ����� �㬬� ��諠 �ᯥ譮 */
         /*����稬 ���ᨬ��쭮� ������⢮ ����⨩ �� ��ਮ� */
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           '����⊮����',
                                           OUTPUT vMaxCnt).
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           '����⏥ਮ�',
                                           OUTPUT vPeriod).
         IF vMaxCnt = ? THEN DO:
                     oMess =  "�訡�� �� ��।������ ������⢠ �����⨬�� ����⨩ ����⊮����.".
                     UNDO Main, LEAVE Main.
         END.
         IF NUM-ENTRIES(vListDate) >= vMaxCnt THEN 
         DO:
           tmp-date = DATE(ENTRY(NUM-ENTRIES(vListDate) - vMaxCnt + 1,vListDate)).
           end-dat1 = Get-end-date(tmp-date,vPeriod).
           IF end-dat1 = ? THEN DO:
                     oMess =  "�訡�� �� ��।������ ��ਮ�� �����⨬�� ����⨩ ����⏥ਮ�.".
                     UNDO Main, LEAVE Main.
           END.
           IF end-dat1 >= iDate THEN DO:
                     oMess =  "���௠�� �����⨬�� ������⢮ ����⨩ �� " + STRING(end-dat1) + " �����⥫쭮!".
                     UNDO Main, LEAVE Main.
           END.
         END.
      END.
   END.
   /* *************************  End of Main Block  ********************** */

END PROCEDURE.



/*------------------------------------------------------------------------------
  ��⮤ ����஫� ������� � ������ �� ������������ ������� 
  � [���������� ����⨩ �� ��।������ ������ �६���](����易⥫��).
  ����뢠���� � ��⋨��∧��� �� ����� ������ ��� �࠭���樨 ������.
  �ᯮ���� ४������:
  ����⋨��� - ��᭨����� ���⮪ 
  (� ��᮫�⭮� ��ࠦ���� ��稭����� � "=" ��� � % �� ��ࢮ��砫쭮�� ����� ��稭����� � "%")
  ����⏥ਮ� (�ଠ� �=,�=,�=) 
  ����⊮����
  �� ����� ����⊮���� ����⨩ �� ��ਮ� ����⏥ਮ�, ��祬 ���⮪ �� ������ �� ����� ����⋨���
  - �᫨ ������ ⮫쪮 ����⊮����,� ��⠥� �� ���� �ப ������
  - �᫨ ����� ⮫쪮 ����⏥ਮ�, � ��⠥� 1 ����⨥ �� ��ਮ�
   ���� ������⢮ �� ����஫������
------------------------------------------------------------------------------*/
PROCEDURE min_cnt_per:
   DEFINE INPUT  PARAMETER iRec  AS RECID       NO-UNDO.
   DEFINE INPUT  PARAMETER iAmt  AS DECIMAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE        NO-UNDO.
   DEFINE OUTPUT PARAMETER oMess AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vAcctLst      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBegDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vEndDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vMaxCnt       AS INT64      NO-UNDO.
   DEFINE VARIABLE vPeriod       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKauSurr      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcct         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAmount       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vFAmount      AS DECIMAL    NO-UNDO.  /* �㬬� ��ࢮ��砫쭮�� ����� */
   DEFINE VARIABLE vMaxAmount    AS DECIMAL    NO-UNDO.  /* �������쭮 �����⨬� ���⮪ */
   DEFINE VARIABLE vMaxAmtChr    AS CHARACTER  NO-UNDO.  /* �������쭮 �����⨬� ���⮪ � ��ࠬ��� ����⋨��� */
   DEFINE VARIABLE vListDate     AS CHARACTER  NO-UNDO.  /* ᯨ᮪ ��� ����⨩ */
   DEFINE VARIABLE tmp-date AS DATE NO-UNDO.
   DEFINE VARIABLE end-dat1 AS DATE NO-UNDO.
   DEFINE VARIABLE vI AS INT64 NO-UNDO.

   DEFINE BUFFER bkau-entry FOR kau-entry.
   /* ***************************  Main Block  *************************** */

   MAIN:
   DO 
      ON ERROR   UNDO MAIN, LEAVE MAIN
      ON END-KEY UNDO MAIN, LEAVE MAIN:
      
      FIND FIRST loan WHERE RECID (loan) EQ iRec
         NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan THEN
      DO:
         oMess =  "�訡�� �� ��।������ ������.".
         UNDO Main, LEAVE Main.   
      END.
      ELSE DO:

         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                          iDate,
                                          OUTPUT vBegDate,
                                          OUTPUT vEndDate).                                           

         vKauSurr  = loan.contract + "," + loan.cont-code + "," + IF vEndDate = ? THEN "��₪��" ELSE "��₪��".

         /*����稬 ��������� ���⮪ ������*/
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           '����⋨���',
                                           OUTPUT vMaxAmtChr).
         IF {assigned vMaxAmtChr} THEN DO: 
            IF vMaxAmtChr BEGINS "=" THEN vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
               oMess =  "�訡�� �� ��।������ ��᭨������� ���⪠ ������ ����⋨��� " + vMaxAmtChr.
               UNDO Main, LEAVE Main.
            END.
            IF vMaxAmtChr BEGINS "%" THEN DO:
               IF vBegDate = loan.open-date THEN
                     vFAmount = DEC(GetXAttrValueEx("loan",
                                    loan.contract + "," + loan.cont-code,
                                    "�㬬���᫊�����",
                                    "0")).
               IF vBegDate > loan.open-date OR vFAmount = 0 THEN DO:
                  /*����祭�� �᭮����� ���*/
                  tmp-date = IF vBegDate > loan.open-date THEN vBegDate - 1 ELSE vBegDate.
                  FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ (IF vEndDate = ? THEN "loan-dps-p" ELSE "loan-dps-t")
                  AND loan-acct.since LE tmp-date NO-LOCK NO-ERROR.
                  IF NOT AVAIL loan-acct THEN
                  DO:
                     oMess =  "�訡�� �� ��।������ �᭮����� ��� ������.".
                     UNDO Main, LEAVE Main.   
                  END.

                  RUN  kau-pos.p(loan-acct.acct,
                     loan-acct.currency,
                     tmp-date,
                     tmp-date,
                     gop-status,
                     vKauSurr).
               vFAmount = ABS(IF loan.currency eq '' then ksh-bal  else ksh-val) .
               END.
               vMaxAmount = DEC(SUBSTRING(vMaxAmtChr,2)) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                     oMess =  "�訡�� �� ��।������ ��᭨������� ���⪠ ������ ����⋨��� " + vMaxAmtChr.
                     UNDO Main, LEAVE Main.
                  END.
               vMaxAmount = vMaxAmount * vFAmount / 100.
            END.
         END.
         ELSE DO:
                     oMess =  "�訡�� �� ��।������ ��᭨������� ���⪠ ����⋨���".
                     UNDO Main, LEAVE Main.
         END.
         /*�⠪, � vMaxAmount ��������� ���⮪ ������*/
                  /*����祭�� �᭮����� ���*/
                  FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ (IF vEndDate = ? THEN "loan-dps-p" ELSE "loan-dps-t")
                  AND loan-acct.since LE iDate NO-LOCK NO-ERROR.
                  IF NOT AVAIL loan-acct THEN
                  DO:
                     oMess =  "�訡�� �� ��।������ �᭮����� ��� ������.".
                     UNDO Main, LEAVE Main.   
                  END.
                  RUN  kau-pos.p (loan-acct.acct,
                     loan-acct.currency,
                     iDate,
                     iDate,
                     gop-status,
                     vKauSurr).
               vAmount = ABS(IF loan.currency eq '' then ksh-bal  else ksh-val) .
         IF vAmount - iAmt < vMaxAmount THEN DO:
            oMess = "�ॢ�襭�� �����⨬�� �㬬� ������ (�����⨬� " + STRING(vAmount - vMaxAmount)  + ")!".
                     UNDO Main, LEAVE Main.
         END.

         /*����稬 ���ᨬ��쭮� ������⢮ ����⨩ �� ��ਮ� */
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           '����⊮����',
                                           OUTPUT vMaxCnt).
         RUN get_last_param    IN h_dpspc (RECID(loan),
                                           iDate,
                                           iDate,
                                           '����⏥ਮ�',
                                           OUTPUT vPeriod).
         IF vMaxCnt = ? AND NOT {assigned vPeriod} THEN DO:
                     oMess =  "".
                     UNDO Main, LEAVE Main.
         END.
         IF vMaxCnt = ? AND {assigned vPeriod} THEN vMaxCnt = 1.
         RUN get_acct IN h_dpspc (RECID(loan),vBegDate,iDate,OUTPUT vAcctLst).
         /* �� �ᥬ ������� ��⠬ �ந������ ������ ���-�� ����⨩ � ��⠢������ ᯨ᪠ ��� */
         tmp-date = vBegDate.
         vListDate = "".
         DO vI = 1 TO NUM-ENTRIES(vAcctLst) :
      
         FIND FIRST acct WHERE acct.acct = ENTRY(vI,vAcctLst) AND acct.currency = loan.currency  NO-LOCK NO-ERROR.
         IF NOT AVAIL acct THEN NEXT.
         tmp-date = IF vI > 1 THEN DATE(ENTRY(vI - 1,vAcctLst)) ELSE vBegDate.
         end-dat1  = IF NUM-ENTRIES(vAcctLst) >= vI + 3 THEN DATE(ENTRY(vI + 3,vAcctLst)) ELSE vEndDate.
         FOR EACH kau-entry WHERE kau-entry.acct = acct.acct AND 
                               kau-entry.currency = acct.currency AND 
                               kau-entry.kau = vKauSurr AND
                               kau-entry.debit AND 
                               kau-entry.op-date >= tmp-date AND
                               kau-entry.op-date <= end-dat1  NO-LOCK,
                               EACH op-entry OF kau-entry 
                               WHERE NOT CAN-FIND(FIRST bkau-entry OF op-entry WHERE bkau-entry.kau = vKauSurr AND NOT bkau-entry.debit):
            {additem.i vListDate STRING(op-entry.op-date)}
         END.

         END.   /*�� �ᥬ vAcctLst*/

         IF NUM-ENTRIES(vListDate) >= vMaxCnt THEN 
         DO:
           tmp-date = DATE(ENTRY(NUM-ENTRIES(vListDate) - vMaxCnt + 1,vListDate)).
           end-dat1 = IF  {assigned vPeriod} THEN Get-end-date(tmp-date,vPeriod) ELSE vEndDate.
           IF end-dat1 = ? THEN DO:
                     oMess =  "".
                     UNDO Main, LEAVE Main.
           END.
           IF end-dat1 >= iDate THEN DO:
                     oMess =  "���௠�� �����⨬�� ������⢮ ����⨩ �� " + STRING(end-dat1) + " �����⥫쭮!".
                     UNDO Main, LEAVE Main.
           END.
         END.
      END.
   END.
   /* *************************  End of Main Block  ********************** */

END PROCEDURE.

