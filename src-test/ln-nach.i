DEF NEW GLOBAL SHARED TEMP-TABLE filost NO-UNDO
      FIELD type    AS INT64
      FIELD balance LIKE loan-var.balance
      FIELD since   LIKE term-obl.end-date
INDEX since IS UNIQUE type since ASCENDING
INDEX s since .


DEF TEMP-TABLE ttCorr NO-UNDO
   FIELD end-date AS DATE
   FIELD Summa    AS DEC
   FIELD Corr     AS DEC
INDEX end-date end-date.
.
DEF TEMP-TABLE tt-trans-sum NO-UNDO
   FIELD contcode AS CHAR
   FIELD since    AS DATE
   FIELD Summa    AS DEC EXTENT 10 INIT ?
INDEX s since contcode
INDEX c contcode
.

&GLOB GCodePeny "����"  /* ��� �⠢�� "����" */
&GLOB GCodePenyFix "����="  /* ��� �⠢�� "���� � " */
&Glob CodOstPar
DEF VAR CodOstPar AS INT64 NO-UNDO .
DEF VAR vIshOst   AS LOG  NO-UNDO.

FUNCTION SetCodPar RETURNS LOG(INPUT iClass AS CHAR):
 CodOstpar = GetParCode(iClass,'����᭄���') .
END.

{w-ost.i}

/* �����頥� ��� ��業⭮� �⠢��.
** ������ ���, ������ ��訢����� � ����ன��� ��.
** ��� ࠡ��� ������ ��楤��� ����室�� ������ svarloan.def
*/
PROCEDURE GetCodeRate.

   DEF INPUT  PARAM ipPrmInt   AS INT64  NO-UNDO. /* ���浪��� ����� %%, ����
                                                   loan.interest[cod-par] */
   DEF OUTPUT PARAM opRateChar AS CHAR NO-UNDO. /* ��� ���/�����ᨨ */
   DEF PARAM  BUFFER loan FOR loan.

   DEF VAR vCommChar AS CHAR
                        INIT "1,2,3,4,5,2,3,4,5,6,4,5,7,8"
                        NO-UNDO. /* ���ᨢ �裡 ��業⮢ � �����ᨩ */

   opRateChar = lrate [lr-st + INT64(ENTRY(ipPrmInt, vCommChar)) - 1].

END PROCEDURE.

/* �������� ��������� �᫮��� � ���ࢠ�� ���
** ��ନ��� �� ���� �᫮��� ��� �奬� ���᫥��� ��業⮢ */
PROCEDURE GetCondDate.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* ��� ��砫� */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* ��� ����砭�� */

   DEF VAR vActualSchChar AS CHAR NO-UNDO. /* ��� �奬� ���᫥��� ��業⮢ */
   DEF VAR vCurrEndDate   AS DATE NO-UNDO. /* ������ ��� ����砭��
                                              ���ࢠ�� */

   DEF BUFFER next-loan-cond FOR loan-cond.

   /* ��������� � ���ࢠ�� ��� */
   DO WHILE ipBegDate LE ipEndDate:

      /* ���� ���㠫쭮�� �᫮��� */
      RUN RE_L_COND IN h_Loan
                    (ipContractChar,
                     ipContCodeChar,
                     ipBegDate,
                     BUFFER loan-cond).

      /* ���� ᫥���饣� �᫮��� */
      RUN RE_L_COND_FRST IN h_Loan
                         (ipContractChar,
                          ipContCodeChar,
                          ipBegDate + 1,
                          BUFFER next-loan-cond).

      /* ��।������ ���� ����砭�� ���ࢠ�� ���᫥��� */

      vCurrEndDate   = IF AVAIL next-loan-cond
                       THEN next-loan-cond.since - 1
                       ELSE ipEndDate.

      /* ����祭�� ���� �奬� ���᫥��� ��業⮢
      ** ��᫥ ��ࠡ�⪨ ��࠭���� ���� �奬�,
      ** ᪮�४�஢��� ⥪�騩 ��� */

      IF NOT AVAILABLE loan-cond THEN
      DO:
         ipBegDate = vCurrEndDate + 1.
         NEXT.
      END.

      vActualSchChar = &IF DEFINED(prefix) = 0
                         &THEN
                         "��_"
                         &ELSE
                         "{&prefix}"
                         &ENDIF   + string(loan-cond.disch-type).

      /* ��������/��������� ⠡���� LnShPrm */
      RUN SetLnShPrm (ipBegDate, vActualSchChar, ?, ?, ?, ?, ?, ?, ?, ?,1,?,?).

      ipBegDate      = vCurrEndDate + 1.

   END.

   RETURN.

END PROCEDURE. /*PROCEDURE GetCondDate.*/

/* ��������� ��ਮ�� �� ���ࢠ�� ������⢠ ��� �� ஫�.
** 㦥 �� �ॡ���� �.� �� ��易�� �⠢�� �� ���
*/
PROCEDURE GetLnAcct.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM ipAcctTypeChar AS CHAR NO-UNDO. /* ���� ��� */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "��" ����砭��
                                                       ���ࢠ�� */

   DEF BUFFER loan-acct FOR loan-acct.

   /* ��।��塞 �������騥 ��� � ���ࢠ�� ��� */
   DO WHILE ipBegDate LE ipEndDate:

      /* ���� ��� �� ���� ipBegDate */
      run RE_L_ACCT IN h_Loan
                    (ipContractChar,
                     ipContCodeChar,
                     ipAcctTypeChar,
                     ipBegDate,
                     BUFFER loan-acct).

      IF AVAILABLE  loan-acct THEN
         /* ��������/��������� ⠡���� LnShPrm */
         RUN SetLnShPrm (ipBegDate, ?, loan-acct.acct,
                         loan-acct.currency, ?, ?, ?, ?, ?, ?,1,?,?).

      /* ���� ᫥���饩 �ਢ離� */
      RUN RE_L_ACCT_FRST IN h_Loan
                        (ipContractChar,
                         ipContCodeChar,
                         ipAcctTypeChar,
                         ipBegDate + 1,
                         BUFFER loan-acct).

      ipBegDate = IF AVAIL loan-acct
                  THEN loan-acct.since
                  ELSE ipEndDate + 1.
   END.

END PROCEDURE. /*PROCEDURE GetLnAcct.*/

/* ��ନ஢���� �������� �奬 ���᫥��� */
PROCEDURE GetLnScheam.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "��" ����砭��
                                                       ���ࢠ�� */

   DEF VAR vSchRecid   AS RECID NO-UNDO. /* �����䨪��� �� ���᫥��� */
   DEF VAR vEndSchDate AS DATE  NO-UNDO. /* ��� ����砭�� ⥪�饩
                                            ��. ���᫥��� */

   DEF BUFFER xLnShPrm FOR LnShPrm.

   /* "��������" ��।������ �奬 � ���ࢠ� ��� */
   DO WHILE ipBegDate LE ipEndDate:

      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.


      IF AVAIL LnShPrm THEN
      DO:
         /* ���� ���㠫��� �奬� �� ���� */
         RUN get_current_loan_scheme IN h_Loan
             (LnShPrm.IntSch, /*  ��� �奬� ���᫥��� %% */
              /*LnShPrm.since,*/
              ipBegDate, /*by ���� - ���᭨��: ����� �奬� �������� ��᫥
                           ��砫� ������� �ந�室�� ��横�������*/
              OUTPUT vSchRecid).

         /* ��������/��������� ⠡���� LnShPrm */
         RUN SetLnShPrm (ipBegDate, ?, ?, ?, vSchRecid, ?, ?, ?, ?, ?,1,?,?).

         /* ����� ���� ����砭�� ����⢨� ⥪�饩 �奬� ���᫥��� */
         RUN get_next_date_loan_scheme  IN h_Loan
             (LnShPrm.IntSch, /*  ��� �奬� ���᫥��� %% */
              /*LnShPrm.since,*/
              ipBegDate, /*by ���� - ���᭨��: ����� �奬� �������� ��᫥
                           ��砫� ������� �ந�室�� ��横�������*/
              OUTPUT vEndSchDate).

         /*� �᫨ ����⢨� ⥪�饩 �奬� �����稢����� �����, 祬
           ��稭����� ����⢨� ��㣮�*/
         FIND FIRST xLnShPrm WHERE
                    xLnShPrm.since  GT ipBegDate
                AND xLnShPrm.IntSch NE LnShPrm.IntSch
            NO-LOCK NO-ERROR.

         IF AVAILABLE xLnShPrm THEN
            vEndSchDate  = IF vEndSchDate <> ? THEN
                              MIN(vEndSchDate,xLnShPrm.since)
                           ELSE
                              xLnShPrm.since.

         /* ���४�஢�� ���� ��砫� ᫥���饩 ���樨 */
         /*  */
         ipBegDate = IF  vEndSchDate NE ? AND
                         vEndSchDate LE ipEndDate
                     THEN vEndSchDate
                     ELSE ipEndDate + 1.
      END.
      ELSE
      DO:

         /* ���� ��ࢮ� �����襩�� ����� � ���� ��砫� ���ࢠ�� */
         FIND FIRST LnShPrm WHERE
                    LnShPrm.since GT ipBegDate
            NO-ERROR.

         /* ���४�஢�� ���� ��砫� ᫥���饩 ���樨 */
         ipBegDate = IF AVAIL LnShPrm
                     THEN LnShPrm.since
                     ELSE ipEndDate + 1.
      END.
   END.

END PROCEDURE.

/* ����祭�� �������� ���⪠ �� ��ࠬ��ࠬ �������.
** ����� ��砩 - ����祭�� ���祭�� �� ������ ��ࠬ����. */
PROCEDURE GET_REM_BY_PRM.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM ipPrmChar      AS CHAR NO-UNDO. /* ��ࠬ���� ��� ����
                                                      ���⪠. */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "��" ����砭��
                                                      ���ࢠ�� */
   DEF INPUT PARAM iCodPar        AS INT64  NO-UNDO. /* */
   DEF INPUT PARAM iCurrency      AS CHAR NO-UNDO. /* �����*/
   DEF INPUT PARAM iSince         AS DATE NO-UNDO. /* ��� ���ﭨ� �������*/
   DEF INPUT PARAM iRecalcLoan    AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP      AS LOG  NO-UNDO.


   DEF VAR vChgDate     AS DATE NO-UNDO. /* ��� ����������� ��ଥ�� */
   DEF VAR vCurrPrmInt  AS INT64  NO-UNDO. /* i-� ��ࠬ��� */
   DEF VAR vTotalPrmDec AS DEC  NO-UNDO. /* �㬬� ��ଥ�஢ �� ���� */
   DEF VAR vPrmValDec   AS DEC  NO-UNDO. /* �㬬� i-��� ��ࠬ��� */
   DEF VAR vDbDec       AS DEC  NO-UNDO. /* �㬬� ����⮢�� ����⮢ */
   DEF VAR vCrDec       AS DEC  NO-UNDO. /* �㬬� �।�⮢�� ����⮢ */
   /* ���������� � 㪠����� ���ࢠ� ��� */
   DO WHILE ipBegDate LE ipEndDate:
      vTotalPrmDec = 0.
      DO vCurrPrmInt = 1 TO NUM-ENTRIES (ipPrmChar):
         IF INT64 (ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar
            AND iCodPar = 1
            AND NOT iRecalcLoan
            AND NOT iRecalcPP THEN
         DO:
            RUN RE_PARAM IN h_Loan
                         (INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                          ipBegDate - IF vIshOst THEN 0 ELSE 1, /* ��� ���� ��ࠬ��� */
                          ipContractChar,
                          ipContCodeChar,
                          OUTPUT vPrmValDec,
                          OUTPUT vDbDec,
                          OUTPUT vCrDec).

            RUN CORR_DOLG_SUMM (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                                INPUT-OUTPUT vPrmValDec ).
         END.
         ELSE
            IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar
                    AND iCodPar = 1
                    AND iRecalcPP THEN
               RUN GetFilOstSumm(ipBegDate - IF vIshOst THEN 0 ELSE 1,
                                 CodOstPar,
                          OUTPUT vPrmValDec).
            ELSE
         /* ���� ��⠢��  ��� ��᮪
         IF INT64 (ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar AND
            iCodPar = 1
         THEN
         DO:

            RUN RE_PARAM IN h_Loan
                         (INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                          ipBegDate - IF vIshOst THEN 0 ELSE 1, /* ��� ���� ��ࠬ��� */
                          ipContractChar,
                          ipContCodeChar,
                          OUTPUT vPrmValDec,
                          OUTPUT vDbDec,
                          OUTPUT vCrDec).

            RUN CORR_DOLG_SUMM (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                               INPUT-OUTPUT vPrmValDec ).
         END.
         ELSE
         */
            /* ����祭�� ���祭�� ��ࠬ��� */
               RUN STNDRT_PARAM IN h_Loan
                             (ipContractChar,
                              ipContCodeChar,
                              INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                              ipBegDate - IF vIshOst THEN 0 ELSE 1, /* ��� ���� ��ࠬ��� */
                              OUTPUT vPrmValDec,
                              OUTPUT vDbDec,
                              OUTPUT vCrDec).

            vTotalPrmDec = vTotalPrmDec + vPrmValDec.
         END.

         /* ���४�஢�� ���⪠ */
         RUN SetLnShPrm (ipBegDate, ?, ?, iCurrency,?,vTotalPrmDec, ?, ?, ?, ?,1,?,?).

        /* ����祭�� ���� ��������� ��ࠬ��� */
        RUN GetChgDateParam (ipContractChar,
                             ipContCodeChar,
                             ipPrmChar,          /* ���᮪ ��������� ��ࠬ��஢ */
                             ipBegDate - IF vIshOst THEN 0 ELSE 1, /* �� ����� ���� ᬮ���� */
                             NOT iRecalcPP,
                             OUTPUT vChgDate).

        IF iCodPar = 1
           AND LOOKUP(string(CodOstPar),ipPrmChar) <> 0
           AND NOT iRecalcLoan
           AND NOT iRecalcPP THEN
            RUN CORR_NEXT_DATE (ipBegDate - IF vIshOst THEN 0 ELSE 1 ,
                   INPUT-OUTPUT vChgDate).

        /* ���४�஢�� ���� ⥪�饣� ���ࢠ�� */
        ipBegDate = IF vChgDate GT ipEndDate THEN
                       ipEndDate + 1
                    ELSE
                       vChgDate + IF vIshOst THEN 0 ELSE 1.  /* ���/�� ���⮪ */
     END.
  RETURN.
END PROCEDURE.

/* ��ନ஢���� ��������� ��������� ���⪠ �� ������� �㬬�� */
PROCEDURE GET_REM_BY_TERM.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM ipIdntInt      AS INT64  NO-UNDO. /* �����䨪��� ��������
                                                      ��魮�� */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "��" ����砭��
                                                      ���ࢠ�� */
   DEF INPUT PARAM iCurrency      AS CHAR NO-UNDO. /* �����*/
   DEF INPUT PARAM iForTransh     AS LOG  NO-UNDO.

   DEF BUFFER term-obl   FOR term-obl.

      /* ���� ���⪠ �� ���� ��砫� i-�� ��ਮ�� */
   FIND LAST term-obl WHERE
             term-obl.contract  EQ ipContractChar
         AND term-obl.cont-code EQ ipContCodeChar
         AND term-obl.idnt      EQ ipIdntInt
         AND term-obl.end-date  LT ipBegDate + (IF vIshOst THEN 1 ELSE 0)
   NO-LOCK NO-ERROR.

   /* ��ନ஢���� ���⪠ � ���ࢠ�� ��� */
   DO WHILE ipBegDate LE ipEndDate:
      /* ��ନ஢����/���४�஢�� ���⪠ */
      IF AVAIL term-obl THEN
      DO:
         IF iForTransh THEN
            RUN SetTranshSum (ipContCodeChar,ipBegDate,term-obl.amt-rub,1).
         ELSE
            RUN SetLnShPrm (ipBegDate,?,?,iCurrency,?,term-obl.amt-rub,?,?,?,?,1,?,?).
      END.
      /* ��।������ ���� ᫥���饣� ��������� ���⥦� */
      FIND FIRST term-obl WHERE
                 term-obl.contract  EQ ipContractChar
             AND term-obl.cont-code EQ ipContCodeChar
             AND term-obl.idnt      EQ ipIdntInt
             AND term-obl.end-date  GE ipBegDate + (IF vIshOst THEN 1 ELSE 0)
         NO-LOCK NO-ERROR.

      ipBegDate = IF AVAIL term-obl
                  THEN term-obl.end-date + 1
                  ELSE ipEndDate + 1.

   END.

   RETURN.

END PROCEDURE.

/* ��ନ஢���� �����ᨨ �� ����� */
PROCEDURE GET_COMM_BY_REM.

   DEF INPUT PARAM ipCommChar AS CHAR NO-UNDO. /* ��� �����ᨨ */
   DEF INPUT PARAM ipKauChar  AS CHAR NO-UNDO. /* ��� ��� */
   DEF INPUT PARAM ipBegDate  AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate  AS DATE NO-UNDO. /* "��" ����砭�� ���ࢠ�� */

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* �����䨪��� ��� */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* ����饥 ���祭�� %% �⠢��/��� */
   DEF VAR vCommTypeChar AS CHAR  NO-UNDO. /* �⠢��/��� */
   DEF VAR vCommCur      AS CHAR  NO-UNDO.

   DEF BUFFER bLnShPrm FOR LnShPrm.
   DEF BUFFER bc-rate  FOR comm-rate.

   /* ��ନ஢���� �����ᨨ � ���ࢠ�� ��� */
   DO WHILE ipBegDate LE ipEndDate:

      /* ���� ��ࠬ��஢ �� ���� */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* �᫨ ��ࠬ���� ��।�����,
      ** � ����⪠ ���᪠ ���祭�� �����ᨨ/��� */
      IF AVAIL LnShPrm THEN
      DO:
         FIND LAST bc-rate WHERE bc-rate.kau        EQ ipKauChar
                             AND bc-rate.commission EQ ipCommChar
                             AND bc-rate.since      LE ipBegDate
            NO-LOCK NO-ERROR.
         /* ����祭�� �����ᨨ/��� �� ���� */
         vRateDec = IF AVAIL bc-rate THEN bc-rate.rate-comm
                                     ELSE 0.
         ASSIGN
             vAcctRecid = ?
             vCommCur   = LnShprm.Currency

             /* ����祭�� ⨯�: "%,=,����" */
             vCommTypeChar = IF CAN-DO("*" + {&GCodePeny} + "*", ipCommChar)
                             THEN
                                {&GCodePeny}
                             ELSE
                             IF GET_COMM_TYPE (
                                  ipCommChar,         /* ��� �����ᨨ */
                                  vAcctRecid,         /* �����䨪��� ��� */
                                  vCommCur,           /* ��� ������ */
                                  ipKauChar,          /* ��� ���  */
                                  LnShPrm.balance[1], /* ��� ���⮪ */
                                  0,                  /* ��ਮ�/�ப */
                                  ipBegDate)
                             THEN "="
                             ELSE "%"
         .
          /* ��筨�, �� � ������ �� ���� 䨪�஢���� */
          IF vCommTypeChar = {&GCodePeny} THEN DO:
            IF GET_COMM_TYPE (
                  ipCommChar,         /* ��� �����ᨨ */
                  vAcctRecid,         /* �����䨪��� ��� */
                  vCommCur,           /* ��� ������ */
                  ipKauChar,          /* ��� ���  */
                  LnShPrm.balance[1], /* ��� ���⮪ */
                  0,                  /* ��ਮ�/�ப */
                  ipBegDate)
            THEN
              vCommTypeChar = {&GCodePenyFix}.
          END.

         /* ���४�஢��/ᮧ����� %% �⠢�� */
         RUN SetLnShPrm (ipBegDate, ?, ?, ?, ?, ?, ?,
                         vRateDec, vCommTypeChar, ?,1,?,?).


         /* ������ ���� ��������� ������ / ������ */
         ipBegDate = GET_NEXT_COMM_DATE (
                      ipCommChar,          /* ��� �����ᨨ */
                      vAcctRecid,          /* �����䨪��� ��� */
                      vCommCur,            /* ��� �ਢ������� ������ */
                      ipKauChar,           /* ��� ��� ("" - �� 㬮�砭��) */
                      LnShPrm.balance[1],  /* ��� ���⮪ (0 - ��㬮�砭��) */
                      0,                   /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                      ipBegDate,
                      ipEndDate).

      END.
      /* �᫨ ��ࠬ��஢ ��� ���᪠ �����ᨩ �� �������,
      ** � �饬 �� ���।� */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since GT ipBegDate
         NO-ERROR.

         /* � �᫨ �� ���,
         ** � � �� ���� �᪠�� �������/��� */
         ipBegDate = IF AVAIL LnShPrm
                     THEN LnShPrm.since
                     ELSE ipEndDate + 1.
      END.
   END.

   RETURN.

END PROCEDURE.

/* ���᫥��� ��業⮢ � �ନ஠��� ���� */
PROCEDURE GET_NAC_REP.

   DEF INPUT PARAM ipSchmRecid AS RECID NO-UNDO. /* ��� �奬� ���᫥��� */
   DEF INPUT PARAM ipBegDate   AS DATE  NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate   AS DATE  NO-UNDO. /* "��" ����砭�� ���ࢠ�� */
   DEF INPUT PARAM ipFormRasch AS CHAR  NO-UNDO. /* ���㫠 ��� ���� ��業⮢ � �� �롔�ଐ����� */

   DEF VAR vEndDate AS DATE NO-UNDO. /* ����砭�� ��ਮ�� ���᫥��� ��業⮢*/
   DEF VAR vDaysInt AS INT64  NO-UNDO. /* ������⢮ ���� � ��ਮ�� */

   DEF BUFFER buf-lnFost        FOR LnShPrm.
   DEF BUFFER interest-sch-line FOR interest-sch-line.

   /* ���� �����ᨨ */
   RUN get_sch_line_by_rid IN h_schem
       (ipSchmRecid,
        BUFFER interest-sch-line).

   /* ���� ��ࠬ��஢ ���᫥��� �� ���� */
   FIND LAST LnShPrm WHERE
             LnShPrm.since LE ipBegDate
      NO-ERROR.

   /* ���� ��ࠬ��஢ ���᫥��� ���। */
   FIND FIRST buf-lnFost WHERE
              buf-lnFost.since GT ipBegDate
      NO-ERROR.

   /* ��।������ ���� ��砫� ���᫥���,
   ** ��室� �� ������ ��ࠬ��஢ ���᫥��� */
   ipBegDate = IF AVAIL LnShPrm
               THEN ipBegDate
               ELSE IF AVAIL buf-lnFost
                    THEN buf-lnFost.since
                    ELSE ipEndDate.

   /* ���������� � ��⠭������� (᪮�४�஢����)
   ** ���ࢠ� ��� */
   DO WHILE ipBegDate LE ipEndDate:

      /* ��室�� ��ࠬ���� ���� */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* ���� ᫥����� ��ࠬ��஢ */
      FIND FIRST buf-lnFost WHERE
                 buf-lnFost.since GT ipBegDate
         NO-ERROR.

      /* ����祭�� ���� ����砭�� ⥪�饣� ��ਮ�� */
      vEndDate = IF AVAIL buf-lnFost
                 THEN buf-lnFost.since - 1
                 ELSE ipEndDate.

      /* ��।������ ���-�� ���� � ���ࢠ�� */
      vDaysInt = cDay(interest-sch-line.interest-month,
                      ipBegDate,
                      vEndDate + 1).

      /* otch1 - ���७�. � ��� � ᮧ���� ���� */
      IF AVAIL LnShPrm THEN
      DO:

         CREATE otch1.
         ASSIGN
             otch1.bal-summ = LnShPrm.balance[1]
             otch1.beg-date = ipBegDate
             otch1.end-date = vEndDate
             otch1.ndays    = vDaysInt
             otch1.rat1     = LnShPrm.rate[1]

             /* ����� ��業⭮� �⠢�� � ����ᨬ��� �� ⨯� */
             otch1.summ_pr =

             /* ����� ���祭�� �� � ⨯�� "�⠢��". */
             IF    LnShPrm.CommType[1] EQ "%"
               AND ipFormRasch EQ "1" THEN
                       /*�� � ���*//*     �㬬�     */   /* �⠢��    */    /*       ���� � ����      */
                ROUND ((vDaysInt * (LnShPrm.balance[1] * LnShPrm.rate[1] / (interest-sch-line.basis-time * 100))), 2)
             ELSE
             IF    LnShPrm.CommType[1] EQ "%"
              AND ipFormRasch EQ "2" THEN
                      /*      �㬬�     */  /*  �⠢��   */ /*100*/ /*�� � ���*/ /*       ���� � ����      */
               ROUND(((LnShPrm.balance[1] * LnShPrm.rate[1] / 100 ) * vDaysInt / interest-sch-line.basis-time),2)

             /* ����� ���祭�� � ⨯�� "���". */
             ELSE IF LnShPrm.CommType[1] EQ "="
             THEN ROUND(LnShPrm.rate[1], 2)

             /* ����� ���祭�� � ⨯�� "����". */
             ELSE IF LnShPrm.CommType[1] EQ {&GCodePeny} THEN
                ROUND ((vDaysInt * (LnShPrm.balance[1] *
                                    LnShPrm.rate[1] / 100)), 2)

             /* ����� ���祭�� � ⨯�� "���� = 䨪�஢�����". */
             ELSE IF LnShPrm.CommType[1] EQ {&GCodePenyFix} THEN
                IF LnShPrm.balance[1] > 0
                  THEN ROUND ((vDaysInt *  LnShPrm.rate[1]) , 2)
                  ELSE 0
             ELSE ?

             otch1.summ_pr  = IF otch1.summ_pr EQ ?
                              THEN 0
                              ELSE otch1.summ_pr

         .

      END.

      ipBegDate = vEndDate + 1.

   END.

   RETURN.

END PROCEDURE.

/* �맮� �奬 ���᫥��� ��業⮢ �� ������ࠬ
** �� ��ନ஢����� �६����� ⠡��� */
PROCEDURE RunLnScheam.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* ��� ��砫� */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* ��� ����砭�� */
   DEF INPUT PARAM dat-per        AS DATE NO-UNDO. /* ��� ���室� �� 39� */
   DEF INPUT PARAM cod-par        AS INT64  NO-UNDO. /* ��� ��ࠬ��� �� ��. */
   DEF INPUT PARAM fl-type-ost    AS INT64  NO-UNDO. /* �ᥣ�� ��।�����
                                                      1 �� ��. */

   DEF VAR vEndDate    AS DATE        NO-UNDO. /* ��� ����砭�� ⥪�饣� ���ࢠ�� */
   DEF VAR vLMDateTmp  AS DATE        NO-UNDO.
   DEF VAR vIsPrSd     AS CHAR        NO-UNDO. /* �� �� �᫮��� ���뢠�� �� ᤢ�� ��業⮢ */
   DEF VAR vBegDateDay AS INT64       NO-UNDO.
   DEF VAR vTmp        AS CHAR        NO-UNDO. /* �� �� �᫮��� ���뢠�� �� ᤢ�� ��業⮢ */
   DEF VAR vI          AS INT64       NO-UNDO.
   DEF VAR vFlag       AS LOGICAL     NO-UNDO.
   DEF VAR vDate       AS DATE        NO-UNDO.

   DEF BUFFER buf-lnFost FOR LnShPrm.
   DEF BUFFER buf-lnTmp  FOR LnShPrm.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bloan      FOR loan.
   DEF BUFFER bpobl      FOR term-obl.

   /* ���� �奬� ࠭�� ���� ��砫� ���ࢠ�� */
   FIND LAST LnShPrm WHERE
             LnShPrm.since LE ipBegDate
      NO-ERROR.

   /* ���� �奬� ����� ���� ��砫� ���ࢠ�� */
   FIND FIRST buf-lnFost WHERE
              buf-lnFost.since GT ipBegDate
      NO-ERROR.

   /* ��।������ ���� ��砫� ���᫥���,
   ** ��室� �� ������ �奬 ���᫥��� */
   ipBegDate = IF AVAIL LnShPrm
               THEN ipBegDate
               ELSE IF AVAIL buf-lnFost
                    THEN buf-lnFost.since
                    ELSE ipEndDate.

      IF AVAILABLE LnShPrm THEN DO:
         IF CAN-DO("��_1,��_17",LnShPrm.IntSch)
         THEN DO:
            FIND LAST bloan-cond WHERE bloan-cond.contract  EQ ipContractChar
                                 AND bloan-cond.cont-code EQ ipContCodeChar
                                 AND bloan-cond.since     LE ipBegDate
            NO-LOCK NO-ERROR.

            vIsPrSd    = GetXAttrValue("loan-cond",
                                       bloan-cond.contract + "," +  bloan-cond.cont-code + "," + STRING(bloan-cond.since),
                                       "���瑤").
            IF vIsPrSd NE "���"
            THEN
               vIsPrSd = GetXattrInit(bloan-cond.class-code,"���瑤").
         END.

         /* ���樠��� �᫮��� ���᫥��� ��業⮢ */
         IF    (   (    LnShPrm.IntSch EQ "��_17")
               OR (    LnShPrm.IntSch EQ "��_1"
                  AND vIsPrSd        EQ "���")
               )
            AND (GetSysConf("calc-s1") NE "yes")
            AND cod-par EQ 1

         THEN DO:

            FIND FIRST bloan WHERE bloan.contract  EQ ipContractChar
                              AND bloan.cont-code EQ ipContCodeChar
            NO-LOCK NO-ERROR.

            IF GetSysConf("calc-loan-state") NE "yes"
            THEN DO:
               IF    vIsPrSd        EQ "���"
                  OR LnShPrm.IntSch EQ "��_17"
               THEN DO:
                  vBegDateDay = IF bloan-cond.cred-date EQ 31
                              THEN 1
                              ELSE (bloan-cond.cred-date + 1).
                  vLMDateTmp  = Date(MONTH(ipBegDate), 1, YEAR(ipBegDate)) - 1.
                  FND_BegDate:
                  DO WHILE     DAY(ipBegDate) NE vBegDateDay
                           AND ipBegDate      GT (bloan.open-date + 1)
                  :
                     ipBegDate = ipBegDate - 1.
                     IF     ipBegDate       EQ vLMDateTmp
                        AND DAY(vLMDateTmp) LE vBegDateDay
                     THEN
                        LEAVE FND_BegDate.
                  END.

                  LnShPrm.since = ipBegDate.
                  vLMDateTmp    = Date(MONTH(ipEndDate), 1, YEAR(ipEndDate)) - 1.

                  FND_EndDate:
                  DO WHILE     DAY(ipEndDate) GT bloan-cond.cred-date
                           AND ipEndDate ne bloan.end-date
                  :
                     ipEndDate = ipEndDate - 1.
                     IF     ipEndDate EQ vLMDateTmp
                        AND DAY(vLMDateTmp) LE bloan-cond.cred-date
                     THEN
                        LEAVE FND_EndDate.
                  END.
                  IF LnShPrm.IntSch EQ "��_17"
                  THEN DO:
                     IF     ipBegDate        LE (bloan.open-date + 1)
                        AND DAY(ipBegDate)   LE bloan-cond.cred-date
                        AND MONTH(ipEndDate) EQ MONTH(ipBegDate)
                        AND YEAR(ipEndDate)  EQ YEAR(ipBegDate)
                     THEN DO:
                        RETURN. /* �� ����塞 ��業�� (��� �।���饣� �����) */
                     END.
                     
                     vDate = DATE(MONTH(ipBegDate),bloan-cond.cred-date,YEAR(ipBegDate)) NO-ERROR.
                     /*  �訡�� ����� ���� ⮫쪮 �᫨ �� �।��� ����� ��᪮稬,
                     ** ⮣�� ��६ ��᫥���� ���� */
                     IF ERROR-STATUS:ERROR THEN
                        vDate = LastMonDate(ipBegDate).

                     BLOCK-DATE:
                     DO WHILE vDate LE ipEndDate:
                        IF vDate GE ipBegDate
                        THEN DO:
                           vFlag = TRUE.
                           LEAVE BLOCK-DATE.

                        END.
                        vDate = GoMonth(vDate,1).
                     END.

                     IF NOT vFlag 
                     THEN
                        RETURN. /* �� ����塞 ��業�� (���� ���᫥��� ��� � ��ਮ��) */

                     IF ipBegDate NE (bloan.open-date + 1)
                     THEN DO:
                        ipBegDate = max(bloan.open-date + 1, FirstMonDate(GoMonth(ipEndDate, -1))).
                     END.

                     IF ipEndDate EQ bloan.end-date
                     THEN DO:
                        IF DAY(ipEndDate) LT bloan-cond.cred-date
                        THEN
                           ipEndDate = LastMonDate(ipBegDate).
                        ELSE IF DAY(ipEndDate) GT bloan-cond.cred-date
                        THEN DO:
                           ipBegDate = FirstMonDate(ipEndDate).
                        END.

                     END.
                     ELSE
                     DO:
                        ipEndDate = LastMonDate(LnShPrm.since).
                     END.
                     LnShPrm.since = ipBegDate.
                  END.
               END.
            END.
            ELSE
            DO:
               IF ipEndDate EQ LastMonDate(ipEndDate) THEN
                  ipBegDate = max(FirstMonDate(ipEndDate), (bloan.open-date)).
               ELSE
                  ipBegDate = max(FirstMonDate(ipEndDate), (bloan.open-date), ipBegDate).
              
               FIND LAST buf-lnTmp WHERE buf-lnTmp.since EQ ipBegDate NO-LOCK NO-ERROR.

                  IF NOT AVAIL buf-lnTmp
                  THEN
                     LnShPrm.since = ipBegDate.
            END.
         END.
   END.
   /* **************************************** */
   /* �맮� �奬 ���᫥��� ��業⮢ �� ������ࠬ */
   RUN_SCH:
   DO WHILE ipBegDate LE ipEndDate:

      /* ��室�� ��ࠬ���� ���� */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* ���� ᫥����� ��ࠬ��஢ */
      FIND FIRST buf-lnFost WHERE
                 buf-lnFost.since GT ipBegDate
         NO-ERROR.

      /* ����祭�� ���� ����砭�� ⥪�饣� ��ਮ�� */
      vEndDate = IF AVAIL buf-lnFost
                 THEN buf-lnFost.since - 1
                 ELSE ipEndDate.

      /* �� �����-� ��稭�� ��� �奬� ���᫥��� */
      IF NOT AVAIL LnShPrm OR LnShPrm.SchRecid EQ ? THEN
      DO:
        ipBegDate = vEndDate + 1.
        NEXT RUN_SCH.
      END.

      /* ����祭�� ������������ ��楤��� */
      RUN GET_SCH_LINE_BY_RID IN h_schem
            (LnShPrm.SchRecid,
             BUFFER interest-sch-line).

      /* ����� �奬� ���᫥��� */
      RUN VALUE (interest-sch-line.proc + ".p")
            (ipContractChar,        /* �����祭�� ������� */
             ipContCodeChar,        /* ����� ������� */
             LnShPrm.SchRecid,      /* �����䨪��� �奬� */
             ipBegDate,             /* ��� ��砫� ���� */
             vEndDate,              /* ��� ����砭�� ���� */
             dat-per,               /* ��� ���室� �� 39� */
             cod-par,               /* ��� ��ࠬ��� �� ��. */
             fl-type-ost)           /* �ᥣ�� ��।����� 1 �� ��. */
      NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
      DO:
         ipBegDate = vEndDate + 1.
         NEXT RUN_SCH.
      END.

      /* ���४�஢�� ���� ��章 ⥪�饣� ���ࢠ��. */
      ipBegDate = vEndDate + 1.
   END.

   RETURN.

END PROCEDURE.

/* ��������/��������� ⠡���� LnShPrn */
PROCEDURE SetLnShPrm.

   DEF INPUT PARAM ipDate         AS DATE  NO-UNDO.  /* ��� ��������� */
   DEF INPUT PARAM ipIntSchChar   AS CHAR  NO-UNDO.  /* ��� �奬� */
   DEF INPUT PARAM ipAcctChar     AS CHAR  NO-UNDO.  /* ��� */
   DEF INPUT PARAM ipCurrencyChar AS CHAR  NO-UNDO.  /* ������ */
   DEF INPUT PARAM ipSchRecid     AS RECID NO-UNDO.  /* �����䨪��� ��.
                                                        ���᫥��� */
   DEF INPUT PARAM ipBalanceDec   AS DEC   NO-UNDO.  /* ���⮪ �� ���� �
                                                       ����塞�� ����� */
   DEF INPUT PARAM ipAcctRemDec   AS DEC   NO-UNDO.  /* ���⮪ � ����� ���
                                                        (��ࠬ���) */
   DEF INPUT PARAM ipRateDec      AS DEC   NO-UNDO.  /* ���祭��
                                                        �⠢��/���� */
   DEF INPUT PARAM ipCommTypeChar AS CHAR  NO-UNDO.  /* ����஢����� �⠢��
                                                        / %% - �⠢��� */
   DEF INPUT PARAM ipOtherComm    AS LOG   NO-UNDO.  /*��� ��楤��� lnremva1.p
                                                       � �������� ����� ����
                                                       ���� ����� �������*/
   DEF INPUT PARAM iNumGrup       AS INT64   NO-UNDO.  /* ���浪��� �����
                                                        ��㯯� */
   DEF INPUT PARAM iMinRate       AS DEC   NO-UNDO.  /* ���. ���� ���. */
   DEF INPUT PARAM iMaxRate       AS DEC   NO-UNDO.  /* ����. ���� ���. */

   DEF VAR vCompareLog AS LOG NO-UNDO. /* १���� �ࠢ����� ���஢ */
   DEF VAR vNumGrup    AS INT64 NO-UNDO. /* ���稪 ����� ��㯯� */

   DEF BUFFER PrevLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm     FOR LnShPrm. /* �⮡� �� ���㡠��� ����� �
                                          ⠡��� ��᫥ ��楤��� */

   /* ���� ����� �� �室���� ���� */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   IF NOT AVAIL LnShPrm THEN
   DO:

      /* ᮧ���� ������ �� ���� */
      CREATE LnShPrm.

      LnShPrm.since = ipDate.
   END.

   ASSIGN
      LnShPrm.IntSch   = ipIntSchChar     WHEN ipIntSchChar   NE ?
      LnShPrm.acct     = ipAcctChar       WHEN ipAcctChar     NE ?
      LnShPrm.currency = ipCurrencyChar   WHEN ipCurrencyChar NE ?
      LnShPrm.SchRecid = ipSchRecid       WHEN ipSchRecid     NE ?
      LnShPrm.acct_rem = ipAcctRemDec     WHEN ipAcctRemDec   NE ?
      LnShPrm.OtherCom = ipOtherComm      WHEN ipOtherComm    NE ?
      LnShPrm.balance[iNumGrup]  = ipBalanceDec     WHEN ipBalanceDec   NE ?
      LnShPrm.rate[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
      LnShPrm.CommType[iNumGrup] = ipCommTypeChar   WHEN ipCommTypeChar NE ?
      LnShPrm.MinRate[iNumGrup]  = iMinRate         WHEN iMinRate       NE ?
      LnShPrm.MaxRate[iNumGrup]  = iMaxRate         WHEN iMaxRate       NE ?
      .

   RELEASE LnShPrm.

   /* ���४�஢�� ����ᥩ � i-�� ���� */
   FOR EACH LnShPrm WHERE
            LnShPrm.since GE ipDate:

      ASSIGN
         LnShPrm.IntSch   = ipIntSchChar     WHEN ipIntSchChar   NE ?
         LnShPrm.acct     = ipAcctChar       WHEN ipAcctChar     NE ?
         LnShPrm.currency = ipCurrencyChar   WHEN ipCurrencyChar NE ?
         LnShPrm.SchRecid = ipSchRecid       WHEN ipSchRecid     NE ?
         LnShPrm.acct_rem = ipAcctRemDec     WHEN ipAcctRemDec   NE ?
         /*LnShPrm.OtherCom = ipOtherComm      WHEN ipOtherComm    NE ?*/
         LnShPrm.balance[iNumGrup]  = ipBalanceDec     WHEN ipBalanceDec   NE ?
         LnShPrm.rate[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
         LnShPrm.CommType[iNumGrup] = ipCommTypeChar   WHEN ipCommTypeChar NE ?
         LnShPrm.MinRate[iNumGrup]  = iMinRate         WHEN iMinRate       NE ?
         LnShPrm.MaxRate[iNumGrup]  = iMaxRate         WHEN iMaxRate       NE ?
         .

      FIND LAST PrevLnShPrm WHERE
                PrevLnShPrm.since LT LnShPrm.since
      NO-ERROR.

      IF AVAIL PrevLnShPrm THEN
      DO:
         ASSIGN
            LnShPrm.IntSch   = PrevLnShPrm.IntSch   WHEN LnShPrm.IntSch   EQ ""
            LnShPrm.acct     = PrevLnShPrm.acct     WHEN LnShPrm.acct     EQ ""
            LnShPrm.currency = PrevLnShPrm.currency WHEN LnShPrm.currency EQ ""
            LnShPrm.SchRecid = PrevLnShPrm.SchRecid WHEN LnShPrm.SchRecid EQ ?
            LnShPrm.acct_rem = PrevLnShPrm.acct_rem WHEN LnShPrm.acct_rem EQ ?
            .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup]  = PrevLnShPrm.balance[vNumGrup]
                                           WHEN LnShPrm.balance[vNumGrup]  EQ ?
               LnShPrm.rate[vNumGrup]     = PrevLnShPrm.rate[vNumGrup]
                                           WHEN LnShPrm.rate[vNumGrup]     EQ ?
               LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                                           WHEN LnShPrm.CommType[vNumGrup] EQ ?
               LnShPrm.MinRate[vNumGrup] = PrevLnShPrm.MinRate[vNumGrup]
                                           WHEN LnShPrm.MinRate[vNumGrup] EQ ?
               LnShPrm.MaxRate[vNumGrup] = PrevLnShPrm.MaxRate[vNumGrup]
                                           WHEN LnShPrm.MaxRate[vNumGrup] EQ ?
               .
         END.
      END.
   END.

   /* ���� ����� �� ���� */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   /* ���� �।��饩 ����� */
   FIND LAST PrevLnShPrm WHERE
             PrevLnShPrm.since LT ipDate
      NO-ERROR.

   /* �஢�ઠ �� �������⭮��� ����ᥩ(�।��饩 � ⥪�饩) */
   IF AVAIL PrevLnShPrm THEN
   DO:

      /* �� �஢�ન �᪫�砥� ���� �� ������ ����砥� ���⮪. */
      BUFFER-COMPARE
         PrevLnShPrm EXCEPT since
         TO LnShPrm
         SAVE result IN vCompareLog
      NO-ERROR.

      /*�᫨ �訡�� ��� � 㤠�塞 ᮧ������ ������*/
      IF vCompareLog THEN
         DELETE LnShPrm.
   END.

   RETURN.

END PROCEDURE.

/* ��������/��������� ⠡���� LnShPrn */
PROCEDURE SetLnShPrmAdd.
   DEF INPUT PARAM ipCurrencyChar AS CHAR  NO-UNDO.  /* ������ */

   DEF VAR vContCode   AS CHAR  NO-UNDO. /* ��� �࠭�                  */
   DEF VAR vCompareLog AS LOG   NO-UNDO. /* १���� �ࠢ����� ���஢ */
   DEF VAR vNumGrup    AS INT64 NO-UNDO. /* ���稪 ����� ��㯯�       */
   DEF VAR vPrevGrup   AS INT64 NO-UNDO. /* ����� ��㯯�                */
   DEF VAR vPrevDate   AS DATE  NO-UNDO. /* ��� ��                     */
   DEF VAR vPrevSumm   AS DEC   EXTENT 10 NO-UNDO. /* �㬬� ��          */

   DEF BUFFER Prev-trans  FOR tt-trans-sum.
   DEF BUFFER PrevLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm     FOR LnShPrm. /* �⮡� �� ���㡠��� ����� �
                                          ⠡��� ��᫥ ��楤��� */
   FOR EACH tt-trans-sum BREAK BY tt-trans-sum.contcode:

       /* ���� ����� �� �室���� ���� */
      FIND FIRST LnShPrm WHERE
                 LnShPrm.since EQ tt-trans-sum.since
         NO-ERROR.
      IF FIRST(tt-trans-sum.contcode) THEN
         vContCode = tt-trans-sum.contcode.
      IF vContCode EQ tt-trans-sum.contcode THEN
      DO:
         IF NOT AVAIL LnShPrm THEN
         DO:
            /* ᮧ���� ������ �� ���� */
            CREATE LnShPrm.
            ASSIGN
               LnShPrm.since = tt-trans-sum.since
            .
            FIND LAST PrevLnShPrm WHERE PrevLnShPrm.since LT LnShPrm.since NO-ERROR.
            IF AVAIL PrevLnShPrm THEN
            DO:
               ASSIGN
                  LnShPrm.IntSch   = PrevLnShPrm.IntSch
                  LnShPrm.acct     = PrevLnShPrm.acct
                  LnShPrm.currency = PrevLnShPrm.currency
                  LnShPrm.SchRecid = PrevLnShPrm.SchRecid
                  LnShPrm.acct_rem = PrevLnShPrm.acct_rem
               .
               DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
                  ASSIGN
                     LnShPrm.rate[vNumGrup]    = PrevLnShPrm.rate[vNumGrup]
                     LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                  .
               END.
            END.
         END.
         ASSIGN
            LnShPrm.currency = ipCurrencyChar WHEN ipCurrencyChar NE ?
         .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup] = tt-trans-sum.Summa[vNumGrup]
                                           WHEN tt-trans-sum.Summa[vNumGrup] NE ?
            .
         END.

         RELEASE LnShPrm.
      END.
      ELSE DO:
         IF NOT AVAIL LnShPrm THEN
         DO:
            /* ᮧ���� ������ �� ���� */
            CREATE LnShPrm.
            ASSIGN
               LnShPrm.since = tt-trans-sum.since
            .
            FIND LAST PrevLnShPrm WHERE PrevLnShPrm.since LT LnShPrm.since NO-ERROR.
            IF AVAIL PrevLnShPrm THEN
            DO:
               ASSIGN
                  LnShPrm.IntSch   = PrevLnShPrm.IntSch
                  LnShPrm.acct     = PrevLnShPrm.acct
                  LnShPrm.currency = PrevLnShPrm.currency
                  LnShPrm.SchRecid = PrevLnShPrm.SchRecid
                  LnShPrm.acct_rem = PrevLnShPrm.acct_rem
               .
               DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
                  ASSIGN
                     LnShPrm.balance[vNumGrup]  = PrevLnShPrm.balance[vNumGrup]
                     LnShPrm.rate[vNumGrup]     = PrevLnShPrm.rate[vNumGrup]
                     LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                  .
               END.
            END.
         END.
         ASSIGN
            LnShPrm.currency = ipCurrencyChar WHEN ipCurrencyChar NE ?
         .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup] = LnShPrm.balance[vNumGrup]
                                         + tt-trans-sum.Summa[vNumGrup]
                                           WHEN tt-trans-sum.Summa[vNumGrup] NE ?
            .
         END.
         RELEASE LnShPrm.
         IF vPrevDate NE ? THEN
         FOR EACH LnShPrm WHERE LnShPrm.since GT vPrevDate
                            AND LnShPrm.since LT tt-trans-sum.since:

            DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
               LnShPrm.balance[vNumGrup] = LnShPrm.balance[vNumGrup]
                                         + vPrevSumm[vNumGrup].
            END.
         END.
         vPrevDate = tt-trans-sum.since.
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            vPrevSumm[vNumGrup] = tt-trans-sum.Summa[vNumGrup].
         END.

      END.
   END.
/*
   /* ���� ����� �� ���� */
   FOR EACH LnShPrm :

      /* ���� �।��饩 ����� */
      FIND LAST PrevLnShPrm WHERE
                PrevLnShPrm.since LT LnShPrm.since
         NO-ERROR.

      /* �஢�ઠ �� �������⭮��� ����ᥩ(�।��饩 � ⥪�饩) */
      IF AVAIL PrevLnShPrm THEN
      DO:
         /* �� �஢�ન �᪫�砥� ���� �� ������ ����砥� ���⮪. */
         BUFFER-COMPARE
            PrevLnShPrm EXCEPT since
            TO LnShPrm
            SAVE result IN vCompareLog
         NO-ERROR.

         /*�᫨ �訡�� ���, � 㤠�塞 ᮧ������ ������*/
         IF vCompareLog THEN
            DELETE LnShPrm.
      END.
   END.
*/

   RETURN.

END PROCEDURE.

/* ��������/��������� ⠡���� tt-trans-sum */
PROCEDURE SetTranshSum.
   DEF INPUT PARAM ipContCode     AS CHAR  NO-UNDO.  /* ��� �������   */
   DEF INPUT PARAM ipDate         AS DATE  NO-UNDO.  /* ��� ��������� */
   DEF INPUT PARAM ipBalanceDec   AS DEC   NO-UNDO.  /* ���⮪ �� ���� �
                                                       ����塞�� ����� */
   DEF INPUT PARAM iNumGrup       AS INT64   NO-UNDO.  /* ���浪��� �����
                                                        ��㯯� */

   DEF VAR vCompareLog AS LOG NO-UNDO. /* १���� �ࠢ����� ���஢ */

   DEF BUFFER Prev-trans  FOR tt-trans-sum.

   /* ���� ����� �� �室���� ���� */
   FIND FIRST tt-trans-sum WHERE tt-trans-sum.since    EQ ipDate
                             AND tt-trans-sum.contcode EQ ipContCode
      NO-ERROR.

   IF NOT AVAIL tt-trans-sum THEN
   DO:

      /* ᮧ���� ������ �� ���� */
      CREATE tt-trans-sum.
      ASSIGN
         tt-trans-sum.since           = ipDate
         tt-trans-sum.Summa[iNumGrup] = 0
         tt-trans-sum.contcode        = ipContCode
      .
   END.

   tt-trans-sum.Summa[iNumGrup]  = ipBalanceDec.

   RELEASE tt-trans-sum.

   /* ���� �।��饩 ����� */
   FIND LAST Prev-trans WHERE Prev-trans.since LT ipDate
                          AND Prev-trans.contcode EQ ipContCode
      NO-ERROR.

   /* �஢�ઠ �� �������⭮��� ����ᥩ(�।��饩 � ⥪�饩) */
   IF AVAIL Prev-trans THEN
   DO:

      /* ���� ����� �� ���� */
      FIND FIRST tt-trans-sum WHERE tt-trans-sum.since EQ ipDate
                                AND tt-trans-sum.contcode EQ ipContCode
         NO-ERROR.

      /* �� �஢�ન �᪫�砥� ���� �� ������ ����砥� ���⮪. */
      BUFFER-COMPARE
         Prev-trans EXCEPT since
         TO tt-trans-sum
         SAVE result IN vCompareLog
      NO-ERROR.

      /*�᫨ �訡�� ���, � 㤠�塞 ᮧ������ ������*/
      IF vCompareLog THEN
         DELETE tt-trans-sum.
   END.

   RETURN.

END PROCEDURE.

/* ��������/��������� ⠡���� LnShPrm */
PROCEDURE SetLnTax.

   DEF INPUT PARAM ipDate         AS DATE  NO-UNDO.  /* ��� ��������� */
   DEF INPUT PARAM ipRateDec      AS DEC   NO-UNDO.  /* ���祭��
                                                       ���� ����� �������*/
   DEF INPUT PARAM iNumGrup       AS INT64   NO-UNDO.  /* ���浪��� �����
                                                        ��㯯� */

   DEF VAR vCompareLog AS LOG NO-UNDO. /* ������� �ࠢ����� ���஢ */
   DEF VAR vNumGrup    AS INT64 NO-UNDO. /* ���稪 ����� ��㯯� */

   DEF BUFFER PrevLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm     FOR LnShPrm. /* �⮡� �� ���㡠��� ����� �
                                           ⠡��� ��᫥ ��楤���*/

   /* ���� ����� �� �室���� ���� */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   IF NOT AVAIL LnShPrm THEN
   DO:

      /* ������� ������ �� ���� */
      CREATE LnShPrm.

      LnShPrm.since = ipDate.
   END.

   ASSIGN
      LnShPrm.tax[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
      .

   RELEASE LnShPrm.

   /* ���४�஢�� ����ᥩ � i-�� ����. */
   FOR EACH LnShPrm WHERE
            LnShPrm.since GE ipDate:

      ASSIGN
         LnShPrm.tax[iNumGrup]     = ipRateDec        WHEN ipRateDec      NE ?
         .

      FIND LAST PrevLnShPrm WHERE
                PrevLnShPrm.since LT LnShPrm.since
      NO-ERROR.

      IF AVAIL PrevLnShPrm THEN
      DO:
         ASSIGN
            LnShPrm.IntSch   = PrevLnShPrm.IntSch   WHEN LnShPrm.IntSch   EQ ""
            LnShPrm.acct     = PrevLnShPrm.acct     WHEN LnShPrm.acct     EQ ""
            LnShPrm.currency = PrevLnShPrm.currency WHEN LnShPrm.currency EQ ""
            LnShPrm.SchRecid = PrevLnShPrm.SchRecid WHEN LnShPrm.SchRecid EQ ?
            LnShPrm.acct_rem = PrevLnShPrm.acct_rem WHEN LnShPrm.acct_rem EQ ?
            .
         DO vNumGrup = 1 TO EXTENT(LnShPrm.balance):
            ASSIGN
               LnShPrm.balance[vNumGrup]  = PrevLnShPrm.balance[vNumGrup]
                                           WHEN LnShPrm.balance[vNumGrup]  EQ ?
               LnShPrm.rate[vNumGrup]     = PrevLnShPrm.rate[vNumGrup]
                                           WHEN LnShPrm.rate[vNumGrup]     EQ ?
               LnShPrm.tax[vNumGrup]     = PrevLnShPrm.tax[vNumGrup]
                                           WHEN LnShPrm.tax[vNumGrup]     EQ ?
               LnShPrm.CommType[vNumGrup] = PrevLnShPrm.CommType[vNumGrup]
                                           WHEN LnShPrm.CommType[vNumGrup] EQ ?
               .
         END.
      END.
   END.

   /* ���� ����� �� ���� */
   FIND FIRST LnShPrm WHERE
              LnShPrm.since EQ ipDate
      NO-ERROR.

   /* ���� �।��饩 ����� */
   FIND LAST PrevLnShPrm WHERE
             PrevLnShPrm.since LT ipDate
      NO-ERROR.

   /* �஢�ઠ �� �������⭮��� ����ᥩ (�।��饩 � ⥪�饩) */
   IF AVAIL PrevLnShPrm THEN
   DO:

      /* �� �஢�ન �᪫�砥� ���� �� ������ ����砥� ���⮪. */
      BUFFER-COMPARE
         PrevLnShPrm EXCEPT since
         TO LnShPrm
         SAVE result IN vCompareLog
      NO-ERROR.

      /*�᫨ �訡�� ���, � 㤠�塞 ᮧ������ ������.*/
      IF vCompareLog THEN
         DELETE LnShPrm.
   END.

   RETURN.

END PROCEDURE.

/* ��ନ஢���� ���� ���᫥���. */
PROCEDURE GetCalcChar:

   DEF INPUT  PARAM iContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT  PARAM iContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT  PARAM iCodPar       AS INT64  NO-UNDO. /* ��� ��ࠬ��� */
   DEF OUTPUT PARAM oCurrCalcChar AS CHAR NO-UNDO. /* ���� ���᫥��� */

   oCurrCalcChar = IF iCodPar EQ 1 THEN
                       "0,7,13"
                   ELSE
                       STRING(most[iCodPar]).

END. /*PROCEDURE GetCalcChar*/


/* ����祭�� �������� ���⪠ �� ��ࠬ��ࠬ �������.
** ����� ��砩 - ����祭�� ���祭�� �� ������ ��ࠬ����. */
PROCEDURE GET_REM_BY_PRM_VTB.

   DEF INPUT PARAM ipContractChar AS CHAR NO-UNDO. /* �����祭�� ������� */
   DEF INPUT PARAM ipContCodeChar AS CHAR NO-UNDO. /* ����� ������� */
   DEF INPUT PARAM ipPrmChar      AS CHAR NO-UNDO. /* ��ࠬ���� ��� ����
                                                      ���⪠. */
   DEF INPUT PARAM ipBegDate      AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate      AS DATE NO-UNDO. /* "��" ����砭��
                                                      ���ࢠ�� */
   DEF INPUT PARAM ipCodPar       AS INT64  NO-UNDO. /* ��� ��ࠬ���*/
   DEF INPUT PARAM iCurrency      AS CHAR NO-UNDO. /* �����*/
   DEF INPUT PARAM iSince         AS DATE NO-UNDO. /* ��� ���ﭨ� �������*/
   DEF INPUT PARAM iRecalcLoan    AS LOG  NO-UNDO.
   DEF INPUT PARAM iRecalcPP      AS LOG  NO-UNDO.

   DEF VAR vChgDate     AS DATE NO-UNDO. /* ��� ����������� ��ଥ�� */
   DEF VAR vCurrPrmInt  AS INT64  NO-UNDO. /* i-� ��ࠬ��� */
   DEF VAR vTotalPrmDec AS DEC  NO-UNDO. /* �㬬� ��ଥ�஢ �� ���� */
   DEF VAR vPrmValDec   AS DEC  NO-UNDO. /* �㬬� i-��� ��ࠬ��� */
   DEF VAR vDbDec       AS DEC  NO-UNDO. /* �㬬� ����⮢�� ����⮢ */
   DEF VAR vCrDec       AS DEC  NO-UNDO. /* �㬬� �।�⮢�� ����⮢ */
   DEF VAR vPrmValDec10 AS DEC  NO-UNDO. /* �㬬� 10-��� ��ࠬ��� */
   DEF VAR vPrmValDec48 AS DEC  NO-UNDO. /* �㬬� 48-��� ��ࠬ��� */

   IF ipCodPar = 1 AND iRecalcPP THEN ipPrmChar = STRING(CodOstPar).
   /* ���������� � 㪠����� ���ࢠ� ��� */
   DO WHILE ipBegDate LE ipEndDate:

      vTotalPrmDec = 0.

      DO vCurrPrmInt = 1 TO NUM-ENTRIES (ipPrmChar):


         IF INT64 (ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar AND
            ipCodPar = 1                                    AND
            NOT iRecalcLoan                                 AND
            NOT iRecalcPP
         THEN
         DO:
            RUN RE_PARAM IN h_Loan
                         (INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                          ipBegDate - IF vIshOst THEN 0 ELSE 1,
                          ipContractChar,
                          ipContCodeChar,
                          OUTPUT vPrmValDec,
                          OUTPUT vDbDec,
                          OUTPUT vCrDec).

            RUN CORR_DOLG_SUMM (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                                INPUT-OUTPUT vPrmValDec ).
         END.
         ELSE IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = CodOstPar AND
                 ipCodPar = 1                           AND
                 iRecalcPP
         THEN
            RUN GetFilOstSumm(ipBegDate - IF vIshOst THEN 0 ELSE 1,
                              CodOstPar,
                       OUTPUT vPrmValDec).
         ELSE

            /* ����祭�� ���祭�� ��ࠬ��� */
            RUN STNDRT_PARAM IN h_Loan
                             (ipContractChar,
                              ipContCodeChar,
                              INT64 (ENTRY(vCurrPrmInt, ipPrmChar)),
                              ipBegDate - IF vIshOst THEN 0 ELSE 1,
                              OUTPUT vPrmValDec,
                              OUTPUT vDbDec,
                              OUTPUT vCrDec).

         /*�᫨ ���㫥��� ���祭�� ��ࠬ��� 10 ��� 48, � ��࠭塞 ���祭��*/
         IF ipCodPar = 1 THEN
         DO:
            IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 10 THEN
               vPrmValDec10 = vPrmValDec.
            ELSE
            IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 48 THEN
               vPrmValDec48 = vPrmValDec.

            vTotalPrmDec = vTotalPrmDec +
                             IF INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 10 OR
                                INT64(ENTRY(vCurrPrmInt, ipPrmChar)) = 48
                             THEN
                                0
                             ELSE
                                vPrmValDec.
         END.
         ELSE
            vTotalPrmDec = vTotalPrmDec + vPrmValDec.
      END.

      /* ���४�஢�� ���⪠ */
      RUN SetLnShPrm (ipBegDate, ?, ?, iCurrency, ?, vTotalPrmDec, ?, ?, ?,
                      (IF (vPrmValDec10 <> 0   OR
                           vPrmValDec48 <> 0 ) AND
                          ipCodPar = 1
                       THEN
                         YES
                       ELSE
                         ?),
                      1,
                      ?,
                      ?).

      /* ����祭�� ���� ��������� ��ࠬ��� */
      RUN GetChgDateParam (ipContractChar,
                           ipContCodeChar,
                           ipPrmChar,          /* ���᮪ ��������� ��ࠬ��஢ */
                           ipBegDate - IF vIshOst THEN 0 ELSE 1, /* �� ����� ���� ᬮ���� */
                           NOT iRecalcPP,
                           OUTPUT vChgDate).

      IF ipCodPar = 1               AND
         LOOKUP(STRING(CodOstPar),ipPrmChar) <> 0 AND
         NOT iRecalcLoan            AND
         NOT iRecalcPP
      THEN
         RUN CORR_NEXT_DATE (ipBegDate - IF vIshOst THEN 0 ELSE 1,
                INPUT-OUTPUT vChgDate).

      /* ���४�஢�� ���� ⥪�饣� ���ࢠ�� */
      ipBegDate = IF vChgDate GT ipEndDate
                  THEN ipEndDate + 1
                  ELSE vChgDate + IF vIshOst THEN 0 ELSE 1.
   END.

   RETURN.

END PROCEDURE.

/* ��ନ஢���� �����ᨨ �� ����� */
PROCEDURE GET_COMM_BY_REM_VTB.

   DEF INPUT PARAM ipCommChar AS CHAR NO-UNDO. /* ��� �����ᨨ */
   DEF INPUT PARAM ipKauChar  AS CHAR NO-UNDO. /* ��� ��� */
   DEF INPUT PARAM ipBegDate  AS DATE NO-UNDO. /* "�" ��砫� ���ࢠ�� */
   DEF INPUT PARAM ipEndDate  AS DATE NO-UNDO. /* "��" ����砭�� ���ࢠ�� */
   DEF INPUT PARAM ipCodPar   AS INT64  NO-UNDO. /* ��� ��ࠬ���*/

   DEF VAR vAcctRecid    AS RECID NO-UNDO. /* �����䨪��� ��� */
   DEF VAR vRateDec      AS DEC   NO-UNDO. /* ����饥 ���祭�� %%
                                              �⠢��/��� */
   DEF VAR vCommTypeChar AS CHAR  NO-UNDO. /* �⠢��/��� */
   DEF VAR  BegDate1     AS DATE  NO-UNDO.
   DEF VAR  BegDate2     AS DATE  NO-UNDO.
   DEF VAR vCommCur      AS CHAR  NO-UNDO.

   DEF VAR vLog  AS LOG  NO-UNDO.
   DEF VAR vSumm AS DEC  NO-UNDO.
   DEF VAR vDate AS DATE NO-UNDO.

   DEF BUFFER bLnShPrm FOR LnShPrm.
   DEF BUFFER LnShPrm  FOR LnShPrm.

   /* ��ନ஢���� �����ᨨ � ���ࢠ�� ��� */
   DO WHILE ipBegDate LE ipEndDate:

      /* ���� ��ࠬ��஢ �� ���� */
      FIND LAST LnShPrm WHERE
                LnShPrm.since LE ipBegDate
         NO-ERROR.

      /* �᫨ ��ࠬ���� ��।�����,
      ** � ����⪠ ���᪠ ���祭�� �����ᨨ/��� */
      IF AVAIL LnShPrm  THEN
      DO:

         ASSIGN
             vAcctRecid = ?
             vCommCur   = LnShPrm.Currency
             .
             /* ����祭�� �����ᨨ/��� �� ���� */
             /*
             vRateDec = GET_COMM (
                   ipCommChar,         /* ��� �����ᨨ */
                   vAcctRecid,         /* �����䨪��� ��� */
                   vCommCur,           /* ��� �ਢ������� ������ */
                   ipKauChar,          /* ��� ��� ("" - �� 㬮�砭��) */
                   LnShPrm.balance[1], /* ��� ���⮪ (0 - ��㬮�砭��) */
                   0,                  /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                   ipBegDate).*/
             /*vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   ipCommChar,
                   ipBegDate).*/
         /*��� lnremva1.p
         �᫨ �᭮��� ��業�� � ���� �� ����襭��� �⠢��,
         � ��।��塞 ��,  �᫨ �� ���, � ࠡ�⠥� �� ��ன */
         IF ipCodPar         = 1    AND
            LnShPrm.OtherCom = YES THEN
         DO:
            /*vRateDec   = GET_COMM(
                "%�����",           /* ��� �����ᨨ */
                vAcctRecid,         /* �����䨪��� ��� */
                vCommCur,           /* ��� �ਢ������� ������ */
                ipKauChar,          /* ��� ��� ("" - �� 㬮�砭��) */
                LnShPrm.balance[1], /* ��������� ���⮪ (0 - ��㬮�砭��) */
                0,                  /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                ipBegDate).*/
            vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   "%�����",
                   ipBegDate).

            IF vRateDec = ?  THEN
               vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   ipCommChar,
                   ipBegDate).
               /*
               vRateDec = GET_COMM(
                   ipCommChar,         /* ��� �����ᨨ */
                   vAcctRecid,         /* �����䨪��� ��� */
                   vCommCur,           /* ��� �ਢ������� ������ */
                   ipKauChar,          /* ��� ��� ("" - �� 㬮�砭��) */
                   LnShPrm.balance[1], /* �����  ���⮪ (0 - ��㬮�砭��) */
                   0,                  /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                   ipBegDate).*/

         END.
         ELSE
           vRateDec = GET_COMM_LOAN
                  (ENTRY(1,ipKauChar),
                   ENTRY(2,ipKauChar),
                   ipCommChar,
                   ipBegDate).
         /* ����祭�� ⨯�: "%,=,����" */
         vCommTypeChar = IF CAN-DO("*" + {&GCodePeny} + "*", ipCommChar)
                         THEN {&GCodePeny}
                         ELSE IF GET_COMM_TYPE (
                                  ipCommChar,         /* ��� �����ᨨ */
                                  vAcctRecid,         /* �����䨪��� ��� */
                                  vCommCur,           /* ��� ������ */
                                  ipKauChar,          /* ��� ���  */
                                  LnShPrm.balance[1], /* ��������� ���⮪ */
                                  0,                  /* ��ਮ�/�ப */
                                  ipBegDate)
                              THEN "="
                              ELSE "%".
          /* ��筨�, �� � ������ �� ���� 䨪�஢���� */
          IF vCommTypeChar = {&GCodePeny} THEN DO:
            IF GET_COMM_TYPE (
                  ipCommChar,         /* ��� �����ᨨ */
                  vAcctRecid,         /* �����䨪��� ��� */
                  vCommCur,           /* ��� ������ */
                  ipKauChar,          /* ��� ���  */
                  LnShPrm.balance[1], /* ��� ���⮪ */
                  0,                  /* ��ਮ�/�ப */
                  ipBegDate)
            THEN
              vCommTypeChar = {&GCodePenyFix}.
          END.


         ASSIGN
            vDate = LnShPrm.Since
            vLog  = LnShPrm.OtherCom
            vSumm = LnShprm.Balance[1]
            .

         /* ���४�஢��/ᮧ����� %% �⠢�� */
         RUN SetLnShPrm (ipBegDate, ?, ?, ?, ?, ?, ?,
                         vRateDec,
                         vCommTypeChar,
                         LnShPrm.OtherCom,
                         1,
                         ?,
                         ?).

         /* ������ ���� ��������� ������ / ������ */
         IF ipCodPar = 1 THEN
         DO:
            FIND FIRST bLnShPrm WHERE
                       bLnShPrm.since GT ipBegDate
            NO-ERROR.

            ASSIGN
               BegDate1 = GET_NEXT_COMM_DATE_LOAN (
                        ENTRY(1,ipKauChar),
                        ENTRY(2,ipKauChar),
                        "%�।",
                        ipBegDate,
                        ipEndDate)
                      /*GET_NEXT_COMM_DATE (
                          "%�।",            /* ��� �����ᨨ */
                          vAcctRecid,         /* �����䨪��� ��� */
                          vCommCur,           /* ��� �ਢ������� ������ */
                          ipKauChar,          /* ��� ��� ("" - �� 㬮�砭��) */
                          vSumm,              /* ��� ���⮪ */
                          0,                  /* ��ਮ�/�ப */
                          ipBegDate,
                          ipEndDate)*/

               BegDate2 = GET_NEXT_COMM_DATE_LOAN (
                        ENTRY(1,ipKauChar),
                        ENTRY(2,ipKauChar),
                        "%�����",
                        ipBegDate,
                        ipEndDate)
               .
                          /*GET_NEXT_COMM_DATE (
                          "%�����",           /* ��� �����ᨨ */
                          vAcctRecid,         /* �����䨪��� ��� */
                          vCommCur,           /* ��� �ਢ������� ������ */
                          ipKauChar,          /* ��� ��� ("" - �� 㬮�砭��) */
                          vSumm,              /* ��� ���⮪ */
                          0,                  /* ��ਮ�/�ப */
                          ipBegDate,
                          ipEndDate)
               .          */

            IF BegDate1 <> ? AND
               BegDate2 <> ? THEN
               ipBegDate = MIN(BegDate1,BegDate2).
            ELSE
            IF BegDate1 <> ? THEN
               ipBegDate = BegDate1.
            ELSE
            IF BegDate2 <> ? THEN
               ipBegDate = BegDate2.
            ELSE
              ipBegDate = ipEndDate + 1.

            IF ipBegDate < ipEndDate + 1 THEN DO:
               RUN SetLnShPrm (ipBegDate, ?, ?, ?, ?, ?, ?, ?, ?,vLog,1,?,?).
             END.

            IF AVAIL bLnShPrm             AND
               bLnShPrm.since < ipBegDate THEN
               ipBegDate = bLnShPrm.since.
         END.
         ELSE
           ipBegDate = GET_NEXT_COMM_DATE_LOAN (
                        ENTRY(1,ipKauChar),
                        ENTRY(2,ipKauChar),
                        ipCommChar,
                        ipBegDate,
                        ipEndDate).

           /*GET_NEXT_COMM_DATE (
                        ipCommChar,         /* ��� �����ᨨ */
                        vAcctRecid,         /* �����䨪��� ��� */
                        vCommCur,           /* ��� �ਢ������� ������ */
                        ipKauChar,          /* ��� ��� ("" - �� 㬮�砭��) */
                        vSumm,              /* ��� ���⮪ (0 - ��㬮�砭��) */
                        0,                  /* ��ਮ�/�ப (0 - ��㬮�砭��) */
                        ipBegDate,
                        ipEndDate).*/

         IF ipCodPar <> 1 THEN
            /* ���४�஢�� ���� ��砫� ��ਮ�� */
            ipBegDate = IF ipBegDate EQ ?
                        THEN ipEndDate + 1
                        ELSE ipBegDate.

      END.
      /* �᫨ ��ࠬ��஢ ��� ���᪠ �����ᨩ �� �������,
      ** � �饬 �� ���।� */
      ELSE
      DO:

         FIND FIRST LnShPrm WHERE
                    LnShPrm.since GT ipBegDate
            NO-ERROR.

         /* � �᫨ �� ���,
         ** � � �� ���� �᪠�� �������/��� */
         ipBegDate = IF AVAIL LnShPrm
                     THEN LnShPrm.since
                     ELSE ipEndDate + 1.
      END.
   END.

   RETURN.

END PROCEDURE.

/* ����祭�� ���� ��������� ��ࠬ��� */
PROCEDURE GetChgDateParam:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* ������� */
   DEF INPUT  PARAM iPrmChar  AS CHAR NO-UNDO. /* ���᮪ ��ࠬ��஢*/
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* �� ����� ���� ᬮ���� */
   DEF INPUT  PARAM iLog      AS LOG  NO-UNDO. /* */
   DEF OUTPUT PARAM oChgDate  AS DATE NO-UNDO. /* �������� ��ࢠ� ��� �������� */

   DEF VAR vDate    AS DATE NO-UNDO.
   DEF VAR vChgDate AS DATE NO-UNDO.

   DEF BUFFER xloan-int FOR loan-int.
   DEF BUFFER xl        FOR loan-int.

   RUN GetChgDate IN h_Loan
                  (iContract,
                   iContCode,
                   iPrmChar,        /* ���᮪ ��������� ��ࠬ��஢ */
                   iBegDate,        /* �� ����� ���� ᬮ���� */
                   OUTPUT vChgDate).

   IF LOOKUP(STRING(CodOstPar),iPrmChar) > 0 AND iLog THEN
   DO:
      FIND FIRST xloan-int WHERE
                 xloan-int.contract  = iContract
             AND xloan-int.cont-code = iContCode
             AND xloan-int.id-d      = 1
             AND xloan-int.id-k      = 2
             AND xloan-int.mdate     > iBegDate
         NO-LOCK NO-ERROR.

      RELEASE xl .

      IF AVAIL xloan-int THEN
         FIND FIRST xl WHERE
                    xl.contract  = iContract
                AND xl.cont-code = iContCode
                AND xl.id-d      = 2
                AND xl.mdate     = xloan-int.mdate
                AND xl.avt
            NO-LOCK NO-ERROR.
      IF     AVAIL xloan-int AND
         NOT AVAIL xl        THEN
         vChgDate = IF vChgDate <> ? THEN
                          MIN(xloan-int.mdate,vChgDate)
                       ELSE
                          xloan-int.mdate.
   END.
   IF LOOKUP(STRING(CodOstPar),iPrmChar) > 0 AND NOT iLog THEN
   DO:
      RUN GetFilOstDate(iBegDate,CodOstPar,OUTPUT vDate).

      vChgDate = IF vDate     <> ? AND
                    vChgDate  <> ?
                 THEN
                    MIN(vDate,vChgDate)
                 ELSE
                 IF vDate  <> ?
                 THEN
                    vDate
                 ELSE
                    vChgDate
                 .

   END.
   oChgDate = vChgDate.

END PROCEDURE.

/* ����祭�� ���� ��᫥����� ��������� ��ࠬ��� */
PROCEDURE GetLastDateParam:

   DEF INPUT  PARAM iContract AS CHAR NO-UNDO. /* �����䨪��� */
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO. /* ������� */
   DEF INPUT  PARAM iPrmChar  AS CHAR NO-UNDO. /* ���᮪ ��ࠬ��஢*/
   DEF INPUT  PARAM iBegDate  AS DATE NO-UNDO. /* �� ����� ���� ᬮ���� */
   DEF OUTPUT PARAM oChgDate  AS DATE NO-UNDO. /* ��᫥���� ��� �������� */

   DEF VAR vI       AS INT64 NO-UNDO.
   DEF VAR vParCode AS INT64 NO-UNDO.
   DEF VAR vDate    AS DATE  NO-UNDO.

   DEF BUFFER xloan-int FOR loan-int.

/*   vDate = iBegDate. */
   /* ���� ��᫥���� ����⮢�� ����樨 �� �������� ������ �� ��ࠬ��஢ */
   DO vI = 1 TO NUM-ENTRIES(iPrmChar):
      ASSIGN
         vParCode = INT64(ENTRY(vI, iPrmChar)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.
   
      FIND LAST xloan-int WHERE xloan-int.contract  EQ iContract 
                            AND xloan-int.cont-code EQ iContCode
                            AND xloan-int.id-d      EQ vParCode
                            AND xloan-int.mdate     GT iBegDate
           NO-LOCK NO-ERROR.
      IF AVAIL xloan-int THEN
         ASSIGN
            vDate    = xloan-int.mdate
            iBegDate = xloan-int.mdate
         .
      FIND LAST xloan-int WHERE xloan-int.contract  EQ iContract
                            AND xloan-int.cont-code EQ iContCode
                            AND xloan-int.id-k      EQ vParCode
                            AND xloan-int.mdate     GT iBegDate
           NO-LOCK NO-ERROR.
      IF AVAIL xloan-int THEN
         ASSIGN
            vDate    = xloan-int.mdate
            iBegDate = xloan-int.mdate
         .
      IF CodOstPar EQ vParCode THEN
      DO:
         FIND LAST xloan-int WHERE xloan-int.contract  EQ iContract
                               AND xloan-int.cont-code EQ iContCode
                               AND xloan-int.id-d      EQ 1
                               AND xloan-int.id-k      EQ 2
                               AND xloan-int.mdate     GT iBegDate
              NO-LOCK NO-ERROR.
         IF AVAIL xloan-int THEN
            vDate = xloan-int.mdate.
      END.
   END.

   oChgDate = vDate.

END PROCEDURE.

PROCEDURE GET_DOLG_CORR:

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iSince    AS DATE NO-UNDO.
   DEF INPUT PARAM iEndDate  AS DATE NO-UNDO.
   DEF INPUT PARAM iPlan     AS LOG  NO-UNDO.

   DEF VAR vRem       AS DEC  NO-UNDO.
   DEF VAR vCorr      AS DEC  NO-UNDO.
   DEF VAR vParam     AS DEC  NO-UNDO.
   DEF VAR vDb        AS DEC  NO-UNDO.
   DEF VAR vCr        AS DEC  NO-UNDO.
   DEF VAR vTotalCorr AS DEC  NO-UNDO.
   DEF VAR vLastCorr  AS DEC  NO-UNDO.

   DEF VAR  procn    AS CHAR NO-UNDO. /*��楤�� ���� ��ࠬ���*/
   DEF VAR s_params  AS CHAR  INIT "0,13,33,34" NO-UNDO. /* ���� ��ࠬ��஢,
                                                        ��ࠡ��뢠��� ᯥ�. �㭪�ﬨ */


   DEF BUFFER term-obl  FOR term-obl.
   DEF BUFFER bterm-obl FOR term-obl.
   
   {empty ttCorr}

   FOR EACH term-obl WHERE
            term-obl.contract   = iContract
        AND term-obl.cont-code  = iContCode
        AND term-obl.idnt       = 3
        AND term-obl.end-date  >= iSince
        AND term-obl.end-date  <= iEndDate
   NO-LOCK:
   
   IF CodOstPar = 0  THEN
      RUN PARAM_0_NEW
         (iContract,       /* ��� ������� */
         iContCode,        /* ����� ������� */
         CodOstPar,        /* ��� ��ࠬ��� */
         term-obl.end-date,/* ��� ������ ������� */
         OUTPUT vParam).   /* ���祭�� ��ࠬ��� �� ���� ������ ������� */
             
   ELSE
   DO:
      procn =   IF LOOKUP (STRING(CodOstPar), s_params) NE 0 THEN 
                 ("param_" + ENTRY(LOOKUP (STRING(CodOstPar), s_params), s_params))
                ELSE 
                 "stndrt_param".

      RUN VALUE (procn)
         (iContract,         /* ��� ������� */
          iContCode,         /* ����� ������� */
          CodOstPar,         /* ��� ��ࠬ��� */
          term-obl.end-date, /* ��� ������ ������� */
          OUTPUT vParam,     /* ���祭�� ��ࠬ��� �� ���� ������ ������� */
          OUTPUT vDb,        /* ����⮢� ����� �� ������ */
          OUTPUT vCr).       /* �।�⮢� ����� �� ������ */
   END.
  
   vRem = vParam - vLastCorr.

   FIND LAST bterm-obl WHERE
          bterm-obl.contract  EQ icontract 
      AND bterm-obl.cont-code EQ iContCode 
      AND bterm-obl.idnt      EQ 2 
      AND bterm-obl.end-date  LE term-obl.end-date
   NO-LOCK NO-ERROR.
    
   IF AVAIL bterm-obl THEN
    vCorr = vCorr + IF vRem - bterm-obl.amt-rub > 0
                         THEN vRem - bterm-obl.amt-rub
                         ELSE 0.

   CREATE ttCorr.
   ASSIGN
         ttCorr.end-date = term-obl.end-date
         ttCorr.Summa    = vParam - vCorr
         ttCorr.Corr     = vCorr
         vLastCorr       = vCorr
   .
   RELEASE ttCorr.
   END.

END PROCEDURE.

PROCEDURE CORR_DOLG_SUMM:

   DEF INPUT        PARAM iDate  AS DATE NO-UNDO.
   DEF INPUT-OUTPUT PARAM pSumma AS DEC  NO-UNDO.

   FIND LAST ttCorr WHERE ttCorr.end-date <= iDate NO-LOCK NO-ERROR.

   pSumma = IF AVAIL ttCorr
            THEN pSumma - ttCorr.Corr
            ELSE pSumma.
   RETURN.

END PROCEDURE.

PROCEDURE CORR_NEXT_DATE:

   DEF INPUT        PARAM iBegDate AS DATE NO-UNDO.
   DEF INPUT-OUTPUT PARAM pChgDate AS DATE NO-UNDO.

   FIND FIRST ttCorr WHERE ttCorr.end-date > iBegDate NO-LOCK NO-ERROR.

   IF pChgDate = ? AND AVAIL ttCorr THEN
      pChgDate = ttCorr.end-date.

   pChgDate = IF AVAIL ttCorr AND ttCorr.end-date < pChgDate
              THEN ttCorr.end-date
              ELSE pChgDate.
   RETURN.

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='23/12/2015 13:20:58.768+04:00' */
/* $LINTUSER='guiv' */
/* $LINTMODE='1' */
/* $LINTFILE='ln-nach.i' */
/*prosign4Jx8ab2MPTsxFPJkKcs+oQ*/