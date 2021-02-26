{intrface.get crd}   /* ������⥪� �����㬥�⮢ ࠡ��� � ����⥪�� */
DEF VAR check-contract AS CHAR INIT "���,�����"  NO-UNDO.

{additem.i check-contract mAcctContCrd2}
{additem.i check-contract mODAcctContr}

/*-----------------------------------------------------------------------------------------------*/
/* ��।���� ����� ���⪠ �� �����祭��                                                        */
/*-----------------------------------------------------------------------------------------------*/
FUNCTION GetLimitContract RETURN DECIMAL (BUFFER b-acct FOR acct,
                                          in-op-date  AS DATE,
                                          in-contract AS CHAR):
   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER loan-acct FOR loan-acct.
   DEFINE BUFFER term-obl  FOR term-obl.

   RELEASE loan-acct.
   RELEASE loan.
   RELEASE term-obl.

   FOR EACH loan-acct WHERE loan-acct.contract  EQ in-contract
                        AND loan-acct.acct-type EQ in-contract
                        AND loan-acct.acct      EQ b-acct.acct
                        AND loan-acct.currency  EQ b-acct.currency NO-LOCK,
      FIRST loan OF loan-acct WHERE
             (   loan.close-date EQ ?
              OR loan.close-date GT in-op-date)
         AND (   loan.end-date   EQ ?
              OR loan.end-date   GE in-op-date) NO-LOCK:
   
      FIND LAST term-obl WHERE term-obl.contract  EQ loan-acct.contract
                           AND term-obl.cont-code EQ loan-acct.cont-code
                           AND term-obl.end-date  LE in-op-date
                           AND term-obl.idnt      EQ 2 NO-LOCK NO-ERROR.
      IF AVAIL term-obl THEN
         RETURN term-obl.amt-rub.
      ELSE RETURN 0.0.

   END.
   RETURN ?.

END FUNCTION.
/*-----------------------------------------------------------------------------------------------*/
/* ��।���� ����� ���⪠ �� ���� ᮣ��᭮ ��������                                           */
/*-----------------------------------------------------------------------------------------------*/
FUNCTION GetLimitPosition RETURNS decimal (BUFFER b-acct     FOR acct,
                                           INPUT  in-op-date AS  DATE):

   DEFINE VAR lim-pos AS DECIMAL INIT 0.00 NO-UNDO.
   DEFINE VAR vItem   AS INT64           NO-UNDO.

   IF b-acct.acct-cat EQ "b" THEN
   DO:
      DO vItem = 1 TO NUM-ENTRIES(check-contract):
         lim-pos = GetLimitContract(BUFFER b-acct, in-op-date, ENTRY(vItem,check-contract)).
         IF lim-pos NE ? THEN LEAVE.
      END.
      IF lim-pos EQ ? THEN lim-pos = 0.0.
   END.
   ELSE lim-pos = 0.

   RETURN lim-pos.

END FUNCTION.

/*-----------------------------------------------------------------------------------------------*/
/* ��।���� ���ᯮ�짮����� ����� �������                                                  */
/*-----------------------------------------------------------------------------------------------*/
FUNCTION GetOverLimit RETURNS decimal (BUFFER b-acct     FOR acct,
                                       INPUT  in-op-date AS  DATE):

   DEFINE VAR lim-pos AS DECIMAL INIT 0.00 NO-UNDO.

   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER loan-acct FOR loan-acct.
   DEFINE BUFFER bloan-acct FOR loan-acct.

   IF b-acct.acct-cat EQ "b" THEN
   DO:

      FOR EACH loan-acct WHERE 
               loan-acct.contract  EQ "�।��"
           AND loan-acct.acct-type EQ "�।����"
           AND loan-acct.acct      EQ b-acct.acct
           AND loan-acct.currency  EQ b-acct.currency 
               NO-LOCK,
         FIRST loan OF loan-acct WHERE
              (loan.close-date EQ ?
            OR loan.close-date GT in-op-date)
          AND (loan.end-date   EQ ?
            OR loan.end-date   GE in-op-date) 
           AND loan.class-code EQ "l_agr_with_per" 
               NO-LOCK,
         FIRST bloan-acct OF loan WHERE
               bloan-acct.acct-type EQ "�।�"
               NO-LOCK:

         RUN acct-pos IN h_base (bloan-acct.acct,
                                 bloan-acct.currency,
                                 in-op-date,
                                 in-op-date,
                                 CHR(251)).
         lim-pos = lim-pos + (IF bloan-acct.currency EQ "" THEN sh-bal ELSE sh-val) * (-1).
      END.
   END.
   ELSE lim-pos = 0.

   RETURN lim-pos.

END FUNCTION.
