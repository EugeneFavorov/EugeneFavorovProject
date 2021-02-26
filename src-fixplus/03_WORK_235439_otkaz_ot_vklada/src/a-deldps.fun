/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: a-deldps.fun
      Comment: TT:0235439 ������. �⪠� ������ �� ��ଫ���� ������
   Parameters:
         Uses:
      Used by: a-deldps.p
      Created: 12/02/2015 14:33 KMBIS ������. �⪠� ������ �� ��ଫ���� ������
                                      �ᯮ����⥫�� �㭪樨
     Modified: 
*/
/*================================================================================================*/
/*=== ���� �易����� ������ �� =================================================================*/
FUNCTION FndDV RETURNS CHAR (INPUT iContr AS CHAR,
                             INPUT iCode  AS CHAR):

DEF VAR vRes  AS  CHAR  NO-UNDO.

DEF BUFFER bLoanAcct FOR loan-acct.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER bLAcct    FOR loan-acct.
DEF BUFFER bAcctDel  FOR acct.

   vRes = "".
   FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ iContr
                        AND bLoanAcct.cont-code EQ iCode 
                      NO-LOCK,
      FIRST bAcct WHERE bAcct.acct      EQ bLoanAcct.acct
                    AND bAcct.currency  EQ bLoanAcct.currency
                  NO-LOCK
                  BREAK BY bAcct.acct.

      IF LAST-OF(bAcct.acct) THEN
      DO:
         FOR EACH bLAcct WHERE bLAcct.acct      EQ bLoanAcct.acct    
                            AND bLAcct.currency  EQ bLoanAcct.currency
                            AND bLAcct.contract  EQ bLoanAcct.contract
                            AND bLAcct.cont-code NE bLoanAcct.cont-code
                            AND bLAcct.acct-type EQ "loan-dps-p"
                          NO-LOCK:

            /*���� ������ ⠪, � � �� �ᯨᠭ� ��� �᪠�� ��� � ������஬ �� */
            {additem.i vRes bLAcct.cont-code}

         END. /* FOR FIRST bLAcct WHERE bLAcct.acct      EQ bLoanAcct.acct     */
      END. /* IF LAST-OF(bAcct.acct) THEN */
   END. /* FOR EACH bLoanAcct WHERE bLoanAcct.contract  EQ bDpsLoan.contract */

   RETURN vRes.

END FUNCTION. /* FndDV */

/*================================================================================================*/

/*================================================================================================*/
/*=== ���� �� ���㫨஢����� ���㬥�⮢ =========================================================*/
FUNCTION FndOp RETURNS LOG (INPUT iContr AS CHAR,
                            INPUT iCode  AS CHAR):

DEF VAR vFind  AS  LOG  NO-UNDO INIT NO.

DEF BUFFER bLoan      FOR loan.
DEF BUFFER bKauEntry  FOR kau-entry.
DEF BUFFER bLoanAcct  FOR loan-acct.
DEF BUFFER bLAcct     FOR loan-acct.
DEF BUFFER bAcct      FOR acct.
DEF BUFFER bOp        FOR op.
DEF BUFFER bOpEntry   FOR op-entry.

   /* �饬 �� ���, �᫨ ���� � �� ࠧ��ࠥ��� �����  */
   FOR FIRST bKauEntry WHERE bKauEntry.kau BEGINS SUBST("&1,&2,", 
                                                        iContr,
                                                        iCode)
                       NO-LOCK:
      /* �� ������ ������� �㡠����⨪� */
      vFind = YES.
   END.

   IF vFind EQ NO THEN
      FOR FIRST bLoan WHERE bLoan.contract  EQ iContr
                        AND bLoan.cont-code EQ iCode 
                      NO-LOCK,
        EACH bLoanAcct WHERE bLoanAcct.contract  EQ bLoan.contract 
                         AND bLoanAcct.cont-code EQ bLoan.cont-code
                       NO-LOCK,
         FIRST bAcct WHERE bAcct.acct      EQ bLoanAcct.acct
                       AND bAcct.currency  EQ bLoanAcct.currency
                       AND bAcct.cust-cat  EQ bLoan.cust-cat
                       AND bAcct.cust-id   EQ bLoan.cust-id
                     NO-LOCK
                     BREAK BY bAcct.acct:
     
         IF FIRST-OF(bAcct.acct) THEN
         DO:
            FOR EACH bOpEntry WHERE bOpEntry.acct-db EQ bAcct.acct
                                 OR bOpEntry.acct-cr EQ bAcct.acct
                              NO-LOCK,
               FIRST bOp WHERE bOp.op      EQ bOpEntry.op
                           AND bOp.op-date GE bLoan.open-date
                           AND bOp.op-date NE ?
                         NO-LOCK:
               /* ��諨 �� ���㫨஢���� ���㬥�� */
           
               /* �饬 �ਢ離� ��� � ��㣮�� �������� */
               FIND FIRST bLAcct WHERE bLAcct.acct      EQ bLoanAcct.acct 
                                   AND bLAcct.currency  EQ bLoanAcct.currency
                                   AND bLAcct.cont-code NE bLoan.cont-code
                                 NO-LOCK NO-ERROR.

               IF NOT AVAIL(bLAcct) THEN
               DO:
                  /* �� ���� ���㬥��, ��� �� �ਢ易� ⮫쪮 � ��襬� �������� */
                  vFind = YES.
               END. /* IF NOT AVAIL(bLAcct) THEN */

            END. /* FOR FIRST op-entry WHERE op-entry.acct-db EQ acct.acct */
         END. /* IF FIRST-OF(bAcct.acct) THEN */
      END. /* FOR EACH loan-acct WHERE loan-acct.contract  EQ loan.contract */

   RETURN vFind.

END FUNCTION. /* FndOp */

/*================================================================================================*/
