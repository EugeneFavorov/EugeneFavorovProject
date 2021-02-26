/*
     ������᪠� ��⥣�஢����� ��⥬� �������
      Filename: peracct.p
      Comment: ���� "����᫥��� � ��� ��᫥ ������� ������"
      Parameters:
      Uses:
      Used by:
      Created: pda
      ����஥�� ��ࠬ����:
      �����䨪����:
*/

{globals.i}
{intrface.get xclass}
{sh-defs.i}

DEF VAR fname  AS CHAR NO-UNDO.
DEF NEW SHARED STREAM vvs.

DEF VAR strcldate AS CHAR NO-UNDO.
DEF VAR strdate   AS CHAR NO-UNDO.
DEF VAR balance   AS DECIMAL INIT 0.00 NO-UNDO.      /*���⮪ �� ��� ��᫥ ���᫥���*/
DEF VAR vAcctFrom AS CHARACTER NO-UNDO.    /*��� � ���ண� �����塞*/
DEF VAR vAcctTo   AS CHARACTER NO-UNDO.    /*��� �� ����� �����塞*/
DEF VAR vDatePer  AS DATE NO-UNDO.         /*��� ����᫥���*/
 
DEF TEMP-TABLE tt-loan NO-UNDO 
    FIELD date-per   AS   DATE             /*��� ����᫥���*/
    FIELD cont-code  LIKE loan.cont-code   /*����� �������*/
    FIELD cont-type  LIKE loan.cont-type   /*⨯ ������*/
    FIELD close-date AS   CHARACTER        /*��� ������� ��� � ���ண� �����塞*/
    FIELD cust-id    LIKE loan.cust-id     /*��� ������*/
    FIELD acct-from  AS   CHARACTER        /*��� � ���ண� �����塞*/
    FIELD acct-to    AS   CHARACTER        /*��� �� ����� �����塞*/
    FIELD acct-bal   AS   DECIMAL          /*���⮪ �� ��� � ���ண� �����塞*/
    FIELD amt-per    LIKE op-entry.amt-rub /*�㬬� ����᫥���*/
    .

{getdates.i}

IF beg-date EQ end-date THEN
strdate = "���� �� " + STRING(beg-date,"99.99.9999").
  ELSE 
  strdate = "���� �� ��ਮ� � " + STRING(beg-date,"99.99.9999") + " �� " + STRING(end-date,"99.99.9999").
FOR EACH loan 
   WHERE loan.contract EQ 'dps'
     AND loan.filial-id EQ shFilial
NO-LOCK,
  FIRST signs
  WHERE signs.file-name  EQ 'loan' 
    AND signs.surrogate  EQ STRING(loan.contract + ',' + loan.cont-code)
    AND signs.code       EQ 'date_trans'
    AND signs.date-value GE beg-date 
    AND signs.date-value LE end-date
  NO-LOCK:
    FIND LAST loan-acct OF loan 
        WHERE loan-acct.acct-type EQ 'loan-dps-t' 
           OR loan-acct.acct-type EQ 'loan-dps-p'
    NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN
      DO:
        RUN acct-pos IN h_base(loan-acct.acct,loan-acct.currency,TODAY,TODAY, ?).
          IF LENGTH(loan-acct.currency) > 0 THEN /*�஢�ઠ �� �ਧ��� �����*/
          balance = ABS(sh-in-val).
            ELSE balance = ABS(sh-bal).
      END.    
      vAcctFrom = loan-acct.acct.
      vAcctTo   = GetXAttrValueEx("loan",loan.contract + ',' + loan.cont-code,"acct_trans","").
      vDatePer  = DATE(GetXAttrValueEx("loan",loan.contract + ',' + loan.cont-code,"date_trans","")).
        FIND FIRST op-entry
             WHERE op-entry.value-date   EQ vDatePer
               AND op-entry.acct-db   EQ loan-acct.acct
               AND op-entry.acct-cr   EQ vAcctTo
               AND op-entry.op-status BEGINS "�"
        NO-LOCK NO-ERROR.
          DO:
            IF loan.close-date <> ? 
            THEN strcldate = STRING(loan.close-date,"99.99.9999"). 
              ELSE strcldate = ''.
          CREATE tt-loan.
          ASSIGN
            tt-loan.date-per   = vDatePer
            tt-loan.cont-code  = loan.cont-code
            tt-loan.cont-type  = loan.cont-type
            tt-loan.acct-from  = vAcctFrom
            tt-loan.acct-to    = vAcctTo
            tt-loan.amt-per    = IF AVAIL op-entry THEN op-entry.amt-rub ELSE 0
            tt-loan.acct-bal   = balance
            tt-loan.close-date = strcldate
            tt-loan.cust-id    = loan.cust-id
            .
          END.
END. /*FOR EACH loan*/    

{peracctxls.i}
/*
{setdest.i &file-name = "111.log"}
FOR EACH tt-loan NO-LOCK:
  PUT UNFORMATTED
    tt-loan.date-per   "; "
    tt-loan.acct-from  "; "
    tt-loan.acct-to    "; "
    tt-loan.amt-per    "; "
    tt-loan.acct-bal   "; "
    tt-loan.close-date "; "
    tt-loan.cust-id    "; "
  SKIP.   
END.   
/*
PUT UNFORMATTED ETIME " msec" SKIP.
*/
{preview.i &file-name = "111.log"}
*/