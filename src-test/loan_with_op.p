{globals.i}
{tmprecid.def}

DEFINE NEW SHARED STREAM vvs.
DEFINE VARIABLE fname AS CHARACTER  INIT "./loan_with_op.csv"  NO-UNDO.
DEFINE VARIABLE delim AS CHARACTER INIT ";" FORMAT "x(1)" NO-UNDO.
DEFINE VARIABLE eol AS CHARACTER FORMAT "x(2)" NO-UNDO.
eol = CHR(13) + CHR(10).


FIND FIRST tmprecid
NO-ERROR.
IF NOT AVAIL tmprecid
THEN RETURN.

{getdates.i}

DEFINE VARIABLE sName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE sContCode AS CHARACTER NO-UNDO.

OUTPUT STREAM vvs TO VALUE (fname)
   UNBUFFERED  CONVERT  TARGET "1251" SOURCE "IBM866".
   PUT STREAM vvs UNFORMATTED beg-date delim end-date eol.
   PUT STREAM vvs UNFORMATTED "Номер договора" delim "Имя клиента" delim "Операция" delim "Дата" delim "Сумма" delim "Счет дебет" delim "Счет кредит" eol.
FOR EACH tmprecid,
   FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK,
   EACH loan-int OF loan WHERE 
                               loan-int.mdate GE beg-date 
                           AND loan-int.mdate LE end-date NO-LOCK,
   FIRST chowhe WHERE chowhe.id-d = loan-int.id-d AND chowhe.id-k = loan-int.id-k NO-LOCK,
   EACH op-entry OF loan-int NO-LOCK BREAK BY loan.cont-code:
 /*   IF FIRST-OF (loan.cont-code) THEN
   DO: */
      RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,OUTPUT sName).
      sContCode = loan.cont-code.
   /* END.
   ELSE
      ASSIGN
        sContCode = ""
        sName     = "". */
   PUT STREAM vvs UNFORMATTED delFilFromLoan(sContCode) delim
                              sName DELIM
                              chowhe.id-op delim 
                              loan-int.mdate delim
                              REPLACE(STRING(loan-int.amt-rub),".",",") delim 
                              "'" + TRIM(ENTRY(1,op-entry.acct-db,"@")) delim 
                              "'" + TRIM(ENTRY(1,op-entry.acct-cr,"@")) eol.   
END.
OUTPUT STREAM vvs CLOSE.

RUN sndbispc ("file=" + fname + ";class=bq").
{intrface.del}

PROCEDURE GetName:
   DEFINE INPUT PARAMETER cat AS CHARACTER.
   DEFINE INPUT PARAMETER id AS INT64.
   DEFINE OUTPUT PARAMETER sname AS CHARACTER.
 
   IF cat = "Ч" THEN
   DO:
      FIND FIRST PERSON 
         WHERE PERSON.PERSON-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE PERSON THEN
         /* ФИО клиента */
         sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
   END.
   ELSE
   DO:
      FIND FIRST CUST-CORP 
         WHERE CUST-CORP.CUST-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE CUST-CORP THEN
         /* наименование организации */
         sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
   END.
END PROCEDURE.
