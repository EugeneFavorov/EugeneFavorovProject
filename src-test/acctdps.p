/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: SOZSCHET.P
      Comment: Транзакция создания счетов.
   Parameters:
         Uses:
      Used by:
      Created: 
*/
{globals.i}
{intrface.get xclass}
{intrface.get ovl}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{tmprecid.def}
DEF INPUT PARAM iOpDate  AS DATE  NO-UNDO.
DEF INPUT PARAM iRecKind AS RECID NO-UNDO.

DEF VAR mErrorCode AS INT64  NO-UNDO.
DEF VAR mCount     AS INT64  NO-UNDO.
DEF VAR mContract  AS CHAR   NO-UNDO.
DEF VAR mClsLoan   AS CHAR   NO-UNDO.
DEF VAR vOk        AS LOGICAL NO-UNDO.
DEF VAR mSDate     AS DATE   NO-UNDO.

DEF BUFFER op-kind FOR op-kind.
DEF BUFFER loan    FOR loan.

FIND FIRST op-kind WHERE RECID(op-kind) = iRecKind NO-LOCK NO-ERROR.

RUN browseld.p ("dep_person",
                            "status" + CHR(1) + "currency",
                            "ф" + CHR(1) + "",
                            "status" + CHR(1) + "currency",
                            4). 

iOpDate = if GetOpDate() <> ? then GetOpDate() else iOpDate.
DO TRANS ON ERROR   UNDO, LEAVE
         ON END-KEY UNDO, LEAVE:
   FOR EACH tmprecid NO-LOCK,
      FIRST loan WHERE RECID(loan) EQ tmprecid.id  
   NO-LOCK :
      /* Set_Loan установит значения номера договора в переменные библиотеки ovl */
      vOk = Set_Loan  ( loan.contract, loan.cont-code) .

      RUN credacct.p (iOpDate,
                      RECID(op-kind),
                      RECID(loan),
                      IF mCount GT 1  
                      THEN
                         ?
                      ELSE 
                        ("РЕДАКТИРОВАНИЕ СЧЕТОВ ПО ДОГОВОРУ № " + loan.doc-ref)
                           ,
                      OUTPUT mErrorCode).
/*         message mErrorCode view-as alert-box.*/


/*find first loan-acct where loan-acct.cont-code = loan.cont-code and loan-acct.acct begins '90909'.
if avail loan-acct then find first acct where acct.acct = loan-acct.acct.
if avail acct then acct.user-id = "B0400AAV".
*/

      IF mErrorCode = -1 THEN UNDO, LEAVE.

      DELETE tmprecid.

   END.
END.

{intrface.del}