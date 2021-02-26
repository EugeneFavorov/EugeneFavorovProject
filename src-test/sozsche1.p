/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: SOZSCHET.P
      Comment: Транзакция создания счетов.
   Parameters:
         Uses:
      Used by:
      Created: 15.07.2004 17:08 FEPA
     Modified: 28.06.2006 ZIAL (0049910) Ошибка автоматической привязки счетов (sozschet).
     Modified: 16.06.2009 17:38 buan     <comment>
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

SetOpDate(iOpDate).
FIND FIRST op-kind WHERE RECID(op-kind) = iRecKind NO-LOCK NO-ERROR.

mContract = GetXattrValueEx("op-kind",op-kind.op-kind,"contract","кредит").
mClsLoan  = GetXattrValueEx("op-kind",op-kind.op-kind,"cls-loan","").
IF {assigned mClsLoan} THEN
   mContract = mContract + "^" + mClsLoan.
mSDate = GetOpDate(). /*сохраняем дату ОП*/
RUN get_loans11.p ( mContract, OUTPUT mCount).
iOpDate = GetOpDate().
DO TRANS ON ERROR   UNDO, LEAVE
         ON END-KEY UNDO, LEAVE:
   FOR EACH tmprecid NO-LOCK,
      FIRST loan WHERE RECID(loan) EQ tmprecid.id  
   NO-LOCK :
      /* Set_Loan установит значения номера договора в переменные библиотеки ovl */
      vOk = Set_Loan  ( loan.contract, loan.cont-code) .
      RUN credacct11.p (iOpDate,
                      RECID(op-kind),
                      RECID(loan),
                         ?
                           ,
                      OUTPUT mErrorCode).
   
      IF mErrorCode = -1 THEN UNDO, LEAVE.
      DELETE tmprecid.
   END.
END.
SetOpDate(mSDate). /*Восстанавливаем дату ОП*/
{intrface.del}