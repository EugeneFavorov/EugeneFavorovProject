/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  g-saldo.p
      Comment:  Сальдирование остатков. Списание текущей суммы
                Переходник.
      Created:  12/05/2003 NIK
*/
DEFINE INPUT PARAMETER iOpDate   LIKE  op.op-date  NO-UNDO.
DEFINE INPUT PARAMETER iKindRID  AS    RECID       NO-UNDO.

RUN g-saldo2.p (iOpDate,iKindRID,NO) NO-ERROR.   /* Без учета курса ЦБ        */
IF ERROR-STATUS:ERROR
   THEN RETURN ERROR "error".
   ELSE RETURN.
/******************************************************************************/
