/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ЗАО "Банковские информационные системы"
     Filename: pp-prsfn.p
      Comment: Библиотека парсерных функций старого парсера
   Parameters:
         Uses:
      Used by:
      Created: 26.02.2006 14:56 ILVI (52132)
     Modified: 
*/
&GLOBAL-DEFINE rstack  20
&GLOBAL-DEFINE type-er ser

{globals.i}             /* Глобальные переменные сессии. */
{def-wf.i
   &NEW_PARSER = YES}
{g-error.def}
{sh-defs.i}
{oldpars.def}
{ksh-defs.i new}
{form.def}
{intrface.get trans}
{intrface.get pbase}

DEFINE VARIABLE bcur-db  AS CHARACTER NO-UNDO.
DEFINE VARIABLE bcur-cr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE bacct-db AS CHARACTER NO-UNDO.
DEFINE VARIABLE bacct-cr AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprate   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE chpar2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chpar1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bncr     AS INT64   NO-UNDO.
DEFINE VARIABLE naimks   AS CHARACTER   NO-UNDO.

DEFINE BUFFER xwop FOR wop.

/* Получение контекста */
PROCEDURE SetEnv.
   DEFINE INPUT  PARAMETER iSer      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iTcur     AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iInOpDate AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iRid      AS RECID      NO-UNDO.
   DEFINE INPUT  PARAMETER iWopSurr  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iPn       AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iPj       AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER TABLE FOR ttPars.

   ASSIGN
      ser        = iSer     
      tcur       = iTcur    
      in-op-date = iInOpDate
      WopSurr    = iWopSurr
      pn         = iPn
      pj         = iPj
   .
   
   /* перегрузка расшаренных переменных и таблиц в переменные и таблицы персистентной процедуры  */
   RUN shrprsvr.p (OUTPUT naimks, 
                   OUTPUT bcur-db, 
                   OUTPUT bcur-cr, 
                   OUTPUT bacct-db,
                   OUTPUT bacct-cr,
                   OUTPUT remove-amt,
                   OUTPUT send-amt,
                   OUTPUT sprate,  
                   OUTPUT chpar2,  
                   OUTPUT chpar1,  
                   OUTPUT bncr,    
                   OUTPUT TABLE frm-fields,
                   OUTPUT TABLE wop).

   FIND FIRST wop WHERE 
              wop.op-kind  EQ ENTRY(1,iWopSurr)
          AND wop.op-templ EQ INT64(ENTRY(2,iWopSurr)) NO-LOCK NO-ERROR.

   IF AVAIL wop THEN
      rid = RECID(wop).
   
   ASSIGN
      mvar     = ""
      result_l = ?
   .

   FOR EACH ttPars NO-LOCK:
      ASSIGN 
         mvar[ttPars.id]     = ttPars.fChar
         result_l[ttPars.id] = ttPars.fDec
      .
   END.
   {empty ttPars}
END PROCEDURE.

/* Возврат контекста */
PROCEDURE GetEnv.
   DEFINE OUTPUT PARAMETER oSer  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oTcur AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oPn   AS INT64    NO-UNDO.
   DEFINE OUTPUT PARAMETER oPj   AS INT64    NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ttPars.

   DEFINE VARIABLE vNum AS INT64 NO-UNDO.

   {empty ttPars}

   ASSIGN
      oSer  = ser    
      oTcur = tcur
      oPn   = pn
      oPj   = pj
   .

   /* перегрузка переменных и таблиц персистентной процедуры в расшаренные переменные и таблицы  */
   RUN lcprsvr.p (INPUT naimks, 
                  INPUT bcur-db, 
                  INPUT bcur-cr, 
                  INPUT bacct-db,
                  INPUT bacct-cr,
                  INPUT remove-amt,
                  INPUT send-amt,
                  INPUT sprate,  
                  INPUT chpar2,  
                  INPUT chpar1,  
                  INPUT bncr,    
                  INPUT TABLE frm-fields,
                  INPUT TABLE wop).
   
   DO vNum = 1 TO {&rstack}:
      CREATE ttPars.
      ASSIGN
         ttPars.id    = vNum
         ttPars.fChar = mvar[vNum]     
         ttPars.fDec  = result_l[vNum] 
      .
   END.

END PROCEDURE.

{g-pfunc.def}
{parsacct.def}
{details.fun}

/* препроцессоры определены для работы пользовательских процедур */
&GLOBAL-DEFINE PARSER-DETAILS-P  *
&GLOBAL-DEFINE PARSER-PARSACCT-P *
&GLOBAL-DEFINE PARSER-PARSSEN-P  *

{extrpars.fun}

PROCEDURE DestroyInterface.
   {intrface.del}          /* Выгрузка инструментария. */ 
END PROCEDURE.

