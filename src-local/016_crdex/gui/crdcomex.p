{globals.i}
{intrface.get tmess}

/* +++ crdcomex.p was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:21am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: crdcomex.p
      Comment: Процедура kau-db для КардКом
   Parameters:
         Uses:
      Used by:
      Created: 28/04/2009 kraw (0103092)
     Modified: 26/06/2009 kraw (0113266) автоматическое списание с картотек
     Modified: 22/12/2009 kraw (0120748) исправление ошибки некорректного выбора документа постановки.
*/


DEFINE INPUT PARAMETER Rid     AS RECID   NO-UNDO.
DEFINE INPUT PARAMETER in-dbcr AS LOGICAL NO-UNDO. /* no - cr  yes - db */

{globals.i}
{kautools.lib}

DEFINE VARIABLE kau-rid   AS RECID                 NO-UNDO .
DEFINE VARIABLE ret-value AS CHARACTER             NO-UNDO.
DEFINE VARIABLE choice    AS LOGICAL   INITIAL YES NO-UNDO.

DEFINE VARIABLE mOpSurr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpTrans  AS INT64   NO-UNDO.
DEFINE VARIABLE mKauRecid AS RECID    NO-UNDO.

DEFINE VARIABLE mCurrentKau AS CHARACTER NO-UNDO.

DEFINE BUFFER op-entry    FOR op-entry.
DEFINE BUFFER op          FOR op.
DEFINE BUFFER xop-entry   FOR op-entry.
DEFINE BUFFER xop         FOR op.
DEFINE BUFFER kau         FOR kau.
DEFINE BUFFER bkau        FOR kau.

FIND FIRST op-entry WHERE RECID(op-entry) EQ Rid EXCLUSIVE-LOCK.

FIND FIRST op OF op-entry NO-LOCK NO-ERROR.

kau-rid = 0.

FOR EACH xop WHERE xop.op-transaction EQ op.op-transaction NO-LOCK,
    EACH xop-entry OF xop WHERE xop-entry.acct-db EQ op-entry.acct-cr
      NO-LOCK:

   IF {assigned xop-entry.kau-db} THEN
   DO:
      op-entry.kau-cr = xop-entry.kau-db.

      FIND FIRST kau WHERE kau.kau EQ xop-entry.kau-db NO-LOCK NO-ERROR.

      IF AVAILABLE kau THEN
         kau-rid = RECID(kau).
      LEAVE.
   END.
END.

IF kau-rid EQ 0 THEN
DO:
   mOpSurr = "".
   mOpTrans = ?.

   mKauRecid  = INT64(GetSysConf("Карт1274")).
   IF mKauRecid NE 0 THEN DO:

      FIND FIRST bKau WHERE RECID(bKau) EQ mKauRecid NO-LOCK NO-ERROR.
      IF AVAIL(bKau) THEN
         mOpSurr = bKau.kau.
   END.

   IF NOT {assigned mOpSurr} THEN 
      mOpSurr = GetSysConf("_cblc2_2_opo_surr").

   IF NOT {assigned mOpSurr} THEN
   DO:

      FOR EACH xop WHERE xop.op-transaction EQ op.op-transaction
                     AND xop.acct-cat       EQ "o"
         NO-LOCK,
          EACH xop-entry OF xop NO-LOCK:

          IF {assigned xop-entry.kau-cr} THEN
          DO:              
      /* xop-entry.kau-cr - показывает на документ постановки на картотеку 2 (или БлСч)*/
            mOpSurr = xop-entry.kau-cr.

          END.
      END.
   END.

   IF {assigned mOpSurr} THEN
   DO:

      FOR EACH xop WHERE xop.op EQ INT64(ENTRY(1, mOpSurr)) NO-LOCK:
         mOpTrans = xop.op-transaction.
      END.
   END.

   mOpSurr = ?.

   IF mOpTrans NE ? THEN
   DO:

      FOR EACH xop WHERE xop.op-transaction EQ mOpTrans 
                     AND xop.acct-cat       EQ "b" 
         NO-LOCK,
          EACH xop-entry OF xop WHERE xop-entry.acct-db EQ op-entry.acct-cr NO-LOCK:

          IF {assigned xop-entry.kau-db} THEN
          DO:
            mOpSurr = xop-entry.kau-db.
          END.
      END.
   END.

   IF {assigned mOpSurr} THEN
   DO:
      FOR FIRST kau WHERE kau.kau EQ mOpSurr NO-LOCK:
         kau-rid = RECID(kau).
      END.
   END.
END.
RUN SetSysConf IN h_base ("_cblc2_2_opo_surr", "").

{crdex.i &dbcr     = cr 
         &CARTNAME = "комиссий"}


/* --- crdcomex.p was humbly modified by (c)blodd converter v.1.09 on 7/26/2016 8:21am --- */
