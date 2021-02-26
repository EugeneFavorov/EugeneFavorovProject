/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2012 ЗАО "Банковские информационные системы"
     Filename: ijpt.p
      Comment:
   Parameters:
      Created: 12/03/12 16:28:14 KMBIS
     Modified:
*/
{globals.i}
{chkacces.i}
{flt-val.i}
{wordwrap.def}
{intrface.get xclass}

DEFINE VARIABLE iMask     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iType     AS CHARACTER  NO-UNDO.

DEFINE VARIABLE name-cl   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cdetails  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mInt      AS INT64      NO-UNDO.
DEFINE VARIABLE mDoc-type AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCounter  AS INT64      NO-UNDO.
DEFINE VARIABLE mBIC      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAcct     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mOAcct    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAction   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mSumma    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE mCur-name AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mName-bank AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBeg-date AS DATE       NO-UNDO.
DEFINE VARIABLE mEnd-date AS DATE       NO-UNDO.
DEFINE VARIABLE mDetails  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDateObr  AS CHARACTER  NO-UNDO EXTENT 20.
DEFINE VARIABLE mIndB     AS INT64      NO-UNDO.
DEFINE VARIABLE mIndE     AS INT64      NO-UNDO.
DEFINE VARIABLE mAcctSend AS CHARACTER  NO-UNDO.
DEFINE BUFFER   opo       FOR op.
DEFINE BUFFER   oe        FOR op-entry.

{tmprecid.def &SH=YES}
{getdates.i}

/*-------------------- Формирование отчета --------------------*/
{strtout3.i &cols=122 &option=Paged}

ASSIGN
   mName-bank = FGetSetting("Банк","","")
   mBeg-date  = DATE("15/09/2015")
   mEnd-date  = DATE("15/09/2015")
   iMask      = "405*,406*,407*,40802*,40807*,40821*,420*,421*,422*,4250*,47407*,47417*"
   iType      = "02,015"
   mBeg-date  = beg-date
   mEnd-date  = end-date
   .

DEFINE BUFFER o-op       FOR op.
DEFINE BUFFER o-op-entry FOR op-entry.

PUT UNFORMATTED " Журнал регистрации инкассовых поручений и платежных требований" SKIP.

IF mBeg-date EQ mEnd-date THEN
   PUT UNFORMATTED " за " STRING(mBeg-date, "99/99/9999") SKIP.
ELSE
   PUT UNFORMATTED " за период с " STRING(mBeg-date, "99/99/9999") " по " STRING(mEnd-date, "99/99/9999") SKIP.

PUT UNFORMATTED " " mName-bank SKIP.
PUT SKIP(1).
PUT UNFORMATTED
"┌─────┬──────────┬─────────┬──────────┬─────────────────┬────────────────────┬────────────┬──────────────────────────────────────────────────────────┐" SKIP
"│  N  │ Дата     │  Номер  │ Дата     │ Сумма           │ Номер счета        │ Действие   │ Дата                                                     │" SKIP
"│ п/п │ поступл. │  док-та │ док-та   │ документа       │ плательщика        │ над док-том│ обработки                                                │" SKIP
"├─────┼──────────┼─────────┼──────────┼─────────────────┼────────────────────┼────────────┼──────────────────────────────────────────────────────────┤" SKIP.
mCounter = 0.

/* Оплаченные */
OPL:
FOR EACH op
   WHERE filial-id     EQ shFilial
     AND op.op-date    GE mBeg-date
     AND op.op-date    LE mEnd-date + 5
     AND op.op-status  GE CHR(251)
   NO-LOCK:

   IF NOT (op.ins-date GE mBeg-date
      AND  op.ins-date LE mEnd-date) THEN NEXT.
   IF NOT CAN-DO(iType,op.doc-type)  THEN NEXT.

   /* оплата с картотеки - пропускаем */
   FOR EACH opo
      WHERE (opo.op-transaction   = op.op-transaction)
        AND (opo.class-code       = "opo")
      NO-LOCK,
   FIRST oe OF opo
      WHERE (oe.acct-db       BEGINS "99999")
      NO-LOCK:

      NEXT OPL.
   END.

   mDoc-type = "".
   FIND FIRST doc-type
      WHERE doc-type.doc-type = op.doc-type
      NO-LOCK NO-ERROR.
   IF AVAIL(doc-type)
   THEN mDoc-type = doc-type.digital.

   ASSIGN
      mDetails = GetXAttrValue("op", STRING(op.op), "Примечание")
      mDateObr = STRING(op.op-date, "99/99/9999")
      mAction  = "Оплачен".

   FIND FIRST op-bank OF op
      NO-LOCK NO-ERROR.
   mBic = IF AVAIL(op-bank) THEN op-bank.bank-code ELSE FGetSetting("БанкМФО", "", "").

   FOR EACH op-entry OF op
      WHERE CAN-DO(iMask,op-entry.acct-db)
      NO-LOCK:

      IF op-entry.currency = "" THEN
         ASSIGN
            mSumma    = op-entry.amt-rub
            mCur-name = "810"
         .
      ELSE
         ASSIGN
            mSumma    = op-entry.amt-cur
            mCur-name = op-entry.currency
         .
      mAcct = STRING(IF (op-entry.acct-db NE ?) THEN op-entry.acct-db ELSE "","x(20)").
      mAcct = DelFilFromAcct(mAcct).
      IF CAN-DO("47417*",op-entry.acct-db) THEN NEXT.
      RUN PrintLine.
   END. /* FOR EACH op-entry OF op */

   FIND FIRST op-entry OF op
      NO-LOCK NO-ERROR.
   IF NOT AVAIL(op-entry) THEN
   DO:
      mAcct     = DelFilFromAcct(GetXAttrValueEx("op",STRING(op.op), "acctbal", "")).
      mSumma    = DEC(GetXAttrValueEx("op",STRING(op.op), "amt-rub", "0")) NO-ERROR.
      IF mSumma EQ ? THEN mSumma = 0.
      mCur-name = "810".
      RUN PrintLine.
   END.
END.

/* Картотеки */
FOR EACH op
   WHERE filial-id  EQ shFilial
     AND op.op-date EQ ?
   NO-LOCK:

   IF NOT CAN-DO(iType,op.doc-type)  THEN NEXT.
   IF NOT (op.ins-date GE mBeg-date
      AND  op.ins-date LE mEnd-date) THEN NEXT.

   mDoc-type = "".
   FIND FIRST doc-type
      WHERE doc-type.doc-type EQ op.doc-type
      NO-LOCK NO-ERROR.
   IF AVAIL(doc-type)
   THEN mDoc-type = doc-type.digital.
   mDetails = GetXAttrValue("op", STRING(op.op), "Примечание").

   IF NOT {assigned mDetails} THEN
   DO:
      FIND FIRST op-entry OF op
         NO-LOCK NO-ERROR.
      IF AVAIL(op-entry) THEN
      FOR EACH PackObject
         WHERE PackObject.file-name EQ "op-entry"
           AND PackObject.Surrogate EQ TRIM(STRING(op-entry.op)) + "," +
                                       TRIM(STRING(op-entry.op-entry))
         NO-LOCK,
      EACH Packet
         WHERE Packet.PacketID      EQ PackObject.PacketID
/*         AND Packet.mail-format   BEGINS "XML-ED274" */
         NO-LOCK,
      EACH Seance
         WHERE Seance.DIRECT        EQ "Экспорт"
           AND  Seance.SeanceID     EQ Packet.SeanceID
         NO-LOCK,
      EACH PacketText
         WHERE PacketText.PacketID  EQ Packet.PacketID
         NO-LOCK:

         mIndB    = INDEX(PacketText.Contents,"<Annotation>").
         mIndE    = INDEX(PacketText.Contents,"</Annotation>").
         mDetails = SUBSTRING(CODEPAGE-CONVERT(PacketText.Contents,SESSION:CHARSET,"1251"),
                              (mIndB + 12),(mIndE - mIndB - 12)).
      END.
   END.

   FIND FIRST signs
      WHERE signs.file EQ "op"
        AND signs.code EQ "op-bal"
        AND signs.code-value EQ STRING(op.op)
      NO-LOCK NO-ERROR.
   IF AVAIL(signs) THEN
      FIND FIRST o-op
         WHERE o-op.op EQ INT64(signs.surrogate)
         NO-LOCK NO-ERROR.
/*   ELSE                                                  */
/*      FIND FIRST o-op WHERE                              */
/*             o-op.op-transaction EQ op.op-transaction    */
/*         AND o-op.acct-cat       EQ "o" NO-LOCK NO-ERROR.*/
   IF AVAIL(o-op) THEN
   DO:
      mDateObr = STRING(o-op.op-date,"99/99/9999").
      FIND FIRST o-op-entry OF o-op
         NO-LOCK NO-ERROR.
      IF AVAIL(o-op-entry) THEN
      DO:
         mOAcct = DelFilFromAcct(o-op-entry.acct-db).
         {find-act.i
            &acct = mOAcct
         }
         IF AVAIL(acct) THEN
            mAction = acct.contract.
         IF mDateObr[1] EQ ? THEN
         ASSIGN
            mDateObr = "Аннулирован внебал. док-т"
            mAction  = "Возврат".
      END.
   END.
   ELSE DO:
      ASSIGN
         mDateObr = IF {assigned mDetails} THEN mDetails ELSE "Аннулирован бал. док-т"
         mAction  = "Возврат".
   END.
   FIND FIRST op-bank OF op
      NO-LOCK NO-ERROR.
   mBic = IF AVAIL(op-bank) THEN op-bank.bank-code ELSE FGetSetting("БанкМФО", "", "").

   FOR EACH op-entry OF op
      WHERE CAN-DO(iMask,op-entry.acct-db)
      NO-LOCK:

      IF op-entry.currency = "" THEN
         ASSIGN
            mSumma = op-entry.amt-rub
            mCur-name = "810"
         .
      ELSE
         ASSIGN
            mSumma = op-entry.amt-cur
            mCur-name = op-entry.currency
         .
      mAcct = STRING(IF (op-entry.acct-db NE ?) THEN op-entry.acct-db ELSE "","x(20)").

      mAcctSend = GetXAttrValueEx("op",STRING(op.op),"acct-send","").
      IF mAcctSend BEGINS "30101" THEN NEXT.

      RUN PrintLine.
   END. /*FOR EACH op-entry OF op*/

   FIND FIRST op-entry OF op
      NO-LOCK NO-ERROR.
   IF NOT AVAIL(op-entry) THEN
   DO:
      mAcct     = DelFilFromAcct(GetXAttrValueEx("op",STRING(op.op), "acctbal", "")).
      mSumma    = DEC(GetXAttrValueEx("op",STRING(op.op), "amt-rub", "0")) NO-ERROR.
      IF mSumma EQ ? THEN mSumma = 0.
      mCur-name = "810".

      RUN PrintLine.
   END.
END.

PUT UNFORMATTED
"└─────┴──────────┴─────────┴──────────┴─────────────────┴────────────────────┴────────────┴──────────────────────────────────────────────────────────┘" SKIP.
/* PUT UNFORMATTED "mCounter = " mCounter SKIP. */

{signatur.i}
{endout3.i &nofooter=yes}
/* ============================================================================ */

PROCEDURE PrintLine PRIVATE.

   mCounter = mCounter + 1.

   {wordwrap.i
    &s = mDateObr
    &n = 20
    &l = 55
   }

   PUT UNFORMATTED "│" AT 1  STRING(mCounter) TO 5
                   "│" AT 7  STRING(op.ins-date,"99/99/9999") AT 8
                   "│" AT 18 TRIM(op.doc-num) TO 27
                   "│" AT 28 STRING(op.doc-date,"99/99/9999") AT 29
                   "│" AT 39 mSumma FORMAT ">,>>>,>>>,>>9.99" TO 56
                   "│" AT 57 mAcct    AT 58
                   "│" AT 78 mAction  AT 79
                   "│" AT 91 mDateObr[1] AT 92
                   "│" AT 150
   SKIP.
   DO mInt = 2 TO 20:
      IF mDateObr[mInt] NE "" THEN
      PUT UNFORMATTED "│" AT 1
                      "│" AT 7
                      "│" AT 18
                      "│" AT 28
                      "│" AT 39
                      "│" AT 57
                      "│" AT 78
                      "│" AT 91 mDateObr[mInt] AT 92
                      "│" AT 150
      SKIP.
      ELSE LEAVE.
   END.
END PROCEDURE. /* PrintLine */
