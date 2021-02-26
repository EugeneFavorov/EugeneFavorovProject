/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ЗАО "Банковские информационные системы"
     Filename: crdstopcop.p
      Comment: Процедура приостановки списания с К2
   Parameters: 
         Uses:
      Used by: 
      Created: 02/02/2011 kraw (0116612)
     Modified: 
*/

{globals.i}

{intrface.get xclass}
{tmpobj.def}  /* Объявление таблицы TmpObj. */


{ch_cart.i}

DEFINE INPUT PARAM iKauIdLst AS CHAR NO-UNDO.

DEFINE VARIABLE mAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRID     AS RECID     NO-UNDO.
DEFINE VARIABLE mIsAmbig AS LOGICAL   NO-UNDO.

DEFINE VARIABLE mDateB   AS DATE      NO-UNDO.
DEFINE VARIABLE mDateE   AS DATE      NO-UNDO.

DEFINE BUFFER oacct FOR acct.

FORM
   SKIP
   mAcct LABEL "Лицевой счет" FORMAT "x(20)" HELP "Лицевой счет"
   SKIP
   WITH FRAME fAcct CENTERED ROW 10 OVERLAY SIDE-LABELS.

ON F1 OF mAcct IN FRAME fAcct
DO:

   DO TRANSACTION:
      RUN browseld.p("acctb",
                     "cust-cat",
                     "Ю,Ч,Б",
                     "",
                     4).

      IF LASTKEY NE KEYCODE("ESC") AND pick-value NE ? THEN
         mAcct:SCREEN-VALUE = pick-value.
   END.
END.

IF NOT {assigned iKauIdLst} THEN
iKauIdLst = "Карт-ка2".

PAUSE 0.

UPDATE mAcct WITH FRAME fAcct.

HIDE FRAME fAcct.

IF LASTKEY EQ KEYCODE("ESC") OR NOT {assigned mAcct} THEN
   RETURN.

{find-act.i
   &acct = mAcct
}

IF NOT AVAILABLE acct THEN
DO:
   MESSAGE "Счет" mAcct "не найден"
   VIEW-AS ALERT-BOX ERROR. 
   RETURN.
END.

{empty TmpObj}

IF LOOKUP("Карт-ка2",iKauIdLst) NE 0 THEN
DO:
   RUN GetCar_2(mAcct,
                acct.currency,
                TODAY,
                OUTPUT mRID,
                OUTPUT mIsAmbig).

   IF mIsAmbig THEN
   DO:
      FIND bal-acct WHERE bal-acct.acct-cat EQ "o"
                      AND bal-acct.kau-id   EQ "Карт-ка2"
         NO-LOCK NO-ERROR.

      pick-value = "".
      RUN "acct(k).p"(bal-acct.bal-acct,
                      bal-acct.kau-id,
                      acct.cust-cat,
                      acct.cust-id,
                      acct.currency,
                      "*",
                      4).

      IF {assigned pick-value} THEN
      FIND oacct WHERE oacct.acct     EQ ENTRY(1,pick-value)
                   AND oacct.currency EQ ENTRY(2,pick-value) 
           NO-LOCK NO-ERROR.
   END.
   ELSE
   FIND FIRST oacct WHERE RECID(oacct) EQ mRID NO-LOCK NO-ERROR.

   IF AVAILABLE oacct THEN
   FOR EACH kau WHERE kau.acct     EQ oacct.acct
                  AND kau.currency EQ oacct.currency
                  AND kau.zero-bal EQ NO  
       NO-LOCK,
       EACH op WHERE op.op         EQ INT64(ENTRY(1, kau.kau))
       NO-LOCK:

       IF mDateB EQ ? THEN
          mDateB = op.op-date.
       ELSE
          mDateB = MINIMUM(mDateB, op.op-date).

       IF mDateE EQ ? THEN
          mDateE = op.op-date.
       ELSE
          mDateE = MAXIMUM(mDateE, op.op-date).

       CREATE TmpObj.
       TmpObj.rid = RECID(op).
   END.
END.

IF LOOKUP("КартБлСч",iKauIdLst) NE 0 THEN
DO:
   RUN GetCar_A(mAcct,
                acct.currency,
                "КБС",
                TODAY,
                OUTPUT mRID,
                OUTPUT mIsAmbig).

   IF mIsAmbig THEN
   DO:
      FIND bal-acct WHERE bal-acct.acct-cat EQ "o"
                      AND bal-acct.kau-id   EQ "КартБлСч"
                    NO-LOCK NO-ERROR.
                                        
      pick-value = "".
      RUN "acct(k).p"(bal-acct.bal-acct,
                      bal-acct.kau-id,
                      acct.cust-cat,
                      acct.cust-id,
                      acct.currency,
                      "*",
                      4).

      IF {assigned pick-value} THEN
      FIND oacct WHERE oacct.acct     EQ ENTRY(1,pick-value)
                   AND oacct.currency EQ ENTRY(2,pick-value) NO-LOCK NO-ERROR.
   END.
   ELSE
   FIND FIRST oacct WHERE RECID(oacct) EQ mRID NO-LOCK NO-ERROR.

   IF AVAILABLE oacct THEN
   FOR EACH kau 
      WHERE kau.acct     EQ oacct.acct
        AND kau.currency EQ oacct.currency
        AND kau.zero-bal EQ NO  
      NO-LOCK,
       EACH op WHERE op.op         EQ INT64(ENTRY(1, kau.kau))
       NO-LOCK:

       IF mDateB EQ ? THEN
          mDateB = op.op-date.
       ELSE
          mDateB = MINIMUM(mDateB, op.op-date).

       IF mDateE EQ ? THEN
          mDateE = op.op-date.
       ELSE
          mDateE = MAXIMUM(mDateE, op.op-date).

       CREATE TmpObj.
       TmpObj.rid = RECID(op).
   END.
END.

FIND FIRST TmpObj NO-ERROR.

IF NOT AVAILABLE TmpObj THEN
DO:
   MESSAGE "Документы на картотеке не обнаружены"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

mTmpObjHand = TEMP-TABLE TmpObj:HANDLE.

RUN browseld.p ("op",
                "UseTmpObjInQuery~001op-date1~001op-date2~001FirstFrame~001ViewSC~001BrwRole~001Title",
                STRING(mTmpObjHand) + CHR(1) + STRING(mDateB) + CHR(1) + STRING(mDateE) + "~00116~001ПриостСпис~001StopWritting~001" + SUBSTRING(mAcct, 1, 20),
                "",
                4).


{intrface.del}

