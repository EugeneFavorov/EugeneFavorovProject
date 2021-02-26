/*              
Банковская интегрированная система БИСквит
*/

{globals.i}
{intrface.get tmess}
{intrface.get exch}

DEFINE VARIABLE mPersonID AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpDog   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNomDog   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDogMIR   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctOver AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctMIR  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCID      AS CHARACTER NO-UNDO.

DEF FRAME  mFrm90909
   mCID AT ROW 2 COL 20 COLON-ALIGNED 
   HELP "CID клиента или нажмите F1 для выбора"
   LABEL "CID клиента" FORMAT "X(7)"
   VIEW-AS FILL-IN SIZE 8 BY 1.

FORM
   mCID
WITH FRAME mFrm90909.

ON F1 OF mCID IN FRAME mFrm90909
DO:
   RUN pclass.p ('Over-MIR','Over-MIR','Список клиентов',1).
   IF (LASTKEY = 13  OR
       LASTKEY = 10) AND
       pick-value <> ? THEN 
       SELF:SCREEN-VALUE = pick-value.
END.

ON LEAVE OF mCID IN FRAME mFrm90909
DO:
   IF LASTKEY EQ 27
   THEN
   DO:
      mCID:SCREEN-VALUE = "".
      RETURN NO-APPLY.
   END.
   
   FIND FIRST code WHERE TRUE 
      AND code.class   EQ "Over-MIR"
      AND code.parent  EQ "Over-MIR"
      AND code.code    EQ mCID:SCREEN-VALUE
      AND code.misc[1] EQ "NO"
      AND code.description[3] NE ""
      AND code.description[3] NE ?
   NO-LOCK NO-ERROR.
   IF AVAIL(code) THEN
   DO:
      mCID      = TRIM(code.code).
      mPersonID = TRIM(code.val).
      mClName   = TRIM(code.name).
      mNomDog   = TRIM(code.description[1]).
      mAcctOver = TRIM(code.description[2]).
      mAcctMIR  = TRIM(code.description[3]).
      
      FIND FIRST acct WHERE TRUE
         AND acct.number EQ mAcctMIR
         AND acct.filial-id EQ "0500"
      NO-LOCK NO-ERROR.
      IF AVAIL(acct) THEN
         mTmpDog = GetXAttrValueEx("acct",acct.acct + "," + acct.currency,"ДогОткрЛС","").
      IF {assigned mTmpDog} THEN
      DO:
         IF NUM-ENTRIES(mTmpDog) GT 1 THEN
         mDogMIR = TRIM(ENTRY(2,mTmpDog)).
      END.
   END.
   ELSE
   DO:
      FIND FIRST code WHERE TRUE 
         AND code.class   EQ "Over-MIR"
         AND code.parent  EQ "Over-MIR"
         AND code.code    EQ mCID:SCREEN-VALUE
      NO-LOCK NO-ERROR.
      IF AVAIL(code) 
      AND code.misc[1] EQ "YES" 
      THEN
      DO:
         RUN Fill-AlertSysMes IN h_tmess ("","","1",
         "По этому клиенту распоряжение уже подписано.").
         mCID:SCREEN-VALUE = "".
         RETURN NO-APPLY.
      END.   
      IF NOT AVAIL(code) THEN
      DO:          
         RUN Fill-AlertSysMes IN h_tmess ("","","1",
                "Недействительный код классификатора.").
         mCID:SCREEN-VALUE = "".
         RETURN NO-APPLY.
      END.
      
      FIND FIRST code WHERE TRUE 
         AND code.class   EQ "Over-MIR"
         AND code.parent  EQ "Over-MIR"
         AND code.code    EQ mCID:SCREEN-VALUE
         AND (code.description[3] EQ "" OR code.description[3] EQ ?)
      NO-LOCK NO-ERROR.
      IF AVAIL(code) 
      THEN
      DO:
         RUN Fill-AlertSysMes IN h_tmess ("","","1",
         "У этого клиента нет счета карты МИР.").
         IF NOT AVAIL(code) THEN          
         mCID:SCREEN-VALUE = "".
         RETURN NO-APPLY.
      END.
   END.
END.

DO TRANS ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
   UPDATE 
      mCID
   WITH FRAME mFrm90909 SIDE-LABEL ROW 6 TITLE COLOR brigth-white 
       "[ Выбор клиента ]" 1 COL OVERLAY 
   CENTERED.
END.

HIDE FRAME mFrm90909 NO-PAUSE.

IF LASTKEY EQ KEYCODE("ESC") 
THEN pick-value = "".
ELSE pick-value = mCID      + CHR(1) +
                  mPersonID + CHR(1) + 
                  mClName   + CHR(1) +
                  mNomDog   + CHR(1) +
                  mDogMIR   + CHR(1) +
                  mAcctOver + CHR(1) + 
                  mAcctMIR  + CHR(1).

RETURN  pick-value.
