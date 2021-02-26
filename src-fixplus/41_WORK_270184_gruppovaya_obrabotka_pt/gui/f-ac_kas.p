{globals.i}

/* +++ f-ac_kas.p was humbly modified by (c)blodd converter v.1.09 on 9/7/2016 12:27pm +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: f-ac_kas.p
      Comment: Форма для дополнительных соглашений к Договору РКО.
   Parameters:
         Uses:
      Used BY:
      Created: 02.06.2016 Sami 0270184
     Modified:
*/

{globals.i}
{intrface.get xclass}
{intrface.get tmess} 
{flt-file.i}
{topkind.def}

DEFINE INPUT PARAMETER iRec      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iPar2     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iPar3     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iID       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iMode     AS INT       NO-UNDO.
DEFINE INPUT PARAMETER iPar6     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mINumL           AS INT       NO-UNDO.
DEFINE VARIABLE mParentContract  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParentCont-Code AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOk              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErrMsg          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctRKO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCcatRKO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCidRKO          AS INTEGER   NO-UNDO.

DEFINE BUFFER bloan  FOR loan.

{form.def}

IF iMode EQ {&MOD_ADD} THEN
DO:
   ASSIGN
      mParentContract  = GetFltVal("Parent-Contract")
      mParentCont-Code = GetFltVal("Parent-Cont-Code")
   .

   FIND FIRST bloan WHERE bloan.Contract  EQ mParentContract
                     AND bloan.Cont-Code EQ mParentCont-Code
   NO-LOCK NO-ERROR.

   IF NOT AVAILABLE bloan THEN
   {return_no_apply.i}

   /* Счет договора РКО */
   FOR LAST loan-acct
      WHERE (loan-acct.cont-code  EQ mParentCont-Code)
        AND (loan-acct.contract   EQ mParentContract)
        AND (loan-acct.acct-type  EQ mParentContract)
      NO-LOCK:

      mAcctRKO = loan-acct.acct + "," + loan-acct.currency.
      mCcatRKO = bloan.cust-cat.
      mCidRKO  = bloan.cust-id.
   END.
END.

DEFINE VARIABLE mDate AS DATE FORMAT "99/99/9999":U
   LABEL "Дата начала            "
   VIEW-AS FILL-IN SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE mDateEnd AS DATE FORMAT "99/99/9999":U
   LABEL "Дата окончания         "
   VIEW-AS FILL-IN SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE mDateClose AS DATE FORMAT "99/99/9999":U
   LABEL "    Закрыто  "
   VIEW-AS FILL-IN SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE mINN AS CHAR FORMAT "x(12)":U
   LABEL "ИНН получателя         "
   VIEW-AS FILL-IN SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE mName AS CHAR FORMAT "x(150)":U
   LABEL "Наименование получателя"
   /*VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 40 BY 2*/ NO-UNDO.

DEFINE VARIABLE mDetails AS CHAR FORMAT "x(210)":U
   LABEL "Условие списания       "
   VIEW-AS FILL-IN SIZE 40 BY 1 NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
   " "                  SKIP
   mINN                 SKIP
/* mDate    VIEW-AS FILL-IN SIZE 40 BY 1            SKIP
   mDateEnd VIEW-AS FILL-IN SIZE 40 BY 1            SKIP */
   mDate                SKIP
   mDateEnd  mDateClose SKIP
   "Наименование получателя:" SKIP
   mName    NO-LABEL VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 65 BY 4 SKIP
   "Условие списания       :" SKIP
   mDetails NO-LABEL VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 65 BY 4
AT ROW 11 COL 1 LEFT-ALIGNED
WITH 1 DOWN KEEP-TAB-ORDER OVERLAY TITLE "Соглашение"
SIDE-LABELS NO-UNDERLINE THREE-D
AT COL 5 ROW 6
SIZE 70 BY 16.

/* ************************  Control Definitions  *********************** */

ON F1 OF mDate IN FRAME fMain
DO:
   DO TRANSACTION:
      RUN calend.p.

      IF {&KEY_FUNCTION} ({&LAST_KEY}) NE "END-ERROR" THEN
      DO:
         ASSIGN
            mDate = DATE(pick-value)
         .
         DISPLAY mDate WITH FRAME fMain.
      END.
   END.
   RETURN.
END.

ON F1 OF mDateEnd IN FRAME fMain
DO:
   DO TRANSACTION:
      RUN calend.p.

      IF {&KEY_FUNCTION} ({&LAST_KEY}) NE "END-ERROR" THEN
      DO:
         ASSIGN
            mDateEnd = DATE(pick-value)
         .
         DISPLAY mDateEnd WITH FRAME fMain.
      END.
   END.
   RETURN.
END.

ON F1 OF mDateClose IN FRAME fMain
DO:
   DO TRANSACTION:
      RUN calend.p.

      IF {&KEY_FUNCTION} ({&LAST_KEY}) NE "END-ERROR" THEN
      DO:
         ASSIGN
            mDateClose = DATE(pick-value)
         .
         DISPLAY mDateClose WITH FRAME fMain.
      END.
   END.
   RETURN.
END.


ON LEAVE, RETURN OF mDate IN FRAME fMain
DO:
   ASSIGN  mDate.
END.

ON LEAVE, RETURN OF mDateEnd IN FRAME fMain
DO:
   ASSIGN  mDateEnd.
END.

ON LEAVE, RETURN OF mDateClose IN FRAME fMain
DO:
   ASSIGN  mDateClose.
END.

ON LEAVE, RETURN OF mINN IN FRAME fMain
DO:
   ASSIGN  mINN.
END.

ON LEAVE, RETURN OF mName IN FRAME fMain
DO:
    ASSIGN  mName.
END.

ON LEAVE, RETURN OF mDetails IN FRAME fMain
DO:
    ASSIGN  mDetails.
END.

ON GO OF FRAME fMain ANYWHERE
DO:
   IF iMode EQ {&MOD_EDIT} THEN
   DO:
      FIND FIRST loan
        WHERE loan.contract  EQ ENTRY(1, iID)
          AND loan.cont-code EQ ENTRY(2, iID)
        EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN
         loan.doc-num      = mINN
         loan.comment      = mName
         loan.open-date    = mDate
         loan.end-date     = mDateEnd
         loan.close-date   = mDateClose
         .
      UpdateSigns("loan", loan.contract + "," + loan.cont-code, "УслСпис", mDetails, NO).
   END.

   IF iMode EQ {&MOD_ADD} THEN
   DO:
      mINumL = 1.

      FOR EACH bloan WHERE bloan.class-code EQ "loanr_ac"
            AND bloan.contract EQ "КассСогАкц"
            AND bloan.Parent-Contract EQ mParentContract
            AND bloan.Parent-Cont-Code EQ mParentCont-Code
         NO-LOCK BREAK BY bloan.cont-code:

         IF mINumL LE INT(ENTRY(1,bloan.cont-code,"_"))
         THEN
            mINumL = INT(ENTRY(1,bloan.cont-code,"_")) + 1.
      END.

      CREATE loan.
      ASSIGN
         loan.class-code      = "loanr_ac"
         loan.contract        = "КассСогАкц"
         loan.cont-code       = (STRING(mINumL) + "_" + DelFilFromLoan(mParentCont-Code))
         loan.Parent-Contract  = mParentContract
         loan.Parent-Cont-Code = mParentCont-Code
         loan.cust-cat        = mCcatRKO
         loan.cust-id         = mCidRKO
         loan.doc-num         = mINN
         loan.comment         = mName
         loan.open-date       = mDate
         loan.end-date        = mDateEnd
         loan.close-date      = mDateClose
         loan.filial-id       = shFilial
         .
         UpdateSigns("loan", loan.contract + "," + loan.cont-code, "УслСпис", mDetails, NO).

      IF (mDateClose EQ ?)
      THEN DO:
         /* Открываем счет 90909 и создаем проводку на 1 руб */
         {empty tOpKindParams}     /* очистить таблицу параметров */
         ASSIGN
            lOk =  TDAddParam("__acct", mAcctRKO)
               AND TDAddParam("__loan", loan.cont-code)
               AND TDAddParam("__ccat", mCcatRKO)
               AND TDAddParam("__cid",  STRING(mCidRKO))
               AND TDAddParam("__rcvr", mName)
               AND TDAddParam("__det" , "Доп.соглашение к " + DelFilFromAcct(ENTRY(1,mAcctRKO)) + " от " + STRING(mDate, "99.99.9999")
                                      + ", получатель: " + mName + ", ИНН " + mINN + ", условия оплаты: " + mDetails)
            NO-ERROR.
         RUN ex-trans.p ("_inCrd3ac", TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).
         IF NOT lOk
         THEN RUN Fill-SysMes IN h_tmess ("","","-1", "Ошибка при открытии счета 90909: " + cErrMsg).
      END.
   END.
END.

/* ************************  Main Block *********************** */

PAUSE 0.

IF iMode EQ {&MOD_EDIT} OR iMode EQ {&MOD_VIEW}
THEN DO:
   FIND FIRST loan
      WHERE loan.contract  EQ ENTRY(1, iID)
        AND loan.cont-code EQ ENTRY(2, iID)
      NO-LOCK NO-ERROR.
   ASSIGN
      mINN       =  loan.doc-num
      mName      =  loan.comment
      mDate      =  loan.open-date
      mDateEnd   =  loan.end-date
      mDateClose =  loan.close-date
      mDetails   =  GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "УслСпис", ?).
   .
END.

DISPLAY
   mINN
   mName
   mDate
   mDateEnd
   mDateClose
   mDetails
WITH FRAME fMain.

IF iMode EQ {&MOD_EDIT} OR iMode EQ {&MOD_ADD}
THEN
   {set.i &THIS_FRAME = "fMain" &EXFILE = "f-ac_kas.p.st1" {&*}} .
ELSE DO:
   DISPLAY mName mDate mDateEnd mDateClose mDetails WITH FRAME fMain.
   {pause.i &THIS_FRAME = "fMain"}
   HIDE FRAME fMain.
END.

/* --- f-ac_kas.p was humbly modified by (c)blodd converter v.1.09 on 9/7/2016 12:27pm --- */
