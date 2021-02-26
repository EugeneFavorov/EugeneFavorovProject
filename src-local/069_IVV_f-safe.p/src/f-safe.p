&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          bisquit          PROGRESS
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-loan-cond NO-UNDO LIKE loan-cond
       FIELD safe-num AS CHARACTER /* Номер сейфовой ячейки */
       FIELD safe-status AS CHARACTER /* Статус сейфовой ячейки */
       FIELD safe-type AS CHARACTER /* Тип сейфовой ячейки */
       FIELD nds$ AS DECIMAL /* Ставка НДС */
       FIELD tariff-type AS CHARACTER /* Тип тарифа */
       FIELD sewifsrok$ AS CHARACTER /* Срок аренды ячейки */
       FIELD sum AS DECIMAL /* Сумма залога */
       FIELD currency AS CHAR /* Валюта залога */
       FIELD safe-filial AS CHARACTER /* Код филиала сейфа */
       FIELD cred-offset AS CHARACTER /*  */
       FIELD int-offset AS CHARACTER /*  */
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-loan-cond" "loan-cond" }
       
       .
DEFINE TEMP-TABLE tt-loan-rent NO-UNDO LIKE loan
       FIELD datasogl$ AS DATE
       FIELD Branch-Rent as char
       
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-loan-rent" "" }
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*------------------------------------------------------------------------

  File: lspon-ed.p

  Description: Редактор договоров с банками-спонсорами

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Turin

  Created: 


------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
     
{globals.i}     
{intrface.get sloan}
{intrface.get loan}
{intrface.get asset}
{intrface.get tsafe}
{intrface.get tmess}
{intrface.get date}
{intrface.get refer}
{intrface.get acct}
{mf-loan.i}
{topkind.def}
{safeb.fun}
{safe_open.i}
{parsin.def}

 DEFINE VAR      mNumType    AS CHARACTER NO-UNDO.
 DEFINE VAR      mTehStat    AS CHARACTER NO-UNDO.
 DEFINE VAR      mStat       AS CHARACTER NO-UNDO.
 DEFINE VAR      mContCode   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mKau        AS CHARACTER NO-UNDO. /* Кау */
 DEFINE VARIABLE mNdsCm      AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mNds        AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mProcUpd    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mHndlUpd    AS HANDLE    NO-UNDO.
 DEFINE VARIABLE mFake       AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mTarType    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
 DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
 DEFINE VARIABLE mLEndDate   AS DATE      NO-UNDO.
 DEFINE VARIABLE mLOpeDate   AS DATE      NO-UNDO.
 DEFINE VARIABLE mNDaysOld   AS INT64     NO-UNDO.
 DEFINE VARIABLE mLStatus    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mRes        AS LOGICAL   NO-UNDO.
 DEFINE VARIABLE mOkey       AS LOGICAL   NO-UNDO.
 DEFINE VARIABLE mMessage    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mSurrCond   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mSurrCond1  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mNN         AS INT64   NO-UNDO.
 DEFINE VARIABLE mDSNumCh    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mDSNum      AS INT64   NO-UNDO.
 DEFINE VARIABLE mAmoutDop   AS DEC       NO-UNDO.
 DEFINE VARIABLE mPeriod     AS INT64   NO-UNDO.
 DEFINE VARIABLE mFlag       AS LOG       NO-UNDO.
 DEFINE VARIABLE mPrevStat   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE mProDate    AS LOGICAL   NO-UNDO.
 DEFINE VARIABLE mIndSearch  AS LOGICAL   NO-UNDO.
 DEFINE VARIABLE mChangeTar  AS LOGICAL   NO-UNDO.
 DEFINE VARIABLE mSellSurr   AS CHARACTER NO-UNDO.

 DEFINE BUFFER bAsset  FOR asset.
 DEFINE BUFFER bloan FOR loan.
 DEFINE BUFFER bloan-cond  FOR loan-cond.
 DEFINE BUFFER bbloan-cond FOR loan-cond.
 DEFINE BUFFER bcomm-rate  FOR comm-rate.
 DEFINE BUFFER bbcomm-rate FOR comm-rate.

IF FGetSetting("ВклДатНач", "", "Да") NE "Да" AND FGetSetting("ВклДатНач", "", "Да") NE "YES" THEN
   mProDate = YES. /* Работаем по новому, т. е. не включаем open-date в срок аренды */
ELSE
   mProDate = NO. /* Работаем по старому, т. е. включаем open-date в срок аренды */

ASSIGN
   mChangeTar = FGetSetting("indSafe", "ChangeTar", "Нет") EQ "Да"
   mIndSearch = FGetSetting("indSafe", "indSearch", "Нет") EQ "Да".

 CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&GLOBAL-DEFINE MAIN-FRAME fMain

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-loan-rent

/* Definitions for FRAME fMain                                          */
&Scoped-define SELF-NAME fMain
&Scoped-define QUERY-STRING-fMain FOR EACH tt-loan-rent SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY {&SELF-NAME} FOR EACH tt-loan-rent SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain tt-loan-rent
&Scoped-define FIRST-TABLE-IN-QUERY-fMain tt-loan-rent


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-loan-rent.branch-id tt-loan-rent.cust-id ~
tt-loan-rent.Branch-Rent tt-loan-rent.doc-ref tt-loan-rent.cont-type ~
tt-loan-rent.open-date tt-loan-rent.end-date tt-loan-cond.safe-type ~
tt-loan-cond.safe-num tt-loan-cond.safe-status tt-loan-cond.sum ~
tt-loan-cond.sewifsrok$ tt-loan-cond.tariff-type tt-loan-cond.disch-type ~
tt-loan-cond.nds$ tt-loan-cond.cred-period tt-loan-cond.cred-month ~
tt-loan-cond.cred-date tt-loan-cond.cred-offset tt-loan-cond.int-period ~
tt-loan-cond.int-month tt-loan-cond.int-date tt-loan-cond.int-offset ~
tt-loan-rent.user-id tt-loan-rent.loan-status tt-loan-rent.close-date ~
tt-loan-rent.datasogl$ tt-loan-rent.comment 
&Scoped-define ENABLED-TABLES tt-loan-rent tt-loan-cond
&Scoped-define FIRST-ENABLED-TABLE tt-loan-rent
&Scoped-define SECOND-ENABLED-TABLE tt-loan-cond
&Scoped-Define ENABLED-OBJECTS mNDays 
&Scoped-Define DISPLAYED-FIELDS tt-loan-rent.branch-id ~
tt-loan-rent.cust-cat tt-loan-rent.cust-id tt-loan-rent.Branch-Rent ~
tt-loan-rent.doc-ref tt-loan-rent.cont-type tt-loan-rent.open-date ~
tt-loan-rent.end-date tt-loan-cond.safe-type tt-loan-cond.safe-num ~
tt-loan-cond.safe-status tt-loan-cond.sum tt-loan-cond.currency ~
tt-loan-cond.sewifsrok$ tt-loan-cond.tariff-type tt-loan-cond.disch-type ~
tt-loan-cond.nds$ tt-loan-cond.cred-period tt-loan-cond.cred-month ~
tt-loan-cond.cred-date tt-loan-cond.cred-offset tt-loan-cond.int-period ~
tt-loan-cond.int-month tt-loan-cond.int-date tt-loan-cond.int-offset ~
tt-loan-rent.user-id tt-loan-rent.loan-status tt-loan-rent.close-date ~
tt-loan-rent.datasogl$ tt-loan-rent.comment 
&Scoped-define DISPLAYED-TABLES tt-loan-rent tt-loan-cond
&Scoped-define FIRST-DISPLAYED-TABLE tt-loan-rent
&Scoped-define SECOND-DISPLAYED-TABLE tt-loan-cond
&Scoped-Define DISPLAYED-OBJECTS mBranchName mSumma CustName mNDays mTariff ~
separator-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-loan-rent.branch-id tt-loan-rent.cust-cat ~
tt-loan-rent.cust-id tt-loan-rent.Branch-Rent tt-loan-rent.doc-ref ~
tt-loan-rent.cont-type tt-loan-rent.open-date mNDays tt-loan-rent.end-date ~
tt-loan-cond.safe-type tt-loan-cond.safe-num tt-loan-cond.sum ~
tt-loan-cond.sewifsrok$ tt-loan-cond.tariff-type tt-loan-cond.disch-type ~
tt-loan-cond.nds$ tt-loan-cond.cred-period tt-loan-cond.cred-month ~
tt-loan-cond.cred-date tt-loan-cond.cred-offset tt-loan-cond.int-period ~
tt-loan-cond.int-month tt-loan-cond.int-date tt-loan-cond.int-offset ~
tt-loan-rent.loan-status tt-loan-rent.user-id tt-loan-rent.datasogl$ tt-loan-rent.comment 
&Scoped-define List-2 tt-loan-rent.branch-id tt-loan-rent.cust-cat ~
tt-loan-rent.cust-id tt-loan-rent.open-date mNDays tt-loan-rent.end-date ~
tt-loan-cond.cred-offset tt-loan-cond.int-offset tt-loan-rent.user-id ~
tt-loan-rent.loan-status tt-loan-rent.close-date tt-loan-rent.datasogl$ ~
tt-loan-rent.comment
&Scoped-define List-3 tt-loan-rent.branch-id tt-loan-rent.cust-id ~
tt-loan-rent.doc-ref tt-loan-rent.open-date mNDays tt-loan-rent.end-date ~
tt-loan-cond.cred-offset tt-loan-cond.int-offset tt-loan-rent.user-id ~
tt-loan-rent.loan-status tt-loan-rent.close-date tt-loan-rent.datasogl$ ~
tt-loan-rent.comment 
&Scoped-define List-4 tt-loan-rent.open-date mNDays tt-loan-rent.contract ~
tt-loan-rent.end-date tt-loan-cond.disch-type tt-loan-cond.nds$ ~
tt-loan-cond.cred-offset tt-loan-cond.int-offset 
&Scoped-define List-5 mBranchName CustName 
&Scoped-define List-6 mNDays tt-loan-rent.end-date tt-loan-cond.cred-offset ~
tt-loan-cond.int-offset 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FEndDate TERMINAL-SIMULATION 
FUNCTION FEndDate RETURNS CHARACTER
  ( INPUT  iCode AS CHAR,
    INPUT  iDate AS DATE,
    OUTPUT oBegDate AS Date, 
    OUTPUT oEndDate AS Date )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FDopKrit TERMINAL-SIMULATION 
FUNCTION FDopKrit RETURNS CHARACTER
  ( INPUT iClassC AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FDopKrit1 TERMINAL-SIMULATION 
FUNCTION FDopKrit1 RETURNS CHARACTER
  ( INPUT iLstParam AS CHARACTER,
    INPUT iName     AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FTypTar TERMINAL-SIMULATION 
FUNCTION FTypTar RETURNS CHARACTER
  ( INPUT iBranchId AS CHARACTER,
    INPUT iSafeType AS CHARACTER,
    INPUT iSrok     AS INT64 ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE CustName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Клиент" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 35 BY 1
     &ELSE SIZE 35 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mBranchName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
     &ELSE SIZE 27 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mNDays AS CHARACTER FORMAT "X(256)":U 
     LABEL "к-во дней" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mSumma AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "К оплате" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mTariff AS CHARACTER FORMAT "X(17)" INITIAL ? 
     LABEL "Тариф" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
     &ELSE SIZE 12 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator-1 AS CHARACTER FORMAT "X(256)":U INITIAL "--------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator-2 AS CHARACTER FORMAT "X(256)":U INITIAL "--------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator-3 AS CHARACTER FORMAT "X(256)":U INITIAL "--------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 78 BY 1
     &ELSE SIZE 78 BY 1 &ENDIF.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      tt-loan-rent SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-loan-rent.branch-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 16 COLON-ALIGNED
          &ELSE AT ROW 1 COL 16 COLON-ALIGNED &ENDIF
          LABEL "Подразделение"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     mBranchName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 25 COLON-ALIGNED
          &ELSE AT ROW 1 COL 25 COLON-ALIGNED &ENDIF NO-LABEL
     tt-loan-rent.cust-cat
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 16 COLON-ALIGNED
          &ELSE AT ROW 2 COL 16 COLON-ALIGNED &ENDIF
          LABEL " Арендатор"
          VIEW-AS COMBO-BOX INNER-LINES 5
          DROP-DOWN-LIST
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-loan-rent.cust-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 27 COLON-ALIGNED
          &ELSE AT ROW 2 COL 27 COLON-ALIGNED &ENDIF
          LABEL "Код"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 7 BY 1
          &ELSE SIZE 7 BY 1 &ENDIF
     mSumma
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 62 COLON-ALIGNED
          &ELSE AT ROW 11 COL 62 COLON-ALIGNED &ENDIF NO-TAB-STOP 
     CustName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 43 COLON-ALIGNED
          &ELSE AT ROW 2 COL 43 COLON-ALIGNED &ENDIF
     tt-loan-rent.Branch-Rent
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 27 COLON-ALIGNED
          &ELSE AT ROW 3 COL 27 COLON-ALIGNED &ENDIF
          LABEL "Подразделение-арендатор"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
          &ELSE SIZE 15 BY 1 &ENDIF
     tt-loan-rent.doc-ref
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 17 COLON-ALIGNED
          &ELSE AT ROW 4 COL 17 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 20 BY 1 &ENDIF
     tt-loan-rent.cont-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 42 COLON-ALIGNED
          &ELSE AT ROW 4 COL 42 COLON-ALIGNED &ENDIF HELP
          "Тип договора." NO-LABEL FORMAT "x(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     separator-1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 1
          &ELSE AT ROW 5 COL 1 &ENDIF NO-LABEL
     tt-loan-rent.open-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 13 COLON-ALIGNED
          &ELSE AT ROW 6 COL 13 COLON-ALIGNED &ENDIF
          LABEL "C" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     mNDays
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 40 COLON-ALIGNED
          &ELSE AT ROW 6 COL 40 COLON-ALIGNED &ENDIF
     tt-loan-rent.contract
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 20 COLON-ALIGNED
          &ELSE AT ROW 18 COL 20 COLON-ALIGNED &ENDIF NO-LABEL DEBLANK 
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF NO-TAB-STOP 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-loan-rent.end-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 65 COLON-ALIGNED
          &ELSE AT ROW 6 COL 65 COLON-ALIGNED &ENDIF HELP
          "Дата окончания договора"
          LABEL "По" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     separator-2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 1
          &ELSE AT ROW 7 COL 1 &ENDIF NO-LABEL
     tt-loan-cond.safe-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 14 COLON-ALIGNED
          &ELSE AT ROW 8 COL 14 COLON-ALIGNED &ENDIF
          LABEL "Тип ячейки" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     tt-loan-cond.safe-num
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 42 COLON-ALIGNED
          &ELSE AT ROW 8 COL 42 COLON-ALIGNED &ENDIF
          LABEL "Номер ячейки" FORMAT "x(14)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     tt-loan-cond.safe-status
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 65 COLON-ALIGNED
          &ELSE AT ROW 8 COL 65 COLON-ALIGNED &ENDIF
          LABEL "Статус"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-loan-cond.sum
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 14 COLON-ALIGNED
          &ELSE AT ROW 9 COL 14 COLON-ALIGNED &ENDIF
          LABEL "Сумма залога"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 22 BY 1
          &ELSE SIZE 22 BY 1 &ENDIF
     tt-loan-cond.currency
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 42 COLON-ALIGNED
          &ELSE AT ROW 9 COL 42 COLON-ALIGNED &ENDIF
          LABEL "Вал" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-loan-cond.sewifsrok$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 14 COLON-ALIGNED
          &ELSE AT ROW 10 COL 14 COLON-ALIGNED &ENDIF
          LABEL "Срок аренды"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-loan-cond.tariff-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 42 COLON-ALIGNED
          &ELSE AT ROW 10 COL 42 COLON-ALIGNED &ENDIF
          LABEL "Тип тарифа" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     mTariff
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 62 COLON-ALIGNED
          &ELSE AT ROW 10 COL 62 COLON-ALIGNED &ENDIF
     tt-loan-cond.disch-type
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 16 COLON-ALIGNED
          &ELSE AT ROW 11 COL 16 COLON-ALIGNED &ENDIF HELP
          "Форма расчета платежей погашения процентов (F1 - выбор)"
          LABEL "Форма расчета" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-loan-cond.nds$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 42 COLON-ALIGNED
          &ELSE AT ROW 11 COL 42 COLON-ALIGNED &ENDIF
          LABEL "Ставка НДС" FORMAT ">9.99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 8 BY 1 &ENDIF
     tt-loan-cond.cred-period
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 14 COLON-ALIGNED
          &ELSE AT ROW 12 COL 14 COLON-ALIGNED &ENDIF
          LABEL "Оплата"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-loan-cond.cred-month
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 27 COLON-ALIGNED
          &ELSE AT ROW 12 COL 27 COLON-ALIGNED &ENDIF
          LABEL "Месяц" FORMAT ">9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
          &ELSE SIZE 6 BY 1 &ENDIF
     tt-loan-cond.cred-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 42 COLON-ALIGNED
          &ELSE AT ROW 12 COL 42 COLON-ALIGNED &ENDIF
          LABEL "День"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-loan-cond.cred-offset
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 62 COLON-ALIGNED
          &ELSE AT ROW 12 COL 62 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Сдвиг" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan-cond.int-period
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 14 COLON-ALIGNED
          &ELSE AT ROW 13 COL 14 COLON-ALIGNED &ENDIF NO-LABEL
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
     tt-loan-cond.int-month
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 27 COLON-ALIGNED
          &ELSE AT ROW 13 COL 27 COLON-ALIGNED &ENDIF
          LABEL "Месяц" FORMAT ">9"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
          &ELSE SIZE 6 BY 1 &ENDIF
     tt-loan-cond.int-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 42 COLON-ALIGNED
          &ELSE AT ROW 13 COL 42 COLON-ALIGNED &ENDIF
          LABEL "День"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 5 BY 1
          &ELSE SIZE 5 BY 1 &ENDIF
     tt-loan-cond.int-offset
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 62 COLON-ALIGNED
          &ELSE AT ROW 13 COL 62 COLON-ALIGNED &ENDIF HELP
          ""
          LABEL "Сдвиг" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     separator-3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 1
          &ELSE AT ROW 14 COL 1 &ENDIF NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-loan-rent.user-id
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 18 COLON-ALIGNED
          &ELSE AT ROW 15 COL 18 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-loan-rent.loan-status
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 42 COLON-ALIGNED
          &ELSE AT ROW 15 COL 42 COLON-ALIGNED &ENDIF
          LABEL "Статус"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan-rent.close-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 62 COLON-ALIGNED
          &ELSE AT ROW 15 COL 62 COLON-ALIGNED &ENDIF FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-loan-rent.datasogl$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 18 COLON-ALIGNED
          &ELSE AT ROW 16 COL 18 COLON-ALIGNED &ENDIF
          LABEL "Дата регистрации"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
          &ELSE SIZE 16 BY 1 &ENDIF
     tt-loan-rent.comment
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 17 COL 18 COLON-ALIGNED
          &ELSE AT ROW 17 COL 18 COLON-ALIGNED &ENDIF
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 57 BY 1
          &ELSE SIZE 57 BY 1 &ENDIF
     "Учет доходов:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 2
          &ELSE AT ROW 13 COL 2 &ENDIF
     "Тип:" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 4 BY 1
          &ELSE SIZE 4 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 39
          &ELSE AT ROW 4 COL 39 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-loan-cond T "?" NO-UNDO bisquit loan-cond
      ADDITIONAL-FIELDS:
          FIELD safe-num AS CHARACTER /* Номер сейфовой ячейки */
          FIELD safe-status AS CHARACTER /* Статус сейфовой ячейки */
          FIELD safe-type AS CHARACTER /* Тип сейфовой ячейки */
          FIELD nds$ AS DECIMAL /* Ставка НДС */
          FIELD tariff-type AS CHARACTER /* Тип тарифа */
          FIELD sewifsrok$ AS CHARACTER /* Срок аренды ячейки */
          FIELD sum AS DECIMAL /* Сумма залога */
          FIELD currency AS CHAR /* Валюта залога */
          FIELD safe-filial AS CHARACTER /* Код филиала сейфа */
          FIELD cred-offset AS CHARACTER /*  */
          FIELD int-offset AS CHARACTER /*  */
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-loan-cond" "loan-cond" }
          
          
      END-FIELDS.
      TABLE: tt-loan-rent T "?" NO-UNDO bisquit loan
      ADDITIONAL-FIELDS:
          FIELD datasogl$ AS DATE
          FIELD Branch-Rent as char
          
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-loan-rent" "" }
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW TERMINAL-SIMULATION ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 20.71
         WIDTH              = 92.43
         MAX-HEIGHT         = 20.71
         MAX-WIDTH          = 92.43
         VIRTUAL-HEIGHT     = 20.71
         VIRTUAL-WIDTH      = 92.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB TERMINAL-SIMULATION 
/* ************************* Included-Libraries *********************** */

{bis-tty.pro}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME fMain
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN tt-loan-rent.branch-id IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-loan-rent.Branch-Rent IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-loan-rent.close-date IN FRAME fMain
   2 3 EXP-FORMAT                                                       */
/* SETTINGS FOR FILL-IN tt-loan-rent.comment IN FRAME fMain
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN tt-loan-rent.cont-type IN FRAME fMain
   1 EXP-LABEL EXP-FORMAT EXP-HELP                                      */
/* SETTINGS FOR FILL-IN tt-loan-rent.contract IN FRAME fMain
   NO-DISPLAY NO-ENABLE 4 EXP-LABEL                                     */
ASSIGN 
       tt-loan-rent.contract:HIDDEN IN FRAME fMain           = TRUE
       tt-loan-rent.contract:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-loan-cond.cred-date IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-loan-cond.cred-month IN FRAME fMain
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN tt-loan-cond.cred-offset IN FRAME fMain
   1 2 3 4 6 EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN tt-loan-cond.cred-period IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-loan-cond.currency IN FRAME fMain
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       tt-loan-cond.currency:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR COMBO-BOX tt-loan-rent.cust-cat IN FRAME fMain
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN tt-loan-rent.cust-id IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN CustName IN FRAME fMain
   NO-ENABLE 5                                                          */
ASSIGN 
       CustName:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-loan-rent.datasogl$ IN FRAME fMain
   1 2 3 EXP-LABEL                                                      */
/* SETTINGS FOR FILL-IN tt-loan-cond.disch-type IN FRAME fMain
   1 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-loan-rent.doc-ref IN FRAME fMain
   1 3                                                                  */
/* SETTINGS FOR FILL-IN tt-loan-rent.end-date IN FRAME fMain
   1 2 3 4 6 EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN tt-loan-cond.int-date IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-loan-cond.int-month IN FRAME fMain
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN tt-loan-cond.int-offset IN FRAME fMain
   1 2 3 4 6 EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN tt-loan-cond.int-period IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-loan-rent.loan-status IN FRAME fMain
   2 3 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN mBranchName IN FRAME fMain
   NO-ENABLE 5                                                          */
ASSIGN 
       mBranchName:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mNDays IN FRAME fMain
   1 2 3 4 6                                                            */
/* SETTINGS FOR FILL-IN mSumma IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mTariff IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mTariff:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-loan-cond.nds$ IN FRAME fMain
   1 4 EXP-LABEL EXP-FORMAT                                             */
ASSIGN 
       tt-loan-cond.nds$:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-loan-rent.open-date IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN tt-loan-cond.safe-num IN FRAME fMain
   1 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       tt-loan-cond.safe-num:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tt-loan-cond.safe-status IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN tt-loan-cond.safe-type IN FRAME fMain
   1 EXP-LABEL EXP-FORMAT                                               */
ASSIGN 
       tt-loan-cond.safe-type:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN separator-1 IN FRAME fMain
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN separator-2 IN FRAME fMain
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN separator-3 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN tt-loan-cond.sewifsrok$ IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-loan-cond.sum IN FRAME fMain
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN tt-loan-cond.tariff-type IN FRAME fMain
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN tt-loan-rent.user-id IN FRAME fMain
   1 2 3                                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-loan-rent SHARE-LOCK.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fMain
&Scoped-define SELF-NAME tt-loan-rent.branch-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.branch-id TERMINAL-SIMULATION
ON ENTRY OF tt-loan-rent.branch-id IN FRAME fMain /* Подразделение */
DO:
  mBranchName:SCREEN-VALUE = GetCliName("В",
                                        SELF:SCREEN-VALUE,
                                        OUTPUT vAddr,
                                        OUTPUT vINN,
                                        OUTPUT vKPP,
                                        INPUT-OUTPUT vType,
                                        OUTPUT vCode,
                                        OUTPUT vAcct).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.branch-id TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.branch-id IN FRAME fMain /* Подразделение */
DO:
   {&BEG_BT_LEAVE}
 
   IF     iMode NE {&MOD_VIEW} 
      AND GetBufferValue("branch",
                         "where branch-id = '" + SELF:SCREEN-VALUE + "'",
                         "branch-id"
                        ) EQ ? THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Неверный код подразделения").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   mBranchName:SCREEN-VALUE = GetCliName("В",
                                         SELF:SCREEN-VALUE,
                                         OUTPUT vAddr,
                                         OUTPUT vINN,
                                         OUTPUT vKPP,
                                         INPUT-OUTPUT vType,
                                         OUTPUT vCode,
                                         OUTPUT vAcct).
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.Branch-Rent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.Branch-Rent TERMINAL-SIMULATION
ON F1 OF tt-loan-rent.Branch-Rent IN FRAME fMain /* Подразделение-арендатор */
DO:
   DO TRANSACTION:
      RUN browseld.p ("branch", 
                      "parent-id" + chr(1) + "isbank",
                      "Top" + CHR(1) + "yes",
                       "",
                       4).
      IF pick-value NE "loan-rent" THEN
         tt-loan-rent.Branch-Rent:SCREEN-VALUE = pick-value.
      
      FIND FIRST branch WHERE branch.Branch-Id EQ pick-value NO-LOCK NO-ERROR.
      CustName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = branch.Short-Name.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.Branch-Rent TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.Branch-Rent IN FRAME fMain /* Подразделение-арендатор */
DO:
   {&BEG_BT_LEAVE}

   IF     SELF:INPUT-VALUE EQ "" 
      AND tt-loan-rent.cust-cat:SCREEN-VALUE EQ "В" THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Номер подразделения-арендатора должен быть введен!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   FIND FIRST branch WHERE branch.Branch-Id EQ tt-loan-rent.branch-rent:SCREEN-VALUE
      NO-LOCK NO-ERROR.

   IF NOT AVAILABLE branch THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("",
                                  "",
                                  "0",
                                  "Подразделение " + 
                                  string(tt-loan-rent.branch-rent:SCREEN-VALUE) + 
                                  " не найдено").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   ELSE
       CustName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = branch.short-name.
   {&END_BT_LEAVE}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.close-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.close-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.close-date IN FRAME fMain /* Закрыт */
DO:
   DEFINE VARIABLE vDeltaDates AS INT64 NO-UNDO.

   vDeltaDates = tt-loan-rent.close-date:INPUT-VALUE - tt-loan-rent.open-date:INPUT-VALUE.

   IF mProDate THEN
      vDeltaDates = vDeltaDates - 1.

   IF vDeltaDates LT 0 THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Дата закрытия не может быть меньше даты начала").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.cont-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.cont-type TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.cont-type IN FRAME fMain /* cont-type */
DO:

   IF tt-loan-rent.cont-type NE tt-loan-rent.cont-type:SCREEN-VALUE IN FRAME fMain THEN
      RUN CredIntAssgn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.cred-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.cred-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.cred-date IN FRAME fMain /* День */
DO:

   IF     CAN-DO("Г,К,М,ПГ", tt-loan-cond.cred-period:SCREEN-VALUE)
      AND 
          (    tt-loan-cond.cred-date:INPUT-VALUE GT 31
            OR tt-loan-cond.cred-date:INPUT-VALUE LE 0 )
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Неправильное значение поля День").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.cred-month
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.cred-month TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.cred-month IN FRAME fMain /* Месяц */
DO:

   IF     tt-loan-cond.cred-period:SCREEN-VALUE EQ "К" 
      AND NOT CAN-DO("1,2,3", tt-loan-cond.cred-month:SCREEN-VALUE) 
      OR  tt-loan-cond.cred-period:SCREEN-VALUE EQ "ПГ"
      AND NOT CAN-DO("1,2,3,4,5,6", tt-loan-cond.cred-month:SCREEN-VALUE) 
      OR  tt-loan-cond.cred-period:SCREEN-VALUE EQ "Г"
      AND NOT CAN-DO("1,2,3,4,5,6,7,8,9,10,11,12", tt-loan-cond.cred-month:SCREEN-VALUE)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Неправильное значение поля Месяц").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.cred-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.cred-period TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.cred-period IN FRAME fMain /* Оплата */
DO:
   {&BEG_BT_LEAVE}
   CASE tt-loan-cond.cred-period:SCREEN-VALUE:
      WHEN "НС" THEN ASSIGN
         tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
         tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE.

      WHEN "" THEN ASSIGN
         tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
         tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE.

      WHEN "П" THEN ASSIGN
         tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
         tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE.

      WHEN "Кс" THEN ASSIGN
         tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
         tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE.

      WHEN "Д"  THEN ASSIGN
         tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
         tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE.

      WHEN "М"  THEN
      DO:

         IF    tt-loan-cond.cred-month:HIDDEN IN FRAME fMain EQ FALSE
            OR tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain EQ TRUE 
         THEN
         DO:
            ASSIGN
               tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
               tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = FALSE
            .
            RETURN NO-APPLY.
         END.
      END.

      OTHERWISE
      DO:

         IF    tt-loan-cond.cred-month:HIDDEN IN FRAME fMain EQ TRUE
            OR tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain EQ TRUE THEN
         DO:
            ASSIGN
               tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = FALSE
               tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = FALSE
            .
            RETURN NO-APPLY.
         END.
      END.
   END CASE.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.cust-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.cust-cat TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-loan-rent.cust-cat IN FRAME fMain /*  Арендатор */
DO:

   IF tt-loan-rent.cust-cat:SCREEN-VALUE EQ "В" THEN
   DO:
      tt-loan-rent.cust-id:HIDDEN     = YES.
      tt-loan-rent.Branch-Rent:HIDDEN = NO.
   END.
   ELSE
   DO:
      tt-loan-rent.cust-id:HIDDEN     = NO.
      tt-loan-rent.Branch-Rent:HIDDEN = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.cust-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.cust-id TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.cust-id IN FRAME fMain /* Код */
DO:
   {&BEG_BT_LEAVE}

   IF    tt-loan-rent.cust-id:SCREEN-VALUE EQ ?
      OR tt-loan-rent.cust-id:SCREEN-VALUE EQ "?" THEN 
      CustName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   ELSE
      CustName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = GetCliName(tt-loan-rent.cust-cat:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                                                tt-loan-rent.cust-id:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                                                OUTPUT vAddr,
                                                                OUTPUT vINN,
                                                                OUTPUT vKPP,
                                                                INPUT-OUTPUT vType,
                                                                OUTPUT vCode,
                                                                OUTPUT vAcct).

   {&END_BT_LEAVE}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.datasogl$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.datasogl$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.datasogl$ IN FRAME fMain /* Дата регистрации */
DO:
   {&BEG_BT_LEAVE}

   IF SELF:MODIFIED THEN
   DO:

      IF tt-loan-rent.datasogl$:INPUT-VALUE GT tt-loan-rent.open-date:INPUT-VALUE THEN
      DO:

         IF iMode EQ {&MOD_ADD} THEN 
            tt-loan-rent.open-date:SCREEN-VALUE = tt-loan-rent.datasogl$:SCREEN-VALUE.
         ELSE
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0",
            "Дата заключения договора не может быть больше даты открытия").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
      SELF:MODIFIED = NO.
   END.
       
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.disch-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.disch-type TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.disch-type IN FRAME fMain /* Форма расчета */
DO:
   DEFINE VARIABLE vRest   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vP      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vD      AS CHARACTER  NO-UNDO.

   IF tt-loan-cond.disch-type NE INT64  (tt-loan-cond.disch-type:SCREEN-VALUE IN FRAME fMain) THEN
   DO:
      /* Получить значение классификатора "ТипРасчАрд" по коду tt-loan-cond.ditsch-type  */

      vRest = GetCode("ТипРасчАрд", STRING(tt-loan-cond.disch-type:SCREEN-VALUE IN FRAME fMain)).

      IF {assigned vRest} THEN
      DO:
         /*
            Проверить, что заданный период погашения для графиков (поля tt-loan-cond.cred-period,
            tt-loan-cond.int-period, tt-loan-cond.int-date )  соответствуют списку значений
            параметра  "Р" из настройки vRest
         */

         vP = GetParamByNameAsChar(vRest, "P", GetParamByNameAsChar(vRest, "Р", "")).
         vD = GetParamByNameAsChar(vRest, "D", "").

         IF NUM-ENTRIES(vP) GT 1 THEN
         DO:
            tt-loan-cond.int-period:SCREEN-VALUE  IN FRAME fMain = ENTRY(1, vP).
            tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain = ENTRY(2, vP).
            APPLY "LEAVE" TO tt-loan-cond.cred-period.
            APPLY "LEAVE" TO tt-loan-cond.int-period.
         END.
      END.
      RUN CredIntAssgn.
      tt-loan-cond.disch-type =  INT64  (tt-loan-cond.disch-type:SCREEN-VALUE IN FRAME fMain).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-loan-rent.loan-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.loan-status TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.loan-status IN FRAME fMain
DO:
   FIND FIRST code WHERE code.class EQ "СтатусыЯчеек" AND
                         code.parent EQ "СтатусыЯчеек" AND
                         CAN-DO(code.description[1], tt-loan-rent.loan-status:SCREEN-VALUE)
   NO-LOCK NO-ERROR.
   IF AVAIL(code) THEN
   DO:
      tt-loan-cond.safe-status:SCREEN-VALUE = code.code.
      tt-loan-cond.safe-status              = code.code.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-loan-rent.doc-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.doc-ref TERMINAL-SIMULATION
ON F1 OF tt-loan-rent.doc-ref IN FRAME fMain /* Номер договора */
DO:

   DEFINE VARIABLE vCC AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vUNK AS CHARACTER NO-UNDO.

   IF iMode EQ {&MOD_ADD} THEN
   DO:   
   
      DO WHILE YES:

         vCC                          = GenNewSafeLoanNumber(tt-loan-rent.class-code, TODAY).
         IF INDEX(vCC,"[УНК]") GE 0 AND tt-loan-rent.cust-cat NE "В" THEN DO:
            IF tt-loan-rent.cust-cat:SCREEN-VALUE EQ "Ч" THEN 
               vUNK = GetXAttrValueEx("person", tt-loan-rent.cust-id:SCREEN-VALUE, "УНК", "").
            IF tt-loan-rent.cust-cat:SCREEN-VALUE EQ "Ю" THEN   
               vUNK = GetXAttrValueEx("cust-corp", tt-loan-rent.cust-id:SCREEN-VALUE, "УНК", "").
            IF tt-loan-rent.cust-cat:SCREEN-VALUE EQ "Б" THEN 
               vUNK = GetXAttrValueEx("banks", tt-loan-rent.cust-id:SCREEN-VALUE, "УНК", "").
            DO WHILE INDEX(vUNK,"0") EQ 1 :
               vUNK = SUBSTRING(vUNK,2,LENGTH(vUNK) - 1). 
            END.  
            vCC = REPLACE(vCC,"[УНК]",vUNK).
         END.
         tt-loan-rent.cont-code            = AddFilToAcct(vCC, shFilial).         
         tt-loan-rent.cont-code            = AddFilToAcct(vCC, shFilial).         
         tt-loan-rent.doc-ref:SCREEN-VALUE = DelFilFromAcct(vCC).

         IF NOT CAN-FIND (FIRST loan WHERE loan.contract  EQ "АРЕНДА"
                                       AND loan.cont-code EQ vCC) THEN
         DO:
            ASSIGN tt-loan-rent.doc-ref.
            LEAVE.
         END.
      END.
      
      RETURN NO-APPLY.  
      
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.doc-ref TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.doc-ref IN FRAME fMain /* Номер договора */
DO:
   {&BEG_BT_LEAVE}

   IF SELF:INPUT-VALUE EQ "" THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Номер договора должен быть введен!").
      RETURN NO-APPLY {&RET-ERROR}.
   END.

   IF CAN-FIND(FIRST loan WHERE loan.filial-id EQ tt-loan-rent.filial-id 
                            AND loan.contract  EQ tt-loan-rent.contract 
                            AND loan.doc-ref   EQ SELF:INPUT-VALUE ) THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Договор с таким номером уже существует").
      RETURN NO-APPLY {&RET-ERROR}.
   END.

   IF   INDEX(SELF:INPUT-VALUE,",") NE 0
     OR INDEX(SELF:INPUT-VALUE,";") NE 0 THEN
   DO:
      RUN fill-sysmes IN h_tmess ("", "", "0", "Номер договора содержит запрещенные символы.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.

   IF ShMode THEN 
      tt-loan-rent.cont-code = addFilToLoan(SELF:INPUT-VALUE,ShFilial).
    ELSE
      tt-loan-rent.cont-code = SELF:INPUT-VALUE.
   
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-loan-rent.loan-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.loan-status TERMINAL-SIMULATION
ON ENTRY OF tt-loan-rent.loan-status IN FRAME fMain
DO:
   mPrevStat = tt-loan-rent.loan-status:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.end-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.end-date IN FRAME fMain /* По */
DO:
   {&BEG_BT_LEAVE}

   IF mProDate THEN
   DO:

      IF tt-loan-rent.end-date:INPUT-VALUE LT tt-loan-rent.open-date:INPUT-VALUE + 1 THEN
      DO:
          RUN Fill-SysMes IN h_tmess ("","","0","Дата окончания должна быть больше даты начала").
          RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   ELSE
   DO:

      IF tt-loan-rent.end-date:INPUT-VALUE LT tt-loan-rent.open-date:INPUT-VALUE THEN
      DO:
          RUN Fill-SysMes IN h_tmess ("","","0","Дата окончания не должна быть меньше даты начала").
          RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.

   IF tt-loan-rent.end-date:INPUT-VALUE LT tt-loan-rent.end-date 
      AND CAN-FIND(FIRST loan-cond WHERE loan-cond.since     GE tt-loan-rent.end-date:INPUT-VALUE
                                     AND loan-cond.contract  EQ tt-loan-rent.contract
                                     AND loan-cond.cont-code EQ tt-loan-rent.cont-code) THEN
   DO:
       RUN Fill-SysMes IN h_tmess ("","","0",
       "Уменьшить дату окончания можно только удалив пролонгацию").
       RETURN NO-APPLY {&RET-ERROR}.
   END.

   IF mProDate THEN
      mNDays:SCREEN-VALUE = STRING(DATE(tt-loan-rent.end-date:SCREEN-VALUE) - DATE(tt-loan-rent.open-date:SCREEN-VALUE)).
   ELSE
      mNDays:SCREEN-VALUE = STRING(DATE(tt-loan-rent.end-date:SCREEN-VALUE) - DATE(tt-loan-rent.open-date:SCREEN-VALUE) + 1).
   
   IF iMode EQ {&MOD_EDIT} THEN
   DO:

      IF (mNDaysOld NE mNDays:INPUT-VALUE AND tt-loan-rent.open-date:INPUT-VALUE NE mLOpeDate) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0",
         "Нельзя менять дату открытия одновременно с пролонгацией (к-вом дней).").
         RETURN NO-APPLY {&RET-ERROR}.
      END.

      IF (mNDaysOld NE INT64(mNDays:INPUT-VALUE)) THEN
      DO:

         IF ((INT64(mNDays:INPUT-VALUE) - mNDaysOld) LT 
              INT64(FGetSetting("МинДнТар", "", "0"))) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0",
           "Невозможно пролонгировать договор на данное количество дней.").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.

      IF BT_Modify(SELF) AND 
         (mNDaysOld NE INT64(mNDays:INPUT-VALUE)) THEN
      DO:
         IF mIndSearch AND
            mChangeTar AND
           {assigned tt-loan-rent.end-date:SCREEN-VALUE } THEN
         DO:
            tt-loan-cond.tariff-type:SCREEN-VALUE = FTypTar(tt-loan-rent.branch-id:INPUT-VALUE,
                                                            tt-loan-cond.safe-type:SCREEN-VALUE,
                                                            INT64(mNDays:INPUT-VALUE) - mNDaysOld /* + 1 */
                                                            ).  
             RUN CalcTariff IN THIS-PROCEDURE.
         END.

         mNdsCm = GetXAttrValueEx("asset",
                                  tt-loan-cond.safe-filial + "," +
                                  tt-loan-cond.safe-type:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                  "nds_cm",
                                  "").

         RUN GetNDSValue IN h_asset (mNdsCm,
                                 DATE(tt-loan-rent.open-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 
                                 IF mProDate THEN 1 ELSE 0,
                                 OUTPUT mNds).
         ASSIGN
            tt-loan-cond.nds$:SCREEN-VALUE = mNds
         .

      END.

   END.
   
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.int-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.int-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.int-date IN FRAME fMain /* День */
DO:
   {&BEG_BT_LEAVE}

   IF CAN-DO("Г,К,М,ПГ", tt-loan-cond.int-period:SCREEN-VALUE)
      AND (   tt-loan-cond.int-date:INPUT-VALUE GT 31
           OR tt-loan-cond.int-date:INPUT-VALUE LE 0 )
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Неправильное значение поля День").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.int-month
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.int-month TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.int-month IN FRAME fMain /* Месяц */
DO:
   {&BEG_BT_LEAVE}

    IF     tt-loan-cond.int-period:SCREEN-VALUE EQ "К"
       AND NOT CAN-DO("1,2,3", tt-loan-cond.int-month:SCREEN-VALUE) 
       OR  tt-loan-cond.int-period:SCREEN-VALUE EQ "ПГ"
       AND NOT CAN-DO("1,2,3,4,5,6", tt-loan-cond.int-month:SCREEN-VALUE) 
       OR  tt-loan-cond.int-period:SCREEN-VALUE EQ "Г"
       AND NOT CAN-DO(",1,2,3,4,5,6,7,8,9,10,11,12", tt-loan-cond.int-month:SCREEN-VALUE)
    THEN
    DO:
       RUN Fill-SysMes IN h_tmess ("","","0","Неправильное значение поля Месяц").
       RETURN NO-APPLY {&RET-ERROR}.
    END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.int-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.int-period TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.int-period IN FRAME fMain /* int-period */
DO:
   {&BEG_BT_LEAVE}

   CASE tt-loan-cond.int-period:SCREEN-VALUE:
      WHEN "НС" THEN
         ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "" THEN
         ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "П" THEN
         ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "Кс" THEN
         ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "Д"  THEN
         ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "М"  THEN
      DO:

         IF    tt-loan-cond.int-month:HIDDEN IN FRAME fMain EQ FALSE
            OR tt-loan-cond.int-date:HIDDEN  IN FRAME fMain EQ TRUE THEN
         DO:
            ASSIGN
               tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
               tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = FALSE
            .
            RETURN NO-APPLY.
         END.
      END.

      OTHERWISE
      DO:

         IF    tt-loan-cond.int-month:HIDDEN IN FRAME fMain EQ TRUE
            OR tt-loan-cond.int-date:HIDDEN  IN FRAME fMain EQ TRUE THEN
         DO:
            ASSIGN
               tt-loan-cond.int-month:HIDDEN IN FRAME fMain = FALSE
               tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = FALSE
            .
            RETURN NO-APPLY.
         END.
      END.
   END CASE.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mNDays
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mNDays TERMINAL-SIMULATION
ON LEAVE OF mNDays IN FRAME fMain /* к-во дней */
DO:
   {&BEG_BT_LEAVE} 
   INT64(mNDays:SCREEN-VALUE) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Вводимое Вами значение должно быть числовым" VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.   
   IF DATE(tt-loan-rent.end-date:SCREEN-VALUE) <> tt-loan-rent.end-date THEN
      APPLY "LEAVE" TO tt-loan-rent.end-date.
   IF mNDays:SCREEN-VALUE NE "" THEN
   DO:

      IF mProDate THEN
         tt-loan-rent.end-date:SCREEN-VALUE = 
         STRING(INT64  (mNDays:SCREEN-VALUE) + DATE(tt-loan-rent.open-date:SCREEN-VALUE)).
      ELSE
         tt-loan-rent.end-date:SCREEN-VALUE = 
         STRING(INT64  (mNDays:SCREEN-VALUE) + DATE(tt-loan-rent.open-date:SCREEN-VALUE) - 1).
   END.
   ELSE
      APPLY "LEAVE" TO tt-loan-rent.end-date.

   IF {assigned mNDays:SCREEN-VALUE} THEN 
   DO:
      FIND FIRST code WHERE code.class = "СейфСроки2" AND
                            (IF NUM-ENTRIES(code.description[1],"-") > 1 THEN
                               INT64(ENTRY(1,code.description[1],"-")) <= INT64(mNDays:SCREEN-VALUE) AND  
                               INT64(mNDays:SCREEN-VALUE) <= INT64(ENTRY(2,code.description[1],"-"))
                             ELSE
                               INT64(code.description[1]) EQ INT64(mNDays:SCREEN-VALUE)
                             )
      NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN DO:
         tt-loan-cond.sewifsrok$:SCREEN-VALUE = STRING(code.code).
      END.
   END.      
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.nds$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.nds$ TERMINAL-SIMULATION
ON F1 OF tt-loan-cond.nds$ IN FRAME fMain /* Ставка НДС */
DO:
   pick-value = ?.

   DO TRANSACTION:

      RUN browseld.p ("commission",
                      "contract",
                      "axd",
                      "contract",
                      4).

      IF     {assigned pick-value}
         AND (LASTKEY = 10 OR LASTKEY = 13) THEN 
      DO:
         mNdsCm = pick-value.
         RUN GetNDSValue IN h_asset (mNdsCm,
                                     INPUT IF mProDate THEN (INPUT tt-loan-rent.open-date + 1) ELSE (INPUT tt-loan-rent.open-date),
                                     OUTPUT mNds).
         ASSIGN tt-loan-cond.nds$:SCREEN-VALUE = mNds.
      END.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.nds$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.nds$ IN FRAME fMain /* Ставка НДС */
DO:
   {&BEG_BT_LEAVE}

   IF    iMode EQ {&MOD_ADD} THEN
   DO:
   RUN FGetSummRent.
   END.
   {&END_BT_LEAVE}                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-rent.open-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-rent.open-date TERMINAL-SIMULATION
ON LEAVE OF tt-loan-rent.open-date IN FRAME fMain /* C */
DO:
   {&BEG_BT_LEAVE}

   IF  NOT {assigned tt-loan-rent.open-date:SCREEN-VALUE} THEN
      tt-loan-rent.open-date:SCREEN-VALUE = STRING(today).

   IF iMode EQ {&MOD_EDIT}
      AND DATE(tt-loan-rent.open-date:SCREEN-VALUE) LT tt-loan-rent.open-date THEN
   DO:
      MESSAGE "Нельзя уменьшать дату открытия договора"
      VIEW-AS ALERT-BOX. 
      RETURN NO-APPLY.
   END.

   IF iMode EQ {&MOD_EDIT}
      AND DATE(tt-loan-rent.open-date:SCREEN-VALUE) NE tt-loan-rent.open-date THEN
   DO:
      APPLY "LEAVE" TO mNDays.
   END.

   IF iMode EQ {&MOD_ADD} THEN
      tt-loan-cond.since = DATE(tt-loan-rent.open-date:SCREEN-VALUE).

/*
   IF BT_Modify(SELF) THEN
      APPLY "LEAVE" TO tt-loan-cond.sewifsrok$.
*/
   {&END_BT_LEAVE}                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.safe-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.safe-num TERMINAL-SIMULATION
ON F1 OF tt-loan-cond.safe-num IN FRAME fMain /* Номер ячейки */
DO:

   DEFINE VARIABLE vBranchID AS CHARACTER NO-UNDO.

   pick-value = ?.

   DO TRANSACTION:

      IF NOT {assigned tt-loan-cond.safe-type:SCREEN-VALUE} THEN
      DO:

         vBranchID = GetXAttrValueEx("_user",
                                     USERID("bisquit"),
                                     "Отделение",
                                     "").

         IF mProDate THEN
            RUN browseld.p("rent-safe",
                           "mBranch-id" + CHR(1) 
                         + "mfilial-id" + CHR(1) 
                         + "mStat" + CHR(1) 
                         + "mTehStat" + CHR(1) 
                         + "mDate" + CHR(1)
                         + "mTmpRsrv",
                           vBranchID + CHR(1) 
                         + tt-loan-cond.safe-filial + CHR(1) 
                         + "СВОБОДНА" + CHR(1) 
                         + "Исправна" + CHR(1) 
                         + STRING(DATE(tt-loan-rent.open-date:SCREEN-VALUE) + 1) + CHR(1), 
                           "mBranch-id",
                            4) NO-ERROR.
         ELSE
            RUN browseld.p("rent-safe",
                           "mBranch-id" + CHR(1) 
                         + "mfilial-id" + CHR(1) 
                         + "mStat" + CHR(1) 
                         + "mTehStat" + CHR(1) 
                         + "mDate" + CHR(1)
                         + "mTmpRsrv",
                           vBranchID + CHR(1) 
                         + tt-loan-cond.safe-filial + CHR(1) 
                         + "СВОБОДНА" + CHR(1) 
                         + "Исправна" + CHR(1) 
                         + tt-loan-rent.open-date:SCREEN-VALUE + CHR(1), 
                           "mBranch-id",
                           4) NO-ERROR.
      END.
      ELSE
      DO:

         IF mProDate THEN
            RUN browseld.p("rent-safe",
                           "mType" + CHR(1) 
                         + "mfilial-id" + CHR(1) 
                         + "mStat" + CHR(1) 
                         + "mTehStat" + CHR(1) 
                           + "mDate" + CHR(1)
                           + "mTmpRsrv",
                           tt-loan-cond.safe-type:SCREEN-VALUE + CHR(1) 
                         + tt-loan-cond.safe-filial + CHR(1) 
                         + "СВОБОДНА" + CHR(1) 
                         + "Исправна" + CHR(1) 
                           + STRING(DATE(tt-loan-rent.open-date:SCREEN-VALUE) + 1) + CHR(1), 
                           "mType",
                            4) NO-ERROR.
         ELSE
            RUN browseld.p("rent-safe",
                           "mType" + CHR(1) 
                         + "mfilial-id" + CHR(1) 
                         + "mStat" + CHR(1) 
                         + "mTehStat" + CHR(1) 
                         + "mDate" + CHR(1)
                         + "mTmpRsrv",
                           tt-loan-cond.safe-type:SCREEN-VALUE + CHR(1) 
                         + tt-loan-cond.safe-filial + CHR(1) 
                         + "СВОБОДНА" + CHR(1) 
                         + "Исправна" + CHR(1) 
                         + tt-loan-rent.open-date:SCREEN-VALUE + CHR(1), 
                           "mType",
                           4) NO-ERROR.
      END.

      IF      (LASTKEY EQ 10 OR LASTKEY EQ 13)
          AND pick-value NE ? THEN
      DO:

          FIND FIRST asset WHERE asset.filial-id EQ ENTRY(1, pick-value) 
                             AND asset.cont-type EQ ENTRY(2, pick-value)
            NO-LOCK NO-ERROR.

          IF AVAILABLE asset THEN
            ASSIGN
               SELF:SCREEN-VALUE = asset.cont-type
               tt-loan-cond.safe-filial            = asset.filial-id.
               tt-loan-cond.safe-type:SCREEN-VALUE = GetXAttrValueEx("asset",
                                                                     pick-value,
                                                                     "type",
                                                                     "")
            .
         APPLY "LEAVE" TO tt-loan-cond.safe-type.
      END.
   END.
   
   RUN CalcSumZalog.
   
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.safe-num TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.safe-num IN FRAME fMain /* Номер ячейки */
DO:
   DEFINE VARIABLE mContCode   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mBegDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE mEndDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE vOldSType   AS CHARACTER NO-UNDO.

  {&BEG_BT_LEAVE}
   IF NOT {assigned tt-loan-cond.safe-num:SCREEN-VALUE} THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Не выбрана ячейка !").
      RETURN NO-APPLY {&RET-ERROR}.
   END.
 
   FOR FIRST asset WHERE asset.filial-id EQ tt-loan-cond.safe-filial 
                     AND asset.cont-type EQ tt-loan-cond.safe-num:SCREEN-VALUE
      NO-LOCK:
 
      vOldSType = tt-loan-cond.safe-type:SCREEN-VALUE.
      IF GetFutureSafe(asset.cont-type, 
                       tt-loan-rent.cont-code, 
                       tt-loan-cond.since,
                       tt-loan-rent.end-date:INPUT-VALUE, 
                       asset.filial-id, 
                       OUTPUT mContCode, 
                       OUTPUT mBegDate, 
                       OUTPUT mEndDate) 
      THEN
      DO:
         RUN Fill-SysMes("","","0","Ячейка "          + asset.cont-type  + 
                                   " будет занята с " + STRING(mBegDate) + 
                                   " до "             + STRING(mEndDate) + 
                                   " по договору "    + mContCode).
         RETURN NO-APPLY {&RET-ERROR}.
      END.
 
      IF mSellSurr  NE asset.filial-id + "," + asset.cont-type THEN
      DO:

         IF GetXAttrValueEx("asset",
                          asset.filial-id + "," + asset.cont-type,
                          "TmpRsrv",
                          "-- пусто --") NE "-- пусто --" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("","","0","Ячейка "          + asset.cont-type  + 
                                      " зарезервирована ").
            RETURN NO-APPLY {&RET-ERROR}.
         END.

      tt-loan-cond.safe-type:SCREEN-VALUE = GetXAttrValueEx("asset",
                                                            asset.filial-id + "," + 
                                                            asset.cont-type,
                                                            "type",
                                                            "").

         UpdateSigns(asset.class-code,
                     asset.filial-id + "," + asset.cont-type,
                     "TmpRsrv",
                     STRING(gend-date, "99/99/9999"),
                     ?).

         mSellSurr = asset.filial-id + "," + asset.cont-type.
      END.
                  
      IF vOldSType NE tt-loan-cond.safe-type:SCREEN-VALUE THEN
      APPLY "LEAVE" TO tt-loan-cond.safe-type.
   END.    
 
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.safe-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.safe-type TERMINAL-SIMULATION
ON F1 OF tt-loan-cond.safe-type IN FRAME fMain /* Тип ячейки */
DO:
   pick-value = ?.

   DO TRANSACTION:
      RUN browseld.p("asset-rent",
                     "mSafe~001sc-1",
                     "*~001branch-id",
                     "",
                      4) NO-ERROR.

      IF     (LASTKEY EQ 10 OR LASTKEY EQ 13)
         AND pick-value NE ? THEN 
      DO:
         RUN CreateStateSafe(INPUT INPUT tt-loan-rent.open-date + IF mProDate THEN 1 ELSE 0).
         FIND FIRST asset WHERE asset.filial-id EQ ENTRY(1, pick-value) 
                            AND asset.cont-type EQ ENTRY(2, pick-value)
            NO-LOCK NO-ERROR.

         IF AVAILABLE asset THEN /*нашли asset с выбранным типом ячейки*/
         DO:
            SELF:SCREEN-VALUE        = asset.cont-type.
            tt-loan-cond.safe-filial = asset.filial-id.
            
            FOR EACH  bAsset WHERE bAsset.Class-Code EQ "rent-safe" 
                               AND bAsset.filial-id  EQ asset.filial-id
               NO-LOCK: /*находдим ассеты с ячейками*/
               mNumType = GetXAttrValue ("asset", 
                                         GetSurrogateBuffer("asset",(BUFFER bAsset:HANDLE)), 
                                         "type"). /*определяем тип проверяемой ячейки*/ 
                
               IF mNumType EQ asset.cont-type THEN   /*если тип проверяемой ячейки совпадает с выбранным типом ячейки, продолжим*/
               DO:
                  mStat    = GetStateSafe (bAsset.cont-type, 
                                           DATE(tt-loan-rent.open-date:SCREEN-VALUE) + IF mProDate THEN 1 ELSE 0, 
                                           bAsset.filial-id, 
                                           OUTPUT mContCode).
                  mTehStat = GetCode("ТехСост", 
                                     GetXAttrValue ("asset", 
                                                    GetSurrogateBuffer("asset",(BUFFER bAsset:HANDLE)), 
                                                    "TehSost")
                                    ).
                  
                  IF     mStat    EQ "СВОБОДНА"
                     AND mTehStat EQ "Исправна" /*если с ячейкой всё ок, значит показываем её на форме и закончим бежать по циклу*/
                     AND NOT GetFutureSafe(bAsset.cont-type, 
                                           tt-loan-rent.cont-code,
                                           tt-loan-cond.since, 
                                           tt-loan-rent.end-date:INPUT-VALUE,
                                           bAsset.filial-id, 
                                           OUTPUT mContCode, 
                                           OUTPUT mBegDate, 
                                           OUTPUT mEndDate) 
                     AND GetXAttrValueEx("asset",
                                         GetSurrogateBuffer("asset",(BUFFER bAsset:HANDLE)),
                                         "TmpRsrv",
                                         "-- пусто --") EQ "-- пусто --"
                  THEN
                  DO:
                     tt-loan-cond.safe-num:SCREEN-VALUE = bAsset.cont-type.
                     LEAVE.
                  END.     
               END.
            END. /*Закончили проверку типа ячейки и подходящей ячейки к этому типу*/   
        
            RUN CalcSumZalog.
         END.            
      END. 
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.safe-type TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.safe-type IN FRAME fMain /* Тип ячейки */
DO:
   {&BEG_BT_LEAVE}
   IF tt-loan-cond.safe-type NE tt-loan-cond.safe-type:SCREEN-VALUE OR 
      mFlag EQ YES THEN
   DO:
      IF  mIndSearch AND
         {assigned tt-loan-rent.end-date:SCREEN-VALUE } THEN
      DO:
         tt-loan-cond.tariff-type:SCREEN-VALUE = FTypTar(tt-loan-rent.branch-id:INPUT-VALUE,
                                                         tt-loan-cond.safe-type:SCREEN-VALUE,
                                                         tt-loan-rent.end-date:INPUT-VALUE - 
                                                         tt-loan-rent.open-date:INPUT-VALUE + 1 ).  
         RUN CalcTariff IN THIS-PROCEDURE.
      END.
      mNdsCm = GetXAttrValueEx("asset",
                           tt-loan-cond.safe-filial + "," +
                           tt-loan-cond.safe-type:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                           "nds_cm",
                           "").

      RUN GetNDSValue IN h_asset (mNdsCm,
                                  DATE(tt-loan-rent.open-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + IF mProDate THEN 1 ELSE 0,
                                  OUTPUT mNds).
      ASSIGN
         tt-loan-cond.tariff-type
         tt-loan-cond.safe-type
         tt-loan-cond.nds$:SCREEN-VALUE    = mNds
      .
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.sewifsrok$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.sewifsrok$ TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.sewifsrok$ IN FRAME fMain /* Срок аренды */
DO:
   {&BEG_BT_LEAVE}

   IF NOT {assigned tt-loan-cond.sewifsrok$:SCREEN-VALUE} THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("","","0","Не установлен срок аренды.").
      RETURN NO-APPLY {&RET-ERROR}.
   END.

   IF SELF:MODIFIED /* OR
      ( BT_Modify(tt-loan-rent.open-date:HANDLE) AND
        {assigned SELF:SCREEN-VALUE }) */ THEN
   DO:
      tt-loan-cond.tariff-type:SCREEN-VALUE = FEndDate(INPUT  INPUT FRAME {&FRAME-NAME} tt-loan-cond.sewifsrok$, 
                                                       INPUT  (INPUT FRAME {&FRAME-NAME} tt-loan-rent.open-date + IF mProDate THEN 1 ELSE 0),
                                                       OUTPUT mBegDate, 
                                                       OUTPUT mEndDate). 
      RUN CalcTariff IN THIS-PROCEDURE.

      IF NOT {assigned tt-loan-rent.end-date:SCREEN-VALUE} THEN
         tt-loan-rent.end-date:SCREEN-VALUE = STRING(mBegDate).
     
      IF    tt-loan-rent.end-date:INPUT-VALUE LT mBegDate
         OR tt-loan-rent.end-date:INPUT-VALUE GT mEndDate THEN 
      DO:
         RUN Fill-SysMes("","",4,"Установленная дата окончания не соответствует выбранному сроку аренды. Пересчитать дату ?").

         IF pick-value EQ "Yes" THEN
            tt-loan-rent.end-date:SCREEN-VALUE = STRING(mBegDate).
      END.

      SELF:MODIFIED = NO.
      APPLY "LEAVE" TO tt-loan-rent.end-date.
   END.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-loan-cond.tariff-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.tariff-type TERMINAL-SIMULATION
ON F1 OF tt-loan-cond.tariff-type IN FRAME fMain /* Тип тарифа */
DO:
    IF NOT {assigned tt-loan-cond.safe-type:SCREEN-VALUE IN FRAME {&FRAME-NAME}} THEN DO:
        RUN Fill-SysMes IN h_tmess ("",
                                    "",
                                    "0",
                                    "Не указан тип ячейки").
        RETURN NO-APPLY {&RET-ERROR}.
    END.
    ASSIGN
        pick-value = ?
        mKau       = "asset-rent" + "~006" + tt-loan-cond.safe-filial +
                     "," +
                     tt-loan-cond.safe-type:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    .

   DO TRANSACTION:

      IF     mPeriod = 0 
         AND tt-loan-cond.sewifsrok$:SCREEN-VALUE = "" THEN
         mPeriod = 9999999.
      RUN SetSysConf IN h_base ("comm-rate-kau", mKau).
      RUN browseld.p (
                      "commission",
                      "contract" + CHR(1) + "SetFirstFrm" + CHR(1) + "since2" + CHR(1) + "period2",
                      "asset-rent" + CHR(1) + "2" + CHR(1) + STRING(today) + CHR(1) + STRING(mPeriod),
                      "",
                      6).
      RUN DeleteOldDataProtocol IN h_base ("comm-rate-kau").
   
      IF     (LASTKEY EQ 10 OR LASTKEY EQ 13) 
         AND pick-value NE ? THEN 
      DO:
         FIND LAST comm-rate WHERE
             comm-rate.commission =  pick-value AND
             comm-rate.kau        =  mKau       AND
             comm-rate.since      <= INPUT FRAME {&FRAME-NAME} tt-loan-rent.open-date + IF mProDate THEN 1 ELSE 0
         NO-LOCK NO-ERROR.
         IF AVAILABLE comm-rate THEN
            ASSIGN
               SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + MIN(SELF:SCREEN-VALUE,",") + pick-value
            .
         ELSE
         DO:
            RUN Fill-SysMes IN h_tmess("","","0","ТАРИФ НЕ НАЙДЕН !").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END. 
   END.
   RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-loan-cond.tariff-type TERMINAL-SIMULATION
ON LEAVE OF tt-loan-cond.tariff-type IN FRAME fMain /* Тип тарифа */
DO:
   {&BEG_BT_LEAVE}
   RUN CalcTariff IN THIS-PROCEDURE.
   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF FRAME fMain ANYWHERE DO:

   FOR FIRST asset WHERE asset.filial-id EQ tt-loan-cond.safe-filial 
                     AND asset.cont-type EQ tt-loan-cond.safe-num:SCREEN-VALUE IN FRAME fMain
      NO-LOCK:      
      UpdateSigns(asset.class-code,
                  asset.filial-id + "," + asset.cont-type,
                  "TmpRsrv",
                  "",
                  ?).
   END.

   mRetVal = IF mOnlyForm THEN
      {&RET-ERROR}
      ELSE 
         "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

RUN StartBisTTY.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* Commented by KSV: Инициализация системных сообщений */
    RUN Init-SysMes("","","").

    /* Commented by KSV: Корректируем вертикальную позицию фрейма */
    iLevel = GetCorrectedLevel(FRAME fMain:HANDLE,iLevel).
    FRAME fMain:ROW = iLevel.

    /* Commented by KSV: Читаем данные */
    RUN GetObject.

    IF tt-loan-cond.int-offset EQ ? THEN
       tt-loan-cond.int-offset = "".

    IF tt-loan-cond.cred-offset EQ ? THEN
       tt-loan-cond.cred-offset = "".

    /* Заполняем COMBO-BOX'ы данными из метасхемы */
    RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

    /* Подсветка полей из LIST-5 (настроить для себя )*/
    RUN SetColorList(FRAME {&MAIN-FRAME}:HANDLE,
                     REPLACE("{&LIST-5}"," ",","),
                     "bright-green").

    /*предустановка агрегированных классов*/
    RUN LocalBeforEnable.

    /* Commented by KSV: Показываем экранную форму */
    STATUS DEFAULT "".



   RUN enable_UI.


      /* Commented by KSV: Открываем те поля, которые разрешено изменять
   ** в зависимости от режима открытия */
   RUN EnableDisable.

   /* Commented by KSV: Рисуем разделители. Разделители задаются как FILL-IN
   ** с идентификатором SEPARATOR# с атрибутом VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"1").




   IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
 END.

 /* Commented by KSV: Закрываем службу системных сообщений */
 RUN End-SysMes.

 RUN disable_ui.

 RUN EndBisTTY.

 /* Commented by KSV: Выгружаем библиотеки */
 {intrface.del}
 {flt-file.i}

 /* Commented by KSV: Возвращаем значение вызывающей процедуре */
 RETURN mRetVal.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcSumZalog TERMINAL-SIMULATION 
PROCEDURE CalcSumZalog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE mKey1 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mKey2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mSum1 AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE mSum2 AS DECIMAL   NO-UNDO.
    
   mKey1 = GetXAttrValueEx("asset",
                           tt-loan-cond.safe-filial + "," +
                           tt-loan-cond.safe-num:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                           "key1","").
   mKey2 = GetXAttrValueEx("asset",
                           tt-loan-cond.safe-filial + "," +
                           tt-loan-cond.safe-num:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                           "key2","").

   IF {assigned mKey1} THEN

      FOR FIRST code WHERE code.class EQ "ТипКлюч"
                       AND code.code  EQ mKey1 NO-LOCK:
          ASSIGN
             mSum1                                                     = DECIMAL(code.val)
             tt-loan-cond.currency:SCREEN-VALUE IN FRAME {&FRAME-NAME} = code.description[1].
       END.

   IF {assigned mKey2} THEN

      FOR FIRST code WHERE code.class EQ "ТипКлюч"
                       AND code.code  EQ mKey2 NO-LOCK:
         mSum2 = DEC(code.val).
      END.

   IF    INPUT tt-loan-cond.sum EQ ?
      OR INPUT tt-loan-cond.sum EQ 0 THEN
      ASSIGN 
         tt-loan-cond.SUM              = mSum1 + mSum2
         tt-loan-cond.sum:SCREEN-VALUE = STRING(mSum1 + mSum2)
      .

   IF INPUT tt-loan-cond.sum NE (mSum1 + mSum2) THEN 
   DO:
      RUN Fill-SysMes("","",4,"Установленная сумма залога не соответствует выбранной ячейке. Пересчитать сумму ?").

      IF pick-value EQ "Yes" THEN
         tt-loan-cond.sum:SCREEN-VALUE = STRING(mSum1 + mSum2).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcTariff TERMINAL-SIMULATION 
PROCEDURE CalcTariff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE vItem   AS INT64  NO-UNDO.
   DEFINE VARIABLE vTT  AS CHAR NO-UNDO.

   ASSIGN
      mTariff = ""
      vTT     = tt-loan-cond.tariff-type:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
      mPeriod = 0
   .
 
   IF {assigned vTT} THEN
   DO:

      DO vItem = 1 TO NUM-ENTRIES(vTT):
         FIND LAST comm-rate WHERE comm-rate.kau        EQ "asset-rent" 
                                                         + "~006" + tt-loan-cond.safe-filial 
                                                         + "," 
                                                         + tt-loan-cond.safe-type:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                               AND comm-rate.commission EQ ENTRY(vItem, vTT) 
                               AND comm-rate.since      LE INPUT FRAME {&FRAME-NAME} tt-loan-rent.open-date + IF mProDate THEN 1 ELSE 0 
            NO-LOCK  NO-ERROR.

         IF AVAILABLE comm-rate THEN 
         DO:
            mTariff              = mTariff 
                                 + MIN(mTariff, ",") 
                                 + TRIM(STRING(comm-rate.rate-comm, "zzzzzz9.99")).
            mTariff:SCREEN-VALUE = mTariff.
            
            IF comm-rate.period GT mPeriod THEN
               mPeriod = comm-rate.period.
         END.         
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CredIntAssgn TERMINAL-SIMULATION 
PROCEDURE CredIntAssgn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE vSpec AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vParP AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vParD AS CHARACTER NO-UNDO.

   vSpec = GetCode("ТипРасчАрд", STRING(tt-loan-cond.disch-type:SCREEN-VALUE IN FRAME fMain)).
   vParP = GetParamByNameAsChar(vSpec, "P", GetParamByNameAsChar(vSpec, "Р", "")).
   vParD = GetParamByNameAsChar(vSpec, "D", "").

   IF TRIM(tt-loan-cond.disch-type:SCREEN-VALUE IN FRAME fMain) EQ "2" THEN
   DO:
      tt-loan-cond.int-period:SCREEN-VALUE  IN FRAME fMain = ENTRY(1, vParP).

      IF NUM-ENTRIES(vParP) GT 1 THEN
         tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain = ENTRY(2, vParP).

/*
      IF NUM-ENTRIES(vParP) GT 2 THEN
         tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain = ENTRY(3, vParP).
*/

      IF NUM-ENTRIES(vParD) GT 1 AND TRIM(ENTRY(2, vParD)) NE "" THEN
         tt-loan-cond.int-date:SCREEN-VALUE IN FRAME fMain = ENTRY(2, vParD).
      ELSE
      DO:

         IF TRIM(ENTRY(1, vParD)) NE "" THEN
            tt-loan-cond.int-date:SCREEN-VALUE IN FRAME fMain = ENTRY(1, vParD).
      END.

      IF NOT {assigned tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain} THEN
         tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain = "Нс".

      IF NOT {assigned tt-loan-cond.int-period:SCREEN-VALUE IN FRAME fMain} THEN
         tt-loan-cond.int-period:SCREEN-VALUE  IN FRAME fMain = "М".

      IF NOT {assigned tt-loan-cond.int-date:SCREEN-VALUE IN FRAME fMain} THEN
         tt-loan-cond.int-date:SCREEN-VALUE    IN FRAME fMain = "31".
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI TERMINAL-SIMULATION  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI TERMINAL-SIMULATION  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY mBranchName mSumma CustName mNDays mTariff separator-3 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-loan-cond THEN 
    DISPLAY tt-loan-cond.safe-type tt-loan-cond.safe-num tt-loan-cond.safe-status 
          tt-loan-cond.sum tt-loan-cond.currency tt-loan-cond.sewifsrok$ 
          tt-loan-cond.tariff-type tt-loan-cond.disch-type tt-loan-cond.nds$ 
          tt-loan-cond.cred-period tt-loan-cond.cred-month 
          tt-loan-cond.cred-date tt-loan-cond.cred-offset 
          tt-loan-cond.int-period tt-loan-cond.int-month tt-loan-cond.int-date 
          tt-loan-cond.int-offset 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-loan-rent THEN 
    DISPLAY tt-loan-rent.branch-id tt-loan-rent.cust-cat tt-loan-rent.cust-id 
          tt-loan-rent.Branch-Rent tt-loan-rent.doc-ref tt-loan-rent.cont-type 
          tt-loan-rent.open-date tt-loan-rent.end-date tt-loan-rent.user-id 
          tt-loan-rent.loan-status tt-loan-rent.close-date 
          tt-loan-rent.datasogl$ tt-loan-rent.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-loan-rent.branch-id tt-loan-rent.cust-id tt-loan-rent.Branch-Rent 
         tt-loan-rent.doc-ref tt-loan-rent.cont-type tt-loan-rent.open-date 
         mNDays tt-loan-rent.end-date tt-loan-cond.safe-type 
         tt-loan-cond.safe-num tt-loan-cond.safe-status tt-loan-cond.sum 
         tt-loan-cond.sewifsrok$ tt-loan-cond.tariff-type 
         tt-loan-cond.disch-type tt-loan-cond.nds$ tt-loan-cond.cred-period 
         tt-loan-cond.cred-month tt-loan-cond.cred-date 
         tt-loan-cond.cred-offset tt-loan-cond.int-period 
         tt-loan-cond.int-month tt-loan-cond.int-date tt-loan-cond.int-offset 
         tt-loan-rent.user-id tt-loan-rent.loan-status tt-loan-rent.close-date 
         tt-loan-rent.datasogl$ tt-loan-rent.comment 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FGetSummRent TERMINAL-SIMULATION 
PROCEDURE FGetSummRent :
/*------------------------------------------------------------------------------
  Purpose: Расчет суммы аренды по основному условию договора    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


   DEFINE VARIABLE vSafeNum      AS CHARACTER    NO-UNDO. /* Номер ячейки */
   DEFINE VARIABLE vSafeType     AS CHARACTER    NO-UNDO. /* Тип ячейки */
   DEFINE VARIABLE vSafePeriod   AS CHARACTER    NO-UNDO. /* Срок договора аренды */
   DEFINE VARIABLE vTarifType    AS CHARACTER    NO-UNDO. /* Тип тарифа */
   DEFINE VARIABLE vMinValue     AS INT64        NO-UNDO. /* Период действия тарифа */
   DEFINE VARIABLE vCodeTarType  AS CHARACTER    NO-UNDO. /* Тип тарифа из классификатора */
   DEFINE VARIABLE vBasePeriod   AS INT64        NO-UNDO. /* Базовый рассчетный период (1-день, 2-месяц, 3-год) */
   DEFINE VARIABLE vKau          AS CHARACTER    NO-UNDO. /* Базовый рассчетный период */
   DEFINE VARIABLE vTarifList    AS CHARACTER    NO-UNDO. /* Список тарифов аренды*/
   DEFINE VARIABLE vTarifListOut AS CHARACTER    NO-UNDO. /* Список тарифов аренды*/
   DEFINE VARIABLE vTarif        AS DECIMAL      NO-UNDO. /* Тариф аренды*/
   DEFINE VARIABLE vTarifNDS     AS DECIMAL      NO-UNDO. /* Ставка НДС*/
   DEFINE VARIABLE vAmout        AS DECIMAL      NO-UNDO. /* Предварительная сумма */

   
   DEFINE VARIABLE vNDays        AS INT64        NO-UNDO.
   DEFINE VARIABLE vNDays1       AS INT64        NO-UNDO. /* Для сложных тарифов с декадой */
   DEFINE VARIABLE vNMonth       AS INT64        NO-UNDO.
   DEFINE VARIABLE vNMonth1      AS INT64        NO-UNDO. /* Для GoMonth */
   DEFINE VARIABLE vNYear        AS INT64        NO-UNDO.
   DEFINE VARIABLE vNQuarter     AS INT64        NO-UNDO.
   DEFINE VARIABLE vNHYear       AS INT64        NO-UNDO.
   DEFINE VARIABLE vNDecade      AS INT64        NO-UNDO.
   DEFINE VARIABLE vNDecade1     AS INT64        NO-UNDO. /* Для сложных тарифов с декадой */

   DEFINE VARIABLE vND AS INT64   NO-UNDO.

   DEFINE VARIABLE vBegDate AS DATE NO-UNDO.
   DEFINE VARIABLE vEndDate AS DATE NO-UNDO.

   DEFINE VARIABLE mAmout        AS DECIMAL     NO-UNDO. /* Сумма за аренду */
   DEFINE VARIABLE mAmoutNDS     AS DECIMAL     NO-UNDO. /* Сумма за аренду + НДС */
   DEFINE VARIABLE mNDS          AS DECIMAL     NO-UNDO. /* Сумма за аренду + НДС */

   IF tt-loan-cond.since EQ ? OR iMode EQ {&MOD_ADD} THEN
      tt-loan-cond.since = DATE(tt-loan-rent.open-date:SCREEN-VALUE IN FRAME fMain).

   vBegDate = DATE(tt-loan-cond.since) + IF mProDate THEN 1 ELSE 0.
   vEndDate = DATE(tt-loan-rent.END-date:SCREEN-VALUE IN FRAME fMain).

   IF vBegDate EQ ? AND tt-loan-cond.contract EQ "" THEN
      vBegDate = DATE(tt-loan-rent.open-date:SCREEN-VALUE IN FRAME fMain) + IF mProDate THEN 1 ELSE 0.

   mAmoutNDS = 0.
   mNDS      = 0.
   mAmout    = 0.

   /* vBegDate = vBegDate + 1. /* предложение реализовать как в кредитах */ */

/*
   vSafeType    = GetXAttrValue("loan-cond", 
                               loan-cond.contract + "," + 
                               loan-cond.cont-code + "," + 
                               STRING(loan-cond.since), 
                               "safe-type").

   vTarifType   = GetXAttrValue("loan-cond", 
                               loan-cond.contract + "," + 
                               loan-cond.cont-code + "," + 
                               STRING(loan-cond.since), 
                               "tariff-type").

   vTarifNDS    = dec(GetXAttrValue("loan-cond", 
                               loan-cond.contract + "," + 
                               loan-cond.cont-code + "," + 
                               STRING(loan-cond.since), 
                               "НДС")).

   vTarifList   = GetXAttrValue("loan-cond", 
                               loan-cond.contract + "," + 
                               loan-cond.cont-code + "," + 
                               STRING(loan-cond.since), 
                               "tariff").
*/
   vSafeType    = tt-loan-cond.safe-type:SCREEN-VALUE IN FRAME fMain.
   vTarifType   = tt-loan-cond.tariff-type:SCREEN-VALUE IN FRAME fMain.
   vTarifNDS    = DECIMAL(tt-loan-cond.nds$:SCREEN-VALUE IN FRAME fMain).
   vTarifList   = mTariff:SCREEN-VALUE IN FRAME fMain.
/*
   vTarifList   = tt-loan-cond.tariff:SCREEN-VALUE IN FRAME fMain.
MESSAGE
"vSafeType    = " vSafeType    SKIP 
"vTarifType   = " vTarifType   SKIP 
"vTarifNDS    = " vTarifNDS    SKIP 
"vTarifList   = " vTarifList   SKIP 
VIEW-AS ALERT-BOX.
*/

   /* находим кау */
   vKau = 'asset-rent' + CHR(6) + shfilial + "," + vSafeType.
   
   IF NUM-ENTRIES(vTarifList) NE NUM-ENTRIES(vTarifType) THEN
   DO:
      /* RUN Fill-SysMes("","","0","Список значений тарифов '" + vTarifList + "' не соответствует списку типов тарифов '" + vTarifType + "'"). */
      vTarif        = 0.00.
      vTarifList    = "".
      vTarifListOut = "".
   END.

   DO vND = 1 TO NUM-ENTRIES(vTarifType):

      IF vTarifList NE "" THEN
      DO:
          ASSIGN 
              vTarif = dec(entry(vND,vTarifList))
          NO-ERROR.

          IF ERROR-STATUS:error THEN
          DO:
             RUN Fill-SysMes("","","0","Неверное значение '" + entry(vND,vTarifList) + "' для тарифа '" + entry(vND,vTarifType) + "'").
             RETURN.
          END.
      END.

      FIND LAST comm-rate WHERE comm-rate.since      LE tt-loan-cond.since 
                            AND comm-rate.kau        EQ vKau
                            AND (comm-rate.rate-comm EQ vTarif OR vTarifList EQ "")
                            AND comm-rate.commission EQ ENTRY(vND,vTarifType)
         NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE comm-rate THEN
      DO:
         RUN Fill-SysMes("","","0","Тариф с датой <= " + string(tt-loan-rent.open-date) + " и комиссией " + string(entry(vND,vTarifType)) + " не найден для типа ячейки: " + vSafeType).
         RETURN.
      END.

      vTarifListOut = vTarifListOut + MIN(vTarifListOut, ",") + TRIM(STRING(comm-rate.rate-comm, "zzzzzz9.99")).
      vTarif        = comm-rate.rate-comm.
      vBasePeriod   = comm-rate.period.
      vCodeTarType  = GetCode("РасчПериод",STRING(comm-rate.period)).
      vMinValue     = comm-rate.min-value.
      
      IF vBasePeriod EQ 0 THEN
         mAmout = vTarif.
/*         
MESSAGE vBegDate SKIP
vNMonth1
VIEW-AS ALERT-BOX.          
*/
      
      vBegDate = GoMonth(vBegDate,vNMonth1).

      RUN DMY_In_Per(vBegDate, 
                     vEndDate,
                     OUTPUT vNDays,
                     OUTPUT vNMonth,
                     OUTPUT vNYear).
      vNMonth1 = 0.
/*
MESSAGE vCodeTarType VIEW-AS ALERT-BOX. 
*/
      
      CASE vCodeTarType:
         WHEN "день" THEN
         DO:

             IF vEndDate EQ vBegDate THEN
                vAmout = 0.
             ELSE

                IF vND EQ NUM-ENTRIES(vTarifType) THEN
                DO:
/*
MESSAGE vEndDate SKIP
vBegDate SKIP
vNDecade SKIP
vNDays1 SKIP
vTarif
VIEW-AS ALERT-BOX.                 
*/
                   vAmout = ((vEndDate - vBegDate + 1) - (vNDecade * 10) - vNDays1) * vTarif.
                END.
                ELSE
                DO:
                   IF vMinValue > 0 AND (vEndDate - vBegDate) >= vMinValue THEN
                   DO:
                      vNDays1 = vMinValue.
                      vAmout  = (vNDays1 - (vNDecade * 10)) * vTarif.
                   END.
                   ELSE
                     vAmout = ((vEndDate - vBegDate + 1) - (vNDecade * 10)) * vTarif.
                END.
         END.
         WHEN "месяц" THEN
         DO:

            IF vND EQ NUM-ENTRIES(vTarifType) THEN
            DO:
               IF vNYear GT 0 THEN
                  vNMonth = vNMonth + vNYear * 12.

               IF vNDays GT 0 THEN
                  vNMonth = vNMonth + 1.
            END.
            ELSE
            DO:

               IF vNYear GT 0 THEN
                  vNMonth = vNMonth + vNYear * 12.

               IF vMinValue GT 0 AND vNMonth GE vMinValue THEN
                  vNMonth = vMinValue.
            END.

            IF vNMonth EQ 0 THEN
               vAmout = 0.
            ELSE
               vAmout = vNMonth * vTarif.
            vNMonth1 = vNMonth.
         END.
         WHEN "год" THEN
         DO:

            IF vND EQ NUM-ENTRIES(vTarifType) THEN
            DO:
               IF vNDays GT 0 OR vNMonth GT 0 THEN
                  vNYear = vNYear + 1.
            END.
            ELSE IF vMinValue GT 0 AND vNYear GE vMinValue THEN
               vNYear = vMinValue.

            IF vNYear EQ 0 THEN
               vAmout = 0.
            ELSE
               vAmout = vNYear * vTarif.
            vNMonth1 = vNYear * 12.
      
         END.
         WHEN "квартал" THEN
         DO:

            IF vNYear GT 0 THEN
               vNQuarter = vNYear * 4.

            IF vNMonth GT 0 OR vNDays GT 0 THEN
            DO:

               IF vND EQ NUM-ENTRIES(vTarifType) THEN
               DO:
                  IF CAN-DO("3,6,9,12", STRING(vNMonth)) THEN
                  DO:
                     vNQuarter = vNQuarter + (vNMonth / 3).

                     IF vNDays GT 0 THEN
                        vNQuarter = vNQuarter + 1.
                  END.
                  ELSE 
                     vNQuarter = vNQuarter + TRUNC((vNMonth / 3),0) + 1.
               END.
               ELSE
               DO:
                  vNQuarter = vNQuarter + TRUNC((vNMonth / 3),0).

                  IF vMinValue GT 0 AND vNQuarter GE vMinValue THEN
                     vNQuarter = vMinValue.
               END.
            END.

            IF vNQuarter EQ 0 THEN
               vAmout = 0.
            ELSE
               vAmout = vNQuarter * vTarif.

            vNMonth1 = vNQuarter * 3.
         END.
         WHEN "полугодие" THEN
         DO:

            IF vND EQ NUM-ENTRIES(vTarifType) THEN
            DO:

               IF vNYear GT 0 THEN
                  vNHYear = vNYear * 2.

               IF vNMonth GT 0 OR vNDays GT 0 THEN
               DO:

                  IF vNMonth EQ 6 THEN
                  DO:
                     IF vNDays GT 0 THEN
                        vNHYear = vNHYear + 2.
                     ELSE
                        vNHYear = vNHYear + 1.
                  END.
                  ELSE 
                      vNHYear = vNHYear + TRUNC((vNMonth / 6),0) + 1.
               END.
            END.
            ELSE
            DO:
               IF vNYear GT 0 THEN
                  vNHYear = vNYear * 2.

               IF vNMonth GE 6 THEN
                  vNHYear = vNHYear + 1.
                     
               IF vMinValue GT 0 AND vNHYear GE vMinValue THEN
                  vNHYear = vMinValue.
            END.

            IF vNHYear EQ 0 THEN
               vAmout = 0.
            ELSE
               vAmout = vNHYear * vTarif.

            vNMonth1 = vNHYear * 6.
         END.
         WHEN "декада" THEN
         DO:

            IF vND EQ NUM-ENTRIES(vTarifType) THEN
            DO:
               IF ((vEndDate - vBegDate + 1) MODULO 10) GT 0 THEN
                  vNDecade = TRUNC(((vEndDate - vBegDate) / 10),0) + 1 - vNDecade.
               ELSE
                  vNDecade = ((vEndDate - vBegDate + 1) / 10) - vNDecade.
            END.
            ELSE
            DO:
                vNDecade = TRUNC(((vEndDate - vBegDate + 1) / 10),0).

                IF vMinValue GT 0 AND vNDecade GE vMinValue THEN
                  vNDecade = vMinValue.
            END.

            IF vNDecade EQ 0 THEN
               vAmout = 0.
            ELSE
               vAmout = vNDecade * vTarif.
         END.
      END CASE.
/*      
MESSAGE "mAmout = " mAmout VIEW-AS ALERT-BOX. 
*/
      mAmout = mAmout + vAmout.
   END. /* DO vND = 1 TO... */
/*
   IF {assigned vTarifListOut} THEN
      UpdateSigns( "rent-cond",  
                   loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),  
                   "tariff", trim(vTarifListOut),
                   ? ).
*/
   /* Если настроечный параметр ТарифНДС = YES то,сумма оплаты будет равна сумме арендной платы */
   IF FGetSetting("ТарифНДС","",?) EQ "NO" THEN 
   DO:
      mAmoutNDS = mAmout + (mAmout * vTarifNDS) / 100.
      mNDS      = (mAmout * vTarifNDS) / 100.
   END.
   ELSE
   DO:
      mAmoutNDS = mAmout.
      mNDS      = mAmout * vTarifNDS /(vTarifNDS + 100).
      mAmout    = mAmout - mNDS.
   END.
   mSumma:SCREEN-VALUE IN FRAME fMain = STRING(mAmoutNDS).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalBeforEnable TERMINAL-SIMULATION 
PROCEDURE LocalBeforEnable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF tt-loan-cond.nds$ EQ ? THEN
   tt-loan-cond.nds$ = 0.

IF tt-loan-cond.currency EQ ? THEN
   tt-loan-cond.currency = "".
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalEnableDisable TERMINAL-SIMULATION 
PROCEDURE LocalEnableDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE vErrMess AS CHARACTER NO-UNDO.

   tt-loan-cond.sum:HIDDEN         IN FRAME fMain = NO.
   tt-loan-cond.currency:HIDDEN    IN FRAME fMain = NO.
   tt-loan-cond.safe-type:HIDDEN   IN FRAME fMain = NO.
   tt-loan-cond.safe-num:HIDDEN    IN FRAME fMain = NO.
   tt-loan-cond.safe-type:HIDDEN   IN FRAME fMain = NO.
   tt-loan-cond.safe-status:HIDDEN IN FRAME fMain = NO.
   tt-loan-cond.sewifsrok$:HIDDEN  IN FRAME fMain = mIndSearch. 
   tt-loan-cond.tariff-type:HIDDEN IN FRAME fMain = NO.
   tt-loan-cond.nds$:HIDDEN        IN FRAME fMain = NO.
   tt-loan-cond.cred-period:HIDDEN IN FRAME fMain = NO.
   tt-loan-cond.cred-month:HIDDEN  IN FRAME fMain = NO.
   tt-loan-cond.cred-date:HIDDEN   IN FRAME fMain = NO.
   tt-loan-cond.int-period:HIDDEN  IN FRAME fMain = NO.
   tt-loan-cond.int-month:HIDDEN   IN FRAME fMain = NO.
   tt-loan-cond.int-date:HIDDEN    IN FRAME fMain = NO.


   IF iMode EQ {&MOD_ADD} THEN
   DO:
      /* Выставляем начальные значения */
      tt-loan-rent.class-code       = "loan-rent".
      tt-loan-rent.contract         = "АРЕНДА".
      tt-loan-rent.loan-status      = FGetSetting("СтатДогОткр",?,"ВВЕД").
      tt-loan-cond.safe-status = "АРЕНДОВАНА". 

      tt-loan-rent.contract:SCREEN-VALUE IN FRAME fMain         = "АРЕНДА".
      tt-loan-rent.loan-status:SCREEN-VALUE IN FRAME fMain      = FGetSetting("СтатДогОткр",?,"ВВЕД").
      tt-loan-cond.safe-status:SCREEN-VALUE IN FRAME fMain = "АРЕНДОВАНА".

      tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain = "Нс".

      tt-loan-cond.disch-type:SCREEN-VALUE IN FRAME fMain = GetXattrInit(tt-loan-cond.class-code, "disch-type").
      tt-loan-rent.cont-type:SCREEN-VALUE  IN FRAME fMain = GetXattrInit(tt-loan-rent.class-code, "cont-type").

      tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain = "".
      tt-loan-cond.int-period:SCREEN-VALUE  IN FRAME fMain = "".
/*      tt-loan-cond.int-date:SCREEN-VALUE    IN FRAME fMain = "".*/
      tt-loan-cond.int-date:SCREEN-VALUE IN FRAME fMain = GetXattrInit(tt-loan-cond.class-code, "int-date").

      RUN CredIntAssgn.

      /* если в фильтре не *, то значение фильтра подставляем в форму */
      IF GetFltVal("mSafeType") NE "*" THEN
      DO:
         tt-loan-cond.safe-type:SCREEN-VALUE = GetFltVal("mSafeType").

         IF GetFltVal("mSafeNum") NE "*" THEN
         DO:
            FIND FIRST asset WHERE asset.class-code EQ "rent-safe" 
                               AND asset.cont-type  EQ GetFltVal("mSafeNum")
               NO-LOCK NO-ERROR.

            IF AVAILABLE asset THEN     /*нашли asset с выбранным типом ячейки*/
            DO:
               mStat    = GetStateSafe (asset.cont-type, 
                                        gend-date + IF mProDate THEN 1 ELSE 0, 
                                        asset.filial-id, 
                                        OUTPUT mContCode).
               mTehStat = GetCode("ТехСост", 
                                  GetXAttrValue ("asset", 
                                                 GetSurrogateBuffer("asset",
                                                                    (BUFFER asset:HANDLE)
                                                ), 
                                                                    "TehSost")
                                 ).

               IF     mStat    EQ "СВОБОДНА" 
                  AND mTehStat EQ "Исправна" THEN   /*если с ячейкой всё ок, значит показываем её на форме и закончим бежать по циклу*/
               DO:
                  tt-loan-cond.safe-filial           = asset.filial-id.
                  tt-loan-cond.safe-num:SCREEN-VALUE = GetFltVal("mSafeNum").
               END.
               ELSE
               DO:

                  IF mStat NE "СВОБОДНА" THEN
                     RUN Fill-SysMes("","","0","Ячейку номер " + STRING(GetFltVal("mSafeNum")) + " нельзя арендовать т.к. она " + mStat).

                  IF mTehStat NE "Исправна" THEN
                     RUN Fill-SysMes("","","0","Ячейку номер " + STRING(GetFltVal("mSafeNum")) + " нельзя арендовать т.к. она " + mTehStat).
                  tt-loan-cond.safe-filial = asset.filial-id.
                  mFlag = YES.
               END.
            END.
         END.
         ELSE
         DO:
            FIND FIRST asset WHERE asset.class-code EQ "asset-rent" 
                               AND asset.cont-type  EQ tt-loan-cond.safe-type:SCREEN-VALUE
               NO-LOCK NO-ERROR.

            IF AVAILABLE asset THEN      /*нашли asset с выбранным типом ячейки*/
            DO:
               /*SELF:SCREEN-VALUE        = asset.cont-type.*/
               tt-loan-cond.safe-filial = asset.filial-id.

               FOR EACH bAsset WHERE bAsset.Class-Code = "rent-safe" NO-LOCK: /*находдим ассеты с ячейками*/

                  IF asset.cont-type EQ GetXAttrValue ("asset", 
                                                       GetSurrogateBuffer("asset",
                                                                          (BUFFER bAsset:HANDLE)
                                                                         ), 
                                                       "type") THEN   /*если тип проверяемой ячейки совпадает с выбранным типом ячейки, продолжим*/
                  DO:
                     mStat    = GetStateSafe (bAsset.cont-type, 
                                              gend-date + IF mProDate THEN 1 ELSE 0, 
                                              bAsset.filial-id, 
                                              OUTPUT mContCode).
                     mTehStat = GetCode("ТехСост", 
                                        GetXAttrValue ("asset", 
                                                       GetSurrogateBuffer("asset",
                                                                          (BUFFER asset:HANDLE)
                                                                         ), "TehSost")
                                       ).         
                     
                     IF     mStat    EQ "СВОБОДНА" 
                        AND mTehStat EQ "Исправна" THEN   /*если с ячейкой всё ок, значит показываем её на форме и закончим бежать по циклу*/
                     DO:

                        IF NOT GetFutureSafe(bAsset.cont-type, 
                                             tt-loan-rent.cont-code,
                                             tt-loan-cond.since, 
                                             tt-loan-rent.end-date:INPUT-VALUE,
                                             bAsset.filial-id, 
                                             OUTPUT mContCode, 
                                             OUTPUT mBegDate, 
                                             OUTPUT mEndDate) 
                           THEN
                        DO:
                           tt-loan-cond.safe-num:SCREEN-VALUE = bAsset.cont-type.
                           LEAVE.
                        END.     
                     END.    
                  END.
               END.
            END. /*IF AVAIL asset THEN */  
         END. /*IF GetFltVal("mSafeNum") NE "*" THEN DO:*/
         RUN CalcSumZalog.
      END. /*GetFltVal("mSafeType") NE "*" THEN DO:*/
   END.

   IF iMode EQ {&MOD_EDIT} THEN
   DO:

      IF tt-loan-rent.close-date NE ? THEN
      DO:
         RUN BT_HiddOrDisableField(tt-loan-rent.Branch-rent:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.branch-id:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.cust-cat:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.cust-id:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.doc-ref:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.end-date:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.user-id:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.loan-status:HANDLE,NO,YES).
         RUN BT_HiddOrDisableField(tt-loan-rent.datasogl$:HANDLE,NO,YES).
      END.
      ELSE

         IF GetXAttrValueEx("loan",tt-loan-rent.contract + "," + tt-loan-rent.cont-code,"Конверт","") NE "ДА"
         THEN
         DO:
            RUN BT_HiddOrDisableField(tt-loan-rent.cust-cat:HANDLE,NO,YES).
            RUN BT_HiddOrDisableField(tt-loan-rent.cust-id:HANDLE,NO,YES).
         END.
      mLStatus  = INPUT tt-loan-cond.safe-status.

      RUN IsAvailChangeLoanDate(tt-loan-rent.Contract,
                                tt-loan-rent.Cont-Code,
                                OUTPUT vErrMess).

      IF {assigned vErrMess} THEN
         tt-loan-rent.open-date:SENSITIVE IN FRAME fMain = NO.
      ELSE
         tt-loan-rent.open-date:SENSITIVE IN FRAME fMain = YES.

   END.

   IF    iMode EQ {&MOD_EDIT} 
      OR iMode EQ {&MOD_VIEW} THEN
   DO:

      FOR LAST  bloan-cond NO-LOCK WHERE bloan-cond.contract  EQ tt-loan-rent.contract
                                     AND bloan-cond.cont-code EQ tt-loan-rent.cont-code :
         mSurrCond = GetSurrogateBuffer("loan-cond",
                                        (BUFFER bloan-cond:HANDLE)).
         tt-loan-cond.safe-filial = GetXAttrValueEx("loan-cond", 
                                                    mSurrCond, 
                                                    "safe-filial",
                                                    "").
      END.
      RUN CalcTariff.
      mLEndDate = INPUT tt-loan-rent.end-date.
      mLOpeDate = INPUT tt-loan-rent.open-date.

      IF iMode EQ {&MOD_EDIT} THEN
         mNDaysOld = INT64  (INPUT mNDays).
   END.

   IF tt-loan-rent.cust-cat:SCREEN-VALUE IN FRAME fMain EQ "В" THEN
   DO:
      tt-loan-rent.cust-id:HIDDEN IN FRAME fMain     = YES.
      tt-loan-rent.Branch-Rent:HIDDEN IN FRAME fMain = NO.
   END.
   ELSE
   DO:
      tt-loan-rent.cust-id:HIDDEN IN FRAME fMain     = NO.
      tt-loan-rent.Branch-Rent:HIDDEN IN FRAME fMain = YES.
   END.
          
   CASE tt-loan-cond.cred-period:SCREEN-VALUE IN FRAME fMain:
      WHEN "НС" THEN
         ASSIGN
            tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "" THEN
         ASSIGN
            tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "М"  THEN
         ASSIGN
            tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
         .

      WHEN "П" THEN
         ASSIGN
            tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "Кс" THEN
         ASSIGN
            tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE
         .

      WHEN "Д"  THEN
         ASSIGN
            tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = TRUE
         .

      OTHERWISE
         ASSIGN
            tt-loan-cond.cred-month:HIDDEN IN FRAME fMain = FALSE
            tt-loan-cond.cred-date:HIDDEN  IN FRAME fMain = FALSE
         .
   END CASE.

   CASE tt-loan-cond.int-period:SCREEN-VALUE IN FRAME fMain:
      WHEN "НС" THEN
         ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
      .

      WHEN "" THEN
      ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
      .

      WHEN "М"  THEN
      ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
      .

      WHEN "П" THEN
      ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
      .

      WHEN "Кс" THEN
      ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
      .

      WHEN "Д"  THEN
      ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = TRUE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = TRUE
      .

      OTHERWISE
         ASSIGN
            tt-loan-cond.int-month:HIDDEN IN FRAME fMain = FALSE
            tt-loan-cond.int-date:HIDDEN  IN FRAME fMain = FALSE
      .
   END CASE.
   RUN FGetSummRent.
   RUN BT_InitHiddOrDisableFieldsAlways.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalGetObject TERMINAL-SIMULATION 
PROCEDURE LocalGetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalSetObject TERMINAL-SIMULATION 
PROCEDURE LocalSetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE vRest   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vP      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vD      AS CHARACTER  NO-UNDO.
   /* Получить значение классификатора "ТипРасчАрд" по коду tt-loan-cond.ditsch-type  */

   vRest = GetCode("ТипРасчАрд", STRING(tt-loan-cond.disch-type)).

   IF {assigned vRest} THEN
   DO:
      /*
         Проверить, что заданный период погашения для графиков (поля tt-loan-cond.cred-period,
         tt-loan-cond.int-period, tt-loan-cond.int-date )  соответствуют списку значений
         параметра  "Р" из настройки vRest
      */

      vP = GetParamByNameAsChar(vRest, "P", GetParamByNameAsChar(vRest, "Р", "")).
      vD = GetParamByNameAsChar(vRest, "D", "").

      IF    NOT CAN-DO(vP, tt-loan-cond.cred-period)
         OR NOT CAN-DO(vP, tt-loan-cond.int-period)   THEN
      DO:
         APPLY 'ENTRY' TO tt-loan-cond.cred-period IN FRAME fMain.
         RETURN ERROR "Период расчета графиков не соответствует выбранной форме расчета".
      END.
/*
      IF tt-loan-cond.int-period EQ "М" AND tt-loan-cond.int-date LT DAY(tt-loan-rent.open-date) THEN
      DO:
         APPLY 'ENTRY' TO tt-loan-cond.int-period IN FRAME fMain.
         RETURN ERROR "Период расчета графиков не соответствует выбранной форме расчета".
      END.
*/
   END.

   IF (   iMode EQ {&MOD_ADD}
       OR iMode EQ {&MOD_EDIT} 
      )
      AND GetFutureSafe(tt-loan-cond.safe-num:INPUT-VALUE IN FRAME {&FRAME-NAME}, 
                        tt-loan-cond.cont-code, 
                        tt-loan-cond.since,
                        tt-loan-rent.end-date:INPUT-VALUE IN FRAME {&FRAME-NAME}, 
                        tt-loan-cond.safe-filial, 
                        OUTPUT mContCode, 
                        OUTPUT mBegDate, 
                        OUTPUT mEndDate) 
   THEN
   DO:
      RUN Fill-SysMes("","","0","Ячейка "          + tt-loan-cond.safe-num:INPUT-VALUE IN FRAME {&FRAME-NAME}  + 
                                " будет занята с " + STRING(mBegDate) + 
                                " до "             + STRING(mEndDate) + 
                                " по договору "    + mContCode).
      APPLY 'ENTRY' TO tt-loan-cond.safe-num IN FRAME {&FRAME-NAME}.
      RETURN ERROR.
   END.   
   
   IF tt-loan-rent.end-date LT tt-loan-rent.open-date + 
                               ( IF mProDate THEN 1 ELSE 0 ) THEN
   DO:
      mRetVal = {&RET-ERROR}.
      APPLY "entry" TO tt-loan-rent.end-date IN FRAME {&FRAME-NAME}.
       RETURN ERROR "Дата окончания " +
                    ( IF mProDate 
                      THEN "должна быть больше" 
                      ELSE "не должна быть меньше" ) +
                    " даты начала".
   END.

   IF tt-loan-cond.contract EQ "" THEN
      ASSIGN
         tt-loan-cond.contract  = tt-loan-rent.contract
         tt-loan-cond.cont-code = tt-loan-rent.cont-code
         tt-loan-cond.since     = INPUT FRAME {&FRAME-NAME} tt-loan-rent.open-date 
      .
   
   ASSIGN
      tt-loan-cond.nds$ = DEC(tt-loan-cond.nds$:SCREEN-VALUE IN FRAME {&FRAME-NAME})
   .
   UpdateSigns(tt-loan-rent.class-code,
               tt-loan-rent.contract + "," + tt-loan-rent.cont-code,
               "NDays",
               mNDays,
               ?).
   
   
   SetFormDefList(GetFormDefList() + ",tt-loan-cond.safe-filial").
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LOCAL_GO TERMINAL-SIMULATION 
PROCEDURE LOCAL_GO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF tt-loan-rent.open-date:INPUT-VALUE IN FRAME {&FRAME-NAME} LT tt-loan-rent.datasogl$:INPUT-VALUE IN FRAME {&FRAME-NAME} THEN
   DO:
      RUN Fill-SysMes("","","0","Дата открытия должна быть больше или равна даты заключения договора").
      RETURN ERROR.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF    iMode EQ {&MOD_EDIT}
      OR iMode EQ {&MOD_VIEW} THEN
   DO:
      FIND LAST tt-loan-cond WHERE tt-loan-cond.since LE gend-date
         NO-LOCK NO-ERROR.

     IF NOT AVAILABLE tt-loan-cond THEN
        FIND LAST tt-loan-cond NO-LOCK NO-ERROR.

      mNDays = GetXAttrValueEx("loan",
                               tt-loan-rent.contract + "," + tt-loan-rent.cont-code,
                               "NDays",
                               "").
   END.

   mSellSurr = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostSetObject TERMINAL-SIMULATION 
PROCEDURE PostSetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {f-safe1.i}  

   DEFINE VARIABLE vCloseRoles  AS CHARACTER   NO-UNDO INITIAL "".
   DEFINE VARIABLE vClsAcctLst  AS CHARACTER   NO-UNDO INITIAL "".
   DEFINE VARIABLE vAttr        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vError       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOk          AS INT64       NO-UNDO.
   DEFINE VARIABLE vI           AS INT64       NO-UNDO.

   DEFINE BUFFER loan-acct FOR loan-acct.
   DEFINE BUFFER acct      FOR acct.
   DEFINE BUFFER xloan     FOR loan.
   DEFINE BUFFER asset     FOR asset.

   /* при установке статуса ЗАКР запрашиваем дату закрытия счетов и проверяем
      возможность их закрытия */
   IF tt-loan-rent.loan-status EQ "ЗАКР" THEN
   DO:
      CLOSE_LOAN_TR:
      DO
         ON ERROR  UNDO, LEAVE
         ON ENDKEY UNDO, LEAVE:
         FOR EACH xloan WHERE xloan.parent-contract  EQ tt-loan-rent.contract
                          AND xloan.parent-cont-code EQ tt-loan-rent.cont-code
                          AND xloan.close-date       EQ ?
                          AND xloan.Class-Code       EQ "loan-rent-dop"
            EXCLUSIVE-LOCK:
            xloan.close-date = tt-loan-rent.close-date NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
               vError = "Нельзя закрыть договор, связанный с классом " 
                      + xloan.Class-Code + "!".
               UNDO CLOSE_LOAN_TR, LEAVE CLOSE_LOAN_TR.
            END.
         END.
      END.
      IF {assigned vError} THEN
         RETURN ERROR vError. 

      vCloseRoles = FGetSetting("РольЗакрСч", ?, "").
      DO vI = 1 TO NUM-ENTRIES(vCloseRoles):
         /* сначала проверим наличие такого счета в картотеке и что он открыт */
         FOR EACH loan-acct WHERE loan-acct.contract  EQ tt-loan-rent.contract
                              AND loan-acct.cont-code EQ tt-loan-rent.cont-code
                              AND loan-acct.acct-type EQ ENTRY(vI, vCloseRoles)
         NO-LOCK BY loan-acct.since DESCENDING:
            LEAVE.
         END.
         IF AVAILABLE loan-acct AND
            CAN-FIND(FIRST acct WHERE acct.acct       EQ loan-acct.acct
                                  AND acct.currency   EQ loan-acct.currency
                                  AND acct.close-date EQ ?
                     )
         THEN DO:
            /* счет привязан и открыт, сохраняем его */
            vAcct = loan-acct.acct + ";" + loan-acct.currency.
            {additem.i vClsAcctLst vAcct}
         END.
      END.
      /* если есть, что закрывать - спрашиваем дату закрытия и проверяем
         возможность закрытия этой датой */
      vOk = 0.
      IF vClsAcctLst NE "" THEN
      DO:
         {getdate.i
            &DateLabel = "Закрыть счета"
            &DateHelp  = "Введите дату закрытия счетов (F1- календарь)"
            &RETURN    = "RETURN ERROR 'Договор не закрыт'"
         }
         CLOSE_ACCTS_TR:
         DO
         ON ERROR  UNDO, LEAVE
         ON ENDKEY UNDO, LEAVE:
            DO vI = 1 TO NUM-ENTRIES(vClsAcctLst):
               FIND FIRST acct WHERE
                      acct.acct     EQ ENTRY(1, ENTRY(vI, vClsAcctlst), ";")
                  AND acct.currency EQ ENTRY(2, ENTRY(vI, vClsAcctlst), ";")
               EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
               IF AVAILABLE acct THEN
               DO:
                  acct.close-date = end-date.
                  RUN Check-Acct IN h_acct (BUFFER acct, OUTPUT vAttr, OUTPUT vOk).
                  IF vOk NE 0 THEN
                     UNDO CLOSE_ACCTS_TR, LEAVE CLOSE_ACCTS_TR.
                  RELEASE acct.
               END.
               ELSE
                  vOk = -1.
            END.
         END.
      END.
      IF vOk NE 0 THEN
         RETURN ERROR "Невозможно закрыть счета. Договор не закрыт!".
   END.

   FOR FIRST asset WHERE asset.filial-id EQ tt-loan-cond.safe-filial 
                     AND asset.cont-type EQ tt-loan-cond.safe-num
      NO-LOCK:
      UpdateSigns(asset.class-code,
                  asset.filial-id + "," + asset.cont-type,
                  "TmpRsrv",
                  "",
                  ?).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FEndDate TERMINAL-SIMULATION 
FUNCTION FEndDate RETURNS CHARACTER
  ( INPUT  iCode AS CHAR,
    INPUT  iDate AS DATE,
    OUTPUT oBegDate AS Date, 
    OUTPUT oEndDate AS Date ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ "СейфСроки2"
                     AND code.code  EQ iCode
      NO-LOCK NO-ERROR.
   
   IF AVAILABLE code THEN
   DO:
   
      IF iDate EQ ? THEN
         iDate = TODAY.
   
      CASE code.code :
         WHEN "День"  THEN
            ASSIGN
               oBegDate = iDate + 1
               oEndDate = iDate + 1
            .
         WHEN "Месяц" THEN
            ASSIGN
               oBegDate = iDate + 30
               oEndDate = iDate + 30
            .
         WHEN "Год"   THEN
            ASSIGN
               oBegDate = iDate + 365
               oEndDate = iDate + 365
            .
         OTHERWISE 
            DO:
   
               IF     INDEX(code.code, "Д")       NE 0
                  AND NUM-ENTRIES(code.code, "-") GT 1 THEN 
                  ASSIGN 
                    oBegDate = iDate + INT64  (REPLACE(ENTRY(1,code.code,"-"),",",".")) - 1
                    oEndDate = iDate + INT64  (REPLACE(SUBSTRING(ENTRY(2,code.code,"-"),
                                                                 1,
                                                                 INDEX(ENTRY(2,code.code,"-"), "Д") - 1
                                                                ),",",".")) - 1
                  .
               ELSE
   
                  IF     INDEX(code.code, "М")       NE 0
                     AND NUM-ENTRIES(code.code, "-") GT 1 THEN 
                     ASSIGN
                       oBegDate = iDate + ROUND(DECIMAL(REPLACE(ENTRY(1,code.code,"-"),",",".")) * 30, 0) - 1
                       oEndDate = iDate + ROUND(DECIMAL(REPLACE(SUBSTRING(ENTRY(2,code.code,"-"),
                                                                          1,
                                                                          INDEX(ENTRY(2,code.code,"-"), "М") - 1
                                                                         ),",",".")) * 30, 0) - 1
                     .
                  ELSE
                     IF     INDEX(code.code, "Г")       NE 0
                        AND NUM-ENTRIES(code.code, "-") GT 1 THEN 
                        ASSIGN
                          oBegDate = iDate + ROUND(DECIMAL(REPLACE(ENTRY(1,code.code,"-"),",",".")) * 365, 0) - 1
                          oEndDate = iDate + ROUND(DECIMAL(REPLACE(SUBSTRING(ENTRY(2,code.code,"-"),
                                                                             1,
                                                                             INDEX(ENTRY(2,code.code,"-"), "Г") - 1
                                                                            ),",",".")) * 365, 0) - 1.        
            END.
       END CASE.
   END.
   RETURN (IF AVAILABLE code THEN code.val 
                             ELSE "").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FDopKrit TERMINAL-SIMULATION 
FUNCTION FDopKrit RETURNS CHARACTER
  ( INPUT iClassC AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vListParam AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE vListKrit  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vKrit      AS CHARACTER NO-UNDO.
                
    DEFINE BUFFER code FOR code.

    /* SORT-ACCESS code */
    FOR EACH code NO-LOCK
       WHERE code.class  EQ iClassC 
         AND code.parent EQ iClassC 
       BREAK BY INT64(ENTRY(1,code.code,"_")) :

       {additem.i vListParam code.val }

       IF LAST-OF( INT64(ENTRY(1,code.code,"_")) ) THEN
       DO:
          vKrit = IF NUM-ENTRIES(vListParam) EQ 1 
                  THEN code.val
                  ELSE FDopKrit1(vListParam, code.name) NO-ERROR.   
          {additem.i vListKrit vKrit } 
          ASSIGN vListParam = "".
       END.
    END.

    RETURN vListKrit.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FDopKrit1 TERMINAL-SIMULATION 
FUNCTION FDopKrit1 RETURNS CHARACTER
  ( INPUT iLstParam AS CHARACTER,
    INPUT iName     AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
                             
    DEFINE VARIABLE vKrit AS CHARACTER VIEW-AS COMBO-BOX NO-UNDO.
    DEFINE VARIABLE vWH   AS WIDGET-HANDLE               NO-UNDO.
    DEFINE VARIABLE vI    AS INT64                       NO-UNDO.

    FORM   
    WITH FRAME dataframe CENTERED ROW 10 OVERLAY 1 COL NO-LABELS COLOR MESSAGES
     TITLE "[ ДОПОЛНИТЕЛЬНЫЕ КРИТЕРИИ ВЫБОРА ТИПА ТАРИФА ]".

    ON RETURN OF FRAME dataframe ANYWHERE APPLY "GO".

    vKrit = ENTRY(1,iLstParam).
    DISPLAY iName FORMAT "x(40)" vKrit FORMAT "x(25)" WITH FRAME dataframe. 
    ASSIGN  vWH              = vKrit:HANDLE IN FRAME dataframe
            vWH:PRIVATE-DATA = ""
    .

    DO vI = 1 TO NUM-ENTRIES(iLstParam) :
       IF vWH:ADD-LAST(ENTRY(vI,iLstParam)) THEN
       {additem.i vWH:PRIVATE-DATA ENTRY(vI,iLstParam)}
    END.

    vWH:INNER-LINES = NUM-ENTRIES(vWH:LIST-ITEMS).

    DO ON ERROR  UNDO, LEAVE
       ON ENDKEY UNDO, LEAVE
       WITH FRAME dataframe :                           
       UPDATE vKrit.
    END.                                       
    vKrit = ENTRY(vKrit:LOOKUP(vKrit:SCREEN-VALUE), vKrit:PRIVATE-DATA).             

    HIDE FRAME dataframe NO-PAUSE.

    RETURN vKrit.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FTypTar TERMINAL-SIMULATION 
FUNCTION FTypTar RETURNS CHARACTER
  ( INPUT iBranchId AS CHARACTER,
    INPUT iSafeType AS CHARACTER,
    INPUT iSrok     AS INT64 ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vTypTar  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vDopKrit AS CHARACTER NO-UNDO.

    vDopKrit = FDopKrit("СейфКрит") NO-ERROR.
    vDopKrit = IF {assigned vDopKrit} THEN "," + vDopKrit
                                      ELSE "".
            
    vTypTar  = GetRefVal("СейфТарифы", 
                         gend-date, 
                         iBranchId + ",METHOD1_" + 
                         iSafeType + ",METHOD3_" + 
                         STRING(iSrok) + 
                         vDopKrit) NO-ERROR.

    IF vTypTar EQ ? THEN
    RUN Fill-SysMes IN h_tmess ("",
                                "",
                                "-1",
                                "Невозможно определить тип тарифа в <Indicate/СейфТарифы> по указанному набору критериев: " +
                                iBranchId + "," + iSafeType + "," + STRING(iSrok) + vDopKrit).
    RETURN vTypTar.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* $LINTFILE='f-safe.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='23/09/2016 18:12:45.708+03:00' */
/*prosignRsA9X1hPnJ7N58cHb+dZOw*/