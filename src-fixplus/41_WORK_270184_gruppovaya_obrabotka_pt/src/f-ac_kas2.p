/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: f-ac_kas2.p
      Comment: Форма просмотра данных операции и соглашений к Договору РКО.
   Parameters: 
         Uses:
      Used BY:
      Created: 02.06.2016 Sami 0270184
     Modified:      
*/

{globals.i}
{intrface.get xclass}
{flt-file.i}

{intrface.get cust}

DEFINE INPUT PARAMETER iOp       AS INT NO-UNDO.

DEFINE VARIABLE mAddress AS CHAR NO-UNDO.
DEFINE VARIABLE mKPP AS CHAR NO-UNDO.
DEFINE VARIABLE mType AS CHAR NO-UNDO.
DEFINE VARIABLE mCode AS CHAR NO-UNDO.
DEFINE VARIABLE mAcct AS CHAR NO-UNDO.  
                        
DEFINE TEMP-TABLE tt-tab NO-UNDO
    FIELD fdoc-num      AS CHARACTER
    FIELD fopen-date    AS DATE
    FIELD fend-date     AS DATE
    FIELD fcomment      AS CHARACTER
    FIELD fcontract     AS CHARACTER
    FIELD fcont-code    AS CHARACTER
    FIELD fuslov        AS CHARACTER
    FIELD fsovp         AS CHARACTER
.

FIND FIRST op WHERE op.op EQ iOp NO-LOCK NO-ERROR.
   
{form.def}

DEFINE VARIABLE mNum AS CHAR FORMAT "x(15)":U 
   LABEL "Номер платежного требования" 
   VIEW-AS FILL-IN SIZE 16 BY 1 NO-UNDO. 

DEFINE VARIABLE mINN AS CHAR FORMAT "x(12)":U 
   LABEL "ИНН" 
   VIEW-AS FILL-IN SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE mName AS CHAR FORMAT "x(160)":U 
   LABEL "Наименование    " 
   VIEW-AS EDITOR /*NO-WORD-WRAP*/ SIZE 58 BY 2 NO-UNDO. 
 
DEFINE VARIABLE mDetails AS CHAR FORMAT "x(210)":U
   LABEL "Содержание" 
   VIEW-AS EDITOR /*NO-WORD-WRAP*/ SIZE 64 BY 3 NO-UNDO. 
   
DEFINE VARIABLE mUsl AS CHAR FORMAT "x(195)":U
   LABEL "Условие" 
   VIEW-AS EDITOR /*NO-WORD-WRAP*/ SIZE 65 BY 3 NO-UNDO.   
 
DEFINE QUERY q1 FOR tt-tab SCROLLING.

FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
FIND FIRST loan-acct WHERE loan-acct.acct EQ op-entry.acct-db
            AND loan-acct.contract EQ "Расчет" NO-LOCK NO-ERROR.
            
ASSIGN
   mNum     =  op.doc-num
   mINN     =  GetXAttrValueEx("op", STRING(op.op), "INN-rec", "").
   mName    =  GetXAttrValueEx("op", STRING(op.op), "name-rec", "").
   mDetails =  op.details
.    

IF mINN EQ "" OR mName EQ "" THEN
DO:
   mINN = op.inn.
   mName = op.name-ben.
END.

IF mINN EQ "" OR mName EQ "" THEN
DO:
   FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
   DO:
      mName = GetCliName(acct.cust-cat,
                        STRING(acct.cust-id),
                        OUTPUT mAddress,
                        OUTPUT mINN,
                        OUTPUT mKPP,
                        INPUT-OUTPUT mType,
                        OUTPUT mCode,
                        OUTPUT mAcct).  
   END.
END.
             
FOR EACH loan
   WHERE loan.contract EQ 'КассСогАкц'
     AND loan.parent-contract  EQ loan-acct.contract
     AND loan.parent-cont-code EQ loan-acct.cont-code
     AND loan.open-date        LE gend-date
     AND loan.close-date       EQ ?
     AND (loan.end-date        EQ ?
       OR loan.end-date        GE gend-date)
   NO-LOCK:
           
   CREATE tt-tab.
   ASSIGN
      fdoc-num      = TRIM(loan.doc-num)
      fopen-date    = loan.open-date 
      fend-date     = loan.end-date
      fcomment      = loan.comment
      fcontract     = loan.contract 
      fcont-code    = loan.cont-code 
      fuslov        = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,
                     "УслСпис","").
      fsovp         = (IF mINN EQ tt-tab.fdoc-num THEN "!" ELSE "")            
   .

END.           
          
OPEN QUERY q1
     FOR EACH tt-tab NO-LOCK.
                    
DEFINE BROWSE b1
    QUERY q1
    DISPLAY
        tt-tab.fsovp
            FORMAT "x(1)"
            COLUMN-LABEL " "
        tt-tab.fdoc-num
            FORMAT "x(12)"
            COLUMN-LABEL "ИНН"
        tt-tab.fcomment
            FORMAT "x(55)"
            COLUMN-LABEL "Получатель"             
      /*  tt-tab.fopen-date
            FORMAT "99/99/9999"
            COLUMN-LABEL "Дата от" 
        tt-tab.fend-date
            FORMAT "99/99/9999"
            COLUMN-LABEL "Дата до" */
      /*  tt-tab.fuslov
            FORMAT "x(15)"
            COLUMN-LABEL "Условие" */
   WITH 3 DOWN
NO-ROW-MARKERS.

DEFINE FRAME fMain
   b1 SKIP
   mUsl SKIP
   "----------------------------------------------------------------------------" SKIP
   mINN                 /* SKIP */
   mNum                 SKIP
   mName                SKIP
   mDetails             SKIP
   " "
AT ROW 6 COL 1 LEFT-ALIGNED
WITH 1 DOWN KEEP-TAB-ORDER OVERLAY TITLE "Соглашения к счету " + DelFilFromAcct(op-entry.acct-db)
SIDE-LABELS NO-UNDERLINE THREE-D 
AT COL 2 ROW 4
SIZE 78 BY 19.

ON "F1" OF BROWSE b1 DO:
   IF AVAIL tt-tab THEN DO:
       RUN f-ac_kas.p("","","",tt-tab.fcontract + "," + tt-tab.fcont-code,3,"").
   END.
END.

/* для первого входа */
mUsl = tt-tab.fuslov.


ON VALUE-CHANGED OF BROWSE b1 DO: 
mUsl = tt-tab.fuslov.

   DISPLAY
      b1
      mUsl
      mINN
      mNum     
      mName
      mDetails
   WITH FRAME fMain.
END.

PAUSE 0.

DISPLAY
   b1
   mUsl
   mINN
   mNum     
   mName
   mDetails
WITH FRAME fMain.

SET b1 WITH FRAME fMain.



