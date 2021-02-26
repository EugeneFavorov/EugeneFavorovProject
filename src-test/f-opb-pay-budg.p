&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 Character
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME TERMINAL-SIMULATION


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-opb-pay NO-UNDO 
       FIELD pokdd$ AS CHARACTER /* ПокДД */
       FIELD poknd$ AS CHARACTER /* ПокНД */
       FIELD poknp$ AS CHARACTER /* ПокНП */
       FIELD pokop$ AS CHARACTER /* ПокОП */
       FIELD pokst$ AS CHARACTER /* ПокСт */
       FIELD poktp$ AS CHARACTER /* ПокТП */
       FIELD kbk$ AS CHARACTER /* КБК */
       FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
       FIELD details AS CHARACTER /* details */
       FIELD pntelefon$ AS CHARACTER /* ПНТелефон */
       FIELD pninn$ AS CHARACTER /* ПНИНН */
       FIELD pnnomerblanka$ AS CHARACTER /* ПННомерБланка */
       FIELD pnkodvidaplatewza$ AS CHARACTER /* ПНКодВидаПлатежа */
       FIELD pnkodparametraplatewza$ AS CHARACTER /* ПНКодПараметраПлатежа */
       FIELD Birthday AS DATE /* Birthday */
       FIELD adres$ AS CHARACTER /* Адрес */
       FIELD FIASHouseGuid AS CHARACTER /* FIASHouseGuid */
       FIELD dokum$ AS CHARACTER /* Докум */
       FIELD fio$ AS CHARACTER /* ФИО */
       FIELD country-pers AS CHARACTER /* country-pers */
       FIELD cust-doc-who AS CHARACTER /* cust-doc-who */
       FIELD document-id AS CHARACTER /* document-id */
       FIELD Document4Date_vid AS DATE /* Document4Date_vid */
       FIELD BirthPlace AS CHARACTER /* BirthPlace */
       FIELD acct     AS CHARACTER /* acct */
       FIELD cont-cli AS CHARACTER /* cont-cli */
       FIELD bik$     AS DECIMAL /* БИК */
       FIELD pnpoluwcatelw#naimenovanie$ AS CHARACTER /* ПНПолучательНаименование */
       FIELD pnpoluwcatelw#inn$ AS CHARACTER /* ПНПолучательИНН */
       FIELD pnpoluwcatelw#kpp$ AS CHARACTER /* ПНПолучательКПП */
       FIELD amt-rub AS DECIMAL /* amt-rub */
       FIELD amt-rub-commis AS DECIMAL /* amt-rub-commis */
       FIELD op-date AS DATE /* op-date */
       FIELD name-last AS CHARACTER /* name-last */
       FIELD first-names AS CHARACTER /* first-names */
       FIELD NewReceiver AS LOGICAL /* NewReceiver */
       FIELD pnkomplat$ AS CHARACTER /* ПНКомПлат */
       FIELD uin$ AS CHARACTER
       FIELD uip$ AS CHARACTER
       FIELD uiz$ AS CHARACTER
       FIELD aip$ AS CHARACTER
       FIELD eip$ AS CHARACTER       
       FIELD branch-id AS CHARACTER /* branch-id */
       /* Записываем ссылку на временную таблицу в специальную таблицу */
       {ln-tthdl.i "tt-opb-pay" "" }
       .


DEFINE VARIABLE mUIN               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mEIP               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAIP               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mUIZ               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mUIP               AS CHARACTER  NO-UNDO.

/*Признак обязательности ввода реквизитов СМЭВ - АИП и ЕИП*/
DEFINE VARIABLE mSMEVreq           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mESC               AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mIDPerson          AS INT64       NO-UNDO.
DEFINE VARIABLE mNalCheck          AS CHARACTER  NO-UNDO.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS TERMINAL-SIMULATION 
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: f-opb-pay-budg.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 
     Modified: 
*/
/*          This file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
{globals.i}
{intrface.get pbase}
{intrface.get fnum}
{intrface.get comm}
{intrface.get trans}
{intrface.get brnch}
{intrface.get xclass}
{intrface.get ppn}
{def-wf.i NEW}
{148n.i}
DEFINE VAR in-op-date AS DATE NO-UNDO.
{details.def}
{limitsum.chk}

DEFINE VAR mSumLimitSP AS DECIMAL NO-UNDO.
DEFINE VAR mLimitVP    AS CHAR    NO-UNDO.
mSumLimitSP = DECIMAL(FGetSetting("СуммаПлат",?,"0")) NO-ERROR.
IF mSumLimitSP = ? THEN mSumLimitSP = 0.0.
mLimitVP = FGetSetting("ВидПлИденКл",?,"").

CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */

DEFINE VAR mNonFilledHdl AS HANDLE  NO-UNDO.
DEFINE VAR mQuestWas     AS LOGICAL NO-UNDO.
DEFINE VAR mDetailsOk    AS LOGICAL NO-UNDO.
DEFINE VAR mSeparate     AS LOGICAL NO-UNDO.
DEFINE VAR mGroupBy      AS CHAR    NO-UNDO.

DEFINE VAR mAdresFull    AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE MAIN-FRAME fMain

/* Расскомментировать в случае вызова из NAVIGATE.CQR
{navigate.cqr
   ...
   &UseBisTTY=YES
   &edit=bis-tty.ef
   ...
}
   Если определена &UseBisTTY - то ссылка на динамическую таблицу верхнего класса
будет храниться в переменной IInstance.
   Если определена &InstanceFile - то будет определена и заполнена статическая
TEMP-TABLE tt-instance LIKE {&InstanceFile}

&GLOBAL-DEFINE UseBisTTY 
&GLOBAL-DEFINE InstanceFile ИМЯ_ТАБЛИЦЫ_ПРОГРЕСС_ДЛЯ_ВЕРХНЕГО_КЛАССА
*/

/* Для просмотра полученной mInstance в GetObject */
&GLOBAL-DEFINE xDEBUG-INSTANCE-GET

{sh-defs.i}
{g-error.def}
{intrface.get op}
{intrface.get pbase}
{intrface.get instrum}
{intrface.get acct}
{intrface.get cust}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-opb-pay.pnkodvidaplatewza$ ~
tt-opb-pay.cont-cli tt-opb-pay.op-date ~
tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.acct ~
tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$ tt-opb-pay.bik$ ~
tt-opb-pay.pokst$ tt-opb-pay.kbk$ tt-opb-pay.pokop$ tt-opb-pay.poknd$ ~
tt-opb-pay.poktp$ tt-opb-pay.okato-nalog$ tt-opb-pay.poknp$ ~
tt-opb-pay.pokdd$ tt-opb-pay.amt-rub tt-opb-pay.amt-rub-commis ~
tt-opb-pay.name-last tt-opb-pay.first-names tt-opb-pay.adres$ ~
tt-opb-pay.country-pers tt-opb-pay.Birthday tt-opb-pay.BirthPlace ~
tt-opb-pay.dokum$ tt-opb-pay.cust-doc-who tt-opb-pay.Document4Date_vid ~
tt-opb-pay.pninn$ tt-opb-pay.pntelefon$ tt-opb-pay.details  tt-opb-pay.uin$
&Scoped-define ENABLED-TABLES tt-opb-pay
&Scoped-define FIRST-ENABLED-TABLE tt-opb-pay
&Scoped-Define ENABLED-OBJECTS mBankName mCorrAcct mTarif mTarifSign mItogo ~
mDocumentIdName 
&Scoped-Define DISPLAYED-FIELDS tt-opb-pay.pnkodvidaplatewza$ ~
tt-opb-pay.cont-cli tt-opb-pay.op-date ~
tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.acct ~
tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$ tt-opb-pay.bik$ ~
tt-opb-pay.pokst$ tt-opb-pay.kbk$ tt-opb-pay.pokop$ tt-opb-pay.poknd$ ~
tt-opb-pay.poktp$ tt-opb-pay.okato-nalog$ tt-opb-pay.poknp$ ~
tt-opb-pay.pokdd$ tt-opb-pay.amt-rub tt-opb-pay.amt-rub-commis ~
tt-opb-pay.name-last tt-opb-pay.first-names tt-opb-pay.adres$ ~
tt-opb-pay.country-pers tt-opb-pay.Birthday tt-opb-pay.BirthPlace ~
tt-opb-pay.dokum$ tt-opb-pay.cust-doc-who tt-opb-pay.Document4Date_vid ~
tt-opb-pay.pninn$ tt-opb-pay.pntelefon$ tt-opb-pay.details tt-opb-pay.uin$
&Scoped-define DISPLAYED-TABLES tt-opb-pay
&Scoped-define FIRST-DISPLAYED-TABLE tt-opb-pay
&Scoped-Define DISPLAYED-OBJECTS separator1 mBankName mCorrAcct separator2 ~
mTarif mTarifSign mItogo separator3 separator4 mDocumentIdName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tt-opb-pay.cont-cli ~
tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.acct ~
tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$ tt-opb-pay.bik$ ~
tt-opb-pay.pokst$ tt-opb-pay.kbk$ tt-opb-pay.pokop$ tt-opb-pay.poknd$ ~
tt-opb-pay.poktp$ tt-opb-pay.okato-nalog$ tt-opb-pay.poknp$ ~
tt-opb-pay.pokdd$ tt-opb-pay.amt-rub tt-opb-pay.name-last ~
tt-opb-pay.first-names tt-opb-pay.adres$ tt-opb-pay.country-pers ~
tt-opb-pay.Birthday tt-opb-pay.BirthPlace mDocumentIdName tt-opb-pay.dokum$ ~
tt-opb-pay.cust-doc-who tt-opb-pay.Document4Date_vid tt-opb-pay.pninn$ ~
tt-opb-pay.pntelefon$ tt-opb-pay.details tt-opb-pay.uin$
&Scoped-define List-2 tt-opb-pay.pokst$ tt-opb-pay.kbk$ tt-opb-pay.pokop$ ~
tt-opb-pay.poknd$ tt-opb-pay.poktp$ tt-opb-pay.okato-nalog$ ~
tt-opb-pay.poknp$ tt-opb-pay.pokdd$ tt-opb-pay.amt-rub tt-opb-pay.name-last ~
tt-opb-pay.first-names tt-opb-pay.adres$ tt-opb-pay.country-pers ~
tt-opb-pay.Birthday tt-opb-pay.BirthPlace mDocumentIdName tt-opb-pay.dokum$ ~
tt-opb-pay.cust-doc-who tt-opb-pay.Document4Date_vid tt-opb-pay.pninn$ ~
tt-opb-pay.pntelefon$ tt-opb-pay.details tt-opb-pay.uin$
&Scoped-define List-3 tt-opb-pay.pnkodvidaplatewza$ tt-opb-pay.cont-cli ~
tt-opb-pay.op-date tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.acct ~
tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$ tt-opb-pay.bik$ ~
mBankName mCorrAcct tt-opb-pay.pokst$ tt-opb-pay.kbk$ tt-opb-pay.pokop$ ~
tt-opb-pay.poknd$ tt-opb-pay.poktp$ tt-opb-pay.okato-nalog$ ~
tt-opb-pay.poknp$ tt-opb-pay.pokdd$ mTarif mTarifSign tt-opb-pay.amt-rub ~
tt-opb-pay.amt-rub-commis mItogo tt-opb-pay.name-last ~
tt-opb-pay.first-names tt-opb-pay.adres$ tt-opb-pay.country-pers ~
tt-opb-pay.Birthday tt-opb-pay.BirthPlace mDocumentIdName tt-opb-pay.dokum$ ~
tt-opb-pay.cust-doc-who tt-opb-pay.Document4Date_vid tt-opb-pay.pninn$ ~
tt-opb-pay.pntelefon$ tt-opb-pay.details tt-opb-pay.uin$
&Scoped-define List-4 tt-opb-pay.pnkodvidaplatewza$ tt-opb-pay.cont-cli ~
tt-opb-pay.op-date tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.acct ~
tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$ tt-opb-pay.bik$ ~
mBankName mCorrAcct tt-opb-pay.pokst$ tt-opb-pay.kbk$ tt-opb-pay.pokop$ ~
tt-opb-pay.poknd$ tt-opb-pay.poktp$ tt-opb-pay.okato-nalog$ ~
tt-opb-pay.poknp$ tt-opb-pay.pokdd$ mTarif mTarifSign tt-opb-pay.amt-rub ~
tt-opb-pay.amt-rub-commis mItogo tt-opb-pay.name-last ~
tt-opb-pay.first-names tt-opb-pay.adres$ tt-opb-pay.country-pers ~
tt-opb-pay.Birthday tt-opb-pay.BirthPlace mDocumentIdName tt-opb-pay.dokum$ ~
tt-opb-pay.cust-doc-who tt-opb-pay.Document4Date_vid tt-opb-pay.pninn$ ~
tt-opb-pay.pntelefon$ tt-opb-pay.details tt-opb-pay.uin$

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR TERMINAL-SIMULATION AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE mBankName AS CHARACTER FORMAT "X(256)":U 
     LABEL "БАНК" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 48 BY 1
     &ELSE SIZE 48 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mCorrAcct AS CHARACTER FORMAT "X(256)":U 
     LABEL "К/С" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
     &ELSE SIZE 20 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mDocumentIdName AS CHARACTER FORMAT "x(256)" INITIAL ? 
     LABEL "ТИП ДОКУМЕНТА" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
     &ELSE SIZE 13 BY 1 &ENDIF.

DEFINE VARIABLE mItogo AS DECIMAL FORMAT ">>>>>>>9.99":U INITIAL 0 
     LABEL "ИТОГО" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mTarif AS DECIMAL FORMAT ">>>>>>>9.99":U INITIAL 0 
     LABEL "ТАРИФ КОМИССИИ" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE mTarifSign AS CHARACTER FORMAT "x(1)":U INITIAL ""
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 1 BY 1
     &ELSE SIZE 1 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator1 AS CHARACTER FORMAT "X(256)":U INITIAL "--------------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 77 BY 1
     &ELSE SIZE 77 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator2 AS CHARACTER FORMAT "X(256)":U INITIAL "--------------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 77 BY 1
     &ELSE SIZE 77 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator3 AS CHARACTER FORMAT "X(256)":U INITIAL "--------------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 77 BY 1
     &ELSE SIZE 77 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE separator4 AS CHARACTER FORMAT "X(256)":U INITIAL "--------------------------------------------------------------------------------------------------------------------------------" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 77 BY 1
     &ELSE SIZE 77 BY 1 &ENDIF NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tt-opb-pay.pnkodvidaplatewza$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 12 COLON-ALIGNED
          &ELSE AT ROW 1 COL 12 COLON-ALIGNED &ENDIF HELP
          "Код вида платежа"
          LABEL "ВИД ПЛАТЕЖА" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-opb-pay.cont-cli
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 18 COLON-ALIGNED
          &ELSE AT ROW 2 COL 18 COLON-ALIGNED &ENDIF HELP
          "ПЛАТЕЖ"
          LABEL "ПАРАМЕТРЫ ПЛАТЕЖА" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 40 BY 1
          &ELSE SIZE 40 BY 1 &ENDIF
     tt-opb-pay.op-date
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 66 COLON-ALIGNED
          &ELSE AT ROW 2 COL 66 COLON-ALIGNED &ENDIF
          LABEL "ДАТА"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     separator1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 1
          &ELSE AT ROW 3 COL 1 &ENDIF NO-LABEL
     tt-opb-pay.pnpoluwcatelw#naimenovanie$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 12 COLON-ALIGNED
          &ELSE AT ROW 4 COL 12 COLON-ALIGNED &ENDIF HELP
          "ПОЛУЧАТЕЛЬ"
          LABEL "ПОЛУЧАТЕЛЬ" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 64 BY 1
          &ELSE SIZE 64 BY 1 &ENDIF
     tt-opb-pay.acct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 12 COLON-ALIGNED
          &ELSE AT ROW 5 COL 12 COLON-ALIGNED &ENDIF HELP
          "Р/С"
          LABEL "Р/С" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 20 BY 1
          &ELSE SIZE 20 BY 1 &ENDIF
     tt-opb-pay.pnpoluwcatelw#inn$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 43 COLON-ALIGNED
          &ELSE AT ROW 5 COL 43 COLON-ALIGNED &ENDIF HELP
          "ИНН"
          LABEL "ИНН" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
     tt-opb-pay.pnpoluwcatelw#kpp$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 62 COLON-ALIGNED
          &ELSE AT ROW 5 COL 62 COLON-ALIGNED &ENDIF HELP
          "КПП"
          LABEL "КПП" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
          &ELSE SIZE 14 BY 1 &ENDIF
     tt-opb-pay.bik$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 12 COLON-ALIGNED
          &ELSE AT ROW 6 COL 12 COLON-ALIGNED &ENDIF HELP
          "БИК БАНКА"
          LABEL "БИК БАНКА" FORMAT "999999999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 9 BY 1
          &ELSE SIZE 9 BY 1 &ENDIF
     mBankName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 28 COLON-ALIGNED
          &ELSE AT ROW 6 COL 28 COLON-ALIGNED &ENDIF HELP
          "БАНК"
     mCorrAcct
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 12 COLON-ALIGNED
          &ELSE AT ROW 7 COL 12 COLON-ALIGNED &ENDIF HELP
          "К/С"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 26.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     separator2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 1
          &ELSE AT ROW 8 COL 1 &ENDIF NO-LABEL
     tt-opb-pay.pokst$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 19 COLON-ALIGNED
          &ELSE AT ROW 10 COL 19 COLON-ALIGNED &ENDIF HELP
          "ПОКАЗАТЕЛЬ СТАТУСА ПЛАТЕЛЬЩИКА"
          LABEL "СТАТУС ПЛАТЕЛЬЩИКА" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-opb-pay.kbk$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 4 COLON-ALIGNED
          &ELSE AT ROW 11 COL 4 COLON-ALIGNED &ENDIF HELP
          "КБК"
          LABEL "КБК" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 23 BY 1
          &ELSE SIZE 23 BY 1 &ENDIF
     tt-opb-pay.pokop$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 19 COLON-ALIGNED
          &ELSE AT ROW 11.99 COL 19 COLON-ALIGNED &ENDIF HELP
          "ОСНОВАНИЯ ПЛАТЕЖА"
          LABEL "ОСНОВАНИЯ ПЛАТЕЖА" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-opb-pay.poknd$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 19 COLON-ALIGNED
          &ELSE AT ROW 13 COL 19 COLON-ALIGNED &ENDIF HELP
          "НОМЕРА ДОКУМЕНТА"
          LABEL "НОМЕР ДОКУМЕНТА" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-opb-pay.poktp$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 19 COLON-ALIGNED
          &ELSE AT ROW 14 COL 19 COLON-ALIGNED &ENDIF HELP
          "ТИПА ПЛАТЕЖА"
          LABEL "ТИПА ПЛАТЕЖА" FORMAT "x(50)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-opb-pay.okato-nalog$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 39 COLON-ALIGNED
          &ELSE AT ROW 11.99 COL 39 COLON-ALIGNED &ENDIF HELP
          "КОД ОКТМО"
          LABEL "КОД ОКТМО" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 8 BY 1
          &ELSE SIZE 12 BY 1 &ENDIF
      tt-opb-pay.uin$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 12 COL 57 COLON-ALIGNED
          &ELSE AT ROW 11.99 COL 62 COLON-ALIGNED &ENDIF HELP
          "УИН"
          LABEL "УИН" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
          &ELSE SIZE 25 BY 1 &ENDIF            
     tt-opb-pay.poknp$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 39 COLON-ALIGNED
          &ELSE AT ROW 13 COL 39 COLON-ALIGNED &ENDIF HELP
          "НАЛОГОВОГО ПЕРИОДА"
          LABEL "ПЕРИОД" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
     tt-opb-pay.pokdd$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 14 COL 39 COLON-ALIGNED
          &ELSE AT ROW 14 COL 39 COLON-ALIGNED &ENDIF HELP
          "ДАТЫ ДОКУМЕНТА"
          LABEL "ДАТЫ ДОКУМЕНТА" FORMAT "xx/xx/xxxx"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 26.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     mTarif
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 65 COLON-ALIGNED
          &ELSE AT ROW 9 COL 65 COLON-ALIGNED &ENDIF HELP
          "Тариф комиссии"
     mTarifSign
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 75 COLON-ALIGNED
          &ELSE AT ROW 9 COL 75 COLON-ALIGNED &ENDIF NO-LABEL
     tt-opb-pay.amt-rub
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 10 COL 65 COLON-ALIGNED
          &ELSE AT ROW 10 COL 65 COLON-ALIGNED &ENDIF HELP
          "СУММА ПЛАТЕЖА"
          LABEL "СУММА ПЛАТЕЖА" FORMAT ">>>>>>>9.99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     tt-opb-pay.amt-rub-commis
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 11 COL 65 COLON-ALIGNED
          &ELSE AT ROW 11 COL 65 COLON-ALIGNED &ENDIF HELP
          "СУММА КОМИССИИ"
          LABEL "СУММА КОМИССИИ" FORMAT ">>>>>>>9.99"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
          &ELSE SIZE 11 BY 1 &ENDIF
     mItogo
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 13 COL 65 COLON-ALIGNED
          &ELSE AT ROW 13 COL 65 COLON-ALIGNED &ENDIF HELP
          "Итог"
     separator3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 15 COL 1
          &ELSE AT ROW 15 COL 1 &ENDIF NO-LABEL
     tt-opb-pay.details
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 20 COLON-ALIGNED
          &ELSE AT ROW 16 COL 20 COLON-ALIGNED &ENDIF HELP
          "Назначение платежа"
          LABEL "НАЗНАЧЕНИЕ ПЛАТЕЖА" FORMAT "x(50)"
          VIEW-AS EDITOR NO-WORD-WRAP LARGE
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 56 BY 2
          &ELSE SIZE 56 BY 2 &ENDIF
     separator4
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 1
          &ELSE AT ROW 18 COL 1 &ENDIF NO-LABEL
     tt-opb-pay.name-last
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 14 COLON-ALIGNED
          &ELSE AT ROW 19 COL 14 COLON-ALIGNED &ENDIF HELP
          "ФАМИЛИЯ"
          LABEL "ФАМИЛИЯ" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
          &ELSE SIZE 16 BY 1 &ENDIF
     tt-opb-pay.first-names
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 45 COLON-ALIGNED
          &ELSE AT ROW 19 COL 45 COLON-ALIGNED &ENDIF HELP
          "ИМЯ, ОТЧЕСТВО"
          LABEL "ИМЯ, ОТЧЕСТВО" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 31 BY 1
          &ELSE SIZE 31 BY 1 &ENDIF
     tt-opb-pay.adres$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 20 COL 14 COLON-ALIGNED
          &ELSE AT ROW 20 COL 14 COLON-ALIGNED &ENDIF HELP
          "Адрес клиента"
          LABEL "АДРЕС" FORMAT "x(250)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 62 BY 1
          &ELSE SIZE 62 BY 1 &ENDIF
     tt-opb-pay.country-pers
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 21 COL 14 COLON-ALIGNED
          &ELSE AT ROW 21 COL 14 COLON-ALIGNED &ENDIF HELP
          "Символьный код страны клиента"
          LABEL "КОД СТРАНЫ" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 3 BY 1
          &ELSE SIZE 3 BY 1 &ENDIF
     tt-opb-pay.Birthday
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 21 COL 33 COLON-ALIGNED
          &ELSE AT ROW 21 COL 33 COLON-ALIGNED &ENDIF HELP
          "Дата рождения клиента"
          LABEL "ДАТА РОЖДЕНИЯ" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
          &ELSE SIZE 10 BY 1 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 26.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     tt-opb-pay.BirthPlace
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 21 COL 60 COLON-ALIGNED
          &ELSE AT ROW 21 COL 60 COLON-ALIGNED &ENDIF HELP
          "Место рождения"
          LABEL "МЕСТО РОЖДЕНИЯ" FORMAT "x(250)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 16 BY 1
          &ELSE SIZE 16 BY 1 &ENDIF
     mDocumentIdName
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 22 COL 14 COLON-ALIGNED
          &ELSE AT ROW 22 COL 14 COLON-ALIGNED &ENDIF HELP
          "Тип документа клиента"
     tt-opb-pay.dokum$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 22 COL 43 COLON-ALIGNED
          &ELSE AT ROW 22 COL 43 COLON-ALIGNED &ENDIF HELP
          "Серия и номер документа клиента"
          LABEL "СЕРИЯ И НОМЕР" FORMAT "x(52)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 33 BY 1
          &ELSE SIZE 33 BY 1 &ENDIF
     tt-opb-pay.cust-doc-who
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 23 COL 14 COLON-ALIGNED
          &ELSE AT ROW 23 COL 14 COLON-ALIGNED &ENDIF HELP
          "Кем выдан документ"
          LABEL "КЕМ ВЫДАН" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 40 BY 1
          &ELSE SIZE 40 BY 1 &ENDIF
     tt-opb-pay.Document4Date_vid
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 23 COL 69 COLON-ALIGNED
          &ELSE AT ROW 23 COL 69 COLON-ALIGNED &ENDIF HELP
          "Дата выдачи документа"
          LABEL "  ДАТА ВЫДАЧИ" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 7 BY 1
          &ELSE SIZE 7 BY 1 &ENDIF
     tt-opb-pay.pninn$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 24 COL 14 COLON-ALIGNED
          &ELSE AT ROW 24 COL 14 COLON-ALIGNED &ENDIF HELP
          "ИНН"
          LABEL "ИНН" FORMAT "x(13)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 13 BY 1
          &ELSE SIZE 13 BY 1 &ENDIF
     tt-opb-pay.pntelefon$
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 24 COL 43 COLON-ALIGNED
          &ELSE AT ROW 24 COL 43 COLON-ALIGNED &ENDIF HELP
          "Телефон"
          LABEL "ТЕЛЕФОН" FORMAT "x(50)"
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 33 BY 1
          &ELSE SIZE 33 BY 1 &ENDIF
     "ДОПОЛНИТЕЛЬНЫЕ РЕКВИЗИТЫ" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 25 BY 1
          &ELSE SIZE 25 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 11
          &ELSE AT ROW 9 COL 11 &ENDIF
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 26
        TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tt-opb-pay T "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          FIELD pokdd$ AS CHARACTER /* ПокДД */
          FIELD poknd$ AS CHARACTER /* ПокНД */
          FIELD poknp$ AS CHARACTER /* ПокНП */
          FIELD pokop$ AS CHARACTER /* ПокОП */
          FIELD pokst$ AS CHARACTER /* ПокСт */
          FIELD poktp$ AS CHARACTER /* ПокТП */
          FIELD kbk$ AS CHARACTER /* КБК */
          FIELD okato-nalog$ AS CHARACTER /* ОКАТО-НАЛОГ */
          FIELD details AS CHARACTER /* details */
          FIELD pntelefon$ AS CHARACTER /* ПНТелефон */
          FIELD pninn$ AS CHARACTER /* ПНИНН */
          FIELD pnnomerblanka$ AS CHARACTER /* ПННомерБланка */
          FIELD pnkodvidaplatewza$ AS CHARACTER /* ПНКодВидаПлатежа */
          FIELD pnkodparametraplatewza$ AS CHARACTER /* ПНКодПараметраПлатежа */
          FIELD Birthday AS DATE /* Birthday */
          FIELD adres$ AS CHARACTER /* Адрес */
          FIELD dokum$ AS CHARACTER /* Докум */
          FIELD fio$ AS CHARACTER /* ФИО */
          FIELD country-pers AS CHARACTER /* country-pers */
          FIELD cust-doc-who AS CHARACTER /* cust-doc-who */
          FIELD document-id AS CHARACTER /* document-id */
          FIELD Document4Date_vid AS DATE /* Document4Date_vid */
          FIELD BirthPlace AS CHARACTER /* BirthPlace */
          FIELD acct     AS CHARACTER /* acct */
          FIELD cont-cli AS CHARACTER /* cont-cli */
          FIELD bik$     AS DECIMAL /* БИК */
          FIELD pnpoluwcatelw#naimenovanie$ AS CHARACTER /* ПНПолучательНаименование */
          FIELD pnpoluwcatelw#inn$ AS CHARACTER /* ПНПолучательИНН */
          FIELD pnpoluwcatelw#kpp$ AS CHARACTER /* ПНПолучательКПП */
          FIELD amt-rub AS DECIMAL /* amt-rub */
          FIELD amt-rub-commis AS DECIMAL /* amt-rub-commis */
          FIELD op-date AS DATE /* op-date */
          FIELD name-last AS CHARACTER /* name-last */
          FIELD first-names AS CHARACTER /* first-names */
          FIELD NewReceiver AS LOGICAL /* NewReceiver */
          FIELD pnkomplat$ AS CHARACTER /* ПНКомПлат */
          FIELD branch-id AS CHARACTER /* branch-id */
          /* Записываем ссылку на временную таблицу в специальную таблицу */
          {ln-tthdl.i "tt-opb-pay" "" }
          
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
         HEIGHT             = 31.99
         WIDTH              = 114
         MAX-HEIGHT         = 31.99
         MAX-WIDTH          = 114
         VIRTUAL-HEIGHT     = 31.99
         VIRTUAL-WIDTH      = 114
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
/* SETTINGS FOR FILL-IN tt-opb-pay.acct IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-opb-pay.adres$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.amt-rub IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.amt-rub-commis IN FRAME fMain
   3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-opb-pay.bik$ IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-opb-pay.Birthday IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.BirthPlace IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.cont-cli IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-opb-pay.country-pers IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.cust-doc-who IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR EDITOR tt-opb-pay.details IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
ASSIGN 
       tt-opb-pay.details:RETURN-INSERTED IN FRAME fMain  = TRUE.

/* SETTINGS FOR FILL-IN tt-opb-pay.Document4Date_vid IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.dokum$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.first-names IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.kbk$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN mBankName IN FRAME fMain
   3 4                                                                  */
/* SETTINGS FOR FILL-IN mCorrAcct IN FRAME fMain
   3 4                                                                  */
/* SETTINGS FOR FILL-IN mDocumentIdName IN FRAME fMain
   1 2 3 4                                                              */
/* SETTINGS FOR FILL-IN mItogo IN FRAME fMain
   3 4                                                                  */
/* SETTINGS FOR FILL-IN mTarif IN FRAME fMain
   3 4                                                                  */
/* SETTINGS FOR FILL-IN mTarifSign IN FRAME fMain
   3 4                                                                  */
/* SETTINGS FOR FILL-IN tt-opb-pay.name-last IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.okato-nalog$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.op-date IN FRAME fMain
   3 4 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN tt-opb-pay.pninn$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.pnkodvidaplatewza$ IN FRAME fMain
   3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                    */
/* SETTINGS FOR FILL-IN tt-opb-pay.pnpoluwcatelw#inn$ IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-opb-pay.pnpoluwcatelw#kpp$ IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-opb-pay.pnpoluwcatelw#naimenovanie$ IN FRAME fMain
   1 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                  */
/* SETTINGS FOR FILL-IN tt-opb-pay.pntelefon$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.pokdd$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.poknd$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.poknp$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.pokop$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.pokst$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN tt-opb-pay.poktp$ IN FRAME fMain
   1 2 3 4 EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN separator1 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       separator1:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN separator2 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       separator2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN separator3 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       separator3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN separator4 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       separator4:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(TERMINAL-SIMULATION)
THEN TERMINAL-SIMULATION:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME tt-opb-pay.poknp$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.poknp$ TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.poknp$ IN FRAME fMain /* ПокНП */
DO:
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
      RUN codelay.p ("Нал:НП","Нал:НП","", 4).
      IF LASTKEY = 10 THEN DO WITH FRAME {&MAIN-FRAME}:
         tt-opb-pay.poknp$ = pick-value.
         DISPLAY tt-opb-pay.poknp$.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-opb-pay.pokdd$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.pokdd$ TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.pokdd$ IN FRAME fMain /* ПокДД */
DO:
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
      RUN calend.p.
      IF LASTKEY = 10 THEN DO WITH FRAME {&MAIN-FRAME}:
         tt-opb-pay.pokdd$:SCREEN-VALUE = STRING(DATE(pick-value),"99/99/9999").
         ASSIGN tt-opb-pay.pokdd$.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain TERMINAL-SIMULATION
ON F3 OF FRAME fMain ANYWHERE
DO:
   mSMEVreq = NO.
   RUN smev IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK TERMINAL-SIMULATION 

/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{f-opb-pay-common.i &opb-pay-budg = YES}

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
   mRetVal = IF mOnlyForm THEN {&RET-ERROR} ELSE "".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

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
   FIND FIRST tt-opb-pay NO-ERROR.

   in-op-date = tt-opb-pay.op-date.

   /* Заполняем COMBO-BOX'ы данными из метасхемы */
   RUN FillComboBox(FRAME {&MAIN-FRAME}:HANDLE).

   /* Commented by KSV: Показываем экранную форму */
   STATUS DEFAULT "".

   RUN enable_UI.

   SetListNoHiddForTeml("cl-*").
   /* Commented by KSV: Открываем те поля, которые разрешено изменять
   ** в зависимости от режима открытия */
   RUN EnableDisable.

   /* Commented by KSV: Рисуем разделители. Разделители задаются как FILL-IN
   ** с идентификатором SEPARATOR# с атрибутом VIES-AS TEXT */
   RUN Separator(FRAME fMain:HANDLE,"2").

   mNonFilledHdl = ?.
   RUN FindMandButNonFilledWidg (INPUT-OUTPUT mNonFilledHdl, ?, ?, YES).
   IF VALID-HANDLE(mNonFilledHdl)
   THEN mFirstTabItem = mNonFilledHdl.
   ELSE DO:
      IF {assigned tt-opb-pay.fio$}
      THEN mFirstTabItem = tt-opb-pay.amt-rub:HANDLE.
   END.

   RUN AddCustomStringToHelp.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS mFirstTabItem.

END.

/* Commented by KSV: Закрываем службу системных сообщений */
RUN End-SysMes.

RUN disable_ui.

/* Commented by KSV: Удаляем экземпляр объекта */
IF VALID-HANDLE(mInstance) AND NOT mOnlyForm THEN 
   RUN DelEmptyInstance(mInstance).

IF iMode = {&MOD_ADD}
OR iMode = {&MOD_EDIT} THEN DO:
   IF  TRANSACTION
   AND mRetVal = {&RET-ERROR}
   AND GetBaseOpkind() = "ПНФорма"
   THEN mRetVal = {&RET-SKIP}.
END.

/* Commented by KSV: Выгружаем библиотеки */
{intrface.del}

/* Commented by KSV: Возвращаем значение вызывающей процедуре */
RETURN mRetVal.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY separator1 mBankName mCorrAcct separator2 mTarif mTarifSign mItogo separator3 
          separator4 mDocumentIdName 
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  IF AVAILABLE tt-opb-pay THEN 
    DISPLAY tt-opb-pay.pnkodvidaplatewza$ tt-opb-pay.cont-cli tt-opb-pay.op-date 
          tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.acct 
          tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$ 
          tt-opb-pay.bik$ tt-opb-pay.pokst$ tt-opb-pay.kbk$ tt-opb-pay.pokop$ 
          tt-opb-pay.poknd$ tt-opb-pay.poktp$ tt-opb-pay.okato-nalog$ 
          tt-opb-pay.poknp$ tt-opb-pay.pokdd$ tt-opb-pay.amt-rub 
          tt-opb-pay.amt-rub-commis tt-opb-pay.name-last tt-opb-pay.first-names 
          tt-opb-pay.adres$ tt-opb-pay.country-pers tt-opb-pay.Birthday 
          tt-opb-pay.BirthPlace tt-opb-pay.dokum$ tt-opb-pay.cust-doc-who 
          tt-opb-pay.Document4Date_vid tt-opb-pay.pninn$ tt-opb-pay.pntelefon$ 
          tt-opb-pay.details tt-opb-pay.uin$
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  ENABLE tt-opb-pay.pnkodvidaplatewza$ tt-opb-pay.cont-cli tt-opb-pay.op-date 
         tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.acct 
         tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$ 
         tt-opb-pay.bik$ mBankName mCorrAcct tt-opb-pay.pokst$ tt-opb-pay.kbk$ 
         tt-opb-pay.pokop$ tt-opb-pay.poknd$ tt-opb-pay.poktp$ 
         tt-opb-pay.okato-nalog$ tt-opb-pay.poknp$ tt-opb-pay.pokdd$ mTarif mTarifSign 
         tt-opb-pay.amt-rub tt-opb-pay.amt-rub-commis mItogo 
         tt-opb-pay.name-last tt-opb-pay.first-names tt-opb-pay.adres$ 
         tt-opb-pay.country-pers tt-opb-pay.Birthday tt-opb-pay.BirthPlace 
         mDocumentIdName tt-opb-pay.dokum$ tt-opb-pay.cust-doc-who 
         tt-opb-pay.Document4Date_vid tt-opb-pay.pninn$ tt-opb-pay.pntelefon$ 
         tt-opb-pay.details tt-opb-pay.uin$
      WITH FRAME fMain IN WINDOW TERMINAL-SIMULATION.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW TERMINAL-SIMULATION.
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
   ASSIGN
      tt-opb-pay.pokdd$ = TRIM(REPLACE(REPLACE(tt-opb-pay.pokdd$,"/",""),".",""))
   .
   IF  {assigned tt-opb-pay.pokdd$}
   AND tt-opb-pay.pokdd$ <> "0"
   THEN tt-opb-pay.pokdd$ = SUBSTR(tt-opb-pay.pokdd$,1,2) + "." + SUBSTR(tt-opb-pay.pokdd$,3,2) + "." + SUBSTR(tt-opb-pay.pokdd$,5).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


PROCEDURE Smev:

   DEF BUFFER code    FOR code.
   DEF BUFFER country FOR country.
   DEF BUFFER person  FOR person.

      DEFINE VARIABLE vCntry  AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vDoc AS CHARACTER   NO-UNDO.
   ASSIGN 
     vCntry = tt-opb-pay.country-pers:SCREEN-VALUE IN FRAME fMain
     vDoc   = tt-opb-pay.dokum$:SCREEN-VALUE IN FRAME fMain
   .
   FIND FIRST country WHERE 
              country.country-id =  vCntry
   NO-LOCK NO-ERROR.
     
   FIND FIRST code WHERE 
              code.class =  "КодДокум"
          AND code.code  =  tt-opb-pay.document-id
   NO-LOCK NO-ERROR.
    
   IF AVAIL code THEN
      mAIP =    STRING(code.description[3],"99")   + 
                STRING(INT64(REPLACE(vDoc," ","")), 
                       "99999999999999999999")      +     
                (IF AVAIL country 
                 THEN STRING(country.country-alt-id,"999")
                 ELSE "").

   mUIN =  tt-opb-pay.uin$:SCREEN-VALUE IN FRAME fMain.

   RUN nalpl_smev.p(
       INPUT ?,
       INPUT ?,
       INPUT NO,
       INPUT mSMEVreq,
       INPUT-OUTPUT mUIN, 
       INPUT-OUTPUT mEIP, 
       INPUT-OUTPUT mAIP, 
       INPUT-OUTPUT mUIP).

   IF LASTKEY <> 10 THEN
      IF mSMEVreq =  YES THEN
         mESC = YES.
   ASSIGN
      tt-opb-pay.uin$ = mUIN
      tt-opb-pay.uiz$ = mUIZ
      tt-opb-pay.aip$ = mAIP
      tt-opb-pay.eip$ = mEIP
          
                                                   
   tt-opb-pay.uin$:SCREEN-VALUE = mUIN.
   

   FIND FIRST country WHERE 
              country.country-alt-id =  INT64(SUBSTRING(mAIP,23,3))
   NO-LOCK NO-ERROR.
   IF AVAIL country  THEN
      tt-opb-pay.country-pers:SCREEN-VALUE = country.country-id.

   FIND FIRST code WHERE 
              code.class            =  "КодДокум"
          AND code.description[3]   =  SUBSTRING(mAIP,1,2)
   NO-LOCK NO-ERROR.
   IF AVAIL code  THEN
      ASSIGN 
         tt-opb-pay.document-id       = code.code
         mDocumentIdName:SCREEN-VALUE = code.name
      .
   tt-opb-pay.dokum$ = LEFT-TRIM(SUBSTRING(mAIP,3,20)).           
   
 

   IF {assigned tt-opb-pay.uin$} THEN RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"УИН-save",tt-opb-pay.uin$).
   IF {assigned tt-opb-pay.uiz$} THEN RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"УИЗ-save",tt-opb-pay.uiz$).
   IF {assigned tt-opb-pay.aip$} THEN RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"АИП-save",tt-opb-pay.aip$).
   IF {assigned tt-opb-pay.eip$} THEN RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"ЕИП-save",tt-opb-pay.eip$).
END PROCEDURE.
/* $LINTFILE='f-opb-pay-budg.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:37.671+03:00' */
/*prosignnb25+aZfbghLwwHsS7cKtw*/