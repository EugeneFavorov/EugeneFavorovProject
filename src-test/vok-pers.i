/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: vok-pers.i
      Comment: PERS-FRAME в экранных формах транзакций ВОК
   Parameters:
         Uses:
      Used by: 
      Created: fedm в 2004 
     Modified: 17.01.2005 17:02 ligp     39221: Нет возможности редактирования значения поля "Валюта"
                                         для vok-kas
     Modified: 01.09.2005 19:16 rija     43692: Реализовать транзакцию на выполнение операции по
                                         выбранному счету клиента
     Modified: 15.11.2006 15:53 DEMA     (0069967) Ошибка создания клиента через
                                         операции модуля ВОК
     Modified: 28.07.2008 19:50 elus      0093507: Запрет на проведение операций ВОК на сумму выше
                                          имеющейся наличности в смене
*/

/* Начало - Клиенты с ролью из ДР "ПРольСубъекта" на op-kind */

{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get parsr}    /* Интерпретатор Bisquit Script (парсер) */
{intrface.get data}     /* Библиотека для работы с отвязанным набором данных. */
{intrface.get cust}     /* Библиотека для работы с клиентами. */


&IF DEFINED(DefXattr) = 0 &THEN

   &GLOBAL-DEFINE DefXattr YES

   DEFINE VARIABLE vChRole             AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vChOpKind           AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vChSreenAcct        AS CHARACTER  NO-UNDO.

   DEFINE BUFFER B_Op-Kind FOR op-kind.

   DEFINE TEMP-TABLE ttperson NO-UNDO
      FIELD person-id AS INT64.

   FUNCTION GetCust-orp RETURNS INT64 (INPUT iAcct AS CHARACTER):
      DEFINE VARIABLE vIntCustId AS INT64    NO-UNDO.
      {find-act.i &acct=iAcct}
      IF AVAILABLE acct THEN
         vIntCustId = acct.cust-id.
      ELSE 
         vIntCustId = ?.
      RETURN vIntCustId.
   END FUNCTION.

   PROCEDURE CreateTTPerson:
      DEFINE INPUT  PARAMETER iAcct          AS CHARACTER  NO-UNDO.
      DEFINE OUTPUT PARAMETER TABLE FOR ttperson.  

      DEFINE VARIABLE vChTableNameCustRole   AS CHARACTER INITIAL "cust-corp" NO-UNDO.
      DEFINE VARIABLE vChSurrogateCustRole   AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE vChClassCodeCustRole   AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE vDateDoc-date          AS DATE       NO-UNDO.

      ASSIGN
         vChSurrogateCustRole = STRING(GetCust-orp(iAcct))
         vChClassCodeCustRole = vChRole
         vDateDoc-date        = IF {&tt-op}.doc-date <> ? THEN {&tt-op}.doc-date ELSE TODAY.
         .
      
      FOR EACH cust-role WHERE
              cust-role.File-name =  vChTableNameCustRole
         AND  cust-role.Surrogate =  vChSurrogateCustRole
         AND cust-role.Class-code =  vChClassCodeCustRole
         AND cust-role.Open-date  <= vDateDoc-date
         AND cust-role.Close-date >= IF cust-role.Close-date <> ? THEN vDateDoc-date ELSE cust-role.Close-date
         NO-LOCK:
         CREATE ttperson.
         ttperson.person-id = INT64(cust-role.Cust-id) NO-ERROR. 
      END.

   END PROCEDURE.
&ENDIF
/* Конец - Клиенты с ролью из ДР "ПРольСубъекта" на op-kind */

&IF DEFINED(AssignFrame) &THEN
    &IF "{&AssignFrame}" = "ADD" &THEN
       ASSIGN FRAME {&PERS-FRAME} {&CUSTOMLIST-1} .
    &ENDIF
    &IF "{&AssignFrame}" = "EDIT" &THEN
       ASSIGN FRAME {&PERS-FRAME} {&CUSTOMLIST-2} .
    &ENDIF
&ENDIF
&IF DEFINED(InitFrame) &THEN
{vok-pers.def}
&GLOBAL-DEFINE PERS-FRAME fPerson

&IF DEFINED(tt-vok-val) = 0 &THEN
   &GLOBAL-DEFINE tt-vok-val tt-vok-val
&ENDIF

/* ID клиента */
DEF VAR mPersonId   AS INT64   NO-UNDO INITIAL ?.

/* Наименование валюты счета */
DEF VAR mCurr3-name  AS CHAR  FORMAT "X(256)"
   VIEW-AS TEXT SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE mAdrProp      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSurrAdr      AS CHARACTER NO-UNDO INIT ?.
DEFINE VARIABLE mParamList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mParam1       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE mVal          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTmpMode      AS INT64     NO-UNDO.
DEFINE VARIABLE mInstanceProp AS HANDLE    NO-UNDO.

DEFINE VARIABLE mTmpFio               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpCountry-pers      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpDocument-id       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpDokum             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpCust-doc-who      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpDocument4Date_vid AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpAdres             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpBirthPlace        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpBirthday          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpPodrazd           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mTmpKodRegGni         AS CHARACTER   NO-UNDO.

DEFINE VARIABLE scrTmpCountry-pers    AS CHARACTER   NO-UNDO.
scrTmpCountry-pers = "1111111111".

DEF VAR mHError AS HANDLE NO-UNDO.

/*
      if index(PROGRAM-NAME(1),"vok-val.p") > 0 then do:
         scrTmpCountry-pers = "ГРАЖДАНСТВО".
      end.
      else do:
         scrTmpCountry-pers = "РЕЗИДЕНТНОСТЬ".
      end.
*/
DEF FRAME {&PERS-FRAME}
     {&tt-op}.fio$
          AT ROW 1 COL 13 COLON-ALIGNED
          HELP  "Фамилия, имя, отчество клиента ВОК"
          LABEL "КЛИЕНТ" FORMAT "x(140)"
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     {&tt-op}.dover$
          AT ROW 1 COL 59
          HELP  "Доверенность"
          LABEL "ДОВЕРЕННОСТЬ"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY 1
&IF DEFINED(tt-op-entry) &THEN
     {&tt-op-entry}.acct-cr
          AT ROW 2 COL 13 COLON-ALIGNED
          HELP  "Кредит счета"
          LABEL "СЧЕТ" FORMAT "xxxxx-xxx-x-xxxx-xxxxxxx"
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     {&tt-op-entry}.currency
          AT ROW 2 COL 47 COLON-ALIGNED
          HELP  "ВАЛ"
          LABEL "ВАЛ" FORMAT "xxx"
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
     mCurr3-name
          AT ROW 2 COL 51 COLON-ALIGNED
          NO-LABEL
     &SCOPED-DEFINE CurRow 3
&ELSE
     &SCOPED-DEFINE CurRow 2
&ENDIF

     {&tt-op}.country-pers
          AT ROW {&CurRow} COL 13 COLON-ALIGNED HELP
          "Символьный код страны клиента"
          LABEL "КОД СТРАНЫ" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 3 BY 1 
     {&tt-op}.document-id
          AT ROW {&CurRow} COL 32 COLON-ALIGNED HELP
          "Тип документа клиента"
          LABEL "ТИП ДОКУМЕНТА" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1 
     {&tt-op}.dokum$
          AT ROW {&CurRow} COL 57 COLON-ALIGNED HELP
          "Серия и номер документа клиента"
          LABEL "СЕРИЯ И НОМЕР" FORMAT "x(52)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
&IF DEFINED(tt-op-entry) &THEN
     &SCOPED-DEFINE CurRow 4
&ELSE
     &SCOPED-DEFINE CurRow 3
&ENDIF
     {&tt-op}.cust-doc-who
          AT ROW {&CurRow} COL 13 COLON-ALIGNED HELP
          "Кем выдан документ"
          LABEL "КЕМ ВЫДАН" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     {&tt-op}.podrazd$      
          AT ROW {&CurRow} COL 46 COLON-ALIGNED HELP
          "Код подразделения выдавшего документ "
          LABEL "КОД ПОДР" FORMAT "x(17)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1          
     {&tt-op}.Document4Date_vid
          AT ROW {&CurRow} COL 67 COLON-ALIGNED HELP
          "Дата выдачи документа"
          LABEL "  ДАТА ВЫДАЧИ" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1

&IF DEFINED(tt-op-entry) &THEN
     &SCOPED-DEFINE CurRow 5
&ELSE
     &SCOPED-DEFINE CurRow 4
&ENDIF
     {&tt-op}.adres$
          AT ROW {&CurRow} COL 13 COLON-ALIGNED HELP
          "Адрес клиента"
          LABEL "АДРЕС" FORMAT "x(250)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1 
     {&tt-op}.kodreggni$
          AT ROW {&CurRow} COL 50 COLON-ALIGNED HELP
          "Код региона ГНИ"
          LABEL "КОД ГНИ" FORMAT "x(2)"
          VIEW-AS FILL-IN
          SIZE 2 BY 1

     {&tt-op}.Birthday
          AT ROW {&CurRow} COL 67 COLON-ALIGNED HELP
          "Дата рождения клиента"
          LABEL "ДАТА РОЖДЕНИЯ" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
&IF DEFINED(tt-op-entry) &THEN
     &SCOPED-DEFINE CurRow 6
&ELSE
     &SCOPED-DEFINE CurRow 5
&ENDIF
     {&tt-op}.BirthPlace
          AT ROW {&CurRow} COL 17 COLON-ALIGNED HELP
          "Место рождения"
          LABEL "МЕСТО РОЖДЕНИЯ" FORMAT "x(250)"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
&IF DEFINED(tt-op-entry) &THEN
     &SCOPED-DEFINE CurRow 7
&ELSE
     &SCOPED-DEFINE CurRow 6
&ENDIF
     {&tt-op}.form-seria
          AT ROW {&CurRow} COL 13 COLON-ALIGNED HELP
          "Серия выданной справки"
          LABEL "СПРАВКА" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1 
     {&tt-op}.form-num
          AT ROW 6 COL 24 COLON-ALIGNED HELP
          "Номер выданной справки" NO-LABEL FORMAT "x(300)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D.

&IF DEFINED(tt-op-entry) &THEN

&SCOPED-DEFINE SELF-NAME {&tt-op-entry}.acct-cr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL {&tt-op-entry}.acct-cr TERMINAL-SIMULATION
ON F1 OF {&tt-op-entry}.acct-cr IN FRAME {&PERS-FRAME} /* СЧЕТ */
DO:
   IF mPersonId <> ? THEN
   RUN browseld.p("acct",
                  "acct-cat" + CHR(1) + "cust-cat" + CHR(1) + "cust-id",
                  "b"        + CHR(1) + "Ч"        + CHR(1) + STRING(mPersonId),
                  "acct-cat",
                  iLevel + 1
                 ).
   ELSE
   RUN browseld.p("acct",
                  "acct-cat",
                  "b",
                  "acct-cat",
                  iLevel + 1
                 ).

   IF LASTKEY = 10 THEN
   DO:
      SELF:SCREEN-VALUE = ENTRY(1, pick-value).
      APPLY "TAB" TO SELF.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL {&tt-op-entry}.acct-cr TERMINAL-SIMULATION
ON LEAVE OF {&tt-op-entry}.acct-cr IN FRAME {&PERS-FRAME} /* СЧЕТ */
DO:
   DEF VAR vAcct      AS CHAR  NO-UNDO.
   DEF VAR vCurr      AS CHAR  NO-UNDO.
   DEF VAR vCurrName  AS CHAR  NO-UNDO.
   DEF VAR vMess      AS CHAR  NO-UNDO.
   DEFINE VARIABLE StrWhere AS CHARACTER   NO-UNDO.

   {&BEG_BT_LEAVE}

   IF {assigned SELF:INPUT-VALUE} THEN
   DO:
      StrWhere = IF shmode THEN 
                    "WHERE number = " + QUOTER(SELF:INPUT-VALUE) + " AND filial-id = " +  QUOTER(ShFilial)
                 ELSE
                    "WHERE acct = " + QUOTER(SELF:INPUT-VALUE).
      ASSIGN 
         vAcct = GetBufferValue("acct", StrWhere,"acct,currency")
         vCurr = ENTRY(2, vAcct, CHR(2))
         vAcct = ENTRY(1, vAcct, CHR(2))
      NO-ERROR.

      IF vAcct <> ? THEN
         ASSIGN
            {&tt-op-entry}.currency:SCREEN-VALUE = vCurr
            mCurr3-name:SCREEN-VALUE = GetBUfferValue("currency",
                                                      "WHERE currency.currency = " + QUOTER(vCurr),
                                                      "name-currenc"
                                                     ).
      IF vAcct = ? THEN
         vMess = "Счет " + QUOTER(SELF:SCREEN-VALUE) + " не найден".
      ELSE IF vCurr <> "" AND vCurr <> tt-op-entry1.currency:SCREEN-VALUE IN FRAME {&MAIN-FRAME} THEN
         vMess = "Счет " + QUOTER(SELF:SCREEN-VALUE)
               + " не соответствует валюте " + tt-op-entry1.currency:SCREEN-VALUE IN FRAME {&MAIN-FRAME}.
      ELSE 
         vMess = "".

      IF vMess <> "" THEN
      DO:
         MESSAGE vMess
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY {&RET-ERROR}.
      END.

      /* 
        Если в экранной форме определена процедура LoanAcct_Leave, то запускаем её.
        (определение курса и пересчет суммы при операциях со счетами физ. лиц)
      */
      IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES, "LoanAcct_Leave") THEN
      DO:
         RUN LoanAcct_Leave NO-ERROR. 
         IF ERROR-STATUS:ERROR THEN
            RETURN NO-APPLY.
      END.                            

   END.

   {&END_BT_LEAVE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&SCOPED-DEFINE SELF-NAME {&tt-op}.fio$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL {&tt-op}.fio$ TERMINAL-SIMULATION
ON F1 OF {&tt-op}.fio$ IN FRAME {&PERS-FRAME} /* КЛИЕНТ */
DO:
   &SCOPED-DEFINE PersMenuItems 'Клиент ВОК,Клиент банка,Не клиент банка,Договор'

   RUN messmenu.p (FRAME {&PERS-FRAME}:ROW + SELF:ROW - 1,
                   "",
                   "",
                   {&PersMenuItems}
                  ).
   IF LASTKEY = 27 THEN
      RETURN NO-APPLY.
   RUN FindClient (ENTRY(INT64(pick-value), {&PersMenuItems}),
                   GetWidgetHandle(FRAME {&MAIN-FRAME}:HANDLE, "mCont-Code"),
                   {&tt-op}.fio$:HANDLE
                  ).

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(SESSION-REMOTE) &THEN
/* Тригер для замены неработающего в QBIS ANY-PRINTABLE */
&SCOPED-DEFINE SELF-NAME {&tt-op}.adres$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL {&tt-op}.adres$ TERMINAL-SIMULATION
ON ENTRY OF {&tt-op}.adres$ IN FRAME {&PERS-FRAME} /* adres$ */
DO:
   SELF:READ-ONLY = YES.
   IF NOT {assigned SELF:SCREEN-VALUE} THEN
      APPLY "F1" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ENDIF


&SCOPED-DEFINE SELF-NAME {&tt-op}.adres$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL {&tt-op}.adres$ TERMINAL-SIMULATION
ON F1, ANY-PRINTABLE OF {&tt-op}.adres$ IN FRAME {&PERS-FRAME} /* adres$ */
DO:
   DEFINE VARIABLE mOk           AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vAdrCntry     AS CHARACTER  NO-UNDO. /* адрес страны */
   DEFINE VARIABLE vAdrIndInt    AS INT64      NO-UNDO. /* Индекс */   
   DEFINE VARIABLE vOblChar      AS CHARACTER  NO-UNDO. /* Район  */   
   DEFINE VARIABLE vGorChar      AS CHARACTER  NO-UNDO. /* Город  */   
   DEFINE VARIABLE vPunktChar    AS CHARACTER  NO-UNDO. /* Нас.пункт */
   DEFINE VARIABLE vUlChar       AS CHARACTER  NO-UNDO. /* Ул.    */   
   DEFINE VARIABLE vDomChar      AS CHARACTER  NO-UNDO. /* Дом    */   
   DEFINE VARIABLE vKorpChar     AS CHARACTER  NO-UNDO. /* Корп.  */   
   DEFINE VARIABLE vKvChar       AS CHARACTER  NO-UNDO. /* Кв.    */   
   DEFINE VARIABLE vStrChar      AS CHARACTER  NO-UNDO. /* Стр.   */   

   DEFINE BUFFER cust-ident FOR cust-ident.
   FIND FIRST cust-ident WHERE cust-ident.Class-code     =  "p-cust-adr"
                           AND cust-ident.cust-code-type =  "АдрПроп"
                           AND cust-ident.cust-cat       =  "Ч"
                           AND cust-ident.cust-id        =  mPersonId NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
      mSurrAdr = cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num).
   mParamList = CHR(3) + "Ч" + CHR(3) + (IF mPersonId =  ? THEN ""
                                                           ELSE STRING(mPersonId)) + CHR(3) + "АдрПроп".
   ASSIGN
      mParam1  = "U1~np-cust-adr"
      mTmpMode = IF mPersonId =  ? THEN 1
                                   ELSE 2
   .
   IF     mPersonId =  ?
       OR (NOT AVAIL cust-ident
      AND mPersonId <> ?)   THEN
   DO:
      RUN GetInstance IN h_data ("p-cust-adr",
                                 ?,
                                 OUTPUT mInstanceProp,
                                 OUTPUT mOk).
      RUN SetInstanceProp (mInstanceProp,"cust-cat"      ,"Ч"      ,OUTPUT mOk) NO-ERROR.
      RUN SetInstanceProp (mInstanceProp,"cust-code-type","АдрПроп",OUTPUT mOk) NO-ERROR.

      IF {assigned mAdrProp} THEN
         RUN SetInstanceProp (mInstanceProp,"issue",mAdrProp,OUTPUT mOk) NO-ERROR.

      ASSIGN
         mParam1  = "U1~n" + STRING(mInstanceProp) + "~003p-cust-adr"
         mTmpMode = 1
      .
   END.

   RUN formld.p (mParam1,
                 IF {assigned mSurrAdr} THEN mSurrAdr
                                        ELSE "",
                 mParamList, 
                 mTmpMode,
                 iLevel + 1).
   IF LAST-EVENT:FUNCTION =  "GO" THEN
   DO:
      IF     mPersonId =  ?
          OR (NOT AVAIL cust-ident
         AND mPersonId <> ?) THEN
      DO:
         mTmpBuffer = mInstanceProp:DEFAULT-BUFFER-HANDLE.
         mTmpBuffer:FIND-FIRST().

         mVal = mTmpBuffer:BUFFER-FIELD("issue"):BUFFER-VALUE.
      END.
      ELSE
         mVal = cust-ident.issue.

      mAdrProp = TRIM(mVal).
      
      {&tt-op}.adres$:SCREEN-VALUE = fGetStrAdr(TRIM(mVal)).
   END.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&SCOPED-DEFINE SELF-NAME {&tt-op}.form-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL {&tt-op}.form-num TERMINAL-SIMULATION
ON LEAVE OF {&tt-op}.form-num IN FRAME {&PERS-FRAME} /* form-num */
DO:
   IF vLogOff113i THEN
   DO:
      {&BEG_BT_LEAVE}

      DEF VAR vRes  AS LOGICAL  NO-UNDO.

   &IF DEFINED(ttFormNumbers) &THEN
      /* Чтобы не испортить содержимое - сохраняем tt */
      RUN GetTTFormNumbers IN h_fnum (OUTPUT TABLE ttFormNumbers).
   &ENDIF
     
      RUN CrTwoTT-DbCr IN h_fnum (mDpr-Id,"Справки","*","Наличие").

      vRes = ChkNumber (mDpr-Id, 
                        "Справки",
                        "*",
                        "Наличие",
                        {&tt-op}.form-seria:SCREEN-VALUE,
                        INT64({&tt-op}.form-num:SCREEN-VALUE),
                        DEC(1)
                       ).

   &IF DEFINED(ttFormNumbers) &THEN
      /* Восстанавливаем tt */
      RUN SetTTFormNumbers IN h_fnum (INPUT TABLE ttFormNumbers).
   &ENDIF
     
      IF NOT vRes THEN
      DO:
         MESSAGE
            "Справки серия" {&tt-op}.form-seria:SCREEN-VALUE
                    "номер" {&tt-op}.form-num:SCREEN-VALUE   SKIP
            "не было в приходе или она уже использована."
         VIEW-AS ALERT-BOX ERROR.

         RETURN NO-APPLY {&RET-ERROR}.
      END.

      {&END_BT_LEAVE}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* Enable_UI для фрейма fPers (данные клиента) */
PROCEDURE PersED:

   DEFINE VARIABLE vLogView  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vLogView2 AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vLogView3 AS LOGICAL     NO-UNDO.

   vLogView  = LOGICAL(FGetSetting("ДатаРождения","",""),FGetSettFormat("ДатаРождения","","Да/Нет")) NO-ERROR.
   vLogView2 = LOGICAL(FGetSetting("ДатаВыдачи","",""),FGetSettFormat("ДатаВыдачи","","Да/Нет")) NO-ERROR.
   vLogView3 = LOGICAL(FGetSetting("МестоРождения","",""),FGetSettFormat("МестоРождения","","Да/Нет")) NO-ERROR.


   IF AVAILABLE {&tt-op} THEN
      DISPLAY
         {&tt-op}.fio$  {&tt-op}.dover$
         {&tt-op}.country-pers {&tt-op}.document-id  {&tt-op}.dokum$
         {&tt-op}.cust-doc-who
         {&tt-op}.podrazd$
         {&tt-op}.Document4Date_vid
         {&tt-op}.adres$
         {&tt-op}.kodreggni$
         {&tt-op}.Birthday 
         {&tt-op}.BirthPlace 
         {&tt-op}.form-seria {&tt-op}.form-num
      WITH FRAME {&PERS-FRAME} IN WINDOW TERMINAL-SIMULATION.

&IF DEFINED(tt-op-entry) &THEN
   IF AVAILABLE {&tt-op-entry} THEN
      DISPLAY
         {&tt-op-entry}.acct-cr
         {&tt-op-entry}.currency
         mCurr3-name
      WITH FRAME {&PERS-FRAME} IN WINDOW TERMINAL-SIMULATION.
&ENDIF
&SCOPED-DEFINE List
   ENABLE
&SCOPED-DEFINE List {&list} {&tt-op}.fio$  {&tt-op}.dover$
      {&tt-op}.fio$  {&tt-op}.dover$
   &IF DEFINED(tt-op-entry) &THEN
&SCOPED-DEFINE List {&list} {&tt-op-entry}.acct-cr
      {&tt-op-entry}.acct-cr  /*{&tt-op-entry}.currency*/
   &ENDIF
&SCOPED-DEFINE List {&list} {&tt-op}.country-pers {&tt-op}.document-id  {&tt-op}.dokum$ {&tt-op}.cust-doc-who {&tt-op}.podrazd$ {&tt-op}.Document4Date_vid {&tt-op}.adres$ {&tt-op}.kodreggni$ {&tt-op}.Birthday {&tt-op}.BirthPlace {&tt-op}.form-seria {&tt-op}.form-num
      {&tt-op}.country-pers {&tt-op}.document-id  {&tt-op}.dokum$
      {&tt-op}.cust-doc-who
      {&tt-op}.podrazd$      
      {&tt-op}.Document4Date_vid
      {&tt-op}.adres$
      {&tt-op}.kodreggni$
      {&tt-op}.Birthday 
      {&tt-op}.BirthPlace
      {&tt-op}.form-seria {&tt-op}.form-num
   WITH FRAME {&PERS-FRAME} IN WINDOW TERMINAL-SIMULATION.

&GLOBAL-DEFINE CustomList-1 {&List}
&GLOBAL-DEFINE CustomList-2 {&List}
&GLOBAL-DEFINE CustomList-3 {&List}

   DEFINE VARIABLE vHBDF  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vHBDF2 AS HANDLE      NO-UNDO.
   DEFINE VARIABLE vHBDF3 AS HANDLE      NO-UNDO.

   vHBDF  = GetWidgetHandle(FRAME {&PERS-FRAME}:HANDLE,"{&tt-op}.Birthday").
   vHBDF2 = GetWidgetHandle(FRAME {&PERS-FRAME}:HANDLE,"{&tt-op}.Document4Date_vid").
   vHBDF3 = GetWidgetHandle(FRAME {&PERS-FRAME}:HANDLE,"{&tt-op}.BirthPlace").

&IF DEFINED(tt-op-entry) &THEN
   DEF VAR vH   AS HANDLE  NO-UNDO.
   DEF VAR vHF  AS HANDLE  NO-UNDO.

   vHF = GetWidgetHandle(FRAME {&PERS-FRAME}:HANDLE,"{&tt-op-entry}.acct-cr").
   {view-fld.i vH vHF IsViewAcctDps()}
   vHF = GetWidgetHandle(FRAME {&PERS-FRAME}:HANDLE,"{&tt-op-entry}.currency").
   IF NOT IsViewAcctDps() THEN
   DO:
      {view-fld.i vH vHF NO}
   END.

   mCurr3-name:READ-ONLY IN FRAME {&PERS-FRAME} = YES.

&ENDIF

   RUN InitForm(FRAME {&PERS-FRAME}:HANDLE).
   
    vHBDF:HIDDEN = NOT vLogView.
   vHBDF2:HIDDEN = NOT vLogView2.
   vHBDF3:HIDDEN = NOT vLogView3.

   IF vLogOff113i THEN
      ASSIGN
         {&tt-op}.form-seria:HIDDEN = YES
         {&tt-op}.form-num:HIDDEN   = YES.

   RUN CheckPers.

   RETURN.

END PROCEDURE.

{limitsum.chk} /* Функции и переменные для определения граничной рублевой суммы */

/* Проверим обязательность заполнения данных клиента при большой сумме документа
** + контроль по "чёрному списку"
*/
{limitsum.i
   &TabName = {&tt-op}
}

/* SetObject для фрейма fPers (данные клиента) */
PROCEDURE PersSO:

   DEFINE VARIABLE vLogView  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vLogView2 AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vLogView3 AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vSave AS LOGICAL     NO-UNDO.

   vLogView  = LOGICAL(FGetSetting("ДатаРождения","",""),FGetSettFormat("ДатаРождения","","Да/Нет")) NO-ERROR.
   vLogView2 = LOGICAL(FGetSetting("ДатаВыдачи","",""),FGetSettFormat("ДатаВыдачи","","Да/Нет")) NO-ERROR.
   vLogView3 = LOGICAL(FGetSetting("МестоРождения","",""),FGetSettFormat("МестоРождения","","Да/Нет")) NO-ERROR.

   DEFINE VARIABLE xxx AS HANDLE      NO-UNDO.

   ASSIGN FRAME {&PERS-FRAME}
      {&tt-op}.fio$  {&tt-op}.dover$
      {&tt-op}.country-pers {&tt-op}.document-id  {&tt-op}.dokum$
      {&tt-op}.cust-doc-who
      {&tt-op}.podrazd$      
      {&tt-op}.Document4Date_vid WHEN vLogView2
      {&tt-op}.adres$
      {&tt-op}.kodreggni$
      {&tt-op}.Birthday WHEN vLogView
      {&tt-op}.BirthPlace WHEN vLogView3
      {&tt-op}.form-seria {&tt-op}.form-num
      xxx = BUFFER {&tt-op}:BUFFER-FIELD("Birthday")
   .
   ASSIGN
      {&tt-op}.adres$ = mAdrProp
   .
&IF DEFINED(tt-op-entry) &THEN
   IF AVAILABLE {&tt-op-entry} THEN
      ASSIGN FRAME {&PERS-FRAME}
         {&tt-op-entry}.acct-cr
         {&tt-op-entry}.currency
         mCurr3-name
      .
&ENDIF
   /* Сохранить введённого клиента? */
   IF mPersonId = ? AND
          NOT CAN-FIND(FIRST person WHERE
                             person.document-id = {&tt-op}.document-id
                         AND person.document    = {&tt-op}.dokum$
                         AND person.country-id  = {&tt-op}.country-pers)
      AND GetAttrValue2("",0,"_TypeOper") <> "Конвертация"
      AND (mMaxAmtRub >= mSumLimit2 OR mEnablePers)
   THEN
   DO:
      IF     mMaxAmtRub >= mSumLimit2
         AND FGetSetting("АвтСохрКл","","Нет") =  "Да" THEN
      DO:   
         vSave = YES.
      END.   
      ELSE
      DO:
         RUN Fill-SysMes ("","","4","Сохранять клиента?").
         vSave = LOGICAL(pick-value).
      END.
      IF vSave THEN
         RUN SavePerson.
   END.

   DYNAMIC-FUNCTION("SetFormDefList",DYNAMIC-FUNCTION("GetFormDefList") + ",{&tt-op}.kodreggni$").

   IF vLogView THEN
      DYNAMIC-FUNCTION("SetFormDefList",DYNAMIC-FUNCTION("GetFormDefList") + ",{&tt-op}.Birthday").
   IF vLogView2 THEN
      DYNAMIC-FUNCTION("SetFormDefList",DYNAMIC-FUNCTION("GetFormDefList") + ",{&tt-op}.Document4Date_vid").
   IF vLogView3 THEN
      DYNAMIC-FUNCTION("SetFormDefList",DYNAMIC-FUNCTION("GetFormDefList") + ",{&tt-op}.BirthPlace").
   
   RETURN.

END PROCEDURE.

PROCEDURE CheckPers.

   DEFINE BUFFER code1 FOR code.
   {limitsum.swd}
   DEFINE VARIABLE vVidopnalv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vH         AS HANDLE    NO-UNDO.

   ASSIGN
      vH = GetWidgetHandle((FRAME {&MAIN-FRAME}:HANDLE),"vidopnalv$") 
      vVidopnalv = vh:INPUT-VALUE
      NO-ERROR.

   FIND FIRST code1 WHERE code1.class = "ПравКонтр" AND code1.code = vVidopnalv + "_Обяз" NO-LOCK NO-ERROR.
   {limitsum.sw vVidOpNalV}


   IF (AVAIL code1 AND {assigned code1.name}) OR 
      (IF AVAIL code3 AND code3.val = "gt" THEN mMaxAmtRub > mSumLimit ELSE mMaxAmtRub >= mSumLimit) THEN
   DO:
      DO WHILE VALID-HANDLE(hFieldName):
         IF hFieldName:TYPE = "FILL-IN" THEN
         DO:
            IF LOOKUP(hFieldName:NAME,List) > 0 THEN
            DO:
               IF hFieldName:INPUT-VALUE =  "" THEN
               DO WITH FRAME {&PERS-FRAME}:
                  ASSIGN
                     {&tt-op}.fio$:SCREEN-VALUE              = mTmpFio              
                     {&tt-op}.country-pers:SCREEN-VALUE      = mTmpCountry-pers     
                     {&tt-op}.document-id:SCREEN-VALUE       = mTmpDocument-id      
                     {&tt-op}.dokum$:SCREEN-VALUE            = mTmpDokum            
                     {&tt-op}.cust-doc-who:SCREEN-VALUE      = mTmpCust-doc-who
                     {&tt-op}.podrazd$:SCREEN-VALUE          = mTmpPodrazd
                     {&tt-op}.Document4Date_vid:SCREEN-VALUE = mTmpDocument4Date_vid
                     {&tt-op}.adres$:SCREEN-VALUE            = mTmpAdres            
                     {&tt-op}.kodreggni$                     = mTmpKodRegGni
                     {&tt-op}.BirthPlace:SCREEN-VALUE        = mTmpBirthPlace       
                     {&tt-op}.Birthday:SCREEN-VALUE          = mTmpBirthday         
                     .
                  ENABLE
                     {&tt-op}.fio$
                     {&tt-op}.dover$
                     {&tt-op}.country-pers
                     {&tt-op}.document-id
                     {&tt-op}.dokum$
                     {&tt-op}.cust-doc-who
                     {&tt-op}.podrazd$
                     {&tt-op}.Document4Date_vid
                     {&tt-op}.adres$
                     {&tt-op}.kodreggni$
                     {&tt-op}.BirthPlace
                     {&tt-op}.Birthday
                     .
                  APPLY "LEAVE" TO hFieldName.
                  APPLY 'ENTRY' TO hFieldName.
                  RETURN ERROR {&RET-ERROR}.
               END.
            END.
         END.
         hFieldName = hFieldName:NEXT-SIBLING.
      END.
   END.
   
   IF fGetSetting("ОбязКонтрСумм","","") =  "Да" THEN
   DO:
      IF ((AVAIL code1 AND {assigned code1.name}) OR 
         (IF AVAIL code2 AND code2.val = "gt" THEN mMaxAmtRub > mSumLimit2 ELSE mMaxAmtRub >= mSumLimit2)) THEN DO: 
         DO WHILE VALID-HANDLE(hFieldName):
            IF hFieldName:TYPE = "FILL-IN" THEN
            DO:
               IF LOOKUP(hFieldName:NAME,List2) > 0 THEN
               DO:
                  IF hFieldName:INPUT-VALUE =  "" THEN
                  DO WITH FRAME {&PERS-FRAME}:
                     ASSIGN
                        {&tt-op}.fio$:SCREEN-VALUE              = mTmpFio              
                        {&tt-op}.country-pers:SCREEN-VALUE      = mTmpCountry-pers     
                        {&tt-op}.document-id:SCREEN-VALUE       = mTmpDocument-id      
                        {&tt-op}.dokum$:SCREEN-VALUE            = mTmpDokum            
                        {&tt-op}.cust-doc-who:SCREEN-VALUE      = mTmpCust-doc-who
                        {&tt-op}.podrazd$:SCREEN-VALUE          = mTmpPodrazd     
                        {&tt-op}.Document4Date_vid:SCREEN-VALUE = mTmpDocument4Date_vid
                        {&tt-op}.adres$:SCREEN-VALUE            = mTmpAdres            
                        {&tt-op}.kodreggni$                     = mTmpKodRegGni
                        {&tt-op}.BirthPlace:SCREEN-VALUE        = mTmpBirthPlace       
                        {&tt-op}.Birthday:SCREEN-VALUE          = mTmpBirthday         
                        .
                     ENABLE
                        {&tt-op}.fio$
                        {&tt-op}.dover$
                        {&tt-op}.country-pers
                        {&tt-op}.document-id
                        {&tt-op}.dokum$
                        {&tt-op}.cust-doc-who
                        {&tt-op}.podrazd$
                        {&tt-op}.Document4Date_vid
                        {&tt-op}.adres$
                        {&tt-op}.kodreggni$
                        {&tt-op}.BirthPlace
                        {&tt-op}.Birthday
                        .
                      APPLY "LEAVE" TO hFieldName.
                      APPLY 'ENTRY' TO hFieldName.
                     RETURN ERROR {&RET-ERROR}.
                  END.
               END.
            END.
            hFieldName = hFieldName:NEXT-SIBLING.
         END.
      END.
      ELSE
      DO:
         IF work-module <> "cm" THEN
         DO:
            IF     NOT mEnablePers 
               AND {&tt-op}.fio$:SENSITIVE 
               AND NOT mCheckFio THEN
            DO WITH FRAME {&PERS-FRAME}:
               ASSIGN
                  mTmpFio               = {&tt-op}.fio$:SCREEN-VALUE
                  mTmpCountry-pers      = {&tt-op}.country-pers:SCREEN-VALUE
                  mTmpDocument-id       = {&tt-op}.document-id:SCREEN-VALUE
                  mTmpDokum             = {&tt-op}.dokum$:SCREEN-VALUE
                  mTmpCust-doc-who      = {&tt-op}.cust-doc-who:SCREEN-VALUE
                  mTmpPodrazd           = {&tt-op}.podrazd$:SCREEN-VALUE
                  mTmpDocument4Date_vid = {&tt-op}.Document4Date_vid:SCREEN-VALUE
                  mTmpAdres             = {&tt-op}.adres$:SCREEN-VALUE
                  mTmpBirthPlace        = {&tt-op}.BirthPlace:SCREEN-VALUE
                  mTmpBirthday          = {&tt-op}.Birthday:SCREEN-VALUE
                  mTmpKodRegGni         = {&tt-op}.kodreggni$:SCREEN-VALUE
                  {&tt-op}.fio$:SCREEN-VALUE              = ""
                  {&tt-op}.country-pers:SCREEN-VALUE      = ""
                  {&tt-op}.document-id:SCREEN-VALUE       = ""
                  {&tt-op}.dokum$:SCREEN-VALUE            = ""
                  {&tt-op}.cust-doc-who:SCREEN-VALUE      = ""
                  {&tt-op}.podrazd$:SCREEN-VALUE          = ""
                  {&tt-op}.Document4Date_vid:SCREEN-VALUE = ""
                  {&tt-op}.adres$:SCREEN-VALUE            = ""
                  {&tt-op}.kodreggni$:SCREEN-VALUE        = ""
                  {&tt-op}.BirthPlace:SCREEN-VALUE        = ""
                  {&tt-op}.Birthday:SCREEN-VALUE          = ""
                  .
               DISABLE
                  {&tt-op}.fio$
                  {&tt-op}.dover$
                  {&tt-op}.country-pers
                  {&tt-op}.document-id
                  {&tt-op}.dokum$
                  {&tt-op}.cust-doc-who
                  {&tt-op}.podrazd$
                  {&tt-op}.Document4Date_vid
                  {&tt-op}.adres$
                  {&tt-op}.kodreggni$
                  {&tt-op}.BirthPlace
                  {&tt-op}.Birthday
                  .
            END.
         END. /* IF work-module NE "cm" */
      END. /* ELSE mMaxAmtRub >= mSumLimit2 */
   END. /* IF fGetSetting(" */
END PROCEDURE.

/* Инструменты для работы с клиентом */
{pers.fun}

/* Сохранение клиента */
PROCEDURE SavePerson:
   DEF VAR vFlagUnk AS LOGICAL NO-UNDO.
   DEF VAR vUnk     AS CHAR    NO-UNDO.

   DEFINE BUFFER b-cust-ident FOR cust-ident.

   HIDE MESSAGE NO-PAUSE.

   DO ON ERROR UNDO, RETRY:

      DEF BUFFER person FOR person.

      CREATE person.

      ASSIGN
         person.person-id   = Get-New-Person-Id()
         person.name-last   = ENTRY(1, {&tt-op}.fio$, " ")
         person.first-names = SUBSTR({&tt-op}.fio$, LENGTH(person.name-last) + 2)
         person.country-id  = {&tt-op}.country-pers
         person.document-id = {&tt-op}.document-id
         person.document    = {&tt-op}.dokum$
         person.address[1]  = mAdrProp
         person.address[2]  = "__FORM~001"
         person.issue       = {&tt-op}.cust-doc-who
         person.birthday    = {&tt-op}.Birthday
      .
      {getflagunk.i &class-code="'person'" &flag-unk="vFlagUnk"}
      IF vFlagUnk THEN DO:
         vUnk = STRING(NewUnk("person"), GetXAttrEx("person", "УНК", "data-format")).
         UpdateSigns(person.class-code, STRING(person.person-id), "УНК", vUnk, ?).
      END.

      UpdateSigns(person.class-code, STRING(person.person-id), "Document4Date_vid", STRING({&tt-op}.Document4Date_vid), ?).
      UpdateSigns(person.class-code, STRING(person.person-id), "BirthPlace", STRING({&tt-op}.BirthPlace), ?).
      UpdateSigns(person.class-code, STRING(person.person-id), "КодРегГНИ", STRING({&tt-op}.kodreggni$), ?).
      UpdateSigns(person.class-code, STRING(person.person-id), "country-id2",person.country-id, ?).

      VALIDATE person NO-ERROR.
      IF    ERROR-STATUS:ERROR
         OR RETURN-VALUE >  "" THEN DO:
         RUN Fill-SysMes("","","-1",ERROR-STATUS:GET-MESSAGE(ERROR-STATUS:NUM-MESSAGES) + "~n" + RETURN-VALUE).
      END.
      
      FIND FIRST b-cust-ident WHERE 
                 b-cust-ident.cust-cat       =  "Ч"
             AND b-cust-ident.cust-id        =  person.person-id
             AND b-cust-ident.cust-code-type =  person.document-id
             AND b-cust-ident.cust-code      =  person.document 
         NO-ERROR.
      IF AVAIL b-cust-ident THEN
         UpdateSigns(b-cust-ident.class-code,GetSurrogate("cust-ident",ROWID(b-cust-ident)),"подразд", {&tt-op}.podrazd$, ?) NO-ERROR.

      IF VALID-HANDLE(mInstanceProp) THEN
      DO:
         RUN SetInstanceProp (mInstanceProp,"cust-id",STRING(person.person-id),OUTPUT mOk) NO-ERROR.
         RUN SetInstance("p-cust-adr",mInstanceProp,OUTPUT mOk).
         RUN DelEmptyInstance(mInstanceProp).
      END.
      RUN pers-ed.p ("person", RECID(person), 5) NO-ERROR.
      IF LASTKEY = 27 THEN UNDO, LEAVE.
      IF ERROR-STATUS:ERROR THEN
      DO:
         RUN Fill-SysMes("","","-1",ERROR-STATUS:GET-MESSAGE(ERROR-STATUS:NUM-MESSAGES)).
      END.
      ELSE
      DO:
         RUN AddAttr2TableEx("",
                             0,
                             -1,
                             "",
                             0,
                             "_person-id",
                             STRING(person.person-id)).
      END.
   END.

   RETURN.

END PROCEDURE.

/* Отображение данных выбранного клиента */
PROCEDURE DispPerson:

   DEF INPUT PARAMETER iPersonId  AS INT64  NO-UNDO.

   DEF BUFFER person      FOR person.
   DEF BUFFER bcust-ident FOR cust-ident.
   FOR FIRST person WHERE
             person.person-id = iPersonId
      NO-LOCK
      WITH FRAME {&PERS-FRAME}:
      
      ASSIGN
         mAdrProp = TRIM(person.address[1])
         {&tt-op}.Document4Date_vid:SCREEN-VALUE = ""
      .
      FIND FIRST bcust-ident WHERE 
                 bcust-ident.cust-code-type =  person.document-id 
             AND bcust-ident.cust-code      =  person.document
             AND bcust-ident.cust-cat       =  "Ч"
             AND bcust-ident.cust-id        =  person.person-id
         NO-LOCK NO-ERROR.
      IF AVAIL bcust-ident THEN
         ASSIGN
            {&tt-op}.podrazd$:SCREEN-VALUE = GetXAttrValueEx("cust-ident",GetSurrogate("cust-ident",ROWID(bcust-ident)),"подразд","")
         .

      if index(PROGRAM-NAME(1),"vok-val.p") > 0 then do:

         ASSIGN
            {&tt-op}.country-pers:SCREEN-VALUE        = GetXAttrValue("person",STRING(person.person-id),"country-id2")
         .
         country-pers:label = "ГРАЖДАНСТВО" .
      end.
      else do:
         ASSIGN
            {&tt-op}.country-pers:SCREEN-VALUE        = person.country-id
         .
         country-pers:label = "РЕЗИДЕНТНОСТЬ" .
      end.


      ASSIGN
         {&tt-op}.fio$:SCREEN-VALUE                = person.name-last + " "
                                                      + person.first-names
/*
         {&tt-op}.country-pers:SCREEN-VALUE        = person.country-id
*/
         {&tt-op}.country-pers:SCREEN-VALUE        = GetXAttrValue("person",STRING(person.person-id),"country-id2")
         {&tt-op}.document-id:SCREEN-VALUE         = person.document-id
         {&tt-op}.dokum$:SCREEN-VALUE              = person.document
         {&tt-op}.Document4Date_vid:SCREEN-VALUE   = GetXAttrValue("person",STRING(person.person-id),"Document4Date_vid")
         {&tt-op}.adres$:SCREEN-VALUE              = fGetStrAdr(mAdrProp)
         {&tt-op}.kodreggni$:SCREEN-VALUE          = GetXAttrValue("person",STRING(person.person-id),"КодРегГНИ")
         {&tt-op}.Birthday:SCREEN-VALUE            = STRING(person.birthday,"99/99/9999")
         {&tt-op}.BirthPlace:SCREEN-VALUE          = GetXAttrValue("person",STRING(person.person-id),"BirthPlace")
         {&tt-op}.cust-doc-who:SCREEN-VALUE        = ENTRY(1,person.issue)
         .
      ASSIGN
         {&tt-op}.fio$
         {&tt-op}.country-pers
         {&tt-op}.document-id
         {&tt-op}.dokum$
         {&tt-op}.Document4Date_vid
         {&tt-op}.adres$
         {&tt-op}.kodreggni$
         {&tt-op}.Birthday
         {&tt-op}.BirthPlace
         {&tt-op}.cust-doc-who
         {&tt-op}.podrazd$
         .
      IF {&tt-op}.Document4Date_vid:SCREEN-VALUE =  "" 
            AND (AVAILABLE bcust-ident 
                AND  (bcust-ident.close-date     =  ? 
                   OR bcust-ident.close-date     >= mOpDate)
                AND   bcust-ident.cust-code-type =  {&tt-op}.document-id:SCREEN-VALUE
                AND   bcust-ident.cust-code      =  {&tt-op}.dokum$:SCREEN-VALUE) THEN
      DO:
         ASSIGN {&tt-op}.Document4Date_vid:SCREEN-VALUE = STRING(bcust-ident.open-date).
         ASSIGN {&tt-op}.Document4Date_vid.
      END.
      APPLY "LEAVE" TO {&tt-op}.document-id IN FRAME {&PERS-FRAME}.

      APPLY "LEAVE" TO {&tt-op}.dokum$.
      IF RETURN-VALUE = {&RET-ERROR} THEN
         RETURN ERROR {&RET-ERROR}.
   END.

   RETURN.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Ищет клиента по Person или договору
  Parameters:  iRowMenu - Пункт меню (Клиент ВОК,Клиент банка,Договор)
               iSrchFld - Наименования поля, в котором ищем клиента по F1
  Notes:       договора отфильтровываются по тому, что если в введено iSrchFld  
------------------------------------------------------------------------------*/
PROCEDURE FindClient:
   /* Пункт меню (Клиент ВОК,Клиент банка,Не клиент банка,Договор) */
   DEFINE INPUT PARAMETER iRowMenu  AS CHAR    NO-UNDO.
   /* Поле договора */
   DEFINE INPUT PARAMETER iHCdCode  AS HANDLE  NO-UNDO.
   /* Поле, в котором нажали F1 */
   DEFINE INPUT PARAMETER iHSrchFl  AS HANDLE  NO-UNDO.

   /* Номер договора */
   DEF VAR vCont-Code  AS CHAR  NO-UNDO.
   /* Номер счета */
   DEF VAR vAcct       AS CHAR  NO-UNDO.
   /* Параметры browseld */
   DEF VAR vBrPrms     AS CHAR  NO-UNDO  EXTENT 4.

   CASE iRowMenu:
      WHEN "Клиент ВОК"  THEN
         ASSIGN
            vBrPrms[1] = "person"
            vBrPrms[2] = "crClass-Code"
            vBrPrms[3] = "КлиентВок"
            vBrPrms[4] = "crClass-Code".
      WHEN "Клиент банка" THEN
      DO:
         ASSIGN
            vBrPrms[1] = "person".
         IF vChRole <> "" THEN
         DO:
            {empty ttperson}
            RUN CreateTTPerson (vChSreenAcct, OUTPUT TABLE ttperson).
            FIND FIRST ttperson NO-LOCK NO-ERROR.
            IF AVAILABLE ttperson THEN
               ASSIGN
                  vBrPrms[2] = "FilterTable" + CHR(1) + "crClass-Code"
                  vBrPrms[3] = STRING(TEMP-TABLE ttperson:HANDLE) + CHR(1) + "*"
                  vBrPrms[4] = ""
                  .
            ELSE 
               ASSIGN
                  vBrPrms[1] = "person".
         END.
      END.
      WHEN "Не клиент банка" THEN
      DO:
         ASSIGN
            vBrPrms[1] = "person"
            vBrPrms[2] = "crClass-Code"
            vBrPrms[3] = "ImaginNoClient"
            vBrPrms[4] = "crClass-Code".
      END.
      WHEN "Договор"      THEN
         ASSIGN
            vBrPrms[1] = "dep_person"
            vBrPrms[2] = ""
            vBrPrms[3] = ""
            vBrPrms[4] = "".
   END CASE. /* iRowMenu */

   pick-value = "".
   
   RUN browseld.p (vBrPrms[1],
                   vBrPrms[2],  
                   vBrPrms[3],
                   vBrPrms[4],
                   iLevel
                  ).
   /* Если что-то выбрали */

   IF {assigned pick-value} AND
      (LASTKEY = 13 OR LASTKEY = 10) THEN
   DO:
      IF iRowMenu <> "Договор" THEN
         mPersonId = INT64(pick-value) NO-ERROR. 
      ELSE
      DO:
         ASSIGN
            vCont-Code = pick-value
            mPersonId  = INT64(GetBufferValue("loan",
                                            "WHERE loan.contract = 'dps'
                                               AND loan.cont-code = "
                                               + QUOTER(vCont-Code + IF shmode THEN ("@" + ShFilial) ELSE ""),
                                            "cust-id"
                                           )
                            )
         NO-ERROR.

         &IF DEFINED(tt-op-entry) &THEN
            RUN GetBaseAcct IN h_dps ("dps",
                                      vCont-Code + (IF shmode THEN ("@" + ShFilial) ELSE ""),
                                      mOpDate,
                                      OUTPUT vAcct
                                     ).
            IF {assigned vAcct} THEN
            DO:
               DISPLAY
                  vAcct @ {&tt-op-entry}.acct-cr
               WITH FRAME {&PERS-FRAME}.
               APPLY "LEAVE" TO {&tt-op-entry}.acct-cr.
            END.
         &ENDIF
      END.

      IF mPersonId <> ? THEN
      DO:
         RUN DispPerson(mPersonId).
         RUN AddAttr2TableEx("",
                             0,
                             -1,
                             "",
                             0,
                             "_person-id",
                             STRING(mPersonId)).
      END.
            
      /* Если в форме есть поле с номером договора, то выводим его */
      IF VALID-HANDLE(iHCdCode) AND
         iHCdCode:VISIBLE THEN
      DO:
         IF vCont-Code = "" THEN
            RUN FindLoanFromPerson(mPersonId, OUTPUT vCont-Code).

         iHCdCode:SCREEN-VALUE = vCont-Code.

         APPLY "LEAVE" TO iHCdCode.
      END.
   END.


   RETURN.

END PROCEDURE. /* FindClient */

/*------------------------------------------------------------------------------
  Purpose:     Ищет договор клиента о вкладе
  Parameters:  iPersonId - Код клиента
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE FindLoanFromPerson:
   /* Код клиента */
   DEF INPUT  PARAMETER iPersonId  AS INT64   NO-UNDO.
   /* Номер договора клиента */
   DEF OUTPUT PARAMETER oCondCode  AS CHAR  NO-UNDO.

   CASE CountObjects ("loan",
                      "loan.contract = 'dps' AND
                      loan.cust-id = " + QUOTER(STRING(iPersonId))
                     ):
      WHEN 0 THEN
      DO:
         MESSAGE
            "У клиента нет ни одного договора ""Частных вкладов"""
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

         RETURN ERROR {&RET-ERROR}.
      END.

      WHEN 1 THEN
         oCondCode = GetBufferValue("loan",
                                    "WHERE loan.contract = 'dps' AND
                                           loan.cust-id = " + QUOTER(STRING(iPersonId)),
                                    "cont-code"
                                   ).
      OTHERWISE
      DO:
         /* Частные вклады клиента */
         RUN browseld.p ("dep_person",
                         "cust-id",
                         STRING(iPersonId),
                         "cust-id",
                         iLevel
                        ).

         IF (LASTKEY = 13 OR LASTKEY = 10) AND
            {assigned pick-value}
         THEN
            oCondCode = pick-value.
      END.
   END CASE.

   IF oCondCode = ? THEN
      oCondCode = "".

END PROCEDURE. /* FindLoanFromPerson */

&ENDIF /* IF DEFINED(initframe) */
/* $LINTFILE='vok-pers.i' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='stre' */
/* $LINTDATE='20/04/2017 13:14:15.386+03:00' */
/*prosigneEj7nXesKQApGvoCJuk/mg*/