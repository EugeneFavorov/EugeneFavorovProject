{intrface.get flt}

&Scoped-define SELF-NAME tt-opb-pay.adres$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.adres$ TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-opb-pay.adres$ IN FRAME fMain /* ����� */
DO:
   RETURN /*NO-APPLY*/.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.adres$ TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.adres$ IN FRAME fMain /* ����� */
DO:
   RUN pers-adr.p(INPUT-OUTPUT tt-opb-pay.adres$).
   SELF:SCREEN-VALUE = tt-opb-pay.adres$.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-opb-pay.amt-rub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.amt-rub TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-opb-pay.amt-rub IN FRAME fMain /* ����� ������� */
DO:
   DEFINE VAR vCursorOffset AS INT64 NO-UNDO.
   DEFINE VAR vMinComission LIKE op-entry.amt-rub NO-UNDO.
   DEFINE VAR vMaxComission LIKE op-entry.amt-rub NO-UNDO.
   DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN
         mTarif                    = 0.0
         mTarifSign                = ""
         tt-opb-pay.amt-rub-commis = 0.0
      .
      ASSIGN
         tt-opb-pay.amt-rub
      .
      
      &IF DEFINED(opb-pay-loan) &THEN
         IF mPeniRuchV THEN
            ASSIGN
               tt-opb-pay.pnsummapeni$
            .
         ELSE
            ASSIGN
               tt-opb-pay.pntarifpeni$
            .
      &ENDIF


      IF {assigned tt-opb-pay.pnkomplat$} THEN DO:
         RUN GET_HEAD_COMM(tt-opb-pay.pnkomplat$,"",tt-opb-pay.amt-rub,0,BUFFER commission).
         IF AVAIL commission THEN DO:
            RUN GET_COMM_BUF (commission.commission, ?, "", "", tt-opb-pay.amt-rub, 0, tt-opb-pay.op-date, BUFFER comm-rate).
            IF AVAIL comm-rate THEN DO:
               mTarif = comm-rate.rate-comm.
               IF comm-rate.rate-fixed
               THEN ASSIGN
                  mTarifSign = "="
                  tt-opb-pay.amt-rub-commis = mTarif
               .
               ELSE DO:
                  mTarifSign = "%".
                  tt-opb-pay.amt-rub-commis = ROUND((mTarif / 100) * tt-opb-pay.amt-rub,2).
                  IF GetClassObj(BUFFER comm-rate:HANDLE) = "comm-rate-pn" THEN DO:
                     vMinComission = DECIMAL(GetXAttrValueEx("comm-rate",GetSurrogateBuffer("comm-rate",(BUFFER comm-rate:HANDLE)),"�������",?)) NO-ERROR.
                     vMaxComission = DECIMAL(GetXAttrValueEx("comm-rate",GetSurrogateBuffer("comm-rate",(BUFFER comm-rate:HANDLE)),"���ᇭ��",?)) NO-ERROR.
                     IF  vMinComission             > 0
                     AND tt-opb-pay.amt-rub-commis < vMinComission
                     THEN tt-opb-pay.amt-rub-commis = vMinComission.
                     IF  vMaxComission             > 0
                     AND tt-opb-pay.amt-rub-commis > vMaxComission
                     THEN tt-opb-pay.amt-rub-commis = vMaxComission.
                  END.
               END.
            END.
         END.
      END.

      &IF DEFINED(opb-pay-loan) &THEN
         IF NOT mPeniRuchV THEN
         IF tt-opb-pay.pntarifpeni$ = ?
         OR tt-opb-pay.pntarifpeni$ = 0.0
         THEN tt-opb-pay.pnsummapeni$ = 0.0.
         ELSE DO:
            tt-opb-pay.pnsummapeni$ = ROUND(tt-opb-pay.amt-rub * (tt-opb-pay.pntarifpeni$ / 100),2).
            IF tt-opb-pay.pnsummapeni$ = ? THEN tt-opb-pay.pnsummapeni$ = 0.
         END.
      &ENDIF

      mItogo = tt-opb-pay.amt-rub
             + tt-opb-pay.amt-rub-commis
             &IF DEFINED(opb-pay-loan) &THEN
             + tt-opb-pay.pnsummapeni$
             &ENDIF
      .
      vCursorOffset = SELF:CURSOR-OFFSET.
      DISPLAY
         tt-opb-pay.amt-rub-commis
         mTarif
         mTarifSign
         mItogo
         &IF DEFINED(opb-pay-loan) &THEN
            tt-opb-pay.pnsummapeni$
         &ENDIF
      .
      SELF:CURSOR-OFFSET = vCursorOffset.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-opb-pay.bik$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.bik$ TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.bik$ IN FRAME fMain /* ��� ����� */
DO:
   DEFINE BUFFER banks FOR banks.
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
      RUN browseld.p ("banks",
                      "client" + CHR(1) + "return"  + CHR(1) + "SetFirstFrm",
                      "?"      + CHR(1) + "bank-id" + CHR(1) + "4",
                      "",
                      4).
      IF LASTKEY = 10
      AND pick-value <> ? THEN DO:
         FIND FIRST banks WHERE banks.bank-id = INT64(pick-value) NO-LOCK NO-ERROR.
         IF AVAIL banks THEN DO:
            {getcode.i banks "���-9"}
            IF AVAIL banks-code THEN DO:
               tt-opb-pay.bik$ = INT64(banks-code.bank-code) NO-ERROR.
               DISPLAY tt-opb-pay.bik$.
               APPLY "VALUE-CHANGED" TO SELF.
            END.
         END.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.bik$ TERMINAL-SIMULATION
ON VALUE-CHANGED OF tt-opb-pay.bik$ IN FRAME fMain /* ��� ����� */
DO:
   DEFINE BUFFER banks-code FOR banks-code.
   DEFINE BUFFER banks      FOR banks.
   DEFINE BUFFER banks-corr FOR banks-corr.
   DEFINE BUFFER banks2     FOR banks.
   DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN tt-opb-pay.bik$.
      mBankName = "". mCorrAcct = "".
      FIND FIRST banks-code WHERE banks-code.bank-code-type = "���-9"
                              AND banks-code.bank-code      = STRING(tt-opb-pay.bik$,"999999999")
      NO-LOCK NO-ERROR.
      IF AVAIL banks-code THEN DO:
         FIND FIRST banks WHERE banks.bank-id = banks-code.bank-id NO-LOCK NO-ERROR.
         IF AVAIL banks THEN DO:
            mBankName = banks.short-name.
            FIND FIRST banks-corr WHERE banks-corr.bank-corr = banks.bank-id
                                    AND CAN-FIND(FIRST banks2 OF banks-corr WHERE banks2.flag-rkc = YES NO-LOCK)
            NO-LOCK NO-ERROR.
            IF AVAIL banks-corr THEN DO:
               mCorrAcct = banks-corr.corr-acct.
            END.
         END.
      END.
      DISPLAY mBankName mCorrAcct.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-opb-pay.country-pers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.country-pers TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.country-pers IN FRAME fMain /* ��� ������ */
DO:
   DO TRANSACTION WITH FRAME {&MAIN-FRAME}:
      RUN browseld.p("country", "", "", "", 4).
      IF  {assigned pick-value}
      AND (   LASTKEY = 13
           OR LASTKEY = 10)
      THEN DO:
         tt-opb-pay.country-pers = pick-value. 
         DISPLAY tt-opb-pay.country-pers.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mDocumentIdName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mDocumentIdName TERMINAL-SIMULATION
ON ANY-PRINTABLE OF mDocumentIdName IN FRAME fMain /* ��� ��������� */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mDocumentIdName TERMINAL-SIMULATION
ON F1 OF mDocumentIdName IN FRAME fMain /* ��� ��������� */
DO:
   IF iMode = {&MOD_VIEW} THEN RETURN.
   DO TRANSACTION:
      RUN codelay.p ("�������","�������","", 4). /* �������� ��� ��������� */
      IF LASTKEY = 10 THEN DO WITH FRAME {&MAIN-FRAME}:
         tt-opb-pay.document-id = pick-value.
         mDocumentIdName = GetCodeName("�������",tt-opb-pay.document-id).
         IF mDocumentIdName = ? THEN mDocumentIdName = "".
         DISPLAY mDocumentIdName.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-opb-pay.name-last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.name-last TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.name-last IN FRAME fMain /* ������� */
DO:
   DEFINE VAR vBrPrms       AS CHAR NO-UNDO EXTENT 4.
   DEFINE VAR vIdPerson     LIKE person.person-id NO-UNDO.

   RUN messmenu.p (10,
                   "",
                   "",
                   "����ﭭ� ���⥫�騪�,�����᪨� ���"
                  ).
   IF LASTKEY = 27 THEN RETURN NO-APPLY.

   CASE INT64(pick-value):
      WHEN 1 THEN ASSIGN /* ����ﭭ� ���⥫�騪� */
         vBrPrms[1] = "person"
         vBrPrms[2] = "crClass-Code~001crCreate~001SetFirstFrm"
         vBrPrms[3] = "cust-pn~001cust-pn~0015"
         vBrPrms[4] = "crClass-Code"
      .
      WHEN 2 THEN ASSIGN /* �����᪨� ��� */
         vBrPrms[1] = "person"
         vBrPrms[2] = "SetFirstFrm~001crClass-Code"
         vBrPrms[3] = "5~001*"
      .
   END CASE.

   pick-value = "".
   RUN browseld.p (vBrPrms[1],
                   vBrPrms[2],
                   vBrPrms[3],
                   vBrPrms[4],
                   iLevel
                  ).
   /* �᫨ ��-� ��ࠫ� */
   IF  {assigned pick-value}
   AND (   LASTKEY = 13
        OR LASTKEY = 10)
   THEN DO:
      vIdPerson = ?.
      vIdPerson = INT64(pick-value) NO-ERROR.
      IF vIdPerson > 0 THEN DO:
         RUN DisplayPerson (vIdPerson).

         /* �饬 ��易⥫쭮� ������������� ���� */
         mNonFilledHdl = ?.
         RUN FindMandButNonFilledWidg (INPUT-OUTPUT mNonFilledHdl, ?, ?, NO).
         IF VALID-HANDLE(mNonFilledHdl) THEN DO:
            APPLY "ENTRY" TO mNonFilledHdl.
            RETURN NO-APPLY.
         END.
         ELSE APPLY "TAB" TO SELF.
      END.
   END.

   &IF  DEFINED(opb-pay-budg) > 0 &THEN

      find first  code WHERE code.class  EQ "�������"       
                         AND code.parent EQ "�������"  
                         and code.name   EQ mDocumentIdName
                         no-lock no-error.
      if avail code then do:
         if tt-opb-pay.dokum$ <> "" then do:
            tt-opb-pay.poknd$:SCREEN-VALUE =  code.description[3] + ";" + replace(tt-opb-pay.dokum$," ",""). 
         end.
         else do:
            tt-opb-pay.poknd$:SCREEN-VALUE =  "0". 
         end.
      end.
      mIDPerson = vIdPerson.
   &ENDIF
   
   {&END_BT_F1}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tt-opb-pay.first-names
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.first-names TERMINAL-SIMULATION

ON ENTER OF tt-opb-pay.first-names IN FRAME fMain 
DO:        

   DEFINE VAR vBrPrms       AS CHAR NO-UNDO EXTENT 4.
   DEFINE VAR vIdPerson     LIKE person.person-id NO-UNDO.

   ASSIGN
      vBrPrms[1] = "person"
      vBrPrms[2] = "name-last"                       + CHR(1) + "first-names"                       + CHR(1) + "SetFirstFrm" + CHR(1) + "crClass-Code"
      vBrPrms[3] = tt-opb-pay.name-last:SCREEN-VALUE + CHR(1) + tt-opb-pay.first-names:SCREEN-VALUE + CHR(1) + "5"           + CHR(1) + "*"
      vBrPrms[4] = ""
   .

   IF (dokum$:SIDE-LABEL-HANDLE:DCOLOR = GetDCOLOR("green"))
   OR CAN-DO(mLimitVP,tt-opb-pay.pnkodvidaplatewza$:SCREEN-VALUE)
   THEN ASSIGN
      vBrPrms[2] = vBrPrms[2] + CHR(1) + "document"
      vBrPrms[3] = vBrPrms[2] + CHR(1) + "!���*,*"
   .

   RUN browseld.p (vBrPrms[1],
                   vBrPrms[2],
                   vBrPrms[3],
                   vBrPrms[4],
                   iLevel
                  ).

   IF  {assigned pick-value}
   AND (   LASTKEY = 13
        OR LASTKEY = 10)
   THEN DO:

      vIdPerson = ?.
      vIdPerson = INT64(pick-value) NO-ERROR.
      IF vIdPerson > 0 THEN DO:
         RUN DisplayPerson (vIdPerson).
         /* �饬 ��易⥫쭮� ������������� ���� */
         mNonFilledHdl = ?.
         RUN FindMandButNonFilledWidg (INPUT-OUTPUT mNonFilledHdl, ?, ?, NO).
         IF VALID-HANDLE(mNonFilledHdl) THEN DO:
            APPLY "ENTRY" TO mNonFilledHdl.
            RETURN NO-APPLY.
         END.
         ELSE APPLY "TAB" TO SELF.
      END.

   END.
   RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-opb-pay.pnkodvidaplatewza$
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.pnkodvidaplatewza$ TERMINAL-SIMULATION
ON ANY-PRINTABLE OF tt-opb-pay.pnkodvidaplatewza$ IN FRAME fMain /* ��� ������� */
DO:
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.pnkodvidaplatewza$ TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.pnkodvidaplatewza$ IN FRAME fMain /* ��� ������� */
DO:
   IF iMode = {&MOD_VIEW} THEN RETURN.
   DO TRANSACTION:
      RUN browseld.p ("��������⥦�",
                      "class"        + CHR(1) + "misc[3]",
                      "��������⥦�" + CHR(1) + "���",
                      "class",
                      4).
      IF  LASTKEY = 10
      AND {assigned pick-value}
      THEN DO WITH FRAME {&MAIN-FRAME}:
         tt-opb-pay.pnkodvidaplatewza$ = pick-value.
         DISPLAY tt-opb-pay.pnkodvidaplatewza$.
      END.
   END.
   {&END_BT_F1}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(opb-pay) > 0 OR DEFINED(opb-pay-budg) > 0 &THEN

&Scoped-define SELF-NAME tt-opb-pay.details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.details TERMINAL-SIMULATION
ON ENTRY OF tt-opb-pay.details IN FRAME fMain /* details */
DO:
   DEFINE VAR vDetails LIKE op.details NO-UNDO.
   vDetails = SELF:SCREEN-VALUE.
   RUN CreateFrmFields (?,"opreq", "amt-cur", STRING(tt-opb-pay.amt-rub)).
   mDetailsOk = YES.
   RUN ProcessDetails (?, INPUT-OUTPUT vDetails).
   SELF:SCREEN-VALUE = vDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.details TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.details IN FRAME fMain /* details */
DO:
   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION:
      RUN browseldvar.p ("description",
                         "�������⍂��",
                          "class"        + CHR(1) + "misc[1]",
                          "�������⍂��" + CHR(1) + tt-opb-pay.pnkodvidaplatewza$,
                          "class"        + CHR(1) + "misc[1]",
                          iLevel + 1
                        ).
      IF  LASTKEY = 10
      AND {assigned pick-value}
      THEN DO:
         pick-value = ENTRY(1,pick-value,CHR(127)).
         IF {assigned pick-value}
         THEN SELF:SCREEN-VALUE = pick-value.
      END.
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(opb-pay-loan) &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.pnswcet$ TERMINAL-SIMULATION
ON F1 OF tt-opb-pay.pnswcet$ IN FRAME fMain 
DO:

   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO TRANSACTION:

      IF NOT {assigned mClassif} THEN DO:
         RUN Fill-SysMes("","","-1","�� ��ࠬ��� ���⥦� �� ����� �����䨪��� ��⮢ ���⥫�騪��!").
         RETURN NO-APPLY {&RET-ERROR}.
      END.

      RUN codelay.p (mClassif,mClassif,"����� ������������", 4). 
      IF LASTKEY = 10
      AND {assigned pick-value}
      THEN DO:
         pick-value = ENTRY(1,pick-value,CHR(127)).
         IF {assigned pick-value}
         THEN SELF:SCREEN-VALUE = pick-value.
      END. 
   END.
   {&END_BT_F1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-opb-pay.pnswcet$ TERMINAL-SIMULATION
ON LEAVE OF tt-opb-pay.pnswcet$ IN FRAME fMain 
DO:

   IF iMode = {&MOD_ADD}
   OR iMode = {&MOD_EDIT}
   THEN DO:

      IF {assigned tt-opb-pay.pnswcet$:SCREEN-VALUE} THEN DO:

         FIND FIRST code WHERE
                    CODE.class EQ mClassif
                AND CODE.code  EQ tt-opb-pay.pnswcet$:SCREEN-VALUE
                    NO-LOCK NO-ERROR.
         IF AVAIL(CODE) THEN 
         ASSIGN
            tt-opb-pay.name-last:SCREEN-VALUE   = ENTRY(1,CODE.name," ") 
            tt-opb-pay.first-names:SCREEN-VALUE = SUBSTRING(CODE.NAME,GetPosEntry(CODE.NAME,2," ")) WHEN  NUM-ENTRIES(CODE.NAME," ") GE 2
            tt-opb-pay.adres$:SCREEN-VALUE      = CODE.val.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


ON LEAVE OF tt-opb-pay.pnkodvidaplatewza$,
            tt-opb-pay.cont-cli,
            tt-opb-pay.pnpoluwcatelw#naimenovanie$,
            tt-opb-pay.acct,
            tt-opb-pay.pnpoluwcatelw#inn$,
            tt-opb-pay.pnpoluwcatelw#kpp$,
            tt-opb-pay.bik$,
            tt-opb-pay.first-names,
            tt-opb-pay.name-last,
            tt-opb-pay.adres$,
            tt-opb-pay.Birthday,
            tt-opb-pay.BirthPlace,
            tt-opb-pay.country-pers,
            mDocumentIdName,
            tt-opb-pay.dokum$,
            tt-opb-pay.cust-doc-who,
            tt-opb-pay.Document4Date_vid,
            tt-opb-pay.pninn$,
            tt-opb-pay.pntelefon$,
            &IF DEFINED(opb-pay-loan) &THEN
            tt-opb-pay.pnpokaznawc$,
            tt-opb-pay.pnpokazkon$,
            tt-opb-pay.pnperiod$,
            tt-opb-pay.pntarifpeni$,
            tt-opb-pay.pnprosrowcka$,
            &ENDIF
            &IF DEFINED(opb-pay) > 0 OR DEFINED(opb-pay-budg) > 0 &THEN
            tt-opb-pay.details,
            &ENDIF
            &IF DEFINED(opb-pay) &THEN
            tt-opb-pay.pnswcet$,
            &ENDIF
            &IF DEFINED(opb-pay-budg) &THEN
            tt-opb-pay.pokdd$,
            tt-opb-pay.poknd$,
            tt-opb-pay.poknp$,
            tt-opb-pay.pokop$,
            tt-opb-pay.pokst$,
            tt-opb-pay.poktp$,
            tt-opb-pay.kbk$,
            tt-opb-pay.okato-nalog$,
            &ENDIF
            tt-opb-pay.amt-rub
   IN FRAME {&MAIN-FRAME}
DO:
   DEFINE VAR vChkResult  AS INT64   NO-UNDO. /* ������� �஢�ન ����� ���㬥�� */
   DEFINE VAR vOver       AS INT64   NO-UNDO.
   DEFINE VAR vPNForma    AS CHARACTER NO-UNDO.
   DEFINE VAR vDocClass   AS CHARACTER NO-UNDO.
   &IF DEFINED(opb-pay-budg) &THEN
   DEFINE VAR vDate       AS DATE      NO-UNDO.
   DEFINE VAR vPokNpCode  AS CHARACTER NO-UNDO.
   DEFINE VAR vPokNp1Int  AS INT64   NO-UNDO.
   DEFINE VAR vPokNp2     AS CHARACTER NO-UNDO.
   DEFINE VAR vPokNp2Int  AS INT64   NO-UNDO.
   DEFINE VAR vPokNp2Max  AS INT64   NO-UNDO.
   DEFINE VAR vPokNp3     AS CHARACTER NO-UNDO.
   DEFINE VAR vBICINN     AS CHARACTER NO-UNDO.
   &ENDIF
   DEFINE BUFFER xxcode FOR code.
   DEFINE BUFFER xxattr FOR xattr.

   IF  NUM-ENTRIES(PROGRAM-NAME(2)," ") = 2
   AND ENTRY(1,PROGRAM-NAME(2)," ")     = "Local_Go"
   AND ENTRY(2,PROGRAM-NAME(2)," ")     = THIS-PROCEDURE:FILE-NAME
   THEN RETURN.

   {&BEG_BT_LEAVE}

   &IF DEFINED(opb-pay-budg) &THEN
   IF  SELF:NAME = "poknp$"
   AND {assigned TRIM(SELF:INPUT-VALUE)}
   AND TRIM(SELF:INPUT-VALUE) <> "?" THEN DO:


      IF SELF:INPUT-VALUE <> "0" THEN DO:
         vPokNpCode = GetCode("���:��", ENTRY(1,SELF:INPUT-VALUE,".")).
         IF INDEX(SELF:INPUT-VALUE,".") > 0 THEN DO:

            IF vPokNpCode = ? THEN DO:
               vPokNp1Int = INT64(ENTRY(1,SELF:INPUT-VALUE,".")) NO-ERROR.
               IF ERROR-STATUS:ERROR
               OR vPokNp1Int < 0
               OR vPokNp1Int > 31
               THEN DO:
                  RUN Fill-SysMes("","","-1","���祭��� ���� ����� ���� ���� (������ 0)~n��� ���祭�� �����䨪��� '���:��' !").
                  RETURN NO-APPLY {&RET-ERROR}.
               END.
            END.

            ASSIGN
               vPokNp2 = ENTRY(2,SELF:INPUT-VALUE,".")
               vPokNp3 = ""
               vPokNp3 = ENTRY(3,SELF:INPUT-VALUE,".") WHEN NUM-ENTRIES(SELF:INPUT-VALUE,".") > 2
            .
            
            IF LENGTH(vPokNP2) <> 2 THEN DO:
               RUN Fill-SysMes("","","-1","�� �ࠢ��쭮 ����� �����").
               RETURN NO-APPLY {&RET-ERROR}.
            END.

            vPokNp2Max = (IF vPokNpCode = ? THEN 12 ELSE INT64(vPokNpCode)).
            vPokNp2Int = INT64(vPokNP2) NO-ERROR.
            IF ERROR-STATUS:ERROR
            OR vPokNp2Int < 0
            OR vPokNp2Int > vPokNp2Max
            THEN DO:
               RUN Fill-SysMes("","","-1","���祭�� ����� ������ ���� �� 00 �� " + STRING(vPokNp2Max,"99")).
               RETURN NO-APPLY {&RET-ERROR}.
            END.
            
            IF LENGTH(vPokNP3) <> 4 THEN DO:
               RUN Fill-SysMes("","","-1","�� �ࠢ��쭮 ����� ���").
               RETURN NO-APPLY {&RET-ERROR}.
            END.
            
            IF vPokNpCode = ? THEN DO: /* ���祭�� ���� ��� � �����䨪��� - �� ������ ���� ���*/
               STRING(DATE(SELF:INPUT-VALUE)) NO-ERROR.
               IF  ERROR-STATUS:ERROR
               AND {assigned SELF:INPUT-VALUE}
               THEN DO:
                  RUN Fill-SysMes("","","-1","���ࠢ��쭮 ������� ��� ���������� ��ਮ�� !").
                  RETURN NO-APPLY {&RET-ERROR}.
               END.
            END.
            ELSE DO:
               INT64(vPokNP3) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  RUN Fill-SysMes("","","-1","���ࠢ��쭮 ������ ���").
                  RETURN NO-APPLY {&RET-ERROR}.
               END.
            END.
         END.
      END.
   END.
   ELSE
   IF  SELF:NAME = "okato-nalog$"
   AND {assigned TRIM(SELF:INPUT-VALUE)}
   AND TRIM(SELF:INPUT-VALUE) <> "?" THEN DO:
      mNalCheck = check148n-okatonalog(TRIM(SELF:INPUT-VALUE)).
      IF {assigned mNalCheck} THEN DO:
         RUN Fill-SysMes("","","-1",mNalCheck).
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      IF (TRIM(SELF:INPUT-VALUE) <> "0" AND LENGTH(TRIM(SELF:INPUT-VALUE)) NE 8 /* AND LENGTH(TRIM(SELF:INPUT-VALUE)) NE 11 */ ) 
      OR TRIM(TRIM(TRIM(SELF:INPUT-VALUE),"1234567890")) NE ""
      THEN DO:
         RUN Fill-SysMes("","","-1","����� ���� 105 (�����) ������ ���� 8 ���.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   ELSE
   IF  SELF:NAME = "pokdd$"
   AND {assigned TRIM(SELF:INPUT-VALUE)}
   AND TRIM(SELF:INPUT-VALUE) <> "?" THEN DO:
      IF SELF:INPUT-VALUE <> "0" THEN DO:
         vDate = DATE(SELF:INPUT-VALUE) NO-ERROR.
         IF TRIM(TRIM(TRIM(SELF:INPUT-VALUE),"1234567890")) <> ""
         OR vDate = ? THEN DO:
            RUN Fill-SysMes("","","-1","���� " + SELF:LABEL + " ������ ���� ��⮩ ��� 0").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
         SELF:SCREEN-VALUE = STRING(vDate,"99/99/9999").
      END.
   END.
   ELSE
   &ENDIF

   IF SELF:NAME = "first-names"
   OR SELF:NAME = "name-last" THEN DO:
      ASSIGN tt-opb-pay.first-names tt-opb-pay.name-last.
      tt-opb-pay.fio$ = TRIM(tt-opb-pay.name-last + " " + tt-opb-pay.first-names).
   END.

   IF (    SELF:NAME = "mDocumentIdName"
       AND NOT {assigned tt-opb-pay.document-id})
   OR (    SELF:NAME <> "mDocumentIdName"
       AND (   NOT {assigned TRIM(SELF:INPUT-VALUE)}
           OR TRIM(SELF:INPUT-VALUE) = "?"))
   OR (SELF:NAME = "dokum$"  AND TRIM(SELF:INPUT-VALUE) BEGINS "���")
       THEN DO:

      IF tt-opb-pay.NewReceiver = YES THEN DO:
         IF CAN-DO("acct,bik$,pnpoluwcatelw#naimenovanie$",SELF:NAME) THEN DO:
            RUN Fill-SysMes("",
                            "",
                            "-1",
                            "���� ४����� ~"" + SELF:LABEL + "~" ��易⥫��!").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.

      IF CAN-DO("country-pers,cust-doc-who,first-names,name-last,adres$,dokum$,Document4Date_vid,Birthday",SELF:NAME)
      OR SELF:NAME = "mDocumentIdName" THEN DO:
         ASSIGN tt-opb-pay.amt-rub tt-opb-pay.pnkodvidaplatewza$.
         IF tt-opb-pay.amt-rub >= mSumLimit THEN vOver = 1.
         IF  tt-opb-pay.amt-rub >= mSumLimitSP
         AND (   CAN-DO("cust-doc-who,first-names,name-last,adres$,dokum$,Document4Date_vid",SELF:NAME)
              OR SELF:NAME = "mDocumentIdName") THEN DO:
            IF  vOver      = 1
            AND mSumLimit >= mSumLimitSP
            THEN vOver = 1.
            ELSE vOver = 2.
         END.
         IF vOver > 0 THEN DO:
            RUN Fill-SysMes("","","-1",
                            "�� �����⢫���� ����権 �� �㬬� ��� " 
                            + TRIM(STRING((IF vOver = 1 THEN mSumLimit ELSE mSumLimitSP),">>,>>>,>>>,>>9.99")) + " ��." + CHR(10) +
                            "�� ����� ����樨 ��� �� ����� ��" + CHR(10) +
                            "���� ४����� ~"" + SELF:LABEL + "~" ��易⥫��!").
            RETURN NO-APPLY {&RET-ERROR}.
         END.
         ELSE DO:
            IF CAN-DO(mLimitVP,tt-opb-pay.pnkodvidaplatewza$)
            AND NOT (SELF:NAME = "dokum$"  AND TRIM(SELF:INPUT-VALUE) BEGINS "���")
            AND (   CAN-DO("cust-doc-who,first-names,name-last,adres$,dokum$,Document4Date_vid",SELF:NAME)
                 OR SELF:NAME = "mDocumentIdName") THEN DO:
               RUN Fill-SysMes("","","-1",
                               "��� ���� ���⥦� " + tt-opb-pay.pnkodvidaplatewza$ + " " +
                               "���� ४����� ~"" + SELF:LABEL + "~" ��易⥫��!").
               RETURN NO-APPLY {&RET-ERROR}.
            END.
         END.
      END.

      IF NOT (SELF:NAME = "dokum$"  AND TRIM(SELF:INPUT-VALUE) BEGINS "���") AND
         {assigned tt-opb-pay.pnkodvidaplatewza$} THEN DO:
         vPNForma = GetCodeMisc("��������⥦�",tt-opb-pay.pnkodvidaplatewza$,6).
         IF  {assigned vPNForma}
         AND CAN-FIND(FIRST code WHERE code.class = "������"
                                   AND code.code  = vPNForma NO-LOCK)
         THEN
         FOR EACH xxcode WHERE xxcode.class  = "�����돮��"
                           AND xxcode.parent = vPNForma NO-LOCK:
            IF  NUM-ENTRIES(xxcode.code,"|")            >= 2
            AND GetMangledName(ENTRY(2,xxcode.code,"|")) = SELF:NAME THEN DO:
               RUN Fill-SysMes("",
                               "",
                               "-1",
                               "���� ४����� ~"" + SELF:LABEL + "~" ��易⥫��!").
               RETURN NO-APPLY {&RET-ERROR}.
            END.
         END.
      END.

      IF NOT (SELF:NAME = "dokum$"  AND TRIM(SELF:INPUT-VALUE) BEGINS "���") AND
         {assigned tt-opb-pay.pnkodparametraplatewza$} THEN DO:
         vDocClass = GetXAttrValueEx("loan",tt-opb-pay.pnkodparametraplatewza$,"����ጥ��奬�",?).
         FOR EACH xxattr WHERE (    xxattr.class     = vDocClass
                                AND xxattr.Mandatory = YES)
                            OR (    xxattr.class     = iClass
                                AND xxattr.Mandatory = YES)
         NO-LOCK:
            IF GetMangledName(xxattr.xattr-code) = SELF:NAME
            OR (    (   SELF:NAME = "name-last"
                     OR SELF:NAME = "first-names")
                AND xxattr.xattr-code = "���")
            THEN DO:
               RUN Fill-SysMes("",
                               "",
                               "-1",
                               "���� ४����� ~"" + SELF:LABEL + "~" ��易⥫��!").
               RETURN NO-APPLY {&RET-ERROR}.
            END.
         END.
      END.

   END.

   IF SELF:NAME = "dokum$" THEN DO:
      RUN ChkBlackList (SELF:SCREEN-VALUE,
                        OUTPUT vChkResult
                       ).

   &IF  DEFINED(opb-pay-budg) > 0 &THEN

   find first  code WHERE code.class  EQ "�������"       
                      AND code.parent EQ "�������"  
                      and code.name   EQ mDocumentIdName
                      no-lock no-error.
   if avail code  then do:
      if SELF:SCREEN-VALUE <> "" then do:
         tt-opb-pay.poknd$:SCREEN-VALUE =  code.description[3] + ";" + replace(SELF:SCREEN-VALUE," ",""). 
      end.
      else do:
         tt-opb-pay.poknd$:SCREEN-VALUE = "0". 
      end.
   end.
   &ENDIF

      IF vChkResult = -1 THEN  /* ���� ���ࠢ��� ����� � ��� */
         RETURN NO-APPLY {&RET-ERROR}.
   END.

   IF SELF:NAME = "pninn$" THEN DO:

      &IF  DEFINED(opb-pay-budg) > 0 &THEN
      if SELF:SCREEN-VALUE = "" or SELF:SCREEN-VALUE = "0" or SELF:SCREEN-VALUE = ? then do:
         vBICINN =  GetCode("������",tt-opb-pay.kbk$:SCREEN-VALUE).
         if vBICINN = "" or vBICINN = ? or vBICINN = "0" then do:
            vChkResult = 0.
         end.
         else do:
            message "��� ��� " tt-opb-pay.kbk$:SCREEN-VALUE " ���������� ��� ��易⥫쭮! " view-as alert-box. 
            vChkResult = -1.
         end.
      end.
      else do:
        vChkResult = 0.
      end.

      &ENDIF
      IF vChkResult = -1 THEN  /* ���� ���ࠢ��� ����� � ��� */
         RETURN NO-APPLY {&RET-ERROR}.
   END.

   IF  tt-opb-pay.NewReceiver = YES
   AND SELF:NAME = "bik$"
   THEN DO:
      IF NOT CAN-FIND(FIRST banks-code WHERE banks-code.bank-code-type = "���-9"
                                         AND banks-code.bank-code      = SELF:SCREEN-VALUE)
      THEN DO:
         RUN Fill-SysMes("",
                         "",
                         "-1",
                         "��� ⠪��� ��� (" + SELF:SCREEN-VALUE + ") !").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.
   ELSE IF  tt-opb-pay.NewReceiver = YES
        AND SELF:NAME = "acct"
   THEN DO:
      ASSIGN tt-opb-pay.acct.
      IF LENGTH(tt-opb-pay.acct) <> 20 THEN DO:
         RUN Fill-SysMes("","","-1","����� ����� ��� ������ ���� 20 ᨬ�����.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
      IF TRIM(tt-opb-pay.acct,"0123456789") <> "" THEN DO:
         RUN Fill-SysMes("","","-1","����� ��� ������ ᮤ�ঠ�� ⮫쪮 ����.").
         RETURN NO-APPLY {&RET-ERROR}.
      END.
   END.


   {&END_BT_LEAVE}

   IF LASTKEY = 13 THEN DO:
      mNonFilledHdl = ?.
      RUN FindMandButNonFilledWidg (INPUT-OUTPUT mNonFilledHdl, ?, ?, NO).
      IF VALID-HANDLE(mNonFilledHdl) THEN DO:
         IF tt-opb-pay.op-date GE DATE(FGetSetting("���","���3844�",""))
         THEN 
            APPLY "ENTRY" TO mNonFilledHdl.
         ELSE 
            APPLY "tab" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         IF mQuestWas = NO THEN DO:
            mQuestWas = YES.
            pick-value = "yes".
            RUN Fill-SysMes IN h_tmess ("","","4","�� ��易⥫�� ���� ���������.~n�������� ���� ��� ?").
            IF pick-value = "yes" THEN APPLY "GO" TO FRAME {&MAIN-FRAME}.
            ELSE  RETURN NO-APPLY {&RET-ERROR}.
         END.
      END.
   END.

END.

&IF DEFINED(opb-pay-loan) &THEN
ON F2 OF FRAME {&MAIN-FRAME} ANYWHERE DO:

   DEFINE VARIABLE vAmt    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vStr    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResult AS LOGICAL    NO-UNDO.

   IF  NOT CAN-FIND(FIRST ttUsl) THEN
      RUN CreateTTUsl(OUTPUT mPokazLst).
     
   IF CAN-FIND(FIRST ttUsl) THEN DO:
      RUN inp-usl.p (INPUT tt-opb-pay.pnkodparametraplatewza$,
                     INPUT DEC(tt-opb-pay.amt-rub:SCREEN-VALUE),
                     INPUT mPokazLst,
                     OUTPUT vAmt,
                     OUTPUT vStr, 
                     OUTPUT vResult,
                     INPUT-OUTPUT TABLE ttUsl).
      IF vResult THEN
         ASSIGN
            tt-opb-pay.pnrazbivuslugi$      = vStr
            tt-opb-pay.amt-rub:SCREEN-VALUE = STRING(vAmt).
   END.
   ELSE RUN Fill-SysMes IN h_tmess ("","","0","��� ������� ��ࠬ��� ���⥦� �� ����� ���祭� ���.").

END.
&ENDIF

ON F6 OF FRAME {&MAIN-FRAME} ANYWHERE DO:
   DEFINE VAR vNonFilled AS CHAR NO-UNDO.
   DEFINE VAR vPersonId  LIKE person.person-id NO-UNDO.
   DEFINE VAR vAddr      AS CHAR NO-UNDO.
   DEFINE VAR vAddrErr   AS CHAR NO-UNDO.

   ASSIGN
      tt-opb-pay.name-last
      tt-opb-pay.first-names
      tt-opb-pay.adres$
      tt-opb-pay.Birthday
      tt-opb-pay.BirthPlace
      tt-opb-pay.country-pers
      tt-opb-pay.dokum$
      tt-opb-pay.cust-doc-who
      tt-opb-pay.Document4Date_vid
   .
   IF NOT {assigned tt-opb-pay.name-last}         THEN {additem.i vNonFilled tt-opb-pay.name-last:LABEL}
   IF NOT {assigned tt-opb-pay.first-names}       THEN {additem.i vNonFilled tt-opb-pay.first-names:LABEL}
   IF NOT {assigned tt-opb-pay.adres$}            THEN {additem.i vNonFilled tt-opb-pay.adres$:LABEL}
   IF (dokum$:SIDE-LABEL-HANDLE:DCOLOR EQ  GetDCOLOR("green")) OR CAN-DO(mLimitVP,tt-opb-pay.pnkodvidaplatewza$:SCREEN-VALUE) THEN  DO:

      IF NOT {assigned tt-opb-pay.document-id}       THEN {additem.i vNonFilled mDocumentIdName:LABEL}
      IF NOT {assigned tt-opb-pay.dokum$}            THEN {additem.i vNonFilled tt-opb-pay.dokum$:LABEL}
      IF NOT {assigned tt-opb-pay.cust-doc-who}      THEN {additem.i vNonFilled tt-opb-pay.cust-doc-who:LABEL}
      IF tt-opb-pay.Document4Date_vid = ?            THEN {additem.i vNonFilled tt-opb-pay.Document4Date_vid:LABEL} 

   END.

   IF {assigned vNonFilled} THEN DO:
      RUN Fill-SysMes("",
                      "",
                      "1",
                      "�������� ����: " + vNonFilled + " !").
      RETURN NO-APPLY.
   END.

   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"name-last-save",tt-opb-pay.name-last).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"first-names-save",tt-opb-pay.first-names).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"����-save",tt-opb-pay.adres$).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"Birthday-save",tt-opb-pay.Birthday).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"BirthPlace-save",tt-opb-pay.BirthPlace).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"country-pers-save",tt-opb-pay.country-pers).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"document-id-save",tt-opb-pay.document-id).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"����-save",tt-opb-pay.dokum$).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"cust-doc-who-save",tt-opb-pay.cust-doc-who).
   RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"Document4Date_vid-save",tt-opb-pay.Document4Date_vid).
   /* �஢��塞 ���� �� ������ � ���� */
   RUN RunTransaction IN h_pbase ("�������").
   vPersonId = INT64(GetAttrValue2("",0,"person-id-save")) NO-ERROR.
   IF vPersonId > 0 THEN DO:
      RUN Fill-SysMes("",
                      "",
                      "-1",
                      "������ 㦥 ��࠭��.").
      RETURN NO-APPLY.
   END.
   /* ���࠭塞 */
   RUN RunTransaction IN h_pbase ("����唋").
   /* �஢��塞 �� ��࠭���� */
   RUN RunTransaction IN h_pbase ("�������").
   vPersonId = INT64(GetAttrValue2("",0,"person-id-save")) NO-ERROR.
   IF vPersonId > 0
   THEN RUN Fill-SysMes("",
                   "",
                   "1",
                   "������ ��࠭��.").
   RETURN NO-APPLY.
END.

ON SHIFT-F7 OF FRAME {&MAIN-FRAME} ANYWHERE DO:
   DO TRANSACTION:
      pick-value = "".
      RUN codelay.p ("��������", "", "", 4).
      IF  LASTKEY = 10
      AND {assigned pick-value} THEN DO:
         RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"���������-save",pick-value).
         mNonFilledHdl = ?.
         RUN FindMandButNonFilledWidg (INPUT-OUTPUT mNonFilledHdl, ?, ?, YES).
      END.
   END.
   {&END_BT_F1}
END.

&IF DEFINED(opb-pay-budg) &THEN
DEFINE TEMP-TABLE tToHide NO-UNDO
   FIELD Hdl AS HANDLE
INDEX ByHdl IS PRIMARY Hdl.
&SCOPED-DEFINE NalAttrs "�����,�����,�����,�����,�����,�����"
&ENDIF
DEFINE VAR mStrangeOp AS LOGICAL NO-UNDO.
PROCEDURE FindMandButNonFilledWidg:
   DEFINE INPUT-OUTPUT PARAMETER ioHdl     AS HANDLE  NO-UNDO.
   DEFINE INPUT        PARAMETER iPNForma  AS CHAR    NO-UNDO.
   DEFINE INPUT        PARAMETER iDocClass AS CHAR    NO-UNDO.
   DEFINE INPUT        PARAMETER iColering AS LOGICAL NO-UNDO.
   
   DEFINE VAR vWidgetHdl  AS HANDLE NO-UNDO.
   DEFINE VAR vChildHdl   AS HANDLE NO-UNDO.
   DEFINE VAR vFldName    AS CHAR   NO-UNDO.
   DEFINE VAR vFindedHdl  AS HANDLE NO-UNDO.
   DEFINE VAR vStrangeStr AS CHAR   NO-UNDO.
   DEFINE BUFFER xxcode FOR code.
   DEFINE BUFFER xxattr FOR xattr.
   /* ����� ����᪨ */
   IF iColering = YES THEN DO:

      vStrangeStr = GetAttrValue("", 0, "���������-save").
      mStrangeOp = (vStrangeStr > "" AND vStrangeStr <> {&RET-ERROR}).

      &IF DEFINED(opb-pay-budg) &THEN
         {empty tToHide}
         
         IF {assigned tt-opb-pay.pnkodparametraplatewza$} THEN DO:
            mSeparate = (GetXAttrValue("loan",tt-opb-pay.pnkodparametraplatewza$,"���⤑㬬���") = ENTRY(1,GetXAttrEx("loan-pn-par","���⤑㬬���","Data-Format"),"/")).
            IF mSeparate = NO THEN DO:
               vFldName = GetXAttrValue("loan",tt-opb-pay.pnkodparametraplatewza$,"��������⥦�").
               IF {assigned vFldName} THEN DO:
                  FIND FIRST code WHERE code.class = "��������⥦�"
                                    AND code.code  = vFldName
                  NO-LOCK NO-ERROR.
                  IF AVAIL code THEN mGroupBy = code.description[3].
               END.
            END.
         END.
      &ENDIF

      /* ����᪠ */
      vWidgetHdl = ioHdl.
      RUN FindMandButNonFilledWidg IN THIS-PROCEDURE (INPUT-OUTPUT ioHdl,?,?,?).

      &IF DEFINED(opb-pay-budg) &THEN
         IF mSeparate = NO THEN DO:
            FOR EACH tToHide EXCLUSIVE-LOCK:
               ASSIGN
                  tToHide.Hdl:SENSITIVE = NO
                  tToHide.Hdl:VISIBLE   = NO
               .
               DELETE tToHide.
            END.
         END.
      &ENDIF

      ioHdl = vWidgetHdl.
      /* ������ ��易⥫쭮�� */
      RUN FindMandButNonFilledWidg IN THIS-PROCEDURE (INPUT-OUTPUT ioHdl,?,?,NO).

      RETURN.
   END.

   IF  NOT {assigned iPNForma}
   AND NOT {assigned iDocClass} THEN DO WITH FRAME {&MAIN-FRAME}:
      IF {assigned tt-opb-pay.pnkodvidaplatewza$} THEN DO:
         iPNForma = GetCodeMisc("��������⥦�",tt-opb-pay.pnkodvidaplatewza$,6).
         IF  {assigned iPNForma}
         AND NOT CAN-FIND(FIRST code WHERE code.class = "������"
                                       AND code.code  = iPNForma NO-LOCK)
         THEN iPNForma = "".
      END.

      IF {assigned tt-opb-pay.pnkodparametraplatewza$} THEN DO:
         iDocClass = GetXAttrValueEx("loan",tt-opb-pay.pnkodparametraplatewza$,"����ጥ��奬�",?).
      END.

      IF {assigned iPNForma}
      OR {assigned iDocClass} THEN DO:
         ioHdl = FRAME {&MAIN-FRAME}:HANDLE.
         RUN FindMandButNonFilledWidg (INPUT-OUTPUT ioHdl, iPNForma, iDocClass, iColering).
      END.
      RETURN.
   END.

   IF NOT VALID-HANDLE(ioHdl) THEN RETURN.
   IF NOT CAN-QUERY(ioHdl,"FIRST-CHILD") THEN DO:
      ioHdl = ?.
      RETURN.
   END.
   
   vWidgetHdl = ioHdl:FIRST-CHILD.
   _SchWidg:
   REPEAT:
      IF NOT VALID-HANDLE(vWidgetHdl) THEN LEAVE _SchWidg.
      IF  CAN-QUERY(vWidgetHdl,"SENSITIVE")
      AND CAN-QUERY(vWidgetHdl,"VISIBLE")
      AND vWidgetHdl:SENSITIVE = YES
      AND vWidgetHdl:VISIBLE   = YES
      AND (   (   vWidgetHdl:INPUT-VALUE = ""
               OR vWidgetHdl:INPUT-VALUE = ?
               OR (    vWidgetHdl:NAME        = "amt-rub"
                   AND vWidgetHdl:INPUT-VALUE = 0)                                 
               &IF DEFINED(opb-pay) > 0 OR DEFINED(opb-pay-budg) > 0 &THEN
               OR (    vWidgetHdl:NAME        = "details"
                   AND mDetailsOk = NO
                   AND vWidgetHdl:INPUT-VALUE MATCHES "*<*")
               &ENDIF
              )
           OR iColering = ?) 
      THEN DO: 
         IF {assigned iDocClass} THEN DO:
            vFldName = vWidgetHdl:NAME.
            IF vFldName = "name-last"
            OR vFldName = "first-names"
            THEN vFldName = "fio$".
            ELSE IF vFldName = "mDocumentIdName"
                 THEN vFldName = "document-id".
            IF {assigned iDocClass} THEN _dc:
            FOR EACH xxattr WHERE  
                     xxattr.class = iDocClass 
                  OR xxattr.class = iClass

            NO-LOCK BY xxattr.Mandatory DESCENDING:

               IF NOT (    (    xxattr.class = iDocClass                                                                                                
                            AND (   xxattr.Mandatory = YES                                                                                             
                                 &IF DEFINED(opb-pay-budg) &THEN                                                                                       
                                 OR (    CAN-DO({&NalAttrs},xxattr.xattr-code)                                                                         
                                     AND mSeparate = NO                                                                                                
                                     AND iColering = ?                                                                                                 
                                     AND NOT CAN-DO(mGroupBy,xxattr.xattr-code))                                                                       
                                 &ENDIF                                                                                                                
                                ))                                                                                                                     
                        OR (    xxattr.class = iClass                                                                                                  
                            AND (   xxattr.Mandatory = YES                                                                                             
                                 &IF DEFINED(opb-pay-budg) &THEN                                                                                       
                                 OR (    CAN-DO({&NalAttrs},xxattr.xattr-code)                                                                         
                                     AND mSeparate = NO                                                                                                
                                     AND iColering = ?                                                                                                 
                                     AND NOT CAN-DO(mGroupBy,xxattr.xattr-code))                                                                       
                                 &ENDIF                                                                                                                
                                ))                                                                                                                     
                        OR (    xxattr.class = iClass                                                                                                  
                            AND mStrangeOp = YES                                                                                                       
                            AND CAN-DO("���,����,Birthday,country-pers,document-id,����,cust-doc-who,Document4Date_vid",xxattr.xattr-code))         
                   ) 
               THEN
                   NEXT _dc.

               IF GetMangledName(xxattr.xattr-code) = vFldName THEN DO:
                  &IF DEFINED(opb-pay-budg) &THEN
                  IF     xxattr.Mandatory <> YES 
                     AND xxattr.Xattr-Code NE "�����" 
                  THEN DO:

                     CREATE tToHide. tToHide.Hdl = vWidgetHdl.
                     NEXT _dc.
                  END. 
                  &ENDIF
                  IF iColering = ? THEN DO:
                     &IF DEFINED(opb-pay-budg) &THEN
                     IF  iColering = ?
                     AND mSeparate = NO
                     THEN DO:
                        FOR EACH tToHide WHERE tToHide.Hdl = vWidgetHdl EXCLUSIVE-LOCK:
                           DELETE tToHide.
                        END.
                     END.
                     &ENDIF
                     vWidgetHdl:SIDE-LABEL-HANDLE:DCOLOR = GetDCOLOR("green").
                     LEAVE _dc.
                  END.
                  ioHdl = vWidgetHdl.
                  RETURN.
               END.
            END.
         END.
         IF {assigned iPNForma} THEN _pc:
         FOR EACH xxcode WHERE xxcode.class  = "�����돮��"
                           AND xxcode.parent = iPNForma NO-LOCK:
            IF  NUM-ENTRIES(xxcode.code,"|")            >= 2
            AND GetMangledName(ENTRY(2,xxcode.code,"|")) = vWidgetHdl:NAME THEN DO:
               IF iColering = ? THEN DO:
                  &IF DEFINED(opb-pay-budg) &THEN
                  IF  iColering = ?
                  AND mSeparate = NO
                  THEN DO:
                     FOR EACH tToHide WHERE tToHide.Hdl = vWidgetHdl EXCLUSIVE-LOCK:
                        DELETE tToHide.
                     END.
                  END.
                  &ENDIF
                  vWidgetHdl:SIDE-LABEL-HANDLE:DCOLOR = GetDCOLOR("green").
                  LEAVE _pc.
               END.
               ioHdl = vWidgetHdl.
               RETURN.
            END.
         END.
      END.
   
      vChildHdl = vWidgetHdl.
      RUN FindMandButNonFilledWidg IN THIS-PROCEDURE (INPUT-OUTPUT vChildHdl, iPNForma, iDocClass, iColering).
      IF VALID-HANDLE(vChildHdl) THEN DO:
         ioHdl = vChildHdl.
         RETURN.
      END.
   
      vWidgetHdl = vWidgetHdl:NEXT-SIBLING.
   END.

   ioHdl = ?.
   RETURN.
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChkBlackList TERMINAL-SIMULATION 
PROCEDURE ChkBlackList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iDocument  AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER oChkResult AS INT64 NO-UNDO INITIAL 1.

   DEFINE VAR vBlackListNameChar AS CHAR NO-UNDO.
   DEFINE VAR vBlackListCodeChar AS CHAR NO-UNDO.
   DEFINE VAR vBlackWhyCodeChar  AS CHAR NO-UNDO.
   DEFINE VAR vBlackWhyNameChar  AS CHAR NO-UNDO.

   ASSIGN
      vBlackListCodeChar = iDocument
      vBlackListNameChar = GetCodeName("black-list",vBlackListCodeChar)
   .

   IF vBlackListNameChar <> ? THEN
   DO:
      ASSIGN
         vBlackWhyCodeChar  = GetCode("black-list",vBlackListCodeChar)
         vBlackWhyCodeChar  = IF vBlackWhyCodeChar = ? THEN "?" ELSE vBlackWhyCodeChar
         vBlackWhyNameChar  = GetCodeName("black-why",vBlackWhyCodeChar)
         vBlackWhyNameChar  = IF vBlackWhyNameChar = ? THEN "?" ELSE vBlackWhyNameChar
      .

      RUN Fill-SysMes ("", 
                       "", 
                       "4", 
                       SUBSTR("    ��������: " + SUBSTR(vBlackListNameChar,1,12)
                                               + FILL(" ",45),1,45) 
                       + "~n" + /* ��ॢ�� �� ᫥� ��ப� */
                       SUBSTR("�����, �����: " + SUBSTR(vBlackListCodeChar,1,25)
                                               + FILL(" ",45),1,45) 
                       + "~n" + "~n" + 
                       "*** ���������� � ������ ������ ���������� ***" 
                       + "~n" + "~n" + 
                       SUBSTR("    ������� : "  + vBlackWhyCodeChar
                                                + FILL(" ",45),1,45) 
                       + "~n" + 
                       SUBSTR("            - "  + SUBSTR(vBlackWhyNameChar,1,45)
                                                + FILL(" ",45),1,45) 
                       + "~n" + "~n" + "�த������?").

      IF pick-value NE "YES" THEN 
         oChkResult = -1.

   END. /* IF vBlackListNameChar <> ? */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayPerson TERMINAL-SIMULATION 
PROCEDURE DisplayPerson :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER iPersonId LIKE person.person-id NO-UNDO.
   FIND FIRST person WHERE person.person-id = iPersonId NO-LOCK NO-ERROR.
   IF AVAIL person THEN DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN
         tt-opb-pay.name-last:SCREEN-VALUE           = person.name-last
         tt-opb-pay.first-names:SCREEN-VALUE         = person.first-names
         tt-opb-pay.country-pers:SCREEN-VALUE        = person.country-id
         tt-opb-pay.document-id                      = person.document-id
         tt-opb-pay.dokum$:SCREEN-VALUE              = person.document
         tt-opb-pay.Document4Date_vid:SCREEN-VALUE   = GetXAttrValue("person",STRING(person.person-id),"Document4Date_vid")
         tt-opb-pay.adres$:SCREEN-VALUE              = person.address[1]
         tt-opb-pay.Birthday:SCREEN-VALUE            = STRING(person.birthday,"99/99/9999")
         tt-opb-pay.BirthPlace:SCREEN-VALUE          = GetXAttrValue("person",STRING(person.person-id),"BirthPlace")
         tt-opb-pay.cust-doc-who:SCREEN-VALUE        = person.issue
         tt-opb-pay.pninn$:SCREEN-VALUE              = person.inn
         tt-opb-pay.pntelefon$:SCREEN-VALUE          = TRIM(TRIM(TRIM(IF {assigned person.phone[1]} THEN person.phone[1] ELSE ""),",") + "," + TRIM(TRIM(IF {assigned person.phone[2]} THEN person.phone[2] ELSE ""),","),",")
      .
      ASSIGN
         tt-opb-pay.name-last
         tt-opb-pay.first-names
         tt-opb-pay.country-pers
         tt-opb-pay.dokum$
         tt-opb-pay.Document4Date_vid
         tt-opb-pay.adres$
         tt-opb-pay.Birthday
         tt-opb-pay.BirthPlace
         tt-opb-pay.cust-doc-who
         tt-opb-pay.pninn$
      .
      tt-opb-pay.fio$ = tt-opb-pay.name-last + " " + tt-opb-pay.first-names.
      mDocumentIdName = GetCodeName("�������",tt-opb-pay.document-id).
      IF mDocumentIdName = ? THEN mDocumentIdName = "".
      mDocumentIdName:SCREEN-VALUE = mDocumentIdName.
      mDocumentIdName:SCREEN-VALUE = mDocumentIdName.
      
      IF RETURN-VALUE = {&RET-ERROR}
      THEN RETURN NO-APPLY.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCustAcct TERMINAL-SIMULATION 
PROCEDURE GetCustAcct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAM use-inn AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER use-signs AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER  name1 AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER name2 AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER cust-inn AS CHARACTER NO-UNDO.

   DEFINE BUFFER xacct FOR acct.
   {find-act.i
      &bact = xacct
      &acct = iAcct
      &curr = iCurr
   }
   IF AVAIL xacct THEN
      RUN GetCust IN h_base (BUFFER xacct,YES,YES,
                             OUTPUT Name1,OUTPUT Name2,OUTPUT cust-inn).
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
   
   IF tt-opb-pay.NewReceiver <> YES THEN DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN
         tt-opb-pay.pnkodvidaplatewza$:SENSITIVE          = NO
         tt-opb-pay.cont-cli:SENSITIVE                    = NO
         tt-opb-pay.pnpoluwcatelw#naimenovanie$:SENSITIVE = NO
         tt-opb-pay.acct:SENSITIVE                        = NO
         tt-opb-pay.pnpoluwcatelw#inn$:SENSITIVE          = NO
         tt-opb-pay.pnpoluwcatelw#kpp$:SENSITIVE          = NO
         tt-opb-pay.bik$:SENSITIVE                        = NO
      .
   END.
   &IF DEFINED(opb-pay-budg) &THEN
      tt-opb-pay.poktp$:HIDDEN  = tt-opb-pay.op-date GE DATE(FGetSetting("���","���3844�","")) .
   &ENDIF
   &IF DEFINED(opb-pay-loan) &THEN
      mClassif = GetXattrValue("loan", tt-opb-pay.pnkodparametraplatewza$,"����������").
      mPeniRuchV = LOGICAL(GetXAttrValueEx("loan", tt-opb-pay.pnkodparametraplatewza$, "�����炢","���"),"��/���").
      IF mPeniRuchV THEN   
         tt-opb-pay.pntarifpeni$:SENSITIVE = NO.
      ELSE
         tt-opb-pay.pnsummapeni$:SENSITIVE = NO.         
   &ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalPutTitle TERMINAL-SIMULATION 
PROCEDURE LocalPutTitle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE vTitle     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vModeStr   AS CHARACTER NO-UNDO INIT {&MOD_LIST}.
   DEFINE VARIABLE vClassName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vLen       AS INT64   NO-UNDO.
   vTitle = " - " + ENTRY(iMode,vModeStr) + IF mMOD_VIEW_DATE THEN " �� ����: " + STRING(gend-date) ELSE "".
   vLen = FRAME {&MAIN-FRAME}:WIDTH.
   vClassName = CAPS(GetXclassName(iClass)).
   IF LENGTH(vClassName) GT vLen - LENGTH(vTitle) - 4 THEN
      SUBSTRING(vClassName,
                vLen - LENGTH(vTitle) - 8,1000) = "...".
   vTitle = vClassName + LC(vTitle).

   mBT_Title = "[" + vTitle + "]".
   FRAME fMain:TITLE = "[" + vTitle + "]".

   RETURN ERROR.
END PROCEDURE.
DEF VAR mIp AS INT64 NO-UNDO.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local_GO TERMINAL-SIMULATION 
PROCEDURE local_GO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VAR vShema       AS CHARACTER NO-UNDO.
   DEFINE VAR vChildBrList AS CHARACTER NO-UNDO.
   DEFINE VAR vKey         AS INT64     NO-UNDO.
   DEFINE BUFFER xloan-par FOR loan.
   DEFINE BUFFER xloan     FOR loan.
   DEFINE BUFFER loan      FOR loan.
   DEFINE VARIABLE vAmt    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vStr    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vResult AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vCheck    AS   LOGICAL        NO-UNDO.
   DEFINE VARIABLE vContract LIKE loan.contract  NO-UNDO.
   DEFINE VARIABLE vContCode LIKE loan.cont-code NO-UNDO.
   DEFINE VARIABLE vCustCat  LIKE loan.cust-cat  NO-UNDO.
   DEFINE VARIABLE vCustId   LIKE loan.cust-id   NO-UNDO.
   DEFINE VARIABLE vOK       AS   LOGICAL        NO-UNDO.
   DEFINE VARIABLE vWH       AS   WIDGET-HANDLE  NO-UNDO.

   vStr = tt-opb-pay.pnkodparametraplatewza$.
   vCheck = GetXAttrValueEx("loan", vStr, "��流������", "���") = "��".
   IF NOT vCheck THEN DO:
      ASSIGN
         vCheck    = ?
         vContract = ENTRY(1, vStr)
         vContCode = ENTRY(2, vStr)
                     WHEN NUM-ENTRIES(vStr) = 2
      .
      FIND FIRST loan WHERE
         loan.contract  = vContract AND
         loan.cont-code = vContCode
      NO-LOCK NO-ERROR.
      IF AVAILABLE loan THEN DO:
         ASSIGN
            vContract = loan.parent-contract
            vContCode = loan.parent-cont-code
         .
         FIND FIRST loan WHERE
            loan.contract  = vContract AND
            loan.cont-code = vContCode
         NO-LOCK NO-ERROR.
         IF AVAILABLE loan THEN DO:
            vStr = GetCustClass(loan.cust-cat).
            vCheck = GetXAttrValueEx(vStr,
                                     STRING(loan.cust-id),
                                     "����流������",
                                     "���") <> "��".
            IF vCheck THEN
               RUN TestRecord IN h_flt (vStr,
                                        STRING(loan.cust-id),
                                        FGetSetting("��類������", "", ""),
                                        OUTPUT vCheck).
         END.
      END.
   END.
   IF vCheck THEN DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN
         tt-opb-pay.name-last
         tt-opb-pay.first-names
         tt-opb-pay.adres$
         tt-opb-pay.country-pers
         tt-opb-pay.BirthPlace
         tt-opb-pay.dokum$
         tt-opb-pay.cust-doc-who
         tt-opb-pay.Birthday
         tt-opb-pay.Document4Date_vid
         mDocumentIdName
      .
      tt-opb-pay.fio$ = tt-opb-pay.name-last + " " + tt-opb-pay.first-names.
      IF NOT {assigned tt-opb-pay.fio$}         OR
         NOT {assigned tt-opb-pay.adres$}       OR
         NOT {assigned tt-opb-pay.country-pers} OR
         NOT {assigned tt-opb-pay.BirthPlace}   OR
         NOT {assigned tt-opb-pay.dokum$}       OR
         NOT {assigned tt-opb-pay.cust-doc-who} OR
         tt-opb-pay.Birthday EQ ?               OR
         tt-opb-pay.Document4Date_vid EQ ?      OR
         NOT {assigned mDocumentIdName}
      THEN DO:
         RUN Fill-SysMes IN h_tmess ("",
                                     "",
                                     "1",
                                     "����室��� ����� ���祭�� ��� �����," +
                                     " �।�����祭��� ��� �����䨪�樨 ���⥫�騪�.").
         RETURN ERROR {&RET-ERROR}.
      END.
   END.

   IF NUM-ENTRIES(tt-opb-pay.pnkodparametraplatewza$) = 2 THEN DO:
      FIND FIRST xloan-par WHERE xloan-par.contract  = ENTRY(1,tt-opb-pay.pnkodparametraplatewza$)
                             AND xloan-par.cont-code = ENTRY(2,tt-opb-pay.pnkodparametraplatewza$)
      NO-LOCK NO-ERROR.
      IF AVAIL xloan-par
      THEN FIND FIRST xloan WHERE xloan.contract  = xloan-par.parent-contract
                              AND xloan.cont-code = xloan-par.parent-cont-code
           NO-LOCK NO-ERROR.
      IF  AVAIL xloan
      AND xloan.cont-type <> "���" THEN DO:
         vShema = GetXAttrValueEx("loan",xloan.contract + "," + xloan.cont-code,"���奬��ਥ��",?).
         IF  vShema = "Only_here"
         AND xloan.branch-id <> tt-opb-pay.branch-id THEN DO:
            RUN Fill-SysMes("", "", "-1", "����� ���⥦ �� ����� ���� �ਭ�� � �⮬ ���ࠧ������� �����").
            RETURN ERROR {&RET-ERROR}.
         END.
         IF vShema = "Here_and_child" THEN DO:
            vChildBrList = "".
            RUN GetBranchChild IN h_brnch (xloan.branch-id,INPUT-OUTPUT vChildBrList).
            IF NOT CAN-DO(vChildBrList,tt-opb-pay.branch-id) THEN DO:
               RUN Fill-SysMes("", "", "-1", "����� ���⥦ �� ����� ���� �ਭ�� � �⮬ ���ࠧ������� �����").
               RETURN ERROR {&RET-ERROR}.
            END.
         END.
      END.
   END.

   IF tt-opb-pay.NewReceiver = YES THEN DO WITH FRAME {&MAIN-FRAME}:
      ASSIGN tt-opb-pay.acct tt-opb-pay.bik$.
      RUN key-tst.p (tt-opb-pay.acct,STRING(tt-opb-pay.bik$,"999999999"),OUTPUT vKey).
      IF INT64(SUBSTR(tt-opb-pay.acct,9,1)) <> vKey THEN DO:
         RUN Fill-SysMes("","","-1","������ ����.").
         APPLY "ENTRY" TO tt-opb-pay.acct.
         RETURN ERROR {&RET-ERROR}.
      END.
   END.

   mNonFilledHdl = ?.
   RUN FindMandButNonFilledWidg (INPUT-OUTPUT mNonFilledHdl, ?, ?, NO).
   IF VALID-HANDLE(mNonFilledHdl) THEN DO:
      RUN Fill-SysMes("", "", "1", "�������� ��易⥫쭮� ���� " + mNonFilledHdl:LABEL).
      APPLY "ENTRY" TO mNonFilledHdl.
      RETURN ERROR {&RET-ERROR}.
   END.

   &IF DEFINED(opb-pay-loan) &THEN
   /*
   DEFINE VAR vDetails LIKE op.details NO-UNDO.
   vDetails = SELF:SCREEN-VALUE.
   RUN CreateFrmFields (?,"opreq", "amt-cur", STRING(tt-opb-pay.amt-rub)).
   RUN ProcessDetails (?, INPUT-OUTPUT vDetails).
   SELF:SCREEN-VALUE = vDetails. */

   IF  NOT CAN-FIND(FIRST ttUsl) THEN
      RUN CreateTTUsl(OUTPUT mPokazLst).
     
   IF CAN-FIND(FIRST ttUsl) THEN DO:
      RUN inp-usl.p (INPUT tt-opb-pay.pnkodparametraplatewza$,
                     INPUT DEC(tt-opb-pay.amt-rub:SCREEN-VALUE),
                     INPUT mPokazLst,
                     OUTPUT vAmt,
                     OUTPUT vStr, 
                     OUTPUT vResult,
                     INPUT-OUTPUT TABLE ttUsl).
      IF vResult THEN
         ASSIGN
            tt-opb-pay.pnrazbivuslugi$      = vStr
            tt-opb-pay.amt-rub:SCREEN-VALUE = STRING(vAmt).

      RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"����������㣨-save",tt-opb-pay.pnrazbivuslugi$).
   END.

   &ENDIF
  &IF DEFINED(opb-pay-budg) &THEN

   ASSIGN 
      tt-opb-pay.pokst$
      tt-opb-pay.uin$ 
      tt-opb-pay.pninn$
      tt-opb-pay.poknd$
   . 
   IF   CAN-DO(fGetSetting("���","����ሏ",""),tt-opb-pay.pokst$)
     AND (NOT {assigned  tt-opb-pay.uin$}  OR   tt-opb-pay.uin$   EQ "0")
     AND (NOT {assigned tt-opb-pay.pninn$} OR   tt-opb-pay.pninn$ EQ "0") 
    
   THEN DO:
      RUN Fill-SysMes("", "", "1", 
         "��������! ��� ���������� ���祭�� ���� (101) " + tt-opb-pay.pokst$ + 
         " � ������⢨� ��� � ���㬥�� ��� ���⥫�騪� ������ ���� ��������!").
       APPLY "ENTRY" TO tt-opb-pay.uin$.
       RETURN ERROR.
   END.

  /* �᫨ ���� (101) 㤮���⢮��� ᯨ�� �� �����108_22, ���� (22) ��� � ���� (108) ���������
     ���祭��� 0, ������ ���� �������� ��� (12 ࠧ�冷�) */
   IF CAN-DO(fGetSetting("���","�����108_22",""),tt-opb-pay.pokst$)
      AND tt-opb-pay.uin$ = "0" AND tt-opb-pay.poknd$ = "0" AND 
     (NOT {assigned tt-opb-pay.pninn$} OR LENGTH(tt-opb-pay.pninn$) NE 12) 
     THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1","�� ���������� ���� (101) ���祭��� " + tt-opb-pay.pokst$ + 
        ", ����� (22) ��� � (108) ���祭��� ���� ��� ���⥫�騪� ������ ���� �������� (12 ࠧ�冷�)!").
       APPLY "ENTRY" TO tt-opb-pay.pninn$.
       RETURN ERROR.
     END.        

   /* �஢�ન */
   IF (LENGTH(tt-opb-pay.pninn$) EQ 12) AND
      SUBSTR(tt-opb-pay.pninn$,1,2) EQ "00" 
     THEN DO:
        RUN Fill-SysMes IN h_tmess ("","","-1","���� ��� ���� ��� ���⥫�騪� " + 
        tt-opb-pay.pninn$ + " �� ������ ���� ࠢ�� 00").
       APPLY "ENTRY" TO tt-opb-pay.pninn$.
       RETURN ERROR.
     END.

   &ENDIF   
IF FGetSetting("�������������","","") EQ "��" 
   THEN DO:
      DEF VAR mCode  AS CHARACTER NO-UNDO.
      DEF VAR mCodCl AS CHARACTER NO-UNDO.
      DEF BUFFER dcode FOR code.

      mCodCl =  GetXAttrValue("loan",tt-opb-pay.pnkodparametraplatewza$,"�������ᄮ��").
      IF {assigned mCodCl} AND {assigned tt-opb-pay.name-last:SCREEN-VALUE} THEN DO:
         FOR EACH dcode WHERE dcode.class  EQ mCodCl 
                          AND dcode.parent EQ mCodCl 
                          AND (dCODE.NAME  EQ tt-opb-pay.name-last:SCREEN-VALUE 
                          AND (ENTRY(1,dCODE.val,";") EQ tt-opb-pay.adres$:SCREEN-VALUE
                           OR ENTRY(1,dCODE.val,";")  EQ ENTRY(1,tt-opb-pay.adres$:SCREEN-VALUE,";")))
         NO-LOCK:
            mCode = mCode + "," + dCODE.code.
         END.

         IF mIp = 0 AND
            {assigned mCode}
         THEN DO:
            mIp = mIp + 1.

            RUN browseld.p (mCodCl,
                           "FirstFrame" + CHR(1) + "code",
                           "2" + CHR(1) + mCode,
                           "",
                           1). 
            IF LASTKEY EQ 10 OR LASTKEY EQ KEYCODE ("ESC") THEN
               RETURN ERROR {&RET-ERROR}.
         END.
      END.
   END.

   &IF DEFINED(opb-pay-budg) &THEN
   RUN Validate148n IN THIS-PROCEDURE (OUTPUT vOK, OUTPUT vWH).
   IF NOT vOK THEN DO:
      IF VALID-HANDLE(vWH) THEN
         APPLY "ENTRY" TO vWH.
      RETURN ERROR {&RET-ERROR}.
   END.
   &ENDIF

   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(opb-pay-loan) &THEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LocalSetObject TERMINAL-SIMULATION 
PROCEDURE LocalSetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF {assigned tt-opb-pay.pnswcet$} THEN DO:

     FIND FIRST code WHERE
                CODE.class EQ mClassif
            AND CODE.code  EQ tt-opb-pay.pnswcet$
                NO-LOCK NO-ERROR.
     IF NOT AVAIL(CODE) THEN DO:

        CREATE CODE.
        ASSIGN
           CODE.CLASS  = mClassif
           CODE.parent = mClassif
           CODE.code   = tt-opb-pay.pnswcet$
           CODE.name   = TRIM(tt-opb-pay.name-last + " " + tt-opb-pay.first-names)
           CODE.val    = tt-opb-pay.adres$.

        tt-opb-pay.pndobavswc$ = YES.
        RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"���������-save",STRING(tt-opb-pay.pndobavswc$)).
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ENDIF
  
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostGetObject TERMINAL-SIMULATION 
PROCEDURE PostGetObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
   DO WITH FRAME {&MAIN-FRAME}:
      DISPLAY tt-opb-pay.bik$.
      APPLY "VALUE-CHANGED" TO tt-opb-pay.bik$.
      APPLY "VALUE-CHANGED" TO tt-opb-pay.amt-rub.
      &IF DEFINED(opb-pay-loan) &THEN
      APPLY "VALUE-CHANGED" TO tt-opb-pay.pnpokazkon$.
      &ENDIF
      IF tt-opb-pay.NewReceiver THEN DO:
         ASSIGN
            tt-opb-pay.pnpoluwcatelw#naimenovanie$ = ""
            tt-opb-pay.pnpoluwcatelw#kpp$          = ""
         .
         DISPLAY tt-opb-pay.pnpoluwcatelw#naimenovanie$ tt-opb-pay.pnpoluwcatelw#inn$ tt-opb-pay.pnpoluwcatelw#kpp$.
      END.
      mDocumentIdName = GetCodeName("�������",tt-opb-pay.document-id).
      IF mDocumentIdName = ? THEN mDocumentIdName = "".
      DISPLAY mDocumentIdName.
      &IF DEFINED(opb-pay-budg) &THEN
      ASSIGN
         tt-opb-pay.pokdd$ = REPLACE(REPLACE(tt-opb-pay.pokdd$,"/",""),".","")
         tt-opb-pay.poknp$ = REPLACE(tt-opb-pay.poknp$,"/",".")
      .

      &ENDIF
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckAddrStruct TERMINAL-SIMULATION 
PROCEDURE CheckAddrStruct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iAddress AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oAddrErr AS CHARACTER INIT "" NO-UNDO.
   DEFINE VAR vOk AS INT64 NO-UNDO.
   RUN PARSFUNC-��_���������_����� IN h_ppn (iAddress, OUTPUT oAddrErr, OUTPUT vOk).
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddCustomStringToHelp TERMINAL-SIMULATION 
PROCEDURE AddCustomStringToHelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   SetHelpStrAdd(TRIM(mHelpStrAdd + "�" + "F6 ���࠭���� ��","�")).
   SetHelpStrAdd(TRIM(mHelpStrAdd + "�" + "SHIFT-F7 ������.ᤥ���","�")).
   &IF DEFINED(opb-pay-budg) &THEN
      SetHelpStrAdd(TRIM(mHelpStrAdd + "�" + "F3 ��������� ����","�")).
   &ENDIF

   RUN PutHelp("",FRAME {&MAIN-FRAME}:HANDLE).
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(opb-pay-loan) &THEN

PROCEDURE CreateTTUsl:
DEFINE OUTPUT PARAMETER oPokazLst AS CHARACTER NO-UNDO.

DEFINE VARIABLE vVal  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vCnt  AS INT64 NO-UNDO.

FOR EACH term-obl WHERE
    term-obl.contract  EQ ENTRY(1, tt-opb-pay.pnkodparametraplatewza$) AND
    term-obl.cont-code EQ ENTRY(2, tt-opb-pay.pnkodparametraplatewza$) 
    NO-LOCK:

    CREATE ttUsl.
    ASSIGN
       ttUsl.tName = term-obl.lnk-contract
       ttUsl.tarif = term-obl.price
       ttUsl.tCode = term-obl.alt-contract 
        .
    vVal = GetCode("�����㣨",term-obl.alt-contract).

    DO vCnt = 1 TO NUM-ENTRIES(vVal):

       IF LOOKUP(TRIM(ENTRY(vCnt,vVal)),oPokazLst) EQ 0 THEN DO:
          {additem.i oPokazLst TRIM(ENTRY(vCnt,vVal))}
       END.
    END.
END.

IF NUM-ENTRIES (oPokazLst) < 15 THEN
   oPokazLst = oPokazLst + FILL(",", 15 - NUM-ENTRIES (oPokazLst)) NO-ERROR.

END PROCEDURE.

&ENDIF

&IF DEFINED(opb-pay-budg) &THEN

{148n.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddCustomStringToHelp TERMINAL-SIMULATION 
PROCEDURE Validate148n:
/*------------------------------------------------------------------------------
  Purpose:     ����஫� ४����⮢ ���⥦� ᮣ��᭮ �ਪ��� 148�
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER oOK    AS LOGICAL       NO-UNDO.
   DEFINE OUTPUT PARAMETER oField AS WIDGET-HANDLE NO-UNDO.

   DEFINE VARIABLE vErrorMsg AS   CHARACTER        NO-UNDO.
   DEFINE VARIABLE vErr      AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vPersonId LIKE person.person-id NO-UNDO.

   vPersonId = INT64(GetAttrValue2("", 0, "person-id-save")) NO-ERROR.
   DO WITH FRAME fMain:
      ASSIGN
         tt-opb-pay.kbk$
         tt-opb-pay.okato-nalog$
         tt-opb-pay.pokst$
         tt-opb-pay.pokop$
         tt-opb-pay.poknp$
         tt-opb-pay.poknd$
         tt-opb-pay.pokdd$
         tt-opb-pay.uin$
         tt-opb-pay.pninn$
         tt-opb-pay.pnpoluwcatelw#inn$
      .
   END.

   /* ����� ���४������ �ଠ� ���� �����, ᤥ���� �� �ࠧ� */
   RUN LocalSetObject IN THIS-PROCEDURE.

   IF {assigned tt-opb-pay.pokst$} THEN    
      RUN chksgnnaldoc.p(tt-opb-pay.pokst$,
                     tt-opb-pay.kbk$,
                     tt-opb-pay.pokop$,
                     tt-opb-pay.poknp$,
                     tt-opb-pay.poknd$,
                     tt-opb-pay.pokdd$,
                     tt-opb-pay.acct,
                     OUTPUT vErr).
   /* ��� � ��� */
   IF NOT vErr THEN DO:
      vErrorMsg = check148n-uin-kbk(tt-opb-pay.uin$,
                                    tt-opb-pay.kbk$).
      IF {assigned vErrorMsg} THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "0", vErrorMsg).
         oField = tt-opb-pay.uin$:HANDLE IN FRAME fMain.
         vErr = YES.
      END.
   END.

/*
   /* ����� ���⥫�騪�, ��� � ��� ���⥫�騪� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-pokst-uin-pninn(tt-opb-pay.pokst$,
                                            tt-opb-pay.uin$,
                                            tt-opb-pay.pninn$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pninn$:HANDLE IN FRAME fMain.
   END.

   /* ��� ���⥫�騪� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-pninn(tt-opb-pay.pninn$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pninn$:HANDLE IN FRAME fMain.
   END.

   /* ���, ����� � ����� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-kbk-pokop-pokdd(tt-opb-pay.kbk$,
                                            tt-opb-pay.pokop$,
                                            tt-opb-pay.pokdd$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pokdd$:HANDLE IN FRAME fMain.
   END.

   /* ���, ����� � ��� �����⥫� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-kbk-pokop-pnpoluwcatelwinn(tt-opb-pay.kbk$,
                                                       tt-opb-pay.pokop$,
                                                       tt-opb-pay.pnpoluwcatelw#inn$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pnpoluwcatelw#inn$:HANDLE IN FRAME fMain.
   END.

   /* ��� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-kbk(tt-opb-pay.kbk$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.kbk$:HANDLE IN FRAME fMain.
   END.

   /* ����� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-okatonalog(tt-opb-pay.okato-nalog$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.okato-nalog$:HANDLE IN FRAME fMain.
   END.

   /* ��� � ����� ���⥫�騪� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-kbk-pokst(tt-opb-pay.kbk$,
                                      tt-opb-pay.pokst$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pokst$:HANDLE IN FRAME fMain.
   END.

   /* ��� � ����� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-kbk-pokop(tt-opb-pay.kbk$,
                                      tt-opb-pay.pokop$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pokop$:HANDLE IN FRAME fMain.
   END.

   /* �����, ����� � ����� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-pokop-poknd-pokdd(tt-opb-pay.pokop$,
                                              tt-opb-pay.poknd$,
                                              tt-opb-pay.pokdd$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pokdd$:HANDLE IN FRAME fMain.
   END.

   /* ����� � ����� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-pokop-poknd(tt-opb-pay.pokop$,
                                        tt-opb-pay.poknd$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.poknd$:HANDLE IN FRAME fMain.
   END.

   /* ��� � ��� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-uin-kbk(tt-opb-pay.uin$,
                                    tt-opb-pay.kbk$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.uin$:HANDLE IN FRAME fMain.
   END.

   /* ���, �᭮����� ���⥦� � �������� ��ਮ� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-kbk-pokop-poknp(tt-opb-pay.kbk$,
                                            tt-opb-pay.pokop$,
                                            tt-opb-pay.poknp$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.poknp$:HANDLE IN FRAME fMain.
   END.

   /* ���⥫�騪 � ��� ���⥫�騪� */
   IF NOT {assigned vErrorMsg} THEN DO:
      vErrorMsg = check148n-_personid-pninn(vPersonId,
                                            tt-opb-pay.pninn$).
      IF {assigned vErrorMsg} THEN
         oField = tt-opb-pay.pninn$:HANDLE IN FRAME fMain.
   END.

   IF {assigned vErrorMsg} THEN
      RUN Fill-SysMes IN h_tmess ("", "", "0", vErrorMsg).
   ELSE
*/
   IF NOT vErr THEN
      oOK = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/* $LINTFILE='f-opb-pay-common.i' */
/* $LINTMODE='1' */
/* $LINTENV ='2st' */
/* $LINTVSS ='$/ws2-tst/bq/4.1d/' */
/* $LINTUSER='paus' */
/* $LINTDATE='22/04/2016 15:15:29.505+03:00' */
/*prosignAtxHTUGGK+Ov3ETYzK2Esg*/