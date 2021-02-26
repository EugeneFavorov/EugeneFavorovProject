/*
               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: REE-117I.I
      Comment: <comment>
   Parameters: нет
         Uses:
      Used by:
      Created: 22.12.2006 20:02 duvu    
     Modified: 04.10.2007 19:43 duvu     <comment>
*/

/*DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.*/

{intrface.get db2l}
{intrface.get strng}

DEF VAR mCodNVal        AS CHARACTER           NO-UNDO.
DEF VAR mCodOpVal       AS CHARACTER           NO-UNDO.
DEF VAR mCorNasn        AS CHARACTER           NO-UNDO.
DEF VAR mI              AS INT64             NO-UNDO.
DEF VAR mMax            AS INT64             NO-UNDO.
DEF VAR mCliName        AS CHARACTER EXTENT 10 NO-UNDO.
DEF VAR mNameBank       AS CHARACTER           NO-UNDO.
DEF VAR mKodOpVal117    AS CHARACTER           NO-UNDO.
DEF VAR mPasportSdel    AS CHARACTER           NO-UNDO.
DEF VAR mSumValKontr    AS CHARACTER           NO-UNDO.
DEF VAR mNasnOfAcct     AS CHARACTER           NO-UNDO.
DEF VAR mOFF            AS LOGICAL             NO-UNDO.
DEF VAR mCustCat        AS CHARACTER           NO-UNDO.
DEF VAR mCustId         AS INT64             NO-UNDO.
DEF VAR mClientSurr     AS CHARACTER           NO-UNDO.
DEF VAR mClientListSurr AS CHARACTER           NO-UNDO.
DEF VAR mopop           AS INT64             NO-UNDO.
DEF VAR mopop1          AS INT64             NO-UNDO.
DEF VAR vBalExs         AS CHARACTER           NO-UNDO.
DEF VAR mOpOpenSurr     AS CHARACTER           NO-UNDO.
DEF VAR mPName          AS CHARACTER EXTENT 10 NO-UNDO.  
DEF VAR mAdr            AS CHARACTER EXTENT 10 NO-UNDO.
DEF VAR mBname          AS CHARACTER EXTENT 10 NO-UNDO.  
DEF VAR mCountryCode    AS CHARACTER           NO-UNDO.
DEF VAR mCountrySymb    AS CHARACTER           NO-UNDO.
DEF VAR mCountryCode2   AS CHARACTER           NO-UNDO.
DEF VAR mCountrySymb2   AS CHARACTER           NO-UNDO.
DEF VAR mINN            AS CHARACTER           NO-UNDO.
DEF VAR mKPP            AS CHARACTER           NO-UNDO.
DEF VAR mRegDate        AS CHARACTER           NO-UNDO.
DEF VAR mAddr           AS CHARACTER           NO-UNDO.
DEF VAR mPredpr         AS CHARACTER           NO-UNDO.
DEF VAR mMaskCor117     AS CHARACTER           NO-UNDO.
DEF VAR mLstKodVO       AS CHARACTER           NO-UNDO.
DEF VAR mBrCode         AS INT64               NO-UNDO.

DEFINE VARIABLE mIsOurBank AS LOGICAL NO-UNDO.
DEFINE VARIABLE mFlag2     AS LOGICAL     NO-UNDO.

DEFINE BUFFER cor-acct FOR acct.
DEFINE BUFFER xop      FOR op.
DEFINE BUFFER xop-entr FOR op-entry.

DEFINE VARIABLE m-handle      AS HANDLE    NO-UNDO.
DEFINE VARIABLE mhTable       AS HANDLE    NO-UNDO.
DEFINE VARIABLE mhQuery       AS HANDLE    NO-UNDO.

DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.

DEFINE VARIABLE mBankId       AS INT64       NO-UNDO.
DEFINE VARIABLE mBankCode     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mBankCode3    AS CHARACTER   NO-UNDO.

DEFINE BUFFER xop-entry FOR op-entry.
{parsin.def}

DEFINE TEMP-TABLE tt-117 NO-UNDO
   FIELD op     AS INT64     /*op.op*/
   FIELD c-cat  AS CHARACTER /*категория*/
   FIELD c-nam  AS CHARACTER /*имя для сортир*/
   FIELD c-acc  AS CHARACTER /*счет*/
   FIELD op-dt  AS DATE      /*дата*/
   FIELD c-id   AS INT64     /*идентиф для сорт*/
   FIELD f-nam  AS CHARACTER /*полное имя*/
   FIELD r-KOV  AS CHARACTER /*код валютной операции */
   FIELD r-PSd  AS CHARACTER /*номер паспорта сделки */
   FIELD r-SVC  AS DECIMAL   /*сумма в вал контракта из рекв. */
   FIELD r-VCO  AS CHARACTER /*валюта контракта из рекв.*/
   FIELD op-cu  AS CHARACTER /*валюта */
   FIELD op-su  AS DECIMAL   /*сумма в валюте*/
   FIELD tsurr  AS CHARACTER /* op.op & op-entry.op-entry для правильного выбора стороны в проводке одного клиента */
   FIELD napr   AS CHARACTER /* направление платежа */
   FIELD bname  AS CHARACTER /* наименование банка */
   FIELD bcode  AS CHARACTER /* бик банка */
   FIELD bctype AS CHARACTER /* тип идентификатора банка */
   FIELD ccode  AS CHARACTER /* код страны владельца счета (цифровой) */
   FIELD pcode  AS CHARACTER /* код страны получателя (цифровой) */
   FIELD cinn   AS CHARACTER /* инн */
   FIELD rdate  AS CHARACTER /* дата регистрации */
   FIELD adr    AS CHARACTER /* адрес */
   FIELD odate  AS CHARACTER /* дата открытия */
   FIELD pname1 AS CHARACTER /* наим. клиента1 */
   FIELD pname2 AS CHARACTER /* наим. клиента2 */
   FIELD pinn   AS CHARACTER /* инн клиента */
   FIELD numdog AS CHARACTER /* номер контракта */
   FIELD datdog AS CHARACTER /* дата контракта */
   FIELD ckpp   AS CHARACTER /* кпп */
   FIELD brcode AS CHARACTER /* код страны регистрации банка (цифровой) */
   FIELD dt-spr AS CHARACTER /* дата Справки */
   INDEX tsurr tsurr
.

DEFINE VARIABLE iParam AS CHARACTER NO-UNDO.

iParam = "*;2".

IF iParam EQ "" THEN
DO:
   MESSAGE "Не указаны назначения счетов!" VIEW-AS ALERT-BOX.
   RETURN.
END.
mNasnOfAcct = ENTRY(1,iParam,";").
IF INDEX(mNasnOfAcct,"НазнСч") NE 0 THEN
mNasnOfAcct = GetParamByNameAsChar(mNasnOfAcct, "НазнСч", "*пусто*").

IF NUM-ENTRIES (iParam,";") GE 2 THEN       /* Флаг двойного отображения */
   mFlag2 = (ENTRY(2, iParam, ";") EQ "2").

&IF DEFINED(ext-dates) = 0 &THEN
{getdates.i}.
&ELSE
ASSIGN
    beg-date = iBegDate
    end-date = iEndDate
.
&ENDIF

mNameBank   = FGetSetting("Банк","","").
mCodNVal    = FGetSetting("КодНацВал0406007","","").
mCorNasn    = FGetSetting("НазнСчМБР",?,"").
vBalExs     = FGetSetting("Выписки","СчетаКр","").
mMaskCor117 = FGetSetting("СтандТр","МаскаКорСч117","").
mLstKodVO   = FGetSetting("КодыВО","","").

{justamin}

FOR EACH op WHERE TRUE 
   AND op.op-date GE beg-date
   AND op.op-date LE end-date   AND op.filial-id EQ shFilial   /*AND op.op EQ 93466339*/
   NO-LOCK,
   EACH op-entry OF op 
   NO-LOCK:

   IF mopop NE op.op THEN
   DO:
      IF CAPS(GetXAttrValueEx("op",
                        STRING(op.op),
                        "ExcCodeVO",
                        "")) NE "ДА" THEN
      DO: 
         IF GetXAttrValueEx("op",
                            STRING(op.op),
                            "КодОпВал117",
                            "") EQ ""
                         AND GetXAttrValueEx("op",
                            STRING(op.op),
                            "NoCodeVO",
                            "") NE "Да"
              AND INDEX(op.details,"~{VO") EQ 0
              AND INDEX(op.details,"(VO") EQ 0 THEN
         DO:
            RUN getR-V.
         END.
         ELSE  
            RUN pGet117ReqN IN h_op (op.op,
                                     OUTPUT m-handle ).
      END.
      ELSE
      DO:
         RUN getR-V.
      END.      
      mopop = op.op.
   END.
   IF RETURN-VALUE NE "0" THEN
      NEXT.
      
   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + FILL("=",60)).
      
   IF mopop1 NE op.op THEN
   DO:
      RUN AcctDebList(RECID(op), vBalExs, OUTPUT mClientListSurr).
      mopop1 = op.op.
   END.
   mhTable = m-handle:DEFAULT-BUFFER-HANDLE.

   /* проведем анализ счетов */
   ASSIGN
      mCustCat     = ""
      mCustId      = 0
      mOpOpenSurr  = ""
   .

   mStrTMP = op-entry.acct-cr.

   IF mStrTMP EQ ? OR mStrTMP EQ "" THEN
   DO:
      FOR EACH xop-entry OF op WHERE RECID(xop-entry) NE RECID(op-entry) NO-LOCK,
         EACH cor-acct WHERE cor-acct.acct EQ xop-entry.acct-cr
                         AND cor-acct.currency EQ xop-entry.currency 
                         AND cor-acct.cust-cat NE "В"
                         AND CAN-DO(mCorNasn,cor-acct.contract)
         NO-LOCK:
         mStrTMP = cor-acct.acct.
         LEAVE.
     END.
   END.

   IF mStrTMP EQ ? OR mStrTMP EQ "" THEN
   DO:
      FOR EACH xop-entry OF op WHERE RECID(xop-entry) NE RECID(op-entry) NO-LOCK,
         EACH cor-acct WHERE cor-acct.acct EQ xop-entry.acct-cr
                         AND cor-acct.currency EQ xop-entry.currency 
                         AND cor-acct.cust-cat NE "В"
         NO-LOCK:
         mStrTMP = cor-acct.acct.
         LEAVE.
     END.
   END.

   IF mStrTMP EQ ? OR mStrTMP EQ "" THEN
   DO:
      FOR EACH xop-entry OF op WHERE RECID(xop-entry) NE RECID(op-entry) NO-LOCK,
         EACH cor-acct WHERE cor-acct.acct EQ xop-entry.acct-cr
                         AND cor-acct.currency EQ xop-entry.currency 
         NO-LOCK:
         mStrTMP = cor-acct.acct.
         LEAVE.
     END.
   END.

   RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op)).

   RUN cre-tt (op-entry.acct-db,mStrTMP,"REC",CAN-DO(mMaskCor117,op.ben-acct),
               INPUT-OUTPUT mCustCat, INPUT-OUTPUT mCustId).
   RUN cre-tt (op-entry.acct-cr,mStrTMP,"SEND",CAN-DO(mMaskCor117,op.ben-acct),
               INPUT-OUTPUT mCustCat, INPUT-OUTPUT mCustId).

END.

IF CAN-FIND(FIRST tt-117) THEN
DO:
   FOR EACH tt-117 BREAK BY tt-117.c-acc:
      RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1).
      IF FIRST-OF(tt-117.c-acc) THEN
      DO:
         ASSIGN
            mPredpr      = "NO"
            mCliName[1]  = ""
            mCountryCode = ""
            mCountrySymb = ""
            mINN         = ""
            mKPP         = ""
            mRegDate     = ""
            mAddr        = ""
            mOFF         = FALSE
         .
         FIND FIRST acct WHERE acct.acct EQ tt-117.c-acc NO-LOCK NO-ERROR.
         CASE acct.cust-cat:
            WHEN "Ч" THEN
               DO:
                  FIND FIRST person WHERE person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
                  IF AVAIL person THEN DO:
                     ASSIGN
                        mCliName[1]  = acct.details /*person.name-last + " " + person.first-names*/
                        tt-117.c-nam = SUBSTRING(mCliName[1],1,30)
                        tt-117.f-nam = mCliName[1]
                        mCountrySymb = person.country-id
                        mINN         = IF person.inn NE ? THEN person.inn ELSE ""
                        tt-117.cinn  = mINN
                        mKPP         = "" 
                        tt-117.ckpp  = mKPP
                        mRegDate     = GetXattrValueEx("person",STRING(person.person-id),"ДатаВПред","")
                        tt-117.rdate = mRegDate
                        mPredpr      = GetXattrValueEx("person",STRING(person.person-id),"Предпр","NO")
                     .
                     RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " mCliName[1] = " + mCliName[1]).
                     FIND LAST cust-ident 
                         WHERE cust-ident.cust-cat       EQ "Ч" 
                           AND cust-ident.cust-id        EQ person.person-id 
                           AND cust-ident.cust-code-type EQ "АдрПроп" NO-LOCK NO-ERROR.
                     IF AVAIL cust-ident THEN
                        mAddr = cust-ident.issue.
                     IF mAddr EQ "" THEN
                        mAddr = IF   person.address[2] NE "" THEN person.address[1] + "," + person.address[2]
                                ELSE person.address[1].
                     tt-117.adr   = mAddr.
                  END.
               END.
            WHEN "Ю" THEN
               DO:
                  FIND FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
                  IF AVAIL cust-corp THEN
                     ASSIGN
                        mCliName[1]  = cust-corp.cust-stat + " " + cust-corp.name-corp
                        tt-117.c-nam = SUBSTRING(mCliName[1],1,30)
                        tt-117.f-nam = mCliName[1]
                        mCountrySymb = cust-corp.country-id
                        mINN         = IF cust-corp.inn NE ? THEN cust-corp.inn ELSE ""
                        tt-117.cinn  = mINN
                        mKPP         = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"КПП","")
                        tt-117.ckpp  = mKPP
                        mRegDate     = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"RegDate","")
                        tt-117.rdate = mRegDate
                        mAddr        = IF cust-corp.addr-of-low[2] NE "" THEN cust-corp.addr-of-low[1] + ", " + cust-corp.addr-of-low[2] ELSE cust-corp.addr-of-low[1]
                        tt-117.adr   = mAddr
                     .
                     RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " mCliName[1] = " + mCliName[1]).  
               END.
            WHEN "Б" THEN
               DO:
                  FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
                  IF AVAIL banks THEN DO:
                     RUN GetBankInnKpp(BUFFER banks,
                                       OUTPUT mINN,
                                       OUTPUT mKPP)
                     NO-ERROR.
                     ASSIGN
                        mCliName[1]  = banks.name
                        tt-117.c-nam = SUBSTRING(mCliName[1],1,30)
                        tt-117.f-nam = mCliName[1]
                        mCountrySymb = banks.country-id
                        tt-117.cinn  = mINN 
                        tt-117.ckpp  = mKPP
                        mRegDate     = GetXattrValueEx("banks",STRING(banks.bank-id),"RegDate","")
                        tt-117.rdate = mRegDate
                        mAddr        = banks.law-address
                        tt-117.adr   = mAddr
                     .
                     RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " mCliName[1] = " + mCliName[1]).
                  END.
               END.
         END CASE.
         FIND FIRST country WHERE country.country-id EQ mCountrySymb NO-LOCK NO-ERROR.
         mCountryCode = IF AVAIL country
                        THEN STRING( country.country-alt-id, "999" )
                        ELSE "".
         tt-117.ccode = mCountryCode.
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " mCountryCode = " + mCountryCode + " tt-117.ccode = " + tt-117.ccode).
         IF  mCliName[1] EQ "" 
            OR (       acct.cust-cat EQ "Ч" 
               AND     mPredpr       EQ "NO" 
               AND  NOT {assigned tt-117.r-PSd}) THEN
         DO: 
            mOFF = TRUE.
&IF DEFINED(cb) &THEN
            numrec = numrec - 1.
&ENDIF
            DELETE tt-117.
         END.
      END.
      ELSE DO:
         IF mOFF THEN DO:
&IF DEFINED(cb) &THEN
            numrec = numrec - 1.
&ENDIF
            DELETE tt-117.
         END.
         ELSE
            ASSIGN
               tt-117.adr   = mAddr
               tt-117.rdate = mRegDate
               tt-117.cinn  = mINN
               tt-117.ckpp  = mKPP
               tt-117.ccode = mCountryCode 
               tt-117.c-nam = SUBSTRING(mCliName[1],1,30)
               tt-117.f-nam = mCliName[1]
            .
/*         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " mCountryCode = " + mCountryCode + " tt-117.ccode = " + tt-117.ccode).*/
/*         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " mCliName[1] = " + mCliName[1]).                                      */
      END.
   END.
END.

PROCEDURE cre-tt:
   DEFINE INPUT PARAMETER        iOpAcct    AS CHAR.
   DEFINE INPUT PARAMETER        iOpAcctCor AS CHAR.
   DEFINE INPUT PARAMETER        iWhichBank AS CHAR.
   DEFINE INPUT PARAMETER        iFlag      AS LOGICAL.
   DEFINE INPUT-OUTPUT PARAMETER oCustCat   AS CHAR.
   DEFINE INPUT-OUTPUT PARAMETER oCustId    AS INT64.

   DEFINE VAR vCliName     AS CHAR EXTENT 2 NO-UNDO.
   DEFINE VAR vNum-cont    AS CHAR NO-UNDO.
   DEFINE VAR vDat-cont    AS CHAR NO-UNDO.
   DEFINE VAR vDogOpenAcct AS CHAR NO-UNDO.
   DEFINE VAR vKodOpVal117 AS CHAR NO-UNDO.
   
   DEFINE VARIABLE vBalAcctBnk   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vXAcct        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPas-cont     AS CHARACTER NO-UNDO.

   DEFINE BUFFER xacct FOR acct.
   
   FIND FIRST acct WHERE acct.acct EQ iOpAcct AND
&IF DEFINED(oracle) EQ 0 &THEN
            op-entry.currency BEGINS acct.currency 
&ELSE
            (acct.currency EQ op-entry.currency OR acct.currency EQ "") 
&ENDIF
      NO-LOCK NO-ERROR. 

   IF AVAIL acct 
      AND CAN-DO(mNasnOfAcct,acct.contract)
      AND CAN-DO("Ч,Ю,Б",acct.cust-cat)
      AND (mOpOpenSurr   NE ""       OR
           acct.cust-cat NE oCustCat OR
           acct.cust-id  NE oCustId  OR
           mFlag2
          ) THEN
   DO:
&IF DEFINED(cb) &THEN
   IF (mFrameCustId NE "" 
      AND acct.cust-cat EQ mFrameCustCut
&IF DEFINED(frmSel) &THEN
      AND CAN-FIND(ttRetVal WHERE acct.cust-id EQ INT64(ttRetVal.pickvalue) NO-LOCK) )
&ELSE
      AND acct.cust-id  EQ INT64(mFrameCustId)) 
&ENDIF
      OR mFrameCustId EQ "" THEN DO:
&ENDIF
      IF     mClientListSurr NE ""
         AND mOpOpenSurr     EQ "" 
         AND iwhichbank EQ "SEND" THEN
      DO:
         mClientSurr = acct.cust-cat + "^" + STRING(acct.cust-id).
         IF LOOKUP(mClientSurr,mClientListSurr) GT 0 AND 
            NOT mFlag2 THEN
            RETURN "0".
      END.
      IF     mOpOpenSurr NE "" 
         AND iwhichbank  EQ "SEND" THEN
      FOR EACH tt-117 WHERE tt-117.tsurr EQ mOpOpenSurr:
         DELETE tt-117.
      END.
      
      ASSIGN
         oCustCat = acct.cust-cat
         oCustId  = acct.cust-id
      .

      CREATE QUERY mhQuery.

      mhQuery:SET-BUFFERS (mhTable).
      mhQuery:QUERY-PREPARE ("FOR EACH " + m-handle:NAME + " NO-LOCK").
      mhQuery:QUERY-OPEN ().
      {for_dqry.i mhQuery}
         IF    (    iwhichbank EQ "REC"
                 AND mhTable:BUFFER-FIELD (7):BUFFER-VALUE EQ op-entry.op-entry)
            OR (    iwhichbank NE "REC"
                 AND mhTable:BUFFER-FIELD (8):BUFFER-VALUE EQ op-entry.op-entry) THEN
         DO:
&IF DEFINED(cb) &THEN
            IF mFrameKodOp NE "" 
               AND NOT CAN-DO(mFrameKodOp,mhTable:BUFFER-FIELD (1):BUFFER-VALUE) THEN
                NEXT.
&ENDIF
            CREATE tt-117.
            ASSIGN
&IF DEFINED(cb) &THEN
               numrec        = numrec + 1
&ENDIF
               tt-117.op     = op.op
               tt-117.c-cat  = acct.cust-cat
               tt-117.c-nam  = ""
               tt-117.c-acc  = iOpAcct
               tt-117.op-dt  = op.op-date
               tt-117.f-nam  = ""
               tt-117.c-id   = acct.cust-id
               tt-117.r-KOV  = mhTable:BUFFER-FIELD (1):BUFFER-VALUE
               
               tt-117.r-SVC  = mhTable:BUFFER-FIELD (4):BUFFER-VALUE
               tt-117.r-VCO  = IF tt-117.r-SVC GT 0
                               THEN mhTable:BUFFER-FIELD (5):BUFFER-VALUE
                               ELSE ""
               tt-117.op-cu  = IF acct.currency EQ "" THEN mCodNVal
                                                      ELSE op-entry.currency
               tt-117.op-su  = IF iwhichbank EQ "REC" THEN mhTable:BUFFER-FIELD (3):BUFFER-VALUE
                                                      ELSE mhTable:BUFFER-FIELD (6):BUFFER-VALUE
               tt-117.tsurr  = STRING(op.op,"9999999999") + "," + STRING(op-entry.op-entry,"9999999999")
               mOpOpenSurr   = IF     iwhichbank                                 EQ "REC"
                                  AND mhTable:BUFFER-FIELD("db-cr"):BUFFER-VALUE EQ "К"
                               THEN tt-117.tsurr
                               ELSE ""
               tt-117.napr   = IF iwhichbank EQ "REC" THEN (IF acct.side EQ "А" THEN "1" ELSE "2")
                               ELSE (IF acct.side EQ "А" THEN "2" ELSE "1")
               tt-117.odate  = STRING(acct.open-date)
               tt-117.pname1 = IF op.name-ben = ? THEN "" ELSE op.name-ben
               tt-117.pname2 = ""
               tt-117.pinn   = IF op.inn = ?      THEN "" ELSE op.inn
               tt-117.brcode = ""
               tt-117.pcode  = "" 
               mIsOurBank    = NO
               vNum-cont     = mhTable:BUFFER-FIELD (10):BUFFER-VALUE
               vDat-cont     = STRING(mhTable:BUFFER-FIELD (11):BUFFER-VALUE, "99/99/9999")
               vPas-cont     = mhTable:BUFFER-FIELD (2):BUFFER-VALUE 
               vKodOpVal117  = GetXAttrValueEx("op",
                                               STRING(op.op),
                                               "КодОпВал117",
                                               "")
            .
            
            /*ccc*/
            mCountrySymb2 = "".
            mCountryCode2 = "".
            mCountrySymb2 = GetXattrValueEx("op", STRING(op.op), "bank-country-send", "") + GetXattrValueEx("op", STRING(op.op), "bank-country-rec", "").
            FIND FIRST country WHERE country.country-id EQ mCountrySymb2 NO-LOCK NO-ERROR.
            mCountryCode2 = IF AVAIL country
                     THEN STRING(country.country-alt-id,"999")
                     ELSE "".
            tt-117.brcode = mCountryCode2.
            RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " mCountryCode2 = " + mCountryCode2 + "  tt-117.brcode = " + tt-117.brcode).
            
            mCountrySymb2 = "".
            mCountryCode2 = "".
            IF iWhichBank EQ "REC" THEN
            DO:
               mCountrySymb2 = GetXattrValueEx("op", STRING(op.op),"country-rec","").
               FIND FIRST country WHERE country.country-id EQ mCountrySymb2 NO-LOCK NO-ERROR.
               mCountryCode2 = IF AVAIL country
                        THEN STRING(country.country-alt-id,"999")
                        ELSE "".
               tt-117.pcode = mCountryCode2.
               RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " mCountryCode2 = " + mCountryCode2 + "  tt-117.pcode = " + tt-117.pcode).
            END.
            IF iWhichBank EQ "SEND" THEN
            DO:
               mCountrySymb2 = GetXattrValueEx("op", STRING(op.op),"country-send","").
               FIND FIRST country WHERE country.country-id EQ mCountrySymb2 NO-LOCK NO-ERROR.
               mCountryCode2 = IF AVAIL country
                        THEN STRING(country.country-alt-id,"999")
                        ELSE "".
               tt-117.pcode = mCountryCode2.
               RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " mCountryCode2 = " + mCountryCode2 + "  tt-117.pcode = " + tt-117.pcode).
            END.
            
            RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 + " tt-117.pname2 = " + tt-117.pname2).
            /*ccc*/
            
            IF      vKodOpVal117              NE ""
                AND NUM-ENTRIES(vKodOpVal117) GE 2 
                AND {assigned vPas-cont}
            THEN DO:
               IF IsResident(oCustCat, oCustId) THEN
                  ASSIGN
                     tt-117.r-PSd  = mhTable:BUFFER-FIELD (2):BUFFER-VALUE
                     tt-117.numdog = ""
                     tt-117.datdog = ""
                  .
               ELSE 
                  ASSIGN
                     tt-117.r-PSd = ""
                     tt-117.numdog = IF vNum-cont NE "" THEN vNum-cont ELSE GetXattrValueEx("op", STRING(op.op), "ВалКонтракт", "")
                     tt-117.datdog = IF vDat-cont NE ?  THEN vDat-cont ELSE GetXattrValueEx("op", STRING(op.op), "ВалДата", "")
                  .
            END.
            ELSE
               ASSIGN
                  tt-117.r-PSd  = mhTable:BUFFER-FIELD (2):BUFFER-VALUE
                  tt-117.numdog = IF vNum-cont NE "" THEN vNum-cont ELSE GetXattrValueEx("op", STRING(op.op), "ВалКонтракт", "")
                  tt-117.datdog = IF vDat-cont NE ?  THEN vDat-cont ELSE GetXattrValueEx("op", STRING(op.op), "ВалДата", "")
               .
             

/*            IF CAN-DO(mLstKodVO, tt-117.r-KOV) AND                                                      */
/*               NOT ( {assigned tt-117.r-PSd  } OR                                                       */
/*                     {assigned tt-117.numdog } OR                                                       */
/*                     {assigned tt-117.datdog }                                                          */
/*                   ) THEN                                                                               */
/*            DO:                                                                                         */
/*               vDogOpenAcct = GetXattrValueEx("acct", acct.acct + "," + acct.currency, "ДогОткрЛс", "").*/
/*               IF {assigned vDogOpenAcct}                                                               */
/*               THEN ASSIGN                                                                              */
/*                  tt-117.datdog = GetEntries(1,vDogOpenAcct,",","")                                     */
/*                  tt-117.numdog = GetEntries(2,vDogOpenAcct,",","")                                     */
/*               .                                                                                        */
/*               ELSE                                                                                     */
/*                  FOR EACH loan-acct WHERE loan-acct.acct     EQ acct.acct                              */
/*                                       AND loan-acct.currency EQ acct.currency NO-LOCK,                 */
/*                  FIRST loan OF loan-acct NO-LOCK:                                                      */
/*                     ASSIGN                                                                             */
/*                        tt-117.datdog = STRING(loan.open-date, "99/99/9999").                           */
/*                        tt-117.numdog = loan.doc-ref.                                                   */
/*                     .                                                                                  */
/*                  END.                                                                                  */
/*               IF NOT {assigned tt-117.datdog} THEN                                                     */
/*                  IF {assigned STRING(op.doc-date)} THEN                                                */
/*                     tt-117.datdog = STRING(op.doc-date, "99/99/9999").                                 */
/*                  ELSE                                                                                  */
/*                     tt-117.datdog = "".                                                                */
/*               IF NOT {assigned tt-117.numdog} THEN                                                     */
/*                  IF {assigned op.doc-num} THEN                                                         */
/*                     tt-117.numdog = op.doc-num.                                                        */
/*                  ELSE                                                                                  */
/*                     tt-117.numdog = "".                                                                */
/*            END.                                                                                        */
            
            IF iFlag THEN DO:
               FIND FIRST op-bank OF op NO-LOCK NO-ERROR.
               IF AVAIL(op-bank) THEN 
               DO:
                  ASSIGN
                     tt-117.bcode = (IF (op-bank.bank-code = ? OR op-bank.bank-code = "") 
                                    AND op-bank.bank-code-type = "BIC" 
                                    THEN "НР" 
                                    ELSE (IF (op-bank.bank-code = ? OR op-bank.bank-code = "") 
                                          THEN "" 
                                          ELSE op-bank.bank-code)
                                    )
                     tt-117.bname = (IF op-bank.bank-name = ? THEN "" ELSE op-bank.bank-name).
                  
                  RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.bctype = " + tt-117.bctype +  " tt-117.bcode = " + tt-117.bcode).
                  
/*                  IF op-bank.bank-code-type <> "МФО-9" THEN                                                                                   */
/*                  DO:                                                                                                                         */
/*                     FOR FIRST banks-code OF op-bank NO-LOCK,                                                                                 */
/*                               banks OF banks-code NO-LOCK:                                                                                   */
/*                         tt-117.brcode = banks.country-id.                                                                                    */
/*                     END.                                                                                                                     */
/*                  END.                                                                                                                        */
/*                  RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.brcode = " + tt-117.brcode).*/
               END.
            END.

            IF NOT iFlag THEN DO:
               {empty Info-Store}
               RUN Identify-Client (iwhichbank).

               IF AVAIL Info-Store THEN
                  ASSIGN
                     tt-117.pname1 = IF NOT {assigned tt-117.pname1} THEN Info-Store.name ELSE tt-117.pname1
                     tt-117.pinn   = Info-Store.inn
                  .
               RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 + " tt-117.pname2 = " + tt-117.pname2).
               
               {empty Info-Store}
               RUN Identify-Banks (iwhichbank).
               IF AVAIL Info-Store THEN DO:
                  ASSIGN
                     tt-117.bcode = Info-Store.code
                     tt-117.bctype = Info-Store.code-type
                     tt-117.bname  = Info-Store.name
                  .
                  RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.bctype = " + tt-117.bctype +  " tt-117.bcode = " + tt-117.bcode).
/*                  FIND FIRST banks WHERE banks.bank-id = Info-Store.bank-id NO-LOCK NO-ERROR.                                                 */
/*                  IF AVAIL banks THEN                                                                                                         */
/*                  DO:                                                                                                                         */
/*                     IF CAN-FIND(FIRST banks-code OF banks WHERE banks-code.bank-code-type <> "МФО-9")                                        */
/*                     THEN tt-117.brcode = banks.country-id.                                                                                   */
/*                  END.                                                                                                                        */
/*                  RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.brcode = " + tt-117.brcode).*/
               END.  
               ELSE 
               DO:
                  FIND FIRST cor-acct
                       WHERE cor-acct.acct EQ iOpAcctCor
                         AND op-entry.currency BEGINS cor-acct.currency
                         AND CAN-DO(mCorNasn,cor-acct.contract) NO-LOCK NO-ERROR.
                  IF AVAIL cor-acct THEN DO:
                     IF cor-acct.cust-cat EQ "Б" THEN DO:
                        FIND FIRST banks      WHERE banks.bank-id      EQ cor-acct.cust-id NO-LOCK NO-ERROR.
                        FIND FIRST banks-code WHERE banks-code.bank-id EQ cor-acct.cust-id
                               AND banks-code.bank-code-type EQ "МФО-9" NO-LOCK NO-ERROR.
                        IF AVAIL banks THEN DO:
                           tt-117.bname = banks.name.
                           IF AVAIL banks-code  THEN
                           DO:
                              tt-117.bcode = banks-code.bank-code.
                              tt-117.bctype = "МФО-9".
                              RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.bctype = " + tt-117.bctype +  " tt-117.bcode = " + tt-117.bcode).
			                  END.
                           ELSE DO:
                              tt-117.bcode = "НР".
/*                              tt-117.brcode = banks.country-id.*/
                              RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.bcode = " + tt-117.bcode).
/*                              RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.brcode = " + tt-117.brcode).*/
                           END.
                        END.
                     END.
                     RELEASE cor-acct.
                  END.
                  ELSE
                  DO:
                     ASSIGN
                        tt-117.bname = dept.name-bank
                        tt-117.bcode = GetXAttrValueEx("branch", dept.branch, "БанкМФО", FGetSetting("БанкМФО", ? , ""))
                        tt-117.bctype = "МФО-9"
                        mIsOurBank   = YES
/*                        tt-117.brcode = GetXattrValueEx("op", STRING(op.op), "bank-country-send", "") +*/
/*                                        GetXattrValueEx("op", STRING(op.op), "bank-country-rec", "")   */
                     .
                     RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.bctype = " + tt-117.bctype +  " tt-117.bcode = " + tt-117.bcode).
                  END.
               END.

               IF mIsOurBank OR tt-117.bcode EQ "" THEN 
               DO:
                  FIND FIRST cor-acct
                       WHERE cor-acct.acct     EQ     iOpAcctCor
                         AND cor-acct.currency BEGINS op-entry.currency NO-LOCK NO-ERROR.
                  IF NOT AVAIL cor-acct THEN
                     FIND FIRST cor-acct WHERE
                                cor-acct.acct     EQ iOpAcctCor
                            AND cor-acct.currency EQ "" NO-LOCK NO-ERROR.
 
                  IF AVAIL cor-acct /*AND cor-acct.cust-cat EQ "В"*/ THEN DO:
                     IF tt-117.pname1 EQ "" THEN 
                     DO:
                        IF iWhichBank = "SEND" THEN DO:
                           {find-act.i &bact=xacct &acct=op-entry.acct-db}
                           IF AVAIL xacct THEN DO:
                              vXAcct = xacct.acct.
                              IF CAN-DO("Ю,Ч,Б",xacct.cust-cat) THEN DO:
                                 {getcust.i &pref=x &OFFinn=yes &OFFsigns=yes &name=vCliName &inn=tt-117.pinn}
                                 ASSIGN
                                    tt-117.pname1 = vCliName[1]
                                    tt-117.pname2 = vCliName[2]
                                 .
                                 RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 + " tt-117.pname2 = " + tt-117.pname2).
                              END.
                           END.
                        END.
                        ELSE DO:
                           {find-act.i &bact=xacct &acct=op-entry.acct-cr}
                           IF AVAIL xacct THEN DO:
                              vXAcct = xacct.acct.
                              IF CAN-DO("Ю,Ч,Б",xacct.cust-cat) THEN DO:
                                 {getcust.i &pref=x &OFFinn=yes &OFFsigns=yes &name=vCliName &inn=tt-117.pinn}
                                 ASSIGN
                                    tt-117.pname1 = vCliName[1]
                                    tt-117.pname2 = vCliName[2]
                                 .
                                 RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 + " tt-117.pname2 = " + tt-117.pname2).
                              END.
                           END.
                        END.
                     END.
                     {empty Info-Store}
                     RUN Our-Bank.
                     RUN General-Bank.
                     ASSIGN
                        tt-117.bcode  = Info-Store.code
                        tt-117.bctype = Info-Store.code-type
                        tt-117.bname  = Info-Store.Sh-name
                     .
                     RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.bctype = " + (IF tt-117.bctype EQ ? THEN "     " ELSE tt-117.bctype) +  " tt-117.bcode = " + tt-117.bcode).
/*                     FIND FIRST banks WHERE banks.bank-id = Info-Store.bank-id NO-LOCK NO-ERROR.                                                    */
/*                     IF AVAIL banks THEN                                                                                                            */
/*                     DO:                                                                                                                            */
/*                        IF CAN-FIND(FIRST banks-code OF banks WHERE banks-code.bank-code-type <> "МФО-9")                                           */
/*                        THEN tt-117.brcode = banks.country-id.                                                                                      */
/*                        RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.brcode = " + tt-117.brcode).*/
/*                     END.                                                                                                                           */

                     IF tt-117.pname1 EQ "" THEN
                     ASSIGN
                        tt-117.pinn   = Info-Store.inn
                        tt-117.pname1 = Info-Store.Sh-name
                        tt-117.pname2 = ""
                     .
                     RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 + " tt-117.pname2 = " + tt-117.pname2).
                  END.
               END.
               /* данные из cust-role */
               FIND FIRST cust-role WHERE 
                          cust-role.file-name  EQ "op"
                      AND cust-role.surrogate  EQ STRING(op.op)
                      AND cust-role.class-code EQ IF iwhichbank EQ "REC" THEN "Benef-Cust"
                                                                         ELSE "Order-Cust" 
               NO-LOCK NO-ERROR.
/*               IF AVAIL cust-role THEN                  */
/*                  ASSIGN                                */
/*                     tt-117.pname1 = cust-role.cust-name*/
/*                     tt-117.pinn   = cust-role.inn      */
/*                  .                                     */
               RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 + " tt-117.pname2 = " + tt-117.pname2).
               FIND FIRST cust-role WHERE 
                          cust-role.file-name  EQ "op"
                      AND cust-role.surrogate  EQ STRING(op.op)
                      AND cust-role.class-code EQ IF iwhichbank EQ "REC" THEN "Benef-Inst"
                                                                         ELSE "Order-Inst"
               NO-LOCK NO-ERROR.
               IF AVAIL cust-role THEN 
               DO:
                  ASSIGN
                     tt-117.bname = cust-role.cust-name
                     tt-117.bcode = cust-role.cust-code
/*                     tt-117.brcode = IF cust-role.cust-code-type <> "МФО-9" THEN cust-role.country-id ELSE ""*/
                  .
                  RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.bcode = " + tt-117.bcode).
/*                  RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.brcode = " + tt-117.brcode).*/
                  FIND FIRST banks WHERE
                     banks.bank-id = INT64(cust-role.cust-id)
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE banks THEN DO:
                     FIND banks-code WHERE
                        banks-code.bank-id   = banks.bank-id AND
                        banks-code.bank-code = cust-role.cust-code
                     NO-LOCK NO-ERROR.
                     IF AVAILABLE banks-code AND NOT AMBIG(banks-code) THEN
                        tt-117.bctype = banks-code.bank-code-type.
                  END.
               END.
            END. /* iFlag */
            vBalAcctBnk = FGetSetting("ПлатДок","БалСчБнк","").
            RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " vBalAcctBnk = " + vBalAcctBnk + " vXAcct = " + vXAcct).
/*            IF CAN-DO(vBalAcctBnk,vXAcct) THEN              */
/*            /* Используем наименование и ИНН нашего банка */*/
/*            ASSIGN                                          */
/*               tt-117.pname1 = tt-117.bname                 */
/*               tt-117.pname2 = ""                           */
/*               tt-117.pinn   = FGetSetting("ИНН", "", "")   */
/*            .                                               */
/*            IF NOT {assigned tt-117.pname1} THEN            */
/*            /* Используем наименование и ИНН нашего банка */*/
/*            ASSIGN                                          */
/*               tt-117.pname1 = tt-117.bname                 */
/*               tt-117.pname2 = ""                           */
/*               tt-117.pinn   = FGetSetting("ИНН", "", "")   */
/*            .                                               */
            RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","tt-117.op = " + STRING(tt-117.op) + " tt-117.pname1 = " + tt-117.pname1 + " tt-117.pname2 = " + tt-117.pname2).
/*            RUN GetDigitalCountryId4NR(tt-117.brcode, OUTPUT mBrCode).                                                                  */
/*            tt-117.brcode = IF mBrCode = ? THEN ""                                                                                      */
/*                                           ELSE STRING(mBrCode).                                                                        */
/*            RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.brcode = " + tt-117.brcode).*/

            /*---------------------------------------------------------------------*/
            /*    182509 - Если код страны не найден, берем его из ДР BIC          */
 
/*            IF tt-117.brcode = "" THEN DO:                                                                                                 */
/*               mBankCode = substring(tt-117.bcode, 5, 2).                                                                                  */
/*               FOR FIRST signs                                                                                                             */
/*                  WHERE signs.file-name = "country"                                                                                        */
/*                    AND signs.code = "ALFA-2"                                                                                              */
/*                    AND signs.code-value = mBankCode                                                                                       */
/*                    NO-LOCK:                                                                                                               */
/*                  mBankCode3 = signs.surrogate.                                                                                            */
/*                  FOR FIRST country                                                                                                        */
/*                      WHERE country.country-id = mBankCode3                                                                                */
/*                      NO-LOCK:                                                                                                             */
/*                                                                                                                                           */
/*                      tt-117.brcode = STRING(country.country-alt-id, "999").                                                               */
/*                  END.                                                                                                                     */
/*               END.                                                                                                                        */
/*               RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}","op.op = " + STRING(op.op) + " tt-117.brcode = " + tt-117.brcode).*/
/*            END.                                                                                                                           */
            /*---------------------------------------------------------------------*/
            tt-117.dt-spr = GetXattrValueEx("op",STRING(op.op),"ДатаСправкиВО","").
         END.
      END.
      mhQuery:QUERY-CLOSE ().
      DELETE OBJECT mhQuery.
   END.
&IF DEFINED(cb) &THEN
   END.
&ENDIF
   RETURN "0".
END PROCEDURE.

      /*данные по кредиту обрабатываем только если 
        счет кредита не принадлежит владельцу счета дебета */
PROCEDURE AcctDebList:
   DEF INPUT  PARAMETER iRecid  AS RECID.
   DEF INPUT  PARAMETER iBalExs AS CHARACTER.
   DEF OUTPUT PARAMETER oClientListSurr AS CHAR.
   
   DEF VAR vClientSurr  AS CHARACTER NO-UNDO.
   
   oClientListSurr = "".

   FOR FIRST xop WHERE RECID(xop) EQ iRecid NO-LOCK:

      IF    GetXAttrValueEx("op",
                            STRING(xop.op),
                            "КодОпВал117",
                            "") NE "" 
         OR GetXAttrValueEx("op",
                            STRING(xop.op),
                            "NoCodeVO",
                            "") EQ "Да"
      THEN
         RETURN.
   END.

   
   FOR EACH xop-entr OF xop WHERE xop-entr.acct-db NE ?
                             AND NOT CAN-DO(iBalExs,xop-entr.acct-db) NO-LOCK,
       FIRST acct WHERE acct.acct EQ xop-entr.acct-db AND
&IF DEFINED(oracle) EQ 0 &THEN
                        xop-entr.currency BEGINS acct.currency
&ELSE
            (acct.currency EQ xop-entr.currency OR acct.currency EQ "") 
&ENDIF
      NO-LOCK:
      IF CAN-DO(mNasnOfAcct,acct.contract) THEN
      DO:
         vClientSurr = acct.cust-cat + "^" + STRING(acct.cust-id).
         {additem.i oClientListSurr vClientSurr}
      END.
   END.
END PROCEDURE.

PROCEDURE getR-V:
   RETURN "35".
END PROCEDURE.

PROCEDURE GetDigitalCountryId4NR.
    DEFINE INPUT  PARAMETER iCountryId LIKE country.country-id     NO-UNDO.
    DEFINE OUTPUT PARAMETER oDigitalId LIKE country.country-alt-id NO-UNDO.

    DEFINE BUFFER country FOR country.

    IF iCountryId <> "RUS" THEN
        FIND FIRST country WHERE
            country.country-id = iCountryId
        NO-LOCK NO-ERROR.
    oDigitalId = IF AVAILABLE country THEN country.country-alt-id
                                      ELSE ?.
END PROCEDURE.

PROCEDURE GetBankInnKpp.
    DEFINE        PARAMETER BUFFER banks FOR banks.
    DEFINE OUTPUT PARAMETER oINN         AS  CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oKPP         AS  CHARACTER NO-UNDO.

    IF NOT AVAILABLE banks THEN
        RETURN ERROR.
    oINN = GetBankInn("bank-id",STRING(banks.bank-id)) NO-ERROR.
    RUN GetBankCodeValue(BUFFER banks, "КПП", OUTPUT oKPP) NO-ERROR.
    ASSIGN
        oINN = banks.inn
               WHEN NOT {assigned oINN}
        oKPP = GetXAttrValue("banks", Surrogate(BUFFER banks:HANDLE), "КПП")
               WHEN NOT {assigned oKPP}
    .
    ASSIGN
        oINN = ""
               WHEN NOT {assigned oINN}
        oKPP = IF {assigned oKPP} THEN TRIM(ENTRY(1, oKPP))
                                  ELSE ""
    .
END PROCEDURE.

PROCEDURE GetBankCodeValue.
    DEFINE       PARAMETER BUFFER banks   FOR  banks.
    DEFINE INPUT PARAMETER iBankCodeType  LIKE banks-code.bank-code-type NO-UNDO.
    DEFINE OUTPUT PARAMETER oBankCodeValue LIKE banks-code.bank-code     NO-UNDO.

    DEFINE BUFFER banks-code FOR banks-code.

    IF NOT (AVAILABLE banks AND {assigned iBankCodeType}) THEN
        RETURN ERROR.
    FIND FIRST banks-code OF banks WHERE
        banks-code.bank-code-type = iBankCodeType
    NO-LOCK NO-ERROR.
    IF AVAILABLE banks-code THEN
        oBankCodeValue = banks-code.bank-code.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='21/10/2015 18:45:54.458+04:00' */
/* $LINTUSER='krok' */
/* $LINTMODE='1' */
/* $LINTFILE='ree-117i.i' */
/*prosignNbxzQgaiKcp1UddlRioxHw*/