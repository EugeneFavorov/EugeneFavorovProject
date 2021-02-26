/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: ovn-nps.p
      Comment: объявление на взнос наличными
               (Открытая система печати)
      Comment: NO COMMENT
   Parameters: Нeт
         Uses: 
      Used by:
      Created: 23/09/2004 kraw (0026941)
     Modified: 15/01/2008 kraw (0086034) Сумма цифрами без "="
     Modified: 16/12/2011 kraw (0161338) Восстановили имя и ИНН получателя.
     Modified: 21/02/2012 kraw (0165983) в LAW_318p убираем ИНН из имени
     Modified: 30/06/2015 kraw (0248554) выбор формы со свободным полем или без оного
     Modified: 24/11/2015 kraw (0272233) Номер счета печатаем без филиала
*/
&GLOBAL-DEFINE op-entry yes
&GLOBAL-DEFINE tt-op-entry yes

{globals.i}                                 /* глобальные переменные         */
{intrface.get xclass}
{signature.pro}

DEFINE INPUT PARAMETER iRID AS RECID NO-UNDO.

{prn-doc.def &with_proc=YES}
{sumstrfm.i}
{intrface.get cust}
{intrface.get db2l}
{pp-uni.var
   &multiline-author = YES,
   &FILE_sword_p     = YES
}                           /* определение переменных        */
{pp-uni.err}                /* сообщения об ошибках          */
{pp-uni.prg}                /* описание стандартных процедур */

{mo-uni.dec}

{def-wf.i NEW}
{mo-pars.i}
{branch.pro}

DEFINE VARIABLE mPName  AS CHARACTER EXTENT  2 NO-UNDO.

DEFINE VARIABLE mAmt1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchAmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchDec    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmtStr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDate     AS DATE      NO-UNDO.
DEFINE VARIABLE mDDate    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrTMP   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrTMP1V AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mStrTMP2V AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mItem     AS INT64 INITIAL 1   NO-UNDO.
DEFINE VARIABLE mDprId    AS INT64     NO-UNDO.
DEFINE VARIABLE mAmt      AS DECIMAL INITIAL 0.0 NO-UNDO.
DEFINE VARIABLE mBankName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankName2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mPlName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSumFormat AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSumSep    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsINN2    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mTypeChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrPar    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCorNameOb AS CHARACTER NO-UNDO. /*НП КорНаимОбВН*/
DEFINE VARIABLE mFldNameBnk AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankBIK   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankCity  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsPrtCity AS LOGICAL   NO-UNDO.

&IF DEFINED(LAW_318p) <> 0 &THEN
   DEFINE VARIABLE mPlNameP  AS CHARACTER EXTENT 4 NO-UNDO.
   DEFINE VARIABLE mPoName     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mPoINN      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mPoKPP      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mPoOKATO    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mPoOKTMO    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mNaimOKTMO  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mPoRAcct    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mPoBankName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mPoBankCode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mCodeDoc    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mchWorkerBuhP  AS CHARACTER NO-UNDO. /* Должность */
   DEFINE VARIABLE mchWorkerKasP  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mchWorkerBuh   AS CHARACTER NO-UNDO. /* ФИО */ 
   DEFINE VARIABLE mchWorkerKas   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mOurBank   AS LOGICAL   NO-UNDO.
&ENDIF
DEFINE BUFFER recacct   FOR acct.
DEFINE BUFFER xop-entry FOR op-entry.

ASSIGN
   mCorNameOb = FGetSetting("ПлатДок", "КорНаимОбВН", "?")
   mStrPar = FGetSetting("ПлатДок", "ОбВзНал", "")
   mBankName = FGetSetting("Банк",    ?,"")
   mBankBIK  = FGetSetting("БанкМФО", ?,"")
   mBankCity = ", " + FGetSetting("БанкГород", ?,"")
&IF DEFINED(LAW_318p) <> 0 &THEN
   mIsPrtCity = (FGetSetting("ПлатДок", "ВыводМест","Да") = "Да")
&ELSE
   mIsPrtCity = YES
&ENDIF
   mFldNameBnk = IF mCorNameOb =  "Да" THEN "short-name"
                                    ELSE "name".

RUN ProcProcessFreeField(mStrPar,?,?).

RUN GetSeparator318p(OUTPUT mSumSep).
mTypeChar = FGetSetting("Принтер","ТипПрочерк","").

FIND FIRST op WHERE RECID(op) = iRID NO-LOCK NO-ERROR.

IF NOT AVAILABLE op THEN
DO:
  MESSAGE "Нет записи <op>".
  RETURN.
END.

FOR EACH op-entry OF op NO-LOCK:

   &IF DEFINED(LAW_318p) <> 0 &THEN
      IF mItem >  4 THEN
         LEAVE.
   &ELSE
      IF mItem >  10 THEN
         LEAVE.
   &ENDIF

   ACCUMULATE op-entry.amt-rub (TOTAL).

   RUN Insert_TTName("Symb" + STRING(mItem,"9"), op-entry.symbol).

   mSumFormat = getFormatStr(11, ",", YES).
   mStrTmp    = AmtStrSepFormat(op-entry.amt-rub, mSumSep, mSumFormat).

   RUN Insert_TTName("Sum" + STRING(mItem,"9"),
                     IF INT64(op-entry.amt-rub) = op-entry.amt-rub THEN
                         (STRING(op-entry.amt-rub, "-zzzzzzzzzzzz9") + "=")
                     ELSE
                         mStrTmp).
   &IF DEFINED(LAW_318p) = 0 &THEN
   RUN Insert_TTName("Sum0" + STRING(mItem,"9"), mStrTmp).
   &ELSE
   RUN Insert_TTName("Sum0" + STRING(mItem,"9"), TRIM(mStrTmp)).
   &ENDIF
   mItem = mItem + 1.
END.

mAmt = ACCUM TOTAL op-entry.amt-rub.

FIND FIRST op-entry OF op NO-LOCK NO-ERROR.

mDate  = IF op.doc-date <> ? THEN op.doc-date 
                             ELSE op.op-date.
mDDate = IF op.doc-date <> ? THEN {strdate.i op.doc-date}
                             ELSE {strdate.i op.op-date}.
mDDate = REPLACE(mDDate, " г.", "").

&IF DEFINED(LAW_318p) <> 0 &THEN
   RUN Insert_TTName("d-date", mDDate + " года").
   IF NUM-ENTRIES(mDDate," ") >= 3 THEN DO:
      RUN Insert_TTName("d-date-day", ENTRY(1,mDDate," ")).
      RUN Insert_TTName("d-date-month", ENTRY(2,mDDate," ")).
      RUN Insert_TTName("d-date-year", SUBSTR(ENTRY(3,mDDate," "),3)).
   END.
&ELSE
   RUN Insert_TTName("d-date", mDDate).
&ENDIF

RUN Insert_TTName("Detail", op.detail).

IF op-entry.acct-cr <> ?  THEN
   RUN Insert_TTName("AcctCr", DelFilFromAcct(op-entry.acct-cr)).

{find-act.i &acct=op-entry.acct-cr &curr=op-entry.currency}

IF {assigned op.name-ben} THEN
   mPlName = op.name-ben.
ELSE
DO:
   IF AVAIL acct THEN DO:
      IF acct.cust-cat = 'В' THEN
      DO:
         RUN Insert_TTName("PlNameINN", 'ИНН ' + FGetSetting("ИНН", "", "") + ' ' 
                                        + FGetSetting("Банк","","")).
         RUN Insert_TTName("PlNameWOINN", FGetSetting("Банк","","")).
         mPlName = (IF FGetSetting("ПлатДок", "ВыводИНН", "Да") =  "Да" THEN
                   ('ИНН ' + FGetSetting("ИНН", "", "") + ' ') ELSE "" )
                 + FGetSetting("Банк","","").
      END.
      ELSE IF acct.cust-cat = 'Ю' THEN
      DO:
         IF AVAIL acct THEN DO:
            RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT mStrTMP1V[1]).
            RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mStrTMP1V[2]).
         END.  
         ELSE ASSIGN
            mStrTMP1V = ""
            mStrTMP2V = ""
         .
         RUN Insert_TTName("PlNameINN", mStrTMP1V[1] + " " + mStrTMP1V[2]).
         RUN Insert_TTName("PlNameWOINN", mStrTMP1V[2]).
         IF FGetSetting("ПлатДок", "ВыводИНН", "Да") =  "Да" THEN
            mPlName = mStrTMP1V[1] + " " + mStrTMP1V[2].
         ELSE
            mPlName = mStrTMP1V[2].
      END.
      ELSE
      DO:
         IF AVAIL acct THEN DO:
            {getcust.i &name=mStrTMP1V}
            {getcust.i &name=mStrTMP2V &OFFInn=YES}
         END.
         ELSE ASSIGN
            mStrTMP1V = ""
            mStrTMP2V = ""
         .

         RUN Insert_TTName("PlNameINN", mStrTMP1V[1] + " " + mStrTMP1V[2]).
         RUN Insert_TTName("PlNameWOINN", mStrTMP2V[1] + " " + mStrTMP2V[2]).
         IF FGetSetting("ПлатДок", "ВыводИНН", "Да") =  "Да" THEN
            mPlName = mStrTMP1V[1] + " " + mStrTMP1V[2].
         ELSE
            mPlName = mStrTMP2V[1] + " " + mStrTMP2V[2].
      END.
   END.
END.

&IF DEFINED(LAW_318p) <> 0 &THEN
   mPlNameP = "От кого " + mPlName.
   {wordwrap.i &s=mPlNameP &n=4 &l=37}
   IF mPlNameP[4] = "" THEN DO:
      /* 1. aaa    ___       1. aaa    ___       1. aaa    ___
         2. bbb => aaa  или  2. bbb => aaa  или  2. ___ => aaa
         3. ccc    bbb       3. ___    bbb       3. ___    ___
         4. ___    ccc       4. ___    ___       4. ___    ___
      */
      ASSIGN
         mPlNameP[4] = mPlNameP[3]
         mPlNameP[3] = mPlNameP[2]
         mPlNameP[2] = mPlNameP[1]
         mPlNameP[1] = ""
      .
      /*                     1. ___    ___       1. ___    ___
                             2. aaa => ___  или  2. aaa => ___
                             3. bbb    aaa       3. ___    aaa
                             4. ___    bbb       4. ___    ___
      */
      IF mPlNameP[4] = "" THEN ASSIGN
         mPlNameP[4] = mPlNameP[3]
         mPlNameP[3] = mPlNameP[2]
         mPlNameP[2] = ""
      .
   END.
   RUN Insert_TTName("PlName11", mPlNameP[1]).
   RUN Insert_TTName("PlName12", mPlNameP[2]).
   RUN Insert_TTName("PlName13", mPlNameP[3]).
   RUN Insert_TTName("PlName14", mPlNameP[4]).

   mPlNameP = "От кого " + mPlName.
   {wordwrap.i &s=mPlNameP &n=3 &l=62}
   IF mPlNameP[3] = "" THEN DO:
      /* 1. aaa    ___  или  1. aaa    ___
         2. bbb => aaa       2. ___ => aaa
         3. ___    bbb       3. ___    ___
      */
      ASSIGN
         mPlNameP[3] = mPlNameP[2]
         mPlNameP[2] = mPlNameP[1]
         mPlNameP[1] = ""
      .
      /* 1. ___    ___
         2. aaa => ___
         3. ___    aaa
      */
      IF mPlNameP[3] = "" THEN ASSIGN
         mPlNameP[3] = mPlNameP[2]
         mPlNameP[2] = ""
      .
   END.
   RUN Insert_TTName("PlName21", mPlNameP[1]).
   RUN Insert_TTName("PlName22", mPlNameP[2]).
   RUN Insert_TTName("PlName23", mPlNameP[3]).
&ENDIF
RUN Insert_TTName("PlName", mPlName).
mIsINN2 = NO.

IF AVAIL acct THEN DO:
   IF acct.cust-cat =  'В' THEN
   DO:
      &IF DEFINED(LAW_318p) <> 0 &THEN
         mPoINN = FGetSetting("ИНН", "", "").
         mPoKPP = FGetSetting("БанкКПП", "", "").
         FOR FIRST branch WHERE branch.branch-id = acct.branch-id NO-LOCK:
            mPoOKATO = GetXAttrValueEx("branch",acct.branch-id,"ОКАТО-НАЛОГ","").
         END.
         IF NOT {assigned mPoOKATO}
         THEN mPoOKATO = FGetSetting("БанкОКАТО", "", "").
         mPoName = FGetSetting("Банк","", "").
      &ELSE
         RUN Insert_TTName("PoNameINN", "ИНН " + FGetSetting("ИНН", "", "") + " "
                                        + FGetSetting("Банк","","")).
         RUN Insert_TTName("PoNameWOINN", FGetSetting("Банк","","")).
         RUN Insert_TTName("PoName", (IF FGetSetting("ПлатДок", "ВыводИНН", "Да") =  "Да" THEN
                                        ('ИНН ' + FGetSetting("ИНН", "", "") + ' ') ELSE "" )
                                     + FGetSetting("Банк","","")).
      &ENDIF
   END.
   ELSE
   DO:
      IF acct.cust-cat =  "Ю" THEN DO:
         &IF DEFINED(LAW_318p) <> 0 &THEN
            IF CAN-DO(FGetSetting("ПлатДок", "ПолучОбъяв", ""),STRING(acct.bal-acct)) THEN DO:
               {getcust.i &name=mStrTMP2V &OFFInn=YES &OFFsigns=YES &inn=mStrTMP}
            END.
            ELSE DO: 
               {getcust.i &name=mStrTMP2V &OFFInn=YES &inn=mStrTMP}
            END.
         &ELSE
            {getcust.i &name=mStrTMP2V &OFFInn=YES}
         &ENDIF
         RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT mStrTMP2V[1]).
         mIsINN2 = YES.
         RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mStrTMP2V[2]).
      END.
      ELSE DO:
         {getcust.i &name=mStrTMP1V}
         &IF DEFINED(LAW_318p) <> 0 &THEN
            IF CAN-DO(FGetSetting("ПлатДок", "ПолучОбъяв", ""),STRING(acct.bal-acct)) THEN DO:
               {getcust.i &name=mStrTMP2V &OFFInn=YES &OFFsigns=YES &inn=mStrTMP}
            END.
            ELSE DO: 
               {getcust.i &name=mStrTMP2V &OFFInn=YES &inn=mStrTMP}
      
               IF CAN-DO(FGetSetting("БалСчИНН","",""), STRING(acct.bal-acct)) THEN DO:
      
                  CASE acct.cust-cat:
                     WHEN "Ч" THEN DO:
                        FIND FIRST person WHERE person.person-id = acct.cust-id NO-LOCK NO-ERROR.
                        IF AVAIL person THEN 
                           mStrTMP = person.inn.
                     END.
                     WHEN "Б" THEN DO:
                        FIND FIRST banks WHERE banks.bank-id = acct.cust-id NO-LOCK NO-ERROR.
                           IF AVAIL banks THEN DO:
                           mStrTMP = GetBankInn ("bank-id", STRING (banks.bank-id)).
                        END.
                     END.
                  END CASE.
                  IF mStrTmp =  ? THEN mStrTMP = "".
               END.
            END.
         &ELSE
            {getcust.i &name=mStrTMP2V &OFFInn=YES}
         &ENDIF
      END.

      IF mStrTMP <> "" THEN
         ASSIGN
            mStrTMP1V[1] = "ИНН " + mStrTMP
            mStrTMP1V[2] = mStrTMP2V[1] + " " + mStrTMP2V[2]
         .

      &IF DEFINED(LAW_318p) <> 0 &THEN
   
         IF mIsINN2 THEN
            mStrTMP2V[1] = "".

         mPoName = TRIM(mStrTMP2V[1] + " " + mStrTMP2V[2]).
         mPoINN = mStrTMP.
         CASE acct.cust-cat:
            WHEN "Ю" THEN mStrTMP = "cust-corp".
            WHEN "Ч" THEN mStrTMP = "person".
            WHEN "Б" THEN mStrTMP = "banks".
         END CASE.
         mPoKPP = GetXAttrValueEx(mStrTMP, STRING(acct.cust-id), "КПП", "").
         mPoOKATO = GetXAttrValueEx(mStrTMP, STRING(acct.cust-id), "ОКАТО-НАЛОГ", "").
         FIND FIRST recacct WHERE recacct.cust-cat = acct.cust-cat
                              AND recacct.cust-id  = acct.cust-id
                              AND recacct.acct-cat = "b"
                              AND recacct.contract = "Расчет"
                              AND CAN-FIND(FIRST op-entry OF op WHERE op-entry.acct-db = recacct.acct
                                                                   OR op-entry.acct-cr = recacct.acct)
         NO-LOCK NO-ERROR.
         IF NOT AVAIL recacct
            THEN FIND FIRST recacct WHERE recacct.cust-cat   = acct.cust-cat
                                      AND recacct.cust-id    = acct.cust-id
                                      AND recacct.acct-cat   = "b"
                                      AND recacct.contract   = "Расчет"
                                      AND recacct.open-date <= op.op-date
                                      AND (   recacct.close-date = ?
                                           OR recacct.close-date > op.op-date)
            NO-LOCK NO-ERROR.
         IF  AVAIL recacct
         AND {assigned recacct.number}
         THEN mPoRAcct = recacct.number.
      &ELSE
         IF acct.cust-cat =  "Ю" THEN DO:
            RUN Insert_TTName("PoNameINN", mStrTMP1V[1] + " " + mStrTMP1V[2]).
            RUN Insert_TTName("PoNameWOINN", mStrTMP1V[2]).

            IF FGetSetting("ПлатДок", "ВыводИНН", "Да") =  "Да" THEN
               RUN Insert_TTName("PoName", mStrTMP1V[1] + " " + mStrTMP1V[2]).
            ELSE
               RUN Insert_TTName("PoName", mStrTMP1V[2]).
         END.
         ELSE DO:
            RUN Insert_TTName("PoNameINN", mStrTMP1V[1] + " " + mStrTMP1V[2]).
            RUN Insert_TTName("PoNameWOINN", mStrTMP2V[1] + " " + mStrTMP2V[2]).

            IF FGetSetting("ПлатДок", "ВыводИНН", "Да") =  "Да" THEN
               RUN Insert_TTName("PoName", mStrTMP1V[1] + " " + mStrTMP1V[2]).
            ELSE
               RUN Insert_TTName("PoName", mStrTMP2V[1] + " " + mStrTMP2V[2]).
         END. 
      &ENDIF
   END.
END.

IF NOT {assigned mBankName} THEN mBankName = dept.name-bank.
&IF DEFINED(LAW_318p) <> 0 &THEN
mBankName2 = mBankName.
FIND op-bank OF op NO-LOCK NO-ERROR.
IF AMBIGUOUS op-bank
THEN FIND op-bank OF op WHERE op-bank.op-bank-type = "" NO-LOCK NO-ERROR.
IF AVAIL op-bank THEN
   ASSIGN
      mPoBankName = op-bank.bank-name
      mPoBankCode = op-bank.bank-code
      .   
ELSE
   ASSIGN
      mPoBankName = mBankName
      mPoBankCode = mBankBIK
      .  
IF mPoBankCode = mBankBIK THEN
   mOurBank = YES.
&ENDIF
   
&IF DEFINED(LAW_318p) <> 0 &THEN
 IF FGetSetting("ПлатДок", "ВыводМест", "Нет") = "Да" THEN DO:
    mBankName2 = mBankName2 + ", " + FGetSetting("БанкГород",?,"").
 END.
&ENDIF

IF op-entry.acct-db <> ?  THEN DO:
 {find-act.i &acct=op-entry.acct-db &curr=op-entry.currency}
 IF AVAIL acct THEN DO:
    IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  "Да" THEN DO:
       RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.branch-id,
                                              OUTPUT mBankName2).
       IF mIsPrtCity THEN mBankName2 = mBankName2 + mBankCity.
    END.
    ELSE
       mBankName2 = GetValueAttr("branch", acct.branch-id, mFldNameBnk) 
                                + ", " + mBankName2.                              
 
 END.
END.
&IF DEFINED(LAW_318p) <> 0 &THEN
IF op-entry.acct-cr <> ? AND mOurBank THEN DO:
   {find-act.i &acct=op-entry.acct-cr &curr=op-entry.currency}    
   IF FGetSetting("НаимБанкСВКО","Наим3352У","Да") =  "Да" THEN DO:
/* Замена Плюс банк
      RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.branch-id,
*/    RUN GetBranchNamesCO IN THIS-PROCEDURE(INPUT  acct.filial-id,
/* Конец замены Плюс банк */
                                             OUTPUT mPoBankName).
      IF mIsPrtCity THEN mPoBankName = mPoBankName + mBankCity.
   END.
   ELSE
   DO:
/* Замена Плюс банк
      mPoBankName = GetValueAttr("branch", acct.branch-id, mFldNameBnk) 
*/    mPoBankName = GetValueAttr("branch", acct.filial-id, mFldNameBnk) 
/* Конец замены Плюс банк */
                      + ", " + mPoBankName.
      IF mIsPrtCity THEN mPoBankName = mPoBankName + mBankCity.                
   END.                       
END.
&ENDIF
IF NOT {assigned mBankName2} THEN mBankName2 = mBankName.

RUN Insert_TTName IN THIS-PROCEDURE ("NameBank", mBankName2).

&IF DEFINED(LAW_318p) <> 0 &THEN
   /* Реквизиты документа перекрывают всё */
   mStr = GetXAttrValueEx("op", STRING(op.op), "name-rec", "").
   IF {assigned mStr} THEN DO:
      mPoName  = mStr.
      mPoINN   = GetXAttrValueEx("op", STRING(op.op), "INN-rec", "").
      mPoKPP   = GetXAttrValueEx("op", STRING(op.op), "KPP-rec", "").
      mPoOKATO = GetXAttrValueEx("op", STRING(op.op), "OKATO-rec", "").
   END.

   IF   {assigned op.ben-acct}
   THEN mPoRAcct = op.ben-acct.

 
   /* Заполнение раквизитов в шаблон */
   RUN Insert_TTName("PoName", mPoName).
   IF {assigned mPoINN}
   THEN RUN Insert_TTName("PoINN", mPoINN).
   ELSE RUN Insert_TTName("PoINN", FILL(CHR(255),5) + FILL("─",9)).

   IF {assigned mPoKPP}
   THEN RUN Insert_TTName("PoKPP", mPoKPP).
   ELSE RUN Insert_TTName("PoKPP", FILL("─",5)).
   IF {assigned mPoOKATO}
   THEN RUN Insert_TTName("PoOKATO", mPoOKATO).
   ELSE RUN Insert_TTName("PoOKATO", FILL("─",5)).

   IF FGetSetting("ПлатДок", "ОВНсч", "") =  "ДА" 
      AND CAN-DO(FGetSetting("ПлатДок", "ОВНсчДт", ""),op-entry.acct-db) 
      AND CAN-DO(FGetSetting("ПлатДок", "ОВНсчКр", ""),op-entry.acct-cr) 
      THEN DO:
        mPoRAcct = op-entry.acct-cr.
      END.

   &IF DEFINED(xls) <> 0 &THEN
      mStrTmp = FILL("-",20).
   &ELSE
      mStrTmp = FILL(CHR(255),11) + FILL("─",20).
   &ENDIF 

   mPoRAcct = DelFilFromAcct(mPoRAcct).
   IF {assigned mPoRAcct}
   THEN RUN Insert_TTName("PoRAcct", mPoRAcct).
   ELSE RUN Insert_TTName("PoRAcct", mStrTmp).

   RUN Insert_TTName("Pobank-MFO", mPoBankCode).
   RUN Insert_TTName("PoNameBank", mPoBankName).

   RUN Insert_TTName("bank-MFO", GetXAttrValueEx("branch",dept.branch,"БанкМФО","")).
&ELSE
   RUN Insert_TTName("bank-MFO", FGetSetting("БанкМФО", ?, "")).
&ENDIF

mSumFormat = REPLACE(getFormatStr(11, ",", YES),"z", ">").
mStrTmp = TRIM(AmtStrSepFormat(mAmt, mSumSep, mSumFormat)).

RUN Insert_TTName("Amt0", mStrtMP).
RUN Insert_TTName("Amt1",
                  IF INT64(mAmt) = mAmt THEN
                      (TRIM(STRING(mAmt, "->>>>>>>>>>>>9")) + "=")
                  ELSE
                      mStrTmp).
mAmt1 = FILL(CHR(205), 24 - LENGTH(mAmt1)) + mAmt1.
RUN Insert_TTName("Amt1_1", mAmt1).
RUN Insert_TTName("Amt01", mStrTmp).
mAmt1 = FILL(CHR(205), 24 - LENGTH(mAmt1)) + mAmt1.
RUN Insert_TTName("Amt01_1", mAmt1).

&IF DEFINED(LAW_318p) <> 0 &THEN
   RUN x-amtstr.p (mAmt,
                   "",
                   NO,
                   NO,
                   OUTPUT mchAmt, OUTPUT mchDec).
   IF mTypeChar <> "" THEN DO:
      RUN Insert_TTName("AmtStrRub", mchAmt).
      RUN Insert_TTName("Rub", "руб.").
      RUN Insert_TTName("AmtStrKop", mchDec).
      RUN Insert_TTName("Kop", "коп.").
   END.
   ELSE DO:
      RUN Insert_TTName("AmtStrRub", mchAmt + "руб.").
      RUN Insert_TTName("AmtStrKop", mchDec + "коп.").
   END.
   mAmtStr = mchAmt + IF INT64(mAmt) <> mAmt THEN (" " + mchDec) ELSE "".
&ELSE
   RUN x-amtstr.p (mAmt,
                   "",
                   YES,
                   YES,
                   OUTPUT mchAmt, OUTPUT mchDec).
   mAmtStr = mchAmt + IF INT64(mAmt) <> mAmt THEN (" " + mchDec) ELSE "".
&ENDIF

&IF DEFINED(LAW_318p) <> 0 &THEN
   DEF BUFFER doc-type FOR doc-type.
   
   FIND FIRST doc-type WHERE
              doc-type.doc-type =  op.doc-type
   NO-LOCK NO-ERROR.
   IF AVAIL(doc-type) THEN
      mCodeDoc = doc-type.digital.
   ELSE
      mCodeDoc = "".
   &IF DEFINED(xls) <> 0 &THEN
      RUN Insert_TTName("CodeDoc", "'" + mCodeDoc).
   &ELSE
      RUN Insert_TTName("CodeDoc", mCodeDoc).
   &ENDIF
   
   IF (op.op-date  <> ? AND
       op.op-date  >= DATE("01/11/2014")) OR
       op.doc-date >= DATE("01/11/2014")
   THEN DO:
      mDprId = INT64(GetXattrValue("op",STRING(op.op),"dpr-id")) NO-ERROR.
      /* Выводим подписную часть */
      RUN GetRepFioByRef(ENTRY(1,THIS-PROCEDURE:FILE-NAME, "."),
                         GetUserBranchID(USERID('bisquit')),
                         INT64(op.op),
                         INT64(mDprId)) NO-ERROR.
      ASSIGN
         mchWorkerBuhP = mPostInRep[1]
         mchWorkerBuh  = mFIOInRep[1]
         mchWorkerKasP = mPostInRep[2]
         mchWorkerKas  = mFIOInRep[2]
      .
      IF NOT {assigned mchWorkerBuhP} OR NOT {assigned mchWorkerBuh} THEN
      DO:
      FIND FIRST _user WHERE _user._userid = op.user-id NO-LOCK NO-ERROR.
      IF AVAIL _user THEN
         ASSIGN
            mchWorkerBuhP = GetXAttrValueEx("_user", _user._userid, "Должность", "Бухгалтер")
            mchWorkerBuh  = _user._user-Name
         .
      END.
      IF mchWorkerBuhP =  "@" THEN mchWorkerBuhP = "".
      IF mchWorkerBuh  =  "@" THEN mchWorkerBuh  = "".
      RUN Insert_TTName("BhPost", mchWorkerBuhP).
      RUN Insert_TTName("BhName", mchWorkerBuh).
      IF NOT {assigned mchWorkerKasP} OR NOT {assigned mchWorkerKas} THEN
      DO:
      FIND FIRST _user WHERE _user._userid = op.user-inspector NO-LOCK NO-ERROR.
      IF AVAIL _user THEN
         ASSIGN
            mchWorkerKasP = GetXAttrValueEx("_user", _user._userid, "Должность", "Кассир")
            mchWorkerKas  = _user._user-Name
         .
      END.
      IF mchWorkerKasP =  "@" THEN mchWorkerKasP = "".
      IF mchWorkerKas  =  "@" THEN mchWorkerKas  = "".
      RUN Insert_TTName("KsPost", mchWorkerKasP).
      RUN Insert_TTName("KsName", mchWorkerKas).
   END.
&ENDIF

CREATE tt-op-entry.
BUFFER-COPY op-entry TO tt-op-entry NO-ERROR.
ASSIGN
   tt-op-entry.op-entry-half-db = op-entry.op-entry
.

RUN ProcProcessFreeField(mStrPar, tt-op-entry.op, (IF AVAIL op-entry THEN op-entry.op-entry ELSE ?)).
RUN Insert_TTName("EmptyField", mVarVal[28]).
RUN Insert_TTName("AmtStr", mAmtStr).
RUN Insert_TTName("AcctDb", DelFilFromAcct(op-entry.acct-db)).

&IF DEFINED(xls) <> 0 &THEN
   RUN Insert_TTName("DocNum", "'" + op.doc-num).
&ELSE
   RUN Insert_TTName("DocNum", op.doc-num).
&ENDIF

IF    FGetSetting("ПлатДок", "СвободПоле", "Да") =  "Да"
   OR FGetSetting("ПлатДок", "ПрихКО",     "")   <> ""
   OR FGetSetting("ПлатДок", "РасхКО",     "")   <> ""
   OR FGetSetting("ПлатДок", "ПрРасКО",    "")   <> ""
   THEN
   RUN Insert_TTName IN THIS-PROCEDURE ("FormWithEP", "1").
ELSE
   RUN Insert_TTName IN THIS-PROCEDURE ("FormWOEP", "1").

&IF DEFINED(xls) <> 0 &THEN
   RUN printvd.p("ОНВН318xls", INPUT TABLE ttnames).
&ELSEIF DEFINED(LAW_318p) <> 0 &THEN
   IF (op.op-date  <> ? AND
            op.op-date  <  DATE("01/11/2014")) OR
            op.doc-date <  DATE("01/11/2014")
   THEN
      RUN printvd.p({&vidDoc}, INPUT TABLE ttnames).
   ELSE
      RUN printvd.p({&vidDoc} + "н", INPUT TABLE ttnames).
&ELSE
RUN printvd.p("ОНВН", INPUT TABLE ttnames).
&ENDIF

{intrface.del}
/* $LINTFILE='ovn-nps.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.503+03:00' */
/*prosignliKVABw1PtHEUZHwSyM1RA*/