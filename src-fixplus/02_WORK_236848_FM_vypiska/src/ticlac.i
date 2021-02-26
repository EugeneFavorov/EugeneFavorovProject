/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: ticlac.i
      Comment: Процедура печати справки о счетах
               (Открытая система печати)
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 04/06/2007 kraw (0077048)
     Modified: 28/09/2007 kraw (0081884) Остаток на начало дня, а не на его окончание.
     Modified: 27/02/2008 kraw (0089131) Таблица 1 в выписке в валюте счета.
     Modified: 14/05/2008 kraw (0093190) Сведения о клиенте в цикле [acct]
     Modified: 19/05/2008 kraw (0091479) оптимизация кода
     Modified: 17/12/2010 kraa (0120008) Реализована печать Выписки в электронном виде по запросу ЦБ РФ
*/
{globals.i}                                 /* глобальные переменные         */
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}
{flt-val.i}
{details.i}
{intrface.get tmess}
&GLOB ContractKassa "Касса"

&IF DEFINED(move) NE 0 OR DEFINED(customreq) NE 0 &THEN
&GLOBAL-DEFINE FILE_sword_p YES
{pp-uni.var}
&UNDEFINE FILE_sword_p
{pp-uni.prg}
DEFINE VARIABLE mPName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPAcct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPRKC   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPCAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPMFO   AS CHARACTER NO-UNDO.
&ELSE
{wordwrap.def}
&ENDIF

&IF DEFINED(ticlax) EQ 0 &THEN
DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.
{parsin.def}
DEFINE VAR mSupressZeroStr AS CHARACTER NO-UNDO.
DEFINE VAR mSupressZero    AS LOGICAL   NO-UNDO.
mSupressZeroStr = GetParamByNameAsChar(iParams, "ПодавлятьНулевые", "Нет").
mSupressZero = (mSupressZeroStr = "Да" OR mSupressZeroStr = "Yes").
&ENDIF

DEFINE VARIABLE mName AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mINN  AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mBINN AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mKPP  AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mCNV  AS CHARACTER          NO-UNDO.

DEFINE VARIABLE mCodeNalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNameNalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE mZDate     AS DATE      NO-UNDO.
DEFINE VARIABLE mZNumb     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTaxAddr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrTMP    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsExsist  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mDecTMP    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mItem      AS INT64   NO-UNDO.
DEFINE VARIABLE mItem1     AS INT64   NO-UNDO.
DEFINE VARIABLE mIsCash    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mCustCat   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustId    AS INT64   NO-UNDO.
DEFINE VARIABLE mCatCorr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mToday     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mFIO       AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mStatus    AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mIn-proc   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE mAmtRubPost AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAmtCurPost AS DECIMAL NO-UNDO.

DEFINE VARIABLE vUserProcNum AS INT64 NO-UNDO.
DEFINE VARIABLE vI           AS INT64 NO-UNDO.
DEFINE VARIABLE vNSym        AS INT64 NO-UNDO.
DEFINE VARIABLE vNumStr      AS INT64 NO-UNDO.
DEFINE VARIABLE vSepNum      AS INT64 NO-UNDO.
DEFINE VARIABLE vSepChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mchPayer       AS CHARACT EXTENT 3 INIT "" NO-UNDO.
DEFINE VARIABLE mchBankName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPayerINN      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDetails LIKE op.details NO-UNDO.

DEFINE BUFFER xacct     FOR acct.
DEFINE BUFFER xop-entry FOR op-entry.

FUNCTION fvlineStr RETURN CHARACTER (INPUT iLine AS CHARACTER):
   DEFINE VARIABLE vItem AS INT64   NO-UNDO.
   DEFINE VARIABLE vRet  AS CHARACTER NO-UNDO.

   IF LENGTH(iLine) LE 1 THEN
      RETURN iLine.

   vRet = SUBSTRING(iLine, 1, 1).

   DO vItem = 2 TO LENGTH(iLine):
      vRet = vRet + "|" + SUBSTRING(iLine, vItem, 1).
   END.

   RETURN vRet.

END FUNCTION.

mBINN = FGetSetting("ИНН", "", FILL("-", 100)).

DO TRANSACTION:

&IF DEFINED(customreq) NE 0 &THEN

{getdate.i
   &DateLabel="Конец периода"
   &DateHelp ="Введите дату конца периода (F1 - календарь)"
   &dispBeforeDate="mCodeNalog FORMAT 'x(320)'     LABEL 'Получатель'       VIEW-AS FILL-IN SIZE 50 BY 1
                    mNameNalog FORMAT 'x(320)'     LABEL 'Адрес получателя' VIEW-AS FILL-IN SIZE 50 BY 1
                    mZDate     FORMAT '99/99/9999' LABEL 'Дата запроса'                                                         SKIP
                    mZNumb     FORMAT 'x(20)'      LABEL 'Номер запроса'                                                        SKIP
                    beg-date   FORMAT '99/99/9999' LABEL 'Начало периода'   HELP 'Введите дату начала периода (F1 - календарь)' SKIP 
                   "
   &UpdBeforeDate="mCodeNalog mNameNalog mZDate mZNumb beg-date"
   &return=" HIDE FRAME dateframe2 no-pause."
   &AddLookUp="
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'mZDate' THEN
               DO:
                  RUN calend.p.

                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     mZDate = DATE(pick-value).
                     DISPLAY mZDate.
                  END.

               END.
               ELSE
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'beg-date' THEN
               DO:
                  RUN calend.p.

                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     beg-date = DATE(pick-value).
                     DISPLAY beg-date.
                  END.

               END.
               ELSE
   "
}
&ELSE

&IF DEFINED(move) EQ 0 &THEN

{getdate.i
   &DateLabel="Отчетная дата"
   &DateHelp ="Введите отчетную дату (F1 - календарь)"
   &dispBeforeDate="mCodeNalog FORMAT 'x(4)'       LABEL 'Налоговый орган' SKIP
                    mNameNalog FORMAT 'x(50)'      LABEL ''                SKIP
                    mZDate     FORMAT '99/99/9999' LABEL 'Дата запроса'    SKIP
                    mZNumb     FORMAT 'x(20)'      LABEL 'Номер запроса'   SKIP
                   "
   &UpdBeforeDate="mCodeNalog mZDate mZNumb"
   &return=" HIDE FRAME dateframe2 no-pause."
   &AddLookUp="
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'mCodeNalog' THEN
               DO:
                  RUN pclass.p('НИ', 'НИ', 'Налоговая инспекция',6).

                  IF pick-value NE ? AND (LASTKEY = 10 OR LASTKEY = 13) THEN
                  DO:
                     mCodeNalog = pick-value.
                     mNameNalog = GetCodeName('НИ', mCodeNalog).

                     DISPLAY mCodeNalog mNameNalog.
                  END.

               END.
               ELSE
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'mZDate' THEN
               DO:
                  RUN calend.p.

                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     mZDate = DATE(pick-value).
                     DISPLAY mZDate.
                  END.

               END.
               ELSE
   "
}
&ELSE

&IF DEFINED(choky) EQ 0 &THEN
&IF DEFINED(ticlaxfsfm) NE 0 &THEN
{getdate.i

   &DateLabel="Конец периода"
   &DateHelp ="Введите дату конца периода (F1 - календарь)"
   &dispBeforeDate="
                    beg-date   FORMAT '99/99/9999' LABEL 'Начало периода'  HELP 'Введите дату начала периода (F1 - календарь)' SKIP 
                   "
   &UpdBeforeDate=" beg-date"
   &return=" HIDE FRAME dateframe2 no-pause."
   &AddLookUp="IF LASTKEY EQ KEYCODE('F1') 
                  AND FRAME-FIELD EQ 'beg-date' THEN
               DO:
                  RUN calend.p.
                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     beg-date = DATE(pick-value).
                     DISPLAY beg-date.
                  END.         
               END.
                ELSE
   "
}
&ELSE
{getdate.i
   &DateLabel="Конец периода"
   &DateHelp ="Введите дату конца периода (F1 - календарь)"
   &dispBeforeDate="mCodeNalog FORMAT 'x(4)'       LABEL 'Налоговый орган' SKIP
                    mNameNalog FORMAT 'x(50)'      LABEL ''                SKIP
                    mZDate     FORMAT '99/99/9999' LABEL 'Дата запроса'    HELP 'Введите дату запроса (F1 - календарь)' SKIP
                    mZNumb     FORMAT 'x(60)'      LABEL 'Номер запроса'   VIEW-AS FILL-IN SIZE 30 BY 1 SKIP
                    beg-date   FORMAT '99/99/9999' LABEL 'Начало периода'  HELP 'Введите дату начала периода (F1 - календарь)' SKIP 
                   "
   &UpdBeforeDate="mCodeNalog mZDate mZNumb beg-date"
   &return=" HIDE FRAME dateframe2 no-pause."
   &AddLookUp="
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'mCodeNalog' THEN
               DO:
                  RUN pclass.p('НИ', 'НИ', 'Налоговая инспекция',6).

                  IF pick-value NE ? AND (LASTKEY = 10 OR LASTKEY = 13) THEN
                  DO:
                     mCodeNalog = pick-value.
                     mNameNalog = GetCodeName('НИ', mCodeNalog).

                     DISPLAY mCodeNalog mNameNalog.
                  END.

               END.
               ELSE
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'mZDate' THEN
               DO:
                  RUN calend.p.

                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     mZDate = DATE(pick-value).
                     DISPLAY mZDate.
                  END.

               END.
               ELSE
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'beg-date' THEN
               DO:
                  RUN calend.p.

                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     beg-date = DATE(pick-value).
                     DISPLAY beg-date.
                  END.

               END.
               ELSE
   "
}
&ENDIF
&ELSE
{getdate.i
   &DateLabel="Конец периода"
   &DateHelp ="Введите дату конца периода (F1 - календарь)"
   &dispBeforeDate="mCodeNalog FORMAT 'x(8)'       LABEL 'Таможенный орган' SKIP
                    mNameNalog FORMAT 'x(50)'      LABEL ''                SKIP
                    mZDate     FORMAT '99/99/9999' LABEL 'Дата запроса'    HELP 'Введите дату запроса (F1 - календарь)' SKIP
                    mZNumb     FORMAT 'x(60)'      LABEL 'Номер запроса'   VIEW-AS FILL-IN SIZE 30 BY 1 SKIP
                    beg-date   FORMAT '99/99/9999' LABEL 'Начало периода'  HELP 'Введите дату начала периода (F1 - календарь)' SKIP 
                   "
   &UpdBeforeDate="mCodeNalog mZDate mZNumb beg-date"
   &return=" HIDE FRAME dateframe2 no-pause."
   &AddLookUp="
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'mCodeNalog' THEN
               DO:
                  RUN pclass.p('Таможни', 'Таможни', 'Таможенные органы',6).

                  IF pick-value NE ? AND (LASTKEY = 10 OR LASTKEY = 13) THEN
                  DO:
                     mCodeNalog = pick-value.
                     mNameNalog = GetCodeName('Таможни', mCodeNalog).

                     DISPLAY mCodeNalog mNameNalog.
                  END.

               END.
               ELSE
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'mZDate' THEN
               DO:
                  RUN calend.p.

                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     mZDate = DATE(pick-value).
                     DISPLAY mZDate.
                  END.

               END.
               ELSE
               IF LASTKEY EQ KEYCODE('F1') 
                  AND frame-field EQ 'beg-date' THEN
               DO:
                  RUN calend.p.

                  IF pick-value NE ? AND (LASTKEY EQ 13 OR LASTKEY EQ 10) THEN
                  DO:
                     beg-date = DATE(pick-value).
                     DISPLAY beg-date.
                  END.

               END.
               ELSE
   "
}
&ENDIF

&ENDIF

&ENDIF

IF KEYFUNC(LASTKEY) EQ "end-error" THEN
   RETURN.
END.
{prn-doc.def &with_proc=YES}
{tmprecid.def}

FIND FIRST tmprecid NO-ERROR.

FIND FIRST acct WHERE RECID(acct) EQ tmprecid.id NO-LOCK NO-ERROR.

FIND FIRST branch WHERE branch.branch-id EQ shFilial NO-LOCK NO-ERROR.

IF NOT AVAILABLE branch THEN
   RETURN.

&IF DEFINED(customreq) NE 0 &THEN
RUN Insert_TTName ("nal","1").
RUN Insert_TTName("ПустСтр1","                                               ").
&ELSE
&IF DEFINED(choky) EQ 0 &THEN
RUN Insert_TTName ("nal","1").
FIND FIRST code WHERE code.class EQ "НИ"
                  AND code.code  EQ mCodeNalog
   NO-LOCK NO-ERROR.
RUN Insert_TTName("НаимОрг","налогового органа").   
RUN Insert_TTName("ПустСтр1","                                               ").
&ELSE
RUN Insert_TTName ("choky","1").
FIND FIRST code WHERE code.class EQ "Таможни"
                  AND code.code  EQ mCodeNalog
   NO-LOCK NO-ERROR.
RUN Insert_TTName("НаимОрг","таможенного органа").      
RUN Insert_TTName("ПустСтр1","                                                ").
&ENDIF
&ENDIF

&IF DEFINED(customreq) NE 0 &THEN
   RUN Insert_TTName("TaxName"  , mCodeNalog).
   RUN Insert_TTName("TaxAddr"  , mNameNalog).
&ELSE
IF AVAILABLE code THEN
DO:
   RUN Insert_TTName("TaxName"  , code.name).
   mTaxAddr = (IF code.description[1] = ? THEN "" ELSE code.description[1] + ' ')
            + (IF code.description[2] = ? THEN "" ELSE code.description[2] + ' ')
            + (IF code.description[3] = ? THEN "" ELSE code.description[3] + ' ').
   RUN Insert_TTName("TaxAddr"  , mTaxAddr).
END.
&ENDIF

RUN Insert_TTName("Дата",  STRING(End-Date, "99/99/9999")).
RUN Insert_TTName("ТДата",  STRING(End-Date, "99.99.9999")).
RUN Insert_TTName("ДатаС",  term2str(End-Date, End-Date)).
RUN Insert_TTName("ДатаС_1",  ENTRY(1,term2str(End-Date, End-Date)," ")).
RUN Insert_TTName("ДатаС_2",  ENTRY(2,term2str(End-Date, End-Date)," ")).
RUN Insert_TTName("ДатаС_3",  ENTRY(3,term2str(End-Date, End-Date)," ") + " " + ENTRY(4,term2str(End-Date, End-Date)," ")).
RUN Insert_TTName("ДатаН",  STRING(Beg-Date, "99/99/9999")).
RUN Insert_TTName("ТДатаН",  STRING(Beg-Date, "99.99.9999")).
RUN Insert_TTName("ДатаЗ",  STRING(mZDate, "99/99/9999")).
RUN Insert_TTName("ТДатаЗ",  STRING(mZDate, "99.99.9999")).
RUN Insert_TTName("НомЗ",  mZNumb).

mToday = {strdate.i TODAY}.

RUN Insert_TTName("Сегодня",mToday).

RUN Insert_TTName("Банк",  branch.name ).
mStrTMP = GetXAttrValueEx("branch", STRING(branch.branch-id), "ИНН", "" ).
RUN Insert_TTName("Банк-ИНН", mStrTMP).   
mStrTMP = GetXAttrValueEx("branch", STRING(branch.branch-id), "КПП", "" ).
RUN Insert_TTName("Банк-КПП", mStrTMP).   
mStrTMP = GetXAttrValueEx("branch", STRING(branch.branch-id), "БанкМФО", "" ).
RUN Insert_TTName("БИК", mStrTMP).   

IF NOT AVAILABLE acct THEN
   ASSIGN
      mCustCat = GetFltVal("cust-cat")
      mCustId  = INT64(GetFltVal("cust-id"))
   .
ELSE
   ASSIGN
      mCustCat = acct.cust-cat
      mCustId  = acct.cust-id
   .
RUN GetCustName IN h_Base (mCustCat,
                           mCustId,
                           IF AVAILABLE acct THEN acct.acct ELSE "",
                           OUTPUT mName[1],
                           OUTPUT mName[2],
                           INPUT-OUTPUT mInn).
IF mCustCat EQ "Ю" THEN DO:
   FIND FIRST code WHERE 
              code.class EQ "КодПредп"
          AND code.val   EQ mName[1]   NO-LOCK NO-ERROR.
   IF AVAIL code THEN
      mName[1]  = code.name.
END.

RUN Insert_TTName("CustName",  TRIM(mName[1]) + " " + TRIM(mName[2])). 
RUN Insert_TTName("CustINN",   mINN).

IF mCustCat EQ "Ю" THEN
   mKPP = GetXAttrValueEx("cust-corp", STRING(mCustId), "КПП", "").
ELSE
   mKPP = "".
RUN Insert_TTName("CustKPP", mKPP  ).

IF FGetSetting("ГНИ", "ФИОНалог", "") NE "" THEN
   ASSIGN   
      mStatus[1] = FGetSetting("ГНИ", "ДолжНалог", "")
      mFIO[1]    = FGetSetting("ГНИ", "ФИОНалог" , "")
   .
ELSE
DO:
   FIND FIRST _user WHERE _user._userid EQ USERID ("bisquit") NO-LOCK NO-ERROR.
   ASSIGN
      mStatus[1] = GetXAttrValueEx("_user", STRING(USERID ("bisquit")), "Должность", "")
      mFIO[1]    = _user._user-name
   .
END.

IF {&prreg} NE "ВПОНС" THEN DO:

   {wordwrap.i &s=mStatus &n=2 &l=24}
   {wordwrap.i &s=mFIO    &n=2 &l=35}

   RUN Insert_TTName("Должн", mStatus[1]).
   RUN Insert_TTName("ФИО",   mFIO[1]).
   RUN Insert_TTName("Должн1",mStatus[2]).
   RUN Insert_TTName("ФИО1",  mFIO[2]).
   
END.
ELSE DO:
   ASSIGN 
      mStatus[1] = SUBSTRING(mStatus[1],1,35)
      mFIO[1]    = SUBSTRING(mFIO[1],1,35).
   RUN Insert_TTName("Должн", mStatus[1]).
   RUN Insert_TTName("ФИО",   mFIO[1]).
END.

mCNV =  FGetSetting("КодНацВал0406007", "", "").
mIsExsist = NO.   
vUserProcNum = INT64(GetSysConf("user-proc-id")).
FIND FIRST user-proc WHERE RECID(user-proc) = vUserProcNum NO-LOCK NO-ERROR.
IF AVAIL(user-proc) AND
   GetXAttrValue("user-proc", STRING(user-proc.public-number), "Подписи") <> "" THEN DO:   
   {signatur.i &in-proc = mIn-proc}
   RUN Insert_TTName ("flag1","1").
END.
ELSE DO:
   IF {&prreg} EQ "ВПОНС" THEN DO:
      mIn-proc = "|" + mStatus[1] + FILL(" ", 36 - LENGTH(mStatus[1])) + "   " + mFIO[1] + "~n" + "|" + mStatus[2] +  FILL(" ", 36 - LENGTH(mStatus[1])) + "   " + mFIO[2].
      RUN Insert_TTName ("flag2","1").
   END.
   ELSE DO:
      mIn-proc = (mStatus[1] + FILL(" ", 24 - LENGTH(mStatus[1])) )+ "                " + mFIO[1].
      RUN Insert_TTName ("flag1","1").
   END.

END.
&IF DEFINED(ticlax) EQ 0 &THEN
RUN BeginCircle_TTName("acct").
&ENDIF
FOR EACH tmprecid,
   EACH acct WHERE RECID(acct) EQ tmprecid.id 
               AND acct.open-date LE end-date NO-LOCK:
&IF DEFINED(ticlax) NE 0 &THEN
   RUN BeginCircle_TTName("acct").
&ENDIF
   ASSIGN
      mCustCat = acct.cust-cat
      mCustId  = acct.cust-id
   .
   RUN GetCustName IN h_Base (mCustCat,
                              mCustId,
                              IF AVAILABLE acct THEN acct.acct ELSE "",
                              OUTPUT mName[1],
                              OUTPUT mName[2],
                              INPUT-OUTPUT mInn).
   IF mCustCat EQ "Ю" THEN DO:
      FIND FIRST code WHERE 
                 code.class EQ "КодПредп"
             AND code.val   EQ mName[1]   NO-LOCK NO-ERROR.
      IF AVAIL code THEN
         mName[1]  = code.name.
   END.

   RUN Insert_TTName("CustName[acct]",  TRIM(mName[1]) + " " + TRIM(mName[2])). 
   RUN Insert_TTName("CustINN[acct]",   mINN).

   IF mCustCat EQ "Ю" THEN
      mKPP = GetXAttrValueEx("cust-corp", STRING(mCustId), "КПП", "").
   ELSE
      mKPP = "".
   RUN Insert_TTName("CustKPP[acct]", mKPP  ).
   mIsExsist = YES.
   RUN Insert_TTName("acct[acct]",   STRING(DelFilFromAcct(acct.acct), "x(25)")).
   RUN Insert_TTName("contract[acct]", acct.contract).

   IF acct.currency EQ "" THEN
      RUN Insert_TTName("curr[acct]", mCNV).
   ELSE
      RUN Insert_TTName("curr[acct]", acct.currency).

   RUN Insert_TTName("open-date[acct]", STRING(acct.open-date, "99/99/9999")).
   RUN Insert_TTName("close-date[acct]", STRING(acct.close-date, "99/99/9999")).

   &IF DEFINED(amt) NE 0 &THEN
   RUN acct-pos IN h_base (acct.acct, acct.currency, end-date - 1, end-date - 1, gop-status).

   IF acct.currency NE "" THEN
      mDecTMP = ABSOLUTE(sh-val).
   ELSE
      mDecTMP = ABSOLUTE(sh-bal).

   RUN Insert_TTName("amt[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>>>>>>>9.99"), ".", "-")).
   &ENDIF

   &IF DEFINED(move) NE 0 OR DEFINED(customreq) NE 0 &THEN
   mItem = 0.
   RUN BeginCircle_TTName("op").

   FOR EACH op-entry WHERE     (    op-entry.acct-db EQ acct.acct
                                AND op-entry.op-date GE beg-date
                                AND op-entry.op-date LE end-date

                               )
                               &IF DEFINED(ticlax) EQ 0 &THEN
                                AND (   mSupressZero = NO
                                     OR (    mSupressZero      = YES
                                         AND op-entry.amt-cur <> 0))
                               &ENDIF
                            OR (    op-entry.acct-cr EQ acct.acct
                                AND op-entry.op-date GE beg-date
                                AND op-entry.op-date LE end-date
                               &IF DEFINED(ticlax) EQ 0 &THEN
                                AND (   mSupressZero = NO
                                     OR (    mSupressZero      = YES
                                         AND op-entry.amt-cur <> 0))
                               &ENDIF     
                               )
      NO-LOCK,
       EACH op OF op-entry WHERE op.op-status GE gop-status NO-LOCK BY op.op-date:
      mItem = mItem + 1.

      RUN Insert_TTName("I[op]", STRING(mItem)).
      RUN Insert_TTName("op-date[op]", STRING(op-entry.op-date, "99.99.9999")).

      FIND FIRST doc-type OF op NO-LOCK NO-ERROR.

      IF AVAILABLE doc-type THEN
         RUN Insert_TTName("doc-type[op]", IF doc-type.digital NE "" THEN doc-type.digital
                                                                     ELSE FILL("-", 100)).
      ELSE
         RUN Insert_TTName("doc-type[op]", FILL("-", 100)).

      RUN Insert_TTName("doc-num[op]", TRIM(op.doc-num)).
      RUN Insert_TTName("doc-date[op]", STRING(op.doc-date, "99.99.9999")).

      ASSIGN
         mPName  = ""
         mPAcct  = ""
         mPRKC   = ""
         mPCAcct = ""
         mPMFO   = ""
         mDecTMP = (IF acct.currency EQ "" THEN op-entry.amt-rub 
                                           ELSE op-entry.amt-cur)
      .

      IF     acct.currency NE op-entry.currency
         AND acct.currency NE ""
         THEN
      DO:

         mDecTMP = CurToCur("Учетный",
                            op-entry.currency,
                            acct.currency,
                            op-entry.op-date,
                            IF op-entry.currency NE "" THEN op-entry.amt-cur
                                                       ELSE op-entry.amt-rub
                           ).
      END.

      {empty Info-Store}
      RUN Collection-Info.
      mCatCorr = "".

&IF DEFINED(ticlax) NE 0 &THEN
      RUN acct-pos IN h_base (acct.acct, acct.currency, op-entry.op-date, op-entry.op-date, gop-status).
      ASSIGN
         mAmtRubPost = sh-in-bal
         mAmtCurPost = sh-in-val
      .

      FOR EACH xop-entry WHERE (xop-entry.acct-db EQ acct.acct OR
                                xop-entry.acct-cr EQ acct.acct)
                           AND xop-entry.op-date   EQ op-entry.op-date
                           AND xop-entry.op-status GE gop-status
                           AND xop-entry.op        LE op-entry.op
         NO-LOCK:

         IF xop-entry.acct-db EQ acct.acct THEN
         DO:
            ASSIGN
               mAmtRubPost = mAmtRubPost + xop-entry.amt-rub
               mAmtCurPost = mAmtRubPost + xop-entry.amt-cur
            .
         END.
         ELSE IF xop-entry.acct-cr EQ acct.acct THEN
         DO:
            ASSIGN
               mAmtRubPost = mAmtRubPost - xop-entry.amt-rub
               mAmtCurPost = mAmtRubPost - xop-entry.amt-cur
            .
         END.
      END.

      IF acct.currency EQ "" THEN
         RUN Insert_TTName("AmtPost[op]", TRIM(STRING(ABS(mAmtRubPost), "->>>>>>>>>>>>>>>>9.99"))).
      ELSE
         RUN Insert_TTName("AmtPost[op]", TRIM(STRING(ABS(mAmtCurPost), "->>>>>>>>>>>>>>>>9.99"))).
      &IF DEFINED(ticlaxfsfm) NE 0 &THEN
      RUN Insert_TTName("TITLE", "Входящий остаток на " + STRING(beg-date) + ";"  + TRIM(STRING(ABS(mAmtCurPost), "->>>>>>>>>>>>>>>>9.99")) + ";" + ( IF acct.currency NE "" THEN TRIM(STRING(ABS(mAmtRubPost), "->>>>>>>>>>>>>>>>9.99")) ELSE "") ).
      &ENDIF
&ENDIF

      IF acct.acct EQ op-entry.acct-cr THEN
      /*  Нужен плательщик */
      DO:
         {find-act.i
         &bact = xacct
         &acct = op-entry.acct-db
         &curr = op-entry.currency
         }
         IF AVAILABLE xacct THEN 
            mCatCorr = xacct.cust-cat.
         mPayerINN = "".
         RUN for-pay("ДЕБЕТ,ПЛАТЕЛЬЩИК,БАНКПЛ,БАНКГО,БАНКФИЛ",
                     "ПП",
                     OUTPUT mPName,
                     OUTPUT mPAcct,
                     OUTPUT mPRKC,
                     OUTPUT mPCAcct,
                     OUTPUT mPMFO).

         IF AVAILABLE xacct AND CAN-DO({&ContractKassa},xacct.contract) THEN DO:
            mchPayer = "".
            mchPayer[1] = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
            IF mchPayer[1] = "" THEN
               ASSIGN
                  mchPayer[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben
                  mPayerINN   = op.inn.

            IF mchPayer[1] = "" THEN DO:
               IF acct.cust-cat EQ "Ю" THEN DO:
                  RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT mPayerINN).
                  RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mchPayer[1]).    
               END.
               ELSE
                 {getcust.i &name=mchPayer &OFFinn=yes &inn=mPayerINN}
                 
               mchPayer[1] = mchPayer[1] + " " + mchPayer[2].
               IF CAN-DO(FGetSetting("КассСчИНН","",?), SUBSTR(acct.acct,1,5))
                  OR GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "bank-inn", ?) = "Да" THEN DO:
                  mPayerINN   = IF FGetSetting("ИНН",?,?) <> "" THEN FGetSetting("ИНН",?,?) ELSE "000000000000".
                  mchPayer[1] = mchPayer[1] + mchBankName.
               END.
            END.
            mPName = mchPayer[1].
         END.
                     
         FIND FIRST Info-Store WHERE Info-Store.info-id EQ "Плательщик" NO-ERROR.

         IF NOT AVAILABLE Info-Store THEN
            FIND FIRST Info-Store WHERE Info-Store.info-id EQ "Дебет" NO-ERROR.
         RUN Insert_TTName("PKPP[op]", GetXAttrValueEx("op",STRING(op.op),"Kpp-send", FILL("-", 100))).
         RUN Insert_TTName("amtc[op]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>9.99"), ".", "-")).
         RUN Insert_TTName("amtd[op]", FILL("-", 100)).

         RUN Insert_TTName("amtdebr[op]", "").
         RUN Insert_TTName("amtcrer[op]", TRIM(STRING(op-entry.amt-rub, ">>>>>>>>>>>>>>>>9.99"))).
         RUN Insert_TTName("amtdebv[op]", "").
         RUN Insert_TTName("amtcrev[op]", TRIM(STRING(op-entry.amt-cur, ">>>>>>>>>>>>>>>>9.99"))).

         /* получатель */
         mStrTMP = GetXAttrValueEx("branch", STRING(branch.branch-id), "БанкМФО", "" ).
         RUN Insert_TTName("Po-BIK[op]", mStrTMP).   
         mStrTMP = GetXAttrValueEx("branch", STRING(branch.branch-id), "КорСч", "" ).
         RUN Insert_TTName("Po-corr[op]", DelFilFromAcct(mStrTMP)).
         RUN Insert_TTName("Po-Bank[op]",  branch.name ).
         RUN Insert_TTName("Po-acct[op]",   STRING(DelFilFromAcct(acct.acct), "x(25)")).
         RUN Insert_TTName("Po-INN[op]",   mINN).
         RUN Insert_TTName("Po-Name[op]",  TRIM(mName[1]) + " " + TRIM(mName[2])). 
         /* плательщик */
         RUN Insert_TTName("Pl-BIK[op]", mPMFO).
         RUN Insert_TTName("Pl-corr[op]", DelFilFromAcct(mPCAcct)).
         RUN Insert_TTName("Pl-Bank[op]", mPRKC).
         RUN Insert_TTName("Pl-acct[op]",DelFilFromAcct( mPAcct)).
         RUN Insert_TTName("Pl-Name[op]", mPName).
          
         IF AVAILABLE Info-Store AND mPayerINN EQ "" THEN DO:
            RUN Insert_TTName("Pl-INN[op]", IF Info-Store.inn NE ? THEN Info-Store.inn 
                                                                   ELSE (IF mCatCorr EQ "В" THEN mBINN
                                                                                             ELSE "")).
         END.
         ELSE IF mPayerINN EQ "" THEN
         DO:
            IF mCatCorr EQ "В" THEN
               RUN Insert_TTName("Pl-INN[op]", mBINN).
            ELSE
               RUN Insert_TTName("Pl-INN[op]", "").
         END.
         ELSE RUN Insert_TTName("Pl-INN[op]", mPayerINN).

      END.
      ELSE IF acct.acct EQ op-entry.acct-db THEN
      /*  Нужен получатель */
      DO:
         {find-act.i
         &bact = xacct
         &acct = op-entry.acct-cr
         &curr = op-entry.currency
         }
         IF AVAILABLE xacct THEN 
            mCatCorr = xacct.cust-cat.
         mPayerINN = "".
         RUN for-pay("КРЕДИТ,ПОЛУЧАТЕЛЬ,БАНКПОЛ,БАНКГО,БАНКФИЛ",
                     "ПП",
                     OUTPUT mPName,
                     OUTPUT mPAcct,
                     OUTPUT mPRKC,
                     OUTPUT mPCAcct,
                     OUTPUT mPMFO).

         IF AVAILABLE xacct AND CAN-DO({&ContractKassa},xacct.contract) THEN DO:
            mchPayer = "".
            mchPayer[1] = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
            IF mchPayer[1] = "" THEN
               ASSIGN
                  mchPayer[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben
                  mPayerINN   = op.inn.

            IF mchPayer[1] = "" THEN DO:
               IF acct.cust-cat EQ "Ю" THEN DO:
                  RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT mPayerINN).
                  RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT mchPayer[1]).    
               END.
               ELSE
                 {getcust.i &name=mchPayer &OFFinn=yes &inn=mPayerINN}
                 
               mchPayer[1] = mchPayer[1] + " " + mchPayer[2].
               IF CAN-DO(FGetSetting("КассСчИНН","",?), SUBSTR(acct.acct,1,5))
                  OR GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "bank-inn", ?) = "Да" THEN DO:
                  mPayerINN   = IF FGetSetting("ИНН",?,?) <> "" THEN FGetSetting("ИНН",?,?) ELSE "000000000000".
                  mchPayer[1] = mchPayer[1] + mchBankName.
               END.
            END.
            mPName = mchPayer[1].
            &IF DEFINED(finmoncb) NE 0 &THEN
               IF NOT {assigned mPAcct } THEN
               mPAcct = xacct.acct.
            &ENDIF
         END.
         
         FIND FIRST Info-Store WHERE Info-Store.info-id EQ "Получатель" NO-ERROR.

         IF NOT AVAILABLE Info-Store THEN
            FIND FIRST Info-Store WHERE Info-Store.info-id EQ "Кредит" NO-ERROR.
         RUN Insert_TTName("PKPP[op]", GetXAttrValueEx("op",STRING(op.op),"Kpp-rec", FILL("-", 100))).
         RUN Insert_TTName("amtd[op]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>9.99"), ".", "-")).
         RUN Insert_TTName("amtc[op]", FILL("-", 100)).

         RUN Insert_TTName("amtcrer[op]", "").
         RUN Insert_TTName("amtdebr[op]", TRIM(STRING(op-entry.amt-rub, ">>>>>>>>>>>>>>>>9.99"))).
         RUN Insert_TTName("amtcrev[op]", "").
         RUN Insert_TTName("amtdebv[op]", TRIM(STRING(op-entry.amt-cur, ">>>>>>>>>>>>>>>>9.99"))).

         /* получатель */
         RUN Insert_TTName("Po-BIK[op]", mPMFO).
         RUN Insert_TTName("Po-corr[op]", DelFilFromAcct(mPCAcct)).
         RUN Insert_TTName("Po-Bank[op]", mPRKC).
         RUN Insert_TTName("Po-acct[op]", DelFilFromAcct(mPAcct)).
         RUN Insert_TTName("Po-Name[op]", mPName).

         IF AVAILABLE Info-Store AND mPayerINN EQ "" THEN
            RUN Insert_TTName("Po-INN[op]", IF Info-Store.inn NE ? THEN Info-Store.inn 
                                                                   ELSE (IF mCatCorr EQ "В" THEN mBINN
                                                                                             ELSE "")).
         ELSE IF mPayerINN EQ "" THEN
         DO:
            IF mCatCorr EQ "В" THEN
               RUN Insert_TTName("Po-INN[op]", mBINN).
            ELSE
               RUN Insert_TTName("Po-INN[op]", "").
         END.
         ELSE RUN Insert_TTName("Po-INN[op]", mPayerINN).

         /* плательщик */
         mStrTMP = GetXAttrValueEx("branch", STRING(branch.branch-id), "БанкМФО", "" ).
         RUN Insert_TTName("Pl-BIK[op]", mStrTMP).   
         mStrTMP = GetXAttrValueEx("branch", STRING(branch.branch-id), "КорСч", "" ).
         RUN Insert_TTName("Pl-corr[op]", DelFilFromAcct(mStrTMP)).
         RUN Insert_TTName("Pl-Bank[op]",  branch.name ).
         RUN Insert_TTName("Pl-acct[op]",   STRING(DelFilFromAcct(acct.acct), "x(25)")).
         RUN Insert_TTName("Pl-INN[op]",   mINN).
         RUN Insert_TTName("Pl-Name[op]",  TRIM(mName[1]) + " " + TRIM(mName[2])). 
      END.
      ELSE
      DO:
         RUN Insert_TTName("PKPP[op]", FILL("-", 100)).
         RUN Insert_TTName("amtd[op]", FILL("-", 100)).
         RUN Insert_TTName("amtc[op]", FILL("-", 100)).
      END.
      IF mPayerINN NE "" THEN DO:
         RUN Insert_TTName("PINN[op]",mPayerINN).
      END.
      ELSE DO:
         IF AVAILABLE Info-Store THEN
            RUN Insert_TTName("PINN[op]", IF Info-Store.inn NE ? THEN Info-Store.inn 
                                                                  ELSE (IF mCatCorr EQ "В" THEN mBINN
                                                                                           ELSE FILL("-", 100))).
         ELSE
         DO:
            IF mCatCorr EQ "В" THEN
               RUN Insert_TTName("PINN[op]", mBINN).
            ELSE
               RUN Insert_TTName("PINN[op]", FILL("-", 100)).
         END.
      END.
      mIsCash = NO.

      IF AVAILABLE xacct THEN
      DO:
         mStrTMP = FGetSetting("НазнСчКас", "", "Касса").

         IF CAN-DO(mStrTMP, xacct.contract) THEN
            mIsCash = YES.
      END.


      IF NOT mIsCash THEN
      DO:
         RUN Insert_TTName("corr[op]", IF mPCAcct NE "" THEN DelFilFromAcct(mPCAcct)
                                                        ELSE FILL("-", 100)).
         RUN Insert_TTName("ИмяБ[op]", IF mPRKC NE "" THEN mPRKC
                                                      ELSE FILL("-", 10)).
         RUN Insert_TTName("МФОБ[op]", IF mPMFO NE "" THEN mPMFO
                                                      ELSE FILL("-", 100)).
         RUN Insert_TTName("PAcct[op]", IF mPAcct NE "" THEN DelFilFromAcct(mPAcct)
                                                        ELSE FILL("-", 100)).
      END.
      ELSE
      DO:
         RUN Insert_TTName("corr[op]", FILL("-", 100)).
         RUN Insert_TTName("ИмяБ[op]", FILL("-", 10)).
         RUN Insert_TTName("МФОБ[op]", FILL("-", 100)).
         RUN Insert_TTName("PAcct[op]", DelFilFromAcct(xacct.acct)).
      END.

      IF mPName BEGINS "ИНН " THEN
      DO:

         mItem1 = 4.

         DO WHILE LOOKUP(SUBSTRING(mPName, mItem1, 1), " ,0,1,2,3,4,5,6,7,8,9") > 0 :
            mItem1 = mItem1 + 1.
         END.
         mPName = SUBSTRING(mPName, mItem1).
      END.

      IF mPName BEGINS "КПП " THEN
      DO:

         mItem1 = 4.

         DO WHILE LOOKUP(SUBSTRING(mPName, mItem1, 1), " ,0,1,2,3,4,5,6,7,8,9") > 0 :
            mItem1 = mItem1 + 1.
         END.
         mPName = SUBSTRING(mPName, mItem1).
      END.

      RUN Insert_TTName("Имя[op]", IF mPName NE "" THEN mPName
                                                   ELSE FILL("-", 10)).
      RUN GetOpDetails(BUFFER op, OUTPUT mDetails) NO-ERROR.
      RUN Insert_TTName("details[op]", IF mDetails NE "" THEN REPLACE(mDetails,"~n"," ")
                                                         ELSE FILL("-", 10)).

      RUN NextCircle_TTName("op").
   END.
   RUN EndCircle_TTName("op").



   RUN acct-pos IN h_base (acct.acct, acct.currency, beg-date, end-date, gop-status).

   IF acct.currency NE "" THEN
   DO:
      mDecTMP = ABSOLUTE(sh-in-val).
      RUN Insert_TTName("balbeg[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      RUN Insert_TTName("balbegd[acct]", TRIM(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"))).
      mDecTMP = ABSOLUTE(sh-vdb).
      RUN Insert_TTName("amtdeb[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      mDecTMP = ABSOLUTE(sh-vcr).
      RUN Insert_TTName("amtcre[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      mDecTMP = ABSOLUTE(sh-val).
      RUN Insert_TTName("balend[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      RUN Insert_TTName("balendd[acct]", TRIM(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"))).
   END.
   ELSE
   DO:
      mDecTMP = ABSOLUTE(sh-in-bal).
      RUN Insert_TTName("balbeg[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      RUN Insert_TTName("balbegd[acct]", TRIM(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"))).
      mDecTMP = ABSOLUTE(sh-db).
      RUN Insert_TTName("amtdeb[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      mDecTMP = ABSOLUTE(sh-cr).
      RUN Insert_TTName("amtcre[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      mDecTMP = ABSOLUTE(sh-bal).
      RUN Insert_TTName("balend[acct]", REPLACE(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"), ".", "-")).
      RUN Insert_TTName("balendd[acct]", TRIM(STRING(mDecTMP, ">>>>>>>>>>>>>>>>9.99"))).
   END.
      
   &ENDIF  

IF {assigned mIn-proc} THEN DO:
    IF NUM-ENTRIES(mIn-proc,"│") > NUM-ENTRIES(mIn-proc,"|") THEN DO:
       vSepChar = "│".
       vSepNum = NUM-ENTRIES(mIn-proc,"│"). 
    END.
    ELSE DO:
       vSepNum = NUM-ENTRIES(mIn-proc,"|"). 
       vSepChar = "|".
    END.
    RUN BeginCircle_TTName("sig").

    DO vI = 1 TO vSepNum:
       IF TRIM( ENTRY(vI,mIn-proc,vSepChar)) NE "" THEN DO:
          IF INDEX(mIn-proc,"~n") NE 0 THEN DO:
            vNumStr = NUM-ENTRIES(TRIM(ENTRY(vI,mIn-proc,vSepChar),"~n"),"~n").
            DO vNSym = 1 TO vNumStr:
               RUN Insert_TTName("sign[sig]", TRIM( ENTRY(vNSym,ENTRY(vI,mIn-proc,vSepChar),"~n"))).
               RUN NextCircle_TTName("sig").            
            END.
          END.
          ELSE DO:
            RUN Insert_TTName("sign[sig]", TRIM( ENTRY(vI,mIn-proc,vSepChar))).
            RUN NextCircle_TTName("sig").            
          END.
       END.
    END.  
    RUN EndCircle_TTName("sig").   
END.        
  
   
&IF DEFINED(ticlax) NE 0 &THEN

DEFINE VARIABLE vInX AS INT64.
DEFINE VARIABLE vFilePath AS CHAR.
DEFINE VARIABLE vFileName AS CHAR.

   mFile = GetParamByNameAsChar(iParms, "Путь", "./") + DelFilFromAcct(acct.acct) + ".csv".
   vInX = R-INDEX(mFile, "/").
   
   IF vInX EQ 0 THEN DO:
       MESSAGE "Некорректное имя файла" VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN.
   END.
   
   vFilePath = SUBSTRING(mFile,1,vInX).
   vFileName = SUBSTRING(mFile,vInX + 1,LENGTH(mFile) - vInX).
   
   RUN Insert_TTName("sep", mSeparator).
   PackagePrint = YES.
   RUN SetSysConf IN h_base("DocCount","1"). 
    
   RUN NextCircle_TTName("acct").  
   RUN EndCircle_TTName("acct").
   RUN SetSysConf IN h_base("csv_tmp",vFilePath + vFileName).    
   RUN printvd.p({&prreg},
               INPUT TABLE ttnames
             ).     
   RUN Clear_TTName.          
   RUN SetSysConf IN h_base("csv_tmp","").
   PackagePrint = NO.           
&ELSE
RUN NextCircle_TTName("acct").
&ENDIF    
END.
&IF DEFINED(ticlax) EQ 0 &THEN
RUN EndCircle_TTName("acct").
&ENDIF

IF mIsExsist THEN
DO:
   RUN Insert_TTName("ex", "X").
   RUN Insert_TTName("ne", "").
END.
ELSE
DO:
   RUN Insert_TTName("ex", "").
   RUN Insert_TTName("NE", "X").
END.

&IF DEFINED(ticlax) EQ 0 &THEN
RUN printvd.p({&prreg},
               INPUT TABLE ttnames
             ).
&ENDIF
&IF DEFINED(ticlax) NE 0 &THEN
FIND tmprecid NO-ERROR.
IF AVAIL(tmprecid) THEN
  RUN Fill-SysMes("","","1",
                  "Экспорт в файл " + vFileName + " выписки по выбранному счету завершен.").   
IF NOT AVAIL(tmprecid) THEN
  RUN Fill-SysMes("","","1",
                  "Экспорт в файлы выписок по выбранным счетам завершен.").
&ENDIF        

/*prosignicn4X4tzoxGnkedgR+xSCA*/