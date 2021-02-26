/* Глухов */
{globals.i}

{intrface.get tmess}
{intrface.get xclass}
{intrface.get trans}
{intrface.get pbase}

{tmprecid.def}
DEF INPUT PARAM in-op-date AS DATE NO-UNDO.

FUNCTION Get47416 RETURNS CHAR(INPUT iFil AS CHAR, INPUT iCurr AS CHAR,INPUT iDate AS DATE) FORWARD.
FUNCTION Get30301 RETURNS CHAR(INPUT iFil AS CHAR, INPUT iCurr AS CHAR,INPUT iDate AS DATE,INPUT iBik AS CHAR) FORWARD.
FUNCTION GetOsnov RETURNS INT64 (INPUT iPunkt AS CHAR) FORWARD.


DEF BUFFER b-op FOR op.
DEF BUFFER b-op-entry FOR op-entry.


DEF VAR mAcctCr   AS CHAR NO-UNDO.
DEF VAR mAcctDb   AS CHAR NO-UNDO.
DEF VAR mDetails  AS CHAR NO-UNDO.
DEF VAR mOldDetails  AS CHAR NO-UNDO.
DEF VAR mOsnov    AS CHAR NO-UNDO.
DEF VAR mSoob     AS CHAR NO-UNDO.
DEF VAR mNom      AS INT64 NO-UNDO.
DEF VAR mNewOpRid AS RECID NO-UNDO.
DEF VAR mFilAcct  AS CHAR NO-UNDO.
DEF VAR mBik      AS CHAR NO-UNDO.
DEF VAR mCurr     AS CHAR NO-UNDO.
DEF VAR mDocType  AS CHAR NO-UNDO.
DEF VAR mSum      AS DEC  NO-UNDO.
DEF VAR mDocNum   AS CHAR NO-UNDO.
DEF VAR mNeedBank AS CHAR NO-UNDO.
DEF VAR mBankBik  AS CHAR NO-UNDO.
DEF VAR mBankName AS CHAR NO-UNDO.
DEF VAR mBankCorr AS CHAR NO-UNDO.
DEF VAR mStatus   AS CHAR NO-UNDO.
DEF VAR mBenACCt  AS CHAR NO-UNDO.
DEF VAR mBankCodeType  AS CHAR NO-UNDO.
DEF VAR mBenName  AS CHAR NO-UNDO.
DEF VAR mInn      AS CHAR NO-UNDO.
DEF VAR mKpp      AS CHAR NO-UNDO.
DEF VAR mKpp1     AS CHAR NO-UNDO.
DEF VAR mbankType AS CHAR NO-UNDO.
DEF VAR mDoc-Kind AS CHAR NO-UNDO.
DEF VAR mType     AS CHAR NO-UNDO.





mOsnov = "Сумма зачислена на счет до выяснения - Наименование получателя не соответствует номеру счета получателя.|
Сумма зачислена на счет до выяснения - счет получателя закрыт.|
Сумма зачислена на счет до выяснения - счет получателя отсутствует на балансе банка.|
Сумма зачислена на счет до выяснения - счет получателя открыт в другом филиале банка.|
Возврат суммы по п/п № &1 от &2 -  Наименование получателя не соответствует номеру счета получателя.|
Возврат суммы по п/п № &1 от &2 Счет  получателя закрыт.|
Возврат суммы по п/п № &1 от &2 Счет  получателя отсутствует на балансе банка.|
Зачислено со счета до выяснения на основании уточнения, полученного от банка-плательщика.|
Зачислено со счета до выяснения на основании СЗ ____ от &1.|
Зачислено со счета до выяснения.".

RUN Fill-SysMes IN h_tmess ("", "", "3",
   "Что будем делать?|" +
   "1. Зачисление на счет до выяснения," +
   "2. Возврат со счета до выяснения," +
   "3. Зачисление со счета до выяснения," +
   "4. Зачисление без использ. счета до выяснения," +
   "5. Отменить (ESC)"
   ).


DEFINE VARIABLE in-acct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE in-currency AS CHARACTER NO-UNDO.
DEFINE VARIABLE level       AS INT64     NO-UNDO.

in-currency = "".
level       = 5.

find first acct where acct.filial-id eq shFilial and acct.currency eq '' and acct.bal-acct eq 47416 
and acct.close-date EQ ? no-lock no-error.
IF AVAIL acct THEN in-acct = acct.acct.
ELSE
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "","Счет 47416 не найден!").
   RETURN.
END.

IF shFilial EQ "0500" THEN in-acct = "47416810605950010001     @0500".

CASE pick-value:
   WHEN "1" THEN
   DO:
      mNom = GetOsnov("1. Неверное наименование,2. Счет закрыт,3. Счет не найден,4. Неверный БИК").
      IF mNom EQ 0 THEN RETURN.
      mSoob = TRIM(REPLACE(ENTRY(mNom,mOsnov,"|"),CHR(13),"")).
      FOR EACH tmprecid NO-LOCK,
         EACH op WHERE RECID(op) EQ tmprecid.id NO-LOCK,
         EACH op-entry OF op NO-LOCK:
         IF op-entry.op-status GE "ФБП" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 слишком высокий статус. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.


/*         IF op-entry.acct-db BEGINS "47416" OR op-entry.acct-cr BEGINS "47416" AND GetXattrValueEx("op",STRING(op.op),"alt-details",?)  NE ? THEN  */
         IF op-entry.acct-db BEGINS "47416" AND GetXattrValueEx("op",STRING(op.op),"alt-details",?)  NE ? THEN 
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 уже есть счет 47416. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.

         FIND FIRST b-op WHERE RECID(b-op) EQ RECID(op) EXCLUSIVE-LOCK NO-ERROR.
         FIND FIRST b-op-entry WHERE RECID(b-op-entry) EQ RECID(op-entry) EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL b-op       AND
            AVAIL b-op-entry AND
            updatesigns(op.class-code,STRING(op.op),"alt-details",op.details,?) 
         THEN
         DO:
            IF shFilial EQ "0500" 
            THEN b-op-entry.acct-cr = "47416810605950010001     @0500".
            ELSE b-op-entry.acct-cr = Get47416(b-op.filial-id,b-op-entry.currency,b-op-entry.op-date).
            
            b-op.details = mSoob + CHR(10) + b-op.details.
            
            IF NOT op-entry.acct-cr BEGINS "47416" THEN 
	            updatesigns(op.class-code,STRING(op.op),"acct-rec",delfilfromacct(op-entry.acct-cr),?).
         END.
      END.
   END.
   WHEN "2" THEN
   DO:
      mNom = GetOsnov("1. Неверное наименование,2. Счет закрыт,3. Счет не найден").
      IF mNom EQ 0 THEN RETURN.
      mSoob = TRIM(REPLACE(ENTRY(mNom + 4,mOsnov,"|"),CHR(13),"")).
      DO TRANS:
      RUN crd3.p(in-acct,in-currency,level).
      END.
      find first kau where recid(kau) eq int64(pick-value) no-lock No-error.
         IF AVAIL kau THEN
         FOR EACH op WHERE op.op EQ int64(entry(1,kau.kau)) NO-LOCK,
         EACH op-entry OF op NO-LOCK:
         RUN clear_var.
         RUN SetSysConf IN h_base("CurrentKau",STRING(RECID(kau))).
/*
         IF op-entry.op-status GE "ФБП" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 слишком высокий статус. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.
*/
         IF NOT op-entry.acct-cr BEGINS "47416" THEN 
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 счет по кредиту не 47416. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.


/*         RUN op-copy-k.p (INPUT RECID(op),OUTPUT mNewOpRid,INPUT in-op-date,"ФБК","opb"). */

          FOR EACH op-bank OF op NO-LOCK:
             ASSIGN
             mBik = op-bank.bank-code
             mBankCorr = op-bank.corr-acct
             mBankCodeType = op-bank.bank-code-type
             mBankName = op-bank.bank-name.
          END.
            ASSIGN
               mStatus   = "ФБК"
               mSum      = op-entry.amt-rub
               mCurr     = op-entry.currency
               mAcctCr   = op-entry.acct-db
               mAcctDb   = op-entry.acct-cr
               mDetails  = TRIM(SUBSTITUTE(mSoob,op.doc-num,op.doc-date)) + "   БЕЗ НДС"
               mdoctype  = "01"
               mNeedBank = "1"
               mBenACCT  = op.ben-acct
               mBankBik  = mBik
               mDocNum   = op.doc-num
               mInn      = op.inn
               mBenName  = op.name-ben
               mKpp      = GetXattrValueEx("op",STRING(op.op),"kpp-send",?)
               mType     = "НЕ"
            .

            RUN opcreate.
      END.
   END.
   WHEN "3" THEN
   DO:
      mNom = GetOsnov("1. Уточнение,2. Служебная записка,3. Мотивированное суждение").
      IF mNom EQ 0 THEN RETURN.
      mSoob = TRIM(REPLACE(ENTRY(mNom + 7,mOsnov,"|"),CHR(13),"")).
      IF mNom EQ 2 THEN mSoob = SUBSTITUTE(mSoob,TODAY).
      DO TRANS:
      RUN crd3.p(in-acct,in-currency,level).
      END.
      find first kau where recid(kau) eq int64(pick-value) no-lock No-error.
         IF AVAIL kau THEN
         FOR EACH op WHERE op.op EQ int64(entry(1,kau.kau)) NO-LOCK,
         EACH op-entry OF op NO-LOCK:
         RUN clear_var.
         RUN SetSysConf IN h_base("CurrentKau",STRING(RECID(kau))).
/*      
         IF op-entry.op-status GE "ФБП" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 слишком высокий статус. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.
*/
         IF NOT op-entry.acct-cr BEGINS "47416" THEN 
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 счет по кредиту не 47416. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.


         mAcctCr = addfiltoacct(GetXattrValueEx("op",STRING(op.op),"acct-rec",?),op.filial-id).

         mFilAcct = "".

         IF CAN-FIND(FIRST acct WHERE acct.acct EQ mAcctCr) THEN
         DO:
         END.
         ELSE
         DO:
            FIND FIRST acct WHERE acct.number EQ TRIM(delfilfromacct(mAcctCr)) NO-LOCK NO-ERROR.
            mAcctCr = "".
            IF AVAIL acct THEN 
            ASSIGN
               mFilAcct = acct.filial-id
               mAcctCr = acct.acct.
         END.

         IF mAcctCr EQ "" THEN 
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",
                SUBSTITUTE("В документе &1 &2 счет назначения не найден в нашем банке. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.

         IF mFilAcct NE "" THEN 
         DO:
            mBik = FGetSettingMF("БанкМФО","","",mFilAcct,NO).
         END.

         IF mFilAcct EQ "" THEN
         DO:
          FOR EACH op-bank OF op NO-LOCK:
             ASSIGN
             mBik = op-bank.bank-code
             mBankCorr = op-bank.corr-acct
             mBankCodeType = op-bank.bank-code-type
             mBankName = op-bank.bank-name
/*             mbankType = "send" */
             mDoc-Kind = "send".
          END.
            mOldDetails = GetXattrValueEx("op",STRING(op.op),"alt-details","").
            IF mOldDetails = "" THEN mOldDetails = op.details.
            ASSIGN
               mStatus   = "Ф"
               mSum      = op-entry.amt-rub
               mCurr     = op-entry.currency
               mAcctDb   = op-entry.acct-cr
               mDocNum   = op.doc-num
               mDetails  = mSoob + " " + mOldDetails
               mdoctype  = "09"
               mNeedBank = "1"
               mBenACCT  = op.ben-acct
               mInn      = op.inn
               mBenName  = op.name-ben
               mBankBik  = mBik
               mKpp1     = GetXattrValueEx("op",STRING(op.op),"kpp-send",?)
               mType     = "ВН"
            .
            RUN opcreate.
         END.
         ELSE
         DO:
            mOldDetails = GetXattrValueEx("op",STRING(op.op),"alt-details","").
            IF mOldDetails = "" THEN mOldDetails = op.details.
            ASSIGN
               mStatus   = "Ф"
               mSum      = op-entry.amt-rub
               mCurr     = op-entry.currency
               mAcctDb   = op-entry.acct-cr
               mDetails  = mSoob  + " " + mOldDetails
               mdoctype  = "017"
               mNeedBank = "1"
               mBenACCT  = mAcctCr
               mAcctCr   = Get30301(mFilAcct,'',in-op-date,mBik)
               mBankBik  = mBik
               mDocNum   = op.doc-num
               mInn      = op.inn
               mBenName  = op.name-ben
               mKpp1     = GetXattrValueEx("op",STRING(op.op),"kpp-send",?)
               mType     = "ВН"
            .

            RUN opcreate.
         END.
      END.
   END.
   WHEN "4" THEN
   DO:
      FOR EACH tmprecid NO-LOCK,
         EACH op WHERE RECID(op) EQ tmprecid.id NO-LOCK,
         EACH op-entry OF op NO-LOCK:
         IF op-entry.op-status GE "ФБП" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 слишком высокий статус. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.
         IF NOT op-entry.acct-cr BEGINS "47416" THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 счет по кредиту не 47416. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.
         mAcctCr  = GetXattrValueEx("op",STRING(op.op),"acct-rec",?).
         mDetails = GetXattrValueEx("op",STRING(op.op),"alt-details",?).
         IF mAcctCr EQ ? OR mDetails EQ ? THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "",SUBSTITUTE("В документе &1 &2 нет первоначальных реквизитов. Пропускаем!",op.doc-type,op.doc-num)).
            NEXT.
         END.
         FIND FIRST b-op WHERE RECID(b-op) EQ RECID(op) EXCLUSIVE-LOCK NO-ERROR.
         FIND FIRST b-op-entry WHERE RECID(b-op-entry) EQ RECID(op-entry) EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL b-op       AND
            AVAIL b-op-entry 
         THEN
         DO:
            ASSIGN
               b-op-entry.acct-cr = mAcctCr
               b-op.details = mDetails.
         END.
      END.
   END.
   OTHERWISE
   DO:
      RETURN.
   END.
END CASE.



FUNCTION Get30301 RETURNS CHAR(INPUT iFil AS CHAR, INPUT iCurr AS CHAR,INPUT iDate AS DATE,INPUT iBik AS CHAR):
   DEF VAR vAcct AS CHAR NO-UNDO.
   IF shFilial NE "0000" THEN
   FOR EACH c-nostro WHERE 
                           CAN-DO("30301*@" + shFilial,c-nostro.acct)
                       AND c-nostro.currency EQ iCurr
   NO-LOCK:
      vAcct = c-nostro.acct.
   END.
   ELSE
   FOR EACH c-nostro WHERE c-nostro.nmfor EQ iBik 
                       AND c-nostro.acct BEGINS "30301" 
                       AND c-nostro.currency EQ iCurr
   NO-LOCK:
      vAcct = c-nostro.acct.
   END.
   RETURN vAcct.
END FUNCTION.



FUNCTION GetOsnov RETURNS INT64 (INPUT iPunkt AS CHAR):
   RUN Fill-SysMes IN h_tmess ("", "", "3",
     "На каком основании?|" + iPunkt).
   RETURN INT64(pick-value).
END FUNCTION.


FUNCTION Get47416 RETURNS CHAR(INPUT iFil AS CHAR, INPUT iCurr AS CHAR,INPUT iDate AS DATE):

   DEF VAR vAcct  AS CHAR NO-UNDO.
   FOR EACH acct WHERE
      acct.acct-cat EQ 'b'
      AND acct.bal-acct EQ 47416
      AND acct.filial-id EQ iFil
      AND acct.currency EQ iCurr
      AND (acct.close-date EQ ? OR acct.close-date GE iDAte)
      NO-LOCK:
      vAcct = acct.acct.
   END.
   
   RETURN vAcct.
END FUNCTION.




PROCEDURE opcreate.

RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-filial",shFilial).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"acctdb",macctdb).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"acctcr",macctcr).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"__USERID",USERID('bisquit')).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-sSum",mSum).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-curr",mCurr).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-doc-type",mDocType).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-op-date",in-op-date).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-op-status",mStatus).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-op-details",mDetails).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-acct-cat","b").
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-doc-num",mDocNum).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"needbank",mNeedBank).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-code",mBankBik).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-name",mBankName).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-corr-acct",mBankCorr).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-ben-acct",mBenAcct).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-inn",mInn).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-kpp",mKpp).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-kpp1",mKpp1).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-name-ben",mBenName).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-bank-type",mbanktype).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-doc-kind",mDoc-Kind).
RUN AddAttr2TableEx IN h_trans ("",0,-1,"",0,"in-type",mType).


IF GetBaseOpDate() EQ ? THEN RUN InitBaseLibrary IN h_pbase (?,in-op-date,?).

 RUN SetSysconf IN h_Base ("АВТОМАТИЧЕСКАЯ ПОСТАНОВКА","ДА"). 

   RUN RunTransaction IN h_pbase ("op14") NO-ERROR.

END PROCEDURE.

PROCEDURE clear_var.
ASSIGN
mAcctCr    = ""
mAcctDb    = ""
mDetails   = ""
mFilAcct   = ""
mBik       = ""
mCurr      = ""
mDocType   = ""
mDocNum    = ""
mNeedBank  = ""
mBankBik   = ""
mBankName  = ""
mBankCorr  = ""
mStatus    = ""
mBenACCt   = ""
mBankCodeType   = ""
mBenName   = ""
mInn       = ""
mKpp       = ""
mKpp1      = ""
mbanktype  = ""
mDoc-Kind = ""
.
END PROCEDURE.