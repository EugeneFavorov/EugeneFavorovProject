
/* +++ crddec.i was humbly modified by (c)blodd converter v.1.09 on 6/30/2016 1:15pm +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2001 ТОО "Банковские информационные системы"
     Filename: CRDDEC.I
      Comment: Создание и редактирование документов при списании с Картотеки 2
   Parameters:
         Uses:
      Used by:
      Created: 17/03/2003 NIK
     Modified: Обработка двух вариантов редактирования
     Modified: 25/03/2004 kraw (0024370) Отключение проверок в документе, списываемом с картотеки
     Modified: 30/05/2005 zial (0029904) Позиционирование курсора при вводе кассового символа
     Modified: 25/03/2004 kraw (0046269) тип коды банка не обязательно МФО-9
     Modified: 21/02/2007 kraw (0074649) Учитываем вид списания.
     Modified: 12.12.2007 kraw (0085886) Диагностика по блокированному клиенту
     Modified: 28.03.2008 muta  0088445  Определение суммы для списания с учетом блокировки суммы и лимита остатка
     Modified: 29/05/2008 kraw (0093826) "с учетом блокировки суммы" только если есть балансовые документы
     Modified: 06/08/2008 kraw (0096639) ищем балансовый если есть аналитика
     
*/
{defwrkop.i}
{g-defs.i}
{g-error.def}
{wordwrap.def}
{intrface.get xclass}
{intrface.get op}
{intrface.get kau}  
{intrface.get cust}
{copyxtr.i}
{def-wf.i}
{intrface.get crd}
{debug.equ}

def output param ok           as    INT64           no-undo.
def input  param in-op-date   like  op.op-date        no-undo.
def input  param rid-work     as    recid             no-undo.
def input  param rid-templ    as    recid             no-undo.
def input  param op-spis      as    logical           no-undo.
def input  param summ-val     like  op-entry.amt-rub  no-undo.
def input  param summ-rub     like  op-entry.amt-rub  no-undo.
def output param out-rid      as    recid             no-undo.

{protdec.def}
{details.def}
{tmprecid.def}

def var vmfo         like  op-bank.bank-code    no-undo.
def var vcorr-acct   like  op-bank.corr-acct    no-undo.
def var nameben      like op.name-ben           no-undo.
def var prev-field   as    char                 no-undo.
def var db-cr        as    logical initial ?    no-undo.
def var kau-flag     as    logical initial no   no-undo.
def var cp-num       as    INT64              no-undo.
def var vDebugXAttr  as    logical              no-undo.
def var dt-zo        as    date                 no-undo.
def var tmp-sign     as    char                 no-undo.
def var hproc        as    handle               no-undo.
def var fler         as    logical              no-undo.

DEFINE VARIABLE vVBOp         AS INT64       NO-UNDO.
DEFINE VARIABLE t-details     AS CHARACTER   NO-UNDO.

DEFINE BUFFER skau      FOR kau.
DEFINE BUFFER sop       FOR op.
DEFINE BUFFER sop-entry FOR op-entry.
DEFINE BUFFER ekau      FOR kau.
DEFINE BUFFER eop       FOR op.
DEFINE BUFFER eop-entry FOR op-entry.

DEFINE VARIABLE mBankCodeType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mMBRMask      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mKpp-send     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mKpp-rec      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mRetPay       AS LOGICAL     NO-UNDO.

DEFINE VARIABLE mDocType AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVidSpis AS INT64   NO-UNDO.
DEFINE VARIABLE mBtnQues AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mSumm    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mRecid   AS RECID     NO-UNDO.

DEFINE VARIABLE mStrTMP    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTypTMP    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNoOpBank  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpTranTMP AS INT64 NO-UNDO.
DEFINE VARIABLE mClassDoc  AS CHARACTER NO-UNDO.

DEFINE VARIABLE Date2Kart  AS DATE      NO-UNDO.

DEFINE VARIABLE vOpRecId   AS RECID     NO-UNDO.

DEFINE TEMP-TABLE ttKauRidOp NO-UNDO
   FIELD rid-kau   AS RECID
   FIELD order-pay LIKE op.order-pay
   FIELD doc-date  LIKE op.doc-date
   FIELD sort      LIKE kau.sort
   INDEX rid-kau IS UNIQUE rid-kau
   INDEX ord order-pay doc-date
.

DEFINE BUFFER tmpOp FOR op.
DEFINE BUFFER bAcct FOR acct.
DEFINE BUFFER bDoc-type FOR doc-type.

def            shared var hist-rec-kau  as recid initial ?  no-undo.
def new global shared var is-pack       as logical          no-undo.
def new global shared var f-EditDoc     as char             no-undo.

DEF BUFFER xop-bank  FOR op-bank.
DEF BUFFER xop-entry FOR op-entry.
DEF BUFFER kau-op    FOR op.
DEF BUFFER b-op      FOR op.

form
   "ДОКУМЕНТ"              op.doc-type  ":"
                           doc-type.name
         "N"               op.doc-num
         "ОТ"              op.doc-date                                      skip
   "Очер. платежа" at 34   op.order-pay format "x(2)"
                                        help   "Очередность платежа"
   "Срок платежа"  at 51   op.due-date  help   "Срок платежа"               skip
   "═════════════════════════════════════╦════════════════════════════════════════" SKIP
   "ДЕБЕТ:"                op-entry.acct-db
   "║КРЕДИТ:"      at 38   op-entry.acct-cr                                 skip
   " "                     name-db[1]
   "║ "            at 38   name-cr[1]                                       skip
   " "                     name-db[2]
   "║ "            at 38   name-cr[2]                                       skip
   "ОСТАТОК:"              bal-db      like acct-pos.balance
   "║ОСТАТОК:"     at 38   bal-cr      like acct-pos.balance                skip
   "═════════════════════════════════════╩════════════════════════════════════════" skip
   "КУРС НА           " "ВАЛ" "   {&in-ua-amtfc}      {&in-uf-amtncn}" "КС" "ЗО"    skip
                           op-entry.value-date help "Дата курса, на которую рассчитан эквивалент в нац. валюте"
                           op-entry.currency   at 25
                           op-entry.amt-cur
                           op-entry.amt-rub
                           op-entry.symbol
                           op-entry.prev-year                               skip
&IF DEFINED(LIKE-BIS) &THEN
   "Клиент:"               op.name-ben view-as fill-in size 51 by 1
   "ИНН:"                  op.inn                                           skip
&ENDIF
   "═[ БАНК-КОРРЕСПОНДЕНТ ]═══════════════════════════════════════════════════════" skip
   "Код:"                  vmfo        help "Идентификационный код банка"
                           bank1.name  format "x(47)" at 32                 skip
   "К/с:"                  vcorr-acct
                           bank2.name  format "x(47)" at 32                 skip
   "Р/с:"                  op.ben-acct                                      skip
&IF DEFINED(LIKE-DIASOFT) &THEN
   "Клиент:"               op.name-ben view-as fill-in size 51 by 1
   "ИНН:"                  op.inn                                           skip
&ENDIF
   "═[ СОДЕРЖАНИЕ ОПЕРАЦИИ ]═══════════════════════════════[ F3-пред., F4-след. ]═" skip
                            op.details view-as editor
                                       inner-chars 78 inner-lines 3 AT 1
with frame opreq 1 down overlay centered no-label row 2 width 80
     title color bright-white "[ ОПЕРАЦИЯ : " + op-kind.name + "; ЗА " +
                              string(in-op-date, "99/99/9999") + " ]".


{crd-trig.i {&*}}                                /* триггеры                  */

/*============================================================== ВЫПОЛНЕНИЕ ==*/
run "g-func.p" persistent set hproc.

assign
   dt-zo = date(FGetSetting("ДатаЗо",?,string(date(3,31,year(in-op-date)))))
   ok    = 1
no-error.
if error-status:error then do:
   RUN Fill-AlertSysMes IN h_tmess("","",-1,"Неверно задано значение настроечного параметра ~"ДатаЗо~".").

   {intrface.del}          /* Выгрузка инструментария. */ 
   return.
end.

/*=============================================================================*/
find work-op  where recid(work-op)  eq rid-work    no-lock.
find op-templ where recid(op-templ) eq rid-templ   no-lock.
find op-kind  of op-templ                          no-lock.

RUN GetDebugXAttr (op-kind.op-kind, OUTPUT vDebugXAttr).

assign
   tcur = ?
   tacct-db = ?
   tacct-cr = ?
   tamt = 0
.

{chkacces.i}

mMBRMask = fGetSetting ("НазнСчМБР", ?, "").

mStrTMP = GetSysConf("VidSpis").
IF {assigned mStrTMP} THEN
    ASSIGN
        mVidSpis = INT64(ENTRY(1, mStrTMP, ";"))
        mDocType = ENTRY(2, mStrTMP, ";")
    NO-ERROR.

doc:
DO TRANS WITH FRAME opreq ON ENDKEY UNDO, LEAVE ON ERROR UNDO,LEAVE:

   CLEAR FRAME opreq NO-PAUSE.

   COLOR DISPLAY BRIGHT-GREEN
      name-db
      name-cr
      doc-type.name
      bank1.name
      bank2.name
   .
   COLOR DISPLAY BRIGHT-WHITE
       op.doc-type
       op.doc-num
       op.doc-date
       op-entry.acct-db
       op-entry.acct-cr
       op-entry.amt-cur
       op-entry.amt-rub
       op-entry.symbol
       op.name-ben
       vmfo
       vcorr-acct
       op.ben-acct
       op.details
   .


/*  дата вычисления комиссии всегда = in-op-date,    *
 *  op.op-date = или ? или in-op-date в зависимости  *
 *  от того, что стоит в шаблоне                     */
   cur-op-date = in-op-date.

   IF work-op.op EQ ? THEN DO:
      CREATE op.
      {op(sess).cr}
      {g-op.ass &workfile=yes}

       IF GetSysConf("Карт2Date") NE "" AND GetSysConf("Карт2Date") NE ? THEN
       DO:
          op.doc-date = DATE(ENTRY(1, GetSysConf("Карт2Date"))) NO-ERROR.
          op.ins-date = DATE(ENTRY(2, GetSysConf("Карт2Date"))) NO-ERROR.
       END.

       IF GetSysConf("Карт2Doc-Type") NE "" AND GetSysConf("Карт2Doc-Type") NE ? THEN
          op.doc-type = GetSysConf("Карт2Doc-Type").
       ELSE
          op.doc-type = ENTRY(1, op.doc-type).

      IF work-op.doc-kind NE ?  AND
         work-op.doc-kind NE "" THEN
         op.doc-kind = work-op.doc-kind.

      IF work-op.op-transaction NE ? THEN ASSIGN
         op.op-transaction = work-op.op-transaction
         cur-op-trans      = work-op.op-transaction
      .

      IF work-op.mfo NE ? THEN ASSIGN
         vmfo        = work-op.mfo
         vcorr-acct  = work-op.corr-acct
         op.ben-acct = work-op.ben-acct
         op.name-ben = work-op.name-ben
         op.inn   = work-op.inn
         mBankCodeType = "МФО-9"
      .
      ASSIGN
         op.order-pay = IF work-op.order-pay NE ?
                           THEN work-op.order-pay
                           ELSE op.order-pay
      .
   END.
   ELSE DO:
      FIND op WHERE op.op EQ work-op.op EXCLUSIVE-LOCK.
   END.

   FIND doc-type OF op NO-LOCK NO-ERROR.
   RUN cr-open.p (OUTPUT ok,
                  in-op-date,
                  cur-op-date,
                  rid-work,
                  rid-templ,
                  OUTPUT out-rid,
                  RECID(op)).
   ok = 1.
   FIND op-entry WHERE RECID(op-entry) EQ out-rid EXCLUSIVE-LOCK.
   IF AVAIL(op-entry) THEN
      ASSIGN op-entry.currency = IF op-entry.currency EQ ? THEN "" ELSE op-entry.currency.
   out-rid = ?.
   {g-acctv.i &workfile=yes &no-disp=yes}
   work-op.amt-rub-enable = (work-op.amt-rub EQ 0 OR work-op.amt-rub EQ ?).
   IF GetSysConf("ImpED107") EQ "Да" THEN is-pack = YES.
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRDDEC1 ", "is-pack: " + String(is-pack) + " " + 
                               "op-spis: " + String(op-spis) + " " +
                               "avail kau: " + String(AVAIL(kau)) + " " +
                               "op-entry.kau-db: " + op-entry.kau-db + " " +
                               "op-entry.kau-cr: " + op-entry.kau-cr + " " +
                               "flager : " + String(flager)).
   &ENDIF
   IF op-spis THEN DO:
      IF {assigned op-entry.kau-cr} THEN DO:
         RUN processing-kau (op-entry.acct-cr,
                             op-entry.currency,
                             "карт",
                             op-entry.kau-cr,
                             "cr").
         IF {&RETURN_VALUE} EQ "UNDO" THEN DO:
            OK = -1.
            UNDO  doc, LEAVE doc.
         END.
      END.
      IF {assigned op-entry.kau-db} THEN DO:
         RUN processing-kau (op-entry.acct-db,
                             op-entry.currency,
                             "карт",
                             op-entry.kau-db,
                             "db").
         IF {&RETURN_VALUE} EQ "UNDO" THEN DO:
            OK = -1.
            UNDO  doc, LEAVE doc.
         END.
      END.

      IF NOT AVAIL kau THEN DO:
         RUN processing-kau (op-entry.acct-cr,
                             op-entry.currency,
                             "карт",
                             "*",
                             "cr").
         IF {&RETURN_VALUE} EQ "UNDO" THEN DO:
            OK = -1.
            UNDO  doc, LEAVE doc.
         END.
         RUN processing-kau (op-entry.acct-db,
                             op-entry.currency,
                             "карт",
                             "*",
                             "db").
         IF {&RETURN_VALUE} EQ "UNDO" THEN DO:
            OK = -1.
            UNDO  doc, LEAVE doc.
         END.
      END.
   END.                                          /* IF op-spis THEN DO:       */

   f-EditDoc = GetXAttrValue( "op-template",
                              op-templ.op-kind + "," +
                              string(op-templ.op-templ),
                              "РедДок") no-error.

   if f-EditDoc eq "Да" or not is-pack then do:
     {g-acctv.i &workfile=yes }
   end.

   {g-amt.i}                                     /* присвоение суммы          */

/*
   IF     getsysconf("crddec1_mDocDate") NE ? 
      AND getsysconf("crddec1_mDocDate") NE "" 
      AND op.acct-cat EQ "b" THEN
   DO:
      op.doc-date = DATE(getsysconf("crddec1_mDocDate")) NO-ERROR.
      RUN setsysconf IN h_base("crddec1_mDocDate", "").
   END.

   IF     getsysconf("crddec1_mInsDate") NE ? 
      AND getsysconf("crddec1_mInsDate") NE ""  
            AND op.acct-cat EQ "b" THEN
   DO:
      op.ins-date = DATE(getsysconf("crddec1_mInsDate")) NO-ERROR.
      RUN setsysconf IN h_base("crddec1_mInsDate", "").
   END.
*/

   FIND xop WHERE xop.op EQ INT64(ENTRY(1,kau.kau)) NO-LOCK NO-ERROR.
   IF AVAIL xop THEN 
   DO:
      mTypTMP = GetXAttrValueEx("op",STRING(xop.op),"alt-type","").
      IF mTypTMP NE "" THEN 
         ASSIGN op.doc-type = mTypTMP.
   END.

   if f-EditDoc eq "Да" or not is-pack THEN

      DISPLAY op.doc-type         WHEN op.doc-type NE ""
              doc-type.name       WHEN AVAIL doc-type
              op.doc-num          WHEN op.doc-num  NE ""
              op.doc-date         WHEN op.doc-date NE ?
              op.order-pay
              op.due-date
              op-entry.acct-db    WHEN op-entry.acct-db NE ?
              op-entry.acct-cr    WHEN op-entry.acct-cr NE ?
              op-entry.value-date
              op-entry.currency   WHEN op-entry.currency NE ?
              op-entry.amt-cur    WHEN op-entry.currency NE ""
              op-entry.amt-rub
              op.name-ben         WHEN op.name-ben      NE ?
              op.inn              WHEN op.inn           NE ?
              vmfo                WHEN vmfo NE ?
              vcorr-acct          WHEN vcorr-acct NE ?
              op.ben-acct         WHEN op.ben-acct NE ?
              op.details
      .
   else do:
      if op.doc-num <> "" then do:
         for each b-op where b-op.op-transaction = cur-op-trans
                         and b-op.doc-num = "":
             b-op.doc-num = op.doc-num.
         end.
      end.
      else do:
         assign cp-num = INT64(GetXAttrValue("op-template",
                                               op-templ.op-kind + "," +
                                               string(op-templ.op-templ),
                                               "КопДокНомер")) no-error.
         if not can-find(first op-templ where
                               op-templ.op-kind eq op-kind.op-kind
                           and op-templ.op-templ = cp-num) then
            op.doc-num = string(INT64(substring(string(op.op),
                                ( IF (length(string(op.op)) - 6) <= 0
                                    then 1
                                    else length(string(op.op)) - 5
                                ),6))).
         if op.doc-num = "" then do:
            find first b-op where b-op.op-transaction eq cur-op-trans
                              and b-op.doc-num        ne "" no-error.
            if avail b-op then op.doc-num = b-op.doc-num.
         end.
      end.
   end.                                          /* if not is-pack else       */

   IF AVAIL kau THEN DO:

      IF kau.kau-id EQ "КартБалП" THEN
         mNoOpBank = GetXAttrValue("op-template",
                                   op-templ.op-kind + "," +
                                   string(op-templ.op-templ),
                                   "ОтклБанкРекв") NO-ERROR.

      /* формировать протокол */
      if f-MakeProt eq "Да"
      then run protocol-op-zach(kau.kau).
      FIND xop WHERE xop.op EQ INT64(ENTRY(1,kau.kau)) NO-LOCK NO-ERROR.
      FIND xop-bank OF xop NO-LOCK NO-ERROR.
      IF AVAIL xop AND kau.kau-id BEGINS "КартБал" THEN DO:
         op.doc-num = xop.doc-num.
         mRecid = RECID(xop).

         if not is-pack then
            DISPLAY op.doc-num.
         {find-act.i
            &acct = kau.acct
            &curr = kau.currency
         }
         IF AVAIL acct AND acct.side EQ "П" THEN DO:
            FIND xop-entry OF xop WHERE xop-entry.op-entry EQ INT64(entry(2,kau.kau))
                 NO-LOCK NO-ERROR.
            IF AVAIL xop-entry AND op-template.acct-cr EQ ? THEN DO:
               mStrTMP = xop-entry.acct-db.
               {find-act.i
                  &acct = xop-entry.acct-db
                  &curr = xop-entry.currency
               }
               IF AVAIL acct AND acct.close-date NE ? 
               THEN DO:
                  FIND FIRST bacct WHERE ( IF shMode THEN bacct.filial-id = shFilial ELSE YES) 
                                     AND bacct.bal-acct = acct.bal-acct
                                     AND bacct.currency = acct.currency
                                     AND bacct.close-date EQ ?
                  NO-LOCK NO-ERROR.
                  IF AVAIL bacct 
                     THEN mStrTMP = bacct.acct.
               END.
               if not is-pack                                         
                  then DISPLAY mStrTMP @ op-entry.acct-cr.
                  else op-entry.acct-cr = mStrTMP.
            END.
         END.
         ELSE IF AVAIL acct AND acct.side EQ "А" THEN DO:
            FIND xop-entry OF xop WHERE xop-entry.op-entry EQ INT64(entry(2,kau.kau))
                 NO-LOCK NO-ERROR.
            IF AVAIL xop-entry AND op-template.acct-db EQ ? THEN DO:
               mStrTMP = xop-entry.acct-cr.
               {find-act.i
                  &acct = xop-entry.acct-cr
                  &curr = xop-entry.currency
               }
               IF AVAIL acct AND acct.close-date NE ? 
               THEN DO:
                  FIND FIRST bacct WHERE ( IF shMode THEN bacct.filial-id = shFilial ELSE YES) 
                                     AND bacct.bal-acct = acct.bal-acct
                                     AND bacct.currency = acct.currency
                                     AND bacct.close-date EQ ?
                  NO-LOCK NO-ERROR.
                  IF AVAIL bacct 
                     THEN mStrTMP = bacct.acct.
               END.
               if not is-pack
                  then DISPLAY mStrTMP @ op-entry.acct-db.
                  else op-entry.acct-db = mStrTMP.
            END.
         END.
         
      END.
      RUN DeleteOldDataProtocol IN h_base("БАЛ-ДОК:DATE-TO-KART").
      IF AVAIL xop AND xop.acct-cat EQ "o" THEN DO:
         tmp-sign = GetXAttrValue("op",string(xop.op),"op-bal").
         IF tmp-sign ne "" THEN
            FIND kau-op WHERE kau-op.op EQ INT64(tmp-sign)
                              NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST kau-op WHERE kau-op.op-transaction EQ xop.op-transaction
                                AND kau-op.acct-cat       EQ "b"
                                AND RECID(kau-op)         NE RECID(xop)
                                    NO-LOCK NO-ERROR.
         IF AVAIL kau-op THEN
/*         DO:
            RUN SetSysConf in h_base ("БАЛ-ДОК:OP-BAL" ,  STRING(kau-op.op)).
            RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-KIND", kau-op.doc-kind).
            RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-NUM", kau-op.doc-num).
            RUN SetSysConf in h_base ("БАЛ-ДОК:ORDER-PAY", kau-op.order-pay).
            RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-DB"  , GetXattrValue("op",STRING(kau-op.op),"acctbal")).
            RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-CR", GetXattrValue("op",STRING(kau-op.op),"acctcorr")).          
            RUN SetSysConf in h_base ("БАЛ-ДОК:BEN-ACCT", kau-op.ben-acct).
            RUN SetSysConf in h_base ("БАЛ-ДОК:NAME-BEN", kau-op.name-ben).
            RUN SetSysConf in h_base ("БАЛ-ДОК:INN"     , kau-op.inn).
            RUN SetSysConf in h_base ("БАЛ-ДОК:DETAILS", kau-op.details).
            RUN SetSysConf in h_base ("БАЛ-ДОК:AMT-RUB", GetXattrValue("op",STRING(kau-op.op),"amt-rub")).
            RUN SetSysConf in h_base ("БАЛ-ДОК:AMT-CUR", GetXattrValue("op",STRING(kau-op.op),"amt-cur")).
            RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-DATE" , STRING(kau-op.doc-date)).
            FIND FIRST bdoc-type WHERE bdoc-type.doc-type EQ kau-op.doc-type NO-LOCK NO-ERROR.
            RUN SetSysConf in h_base ("БАЛ-ДОК:НАИМ-ТИП-ДОК", IF AVAIL(bdoc-type) THEN bdoc-type.name-doc ELSE ""). */
            op.order-pay = kau-op.order-pay. 
            tmp-sign = GetXAttrValueEx("op",tmp-sign,"ДатаПомещенияВКарт","").
            Date2Kart = DATE(tmp-sign) NO-ERROR.
            RUN SetSysConf in h_base ("БАЛ-ДОК:DATE-TO-KART", IF Date2Kart NE ? THEN STRING(Date2Kart) ELSE "").
            RUN SetSysConf in h_base ("БАЛ-ДОК:NALPL_ED", GetXattrValueEx("op-kind",kau-op.op-kind,"РучВводНР","Нет")).
/*         END. */
      END.
      ELSE DO:
         op.doc-kind = xop.doc-kind.
         RUN SetSysConf in h_base ("БАЛ-ДОК:NALPL_ED", GetXattrValueEx("op-kind",op.op-kind,"РучВводНР","Нет")).
         RUN Copy-Xattr-Op(RECID(xop),RECID(op),"ПокСт,КБК,ОКАТО-НАЛОГ,ПокОП,ПокНП,ПокНД,ПокДД,ПокТП,УИН,УИЗ,ЕИП,АИП").

         IF db-cr = YES THEN DO:
            tmp-sign = GetXAttrValue("op",string(xop.op),"acct-rec").
            IF tmp-sign ne "" THEN DO:
               FIND xop-entry OF xop WHERE xop-entry.op-entry EQ INT64(entry(2,kau.kau))
                    NO-LOCK NO-ERROR.
               IF AVAIL xop-entry THEN
                  {find-act.i
                     &acct = tmp-sign
                     &curr = xop-entry.currency
                  }
               ELSE
                  {find-act.i
                     &acct = tmp-sign
                     &curr = "''"
                  }
               IF AVAIL acct AND 
                  acct.close-date EQ ? AND 
                  (op-entry.acct-cr EQ ? OR op-entry.acct-cr EQ "") 
               THEN DO:
                  if not is-pack then DISPLAY acct.acct @ op-entry.acct-cr.
                  else op-entry.acct-cr = acct.acct.
               END.
            END.
         END.
         IF db-cr = NO THEN DO:
            tmp-sign = GetXAttrValue("op",string(xop.op),"acct-send").
            IF tmp-sign ne "" THEN DO:
               FIND xop-entry OF xop WHERE xop-entry.op-entry EQ INT64(entry(2,kau.kau))
                                           NO-LOCK NO-ERROR.
               IF AVAIL xop-entry THEN
                  {find-act.i
                     &acct = tmp-sign
                     &curr = xop-entry.currency
                  }
               ELSE
                  {find-act.i
                     &acct = tmp-sign
                     &curr = "''"
                  }
               IF AVAIL acct THEN DO:
                  if not is-pack
                     then DISPLAY acct.acct @ op-entry.acct-db.
                     else op-entry.acct-db = acct.acct.
               END.
            END.
         END.
      END.
      IF AVAIL xop-bank THEN DO:

         IF mNoOpBank NE "Да" THEN  
         ASSIGN
            vmfo        = IF xop-bank.bank-code NE ? THEN xop-bank.bank-code
                                                     ELSE vmfo
            vcorr-acct  = IF xop-bank.corr-acct NE ? THEN xop-bank.corr-acct
                                                     ELSE vcorr-acct
            mBankCodeType = IF xop-bank.bank-code-type NE ? THEN xop-bank.bank-code-type
                                                     ELSE "МФО-9" 
         .

         ASSIGN
            op.ben-acct = IF xop.ben-acct       NE ? THEN xop.ben-acct
                                                     ELSE op.ben-acct
            op.inn      = IF xop.inn            NE ? THEN xop.inn
                                                     ELSE op.inn
            op.name-ben = IF xop.name-ben       NE ? THEN xop.name-ben
                                                     ELSE op.name-ben
         .
         if not is-pack
            then DISPLAY op.doc-type WHEN op.doc-type NE ""
                         op.inn      WHEN op.inn      NE ?
                         op.name-ben WHEN op.name-ben NE ?
                         vmfo        WHEN vmfo        NE ?
                         vcorr-acct  WHEN vcorr-acct  NE ?
                         op.ben-acct WHEN op.ben-acct NE ?
                 .
      END.
      IF op.acct-cat NE "o" THEN 
        op.details = IF AVAIL xop
                        THEN xop.details
                        ELSE IF {assigned op-templ.details}
                                THEN op-templ.details
                                ELSE op.details.
      if not is-pack then
         DISPLAY op.order-pay
                 op.details
        .   

        IF op-spis AND {assigned op-entry.kau-cr} THEN
        DO:
           FIND FIRST tmpOp WHERE tmpOp.op EQ INT64(ENTRY(1, op-entry.kau-cr)) NO-LOCK NO-ERROR.
   
           mOpTranTMP = tmpOp.op-transaction.
   
         IF  CAN-FIND(FIRST op-template OF op-kind WHERE op-template.acct-cat EQ "b")
             AND CAN-FIND(FIRST op WHERE op.op-transaction EQ mOpTranTMP AND op.op-status BEGINS "А")
            THEN 
         DO: 
            RUN get-kau-id in h_kau (op-entry.acct-cr, op-entry.currency, OUTPUT mStrTMP).
            mSumm = CalcRealAcctPos(op-entry.kau-cr, 
                                    IF mStrTMP EQ "КартБлСч" THEN mStrTMP ELSE "",
                                    "",
                                    "",
                                    op.order-pay, 
                                    op.doc-date). 
            IF op-entry.currency EQ "" THEN op-entry.amt-rub = mSumm.
            ELSE op-entry.amt-cur = mSumm.
            DISPLAY
               op-entry.amt-rub
               op-entry.amt-cur
            .
         END.  
      END.  
   END.                                    /* IF AVAIL kau THEN DO:     */

   DO ON ERROR  UNDO,     RETRY
      ON ENDKEY UNDO doc, LEAVE doc:

      /* если счета закрыты, то разрешаем их редактирование */
      IF AVAILABLE op-entry THEN DO:
         acct-db-enable = acct-db-enable
                          OR
                          CAN-FIND(FIRST acct WHERE acct.acct EQ op-entry.acct-db
                                              &IF DEFINED(ORACLE) EQ 0 &THEN
                                              AND op-entry.currency BEGINS acct.currency
                                              &ENDIF
                                              AND acct.close-date LE gend-date
                          )
         .
         acct-cr-enable = acct-cr-enable
                          OR
                          CAN-FIND(FIRST acct WHERE acct.acct EQ op-entry.acct-cr
                                              &IF DEFINED(ORACLE) EQ 0 &THEN
                                              AND op-entry.currency BEGINS acct.currency
                                              &ENDIF
                                              AND acct.close-date LE gend-date
                          )
         .
      END.

      
      IF     AVAIL op
         AND AVAIL op-entry  THEN DO:
         CREATE wop.
         ASSIGN
            sWOP_SEQUENCE     = sWOP_SEQUENCE + 1
            wop.op-templ      = op-templ.op-templ
            wop.op-kind       = op-templ.op-kind
            wop.details       = op-templ.details
            wop.mfo-needed    = op-templ.mfo-needed
            wop.type          = op-templ.type
            wop.op-cod        = op-templ.op-cod
            wop.op-status     = op-templ.op-status
            wop.acct-cat      = op-templ.acct-cat
            wop.prepf         = op-templ.prep-amt-rub
            wop.prepnv        = op-templ.prep-amt-natcur
            wop.fwop_sequence = sWOP_SEQUENCE
         .
         RUN op-class IN h_xclass (INPUT-OUTPUT wop.class-code, BUFFER op-template).

         IF mRecid NE ? THEN wop.op-recid = mRecid.

      END.

      IF GetXAttrValue( "op-template", op-templ.op-kind + "," + STRING(op-templ.op-templ), "ЧастОтказ") EQ "Да"
      THEN amt-rub-enable = YES. 

      IF NOT {assigned mStrTMP} AND op.acct-cat EQ "b" AND avail(kau) THEN DO: /* не опередан вид списания */ 

         RUN chktpdoc.p (RECID(kau),
                         RECID(op-template),
                         IF op-entry.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur,
                         OUTPUT mDocType,
                         OUTPUT mClassDoc,
                         OUTPUT mVidSpis).
         
         op.doc-type  = mDocType. 

      END.


      ASSIGN
         vOpRecId = INT64(GetSysConf("Карт2Нал"))
      .
      IF vOpRecID NE ? AND vOpRecId NE 0 THEN
         RUN Copy-Xattr-Op(vOpRecId,RECID(op),"ПокСт,Kpp-send,Kpp-rec,КБК,ОКАТО-НАЛОГ,ПокОП,ПокНП,ПокНД,ПокДД,ПокТП,УИН,УИЗ,ЕИП,АИП").

      IF f-EditDoc eq "Да" or NOT is-pack THEN
      DO:
         IF AVAIL(op) THEN
         DO:
           FIND FIRST code WHERE 
                    code.class EQ "acct-cat"
              AND   code.code  EQ op.acct-cat
           NO-LOCK NO-ERROR.
           IF AVAIL(code) 
              AND   code.description[3] NE "Нет" THEN mNeedZO = YES. 
         END.
         {set.i &THIS_FRAME = "opreq" &EXFILE = "crddec.i.st1" &NODISPLAY = "YES" {&*}} .
      END.
      IF     AVAIL op
         AND AVAIL op-entry  THEN DO:
         
         ASSIGN
            wop.currency      = op-entry.currency
            wop.doc-type      = op.doc-type
            wop.value-date    = op-entry.value-date
            wop.acct-db       = op-entry.acct-db
            wop.acct-cr       = op-entry.acct-cr
            wop.amt-cur       = if op-entry.currency eq "" then op-entry.amt-rub
                                                           else op-entry.amt-cur
            wop.details       = op.details
            wop.symbol        = op-entry.symbol
         .

      END.

      IF AVAIL op-entry    AND
         AVAIL kau         THEN DO:

         FIND xop-entry WHERE xop-entry.op       EQ INT64(ENTRY(1,kau.kau))
                          AND xop-entry.op-entry EQ INT64(ENTRY(2,kau.kau))
                                                         NO-LOCK NO-ERROR.
         {find-act.i
            &acct = kau.acct
            &curr = kau.currency
         }
         IF AVAIL xop-entry AND AVAIL acct THEN DO:

            IF acct.side EQ "П" THEN DO:
               IF kau.kau-id BEGINS "КартБал" THEN
                  IF xop-entry.acct-db NE op-entry.acct-cr AND mNoOpBank NE "Да"  THEN DO:
                     if not UpdateSigns("op",string(op.op),"acct-db",xop-entry.acct-db,no)
                        then undo, leave.
                  END.
                  ELSE
                     op.doc-kind = IF op.doc-kind EQ "rec" THEN "send"
                                   ELSE IF op.doc-kind EQ "send" THEN "rec"
                                   ELSE op.doc-kind.
               {find-act.i
                  &bact = bAcct
                  &acct = op-entry.acct-cr
                  &curr = op-entry.currency
               }
               IF AVAIL bAcct THEN
                  mRetPay = CAN-DO (mMBRMask, bAcct.contract).
            END.
            ELSE IF acct.side EQ "А" THEN DO:
               IF kau.kau-id BEGINS "КартБал" THEN
                  IF xop-entry.acct-cr NE op-entry.acct-db THEN DO:
                     if not UpdateSigns("op",string(op.op),"acct-cr",xop-entry.acct-cr,no)
                        then undo, leave.
                  END.
                  ELSE
                     op.doc-kind = IF op.doc-kind EQ "rec" THEN "send"
                                   ELSE IF op.doc-kind EQ "send" THEN "rec"
                                   ELSE op.doc-kind.
               {find-act.i
                  &bact = bAcct
                  &acct = op-entry.acct-db
                  &curr = op-entry.currency
               }
               IF AVAIL bAcct THEN
                  mRetPay = CAN-DO (mMBRMask, bAcct.contract).
            END.

            IF mRetPay THEN   /* возврат плательщику*/
            DO:
               mKPP-rec  = GetXAttrValueEx ("op",     STRING (xop-entry.op), "KPP-send", "").
               mKPP-send = GetXattrValueEx ("branch", acct.branch-id,  "КПП",      "").
               IF mKPP-send EQ "" THEN
                  mKPP-send = fGetSetting ("БанкКПП", ?, "").
            END.
         END.
         
         IF mRetPay THEN
         DO:
            UpdateSigns ("opb", STRING (op.op), "Kpp-send", mKPP-send, ?).
            UpdateSigns ("opb", STRING (op.op), "Kpp-rec",  mKPP-rec , ?).
         END.
         ELSE
            RUN Copy-Xattr-Op(RECID(xop),RECID(op),"Kpp-send,Kpp-rec").

      END.                                      /* kau.kau-id BEGINS "КартБал"*/

      IF vmfo NE "" THEN DO:
         {opbnkcr.i op.op """" mBankCodeType vmfo vcorr-acct}
         {op-type.upd &check-format=Yes &surr=string(op.op)}
      END.

      FIND FIRST xop WHERE RECID(xop) EQ vOpRecID NO-LOCK NO-ERROR.
      IF AVAIL xop THEN DO:
         _OPBANK:
         FOR EACH xop-bank OF xop NO-LOCK:
            IF vmfo NE "" AND xop-bank.op-bank-type EQ "" THEN
               NEXT _OPBANK.
            CREATE op-bank.
            BUFFER-COPY xop-bank EXCEPT op TO op-bank NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN
               op-bank.op = op.op.
         END.
      END.

      {op-type.chk}
      FIND op-entry WHERE op-entry.op       = op.op
                      AND op-entry.op-entry = IF work-op.op-entry NE ?
                                                 THEN work-op.op-entry
                                                 ELSE op-templ.op-templ
           EXCLUSIVE-LOCK.

      ASSIGN
         op.class-code = IF GetSysConf('Карт2КлассДок') NE '' AND
                            GetSysConf('Карт2КлассДок') NE ?
                         THEN GetSysConf('Карт2КлассДок')
                         ELSE op.class-code
      .
      t-details = op.details.
      RUN ProcessDetails ( IF avail wop THEN RECID(wop) 
                                       ELSE ?, 
                          INPUT-OUTPUT t-details).
      op.details = t-details.
      RUN parssign.p (in-op-date,
                      "op-template",
                      op-kind.op-kind + "," + string(op-templ.op-templ),
                      op-templ.class-code,
                      "op",
                      STRING(op.op),
                      op.class-code,
                      RECID(wop)).

      IF     mVidSpis NE ?
      AND {assigned mDocType}
         AND op.acct-cat EQ "b"
         AND mDocType NE op.doc-type 
         AND (   NUM-ENTRIES(mDocType) EQ 1 
              OR NOT CAN-DO(mDocType, SUBSTRING(op.doc-type, INDEX(op.doc-type, ",") + 1)) 
         )
         THEN DO:
         DO:
            mblodd_char_Tmp01 = pick-value.
            RUN Fill-AlertSysMes IN h_tmess("","",4,"Тип документа не соответствует" + CHR(32) + IF mVidSpis EQ 1 THEN "полному" ELSE "частичному" + CHR(32) + "скорректировать на код <" + CHR(32) + STRING(mDocType) + CHR(32) + ">?").
            mBtnQues = (pick-value = "YES").
            pick-value = mblodd_char_Tmp01.
         END.


         IF mBtnQues THEN
            op.doc-type = mDocType.
      END.

      RUN inipoxtr.p (RECID(op),?).

      {xattr-cr.i}
      IF NOT is-pack THEN DO:         
         /* вызываем форму редактирования налоговых реквизитов */
         IF GetSysConf("Карт2ФормаНал") NE "NO" AND NOT is-pack THEN DO:
            RUN nalpl_ed.p (RECID(op), 2, 3).
            IF {&RETURN_VALUE} EQ "ESC" THEN UNDO, RETRY.
         END.
      end.

      IF GetSysConf("Карт1274") NE "" THEN
         RUN SetSysConf IN h_base ( "ОтменитьПроверкуСобытий",
                                    op-kind.op-kind         + "!" + STRING(RECID(op-entry)) + "#" + "d_crp"
                                  ).
      {op-entry.upd &file      = op-entry
                    &871       = yes
                    &open-undo = "if is-pack then undo, return. else undo,retry "
                    &undo      = "if is-pack then undo, return. else undo,retry "
                    &flagerr2-react = "MESSAGE 'Клиент по счету дебета '  op-entry.acct-db:SCREEN-VALUE ' блокирован' VIEW-AS ALERT-BOX ERROR."
                    &flagerr3-react = "MESSAGE 'Клиент по счету кредита ' op-entry.acct-cr:SCREEN-VALUE ' блокирован' VIEW-AS ALERT-BOX ERROR."
      }
      IF db-cr NE ? THEN
         RUN crdex.p(RECID(op-entry),db-cr).
   END.
   out-rid = IF AVAILABLE op-entry THEN RECID(op-entry)
                                   ELSE ?.
   /* формирование протокола */
   if f-MakeProt eq "Да"
   then do:
      if out-rid eq ? then do:
         if avail prottbl1
         then assign prottbl1.ok = "Не успешно".
      end.
      else
         run protocol-op-spis (out-rid,mKau).
   end.

   ok = 0.

END.

HIDE FRAME opreq no-pause.
delete procedure hproc.

return.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE processing-kau :
   DEFINE INPUT PARAM in-acct  LIKE acct.acct NO-UNDO.
   DEFINE INPUT PARAM in-curr  LIKE acct.currency NO-UNDO.
   DEFINE INPUT PARAM in-card  LIKE code.code NO-UNDO.
   DEFINE INPUT PARAM in-kau   LIKE kau.kau NO-UNDO.
   DEFINE INPUT PARAM in-dbcr  AS   CHAR NO-UNDO.

sss:
DO                                
    ON ERROR  UNDO sss, LEAVE sss 
    ON ENDKEY UNDO sss, LEAVE sss:

   IF in-kau = '*' THEN DO:
      {find-act.i
         &acct   = in-acct
         &curr   = in-curr
         &AddWhere = " AND (  acct.kau-id BEGINS in-card
                           OR CAN-FIND(bal-acct of acct WHERE bal-acct.kau-id BEGINS in-card
                                                        AND   acct.kau-id     NE bal-acct.kau-id))"
      }
      IF AVAIL acct THEN DO:
         if is-pack eq no then do:
            pick-value = "mult".
            {empty tmprecid}

            RUN kauacctv.p(in-acct,in-curr,5).
            IF {&KEY_FUNCTION}({&LAST_KEY}) EQ "END-ERROR" THEN RETURN "UNDO".

            FOR EACH tmprecid:
               FIND FIRST ekau WHERE recid(ekau) EQ tmprecid.id NO-LOCK NO-ERROR.
               IF AVAIL ekau THEN
               DO:
                  FIND FIRST signs WHERE signs.FILE-NAME EQ "op"
                     AND signs.surrogate EQ ENTRY(1,kau.kau)
                     AND signs.CODE EQ "op-bal" NO-LOCK NO-ERROR.
                  IF AVAIL signs THEN
                     FIND FIRST eop WHERE eop.op EQ int64(signs.xattr-value) NO-LOCK NO-ERROR.
                  IF AVAIL eop  AND acct.kau-id EQ "Карт-ка2" THEN
                  DO:
                     /*проверка на очередность и номер*/
                     FOR EACH skau WHERE skau.acct EQ ekau.acct
                         AND skau.currency EQ ekau.currency
                         AND skau.zero-bal EQ ekau.zero-bal
                         AND skau.kau      NE kau.kau  NO-LOCK,
                         FIRST signs WHERE signs.FILE-NAME EQ "op"
                                       AND signs.surrogate EQ ENTRY(1,skau.kau)
                                       AND signs.CODE EQ "op-bal" NO-LOCK,
                         FIRST sop WHERE sop.op EQ int(signs.xattr-value) NO-LOCK:  
                        
                         IF FGetSetting("СтандТр","ГрОчСписК2",?) EQ "ДА" THEN DO:
                         IF int64(sop.order-pay) LT int64(eop.order-pay) THEN
                         DO:
                             RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. имеется документ с большим приоритетом").

                             UNDO  sss.
                         END.
                         END.

                         IF FGetSetting("СтандТр","ДатСписК2",?) EQ "ДА" THEN DO:
                         IF  int64(sop.order-pay) EQ int64(eop.order-pay)
                             AND sop.doc-date   > eop.doc-date THEN
                         DO:
                             RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. имеется документ с ранней датой").

                             UNDO  sss.
                         END. 
                         END.

                         IF FGetSetting("СтандТр","РОпСписК2",?) EQ "ДА" THEN DO:
                         IF  int64(sop.order-pay) EQ int64(eop.order-pay)
                             AND sop.doc-date   EQ eop.doc-date
                             AND (sop.doc-type NE "06"
                                  AND sop.doc-type NE "09"
                                  AND (eop.doc-type EQ "06"
                                       OR eop.doc-type EQ "09")) THEN
                         DO:
                             RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. документ с типом 06 и 09 идут в последнюю очередь").

                             UNDO  sss.
                         END.
                         END.       

                         IF FGetSetting("СтандТр","НомСписК2",?) EQ "ДА" THEN DO:
                         IF int64(sop.order-pay) EQ int64(eop.order-pay)
                             AND sop.doc-date   EQ eop.doc-date
                             AND sop.doc-type   EQ eop.doc-type
                             AND int64(sop.doc-num) LT int64(eop.doc-num)THEN
                         DO:
                             RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. имеется документ с меньшим номером").

                             UNDO  sss.
                         END.
                         END.
                     END.
                  END.
               END.
            END.
            for each tmprecid: accumulate tmprecid.id (count). end.
            if (accum count tmprecid.id) >= 1 then do:
               if (accum count tmprecid.id) > 1 then is-pack = yes.

               IF FGetSetting("Карт2", "ОчерСписК2", "Нет") NE "Да" THEN
               DO:
                  FIND FIRST tmprecid.
                  pick-value = STRING(tmprecid.id).
                  DELETE tmprecid.
               END.
               ELSE
               DO:
                  RUN CreateOrderingTT.

                  FOR FIRST ttKauRidOp
                     BY ttKauRidOp.order-pay
                     BY ttKauRidOp.doc-date
                     BY ttKauRidOp.sort:

                     pick-value = STRING(ttKauRidOp.rid-kau).
                     DELETE ttKauRidOp.
                  END.
               END.
            end.
            IF {&LAST_KEY} EQ 10 AND pick-value NE ? THEN DO:
               FIND kau WHERE RECID(kau) EQ INT64(pick-value) NO-LOCK NO-ERROR.
               IF NOT AVAIL kau THEN RETURN "UNDO".
            END.
            ELSE
               RETURN "UNDO".
         END.                                   /* if is-pack eq no then      */
         else do:
            &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgprint.p ("processing-kau ", "is-pack: " + String(is-pack) + " " + 
                                               "in-acct: " + in-acct  + " " +
                                               "AVAIL acct: " + String(AVAIL(acct))).
            &ENDIF
            release kau.
            find first tmprecid no-error.
            if avail tmprecid and GetSysConf("ImpED107") NE "Да" then do:
               pick-value = string(tmprecid.id).
               delete tmprecid.
               FIND kau WHERE RECID(kau) EQ INT64(pick-value) NO-LOCK NO-ERROR.
               IF NOT AVAIL kau THEN RETURN "UNDO".
            end.
            ELSE
            DO:
               vVBOp = INT64(GetSysConf("ImpED107VBOP")).
               &IF DEFINED(IS-DEBUG) &THEN
               RUN dbgprint.p ("processing-kau ", "is-pack: " + String(is-pack) + " " + 
                                                  "in-acct: " + in-acct  + " " +
                                                  "vVBOp: " + String(vVBOp) + " " +
                                                  "AVAIL acct: " + String(AVAIL(acct))).
               &ENDIF
               IF GetSysConf("ImpED107") EQ "Да" THEN
               DO:
                  FIND FIRST kau WHERE kau.acct EQ in-acct AND kau.currency EQ in-curr 
                                  AND INT64(ENTRY(1,kau.kau)) EQ vVBOp NO-LOCK NO-ERROR.
                  IF NOT AVAIL(kau) THEN RETURN "UNDO".
               END.
               ELSE RETURN "UNDO".
            END.
         end.                                   /* if is-pack eq no else      */
      END.                                      /* IF AVAIL acct THEN         */
      ELSE
         RETURN.
   END.                                         /* IF in-kau = '*' THEN       */
   ELSE DO:
      FIND kau WHERE kau.acct     EQ in-acct
                 AND kau.currency EQ in-curr
                 AND kau.kau      EQ in-kau
                     NO-LOCK NO-ERROR.
      IF NOT AVAIL kau THEN RETURN "UNDO".
   END.
   IF AVAIL kau THEN DO:
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("processing-kau ", "is-pack: " + String(is-pack) + " " + 
                                         "in-acct: " + in-acct  + " " +
                                         "kau.balance: " + String(kau.balance)  + " " +
                                         "AVAIL kau: " + String(AVAIL(kau))).
      &ENDIF
      IF kau.balance EQ 0 THEN RETURN "UNDO".

      FIND FIRST signs WHERE signs.FILE-NAME EQ "op"
                    AND signs.surrogate EQ ENTRY(1,kau.kau)
                    AND signs.CODE EQ "op-bal" NO-LOCK NO-ERROR.
      IF AVAIL signs THEN
         FIND FIRST eop WHERE eop.op EQ int64(signs.xattr-value) NO-LOCK NO-ERROR.
      IF AVAIL eop AND kau.kau-id EQ "Карт-ка2" THEN
      DO:
         /*проверка на очередность и номер*/
         FOR EACH skau WHERE skau.acct EQ kau.acct
             AND skau.currency EQ kau.currency
             AND skau.zero-bal EQ kau.zero-bal
             AND skau.kau      NE kau.kau NO-LOCK,
             FIRST signs WHERE signs.FILE-NAME EQ "op"
                           AND signs.surrogate EQ ENTRY(1,skau.kau)
                           AND signs.CODE EQ "op-bal" NO-LOCK,
             FIRST sop WHERE sop.op EQ int64(signs.xattr-value)
             NO-LOCK:

            IF FGetSetting("СтандТр","ГрОчСписК2",?) EQ "ДА" THEN DO:
            IF int64(sop.order-pay) LT int64(eop.order-pay) THEN
            DO:
                RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. имеется документ с большим приоритетом").

                UNDO  sss.
            END.
            END.

          
            IF FGetSetting("СтандТр","ДатСписК2",?) EQ "ДА" THEN DO:
            IF  int64(sop.order-pay) EQ int64(eop.order-pay)
                AND sop.doc-date   > eop.doc-date THEN
            DO:
                RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. имеется документ с ранней датой").

                UNDO  sss.
            END. 
            END.

            IF FGetSetting("СтандТр","РОпСписК2",?) EQ "ДА" THEN DO:
            IF  int64(sop.order-pay) EQ int64(eop.order-pay)
                AND sop.doc-date   EQ eop.doc-date
                AND (sop.doc-type NE "06"
                     AND sop.doc-type NE "09"
                     AND (eop.doc-type EQ "06"
                          OR eop.doc-type EQ "09")) THEN
            DO:
                RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. документ с типом 06 и 09 идут в последнюю очередь").

                UNDO  sss.
            END.
            END.                                

            IF FGetSetting("СтандТр","НомСписК2",?) EQ "ДА" THEN DO:
            IF int64(sop.order-pay) EQ int64(eop.order-pay)
                AND sop.doc-date   EQ eop.doc-date
                AND sop.doc-type   EQ eop.doc-type
                AND int64(sop.doc-num) LT int64(eop.doc-num)THEN
            DO:
                RUN Fill-AlertSysMes IN h_tmess("","",1,"Нельзя списать этот документ, т.к. имеется документ с меньшим номером").

                UNDO  sss.
            END.
            END.
         END.
      END.
      if kau.curr-bal <= summ-val or summ-val = ? then
         work-op.amt-cur = kau.curr-bal.
      else
         work-op.amt-cur = summ-val.
      if kau.balance  <= summ-rub or summ-rub = ? then
         work-op.amt-rub = kau.balance.
      else
         work-op.amt-rub = summ-rub.

      IF kau.currency NE "" AND kau.currency NE ? THEN DO:
         FIND LAST instr-rate WHERE instr-rate.instr-cat  EQ "currency"
                                AND instr-rate.instr-code EQ work-op.currency
                                AND instr-rate.rate-type  EQ "УЧЕТНЫЙ"
                                AND instr-rate.since LE   IF work-op.value-date NE ?
                                                             THEN work-op.value-date
                                                             ELSE op-entry.value-date
                   NO-LOCK NO-ERROR.
         IF NOT AVAIL instr-rate THEN DO:
            RUN Fill-AlertSysMes IN h_tmess("","",-1,"Не установлен курс валюты '" + CHR(32) + STRING(work-op.currency) + CHR(32) + "'" + CHR(32) + "~n" + CHR(32) + "На дату : " + CHR(32) + STRING(IF work-op.value-date NE ? THEN work-op.value-date ELSE op-entry.value-date)).

            RETURN "UNDO".
         END.
         ASSIGN
            work-op.amt-cur = IF work-op.amt-cur NE ? THEN work-op.amt-cur
                                                      ELSE op-entry.amt-cur
            work-op.amt-rub = IF work-op.amt-rub NE ? AND work-op.currency EQ ""
                                 THEN work-op.amt-rub
                                 ELSE IF work-op.amt-rub  NE ? AND
                                         work-op.currency NE ""
                                         THEN work-op.amt-cur *
                                              instr-rate.rate-instr /
                                              instr-rate.per
                                 ELSE IF op-entry.currency NE ""
                                         THEN op-entry.amt-cur *
                                              instr-rate.rate-instr /
                                              instr-rate.per
                                         ELSE op-entry.amt-rub
         .
      END.
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("processing-kau ", " kau.kau: " + kau.kau + " " +
                      " work-op.amt-rub: " + String(work-op.amt-rub) + " " +
                      " work-op.amt-cur: " + String(work-op.amt-cur) + " " +
                      " flager: " + String(flager)).
      &ENDIF
      IF in-dbcr EQ "cr" THEN op-entry.kau-cr = kau.kau.
                         ELSE op-entry.kau-db = kau.kau.
      DO WITH FRAME opreq:
         if not is-pack
            then DISPLAY work-op.amt-rub @ op-entry.amt-rub
                         work-op.amt-cur @ op-entry.amt-cur.
            else assign  op-entry.amt-rub = work-op.amt-rub
                         op-entry.amt-cur = work-op.amt-cur.
      END.
      IF in-dbcr EQ "cr" THEN db-cr = NO.
                         ELSE db-cr = YES.
      hist-rec-kau = RECID(kau).
   END.
   RETURN.
END.
END PROCEDURE.

PROCEDURE CreateOrderingTT:

   DEFINE VARIABLE vOpBal AS INT64 NO-UNDO.
        
   DEFINE BUFFER opb FOR op.
   DEFINE BUFFER opo FOR op.
   
   {empty ttKauRidOp}

   FOR EACH tmprecid:

      FIND kau WHERE RECID(kau) EQ tmprecid.id NO-LOCK NO-ERROR.

      FIND opo WHERE opo.op EQ INT64(ENTRY(1, kau.kau))
         NO-LOCK NO-ERROR.

      vOpBal = INT64(GetXAttrValueEx("op",
                                       STRING(opo.op),
                                       "op-bal",
                                       ?)).

      IF vOpBal EQ ? THEN
         FIND FIRST opb WHERE opb.op-transaction EQ opo.op-transaction
                          AND opb.acct-cat       EQ "b"
/*                          AND RECID(opb)         EQ RECID(opo)*/
                          AND opb.op-status      BEGINS "А"
            NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST opb WHERE opb.op EQ vOpBal NO-LOCK NO-ERROR.

      CREATE ttKauRidOp.
      ASSIGN
         ttKauRidOp.rid-kau   = RECID(kau)
         ttKauRidOp.order-pay = IF AVAIL opb THEN opb.order-pay ELSE opo.order-pay
         ttKauRidOp.doc-date  = IF AVAIL opb THEN opb.doc-date  ELSE opo.doc-date
         ttKauRidOp.sort      = kau.sort
      .
   END.
END PROCEDURE.
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='20/03/2015 08:46:15.679+04:00' */
/* $LINTUSER='BIS' */
/* $LINTMODE='1' */
/* $LINTFILE='crddec.i' */
/*prosignYtigbWE1x6df1a/QLGj5nA*/
/* --- crddec.i was humbly modified by (c)blodd converter v.1.09 on 6/30/2016 1:15pm --- */
