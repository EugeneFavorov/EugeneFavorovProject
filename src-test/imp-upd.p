/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename:  IMP-UPD.P
      Comment:  Cтандартные проверки для импорта документов
                на входе flag-go = 0 - внутр. документы,
                                   1 - начальные обороты,
                                   2 - ответные обороты,
                                   3 - транзитные документы,
   Parameters:  flag-go no-buf
         Uses:  -
      Used BY:  -
      Created:  06/10/1997 Mike
     Modified:  24/04/1998 Mike
                31/01/2000 Mike  добавлена ошибка m93
     Modified:  25/07/2001 Nik   при контроле повторно принятых документов исключаются
                                 документы со статусом "ошибка".
     Modified:  14/11/2001 Nik   Использован метод GetXAttrValue
     Modified:  23/07/2002 TSL   Добавлена проверка формата ИНН получателя
     Modified:  18/04/2003 NIK   Форматирование кода
                                 Контроль доступности после поиска
     Modified:  05/08/2003 NIK   Контроль терроризма
     Modified:  15/05/2006 Muta  Заявка 0061264.  Исправлена проверка сторон документа 
                                 на справочник террористов в процедуре imp-upd.p 
     Modified:  17/08/2012 Mike  В UT не подняты строки с  DelFilFromAcct(), а в DVP они есть
     Modified:  02/07/2013 0198869

*/

DEF INPUT PARAM flag-go AS INT64           NO-UNDO.
DEF INPUT PARAM no-buf  AS logic    INIT NO  NO-UNDO.

{globals.i}
{chkacces.i}
{imp.wrk}
{bank-id.i}
{sh-defs.i}
{wordwrap.def}
{stoplist.fun &ttw="w-"}

DEF VAR counter         AS INT64                                 NO-UNDO.
DEF VAR key             AS INT64 INIT ?                          NO-UNDO.
DEF VAR i               AS INT64                                 NO-UNDO.
DEF VAR j               AS INT64                                 NO-UNDO.
DEF VAR str-error-stat  AS CHAR                                NO-UNDO.
DEF VAR dict-uer        AS CHAR                                NO-UNDO.
DEF VAR reg-mask        AS CHAR INIT "0445*,0446*,445*,446*"   NO-UNDO.
DEF VAR fl-o            AS INT64                                 NO-UNDO.
DEF VAR mess            AS CHAR                                NO-UNDO.

DEF VAR str-bank        AS CHAR                       NO-UNDO.
DEF VAR bank-name       AS CHAR                       NO-UNDO.
DEF VAR buf             AS CHAR                       NO-UNDO.
DEF VAR lim-pos         LIKE op-entry.amt-rub         NO-UNDO.
DEF VAR bic-corr        AS CHAR                       NO-UNDO.
DEF VAR acct-corr       AS CHAR                       NO-UNDO.
DEF VAR corr-acct-corr  AS CHAR                       NO-UNDO.
DEF VAR err-msg         AS CHARACTER                  NO-UNDO.
DEF VAR ferr            AS LOGICAL                    NO-UNDO.
DEF VAR ok              AS LOGICAL                    NO-UNDO.
/* для временного хранения др, настроек и тд */
DEF VAR TmpStr          AS CHAR                       NO-UNDO.
DEF VAR TmpStr1         AS CHAR                       NO-UNDO.
DEF VAR mDigital        AS CHAR                       NO-UNDO.
/* вычисленный контрольный разряд ИНН */
DEF VAR mchCorrectInnSign AS CHAR                     NO-UNDO.
DEF VAR vStrAcct        AS CHAR                       NO-UNDO.

DEF VAR mPercLen       AS INT64 INITIAL 0              NO-UNDO.
DEF VAR mPercWord       AS INT64 INITIAL 0              NO-UNDO.
DEF VAR mNalSTat        AS CHAR                       NO-UNDO.
DEF VAR mAcctMask       AS CHAR                       NO-UNDO.
DEF VAR mField101       AS CHAR                       NO-UNDO.
DEF VAR vKauId          AS CHAR                       NO-UNDO.
DEF VAR mControlNalog   AS LOGICAL                    NO-UNDO.
DEF VAR mBalNalog       AS CHAR                       NO-UNDO.
DEF VAR mSmevMask       AS CHAR                       NO-UNDO.
DEF VAR mOsn106tp       AS CHAR                       NO-UNDO.
DEF VAR vCheckKpp       AS LOGICAL                    NO-UNDO. 
DEF VAR vAcctInn12      AS CHAR                       NO-UNDO.
DEF VAR vINNDec         AS DEC                        NO-UNDO.
DEF VAR vAmtOvr         AS DEC                        NO-UNDO.
DEF VAR vUIN            AS CHAR INITIAL "УИН"         NO-UNDO.
DEF VAR vSLH            AS CHAR INITIAL "///"         NO-UNDO.
DEF VAR vLenAcc         AS INT                        NO-UNDO. /*длина счета*/
DEF VAR mRes            AS LOGICAL                    NO-UNDO INITIAL NO.
DEF VAR mMaskUin        AS CHAR                       NO-UNDO.
DEF VAR mUin            AS CHAR                       NO-UNDO.
DEF VAR mDateCh110      AS DATE                       NO-UNDO.     

DEFINE VARIABLE vAllow       AS LOGICAL                    NO-UNDO.  /*Для проверки корреспонденции счетов */

{errxcode.def}

DEFINE BUFFER cacct     FOR acct.
DEFINE BUFFER bAacct    FOR acct.
DEFINE BUFFER dacct     FOR acct.
DEFINE BUFFER xxcode    FOR code.
DEFINE BUFFER xattr-inn FOR xattr.
DEFINE BUFFER xbanks    FOR banks.

{intrface.get xclass}
{intrface.get blkob}
{intrface.get op}
{intrface.get cust}
{intrface.get bank}
{intrface.get swi}
{intrface.get terr}
{intrface.get strng}
{intrface.get re}       /* Библиотека регулярных выражений. */
{intrface.get tmess}
{intrface.get acct}
{intrface.get import}
{intrface.get crd}   /* Библиотека инструментов работы с картотекой */
{intrface.get kau}
{intrface.get ps}
{intrface.get wchck}

{imp-upd.pro}
{ch_cart.i}
{chkop117.i}         /* Контроль кодов валютных операций по инструкции 117-И. */

{checkvo.def}

{chk-uin.pro}        /*Контроль УИН*/

DEFINE VAR  mOur-Acct   AS CHAR    NO-UNDO.
DEFINE VAR  mOur-Inn    AS CHAR    NO-UNDO.
DEFINE VAR  mContr-MBR  AS CHAR    NO-UNDO.


IF FGetSetting("Форма664","ПроверкаВОвОЭД","") = "Да" AND no-buf THEN DO:      /* экспорт */
                                /* Контроль кодов ВО на основе формул по ф664 */
   FIND FIRST w-op NO-LOCK NO-ERROR.
   {checkvo.664 &InOpDate="w-op.op-date" &OpKind="w-op.op-kind" &pre="w-"}
END.

DEFINE STREAM sImpUpd.
OUTPUT STREAM sImpUpd TO VALUE("/home2/bis/quit41d/imp-exp/0000/bss/in/tmp/imp-upd.txt") APPEND UNBUFFERED.

/*============================================================================*/
ASSIGN
   str-bank    = ""
   bank-name   = dept.name-bank
.

vStrAcct       =  FGetSetting("МЦИ","ЗапрСчета","30101*").
mControlNalog  =  FGetSetting("ГНИ","ВводНР", "") EQ "Да".
mBalNalog      =  FGetSetting("ГНИ","bal-nalog",?).
mSmevMask        = FGetSetting("ГНИ","bal-smev",?).
mOsn106tp        = FGetSetting("ГНИ","Осн106ТП",?).
vAcctInn12     =  FGetSetting("СчИНН12",?,"40802*").
mMaskUin       =  FGetSetting("ПлатДок", "СчетУИН", "").
mDateCh110     =  DATE(FGetSetting("SWIFT", "DateCh110", "01/01/2015")).

FOR EACH w-op-entry,
   FIRST w-op WHERE w-op.op EQ w-op-entry.op:

   ASSIGN
      mOutOpKind = GetEntries(3,w-op.op-kind,",","")
      mPercLen  = INT64(GetXAttrValue("op-kind",
                                     ENTRY(1,w-op.op-kind),
                                     "length-str") )
      mPercWord  = INT64(GetXAttrValue("op-kind",
                                     ENTRY(1,w-op.op-kind),
                                     "PercOfCoinc"))
      mNalSTat   = GetXAttrValue("op-kind",
                                 ENTRY(1,w-op.op-kind),
                                 "НалДокСтатСост")
      mAcctMask  = GetXAttrValue("op-kind",
                                 ENTRY(1,w-op.op-kind),
                                 "НалДокМаскаСчета")
      mField101  = GetWOpSigns("ПокСт") 
   NO-ERROR.

   {delitem.i w-op.op-kind mOutOpKind}

   {errxcode.i &IMP-UPD=YES &op-kind="w-op.op-kind"}

    ASSIGN
      err-class           = err-class + ":"
      dict-uer            = GetXAttrValue("op-kind",w-op.op-kind,"dict-uer")
      reg-mask            = FGetSetting("МЦИ","reg-mask",reg-mask)
      str-bank            = reg-mask
      mDigital            = GetDigital(w-op.doc-type,ENTRY(1,w-op.op-kind))
   .

      w-op.bank-code-send =
           STRING(DEC(w-op.bank-code-send),"999999999") NO-ERROR.
      w-op.bank-code-rec  =
           STRING(DEC(w-op.bank-code-rec),"999999999")  NO-ERROR.

   RUN check-banktype.
   RUN Reciever.
   RUN chek-mci.
   RUN chek-acct-rec.
   RUN Sender.
   RUN CheckAcct.
   RUN CheckTail.
   RUN CheckTerror (no-buf).           /* Террористы                */
   RUN Check110 IN THIS-PROCEDURE.   

   IF ChkStopListImpExp(w-op.op) THEN  /* Стоп-листы                */
      w-op.op-error = AddError(w-op.op-error,err-class,"sl001").

   RUN CheckWOp117I.

/*     Проверка корреспонденции счетов                                       */                                          
   RUN CheckAcctCorr( FStrNVL(w-op-entry.acct-db,"") ,
                      FStrNVL(w-op-entry.acct-cr,"") ,
                      FStrNVL(w-op.acct-send,"") ,
                      FStrNVL(w-op.acct-rec,"") ,
                      w-op.op-date,
                      OUTPUT vAllow ) .
   IF NOT vAllow THEN
   DO:
      w-op.op-error = AddError(w-op.op-error,err-class,"m171").
      IF NOT no-buf THEN
      RUN PrepareAcctUnk (INPUT  SUBSTRING(w-op-entry.acct-cr,1,5),
                             INPUT  "Пост",
                             INPUT  w-op-entry.currency ,
                             OUTPUT w-op-entry.acct-cr).
   END.

   IF FGetSetting("Форма664","ПроверкаВОвОЭД","") = "Да" AND no-buf THEN 
      RUN CheckVO664.
   IF NOT no-buf AND
      CAN-DO(str-error-code,"62") AND
      GetWOpSigns("УслОпл") EQ "" AND
      CAN-DO("02,021,022",w-op.doc-type) THEN
      w-op.op-error = AddError (w-op.op-error,err-class, "62").
   IF NOT no-buf AND
      CAN-DO(str-error-code,"62t") AND
      GetWOpSigns("УслОпл") NE "" AND
      NOT CAN-DO(FGetSetting("УФЭБС","УслОпл",""),GetWOpSigns("УслОпл")) THEN
      w-op.op-error = AddError (w-op.op-error,err-class, "62t").
   IF NOT no-buf AND /*импорт*/
      CAN-DO(str-error-code,"s101") THEN
   DO:
      IF CAN-DO(FGetSetting("ГНИ","bal-smev",?),w-op.acct-rec) THEN
      DO:
         vLenAcc = INDEX(w-op.details,vSLH) - INDEX(w-op.details,vUIN) 
                                            - LENGTH(vUIN).
         IF w-op.op-date GE DATE("01/01/2014") AND
            w-op.op-date LT DATE("31/03/2014") THEN
         DO:
            /**218654: Начиная с 31/03/2014 УИН в назначении платежа не
            проверять. Теперь УИН должен стоять в начале назначения платежа
            только для документов 01/01/2014=<op.op-date<31/03/2014**/
            IF INDEX(w-op.details,vUIN) EQ 0 OR 
               INDEX(w-op.details,vSLH) EQ 0 OR
               (vLenAcc NE 1 AND vLenAcc NE 20) OR
               (vLenAcc EQ 1 AND 
                  SUBSTRING(w-op.details,INDEX(w-op.details,vUIN) 
                          + LENGTH(vUIN),1) NE "0")
            THEN
            DO:
               w-op.op-error = AddError (w-op.op-error,err-class, "s101").
            END.
         END.
         ELSE
         DO:
         /* старый механизм - для документов до 01/01/2014 */
         IF w-op.op-date LE DATE("01/01/2014") THEN
            IF INDEX(w-op.details,"///УИН") EQ 0 OR 
               ((LENGTH(ENTRY(1,SUBSTRING(w-op.details,INDEX(w-op.details,"///УИН") + 6),";")) - 1) NE 1 AND 
                (LENGTH(ENTRY(1,SUBSTRING(w-op.details,INDEX(w-op.details,"///УИН") + 6),";")) - 1) NE 20) OR 
               ((LENGTH(ENTRY(1,SUBSTRING(w-op.details,INDEX(w-op.details,"///УИН") + 6),";")) - 1) EQ 1 AND
                        ENTRY(1,SUBSTRING(w-op.details,INDEX(w-op.details,"///УИН") + 6),";") NE "0")
            THEN w-op.op-error = AddError (w-op.op-error,err-class, "s101").
         END.
      END.
   END.
   IF NOT no-buf AND /*импорт*/
      CAN-DO(str-error-code,"s103") AND 
      {assigned w-op.acct-rec} THEN
   DO:
      IF CAN-DO(mMaskUin,SUBSTR(w-op.acct-rec,1,5)) THEN 
      DO:
         mUin = GetWOpSigns("УИН").
         IF {assigned mUin} THEN 
         DO:
            RUN ChkKey IN THIS-PROCEDURE( DelFilFromAcct(w-op.acct-rec), mUin, OUTPUT mRes).
            IF NOT mRes THEN 
            DO:
               w-op.op-error = AddError(w-op.op-error,err-class,'s103').
            END.
         END.
         ELSE w-op.op-error = AddError(w-op.op-error,err-class,'s103').
      END.
   END.
   IF str-error-code MATCHES "*knf*" THEN DO:
      RUN CheckKNF IN h_base (w-op-entry.acct-db,
                              w-op-entry.acct-cr,
                              w-op.details,
                              w-op.acct-send,
                              w-op.acct-rec, w-op.op-kind).
      buf = RETURN-VALUE.
      IF buf NE "" THEN DO:            /* Вывод сообщения по инструкции (93-И)*/
         buf = {div2ch.i &buf  = buf
                         &div1 = '"("' &div2='")"'
                         &add1 = '"knf-"' }.
         {additem.i "w-op.op-error" "err-class + buf"}
      END.
   END.
                                                 /* Проверки в связи с 1256-У */
/*    0015363 
      ЕСЛИ Счет получателя платежа удовлетворяет одной из масок из 
           настроечного параметра <bal-nalog>. В этом случае это налоговый 
           платеж...
*/     


   IF GetWOpSigns("ПокСт") NE ""
      OR (CAN-DO(mBalNalog, (      IF flag-go EQ 0 THEN w-op-entry.acct-cr
                              ELSE IF flag-go EQ 2 THEN w-op.acct-send
                              ELSE w-op.acct-rec))
          AND mControlNalog)
   THEN DO:
      vCheckKpp = Yes.
      FOR FIRST acct WHERE acct.acct EQ w-op.acct-send NO-LOCK:
         vCheckKpp = GetXattrValue((IF acct.cust-cat EQ "Ю" THEN "cust-corp"
                                                            ELSE "person"),
                                   STRING(acct.cust-id),
                                   "Предпр") EQ "".
         LEAVE.
      END.
      RUN CheckSigns("CheckNalog").

   END.
   RUN CheckPaymOrder(mDigital).       /* Контроль пл. ордера       */
   RUN CheckNDS(w-op.details).         /* Контроль НДС в назначении платежа  */

END.                                   /* EACH w-op-entry                     */
OUTPUT STREAM sImpUpd CLOSE.
{intrface.del}          /* Выгрузка инструментария. */ 
RETURN.
/****************************** ПЛАТЕЛЬЩИК ************************************/
PROCEDURE Sender:

   DEF VAR vInnBase     AS CHAR                       NO-UNDO.
   DEF VAR vNameBase    AS CHAR                       NO-UNDO.
   DEF VAR vNameE       AS CHAR                       NO-UNDO.
   DEF VAR vNameCorp    AS CHAR                       NO-UNDO.
   DEF VAR vOK          AS LOGICAL                    NO-UNDO.
   DEF VAR vError       AS CHAR                       NO-UNDO.

   DEFINE VARIABLE vOldNameSend    AS CHARACTER       NO-UNDO.
   DEFINE VARIABLE vPersMascAcct   AS CHAR            NO-UNDO.
   DEFINE VARIABLE vAmt1626        AS DECIMAL         NO-UNDO.
   DEFINE VARIABLE vNameShort      AS CHARACTER       NO-UNDO.
   DEFINE VARIABLE vFLCP           AS CHARACTER       NO-UNDO.
   DEFINE VARIABLE vNameLen        AS INTEGER         NO-UNDO.

   vPersMascAcct    = FGetSetting("ФизМаск",?,"").
   vAmt1626         = DEC(FGetSetting("И113","Сумма1626","15,000.00")).

   RUN GetBanks IN h_swi (w-op.bank-code-send, "МФО-9",
                          BUFFER banks-code, BUFFER banks).

   IF NOT AVAIL banks AND
      LOOKUP(err-class + 'm161',w-op.op-error) EQ 0 THEN  /* "Нет банка получателя"    */
      w-op.op-error = AddError(w-op.op-error,err-class,"m160").
   ELSE IF AVAIL banks THEN DO:
      IF (flag-go EQ 0 OR flag-go EQ 1)     AND
         w-op.bank-code-send NE  bank-mfo-9 AND
         dec(w-op.bank-corr-acct-send) NE 0 THEN /* Получатель - не наш банк  */
         w-op.op-error = AddError(w-op.op-error,err-class,"m170").

                                  /* "Неверный коррсчет банка клиента "       */
      IF NOT banks.flag-rkc                                                   AND
         (NOT GetCorrRKC(BUFFER banks, BUFFER banks-corr)                     OR
          decimal(banks-corr.corr-acct) NE decimal(w-op.bank-corr-acct-send)) THEN
         IF NOT CAN-DO(str-bank, w-op.bank-code-send) THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"m180").

      IF TRIM(w-op.acct-send) NE ""                     AND
         w-op.acct-send       NE "0"                    AND
         w-op.acct-send       NE "00000000000000000000" THEN DO:

         RUN key-tst.p (w-op.acct-send,w-op.bank-code-send, OUTPUT key).

                                  /* "Ошибка ключа расч. счета клиента"       */
         IF key NE ?                               AND
            key NE INT64(substr(w-op.acct-send,9,1)) THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"m190").
      END.
   END.                                          /* ELSE AVAIL banks          */

   IF flag-go EQ 0 THEN
      {find-act.i &acct = w-op-entry.acct-db}
   ELSE
      {find-act.i &acct = w-op.acct-send}
      
      
                                       /*  "Отсутствует ИНН  отправителя "    */
   IF flag-go NE 0 AND  AVAIL acct THEN DO:
      RUN CheckInnEx(INPUT  REPLACE(w-op.inn-send,"0",""),
                     INPUT  acct.cust-cat,
                     INPUT  acct.cust-id ,
                     INPUT  (GetWOpSigns("ПокСт") NE ""),
                     OUTPUT vError).                  /* m240R m240Nnal m240N */
      IF {assigned vError} THEN
         w-op.op-error = AddError(w-op.op-error,err-class,vError).
   END.
   IF AVAIL acct THEN DO:
      IF NOT {assigned w-op.inn-send}                AND
         CAN-DO(vPersMascAcct,w-op.acct-send)        AND
         w-op-entry.amt-rub GE vAmt1626              AND
         NOT (NUM-ENTRIES(w-op.name-send,"/") EQ 5   AND
         {assigned ENTRY(1,w-op.name-send,'"/"')}    AND
         {assigned ENTRY(3,w-op.name-send,'"/"')})
      THEN DO:
         w-op.op-error  = AddError(w-op.op-error,err-class,"m239").
      END.
   END.

   IF INDEX(PROGRAM-NAME(3), "imp-scan") GT 0                  AND 
      NOT fValidInnSignature(w-op.inn-send, mchCorrectInnSign) THEN 
   DO:                       /* неверный контрольный разряд в ИНН отправителя */
      w-op.op-error = AddError(w-op.op-error,err-class, "m241").
   END.

   IF AVAIL acct                 AND
      flag-go NE 2               AND
      {assigned w-op.name-send}  THEN
   DO:
      IF NOT (CAN-DO(mNalSTat,mField101 )  AND 
              CAN-DO(mAcctMask,acct.acct)) THEN 
      DO:
         RUN Get_Inn_Name IN THIS-PROCEDURE (BUFFER acct,
                                    OUTPUT vInnBase,
                                    OUTPUT vNameBase).
         PUT STREAM sImpUpd UNFORMATTED 
            STRING(NOW,"99/99/9999 HH:MM:SS") ";"
            w-op.inn-send ";"
            vInnBase ";"
            vNameBase ";"
            shFilial
         SKIP.                                    
         RUN GetCustShortName IN h_cust (acct.cust-cat,
                                         acct.cust-id,
                                         OUTPUT vNameShort).
                                                 /* Неверный ИНН              */
         IF LOOKUP("m790",str-error-code)                      NE 0 AND
            {assigned w-op.inn-send}                                AND
            left-trim(w-op.inn-send,"0") NE left-trim(vINNBase,"0") THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"m790").
      END.

      IF {assigned w-op.name-send}             AND
         {assigned vNameBase}                 AND
         vNameBase                    NE "-"  AND
        (LOOKUP("m792",  str-error-code) NE 0  OR 
         LOOKUP("m792k", str-error-code) NE 0) AND 
         NOT Chk2863U (w-op.details, w-op.name-send)
      THEN DO:
         ASSIGN
            vOldNameSend   = w-op.name-send
            vNameBase      = ReplaceBad(REPLACE (REPLACE (vNameBase, "ё", "е"), "Ё", "Е"))
            vNameShort     = ReplaceBad(REPLACE (REPLACE (vNameShort, "ё", "е"), "Ё", "Е"))
            vNameE         = ReplaceBad(REPLACE (REPLACE (w-op.name-send, "ё", "е"), "Ё", "Е"))
         .
         /* Выделение наименования плательщика из строки, в формате ФИО //Адрес//> 
         или <ФИО (правовой статус или вид деятельности)//Адрес// */
         IF GetSysConf("telex-bss-neo1") NE "" THEN 
         DO:
            vFLCP = FGetSetting("СтандТр","СтатусФЛЧП","").
            vNameLen = INDEX(vNameE,"//").
            IF vNameLen NE 0 AND 
               INDEX(vNameE,"//", vNameLen + 2) NE 0 THEN 
            DO:
               vNameE = TRIM(SUBSTRING(vNameE,1,vNameLen - 1)).
               IF CAN-DO(vFLCP,ENTRY(1,vNameE," ")) OR 
                  CAN-DO(vFLCP,
                         "(" + ENTRY(1,vNameE," ") + ")") THEN 
               DO:
                  ENTRY(1,vNameE," ") = "".
                  vNameE = TRIM(vNameE).
               END.
               ELSE IF CAN-DO(vFLCP,ENTRY(NUM-ENTRIES(vNameE," "),vNameE," ")) OR
                       CAN-DO(vFLCP,"(" + ENTRY(NUM-ENTRIES(vNameE," "),vNameE," ") + ")") THEN 
               DO:
                  ENTRY(NUM-ENTRIES(vNameE," "),vNameE," ") = "".
                  vNameE = TRIM(vNameE).
               END.
            END.
         END.
         vOK = CompareNameFast (INPUT  vNameBase,
                                INPUT  vNameE,
                                INPUT  mPercLen,
                                INPUT  mPercWord).
         IF vOK NE YES THEN
            vOK = CompareNameSlow (INPUT  vNameBase,
                                   INPUT  vNameE,
                                   INPUT  mPercLen,
                                   INPUT  mPercWord).
         IF vOK NE YES THEN DO:
            IF LOOKUP("m792",  str-error-code) NE 0 THEN
               w-op.op-error = AddError(w-op.op-error,err-class,"m792").

            IF LOOKUP("m792k",  str-error-code) NE 0 THEN DO:

               vOK = CompareNameFast (INPUT  vNameShort,
                                      INPUT  vNameE,
                                      INPUT  mPercLen,
                                      INPUT  mPercWord).
               IF vOK NE YES THEN
                  vOK = CompareNameSlow (INPUT  vNameShort,
                                         INPUT  vNameE,
                                         INPUT  mPercLen,
                                         INPUT  mPercWord).
               IF vOK NE YES THEN 
                  w-op.op-error = AddError(w-op.op-error,err-class,"m792k").

            END.
            IF LOOKUP("m792c",  str-error-code) NE 0 THEN DO:
               {check_name.i &cust-id = acct.cust-id}
               IF vOK NE YES THEN
                  w-op.op-error = AddError(w-op.op-error,err-class,"m792c").
               END.
         END.
         w-op.name-send = vOldNameSend.
      END.

      IF {assigned w-op.name-send} THEN
      DO:
         IF LOOKUP ("m249",str-error-code) NE 0    AND
            LENGTH (w-op.name-send) GT 160         THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"m249").
         IF LOOKUP ("m247",str-error-code) NE 0    AND
            LENGTH (w-op.name-send) GT 300         THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"m247").
      END.

   END.

END PROCEDURE.
/***************************************** ПОЛУЧАТЕЛЬ *************************/
PROCEDURE Reciever:
   DEFINE VAR vTemplate AS CHAR  NO-UNDO.
   DEFINE VAR vError    AS CHAR  NO-UNDO.
   DEFINE VAR vOpClass  AS CHAR  NO-UNDO.

   DEFINE VAR vOk       AS LOGICAL  NO-UNDO.

   DEFINE VAR vFlagRKC       AS LOGICAL  NO-UNDO.

   RUN GetBanks IN h_swi (w-op.bank-code-rec, "МФО-9",
                          BUFFER banks-code, BUFFER banks).

                                  /* "Нет банка получателя"                   */
   IF (NOT AVAIL banks                                         OR
       w-op.bank-code-rec                      EQ "000000000") AND
      LOOKUP(err-class + 'm163',w-op.op-error) EQ 0            THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m10').
   ELSE IF AVAILABLE(banks) THEN DO:                  /* банк получателя есть */

      vFlagRKC = banks.flag-rkc NO-ERROR.
      IF((w-op-entry.currency EQ "") AND
         (NOT no-buf               ))                   /* только для импрота */
      THEN DO:
         /*----------------------------------------*/
         /* проверка наименования банка получателя */
         /*----------------------------------------*/
         FIND FIRST     w-op-bank WHERE 
                        w-op-bank.op-bank-type = "rec"   NO-ERROR.
         IF NOT AVAIL   w-op-bank THEN
             FIND FIRST w-op-bank WHERE 
                        w-op-bank.op-bank-type = ""      NO-ERROR.

         IF (LOOKUP("m192",str-error-code) NE 0) AND 
             AVAIL w-op-bank                     THEN
         DO:
            vOK = GompareBankNames(INPUT  banks.bank-id,
                                   INPUT  ENTRY(1,w-op-bank.bank-name,CHR(1)),
                                   INPUT  mPercLen,
                                   INPUT  mPercWord).
            IF NOT vOK THEN
               /* неверное наименование банка получателя             */
               w-op.op-error = AddError(w-op.op-error,err-class,"m192").
         END.  
      END.

      vError = CheckBankReal(BUFFER banks).           /* ограничения расчетов */

      IF {assigned vError} THEN
         w-op.op-error = AddError(w-op.op-error,err-class,vError).
      IF     no-buf                           /* только экспорт */
         AND CAN-DO(str-error-code,"m950")
         AND banks.flag-rkc  
         AND banks.bank-type BEGINS "ПУ" 
         AND CAN-DO(vStrAcct,w-op.acct-rec) THEN 
m950:
         FOR EACH banks-corr WHERE 
                  banks-corr.corr-acct EQ w-op.acct-rec NO-LOCK,
             FIRST xbanks WHERE xbanks.bank-id EQ banks-corr.bank-id
                            AND xbanks.flag-rkc   NO-LOCK:
                w-op.op-error = AddError(w-op.op-error,err-class,'m950').
                LEAVE m950.
      END.
                        /* У банка-получателя запрещены транзитные документы  */
      IF GetXAttrValue("banks",string(banks.bank-id),"is-transit") EQ "0" AND
         w-op.acct-rec BEGINS "30301"                                     THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m19').

      RUN CheckM18.

      IF CAN-DO(reg-mask,w-op.bank-code-rec)
         AND SUBSTRING(w-op.bank-code-rec,7) EQ "002"
         AND NOT CAN-DO(FGetSetting("МЦИ",
                                    "РазрешПУ",
                                    "044501002,044536002,044537002"),
                        w-op.bank-code-rec)
         AND CAN-DO(str-error-code,"m945")
      THEN DO:
         RUN GetBanks IN h_swi (INPUT  w-op.bank-code-rec,
                                INPUT  "МФО-9",
                                BUFFER banks-code,
                                BUFFER banks).
         IF AVAILABLE(banks)                                AND
            banks.bank-type                     BEGINS "ПУ" AND
            GetXAttrValue("banks",
                          string(banks.bank-id),
                          "Договор_с_МЦИ")       EQ     "1" THEN.
         ELSE DO:                      /* Недопустимое учреждение Банка       */
            w-op.op-error = AddError(w-op.op-error,err-class,'m945').
            RUN CheckM18.
         END.
      END.
                                       /* "Получатель - не наш банк"          */
      IF flag-go EQ 2 THEN DO:

         IF w-op.bank-code-rec NE  bank-mfo-9 AND
            dec(w-op.bank-corr-acct-rec) NE 0 THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m20').

                                       /* Неверный коррсчет банка клиента     */
         RUN GetBanks IN h_swi (w-op.bank-code-rec,
                                "МФО-9",
                                BUFFER banks-code,
                                BUFFER banks).
   
         IF NOT banks.flag-rkc                                                  AND
            (NOT GetCorrRKC(BUFFER banks, BUFFER banks-corr)                    OR
             decimal(banks-corr.corr-acct) NE decimal(w-op.bank-corr-acct-rec)) THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m30').
   
         IF banks.flag-rkc
            AND {assigned w-op.bank-corr-acct-rec} THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m30').

      END.
      IF flag-go NE 2                            AND
         TRIM(w-op.acct-rec) NE ""               AND
         w-op.acct-rec NE "0"                    AND
         w-op.acct-rec NE "00000000000000000000" THEN DO:


         j =  INT64(substr(w-op.acct-rec,9,1)) NO-ERROR.
                                       /* "Ошибка длины расч. счета клиента"  */
         IF length(DelFilFromAcct(TRIM(w-op.acct-rec))) NE 20 THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m35').
         IF j NE ? THEN DO:
                                       /* "Ошибка ключа расч. счета клиента"  */
            RUN key-tst.p (w-op.acct-rec,w-op.bank-code-rec, OUTPUT key) NO-ERROR.
            IF key NE ? AND key NE j THEN
               w-op.op-error = AddError(w-op.op-error,err-class,'m40').

         END.
                                       /* "Счет получателя закрыт"             */
        RUN chbenacc.p (w-op.op-date,w-op.acct-rec + "," +
                        w-op.bank-corr-acct-rec, w-op.bank-code-rec,
                        ENTRY(1,w-op.op-kind),flag-go,YES,
                        OUTPUT TmpStr, OUTPUT j,OUTPUT TmpStr1).
        DO i = 1 TO NUM-ENTRIES(TmpStr):
           IF CAN-DO(str-error-code,ENTRY(i,TmpStr)) THEN DO:
              w-op.op-error = AddError(w-op.op-error,err-class,ENTRY(i,TmpStr)).
           END.
        END.
      END.
                                       /* Банк получателя не в нашем регионе  */
      IF CAN-DO("02,06,16",mDigital)                                AND
         SUBSTRING(bank-mfo-9,1,4) NE SUBSTRING(w-op.bank-code-rec,1,4) THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m162').

   END.                                /* ELSE DO  IF AVAIL banks             */

   IF {assigned w-op.acct-rec} THEN DO:

      FIND FIRST bal-acct WHERE bal-acct.bal-acct EQ INT64(SUBSTRING(w-op.acct-rec,1,5))
                            AND bal-acct.acct-cat EQ "b" NO-LOCK NO-ERROR.

                                          /*  "Бал Счет не открыт"               */
      IF NOT AVAIL bal-acct                     AND
         INT64(SUBSTRING(w-op.acct-rec,1,5)) NE 0 THEN DO:
         IF     CAN-FIND(FIRST code WHERE
                               code.class = ""
                           AND code.code  = "bs20.nsi")                   AND
            NOT CAN-FIND(FIRST code WHERE
                               code.class = "bs20.nsi"
                           AND code.code  = SUBSTRING(w-op.acct-rec,1,5)) THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m51').
      END.
      ELSE  
      IF CAN-FIND(FIRST code WHERE
                  code.class = "bs20.nsi"
                  AND code.code  = SUBSTRING(w-op.acct-rec,1,5)
                  AND CAN-DO("1,2,",CODE.val )) THEN
      DO:
         IF vFlagRkc THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m53').
         ELSE w-op.op-error = AddError(w-op.op-error,err-class,'m52').
      END.      
                                          /*  "Экспорт только в рублях"          */
      IF flag-go EQ 1 AND SUBSTRING(w-op.acct-rec,6,3) NE "{&in-NC-Code}"
                      AND SUBSTRING(w-op.acct-rec,6,3) NE "000"
                      AND SUBSTRING(w-op.acct-rec,6,3) NE "   "  THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m45').
   END.


   IF w-op.acct-cat EQ "d" THEN DO:    /*  "Сумма не может быть равно 0"      */
      IF w-op-entry.qty EQ 0 THEN DO:     
         w-op.op-error = AddError(w-op.op-error,err-class,'m111').
      END.
   END.
   ELSE DO:                            
      IF w-op-entry.amt-rub   EQ 0     AND
         w-op-entry.amt-cur   EQ 0     THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m111').
   END.
                                       /*  "Отсутствует ИНН  получателя "     */
   IF (TRIM(w-op.inn-rec)     EQ "" OR
       w-op.inn-rec           EQ ?) AND
       flag-go NE 0   THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m245').

   IF INDEX(PROGRAM-NAME(3), "imp-scan") GT 0 AND NOT fValidInnSignature(w-op.inn-rec, mchCorrectInnSign) THEN DO:
   /* неверный контрольный разряд в ИНН получателя */
      w-op.op-error = AddError(w-op.op-error,err-class, "m246").
   END.

   IF NOT (TRIM(w-op.inn-rec) = "" OR w-op.inn-rec = ?) THEN DO:
      IF NUM-ENTRIES(w-op.op-kind)  > 1 AND
         GetEntries(2,w-op.op-kind,",","0") NE "0" THEN
         FIND FIRST op-template WHERE op-template.op-kind  EQ ENTRY(1,w-op.op-kind)
                                  AND op-template.op-templ EQ INT64(ENTRY(2,w-op.op-kind))
                                      NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST op-template WHERE op-template.op-kind EQ ENTRY(1,w-op.op-kind)
                                      NO-LOCK NO-ERROR.

      IF AVAILABLE(op-template) THEN DO:
         vTemplate = GetXAttrValue("op-template",
                                   STRING(op-template.op-kind) + "," +
                                   STRING(op-template.op-template),
                                   "op-kind-child").
         IF vTemplate NE "" THEN
            FIND FIRST op-template WHERE op-template.op-kind EQ vTemplate
                       NO-LOCK NO-ERROR.
      END.

      vOpClass = "opb".
      IF AVAILABLE(op-template) THEN
         RUN op-class IN h_xclass (INPUT-OUTPUT vOpClass, BUFFER op-template).

      RUN GetXAttr IN h_xclass  (vOpClass,
                                 "inn",
                                 BUFFER xattr-inn).
      err-msg = ?.
      RUN CheckField IN h_xclass (BUFFER       xattr-inn,
                                               RIGHT-TRIM(w-op.inn-rec),
                                  INPUT-OUTPUT err-msg,
                                  OUTPUT       ferr).

      IF ferr THEN                     /*  "Неверный формат ИНН получателя "  */
         w-op.op-error = AddError(w-op.op-error,err-class,'m244').
   END.

   IF (LOOKUP("m244f",str-error-code) NE 0)
      AND CAN-DO(vAcctInn12,w-op.acct-rec) THEN DO:
      ASSIGN 
         vINNDec = DEC(w-op.inn-rec) NO-ERROR.
      IF ERROR-STATUS:ERROR OR
         LENGTH(TRIM(w-op.inn-rec)) NE 12 THEN    
         w-op.op-error = AddError(w-op.op-error,err-class,'m244f').
   END.

   IF flag-go LE 1 THEN DO:
      RUN check-asc(w-op.details,  "m46").
      RUN check-asc(w-op.name-send,"m67").
      RUN check-asc(w-op.name-rec, "m68").
   END.

   IF {assigned w-op.name-rec} THEN
   DO:
      IF LOOKUP("m250",str-error-code) NE 0   AND
         LENGTH(w-op.name-rec) GT 160         THEN
         w-op.op-error = AddError(w-op.op-error,err-class,"m250").
      IF LOOKUP("m248",str-error-code) NE 0   AND
         LENGTH(w-op.name-rec) GT 300         THEN
         w-op.op-error = AddError(w-op.op-error,err-class,"m248").
   END.

END PROCEDURE.

/************************************ КОНТРОЛЬ СЧЕТОВ *************************/
PROCEDURE CheckAcct:

   DEF VAR from-filial AS LOGICAL INIT NO NO-UNDO.
   DEF VAR mbl-pos     AS DECIMAL INIT 0  NO-UNDO.
   DEF VAR vAcctStat       AS CHARACTER NO-UNDO.
   DEF VAR vAcctStatOrdPay AS CHARACTER NO-UNDO.
   DEF VAR vAcctStatList   AS CHARACTER NO-UNDO.
   DEF VAR vStatTmp        AS CHARACTER NO-UNDO.
   DEF VAR vI              AS INT       NO-UNDO.

   DEFINE VARIABLE vSaldoErr AS LOGICAL     NO-UNDO.

                                       /*"Одинаковые счета Дб и Кр"           */
   IF w-op-entry.acct-db EQ w-op-entry.acct-cr THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m95').

   IF flag-go EQ 0 OR
      flag-go EQ 1 THEN DO:

      IF w-op.acct-send EQ w-op.acct-rec THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m215').


      IF flag-go EQ 0 OR
         NOT {assigned w-op.acct-send} THEN
         {find-act.i &acct = w-op-entry.acct-db}
      ELSE
         {find-act.i &acct = w-op.acct-send}

      IF NOT AVAIL acct THEN DO:
         IF CAN-DO(str-error-code,"m200") THEN DO:

            {getbracc.i &acct="w-op.acct-send"}

            from-filial = AVAIL branch.

            IF NOT from-filial THEN DO:   /*  "Счет не открыт"                   */
               {additem.i "w-op.op-error" "err-class + 'm200'"}
            END.
         END.
      END.
      ELSE DO:
         vAcctStatList = BlockAcct(acct.acct + ',' + acct.currency,
                               IF w-op.op-date EQ TODAY THEN DATETIME(TODAY,MTIME) 
                               ELSE DATETIME(w-op.op-date + 1) - 1).
         DO vI = 1 TO NUM-ENTRIES (vAcctStatList) :
            vStatTmp = GetCodeMisc("acct-status",ENTRY(vI,vAcctStatList),1).
            IF {assigned vStatTmp} AND NOT CAN-DO(vAcctStat,vStatTmp) THEN 
               {additem.i "vAcctStat" "vStatTmp"}.
            vStatTmp = "".
         END.

         IF vAcctStat MATCHES "Блок*" THEN DO:
                                       /*"Счет блокирован"                    */
            IF    LOOKUP("Блок",vAcctStat)   GT 0 
               OR LOOKUP("БлокДб",vAcctStat) GT 0 THEN DO:

               IF GetCode(ENTRY(1,err-class,":"),"m210") BEGINS "Ош" THEN
                  w-op.op-error = AddError(w-op.op-error,err-class,'m210').
               ELSE DO:
                  vAcctStatOrdPay = BlckAcctOrdPay(acct.acct + ',' + acct.currency,
                                                   IF w-op.op-date EQ TODAY THEN DATETIME(TODAY,MTIME)
                                                                            ELSE DATETIME(w-op.op-date + 1) - 1,
                                                   w-op.order-pay).
                  IF    (    LOOKUP("Блок",vAcctStatOrdPay)   GT 0
                         AND LOOKUP("Блок",vAcctStat)         GT 0
                        )
                     OR (    LOOKUP("БлокДб",vAcctStatOrdPay) GT 0
                         AND LOOKUP("БлокДб",vAcctStat)       GT 0
                        ) 
                  THEN
                     w-op.op-error = AddError(w-op.op-error,err-class,'m210b').
                  ELSE
                     w-op.op-error = AddError(w-op.op-error,err-class,'m210a').
               END.
            END.
         END.

                                       /* "Счет отправителя еще не открыт"    */
         IF acct.open-date GT w-op.op-date THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m220e').

                                       /* "Счет закрыт"                       */
         IF acct.close-date NE ? THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m220').

                                       /*  "Категория"                        */
         IF w-op-entry.acct-cat NE acct.acct-cat THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m290').

         FIND FIRST bal-acct OF acct NO-LOCK NO-ERROR.
         IF NOT AVAIL bal-acct THEN    /*  "Бал Счет не открыт"               */
            w-op.op-error = AddError(w-op.op-error,err-class,'m201').

         IF CAN-DO (str-error-code,"m275_r") THEN
         DO:
            IF w-op.op-status EQ "" AND NOT AVAIL op-template THEN
               FIND FIRST op-template WHERE op-template.op-kind  EQ ENTRY(1,w-op.op-kind)
                                        AND op-template.op-templ EQ INT64(ENTRY(2,w-op.op-kind))
               NO-LOCK NO-ERROR.

            RUN CheckProbSaldo IN h_op ("db",
                                        acct.acct,
                                        acct.currency,
                                        w-op-entry.amt-rub,
                                        w-op-entry.amt-cur,
                                        w-op.op-date, 
                                        IF w-op.op-status EQ "" AND AVAIL op-template 
                                        THEN op-template.op-status 
                                        ELSE IF w-op.op-status NE "" THEN w-op.op-status
                                        ELSE "", 
                                        OUTPUT vSaldoErr).
            IF vSaldoErr THEN
               w-op.op-error = AddError(w-op.op-error,err-class,'m275_r').
         END.

         IF CAN-DO(str-error-code,"m230")     OR
            CAN-DO(str-error-code,"m113")     OR
            CAN-DO(str-error-code,"m260")     OR
            CAN-DO(str-error-code,"m260_ovr") OR
            CAN-DO(str-error-code,"m260_nov") OR
            CAN-DO(str-error-code,"m260_ov")  OR
            CAN-DO(str-error-code,"m270")     OR
            CAN-DO(str-error-code,"m280")     THEN DO:

            RUN chek_lim_ovr(IF w-op-entry.currency > '' THEN w-op-entry.amt-cur
                             ELSE w-op-entry.amt-rub,
                             OUTPUT vAmtOvr).

            RUN cli-pos.p (acct.acct,
                           acct.currency,
                           gend-date,
                           gend-date,
                           "П").
            lim-pos = GetLimitPosition(BUFFER acct,gend-date).
            IF acct.side         EQ "П" AND
               w-op-entry.amt-rub > 0   AND
               CAN-DO(mAcctContCrd2,acct.contract) THEN
            DO:
               RUN CheckCard2 (acct.acct,acct.currency).
               IF RETURN-VALUE NE "" THEN
                  w-op.op-error = AddError(w-op.op-error,err-class,'m230').

               IF  NOT no-buf                         /* только импорт */
                   AND LOOKUP("БлокСумм",vAcctStat)   GT 0 /* могут быть заблокированные суммы */
                   AND acct.acct-cat    EQ "b" THEN   /* на балансовых счетах             */
               DO:
                  mbl-pos = GetBlockPosition (INPUT acct.acct,
                                              INPUT acct.currency,
                                              INPUT w-op.order-pay,
                                              INPUT w-op.op-date).
                  IF mbl-pos NE 0.00
                     AND (( - sh-bal - vAmtOvr - w-op-entry.amt-rub LT {abs mbl-pos} AND acct.currency EQ "")
                      OR  ( - sh-val - vAmtOvr  - w-op-entry.amt-cur LT {abs mbl-pos} AND acct.currency NE ""))
                                                /* остаток меньше блок. суммы    */
                     THEN w-op.op-error = AddError(w-op.op-error,err-class,'m113').
               END.
            END.

                                          /*проверяем есть нарушение режима счета*/
            IF acct.side EQ "П" AND w-op-entry.amt-rub > 0
             AND
              (
               (sh-bal +
                (IF no-buf THEN 0 ELSE w-op-entry.amt-rub ) - lim-pos > 0
               )
               OR
                (acct.currency > ''
                 AND sh-val  +
                 (IF no-buf THEN 0 ELSE w-op-entry.amt-cur)  - lim-pos > 0
                )
               )
             AND acct.contr-acct EQ "" THEN
            DO:
                                          /* Обнаружено нарушение режима счета   */
               IF CAN-DO(mAcctContCrd2,acct.contract) AND
                  CAN-DO(str-error-code,"m260") THEN DO:
                                          /* здесь подключаем метод , позволяющий
                                             найти соглашение и сделать необходимые
                                             проверки                            */
                  RUN chk_over.p (acct.cust-cat,
                                  acct.cust-id,
                                  'd_dsp',
                                  gend-date,
                                  recid(acct),
                                  IF w-op-entry.currency > ''
                                     THEN abs(sh-val + (IF no-buf
                                                           THEN 0
                                                            ELSE w-op-entry.amt-cur ))
                                     ELSE abs(sh-bal + (IF no-buf
                                                           THEN 0
                                                           ELSE w-op-entry.amt-rub)),
                                  NO,
                                  OUTPUT mess,
                                  OUTPUT fl-o).
                  IF fl-o <>  0 THEN DO :
                     IF fl-o = - 999 THEN DO:
                        {additem.i "w-op.op-error"  "err-class + 'm260_ovr'"}
                     END.
                     ELSE
                     IF fl-o < 0 THEN DO:
                        {additem.i "w-op.op-error"  "err-class + 'm260_nov'"}
                     END.
                     ELSE DO :
                        {additem.i "w-op.op-error"  "err-class + 'm260_ov'"}
                     END.
                  END.                 /* конец проверки расчетного счета     */
                  ELSE DO:             /* Дебетовое сальдо на пассивном счете */
                     IF CAN-DO(str-error-code,"m260")   THEN DO:
                        RUN chek_lim_ovr(IF w-op-entry.currency > '' THEN w-op-entry.amt-cur
                                         ELSE w-op-entry.amt-rub,
                                         OUTPUT vAmtOvr).
                        IF RETURN-VALUE NE "" THEN DO:
                           {additem.i "w-op.op-error" "err-class + 'm260'"}
                        END.
                     END.
                  END.
               END.                       /* IF acct.contract     EQ "Расчет"    */
               ELSE                       /* Остаток меньше чем сумма документа  */
                  w-op.op-error = AddError(w-op.op-error,err-class,'m270').
            END.                       /* IF acct.side EQ "П"                 */
            ELSE                       /* Кредитовое сальдо на активном счете */
               IF acct.side EQ "А" AND w-op-entry.amt-rub                LT  0   AND
                  ((sh-bal + (IF no-buf THEN 0 ELSE w-op-entry.amt-rub ) LT  0)  OR
                   (acct.currency NE "" AND
                   (sh-val + (IF no-buf THEN 0 ELSE w-op-entry.amt-cur ) LT  0))) AND
                  acct.contr-acct                                        EQ ""   THEN
                  w-op.op-error = AddError(w-op.op-error,err-class,'m280').

         END.                          /* IF CAN-DO(str-error-code,"m230") */
      END.                             /* AVAIL acct                          */
   END.                                /* flag-go = 0 OR flag-go= 1           */
END PROCEDURE.

/*************************** КОНТРОЛЬ ДАТЫ И ПОВТОРНОГО ПРИЕМА ****************/
PROCEDURE CheckTail:
   DEF VAR vShiftDate  AS CHAR  NO-UNDO.
   DEF VAR vShiftDate1 AS INT64 NO-UNDO.
   DEF VAR vShiftDate2 AS INT64 NO-UNDO.

                                       /* "Нет такого типа документа !"       */
   IF NOT {assigned mDigital} THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m112').

                                       /* Документ с просроченной датой !     */
   IF (w-op.op-date - w-op.doc-date) > 10  THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m100').

                                      /* В документе отсутствует дата  !      */
   IF w-op.doc-date EQ ? THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m103').
   ELSE                               /* "Документ с просроченной датой !"    */
      IF w-op.due-date < w-op.op-date  AND
         CAN-DO(str-error-code,"m102") THEN DO:
         w-op.due-date = w-op.op-date.
         w-op.op-error = AddError(w-op.op-error,err-class,'m102').
      END.
                                       /* Дата документ от > даты опредня !   */
   IF w-op.doc-date > w-op.op-date THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m104').

   IF w-op.acct-cat EQ "d" THEN DO:
      IF NOT CAN-FIND(sec-code WHERE sec-code.sec-code = w-op-entry.currency) THEN DO:
         w-op.op-error = AddError(w-op.op-error,err-class,'m425').
      END.
   END.
   ELSE DO:
      IF w-op-entry.currency NE ""              AND
         w-op-entry.currency NE "{&in-NC-Code}" THEN DO:
         FIND FIRST currency WHERE currency.currency EQ w-op-entry.currency NO-LOCK NO-ERROR.
         IF NOT AVAIL currency THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m420').
      END.
   END.

   IF CAN-DO(str-error-code,"m942") AND
      INDEX(w-op.op-error,"m942") EQ 0 AND
      w-op.reference NE "" AND
      GetOpByRef(w-op.reference,w-op.op-date) NE ? THEN
      w-op.op-error = AddError(w-op.op-error,err-class,"m942").

   IF CAN-DO(str-error-code,"m947") AND w-op.reference NE "" THEN DO:
      FOR EACH op-impexp WHERE op-impexp.op-reference EQ w-op.reference
                           AND op-impexp.op-date EQ w-op.op-date NO-LOCK,
          FIRST op-entry OF op-impexp
                         WHERE op-entry.acct-db EQ w-op-entry.acct-db
                           AND op-entry.op-date EQ w-op.op-date NO-LOCK,
             FIRST op OF op-entry WHERE op.op-status NE "В" NO-LOCK
             BREAK BY op-entry.op:
               IF FIRST(op-entry.op)
                    AND INDEX(w-op.op-error,"m947") EQ 0 THEN DO:
                        w-op.op-error = AddError(w-op.op-error,err-class,'m947').
                  LEAVE.
               END.
      END.
   END.
   IF CAN-DO(str-error-code,"m948") AND w-op.reference NE "" THEN DO:

      vShiftDate = GetXAttrValueEx("op-kind",
                                   ENTRY(1,w-op.op-kind),
                                   "DateIntervalCvi",
                                   "").
      IF {assigned vShiftDate} THEN DO:
         IF NUM-ENTRIES(vShiftDate) EQ 1 THEN 
            ASSIGN
               vShiftDate1 = INT64(vShiftDate)
               vShiftDate2 = vShiftDate1.
         ELSE
            ASSIGN
               vShiftDate1 = INT64(ENTRY(1,vShiftDate))
               vShiftDate2 = INT64(ENTRY(2,vShiftDate)).
   
   
         FOR EACH op-date WHERE
                  op-date.op-date LE w-op.op-date + vShiftDate1
              AND op-date.op-date GE w-op.op-date - vShiftDate2 NO-LOCK,
   
             EACH op-impexp WHERE op-impexp.op-reference EQ w-op.reference
                              AND op-impexp.op-date EQ op-date.op-date NO-LOCK,
             FIRST op-entry OF op-impexp
                            WHERE op-entry.acct-db EQ w-op-entry.acct-db
                              AND op-entry.op-date EQ op-date.op-date NO-LOCK,
                FIRST op OF op-entry WHERE op.op-status NE "В" NO-LOCK
                BREAK BY op-entry.op:
                  IF FIRST(op-entry.op)
                       AND INDEX(w-op.op-error,"m948") EQ 0 THEN DO:
                           w-op.op-error = AddError(w-op.op-error,err-class,'m948').
                     LEAVE.
                  END.
         END.
      END.
   END.
                                  /* Такой документ уже есть по номеру и сумме*/
   IF CAN-DO(str-error-code,"m943")    AND
      INDEX(w-op.op-error,"m943") EQ 0 THEN DO:
      RUN Checkm943 ("m943").
   END.
   IF CAN-DO(str-error-code,"m943A")    AND
      INDEX(w-op.op-error,"m943A") EQ 0 THEN DO:
      RUN Checkm943 ("m943A").
   END.

END PROCEDURE.
/************************************ КОНТРОЛЬ СЧЕТА ПОЛУЧАТЕЛЯ ***************/
PROCEDURE chek-acct-rec.

   DEF VAR vInnBase     AS CHAR                       NO-UNDO.
   DEF VAR vNameBase    AS CHAR                       NO-UNDO.
   DEF VAR vNameE       AS CHAR                       NO-UNDO.
   DEF VAR vNameCorp    AS CHAR                       NO-UNDO.
   DEF VAR vOK          AS LOGICAL                    NO-UNDO.
   DEF VAR vAcctStat    AS CHARACTER                  NO-UNDO.

   DEFINE VARIABLE vOldNameRec   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNameShort    AS CHARACTER   NO-UNDO.

   IF flag-go EQ 0 OR flag-go EQ 2 THEN DO:

      IF flag-go EQ 0 THEN DO:
        {find-act.i &acct = w-op-entry.acct-cr}
      END.
      ELSE DO:
         {find-act.i &acct = w-op.acct-rec}
      END.

      IF NOT AVAIL acct THEN           /*  "Счет не открыт"                   */
         w-op.op-error = AddError(w-op.op-error,err-class,'m50').
      ELSE DO:
                                       /*  "Категория"                        */
         IF w-op-entry.acct-cat NE acct.acct-cat THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m295').

         vAcctStat = BlockAcct(acct.acct + ',' + acct.currency,
                               IF w-op.op-date EQ TODAY THEN DATETIME(TODAY,MTIME) 
                               ELSE DATETIME(w-op.op-date + 1) - 1).
         IF vAcctStat MATCHES "Блок*" THEN DO:
                                       /*"Счет кредита блокирован"            */
            IF (   LOOKUP("Блок",vAcctStat)   GT 0
                OR LOOKUP("БлокКр",vAcctStat) GT 0
               ) THEN
               w-op.op-error = AddError(w-op.op-error,err-class,'m60').
         END.
                                       /* "Счет получателя еще не открыт"                */
         IF acct.open-date GT w-op.op-date THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m70e').

                                       /* "Счет закрыт"                       */
         IF acct.close-date NE ? THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m70').
         ELSE
            IF acct.side         EQ "П" AND
               w-op-entry.amt-rub > 0   THEN DO:
               IF CAN-DO(mAcctContCrd2,acct.contract) THEN DO:
                  RUN CheckCard2 (acct.acct,acct.currency).
                  IF RETURN-VALUE NE "" THEN
                     w-op.op-error = AddError(w-op.op-error,err-class,'m80').
               END.
               IF CAN-DO(str-error-code,"m85")  THEN  RUN is-overdr.
            END.

         RUN is-kau.
                                       /*  "Бал Счет не открыт"               */
         FIND FIRST bal-acct OF acct NO-LOCK NO-ERROR.
         IF NOT AVAIL bal-acct THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m51').

         IF AVAILABLE(acct) THEN DO:
            RUN GetINNAndName (NO,
                               BUFFER acct,
                               OUTPUT vInnBase,
                               OUTPUT vNameBase).
            RUN GetCustShortName IN h_cust (acct.cust-cat,
                                            acct.cust-id,
                                            OUTPUT vNameShort).
                                                 /* Неверный ИНН              */
            IF LOOKUP("m90",str-error-code)                       NE 0 AND
               {assigned w-op.inn-rec}                                 AND
               left-trim(w-op.inn-rec,"0") NE left-trim(vInnBase,"0")  THEN
               w-op.op-error = AddError(w-op.op-error,err-class,"m90").

            IF {assigned w-op.name-rec}            AND
               {assigned vNameBase}               AND
               vNameBase                   NE "-" AND
              (LOOKUP("m92",  str-error-code) NE 0  OR
               LOOKUP("m92k", str-error-code) NE 0  OR 
               LOOKUP("m92c", str-error-code) NE 0 )AND 
               NOT Chk2863U (w-op.details, w-op.name-send)
            THEN DO:
               
               ASSIGN
                  vOldNameRec   = w-op.name-rec
                  vNameBase     = ReplaceBad(REPLACE (REPLACE (vNameBase, "ё", "е"), "Ё", "Е"))
                  vNameShort    = ReplaceBad(REPLACE (REPLACE (vNameShort, "ё", "е"), "Ё", "Е"))
                  vNameE        = ReplaceBad(REPLACE (REPLACE (w-op.name-rec, "ё", "е"), "Ё", "Е"))
               .
               vOK = CompareNameFast (INPUT  vNameBase,
                                      INPUT  vNameE,
                                      INPUT  mPercLen,
                                      INPUT  mPercWord).
               IF vOK NE YES THEN
                  vOK = CompareNameSlow (INPUT  vNameBase,
                                         INPUT  vNameE,
                                         INPUT  mPercLen,
                                         INPUT  mPercWord).
               IF vOK NE YES THEN DO:
                  IF LOOKUP("m92",  str-error-code) NE 0 THEN
                     w-op.op-error = AddError(w-op.op-error,err-class,"m92").

                  IF LOOKUP("m92k",  str-error-code) NE 0 THEN DO:

                     vOK = CompareNameFast (INPUT  vNameShort,
                                            INPUT  vNameE,
                                            INPUT  mPercLen,
                                            INPUT  mPercWord).
                     IF vOK NE YES THEN
                        vOK = CompareNameSlow (INPUT  vNameShort,
                                               INPUT  vNameE,
                                               INPUT  mPercLen,
                                               INPUT  mPercWord).
                     IF vOK NE YES THEN
                        w-op.op-error = AddError(w-op.op-error,err-class,"m92k").
                  END.
                  IF LOOKUP("m92c",  str-error-code) NE 0 THEN DO:
                     {check_name.i &cust-id = acct.cust-id}
                     IF vOK NE YES THEN
                        w-op.op-error = AddError(w-op.op-error,err-class,"m92c").
                  END.

               END.

               w-op.name-rec = vOldNameRec.
            END.

            FIND FIRST bal-acct OF acct NO-LOCK NO-ERROR.
            IF CAN-DO(str-error-code,"m110") OR
               CAN-DO(str-error-code,"m130") THEN DO:

               RUN cli-pos.p (acct.acct,
                              acct.currency,
                              gend-date,
                              gend-date,
                              "П").
                                          /* Дебетовое сальдо при кред. пас.счета*/
               IF (w-op-entry.amt-rub LT 0 OR w-op-entry.amt-cur LT 0) AND
                   acct.side EQ "П" AND ((sh-bal - w-op-entry.amt-rub) GT 0 OR
                      (acct.currency NE "" AND
                                  (sh-val - w-op-entry.amt-cur) GT 0 )) AND
                  acct.contr-acct EQ ""                                THEN
                  w-op.op-error = AddError(w-op.op-error,err-class,'m110').
               ELSE                       /* "Кредитовое сальдо"                 */
                  IF acct.side                      EQ "А" AND
                     ((sh-bal - w-op-entry.amt-rub) LT 0   OR
                      (acct.currency NE "" AND
                         (sh-val - w-op-entry.amt-cur) LT 0 )
                      )                                    AND
                     acct.contr-acct EQ ""                THEN
                     w-op.op-error = AddError(w-op.op-error,err-class,'m130').

            END.

            RUN Get-Kau-Id in h_kau (acct.acct, acct.currency, output vKauId).
            IF vKauId BEGINS "loan-dps" 
               AND SearchPfile ("dps-imp-chk")   THEN DO: 
               RUN dps-imp-chk.p (INPUT RECID(acct),
                                 INPUT w-op.op-date,
                                 INPUT (IF w-op-entry.currency EQ ""
                                           THEN w-op-entry.amt-rub
                                           ELSE w-op-entry.amt-cur),
                                 OUTPUT fl-o).
               IF fl-o NE 0 AND fl-o NE ? THEN 
                  w-op.op-error = AddError(w-op.op-error,err-class,"m" + STRING(fl-o)).
            END.
         END.                          /* AVAIL bal-acct                      */
      END.                             /* IF AVAIL acct THEN DO:              */
   END.                                /* flag-go EQ 0 OR flag-go EQ 2        */
   ELSE DO:                            /* начальные */
      RUN check-ascii-num (INPUT DelFilFromAcct(w-op.acct-rec), "", OUTPUT ok).
      IF NOT ok THEN
         w-op.op-error = AddError(w-op.op-error,err-class,"m446").
   END.
END PROCEDURE.

/*************************** КОНТРОЛЬ СООТВЕТСТВИЯ МАКЕТУ МЦИ *****************/
PROCEDURE chek-mci.

   DEFINE VARIABLE vDetails AS CHARACTER   NO-UNDO.

   IF flag-go EQ 1 AND CAN-DO("01,06,16",mDigital) THEN DO:

      vDetails = RIGHT-TRIM(w-op.details).
                                       /* Длина назначение платежа > 210 симв.*/
      DO:
         IF length(vDetails) > 210 THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m47').

                                       /*  Длина назначение платежа < 2 симв. */
         IF length(vDetails) < 2 THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m48').

                                       /*  наименование получателя  < 2       */
         IF length(vDetails)      > 2 AND
            length(w-op.name-rec) < 2 THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'m91').

         RUN check-ascii-num (INPUT w-op.inn-rec, "", OUTPUT ok).

         IF NOT ok THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"m944").

         w-op.inn-send = TRIM(w-op.inn-send).
         RUN check-ascii-num (INPUT w-op.inn-send, "", OUTPUT ok).

         IF NOT ok THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"m946").
      END.
                                       /*  Отсутствует номер документа        */
      IF length(vDetails)       > 2 AND
         length(w-op.name-rec)  > 2 AND
         (w-op.doc-num EQ "" OR w-op.doc-num EQ ?) THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m49').

      i = INT64(w-op.doc-num) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN       /* В номере должны быть только цифры   */
         w-op.op-error = AddError(w-op.op-error,err-class,'m93').
      ELSE
         IF ((INT64(w-op.doc-num)         EQ 0)  OR
            (INT64(w-op.doc-num) mod 1000 EQ 0)) AND
            CAN-DO(str-error-code,"m499")      THEN DO:
             {additem.i "w-op.op-error" ""m499""}  /* Формат обмена "Омск" - номере документа не должен быть равным 0 или быть кратным 1000 */
         END.
                                       /* Счет получатедя д/б 30109           */
      IF w-op.acct-send    BEGINS "30110" AND
         NOT w-op.acct-rec BEGINS "30109" THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m37').

                                       /* Счет плательщика д/б 30109          */
      IF w-op.acct-rec      BEGINS "30110" AND
         NOT w-op.acct-send BEGINS "30109" THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'m38').

   END.                                /* flag-go EQ 1                        */

   /*отсутствует очередность платежа*/
   IF NOT {assigned w-op.order-pay} THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m108').
                                       /* Некрр. гр. очередности платежа      */
   IF   /*flag-go NE 0  AND*/
      NOT CAN-FIND(FIRST code WHERE code.class     = "order-pay"
                                AND code.code = w-op.order-pay
                                AND ((NOT {assigned code.val}) OR DATE(code.val) GT w-op.doc-date)) 
   THEN
      w-op.op-error = AddError(w-op.op-error,err-class,'m109').

END PROCEDURE.

/*************************** КОНТРОЛЬ СООТВЕТСТВИЯ МАКЕТУ М18 *****************/
PROCEDURE CheckM18:
   DEFINE VAR vErr AS CHAR NO-UNDO.
   DEFINE VAR vI   AS INT64 NO-UNDO.

   IF CAN-DO(str-error-code,"m18") THEN
      vErr = CheckElectro(BUFFER banks, bank-mfo-9, dict-uer).
   ELSE
      vErr = CheckElectroNew(BUFFER banks, bank-mfo-9, dict-uer).

   CASE vErr:
      WHEN ""      THEN.
      WHEN "error" THEN w-op.op-error = AddError(w-op.op-error,err-class,
                                                 IF CAN-DO(str-error-code,"m18") THEN
                                                    "m18"
                                                 ELSE "m181"
                                                ).
      OTHERWISE DO vI = 1 TO NUM-ENTRIES(vErr) :
                   w-op.op-error = AddError(w-op.op-error,err-class,ENTRY(vI,vErr)).
                END.
   END CASE.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
PROCEDURE is-overdr.
IF FGetSetting("СчОверДр",?,"Нет") EQ "Да" THEN
DO:
   overdr-loop:
   FOR EACH cacct WHERE cacct.acct-cat   EQ "b"
                    AND cacct.currency   EQ acct.currency
                    AND cacct.cust-cat   EQ acct.cust-cat
                    AND cacct.cust-id    EQ acct.cust-id
                    AND cacct.contract   EQ "ОверДр"
                    AND cacct.close-date EQ ?
                    USE-INDEX acct-cust
   NO-LOCK:
      RUN acct-pos IN h_base (cacct.acct,cacct.currency,gend-date,gend-date,"П").

      IF (sh-bal NE 0 OR sh-val NE 0) THEN DO:
         w-op.op-error = AddError(w-op.op-error,err-class,'m85').
         LEAVE  overdr-loop.
      END.
   END.
END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
PROCEDURE is-kau.                      /* "Счет имеет КАУ "                   */
   IF     AVAILABLE(acct)
      AND {assigned acct.kau-id}
      AND GetXattrVAlue("code",
                        "ШаблКау" + "," + acct.kau-id,
                        "KauCheckImp"
                       ) NE  "Да"
      THEN w-op.op-error = AddError(w-op.op-error,err-class,"m86").

END PROCEDURE.
/*----------------------------------------------------------------------------*/
PROCEDURE check-asc.
   DEF INPUT PARAM in-buf AS CHAR NO-UNDO.
   DEF INPUT PARAM in-err AS CHAR NO-UNDO.

   RUN check-ascii-expand (in-buf,{&ASC-EXPAND},OUTPUT ok).
   IF NOT ok THEN
         w-op.op-error = AddError(w-op.op-error,err-class,in-err).
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Поиск в списке террористов                                                 */
/*----------------------------------------------------------------------------*/
PROCEDURE CheckTerror:
   DEFINE INPUT PARAMETER iExport AS LOGICAL NO-UNDO.

   DEFINE VAR vTerrSend    AS LOGICAL           NO-UNDO.
   DEFINE VAR vTerrRecv    AS LOGICAL           NO-UNDO.
   DEFINE VAR vTerrDetails AS LOGICAL           NO-UNDO.
   DEFINE VAR vTerrList    AS CHAR              NO-UNDO.
   DEFINE VAR vListSend    AS CHAR              NO-UNDO.
   DEFINE VAR vListRecv    AS CHAR              NO-UNDO.
   DEFINE VAR vListDetails AS CHAR              NO-UNDO.
   DEFINE VAR vItm         AS INT64             NO-UNDO.

   DEFINE VARIABLE vNameSend AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNameRecv AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct     AS CHARACTER   NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   IF FGetSetting("ОпОтмыв","TerrCheck",?) NE "ДА" THEN RETURN.

   IF CAN-FIND(FIRST op WHERE
                     op.op             EQ w-op.op
                 AND op.op-date        EQ w-op.op-date
                 AND op.doc-num        EQ w-op.doc-num
                 AND op.op-transaction EQ w-op.op-transaction) THEN DO:
      vTerrList = GetXAttrValue("op",string(w-op.op),"LegTerr").
      IF vTerrList EQ "NO" THEN RETURN.
   END.

   IF FGetSetting("ОпОтмыв","TerrKeep","НЕТ") EQ "ДА" THEN DO:

      vNameSend = w-op.name-send.
      IF flag-go EQ 0 OR flag-go EQ 1 THEN DO:
         vAcct = IF flag-go EQ 0
                 THEN w-op-entry.acct-db
                 ELSE w-op.acct-send.
         {find-act.i &acct = vAcct}
         IF AVAIL acct THEN DO:
            IF CheckClientWhite(acct.cust-cat, acct.cust-id) THEN
               vNameSend = "".
         END.
      END.
      IF {assigned vNameSend} AND
         {assigned w-op.inn-send} AND w-op.inn-send NE FILL("0", LENGTH(w-op.inn-send)) THEN
         IF CheckClientWhiteByInn(w-op.inn-send) THEN
            vNameSend = "".
      
      vNameRecv = w-op.name-rec.
      IF flag-go EQ 0 OR flag-go EQ 2 THEN DO:
         vAcct = IF flag-go EQ 0
                 THEN w-op-entry.acct-cr
                 ELSE w-op.acct-rec.
         {find-act.i &acct = vAcct}
         IF AVAIL acct THEN DO:
            IF CheckClientWhite(acct.cust-cat, acct.cust-id) THEN
               vNameRecv = "".
         END.
      END.
      IF {assigned vNameRecv} AND
         {assigned w-op.inn-rec} AND w-op.inn-rec NE FILL("0", LENGTH(w-op.inn-rec)) THEN
         IF CheckClientWhiteByInn(w-op.inn-rec) THEN
            vNameRecv = "".

      RUN CheckTerrorName2(INPUT  ReplaceBad(vNameSend),
                           INPUT  ReplaceBad(vNameRecv),
                           OUTPUT vListSend,
                           OUTPUT vListRecv).

      IF {assigned vListSend} THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'t001').

      IF {assigned vListRecv} THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'t002').

      IF FGetSetting("ОпОтмыв","TerrDetails","НЕТ") EQ "ДА" THEN DO:
         RUN CheckTerrorDetails2(INPUT  ReplaceBad(w-op.details),
                                 OUTPUT vListDetails).
         IF {assigned vListDetails} THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'t005').
      END.

      IF iExport THEN DO:                        /* Экспорт                   */
         IF NOT {assigned vTerrList} THEN
            UpdateSigns("op",
                        string(w-op.op),
                        "LegTerr",
                        TRIM(vListSend + "," + vListRecv + "," + vListDetails,","),
                        NO).
      END.
      ELSE DO:                                   /* Импорт                    */
         CrWOpSigns("op",
                    STRING(w-op.op),
                    "LegTerr",
                    SUBSTR(TRIM(vListSend + "," + vListRecv + "," + vListDetails ,","),1,4000),
                    NO).
      END.
   END.
   ELSE DO:
      RUN CheckTerrorName(INPUT  ReplaceBad(w-op.name-send),
                          INPUT  ReplaceBad(w-op.name-rec),
                          OUTPUT vTerrSend,
                          OUTPUT vTerrRecv).

      IF vTerrSend THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'t001').

      IF vTerrRecv THEN
         w-op.op-error = AddError(w-op.op-error,err-class,'t002').

      IF FGetSetting("ОпОтмыв","TerrDetails","НЕТ") EQ "ДА" THEN DO:
         RUN CheckTerrorDetails(INPUT  ReplaceBad(w-op.details),
                                OUTPUT vTerrDetails).
         IF vTerrDetails THEN
            w-op.op-error = AddError(w-op.op-error,err-class,'t005').
      END.

   END.

END PROCEDURE.

PROCEDURE MakePDLCheckList.
   DEFINE INPUT        PARAMETER iAcct       AS CHARACTER   NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioCheckList AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vName1 AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vName2 AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vInn   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFIO   AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bAcct FOR acct.

   {find-act.i 
      &acct = iAcct 
      &bact = bAcct}
   IF AVAIL bAcct THEN
      IF bAcct.cust-cat EQ "Ч" THEN DO:
         RUN GetCustName IN h_base (bAcct.cust-cat, bAcct.cust-id, "",
                                    OUTPUT vName1, OUTPUT vName2, INPUT-OUTPUT vInn).
         vFIO = TRIM(vName1 + " " + vName2).
      END.
   IF vFIO NE "" AND LOOKUP(vFIO, ioCheckList) EQ 0 THEN
      {additem.i ioCheckList vFIO}

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE Checkm943.
   DEF INPUT PARAM iError       AS CHAR    NO-UNDO.
   DEF VAR         vName        AS CHAR    NO-UNDO.
   DEF VAR         vNameKind    AS CHAR    NO-UNDO.
   DEF VAR         vIsEqual     AS LOGICAL NO-UNDO.
   DEF VAR         CheckStatusB AS LOGICAL NO-UNDO.

   CheckStatusB = GetXAttrValueEX("op-kind",
                                  ENTRY(1,w-op.op-kind),
                                  "CheckStatusB",
                                  "Да") EQ "Да".

   CASE flag-go:
      WHEN 0 THEN                      /* внутренние                          */
         ASSIGN
            vName       =  ""
            vNameKind   =  ""
            acct-corr   =  w-op.acct-rec.
      WHEN 1 THEN                      /* начальные                           */
         ASSIGN
            bic-corr       =  w-op.bank-code-rec
            corr-acct-corr =  w-op.bank-corr-acct-rec
            acct-corr      =  w-op.acct-rec
            vName          =  w-op.name-send
            vNameKind      =  "name-send".
      WHEN 2 THEN                      /* ответные                            */
         ASSIGN
            bic-corr       =  w-op.bank-code-send
            corr-acct-corr =  w-op.bank-corr-acct-send
            acct-corr      =  w-op.acct-send
            vName          =  w-op.name-rec
            vNameKind      =  "name-rec".
   END CASE.

LOOP:
      FOR EACH op-entry WHERE op-entry.op-date  EQ w-op.op-date       
                          AND op-entry.acct-cat EQ w-op.acct-cat
                          AND op-entry.amt-rub  EQ w-op-entry.amt-rub 
                          AND op-entry.currency EQ ""
                          AND op-entry.op       NE w-op.op   NO-LOCK:
         IF TRIM(ENTRY(1,op-entry.acct-db,"@"))   EQ
            TRIM(ENTRY(1,w-op-entry.acct-db,"@")) AND
            TRIM(ENTRY(1,op-entry.acct-cr,"@"))   EQ
            TRIM(ENTRY(1,w-op-entry.acct-cr,"@")) THEN
         DO:
            FIND FIRST op OF op-entry  WHERE op.doc-num = w-op.doc-num
                         AND op.ben-acct EQ (IF op.ben-acct NE "" THEN acct-corr
                                             ELSE "")
                         AND IF CheckStatusB THEN op.op-status NE "В"
                                             ELSE YES 
            NO-LOCK NO-ERROR.
            IF iError EQ "m943A" AND AVAIL op THEN
            DO:
               RUN CheckOpSigns(INPUT vName, INPUT vNameKind, OUTPUT vIsEqual).
               IF NOT vIsEqual THEN NEXT LOOP.
            END.
            IF AVAIL op THEN
            DO:
               FIND FIRST op-bank OF op NO-LOCK NO-ERROR.
               IF NOT AVAIL op-bank OR
                     (AVAIL op-bank AND op-bank.bank-code EQ bic-corr AND
                      op-bank.corr-acct EQ TRIM(ENTRY(1,corr-acct-corr,"@"))) THEN
                  w-op.op-error = AddError(w-op.op-error,err-class,iError).
            END.
         END.
      END. /*FOR EACH*/

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE CheckOpSigns.
   DEF INPUT  PARAM iName      AS CHAR             NO-UNDO.
   DEF INPUT  PARAM iNameKind  AS CHAR             NO-UNDO.
   DEF OUTPUT PARAM oIsEqual   AS LOGICAL INIT YES NO-UNDO.

   DEF VAR vSignsVal   AS CHAR NO-UNDO.

   vSignsVal = GetXAttrValue("op",string(op.op),iNameKind).
   IF {assigned vSignsVal} THEN
      oIsEqual = TRIM(iName) EQ TRIM(vSignsVal).

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*         Контроль проставления кодов операций в назначении платежа 117-И    */
/*----------------------------------------------------------------------------*/
PROCEDURE CheckWOp117I.
 IF     w-op-entry.currency EQ "" /* см. 0039034 */
    AND CAN-DO(str-error-code,"m485")
    AND (
        ChckAcct117I(w-op-entry.acct-db)
    OR  ChckAcct117I(w-op-entry.acct-cr)
    OR  ChckAcct117I(w-op.acct-send)
    OR  ChckAcct117I(w-op.acct-rec)
        )
 THEN DO:
    RUN ChkStr117i (w-op.details).
    IF RETURN-VALUE = "error"
    THEN w-op.op-error = AddError(w-op.op-error,err-class,"m485").
 END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*         Контроль поля 110 для таможенных и налоговых платежей              */
/*----------------------------------------------------------------------------*/
PROCEDURE Check110 PRIVATE:
   
   DEFINE VARIABLE vPokOP110 AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER bCode FOR  Code.

   vPokOP110 = GetWOpSigns("ПокТП") .
   
   IF CAN-DO(mBalNalog, (IF flag-go EQ 0 THEN w-op-entry.acct-cr
                              ELSE IF flag-go EQ 2 THEN w-op.acct-send
                              ELSE w-op.acct-rec ) )
   THEN DO:

      FIND FIRST bCode WHERE 
               bCode.class EQ "Нал:ВП" 
            AND bCode.parent EQ "Нал:ВП" 
            AND bCode.code EQ vPokOP110 NO-LOCK NO-ERROR.
      IF AVAILABLE bCode THEN
      DO:
         IF CAN-DO(mOsn106tp,GetWOpSigns("ПокОП"))          
            AND bCode.val NE "*"
            AND NOT CAN-DO(bCode.val,"т") THEN
            w-op.op-error = AddError(w-op.op-error,err-class,"n110b").
         ELSE 
         IF NOT CAN-DO(mOsn106tp,GetWOpSigns("ПокОП"))
            AND bCode.val NE "*"
            AND NOT CAN-DO(bCode.val,"н") THEN 
            w-op.op-error = AddError(w-op.op-error,err-class,"n110b"). 
      END.                              
   END.
        
END PROCEDURE.
/**/
PROCEDURE Get_Inn_Name:
   DEF PARAM  BUFFER bAcct        FOR                 acct.
   DEF OUTPUT PARAM  oINN         AS CHAR             NO-UNDO.
   DEF OUTPUT PARAM  oNames       AS CHAR             NO-UNDO.

   DEF VAR          vNamePol1     AS CHAR             NO-UNDO.
   DEF VAR          vNamePol2     AS CHAR             NO-UNDO.
   DEF VAR          vNamePol3     AS CHAR             NO-UNDO.
   DEF VAR          vNamePol4     AS CHAR             NO-UNDO.
   DEF VAR          vName         AS CHAR EXTENT 2    NO-UNDO.
   DEF BUFFER       bal-acct      FOR                 bal-acct.

   FIND FIRST bal-acct  OF  bAcct NO-LOCK NO-ERROR.

   ASSIGN
      mOur-Acct  = FGetSetting("БалСчИНН",?,"")
      mOur-Inn   = FGetSetting("ИНН",     ?,"")
      mContr-MBR = FGetSetting("НазнСчМБР",?,"")
   .
   IF    bAcct.cust-cat EQ "В"
   OR    CAN-DO(mOur-Acct,STRING(bal-acct.bal-acct))
   THEN DO:
      FIND FIRST branch WHERE branch.branch-id EQ dept.branch NO-LOCK NO-ERROR. 
      IF AVAIL branch THEN 
         ASSIGN
            oINN      = mOur-Inn
            vNamePol1 = branch.name + CHR(250) + branch.short-name.
   END.
   ELSE DO:
      RUN GetCustName IN h_base(bacct.cust-cat,
                                bacct.cust-id,
                                ?,
                                OUTPUT vName[1],
                                OUTPUT vName[2],
                                INPUT-OUTPUT oINN).
      vNamePol1 = TRIM(GetNullStr(vName[1] + " " + vName[2])," ?").
      IF NOT {assigned oINN} THEN
      CASE bacct.cust-cat:
         WHEN "Ю" THEN
         DO:
            FIND FIRST cust-corp WHERE
               cust-corp.cust-id = bacct.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL(cust-corp) THEN
            ASSIGN 
               oINN      = cust-corp.inn
               vName[1]  = cust-corp.cust-stat
               vName[2]  = cust-corp.name-corp
               vNamePol1 = TRIM(GetNullStr(vName[1] + " " + vName[2])," ?").
         END.
         WHEN "Ч" THEN
         DO:
            FIND FIRST person WHERE
               person.person-id = bacct.cust-id
            NO-LOCK NO-ERROR.
            IF AVAIL(person) THEN
            ASSIGN
               oINN      = person.inn
               vName[1]  = person.name-last
               vName[2]  = person.first-names
               vNamePol1 = TRIM(GetNullStr(vName[1] + " " + vName[2])," ?").
         END.
      END.
      PUT STREAM sImpUpd UNFORMATTED 
         STRING(NOW,"99/99/9999 HH:MM:SS") ";"
         "Get_Inn_Name: "
         bacct.cust-cat ";"
         bacct.cust-id ";"
         STRING(AVAIL(cust-corp)) ";"
         oINN ";"
         vNamePol1 ";"
         shFilial
      SKIP.
      FOR FIRST code
          WHERE code.class = "КодПредп"
            AND code.val   = vName[1] NO-LOCK:
         vNamePol2 = TRIM(GetNullStr(code.name + " " + vName[2])," -?").
      END.
      
      IF NOT {assigned oINN} THEN
      DO:
         {getcust.i &pref     = b
	                 &name     = vName
	                 &inn      = oINN
	                 &OFFinn   = "/*"
	      }
      END.
      
      vNamePol3 = TRIM(GetNullStr(vName[1] + " " + vName[2])," -?").
      IF vNamePol3 EQ vNamePol1 THEN vNamePol3 = "".
      
      RUN GetEnglishName (INPUT  bacct.cust-cat,
                          INPUT  bacct.cust-id,
                          OUTPUT vNamePol4).
   END.

   oNames = TRIM(vNamePol1 + CHR(250) +
                 vNamePol2 + CHR(250) +
                 vNamePol3 + CHR(250) +
                 vNamePol4,CHR(250)).

END PROCEDURE.
/******************************************************************************/
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='04/06/2015 21:44:18.183+04:00' */
/* $LINTUSER='voob' */
/* $LINTMODE='1' */
/* $LINTFILE='imp-upd.p' */
/*prosignZ8o/kFRQCKNAQZtGaNJsWg*/