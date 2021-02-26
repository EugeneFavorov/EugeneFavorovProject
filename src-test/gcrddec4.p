/*
               Банковская интегрированная система БИСквит
    copyright: (c) 1992-2001 ТОО "Банковские информационные системы"
     filename: gcrddec3.p
      comment: Списание документов с картотеки
      comment:
   parameters:
         uses:
      used by:
      created: 27/07/2002 kostik
     modified:
  last change:
*/

{globals.i}
{tmprecid.def}
{g-defs.i}
{pick-val.i}
{sh-defs.i new}
{def-wf.i new}
{defframe.i new}
{g-docnum.def}    /* Для схем автонумерации. */ 


DEFINE INPUT  PARAMETER in-op-date    AS DATE      NO-UNDO. /*Дата опер дня */
DEFINE INPUT  PARAMETER ipOpKindRecId AS RECID     NO-UNDO. /*Транзакция */

DEFINE VARIABLE vMethodTempl      AS   CHARACTER       NO-UNDO. /*Имена функций для обработки
                                                                 и контроля документа.
                                                                 Доп реквизиты: ВыпДо ВыпПосле*/
DEFINE VARIABLE vMethodParamTempl AS   CHARACTER      NO-UNDO. /*Параметры для функции обработки
                                                                 и контроля документа.
                                                                 Доп реквизиты: ВыпДо ВыпПосле*/

DEFINE VARIABLE vFlagErrlog  AS   LOGICAL              NO-UNDO.  /*Флаг Ошибки для обработки
                                                                  различных ситуаций*/
DEFINE VARIABLE vMessChar    AS   CHARACTER            NO-UNDO. /*Сообщения об ошибках*/
DEFINE VARIABLE vLogFlag     AS   LOGICAL              NO-UNDO.
DEFINE VARIABLE fler         AS   LOGICAL              NO-UNDO.  /*Для инклюдника details.def*/

DEFINE VARIABLE in-cont-code AS   CHARACTER            NO-UNDO. /*Для g-acctv1.i*/
DEFINE VARIABLE dval         LIKE op-entry.value-date  NO-UNDO. /*Дата валютирования (asswop.i)*/
DEFINE VARIABLE vDetailsChar AS   CHARACTER            NO-UNDO. /*Для обработки поля "Детали платежа"*/
DEFINE VARIABLE vAutolog     AS   LOGICAL              NO-UNDO. /*Флаг автомати*/

DEFINE VARIABLE CopyXattrStr AS   CHARACTER            NO-UNDO. /*Для копирования доп реквизитов */
/* Вставка Плюс банк */
DEFINE VARIABLE cCommision   AS   CHARACTER    NO-UNDO INIT "". /* Список op.op для начисления комиссий */
/* Конец вставки Плюс банк */

DEFINE BUFFER xwop           FOR wop.                      /*Для g-acctv1.i*/

vAutoLog = NO.
/*Проверка прав на транзакцию*/
{gcrddec3.i}               /*Процедуры обработки картотек*/
{details.def}
{kautools.lib}
{tmprecid.def &NGSH=YES &PREF="old_"}
{tmprecid.def &NGSH=YES &PREF="kau_"}
/*Выбор документа для списания
  Реквизит "Процедура вызываемая при списании":

  Список процедур разработанных ранее:
      ПросмотрДок - Процедура для выбора документа по счету.
                    Входной параметр номер счета.
                         - Для определения счета можно использовать
                           процедуры выбора счета: ПросмотрДок[shwacct1()]
                         - Прописать номер счета явно: ПросмотрДок[<Счет>,<Валюта>]
      --------------------------------------------------------------------------
      showacct1  -  Процедуры для выбора счета.
      showacct2     Входной  параметр название картотеки. (Название Шаблона КАУ)
      showacct3     Выходной параметр счет.
      acct(crd
      --------------------------------------------------------------------------
      showdoc1   - Просмотр документов по картотеке.
                   Входной  параметр название картотеки. (Название Шаблона КАУ)
                   Выходной параметр документ для списания
                   Разрабатывалась для списания с первой картотеки.
      --------------------------------------------------------------------------

  Выбранные записи помещаются в tmprecid.
*/
DEFINE VARIABLE mKauRID AS RECID NO-UNDO.
FIND FIRST op-kind WHERE RECID(op-kind) EQ ipOpKindRecId NO-LOCK.

RUN rid-keep.p(INPUT TABLE old_tmprecid).

FOR EACH tmprecid :
   DELETE tmprecid.
END.

FIND FIRST op-templ OF op-kind NO-LOCK NO-ERROR.
if avail op-templ then RUN setsysconf IN h_base ("ВалБалСч",op-templ.currency).

RUN SelectKau(BUFFER op-kind,OUTPUT mKauRid).

FIND FIRST kau  WHERE RECID(kau) EQ mKauRid
                           NO-LOCK NO-ERROR.
IF AVAIL kau AND NOT CAN-FIND(FIRST tmprecid WHERE tmprecid.id = RECID(kau))
THEN DO:
   CREATE tmprecid.
   tmprecid.id = RECID(kau).
END. 

RUN rid-rest.p(OUTPUT TABLE kau_tmprecid).

/* **************************************************************************** */
/*Логирование работы процедуры в автоматическом режиме */
{gcrddec3.log &DEFINELOG=YES}
/* ***************************************************** */
{g-currv1.i &ofbase="/*"} /*Обработка валюты документа*/
P_KAU:
FOR EACH kau_tmprecid NO-LOCK TRANSACTION:  /*Идем по таблице KAU*/
   RUN MessTool("","HEAD",vAutoLog,OUTPUT vLogFlag).
   FOR EACH wop:
      DELETE wop.
   END.
   GEN_DOC:
   FOR EACH op-template OF op-kind NO-LOCK:
      CopyXattrStr = GetXAttrValueEx("op-template",
                                     STRING(op-templ.op-kind) + "," +
                                     STRING(op-templ.op-templ),
                                     "КопДопРекв","").
      RUN SetSysConf in h_base ("Карт2КопРекв",CopyXattrStr).
      RUN MessTool("Шаблон N" + STRING(op-template.op-template,">9"),"",vAutoLog,OUTPUT vLogFlag).
      /*Создание документа */
      cur-op-date = in-op-date.
      CREATE op.
      {op(sess).cr}
      {g-op.ass}
      CREATE op-entry.
      {g-en.ass}

      ASSIGN
         op-entry.value-date = in-op-date
         op-entry.currency   = IF op-templ.currency <> ? THEN GetCurr(op-templ.currency)
                                                         ELSE op-entry.currency
      .
      CREATE wop.
      ASSIGN
         wop.currency = op-entry.currency
         dval         = op-entry.value-date
         wop.op-templ = op-templ.op-templ
         wop.op-kind  = op-kind.op-kind
         tcur         = op-entry.currency
         wop.op-recid = RECID(op)
      .

      {asswop.i}

      /* Получаем значение кода схемы автонумерации. */
      docnum-tmpl = GetXattrValue ("op-template",
                                    op-templ.op-kind + "," + STRING (op-templ.op-templ),
                                   "DocCounter").
      IF docnum-tmpl EQ "" THEN
         docnum-tmpl = GetXattrValue ("op-template",
                                       op-templ.op-kind + "," + STRING (op-templ.op-templ),
                                      "ДокНомер").
      IF docnum-tmpl NE "" THEN
      DO:
         DocNum-OP = STRING (RECID (Op)).
         op.doc-num = STRING(GetCounterNextValue(docnum-tmpl, gend-date)).
         IF op.doc-num EQ ?
             THEN RUN Fill-SysMes IN h_tmess ("", "", "0","Невозможно получить значение счетчика.").
      END.

      /********************Документ создан*************************************/
      vMethodTempl = GetXAttrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "ВыпДо",
                                     "").
      vMethodParamTempl = "".
      IF INDEX(vMethodTempl,"((") NE 0 THEN
      ASSIGN
         vMethodParamTempl = SUBSTRING(vMethodTempl,INDEX(vMethodTempl,"((") + 2)
         vMethodTempl      = SUBSTRING(vMethodTempl,1,INDEX(vMethodTempl,"((") - 1)
      .
      IF vMethodTempl NE "" AND vMethodTempl NE ? THEN DO: /*Процедура обработки шаблона*/

         IF LOOKUP(vMethodTempl, this-procedure:internal-entries) = 0 THEN DO:
            RUN MessTool("Не найден метод обработки шаблона: " + vMethodTempl,
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU, NEXT P_KAU.
         END.

         RUN VALUE(vMethodTempl) (kau_tmprecid.id,vMethodParamTempl,OUTPUT vFlagErrlog,OUTPUT vMessChar).
         IF vFlagErrlog THEN DO:
            RUN MessTool(vMessChar,
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU, NEXT P_KAU.
         END.
         ASSIGN
            wop.currency = op-entry.currency
            tcur         = op-entry.currency
         .
      END.
      {g-acctv1.i &OFbase    = YES
                  &vacct     = op-entry.acct
                  &nodisplay = YES
                  &OFsrch    = YES
      }

      ASSIGN
         wop.acct-db = op-entry.acct-db
         wop.acct-cr = op-entry.acct-cr
      .
      IF (op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "") OR
         (op-templ.prep-amt-natcur <> ? AND op-templ.prep-amt-natcur <> "") THEN DO:
         RUN parssen.p (RECID(wop), in-op-date, OUTPUT vFlagErrlog).
         IF vFlagErrlog THEN DO:
            RUN MessTool("Ошибка при расчете суммы",
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU,NEXT P_KAU.
         END.
         IF wop.amt-rub = 0 THEN 
              UNDO GEN_DOC,  NEXT GEN_DOC. 

         ASSIGN
            op-entry.amt-rub = wop.amt-rub
            op-entry.amt-cur = IF wop.currency NE "" AND wop.currency NE ? THEN
                               wop.amt-cur
                               ELSE
                               0
         .
      END.

      RUN parssign.p (in-op-date,
                      "op-template",
                      op-kind.op-kind + "," + string(op-templ.op-templ),
                      op-templ.class-code,
                      "op",
                      STRING(op.op),
                      op.class-code,
                      RECID(wop)).

      IF NOT vAutoLog THEN DO:
         ASSIGN
            op.op-status       = "А"
            op-entry.op-status = "А"
         .
         RUN "edit(op).p" (in-op-date,
                           RECID(op),
                           RECID(op-entry),
                           RECID(op-kind),
                           op-template.op-template,
                           NO
                          ).
         IF RETURN-VALUE EQ "ERROR,UNDO" THEN UNDO P_KAU, NEXT P_KAU.
         ASSIGN
            op.op-status       = op-template.op-status
            op-entry.op-status = op-template.op-status
         .
      END.
      ELSE DO:
         vDetailsChar = op-template.details.
         RUN ProcessDetails (RECID(wop), INPUT-OUTPUT vDetailsChar).
         op.details = vDetailsChar.
      END.

      /*Процедура контроля введенных значений*/
      vMethodTempl = GetXAttrValueEx("op-template",
                                     op-template.op-kind + "," + STRING(op-template.op-template),
                                     "ВыпПосле",
                                     "").
      vMethodParamTempl = "".
      IF INDEX(vMethodTempl,"((") NE 0 THEN
      ASSIGN
         vMethodParamTempl = SUBSTRING(vMethodTempl,INDEX(vMethodTempl,"((") + 2)
         vMethodTempl      = SUBSTRING(vMethodTempl,1,INDEX(vMethodTempl,"((") - 1)
      .

      IF vMethodTempl NE "" AND vMethodTempl NE ? THEN DO:
         IF LOOKUP(vMethodTempl, this-procedure:internal-entries) = 0 THEN DO:
            RUN MessTool("Не найден метод обработки шаблона: " + vMethodTempl,
                         "ERROR",
                          vAutoLog,
                          OUTPUT vLogFlag).
            UNDO P_KAU, NEXT P_KAU.
         END.

         RUN VALUE(vMethodTempl)(kau_tmprecid.id,
                                 vMethodParamTempl,
                                 OUTPUT vFlagErrlog,
                                 OUTPUT vMessChar,
                                 BUFFER op-entry).
         IF vFlagErrLog THEN UNDO GEN_DOC, RETRY GEN_DOC.
      END.

      IF op.doc-type EQ "016" THEN RUN inipoxtr.p (RECID(op),?). /*+ доп.реки*/

      /* ************************************** */
      FIND FIRST kau WHERE RECID(kau) EQ kau_tmprecid.id NO-LOCK NO-ERROR.
      IF op-entry.acct-db EQ kau.acct OR op-entry.acct-cr EQ kau.acct THEN
      RUN setKauDocSysConf(op.op-kind,RECID(op-entry),kau_tmprecid.id).
      {op-entry.upd &871=yes}
      RUN EditXattr.

/* Вставка Плюс банк */
      IF (op-template.op-template NE 1)
      THEN cCommision = cCommision + (IF (cCommision EQ "") THEN "" ELSE ",") + STRING(op.op).
/* Конец вставки Плюс банк */
   END.
END.

RUN rid-rest.p(OUTPUT TABLE old_tmprecid).
/* {g-print1.i} */

/* Вставка Плюс банк */
/* RUN SetSysconf IN h_base("ОплатаСКартотеки",cCommision). */
{topkind.def}
DEFINE VARIABLE cTranz      AS CHARACTER NO-UNDO INIT "sbk_comm".
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cErrMsg     AS CHARACTER NO-UNDO.

{empty tOpKindParams}     /* очистить таблицу параметров */
ASSIGN
    lOk =   TDAddParam("__oplst", cCommision)
    NO-ERROR.
IF NOT lOk
THEN MESSAGE "Возникла ошибка при передаче параметров в транзакцию " + cTranz
             ".~nКомиссии не начислены."
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
ELSE DO:
    RUN ex-trans.p (cTranz, TODAY, TABLE tOpKindParams, OUTPUT lOk, OUTPUT cErrMsg).

    IF NOT lOk
    THEN MESSAGE cErrMsg + "~nКомиссии не начислены."
        VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
END.
/* Конец вставки Плюс банк */

{gcrddec3.log &SHOWLOG=YES}
{intrface.del}          /* Выгрузка инструментария. */ 



