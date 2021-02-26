/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pp-pays.p
      Comment: Библиотека процедур обмена с платежными системами онлайн
   Parameters: нет
         Uses:
      Used BY:
      Created: 23.04.2015 KMBIS TT:0236973 Миграция.Прием платежей QIWI, Рапида, Уралсиб, КиберПлат
     Modified: 
*/
{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "pays"
   &LIBNAME       = "Онлайн обмен с платежными системами"
   &DESCRIPTION   = "Методы онлайн обмена"
}
{globals.i}
{exchange.equ}

&SCOP NO-BASE-PROC YES

DEF VAR mFileID  AS  INT64  NO-UNDO.

DEF TEMP-TABLE ttErrPay NO-UNDO
   FIELD PayRef    AS CHAR  /* Уникальный номер перевода в НКО */
   FIELD amt-rub   AS DEC   /* Сумма перевода                  */
   FIELD OrderAcct AS CHAR  /* Номер счета                     */
   FIELD PayErr    AS CHAR  /* Код причины возврата            */
.

{intrface.get kau}
{intrface.get card}
{intrface.get exch}
{intrface.get pack}
{intrface.get xrkc}
{intrface.get blkob}
{intrface.get count}
{intrface.get pbase}
{intrface.get refer}
{intrface.get rfrnc}
{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get xclass}

/* Процедура удаления документов */
{pck-op-del.pro}

{pays.fun}
{pays.pro}


/*===============================================================================================*/
/*=== Импорт онлайн запроса из КиберПлат ========================================================*/
{pfuncdef 
   &DEFPROC="CPlatOnlImport"
   &DESCRIPTION="Импорт онлайн запроса из КиберПлат"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE CPlatOnlImport:
   DEF INPUT PARAM iClass     AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iInstance  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG     NO-UNDO INIT ?.
DEF VAR vExch      AS  HANDLE  NO-UNDO.
DEF VAR vReq       AS  HANDLE  NO-UNDO. /* Указатель на таблицу с данными документа */                            

DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatOnlImport", "RUN").
   &ENDIF

   ASSIGN
      vExch = iInstance:DEFAULT-BUFFER-HANDLE
      vReq  = WIDGET-HANDLE(GetAttrValue2(GetBaseOpKind(),0,"CPlatHandle"))
   NO-ERROR. {&ON-ERROR}

   RUN InstanceCreateEx IN h_exch (?, vExch, 0).

   /*=== Заполняем недостающие поля в платеже ===*/
   ASSIGN
      /* Банк получателя     */
      vExch::bank-code-rec      = vReq::bank-code-rec
      vExch::bank-corr-acct-rec = vReq::bank-corr-acct-rec

      /* Транзитный счет     */
      vExch::acct-cr            = vReq::acct-cr    WHEN {assigned vReq::acct-cr}

      /* Данные получателя   */
      vExch::acct-rec           = vReq::acct-rec   

      /* Сумма к зачислению  */
      vExch::amt-rub            = vReq::amt-rub

      /* Отправитель платежа */

      /* Референс документа  */
      vExch::SendREF            = vReq::SendREF


      /* Сохраняем в первоначальной таблице данные импорта */
      vReq::SeanceID            = vExch::SeanceID
      vReq::SendREF             = SUBST("&1|&2", vExch::SendId, vExch::SendREF)

      /* Содержание операции  */
      vExch::details            = ParsDetails(vExch, vExch::details)

      /* Нумерация документов */
      vExch::doc-num            = SetCounterValue(vExch::CounterName, ?, gend-date)

      /* Запускаем маршрутизацию документа */
      vExch::op-kind            = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user)
   NO-ERROR. {&ON-ERROR}



   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatOnlImport", SUBST("SeanceId: &1, SendRef: &2, op-kind: &3",
                                           vExch::SeanceID,
                                           vReq::SendREF,
                                           vExch::op-kind)).
   RUN dbgprint.p ("CPlatOnlImport", SUBST("RunTransaction(&1)", vExch::op-kind)).
   &ENDIF

   /* Создание документа */
   RUN RunTransaction IN h_pbase (vExch::op-kind) NO-ERROR. 
   /* Сохраняем op документа */
   vReq::OpId = vExch::op.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN DumpObject IN h_exch(vExch).
   &ENDIF

   vFlagSet = YES.
   
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatOnlImport", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatOnlImport */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Проверка на уникальность референса документа ==============================================*/
{pfuncdef 
   &DEFPROC="ChkOpRef"
   &DESCRIPTION="Проверка на уникальность референса документа"
   &PARAMETERS="Указатель на документ,список ошибок документа"
   &RESULT="Результат операции"
}
PROCEDURE ChkOpRef:
   DEF INPUT PARAM iWOP   AS HANDLE NO-UNDO.
   DEF INPUT PARAM iList  AS CHAR   NO-UNDO.

DEF VAR vFlagSet  AS  LOG     NO-UNDO INIT ?.
DEF VAR vSendREF  AS  CHAR    NO-UNDO.
DEF VAR vSendID   AS  CHAR    NO-UNDO.
DEF VAR vMailFmt  AS  CHAR    NO-UNDO.

DEF BUFFER bRef  FOR Reference.
DEF BUFFER bPack FOR packet.
DEF BUFFER bCode FOR code.

MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ChkOpRef", "RUN").
   &ENDIF

   ASSIGN
      vSendREF = iWOP::SendREF
      vSendID  = iWOP::SendID
      vMailFmt = iWOP::mail-format
   NO-ERROR. {&NO-ERROR}

   IF EXCH-MSGBuff(vMailFmt, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ChkOpRef", SUBST("vSendREF = &1; vSendID = &2; Op.op = &1; RefClass = &4", 
                                     vSendREF, 
                                     vSendID,
                                     STRING(iWOP::op),
                                     bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /* Проверяем референс без учета дня */
   FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}]
                    AND bRef.RefValue    EQ SUBST("&1|&2", vSendID, vSendREF)
                  NO-LOCK,
      FIRST bPack WHERE bPack.PacketID EQ bRef.PacketID
                  NO-LOCK:

      RUN AddErrorFormat IN h_exch (iWOP,iList).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("ChkOpRef", SUBST("PackDate: &1; PAckTime: &2",
                                        STRING(bPack.PackDate, "99/99/9999"),
                                        STRING(bPack.PackTime, "HH:MM:SS"))).
      &ENDIF

   END. /* FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}] */

   vFlagSet = YES.
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("ChkOpRef", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* ChkOpRef */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Инициализация при импорте строки из реестра ===============================================*/
{pfuncdef 
   &DEFPROC="CPlatReeIni"
   &DESCRIPTION="Инициализация при импорте строки из реестра"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE CPlatReeIni:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vFileName AS  CHAR  NO-UNDO. /* Имя загружаемого файла */
DEF VAR vSeanceId AS  INT64 NO-UNDO. /* Номер сеанса обмена    */
DEF VAR vOpKind   AS  CHAR  NO-UNDO.
DEF VAR vMsg      AS  CHAR  NO-UNDO.

DEF BUFFER bFileExch FOR FileExch.
DEF BUFFER bSeance   FOR Seance.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeIni", "RUN").
   &ENDIF

   IF mFileID NE iExch::FileExchID THEN
   DO:
      /* Контроль на повторную загрузку */
      ASSIGN
         vOpKind   = iExch::op-kind
         mFileID   = INT64(iExch::FileExchID)
         vSeanceId = INT64(iExch::SeanceId)
      NO-ERROR. {&ON-ERROR}

      /* Ищем имя файла */
      FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ mFileID 
                          NO-LOCK:
         vFileName = bFileExch.Name.
      END. /* FOR FIRST bFileExch WHERE bFileExch EQ mFileID  */

      IF {assigned vFileName} THEN
      DO:
         /* Проверяем не был ли загружен файл с тем же именем */
         FOR EACH bFileExch WHERE bFileExch.Name     EQ vFileName 
                              AND bFileExch.SeanceId NE vSeanceId
                            NO-LOCK,
            FIRST bSeance WHERE bSeance.SeanceId EQ bFileExch.SeanceId
                            AND bSeance.op-kind  EQ vOpKind
                          NO-LOCK:
            vMsg = SUBST("Файл '&1' уже загружен, номер сеанса обмена '&2' от '&3'",
                         vFileName,
                         bSeance.Number,
                         STRING(bSeance.SeanceDate,"99/99/9999")).
            RUN Fill-SysMes IN h_tmess("", "", "0", vMsg).
         END. /* FOR EACH bFileExch WHERE bFileExch.Name     EQ vFileName  */
      END. /* IF {assigned vFileName} THEN */
   END. /* IF mFileID NE iExch::FileExchID THEN */

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeIni", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatReeIni */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Обработка строки итогового реестра ========================================================*/
{pfuncdef 
   &DEFPROC="CPlatReeImp"
   &DESCRIPTION="Обработка строки итогового реестра"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE CPlatReeImp:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG     NO-UNDO INIT ?.
DEF VAR vFlagPub   AS  LOG     NO-UNDO INIT NO.
DEF VAR vExchOp    AS  HANDLE  NO-UNDO. /* Указатель на транспортную форму документа из реестра */
DEF VAR vSendREF   AS  CHAR    NO-UNDO. /* Референс документа из реестра                        */
DEF VAR vOpDate    AS  DATE    NO-UNDO. /* Дата документа из реестра                            */
DEF VAR vSendID    AS  CHAR    NO-UNDO. /* Идентификатор отправителя из транзакции              */
DEF VAR vPackId    AS  INT64   NO-UNDO. /* Номер создаваемого пакета                            */
DEF VAR vFilialId  AS  CHAR    NO-UNDO. /* Код подразделения получателя платежа                 */
DEF VAR vCorr      AS  CHAR    NO-UNDO. /* Корр.счет банка получателя платежа                   */
DEF VAR vAcctCr    AS  CHAR    NO-UNDO. /* Транзитный счет межфилиальных рассчетов              */
DEF VAR vSaveFile  AS  LOG     NO-UNDO. /* Сохранять ли обрабатываемые строки                   */
DEF VAR vBuffer    AS  CHAR    NO-UNDO. /* Обрабатываемая строка из файла                       */
DEF VAR vOpKind    AS  CHAR    NO-UNDO. /* Код транзакции для зачисления                        */
DEF VAR vTmpStr    AS  CHAR    NO-UNDO.

DEF BUFFER bRef      FOR Reference.   
DEF BUFFER bPack     FOR packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOp       FOR op.
DEF BUFFER bPackInt  FOR packet.
DEF BUFFER bCode     FOR code.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   ASSIGN
      vTmpStr   = TRNSettingValue("","SaveFile","NO").
      vSaveFile = (vTmpStr EQ "Да") OR (vTmpStr EQ "YES")
   .

   IF vSaveFile THEN
   DO:
      /* Получим загруженную строку и сохраним ее на пакете */
      PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).
      IF vFlagPub EQ YES THEN 
         RUN PacketTextSave IN h_pack(iExch::PacketID, vBuffer + "~n").

   END. /* IF vSaveFile THEN */

   IF GetSysConf("ErrorString") NE "NoError" THEN
   DO:
      IF NOT vSaveFile THEN 
         PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CPlatReeImp", SUBST("ERROR LINE '&1'", vBuffer)).
      &ENDIF
      IF NOT CAN-DO("ИТОГО: кол-во платежей * на сумму * руб.", vBuffer) THEN
         RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("Строка не соответсвут формату: '&1'",
                                                       vBuffer)).

      vFlagSet = YES.
      LEAVE MAIN.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", "ASSIGN VALUES").
   &ENDIF

   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      iExch::DTPay         = TRIM(iExch::DTPay)
      iExch::number        = TRIM(SUBSTR(iExch::number, 1, 20))
      iExch::bank-code-rec = TRIM(iExch::bank-code-rec)
      iExch::abtype        = TRIM(iExch::abtype)
      iExch::amt-rub       = TRIM(iExch::amt-rub)
      iExch::receipt       = TRIM(iExch::receipt)
      iExch::SendID        = TRIM(iExch::SendID)
      vSendREF             = iExch::receipt
      vSendID              = iExch::SendID
      vTmpStr              = ENTRY(1, iExch::DTPay, "T")
      vTmpStr              = SUBST("&1/&2/&3",
                                   ENTRY(3, vTmpStr, "-"),
                                   ENTRY(2, vTmpStr, "-"),
                                   ENTRY(1, vTmpStr, "-"))
      vOpDate              = DATE(vTmpStr)
   NO-ERROR. {&NO-ERROR}

   RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("Платеж номер '&1' от '&2'", 
                                                 vSendREF, 
                                                 STRING(vOpDate))).
   /* Ищем ранее загруженный документ */
   lOpOnlineFnd:
   FOR EACH bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}]
                   AND bRef.RefValue    EQ SUBST("&1|&2", vSendID, vSendREF)
                 NO-LOCK,
      FIRST bPack WHERE bPack.PacketID EQ bRef.PacketID
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketID  EQ bPack.PacketID
                       AND bPackObj.file-name EQ 'op-entry'
                     NO-LOCK,
      FIRST bOp WHERE bOp.op        EQ INT64(ENTRY(1, bPackObj.Surrogate))
                  AND bOp.op-date   EQ vOpDate
                  AND bOp.op-status GE "В"
                NO-LOCK:

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CPlatReeImp", SUBST("FIND op: &1; ref: &2",
                                           STRING(bOp.op),
                                           bRef.RefValue)).
      &ENDIF

      /* Связываем документ с нашим сеансом импорта */
      {pack-crt.i
         &Packet     = bPackInt
         &PacketID   = vPackId
         &SeanceID   = iExch::SeanceID
         &AbonentID  = -1
         &MailUserID = iExch::mail-user-num
         &State      = "{&STATE-CNF}"
         &Kind       = bCode.Misc[{&RKC-KIND}]
         &Format     = iClass
         &ClassCode  = bCode.Misc[{&RKC-CLASS}]
         &ParentID   = iExch::PacketID
      }

      RUN PacketCreateLink IN h_pack(vPackId,
                                     "op-entry",
                                     bPackObj.Surrogate,
                                     "ICyberRee").
      RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("Найден в системе, op.op = &1", 
                                                    STRING(bOp.op))).
      LEAVE lOpOnlineFnd.
   END. /* FOR FIRST bRef WHERE bRef.Class-Code  EQ bCode.Misc[{&RKC-REPLY}] */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", SUBST("FIND op: &1", STRING(vPackId GT 0))).
   &ENDIF

   IF vPackId LE 0 THEN
   DO:
      /* Документ не нашли, нужно создать */
      RUN Fill-SysMes IN h_tmess("", "", "0", "Не найден в системе").

      vExchOp = GetTransObject("EXCHCyber").
      IF NOT VALID-HANDLE(vExchOp) THEN
      DO:
         RUN Fill-SysMes IN h_tmess("", "", "-1", "Ошибка создания документа зачисления.").
         LEAVE MAIN.
      END.
      vExchOp = vExchOp:DEFAULT-BUFFER-HANDLE.

      /*=== Определяем данные подразделения получателя ===*/
      RUN CorrBranch IN THIS-PROCEDURE(iExch::bank-code-rec,
                                       "КиберПл",
                                       gend-date,
                                       OUTPUT vFilialId,
                                       OUTPUT vCorr,
                                       OUTPUT vAcctCr).

      IF {assigned vFilialId} THEN
      DO:
         /*=== Ищем счет получателя в базе ===*/
         {find-act.i &filial = vFilialId
                     &acct   = iExch::number
                     &curr   = "''"
                     &bact   = bAcct
                     &NoFindInNatCurr = YES}
      END. /* IF {assigned vFilialId} THEN */

      IF NOT AVAIL(bAcct) THEN
      DO:
         vTmpStr = SUBST("Счет '&1': не удалось установить получателя платежа.", iExch::number).
         RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
         LEAVE MAIN.

      END. /* IF NOT AVAIL(bAcct) THEN */

      /* Задаем охватывающий пакет */
      vExchOp::PacketId = iExch::PacketID.
      RUN InstanceCreateEx IN h_exch(?, vExchOp, 0).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CPlatReeImp", "ASSIGN VALUES new_op").
      &ENDIF

      /*=== Заполняем недостающие поля в платеже ===*/
      ASSIGN
         /* Банк получателя      */
         vExchOp::bank-code-rec      = iExch::bank-code-rec
         vExchOp::bank-corr-acct-rec = vCorr                 WHEN {assigned vAcctCr}

         /* Транзитный счет      */
         vExchOp::acct-cr            = vAcctCr               WHEN {assigned vAcctCr}

         /* Данные получателя    */
         vExchOp::acct-rec           = bAcct.acct
         vExchOp::acct-rec           = bAcct.number          WHEN {assigned vAcctCr}

         /* Сумма к зачислению   */
         vExchOp::amt-rub            = iExch::amt-rub

         /* Отправитель платежа  */


         /* Референс документа   */
         vExchOp::SendREF            = iExch::receipt

         /* Информация по обмену */
         vExchOp::SeanceID           = iExch::SeanceID
         vExchOp::mail-user-num      = iExch::mail-user-num

         /* Содержание операции  */
         vExchOp::details            = ParsDetails(vExchOp, vExchOp::details)

         /* Нумерация документов */
         vExchOp::doc-num            = SetCounterValue(vExchOp::CounterName, ?, gend-date)
      NO-ERROR. {&NO-ERROR}

      /*=== Проверка правильности присвоения счетчика ===*/
      IF NOT {assigned vExchOp::doc-num} THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","","-1","Ошибка получения значения для счетчика документов").
         LEAVE MAIN.

      END. /* IF NOT {assigned iExch::doc-num} THEN */

      /*=== Маршрутизация документа ===*/
      vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
      IF    vExchOp::op-kind EQ vOpKind   /* Совпадает с текущей                     */
         OR ERROR-STATUS:ERROR          /* Возникли ошибки в функции маршрутизации */
         OR NOT {assigned vOpKind}      /* Код транзакции пуст                     */
      THEN
      DO:
    
         vTmpStr = "Не определена транзакция создания для платежа с номером '&1'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vExchOp::SendREF)).
         LEAVE MAIN.
      END. /* IF    iExch::op-kind EQ vOpKind */
      ELSE
         vExchOp::op-kind = vOpKind.


      /*=== Запускаем транзакцию зачисления ===*/
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("USIBReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
      &ENDIF
      &IF DEFINED(IS-DEBUG) &THEN
      RUN DumpObject IN h_exch(vExchOp).
      &ENDIF

      RUN RunTransaction IN h_pbase (vExchOp::op-kind) NO-ERROR. 

      &IF DEFINED(IS-DEBUG) &THEN
      RUN DumpObject IN h_exch(vExchOp).
      &ENDIF

      /*=== Проверка создания документа ===*/
      IF vExchOp::op EQ "0" THEN
      DO:
         vTmpStr = "Ошибка создания документа с номером '&1'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vExchOp::SendREF)).
         LEAVE MAIN.
      END. /* IF iExch::op EQ "0" THEN */

      RUN Fill-SysMes IN h_tmess("", "", "0", "Создан.").

   END. /* IF vPackId LE 0 THEN */

   RUN Fill-SysMes IN h_tmess("", "", "0", "").
   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeImp", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

IF vFlagSet NE YES THEN
DO:
   RUN Fill-SysMes IN h_tmess("","","-1", "").
   RUN Fill-SysMes IN h_tmess("","","-1", "ОШИБКА: Отмена импорта.").
   RUN Fill-SysMes IN h_tmess("","","-1", "").
END. /* IF vFlagSet NE YES THEN */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatReeImp */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Подсчет зачисленных сумм по реестру =======================================================*/
{pfuncdef 
   &DEFPROC="CPlatReeCalc"
   &DESCRIPTION="Подсчет зачисленных сумм по реестру"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE CPlatReeCalc:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vCount    AS  INT64 NO-UNDO.         /* Количество созданных документов  */
DEF VAR vSum      AS  DEC   NO-UNDO.         /* Общая сумма созданных документов */
DEF VAR vIntFmt   AS  CHAR  NO-UNDO.         /* Выровненный по длинне формат     */
DEF VAR vSumFmt   AS  CHAR  NO-UNDO.         /* Выровненный по длинне формат     */

DEF BUFFER bPacket   FOR Packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOpEntry  FOR op-entry.
DEF BUFFER bCode     FOR code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeCalc", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   /*=== Считаем сумму созданных документов ===*/
   FOR EACH bPacket WHERE bPacket.ParentID   EQ INT64(iExch::PacketID)
                    NO-LOCK,
      EACH bPackObj WHERE bPackObj.PacketID  EQ bPacket.PacketID
                      AND bPackObj.file-name EQ 'op-entry'
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op        EQ INT64(ENTRY(1, bPackObj.Surrogate))
                       AND bOpEntry.op-entry  GE INT64(ENTRY(2, bPackObj.Surrogate))
                       AND bOpEntry.op-status GE "В"
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END. /* FOR EACH bPacket WHERE bPacket.ParentID EQ INT64(iExch::PacketID) */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeCalc", SUBST("Reestr: &3, &4",
                                         STRING(vCount),
                                         STRING(vSum))).
   &ENDIF

   ASSIGN
      vIntFmt = IntFmt(vCount, 0)
      vSumFmt = DecFmt(vSum,   0)
   .

   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего по реестру платежей: &1 на сумму &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0","").

   /* Сохраняем сумму по реестру */
   vFlagSet = UpdateSigns(bCode.Misc[{&RKC-CLASS}], iExch::PacketID, "SummAmt", STRING(vSum), ?).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CPlatReeCalc", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CPlatReeCalc */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Импорт строк реестра Уралсиба =============================================================*/
{pfuncdef 
   &DEFPROC="USIBReeImpDT"
   &DESCRIPTION="Импорт строк реестра Уралсиба"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE USIBReeImpDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vBicMask  AS  CHAR  NO-UNDO.  /* Маска допустимых БИК                                    */
DEF VAR vAcctMask AS  CHAR  NO-UNDO.  /* Маска доступных счетов                                  */
DEF VAR vFilialId AS  CHAR  NO-UNDO.  /* Код подразделения получателя платежа                    */
DEF VAR vCorr     AS  CHAR  NO-UNDO.  /* Корр.счет банка получателя платежа                      */
DEF VAR vAcctCr   AS  CHAR  NO-UNDO.  /* Транзитный счет межфилиальных рассчетов                 */
DEF VAR vOpKind   AS  CHAR  NO-UNDO.  /* Код транзакции для зачисления                           */
DEF VAR vTmpStr   AS  CHAR  NO-UNDO.

DEF BUFFER bAcct     FOR acct.
DEF BUFFER bCode     FOR Code.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iExch::mail-format, BUFFER bCode) NE YES THEN
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", "ASSIGN VALUES").
   &ENDIF

   /*=== Инициализация переменных ===*/
   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      iExch::SendREF       = TRIM(iExch::SendREF)
      iExch::OrderDate     = TRIM(iExch::OrderDate)
      iExch::Amt           = TRIM(iExch::Amt)
      iExch::AmtCom        = TRIM(iExch::AmtCom)
      iExch::OrderAcct     = TRIM(iExch::OrderAcct)
      iExch::bank-code-rec = TRIM(iExch::bank-code-rec)
      iExch::name-rec      = TRIM(iExch::name-rec) 
      iExch::name-send     = TRIM(iExch::name-send)
      iExch::name-rec      = DelDoubleChars(iExch::name-rec,  " ")
      iExch::name-send     = DelDoubleChars(iExch::name-send, " ")
      iExch::tel-send      = TRIM(iExch::tel-send)
      iExch::name-send     = TRIM(iExch::name-send)

      /* Маска допустимых счетов для пополнения  */
      vAcctMask            = TRNSettingValue("","AcctMask","!*")

      /* Маска допустимых БИК банков получателей */
      vBicMask             = TRNSettingValue("","BICMask","!*")
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                         iExch::SendRef, 
                                         iExch::SendId,
                                         bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== Проверка повторной загрузки платежа ===*/
   IF NOT {assigned iExch::SendREF} THEN 
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1", "В реестре есть платежи без уникального номера.").
      LEAVE MAIN.
   END. /* IF NOT {assigned iExch::SendREF} THEN  */

   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendREF),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("Документ уже загружен, уникальный номер: '&1'", 
                                       iExch::SendREF)).
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("USIBReeImpDT", SUBST("Платеж уже загружен. &1",
                                            vTmpStr)).
      &ENDIF
      LEAVE MAIN.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("BIC = &1; BicMask = &2", 
                                         iExch::bank-code-rec, 
                                         vBicMask)).
   RUN dbgprint.p ("USIBReeImpDT", SUBST("Acct = &1; AcctMask = &2", 
                                         iExch::OrderAcct, 
                                         vAcctMask)).
   &ENDIF

   /*=== Проверка БИК банка получателя ===*/
   IF NOT (CAN-DO(vBicMask, iExch::bank-code-rec) AND {assigned iExch::bank-code-rec}) THEN
   DO:
      vTmpStr = SUBST("БИК &1 для счета '&2' не соответствует маске БИК банков получателей.", 
                      iExch::bank-code-rec,
                      iExch::OrderAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END.

   /*=== Проверка счета получателя ===*/
   IF NOT (CAN-DO(vAcctMask, iExch::OrderAcct) AND {assigned iExch::OrderAcct}) THEN
   DO:
      vTmpStr = SUBST("Счет '&1' не соответствует маске допустимых счетов.", iExch::OrderAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END.

   /*=== Определяем данные подразделения получателя ===*/
   RUN CorrBranch IN THIS-PROCEDURE(iExch::bank-code-rec,
                                    "УсибПл",
                                    gend-date,
                                    OUTPUT vFilialId,
                                    OUTPUT vCorr,
                                    OUTPUT vAcctCr).

   IF {assigned vFilialId} THEN
   DO:
      /*=== Ищем счет получателя в базе ===*/
      {find-act.i &filial = vFilialId
                  &acct   = iExch::OrderAcct
                  &curr   = "''"
                  &bact   = bAcct
                  &NoFindInNatCurr = YES}
   END. /* IF {assigned vFilialId} THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   vTmpStr = IF AVAIL(bAcct) THEN bAcct.acct
                             ELSE "not found".
   RUN dbgprint.p ("USIBReeImpDT", SUBST("acct = &1", vTmpStr)).
   &ENDIF                        

   IF AVAIL(bAcct) THEN
   DO:
      /*=== Заполняем недостающие поля в платеже ===*/
      ASSIGN
         /* Банк получателя      */
         iExch::bank-corr-acct-rec = vCorr          WHEN {assigned vAcctCr}

         /* Транзитный счет      */
         iExch::acct-cr            = vAcctCr        WHEN {assigned vAcctCr}

         /* Данные получателя    */
         iExch::acct-rec           = bAcct.acct
         iExch::acct-rec           = bAcct.number   WHEN {assigned vAcctCr}

         /* Сумма к зачислению   */
         iExch::amt-rub            = iExch::Amt
     
         /* Отправитель платежа  */

         /* Содержание операции  */
         iExch::details            = ParsDetails(iExch, iExch::details)

         /* Нумерация документов */
         iExch::doc-num            = SetCounterValue(TRIM(iExch::CounterName), ?, gend-date)
      .

      /*=== Проверка правильности присвоения счетчика ===*/
      IF NOT {assigned iExch::doc-num} THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","","-1","Ошибка получения значения для счетчика документов").
         LEAVE MAIN.

      END. /* IF NOT {assigned iExch::doc-num} THEN */
   END. /* IF AVAIL(bAcct) THEN */
   ELSE 
   DO:
      vTmpStr = SUBST("Счет '&1': не удалось установить получателя платежа.", iExch::OrderAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.

   END. /* IF AVAIL(bAcct) THEN ... ELSE */


   /*=== Маршрутизация документа ===*/
   vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
   IF    iExch::op-kind EQ vOpKind   /* Совпадает с текущей                     */
      OR ERROR-STATUS:ERROR          /* Возникли ошибки в функции маршрутизации */
      OR NOT {assigned vOpKind}      /* Код транзакции пуст                     */
   THEN
   DO:

      vTmpStr = "Не определена транзакция создания для платежа с номером '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF    iExch::op-kind EQ vOpKind */
   ELSE
      iExch::op-kind = vOpKind.

   /*=== Запускаем транзакцию зачисления ===*/
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
   &ENDIF

   RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

   &IF DEFINED(IS-DEBUG) &THEN
   RUN DumpObject IN h_exch(iExch).
   &ENDIF

   /*=== Проверка создания документа ===*/
   IF iExch::op EQ "0" THEN
   DO:
      vTmpStr = "Ошибка создания документа с номером '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF iExch::op EQ "0" THEN */

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* USIBReeImpDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Контроль итоговой строки и зачислений по реестру Уралсиба =================================*/
{pfuncdef 
   &DEFPROC="USIBReeImpFF"
   &DESCRIPTION="Контроль итоговой строки и зачислений по реестру Уралсиба"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE USIBReeImpFF:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vTotCount AS  INT64 NO-UNDO.         /* Количество платежей в реестре    */
DEF VAR vCount    AS  INT64 NO-UNDO.         /* Количество созданных документов  */
DEF VAR vTotSum   AS  DEC   NO-UNDO.         /* Общая сумма перечислений         */
DEF VAR vSum      AS  DEC   NO-UNDO.         /* Общая сумма созданных документов */
DEF VAR vIntFmt   AS  CHAR  NO-UNDO.         /* Выровненный по длинне формат     */
DEF VAR vSumFmt   AS  CHAR  NO-UNDO.         /* Выровненный по длинне формат     */

DEF BUFFER bPack     FOR Packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOpEntry  FOR op-entry.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpFF", "RUN").
   &ENDIF

   ASSIGN
      iExch::TotCount = TRIM(SUBSTR(iExch::TotCount ,2))
      iExch::TotAmt   = TRIM(iExch::TotAmt)
      vTotCount       = DEC(iExch::TotCount)
      vTotSum         = DEC(iExch::TotAmt)
   NO-ERROR. {&NO-ERROR}

   /*=== Считаем сумму и количество созданных документов ===*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketId EQ bPack.PacketId
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op       EQ INT64(ENTRY(1, bPackObj.surrogate))
                       AND bOpEntry.op-ENTRY EQ INT64(ENTRY(2, bPackObj.surrogate))
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END.

   ASSIGN
      vIntFmt = IntFmt(vCount, vTotCount)
      vSumFmt = DecFmt(vSum,   vTotSum)
   .
   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего по реестру платежей: &1 на сумму &2",
                                               STRING(vTotCount, vIntFmt),
                                               STRING(vTotSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего создано документов:  &1 на сумму &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", SUBST("Reestr: &1, &2; Create: &3, &4",
                                          STRING(vTotCount),
                                          STRING(vTotSum),
                                          STRING(vCount),
                                          STRING(vSum))).
   &ENDIF

   IF (vSum EQ vTotSum) AND (vCount EQ vTotCount) THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","0","Расхождений с итоговой строкой реестра нет.").
      vFlagSet = YES.
   END.
   ELSE /* IF (vSum EQ vTotSum) AND (vCount NE vTotCount) THEN ... ELSE */
      RUN Fill-SysMes IN h_tmess("","", "-1","ОШИБКА: Расхождение с итоговой строкой реестра.").

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpFF", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

RUN Fill-SysMes IN h_tmess("","","0","").

{doreturn.i vFlagSet}

END PROCEDURE. /* USIBReeImpFF */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Инициализация первой строки реестра Уралсиба ==============================================*/
{pfuncdef 
   &DEFPROC="USIBReeIniFH"
   &DESCRIPTION="Инициализация первой строки реестра Уралсиба"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE USIBReeIniFH:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeIniFH", "RUN").
   &ENDIF

   /* Импорт основан на crdiucs, без заголовка */
   /* первая строка содежит данные платежа, поэтому порождаем новый объект */
   RUN InstanceCreateEx IN h_exch (?, iExch, 0).
   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeIniFH", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* USIBReeImpFF */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Импорт первой строки реестра Рапиды =======================================================*/
{pfuncdef 
   &DEFPROC="RapidReeImpFH"
   &DESCRIPTION="Импорт первой строки реестра Рапида"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE RapidReeImpFH:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.
DEF VAR vTmpStr   AS  CHAR NO-UNDO.

DEF BUFFER bCode     FOR Code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", "ASSIGN VALUES").
   &ENDIF

   /*=== Инициализация переменных ===*/
   ASSIGN
      iExch::CliSys      = TRIM(iExch::CliSys)
      iExch::ReeRef      = TRIM(iExch::ReeRef)
      iExch::DTReeBeg    = TRIM(iExch::DTReeBeg)
      iExch::DTReeEnd    = TRIM(iExch::DTReeEnd)
      iExch::TotCount    = TRIM(iExch::TotCount)
      iExch::TotAmt      = TRIM(iExch::TotAmt)
      iExch::TotAmtWOCom = TRIM(iExch::TotAmtWOCom)
      iExch::ReeDate     = GetEntries(1, iExch::DTReeBeg, " ", "")
      iExch::ReeTime     = GetEntries(2, iExch::DTReeBeg, " ", "")
      iExch::ReeDate     = SUBST("&1.&2.&3", 
                                 ENTRY(3, iExch::ReeDate,"-"),
                                 ENTRY(2, iExch::ReeDate,"-"),
                                 ENTRY(1, iExch::ReeDate,"-"))
      iExch::SendRef     = iExch::ReeRef
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== Ищем загруженный документ через референс без учета даты загрузки ===*/
   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendRef),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("Файл уже загружен, уникальный номер посылки: '&1'", 
                                       iExch::SendRef)).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpFH", SUBST("Файл уже загружен. &1",
                                             vTmpStr)).
      &ENDIF
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).
      UNDO MAIN, LEAVE MAIN.
   END.

   /*=== Сохраняем ссылку на файл импорта ===*/
   RUN PacketCreateRef IN h_rfrnc (gend-date, 
                                   INT64(TRIM(iExch::PacketId)), 
                                   bCode.Misc[{&RKC-REPLY}],
                                   SUBST("&1|&2",TRIM(iExch::SendId),TRIM(iExch::SendRef))).

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFH", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeImpFH */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Импорт строк с платежами из реестра Рапиды ================================================*/
{pfuncdef 
   &DEFPROC="RapidReeImpDT"
   &DESCRIPTION="Импорт строк с платежами из реестра Рапиды"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE RapidReeImpDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vNonCrit  AS  CHAR  NO-UNDO. /* Маска допустимых ошибок платежей                     */
DEF VAR vBicMask  AS  CHAR  NO-UNDO. /* Маска допустимых БИК                                 */
DEF VAR vAcctMask AS  CHAR  NO-UNDO. /* Маска доступных счетов                               */
DEF VAR vFilialId AS  CHAR  NO-UNDO. /* Код подразделения получателя платежа                 */
DEF VAR vCorr     AS  CHAR  NO-UNDO. /* Корр.счет банка получателя платежа                   */
DEF VAR vAcctCr   AS  CHAR  NO-UNDO. /* Транзитный счет межфилиальных рассчетов              */
DEF VAR vPackId   AS  INT64 NO-UNDO. /* Номер охватывающего пакета                           */
DEF VAR vOpKind   AS  CHAR  NO-UNDO. /* Код транзакции для зачисления                        */
DEF VAR vTmpStr   AS  CHAR  NO-UNDO.

DEF BUFFER bCode     FOR Code.
DEF BUFFER bAcct     FOR acct.
DEF BUFFER bPackInt  FOR packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "ASSIGN VALUES").
   &ENDIF

   /*=== Инициализация переменных ===*/
   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      vPackId              = 0
      iExch::DTPay         = TRIM(iExch::DTPay)
      iExch::SendRef       = TRIM(iExch::SendRef)
      iExch::amt-rub       = TRIM(iExch::amt-rub)
      iExch::OrderAcct     = TRIM(iExch::OrderAcct)
      iExch::bank-code-rec = TRIM(iExch::bank-code-rec)
      iExch::name-rec      = TRIM(iExch::name-rec, " ")
      iExch::name-send     = TRIM(iExch::name-send, " ")
      iExch::name-rec      = DelDoubleChars(iExch::name-rec, " ")
      iExch::name-send     = DelDoubleChars(iExch::name-send, " ")
      iExch::addr-send     = TRIM(iExch::addr-send)

      /* Маска допустимых счетов для пополнения  */
      vBicMask             = TRNSettingValue("","BICMask","!*")

      /* Маска допустимых БИК банков получателей */
      vAcctMask            = TRNSettingValue("","AcctMask","!*")

      /* Маска допустимых ошибок платежей                     */
      vNonCrit             = TRNSettingValue("","NonError","!*")
      vNonCrit             = SUBST("&1,", vNonCrit)
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF                        

   /*=== Проверка повторной загрузки платежа ===*/
   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendRef),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("Платеж уже загружен, уникальный номер: '&1'", 
                                       iExch::SendRef)).
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("RapidReeImpDT", SUBST("Платеж уже загружен. &1",
                                             vTmpStr)).
      &ENDIF
      LEAVE MAIN.
   END. /* IF FndRef(bCode.Misc[{&RKC-REPLY}],  */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("BIC = &1; BicMask = &2", 
                                          iExch::bank-code-rec, 
                                          vBicMask)).
   RUN dbgprint.p ("RapidReeImpDT", SUBST("Acct = &1; AcctMask = &2", 
                                          iExch::OrderAcct,
                                          vAcctMask)).
   &ENDIF

   /* Блок определений параметров платежа. У Рапиды есть возвратный реестр - поэтому не прерываем */
   /* полностью импорт.                                                                           */
   lOpCreate:
   DO:
      /*=== Проверка БИК банка получателя ===*/
      IF NOT (CAN-DO(vBicMask, iExch::bank-code-rec) AND {assigned iExch::bank-code-rec}) THEN
      DO:
         /* Не допустимый БИК банка получателя */
         vTmpStr = SUBST("БИК &1 для счета '&2': не соответствует маске БИК банков получателей.",
                         iExch::bank-code-rec,
                         iExch::OrderAcct).
         RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
         LEAVE lOpCreate.
      END. /* IF NOT (CAN-DO(vBicMask, vBic) AND {assigned vBic}) THEN */

      /*=== Проверка счета получателя ===*/
      IF NOT (CAN-DO(vAcctMask, iExch::OrderAcct) AND {assigned iExch::OrderAcct}) THEN
      DO:
         /* Не допустимый счет получателя */
         vTmpStr = SUBST("Счет '&1' в платеже c номером '&2': не соответствует маске получателей.", 
                         iExch::OrderAcct,
                         iExch::SendRef).
         RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
         LEAVE lOpCreate.
      END.

      /*=== Определяем данные подразделения получателя ===*/
      RUN CorrBranch IN THIS-PROCEDURE(iExch::bank-code-rec,
                                       "РапидаПл",
                                       gend-date,
                                       OUTPUT vFilialId,
                                       OUTPUT vCorr,
                                       OUTPUT vAcctCr).
      
      IF {assigned vFilialId} THEN
      DO:
         /*=== Ищем счет получателя в базе ===*/
         {find-act.i &filial = vFilialId
                     &acct   = iExch::OrderAcct
                     &curr   = "''"
                     &bact   = bAcct
                     &NoFindInNatCurr = YES}
      END. /* IF {assigned vFilialId} THEN */

      &IF DEFINED(IS-DEBUG) &THEN
      vTmpStr = IF AVAIL(bAcct) THEN bAcct.acct
                                ELSE "not found".
      RUN dbgprint.p ("RapidReeImpDT", SUBST("acct = &1", vTmpStr)).
      &ENDIF                        

      IF AVAIL(bAcct) THEN
      DO:
         /*=== Заполняем недостающие поля в платеже ===*/
         ASSIGN
            /* Банк получателя      */
            iExch::bank-corr-acct-rec = vCorr

            /* Транзитный счет      */
            iExch::acct-cr            = vAcctCr               WHEN {assigned vAcctCr}

            /* Данные получателя    */
            iExch::FilialId           = bAcct.filial-id
            iExch::acct-rec           = bAcct.acct
            iExch::acct-rec           = bAcct.number          WHEN {assigned vAcctCr}

            /* Сумма к зачислению   */

            /* Отправитель платежа  */

            /* Содержание операции  */
            iExch::details            = ParsDetails(iExch, iExch::details)

            /* Нумерация документов */
            iExch::doc-num            = SetCounterValue(iExch::CounterName, ?, gend-date)
         NO-ERROR. {&NO-ERROR}

         /*=== Проверка правильности присвоения счетчика ===*/
         IF NOT {assigned iExch::doc-num} THEN
         DO:
            RUN Fill-SysMes IN h_tmess("","","-1","Ошибка получения значения для счетчика документов").
            LEAVE MAIN.
         
         END. /* IF NOT {assigned iExch::doc-num} THEN */

         /*=== Маршрутизация документа ===*/
         vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
         IF    iExch::op-kind EQ vOpKind   /* Совпадает с текущей                     */
            OR ERROR-STATUS:ERROR          /* Возникли ошибки в функции маршрутизации */
            OR NOT {assigned vOpKind}      /* Код транзакции пуст                     */
         THEN
         DO:
       
            vTmpStr = "Не определена транзакция создания для платежа с номером '&1'".
            RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
            LEAVE MAIN.
         END. /* IF    iExch::op-kind EQ vOpKind */
         ELSE
            iExch::op-kind = vOpKind.

      END. /* IF AVAIL(bAcct) THEN */
   END. /* lOpCreate */

   /*=== Проверка стандартных ошибок Рапиды ===*/
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "RUN RapidaChkErr").
   &ENDIF                        

   RUN RapidaChkErr IN THIS-PROCEDURE(iExch).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("RapidaErr: &1", iExch::RapidaErr)).
   &ENDIF                        

   IF {assigned iExch::RapidaErr} THEN
   DO:
      /* При импорте реестра есть ошибки */
      IF NOT {assigned iExch::SendRef} THEN
         vTmpStr = SUBST("от '&1' на сумму &2", iExch::DTPay, iExch::amt-rub).
      ELSE
         vTmpStr = SUBST("номер '&1'", iExch::SendRef).

      vTmpStr = SUBST("Платеж &1 ошибка: &2",
                      vTmpStr, 
                      GetCodeNameEx("ОшбкРапида", iExch::RapidaErr, iExch::RapidaErr)).
      RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

   END. /* IF {assigned iExch::RapidaErr} THEN */

   IF CAN-DO(vNonCrit, iExch::RapidaErr) THEN
   DO:
      /*=== Запускаем транзакцию зачисления ===*/
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("RapidReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
      &ENDIF

      /* Запускаем транзакцию зачисления */
      RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

      &IF DEFINED(IS-DEBUG) &THEN
      RUN DumpObject IN h_exch(iExch).
      &ENDIF

      IF iExch::op EQ "0" THEN
      DO:
         vTmpStr = "Ошибка создания документа с номером '&1'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendRef)).
         UNDO MAIN, LEAVE MAIN.
      END.

      /* Ищем связанный с импортируемым файлом пакет */
      FOR FIRST bPackObj WHERE bPackObj.SeanceId  EQ     iExch::SeanceId
                           AND bPackObj.file-name EQ     "op-entry"
                           AND bPackObj.surrogate BEGINS SUBST("&1,", iExch::op)
                         NO-LOCK:
         vPackId = bPackObj.PacketId.
      END. /* FOR FIRST bPackObj WHERE bPackObj.SeanceId  EQ     iExch::SeanceId */

   END. /* IF CAN-DO(NonCrit, iExch::RapidaErr) THEN */
   ELSE
   DO:
      /* Строка не подлежит зачислению   */
      /* Создаем пакет для строки данных */
      {pack-crt.i
         &Packet     = bPackInt
         &PacketID   = vPackId
         &SeanceID   = iExch::SeanceID
         &AbonentID  = -1
         &MailUserID = iExch::mail-user-num
         &State      = "{&STATE-IMP}"
         &Kind       = bCode.Misc[{&RKC-KIND}]
         &Format     = iClass
         &ClassCode  = bCode.Misc[{&RKC-CLASS}]
         &ParentID   = iExch::PacketID
      }

   END. /* IF CAN-DO(NonCrit, iExch::RapidaErr) THEN ... ELSE */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", SUBST("PacketID = '&1'", STRING(vPackId))).
   &ENDIF

   IF vPackId NE 0 THEN
   DO:
      /* Сохраняем данные из строки */
      RUN SaveAttrRapida IN THIS-PROCEDURE (iExch, bCode.Misc[{&RKC-CLASS}], vPackId).
   END.
   ELSE
   DO:
      vTmpStr = "Ошибка сохранения реквизитов документа с номером '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendRef)).
      UNDO MAIN, LEAVE MAIN.
   END.

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeImpDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Импорт хвостовика реестра Рапиды ==========================================================*/
{pfuncdef 
   &DEFPROC="RapidReeImpFF"
   &DESCRIPTION="Импорт хвостовика реестра Рапиды"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE RapidReeImpFF:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG   NO-UNDO INIT ?.
DEF VAR vTotCount  AS  INT64 NO-UNDO. /* Количество платежей в реестре    */
DEF VAR vCount     AS  INT64 NO-UNDO. /* Количество созданных документов  */
DEF VAR vTotSum    AS  DEC   NO-UNDO. /* Общая сумма перечислений         */
DEF VAR vSum       AS  DEC   NO-UNDO. /* Общая сумма созданных документов */
DEF VAR vIntFmt    AS  CHAR  NO-UNDO. /* Выровненный по длинне формат     */
DEF VAR vSumFmt    AS  CHAR  NO-UNDO. /* Выровненный по длинне формат     */
DEF VAR vReestrErr AS  CHAR  NO-UNDO. /* Ошибка реестре                   */
DEF VAR vPayErr    AS  CHAR  NO-UNDO. /* Ошибка платежа                   */
DEF VAR vNonCrit   AS  CHAR  NO-UNDO. /* Список не критических ошибок     */
DEF VAR vTmpStr    AS  CHAR  NO-UNDO.
DEF VAR vTmpAmt    AS  DEC   NO-UNDO.

DEF BUFFER bPack     FOR packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bFileExch FOR FileExch.
DEF BUFFER bOpEntry  FOR op-entry.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", "RUN").
   &ENDIF

   ASSIGN
      vTotCount = INT64(GetDecVal(iExch::TotCount, 0))
      vTotSum   = GetDecVal(iExch::TotAmt, 0)

      /* Маска допустимых ошибок платежей */
      vNonCrit  = SUBST("&1,", TRNSettingValue("","NonError","!*"))
   .

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", "Counting values").
   &ENDIF

   /*=== Считаем сумму и количество  по всем строкам реестра ==============================================*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                 NO-LOCK:

      ASSIGN
         vPayErr    = GetXAttrValueEx("Packet", STRING(bPack.PacketId), "RapidaErr", "")
         vTmpStr    = GetXAttrValueEx("Packet", STRING(bPack.PacketId), "amt-rub", "0")
         vCount     = vCount + 1
         vTmpAmt    = GetDecVal(vTmpStr, 0)
         vSum       = vSum + vTmpAmt
      .

      /* 0200 - критическая (возврат всего реестра), остальные ошибки платежей информационные */
      IF CAN-DO("0200", vPayErr) THEN
         vReestrErr = vPayErr.

   END. /* FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId) */

   /*=== Сверяем количество обработанного с итоговой строкой реестра ===*/
   ASSIGN
      /*=== 0300 Количество строк, в заголовке, не соответствует количеству строк Реестра =======*/
      vReestrErr = "0300"                 WHEN vCount NE vTotCount
      /*=== 0400 Сумма, указанная в первой строке, не соответствует сумме всех строк Реестра ====*/
      vReestrErr = "0400"                 WHEN vSum   NE vTotSum
   .

   /*=== Сохраняем итоговые данные ===*/
   FOR FIRST bPack WHERE bPack.PacketId EQ INT64(iExch::PacketId)
                   NO-LOCK:
      /* Сохраним данные по реестру */
      UpdateSigns(bPack.Class-code, iExch::PacketId, "CliSys",   iExch::CliSys,   ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "ReeRef",   iExch::ReeRef,   ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "TotCount", iExch::TotCount, ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "TotAmt",   iExch::TotAmt,   ?).
      UpdateSigns(bPack.Class-code, iExch::PacketId, "ReeError", vReestrErr,      ?).

   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", SUBST("Reestr: &1, &2; Packet: &3, &4",
                                          STRING(vTotCount),
                                          STRING(vTotSum),
                                          STRING(vCount),
                                          STRING(vSum))).
   RUN dbgprint.p ("RapidReeImpFF", SUBST("RapidaErr: '&1'; NonCritical: '&2'", 
                                          vReestrErr, 
                                          vNonCrit)).
   &ENDIF

   IF {assigned vReestrErr} THEN
   DO:
      /*=== Критическая ошибка по реестру (возврат всего реестра) ===*/
      vTmpStr = "".
      FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ INT64(iExch::FileExchID)
                          NO-LOCK:
         vTmpStr = SUBST(" (файл '&1')", bFileExch.name).

      END. /* FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ INT64(iExch::FileExchID) */

      vTmpStr = SUBST("ОШИБКА! Реестр номер '&2'&1: &3",
                      vTmpStr, 
                      iExch::ReeRef,
                      GetCodeNameEx("ОшбкРапида", vReestrErr, vReestrErr)).

      RUN Fill-SysMes IN h_tmess("","","0", "").
      RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

      /*=== Удалим все созданные документы ===*/
      FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                     NO-LOCK,
         EACH bPackObj WHERE bPackObj.PacketId  EQ     bPack.PacketId
                         AND bPackObj.file-name BEGINS "op"
                       NO-LOCK:
         RUN OperationDelete IN THIS-PROCEDURE(INT64(ENTRY(1, bPackObj.surrogate))) NO-ERROR.

      END. /* FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId) */
   END. /* IF {assigned vReestrErr} THEN */

   ASSIGN
      vIntFmt = IntFmt(vCount, vTotCount)
      vSumFmt = DecFmt(vSum,   vTotSum)
   .

   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего по реестру платежей: &1 на сумму &2",
                                               STRING(vTotCount, vIntFmt),
                                               STRING(vTotSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего обработано платежей: &1 на сумму &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).
   ASSIGN
      vSum   = 0
      vCount = 0
   .

   /*=== Считаем сумму и количество созданных документов ===*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketId EQ bPack.PacketId
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op       EQ INT64(ENTRY(1, bPackObj.surrogate))
                       AND bOpEntry.op-ENTRY EQ INT64(ENTRY(2, bPackObj.surrogate))
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END.

   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего создано документов:  &1 на сумму &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", SUBST("Create: &1, &2", STRING(vCount), STRING(vSum))).
   &ENDIF

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeImpFF", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeImpFF */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Создание возвратного реестра для Рапиды ===================================================*/
{pfuncdef 
   &DEFPROC="RapidReeOutDT"
   &DESCRIPTION="Создание возвратного реестра для Рапиды"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE RapidReeOutDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet   AS  LOG    NO-UNDO INIT ?.
DEF VAR vFileName  AS  CHAR   NO-UNDO. /* Имя загруженного файла                    */
DEF VAR vImportId  AS  INT64  NO-UNDO. /* Сеанс импорта реестра из Рапиды           */
DEF VAR vBankRef   AS  CHAR   NO-UNDO. /* Уникальный код Участника Системы в НКО    */
DEF VAR vReeRef    AS  CHAR   NO-UNDO. /* Уникальный номер реестра НКО              */
DEF VAR vCount     AS  INT64  NO-UNDO. /* Количество записей, вошедших в реестр     */
DEF VAR vAmt       AS  DEC    NO-UNDO. /* Общая сумма непринятых переводов в рублях */
DEF VAR vReeErr    AS  CHAR   NO-UNDO. /* Ошибка зачисления всего реестра           */
DEF VAR vPayErr    AS  CHAR   NO-UNDO. /* Ошибка зачисления платежа                 */
DEF VAR vNonCrit   AS  CHAR   NO-UNDO. /* Список не критических ошибок     */
DEF VAR vPackId    AS  INT64  NO-UNDO. /* Номер создаваемого пакета                 */
DEF VAR vPayId     AS  CHAR   NO-UNDO. 
DEF VAR vTmpStr    AS  CHAR   NO-UNDO. 

DEF BUFFER bCode     FOR Code.
DEF BUFFER bPack     FOR packet.
DEF BUFFER bPay      FOR packet.
DEF BUFFER bErrPay   FOR ttErrPay.
DEF BUFFER bFileExch FOR FileExch.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeOutDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iExch::mail-format, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   ASSIGN
      /* Маска допустимых ошибок платежей */
      vNonCrit  = SUBST("&1,", TRNSettingValue("","NonError","!*"))

      vImportId = INT64(TRNSettingValue("","ImpSeanceID","0"))

   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeOutDT", SUBST("ImpSeance: &1, NonCritical: &2", 
                                          vImportId,
                                          vNonCrit)).
   &ENDIF

   /*=== Перебираем загруженные файлы реестров в переданном сеансе ===*/
   FOR EACH bPack WHERE bPack.SeanceId EQ vImportId
                    AND bPack.ParentId EQ 0
                  NO-LOCK:

      ASSIGN
         vPayId   = STRING(bPack.PacketId)
         vBankRef = GetXAttrValueEx("Packet", vPayId, "CliSys",   "")
         vReeRef  = GetXAttrValueEx("Packet", vPayId, "ReeRef",   "")
         vReeErr  = GetXAttrValueEx("Packet", vPayId, "ReeError", "")
         vCount   = 0
         vAmt     = 0
         vFileName= ""
      .
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("RapidReeOutDT", SUBST("PacketId: &1, ReeRef: &2, RapidaErr: &3", 
                                             vPayId,
                                             vReeRef,
                                             vReeErr)).
      &ENDIF

      FOR FIRST bFileExch WHERE bFileExch.FileExchID EQ bPack.FileExchID
                          NO-LOCK:
         vFileName = SUBST("Файл &1:", bFileExch.Name).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("RapidReeOutDT", SUBST("FileName: '&1'", bFileExch.Name)). 
         &ENDIF

      END. /* FOR FIRST bFileExch WHERE bFileExch EQ mFileID  */

      {empty bErrPay}

      IF LENGTH(vReeRef) NE 8 THEN
      DO:
         vTmpStr = "&1Уникальный номер реестра '&2' не соответсвуте формату 'YYYYMMDD'".
         RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vFileName, vReeRef)).
         UNDO MAIN, LEAVE MAIN.
      END.

      /* Перебираем ошибочные строки */
      lPayFnd:
      FOR EACH bPay WHERE bPay.SeanceId EQ bPack.SeanceId
                      AND bPay.ParentId EQ bPack.PacketId
                    NO-LOCK:

         vPayId = STRING(bPay.PacketId).

         /*=== Если на реестре есть ошибка, то возвратные формируем по всем строкам ===*/
         IF NOT {assigned vReeErr} THEN
         DO:
            vPayErr = GetXAttrValueEx("Packet", vPayId, "RapidaErr", "").

            /*=== Не критические ошибки нас не интересуют ===*/
            IF CAN-DO(vNonCrit, vPayErr) THEN
               NEXT lPayFnd.

         END. /* IF NOT {assigned vReeErr} THEN */

         /* Таблица с ошибочными платежами */
         CREATE bErrPay.
         ASSIGN
            /* Код ошибки */
            bErrPay.PayErr    = vPayErr   WHEN NOT {assigned vReeErr}
            bErrPay.PayErr    = vReeErr   WHEN {assigned vReeErr}

            /* Уникальные номер платежа */
            bErrPay.PayRef    = GetXAttrValueEx("Packet", vPayId, "SendRef", "")

            /* Счет получателя платежа */
            bErrPay.OrderAcct = GetXAttrValueEx("Packet", vPayId, "OrderAcct", "")

            /* Сумма платежа */
            vTmpStr           = GetXAttrValueEx("Packet", vPayId, "amt-rub", "0")
            bErrPay.amt-rub   = GetDecVal(vTmpStr, 0)

            /* Подсчет итогов */
            vCount            = vCount + 1
            vAmt              = vAmt   + bErrPay.amt-rub
         .
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("RapidReeOutDT", SUBST("PayId: &1, PayRef: &2, amt-rub: &3, PayErr: &4", 
                                                vPayId,
                                                bErrPay.PayRef,
                                                STRING(bErrPay.amt-rub),
                                                bErrPay.PayErr)).
         &ENDIF
      END. /* lPayFnd: FOR EACH bPay WHERE bPay.SeanceId EQ bPack.SeanceId */

      /* Есть данные для выгрузки */
      IF vCount NE 0 THEN
      DO:
         /* Создаем пакет */
         RUN PacketCreateF IN h_pack(INPUT  INT64(iExch::SeanceID),
                                     BUFFER bCode,
                                     OUTPUT vPackId).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("RapidReeOutDT", SUBST("CREATE PACKET: &1", STRING(vPackId))).
         &ENDIF

         IF NOT (vPackId GT 0) THEN
         DO:
            vTmpStr = "Ошибка создание пакета экспорта для реестра '&1' по записи '&2'".
            RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, vReeRef, bErrPay.PayRef)).
            UNDO MAIN, LEAVE MAIN.
         END.

         /*=== Сохраняем заголовок возвратного реестра ===*/
         vTmpStr = SUBST("sum;&1;&2;&3;&4~n",
                         vBankRef,
                         vReeRef,
                         STRING(vCount),
                         TRIM(STRING(vAmt, DecFmt(vAmt, 0)))).
         RUN PacketTextSave IN h_pack(vPackId, vTmpStr).

         FOR EACH bErrPay NO-LOCK:

            /*=== Сохраняем строки с возвратами ===*/
            vTmpStr = SUBST("pay;&1;&2;&3;&4~n",
                            bErrPay.PayRef,
                            TRIM(STRING(bErrPay.amt-rub, DecFmt(bErrPay.amt-rub, 0))),
                            bErrPay.OrderAcct,
                            bErrPay.PayErr).
            RUN PacketTextSave IN h_pack(vPackId, vTmpStr).

         END. /* FOR EACH bErrPay NO-LOCK: */

         /*=== Сохраняем имя файла ===*/
         vTmpStr = SUBST("reestr&1.&2.&3.txt",
                         SUBSTR(vReeRef, 1, 4),
                         SUBSTR(vReeRef, 5, 2),
                         SUBSTR(vReeRef, 7, 2)).
         UpdateSigns(bCode.Misc[{&RKC-CLASS}], STRING(vPackId), "FileName", vTmpStr, ?).

         RUN Fill-SysMes IN h_tmess("","","0","").
         RUN Fill-SysMes IN h_tmess("","","0",SUBST("Сформирован возвратный реестр: &1", vTmpStr)).
         RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего по возвратному реестру: &1 на сумму &2", 
                                                    STRING(vCount),
                                                    STRING(vAmt, DecFmt(vAmt, 0)))).

         RUN Fill-SysMes IN h_tmess("","","0","").
      END. /* IF vCount NE 0 THEN */
      
   END. /* FOR EACH bPack WHERE bPack.SeanceId EQ vImpSeance */
   
   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("RapidReeOutDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* RapidReeOutDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Импорт первой строки реестра Золотой Короны ===============================================*/
{pfuncdef 
   &DEFPROC="CRownReeImpFH"
   &DESCRIPTION="Импорт первой строки реестра Золотой Короны"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE CRownReeImpFH:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.
DEF VAR vTmpStr   AS  CHAR NO-UNDO.

DEF BUFFER bCode     FOR Code.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", "ASSIGN VALUES").
   &ENDIF

   /*=== Инициализация переменных ===*/
   ASSIGN
      iExch::ReeNom      = TRIM(iExch::ReeNom)
      iExch::ReeAmt      = TRIM(iExch::ReeAmt)
      iExch::ReePeny     = TRIM(iExch::ReePeny)
      iExch::ReeHoldAmt  = TRIM(iExch::ReeHoldAmt)
      iExch::ReePayAmt   = TRIM(iExch::ReePayAmt)
      iExch::ReeCountTot = TRIM(iExch::ReeCountTot)
      iExch::ReeAgent    = TRIM(iExch::ReeAgent)
      iExch::ReeNomUsl   = TRIM(iExch::ReeNomUsl)
      iExch::ReeDate     = TRIM(iExch::ReeDate)
      iExch::ReeBegDate  = TRIM(iExch::ReeBegDate)
      iExch::ReeEndDate  = TRIM(iExch::ReeEndDate)
      iExch::ReePrim     = TRIM(iExch::ReePrim)
      iExch::SendId      = TRIM(iExch::SendId)
      iExch::SendRef     = iExch::ReeNom
      iExch::ReeDate     = ENTRY(1, iExch::ReeDate, " ")
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== Ищем загруженный документ через референс без учета даты загрузки ===*/
   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendRef),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("Файл уже загружен, уникальный номер посылки: '&1'", 
                                       iExch::SendRef)).
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpFH", SUBST("Файл уже загружен. &1",
                                             vTmpStr)).
      &ENDIF
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).
      UNDO MAIN, LEAVE MAIN.
   END.

   /*=== Сохраняем ссылку на файл импорта ===*/
   RUN PacketCreateRef IN h_rfrnc (gend-date, 
                                   INT64(TRIM(iExch::PacketId)), 
                                   bCode.Misc[{&RKC-REPLY}],
                                   SUBST("&1|&2",TRIM(iExch::SendId),TRIM(iExch::SendRef))).

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFH", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CRownReeImpFH */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Импорт строк с платежами реестра Золотой Короны ===========================================*/
{pfuncdef 
   &DEFPROC="CRownReeImpDT"
   &DESCRIPTION="Импорт строк с платежами реестра Золотой Короны"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE CRownReeImpDT:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG  NO-UNDO INIT ?.
DEF VAR vBicMask  AS  CHAR NO-UNDO.          /* Маска допустимых БИК                             */
DEF VAR vAcctMask AS  CHAR NO-UNDO.          /* Маска доступных счетов                           */
DEF VAR vDopInfo  AS  CHAR NO-UNDO EXTENT 8. /* Дополнительные поля в сообщении                  */
DEF VAR vAcct     AS  CHAR NO-UNDO.          /* Счет получателя                                  */
DEF VAR vCurr     AS  CHAR NO-UNDO.          /* Валюта счета получателя                          */
DEF VAR vBIC      AS  CHAR NO-UNDO.          /* БИК банка получателя                             */
DEF VAR vFilialId AS  CHAR NO-UNDO.          /* Код подразделения получателя платежа             */
DEF VAR vCorr     AS  CHAR NO-UNDO.          /* Корр.счет банка получателя платежа               */
DEF VAR vAcctCr   AS  CHAR NO-UNDO.          /* Транзитный счет межфилиальных рассчетов          */
DEF VAR vOpKind   AS  CHAR NO-UNDO.          /* Код транзакции для зачисления                    */
DEF VAR vTmpStr   AS  CHAR NO-UNDO.

DEF BUFFER bCode  FOR Code.
DEF BUFFER bCard  FOR loan.
DEF BUFFER bAcct  FOR acct.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", "RUN").
   &ENDIF

   IF EXCH-MSGBuff(iClass, BUFFER bCode) NE YES THEN 
      UNDO MAIN, LEAVE MAIN.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", "ASSIGN VALUES").
   &ENDIF

   /*=== Инициализация переменных ===*/
   ASSIGN
      vOpKind        = TRIM(iExch::op-kind)
      iExch::FIOCli  = TRIM(iExch::FIOCli)
      iExch::AddrCli = TRIM(iExch::AddrCli)
      iExch::AcctCli = TRIM(iExch::AcctCli)
      iExch::Amt     = TRIM(iExch::Amt)
      iExch::DopInfo = TRIM(iExch::DopInfo)
      iExch::SendRef = TRIM(iExch::SendRef)
      iExch::Peny    = TRIM(iExch::Peny)
      vDopInfo[1]    = GetEntries(1, iExch::DopInfo, ":", "") /* Доп.код РНКО                    */
      vDopInfo[2]    = GetEntries(2, iExch::DopInfo, ":", "") /* Счет получателя                 */
      vDopInfo[3]    = GetEntries(3, iExch::DopInfo, ":", "") /* ФИО получателя                  */
      vDopInfo[4]    = GetEntries(4, iExch::DopInfo, ":", "") /* ФИО плательщика                 */
      vDopInfo[5]    = GetEntries(5, iExch::DopInfo, ":", "") /* ???                             */
      vDopInfo[6]    = GetEntries(6, iExch::DopInfo, ":", "") /* Адрес плательщика               */
      vDopInfo[7]    = GetEntries(7, iExch::DopInfo, ":", "") /* Номер телефона плательщика      */
      vDopInfo[8]    = GetEntries(8, iExch::DopInfo, ":", "") /* БИК банка получателя            */
      vBIC           = vDopInfo[8]

      /* Маска допустимых счетов для пополнения  */
      vBicMask       = TRNSettingValue("","BICMask","!*")

      /* Маска допустимых БИК банков получателей */
      vAcctMask      = TRNSettingValue("","AcctMask","!*")
   NO-ERROR. {&NO-ERROR}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", SUBST("SendREF = &1; SendID = &2; RefClass = &3",
                                          iExch::SendRef, 
                                          iExch::SendId,
                                          bCode.Misc[{&RKC-REPLY}])).
   &ENDIF

   /*=== Проверка повторной загрузки платежа ===*/
   IF NOT {assigned iExch::SendREF} THEN 
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1", "В реестре есть платежи без уникального номера.").
      LEAVE MAIN.
   END. /* IF NOT {assigned iExch::SendREF} THEN  */

   IF FndRef(bCode.Misc[{&RKC-REPLY}], 
             SUBST("&1|&2", iExch::SendId, iExch::SendREF),
             OUTPUT vTmpStr)
   THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 SUBST("Платеж уже загружен, уникальный номер: '&1'", 
                                       iExch::SendREF)).
      IF {assigned vTmpStr} THEN
         RUN Fill-SysMes IN h_tmess("","","-1", vTmpStr).

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpFH", SUBST("Платеж уже загружен. &1",
                                             vTmpStr)).
      &ENDIF
      LEAVE MAIN.
   END. /* IF FndRef(bCode.Misc[{&RKC-REPLY}],  */

   /*=== Определяем счет, т.к. может передаваться только номер карты ===*/
   IF LENGTH(iExch::AcctCli) EQ 16 THEN
   DO:
      /* 16 символов, получен номер карты */
      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("CRownReeImpDT", SUBST("CardNum = &1",
                                             iExch::AcctCli)).
      &ENDIF
      FOR FIRST bCard WHERE bCard.contract EQ "card"
                        AND bCard.doc-num  EQ iExch::AcctCli
                      NO-LOCK:
         /* Нашли карту */
         /* По filial-id найдем БИК банка*/
         vBIC = GetXAttrValueEx("branch", bCard.filial-id, "БанкМФО", "").
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CRownReeImpDT", SUBST("contract = '&1'; cont-code = '&2'",
                                                bCard.contract,
                                                bCard.cont-code)).
         RUN dbgprint.p ("CRownReeImpDT", SUBST("parent-contract = '&1'; parent-cont-code = '&2'",
                                                bCard.parent-contract,
                                                bCard.parent-cont-code)).
         &ENDIF

         /* Ищем счет СКС */
         RUN GetRoleAcct IN h_card (SUBST("&1,&2", bCard.parent-contract, bCard.parent-cont-code),
                                    gend-date,
                                    "SCS",
                                    "",
                                    OUTPUT vAcct,
                                    OUTPUT vCurr).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CRownReeImpDT", SUBST("Acct = &1; Curr = &2",
                                                vAcct,
                                                vCurr)).
         &ENDIF
         /* Переводы в на валютные счета не принимаем */
         IF {assigned vCurr} THEN
            ASSIGN
               vAcct = ""
               vCurr = ""
               vBIC  = ""
            .
      END. /* FOR FIRST bCard WHERE bCard.contract EQ "card" */
   END. /* IF LENGTH(iExch::AcctCli) EQ 16 THEN */
   ELSE IF LENGTH(iExch::AcctCli) EQ 20 THEN
   DO:
      /* 20 символов, получен номер счета */
      vAcct = iExch::AcctCli.

   END. /* ELSE IF LENGTH(iExch::AcctCli) EQ 20 THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", SUBST("BIC = &1; BicMask = &2",   vBic,  vBicMask)).
   RUN dbgprint.p ("CRownReeImpDT", SUBST("Acct = &1; AcctMask = &2", vAcct, vAcctMask)).
   &ENDIF

   /*=== Проверка БИК банка получателя ===*/
   IF NOT (CAN-DO(vBicMask, vBic) AND {assigned vBic}) THEN
   DO:
      /* Не допустимый БИК банка получателя */
      vTmpStr = SUBST("БИК &1 для счета '&2': не соответствует маске БИК банков получателей.", 
                      vBic,
                      vAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END. /* IF NOT (CAN-DO(vBicMask, vBic) AND {assigned vBic}) THEN */

   /*=== Проверка счета получателя ===*/
   IF NOT (CAN-DO(vAcctMask, vAcct) AND {assigned vAcct}) THEN
   DO:
      /* Не допустимый счет получателя */
      vTmpStr = SUBST("Счет '&1' в платеже c номером '&2': не соответствует маске получателей.", 
                      vAcct,
                      iExch::SendRef).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.
   END.

   /*=== Определяем данные подразделения получателя ===*/
   RUN CorrBranch IN THIS-PROCEDURE(vBic,
                                    "ЗолКорПл",
                                    gend-date,
                                    OUTPUT vFilialId,
                                    OUTPUT vCorr,
                                    OUTPUT vAcctCr).

   IF {assigned vFilialId} THEN
   DO:
      /*=== Ищем счет получателя в базе ===*/
      {find-act.i &filial = vFilialId
                  &acct   = vAcct
                  &curr   = "''"
                  &bact   = bAcct
                  &NoFindInNatCurr = YES}
   END. /* IF {assigned vFilialId} THEN */

   &IF DEFINED(IS-DEBUG) &THEN
   vTmpStr = IF AVAIL(bAcct) THEN bAcct.acct
                             ELSE "not found".
   RUN dbgprint.p ("CRownReeImpDT", SUBST("acct = &1", vTmpStr)).
   &ENDIF                        

   IF AVAIL(bAcct) THEN
   DO:
      /*=== Заполняем недостающие поля в платеже ===*/
      ASSIGN
         /* Банк получателя      */
         iExch::bank-code-rec      = vBic                  
         iExch::bank-corr-acct-rec = vCorr                 WHEN {assigned vAcctCr}

         /* Транзитный счет      */
         iExch::acct-cr            = vAcctCr               WHEN {assigned vAcctCr}

         /* Данные получателя    */
         iExch::acct-rec           = bAcct.acct
         iExch::acct-rec           = bAcct.number          WHEN {assigned vAcctCr}
         iExch::name-rec           = DelDoubleChars(iExch::FIOCli, " ")

         /* Сумма к зачислению   */
         iExch::amt-rub            = iExch::Amt

         /* Отправитель платежа  */
         iExch::name-send          = DelDoubleChars(vDopInfo[4], " ")

         /* Содержание операции  */
         iExch::details            = ParsDetails(iExch, iExch::details)

         /* Нумерация документов */
         iExch::doc-num            = SetCounterValue(iExch::CounterName, ?, gend-date)
      NO-ERROR. {&NO-ERROR}

      /*=== Проверка правильности присвоения счетчика ===*/
      IF NOT {assigned iExch::doc-num} THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","","-1","Ошибка получения значения для счетчика документов").
         LEAVE MAIN.

      END. /* IF NOT {assigned iExch::doc-num} THEN */
   END. /* IF AVAIL(bAcct) THEN */
   ELSE 
   DO:
      vTmpStr = SUBST("Счет '&1': не удалось установить получателя платежа.", vAcct).
      RUN Fill-SysMes IN h_tmess("", "", "-1", vTmpStr).
      LEAVE MAIN.

   END. /* IF AVAIL(bAcct) THEN ... ELSE */

   /*=== Маршрутизация документа ===*/
   vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
   IF    iExch::op-kind EQ vOpKind   /* Совпадает с текущей                     */
      OR ERROR-STATUS:ERROR          /* Возникли ошибки в функции маршрутизации */
      OR NOT {assigned vOpKind}      /* Код транзакции пуст                     */
   THEN
   DO:

      vTmpStr = "Не определена транзакция создания для платежа с номером '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF    iExch::op-kind EQ vOpKind */
   ELSE
      iExch::op-kind = vOpKind.

   /*=== Запускаем транзакцию зачисления ===*/
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("USIBReeImpDT", SUBST("RunTransaction(&1)", iExch::op-kind)).
   &ENDIF

   RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

   &IF DEFINED(IS-DEBUG) &THEN
   RUN DumpObject IN h_exch(iExch).
   &ENDIF

   /*=== Проверка создания документа ===*/
   IF iExch::op EQ "0" THEN
   DO:
      vTmpStr = "Ошибка создания документа с номером '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::SendREF)).
      LEAVE MAIN.
   END. /* IF iExch::op EQ "0" THEN */

   vFlagSet = YES.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpDT", "Done").
   &ENDIF
END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. /* CRownReeImpDT */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== Хвостовик: подсчет итоговых данных реестра ================================================*/
{pfuncdef 
   &DEFPROC="CRownReeImpFF"
   &DESCRIPTION="Импорт строк с платежами реестра Золотой Короны"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}
PROCEDURE CRownReeImpFF:
   DEF INPUT PARAM iClass AS  CHAR    NO-UNDO.
   DEF INPUT PARAM iExch  AS  HANDLE  NO-UNDO.

DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vTotCount AS  INT64 NO-UNDO.         /* Количество платежей в реестре    */
DEF VAR vCount    AS  INT64 NO-UNDO.         /* Количество созданных документов  */
DEF VAR vTotSum   AS  DEC   NO-UNDO.         /* Общая сумма перечислений         */
DEF VAR vSum      AS  DEC   NO-UNDO.         /* Общая сумма созданных документов */
DEF VAR vIntFmt   AS  CHAR  NO-UNDO.         /* Выровненный по длинне формат     */
DEF VAR vSumFmt   AS  CHAR  NO-UNDO.         /* Выровненный по длинне формат     */

DEF BUFFER bPack     FOR Packet.
DEF BUFFER bPackObj  FOR PackObject.
DEF BUFFER bOpEntry  FOR op-entry.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", "RUN").
   &ENDIF

   ASSIGN
      vTotCount = INT64(iExch::ReeCountTot)
      vTotSum   = DEC(iExch::ReePayAmt)
   NO-ERROR. {&NO-ERROR}

   /*=== Считаем сумму и количество созданных документов ===*/
   FOR EACH bPack WHERE bPack.ParentId EQ INT64(iExch::PacketId)
                  NO-LOCK,
      FIRST bPackObj WHERE bPackObj.PacketId EQ bPack.PacketId
                     NO-LOCK,
      FIRST bOpEntry WHERE bOpEntry.op       EQ INT64(ENTRY(1, bPackObj.surrogate))
                       AND bOpEntry.op-ENTRY EQ INT64(ENTRY(2, bPackObj.surrogate))
                     NO-LOCK:
      ASSIGN
         vSum   = vSum   + bOpEntry.amt-rub
         vCount = vCount + 1
      .
   END.

   ASSIGN
      vIntFmt = IntFmt(vCount, vTotCount)
      vSumFmt = DecFmt(vSum,   vTotSum)
   .
   RUN Fill-SysMes IN h_tmess("","","0","").
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего по реестру платежей: &1 на сумму &2",
                                               STRING(vTotCount, vIntFmt),
                                               STRING(vTotSum,   vSumFmt))).
   RUN Fill-SysMes IN h_tmess("","","0",SUBST("Всего создано документов:  &1 на сумму &2",
                                              STRING(vCount, vIntFmt),
                                              STRING(vSum,   vSumFmt))).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", SUBST("Reestr: &1, &2; Create: &3, &4",
                                          STRING(vTotCount),
                                          STRING(vTotSum),
                                          STRING(vCount),
                                          STRING(vSum))).
   &ENDIF

   IF (vSum EQ vTotSum) AND (vCount EQ vTotCount) THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","0","Расхождений с итоговой строкой реестра нет.").
      vFlagSet = YES.
   END.
   ELSE /* IF (vSum EQ vTotSum) AND (vCount NE vTotCount) THEN ... ELSE */
      RUN Fill-SysMes IN h_tmess("","", "-1","ОШИБКА: Расхождение с итоговой строкой реестра.").

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CRownReeImpFF", "Done").
   &ENDIF

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

RUN Fill-SysMes IN h_tmess("","","0","").

{doreturn.i vFlagSet}

END PROCEDURE. /* CRownReeImpFF */

/*===============================================================================================*/