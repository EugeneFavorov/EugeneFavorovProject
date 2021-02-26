{globals.i}
{intrface.get tmess}

/* +++ pp-op.p was humbly modified by (c)blodd converter v.1.09 on 1/19/2017 8:26am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: PP-OP.P
      Comment: (0046989) Содержит методы для новой
               системы валидации
   Parameters: Нет
         Uses:
      Used by:
      Created: 06.09.2005 13:04 KSV     
     Modified: 19.09.2005 19:19 KSV      (0046989) Содержит методы для новой
                                         системы валидации
     Modified: 22.09.2005 12:51 KSV      (0046989) Изменены вызовы FastCache.
     Modified: 23.09.2005 15:14 KSV      (0046989) Незначительные исправления
     Modified: 30.09.2005 14:46 KSV      (0046989) Незначительное исправление
     Modified: 06.10.2005 13:04 KSV      (0051030) Синхронизация
     Modified: 20.10.2005 11:08 KSV      (0046989) Исправлена ошибка с
                                         обработкой несуществующего кассового
                                         символа.
     Modified: 31.10.2005 13:21 KSV      (0046989) Исправлена ошибка в
                                         процедуре v__method__check, связанная
                                         с обработкой результатов работы метода
                                         chkupd.
     Modified: 22.11.2005 11:32 KSV      (0046989) Исправлена ошибка в
                                         процедуре CheckBalance.
     Modified: 16.01.2006 14:02 KSV      (0046989) Исправлена ошибка в
                                         процедуре CheckAcctOE с глобальными
                                         переменными mVL_Params и mVL_Values.
     Modified: 15/03/2006 kraw (0059228) Умолчание на ДР "ВалКонтр" изменен с "нет" на "да"
     Modified: 10.10.2006 14:02 DEMA     (0069051) Проверка красного сальдо при
                                         ежедневной загрузки данных
     Modified: 22/10/2007 kraw (0083371) ослабить валидацию для ручной постановки на картотеку 2 
                                         во избежание тавтологии
     Modified: 02/11/2007 kraw (0083151) Темпоральные кассовые символы.
     Modified: 22/10/2007 kraw (0086643) то же самое, что и 0083371, но для картотеки 1. 
                                         Т. е. ослабили валидацию для обеих картотек (1 и 2)
     Modified: 11.03.2008 muta 0088286   Возможность ввести  документы постановки на Картотеку 1 по блокированному счету                                  
     Modified: 01.04.2008 muta 0087973   CheckAcctOE: Отключение проверки на блокировку если статус документа <= "П" и
                                         настроечный параметр "ОтклБлокПров" = ДА;
     Modified: 29/04/2008 kraw (0091871) на картотеку ставим не только при помощи g-crd1
     Modified: 29/04/2008 kraw (0094516) Исключаем использование gend-date при проверке кассового символа
     Modified: 03/04/2009 kraw (0100898) "У клиента есть документы на КБС"
     Modified: 12/10/2009 kraw (0110638) CheckAcctOE: Отключение проверки на блокировку если статус документа <= "П" и
                                         настроечный параметр "ОтклБлокПров" = ДА для БлокДб
     Modified: 10/02/2010 kraw (0123642) автоматическое списание с картотеки и для банков-клиентов тоже
     Modified: 11/06/2010 kraw (0129433) можно анулировать документ с нулевой суммой.
     Modified: 20/05/2011 kraw (0145067) Используем GetBlkType
     Modified: 24/10/2011 kraw (0153781) анализируем ДР AcctNoPos на op-kind
     Modified: 17/11/2015 IGOR Bсправлена реакция системы на нажатие Esc. При нажатии Esc сообщение о блокировках и картотеках
                               остается на экране, пока не будет нажата клавиша Enter.
                
*/
{globals.i}                                 /* глобальные переменные         */

{intrface.get valid}
{intrface.get cache}
{intrface.get strng}    /* Библиотека для работы со строками. */
{intrface.get tmcod}
{intrface.get blkob}
{intrface.get separate} 
{intrface.get hist}
{intrface.get ovl}

{pp-uni.var &FILE_sword_p=YES}   /* определение переменных        */
{pp-uni.prg}                     /* описание стандартных процедур */
{mes_over.i}
{op.def}
{op.pro}

{bal-corr.def}

{fms-chkdoc.i &nofmsprint=yes}

{pfuncdef
 &DefLib="op" 
 &Description="Содержит методы для новой системы валидации"}

DEFINE VARIABLE mStrClass AS CHAR NO-UNDO.
DEFINE VARIABLE mStrClMsg AS CHAR NO-UNDO.
ASSIGN
   mStrClass = FGetSetting("ОверКлассТранз","ОверКлДляГрТранз","")
   mStrClMsg = FGetSetting("ГНИ", "СбщОтклПров108н","")
.
{pfuncdef
   &DEFPROC     = "DocImpUfebs"
   &DESCRIPTION = "Был ли документ импортирован в рамках УФЭБС?"
   &PARAMETERS  = "iOp - код документа (op.op)"
   &RESULT      = "Логическое значение"
   &SAMPLE      = "DocImpUfebs(vOp)"}
FUNCTION DocImpUfebs RETURN LOGICAL PRIVATE (
   INPUT iOp AS INt64
):
   DEF BUFFER PackObject   FOR PackObject.   /* Локализация буфера. */
   DEFINE BUFFER Packet     FOR Packet.
   DEF BUFFER Seance       FOR Seance.       /* Локализация буфера. */
   DEF BUFFER op           FOR op.           /* Локализация буфера. */

   BLCK_GetFirstOp:
            FOR EACH PackObject WHERE
            PackObject.file-name EQ       "op-entry"
      AND   PackObject.Surrogate BEGINS   STRING(iOp) + ","
                     NO-LOCK,
                EACH Packet WHERE
                     Packet.PacketID    EQ PackObject.PacketID
      AND   CAN-DO(mStrClMsg, Packet.class-code) 
                     NO-LOCK,
                EACH Seance WHERE
                     Seance.DIRECT EQ "Импорт"
      AND   Seance.SeanceID   EQ Packet.SeanceID
   NO-LOCK,
   FIRST op WHERE
            op.op EQ iop
      AND   CAN-DO("ibsp*,i-ed*,i-trnree", op.op-kind)
   NO-LOCK:
      LEAVE BLCK_GetFirstOp.
            END.

   RETURN AVAIL op.
END FUNCTION.


/*------------------------------------------------------------------------------
  Purpose:     Возвращает структуру объекта OP для валидатора
  Parameters:  iObject     - хэндл на контейнер объекта
               oStructure  - возвращаемая структура
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE s__op: 
   {&VPD_S_PARAM}

   oStructure = "op-entry" + {&ST_DELIM2} + "*" + {&ST_DELIM2} + "*" + 
                {&ST_DELIM2} + "for each op-entry where op-entry.op = " +
                QUOTER(iObject:BUFFER-FIELD("op"):BUFFER-VALUE).
   
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка рублевой суммы проводки
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE va__op-entry__amt-rub:
   {&VPD_VA_PARAM}

   DEFINE VARIABLE vValue AS DECIMAL    NO-UNDO.

   IF      iObject:BUFFER-FIELD("acct-cat"):BUFFER-VALUE EQ "d"
       OR (iObject:BUFFER-FIELD("qty"):BUFFER-VALUE      NE 0
      AND  iObject:BUFFER-FIELD("amt-rub"):BUFFER-VALUE  EQ 0) THEN
      vValue = iObject:BUFFER-FIELD("qty"):BUFFER-VALUE NO-ERROR.
   ELSE
      vValue = DEC(iValue) NO-ERROR.

   IF iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE BEGINS "А" THEN
      RETURN.

   IF iObject:BUFFER-FIELD("amt-cur"):BUFFER-VALUE NE 0 AND
      iObject:BUFFER-FIELD("amt-rub"):BUFFER-VALUE EQ 0 AND
      ROUND(CurToBase ("Учетный", 
                 iObject:BUFFER-FIELD("currency"):BUFFER-VALUE,
                 iObject:BUFFER-FIELD("op-date"):BUFFER-VALUE,
                 iObject:BUFFER-FIELD("amt-cur"):BUFFER-VALUE),
      iObject:HANDLE:BUFFER-FIELD("amt-rub"):DECIMALS) EQ 0
   THEN
      vValue = 1.

   IF vValue < 0 THEN       
      {except.i &Exception = "(IF FGetSetting('СтандТр','ОтрицСуммы','ДА') = 'Да' ~
                               THEN '__core01'                                    ~
                               ELSE '__core53')"}.

   IF vValue = 0 THEN {except.i &Exception = "'__core02'"}.

   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка валютной суммы проводки
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE va__op-entry__amt-cur:
   {&VPD_VA_PARAM}

   DEFINE VARIABLE vValue AS DECIMAL  NO-UNDO.
   DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cCr    AS CHARACTER NO-UNDO.

   vValue = DEC(iValue) NO-ERROR.

   IF iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE BEGINS "А" THEN
      RETURN.

   IF vValue < 0 THEN
      {except.i &Exception = "(IF FGetSetting('СтандТр','ОтрицСуммы','ДА') = 'Да' ~
                               THEN '__core01'                                    ~
                               ELSE '__core53')"}.
   /* Плюс банк: нулевая сумма в валютной проводке */
   IF     (vValue EQ 0)
      AND (iObject:BUFFER-FIELD("currency"):BUFFER-VALUE NE "")
   THEN DO:
      FIND FIRST op
         WHERE (op.op EQ INT64(iObject:BUFFER-FIELD("op"):BUFFER-VALUE))
         NO-LOCK NO-ERROR.
      IF CAN-DO("!*Переоценка*,!*Курсовая разница*,*", op.details)
      THEN DO:
         cDb = "*".
         cCr = "*".
         FOR EACH code
            WHERE (code.class   EQ 'TstCur0')
              AND (code.parent  EQ 'TstCur0')
            NO-LOCK:

            IF (code.name EQ "Дб")
            THEN cDb = "!" + code.val + " *," + cDb.
            ELSE cCr = "!" + code.val + " *," + cCr.
         END.

         IF     CAN-DO(cDb, iObject:BUFFER-FIELD("acct-db"):BUFFER-VALUE)
            AND CAN-DO(cCr, iObject:BUFFER-FIELD("acct-cr"):BUFFER-VALUE)
         THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1", "Нулевая сумма в валютной проводке!").
            RETURN 'ERROR'.
         END.
      END.
   END.
   /* === */

   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка валюты проводки
  Parameters:  Oписание параметров см. в valid.def
  Notes: 
------------------------------------------------------------------------------*/
PROCEDURE va__op-entry__currency:
   {&VPD_VA_PARAM}

   DEFINE BUFFER bcurrency FOR currency.

   IF iValue NE "" AND 
      iValue NE "{&in-NC-Code}" THEN
   DO:
      /* Commented by KSV: Проверяем, что в данный момент не вводится курс.
         SHARE-LOCK bcurrency обязателен */
      FIND FIRST bcurrency WHERE bcurrency.currency = iValue
            SHARE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED bcurrency THEN 
         {except.i 
            &Exception = "'__core03'"
            &OtherInfo = "'2'"}
      RELEASE bcurrency.
   END.
   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка заключительных оборотов
  Parameters:  Oписание параметров см. в valid.def
  Notes: 
------------------------------------------------------------------------------*/
PROCEDURE va__op-entry__prev-year:
   {&VPD_VA_PARAM}

   DEFINE VARIABLE vOpDate    AS DATE       NO-UNDO.
   DEFINE VARIABLE vAcctCat   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE dt-zo      AS DATE       NO-UNDO.

   ASSIGN
      vOpDate  = iObject:BUFFER-FIELD("op-date"):BUFFER-VALUE
      vAcctCat = iObject:BUFFER-FIELD("acct-cat"):BUFFER-VALUE.

   CASE vAcctCat:
      WHEN "n" THEN
          dt-zo = IF data-zo-nu NE "" 
                  THEN DATE(INT64 (SUBSTR(data-zo-nu,4,2)),
                            INT64 (SUBSTR(data-zo-nu,1,2)),
                            YEAR(vOpdate))
                  ELSE DATE(3,31,YEAR(vOpdate)).
      OTHERWISE   
         dt-zo = IF data-zo NE "" 
                 THEN DATE(INT64 (SUBSTR(data-zo,4,2)),
                           INT64 (SUBSTR(data-zo,1,2)),
                           YEAR(vOpdate))
                 ELSE DATE(3,31,YEAR(vOpdate)).
   END CASE.

   IF iValue = "YES" THEN
   DO:
      IF vOpdate > dt-zo THEN
      DO:
         mVL_Except = "__core05(%s=ДатаЗО" + 
                      ( IF vAcctCat = "n" THEN "ну" ELSE  "") + ")".
         {except.i &OtherInfo = "'2'"}
      END.
      ELSE 
         IF zaklobor EQ "Нет" THEN 
            {except.i 
               &Exception = "'__core06'"
               &OtherInfo = "'2'"}.
   END.

   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка счета дебета проводки
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE va__op-entry__acct-db:
   {&VPD_VA_PARAM}

   IF iValue = ? THEN RETURN .
   
   RUN CheckAcct (iObject,iValue,"db",OUTPUT oOtherInfo).
   IF {assigned RETURN-VALUE} THEN RETURN {&RETURN_VALUE}.
   
   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка счета кредита проводки
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE va__op-entry__acct-cr:
   {&VPD_VA_PARAM}
   
   IF iValue = ? THEN RETURN .
   
   RUN CheckAcct (iObject,iValue,"cr",OUTPUT oOtherInfo).
   IF {assigned RETURN-VALUE} THEN RETURN {&RETURN_VALUE}.
   
   RETURN .
END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose:     Проверка корреспонденции счетов в проводке
  Parameters:  Oписание параметров см. в valid.def
  Notes: 
------------------------------------------------------------------------------*/
FUNCTION getBalAcct RETURN CHARACTER (INPUT iStr AS CHARACTER):
    RETURN SUBSTRING(iStr, 1, 5).
END FUNCTION.

FUNCTION getBalAcctList RETURN CHARACTER (INPUT iStr AS CHARACTER):
    RETURN ENTRY(1, iStr, ";").
END FUNCTION.

PROCEDURE vg__op-entry__acct-db__acct-cr:
   {&VPD_VG_PARAM}

   DEFINE BUFFER dacct FOR acct.
   DEFINE BUFFER cacct FOR acct.

   DEFINE VARIABLE vDAcct AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCAcct AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vOpDate LIKE op.op-date NO-UNDO.
   DEFINE VARIABLE vOpKind LIKE op.op-kind NO-UNDO.

   DEFINE VARIABLE vAllow AS LOGICAL NO-UNDO.
   
   ASSIGN
      vDAcct = iObject:BUFFER-FIELD("acct-db"):BUFFER-VALUE
      vCAcct = iObject:BUFFER-FIELD("acct-cr"):BUFFER-VALUE.

   /* Commented by KSV: Проверка корреспонденции внебалансовых счетов */
   IF iObject:BUFFER-FIELD("acct-cat"):BUFFER-VALUE = 'o' THEN 
   DO:
      {find-act.i
         &bact = dacct
         &acct = vDAcct
      }

      {find-act.i
         &bact = cacct
         &acct = vCAcct
      }
      IF {out-bal2.cor dacct cacct settA settP} THEN 
      DO:
         mVL_Except = "__core04(%s=" + vDAcct + "%s=" + vCAcct + ")".
         {except.i &OtherInfo = "'2'"}
      END.
         
   END.

   /* Commented by KSV: Счета в проводке должны быть разными */
   
   IF FGetSetting('СтандТр','КонтБалСчетов','ДА') NE 'Нет' THEN
      IF vDAcct = vCAcct THEN 
         {except.i 
            &Exception = "'__core07'"
            &OtherInfo = "'2'"}.

   IF VALID-HANDLE(iUpObject) THEN
      ASSIGN
         vOpDate = iUpObject:BUFFER-FIELD("op-date"):BUFFER-VALUE
         vOpKind = iUpObject:BUFFER-FIELD("op-kind"):BUFFER-VALUE
      .
   ELSE DO:
      FIND FIRST op
      WHERE
         op.op = iObject:BUFFER-FIELD("op"):BUFFER-VALUE
      NO-LOCK NO-ERROR.
      ASSIGN
         vOpDate = op.op-date
         vOpKind = op.op-kind
      .
   END.

   /* Проверка корреспонденции счетов */
   IF FGetSetting("СтандТр", "РазрешКоррСчетов", "Нет") = "Да" AND
      GetXAttrValueEx("op-kind",
                      vOpKind,
                      "РазрешКоррСчетов",
                      "Нет") = "Да" AND
      GetLstParam({&VP_CORRACCT_CONTROL},
                  iParams,
                  iValues,
                  {&VP_PARAM_DLM}) <> "NO" THEN DO:
      IF AVAIL(op) THEN DO:
         FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
         {empty Info-Store} 
         RUN Collection-Info.
         RUN for-pay("ДЕБЕТ,ПЛАТЕЛЬЩИК,БАНКПЛ,БАНКГО,БАНКФИЛ",
                     "ПП",
                     OUTPUT PlName[1],
                     OUTPUT PlLAcct,
                     OUTPUT PlRKC[1],
                     OUTPUT PlCAcct,
                     OUTPUT PlMFO).
         RUN for-rec("КРЕДИТ,ПОЛУЧАТЕЛЬ,БАНКПОЛ,БАНКГО,БАНКФИЛ",
                     "ПП",
                     OUTPUT PoName[1],
                     OUTPUT PoAcct,
                     OUTPUT PoRKC[1],
                     OUTPUT PoCAcct,
                     OUTPUT PoMFO).
      END.  

      RUN CheckAcctCorr(vDAcct, vCAcct, PlLAcct, PoAcct, vOpDate, OUTPUT vAllow).
      IF NOT vAllow THEN DO:
         mVL_Except = "__core58(%s=" + vDAcct + "%s=" + vCAcct + ")".
         {except.i &OtherInfo = "'2'"}
      END.

   END.


   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка кассового символа и корреспонденции счетов в проводке
  Parameters:  Oписание параметров см. в valid.def
  Notes: 
------------------------------------------------------------------------------*/
PROCEDURE vg__op-entry__acct-db__acct-cr__symbol:
   {&VPD_VG_PARAM}

   DEFINE BUFFER dacct  FOR acct.
   DEFINE BUFFER cacct  FOR acct.
   DEFINE BUFFER symbol FOR symbol.

   DEFINE VARIABLE vDAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctCat   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSymbol    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmp       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSymbPr    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOpDate    AS CHARACTER  NO-UNDO.

   ASSIGN
      vDAcct   = iObject:BUFFER-FIELD("acct-db"):BUFFER-VALUE
      vCAcct   = iObject:BUFFER-FIELD("acct-cr"):BUFFER-VALUE
      vAcctCat = iObject:BUFFER-FIELD("acct-cat"):BUFFER-VALUE
      vSymbol  = iObject:BUFFER-FIELD("symbol"):BUFFER-VALUE
      vOpDate  = iObject:BUFFER-FIELD("op-date"):BUFFER-VALUE
      .

      {find-act.i
         &bact = dacct
         &acct = vDAcct
      }

      {find-act.i
         &bact = cacct
         &acct = vCAcct
      }
      FIND FIRST op WHERE op.op EQ iObject:BUFFER-FIELD("op"):BUFFER-VALUE
      NO-LOCK NO-ERROR.
      
      IF vAcctCat = "b" AND vSymbol <> "" AND 
         NOT GetFastCache(PROGRAM-NAME(1),
                          {unknown vDAcct} + 
                          {unknown vCAcct} + 
                          {unknown vSymbol},OUTPUT vTmp) THEN
      DO:
         vSymbPr = getTCodeFld("val", "КасСимволы", vSymbol, DATE(vOpDate)).

         IF vSymbPr EQ ? THEN
         DO:
             mVL_Except = "__core19(%s=" + {unknown vSymbol} + ")".
            {except.i &OtherInfo = "'2'"}.
         END.

         IF (
             vDAcct         <> ?       AND 
             dacct.contract  = "Касса" AND 
             vSymbPr         = "Расх"  AND 
             (vCAcct = ? OR (vCAcct <> ? AND cacct.contract   <> "Касса"))
            ) 
            OR 
            (
             vCAcct         <> ?       AND 
             cacct.contract  = "Касса" AND 
             vSymbPr         = "Прих"  AND 
             (vDAcct = ? OR (vDAcct <> ? AND dacct.contract <> "Касса"))
            ) 
            THEN {except.i &Exception = "'__core29'"}.
         RUN SetFastCache IN h_cache (PROGRAM-NAME(1),
                                      {unknown vDAcct} + 
                                      {unknown vCAcct} + 
                                      {unknown vSymbol},"").
      END.
      IF AVAIL cAcct AND cAcct.cust-cat EQ "Ч" THEN DO:
         RUN chkpersdoc.p (op.op-kind,
                           cAcct.cust-cat,
                           cAcct.cust-id,
                           op.op-date,
                           "").
         pick-value = "NO".
         IF {&RETURN_VALUE} EQ "YES" THEN
         RETURN "NO".
      END.
      IF AVAIL dAcct AND dAcct.cust-cat EQ "Ч" THEN DO:
         RUN chkpersdoc.p (op.op-kind,
                           dAcct.cust-cat,
                           dAcct.cust-id,
                           op.op-date,
                           "").
         pick-value = "NO".
         IF {&RETURN_VALUE} EQ "YES" THEN
         RETURN "NO".
      END.

   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Дополнительная проверка документа
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE vg__op__op:
   {&VPD_VG_PARAM}
   DEFINE VARIABLE vOpHandle AS HANDLE   NO-UNDO.
   DEFINE VARIABLE vAcctChk    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAttrChk    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcctChkStr AS CHARACTER INIT "acct-rec,acct-send,op.ben-acct" NO-UNDO.
   DEFINE VARIABLE vAttrChkStr AS CHARACTER INIT "ДовЛицо,Плательщик,Получатель,ПредПлательщика,ПредПолучателя" NO-UNDO.
   DEFINE VARIABLE vI          AS INTEGER   NO-UNDO.

   DEFINE BUFFER bf_chkacct FOR acct.
      
   
   DO vI = 1 TO NUM-ENTRIES(vAcctChkStr):
      IF INDEX(ENTRY(vI,vAcctChkStr),".") NE 0 THEN DO:         
         vAcctChk = iObject:buffer-FIELD(ENTRY(2,ENTRY(vI,vAcctChkStr),".")):BUFFER-VALUE.
      END.
      ELSE 
         vAcctChk = GetXattrValueEx("op",STRING(iObject:buffer-FIELD("op"):BUFFER-VALUE),ENTRY(vI,vAcctChkStr),"").

      IF {assigned vAcctChk} THEN DO:
         {find-act.i &bact = bf_chkacct
                     &acct  = vAcctChk
         }

         IF AVAIL bf_chkacct AND bf_chkacct.cust-cat EQ "Ч" THEN DO:
            RUN chkpersdoc.p (iObject:buffer-FIELD("op-kind"):BUFFER-VALUE,
                              bf_chkacct.cust-cat,
                              bf_chkacct.cust-id,
                              iObject:buffer-FIELD("op-date"):BUFFER-VALUE,
                              "").
         IF {&RETURN_VALUE} EQ "YES" THEN
            RETURN "NO".
         END.
      END.
   END.  
   DO vI = 1 TO NUM-ENTRIES(vAttrChkStr):
      vAttrChk = GetXattrValueEx("op",STRING(iObject:buffer-FIELD("op"):BUFFER-VALUE),ENTRY(vI,vAttrChkStr),"").

      IF NUM-ENTRIES(vAttrChk) EQ 2 THEN DO:
         IF ENTRY(1,vAttrChk) NE "Ч" THEN 
            NEXT.
         IF TRIM(ENTRY(2,vAttrChk),"0123456789") NE "" THEN
            NEXT.
         RUN chkpersdoc.p (iObject:buffer-FIELD("op-kind"):BUFFER-VALUE,
                           "Ч",
                           INT64(ENTRY(2,vAttrChk)),
                           iObject:buffer-FIELD("op-date"):BUFFER-VALUE,
                           "").
         IF {&RETURN_VALUE} EQ "YES" THEN
            RETURN "NO".
      END.
   END.

   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка статуса документа
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE va__op__op-status:
   {&VPD_VA_PARAM}
   DEF VAR mStatus      AS CHAR   NO-UNDO.   /* для хранения статуса */
   DEF VAR mClass       AS CHAR   NO-UNDO.   /* класс объекта */
   DEF VAR mSM_Init     AS CHAR   NO-UNDO.
   DEF VAR mStatModOn   AS LOG    NO-UNDO.
   DEF VAR mErr         AS LOG    NO-UNDO.

   DEF BUFFER yxstatus FOR xstatus.

   mStatus = iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE.       /* получаем статус документа */
   mClass  = iObject:BUFFER-FIELD("class-code"):BUFFER-VALUE.

            /* Определяем, включен ли механизм обработки модели состояний на классе */
   mSM_Init   = GetXattrEx(mClass,"StatModelOn","Initial").
   mStatModOn = {assigned mSM_Init} AND mSM_Init EQ ENTRY(1,GetXattrEx(mClass,"StatModelOn","Data-Format"),"~/").

   IF mStatModOn THEN
   DO:
      RUN GetXstatus(mClass,mStatus,BUFFER yxstatus).
      IF NOT AVAIL yxstatus THEN mErr = YES.
   END.
   ELSE
      /* ищем полученный статус в базе, если статус не найден, то  */
      IF NOT AvailCode ("Статус",mStatus) THEN mErr = YES.

   IF mErr
   THEN DO:
            /* генерируем исключение и прерываем выполнение */
      mVL_Except = "Статус " + mStatus + " данного документа отсутствует в классификаторе ""Статус"".".
      {except.i OtheInfo = "'2'"}
   END.
   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Процедура проверки счета в проводке
  Parameters:  iObject     - хэндл на контейнер объекта
               iAcct       - номер счета
               iSide       - позиция счета в проводке
                              db - для дебета
                              cr - для кредита 
               oOtherInfo  - дополнительная иформация для возврата валидатору
  Notes:       ПРОЦЕДУРА ИСПОЛЬЗУЕТ КЭШИРОВАНИЕ. Будьте ВНИМАТЕЛЬНЫ 
               при изменении
------------------------------------------------------------------------------*/
PROCEDURE CheckAcct PRIVATE:
   DEFINE INPUT  PARAMETER iObject     AS HANDLE     NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSide       AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOtherInfo  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vAcctCat    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOpdate     AS DATE       NO-UNDO.
   DEFINE VARIABLE vTmp        AS CHARACTER  NO-UNDO.

   {alowacop.i &def_var = YES}

   DEFINE BUFFER bAcct FOR acct.

   ASSIGN
      vAcctCat    = iObject:BUFFER-FIELD("acct-cat"):BUFFER-VALUE
      vOpdate     = iObject:BUFFER-FIELD("op-date"):BUFFER-VALUE
      .

   /* Commented by KSV: Ищем счет без валюты!!! В момент ввода счета в экранную
   ** форму поле валюта проводки может быть не заполнено, однако счет проверить
   ** все-таки необходимо. Если в системе все-таки будут разрешены одинаковые 
   ** счета для разных валют, то структура op-entry д.б. изменена, т.к. текущая
   ** op-entry этого не позволит */
   {find-act.i
      &bact   = bAcct
      &acct   = iAcct
      &lockac = "SHARE-LOCK NO-WAIT"
   }   
   
   IF LOCKED bAcct THEN
   DO:
      {find-act.i
         &bact   = bAcct
         &acct   = iAcct
      }
      vTmp = "(%s=acct" + "%s=" + iAcct + "%s=".
      IF AVAIL bAcct THEN
         Wholocks2(RECID(bAcct),"Acct",INPUT-OUTPUT vTmp).
      mVL_Except = "__comm09" + vTmp + ")".
      {except.i &OtherInfo = "'2'"}
   END.

   IF NOT AVAILABLE bAcct THEN
   DO:
      mVL_Except = ( IF vAcctCat <> "d" THEN "__core08" ELSE "__core09") +
                   "(%s=" + iAcct + "%s=)".
      {except.i &OtherInfo = "'2'"}
   END.
      
      
   IF bAcct.close-date NE ? THEN 
   DO:
      mVL_Except = "__core10(%s=" + ClueAcctCurr(iAcct,bAcct.Currency) + ")".
      {except.i &OtherInfo = "'2'"}
   END.
      
   
   IF bAcct.open-date > vOpdate THEN
   DO:
      mVL_Except = "__core11(%t=" + STRING(vOpdate) +
                    "%t=" + STRING(bAcct.open-date) +
                    "%s=" + ClueAcctCurr(iAcct,bAcct.currency) + ")".
      {except.i &OtherInfo = "'2'"}
   END.

   IF bAcct.acct-cat NE vAcctCat THEN
   DO:
      mVL_Except = "__core13(%s=" + bAcct.acct-cat +
                    "%s=" + GetCodeName("acct-cat",bAcct.acct-cat) +
                    "%s=" + ClueAcctCurr(iAcct,bAcct.currency) +
                    "%s=" + vAcctCat + 
                    "%s=" + GetCodeName("acct-cat",vAcctCat) + ")".
      {except.i &OtherInfo = "'2'"}
   END.

   IF NOT CAN-DO(getThisUserXAttrValue("ClassAcct" + iSide),
                 bAcct.class-code) 
      AND NOT (iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE BEGINS "А") THEN 
   DO:
      mVL_Except = "__core14(%s=" + ( IF iSide = "db" THEN "дебетовать" ELSE "кредитовать") +
                   "%s=" + bAcct.class-code + ")".
      {except.i &OtherInfo = "'2'"}
   END.

   {alowacop.i
      &condition  = YES
      &side       = iSide
      &bufacct    = bAcct
   }

   IF vMessNum <> "" THEN DO:
     CASE vMessNum:
       WHEN "Core15" THEN
       mVL_Except =  "__core15(%s=" + ( IF iSide = "db" THEN "дебетовать" ELSE "кредитовать") +
                       "%s=" + bAcct.branch-id +
                       "%s=" + bAcct.acct + ")".
       WHEN "Core15_1" THEN
       mVL_Except =  "__core15_1(%s=" + ( IF iSide = "db" THEN "дебетовать" ELSE "кредитовать") +
                       "%s=" + bAcct.acct + ")".

       WHEN "Core15_2" THEN
       mVL_Except =  "__core15_2(%s=" + ( IF iSide = "db" THEN "дебетовать" ELSE "кредитовать") +
                     "%s=" + bAcct.branch-id + ")".
     END CASE.
     {except.i &OtherInfo = "'2'"}
   END.

   RETURN .
END PROCEDURE.

FUNCTION getExceptionErrorMsg RETURN CHARACTER (INPUT iException AS CHARACTER,
                                                INPUT iAcct      AS CHARACTER,
                                                INPUT iStatType  AS CHARACTER):
    RETURN ( IF iException MATCHES "*_bl*" OR
            CAN-DO("d_dsp,d_dspb,d_ksa", iException) THEN
                (( IF {assigned iStatType} THEN ("Счет "           +
                                                GetNullStr(iAcct) +
                                                " заблокирован. ")
                                          ELSE "")
                 +
                 GetCodeName("acct-status", iStatType))
            ELSE
                "").
END FUNCTION.

PROCEDURE SetAcctLockErrorMsg.
    DEFINE INPUT PARAMETER iException AS   CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iAcct      LIKE acct.acct NO-UNDO.
    DEFINE INPUT PARAMETER iStatType  LIKE code.code NO-UNDO.
    DEFINE INPUT PARAMETER iCustomMsg AS   CHARACTER NO-UNDO.

    IF {assigned iStatType} THEN
        RUN SetSysConf IN h_base ("block-acct-msg",
                                  IF {assigned iCustomMsg}
                                  THEN iCustomMsg
                                  ELSE getExceptionErrorMsg(iException,
                                                            iAcct,
                                                            iStatType)).
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка счета в проводке после ввода всех реквизитов проводки
  Parameters:  iObject     - ссылка на буфер проводки
               iUpObject   - ссылка на буфер документа
               bAcct       - буфер счета
               iCurrency   - валюта проводки
               iSide       - позиция счета в проводке
                              db - для дебета
                              cr - для кредита 
               iPosChk     - полностью отключает проверку остатка по счетам,
                             что существенно ускоряет групповые операции, но
                             не допустимо для одиночных операций
               iExtChk     - выключает расширенную проверку остатка для ряда
                             операций, при этом отключается проверка на
                             списание с картотеки, что допустимо для групповых
                             операций, но не допустимо для одиночных
               oOtherInfo  - дополнительная иформация для возврата валидатору
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE CheckAcctOE PRIVATE:
   DEFINE INPUT  PARAMETER iObject     AS HANDLE     NO-UNDO.
   DEFINE INPUT  PARAMETER iUpObject   AS HANDLE     NO-UNDO.
   DEFINE PARAMETER BUFFER bAcct       FOR acct.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSide       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iPosChk     AS LOGICAL    NO-UNDO.
   DEFINE INPUT  PARAMETER iExtChk     AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER oOtherInfo  AS CHARACTER  NO-UNDO.
   
   DEFINE VARIABLE vAcctCat   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAmtCur    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vAmtRub    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vPrevYear  AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vOpdate    AS DATE       NO-UNDO.
   DEFINE VARIABLE vOrderPay  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCheck     AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vAcct      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vStat      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vVL_Params AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vVL_Values AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vBlckRecid AS RECID      NO-UNDO.
   DEFINE VARIABLE vBlockAcct AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctRec   AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vOpKind     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpTemplate AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vBal       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vVal       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vPSErr     AS LOGICAL    NO-UNDO.

   DEFINE VARIABLE vLimOver   AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vMess      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vLim       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE mStatus    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vPrevStat  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vExecStat  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vStatCtrl  AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vIsBudget  AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vWithK2Docs AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vStatIP    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDocID     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDoc       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vClient    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vNoOrdBlk  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vFlChk     AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vProc-name AS CHARACTER  NO-UNDO.    /* Процедура поиска договора овердрафта */
   DEFINE VARIABLE vRid1      AS RECID      NO-UNDO.    /* RecId на договор */

   DEFINE VARIABLE mTxt       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOAcct     AS RECID      NO-UNDO.
   DEFINE VARIABLE vSum1      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vSum2      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vDateTime  AS DATETIME   NO-UNDO.

   DEFINE VARIABLE vOp LIKE op.op NO-UNDO.
   DEFINE VARIABLE vDocNum    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vBenAcct   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vBalBudg   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vUserAns   AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vBlockOrd  AS CHARACTER  NO-UNDO.

   DEFINE BUFFER op-kind     FOR op-kind.
   DEFINE BUFFER BlockObject FOR BlockObject. /* Локализация буфера. */
   DEFINE BUFFER cust-corp   FOR cust-corp.
   DEFINE BUFFER person      FOR person.
   DEFINE BUFFER oacct       FOR acct.
   DEFINE BUFFER loan        FOR loan.

/*   FORM mTxt NO-LABEL VIEW-AS EDITOR SIZE 70 BY 25                     */
/*   WITH OVERLAY FRAME sss1 SCROLLABLE SIDE-LABELS 0 COL CENTERED ROW 8.*/

   ASSIGN
      vAcctCat    = iObject:BUFFER-FIELD("acct-cat"):BUFFER-VALUE
      vAmtCur     = iObject:BUFFER-FIELD("amt-cur"):BUFFER-VALUE
      vPrevYear   = iObject:BUFFER-FIELD("prev-year"):BUFFER-VALUE
      vOpdate     = iObject:BUFFER-FIELD("op-date"):BUFFER-VALUE
      vAmtRub     = iObject:BUFFER-FIELD("amt-rub"):BUFFER-VALUE
        /* Если депо-счет, подменяем рублевую сумму на количество */
      vAmtRub     = iObject:BUFFER-FIELD("qty"):BUFFER-VALUE WHEN bAcct.acct-cat = "d"
      vAcct       = iObject:BUFFER-FIELD("acct-" + iSide):BUFFER-VALUE
      lim-pos     = 0.00
      mbl-pos     = 0.00

      mAcctStatCode = ""
      mAcctStatType = ""
      mLimitSumMess = ?  
      mAmtSumMess   = ?
      mMainCodeMess = ""
      mIsOverLimit  = FALSE
   .

   IF NOT AVAIL bAcct OR
      (vAmtCur <> 0 AND bAcct.currency = "" AND NOT type-balance) OR
      (bAcct.currency = "" AND type-curracct AND iCurrency <> "") THEN 
   DO:
      mVL_Except = ( IF vAcctCat <> "d" THEN "__core08" ELSE "__core09") +
                   "(%s=" + vAcct + "%s=" + iCurrency + ")".
      {except.i &OtherInfo = "'2'"}
   END.
      

   IF vPrevYear AND
      YEAR(bAcct.open-date) >= YEAR(vOpdate) THEN
   DO:
      mVL_Except = "__core12(%t=" + STRING(vOpdate) +
                    "%t=" + STRING(bAcct.open-date) +
                    "%s=" + ClueAcctCurr(bAcct.acct,bAcct.currency) + ")".
      {except.i}
   END.

   RUN SetLstParam IN h_strng ({&VP_ACCT_HANDLE},INPUT BUFFER bAcct:HANDLE,
                               INPUT-OUTPUT vVL_Params,INPUT-OUTPUT vVL_Values,
                               {&VP_PARAM_DLM}).

   IF VALID-HANDLE(iUpObject)  THEN
      ASSIGN
         vOp         = iUpObject:BUFFER-FIELD("op"):BUFFER-VALUE
         vOrderPay   = iUpObject:BUFFER-FIELD("order-pay"):BUFFER-VALUE
         vOpKind     = iUpObject:BUFFER-FIELD("op-kind"):BUFFER-VALUE
         vOpTemplate = vOpKind + "," + iUpObject:BUFFER-FIELD("op-template"):BUFFER-VALUE
         vStat       = iUpObject:BUFFER-FIELD("op-status"):BUFFER-VALUE
         vDocNum     = iUpObject:BUFFER-FIELD("doc-num"):BUFFER-VALUE
         vBenAcct    = iUpObject:BUFFER-FIELD("ben-acct"):BUFFER-VALUE
      .
   ELSE
   DO:
      IF VALID-HANDLE(iObject) THEN
         vOp = iObject:BUFFER-FIELD("op"):BUFFER-VALUE.
      FIND FIRST op WHERE 
         op.op = INT64(iObject:BUFFER-FIELD("op"):BUFFER-VALUE) NO-LOCK NO-ERROR.

      FIND FIRST op-entry of op NO-LOCK NO-ERROR.

      vOrderPay   = IF AVAILABLE op THEN op.order-pay ELSE ?.
      vOpKind     = IF AVAILABLE op THEN op.op-kind   ELSE "".
      vOpTemplate = IF AVAILABLE op THEN (vOpKind + "," + STRING(OP.op-template))   ELSE "".
      vStat       = IF AVAILABLE op THEN op.op-status ELSE "".
      vDocNum     = IF AVAILABLE op THEN op.doc-num ELSE "".
      vBenAcct    = IF AVAILABLE op THEN op.ben-acct ELSE "no acct".
      vAcctRec    = IF AVAIL op AND {assigned op.ben-acct} THEN op.ben-acct       ELSE
                    IF AVAIL op-entry                      THEN op-entry.acct-cr  ELSE "".  
   END.

   RUN IsBudgetPayment(vOp, OUTPUT vIsBudget) NO-ERROR.

   IF vOpKind NE "" THEN
      FIND FIRST op-kind WHERE op-kind.op-kind EQ vOpKind NO-LOCK NO-ERROR.

   IF AVAILABLE op-kind THEN
      vOpKind = op-kind.proc + "," + GetXAttrValueEx("op-kind", op-kind.op-kind, "name-card", "").
   ELSE
      vOpKind = "".

   IF FGetSetting("СтандТр","ВывСообщБлок","Нет") EQ "Да" AND 
      GetXAttrValueEx("op-kind",
                      op.op-kind,
                      "NoBlockMsg",
                      "Нет") NE "Да"
      AND NOT vStat BEGINS "А"
      AND NOT vStat BEGINS "В" THEN 
   DO:
       ASSIGN 
          mTxt      = ""
          vDateTime = /* NOW */ DATETIME(vOpDate, MTIME)
       .
       FOR EACH BlockObject NO-LOCK
          WHERE BlockObject.class-code   EQ "BlockAcct"
            AND BlockObject.file-name    EQ "acct"
            AND BlockObject.surrogate    EQ bAcct.acct + "," + bAcct.currency
            AND BlockObject.beg-datetime LE vDateTime 
          BY BlockObject.beg-datetime :

          IF (BlockObject.end-datetime GE vDateTime OR
              BlockObject.end-datetime EQ ?) THEN
          mTxt = mTxt + "С " + 
               STRING(DATE(ENTRY(1,STRING(BlockObject.beg-datetime)," ")),"99.99.9999") + 
                 " " +
                 ( IF BlockObject.end-datetime EQ ?
                   THEN FILL(" ",13)
                   ELSE "ПО " + 
                 STRING(DATE(ENTRY(1,STRING(BlockObject.end-datetime)," ")),"99.99.9999")
                 ) + " " +
                 STRING(BlockObject.block-type,"x(12)") + " " +
                 ( IF BlockObject.block-type NE "БлокСумм"
                   THEN FILL(" ",17)
                   ELSE STRING(ABS(BlockObject.val[3]),">>,>>>,>>>,>>9.99") 
                 ) +  " " +
                 FILL(" ", 11 - LENGTH(TRIM(BlockObject.txt[1]))) + 
                 TRIM(BlockObject.txt[1]) + 
                 "~n".
       END.   

       IF {assigned mTxt } THEN
       mTxt = "Блокировки по счету:~n" + mTxt + FILL("_",69).

       RUN card-acct(recid(bAcct), OUTPUT vOAcct, "Карт2ВнСчет").
       IF vOAcct NE ? THEN 
       FIND FIRST oacct WHERE RECID(oacct) EQ vOAcct NO-LOCK NO-ERROR. 
       IF AVAIL(oacct) THEN 
       DO:
          RUN acct-pos in h_base (oacct.acct, 
                                  oacct.currency, 
                                  vOpDate, 
                                  vOpDate, 
                                  gop-status).
          vSum1 = IF oacct.currency EQ ""
                  THEN sh-bal
                  ELSE sh-val.
          RELEASE oacct.
       END.
              
       RUN card-acct(recid(bAcct), OUTPUT vOAcct, "КартБВнСчет").
       IF vOAcct NE ? THEN 
       FIND FIRST oacct WHERE RECID(oacct) EQ vOAcct NO-LOCK NO-ERROR. 
       IF AVAIL(oacct) THEN 
       DO:
          RUN acct-pos in h_base (oacct.acct, 
                                  oacct.currency, 
                                  vOpDate, 
                                  vOpDate, 
                                  gop-status).
          vSum2 = IF oacct.currency EQ ""
                  THEN sh-bal
                  ELSE sh-val.
          RELEASE oacct.
       END.
    
       IF {assigned mTxt} OR 
          vSum1 NE 0       OR 
          vSum2 NE 0       THEN
       DO:
           mTxt = mTxt + "~n" +
                  "Картотека 2: " + 
                  STRING(ABS(vSum1),">>,>>>,>>>,>>9.99") + "~n" +
                  "        КБС: " + 
                  STRING(ABS(vSum2),">>,>>>,>>>,>>9.99") + "~n".

/*
           DISPLAY mTxt WITH FRAME sss1 TITLE "Счет " + bAcct.number.
           
           DO ON ERROR  UNDO,RETRY
              ON ENDKEY UNDO,RETRY:
              PAUSE 0.
              WAIT-FOR "ENTER" OF FRAME sss1 FOCUS mTxt.
           END.
           /*
           {wait_for.i 
             &THIS_FRAME = "sss1" 
             &EVENT_LIST="'GO,ESC,ENTER,END-ERROR'" 
             &EXTEXT= "GO,ESC,ENTER,END-ERROR OF FRAME sss1 FOCUS mTxt"}.
           HIDE FRAME sss1.
           */
*/           

         FORM
            mTxt NO-LABEL VIEW-AS EDITOR SIZE 74 BY 20
         WITH FRAME frCard OVERLAY CENTERED ROW 4  /* SIDE-LABELS 1 COL*/
         TITLE "Счет " + bAcct.number.
         
         mTxt:READ-ONLY = YES.
         mTxt:SENSITIVE = NO.
         
         ON ANY-KEY OF mTxt
         DO:
            IF {&LAST_KEY} EQ 13 THEN
            DO:
               APPLY "GO" TO FRAME frCard.
               {return_no_apply.i}
            END.
            IF {&LAST_KEY} EQ 27 THEN
            DO:
               APPLY "ENTRY" TO mTxt.
               {return_no_apply.i}
            END.
         END.
         
         UPD:
         DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
                        ON ENDKEY UNDO UPD, LEAVE UPD:
            {update.i &THIS_FRAME = "frCard" &EXFILE = "pp-op.p.st1" {&*}}.
         END.
         HIDE FRAME frCard.
      END.
   END.
   /* */
   ASSIGN
       vPrevStat = GetSysConf("ПредСтатус")
       vExecStat = FGetSetting("СтандТр", "СтатИспДок", "ФБП")
       vBalBudg  = FGetSetting("ГНИ","bal-budget-ex","")
       vNoOrdBlk = BlockAcctNoOrderPay(bAcct.acct + "," + bAcct.currency, DATETIME(TODAY, MTIME))
   .
   vStatCtrl = {assigned vPrevStat} AND
               (vPrevStat <  vExecStat OR
                vPrevStat >= vExecStat AND vStat < vExecStat).
   IF NOT {assigned vPrevStat} OR vStatCtrl THEN DO:
       mAcctStatCode = CheckObject("acct",
                                   Surrogate(BUFFER bAcct:HANDLE),
                                   IF vStatCtrl THEN
                                       ""
                                   ELSE
                                       ( IF VALID-HANDLE(iUpObject) THEN
                                            "op"
                                        ELSE
                                            "op-entry"),
                                   Surrogate(IF VALID-HANDLE(iUpObject) THEN
                                                 iUpObject
                                             ELSE
                                                 iObject),
                                   DATETIME(vOpDate, MTIME),
                                   OUTPUT vBlckRecid).
       FIND FIRST BlockObject WHERE
           RECID(BlockObject) = vBlckRecid
       NO-LOCK NO-ERROR.
   END.
   mAcctStatType = GetBlkType(mAcctStatCode).
   

   IF vIsBudget THEN DO:
      mbl-pos = IF Chk_AcctRec_For_CBLACCT(vAcctRec)
                THEN GetBlockPositionAll(bAcct.acct, bAcct.currency, vOpDate)
                ELSE GetBlockPosition(bAcct.acct, bAcct.currency, "-1", vOpDate).
   END.
   ELSE IF AVAILABLE BlockObject AND mAcctStatType <> "БлокСумм" THEN
      mbl-pos = BlockObject.val[3].
   ELSE DO:
      mbl-pos = IF Chk_AcctRec_For_CBLACCT(vAcctRec)
                THEN GetBlockPositionAll(bAcct.acct, bAcct.currency, vOpDate)
                ELSE GetBlockPosition(bAcct.acct, bAcct.currency, vOrderPay, vOpDate).
   END.

   /* если счет блокирован и статус  Блок,БлокДб,БлокКр */
   IF (NOT (FGetSetting("СтандТр","ОтклБлокПров","ДА") EQ "ДА" AND {assigned vStat} AND vStat LE "П")) OR mAcctStatType NE "БлокДб" THEN
   DO:

      IF     {assigned mAcctStatType} 
         AND (LOOKUP(mAcctStatType,"Блок,БлокДб,БлокКр") NE 0
              /* OR LOOKUP(mAcctStatType,"Блок,БлокДб,БлокКр,БлокСумм") EQ 0 */ )  /* Нестандартные блокировки действуют аналогично общей блокировке Блок */
         AND NOT (NUM-ENTRIES(vOpKind) GT 1 AND ENTRY(2, vOpKind) BEGINS "картотека") THEN
      DO:
         IF iSide = "db" THEN
         DO:
            IF AVAIL BlockObject AND 
               (LOOKUP(mAcctStatType,"Блок,БлокДб") NE 0 
                   OR LOOKUP(mAcctStatType,"Блок,БлокДб,БлокКр,БлокСумм") EQ 0)
            THEN DO:
               vUserAns = no.
               IF {assigned vBalBudg} AND
                  CAN-DO(vBalBudg,vBenAcct) AND 
                  NOT ({assigned vNoOrdBlk} OR 
                       CAN-DO(BlockObject.txt[1],vOrderPay))
               THEN DO:
                  pick-value = ?.
                  RUN DeleteOldDataProtocol IN h_base ("Acct-bal-budget-ex").
                  RUN Fill-SysMes IN h_tmess 
                     ("","","4",
                      "Внимание! По документу " + vDocNum + " с очередностью " + vOrderPay +
                      ", счет получателя " + bAcct.acct +
                      " решение о проводке должно быть принято вручную. Провести данный документ?").
                  IF pick-value EQ "no" THEN DO:
                     RUN SetSysConf IN h_base ("Acct-bal-budget-ex","NO").
                     vUserAns = yes.
                  END.
                  IF pick-value EQ "yes" THEN
                     RUN SetSysConf IN h_base ("Acct-bal-budget-ex","YES").
               END.
               IF {assigned vNoOrdBlk}
                   OR vUserAns
                   OR  NOT CAN-DO(BlockObject.txt[1],vOrderPay) THEN 
               DO:
                  mVL_Except = ( IF bAcct.side = "А" THEN "d_bla" ELSE "d_blp").
                  RUN SetAcctLockErrorMsg(mVL_Except,
                                          bAcct.acct,
                                          mAcctStatType,
                                          "").
                  {except.i &ExtParams = vVL_Params &ExtValues = vVL_Values}
               END.
               RUN DeleteOldDataProtocol IN h_base ("Acct-bal-budget-ex").
            END.
         END.
         ELSE DO:
            RUN SetAcctLockErrorMsg("k_bl",
                                    bAcct.acct,
                                    mAcctStatType,
                                    "").
            {except.i &Exception = "'k_bl'" 
                      &ExtParams = vVL_Params 
                      &ExtValues = vVL_Values}.
         END.
      END.
   END.

   IF iPosChk THEN DO:
      /* Commented by KSV: Проверка остатка. 0046989 отсекаем ситуации, приводящие
      ** к увеличению остатка */
      vCheck = ((iSide = "cr" AND bAcct.side = "А" ) OR
                (iSide = "db" AND bAcct.side = "П")) .
      IF vAmtRub < 0 THEN vCheck = NOT vCheck.

      /* Commented by KSV: Для счета не являющимся внутренним, при iExtChk = YES,
      ** включаем расширенную проверку, чтобы не пропустить систуацию списания с
      ** картотеки */
      IF NOT(vCheck) AND iExtChk AND bAcct.cust-cat <> "В" THEN vCheck = YES.
   END.

   IF     AVAILABLE op-kind
      AND
          CAN-DO(GetXAttrValueEx("op-kind",
                                 op-kind.op-kind,
                                 "AcctNoPos",
                                 "-- пусто --"),
                 bAcct.number)
      THEN
   DO:
      vCheck = NO.
   END.

   /* Commented by KSV: выполняем контроль остатка */
   IF vCheck AND
      ((bAcct.check-op <> "ДбКрОст" AND bAcct.check-op <> "ДбОст" AND iSide = "db") OR
       (bAcct.check-op <> "ДбКрОст" AND bAcct.check-op <> "КрОст" AND iSide = "cr")) THEN 
   DO:

      /* Берем статус для вычисления остатка с транзакции, если есть (только для овердрафтов) */
      IF AVAIL op-kind THEN 
      DO:
         mStatus = GetXattrValueEx("op-kind",op-kind.op-kind,"StatusOvr","П").
         /* проверка наличия статуса в классификаторе */
         IF mStatus NE "П" AND
            NOT CAN-FIND(FIRST code WHERE code.class EQ "Статус"
                                      AND code.code  EQ mStatus) THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","Не существует статус " + mStatus + "
                                        для документов").
            mStatus = "П".
         END.
      END.
      ELSE mStatus = "П".
      vWithK2Docs = GetXAttrValueEx("op-template",
                                    vOpTemplate,
                                    "УчетДокК2",
                                    FGetSetting("СтандТр",
                                                "УчетДокК2",
                                                "")) <> "Отменить".
      IF {assigned vAccessMask} AND CAN-DO(vAccessMask,bAcct.acct) AND 
         {assigned vAccessContAcct} AND CAN-DO(vAccessContAcct,bAcct.contract) AND
         ((iSide = "db" AND bAcct.side = "П") OR (iSide = "cr" AND bAcct.side = "А") OR
          (iSide = "cr" AND bAcct.side = "П" AND {assigned vPrevStat} AND vPrevStat > vStat))  /* понижение статуса кредитующих пассивный счет операций */
          THEN  RUN CalcAvailPos(bAcct.acct, bAcct.currency, gend-date, gend-date, 
                                           IF {assigned vAccessStatus} THEN vAccessStatus ELSE mStatus, mStatus, "cli-pos", 
                                           vWithK2Docs, IF vIsBudget THEN "-1" ELSE vOrderPay, NO,
                                           OUTPUT sh-bal, OUTPUT sh-val).
      ELSE         
         RUN cli-pos IN h_base (bAcct.acct,bAcct.currency,gend-date,gend-date,mStatus).
      vBal = sh-bal.
      vVal = sh-val.

     /* Для депо счета заменяем остаток денежный на количество */

     IF bAcct.acct-cat EQ "d" THEN
     DO:
      RUN acct-qty IN h_base (bAcct.acct,
                              bAcct.currency,
                              gend-date,
                              gend-date,
                              "П").
      vBal = sh-qty.
     END.
     IF NOT (FGetSetting("СтандТр","ОтклБлокПров","ДА") = "ДА" AND {assigned vStat} AND vStat LE "П") THEN
      IF iSide = "db" AND 
         mAcctStatType = "БлокСумм" AND 
         bacct.acct-cat    = "b"        
         AND NOT (NUM-ENTRIES(vOpKind) GT 1 AND ENTRY(2, vOpKind) BEGINS "Картотека")
         AND NOT(bAcct.currency NE "" AND vAmtRub NE 0.0 AND vAmtCur EQ 0.0)
         AND NOT(bAcct.currency EQ "" AND vAmtRub EQ 0.0 AND vAmtCur NE 0.0)
         THEN   
      DO:

         /* Получим сумму лимита овердрафта */
         RUN getoversm.p (RECID(bAcct),
                          vOpdate, 
                          OUTPUT vLimOver,
                          OUTPUT vMess).  

         IF mbl-pos <> 0 AND bAcct.side = "П" AND vAmtRub > 0   AND 
            ((vBal > (vLimOver + mbl-pos) AND bAcct.currency =  "")        OR 
             (vVal > (vLimOver + mbl-pos) AND bAcct.currency <> ""))       THEN DO:
            ASSIGN
               vUserAns  = no
               vBlockOrd = IF AVAIL BlockObject THEN BlockObject.txt[1] ELSE "-"
            .
            IF {assigned vBalBudg} AND
               CAN-DO(vBalBudg,vBenAcct) AND 
               NOT ({assigned vNoOrdBlk} OR 
                    CAN-DO(vBlockOrd,vOrderPay))
            THEN DO:
               pick-value = ?.
               RUN DeleteOldDataProtocol IN h_base ("Acct-bal-budget-ex").
               RUN Fill-SysMes IN h_tmess 
                  ("","","4",
                   "Внимание! По документу " + vDocNum + " с очередностью " + vOrderPay +
                   ", счет получателя " + bAcct.acct +
                   " решение о проводке должно быть принято вручную. Провести данный документ?").
               IF pick-value EQ "no" THEN 
                  RUN SetSysConf IN h_base ("Acct-bal-budget-ex","NO").
               IF pick-value EQ "yes" THEN DO:
                  RUN SetSysConf IN h_base ("Acct-bal-budget-ex","YES").
                  vUserAns = yes.
               END.
            END.

            IF NOT vUserAns THEN DO:
            RUN SetAcctLockErrorMsg("d_dspb",
                                    bAcct.acct,
                                    mAcctStatType,
                                    "").
            {except.i &Exception = "'d_dspb'"
                      &ExtParams = vVL_Params 
                      &ExtValues = vVL_Values}
               RUN DeleteOldDataProtocol IN h_base ("Acct-bal-budget-ex").
            END.
         END.
      END.
      IF iSide = "db" AND bAcct.side = "А" AND vAmtRub < 0 AND
         ((vBal < mbl-pos AND bAcct.currency EQ "") OR 
          (vVal < mbl-pos AND bAcct.currency NE ""))       AND 
         bAcct.contr-acct = ""  THEN DO:
         RUN SetAcctLockErrorMsg("d_ksa",
                                 bAcct.acct,
                                 mAcctStatType,
                                 "").
         {except.i &Exception = "'d_ksa'"
                   &ExtParams = vVL_Params 
                   &ExtValues = vVL_Values}
      END.

      
      IF iSide = "cr" AND bAcct.side = "П"       AND
           ((vBal > 0 AND bAcct.currency EQ "")  OR 
            (vVal > 0 AND bAcct.currency NE "")) THEN 
         {except.i &Exception = "'k_dsp'"
                   &ExtParams = vVL_Params 
                   &ExtValues = vVL_Values}
      IF iSide = "db" AND bAcct.side = "П" AND vAmtRub > 0 
        AND NOT(bAcct.currency NE "" AND vAmtRub NE 0.0 AND vAmtCur EQ 0.0 AND bAcct.acct-cat EQ "b") 
      THEN
      DO:
         lim-pos = GetLimitPosition(BUFFER bAcct, vOpdate).
         IF NOT (FGetSetting("СтандТр","ОтклБлокПров","ДА") EQ "ДА" AND {assigned vStat} AND vStat LE "П")
            THEN vLim = lim-pos + mbl-pos.
            ELSE vLim = lim-pos.

         IF (
               (vBal > vLim AND bAcct.currency EQ "")
            OR (vVal > vLim AND bAcct.currency NE "")
            OR (vBal > vLim AND bAcct.acct-cat EQ "d") 
            )
            AND NOT (NUM-ENTRIES(vOpKind) GT 1 AND ENTRY(2, vOpKind) BEGINS "Картотека")
            THEN
         DO:
            IF bAcct.acct-cat = "b" AND Cart2Flag  THEN
            DO:
               RUN ResetReturnValue IN h_base.
               RUN CheckCard2(bAcct.acct,bAcct.currency,vOpdate).
               IF {&RETURN_VALUE} <> "" THEN
                  RUN SetSysConf IN h_base ("У_клиента_есть_картотека","Да").
               RUN ResetReturnValue IN h_base.

               IF    FGetSetting("СтандТР", "ОтклКонтрБлок", "нет") NE "да"
                  OR GetSysConf("nokbs")                            NE "nokbs" THEN
               DO:
                  RUN CheckCardA(bAcct.acct,bAcct.currency,"КБС",vOpdate).

                  IF {&RETURN_VALUE} <> "" THEN
                     RUN SetSysConf IN h_base ("У_клиента_есть_КБС","Да").
               END.
               RUN ResetReturnValue IN h_base.
            END.
            RUN SetAcctLockErrorMsg("d_dsp",
                                    bAcct.acct,
                                    mAcctStatType,
                                    "").
            /* определяем есть ли у счета овердрафт */   
            /* Определим метод поиска соглашения об овердрафте */
            RUN GetClassMethod in h_xclass  (bacct.class-code,
                                             "FindLoan",
                                             "",
                                             "",
                                             OUTPUT vProc-name,
                                             OUTPUT vMess).
            IF vProc-name NE ? THEN 
            DO:
                 /* Запускаем метод определения договора овердрафта */
               RUN VALUE(vProc-name + ".p") (RECID(bacct),
                                             vOpDate, 
                                             OUTPUT vRid1, 
                                             OUTPUT vMess).
               IF vRid1 NE ? THEN 
                  FIND FIRST loan WHERE
                       RECID(loan) EQ vRid1 
                  NO-LOCK NO-ERROR.
            END.
              /* иначе стандартным способом... */
            ELSE
               FIND LAST loan WHERE
                         loan.contract   EQ "Кредит"
                  AND    loan.cust-cat   EQ bacct.cust-cat
                  AND    loan.cust-id    EQ bacct.cust-id
                  AND    loan.currency   EQ bacct.currency
                  AND    CAN-DO(mStrClass,  loan.class-code)
                  AND    loan.open-date  LE vOpDate
                  AND    loan.close-date EQ ? 
               NO-LOCK NO-ERROR.

          /* Если еть такой договор, то определим лимит остатка по договору */
            IF AVAIL loan THEN
            DO:                                                       
               /* Получим сумму лимита овердрафта */
               RUN getoversm.p (RECID(bAcct),
                                vOpdate, 
                                OUTPUT vLimOver,
                                OUTPUT vMess).                             
               IF vLimOver < vBal THEN
               DO:
                  ASSIGN
                     mLimitSumMess = vLimOver  
                     mAmtSumMess = vBal
                     mMainCodeMess = loan.cont-code
                     mIsOverLimit = TRUE
                     .
               END.
            END.                                       
            {except.i &Exception = "'d_dsp'"
                      &ExtParams = vVL_Params 
                      &ExtValues = vVL_Values}.
         END.
        
        IF NOT (NUM-ENTRIES(vOpKind) GT 1 AND ENTRY(2, vOpKind) BEGINS "Картотека") THEN
        DO:                       /* сумма проверяемого документа будет учтена при подсчете остатка */
           RUN CheckProbSaldo (iSide, bAcct.acct, bAcct.currency, 0, 0, vOpDate, vStat, OUTPUT vPSErr). 
           IF vPSErr THEN
           DO: 
               {except.i &Exception = "'d_dspr'"
                         &ExtParams = vVL_Params 
                         &ExtValues = vVL_Values}.
           END.
        END.

      END.

      /* проверка на вероятное красное сальдо при понижении статуса документа зачисления */
      IF iSide = "cr" AND bAcct.side = "П" AND {assigned vPrevStat} AND vPrevStat > vStat AND 
        vPrevStat GE CHR(251) AND vStat LT CHR(251) THEN DO:
        IF NOT (NUM-ENTRIES(vOpKind) GT 1 AND ENTRY(2, vOpKind) BEGINS "Картотека") THEN
        DO:                       /* сумма проверяемого документа будет учтена при подсчете остатка */
           RUN CheckProbSaldo (iSide, bAcct.acct, bAcct.currency, 0, 0, vOpDate, vStat, OUTPUT vPSErr). 
           IF vPSErr THEN
           DO: 
               {except.i &Exception = "'k_dspr'"
                         &ExtParams = vVL_Params 
                         &ExtValues = vVL_Values}.
           END.
        END.
      END.

      IF iSide = "cr" AND bAcct.side = "П" AND
         (bAcct.cust-cat NE "В" AND CAN-DO(mAcctContCrd2,bacct.contract)) AND 
         bAcct.acct-cat  = "b" THEN
      DO:
         lim-pos = GetLimitPosition(BUFFER bAcct, vOpdate).
         IF (bAcct.currency  = "" AND vBal < lim-pos) OR  
            (bAcct.currency <> "" AND vVal < lim-pos) THEN
            {except.i &Exception = "'k_ksp'"
                      &ExtParams = vVL_Params 
                      &ExtValues = vVL_Values}
      END.

      IF iSide = "cr" AND bAcct.side = "А" AND vAmtRub > 0 THEN
      DO:
         lim-pos = GetLimitPosition(BUFFER bAcct, vOpdate).
         IF (bAcct.currency  = ""  AND vBal < lim-pos) OR 
            (bAcct.currency <> ""  AND vVal < lim-pos) OR 
            (bAcct.acct-cat  = "d" AND vBal < lim-pos) THEN
            {except.i &Exception = "'k_ksa'"
                      &ExtParams = vVL_Params 
                      &ExtValues = vVL_Values}
      END.
   END.
   
   /* Commented by KSV: Картотека */
   IF iSide = "db" AND bAcct.side = "П"               AND 
      bAcct.acct-cat = "b" AND Cart2Flag              AND 
      GetSysConf("У_клиента_есть_картотека") <> "Да"  THEN
   DO:
      RUN ResetReturnValue IN h_base.
      RUN CheckCard2(bAcct.acct,bAcct.currency,vOpdate).

      IF {&RETURN_VALUE} <> "" THEN DO:

         vCartName = "Карт2".
         {except.i &Exception = "'d_crp'"
                   &ExtParams = vVL_Params 
                   &ExtValues = vVL_Values}
      END.
   END.

   IF iSide = "db" AND bAcct.side = "П"               AND 
      bAcct.acct-cat = "b" AND Cart2Flag              AND 
      bAcct.cust-cat NE "В"                           AND
      (FGetSetting("СтандТР", "ОтклКонтрБлок", "нет") NE "да" OR
      GetSysConf("nokbs") NE "nokbs")                  AND
      GetSysConf("У_клиента_есть_КБС") <> "Да"  THEN
   DO:
      RUN ResetReturnValue IN h_base.
      RUN CheckCardA(bAcct.acct,bAcct.currency,"КБС",vOpdate).

      IF {&RETURN_VALUE} <> "" THEN DO:
         vCartName = "КартБлСч".
         {except.i &Exception = "'d_crp'"
                   &ExtParams = vVL_Params 
                   &ExtValues = vVL_Values}
      END.                                     
   END.

   /* Проверка действительности паспорта РФ клиента по базе ФМС */
   ASSIGN
      vMess = FGetSetting("ОбменФМС","ФМСРед_Док","")
      vAcct = FGetSetting("ОбменФМС","ФМССчета_Док","")
   .
   IF AVAILABLE op-kind THEN
      vMess = GetXAttrValueEx("op-kind", op-kind.op-kind, "ОбменФМС", vMess).
   IF vMess EQ "Да" AND 
      CAN-DO(vAcct,STRING(bAcct.bal-acct)) AND 
      NOT(GetSysConf("NoFmsDocChk") EQ "YES")
   THEN DO:
      vFlChk = no.
      IF bAcct.cust-cat EQ "Ю" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ bAcct.cust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN DO:
            ASSIGN
               vMess   = GetXattrValueEX("cust-corp", STRING(bAcct.cust-id), "Предпр", "")
               vStatIP = FGetSetting("СтандТр","СтатусФЛЧП","")
            .
            IF FGetSetting("ОбменФМС","ФМСРед_КлЮ","") EQ "Да" AND
               (vMess NE "" OR CAN-DO(vStatIP,cust-corp.cust-stat))
            THEN 
               ASSIGN
                  vMess   = "cust-corp"
                  vDocID  = GetXattrValueEX("cust-corp",STRING(cust-corp.cust-id),"document-id","")
                  vDoc    = GetXattrValueEX("cust-corp",STRING(cust-corp.cust-id),"document","")
                  vClient = cust-corp.name-corp
                  vFlChk  = yes
               .
         END.
      END.   
      ELSE
         IF bAcct.cust-cat EQ "Ч" AND 
            FGetSetting("ОбменФМС","ФМСРед_Кл","") EQ "Да"
         THEN DO:
            FIND FIRST person WHERE person.person-id EQ bAcct.cust-id NO-LOCK NO-ERROR.
            IF AVAIL person THEN 
               ASSIGN
                  vMess   = "person"
                  vDocID  = person.document-id
                  vDoc    = person.document
                  vClient = person.name-last + " " + person.first-names
                  vFlChk  = yes
               .
         END.
      IF vFlChk THEN DO:
         {cl-fmschk.i
            "'op'"
            "STRING(vOp)"
            "vDocID"
            "vDoc"
            "GetXattrValueEX(vMess, STRING(bAcct.cust-id), 'Document4Date_vid', '')"
            "vClient"
         }
      END.
   END.

   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка всей проводки
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE vo__op-entry:
   {&VPD_VO_PARAM}

   DEFINE VARIABLE vAcctDB    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctCR    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCurrency  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vFlagerr   AS INT64    NO-UNDO.
   DEFINE VARIABLE vTmp       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctCat   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vPosChk    AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vExtChk    AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vIsDbVal AS LOG NO-UNDO.
   DEFINE VARIABLE vIsCrVal AS LOG NO-UNDO.
   DEFINE VARIABLE vStatus    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE VStatVld   AS CHARACTER INIT "" NO-UNDO.
   DEFINE BUFFER   dacct      FOR acct.
   DEFINE BUFFER   cacct      FOR acct.
   
   ASSIGN
      vAcctDB     = iObject:BUFFER-FIELD("acct-db"):BUFFER-VALUE
      vAcctCR     = iObject:BUFFER-FIELD("acct-cr"):BUFFER-VALUE
      vCurrency   = iObject:BUFFER-FIELD("currency"):BUFFER-VALUE
      vAcctCat    = iObject:BUFFER-FIELD("acct-cat"):BUFFER-VALUE
      vStatus     = iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE
   .
   IF FGetSettingEx("СтандТр","КнтрОстСт","",?) EQ "Да" THEN /*Использовать статус из НП РасшОбр для контроля сальдо*/
   VStatVld = FGetSettingEx("РасшОбр",?,?,yes). /*С какого статуса проверять сальдо*/
   
   /* Commented by KSV: Параметр EXT_CHECK отключает ряд проверок, связанных со
   ** счетом, в частности отключает возможность списания с картотеки, что
   ** допустимо для групповых операций, но не допустимо для ввода */
   /* Dema: Параметр POS_CHECK отключает проверку остатка по счетам, что
   ** допустимо только для групповых операций, но существенно быстрее */
   ASSIGN
      vPosChk = NOT (GetLstParam({&VP_POS_CHECK},iParams,iValues,{&VP_PARAM_DLM}) = "NO")
      vExtChk = NOT (GetLstParam({&VP_EXT_CHECK},iParams,iValues,{&VP_PARAM_DLM}) = "NO")
      .
   IF VStatVld NE "" AND VStatus LT VStatVld THEN
   vPosChk = NO.
   
   
   RUN SetSysConf IN h_base ("У_клиента_есть_картотека",?).
   RUN SetSysConf IN h_base ("У_клиента_есть_КБС",?).
   
   IF {assigned vAcctDB} THEN
   DO:
      {find-act.i
         &bact = dacct
         &acct = vAcctDB
         &curr = vCurrency
         &AddCond = " AND vAcctCat <> 'd'"
      }   

      /* Commented by KSV: Проверка счета дебета */
      RUN CheckAcctOE(iObject,iUpObject,BUFFER dacct,vCurrency,"db",vPosChk,vExtChk,
                      OUTPUT oOtherInfo).
      IF {assigned RETURN-VALUE} THEN RETURN {&RETURN_VALUE}.
   END.

   IF {assigned vAcctCR} THEN
   DO:
      {find-act.i
         &bact = cacct
         &acct = vAcctCR
         &curr = vCurrency
         &AddCond = " AND vAcctCat <> 'd'"
      }   

      /* Commented by KSV: Проверка счета кредита */
      RUN CheckAcctOE(iObject,iUpObject,BUFFER cacct,vCurrency,"cr",vPosChk,vExtChk,
                      OUTPUT oOtherInfo).
      IF {assigned RETURN-VALUE} THEN RETURN {&RETURN_VALUE}.
   END.

   /*  проверка разрешенной корреспонденции счетов с счетами дохода и расхода */
   IF vAcctDB <> ?                           AND 
      vAcctCR <> ?                           AND 
      ENTRY(1,vKontrKopp,";") EQ "Да"        AND /* контроль включен */ 
      CAN-DO(vMaskDohRash, vAcctCR)          AND /* зачисление на счет дохода/расхода */ 
     (NUM-ENTRIES(vKontrKopp,";") = 1        OR  /* НЕ списание с НЕконтролируемого счета */
      NOT CAN-DO(ENTRY(2,vKontrKopp,";"),vAcctDB)) THEN 
   DO:

      /* определим валютность счета дебета */
      vIsDbVal = dacct.currency NE "".
      /* определим разрешенную валютность счета дебета для счета кредита */
      vIsCrVal = GetXattrValueEx("acct",
                                 cacct.acct + "," + cacct.currency,
                                 "f102_cur",
                                 "no") = "yes".
      IF vIsCrVal NE vIsDbVal THEN
      DO:
         mVL_Except = "Валюта счета дебета не соответствует учетной валюте " + 
                      "счета кредита или неверно введено значение дополнительного " +
                      "реквизита f102_cur".
         {except.i &OtherInfo = "2"}
      END.
   END.
   
   IF  vCurrency NE "" AND 
      (vAcctDB EQ ? OR dacct.currency EQ "") AND 
      (vAcctCR EQ ? OR cacct.currency EQ "") THEN
      iObject:BUFFER-FIELD("currency"):BUFFER-VALUE = "".

   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка баланса документа
  Parameters:  iOp        - идентификатор документа
               iCurrBal   - флаг проверки валютного баланса
               iNoExcept  - выключает возникновения исключения, что позволяет
                            использовать процедуру вне контекста валидатора
               oOtherInfo - дополнительная иформация для возврата валидатору
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE CheckBalance:
   DEFINE INPUT  PARAMETER iOp         AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrBal    AS LOGICAL    NO-UNDO.
   DEFINE INPUT  PARAMETER iNoExcept   AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER oOtherInfo  AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bop-entry FOR op-entry.
   DEFINE BUFFER bop       FOR op.

   DEFINE VARIABLE vDbAmt    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCrAmt    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vDbRubAmt AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCrRubAmt AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vNCr      AS INT64 NO-UNDO.
   DEFINE VARIABLE vNDb      AS INT64 NO-UNDO.
   DEFINE VARIABLE vMultiHE  AS LOGICAL    NO-UNDO.
   

   /* При наличии настроечного параметра позволяем много полупроводок         */
   vMultiHE = (FGetSetting("МнПолуПров", ?, "Нет") EQ "Да").
   
   FOR EACH bop-entry WHERE 
         bop-entry.op = iOp NO-LOCK,
      FIRST bop OF bop-entry NO-LOCK
      BREAK BY bop-entry.currency:

      IF iCurrBal AND FIRST-OF(bop-entry.currency) THEN
         ASSIGN
            vDbAmt = 0
            vCrAmt = 0
         .

      IF bop-entry.acct-db NE ? 
         THEN vDBRubAmt = vDBRubAmt + bop-entry.amt-rub.
         ELSE vNDb    = vNDb + 1.

      IF bop-entry.acct-cr NE ? 
         THEN vCrRubAmt = vCrRubAmt + bop-entry.amt-rub.
         ELSE vNCr    = vNCr + 1.

      IF iCurrBal                 AND 
         bop-entry.currency <> "" THEN
      DO:
         IF bop-entry.acct-db <> ? THEN 
              vDBAmt = vDBAmt + bop-entry.amt-cur.
         IF bop-entry.acct-cr <> ? THEN
              vCrAmt = vCrAmt + bop-entry.amt-cur.
      END.

      IF iCurrBal                    AND 
         LAST-OF(bop-entry.currency) AND
         vDBAmt <> vCrAmt            THEN
      DO:
         mVL_Except = "__core28(%s=в валюте " + bop-entry.currency + 
                               "%d=" + STRING(vDBAmt,"->>>,>>>,>>>,>>>,>>9.99") +
                               "%d=" + STRING(vCrAmt,"->>>,>>>,>>>,>>>,>>9.99") + ")".
         IF iNoExcept THEN RETURN mVL_Except.
         {except.i}
      END.

      IF LAST(bop-entry.currency) THEN
      DO:
         IF vDBRubAmt <> vCrRubAmt THEN
         DO:
            mVL_Except = "__core28(%s=в документе номер " + bop.doc-num + 
                                  "%d=" + STRING(vDBRubAmt,"->>>,>>>,>>>,>>>,>>9.99") +
                                  "%d=" + STRING(vCrRubAmt,"->>>,>>>,>>>,>>>,>>9.99") + ")".
            IF iNoExcept THEN RETURN mVL_Except.
            {except.i}
         END.
         IF vNDb > 1 AND vNCr > 1 AND NOT vMultiHE THEN
         DO:    
            mVL_Except = "__core27(%s=" + bop.doc-num + ")".
            IF iNoExcept THEN RETURN mVL_Except.
            {except.i}
         END.
      END.
   END.

   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка документа
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE vo__op:
   {&VPD_VO_PARAM}

   DEFINE VARIABLE vOp     AS INT64    NO-UNDO.
   DEFINE VARIABLE vOpkind AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOpTemplate AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDprID  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vCorrOK  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vErrAcct AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTerrCh  AS LOGICAL     NO-UNDO.

   DEFINE VARIABLE vFMS     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDocID   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vLastAct AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vOk        AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vSetBlack AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBlack1   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vBlack2   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPrevStat AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vError    AS CHARACTER  NO-UNDO.

   DEFINE BUFFER history FOR history.

   ASSIGN
      vOP         = iObject:BUFFER-FIELD("op"):BUFFER-VALUE
      vOpkind     = iObject:BUFFER-FIELD("op-kind"):BUFFER-VALUE
      vOpTemplate = iObject:BUFFER-FIELD("op-template"):BUFFER-VALUE.

   /* Commented by KSV: Проверка баланса проводок */
   RUN CheckBalance(vOp,NOT type-bal,NO,OUTPUT oOtherInfo).
   IF {assigned RETURN-VALUE} THEN
      RETURN {&RETURN_VALUE}.
   
   /* Не менять значение переменной! Нужна для проверки паспорта клиента в конце процедуры */
   vLastAct = "".
   IF CAN-FIND(FIRST history WHERE history.file-name EQ "op"
                               AND history.field-ref EQ STRING(vOp))
   THEN
      FOR LAST history WHERE history.file-name EQ "op"
                         AND history.field-ref EQ STRING(vOp) 
      NO-LOCK BY history.modif-date BY history.modif-time:
         vLastAct = history.modify.
         LEAVE.
      END.

   /* Commented by KSV: Проверка документа на отмывание денег */
   IF GetLstParam({&VP_TERR_CHECK},iParams,iValues,{&VP_PARAM_DLM})<> "NO" AND
      GetXattrEx(iClass,"LegTerr","data-type")                     <>  ?   AND
      FGetSetting("ОпОтмыв","TerrCheck",?)                         = "ДА" THEN 
        DO:
          vTerrCh = (GetXattrValue("op-kind",vOpKind,"TerrCheck") = "Нет").
          IF NOT vTerrCh THEN
          vTerrCh = (GetXattrValue("op-template",vOpKind + "," + vOpTemplate,"TerrCheck") = "Нет").
          IF NOT vTerrCh THEN
            {except.i &Exception = "'TerrCheck'"}
        END.

   /* Commented by KSV: Валютный контроль */
   IF GetLstParam({&VP_VO_CONTROL},iParams,iValues,{&VP_PARAM_DLM}) <> "NO" AND
      GetXattrValueEx("op-kind", vOpkind, "ВалКонтр", "Да")         NE "Нет" THEN
      {except.i &Exception = "'ВалКонтр'"}
   
   /* Контроль соблюдений требований о резервировании при совершении вал.опер.*/
   IF FGetSetting("ТребРезервР", ?, ?)                        = "ДА" THEN 
      {except.i &Exception = "'ТребРезерв'"}
   
   /* Контроль корреспонденции счетов доходов и расходов для полупроводок */
   RUN DRCheck (vOp, OUTPUT vCorrOK, OUTPUT vErrAcct).
   IF vCorrOK EQ ? THEN
       RUN Fill-SysMes IN h_tmess ("", "core52", "", ""). /* контроль невозможен */
   ELSE
       IF NOT vCorrOK THEN
       DO:
           mVL_Except = "__core51(%s=" + vErrAcct + ")".                /* ошибка, операция запрещена */
           {except.i &OtherInfo = "'2'"}
       END.

   /*Проверка на совпадение с фигурантами из классификатора Стоп-листы*/
   IF ENTRY(1, FGetSetting("Стоп-листы","КонтрВводРед_Док",""), ";") EQ "Да" THEN
      {except.i &Exception = "'StopList'"}

   /* Если НП СчетУИН не пуст, и не аннулирование, то проверяем ДР УИН документа */
   IF FGetSetting("ПлатДок", "СчетУИН", "")  NE "" AND
      iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE NE "А"
   THEN DO:
      RUN chk-uin.p (STRING(iObject:buffer-FIELD("op"):BUFFER-VALUE), OUTPUT vOk).
      IF vOk THEN DO:
         {except.i &Exception = "'СчетУИН'"}
      END.
   END.

   /* Проверка действительности паспорта РФ клиента по базе ФМС */
   ASSIGN
      vFMS     = FGetSetting("ОбменФМС","ФМСРед_Док","")
      vFMS     = GetXAttrValueEx("op-kind", vOpKind, "ОбменФМС", vFMS)
      vDocID   = GetXAttrValueEx("op", STRING(vOp), "document-id", "")
   .
   IF vFMS EQ "Да" AND {assigned vDocID} AND NOT(GetSysConf("NoFmsDocChk") EQ "YES") 
   THEN DO:
      IF vLastAct NE "C" THEN DO:
         {cl-fmschk.i
            "'op'"
            "STRING(vOp)"
            "GetXAttrValueEx('op', STRING(vOp), 'document-id', '')"
            "GetXAttrValueEx('op', STRING(vOp), 'Докум', '')"
            "GetXAttrValueEx('op', STRING(vOp), 'Document4Date_vid', '')"
            "GetXAttrValueEx('op', STRING(vOp), 'ФИО', '')"
         }
      END.
   END.

   /* Контроль с черным списком документов (black-list) клиентов платежного документа */
   ASSIGN
     vSetBlack  = FGetSetting("black-list","КонтрВводРед_Док","Нет")
     vBlack1    = (ENTRY(1,vSetBlack,";") EQ "Да").
   IF vBlack1 THEN DO:
      IF NUM-ENTRIES(vSetBlack,";") > 1 THEN
         vBlack2 = ENTRY(2,vSetBlack,";").
      IF CAN-DO("вопрос,запрет",vBlack2) AND
         iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE NE "А"
      THEN DO:
         RUN chk-black-list.p (STRING(iObject:buffer-FIELD("op"):BUFFER-VALUE),
                               vBlack2,
                               OUTPUT vOk).
         IF vOk THEN DO:
           {except.i &Exception = "'Black-list'"}
         END.
      END.
   END.

   /* Если поле (101) не заполнено, либо НП ГНИ.ДопКонтр148 = Нет,
      контроль налоговых реквизитов не должен выполняться */
   IF FGetSetting("ГНИ", "ДопКонтр148", "Да")   <> "Нет" AND
      NOT DocImpUfebs(vOp)                               AND
      GetXattrValue("op", STRING(vOp), "ПокСТ") <> ""
   THEN DO:
      vPrevStat = GetSysConf("ПредСтатус").
                        /* Не выполняем контроль налоговых реквизитов при 
                        ** понижении статуса. */
      IF    NOT {assigned vPrevStat}
         OR vPrevStat LT iObject:BUFFER-FIELD("op-status"):BUFFER-VALUE THEN
      DO:
         /*
                           /* Контроль налоговых реквизитов. */
         RUN GhkNalAttrs IN THIS-PROCEDURE (vOp,OUTPUT vError).
         IF {assigned vError } THEN
         DO:
            IF FGetSetting("ГНИ", "ИНН-УИН", "Запрет")   EQ "Пред"
            THEN
               RUN Fill-SysMes IN h_tmess ("", "", "0", vError).
            ELSE
            DO:
               mVL_Except = vError.
               {except.i &OtherInfo = "'2'"}
            END.
         END.
         */
                        /* Проверка реквизитов плательщика и получателя 
                        ** для налоговых, таможенных и бюджетных платежей. */
         RUN chksgnnalplat.p (
            STRING (iObject:buffer-FIELD("op"):BUFFER-VALUE),
            OUTPUT vOk
         ).
         IF vOk THEN
            RETURN 'ERROR'.
      END.
   END.

   /*Глухов. Установка блокировки суммы для документов создаваемых в статусе ФКС*/
   DEFINE BUFFER kop FOR op.
   FIND FIRST kop WHERE kop.op = iObject:BUFFER-FIELD("op"):BUFFER-VALUE NO-LOCK NO-ERROR.
   IF AVAIL kop THEN
   DO:
      IF kop.op-status GE "ФБН" THEN
      DO:
         vOk = No.
         RUN kart_sum_block.p(BUFFER kop, "SET", OUTPUT vOk) NO-ERROR.
         IF vOk NE Yes THEN
         DO:
            RUN Fill-SysMes IN h_tmess ("", "", "-1", "Ошибка!").
            RETURN 'ERROR'.
         END.
      END.

      /*
      IF kop.op-status GE CHR(251) AND GetXattrValueEx("op",STRING(op.op),"БлокСуммКарт","") NE ""  THEN
         UpdateSigns(op.class-code,STRING(op.op),"БлокСуммКарт", "",?).
     */
   END.
   ELSE
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Ошибка создания документа!").
      RETURN 'ERROR'.
   END.

   RETURN .

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Обработчик исключений
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE v__e__handler:
   {&VPD_EH_PARAM}

   DEFINE VARIABLE bhAcct     AS HANDLE     NO-UNDO.
   DEFINE VARIABLE vOp        AS INT64    NO-UNDO.
   DEFINE VARIABLE vOpEntry   AS INT64    NO-UNDO.
   DEFINE VARIABLE vPos       AS INT64    NO-UNDO.
   DEFINE VARIABLE vFlagerr   AS INT64    NO-UNDO.
   DEFINE VARIABLE vOk        AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vTable     AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vTerrProc  AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bAcct     FOR acct.
   DEFINE BUFFER bOpEntry  FOR op-entry.

   oOtherInfo = "1".

   vTable = GetXclassProgress(iClass).
   vOp    = iObject:BUFFER-FIELD("op"):BUFFER-VALUE.

   IF vTable = "op-entry" THEN
   DO:
       IF NOT iException BEGINS "__" THEN
       DO:
         vOpEntry    = iObject:BUFFER-FIELD("op-entry"):BUFFER-VALUE.
      
         FIND FIRST bOpEntry WHERE 
            bOpEntry.op       = vOp       AND
            bOpEntry.op-entry = vOpEntry NO-LOCK  NO-ERROR.
      
         IF NOT AVAILABLE bOpEntry THEN 
         DO:
            iException = "Проводка не найдена. Ошибка обработки исключения: " + 
                         {unknown iException}.
            RETURN .
         END.
         
         bhAcct = WIDGET-HANDLE(GetLstParam({&VP_ACCT_HANDLE},iParams,
                                            iValues,{&VP_PARAM_DLM})) NO-ERROR. 

         IF VALID-HANDLE(bhAcct) THEN
            FIND FIRST bAcct WHERE ROWID(bAcct) = bhAcct:ROWID NO-LOCK NO-ERROR.

         IF AVAIL bAcct THEN
         DO:
            RUN err-event(BUFFER bOpEntry,BUFFER bAcct,iException,OUTPUT vFlagerr).
            IF vFlagerr <> 0 THEN 
            DO:
               oNoCont = YES.
               oOtherInfo = STRING(vFlagErr).
            END.
            oWasTreated = YES.
         END.
         ELSE
         DO:
            iException = "Cчет не найден. Ошибка обработки исключения: " + 
                         {unknown iException}.
            RETURN .
         END.
      END.
   END.
   
   CASE iException:
      WHEN "__core01" THEN 
      DO:
         RUN Fill-SysMes("","core01","","").
         IF pick-value <> "YES" THEN oNoCont = YES.
         oWasTreated = YES.
         RETURN .
      END.
      WHEN "TerrCheck" THEN
      DO:
         vTerrProc = GetCode("ОпОтмыв","7001").
         /* В классикаторе ОпОтмыв для кода 7001 есть настройка необходимой длины */
         IF     vTerrProc GT ""
            AND NUM-ENTRIES(vTerrProc,CHR(1)) GT 16  THEN DO:
            vTerrProc = ENTRY(17,vTerrProc,CHR(1)).
            IF NOT SearchPfile(vTerrProc) THEN
               vTerrProc = "".

         END.
         IF NOT {assigned vTerrProc} THEN
            vTerrProc = "lg7001".
         RUN VALUE(vTerrProc + '.p') (INPUT vOP, OUTPUT vOk).
         IF vOk THEN 
         DO:
             /* если вывод на экран отключен */
            IF GetProcSettingByCode("СС_ВыводНаЭкран") EQ "Нет" THEN
            /* вывод сообщения (в протокол) */
            RUN Fill-SysMes("","core3301","","").
            ELSE DO: /* иначе запрос пользователю */
               RUN Fill-SysMes("","core33","","").
               IF NOT (pick-value = "yes") THEN oNoCont = YES.
            END.
         END.
         oWasTreated = YES.
         RETURN .
      END.
      WHEN "StopList" THEN
      DO:
         RUN printstoplstlog.p(vOp, OUTPUT oNoCont).
         oWasTreated = YES.
         RETURN .            
      END.
      WHEN "ВалКонтр" THEN
      DO:
         RUN chk117i.p (vOP, OUTPUT vOk).
         IF vOk THEN oNoCont = YES.
         oWasTreated = YES.
         RETURN .
      END.
      WHEN "ТребРезерв" THEN
      DO:
         RUN b-ravrf.p (vOp).
         IF {&RETURN_VALUE} NE "" THEN oNoCont = YES.
         oWasTreated = YES.
         RETURN .
      END.
      WHEN "СчетУИН" THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","Отрицательный результат контроля УИН.").
         oNoCont = YES.
         oWasTreated = YES.
         RETURN.
      END.
      WHEN "Black-list" THEN
      DO:
         oNoCont = YES.
         oWasTreated = YES.
         RETURN.
      END.
   END CASE.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Вызов метода CHKUPD на метасхеме
  Parameters:  Oписание параметров см. в valid.def
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE v__method__check:
   {&VPD_MC_PARAM}

   DEFINE VARIABLE vParam  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmp    AS CHARACTER  NO-UNDO.

   IF GetFastCache(PROGRAM-NAME(1),iClass,OUTPUT vTmp) OR
      GetBQObjectType(iObject) <> {&OT_TABLERECORD} THEN RETURN .

   vParam = GetLstParam({&VP_EXT_PARAM} + iClass,iParams,iValues,
                        {&VP_PARAM_DLM}).
   IF NOT {assigned vParam} THEN vParam = "".

   /* Commented by KSV: Запускаем валидатор на метасхеме */
   RUN RunClassMethod IN h_xclass (iClass,"chkupd","","",?,
                                   STRING(iObject:RECID) + "," + vParam).
   IF NOT CAN-DO("no-method,no-proc",{&RETURN_VALUE}) AND 
      pick-value <> "YES" THEN 
      RETURN IF {assigned RETURN-VALUE} 
             THEN {&RETURN_VALUE}
             ELSE "Ошибка вызова метода CHKUPD для класса " + iClass.

   IF CAN-DO("no-method,no-proc",{&RETURN_VALUE})  THEN
      RUN SetFastCache IN h_cache (PROGRAM-NAME(1),iClass,"").

   RETURN .
END PROCEDURE.

/* вспомогательная функция для контроля корреспонденции счетов доходов/расходов в полупроводках*/

FUNCTION DRCheckBySide RETURNS LOGICAL (INPUT iSide             AS CHARACTER,
                                        INPUT iOp               AS INT64,
                                        INPUT iAcct             AS CHARACTER, 
                                        INPUT iCurr             AS CHARACTER,
                                        INPUT iExcList          AS CHARACTER, 
                                        INPUT-OUTPUT ioPrevF102 AS CHARACTER):
    
    DEFINE BUFFER xop-entry FOR op-entry.
    DEFINE VARIABLE vOK    AS LOGICAL   INIT YES NO-UNDO.
    DEFINE VARIABLE vF102  AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE vFound AS LOGICAL            NO-UNDO.

    vF102 = GetXattrValueEx ("acct", iAcct + "," + iCurr, "f102_cur", "no").
    IF ioPrevF102 NE "" THEN
    DO:
        IF vF102 NE ioPrevF102 THEN
        DO:
            vOK = ?.
            RETURN vOK.
        END.
    END.
    ELSE
        ioPrevF102 = vF102.
    vFound = ?.
    IF iSide = "DB" THEN
    FOR EACH xop-entry WHERE xop-entry.op      EQ iOp
                       AND   xop-entry.acct-cr EQ ?
                       AND   NOT CAN-DO (iExcList, xop-entry.acct-db)
      NO-LOCK:
      vFound = xop-entry.currency NE "".
      IF vFound THEN
         LEAVE.
    END.
    ELSE
    FOR EACH xop-entry WHERE xop-entry.op      EQ iOp
                       AND   xop-entry.acct-db EQ ?
                       AND   NOT CAN-DO (iExcList, xop-entry.acct-cr)
         NO-LOCK:
       vFound = xop-entry.currency NE "".
       IF vFound THEN
          LEAVE.
    END.
    IF vFound NE ? THEN
      IF vFound THEN
           vOK = (vF102 EQ "yes").
      ELSE
           vOK = (vF102 EQ "no"). /* если все счета в противоположном обороте рублевые, а f102_cur = yes,
                                  операция запрещена                                 */
    
    RETURN vOK.

END FUNCTION.



/* Контроль корреспонденции счетов доходов/расходов в полупроводках  */

PROCEDURE DRCheck.

DEFINE INPUT  PARAMETER iOp        AS INT64            NO-UNDO.
DEFINE OUTPUT PARAMETER oCorrect   AS LOGICAL   INIT YES NO-UNDO.
DEFINE OUTPUT PARAMETER oErrorAcct AS CHARACTER          NO-UNDO.

DEFINE BUFFER bop-entry FOR op-entry.

DEFINE VARIABLE vContrParams AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vContrCorr   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vExcList     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vMask        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vPrevF102    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE vCorrect AS LOGICAL INIT YES NO-UNDO.

vContrParams = fGetSetting("СтандТр","КонтКорр","").
vContrCorr = ENTRY (1, vContrParams, ";") EQ "Да".
IF NUM-ENTRIES (vContrParams, ";") GT 1 THEN
    vExcList = SUBSTRING (vContrParams, INDEX (vContrParams, ";") + 1).
vMask = REPLACE (fGetSetting("СтандТр","СчетаДхРсх",""), ";", ",").

IF vContrCorr THEN
DO:
FOR EACH bop-entry WHERE  bop-entry.op EQ iOp
         NO-LOCK:

    IF     CAN-DO (vMask,    bop-entry.acct-db) AND 
       NOT CAN-DO (vExcList, bop-entry.acct-db) AND 
       bop-entry.acct-cr  EQ ?                  AND 
       bop-entry.currency EQ "" THEN
    DO:
        vCorrect = vCorrect AND DRCheckBySide ("CR", iOp, bop-entry.acct-db, bop-entry.currency, vExcList, INPUT-OUTPUT vPrevF102).
        IF vCorrect EQ ? THEN
            LEAVE.
        ELSE IF NOT vCorrect THEN
            oErrorAcct = bop-entry.acct-db.
    END.
    

    IF     CAN-DO (vMask,    bop-entry.acct-cr) AND 
       NOT CAN-DO (vExcList, bop-entry.acct-cr) AND
       bop-entry.acct-db  EQ ?                  AND 
       bop-entry.currency EQ "" THEN
    DO:
        vCorrect = vCorrect AND DRCheckBySide ("DB", iOp, bop-entry.acct-cr, bop-entry.currency, vExcList, INPUT-OUTPUT vPrevF102).
        IF vCorrect EQ ? THEN
            LEAVE.
        ELSE IF NOT vCorrect THEN
            oErrorAcct = bop-entry.acct-cr.
    END.
END.

END.

oCorrect = vCorrect.

END PROCEDURE.

/* Определение строки балансового счета 2-го порядка по номеру лицевого счета */
FUNCTION getBalAcctStr RETURN CHARACTER (INPUT iAcct AS CHARACTER):
    RETURN SUBSTRING(iAcct, 1, 5).
END FUNCTION.

/* Проверка корреспонденции счетов в проводке                            */
/* по классификатору РазрешКоррСчетов.                                   */
/* Описание механизма хранения см. в файлах bal-corr.def и bal-corr.pro. */
PROCEDURE CheckAcctCorr.
    DEFINE INPUT  PARAMETER iAcctDb LIKE acct.acct  NO-UNDO.
    DEFINE INPUT  PARAMETER iAcctCr LIKE acct.acct  NO-UNDO.
    DEFINE INPUT  PARAMETER iPlAcct LIKE acct.acct  NO-UNDO.
    DEFINE INPUT  PARAMETER iPoAcct LIKE acct.acct  NO-UNDO.
    DEFINE INPUT  PARAMETER iOpDate LIKE op.op-date NO-UNDO.
    DEFINE OUTPUT PARAMETER oAllow  AS   LOGICAL    NO-UNDO.

    DEFINE VARIABLE vBalAcctDbStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vBalAcctCrStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vBalPlAcctStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vBalPoAcctStr AS CHARACTER NO-UNDO.

    DEFINE BUFFER code FOR code.

    ASSIGN
        vBalAcctDbStr = getBalAcctStr(iAcctDb)
        vBalAcctCrStr = getBalAcctStr(iAcctCr)
        vBalPlAcctStr = getBalAcctStr(iPlAcct)
        vBalPoAcctStr = getBalAcctStr(iPoAcct)
        oAllow        = YES
    .
    FOR EACH code WHERE
        code.class  = {&BalAcctCorrClass}         AND
        code.parent = {&BalAcctCorrClass}         AND
        CAN-DO(code.{&db-field}, vBalAcctDbStr)   AND
        CAN-DO(code.{&cr-field}, vBalAcctCrStr)   AND
        CAN-DO(code.{&plAcct-field}, vBalPlAcctStr)   AND
        CAN-DO(code.{&poAcct-field}, vBalPoAcctStr)   AND
        (NOT {assigned code.{&beg-date-field}}
         OR
         DATE(code.{&beg-date-field}) <= iOpDate) AND
        (NOT {assigned code.{&end-date-field}}
         OR
         DATE(code.{&end-date-field}) >= iOpDate)
    NO-LOCK
    BY INT64(code.{&priority-field}) DESCENDING
    BY code.{&db-field}              DESCENDING
    BY code.{&cr-field}              DESCENDING
    BY code.{&plAcct-field}          DESCENDING
    BY code.{&poAcct-field}          DESCENDING
    BY DATE(code.{&beg-date-field})  = ?
    BY DATE(code.{&beg-date-field})  DESCENDING
    BY DATE(code.{&end-date-field})
    BY code.{&allow-field}           DESCENDING:
        oAllow = NOT {assigned code.{&allow-field}}.
        LEAVE.
    END.
END PROCEDURE.


{pfuncdef
   &DEFPROC     = "GhkNalAttrs"
   &DESCRIPTION = "Проверка налоговых реквизитов документа"
   &PARAMETERS  = "iOp - код документа (op.op)
                   oErrorTxt - текст ошибки"
   &RESULT      = "oErrorTxt: заполнен - ошибка, пустое значение - нет ошибки"
   &SAMPLE      = "RUN GhkNalAttrs IN h_op (vOp, OUTPUT vErrorTxt)."}
   
PROCEDURE GhkNalAttrs  :
   DEFINE INPUT  PARAM  iOp        AS INT64      NO-UNDO.
   DEFINE OUTPUT PARAM  oErrorTxt  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vInnSend AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE vName    AS CHARACTER EXTENT 2 NO-UNDO.
   DEFINE VARIABLE vPOKST   AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE vUIN     AS CHARACTER          NO-UNDO.

   DEFINE BUFFER acct-db  FOR acct.
   DEFINE BUFFER op       FOR op.
   DEFINE BUFFER op-entry FOR op-entry.

   FIND FIRST op WHERE op.op EQ iOp NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN 
      RETURN.

   ASSIGN 
      vUIN     = GetXattrValueEx("op",STRING(iOp), "УИН",     "0")
      vPOKST   = GetXattrValueEx("op",STRING(iOp), "ПокСт",   "0")
      vInnSend = GetXattrValueEx("op",STRING(iOp), "inn-send","").


   IF   NOT CAN-DO(fGetSetting("ГНИ","СтатусИП",""),vPOKST)
     OR vUIN NE "0"  
   THEN
      RETURN.
   IF NOT {assigned vInnSend} THEN DO:
      FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
      IF NOT AVAIL op-entry THEN
         RETURN.
      {find-act.i &bact = acct-db
                  &acct  = op-entry.acct-db}
      IF NOT AVAIL acct-db THEN
         RETURN.
      RUN GetCustName IN h_Base  (acct-db.cust-cat, 
                                  acct-db.cust-id,
                                  "",
                                  OUTPUT vName[1],
                                  OUTPUT vName[2],
                                  INPUT-OUTPUT vInnSend).
   END.
   
   
   IF    (NOT {assigned vInnSend}  OR vInnSend EQ "0") 
    
   THEN 
      oErrorTxt = "Внимание! Для введенного значения поля (101) " + vPOKST + 
         " и отсутствия УИН в документе ИНН плательщика должен быть заполнен!".

   RETURN.

END PROCEDURE.
/* $LINTFILE='pp-op.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='mkv' */
/* $LINTDATE='08/04/2016 11:17:44.586+04:00' */
/*prosignNQ5BifLIvOIjtVTYa1e8sQ*/
/* --- pp-op.p was humbly modified by (c)blodd converter v.1.09 on 1/19/2017 8:26am --- */
