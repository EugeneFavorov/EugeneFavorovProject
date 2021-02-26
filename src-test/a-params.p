/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: a-params.p
      Comment: Возвращает дату отчета, код пользователя, код подразделения 
               По умолчанию определяет все по идентификатору смены ВОК
   Parameters: ID смены, заголовок формы
         Uses: нет
      Used by: нет
      Created: 19.03.2004 13:35 ligp    
     Modified: 05.04.2004 12:02 ligp     25339
     Modified: 15.04.2004 18:24 ligp     25345
     Modified: 07.05.2004 13:02 ligp     25343
     Modified: 15.07.2004 15:02 ligp     32087
     Modified: 24.09.2004 15:02 ligp     32092: Указание № 1433-У (Приложение 3)╓Сводная справка о
                                         кассовых оборотах·
     Modified: 29.09.2004 15:09 ligp     32095: Указание № 1433-У (Приложение 4)╓Книга учета ден
                                         наличности и др, ценностей·
     Modified: 02.11.2004 13:45 fedm     32607: Вызовы справочников и проверки
     Modified: 09.12.2004 11:30 rija      
     Modified: 25.10.2006 15:28 ELUS     0066832: ВОК Отчет "Справка о принятой и выданной
                                         наличности" - spr-cash
     Modified: 24.04.2007 14:01 elus     Формирование реестров операций после реализации 58567
     Modified: 24.01.2008 15:11 elus     81126
     Modified: 18.02.2008 18:35 elus      0101185: Разграничение отчетных данных ВОК дневной и
                                          вечерней кассы
*/

{globals.i}
{intrface.get vok}
{intrface.get sessions}
{intrface.get tmess}
{intrface.get date}

RUN Init-SysMes("","","").
/*------------------------------------------------------------------------------
  Назначение:  возвращает дату, код пользователя, код подразделения 
  Параметры:   iDprID   - Идентификатор смены 
               iTitle   - Заголовок формы выбора
               oDateRep - Дата ОД смены
               oUserRep - Код юзера
               oRecvRep - Код кассира - получателя
               oBrchRep - Код подразделения
               oPrmsOK  - Нормально ли все выбрали
               oBookEnd - Печатать ли заверительную надпись (нужно для книг учета)
               oBookTit - Печатать ли титульный лист (нужно для книг учета)               
               oBookTyp - Печатать ли книгу учета с оборотами или нет               
  Примечание:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER iDprID   AS CHARACTER NO-UNDO. /* Идентификатор смены */
DEFINE INPUT  PARAMETER iTitle   AS CHARACTER NO-UNDO. /* Заголовок формы */  
DEFINE OUTPUT PARAMETER oDateRep AS DATE NO-UNDO.      /* Дата отчета */
DEFINE OUTPUT PARAMETER oBrchRep LIKE branch.branch-id NO-UNDO. /* Код подразделения */   
DEFINE OUTPUT PARAMETER oBrchKey LIKE branch.branch-id NO-UNDO. /* Ключи от подразделения */   
DEFINE OUTPUT PARAMETER oUserRep LIKE _User._Userid NO-UNDO.       /* Код юзера */
DEFINE OUTPUT PARAMETER oRecvRep LIKE _User._Userid NO-UNDO.       /* Код получателя */
DEFINE OUTPUT PARAMETER oRecvDpr AS CHARACTER             NO-UNDO.
DEFINE OUTPUT PARAMETER oPrmsOK  AS LOGICAL INITIAL FALSE NO-UNDO. /* Все ли ОК */
DEFINE OUTPUT PARAMETER oBookEnd AS LOGICAL INITIAL FALSE NO-UNDO. /* Печатать ли заверительную надпись (нужно для книг учета)*/
DEFINE OUTPUT PARAMETER oBookTit AS LOGICAL INITIAL FALSE NO-UNDO. /* Печатать ли титульный лист (нужно для книг учета)*/
DEFINE OUTPUT PARAMETER oBookTyp AS LOGICAL INITIAL FALSE NO-UNDO. /* Печатать ли заверительную надпись (нужно для книг учета)*/
DEFINE OUTPUT PARAMETER oChBlank AS LOGICAL INITIAL FALSE NO-UNDO. /* Учитывать ли бланки (нужно для spr-cash)*/
DEFINE OUTPUT PARAMETER oDprID   AS CHARACTER NO-UNDO.      /* Id Смены */
DEFINE OUTPUT PARAMETER oBalAcct AS INT64   NO-UNDO.      /* Счет 2-ого порядка */
DEFINE OUTPUT PARAMETER oItogEkv AS LOGICAL INITIAL FALSE NO-UNDO. /* Итог в эквиваленте */
DEFINE OUTPUT PARAMETER oNameCen AS LOGICAL INITIAL FALSE NO-UNDO. /* Наименование ценностей */
DEFINE OUTPUT PARAMETER oMoreTit AS LOGICAL INITIAL FALSE NO-UNDO. /* Печатать ли  отдельные сшивы в v-tit-pg318p */
DEFINE OUTPUT PARAMETER oPrnAcct AS LOGICAL INITIAL FALSE NO-UNDO. /* Печатать ли лицивые счета (book-reg318p) */
DEFINE OUTPUT PARAMETER oCashOrd AS LOGICAL INITIAL FALSE NO-UNDO. /* Подсчёт кол-ва по сводным ордерам */
DEFINE OUTPUT PARAMETER oTotalCS AS LOGICAL INITIAL FALSE NO-UNDO. /* Итоги по КС */
DEFINE OUTPUT PARAMETER oByRate  AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oRstPril AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oByInsp  AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oRazbIsp AS LOGICAL INITIAL FALSE NO-UNDO. /* Разбивка по исполнителям */
DEFINE OUTPUT PARAMETER oDateOtc AS CHARACTER             NO-UNDO.
DEFINE OUTPUT PARAMETER oFullItg AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oShowZero AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER mSessFlt AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oNCurrency AS LOGICAL  NO-UNDO.
DEFINE OUTPUT PARAMETER oICurrency AS LOGICAL  NO-UNDO.
DEFINE OUTPUT PARAMETER oRCurrency AS LOGICAL  NO-UNDO.
DEFINE OUTPUT PARAMETER oInUse   AS LOGICAL INITIAL TRUE  NO-UNDO.
DEFINE OUTPUT PARAMETER oPK      AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oByPrAcct  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER oTitPage AS LOGICAL               NO-UNDO.
DEFINE OUTPUT PARAMETER oNextDay AS LOGICAL  INIT YES     NO-UNDO.

DEFINE VARIABLE vBookEnd  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vBookTit  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vBookTyp  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vChBlank  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vItogEkv  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vNameCen  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vMoreTit  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vPrnAcct  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vCashOrd  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vTotalCS  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vByRate   AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRstPril  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vByInsp   AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRazbIsp  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRazbDat  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vFullItg  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vShowZero AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vNCurrency AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vICurrency AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vRCurrency AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE mInUse    AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE mPK       AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vTitPage  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.
DEFINE VARIABLE vNextDay  AS LOGICAL VIEW-AS TOGGLE-BOX NO-UNDO.

DEFINE VARIABLE mByPrAcct AS CHARACTER FORMAT "X(8)" 
  VIEW-AS COMBO-BOX INNER-LINES 4
  LIST-ITEMS "","Дневные", "Вечерние" 
  DROP-DOWN-LIST
  NO-UNDO.

DEFINE VARIABLE vSessFlt AS CHARACTER FORMAT "X(8)" 
  VIEW-AS COMBO-BOX INNER-LINES 4
  LIST-ITEMS "Нет","Все","Дневные", "Вечерние" 
  DROP-DOWN-LIST
  NO-UNDO.

DEFINE VARIABLE mProgramName AS CHARACTER           NO-UNDO.
DEFINE VARIABLE mDprList     AS CHARACTER           NO-UNDO.
DEFINE VARIABLE mDprListType AS CHARACTER INIT 1    NO-UNDO. /* "list" - смен несколько,
                                                                "*"    - все, 
                                                                "1"    - одна */
DEFINE VARIABLE vDateOtc     AS DATE                NO-UNDO.
DEFINE VARIABLE mDateBeg     AS DATE                NO-UNDO.
DEFINE VARIABLE mDateEnd     AS DATE                NO-UNDO.

ASSIGN
   vItogEkv  = oItogEkv
   vChBlank  = oChBlank
   vBookTyp  = oBookTyp
   vBookTit  = oBookTit
   vBookEnd  = oBookEnd
   vNameCen  = oNameCen
   vMoreTit  = oMoreTit
   vPrnAcct  = oPrnAcct
   vCashOrd  = oCashOrd
   vTotalCS  = oTotalCS
   vRazbIsp  = oRazbIsp   
   vShowZero = oShowZero   
   vNCurrency = oNCurrency
   vICurrency = oICurrency   
   vRCurrency = oRCurrency   
   vTitPage  = oTitPage
   vNextDay  = oNextDay
   mProgramName = ENTRY(1, PROGRAM-NAME(2), ".")
   .
IF iTitle = "" THEN
   iTitle = "ВЫБОР ПАРАМЕТРОВ ОТЧЕТА".

FORM /* === Форма корректировки параметров отчета === */
   oDateRep LABEL "Дата ОД смены"
            FORMAT "99/99/9999" HELP "Дата опердня смены"
            AT ROW 1 COL 2

   oDprID   LABEL "Код смены" 
            FORMAT "x(7)"       HELP "Код смены"
            AT ROW 1 COL 30

   vDateOtc LABEL "Дата отчета"
            FORMAT "99/99/9999" HELP "Дата отчета"
            AT ROW 2 COL 4

   mDateBeg LABEL "Дата начала"
            FORMAT "99/99/9999" HELP "Дата начала"
            AT ROW 4 COL 30

   mDateEnd LABEL "Дата оконч."
            FORMAT "99/99/9999" HELP "Дата окончания"
            AT ROW 6 COL 30

   vRazbDat LABEL "Разбивка по датам"
                                HELP "Разбивка по датам"
            AT ROW 2 COL 30
   
   vSessFlt LABEL "Быстрая фильтрация смен"
            HELP "Фильтрация по типам смен"
            AT ROW 3 COL 16

   oBrchRep LABEL "Код кассы  "
            FORMAT "x(10)"      HELP "Код подразделения"
            AT ROW 4 COL 2

   vBookTit LABEL "Титульный лист" 
            FORMAT "x(10)"      HELP "Печатать ли титульный лист"
            AT ROW 4 COL 30

   vTitPage LABEL "Отд. лист"
            FORMAT "x(10)"      HELP "Печать титульного листа на отдельном листе"
            AT ROW 5 COL 30

   oBalAcct LABEL "Бал. счёт"
            FORMAT ">>>>>"      HELP "Внебалансовый счёт 2-го порядка"
            AT ROW 4 COL 30

   oUserRep LABEL "Код кассира"
            FORMAT "x(10)"      HELP "Код кассира"
            AT ROW 6 COL 2
   
   oRecvDpr LABEL "Принимает"                   
            FORMAT "x(5)"       HELP "Код смены"
            AT ROW 6 COL 30

   oRecvRep LABEL "Код кассира"
            FORMAT "x(10)"      HELP "Код кассира, принимающего ценности"
            AT ROW 6 COL 47

   vRazbIsp LABEL "Разбивка по исполнителям"
                                HELP "Разбивка по исполнителям"
            AT ROW 7 COL 2
   
   vNCurrency LABEL "Рублевая"
                                HELP "Включать проводки в рублях"
            AT ROW 7 COL 2

   vICurrency LABEL "Валютная"
                                HELP "Включать проводки в валюте"
            AT ROW 7 COL 15
   vRCurrency LABEL "Валютная в руб. эквиваленте"
                                HELP "Включать проводки в валюте c пересчетом"
            AT ROW 7 COL 28

   vItogEkv LABEL "Итог в Эквивал."
                                HELP "Печатать ли общий итог"
            AT ROW 6 COL 30

   vPrnAcct LABEL "Лицевые счета"
                                HELP "Печатать ли лицевые счета"
            AT ROW 6 COL 30

   mByPrAcct LABEL "Признак счета"
             HELP "Фильтрация по признаку счета Дневной/Вечерний"
             AT ROW 7 COL 30

   vNextDay LABEL "Остаток на след. ОД "
                                HELP " Отображать дату следующего за опер. днем"
            AT ROW 7 COL 2

   vByInsp  LABEL "Групп. По бух. работникам"
                                HELP "Группировка по бухгалтерским работникам"

            AT ROW 6 COL 30
   vBookEnd LABEL " Заверительная надпись" 
                                HELP "Печатать ли заверительную надпись"
            AT ROW 8 COL 2

   oBrchKey LABEL "Ключи от   "
                                HELP "Подразделение от которого в Хранилище хранятся дубликаты ключей"
            AT ROW 8 COL 2

   vChBlank LABEL " С чеками"
                                HELP "Печатать ли данные по чекам"
            AT ROW 8 COL 2

   vMoreTit LABEL " Отдельные сшивы " 
                                HELP "Печатать ли отдельные сшивы "
            AT ROW 8 COL 2

   vBookTyp LABEL " С оборотами" 
                                HELP "Печатать ли книгу с оборотами по счетам"
            AT ROW 8 COL 30

   vCashOrd LABEL " По свод. ордерам"
                                HELP "Подсчет сводных ордеров"
            AT ROW 8 COL 30
   vTotalCS LABEL " Итоги по КС"
                                HELP "Итоги по кассовым символам"
            AT ROW 4 COL 30
   vNameCen LABEL "Наим. ценностей" 
            FORMAT "x(10)"      HELP "Печатать ли наименования ценностей"
            AT ROW 8 COL 30
   vByRate  LABEL " Итоги по курсам"
                                HELP "Итоги по курсу"
            AT ROW 4 COL 30
   vFullItg LABEL " Общие итоги"
                                HELP "Общие итоги "
            AT ROW 4 COL 50            
   vRstPril LABEL " Вывод приложения к реестру"
                                HELP "Вывод приложения к реестру"
            AT ROW 6 COL 30
   vShowZero LABEL " Отражать нулевые остатки"
                                HELP "Отражать нулевые остатки "
            AT ROW 8 COL 30
   mInUse   LABEL " Признак монет в обращение"
                                HELP "Монеты в обращение"
            AT ROW 4 COL 30
   mPK      LABEL " Оплата ПК"
                                HELP "Оплата пластиковой картой"
            AT ROW 6 COL 30
WITH FRAME FrameInpParams CENTERED ROW 10 OVERLAY SIDE-LABELS
   TITLE COLOR bright-white "[ " + iTitle + " ]" WIDTH 72.

ASSIGN
   vTotalCS:CHECKED = fGetSetting("КасЖурВОК","ПарЗапКС","Нет")  =  "Да"
   vRazbIsp:CHECKED = fGetSetting("КасЖурВОК","ПарЗапИсп","Нет") =  "Да"
   mInUse:CHECKED   = YES
   vNextDay:CHECKED = YES
   vTitPage:CHECKED = YES
   .

/* =============== Фильтрация смен ================================ */
ON F1,ANY-PRINTABLE OF vSessFlt IN FRAME FrameInpParams
DO:
   RETURN NO-APPLY.
END.

/* =============== Выбор опердня ================================== */
ON F1 OF oDateRep IN FRAME FrameInpParams
DO:
   RUN op-date.p (6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.
END.

ON F1 OF vDateOtc IN FRAME FrameInpParams
DO:
   RUN calend.p.

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF vDateOtc IN FRAME FrameInpParams
DO:
   ASSIGN vDateOtc.
   IF vDateOtc <> ? THEN
   DO:
      ASSIGN
         vRazbDat                                   = NO
         vRazbDat:SCREEN-VALUE                      = "no"
         vRazbDat:SENSITIVE IN FRAME FrameInpParams = NO
         mDateBeg                                   = ?
         mDateEnd                                   = ?
         mDateBeg:SCREEN-VALUE                      = ""
         mDateEnd:SCREEN-VALUE                      = ""
         mDateBeg:SENSITIVE IN FRAME FrameInpParams = NO
         mDateEnd:SENSITIVE IN FRAME FrameInpParams = NO
      .
   END.
   ELSE
   DO:
      ASSIGN
         vRazbDat:SENSITIVE IN FRAME FrameInpParams = YES
         mDateBeg:SENSITIVE IN FRAME FrameInpParams = YES
         mDateEnd:SENSITIVE IN FRAME FrameInpParams = YES.
      /* ENABLE vRazbDat WITH FRAME FrameInpParams. */
      /* APPLY "TAB" TO vRazbDat. */
   END.
END.

ON F1 OF mDateBeg IN FRAME FrameInpParams
DO:
   RUN calend.p.

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mDateBeg IN FRAME FrameInpParams
DO:
   ASSIGN mDateBeg.
END.

ON F1 OF mDateEnd IN FRAME FrameInpParams
DO:
   RUN calend.p.

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF mDateEnd IN FRAME FrameInpParams
DO:
   ASSIGN mDateEnd.
END.

ON LEAVE OF vRazbDat IN FRAME FrameInpParams
DO:
   ASSIGN vRazbDat.
   IF vRazbDat THEN
   DO:
      ASSIGN
         vDateOtc                                   = ?
         vDateOtc:SCREEN-VALUE                      = ""
         vDateOtc:SENSITIVE IN FRAME FrameInpParams = NO
      .
   END.
   ELSE
   DO:
      ASSIGN
         vDateOtc                                   = ?
         vDateOtc:SCREEN-VALUE                      = ""
         vDateOtc:SENSITIVE IN FRAME FrameInpParams = YES
      .
      ENABLE vDateOtc WITH FRAME FrameInpParams.
   END.
END.

/* =============== Выбор смены ============================ */
ON '*' OF oDprId IN FRAME FrameInpParams
DO:
   ASSIGN
      SELF:SCREEN-VALUE     = "*"
      mDprListType          = "*".
   IF vDateOtc =  ? AND mDateBeg =  ? THEN 
   ASSIGN
      mDateBeg              = TODAY - 1
      mDateBeg:SCREEN-VALUE = String(TODAY - 1,"99/99/9999").
   IF vDateOtc =  ? AND mDateEnd =  ? THEN 
   ASSIGN
      mDateEnd              = TODAY - 1
      mDateEnd:SCREEN-VALUE = String(TODAY,"99/99/9999").
   RETURN.
END.

ON F1,ANY-PRINTABLE OF oDprId IN FRAME FrameInpParams
DO:
   DEFINE VARIABLE mParDate1  AS DATE       NO-UNDO.
   DEFINE VARIABLE mParDate2  AS DATE       NO-UNDO.

   ASSIGN
      mParDate1 = oDateRep
      mParDate2 = mParDate1
      .
   DO WITH FRAME FrameInpParams:
     IF (oRecvDpr:HIDDEN =  NO) AND (oRecvRep:HIDDEN =  NO) AND iTitle BEGINS "АКТ" THEN
     DO: /* Акт передачи наличности и бланков */
        ASSIGN 
           mParDate1 = PrevWorkDay(mParDate1)
           mParDate2 = NextWorkDay(mParDate2)
           .
     END.
   END.
   
   pick-value = "".
   RUN browseld.p ("sessions",
                   "op-date1"          + CHR(1) + "op-date2"         + CHR(1) + "ActionLock"          + CHR(1) + "Return",
                   STRING(mParDate1)   + CHR(1) + STRING(mParDate2)  + CHR(1) + "ENTER,F1,INS,DEL,F8" + CHR(1) + "seslist",
                   "ActionLock"        + CHR(1) + "Return",
                   5).

   IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN
   DO:
      ASSIGN
         SELF:SCREEN-VALUE = IF NUM-ENTRIES(pick-value) > 1 THEN "list" ELSE pick-value
         mDprListType      = IF NUM-ENTRIES(pick-value) > 1 THEN "list" ELSE "1"
         mDprList          = pick-value
      .
      APPLY "LEAVE" TO SELF.
   END.
   RETURN NO-APPLY.
END.

ON F1,ANY-PRINTABLE OF oRecvDpr IN FRAME FrameInpParams
DO:
   DEFINE VARIABLE mParDate1  AS DATE       NO-UNDO.
   DEFINE VARIABLE mParDate2  AS DATE       NO-UNDO.

   ASSIGN
      mParDate1 = oDateRep
      mParDate2 = mParDate1
      .
   DO WITH FRAME FrameInpParams:
     IF (oRecvDpr:HIDDEN =  NO) AND (oRecvRep:HIDDEN =  NO) AND iTitle BEGINS "АКТ" THEN
     DO: /* Акт передачи наличности и бланков */
        ASSIGN 
           mParDate1 = PrevWorkDay(mParDate1)
           mParDate2 = NextWorkDay(mParDate2)
           .
     END.
   END.
   
   pick-value = "".
   RUN browseld.p ("sessions",
                   "op-date1"          + CHR(1) + "op-date2"         + CHR(1) + "ActionLock"          + CHR(1) + "Return",
                   STRING(mParDate1)   + CHR(1) + STRING(mParDate2)  + CHR(1) + "ENTER,F1,INS,DEL,F8" + CHR(1) + "seslist",
                   "",
                   5).

   IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN
   DO:
      FIND FIRST sessions WHERE 
         sessions.dpr-id =  INT64(pick-value)
         NO-LOCK NO-ERROR.
      IF AVAILABLE sessions THEN
      DO:
         ASSIGN
            SELF:SCREEN-VALUE = pick-value
            oRecvDpr          = pick-value
            oRecvRep:SCREEN-VALUE IN FRAME FrameInpParams = sessions.user-id
            oRecvRep          = sessions.user-id
            .
      END.
   END.
   RETURN NO-APPLY.
END.

/* =============== Переопределение параметров ============================ */
ON LEAVE OF oDprId IN FRAME FrameInpParams
DO:
   DEFINE VARIABLE vIntDpr AS INT64     NO-UNDO.
   DEFINE VARIABLE vOpDate AS DATE        NO-UNDO.
   DEFINE VARIABLE i       AS INT64     NO-UNDO.

   CASE vSessFlt:SCREEN-VALUE IN FRAME FrameInpParams:
      WHEN "Нет" THEN
      DO:
         mDprListType = mDprListType.
      END.
      WHEN "Дневные" OR WHEN "Вечерние" OR WHEN "Все" THEN
      DO:
         mDprListType = "*".
      END.
   END CASE.
   CASE mDprListType:
      WHEN "1" THEN
      DO:
         ASSIGN
            oDprId   = SELF:SCREEN-VALUE
            vIntDpr  = INT64(oDprId) NO-ERROR
            .
         FIND FIRST sessions WHERE 
                    sessions.dpr-id =  vIntDpr 
            NO-LOCK NO-ERROR.
         IF AVAIL sessions THEN
            ASSIGN
               oBrchRep:SCREEN-VALUE = sessions.Branch-Id
               oUserRep:SCREEN-VALUE = sessions.user-id
            .
      END.
      WHEN "*" THEN
      DO:
         IF iTitle =  "ЖУРНАЛ НАЛИЧНОСТИ,ПЕРЕДАННОЙ МЕЖДУ КАССИРАМИ" THEN
         DO:
            RUN Fill-SysMes("",
                            "",
                            0,
                            "Для отчета ~"Журнал наличности, переданной между кассирами~"
                             необходимо указывать конкретную смену.").
            RETURN NO-APPLY SELF:NAME.
         END.

         CASE vSessFlt:SCREEN-VALUE IN FRAME FrameInpParams:
            WHEN "Все" THEN
            DO:
               ASSIGN
                  oDprId   = "*"
                  mDprList = "*".
            END.
            WHEN "Дневные" THEN
            DO:
               ASSIGN
                  oDprId   = "*"
                  mDprList = GetListOpenSessionsDV(work-module,oDateRep,"Д",oBrchRep:SCREEN-VALUE)
                  .
            END.
            WHEN "Вечерние" THEN
            DO:
               ASSIGN
                  oDprId   = "*"
                  mDprList = GetListOpenSessionsDV(work-module,oDateRep,"В",oBrchRep:SCREEN-VALUE)
                  .
            END.
            OTHERWISE
            DO:
               oDprId = "*".
            END.
         END CASE.
      END.
      WHEN "list" THEN
      DO:
         vIntDpr = INT64(ENTRY(1,mDprList)) NO-ERROR.
         FIND FIRST sessions WHERE
                    sessions.dpr-id =  vIntDpr
            NO-LOCK NO-ERROR.
         IF AVAIL sessions THEN
            ASSIGN
               oBrchRep:SCREEN-VALUE = sessions.Branch-Id
               vOpDate               = sessions.op-date
            .
         DO i = 2 TO NUM-ENTRIES(oDprId):
            vIntDpr = INT64(ENTRY(i,oDprId)) NO-ERROR.
            FIND FIRST sessions WHERE
                       sessions.dpr-id =  vIntDpr
               NO-LOCK NO-ERROR.
            IF     AVAIL sessions 
               AND (   sessions.op-date   <> vOpDate
                    OR sessions.Branch-Id <> oBrchRep:SCREEN-VALUE) THEN
            DO:
               RUN Fill-SysMes("",
                               "",
                               0,
                               "Смены должны относится к одному подразделению и быть открыты в одном опер. дне").
               ASSIGN
                  oDprId            = ""
                  mDprList          = ""
                  SELF:SCREEN-VALUE = ""
                  .
               RETURN NO-APPLY.
            END.
         END.
      END.
   END CASE.
END.

/* =============== Выбор подразделения ============================ */
ON F1 OF oBrchRep,oBrchKey IN FRAME FrameInpParams
DO:
   RUN vok-bran.p (6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* =============== Проверка введенного кода подразделения ========= */
ON LEAVE OF oBrchRep IN FRAME FrameInpParams
DO:
   DEF VAR vBranch AS CHAR  NO-UNDO.

   vBranch = SELF:SCREEN-VALUE.

   IF vBranch <> ""   AND
      vBranch <> "*"  AND
      NOT BranchIsVOK(vBranch) THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      0,
                      "Подразделение " + vBranch + " не является кассой.").
      RETURN NO-APPLY SELF:NAME.
   END.
   RETURN.
END.

/* =============== Проверка введенного кода подразделения ========= */
ON LEAVE OF oBrchKey IN FRAME FrameInpParams
DO:
   DEF VAR vBranch AS CHAR  NO-UNDO.

   vBranch = SELF:SCREEN-VALUE.

   IF NOT BranchIsVOK(vBranch) THEN
   DO:    
      RUN Fill-SysMes("",
                      "",
                      0,
                      "Подразделение " + vBranch + " не является кассой.").
      RETURN NO-APPLY SELF:NAME.
   END.
   RETURN.
END.

/* =============== Проверка введенного балансового счета ========= */
ON LEAVE OF oBalAcct IN FRAME FrameInpParams
DO:
   DEF VAR vBalAcct AS INT64  NO-UNDO.

   vBalAcct = INT64(SELF:SCREEN-VALUE).

   FIND FIRST bal-acct WHERE 
              bal-acct.bal-acct =  vBalAcct 
          AND bal-acct.acct-cat =  "o"
      NO-LOCK NO-ERROR.
   IF NOT AVAIL bal-acct THEN
   DO:
      RUN Fill-SysMes("",
                      "",
                      0,
                      "Введено не верное значение счета второго порядка").
      RETURN NO-APPLY SELF:NAME.
   END.
   RETURN.
END.

/* =============== Выбор балансового счета ======================== */
ON F1 OF oBalAcct IN FRAME FrameInpParams
DO:
   RUN browseld("bal-acct",
                "acct-cat",
                "o",
                "acct-cat",
                6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.

END.

/* =============== Выбор кассира ВОК ============================== */
ON F1 OF oUserRep,oRecvRep IN FRAME FrameInpParams
DO:
   RUN vok-user.p (6).

   IF LASTKEY =  10 AND pick-value <> ? THEN
   DO:
      SELF:SCREEN-VALUE = pick-value.
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* =============== Проверка введенного кода кассира =============== */
ON LEAVE OF oUserRep,oRecvRep IN FRAME FrameInpParams
DO:
   DEF VAR vUser AS CHAR  NO-UNDO.
   
   DEFINE BUFFER sessions FOR sessions. 
   
   FIND FIRST sessions WHERE sessions.dpr-id =  INT64(iDprID)
      NO-LOCK NO-ERROR. 

   vUser = SELF:SCREEN-VALUE.
   
   IF AVAIL sessions AND sessions.user-id <> vUser THEN
      IF vUser <> ""   AND
         vUser <> "*"  AND
         NOT UserIsCashier (vUser) THEN
      DO:
         RUN Fill-SysMes("",
                         "",
                         0,
                         "Сотрудник " +  vUser + " не является кассиром.").
         RETURN NO-APPLY SELF:NAME.
      END. 
   RETURN.
END.

/* ============ И то и то одновременно не сработает =============== */
ON LEAVE OF vChBlank IN FRAME FrameInpParams
DO:
   IF vChBlank:CHECKED THEN
      vCashOrd:CHECKED = NO.
END.

ON LEAVE OF vCashOrd IN FRAME FrameInpParams
DO:
   IF vCashOrd:CHECKED THEN
      ASSIGN
         vChBlank:CHECKED = NO
         vMoreTit:CHECKED = NO
         vTotalCS:CHECKED = NO
         .
END.

ON LEAVE OF vTotalCS IN FRAME FrameInpParams
DO:
   IF vTotalCS:CHECKED THEN
      vCashOrd:CHECKED = NO.
END.

ON LEAVE OF vMoreTit IN FRAME FrameInpParams
DO:
   IF vMoreTit:CHECKED THEN
      vCashOrd:CHECKED = NO.
END.

/* =============== Контроль и сохранение введённых данных ========= */
ON GO OF FRAME FrameInpParams ANYWHERE
DO:
   APPLY "LEAVE" TO oDprId.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO oBrchRep.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO oUserRep.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO mDateBeg.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   APPLY "LEAVE" TO mDateEnd.
   IF RETURN-VALUE <> "" THEN
      RETURN NO-APPLY.

   IF oBrchKey:VISIBLE THEN
   DO:
      APPLY "LEAVE" TO oBrchKey.
      IF RETURN-VALUE <> "" THEN
         RETURN NO-APPLY.
   END.
   IF oBalAcct:VISIBLE THEN
   DO:
      APPLY "LEAVE" TO oBalAcct.
      IF RETURN-VALUE <> "" THEN
         RETURN NO-APPLY.
   END.

   IF oRecvRep:VISIBLE THEN
   DO:
      APPLY "LEAVE" TO oRecvRep.
      IF RETURN-VALUE <> "" THEN
         RETURN NO-APPLY.
   END.

   ASSIGN
      mByPrAcct
      oBrchRep
      oBrchKey
      oUserRep
      oRecvRep
      oBalAcct.
END.

/* ============== Выставляем параметры отбора по умолчанию ======== */
oDateRep = gend-date.  /* Выбираем дату отчета - опер. день */
IF oDateRep = ? THEN 
   oDateRep = TODAY.

IF iDprID = ? THEN 
DO: /* Если вызов идет не из смены ВОК, узнаем параметры запуска отчета по логину */
   oUserRep = USERID("bisquit").
   oBrchRep = GetUserXAttrValue(oUserRep, "Отделение").
   oDprID   = "*".
END.
ELSE 
DO: /* Если вызов идет из смены ВОК, узнаем параметры запуска отчета по смене ВОК */
   oUserRep = GetUserIDFromSessions(INT64(iDprID)).
   oBrchRep = GetBranchIDFromSessions(INT64(iDprID)).
   oDprID   = iDprID.
END.
IF iTitle BEGINS "СВОД" THEN
DO: /* Если отчет "Сводный", то по умолчанию пытаемся по всем кассирам выполнять */
   oUserRep = "*".
   oDprID   = "*".
END.

oRecvRep = oUserRep. /* Пока так. Что второй кассир не пустой был */

/* =========================== Main block ============================*/
PAUSE 0.

FrameInpParams: /* Заполняем параметры отбора */
DO TRANSACTION ON ERROR  UNDO FrameInpParams, LEAVE FrameInpParams 
               ON ENDKEY UNDO FrameInpParams, LEAVE FrameInpParams:

   DISPLAY 
      oDprId
      vSessFlt  
      oDateRep
      vDateOtc
      vRazbDat
      oBrchRep
      oBrchKey
      oUserRep
      oRecvDpr
      oRecvRep      
      oBalAcct
   WITH FRAME FrameInpParams.

   IF oDprID = ? THEN /* Вызов из опер дня см. комментарий к заявке 0025339 */
   DO:
      ENABLE
         oDprId
         vSessFlt 
         oDateRep
         vDateOtc
         vRazbDat
         oBrchRep
         oBrchKey
         oUserRep
         oRecvDpr
         oRecvRep         
      WITH FRAME FrameInpParams.
   END.
   ELSE               /* Вызов из смены ВОК см. комментарий к заявке 0025339 */
   DO:
      ENABLE 
         oDprId
         vDateOtc
         vRazbDat
         vSessFlt 
         oUserRep
         oRecvDpr
         oRecvRep
      WITH FRAME FrameInpParams.
   END.

   IF work-module =  "hran" THEN
      HIDE vSessFlt
        IN FRAME FrameInpParams.
   /*
   IF mProgramName EQ "jou-ann-pl"THEN
      HIDE 
      vDateOtc
      vSessFlt
      vRazbDat
        IN FRAME FrameInpParams.
  */

  /* В большинстве отчетов эти параметры не используются */
   HIDE 
      vBookEnd 
      vMoreTit   
      vBookTit 
      vNameCen 
      vBookTyp 
      oRecvDpr
      oRecvRep 
      vChBlank 
      vByInsp
      vItogEkv 
      vPrnAcct 
      mByPrAcct
      vCashOrd
      vTotalCS
      vByRate
      vFullItg      
      vRstPril
      oBalAcct 
      oBrchKey
      vRazbIsp
      vShowZero
      vNCurrency
      vICurrency
      vRCurrency
      mInUse
      mPK
      mDateBeg
      mDateEnd
      vTitPage
      vNextDay
   IN FRAME FrameInpParams.

   IF CAN-DO("rst-cm,r-rinvok,book-reg,joukasin,joukasout,book-val,rst-136i,rst-opn,cas-svod,spr-cash,v-tit-pg",mProgramName) THEN
      HIDE 
         vDateOtc
         vRazbDat
         IN FRAME FrameInpParams.      

   CASE iTitle:
      WHEN "ОПИСЬ ДУБЛИКАТОВ КЛЮЧЕЙ" THEN
         ENABLE
            oBrchKey
         WITH FRAME FrameInpParams.

      WHEN "АКТ ОБ УНИЧТОЖЕНИИ ЦЕННОСТЕЙ" THEN
         ENABLE
            oBalAcct
         WITH FRAME FrameInpParams.

      WHEN "СПРАВКА О НАЛИЧНОСТИ" THEN
         ENABLE
            vChBlank
         WITH FRAME FrameInpParams.
      WHEN "ОТЧЕТНАЯ СПРАВКА" THEN
      DO:
         vByInsp:CHECKED = YES.
         ENABLE
            vChBlank
            vByInsp
         WITH FRAME FrameInpParams.
      END.

      WHEN "ТИТУЛЬНЫЙ ЛИСТ КАССОВЫХ ДОКУМЕНТОВ ДНЯ(318-П)" THEN
         ENABLE 
            vMoreTit
         WITH FRAME FrameInpParams.

      WHEN "КОНТРОЛЬНЫЙ ЛИСТ" THEN
         ENABLE
            vNameCen
         WITH FRAME FrameInpParams.

      WHEN "КНИГА УЧЕТА ПРИНЯТЫХ И ВЫДАННЫХ ДЕНЕГ" THEN
         ENABLE
            vBookEnd
            vNameCen
         WITH FRAME FrameInpParams.

      WHEN "КНИГА УЧЕТА ДЕНЕЖНОЙ НАЛИЧНОСТИ" THEN
         ENABLE
            vBookEnd
            vBookTit
            vItogEkv
            vBookTyp
         WITH FRAME FrameInpParams.

      WHEN "КНИГА ХРАНИЛИЩА ЦЕННОСТЕЙ" THEN
         ENABLE
            vNextDay
            vBookEnd
            vBookTit
            vTitPage
            vPrnAcct
            mByPrAcct
            vShowZero
         WITH FRAME FrameInpParams.

      WHEN "ЖУРНАЛ ИЗМЕНЕННЫХ ОПЕРАЦИЙ" THEN DO:
         ENABLE
            oBrchRep
            mDateBeg
            mDateEnd
         WITH FRAME FrameInpParams.
         HIDE 
            vRazbDat
            vSessFlt
         IN FRAME FrameInpParams.      

      END.
      WHEN "КНИГА УЧЕТА ДМ" THEN
         ENABLE
            vBookTit
         WITH FRAME FrameInpParams.

      OTHERWISE
      DO:
         IF iTitle BEGINS "АКТ" THEN
         DO:
            oRecvRep = "".
            DISPLAY 
               oRecvRep
               WITH FRAME FrameInpParams.

            ENABLE
               oRecvDpr
               oRecvRep
            WITH FRAME FrameInpParams.
            DO WITH FRAME FrameInpParams:
               vSessFlt:HIDDEN    = YES.
               oUserRep:SENSITIVE = NO.
               oRecvRep:SENSITIVE = NO.
            END.
         END.

         IF iTitle BEGINS "КНИГА" OR 
            iTitle =  "ТИТУЛЬНЫЙ ЛИСТ КАССОВЫХ ДОКУМЕНТОВ ДНЯ" OR
            iTitle =  "ЖУРНАЛ УЧЕТА ПРИНЯТЫХ И ВЫДАННЫХ ЦЕННОСТЕЙ" THEN
         DO:
            ENABLE 
               vBookEnd
            WITH FRAME FrameInpParams.
         END.
      END.
   END CASE.

   CASE mProgramName:
      WHEN "cas-svod318p"   OR 
      WHEN "cas-svod318p-o" THEN
         ENABLE 
            vNCurrency
            vICurrency
            vRCurrency
            vCashOrd
         WITH FRAME FrameInpParams.
      WHEN "v-tit-pg318p"   OR 
      WHEN "spr-cash318p"   OR
      WHEN "v-tit-pg318p-o" OR 
      WHEN "spr-cash318p-o" THEN
         ENABLE 
            vCashOrd
         WITH FRAME FrameInpParams.
      WHEN "rvok-pos" THEN
      DO:
         ENABLE
            vBookEnd
            vNameCen
            WITH FRAME FrameInpParams.
         DO WITH FRAME FrameInpParams:
            vBookEnd:LABEL = "Внебалансовые счета".
         END.
      END.
      WHEN "cashjour" THEN
         ENABLE
            vRazbIsp 
            vTotalCS
            vCashOrd
         WITH FRAME FrameInpParams.
      WHEN "rst-136i" THEN
      DO:
         vByRate:CHECKED = YES.
         ENABLE 
            vByRate
            vFullItg            
            vRstPril
         WITH FRAME FrameInpParams.
         HIDE
            vDateOtc
            vRazbDat
            IN FRAME FrameInpParams.
         
      END.
      WHEN "rst-cm" THEN
      DO:
         ENABLE
            mInUse
            mPK
         WITH FRAME FrameInpParams.
         HIDE
            vSessFlt
            IN FRAME FrameInpParams.
      END.
   END CASE.
   if PROGRAM-NAME(2) = "akt-reg318p.p" then   LEAVE FrameInpParams.
   WAIT-FOR GO OF CURRENT-WINDOW OR WINDOW-CLOSE OF CURRENT-WINDOW.
END.

HIDE FRAME FrameInpParams NO-PAUSE.
IF LASTKEY <> 10 AND LASTKEY <> 13 THEN 
DO:
   {intrface.del}
   RETURN.
END.
ASSIGN
   oDateRep
   oBrchRep
   oBrchKey
   oUserRep
   oDprId
   vSessFlt
   oRecvRep
   oBalAcct
   oByPrAcct = mByPrAcct
   oPrmsOK  = TRUE
   oBookEnd = vBookEnd:CHECKED
   oMoreTit = vMoreTit:CHECKED
   oBookTit = vBookTit:CHECKED
   oBookTyp = vBookTyp:CHECKED
   oChBlank = vChBlank:CHECKED
   oByInsp  = vByInsp:CHECKED
   oItogEkv = vItogEkv:CHECKED
   oPrnAcct = vPrnAcct:CHECKED
   oNameCen = vNameCen:CHECKED
   oCashOrd = vCashOrd:CHECKED
   oTotalCS = vTotalCS:CHECKED
   oByRate  = vByRate:CHECKED
   oFullItg = vFullItg:CHECKED   
   oRstPril = vRstPril:CHECKED
   oRazbIsp = vRazbIsp:CHECKED
   oShowZero = vShowZero:CHECKED 
   oNCurrency = vNCurrency:CHECKED
   oICurrency = vICurrency:CHECKED
   oRCurrency = vRCurrency:CHECKED
   oInUse   = mInUse:CHECKED
   oPK      = mPK:CHECKED
   oTitPage = vTitPage:CHECKED
   oNextDay = vNextDay:CHECKED
   oDateOtc = IF vDateOtc <> ? THEN STRING(vDateOtc, "99/99/9999")
                               ELSE (IF vRazbDat THEN "*"
                                                 ELSE (
                               IF mDateBeg <> ?  AND mDateEnd <> ? THEN 
                                                                   STRING(mDateBeg, "99/99/9999")
                                                                 + ","
                                                                 + STRING(mDateEnd, "99/99/9999")
                                                 ELSE (
                               IF mDateBeg <> ?  THEN STRING(mDateBeg, "99/99/9999")
                                                 ELSE (
                               IF mDateEnd <> ?  THEN STRING(mDateEnd, "99/99/9999")                     
                                                 ELSE ""
                                                    ))))
.

/* ====================== Уточняем параметры отбора ========================= */
IF oUserRep = "" OR oUserRep = "*" THEN 
   oUserRep = ?.   /* Если user не выбран, то отчет по всем */

IF oBrchRep = "" OR oBrchRep = "*" THEN
   oBrchRep = ?.   /* Если подр не выбрано, то отчет по всем */

/* Если ничего не выбрано, то отчет по подразд определяемого по логину юзера */
IF oUserRep = ? AND oBrchRep = ? THEN 
   oBrchRep = GetUserXAttrValue(USERID("bisquit"), "Отделение").

/* GetUserXAttrValue возвращает "*" при неудаче, а нам надо "?" */
IF oBrchRep = "*" THEN
   oBrchRep = ?.

IF oDprId =  "list" THEN
   oDprId = mDprList.

IF vSessFlt <> "Нет" THEN
   oDprId = mDprList.

mSessFlt = vSessFlt:SCREEN-VALUE IN FRAME FrameInpParams.

/* Если отбор не по всем и юзер не соответствует позразделению */
IF oDprId  =  "*" AND
   oUserRep <> ? AND 
   oBrchRep <> ? AND
   GetUserXAttrValue(oUserRep, "Отделение") <> oBrchRep THEN
DO:
   MESSAGE "Код подразделения, не соответствует коду кассира." SKIP
           "Приоритет имеет код кассира."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   oBrchRep = GetUserXAttrValue(oUserRep, "Отделение").
END.
/* =========================== Main block ============================*/
{intrface.del}


/* Подъем ЦМ и ВОК в 56 патч (0122104) */
/* $LINTFILE='a-params.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:35.471+03:00' */
/*prosign2iYBXHQa/vrpVrugTp2jpQ*/