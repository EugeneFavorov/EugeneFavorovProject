/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: ACCTTMPL.P
      Comment: Процедура обработки шаблона счета (создание,поиск). Обработка
               счета ведется относительно договора.
   Parameters: RECID(op-template) - ID шаблона счета, в котором объявляются
                                    параметры счета
               RECID(loan)        - ID договора относительно которого ведется
                                    обработка шаблона счета
               DATE               - Дата открытия счета
         Uses: accttmpl.lib
      Used by:
      Created: 30.10.2001 Kostik
     Modified: 02.07.2002 18:34 KSV      (0007589) Добавлено создание доп.реквизитов, хранящихся во
                                         вр. таблице tAttr (объявлена в ACCTTMPL.LIB).
     Modified: 27.02.2003 Илюха   - после определения роли счета добавлена
                                    проверка наличия счета с такой ролью
     Modified: 17.03.2003 14:29 kolal    изменен источник branch-id.
                                         Если у договора есть значение
                                         ДР branch-id то берется из ДР.
                                         Заявка 13858.
     Modified: 28.03.2003 12:18 ilvi     по заявке 11822
     Modified: 31.03.2003 18:38 ilvi     по заявке 11822
     Modified: 21.05.2003 12:38 ilvi     по заявке 11822
     Modified: 10.11.2005 ZIAL (0043043) Вычисление счета с использованием
                                         функции PLACCT
     Modified: 30.01.2008 jadv (0077751) Доработка РольС, если счет резерва закрыт,
                                         открывать новый счет.
     Modified: 27.02.2008 jadv (0047838) Не создавать счет с незаданной на шаблоне ролью.
     Modified:29/12/2016 kraw (0275914) учитываем старый номер счета в маске
*/

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get tmess}
{intrface.get pbase}
{intrface.get ovl}
{intrface.get xclass}

DEFINE INPUT PARAMETER iRidCond          AS CHAR  NO-UNDO. /* Для передачи контекста. */
DEFINE INPUT PARAMETER in-rid-loan       AS RECID NO-UNDO.
DEFINE INPUT PARAMETER in-op-date        AS DATE  NO-UNDO.

DEFINE VARIABLE vKodDoxRash AS CHARACTER NO-UNDO. /* Затычка для перевычисления tv-acct-mask */

/* Переменные для Check-Acct */
DEF VAR vField       AS CHAR   NO-UNDO. /* Код поля ошибки. */
DEF VAR vUpdValid    AS INT64  NO-UNDO. /* Код ошибки. */
DEF VAR in-rid-templ AS RECID  NO-UNDO.
DEF VAR vChkLA       AS LOG    NO-UNDO. /* Делать ли проверку на loan-acct. */
DEF VAR vTokacct     AS CHAR   NO-UNDO.
DEF VAR vTokidx      AS CHAR   NO-UNDO.
DEF VAR vToklen      AS CHAR   NO-UNDO.

DEF VAR vMethodAfter AS CHARACTER NO-UNDO.
DEF VAR vMethodAfterOb AS CHARACTER NO-UNDO.
DEF VAR vMethodAfterKr AS CHARACTER NO-UNDO.
DEF VAR vListOb        AS CHARACTER NO-UNDO.
DEF VAR vListKr        AS CHARACTER NO-UNDO.

/* Для определения блокированного клиента */
DEF VAR mClient-Id   AS INT64 NO-UNDO .
DEF VAR mClient-Type AS CHARACTER NO-UNDO .
DEF VAR mClient-Name AS CHARACTER NO-UNDO .
DEF VAR mBlock       AS CHARACTER NO-UNDO .

DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mItem   AS INTEGER   NO-UNDO.


IF NUM-ENTRIES (iRidCond, CHR (1))  GT 1
THEN ASSIGN
   vChkLA         =  NOT (ENTRY (2, iRidCond, CHR (1))  EQ "Нет")
   in-rid-templ   =  INT64 (ENTRY (1, iRidCond, CHR (1)))
.
ELSE
   ASSIGN
      vChkLA       = YES
      in-rid-templ = INT64 (iRidCond)
   .

FIND loan WHERE RECID(loan) EQ in-rid-loan
                          NO-LOCK NO-ERROR.

{intrface.get flt}
{tmprecid.def &NSH=YES}  /* Идентификатор записи      */

&GLOB skip-acct
&GLOB NO_BAL2ACCT YES /*из за неопределенного acct.number*/

DEF BUFFER bacct FOR acct.
DEF VAR mOtdel AS CHAR NO-UNDO.

{accttmpl.lib}

IF AVAILABLE loan THEN
   mOtdel = loan.branch-id.
IF mOtdel EQ "" THEN
   mOtdel = TRIM(GetUserBranchId(userid("bisquit"))).

RUN Get-Xattr-Templ(in-rid-templ).

/* Если клиент заблокирован , сообщим об этом , для ссудного счета  тип ='Кредит'.*/
IF AVAILABLE loan AND tv-Acct-type = 'Кредит'
THEN DO:
   ASSIGN
      mClient-Id    = loan.cust-id
      mClient-Type  = loan.cust-cat
      mBlock = ""
   .
   CASE mClient-Type :
      WHEN 'Ч' THEN
         mBlock = GetXAttrValue ('person', string(mClient-Id),'Блок') .
      WHEN 'Ю' THEN
         mBlock = GetXAttrValue ('cust-corp', string(mClient-Id),'Блок') .
      WHEN 'В' THEN
         mBlock = GetXAttrValue ('banks', string(mClient-Id),'Блок') .
   END CASE.
   IF mBlock = "Да"
   THEN DO:
      RUN RE_CLIENT_FULL ( mClient-Type, mClient-Id, INPUT-OUTPUT  mClient-Name ) .
      RUN Fill-SysMes("","","", SUBSTITUTE("ВНИМАНИЕ!!!  Клиент &1 &2 - заблокирован. Ссудный счет автоматически не создастся." ,mClient-Id , mClient-Name )) .
   END.
END.


/* Доработка для УМЦ.
** otdel определяется в Get-Xattr-Templ из ДР branch-id класса acct-templ-umc
** Если ДР не указан действуем по старой схеме
*/
IF tv-otdel NE ""
THEN DO:
   RUN GetOtdel.

   IF RETURN-VALUE NE ""
   THEN RETURN "-1".
   ELSE mOtdel = tv-otdel.
END.

IF tv-acct-type EQ "" OR tv-acct-type EQ ? THEN
DO:
   RUN Fill-SysMes("","","","Для шаблона № " + STRING(tv-op-template) +
                   "транзакции '" + tv-op-kind + "'" + "не указана роль счета.").
   RETURN.
END.

/* Потом для этого сделать настройку */
FIND FIRST loan-acct WHERE
           loan-acct.contract  EQ loan.contract
       AND loan-acct.cont-code EQ loan.cont-code
       AND loan-acct.acct-type EQ tv-Acct-type
   NO-LOCK NO-ERROR.

/* Если необходимо производить проверки на наличие счета, то
** сначала просто найдем есть ли такой */
IF    vChkLA
  AND AVAIL loan-acct THEN
DO:
      /* Проверяем дату привязки счета. Если она позднее, чем производится операция, то
      ** ругаемся и не открывем счет  */
   IF loan-acct.since GT in-op-date THEN
   DO:
      RUN Fill-SysMes("","","","Найден счет с ролью '" + tv-Acct-type +
                      "' привязанный более поздней датой, чем дата операции.").
      RETURN.
   END.
      /* Проверяем счет. Если он открыт, то пропускаем создание */
   IF CAN-FIND(FIRST acct WHERE acct.acct       EQ loan-acct.acct
                            AND acct.currency   EQ loan-acct.currency
                            AND acct.close-date EQ ?)
   THEN
      RETURN.
   ELSE
   DO:
         /* Если привязанный к договору счет с заданной ролью оказался закрытым, то */
      IF CAN-FIND(FIRST acct WHERE acct.acct       EQ loan-acct.acct
                               AND acct.currency   EQ loan-acct.currency
                               AND acct.close-date NE ?)
      THEN DO:
            /*  Если роль не найдена в значении настречного параметра ПрРольС, то счет не открывается. */
         IF NOT CAN-DO(FGetSetting("ПрРольС","",?),tv-Acct-type) THEN
            RETURN.
      END.
   END.
END.

IF AVAILABLE loan THEN
   Set_Loan(loan.contract,loan.cont-code).

RUN SetSysConf IN h_base ("LoanRecid-Acct19", STRING(in-rid-loan)).
RUN Convert-Param.
RUN DeleteOldDataProtocol IN h_base ("LoanRecid-Acct19").


RUN Search-Acct.   /*Можно сделать так: если счет не найден, то процедура
                     меняет tv-Create-Find = "Создавать" и создается
                     новый счет. */
mStrTMP = GetSysConf("LoanAcctTmplMaskOldAcct").

IF INDEX(tv-Acct-Mask, "п") GT 0
   AND LENGTH(mStrTMP) EQ 20
   THEN
DO:

   DO mItem = 1 TO LENGTH(tv-Acct-Mask):
      IF SUBSTRING(tv-Acct-Mask, mItem, 1) EQ "п" THEN
         SUBSTRING(tv-Acct-Mask, mItem, 1) = SUBSTRING(mStrTMP, mItem, 1).
   END.
   RUN SetSysConf IN h_base ("LoanAcctTmplMaskOldAcct", "").
END.

RUN Create-Acct(OUTPUT mess).
IF mess NE "" THEN
   RUN Fill-SysMes("","","","Шаблон № " + STRING(tv-op-template) +
                   " транзакции '" + tv-op-kind + "':" + "~n" + mess).

IF AVAIL acct THEN
DO:
/*  acct.number = TRIM(ENTRY(1, acct.acct, "@")).
это теперь делает RUN Create-Acct., и acct NO-LOCK */

  /* Сохраним ссылку на договор  для спец обработки в процедурах проверки счетов , в частности для ошибки ACCT19 */
  RUN SetSysConf IN h_base ("LoanRecid-Acct19", string(in-rid-loan)).

  /* Проверочки, постановка УНКг если надо и прочее */
  RUN Check-Acct IN h_acct (BUFFER acct,
                            OUTPUT vField,
                            OUTPUT vUpdValid ).
  RUN DeleteOldDataProtocol IN h_base ("LoanRecid-Acct19").
  IF vUpdValid NE 0
      THEN RETURN "-1".

  FOR EACH tAttr WHERE tAttr.fAttrVal <> ? AND tAttr.fAttrVal <> "":
    IF UpdateSigns ("acct",acct.acct + "," + acct.currency,tAttr.fAttrName,tAttr.fAttrVal,
                   isXAttrIndexed(acct.class-code,tAttr.fAttrName)) <> YES THEN
    DO:
      mess = "Ошибка создания доп.реквизита " + tAttr.fAttrName + " (" + tAttr.fAttrVal + ")".
      RETURN "-1".
    END.
  END. /* End of FOR */

   /* Перевычисляем tv-acct-mask дабы не было введенных счетов
      с некорректным балансовым счетом и/или валютой */
   RUN "FindAcctMask" (tv-Cr-Class-Code,
                       tv-Bal-Acct,
                       INPUT-OUTPUT tv-Acct-Mask,
                       INPUT-OUTPUT vKodDoxRash ) NO-ERROR.
   RUN GetAcctMask IN h_acct
                  (INPUT tv-Acct-Mask,
                   OUTPUT vTokacct,
                   OUTPUT vTokidx,
                   OUTPUT vToklen) NO-ERROR.
END.

  CREATE tt-editacct.
   ASSIGN
      tt-editacct.num       = GetNumAcct()
      tt-editacct.rid       = IF AVAIL acct THEN RECID(acct)
                                            ELSE ?
      tt-editacct.acct-type = tv-Acct-type
      tt-editacct.edit      = IF AVAIL acct AND tv-Create-Find EQ "Создавать" THEN yes
                                                                              ELSE no
      tt-editacct.find-acct = IF mess BEGINS "FIND"     THEN yes ELSE no
      tt-editacct.find-res  = 0
      tt-editacct.name      = IF tv-Acct-name NE "" THEN tv-Acct-name
                                                    ELSE "СЧЕТ:"
      tt-editacct.maskedit  = vTokacct
      tt-editacct.fndstat   = tv-Create-Find EQ "Создавать"
      tt-editacct.acct-code = tv-Acct-code
   .
/*   MESSAGE "Не могу определить счет!"    SKIP
           "транзакция: '" tv-op-kind "'"
           "шаблон  №"  tv-op-template
   VIEW-AS ALERT-BOX ERROR BUTTONS OK.*/

IF AVAIL acct THEN
DO:
   vMethodAfter = GetXAttrValueEx("op-template", tv-op-kind + "," + STRING(tv-op-template), "RunAfterCreate", "").
   IF vMethodAfter NE "" THEN
   DO:
      vListOb = ENTRY(1,vMethodAfter,")").
      vListOb = SUBSTRING(vListOb, INDEX(vListOb,"(") + 1 ).
      vMethodAfterOb = ENTRY(1,vMethodAfter,"(").
      IF SearchPFile(vMethodAfterOb) THEN
         RUN VALUE(vMethodAfterOb + ".p") (in-rid-loan,RECID(acct),tv-Acct-type,vLISTOb).
      
      vLISTKr = ENTRY(2,vMethodAfter,";").
      vMethodAfterKr = ENTRY(1,vLISTKr,"(").
      vLISTKr = SUBSTRING(vLISTKr, INDEX(vLISTKr,"(") + 1, INDEX(vLISTKr,")") - INDEX(vLISTKr,"(") - 1).
      IF SearchPFile(vMethodAfterKr) THEN
         RUN VALUE(vMethodAfterKr + ".p") (in-rid-loan,RECID(acct),tv-Acct-type,vLISTKr).
   END.
END.

{intrface.del}          /* Выгрузка инструментария. */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='29/12/2015 18:55:02.925+04:00' */
/* $LINTUSER='kraw' */
/* $LINTMODE='1' */
/* $LINTFILE='accttmpl.p' */
/*prosigno3HNI/PxiUnmhH93ISZfdQ*/