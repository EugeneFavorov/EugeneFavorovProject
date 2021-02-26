/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: DPS_PRN.P
      Comment: Формулы для печати документов из модуля DPS.
   Parameters:
      Created: Om 16/06/99
     Modified: Om 30/06/99
     Modified: Om 12/07/99
     Modified: Om 23/07/99
     Modified: Om 19/06/2000
     Modified: Om 06/06/2001 Доработка:
                                - добавлен блок "тек_ст".
                                - исправлен формат отображения %% ставки в "нач_ст".
     Modified: Om 07/06/2001 Ошибка:
                                - "тек_ст" не обрабатывала минимальный остаток.
     Modified: Om 09/08/2001 Ошибка:
                                - "тек_ст" не учитывала пролонгацию вклада.
     Modified: 10/10/2002 kraw (0010361) Функция "сум_вкл_ц"
     Modified: 10/10/2002 kraw (0010899) Функция "нач_ст". Учет суммы вклада.
     Modified: 10/10/2002 kraw (0010899) Функция "нач_ст" через функцию "комиссия"
     Modified: 03/02/2004 21-30 Laav   Заявка 0025657. В блоке, печатающем адрес, 
                                добавлена обработка формата вывода в зависимости 
                                от длины стороки с адресом (добавлена функция 
                                if..then..else после атрибута format)   
     Modified: 08/07/2005 koav добавлена функция "ДопРекВкл" - печать указанного 
                                                  доп.реквизита вкладчика       
     Modified: 13.10.2006 18:18 OZMI (0062669)
*/
Form "~n@(#) dps_prn.p 1.0 Om 16/06/99 Om 19/06/2000 "
with frame sccs-id stream-io width 250.

{norm.i}
{globals.i}
{ksh-defs.i new}
{inst_prn.i new}
/* Вспомогательные функции по вкладам */
{dpsproc.def}


{f_for_t.i} /* Функции для работы с довложениями */

{intrface.get dps}
{intrface.get cust}     /* Библиотека для работы с клиентами. */
{intrface.get date}     /* Инструменты для работы с датами. */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get strng}
printres = no.

DEF OUTPUT PARAM  Xresult AS DECIMAL NO-UNDO.
DEF INPUT  PARAM  Xdate1  AS DATE    NO-UNDO.
DEF INPUT  PARAM  Xdate   AS DATE    NO-UNDO.
DEF INPUT  PARAM  strpar  AS CHAR    NO-UNDO.

DEF SHARED VAR rid_loan AS RECID. /* RECID LOAN-a */

DEF VAR out_str AS CHAR FORMAT "x(70)" NO-UNDO. /* Строка для печати в поток */
DEF VAR mont_h  AS CHAR
INIT "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря" NO-UNDO.
DEF VAR kod_ost AS CHAR NO-UNDO. /* Код остатка */
DEF VAR l_acct  AS CHAR NO-UNDO. /* Роль счета */
DEF VAR in_kau  AS CHAR NO-UNDO.
DEF VAR rab_str AS CHAR NO-UNDO.
DEF VAR mParam2 AS CHAR NO-UNDO.
DEF VAR COMMAND AS CHAR NO-UNDO. /* Необходимая функция */
DEF VAR max_len AS INT64  NO-UNDO. /* Максимальная длина поля */
DEF VAR offset  AS INT64  NO-UNDO. /* Смещение относительно левого края */
DEF VAR in-surrogate AS CHAR NO-UNDO. /* Для поиска дополнительного реквизита */
DEF VAR mReturn AS CHAR   NO-UNDO.

DEF VAR c1 AS CHAR NO-UNDO. /* Для вывода суммы прописью */
DEF VAR c2 AS CHAR NO-UNDO. /* Для вывода суммы прописью */
DEF VAR i  AS INT64  NO-UNDO. /* Вспомогательный счетчик */
DEF VAR j  AS INT64  NO-UNDO. /* Вспомогательный счетчик */
DEF VAR num_templ AS INT64 NO-UNDO. /* Номер шаблона в транзакции */

DEFINE VAR vCommChar     AS CHAR  NO-UNDO.  /* Код комиссии */
DEFINE VAR vProlOpenDate AS DATE  NO-UNDO.  /* Дата открытия */
DEFINE VAR vOpKindChar   AS CHAR  NO-UNDO.  /* Код актуальной транзакция */
DEFINE VAR vOpTemplInt   AS INT64   NO-UNDO.  /* Номер шаблона карточки */
DEFINE VAR vTemplRecid   AS RECID NO-UNDO.  /* Recid шаблона карточки */
DEF VAR delta AS INT64.
DEF VAR d AS DATE NO-UNDO.
DEF VAR dep_period AS CHAR NO-UNDO.
DEF VAR vBankMFO AS CHARACTER NO-UNDO.
DEF VAR ksh-bal1 AS DECIMAL   NO-UNDO. /* для хранения копеек */
DEF VAR logTrim AS LOG NO-UNDO INIT NO.
DEF VAR chTrim AS CHAR NO-UNDO.
DEF VAR iIndex AS INT64 NO-UNDO.

DEF VAR mSubs  AS CHAR   NO-UNDO.
DEF VAR vCurr     AS CHARACTER INIT ? NO-UNDO.
DEF VAR vParentCC AS CHARACTER NO-UNDO.
DEF VAR vLimitProl AS INT64 NO-UNDO.
DEF VAR vUnit      AS CHARACTER NO-UNDO.
DEF VAR vUnitStart AS CHARACTER NO-UNDO.
DEF VAR mLnend-date   AS DATE        NO-UNDO.
DEF VAR vInterest AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPersSurr AS CHARACTER    NO-UNDO.
DEFINE VARIABLE vDogFlag  AS LOGICAL      NO-UNDO.
DEF VAR SubStatus AS CHAR NO-UNDO.
DEF VAR logReturn AS LOGICAL INIT YES NO-UNDO.

DEF BUFFER b-person FOR person. /* Локализация буфера. */

IF strpar MATCHES "*trim*" AND 
   NOT strpar MATCHES "*trimstr*" THEN 
DO:
   logTrim = YES.
   iIndex = INDEX(strpar,",").
   chTrim = SUBSTRING(strpar,1,iIndex - 1).
   strpar = SUBSTRING(strpar,iIndex + 1,LENGTH(strpar) - iIndex).
END.


FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) FORWARD.

FUNCTION trimaddress RETURNS CHAR (INPUT in-address AS CHAR) FORWARD.

FIND FIRST loan WHERE RECID (loan) EQ rid_loan NO-LOCK NO-ERROR.

RUN GetBaseAcctRole (rid_loan,
                     xDate,
                     OUTPUT l_acct).       
RUN GetBaseKodOst (l_acct,
                   OUTPUT kod_ost).

FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                  AND loan-acct.since     LE xdate
                                  NO-LOCK NO-ERROR.
CASE loan.cust-cat:
   WHEN "Ч" THEN
      FIND FIRST person    WHERE
         person.person     EQ loan.cust-id
      NO-LOCK NO-ERROR.
   WHEN "Ю" THEN
      FIND FIRST cust-corp WHERE
         cust-corp.cust-id EQ loan.cust-id
      NO-LOCK NO-ERROR.
END CASE.
ASSIGN
    COMMAND = ENTRY(1,STRPAR)
/*     max_len = INT64(if num-entries(STRPAR) ge 2 then entry(1,STRPAR) else '0') */
/*     offset  = INT64(if num-entries(STRPAR) ge 3 then entry(1,STRPAR) else '0') */
.

IF RetString THEN
DO:
   {setdest2.i &stream="stream fil" &filename="_spool1.tmp"} 
END.
   

FUNCTION dat_ RETURN CHAR (INPUT d AS INT64,
              INPUT p AS CHAR):
    DEF VAR yy AS CHAR EXTENT 3 INIT ['год','года','лет'] NO-UNDO.
    DEF VAR mm AS CHAR EXTENT 3 INIT ['месяц','месяца','месяцев'] NO-UNDO.
    DEF VAR dd AS CHAR EXTENT 3 INIT ['день','дня','дней'] NO-UNDO.
    d = IF d GT 3 THEN 3 ELSE d.
    IF p EQ 'г' THEN RETURN yy[d].
    ELSE IF p EQ 'м' THEN RETURN mm[d].
    ELSE IF p EQ 'д' THEN RETURN dd[d].
END FUNCTION.

FUNCTION c_center RETURN CHAR (INPUT i_string AS CHAR):
    RETURN FILL(" ", INT64(TRUNCATE((PrinterWidth) / 2, 0))
                - INT64(TRUNCATE((LENGTH(TRIM(i_string))) / 2, 0)))
                + TRIM(i_string).
END FUNCTION.

IF COMMAND MATCHES "сум_вкл*_ц" THEN
DO:
   IF LENGTH(COMMAND) > 9 THEN
   DO:
      rab_str = SUBSTR(COMMAND,8,1).
      COMMAND = "сум_вкл_ц".
   END.
   ELSE rab_str = gop-status.
END.

ELSE IF CAN-DO("сум_вкл2,сум_вкл2проп,сумвкл2проп2,сум_сумП2",COMMAND) THEN
DO:
   rab_str = "П".
END.

ELSE IF COMMAND BEGINS "сум_вкл_мульт" THEN DO:
   IF NUM-ENTRIES(COMMAND, "@") GE 3 THEN
   DO:
       rab_str = ENTRY(3, COMMAND, "@").
   END.
   ELSE rab_str = gop-status. 
   ASSIGN 
      
      vCurr   = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                   ELSE ENTRY(2, COMMAND, "@")
      COMMAND = ENTRY(1, COMMAND, "@")
   .
END.


ELSE IF COMMAND BEGINS "сум_вкл" THEN
DO:
   IF LENGTH(COMMAND) > 7 THEN
   DO:
      rab_str = SUBSTR(COMMAND,8,1).
      COMMAND = SUBSTR(COMMAND,1,7).
   END.
   ELSE rab_str = gop-status.

END.

ELSE IF COMMAND BEGINS "МинСумма" THEN
DO:
   IF NUM-ENTRIES(COMMAND, "@") GE 2 THEN
      ASSIGN 
         vCurr   = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                      ELSE ENTRY(2, COMMAND, "@")
         COMMAND = ENTRY(1, COMMAND, "@")
      .
END.

ELSE IF COMMAND BEGINS "выдан" THEN
DO:
   IF LENGTH(COMMAND) GT 5 THEN
   DO:
      ASSIGN
         rab_str = SUBSTR(COMMAND,7,LENGTH(COMMAND))
         COMMAND = SUBSTR(COMMAND,1,5).
   END.
   ELSE
      ASSIGN rab_str = ?.
END.

ELSE IF COMMAND BEGINS "нач_ст"
        AND NOT (COMMAND  BEGINS "нач_ст_проп")  THEN
DO:
   IF NUM-ENTRIES(COMMAND, "@") LE 1 THEN
   DO:   
      IF LENGTH(COMMAND) > 6 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,7,1))).
         COMMAND = SUBSTR(COMMAND,1,6).
      END.
      ELSE rab_str = "99999".
   END.
   ELSE DO:
      IF LENGTH(ENTRY(1, COMMAND, "@")) > 6 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(ENTRY(1, COMMAND, "@"),7,1))).
      END.
      ELSE
         ASSIGN 
            rab_str = "99999"
      .                  

      ASSIGN 
         vCurr = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                    ELSE ENTRY(2, COMMAND, "@")
         COMMAND = SUBSTR(ENTRY(1, COMMAND, "@"),1,6)
         .
   END.
END.
ELSE IF COMMAND BEGINS "нач_ст_проп" THEN
DO:
   IF NUM-ENTRIES(COMMAND, "@") LE 1 THEN
   DO:   
      IF LENGTH(COMMAND) > 11 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,7,1))).
         COMMAND = SUBSTR(COMMAND,1,11).
      END.
      ELSE rab_str = "99999".
   END.
   ELSE DO:
      IF LENGTH(ENTRY(1, COMMAND, "@")) > 11 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(ENTRY(1, COMMAND, "@"),12,1))).
      END.
      ELSE
         ASSIGN 
            rab_str = "99999"
      .                  

      ASSIGN 
         vCurr = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                    ELSE ENTRY(2, COMMAND, "@")
         COMMAND = SUBSTR(ENTRY(1, COMMAND, "@"),1,11)
         .
   END.
END.

ELSE IF COMMAND BEGINS "тек_стШ" 
     AND NOT (COMMAND  BEGINS "тек_стШ_проп") THEN DO:
   
   IF LENGTH(COMMAND) > 7 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,8,1))).
         COMMAND = SUBSTR(COMMAND,1,7).
      END.
      ELSE rab_str = "99999".
END.

ELSE IF COMMAND BEGINS "тек_стШ_проп" THEN DO:
   
   IF LENGTH(COMMAND) > 12 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,13,1))).
         COMMAND = SUBSTR(COMMAND,1,12).
      END.
      ELSE rab_str = "99999".
END.

ELSE IF COMMAND BEGINS "вал_сч" THEN
DO:   
   IF NUM-ENTRIES(COMMAND, "@") > 1 THEN 
   DO:   
      ASSIGN
         rab_str = ENTRY(2, COMMAND, "@")
         COMMAND = ENTRY(1, COMMAND, "@")
      .
   END.   
END.

ELSE IF COMMAND BEGINS "тек_ст" THEN
DO:
   rab_str = "99999".
   IF NUM-ENTRIES(COMMAND, "@") GE 2 THEN
      ASSIGN
         vCurr = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                    ELSE ENTRY(2, COMMAND, "@")
         COMMAND = ENTRY(1, COMMAND, "@")
      .
   IF LENGTH(COMMAND) = 7 THEN  /*последний символ - точность вывода, исключили тек_ст_проп*/
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,7,1))).
         COMMAND = SUBSTR(COMMAND,1,6).
      END.
      ELSE rab_str = "99999".
END.

ELSE IF COMMAND BEGINS "дата_прописью" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"|") GE 2
   THEN 
      rab_str = ENTRY(2,COMMAND,"|").
   
   IF rab_str EQ ""   
   THEN
      rab_str = STRING(TODAY).
   
   IF NUM-ENTRIES(COMMAND,"|") GE 3
      THEN mParam2 = ENTRY(3,COMMAND,"|").

   COMMAND = "дата_прописью".
END.

ELSE IF COMMAND BEGINS "КодАдр" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").         
   COMMAND = "КодАдр".
END.

ELSE IF COMMAND BEGINS "АдресФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   IF NUM-ENTRIES(COMMAND,"@") GE 3 THEN 
      mParam2 = ENTRY(3,COMMAND,"@").
   COMMAND = "АдресФЛ".
END.

ELSE IF COMMAND BEGINS "ФамилияФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "ФамилияФЛ".
END.

ELSE IF COMMAND BEGINS "ИмяОтчестФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "ИмяОтчестФЛ".
END.

ELSE IF COMMAND BEGINS "ДатаРождФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "ДатаРождФЛ".
END.

ELSE IF COMMAND BEGINS "МестоРождФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "МестоРождФЛ".
END.

ELSE IF COMMAND BEGINS "ТипДокФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "ТипДокФЛ".
END.

ELSE IF COMMAND BEGINS "НомерСерияФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "НомерСерияФЛ".
END.

ELSE IF COMMAND BEGINS "ДатаВыдДокФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "ДатаВыдДокФЛ".
END.

ELSE IF COMMAND BEGINS "КемВыдДокФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "КемВыдДокФЛ".
END.

ELSE IF COMMAND BEGINS "КодПодрДокФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "КодПодрДокФЛ".
END.

ELSE IF COMMAND BEGINS "ТелДомФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "ТелДомФЛ".
END.

ELSE IF COMMAND BEGINS "ТелДомФактФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "ТелДомФактФЛ".
END.

ELSE IF COMMAND BEGINS "ТелРабФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "ТелРабФЛ".
END.

ELSE IF COMMAND BEGINS "ТелМобФЛ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "ТелМобФЛ".
END.

ELSE IF COMMAND EQ "адр_подр_д" THEN
DO:
   vDogFlag = YES.
   COMMAND = "адр_подр".
END.

ELSE IF COMMAND EQ "наимен_подр_д" THEN
DO:
   vDogFlag = YES.
   COMMAND = "Наим_банка".
END.

ELSE IF COMMAND BEGINS "ТипВкл" THEN
DO:
   IF NUM-ENTRIES(COMMAND,'@') EQ 2 THEN
   DO:
      ASSIGN
      rab_str = ENTRY(2,COMMAND,'@')
      COMMAND = ENTRY(1,COMMAND,'@').
   END.
END.

CASE COMMAND:

WHEN "CustStatName" THEN 
DO:
   FOR FIRST cust-corp WHERE
             cust-corp.cust-id             EQ loan.cust-id
         AND LENGTH (cust-corp.cust-stat)  GT 0
         AND loan.cust-cat                 EQ "ю"
   NO-LOCK,
   FIRST code WHERE code.class  EQ "КодПредп"
                AND code.val    EQ cust-corp.cust-stat
   NO-LOCK:
      mReturn = code.name.
   END.   
END.

WHEN "нач_дог" THEN
   DO:
   IF Old_dps(BUFFER loan,OUTPUT dep_period) THEN
      d = loan.open-date.
      ELSE RUN deposit-start-date in h_dpspc (RECID(loan),
                                                      Xdate,
                                                      OUTPUT d).
      delta = 0.
   IF NUM-ENTRIES(strpar) = 2 THEN
   DO ON ERROR UNDO, LEAVE:
      delta = INT64(ENTRY(2, strpar)).
   END.
   IF NOT RetString THEN
      mReturn = (IF logTrim 
                 THEN TrimFormula(chTrim,
                                  STRING(d + delta, '99.99.9999')) 
                 ELSE STRING(d + delta, '99.99.9999')).
   ELSE
      mReturn = STRING(DAY(d + delta)) + " " + 
                       ENTRY(MONTH(d + delta), mont_h) +
                       STRING(YEAR(d + delta), " 9999 г.").
END.

WHEN "окон_дог" THEN /* Для срочного вклада */
DO:
   IF Old_dps(BUFFER loan,OUTPUT dep_period) THEN
      d = loan.end-date.
   ELSE RUN deposit-end-date in h_dpspc (RECID(loan),
                                                Xdate,
                                                GetXattrValueEx("loan",
                                                                loan.contract
                                                                + ","
                                                                + loan.cont-code,
                                                                "dep_period",
                                                                ?),
                                                OUTPUT d)
                                                .
   delta = 0.

   IF NUM-ENTRIES(strpar) = 2 THEN
   DO ON ERROR UNDO, LEAVE:
      delta = INT64(ENTRY(2, strpar)).
   END.

   IF NOT RetString THEN
      mReturn = (IF logTrim 
                 THEN TrimFormula(chTrim,
                                  STRING(d + delta, '99.99.9999')) 
                 ELSE STRING(d + delta, '99.99.9999')).
   ELSE
      mReturn = STRING(DAY(d + delta)) + " " + 
                       ENTRY(MONTH(d + delta), mont_h) +
                       STRING(YEAR(d + delta), " 9999 г.").
END.

WHEN "адр_подр" THEN /* адрес подразделения банка */
DO:
   IF vDogFlag THEN
      vUnit = loan.branch-id.
   ELSE
      vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "Отделение")).
   IF {assigned vUnit}
   THEN DO:
      mReturn = GetXAttrValue("branch", vUnit, "Адрес_юр").
      vUnitStart = vUnit.
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN LEAVE.
         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* если ходим по кругу */
         IF vUnit EQ vUnitStart THEN LEAVE.
         mReturn = GetXAttrValue("branch", vUnit, "Адрес_юр").
      END.
   END.
   IF NOT {assigned mReturn} THEN
      mReturn = FGetSetting("Адрес_юр",?,"").
END.

WHEN "рук_подр" THEN /* руководитель подразделения банка */
DO:
   vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "Отделение")).
   IF {assigned vUnit}
   THEN DO:
      mReturn = GetXAttrValue("branch", vUnit, "ФИОРукПодр").
      vUnitStart = vUnit.
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN LEAVE.
         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* если ходим по кругу */
         IF vUnit EQ vUnitStart THEN LEAVE.
         mReturn = GetXAttrValue("branch", vUnit, "ФИОРукПодр").
      END.
   END.
   IF NOT {assigned mReturn} THEN
      mReturn = FGetSetting("ФИОРук",?,"").
END.
WHEN "Наим_банка" THEN
DO:
   IF vDogFlag THEN
      vUnit = loan.branch-id.
   ELSE
      vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "Отделение")).
   IF {assigned vUnit} THEN
   DO:
      FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
      IF AVAIL branch THEN
         mReturn = branch.name.
   END.
END.
WHEN "Подразделение" THEN
DO:
   vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "Отделение")).   
   IF vUnit EQ "*" THEN
      vUnit = sh-branch-id.
   FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
   IF AVAIL branch THEN
      mReturn = branch.Branch-Id.

END.
WHEN "НомерВкл_Дата_откр" THEN
DO:
   RUN Get-beg-date-prol in h_dpspc (rid_loan,
                                     xDate,
                                     OUTPUT vProlOpenDate,
                                     OUTPUT mLnend-date).

   mReturn = loan.cont-code + " от " + STRING(vProlOpenDate).
END.
WHEN "долж_рук_подр" THEN /* должность руководителя подразделения */
DO:
   vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "Отделение")).
   IF {assigned vUnit}
   THEN DO:
      mReturn = GetXAttrValue("branch", vUnit, "ДолжнРукПодр").
      vUnitStart = vUnit.
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN LEAVE.
         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* если ходим по кругу */
         IF vUnit EQ vUnitStart THEN LEAVE.
         mReturn = GetXAttrValue("branch", vUnit, "ДолжнРукПодр").
      END.
   END.
   IF NOT {assigned mReturn} THEN
      mReturn = FGetSetting("ДолжнРук",?,"").
END.

/* Документ на основании которого действует руководитель отделения банка */
WHEN "ДокОснование" THEN 
DO:
   vUnit = TRIM(GetXAttrValue("_user",
                              USERID("bisquit"), 
                              "Отделение")).
   IF {assigned vUnit} THEN 
   DO:
      mReturn = GetXAttrValue("branch", vUnit, "ДокОснРукПодр").
      vUnitStart = vUnit.

      BRNCHS:
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit 
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN 
            LEAVE BRNCHS.

         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* Если ходим по кругу */
         IF vUnit EQ vUnitStart THEN 
            LEAVE BRNCHS.
         mReturn = GetXAttrValue("branch", 
                                 vUnit, 
                                 "ДокОснРукПодр").
      END.
   END.
END.

WHEN "родИП" THEN
DO:
	IF person.gender EQ TRUE THEN 
		mReturn = "ый".
	ELSE
		mReturn = "ая".
END.

WHEN "дата_рожд" THEN /* дата рождения владельца частного вклада */
DO:
   IF person.birthday NE ? THEN
      mReturn = STRING(person.birthday,'99.99.9999').
END.

WHEN "дата_рождТ" THEN /* дата рождения владельца частного вклада */
DO:
   IF person.birthday NE ? THEN
      mReturn = STRING( person.birthday, '99.99.9999' ).
END.

WHEN "дата_рожд2" THEN /* дата рождения владельца частного вклада формата 99 месяца 9999 года*/
DO:
   IF person.birthday NE ? THEN
      mReturn = string(day(person.birthday),'99') + " " + entry(month(person.birthday),"января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
	  октября,ноября,декабря") + " " + string(year(person.birthday)) + " года".
END.
   
WHEN "место_рожд" THEN /* место рождения владельца частного вклада */
DO:
   mReturn = GetXAttrValue("person", string(person.person-id) ,"BirthPlace").
END.

WHEN "работа" THEN /* место работы и должность владельца частного вклада */
DO:
   mReturn = GetXAttrValue("person", string(person.person-id) ,"работа").
END.

WHEN "откр_сч" THEN /* дата открытия вкладного счета */
DO:
   FIND FIRST acct WHERE acct.acct     EQ loan-acct.acct
                     AND acct.currency EQ loan-acct.currency
        NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
      mReturn = STRING(DAY(acct.open-date)) + " " + 
                       ENTRY(MONTH(acct.open-date), mont_h) +
                       STRING(YEAR(acct.open-date), " 9999 г.").
END.

WHEN "инн" THEN /* инн вкладчика */
DO:
   CASE loan.cust-cat:
      WHEN "Ч" THEN      
         mReturn  =  IF LENGTH (person.inn)     GT 0 THEN person.inn    ELSE "".
      WHEN "Ю" THEN
         mReturn  =  IF LENGTH (cust-corp.inn)  GT 0 THEN cust-corp.inn ELSE "".
   END CASE.
END.

/*----------------------------------------------------
  Дата возврата (когда клиент может вернуть средства)
----------------------------------------------------*/
WHEN "Дата_возвр" THEN
DO:
   RUN deposit-return-date in h_dpspc (RECID(loan),
                                               Xdate,
                                               Get-Dep-Period(loan.contract
                                                              + ","
                                                              + loan.cont-code),
                                               OUTPUT d)
                                               .
    mReturn = IF logTrim 
              THEN TrimFormula(chTrim,
                               STRING(d, '99.99.9999')) 
              ELSE STRING(d, '99.99.9999').
END.

WHEN "период" THEN
DO:
   delta = 0.
   IF NUM-ENTRIES(strpar) = 2 THEN
   DO ON ERROR UNDO, LEAVE:
      delta = INT64(ENTRY(2, strpar)).
   END.
   mReturn = IF logTrim 
             THEN TrimFormula(chTrim,
                              STRING(loan.end-date 
                                     - loan.open-date 
                                     + delta)) 
             ELSE STRING(loan.end-date 
                         - loan.open-date 
                         + delta).
END.

WHEN "ПериодТ" THEN
DO:
   delta = IF NUM-ENTRIES(strpar) EQ 2 THEN 
              INT64(ENTRY(2, strpar))
           ELSE 
              0.

   i = loan.end-date - loan.open-date + delta.    
   run amtstr ( DEC(i), FALSE, OUTPUT mReturn, OUTPUT c1). 
   i = 0.
   c1 = "".
   delta = 0.
END.
/*-------------------------------------------------------------------
  срок вклада прописью - столько лет, столько месяцев и столько дней
--------------------------------------------------------------------*/
WHEN "Период2" THEN
DO:
   IF loan.end-date EQ ? THEN
   DO:
      c2 = "до востребования" .
   END. ELSE
   DO:
      DEF VAR yy AS INT64 NO-UNDO.
      DEF VAR mm AS INT64 NO-UNDO.
      DEF VAR dd AS INT64 NO-UNDO.

      IF Old_dps(BUFFER loan,OUTPUT dep_period) THEN
         c1 = "Д=" + STRING(loan.end-date - loan.open-date).
      ELSE
         c1 = GetXattrValueEx("loan",
                              loan.contract
                              + ","
                              + loan.cont-code,
                              "dep_period",
                              ?)
                              .
      RUN period2dmy(c1, OUTPUT yy, OUTPUT mm, OUTPUT dd).

      c2 = "".
      IF yy > 0 THEN
      DO:
         i = yy.
         i = IF (i GE 5 AND i LE 20) OR i EQ (INT64((i / 10)) * 10)
               THEN 3
               ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
         c2 = c2 + STRING(yy) + " " + dat_(i, "г") + " ".
      END.

      IF mm > 0 THEN
      DO:
         i = mm.
         i = IF (i GE 5 AND i LE 20) OR i EQ (INT64((i / 10)) * 10)
               THEN 3
               ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
         c2 = c2 + STRING(mm) + " " + dat_(i, "м") + " ".
      END.

      IF dd > 0 THEN
      DO:
         i = dd.
         i = IF (i GE 5 AND i LE 20) OR i EQ (INT64((i / 10)) * 10)
               THEN 3
               ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
         c2 = c2 + STRING(dd) + " " + dat_(i, "д").
      END.
   END.
   mReturn = TRIM(c2).
END.

WHEN "сум_вкл" THEN
   RUN Сум_Вкл("сум_вкл",OUTPUT mReturn).

/* Сумма вклада без суммы прописью */
WHEN "сум_вкл_мульт" THEN
DO:
   IF vCurr NE ? AND AVAIL loan THEN DO:
      ASSIGN vParentCC = loan.cont-code. 
      FIND FIRST loan WHERE loan.contract         EQ "dps"
                        AND loan.parent-cont-code EQ vParentCC 
                        AND loan.parent-contract  EQ "dps" 
                        AND loan.currency         EQ vCurr 
                        NO-LOCK NO-ERROR. 
      RUN GetBaseAcctRole (RECID(loan),
                           xDate,
                           OUTPUT l_acct).       
      RUN GetBaseKodOst (l_acct,
                         OUTPUT kod_ost).
      FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                                    AND loan-acct.since     LE xdate
                                    NO-LOCK NO-ERROR.
   
      IF AVAIL loan-acct THEN
      DO:
         in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
         FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
         RUN kau-pos.p(loan-acct.acct,
                       loan-acct.currency,
                       xdate,
                       xdate,
                       rab_str,
                       in_kau)
                       .
                       
         /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
         IF RETURN-VALUE EQ "Ошибка" THEN
            ASSIGN Xresult = -2.
    
         IF ksh-bal = 0 AND ksh-val = 0 THEN
            RUN get-kauost-trans(loan.contract,
                                 loan.cont-code,
                                 "*",
                                 loan-acct.acct-type,
                                 "",
                                 "",
                                 kod_ost,
                                 end-date,
                                 end-date,
                                 gop-status,
                                 OUTPUT ksh-bal). 
         
         FIND FIRST code WHERE code.class EQ 'ШаблКау'
                           AND code.code  EQ acct.kau-id NO-LOCK NO-ERROR.
         SubStatus = GetEntries(2,code.misc[4],",",""). 
                      
         IF     SubStatus NE "" 
            AND SubStatus GE rab_str THEN
         DO:
            FOR EACH op-entry WHERE (op-entry.acct-cr   EQ acct.acct
                                 AND op-entry.op-date   LE Xdate
                                 AND op-entry.op-status GE rab_str
                                 AND op-entry.op-status LT SubStatus) 
                                 OR (op-entry.acct-db   EQ acct.acct
                                 AND op-entry.op-date   LE Xdate
                                 AND op-entry.op-status GE rab_str
                                 AND op-entry.op-status LT SubStatus)                       
            NO-LOCK:
               IF acct.currency NE "" THEN
               DO:
                  IF op-entry.acct-db EQ acct.acct THEN 
                     ksh-val = ksh-val + op-entry.amt-cur.
                  IF op-entry.acct-cr EQ acct.acct THEN
                     ksh-val = ksh-val - op-entry.amt-cur.         
               END.    
               IF acct.currency EQ "" THEN
               DO:
                  IF op-entry.acct-db EQ acct.acct THEN 
                     ksh-bal = ksh-bal + op-entry.amt-rub.
                  IF op-entry.acct-cr EQ acct.acct THEN
                     ksh-bal = ksh-bal - op-entry.amt-rub.
               END.                                 
            END.
            IF Xresult EQ -2 AND (ksh-bal NE 0 OR ksh-val NE 0)  THEN 
               Xresult = 0.    
         END.
         
         ksh-bal = IF loan.currency  EQ ''
                   THEN (IF acct.side EQ 'А'
                         THEN ksh-bal
                         ELSE ksh-bal * (-1))
                   ELSE (IF acct.side EQ 'А'
                         THEN ksh-val
                         ELSE ksh-val * (-1)).
         mReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')).
      END.
   END.
END.

/* ПРОПИСЬЮ: Сумма вклада */
WHEN "сум_вкл_мульт_проп" THEN
DO:
   IF vCurr NE ? AND AVAIL loan THEN DO:
      ASSIGN vParentCC = loan.cont-code. 
      FIND FIRST loan WHERE loan.contract         EQ "dps"
                        AND loan.parent-cont-code EQ vParentCC
                        AND loan.parent-contract  EQ "dps" 
                        AND loan.currency         EQ vCurr 
                        NO-LOCK NO-ERROR. 
      RUN GetBaseAcctRole (RECID(loan),
                           xDate,
                           OUTPUT l_acct).       
      RUN GetBaseKodOst (l_acct,
                         OUTPUT kod_ost).
      FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                                    AND loan-acct.since     LE xdate
                                    NO-LOCK NO-ERROR.   
      IF AVAIL loan-acct THEN
      DO:
         in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
         FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
            RUN kau-pos.p(loan-acct.acct,
                         loan-acct.currency,
                         xdate,
                         xdate,
                         rab_str,
                         in_kau).
                         
         /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
         IF RETURN-VALUE EQ "Ошибка" THEN
            ASSIGN Xresult = -2.
    
         IF ksh-bal = 0 AND ksh-val = 0 THEN
            RUN get-kauost-trans(loan.contract,
                                 loan.cont-code,
                                 "*",
                                 loan-acct.acct-type,
                                 "",
                                 "",
                                 kod_ost,
                                 end-date,
                                 end-date,
                                 gop-status,
                                 OUTPUT ksh-bal). 
            
            FIND FIRST code WHERE code.class EQ 'ШаблКау'
                              AND code.code  EQ acct.kau-id NO-LOCK NO-ERROR.
            SubStatus = GetEntries(2,code.misc[4],",",""). 
                       
            IF     SubStatus NE "" 
               AND SubStatus GE rab_str THEN
            DO:
               FOR EACH op-entry WHERE (op-entry.acct-cr   EQ acct.acct
                                    AND op-entry.op-date   LE Xdate
                                    AND op-entry.op-status GE rab_str
                                    AND op-entry.op-status LT SubStatus) 
                                    OR (op-entry.acct-db   EQ acct.acct
                                    AND op-entry.op-date   LE Xdate
                                    AND op-entry.op-status GE rab_str
                                    AND op-entry.op-status LT SubStatus)                       
               NO-LOCK:
                  IF acct.currency NE "" THEN
                  DO:
                     IF op-entry.acct-db EQ acct.acct THEN 
                        ksh-val = ksh-val + op-entry.amt-cur.
                     IF op-entry.acct-cr EQ acct.acct THEN
                        ksh-val = ksh-val - op-entry.amt-cur.      
                  END.    
                  IF acct.currency EQ "" THEN
                  DO:
                     IF op-entry.acct-db EQ acct.acct THEN 
                        ksh-bal = ksh-bal + op-entry.amt-rub.
                     IF op-entry.acct-cr EQ acct.acct THEN
                        ksh-bal = ksh-bal - op-entry.amt-rub.
                  END.                                 
               END.
               IF Xresult EQ -2 AND (ksh-bal NE 0 OR ksh-val NE 0) THEN 
                  Xresult = 0.     
            END.
            
            ksh-bal = IF loan.currency  EQ ''
                     THEN (IF acct.side EQ 'А'
                           THEN ksh-bal
                           ELSE ksh-bal * (-1))
                     ELSE (IF acct.side EQ 'А'
                           THEN ksh-val
                           ELSE ksh-val * (-1)).
            RUN "x-amtstr.p" (ksh-bal,
                             loan-acct.currency,
                             YES,
                             YES,
                             OUTPUT c1,
                             OUTPUT c2).
    
            c1 = c1 + " " + c2.

           mReturn = IF logTrim THEN TrimFormula(chTrim,c1) ELSE c1.
      END.
   END.
END.

/* Сумма вклада без суммы прописью */
WHEN "сум_вкл2" THEN
DO:
   IF AVAIL loan-acct THEN
   DO:
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
      RUN kau-pos.p(loan-acct.acct,
                    loan-acct.currency,
                    xdate,
                    xdate,
                    rab_str,
                    in_kau)
                    .
      
      /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
      IF RETURN-VALUE EQ "Ошибка" THEN
         ASSIGN Xresult = -2.

      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal). 

      ksh-bal = IF loan.currency  EQ ''
                THEN (IF acct.side EQ 'А'
                      THEN ksh-bal
                      ELSE ksh-bal * (-1))
                ELSE (IF acct.side EQ 'А'
                      THEN ksh-val
                      ELSE ksh-val * (-1)).
      mReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')) .
   END.
END.

/* Сумма вклада без суммы прописью с добавлением "руб" и "коп"*/
WHEN "сумвкл2проп2" THEN
   RUN СумВклПроп("сумвкл2проп2",OUTPUT mReturn).

WHEN "сум_сумП2" THEN
   RUN Сум_Вкл("сум_сумП2", OUTPUT mReturn).

/* ПРОПИСЬЮ: Сумма вклада */
WHEN "сум_вкл2проп" THEN
   RUN СумВклПроп("сум_вкл2проп",OUTPUT mReturn).

WHEN "сум_вкл_ц" THEN
DO:
   IF AVAIL loan-acct THEN
   DO:
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
      RUN kau-pos.p(loan-acct.acct,
                    loan-acct.currency,
                    xdate,
                    xdate,
                    gop-status,
                    in_kau).
      /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
      IF RETURN-VALUE EQ "Ошибка" THEN
         ASSIGN Xresult = -2.
      ksh-bal = IF loan.currency  EQ ''
                THEN (IF acct.side EQ 'А'
                      THEN ksh-bal
                      ELSE ksh-bal * (-1))
                ELSE (IF acct.side EQ 'А'
                      THEN ksh-val
                      ELSE ksh-val * (-1)).
      RUN "x-amtstr.p" (ksh-bal,
                        loan-acct.currency,
                        YES,
                        YES,
                        OUTPUT c1,
                        OUTPUT c2)
                        .
      c1 = c1 + " " + c2.
      mReturn = c_center(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99'))  + 
                (IF NOT RetString THEN "~n~n" ELSE "") +
                 c_center(c1).

   END.
END.

WHEN "вал_сч" THEN
DO:   
   IF rab_str EQ "" THEN
      FIND FIRST currency WHERE currency.currency EQ loan-acct.currency
           NO-LOCK NO-ERROR.
   ELSE DO:
      ASSIGN vCurr = rab_str.
      vCurr = IF rab_str EQ "810" THEN ""
                                  ELSE rab_str.

      IF vCurr NE ? THEN DO:
         ASSIGN vParentCC = loan.cont-code.          
         FIND FIRST loan WHERE loan.contract         EQ "dps"
                           AND loan.parent-cont-code EQ vParentCC
                           AND loan.parent-contract  EQ "dps" 
                           AND loan.currency         EQ vCurr 
                           NO-LOCK NO-ERROR.        
         IF AVAIL loan THEN
         DO:            
            FIND FIRST currency WHERE currency.currency EQ vCurr
               NO-LOCK NO-ERROR. 

         END.
      END.
   END.

   IF AVAIL currency THEN
      mReturn = TRIM(currency.name-currenc).
END.

WHEN "вал_дог" THEN
DO:
   FIND FIRST currency WHERE currency.currency EQ loan-acct.currency
                       NO-LOCK NO-ERROR.
   IF AVAIL currency THEN
   DO:
      IF loan-acct.currency = "840"
      THEN mReturn =  "ДОЛЛАРАХ США".
      ELSE IF loan-acct.currency = ""
           THEN mReturn = "РУБЛЯХ".
      ELSE IF loan-acct.currency = "276" OR loan-acct.currency = "280"
           THEN mReturn = "НЕМЕЦКИХ МАРКАХ".
      ELSE mReturn = currency.name-currenc.
   END.
END.

WHEN "нач_ст"   /* Начальная ставка             */ THEN RUN Комиссия ('commission', NO,  OUTPUT mReturn).
WHEN "нач_ст_проп" /* Начальная ставка прописью */ THEN RUN Комиссия ('commission', YES, OUTPUT mReturn).
WHEN "тек_ст"   /*          Проц.ставка текущая */ THEN RUN Комиссия ('commission', NO,  OUTPUT mReturn).
WHEN "тек_стШ"  /* Штрафная проц.ставка текущая */ THEN RUN Комиссия ("pen-commi" , NO,  OUTPUT mReturn).
WHEN "тек_ст_проп" /*Текущая тсавка прописью*/     THEN RUN Комиссия ('commission', YES, OUTPUT mReturn).
WHEN "тек_стШ_проп"  /* Штрафная проц.ставка прописью */ THEN RUN Комиссия ("pen-commi" , YES,  OUTPUT mReturn).

WHEN "фио" THEN
DO:
   out_str = IF NOT RetString THEN CAPS(person.name-last + " " + person.first-name)
                              ELSE person.name-last + " " + person.first-name.
   IF     NOT RetString 
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim,out_str).
   ELSE mReturn = out_str.
END.
WHEN "фиоРод" THEN
DO:
   mReturn = GetXAttrValueEx("person",
                            STRING(person.person-id),
                            "ПадежРод",
                            person.name-last + " " + person.first-names).
END.
WHEN "фиоДат" THEN
DO:
   mReturn = GetXAttrValueEx("person",
                            STRING(person.person-id),
                            "ПадежДат",
                            person.name-last + " " + person.first-names).
END.
WHEN "фиоТвор" THEN
DO:
   mReturn = GetXAttrValueEx("person",
                            STRING(person.person-id),
                            "ПадежТвор",
                            person.name-last + " " + person.first-names).

END.

WHEN "фамилия" THEN
   IF     NOT RetString
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim, person.name-last). 
   ELSE mReturn = person.name-last.

WHEN "имя_отч" THEN
   IF     NOT RetString
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim, person.first-name). 
   ELSE mReturn = person.first-name.

WHEN "адрес" THEN
DO:
   DEF VAR vDefinedLength AS INT64.
   DEF VAR vAddressString AS CHAR.
   
   RUN RetAdr.p(person.person-id,"Ч","АдрПроп",?,OUTPUT vAddressString).     /* aa4 -- подставляет к адресу "д.", "кв.", и т.п. */
  /* vAddressString = trimaddress(person.address[1])
                               + TRIM(person.address[2]). */                         
   vDefinedLength = INT64(ENTRY(2,strpar)) NO-ERROR.                       
   IF NOT RetString THEN DO:   
      IF NOT ERROR-STATUS:ERROR /*Проверка параметра количества символов на формат. */
            AND vDefinedLength GT 0 THEN        /*Если не положительное, то печатаем до конца строки*/
         mReturn =       /*Определяем формат под адрес (см. заявку 25657)*/
            IF INT64(ENTRY(2,strpar)) > LENGTH(vAddressString) /*Если под адрес отведено больше чем надо символов,*/
               THEN STRING(vAddressString ,  "x("+ STRING(LENGTH(vAddressString)) + ")") /*то выводим только адрес (без пробелов справа),*/
               ELSE STRING(vAddressString ,  "x(" + ENTRY(2,strpar) + ")"). /*в противном случае адрес выводится в строке заданной длины.*/      
      ELSE
         DO: /*Допечатывает до конца строки. заявка 15409*/
              vAddressString = SUBSTRING(vAddressString,
                                       1,
                                       (PrinterWidth - LENGTH(sh-context)))
                                       .
               mReturn = STRING (vAddressString, "x(" + STRING(LENGTH(vAddressString)) + ")").
         END.
   END.
   ELSE 
      mReturn = vAddressString.
END.

WHEN "адрес_1" THEN
DO:
   mReturn = STRING( trimaddress(person.address[1]),"x(35)").
   IF person.address[2] NE "" 
   THEN ASSIGN
      mReturn = mReturn + "~n" + FILL(" ",43) + "───────────────────────────────────"
      mReturn = mReturn + "~n" + FILL(" ",43) + person.address[2].
   .
END.

WHEN "адрес_2" THEN
DO:
   out_str = trimaddress(fGetStrAdr(person.address[1] + person.address[2])).
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,SUBSTRING(out_str,1,35)). 
      ELSE mReturn = SUBSTRING(out_str,1,35).
END.

WHEN "адрес_21" THEN
DO:
   out_str = trimaddress(fGetStrAdr(person.address[1] + person.address[2])).
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,SUBSTRING(out_str,36,35)). 
      ELSE mReturn = SUBSTRING(out_str,36,35).
END.

WHEN "тел" THEN DO:
   mReturn = ENTRY(1,person.phone[1]).
   {additem.i mReturn ENTRY(2,person.phone[1])}
   {additem.i mReturn ENTRY(1,person.phone[2])}
   {additem.i mReturn ENTRY(2,person.phone[2])}
END.

WHEN "документ" THEN
DO:
   out_str = person.document-id + " N " + person.document.
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,out_str). 
      ELSE mReturn = STRING( out_str, "x(78)").
END.


WHEN "ВлДокСерия" THEN
DO:
   FIND FIRST cust-ident WHERE cust-ident.class-code     EQ "p-cust-ident"
                           AND cust-ident.cust-code-type EQ person.document-id
                           AND cust-ident.cust-cat       EQ "Ч"
                           AND cust-ident.cust-id        EQ person.person-id
                           AND (   cust-ident.close-date EQ ?
                           OR cust-ident.close-date LE xDate)
   NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
      mReturn = substr(cust-ident.cust-code,1,5).

END.

WHEN "ВлДокНомер" THEN
DO:
   FIND FIRST cust-ident WHERE cust-ident.class-code     EQ "p-cust-ident"
                           AND cust-ident.cust-code-type EQ person.document-id
                           AND cust-ident.cust-cat       EQ "Ч"
                           AND cust-ident.cust-id        EQ person.person-id
                           AND (   cust-ident.close-date EQ ?
                           OR cust-ident.close-date LE xDate)
   NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
      mReturn = substr(cust-ident.cust-code,6,7).
END.
WHEN "ВлДокКодП" THEN
DO:
   FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                           AND (   cust-ident.close-date EQ ?
                           OR cust-ident.close-date GE xdate)
                           AND cust-ident.cust-cat       EQ "Ч"
                           AND cust-ident.cust-id        EQ person.person-id
                           AND cust-ident.cust-code-type EQ person.document-id
   NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
      mReturn = GetXAttrValue("cust-ident",
                              cust-ident.cust-code-type + "," 
                            + cust-ident.cust-code + ","
                            + STRING(cust-ident.cust-type-num),
                              "Подразд").
END.

WHEN  "ФИОРукР" THEN DO:
   out_str = FGetSetting("ФИОРук", "", "") .
   RUN corr-bin.p (INPUT "рфио", INPUT-OUTPUT out_str).
    IF     NOT RetString
      AND logTrim 
    THEN mReturn = TrimFormula(chTrim, out_str). 
    ELSE mReturn = out_str.
 END.   
WHEN  "ДолжнРукР" THEN DO:
   out_str = FGetSetting("ДолжнРук", "", "").
   RUN corr-bin.p (INPUT "Д", INPUT-OUTPUT out_str).
    IF     NOT RetString
      AND logTrim 
    THEN mReturn = TrimFormula(chTrim, out_str). 
    ELSE mReturn = out_str.
 END.
WHEN  "ДолжнРук" THEN 
    IF     NOT RetString
      AND logTrim 
    THEN mReturn = TrimFormula(chTrim, FGetSetting("ДолжнРук", "", "")). 
    ELSE mReturn = FGetSetting("ДолжнРук", "", "").

WHEN "документ_1" THEN
DO:
   out_str = person.document-id + " N " + person.document.
   IF LENGTH(out_str) LE 41 THEN
      mReturn = STRING(out_str , "x(41)").
   ELSE ASSIGN
      mReturn = SUBSTRING(person.issue,1,41) + "~n" 
      mReturn = mReturn + FILL(" ",37) + "─────────────────────────────────────────" + "~n" 
      mReturn = mReturn + FILL(" ",37) + (SUBSTRING(person.issue,42,41)) + "~n" 
      mReturn = mReturn + FILL(" ",37) + "─────────────────────────────────────────" + "~n" 
      mReturn = mReturn + FILL(" ",37) + (SUBSTRING(person.issue,83,41)) .
   .
END.

WHEN "документ_2" THEN
DO:
   out_str = person.document-id + " N " + person.document.
   mReturn = SUBSTRING(out_str,1,35).
   IF LENGTH(out_str) > 35  THEN
      mReturn = mReturn + "~n" + FILL(" ",43) + SUBSTRING(out_str,36,35).
END.

WHEN "выдан" THEN
   RUN PrintIssue (COMMAND, rab_str, logTrim, OUTPUT mReturn).

WHEN "номер_счета" THEN
      mReturn = STRING (IF AVAIL loan-acct
                        THEN DelFilFromAcct(loan-acct.acct)
                        ELSE "",
                       "x(20)").

WHEN "роль_счета" THEN
DO:
   mReturn = STRING (IF loan.end-date EQ ?
                     THEN 'до востребования'
                     ELSE 'депозитный срочный',
                    "x(20)").
END.

WHEN "договор" THEN
   IF     NOT RetString
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim,loan.doc-ref). 
   ELSE mReturn = loan.doc-ref.

WHEN "гражд" THEN
DO:
   IF AVAIL person THEN
      FIND FIRST country WHERE country.country-id EQ person.country-id
           NO-LOCK NO-ERROR.
   mReturn =  (IF AVAIL country
               THEN TRIM(country.country-name)
               ELSE "").
END.

WHEN "город" THEN
DO:
   ASSIGN vBankMFO = FGetSettingEx("БанкМФО",?,?,YES).
   FIND banks-code WHERE banks-code.bank-code-type EQ "МФО-9" AND
                         banks-code.bank-code      EQ vBankMFO
                   NO-LOCK NO-ERROR.
   FIND banks OF banks-code NO-LOCK NO-ERROR.
   IF AVAIL banks THEN
      IF NOT RetString THEN
         mReturn = STRING ("   г. " + banks.town , "x(27)").
      ELSE
         mReturn = "   г. " + banks.town.
END.

WHEN "срок_прол" THEN
DO:
   FIND FIRST op-template WHERE op-template.op-template EQ loan.op-template
                          AND   op-template.op-kind     EQ loan.op-kind
                          NO-LOCK NO-ERROR.
   in-surrogate = op-template.op-kind + ',' + STRING(op-templ.op-templ).

   IF AvailXattr("op-template",in-surrogate,"prol-kind") THEN
      in-surrogate  = GetXattrValue("op-template",in-surrogate,"prol-kind").

   ELSE
   DO:
      i = loan.end-date - loan.open-date.
      num_templ = IF (i GE 5 AND i LE 20) OR
                      i EQ (INT64((i / 10)) * 10)
                  THEN 3
                  ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
      out_str = STRING (i,'>>>9') + ' ' + dat_(num_templ,'Д').
      IF logTrim 
         THEN PUT STREAM fil UNFORMATTED TrimFormula(chTrim,out_str). 
         ELSE PUT STREAM fil out_str FORMAT "x(30)".
      RETURN.
   END.

   FIND FIRST op-kind WHERE op-kind.op-kind EQ in-surrogate NO-LOCK NO-ERROR.

   num_templ = get_op-templ(op-kind.op-kind,'loan','').

   FIND FIRST op-template OF op-kind
        WHERE op-template.op-kind     EQ op-kind.op-kind
          AND op-template.op-template EQ num_templ
              NO-LOCK NO-ERROR.

   in-surrogate = op-template.op-kind + ',' + STRING(op-templ.op-templ).

   IF AvailXattr("op-template",in-surrogate,"dep-period")  THEN
   DO:
      in-surrogate = GetXattrValue("op-template",in-surrogate,"dep-period").

         out_str = "".
      DO i = 1 TO NUM-ENTRIES(in-surrogate):
         out_str = out_str + ' ' + SUBSTR(ENTRY(i,in-surrogate),3).
         IF INT64(SUBSTR(ENTRY(i,in-surrogate),3)) EQ 1 THEN
             out_str = out_str + ' ' + dat_(1,SUBSTR(ENTRY(i,in-surrogate),1,1)).
         ELSE IF INT64(SUBSTR(ENTRY(i,in-surrogate),3)) GE 2 AND
             INT64(SUBSTR(ENTRY(i,in-surrogate),3)) LE 4 THEN
             out_str = out_str + ' ' + dat_(2,SUBSTR(ENTRY(i,in-surrogate),1,1)).
         ELSE IF INT64(SUBSTR(ENTRY(i,in-surrogate),3)) GE 5 AND
             INT64(SUBSTR(ENTRY(i,in-surrogate),3)) LE 20 THEN
             out_str = out_str + ' ' + dat_(3,SUBSTR(ENTRY(i,in-surrogate),1,1)).
      END.
   END.
   ELSE
     out_str = "до востребования".
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,out_str). 
      ELSE mReturn = STRING( out_str , "x(30)").
END.
/* Минимальная сумма доп.взноса */
WHEN "МинСумма"  THEN
DO:
  IF vCurr NE ? AND AVAIL loan THEN
  DO:
     ASSIGN vParentCC = loan.cont-code. 
     FIND FIRST loan WHERE loan.contract         EQ "dps"
                       AND loan.parent-cont-code EQ vParentCC 
                       AND loan.parent-contract  EQ "dps"
                       AND loan.currency         EQ vCurr 
                       NO-LOCK NO-ERROR. 
  END.

  RUN get_last_param in h_dpspc (RECID(loan),
                                         xdate1,
                                         xdate,
                                         "МинСумма",
                                          OUTPUT out_str).

  mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* Минимальный неснижаемый остаток по вкладу */
WHEN "МинОст" THEN
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"МинОст",?).
  IF out_str = ? THEN

     RUN get_last_param in h_dpspc (rid_loan,
                                    xdate1,
                                    xdate,
                                    "МинОст",
                                    OUTPUT out_str).
   mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* Сумма прописью - минимальная сумма доп.взноса */
WHEN "МинСуммаПРОП"  THEN
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                         xdate1,
                                         xdate,
                                         "МинСумма",
                                          OUTPUT out_str).
   RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency,
                    YES,
                    YES,
                    OUTPUT c1,
                    OUTPUT c2).
   
   mReturn = TRIM(c1 + " " + c2).
END.

/* Сумма прописью - минимальный неснижаемый остаток по вкладу */
WHEN "МинОстПРОП" THEN 
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"МинОст",?).
  IF out_str = ? THEN

     RUN get_last_param in h_dpspc (rid_loan,
                                            xdate1,
                                            xdate,
                                            "МинОст",
                                            OUTPUT out_str).
  RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency, 
                    YES, 
                    YES, 
                    OUTPUT c1, 
                    OUTPUT c2).
  mReturn = TRIM(c1 + " " + c2).    
END.

/* Максимальный остаток по вкладу */
WHEN "МаксОст" THEN
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "МаксОст",
                                          OUTPUT out_str).
   IF out_str = ? THEN
      out_str = GetXattrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "МаксОст",
                                ?).
  mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* Сумма прописью - максимальный неснижаемый остаток по вкладу */
WHEN "МаксОстПРОП" THEN 
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "МаксОст",
                                          OUTPUT out_str).
  IF out_str = ? THEN
     out_str = GetXattrValueEx("loan",
                               loan.contract + "," + loan.cont-code,
                               "МаксОст",
                               ?).
  RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency, 
                    YES, 
                    YES, 
                    OUTPUT c1, 
                    OUTPUT c2).
  mReturn = TRIM(c1 + " " + c2).
END.

/* Максимальный допустимый остаток по вкладу */
WHEN "МаксДопОст" THEN
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"МаксДопОст",?).
  mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* Сумма прописью - максимальный допустимый остаток по вкладу */
WHEN "МаксДопОстП" THEN 
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"МаксДопОст",?).
  RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency, 
                    YES, 
                    YES, 
                    OUTPUT c1, 
                    OUTPUT c2).
  mReturn = TRIM(c1 + " " + c2).
END.
/* Минимальная сумма первоначального взноса */
WHEN "МинСПВ" THEN
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "МинСПВ",
                                          OUTPUT out_str).
   mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>9.99")).
END.

/* Сумма прописью - Минимальная сумма первоначального взноса */
WHEN "МинСПВПРОП" THEN 
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "МинСПВ",
                                          OUTPUT out_str).
   RUN "x-amtstr.p" (DECIMAL(out_str),
                     loan.currency, 
                     YES, 
                     YES, 
                     OUTPUT c1, 
                     OUTPUT c2).
   mReturn = TRIM(c1 + " " + c2).
END.


/* печать в договоре указанного доп. реквизита вкладчика */
WHEN "ДопРекВкл" THEN 
DO:
   mReturn = GetXAttrValue("person",string(person.person-id) ,ENTRY(2,strpar)).
     /* где ENTRY(2,strpar) - получение доп. реквизита */
END.

/* Фамилия 3го лица */
WHEN "ФамилияТЛ" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.name-last.
     
END.

/* Имя Отчество 3го лица */
WHEN "ИмяОтчТЛ" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.first-names.
END.

/* Адрес 3го лица */
WHEN "АдресТЛ" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.address[1].
END.

/* Тип документа 3го лица */
WHEN "ТипДокТЛ" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE
   DO:
      FIND FIRST code WHERE CODE.code = person.document-id
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE code THEN
         mReturn = "".
      ELSE
         mReturn = CODE.name.
   END.
END.

/* Номер документа 3го лица */
WHEN "НомерДокТЛ" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.document.
END.

/* Кем выдан документ 3го лица */
WHEN "ВыданДокТЛ" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.issue.
END.

/*Дата окончания периода пополнения*/
WHEN "ОкончПополн" THEN 
DO:
    DEF VAR mLim-date AS DATE NO-UNDO.
    DEF VAR mMess AS CHAR NO-UNDO INIT "".
    RUN end_doloan_dps in h_dpspc (RECID(loan),
                                       xDate,
                                       OUTPUT mLim-date,
                                       OUTPUT mMess).

    IF mLim-date NE ? THEN
        mReturn = STRING(mLim-date, "99/99/9999").
END.

WHEN "МаксПролМ" THEN
DO:
   ASSIGN vParentCC = loan.cont-code. 
   FIND FIRST loan WHERE loan.contract         EQ "dps"
                     AND loan.parent-cont-code EQ vParentCC 
                     AND loan.parent-contract  EQ "dps"
                     NO-LOCK NO-ERROR. 
   /*проверка ограничения кол-ва пролонгаций через классификатор
   Если ссылки на классификатор нет - то проверяем обычным путем */
   RUN  get_limitprol_from_code in h_dpspc (RECID(loan),
                                                    XDate,
                                                    OUTPUT vLimitProl). 
   IF vLimitProl = ? THEN 
      /*на шаблоне вклада транзакции открытия или на классе вклада - реквизит limitprol*/
      RUN get-param-const in h_dpspc (RECID(loan),
                                              "limitprol",
                                              OUTPUT vLimitProl).
   IF vLimitProl <> ? THEN
      mReturn = STRING(vLimitProl).
END.

WHEN "ДВыдДокДЛ" THEN
DO:
   FIND FIRST cust-role WHERE cust-role.file-name  EQ "loan"
                          AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
                          AND cust-role.Class-Code EQ "Доверенное_лицо"
      NO-LOCK NO-ERROR.
   IF     AVAIL cust-role
      AND cust-role.cust-cat EQ "Ч" 
   THEN 
      mReturn = fGetDocIssueDate (INT64(cust-role.cust-id)).
   ELSE
      mReturn = FILL("_",10).
END.
      
WHEN "НомерДокДЛ" THEN
DO:
   FIND FIRST cust-role WHERE cust-role.file-name  EQ "loan"
                          AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
                          AND cust-role.Class-Code EQ "Доверенное_лицо"
      NO-LOCK NO-ERROR.
   IF     AVAIL cust-role
      AND cust-role.cust-cat EQ "Ч" 
   THEN
      FIND FIRST b-person WHERE b-person.person-id EQ INT64(cust-role.cust-id)
         NO-LOCK NO-ERROR.
   IF AVAIL b-person 
   THEN 
      mReturn = b-person.document.
   ELSE
      mReturn = FILL("_",20).
END.

WHEN "дата_прописью" THEN
DO:
   mReturn = getDateString(DATE(rab_str), mParam2 EQ "ДА").
END. 

WHEN "inst_prn" THEN
DO:
   gClass = loan.class-code.
   gSurrogate = loan.contract + "," + loan.cont-code.
   gPrepareInstance = REPLACE(ENTRY(2,strpar),":",",").
   ENTRY(NUM-ENTRIES(gPrepareInstance),gPrepareInstance) = "".
   strpar = SUBSTRING(strpar,INDEX(strpar,",") + 1).
   strpar = REPLACE(strpar,"<<","(").
   strpar = REPLACE(strpar,">>",")").
   run inst_prn.p(
      OUTPUT Xresult,
      Xdate1,
      Xdate,
      strpar).
   mReturn = RETURN-VALUE.
END. 

WHEN "дата_кап" THEN
   DO:
	  d = DATE(GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ДатаНачПереч","")).
	  mReturn = STRING(DAY(d)) + " " + 
                    ENTRY(MONTH(d), mont_h) +
                    STRING(YEAR(d), " 9999 г.").
END.

WHEN "ТипВкл" THEN
DO:
   IF rab_str EQ "" THEN
      mReturn = loan.cont-type.
   ELSE DO:
      FIND FIRST code WHERE code.class  EQ 'cont-type'
                        AND code.parent EQ 'cont-type'
                        AND code.code   EQ loan.cont-type
      NO-LOCK NO-ERROR.
        IF AVAIL code THEN
        DO:
           CASE rab_str:
              WHEN "Назв" THEN 
                 mReturn = code.name.
              WHEN "Зн" THEN
                 mReturn = code.val.
           END CASE.
        END.
   END.
END.

WHEN "КодАдр" THEN
DO:

   /*RUN GetAdrCode (loan.cust-cat + "," + STRING(loan.cust-id),
                   rab_str,
                   OUTPUT mReturn).*/
 
  RUN RetAdr.p(loan.cust-id,loan.cust-cat,rab_str,loan.open-date,OUTPUT mReturn). /* ayv - изменено форматирование адреса*/
        IF mReturn EQ '' and rab_str = 'АдрФакт' THEN DO: RUN RetAdr.p(loan.cust-id,loan.cust-cat,'АдрПроп',loan.open-date,OUTPUT mReturn). /*ZSS по вкладам если адрес факта пустой то брать прописку*/
        END.

END.

/* Переменные для работы с субъектами по вкладу */
WHEN "АдресФЛ" THEN
DO:
   RUN GetPersType (rid_loan,                    
                    mParam2,
                    OUTPUT mPersSurr).
   RUN GetAdrCode (mPersSurr,
                   rab_str,
                   OUTPUT mReturn).
END.

WHEN "ФамилияФЛ" THEN
DO:
   RUN GetSignsPers ("Фамилия",
                     rab_str,
                     OUTPUT mReturn).
END.

WHEN "ИмяОтчестФЛ" THEN
   RUN GetSignsPers ("ИмяОтчест",
                     rab_str,
                     OUTPUT mReturn).

WHEN "ДатаРождФЛ" THEN
   RUN GetSignsPers ("ДРождДЛ",
                     rab_str,
                     OUTPUT mReturn).

WHEN "МестоРождФЛ" THEN
   RUN GetSignsPers ("МестоРожд",
                     rab_str,
                     OUTPUT mReturn).

WHEN "ТипДокФЛ" THEN
   RUN GetSignsPers ("ТипДок",
                     rab_str,
                     OUTPUT mReturn).

WHEN "НомерСерияФЛ" THEN
   RUN GetSignsPers ("НомерДок",
                     rab_str,
                     OUTPUT mReturn).

WHEN "ДатаВыдДокФЛ" THEN
   RUN GetSignsPers ("ДатаВыдДок",
                     rab_str,
                     OUTPUT mReturn).

WHEN "КемВыдДокФЛ" THEN
   RUN GetSignsPers ("КемВыдДок",
                     rab_str,
                     OUTPUT mReturn).

WHEN "КодПодрДокФЛ" THEN
   RUN GetSignsPers ("КодПодрДок",
                     rab_str,
                     OUTPUT mReturn).

WHEN "ТелДомФЛ" THEN
   RUN GetSignsPers ("ТелДом",
                     rab_str,
                     OUTPUT mReturn).

WHEN "ТелДомФактФЛ" THEN
   RUN GetSignsPers ("ТелДомФакт",
                     rab_str,
                     OUTPUT mReturn).

WHEN "ТелРабФЛ" THEN
   RUN GetSignsPers ("ТелРаб",
                     rab_str,
                     OUTPUT mReturn).

WHEN "ТелМобФЛ" THEN
   RUN GetSignsPers ("ТелМоб",
                     rab_str,
                     OUTPUT mReturn).

/* Возвращает X если вклад До востребования */
WHEN "ВкладДВ" THEN
DO:
   IF loan.end-date EQ ? THEN
      mReturn = "X".
   ELSE
      mReturn = " ".
END.

/* Возвращает X если класс вклада соответствует маске */
WHEN "ВкладКласс" THEN
DO:
   IF loan.class-code MATCHES ENTRY(2,strpar) THEN
      mReturn = "X".
   ELSE
      mReturn = " ".
END.

END CASE.


IF COMMAND BEGINS("номер_прикр") THEN
DO:
   vCurr = ?.

   IF LENGTH(COMMAND) > 11 THEN
   DO:
      mSubs = SUBSTR(COMMAND, 13, (LENGTH(COMMAND) - 13) + 
                        IF SUBSTR(COMMAND, LENGTH(COMMAND)) EQ ")" THEN 0
                                                                   ELSE 1).
      COMMAND = SUBSTR(COMMAND, 1 , 11).

      IF NUM-ENTRIES(mSubs, "@") GT 1 THEN
         ASSIGN 
             vCurr = IF ENTRY(2, mSubs,"@") EQ "810" THEN ""
                                                     ELSE ENTRY(2, mSubs, "@")
             mSubs = ENTRY(1, mSubs,"@") 
             .
   END.
   IF INDEX(mSubs, "_") NE 0 THEN
      mSubs = REPLACE(mSubs, "_", "-"). 

   FIND FIRST loan WHERE 
        RECID(loan) EQ rid_loan NO-LOCK NO-ERROR.   
   IF vCurr NE ? AND AVAIL loan THEN DO:
      ASSIGN vParentCC = loan.cont-code. 
      FIND FIRST loan WHERE loan.contract         EQ "dps"
                        AND loan.parent-cont-code EQ vParentCC 
                        AND loan.currency         EQ vCurr 
                        NO-LOCK NO-ERROR.                           
   END.
   IF AVAIL loan THEN
   DO:      
      
      FIND LAST loan-acct WHERE
             loan-acct.contract  EQ "DPS"
         AND loan-acct.acct-type EQ mSubs
         AND loan-acct.cont-code EQ loan.cont-code
         AND loan-acct.since     LE xdate
      NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN
      DO:      
         mReturn = DelFilFromAcct(loan-acct.acct).
      END.
   END.
END.

IF CAN-DO("фиоДЛ,АдресДЛ,ТипДокДЛ,НомерДокДЛ,КВыданДокДЛ,ДВыдДокДЛ,ДРождДЛ", COMMAND) THEN
   RUN GetSignsPers(COMMAND, "Доверенное_лицо",OUTPUT mReturn).

IF CAN-DO("фиоНЛ,АдресНЛ,ТипДокНЛ,НомерДокНЛ,КВыданДокНЛ,ДВыдДокНЛ,ДРождНЛ,ДоляНЛ", COMMAND) THEN
   RUN GetSignsPers(COMMAND, "НаследникВклада",OUTPUT mReturn).
   
IF CAN-DO("УполнЛицо,УполнЛицоРП,ДолжнУполнЛица,ДолжнУполнЛицаРП,НомДовУполнЛица,ДатаДовУполнЛица,ОсновУполнЛица", COMMAND) THEN
   RUN GetDocSotr(COMMAND, OUTPUT mReturn).   

/*Если печатали по вкладу с нулевой суммой, то есть ошибка, а надо чтобы ее не было*/
IF xResult EQ -2 AND NOT RetString THEN
DO:
   xResult = 0.
   IF logReturn THEN
   RETURN "".
END.
IF NOT RetString 
THEN      
   PUT STREAM fil UNFORMATTED mReturn.
ELSE
   RETURN mReturn.

/* Процедура получения реквизитов Доверенного лица или Наследника */
PROCEDURE GetSignsPers:
   DEF INPUT  PARAM iCommand AS CHAR NO-UNDO.
   DEF INPUT  PARAM iCode    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oReturn  AS CHAR NO-UNDO.

   DEF BUFFER person       FOR person.
   DEF BUFFER cust-role    FOR cust-role.
   DEF BUFFER cust-ident   FOR cust-ident.
   DEF BUFFER country      FOR country.

   FOR FIRST cust-role         WHERE
             cust-role.file-name  EQ "loan"
         AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
         AND cust-role.class-code EQ iCode
      NO-LOCK,
      FIRST  person            WHERE
             person.person-id     EQ INT64(CUST-ROLE.cust-id)
      NO-LOCK:
                        /* Определяем ДР. */
      IF iCommand BEGINS "ДР|"
         THEN oReturn   =  GetXAttrValueEx (
                              "person",
                              STRING (person.person-id),
                              ENTRY (2, iCommand, "|"),
                              "").
                        /* Точные команды. */
      CASE iCommand:
      WHEN "фиоДЛ"      THEN
         oReturn = person.name-last + " " + person.first-names.
      WHEN "АдресДЛ"    THEN
         oReturn = person.address[1].
      WHEN "ТипДокДЛ" THEN
         oReturn = GetCodeName("КодДокум", person.document-id).
      WHEN "НомерДокДЛ" THEN
         oReturn = person.document.
      WHEN "КВыданДокДЛ" THEN
         oReturn = person.issue.
      WHEN "ДВыдДокДЛ" THEN
         oReturn = GetXAttrValue("person",
                                 STRING(person.person-id),
                                 "Document4Date_vid"
                                 ).
      WHEN "ДРождДЛ"   THEN
         oReturn =  STRING(person.birthday,"99/99/9999").
      WHEN "фиоНЛ"      THEN
         oReturn = person.name-last + " " + person.first-names.
      WHEN "АдресНЛ"    THEN
         oReturn = person.address[1].
      WHEN "ТипДокНЛ" THEN
         oReturn = GetCodeName("КодДокум", person.document-id).
      WHEN "НомерДокНЛ" THEN
         oReturn = person.document.
      WHEN "КВыданДокНЛ" THEN
         oReturn = person.issue.
      WHEN "ДоляНЛ" THEN
         oReturn = GetXAttrValueEx("cust-role", 
                                    STRING(cust-role.cust-role-id), 
                                    "heir-part", 
                                    "").
      WHEN "ДВыдДокНЛ" THEN
         oReturn =  GetXAttrValue("person",
                                  STRING(person.person-id),
                                  "Document4Date_vid"
                                  ).
      WHEN "ДРождНЛ"   THEN
         oReturn = string(person.birthday,"99/99/9999").

      WHEN "Фамилия" THEN
         oReturn = person.name-last.
      WHEN "ИмяОтчест" THEN
         oReturn = person.first-names.
      WHEN "МестоРожд" THEN
         oReturn = GetXAttrValue("person",
                                 STRING(person.person-id),
                                 "BirthPlace").
      WHEN "ТипДок" THEN
         oReturn = GetCodeName("КодДокум", person.document-id).
      WHEN "НомерДок" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "Ч"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = cust-ident.cust-code.
         END.
      END.
      WHEN "КемВыдДок" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "Ч"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = cust-ident.issue.
         END.
      END.
      WHEN "ДатаВыдДок" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "Ч"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = STRING(cust-ident.open-date, "99/99/9999").
         END.
      END.
      WHEN "КодПодрДок" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "Ч"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = GetXAttrValue("cust-ident",
                                    cust-ident.cust-code-type + "," 
                                       + cust-ident.cust-code + ","
                                       + STRING(cust-ident.cust-type-num),
                                    "Подразд").
         END.
      END.
      WHEN "ТелДом" THEN
         oReturn = ENTRY(1, person.phone[1]).
      WHEN "ТелДомФакт" THEN
         oReturn = ENTRY(2, person.phone[1]).
      WHEN "ТелРаб" THEN
         oReturn = ENTRY(1, person.phone[2]).
      WHEN "ТелМоб" THEN
      DO:
         oReturn = ENTRY(2, person.phone[2]).
         IF NOT {assigned oReturn} THEN
            oReturn = GetXattrValueEx ("person",
                                       STRING (person.person-id),
                                       "cell-phone",
                                       ?).
      END.
      END CASE.
   END.

   RETURN.
END PROCEDURE.

PROCEDURE Комиссия:
  DEFINE INPUT  PARAMETER in-commission AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iPropis       AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER oReturn       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c_date                AS DATE      NO-UNDO. /*Дата на
                                                                которую идет расчет комиссии*/
  DEFINE VARIABLE c_stat                AS CHARACTER NO-UNDO. /*Статус, по
                                                                которому считать комиссию*/
  DEF VAR mMinOst AS DEC NO-UNDO. /*Минимальный остаток вклада*/
  DEF VAR vSinceDate AS DATE NO-UNDO.
  DEF VAR vDateEnd   AS DATE NO-UNDO.
  DEF VAR vBonusRate AS DEC  NO-UNDO.

  ASSIGN
     c_date = xdate
     c_date = loan.open-date  WHEN command BEGINS "нач_ст"
     c_stat = gop-status
     c_stat = ENTRY(2,strpar) WHEN NUM-ENTRIES(strpar) GE 2
  .

   IF COMMAND BEGINS "нач_ст" AND c_stat NE gop-status 
   THEN logReturn = NO.

  IF vCurr NE ? AND AVAIL loan THEN DO:
     ASSIGN vParentCC = loan.cont-code. 
     FIND FIRST loan WHERE loan.contract         EQ "dps"
                       AND loan.parent-cont-code EQ vParentCC 
                       AND loan.currency         EQ vCurr 
                       NO-LOCK NO-ERROR. 
     RUN GetBaseAcctRole (RECID(loan),
                          c_date,
                          OUTPUT l_acct).       
     RUN GetBaseKodOst (l_acct,
                        OUTPUT kod_ost).
     FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                                   AND loan-acct.since     LE c_date
                                   NO-LOCK NO-ERROR.
  END.
  IF COMMAND BEGINS "нач_ст_проп" THEN
  DO:
     RUN Get-beg-date-prol IN h_dpspc (RECID(loan),
                                       c_date,
                                       OUTPUT c_date,
                                       OUTPUT vDateEnd).
  END.
  IF AVAIL loan-acct THEN
  DO:
  /* Получение кода комиссии */
  IF in-commission = "commission" THEN 
    RUN Get_Last_Commi     in h_dpspc (RECID(loan), c_date, c_date, OUTPUT vCommChar).
  ELSE 
    RUN Get_Last_Pen-Commi in h_dpspc (RECID(loan), c_date, c_date, OUTPUT vCommChar).

  /* Ищем счет */
  FIND FIRST acct OF loan-acct
  NO-LOCK NO-ERROR.

  in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.

  ASSIGN vSinceDate = c_date.
  /*ищем приход на вклад сразу после привязки счета*/
  FIND FIRST kau-entry WHERE
         kau-entry.acct = loan-acct.acct
     AND kau-entry.currency = loan.currency
     AND kau-entry.op-status >= gop-status
     AND NOT kau-entry.debit
     AND kau-entry.op-date >= vSinceDate
     AND kau-entry.kau = in_kau 
     NO-LOCK NO-ERROR.
  IF AVAILABLE kau-entry THEN 
  DO:
     FIND FIRST op OF kau-entry 
        NO-LOCK NO-ERROR.
     IF AVAILABLE op 
     /*плановая дата документа должна совпалать с датой открытия!!!*/
        AND op.contract-date = c_date THEN 
     DO:
        vSinceDate = kau-entry.op-date.
     END.
  END.

  RUN kau-pos.p (loan-acct.acct,
                 loan-acct.currency,
                 vSinceDate,
                 vSinceDate,
                 c_stat,
                 in_kau).
  /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
  IF RETURN-VALUE EQ "Ошибка" THEN
     ASSIGN Xresult = -2.

  ASSIGN
    ksh-bal = ksh-bal * (-1) WHEN loan.currency EQ '' AND acct.side NE 'А'
    ksh-bal = ksh-val * (-1) WHEN loan.currency NE '' AND acct.side NE 'А'
  .

  IF loan.end-date EQ ? THEN
     delta = 0. /* вклад до востребования */
  ELSE
     RUN deposit-dep-period in h_dpspc (RECID(loan),
                                                c_date,
                                                Get-Dep-Period(loan.contract + "," + loan.cont-code),
                                                OUTPUT delta).

  
     
     
 
  
  RUN Get_Last_Inter IN h_dpspc (RECID(loan),c_date,c_date,OUTPUT vInterest).
  IF vInterest <> ?  AND vInterest <> '?'
  THEN DO:
      {findsch.i &dir=LAST &sch=vInterest &since1 =" <= c_date"}
      IF AVAILABLE interest-sch-line AND
         interest-sch-line.proc-name  BEGINS "nchmin_o"
      THEN DO:
          mMinOst = DEC(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"МинОст",?)) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
              ASSIGN  mMinOst = 0.
              IF mMinOst NE ? THEN
              ASSIGN  ksh-bal = mMinOst 
                      ksh-val = mMinOst.
      END.
      
  END.




  {findcom1.i
      &dir       = LAST
      &comm-rate = comm-rate
      &rcom      = vCommChar
      &rsum      = ksh-bal
      &since1    = " le c_date "
      &vPeriodInt = delta
  }

  RUN getbonusrate(c_date, OUTPUT vBonusRate).

  IF AVAIL comm-rate THEN 
     IF NOT iPropis THEN
        oReturn = TRIM(STRING(comm-rate.rate-comm + vBonusRate,'zzzz9.' + rab_str)) .
     ELSE
     DO:
        RUN procprop.p (trim(string(comm-rate.rate-comm + vBonusRate,'zzzz9.' + rab_str)),
                        OUTPUT oReturn).
     END.

  RELEASE op-templ.
  END. /*AVAIL loan-acct */
END PROCEDURE.

PROCEDURE getbonusrate:
   DEF INPUT  PARAM iDate AS DATE  NO-UNDO .
   DEF OUTPUT PARAM oBonusRate AS DEC NO-UNDO .

   DEF VAR vBonus AS INT64 NO-UNDO.
   DEF VAR vCommiBonus AS CHAR NO-UNDO.
   DEF BUFFER bLoan-cond  FOR loan-cond.
   DEF BUFFER bBonus-rate FOR comm-rate.
   
   FIND LAST bLoan-cond WHERE bLoan-cond.contract EQ loan.contract AND
                bLoan-cond.cont-code EQ loan.cont-code AND
                bLoan-cond.since LE iDate 
                NO-LOCK NO-ERROR.
   IF AVAIL bLoan-cond THEN
   DO:          
      vBonus = INT64(GetXattrValueEx('loan-cond',bLoan-cond.contract + "," + bLoan-cond.cont-code + "," +
                               string(YEAR (bLoan-cond.since),"9999") + 
                               string(MONTH(bLoan-cond.since),"99"  ) +
                               string(DAY  (bLoan-cond.since),"99"  ),
                               "Бонус", "0")) NO-ERROR.      
      IF ERROR-STATUS:ERROR OR vBonus = ? THEN
         vBonus = 0. 
      END.
   IF vBonus GT 0 THEN
   DO:
      vCommiBonus = fGetSetting("ВкладнойБонус", ?, "").
      IF vCommiBonus NE "" THEN
      DO:
         { findcom1.i
            &dir=last
            &rsum=0
            &comm-rate=bBonus-rate
            &rcom=vCommiBonus
            &since1=" le iDate "
         }

         IF AVAILABLE bBonus-rate THEN
            oBonusRate = vBonus * bBonus-rate.rate-comm.
      END.
   END.
END PROCEDURE.

PROCEDURE period2dmy :
/*------------------------------------------------------------------------------
  Purpose:   "расшифровка" периода
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM in-str AS CHAR.
  DEFINE OUTPUT PARAM out-yy AS INT64.
  DEFINE OUTPUT PARAM out-mm AS INT64.
  DEFINE OUTPUT PARAM out-dd AS INT64.

  DEF VAR j AS INT64.
  DEF VAR s2 AS CHAR.

  ASSIGN
    out-yy = 0
    out-mm = 0
    out-dd = 0
  .

  REPEAT j = 1 TO NUM-ENTRIES(in-str) :
    s2 = ENTRY(j, in-str).
    IF NUM-ENTRIES(s2, "=") = 2 THEN DO:
      CASE ENTRY(1, s2, "=") :
        WHEN "Г" THEN out-yy = INT64(ENTRY(2, s2, "=")) NO-ERROR.
        WHEN "М" THEN out-mm = INT64(ENTRY(2, s2, "=")) NO-ERROR.
        WHEN "Д" THEN out-dd = INT64(ENTRY(2, s2, "=")) NO-ERROR.
      END CASE.
    END.
  END.
END PROCEDURE.

FUNCTION trimaddress RETURNS CHAR (INPUT in-address AS CHAR):
   DEF VAR t-s AS CHAR NO-UNDO.
   DEF VAR s AS CHAR NO-UNDO.
      DO i = 1 TO NUM-ENTRIES(in-address):
         s = TRIM(ENTRY(i,in-address)).
         IF s = "" THEN NEXT.
         {additem.i t-s s}
      END.
   RETURN t-s.
END.

FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) :
   CASE FormatTrim:
      WHEN "trim"  THEN cValue = TRIM(cValue).
      WHEN "ltrim" THEN cValue = LEFT-TRIM(cValue).
      WHEN "rtrim" THEN cValue = RIGHT-TRIM(cValue).
   END CASE.
   RETURN cValue.
END.

PROCEDURE PrintIssue:
   DEF INPUT  PARAM iCommand AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iSubStr  AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iLogTrim AS LOGICAL   NO-UNDO.
   DEF OUTPUT PARAM oReturn  AS CHARACTER NO-UNDO.

   DEF VAR vDate AS DATE NO-UNDO.
   DEF VAR vType AS INT64 NO-UNDO.
   DEF VAR vTmpStr AS CHARACTER NO-UNDO.

   IF iSubStr MATCHES "*дата" THEN
      ASSIGN vDate = DATE(GetXattrValueEx("person", 
                                          STRING(person.person-id),
                                          "Document4Date_Vid",
                                          "")).

   ASSIGN vTmpStr = person.issue + IF vDate NE ? THEN (", " + STRING(vDate)) 
                                                            ELSE "".

          vTmpStr = REPLACE(vTmpStr,CHR(10),''). /*zss удаление переноса строки(не печатается в дог.вкл)*/

   IF (iSubStr EQ ? OR iSubStr EQ "дата") AND NOT RetString THEN
      ASSIGN vType = 0.
   ELSE IF iSubStr BEGINS "1" AND NOT RetString THEN
      ASSIGN vType = 1.
   ELSE IF iSubStr BEGINS "2" AND NOT (iSubStr BEGINS "21") AND NOT RetString THEN
      ASSIGN vType = 2.
   ELSE IF iSubStr BEGINS "21" AND NOT RetString THEN
      ASSIGN vType = 21.
   ELSE IF RetString THEN
      ASSIGN vType = 3.
   ELSE
      RETURN.

   CASE vType:
      WHEN 0 THEN
      DO:
         ASSIGN vTmpStr = person.issue + IF vDate NE ? THEN (", " + STRING(vDate)) 
                                                            ELSE "".
         IF LENGTH(vTmpStr) LE 78 THEN   
         DO:
            IF iLogTrim THEN 
               oReturn =  TrimFormula(chTrim,vTmpStr). 
            ELSE 
               oReturn = vTmpStr.
      
         END.      
         ELSE
            IF vDate EQ ? OR (vDate NE ? AND
                              LENGTH(SUBSTRING(vTmpStr,79)) GE 10) THEN
            DO:
               oReturn = SUBSTRING(vTmpStr,1,78)                                                          + "~n" 
                       + "──────────────────────────────────────────────────────────────────────────────" + "~n" 
                       + SUBSTRING(vTmpStr,79).
            END.
            ELSE 
            DO:
               oReturn = SUBSTRING(vTmpStr,1,LENGTH(vTmpStr) - 10)                                        + "~n"
                       + "──────────────────────────────────────────────────────────────────────────────" + "~n"
                       + SUBSTRING(vTmpStr,LENGTH(vTmpStr) - 9).
            END.
      END.

      WHEN 1 THEN
      DO:
         oReturn = SUBSTRING(vTmpStr,1,41).
         IF LENGTH(vTmpStr) GT 41 THEN
            oReturn = oReturn + "~n" + FILL(" ",37) + "─────────────────────────────────────────" 
                              + "~n" + FILL(" ",37) + SUBSTRING(vTmpStr,42,41).
         IF LENGTH(vTmpStr) GT 83 THEN
            oReturn = oReturn + "~n" + FILL(" ",37) + "─────────────────────────────────────────"
                              + "~n" + FILL(" ",37) + SUBSTRING(vTmpStr,84,41).
         IF LENGTH(person.issue) GT 125 THEN
            oReturn = oReturn + "~n" + FILL(" ",37) + "─────────────────────────────────────────"
                              + "~n" + FILL(" ",37) + SUBSTRING(vTmpStr,126,41).
      END.

      WHEN 2 THEN
      DO:
         IF iLogTrim 
            THEN oReturn = TrimFormula(chTrim,SUBSTRING(vTmpStr,1,35)). 
            ELSE oReturn = SUBSTRING(vTmpStr,1,35).
      END.
      
      WHEN 21 THEN
      DO:
         IF iLogTrim 
            THEN oReturn = TrimFormula(chTrim,SUBSTRING(vTmpStr,36,35)). 
            ELSE oReturn = SUBSTRING(vTmpStr,36,35).
      END.
      WHEN 3 THEN
         oReturn = vTmpStr.
   
   END CASE.
END PROCEDURE.

PROCEDURE СумВклПроп:
   DEFINE INPUT  PARAMETER iFrml    AS CHARACTER   NO-UNDO. /* название формулы */
   DEFINE OUTPUT PARAMETER oReturn  AS CHARACTER   NO-UNDO.
   
   IF AVAIL loan-acct THEN
   DO:
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
      RUN kau-pos.p(loan-acct.acct,
                    loan-acct.currency,
                    xdate,
                    xdate,
                    rab_str,
                    in_kau).
   
      /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
      IF RETURN-VALUE EQ "Ошибка" THEN
         ASSIGN Xresult = -2.
   
      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal).
   
      ksh-bal = IF loan.currency  EQ ''
                THEN (IF acct.side EQ 'А'
                      THEN ksh-bal
                      ELSE ksh-bal * (-1))
                ELSE (IF acct.side EQ 'А'
                      THEN ksh-val
                      ELSE ksh-val * (-1)).
   
      IF iFrml EQ "сум_вкл2проп" THEN
      DO:
         RUN "x-amtstr.p" (ksh-bal,
                             loan-acct.currency,
                             YES,
                             YES,
                             OUTPUT c1,
                             OUTPUT c2).
         c1 = c1 + " " + c2.
         oReturn = IF logTrim THEN TrimFormula(chTrim,c1) ELSE c1.
      END.
      ELSE
      DO:
         ksh-bal1 = (ksh-bal - TRUNC(ksh-bal,0)) * 100.
   
         IF NOT RetString THEN
            oReturn = TRIM(STRING(TRUNC(ksh-bal,0),'>>>,>>>,>>>,>>9')) + " руб. " + STRING(ksh-bal1,'99') + " коп. ".
         ELSE
            oReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')).
      END.
   END.
END PROCEDURE.

PROCEDURE СумВклПроп2:
   DEFINE INPUT  PARAMETER iFrml    AS CHARACTER   NO-UNDO. /* название формулы */
   DEFINE OUTPUT PARAMETER oReturn  AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE c_date     AS DATE NO-UNDO. /*Дата на которую идет расчет*/
   DEFINE VARIABLE vSinceDate AS DATE NO-UNDO.
   DEFINE BUFFER  xloan-cond FOR loan-cond.
   
   ASSIGN c_date = xdate
          ksh-bal = 0.0 .
   
   /* Ищем сначала условия и на них доп.реквизит */
   IF c_date LT loan.open-date
   THEN c_date = loan.open-date.
   FIND LAST xloan-cond WHERE xloan-cond.contract = loan.contract
      AND xloan-cond.cont-code = loan.cont-code
      AND xloan-cond.since <= c_date
      NO-LOCK NO-ERROR.
   IF NOT AVAIL xloan-cond
   THEN
   FIND FIRST xloan-cond WHERE xloan-cond.contract = loan.contract
     AND xloan-cond.cont-code = loan.cont-code
     AND xloan-cond.since > c_date
     NO-LOCK NO-ERROR.   
  IF AVAIL  xloan-cond THEN DO :
      ksh-bal =  DECIMAL(GetXattrValueEx("loan-cond",
               xloan-cond.contract + "," 
               + xloan-cond.cont-code + "," +
               string(YEAR (xloan-cond.since),"9999") + 
               string(MONTH(xloan-cond.since),"99"  ) +
               string(DAY  (xloan-cond.since),"99"  ),
               "СуммаДог",?)) NO-ERROR.
     IF ERROR-STATUS:ERROR 
     THEN ASSIGN ksh-bal = 0.0 .          
  END.   
  IF NOT {assigned string(ksh-bal)}
  THEN ksh-bal = 0 .           
  IF ksh-bal = 0 AND AVAIL loan-acct THEN
   DO:
     c_date = xdate .
      /* Ищем счет */
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
   
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
   
      ASSIGN vSinceDate = c_date.
   
      /*ищем приход на вклад сразу после привязки счета*/
      FIND FIRST kau-entry WHERE kau-entry.acct = loan-acct.acct
                             AND kau-entry.currency = loan.currency
                             AND kau-entry.op-status >= gop-status
                             AND NOT kau-entry.debit
                             AND kau-entry.op-date >= vSinceDate
                             AND kau-entry.kau = in_kau NO-LOCK NO-ERROR.
      IF AVAILABLE kau-entry THEN
      DO:
         FIND FIRST op OF kau-entry NO-LOCK NO-ERROR.
         IF AVAIL op AND op.contract-date = c_date THEN
            vSinceDate = kau-entry.op-date.
      END.
   
      RUN kau-pos.p (loan-acct.acct,
                     loan-acct.currency,
                     vSinceDate,
                     vSinceDate,
                     rab_str,
                     in_kau).
   
      /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
      IF {&RETURN_VALUE} EQ "Ошибка" THEN
         ASSIGN Xresult = -2.
   
      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal).
   
      ASSIGN
         ksh-bal = ksh-bal * (-1) WHEN loan.currency EQ '' AND acct.side NE 'А'
         ksh-bal = ksh-val * (-1) WHEN loan.currency NE '' AND acct.side NE 'А'
      .
   
  END . 

/*****/   
   
      IF iFrml EQ "сум_вкл2проп" THEN
      DO:
         RUN "x-amtstr.p" (ksh-bal,
                             loan-acct.currency,
                             YES,
                             YES,
                             OUTPUT c1,
                             OUTPUT c2).
         c1 = c1 + " " + c2.
         oReturn = IF logTrim THEN TrimFormula(chTrim,c1) ELSE c1.
      END.
      ELSE
      DO:
         ksh-bal1 = (ksh-bal - TRUNC(ksh-bal,0)) * 100.
   
         IF NOT RetString THEN
            oReturn = TRIM(STRING(TRUNC(ksh-bal,0),'>>>,>>>,>>>,>>9')) + " руб. " + STRING(ksh-bal1,'99') + " коп. ".
         ELSE
            oReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')).
      END.
/*  END. */
END PROCEDURE.

PROCEDURE Сум_Вкл:
   DEFINE INPUT  PARAMETER iFrml    AS CHARACTER   NO-UNDO. /* название формулы */
   DEFINE OUTPUT PARAMETER oReturn  AS CHARACTER   NO-UNDO.
   
   DEFINE VARIABLE c_date     AS DATE NO-UNDO. /*Дата на которую идет расчет*/
   DEFINE VARIABLE vSinceDate AS DATE NO-UNDO.
   
   ASSIGN c_date = xdate.
   IF AVAIL loan-acct THEN
   DO:
      /* Ищем счет */
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
   
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
   
      ASSIGN vSinceDate = c_date.
   
      /*ищем приход на вклад сразу после привязки счета*/
      FIND FIRST kau-entry WHERE kau-entry.acct = loan-acct.acct
                             AND kau-entry.currency = loan.currency
                             AND kau-entry.op-status >= gop-status
                             AND NOT kau-entry.debit
                             AND kau-entry.op-date >= vSinceDate
                             AND kau-entry.kau = in_kau NO-LOCK NO-ERROR.
      IF AVAILABLE kau-entry THEN
      DO:
         FIND FIRST op OF kau-entry NO-LOCK NO-ERROR.
         IF AVAIL op AND op.contract-date = c_date THEN
            vSinceDate = kau-entry.op-date.
      END.
   
      RUN kau-pos.p (loan-acct.acct,
                     loan-acct.currency,
                     vSinceDate,
                     vSinceDate,
                     rab_str,
                     in_kau).
   
      /*если печатаем по вкладу с нулевой суммой, то возникает ошибка, ее надо убрать*/
      IF RETURN-VALUE EQ "Ошибка" THEN
         ASSIGN Xresult = -2.
   
      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal).
   
      ASSIGN
         ksh-bal = ksh-bal * (-1) WHEN loan.currency EQ '' AND acct.side NE 'А'
         ksh-bal = ksh-val * (-1) WHEN loan.currency NE '' AND acct.side NE 'А'
      .
   
      RUN "x-amtstr.p" (ksh-bal,
                        loan-acct.currency,
                        YES,
                        YES,
                        OUTPUT c1,
                        OUTPUT c2).
      c1 = c1 + " " + c2.
   
      IF iFrml EQ "сум_вкл" THEN
         oReturn = STRING(ksh-bal,'>>>,>>>,>>>,>>9.99') + "~n"
                 + FILL(" ",8) +  "──────────────────────────────────────────────────────────────────────"
                 + "~n" + c1 .
      ELSE
      DO:
         ksh-bal1 = (ksh-bal - TRUNC(ksh-bal,0)) * 100.
         oReturn = TRIM(STRING(TRUNC(ksh-bal,0),'>>>,>>>,>>>,>>9')) + " руб. " + STRING(ksh-bal1,'99') + " коп. " + "~n"
                 + FILL(" ",8) + "──────────────────────────────────────────────────────────────────────"
                 + "~n" + c1.
      END.
   END.
END PROCEDURE.

PROCEDURE GetAdrCode.
   DEFINE INPUT  PARAMETER iCliSurr AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAdrCode AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oAdr     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vTmpStr AS CHARACTER   NO-UNDO.   
   
   DEF BUFFER cust-ident FOR cust-ident.

   FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-adr"
                           AND cust-ident.cust-code-type EQ iAdrCode
                           AND cust-ident.cust-cat       EQ ENTRY(1, iCliSurr)
                           AND cust-ident.cust-id        EQ INT64(ENTRY(2, iCliSurr))
                           AND (   cust-ident.close-date EQ ?
                                OR cust-ident.close-date GE xDate)
      NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
   DO:
      vTmpStr = GetAdrStr(cust-ident.issue, "индекс").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "район").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "город").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "пункт").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "улица").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "дом").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "строение").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "корпус").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "квартира").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
   END.      

END PROCEDURE.

PROCEDURE GetPersType.
   DEFINE INPUT  PARAMETER iLoanRID    AS RECID       NO-UNDO.   
   DEFINE INPUT  PARAMETER iPersType   AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oPersSurr   AS CHARACTER   NO-UNDO.

   DEF BUFFER loan      FOR loan.
   DEF BUFFER cust-role FOR cust-role.   

   FOR FIRST loan WHERE RECID(loan) EQ iLoanRID
      NO-LOCK,
       FIRST cust-role WHERE cust-role.file-name  EQ "loan"
                         AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
                         AND cust-role.class-code EQ iPersType
                         AND cust-role.cust-cat   EQ "Ч"
      NO-LOCK:

      oPersSurr = "Ч," + cust-role.cust-id.
   END.


END PROCEDURE. 

PROCEDURE GetDocSotr.
   DEFINE INPUT  PARAMETER iCommand  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oReturn   AS CHARACTER   NO-UNDO.
   
   DEF VAR vCode    AS CHAR   NO-UNDO.
   DEF VAR vBegDate AS DATE   NO-UNDO.
   DEF VAR vEndDate AS DATE   NO-UNDO.
   DEF VAR vHandl   AS HANDLE NO-UNDO.
   DEF VAR vCheck   AS LOGIC  INIT  YES  NO-UNDO.
   
   DEF BUFFER b1_code FOR code.

   vHandl = session:last-procedure.
   DO WHILE VALID-HANDLE(vHandl):
      IF vHandl:file-name EQ 'l-sotr.p' THEN
         LEAVE.
      vHandl = vHandl:prev-sibling .
   END.

   IF VALID-HANDLE(vHandl) THEN
      vCode = vHandl:private-data.
   MAIN:
   DO 
      ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:
     
      IF NOT ({assigned vCode}) THEN
      DO TRANSACTION: 
         pick-value = "".
         RUN browseld.p ('Докум_сотр',
            'class' + CHR(1) + 'parent' + CHR(1) + 'misc[1]',
            'Докум_сотр' + CHR(1) + 'Докум_сотр'
             + CHR(1) + GetXAttrValue("_user",USERID('bisquit'),"Отделение"),
            '', 4).
         IF {assigned pick-value} THEN
            vCode = pick-value.
         ELSE DO:
            IF VALID-HANDLE(vHandl) THEN
            vHandl:private-data = "esc".
            oReturn = "esc".
         END.
      END.
      ELSE  /* сохраненное значение контролировать не будем */
            vCheck = NO.                                

      IF {assigned vCode} THEN
      DO:
         FIND FIRST _User WHERE _User._userid EQ vCode
         NO-LOCK NO-ERROR.
         IF AVAIL _User THEN
         DO:
            FIND FIRST b1_code WHERE b1_code.class  EQ "Докум_сотр"
                                 AND b1_code.parent EQ "Докум_сотр"
                                 AND b1_code.code   EQ vCode
               NO-LOCK NO-ERROR.
            IF AVAIL b1_code THEN
            DO:
               ASSIGN
                  vBegDate = DATE(b1_code.misc[5])
                  vEndDate = DATE(b1_code.misc[6])
                  NO-ERROR.

               IF vCheck THEN
               DO:  
                  IF vBegDate GT gend-date THEN
                  DO:
                     RUN Fill-SysMes IN h_tmess ("","",4,
                        "Не наступил срок полномочий.~nВыбрать другое уполномоченное лицо?" ).
                     IF pick-value EQ "yes" THEN
                     DO:
                        vCode = "".
                        UNDO MAIN, RETRY MAIN.
                     END.   
                  END.
                  IF vEndDate LT gend-date THEN
                  DO:
                     RUN Fill-SysMes IN h_tmess ("","",4,
                        "Истек срок полномочий.~nВыбрать другое уполномоченное лицо?" ).
                     IF pick-value EQ "yes" THEN
                     DO:
                        vCode = "".
                        UNDO MAIN, RETRY MAIN.
                     END.   
                  END.
               END.

               IF VALID-HANDLE(vHandl) THEN
                  vHandl:private-data = vCode.

               CASE iCommand:
                  WHEN "УполнЛицо" THEN
                     oReturn = b1_code.name.
                  WHEN "УполнЛицоРП" THEN
                     oReturn = b1_code.description[3].
                  WHEN "ДолжнУполнЛица" THEN
                     oReturn = b1_code.misc[3].
                  WHEN "НомДовУполнЛица" THEN
                     oReturn = b1_code.val.
                  WHEN "ДатаДовУполнЛица" THEN
                     oReturn = STRING(vBegDate,"99/99/9999").
                  WHEN "ОсновУполнЛица" THEN
                     oReturn = b1_code.misc[4].
                  WHEN "ДолжнУполнЛицаРП" THEN
                     oReturn = b1_code.misc[7].
               END CASE.
            END.
         END.
      END.
   END.        
END PROCEDURE.
{intrface.del}
