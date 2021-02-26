/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: PP-SACCT-API-BS.I
      Comment: Библиотека процедур API службы "Лицевые счета" - BS (бизнес-логика)
   Parameters:
         Uses:
      Used by:
      Created: 18.10.2004 16:22 MDY из pp-acct.p      
     Modified: 25.11.2004 12:55 KSV      (0038814) Добавлена симметричная
                                         процедура AcctFree, осовбождающая
                                         номер зарезервированного счета с
                                         помощью процедуры AcctKeep.
     Modified: 07.12.2004 11:45 KSV      (0038814) Процедуры валидации счета
                                         перенесены из pp-acct.p
     Modified: 25.01.2005 13:39 KSV      (0038814) Синхронизировано с 4.1D
                                         Costing.
     Modified: 26.01.2005 17:31 KSV      (0038814) Исправлена процедура
                                         Check-Acct-Mask для работы с масками,
                                         заданными в классификаторе.
     Modified: 11.02.2005 12:50 MDY      0042688 - исправлены гл.ошибки - 2 штуки
     Modified: 15.02.2005 13:20 KSV      (0042889) Исправлена ошибка в
                                         Check-Acct-Mask для иерархического
                                         классификатора с масками счетов.
     Modified: 18.02.2005 10:39 KSV      (0043089) Проверка счета на
                                         соответствие маске исключена из
                                         Check-Acct.
     Modified: 04.03.2005 Om  Ошибка.
                        Для инициализации переменных otdel.i необходимо наличие
                        зарегистрированного пользователя в системе.
     Modified: 14.05.2005 Om  Доработка.
                        Добавлена процедура GetNameAcctExCorp - для счетов юр. лиц
                        формирует наименование из поля cust-corp.name-short.
     Modified: 14.05.2005 SADM  Ошибка.
                        Процедура Check-Acct-Key , изменен ответ по-умолчанию
                        на неверный ключ - с "оставить" на "не оставлять".
                        Добавлена обработка типа сообщения "Ошибка" для
                        возможности запрещения ввода счета с неверным ключем.
     Modified: 25.07.2005 16:16 MDY      37923
     Modified: 10.08.2005 15:04 KSV      (0037923) Добавлена инициализация
                                         реквизита acct.kau-id из метасхемы.
     Modified: 19/01/99 Serge процедура GetAcctMask вызывается из Acct-Cr,
                              если xattrid не найден, то вызывается XattrAll.
     Modified: 12/02/99 Lera  ошибку при создании номера счета можно получить в данной процедуре и отправить в вызывающую, а там
                              ее обработать. Для этого надо указать &ONerror - имя переменной кода ошибки (input-output параметр).
                              Return-value при этом "error".
     Modified: 17/01/2003 kraw (13482) увеличение длины массива cnt-srt
     Modified: 03.03.2003 17:55 KSV      (0012920) Добавлена дополнительная
                                         параметризация метода выделения счетчика
                                         через механизм SysConf.
                                         Добавлен мехнаизм, позволяющий
                                         использовать в открываемом счете
                                         определенное значение счетчика. См
                                         процедуры Acct-CR, SetDefinedCntParam и
                                         GetDefinedCounter.
                                         Подключен механизм инициализации доп.
                                         реквизитов. См. инклюдник BAL2ACCT.I и
                                         процедуру Acct-CR
                                         Добавлена параметризация констант,
                                         входящих в маску счета. См. инклюдник
                                         ACCTTOK.DEF и процедуру Acct-CR.
                                         Использование метода XATTRALL для
                                         инициализации реквизитов создаваемого
                                         счета заменено на GetXAttrInit. См.
                                         процедуры Acct-Cr и GetAcctMask.
     Modified: 04.03.2003 14:03 KSV      (0012920) В параметры метода счетчика
                                         добален номер итерации открытия счета,
                                         который может влиять на получаемый
                                         счетчик.
     Modified: 04.03.2003 15:45 KSV      (0012920) Добавлено препроцессорное
                                         выключение вызова процедуры
                                         BalToAcct_Xattr. См. процедуру
                                         Acct-CR.
     Modified: 16.04.2003 11:39 KSV      (0012920) В процедуру Acct-Cr в
                                         обработчик токенов TOK_USR, добавлена
                                         передача параметра со сформированным
                                         номером счета.
     Modified: 16.04.2003 15:47 KSV      (0012920) В процедуру Acct-Cr
                                         добавлена возмножность нумерации
                                         счетов с произвольного номера. Номер,
                                         с которого начинается нумерация
                                         задается на реквизите класса счета
                                         МинСчетчик.
     Modified: 21.06.2003       AVAL     (0013453) В процедуре Acct-Cr добавлена
                                         обработка токена "д" для статьи
                                         Доходов/Расходов.
     Modified: 02.06.2005       kraw (0047326)   branch-list = otdel-list в SetOtdelVariables
     Modified: 10.08.2006 Om  Доработка.
                        1. В процедуру Check-Acct добавлен вызов метода CHKUPD.
                        2. Изменена декларация части переменных. Увы сделаны SHARED.
                           Причина в реализации методов CHKUPD.
     Modified: 29.10.2007 muta 0082120  Добавлена процедура GetAcctQty - аналог GetAcctCur.
                               Ищет остаток по acct-qty (остаток находится в поле qty)
     Modified: 05.04.2011 Malik 0148047  
                                1. В процедуре CreateAcctNumber  указание STRING(key,"9") 
                                   заменено на STRING(key, FILL ("9", toklen[{&TOK_KEY_IDX}])).
                                2. В процедуре Check-Acct строка  
                                  SUBSTRING(STRING(acct.acct),1,5) 
                                  заменена на 
                                  SUBSTRING(STRING(acct.acct),INDEX(tokacct,{&TOK_BAL}),toklen[{&TOK_BAL_IDX}])
                                  Там же строка SUBSTRING(acct.acct,6,3) NE acct.currency
                                  заменена на 
                                  SUBSTRING(acct.acct,INDEX(tokacct,{&TOK_CUR}),toklen[{&TOK_CUR_IDX}]) NE acct.currency        
                                3. Добавлена процедура FillAcctMask для использования в Check-Acct-Key
     Modified: 29/07/2011 Mixey: (0152425)  1. Исправил процедуру FillAcctMask: 
                                            Добавил заполнение маски счета для классификатора 'МаскиСчетов'.
                                            2. Исправил процедуру Check-acct:  
                                            Убрал проверку для классификатора 'МаскиСчетов'.
                                            Проверяем балансовый счет в соответствии с маской при любом раскладе.
     Modified: 31.10.2012 Malik (0183446) В процедуре Check-Acct-Key добавлено условие, для исключения таких ошибок:
                                ** Starting position for SUBSTRING, OVERLAY, etc. must be 1 or greater.                                            
                                
*/

{accttok.def}           /* Объявления констант используемых в маске. */
{get_dob.i}             /* Дата последнего закрытого ОД. */
{workplan.def} 

PROCEDURE SetOtdelVariables PRIVATE.
                        /* Т.к. поле ShFilial определяется только после
                        ** логина, то приходится вновь искать */
   dob = fGetLastClsDate (?,"*").
                        /* Если дата последнего закрытого ОД не определена,
                        ** то инициализируем текущей датой.
                        ** (TODAY - 1) необходим, т.к. nextdob.i безусловно
                        ** прибавляет один день вперед. */
   IF dob =  ?
      THEN dob = (TODAY - 1).
                        /* Поиск вперед первого рабочего ОД
                        ** от даты dob. */
   {nextdob.i}

   RUN ACGetKeyProg(vclass, OUTPUT keyprog).
   ASSIGN
      no-key      = FGetSetting ( "КлючаНет", ?, "" )
      my-key      = FGetSetting ( "Ключ", ?, "" )
      local-cur   = FGetSetting ( "КодНацВал", ?, "{&in-NC-Code}" )
      depobal     = (FGetSetting("depo-bal",?,?) = "Да").
   .

   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Получить из метасхемы метод, вычисляющий ключ
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE ACGetKeyProg:
   DEFINE INPUT  PARAMETER vclass  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER keyprog AS CHARACTER  NO-UNDO.
   keyprog = GET-CLASS-METHOD(vclass, "U1").
   if keyprog = ? then keyprog = "key-tst".
END PROCEDURE.

PROCEDURE Acct-Cr.
   DEF INPUT         PARAM       in-bal-acct LIKE acct.bal-acct   NO-UNDO.
   DEF INPUT         PARAM       in-currency LIKE acct.currency   NO-UNDO.
   DEF INPUT-OUTPUT  PARAMETER   vAcct       LIKE acct.acct       NO-UNDO.
   DEFINE PARAMETER BUFFER acct FOR acct.

   DEFINE VARIABLE ac      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRet    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrMsg AS CHARACTER   NO-UNDO.

   RUN CreateAcctNumber (vClass,
                         in-bal-acct,
                         in-currency,
                         vAcct-Cat,
                         acctmask,
                         acct.branch-id,
                         acct.cust-cat,
                         acct.cust-id,
                         mSymbPU,
                         acct.acct,
                         OUTPUT ac,
                         OUTPUT vErrMsg).

   vAcct = ac.
   vRet = RETURN-VALUE.
   IF vErrMsg <> "" THEN
      RUN Fill-SysMes IN h_tmess (ENTRY(1,vErrMsg,"|"),
                                  IF NUM-ENTRIES(vErrMsg, "|") >= 2
                                    THEN ENTRY(2,vErrMsg,"|")
                                    ELSE "",
                                  IF NUM-ENTRIES(vErrMsg, "|") >= 3
                                    THEN ENTRY(3,vErrMsg,"|")
                                    ELSE "-1",
                                  IF NUM-ENTRIES(vErrMsg, "|") >= 4
                                    THEN ENTRY(4,vErrMsg,"|")
                                    ELSE "Ошибка генерации номера счета").
   RETURN vRet.
END PROCEDURE.

/*-------Commented by Malik----------------------------------------------------
  Purpose:     Процедура заполнения переменных по маске счета
  Parameters:  iMask - маска
               
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE FillAcctMask:
   DEFINE INPUT  PARAMETER iMask     AS CHARACTER   NO-UNDO.

   DEF VAR  sTokidx     AS CHAR    NO-UNDO.
   DEF VAR  sToklen     AS CHAR    NO-UNDO.
   DEF VAR i            AS INT64     NO-UNDO.
   /* Commented by Mixey:  29/07/2011 (0152425) Добавил заполнение маски счета
                                                для классификатора 'МаскиСчетов' */ 
   IF CAN-FIND(FIRST code WHERE code.class =  "" AND code.code =  iMask) THEN iMask = acctmask.
   RUN GetAcctMask (INPUT iMask, OUTPUT tokacct,
                    OUTPUT sTokidx, OUTPUT sToklen) NO-ERROR.      
   IF ERROR-STATUS:ERROR THEN DO:
      RETURN.
   END. 

   DO i = 1 TO EXTENT(tokidx):
       tokidx[i] = INT64(ENTRY(i,sTokidx)).
       IF i <= EXTENT(toklen) THEN DO:
           toklen[i]= INT64(ENTRY(i,sToklen)).
       END.
   END.
END PROCEDURE.

/*
   Процедура предназначена для подсчёта количества свободных значений счётчика
   при открытии счёта. Параметры:
    - iAcct     - фактический номер, который получил счёт после подстановки всех
                  символов маски;
    - iCurrency - код валюты;
    - iAcctCat  - категория учёта;
    - iBalAcct  - счёт 2-го порядка;
    - iClass    - класс открываемого счёта на метасхеме;
    - iMask     - исходная маска, с которой открывался счёт.
   Занятыми считаются те значения счётчика, которые либо уже задействованы в
   существующих счетах с такими реквизитами, либо использованы в записях
   классификатора СчетаРезерва для счетов, открытых по данной маске.
   Корректность ключевого разряда в записях классификатора СчетаРезерва не
   проверяется по соображениям производительности.
   Возвращается число свободных значений счётчика (0, если свободных нет), либо
   -1 в случае ошибки в параметрах.
*/
PROCEDURE CalcNumFreeCounters PRIVATE:
   DEFINE INPUT  PARAMETER iAcct     LIKE acct.acct       NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency LIKE acct.currency   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat  LIKE acct.acct-cat   NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct  LIKE acct.bal-acct   NO-UNDO.
   DEFINE INPUT  PARAMETER iClass    LIKE acct.class-code NO-UNDO.
   DEFINE INPUT  PARAMETER iMask     AS   CHARACTER       NO-UNDO.
   DEFINE OUTPUT PARAMETER oNumFree  AS   INT64           NO-UNDO INITIAL -1.

   DEFINE BUFFER acct FOR acct.
   DEFINE BUFFER code FOR code.

   DEFINE VARIABLE vI     AS INT64     NO-UNDO.
   DEFINE VARIABLE vN     AS INT64     NO-UNDO INITIAL 1.
   DEFINE VARIABLE vIsCnt AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vC     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vS     AS CHARACTER NO-UNDO.

   DO vI = MINIMUM(LENGTH(iAcct), LENGTH(iMask)) TO 1 BY -1:
      ASSIGN
         vC     = SUBSTRING(iMask, vI, 1)
         vIsCnt = (vC = {&TOK_CNT})
         vN     = vN * 10
                  WHEN vIsCnt
         vS     = SUBSTITUTE("&1&2",
                             IF vIsCnt OR vC = {&TOK_KEY}
                             THEN "."
                             ELSE SUBSTRING(iAcct, vI, 1),
                             vS)
      .
   END.
   FOR EACH acct WHERE
      acct.class-code = iClass    AND
      acct.currency   = iCurrency AND
      acct.acct-cat   = iAcctCat  AND
      acct.bal-acct   = iBalAcct  AND
      CAN-DO(vS, acct.acct)
   NO-LOCK:
      ACCUMULATE acct.acct (COUNT).
   END.
   FOR EACH code WHERE
      code.class  = "СчетаРезерва" AND
      code.parent = "СчетаРезерва" AND
      CAN-DO(vS, code.code)
   NO-LOCK:
      ACCUMULATE code.code (COUNT).
   END.
   oNumFree = vN - (ACCUM COUNT acct.acct) - (ACCUM COUNT code.code).
END PROCEDURE.

PROCEDURE CreateAcctNumber.
   DEFINE INPUT  PARAMETER iClass    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBal      AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr     AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat  AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iMask     AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBranchId AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCustCat  AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId   AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iSymPU    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct     AS CHARACTER   NO-UNDO.

   DEFINE OUTPUT PARAMETER oAcct     AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrMsg   AS CHARACTER   NO-UNDO.

   DEF BUFFER xac FOR acct.

   DEF VAR ac           LIKE acct.acct NO-UNDO.
   DEF VAR jj           AS INT64     NO-UNDO.
   DEF VAR i            AS INT64     NO-UNDO.
   DEF VAR j            AS INT64     NO-UNDO.
   DEF VAR fnd-acct     AS CHAR    NO-UNDO INITIAL ?.
   DEF VAR cnt-st       AS CHAR    NO-UNDO.
   DEF VAR ch           AS CHAR    NO-UNDO.
   DEF VAR cnt-srt      AS INT64     NO-UNDO EXTENT 11.
   DEF VAR vMinCnt      AS INT64     NO-UNDO.
   DEF VAR U2-proc      AS CHAR    NO-UNDO INITIAL ?.
   DEF VAR U3-proc      AS CHAR    NO-UNDO INITIAL ?.
   DEF VAR vIntBranch   AS INT64     NO-UNDO.
   DEF VAR  sTokidx     AS CHAR    NO-UNDO.
   DEF VAR  sToklen     AS CHAR    NO-UNDO.
   DEF VAR tmp-parr     AS CHAR    NO-UNDO.
   DEF VAR vFullacct    AS CHAR    NO-UNDO.
   DEF VAR cnstacct     AS CHAR    NO-UNDO.
   DEF VAR vCntAcct     AS CHAR    NO-UNDO. /* Порядковый номер счетчика в полученном счете. */
   DEF VAR vPosAcct     AS INT64     NO-UNDO. /* Позиция не цифры в счетчике. */
   DEF VAR vFlagUnk     AS LOGICAL NO-UNDO.
   DEF VAR vCnstIgnor   AS CHAR    NO-UNDO.
   DEF VAR vNumFreeCnt  AS INT64   NO-UNDO INITIAL ?.
/* Вставка Плюс банк */
   DEF VAR mHandle      AS INT64   NO-UNDO.
   DEF VAR mac11        AS CHAR    NO-UNDO.
   DEFINE VARIABLE iStat    AS INT64     NO-UNDO.
/* Конец вставки Плюс банк */

   ASSIGN
      tmp-parr = GetSysConf ("AcctONcreate-acct")
      U2-proc  = GET-CLASS-METHOD(iClass, "U2")
   .
   IF       U2-proc <> ?
      AND   NOT SearchPfile (U2-proc)
   THEN DO:
      oErrMsg = "|acct04||%s=" + U2-proc.
      RETURN.
   END.
   U3-proc = GET-CLASS-METHOD(iClass, "U3").
   IF U3-proc <> ?
      AND NOT SearchPfile (U3-proc)
   THEN DO:
      oErrMsg = "|acct05||%s=" + U3-proc.
      RETURN.
   END.

   RUN GetAcctMask (INPUT iMask, OUTPUT tokacct,
                    OUTPUT sTokidx, OUTPUT sToklen) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      oErrMsg = "||-1|" + GetErrMsg().
      RETURN.
   END.

   DO i = 1 TO EXTENT(tokidx):
       tokidx[i] = INT64(ENTRY(i,sTokidx)).
       IF i <= EXTENT(toklen) THEN DO:
           toklen[i]= INT64(ENTRY(i,sToklen)).
       END.
   END.
   ac = FILL("0", LENGTH(tokacct)).
   DO i = 1 TO LENGTH(tokacct):
      ch = SUBSTR(tokacct,i,1).
      IF  ch >= "0"
      AND ch <= "9"
         THEN OVERLAY(ac,i,1) = ch.
   END.

   FIND bal-acct WHERE bal-acct.bal-acct = iBal NO-LOCK NO-ERROR.
   IF NOT AVAIL bal-acct THEN DO:
      oErrMsg = "||0|" + ERROR-STATUS:GET-MESSAGE(1).
      RETURN.
   END.
   iCurr = IF iCurr = ?
           THEN (IF bal-acct.foreign
                 THEN ?
                 ELSE "")
           ELSE IF  iCurr     = local-cur
                AND iAcctCat <> "d"
                THEN ""
                ELSE iCurr.
   jj = 0.

   /* Commented by KSV: Если на классе задан минимально возможный счетчик,
   ** то начинаем нумерацию с него. В ДБ счета нумеруются с 1  */
   vMinCnt = INT64(GetXAttrInit(iClass,"МинСчетчик")) NO-ERROR.
   IF vMinCnt = ? THEN vMinCnt = 0.

   vCnstIgnor = {&TOK_CNT} + "," + {&TOK_KEY} + "," + {&TOK_ANY}.

   NEXTACCT:
   REPEAT
   ON ERROR  UNDO NEXTACCT, LEAVE NEXTACCT
   ON ENDKEY UNDO NEXTACCT, LEAVE NEXTACCT:
      /* счет 2-го порядка */
      IF toklen[{&TOK_BAL_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_BAL},
                           STRING(iBal,FILL("9",MAXIMUM(toklen[{&TOK_BAL_IDX}],LENGTH(STRING(iBal)))))).

      /* валюта */
      IF toklen[{&TOK_CUR_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_CUR},
                           IF  iCurr <> ""
                           AND iCurr <> local-cur
                           AND iCurr <> ?
                              THEN iCurr
                              ELSE local-cur).

      /* филиал */
      IF toklen[{&TOK_BRANCH_IDX}] > 0 THEN DO:
         cnt-st = iBranchId.
         IF cnt-st = ""
         OR cnt-st = ?
            THEN cnt-st = GetThisUserOtdel().
         vIntBranch = INT64(cnt-st) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            oErrMsg = "|acct06||".
            RETURN "exit".
         END.
         RUN SetAcct (INPUT-OUTPUT ac, {&TOK_BRANCH},
                      STRING(vIntBranch,FILL("9",MAXIMUM(toklen[{&TOK_BRANCH_IDX}],LENGTH(cnt-st))))).
      END.

      /* код клиента */
      IF toklen[{&TOK_CLIENT_IDX}] > 0 THEN DO:
         IF iCustCat <> "В"
            THEN cnt-st = STRING(iCustId).
            ELSE cnt-st = "".
         IF  cnt-st <> ""
         AND cnt-st <> ?
         AND cnt-st <> "0"
            THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_CLIENT}, cnt-st).
      END.

      /* УНК */
      IF toklen[{&TOK_UNKCLI_IDX}] > 0 THEN DO:
         IF iCustCat <> "В" THEN DO:
            {getflagunk.i &only-np=YES &flag-unk="vFlagUnk"}
            IF vFlagUnk = NO THEN DO:
               oErrMsg = "|acct44||".
               RETURN "EXIT".
            END.
            cnt-st = GetUNK(iCustCat, iCustId).
         END.
         ELSE cnt-st = "".
         IF  cnt-st <> ""
         AND cnt-st <> ?
         AND cnt-st <> "0"
            THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_UNKCLI}, cnt-st).
      END.

      /* любой символ */
      IF toklen[{&TOK_ANY_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_ANY}, FILL(" ",toklen[{&TOK_ANY_IDX}])).

      /* статья доходов */
      IF toklen[{&TOK_PU_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_PU}, FILL(iSymPU,toklen[{&TOK_PU_IDX}])).
  
      /* счетчик */
      IF toklen[{&TOK_CNT_IDX}] > 0 THEN DO:
         /* Проверяем, есть ли определенный счетчик,
         ** и можно ли его использовать. */
         RUN GetDefinedCounter(iBal, iCurr, ac, tokacct, OUTPUT fnd-acct).
         /* Если произошла "управляемая" ошибка,
         ** то прекращаем открытие счета. */
         IF RETURN-VALUE = "ERROR"
            THEN RETURN "EXIT".
         /* Если счетчик не определен, то... */
         IF fnd-acct = ? THEN DO:
            cnt-st = "".
            IF U2-proc <> ? THEN DO:
               tmp-branch-id = iBranchId.
               /* Параметризуем метод получения счетчика  */
               RUN SetSysConf IN h_base ("AcctAc",ac).
               RUN SetSysConf IN h_base ("AcctCustCat",iCustCat).
               RUN SetSysConf IN h_base ("AcctCustID",STRING(iCustId)).
               RUN SetSysConf IN h_base ("AcctMsk",iMask).
               RUN SetSysConf IN h_base ("AcctInc",STRING(jj)).
               /* Выбор метода по входным параметрам. */
               IF  tmp-parr <> ""
               AND tmp-parr <> ?
                  THEN RUN VALUE (U2-proc + ".p") (tmp-parr,        OUTPUT fnd-acct).
                  ELSE RUN VALUE (U2-proc + ".p") (iBal, iCurr, "", OUTPUT fnd-acct).
               /* Сбрасываем параметры счетчика */
               RUN DeleteOldDataProtocol IN h_base ("AcctAc").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustCat").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustID").
               RUN DeleteOldDataProtocol IN h_base ("AcctMsk").
               RUN DeleteOldDataProtocol IN h_base ("AcctInc").
               /* Если счетчик не определен и указан метод U3,
               ** то пробуем его запустить. */
               RUN SetSysConf IN h_base ("AcctAc",ac).
               RUN SetSysConf IN h_base ("AcctCustCat",iCustCat).
               RUN SetSysConf IN h_base ("AcctCustID",STRING(iCustId)).
               RUN SetSysConf IN h_base ("AcctMsk",iMask).
               RUN SetSysConf IN h_base ("AcctInc",STRING(jj)).
               IF  fnd-acct  = ?
               AND U3-proc  <> ? THEN DO:
                  RUN VALUE (U3-proc + ".p") (iBal, iCurr, iClass).
                  RUN VALUE (U2-proc + ".p") (iBal, iCurr, "", OUTPUT fnd-acct).
               END.
               RUN DeleteOldDataProtocol IN h_base ("AcctAc").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustCat").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustID").
               RUN DeleteOldDataProtocol IN h_base ("AcctMsk").
               RUN DeleteOldDataProtocol IN h_base ("AcctInc").
               /* Если счетчик не определен, то выходим с ошибкой. */
               IF fnd-acct = ? THEN DO:
                  oErrMsg = "|acct07||".

                  dvptoolhelper:WriteDebug('Ошибка генерации номера счета!', -1).
                  dvptoolhelper:WriteDebug('Код ошибки: acct07', -1).
                  dvptoolhelper:WriteDebug('Класс счета: ' + iClass, -1).
                  dvptoolhelper:WriteDebug('Маска счета: ' + iMask, -1).
                  dvptoolhelper:WriteDebug('Метод U2: ' + U2-proc, -1).
                  dvptoolhelper:WriteDebug('Метод U3: ' + U3-proc, -1).
                  dvptoolhelper:WriteDebug('Прототип счета: ' + ac, -1).

                  RETURN "EXIT".
               END.
            END.
            ELSE DO:
               ASSIGN
                  cnstacct = ac
                  j        = 0
                  cnt-srt  = 100
               .
               DO i = 1 TO LENGTH(ac):
                  IF LOOKUP(SUBSTR(tokacct,i,1), vCnstIgnor) > 0
                     THEN OVERLAY(cnstacct,i,1) = ".".
                  IF SUBSTR(tokacct,i,1) = {&TOK_CNT}
                     THEN ASSIGN
                        j           = j + 1
                        cnt-srt[j]  = i
                     .
               END.

               vfullacct = AddFilToAcct(iAcct,shFilial).
               FOR EACH xac WHERE xac.bal-acct = iBal
                              AND xac.currency = iCurr
                              AND xac.acct    <> ?
                              AND xac.acct    <> vFullAcct
                              AND xac.filial-id =  shFilial
                              AND CAN-DO(cnstacct, xac.number)
                  NO-LOCK
                  BY SUBSTR (xac.acct,cnt-srt[1],1) +
                     SUBSTR (xac.acct,cnt-srt[2],1) +
                     SUBSTR (xac.acct,cnt-srt[3],1) +
                     SUBSTR (xac.acct,cnt-srt[4],1) +
                     SUBSTR (xac.acct,cnt-srt[5],1) +
                     SUBSTR (xac.acct,cnt-srt[6],1) +
                     SUBSTR (xac.acct,cnt-srt[7],1) +
                     SUBSTR (xac.acct,cnt-srt[8],1) +
                     SUBSTR (xac.acct,cnt-srt[9],1) +
                     SUBSTR (xac.acct,cnt-srt[10],1) +
                     SUBSTR (xac.acct,cnt-srt[11],1)
                  DESCENDING:
                  LEAVE.
               END.
            END.
         END. /* fnd-acct = ? */

         q = DECIMAL(fnd-acct) NO-ERROR.
         IF fnd-acct <> ?
            THEN cnt-st = (IF ERROR-STATUS:ERROR
                           OR toklen[{&TOK_CNT_IDX}] < LENGTH(fnd-acct)
                              THEN fnd-acct
                              ELSE STRING(IF q = 0
                                             THEN MAXIMUM(vMinCnt,q)
                                             ELSE q,
                                          FILL("9",toklen[{&TOK_CNT_IDX}]))
                          ) NO-ERROR.
         ELSE DO:
            IF AVAIL xac THEN
            DO:
               vCntAcct = "".  
               /* Выделяем из счета позиции относящиеся к счетчику. */
               DO i=1 TO LENGTH(xac.number):
                   IF SUBSTRING(tokacct,i,1) =  {&TOK_CNT} THEN
                      vCntAcct = vCntAcct + SUBSTRING(xac.number,i,1). 
               END. 
               /* Пытаемся преобразовать к числу. */
               DECIMAL (vCntAcct) NO-ERROR.
               /* В счетчике есть символы отличные от цифр.
               ** Заменяем символы на цифру "0". */
               IF ERROR-STATUS:ERROR THEN
               DO vPosAcct = 1 TO LENGTH(vCntAcct):
                  IF SUBSTR(vCntAcct, vPosAcct, 1) < "0"
                  OR SUBSTR(vCntAcct, vPosAcct, 1) > "9"
                     THEN SUBSTR(vCntAcct, vPosAcct, 1) = "0".
               END.
            END.
            /* Получаем новое значение счетчика. */
            cnt-st = STRING((IF AVAIL xac
                                THEN MAXIMUM(DECIMAL(vCntAcct) + 1,vMinCnt)
                                ELSE vMinCnt
                            ) + jj, FILL ("9", toklen[{&TOK_CNT_IDX}])
                           ) NO-ERROR.
         END.
                        /* Если счетчик не присвоился то откатываем создание счета. */
         IF ERROR-STATUS:GET-MESSAGE(1) > "" THEN DO:
            oErrMsg = "|acct07||".

            dvptoolhelper:WriteDebug('Ошибка генерации номера счета!', -1).
            dvptoolhelper:WriteDebug('Код ошибки: acct07', -1).
            dvptoolhelper:WriteDebug('Класс счета: ' + iClass, -1).
            dvptoolhelper:WriteDebug('Маска счета: ' + iMask, -1).
            dvptoolhelper:WriteDebug('Метод U2: ' + U2-proc, -1).
            dvptoolhelper:WriteDebug('Метод U3: ' + U3-proc, -1).
            dvptoolhelper:WriteDebug('Прототип счета: ' + ac, -1).

            RETURN "EXIT".
         END.

         RUN SetAcct (INPUT-OUTPUT ac, {&TOK_CNT}, cnt-st).
      END.

      /* ключ */
      IF  toklen[{&TOK_KEY_IDX}] <> 0
      AND my-key <> ?
      AND NOT CAN-DO(no-key, STRING(bal-acct.bal-acct)) THEN DO:
         IF NOT SearchPfile (keyprog) THEN DO:
            oErrMsg = "|acct08||%s=" + keyprog.
            RETURN.
         END.
         RUN VALUE(keyprog + ".p") (ac, my-key, output key).
         IF key <> ?
            THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_KEY}, STRING(key, FILL ("9", toklen[{&TOK_KEY_IDX}]))).
      END.

      /* символ полученный из процедуры формирования номера лицевого счета */
      IF toklen[{&TOK_USR_IDX}] > 0 THEN DO:
         IF U2-proc <> ? THEN DO:
            /* Commented by KSV: Передаем в процедуру номерации счета
            ** номер уже сформированного счета */
            RUN SetSysConf IN h_base ("AcctNumber",ac).

            IF  tmp-parr <> ""
            AND tmp-parr <> ?
               THEN RUN VALUE(U2-proc + ".p") (tmp-parr, OUTPUT fnd-acct).
               ELSE RUN Value(U2-proc + ".p") (iBal,iCurr,"", OUTPUT fnd-acct).

            /* Commented by KSV: Удаляем глобальный параметр */
            RUN DeleteOldDataProtocol IN h_base ("AcctNumber").

            IF fnd-acct = ? THEN DO:
              RUN VALUE(U3-proc + ".p") (iBal,iCurr,iClass).
              RUN VALUE(U2-proc + ".p") (iBal,iCurr,"", OUTPUT fnd-acct).
              IF fnd-acct = ? THEN RETURN "exit".
            END.
         END.
         IF fnd-acct <> ? THEN ac = fnd-acct.
      END.

      /* Проверяем наличие свободных значений счётчика, если ещё не проверяли. */
      IF vNumFreeCnt = ? THEN DO:
         RUN CalcNumFreeCounters IN THIS-PROCEDURE (ac,
                                                    iCurr,
                                                    iAcctCat,
                                                    iBal,
                                                    iClass,
                                                    iMask,
                                                    OUTPUT vNumFreeCnt).
         IF vNumFreeCnt = 0 THEN DO:
            oErrMsg = IF CAN-FIND (FIRST code WHERE
                                      code.class = "СчетаРезерва" AND
                                      code.code  = AddFilToAcct(ac, shFilial)
                                   NO-LOCK)
                      THEN "Все значения либо используются, либо зарезервированы."
                      ELSE "".
            oErrMsg = "|acct60||%s=" + oErrMsg.
            LEAVE NEXTACCT.
         END.
      END.

      /* Проверка на присутствие номера счета в резерве счетов. */
      /* Вставка Плюс банк */
      RUN STORED-PROCEDURE IS_ACCT_EXISTS mHandle = PROC-HANDLE
         (
         INPUT  PARAM P_ACCT = ac,
         OUTPUT PARAM IS_ACCT_EXISTS = ?
         ).
      CLOSE STORED-PROC IS_ACCT_EXISTS iStat = PROC-STATUS.
      IF iStat = 0 THEN
      DO:
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nac = " + ac + " iStat = " + STRING(iStat) + " IS_ACCT_EXISTS = " + STRING(IS_ACCT_EXISTS)).
         IF IS_ACCT_EXISTS EQ 1 THEN
         DO:
            jj = jj + 1.
            NEXT NEXTACCT.
         END.
      END.
      ELSE
      DO:
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "iStat = " + STRING(iStat)).
         oErrMsg = "||-1|" + ERROR-STATUS:GET-MESSAGE(1).
         jj = jj + 1.
         NEXT NEXTACCT.
      END.
      /* Конец вставки Плюс банк */

      IF CAN-FIND (FIRST CODE WHERE code.class = "СчетаРезерва"
                                AND code.code  = AddFilToAcct(ac, shFilial))
      THEN DO:
         /* Если счет занесен в классификатор "счета резерва",
         ** и в маске нет счетчика,
         ** то выдаем сообщение и выходим.*/
         IF toklen[{&TOK_CNT_IDX}] = 0 THEN DO:
            oErrMsg = "|acct02||%s=" + ac.
            LEAVE NEXTACCT.
         END.
         jj = jj + 1.
         NEXT NEXTACCT.
      END.
      /* Проверка на наличия счета с полученным номером. */
      IF CAN-FIND (FIRST acct WHERE acct.filial-id = shFilial
                                AND acct.number    = ac
                                AND acct.curr      = iCurr)
      THEN DO:
         /* Если такой счет уже существует
         ** и счетчика в номере не предусмотрено,
         ** то выдаем сообщение и выходим.  */
         IF toklen[{&TOK_CNT_IDX}] = 0 THEN DO:
            oErrMsg = "|acct07||".
            LEAVE NEXTACCT.
         END.
         /* Если счетчик существует,
         ** то инкрементируем его и повторяем цикл. */
         jj = jj + 1.
         NEXT NEXTACCT.
      END.
      /* Формируем номер счета и выходим. */
      oAcct = ac.
      LEAVE NEXTACCT.
   END.
END PROCEDURE.

/* Выдает маску по классу
   iClass - класс счета , например acctb
   oMask  - маска счета */
PROCEDURE GetAcctClassMaskString:
   DEFINE INPUT  PARAMETER iClass LIKE acct.class-code NO-UNDO.
   DEFINE OUTPUT PARAMETER oMask  AS CHARACTER INIT "" NO-UNDO.
   DEFINE VARIABLE vMask AS CHARACTER NO-UNDO.
   DEFINE BUFFER bCode FOR code.

   vMask = GetXattrInit(iClass,"acct").
   IF NOT {assigned vMask} THEN RETURN.

   /* Commented by KSV: Определяем, является ли маска классификатором.
   ** Если в маске счета задан классификатор, то ищем любое значение этого
   ** классификатора и по нему определяем длину счета. Т.е. должно обеспечиваться
   ** условие, что в классификаторе содержатся равноценные по длине маски */
   IF NUM-ENTRIES(vMask) = 1 THEN
      FIND FIRST bCode WHERE
         bCode.class  = vMask AND
         bCode.parent = vMask AND
         bCode.val    > ""    NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST bCode WHERE
         bCode.class  = ENTRY(1,vMask) AND
         bCode.parent = ENTRY(2,vMask) AND
         bCode.val    > ""    NO-LOCK NO-ERROR.

   /* Commented by KSV: Если маска задана как классификатор, берем первое
   ** значение из классификатора */
   IF AVAIL bCode THEN vMask = bCode.val.

   /* Commented by KSV: В случае, если для классификатора задана сложная
   ** иерархия берем таки значение по умолчанию */
   IF NOT AVAILABLE bCode AND
      CAN-FIND(FIRST bCode WHERE bCode.class = ENTRY(1,vMask)) THEN
      vMask = "бббббвввкффффссссссс".

   oMask = vMask.
END PROCEDURE.

PROCEDURE GetAcctMask:
   DEFINE INPUT  PARAMETER iAcctMask  AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oTokAcct   AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oTokIdx    AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oTokLen    AS CHAR NO-UNDO.

   DEFINE VAR vTokIdx   LIKE tokidx NO-UNDO.
   DEFINE VAR vTokLen   LIKE toklen NO-UNDO.
   DEFINE VAR vTokList  AS CHAR INIT {&TOK_LIST_EQ} NO-UNDO.
   DEFINE VAR vMaskPos  AS INT64  NO-UNDO.
   DEFINE VAR vMaskSym  AS CHAR     NO-UNDO.
   DEFINE VAR vIdxPos   AS INT64  NO-UNDO.
   DEFINE VAR vSymCode  AS INT64  NO-UNDO.
   DEFINE VAR vBlockStr AS CHAR     NO-UNDO.
   DEFINE VAR vBlockBeg AS INT64  NO-UNDO.
   DEFINE VAR vBlockEnd AS INT64  NO-UNDO.
   DEFINE VAR vBlockPos AS INT64  NO-UNDO.

   ASSIGN
      oTokAcct = ""
      vTokLen = 0
      vTokIdx = 0
   .

   DO vMaskPos = 1 TO LENGTH(iAcctMask):
      vMaskSym = SUBSTR(iAcctMask,vMaskPos,1).
      IF  vMaskSym >= "0"
      AND vMaskSym <= "9" THEN DO:
         ASSIGN
            vIdxPos  = vIdxPos  + 1
            oTokAcct = oTokAcct + vMaskSym
         .
         NEXT.
      END.
      vSymCode = LOOKUP(vMaskSym,vTokList).
      IF vSymCode = 0 THEN DO:
         /* ошибка, нет такого символа */
         RUN Fill-SysMes("","acct09","","%s=" + vMaskSym + "%s=" + iAcctMask).
         RETURN.
      END.
      ELSE IF vSymCode = {&TOK_BRACKET_IDX} THEN DO:
         /* блок [ ... ] */
         ASSIGN
            vMaskSym  = SUBSTR(iAcctMask,vMaskPos - 1,1)
            vSymCode  = LOOKUP(vMaskSym,vTokList)
            vBlockStr = ENTRY(1,SUBSTR(iAcctMask,vMaskPos + 1),"]")
            vMaskPos  = vMaskPos + LENGTH(vBlockStr) + 1
            vBlockBeg = INT64(ENTRY(1,vBlockStr,"-"))
            vBlockEnd = (IF NUM-ENTRIES(vBlockStr,"-") = 1
                         THEN vBlockBeg
                         ELSE INT64(ENTRY(2,vBlockStr,"-")))
         .
         DO vBlockPos = vBlockBeg TO vBlockEnd:
            ASSIGN
               oTokAcct         = oTokAcct + vMaskSym
               vIdxPos          = vIdxPos  + 1
               vTokIdx[vIdxPos] = vBlockPos
            .
         END.
         vTokLen[vSymCode] = vTokLen[vSymCode] + vBlockEnd - vBlockBeg + 1.
      END.
      ELSE IF (    vMaskPos < LENGTH(iAcctMask)
               AND SUBSTR(iAcctMask, vMaskPos + 1, 1) <> {&TOK_BRACKET})
           OR vMaskPos = LENGTH(iAcctMask) THEN ASSIGN
         /* Просто символ */
         vIdxPos           = vIdxPos           + 1
         oTokAcct          = oTokAcct          + vMaskSym
         vTokLen[vSymCode] = vTokLen[vSymCode] + 1
         vTokIdx[vIdxPos]  = vTokLen[vSymCode]
      .
   END.
   DO vIdxPos = 1 TO EXTENT(vtokidx):
      oTokIdx = oTokIdx + STRING(vtokidx[vIdxPos]) + ','.
      IF vIdxPos <= EXTENT(vtoklen)
         THEN oTokLen = oTokLen + STRING(vtoklen[vIdxPos]) + ','.
   END.
END PROCEDURE.

PROCEDURE SetAcct.
   DEFINE INPUT-OUTPUT PARAMETER ac AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER toktype   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER tok       AS CHARACTER NO-UNDO.

   DEFINE VAR i        AS INT64 NO-UNDO.
   DEFINE VAR vMAXPOS  AS INT64 NO-UNDO.

   vMAXPOS = -1.
   DO i = 1 TO LENGTH(tokacct):
      ASSIGN
         vMAXPOS = tokidx[i] WHEN SUBSTR(tokacct,i,1) =  toktype
                              AND INT64(tokidx[i])  >  vMAXPOS
      .
   END.
   IF vMAXPOS - LENGTH(tok) >  0 THEN
   tok = FILL("0",vMAXPOS - LENGTH(tok)) + tok.

   DO i = 1 TO LENGTH(tokacct):
      IF SUBSTR(tokacct,i,1) =  toktype THEN
      OVERLAY(ac,i,1) = SUBSTR(tok,tokidx[i],1).
   END.
END PROCEDURE.

{bal2acct.i} /* Инструменты копирования доп.реквизитов */

/*------------------------------------------------------------------------------
  Purpose:     Устанавливает параметры счетчика.
  Parameters:  iCounter - желательный счетчик для счета
               iErrProc - хэндл процедуры содержащий процедуру обратного вызова
                          SetFoundAcct, запускаемой в случае, если счет с таким
                          счетчиком существует и может быть использован вместо
                          открытия нового счета
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE SetDefinedCntParam:
   DEFINE INPUT  PARAMETER iCounter AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iErrProc AS HANDLE     NO-UNDO.

   ASSIGN
      mDefinedCounter = iCounter
      mDefinedErrProc = iErrProc.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Пытается использовать заранее опредленный счетчик для
               создаваемого счета.
  Parameters:  iBalAcct    - номер счета 2-го порядка для открываемого счета
               iCurrency   - код валюты для открываемого счета
               iAcctNum    - номер сформированного счета
               iAcctMsk    - маска, используемая для открытия счета
               oCounter    - значение счетчика
  Notes:       Если счет с требуемым счетчиком уже существует, то предусмотрен
               вызов процедуры обратного вызова, которая может возвратить
               "ERROR", т.о. прекращая открытие нового счета.
------------------------------------------------------------------------------*/
PROCEDURE GetDefinedCounter:
   DEFINE INPUT  PARAMETER iBalAcct  AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctNum  AS CHARACTER        NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctMsk  AS CHARACTER        NO-UNDO.
   DEFINE OUTPUT PARAMETER oCounter  AS CHARACTER INIT ? NO-UNDO.

   DEFINE VARIABLE vMskCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vMskLen AS INT64    NO-UNDO.
   DEFINE VARIABLE vMinPos AS INT64    NO-UNDO.
   DEFINE VARIABLE vMaxPos AS INT64    NO-UNDO.
   DEFINE VARIABLE vCntStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCntInt AS INT64    NO-UNDO.
   DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bAcct FOR acct.

   MAIN:
   DO ON ERROR UNDO MAIN, LEAVE MAIN:
      /* Проверяем, определен ли счетчик  */
      IF mDefinedCounter = ? THEN LEAVE MAIN.

      vMinPos = INDEX  (iAcctMsk,{&TOK_CNT}).
      vMaxPos = R-INDEX(iAcctMsk,{&TOK_CNT}).

      /* Проверяем, есть ли  в маске счета место для счетчика */
      IF vMinPos = 0 THEN LEAVE MAIN.

      vCntStr = SUBSTR(iAcctMsk,vMinPos,vMaxPos - vMinPos + 1).

      /* Проверяем, совпадает ли длина желаемого счетчика с возможным */
      IF LENGTH(vCntStr) <> LENGTH(mDefinedCounter) THEN
      DO:
         /* Проверяем, является ли счетчик числом. Нечисловые счетчики
         ** не поддерживаются */
         vCntInt = INT64(mDefinedCounter) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN LEAVE MAIN.

         /* Проверяем, может ли счетчик использован без потери информации */
         IF LENGTH(STRING(vCntInt)) > LENGTH(vCntStr) THEN LEAVE MAIN.

         /* Выравниваем счетчик под требуемую длину */
         mDefinedCounter = STRING(vCntInt,FILL("9",LENGTH(vCntStr))).
      END.

      vMskLen = LENGTH(iAcctMsk).
      vCntInt = 1.

      /* Создаем маску счета с включенным в нее счетчиком */
      DO vMskCnt = 1 TO vMskLen:
         IF CAN-DO("к,п",SUBSTR(iAcctMsk,vMskCnt,1)) THEN
            SUBSTR(iAcctNum,vMskCnt,1) = ".".
         IF SUBSTR(iAcctMsk,vMskCnt,1) = {&TOK_CNT} THEN
         DO:
            SUBSTR(iAcctNum,vMskCnt,1) = SUBSTR(mDefinedCounter,vCntInt,1).
            vCntInt = vCntInt + 1.
         END.
      END.

      /* Проверяем, нет ли счета с такой маской в БД */
      FOR FIRST bAcct WHERE
         bAcct.bal-acct =        iBalAcct  AND
         bAcct.currency =        iCurrency AND
         bAcct.acct     MATCHES  iAcctNum  NO-LOCK:

         /* Проверяем, есть ли обработчик для найденного счета */
         IF VALID-HANDLE(mDefinedErrProc) THEN
         DO:
            /* Проверяем, удовлетворяет найденный счет вызывающую процедуру,
            ** если - да, то она должна возвратить "ERROR" и создание нового
            ** счета будет прекращено  */
            RUN SetFoundAcct IN mDefinedErrProc (bAcct.acct) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN vResult = RETURN-VALUE.
         END.
         LEAVE MAIN.
      END. /* End of FOR */

      /* Возвращаем счетчик */
      oCounter = mDefinedCounter.
   END. /* MAIN: */

   /* Сбрасываем счетчик и процедуру обратного вызова, чтобы они случайно не
   ** были использованы повторно */
   ASSIGN
     mDefinedCounter = ?
     mDefinedErrProc = ?.

   RETURN vResult.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Создание счета
  Parameters:  iClass      - класс создаваемого счета
               iBal        - номер счета 2-го порядка
               iCurr       - код валюты
               iCustCat    - тип клиента (Ю,Б,Ч)
               iCustID     - идентификатор клиента
               iOpenDate   - дата открытия счета
               oAcct       - номер созданного счета
                             ? - ошибка создания счета
  Notes: ВНИМАНИЕ !!! ИЗ MAKEACCT УБРАНЫ ИНТЕРФЕЙСЫ ВЫБОРА ИЗ КЛАССИФИКАТОРОВ
         МАСКИ СЧЕТА И КОДОВ ДОХОДОВ/РАСХОДОВ . ЕСЛИ ЭТИ ИНТЕРФЕЙСЫ НУЖНЫ -
         НАДО ЗАПУСКАТЬ cm_acct_cr()
------------------------------------------------------------------------------*/
PROCEDURE MakeAcct:
   DEF INPUT  PARAM iClass          AS CHAR   NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iBal            AS INT64    NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iCurr           AS CHAR   NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iCustCat        AS CHAR   NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iCustID         AS INT64    NO-UNDO. /* неОбязательный */
   DEF INPUT  PARAM iOpenDate       AS DATE   NO-UNDO. /* неОбязательный */
   DEF OUTPUT PARAM oAcct           AS CHAR            /* Откатываемый */
                                    INIT ?.
   DEF        PARAM BUFFER acct     FOR acct.          /* Буфер счета. */
   DEF INPUT  PARAM iAcctMask       AS CHAR   NO-UNDO
                                    FORMAT "X(25)".
   DEF INPUT  PARAM iKodDoxRash     AS CHAR   NO-UNDO
                                    FORMAT "X(5)".
   DEF INPUT  PARAM iDetails        AS CHAR   NO-UNDO. /* неОбязательный */
   DEF INPUT  PARAM iKauId          AS CHAR   NO-UNDO. /* неОбязательный */
   DEF INPUT  PARAM iContract       AS CHAR   NO-UNDO. /* неОбязательный */
   DEF INPUT  PARAM iUserId         AS CHAR   NO-UNDO. /* неОбязательный */
   DEF INPUT  PARAM iBranchId       AS CHAR   NO-UNDO. /* неОбязательный */
   DEF INPUT  PARAM iCopyBalXattr   AS LOG    NO-UNDO. /* копировать ДР с bal-acct*/

   DEF VAR vAcct LIKE acct.acct NO-UNDO. /* Номер счета. */
   DEF VAR vDate-In AS DATE NO-UNDO.
   DEFINE VARIABLE vOK AS LOGICAL NO-UNDO.

   ASSIGN
      vClass    = iClass
      vAcct-Cat = GetXAttrInit(iClass,"acct-cat")
   .
   FIND FIRST bal-acct WHERE
      bal-acct.bal-acct =  iBal
   NO-LOCK NO-ERROR.
   {was-err.i}

   IF NOT fBalAcctWorkPlan(string(iBal)) THEN DO:

      IF FGetSetting("Н302П","ContrWorkPlan","") =  "Предупреждение" THEN
         IF SESSION:BATCH-MODE AND SESSION:CLIENT-TYPE <> "WEBSPEED"
         THEN MESSAGE "Счет второго порядка "  + string(iBal) + " не включен в рабочий план счетов.".
         ELSE RUN Fill-SysMes("","","0","Счет второго порядка "  + STRING(iBal) + " не включен в рабочий план счетов.").
      IF FGetSetting("Н302П","ContrWorkPlan","") =  "Запрет" THEN
         RETURN ERROR "Счет второго порядка "  + STRING(iBal) + " не включен в рабочий план счетов.".

   END.

   RUN ACGetKeyProg(vclass, OUTPUT keyprog).
   ASSIGN
                        /* Код дох.расх должен быть во вх.параметрах*/
      mSymbPU  =  iKodDoxRash
                        /* Маска должна быть во вх.параметрах
                        **  ИХ ^^^^^ нужно получать ранее запуском FindAcctMask */
      acctmask =  iAcctMask
   .
   TR:
   DO TRANSACTION
   ON ERROR UNDO TR, LEAVE TR
   ON STOP  UNDO TR, LEAVE TR:
                        /* Если счет не создан, то создаем его. */
      IF NOT AVAILABLE acct
      THEN DO:
         CREATE acct NO-ERROR.
         {was-err.i &LBL=TR }
      END.
                        /* Инициализация значениями. */
      ASSIGN
         acct.class-code   =  iClass
         acct.bal-acct     =  iBal
         acct.currency     =  iCurr
         acct.cust-cat     =  iCustCat
         acct.cust-id      =  IF iCustCat    <> 'В'   THEN iCustID   ELSE 0
         acct.open-date    =  IF iOpenDate   <> ?     THEN iOpenDate ELSE dob
         acct.contract     =  IF       iContract <> ?
                                 AND   iContract <> ''
                                 THEN iContract
                                 ELSE IF bal-acct.contract <> ""
                                    THEN bal-acct.contract
                                    ELSE acct.contract
         acct.user-id      =  IF    iUserId  =  ""
                                 OR iUserId  =  ?
                                 THEN USERID ("bisquit")
                                 ELSE iUserId
         acct.side         =  bal-acct.side
                                 WHEN LOOKUP(bal-acct.side, 'А,П,АП') <> 0
         acct.acct-cat     =  bal-acct.acct-cat
         acct.rate-type    =  IF acct.currency <> ""
                                 THEN "Учетный"
                                 ELSE ""
         acct.kau-id       =  IF       iKauId <> ?
                                 AND   iKauId <> ''
                                 THEN iKauId
                                 ELSE ""
      NO-ERROR.
      {was-err.i &LBL=TR }
      ASSIGN
         acct.branch-id    =  IF    iBranchId =  ""
                                 OR iBranchId =  ?
                                 THEN TRIM (GetUserBranchId (USERID ("bisquit")))
                                 ELSE iBranchId
      NO-ERROR.
      {was-err.i &LBL=TR }
      IF LOOKUP(acct.cust-cat,"Ю,Б,Ч") > 0 THEN DO:
      vDate-In = DATE(getValueAttr(getCustClass(acct.cust-cat),
                      STRING(acct.cust-id),
                     "date-in")) NO-ERROR.
      {was-err.i &LBL=TR }
       IF vDate-In > acct.open-date THEN
           UNDO TR, RETURN ERROR 'ВНИМАНИЕ! Дата открытия счета не может быть меньше даты регистрации клиента!'.
      END.

      IF GetCode ("ШаблКАУ", acct.kau-id) =  ?
         THEN acct.kau-id  = GetXAttrInit(vClass,"kau-id").
                        /* Формирование номера счета. */
      vAcct = ''.
      RUN Acct-Cr (iBal, iCurr, INPUT-OUTPUT vAcct, BUFFER acct) NO-ERROR.
      {was-err.i LBL=TR}

      vAcct = IF GetSysConf("PlacementFO_RSHB_acct") <> ? THEN GetSysConf("PlacementFO_RSHB_acct")
                                                          ELSE vAcct.
      IF    vAcct =  ?
         OR vAcct =  ''
         THEN  UNDO TR, RETURN ERROR 'Ошибка в процедуре заполнения номера счета '.
      ASSIGN
         acct.acct   =  vAcct
         acct.number =  vAcct
      NO-ERROR.
      {was-err.i &LBL=TR }
                        /* Вызов триггера на WRITE. */
      VALIDATE acct NO-ERROR.
      {was-err.i &LBL=TR }
                        /* Создание ДР на счете. */
      IF {assigned iDetails} THEN DO:
         RUN SetAcctDetails IN THIS-PROCEDURE (BUFFER acct,
                                               iOpenDate,
                                               iDetails,
                                               OUTPUT vOK).
         IF vOK <> YES THEN DO:
            vErrStr = GetErrMsg() NO-ERROR.
            UNDO TR, RETURN ERROR vErrStr.
         END.
      END.
      RUN MakeXattr (acct.acct, acct.currency, iCopyBalXattr) NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN RETURN ERROR RETURN-VALUE.
                        /* Сохраняем номер счета. */
      oAcct = acct.acct.
   END.
   RETURN.
END PROCEDURE.

/* Создание ДР для счета. */
PROCEDURE MakeXattr.
   DEF INPUT  PARAM iAcct        AS CHAR   NO-UNDO. /* Счет. */
   DEF INPUT  PARAM iCurrency    AS CHAR   NO-UNDO. /* Валюта. */
   DEF INPUT  PARAM iCopyXattr   AS LOG    NO-UNDO. /* Копировать ли ДР со счета 2-го порядка. */

   DEF BUFFER acct FOR acct. /* Локализация буфера. */

   FIND FIRST acct WHERE
            acct.acct      =  iAcct
      AND   acct.currency  =  iCurrency
   NO-LOCK NO-ERROR.

   IF AVAIL acct THEN
   BLCK:
   DO TRANSACTION:
                        /* Создаем ДР сотрудник открывший счет. */
      UpdateSigns (
         acct.class-code,
         acct.acct + "," + acct.currency,
         "СотрОткрСч",
         USERID ("bisquit"),
         ?
      ).
      /* Копируем ДР со счета 2-го порядка или с
      ** использованием классификатора МаскиНаслед. */
      IF iCopyXattr <> FALSE
      THEN DO:
         RUN BalToAcct_Xattr (RECID (acct), "*", YES, YES) NO-ERROR.
         {was-err.i &LBL=BLCK}
      END.
   END.
   RETURN.
END PROCEDURE.

/* Получение формата счета. */
FUNCTION GetAcctFmtEx RETURN CHAR
   (INPUT iAcctCat  AS CHAR,   /*категория счета*/
    INPUT iDefFmt   AS CHAR,   /*значение по умолчанию */
    INPUT iMessMode AS LOG):   /*выводить сообщения ? */

   DEFINE VARIABLE vAcctFmt AS CHARACTER NO-UNDO. /* Формат счета. */

   vAcctFmt = FGetSettingEx ("Output-Formats", iAcctCat + "-Acct-Fmt" ,iDefFmt,iMessMode).
   IF iDefFmt  =  ? AND
      vAcctFmt =  ?
      THEN vAcctFmt = FGetSetting ("Output-Formats",
                                   "-Acct-Fmt",
                                   "xxxxxxxxxxxxxxxxxxxx").

   RETURN vAcctFmt.
END FUNCTION.


FUNCTION GetAcctFmt RETURN CHAR (
   INPUT iAcctCat AS CHAR        /* Категория счета. */
   ):

   RETURN GetAcctFmtEx(iAcctCat,?,no).

END FUNCTION.

/* Получение цвета отображения остатка. */
FUNCTION GetBalColorBuffer RETURN CHAR (
   INPUT iHAcct   AS HANDLE,     /* Указатель на буффер счета. */
   INPUT iBalance AS DECIMAL     /* Значение остатка со знаком. */
):
   DEF VAR vAcctPosColor   AS CHAR   NO-UNDO. /* Цвет остатка. */
   DEF VAR vAcctPosCode    AS CHAR   NO-UNDO. /* Код остатка (нормальны/нарушение). */

                        /* Получение кода цвета. */
   vAcctPosCode =
      IF    iBalance =  ?
         OR NOT AcctLookBuffer (iHAcct)
         THEN "NR"
         ELSE IF iHAcct:BUFFER-FIELD ("side"):BUFFER-VALUE =  "А"
            THEN IF iBalance >= 0
               THEN "AN"
               ELSE "ABB"
            ELSE IF iHAcct:BUFFER-FIELD ("side"):BUFFER-VALUE =  "П"
               THEN IF iBalance <= 0
                  THEN "PN"
                  ELSE "PBB"
               ELSE "NR".
                        /* Поиск настройки цвета по коду. */
   vAcctPosColor = fGetSetting ("AcctPosColors", vAcctPosCode, ?).
                        /* Формирование цвета по коду остатка,
                        ** если не установлен НП. */
   IF vAcctPosColor =  ?
   THEN CASE vAcctPosCode:
      WHEN "AN"   THEN vAcctPosColor = "bright-red".
      WHEN "ABB"  THEN vAcctPosColor = "blink-bright-red".
      WHEN "PN"   THEN vAcctPosColor = "bright-cyan".
      WHEN "PBB"  THEN vAcctPosColor = "blink-bright-cyan".
      WHEN "NR"   THEN vAcctPosColor = "bright-yellow".
   END CASE.
   RETURN vAcctPosColor.
END FUNCTION.

/* Получение цвета счета по указателю на счет. */
FUNCTION GetAcctColorBuffer RETURN CHAR (
   INPUT iHAcct   AS HANDLE,     /* Указатель на буффер счета. */
   INPUT iDate    AS DATE        /* Дата определения цвета */
):
   RETURN IF     (   BlockAcct (iHAcct:BUFFER-FIELD ("acct"):BUFFER-VALUE + ',' + iHAcct:BUFFER-FIELD ("currency"):BUFFER-VALUE,
                                DATETIME(iDate + 1) - 1
                                ) <> ""
                  OR ChkAcctCart(iHAcct))
             AND {assigned mColorBlockAcct}
          THEN mColorBlockAcct
          ELSE  IF     iHAcct:BUFFER-FIELD ("close-date"):BUFFER-VALUE <> ?
                   AND {assigned mColorCloseAcct}
          THEN mColorCloseAcct
          ELSE "normal".

END FUNCTION.
/* Получение цвета отображения остатка */
FUNCTION GetBalColor RETURN CHAR (
   BUFFER bacct FOR acct,        /* Буффер счета */
   INPUT iBalance AS DECIMAL     /* Баланс (либо ? если нет) */
):
   RETURN GetBalColorBuffer ((BUFFER bAcct:HANDLE), iBalance).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     Выполняет резервирование номера счета для дальнейшего
               использования. Зарезервированный номер счета не может быть
               использован повторно.
  Parameters:  iAcct - номер счета
               oOk   - флаг возврата: YES - счет зарезервирован успешно
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE AcctKeep:
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk   AS LOGICAL.

   DEFINE BUFFER CODE FOR CODE.

   IF GetCodeEx("СчетаРезерва",iAcct,?) <> ? THEN
   DO:
      oOk = YES.
      RETURN .
   END.


   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:
      CREATE CODE.
      ASSIGN
         CODE.class  = "СчетаРезерва"
         CODE.parent = "СчетаРезерва"
         CODE.code   = iAcct.
      oOk = YES.

      RELEASE code.
   END.  /* End of TR BLOCK */

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Выполняет освобождение зарезервированного номера счета.
  Parameters:  iAcct - номер счета
               oOk   - флаг возврата: YES - счет освобожден успешно
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE AcctFree:
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk   AS LOGICAL    .

   DEFINE BUFFER CODE FOR CODE.

   IF GetCodeEx("СчетаРезерва",iAcct,?) = ? THEN
   DO:
      oOk = YES.
      RETURN .
   END.


   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:
      FIND FIRST CODE WHERE
         CODE.class  = "СчетаРезерва" AND
         CODE.parent = "СчетаРезерва" AND
         CODE.code   = iAcct NO-ERROR.
      DELETE CODE.
      oOk = YES.
   END.  /* End of TR BLOCK */

END PROCEDURE.

/**************************************
 * ПРОЦЕДУРЫ ВАЛИДАЦИИ ЛИЦЕВОГО СЧЕТА *
 *************************************/
/*------------------------------------------------------------------------------
  Purpose:     Проверка типа клиента лицевого счета
  Parameters:  iBalAcct - счет 2-го порядка
               iCustCat - тип клиента
               oOk      - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Cust-Cat:
   DEFINE INPUT  PARAMETER iBalAcct AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iCustCat AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct    AS CHARACTER  NO-UNDO. /* Счет 1го порядка для инфо */
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER bal-acct FOR bal-acct.
   DEFINE BUFFER b-loan   FOR loan.

   DEF VAR vTmp     AS CHARACTER NO-UNDO .
   DEF VAR vStrLoan AS CHARACTER NO-UNDO .

   /* Commented by KSV: Ищем счет 2-го порядка и сравниваем допустимые типы
   ** клиентов */
   FIND bal-acct WHERE bal-acct.bal-acct = iBalAcct NO-LOCK NO-ERROR.
   IF AVAILABLE bal-acct             AND
      LENGTH(bal-acct.cust-cat) = 1  AND
      iCustCat <> bal-acct.cust-cat  THEN
   DO:
      /* Номер договора показывается только для транзакций   . проставляется в credacct.p */
      vTmp  = GetSysConf ("LoanRecid-Acct19") .
      RUN DeleteOldDataProtocol IN h_base ("LoanRecid-Acct19").
      vStrLoan = "".
      IF vTmp <> ""  and vTmp <>  ? THEN DO:
         FIND FIRST b-loan WHERE
                    RECID(b-loan)  = INT64(vTmp)
                    NO-LOCK NO-ERROR .
         IF AVAILABLE b-loan THEN
            vStrLoan = " Договор : " + b-loan.doc-ref.
      END.

      RUN Fill-SysMes ("","acct19","",
      substitute("%s=&1%s=&2  Лицевой счет: &3 &4"  , iCustCat , bal-acct.cust-cat, iAcct , vStrLoan )) .
      
      RETURN.
   END.
   oOk = 0.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка счета 2-го порядка
  Parameters:  iBalAcct - счет 2-го порядка
               iAcctCat - категория учета лицевого счета
               iCustCat - тип клиента лицевого счета
               oOk      - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Bal-Acct:
   DEFINE INPUT  PARAMETER iBalAcct AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustCat AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct    AS CHARACTER  NO-UNDO. /* Счет 1го порядка для инфо */
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER bal-acct FOR bal-acct.

   FIND bal-acct WHERE bal-acct.bal-acct = iBalAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bal-acct then
   DO:
      RUN Fill-SysMes("","acct17","",
                      "%s=" + IF iBalAcct = ?
                              THEN "?"
                              ELSE STRING(iBalAcct)).
      RETURN.
   END.

   IF bal-acct.acct-cat <> iAcctCat THEN
   DO:
      RUN Fill-SysMes("","acct18","",
                      "%s=" + iAcctCat +
                      "%s=" + bal-acct.acct-cat).
      RETURN.
   END.

   RUN Check-Acct-Cust-Cat(iBalAcct,iCustCat, iAcct, OUTPUT oOk).

   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка валюты лицевого счета
  Parameters:  iCurrency  - код валюты
               iAcctCat   - категория учета
               oOk        - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Currency:
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk         AS INT64    NO-UNDO INIT -1.

   IF iAcctCat <> "d" THEN
   DO:
      IF NOT CAN-FIND(currency WHERE currency.currency = iCurrency) THEN
      DO:
         RUN Fill-SysMes("","acct20","","%s=" + iCurrency).
         RETURN.
      END.
   END.
   ELSE
   DO:
      IF NOT CAN-FIND(sec-code WHERE sec-code.sec-code = iCurrency) THEN
      DO:
         RUN Fill-SysMes("","acct21","","%s=" + iCurrency).
         RETURN.
      END.
   END.

   oOk = 0.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка парного счета
  Parameters:  iContrAcct - парный счет
               iCurrency  - код валюты
               iBalAcct   - счет 2-го порядка
               iSide      - признак актив/пассив
               iAcctCat   - категория учета
               oOk        - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Contr-Acct:
   DEFINE INPUT  PARAMETER iContrAcct  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct    AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iSide       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk         AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER xacct FOR acct.
   DEFINE BUFFER CODE  FOR CODE.

   IF NOT {assigned iContrAcct} THEN
   DO:
      oOk = 0.
      RETURN .
   END.

   /* ищем новый парный счет */
   {find-act.i
      &bact = xacct
      &acct = iContrAcct
      &curr = iCurrency
   }

   IF NOT AVAIL xacct THEN
   DO:
      RUN Fill-SysMes("","acct22","","%s=" + iContrAcct +
                                     "%s=" + iCurrency).
      RETURN.
   END.
  
   IF xacct.currency <> iCurrency THEN
   DO:
      RUN Fill-SysMes("","acct22","","%s=" + iContrAcct +
                                     "%s=" + iCurrency).
      RETURN.
   END.

   IF xacct.acct-cat <> iAcctCat THEN
   DO:
      RUN Fill-SysMes("","acct23","","%s=" + iContrAcct +
                                     "%s=" + xacct.acct-cat +
                                     "%s=" + iAcctCat).
      RETURN.
   END.

   IF xacct.side  =  iSide THEN
   DO:
      RUN Fill-SysMes("","acct24","","%s=" + iContrAcct +
                                     "%s=" + xacct.side +
                                     "%s=" + iSide).
      RETURN.
   END.

   /* проверка справочника парных счетов */
   FIND FIRST CODE WHERE
       code.class =  "Dual-bal-acct"  AND
      (code.code =  STRING(iBalAcct)  AND code.val  =  STRING(xacct.bal-acct) OR
       code.val  =  STRING(iBalAcct)  AND code.code =  STRING(xacct.bal-acct))
      NO-LOCK NO-ERROR.

   IF NOT AVAIL code THEN
   DO:
      RUN Fill-SysMes("","acct25","","%s=" + STRING(iBalAcct) +
                                     "%s=" + STRING(xacct.bal-acct)).
      RETURN.
   END.

   oOk = 0.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверка признака актив/пассив
  Parameters:  iCheckOp - тип проверки документов
               iSide    - признак актив/пассив
               oOk      - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Acct-Check-Side:
   DEFINE INPUT  PARAMETER iCheckOp AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSide    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER CODE FOR CODE.
   FIND code WHERE
      code.class = "check-op" AND
      code.code  = iCheckOp NO-LOCK NO-ERROR.

   IF {assigned iCheckOp} AND
      AVAIL code          AND
      NOT CAN-DO(code.val, iSide) THEN
   DO:
      RUN Fill-SysMes("","acct27","","%s=" + iSide +
                                     "%s=" + CODE.val).
      RETURN.
   END.
   oOk = 0.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверяем счет на соответсвие маске
  Parameters:  acct - буфер лицевого счета
               oOk   - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Mask:
   DEFINE PARAMETER BUFFER acct     FOR acct.
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO.

   DEFINE VARIABLE vMask      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCnt       AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum       AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen       AS INT64    NO-UNDO.
   DEFINE VARIABLE vAcctLen   AS INT64    NO-UNDO.
   DEFINE VARIABLE vTokAcct   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTokidxs   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vToklens   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vHCode     AS HANDLE     NO-UNDO.

   /* Берем маску счета */
   RUN GetAcctClassMaskString (acct.class-code, OUTPUT vMask).

   IF NOT {assigned vMask} THEN RETURN.

   /* Commented by KSV: Парсим маску счета */
   RUN GetAcctMask(vMask,OUTPUT vTokacct,OUTPUT vTokidxs,OUTPUT vToklens).


   /* Commented by KSV: Определяем длину счета по маске */
   vLen = LENGTH(vTokacct).

   vAcctLen = LENGTH(acct.number).

   /* Commented by KSV: Проверка длина номера счета на соответсвие маске */
   IF vLen = ? OR vLen <> vAcctLen THEN
   DO:
      oOk = -1.
      RUN Fill-SysMes("","acct28","","%s=" + acct.number +
                                     "%s=" + (IF vAcctLen = ?  THEN "?" ELSE STRING(vAcctLen)) +
                                     "%s=" + (IF vLen = ?      THEN "?" ELSE STRING(vLen))).

   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Полная проверка счета
  Parameters:  acct  - буфер лицевого счета
               oAttr - атрибут лицевого счета, не прошедший валидацию
               oOk   - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct:
   DEFINE PARAMETER BUFFER acct  FOR acct.
   DEFINE OUTPUT PARAMETER oAttr AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk   AS INT64    NO-UNDO INIT -1.

   DEFINE VARIABLE vOk        AS INT64    NO-UNDO.
   DEFINE VARIABLE vlOk       AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE proc-name  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vProcName  AS CHAR       NO-UNDO. /* Процедура метода. */
   DEFINE VARIABLE vUpdValid  AS INT64        NO-UNDO. /* Код ошибки. */
   DEFINE VARIABLE vLocalCur  AS CHAR       NO-UNDO.
   DEFINE VARIABLE vMask      AS CHAR       NO-UNDO.
   DEFINE VARIABLE sTokidx    AS CHAR       NO-UNDO.
   DEFINE VARIABLE sToklen    AS CHAR       NO-UNDO.

   DEFINE BUFFER bal-acct FOR bal-acct.
   DEFINE BUFFER op-entry FOR op-entry.
   DEFINE BUFFER bacct    FOR acct.
   DEFINE BUFFER banks    FOR banks.

   IF NOT AVAILABLE acct THEN RETURN.

   /* Commented by KSV: Проверяем счет 2-го порядка  */
   RUN Check-Acct-Bal-Acct(acct.bal-acct,
                           acct.acct-cat,
                           acct.cust-cat,
                           acct.acct,
                           OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "bal-acct".
      RETURN.
   END.

   /* Commented by KSV: Проверяем валюту счета  */
   RUN Check-Acct-Currency(acct.currency,
                           acct.acct-cat,
                           OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "currency".
      RETURN.
   END.

   /* Commented by KSV: Проверяем парный счет */
   RUN Check-Contr-Acct(acct.contr-acct,
                        acct.currency,
                        acct.bal-acct,
                        acct.side,
                        acct.acct-cat,
                        OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "contr-acct".
      RETURN.
   END.

   /*
   /* Commented by KSV: Проверяем счета на соответсвие маске */
   RUN Check-Acct-Mask(BUFFER acct,
                       OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "acct".
      RETURN.
   END.
   */

   /* Commented by KSV: Проверяем признак актив/пассив */
   RUN Acct-Check-Side(acct.check-op,
                       acct.side,
                       OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "check-op".
      RETURN .
   END.

   /* Получим маску */
   RUN GetAcctClassMaskString (acct.class-code, OUTPUT vMask).
   /* Commented by Malik При вызове из внешней процедуры возникают случаи, когда маска счёта не заполнена, заполняем сами */
   RUN GetAcctMask (INPUT vMask, OUTPUT tokacct,
                    OUTPUT sTokidx, OUTPUT sToklen) NO-ERROR.      
   RUN FillAcctMask (INPUT vMask) NO-ERROR.
   /* Проверяем счет 2-го порядка у лицевого счета если он есть в маске */
   /* Commented by Mixey:  29/07/2011 (0152425) Убрал проверку для классификатора 'МаскиСчетов'.
                                                Проверяем балансовый счет в соответствии с маской при любом раскладе */ 
   
   IF  INDEX(tokacct,FILL({&TOK_BAL},5)) > 0 THEN DO:
      IF ((acct.acct-cat =  "d" AND depobal) OR
           acct.acct-cat <> "d" AND
           acct.acct-cat <> "n" AND
           acct.acct-cat <> "x") AND
         INT64(SUBSTRING(STRING(acct.acct),INDEX(tokacct,{&TOK_BAL}),toklen[{&TOK_BAL_IDX}])) <> acct.bal-acct THEN
      DO:
         RUN Fill-SysMes("","acct10","","%s=" + acct.Acct +
                                        "%s=" + STRING(acct.bal-acct)).
         oAttr = "acct".
         RETURN.
      END.
   END.

   /* Проверяем валюту если она есть в маске счета */
   IF INDEX(tokacct,{&TOK_CUR}) > 0 THEN DO:
      vLocalCur = FGetSetting("КодНацВал", ?, "{&in-NC-Code}").
      IF acct.acct-cat <> "d"                         AND
         acct.acct-cat <> "n"                         AND
         acct.acct-cat <> "x"                         AND
         (SUBSTRING(acct.acct,INDEX(tokacct,{&TOK_CUR}),toklen[{&TOK_CUR_IDX}]) <> acct.currency   AND
          acct.currency            <> ""              OR
          SUBSTRING(acct.acct,INDEX(tokacct,{&TOK_CUR}),toklen[{&TOK_CUR_IDX}]) <> vLocalCur       AND
          acct.currency            =  "") THEN
      DO:

         RUN Fill-SysMes("","acct11","","%s=" + acct.Acct +
                                        "%s=" + acct.currency).
         oAttr = "acct".
         RETURN.
      END.
   END.

   /* Commented by KSV: Проверяем признак актив/пассив по счету 2-го порядка */
   FIND bal-acct OF acct NO-LOCK.
   IF INDEX(bal-acct.side, acct.side) =  0 THEN
   DO:
      RUN Fill-SysMes("","acct12","","%s=" + acct.Acct      +
                                     "%s=" + bal-acct.side  +
                                     "%s=" + acct.side).
      oAttr = "side".
      RETURN.
   END.

   /* Commented by KSV: Проверяем валюту по счету 2-го порядка */
   IF bal-acct.foreign-curr <> ?     AND
      bal-acct.acct-cat     <> "d"   THEN
   DO:
      IF (NOT bal-acct.foreign-curr AND acct.currency >  "") OR
         (bal-acct.foreign-curr     AND acct.currency =  "") THEN
      DO:
         RUN Fill-SysMes("","acct13","","%s=" + acct.Acct).
         oAttr = "currency".
         RETURN.
      END.
   END.

   /* Commented by KSV: Проверяем дату открытия */
   FIND FIRST op-entry WHERE
      op-entry.acct-db  =      acct.acct        AND
      op-entry.currency BEGINS acct.currency    AND
      op-entry.op-date  <      acct.open-date   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE op-entry THEN
      FIND FIRST op-entry WHERE
         op-entry.acct-cr  =      acct.acct        AND
         op-entry.currency BEGINS acct.currency    AND
         op-entry.op-date  <      acct.open-date   NO-LOCK NO-ERROR.
   IF AVAILABLE op-entry THEN
   DO:
      RUN Fill-SysMes("","acct14","","%s=" + acct.Acct).
      oAttr = "open-date".
      RETURN.
   END.

   /* Commented by KSV: Проверяем уникальность номера счета */
   IF NOT type-curracct THEN
   DO:
      IF acct.currency =  "" THEN
         FIND FIRST bacct WHERE
            bacct.acct     =  acct.acct AND
            bacct.currency >  ""        NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST bacct WHERE
            bacct.acct     =  acct.acct AND
            bacct.currency =  ""        NO-LOCK NO-ERROR.
      IF AVAILABLE bacct THEN
      DO:
         RUN Fill-SysMes("","acct16","","%s=" + acct.Acct).
         oAttr = "currency".
         RETURN.
      END.
   END.

   /* Запуск метода проверки ключа счета.
   ** Метод проверки не запускается для счетов, категории которых указаны в НП КлючаНетКатСч (vNoKeyCat).
   ** Если НП пуст, то пропускаются только депо счета */
   IF NOT CAN-DO(IF {assigned vNoKeyCat} THEN vNoKeyCat ELSE "d",acct.acct-cat)
   THEN DO:
      vProcName = GET-CLASS-METHOD (acct.class-code, "Chkupd").
      IF vProcName <> ?
      THEN DO:
         IF NOT SearchPfile (vProcName)
         THEN DO:
            RUN Fill-SysMes (
               "", "", "0",
               "Не могу запустить процедуру проверки счета " + vProcName +
               " - процедура не найдена."
            ).
            RETURN.
         END.
         RUN VALUE (vProcName + ".p") (RECID (acct)).
         IF RETURN-VALUE <> "Ok"
            THEN RETURN.
      END.
      ELSE DO:
         RUN Check-Acct-Key (acct.class-code, acct.bal-acct, acct.acct, OUTPUT vUpdValid).
         IF vUpdValid <> 0
            THEN RETURN.
      END.
   END.
   /* Commented by KSV: Проверка клиента банк */
   IF acct.cust-cat =  "Б" THEN
   DO:
      FIND FIRST banks WHERE
         banks.bank-id =  acct.cust-id NO-LOCK.
      {run-meth.i '"banks"' "chkupd" '"cust-req"'} (RECID(banks)).
      IF RETURN-VALUE <> "" THEN RETURN.
   END.

   /* Commented by KSV: Проверяем тип курса */
   IF acct.acct-cat  <> "d" AND
      ((acct.currency =  "" AND     {assigned acct.rate-type})  OR
       (acct.currency <> "" AND NOT {assigned acct.rate-type})) THEN
   DO:
      RUN Fill-SysMes("","acct26","","%s=" + acct.Acct      +
                                     "%s=" + acct.rate-type +
                                     "%s=" + acct.currency).
      oAttr = "rate-type".
      RETURN.
   END.

   /* Commented by KSV: Проверяем подразделение счета */
   RUN CheckBranch IN h_brnch (acct.branch-id).
   IF RETURN-VALUE <> "" THEN
   DO:
      oAttr = "branch-id".
      RETURN.
   END.

   /* Commented by KSV: Проверяем открытость клиента счета */
   RUN custools.p(acct.cust-cat,
                  acct.cust-id,
                  OUTPUT vlOk).
   IF vlOk THEN
   DO:
      oAttr = "cust-id".
      RETURN .
   END.

   /* Commented by KSV: Проверка периода действия счета относительно периода
   ** действия счета 2-го порядка */
   vDate = DATE(GetXattrValue("bal-acct",STRING(acct.bal-acct),"open-date")) NO-ERROR.

   IF vDate > acct.open-date THEN
   DO:
      RUN Fill-SysMes("","acct30","", "%s=" + acct.acct +
                                      "%s=" + STRING(acct.open-date) +
                                      "%s=" + STRING(vDate)).
      oAttr = "open-date".
      RETURN.
   END.

   vDate = DATE(GetTempXattrValue("bal-acct",
                                  STRING(acct.bal-acct),
                                  "close-date")) NO-ERROR.
   IF vDate < acct.open-date THEN
   DO:
      RUN Fill-SysMes("","acct31","", "%s=" + acct.acct +
                                      "%s=" + STRING(acct.open-date) +
                                      "%s=" + STRING(vDate)).
      oAttr = "open-date".
      RETURN.
   END.


   IF acct.close-date <> ? THEN
   DO:
      IF vDate < acct.close-date THEN
      DO:
         RUN Fill-SysMes("","acct32","", "%s=" + acct.acct +
                                         "%s=" + STRING(acct.close-date) +
                                         "%s=" + STRING(vDate)).
         oAttr = "close-date".
         RETURN.
      END.

      /* Commented by KSV: Проверяем наличие проводок после даты закрытия */
      FIND LAST op-entry WHERE
         op-entry.acct-db   =      acct.acct       AND
         op-entry.currency  BEGINS acct.currency   AND
         NOT op-entry.op-status BEGINS 'А'         AND
         op-entry.op-date   <> ?                   AND
         op-entry.op-date   >      acct.close-date NO-LOCK NO-ERROR.
      IF NOT AVAILABLE op-entry THEN
         FIND LAST op-entry WHERE
            op-entry.acct-cr   =      acct.acct       AND
            op-entry.currency  BEGINS acct.currency   AND
            NOT op-entry.op-status BEGINS 'А'         AND
            op-entry.op-date   <> ?                   AND
            op-entry.op-date   >      acct.close-date NO-LOCK NO-ERROR.
      IF AVAILABLE op-entry THEN
      DO:
         RUN Fill-SysMes("","acct15","","%s=" + acct.Acct).
         oAttr = "close-date".
         RETURN.
      END.

      /* Commented by KSV: Проверяем наличие неакцептованных проводок */
      FIND FIRST op-entry WHERE
         op-entry.acct-db  = acct.acct       AND
         op-entry.currency = acct.currency   AND
         op-entry.op-status < CHR(251)       AND
         NOT op-entry.op-status BEGINS 'А'   AND
         op-entry.op-date   <> ?             
         USE-INDEX entry-db NO-LOCK NO-ERROR.
      IF NOT AVAIL op-entry THEN
         FIND FIRST op-entry WHERE
            op-entry.acct-cr  = acct.acct       AND
            op-entry.currency = acct.currency   AND
            op-entry.op-status < CHR(251)       AND
            NOT op-entry.op-status BEGINS 'А'   AND
            op-entry.op-date   <> ?             
            USE-INDEX entry-cr NO-LOCK NO-ERROR.
      IF AVAIL op-entry THEN
      DO:
         FIND FIRST op OF op-entry NO-LOCK.
         RUN Fill-SysMes("","acct33","", "%s=" + acct.acct +
                                         "%s=" + IF op-entry.op-date <> ?
                                                 THEN STRING(op-entry.op-date)
                                                 ELSE STRING(op.doc-date)).
         oAttr = "close-date".
         RETURN.
      END. /* if avail op-entry then do: */

      /* Commented by KSV: Проверка остатков и дат движения по счету */
      RUN acct-pos IN h_base (acct.acct,acct.currency,acct.close-date,?,'П').

      IF sh-bal <> 0 THEN
      DO:
         RUN Fill-SysMes("","acct34","", "%s=" + acct.acct +
                                         "%s=" + STRING(sh-bal) +
                                         "%s=" + (IF lastmove <> ?
                                                  THEN "за " + STRING(lastmove)
                                                  ELSE "")).
         oAttr = "close-date".
         RETURN.
      END. /* if sh-bal <> 0 then do: */

      IF acct.close-date < lastmove THEN
      DO:
         RUN Fill-SysMes("","acct35","", "%s=" + acct.acct +
                                         "%s=" + STRING(acct.close-date) +
                                         "%s=" + STRING(lastmove)).
         oAttr = "close-date".
         RETURN.
      END.

      IF acct.open-date > acct.close-date THEN
      DO:
         RUN Fill-SysMes("","acct36","", "%s=" + acct.acct +
                                         "%s=" + STRING(acct.close-date) +
                                         "%s=" + STRING(acct.open-date)).
         oAttr = "close-date".
         RETURN.
      END.
   END.

   oOk = 0.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     Проверят ключ счета
  Parameters:  iClass   - класс счета
               iBalAcct - счет 2-го порядка
               iAcct    - лицевой счет
               oOk      - 0 - все ОК
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Key:
   DEFINE INPUT  PARAMETER iClass   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO.

   DEFINE VARIABLE vProc   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKey    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctKey AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMask    AS CHARACTER  NO-UNDO.

   /* Commented by Malik При вызове из внешней процедуры возникают случаи, когда маска счёта не заполнена, заполняем сами */
   IF NOT {assigned tokacct} THEN DO:          
     vMask = GetXattrInit(iClass, "acct").
     IF NOT {assigned vMask} THEN RETURN.
     RUN FillAcctMask (INPUT vMask) NO-ERROR.
   END.
   /*Commented by Malik Добавлено условие, для невозникновения таких ошибок:
    ** Starting position for SUBSTRING, OVERLAY, etc. must be 1 or greater.*/
   IF my-key <> ? AND NOT CAN-DO(no-key, STRING(iBalAcct)) AND INDEX(tokacct,{&TOK_KEY}) > 0 THEN
   DO:
      vProc = GET-CLASS-METHOD(iClass, "U1").
      IF vProc = ? THEN keyprog = "key-tst".
      RUN VALUE(keyprog + ".p") (iAcct, my-key, OUTPUT vKey).
   /* Commented by Malik Изменен алгоритм проверки, ключ может быть не только 9-м символом, не обязательно 1 цифра, проверяем по маске */           
      vAcctKey = SUBSTRING(iAcct,INDEX(tokacct,{&TOK_KEY}),toklen[{&TOK_KEY_IDX}]).
      vKey = STRING(vKey, FILL ("9", toklen[{&TOK_KEY_IDX}])).
      IF vAcctKey <> vKey THEN
      DO:
         pick-value = "no".
         RUN Fill-SysMes("","acct29","", "%s=" + (IF iAcct = ? THEN "?" ELSE iAcct) +
                                         "%s=" + (IF vKey = ?  THEN "?" ELSE vKey) +
                                         "%s=" + (IF vAcctKey = ? THEN "?" ELSE vAcctKey)).
         IF pick-value <> "yes" OR GetCode("КодОш", "acct29") =  "-1" THEN
         DO:
            oOk = -1.
            RETURN .
         END.
      END.
   END.
END PROCEDURE.

/* Формирование наименования счета.
** Для юрилических лиц название формируется из поля name-short. */
PROCEDURE GetNameAcctExCorp.
   DEF PARAM BUFFER acct FOR acct.
   DEF OUTPUT PARAM oNameAcct AS CHAR   NO-UNDO.

   DEF VAR name AS CHAR EXTENT 2  NO-UNDO.

   DEF BUFFER cust-corp FOR cust-corp. /* Локализация буфера. */
   IF acct.cust-cat <> "Ю"
   THEN DO:
      {getcust.i
         &name    = name
         &Offinn  = {comment}
      }
      oNameAcct = TRIM (NAME [1] + " " + NAME [2]).
   END.
   ELSE DO:
      IF NOT {assigned acct.details}
      THEN FOR FIRST cust-corp WHERE
         cust-corp.cust-id =  acct.cust-id
      NO-LOCK:
         oNameAcct = cust-corp.name-short.
      END.
      ELSE oNameAcct = acct.details.
   END.

   RETURN.
END PROCEDURE.

/* Процедура удаления счетчика (метод U4).*/
PROCEDURE DelAcin.
   DEF INPUT  PARAM in-acct AS RECID  NO-UNDO.

   DEF VAR i         AS INT64    NO-UNDO.
   DEF VAR ch        AS CHAR   NO-UNDO.
   DEF VAR st        AS CHAR   NO-UNDO.

   DEF BUFFER acct            FOR acct.            /* Локализация буфера. */
   DEF BUFFER bis-temp-table  FOR bis-temp-table.  /* Локализация буфера. */

   BLCK_MAIN:
   DO
   ON ERROR  UNDO BLCK_MAIN, LEAVE BLCK_MAIN
   ON ENDKEY UNDO BLCK_MAIN, LEAVE BLCK_MAIN:

      FIND FIRST acct WHERE
         RECID (acct) =  in-acct
      NO-LOCK NO-ERROR.
      IF NOT AVAIL acct
         THEN LEAVE BLCK_MAIN.

      DO i = 1 TO LENGTH (tokacct):
         ch = SUBSTR (tokacct,i,1).
         IF ch =  "с"
            THEN st = st + SUBSTR (acct.acct, i, 1).
      END.
      FIND FIRST bis-temp-table WHERE
         bis-temp-table.surr  =  STRING (acct.bal-acct)  + "," +
                                 STRING (acct.currency)  + "," +
                                 (IF toklen[{&TOK_BRANCH_IDX}] <> 0
                                    THEN acct.branch-id
                                    ELSE dept.branch)    + "," +
                                 st
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bis-temp-table
         THEN DELETE bis-temp-table.
   END.
   RETURN.
END PROCEDURE.

/* Поиск рублевого остатка по счету на дату. */
PROCEDURE GetAcctPos.
   DEF INPUT  PARAM iHAcct AS HANDLE NO-UNDO.   /* Указатель на буффер счета. */
   DEF INPUT  PARAM iDate  AS DATE   NO-UNDO.   /* Дата поиска остатка. */
   DEF OUTPUT PARAM oPos   AS DEC    NO-UNDO.   /* Значение остатка. */
   DEF OUTPUT PARAM oDate  AS DATE   NO-UNDO.   /* Дата остатка. */

   DEF BUFFER acct-pos FOR acct-pos. /* Локализация буфера. */
                        /* Поиск рублевого остатка. */
   FIND LAST acct-pos WHERE
            acct-pos.acct     =  STRING (iHAcct:BUFFER-FIELD("acct")    :BUFFER-VALUE)
      AND   acct-pos.currency =  STRING (iHAcct:BUFFER-FIELD("currency"):BUFFER-VALUE)
      AND   acct-pos.since    <= iDate
   NO-LOCK NO-ERROR.
                        /* Формирование остатка. */
   IF AVAIL acct-pos
   THEN ASSIGN
      oPos  = acct-pos.balance
      oDate = acct-pos.since
   .
   ELSE ASSIGN
      oPos  = ?
      oDate = ?
   .
   RETURN.
END PROCEDURE.

/* Поиск валютного остатка по счету на дату. */
PROCEDURE GetAcctCur.
   DEF INPUT  PARAM iHAcct AS HANDLE NO-UNDO.   /* Указатель на буффер счета. */
   DEF INPUT  PARAM iDate  AS DATE   NO-UNDO.   /* Дата поиска остатка. */
   DEF OUTPUT PARAM oPos   AS DEC    NO-UNDO.   /* Значение остатка. */
   DEF OUTPUT PARAM oDate  AS DATE   NO-UNDO.   /* Дата остатка. */

   DEF BUFFER acct-cur FOR acct-cur. /* Локализация буфера. */
                        /* Поиск валютного остатка. */
   FIND LAST acct-cur WHERE
            acct-cur.acct     =  STRING (iHAcct:BUFFER-FIELD("acct")    :BUFFER-VALUE)
      AND   acct-cur.currency =  STRING (iHAcct:BUFFER-FIELD("currency"):BUFFER-VALUE)
      AND   acct-cur.since    <= iDate
   NO-LOCK NO-ERROR.
                        /* Формирование остатка. */
   IF AVAIL acct-cur
   THEN ASSIGN
      oPos  = acct-cur.balance
      oDate = acct-cur.since
   .
   ELSE ASSIGN
      oPos  = ?
      oDate = ?
   .
   RETURN.
END PROCEDURE.

/* Поиск остатка по acct-qty на дату. */
PROCEDURE GetAcctQty.
   DEF INPUT  PARAM iHAcct AS HANDLE NO-UNDO.   /* Указатель на буффер счета. */
   DEF INPUT  PARAM iDate  AS DATE   NO-UNDO.   /* Дата поиска остатка. */
   DEF OUTPUT PARAM oPos   AS DEC    NO-UNDO.   /* Значение остатка. */
   DEF OUTPUT PARAM oDate  AS DATE   NO-UNDO.   /* Дата остатка. */

   DEF BUFFER acct-qty FOR acct-qty. /* Локализация буфера. */
                        /* Поиск валютного остатка. */
   FIND LAST acct-qty WHERE
             acct-qty.acct     =  STRING (iHAcct:BUFFER-FIELD("acct")    :BUFFER-VALUE)
       AND   acct-qty.currency =  STRING (iHAcct:BUFFER-FIELD("currency"):BUFFER-VALUE)
       AND   acct-qty.since    <= iDate
   NO-LOCK NO-ERROR.
                        /* Формирование остатка. */
   IF AVAIL acct-qty
   THEN ASSIGN
      oPos  = acct-qty.qty
      oDate = acct-qty.since
   .
   ELSE ASSIGN
      oPos  = ?
      oDate = ?
   .
   RETURN.
END PROCEDURE.

/* Процедура пересчет ключа счета.
** Требуется заполненный массив toklen. */
PROCEDURE RecalcKey:
   DEF INPUT  PARAM iClassCode   AS CHAR   NO-UNDO. /* Сласс объекта. */
   DEF INPUT  PARAM iBalAcct     AS INT64    NO-UNDO. /* Счет второго порядка. */
   DEF INPUT  PARAM iNumber      AS CHAR   NO-UNDO. /* Номер счета для проверки. */
   DEF OUTPUT PARAM oNumber      AS CHAR   NO-UNDO. /* Переключеваный счет. */

   DEF VAR vKey      AS INT64    NO-UNDO. /* Новый ключ. */
   DEF VAR vKeyOff   AS CHAR   NO-UNDO. /* Маска балансовых счетов для которых не вычисляется ключ. */
   DEF VAR vKeyOn    AS CHAR   NO-UNDO. /* Включать ли ключ в счет. */
   DEF VAR vKeyProg  AS CHAR   NO-UNDO. /* Программа расчета ключа. */
   DEF VAR vMask     AS CHAR   NO-UNDO. /* Маска счета */

   ASSIGN
      vKeyOff  = FGetSetting ("КлючаНет", ?, "")
      vKeyOn   = FGetSetting ("Ключ",     ?, "")
      oNumber  = iNumber
   .
                        /* Получение процедуры пересчета ключа счета. */
   RUN ACGetKeyProg (iClassCode, OUTPUT vKeyProg).
   /* Commented by Malik При вызове из внешней процедуры возникают случаи, когда маска счёта не заполнена, заполняем сами */
   vMask = GetXattrInit(iClassCode, "acct").
   RUN FillAcctMask (INPUT vMask) NO-ERROR.

   IF  toklen[{&TOK_KEY_IDX}]  > 0
   AND vKeyOn                 <> ""
   AND NOT CAN-DO(vKeyOff,STRING(iBalAcct))
   THEN DO:
      IF SearchPfile (vKeyProg) =  ?
      THEN DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "1",
            "Не могу запустить процедуру расчета ключа " + keyprog + " - процедура не найдена."
         ).
         RETURN.
      END.
      RUN VALUE (vKeyProg + ".p") (oNumber, vKeyOn, OUTPUT vKey).
      IF vKey <> ?
          THEN RUN SetAcct (INPUT-OUTPUT oNumber, {&TOK_KEY}, STRING(vkey, FILL ("9", toklen[{&TOK_KEY_IDX}]))).
   END.
   RETURN.
END PROCEDURE.

/* Проверка поля User-Id при создании / редактировании счета. */
PROCEDURE ChkUpdUser-Id.
   DEF INPUT  PARAM iUserId AS CHAR   NO-UNDO. /* Код пользователя. */

   DEF VAR vSlaves   AS CHAR   NO-UNDO. /* Список подчиненных. */
   DEF VAR vErr      AS CHAR   NO-UNDO. /* Текст ошибки. */
                        /* Проверка осуществляется если установлено ограничение
                        ** на просмотр по сотрудникам. */
   IF getThisUserXAttrValue("ПросмотрСотр") =  "Да"
   THEN DO:
      vSlaves = getSlaves () + "," + USERID ("bisquit").
      IF NOT CAN-DO (vSlaves, iUserId)
         THEN vErr = "Сотрудник ~"" + iUserId + "~" не является вашим подчиненным.".
   END.
   RETURN vErr.
END PROCEDURE.
/* $LINTFILE='pp-sacct-api-bs.i' */
/* $LINTMODE='1,2,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='29/06/2017 11:21:34.460+03:00' */
/*prosignLqeZ7Twt0CXuo2xoFfAi3A*/