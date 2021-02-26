/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: DOG.P
      Comment: Инструменты для печати договоров.
   Parameters:
         Uses:
      Used by:
      Created: ??/??/????
     Modified: 23/01/2001 Om    Ошибка: Доработан блок "КЛ".
     Modified: 02/08/2006 54505 - знаки после формулы переносятся на другую строку.
     Modified: 08/08/2006 ZIAL (0058216) 54505 - знаки после формулы переносятся на другую строку.
     Modified: 09/08/2006 ZIAL (0058216) 54505 - знаки после формулы переносятся на другую строку.
     Modified: 17/04/2008 jadv (0078661)
     Modified: 17/04/2008 jadv (0078661)
     Modified: 20/12/2010 ches % - проценты , = - рубли
     Modified: 16/02/2011 priv анализ наличия формата в параметрах.
                          Если есть формат, то обязательно mFlFormat присвоить TRUE
*/

/*
	aa4 -  добавлен тэг %Кред|чсп - выводит %% прописью 18.11.2013
	aa4 -  добавлены тэги: Дсогл, Дгаш1, Дгаш2 - день начисления %% прописью для Авто+.
*/

FORM "~n@(#) dog.p 1.0 ??? ??/??/???? Om 23/01/2001"
WITH frame sccs-id stream-io width 250.

DEFINE VAR vClass    AS CHARACTER NO-UNDO.
DEFINE VAR vAcct-cat AS CHARACTER NO-UNDO.

{globals.i}             /* Глобальные переменные сессии. */
{svarloan.def}          /* Shared переменные модуля "Кредиты и депозиты". */
{client.i}              /* Формирование имени клиента */
{norm.i}
{def_work.i new}        /* Объявление переменных для работы с
                         начислением процентов. */
{sh-defs.i}             /* Переменные для рассчета остатка на счете. */
{amtsclon.i}            /* Добавлять слово "проценты в нужном склонении" */
{intrface.get comm}     /* Инструменты для работы с комиссиями. */
{intrface.get loan}     /* Инструменты для работы с кредитами.  */
{intrface.get xclass}   /* Инструменты для работы с метасхемой.  */
{intrface.get acct}     /* Библиотека для работы со счетами.    */
{intrface.get tmess}    /* Инструменты обработки сообщений.     */
{intrface.get date}
{intrface.get db2l}
{intrface.get cust}
{intrface.get i254}
{intrface.get crole}
{intrface.get tmcod}
{intrface.get refer}    /* Библиотека службы справочников. */
/*{wordwrap.def}*/
{pp-uni.var &FILE_sword_p=YES}
{pp-uni.prg}
{dtterm.i}
{loan.pro}     /* Подключение инструментов для расчета параметров договора */
{t-otch.i NEW}
{repinfo.i}
DEFINE TEMP-TABLE ttIndicate2 LIKE ttIndicate.
DEFINE TEMP-TABLE ttIndicate3 LIKE ttIndicate.

&GLOB  Rates "%Кред,%КрПр,Пеня-К,%КрПр%,Пеня%К,НеиспК,%КрКом,%КрПов,~
%Деп,%ДeПр,Пеня-Д,%ДеПр%,Пеня%Д,НеиспД,%ДпКом,%ДпПов,ОбязКОб,%Выд,%ВыдСКред,~
%ОткрЛим"

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"

DEF OUTPUT PARAM Xresult AS DECIMAL   NO-UNDO.
DEF INPUT  PARAM Xdate1  AS DATE      NO-UNDO.
DEF INPUT  PARAM Xdate   AS DATE      NO-UNDO.
DEF INPUT  PARAM strpar  AS CHARACTER NO-UNDO.

DEF VAR rproc         AS CHAR FORMAT "x(120)".
DEF VAR rproc1        AS CHAR FORMAT "x(70)".
DEF VAR per           AS CHAR INITIAL "".
DEF VAR per2          AS CHAR INITIAL "".
DEF VAR dayinc        AS INT64.
DEF VAR fl            AS LOG  INITIAL YES.
DEFINE VARIABLE  c1   AS CHARACTER.
DEF VAR c2            AS CHAR.
DEF VAR c3            AS CHAR.
DEF VAR vPrmDec       AS DEC   NO-UNDO. /* Остаток по параметру */
DEF VAR vDbSumDec     AS DEC   NO-UNDO. /* Дебетовые обороты по параметру */
DEF VAR vCrSumDec     AS DEC   NO-UNDO. /* Кредитовые обороты по параметру */
DEF VAR vSrcChar      AS CHAR  NO-UNDO. /* Временная строка для обработки. */
DEF VAR vCountInt     AS INT64   NO-UNDO. /* Счетчик. */
DEF VAR vTrmSurChar   AS CHAR  NO-UNDO. /* Суррогат по таблице term-obl */
DEF VAR vNameChar     AS CHAR  NO-UNDO. /* Наименование ДР */
DEF VAR vXattrChar    AS CHAR  NO-UNDO. /* Код ДР */
DEF VAR mCommRate     AS DEC       NO-UNDO. /* Значение % ставки */
DEF VAR mRateFixed    AS LOGICAL NO-UNDO . /*  Тип % ставки      */
DEF VAR mTmpStr       AS CHAR      NO-UNDO. /* Временная строка */
DEF VAR vAmtStr       AS CHAR      NO-UNDO. /* Строка для прописи*/
DEF VAR vDecStr       AS CHAR      NO-UNDO. /* Строка для прописи*/
DEF VAR vProp         AS CHAR      NO-UNDO. /* Признак прописи */
DEF VAR vData         AS DATE          NO-UNDO. /* Для График|* */
DEF VAR vData1        AS DATE          NO-UNDO. /* Для График|* */
DEF VAR vIntTmp       AS INT64           NO-UNDO. /* Для ЧАдрес|* */
DEF VAR vTmpRec       AS RECID         NO-UNDO. /* Временная перем. с Recid'ом */
DEF VAR vCount        AS INT64           NO-UNDO. /* Номер режима Графика */
DEF VAR vTmpDate      AS DATE          NO-UNDO. /* Временная дата */
DEF VAR vTmpDec       AS DEC           NO-UNDO. /* Значение параметра Резерв */
DEF VAR vNum          AS INT64           NO-UNDO. /* Количество полных лет договора. Ф-ия "Срок" */
DEF VAR vMesNum       AS INT64           NO-UNDO. /* Количество полных месяцев договора. Ф-ия "Срок" */
DEF VAR vDayNum       AS INT64           NO-UNDO. /* Количество дней договора. Ф-ия "Срок" */
DEF VAR vSurr         AS CHAR          NO-UNDO.
DEF VAR vMes          AS CHAR EXTENT 50 NO-UNDO. /* Строчка. Используется в неск. ф-ях */
DEF VAR vSymm         AS DEC  EXTENT 3 NO-UNDO. /* Для График|* */
DEF VAR vAvrgDayInMon AS DEC NO-UNDO.
DEF VAR vFormat       AS CHAR NO-UNDO. /* Формат вывода */
DEF VAR vValue        AS CHAR NO-UNDO.
DEF VAR pick-value    AS CHAR NO-UNDO.
DEF VAR strrole       AS CHAR NO-UNDO.
DEF VAR vDateSumm     AS DATE NO-UNDO. /* Дата от которой считать сумму в функции СуммаЧс */
DEF VAR vName         AS CHAR NO-UNDO EXTENT 2. /* Используется для формирования наименования счета */
DEF VAR logTrim       AS LOG  NO-UNDO INIT NO. /* Признак - надо делать trim или нет. Для всех функций. */
DEF VAR chTrim        AS CHAR NO-UNDO. /* Используется в trim */
DEF VAR iIndex        AS INT64  NO-UNDO. /* Используется в trim */
DEF VAR mGrRiska      AS INT64  NO-UNDO.
DEF VAR vRateCode     AS CHAR NO-UNDO. /* Используется в функциях по плавающим ставкам */
DEF VAR vNextDate     AS DATE NO-UNDO. /* Используется в функциях по плавающим ставкам */
DEF VAR vRateSumm     AS DEC  NO-UNDO. /* Используется в функциях по плавающим ставкам */
DEF VAR vDRCode       AS CHAR  NO-UNDO. /* Код ДР */
DEF VAR vI            AS INT64  NO-UNDO.
DEF VAR vTotAmt       AS DEC  NO-UNDO. /* Для функции ПрмОСумм */
DEF VAR CodOstPar     AS INT64  NO-UNDO.
DEF VAR vProcString   AS CHAR NO-UNDO.
DEF VAR mBal2-acct    AS CHAR  NO-UNDO.
DEF VAR mTmp-acct     AS CHAR  INITIAL "" NO-UNDO.
DEF VAR mInpDate      AS DATE  NO-UNDO.  /* Произвольная дата */
DEF VAR mInpSumm      AS DEC   NO-UNDO.  /* Произвольная сумма */
DEF VAR mRatesLst     AS CHAR NO-UNDO.  /* Список ставок, на замену Rates */
DEF VAR mContract     AS CHAR  NO-UNDO.
DEF VAR mContCode     AS CHAR  NO-UNDO.
DEF VAR mFindTech     AS LOG   NO-UNDO.
DEF VAR mNotFound     AS LOG   NO-UNDO.
DEF VAR mStrPar       AS CHAR  NO-UNDO.
DEF VAR mFlFormat     AS LOG   NO-UNDO INIT FALSE. /* Признак наличия формата в параметре */
DEF VAR vReg          AS CHAR  NO-UNDO.
DEF VAR vRegName      AS CHAR  NO-UNDO.
DEF VAR mOldAdr       AS LOG   NO-UNDO.
DEF VAR mPrefAdr      AS CHAR  NO-UNDO.
DEF VAR mSepAdr       AS CHAR  NO-UNDO.
DEF VAR mFlDot        AS LOG   NO-UNDO INIT FALSE. /* Использовалась ли точка в адресе */
DEF VAR mStatCode     AS CHAR  NO-UNDO.
DEF VAR vBag          AS CHAR  NO-UNDO.
DEF VAR mGrTyp        AS INT64 NO-UNDO.
DEF VAR mFlOst        AS LOG   NO-UNDO.
DEF VAR mFirstPay     AS DEC   NO-UNDO.
DEF VAR mFormGrKom    AS CHAR  NO-UNDO.
DEF VAR months2       AS CHAR  INITIAL "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря" NO-UNDO.
DEF VAR dat           AS DATE          NO-UNDO.
DEF VAR mCust         AS CHAR NO-UNDO.
DEF VAR soan		  AS CHAR NO-UNDO.
DEF VAR bikr  		  AS CHAR NO-UNDO.
DEF VAR acctr 		  AS CHAR NO-UNDO.
DEF VAR namer 		  AS CHAR NO-UNDO.
DEF VAR acctk 		  AS CHAR NO-UNDO.
DEF VAR nameb 		  AS CHAR NO-UNDO.
DEF VAR vVnut 		  AS LOG   INIT FALSE NO-UNDO.
DEFINE VARIABLE hBufFld AS HANDLE     NO-UNDO.

/* cda */
DEF VAR vNomDog       AS CHAR  NO-UNDO.
DEF VAR vVidOb        AS CHAR  NO-UNDO.
DEF VAR vVidDogOb     AS CHAR  NO-UNDO.
DEF VAR vCodeVal      AS CHAR  NO-UNDO.
DEF VAR vFrameTitle   AS CHAR  NO-UNDO.
DEF NEW GLOBAL SHARED VAR lgar_recid    AS RECID NO-UNDO.

DEFINE TEMP-TABLE tt NO-UNDO
   FIELD NomDog   AS CHAR
   FIELD CodeVal  AS CHAR
   FIELD NomPP    AS INT
   FIELD ChVal    AS CHAR
   FIELD term-obl-id AS RECID.
   
DEFINE QUERY q_term FOR tt.
DEFINE BROWSE b_term QUERY q_term NO-LOCK 
DISPLAY
   tt.NomPP   COLUMN-LABEL "#"               FORMAT 99
   tt.NomDog  COLUMN-LABEL "НОМЕР ДОГОВОРА"  FORMAT "x(20)" 
   tt.CodeVal COLUMN-LABEL "ВИД"             FORMAT "x(45)"
/*   tt.ChVal   COLUMN-LABEL "ПОДСТАНОВКА"     FORMAT "x(45)" */
   WITH 5 DOWN WIDTH 73 TITLE "".

DEFINE FRAME f_term 
   b_term SKIP 
   WITH 1 COLUMN SIDE-LABELS ROW 5 CENTERED OVERLAY NO-BOX.
/* */

DEF BUFFER bloan-cond      FOR loan-cond. /* Используется в суммачс для корректного определения даты */
DEF BUFFER bloan           FOR loan.
DEF BUFFER bcust-corp      FOR cust-corp.
DEF BUFFER bterm-obl       FOR term-obl.
DEF BUFFER xloan           FOR loan-cond.
DEF BUFFER buf-cust-ident  FOR cust-ident.
DEF BUFFER bcode           FOR code.
DEF BUFFER ploan		   FOR loan.
DEF BUFFER bloan-acct      FOR loan-acct.

DEF SHARED STREAM fil.

DEF TEMP-TABLE obl NO-UNDO
    FIELD     end-date LIKE term-obl.end-date
    FIELD     amt      LIKE term-obl.amt
    FIELD     amt2     LIKE term-obl.amt
    FIELD     amt3     AS   DECIMAL
    .

FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) FORWARD.

/*-------------------------------------------------------------------------
  Процедура для определения балансового счета 2-го порядка для ссудного счета.
  Номер балансового счета определяется на основании данных договора.
  -------------------------------------------------------------------------*/
PROCEDURE GetBalAcctFromLoan:

   DEFINE VAR vTerm         AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR DTType        AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR DTKind        AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR DTTerm        AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR DTCust        AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR mask          AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR mask-internal AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR s             AS CHAR    INITIAL "" NO-UNDO.
   DEFINE VAR yy            AS INT64     NO-UNDO.
   DEFINE VAR dd            AS INT64     NO-UNDO.

	pick-value = ?.
   mBal2-acct  = ?.
   RUN GetDBITerm(loan.open-date,
                  loan.end-date,
   					loan.contract,
	   				loan.cust-cat,
						OUTPUT vTerm).

	RUN DTCust(loan.cust-cat, loan.cust-id, ?, OUTPUT DtCust).

	ASSIGN
	   DTType = GetXAttrInit(loan.class, "DTType")
	   DTKind = GetXAttrInit(loan.class, "DTKind")
	   DTTerm = ENTRY(3, vTerm,"/").

	IF DTType = ? OR DTType = "" THEN DTType = "*".
	IF DTKind = ? OR DTKind = "" THEN DTKind = "*".
	IF DTTerm = ? OR DTTerm = "" THEN DTTerm = "*".

   FIND FIRST code WHERE code.class = "ТипСчДог"
                     AND code.code  = ENTRY(2, strpar, ",")
      NO-LOCK NO-ERROR.
   IF NOT AVAIL code THEN
      MESSAGE "Не найден код счета " + code.code VIEW-AS ALERT-BOX.

	ASSIGN
		mask-internal = code.code + CHR(1) +
		                DTType + CHR(1) +
		                DTCust + CHR(1) +
		                DTKind + CHR(1) +
		                DTTerm
		s = "".

	FOR EACH code WHERE code.class = "DTTerm" AND code.parent = "DTTerm"
      NO-LOCK:
	   IF IS-Term(loan.open-date,
	              (IF loan.end-date = ? THEN
	                  12/31/9999
	               ELSE
	                  loan.end-date),
	               code.code,
	               NO,
	               0,
	               OUTPUT yy,
	               OUTPUT dd)
	   THEN
	      {additem.i s code.code}

	END. /*FOR*/

	ASSIGN
	   ENTRY(5,mask-internal,CHR(1)) = s
	   mask = mask-internal
      .

   RUN cbracct.p("DecisionTable", "DecisionTable", "DecisionTable", -1).

   IF pick-value <> ? AND pick-value <> "" AND
      CAN-FIND( bal-acct WHERE bal-acct.bal-acct = INT64(TRIM(pick-value)) )
   THEN
   DO:
      mBal2-acct = TRIM(pick-value).
      RUN SetSysConf IN h_base ("БАЛАНСОВЫЙ СЧЕТ", mBal2-acct).
   END.

END PROCEDURE.

FUNCTION TitleCase RETURNS CHARACTER
  (pcText AS CHARACTER) :

  DEFINE VARIABLE i           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cWord       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iLast       AS INTEGER     NO-UNDO.

  DEFINE VARIABLE cSmallWords AS CHARACTER   NO-UNDO
     INITIAL "на,область,край,автономная,г".

  pcText = REPLACE(LC(pcText),"-"," - ").
  iLast = NUM-ENTRIES(pcText, " ").
  DO i = 1 TO iLast:
    cWord = ENTRY(i, pcText, " ").
    IF LENGTH(cWord) > 0 THEN
      IF LOOKUP(cWord, "АО") = 0 THEN
      IF LOOKUP(cWord, cSmallWords) = 0 THEN
      ENTRY(i, pcText, " ") = CAPS(SUBSTRING(cWord, 1, 1)) + LC(SUBSTRING(cWord, 2)).
    ELSE ENTRY(i, pcText, " ") = LC(cWord).
    /*pda исправлена ошибка в названии региона "NameCity г"*/
    IF i = iLast AND ENTRY(i, pcText, " ") = "г" THEN DO:
      pcText = REPLACE(pcText, " г", "").
      pcText = "г. " + pcText.
    END.
  END.
  IF ENTRY(1, pcText, " ") = "г" THEN
    pcText = "г." + SUBSTRING(pcText,2).
  RETURN REPLACE(pcText," - ","-").
END FUNCTION.

/*-------------------------------------------------------------------------
  Процедура для определения лицевого счета по учету резерва.
  Номер счета определяется на основании данных договора.
  -------------------------------------------------------------------------*/
PROCEDURE GetAcctFromLoan:

   DEFINE VAR iBranch  AS CHAR INITIAL "" NO-UNDO.
   DEFINE BUFFER bacct FOR acct.
   iBranch = loan.branch-id.
	IF iBranch = "" THEN
      iBranch = TRIM(GetUserBranchId(USERID("bisquit"))).

   mBal2-acct = GetSysConf("БАЛАНСОВЫЙ СЧЕТ").

   IF mBal2-acct = "" OR mBal2-acct = ? THEN
      RUN GetBalAcctFromLoan.

   FIND LAST bal-acct WHERE
             bal-acct.acct-cat EQ "b"
         AND TRIM(bal-acct.bal-acct1) EQ SUBSTR( TRIM(STRING(mBal2-acct)), 1, 3 )
         AND bal-acct.side EQ "П"
   NO-LOCK NO-ERROR.

   IF AVAILABLE bal-acct THEN
      mBal2-acct = TRIM(STRING(bal-acct.bal-acct)).
      vClass = "ACCT".

      RELEASE acct NO-ERROR.

      RUN Cm_acct_cr IN h_acct (
             vClass,               /* iClass                  */
             INT64(mBal2-acct),      /* iBal                    */
             loan.currency,        /* iCurr                   */
             loan.cust-cat,        /* iCustCat                */
             loan.cust-id,         /* iCustID                 */
             Xdate,                /* iOpenDate               */
             OUTPUT mTmp-acct,     /* oAcct                   */
             BUFFER acct,           /* BUFFER iacct FOR acct . */
             ?,                     /* iAcctMask               */
             ?,                     /* iDetails                */
             bal-acct.kau-id,       /* iKauId                  */
             loan.contract,         /* iContract               */
             USERID ('bisquit'),    /* iUserId                 */
             iBranch,               /* iBranchId               */
             YES                    /* iCopyBalXattr           */
      ) NO-ERROR.

   IF ERROR-STATUS:ERROR OR mTmp-acct  EQ ? THEN DO:
      MESSAGE "Ошибка при вычислении номера счета ! " + RETURN-VALUE.
   END.
   ELSE IF AVAILABLE acct THEN DO:
      FIND FIRST bacct WHERE RECID(bacct) = RECID(acct)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      {was-err.i}
      DELETE bacct NO-ERROR.
      {was-err.i}
   END.

END PROCEDURE.
/* Получение суммы прописью одним из двух способов: скобки вначале и конце; скобки вначале и после валюты */
PROCEDURE FrmtAmt:
   DEF INPUT  PARAM iAmt      AS DEC   NO-UNDO.
   DEF INPUT  PARAM iCurrency AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iKindAmt  AS INT64 NO-UNDO.
   DEF OUTPUT PARAM oStramt   AS CHAR  NO-UNDO.

   DEF VAR vAmtStr AS CHAR  NO-UNDO.
   DEF VAR vDecStr AS CHAR  NO-UNDO.
   DEF VAR vCnt1   AS INT64 NO-UNDO.
   DEF VAR vCnt2   AS INT64 NO-UNDO.

   RUN x-amtstr.p (iAmt, iCurrency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
   IF iKindAmt EQ 2 THEN
   DO:
      oStramt = TRIM(STRING(iAmt, ">>>,>>>,>>>,>>9.99")) + " (" + vAmtStr + " " + vDecStr.
      DO vCnt2 = 0 TO NUM-ENTRIES(oStramt," ") - 1:
         IF ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") NE "" THEN
            vCnt1 = vCnt1 + 1.
            IF vCnt1 EQ 4 THEN
            DO:
               ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") = ENTRY(NUM-ENTRIES(oStramt," ") - vCnt2, oStramt, " ") + ")".
               LEAVE.
            END.
      END.
   END.
   ELSE
      oStramt = TRIM(STRING(iAmt, ">>>,>>>,>>>,>>9.99")) + " (" + vAmtStr + " " + vDecStr + ")".
END PROCEDURE.
/*-------------------------------------------------------------------------*/
/* Для функции Trim - урезание не значащих пробелов */
FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) :
   CASE FormatTrim:
      WHEN "trim"  THEN cValue = TRIM(cValue).
      WHEN "ltrim" THEN cValue = LEFT-TRIM(cValue).
      WHEN "rtrim" THEN cValue = RIGHT-TRIM(cValue).
   END CASE.
   RETURN cValue.
END.

/* =========================================-==-===
   ** Возвращает реквизит из "сложного" списка
   ** комиссия1=вид оп.1,комиссия2=вид оп.2  и т.д. */
FUNCTION GetParsSett RETURNS CHAR
  (INPUT iPar AS INT64,         /* 1 Комиссия, 2 Вид опер. */
   INPUT iQty  AS INT64,        /* Порядковый номер в строке iStr */
   INPUT iStr AS CHAR).       /* Строка со списком */
   RETURN ENTRY(iPar, ENTRY(iQty, iStr), "=").
END FUNCTION.

IF rid-p = ? THEN DO:
   RUN fill-sysmes IN h_tmess ("","","0","Печать шаблонов возможна только из меню печати по конкретному договору (F1 на договоре)").
   RETURN.
END.

/* возвращает число в родительном падеже для фразы числа месяца */
FUNCTION amt-r RETURNS CHAR
  (input Amt as int).

	DEF VAR   AmtStr as char.
	def var decstr as char.

	def var ones as char extent 21 no-undo initial
	['первого ','второго ','третьего ','четвертого ','пятого ','шестого ','седьмого ','восьмого ','девятого ','десятого ',
 	'одиннадцатого ','двенадцатого ','тринадцатого ','четырнадцатого ','пятнадцатого ','шестнадцатого ','семнадцатого ',
 	'восемнадцатого ','девятнадцатого ','двадцатого '].

	IF Amt=0
		THEN AmtStr = "ноль ".
		ELSE
	IF Amt<21
		THEN AmtStr = ones[Amt].
		ELSE
	IF Amt<30
		THEN AmtStr = 'двадцать ' + ones[Amt - 20].
		ELSE
	IF Amt=30
		THEN AmtStr = 'тридцатого '.
		ELSE
	IF Amt<40
		THEN AmtStr = 'тридцать ' + ones[Amt - 30].
		ELSE AmtStr = STRING(Amt).
	RETURN AmtStr.
END.

FUNCTION GetDateStr RETURN CHARACTER PRIVATE(INPUT iDate AS DATE):
   IF iDate = ? THEN
      RETURN ?.
   RETURN (STRING(DAY(iDate),"99") + " " + ENTRY(MONTH(iDate),"января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря") + " " + STRING(YEAR(iDate)) + " г.").
END FUNCTION.

DO TRANSACTION ON ENDKEY UNDO, LEAVE
               ON ERROR  UNDO, LEAVE:

   mRatesLst = {&Rates}.
   FOR EACH setting WHERE
            setting.code     EQ "РазрешДопКом"
      AND   setting.module   EQ "loan"
      AND   setting.sub-code NE ""
   NO-LOCK:

      {additem.i mRatesLst setting.val}
   END.

   FIND loan WHERE
        RECID(loan) EQ rid-p
      NO-LOCK NO-ERROR.

   FIND FIRST loan-cond WHERE
        RECID(loan-cond) = rid-t
   NO-LOCK NO-ERROR.

   per  = ENTRY(1,strpar).
   
      /* если код тега начинается с ОхватДог,
      ** то всё тянем с охватывающего договора */
   IF per BEGINS "ОхватДог|" THEN
   DO:
      per = SUBSTRING(per, INDEX(per, "|") + 1).
      IF NUM-ENTRIES (loan.cont-code, " ") GT 1 THEN
      DO:
         ASSIGN
            mContract = loan.contract
            mContCode = ENTRY (1, loan.cont-code, " ")
         .
         FIND FIRST loan WHERE
                    loan.contract  EQ mContract
                AND loan.cont-code EQ mContCode
         NO-LOCK NO-ERROR.
         FIND LAST loan-cond WHERE
                   loan-cond.contract  EQ mContract
               AND loan-cond.cont-code EQ mContCode
               AND loan-cond.since     LE Xdate
         NO-LOCK NO-ERROR.
      END.
   END.

      /* если код тега начинается с ИлиОхватДог,
      ** то устанавливаем флаг и при неудачном поиске на текущем договоре,
      ** переискиваем на охватывающем */
   IF per BEGINS "ИлиОхватДог|" THEN
   DO:
      ASSIGN
         per       = SUBSTRING(per, INDEX(per, "|") + 1)
         mStrPar   = per
         mFindTech = YES
      .
   END.

   strrole = GetAggrCustRole(loan.class-code) .

   printres = NO. /*не выводить значение xrezult */

   mTmpStr = FGetSetting("БанкМФО",?,?).

   IF mTmpStr = ? THEN
   DO:
      MESSAGE "Нет информации о вашем банке".
      RETURN.
   END.

   FIND FIRST banks-code WHERE
              banks-code.bank-code-type EQ "МФО-9"
          AND banks-code.bank-code      EQ mTmpStr
      NO-LOCK.
   FIND banks OF banks-code
      NO-LOCK NO-ERROR.

   IF per MATCHES "*trim*" THEN
   DO:
      logTrim = YES.
      iIndex  = INDEX(strpar,",").
      chTrim  = SUBSTRING(strpar,1,iIndex - 1).
      per     = SUBSTRING(strpar,iIndex + 1,LENGTH(strpar) - iIndex).
   END.

   FORM
      a      AS CHARACTER FORMAT "x(70)" SKIP
      rproc1                             SKIP
      c      AS CHARACTER FORMAT "x(70)" SKIP
      rproc               FORMAT "x(70)" SKIP
      WITH FRAME q2 OVERLAY NO-LABELS ROW 3 COLUMNS 5
      TITLE "Реквизиты ответственного лица".
/* СВЯЗАННЫЕ СУБЪЕКТЫ */
    IF CAN-DO(strrole,per) THEN DO :
        FOR EACH cust-role WHERE
         cust-role.file-name     = 'loan'
         AND cust-role.surrogate = loan.contract + ',' + loan.cont-code
         AND cust-role.class-code = per
         AND cust-role.close-date = ?
         NO-LOCK :
           vMes[1] = cust-role.cust-name.
           {wordwrap.i
                    &s = vMes
                    &n = 50
                    &l = 80
                }
           DO vNum = 1 TO 50:
              IF vMes[vNum] = "" THEN
                     LEAVE.
              rproc = rproc
                    + (IF logTrim
                       THEN TrimFormula(chTrim,vMes[vNum])
                       ELSE vMes[vNum])
                    + (IF    vNum           EQ 50
                          OR vMes[vNum + 1] EQ ""
                       THEN ""
                       ELSE "\n").

           END.

       END.
   END.

   /* находим дату первого платежа по графику, после создания заявки на досрочное погашение ссуды */
   IF per = "firstPlat" 
   THEN DO:
   def var dateTermObl as date no-undo init ?.
   def var dateInsertTermObl as date no-undo init ?.
   def var termOblNN as char no-undo.
   def var termOblStatus as char no-undo.
	/* 
   find first term-obl where term-obl.cont-code EQ loan.cont-code and
	term-obl.contract EQ 'Кредит' and term-obl.idnt EQ 300 no-lock no-error.
	*/
   for each term-obl where term-obl.cont-code EQ loan.cont-code and
	term-obl.contract EQ 'Кредит' and term-obl.idnt EQ 300 by term-obl.end-date DESCENDING:
	termOblStatus = getxattrvalue("term-obl",loan.contract + ',' + loan.cont-code + ',300,' + string(term-obl.end-date, "99/99/99") + ',' + string(term-obl.nn),"term-obl-status").
	if termOblStatus = "ВВЕД" then do:
		dateTermObl = term-obl.end-date.
		termOblNN = string(term-obl.nn).
		leave.
	end.
   end.
   if dateTermObl <> ? then do:
		find first history where history.file-name = "term-obl"
			and	history.modify = 'C' 
			and history.field-ref = loan.contract + ',' + loan.cont-code + ',300,' + string(dateTermObl, "99/99/99") + ',' + termOblNN no-lock no-error.
		if avail history then do:
			dateInsertTermObl = history.modif-date.
			FOR EACH term-obl WHERE term-obl.cont-code EQ loan.cont-code AND
			term-obl.contract EQ 'Кредит' AND term-obl.idnt EQ 3 AND
			term-obl.end-date >= dateInsertTermObl NO-LOCK
			BY term-obl.end-date:
				dateTermObl = term-obl.end-date.
				leave.
			end.
		end.
		rproc = string(dateTermObl, "99.99.9999"). 
   end.
   END.

   /* Код группы риска */
   IF per BEGINS "Риск"
   THEN DO:
      mGrRiska = re_history_risk(loan.contract, loan.cont-code, loan.since, 1).
      IF per EQ "Риск"
      THEN DO:
         rproc = STRING(mGrRiska).
      END.
      /* Код группы риска -полный формат */
      IF per EQ "Риск|р" THEN
      DO:
         CASE mGrRiska:
            WHEN 1 THEN rproc = STRING(mGrRiska) + "гр. - I Категория качества - Стандартные (ссуды, входящие в ПОС II категории качества)".
            WHEN 2 THEN rproc = STRING(mGrRiska) + "гр. - II Категория качества - Нестандартные".
            WHEN 3 THEN rproc = STRING(mGrRiska) + "гр. - III Категория качества - Сомнительные".
            WHEN 4 THEN rproc = STRING(mGrRiska) + "гр. - IV Категория качества - Проблемные".
            WHEN 5 THEN rproc = STRING(mGrRiska) + "гр. - V Категория качества - Безнадежные".
         END CASE.
      END.
      /* Код группы риска -полный формат + поправка на ПОС */
      IF per EQ "Риск|ПОС"
      THEN DO:
         /* Если договор портфельный, то он в 1й группе риска */
         ASSIGN
           rproc    = substitute("&1 - (&2)" , mGrRiska , GetCodeName("Резерв",string(mGrRiska) + "гр"))   /* 1гр - I категория  ... */
           vBag = ""
         .
         IF mGrRiska > 1
         THEN
            vBag = LnInBagOnDate (loan.contract, loan.cont-code, Xdate) .
         IF {assigned vBag} THEN
            mGrRiska = 1  .
         IF mGrRiska = 1 THEN
            rproc    = substitute("&1 - &2 (ссуды, входящие в ПОС II категории качества)" , mGrRiska , GetCodeName("Резерв", string(mGrRiska) + "гр")) .  /* 1гр - I категория  ... */
      END.
   END.


   /* Получение значения %% ставки */
   IF CAN-DO(mRatesLst, ENTRY(1, per, "|")) THEN
   DO:
      mCommRate = GET_COMM (ENTRY(1, per, "|"),                   /* Код комиссии */
                            ?,                                    /* RecId счета*/
                            loan.currency,                        /* Валюта*/
                            loan.contract + "," + loan.cont-code, /* Кау*/
                            0.00,                                 /* MIN остаток */
                            0,                                    /* Период */
                            Xdate).                               /* Дата */

      mRateFixed = GET_COMM_TYPE (ENTRY(1, per, "|"),                   /* Код комиссии */
                            ?,                                    /* RecId счета*/
                            loan.currency,                        /* Валюта*/
                            loan.contract + "," + loan.cont-code, /* Кау*/
                            0.00,                                 /* MIN остаток */
                            0,                                    /* Период */
                            Xdate).                               /* Дата */
							
      IF mCommRate NE ? THEN
      DO:
         IF     NUM-ENTRIES(per, "|") GE 2
            AND    ENTRY(2, per, "|") EQ "чс" THEN
            IF  NUM-ENTRIES(per, "|") EQ 2 THEN
               rproc = STRING(mCommRate, ">>99.99999").
            ELSE /* Задан формат */
            DO:
               vFormat = ENTRY(3, per, '|').
               rproc   = STRING(mCommRate, vFormat).
               IF NUM-ENTRIES(per, "|") EQ 4 THEN
                  rproc = rproc + ENTRY(4, per, '|').
            END.
         ELSE IF     NUM-ENTRIES(per, "|") GE 2                                 /* aa4 - ставка прописью */
         AND    ENTRY(2, per, "|") EQ "чсп" THEN DO:
				 RUN x-amtstr.p (mCommRate,"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
				 IF vDecStr EQ "00" THEN
					rproc = STRING(mCommRate, ">>99") + " ( " + STRING(vAmtStr) + ")".
				 ELSE
				 DO:
					rproc = STRING(mCommRate, ">>99.99")  + " (" + STRING(vAmtStr) + "целых ".
					RUN x-amtstr.p (DEC(vDecStr),"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
					rproc = rproc + LC(STRING(vAmtStr)) + "сотых)".
				 END.
				 rproc= REPLACE(rproc,".",",").
		 END.	 
         ELSE
         DO:
            vFormat = (IF NUM-ENTRIES(per, "|") GE 3 THEN
                          ENTRY(3, per, "|")
                       ELSE ">>99.99999").
            RUN amt.p (mCommRate,
                       OUTPUT rproc).
            rproc = STRING(mCommRate, vFormat) + " ( " + rproc + ")".
            RUN persent (mCommRate, loan.currency , mRateFixed , INPUT-OUTPUT rproc).
         END.
      END.
      ELSE
         mNotFound = YES.
   END.

   /* Возвращает день оплаты процентов.
   ** Обрабатывает только ежемесячное начисление процентов. */
   IF per BEGINS "ДО%" THEN
   DO:
      /* Поиск последнего условия действующего на дату end-date */
      RUN RE_L_COND IN h_Loan (loan.contract,
                               loan.cont-code,
                               Xdate,
                               BUFFER loan-cond).

      IF AVAILABLE loan-cond AND
         loan-cond.cred-period EQ "М" THEN DO:

		IF loan-cond.cred-date EQ 31
			THEN rproc = "последний день месяца".
			ELSE DO:
			IF NUM-ENTRIES(per,"|")>1 AND ENTRY(2,per,"|")="Пр"
				THEN rproc = amt-r(loan-cond.cred-date) + " числа ежемесячно.".
                ELSE rproc = STRING(loan-cond.cred-date) + " числа ежемесячно.".
		END.
	  END.
   END.

/*ayv Определение даты последнего изменения ставки*/
   IF per BEGINS "ДатаСмеСт" THEN
   DO:
      mInpSumm = 0.
      FOR EACH comm-rate WHERE comm-rate.kau eq loan.contract + ',' + loan.cont-code
                          AND  comm-rate.commission eq '%Кред'
      NO-LOCK BY comm-rate.since DESC:
        IF mInpSumm EQ 0 THEN DO:
          ASSIGN
            mInpSumm = comm-rate.rate-comm
            mInpDate = comm-rate.since
          .
          rproc = STRING(mInpDate,"99.99.9999").
        END.
        ELSE
          IF mInpSumm EQ comm-rate.rate-comm THEN 
              mInpDate = comm-rate.since.
          ELSE DO:
            rproc = STRING(mInpDate,"99.99.9999").
            LEAVE.
          END.
      END.
   END.

   /*pda Определение ставки на дату последнего изменения ставки*/   
   IF per BEGINS "СмеСт" THEN
   DO:
      mInpSumm = 0.
      FOR EACH comm-rate WHERE comm-rate.kau eq loan.contract + ',' + loan.cont-code
                          AND  comm-rate.commission eq '%Кред'
      NO-LOCK BY comm-rate.since DESC:
        IF mInpSumm EQ 0 THEN 
        DO:
          ASSIGN
            mInpSumm = comm-rate.rate-comm
            mInpDate = comm-rate.since
          .
        END.
        ELSE
          IF mInpSumm EQ comm-rate.rate-comm THEN 
             mInpDate = comm-rate.since.
      END.

      mCommRate = GET_COMM (ENTRY(2, per, "|"),                   /* Код комиссии */
                            ?,                                    /* RecId счета*/
                            loan.currency,                        /* Валюта*/
                            loan.contract + "," + loan.cont-code, /* Кау*/
                            0.00,                                 /* MIN остаток */
                            0,                                    /* Период */
                            mInpDate).                               /* Дата */

      rproc = STRING(mCommRate, ">>99.99").
   END.

   /* ayv Возвращает число дня оплаты процентов.
   ** Обрабатывает только ежемесячное начисление процентов. */
   IF per BEGINS "ЧДО%" THEN
   DO:
      RUN RE_L_COND IN h_Loan (loan.contract,
                               loan.cont-code,
                               Xdate,
                               BUFFER loan-cond).
							   
      IF AVAILABLE loan-cond AND
         loan-cond.cred-period EQ "М" THEN rproc = string(loan-cond.cred-date).
   END.
   
   /*ayv Выводит дни просрочки и размер расчетного резерва для ПОС*/
   IF per MATCHES ("Портфель|*") THEN
   DO:
		FIND FIRST term-obl OF loan WHERE
				   term-obl.idnt = 128
			 NO-LOCK NO-ERROR.
		
		IF AVAILABLE term-obl THEN DO:
			FIND FIRST ploan WHERE
			       	   ploan.cont-code = term-obl.lnk-cont-code
				NO-LOCK NO-ERROR.
			IF ENTRY(2,per,"|") EQ "%" THEN 
				IF AVAILABLE ploan THEN
					IF ploan.cont-code MATCHES "ПФАвто*" THEN
						rproc = "" + STRING(ploan.risk, ">>>9.99").
					ELSE
						rproc = "5.00".
			IF ENTRY(2,per,"|") EQ "ПОССрПрос" THEN
					rproc = GetXAttrValueEx("loan",
											ploan.contract + "," + ploan.cont-code,
											ENTRY(2, per, '|'),
											"").
			IF ENTRY(2,per,"|") EQ "ПОСИмя" THEN
				IF SUBSTRING(ploan.cont-code,1,10) EQ "ПФАвтоНеоб" THEN
					rproc = "Авто Плюс - легковой автотранспорт - необеспеченный".
				ELSE
					IF SUBSTRING(ploan.cont-code,1,6) EQ "ПФАвто" THEN
						rproc = "Авто Плюс - легковой автотранспорт".
					ELSE
						rproc = "2014 - Потребительские кредиты".
			IF ENTRY(2,per,"|") EQ "Кат" THEN
				rproc = "0".
		END.
		ELSE DO:
			IF ENTRY(2,per,"|") EQ "Кат" THEN
				rproc = "1".
		END.
   END.

   IF per = "paz" THEN /*проверка на автозалог+*/
   DO:
     IF SUBSTRING(loan.doc-ref,LENGTH(loan.doc-ref) - 2,3) = "ПАЗ" THEN
	   rproc = "1".
	 ELSE
	   rproc = "0".
   END.
   
   IF per = "strpr" THEN
   DO:
		rproc = "0".
		FOR EACH ploan WHERE
				 ploan.contract = "СТРАХ" AND
				 ploan.parent-cont-code = loan.cont-code NO-LOCK by ploan.open-date :
			IF GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"VidStr","") = "КАСКО_К" THEN
				IF rproc = "2" 
					THEN
						rproc = "3".
					ELSE
						rproc = "1".
			IF GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"VidStr","") = "Жизнь" THEN
				IF rproc = "1" 
					THEN
						rproc = "3".
					ELSE
						rproc = "2".
		END.
   END.
   
   IF per MATCHES ("НДС|*") THEN
   DO:
		IF ENTRY(2,per,"|") = "Прод" THEN
			IF (DEC(GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"rko1_nds","")) NE 0) AND
			   (GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"rko1_nds","") NE "") THEN
				rproc = "В том числе НДС " + GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"rko1_nds","") + " руб.".
			ELSE
				rproc = "Без НДС.".
		ELSE
			FOR EACH ploan WHERE
					 ploan.contract = "СТРАХ" AND
					 ploan.parent-cont-code = loan.cont-code NO-LOCK by ploan.open-date :
				IF AVAIL ploan AND GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"VidStr","") = ENTRY(2,per,"|") THEN
					IF (DEC(GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"nds","")) NE 0) AND
					   (GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"nds","") NE "") THEN DO:
						FIND FIRST term-obl OF ploan WHERE
								   term-obl.idnt     EQ 1
							   AND term-obl.end-date GE ploan.open-date
							NO-LOCK NO-ERROR.
						IF AVAILABLE term-obl THEN DO:
						rproc = "В том числе НДС " + GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"nds","") + " руб.".
						END.
					END.
					ELSE
						rproc = "Без НДС.".
			END.
   END.
   
   /*ayv расчет даты проф.суждения*/
   IF per MATCHES ("ДатаПС") THEN
   DO:
		RUN RE_L_ACCT IN h_Loan(loan.contract,
                                    loan.cont-code,
                                    "КредРасч",
                                    loan.since,
                                    BUFFER loan-acct).
		RUN RE_L_ACCT IN h_Loan(loan.contract,
                                    loan.cont-code,
                                    "Кредит",
                                    loan.since,
                                    BUFFER bloan-acct).
		/*находим два счета и ищем по ним проводку, проверяя код транзакции и тип документа*/
		IF AVAIL loan-acct AND AVAIL bloan-acct THEN DO:
			FOR EACH op-entry WHERE
				     op-entry.acct-cr = loan-acct.acct
				 AND op-entry.acct-db = bloan-acct.acct,
			FIRST op of op-entry WHERE
				  op.op-kind BEGINS 'ВыдАвто+'
			  AND op.doc-type  = '017'
			  AND op.op-status NE 'А'
			NO-LOCK BY op-entry.acct-db BY op-entry.op-date:
				rproc = STRING(op.op-date, "99.99.9999").
			END.
		END.
   END.
   
   /*ayv Данные для распоряжения на выдачу средств Продавцу и Страховому агенту */
   IF per MATCHES ("Распор|*") THEN
   DO:
    IF loan.open-date GE 05/01/15 THEN
		  RUN RE_L_ACCT IN h_Loan(loan.contract,
                                    loan.cont-code,
                                    "КредРасч",
                                    loan.since,
                                    BUFFER loan-acct).
    ELSE DO:
      FIND bloan WHERE bloan.doc-ref EQ loan.doc-ref
                 AND   bloan.contract EQ bloan.contract
                 AND   bloan.filial-id EQ '0400' NO-ERROR.
      IF AVAIL(bloan) THEN
          RUN RE_L_ACCT IN h_Loan(bloan.contract,
                                    bloan.cont-code,
                                    "КредРасч",
                                    bloan.since,
                                    BUFFER loan-acct).
    END. 
		IF AVAILABLE loan-acct THEN DO:
			IF ENTRY(3,per,"|") = "СчРс" THEN
				rproc = STRING(delFilFromAcct(loan-acct.acct)).
		END.
		IF ENTRY(2,per,"|") = "ВыдОпл" THEN
		DO:
			soan = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"rko2_N_DKP","").
			IF soan = "" THEN soan = "Б/Н".
			IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ПолучательРекв","") NE "" THEN DO:
				vValue = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ПолучательРекв","").
				bikr  = ENTRY(1,vValue,"^").
				acctr = ENTRY(2,vValue,"^").
				namer = ENTRY(5,vValue,"^").
				FIND FIRST banks-code WHERE
						   banks-code.bank-code = bikr
					NO-LOCK NO-ERROR.
					IF AVAILABLE banks-code THEN DO:
						FIND FIRST banks WHERE
								   banks.bank-id = banks-code.bank-id
							NO-LOCK NO-ERROR.
						IF AVAIL banks THEN nameb = banks.name.
						FIND FIRST banks-corr WHERE 
								   banks-corr.bank-corr EQ banks.bank-id AND 
								   CAN-FIND (banks OF banks-corr WHERE banks.flag-rkc) 
							NO-LOCK NO-ERROR.
						IF AVAIL banks-corr THEN acctk = banks-corr.corr-acct.
					END.
			END.
			ELSE DO:
				IF GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ПолучСредств","") NE "" THEN
					vValue = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ПолучСредств","").
				ELSE
					vValue = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"Продавец","").
				FIND FIRST code WHERE
						   code.class = "strahpol" AND
						   code.code = vValue
					NO-LOCK NO-ERROR.
				IF AVAIL code THEN DO:
					bikr  = code.misc[3].
					acctr = code.description[2].
					namer = code.description[1].
					acctk =	code.misc[2].
					nameb =	code.misc[1].				
				END.
			END.
		END.
		ELSE DO:
			FOR EACH ploan WHERE
					 ploan.contract = "СТРАХ" AND
					 ploan.parent-cont-code = loan.cont-code NO-LOCK by ploan.open-date :
				IF AVAIL ploan AND GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"VidStr","") MATCHES (ENTRY(4,per,"|") + "*") THEN DO:	
					IF GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"PLpolis","") NE "" THEN
						soan = GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"PLpolis","").
					ELSE
						soan = ploan.doc-ref.
					IF GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"ПолучательРекв","") NE "" THEN DO:
						vValue =  GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"ПолучательРекв","").
						bikr  = ENTRY(1,vValue,"^").
						acctr = ENTRY(2,vValue,"^").
						namer = ENTRY(5,vValue,"^").
						FIND FIRST banks-code WHERE
								   banks-code.bank-code = bikr
							NO-LOCK NO-ERROR.
							IF AVAILABLE banks-code THEN DO:
								FIND FIRST banks WHERE
										   banks.bank-id = banks-code.bank-id
									NO-LOCK NO-ERROR.
								IF AVAIL banks THEN nameb = banks.name.
								FIND FIRST banks-corr WHERE 
										   banks-corr.bank-corr EQ banks.bank-id AND 
										   CAN-FIND (banks OF banks-corr WHERE banks.flag-rkc) 
									NO-LOCK NO-ERROR.
								IF AVAIL banks-corr THEN acctk = banks-corr.corr-acct.
							END.
					END.
					ELSE DO:
						vValue =  GetXAttrValueEx("loan",ploan.contract + "," + ploan.cont-code,"ПолСтрахПрем","").
						FIND FIRST code WHERE
								   code.class = "strahpol" AND
								   code.code = vValue
							NO-LOCK NO-ERROR.
						IF AVAIL code THEN DO:
							bikr  = code.misc[3].
							acctr = code.description[2].
							namer = code.description[1].
							acctk =	code.misc[2].
							nameb =	code.misc[1].	
						END.
						ELSE DO:
							FIND FIRST cust-corp WHERE
									   cust-corp.cust-id = ploan.cust-id
								NO-LOCK NO-ERROR.
							IF AVAIL cust-corp THEN DO:
								bikr  = cust-corp.bank-code.
								acctr = cust-corp.benacct.
								namer = cust-corp.name-short.
								acctk = cust-corp.corr-acct.
								FIND FIRST banks-code WHERE
										   banks-code.bank-code = cust-corp.bank-code
									NO-LOCK NO-ERROR.
								IF AVAILABLE banks-code THEN DO:
									FIND FIRST banks WHERE
											   banks.bank-id = banks-code.bank-id
										NO-LOCK NO-ERROR.
									nameb = banks.name.
								END.
							END.
						END.
					END.
				END.
			END.
		END.
		IF ENTRY(3,per,"|") = "БИК" THEN
			rproc = bikr.
		IF ENTRY(3,per,"|") = "СчКр" THEN
			rproc = acctk.
		IF ENTRY(3,per,"|") = "СчПл" THEN
			rproc = acctr.
		IF ENTRY(3,per,"|") = "ИмяП" THEN
			rproc = namer.
		IF ENTRY(3,per,"|") = "ИмяБ" THEN
			rproc = nameb.

		FOR EACH op-entry WHERE
				 op-entry.acct-db = loan-acct.acct,
		FIRST op of op-entry WHERE
         op.op-kind BEGINS ENTRY(2,per,"|") NO-LOCK BY op-entry.acct-db BY op-entry.op-date DESC:
			IF AVAILABLE op /*AND 
						 op.op-kind MATCHES (ENTRY(2,per,"|") + "*")*/ AND
						 INDEX(op.details, soan) NE 0 THEN
			DO:
				IF ENTRY(3,per,"|") = "Сумма" THEN
					rproc = TRIM(STRING(op-entry.amt-rub, ">>>,>>>,>>>,>>9.99")).
				IF ENTRY(3,per,"|") = "НазП" THEN DO:
				/* kam */
				    rproc = substring(op.details,index(op.details,'Оплата')).
				END.
			END.
		END.
   END.
 
   /* ayv вывод даты выдачи ПТС в формате 99 месяца 9999 */
   IF per EQ "ПТСДата" THEN
   DO:
		FIND term-obl WHERE
			 term-obl.cont-code = loan.cont-code AND
			 term-obl.contract = "Кредит" AND
			 term-obl.idnt = 5
		NO-LOCK NO-ERROR.
		vTmpDate = date(GetXAttrValueEx ("term-obl",
                                 loan.contract + "," + loan.cont-code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                 "TCDATE",
                                 "")).
		rproc = string(day(vTmpDate)) + " " + entry(month(vTmpDate),{&Months}) + " " + string(year(vTmpDate)) + " года".
   END.
   
   /* Сумма ежемесячного возврата ссуды */
   IF per EQ "ВС" THEN
   DO:
      /* Поиск последнего условия действующего на дату end-date */
      RUN RE_L_COND IN h_Loan (loan.contract,
                               loan.cont-code,
                               Xdate,
                               BUFFER loan-cond).

      IF AVAILABLE loan-cond          AND
         loan-cond.cred-period EQ "М" THEN
      DO:
         /*Илюха - выяснить почему не используется RE_FIST_TERM_OBL ? */
         FIND FIRST term-obl OF loan WHERE
                    term-obl.idnt     EQ 3
                AND term-obl.end-date GE loan-cond.since
            NO-LOCK NO-ERROR.

         IF AVAILABLE term-obl THEN
            rproc = TRIM(STRING(term-obl.amt-rub, ">>>,>>>,>>>,>>9.99")).
      END.
   END.

   /* Сумма последнего возврата ссуды */
   IF per EQ "ВСП" THEN
   DO:
      FIND LAST term-obl OF loan WHERE
                term-obl.idnt  EQ 3
         NO-LOCK NO-ERROR.

      IF AVAILABLE term-obl THEN
         rproc = TRIM(STRING(term-obl.amt-rub, ">>>,>>>,>>>,>>9.99")).
   END.
   
   /* Возвращает дополнительный реквизит по договору */
   IF per MATCHES ("ДР|*") THEN    /* Добавлены параметры - число строк и начая позиция */
   DO:
       RUN GetXattr (loan.Class-Code, ENTRY(2, per, '|'), BUFFER xattr).
       IF AVAIL xattr THEN
       DO:
          IF xattr.progress-field THEN
             rproc = GetBufferValue("loan",
                                    "WHERE loan.contract EQ '" + loan.contract + "'
                                    AND loan.cont-code EQ '" + loan.cont-code + "'",
                                    STRING(ENTRY(2, per, '|'))) NO-ERROR.
          ELSE
          DO:
             rproc = GetXAttrValueEx("loan",
                                     loan.contract + "," + loan.cont-code,
                                     ENTRY(2, per, '|'),
                                     "").
          END.
          IF {assigned rproc} THEN
             DO:
                /* Если ДР определен на классификаторе, то по коду этого
                   классификатора возвращаем его наименование */
             IF xattr.Domain-Code NE "" THEN
             DO:
                mTmpStr = GetCodeName(xattr.Domain-Code,rproc).
                IF {assigned mTmpStr} THEN
                  rproc = mTmpStr.
             END.
          END.
       END.
       IF NUM-ENTRIES(per,'|') > 2 AND ENTRY(3,per,'|') EQ "99.99.9999" THEN
          rproc = STRING(DATE(rproc),"99.99.9999").
       /*IF ENTRY(2, per, '|') EQ "cont-code" THEN
          rproc = delFilFromLoan(rproc).*/
       IF ENTRY(2, per, '|') EQ "ДатаСогл" THEN
           	    rproc = STRING(DATE(rproc),"99.99.9999").
     /* sku */
       IF NUM-ENTRIES(per,'|') > 2 AND
          ENTRY(3,per,'|') MATCHES ("Пр*") THEN
       DO:
          CASE ENTRY(3,per,'|'):
          WHEN "Пр" THEN
           RUN FrmtAmt(DEC(rproc),loan.currency,1, OUTPUT rproc).
          WHEN "Пр0" THEN DO:
            rproc = TRIM(STRING(DEC(rproc),">>>,>>>,>>9.99")).
		  END.
          WHEN "Пр2" THEN
           RUN FrmtAmt(DEC(rproc),loan.currency,2, OUTPUT rproc).
          WHEN "Пр3" THEN DO:
            RUN x-amtstr.p (DEC(rproc), loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
            rproc = vAmtStr + " " + vDecStr.
          END.
          END CASE.
       END. ELSE
       IF NUM-ENTRIES(per, '|') > 2 AND ENTRY(3, per, '|') EQ "99.99.9999" THEN
       DO:
    	    rproc = STRING(DATE(rproc),"99.99.9999").
       END. ELSE
       IF NUM-ENTRIES(per, '|') > 2 AND ENTRY(3, per, '|') EQ "dd mmmm yyyy" THEN
       DO:
          rproc = GetDateStr(DATE(rproc)).
       END. ELSE
       /* */
       IF NUM-ENTRIES(per,'|') > 2 THEN
       DO:
          ASSIGN
             mFlFormat  = TRUE
             vMes[1]    = rproc.
          {wordwrap.i
                    &s = vMes
                    &n = 50
                    &l = INT64(ENTRY(3,per,'|'))
                 }
          IF rproc <> "" AND rproc <> ? THEN
          DO:
              rproc = "".
              DO vNum = 1 TO 50:
                 IF vMes[vNum] = "" THEN
                     LEAVE.
              rproc = rproc + FILL(" ",INT64(ENTRY(4,per,'|'))) + vMes[vNum] + "\n".
              END.
          END.
       END.

   END.

   /* Если все перечисленные ДР заполнены возвращает 1 */
  IF per MATCHES ("ДРКлРП|*") THEN 
  DO vI = 2 TO NUM-ENTRIES(per, '|'):
      RUN GetXattr ((IF loan.cust-cat EQ "Ч" THEN "person" ELSE "cust-corp"), ENTRY(vI, per, '|'), BUFFER xattr).
        IF AVAIL xattr THEN 
        DO:
        IF xattr.progress-field THEN DO:
        IF loan.cust-cat EQ "Ч" THEN
          rproc = GetBufferValue("person",
                      "WHERE person.person-id EQ " + STRING(loan.cust-id),
                      STRING(ENTRY(vI, per, '|'))) NO-ERROR.
        ELSE
          rproc = GetBufferValue("cust-corp",
                      "WHERE cust-corp.cust-id EQ " + STRING(loan.cust-id),
                      STRING(ENTRY(vI, per, '|'))) NO-ERROR.
            END. ELSE
            DO:
               rproc = GetXAttrValueEx((IF loan.cust-cat EQ "Ч" THEN "person" ELSE "cust-corp"),
                                       STRING( loan.cust-id),
                                       ENTRY(vI, per, '|'),
                                       "").
            END.
        END.
    IF rproc NE "" THEN 
      rproc = "1".
    ELSE DO: rproc = "0".
     LEAVE.
    END.
  END.
   
   /* Возвращает расчетный счет для перевода средств и БИК банка */
  IF per MATCHES ("КлДепоз|*") THEN
  DO:
    FIND FIRST cust-corp WHERE
               cust-corp.cust-id = loan.cust-id
    NO-LOCK NO-ERROR.
    IF AVAIL cust-corp THEN DO:
      CASE ENTRY(2,per,'|'):   
        WHEN "bik" THEN
          rproc = cust-corp.bank-code.
        WHEN "bacct" THEN
          rproc = cust-corp.benacct.
      END CASE.  
    END.    
  END.
   /* Возвращает дополнительный реквизит по клиенту договора */
   IF per MATCHES ("ДРКЛ|*") THEN 
   DO:
       RUN GetXattr ((IF loan.cust-cat EQ "Ч" THEN "person" ELSE "cust-corp"), ENTRY(2, per, '|'), BUFFER xattr).
       IF AVAIL xattr THEN
       DO:
          IF xattr.progress-field THEN DO:
			IF loan.cust-cat EQ "Ч" THEN
				rproc = GetBufferValue("person",
										"WHERE person.person-id EQ " + STRING(loan.cust-id),
										STRING(ENTRY(2, per, '|'))) NO-ERROR.
			ELSE
				rproc = GetBufferValue("cust-corp",
										"WHERE cust-corp.cust-id EQ " + STRING(loan.cust-id),
										STRING(ENTRY(2, per, '|'))) NO-ERROR.
          END. ELSE
          DO:
             rproc = GetXAttrValueEx((IF loan.cust-cat EQ "Ч" THEN "person" ELSE "cust-corp"),
                                     STRING( loan.cust-id),
                                     ENTRY(2, per, '|'),
                                     "").
          END.
          IF {assigned rproc} THEN
             DO:
                /* Если ДР определен на классификаторе, то по коду этого
                   классификатора возвращаем его наименование */
             IF xattr.Domain-Code NE "" THEN
             DO:
                mTmpStr = GetCodeName(xattr.Domain-Code,rproc).
                IF {assigned mTmpStr} THEN
                  rproc = mTmpStr.
             END.
          END.
       END.
       IF ENTRY(2, per, '|') EQ "cont-code" THEN
          rproc = delFilFromLoan(rproc).

       /* sku */
       IF NUM-ENTRIES(per,'|') > 2 AND
          ENTRY(3,per,'|') MATCHES ("Пр*") THEN
       DO:
          CASE ENTRY(3,per,'|'):
          WHEN "Пр" THEN
           RUN FrmtAmt(DEC(rproc),loan.currency,1, OUTPUT rproc).
          WHEN "Пр0" THEN DO:
            rproc = TRIM(STRING(DEC(rproc),">>>,>>>,>>9.99")).
		  END.
          WHEN "Пр2" THEN
           RUN FrmtAmt(DEC(rproc),loan.currency,2, OUTPUT rproc).
          WHEN "Пр3" THEN DO:
            RUN x-amtstr.p (DEC(rproc), loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
            rproc = vAmtStr + " " + vDecStr.
          END.
          END CASE.
       END. ELSE
       IF NUM-ENTRIES(per, '|') > 2 AND ENTRY(3, per, '|') EQ "99.99.9999" THEN
       DO:
    	    rproc = STRING(DATE(rproc),"99.99.9999").
       END. ELSE
       /* */
       IF NUM-ENTRIES(per,'|') > 2 THEN
       DO:
          ASSIGN
             mFlFormat  = TRUE
             vMes[1]    = rproc.
          {wordwrap.i
                    &s = vMes
                    &n = 50
                    &l = INT64(ENTRY(3,per,'|'))
                 }
          IF rproc <> "" AND rproc <> ? THEN
          DO:
              rproc = "".
              DO vNum = 1 TO 50:
                 IF vMes[vNum] = "" THEN
                     LEAVE.
              rproc = rproc + FILL(" ",INT64(ENTRY(4,per,'|'))) + vMes[vNum] + "\n".
              END.
          END.
       END.

   END.
   
   
   /* Дата первой пролонгации */
   IF per EQ "ПРО_НАЧ" THEN
   DO:
      FIND FIRST pro-obl OF loan WHERE
                 pro-obl.idnt EQ 3
         NO-LOCK NO-ERROR.

      IF AVAILABLE pro-obl THEN
         rproc = STRING(pro-obl.pr-date, "99/99/9999").
   END.

   /* Дата последней пролонгации */
   IF per EQ "ПРО_ФИН" THEN
   DO:
      FIND LAST pro-obl OF loan WHERE
                pro-obl.idnt EQ 3
         NO-LOCK NO-ERROR.

      IF AVAILABLE pro-obl THEN
         rproc = STRING(pro-obl.pr-date, "99/99/9999").
   END.

      /* Количество пролонгации */
   IF per EQ "ПРО_КОЛ" THEN
   DO:
      vCount = 0.
      FOR EACH pro-obl WHERE
               pro-obl.contract  EQ loan.contract
           AND pro-obl.cont-code EQ loan.cont-code
           AND pro-obl.idnt      EQ 3
      NO-LOCK:
         vCount = vCount + 1.
      END.
      rproc = STRING(vCount).
   END.

   /* Возвращает размер плановой суммы обеспечения */
   IF per EQ "ПЛ_СУММ" THEN
   DO:
      RUN RE_TERM_OBL IN h_Loan (loan.contract,
                                 loan.cont-code,
                                 5,
                                 Xdate,
                                 BUFFER term-obl).

      IF AVAILABLE term-obl THEN
         rproc = TRIM(STRING(term-obl.amt-rub,">>>,>>>,>>>,>>9.99")).
   END.

   /* Сумму кредита по первому условию */
   IF per EQ "СумУсл1" THEN
   DO:
      FIND FIRST term-obl OF loan WHERE
                 term-obl.idnt EQ 2
         NO-LOCK NO-ERROR.

      IF AVAILABLE term-obl THEN
         rproc = TRIM(STRING(term-obl.amt-rub,">>>,>>>,>>>,>>9.99")).
   END.

   /* Номер счета по роли */
   IF per MATCHES ("РНкод_*") THEN
   DO:
      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "_"),
                               Xdate,
                               BUFFER loan-acct).

      IF AVAILABLE loan-acct THEN
         rproc = delFilFromAcct(loan-acct.acct).

      /* Форматирование */
      IF NUM-ENTRIES(per,'_') > 2 THEN
      DO:
         ASSIGN
            mFlFormat  = TRUE
            vMes[1]    = rproc.
         {wordwrap.i
                   &s = vMes
                   &n = 50
                   &l = INT64(ENTRY(3,per,'_'))
               }
         IF rproc <> "" AND rproc <> ? THEN
         DO:
             DO vNum = 1 TO 50:
                IF vMes[vNum] = "" THEN
                    LEAVE.
             rproc = rproc + FILL(" ",INT64(ENTRY(4,per,'_'))) + vMes[vNum] + "\n".
             END.
         END.
      END.
   END.

   /* Наименоание роли счета */
   IF per MATCHES ("РИкод_*") THEN
   DO:
      mTmpStr = GetCodeName("ТипСчДог",ENTRY(2, per, "_")).

      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "_"),
                               Xdate,
                               BUFFER loan-acct).
      IF NOT AVAIL loan-acct THEN LEAVE.

      IF mTmpStr <> ? THEN
         rproc = TRIM (mTmpStr).

      /* Форматирование */
      IF NUM-ENTRIES(per,'_') > 2 THEN
      DO:
         ASSIGN
            vMes[1]     = rproc
            mFlFormat   = TRUE.
         {wordwrap.i
                   &s = vMes
                   &n = 50
                   &l = INT64(ENTRY(3,per,'_'))
               }
         IF rproc <> "" AND rproc <> ? THEN
         DO:
             DO vNum = 1 TO 50:
                IF vMes[vNum] = "" THEN
                    LEAVE.
             rproc = rproc + FILL(" ",INT64(ENTRY(4,per,'_'))) + vMes[vNum] + "\n".
             END.
         END.
      END.
   END.

   /* Номер счета по роли */
   IF per MATCHES ("РНкод_*") THEN
   DO:
      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "_"),
                               end-date,
                               BUFFER loan-acct).

      IF AVAILABLE loan-acct THEN
         rproc = delFilFromAcct(loan-acct.acct).

      /* Форматирование */
      IF NUM-ENTRIES(per,'_') > 2 THEN
      DO:
         vMes[1] = rproc.
         {wordwrap.i
                   &s = vMes
                   &n = 50
                   &l = int(ENTRY(3,per,'_'))
               }
         IF rproc <> "" AND rproc <> ? then
         DO:
             DO vNum = 1 TO 50:
                IF vMes[vNum] = "" THEN
                    LEAVE.
             PUT STREAM fil UNFORMATTED SPACE(int(ENTRY(4,per,'_'))) vMes[vNum] SKIP.
             END.
         END.
         rproc = "".
      END.
   END.

   /* Наименоание счета */
   IF per MATCHES ("РНаимСч_*") THEN
   DO:
      mTmpStr = GetCodeName("ТипСчДог",ENTRY(2, per, "_")).


      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "_"),
                               Xdate,
                               BUFFER loan-acct).
      IF NOT AVAIL loan-acct THEN LEAVE.

      find first acct where acct.acct = loan-acct.acct no-lock no-error.
      rproc = TRIM(acct.Details).
   END.

   /* Дата открытия счета счета */
   IF per MATCHES ("РДтОткрСч_*") THEN
   DO:
      mTmpStr = GetCodeName("ТипСчДог",ENTRY(2, per, "_")).


      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "_"),
                               Xdate,
                               BUFFER loan-acct).
      IF NOT AVAIL loan-acct THEN LEAVE.

      find first acct where acct.acct = loan-acct.acct no-lock no-error.
      rproc = TRIM(STRING(acct.open-date)).
   END.


   /* Дата открытия счета счета - прописью */
   IF per MATCHES ("РПропДтОткрСч_*") THEN
   DO:
      mTmpStr = GetCodeName("ТипСчДог",ENTRY(2, per, "_")).


      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "_"),
                               Xdate,
                               BUFFER loan-acct).
      IF NOT AVAIL loan-acct THEN LEAVE.

      find first acct where acct.acct = loan-acct.acct no-lock no-error.
      rproc = TRIM(STRING(DAY(acct.open-date),"99") + " " + ENTRY(MONTH(acct.open-date),months2) + " " + STRING(YEAR(acct.open-date),"9999") + "г.").
   END.

   /* Статус клиента */
   IF per MATCHES ("РСтатусКл*") THEN
   DO:
      if loan.cust-cat EQ "Ю"
	THEN do:
           FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
	   rproc = TRIM(cust-corp.cust-stat).
        end.
   END.


   /* Остаток на счете с ролью */
   IF per MATCHES ("РОкод_*") THEN
   DO:
      IF NUM-ENTRIES (per,"_") EQ 3 THEN
      DO:
         IF ENTRY(3, per, "_") EQ "Крыж" THEN
            vValue = "√".
         ELSE
         IF ENTRY(3, per, "_") EQ "Крыж2" THEN
            vValue = "√√".
         ELSE
         vValue = ENTRY(3, per, "_").
      END.
      ELSE
         vValue = gop-status.
      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "_"),
                               Xdate,
                               BUFFER loan-acct).
      IF NOT AVAIL loan-acct THEN LEAVE.

      RUN acct-pos IN h_base (loan-acct.acct,
                              loan-acct.currency,
                              Xdate,
                              Xdate,
                              vValue).
      rproc = TRIM(STRING(IF loan-acct.currency EQ "" THEN
                             ABSOLUTE(sh-bal)
                          ELSE
                             ABSOLUTE(sh-val),
                          ">>>,>>>,>>>,>>9.99")).

      /* Форматирование */
      IF NUM-ENTRIES(per,'_') > 3 THEN
      DO:
         ASSIGN
            mFlFormat  = TRUE
            vMes[1]    = rproc.
         {wordwrap.i
                   &s = vMes
                   &n = 50
                   &l = INT64(ENTRY(4,per,'_'))
               }
         IF rproc <> "" AND rproc <> ? THEN
         DO:
             DO vNum = 1 TO 50:
                IF vMes[vNum] = "" THEN
                    LEAVE.
                rproc = rproc + (IF NUM-ENTRIES(per,'_') = 5
                                 THEN FILL(" ",INT64(ENTRY(5,per,'_')))
                                 ELSE "")
                              + vMes[vNum]
                              + "\n".
             END.
         END.
      END.
   END.

   /* Остаток на договоре (остаток  по параметру "0") */
   IF per MATCHES ("ПрмО_*") THEN
   DO:
      ASSIGN
         per       = IF NUM-ENTRIES (per, "_") EQ 2 THEN ENTRY(2, per, "_") ELSE "0"
         CodOstpar = GetParCode(loan.class-code, 'КодОснДолг')
      .
      INT64 (per) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR
      THEN DO:
         RUN STNDRT_PARAM IN h_Loan (loan.contract,
                                     loan.cont-code,
                                     INT64 (per),
                                     Xdate, /* Рассчет на дату end-date */
                                     OUTPUT vPrmDec,
                                     OUTPUT vDbSumDec,
                                     OUTPUT vCrSumDec).

         IF INT64(per) GE CodOstPar THEN
         DO:
            RUN inter_current (BUFFER loan, (INT64(per) - CodOstPar), OUTPUT vTmpDec).
            vPrmDec = vPrmDec + vTmpDec.
         END.
         rproc = TRIM(STRING(ABSOLUTE(vPrmDec), ">>>,>>>,>>>,>>9.99")).
      END.
   END.

   /* Остаток на договоре по сумме параметров */
   IF per MATCHES ("ПрмОСумм_*") THEN
   DO:
      IF NUM-ENTRIES(per, "|") GT 0 THEN
      DO:
      ASSIGN
         per       = ENTRY(2, per, "_")
         vTotAmt   = 0
         CodOstpar = GetParCode(loan.class-code, 'КодОснДолг')
      .
         DO vI = 1 TO NUM-ENTRIES(per, "|"):
            RUN STNDRT_PARAM IN h_Loan (loan.contract,
                                        loan.cont-code,
                                        INT64(ENTRY(vI, per, "|")),
                                        Xdate, /* Рассчет на дату end-date */
                                        OUTPUT vPrmDec,
                                        OUTPUT vDbSumDec,
                                        OUTPUT vCrSumDec).
            IF INT64(ENTRY(vI, per, "|")) GE CodOstPar THEN
            DO:
               RUN inter_current (BUFFER loan,
                                  (INT64(ENTRY(vI, per, "|")) - CodOstPar),
                                  OUTPUT vTmpDec).
               vPrmDec = vPrmDec + vTmpDec.
            END.
            vTotAmt = vTotAmt + vPrmDec.
         END.
      END.
      rproc = TRIM(STRING(ABSOLUTE(vTotAmt), ">>>,>>>,>>>,>>9.99")).
   END.

   /* Паспортные данные клиента (физического лица) */
   IF per EQ "Psprt" THEN
   DO:
      IF loan.cust-cat EQ "Ч" THEN
      DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR.

         IF AVAILABLE person THEN
            rproc = person.document-id + " " +
                    person.document + ", выдан " +
                    fGetDocIssue(person.person-id).
      END.
   END.

   IF per MATCHES ("ДР_СО|*|") THEN
   DO:

      ASSIGN
         vSrcChar = ENTRY(1, ENTRY(2,per,"|"), "|")
         rproc    = ""
         .

      FOR EACH term-obl OF loan WHERE
               term-obl.idnt EQ 5
         NO-LOCK:

         vTrmSurChar = term-obl.contract + "," + term-obl.cont-code  + "," +
                       STRING (term-obl.idnt)     + "," +
                       STRING (term-obl.end-date) + "," +
                       STRING (term-obl.nn).

         DO vCountInt = 1 TO NUM-ENTRIES (vSrcChar, "."):

            vXattrChar = ENTRY (vCountInt, vSrcChar, '.').
            FIND FIRST xattr WHERE
                       xattr.xattr-code EQ vXattrChar
               NO-LOCK NO-ERROR.

            IF AVAILABLE xattr THEN
               vNameChar = TRIM(xattr.name).

            IF LENGTH (vNameChar) LT 40 THEN
               vNameChar = vNameChar + FILL (" ", 40 - LENGTH (vNameChar)).

            ASSIGN
               vNameChar = vNameChar + " : ".
               mTmpStr = GetXAttrValueEx("term-obl",
                                         vTrmSurChar,
                                         vXattrChar,
                                         "")
               .

            IF mTmpStr <> "" THEN
               rproc = rproc + vNameChar + mTmpStr + "~n".
         END.
         rproc = rproc + "~n".
      END.
   END.

      /* Сумма аннуитентного платежа */
   IF per MATCHES "АннПлат*" THEN
   DO:
      FIND LAST bloan-cond WHERE bloan-cond.contract  EQ loan.contract
                             AND bloan-cond.cont-code EQ loan.cont-code
      NO-LOCK NO-ERROR.

      mTmpStr = GetXAttrValueEx("loan-cond",
                                loan.contract + "," + loan.cont-code + "," + STRING(bloan-cond.since, "99/99/99"),
                                "АннуитПлат",
                                "").

      IF      NUM-ENTRIES(per, "|") GT 1
         AND (   ENTRY(2, per, "|") EQ "Пр"
              OR ENTRY(2, per, "|") EQ "Пр2") THEN
      DO:
         RUN FrmtAmt(DEC(mTmpStr),loan.currency,(IF ENTRY(2, per, "|") EQ "Пр2" THEN 2 ELSE 1), OUTPUT rproc).
      END.
      ELSE
         rproc = TRIM(STRING(DEC(mTmpStr), ">>>,>>>,>>>,>>9.99")).

   END.

      /* Сумма аннуитентного платежа */
   IF per MATCHES "АннПлат2*" THEN
   DO:
      FIND FIRST bloan-cond WHERE bloan-cond.contract  EQ loan.contract
                             AND bloan-cond.cont-code EQ loan.cont-code
      NO-LOCK NO-ERROR.

      mTmpStr = GetXAttrValueEx("loan-cond",
                                loan.contract + "," + loan.cont-code + "," + STRING(bloan-cond.since, "99/99/99"),
                                "АннуитПлат",
                                "").

      IF      NUM-ENTRIES(per, "|") GT 1
         AND (   ENTRY(2, per, "|") EQ "Пр"
              OR ENTRY(2, per, "|") EQ "Пр2") THEN
      DO:
         RUN FrmtAmt(DEC(mTmpStr),loan.currency,(IF ENTRY(2, per, "|") EQ "Пр2" THEN 2 ELSE 1), OUTPUT rproc).
      END.
      ELSE
         rproc = TRIM(STRING(DEC(mTmpStr), ">>>,>>>,>>>,>>9.99")).

   END.


      /* Полная стоимость кредита */
   IF per MATCHES "ПолнСуммКр*" THEN
   DO:
      mTmpStr = TRIM(STRING(GetPskLoan(loan.contract,loan.cont-code,loan.since), ">>>,>>>,>>>,>>9.99")).
      IF     NUM-ENTRIES(per, "|") GT 1
         AND (   ENTRY(2, per, "|") EQ "Пр"
              OR ENTRY(2, per, "|") EQ "Пр2") THEN
      DO:
         RUN FrmtAmt(DEC(mTmpStr),loan.currency,(IF ENTRY(2, per, "|") EQ "Пр2" THEN 2 ELSE 1), OUTPUT rproc).
      END.
      ELSE
         rproc = mTmpStr.
   END.

   /* Первый платеж по графику (корректно только для аннуитетов). Второй параметр -
   коды единовременных комиссий для учета, через запятую. Третий - список ДР кредитного договора, на которых хранятся суммы,
   которые необходимо прибавить к первому платежу. Четвертый- вид вывода расшифровки суммы: Пр или Пр2 */
   IF per MATCHES("ПервПлат|*") THEN
   DO:
     mFirstPay=0.
    /*  FIND LAST bloan-cond WHERE bloan-cond.contract  EQ loan.contract
                             AND bloan-cond.cont-code EQ loan.cont-code
      NO-LOCK NO-ERROR.        */
      /* получаем сумму аннуитета */
     /* mFirstPay = mFirstPay +
                  DEC(GetXAttrValueEx("loan-cond",
                                      loan.contract + "," + loan.cont-code + "," + STRING(bloan-cond.since, "99/99/99"),
                                      "АннуитПлат",
                                      "0")). */
/* sku */
find first term-obl OF loan WHERE term-obl.cont-code EQ loan.cont-code
		and term-obl.idnt EQ 3
		and term-obl.contract EQ "Кредит"
NO-LOCK NO-ERROR.
find first bterm-obl OF loan WHERE bterm-obl.cont-code EQ loan.cont-code
		and bterm-obl.idnt EQ 1
		and bterm-obl.contract EQ "Кредит"
NO-LOCK NO-ERROR.

IF AVAIL term-obl AND
   term-obl.dsc-beg-date <= 
    (IF AVAIL bterm-obl THEN bterm-obl.dsc-beg-date ELSE term-obl.dsc-beg-date)
THEN
mFirstPay=mFirstPay + term-obl.AMT-RUB.

IF AVAIL bterm-obl AND
   bterm-obl.dsc-beg-date <= 
    (IF AVAIL term-obl THEN term-obl.dsc-beg-date ELSE bterm-obl.dsc-beg-date)
THEN
mFirstPay=mFirstPay + bterm-obl.AMT-RUB. 
/**/

		 
      /* рассчитываем сумму единовременных комиссий, указанных во втором параметре */


/*      IF NUM-ENTRIES(per, "|") GT 1 THEN
         DO vIntTmp = 1 TO NUM-ENTRIES(ENTRY(2,per,"|"),";"):
            FIND FIRST code WHERE code.class EQ "НачКом"
                              AND code.code  EQ ENTRY(vIntTmp,ENTRY(2,per,"|"),";")
            NO-LOCK NO-ERROR.
 sveta*/
      IF NUM-ENTRIES(per, "|") GT 1 THEN
         DO vIntTmp = 1 TO NUM-ENTRIES(ENTRY(2,per,"|"),";"):
            IF ENTRY(vIntTmp,ENTRY(2,per,"|"),";") = "%РКО" THEN DO:
        	FIND FIRST comm-rate WHERE
        	    comm-rate.kau EQ loan.contract + "," + loan.cont-code
        	    AND     comm-rate.currency   EQ loan.currency
        	    AND     comm-rate.commission EQ "%РКО"
        	    NO-LOCK NO-ERROR.
    		IF AVAIL comm-rate THEN DO:
                  mFirstPay = mFirstPay + comm-rate.rate-comm.
		END.
            END. ELSE DO:
        	FIND FIRST code WHERE code.class EQ "НачКом"
                      AND code.code  EQ ENTRY(vIntTmp,ENTRY(2,per,"|"),";")
        	    NO-LOCK NO-ERROR.
        	IF AVAIL code THEN
        	DO:
            	    {empty otch1}
            	    RUN lncommsh.p (loan.contract,
                               loan.cont-code,
                               code.code,
                               loan.open-date,
                               loan.end-date,
                               NO).
            	    FOR FIRST otch1 BY otch1.beg-date:
                	mFirstPay = mFirstPay + otch1.summ_pr.
            	    END.
        	END.
            END.
         END.
      /* проходим по спису ДР договора, значения с которых необходимо прибавить */
      IF NUM-ENTRIES(per, "|") GT 2 THEN
         DO vIntTmp = 1 TO NUM-ENTRIES(ENTRY(3,per,"|")):
            mFirstPay = mFirstPay +
                        DEC(GetXAttrValueEx("loan",
                                            loan.contract + "," + loan.cont-code,
                                            ENTRY(vIntTmp,ENTRY(3,per,"|")),
                                            "0"))
            NO-ERROR.
         END.

      IF      NUM-ENTRIES(per, "|") GT 3
         AND (   ENTRY(4, per, "|") EQ "Пр"
              OR ENTRY(4, per, "|") EQ "Пр2") THEN
      DO:
	RUN x-amtstr.p (mFirstPay, loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
        		rproc = vAmtStr + " " + vDecStr.

/* sveta        RUN FrmtAmt(mFirstPay,loan.currency,(IF ENTRY(4, per, "|") EQ "Пр2" THEN 2 ELSE 1), OUTPUT rproc).*/
      END.
      ELSE
         rproc = TRIM(STRING(mFirstPay, ">>>,>>>,>>>,>>9.99")).
   END.

   /* Количество обеспечений по заданному условию на доп.реквизите : */
   IF per MATCHES ("КолОб|*|*") THEN
   DO:
      /* код рекв. и значение (маска для can-do) */
      ASSIGN
         vXattrChar  = ENTRY(2,per,"|")
         vSrcChar    = ENTRY(3,per,"|")
         rproc       = ""
      .

      vCountInt = 0 .
      FOR EACH term-obl OF loan WHERE
               term-obl.idnt EQ 5
         NO-LOCK:
         vTrmSurChar = term-obl.contract + "," + term-obl.cont-code  + "," +
                       STRING (term-obl.idnt)     + "," +
                       STRING (term-obl.end-date) + "," +
                       STRING (term-obl.nn).

         mTmpStr = GetXAttrValueEx("term-obl",
                                   vTrmSurChar,
                                   vXattrChar,
                                   "")
         .
         IF mTmpStr <> "" AND CAN-DO(vSrcChar, mTmpStr) THEN
            vCountInt = vCountInt + 1 .
      END.

      IF NUM-ENTRIES(per,"|") GE 4 THEN
         vProp = ENTRY(4,per,"|") .   /* параметр - пр ? */

      IF TRIM(vProp) EQ "пр" THEN DO :  /* результат - прописью */
         RUN x-amtstr.p (vCountInt, "", NO, NO, OUTPUT vAmtStr, OUTPUT vDecStr) .
         rproc = LC(vAmtStr) .
      END.
      ELSE
         rproc = STRING(vCountInt) .  /* только число */
   END.
	
   CASE per:
      WHEN "Г" THEN
         rproc = LC(banks.town-type) + ". " + TitleCase(banks.town).
      WHEN "И" THEN
         rproc = banks.name.
      WHEN "А" THEN
         rproc = banks.law-address.
      WHEN "К" THEN
         rproc = IF loan.contract EQ "Кредит" THEN
                    "КРЕДИТНЫЙ ДОГОВОР"
                 ELSE
                    "ДЕПОЗИТНЫЙ ДОГОВОР".
      WHEN "OK" OR  WHEN "ОК" THEN
      DO:
         IF loan.contract  NE cod1 OR
            loan.cont-code NE cod  THEN
         DO:
            MESSAGE " Введите реквизиты ответственного лица банка".
            DISPLAY  "Должность :"              @ a
                     "Фамилия,имя,отчество : "  @ c
               WITH FRAME q2.
            SET rproc1
                rproc WITH FRAME q2.

            ASSIGN
               name-k = rproc
               dol-k  = rproc1
               .

            IF loan.cust-cat EQ "Ч" THEN
               ASSIGN
                  cod  = loan.cont-code
                  cod1 = loan.contract
                  .
         END. /* if loan.contract ne cod1 or loan.cont-code ne cod then do: */

         rproc =  rproc
               + "Должность ответственного лица банка: "            + "\n"
               + STRING(dol-k)                                      + "\n"
               + "Фамилия,имя,отчество ответственного лица банка: " + "\n"
               + name-k.
         HIDE MESSAGE .
         HIDE FRAME q2.
      END.  /* else if per eq "ОК" then do: */
      WHEN "ОЗ" THEN
      DO:
         IF loan.cust-cat EQ "Ю" THEN
         DO:
            IF loan.contract  NE cod1 OR loan.cont-code NE cod  THEN
            DO:
               MESSAGE
                  " Введите реквизиты ответственного лица клиента".
               DISPLAY "Должность :"              @ a
                       "Фамилия,имя,отчество : "  @ c
                  WITH FRAME q2.
               SET rproc1
                   rproc WITH FRAME q2.

               ASSIGN
                  name-z = rproc
                  dol-z  = rproc1
                  cod    = loan.cont-code
                  cod1   = loan.contract
                  .
            END. /* if loan.contract ne cod1 or loan.cont-code ne cod then do: */

            rproc = rproc
                  + "Должность ответственного лица клиента: "            + "\n"
                  + STRING(dol-z)                                        + "\n"
                  + "Фамилия,имя,отчество ответственного лица клиента: " + "\n"
                  + name-z.
            HIDE MESSAGE .
            HIDE FRAME q2.
         END.  /*   if loan.cust-cat eq "Ю" then do: */
      END.     /*   else if per eq "ОЗ"  then do:    */
      /* Наименование клиента */
      WHEN "КЛ_И"  THEN 
          RUN FIO_CLIENT ("И",               /* Ф-фамилия И-имя О-отчество */
                        loan.cust-id,        /* идентификатор клиента */
                        INPUT-OUTPUT rproc). /* наименование клиента */
      WHEN "КЛ_О"  THEN 
          RUN FIO_CLIENT ("О",               /* Ф-фамилия И-имя О-отчество */
                        loan.cust-id,        /* идентификатор клиента */
                        INPUT-OUTPUT rproc). /* наименование клиента */
      WHEN "КЛ_Ф"  THEN  
          RUN FIO_CLIENT ("Ф",               /* Ф-фамилия И-имя О-отчество */
                        loan.cust-id,        /* идентификатор клиента */
                        INPUT-OUTPUT rproc). /* наименование клиента */                      
      WHEN "КЛ"  THEN
          RUN RE_CLIENT (loan.cust-cat,       /* тип клиента */
                        loan.cust-id,        /* идентификатор клиента */
                        INPUT-OUTPUT rproc). /* наименование клиента */
      WHEN "КЛ_П"  THEN
          RUN RE_CLIENT_FULL(loan.cust-cat,       /* тип клиента */
                        loan.cust-id,        /* идентификатор клиента */
                        INPUT-OUTPUT rproc). /* наименование клиента */
      WHEN "КЛ_ОРГ"  THEN DO:
          RUN RE_CLIENT (loan.cust-cat,       /* тип клиента */
                        loan.cust-id,        /* идентификатор клиента */
                        INPUT-OUTPUT rproc). /* наименование клиента */
          IF loan.cust-cat EQ "Ю" THEN DO:
             FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
             IF AVAIL(cust-corp) THEN DO:
                IF {assigned cust-corp.cust-stat} THEN
                   rproc = TRIM(TRIM(cust-corp.cust-stat) + " " + TRIM(rproc)).
                ELSE
                   rproc = TRIM(rproc).
             END.
          END.
          IF loan.cust-cat EQ "Б" THEN DO:
             FIND FIRST banks WHERE banks.bank-id EQ loan.cust-id NO-LOCK NO-ERROR.
             IF AVAIL(banks) THEN DO:
                mStatCode = GetXAttrValueEx("banks", STRING(banks.bank-id), "bank-stat", "").
                FIND code WHERE code.class EQ "КодПредп" AND code.code EQ mStatCode NO-LOCK NO-ERROR.
                IF AVAIL(code) THEN
                   rproc = TRIM(code.val + " " + TRIM(rproc)).
                ELSE
                   rproc = TRIM(rproc).
             END.
          END.
      END.
      WHEN "КЛ_СТАТ" THEN DO:
          rproc = "".
          IF loan.cust-cat EQ "Ю" THEN DO:		  
             FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
             IF AVAIL(cust-corp) THEN DO:
                IF {assigned cust-corp.cust-stat} THEN DO:
					FIND code WHERE code.val EQ cust-corp.cust-stat
					AND code.class EQ "КодПредп".
					rproc = code.name.
				END.
                IF rproc = ? THEN rproc = "".
             END.
          END.
      END.
      WHEN "З" THEN
         IF loan.cust-cat EQ "Ю" THEN
         DO:
            FIND FIRST cust-corp WHERE
            cust-corp.cust-id EQ loan.cust-id
            NO-LOCK NO-ERROR.
            rproc = STRING(cust-stat) + " " + STRING(cust-corp.name-corp) + "\n"
            + "Адрес: " + STRING(cust-corp.addr-of-low[1])          .
            IF cust-corp.addr-of-low[2] NE "" THEN
            DO:
               rproc = rproc + "\n" + STRING(cust-corp.addr-of-low[2]).
            END.
			find first buf-cust-ident WHERE (buf-cust-ident.close-date EQ ? OR
			buf-cust-ident.close-date >= today)
			and buf-cust-ident.cust-id eq loan.cust-id
			and buf-cust-ident.class-code eq "p-cust-adr" no-lock.
			if avail buf-cust-ident then do:
				if buf-cust-ident.cust-code-type eq "АдрФакт" then 
					rproc = rproc + "\n" + "Адрес факт.: " + TRIM(STRING(buf-cust-ident.issue)).
				if buf-cust-ident.cust-code-type eq "АдрПроп" then 
					rproc = rproc + "\n" + "Адрес проп.: " + TRIM(STRING(buf-cust-ident.issue)).
				if buf-cust-ident.cust-code-type eq "АдрЮр" then 
					rproc = rproc + "\n" + "Адрес юр.: " + TRIM(STRING(buf-cust-ident.issue)).
			end.
         END. /* if loan.cust-cat eq "Ю" then do:*/
         ELSE  IF loan.cust-cat EQ "Ч" THEN
         DO:
            FIND FIRST person WHERE
            person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR.
			rproc = STRING(person.first-name) + " "
            + STRING(person.name-last)  + " "
            + person.document-id + ": " + person.document + "\n"
			+ "Адрес: " + TRIM(STRING(person.address[1])).
            IF person.address[2] NE "" THEN
               rproc = rproc + "\n" + TRIM(STRING(person.address[2])).
			find first buf-cust-ident WHERE (buf-cust-ident.close-date EQ ? OR
			buf-cust-ident.close-date >= today)
			and buf-cust-ident.cust-id eq loan.cust-id
			and buf-cust-ident.class-code eq "p-cust-adr" no-lock.
			if avail buf-cust-ident then do:
				if buf-cust-ident.cust-code-type eq "АдрФакт" then 
					rproc = rproc + "\n" + "Адрес факт.: " + TRIM(STRING(buf-cust-ident.issue)).
				if buf-cust-ident.cust-code-type eq "АдрПроп" then 
					rproc = rproc + "\n" + "Адрес проп.: " + TRIM(STRING(buf-cust-ident.issue)).
				if buf-cust-ident.cust-code-type eq "АдрЮр" then 
					rproc = rproc + "\n" + "Адрес юр.: " + TRIM(STRING(buf-cust-ident.issue)).
			end.
         END.  /*  if loan.cust-cat eq "Ч" then do: */
      WHEN "Н" THEN 
         rproc = STRING(loan.doc-ref).
      WHEN "ДН" THEN
         rproc = STRING(DAY(loan.open-date)) + " " +
                 ENTRY(MONTH(loan.open-date),
                       {&Months}) +
                 STRING(YEAR(loan.open-date), " 9999 г.").
       WHEN "ДН2" THEN
         rproc = STRING(DAY(loan.open-date)) + " " +
                 ENTRY(MONTH(loan.open-date),
                       {&Months}) +
                 STRING(YEAR(loan.open-date), " 9999 года").
      /*pda - дата гашения в формате 99.99.9999*/
      WHEN "ДГ" THEN
        rproc = STRING(loan.close-date,"99.99.9999").
      /*ayv - дата начала в формате 99.99.9999*/
      WHEN "ДНЦ" THEN
          rproc = STRING(loan.open-date,"99.99.9999").
      WHEN "ДК" THEN
         rproc = STRING(DAY(loan.end-date)) + " " +
                  ENTRY(MONTH(loan.end-date),
                       {&Months}) +
                  STRING(YEAR(loan.end-date), " 9999 г.").
      WHEN "Дсогл" THEN  /* aa4 -   добавлены тэги: Дсогл, Дгаш1, Дгаш2 - день начисления %% прописью для Авто+. */
      IF loan-cond.cred-date EQ 3 THEN
         rproc = REPLACE(amt-r(loan-cond.cred-date),"го","").
      ELSE IF loan-cond.cred-date LE 20 THEN
         rproc = REPLACE(amt-r(loan-cond.cred-date),"го","е").
      ELSE
         rproc = REPLACE(amt-r(20),"го","е").
      WHEN "Дгаш1" THEN
      IF loan-cond.cred-date EQ 1 THEN DO:
         dayinc = loan-cond.cred-date + 1.
         rproc = "со " + amt-r(dayinc).
      END.
      ELSE IF loan-cond.cred-date LE 20 THEN DO:
         dayinc = loan-cond.cred-date + 1.
         rproc = "с " + amt-r(dayinc).
      END.
      ELSE
         rproc = "с " + amt-r(21).
      WHEN "Дгаш2" THEN
      IF loan-cond.cred-date LE 20 THEN
         rproc = amt-r(loan-cond.cred-date).
      ELSE
         rproc = amt-r(20).         
      WHEN "Дсоглмб" THEN  /* aa4 -   добавлены тэги: Дсоглмб, Дгашмб1, Дгашмб2 - день начисления %% прописью для МБ+. */
      IF loan-cond.cred-date EQ 3 THEN
         rproc = REPLACE(amt-r(loan-cond.cred-date),"го","").
      ELSE IF loan-cond.cred-date LE 15 THEN
         rproc = REPLACE(amt-r(loan-cond.cred-date),"го","е").
      ELSE
         rproc = REPLACE(amt-r(15),"го","е").
      WHEN "Дгашмб1" THEN
      IF loan-cond.cred-date EQ 1 THEN DO:
         dayinc = loan-cond.cred-date + 1.
         rproc = "со " + amt-r(dayinc).
      END.
      ELSE IF loan-cond.cred-date LE 15 THEN DO:
         dayinc = loan-cond.cred-date + 1.
         rproc = "с " + amt-r(dayinc).
      END.
      ELSE
         rproc = "с " + amt-r(16).
      WHEN "Дгашмб2" THEN
      IF loan-cond.cred-date LE 15 THEN
         rproc = amt-r(loan-cond.cred-date).
      ELSE
         rproc = amt-r(15).                  
      WHEN "C" THEN
      DO:
         FIND FIRST loan-acct {wh-t &f=loan-acct &c="/*"} /* для красоты */
                AND loan-acct.acct-type EQ loan.contract
                AND loan-acct.since     LE loan.open-date
            NO-LOCK NO-ERROR .
         IF AVAIL loan-acct  THEN
            rproc = STRING(delFilFromAcct(loan-acct.acct)).
      END.
      WHEN "Ц" THEN
      DO:
         mTmpStr = GetXAttrValueEx("loan",
                                   loan.contract + "," + loan.cont-code,
                                   "ЦельКред",
                                   ?).
         IF mTmpStr <> ? THEN
         DO:
            mTmpStr = GetCodeName("ЦельКред",mTmpStr).
            rproc = IF mTmpStr <> ? THEN
                       mTmpStr
                    ELSE
                       "Прочие нужды".
         END.
         ELSE
            rproc = "Прочие нужды" .
      END. /* else if per eq "ПРОДУКТ" then do: */
      WHEN "СумПервВзн" THEN  /* pda сумма первоначального взноса */
      IF AVAILABLE(loan) THEN
      DO:
         vTmpDec = DEC(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"rko11_price","0")) -
                   DEC(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"rko1_price","0")) NO-ERROR.
         rproc = TRIM(STRING(vTmpDec, "->>>,>>>,>>>,>>9.99")).
      END.
      WHEN "СумПервВзнПроп" THEN  /* сумма первоначального взноса прописью */
      IF AVAILABLE(loan) THEN
      DO:
         vTmpDec = DEC(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"rko11_price","0")) -
                   DEC(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"rko1_price","0")) NO-ERROR.
         RUN persent (vTmpDec,loan.currency,TRUE,INPUT-OUTPUT rproc).
      END. 
      WHEN "ПРОДУКТ" THEN
      DO:
         mTmpStr = GetXAttrValueEx("loan",
                                   loan.contract + "," + loan.cont-code,
                                   "ПродКод",
                                   "").
         IF {assigned mTmpStr} THEN
            rproc = GetTCodeFld("name", "ПродЛин", mTmpStr, Xdate).
         ELSE
            rproc = "" .
      END. /* else if per eq "Ц" then do: */
      WHEN "П" THEN
      DO:
         mTmpStr = GetXAttrValueEx("loan",
                                   loan.contract + "," + loan.cont-code,
                                   "ВидГаран",
                                   ?).
         IF mTmpStr <> ? THEN
         DO:
            mTmpStr = GetCodeName("ВидГаран",mTmpStr).
            rproc = IF mTmpStr <> ? THEN
                       mTmpStr
                    ELSE
                       "Вид гарантии не установлен".
         END.
         ELSE
            rproc = "Вид гарантии не установлен".

         IF loan.warr-id NE ? THEN
         DO:
            rproc = rproc                              + "\n"
                  + "Поручителем клиента является : "  + "\n".

            IF loan.warr-cat EQ "Ю" THEN
            DO:
                FIND FIRST cust-corp WHERE
                cust-corp.cust-id EQ loan.warr-id
                NO-LOCK  NO-ERROR.
                rproc = rproc                        + " "
                     +  STRING(cust-stat)           + " "
                     +  STRING(cust-corp.name-corp) + "\n"
                     +  " Адрес: " + STRING(cust-corp.addr-of-low[1]).
                IF cust-corp.addr-of-low[2] NE "" THEN
                rproc = rproc + "\n" + STRING(cust-corp.addr-of-low[2]).
			find first buf-cust-ident WHERE (buf-cust-ident.close-date EQ ? OR
			buf-cust-ident.close-date >= today)
			and buf-cust-ident.cust-id eq loan.warr-id
				and buf-cust-ident.class-code eq "p-cust-adr" no-lock.
				if avail buf-cust-ident then do:
					if buf-cust-ident.cust-code-type eq "АдрФакт" then 
						rproc = rproc + "\n" + "Адрес факт.: " + TRIM(STRING(buf-cust-ident.issue)).
					if buf-cust-ident.cust-code-type eq "АдрПроп" then 
						rproc = rproc + "\n" + "Адрес проп.: " + TRIM(STRING(buf-cust-ident.issue)).
					if buf-cust-ident.cust-code-type eq "АдрЮр" then 
						rproc = rproc + "\n" + "Адрес юр.: " + TRIM(STRING(buf-cust-ident.issue)).
				end.
            END.
            IF loan.warr-cat EQ "Ч" THEN
            DO:
				FIND FIRST person WHERE
				person.person-id EQ loan.warr-id
				NO-LOCK NO-ERROR.
				rproc = STRING(person.first-name) + " "
				+ STRING(person.name-last)  + " "
				+ person.document-id + ": " + person.document + "\n"
				+ "Адрес: " + TRIM(STRING(person.address[1])).
				IF person.address[2] NE "" THEN
				   rproc = rproc + "\n" + TRIM(STRING(person.address[2])).
			find first buf-cust-ident WHERE (buf-cust-ident.close-date EQ ? OR
			buf-cust-ident.close-date >= today)
			and buf-cust-ident.cust-id eq loan.warr-id
				and buf-cust-ident.class-code eq "p-cust-adr" no-lock.
				if avail buf-cust-ident then do:
					if buf-cust-ident.cust-code-type eq "АдрФакт" then 
						rproc = rproc + "\n" + "Адрес факт.: " + TRIM(STRING(buf-cust-ident.issue)).
					if buf-cust-ident.cust-code-type eq "АдрПроп" then 
						rproc = rproc + "\n" + "Адрес проп.: " + TRIM(STRING(buf-cust-ident.issue)).
					if buf-cust-ident.cust-code-type eq "АдрЮр" then 
						rproc = rproc + "\n" + "Адрес юр.: " + TRIM(STRING(buf-cust-ident.issue)).
				end.
            END.
         END.  /* else if per eq "П" then do: */
      END.
   END CASE.

   IF per EQ "Р" THEN
   DO:
      rproc = rproc
            + STRING ("За банк :"      ,"X(50)") + "За клиента :"       + "n"
            + STRING (dol-k            ,"X(50)") + dol-z                + "n"
            + STRING(name-k            ,"X(50)") + name-z               + "n"
            + STRING( "_______________","X(50)") + "_________________".
      mFlFormat = TRUE.
   END.
   IF per EQ "В" THEN
   DO:
      IF loan.currency EQ "" THEN
         rproc = " в {&in-LP-C6} ".
      ELSE
      DO:
         FIND FIRST currency WHERE
                    currency.currency EQ loan.currency
            NO-LOCK NO-ERROR.
         rproc = "в валюте: " + currency.name-currenc.
      END.
   END.
   IF per EQ "Ср" THEN
   DO:
      RUN "amtstr.p" (loan.end-date - loan.open-date,NO,OUTPUT c2,OUTPUT c3).
      rproc = STRING(loan.end-date - loan.open-date) + "(" + c2 + ")".
   END.
   IF per EQ "%" THEN
   DO:
      IF loan.contract EQ "Кредит" THEN
         RUN RE_L_ACCT IN h_Loan(loan.contract,
                                 loan.cont-code,
                                 "КредПроц",
                                 loan.since,
                                 BUFFER loan-acct).

      ELSE
         IF loan.contract EQ "Депоз" THEN
            RUN RE_L_ACCT IN h_Loan(loan.contract,
                                    loan.cont-code,
                                    "КредПроц",
                                    loan.since,
                                    BUFFER loan-acct).
      IF AVAILABLE loan-acct THEN
         rproc = STRING(delFilFromAcct(loan-acct.acct)).
   END.

   IF per MATCHES "ЧНомДок" OR per MATCHES "ЧТипДок" OR per MATCHES "ЧВыдДок" OR per MATCHES "ЧВыдДокП" OR
      per MATCHES "ЧНомДок|*" OR per MATCHES "ЧТипДок|*" OR per MATCHES "ЧВыдДок|*" THEN
   DO:
       IF loan.cust-cat EQ "Ч" THEN
       DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR.

         IF AVAILABLE person THEN
             CASE ENTRY(1,per,'|'):
             WHEN "ЧТипДок" THEN
                 rproc = person.document-id.
             WHEN "ЧНомДок" THEN
                 rproc = person.document.
             WHEN "ЧВыдДок" THEN DO:
                 rproc = REPLACE(fGetDocIssue(person.person-id),",",", код подразделения: ").
				 SUBSTRING(rproc,LENGTH(rproc, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
				 rproc = REPLACE(rproc,"/",".").
			 END.
			 WHEN "ЧВыдДокП" THEN DO: /* ayv дата формата 99 месяца 9999 года */
				 rproc = fGetDocIssue(person.person-id).
				 vTmpDate = date(SUBSTRING(rproc,LENGTH(rproc, "CHARACTER") - 9,10,"CHARACTER")).
				 mTmpStr = ", " + string(day(vTmpDate)) + " " + entry(month(vTmpDate),{&Months}) + " " + string(year(vTmpDate)) + " года, код подразделения: ".
				 rproc = REPLACE(fGetDocIssue(person.person-id),",",mTmpStr).
				 rproc = SUBSTRING(rproc,1,LENGTH(rproc, "CHARACTER") - 9).
   			 END.
             END CASE.

         IF NUM-ENTRIES(per,'|') > 1 THEN DO:
             ASSIGN
                vFormat    = ENTRY(2,per,'|')
                mFlFormat  = TRUE.
             rproc = STRING(rproc,vFormat).
         END.
       END.
       ELSE rproc = "".
   END.
   
   /*ayv ИНН*/
   IF per EQ "КлИНН" THEN /*IF per EQ "ИНН" THEN*/
   DO:
	   IF loan.cust-cat EQ "Ч" THEN
		  DO :
			 FIND FIRST person WHERE person.person-id EQ loan.cust-id
				NO-LOCK NO-ERROR .
			 IF AVAILABLE person THEN 
				rproc = person.inn .
		  END .
		  ELSE IF loan.cust-cat EQ "Ю" THEN
		  DO :
			 FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id
				NO-LOCK NO-ERROR .
			 IF AVAILABLE cust-corp THEN 
				rproc = cust-corp.inn .
      END .
	END.

  IF strpar MATCHES "КлОГРН" THEN  /*IF per EQ "КПП" THEN*/
    DO:
     IF loan.cust-cat EQ "Ч" THEN
      DO :
       FIND FIRST person WHERE person.person-id EQ loan.cust-id
        NO-LOCK NO-ERROR .
       IF AVAILABLE person THEN 
        rproc = GetXAttrValueEx("person",STRING(person.person-id),"ОГРН","") .
      END .
      ELSE IF loan.cust-cat EQ "Ю" THEN
      DO :
       FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id
        NO-LOCK NO-ERROR .
       IF AVAILABLE cust-corp THEN 
        rproc = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ОГРН","") .
      END .
    END.

     /*pda КПП*/
    IF strpar MATCHES "КлДр|КПП" THEN  /*IF per EQ "КПП" THEN*/
    DO:
     IF loan.cust-cat EQ "Ч" THEN
      DO :
       FIND FIRST person WHERE person.person-id EQ loan.cust-id
        NO-LOCK NO-ERROR .
       IF AVAILABLE person THEN 
        rproc = GetXAttrValueEx("person",STRING(person.person-id),"КПП","") .
      END .
      ELSE IF loan.cust-cat EQ "Ю" THEN
      DO :
       FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id
        NO-LOCK NO-ERROR .
       IF AVAILABLE cust-corp THEN 
        rproc = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"КПП","") .
      END .
    END.  
	
   /*ayv*/
   IF per MATCHES "ЧДок|*" THEN
   DO:
	   IF loan.cust-cat EQ "Ч" THEN
       DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR.

         IF AVAILABLE person THEN
             CASE ENTRY(2,per,'|'):
             WHEN "Тип" THEN
                 rproc = person.document-id.
			 WHEN "Ном" THEN
				 rproc = person.document.
			 /* ПНом и ПСер используются только для российских паспортов*/
          WHEN "ПСер" THEN 
          DO:
             IF NUM-ENTRIES(person.document," ") EQ 3
             THEN rproc = ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ").
             ELSE rproc = "".    
          END.
			 WHEN "ПНом" THEN
          DO:
             IF NUM-ENTRIES(person.document," ") EQ 3
				 THEN rproc = ENTRY(3,person.document," ").
				 ELSE rproc = person.document.
          END.
			 WHEN "День" THEN
				 rproc = ENTRY(1,GetXAttrValue("person",STRING(person.person-id),"Document4Date_vid"),"/").
			 WHEN "МесГ" THEN DO:
				 vTmpDate = date(GetXAttrValue("person",STRING(person.person-id),"Document4Date_vid")).
				 rproc = entry(month(vTmpDate),{&Months}) + " " + string(year(vTmpDate)).
			 END.
			 WHEN "Дата" THEN DO:
				 rproc = GetXAttrValue("person",STRING(person.person-id),"Document4Date_vid").
				 rproc = REPLACE(rproc,"/",".").
             END.
			 WHEN "ДатаП" THEN DO:
				 vTmpDate = date(GetXAttrValue("person",STRING(person.person-id),"Document4Date_vid")).
				 rproc = string(day(vTmpDate)) + " " + entry(month(vTmpDate),{&Months}) + " " + string(year(vTmpDate)) + " года".
			 END. 
			 WHEN "Выд" THEN DO:
                 rproc = REPLACE(fGetDocIssue(person.person-id),",",", код подразделения: ").
				 SUBSTRING(rproc,LENGTH(rproc, "CHARACTER") - 10 ,1,"CHARACTER") = ", ".
				 rproc = SUBSTRING(rproc,1,LENGTH(rproc, "CHARACTER") - 12).
			 END.
             END CASE.
       END.
       ELSE rproc = "".
   END.

   IF per MATCHES "ЧАдрес|*" THEN
   DO:
       ASSIGN
          mTmpStr = STRING(ENTRY(2,per,'|'))
          mOldAdr = FGetSetting("ВарПечатАдр","",?) NE "Новый".

       IF loan.cust-cat EQ "Ч" THEN
       DO:
         FIND FIRST person WHERE
                    person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR.

         IF AVAILABLE person THEN
            vReg = GetXAttrValue("person",STRING(person.person-id),"КодРегГНИ").
         vRegName = GetCodeNameEx("КодРегГНИ",vReg,"").
         IF NUM-ENTRIES(ENTRY(2,per,'|'),'_') EQ 1 THEN
         DO:
            IF mTmpStr EQ "КодРегГНИ" THEN
               rproc = vRegName.
            ELSE
            DO:
               IF NOT mOldAdr THEN
                  CASE INT64(mTmpStr):
                      WHEN 6 THEN
                         mPrefAdr = "д.".
                      WHEN 7 THEN
                         mPrefAdr = "стр.".
                      WHEN 8 THEN
                         mPrefAdr = "корп.".
                      WHEN 9 THEN
                         mPrefAdr = "кв.".
                      OTHERWISE
                         mPrefAdr = "".
                  END CASE.
               rproc = mPrefAdr + ENTRY(INT64(mTmpStr),person.address[1],",").
            END.
         END.
         ELSE
         DO vIntTmp = 1 TO NUM-ENTRIES(mTmpStr,'_'):
             vMes[vIntTmp] = ENTRY(vIntTmp,mTmpStr,'_').
             IF NOT mOldAdr THEN
             DO:
                CASE vMes[vIntTmp]:
                   WHEN "6" THEN
                      mPrefAdr = "д.".
                   WHEN "8" THEN
                      mPrefAdr = "стр.".
                   WHEN "7" THEN
                      mPrefAdr = "корп.".
                   WHEN "9" THEN
                      mPrefAdr = "кв.".
                   OTHERWISE
                      mPrefAdr = "".
                END CASE.
                IF     NOT mFlDot
                   AND vMes[vIntTmp] NE "КодРегГНИ"
                   AND INT64(vMes[vIntTmp]) GE 5 THEN
                   ASSIGN
                      mSepAdr = ". "
                      mFlDot  = TRUE.
                ELSE
                   mSepAdr = ", ".
             END.
             ELSE
                mSepAdr = " ".

             IF vMes[vIntTmp] EQ "КодРегГНИ" THEN
                rproc = rproc + " " + vRegName.
             ELSE IF ENTRY(INT64(vMes[vIntTmp]),person.address[1],",") NE "" THEN DO:
                IF NUM-ENTRIES(per,'|') > 2 THEN DO:
                   vValue = ENTRY(vIntTmp,ENTRY(3,per,'|'),'_').
                   IF vIntTmp EQ 1 THEN
                      rproc = vValue + rproc + mSepAdr + mPrefAdr + ENTRY(INT64(vMes[vIntTmp]),person.address[1],",") + " ".
                   ELSE
                      rproc = rproc + vValue + mSepAdr + mPrefAdr + ENTRY(INT64(vMes[vIntTmp]),person.address[1],",") + " ".
                END.
                ELSE DO:
                   IF vIntTmp EQ 1 THEN
                      rproc = ENTRY(INT64(vMes[vIntTmp]),person.address[1],",").
                   ELSE
                      rproc = rproc + mSepAdr + mPrefAdr + ENTRY(INT64(vMes[vIntTmp]),person.address[1],",").
                END.
             END.
         END.
         IF NUM-ENTRIES(per,'|') > 3  THEN
         DO:
            ASSIGN
                mFlFormat  = TRUE
                vFormat    = ENTRY(4,per,'|').
            rproc = STRING(rproc,vFormat).
         END.
       END.
       ELSE
          rproc = "".
   END.
   
   /* vvv */
   IF per MATCHES "КодПродМПроп" THEN
   DO:
		FIND LAST loan-cond WHERE loan-cond.cont-code EQ loan.cont-code
                             AND loan-cond.since <= XDate NO-LOCK NO-ERROR.

		mTmpStr = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE)).
		rproc   = STRING(INT64(GetXAttrValueEx("loan-cond",
                               mTmpStr,
                               "NMonthes",
                               "0"))          +
               INT64(GetXAttrValueEx("loan-cond",
                               mTmpStr,
                               "NYears",
                               "0")) * 12     +
               INT64(GetXAttrValueEx("loan-cond",
                               mTmpStr,
                               "NDays",
                               "0")) / 30, ">>>>>>>>>>>>>>>>9").
		RUN x-amtstr.p (rproc,"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
		IF vDecStr EQ "00" THEN
			rproc = STRING(vAmtStr) + vProcString.
		ELSE
		DO:
			rproc = STRING(vAmtStr) + "целых ".
			RUN x-amtstr.p (DEC(vDecStr),"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
			rproc = rproc + LC(STRING(vAmtStr)) + "сотых " + vProcString.
		END.
   END.   
	/* vvv */

   /* График пдатежей */
   /* Третьим параметром можно указать тип графика: ОД - по основному долгу */
   IF per MATCHES "График|*" THEN
   DO:
      vCount = INT64(ENTRY(2,per,'|')).
      IF fl THEN
         DO:
            PUT STREAM fil UNFORMATTED "График погашения кредита : "
               "Дата платежа  " AT 5 "Сумма " AT 25 "Сумма начисленных " AT 40
               "Общая сумма " AT 68
               "погашения " AT 25 "процентов" AT 48 "платежа " AT 72 SKIP.

            PUT STREAM fil "--------------------------------------------------------------------------------------" AT 5 SKIP.

            fl = NO.
         END.

      /* если vCount = 1 */
      IF vCount EQ 3 THEN /* На весь договор */
      DO:
          vData = loan.open-date.
          vData1 = loan.end-date.
      END.
      IF vCount EQ 2 THEN /* От начала действия условий и до конца договора */
      DO:
           vData = xDate.
           vData1 = loan.end-date.
      END.
      IF vCount EQ 1 THEN /* От начала действия условий и до начала действия следующих */
      DO:
          vData = xDate.
          IF NOT CAN-FIND(LAST loan-cond WHERE
                          loan-cond.cont-code EQ loan.cont-code
                      AND loan-cond.since >= xDate) THEN
          vData1 = loan.end-date.
          ELSE
          DO:
             FIND LAST loan-cond WHERE
                       loan-cond.cont-code EQ loan.cont-code
                   AND loan-cond.since >= xDate.
             vData1 = loan-cond.since.
          END.

      END.
      IF vCount EQ 4 THEN
      DO:
         vData = loan.open-date.
         vData1 = XDate.
      END.

      FOR EACH term-obl {wh-t &f=term-obl &c="/*"}
           AND term-obl.end-date >= vData
           AND term-obl.end-date <= vData1
           AND term-obl.idnt EQ 1:

              CREATE obl.
              ASSIGN
                 obl.end-date = term-obl.end-date
                 obl.amt      = term-obl.amt
                 .
              vSymm[1] = vSymm[1] + obl.amt.
      END.

      FOR EACH term-obl {wh-t &f=term-obl &c="/*"}
           AND term-obl.end-date >= vData
           AND term-obl.end-date <= vData1
           AND term-obl.idnt EQ 3:

               IF CAN-FIND(FIRST obl WHERE obl.end-date EQ term-obl.end-date ) THEN
               DO:
                  FIND FIRST obl WHERE obl.end-date EQ term-obl.end-date.
                  ASSIGN
                     obl.amt2 = term-obl.amt.
               END.
               ELSE
               DO:
                   CREATE obl.
                   ASSIGN
                      obl.end-date = term-obl.end-date
                      obl.amt2     = term-obl.amt
                      .
               END.

               IF term-obl.end-date EQ vTmpDate AND RECID(term-obl) NE vTmpRec THEN
               DO:
                  CREATE obl.
                  ASSIGN
                     obl.end-date = term-obl.end-date
                     obl.amt2     = term-obl.amt
                     .
               END.
               vSymm[2] = vSymm[2] + obl.amt2.
               vTmpRec  = RECID(term-obl).
               vTmpDate = term-obl.end-date.
      END.

      FOR EACH obl BY obl.end-date:
          obl.amt3 = obl.amt + obl.amt2.
          vSymm[3] = vSymm[3] + obl.amt3.
          IF obl.amt  EQ 0 THEN vMes[1] = "".
          ELSE
              vMes[1] = STRING(obl.amt,"->,>>>,>>>,>>>,>>9.99").
          IF obl.amt2 EQ 0 THEN vMes[2] = "".
          ELSE
              vMes[2] = STRING(obl.amt2,"->,>>>,>>>,>>>,>>9.99").
          IF obl.amt3 EQ 0 THEN vMes[3] = "".
          ELSE
              vMes[3] = STRING(obl.amt3,"->,>>>,>>>,>>>,>>9.99").

          PUT STREAM fil UNFORMATTED obl.end-date AT 5 vMes[2] AT 13 vMes[1] AT 36 vMes[3] AT 58.
      END.
      IF vSymm[1]  EQ 0 THEN vMes[1] = "".
      ELSE
         vMes[1] = STRING(vSymm[1],"->,>>>,>>>,>>>,>>9.99").
      IF vSymm[2] EQ 0 THEN vMes[2] = "".
      ELSE
         vMes[2] = STRING(vSymm[2],"->,>>>,>>>,>>>,>>9.99").
      IF vSymm[3] EQ 0 THEN vMes[3] = "".
      ELSE
         vMes[3] = STRING(vSymm[3],"->,>>>,>>>,>>>,>>9.99").

      PUT STREAM fil "--------------------------------------------------------------------------------------" AT 5 SKIP.
      PUT STREAM fil UNFORMATTED CAPS("итого :") AT 6 vMes[2] AT 13 vMes[1] AT 36 vMes[3] AT 58 SKIP(1).

   END. /* конец График */

   /* График ОД для печати в Word */
   IF per MATCHES "График_Word*" THEN
   DO:
      IF NUM-ENTRIES(per,"|") EQ 2 THEN
         vI = INT64(ENTRY(2,per,"|")).
      ELSE vI = 1.

      IF NUM-ENTRIES(per,"|") EQ 3 THEN
         mGrTyp = IF ENTRY(3,per,"|") EQ "ОД" THEN 3 ELSE 1.
      ELSE mGrTyp = 3.

      IF NUM-ENTRIES(per,"|") EQ 4 THEN
         mFlOst = TRUE.
      ELSE mFlOst = FALSE.

      rproc = "[table]:" + STRING(vI) + "~n".

      FOR EACH term-obl WHERE term-obl.contract  EQ loan.contract
                          AND term-obl.cont-code EQ loan.cont-code
                          AND term-obl.idnt      EQ mGrTyp
      NO-LOCK:
         mTmpStr = STRING(term-obl.amt-rub,"zzz,zzz,zzz,zz9.99") + "~n".
         IF mFlOst THEN
         DO:
            FIND FIRST bterm-obl WHERE bterm-obl.contract  EQ loan.contract
                                   AND bterm-obl.cont-code EQ loan.cont-code
                                   AND bterm-obl.idnt      EQ 2
                                   AND bterm-obl.end-date  EQ term-obl.end-date
            NO-LOCK NO-ERROR.
            IF AVAIL bterm-obl THEN
            DO:
               mTmpStr = mTmpStr + " " + STRING(bterm-obl.amt-rub,"zzz,zzz,zzz,zz9.99") + "~n".
            END.
         END.
         mTmpStr = mTmpStr + " " + STRING(term-obl.end-date) + "~n".

         rproc = rproc + mTmpStr.
      END.
      rproc = rproc + "[table/]:" + STRING(vI) + "~n".
   END.

   IF per MATCHES ("ПОС|*") THEN
   DO:
      mTmpStr = GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "UniformBag","") .
      IF     NUM-ENTRIES(per,"|") EQ 2
         AND ENTRY(2,per,"|")     EQ "П" THEN
         rproc = GetCodeName("UnifrmBag", mTmpStr).
      ELSE
         rproc = mTmpStr.
   END.

   /* kam */
   /* выбираем значения из справочника получателей средств */
   IF per MATCHES ("polsr|*") or per MATCHES ("prodav|*") THEN DO:
        IF per MATCHES ("polsr|*") THEN DO:
            mTmpStr = GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "ПолучСредств", ""). /* получатель средств */
            IF TRIM(mTmpStr) = "" THEN mTmpStr = GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "Продавец", ""). /* получатель средств */
        END.
        ELSE DO:
            mTmpStr = GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "Продавец", ""). /* получатель средств */
        END.
        find first bcode where bcode.class = 'strahpol' and bcode.code = mTmpStr no-lock no-error.
        if avail bcode then do:
          CASE ENTRY(2, per, '|'):
            WHEN "Name" THEN DO:    /* наименование продавца */
                rproc = trim(bcode.description[1]).
            END.
            WHEN "INN" THEN DO:    /* INN продавца */
                rproc = trim(bcode.name).
            END.
            WHEN "Acct" THEN DO:    /* расчетный счет продавца */
                rproc = trim(bcode.description[2]).
            END.
            WHEN "BankName" THEN DO:    /*  */
                rproc = trim(bcode.misc[1]).
            END.            
            WHEN "CorrAcct" THEN DO:    /*  */
                rproc = trim(bcode.misc[2]).
            END. 
            WHEN "BIC" THEN DO:    /*  */
                rproc = trim(bcode.misc[3]).
            END.
            WHEN "Tel" THEN DO:    /* телефон */
                rproc = trim(bcode.misc[4]).
            END.            
          END CASE.
        end.
   END.

   IF per MATCHES ("prodav_*") THEN DO:
        mTmpStr = GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "Продавец", ""). /* получатель средств */
        find first bcode where bcode.class = 'strahpol' and bcode.code = mTmpStr no-lock no-error.
        if avail bcode then do:
		  mTmpStr = substring(bcode.description[3],1,6) + substring(bcode.description[3],7,6).
          find first person where person.document eq mTmpStr no-lock no-error.
		  CASE ENTRY(2, per, '_'):
            WHEN "name" THEN DO:    /* наименование продавца */
                rproc = trim(bcode.description[1]).
            END.
            WHEN "passport" THEN DO:    /* паспортные данные продавца */
                if avail person then do:
					rproc = fGetDocIssue(person.person-id).
					vTmpDate = date(SUBSTRING(rproc,LENGTH(rproc, "CHARACTER") - 9,10,"CHARACTER")).
					mTmpStr = ", " + string(day(vTmpDate)) + " " + entry(month(vTmpDate),{&Months}) + " " + string(year(vTmpDate)) + " года, код подразделения: ".
					rproc = REPLACE(fGetDocIssue(person.person-id),",",mTmpStr).
					rproc = trim(person.document) + ', выдан ' + SUBSTRING(rproc,1,LENGTH(rproc, "CHARACTER") - 9).
				end.
				else do:
					vTmpDate = date(string(trim(bcode.misc[7]), "99.99.9999")).
					rproc = trim(bcode.description[3]) + ', выдан ' + trim(bcode.misc[8]) + ", "+ string(day(vTmpDate)) + " " + entry(month(vTmpDate),{&Months}) + " " + string(year(vTmpDate)) + ' года '.
				end.
			END.
            WHEN "address" THEN DO:    /* адрес продавца */
			   if avail person then 
					RUN RetAdr.p(person.person-id,"Ч","АдрПроп",?,OUTPUT rproc).
				else
					rproc = trim(bcode.val).
            END.
            WHEN "birthday" THEN DO:    /* ayv */
                vTmpDate = date(string(trim(bcode.kind), "99.99.9999")).
				rproc = string(day(vTmpDate)) + " " + entry(month(vTmpDate),{&Months}) + " " + string(year(vTmpDate)).
            END.
			WHEN "okon" THEN DO:
				if avail person then 
					if person.gender EQ TRUE THEN 
						rproc = "ый".
					ELSE
						rproc = "ая".
				else rproc = "ый".
			END.
          END CASE.
        end.
		else do:
			if GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "", "") NE "" THEN DO:
				mTmpStr = ENTRY(5,GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "", ""),"^").
				CASE ENTRY(2, per, '_'):
					WHEN "name" THEN     /* наименование продавца */
						rproc = trim(mTmpStr).
					WHEN "okon" THEN
						rproc = "ый".
				END CASE.
			end.
		end.
   END.

   /* расшифровка ДР ГВК */
   IF per EQ "GVKFull"  THEN
   DO:
         mTmpStr = GetXattrValueEx(IF    loan.cust-cat EQ "Ч"
                                      OR loan.cust-cat EQ "П" THEN "person"
                                   ELSE "cust-corp",
                                   STRING(loan.cust-id),"ГВК","").
         rproc = GetCodeName("ГВК",mTmpStr).
         IF rproc EQ ? THEN
            rproc  = "".
   END.

   /* Связанность с банком */
   IF per EQ "СвязСБанк"  THEN
   DO:
      mTmpStr = GetXattrValueEx(IF    loan.cust-cat EQ "Ч"
                                   OR loan.cust-cat EQ "П" THEN "person"
                                ELSE "cust-corp",
                                STRING(loan.cust-id),"Клиент","").
      rproc = GetCodeName("Клиент",mTmpStr).
      IF rproc EQ ? THEN
         rproc  = "".
   END.

   /* Связанность с банком */
   IF per EQ "СуммаДог"  THEN
   DO:
      FOR FIRST term-obl WHERE term-obl.contract  EQ loan.contract
                           AND term-obl.cont-code EQ loan.cont-code
                           AND term-obl.idnt      EQ 2
      NO-LOCK BY term-obl.end-date:
         rproc  = TRIM(string(term-obl.amt-rub,'zzz,zzz,zzz,zz9.99')).
      END.
      IF NOT {assigned rproc} THEN
         rproc  = "".
   END.

   /* Вывод в отчет произвольного значения запрашиваемого при построении отчета */
   IF per MATCHES "Запр|*" THEN
   DO:
         vMes[1]    = ENTRY(2,per,"|").
      IF NUM-ENTRIES(per, "|") GT 2 THEN
      ASSIGN
         vMes[2] = ENTRY(3,per,'|')
         vMes[2] = REPLACE(vMes[2],"_"," ")
      .

       FORM
           j      AS CHARACTER FORMAT "x(70)" SKIP
       rproc                   FORMAT "x(70)" SKIP
       WITH FRAME trt OVERLAY NO-LABELS ROW 3 COLUMNS 5
       TITLE "Введите значение".

       FORM
       k          AS CHARACTER FORMAT "x(70)" SKIP
       rproc                   FORMAT "x(70)" SKIP
       "Введите валюту:"                      SKIP
       vSrcChar                FORMAT "x(3)"  SKIP
       WITH FRAME trt1 OVERLAY NO-LABELS ROW 3 COLUMNS 5
       TITLE "Введите значение".

      FORM
         label_date  AS CHAR FORMAT "x(70)" SKIP
         mInpDate          SKIP
      WITH FRAME frame_date OVERLAY NO-LABELS ROW 3 COLUMNS 5
      TITLE "Введите дату".

      FORM
         label_sum  AS CHAR FORMAT "x(70)" SKIP
         mInpSumm          SKIP
      WITH FRAME frame_sum OVERLAY NO-LABELS ROW 3 COLUMNS 5
      TITLE "Введите сумму".

      CASE vMes[1]:
         WHEN "Дпр" THEN
         DO:
          DISPLAY
             vMes[2] @ k
                WITH FRAME trt1.
          SET rproc WITH FRAME trt1.
          SET vSrcChar WITH FRAME trt1.
          HIDE FRAME trt1.
       FIND FIRST currency WHERE currency.currency EQ vSrcChar NO-LOCK NO-ERROR.
       IF NOT AVAIL currency THEN
       DO:
          MESSAGE "Указаная валюта отсутствует в справочнике валют!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
          rproc = "".
          RETURN.
       END.
         END.
         WHEN "Дата" THEN
         DO:
            DISPLAY
               vMes[2] @ label_date
            WITH FRAME frame_date.
            SET mInpDate WITH FRAME frame_date.
            HIDE FRAME frame_date.
         END.
         WHEN "Сумма" THEN
         DO:
            DISPLAY
               vMes[2] @ label_sum
                     0 @ mInpSumm
            WITH FRAME frame_sum.
            SET mInpSumm WITH FRAME frame_sum.
            HIDE FRAME frame_sum.
         END.
         OTHERWISE
         DO:
            DISPLAY
               vMes[2] @ j
                  WITH FRAME trt.
            SET rproc WITH FRAME trt.
            HIDE FRAME trt.
         END.
      END CASE.

       CASE vMes[1]:
           WHEN "Д" THEN
              rproc = STRING(DEC(rproc),"->,>>>,>>>,>>>,>>9.99").
           WHEN "Дпр" THEN
           DO:
              RUN "x-amtstr.p" (dec(rproc),
                                vSrcChar,
                                YES,
                                YES,
                                OUTPUT vAmtStr,
                                OUTPUT vDecStr).
              rproc = STRING(DEC(rproc),"->,>>>,>>>,>>>,>>9.99") + " ( " + vAmtStr + " " + vDecStr + ")" .
           END.
           WHEN "Н" THEN
              rproc = STRING(INT64(rproc),"->,>>>,>>>,>>>,>>9").
           WHEN "С" THEN
              rproc = rproc.
         WHEN "Дата" THEN
            rproc = STRING (mInpDate).
         WHEN "Сумма" THEN
            rproc = STRING (mInpSumm).
       END CASE.
   END.

   /* sku реквизиты подразделения по договору */
   IF per MATCHES ("branch|*") THEN
   DO:
	rproc = "".
	FIND FIRST branch WHERE branch.branch-id EQ loan.branch-id NO-LOCK NO-ERROR.
	IF AVAIL branch THEN DO:
    	    hBufFld = BUFFER branch:BUFFER-FIELD (ENTRY(2, per, '|')) NO-ERROR.
            IF VALID-HANDLE (hBufFld)
              THEN rproc = hBufFld:BUFFER-VALUE.
    	      ELSE
    	    IF IsXAttrIndexed ("branch",ENTRY(2, per, '|')) NE ? /* ? - когда нет реквизита */
              THEN rproc = GetBranchXAttrValue (branch.branch-id, ENTRY(2, per, '|')).
	END.
   END.
   /* sku название продукта по ДР на договоре */
   IF per MATCHES ("product") THEN
   DO:
	rproc = GetXAttrValueEx("loan",
                                     loan.contract + "," + loan.cont-code,
                                     "ПродКод",
                                     "").
	FIND FIRST code
	    WHERE code.class = "ПродЛин"
	      AND code.code = rproc no-lock no-error.
	IF AVAIL code THEN rproc = code.name.
	ELSE rproc = loan.cont-type.
   END.
   /* sku сумма кредита цифрами и прописью раздельно ( дубликат lcond(O) ) */
   IF per MATCHES ("lcond|*") THEN
   DO:
    FIND FIRST xloan {wh-t &f=xloan &c="/*"}
       AND xloan.since GT loan-cond.since
       NO-LOCK NO-ERROR.

    IF AVAILABLE xloan THEN
       dat = xloan.since.
    ELSE
       dat = loan.end-date + 1.
      FIND FIRST term-obl {wh-t &f=term-obl &c="/*"}
             AND term-obl.idnt     EQ 2
             AND term-obl.end-date LE loan-cond.since
             AND term-obl.end-date LT dat
      NO-LOCK NO-ERROR.

      IF NOT AVAILABLE term-obl THEN
         FIND LAST term-obl {wh-t &f=term-obl &c="/*"}
               AND term-obl.idnt     EQ 2
               AND term-obl.end-date LE loan-cond.since
         NO-LOCK NO-ERROR.

      IF AVAILABLE term-obl AND ENTRY(2, per, '|')='sumkr' THEN
      DO:
        IF NUM-ENTRIES(per,'|')<3 THEN
        DO:
         rproc = TRIM(STRING(term-obl.amt, "->>>,>>>,>>>,>>9.99")).
        END. ELSE
        CASE ENTRY(3, per, '|'):
        WHEN "Пр" THEN
        DO:
         RUN "x-amtstr.p" (term-obl.amt,loan.currency, YES , YES, OUTPUT c1, OUTPUT c2).
         rproc = TRIM(STRING(term-obl.amt, "->>>,>>>,>>>,>>9.99")) +
            " (" + trim(c1) + " " + trim(c2) + ")".

        END.
        WHEN "Пр2" THEN
        DO:
         RUN "x-amtstr.p" (term-obl.amt,loan.currency, YES , YES, OUTPUT c1, OUTPUT c2).
         rproc = trim(c1) + " " + trim(c2).

        END.
		WHEN "РГС" THEN /*ayv расчет страховой премии для РГС прог.2*/
		DO:
			IF term-obl.amt LE 1000000 THEN DO:
			    RUN "x-amtstr.p" (term-obl.amt,loan.currency, YES , YES, OUTPUT c1, OUTPUT c2).
				rproc = TRIM(STRING(term-obl.amt, "->>>,>>>,>>>,>>9.99")) +
				" (" + trim(c1) + " " + trim(c2) + ")".
			END.
			ELSE
				rproc = "1,000,000.00 (Один миллион рублей 00 копеек)".
		END.
        END CASE.
      END.
    END.

   /* sku Вывод в отчет реквизитов обеспечения 
      сначала нужно вызвать dog(lgar|Выбор)
      затем можно уже вызывать 
      dog(lgar|Кл|реквизит)
      dog(lgar|ДР|реквизит)
    */
   IF per MATCHES ("lgar|*") THEN
   DO:
	rproc = "".
	/* поиск ранее выбранного договора*
	IF lgar_recid <> ? THEN DO:
    	    FIND FIRST term-obl OF loan WHERE
                    term-obl.idnt     EQ 5
                    AND RECID(term-obl) EQ lgar_recid
        	NO-LOCK NO-ERROR.
            IF NOT AVAIL term-obl THEN lgar_recid = ?.
	END. ELSE DO:
	    RELEASE term-obl.
	END.*/
        CASE ENTRY(2, per, '|'):
        WHEN "Обсумма" THEN
        DO:
    	    FIND FIRST term-obl OF loan WHERE
                    term-obl.idnt     EQ 5
                /*AND term-obl.end-date GE bloan.open-date*/
            NO-LOCK NO-ERROR.

    	    IF AVAILABLE term-obl THEN
	    DO:
		rproc = TRIM(STRING(term-obl.amt-rub,"->>>,>>>,>>9.99")).
    		IF NUM-ENTRIES(per,'|') > 2 AND
        	    ENTRY(3,per,'|') MATCHES ("Пр*") THEN
    		DO:
        	    CASE ENTRY(3,per,'|'):
        	    WHEN "Пр" THEN
        		RUN FrmtAmt(term-obl.amt-rub,loan.currency,1, OUTPUT rproc).
        	    WHEN "Пр2" THEN
        		RUN FrmtAmt(term-obl.amt-rub,loan.currency,2, OUTPUT rproc).
        	    WHEN "Пр3" THEN DO:
        		RUN x-amtstr.p (term-obl.amt-rub, loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
        		rproc = vAmtStr + " " + vDecStr.
        	    END.
        	    END CASE.
    		END.
    	    END.
	END.
        /*WHEN "Выбор" THEN * выбор договора обеспечения *
        DO:
	    FOR EACH term-obl WHERE term-obl.contract   EQ loan.contract
                          AND term-obl.cont-code  EQ loan.cont-code
                          AND term-obl.class-code EQ "term-obl-gar" 
                          NO-LOCK:
                          
        	vVidDogOb = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "ВидДогОб", "*").
        	vVidOb    = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "ВидОб", "*").
            
        	FIND FIRST code WHERE code.code EQ vVidOb NO-LOCK NO-ERROR.
        	
        	vCodeVal = IF AVAIL code THEN code.name ELSE "".
         
        	IF CAN-DO(vVidDogOb + "," + "Все", ENTRY(3, per, '|')) THEN 
        	DO:
            	    vFrameTitle = " [ ВЫБОР ДОГОВОРА ОБЕСПЕЧЕНИЯ ] ".
                    CREATE tt.
                    ASSIGN
                                    tt.NomDog  = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "НомДогОб", "*")
                                    tt.NomPP   = term-obl.nn
                                    tt.CodeVal = CAPS(vCodeVal)
                                    tt.term-obl-id = recid(term-obl)
                                    *tt.ChVal   = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "НомДогОб", "*")*
                                    .
                END.
            END. 
    	    b_term:NUM-LOCKED-COLUMNS = 2.
    	    b_term:TITLE = vFrameTitle.

    	    OPEN QUERY q_term FOR EACH tt NO-LOCK BY tt.NomPP.

    	    PAUSE 0.
    	    VIEW b_term.

    	    ENABLE ALL WITH FRAME f_term.
    	    WAIT-FOR ENTER,ESC OF FRAME f_term.
    	    HIDE FRAME f_term.
    	    IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN DO:
    		rproc = tt.NomDog.
    		lgar_recid = tt.term-obl-id.
    	    END. ELSE DO:
    		lgar_recid = ?.
    		RETURN "NO-APPLY".
    	    END.
	END.*
        WHEN "ДР" THEN * данные клиента по договору обеспечения *
        DO:
    	    rproc = GetXAttrValueEx ("term-obl",
                                 Loan.Contract + "," + Loan.Cont-Code + ",5,"
                                  + STRING(term-obl.end-date) + ","
                                  + STRING(term-obl.nn),
                                 ENTRY(3, per, '|'),
                                 "").

    	    IF rproc <> "" THEN
    	    DO:
        	* Если ДР определен на классификаторе, то по коду этого
        	    классификатора возвращаем его наименование *
        	IF GetCodeName(ENTRY(3, per, '|'), rproc) <> ? THEN
        	    rproc = GetCodeName(ENTRY(3, per, '|'),rproc).
    	    END.
	END.
        WHEN "Кл" THEN * данные клиента по договору обеспечения *
        DO:
    	    RUN GetXattr (
    		(IF term-obl.symbol EQ "Ч" THEN "person" ELSE "cust-corp"), 
    		ENTRY(3, per, '|'), BUFFER xattr).
    	    IF AVAIL xattr THEN
    	    DO:
        	IF xattr.progress-field THEN DO:
			IF term-obl.symbol EQ "Ч" THEN
			    rproc = GetBufferValue("person",
					"WHERE person.person-id EQ " + STRING(term-obl.fop),
					STRING(ENTRY(3, per, '|'))) NO-ERROR.
			ELSE
			    rproc = GetBufferValue("cust-corp",
					"WHERE cust-corp.cust-id EQ " + STRING(term-obl.fop),
					STRING(ENTRY(3, per, '|'))) NO-ERROR.
        	END. ELSE DO:
            	    rproc = GetXAttrValueEx((IF term-obl.symbol EQ "Ч" THEN "person" ELSE "cust-corp"),
                                     STRING( term-obl.fop),
                                     ENTRY(3, per, '|'),
                                     "").
        	END.
        	IF {assigned rproc} THEN
                DO:
            	    /* Если ДР определен на классификаторе, то по коду этого
            	       классификатора возвращаем его наименование */
            	    IF xattr.Domain-Code NE "" THEN
            	    DO:
            	    mTmpStr = GetCodeName(xattr.Domain-Code,rproc).
            	    IF {assigned mTmpStr} THEN
                	rproc = mTmpStr.
            	END.
            END.
	END.*/
	END CASE.
   END.
   /* sku Вывод в отчет реквизитов страхования */
   IF per MATCHES ("insurance|*") THEN
   DO:
 /*message 'insurance' view-as alert-box. */
    FOR EACH bloan
	WHERE
        bloan.PARENT-CONT-CODE EQ loan.cont-code
        AND bloan.class-code EQ "insurance"
        AND bloan.contract   EQ 'СТРАХ'
        NO-LOCK:
/*	IF NOT can-do( ENTRY(2, per, '|'), GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN NEXT.  */
/* kam */
	IF NOT GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "") matches (ENTRY(2, per, '|') + '*') THEN NEXT.
/* message per view-as alert-box. */
    /* kam - если возможно берем значения из классификатора strahpol */                                     
    mTmpStr = GetXAttrValueEx("loan", bloan.contract + "," + bloan.cont-code, "ПолСтрахПрем", ""). /* получатель страховой премии */
    find first bcode where bcode.class = 'strahpol' and bcode.code = mTmpStr no-lock no-error.
    
    FIND FIRST bcust-corp
    		WHERE bcust-corp.cust-id EQ bloan.cust-id
        	NO-LOCK NO-ERROR.
        CASE ENTRY(3, per, '|'):
    WHEN "doc_ref" THEN /* номер договора страхования */
    DO:
    	    rproc = TRIM(SUBSTRING(bloan.cont-code, 1, INDEX(bloan.cont-code,"@") - 1) + SUBSTRING(bloan.cont-code, INDEX(bloan.cont-code,"@") + 5)).

    END.
	WHEN "cust_name" THEN
	DO:
	    if avail bcode then do:
           rproc = bcode.description[1].
        end.
        else do:
	       rproc = (IF AVAILABLE bcust-corp THEN
                           TRIM(bcust-corp.name-short)
                        ELSE "").
        end.
	END.
	WHEN "open_date" THEN
	DO:
    	    rproc = string(bloan.open-date, "99.99.9999").
        END.
	WHEN "months" THEN
	DO:
    	    rproc = string( YEAR(bloan.end-date)  * 12 + MONTH(bloan.end-date) 
                          - YEAR(bloan.open-date) * 12 - MONTH(bloan.open-date)).
        END.
	WHEN "inn" THEN
	DO:
	    if avail bcode then do:
	       rproc = bcode.name.
	    end.
	    else do:
	       rproc = (IF AVAILABLE bcust-corp THEN
                           (TRIM(bcust-corp.inn))
                        ELSE "").
        end.
	END.
	WHEN "kpp" THEN
	DO:
	    rproc = (IF AVAILABLE bcust-corp THEN
		      GetXattrValueEx ("cust-corp", STRING(bcust-corp.cust-id), "КПП", "")
                    ELSE "").
	END.
	WHEN "bik" THEN
	DO:
	    if avail bcode then do:
           rproc = bcode.misc[3].
        end.
        else do:
	       rproc = (IF AVAILABLE bcust-corp THEN
                       (TRIM(bcust-corp.bank-code))
                    ELSE "").
        end.
	END.
	WHEN "bank" THEN
	DO:
	    if avail bcode then do:
           rproc = bcode.misc[1].
        end.
        else do:
            
    	    rproc = "".
    	    IF AVAIL bcust-corp THEN
    	    DO:
        	FIND FIRST banks-code
        	    WHERE banks-code.bank-code EQ bcust-corp.bank-code
            	    NO-LOCK NO-ERROR.
        	IF AVAILABLE(banks-code) THEN
                FIND FIRST banks
            	    WHERE banks.bank-id EQ banks-code.bank-id
                    NO-LOCK NO-ERROR.
                IF AVAILABLE(banks) THEN
                   rproc = TRIM(banks.NAME).
    	    END.
    	end.
	END.
	WHEN "corr" THEN
	DO:
	    if avail bcode then do:
           rproc = bcode.misc[2].
        end.
        else do:
	       rproc = (IF AVAILABLE bcust-corp THEN
                       (TRIM(bcust-corp.corr-acct))
                    ELSE "").
        end.
	END.
	WHEN "bacct" THEN
	DO:
	    if avail bcode then do:
           rproc = bcode.description[2].
        end.
        else do:
	       rproc = (IF AVAILABLE bcust-corp THEN
                       (TRIM(bcust-corp.benacct))
                    ELSE "").
        end.
	END.
	WHEN "tel" THEN
	DO:
	    if avail bcode then do:
           rproc = bcode.misc[4].
        end.
        else do:
	       rproc = (IF AVAILABLE bcust-corp THEN
                       (GetXAttrValueEx("cust-corp", STRING(bcust-corp.cust-id), "tel", ""))
                    ELSE "").
        end.
	END.
	WHEN "fax" THEN
	DO:
	    rproc = (IF AVAILABLE bcust-corp THEN
                       (TRIM(bcust-corp.fax))
                    ELSE "").
	END.
	WHEN "end_date" THEN	/*ayv конец договора*/
	DO:
   	    rproc = string(bloan.end-date, "99.99.9999").
	END.
	WHEN "rgsdate" THEN   /*ayv дата оплаты договора*/
	DO:
		rproc = string(bloan.open-date + 5, "99.99.9999").
	END.
	WHEN "gemon" THEN /*ayv больше ли 61 месяца*/
	DO:
		 IF ( YEAR(bloan.end-date)  * 12 + MONTH(bloan.end-date) - YEAR(bloan.open-date) * 12 - MONTH(bloan.open-date)) GE 61 THEN rproc = "Y". ELSE rproc = "N".
	END.
	WHEN "ЦенаКред" THEN /* д.р. цена автомобиля за счет кредита */
	DO:
	   rproc = "".
    	IF AVAILABLE(loan) THEN
        	RUN persent ( DECIMAL(GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "rko1_price", "")) , loan.currency, TRUE , INPUT-OUTPUT rproc).
	END.
	WHEN "Цена" THEN /* д.р. цена автомобиля прописью */
	DO:
	   rproc = "".
    	IF AVAILABLE(loan) THEN
        	RUN persent ( DECIMAL(GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "rko11_price", "")) , loan.currency, TRUE , INPUT-OUTPUT rproc).
	END.
	WHEN "ЦенаЦ" THEN /* д.р. цена автомобиля цифрами */
	DO:
	   rproc = "".
    	IF AVAILABLE(loan) THEN
        	vTmpDec = DECIMAL(GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "rko11_price", "")).
      rproc = TRIM(STRING(vTmpDec, "->>>,>>>,>>>,>>9.99")).
   END.
	
   WHEN "ПремияСтрах" THEN /* страховая премия */
	DO:
                  /*  message per + "  ddddss" view-as alert-box. */
	    rproc = "".
    	    FIND FIRST term-obl OF bloan WHERE
                    term-obl.idnt     EQ 1
                AND term-obl.end-date GE bloan.open-date
            NO-LOCK NO-ERROR.

    	    IF AVAILABLE term-obl THEN
    		vTmpDec = term-obl.amt-rub.
	    ELSE
    	    /* IF AVAILABLE(bloan) AND CAN-DO("КАСКО*",GetXAttrValueEx("loan",
                                     bloan.contract + "," + bloan.cont-code,
                                     "vidstr",
                                     "")) THEN */
        	vTmpDec = DECIMAL(GetXAttrValueEx("loan", loan.contract + "," + loan.cont-code, "kasko3", "")).
    	    IF NUM-ENTRIES(per,'|')<4 THEN DO:
        	/* RUN persent ( vTmpDec, '', TRUE , INPUT-OUTPUT rproc). */
        	rproc = TRIM(STRING( vTmpDec, "->>>,>>>,>>>,>>9.99")).
    	    END. ELSE
    		CASE ENTRY(4, per, '|'):
	        WHEN "Пр" THEN
	        DO:
	             RUN "x-amtstr.p" ( vTmpDec, '', YES , YES, OUTPUT c1, OUTPUT c2).
	             rproc = TRIM(STRING( vTmpDec, "->>>,>>>,>>>,>>9.99")) +
	                " (" + trim(c1) + " " + trim(c2) + ")".
	        END.
	        WHEN "Пр2" THEN
	        DO:
	             RUN "x-amtstr.p" ( vTmpDec, '', YES , YES, OUTPUT c1, OUTPUT c2).
	             rproc = trim(c1) + " " + trim(c2).
	        END.
	        END CASE.
	END.
	END CASE.

    END.
   END.

   /* sku Вывод в отчет тегов из справочника precrdprn */
   IF per MATCHES ("indicate|*") THEN
   DO:
		GetRefCrVal ("precrdprn",
                          "cont-code",
                          loan.open-date,
                          ?,
                         (TEMP-TABLE ttIndicate:HANDLE)).
		FOR EACH ttIndicate WHERE CAN-DO( REPLACE(REPLACE(ttIndicate.fChar,';',','),'%','*'), loan.cont-code):
			GetRefCrVal ("precrdprn",
                          "cont-type",
                          loan.open-date,
                          ttIndicate.fChar,
                         (TEMP-TABLE ttIndicate2:HANDLE)).
/*		run instview.p(TEMP-TABLE ttIndicate:HANDLE).				 
*/
			FOR EACH ttIndicate2 WHERE CAN-DO( REPLACE(REPLACE(ttIndicate2.fChar,';',','),'%','*'), loan.cont-type):
				GetRefCrVal ("precrdprn",
                          "branch-id",
                          loan.open-date,
                          ttIndicate.fChar + "," + ttIndicate2.fChar,
                         (TEMP-TABLE ttIndicate3:HANDLE)).
				FOR EACH ttIndicate3 WHERE CAN-DO( REPLACE(REPLACE(ttIndicate3.fChar,';',','),'%','*'), loan.branch-id):
					rproc = GetRefVal ("precrdprn",
							loan.open-date,
							ttIndicate.fChar + "," + ttIndicate2.fChar + "," + ttIndicate3.fChar + "," + ENTRY(2, per, '|')).
				END.
			END.
		END.	
/* 	rproc = GetRefVal("precrdprn", loan.open-date, 
		    loan.cont-code + "," + loan.cont-type + "," + loan.branch-id + "," + ENTRY(2, per, '|')). */
	IF rproc = "" THEN DO:
	GetRefCrVal ("precrdprn",
                          "cont-code",
                          loan.open-date,
                          ?,
                         (TEMP-TABLE ttIndicate:HANDLE)).
/*run instview.p(TEMP-TABLE ttIndicate:HANDLE).
*/
	FOR EACH ttIndicate
/*	    WHERE CAN-DO( REPLACE(REPLACE(ttIndicate.fChar,';',','),'%','*'), 
			    loan.cont-code + '|' + loan.cont-type + '|' + loan.branch-id + '|' + ENTRY(2, per, '|')
			    )*/:
		IF NUM-ENTRIES(ttIndicate.fChar,'|') = 4 THEN DO:
		    IF 
			CAN-DO(
			REPLACE(REPLACE(ENTRY(1,ttIndicate.fChar,"|"),';',','),'%','*'),
			loan.cont-code
			) = False OR
			CAN-DO(
			REPLACE(REPLACE(ENTRY(2,ttIndicate.fChar,"|"),';',','),'%','*'),
			loan.cont-type
			) = False OR
			CAN-DO(
			REPLACE(REPLACE(ENTRY(3,ttIndicate.fChar,"|"),';',','),'%','*'),
			loan.branch-id
			) = False OR
			CAN-DO(
			REPLACE(REPLACE(ENTRY(4,ttIndicate.fChar,"|"),';',','),'%','*'),
			ENTRY(2, per, '|')
			) = False THEN NEXT.
		    rproc = GetRefVal ("precrdprn",
				loan.open-date,
				ttIndicate.fChar + ',,,').
		    IF rproc = "" THEN
		    rproc = GetRefVal ("precrdprn",
				loan.open-date,
				ttIndicate.fChar).
		    LEAVE.
		END.
	END.
	END.
   END.

   /* Вывод в отчет доп.реквизитов условий договора */
   IF per MATCHES ("ДРУ|*") THEN
   DO:
       FIND LAST loan-cond WHERE loan-cond.cont-code EQ loan.cont-code
                              AND loan-cond.since <= XDate NO-LOCK NO-ERROR.
       RUN GetXattr (loan-cond.Class-Code, ENTRY(2, per, '|'), BUFFER xattr).
       IF AVAIL xattr THEN
       DO:
          IF xattr.progress-field THEN
             rproc = GetBufferValue("loan-cond",
                                    "WHERE loan-cond.contract  EQ '" + loan-cond.contract +
                                    "' AND loan-cond.cont-code EQ '" + loan-cond.cont-code +
                                    "' AND loan-cond.since     EQ "  + STRING (loan-cond.since),
                                    ENTRY(2, per, '|')) NO-ERROR.
          ELSE
          DO:
       mTmpStr = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE)).
       rproc = GetXAttrValueEx("loan-cond",
                               mTmpStr,
                               ENTRY(2, per, '|'),
                               "").
          END.
          IF {assigned rproc} THEN
          DO:
                /* Если ДР определен на классификаторе, то по коду этого
                   классификатора возвращаем его наименование */
             IF xattr.Domain-Code NE "" THEN
             DO:
                mTmpStr = GetCodeName(xattr.Domain-Code,rproc).
                IF {assigned mTmpStr} THEN
                  rproc = mTmpStr.
             END.
          END.
       END.
       IF rproc <> "" THEN
       DO:
          IF GetCodeName(ENTRY(2, per, '|'),rproc) <> ? THEN
             rproc = GetCodeName(ENTRY(2, per, '|'),rproc).
       END.
       /* sku */
       IF NUM-ENTRIES(per,'|') > 2 AND
          ENTRY(3,per,'|') MATCHES ("Пр*") THEN
       DO:
          CASE ENTRY(3,per,'|'):
          WHEN "Пр0" THEN DO:
            rproc = TRIM(STRING(DEC(rproc),">>>,>>>,>>9.99")).
	  END.
          WHEN "Пр" THEN
           RUN FrmtAmt(DEC(rproc),loan.currency,1, OUTPUT rproc).
          WHEN "Пр2" THEN
           RUN FrmtAmt(DEC(rproc),loan.currency,2, OUTPUT rproc).
          WHEN "Пр3" THEN DO:
            RUN x-amtstr.p (DEC(rproc), loan.currency, TRUE, TRUE, OUTPUT vAmtStr, OUTPUT vDecStr).
            rproc = vAmtStr + " " + vDecStr.
          END.
          END CASE.

       END. ELSE
       /* */
       IF NUM-ENTRIES(per,'|') > 2 THEN
       DO:
          ASSIGN
             vFormat    = ENTRY(3,per,'|')
             mFlFormat  = TRUE.
          rproc = STRING(rproc,vFormat).
       END.
   END.

   /* Вывод в отчет коэффициента резервирования */
   IF per MATCHES "Резерв" OR per MATCHES "Резерв|*" THEN
   DO:

      vTmpDec = LnRsrvRate(loan.contract, loan.cont-code, Xdate).

      IF NUM-ENTRIES(per,'|') > 1 THEN
      DO:
         vFormat = REPLACE(ENTRY(2,per,'|'),"_",",").
         rproc = STRING(DEC(vTmpDec),vFormat).
         vTmpDec = 0 .
      END.
      ELSE DO:
         rproc = STRING(vTmpDec).
      END.
   END.

   /* Выводит в отчет количество лет, месяцев и дней жизни договора */
   IF per MATCHES "Срок*|*" THEN
   DO:

      CASE ENTRY(1,per,'|'):
         WHEN "Срок" THEN
            ASSIGN
               vAvrgDayInMon = (01/01/3001 - 01/01/2001) / 1000 / 12
               vTmpDec  = MonInPer(loan.open-date,loan.end-date)
               vMesNum  = TRUNCATE(vTmpDec,0)       /* Количество месяцев */
               vNum     = TRUNCATE(DEC(vMesNum / 12) ,0) /* Количество лет */
               vDayNum  = ( vTmpDec - vMesNum ) * vAvrgDayInMon
               vMesNum  = vMesNum - ( vNum * 12 )
            .
         WHEN "СрокДн" THEN
            ASSIGN
               vDayNum  = loan.end-date - loan.open-date
               vMesNum  = 0
               vNum     = 0
            .
      END CASE.

      vProp = ENTRY(1, ENTRY(2,per,'|'), ";").

      IF vProp EQ "пр" THEN
      DO :  /* результат - прописью */
         RUN x-amtstr.p (vNum, "", NO, NO, OUTPUT vMes[4], OUTPUT vMes[1] ) .
         RUN x-amtstr.p (vMesNum, "", NO, NO, OUTPUT vMes[5], OUTPUT vMes[2] ) .
         RUN x-amtstr.p (vDayNum, "", NO, NO, OUTPUT vMes[6], OUTPUT vMes[3] ) .
      END.

      IF vProp EQ "чс" THEN
      DO:
         vMes[4] = STRING(vNum) + " ".
         vMes[5] = STRING(vMesNum) + " ".
         vMes[6] = STRING(vDayNum) + " ".
      END.

      IF vNum EQ 0 THEN
          vMes[1] ="".
      IF vNum EQ 1 THEN
          vMes[1] = vMes[4] + "Год ".
      IF CAN-DO("2,3,4",STRING(vNum)) THEN
          vMes[1] = vMes[4] + "Года ".
      IF NOT CAN-DO("0,1,2,3,4",string(vNum)) THEN
          vMes[1] = vMes[4] + "Лет ".

      IF vMesNum EQ 0 THEN
          vMes[2] ="".
      IF vMesNum EQ 1 THEN
          vMes[2] = vMes[5] + "Месяц ".
      IF CAN-DO("2,3,4",string(vMesNum)) THEN
          vMes[2] = vMes[5] + "Месяца ".
      IF NOT CAN-DO("0,1,2,3,4",string(vMesNum)) THEN
          vMes[2] = vMes[5] + "Месяцев ".

      IF vDayNum EQ 0 THEN
          vMes[3] ="".
      IF vDayNum EQ 1 THEN
          vMes[3] = vMes[6] + "День ".
      IF CAN-DO("2,3,4,",string(vDayNum)) THEN
          vMes[3] = vMes[6] + "Дня ".
      IF NOT CAN-DO("0,1,2,3,4",STRING(vDayNum)) THEN
          vMes[3] = vMes[6] + "Дней ".

      vMes[1] = STRING(vMes[1] + vMes[2] + vMes[3]).

      IF NUM-ENTRIES(ENTRY(1, per, ";"),'|') GT 2 THEN
      DO:
         vFormat = ENTRY(3,ENTRY(1, per, ";"),'|').
         vMes[1] = STRING(vMes[1],vFormat).
      END.

      rproc = vMes[1].
   END.

   /* Аналог функции "Сумма", но число + валюта прописью */
   IF per MATCHES ("СуммаЧс*") THEN
   DO:
      FIND FIRST bloan-cond {wh-t &f=bloan-cond &c="/*"}
                   AND bloan-cond.since GT loan-cond.since
      NO-LOCK NO-ERROR.

      vDateSumm = IF AVAILABLE bloan-cond
                     THEN bloan-cond.since
                     ELSE loan.end-date + 1.

      FIND FIRST term-obl {wh-t &f=term-obl &c="/*"}
             AND term-obl.idnt     EQ 2
             AND term-obl.end-date GE loan-cond.since
             AND term-obl.end-date LT vDateSumm
      NO-LOCK NO-ERROR.

      IF NOT AVAILABLE term-obl THEN
         FIND LAST term-obl {wh-t &f=term-obl &c="/*"}
               AND term-obl.idnt     EQ 2
               AND term-obl.end-date LE loan-cond.since
         NO-LOCK NO-ERROR.

      IF AVAILABLE term-obl THEN
      DO:
         FIND FIRST currency WHERE currency.currency EQ loan.currency NO-LOCK NO-ERROR.

         /* Определяем правильное отображение названия валюты в зависимости от
         ** количества ( 1 - доллар, 2 - доллара )*/
         mTmpStr = IF term-obl.amt EQ 1 THEN currency.curr-form1
                   ELSE IF  ( term-obl.amt < 1
                          AND term-obl.amt > 0 )
                          OR ( term-obl.amt > 1
                          AND term-obl.amt < 5 ) THEN currency.curr-form2
                   ELSE currency.curr-form5 .

         IF NUM-ENTRIES(per, "|") GT 1 AND ENTRY(2, per, "|") EQ "Пр" THEN
         DO:
            RUN amt.p (term-obl.amt, OUTPUT rproc).
            rproc = TRIM(STRING(term-obl.amt, "->>>,>>>,>>>,>>9.99") + " ( " + rproc + ") " + mTmpStr).
         END.
         ELSE
            rproc = TRIM(STRING(term-obl.amt, "->>>,>>>,>>>,>>9.99") + " " + mTmpStr).
      END.
   END.

   /* Возвращает наименование счета с указанной ролью ( если пусто, то наименование клиента ) */
   IF per MATCHES ("НаимСчета|*") THEN
   DO:
      /* Ищем номер счета по роли */
      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per,"|"),
                               Xdate,
                               BUFFER loan-acct).

      IF AVAILABLE loan-acct
      THEN DO:
         FIND FIRST acct OF loan-acct.

         {getcust.i
            &name    = vName
            &Offinn  = {comment}
         }
         vName[1] = TRIM (vName[1] + " " + vName[2]).
         rproc = vName[1].
         /* Форматирование */
         IF NUM-ENTRIES(per,'|') > 2 THEN
         DO:
            ASSIGN
               vMes[1]   = rproc
               mFlFormat = TRUE.
            {wordwrap.i
                      &s = vMes
                      &n = 50
                      &l = INT64(ENTRY(3,per,'|'))
                  }
            IF rproc <> "" AND rproc <> ? THEN
            DO:
                rproc = "".
                DO vNum = 1 TO 50:
                   IF vMes[vNum] = ""
                   THEN DO:
                      rproc = rproc + FILL(" ",INT64(ENTRY(3,per,'|')) - LENGTH(vMes[vNum - 1])).
                      LEAVE.
                   END.
                IF vNum NE 1
                THEN DO:
                   rproc = rproc + "\n".
                   IF NUM-ENTRIES(per,'|') > 4 THEN
                      rproc = rproc + FILL(" ",INT64(ENTRY(5,per,'|'))) .
                END.
                rproc = rproc + FILL(" ",INT64(ENTRY(4,per,'|'))) + vMes[vNum].
                END.
            END.
         END.
      END.
   END.

   /* Вывод в отчет реквизитов Выбранного кода классиф. */
   IF per MATCHES ("ИнфКлас|*|*") THEN
   DO:
      mTmpStr   = ENTRY(2,per,'|').
      vSrcChar  = ENTRY(3,per,'|').

      IF NUM-ENTRIES(per,'|') > 3 THEN
      DO:
         vNameChar = ENTRY(4,per,'|').
         vNameChar = REPLACE(vNameChar,"_"," ").
         RUN pclass.p (mTmpStr,mTmpStr,vNameChar,7).
      END.
      ELSE
      RUN pclass.p (mTmpStr,mTmpStr,"Выберите интересующий вас код классификаторара :",7).
      IF (LASTKEY EQ 10 OR LASTKEY EQ 13 ) AND pick-value NE ? THEN
      DO:
         FIND FIRST code WHERE code.class EQ mTmpStr
                           AND code.code  EQ pick-value NO-LOCK NO-ERROR.
         CASE vSrcChar:
            WHEN "Опис" THEN
                 rproc = STRING(code.description[1] + code.description[2] + code.description[3]).
            WHEN "Наим" THEN
                 rproc = STRING(code.name).
            WHEN "Знач" THEN
                 rproc = STRING(code.val).
            WHEN "Код" THEN
                 rproc = STRING(code.code).
         END CASE.
      END.

      IF NUM-ENTRIES(per,'|') > 4 THEN
      DO:
         ASSIGN
            mFlFormat  = TRUE
            vMes[1]    = rproc.
         {wordwrap.i
                   &s = vMes
                   &n = 50
                   &l = INT64(ENTRY(5,per,'|'))
               }
         IF rproc <> "" AND rproc <> ? THEN
         DO:
             DO vNum = 1 TO 50:
                IF vMes[vNum] = "" THEN
                    LEAVE.
             rproc = rproc + FILL(" ",INT64(ENTRY(6,per,'|'))) + vMes[vNum] + "\n".
             END.
         END.
      END.
   END.

   CASE per:
      WHEN "Сотр_ФИО" THEN
      DO:
         FIND FIRST _user WHERE _user._Userid EQ loan.USER-ID NO-LOCK NO-ERROR.
         IF     RetString
            AND AVAIL _user
         THEN rproc = STRING(_user._User-Name).
      END.
	  WHEN "Сотр_ФИ.О." THEN
	  DO:
		FIND FIRST _user WHERE _user._Userid EQ loan.USER-ID NO-LOCK NO-ERROR.
         IF     RetString
            AND AVAIL _user THEN
				IF NUM-ENTRIES(STRING(_user._User-Name)," ") < 3
				THEN
					rproc = "".
				ELSE
					IF NUM-ENTRIES(STRING(_user._User-Name),".") > 1 
					THEN 
						rproc = STRING(_user._User-Name).
					ELSE 
						rproc = ENTRY(1,STRING(_user._User-Name)," ") + " " +
						SUBSTRING(ENTRY(2,STRING(_user._User-Name)," "),1,1,"CHARACTER") + "." +
						SUBSTRING(ENTRY(3,STRING(_user._User-Name)," "),1,1,"CHARACTER") + ". ".
	  END.
	  WHEN "Сотр_Код" THEN
	  DO:
		rproc = loan.user-id.
	  END.
      WHEN "Сотр_Должн" THEN
      DO:
         FIND FIRST _user WHERE _user._Userid EQ loan.user-id NO-LOCK NO-ERROR.
         IF     RetString
            AND AVAIL _user
         THEN DO:
            rproc = STRING(GetXAttrValue("_user",loan.user-id,"Должность")).
         END.
      END.
   /*ayv печать ФИО сотрудника, запускающего функцию*/
   WHEN "Сотр_ФИО_НЕК" THEN
   DO:
      find first _user where _Userid=userid("bisquit") no-lock no-error.
      if avail _user then do:
        IF NUM-ENTRIES(_User._User-Name,".") EQ 1 THEN 
          rproc = ENTRY(1,_User._User-Name," ") + SUBSTRING(ENTRY(2,_User._User-Name," "),1,1)+ "." + SUBSTRING(ENTRY(3,_User._User-Name," "),1,1) + ".".
        ELSE 
          rproc = _User._User-Name.
      end.
   END.
   END CASE.

   IF per MATCHES ("ДогОткрЛС|*") THEN
   DO:
      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               ENTRY(2, per, "|"),
                               gend-date,
                               BUFFER loan-acct).
      IF AVAIL loan-acct THEN
      DO:
         rproc = GetXAttrValueEx ("acct",
                                  loan-acct.acct + "," + loan-acct.currency,
                                  "ДогОткрЛС",
                                  ",").
         vTmpDate = DATE(ENTRY(1, rproc)) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN
            rproc = 'Договора банковского счета № ' +
                    (IF NUM-ENTRIES(rproc) LE 2 THEN ENTRY(2, rproc) ELSE '____') +
                    ' от " ' + STRING(DAY(vTmpDate)) + ' " ' +
                    ENTRY(MONTH(vTmpDate), {&Months}) +
                    STRING(YEAR(vTmpDate), " 9999 г.").
      END.
   END.

   /* расчет ЭПС */
   IF per MATCHES "ЭПС" THEN
      rproc = STRING (GetEpsLoan(loan.contract,loan.cont-code,loan.since) * 100).
   IF per MATCHES "КодПродМ" THEN
   DO:
      FIND LAST loan-cond WHERE loan-cond.cont-code EQ loan.cont-code
                             AND loan-cond.since <= XDate NO-LOCK NO-ERROR.

      mTmpStr = GetSurrogateBuffer("loan-cond",(BUFFER loan-cond:HANDLE)).
      rproc   = STRING(INT64(GetXAttrValueEx("loan-cond",
                               mTmpStr,
                               "NMonthes",
                               "0"))          +
               INT64(GetXAttrValueEx("loan-cond",
                               mTmpStr,
                               "NYears",
                               "0")) * 12     +
               INT64(GetXAttrValueEx("loan-cond",
                               mTmpStr,
                               "NDays",
                               "0")) / 30, ">>>>>>>>>>>>>>>>9").

   END.

   /* Наименование базовой ставки */
   IF per MATCHES ("ПлБазНаим|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode}

      IF AVAIL comm-cond THEN
      DO:
         FIND FIRST commission WHERE
                    commission.commission EQ comm-cond.BaseCode
         NO-LOCK NO-ERROR.

         IF AVAIL commission THEN
            rproc = commission.name-comm[1].
      END.
   END.

   /* Действие над ПС */
   IF per MATCHES ("ПлДейст|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode}

      IF AVAIL comm-cond THEN
         rproc = STRING(comm-cond.action).
   END.

   /* Число, на которое изменяется ПС */
   IF per MATCHES ("ПлИзм|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode}
      IF AVAIL comm-cond THEN
      DO:
         IF comm-cond.BaseChange EQ 0 THEN
            vProcString = "процентов".
         ELSE IF    comm-cond.BaseChange EQ 0
                AND DEC(vDecStr) NE 0 THEN
                 vProcString = "процента".
         ELSE
            IF    comm-cond.BaseChange EQ 1
              AND DEC(vDecStr) EQ 0 THEN
               vProcString = "процент".
         ELSE
            IF    comm-cond.BaseChange < 4
              AND comm-cond.BaseChange > 1 THEN
               vProcString = "процента".
         ELSE
            IF comm-cond.BaseChange > 4 THEN
               vProcString = "процентов".

         RUN x-amtstr.p (comm-cond.BaseChange,"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).

         IF vDecStr EQ "00" THEN
            rproc = STRING(vAmtStr) + vProcString.
         ELSE
         DO:
            rproc = STRING(vAmtStr) + "целых ".
            RUN x-amtstr.p (DEC(vDecStr),"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
            rproc = rproc + LC(STRING(vAmtStr)) + "сотых " + vProcString.
         END.
      END.
/*
      IF AVAIL comm-cond THEN
         rproc = STRING(comm-cond.BaseChange).
*/
   END.

   /* Число, на которое изменяется ПС, прописью */
   IF per MATCHES ("ПлИзмРП|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode}

      IF AVAIL comm-cond THEN
      DO:
         IF comm-cond.BaseChange EQ 0 THEN
            vProcString = "процентов".
         ELSE IF    comm-cond.BaseChange EQ 0
                AND DEC(vDecStr) NE 0 THEN
                 vProcString = "процента".
         ELSE
            IF    comm-cond.BaseChange EQ 1
              AND DEC(vDecStr) EQ 0 THEN
               vProcString = "процент".
         ELSE
            IF    comm-cond.BaseChange < 4
              AND comm-cond.BaseChange > 1 THEN
               vProcString = "процента".
         ELSE
            IF comm-cond.BaseChange > 4 THEN
               vProcString = "процентов".

         RUN x-amtstr.p (comm-cond.BaseChange,"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).

         IF vDecStr EQ "00" THEN
            rproc = STRING(vAmtStr) + vProcString.
         ELSE
         DO:
            rproc = STRING(vAmtStr) + "целых ".
            RUN x-amtstr.p (DEC(vDecStr),"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
            rproc = rproc + LC(STRING(vAmtStr)) + "сотых " + vProcString.
         END.
      END.
   END.

   /* Сдвиг первой даты ПС */
   IF per MATCHES ("ПлСд1|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode }

      IF AVAIL comm-cond THEN
         rproc = STRING(comm-cond.FirstDelay).
   END.

   /* Сдвиг первой даты ПС прописью */
   IF per MATCHES ("ПлСд1ИП|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode }

      IF AVAIL comm-cond THEN
      DO:
         RUN x-amtstr.p (comm-cond.FirstDelay,"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
         rproc = vAmtStr.
      END.
   END.

   /* Рамерность периода ПС */
   IF per MATCHES ("ПлРазм|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode }

      IF AVAIL comm-cond THEN
         rproc = GetCodeNameEx ("ПлавСтПериод",comm-cond.period,"").
   END.

   /* Период ПС */
   IF per MATCHES ("ПлСрок|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode }

      IF AVAIL comm-cond THEN
         rproc = comm-cond.period.
   END.

   /* День в периоде смены ПС */
   IF per MATCHES ("ПлДень|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode }

      IF AVAIL comm-cond THEN
         rproc = STRING(comm-cond.day).
   END.

   /* Сдвиг ПС в рабочих днях */
   IF per MATCHES ("ПлСд2|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode }

      IF AVAIL comm-cond THEN
         rproc = STRING(comm-cond.delay).
   END.

   /* Общая величина Плавающей Процентной Ставки */
   IF per MATCHES ("ПлИтого|*") THEN
   DO:
      RUN CalcCommCondRate (loan.contract,loan.cont-code,ENTRY(2,per,"|"),xdate, OUTPUT vNextDate,OUTPUT vRateSumm).
      RUN CalcCommCondRate (loan.contract,loan.cont-code,ENTRY(2,per,"|"),vNextDate, OUTPUT vNextDate,OUTPUT vRateSumm).
      rproc = STRING(vRateSumm).
   END.

   /* Общая величина Плавающей Процентной Ставки прописью */
   IF per MATCHES ("ПлИтогоРП|*") THEN
   DO:
      RUN CalcCommCondRate (loan.contract,loan.cont-code,ENTRY(2,per,"|"),xdate, OUTPUT vNextDate,OUTPUT vRateSumm).
      RUN CalcCommCondRate (loan.contract,loan.cont-code,ENTRY(2,per,"|"),vNextDate, OUTPUT vNextDate,OUTPUT vRateSumm).

      IF vRateSumm EQ 0 THEN
            vProcString = "процентов".
      ELSE IF    vRateSumm EQ 0
             AND DEC(vDecStr) NE 0 THEN
              vProcString = "процента".
      ELSE
         IF    vRateSumm EQ 1
           AND DEC(vDecStr) EQ 0 THEN
            vProcString = "процент".
      ELSE
         IF    vRateSumm < 4
           AND vRateSumm > 1 THEN
            vProcString = "процента".
      ELSE
         IF vRateSumm > 4 THEN
            vProcString = "процентов".

      RUN x-amtstr.p (vRateSumm,"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).

      IF vDecStr EQ "00" THEN
         rproc = STRING(vAmtStr) + vProcString.
      ELSE
      DO:
         rproc = STRING(vAmtStr) + "целых ".
         RUN x-amtstr.p (DEC(vDecStr),"",NO,NO,OUTPUT vAmtStr,OUTPUT vDecStr).
         rproc = rproc + LC(STRING(vAmtStr)) + "сотых " + vProcString.
      END.
   END.

   /* Дата изменения ставки */
   IF per MATCHES ("ПлДатаИзм|*") THEN
   DO:
      RUN CalcCommCondRate (loan.contract,loan.cont-code,ENTRY(2,per,"|"),xdate, OUTPUT vNextDate,OUTPUT vRateSumm).
      IF vNextDate NE ? THEN
         rproc = STRING(vNextDate,"99.99.9999").
   END.

   /* Размер ПС на дату её фиксации (т.е. на дату следующей смены) */
   IF per MATCHES ("ПлБазСт|*") THEN
   DO:
      vRateCode = ENTRY(2, per, "|").
      {fcmcond.i
         &rate = vRateCode }

      IF AVAIL comm-cond THEN
      DO:
         IF comm-cond.source EQ "Общие ставки" THEN
            FIND LAST comm-rate WHERE comm-rate.commission EQ comm-cond.basecode
                                  AND comm-rate.acct       EQ "0"
                                  AND comm-rate.currency   EQ loan.currency
                                  AND comm-rate.kau        EQ ""
                                  AND comm-rate.min-value  EQ 0
                                  AND comm-rate.period     EQ 0
                                  AND comm-rate.since      LE XDate
            NO-LOCK NO-ERROR.
            IF AVAIL comm-rate THEN
               mCommRate = comm-rate.rate-comm.
         ELSE
            mCommRate = GET_COMM (comm-cond.basecode,                   /* Код комиссии */
                                  ?,                                    /* RecId счета*/
                                  loan.currency,                        /* Валюта*/
                                  loan.contract + "," + loan.cont-code, /* Кау*/
                                  0.00,                                 /* MIN остаток */
                                  0,                                    /* Период */
                                  Xdate).                               /* Дата */

         IF    mCommRate NE 0
           AND mCommRate NE ? THEN
            rproc = STRING(mCommRate).
      END.
   END.
      /* формирование таблицы по обеспечениям, если первым в пераметре указан table*
      ** то формируется набор значений в теге [table*] */
   IF per MATCHES ("ТаблицаПоОбеспечениям|*") THEN
   DO:
      IF NUM-ENTRIES(per, "|") GE 2 THEN
      DO:
         RUN tab-obesp.p (BUFFER loan,
                          SUBSTRING(per, INDEX(per, "|") + 1),
                          OUTPUT rproc).
      END.
   END.
      /* Информация по договору: (второй параметр составной - разделитель ;)
                                 1 параметр - код ДР на договоре
                                 2 параметр - код тега в ДР
                                 (необязательные)
                                 3 параметр - код классификатора, где хранится значение по умолчанию
                                 4 параметр - код параметра верхнего уровня, если в классификаторе организована иерархия
                                 (третий и четвертый параметры - форматирование)
      */
   IF per MATCHES ("ИнформацияПоДоговору|*;*") THEN
   DO:
      vDRCode = ENTRY(2, per, "|").
      CASE NUM-ENTRIES(vDRCode, ";"):
         WHEN 2 THEN
            RUN infoloan.p ((BUFFER loan:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            "",
                            "",
                            OUTPUT rproc
                            ).
         WHEN 3 THEN
            RUN infoloan.p ((BUFFER loan:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            ENTRY (3, vDRCode, ";"),
                            "",
                            OUTPUT rproc
                            ).
         WHEN 4 THEN
            RUN infoloan.p ((BUFFER loan:HANDLE),
                            ENTRY (1, vDRCode, ";"),
                            ENTRY (2, vDRCode, ";"),
                            ENTRY (3, vDRCode, ";"),
                            ENTRY (4, vDRCode, ";"),
                            OUTPUT rproc
                            ).
      END CASE.
      IF NUM-ENTRIES(per,'|') GT 2 THEN
      DO:
         ASSIGN
            mFlFormat  = TRUE
            vMes[1]    = rproc.
         {wordwrap.i
                   &s = vMes
                   &n = 50
                   &l = INT64(ENTRY(3,per,'|'))
                }
         IF rproc <> "" AND rproc <> ? THEN
         DO:
            DO vNum = 1 TO 50:
               IF vMes[vNum] = "" THEN
                  LEAVE.
            IF NUM-ENTRIES(per, '|') GT 3
               AND NOT (NUM-ENTRIES(per, '|') GT 4 AND INT64(ENTRY(5, per, '|')) GE vNum) THEN
               rproc = rproc + FILL(" ",INT64(ENTRY(4, per, '|'))).
            rproc = rproc + vMes[vNum]+ "\n".
            END.
         END.
      END.

   END.
      /* участие обеспечения в расчете резерва */
   IF per EQ ("УчОбеспеч") OR per MATCHES ("УчОбеспеч|*") THEN
   DO:
         /* для договора и для охватывающего договора */
      FOR EACH term-obl WHERE
          (    term-obl.contract  EQ loan.contract
           AND term-obl.cont-code EQ loan.cont-code
           AND term-obl.idnt      EQ 5
          )
       OR (    NUM-ENTRIES(loan.cont-code, " ") GT 1
           AND term-obl.contract  EQ loan.contract
           AND term-obl.cont-code EQ ENTRY (1, loan.cont-code)
           AND term-obl.idnt      EQ 5
          )
      NO-LOCK:
         vSurr = term-obl.contract         + "," +
                 term-obl.cont-code        + "," +
                 STRING(term-obl.idnt)     + "," +
                 STRING(term-obl.end-date) + "," +
                 STRING(term-obl.nn)
         .
         FIND LAST comm-rate WHERE
                   comm-rate.kau        EQ vSurr
               AND comm-rate.commission EQ "КачОбеспеч"
         NO-LOCK NO-ERROR.
         IF AVAIL comm-rate THEN
         DO:
            mTmpStr = GetXAttrValueEx("comm-rate",
                                      GetSurrogateBuffer("comm-rate",
                                                         (BUFFER comm-rate:HANDLE)
                                                         ),
                                      "КачОбеспеч",
                                      "").
            IF mTmpStr NE "" THEN
            DO:
               mTmpStr = GetXAttrValueEx ("term-obl",
                                          vSurr,
                                          "ВидДогОб",
                                          "").
               IF {assigned mTmpStr} THEN
                  mTmpStr = GetCodeName("ВидДогОб", mTmpStr).
               IF {assigned mTmpStr}
                  AND NOT CAN-DO (rproc, mTmpStr) THEN
               DO:
                  {additem.i rproc mTmpStr}
               END.
            END.
         END.
      END.
   END.

   /* sveta Сумма ЗАЯВКИ НА ДОСРОЧНОЕ ПОГАШЕНИЕ ССУДЫ */
   IF per MATCHES "ЗАЯВ*" THEN
    DO:
      FIND LAST bloan-cond WHERE bloan-cond.contract  EQ loan.contract
                             AND bloan-cond.cont-code EQ loan.cont-code
        NO-LOCK NO-ERROR.  
      FIND LAST term-obl WHERE term-obl.cont-code EQ loan.cont-code
        AND     term-obl.contract EQ 'Кредит'
        AND term-obl.idnt EQ 300 NO-LOCK NO-ERROR. 
      IF AVAIL term-obl THEN DO:
            mTmpStr = TRIM(STRING(term-obl.amt-rub, ">>>,>>>,>>>,>>9.99")).
      END.
      ELSE mTmpStr = "".

      IF      NUM-ENTRIES(per, "|") GT 1
         AND (   ENTRY(2, per, "|") EQ "Пр"
              OR ENTRY(2, per, "|") EQ "Пр2") THEN
      DO:
         RUN FrmtAmt(DEC(mTmpStr),loan.currency,(IF ENTRY(2, per, "|") EQ "Пр2" THEN 2 ELSE 1), OUTPUT rproc).
      END.
      ELSE
         rproc = TRIM(STRING(DEC(mTmpStr), ">>>,>>>,>>>,>>9.99")).

   END.

   /* cda вывод экранной формы с целью выбора договора поручительства */
   IF per BEGINS "guarantee|" THEN
   DO:    
      FOR EACH term-obl WHERE term-obl.contract   EQ loan.contract
                          AND term-obl.cont-code  EQ loan.cont-code
                          AND term-obl.class-code EQ "term-obl-gar" 
                          NO-LOCK:
                          
         vVidDogOb = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "ВидДогОб", "*").
         vVidOb    = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "ВидОб", "*").
         
         FIND FIRST code WHERE code.code EQ vVidOb NO-LOCK NO-ERROR.
         IF AVAIL code THEN
            vCodeVal = code.name.
         
         IF CAN-DO(vVidDogOb + "," + "Все", ENTRY(2, per, '|')) THEN 
         DO:
            CASE ENTRY(2, per, '|'):
               WHEN "Все" THEN
               DO:
                  IF CAN-DO(vVidOb + "," + "Все", ENTRY(3, per, '|')) THEN
                  DO:
                     CASE ENTRY(3, per, '|'): 
                        WHEN "Все" THEN
                        DO:
                           CASE ENTRY(4, per, '|'):
                              WHEN "Номер" THEN
                              DO:
                                 vFrameTitle = " [ ВЫБОР НОМЕРА ДОГОВОРА ] ".
                                 CREATE tt.
                                 ASSIGN
                                    tt.NomDog  = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "НомДогОб", "*")
                                    tt.NomPP   = term-obl.nn
                                    tt.CodeVal = CAPS(vCodeVal)
                                    tt.ChVal   = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "НомДогОб", "*")
                                    .
                              END.
                              WHEN "ДатаОткрытия" THEN
                              DO:
                                 vFrameTitle = " [ ВЫБОР ДАТЫ ОТКРЫТИЯ ] ".
                                 CREATE tt.
                                 ASSIGN
                                     tt.NomDog  = GetXattrValueEx("term-obl", STRING(term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(loan.end-date,"99/99/99") + "," + STRING(term-obl.nn)), "НомДогОб", "*")
                                     tt.NomPP   = term-obl.nn
                                     tt.CodeVal = CAPS(vCodeVal)
                                     tt.ChVal   = STRING(term-obl.fop-date,"99.99.9999")
                                 .
                              END.
                           END CASE.
                        END.
                     END CASE.
                  END.
               END.
            END CASE.
         END. 
      END.
      
      b_term:NUM-LOCKED-COLUMNS = 2.
      b_term:TITLE = vFrameTitle.
      
      OPEN QUERY q_term FOR EACH tt NO-LOCK BY tt.NomPP.

      PAUSE 0.
      VIEW b_term.

      ENABLE ALL WITH FRAME f_term.
      WAIT-FOR ENTER,ESC OF FRAME f_term.
      HIDE FRAME f_term.
      IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN DO:
        rproc = tt.ChVal.
      END. ELSE DO:
        RETURN "NO-APPLY".
      END.
   END.

      /* если код тега начинается с ИлиОхватДог,
      ** и на переданном договоре тэг не найден,
      ** переискиваем на охватывающем */
   IF mFindTech AND mNotFound AND NUM-ENTRIES (loan.cont-code, " ") GT 1 THEN
   DO:
      ASSIGN
         mContract = loan.contract
         mContCode = ENTRY (1, loan.cont-code, " ")
      .
      FIND FIRST loan WHERE
                 loan.contract  EQ mContract
             AND loan.cont-code EQ mContCode
      NO-LOCK NO-ERROR.
      IF AVAIL loan THEN
         rid-p = RECID(loan).
      FIND LAST loan-cond WHERE
                loan-cond.contract  EQ mContract
            AND loan-cond.cont-code EQ mContCode
            AND loan-cond.since     LE Xdate
      NO-LOCK NO-ERROR.
      IF AVAIL loan-cond THEN
         rid-t = RECID(loan-cond).
      RUN dog.p (OUTPUT per, Xdate1, Xdate, mStrPar).
   END.

   xresult = 0.
   PUT "".
   IF NOT Retstring THEN /*Для печати в Word не нужно*/
      PUT STREAM fil UNFORMATTED (IF logTrim
                                  THEN TrimFormula(chTrim,STRING(rproc))
                                  ELSE STRING(rproc)) .
END.  /* do transaction on endkey undo,leave on error undo,leave: */

HIDE FRAME q2.


{intrface.del}
RETURN (IF mFlFormat THEN "" ELSE rproc).
