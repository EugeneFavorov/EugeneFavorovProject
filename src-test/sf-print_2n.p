/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: SF-PRINT_2N.P
      Comment: Печать счетов-фактур
   Parameters: строка
         Uses:
      Used by:
      Created: 27.01.2005 Dasu
     Modified: 19/06/2006 ZIAL (0060144) АХД. Доработки по счетам-фактур. Патч d15.
     Modified: 22/06/2006 ZIAL (0060144) АХД. Доработки по счетам-фактур. Патч d15.
     Modified: 03/07/2006 ZIAL (0060144) АХД. Доработки по счетам-фактур. Патч d15.
     Modified: 06/07/2006 ZIAL (0060144) АХД. Доработки по счетам-фактур. Патч d15.
     Modified: 13/09/2013 kraw (0170344) Версия для ОСП
     Modified: 23/08/2017 kraw (0315149) только первое значение КПП
*/

/* В качестве параметра указывается список кодов из классификатора СостСчФакт.
** Процедура работает только по тем с/ф, у которых значения ДР СостСчФакт
** входят в этот список. Если в списке есть "?", то обрабатываются с/ф с
** незаполненным ДР */
DEFINE INPUT PARAMETER iSostLst AS CHARACTER NO-UNDO.

{tmprecid.def}

{globals.i}
{intrface.get date}
{intrface.get axd}
{intrface.get asset}
{intrface.get xclass}
{intrface.get strng }
{intrface.get cdrep}
{intrface.get db2l}
{intrface.get cust}
{intrface.get tmess}
&IF DEFINED(OPS) <> 0 &THEN
{intrface.get prnvd}
&ENDIF
{getcli.pro}
{branch.pro}    /* Интсрументы для работы с подразделениями */
{wordwrap.def}

/* СФ */
DEFINE VAR mSFNum         AS CHAR NO-UNDO. /* номер счёта-фактуры */
DEFINE VAR mSFDate        AS DATE NO-UNDO. /* дата сф */
DEFINE VAR mSFFixInfo     AS CHAR NO-UNDO. /* информация по исправлениям */
DEFINE VAR mSFSost        AS CHAR NO-UNDO. /* состояние с/ф (СостСчФакт) */
DEFINE VAR mSFFixDate     AS DATE NO-UNDO. /* дата исправления */
DEFINE VAR mSFFixNum      AS CHAR NO-UNDO. /* номер исправления */
DEFINE VAR mSFTovar       AS LOG  NO-UNDO. /* ДР Товар */
DEFINE VAR mSFSeller      AS CHAR NO-UNDO. /* продавец */
DEFINE VAR mSFSellerAddr  AS CHAR NO-UNDO. /* продавец Адрес сф */
DEFINE VAR mSFSellerINN   AS CHAR NO-UNDO. /* продавец INN сф */
DEFINE VAR mSFSellerKPP   AS CHAR NO-UNDO. /* продавец KPP сф */
DEFINE VAR mSFCurrInfo    AS CHAR NO-UNDO. /* код и наименование валюты сф */
DEFINE VAR mSFCurrLine    AS CHAR NO-UNDO. /* прочерк под реквизитами сф */
DEFINE VAR mSFCurrName    AS CHAR NO-UNDO. /* наименование сф */
DEFINE VAR mIgk           AS CHAR NO-UNDO. /* ДР ИГК */

DEFINE VAR mSFBuyer       AS CHAR NO-UNDO. /* покупатель */
DEFINE VAR mSFBuyerAddr   AS CHAR NO-UNDO. /* покупатель Адрес сф */
DEFINE VAR mSFBuyerINN    AS CHAR NO-UNDO. /* покупатель INN сф */
DEFINE VAR mSFBuyerKPP    AS CHAR NO-UNDO. /* покупатель KPP сф */
                         
DEFINE VAR mSFOtprav      AS CHAR NO-UNDO. /* Грузоотправ сф */
DEFINE VAR mSFOtpravAddr  AS CHAR NO-UNDO. /* Адрес Грузоотправ сф */
DEFINE VAR mSFPoluch      AS CHAR NO-UNDO. /* Грузополучатель сф */
DEFINE VAR mSFPoluchAddr  AS CHAR NO-UNDO. /* Адрес Грузополучателя сф */
                         
DEFINE VAR mOpNum         AS CHAR NO-UNDO. /* Номер платёжного документа */
DEFINE VAR mOpDate        AS DATE NO-UNDO. /* Дата платёжного документа */
DEFINE VAR mDocNumDate    AS CHAR NO-UNDO. /* список номеров платёжных документов и дат выдачи для проводок типа "sf-of-pay" */
DEFINE VAR mDocNumLine    AS CHAR NO-UNDO. /* прочерк под списком номеров платёжных документов */
DEFINE VAR mSurrOp        AS CHAR NO-UNDO. /* Список суррогатов платежей, связанных со счётом-фактурой */
DEFINE VAR mTotalSumm     AS DEC  NO-UNDO.
DEFINE VAR mNalogSumm     AS DEC  NO-UNDO.
DEFINE VAR mPriceSumm     AS DEC  NO-UNDO.
DEFINE VAR mNameSrv       AS CHAR NO-UNDO. /* наименование услуги */
DEFINE VAR mBranchId      AS CHAR NO-UNDO.
DEFINE VAR mStrSeller     AS CHAR NO-UNDO EXTENT 10. /* продавец */
DEFINE VAR mStrSellerAddr AS CHAR NO-UNDO EXTENT 10. /* продавец Адрес сф */
DEFINE VAR mStrBuyer      AS CHAR NO-UNDO EXTENT 10. /* покупатель */
DEFINE VAR mStrBuyerAddr  AS CHAR NO-UNDO EXTENT 10. /* покупатель Адрес сф */
DEFINE VAR mStrOtprav     AS CHAR NO-UNDO EXTENT 10. /* Грузоотправ сф + Адрес */
DEFINE VAR mStrPoluch     AS CHAR NO-UNDO EXTENT 10. /* Грузополучатель сф + Адрес */
DEFINE VAR mWide          AS INT64  NO-UNDO.
DEFINE VAR mNameGO        AS CHARACTER   NO-UNDO.
DEFINE VAR mAdresGO       AS CHARACTER   NO-UNDO.
DEFINE VAR mMonthes       AS CHARACTER   NO-UNDO INIT "января,февраля,марта,~
апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря".

DEFINE VAR mDispReq AS CHAR NO-UNDO.

DEFINE STREAM sfact.

DEF VAR mI AS INT64 NO-UNDO. /* счётчик */
DEF VAR mPage AS INT64 NO-UNDO.
DEF VAR mLeng AS INT64 NO-UNDO INIT 187.
DEF VAR mLengBody AS INT64 NO-UNDO INIT 3.
DEF VAR mLs AS INT64 NO-UNDO INIT 0.

DEF VAR mMaxRow AS INT64  NO-UNDO.

DEFINE VARIABLE mLoanSurr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTermOblSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVATformat   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSchFPolnNam AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mStrNlog     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFractional  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mX           AS DECIMAL   NO-UNDO.
/* признак отключения авторасчета по НП ОтклАвтРасч */
DEFINE VARIABLE mNoAutoCalc  AS LOGICAL   NO-UNDO.

&GLOB  STR1 'К платёжно-расчётному документу'
&IF DEFINED(OPS) =  0 &THEN
{setdest.i &stream="stream sfact" &filename="'_spool_sf.tmp'"}
&ENDIF

/* таблица для услуг */
DEFINE TEMP-TABLE ttServ NO-UNDO
   FIELD NameServ    AS CHAR     /* Наименование услуги */
   FIELD KodTov      AS CHAR     /* Код товара ЕЭС */
   FIELD Edin        AS CHAR     /* Единица измерения */
   FIELD EdinName    AS CHAR     /* Условное обозначение единицы измерения */
   FIELD Quant       AS DECIMAL  /* количество */
   FIELD Price       AS DECIMAL  /* цена за единицу */
   FIELD SummOut     AS DECIMAL  /* сумма без налога */
   FIELD Akciz       AS DECIMAL  /* сумма акциза */
   FIELD Rate        AS DECIMAL  /* ставка налога из term-obl */
   FIELD Nlog        AS DECIMAL  /* ставка налога */
   FIELD NalogSumm   AS DECIMAL  /* сумма налога */
   FIELD TotalSumm   AS DECIMAL  /* сумма с налогом */
   FIELD Contry      AS CHAR     /* страна происхождения */
   FIELD ContryName  AS CHAR     /* наименование страны происхождения */
   FIELD GTDNum      AS CHAR     /* Номер ГТД */
   FIELD Curr        AS CHAR     /* Валюта */
   FIELD Rate-fixed  AS CHAR     /* Фиксированный тариф */
.

DEFINE BUFFER b-loan FOR loan. /* Локализация буффера */

mPage = 0.

ASSIGN
   mVATformat = (IF FGetSetting("СчФОтрНДС", "", "Нет") =  "Да"
                 THEN ">>>>>>>9%"
                 ELSE ">>>>>9.99")   
   mSchFPolnNam = FGetSetting("СчФПолнНаим", "", "Нет") =  "Да"
   mDispReq     = FGetSetting("СчФОтрСведДекл", "", "---")
.
IF mDispReq =  ? THEN mDispReq = '---'.

&IF DEFINED(OPS) <> 0 &THEN
RUN BeginCircle_TTName IN h_prnvd ("sf").
&ENDIF

FOR EACH TMPRECID NO-LOCK:
   FIND FIRST loan WHERE 
         RECID(loan) =  tmprecid.id /* нашли СФ */
      NO-LOCK NO-ERROR.

   ASSIGN
      mStrSeller = ""
      mLoanSurr  = GetSurrogateBuffer("loan", (BUFFER loan:HANDLE))
      mSFSost    = GetXAttrValueEx("loan", mLoanSurr, "СостСчФакт", "?")
      mBranchId  = TRIM(GetXAttrValueEx("_user",loan.user-id,"Отделение", ""))
   .
   IF iSostLst <> "" AND
      NOT CAN-DO(iSostLst, mSFSost) THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "0",
         SUBSTITUTE("C/ф &1 пропущен.~nДля таких счетов-фактур существует ~
другая процедура печати.",
                    loan.doc-num)).
      NEXT.
   END.

   /* определяем переменные для СФ */
   RUN SFAttribsNames IN h_axd (INPUT loan.contract,
                      INPUT loan.cont-code,

                      OUTPUT mSFSeller,
                      OUTPUT mSFSellerAddr,
                      OUTPUT mSFSellerINN,
                      OUTPUT mSFSellerKPP,

                      OUTPUT mSFBuyer,    
                      OUTPUT mSFBuyerAddr,
                      OUTPUT mSFBuyerINN,
                      OUTPUT mSFBuyerKPP,
                      
                      OUTPUT mSFOtprav,    
                      OUTPUT mSFOtpravAddr,
                      OUTPUT mSFPoluch,
                      OUTPUT mSFPoluchAddr).

   RUN SFAttribs_Nums IN h_axd (INPUT loan.contract,
                       INPUT loan.cont-code,

                       OUTPUT mSFNum,  
                       OUTPUT mSFDate,
                       OUTPUT mOpNum, 
                       OUTPUT mOpDate
                      ).
   
                         
                      
   ASSIGN
      mDocNumDate = ""
      mSFNum = "" WHEN mSFNum =  ?
      mOpNum = "" WHEN mOpNum =  ?
      mSFSellerKPP = ENTRY(1, mSFSellerKPP)
      mSFBuyerKPP  = ENTRY(1, mSFBuyerKPP)
   .

   IF mSFSost =  "?" THEN
      mSFFixInfo = "".
   ELSE
      mSFFixInfo = GetXattrValue("loan", mLoanSurr, "НомДатКорр").

   mSFTovar = GetXattrValue("loan", mLoanSurr, "Товар") =  "Да".

   IF {assigned loan.currency} THEN
   DO:
      mSFCurrName = GetBufferValue("currency",
                                   "WHERE currency.currency EQ " + 
                                   QUOTER(loan.currency),
                                   "name-currenc").
      ASSIGN
         mSFCurrInfo = " наименование, код " + mSFCurrName + ", " + loan.currency
         mSFCurrLine = FILL(" ", 26) + FILL("─",LENGTH(mSFCurrName)) + "  ───"
         .
   END.
   ELSE
      ASSIGN    
         mSFCurrInfo = " наименование, код Российский рубль, 643"
         mSFCurrLine = FILL(" ", 26) + "────────────────  ───"
         .
   mIgk = GetXAttrValueEx("loan",
                          loan.contract + "," + loan.cont-code,
                          "ИГК",
                          "-").
   IF loan.cont-type =  "а/о" THEN   
   DO:
      mSurrOp = GetLinks(loan.class-code, /* ID класса     */
                mLoanSurr,                /* ID(cуррогат) объекта   */
                ?,                        /* Направление связи: s | t | ?         */
                "sf-op-pay",              /* Список кодов линков в CAN-DO формате */
                ";",                      /* Разделитель результирующего списка   */
                ?).
      DO mI=1 TO NUM-ENTRIES(mSurrOp,";"):
         FOR EACH op WHERE op.op = INT64(ENTRY(1, ENTRY(mI, mSurrOp, ";"), ","))
         NO-LOCK:
            mDocNumDate = mDocNumDate + (IF mI = 1
                                         THEN ''
                                         ELSE FILL(' ',mLengBody + mLs + 
LENGTH({&STR1}))) + ' ' + "№" + STRING(OP.doc-num,"x(10)") + "от" + " " + 
STRING(OP.op-date,"99/99/9999").  
               
               END.
         mDocNumLine = "                               ────────────    ─────────────────────────────────".       
      END.
   END.
   IF loan.loan-work AND {assigned mOpNum} THEN
      ASSIGN
         mDocNumDate = SUBSTITUTE("№&1 от &2",
                                  STRING(mOpNum,"x(10)"),
                                  STRING(mOpDate,"99/99/9999"))
         mDocNumLine = 
                 "                               ────────────    ─────────────────────────────────"
      .
   IF mDocNumDate =  "" THEN
   DO:
      ASSIGN
         mDocNumDate = "-"
         mDocNumLine = "                               ─────────────────────────────────────────────────"
         .
   END.   

   
   mNoAutoCalc = CAN-DO(FGetSetting("ОтклАвтРасч", ?, ""), loan.contract).
   /* смотрим и записываем услуги */
   FOR EACH term-obl WHERE 
            term-obl.contract  = loan.contract
        AND term-obl.cont-code = loan.cont-code
   NO-LOCK:
      mTermOblSurr = GetSurrogateBuffer("term-obl", (BUFFER term-obl:HANDLE)).
      mFractional  = (IF GetXattrValueEx(term-obl.class-code, mTermOblSurr, "НДСДр", "") =  "Да"
                      THEN YES
                      ELSE NO).
      
      CREATE ttServ.

      FIND FIRST currency WHERE currency.currency =  loan.currency NO-LOCK NO-ERROR.
      
      ASSIGN 
         mNameSrv        = SFAssetName(mTermOblSurr, term-obl.symbol)
&IF DEFINED(OPS) =  0 &THEN
         ttServ.NameServ = SplitStr(mNameSrv, 20, '~n')
&ELSE
         ttServ.NameServ = mNameSrv
&ENDIF
         ttServ.EdinName = SFAssetUnit(mTermOblSurr, term-obl.symbol)
         ttServ.Edin     = GetCodeEx("Unit", ttServ.EdinName, "")
&IF DEFINED(OPS) =  0 &THEN
         ttServ.EdinName = SplitStr(ttServ.EdinName, 14, '~n')
&ENDIF
         .

      ASSIGN
         ttServ.KodTov = GetXattrValueEx("term-obl",
                                         mTermOblSurr,
                                         "КодТовараЕЭС",
                                         "")
         ttServ.rate-fixed = GetXattrValueEx("term-obl", mTermOblSurr, "rate-fixed", "")
         ttServ.Curr      = IF loan.currency =  "" THEN "Российский рубль"
                                                   ELSE (IF AVAIL currency
                                                         THEN currency.name-currenc
                                                         ELSE "")
         ttServ.Quant      = term-obl.ratio       /* кол-во */
         ttServ.Price      = IF ttServ.rate-fixed =  '%'
                             THEN DEC(GetXattrValueEx("term-obl", 
                                                 mTermOblSurr,
                                                 "Price-rate",
                                                 STRING(term-obl.price)))
                             ELSE term-obl.price  /* цена */
         ttServ.Akciz      = term-obl.dsc-int-amt /* Акциз */
         ttServ.Rate       = term-obl.rate        /* Ставка НДС из term-obl */
         ttServ.Nlog       = (IF mFractional
                              THEN (term-obl.rate / (term-obl.rate + 100))
                              ELSE  term-obl.rate)
                                                  /* Ставка НДС */
         ttServ.NalogSumm  = term-obl.int-amt     /* Сумма НДС */
         ttServ.TotalSumm  = term-obl.amt-rub     /* Общая сумма */
         ttServ.SummOut    = (IF mNoAutoCalc       /* сумма без налога */
                                THEN term-obl.amount-of-payment
                                ELSE (term-obl.amt-rub - term-obl.int-amt))
         ttServ.GTDNum     = GetXattrValueEx("term-obl", mTermOblSurr, "declare", "")            /* Номер ГТД */
         ttServ.contry     = SFAssetCountryAttr(mTermOblSurr, term-obl.symbol, "country-alt-id") /* Страна */
         ttServ.ContryName = SFAssetCountryAttr(mTermOblSurr, term-obl.symbol, "country-name")   /* Страна */  
&IF DEFINED(OPS) =  0 &THEN
         ttServ.ContryName = SplitStr(ttServ.ContryName, 12, '~n')
&ENDIF
         .
      RELEASE ttServ.
   END.

&IF DEFINED(OPS) =  0 &THEN
   /* делаем перевод страницы перед печатью счёт-фактуры, кроме первой */
   
   IF mPage = 0 THEN mPage = 1.
   ELSE PAGE STREAM sfact.
&ENDIF
    
   ASSIGN 
      mStrSeller     = mSFSeller
      mStrSellerAddr = mSFSellerAddr
      mStrBuyer      = mSFBuyer
      mStrBuyerAddr  = mSFBuyerAddr 
   .

   IF loan.cust-cat =  "Ю" THEN
   DO:
      FIND FIRST cust-corp WHERE cust-corp.cust-id =  loan.cust-id NO-LOCK NO-ERROR.
      IF AVAIL cust-corp THEN
           ASSIGN
/* Вставка Плюс банк */
            mStrBuyer = GetTempXattrValueEx("cust-corp",
                                                STRING(cust-corp.cust-id),
                                                "name-short",
                                                loan.open-date,"")
/* Конец вставки Плюс банк */
            mNameGO  = GetXAttrValueEx("cust-corp",
                                       STRING(cust-corp.cust-id),
                                       "NameGO",
                                       "")
            mAdresGO = GetXAttrValueEx("cust-corp",
                                       STRING(cust-corp.cust-id),
                                       "AdresGO",
                                       "")
         .
   END.

   IF     {assigned mNameGO}
      AND {assigned mAdresGO} THEN
   DO:
      IF loan.contract =  "sf-in" THEN
         ASSIGN
            mStrSeller     = mNameGO
            mStrSellerAddr = mAdresGO
         .
      ELSE
         ASSIGN
            mStrBuyer     = mNameGO
            mStrBuyerAddr = mAdresGO
         .
   END.
   
   IF    loan.cont-type =  "а/о"
      OR loan.cont-type =  "а/п"
      OR NOT mSFTovar THEN
      ASSIGN
         mStrOtprav     = "-"
         mStrPoluch     = "-"
      .
   ELSE
   DO:
      IF INDEX(mStrOtprav[1],mSFOtprav) =  0 THEN
         mStrOtprav = mSFOtprav + " " + mSFOtpravAddr.
      IF INDEX(mStrPoluch[1],mSFPoluch) =  0 THEN
         mStrPoluch = mSFPoluch + " " + mSFPoluchAddr.     
   END.

&IF DEFINED(OPS) =  0 &THEN
   {sf-print_2n.i}
&ELSE
   {sf-print_2n_ops.i}
&ENDIF

   /* печатаем */
   {empty ttServ}

   &IF DEFINED(OPS) <> 0 &THEN
   RUN NextCircle_TTName IN h_prnvd ("sf").
   &ENDIF

END.

&IF DEFINED(OPS) <> 0 &THEN
RUN EndCircle_TTName IN h_prnvd ("sf").
&ENDIF

&IF DEFINED(OPS) =  0 &THEN
{preview.i &STREAM="STREAM sfact" &FILENAME = "'_spool_sf.tmp'"}
&ELSE
RUN prnvd IN h_prnvd ("sfprint2n").
&ENDIF

{intrface.del}
/* $LINTFILE='sf-print_2n.p' */
/* $LINTMODE='1,3,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq/' */
/* $LINTUSER='ozmi' */
/* $LINTDATE='19/09/2017 11:37:37.641+03:00' */
/*prosign7uS0o0ERIg6EQWjRK3hnfA*/