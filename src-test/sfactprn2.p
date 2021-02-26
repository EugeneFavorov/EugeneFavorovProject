/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: SFACTPRN2.P
      Comment: Акт на выполнение работ/услуг по счету-фактуре
   Parameters: none
         Uses:
      Used by:
      Created: 06.05.2006 ZIAL
     Modified: 18/05/2006 ZIAL (0059334) АХД. Создание Акта на выполнение работ/услуг 
               по счету-фактуре
     Modified: 24/07/2006 ZIAL (0059334) АХД. Создание Акта на выполнение работ/услуг 
               по счету-фактуре
     Modified: 20.03.2007 17:38 OZMI     (0070614)
     Modified: 04/02/2015 kraw (0247185) Подъем в спецверсию
*/

{tmprecid.def}

{globals.i}
{intrface.get date}
{intrface.get axd}
{intrface.get asset}
{intrface.get xclass}
{intrface.get strng }
{intrface.get tmess}
{getcli.pro}
{wordwrap.def}
/*СФ*/
DEFINE VAR mSFNum        AS CHAR NO-UNDO. /*номер счёта-фактуры*/
DEFINE VAR mSFDate       AS DATE NO-UNDO. /*дата сф*/
DEFINE VAR mSFSeller     AS CHAR NO-UNDO. /*продавец*/
DEFINE VAR mSFSellerFIO  AS CHAR NO-UNDO. /*представитель продавца*/
DEFINE VAR mSFSellerAddr AS CHAR NO-UNDO. /*продавец Адрес сф*/
DEFINE VAR mSFSellerINN  AS CHAR NO-UNDO. /*продавец INN сф*/
DEFINE VAR mSFSellerKPP  AS CHAR NO-UNDO. /*продавец KPP сф*/


DEFINE VAR mSFBuyer      AS CHAR NO-UNDO. /*покупатель*/
DEFINE VAR mSFBuyerFIO   AS CHAR NO-UNDO. /*представитель покупателя*/
DEFINE VAR mSFBuyerAddr  AS CHAR NO-UNDO. /*покупатель Адрес сф*/
DEFINE VAR mSFBuyerINN   AS CHAR NO-UNDO. /*покупатель INN сф*/
DEFINE VAR mSFBuyerKPP   AS CHAR NO-UNDO. /*покупатель KPP сф*/


DEFINE VAR mSFOtprav     AS CHAR NO-UNDO. /*Грузоотправ сф*/
DEFINE VAR mSFOtpravAddr AS CHAR NO-UNDO. /*Адрес Грузоотправ сф*/
DEFINE VAR mSFPoluch     AS CHAR NO-UNDO. /*Грузополучатель сф*/
DEFINE VAR mSFPoluchAddr AS CHAR NO-UNDO. /*Адрес Грузополучателя сф*/

DEFINE VAR mOpNum        AS CHAR NO-UNDO. /*Номер платёжного документа*/
DEFINE VAR mOpDate       AS DATE NO-UNDO. /*Дата платёжного документа*/





DEFINE VAR mAxdNum       AS CHAR NO-UNDO.
DEFINE VAR mAxdDate      AS DATE NO-UNDO.



DEFINE VAR mTotalSumm    AS DECIMAL NO-UNDO.
DEFINE VAR mNalogSumm    AS DECIMAL NO-UNDO.
/*DEFINE VAR mIsOut        AS LOG     NO-UNDO. /*выставленная или нет СФ*/*/
DEFINE VAR mNameSrv      AS CHAR NO-UNDO.    /* наименование услуги */


DEFINE VAR mSummOut      AS DECIMAL NO-UNDO.
DEFINE VAR mSummOutRub   AS CHAR    NO-UNDO.
DEFINE VAR mSummOutCop   AS CHAR    NO-UNDO.
DEFINE VAR mSummNalogSumm   AS DECIMAL    NO-UNDO.


DEFINE VAR mSFSellerTel  AS CHAR    NO-UNDO.




DEFINE VAR mBranchId     AS CHAR NO-UNDO.

DEFINE VAR mN            AS INT64 FORMAT ">>9" NO-UNDO.




DEF VAR mMonthes AS CHAR NO-UNDO 
        INIT "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря".

DEFINE STREAM sfact.

DEF VAR mI AS INT64 NO-UNDO. /* счетчик */

DEF VAR mUserCode        AS CHAR NO-UNDO.    /*Исполнитель*/
DEF VAR mShFBankRek      AS CHAR NO-UNDO.

DEF VAR mClFileName      AS CHAR NO-UNDO.
DEF VAR mContrAgent      AS CHAR NO-UNDO.
DEF VAR mIspolnit        AS CHAR NO-UNDO.
DEF VAR mNameStr         AS CHAR NO-UNDO EXTENT 10.


{setdest.i &cols=120 &STREAM="stream sfact" }

/*таблица для услуг*/
DEFINE TEMP-TABLE ttServ NO-UNDO
   FIELD NameServ    AS CHAR     /*Наименование услуги*/
   FIELD Edin        AS CHAR     /*Единица измерения*/
   FIELD Quant       AS DECIMAL  /*количество*/
   FIELD Price       AS DECIMAL  /*цена за единицу*/
   FIELD SummOut     AS DECIMAL  /*сумма без налога*/
/*   FIELD Akciz       AS DECIMAL  /*сумма акциза*/*/
   FIELD Nlog        AS DECIMAL  /*ставка налога*/
   FIELD NalogSumm   AS DECIMAL  /*сумма налога*/
   FIELD TotalSumm   AS DECIMAL  /*сумма с налогом*/
/*   FIELD Contry      AS CHAR     /*страна происхождения*/*/
/*   FIELD GTDNum      AS CHAR     /*Номер ГТД*/*/
.


FOR EACH TMPRECID NO-LOCK:

   FIND FIRST loan WHERE 
         RECID(loan) EQ tmprecid.id /*нашли СФ*/
      NO-LOCK NO-ERROR.



   /*определяем переменные для СФ*/
   RUN SFAttribsNames IN THIS-PROCEDURE(INPUT loan.contract,
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

   RUN SFAttribs_Nums IN THIS-PROCEDURE(INPUT loan.contract,
                                        INPUT loan.cont-code,
                                       
                                        OUTPUT mSFNum,  
                                        OUTPUT mSFDate,
                                        OUTPUT mOpNum, 
                                        OUTPUT mOpDate
                                       ).

   RUN AxdAttribs_Nums IN THIS-PROCEDURE(
                                         INPUT loan.parent-contract,
                                         INPUT loan.parent-cont-code,
                                       
                                         OUTPUT mAxdNum,  
                                         OUTPUT mAxdDate
                                        ).
                                       
   IF mSFNum EQ ? THEN mSFNum = "".
   IF mOpNum EQ ? THEN mOpNum = "".
   IF mAxdNum EQ ? THEN mAxdNum = "".


   /*смотрим и записываем услуги*/
   FOR EACH term-obl WHERE 
            term-obl.contract  = loan.contract
        AND term-obl.cont-code = loan.cont-code
   NO-LOCK:
      
      CREATE ttServ.

      ASSIGN 
         mNameSrv = SFAssetName(term-obl.contract + "," +
                                term-obl.cont-code + "," + 
                                STRING(term-obl.idnt) + "," + 
                                STRING(term-obl.end-date) + "," +
                                STRING(term-obl.nn),
                                term-obl.symbol)
         ttServ.NameServ = SplitStr(mNameSrv,30,'~n')
         ttServ.Edin = SFAssetUnit(term-obl.contract         + "," 
                                 + term-obl.cont-code        + ","
                                 + STRING(term-obl.idnt)     + ","
                                 + STRING(term-obl.end-date) + ","
                                 + STRING(term-obl.nn), 
                                   term-obl.symbol)
         .
                          
      ASSIGN         
         ttServ.Quant  = term-obl.ratio      /* кол-во */
         ttServ.Price = term-obl.price       /* цена */
/*         ttServ.Akciz = term-obl.dsc-int-amt /* Акциз */*/
         ttServ.Nlog  = term-obl.rate        /* Ставка НДС */
         ttServ.NalogSumm = term-obl.int-amt /* Сумма НДС */
         ttServ.TotalSumm = term-obl.amt-rub /* Общая сумма */
         ttServ.SummOut = term-obl.amt-rub - term-obl.int-amt /*сумма без налога*/
/*         ttServ.GTDNum = GetXattrValueEx("term-obl",
                          term-obl.contract + "," + term-obl.cont-code
                          + "," + STRING(term-obl.idnt) + "," 
                          + STRING(term-obl.end-date) + "," 
                          + STRING(term-obl.nn),
                          "declare",
                          "")                             /* Номер ГТД */*/
/*        ttServ.contry = SFAssetCountry(term-obl.contract         + "," 
                                       + term-obl.cont-code        + ","
                                       + STRING(term-obl.idnt)     + ","
                                       + STRING(term-obl.end-date) + ","
                                       + STRING(term-obl.nn), 
                                         term-obl.symbol) /* Страна */*/
      . 
      RELEASE ttServ.
   END.

   IF loan.cust-cat EQ "Ю" THEN
      mClFileName = "cust-corp".
   ELSE IF loan.cust-cat EQ "Б" THEN
      mClFileName = "banks".
   ELSE IF loan.cust-cat EQ "Ч" THEN
      mClFileName = "person".

   IF loan.contract EQ "sf-out" THEN
   DO:

      mSFSellerFIO = "".
      mSFBuyerFIO = "".

      mUserCode = FGetSetting("ПредстАкт",?,"").
      IF mUserCode NE "" THEN 
      DO:
         FIND FIRST _User WHERE _User._Userid EQ mUserCode NO-LOCK NO-ERROR.
         IF AVAILABLE _User THEN ASSIGN mSFSellerFIO = _User._User-Name.
      END.

      /*ASSIGN mSFSeller = FGetSetting("Банк",?,"").*/

      IF loan.cust-cat EQ "Ю"  OR loan.cust-cat EQ "Ч" OR loan.cust-cat EQ "Б" THEN
      DO:
         FIND FIRST cust-role WHERE 
            cust-role.file-name EQ mClFileName AND
            cust-role.class-code EQ "Представительство" AND
            cust-role.surrogate EQ STRING(loan.cust-id) NO-LOCK NO-ERROR.
         IF AVAILABLE cust-role THEN 
            ASSIGN mSFBuyerFIO = cust-role.cust-name.

         IF loan.cust-cat EQ "Ч" AND GetXattrValue("person", STRING(loan.cust-id),"Предпр") NE "" 
         THEN
         DO:
            mSFBuyer =   mSFBuyer. 
/*            mSFBuyer =  FGetSetting("СчФСтКл",?,"") + " " + mSFBuyer. */

         END.
      END.

      ASSIGN mSFBuyer = mSFBuyer.
      mShFBankRek = FGetSetting("СчФБанкРек",?,"").

      IF mShFBankRek EQ "Да" THEN
      DO:

         ASSIGN
            mSFSellerAddr = FGetSetting("Адрес_юр",?,"")
            mSFSellerINN = FGetSetting("ИНН",?,"")
            mSFSellerKPP  = FGetSetting("БанкКПП",?,"")
         .
      END.
      ELSE /*IF mShFBankRek EQ "Нет" THEN*/
      DO:

         /*mBranchId = getUserBranchId(USERID('bisquit')).*/
         FIND FIRST branch WHERE 
                    branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
         IF AVAIL branch THEN 
         DO:
            mBranchId = branch.branch-id.
            ASSIGN
               mSFSeller     = branch.name
               mSFSellerAddr = branch.address
               mSFSellerTel = GetXAttrValue("branch",mBranchId,"Телефон")
               mSFSellerINN = GetXAttrValue("branch",mBranchId,"ИНН")
               mSFSellerKPP = GetXAttrValue("branch",mBranchId,"КПП")
            .
         END.
      END.

   END.
   ELSE IF loan.contract EQ "sf-in" THEN
   DO:

      mSFSellerFIO = "".
      mSFBuyerFIO = "".

      mUserCode = FGetSetting("ПредстАкт",?,"").
      IF mUserCode NE "" THEN 
      DO:
         FIND FIRST _User WHERE _User._Userid EQ mUserCode NO-LOCK NO-ERROR.
         IF AVAILABLE _User THEN 
            ASSIGN mSFBuyerFIO = _User._User-Name.
      END.
      ASSIGN mSFBuyer = FGetSetting("Банк",?,"").

      IF loan.cust-cat EQ "Ю"  OR loan.cust-cat EQ "Ч" OR loan.cust-cat EQ "Б" THEN
      DO:
         FIND FIRST cust-role WHERE 
            cust-role.file-name EQ mClFileName AND
            cust-role.class-code EQ "Представительство" AND
            cust-role.surrogate EQ STRING(loan.cust-id) NO-LOCK NO-ERROR.
         IF AVAILABLE cust-role THEN 
            ASSIGN mSFSellerFIO = cust-role.cust-name.
      END.
      ASSIGN mSFSeller = mSFSeller.

      mShFBankRek = FGetSetting("СчФБанкРек",?,"").
      IF mShFBankRek EQ "Да" THEN
      DO:
         ASSIGN
            mSFBuyerAddr = FGetSetting("Адрес_юр",?,"")
            mSFBuyerINN = FGetSetting("ИНН",?,"")
            mSFBuyerKPP  = FGetSetting("БанкКПП",?,"")
         .
      END.
      ELSE IF mShFBankRek EQ "Нет" THEN
      DO:
         mBranchId = getUserBranchId(USERID('bisquit')).
         FIND FIRST branch WHERE 
                    branch.Branch-Id EQ mBranchId NO-LOCK NO-ERROR.
         IF AVAIL branch THEN 
         DO:
         ASSIGN
            mSFBuyerAddr = branch.address
            mSFBuyerINN = GetXAttrValue("branch",mBranchId,"ИНН")
            mSFBuyerKPP = GetXAttrValue("branch",mBranchId,"КПП")
         .
         END.
      END.

   END.

   {sfactprn2.i}     /*1 копия*/

   PUT STREAM sfact UNFORMATTED SKIP(4).

   IF mN GT 2 THEN 
      PAGE STREAM sfact.

   {sfactprn2.i}    /*2 копия*/

   PAGE STREAM sfact.

   /*печатаем*/
   {empty ttServ}

END.

{preview.i &STREAM="STREAM sfact"}

{intrface.del}
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='31/08/2015 17:24:07.825+04:00' */
/* $LINTUSER='kuds' */
/* $LINTMODE='1' */
/* $LINTFILE='sfactprn2.p' */
/*prosignRYYpZhXPz9oN5NibH7Qt5w*/