{globals.i}
{intrface.get pbase}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{intrface.get instrum}
{intrface.get tmess}
{intrface.get pqres}
{dpsproc.def}
{parsin.def}
{sh-defs.i}
{tmprecid.def}

DEFINE VARIABLE mInt         AS INT64     NO-UNDO.
DEFINE VARIABLE mBegDate     AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate     AS DATE      NO-UNDO.
DEFINE VARIABLE mFilial      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskDb      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskCr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVal         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClassCode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBalAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrency    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctMask    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDetails     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKauID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mContract    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUserID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBranchID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustCat     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustID      AS INT64     NO-UNDO.
DEFINE VARIABLE mResAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResDate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE oAcct        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNumber      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mErrMsg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUpd         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mField       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUpdValid    AS INT64     NO-UNDO.

/*{setdest.i &filename = "'111.log'"}*/

ASSIGN
   mInt     = 0.
   
FOR EACH tmprecid NO-LOCK,
   FIRST cust-corp WHERE
   RECID(cust-corp) EQ tmprecid.id
   NO-LOCK:
   ASSIGN
      mInt    = mInt + 1
      mCustID = cust-corp.cust-id.
END.

IF    mInt GT 1
   OR mInt EQ 0 THEN
DO:   
   MESSAGE "Должен быть выбран один клиент!" 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND op-kind WHERE 
   op-kind.op-kind EQ "resacct"
NO-LOCK NO-ERROR.
FIND FIRST op-template OF op-kind
NO-LOCK NO-ERROR.
IF    NOT AVAIL(op-kind)
   OR NOT AVAIL(op-template) THEN
DO:
   MESSAGE "Не найдена транзакция открытия счета." 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

ASSIGN
   mClassCode = op-template.cr-class-code
/*   mBalAcct   = Get_Param("bal-acct",  RECID(op-template))*/
   mCurrency  = IF Get_Param("l-currency",RECID(op-template)) EQ ? THEN "" ELSE Get_Param("l-currency",RECID(op-template))
   mAcctMask  = Get_Param("open_mask", RECID(op-template))
   mDetails   = op-template.details
   mKauID     = Get_Param("kau-id",RECID(op-template))
   mContract  = Get_Param("contract",RECID(op-template))
   mUserID    = USERID("bisquit")
   mBranchID  = shFilial
   mCustCat   = "Ю"
   mResAcct   = GetXattrValueEx("cust-corp",STRING(mCustID),"ResAcct","")
   mResDate   = GetXAttrValueEx("cust-corp",STRING(mCustID),"ResDate","")
   mNumber    = DelFilFromAcct(mResAcct)
   mBalAcct   = SUBSTRING(mNumber,1,5)
/*   mCurrency  = IF SUBSTRING(mNumber,6,3) EQ "810" THEN "" ELSE SUBSTRING(mNumber,6,3)*/
   .
   

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   {find-act.i
      &acct = mResAcct 
   }
   
   IF AVAIL(acct) THEN
   DO:
      MESSAGE "Счет " + mNumber + " уже открыт." 
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   
   oAcct = mResAcct.
      
   RUN MyMakeAcct  (
         mClassCode,          /* iClass                  */  
         INT64(mBalAcct),     /* iBal                    */  
         mCurrency,           /* iCurr                   */  
         mCustCat,            /* iCustCat                */  
         mCustID,             /* iCustID                 */  
         TODAY,               /* iOpenDate               */  
         INPUT-OUTPUT oAcct,  /* oAcct                   */  
         BUFFER acct,         /* BUFFER iacct FOR acct . */  
         mAcctMask,           /* iAcctMask               */  
         "",                  /* iKodDoxRash             */  
         mDetails,            /* iDetails                */  
         mKauID,              /* iKauId                  */  
         mContract,           /* iContract               */  
         mUserID,             /* iUserId                 */  
         mBranchID,           /* iBranchId               */  
         YES                  /* iCopyBalXattr           */  
   ) NO-ERROR.
      
	IF ERROR-STATUS:ERROR THEN
   DO:   
      MESSAGE "Ошибка при создании счета." 
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

   /* Инициализация доп.реквизитов со счета 2-го порядка
   ** и из классификатора "МаскиНаслед". */
   RUN BalToAcct_Xattr(RECID(acct),"*",YES,YES).

   RUN Check-Acct IN h_acct (
      BUFFER acct,
      OUTPUT mField,
      OUTPUT mUpdValid
   ).
   IF mUpdValid NE 0
      THEN RETURN ERROR.

   /* Удаляем счет из классификатора "СчетаРезерва". */
   RUN AcctFree IN h_acct (oAcct                ,OUTPUT mOk).
   RUN AcctFree IN h_acct (DelFilFromAcct(oAcct),OUTPUT mOk).
    
   mUpd = UpdateSigns("cust-corp",STRING(mCustID),"ResDate","",?).
   mUpd = UpdateSigns("cust-corp",STRING(mCustID),"ResAcct","",?).
   
   mUpd = UpdateSigns("acct",acct.acct + "," + acct.curr,"ResDate",mResDate,?).

   MESSAGE "Счет " + DelFilFromAcct(oAcct) + " открыт."
   VIEW-AS ALERT-BOX TITLE " Сообщение ".
   
END.

/*{preview.i &filename = "'111.log'"}*/

{intrface.del}

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.

PROCEDURE MyMakeAcct:
   DEF INPUT  PARAM iClass          AS CHAR   NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iBal            AS INT64  NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iCurr           AS CHAR   NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iCustCat        AS CHAR   NO-UNDO. /* Обязательный */
   DEF INPUT  PARAM iCustID         AS INT64  NO-UNDO. /* неОбязательный */
   DEF INPUT  PARAM iOpenDate       AS DATE   NO-UNDO. /* неОбязательный */
   
   DEF INPUT-OUTPUT PARAM oAcct           AS CHAR      /* Откатываемый */
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
  
   DEFINE VARIABLE vClass AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAcct-Cat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mSymbPU AS CHARACTER NO-UNDO.
   DEFINE VARIABLE acctmask AS CHARACTER NO-UNDO.
   ASSIGN
      vClass    = iClass
      vAcct-Cat = GetXAttrInit(iClass,"acct-cat")
   .
   FIND FIRST bal-acct WHERE
      bal-acct.bal-acct EQ iBal
   NO-LOCK NO-ERROR.
   {was-err.i}

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
         acct.cust-id      =  IF iCustCat    NE 'В'   THEN iCustID   ELSE 0
         acct.open-date    =  IF iOpenDate   NE ?     THEN iOpenDate ELSE TODAY
         acct.contract     =  IF       iContract NE ?
                                 AND   iContract NE ''
                                 THEN iContract
                                 ELSE IF bal-acct.contract NE ""
                                    THEN bal-acct.contract
                                    ELSE acct.contract
         acct.user-id      =  IF    iUserId  EQ ""
                                 OR iUserId  EQ ?
                                 THEN USERID ("bisquit")
                                 ELSE iUserId
         acct.side         =  bal-acct.side
                                 WHEN LOOKUP(bal-acct.side, 'А,П,АП') NE 0
         acct.acct-cat     =  bal-acct.acct-cat
         acct.rate-type    =  IF acct.currency NE ""
                                 THEN "Учетный"
                                 ELSE ""
         acct.kau-id       = ""
         /*         acct.kau-id       =  IF       iKauId NE ? */
/*                                 AND   iKauId NE ''*/
/*                                 THEN iKauId       */
/*                                 ELSE ""           */
      NO-ERROR.
      {was-err.i &LBL=TR }
      ASSIGN
         acct.branch-id    =  IF    iBranchId EQ ""
                                 OR iBranchId EQ ?
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

      IF GetCode ("ШаблКАУ", acct.kau-id) EQ ?
         THEN acct.kau-id  = GetXAttrInit(vClass,"kau-id").

      ASSIGN
         acct.acct   =  oAcct
         acct.number =  DelFilFromAcct(oAcct)
      NO-ERROR.
      {was-err.i &LBL=TR }
                        /* Вызов триггера на WRITE. */
      VALIDATE acct NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE GetErrMsg()
         VIEW-AS ALERT-BOX.
      END.
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
