/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2004 ТОО "Банковские информационные системы"
     Filename: kauproc.p
      Comment: открытие счета на картотеке 1,2
   Parameters:
         Uses:
      Used by: 
      Created: 
     Modified: 01.06.2004 abko 0029139 поиск счета на карт2 с учетом подразделения
     Modified: 05.07.2004 abko 0021763 заполнение обязательных доп.рекв. при открытии
                          счета автоматическом
     Modified: 22.09.2005 kraw (0049978)
     Modified: 19.02.2008 muta 0088009 При редактировании номера счета при нажатии
                          ESC и отказе от сохранения счет не должен быть создан.
     Modified: 13/07/2009 kraw (0090110) В Create_Acct_Term обрабатывает токен "о" (кириллическое)
*/
{globals.i}
DEF VAR vclass    LIKE class.class-code NO-UNDO.
DEF VAR vacct-cat LIKE acct.acct-cat    NO-UNDO.
DEF VAR isCreate  AS LOGICAL      NO-UNDO.
DEF VAR in-bal-acct AS INT64    NO-UNDO.
DEFINE BUFFER inacct FOR acct.

DEF VAR MaskPod  AS CHARACTER INITIAL "*" NO-UNDO.
DEF VAR MaskSrok AS CHARACTER INITIAL "ДД.ММ.ГГГГ" NO-UNDO.
DEF VAR oresult  AS INT64   NO-UNDO.
DEFINE VARIABLE  mTmp-acct AS CHARACTER  NO-UNDO.
DEFINE VARIABLE in-op-date AS DATE        NO-UNDO. 
{wclass.i}
{intrface.get acct}     /* Библиотека для работы со счетами. */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get db2l}

{intrface.get rights}
{branch.pro}
{details.def} 
{topkind.def}

/*----------------------------------------------------------------------------*/

FUNCTION GetKauIdByContract RETURN CHARACTER PRIVATE
    (INPUT iContract AS CHARACTER):

    CASE iContract:
        WHEN "Карт1"    THEN RETURN "Карт-ка1".
        WHEN "Карт2"    THEN RETURN "Карт-ка2".
        WHEN "КартБлСч" THEN RETURN "КартБлСч".
        OTHERWISE            RETURN ?.
    END.
END FUNCTION.

FUNCTION getCardAcctXAttrName RETURN CHARACTER (INPUT iContract AS CHARACTER):
    RETURN SUBSTRING(iContract, 1, 5) + "ВнСчет".
END FUNCTION.

PROCEDURE GetPlanDate:
   DEFINE INPUT-OUTPUT PARAMETER ioPlanDate AS DATE NO-UNDO.

   RUN SetSysConf IN h_base ("СрокПлатежа", ?).
   RUN setplday.p (?,
                   "СортШабл",
                   "ДатаПлан",
                   6).
   IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN DO:
      pick-value = "no".
      ioPlanDate = ?.
   END.
   ELSE
      ioPlanDate = DATE(INT64(ENTRY(2,pick-value,".")),
                        INT64(ENTRY(3,pick-value,".")),
                        INT64(ENTRY(1,pick-value,".")))
      .
END PROCEDURE.

/*----------------------------------------------------------------------------*/

PROCEDURE MakeAcctByTerm:
   DEFINE INPUT  PARAMETER iIn-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iPlanDate    AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iEcho        AS LOGICAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iRid         AS RECID     NO-UNDO.  
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   MaskSrok = FGetSetting("СтандТр", 
                          "ПорядК1ПоСрокам", 
                          "ДД.ММ.ГГГГ").

   IF NUM-ENTRIES(MaskSrok,"|") = 2 THEN
      ASSIGN MaskPod  = ENTRY(1,MaskSrok,"|")
             MaskSrok = TRIM(ENTRY(2,MaskSrok,"|"))
      .

   RUN SetSysConf IN h_base ("СрокПлатежа",
                             STRING(YEAR(iPlanDate),"9999") + "." +
                             STRING(MONTH(iPlanDate),"99") + "." +
                             STRING(DAY(iPlanDate),"99")).

   RUN Create_Acct_Term (iIn-Rid,
                         iIn-contract,
                         iIn-op-date,
                         iPlanDate,                         
                         iEcho,
                         iRid,
                         OUTPUT oOut-Rid).
   /* Устанавливаем плановую дату, чтобы она не запрашивалась повторно 
      в процедуре setplday.p
    */
   IF RETURN-VALUE EQ "ERROR" THEN
      RUN SetSysConf IN h_base ("СрокПлатежа",?).

END PROCEDURE.

/*----------------------------------------------------------------------------*/
PROCEDURE MakeAcctByCust.
   DEFINE INPUT  PARAMETER iIn-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iEcho        AS LOGICAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iRid        AS RECID     NO-UNDO.  
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   RUN Create_Acct_Cust (iIn-Rid,
                         iIn-contract,
                         iIn-op-date,
                         iEcho,
                         iRid,
                         OUTPUT oOut-Rid).
END PROCEDURE.
/*----------------------------------------------------------------------------*/

PROCEDURE Create_acct:
   DEFINE INPUT  PARAMETER iIn-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iRid         AS RECID     NO-UNDO.  /* Шаблон счета */
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   DEFINE VARIABLE vPlanDate AS DATE    NO-UNDO.
   DEFINE VARIABLE vCountEnd AS INT64 NO-UNDO.
   DEFINE VARIABLE vCount    AS INT64 NO-UNDO.
   DEFINE VARIABLE vCodeVal  AS CHAR    NO-UNDO.

   IF (FGetSetting("СтандТр", "Карт1ПоСрокам", "") = "Да")
      AND iIn-contract = "Карт1" THEN
   DO:
      /* Вводим срок платежа */
      RUN GetPlanDate(INPUT-OUTPUT vPlanDate).
      IF vPlanDate EQ ? THEN
         RETURN "ERROR".
      RUN MakeAcctByTerm (iIn-Rid,
                          iIn-Contract,
                          iIn-op-date,
                          vPlanDate,
                          YES,
                          iRid,
                          OUTPUT oOut-Rid).

   END.
   ELSE
      RUN MakeAcctByCust(iIn-Rid,
                         iIn-Contract,
                         iIn-op-date,
                         YES,
                         iRid,
                         OUTPUT oOut-rid).
   RETURN RETURN-VALUE.
END PROCEDURE. /* Create_acct */
/*----------------------------------------------------------------------------*/

PROCEDURE FindCardAcctByXAttr.
    DEFINE INPUT PARAMETER  iCode      AS CHARACTER       NO-UNDO.
    DEFINE PARAMETER BUFFER acct       FOR  acct.
    DEFINE PARAMETER BUFFER card-acct  FOR  acct.

    DEFINE VARIABLE vAcct     LIKE acct.acct     NO-UNDO.
    DEFINE VARIABLE vCurrency LIKE acct.currency NO-UNDO.

    vAcct = GetXAttrValueEx("acct",
                            GetSurrogateBuffer("acct", (BUFFER acct:HANDLE)),
                            getCardAcctXAttrName(iCode),
                            "").
    IF NUM-ENTRIES(vAcct) = 2 THEN
        ASSIGN
            vCurrency = ENTRY(2, vAcct)
            vAcct     = ENTRY(1, vAcct)
        .
    ELSE
        vCurrency = acct.currency.
    {find-act.i &bact     = card-acct
                &acct     = vAcct
                &curr     = vCurrency
                &AddWhere = "AND card-acct.acct-cat   = 'o'           ~
                             AND card-acct.cust-cat   = acct.cust-cat ~
                             AND card-acct.cust-id    = acct.cust-id  ~
                             AND card-acct.close-date = ?"}
END PROCEDURE.

PROCEDURE Create_acct_Cust:

   DEFINE INPUT  PARAMETER in-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER in-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER in-op-date  AS DATE      NO-UNDO .
   DEFINE INPUT  PARAMETER iEcho       AS LOGICAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iRid        AS RECID     NO-UNDO.  /* RECID шаблона счета */
   DEFINE OUTPUT PARAMETER out-rid     AS RECID     NO-UNDO.

   DEFINE VARIABLE in-title AS CHARACTER             NO-UNDO.
   DEFINE VARIABLE s_acct   LIKE acct.acct           NO-UNDO.
   DEFINE VARIABLE vChkBrnch AS LOGICAL    INITIAL NO   NO-UNDO.
   DEFINE VARIABLE vBrnch-id AS CHARACTER  INITIAL "*"  NO-UNDO.
   DEFINE VARIABLE vDetails  AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vTmpVal   AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vRes      AS   LOGICAL                NO-UNDO.
   DEFINE VARIABLE vAcctO    AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vOpenByAcct  AS   LOGICAL             NO-UNDO.
   DEFINE VARIABLE vLinkAcct    AS   LOGICAL             NO-UNDO.

   DEFINE VARIABLE vKauId LIKE bal-acct.kau-id NO-UNDO.

   DEFINE VARIABLE mItem    AS INT64   NO-UNDO.
   DEFINE VARIABLE mItem1   AS INT64   NO-UNDO.

   DEFINE BUFFER acct  FOR acct .
   DEFINE BUFFER bacct FOR acct .

   vKauId = GetKauIdByContract(in-contract).

   CASE in-contract:
       WHEN "Карт1"    THEN
      ASSIGN
         in-title    = "картотеке 1"
         in-bal-acct = 90901
      .
       WHEN "Карт2"    THEN
      ASSIGN
         in-title    = "картотеке 2"
         in-bal-acct = 90902
      .
       WHEN "КартБлСч" THEN
           ASSIGN
               in-title    = "картотеке блокированных счетов"
               in-bal-acct = 90901
               in-contract = "КартБл"
           .
   END.
   IF in-contract EQ "КартБл" THEN vOpenByAcct = FGetSetting("КартБлСч","КартБССчет","") EQ "По счету".
   vLinkAcct = FGetSetting("СтандТр","КартЖесткСв","") EQ "Да".

   FIND inacct WHERE
        RECID(inacct) EQ in-rid
        NO-LOCK NO-ERROR.
   FIND bal-acct WHERE
        bal-acct.bal-acct EQ in-bal-acct
        NO-LOCK NO-ERROR.

    IF CAN-DO("Карт1,Карт2", in-contract) THEN DO:
        vChkBrnch = FGetSetting("СтандТр",
                                in-contract + "Подр",
                                "Нет") = "Да".
        vBrnch-id = IF vChkBrnch THEN inacct.branch-id ELSE "*".
   END.
    RUN FindCardAcctByXAttr(in-contract, BUFFER inacct, BUFFER acct).
    
   IF (FGetSetting("СтандТр","К2ПоКл","ДА") EQ "ДА") 
      OR in-contract NE "Карт2" THEN
   DO:

      IF AVAILABLE acct THEN
        s_acct = acct.acct.
      ELSE
    IF NOT (in-contract EQ "КартБл" AND vOpenByAcct) THEN DO: /* нет счета в доп реквизите */
         FIND acct WHERE
              acct.acct-cat   EQ "o"
          AND acct.contract   EQ in-contract
          AND acct.filial-id  EQ ShFilial
          AND acct.currency   EQ inacct.currency
          AND acct.cust-cat   EQ inacct.cust-cat
          AND acct.cust-id    EQ inacct.cust-id
          AND acct.close-date EQ ?
          AND (   NOT vChkBrnch /* или не проверяем подразделение */
               OR acct.branch-id EQ inacct.branch-id) /*или оно совпадает */
              NO-LOCK NO-ERROR.
         IF AMBIGUOUS acct THEN
         DO:
            pick-value = '' .
            RUN "acct(k).p" ( bal-acct.bal-acct,
                              vKauId,
                              inacct.cust-cat,
                              inacct.cust-id,
                              inacct.currency,
                              vBrnch-id,
                              4
                             ).
            IF     pick-value <> ''
               AND pick-value <> ?
            THEN
               {find-act.i
                  &acct = ENTRY(1,pick-value)
                  &curr = ENTRY(2,pick-value)
               }
            ELSE
               RETURN 'ERROR' .
         END.
      END.
      
   IF NOT AVAIL acct AND NOT (in-contract EQ "КартБл" AND vOpenByAcct) THEN
         FIND FIRST acct WHERE
                    acct.acct-cat EQ "o"
                AND acct.contract EQ in-contract
                AND acct.currency EQ inacct.currency
                AND acct.filial-id  EQ ShFilial
                AND acct.cust-cat EQ inacct.cust-cat
                AND acct.cust-id  EQ inacct.cust-id
                AND (   NOT vChkBrnch /* или не проверяем подразделение */
                     OR acct.branch-id EQ inacct.branch-id) /*или оно совпадает */
                    NO-LOCK NO-ERROR.
   
   END.
   else
   do:

      IF NOT AVAIL acct THEN
      do:
         vAcctO = "".
         FND-ACCT:
         FOR EACH acct WHERE
                  acct.acct-cat EQ "o"
              AND acct.contract EQ in-contract
              AND acct.currency EQ inacct.currency
              AND acct.filial-id  EQ ShFilial
              AND acct.cust-cat EQ inacct.cust-cat
              AND acct.cust-id  EQ inacct.cust-id
              AND (   NOT vChkBrnch /* или не проверяем подразделение */
                   OR acct.branch-id EQ inacct.branch-id) /*или оно совпадает */
              NO-LOCK:
         
              find first bacct WHERE
                         bacct.cust-cat EQ inacct.cust-cat
                     AND bacct.cust-id  EQ inacct.cust-id
                     AND bacct.acct-cat EQ "b" 
                     AND bacct.filial-id EQ ShFilial
                     AND can-find(signs where signs.file-name eq "acct" and
                                              signs.surrogate eq bacct.acct + "," + bacct.currency and
                                              signs.code eq "Карт2ВнСчет" and
                                              signs.xattr-value eq acct.acct + "," + acct.currency) no-lock no-error.
              if not avail bacct then 
              do:
                vAcctO = acct.acct.
                leave FND-ACCT.
              end.
         end.
         {find-act.i
            &acct = vAcctO
         }

      end.        
   end.
  
      IF AVAIL acct AND acct.close-date NE ? THEN
      DO:
         IF iEcho THEN
            MESSAGE "Счет на " + in-title " " + acct.acct
                    (IF acct.currency NE "" THEN "/" ELSE "") +  acct.currency SKIP
                    " на внебалансовом счете " +  STRING(acct.bal-acct) + " закрыт !" SKIP
                    "Открыть его ?"
                    VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE choice AS LOGICAL.
         ELSE
            choice = YES.
         IF choice THEN
         DO:
            FIND FIRST acct WHERE
                       acct.acct-cat   EQ "o"
                   AND acct.contract   EQ in-contract
                   AND acct.filial-id  EQ ShFilial
                   AND acct.currency   EQ inacct.currency
                   AND acct.cust-cat   EQ inacct.cust-cat
                   AND acct.cust-id    EQ inacct.cust-id
                   AND acct.close-date NE ?
                   AND (   NOT vChkBrnch /* или не проверяем подразделение */
                        OR acct.branch-id EQ inacct.branch-id) /*или оно совпадает */
                       EXCLUSIVE-LOCK NO-ERROR.
            acct.close-date = ?.
         END.
         ELSE RELEASE acct.
      END.


   IF NOT AVAILABLE acct THEN 
   DO:
      {acc-file.i
         &access-class = "'accto'"
         &access-mode  = c
         &no-access    = "message 'Счет на ' + in-title + ' у клиента отсутствует,' skip
                        'а создать его вы не имеете права.' VIEW-AS ALERT-BOX ERROR. RETURN 'ERROR' "
      }
      vclass = 'accto'.

      /* Получим маску счета и  tokacct */
      DEF VAR acctmask AS CHAR FORMAT "x(25)" NO-UNDO.
      DEF VAR tokacct AS CHAR FORMAT "x(30)" NO-UNDO.
      DEFINE VARIABLE vKodDoxRash AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sTokidx AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sToklen AS CHARACTER  NO-UNDO.

      RUN "FindAcctMask" IN h_acct 
         (
         vclass, 
         bal-acct.bal-acct,
         INPUT-OUTPUT acctmask,
         INPUT-OUTPUT vKodDoxRash 
         ) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().

      FIND FIRST op-template WHERE RECID(op-template) EQ iRid NO-LOCK NO-ERROR.

      IF AVAILABLE op-template THEN
      DO:
         vDetails = GetXattrValue("op-template",
                                  op-template.op-kind + "," + string(op-template.op-template),
                                  "CustInterna").
         acctmask = GetXattrValueEx("op-template",
                                    op-template.op-kind + "," + string(op-template.op-template),
                                    "open_mask",
                                    acctmask).
      END.
      ELSE DO:
         vTmpVal = GetSysConf("НаимСчетаК2").
         IF {assigned vTmpVal} THEN vDetails = vTmpVal.
         vTmpVal = GetSysConf("МаскаСчетаК2").
         IF {assigned vTmpVal} THEN acctmask = vTmpVal.
      END.

      RUN GetAcctMask IN h_acct 
          (
          INPUT acctmask,
          OUTPUT tokacct,    
          OUTPUT sTokidx,    
          OUTPUT sToklen
          )
      NO-ERROR.      
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().
      mItem1 = LENGTH(TRIM(delFilFromAcct(inacct.acct))).

      acctmask = tokacct.
      DO mItem = LENGTH(acctmask) TO 1 BY -1:

         IF SUBSTRING(acctmask, mItem, 1) EQ "о" THEN
         DO:
            acctmask = (IF mItem GT 1 THEN SUBSTRING(acctmask, 1, mItem - 1)
                                    ELSE "")
                    + SUBSTRING(TRIM(delFilFromAcct(inacct.acct)), mItem1, 1)
                    + (IF mItem LT LENGTH(acctmask) THEN SUBSTRING(acctmask, mItem + 1)
                                                  ELSE "").
            mItem1 = mItem1 - 1.
         END.
      END. 

      tokacct = REPLACE(tokacct,"о","с").


      IF {assigned vDetails} THEN DO:

         RUN SetSysConf IN h_base ("РасчСчет", inacct.acct).
         RUN SetSysConf IN h_base ("РасчСчетВал", inacct.currency).

         RUN ProcessDetails (?, input-output vDetails).

         RUN DeleteOldDataProtocol IN h_base ("РасчСчет").
         RUN DeleteOldDataProtocol IN h_base ("РасчСчетВал").
      END.

      ASSN-ACCT:
      DO
         ON ERROR  UNDO ASSN-ACCT, RETRY
         ON ENDKEY UNDO,           LEAVE
         WITH FRAME newacc:
         
         /* Для картотеки блокированных счетов предоставим
            явные шаблоны создаваемых внебалансовых счетов */
         IF in-contract = "КартБл" THEN DO:
             {empty tOpKindParams}
             ASSIGN
                 vRes = TDAddParam("iCurrency", inacct.currency)        AND
                        TDAddParam("iCustCat" , inacct.cust-cat)        AND
                        TDAddParam("iCustId"  , STRING(inacct.cust-id)) AND
                        TDAddParam("iAcctMask", acctmask)               AND
                        TDAddParam("iRegimAuto", STRING(NOT iEcho))
             NO-ERROR.
             IF vRes THEN
                 RUN ex-trans.p("_CBLACTC",
                                in-op-date,
                                TABLE tOpKindParams,
                                OUTPUT vRes,
                                OUTPUT vTmpVal).
             ELSE
                 vTmpVal = "Ошибка передачи параметров " +
                           "в транзакцию " + QUOTER("_CBLACTC").
             IF NOT vRes THEN
                 RETURN ERROR "ERROR: " + vTmpVal.
             vTmpVal = GetSysConf("_CBLACTC_acct").
             RUN DeleteOldDataProtocol IN h_base ("_CBLACTC_acct").
             IF {assigned vTmpVal} THEN DO:
                 {find-act.i &acct = vTmpVal
                             &curr = inacct.currency}
             END.
         END.
         ELSE DO:
         RUN Cm_acct_cr IN h_acct (
                vclass,               /* iClass                  */  
                bal-acct.bal-acct,    /* iBal                    */  
                inacct.currency,      /* iCurr                   */  
                inacct.cust-cat,      /* iCustCat                */  
                inacct.cust-id,       /* iCustID                 */  
                in-op-date,           /* iOpenDate               */  
                OUTPUT mTmp-acct,     /* oAcct                   */  
                BUFFER acct,           /* BUFFER iacct FOR acct . */  
                acctmask,              /* iAcctMask               */  
                vDetails,              /* iDetails                */  
                vKauId,                /* iKauId                  */  
                in-contract,           /* iContract               */  
                USERID ('bisquit'),    /* iUserId                 */  
                inacct.branch-id,           /* iBranchId               */  
                YES                    /* iCopyBalXattr           */  
         ) NO-ERROR. 
         IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().
         isCreate = YES.
         IF iEcho THEN 
            RUN edtnacct.p ( in-title,
                             RECID(acct),
                             tokacct,
                             OUTPUT isCreate
                           ). 

         IF NOT isCreate THEN UNDO ASSN-ACCT, LEAVE.
         RUN Create_xattr_mand (RECID(acct), OUTPUT oresult).
         END.
      END. /* ASSN-ACCT */
   END.

    IF NOT AVAILABLE acct THEN
        RETURN.
      out-rid = RECID(acct).
    IF NOT {assigned s_acct} THEN DO:
        /* 
        pick-value = "NO".
        IF (in-contract EQ "КартБл" AND vOpenByAcct) OR vLinkAcct THEN pick-value = "YES".
        ELSE
        RUN Fill-AlertSysMes IN h_tmess ("",
                                    "",
                                    "4",
                                    "Создать жесткую связь между"             +
                                    " балансовым счетом"                      +
                                    " " + inacct.acct                         +
                                    "~nи внебалансовым счетом "               + 
                                    in-contract + " " + acct.acct                           +
                                    "?").
        IF KEYFUNCTION(LASTKEY) <> "END-ERROR" AND pick-value = "YES" THEN
        */
            IF NOT UpdateSigns(inacct.class-code,
                               GetSurrogateBuffer("acct",
                                                  (BUFFER inacct:HANDLE)),
                               getCardAcctXAttrName(in-contract),
                               GetSurrogateBuffer("acct",
                                                  (BUFFER acct:HANDLE)),
                               ?)
         THEN
                RUN Fill-SysMes IN h_tmess ("", "", "0", "Связь не создана").
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/

/* Процедура ищет/открывает счета на картотеках по срокам (доп. реквизит "СрокПлатежа") */
PROCEDURE Create_Acct_Term:
   DEFINE INPUT  PARAMETER iIn-Rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iPlanDate    AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iEcho        AS LOGICAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iRid         AS RECID     NO-UNDO.
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   DEFINE VARIABLE vTitle      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpMask    AS CHARACTER INIT "" NO-UNDO.
   DEFINE VARIABLE vMaskBranch AS CHARACTER INIT "" NO-UNDO.
   DEFINE VARIABLE i           AS INT64           NO-UNDO.
   DEFINE VARIABLE vIsCartAcct AS LOGICAL           NO-UNDO.
   DEFINE VARIABLE vacct_o     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vcurr_o     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPlanDateSt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChkBrnch   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vBrnch-id   AS CHARACTER  INIT "*"  NO-UNDO.
   DEFINE VARIABLE vDetails    AS CHARACTER            NO-UNDO.
   vPlanDateSt = STRING(iPlanDate,"99/99/9999").
   CASE MaskSrok:
      WHEN "ДД.ММ" THEN DO:
         vPlanDateSt = STRING(DAY(iPlanDate),"99") + "/" + STRING(MONTH(iPlanDate),"99").
      END.
      WHEN "ДД" THEN DO:
         vPlanDateSt = STRING(DAY(iPlanDate),"99") + "/".
      END.
   END CASE.

   IF iIn-contract EQ "Карт1" THEN
      ASSIGN
         vTitle      = "картотеке 1"
         in-bal-acct = 90901
      .
   FIND FIRST inacct WHERE RECID(inacct) EQ iIn-rid
      NO-LOCK
      NO-ERROR.
   FIND FIRST bal-acct WHERE bal-acct.bal-acct EQ in-bal-acct
      NO-LOCK
      NO-ERROR.

   RUN GetBranchParent(inacct.branch-id,INPUT-OUTPUT vTmpMask).
   
   DO i = 1 TO NUM-ENTRIES(vTmpMask):
      IF CAN-DO(MaskPod,ENTRY(i,vTmpMask)) THEN
      {additem.i vMaskBranch ENTRY(i,vTmpMask)}
   END.
   IF LOOKUP(inacct.branch-id,vMaskBranch) EQ 0 THEN
      vMaskBranch = inacct.branch-id + "," + vMaskBranch.
   
   /* проверять принадлежность к одному подразделению бал. и карт. счетов */
   vChkBrnch = FGetSetting("СтандТр","Карт1Подр", "Нет") = "Да".
   vBrnch-id = (IF vChkBrnch EQ YES 
                THEN inacct.branch-id
                ELSE "*").

/* вначале поищем открытый счет в подразделении балансового счета и далее в вышестоящих */
   vIsCartAcct = FALSE.
   DO i = 1 TO NUM-ENTRIES(vMaskBranch):
      vTmpMask = ENTRY(i,vMaskBranch).
      /* пойдем от реквизита */
      FOR EACH signs WHERE signs.file-name EQ "acct"
                       AND signs.code      EQ "СрокПлатежа"
                       AND ENTRY(2,signs.surrogate) EQ inacct.currency
                       AND signs.code-val  BEGINS vPlanDateSt
          USE-INDEX allfield NO-LOCK,
          FIRST acct WHERE acct.branch-id EQ vTmpMask          /*проверим подразделение */
                       AND acct.acct-cat  EQ "o"               /*проверим категорию     */
                       AND acct.bal-acct  EQ bal-acct.bal-acct /*проверим счет 2-го порядка */
                       AND acct.acct      EQ ENTRY(1,signs.surrogate)
                       AND acct.currency  EQ inacct.currency
                       AND acct.cust-cat  EQ 'В'        /* внутренний  */
                       AND acct.close-date EQ ?         /* не закрытый */
                       AND (   NOT vChkBrnch /* или не проверяем подразделение */
                            OR acct.branch-id EQ inacct.branch-id) /*или оно совпадает */
              USE-INDEX branch-id NO-LOCK:

         vIsCartAcct = TRUE. /*значит нашли подходящий счет */
         LEAVE.
      END. /* FOR EACH */
      IF vIsCartAcct THEN
         LEAVE.
   END. /* DO TO NUM-ENTRIES(vTmpMask)*/

   IF NOT vIsCartAcct THEN
   DO i = 1 TO NUM-ENTRIES(vMaskBranch):
/* тоже но для закрытых*/
      vTmpMask = ENTRY(i,vMaskBranch).
      FOR EACH signs WHERE signs.file-name EQ "acct"
                       AND signs.code      EQ "СрокПлатежа"
                       AND ENTRY(2,signs.surrogate) EQ inacct.currency
                       AND signs.code-val  BEGINS vPlanDateSt
          USE-INDEX allfield NO-LOCK,
          FIRST acct WHERE acct.branch-id EQ vTmpMask          /*проверим подразделение */
                       AND acct.acct-cat  EQ "o"               /*проверим категорию     */
                       AND acct.bal-acct  EQ bal-acct.bal-acct /*проверим счет 2-го порядка */
                       AND acct.acct      EQ ENTRY(1,signs.surrogate)
                       AND acct.currency  EQ inacct.currency
                       AND acct.cust-cat  EQ 'В'        /* внутренний  */
                       AND acct.close-date NE ?         /* закрытый */
                       AND (   NOT vChkBrnch /* или не проверяем подразделение */
                            OR acct.branch-id EQ inacct.branch-id) /*или оно совпадает */
              USE-INDEX branch-id NO-LOCK:
      
         vIsCartAcct = TRUE. /*значит нашли подходящий счет */
         LEAVE.
      END. /* FOR EACH */
      IF vIsCartAcct THEN
         LEAVE.
   END.

   IF AVAILABLE acct THEN
   DO:
      IF acct.close-date NE ? THEN
      DO:
         IF iEcho THEN
            MESSAGE "Счет на " + vTitle " " + acct.acct
                    (IF acct.currency NE "" THEN "/" ELSE "") +  acct.currency SKIP
                    " на внебалансовом счете " +  string(acct.bal-acct) + " закрыт !" SKIP
                    "Открыть его ?"
               VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE choice AS LOGICAL.
         ELSE choice = YES.
         IF choice THEN
         DO:
            FIND CURRENT acct
               EXCLUSIVE-LOCK
               NO-ERROR.
            acct.close-date = ? .
         END.
         ELSE DO:
            MESSAGE "Открыть новый счет ?"
               VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE choice.
            IF NOT choice THEN
                   RETURN "ERROR".
         END.
      END.
   END.

   IF NOT AVAILABLE acct THEN
   DO:
      {acc-file.i
         &access-class = "'accto'"
         &access-mode  = c
         &no-access    = "message 'Счет на ' + vTitle + ' по дате платежа отсутствует,' skip
                        'а создать его вы не имеете права.' VIEW-AS ALERT-BOX ERROR. RETURN 'ERROR' "
      }   
      vclass = 'accto'.

      /* Получим маску счета и  tokacct */
      DEF VAR acctmask AS CHAR FORMAT "x(25)" NO-UNDO.
      DEF VAR tokacct AS CHAR FORMAT "x(30)" NO-UNDO.
      DEFINE VARIABLE vKodDoxRash AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sTokidx AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sToklen AS CHARACTER  NO-UNDO.


      RUN "FindAcctMask" IN h_acct 
         (
         vclass, 
         bal-acct.bal-acct,
         INPUT-OUTPUT acctmask,
         INPUT-OUTPUT vKodDoxRash 
         ) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().

      RUN GetAcctMask IN h_acct 
          (
          INPUT acctmask,
          OUTPUT tokacct,    
          OUTPUT sTokidx,    
          OUTPUT sToklen
          )
      NO-ERROR.      
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().

      FIND FIRST op-template WHERE RECID(op-template) EQ iRid NO-LOCK NO-ERROR.
      IF AVAIL(op-template) THEN vDetails = GetXattrValue("op-template",op-template.op-kind + "," + string(op-template.op-template),"CustInterna").

      IF {assigned vDetails} THEN DO:

         RUN SetSysConf IN h_base ("РасчСчет", inacct.acct).
         RUN SetSysConf IN h_base ("РасчСчетВал", inacct.currency).

        RUN ProcessDetails (?, input-output vDetails).

         RUN DeleteOldDataProtocol IN h_base ("РасчСчет").
         RUN DeleteOldDataProtocol IN h_base ("РасчСчетВал").
      END.

      ASSN-ACCT:
      DO
         ON ERROR  UNDO ASSN-ACCT, RETRY
         ON ENDKEY UNDO,           LEAVE
         WITH FRAME newacc:

         RUN Cm_acct_cr IN h_acct (
                vclass,               /* iClass                  */  
                bal-acct.bal-acct,    /* iBal                    */  
                inacct.currency,      /* iCurr                   */  
                'В',                  /* iCustCat                */  
                ?,                    /* iCustID                 */  
                iIn-op-date,           /* iOpenDate               */  
                OUTPUT mTmp-acct,     /* oAcct                   */  
                BUFFER acct,           /* BUFFER iacct FOR acct . */  
                acctmask,              /* iAcctMask               */  
                IF NOT {assigned vDetails} THEN 
                    'Срок платежа ' + STRING(iPlanDate) 
                   ELSE  vDetails,     /* iDetails                */  
                GetKauIdByContract(iIn-contract), /* iKauId                  */  
                iIn-contract,           /* iContract               */  
                USERID ('bisquit'),    /* iUserId                 */  
                inacct.branch-id,           /* iBranchId               */  
                YES                    /* iCopyBalXattr           */  
         ) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().
         isCreate = YES.
         IF iEcho THEN
            RUN edtnacct.p(vTitle, recid(acct), tokacct, OUTPUT isCreate).
         IF NOT isCreate THEN
            RETURN "ERROR".
         UpdateSigns("accto",
                     acct.acct + "," + acct.currency,
                     "СрокПлатежа",
                     STRING(iPlanDate,"99/99/9999"),?).
         RUN Create_xattr_mand (RECID(acct), OUTPUT oresult).
      END. /* ASSN-ACCT */
   END.
   IF AVAIL acct THEN
      oOut-rid = RECID(acct).
END PROCEDURE. /* Create_Acct_Term */

PROCEDURE Create_xattr_mand:
   DEFINE INPUT PARAMETER iAcctRid AS RECID NO-UNDO.
   DEFINE OUTPUT PARAMETER olastkey AS INT64 NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   FIND FIRST acct WHERE RECID(acct) EQ iAcctRid NO-LOCK NO-ERROR.

   /* заполним обязательные др. если они есть */
   RUN chsigns.p (acct.class-code,
                  "accto",
                  acct.acct + "," + acct.currency,
                  YES,
                  OUTPUT oresult).
   /* Если есть незаполненные - запускаем редактирование. */
   IF oresult GT 0
      THEN RUN "xattr-ed.p" (acct.class-code,
                             acct.acct + "," + acct.currency,
                             " СЧЕТА " + acct.acct, YES, 3).
   olastkey = LASTKEY.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='08/10/2014 15:50:50.912+04:00' */
/* $LINTFILE='kauproc.p' */
/*prosign+e5V0tOKGzWjHz7PGJmNeQ*/