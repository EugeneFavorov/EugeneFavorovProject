



{globals.i}
{tmprecid.def}  
{intrface.get xclass}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{intrface.get dps}
{intrface.get dpspr}
{dpsproc.def}
{f_for_t.i} /* Функции для работы с довложениями */
{sh-defs.i}             /* Переменные для рассчета остатка по счету. */


DEFINE TEMP-TABLE dps_param NO-UNDO
    FIELD   acct    LIKE    acct.acct
    FIELD   curr    LIKE    acct.currency
    FIELD   kau     LIKE    kau.kau
    FIELD   name    AS CHAR
    FIELD   blns    LIKE    acct-pos.balance
    FIELD   date1 AS DATE
    FIELD   date2 AS DATE
    INDEX   acct acct.



DEF VAR d1 AS DATE.
DEF VAR d2 AS DATE.
DEF VAR dat1 AS DATE NO-UNDO.
DEF VAR dd1 AS DATE NO-UNDO .
DEF VAR dd2 AS DATE NO-UNDO .
DEF VAR Is_Old AS LOGICAL NO-UNDO .
DEF VAR fl-prol-cond AS LOGICAL NO-UNDO .
DEF VAR mPeriod AS CHAR NO-UNDO .
DEF VAR mOstatok AS DECIMAL NO-UNDO.
def var result  as decimal no-undo.
def var result1 as decimal no-undo.
def var flag    as INT64 init -1.
def var l_acct    like loan-acct.acct no-undo. /* Счет вклада */
def var l_acct_type as char           no-undo.

&glob str-type "loan-dps-ts,loan-dps-tsk,loan-dps-ink"


DEFINE VARIABLE vDRVal        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vSubj         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vI            AS INT64   NO-UNDO.
DEFINE VARIABLE mName         AS CHARACTER NO-UNDO EXTENT 3.
DEFINE VARIABLE mCode         AS CHARACTER NO-UNDO.
def var cred-per as char format "x(17)".
def var int-per  as char format "x(17)".
DEFINE VARIABLE tels          AS CHARACTER EXTENT 6 FORMAT "x(17)"
  INIT ['Ежемесячно по ',
        'Ежеквартально по ',
        'Декада по 11,21,31/1',
        'Произвольно',
        'Полугод',
        'Год'].
def var m-comm       as   char                       no-undo.
def var s-comm       as   char                       no-undo.
def var a-inter      as   char                       no-undo.
def var pen-interest as   char                       no-undo.
DEF VAR mMin-ostStr  AS   CHAR                       NO-UNDO.
DEFINE VARIABLE sh-val-close  AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-val-na     AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-bal-close  AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-bal-na     AS DECIMAL NO-UNDO.


DEF BUFFER bloan     FOR loan.
DEF BUFFER b-acct    FOR acct.
      
FUNCTION GetCliName RETURN CHAR (
   in-cat AS CHAR,
   in-id AS CHAR
):
   IF    NOT {assigned in-cat}
      OR NOT {assigned in-id}
      THEN RETURN "".
   CASE in-cat:
   WHEN "В" THEN RETURN GetObjName("branch",in-id,NO).
   OTHERWISE DO:
      RUN GetCustName IN h_base(in-cat,in-id,?,
                                OUTPUT mName[1],
                                OUTPUT mName[2],
                                INPUT-OUTPUT mName[3]).
      RETURN TRIM(mName[1]) + " " + TRIM(mName[2]).
   END.
   END CASE.
END FUNCTION.


{setdest.i &col=170 } 

FOR EACH tmprecid,
   FIRST bloan WHERE
      RECID(bloan) EQ tmprecid.id
NO-LOCK:

/* Поиск счета вклада */
   IF bloan.class-code NE "dep_pers_trans" AND
      NOT CAN-DO(bloan.class-code,GetXclassAllChildsEx("dps_pers_trans_cap")) THEN
      DO:
         RUN GetBaseAcctRole (RECID(bloan),
                        gend-date,
                        OUTPUT l_acct_type). 
                      
      FIND LAST loan-acct OF bloan WHERE
         loan-acct.acct-type EQ l_acct_type /*BEGINS (if loan.end-date eq ? then "loan-dps-p"
                                                    else "loan-dps-t")*/ AND
         loan-acct.since LE gend-date
         NO-LOCK NO-ERROR.

      /* На случай перехода срочного вклада во вклад до востребования, но
       деньги остаются на счете срочного вклада */
      IF NOT AVAIL loan-acct AND bloan.end-date EQ ? THEN
          FIND LAST loan-acct OF bloan WHERE loan-acct.acct-type EQ "loan-dps-t"
       NO-LOCK NO-ERROR.

      END.
      ELSE 
      DO:
         IF NOT CAN-DO(bloan.class-code,GetXclassAllChildsEx("dps_pers_trans_cap")) THEN
         DO:
            FIND LAST loan-acct OF bloan WHERE
            loan-acct.acct-type EQ (IF bloan.end-date NE ? THEN "loan-dps-ts" ELSE "loan-dps-ps") AND
            loan-acct.since LE gend-date
            NO-LOCK NO-ERROR.
         END.
         ELSE
         DO:
            FIND LAST loan-acct OF bloan WHERE
            loan-acct.acct-type EQ "loan-dps-tsk" AND
            loan-acct.since LE gend-date
            NO-LOCK NO-ERROR.
         END.
      END.

      l_acct = IF AVAIL loan-acct THEN DelFilFromAcct(loan-acct.acct)
         ELSE "На " + STRING (gend-date) + " счет не определен".
      FIND b-acct WHERE b-acct.acct EQ loan-acct.acct NO-LOCK NO-ERROR.
      PUT UNFORMATTED "ВКЛАД N:"  bloan.cont-code FORMAT "x(20)" SKIP.
      PUT UNFORMATTED " Картотека счетов" SKIP.
      PUT UNFORMATTED 
         "┌────────────────────────────────────────┬────────────────────────┬───┬────────┬─┐" SKIP
         "│             НАЗВАНИЕ РОЛИ              │       НОМЕР СЧЕТА      │ВАЛ│  ДАТА  │К│" SKIP
         "├────────────────────────────────────────┼────────────────────────┼───┼────────┼─┤" SKIP.
              
   
      FOR EACH loan-acct WHERE loan-acct.contract  EQ bloan.contract 
                           AND loan-acct.cont-code EQ bloan.cont-code,
         FIRST acct WHERE acct.acct     EQ loan-acct.acct 
                      AND acct.currency EQ loan-acct.currency,
         FIRST code WHERE code.code  EQ loan-acct.acct-type 
                      AND code.class EQ "loan-acct" AND code.parent EQ "loan-dps"
      NO-LOCK:
   
      PUT UNFORMATTED 
         "│" code.name FORMAT "x(40)"    
         "│" loan-acct.acct FORMAT "x(24)"
         "│" loan-acct.currency FORMAT "x(3)"
         "│" loan-acct.since        
         "│" acct.acct-cat
         "│" SKIP.
   
      END.
   
   
      PUT UNFORMATTED
         "└────────────────────────────────────────┴────────────────────────┴───┴────────┴─┘" SKIP(2).

END.

{preview.i &col=170}
{intrface.del}

PROCEDURE Get_Par:
    DEFINE VAR in-kau       AS CHARACTER          NO-UNDO.
    DEFINE VAR in-k         AS CHARACTER EXTENT 2 NO-UNDO.
    DEFINE VAR l-acct       AS CHARACTER          NO-UNDO.

    DEFINE VAR vKind        AS CHARACTER          NO-UNDO.
    DEFINE VAR vTempl       AS INT64            NO-UNDO.
    DEFINE VAR vKindD       AS DATE               NO-UNDO.

    DEFINE VAR dat_start    AS DATE               NO-UNDO. /* Начало периода начисление процентов */
    DEFINE VAR cod_ost      AS CHARACTER          NO-UNDO. /* Код остатка */
    DEFINE VAR beg-dat      AS DATE               NO-UNDO.
    DEFINE VAR in-surrogate AS CHARACTER          NO-UNDO.
    DEFINE VAR i            AS INT64            NO-UNDO.

    DEFINE VAR str-kau      AS CHARACTER          NO-UNDO.
    DEFINE VAR comm         AS CHARACTER          NO-UNDO. /* Код основной комиссии */
    DEFINE VAR in-interest  AS CHARACTER          NO-UNDO. /* Схема нач. процентов */
    DEFINE VAR dat-t        AS DATE               NO-UNDO.
    DEFINE VAR summ%        LIKE acct-pos.balance NO-UNDO.

    DEFINE VAR str-acct     AS CHARACTER          NO-UNDO.
    DEFINE VAR date-contr   AS DATE               NO-UNDO.
    DEFINE VAR end-dat      AS DATE               NO-UNDO.
    DEFINE VAR nn           AS INT64            NO-UNDO.
    DEFINE VAR strnn        AS CHARACTER          NO-UNDO.

    def buffer yop-templ for op-templ .
    {justasec}

    RUN get-beg-date-prol in h_dpspc(RECID(bloan),end-date,
    OUTPUT dd1, OUTPUT dd2).

    RUN loan_param.        /* Расчета остатков по вкладу */
    IF dd2 <> ? /* предполагаем, что вклады с довложениями
                не бывают до востребования */    
    THEN            
    RUN current_persent.    /* Расчет текущих процентов по довложению */
    /*------------ определении даты начисления процентов ------------*/

    FIND LAST loan-cond WHERE
              loan-cond.contract  EQ bloan.contract
          AND loan-cond.cont-code EQ bloan.cont-code
          AND loan-cond.since     LE end-date
    NO-LOCK NO-ERROR.

    IF AVAIL loan-cond THEN DO:
       /*вычисляем СРАЗУ реальную дату начисления %%*/
       RUN DateOfCharge in h_dpspc (end-date, RECID(loan-cond), OUTPUT dat_start).
       IF dat_start NE ? THEN DO:
          /*если есть проблемы с выходными (в эту дату нельзя начислять) */
          IF NOT chk_date(recid(loan-cond), dat_start) THEN DO:
             REPEAT i = 0 TO 30:
                IF chk_date(RECID(loan-cond), dat_start + i ) THEN
                LEAVE.
                HIDE MESSAGE NO-PAUSE .
             END.
             dat_start = dat_start + i.
          END.
       END.
       IF dat_start NE ? THEN DO:
          CREATE dps_param.
          ASSIGN
             dps_param.acct = "???"
             dps_param.name = "Дата начисл. процентов"
          .
          IF bloan.end-date NE ? THEN
             dps_param.date1 = MINIMUM (dat_start, bloan.end-date).
          ELSE
             dps_param.date1 = dat_start.
       END.
    END.

    /*---------------------------------------------------------*/
   
    /* Guva проверяем тип вклада */
    IF dd2 EQ ? THEN
    ASSIGN
       in-k[1] = 'ОстВклВ'
       in-k[2] = 'НачПрВ'
    .
    ELSE
    ASSIGN
       in-k[1] = 'ОстВклС'
       in-k[2] = 'НачПрС1'
    .
                       
    cod_ost = bloan.contract + ',' + bloan.cont-code + ',' + in-k[2].
    RUN  get-beg-date-all in h_dpspc (RECID(bloan),end-date,OUTPUT dat_start).
    
    IF     dat_start NE ? 
       AND (   bloan.loan-status LT "ФДЗ"
            OR bloan.end-date    EQ ?
            OR mOstatok         NE 0)
       AND (    bloan.close-date EQ ?
            OR  bloan.close-date GT end-date)      
       THEN DO: /* Вычисляем текущие проценты */
       FIND LAST loan-acct OF bloan
          WHERE loan-acct.acct-type EQ (if ENTRY(3,cod_ost) EQ 'НачПрВ' THEN 'loan-dps-p'
                                                                        ELSE 'loan-dps-t')
           AND loan-acct.since      LE dat_start
       NO-LOCK NO-ERROR. /* Счет */

       IF NOT AVAIL loan-acct THEN 
         FIND FIRST loan-acct OF loan
          WHERE loan-acct.acct-type EQ (if ENTRY(3,cod_ost) EQ 'НачПрВ' THEN 'loan-dps-p'
                                                                        ELSE 'loan-dps-t')
           AND loan-acct.since      GT dat_start
       NO-LOCK NO-ERROR. /* Счет */
        IF NOT AVAIL loan-acct THEN
       RETURN.

       FIND acct WHERE acct.acct     EQ loan-acct.acct
                   AND acct.currency EQ loan-acct.currency
       NO-LOCK NO-ERROR.

       str-kau = bloan.contract +  ',' + bloan.cont-code + ',' + in-k[1].
       beg-date = dat_start.
       
       CREATE dps_param.
       ASSIGN
          dps_param.acct  = loan-acct.acct
          dps_param.curr  = loan-acct.currency
          d1 = beg-date
          d2 = end-date
       .

       RUN get_acct in h_dpspc(recid(bloan),
                                       beg-date,
                                       end-date,
                                       output str-acct).

       DO i = 1 TO NUM-ENTRIES(str-acct) BY 2:
          FIND FIRST acct
               WHERE acct.acct EQ ENTRY(i,str-acct)
          NO-LOCK NO-ERROR.
          IF NOT AVAIL acct THEN NEXT.
          IF i GT 1 THEN beg-date = IF beg-date LT DATE(ENTRY(i + 1,str-acct)) THEN DATE(ENTRY(i + 1,str-acct))
                                                                               ELSE beg-date.
          IF NUM-ENTRIES(str-acct) GE i + 3
          THEN date-contr = date(entry(i + 3,str-acct)).
          ELSE date-contr = end-date.

          RUN Get_Last_Inter in h_dpspc (RECID(bloan),beg-date,date-contr,OUTPUT in-interest).
          IF in-interest = ?  OR in-interest = '?' THEN DO:
             i = i + 1.
             NEXT.
          END.
          RUN Get_Last_Commi in h_dpspc (RECID(bloan),beg-date,date-contr,OUTPUT comm).
          IF comm = ?  OR comm = '?'  THEN DO :
             i = i + 1.
             NEXT.
          END.

          DO WHILE beg-date LT date-contr:
             result = 0.
             result1 = 0.
             {findsch.i &dir=first
                        &sch=in-interest
                        &since1=" GT beg-date AND interest-sch-line.since LE date-contr"}
             IF AVAIL interest-sch-line THEN dat-t = interest-sch-line.since.
                                        ELSE dat-t = date-contr.

             RELEASE interest-sch-line.
             {findsch.i &dir=last &sch=in-interest &since1 =" lt dat-t"}
             if avail interest-sch-line then
             RUN nachkin.p(recid(interest-sch-line),
                           comm,
                           recid(acct),
                           dat-t,
                           str-kau,
                           ?,
                           output result,
                           output result1,
                           input-output beg-date,
                           output flag)  .
             summ% = summ% + result .
             if flag ne 0 then return .
             beg-date = dat-t .
          END.
       END.
       IF AVAIL dps_param THEN
       ASSIGN
          dps_param.kau   = "No"
          dps_param.name  = "Текущие проценты"
          dps_param.blns  = summ%
          dps_param.date1 = d1
          dps_param.date2 = d2
       .
    END.

end procedure.

PROCEDURE list-kau.
  /*DEF INPUT PARAMETER rec AS RECID.*/
  DEF INPUT PARAMETER l-acct AS CHAR.
  DEF INPUT PARAMETER in-kau AS CHAR.
  DEF OUTPUT PARAMETER list-kau AS CHAR NO-UNDO.
  DEF VAR tmp-list AS CHAR NO-UNDO.
  DEF BUFFER b-loan-acct FOR loan-acct.
  DEF BUFFER b-kau FOR kau.
  tmp-list = "".
  RUN  list-kau-dv(in-kau,l-acct,OUTPUT list-kau ).
  IF list-kau <> '' THEN RETURN .
  FOR EACH b-loan-acct OF bloan WHERE b-loan-acct.acct-type EQ l-acct
                     AND b-loan-acct.since >= dat1
                     AND b-loan-acct.since <= end-date   NO-LOCK:
      FIND b-kau WHERE b-kau.acct = b-loan-acct.acct AND
                       b-kau.currency = b-loan-acct.currency AND
                       b-kau.kau = in-kau NO-LOCK NO-ERROR.
      IF AVAILABLE b-kau THEN DO:
        IF NOT CAN-DO(STRING(RECID(b-kau)), tmp-list) THEN DO:
           {additem.i tmp-list STRING(RECID(b-kau))}
        END.
      END.
  END.
  list-kau = REPLACE(tmp-list,",",";").
END PROCEDURE.

/* процедура для специфической обработки довложений  и получения списка счетов по довложениям */
PROCEDURE list-kau-dv.
   DEF INPUT PARAM in-kau AS CHAR NO-UNDO .
   DEF INPUT PARAM l-acct AS CHAR NO-UNDO .
   DEF OUTPUT PARAMETER list-kau AS CHAR NO-UNDO.

   DEF VAR tmp-list AS CHAR NO-UNDO.

   DEF BUFFER b-loan-acct FOR loan-acct.
   DEF BUFFER b-kau FOR kau.

   IF NOT CAN-DO({&str-type},l-acct) THEN RETURN .
   FOR EACH b-loan-acct OF bloan WHERE b-loan-acct.acct-type EQ l-acct
                     AND b-loan-acct.since >= dat1
                     and b-loan-acct.since <= end-date NO-LOCK,
      EACH b-kau WHERE b-kau.acct = b-loan-acct.acct AND
                       b-kau.currency = b-loan-acct.currency AND
                       b-kau.kau BEGINS ENTRY(1,in-kau) + ',' + ENTRY(2,in-kau)   NO-LOCK :
      IF ENTRY(4,b-kau.kau) = ENTRY(3,in-kau) THEN
         DO :  
         {additem.i tmp-list STRING(RECID(b-kau))}

      END. 
   END.
   list-kau = REPLACE(tmp-list,",",";").
END PROCEDURE .
/* Определение возможных остатков по вкладу */
PROCEDURE LOAN_PARAM.
    DEFINE VARIABLE tmp_summ     AS DECIMAL   NO-UNDO. /* Временная сумма для расчетов */
    DEFINE VARIABLE fin_summ     AS DECIMAL   NO-UNDO. /* Остаток по довложениям */
    DEFINE VARIABLE vLstTypeChar AS CHARACTER NO-UNDO. /*Список ролей счетов вкладов с довложением*/
    DEFINE VARIABLE vLstCodChar  AS CHARACTER NO-UNDO. /*Список кодов остатка*/
    DEFINE VARIABLE vLstNameChar AS CHARACTER NO-UNDO. /*Наименование остатков*/
    DEFINE VARIABLE vIndLst      AS INT64   NO-UNDO. /*Индекс по списку счетов,остатков,наименования*/
    DEFINE VARIABLE vStrKau      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vListKauChar AS CHARACTER NO-UNDO. /*Список RecId всех КАУ по счету*/
    vLstTypeChar = "loan-dps-p"   + "|" +
                   "loan-dps-p"   + "|" +
                   "loan-dps-t"   + "|" +
                   "loan-dps-t"   + "|" +
                   "loan-dps-int" + "|" +
                   "loan-dps-exc" + "|" +
                   "loan-dps-ts"  + "|" +
                   "loan-dps-ts"  + "|" +
                   "loan-dps-tsk" + "|" +
                   "loan-dps-tsk" + "|" +
                   "loan-dps-ink".
    vLstCodChar  = "ОстВклВ"      + "|" +
                   "НачПрВ"       + "|" +
                   "ОстВклС"      + "|" +
                   "НачПрС1"      + "|" +
                   "НачПр"        + "|" +
                   "ПрПр"         + "|" +
                   "ОстВклДВ"     + "|" +
                   ",НачПрС1"     + "|" +
                   "ОстВклДВ"     + "|" +
                   "НачПрС1"      + "|" +
                   "НачПр".
    vLstNameChar = "Остаток вклада до востребования"              + "|" +
                   "Начисленные проценты"                         + "|" +
                   "Остаток срочного вклада"                      + "|" +
                   "Начисленные проценты"                         + "|" +
                   "Предварительно начисленные проценты"          + "|" +
                   "Просроченные начисленные проценты"            + "|" +
                   "Остаток по довложениям"                       + "|" +
                   "Начисленные проценты"                         + "|" +
                   "Остаток по довложениям"                       + "|" +
                   "Начисленные проценты"                         + "|" +
                   "Предварительно начисленные проценты".
    mOstatok = 0.
    DO vIndLst = 1 TO NUM-ENTRIES(vLstTypeChar,"|"):
       fin_summ = 0.
       FIND LAST loan-acct OF bloan
          WHERE loan-acct.acct-type EQ ENTRY(vIndLst,vLstTypeChar,"|")
            AND loan-acct.since     LE end-date
       NO-LOCK NO-ERROR.
       IF NOT AVAIL loan-acct THEN NEXT.
       vStrKau = bloan.contract  + "," +
                 bloan.cont-code + "," +
                 ENTRY(vIndLst,vLstCodChar,"|").
       RUN list-kau (loan-acct.acct-type, vStrKau, OUTPUT vListKauChar).
       RUN kau-pos.p(loan-acct.acct,
                     loan-acct.currency,
                     end-date,
                     end-date,
                     gop-status,
                     vStrKau).
       fin_summ = IF loan-acct.currency  EQ "" THEN ksh-bal
                                               ELSE ksh-val.

       IF fin_summ = 0 THEN DO :
          RUN get-kauost-trans(bloan.contract,
                               bloan.cont-code,
                               "*",
                               loan-acct.acct-type,
                               "",
                               "",
                               ENTRY(vIndLst,vLstCodChar,"|"),
                               end-date,
                               end-date,
                               gop-status,
                               OUTPUT fin_summ
                              ).          
       END.
       IF fin_summ <> 0 THEN DO :
         CREATE dps_param.
         ASSIGN
           dps_param.acct = loan-acct.acct
           dps_param.curr = loan-acct.currency
           dps_param.kau  = IF vListKauChar NE ""
                              THEN vListKauChar
                              ELSE (bloan.contract + "," + 
                                    bloan.cont-code + "," + 
                                    loan-acct.acct-type + "," + 
                                    ENTRY(vIndLst,vLstCodChar,"|"))
           dps_param.name = ENTRY(vIndLst,vLstNameChar,"|")
           dps_param.blns = fin_summ
         .
       END.
       IF    ENTRY(vIndLst,vLstCodChar,"|") EQ "ОстВклС"
          OR ENTRY(vIndLst,vLstCodChar,"|") EQ "ОстВклДВ" THEN
       DO:
          mOstatok = fin_summ.
       END.   
    END.
END.
/* /Остаток по довложениям */

/* Начисленные проценты по довложению */
PROCEDURE CURRENT_PERSENT.
   DEFINE VAR refin_summ AS DECIMAL    NO-UNDO. /* Временная сумма для расчетов */
   DEFINE VAR fin_summ   AS DECIMAL    NO-UNDO. /* Остаток по довложениям */
   DEFINE VAR all_summ   AS DECIMAL    NO-UNDO. /* Остаток по довложениям */
   DEFINE VAR is_ok      AS LOGICAL    NO-UNDO. /* Признак выполнения расчета. */
   DEFINE VAR lnp_date   AS DATE       NO-UNDO. /* Дата первого предварительного
                                                ** начисления процентов по довл. */
   DEF VAR vFlNach        AS LOG       NO-UNDO. /* признак, было ли начисление */
   DEF VAR vTransOpenDate AS DATE      NO-UNDO. /* дата первого движения по довл. */
   DEF VAR vLnpD          AS DATE      NO-UNDO. /* Дата открытия первого довложения
                                                ** (если не было начисления) или
                                                ** дата первого предворительного
                                                ** начисления процентов по довл. */
   DEF VAR vFlagErr       AS INT64   NO-UNDO. /* Флаг ошибки при расчете процентов */
   
   IF        (bloan.loan-status GE "ФДЗ"
         AND  mOstatok         EQ 0) 
      OR (    bloan.close-date NE ?
         AND  bloan.close-date LE end-date) THEN
   RETURN. 
      
   FOR EACH loan-trans OF bloan 
      NO-LOCK:

      /* Определение первого движения */
      RUN get_first_kau_date(loan-trans.contract,
                             loan-trans.cont-code,
                             loan-trans.trans-code,
                             end-date,
                             gop-status,
                             OUTPUT vFlNach,
                             OUTPUT vTransOpenDate).

      /* Поиск даты последжнего начисления процентов */
      RUN get_beg_date_trans (loan-trans.contract,
                              loan-trans.cont-code,
                              loan-trans.trans-code,
                              end-date,
                              OUTPUT lnp_date).
      /* Если начисление будет в будущем - возьмем дату начала довложения */
      IF lnp_date EQ ? THEN 
         lnp_date = IF dd1 LT loan-trans.open-date THEN loan-trans.open-date
                                                   ELSE dd1.
      
      IF    lnp_date GT vTransOpenDate
         OR vLnpD    EQ ? THEN 
         vLnpD = lnp_date.

      /* Рассчет суммы предварительно начисленных процентов */
      RUN Calc_Interest_Full_Dovl IN h_dpspr (BUFFER bloan,
                                              lnp_date,
                                              IF end-date GT lnp_date THEN end-date 
                                                                      ELSE lnp_date,
                                              ?,
                                              NO,
                                              YES,
                                              loan-trans.trans-code,
                                              NO,                                                 
                                              OUTPUT fin_summ,
                                              OUTPUT refin_summ,
                                              OUTPUT vFlagErr).
      all_summ = all_summ + fin_summ.
   END.

   IF all_summ NE 0.00 THEN 
   DO:
      CREATE dps_param.
      ASSIGN
         dps_param.acct = ""
         dps_param.curr = ""
         dps_param.kau  = "No"
         dps_param.name = "Текущие проценты по довложениям"
         dps_param.blns =  all_summ
         dps_param.date1 = vLnpD 
         dps_param.date2 = IF end-date GT lnp_date  THEN end-date ELSE lnp_date
         .
   END.

END PROCEDURE.
procedure chek_dps.
    find first dps_param no-error.
    if not avail dps_param then do:
        message "На дату " + string(end-date,"99.99.9999") +
        " не определен ни один счет!"
        view-as alert-box buttons OK.
    end.
end procedure.
