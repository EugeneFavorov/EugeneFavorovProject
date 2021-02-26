/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ЗАО "Банковские информационные системы"
     Filename: cr-case.p
      Comment: Отчет "Кредитный портфель"
   Parameters:
         Uses:
      Used by:
      Created: Илюха
     Modified: Илюха
*/

{globals.i}
{sh-defs.i}     /* Необходимы для подсчета остатков на счете */
{tmprecid.def}  /* Исходные договоры */
{svarloan.def}
{wordwrap.def}
{loan_par.def
    &new = new} /* Параметры договора по которым рассчитываются проценты */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
                        /* intrface.get tmess должен быть перед  getpath.fun */    
{ getpath.fun }

{intrface.get comm}
{intrface.get instrum}
{intrface.get xclass}
{intrface.get loan}
{intrface.get i254}
{intrface.get bag}
{loan.pro}

def var mytempDate as date no-undo.
DEF VAR rub_cred AS CHAR INIT "1,2,3,3.1,3.2,4,5,6,7,8,9,9.1,9.2,10,11,12,13,14,15,15.1,15.2,16,17,18" NO-UNDO.
DEF VAR cur_cred AS CHAR INIT "19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36" NO-UNDO.
DEF VAR zad_cred AS CHAR INIT "37,38" NO-UNDO.
DEF VAR cred_16 AS CHAR INIT "1,2,3,3.1,3.2,4,5,6,7,8,9,9.1,9.2,10,11,12,13,14,15,15.1,15.2,16" NO-UNDO.
DEF VAR cred_18 AS CHAR INIT "17,18" NO-UNDO.
DEF VAR cred_34 AS CHAR INIT "19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34" NO-UNDO.

DEF VAR korp_cred AS CHAR INIT "1,2,3,3.1,3.2,4,5,6,19,20,21,22,23,24" NO-UNDO.
DEF VAR pred_cred AS CHAR INIT "7,8,9,9.1,9.2,10,11,12,25,26,27,28,29,30" NO-UNDO.
DEF VAR pers_cred AS CHAR INIT "13,14,15,15.1,15.2,16,17,18,31,32,33,34,35,36" NO-UNDO.

DEF VAR vCountInt    AS INT64 NO-UNDO. /* Счетчик */
DEF VAR vSelectLog   AS LOGICAL NO-UNDO. /* Тип ведомости */
DEF VAR vLnCountInt  AS INT64 NO-UNDO. /* Счетчик договоров */
DEF VAR vLnTotalInt  AS INT64 NO-UNDO. /* Общее количество договоров */
DEF VAR vZeroSkipLog AS LOGICAL NO-UNDO. /* Перечень параметров, нулевые
                                            ** значения которых не выводить */
/* для use'анья l-con(l).fnd */
DEF VAR di-sum   LIKE term-obl.amt INIT ? NO-UNDO.
DEF VAR di-sum2  LIKE term-obl.amt INIT ? NO-UNDO.
DEF VAR di-sum-t LIKE term-obl.amt INIT 0.
DEF VAR stav     LIKE comm-rate.rate-comm INIT 0.
DEF VAR c-since  LIKE loan-cond.since NO-UNDO.
DEF VAR xd       LIKE loan-cond.since NO-UNDO.
DEF VAR n         AS INT64 INIT 0.
DEF VAR i         AS INT64  NO-UNDO.
DEF VAR num-line  AS CHAR NO-UNDO.
DEF VAR cred-per  AS CHAR FORMAT "x(17)" .
DEF VAR int-per   AS CHAR FORMAT "x(17)" .
DEF VAR tels      AS CHAR EXTENT 6 FORMAT "x(17)"
  INITIAL ['Ежемесячн. по ','Ежекварт. по ','Дек.по 11,21,31/1','Произв',
          'Полугод','Год']                     NO-UNDO.
DEF VAR period_val AS CHAR INIT "П,М,К,Д,ПГ,Г" NO-UNDO.  /* Периоды погашения
                                                            /оплаты */
/*2 затычки для  вызова RE_PARAM*/
DEF VAR a1 AS DECIMAL NO-UNDO.
DEF VAR a2 AS DECIMAL NO-UNDO.

DEF VAR name-klient AS CHAR EXTENT 3 NO-UNDO.
DEF VAR summ-t AS DECIMAL NO-UNDO.

DEF VAR par_0  AS DECIMAL NO-UNDO.
DEF VAR par_13 AS DECIMAL NO-UNDO.
DEF VAR par_21 AS DECIMAL NO-UNDO.
DEF VAR par_46 AS DECIMAL NO-UNDO.

DEF VAR d1  AS DATE NO-UNDO.
DEF VAR dat AS DATE NO-UNDO.
DEF VAR zz  AS CHAR NO-UNDO.
DEF VAR zz1 AS CHAR NO-UNDO.

DEF VAR c_value   AS CHAR    NO-UNDO.
DEF VAR lines-per AS INT64 NO-UNDO INIT 1.
DEF var j         AS INT64 NO-UNDO.
DEF VAR t-var     AS DECIMAL NO-UNDO.
DEF VAR prn-lin   AS LOG     NO-UNDO.
DEF VAR mParSum   AS DEC     NO-UNDO.
DEF VAR mAccts    AS CHAR    NO-UNDO.

DEF STREAM rep.
DEF STREAM excel.

DEF TEMP-TABLE code-table
   FIELD CODE LIKE CODE.CODE
   FIELD NAME LIKE CODE.NAME
   FIELD c-code AS CHAR
   FIELD razdel AS INT64
   FIELD r-name AS CHAR
   FIELD g-code AS INT64
   FIELD g-name AS CHAR
   .

DEF TEMP-TABLE rep-table
   FIELD razdel       AS INT64
   FIELD r-name       AS CHAR
   FIELD r-type       AS CHAR
   FIELD cl-name      AS CHAR
   FIELD num          AS CHAR /*номер*/
   FIELD kod          AS CHAR /*код классификатора с добавленным первым нулем*/
   FIELD c-name       AS CHAR /*код класса*/
   FIELD c-code       AS CHAR /*название класса*/

   FIELD contract     LIKE loan.contract
   FIELD cont-code    LIKE loan.cont-code
   FIELD sub-grup     AS INT64

   FIELD cli-name     AS CHAR /*название клиента*/
   FIELD acct         AS CHAR /*счет*/
   FIELD cur          LIKE acct.currency /*валюта*/
   FIELD beg-date     LIKE loan.open-date /*дата открытия договора*/
   FIELD comment      AS CHAR  /*основание*/
   FIELD limit        LIKE loan-var.balance /*лимит кредитования*/
   FIELD end-date     LIKE loan.end-date /*срок кредита (дата окончания договора)*/
   /*график погашения  основного долга */
   FIELD date-proc    AS CHAR  /*дата погашения  (строка из дат через " " ) */
   FIELD summ-proc    AS CHAR  /*сумма погашения (строка из сумм через " " )  */
   /*изменения  в графике*/
   FIELD date-change   AS CHAR   /*дата начала   */
   FIELD summ-change   AS CHAR    /*сумма    */
   FIELD proc-change   AS CHAR  /*процент*/
   FIELD srok-change   AS CHAR  /*строк*/

   FIELD used-limit     LIKE loan-var.balance /* */
   FIELD payed-sum      LIKE loan-var.balance /* погашено основного долга*/
   FIELD dolg           LIKE loan-var.balance /*задолженность параметры 0+13*/
   FIELD dolg-main      LIKE loan-var.balance /*просроченная задолженность по долгу параметр 7 */
   FIELD dolg-proc      LIKE loan-var.balance /*просроченная задолженность по %%   параметр 10 */
   FIELD dolg-itog      LIKE loan-var.balance /*итого задолженность  параметры (0+13+7+10) */
   FIELD rate         LIKE comm-rate.rate-comm /* */
   FIELD RateName     AS CHAR
   FIELD rate1        LIKE comm-rate.rate-comm /* */

   FIELD payed          AS DECIMAL /*оплачено 1 - сумма*/
   FIELD vid-obesp      AS CHAR    /*вид обеспечения с разбивкий*/
   FIELD gr-riska       LIKE loan.gr-riska    /*группа риска*/
   FIELD res-fact-summ  LIKE loan-var.balance /*резерв фак. на отчетную дату сумма*/
   FIELD res-fact-proc  AS DECIMAL            /*резерв фак. на отчетную дату %*/
   FIELD reserv-rasch   LIKE loan-var.balance /*расчетный резерв*/
   FIELD destination    AS CHAR               /*назначение кредита*/
   FIELD eps            AS DECIMAL /* ЭПС */
    .

/* в связи с глюкавостью отчета и подведением итогов все итоговые
   суммы будут лежать в ентой TT*/
DEF TEMP-TABLE ttItog NO-UNDO
  FIELD iType      AS CHAR /*группа по которой идут итоги*/
  FIELD Curr       AS CHAR /*валюта*/
  FIELD iLimit     AS DEC INIT 0 /*лимит кредита*/
  FIELD iUsedLimit AS DEC INIT 0  /*использовано отлимита*/
  FIELD iPayedDolg AS DEC INIT 0  /*погашено основного долга*/
  FIELD iSrochDolg AS DEC INIT 0  /*срочная задолж по ссуде*/
  FIELD iPrDolg    AS DEC INIT 0  /*просроч. задолж. по ссуде*/
  FIELD iPrProc    AS DEC INIT 0  /*...              по %   */
  FIELD iDolg      AS DEC INIT 0  /*весь долг*/
  FIELD iPayedComm AS DEC INIT 0  /*оплачено*/
  FIELD iFactRes   AS DEC INIT 0  /*фактич. резерв*/
  FIELD iRaschRes  AS DEC INIT 0  /*расчетн. резерв*/
.

DEF BUFFER loan-cond-buf FOR loan-cond.
DEF BUFFER bLoan-acct    FOR loan-acct.
DEF BUFFER acct1         FOR acct.
DEF BUFFER bLoan         FOR loan.

{cr-case.i}

IF NOT CAN-FIND (FIRST tmprecid) THEN
DO:
   RUN fill-sysmes IN h_tmess ("","","0","Нет ни одного выбранного договора!").
   RETURN.
END.

/* {getdate.i} */
run getdate.p("Выгрузки портфеля ", OUTPUT end-date).
dat = end-date.

/*создаем таблицу классификаторов*/
FOR EACH code WHERE
         code.class = "Груп_дог"
NO-LOCK:

   CREATE code-table.
   ASSIGN
      code-table.code = code.code
      code-table.name = code.val
      zz  = IF NUM-ENTRIES(code.code, ".") = 2 THEN
               ENTRY(1,code.code,".")
            ELSE
               code.code
      zz1 = IF NUM-ENTRIES(code.code, ".") = 2 THEN
               ENTRY(2,code.code,".")
            ELSE
               ""
      .

   IF LENGTH(zz) = 1 THEN
      zz = "0" + zz.
   code-table.c-code = TRIM(zz + IF zz1 <> "" THEN "." + zz1 ELSE "").

   IF CAN-DO(rub_cred,TRIM(code.code)) THEN
      ASSIGN
         code-table.razdel = 1
         code-table.r-name = "Кредиты в рублях"
         .
   ELSE
   IF CAN-DO(cur_cred,TRIM(code.code)) THEN
      ASSIGN
         code-table.razdel = 2
         code-table.r-name = "Кредиты в иностранной валюте (в руб. оценке)"
         .
   ELSE
   IF CAN-DO(zad_cred,TRIM(code.code)) THEN
      ASSIGN
         code-table.razdel = 3
         code-table.r-name = "Задолженность, классифицируемая как ссудная "
         .

   IF CAN-DO(korp_cred,TRIM(code.code)) THEN
      ASSIGN
         code-table.g-code = 1
         code-table.g-name = "Кредиты корпоративной клиентуре"
         .
   ELSE
   IF CAN-DO(pred_cred,TRIM(code.code)) THEN
      ASSIGN
         code-table.g-code = 2
         code-table.g-name = "Кредиты индивидуальным предпринимателям без образования юридического лица"
         .
   ELSE
   IF CAN-DO(pers_cred,TRIM(code.code)) THEN
      ASSIGN
         code-table.g-code = 3
         code-table.g-name = "Кредиты физическим лицам"
        .
   ELSE
      ASSIGN
         code-table.g-code = 4
         code-table.g-name = ""
         .
END.

/*----------- блок обработки договоров -------------------*/
{init-bar.i "Обработка договоров"}

/* Подсчет общего числа договоров */
FOR EACH tmprecid:
    vLnTotalInt = vLnTotalInt + 1.
END.

FOR EACH tmprecid,
FIRST loan WHERE
    RECID(loan) eq tmprecid.id
NO-LOCK:

   /* Бегущая строка - индикатор работы процесса */
   { move-bar.i
        vLnCountInt
        vLnTotalInt
   }

   /* договорчики с пробелами пропускаем 
   IF INDEX(loan.cont-code," ") <> 0 THEN
      NEXT.
*/
   c_value = GetXattrValueEx("loan",
                             loan.contract + "," + loan.cont-code,
                             "Груп_дог",
                             "").

/*   IF TRIM(c_value) = ""  THEN
      NEXT. */

   IF GetCode("Груп_дог", c_value) = ? THEN
      c_value = "".
   ASSIGN
      zz  = IF NUM-ENTRIES(c_value, ".") = 2 THEN
               ENTRY(1,c_value,".")
            ELSE
               c_value
      zz1 = IF NUM-ENTRIES(c_value, ".") = 2 THEN
               ENTRY(2,c_value,".")
            ELSE
               ""
      .

   IF LENGTH(zz) = 1 THEN zz = "0" + zz.

   ASSIGN
      lr-st   = IF loan.contract = "Депоз" THEN
                   ({&lrate-dim} / 2) + 1
                ELSE
                   {&lr-st}
      lr-ed   = IF loan.contract = "Депоз"THEN
                   {&lrate-dim}
                ELSE
                   {&lrate-dim} / 2
      tip-dog = IF loan.contract = "Депоз"  THEN
                   "ТипДогП"
                ELSE
                   "ТипДогА"
      .

   CREATE rep-table.
   ASSIGN
      rep-table.beg-date  = loan.open-date /*начало договора*/
      rep-table.end-date  = loan.end-date  /*окончание договора*/
      rep-table.gr-riska  = re_history_risk(loan.contract,loan.cont-code,dat,loan.gr-riska)  /*группа риска*/
      rep-table.kod       = zz + IF zz1 <> "" THEN
                                    "." + zz1
                                 ELSE
                                    "" /*код классификатора
                                         с лидирующем нулем , для сортировки */
      rep-table.c-name    = GetCode("Груп_дог", c_value)
      rep-table.c-code    = c_value
      rep-table.contract  = loan.contract
      rep-table.cont-code = loan.cont-code
      rep-table.sub-grup  = 1
      .

   /*наименование клиента*/
   RUN GetCustName IN h_base(loan.cust-cat,
                             loan.cust-id,
                             ?,
                             OUTPUT name-klient[1],
                             OUTPUT name-klient[2],
                             INPUT-OUTPUT name-klient[3]).

   rep-table.cli-name = name-klient[1] + " " + name-klient[2].

   RUN GET_ALL_ACCTS(loan.contract,
                     loan.cont-code,
                     dat,
                     OUTPUT Rep-Table.Acct,
                     OUTPUT Rep-Table.Cur).

   DEF VAR tmp-rate AS DECIMAL INITIAL 1 NO-UNDO.

      /*график погашения*/
   FOR EACH term-obl WHERE
            term-obl.contract  = loan.contract
        AND term-obl.cont-code = loan.cont-code
        AND term-obl.idnt      = 3
   NO-LOCK:
/*      RUN summ-t.p (OUTPUT summ-t,
                    loan.contract,
                    loan.cont-code,
                    RECID(term-obl),
                    loan.since).
*/
	  if term-obl.amt-rub <> 0 then do:
	  mytempDate = term-obl.dsc-beg-date.
	  if (mytempDate= ?) then mytempDate = term-obl.end-date.
      {additem2.i date-proc STRING(mytempDate,'99.99.9999') "│"}
      {additem2.i summ-proc STRING(term-obl.amt-rub) "│"}
	  end.
   END.

   /*изменения в графике*/
   j = 1.
   FOR EACH loan-cond WHERE
            loan-cond.contract  = loan.contract
        AND loan-cond.cont-code = loan.cont-code
        AND loan-cond.since     > loan.open-date
   NO-LOCK:
      RUN RE_L_COND_FRST IN h_Loan (loan.contract,
                                    loan.cont-code,
                                    loan-cond.since + 1,
                                    BUFFER loan-cond-buf).

      d1 = IF AVAILABLE loan-cond-buf THEN
              loan-cond-buf.since - 1
           ELSE
           IF loan-cond.since <= loan.end-date THEN
              loan.end-date
           ELSE
              ?.
      {l-con(l)_o.fnd
         &ofch = "/*" }

      {additem2.i date-change STRING(loan-cond.since,'99.99.9999') "│"}
      {additem2.i summ-change STRING(di-sum) "│"}
      {additem2.i proc-change STRING(inrate[lr-st]) "│"}
      {additem2.i srok-change STRING(d1,'99.99.9999') "│"}
   END.

   IF GetXattrValueEx("loan",
                      loan.contract + ',' + loan.cont-code,
                      "Дог_основ",
                      "") <> ""
   THEN
      /*основание*/
      rep-table.comment = GetXattrValueEx("loan",
                                          loan.contract + ',' + loan.cont-code,
                                          "Дог_основ",
                                          "").
   rep-table.comment = REPLACE(rep-table.comment,"~n"," ").

   /*лимит кредита  = сумма первого условия*/
   RUN RE_L_COND_FRST IN h_Loan (loan.contract,
                                 loan.cont-code,
                                 loan.open-date,
                                 BUFFER loan-cond).

   IF AVAILABLE loan-cond THEN
   DO:
      {l-con(l)_o.fnd
         &ofch = "/*" }
      rep-table.limit = di-sum.
   END.

   mAccts = "".
/*
   /*использовано от лимита*/
   RUN CALC_USED_LIMIT(loan.contract,
                       loan.cont-code,
                       dat,
                       YES,
                       INPUT-OUTPUT mAccts,
                       OUTPUT rep-table.used-limit).

   /*погашено основного долга*/
   RUN CALC_PAYED_DOLG(loan.contract,
                       loan.cont-code,
                       dat,
                       YES,
                       OUTPUT rep-table.payed-sum).

   /*срочная задолженность*/
   RUN RE_PARAM IN h_Loan
              (0, /*код параметра*/
               dat, /*дата*/
               loan.contract, /*назначение договора*/
               loan.cont-code, /*код договора*/
               OUTPUT par_0, /*значение параметра*/
               OUTPUT a1,
               OUTPUT a2).
   RUN RE_PARAM IN h_Loan
              (13, /*код параметра*/
               dat, /*дата*/
               loan.contract, /*назначение договора*/
               loan.cont-code, /*код договора*/
               OUTPUT par_13, /*значение параметра*/
               OUTPUT a1,
               OUTPUT a2).
   rep-table.dolg = par_0 + par_13.
   /* ... а теперь по всем течениям */
   FOR EACH bLoan WHERE
            bLoan.Contract  = loan.contract
        AND bLoan.Cont-Code BEGINS loan.Cont-Code + " "
        AND bLoan.Cont-Code <> loan.cont-code
   NO-LOCK:
      RUN RE_PARAM IN h_Loan
                 (0, /*код параметра*/
                  dat, /*дата*/
                  bLoan.contract, /*назначение договора*/
                  bLoan.cont-code, /*код договора*/
                  OUTPUT par_0, /*значение параметра*/
                  OUTPUT a1,
                  OUTPUT a2).
      RUN RE_PARAM IN h_Loan
                 (13, /*код параметра*/
                  dat, /*дата*/
                  bLoan.contract, /*назначение договора*/
                  bLoan.cont-code, /*код договора*/
                  OUTPUT par_13, /*значение параметра*/
                  OUTPUT a1,
                  OUTPUT a2).
      rep-table.dolg = rep-table.dolg + par_0 + par_13.
   END.

   /*просроченная задолженность по основному долгу*/
   RUN RE_PARAM IN h_Loan
             (7, /*код параметра*/
              dat, /*дата*/
              loan.contract, /*назначение договора*/
              loan.cont-code, /*код договора*/
              OUTPUT rep-table.dolg-main, /*значение параметра*/
              OUTPUT a1,
              OUTPUT a2).
   /* ... а теперь по всем течениям */
   FOR EACH bLoan WHERE
            bLoan.Contract  = loan.contract
        AND bLoan.Cont-Code BEGINS loan.Cont-Code + " "
        AND bLoan.Cont-Code <> loan.cont-code
   NO-LOCK:
      RUN RE_PARAM IN h_Loan
                 (7,  /*код параметра*/
                  dat, /*дата*/
                  bLoan.contract,  /*назначение договора*/
                  bLoan.cont-code, /*код договора*/
                  OUTPUT mParSum,  /*значение параметра*/
                  OUTPUT a1,
                  OUTPUT a2).

      rep-table.dolg-main = rep-table.dolg-main + mParSum.
   END.
  /*просроченная задолженность по процентам
    ... и здесь эта байда без течений*/
  RUN RE_PARAM IN h_Loan
             (10, /*код параметра*/
              dat, /*дата*/
              loan.contract, /*назначение договора*/
              loan.cont-code, /*код договора*/
              OUTPUT rep-table.dolg-proc, /*значение параметра*/
              OUTPUT a1,
              OUTPUT a2).
*/
   /*итого задолженность*/
   rep-table.dolg-itog = rep-table.dolg + rep-table.dolg-main + rep-table.dolg-proc.
/*

   /* % комиссии */
   rep-table.rate1 = GET_COMM("%КрКом",
                             ?,
                             loan.currency,
                             loan.contract + "," + loan.cont-code,
                             0.00,
                             0,
                             dat).

   IF rep-table.rate1 = ? THEN
      rep-table.rate1 = 0.

   Rep-Table.RateName = "%КрКом".

   /* % ставка */
   rep-table.rate = GET_COMM("%Кред",
                             ?,
                             loan.currency,
                             loan.contract + "," + loan.cont-code,
                             0.00,
                             0,
                             loan.open-date).
   IF rep-table.rate = ? THEN
      rep-table.rate = 0.

   /* погашено % комиссий */
   FOR EACH loan-int  of loan WHERE
           (loan-int.id-d = 5
        AND loan-int.id-k = 6
        AND loan-int.mdate <= dat )
        OR (loan-int.id-d = 5
        AND loan-int.id-k = 26
        AND loan-int.mdate <= dat )
   NO-LOCK:
      rep-table.payed = rep-table.payed + loan-int.amt-rub.
   END.

   /* obespechenie */
   DEF variable acct-name AS CHAR EXTENT 3 NO-UNDO.

   RELEASE loan-acct.
   RELEASE bLoan-acct.
   RELEASE acct.
   RELEASE acct1.

   FOR EACH loan-acct WHERE
            loan-acct.contract  = loan.contract
        AND loan-acct.cont-code = loan.cont-code
        AND CAN-DO("КредОб*,КредГар*,КредЦБум*",loan-acct.acct-type)
        AND loan-acct.since    <= dat
      NO-LOCK:

      FIND FIRST acct WHERE
                 acct.acct     = loan-acct.acct
             AND acct.currency = loan-acct.currency
         NO-LOCK NO-ERROR.

      IF AVAIL acct THEN
      DO:

         RUN GetCust IN h_base (BUFFER acct,
                                NO,
                                NO,
                                OUTPUT acct-name[1],
                                OUTPUT acct-name[2],
                                OUTPUT acct-name[3] ).

         acct-name[1] = IF acct.details <> "" AND
                           acct.details <> ?
                        THEN
                           REPLACE(acct.details,"~n"," ")
                        ELSE
                           TRIM(TRIM(acct-name[1]) + " " + TRIM(acct-name[2])).

         tmp-rate  = FindRateSimple("Учетный",Acct.Currency,gend-date).
         /* в acct-name[1] - название счета , в данном случае
           это будет название залога */
         /* остаток на счете */
         RUN acct-pos IN h_base (acct.acct,
                                 acct.currency,
                                 gend-date,
                                 gend-date,
                                 ?).
         /* tmp-rate - учетный курс, определен выше */
         rep-table.vid-obesp = rep-table.vid-obesp +
             acct-name[1] + "; " +
             (IF acct.currency = "" THEN
                 TRIM(STRING(ABS(sh-bal), ">>>>>>>>>>>>>>>>>9.99"))
              ELSE
                 TRIM(STRING(ABS(sh-val * tmp-rate),">>>>>>>>>>>>>>>>>9.99"))) + "~n".
      END.
        
   END.

   /* Получим процент резервирования */
   rep-table.res-fact-proc = LnRsrvRate(loan.contract,loan.cont-code,dat).

   tmp-rate  = FindRateSimple("Учетный",Rep-Table.Cur,gend-date).
   /* Фактический резерв */
   RUN RE_PARAM IN h_Loan
              (21,             /*код параметра*/
               dat,            /*дата*/
               loan.contract,  /*назначение договора*/
               loan.cont-code, /*код договора*/
               OUTPUT par_21,  /*значение параметра*/
               OUTPUT a1,
               OUTPUT a2).
   RUN RE_PARAM IN h_Loan
              (46,             /*код параметра*/
               dat,            /*дата*/
               loan.contract,  /*назначение договора*/
               loan.cont-code, /*код договора*/
               OUTPUT par_46,  /*значение параметра*/
               OUTPUT a1,
               OUTPUT a2).

   rep-table.res-fact-summ = ABS(par_21 + par_46).
   /* ... а теперь по всем течениям */
   FOR EACH bLoan WHERE
            bLoan.Contract  = loan.contract
        AND bLoan.Cont-Code BEGINS loan.Cont-Code + " "
        AND bLoan.Cont-Code <> loan.cont-code
   NO-LOCK:
      RUN RE_PARAM IN h_Loan
                 (21,  /*код параметра*/
                  dat, /*дата*/
                  bLoan.contract,  /*назначение договора*/
                  bLoan.cont-code, /*код договора*/
                  OUTPUT Par_21,  /*значение параметра*/
                  OUTPUT a1,
                  OUTPUT a2).
      RUN RE_PARAM IN h_Loan
                 (46,  /*код параметра*/
                  dat, /*дата*/
                  bLoan.contract,  /*назначение договора*/
                  bLoan.cont-code, /*код договора*/
                  OUTPUT Par_46,  /*значение параметра*/
                  OUTPUT a1,
                  OUTPUT a2).
       rep-table.res-fact-summ = rep-table.res-fact-summ +
                                 ABS(par_21 + par_46).

   END.

   ASSIGN
  /*    rep-table.res-fact-summ = rep-table.res-fact-summ
                                * rep-table.res-fact-proc  (По заявке 10582) */
      /*расчетный резерв*/
      rep-table.reserv-rasch = (rep-table.dolg +
                                rep-table.dolg-main) *
                               rep-table.res-fact-proc * tmp-rate / 100
      /*объект кредитования*/
      c_value = GetXattrValueEx("loan",
                                loan.contract + ',' + loan.cont-code,
                                "ЦельКред",
                                "")
      .

   IF c_value <> ""  AND GetCodeName("ЦельКред", c_value) <> ? THEN
      rep-table.destination = GetCodeName("ЦельКред", c_value).
      
   /* ЭПС */
   rep-table.eps = GetEpsLoan(loan.contract,loan.cont-code,loan.since) * 100.

   vLnCountInt = vLnCountInt + 1.
*/
END. /*for each*/

{del-bar.i}

/*RUN PrintInExcelFormat.

{setdest2.i
   &cols   = 249
   &stream = " stream rep "
}

RUN PrnPart1.

PAGE STREAM rep.

RUN PrnPart2.


{preview.i
   &cols   = 249
   &stream = " stream rep " 
}
*/

{vyg2.i}


