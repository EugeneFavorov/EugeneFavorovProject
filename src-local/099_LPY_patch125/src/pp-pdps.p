/*
               Банковская интегрированная система БИСквит
    Copyright: (c) 1992-2017 ЗАО "Банковские информационные системы"
     Filename: PP-PDPS.P
      Comment: Библиотека функций парсера для Частных вкладов
   Parameters: none
         Uses:
      Used by:
     Modified: 07.12.2004       MIOA     (0035880) Добавлена функция
                                         ПРОВЕРИТЬ_МАКСОСТ
     Modified: 07.12.2004       MIOA     (0035880) Добавлена функция
                                         ПРОВЕРИТЬ_СУММАКРАТН

*/

{globals.i}
{ksh-defs.i new}
{dpsproc.def}
{intrface.get kau}
{intrface.get ltran}
{intrface.get tmess}
{intrface.get pbase}

/*****************************************************************************/
{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PDPS"
   &LIBNAME       = "Библиотека функций парсера для Частных вкладов"
   &DESCRIPTION   = "Содержит функции парсера для Частных вкладов: проверки сумм, остатков, и др."
   }

/*****************************************************************************/
{pfuncdef
   &NAME          = "ПРОВЕРИТЬ_МИНОСТ"
   &DESCRIPTION   = "Проверяет, станет ли остаток вклада меньше минимального остатка, если со счета списать указанную сумму
                     (если процедура вернула 0.00 - это значит ЛИБО, что при изъятии суммы на вкладе останется 
                     ровно минимальный остаток, ЛИБО на вкладе не указан реквизит МинОст)"
   &PARAMETERS    = "НАЗНАЧЕНИЕ ДОГОВОРА, НОМЕР ДОГОВОРА, СЧЕТ, ВАЛЮТА СЧЕТА, СУММА, ДАТА ИЗЪЯТИЯ СУММЫ"
   &RESULT        = "СУММА, КОТОРОЙ НЕ ХВАТИТ ДО МИНИМАЛЬНОГО ОСТАТКА"
   &SAMPLE        = "ПРОВЕРИТЬ_МИНОСТ('dps','42301/1',42301810500020000053,'',100000000.00, 12/12/2004) = 500.000~~n~
Если 12/12/2004 изъять со вклада 100000000.00 (в валюте вклада) - на вкладе остается сумма, ~
которая меньше минимально допустимого остатка на  500.00~~n~n~
ПРОВЕРИТЬ_МИНОСТ('dps','42302/10',42302840500020000053,'840',100000000.00, 27/12/2004) = -1000.00~~n~
Если 27/12/2004 изъять со вклада 100000000.00 (в валюте вклада) - остаток на вкладе не будет меньше минимально допустимого~
(и можно изъять еще 1000.00, не нарушая договора вклада)"
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iAcct     AS CHAR NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-acct FOR loan-acct.
   DEFINE VAR vMinOst  AS DECIMAL NO-UNDO.
   DEFINE VAR vBalance AS DECIMAL NO-UNDO.
   DEFINE VAR vKau     AS CHAR    NO-UNDO.
   
   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     out_Result = "Ошибка при определении вклада".
     is-ok = -1.
     RETURN.
   END.
   

   
   RUN get_last_min_ost IN h_dpspc(RECID(b-loan),
                                   iOpDate,
                                   iOpDate,
                                   OUTPUT out_Result).
   /* Если максимальный остаток нигде не указан - будем считать, что превышение равно 0 */
   IF out_Result = ? OR out_Result = "" OR  out_Result = "?" THEN DO:
      out_Result = "0.00".
      RETURN.
   END.
   
   vMinOst = DEC(out_Result) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      out_Result = "0.00".
      is-ok = -1.
      RETURN.
   END.

   FIND LAST b-loan-acct OF b-loan 
                          WHERE b-loan-acct.acct      =  iAcct
                            AND b-loan-acct.currency  =  iCurrency
                            AND b-loan-acct.since     <= iOpDate
                            AND (b-loan-acct.acct-type = "loan-dps-p" OR 
                                 b-loan-acct.acct-type = "loan-dps-t")
                          NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan-acct THEN DO:
      RUN Fill-SysMes IN h_tmess("","","-1","Счет [" + iAcct + "] в валюте [" + iCurrency + 
                                 "] не принадлежит вкладу [" + iContract + "][" + iContCode + "]").
      is-ok = -1. 
      RETURN.
  END.
   
   CASE b-loan-acct.acct-type:
     WHEN "loan-dps-p" THEN DO:
       vKau = "ОстВклВ".
     END.
     WHEN "loan-dps-t" THEN DO:
       vKau = "ОстВклС".
     END.
     OTHERWISE DO:
        RUN Fill-SysMes IN h_tmess("","","-1","Счет [" + iAcct + "] в валюте [" + iCurrency + 
                                   "] привязан ко вкладу с ролью [" + b-loan-acct.acct-type + 
                                   "]. Такая роль не поддерживается данной функцией.").
        is-ok = -1.
        RETURN.
     END.
   END.
   
   RUN kau-pos IN h_kau(iAcct, iCurrency, iOpDate, iOpDate,
                        "Ф", iContract + ","  + iContCode + "," + vKau).
   vBalance = -1 * (IF iCurrency = "" THEN ksh-bal ELSE ksh-val).
   
   out_Result = STRING(vBalance - vMinOst - iSumm).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "ПРОВЕРИТЬ_МАКСОСТ"
   &DESCRIPTION   = "Проверяет, будет ли превышен максимальный остаток частного вклада, если к нему добавить указанную сумму"
   &PARAMETERS    = "НАЗНАЧЕНИЕ ДОГОВОРА, НОМЕР ДОГОВОРА, СЧЕТ, ВАЛЮТА СЧЕТА, СУММА, ДАТА ВНЕСЕНИЯ СУММЫ"
   &RESULT        = "СУММА ПРЕВЫШЕНИЯ"
   &SAMPLE        = "ПРОВЕРИТЬ_МАКСОСТ('dps','42301/1',42301810500020000053,'',100000000.00, 12/12/2004) = 500.000~~n~
Если 12/12/2004 положить на вклад 100000000.00 (в валюте вклада) - максимальная сумма вклада будет превышена на 500.00~~n~n~
ПРОВЕРИТЬ_МАКСОСТ('dps','42302/10',42302840500020000053,'840',100000000.00, 27/12/2004) = -1000.00~~n~
Если 27/12/2004 положить на вклад 100000000.00 (в валюте вклада) - максимальная сумма вклада не будет превышена ~
(и до превышения можно внести еще 1000.00)"
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iAcct     AS CHAR NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-acct FOR loan-acct.
   DEFINE VAR vMaxOst  AS DECIMAL NO-UNDO.
   DEFINE VAR vBalance AS DECIMAL NO-UNDO.
   DEFINE VAR vKau     AS CHAR    NO-UNDO.
   
   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     out_Result = "Ошибка при определении вклада".
     is-ok = -1.
     RETURN.
   END.
   

   
   RUN get_last_max_ost IN h_dpspc(RECID(b-loan),OUTPUT out_Result).
   /* Если максимальный остаток нигде не указан - будем считать, что превышение равно 0 */
   IF out_Result = ? OR out_Result = "" OR  out_Result = "?" THEN DO:
      out_Result = "0.00".
      RETURN.
   END.
   
   vMaxOst = DEC(out_Result) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      out_Result = "0.00".
      is-ok = -1.
      RETURN.
   END.

   FIND LAST b-loan-acct OF b-loan 
                          WHERE b-loan-acct.acct     =  iAcct
                            AND b-loan-acct.currency =  iCurrency
                            AND b-loan-acct.since    <= iOpDate
                            AND (b-loan-acct.acct-type = "loan-dps-p" OR 
                                 b-loan-acct.acct-type = "loan-dps-t")
                          NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan-acct THEN DO:
      RUN Fill-SysMes IN h_tmess("","","-1","Счет [" + iAcct + "] в валюте [" + iCurrency + 
                                 "] не принадлежит вкладу [" + iContract + "][" + iContCode + "]").
      is-ok = -1. 
      RETURN.
  END.
   
   CASE b-loan-acct.acct-type:
     WHEN "loan-dps-p" THEN DO:
       vKau = "ОстВклВ".
     END.
     WHEN "loan-dps-t" THEN DO:
       vKau = "ОстВклС".
     END.
     OTHERWISE DO:
        RUN Fill-SysMes IN h_tmess("","","-1","Счет [" + iAcct + "] в валюте [" + iCurrency + 
                                   "] привязан ко вкладу с ролью [" + b-loan-acct.acct-type + 
                                   "]. Такая роль не поддерживается данной функцией.").
        is-ok = -1.
        RETURN.
     END.
   END.
   
   RUN kau-pos IN h_kau(iAcct, iCurrency, iOpDate, iOpDate,
                        "Ф", iContract + ","  + iContCode + "," + vKau).
   vBalance = -1 * (IF iCurrency = "" THEN ksh-bal ELSE ksh-val).
   
   out_Result = STRING(vBalance + iSumm - vMaxOst).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "ПРОВЕРИТЬ_МИНСУММА"
   &DESCRIPTION   = "Проверяет, достаточна ли сумма взноса (если есть ограничение параметром МинСумма)"
   &PARAMETERS    = "НАЗНАЧЕНИЕ ДОГОВОРА, НОМЕР ДОГОВОРА, ДАТА ВНЕСЕНИЯ СУММЫ, СУММА"
   &RESULT        = ""
   &SAMPLE        = ""
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO INIT "Нет".
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan FOR loan.
   DEFINE VAR vMinSumm  AS DEC NO-UNDO.

   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     RUN Fill-SysMes IN h_tmess("","","-1","Ошибка при определении вклада [" + iContract + "][" + iContCode + "]").
     is-ok = -1.
     RETURN.
   END.
   
   RUN Get_Last_Param IN h_dpspc(RECID(b-loan), iOpDate, iOpDate,'МинСумма',OUTPUT out_Result).

   IF out_Result = ? OR out_Result = "" OR  out_Result = "?" THEN DO:
      out_Result = "0.00".
      RETURN.
   END.

   out_Result = STRING(DEC(out_Result) - iSumm).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "ПРОВЕРИТЬ_СУММАКРАТН"
   &DESCRIPTION   = "Проверяет, кратна ли сумма, которую мы пытаемся внести на вклад (или списать), ~
допустимой сумме кратности пополнения/списания, указанной на вкладе. Если сумма кратна - возвращает ~
пустую строку. Если сумма некратна - возвращает текст ошибки. Паранетр ФЛАГ СПИСАНИЯ равен Да, если создается документ ~
списания, и Нет - если создается документ довнесения."
   &PARAMETERS    = "НАЗНАЧЕНИЕ ДОГОВОРА, НОМЕР ДОГОВОРА, КОД ДОКУМЕНТА, ДАТА ВНЕСЕНИЯ/ИЗЪЯТИЯ СУММЫ, СУММА, ФЛАГ СПИСАНИЯ"
   &RESULT        = "ТЕКСТ ОШИБКИ"
   &SAMPLE        = "ПРОВЕРИТЬ_СУММАКРАТН('dps','42301/1',3844095, 12/12/2004,1000.00,Нет) = 'Сумма должна быть кратна 300'~~n~
Это означает, что в настройке СуммаКратнП указано значение 300 и сумма документа должна быть кратна 300 ~
(что в данном случае неверно) ~~n~n~
ПРОВЕРИТЬ_СУММАКРАТН('dps','42301/1',3844095, 12/12/2004,3000.00,Нет) = ''~~n~
Вклад тот же - здесь сумма для внесения подходящая (кратна 300)"
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iOp       AS INT64  NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iFlag     AS LOG  NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-acct FOR loan-acct.
   
   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     out_Result = "Ошибка при определении вклада".
     is-ok = -1.
     RETURN.
   END.
   
   out_Result = check_mod_amt(RECID(b-loan), iOp, iOpDate, iSumm, iFlag).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "ПРОВЕРИТЬ_ОПОТМЫВ"
   &DESCRIPTION   = "Проверка необходимости контроля суммы вклада в классификаторе ОпОтмыв (по параметрам 9ВАЛ). ~
ВАЛ - код валюты. 810, 840 и т.д. Если параметра для переданной валюты не нашлось - считаем, что контроль не требуется."
   &PARAMETERS    = "СУММА ВЗНОСА, ВАЛЮТА ВЗНОСА"
   &RESULT        = "ТРЕБУЕТСЯ СОХРАНЕНИЕ РЕКВИЗИТОВ ВНОСИТЕЛЯ"
   &SAMPLE        = "ПРОВЕРИТЬ_ОПОТМЫВ(70000.00, '') = ДА~~n~
- Необходимо проверить, есть ли в системе клиент, проводящий взнос."
   }

   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO INIT "Нет".
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-code FOR code.
   DEFINE VAR vSumm AS DECIMAL NO-UNDO INIT 0.00.
   
   IF iCurrency = "" THEN iCurrency = "810".
   
   FIND FIRST b-code WHERE b-code.class = "ОпОтмыв" 
                       AND b-code.code  = "9" + iCurrency
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-code THEN RETURN. /* Все ОК, контроля по этой валюте нет */
   
   IF NUM-ENTRIES(b-code.val, CHR(1)) >= 16 THEN 
     vSumm = DECIMAL(ENTRY(16, b-code.val, CHR(1))).

   IF iSumm >= vSumm THEN out_Result = "Да".
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "ДтКапит"
   &DESCRIPTION   = "Возвращает дату последней капитализации (выплаты) процентов"
   &PARAMETERS    = "НАЗНАЧЕНИЕ ДОГОВОРА,ИДЕНТИФИКАТОР ДОГОВОРА[,ДАТА]"
   &RESULT        = "Дата последней капитализации процентов, относительно ~
переданной даты"
   &SAMPLE        = "ДтКапит(ДЕПОЗ,123,ДАТА())"
   }

   DEF INPUT  PARAM iContract   AS CHAR   NO-UNDO.  /* Назначение договора */
   DEF INPUT  PARAM iContCode   AS CHAR   NO-UNDO.  /* Номер договора */
   DEF INPUT  PARAM iDate       AS DATE   NO-UNDO.  /* Дата относительно, 
                                                       которой вычисляется дата
                                                       начисления процентов */

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEF BUFFER b-loan-acct FOR loan-acct.

   DEF VAR vCntDate AS DATE        NO-UNDO. /* Дата расчета */
   DEF VAR vDtKap   AS DATE        NO-UNDO. /* Дата последней капитализации */
   DEF VAR vDtCond  AS DATE        NO-UNDO.
   DEF VAR vTmpDate AS DATE        NO-UNDO.
   DEF VAR vLocalPV AS CHARACTER   NO-UNDO.
   DEF VAR h_templ  AS HANDLE      NO-UNDO.   
   
   vLocalPV = "0".
   {pchkpar iContract iContCode}
   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /*поиск договора по входящим параметрам*/
      FIND FIRST loan WHERE loan.contract  EQ iContract
                        AND loan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
      DO:
         is-ok = -1.
         RETURN.
         UNDO MAIN, LEAVE MAIN.      
      END.

      /* Определение даты расчета */
      ASSIGN
         vCntDate = IF iDate NE DATE("") AND iDate NE ? THEN iDate
                                                        ELSE GetBaseOpDate()
         vDtKap = loan.open-date
         .

      /* Определение даты капитализации */
      RUN get_beg_kper IN h_dpspc (RECID(loan),
                                   vCntDate,
                                   ?,
                                   INPUT-OUTPUT vDtKap).

      IF vDtKap EQ ? THEN
         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                           vCntDate,
                                           OUTPUT vCntDate,
                                           OUTPUT vTmpDate). 


      /*ищем последнее условие и сравниваем др cond-cr-date  с датой капитализации*/
      FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
                            AND loan-cond.cont-code EQ loan.cont-code NO-LOCK NO-ERROR.


      IF AVAIL loan-cond THEN
         vDtCond = DATE(GetXAttrValueEX("loan-cond",
                                    loan-cond.contract + "," + loan-cond.cont-code + "," +  
                                    STRING(YEAR (loan-cond.since),"9999") + STRING(MONTH(loan-cond.since),"99" ) + STRING(DAY (loan-cond.since),"99" ),
                                   "cond-cr-date",
                                   "")).
      /*ищем последнюю дебетовую субаналитическую проводку*/
      IF vDtKap EQ loan.open-date OR vDtCond EQ vDtKap THEN
      DO:
         FOR EACH loan-acct OF loan WHERE 
                  loan-acct.acct-type = 'loan-dps-int' NO-LOCK:

            FOR LAST kau-entry WHERE kau-entry.acct = loan-acct.acct AND 
                                     kau-entry.currency = loan-acct.currency AND 
                                     kau-entry.kau BEGINS loan.contract + ',' + loan.cont-code + ','  AND 
                                     kau-entry.debit AND 
                                     kau-entry.op-date GE loan-acct.since NO-LOCK:
               FIND FIRST op OF kau-entry NO-LOCK NO-ERROR.
               IF AVAILABLE op THEN 
                  vDtKap = op.contract-date.
            END.
         END.
      END.

      vLocalPV = IF vDtKap NE ? THEN STRING(vDtKap, "99/99/9999")
                                ELSE STRING(vCntDate, "99/99/9999").
   END.

   out_Result = vLocalPV.
   RETURN.

END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "NomerVklada"
   &DESCRIPTION   = "Возвращает номер вклада"
   &PARAMETERS    = "РОЛЬ СЧЕТА,НОМЕР СЧЕТА"
   &RESULT        = "Номер вклада"
   &SAMPLE        = "NomerVklada(loan-dps-p,42301810000000001225)"
   }

   DEF INPUT  PARAM iAcct-role   AS CHAR   NO-UNDO.  
   DEF INPUT  PARAM iAcct        AS CHAR   NO-UNDO.  
   DEF OUTPUT PARAM oUt_Result AS CHAR   NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEF VAR vLoanCount AS INT64 NO-UNDO.
   DEF VAR vAcct      AS CHAR  NO-UNDO.
   DEF VAR vCont-code AS CHAR  NO-UNDO.

   DEF BUFFER loan-acct FOR loan-acct.

   vAcct = iAcct.

   IF NUM-Entries(vAcct,"@") EQ 1 THEN
      vAcct = AddFilToAcct(vAcct, shFilial).

/* SORT-ACCESS loan-acct из-за вынужденной сортировки по номеру договора */
   FOR EACH loan-acct WHERE loan-acct.acct      EQ vAcct
                        AND loan-acct.acct-type EQ iAcct-role
      NO-LOCK BREAK BY loan-acct.cont-code:

      IF FIRST-OF(loan-acct.cont-code) THEN
      DO:
         vCont-code = loan-acct.cont-code.
         vLoanCount = vLoanCount + 1.
      END.
   END.

   IF vLoanCount EQ 1 THEN
      ASSIGN
         out_Result = IF ShMode THEN addFilToLoan(TRIM(vCont-code), shFilial) 
                                ELSE TRIM(vCont-code)
      .
   ELSE
      ASSIGN
         out_Result = ""
      .
   RETURN.

END PROCEDURE.
/* $LINTFILE='pp-pdps.p' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='kuds' */
/* $LINTDATE='31/05/2016 11:30:54.737+03:00' */
/*prosignyHzpBATJlYJPCML80F+5hA*/