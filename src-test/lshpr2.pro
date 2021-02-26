/* Функция определения процедуры расчета параметра */
FUNCTION re_proc_name{&EX} RETURNS CHAR (
    INPUT in_param AS INT64,
    INPUT is_rasn  AS LOGICAL):

    RETURN IF LOOKUP (STRING(in_param), s_params) NE 0 AND is_rasn
        THEN ("param_" + ENTRY(LOOKUP (STRING(in_param), s_params), s_params) + "{&EX}")
        ELSE "stndrt_param{&EX}".
END FUNCTION.

/* Процедура расчета параметра на дату стандартным методом */
PROCEDURE STNDRT_PARAM{&EX}:
   DEFINE INPUT  param in_contract  LIKE   loan.contract  NO-UNDO.
   DEFINE INPUT  param in_cont_code LIKE   loan.cont-code NO-UNDO.
   DEFINE INPUT  param in_param     AS     INT64          NO-UNDO.
   DEFINE INPUT  param in_since     LIKE   loan.since     NO-UNDO.
   &IF DEFINED(EX) &THEN
   DEFINE INPUT  param in_loan_since LIKE  loan.since     NO-UNDO.
   &ENDIF
   DEFINE OUTPUT param param_val    AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param db_oper      AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param cr_oper      AS     DECIMAL        NO-UNDO.

   DEFINE VAR re_date_var AS DATE NO-UNDO. /* Начало интервала расчета операций */

   DEFINE BUFFER loan-var FOR loan-var.
   DEFINE BUFFER loan     FOR loan.

   &IF DEFINED(EX) = 0 &THEN
   FIND FIRST loan WHERE
              loan.contract  EQ in_contract
          AND loan.cont-code EQ in_cont_code
    NO-LOCK NO-ERROR.

   IF NOT AVAIL loan THEN
       RETURN ?.
   &ENDIF

   IF in_since EQ ? THEN
      &IF DEFINED(EX) &THEN
      in_since = in_loan_since
      &ELSE
      in_since = loan.since
      &ENDIF
      .

    /* Поиск значения параметра */
/*
   FIND LAST loan-var WHERE
             loan-var.contract  EQ in_contract
         AND loan-var.cont-code EQ in_cont_code
         AND loan-var.amt-id    EQ in_param
         AND loan-var.since     LE in_since
    NO-LOCK NO-ERROR.
*/

   {profind.i &FWAY=LAST &FBUF=loan-var &FLOCK=NO-LOCK &FNOER=YES
         &FCOND="WHERE loan-var.contract  EQ in_contract
         AND loan-var.cont-code EQ in_cont_code
         AND loan-var.amt-id    EQ in_param
         AND loan-var.since     LE in_since"}

    /* Получение даты пересчета договора */
   &IF DEFINED(EX) &THEN
   re_date_var = in_loan_since.
   &ELSE
   re_date_var = loan.since.
   &ENDIF


   IF AVAIL loan-var
    THEN
       ASSIGN
         param_val   = loan-var.balance
         re_date_var = MAX (loan-var.since + 1, &IF DEFINED(EX) &THEN
                                                in_loan_since
                                                &ELSE
                                                loan.since
                                                &ENDIF
                                                )
       .
    ELSE
      re_date_var = &IF DEFINED(EX) &THEN
                    in_loan_since
                    &ELSE
                    loan.since
                    &ENDIF
                    .
   IF in_since GE re_date_var THEN
   DO:
    FOR EACH loan-int WHERE
           loan-int.contract  EQ in_contract
       AND loan-int.cont-code EQ in_cont_code
       AND loan-int.id-k      EQ in_param
          AND loan-int.mdate     GE re_date_var
          AND loan-int.mdate     LE in_since
    NO-LOCK:
       cr_oper = cr_oper + loan-int.amt-rub.
    END.

    FOR EACH loan-int WHERE
           loan-int.contract  EQ in_contract
       AND loan-int.cont-code EQ in_cont_code
       AND loan-int.id-d      EQ in_param
          AND loan-int.mdate     GE re_date_var
          AND loan-int.mdate     LE in_since
    NO-LOCK:
       db_oper = db_oper + loan-int.amt-rub.
    END.

    param_val = param_val - cr_oper + db_oper.
   END.

END PROCEDURE.

PROCEDURE oper-5{&EX}:
    DEFINE INPUT  param in_contract  LIKE loan.contract  NO-UNDO.
    DEFINE INPUT  param in_cont_code LIKE loan.cont-code NO-UNDO.
    DEFINE INPUT  param in_oper      LIKE chowhe.id-op   NO-UNDO.   /* Вид операции */
    DEFINE INPUT  param in_date      LIKE loan.since     NO-UNDO.   /* Дата расчета договора */
    &IF DEFINED(EX) &THEN
    DEFINE INPUT  param in_loan_since LIKE loan.since    NO-UNDO.
    &ENDIF
    DEFINE OUTPUT param re_val       AS   DECIMAL        NO-UNDO.

    DEFINE BUFFER  b_chowhe FOR chowhe.
    DEFINE BUFFER  b_lint   FOR loan-int.
    DEFINE BUFFER  xloan    FOR loan.
    DEFINE BUFFER  xl-var   FOR loan-var .

    FIND FIRST b_chowhe WHERE
        b_chowhe.id-op EQ in_oper
    NO-LOCK NO-ERROR.

    IF NOT AVAIL b_chowhe THEN RETURN.

    &IF DEFINED(EX) = 0 &THEN
    FIND FIRST xloan WHERE xloan.contract = in_contract AND  xloan.cont-code = in_cont_code NO-LOCK NO-ERROR.
    &ENDIF

   FIND LAST xl-var WHERE
             xl-var.contract = in_contract
         AND xl-var.cont-code = in_cont_code
         AND xl-var.amt-id = 0
         AND xl-var.since LE in_date
   NO-LOCK NO-ERROR .

    FOR EACH b_lint WHERE
        b_lint.contract  EQ in_contract  AND
        b_lint.cont-code EQ in_cont_code AND
        b_lint.id-d      EQ b_chowhe.id-d AND
        b_lint.id-k      EQ b_chowhe.id-k AND
        b_lint.mdate     LE in_date AND
        b_lint.mdate     GE ( IF AVAIL xl-var AND xl-var.since GE &IF DEFINED(EX) &THEN in_loan_since &ELSE xloan.since &ENDIF THEN
                                 xl-var.since + 1
                              ELSE
                                 &IF DEFINED(EX) &THEN
                                 in_loan_since
                                 &ELSE
                                 xloan.since
                                 &ENDIF )
    NO-LOCK:
        re_val = re_val + b_lint.amt-rub.
    END.
 END PROCEDURE .


/* Процедура расчета '0'-го параметра на дату */
PROCEDURE PARAM_0{&EX}:
   DEFINE INPUT  param in_contract  LIKE   loan.contract  NO-UNDO.
   DEFINE INPUT  param in_cont_code LIKE   loan.cont-code NO-UNDO.
   DEFINE INPUT  param in_param     AS     INT64            NO-UNDO.
   DEFINE INPUT  param in_since     LIKE   loan.since     NO-UNDO.
   &IF DEFINED(EX) &THEN
   DEFINE INPUT  param in_loan_since LIKE  loan.since    NO-UNDO.
   &ENDIF
   DEFINE OUTPUT param param_val    AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param db_oper      AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param cr_oper      AS     DECIMAL        NO-UNDO.

   DEFINE VAR param_13 AS DECIMAL NO-UNDO. /* Значение 13 параметра */
   DEFINE VAR oper_5   AS DECIMAL NO-UNDO. /* Значение операции 5   */

   IF in_since EQ ? THEN
      &IF DEFINED(EX) &THEN
      in_since = in_loan_since.
      &ELSE
      RUN re_loan (in_contract, in_cont_code, OUTPUT in_since).
      &ENDIF
   IF in_since EQ ? THEN
       RETURN ?.

   RUN VALUE (re_proc_name{&EX} (13, NO))
                                    (in_contract,
                                     in_cont_code,
                                     13,
                                     in_since,
                                     &IF DEFINED(EX) &THEN
                                     in_loan_since,
                                     &ENDIF
                                     OUTPUT param_13,
                                     OUTPUT db_oper,
                                     OUTPUT cr_oper).


   RUN VALUE (re_proc_name{&EX} (in_param, NO))
                                          (in_contract,
                                           in_cont_code,
                                           in_param,
                                           in_since,
                                           &IF DEFINED(EX) &THEN
                                           in_loan_since,
                                           &ENDIF
                                           OUTPUT param_val,
                                           OUTPUT db_oper,
                                           OUTPUT cr_oper).


   RUN oper-5{&EX}
             (in_contract,
              in_cont_code,
              5,
              in_since,
              &IF DEFINED(EX) &THEN
              in_loan_since,
              &ENDIF
              OUTPUT oper_5).

   /* Значение параметра без разноски */
   /* Дебетовые и кредитовые операции учитывать не надо,
      т.к. учет производиться автоматически */

   /* Разноска операций и параметров */
   IF (oper_5  - param_13)  GT 0 THEN
   DO:
      IF (param_val - (oper_5 - param_13)) GT 0 THEN
         ASSIGN
            param_val = param_val - (oper_5 - param_13)
            cr_oper   = oper_5 - param_13
         .
      ELSE
         ASSIGN
            cr_oper   = param_val
            param_val = 0
         .
      IF param_val < 0 THEN
         param_val = 0.
   END.
END PROCEDURE.

/* Процедура расчета '13'-го параметра на дату */
PROCEDURE PARAM_13{&EX}:
   DEFINE INPUT  param in_contract  LIKE   loan.contract  NO-UNDO.
   DEFINE INPUT  param in_cont_code LIKE   loan.cont-code NO-UNDO.
   DEFINE INPUT  param in_param     AS     INT64            NO-UNDO.
   DEFINE INPUT  param in_since     LIKE   loan.since     NO-UNDO.
   &IF DEFINED(EX) &THEN
   DEFINE INPUT  param in_loan_since LIKE  loan.since    NO-UNDO.
   &ENDIF
   DEFINE OUTPUT param param_val    AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param db_oper      AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param cr_oper      AS     DECIMAL        NO-UNDO.

   DEF VAR oper_5 AS DEC  NO-UNDO. /* Значение операции 5   */
   DEF VAR vSince AS DATE NO-UNDO.

   &IF DEFINED(EX) = 0 &THEN
   DEFINE BUFFER loan FOR loan.

   FIND FIRST loan WHERE
              loan.contract  EQ in_contract
          AND loan.cont-code EQ in_cont_code
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN ?.
   &ENDIF
   
   IF in_since EQ ?  THEN
      &IF DEFINED(EX) &THEN
      in_since = in_loan_since.
      &ELSE
      in_since = loan.since.
      &ENDIF

   IF in_since EQ ?
       THEN RETURN ?.

   RUN STNDRT_PARAM{&EX}
       (in_contract,
        in_cont_code,
        in_param,
        in_since,
        &IF DEFINED(EX) &THEN
        in_loan_since,
        &ENDIF
        OUTPUT param_val,
        OUTPUT db_oper,
        OUTPUT cr_oper).

   IF in_since >= &IF DEFINED(EX) &THEN in_loan_since &ELSE loan.since &ENDIF THEN
   DO:

      RUN re_summ_oper
          (in_contract, in_cont_code, 5, in_since, OUTPUT oper_5).

      /* Разноска операций и параметров */
      IF oper_5 GT 0
      THEN DO:

          IF (param_val - oper_5) GT 0
          THEN ASSIGN
              cr_oper   = oper_5
              param_val = param_val - oper_5
          .
          ELSE ASSIGN
              cr_oper   = param_val
              param_val = 0
          .
      END.
   END. /* in_since > loan.since */
END PROCEDURE.

/* Процедура расчета '33'-го параметра на дату */
PROCEDURE PARAM_33{&EX}:
   DEFINE INPUT  param in_contract  LIKE   loan.contract  NO-UNDO.
   DEFINE INPUT  param in_cont_code LIKE   loan.cont-code NO-UNDO.
   DEFINE INPUT  param in_param     AS     INT64            NO-UNDO.
   DEFINE INPUT  param in_since     LIKE   loan.since     NO-UNDO.
   &IF DEFINED(EX) &THEN
   DEFINE INPUT  param in_loan_since LIKE  loan.since    NO-UNDO.
   &ENDIF

   DEFINE OUTPUT param param_val    AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param db_oper      AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param cr_oper      AS     DECIMAL        NO-UNDO.

   DEFINE VAR param_34  AS DECIMAL NO-UNDO. /* Значение 34 параметра */
   DEFINE VAR param_35  AS DECIMAL NO-UNDO. /* Значение 35 параметра */

   IF in_since EQ ? THEN
      &IF DEFINED(EX) &THEN
      in_since = in_loan_since.
      &ELSE
      RUN re_loan (in_contract, in_cont_code, OUTPUT in_since).
      &ENDIF
   IF in_since EQ ? THEN
       RETURN ?.

   RUN VALUE (re_proc_name{&EX} (34, NO))
                                     (in_contract,
                                      in_cont_code,
                                      34,
                                      in_since,
                                      &IF DEFINED(EX) &THEN
                                      in_loan_since,
                                      &ENDIF
                                      OUTPUT param_34,
                                      OUTPUT db_oper,
                                      OUTPUT cr_oper).
   RUN VALUE (re_proc_name{&EX} (35, NO))
                                     (in_contract,
                                      in_cont_code,
                                      35,
                                      in_since,
                                      &IF DEFINED(EX) &THEN
                                      in_loan_since,
                                      &ENDIF
                                      OUTPUT param_35,
                                      OUTPUT db_oper,
                                      OUTPUT cr_oper).
   RUN VALUE (re_proc_name{&EX} (in_param, NO))
                                     (in_contract,
                                      in_cont_code,
                                      in_param,
                                      in_since,
                                      &IF DEFINED(EX) &THEN
                                      in_loan_since,
                                      &ENDIF
                                      OUTPUT param_val,
                                      OUTPUT db_oper,
                                      OUTPUT cr_oper).

   /* Значение параметра без разноски */
   /* Дебетовые и кредитовые операции учитывать не надо,
      т.к. учет производиться автоматически */
   param_val = param_val.

   /* Разноска операций и параметров */
   IF (abs(param_35)  - param_34) GT 0
   THEN DO:
       IF (param_val - abs(param_35) + param_34) GT 0
       THEN ASSIGN
           cr_oper   = abs(param_35) - param_34
           param_val = param_val - abs(param_35) + param_34
       .
       ELSE ASSIGN
           cr_oper   = param_val
           param_val = 0
       .
   END.
END PROCEDURE.

/* Процедура расчета '34'-го параметра на дату */
PROCEDURE PARAM_34{&EX}.
   DEFINE INPUT  param in_contract  LIKE   loan.contract  NO-UNDO.
   DEFINE INPUT  param in_cont_code LIKE   loan.cont-code NO-UNDO.
   DEFINE INPUT  param in_param     AS     INT64            NO-UNDO.
   DEFINE INPUT  param in_since     LIKE   loan.since     NO-UNDO.
   &IF DEFINED(EX) &THEN
   DEFINE INPUT  param in_loan_since LIKE  loan.since    NO-UNDO.
   &ENDIF
   DEFINE OUTPUT param param_val    AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param db_oper      AS     DECIMAL        NO-UNDO.
   DEFINE OUTPUT param cr_oper      AS     DECIMAL        NO-UNDO.

   DEFINE VAR param_35  AS DECIMAL NO-UNDO. /* Значение 35 параметра */

   IF in_since EQ ? THEN
      &IF DEFINED(EX) &THEN
      in_since = in_loan_since.
      &ELSE
      RUN re_loan (in_contract, in_cont_code, OUTPUT in_since).
      &ENDIF
   IF in_since EQ ?
       THEN RETURN ?.

   RUN VALUE (re_proc_name{&EX} (35, NO))
                                     (in_contract,
                                      in_cont_code,
                                      35,
                                      in_since,
                                      &IF DEFINED(EX) &THEN
                                      in_loan_since,
                                      &ENDIF
                                      OUTPUT param_35,
                                      OUTPUT db_oper,
                                      OUTPUT cr_oper).
   RUN VALUE (re_proc_name{&EX} (in_param, NO))
                                     (in_contract,
                                      in_cont_code,
                                      in_param,
                                      in_since,
                                      &IF DEFINED(EX) &THEN
                                      in_loan_since,
                                      &ENDIF
                                      OUTPUT param_val,
                                      OUTPUT db_oper,
                                      OUTPUT cr_oper).

   /* Значение параметра без разноски */
   /* Дебетовые и кредитовые операции учитывать не надо,
      т.к. учет производиться автоматически */
   param_val = param_val.

   /* Разноска операций и параметров */
   IF abs(param_35) GT 0
   THEN DO:
       IF (param_val - abs(param_35)) GT 0
       THEN ASSIGN
           param_val = param_val - abs(param_35)
           cr_oper   = abs(param_35)
       .
       ELSE ASSIGN
           cr_oper   = param_val
           param_val = 0
       .
   END.
END PROCEDURE.

/* ------------- ACTIONS ----------------- */
/* Процедура вызова расчета праметров договоров. */

PROCEDURE RE_PARAM{&EX}:

   DEFINE INPUT PARAM in_code_id    AS INT64  NO-UNDO. /* код параметра договора */
   DEFINE INPUT PARAM in_date       AS DATE NO-UNDO. /* дата расчета договора */
   &IF DEFINED(EX) &THEN
   DEFINE INPUT PARAM in_loan_since LIKE loan.since  NO-UNDO. /* loan.since */ 
   &ENDIF
   DEFINE INPUT PARAM in_contract   LIKE loan.contract  NO-UNDO. /* назначение договора */
   DEFINE INPUT PARAM in_cont_code  LIKE loan.cont-code NO-UNDO. /* номер договора */

   DEFINE OUTPUT param sh-bal     AS DECIMAL NO-UNDO.
   DEFINE OUTPUT param summ-db    AS DECIMAL NO-UNDO.
   DEFINE OUTPUT param summ-cr    AS DECIMAL NO-UNDO.
   
   &IF DEFINED(EX) = 0 &THEN
   FIND FIRST loan WHERE
              loan.contract  = in_contract
          AND loan.cont-code = in_cont_code
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN.
   &ENDIF
   
   RUN VALUE (re_proc_name{&EX} (in_code_id, YES))
       (&IF DEFINED(EX) &THEN
        in_contract,       /* Тип договора */
        in_cont_code,      /* Номер договора */
        &ELSE
        loan.contract,     /* Тип договора */
        loan.cont-code,    /* Номер договора */
        &ENDIF
        in_code_id,        /* Код параметра */
        in_date,           /* Дата пересчета договора */
        &IF DEFINED(EX) &THEN
        in_loan_since,     /* loan.since */ 
        &ENDIF
        OUTPUT sh-bal,     /* Значение параметра на дату пересчета договора */
        OUTPUT summ-db,    /* Дебетовы оборот по операциям */
        OUTPUT summ-cr).   /* Кредитовый оборот по операциям */
END PROCEDURE.

