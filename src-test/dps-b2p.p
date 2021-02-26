/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: DPS-B2P.P
      Comment: Набор "парсерных" процедур, используемых  в 
               транзакциях открытия новых вкладов
               на основе процедур g-cr2.p и g-cr2unk.p
   Parameters:
         Uses:
      Used by:
      Created:
                tt 0004779  Автоматическое определение № бал. сч. 2-ого порядка при пролонгации 
                            persistent процедура для передачи параметров парсеру 
  
                27/3/02 Парс.функция БалСчетОтк() - определение № бал. сч. 2-ого порядка при открытии вклада
                        Внутр.процедура SET-DEP-PERIOD : передача парсерной функции продолжительности открываемого вклада в
                        формате "Г=99,М=99,Д=99"
      
     Modified: 01.07.2002 14:09 mitr  tt 8271 - 
                                      Счета 702(loan-expens) и 457(loan-future) при настройке вкладов должны 
                                      зависить от резидентности.
                                      Т.е. В транзакции открытия должна быть возможность указать аналитический
                                      счет в зависимости от резидентности клиента.
                                      В момент обработки шаблонов счетов номер вклада нам не известен, поэтому
                                      мы не можем использовать функцию КЛИЕНТ из g-pfunc.def и создаем 
                                      новый вариант данной функции с именем O_РЕЗИДЕНТ (возвращает 1, 
                                      если вкладчик является резидентом РОССИИ).
                                                
     Modified: 4/7/02 mitr@bis.ru     добавлена парсерная функция  О_ВАЛЮТА():   возвращает код валюты , в которой открывается вклад
     Modified: 
*/

DEFINE INPUT PARAMETER loan-rid     AS RECID. /* loan */
DEFINE INPUT PARAMETER in-op-kind   LIKE op-kind.op-kind.
{sh-defs.i}
{globals.i} 
{dpsproc.def}
{intrface.get xclass}

DEF VAR local-dep-period   AS CHARACTER INIT ?  NO-UNDO.
DEF VAR local-summa        AS DECIMAL   INIT ?  NO-UNDO.
DEF VAR local-branch-id    AS CHARACTER INIT ?  NO-UNDO. 
DEF VAR in-op-kind$        AS CHARACTER         NO-UNDO.

DEF TEMP-TABLE tt
   FIELD INTERVAL AS INT64
   FIELD bal-acct AS CHARACTER
   INDEX INTERVAL INTERVAL.

if not this-procedure:persistent then do:
  message "Процедура " this-procedure:file-name " должна запускаться с опцией persistent" view-as alert-box.
  return error.
end.

this-procedure:private-data = "parssen library,БалСчетПер,БалСчетОтк,БалСчетПрол,О_ТИПВКЛ,О_НОМДОГ,О_ФИО,О_ОТКР,О_СТАВ,О_СРОК,О_ГАСИТЬ,о_резидент,о_валюта,О_ОТДЕЛЕНИЕ".


/* ф-ция возвращающая параметр, передаваемый в процедуру */
/* после разбора строки  параметров парсером             */
{getprm.lib}

function dat_ return char (input d as INT64,
              input p as char):
    def var yy as char extent 3 init ['год','года','лет'] no-undo.
    def var mm as char extent 3 init ['месяц','месяца','месяцев'] no-undo.
    def var dd as char extent 3 init ['день','дня','дней'] no-undo.
    d = if d gt 3 then 3 else d.
    if p eq 'г' then return yy[d].
    else if p eq 'м' then return mm[d].
    else if p eq 'д' then return dd[d].
end function.

/* инициализация лок.переменных для процедуры открытия нового вклада 
данные переменные используются в парсерной функции БалСчетОтк() */
PROCEDURE SET-private-data:  
   DEF INPUT PARAMETER in-dep-period   AS CHARACTER   NO-UNDO.
   DEF INPUT PARAMETER in-summa        AS DECIMAL     NO-UNDO.

  ASSIGN   
     local-dep-period = in-dep-period
     local-summa      = in-summa
     .
END PROCEDURE.

procedure setBranch-id:
  def input parameter in-branch-id as char no-undo.
  local-branch-id = in-branch-id.
end procedure.

/*---------------------------------------------------------------------------------------------------------------------------------
  Сумма пролонгированного вклада должна учитываться на балансовом счете второго порядка, установленного ЦБ РФ для учета вкладов, 
  внесенных на срок, который рассчитывается как общий срок привлечения (срок фактического нахождения на счете плюс дополнительный 
  срок привлечения при пролонгации). 
  т.е для депозитов физ лиц 
    42301 - до востребования;
    42302 - на срок до 30 дн.,
    42303 - от 31до 90 дн.,
    42304 - от 91 до 180 дн.,
    42305 - от 181дн. до 1года;
    42306 - от 1г. до 3 лет;
    42307 - свыше 3лет.
  Определение номера балансового счета второго порядка, на котором должна учитываться сумма пролонгированного вклада в соответствии 
  с требованиями ЦБ РФ, должно проводится автоматически, после чего в необходимых случаях  вклад переносится на нужный балансовый счет 
  по срокам привлечения, считая от даты начала договора.
  Основание правила 61 и ПИСЬМО ЦБ от 27 марта 1996 г. N 25-1-322 п. IV.
  Получено подтверждение из ГУТА Банка см. прилож. файл.   
---------------------------------------------------------------------------------------------------------------------------------*/
PROCEDURE БалСчетПер:
  DEF INPUT PARAM rid         AS RECID   NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE    NO-UNDO.
  DEF INPUT PARAM param-count AS INT64     NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR    NO-UNDO.
   
  DEF VAR vnew-date           AS DATE.
  DEF VAR vdelta              AS INT64.
  DEF VAR in-op-template      AS INT64 NO-UNDO.
  DEF VAR vperiod             AS CHAR    NO-UNDO.
  DEF VAR vOpenDate AS DATE NO-UNDO.
  
  /* таблица TT - для сортировки */
  for each tt: delete tt. end.
  for each code where code.class = "dps-b2p" :
    create tt.
    assign tt.interval = INT64(code.code)
           tt.bal-acct = code.val
    .
  end.

  /*для конвертированных вкладов*/
  FIND FIRST loan WHERE RECID(loan) = loan-rid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE loan THEN RETURN. 
  
  vOpenDate = DATE(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"ДатаОткр",?)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN vOpenDate = ?.
  IF vOpenDate = ?  THEN vOpenDate = loan.open-date.

  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    vperiod = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dep_period",?).
    IF vperiod NE ? THEN
    DO: /* проверка установки параметра dep_period (для g-cr2.p) */       
      vnew-date =  Get-End-Date(loan.end-date, vperiod).
      IF vnew-date = loan.end-date THEN vdelta = 0.
      ELSE vdelta = vnew-date -  vOpenDate.
    END.
    ELSE
    DO: /* для g-cr.p */  
      RUN set-op-kind . /* транзакция пролонгации -> in-op-kind$ */
      IF return-value = "error" THEN UNDO, LEAVE.
      in-op-template = GET_OP-TEMPL(in-op-kind$, "loan", "").
      IF in-op-template NE ? THEN
      DO: /* нашли шаблон пролонгации  */ 
        FIND op-template WHERE op-template.op-kind     EQ in-op-kind$
                           AND op-template.op-template EQ in-op-template  NO-LOCK.

        vperiod = Get_Param('dep-period', RECID(op-template)).
        IF vperiod = ? THEN vdelta = 0. /* параметр dep-period в шаблоне не задан */
        ELSE
        DO: 
          vnew-date = Get-End-Date(loan.end-date, vperiod).
          IF vnew-date = loan.end-date THEN vdelta = 0.
          ELSE vdelta = vnew-date - vOpenDate.
        END.
      END.
      ELSE vdelta = 0. /* не нашли шаблон пролонгации, тогда до востребования */ 
    END. 

    /* резидент или нет*/
    FIND person WHERE person.person-id = loan.cust-id NO-LOCK.
  
    FIND FIRST tt WHERE tt.interval GE vdelta NO-ERROR.
    IF AVAIL tt THEN pick-value = ENTRY( IF person.country-id = "rus" THEN 1 ELSE 2, tt.bal-acct) .
                                  
  END. /* DO */

/*message " БалСчетПер: " skip
        "vdelta = " vdelta skip
        "loan.cont-code = " loan.cont-code skip
        "person.country-id = " person.country-id skip
        "in-op-kind = " in-op-kind skip 
        "loan.end-date = " loan.end-date skip
        "loan.open-date = " loan.open-date SKIP
        "vnew-date = " vnew-date skip
        "dep-period = " Get_Param('dep-period', RECID(op-template)) skip
        "in-op-kind$ = " in-op-kind$ skip
        "in-op-template = " in-op-template skip
     "dep_period = " GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"dep_period",?) skip(1)     
view-as alert-box.
*/

end procedure.


/* Аналог БалСчетПер, но счиатет период не от даты открытия, 
** а от даты пролонгации */
PROCEDURE БалСчетПрол:
   DEF INPUT PARAM rid         AS RECID   NO-UNDO.
   DEF INPUT PARAM in-op-date  AS DATE    NO-UNDO.
   DEF INPUT PARAM param-count AS INT64     NO-UNDO.
   DEF INPUT PARAM param-str   AS CHAR    NO-UNDO.
       
   DEF VAR vRetVal        AS CHARACTER INIT ?  NO-UNDO. /* Возвращаемое значение */
   DEF VAR vProlDate      AS DATE              NO-UNDO. /* Дата начала вклада */
   DEF VAR vPeriod        AS CHARACTER         NO-UNDO. /* Продолжительность вклада */
   DEF VAR vNewDate       AS DATE              NO-UNDO. /* Новая дата окончания */
   DEF VAR vDelta         AS INT64           NO-UNDO.   
   DEF VAR in-op-template AS INT64           NO-UNDO.
   
    /* таблица TT - для сортировки */
   {empty tt}
   
   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
   
      /* Для корректной сортировки */
      FOR EACH CODE WHERE CODE.CLASS EQ "dps-b2p"
         NO-LOCK:
         CREATE tt.
         ASSIGN 
            tt.interval = INT64(code.code)
            tt.bal-acct = code.val
         .
      END.
    
      /*для конвертированных вкладов*/
      FIND FIRST loan WHERE RECID(loan) EQ loan-rid 
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loan THEN
         UNDO MAIN, LEAVE MAIN.
            
      ASSIGN
         vProlDate = IF in-op-date EQ ? THEN DATE(GetSysConf("CDate4dpsb2p"))
                                        ELSE in-op-date
      
         vPeriod   = GetXattrValueEx("loan",
                                     loan.contract + "," + loan.cont-code,
                                     "dep_period",
                                     ?).
   
      /* проверка установки параметра dep_period (для g-cr2.p) */       
      IF vPeriod NE ? THEN      
         ASSIGN
            vNewDate = Get-End-Date(loan.end-date, vPeriod)
            vDelta   = IF vNewDate EQ loan.end-date THEN 0
                                                    ELSE (vNewDate - vProlDate).      
      ELSE
      /* для g-cr.p */  
      DO: 
         /* транзакция пролонгации -> in-op-kind$ */
         RUN set-op-kind. 
         IF RETURN-VALUE EQ "error" THEN 
            UNDO MAIN, LEAVE MAIN.
         ASSIGN 
            vDelta = 0
            in-op-template = Get_Op-Templ(in-op-kind$, "loan", "")
            .         
         IF in-op-template NE ? THEN
         DO:             
            /* нашли шаблон пролонгации  */ 
            FIND op-template WHERE op-template.op-kind     EQ in-op-kind$
                               AND op-template.op-template EQ in-op-template  
               NO-LOCK NO-ERROR.
    
            vPeriod = Get_Param('dep-period', RECID(op-template)).   
            IF vPeriod NE ? THEN                            
               ASSIGN
                  vNewDate = Get-End-Date(loan.end-date, vPeriod)
                  vDelta   = IF vNewDate EQ loan.end-date THEN 0
                                                          ELSE (vNewDate - vProlDate).               
         END.         
      END. 
    
      /* резидент или нет*/
      FIND FIRST person WHERE person.person-id EQ loan.cust-id 
         NO-LOCK NO-ERROR.      
      FIND FIRST tt WHERE tt.interval GE vDelta 
         NO-ERROR.
      IF AVAIL tt THEN 
         vRetVal = ENTRY(IF person.country-id EQ "rus" THEN 1 
                                                       ELSE 2, tt.bal-acct).
   END.

   pick-value = vRetVal.
END PROCEDURE.


/*------------------------------------------------------------------------ 
    определение транзакции пролонгации ; результат в in-op-kind 
    return-value() ="error" если возникли ошибки в поиске
------------------------------------------------------------------------*/
procedure set-op-kind:

  def var t             as INT64 no-undo.
  def var loan-op-kind$ as char    no-undo.
  def var out-op-kind as char no-undo .
  DEF VAR fl-ok-prol AS LOGICAL NO-UNDO .

  FIND FIRST op-kind WHERE op-kind.op-kind EQ loan.op-kind NO-LOCK NO-ERROR.
  IF AVAIL op-kind THEN 
  DO:    
     t = get_op-templ(op-kind.op-kind, "loan", "").
     IF t EQ ? THEN 
     DO:        
        RETURN "".
     END.             
     FIND op-template OF op-kind WHERE op-template.op-template EQ t NO-LOCK NO-ERROR.
     IF AVAIL op-template THEN 
     DO:
        RUN Chk_Limit_Per in h_dpspc (loan.end-date,
                                              RECID(loan),
                                              loan.prolong + 1,
                                              OUTPUT fl-ok-prol).  
        /* определение по какой транзакции будет жить вклад */
        IF NOT fl-ok-prol THEN 
        DO:
           ASSIGN in-op-kind$ = ?.
        END.
        ELSE
        DO:
           RUN get-param-const in h_dpspc (RECID(loan),
                                                   'prol-kind',
                                                   OUTPUT in-op-kind$).
           IF in-op-kind$ = ? THEN 
              ASSIGN in-op-kind$ = loan.op-kind.
        END.
     END.
     ELSE 
        RETURN "error".
  END.
  ELSE
     RETURN "error".

end procedure.
/* --------------- Конец определения транзакции пролонгации ------------*/


/*----------------------------------------------------------------------------------------------  
  27/3/02 Парс.функция БалСчетОтк() - определение № бал. сч. 2-ого порядка 
          при открытии вклада 
-----------------------------------------------------------------------------------------------*/
PROCEDURE БалСчетОтк:
   DEF INPUT PARAM rid         AS RECID NO-UNDO.
   DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
   DEF INPUT PARAM param-count AS INT64   NO-UNDO.
   DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.
   
   DEFINE VARIABLE vDelta     AS INT64     NO-UNDO.
   DEFINE VARIABLE vNext29Feb AS DATE        NO-UNDO.
   DEFINE VARIABLE vCorrInt   AS INT64     NO-UNDO.
   DEFINE VARIABLE vCorrMeth  AS INT64     NO-UNDO.
   DEFINE VARIABLE vI         AS INT64     NO-UNDO.
   DEFINE VARIABLE vNameproc  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParams    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNumYears  AS INT64     NO-UNDO.
   DEFINE VARIABLE vEndDate   AS DATE      NO-UNDO.

   /* таблица TT - для сортировки */  
   {empty tt}
   FOR EACH code WHERE code.class EQ "dps-b2p"
      NO-LOCK:
      CREATE tt.
      ASSIGN 
         tt.interval = INT64(code.code)
         tt.bal-acct = code.val
         .
   END.
    
   pick-value = ?.
   
   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:  
   
     /* резидент или нет*/
     FIND FIRST loan WHERE RECID(loan) EQ loan-rid 
        NO-LOCK NO-ERROR.
     IF NOT AVAIL loan THEN
        UNDO MAIN, LEAVE MAIN.
   
     FIND FIRST person WHERE person.person-id EQ loan.cust-id 
        NO-LOCK NO-ERROR.
     IF NOT AVAIL person THEN
        UNDO MAIN, LEAVE MAIN.

     RUN deposit-dep-period IN h_dpspc (RECID(loan), 
                                        in-op-date, 
                                        local-dep-period, 
                                        OUTPUT vDelta).

     /* Если период указан в годах, то начинаются жесткие танцы с бубном, т.к.
     ** надо учесть то, что при переводе в дни в зависимости от метода
     ** "get-end-date" срок в днях может увеличиваться на день, а в случае
     ** если в промежуток попадают високосные годы, то еще на некоторое 
     ** количество дней, и при этом в справочнике "dps-b2p" период 
     ** указывается только в днях */
     IF     NUM-ENTRIES(local-dep-period) EQ 3
        AND local-dep-period BEGINS "Г" THEN
        vNumYears = INT64(ENTRY(2, ENTRY(1, local-dep-period), "=")) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
        vNumYears = 0.
     IF vNumYears NE 0 THEN
     DO:
        /* Ищем сколько 29-ых февраля попадает в срок жизни вклада */
        DO vI = YEAR(gend-date) TO YEAR(gend-date + vDelta):
           vNext29Feb = DATE(02, 29, vI) NO-ERROR.
           IF     NOT ERROR-STATUS:ERROR
              AND vNext29Feb LT (gend-date + vDelta)
              AND vNext29Feb GT gend-date THEN
              ASSIGN
                 vCorrInt     = vCorrInt + 1
                 vNext29Feb   = ?
                 .          
        END.
     
        /* Определяем, используется ли процедура метода "get-end-date" */
        RUN GetClassMethod IN h_xclass (loan.class-code, 
                                        "get-end-date",
                                        "",
                                        "",
                                        OUTPUT vNameproc,
                                        OUTPUT vParams).
        /* Если метод задан, то ищем разницу между вычесленной продолжительностью
        ** вклада и если бы метод был бы не задан, т.е. безо всяких лишних дней */
        IF {assigned vNameproc} THEN 
        DO:
           vEndDate = get-end-date(gend-date, local-dep-period).            
           vCorrMeth = vDelta - (vEndDate - gend-date).
           IF MONTH(vEndDate) EQ 2 AND DAY(vEndDate) EQ 29 THEN
              vCorrInt = vCorrInt - 1.
        END.
        /* Корректируем продолжительность на разницу в сроках из за метода */
        vDelta = vDelta - vCorrMeth.

        /* Корректируем продолжительность на разницу в связи с високосными годами */
        IF ((vDelta - vCorrInt) MODULO 365) EQ 0 THEN
           vDelta = vDelta - vCorrInt.
     END.
   
     /* Ищем балансовый счет в справочнике "dps-b2p" */
     FIND FIRST tt WHERE tt.interval GE vDelta 
        NO-ERROR.
     IF AVAIL tt THEN 
        pick-value = ENTRY(IF person.country-id EQ "rus" THEN 1 
                                                         ELSE 2, tt.bal-acct).                                  
   END. /* DO */
END PROCEDURE.


/*--------------------------------------------------  
  ФУНКЦИИ ДЛЯ ФОРМИРОВАНИЯ НАЗВАНИЯ СЧЕТА
  1) О_ТИПВКЛ  -  ТИП ВКЛАДА
  2) О_НОМДОГ - НОМЕР ДОГОВОРА
  3) О_ФИО    ФИО ВКЛАДЧИКА
  4) О_ОТКР   ДАТА ОТКРЫТИЯ ВКЛАДА
  5) О_СТАВ   ПРОЦ.СТАВКА
  6) О_СРОК   СРОК В ВИДЕ "1 ГОД 6 МЕСЯЦЕВ 20 ДНЕЙ"
  7) О_ГАСИТЬ ДАТА ОКОНЧАНИЯ ДОГОВОРА (ДАТА ВЫДАЧИ МИНУС 1 ДЕНЬ)
---------------------------------------------------*/

/*----------------------------------------------------------------------------------------------  
  1) О_ТИПВКЛ  -  ТИП ВКЛАДА
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_ТИПВКЛ:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND loan WHERE RECID(loan) = loan-rid
       NO-LOCK NO-ERROR.
    FIND FIRST CODE WHERE CODE.class = "cont-type" 
                      AND CODE.CODE  = loan.cont-type
       NO-LOCK NO-ERROR.
    IF AVAIL (CODE) THEN
       pick-value = CODE.NAME.                
  END. /* DO */
END PROCEDURE.
/*----------------------------------------------------------------------------------------------  
  2) О_НОМДОГ - НОМЕР ДОГОВОРА
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_НОМДОГ:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND FIRST loan WHERE RECID(loan) = loan-rid NO-LOCK.
    pick-value = loan.doc-ref.
                
  END. /* DO */
END PROCEDURE.
/*----------------------------------------------------------------------------------------------  
  3) О_ФИО    ФИО ВКЛАДЧИКА
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_ФИО:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND loan WHERE RECID(loan) = loan-rid NO-LOCK.
    find person where person.person-id = loan.cust-id no-lock.
    pick-value = person.name-last + " " + person.first-names .                
  END. /* DO */
END PROCEDURE.
/*----------------------------------------------------------------------------------------------  
  4) О_ОТКР   ДАТА ОТКРЫТИЯ ВКЛАДА
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_ОТКР:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND loan WHERE RECID(loan) = loan-rid NO-LOCK.
    pick-value = STRING(loan.open-date, "99/99/9999").                
  END. /* DO */
END PROCEDURE.
/*----------------------------------------------------------------------------------------------  
  5) О_СТАВ   ПРОЦ.СТАВКА
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_СТАВ:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

  DEF VAR in-templ AS INT64 NO-UNDO.
  DEF VAR cmm AS CHAR NO-UNDO.
  DEF VAR delta AS INT64 INITIAL 0 NO-UNDO.
  DEF VAR mAcctBaseDps AS CHAR NO-UNDO.
  DEF VAR mBegDate AS DATE NO-UNDO.
  DEF VAR mEndDate AS DATE NO-UNDO.
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND loan WHERE RECID(loan) = loan-rid NO-LOCK.
    IF param-count GE 1 AND GetParam(1,param-str) = "Штр" THEN
    RUN Get_Last_Pen-Commi in h_dpspc(RECID(loan),in-op-date,in-op-date,OUTPUT cmm).
    ELSE
    RUN Get_Last_Commi in h_dpspc(RECID(loan),in-op-date,in-op-date,OUTPUT cmm).
    RUN deposit-dep-period in h_dpspc (RECID(loan), in-op-date, local-dep-period, OUTPUT delta).
    delta = IF delta < 0 THEN 0 ELSE delta.

    IF local-summa eq ? THEN DO:
       RUN GetBaseAcct IN h_dpspc(loan.contract,loan.cont-code,in-op-date,OUTPUT mAcctBaseDps).
       FIND FIRST acct WHERE acct.acct eq entry(1,mAcctBaseDps) and acct.currency eq entry(2,mAcctBaseDps) no-lock no-error.
       IF AVAIL(acct) THEN DO:           
          RUN Get_Date_Comm IN h_dpspc(RECID(loan),in-op-date,OUTPUT mBegDate, OUTPUT mEndDate).
          RUN acct-pos IN h_base (acct.acct,acct.currency,mBegDate,mBegDate,gop-status).
          local-summa = ABS(IF acct.currency EQ "" THEN sh-bal ELSE sh-val).
       END.
    END.
        

    FIND LAST comm-rate WHERE comm-rate.commi EQ  cmm
                           AND comm-rate.filial-id = shfilial
                           AND comm-rate.branch-id = ""
                           AND comm-rate.acct EQ  "0" 
                           AND comm-rate.currency EQ loan.currency 
                           AND comm-rate.min-val <= local-summa 
                           AND comm-rate.period LE delta
                           AND comm-rate.since LE in-op-date NO-LOCK NO-ERROR .
    IF AVAIL comm-rate THEN pick-value = STRING(comm-rate.rate-comm).
  END. /* DO */
END PROCEDURE.
/*----------------------------------------------------------------------------------------------  
  6) О_СРОК   СРОК В ВИДЕ "1 ГОД 6 МЕСЯЦЕВ 20 ДНЕЙ"
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_СРОК:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

  DEF VAR c1 AS CHAR NO-UNDO.
  DEF VAR c2 AS CHAR NO-UNDO.
  DEF VAR yy AS INT64 NO-UNDO.
  DEF VAR mm AS INT64 NO-UNDO.
  DEF VAR dd AS INT64 NO-UNDO.
  DEF VAR i  AS INT64 NO-UNDO.

   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND loan WHERE RECID(loan) = loan-rid NO-LOCK.
    IF loan.end-date EQ ? THEN DO:
      c2 = "до востребования" .
    END. ELSE DO:  
      
      RUN period2dmy(local-dep-period, OUTPUT yy, OUTPUT mm, OUTPUT dd).
      c2 = "".
      IF yy > 0 THEN DO:
        i = yy.
        i = if (i ge 5 and i le 20) or i eq (INT64((i / 10)) * 10) then 3 else ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
        c2 = c2 + STRING(yy) + " " + dat_(i, "г") + " ".
      END.
      IF mm > 0 THEN DO:
        i = mm.
        i = if (i ge 5 and i le 20) or i eq (INT64((i / 10)) * 10) then 3 else ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
        c2 = c2 + STRING(mm) + " " + dat_(i, "м") + " ".
      END.
      IF dd > 0 THEN DO:
        i = dd.
        i = if (i ge 5 and i le 20) or i eq (INT64((i / 10)) * 10) then 3 else ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
        c2 = c2 + STRING(dd) + " " + dat_(i, "д").
      END.    
    END.  
    pick-value = trim(c2).                
  END. /* DO */
END PROCEDURE.
/*----------------------------------------------------------------------------------------------  
  7) О_ГАСИТЬ ДАТА ОКОНЧАНИЯ ДОГОВОРА (ДАТА ВЫДАЧИ МИНУС 1 ДЕНЬ)
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_ГАСИТЬ:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

  DEF VAR d AS DATE NO-UNDO. 
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    RUN deposit-end-date in h_dpspc (loan-rid, in-op-date, local-dep-period, OUTPUT d).
    pick-value = STRING(d).
  END. /* DO */
END PROCEDURE.



         /*--------------------------------------

            Процедуры, необходимые при обработке
            шаблоров счетов

         ---------------------------------------*/





/*----------------------------------------------------------------------------------------------  
  8) О_РЕЗИДЕНТ    возвращает 1, если вкладчик является резидентом РОССИИ
-----------------------------------------------------------------------------------------------*/
PROCEDURE о_резидент:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.
  DEF VAR vSett AS CHAR NO-UNDO.
   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND FIRST loan WHERE RECID(loan) = loan-rid NO-LOCK NO-ERROR.
    find FIRST person where person.person-id = loan.cust-id no-lock NO-ERROR.   
    vSett = FGetSetting("КодРез",?,"RUS"). /* по-умолчанию возращаем rus */
    pick-value = if person.country-id eq vSett then "1" else "0".
  END. /* DO */
END PROCEDURE.


/*----------------------------------------------------------------------------------------------  
  9) О_ВАЛЮТА    возвращает код валюты , в которой открывается вклад
-----------------------------------------------------------------------------------------------*/
PROCEDURE о_валюта:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    FIND loan WHERE RECID(loan) = loan-rid NO-LOCK.        
    pick-value = loan.currency.
    if pick-value eq "" then pick-value = FGetSettingEx("КодНацВал",?,?,YES).

  END. /* DO */
END PROCEDURE.


/*----------------------------------------------------------------------------------------------  
  10) О_ОТДЕЛЕНИЕ  возвращает код отделения , 
                   в контексте которого выполняется открытие вклада
                   или привязка счетов
                   Отделение берется из доп.реквизита пользователя "Отделение",
                   запустившего на выполнение процедуру, ИЛИ
                   из локальной переменной local-branch-id , если
                   ее значение не содержит неопределенного значения
-----------------------------------------------------------------------------------------------*/
PROCEDURE О_ОТДЕЛЕНИЕ:
  DEF INPUT PARAM rid         AS RECID NO-UNDO.
  DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.
  DEF INPUT PARAM param-count AS INT64   NO-UNDO.
  DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.

   
  pick-value = ?.
  DO ON ERROR UNDO, LEAVE:
    pick-value = 
    if local-branch-id = ? then 
      getThisUserXAttrValue("Отделение")
    else
      local-branch-id .
  END. /* DO */
 /* message "О_ОТДЕЛЕНИЕ = " pick-value view-as alert-box. */
END PROCEDURE.

/* /*----------------------------------------------------------------------------------------------  */
/*   Шаблон процедуры                                                                                */
/* -----------------------------------------------------------------------------------------------*/ */
/* PROCEDURE <Function-name>:                                                                        */
/*   DEF INPUT PARAM rid         AS RECID NO-UNDO.                                                   */
/*   DEF INPUT PARAM in-op-date  AS DATE  NO-UNDO.                                                   */
/*   DEF INPUT PARAM param-count AS INT64   NO-UNDO.                                                   */
/*   DEF INPUT PARAM param-str   AS CHAR  NO-UNDO.                                                   */
/*                                                                                                   */
/*                                                                                                   */
/*   pick-value = ?.                                                                                 */
/*   DO ON ERROR UNDO, LEAVE:                                                                        */
/*                                                                                                   */
/*                                                                                                   */
/*                                                                                                   */
/*   END. /* DO */                                                                                   */
/* END PROCEDURE.                                                                                    */




/*------------------------------------------------------------------------------
  Purpose:   "расшифровка" периода  
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
PROCEDURE period2dmy :
  DEFINE INPUT PARAM in-str AS CHAR.
  DEFINE OUTPUT PARAM out-yy AS INT64.
  DEFINE OUTPUT PARAM out-mm AS INT64.
  DEFINE OUTPUT PARAM out-dd AS INT64.

  DEF VAR j AS INT64.
  DEF VAR s2 AS CHAR.

  ASSIGN
    out-yy = 0
    out-mm = 0
    out-dd = 0
  .

  REPEAT j = 1 TO NUM-ENTRIES(in-str) :
    s2 = ENTRY(j, in-str).
    IF NUM-ENTRIES(s2, "=") = 2 THEN DO:
      CASE ENTRY(1, s2, "=") :
        WHEN "Г" THEN out-yy = INT64(ENTRY(2, s2, "=")) NO-ERROR.          
        WHEN "М" THEN out-mm = INT64(ENTRY(2, s2, "=")) NO-ERROR.
        WHEN "Д" THEN out-dd = INT64(ENTRY(2, s2, "=")) NO-ERROR.
      END CASE.
    END.
  END.
END PROCEDURE.
