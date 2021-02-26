/*
     Банковская интегрированная система БИСквит
      Filename: svodlst.p
      Comment: Ведомость документов пользователя\электронного хранения\подразделения
      Parameters:
      Uses:
      Used by:
      Created: ayv
      Настроечные параметры: БухЖур(b), БухЖур(o)
      Классификаторы: elhran(elhran.i/def), ХозДок(hozdoc.i/def), ТитПодразд  
*/

{globals.i}

DEF INPUT PARAM iStr AS CHAR NO-UNDO.
DEF VAR iUserInp AS CHAR NO-UNDO. /*пользователь для ведомости*/
DEF VAR iHozS    AS CHAR NO-UNDO. /*тип сшива для хозяйственных документов */
/* Типы сшивов:                                                */
/* Агент - агентские вознаграждения                            */
/* Хоздок - хозяйственные документы                            */
/* Подотчет - подотчетные суммы                                */
/* Докдн - документы дня                                       */

DEF VAR fname  AS CHAR NO-UNDO.
DEF NEW SHARED STREAM vvs.

DEF VAR iFil    AS CHAR            NO-UNDO. /*код подразделения*/
DEF VAR iUserID AS CHAR  INIT ''   NO-UNDO. /*код пользователя*/
DEF VAR tUserID AS CHAR            NO-UNDO. 
DEF VAR tShiv   AS CHAR            NO-UNDO.
DEF VAR tNull   AS LOG   INIT TRUE NO-UNDO. /*есть ли проведенные пользователем документы*/
DEF VAR tNotA   AS LOG   INIT TRUE NO-UNDO. /*проводка должна хранится в бумаге, но создана SERV'ом*/
DEF VAR tStr    AS CHAR            NO-UNDO.
DEF VAR i       AS INT64           NO-UNDO.

DEF VAR tServ   AS CHAR            NO-UNDO.

DEF VAR vUserName AS CHAR NO-UNDO.
DEF VAR vUserOtd  AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-op LIKE op.
DEF BUFFER b-code FOR code.

/* для работы op-cash*/
DEF VAR adb     AS CHAR    NO-UNDO. 
DEF VAR acr     AS CHAR    NO-UNDO.
DEF VAR isCash  AS LOGICAL NO-UNDO.

/*для svodxls.i*/
DEF VAR vSum-b     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение - балансовые*/
DEF VAR vSum-o     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение - внебалансовые*/
DEF VAR vSum-b-val LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение - балансовые    - валюта*/
DEF VAR vSum-o-val LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение - внебалансовые - валюта*/

/*для svodallxls.i*/
DEF VAR vSum-b-p     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*документы на бумаге  - балансовые*/
DEF VAR vSum-o-p     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*документы на бумаге  - внебалансовые*/
DEF VAR vSum-b-p-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*документы на бумаге в валюте - балансовые*/
DEF VAR vSum-o-p-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*документы на бумаге в валюте - внебалансовые*/
DEF VAR vSum-b-e     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение - балансовые*/
DEF VAR vSum-o-e     LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение - внебалансовые*/
DEF VAR vSum-b-e-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение в валюте - балансовые*/
DEF VAR vSum-o-e-v   LIKE op-entry.amt-rub INIT 0.00 NO-UNDO. /*электронное хранение в валюте - внебалансовые*/

DEF TEMP-TABLE tt-op-day NO-UNDO     /*проводки за операционный день*/
  FIELD op-cid    AS   RECID              /*op*/
  FIELD ope-cid   AS   RECID              /*op-entry*/
  FIELD city-id   AS   INTEGER            /*гб или нет*/
  FIELD city      AS   CHARACTER          /*имя отделения*/
  FIELD kko       AS   CHARACTER          /*код отделения*/
  FIELD currency  LIKE op-entry.currency  /*валюта*/
  FIELD acct-cat  LIKE op-entry.acct-cat  /*баланс\внебаланс*/
  FIELD razdel    AS   CHARACTER          /*кассовый ли документ*/
  FIELD save-type AS   CHARACTER          /*вид хранения (1 - бумага, 2 - электронно)*/
  FIELD hozdoc    AS   CHARACTER          /*принадлежность к хоз.документам (определенный тип или все документы)*/
  FIELD acct-dbf  AS   CHARACTER          /*счет второго порядка дебета(для сортировки)*/
  FIELD acct-dbl  AS   CHARACTER          /*последние 5 цифр дебета(для сортировки)*/
  FIELD acct-db   LIKE op-entry.acct-db   /*дебит*/
  FIELD acct-cr   LIKE op-entry.acct-cr   /*кредит*/
  FIELD doc-num   LIKE op.doc-num         /*номер документа*/
  FIELD doc-numt  LIKE op.doc-num         /*номер документа для сортировки*/
  FIELD doc-type  LIKE op.doc-type        /*тип документа*/
  FIELD amt-rub   LIKE op-entry.amt-rub   /*сумма в рублях*/
  FIELD amt-cur   LIKE op-entry.amt-cur   /*сумма в рублях*/
  FIELD op        LIKE op.op              /**/
  FIELD op-kind   LIKE op.op-kind         /*код транзакции*/
  FIELD op-trans  LIKE op.op-transaction  /*номер транзакции*/
  FIELD doc-date  LIKE op.doc-date        /*дата создания*/
  FIELD op-date   LIKE op.op-date         /*дата проводки*/
  FIELD user-id   LIKE op.user-id         /*пользователь*/
  FIELD user-name AS   CHARACTER          /*ФИО пользователя*/
  FIELD branch-id LIKE op.branch-id       /*branch-id проводки*/
INDEX graph save-type currency acct-db acct-dbf acct-dbl doc-numt.

/* заполняем входящие параметры */
iUserInp = ENTRY(1,iStr,';').
iHozS    = ENTRY(2,iStr,';').

{getdate.i}

/* переменные для определения кассового документа */
{op-cash.def}

/* инициализация справочника эл.док-ов */
{elhran.def}

/* инициализация справочника хоз.док-ов */
{hozdoc.def}

/* выбираем пользователя для ведомости */
CASE iUserInp:

  WHEN 'serv' THEN DO:

  /* отрисовываем форму */
    PAUSE 0.

    DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune3:
    UPDATE
      iFil LABEL "Код подразделения" HELP "Введите код подразделения"   
    WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
    TITLE "[ Параметры Отчета ]".
    END.
    HIDE FRAME ftune3 NO-PAUSE.

    IF LASTKEY EQ KEYCODE("ESC") THEN
      RETURN.

    FIND FIRST code WHERE code.class EQ 'ТитПодразд'
                      AND   code.code  EQ  iFil 
    NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN 
      DO:
        IF code.misc[3] EQ iFil THEN
          vUserOtd = code.name.
        ELSE DO:
          FIND FIRST b-code WHERE b-code.class EQ 'ТитПодразд'
                    AND   b-code.code EQ iFil.
          IF AVAIL(b-code) THEN
            vUserOtd = b-code.name.
        END.
      END.

   
    /* ayv 01.09.2015 добавляем IRBIS, BIS и _SERV к SERV */
    ASSIGN
    iUserID = '*SERV*,IRBIS,BIS,SYNC'
    tUserID = 'SERV' + shFilial.
  
  END. /*serv */

  /* выбор пользователя */
  WHEN 'user' THEN DO:

    /* отрисовываем форму */
    PAUSE 0.
    
    DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune:
    UPDATE
      iUserID LABEL "Код пользователя" HELP "Введите код пользователя для отчета"   
    WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
    TITLE "[ Параметры Отчета ]".
    END.
    HIDE FRAME ftune NO-PAUSE.

    IF LASTKEY EQ KEYCODE("ESC") THEN
      RETURN.

    /*заполняем*/
    FIND FIRST _user WHERE _user._userid BEGINS iUserID NO-LOCK NO-ERROR.
    IF AVAIL(_user) THEN DO:
      ASSIGN
        vUserName = _user._user-name
        iUserID   = _user._userid + ',*SERV*,SYNC'.
        tUserID   = _user._userid.

      /*ищем подразделения пользователя*/
      iFil      = GetXattrValueEx("_user",STRING(_user._userid),"Отделение","").

      FIND FIRST code WHERE code.class EQ 'ТитПодразд'
                      AND   code.code EQ  iFil 
          NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN DO:
        IF code.misc[3] EQ shFilial THEN
          vUserOtd = code.name.
        ELSE DO:
          FIND FIRST b-code WHERE b-code.class EQ 'ТитПодразд'
                    AND   b-code.code EQ shFilial.
          IF AVAIL(b-code) THEN
            vUserOtd = b-code.name.
        END.
      END.

    END.

  END. /* user */

  /* пользователь, вызвавший процедуру */
  WHEN 'self' THEN DO:

    FIND FIRST _user WHERE _user._userid EQ userid("bisquit") NO-LOCK NO-ERROR.
    IF AVAIL(_user) THEN DO:
      ASSIGN
        vUserName = _user._user-name
        iUserID   = _user._userid + ',*SERV*,SYNC'.
        tUserID   = _user._userid.

      /*ищем подразделение пользователя*/
      iFil      = GetXattrValueEx("_user",STRING(_user._userid),"Отделение","").

      FIND FIRST code WHERE code.class EQ 'ТитПодразд'
                      AND   code.code  EQ  iFil 
          NO-LOCK NO-ERROR.
      IF AVAIL(code) THEN DO:
        IF code.misc[3] EQ shFilial THEN
          vUserOtd = code.name.
        ELSE DO:
          FIND FIRST b-code WHERE b-code.class EQ 'ТитПодразд'
                    AND   b-code.code EQ shFilial.
          IF AVAIL(b-code) THEN
            vUserOtd = b-code.name.
        END.
      END.

    END.

  END. /*self*/

  /* сводная ведомость для подразделения */
  WHEN 'all' THEN DO:

    /* отрисовываем форму */
    PAUSE 0.

    DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE WITH FRAME ftune2:
    UPDATE
      iFil LABEL "Код подразделения" HELP "Введите код подразделения"   
    WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COL
    TITLE "[ Параметры Отчета ]".
    END.
    HIDE FRAME ftune2 NO-PAUSE.

    IF LASTKEY EQ KEYCODE("ESC") THEN
      RETURN.

    /* ищем код подразделения */
    FIND FIRST code WHERE code.class EQ 'ТитПодразд'
                    AND   code.code  EQ iFil
    NO-LOCK NO-ERROR.  
    IF NOT AVAIL(code) THEN
      MESSAGE 'Неверный код подразделения!' VIEW-AS ALERT-BOX ERROR.

    FIND FIRST branch WHERE branch.branch-id EQ iFil NO-LOCK NO-ERROR.
    IF AVAIL(branch) THEN
      vUserOtd = branch.name.

  END. /* all */

END CASE.

/* Создаем временную таблицу tt-op */
FOR EACH op
   WHERE op.op-date EQ end-date
   AND   CAN-DO("√*,ФДД",op.op-status)
   AND   op.acct-cat NE 'd'
   AND   op.filial-id EQ shFilial
NO-LOCK:
  CREATE tt-op.
  BUFFER-COPY op TO tt-op.
END.

/*===========================================================================*/
FOR EACH tt-op 
   WHERE tt-op.user-inspector NE ''
NO-LOCK:
  FOR EACH  op-entry 
      WHERE op-entry.op EQ tt-op.op
      AND   op-entry.op-date EQ end-date
  NO-LOCK:

    ASSIGN
      adb = op-entry.acct-db
      acr = op-entry.acct-cr
    .
        
    /* определение кассового документа */
    {op-cash.i}
    
    /* определение электронного документа */
    {elhran.i}  
      
    /* переносим документ SERV к контроллеру, если он в бумаге */
    IF isAuto AND NOT isElH AND tt-op.user-inspector NE '' THEN
      DO:
        ASSIGN
          tt-op.user-id = tt-op.user-inspector
          tt-op.branch-id = GetTempXattrValueEx('_user', tt-op.user-id, 'ОтделениеТемп', end-date, '')
        .
        IF tt-op.branch-id EQ '' THEN 
          tt-op.branch-id = GetXattrValueEx('_user', tt-op.user-id, 'office', '').
      END.
  END.
END.

FOR EACH tt-op 
   WHERE tt-op.filial-id EQ '0500'
   AND   tt-op.branch-id EQ '0000'
   OR    CAN-DO('*SERV*,IRBIS,BIS,SYNC', tt-op.user-id)
NO-LOCK:
  ASSIGN
    tt-op.branch-id = tt-op.filial-id
  .
END.

/*
IF end-date EQ DATE(01/23/17) THEN 
DO:
  FOR EACH tt-op 
     WHERE CAN-DO('41740800,41742137', STRING(tt-op.op))
  NO-LOCK:
    ASSIGN
      tt-op.branch-id = '0503'
    .
  END.
END.

IF end-date EQ DATE(01/10/17) THEN 
DO:
  FOR EACH tt-op 
     WHERE tt-op.user-id EQ 'NSK_LTA'
     AND   tt-op.op-date EQ end-date
  NO-LOCK:
    ASSIGN
      tt-op.branch-id = '0517'
    .
  END.
END.
*/

FOR EACH tt-op 
  WHERE CAN-DO('0500LEK,0500FTA,0500RVG,0500STA,OU_CNV,OU_DOP,OU_GTV,OU_KNA,0500GEF,0500SUTA,0500SES,A0503KKB,0500MVY', tt-op.user-id) 
  AND   CAN-DO('0518,0598', tt-op.branch-id)
NO-LOCK:
  ASSIGN
    tt-op.branch-id = '0500'
  .
END.
/*===========================================================================*/

/* отбираем проводки */
FOR EACH  tt-op 
    WHERE tt-op.filial-id EQ shFilial
    AND   CAN-DO(iUserID,tt-op.user-id)
    AND   iUserInp NE "ALL"
     OR
     (tt-op.branch-id EQ iFil 
     AND iUserInp EQ "ALL")
NO-LOCK:
  
  FOR EACH  op-entry 
      WHERE op-entry.op EQ tt-op.op
      AND   op-entry.op-date EQ end-date
  NO-LOCK:

    ASSIGN
      adb = op-entry.acct-db
      acr = op-entry.acct-cr
    .
        
    /* определение кассового документа */
    {op-cash.i}
    
    /* определение электронного документа */
    {elhran.i}  
      
    /* переносим документ SERV к контроллеру, если он в бумаге */
    IF isAuto AND NOT isElH AND tt-op.user-inspector NE '' THEN
      DO:
        ASSIGN
          tNotA = TRUE
          tStr  = tt-op.user-inspector
          tt-op.branch-id = GetTempXattrValueEx('_user', tStr, 'ОтделениеТемп', end-date, '')
        .
        IF tt-op.branch-id EQ '' THEN 
          tt-op.branch-id = GetXattrValueEx('_user', tStr, 'office', '').
      END.
    ELSE
      ASSIGN
        tNotA = FALSE
        tStr = tt-op.user-id
      .

    /* формируем список пользователей по документам для общего реестра*/
    IF iUserInp EQ "ALL" THEN
    DO:
       IF LOOKUP(tStr, tUserID) = 0 THEN tUserID = tUserID + "," + tStr.
    END.

    /* находим фио сотрудника для сводной ведомости */
    FIND FIRST _user 
      WHERE _user._userid EQ tStr 
    NO-LOCK NO-ERROR.
    IF AVAIL(_user) THEN
      
      IF CAN-DO('*SERV*,IRBIS,BIS,QBIS,SYNC',tStr) THEN
        tStr = 'Автоматические транзакции'.
      ELSE
        tStr = _user._user-name.

    CREATE tt-op-day.
    ASSIGN
      tt-op-day.op-cid    = RECID(op)
      tt-op-day.ope-cid   = RECID(op-entry)
      tt-op-day.razdel    = IF isCash THEN "k" ELSE "b"
      tt-op-day.currency  = op-entry.currency
      tt-op-day.acct-cat  = IF tt-op.acct-cat EQ 'b' THEN tt-op.acct-cat ELSE 'o'
      tt-op-day.acct-dbf  = SUBSTRING(op-entry.acct-db,1,5)
      tt-op-day.acct-dbl  = SUBSTRING(op-entry.acct-db,16,5)
      tt-op-day.acct-db   = op-entry.acct-db
      tt-op-day.acct-cr   = op-entry.acct-cr
      tt-op-day.doc-num   = tt-op.doc-num
      tt-op-day.doc-numt  = FILL('0',20 - LENGTH(tt-op.doc-num)) + tt-op.doc-num
      tt-op-day.doc-type  = tt-op.doc-type
      tt-op-day.amt-rub   = IF op-entry.acct-db EQ ? THEN 0.00 ELSE op-entry.amt-rub
      tt-op-day.amt-cur   = IF op-entry.acct-db EQ ? THEN 0.00 ELSE op-entry.amt-cur
      tt-op-day.save-type = IF isElH THEN "2" ELSE "1"                      
      tt-op-day.hozdoc    = '-'
      tt-op-day.op        = tt-op.op
      tt-op-day.op-kind   = tt-op.op-kind
      tt-op-day.op-trans  = tt-op.op-transaction
      tt-op-day.doc-date  = tt-op.doc-date
      tt-op-day.op-date   = tt-op.op-date
      tt-op-day.user-id   = IF tNotA THEN tt-op.user-inspector ELSE (IF iUserInp EQ 'serv' THEN 'SERV' + shFilial ELSE tt-op.user-id)
      tt-op-day.user-name = IF AVAIL(_user) THEN tStr ELSE tt-op.user-id
      tt-op-day.branch-id = tt-op.branch-id
    .
  
    /* определение сшива */
    DO i = 1 TO NUM-ENTRIES(iHozS):
  
      {hozdoc.i &hoz-type = ENTRY(i,iHozS)}
      /* при ДокДн отмечаем все документы, не входящие в классификатор ХозДок */
      IF ENTRY(i,iHozS) = 'Докдн' THEN DO:
        IF NOT isHoz THEN DO:
          tt-op-day.hozdoc = ENTRY(i,iHozS).
          LEAVE.
        END.
      END.
      ELSE DO:
        IF isHoz THEN DO:
          tt-op-day.hozdoc = ENTRY(i,iHozS).
          LEAVE.
        END.
      END.
    
    END. /* do */

  END. /* for each op-entry */

END. /* for each op */

/* {setdest.i &file-name = "111.log"}
FOR EACH tt-op-day NO-LOCK:
  PUT UNFORMATTED tt-op-day.op ";" tt-op-day.user-id ";" tt-op-day.user-name ";" tt-op-day.branch-id ";" tt-op-day.currency ";" tt-op-day.acct-cat ";" tt-op-day.razdel ";" tt-op-day.save-type ";" tt-op-day.hozdoc ";" tt-op-day.amt-rub SKIP.
END.
{preview.i &file-name = "111.log"} */

/* {setdest.i &file-name = "111.log"}
FOR EACH tt-op-day NO-LOCK:
  PUT UNFORMATTED tt-op-day.razdel ";" tt-op-day.currency ";" tt-op-day.acct-cat ";" tt-op-day.acct-dbf ";" tt-op-day.acct-dbl ";" tt-op-day.acct-db ";" tt-op-day.acct-cr ";" tt-op-day.doc-num ";" tt-op-day.doc-numt ";" tt-op-day.doc-type ";" tt-op-day.amt-rub ";" tt-op-day.amt-cur ";" tt-op-day.save-type ";" tt-op-day.hozdoc ";" tt-op-day.op ";" tt-op-day.op-kind ";" tt-op-day.op-trans ";" tt-op-day.doc-date ";" tt-op-day.op-date ";" tt-op-day.user-id ";" tt-op-day.user-name ";" tt-op-day.branch-id SKIP.
END.
{preview.i &file-name = "111.log"} */

/* печатаем сводную ведомость или по пользователю */
IF iUserInp EQ 'all' THEN DO:
  ASSIGN
    tUserID = TRIM(tUserID,",")
    iUserID = tUserID
    iUserID = iUserID + ',*SERV*'
    tServ = 'SERV' + shFilial
  .

  {svodallxls.i}
END.
ELSE DO:
  {svodxls0500.i}
END.