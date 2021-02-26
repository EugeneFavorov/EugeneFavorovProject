/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  acct(crd.p
      Comment:  Просмотр и выбор внебалансовых счетов, работающих с указанным
                шаблоном КАУ. Входящий параметр - код шаблона кау. Исполь -
                зуется в процедуре списания с картотеки 2. Подключается через
                дополнительный реквизит шаблона проводки с кодом "ПроцКау".
                Счета выбираются по следующему алгоритму:
                  Выбираются все внебалансовые счета, работающие по указанному
                шаблону КАУ, за которые отвечает данный сотрудник или
                подчиненные ему сотрудники. На это накладываются по умолчанию
                ограничения - остаток на счете должен быть отличен от нуля ЗА
                эту дату и на соответствующем счете этого клиента на балансе
                НА дату должен быть остаток.
         Uses:  -
      Used by:  gcrddec.p
      Created:  15/09/1999 eagle
     modified: 08/05/2002 kostik  0006764 Формат балансового остатка, показ внебалансовых
                                          счетов с учетом прав пользователя


*/
{globals.i}
{sh-defs.i}
{chkacces.i}
{intrface.get "acct"}
{intrface.get blkob}
{intrface.get cust}
{intrface.get trans}
{intrface.get pbase}

FUNCTION GetBlockPositionAll RETURNS DECIMAL(INPUT iAcct     AS CHARACTER,
                                             INPUT iCurrency AS CHARACTER,
                                             INPUT iDate     AS DATE
                                            ) IN h_blkob.

def input parameter in-code-value like code.code no-undo.

DEF VAR mOnlyUser    AS LOG  NO-UNDO.
DEF VAR mCurrentUser AS CHAR NO-UNDO.
mCurrentUser = userid('bisquit') + "," + getslaves().

DEF VAR mAcctFltN  AS INT64 INIT 1 NO-UNDO.
DEF VAR mUsrBranch AS CHAR         NO-UNDO.

DEF VAR vAcctFlt AS CHARACTER FORMAT "X(23)" INITIAL "По счетам ОИ"
        VIEW-AS COMBO-BOX INNER-LINES 3
        LIST-ITEMS "По счетам ОИ","По счетам отделения ОИ", "По всем счетам" 
        DROP-DOWN-LIST
        NO-UNDO.



FORM
   vAcctFlt   LABEL  "Фильтр по счетам " 
              FORMAT "x(23)"
              HELP   "Фильтр по счетам картотеки блокированных счетов"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE "Постановка на картотеку".

ON GO OF FRAME frAcct DO:

   CASE vAcctFlt:SCREEN-VALUE IN FRAME frAcct:
      WHEN "По счетам ОИ" THEN
      DO:
         mAcctFltN = 1.
      END.
      WHEN "По счетам отделения ОИ" THEN
      DO:
         mUsrBranch = GetUserBranchID(userid("bisquit")).
         mAcctFltN = 2.
      END.
      WHEN "По всем счетам" THEN
      DO:
         mAcctFltN = 3.
      END.
   END CASE.
END.


&GLOB user-rights1 (   (    mOnlyUser                                 ~
                        AND LOOKUP(bfAcct.user-id,mCurrentUser) GT 0  ~
                        AND mAcctFltN EQ 0)                           ~
                    OR (    NOT mOnlyUser)                            ~
                    OR (    bfacct.user-id   EQ USERID("bisquit")     ~
                        AND mAcctFltN        EQ 1)                    ~
                    OR (    bfacct.branch-id EQ mUsrBranch            ~
                        AND mAcctFltN        EQ 2)                    ~
                    OR (    mAcctFltN        EQ 3)                    ~
                   )

IF NUM-ENTRIES(in-code-value,"#") GT 1 THEN
DO:
   IF ENTRY(2,in-code-value,"#") EQ "ShowForm" THEN
   DO:
      UPDATE vAcctFlt
      WITH FRAME frAcct.
   END.
   ELSE 
      mAcctFltN = 0.
 
   ASSIGN
       mOnlyUser = TRUE
       in-code-value = ENTRY(1,in-code-value,"#")
   .
END.
ELSE 
   mAcctFltN = 0.

def var vacct-cat   like acct.acct-cat no-undo.
def var long-acct   as char format "x(24)" no-undo.
def var comment_str as char label "                                   "  no-undo.
def var users       as char no-undo.
def var lstcont     as char initial "Расчет" no-undo.
def buffer bacct for acct.
def var summ-rr like op-entry.amt-rub no-undo.
def var vdebug as log init no no-undo.
def var ff-card as char no-undo.
def var AccessStat  as char no-undo.
def var AccessAcct  as char no-undo.
def var mTmplCurr   as char no-undo.
DEF VAR name AS CHARACTER EXTENT 2 NO-UNDO.
DEF VAR Store-Position AS RECID NO-UNDO.

DEFINE VARIABLE mNF AS INTEGER   NO-UNDO.

DEF TEMP-TABLE ttacct NO-UNDO
   FIELD acct        LIKE acct.acct
   FIELD acct-view   LIKE acct.acct  /* Номер счета в соответствующем формате */
   FIELD bacct       LIKE acct.acct
   FIELD bacct-view  LIKE acct.acct  /* Номер счета в соответствующем формате */
   FIELD curr        LIKE acct.curr
   FIELD bcurr       LIKE acct.curr /* В ЭМБ к валютным балансовым счетам привязываются рублевые внебалансовые*/
   FIELD name        AS   CHARACTER LABEL "Клиент"
                             FORMAT "x(40)"
   FIELD rec-oa      AS   RECID /* для ссылки на внебалансовый счет */
   FIELD obal        LIKE acct-pos.balance
   FIELD fobal       AS   LOG
   FIELD rec-ba      AS   RECID /* для ссылки на балансовый счет */
   FIELD bbal        LIKE acct-pos.balance
   FIELD fbbal       AS   LOG
   FIELD bbal-rub    LIKE acct-pos.balance /* для проверок на превышение лимита */
   FIELD bbal-val    LIKE acct-pos.balance
   FIELD blk-type    AS   CHARACTER LABEL "Блокировка"
   FIELD blk-amt     AS   decimal   LABEL "Блок. сумма"
   FIELD blk-cust    AS   CHARACTER LABEL "Блок. клиента"
INDEX acct   IS PRIMARY acct curr bacct
INDEX wacct             fobal fbbal acct curr bacct
.

find code where code.class eq "ШаблКау"
            and code.code  eq in-code-value
                           no-lock no-error.
if not avail code then return.
{kautools.lib}
ff-card = FGetSetting("СтандТр", "findcard2", "Нет").
AccessStat = FGetSetting("СтандТр", "AccessStatus", "П").
AccessAcct = FGetSetting("СтандТр", "AccessAcct", "").

mTmplCurr = GetSysConf("ВалБалСч").
if mTmplCurr eq ? then mTmplCurr = "*".

/*-------------------------------------------------*/
/* Возвращает самую приоритетную блокировку счета. 
   Если такой нет, вернет "". */
/*-------------------------------------------------*/
FUNCTION GetMainBlock RETURNS CHARACTER (
    INPUT iAcct     AS CHARACTER,
    INPUT iCurrency AS CHARACTER,
    INPUT iDateTime AS DATETIME):

    DEFINE VARIABLE vBlockList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vPrevBlock AS CHARACTER NO-UNDO.

    DEFINE BUFFER acct FOR acct.
    DEFINE BUFFER BlockObject FOR BlockObject.

    DEFINE QUERY BlockObjectQuery FOR BlockObject.

    FIND FIRST acct WHERE
               acct.acct     EQ iAcct
           AND acct.currency EQ iCurrency
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE acct THEN
        RETURN "".

    OPEN QUERY BlockObjectQuery
        FOR EACH  BlockObject WHERE
                  BlockObject.class-code   EQ "BlockAcct"
             AND  BlockObject.file-name    EQ "acct"
             AND  BlockObject.surrogate    EQ acct.acct + "," + acct.currency
             AND  BlockObject.beg-datetime LE iDateTime
             AND (BlockObject.end-datetime EQ ?
               OR BlockObject.end-datetime GE iDateTime)
        NO-LOCK.

    GET FIRST BlockObjectQuery.
    IF AVAILABLE BlockObject THEN
	RETURN "Блокирован".
    ELSE
        RETURN "".

    CLOSE QUERY BlockObjectQuery.

/*
    /* если блокировки разные, то надо вернуть "" */
    REPEAT:
        GET NEXT BlockObjectQuery.
        IF QUERY-OFF-END("BlockObjectQuery") THEN
            LEAVE.
        IF vPrevBlock NE BlockObject.txt[1] THEN
            LEAVE.
        vPrevBlock = BlockObject.txt[1].
    END.

    CLOSE QUERY BlockObjectQuery.

    /* если блокировки с одинаковыми наборами полей - считаем по приоритетам блокировок */
    vBlockList = BlockAcct(acct.acct + ',' + acct.currency,iDateTime).

    IF vPrevBlock EQ ? THEN
        vPrevBlock = "".

    IF CAN-DO(vBlockList,"Блок") THEN
        RETURN "Блок:" + vPrevBlock.
    IF CAN-DO(vBlockList,"БлокДб") AND CAN-DO(vBlockList,"БлокКр") THEN
        RETURN (IF acct.side EQ "П" THEN "БлокДб:" ELSE "БлокКр:") + vPrevBlock.
    IF CAN-DO(vBlockList,"БлокДб") THEN
        RETURN "БлокДб:" + vPrevBlock.
    IF CAN-DO(vBlockList,"БлокКр") THEN
        RETURN "БлокКр:" + vPrevBlock.
    IF CAN-DO(vBlockList,"БлокСумм") THEN
        RETURN "БлокСумм:" + vPrevBlock.

    RETURN "".
*/
END FUNCTION.

&GLOB PROC-ACCT                                                              ~
  run fdbacct( buffer acct, ff-card, in-code-value ).                        ~
  for each buf-ttKau WHERE buf-ttKau.fTbName EQ "ACCTB" NO-LOCK,             ~
    FIRST bacct WHERE RECID(bacct) EQ buf-ttKau.fRecId NO-LOCK               ~
    break by bacct.acct :                                                    ~
      CREATE ttacct.                                                         ~
      ASSIGN                                                                 ~
        ttacct.acct-view   = STRING(acct.acct,GetAcctFmt(code.misc[8]))      ~
        ttacct.rec-oa      = recid(acct)                                     ~
        ttacct.acct        = acct.acct                                       ~
        ttacct.curr        = acct.curr                                       ~
      .                                                                      ~
      RUN acct-pos IN h_base (ttacct.acct,                                   ~
                              ttacct.curr,                                   ~
                              gend-date,                                     ~
                              gend-date,                                     ~
                              "П").                                          ~
      ASSIGN                                                                 ~
        ttacct.obal  = IF ttacct.curr > "" THEN sh-val                       ~
                                            ELSE sh-bal                      ~
        ttacct.fobal = ttacct.obal NE 0                                      ~
      .                                                                      ~
      ASSIGN                                                                 ~
          ttacct.rec-ba = recid(bacct)                                       ~
      .                                                                      ~
         RUN acct-pos IN h_base (bacct.acct,                                 ~
                                 bacct.curr,                                 ~
                                 gend-date,                                  ~
                                 gend-date,                                  ~
                                 if can-do(AccessAcct, bacct.acct) then AccessStat else "П" ).                                  ~
          ttacct.blk-type = GetMainBlock(bAcct.acct,                                                                            ~
                                         bAcct.currency,                                                                        ~
                                         (IF gend-date EQ TODAY THEN DATETIME(TODAY,MTIME) ELSE DATETIME(gend-date + 1)) - 1).  ~
          IF ttacct.blk-type NE "" AND ttacct.blk-type NE ? THEN ttacct.blk-type = "Блокирован".         ~
          ttacct.blk-amt  = GetBlockPositionAll(bAcct.acct, bAcct.currency, gend-date).    ~
          ttacct.blk-cust = ClientXattrVal(bAcct.cust-cat, bAcct.cust-id, "Блок").   ~
                                                                                     ~
      ASSIGN                                                                         ~
          ttacct.bacct-view = STRING(bacct.acct,GetAcctFmt(code.misc[8]))            ~
          ttacct.bacct      = bacct.acct                                             ~
          ttacct.bcurr      = bacct.curr                                             ~
          ttacct.rec-ba     = RECID(bacct)                                           ~
          ttacct.name       = IF acct.cust-cat EQ "Ю" THEN (name[1] + " " + name[2]) ~
                                                  ELSE (name[1] +  " " + name[2])    ~
          ttacct.bbal       = IF bacct.curr  GT "" THEN sh-val ELSE sh-bal           ~
          ttacct.fobal      = IF ttacct.obal GT 0  THEN yes    ELSE no               ~
          ttacct.fbbal      = IF ttacct.bbal LT 0  THEN yes    ELSE no               ~
          ttacct.bbal-rub   = sh-bal                                                 ~
          ttacct.bbal-val   = sh-val                                                 ~
      .                                                                              ~
  END.


RUN SelectAcctOfKauId(code.code).
/* Заполнение временной таблицы */
FOR EACH ttKau WHERE ttKau.fTbName EQ "ACCT" NO-LOCK,
    FIRST acct WHERE RECID(acct) EQ ttKau.fRecId NO-LOCK:
   {getcust.i &name=name &Offinn="/*"}
   {&PROC-ACCT}

END.


FORM
   ttacct.acct-view FORMAT "x(25)"
               HELP "Внебалансовый счет"
               SPACE(5)
   ttacct.obal COLUMN-LABEL "ОСТАТОК НА!ВНЕБАЛАНСОВОМ СЧЕТЕ"
               HELP "Остаток на внебалансовом счете"
   ttacct.bbal COLUMN-LABEL "ОСТАТОК НА!БАЛАНСОВОМ СЧЕТЕ"
               HELP "Остаток на балансовом счете"
WITH FRAME BROWSE1
     TITLE COLOR BRIGHT-WHITE "[ ЛИЦЕВЫЕ СЧЕТА ПО СОСТОЯНИЮ НА " + STRING(gend-date) + " ]"
     WIDTH 79.

FORM
   ttacct.acct-view  FORMAT "x(25)"
                HELP "Внебалансовый счет"
   ttacct.bacct-view FORMAT "x(25)"
                COLUMN-LABEL  "!СЧЕТ НА БАЛАНСЕ"
                HELP "Балансовый счет"
   ttacct.obal  COLUMN-LABEL "ОСТАТОК НА!ВНЕБАЛАНСОВОМ СЧЕТЕ"
                HELP "Остаток на внебалансовом счете"
WITH FRAME BROWSE2
     TITLE COLOR BRIGHT-WHITE "[ ЛИЦЕВЫЕ СЧЕТА ПО СОСТОЯНИЮ НА " + STRING(gend-date) + " ]"
     WIDTH 79.

FORM
   ttacct.acct-view  FORMAT "x(25)"
                HELP "Внебалансовый счет"
   ttacct.name  FORMAT "x(47)"
                COLUMN-LABEL "!НАИМЕНОВАНИЕ СЧЕТА"
                HELP "Наименование (владельца) лицевого счета"
WITH FRAME BROWSE3
     TITLE COLOR BRIGHT-WHITE "[ ЛИЦЕВЫЕ СЧЕТА ]"
     WIDTH 79.

FORM
   ttacct.bacct-view FORMAT "x(25)"
                COLUMN-LABEL  "!СЧЕТ НА БАЛАНСЕ"
                HELP "Балансовый счет"
   ttacct.bbal  
                COLUMN-LABEL "ОСТАТОК НА!БАЛАНСОВОМ СЧЕТЕ"
                HELP "Остаток на балансовом счете"
   ttacct.blk-type FORMAT "x(20)"
                COLUMN-LABEL "Признак!блокировки счета"
                HELP "Признак блокировки балансового счета"
   ttacct.blk-amt FORMAT "->>>,>>>,>>9.99" 
                COLUMN-LABEL "Сумма!блокировки"
                HELP "Сумма блокировки балансового счета"
   ttacct.blk-cust FORMAT "x(7)"
                COLUMN-LABEL "Блок.!клиента"
                HELP "Наличие блокировки клиента"
WITH FRAME BROWSE4
     TITLE COLOR BRIGHT-WHITE "[ ЛИЦЕВЫЕ СЧЕТА ПО СОСТОЯНИЮ НА " + STRING(gend-date) + " ]"
     WIDTH 115.

&glob oqry0 open query qry0 for each ttacct where ttacct.fobal and ttacct.fbbal and can-do(mTmplCurr, ttacct.bcurr) no-lock.
&glob oqry1 open query qry0 for each ttacct where ttacct.fobal and can-do(mTmplCurr, ttacct.bcurr) no-lock.
&glob oqry2 open query qry0 for each ttacct where can-do(mTmplCurr, ttacct.bcurr) no-lock.

release ttacct.


mNF = 4.

{navigate.cqr
   &file     = ttacct
   &files    = "ttacct"
   &qry      = "qry0"
   &maxoq    = 3
   &avfile   = "ttacct "
   &defquery = "def query qry0 for ttacct scrolling."
   &maxfrm   = 4
   &bf1      = "ttacct.acct-view ttacct.obal ttacct.bbal "
   &bf2      = "ttacct.acct-view ttacct.bacct-view ttacct.obal "
   &bf3      = "ttacct.acct-view ttacct.name "
   &bf4      = "ttacct.bacct-view ttacct.bbal ttacct.blk-type ttacct.blk-amt ttacct.blk-cust"
   &first-frm = mNF
   &workfile = "/*"
   &nodel    = "/*"
   &look     = "acct(crd.nav "
   &return   = "acct(crd.ret "
   &oh3      = "│F3"
   &oth3     = "acct(crd.f3 "
   &oh6      = "│F6"
   &oth6     = "acct(crd.f6 "
   &oh2      = "│F2-Детализация"
   &oth2     = "acct(crd.f2 "
   &oh7      = "│F7"
   &oth7     = "findsp.cqr "
     &find1  = "searchsp.cqr  &file-name = ttacct   ~
                              &sfld      = acct     ~
                              &metod     = matches  ~
                              &metmatch  = YES      ~
               "             
     &find2  = "searchsp.cqr  &file-name = ttacct   ~
                              &sfld      = bacct    ~
                              &metod     = matches  ~
                              &metmatch  = YES      ~
               "             
     &find3  = "searchsp.cqr  &file-name = ttacct   ~
                              &sfld      = name     ~
                              &metod     = matches  ~
                              &metmatch  = YES      ~
               "             

}

/*
   &n-str=num-line

   &oh2="│F3 форма"
   &oth2="op-frm.chg "
*/
{intrface.del "acct"}
RETURN.
