/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ЗАО "Банковские информационные системы"
     Filename: card2inc.p
      Comment: Постановка на картотеку2
   Parameters:
         Uses:
      Used by:
      Created: xx/xx/xxxx
     Modified: 06/03/2003 kraw (0012006) Карт(x)ВНСчет может содержать валюту
     Modified: 26/05/2003 kolal  - Вызов формы редактирования налоговых
                                   реквизитов. Заявка 16535.
     Modified: 11/02/2004 kraw (0025554) При частичной постановке на картотеку - принудительно все даты
                                         в текущую
     Modified: 30/12/2005 kraw (0052869) При частичной постановке на картотеку - Все равно формировать анулированный
     Modified: 24/07/2006 kraw (0065156) Копирование налоговых реквизитов при частичной постановке
     Modified: 03/08/2006 kraw (0065317) Код документа при частичной постановке не жестко "016", и ищем по коду ЦБ "16"
     Modified: 10/09/2007 kraw (0081631) если полная постановка (RECID(xop-entry) EQ RECID(xop-entry1)), 
                                         запоминаем исходную сумму в ДР балансового.
     Modified: 19/10/2007 kraw (0082741) Для счета со статусом "БлокСумм" только постановка всей суммы.
     Modified: 13/02/2008 kraw (0084535) op-status для внебалансового документа берется безусловно из шаблона
     Modified: 17/07/2008 kraw (0094566) откат 0082741 и учет блокированных сумм
     Modified: 02/12/2008 Rig  (0103556) Запрет выбора частичной или полной постановки документов на картотеку по настроечному праметру
                                         КартКонСумм. Значение ДА - выбора нет: суммы документов ставятся целиком, НЕТ - появляется
                                         возможность выбирать ставить сууму целиком или только недостающую часть.
*/
{globals.i}
{history.def}

{defopupd.i}
{defoptr.i}
{defwrkop.i new}
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{bank-id.i}
{kautools.lib}
{intrface.get op}       /* Библиотека для работы с документами. */
{intrface.get acct}
{intrface.get blkob}
{intrface.get instrum}
{copyxtr.i}

DEFINE OUTPUT PARAM flager     AS INT64 NO-UNDO.
DEFINE INPUT  PARAM in-op-date AS DATE    NO-UNDO.
DEFINE INPUT  PARAM op-en-rid  AS RECID   NO-UNDO.
DEFINE INPUT  PARAM rid1       AS RECID   NO-UNDO.
DEFINE INPUT  PARAM rid2       AS RECID   NO-UNDO.


DEFINE BUFFER aop-entry FOR op-entry.  /* 2-я проводка транзакции  */
DEFINE BUFFER aop       FOR op.        /* 2-й документ транзакции  */
DEFINE BUFFER buf-op    FOR op.   /* Анулируемый документ */
DEFINE BUFFER xop-entry FOR op-entry.
DEFINE BUFFER buf-op1    FOR op.  /* документ частичного списания */
DEFINE BUFFER buf-opbank FOR op-bank. /* банковские реквизиты для него */
DEFINE BUFFER xop-entry1 FOR op-entry.
DEFINE BUFFER inacct     FOR acct.
DEFINE BUFFER bAcct      FOR acct. /* Локализация буфера. */
DEFINE BUFFER bOpO       FOR op.
DEFINE BUFFER bOpTemplB  FOR op-template.
DEFINE BUFFER buf_op-template FOR op-template.

DEFINE SHARED VARIABLE lim-pos    AS DECIMAL   NO-UNDO.

DEFINE VARIABLE set-all       AS LOGICAL       NO-UNDO.
DEFINE VARIABLE out-rid       AS RECID         NO-UNDO.
DEFINE VARIABLE tmp-rid       AS RECID         NO-UNDO.
DEFINE VARIABLE tmp-doc-type  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE isCreate      AS LOGICAL       NO-UNDO.
DEFINE VARIABLE vclass        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE vacct-cat     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE betwacct      AS LOGICAL
                                 INITIAL NO    NO-UNDO.
DEFINE VARIABLE h_kau        AS HANDLE         NO-UNDO.
DEFINE VARIABLE mAcct-cr     AS CHARACTER      NO-UNDO.
DEFINE VARIABLE main-first   AS LOGICAL        NO-UNDO.
DEFINE VARIABLE tcur         AS CHARACTER      NO-UNDO.
DEFINE VARIABLE mSide        AS CHARACTER      NO-UNDO.
DEFINE VARIABLE mBlSum       AS DECIMAL        NO-UNDO.
DEFINE VARIABLE mAcctStat    AS CHARACTER      NO-UNDO.
DEFINE VARIABLE mBlckRecid   AS RECID          NO-UNDO.
DEFINE VARIABLE mBlkLst      AS CHARACTER      NO-UNDO.
DEFINE VARIABLE lst-templ-acct AS CHARACTER    NO-UNDO.
DEFINE VARIABLE mTmplRid       AS RECID        NO-UNDO.
DEFINE VARIABLE mPropShabl   AS LOGICAL        NO-UNDO.
DEFINE VARIABLE vStatus      AS CHARACTER      NO-UNDO.
DEFINE VARIABLE cur-op-date  AS DATE           NO-UNDO.

{sh-defs.i}
{def-wf.i new}
{defframe.i NEW}
{intrface.get op}
{dpsproc.def} 

flager = 1.
FIND FIRST op-kind
     WHERE RECID(op-kind) EQ rid2 NO-LOCK NO-ERROR.

lst-templ-acct = list-op-templ(op-kind.op-kind,"acct").

FIND FIRST op-template OF op-kind
     WHERE op-template.acct-cat EQ 'o' NO-LOCK NO-ERROR.

IF NOT AVAILABLE op-templ THEN RETURN.
FIND FIRST xop-entry WHERE RECID(xop-entry) EQ op-en-rid EXCLUSIVE-LOCK.
FIND FIRST xop-entry1 WHERE RECID(xop-entry1) EQ op-en-rid EXCLUSIVE-LOCK.

FIND FIRST inacct WHERE RECID(inacct) EQ rid1 NO-LOCK NO-ERROR.

tit:
DO ON ENDKEY UNDO tit,RETURN
   ON ERROR  UNDO tit,RETURN:

    FIND FIRST buf_op-template OF op-kind WHERE
        buf_op-template.op-template EQ INT64(ENTRY(1, lst-templ-acct)) NO-LOCK NO-ERROR.
    IF AVAIL(buf_op-template) THEN
         mTmplRid  = RECID(buf_op-template).

   RUN kauproc.p PERSISTENT SET h_kau.
   RUN Create_acct IN h_kau (rid1,
                              "Карт2",
                              in-op-date,
                              mTmplRid,
                              OUTPUT out-rid) .
   DELETE PROCEDURE(h_kau) .
   IF RETURN-VALUE EQ "ERROR" THEN UNDO tit,RETURN.

   FIND FIRST acct WHERE RECID(acct) EQ out-rid NO-LOCK NO-ERROR.
   IF NOT AVAIL acct THEN UNDO tit, RETURN.

   FIND FIRST buf-op OF xop-entry EXCLUSIVE-LOCK NO-ERROR.
   FIND FIRST buf-op1 OF xop-entry1 EXCLUSIVE-LOCK NO-ERROR.
   IF xop-entry.currency NE "" THEN
      FIND LAST instr-rate
          WHERE instr-rate.instr-code EQ xop-entry.currency
            AND instr-rate.rate-type  EQ "УЧЕТНЫЙ"
            AND instr-rate.instr-cat  EQ "currency"
            AND instr-rate.since      LE (IF xop-entry.value-date NE ?
                                          THEN xop-entry.value-date
                                          ELSE xop-entry.op-date)
      NO-LOCK.
   set-all = YES.
   mSide = IF xop-entry.acct-cr EQ inacct.acct
           THEN "cr"
           ELSE "db".


   mBlkLst = BlckAcctOrdPay(INacct.acct + "," + INacct.currency, 
                            IF buf-op.op-Date EQ TODAY THEN DATETIME(TODAY,MTIME)
                                              ELSE DATETIME(buf-op.op-Date + 1) - 1,
                            buf-op.Order-Pay).

   IF CAN-DO(mBlkLst, "БлокСумм") THEN
      mBlSum = GetBlockPosition(inacct.acct, inacct.currency, buf-op.Order-Pay, buf-op.op-Date).
   ELSE
      mBlSum = 0.0.

   mAcctStat = CheckObject("acct",
                           inacct.acct + ',' + inacct.currency,
                           "op-entry",
                           STRING(xop-entry.op) + ',' + STRING(xop-entry.op-entry),
                           DATETIME(TODAY,MTIME),
                           OUTPUT mBlckRecid
                           ).

   IF    (    (     xop-entry.currency    NE ""
                AND ABS(sh-val - lim-pos - mBlSum) LT xop-entry.amt-cur 
                AND (sh-val - lim-pos - mBlSum)    NE 0
              ) 
           OR (     xop-entry.currency    EQ ""                
                AND ABS(sh-bal - lim-pos - mBlSum) LT xop-entry.amt-rub 
                AND (sh-bal - lim-pos - mBlSum)    NE 0
            )
        ) 
   AND     ( LOOKUP("БлокДб",mAcctStat) EQ 0
           )
        OR (    mSide EQ "db"
            AND LOOKUP("БлокДб",mAcctStat) GT 0
           )    
   THEN DO:

  IF FGetSetting("КартКонСумм",?,"") EQ "Нет" THEN 
  DO: 
      {messmenu &title="[ Постановка на картотеку ]"
                &choices="Поставить сумму проводки целиком,Поставить недостающую часть суммы"
      }
      IF INT64(pick-value)      EQ 1 THEN set-all = TRUE.
      ELSE IF INT64(pick-value) EQ 2 THEN set-all = FALSE.
      ELSE DO:
         flager = 2 .
         UNDO tit,RETURN.
      END.
   END.
 END.


   {g-acctv1.i 
      &no-dacct      = YES
      &vacct         = mAcct
   }
   IF mAcct-cr NE ? THEN DO:
      {find-act.i
          &bact   = bAcct
          &acct   = mAcct-cr
          &curr   = acct.currency
      }
   END.
   IF NOT AVAIL bAcct THEN DO:
      MESSAGE "Ошибка шаблона транзакции!"
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      UNDO tit, RETURN.
   end.

   /*Установка переменных для использования парсером через СисПарам*/
   RUN SetSysConf in h_base ("БАЛ-ДОК:OP-BAL" ,  STRING(buf-op.op)).
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-KIND" ,buf-op.doc-kind).
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-NUM"  ,buf-op.doc-num).
   RUN SetSysConf in h_base ("БАЛ-ДОК:ORDER-PAY",buf-op.order-pay).
   RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-DB"  , xop-entry.acct-db).
   RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-CR"  , xop-entry.acct-cr).
   RUN SetSysConf in h_base ("БАЛ-ДОК:БАЛАНСОВЫЙ",inacct.acct).
   RUN SetSysConf in h_base ("БАЛ-ДОК:BEN-ACCT" , buf-op.ben-acct).
   RUN SetSysConf in h_base ("БАЛ-ДОК:NAME-BEN" , buf-op.name-ben).
   RUN SetSysConf in h_base ("БАЛ-ДОК:INN"      , buf-op.inn).
   RUN SetSysConf in h_base ("БАЛ-ДОК:DETAILS"  , buf-op.details).
   RUN SetSysConf in h_base ("БАЛ-ДОК:AMT-RUB"  , STRING(xop-entry.amt-rub)).
   RUN SetSysConf in h_base ("БАЛ-ДОК:AMT-CUR"  , STRING(xop-entry.amt-cur)).
   RUN SetSysConf in h_base ("БАЛ-ДОК:OP-DATE" ,  STRING(buf-op.op-date)).
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-DATE" ,  STRING(buf-op.doc-date)).

   FIND FIRST doc-type WHERE doc-type.doc-type EQ buf-op.doc-type NO-LOCK NO-ERROR.
   RUN SetSysConf in h_base ("БАЛ-ДОК:НАИМ-ТИП-ДОК"  , IF AVAIL(doc-type) THEN doc-type.name-doc ELSE ""). 

   CREATE work-op .
   ASSIGN
      work-op.acct-db        = acct.acct
      work-op.acct-cr        = mAcct-cr
      work-op.acct-cr-enable = TRUE
      work-op.currency       = acct.currency
      work-op.op-status      = op-templ.op-status
      work-op.amt-cur        = IF xop-entry.currency NE "" THEN
                                  (IF set-all
                                   THEN xop-entry.amt-cur
                                   ELSE min(xop-entry.amt-cur,abs(sh-val - lim-pos - mBlSum)))
                               ELSE 0.0
      work-op.amt-rub        = IF xop-entry.currency EQ "" THEN
                                  (IF set-all
                                   THEN xop-entry.amt-rub
                                   ELSE min(xop-entry.amt-rub,abs(sh-bal - lim-pos - mBlSum))
                                  )
                               ELSE work-op.amt-cur * instr-rate.rate-instr / instr-rate.per
      work-op.amt-rub-enable = FALSE
      work-op.acct-cat       = "o"
      work-op.op-transaction = IF cur-op-trans EQ ? THEN buf-op.op-transaction
                                                    ELSE cur-op-trans
      work-op.details        = REPLACE(op-templ.details,"#НазнПлатБалДок#", buf-op.details)
   .


   /* Принудительно в "сегодня" при частичной постановке */
   IF NOT set-all THEN
   DO:
      CREATE buf-op1. 
      BUFFER-COPY buf-op EXCEPT op TO buf-op1 NO-ERROR.

      {auto-num.i &DefCount  = "buf-op1.op"
                  &DefNum    = "buf-op1.doc-num"
                  &DateCount = "in-op-date"
                  &AssignVar = "buf-op1.doc-num"
      }
      CREATE xop-entry1. 
      BUFFER-COPY xop-entry EXCEPT op TO xop-entry1 NO-ERROR.

      FIND FIRST doc-type WHERE 
                 doc-type.digital EQ "16" NO-LOCK NO-ERROR.

      ASSIGN
         buf-op1.doc-type      = IF AVAILABLE doc-type THEN doc-type.doc-type ELSE "016"
         buf-op1.doc-date      = in-op-date
         buf-op1.due-date      = in-op-date
         buf-op1.op-value-date = in-op-date
         buf-op1.contract-date = in-op-date
         buf-op1.ins-date      = in-op-date
         xop-entry1.op         = buf-op1.op
      .

      FOR EACH op-bank OF buf-op NO-LOCK:
         CREATE buf-opbank.
         BUFFER-COPY op-bank EXCEPT op TO buf-opbank NO-ERROR.
         buf-opbank.op = buf-op1.op.
      END.
   END.

   RUN g-call1.p (OUTPUT flager,
                  in-op-date,
                  RECID(work-op),
                  RECID(op-templ),
                  OUTPUT out-rid).

   IF flager NE 0 THEN DO:
      flager = 2 .
      UNDO tit, RETURN.
   END.
   ELSE flager = 1.

   FIND FIRST op-entry WHERE RECID(op-entry) EQ out-rid
   EXCLUSIVE-LOCK NO-ERROR.

   FIND doc-type OF buf-op NO-LOCK NO-ERROR.

   tmp-doc-type = (IF AVAIL doc-type THEN doc-type.digital ELSE "00").
   UpdateSigns ("op",
                STRING (op-entry.op),
                "op-bal",
                STRING(xop-entry.op),
                isXAttrIndexed(buf-op.class-code, "op-bal")).
   UpdateSigns ("op",
                STRING (op-entry.op),
                "ВидОп",
                tmp-doc-type,
                isXAttrIndexed(buf-op.class-code, "ВидОп")).

   IF RECID(xop-entry) EQ RECID(xop-entry1) THEN 
   DO:

      IF xop-entry.amt-rub NE 0 THEN
         UpdateSigns ("op",
                      STRING (buf-op.op),
                      "amt-rub",
                      STRING(xop-entry.amt-rub),
                      isXAttrIndexed(buf-op.class-code, "amt-rub")).

      IF xop-entry.amt-cur NE 0 THEN
         UpdateSigns ("op",
                      STRING (buf-op.op),
                      "amt-cur",
                      STRING(xop-entry.amt-cur),
                      isXAttrIndexed(buf-op.class-code, "amt-cur")).
   END.

   IF RECID(xop-entry) NE RECID(xop-entry1) THEN  /* частичная постановка */
   ASSIGN
      xop-entry1.amt-cur = IF xop-entry1.currency NE ""
                          THEN xop-entry1.amt-cur - op-entry.amt-cur
                          ELSE xop-entry1.amt-cur
      xop-entry1.amt-rub = IF xop-entry1.currency EQ ""
                          THEN xop-entry1.amt-rub - op-entry.amt-rub
                          ELSE xop-entry1.amt-cur * instr-rate.rate-instr / instr-rate.per
   .

   UpdateSigns ("op",
                STRING (buf-op.op),
                "acctcorr",
                xop-entry.acct-cr,
                isXAttrIndexed(buf-op.class-code, "acctcorr")).

   UpdateSigns ("op",
                STRING (buf-op.op),
                "acctbal",
                xop-entry.acct-db,
                isXAttrIndexed(buf-op.class-code, "acctbal")).

   IF xop-entry.amt-rub NE 0 THEN
      UpdateSigns ("op",
                   STRING (buf-op.op),
                   "amt-rub",
                   STRING(xop-entry.amt-rub),
                   isXAttrIndexed(buf-op.class-code, "amt-rub")).

   IF xop-entry.amt-cur NE 0 THEN
      UpdateSigns ("op",
                   STRING (buf-op.op),
                   "amt-cur",
                   STRING(xop-entry.amt-cur),
                   isXAttrIndexed(buf-op.class-code, "amt-cur")).

   /* Создание ДР для внебалансового документа */
   FIND FIRST bOpO OF op-entry
      NO-LOCK NO-ERROR.
   IF AVAIL bOpO THEN DO:
      UpdateSigns ("op",
                   STRING (bOpo.op),
                   "ИсхСтатус",
                   buf-op.op-status,
                   YES).
      RUN parssign.p (in-op-date,
                      "op-template",
                      op-kind.op-kind + "," + string(op-templ.op-templ),
                      op-templ.class-code,
                      "op",
                      STRING(op-entry.op),
                      bOpO.class-code,
                      RECID(wop)).
   END.
   /* Создание ДР для балансового документа */
   FIND FIRST bOpTemplB OF op-kind WHERE bOpTemplB.acct-cat EQ "b"
      NO-LOCK NO-ERROR.
   IF AVAIL bOpTemplB THEN
      RUN parssign.p (in-op-date,
                      "op-template",
                      op-kind.op-kind + "," + string(bOpTemplB.op-template),
                      bOpTemplB.Class-Code,
                      "op",
                      STRING (buf-op.op),
                      buf-op.Class-Code,
                      RECID (wop)).

   /* До удаления проводки вызываем форму ввода налоговых реквизитов */
   RUN nalpl_ed.p (RECID(buf-op), 2, 3).
   IF RETURN-VALUE EQ "ESC" THEN
   DO:
      flager = 2.
      UNDO tit, RETURN.
   END.
   RUN Copy-Xattr-Op(RECID(buf-op),RECID(buf-op1),"ПокСт,Kpp-send,Kpp-rec,КБК,ОКАТО-НАЛОГ,ПокОП,ПокНП,ПокНД,ПокДД,ПокТП").

   ASSIGN
      buf-op.user-inspector = USERID('bisquit')
      cur-op-date = in-op-date
      vStatus = GetStatCard().

   FIND FIRST op WHERE RECID(op) EQ RECID(buf-op) EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL(op) THEN DO:
      {chst(op.i &open-undo="UNDO tit, RETURN 'Ошибка смены статуса.'"
                 &kau-undo="UNDO tit, RETURN 'Ошибка смены статуса.'"
                 &xattr-undo= "UNDO tit, RETURN 'Ошибка смены статуса.'"
                 &undo="UNDO tit, RETURN 'Ошибка смены статуса.'"}

   END.

   RELEASE history.
   ASSIGN
      buf-op.user-inspector = userid('bisquit')
      buf-op.op-date        = ?
   .
/* этот код никогда не выполнялся, т.к. проводка перед этим удалялась 

   IF AVAILABLE xop-entry THEN  
   DO:
      DEF VAR list-xattr AS CHARACTER NO-UNDO.
      DEF VAR tmp-value  AS CHARACTER NO-UNDO.
      FIND FIRST op-templ OF op-kind WHERE op-templ.acct-cat EQ 'b' NO-LOCK NO-ERROR.
      IF AVAIL op-templ AND xop-entry1.currency EQ "" THEN DO:
         ASSIGN
            buf-op.class-code = IF op-template.cr-class-code EQ "" OR
                                   op-template.cr-class-code EQ ?
                                THEN ("op" + op-template.acct-cat)
                                ELSE op-template.cr-class-code
            vclass            = buf-op.class-code
         .
         RUN xattrall IN h_xclass(vclass,OUTPUT TABLE xattrid).
         list-xattr = "ДатаПлДок,НазнПл,НомДок,НомЧПл,ОстПл,ШифрПл".
         FOR EACH xattrid WHERE CAN-DO(list-xattr,xattrid.xattr-code) NO-LOCK:
            CASE LOOKUP(xattrid.xattr-code,list-xattr):
               WHEN 1 THEN DO:
                  tmp-value = IF buf-op.doc-date NE ? THEN STRING(buf-op.doc-date)
                                                      ELSE STRING(buf-op.op-date).
               END.
               WHEN 2 THEN DO:
                  tmp-value = buf-op.details.
               END.
               WHEN 3 THEN DO:
                  tmp-value = buf-op.doc-num.
               END.
               WHEN 4 THEN DO:
                  tmp-value = "1".
               END.
               WHEN 5 THEN DO:
                  tmp-value = STRING(op-entry.amt-rub).
               END.
               WHEN 6 THEN DO:
                  FIND doc-type OF buf-op1 NO-LOCK NO-ERROR.
                  tmp-value = IF AVAIL doc-type THEN doc-type.digital
                                                ELSE buf-op1.doc-type.
               END.
            END CASE.
            FIND xattr WHERE RECID(xattr) EQ xattrid.xattrid NO-LOCK NO-ERROR.
            IF AVAIL xattr THEN
            UpdateSigns ("op",
                         STRING (buf-op.op),
                         xattr.xattr-code,
                         tmp-value,
                         ?).
         END.
         buf-op1.doc-type   = IF op-templ.doc-type NE "" AND
                                 op-templ.doc-type NE ?
                              THEN op-templ.doc-type
                              ELSE buf-op1.doc-type.
      END.
   END.
*/   
   FIND FIRST op-entry WHERE RECID(op-entry) EQ out-rid
   EXCLUSIVE-LOCK NO-ERROR.

   IF     op-entry.kau-db NE ""
      AND op-entry.kau-db NE ?
      AND NUM-ENTRIES(op-entry.kau-db) EQ 2 THEN
   DO:
&IF DEFINED(oracle) &THEN
      FIND LAST kau WHERE kau.acct EQ op-entry.acct-db
                      AND kau.kau  EQ op-entry.kau-db
                      AND (   kau.currency EQ op-entry.currency
                           OR kau.currency EQ "")
         NO-LOCK NO-ERROR.
&ELSE         
      FIND LAST kau WHERE kau.acct EQ op-entry.acct-db
                      AND kau.kau  EQ op-entry.kau-db
                      AND op-entry.currency BEGINS kau.currency
         NO-LOCK NO-ERROR.
&ENDIF

      IF AVAILABLE kau THEN
         RUN inipoxtr.p(RECID(buf-op1), RECID(kau)).
   END.
END.

RUN SetSysConf in h_base ("БАЛ-ДОК:OP-BAL" , "").
RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-KIND" , "").
RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-NUM"  , "").
RUN SetSysConf in h_base ("БАЛ-ДОК:ORDER-PAY", "").
RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-DB"  ,  "").
RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-CR"  ,  "").
RUN SetSysConf in h_base ("БАЛ-ДОК:БАЛАНСОВЫЙ", "").
RUN SetSysConf in h_base ("БАЛ-ДОК:BEN-ACCT" ,  "").
RUN SetSysConf in h_base ("БАЛ-ДОК:NAME-BEN" , "").
RUN SetSysConf in h_base ("БАЛ-ДОК:INN"      , "").
RUN SetSysConf in h_base ("БАЛ-ДОК:DETAILS"  ,  "").
RUN SetSysConf in h_base ("БАЛ-ДОК:AMT-RUB"  ,  "").
RUN SetSysConf in h_base ("БАЛ-ДОК:AMT-CUR"  ,  "").
RUN SetSysConf in h_base ("БАЛ-ДОК:OP-DATE" ,   "").
RUN SetSysConf in h_base ("БАЛ-ДОК:НАИМ-ТИП-ДОК"  , ""). 
RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-DATE"  , ""). 
/* внебалансовая проводка завершена, сумма ставится на картотеку */
flager =  -1.
                                  
{intrface.del}          /* Выгрузка инструментария. */ 
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='06/05/2015 14:44:24.537+04:00' */
/* $LINTUSER='muta' */
/* $LINTMODE='1' */
/* $LINTFILE='card2inc.p' */
/*prosignBXfcx9Vq1tsdXCsJTCZp0Q*/