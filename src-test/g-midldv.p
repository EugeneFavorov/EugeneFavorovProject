/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-2020 ЗАО "Банковские информационные системы"
     Filename:  g-midlds.p
      Comment:  Ввод не сложных, однострочных документов (g-midl.p)
                с возможностью задания формул расчета эквивалента как
                в Диасофт.
   Parameters: in-op-date,oprid     
         Uses:  -
      Used by:  opkindnav.p
      Created:  28/05/1999 Peter from g-midl1.p
      Изменил:  26/01/2001 Kostik Вставил инклюд файл, который вставляет код КПП
                           при значении доп. реквизита шаблона КПП ДА (kppproc.i)
     Modified: 22.04.2002 12:28 SEMA     по заявке 0007087 вставлена универсальная обработка 
                                         доп.реквизитов
     Modified: 01.07.2002 19:19 SEMA     по заявке 0008325 Подъем заявки 7087 из спецверсии. 
                                         Проведены незначительные
                                         изменения.
     Modified: 30.08.2002 Gunk   Ошибка при создании класификатора
     Modified: 25.11.2003 kolal  Копирование налоговых реквизитов. Заявка 19106.     
     Modified: 17.01.2005 17:36 Kostik   0041345 Перенесн op-entry.upd после 
                                                 формирования банковских реквизитов
                                                 т.к. при формировании суб аналитики
                                                 требовались данные банка.
     Modified: 20.01.2005 kraw (0026950) проверка кода валюты в ben-acct для рублевых документов
     Modified: 11.01.2006 kraw (0052869) перенос ДР с анулированного на док. частичного списания
     Modified: 27.03.2009 18:46 KSV      (0106192) Замена дублирования на
                                         teck_chk.i
     Modified: 22/05/2009 kraw (0102904) аккуратная автонумерация
     Modified: 20/07/2009 kraw (0070076) Вызов parssign.p для каждой проводки документа
     Modified: 08/12/2016 kam вызов процедуры из ДР RunAfterCreate на op-template
     
*/

{g-defs.i}
{g-error.def}
{globals.def}
{def-wf.i new}
{defframe.i new}
{wordwrap.def}
{intrface.get xclass}
{intrface.get cust}
{conf_op.i}       /*Процедуры cохранения/считывания из SysConf параметров документа*/
{g-docnum.def}    /* Для схем автонумерации. */ 
{intrface.get tmcod}
{op-115fl.def}
{doc-templ-cnt.i &do-define = YES}

DEFINE INPUT PARAM in-op-date LIKE op.op-date.
DEFINE INPUT PARAM oprid      AS RECID.
DEFINE VAR vordpay    AS CHAR    NO-UNDO.
DEFINE VAR vmfo       LIKE op-bank.bank-code NO-UNDO.
DEFINE VAR vcorr-acct LIKE op-bank.corr-acct NO-UNDO.
DEFINE VAR fmt        AS CHAR    NO-UNDO.
DEFINE VAR dval       LIKE op-entry.value-date NO-UNDO.
DEFINE VAR fler       AS LOGICAL NO-UNDO.
DEFINE VAR result     AS INT64 NO-UNDO.
DEFINE VAR msg        AS CHAR FORMAT "x(40)" NO-UNDO.
DEFINE VAR hproc      AS HANDLE  NO-UNDO.
DEFINE VAR acctkey    AS INT64 NO-UNDO.
DEFINE VAR temp-acct  AS CHAR    NO-UNDO.
DEFINE VAR mforeq     AS LOGICAL NO-UNDO.
DEFINE VAR std-fmt    AS CHAR    NO-UNDO.
DEFINE VAR need-valdate AS LOGICAL FORMAT "Дата валютирования/" NO-UNDO.
DEF VAR fl-err AS INT64 INIT -1 .
DEF VAR mMethodAfter AS CHAR NO-UNDO.

DEFINE VARIABLE vDebugXAttr AS LOGICAL NO-UNDO. /* выводить к редактированию доп.реквизиты ? */
&GLOB Regim-OneDoc YES
mbank-code-type = "МФО-9".
/* для поточного ввода */
{g-cycle.def}
/* */

DEF BUFFER xxop    FOR op .
DEFINE BUFFER xwop FOR wop.
DEF BUFFER xxcode  FOR code.

FUNCTION g-checkbank RETURNS LOGICAL (INPUT vmfo AS CHAR, INPUT iCodeType AS CHARACTER, INPUT vcorr-acct AS CHAR, INPUT benacct AS CHAR, OUTPUT result AS INT64, OUTPUT msg AS CHAR) IN hproc.
RUN "g-func.p" PERSISTENT SET hproc.

{g-currv1.i &ofbase="/*"}
{g-frame.i &doframe=yes &row=2 &likediasoft=Yes}

RELEASE dacct.
RELEASE cacct.

{chkacces.i}
{g-trig.i &recalc-acct=Yes &likediasoft=Yes &wrapname=Yes &ACCTMESS = YES &CheckBenAccCurrency=YES &EmptyBenAcctOK=YES}

FIND FIRST op-kind WHERE RECID(op-kind) = oprid NO-LOCK NO-ERROR.

{g-cycle.ini}

debugparser = INT64(GetXattrValueex('op-kind', op-kind.op-kind, 'debugparser', '0')).
RUN GetDebugXAttr (op-kind.op-kind, OUTPUT vDebugXAttr).

cycle:
DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
   IF RETRY OR basta THEN LEAVE.
   IF cur-op-trans <> ? AND NOT is_cycle THEN LEAVE.
   cur-op-trans = ?.
   gen:
   DO TRANS WITH FRAME opreq ON ENDKEY UNDO cycle, RETRY cycle ON ERROR UNDO, LEAVE:
      IF RETRY AND basta THEN UNDO gen, LEAVE cycle.
      ASSIGN
         tcur     = ?
         tacct-db = ?
         tacct-cr = ?
         tamt     = 0
         std-fmt  = op.doc-num:FORMAT IN FRAME opreq
      .

      {optr.i &DoBefore=YES}
      {befexpr.i &befopkind = op-kind.op-kind}

      doc:
      FOR EACH op-template OF op-kind NO-LOCK WITH FRAME opreq ON ENDKEY UNDO gen, LEAVE gen
                                                            ON ERROR  UNDO gen, LEAVE gen:

         ASSIGN
            mforeq = op-templ.mfo-needed OR (GetXattrValueEx('op-template', op-kind.op-kind + ',' + STRING(op-templ.op-templ), 'МежБанк',?) = 'Да')
            need-valdate = GetXattrValueEx('op-template', op-kind.op-kind + ',' + STRING(op-templ.op-templ), 'ДатаВал',?) = 'Да'
         .

         {doc-templ-cnt.i &do-before = YES}
         {g-frame.i &dobefore=yes &wrapname=Yes &DoTAcct=*}
         {transit.i}
         {doc-templ-cnt.i &do-disp = YES}
         {g-frame.i &dodisp=yes}
         /* {g-op.cp} */ RUN Copy4Cycle.
         sset:
         DO ON ERROR UNDO, RETRY ON ENDKEY UNDO cycle, RETRY cycle:
            IF RETRY AND basta THEN UNDO gen, LEAVE cycle.

            {g-frame.i &const-recip=Yes}
            {g-frame.i &doset=yes &likediasoft=Yes}
            {kppproc.i &BUF-OP-TEMPLATE = op-template
                       &BUF-OP-ENTRY    = op-entry
                       &BUF-OP          = op}
            RUN setOpDocSysConf(INPUT "КппПол",
                                INPUT RECID(op),
                                INPUT STRING(INT64(vmfo),"999999999") + "," + 
                                             op.ben-acct + "," + op.inn
                               ).
            
            RUN setsysconf IN h_base ("КппПол","").
            IF AVAIL op-entry  OR 
               CAN-FIND(FIRST xxop WHERE xxop.op-transaction EQ op.op-transaction 
                                     AND RECID(xxop)         NE RECID(op))
            THEN DO:
               IF mforeq AND NOT 
                  g-checkbank(vmfo,
                              mbank-code-type, 
                              vcorr-acct, 
                              op.ben-acct, 
                              OUTPUT result, 
                              OUTPUT msg) 
               THEN 
               DO:
                  IF op.ben-acct NE "" THEN DO:
                     /* Commented by KSV: выдача сообщений об ошибке */
                     {tech_chk.i {&*}}
                  END.
               END.
               RUN "g-bank.p" (op-kind.op-kind, 
                               op-templ.op-templ, 
                               op.op, 
                               4,
                               OUTPUT fl-err).
               IF fl-err LT 0 THEN UNDO, RETRY.
               
               IF (vmfo       NE "" AND vmfo       NE ?) OR 
                  (vcorr-acct NE "" AND vcorr-acct NE ?) 
               THEN DO:
                  {opbnkcr.i op.op """" ""МФО-9"" vmfo vcorr-acct}
                  {op-type.upd &check-format=Yes &surr=string(op.op)}
               END.
               {op-type.chk}

               RUN parssign2.p ("PARSSEN_ENTRY_",
                               in-op-date,
                               "op-template",
                               op-kind.op-kind + "," + string(op-templ.op-templ),
                               op-templ.class-code,
                               "op-entry",
                               STRING(op-entry.op) + "," + STRING(op-entry.op-entry),
                               op-entry.class-code,
                               RECID(wop)).

               VALIDATE op-entry NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                  UNDO sset, RETRY sset.
               RUN Post.
               IF RETURN-VALUE = "ESC" THEN
                  UNDO sset, RETRY sset.
               {op-entry.upd &871=YES &copynal=YES}
               {aft-temp.i}
               RUN ValidateCust115fl(BUFFER op,
                                     wop.acct-db,
                                     wop.currency,
                                     wop.amt-rub)
               NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   UNDO gen, LEAVE gen.
            END.            
         END.

         {doc-templ-cnt.i &do-save = YES}
         mMethodAfter = GetXAttrValueEx("op-template", op-kind.op-kind + "," + STRING(op-template.op-template), "RunAfterCreate", "").
      
         IF mMethodAfter NE "" THEN
         DO:
      
            IF SearchPFile(mMethodAfter) THEN
            DO:
               RUN VALUE(mMethodAfter + ".p") (RECID(op)).
      
               IF pick-value NE "YES" THEN
                  UNDO gen, LEAVE gen.
            END.
         END.

         RUN Prepare4Cycle.
      END.
      {optr.i &DoAfter=YES}

   END.
   {g-print1.i}
END.

{g-cycle.pro
    &templautonum = YES}

{xattr-cr.i &no-run-xattr-cr-proc} /* объявляем процедуру для установки class-code и запуска броузера доп.реквизитов */

PROCEDURE Post:
    DEF VAR vKppRec     AS CHARACTER  NO-UNDO.
    DEF VAR vShowNalFrm AS LOGICAL    NO-UNDO.

    DEFINE BUFFER GB4PL_op-kind     FOR op-kind.
    DEFINE BUFFER GB4PL_op-template FOR op-template.
    DEFINE BUFFER GB4PL_op          FOR op.
    DEFINE BUFFER GB4PL_op-entry    FOR op-entry.


    vShowNalFrm = GetXattrValue("op-template",
                                op-kind.op-kind + "," + string(op-templ.op-templ),
                                "НалФрмДоДР") EQ "Да".

    wop.op-recid = RECID(op).

    IF vShowNalFrm THEN DO:
       /* вызываем форму редактирования налоговых реквизитов */
       {nal-cp.i}
       RUN nalpl_ed.p (RECID(op), 2, 3).
       IF RETURN-VALUE EQ "ESC" THEN RETURN "ESC".
    END.

    RUN parssign.p (in-op-date,
                    "op-template",
                    op-kind.op-kind + "," + string(op-template.op-template),
                    op-templ.class-code,
                    "op",
                    STRING(op.op),
                    op.class-code,
                    RECID(wop)).

    {xattr-cr.i} /* и только теперь выполняем ее */

    IF NOT vShowNalFrm THEN DO:
       /* вызываем форму редактирования налоговых реквизитов */
       {nal-cp.i}
       RUN nalpl_ed.p (RECID(op), 2, 3).
       IF RETURN-VALUE EQ "ESC" THEN RETURN "ESC".
    END.

    IF AVAIL op-entry THEN DO :
        IF tcur = ? THEN tcur = op-entry.currency.
        ASSIGN
            wop.acct-db  = op-entry.acct-db
            wop.acct-cr  = op-entry.acct-cr
            wop.currency = op-entry.currency
            wop.amt-cur  = IF op-entry.currency <> "" THEN op-entry.amt-cur else op-entry.amt-rub
            wop.amt-rub  = op-entry.amt-rub
            wop.qty      = op-entry.qty
            .
    END.
    
    vKppRec = GetXAttrValueEx("op", 
                               STRING(op-entry.op), 
                              "kpp-rec", 
                              ?).
    IF {assigned vmfo} AND {assigned op.ben-acct}
    THEN RUN CreateUpdateRecipient IN h_cust (STRING(INT64(vmfo),"999999999"),op.ben-acct,op.inn,op.name-ben,vKppRec,?,?,"ИМЯ").
    
END PROCEDURE.

HIDE FRAME opreq NO-PAUSE.
DELETE PROCEDURE(hproc).
{intrface.del}          /* Выгрузка инструментария. */ 
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='29/02/2016 14:51:01.995+04:00' */
/* $LINTUSER='soav' */
/* $LINTMODE='1' */
/* $LINTFILE='g-midlds.p' */
/*prosignLFw7j3D2yRaaDhgKmCtN8A*/