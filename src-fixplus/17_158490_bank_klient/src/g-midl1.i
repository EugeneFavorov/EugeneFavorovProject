/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename:  G-MIDL1.I
      Comment:  Ввод не сложных, однострочных документов (g-midl.p)
                с возможностью задания формул расчета эквивалента
         Uses:  -
      Used by:  opkindnav.p
      Created:  03/01/1998 Peter from g-midl.p
     Modified:  12/09/2000 Om Оптимизация кода.
      Изменил:  26/01/2001 Kostik Вставил инклюд файл, который вставляет код КПП
                           при значении доп. реквизита шаблона КПП ДА (kppproc.i)
     Modified: 11.03.2002 17:54 SEMA     по заявке 0006077 Введена возможность формировать доп.реквизиты документа с
                                         помощью парсера
     Modified: 08.04.2002 13:38 SEMA     по заявке 0006975 исправлена ошибка заполнения поля wop.qty
     Modified: 16.04.2002 15:46 SEMA     по заявке 0006980 вставлен формат для op-doc-num
     Modified: 22.04.2002 12:28 SEMA     по заявке 0007087 вставлена универсальная обработка доп.реквизитов
     Modified: 26.04.2002 16:42 SEMA     по заявке 0006077 исправление ошибки
     Modified: 01.07.2002 19:19 SEMA     по заявке 0008325 Подъем заявки 7087 из спецверсии. Проведены незначительные
                                         изменения.
     Modified: 01.11.2002 18:15 SEMA     по заявке 0011705 сбрасывание значение doc-num в случае его не соответствия с
                                         устанавливаемым форматом
     Modified: 05.11.2002 19:00 Dema     по заявке 0011169 исправление ошибки выхода из процедуры ввода
                                         доп. реквизитов в Post
     Modified: 22.11.2002 20:45 rija     1734
     Modified: 18.12.2002 17:48 SEMA     по заявке 0012531 изменен вызов инструмента parssign
     Modified: 18.03.2003 18:30 DEMA     (0011169) Исправлены замечания по поводу
                                         зацикливания редактирования при
                                         отсутствии обязательных доп. реквизитов
     Modified: 29.04.2003 17:23 SEMA     по заявке 0015363 подключение вызова формы редактирования налоговых реквизитов
     Modified: 24.11.2003 16:28 kolal    Копирование налоговых реквизитов при
                                         потоковом вводе. Заявка 19106.
     Modified: 25.11.2003 17:48 kolal    19106
     Modified: 17.01.2005 17:36 Kostik   0041345 Перенесн op-entry.upd после 
                                                 формирования банковских реквизитов
                                                 т.к. при формировании суб аналитики
                                                 требовались данные банка.

     Modified: 18.01.2005 13:33 Kostik   
     Modified: 28/09/2005 kraw (0049959) VALIDATE op-bank (совместимость с oracle)
     Modified: 11.01.2006 kraw (0052869) перенос ДР с анулированного на док. частичного списания
     Modified: 27.03.2009 18:48 KSV      (0106192) Замена дублирования на
                                         tech_chk.i
     Modified: 22/05/2009 kraw (0102904) аккуратная автонумерация
     Modified: 20/07/2009 kraw (0070076) Вызов parssign.p для каждой проводки документа
*/

{g-defs.i}
{g-error.def}
{globals.def}
{def-wf.i new}
{defframe.i new}
{wordwrap.def}
{conf_op.i}       /*Процедуры cохранения/считывания из SysConf параметров документа*/
{g-docnum.def}    /* Для схем автонумерации. */ 
{dpsproc.def}
{op-115fl.def}
{doc-templ-cnt.i &do-define = YES}

define input param in-op-date like op.op-date no-undo.
define input param oprid      as recid        no-undo.

define var vordpay as char no-undo.
define var vmfo like op-bank.bank-code no-undo.
define var vcorr-acct like op-bank.corr-acct no-undo.
define var fmt as char no-undo.
define var dval like op-entry.value-date no-undo.
define var fler as logical no-undo.
define var result as INT64 no-undo.
define var msg as char format "x(40)" no-undo.
define var hproc as handle no-undo.
define var acctkey as INT64 no-undo.
define var temp-acct as char no-undo.
define var mforeq as logical no-undo.
define var std-fmt as char no-undo.
define var need-valdate as logical format "Дата валютирования/" no-undo.
def var fl-err as INT64 init -1 .
define var nprprog as char no-undo. /* ДР "предварительная обработка" */
DEF VAR mAfter AS CHAR NO-UNDO. /* ДР "пост обработка" */
DEFINE VAR lst-templ-op AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDebugXAttr      AS LOGICAL    NO-UNDO. /* выводить к редактированию доп.реквизиты ? */
&GLOB Regim-OneDoc YES
{g-cycle.def}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get tmcod}
{copyxtr.i}

def buffer xxop for op .
define buffer xwop for wop.

function g-checkbank  returns logical (input vmfo as char, INPUT iCodeType AS CHARACTER,input vcorr-acct as char, input benacct as char, output result as INT64, output msg as char) in hproc.

run "g-func.p" persistent set hproc.

{g-currv1.i &ofbase="/*"}
{g-frame.i &doframe=yes &row=2}

release dacct.
release cacct.

{chkacces.i}
{g-trig.i &recalc-acct=YES &wrapname=YES &row=2 &ACCTMESS = yes}

find first op-kind where recid(op-kind) = oprid no-lock.
{g-cycle.ini}

/* Обработка дополнительных реквизитов транзакции */
mbank-code-type = "МФО-9".
/* Отладка сумм поводок */
debugparser = INT64(GetXattrValueex('op-kind', op-kind.op-kind, 'debugparser', '0')).

/* Предварительная обработка */
nprprog = GetXattrValueEx("op-kind",op-kind.op-kind,"nprprog",?).
if nprprog ne ?
then do transaction:
    run value(substr(nprprog, 1, if index(nprprog, "((") eq 0
                                 then 255
                                 else index(nprprog, "((") - 1))
                                (substr(nprprog, (if index(nprprog, "((") eq 0
                                then ?
                                else index(nprprog, "((") + 2))).
    If keyfunction(lastkey) eq "end-error" OR
       RETURN-VALUE EQ "end-error" THEN RETURN.
end.

cycle:
do while true on endkey undo, leave:

   if retry then leave.
   if cur-op-trans <> ? and not is_cycle then leave.
   cur-op-trans = ?.
   gen:
   do trans with frame opreq on endkey undo cycle, retry cycle on error undo, leave:
      {optr.i &DoBefore=YES}
      assign
        tcur     = ?
        tacct-db = ?
        tacct-cr = ?
        tamt     = 0
        std-fmt  = op.doc-num:format in frame opreq
      .

      RUN SetSysConf IN h_Base ("ПаспортныеДанныеПостоянногоПолучателя", "").
      RUN SetSysConf IN h_Base ("ПаспортныеДанныеПостоянногоПолучателя.document-id","").
      lst-templ-op = list-op-templ(op-kind.op-kind,"op").
      doc:
      for each op-templ of op-kind no-lock with frame opreq on endkey undo gen, leave gen
                                                            on error undo gen, leave gen:

         IF NOT CAN-DO(lst-templ-op,STRING(op-template.op-template)) THEN NEXT doc.
         &GLOBAL-DEFINE op-doc-num-format op.doc-num:FORMAT
         {doc-templ-cnt.i &do-before = YES}

         ASSIGN
           mforeq = op-templ.mfo-needed OR (GetXattrValueEx('op-template', op-kind.op-kind + ',' + string(op-templ.op-templ), 'МежБанк',?) = 'Да')
           need-valdate = GetXattrValueEx('op-template', op-kind.op-kind + ',' + string(op-templ.op-templ), 'ДатаВал',?) = 'Да'
           .
         RUN GetDebugXAttr (op-kind.op-kind, OUTPUT vDebugXAttr).
         
         {g-frame.i &dobefore=yes &wrapname=Yes &DoTAcct=*}
         {&DoBeforeAfter}
         {transit.i}
         {doc-templ-cnt.i &do-disp = YES}
         {g-frame.i &dodisp=yes}
   /*      {g-op.cp} */ run Copy4Cycle.
         sset:
         do on error undo, retry on endkey undo cycle, retry cycle:
            
            {g-frame.i &const-recip=Yes}
            {g-frame.i &doset=yes}
            if op.op-status begins "А" then
              assign
                op.op-date       = ?
                op-entry.op-date = op.op-date
              .
            {kppproc.i &BUF-OP-TEMPLATE = op-template
                       &BUF-OP-ENTRY    = op-entry
                       &BUF-OP          = op}
            RUN setOpDocSysConf(INPUT "КппПол",
                                INPUT RECID(op),
                                INPUT STRING(INT64(vmfo),"999999999") + "," + op.ben-acct + "," + op.inn
                               ).

            RUN setsysconf IN h_base ("КппПол","").
            IF AVAIL op-entry  OR 
               CAN-FIND(FIRST xxop WHERE xxop.op-transaction EQ op.op-transaction 
                                     AND RECID(xxop) NE RECID(op)) 
            THEN DO:
               IF mforeq AND 
                  NOT g-checkbank(vmfo, 
                                  mbank-code-type,
                                  vcorr-acct, 
                                  op.ben-acct, 
                                  OUTPUT result, 
                                  OUTPUT msg) 
               THEN 
               DO:
                  &IF DEFINED(EmptyBenAcctOK) &THEN 
                  DO:
                     IF op.ben-acct NE "" THEN DO:
                        /* Commented by KSV: выдача сообщений об ошибке */
                        {tech_chk.i {&*}}
                     END.
                  END.
                  &ELSE
                  DO:
                  /* Commented by KSV: выдача сообщений об ошибке */
                  {tech_chk.i {&*}}
                  END.
                  &ENDIF
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
                  {op-type.upd &check-format=Yes}
                  VALIDATE op-bank.
               END.
            
               { op-type.chk }

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
               {&post-kau}
            END.
         END.

         {doc-templ-cnt.i &do-save = YES}

         RUN Prepare4Cycle.
      END.
      {optr.i &DoAfter=YES}
   END.
   {g-print1.i}
END.

{xattr-cr.i &no-run-xattr-cr-proc} /* объявляем процедуру для установки class-code и запуска броузера доп.реквизитов */

procedure Post:

    DEF VAR vShowNalFrm AS LOGICAL NO-UNDO.
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
                    op-kind.op-kind + "," + string(op-templ.op-templ),
                    op-templ.class-code,
                    "op",
                    STRING(op.op),
                    op.class-code,
                    RECID(wop)).
    IF AVAIL op-entry THEN
    RUN setdocat.p (RECID(op), op-entry.acct-db, op-entry.acct-cr, op-entry.currency, FALSE,
                    GetSysConf("ПаспортныеДанныеПостоянногоПолучателя"),
                    GetSysConf("ПаспортныеДанныеПостоянногоПолучателя.document-id")).
    RUN SetSysConf IN h_Base ("ПаспортныеДанныеПостоянногоПолучателя","").
    RUN SetSysConf IN h_Base ("ПаспортныеДанныеПостоянногоПолучателя.document-id","").

    {xattr-cr.i} /* и только теперь выполняем ее */
    
    IF NOT vShowNalFrm THEN DO:
       /* вызываем форму редактирования налоговых реквизитов */
       {nal-cp.i}
       RUN nalpl_ed.p (RECID(op), 2, 3).
       IF RETURN-VALUE EQ "ESC" THEN RETURN "ESC".
    END.

    if avail op-entry then do :
        if tcur = ? then tcur = op-entry.currency.
        assign
            wop.acct-db  = op-entry.acct-db
            wop.acct-cr  = op-entry.acct-cr
            wop.currency = op-entry.currency
            wop.amt-cur  = if op-entry.currency <> "" then op-entry.amt-cur else op-entry.amt-rub
            wop.amt-rub  = op-entry.amt-rub
            wop.qty      = op-entry.qty
            .
    end.

    RUN dopclbch.p (RECID(op),in-op-date).

end procedure.

{g-cycle.pro
    &templautonum = YES}
    
mAfter = op-kind.after.
IF {assigned  mAfter}
   THEN RUN VALUE (substr(mAfter, 1, if index(mAfter, "((") eq 0
                               then 255
                               else index(mAfter, "((") - 1))
                              (substr(mAfter, (if index(mAfter, "((") eq 0
                              then ?
                              else index(mAfter, "((") + 2))).

 hide frame opreq no-pause.
delete procedure(hproc).
