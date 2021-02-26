/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: TW-OP.P
      Comment: Триггер на запись в таблицу OP
   Parameters: нет
         Uses:
      Used by:
     Modified: 17.08.2004 15:50 KSV      (0033194) Переведен на std-trig.i
     Modified: 19.09.2005 19:34 KSV      (0046989) Вставлен инклюдник
                                         terrchck.i.
     Modified: 07.12.2005 kraw (0055178) op.filial-id
     Modified: 28/11/2007 kraw (0085391) запрещено создание документа класса op
     Modified: 28/05/2008 kraw (0093208) если класс op, то патемся "op" + op.acct-cat     
     Modified: 0163393 miol
*/

TRIGGER PROCEDURE FOR WRITE OF op OLD BUFFER oldOp.

{globals.i}
{xsms-mq.i}

DEFINE NEW GLOBAL SHARED VARIABLE shfilial AS CHARACTER NO-UNDO.

DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKomDoc AS INT64   NO-UNDO.
DEFINE VARIABLE mDpr-Id AS INT64     NO-UNDO.
DEFINE VARIABLE mStatusAnnul AS CHARACTER    NO-UNDO.

DEF BUFFER bop    FOR op.
DEF BUFFER bsigns FOR signs.
DEF BUFFER xxcode FOR code.

DEFINE BUFFER class FOR class.
DEFINE BUFFER bop-entry FOR op-entry.
DEFINE BUFFER bkau-entry FOR kau-entry.

DO TRANSACTION:
   {statmodel.i op oldop}
   /* Корректировка зависимых документов НУ,
   ** если изменилась плановая дата.
   ** Зависимых док-в НУ может быть несколько -
   ** поэтому внешняя транзакция необходима!
   */
   /* Переделал так как пролинт не пускал */
   IF op.op-value-date EQ ? THEN
      ASSIGN op.op-value-date = op.op-date .

   mStatusAnnul = FGetSetting("СтатусыАннул", ?, "").

   IF NOT {assigned op.filial-id} THEN
      op.filial-id = shfilial.

   IF op.class-code EQ "op" THEN
   DO:
      mStrTMP = "op" + op.acct-cat.
      FIND FIRST class WHERE class.class-code EQ mStrTMP NO-LOCK NO-ERROR.

      IF NOT AVAILABLE class THEN
         RETURN ERROR "Нельзя создавать объекты класса op".

      op.class-code = mStrTMP.
   END.

   IF Op.contract-date <> oldOp.contract-date THEN
   FOR /* Перебираем все док-ты НУ, зависящие от изменённого документа */
      EACH  bsigns WHERE
            bsigns.file-name  = "Op"
        AND bsigns.code       = "ИсхПроводка"
        AND bsigns.code-value BEGINS STRING(op.op) + ","
         NO-LOCK,

      FIRST bop    WHERE
            bop.op            = INT64(ENTRY(1, bsigns.surrogate))
        AND bop.op-status    <> "И1"
         NO-LOCK:

      RUN chst-op.p (RECID(bop), "И1") NO-ERROR.

      IF ERROR-STATUS:ERROR OR
         RETURN-VALUE <> "" THEN
      DO:
         MESSAGE
            " Не удалось изменить статус зависимого документа по НУ." SKIP
            RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN ERROR.
      END.
   END.
   IF CAN-DO(mStatusAnnul, op.op-STATUS) THEN 
   op.op-DATE = ?.
   FOR EACH op-entry OF op
      NO-LOCK:

      FIND FIRST bop-entry EXCLUSIVE-LOCK WHERE RECID(bop-entry) = RECID(op-entry) NO-WAIT NO-ERROR.

      IF LOCKED bop-entry THEN 
      DO:
         RUN wholocks2.p (RECID(op-entry), "op-entry", "Запись в op-entry заблокирована").
         RETURN ERROR "Запись в op-entry заблокирована". 
      END.

      ASSIGN
         bop-entry.op-status = op.op-status
         bop-entry.op-date = op.op-date
      .
   END.

   FOR EACH kau-entry OF op
      NO-LOCK:

      FIND FIRST bkau-entry EXCLUSIVE-LOCK WHERE RECID(bkau-entry) = RECID(kau-entry) NO-WAIT NO-ERROR.

      IF LOCKED bkau-entry THEN 
      DO:
         RUN wholocks2.p (RECID(kau-entry), "kau-entry", "Запись в kau-entry заблокирована").
         RETURN ERROR "Запись в kau-entry заблокирована". 
      END.

      ASSIGN
         bkau-entry.op-status = op.op-status
         bkau-entry.op-date = op.op-date
         bkau-entry.contract-date = op.contract-date
      .
   END.

   IF op.op-status <> oldop.op-status THEN DO:
      {intrface.get xclass}
      IF CAN-DO(GetXclassAllChildsEx("opb-pay"),op.class-code) THEN DO:
         FIND FIRST xxcode WHERE xxcode.class = "СтатПлат"
                             AND CAN-DO(xxcode.misc[1],op.op-status)
         NO-LOCK NO-ERROR.
         IF  AVAIL xxcode
         AND {assigned xxcode.code} THEN DO:
            IF NOT UpdateSignsEx(op.class-code, 
                                 STRING(op.op),
                                 "ПНСтатусПлатежа",
                                 xxcode.code)
            THEN RETURN ERROR "Не удалось установить доп.реквизит ПНСтатусПлатежа у документа входящего в подкласс opb-pay.".
         END.
         IF op.op-STATUS LE "А" THEN DO:

            mKomDoc = INT64(DYNAMIC-FUNCTION("GetXattrValue" IN h_base,
                                               "op",STRING(op.op),"ПНКодДокументаКомиссии")) NO-ERROR.
            IF  mKomDoc > 0 THEN DO:
                FIND FIRST bOp WHERE bOp.op EQ mKomDoc EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF AVAIL(bOp) THEN 
                   bOp.op-status = op.op-status. 
            END.
         END. 
      END.
      {intrface.del xclass}
   END.

   IF NEW op THEN 
   DO:
      DEFINE VARIABLE vRight AS CHARACTER   NO-UNDO.
      vRight = getThisUserXAttrValue('СтатусСозд').
      IF vRight = "" THEN 
      DO:
&IF DEFINED( MANUAL-REMOTE ) &THEN
         {intrface.get tmess}
         RUN Fill-AlertSysMes("","","-1","Вы не имеете права менять статус!").
         {intrface.del tmess}
&ELSE
         MESSAGE "Вы не имеете права менять статус!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
&ENDIF
         RETURN ERROR "Вы не имеете права менять статус!".
      END.
      IF NOT CAN-DO(vRight,op.op-status) THEN 
      DO:
&IF DEFINED( MANUAL-REMOTE ) &THEN
         {intrface.get tmess}
         RUN Fill-AlertSysMes("","","-1", 
            SUBSTITUTE( "Присвоение статуса &1  невозможно. У вас нет прав создавать документы такого статуса.", op.op-status ) 
         ).
         {intrface.del tmess}
&ELSE
         MESSAGE "Присвоение статуса " + op.op-status + " невозможно. У вас нет прав работать с документами такого статуса." VIEW-AS ALERT-BOX INFO BUTTONS OK.
&ENDIF
         RETURN ERROR "Присвоение статуса " + op.op-status + " невозможно. У вас нет прав создавать документы такого статуса.".
      END.
      {intrface.get xclass}
      DEFINE VARIABLE mCFOUser AS CHARACTER NO-UNDO.
      DEFINE VARIABLE mOfficeUser AS CHARACTER NO-UNDO.
      mCFOUser = GetXattrValueEx("_user",USERID('bisquit'),"cfo-user","").
      IF {assigned mCFOUser} THEN
         UpdateSigns("op",STRING(op.op),"cfo-oper",mCFOUser,isXAttrIndexed(op.class-code,"cfo-oper")).
      mOfficeUser = GetXattrValueEx("_user",USERID('bisquit'),"office","").
      IF {assigned mOfficeUser} THEN
         UpdateSigns("op",STRING(op.op),"office",mOfficeUser,isXAttrIndexed(op.class-code,"office")).
      {intrface.del xclass}
   END.

   IF op.branch-id EQ ? OR op.branch-id EQ "ЦОУ" THEN
      op.branch-id = GetXAttrValueEx("_user", 
                       STRING(USERID("bisquit")), 
                       "Отделение", 
                       FGetSetting("КодФил",?,"")).

   mDpr-Id = INT64(GetXattrValueEx("op",STRING(op.op),"dpr-id","0")) NO-ERROR.
   
   FIND FIRST sessions WHERE 
              sessions.dpr-id EQ mDpr-Id
      NO-LOCK NO-ERROR.
   IF     AVAIL sessions 
      AND sessions.op-date NE op.op-date 
      AND op.op-date NE ? THEN
      RETURN ERROR "Опер. день документа не может отличаться от опер. дня смены к которому он привязан".
                      
   RUN XSMSNotify-Op(BUFFER oldOp, MTIME).
   RUN XSMSNotify-Op(BUFFER op   , MTIME).

   /*****************************************************************
   * Инклюдник,выполняющий стандартную обработку триггера БИСКВИТа *
   * Данная строка должна оставаться последней в этом файле!       *
   ****************************************************************/
   {std-trig.i op oldop} 
END.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='05/10/2015 18:34:15.690+04:00' */
/* $LINTUSER='sados' */
/* $LINTMODE='1' */
/* $LINTFILE='tw-op.p' */
/*prosignfqZgUaXMa5Os64iK/EHMng*/
