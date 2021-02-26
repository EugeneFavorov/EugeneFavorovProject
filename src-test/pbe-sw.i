/* 
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: E-SW.I
      Comment: <comment>
   Parameters:
         Uses:
      Used BY:
      Created: 16.05.2002 21:59 MKV
     Modified: 17.05.2002 11:50 MKV
     Modified: 07/07/2005       NIK Изменение статуса субпроводок
     Modified: 26.06.2006 14:59 revv    Реструктуризация кода
*/


DEF INPUT PARAM in-op-date  LIKE op.op-date NO-UNDO.
DEF INPUT PARAM in-rec-kind AS recid        NO-UNDO.
DEF NEW SHARED VAR nomdoc AS INT64 NO-UNDO.

{globals.i}
/* Замена Плюс банк
{e-telex.def NEW} */
{pbe-telex.def NEW}
/* Конец замены Плюс банк */
{imp.wrk NEW}
{defexp.tab NEW}
{bank-id.i}
{e-swodbi.i}
{e-swofile.pro}
/* Замена Плюс банк
{e-telex.i} */
{pbe-telex.i}
/* Конец замены Плюс банк */

DEF VAR vLockErr  AS CHAR NO-UNDO.
DEF VAR vTextErr  AS char NO-UNDO. 
DEF VAR mRef      AS char NO-UNDO.  /*референс для сохранения в рекв-ах ИЭ*/
DEF VAR vMaskMsg  AS char NO-UNDO.

DEFINE BUFFER u-op FOR op.
DEFINE BUFFER u-oe FOR op-entry.

vTextErr = "Документ изменяет другой пользователь. "  +
           "Проверьте соответствие данных сообщению. " +
           "Статус сообщения не изменен.". 

IF mDBIExchange THEN  /* при использовании DB Internet */
   GetCounterNextValue("SwiftNumFile", gend-date).

IF NOT auto THEN DO:
   {setdest.i &stream="stream err"}
END.

main:
FOR EACH op-templ OF op-kind :
   ASSIGN
      cur-op-date = in-op-date
      str-bank    = "*"
      in-type     = IF op-templ.type EQ "" OR
                       op-templ.type EQ "*" THEN
                       "*"
                    ELSE op-temp.type.
   /* определение внутренних процедур */
   RUN VALUE({&crtt}) (OUTPUT ndd, OUTPUT buf-sum).   /* внутренняя процедура */

END.     /* Окончание main */
/*****************************************************************************/
/* Вывод протокола ошибок                                                    */
/* Если были ошибки : то печатаем протокол и спрашиваем о продолжение работы */

IF flag-err AND
   NOT auto THEN DO:
   {preview.i &stream="stream err" &nodef="/*"}
   MESSAGE COLOR NORMAL SKIP "Продолжить экспорт документов ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.
   IF choice NE YES THEN RETURN.
END.
ELSE OUTPUT STREAM err CLOSE.

/************************************************************************/
/*      Сортировка отобранных документов                                */
IF NOT auto THEN DO:
      RUN "op-order.p" (in-op-date,4).
END.
FIND FIRST exp-temp-table WHERE
   exp-temp-table.order NE ? AND
   exp-temp-table.order NE 0 NO-LOCK NO-ERROR.
IF NOT AVAIL exp-temp-table THEN RETURN.

/************************************************************************/
/*    Создание строки                                                   */
/*    Печать строки в файл.                                             */

ASSIGN
  ndd     = 0
  buf-sum = 0.
EXPTEPMP:
FOR EACH exp-temp-table WHERE
   exp-temp-table.order NE ? AND
   exp-temp-table.order NE 0
BY exp-temp-table.order:
   FIND op WHERE
      op.op EQ exp-temp-table.op NO-LOCK NO-ERROR.

   FIND FIRST op-template WHERE
      RECID(op-template) EQ exp-temp-table.op-templ NO-LOCK NO-ERROR NO-WAIT.
 
   IF op.op-status NE op-template.op-status THEN DO:
      IF NOT auto THEN DO:
         MESSAGE "У документа" Op.doc-num "~n" 
                 "уже изменен статус на " op.op-status  
         VIEW-AS ALERT-BOX.
      END.
      ASSIGN exp-temp-table.arbitrary-text = vTextErr
             exp-temp-table.order          = 0
      .
      NEXT EXPTEPMP.
   END.

   FIND op-entry OF op WHERE
      op-entry.op-entry EQ exp-temp-table.op-entry NO-LOCK NO-ERROR.
   FIND op-bank  OF op NO-LOCK NO-ERROR.
   FIND doc-type OF op NO-LOCK NO-ERROR.

   FIND FIRST u-op WHERE recid(u-op) EQ recid(op) EXCLUSIVE-LOCK NO-ERROR no-wait.
   IF NOT AVAILABLE(u-op) THEN 
   DO:
      WhoLocks2(recid(op),"op",INPUT-OUTPUT vLockErr).
      IF {assigned vLockErr} THEN DO:
         IF NOT auto THEN DO:
            MESSAGE "Документ" Op.doc-num ".~n" vLockErr
                        VIEW-AS ALERT-BOX.
         END.
         ASSIGN exp-temp-table.arbitrary-text = vTextErr
                exp-temp-table.order          = 0.
         NEXT EXPTEPMP.
      END.
   END.

   FOR EACH op-entry OF op NO-LOCK:
      FIND FIRST u-oe WHERE recid(u-oe) EQ recid(op-entry) EXCLUSIVE-LOCK NO-ERROR no-wait. 
      IF NOT AVAILABLE(u-oe) THEN 
      DO:
         WhoLocks2(recid(u-oe),(BUFFER u-oe:TABLE),INPUT-OUTPUT vLockErr).
         IF {assigned vLockErr} THEN DO:
            IF NOT auto THEN DO:
               MESSAGE "Документ" Op.doc-num ".~n" vLockErr
                           VIEW-AS ALERT-BOX.
            END.
         ASSIGN exp-temp-table.arbitrary-text = vTextErr
                exp-temp-table.order          = 0.
            NEXT EXPTEPMP.
         END.
      END.
   END.
   {&pre}

   RUN VALUE({&proc}) (RECID(exp-temp-table),op-kind.op-kind,in-series-num) NO-ERROR.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("e-sw.i","ошибка:" + string(error-status:error)). 
   RUN dbgprint.p ("e-sw.i","RETURN-VALUE:" + string(RETURN-VALUE)). 
   &ENDIF

   mRef = RETURN-VALUE.
   IF RETURN-VALUE BEGINS "ERR" or error-status:error THEN
   DO:
      exp-temp-table.order = 0.
      NEXT.
   END.

   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   {&ass}
   IF RETURN-VALUE BEGINS "ERR" THEN
   DO:
      exp-temp-table.order = 0.
      NEXT.
   END.

END.
/*********************************************************************** */
vMaskMsg = FGetSetting("SWIFT","reestr-exp","").
IF CAN-DO(vMaskMsg,SUBSTRING({&proc},6,3))
   AND NOT auto THEN
   RUN op-ptbin.p.

{exp.end}

IF mDBIExchange THEN RUN OutputToDstFile. /* при использовании DB Internet */

PAUSE BEFORE-HIDE.
/*---------------------------------------------------*/
/* процедура формирования реквизитов сообщения       */
/*---------------------------------------------------*/
PROCEDURE e-swmg:
   DEF INPUT PARAM ipAcct    AS CHAR NO-UNDO. /*счет для правила обмена*/
   DEF INPUT PARAM ipTypeMsg AS CHAR NO-UNDO. /*тип сообщения*/

   IF ipAcct EQ ? THEN
      FIND FIRST mail-user WHERE
         mail-user.mail-user-num EQ in-user-num NO-LOCK NO-ERROR.
   ELSE DO:
      FIND FIRST acct WHERE
         acct.acct EQ ipAcct NO-LOCK NO-ERROR.
      FIND FIRST mail-user WHERE
         mail-user.cust-cat EQ acct.cust-cat AND
         mail-user.cust-id  EQ acct.cust-id NO-LOCK NO-ERROR.
   END.

   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   {sw-msg.cpy} /* сводный */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("e-sw.i","zop 1:" + string(zop)). 
   &ENDIF

   zop = GetXAttrValue('op',string(op.op),vstr). /* ведущий */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("e-sw.i","zop 2:" + string(zop) + 
                            " RETURN-VALUE:" + string(RETURN-VALUE) + 
                            " ipTypeMsg: "   + string(ipTypeMsg) +
                            " (zop EQ пусто):" + string(zop EQ '')). 

   &ENDIF
   IF zop EQ '' THEN
      RUN e-swoie(?,mRef,ipTypeMsg).
   ELSE RUN e-swoie(INT64(zop),mRef,ipTypeMsg).
   RETURN RETURN-VALUE.
END PROCEDURE.
/*---------------------------------------------------*/
/* процедура формирования реквизитов импорта экспорта*/
/*---------------------------------------------------*/
PROCEDURE e-swoie:
   DEF INPUT PARAM ipOpOpSvod AS INT64  NO-UNDO. /*код сводного док*/
   DEF INPUT PARAM ipRef      AS CHAR NO-UNDO. /*референс*/
   DEF INPUT PARAM ipTypeMsg  AS CHAR NO-UNDO. /*тип сообщения*/

   DEF VAR vMskTypeMsg  AS CHAR NO-UNDO. /*маска типов сообщения*/
   DEF VAR vst-no-batch AS CHAR NO-UNDO. /* */


   ASSIGN
      vMskTypeMsg = "200,202,900,910"
      buf-sum     = buf-sum + op-entry.amt-rub
      ndd         = ndd + 1.

   IF ipOpOpSvod EQ ? THEN
      FIND FIRST op-impexp OF op-entry NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST op-impexp WHERE
         op-impexp.op EQ ipOpOpSvod NO-LOCK NO-ERROR.
   FIND FIRST op-template WHERE
      RECID(op-template) EQ exp-temp-table.op-templ NO-LOCK NO-ERROR.

   IF NOT AVAIL op-impexp THEN
      CREATE op-impexp.
   ELSE
      FIND current op-impexp EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF AVAIL op-impexp THEN 
MAIN:
   DO ON ERROR UNDO MAIN, LEAVE MAIN:
      ASSIGN
         vst-no-batch = GetXattrValueEx('op-template',op-kind.op-kind + ',' + STRING(op-templ.op-templ),'st-no-batch',?)
         op-impexp.op             = IF ipOpOpSvod EQ ? THEN op.op ELSE ipOpOpSvod
         op-impexp.exp-date       = today
         op-impexp.exp-time       = time
         
      op-impexp.bank-reference = IF ((CAN-DO(vMskTypeMsg,ipTypeMsg) AND
                                      NOT {assigned op-impexp.bank-reference}
                                     ) OR
                                     NOT CAN-DO(vMskTypeMsg,ipTypeMsg)
                                     ) AND
                                     ipRef NE ? 
                                 THEN
                                    ipRef
                                 ELSE IF CAN-DO(vMskTypeMsg,ipTypeMsg) THEN
                                    op-impexp.bank-reference
                                 ELSE 
                                    ''
      op-impexp.exp-batch      = IF AVAIL op-template AND
                                    CAN-DO(vst-no-batch,op.op-status)
                                 THEN ''
                                 ELSE in-series-num
   .
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("e-sw.i","op-impexp.bank-reference 1):"                + string(op-impexp.bank-reference) + 
                             " (CAN-DO(vMskTypeMsg,ipTypeMsg)):"           + string(CAN-DO(vMskTypeMsg,ipTypeMsg)) +
                             " (NOT {assigned op-impexp.bank-reference}):" + string(NOT {assigned op-impexp.bank-reference}) +
                             " (NOT CAN-DO(vMskTypeMsg,ipTypeMsg)):"       + string(NOT CAN-DO(vMskTypeMsg,ipTypeMsg)) +
                             " (ipRef NE ?):"                              + string(ipRef NE ?) +
                             " (ipRef):"                                   + string(ipRef)
                             ). 
   RUN dbgprint.p ("e-sw.i","op-impexp.bank-reference finish:" + string(op-impexp.bank-reference)).
   &ENDIF

      IF ipOpOpSvod EQ ? THEN
         RUN upd-status.
      ELSE
         FOR EACH signs WHERE signs.file-name EQ 'op' AND
                              signs.code      EQ vstr AND
                              signs.code-val  EQ STRING(op.op)
         NO-LOCK:
             FIND FIRST op 
                  WHERE op.op EQ INT64(signs.surr) 
                  EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
             IF AVAIL op THEN
                RUN upd-status.
         END.
         IF RETURN-VALUE BEGINS "ERR" THEN UNDO MAIN, LEAVE MAIN.
   END.
   RETURN RETURN-VALUE.
END PROCEDURE.
/*---------------------------------------------------*/
/* процедура изменения статуса                       */
/*---------------------------------------------------*/
PROCEDURE upd-status:

   DEF VAR vLockErr  AS CHAR NO-UNDO.
   DEFINE BUFFER u-op FOR op.
   DEFINE BUFFER u-oe FOR op-entry.

   FIND FIRST u-op WHERE recid(u-op) EQ recid(op) EXCLUSIVE-LOCK NO-ERROR no-wait.
   IF AVAILABLE(u-op) THEN 
      ASSIGN status-before  = u-op.op-status
             u-op.op-status = status-done.

   FOR EACH op-entry OF op NO-LOCK:
      FIND FIRST u-oe WHERE recid(u-oe) EQ recid(op-entry) EXCLUSIVE-LOCK NO-ERROR no-wait. 
      IF AVAILABLE(u-oe) THEN 
         u-oe.op-status = status-done.
   END.
      
   FOR EACH kau-entry OF op EXCLUSIVE-LOCK:
      kau-entry.op-status = op.op-status.
   END.
      
END PROCEDURE.

/*---------------------------------------------------*/
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='13/01/2015 10:14:26.223+04:00' */
/* $LINTFILE='e-sw.i' */
/*prosignIBrArtn8PQ0+dY21Hq+eXw*/