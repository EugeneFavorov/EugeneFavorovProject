{globals.i}

/* +++ op-ed.p was humbly modified by (c)blodd converter v.1.11 on 6/19/2017 7:03am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: op-ed.p
      Comment: Редактирование документа
   Parameters: in-rec-id, in-op-date, in-user-id, level
         Uses:
      Used by:
      Created: 17.08.1999 Peter
     Modified: 24.10.2000 Om Добавление поля op.ins-date - "Дата поступления"
     Modified: 03/12/2001 NIK Вызов kautrig.p заменен на  Kau-Trigger in h_op.
     Modified: 15.11.2002 Gunk Длина счета
     Modified: 17/06/2003 Gunk Еще раз длина счета
     Modified: 08/06/2004 kraw (0029512) Если через "банк-клиент" было введено без МФО
     Modified: 27/08/2007 kraw (0077186) блокировка счетов клиентов (запрет резактирования)
     Modified: 12/12/2007 kraw (0085886) более информативное сообщение о блокировке счетов клиентов 
     Modified: 03.06.2008 14:05 KSV      (0085464) Незнач. исправление     
*/

{globals.i}
{defopupd.i}
{intrface.get xclass}
{chkopmf.i}

&GLOBAL-DEFINE rec-label  "БАНК-ПОЛУЧАТЕЛЬ"
&GLOBAL-DEFINE send-label "БАНК-ОТПРАВИТЕЛЬ"
&GLOBAL-DEFINE ben-format 2
&GLOBAL-DEFINE mBankCodeType mBankCodeType

DEFINE INPUT PARAMETER in-rec-id  AS RECID   NO-UNDO.
DEFINE INPUT PARAMETER in-op-date AS DATE    NO-UNDO.
DEFINE INPUT PARAMETER in-user-id LIKE op-entry.user-id .
DEFINE INPUT PARAMETER level      AS INT64 NO-UNDO.

DEFINE BUFFER bank1 FOR banks.
DEFINE BUFFER bank2 FOR banks.
DEFINE BUFFER xop   FOR op.

DEFINE VARIABLE vmfo          LIKE op-bank.bank-code                 NO-UNDO.
DEFINE VARIABLE vcorr-acct    LIKE op-bank.corr-acct FORMAT "x(35)"  NO-UNDO.
DEFINE VARIABLE mci-reference LIKE op-bank.corr-acct                 NO-UNDO.

DEFINE VARIABLE dockind AS CHARACTER INITIAL {&rec-label} NO-UNDO.
DEFINE VARIABLE flager  AS INT64                        NO-UNDO.
DEFINE VARIABLE mV-is-nalog AS LOGICAL NO-UNDO.

DEFINE VARIABLE flag-error  AS INT64                        NO-UNDO.

DEFINE VARIABLE mIsVMFO_F1 AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE hproc      AS HANDLE             NO-UNDO.
DEFINE VARIABLE h_frame    AS HANDLE             NO-UNDO.

DEFINE VARIABLE mMfoOriginal  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBankNameOrig AS CHARACTER NO-UNDO.
DEFINE VARIABLE opedUIN1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE opedUIN2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE opeddetails   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDate107b     AS DATE       NO-UNDO.
DEFINE VARIABLE mDate107e     AS DATE       NO-UNDO.
DEFINE VARIABLE m107n         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mKpp-rec      AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE mCanEdit AS LOGICAL INITIAL NO NO-UNDO.


{intrface.get cmp}
{intrface.get xclass}
{intrface.get op}
{intrface.get acct}
{intrface.get tmess}
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */
{op-ed.frm edit}
{op.trg &FRAME-NAME=edit}
{g-error.def}
{intrface.get cust}
{gkh-signs.i &def-var=YES} 

mDate107b = DATE("01.01.14").
mDate107e = DATE("01.04.14").
   
IF in-op-date >= mDate107b AND in-op-date <  mDate107e THEN
   m107n = "YES".

FUNCTION g-checkbank RETURNS LOGICAL (INPUT vmfo       AS CHARACTER,
                                      INPUT iCodeType  AS CHARACTER,
                                      INPUT vcorr-acct AS CHARACTER,
                                      INPUT benacct    AS CHARACTER,
                                      OUTPUT RESULT    AS INT64,
                                      OUTPUT msg       AS CHARACTER)
   IN hproc.

PROCEDURE CheckBenAcct :

   DEFINE INPUT  PARAM iBenAcct  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iVMFO     AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iCorrAcct AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAM oOk       AS LOG  NO-UNDO.

   DEFINE VARIABLE vBenLen AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen    AS CHARACTER  NO-UNDO.


   ASSIGN
      oOk     = YES
      vBenLen = LENGTH (STRING (" ",GetCodeMisc ("КодБанка", mBankCodeType, {&ben-format})))
   NO-ERROR.

   IF ERROR-STATUS:ERROR OR vBenLen =  ? THEN
   DO:
      vBenLen = 20. /* По умолчанию */
      /* Ругань? */
   END.
   DO WITH FRAME edit:
      IF iBenAcct <> "" AND LENGTH (iBenAcct) <> vBenLen THEN
      DO:
         pick-value = "NO".
         {message1
            &text    = "|Неверная длина Р/с|Продолжить?"
            &buttons = YES-NO
         }
         IF pick-value =  "NO" THEN oOk = NO.
      END.

      /* Ключ проверяем только для российских счетов */
      IF oOk AND vBenLen <= 20 AND mBankCodeType =  'МФО-9' THEN 
      DO:
         iBenAcct = SUBSTR (iBenAcct,1,vBenLen).
         {key-tst.i iBenAcct ivmfo icorracct "/*" " " " WITH FRAME edit"}
      END.
   END.
END PROCEDURE.

/* Сработает только в тех полях, которых нет в {&lookup} */
ON 'F1':U OF FRAME Edit ANYWHERE DO:
   { realtrig.i &name = LABEL }
END.

PROCEDURE OnEntry_Op.Details:
   DEFINE VARIABLE t-details LIKE op.details NO-UNDO.
   t-details = SELF:SCREEN-VALUE.
   RUN p-detail.p (RECID (op),INPUT-OUTPUT t-details).
   SELF:SCREEN-VALUE = t-details.

END PROCEDURE.

{opdetail.i Edit}

ON "F6" ANYWHERE DO:

   IF TRANSACTION AND {&FRAME_NAME} = "edit" THEN DO TRANSACTION:
      DO:
         mblodd_char_Tmp01 = pick-value.
         RUN Fill-AlertSysMes IN h_tmess("","",4,"Изменить роль банка?").
         DEFINE VARIABLE opreg_choice AS LOGICAL NO-UNDO.
         opreg_choice = (pick-value = "YES").
         pick-value = mblodd_char_Tmp01.
      END.


      IF NOT opreg_choice OR opreg_choice =  ? THEN
         RETURN "NO-APPLY".

      IF op.doc-kind = "rec" OR op.doc-kind = "" THEN
         ASSIGN
            op.doc-kind = "send"
            dockind    = {&send-label}
         .
      ELSE
         ASSIGN
            op.doc-kind = "rec"
            dockind    = {&rec-label}
         .
      DISPLAY dockind WITH FRAME edit.
      {return_no_apply.i}
   END.
END.

h_frame = FRAME edit:HANDLE.
ON ENTRY OF FRAME edit DO:
    RUN GetFrameFields IN h_cmp (FRAME edit:HANDLE).
END.

ON "GO":U,"ctrl-g":U OF FRAME edit ANYWHERE DO:
   IF NOT mV-is-nalog THEN DO:
     {gkh-signs.i &run-prc=YES &retline=YES }
   END.

   DEFINE VARIABLE wh      AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE vOk     AS LOGICAL       NO-UNDO.
   DEFINE VARIABLE vResult AS INT64       NO-UNDO.
   DEFINE VARIABLE vMsg    AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE vDate   AS DATE          NO-UNDO.

   vDate = DATE(GetXAttrValueEx("op",STRING(op.op),"ДатаПомещенияВКарт","")) NO-ERROR.
   IF vDate =  ? THEN
      vDate = DATE(op.doc-date:SCREEN-VALUE) NO-ERROR.
   IF NOT CAN-FIND(FIRST code WHERE code.class =  "order-pay" 
                                AND code.CODE  =  op.order-pay:SCREEN-VALUE
                                AND ((NOT {assigned code.val}) OR (vDate =  ?) OR DATE(code.val) >= vDate)) 
   THEN DO:
      RUN Fill-AlertSysMes IN h_tmess("","",-1,"Код очередности платежа недействителен на дату" + CHR(32) + STRING(vDate)).

      APPLY "ENTRY" TO op.order-pay.    
      {return_no_apply.i}
   END.
   
   RUN CheckBenAcct (            op.ben-acct:SCREEN-VAL,
                     INPUT INPUT vmfo, 
                     INPUT INPUT vcorr-acct, 
                     OUTPUT      vOk).
   IF NOT vok THEN 
   DO:
      APPLY "entry" TO op.ben-acct IN FRAME edit.
      {return_no_apply.i}
   END.
   RUN CheckFields in h_xclass (FRAME edit:HANDLE, "",op.class-code,OUTPUT wh).
   IF {&RETURN_VALUE} <> "" THEN 
   DO:
      APPLY "entry" TO wh.
      {return_no_apply.i}
   END.

   IF INPUT vmfo =  "" THEN
   DO:
      IF mBankNameOrig =  "" THEN
         vcorr-acct:SCREEN-VALUE = "".
   END.
   ELSE IF NOT g-checkbank(INPUT INPUT vmfo,
                           mBankCodeType,
                           INPUT INPUT vcorr-acct, 
                           INPUT INPUT op.ben-acct, 
                           OUTPUT vResult, 
                           OUTPUT vMsg) THEN
   DO:
      CASE vResult:
         WHEN {&EGMissingMFO} OR WHEN {&EGMissingBank} THEN
         DO:
            RUN Fill-AlertSysMes IN h_tmess("","",-1,"~n" + CHR(32) + STRING(vMsg) + CHR(32) + FILL("~n",1)).

            APPLY "entry" TO vmfo.
         END.
         WHEN {&EGMissingCorrAcct} OR WHEN {&EGBadCorrAcct} THEN 
            IF mBankCodeType =  "МФО-9" OR mBankCodeType =  "" THEN
            DO:
               RUN Fill-AlertSysMes IN h_tmess("","",-1,"~n" + CHR(32) + STRING(vMsg) + CHR(32) + FILL("~n",1)).

               APPLY "entry" TO vcorr-acct.
            END.
            ELSE
               RETURN.
         OTHERWISE 
            RETURN.
      END CASE.
      {return_no_apply.i}
   END.

   IF INPUT vmfo <> "" THEN DO:
      {getbank.i bank1 "INPUT vmfo" mBankCodeType}
      RUN Check-Bank IN h_op (BUFFER bank1,TRUE, OUTPUT flag-error).
      IF NOT flag-error =  0 THEN 
         {return_no_apply.i}
   END.

   IF Chksgnopint(INPUT op.op) THEN DO:
      {message1
          &text    =  "Документ создан транзакцией "" + STRING(op.op-kind) +  "",|продолжить изменение (сумма документа может 
          |не совпасть с суммой комиссии по счету 47423)?".
          &buttons = YES-NO}
      IF pick-value =  "NO" THEN  {return_no_apply.i}
   END.
   PAUSE 0.
   RETURN.
END.

ON 'LEAVE':U OF op.ben-acct IN FRAME edit DO:

   DEFINE VARIABLE vOk AS LOGICAL    NO-UNDO.

   RUN CheckBenAcct (SELF:SCREEN-VAL,INPUT INPUT vmfo, INPUT INPUT vcorr-acct,OUTPUT vOk).

   IF NOT vOk
      THEN {return_no_apply.i}
      ELSE RETURN.
END.

ON 'LEAVE':U OF op.order-pay IN FRAME edit DO:
   DEFINE VARIABLE vDate AS DATE NO-UNDO.

   ASSIGN op.doc-date.
   IF NOT CAN-FIND(FIRST code WHERE code.class =  "order-pay" AND code.CODE =  SELF:SCREEN-VAL) THEN DO:
      &IF DEFINED(SESSION-REMOTE) &THEN
      RUN Fill-AlertSysMes IN h_tmess("","",1,"Нет такого кода очередности платежа").

      &ELSE
      MESSAGE "Нет такого кода очередности платежа".
      &ENDIF      
      {return_no_apply.i}
   END.
   ELSE DO:
      vDate = DATE(GetXAttrValueEx("op",STRING(op.op),"ДатаПомещенияВКарт","")) NO-ERROR.
      IF vDate =  ? THEN
         vDate = op.doc-date NO-ERROR.
      IF NOT CAN-FIND(FIRST code WHERE code.class =  "order-pay" 
                                   AND code.CODE  =  SELF:SCREEN-VAL
                                   AND ((NOT {assigned code.val}) OR (vDate =  ?) OR DATE(code.val) >= vDate)) 
      THEN DO:
         &IF DEFINED(SESSION-REMOTE) &THEN
         RUN Fill-AlertSysMes IN h_tmess("","",1,"Код очередности платежа недействителен на дату" + CHR(32) + STRING(vDate)).

         &ELSE
         MESSAGE "Код очередности платежа недействителен на дату" vDate.
         &ENDIF      
         {return_no_apply.i}
      END.  
   END. 
END.

/* Вставка Плюс банк */
ON "VALUE-CHANGED":U OF op.name-ben
DO:
   UpdateSigns("op", STRING(op.op), "ben-id", "", NO).
END.
/* Конец вставки Плюс банк */
ON "F1":U OF op.name-ben
DO:
   IF NOT CAN-FIND(FIRST op-kind OF op WHERE op-kind.proc =  "g-midlds") AND 
      NOT CAN-DO(FGetSetting("ТранзВызовКарт","",""),op.op-kind)
   THEN DO:
   /* если документ порожден процедурой не g-midlds */
      RUN "cli-r(ot.p" ("", 1, 4).
      IF {&KEY_FUNCTION}({&LAST_KEY}) <> "END-ERROR" THEN DO:
         IF pick-value <> ? THEN DO:
/* Вставка Плюс банк */
            UpdateSigns("op", STRING(op.op), "ben-id", pick-value, NO).
/* Конец вставки Плюс банк */
            IF pick-value BEGINS "Ю," THEN DO:
               FIND FIRST cust-corp WHERE cust-corp.cust-id = INT64(ENTRY(2, pick-value)) 
               NO-LOCK NO-ERROR.
            END.
            ELSE IF pick-value BEGINS "Ч," THEN do:
               FIND FIRST person WHERE person.person-id = INT64(ENTRY(2, pick-value)) 
               NO-LOCK NO-ERROR.
            END.
            ELSE IF pick-value BEGINS "Б," THEN DO:
               FIND FIRST banks WHERE banks.bank-id = INT64(ENTRY(2, pick-value)) NO-LOCK NO-ERROR.
            END.
         END.
      END.
      IF AVAIL cust-corp THEN DO:
         DISPLAY cust-corp.inn @ op.inn
                 cust-corp.cust-stat + " " + cust-corp.name-corp @ op.name-ben
                 GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id),"КПП","") @ mKpp-rec
         WITH FRAME edit.
      END.
      ELSE IF AVAIL person THEN DO:
         DISPLAY person.name-last + " " + person.first-name @ op.name-ben
                 person.inn @ op.inn
                 GetXattrValueEx("person", STRING(person.person-id),"КПП","") @ mKpp-rec
         WITH FRAME edit.
      END.
      ELSE IF AVAIL banks THEN DO:
         RUN GetCustIdent("Б", banks.bank-id, ?, ?, "ИНН", OUTPUT op.inn).
         DISPLAY banks.name @ op.name-ben
                 op.inn
                 GetXattrValueEx("banks", STRING(banks.bank-id),"КПП","") @ mKpp-rec
         WITH FRAME edit.
      END.
   END.
   ELSE DO:
      pick-value = ?.
      IF INPUT vMFO <> "" THEN DO:
         IF INPUT op.ben-acct <> "" THEN
            RUN cli-ds.p ("", 1,
                          INPUT STRING(INT64(INPUT vMFO), "999999999") +
                          "," + INPUT op.ben-acct, Level + 1).
         ELSE 
            RUN cli-ds.p ("", 1, INPUT STRING(INT64(INPUT vMFO), "999999999"),  Level + 1).
      END.
      ELSE DO:
         RUN cli-ds.p ("", 1, "",  Level + 1).
      END.

      FOR FIRST code WHERE code.class =  "recipient" AND code.code = pick-value NO-LOCK:
         DISPLAY
            code.name @ op.name-ben
            code.val  @ op.inn
            code.misc[3] @ mKpp-rec
         WITH FRAME edit.
      END.
   END.

   RETURN.
END.

RUN "g-func.p" PERSISTENT SET hproc.

DEFINE VARIABLE vChClassChilds AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDocTypeDig    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOrderPay      AS INT64   NO-UNDO.
DEFINE VARIABLE vCustNameLine  AS CHARACTER NO-UNDO.

                        /* Проверка прав доступа к объектам подчиненных */
IF     in-user-id <> USERID ("bisquit")
   AND NOT GetSlavePermission (USERID ("bisquit"), in-user-id, "w")
THEN RETURN.


vChClassChilds = GetXclassAllChilds("opv").

FIND FIRST op WHERE RECID(op) =  in-rec-id NO-LOCK NO-ERROR.
IF CheckSrc(op.class-code,op.op) OR CheckTrg(op.class-code,op.op,mTargetId) THEN DO:
   RUN Fill-SysMes IN h_tmess ("", "", "", "ВНИМАНИЕ! Редактирование документа невозможно. Есть связанный входящий документ в другом филиале.").
   RETURN.
END.


RUN chk-ed-klb.p(op.op,OUTPUT mCanEdit).
IF NOT mCanEdit THEN DO:
   RUN Fill-SysMes IN h_tmess ("", "", "", "ВНИМАНИЕ! Редактирование документа невозможно.~n Документ из клиент-банка.").
   RETURN.
END.




RUN GetDocTypeDigital IN h_op (op.doc-type, ?, OUTPUT vDocTypeDig).
vOrderPay = INT64(op.order-pay) NO-ERROR.
IF  vDocTypeDig <> "02"
AND vDocTypeDig <> "06"
AND (       (vOrderPay = ?)
     OR NOT (vOrderPay >= 1 AND vOrderPay <= 3))
   THEN FOR EACH op-entry OF op NO-LOCK:
   RELEASE acct.
   {find-act.i
      &bact = acct
      &acct = op-entry.acct-db
      &curr = op-entry.currency
   }
   IF AVAILABLE acct THEN DO:
      RUN chk-blk.p (acct.cust-cat,acct.cust-id).
      IF {&RETURN_VALUE} =  "0" THEN DO:
         {getcustline.i &cust-cat = "acct.cust-cat" &cust-id = "acct.cust-id" &output-to = "vCustNameLine"}
         RUN Fill-SysMes IN h_tmess ("", "acct43", "", "%s=" + vCustNameLine + "%s=" + STRING(acct.number,GetAcctFmt(acct.acct-cat))).
         RETURN.
      END.
   END.
END.

RUN isnalop.p(RECID(op), OUTPUT mV-is-nalog).

IF AVAILABLE op THEN
   RELEASE op.
RUN SetSysConf IN h_base ("PROCESS_OP-EDIT","Да").
{rec-ed.i
  &file=op
  &ef="op#.lf"
  &nocols=Yes
  &preset="op#.fnd "
  &editing="op.ed "
  &lookup="op.nau "
  &update="op#.upd "
    &undo="undo outr, retry outr"
  &access-class=op.class-code
  &access-surr=string(op)
}
RUN SetSysConf IN h_base ("PROCESS_OP-EDIT",?).
DELETE PROCEDURE(hproc).

{intrface.del}
/* $LINTFILE='op-ed.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='07/04/2017 12:16:12.088+03:00' */
/*prosignlNEtO3lZoMJcQcr4z0mjeQ*/
/* --- op-ed.p was humbly modified by (c)blodd converter v.1.11 on 6/19/2017 7:03am --- */
