DEFINE NEW SHARED VAR hist-rec-acct AS RECID INITIAL ? NO-UNDO.
DEFINE NEW SHARED VAR hist-rec-kau  AS RECID INITIAL ? NO-UNDO.

DEFINE VARIABLE hproc            AS HANDLE     NO-UNDO.
DEFINE VARIABLE vDebugXAttr      AS LOGICAL    NO-UNDO. /* выводить к редактированию доп.реквизиты ? */

{sh-defs.i NEW}

RUN "g-func.p" PERSISTENT SET hproc.

{lim-pos.i}
  
{xattr-cr.i &no-run-xattr-cr-proc=YES}
{copyxtr.i}

{getrest.fun}

PROCEDURE Карт-ка2: /*Выгрузка данных по картотеке2 в системную таблицу */
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO. /**/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*Флаг ошибки        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*Сообщение об ошибке*/

   DEFINE BUFFER o_op   FOR op.
   DEFINE BUFFER o_open FOR op-entry.
   DEFINE BUFFER b_op   FOR op.
   DEFINE BUFFER b_open FOR op-entry.
   DEFINE BUFFER b_kau  FOR kau.
   DEFINE BUFFER b_opbnk FOR op-bank.

   DEFINE BUFFER top-entry    FOR op-entry.
   DEFINE BUFFER dop-entry    FOR op-entry.
   DEFINE BUFFER bAcct        FOR acct.
   
   DEFINE VARIABLE vSum       AS DEC  NO-UNDO. 

   DEFINE VARIABLE vOpBalChar AS CHARACTER NO-UNDO. /*Балансовый документ на картотеке*/

   DEFINE VARIABLE vAcctDB    AS CHARACTER NO-UNDO. /*Счет ДБ Балансового документа на картотеке*/
   DEFINE VARIABLE vAcctCR    AS CHARACTER NO-UNDO. /*Счет КР Балансового документа на картотеке*/

   DEFINE VARIABLE vSummSpis  AS DECIMAL   NO-UNDO. /*Сумма для списания*/
   DEFINE VARIABLE vTmpSumm   AS DECIMAL   NO-UNDO. /*Временная переменная*/
   DEFINE VARIABLE vCardbAcct AS CHARACTER NO-UNDO. /*Временная переменная*/
   DEFINE VARIABLE vDocType   AS CHARACTER NO-UNDO.  /*Код ЦБ документа*/
 
   FIND b_kau WHERE RECID(b_kau) EQ ipKauRecId NO-LOCK NO-ERROR.

   RUN SetSysConf in h_base ("КАРТ2ОБРАБОТКА",STRING(RECID(b_kau))).
   RUN SetSysConf in h_base ("БАЛ-ДОК:OP-BAL" ,  "").
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-KIND" ,"").
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-NUM"  ,"").
   RUN SetSysConf in h_base ("БАЛ-ДОК:ORDER-PAY","").
   RUN SetSysConf in h_base ("БАЛ-ДОК:БАЛАНСОВЫЙ","").
   RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-DB"  ,"").
   RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-CR"  ,"").

   RUN SetSysConf in h_base ("БАЛ-ДОК:BEN-ACCT" ,"").
   RUN SetSysConf in h_base ("БАЛ-ДОК:NAME-BEN" ,"").
   RUN SetSysConf in h_base ("БАЛ-ДОК:INN"      ,"").
   RUN SetSysConf in h_base ("БАЛ-ДОК:MFO"      ,"").

   RUN SetSysConf in h_base ("БАЛ-ДОК:DETAILS"  ,"").
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:СУММА"    ,"").
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ОСТАТОК"  ,"").
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ВАЛЮТА"   ,"").
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:СУММСПИС"   ,"").
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ДАТА","").
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ДАТАСПИС","").
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:КАРТНАИМ" ,"").
   RUN SetSysConf in h_base ("БАЛ-ДОК:БАНК-РЕКВ",""). /*Обязательно чистим*/

   RUN SetSysConf in h_base ("БАЛ-ДОК:КОД-ДОК","").       /*Sami*/
   RUN SetSysConf in h_base ("БАЛ-ДОК:РАСЧ-СЧ","").       /*Sami*/
   RUN SetSysConf in h_base ("БАЛ-ДОК:СУМ-НДС","").       /*Sami*/
   RUN SetSysConf in h_base ("БАЛ-ДОК:DATA-CARD","").     /*Sami*/

/*ВНЕБАЛАНСОВЫЙ ДОКУМЕНТ КАРТОТЕКИ*/

   FIND o_open WHERE o_open.op EQ INT64(ENTRY(1,b_kau.kau))
                 AND o_open.op-entry EQ INT64(ENTRY(2,b_kau.kau))
                                               NO-LOCK NO-ERROR.
   FIND o_op OF o_open NO-LOCK NO-ERROR.
   IF NOT AVAIL o_op THEN DO:
      ASSIGN
         opErrLogc  = YES
         opMessChar = "Внебалансовый документ постановки не найден."
      .
      RETURN.
   END.
   /*БАЛАНСОВЫЙ ДОКУМЕНТ КАРТОТЕКИ*/

   vOpBalChar = GetXAttrValueEx("op",
                                STRING(o_op.op),
                                "op-bal",
                                "").
   IF vOpBalChar NE "" THEN
      FIND b_op WHERE b_op.op EQ INT64(vOpBalChar)
                                    NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST b_op WHERE b_op.op-transaction EQ o_op.op-transaction
                        AND b_op.acct-cat       EQ "b"
                        AND RECID(b_op)         NE RECID(o_op)
                                                      NO-LOCK NO-ERROR.
   IF AVAIL b_op AND CAN-FIND(FIRST b_opbnk OF b_op) THEN
   RUN SetSysConf in h_base ("БАЛ-ДОК:БАНК-РЕКВ",STRING(b_op.op)).
   ELSE IF AVAIL o_op AND CAN-FIND(FIRST b_opbnk OF o_op) THEN
   RUN SetSysConf in h_base ("БАЛ-ДОК:БАНК-РЕКВ",STRING(o_op.op)).

   IF AVAIL b_op THEN
      RUN SetSysConf in h_base ("БАЛ-ДОК:DR",GetXattrValueEx("op",STRING(b_op.op),"Счет-Фактура","")).       /*Sami*/
      RUN SetSysConf in h_base ("БАЛ-ДОК:ADR",GetXattrValueEx("op",STRING(b_op.op),"Адрес_пок","")).       /*Sami*/
      RUN SetSysConf in h_base ("БАЛ-ДОК:KPP",GetXattrValueEx("op",STRING(b_op.op),"Kpp-send","")).       /*Sami*/
      RUN SetSysConf in h_base ("БАЛ-ДОК:TEL",GetXattrValueEx("op",STRING(b_op.op),"Телеф_пок","")).       /*Sami*/
      RUN SetSysConf in h_base ("БАЛ-ДОК:STAV-NDS",GetXattrValueEx("op",STRING(b_op.op),"Ставка","")).       /*Sami*/
      RUN SetSysConf in h_base ("БАЛ-ДОК:ED-IZM",GetXattrValueEx("op",STRING(b_op.op),"ЕдИзмерения","")).       /*Sami*/
      RUN SetSysConf in h_base ("БАЛ-ДОК:KOL-VO",GetXattrValueEx("op",STRING(b_op.op),"Кол-во","")).       /*Sami*/
      RUN SetSysConf in h_base ("БАЛ-ДОК:DATA-CARD" ,IF AVAIL b_op THEN STRING(b_op.doc-date, "99/99/9999")  ELSE "").  /*Sami*/

    
   /*****************************************************************************/

   vAcctDB = GetXattrValueEX("op",STRING(IF AVAIL b_op THEN b_op.op ELSE o_op.op),"acctbal","").
   vAcctCR = GetXattrValueEX("op",STRING(IF AVAIL b_op THEN b_op.op ELSE o_op.op),"acctcorr","").
   /*Расчет суммы списания по балансовому счету и внебалансовому  документу ,
     выбирается меньшая сумма*/
   vSummSpis = GetRest(RECID(b_kau),(IF in-op-date EQ TODAY THEN
                                         DATETIME(TODAY,MTIME)
                                     ELSE 
                                         DATETIME(in-op-date + 1)) - 1).
   {find-act.i
     &bact   = bAcct
     &acct   = vAcctDB
   }
   IF AVAIL bAcct THEN   
     vSummSpis = vSummSpis + GetOverLimit(BUFFER bAcct, in-op-date).

   vTmpSumm = IF b_kau.currency EQ "" THEN b_kau.balance ELSE b_kau.curr-bal.

   vSummSpis = IF vSummSpis NE ?          AND
                  vSummSpis LT vTmpSumm
               THEN vSummSpis
               ELSE vTmpSumm.

   /**********************************************************/

   /*Установка переменных для использования парсером через СисПарам*/
   RUN SetSysConf in h_base ("БАЛ-ДОК:OP-BAL" ,  IF AVAIL b_op THEN STRING(b_op.op) ELSE "").
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-KIND" ,IF AVAIL b_op THEN b_op.doc-kind  ELSE "").
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-NUM"  ,IF AVAIL b_op THEN b_op.doc-num   ELSE "").
   RUN SetSysConf in h_base ("БАЛ-ДОК:ORDER-PAY",IF AVAIL b_op THEN b_op.order-pay ELSE o_op.order-pay).
   RUN SetSysConf in h_base ("БАЛ-ДОК:DOC-TYPE", IF AVAIL b_op THEN STRING(b_op.doc-type) ELSE "").

   RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-DB"  ,vAcctDB).
   RUN SetSysConf in h_base ("БАЛ-ДОК:ACCT-CR"  ,vAcctCR).
   RUN SetSysConf in h_base ("БАЛ-ДОК:БАЛАНСОВЫЙ",vAcctDB).
   RUN SetSysConf in h_base ("БАЛ-ДОК:BEN-ACCT" ,IF AVAIL b_op THEN b_op.ben-acct  ELSE o_op.ben-acct).
   RUN SetSysConf in h_base ("БАЛ-ДОК:NAME-BEN" ,IF AVAIL b_op THEN b_op.name-ben  ELSE o_op.name-ben).
   RUN SetSysConf in h_base ("БАЛ-ДОК:INN"      ,IF AVAIL b_op THEN b_op.inn       ELSE o_op.inn).

   RUN SetSysConf in h_base ("БАЛ-ДОК:DETAILS"  ,IF AVAIL b_op THEN b_op.details   ELSE o_op.details).

   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ACCT-DB"  ,b_kau.acct).
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ACCT-CR"  ,o_open.acct-cr).
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:СУММА"    ,IF o_open.currency EQ "" THEN o_open.amt-rub ELSE o_open.amt-cur).
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ОСТАТОК"  ,IF b_kau.currency EQ "" THEN b_kau.balance ELSE b_kau.curr-bal).
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ВАЛЮТА"   ,o_open.currency).

   RUN SetSysConf in h_base ("ВНЕБ-ДОК:СУММСПИС" ,vSummSpis).
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ДАТА"     ,STRING(o_open.op-date, "99/99/9999")).
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:ДАТАСПИС" ,STRING(in-op-date, "99/99/9999")).
   RUN SetSysConf in h_base ("ВНЕБ-ДОК:КАРТНАИМ" ,"К2").

   RUN SetSysConf in h_base ("БАЛ-ДОК:КОД-ДОК"   ,IF AVAIL b_op THEN b_op.doc-type      ELSE "").  /*Sami*/
   RUN SetSysConf in h_base ("БАЛ-ДОК:РАСЧ-СЧ"   ,IF AVAIL b_op THEN b_op.ben-acct      ELSE "").  /*Sami*/
    
   RUN SetSysConf in h_base("ВНЕБ-ДОК:ДОСТОСТ", GetRest(RECID(b_kau),(IF in-op-date EQ TODAY THEN DATETIME(TODAY,MTIME) ELSE DATETIME(in-op-date + 1)) - 1)).

   /* меняем дату если полное списание */
   RUN GetDocTypeDigitalEx IN h_op (wop.doc-type, ?, OUTPUT vDocType).
   IF AVAIL wop AND CAN-DO("01,02,06",STRING(vDocType)) THEN
   DO:
       op.doc-date = b_op.doc-date.
       op.ins-date = b_op.doc-date.
   END. 
   ELSE DO:
       op.doc-date = gend-date.
       op.ins-date = gend-date.
   END.
   /*******/

   FOR EACH top-entry WHERE top-entry.acct-cr EQ o_open.acct-db
       AND top-entry.kau-cr EQ o_open.kau-db NO-LOCK:
      FOR EACH dop-entry WHERE dop-entry.op-transaction EQ top-entry.op-transaction 
          AND dop-entry.acct-cr BEGINS "60309" NO-LOCK:
          vSum = vSum + dop-entry.amt-rub.
      END.
   END.

   vCardbAcct =  entry(1, GetXAttrValue("acct", vAcctDB + ',' ,"КартБВнСчет")).

   FOR EACH top-entry WHERE top-entry.acct-cr EQ vCardbAcct
       AND top-entry.kau-cr EQ o_open.kau-db NO-LOCK:
      FOR EACH dop-entry WHERE dop-entry.op-transaction EQ top-entry.op-transaction 
          AND dop-entry.acct-cr BEGINS "60309" NO-LOCK:
          vSum = vSum + dop-entry.amt-rub.
      END.
   END.

   RUN SetSysConf in h_base ("БАЛ-ДОК:СУМ-НДС"   ,vSum).                                  /*Sami*/

END PROCEDURE.

PROCEDURE Карт2БалТ:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**Строка параметров*/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*Флаг ошибки        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*Сообщение об ошибке*/
   DEFINE VARIABLE vTmp AS INT64 NO-UNDO. /*?*/
   DEFINE BUFFER buf_opbnk FOR op-bank.
   RUN Карт-ка2(ipKauRecId,OUTPUT opErrLogc,OUTPUT opMessChar).            /*Выгрузка информации о постановке в SysConf*/
   IF opErrLogc THEN RETURN.

   op.doc-num           = GetSysConf("БАЛ-ДОК:DOC-NUM").
   op.order-pay         = GetSysConf("БАЛ-ДОК:ORDER-PAY").

   UpdateSigns("opbf",string(op.op),"Счет-Фактура",GetSysConf("БАЛ-ДОК:DR"),?).
   UpdateSigns("opbf",string(op.op),"Адрес_пок",GetSysConf("БАЛ-ДОК:ADR"),?).
   UpdateSigns("opbf",string(op.op),"ЕдИзмерения",GetSysConf("БАЛ-ДОК:ED-IZM"),?).
   UpdateSigns("opbf",string(op.op),"Кол-во",GetSysConf("БАЛ-ДОК:KOL-VO"),?).
   UpdateSigns("opbf",string(op.op),"Kpp-send",GetSysConf("БАЛ-ДОК:KPP"),?).
   UpdateSigns("opbf",string(op.op),"Ставка",GetSysConf("БАЛ-ДОК:STAV-NDS"),?).
   UpdateSigns("opbf",string(op.op),"Телеф_пок",GetSysConf("БАЛ-ДОК:TEL"),?).

END PROCEDURE.

PROCEDURE Карт2БалБ:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**Строка параметров*/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*Флаг ошибки        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*Сообщение об ошибке*/

   DEFINE VARIABLE vTmp        AS INT64   NO-UNDO. /*?*/

   DEFINE BUFFER buf_opbnk FOR op-bank.
   DEFINE BUFFER buf_op    FOR op.

   RUN Карт-ка2(ipKauRecId,OUTPUT opErrLogc,OUTPUT opMessChar).            /*Выгрузка информации о постановке в SysConf*/
   IF opErrLogc THEN RETURN.

   op.doc-num           = GetSysConf("БАЛ-ДОК:DOC-NUM").
   op.order-pay         = GetSysConf("БАЛ-ДОК:ORDER-PAY").

   op.doc-kind          = GetSysConf("БАЛ-ДОК:DOC-KIND").
   op.ben-acct          = GetSysConf("БАЛ-ДОК:BEN-ACCT").
   op.name-ben          = GetSysConf("БАЛ-ДОК:NAME-BEN").
   op.inn               = GetSysConf("БАЛ-ДОК:INN").
      
   UpdateSigns("opb",string(op.op),"ДатаПомещенияВКарт",GetSysConf("БАЛ-ДОК:DATA-CARD"),?).

   IF GetSysConf("БАЛ-ДОК:БАНК-РЕКВ") NE "" AND GetSysConf("БАЛ-ДОК:БАНК-РЕКВ") NE ? THEN DO:
      DO:
         vTmp = INT64(GetSysConf("БАЛ-ДОК:БАНК-РЕКВ")).
         FOR EACH buf_opbnk WHERE buf_opbnk.op EQ vTmp NO-LOCK:
            CREATE op-bank.
            ASSIGN
               op-bank.op              =  op.op
               op-bank.op-bank-type    =  buf_opbnk.op-bank-type
               op-bank.bank-code-type  =  buf_opbnk.bank-code-type
               op-bank.bank-code       =  buf_opbnk.bank-code
               op-bank.bank-name       =  buf_opbnk.bank-name
               op-bank.corr-acct       =  buf_opbnk.corr-acct
            .
         END.
      END.
   END.
END PROCEDURE.


PROCEDURE Карт2ВН:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO. /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**Строка параметров*/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*Флаг ошибки        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*Сообщение об ошибке*/

   DEFINE VARIABLE vTmp AS INT64 NO-UNDO. /*?*/
   DEFINE BUFFER buf_opbnk FOR op-bank.
   RUN Карт-ка2(ipKauRecId,OUTPUT opErrLogc,OUTPUT opMessChar).            /*Выгрузка информации о постановке в SysConf*/
   IF opErrLogc THEN RETURN.
   op-entry.acct-cr     = GetSysConf("ВНЕБ-ДОК:ACCT-DB").
/*   op-entry.amt-rub     = DECIMAL(GetSysConf("ВНЕБ-ДОК:СУММА")).*/
   op-entry.currency    = GetSysConf("ВНЕБ-ДОК:ВАЛЮТА").
   /*Обработка параметров*/
   IF CAN-DO(iParamStr,"DOC-NUM") THEN op.doc-num = GetSysConf("БАЛ-ДОК:DOC-NUM").

   RUN SetSysConf  in h_base ("ДокСписания",STRING(RECID(op-entry)) + "," + STRING(ipKauRecId)).
END PROCEDURE.


PROCEDURE Карт2ПРОВ:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**Строка параметров*/
   DEFINE OUTPUT PARAMETER opFlagLogc AS LOGICAL INITIAL NO NO-UNDO.  /*Флаг ошибки*/
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*Сообщение об ошибке*/

   DEFINE PARAMETER BUFFER b_op-entry FOR op-entry.
      IF (b_op-entry.currency EQ "" AND b_op-entry.amt-rub LE 0) OR
         (b_op-entry.currency NE "" AND b_op-entry.amt-cur LE 0) THEN DO:
         RUN MessTool("Сумма не может быть меньше или равна 0!",
                      "ERROR",
                       vAutoLog,
                       OUTPUT opFlagLogc).
         opFlagLogc = YES.
      END.
      ELSE IF b_op-entry.currency EQ "" AND
              b_op-entry.amt-rub  GT DEC(GetSysConf("ВНЕБ-ДОК:ОСТАТОК")) THEN DO:
         RUN MessTool("Сумма превышает остаток на картотеке!",
                      "ERROR",
                       vAutoLog,
                       OUTPUT opFlagLogc).
         opFlagLogc = YES.
      END.
      ELSE IF b_op-entry.currency NE "" AND
              b_op-entry.amt-cur  GT DEC(GetSysConf("ВНЕБ-ДОК:ОСТАТОК")) THEN DO:
         RUN MessTool("Сумма превышает остаток на картотеке!",
                      "ERROR",
                       vAutoLog,
                       OUTPUT opFlagLogc).
         opFlagLogc = YES.
      END.
END PROCEDURE.

PROCEDURE Карт2БАЛНАЛ:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**Строка параметров*/
   DEFINE OUTPUT PARAMETER opFlagLogc AS LOGICAL INITIAL NO NO-UNDO.  /*Флаг ошибки*/
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*Сообщение об ошибке*/
   DEFINE PARAMETER BUFFER b_op-entry FOR op-entry.

   DEFINE VARIABLE vXattrCopy  AS CHARACTER NO-UNDO.  /*Какие Доп Реки копировать*/
   DEFINE VARIABLE vTmp        AS INT64   NO-UNDO. /*?*/
   DEFINE BUFFER buf_op    FOR op.
   FIND buf_op OF b_op-entry NO-LOCK NO-ERROR.

   RUN Карт-ка2(ipKauRecId,OUTPUT opFlagLogc,OUTPUT opMessChar).            /*Выгрузка информации о постановке в SysConf*/
   IF opFlagLogc THEN RETURN.
   ASSIGN
      vXattrCopy  = GetSysConf("Карт2КопРекв")
      vTmp        = INT64(GetSysConf("БАЛ-ДОК:OP-BAL"))
   .

   FIND buf_op WHERE buf_op.op EQ vTmp NO-LOCK NO-ERROR.
   IF AVAIL buf_op THEN DO:
      RUN Copy-Xattr-Op(RECID(buf_op),RECID(op),"ПокСт,Kpp-send,Kpp-rec,КБК,ОКАТО-НАЛОГ,ПокОП,ПокНП,ПокНД,ПокДД,ПокТП").
      RUN Copy-Xattr-Op(RECID(buf_op),RECID(op),vXattrCopy).
   END.
   RUN inipoxtr.p (RECID(op),?).
   RUN nalpl_ed.p (RECID(op), 2, 3).
END PROCEDURE.

PROCEDURE EDITXATTR:
   {xattr-cr.i &use-in-internal-procedure=YES}
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='29/09/2014 13:37:17.783+04:00' */
/* $LINTFILE='gcrddec3.i' */
/*prosignDR3n2XwCOzcRd7j2a/5Uwg*/