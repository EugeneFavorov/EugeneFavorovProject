DEFINE NEW SHARED VAR hist-rec-acct AS RECID INITIAL ? NO-UNDO.
DEFINE NEW SHARED VAR hist-rec-kau  AS RECID INITIAL ? NO-UNDO.

DEFINE VARIABLE hproc            AS HANDLE     NO-UNDO.
DEFINE VARIABLE vDebugXAttr      AS LOGICAL    NO-UNDO. /* ข๋ขฎคจโ์ ช เฅค ชโจเฎข ญจ๎ คฎฏ.เฅชขจงจโ๋ ? */

{sh-defs.i NEW}

RUN "g-func.p" PERSISTENT SET hproc.

{lim-pos.i}
  
{xattr-cr.i &no-run-xattr-cr-proc=YES}
{copyxtr.i}

{getrest.fun}

PROCEDURE  เโ-ช 2: /*๋ฃเใงช  ค ญญ๋ๅ ฏฎ ช เโฎโฅชฅ2 ข แจแโฅฌญใ๎ โ กซจๆใ */
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO. /**/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*ซ ฃ ฎ่จกชจ        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*ฎฎก้ฅญจฅ ฎก ฎ่จกชฅ*/

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

   DEFINE VARIABLE vOpBalChar AS CHARACTER NO-UNDO. /* ซ ญแฎข๋ฉ คฎชใฌฅญโ ญ  ช เโฎโฅชฅ*/

   DEFINE VARIABLE vAcctDB    AS CHARACTER NO-UNDO. /*็ฅโ   ซ ญแฎขฎฃฎ คฎชใฌฅญโ  ญ  ช เโฎโฅชฅ*/
   DEFINE VARIABLE vAcctCR    AS CHARACTER NO-UNDO. /*็ฅโ   ซ ญแฎขฎฃฎ คฎชใฌฅญโ  ญ  ช เโฎโฅชฅ*/

   DEFINE VARIABLE vSummSpis  AS DECIMAL   NO-UNDO. /*ใฌฌ  คซ๏ แฏจแ ญจ๏*/
   DEFINE VARIABLE vTmpSumm   AS DECIMAL   NO-UNDO. /*เฅฌฅญญ ๏ ฏฅเฅฌฅญญ ๏*/
   DEFINE VARIABLE vCardbAcct AS CHARACTER NO-UNDO. /*เฅฌฅญญ ๏ ฏฅเฅฌฅญญ ๏*/
   DEFINE VARIABLE vDocType   AS CHARACTER NO-UNDO.  /*ฎค  คฎชใฌฅญโ */
 
   FIND b_kau WHERE RECID(b_kau) EQ ipKauRecId NO-LOCK NO-ERROR.

   RUN SetSysConf in h_base ("2",STRING(RECID(b_kau))).
   RUN SetSysConf in h_base ("-:OP-BAL" ,  "").
   RUN SetSysConf in h_base ("-:DOC-KIND" ,"").
   RUN SetSysConf in h_base ("-:DOC-NUM"  ,"").
   RUN SetSysConf in h_base ("-:ORDER-PAY","").
   RUN SetSysConf in h_base ("-:","").
   RUN SetSysConf in h_base ("-:ACCT-DB"  ,"").
   RUN SetSysConf in h_base ("-:ACCT-CR"  ,"").

   RUN SetSysConf in h_base ("-:BEN-ACCT" ,"").
   RUN SetSysConf in h_base ("-:NAME-BEN" ,"").
   RUN SetSysConf in h_base ("-:INN"      ,"").
   RUN SetSysConf in h_base ("-:MFO"      ,"").

   RUN SetSysConf in h_base ("-:DETAILS"  ,"").
   RUN SetSysConf in h_base ("-:"    ,"").
   RUN SetSysConf in h_base ("-:"  ,"").
   RUN SetSysConf in h_base ("-:"   ,"").
   RUN SetSysConf in h_base ("-:"   ,"").
   RUN SetSysConf in h_base ("-:","").
   RUN SetSysConf in h_base ("-:","").
   RUN SetSysConf in h_base ("-:" ,"").
   RUN SetSysConf in h_base ("-:-",""). /*ก๏ง โฅซ์ญฎ ็จแโจฌ*/

   RUN SetSysConf in h_base ("-:-","").       /*Sami*/
   RUN SetSysConf in h_base ("-:-","").       /*Sami*/
   RUN SetSysConf in h_base ("-:-","").       /*Sami*/
   RUN SetSysConf in h_base ("-:DATA-CARD","").     /*Sami*/

/*  */

   FIND o_open WHERE o_open.op EQ INT64(ENTRY(1,b_kau.kau))
                 AND o_open.op-entry EQ INT64(ENTRY(2,b_kau.kau))
                                               NO-LOCK NO-ERROR.
   FIND o_op OF o_open NO-LOCK NO-ERROR.
   IF NOT AVAIL o_op THEN DO:
      ASSIGN
         opErrLogc  = YES
         opMessChar = "ญฅก ซ ญแฎข๋ฉ คฎชใฌฅญโ ฏฎแโ ญฎขชจ ญฅ ญ ฉคฅญ."
      .
      RETURN.
   END.
   /*  */

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
   RUN SetSysConf in h_base ("-:-",STRING(b_op.op)).
   ELSE IF AVAIL o_op AND CAN-FIND(FIRST b_opbnk OF o_op) THEN
   RUN SetSysConf in h_base ("-:-",STRING(o_op.op)).

   IF AVAIL b_op THEN
      RUN SetSysConf in h_base ("-:DR",GetXattrValueEx("op",STRING(b_op.op),"็ฅโ- ชโใเ ","")).       /*Sami*/
      RUN SetSysConf in h_base ("-:ADR",GetXattrValueEx("op",STRING(b_op.op),"คเฅแ_ฏฎช","")).       /*Sami*/
      RUN SetSysConf in h_base ("-:KPP",GetXattrValueEx("op",STRING(b_op.op),"Kpp-send","")).       /*Sami*/
      RUN SetSysConf in h_base ("-:TEL",GetXattrValueEx("op",STRING(b_op.op),"ฅซฅไ_ฏฎช","")).       /*Sami*/
      RUN SetSysConf in h_base ("-:STAV-NDS",GetXattrValueEx("op",STRING(b_op.op),"โ ขช ","")).       /*Sami*/
      RUN SetSysConf in h_base ("-:ED-IZM",GetXattrValueEx("op",STRING(b_op.op),"คงฌฅเฅญจ๏","")).       /*Sami*/
      RUN SetSysConf in h_base ("-:KOL-VO",GetXattrValueEx("op",STRING(b_op.op),"ฎซ-ขฎ","")).       /*Sami*/
      RUN SetSysConf in h_base ("-:DATA-CARD" ,IF AVAIL b_op THEN STRING(b_op.doc-date, "99/99/9999")  ELSE "").  /*Sami*/

    
   /*****************************************************************************/

   vAcctDB = GetXattrValueEX("op",STRING(IF AVAIL b_op THEN b_op.op ELSE o_op.op),"acctbal","").
   vAcctCR = GetXattrValueEX("op",STRING(IF AVAIL b_op THEN b_op.op ELSE o_op.op),"acctcorr","").
   /* แ็ฅโ แใฌฌ๋ แฏจแ ญจ๏ ฏฎ ก ซ ญแฎขฎฌใ แ็ฅโใ จ ขญฅก ซ ญแฎขฎฌใ  คฎชใฌฅญโใ ,
     ข๋กจเ ฅโแ๏ ฌฅญ์่ ๏ แใฌฌ */
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

   /*แโ ญฎขช  ฏฅเฅฌฅญญ๋ๅ คซ๏ จแฏฎซ์งฎข ญจ๏ ฏ เแฅเฎฌ ็ฅเฅง จแ เ ฌ*/
   RUN SetSysConf in h_base ("-:OP-BAL" ,  IF AVAIL b_op THEN STRING(b_op.op) ELSE "").
   RUN SetSysConf in h_base ("-:DOC-KIND" ,IF AVAIL b_op THEN b_op.doc-kind  ELSE "").
   RUN SetSysConf in h_base ("-:DOC-NUM"  ,IF AVAIL b_op THEN b_op.doc-num   ELSE "").
   RUN SetSysConf in h_base ("-:ORDER-PAY",IF AVAIL b_op THEN b_op.order-pay ELSE o_op.order-pay).
   RUN SetSysConf in h_base ("-:DOC-TYPE", IF AVAIL b_op THEN STRING(b_op.doc-type) ELSE "").

   RUN SetSysConf in h_base ("-:ACCT-DB"  ,vAcctDB).
   RUN SetSysConf in h_base ("-:ACCT-CR"  ,vAcctCR).
   RUN SetSysConf in h_base ("-:",vAcctDB).
   RUN SetSysConf in h_base ("-:BEN-ACCT" ,IF AVAIL b_op THEN b_op.ben-acct  ELSE o_op.ben-acct).
   RUN SetSysConf in h_base ("-:NAME-BEN" ,IF AVAIL b_op THEN b_op.name-ben  ELSE o_op.name-ben).
   RUN SetSysConf in h_base ("-:INN"      ,IF AVAIL b_op THEN b_op.inn       ELSE o_op.inn).

   RUN SetSysConf in h_base ("-:DETAILS"  ,IF AVAIL b_op THEN b_op.details   ELSE o_op.details).

   RUN SetSysConf in h_base ("-:ACCT-DB"  ,b_kau.acct).
   RUN SetSysConf in h_base ("-:ACCT-CR"  ,o_open.acct-cr).
   RUN SetSysConf in h_base ("-:"    ,IF o_open.currency EQ "" THEN o_open.amt-rub ELSE o_open.amt-cur).
   RUN SetSysConf in h_base ("-:"  ,IF b_kau.currency EQ "" THEN b_kau.balance ELSE b_kau.curr-bal).
   RUN SetSysConf in h_base ("-:"   ,o_open.currency).

   RUN SetSysConf in h_base ("-:" ,vSummSpis).
   RUN SetSysConf in h_base ("-:"     ,STRING(o_open.op-date, "99/99/9999")).
   RUN SetSysConf in h_base ("-:" ,STRING(in-op-date, "99/99/9999")).
   RUN SetSysConf in h_base ("-:" ,"2").

   RUN SetSysConf in h_base ("-:-"   ,IF AVAIL b_op THEN b_op.doc-type      ELSE "").  /*Sami*/
   RUN SetSysConf in h_base ("-:-"   ,IF AVAIL b_op THEN b_op.ben-acct      ELSE "").  /*Sami*/
    
   RUN SetSysConf in h_base("-:", GetRest(RECID(b_kau),(IF in-op-date EQ TODAY THEN DATETIME(TODAY,MTIME) ELSE DATETIME(in-op-date + 1)) - 1)).

   /* ฌฅญ๏ฅฌ ค โใ ฅแซจ ฏฎซญฎฅ แฏจแ ญจฅ */
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

   vCardbAcct =  entry(1, GetXAttrValue("acct", vAcctDB + ',' ," เโญ็ฅโ")).

   FOR EACH top-entry WHERE top-entry.acct-cr EQ vCardbAcct
       AND top-entry.kau-cr EQ o_open.kau-db NO-LOCK:
      FOR EACH dop-entry WHERE dop-entry.op-transaction EQ top-entry.op-transaction 
          AND dop-entry.acct-cr BEGINS "60309" NO-LOCK:
          vSum = vSum + dop-entry.amt-rub.
      END.
   END.

   RUN SetSysConf in h_base ("-:-"   ,vSum).                                  /*Sami*/

END PROCEDURE.

PROCEDURE  เโ2 ซ:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**โเฎช  ฏ เ ฌฅโเฎข*/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*ซ ฃ ฎ่จกชจ        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*ฎฎก้ฅญจฅ ฎก ฎ่จกชฅ*/
   DEFINE VARIABLE vTmp AS INT64 NO-UNDO. /*?*/
   DEFINE BUFFER buf_opbnk FOR op-bank.
   RUN  เโ-ช 2(ipKauRecId,OUTPUT opErrLogc,OUTPUT opMessChar).            /*๋ฃเใงช  จญไฎเฌ ๆจจ ฎ ฏฎแโ ญฎขชฅ ข SysConf*/
   IF opErrLogc THEN RETURN.

   op.doc-num           = GetSysConf("-:DOC-NUM").
   op.order-pay         = GetSysConf("-:ORDER-PAY").

   UpdateSigns("opbf",string(op.op),"็ฅโ- ชโใเ ",GetSysConf("-:DR"),?).
   UpdateSigns("opbf",string(op.op),"คเฅแ_ฏฎช",GetSysConf("-:ADR"),?).
   UpdateSigns("opbf",string(op.op),"คงฌฅเฅญจ๏",GetSysConf("-:ED-IZM"),?).
   UpdateSigns("opbf",string(op.op),"ฎซ-ขฎ",GetSysConf("-:KOL-VO"),?).
   UpdateSigns("opbf",string(op.op),"Kpp-send",GetSysConf("-:KPP"),?).
   UpdateSigns("opbf",string(op.op),"โ ขช ",GetSysConf("-:STAV-NDS"),?).
   UpdateSigns("opbf",string(op.op),"ฅซฅไ_ฏฎช",GetSysConf("-:TEL"),?).

END PROCEDURE.

PROCEDURE  เโ2 ซ:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**โเฎช  ฏ เ ฌฅโเฎข*/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*ซ ฃ ฎ่จกชจ        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*ฎฎก้ฅญจฅ ฎก ฎ่จกชฅ*/

   DEFINE VARIABLE vTmp        AS INT64   NO-UNDO. /*?*/

   DEFINE BUFFER buf_opbnk FOR op-bank.
   DEFINE BUFFER buf_op    FOR op.

   RUN  เโ-ช 2(ipKauRecId,OUTPUT opErrLogc,OUTPUT opMessChar).            /*๋ฃเใงช  จญไฎเฌ ๆจจ ฎ ฏฎแโ ญฎขชฅ ข SysConf*/
   IF opErrLogc THEN RETURN.

   op.doc-num           = GetSysConf("-:DOC-NUM").
   op.order-pay         = GetSysConf("-:ORDER-PAY").

   op.doc-kind          = GetSysConf("-:DOC-KIND").
   op.ben-acct          = GetSysConf("-:BEN-ACCT").
   op.name-ben          = GetSysConf("-:NAME-BEN").
   op.inn               = GetSysConf("-:INN").
      
   UpdateSigns("opb",string(op.op)," โ ฎฌฅ้ฅญจ๏ เโ",GetSysConf("-:DATA-CARD"),?).

   IF GetSysConf("-:-") NE "" AND GetSysConf("-:-") NE ? THEN DO:
      DO:
         vTmp = INT64(GetSysConf("-:-")).
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


PROCEDURE  เโ2:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO. /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**โเฎช  ฏ เ ฌฅโเฎข*/
   DEFINE OUTPUT PARAMETER opErrLogc  AS LOGICAL   NO-UNDO.  /*ซ ฃ ฎ่จกชจ        */
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*ฎฎก้ฅญจฅ ฎก ฎ่จกชฅ*/

   DEFINE VARIABLE vTmp AS INT64 NO-UNDO. /*?*/
   DEFINE BUFFER buf_opbnk FOR op-bank.
   RUN  เโ-ช 2(ipKauRecId,OUTPUT opErrLogc,OUTPUT opMessChar).            /*๋ฃเใงช  จญไฎเฌ ๆจจ ฎ ฏฎแโ ญฎขชฅ ข SysConf*/
   IF opErrLogc THEN RETURN.
   op-entry.acct-cr     = GetSysConf("-:ACCT-DB").
/*   op-entry.amt-rub     = DECIMAL(GetSysConf("-:")).*/
   op-entry.currency    = GetSysConf("-:").
   /*กเ กฎโช  ฏ เ ฌฅโเฎข*/
   IF CAN-DO(iParamStr,"DOC-NUM") THEN op.doc-num = GetSysConf("-:DOC-NUM").

   RUN SetSysConf  in h_base ("ฎชฏจแ ญจ๏",STRING(RECID(op-entry)) + "," + STRING(ipKauRecId)).
END PROCEDURE.


PROCEDURE  เโ2:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**โเฎช  ฏ เ ฌฅโเฎข*/
   DEFINE OUTPUT PARAMETER opFlagLogc AS LOGICAL INITIAL NO NO-UNDO.  /*ซ ฃ ฎ่จกชจ*/
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*ฎฎก้ฅญจฅ ฎก ฎ่จกชฅ*/

   DEFINE PARAMETER BUFFER b_op-entry FOR op-entry.
      IF (b_op-entry.currency EQ "" AND b_op-entry.amt-rub LE 0) OR
         (b_op-entry.currency NE "" AND b_op-entry.amt-cur LE 0) THEN DO:
         RUN MessTool("ใฌฌ  ญฅ ฌฎฆฅโ ก๋โ์ ฌฅญ์่ฅ จซจ เ ขญ  0!",
                      "ERROR",
                       vAutoLog,
                       OUTPUT opFlagLogc).
         opFlagLogc = YES.
      END.
      ELSE IF b_op-entry.currency EQ "" AND
              b_op-entry.amt-rub  GT DEC(GetSysConf("-:")) THEN DO:
         RUN MessTool("ใฌฌ  ฏเฅข๋่ ฅโ ฎแโ โฎช ญ  ช เโฎโฅชฅ!",
                      "ERROR",
                       vAutoLog,
                       OUTPUT opFlagLogc).
         opFlagLogc = YES.
      END.
      ELSE IF b_op-entry.currency NE "" AND
              b_op-entry.amt-cur  GT DEC(GetSysConf("-:")) THEN DO:
         RUN MessTool("ใฌฌ  ฏเฅข๋่ ฅโ ฎแโ โฎช ญ  ช เโฎโฅชฅ!",
                      "ERROR",
                       vAutoLog,
                       OUTPUT opFlagLogc).
         opFlagLogc = YES.
      END.
END PROCEDURE.

PROCEDURE  เโ2:
   DEFINE INPUT  PARAMETER ipKauRecId AS RECID     NO-UNDO.  /**/
   DEFINE INPUT  PARAMETER iParamStr  AS CHARACTER NO-UNDO.  /**โเฎช  ฏ เ ฌฅโเฎข*/
   DEFINE OUTPUT PARAMETER opFlagLogc AS LOGICAL INITIAL NO NO-UNDO.  /*ซ ฃ ฎ่จกชจ*/
   DEFINE OUTPUT PARAMETER opMessChar AS CHARACTER NO-UNDO.  /*ฎฎก้ฅญจฅ ฎก ฎ่จกชฅ*/
   DEFINE PARAMETER BUFFER b_op-entry FOR op-entry.

   DEFINE VARIABLE vXattrCopy  AS CHARACTER NO-UNDO.  /* ชจฅ ฎฏ ฅชจ ชฎฏจเฎข โ์*/
   DEFINE VARIABLE vTmp        AS INT64   NO-UNDO. /*?*/
   DEFINE BUFFER buf_op    FOR op.
   FIND buf_op OF b_op-entry NO-LOCK NO-ERROR.

   RUN  เโ-ช 2(ipKauRecId,OUTPUT opFlagLogc,OUTPUT opMessChar).            /*๋ฃเใงช  จญไฎเฌ ๆจจ ฎ ฏฎแโ ญฎขชฅ ข SysConf*/
   IF opFlagLogc THEN RETURN.
   ASSIGN
      vXattrCopy  = GetSysConf(" เโ2ฎฏฅชข")
      vTmp        = INT64(GetSysConf("-:OP-BAL"))
   .

   FIND buf_op WHERE buf_op.op EQ vTmp NO-LOCK NO-ERROR.
   IF AVAIL buf_op THEN DO:
      RUN Copy-Xattr-Op(RECID(buf_op),RECID(op),"ฎชโ,Kpp-send,Kpp-rec,,-,ฎช,ฎช,ฎช,ฎช,ฎช").
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