/* ”เฅฉฌ คซ๏ ฏเฎแฌฎโเ  จ เฅค ซชโจเฎข ญจ๏ คฎชใฌฅญโ  */
FORM
   op.op-date        AT 10  
   op.op-kind        AT 28  FORMAT "X(14)"
   op.op-transaction AT 58 format ">>>>>>>>9" SKIP

   op.op-status      AT 8  
   op.user-id        AT 38 LABEL "ขฅซ" 
   op.user-inspector AT 58 SKIP

   op.doc-type       AT 4 
   op.doc-num        AT 30 FORMAT "x(10)"
   op.doc-date       AT 56 SKIP

   op.due-date       AT 2 
   op.op-value-date  AT 33 LABEL " โ  ข ซ."
   op.order-pay      AT 56 FORMAT "x(2)"
                           LABEL "็ฅเ. ฏซ โ." 
                           HELP "็ฅเฅคญฎแโ์ ฏซ โฅฆ " SKIP

   op.contract-date  AT 4  
   op.ins-date       AT 30 LABEL " โ  ฏฎแโใฏ." 
   op.branch-id      AT 59 FORMAT "x(10)"
                           LABEL  "ฎคเ งค."
   SKIP

   "อ[" dockind FORMAT "x(18)" NO-LABEL 
   "]อออออออออออออออออออออออออออออออออออออออออออออออออออออออ" SKIP

   ":"            AT 2 
   vmfo                    NO-LABEL 
   bank1.name        AT 30 NO-LABEL FORMAT "x(45)" SKIP

   "/แ:"            AT 2 
   vcorr-acct              NO-LABEL 
   bank2.name        AT 45 NO-LABEL FORMAT "x(30)" SKIP

   op.ben-acct       AT 2 FORMAT "x(35)"
   "ฤ[  จฌฅญฎข ญจฅ ชซจฅญโ  ]ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ" SKIP

   op.name-ben            NO-LABEL op.inn SKIP

   "อ[ ‘ฎคฅเฆ ญจฅ ฎฏฅเ ๆจจ  ]อออออออออออออออออออออออออออออออออออออออออออออออออออออ"  SKIP

   op.details VIEW-AS EDITOR inner-chars 78 INNER-LINES 3 NO-LABEL

WITH FRAME {1} 1 DOWN SIDE-LABELS TITLE COLOR BRIGHT-WHITE "[ “…’ ]".

