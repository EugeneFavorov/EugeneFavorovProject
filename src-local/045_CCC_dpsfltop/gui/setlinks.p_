{globals.i}
{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{parsin.def}
{sh-defs.i}

{tmprecid.def}

DEFINE VARIABLE mCnt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.
DEFINE VARIABLE mNom   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymb  AS CHARACTER NO-UNDO.
DEFINE BUFFER b-loan FOR loan.
DEFINE VARIABLE mFIO  AS CHARACTER NO-UNDO.

ETIME(YES).

DEFINE VARIABLE iPotok  AS INT64 NO-UNDO.
DEFINE VARIABLE iFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDelim  AS INT64 NO-UNDO.

DEFINE VARIABLE mTran   AS CHARACTER NO-UNDO.

iPotok  = INT64(OS-GETENV("POTOK")).
iFilial = OS-GETENV("FILIAL").
iDelim  = INT64(OS-GETENV("DELIM")).

DEFINE VARIABLE vKey    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mString AS CHARACTER NO-UNDO.
DEFINE VARIABLE mName   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-Op
   FIELD op             AS INT64
   FIELD op-kind AS CHARACTER
   FIELD acct-db        AS CHARACTER
   FIELD acct-cr        AS CHARACTER.
   
DEFINE TEMP-TABLE tt-rid
   FIELD rid             AS INT64.
      

{extexch.def} /* Содержит описание временной таблицы ttExtAcct */

DEFINE VARIABLE iInn        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKpp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAcct       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPredpr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSubject    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAllFilials AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mLinkSurr   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE  tt-dir
   FIELD cid-ul    AS INT64
   FIELD cid-fl    AS INT64
   FIELD ul-name   AS CHARACTER
   FIELD fl-name   AS CHARACTER
   FIELD cust-id   AS INT64
   FIELD person-id AS INT64
   FIELD cust-name AS CHARACTER
   FIELD pers-name AS CHARACTER.

DEFINE BUFFER u-signs FOR signs.
DEFINE BUFFER f-signs FOR signs.
DEFINE BUFFER b-signs FOR signs.

{setdest.i &filename = "'setlinks.log'"}

mInt = 0.

/*
FOR EACH links WHERE
       links.link-id   EQ 190
   AND CAN-DO("5*",links.target-id) 
   NO-LOCK,
   FIRST acct WHERE
       acct.acct     EQ ENTRY (1,links.source-id)
   AND acct.currency EQ ENTRY (2,links.source-id)
   NO-LOCK:

   mInt = mInt + 1.
   
   PUT UNFORMATTED
      mInt      ";"
      links.target-id ";"
      acct.acct ";"
   SKIP.
END.
*/

/*
FOR EACH signs WHERE
       signs.file-name   EQ "acct" /*"cust-corp"*/
   AND signs.code        EQ "groupOABS"
   NO-LOCK:

   mInt = mInt + 1.
   
   IF CAN-DO("5*",signs.code-value) THEN
   DO:
      PUT UNFORMATTED
         mInt             ";"
         signs.surrogate  ";"
         signs.code-value ";"
      SKIP.
            
   END.   
END.
*/

FOR EACH qbis.acct WHERE
       qbis.acct.filial-id EQ "0500"
/*   AND CAN-DO("40817,40820",STRING(qbis.acct.bal-acct))*/
/*   AND CAN-DO("40701,40702,40703,40802",STRING(qbis.acct.bal-acct))*/
/*   AND qbis.acct.number EQ "40701810100700010002"*/
   NO-LOCK,
   EACH signs WHERE
       signs.file-name   EQ "acct"
   AND signs.code        EQ "groupOABS"
   AND signs.surrogate   EQ qbis.acct.acct + "," + qbis.acct.currency
   /*AND signs.code-value  EQ "599"*/           /*указанная группа*/
   NO-LOCK:

   mInt = mInt + 1.
   
   RUN CreateLinksRetSurr(
       "acct",
       "acct-group",
       qbis.acct.acct + "," + qbis.acct.currency,
       signs.code-value,
       DATE("01/01/1900"),
       ?,
       "",
       OUTPUT mLinkSurr) NO-ERROR.
       
   PUT UNFORMATTED
      mInt               ";"
      signs.surrogate    ";"
      signs.code-value   ";"
      mLinkSurr
   SKIP.

/*   MESSAGE mLinkSurr /*190;40702810800100010891     @0500,;581;01/01/1900*/*/
/*   VIEW-AS ALERT-BOX.                                                      */
   
END.


{preview.i &filename = "'setlinks.log'"}

{intrface.del}

RETURN.

/*
INPUT FROM VALUE("dir2.csv").
REPEAT:
   IMPORT UNFORMATTED mString.
   IF mString NE "end" THEN
   DO:
   	FIND FIRST u-signs WHERE
             u-signs.file-name   EQ "cust-corp"
         AND u-signs.code        EQ "CID"
         AND u-signs.dec-value   EQ INT64(ENTRY(1,mString,";"))
/*         AND u-signs.xattr-value EQ ENTRY(1,mString,";")*/
      NO-LOCK NO-ERROR.

      IF AVAIL(u-signs) THEN
      DO:
         FIND FIRST cust-corp WHERE
            cust-corp.cust-id EQ INT64(u-signs.surrogate)
         NO-LOCK NO-ERROR.
         IF AVAIL(cust-corp) THEN
         DO:
            FIND FIRST f-signs WHERE
                   f-signs.file-name   EQ "person"
               AND f-signs.code        EQ "CID"
               AND f-signs.dec-value   EQ DECIMAL(ENTRY(2,mString,";"))
            NO-LOCK NO-ERROR.
            FIND FIRST person WHERE
               person.person-id EQ INT64(f-signs.surrogate)
            NO-LOCK NO-ERROR.
            IF AVAIL(person) THEN
            DO:
               CREATE tt-dir.
               ASSIGN
                  tt-dir.cid-ul    = INT64(ENTRY(1,mString,";"))
                  tt-dir.cid-fl    = INT64(ENTRY(2,mString,";"))
                  tt-dir.ul-name   = ENTRY(3,mString,";")
                  tt-dir.fl-name   = ENTRY(4,mString,";")
                  tt-dir.cust-id   = cust-corp.cust-id
                  tt-dir.cust-name = cust-corp.name-corp 
                  tt-dir.person-id = person.person-id
                  tt-dir.pers-name = name-last + " " + first-names.
            END.
         END.
      END.
   END.
END.

mInt = 0.

FOR EACH tt-dir NO-LOCK:
   
END.

FOR EACH tt-dir NO-LOCK:
   mInt = mInt + 1.

	/*   PUT UNFORMATTED        */
	/*      mInt             ";"*/
	/*	   tt-dir.cust-id   ";"  */
	/*	   tt-dir.person-id ";"  */
	/*	SKIP.                    */

   FOR EACH cust-role WHERE
          cust-role.file-name  EQ "cust-corp"
      AND cust-role.class-code EQ "Директор"
      AND cust-role.surrogate  EQ STRING(tt-dir.cust-id)
      AND cust-role.cust-id    EQ STRING(tt-dir.person-id)
      NO-LOCK:
       
      PUT UNFORMATTED
         tt-dir.cust-id   ";"
         tt-dir.person-id ";"
   	SKIP.
   END.           
END.
*/

/*
FOR EACH op WHERE
       op.op-date EQ DATE("29/02/2016")
   AND CAN-DO("0000,0300",op.filial-id) 
   AND op.user-id EQ "I0400STS"
   NO-LOCK:
   PUT UNFORMATTED op.op-date ";" op.op ETIME " msec" SKIP.
   CREATE tt-Op.
   ASSIGN
      tt-Op.op = op.op.
END.

FOR EACH tt-Op NO-LOCK:

   FIND FIRST op WHERE
      op.op EQ tt-Op.op EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL(op) THEN
      op.user-id = "BIS".

END.
FOR EACH tt-Op NO-LOCK:

   FIND FIRST op-entry WHERE
      op-entry.op EQ tt-Op.op EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL(op) THEN
      op-entry.user-id = "BIS".

END.
*/


/*



FOR EACH acct WHERE
   acct.acct BEGINS "40702810205820010064"
   AND acct.filial-id EQ "0500"
   EXCLUSIVE-LOCK:

   ASSIGN acct.cust-id = 15496.

   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date ";" acct.cust-id ";" "msec" SKIP.

END.



FOR EACH  signs WHERE
          signs.file-name EQ "person"
/*      AND signs.surrogate EQ loan.contract + "," + loan.Cont-Code*/
      AND signs.code      EQ "CID"
      AND signs.xattr-value EQ "159024"
      NO-LOCK:

   PUT UNFORMATTED 
      signs.code " = "
      signs.xattr-value ";"
      " person-id = " signs.surrogate ";"
   SKIP. 

END.

FOR EACH  signs WHERE
          signs.file-name EQ "person"
/*      AND signs.surrogate EQ loan.contract + "," + loan.Cont-Code*/
      AND signs.code      EQ "CIDIP"
      AND signs.xattr-value EQ "159024"
      NO-LOCK:

   PUT UNFORMATTED 
      signs.code " = "
      signs.xattr-value ";"
      " person-id = " signs.surrogate ";"
      
   SKIP. 

END.

FOR EACH op WHERE
       op.op EQ 24372584
/*   AND op.op-date >= 01/02/16*/
/*   AND op.op-date <= 12/02/16*/
/*   AND op.op-status BEGINS ''*/
/*   AND op.user-id BEGINS ''  */
   AND op.filial-id EQ "0300"
   
   NO-LOCK:
      
   PUT UNFORMATTED 
      op.op ";"
      op.op-date ";"
      op.doc-num ";"
      op.op-status ";"
      op.acct-cat ";"
      op.filial-id ";" 
      op.class-code ";"
   SKIP.      
      
      
END.      

/*
DEFINE VARIABLE mFileName AS CHARACTER NO-UNDO.

mFileName = "111.csv".

OUTPUT TO VALUE(mFileName) CONVERT TARGET "1251".

PUT UNFORMATTED 
      "Отчет" 
   SKIP.
   PUT UNFORMATTED 
      "Колонка1;" 
      "Колонка2;"
      "Колонка3;"
      "Колонка4;"
      "Колонка5;"
      "Колонка6;"
      "Колонка7;"
      "Колонка8;" 
   SKIP.

FOR EACH acct WHERE
   CAN-DO("40817*,40820*",acct.acct)
   AND acct.filial-id EQ "0000"
   AND acct.close-date EQ ? NO-LOCK,
   EACH BlockObject WHERE
       BlockObject.class-code   EQ "BlockAcct"
   AND BlockObject.file-name    EQ "acct"
   AND BlockObject.surrogate    EQ acct.acct + "," + acct.currency
   AND BlockObject.end-datetime EQ ?
/*   AND BlockObject.txt[1]       EQ "1,2,3,4"*/
   AND BlockObject.beg-datetime LE NOW 
   NO-LOCK,
   FIRST person WHERE
      person.person-id EQ INT64(acct.cust-id)
      AND person.date-out EQ ?
   NO-LOCK:
   
   ASSIGN
      mPredpr  = GetXattrValueEx("person",STRING(person.person-id),"Предпр", "")
      mSubject = GetXattrValueEx("person",STRING(person.person-id),"Субъект","").  
   IF mPredpr  BEGINS "Пред" THEN NEXT. 
   IF mSubject EQ     "ФЛП"  THEN NEXT.   
   
   PUT UNFORMATTED 
      person.person-id ";" 
      person.name-last ";" 
      person.first-names ";"
      mPredpr ";"
      mSubject ";"
      acct.acct ";" 
      BlockObject.class-code ";" 
      BlockObject.block-type ";"       
      ETIME " msec" 
   SKIP.
   
END.     

OUTPUT CLOSE.

MESSAGE "Ok " ETIME 
VIEW-AS ALERT-BOX.

RUN sndbispc ("file=" + mFileName + ";class=bq").

*/


FOR EACH op WHERE
       op.op EQ 22288451
   EXCLUSIVE-LOCK:

   ASSIGN
      op.details = "Возврат неиспользованной части кредитных средств по КД № 26981-КЛ от 28.12.15г. Сумма 6716000-00. Без налога (НДС).".
      
   PUT UNFORMATTED op.op-date ";" op.op ";" op.doc-num ";" op.details ETIME " msec" SKIP.
         
END.


FOR EACH acct WHERE
   acct.acct BEGINS "70606810001161601101"
   AND acct.filial-id EQ "0000"
   EXCLUSIVE-LOCK:

   ASSIGN acct.close-date = DATE("01/01/2016").

   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.

END.

FOR EACH op WHERE
       op.op EQ 24086640
   EXCLUSIVE-LOCK:
   
   
   ASSIGN
      op.op-transaction = 1856026.
      
   PUT UNFORMATTED op.op-date ";" op.op ETIME " msec" SKIP.
         
END.

FOR EACH op WHERE
       op.op-date GE DATE("31/01/2016")
   AND CAN-DO("0000,0300",op.filial-id) 
   AND op.user-id EQ "I0400STS"
   NO-LOCK:
   PUT UNFORMATTED op.op-date ";" op.op ETIME " msec" SKIP.
   CREATE tt-Op.
   ASSIGN
      tt-Op.op = op.op.
END.

FOR EACH tt-Op NO-LOCK:

   FIND FIRST op WHERE
      op.op EQ tt-Op.op EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL(op) THEN
      op.user-id = "BIS".

END.

FOR EACH tt-Op NO-LOCK:

   FIND FIRST op-entry WHERE
      op-entry.op EQ tt-Op.op EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL(op) THEN
      op-entry.user-id = "BIS".

END.

ZNO17106641_723020160127_001171.txt

НомСч:40802810706600010059
ВалСч:643
ДатаНачала:01.01.2012
ДатаКонца:31.12.2015
ОстатНач:0.00
СуммаДеб:13302531.54
СуммаКред:13304627.10
ОстатКон:0.00


DEFINE VARIABLE iBegDate AS DATE NO-UNDO.   /* Дата начала выгрузки    */
DEFINE VARIABLE iEndDate AS DATE NO-UNDO.   /* Дата окончания выгрузки */

DEFINE VARIABLE tthndl AS handle NO-UNDO. 
DEFINE VARIABLE res    AS INTEGER NO-UNDO.
DEFINE VARIABLE bh AS HANDLE NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.

create temp-table tthndl.
      
RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl

res = PROC-STATUS
(
      "select * from BANKER.TOBIS365P3
      where TOBIS365P.account = '" + "40702810814100010020" + "'
        and TOBIS365P.postime >= to_date('" + STRING(iBegDate,'99/99/9999') + "','dd/mm/yyyy')
        AND TOBIS365P.postime < to_date('" + STRING(iEndDate,'99/99/9999') + "','dd/mm/yyyy')").

bh = tthndl:DEFAULT-BUFFER-HANDLE.
CREATE QUERY qh.
qh:SET-BUFFERS(bh).
qh:QUERY-PREPARE("for each " + bh:name ).
qh:QUERY-OPEN.
REPEAT:
    qh:GET-NEXT().
    if qh:QUERY-OFF-END THEN LEAVE.
          /* Поля для выгрузки, имена соответсвуют тэгам файле */
          MESSAGE 
          
/*          iHRepOp:BUFFER-FIELD(GetMangledName("ДатаОпер")):BUFFER-VALUE = REPLACE( STRING( bh:buffer-field("DATE_PAY"):buffer-value, '99.99.9999'), '/', '.')            */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("ВидДок")):BUFFER-VALUE   = bh:buffer-field("DOC_TYPE"):buffer-value                                                       */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("НомДок")):BUFFER-VALUE   = bh:buffer-field("DOC_NUM"):buffer-value                                                        */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("ДатаДок")):BUFFER-VALUE  = bh:buffer-field("DOC_DATE"):buffer-value                                                       */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("НомКорСч")):BUFFER-VALUE = bh:buffer-field("CORR_ACC"):buffer-value /* номер кор.счета банка плательщика */               */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("НаимБП")):BUFFER-VALUE   = REPL_VK( bh:buffer-field("NAME_BANK"):buffer-value)   /* наименование банка плательщика */     */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("БИКБП")):BUFFER-VALUE    = bh:buffer-field("BIC_BANK"):buffer-value    /* БИК банка плательщика */                        */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("ИННПП")):BUFFER-VALUE    = bh:buffer-field("INN_RECIPIENT"):buffer-value    /* ИНН/КИО плательщика/получателя */          */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("КПППП")):BUFFER-VALUE    = bh:buffer-field("KPP_RECIPIENT"):buffer-value    /* КПП плательщика/получателя */              */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("НаимПП")):BUFFER-VALUE   = REPL_VK( bh:buffer-field("RECIPIENT"):buffer-value)   /* наименование плательщика/получателя */*/
/*          iHRepOp:BUFFER-FIELD(GetMangledName("НомСчПП")):BUFFER-VALUE  = REPL_VK( bh:buffer-field("ACC_RECIPIENT"):buffer-value)  /* счет плательщика/получателя */     */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("Дебет")):BUFFER-VALUE    = DEC(bh:buffer-field("DEBIT"):buffer-value)                                                     */
/*          iHRepOp:BUFFER-FIELD(GetMangledName("Кредит")):BUFFER-VALUE   = DEC(bh:buffer-field("CREDIT"):buffer-value)                                                    */
          
          bh:buffer-field("DETAILS"):buffer-value
          
          VIEW-AS ALERT-BOX.
    END.

DISABLE TRIGGERS FOR LOAD OF signs.

 
FOR EACH signs WHERE 
   signs.file-name = "term-obl"
   AND signs.surrogate  BEGINS "АРЕНДА"
   AND signs.code-value = "31/01/2016"
   NO-LOCK:

   
   
   PUT UNFORMATTED signs.file-name ";" signs.surrogate ";" signs.code-value ";" signs.date-value ";" ETIME " msec" SKIP.


/*DELETE signs.*/

END.

RUN ext-findactex.p( iInn, iKpp, iName, iAcct, iAllFilials,
    OUTPUT TABLE ttExtAcct).


DEFINE VARIABLE tthndl AS handle NO-UNDO.
DEFINE VARIABLE res    AS INTEGER NO-UNDO.
DEFINE VARIABLE bh AS HANDLE NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE mCID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCIDIP AS CHARACTER NO-UNDO.

/*
969         5502005562
970         5502008588
973         5503001264
974         5503022539
982         5503001017
983         5503022553
984         0000000000
985         5503022803
990         5503022715
992         5503022521
994         5501030407
996         5503005300
1000        5504006392
mCID = "973,974,982,1000".
40702810814100010020
*/

IF     {assigned iInn} 
   AND LENGTH(iInn) EQ 10 THEN
DO:
   FIND FIRST cust-corp WHERE
      cust-corp.inn EQ iInn
   NO-LOCK NO-ERROR.
   IF AVAIL(cust-corp) THEN
   ASSIGN
      mCID = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"CID","").
END.
ELSE IF {assigned iInn} 
   AND  LENGTH(iInn) EQ 12 THEN
DO:
   FIND FIRST person WHERE
      person.inn EQ iInn
   NO-LOCK NO-ERROR.
   IF AVAIL(person) THEN
   ASSIGN
      mCID   = GetXAttrValueEx("person",STRING(person.person-id),"CID","")
      mCIDIP = GetXAttrValueEx("person",STRING(person.person-id),"CIDIP","").
   IF {assigned mCID}
      AND {assigned mCIDIP} 
   THEN mCID = mCID + "," + mCIDIP.
END.

mCID = "".

PUT UNFORMATTED "mCID: " mCID " iInn: " iInn SKIP.

IF {assigned mCID} THEN
DO:
	{empty ttExtAcct}      
   
   CREATE TEMP-TABLE tthndl.
   
   RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
   res = PROC-STATUS
      (
      "select * from BANKER.TOBIS365PCLOSED
	   where CID in (" + mCID + ")"
      ).
   
   bh = tthndl:DEFAULT-BUFFER-HANDLE.
   CREATE QUERY qh.
   qh:SET-BUFFERS(bh).
   qh:QUERY-PREPARE("for each " + bh:name).
   qh:QUERY-OPEN.
   mInt = 0.
   REPEAT:
      qh:GET-NEXT().
      IF qh:QUERY-OFF-END THEN LEAVE.
      mInt = mInt + 1.
      PUT UNFORMATTED mInt "; where CID in " bh:buffer-field("CID"):buffer-value ";" bh:buffer-field("account"):buffer-value SKIP.
      RUN ext-findact.p(bh:buffer-field("account"):buffer-value,
                        iAllFilials,
                        INPUT-OUTPUT TABLE ttExtAcct).
   END.
   
   PUT UNFORMATTED "Итого: " mInt SKIP.
   
   PUT UNFORMATTED "111" FILL("=",50) SKIP.
   mInt = 0.
   FOR EACH ttExtAcct NO-LOCK:
      mInt = mInt + 1.
      PUT UNFORMATTED ttExtAcct.open-date ";" ttExtAcct.close-date ";" ttExtAcct.acct SKIP.
   END.
   PUT UNFORMATTED "111" FILL("=",50) SKIP.
END.
ELSE
DO:
	{empty ttExtAcct}
   
   CREATE TEMP-TABLE tthndl.
   
   IF mInt EQ 0 THEN 
   DO:
      RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
      res = PROC-STATUS 
         (
         "select * from BANKER.TOBIS365PCLOSED t 
	       where t.inn = '" + iInn + "'"
         ).
   
      bh = tthndl:DEFAULT-BUFFER-HANDLE.
      CREATE QUERY qh.
      qh:SET-BUFFERS(bh).
      qh:QUERY-PREPARE("for each " + bh:name).
      qh:QUERY-OPEN.
      mInt2 = 0.
      
      REPEAT:
         qh:GET-NEXT().
         IF qh:QUERY-OFF-END THEN LEAVE.
         mInt2 = mInt2 + 1.
         PUT UNFORMATTED 
            mInt2 "; where inn = " 
            bh:buffer-field("inn"):buffer-value ";" 
            bh:buffer-field("CID"):buffer-value ";" 
            bh:buffer-field("account"):buffer-value 
         SKIP.         
         RUN ext-findact.p(bh:buffer-field("account"):buffer-value,
                           iAllFilials,
                           INPUT-OUTPUT TABLE ttExtAcct).
      END.
   END.
   
   PUT UNFORMATTED "222" FILL("=",50) SKIP.
   mInt = 0.
   FOR EACH ttExtAcct NO-LOCK:
      mInt = mInt + 1.
      PUT UNFORMATTED mInt3 ";" ttExtAcct.open-date ";" ttExtAcct.close-date ";" ttExtAcct.acct SKIP.
   END.
   
   PUT UNFORMATTED "222" FILL("=",50) SKIP.
END.

IF mInt EQ 0 
THEN PUT UNFORMATTED "Ничего не выбрано. " ETIME " msec" SKIP.
ELSE PUT UNFORMATTED mInt ETIME " msec" SKIP.


DEFINE VARIABLE iBegDate AS DATE NO-UNDO.
DEFINE VARIABLE iEndDate AS DATE NO-UNDO.
DEFINE VARIABLE iAllFil  AS LOGICAL NO-UNDO.

DEFINE VARIABLE oAmtIn    AS DECIMAL NO-UNDO.
DEFINE VARIABLE oAmtDb    AS DECIMAL NO-UNDO.
DEFINE VARIABLE oAmtCr    AS DECIMAL NO-UNDO.
DEFINE VARIABLE oAmtOut   AS DECIMAL NO-UNDO.


iBegDate = DATE("01/11/2014").
iEndDate = DATE("01/12/2014").
iAllFil  = NO.

mCID = "515776".

Create ttExtAcct. 
ttExtAcct.number = "40702810814100010020".

/*MESSAGE mCID ";"  ttExtAcct.number*/
/*VIEW-AS ALERT-BOX.                */

CREATE TEMP-TABLE tthndl.

RUN ext-acctpos_.p( iBegDate, iEndDate, iAllFil, INPUT TABLE ttExtAcct, 
    OUTPUT oAmtIn, OUTPUT oAmtDb, OUTPUT oAmtCr, OUTPUT oAmtOut).

MESSAGE oAmtIn ";" oAmtDb ";" oAmtCr ";" oAmtOut
VIEW-AS ALERT-BOX.

FOR EACH blockobject WHERE
       blockobject.surrogate    BEGINS "707"
   AND blockobject.file-name    EQ "acct"
   AND blockobject.block-type   EQ "Блок"
   AND blockobject.class-code   EQ "BlockAcct"
   AND blockobject.beg-datetime >= datetime(DATE("01/01/2016"))
   EXCLUSIVE-LOCK:

   mInt = mInt + 1.
   PUT UNFORMATTED
      mInt ";"
      blockobject.surrogate ";"
      blockobject.beg-datetime
   SKIP.

   DELETE blockobject.
      	
END.

/*FOR EACH acct WHERE                                                    */
/*   acct.acct BEGINS "42301810904000001027"                             */
/*   AND acct.filial-id EQ "0400"                                        */
/*   EXCLUSIVE-LOCK:                                                     */
/*                                                                       */
/*   ASSIGN acct.close-date = DATE("30/04/2015").                        */
/*                                                                       */
/*   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.*/
/*                                                                       */
/*END.                                                                   */

/*FOR EACH acct WHERE                                                   */
/*       acct.filial-id   EQ "0000"                                     */
/*   AND acct.acct-cat    EQ "b"                                        */
/*   AND acct.open-date   LE DATE("30/04/2015")                         */
/*   AND (acct.close-date GE DATE("30/04/2015") OR acct.close-date EQ ?)*/
/*   NO-LOCK:                                                           */
/*                                                                      */
/*   mInt = mInt + 1.                                                   */
/*                                                                      */
/*   PUT UNFORMATTED                                                    */
/*      mInt ";"                                                        */
/*      acct.acct-cat ";"                                               */
/*      acct.acct ";"                                                   */
/*      acct.open-date ";"                                              */
/*      acct.close-date                                                 */
/*   SKIP.                                                              */
/*                                                                      */
/*END.                                                                  */

/*mInt = 0.                                                       */
/*FOR EACH acct WHERE                                             */
/*       acct.acct       BEGINS "707"                             */
/*   AND acct.filial-id  EQ "0000"                                */
/*   AND acct.close-date EQ ?                                     */
/*   NO-LOCK:                                                     */
/*   mInt = mInt + 1.                                             */
/*   CREATE blockobject.                                          */
/*   ASSIGN                                                       */
/*   	blockobject.surrogate = acct.acct + "," + acct.currency     */
/*   	blockobject.file-name = "acct"                              */
/*   	blockobject.block-type = "Блок"                             */
/*   	blockobject.class-code = "BlockAcct"                        */
/*      blockobject.beg-datetime = DATETIME(DATE("01/01/2016"),0).*/
/*                                                                */
/*   PUT UNFORMATTED                                              */
/*      mInt ";"                                                  */
/*      blockobject.surrogate ";"                                 */
/*      blockobject.beg-datetime                                  */
/*   SKIP.                                                        */
/*END.                                                            */

/*
70606810800094810202
70606810100094810203
70606810300094810210
*/

/*FOR EACH acct WHERE                                                    */
/*   acct.acct BEGINS "70606810300094810210"                             */
/*/*   AND acct.filial-id EQ "0000"*/                                    */
/*   EXCLUSIVE-LOCK:                                                     */
/*                                                                       */
/*   ASSIGN acct.open-date = DATE("01/01/2016").                         */
/*                                                                       */
/*   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.*/
/*                                                                       */
/*END.                                                                   */

/*mInt = 0.                                                     */
/*FOR EACH blockobject WHERE                                    */
/*       blockobject.surrogate    BEGINS "707"                  */
/*   AND blockobject.file-name    EQ "acct"                     */
/*   AND blockobject.block-type   EQ "Блок"                     */
/*   AND blockobject.class-code   EQ "BlockAcct"                */
/*   AND blockobject.beg-datetime > datetime(DATE("01/01/2016"))*/
/*   EXCLUSIVE-LOCK:                                            */
/*                                                              */
/*   mInt = mInt + 1.                                           */
/*   PUT UNFORMATTED                                            */
/*      mInt ";"                                                */
/*      blockobject.surrogate ";"                               */
/*      blockobject.beg-datetime                                */
/*   SKIP.                                                      */
/*                                                              */
/*   DELETE blockobject.                                        */
/*      	                                                       */
/*END.                                                          */


/*
FOR EACH blockobject WHERE
   (blockobject.end-datetime EQ ? OR blockobject.end-datetime >=
DATETIME("12/01/2016 07:12:22.797"))
   AND blockobject.class-code EQ 'BlockAcct'
   AND blockobject.file-name EQ 'acct'
   AND blockobject.surrogate EQ '40701810100000012784 @0000,'
   NO-LOCK
   BY BlockObject.beg-datetime

mCnt = 0.
mString = "".
INPUT FROM VALUE("operu.csv").
REPEAT:
   IF mString NE "end" THEN
   DO:
   	IMPORT UNFORMATTED mString.
   	FIND FIRST op-kind WHERE 
         op-kind.op-kind EQ ENTRY(4,mString,";")
      NO-LOCK NO-ERROR.
      IF AVAIL(op-kind) THEN mName = op-kind.name-opkind.
   	PUT UNFORMATTED 
   	   ENTRY(1,mString,";") ";" 
   	   ENTRY(2,mString,";") ";"
   	   ENTRY(3,mString,";") ";"
   	   ENTRY(5,mString,";") ";"
   	   ENTRY(4,mString,";") ";"
   	   mName
   	SKIP.   
   END.
END.
INPUT CLOSE.


42301-810-2-0102-0000963
42306-810-4-0102-0281406

FOR EACH acct WHERE
       acct.acct BEGINS "42301810201020000963"
   AND acct.filial-id EQ "0000"
   EXCLUSIVE-LOCK:

   ASSIGN acct.close-date = DATE("31/12/2015").
   
   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.   
      
END.

FOR EACH acct WHERE
   acct.acct BEGINS "70601810801146305300"
   AND acct.filial-id EQ "0000"
   EXCLUSIVE-LOCK:

   ASSIGN acct.close-date = DATE("01/01/2016").
   
   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.   
      
END.

Сергей, прошу закрыть старые счета:
70601-810-2-0114-6305201
70601-810-8-0114-6305300

FOR EACH acct WHERE
       (acct.close-date EQ ? OR acct.close-date >= DATE("31/12/2015"))
   AND acct.acct-cat EQ 'b'
   AND STRING(acct.bal-acct) BEGINS '706'
   AND acct.open-date <= DATE("31/12/2015")
   AND acct.filial-id EQ "0000"
   NO-LOCK:
   
   IF acct.acct BEGINS "70601810201146305201"
   OR acct.acct BEGINS "70601810801146305300" THEN NEXT.
   
   mInt = mInt + 1.
   
   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.   
      
END.


FOR EACH acct WHERE
       (acct.close-date EQ ? OR acct.close-date >= DATE("31/12/2015"))
   AND acct.acct-cat EQ 'b'
   AND STRING(acct.bal-acct) BEGINS '706'
   AND acct.open-date <= DATE("31/12/2015")
   AND acct.filial-id EQ "0000"
   EXCLUSIVE-LOCK:

   IF acct.acct BEGINS "70601810201146305201"
   OR acct.acct BEGINS "70601810801146305300" THEN NEXT.
    
   mInt = mInt + 1.
   ASSIGN acct.close-date = DATE("01/01/2016").
   
   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.   
      
END.
*/

FIND FIRST op WHERE
      op.op EQ 22650125 EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL(op) THEN
   op.user-inspector = "KFO_PAA".
   

FOR EACH acct WHERE
       (acct.close-date EQ ? OR acct.close-date >= DATE("31/12/2015"))
   AND acct.acct-cat EQ 'b'
   AND STRING(acct.bal-acct) BEGINS '706'
   AND acct.open-date <= DATE("31/12/2015")
   AND acct.filial-id EQ "0300"
   NO-LOCK:
    
   mInt = mInt + 1.
   
   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.   
      
END.

FOR EACH acct WHERE
       (acct.close-date EQ ? OR acct.close-date >= DATE("31/12/2015"))
   AND acct.acct-cat EQ 'b'
   AND STRING(acct.bal-acct) BEGINS '706'
   AND acct.open-date <= DATE("31/12/2015")
   AND acct.filial-id EQ "0300"
   EXCLUSIVE-LOCK:
    
   mInt = mInt + 1.
   ASSIGN acct.close-date = DATE("01/01/2016").
   
   PUT UNFORMATTED mInt ";" acct.acct ";" acct.close-date " msec" SKIP.   
      
END.

FOR EACH op WHERE
           op.op-date EQ DATE("01/01/2016")
   NO-LOCK,
   FIRST op-entry OF op WHERE
      CAN-DO("70706*",op-entry.acct-db)
   NO-LOCK:
   
   PUT UNFORMATTED mInt ";" op.doc-num ";" op-entry.acct-db ";" op-entry.acct-cr ";" op-entry.amt-rub SKIP.
   
END.

FOR EACH acct WHERE
       (acct.close-date EQ ? OR acct.close-date >= DATE("31/12/2015"))
   AND acct.acct-cat EQ 'b'
   AND STRING(acct.bal-acct) BEGINS '706'
   AND acct.open-date <= DATE("31/12/2015")
   AND acct.filial-id EQ "0300"
   NO-LOCK:
    
   mInt = mInt + 1.
   
   PUT UNFORMATTED mInt ";" acct.acct ";" ETIME " msec" SKIP.   
      
END.

FIND FIRST op WHERE
      op.op EQ 22650125 EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL(op) THEN
   op.user-inspector = "KFO_PAA".
   
mInt = 0.

FOR EACH op WHERE
           op.op-date EQ DATE("01/01/2016")
       AND op.user-id EQ "SERV0000"
   NO-LOCK,
   FIRST op-entry OF op WHERE
      CAN-DO("706*",op-entry.acct-db)
   NO-LOCK:

   mInt = mInt + 1.
      
/*   PUT UNFORMATTED mInt ";" op.doc-num ";" op.user-id ";" ETIME " msec" SKIP.*/
      
   CREATE tt-Op.
   ASSIGN
      tt-Op.op      = op.op
      tt-Op.acct-db = op-entry.acct-db.
END.   

FOR EACH op WHERE
           op.op-date EQ DATE("01/01/2016")
       AND op.user-id EQ "SERV0000"
   NO-LOCK,
   FIRST op-entry OF op WHERE
      CAN-DO("706*",op-entry.acct-cr)
   NO-LOCK:

   mInt = mInt + 1.
      
/*   PUT UNFORMATTED mInt ";" op.doc-num ";" op.user-id ";" ETIME " msec" SKIP.*/
      
   CREATE tt-Op.
   ASSIGN
      tt-Op.op      = op.op
      tt-Op.acct-db = op-entry.acct-cr.
END.

FOR EACH tt-Op NO-LOCK
   BREAK BY tt-Op.acct-db:
   
   IF FIRST-OF(tt-Op.acct-db) THEN
   DO:
      
      {find-act.i
	      &acct = tt-Op.acct-db 
	   }
    
      FIND FIRST op WHERE
         op.op EQ tt-Op.op NO-LOCK NO-ERROR.
      
      IF AVAIL(acct) 
         AND AVAIL(op) THEN
      DO:
         PUT UNFORMATTED
            tt-Op.op ";"
            op.doc-num ";"
            tt-Op.acct-db ";"
            acct.open-date
         SKIP.
      END.
   END.
   
END.

FOR EACH op WHERE
       op.op-date GE DATE("01/11/2015")
   AND CAN-DO("0000,0300",op.filial-id) 
   NO-LOCK,
   FIRST op-entry OF op WHERE
      CAN-DO("706*",op-entry.acct-db)
   NO-LOCK:
   
   CREATE tt-Op.
   ASSIGN
      tt-Op.op      = op.op
      tt-Op.acct-db = op-entry.acct-db
      tt-Op.acct-cr = op-entry.acct-cr
      tt-Op.op-kind = op.op-kind.
END.

FOR EACH tt-Op NO-LOCK
   BREAK BY tt-Op.acct-db:
   
   IF FIRST-OF(tt-Op.acct-db) THEN mTran = "".
   IF INDEX(mTran,tt-Op.op-kind) EQ 0 THEN
      mTran = mTran + tt-Op.op-kind + ",".
   IF LAST-OF(tt-Op.acct-db) THEN
      PUT UNFORMATTED
         tt-Op.op ","
         tt-Op.acct-db ","
         tt-Op.acct-cr ","
         TRIM(mTran,",")
      SKIP.
END.


FOR EACH tt-Op NO-LOCK:
   
   FIND FIRST op WHERE
      op.op EQ tt-Op.op EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL(op) THEN
      op.user-id = "SERV0000".
   
END.

*/

/*RUN key-tst("70601810100141111601","044599350", OUTPUT vKey ).*/
/*                                                              */
/*MESSAGE vKey                                                  */
/*VIEW-AS ALERT-BOX.                                            */

/*   c-ls = SUBSTR(c-ls,1,8) + string(vKey,"9") + SUBSTR(c-ls,10,11).*/
   
/*
{empty tmprecid} 
mInt2   = 0.

FOR EACH loan WHERE
      (loan.close-date EQ ? OR
       loan.close-date GE TODAY)
   AND loan.contract   EQ "КРЕДИТ"
   AND loan.filial-id  NE "0400"
   NO-LOCK:
      
   CREATE tmprecid. 
   tmprecid.id = RECID(loan).
   mInt2 = mInt2 + 1.    
END.


return.
*/


/*
{setdest.i &file-name = "111.log"}

iPotok  = 0.
iFilial = "0000".
iDelim  = 10.
mInt    = 0.
mInt3   = 0.

DO mCnt = 0 TO 10:
   MESSAGE mCnt 
   VIEW-AS ALERT-BOX.
   mInt = 0. 
   FOR EACH tmprecid EXCLUSIVE-LOCK:
      IF INT64(tmprecid.id) MODULO iDelim EQ mCnt THEN 
      DO:
         PUT UNFORMATTED ETIME ";" tmprecid.id ";" iDelim ";" mCnt " NEXT msec" SKIP.
         mInt = mInt + 1.
         NEXT.
      END.
	/*   PUT UNFORMATTED ETIME ";" tmprecid.id " DELETE tmprecid msec" SKIP.*/
/*      DELETE tmprecid.*/
   END.
   mInt3 = mInt3 + mInt. 
END.

PUT UNFORMATTED mInt3 ";" mInt2 ";" mInt ";" ETIME " msec" SKIP.

{preview.i &file-name = "111.log"}

{intrface.del}

RETURN.
*/

