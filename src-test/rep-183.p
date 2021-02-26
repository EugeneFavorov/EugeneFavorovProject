/* rep-183.p*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get blkob}

DEFINE VARIABLE mFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustID     AS INT64     NO-UNDO.
DEFINE VARIABLE mBlock      AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE mBlock      AS CHARACTER EXTENT 5 NO-UNDO.*/
DEFINE VARIABLE mBlockList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBlockSum   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mBlockSumS  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOrdPay     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpLst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOOst       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mAmt        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mInt        AS INT64     NO-UNDO.
DEFINE VARIABLE mRepDate    AS DATE      NO-UNDO.

DEFINE TEMP-TABLE rep183
   FIELD acct      AS CHARACTER
   FIELD currency  AS CHARACTER
   FIELD cust-cat  AS CHARACTER
   FIELD cust-id   AS INT64
   FIELD filial-id AS CHARACTER
   FIELD nam-block AS CHARACTER
   FIELD sum-block AS DECIMAL
   FIELD contract  AS CHARACTER
   FIELD ost       AS CHARACTER
   FIELD cust-name AS CHARACTER
   FIELD op-lst    AS CHARACTER
   FIELD ord-pay   AS CHARACTER.
   
{empty rep183}

DEFINE BUFFER o-acct FOR acct.

DEFINE STREAM out-stream.

mFileName = "./" + 
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(MTIME)) + "-" + "rep-bl-fl.xml".
OUTPUT STREAM out-stream TO VALUE(mFileName)
       UNBUFFERED CONVERT TARGET "UTF-8" SOURCE "IBM866".

/*OUTPUT STREAM out-stream TO VALUE(mFileName) UNBUFFERED.*/

/*
1.	   Код клиента;
2.	   Наименование;
3.	   Номер р/счета: Номер р/счета клиента по которому произошло дебетование по счету в текущем опер. дне;
4.	   Остаток на р/счете: РЕАЛЬНЫЙ ОСТАТОК ДЕНЕЖНЫХ СРЕДСТВ НА МОМЕНТ ФОРМИРОВАНИЯ ОТЧЕТА.
5.	   Очередность платежа: поле очередность платежа в расчетном документе при дебетовании р/счета;
6.	   Дата документа: Дата первичного документа;
7.	   Блокировки: Наименование блокировки
8.	   Сумма блокировки: Сумма на которую установлена блокировка.
*/

{ getdate.i }

MESSAGE "Формирование отчета..".

/*mRepDate = DATE("14/12/2015").*/
mRepDate = end-date.

/*проход по блокировкам*/
mInt = 0.
FOR EACH acct WHERE
   CAN-DO("405*,406*,407*,40802*,40807*,40821*",acct.acct)
/*   AND acct.acct EQ "40702810300000028124     @0000"*/
   AND acct.filial-id  EQ shFilial
   AND acct.close-date EQ ? NO-LOCK,
   EACH BlockObject WHERE
       BlockObject.class-code    EQ "BlockAcct"
   AND BlockObject.file-name     EQ "acct"
   AND BlockObject.surrogate     EQ acct.acct + "," + acct.currency
   AND (BlockObject.end-datetime EQ ? 
     OR Blockobject.end-datetime GE DATETIME(mRepDate,24 * 60 * 60 * 1000 - 1))
   AND BlockObject.beg-datetime  LE DATETIME(mRepDate,24 * 60 * 60 * 1000 - 1)
   NO-LOCK BREAK BY acct.acct:

   IF FIRST-OF(acct.acct) THEN
   ASSIGN
      mBlock     = ""
      mBlockList = ""
      mBlockSum  = 0.
   
   IF BlockObject.block-type EQ "БлокСумм" THEN
   DO:
      mBlock =  IF mBlock EQ ""
         THEN BlockObject.block-type + " " + 
              (IF BlockObject.txt[3] EQ "Арест" THEN "Арест" ELSE "1,2,3")
         ELSE mBlock + ";" +
              BlockObject.block-type + " " + 
              (IF BlockObject.txt[3] EQ "Арест" THEN "Арест" ELSE "1,2,3").
      mBlock    = mBlock + ": " + TRIM(STRING(BlockObject.val[3],"->>>,>>>,>>>,>>9.99")).
      mBlockSum = mBlockSum + BlockObject.val[3].
   END.
   ELSE
   DO:
      mBlock =  IF mBlock EQ ""
      THEN BlockObject.block-type
      ELSE mBlock + ";" + BlockObject.block-type.
   END.
   
   IF LAST-OF(acct.acct) THEN
   DO:
      /* Если нет еще такого в rep183 */
      FIND FIRST rep183 WHERE 
             rep183.acct     EQ acct.acct
         AND rep183.currency EQ acct.currency
      NO-LOCK NO-ERROR.
      IF NOT AVAIL(rep183) THEN
      DO:
         CREATE rep183.
         ASSIGN 
            rep183.acct      = acct.acct  
            rep183.currency  = acct.currency
            rep183.cust-cat  = acct.cust-cat
            rep183.cust-id   = acct.cust-id
            rep183.filial-id = acct.filial-id
            rep183.nam-block = mBlock
            rep183.sum-block = mBlockSum.
      END.
   END.
END.

/*проход по картотекам*/
FOR EACH acct WHERE
   CAN-DO("405*,406*,407*,40802*,40807*,40821*",acct.acct)
/*   AND acct.acct EQ "40702810300000028124     @0000"*/
   AND acct.filial-id  EQ shFilial
   AND acct.close-date EQ ? NO-LOCK:
   
   /*ВБС клиента БС*/      
   FOR EACH o-acct WHERE 
            o-acct.acct-cat  EQ 'o'
      AND   o-acct.filial-id EQ shFilial 
      AND   o-acct.cust-cat  EQ acct.cust-cat
      AND   o-acct.cust-id   EQ acct.cust-id
      AND   CAN-DO("90901*,90902*",o-acct.acct) 
      AND   CAN-DO("Карт2,КартБл",o-acct.contract)
      NO-LOCK:
      
      /*остаток на ВБС*/      
      RUN acct-pos IN h_base (o-acct.acct,
                              o-acct.currency,
                              mRepDate,
                              mRepDate,
                              CHR(251)).
      
      mOOst = IF o-acct.currency EQ "" THEN sh-bal ELSE sh-val.
         
      IF mOOst GT 0 THEN
      DO:
         /* Если нет еще такого в rep183 */
         FIND FIRST rep183 WHERE 
                rep183.acct     EQ acct.acct
            AND rep183.currency EQ acct.currency
         NO-LOCK NO-ERROR.
         IF NOT AVAIL(rep183) THEN
         DO:
            CREATE rep183.
            ASSIGN 
               rep183.acct      = acct.acct  
               rep183.currency  = acct.currency
               rep183.cust-cat  = acct.cust-cat
               rep183.cust-id   = acct.cust-id
               rep183.filial-id = acct.filial-id
               rep183.contract  = o-acct.contract.
         END.
         ELSE
            ASSIGN 
               rep183.contract  = o-acct.contract.
      END.
   END.
END.

/*Заполнение данных отчета*/
FOR EACH rep183 NO-LOCK:
   /*Наименование клиента*/
   ASSIGN
      mCustName = ""
      mCustID   = 0.

   CASE rep183.cust-cat:
   WHEN "Ю" THEN
   DO:
      FIND cust-corp WHERE cust-corp.cust-id = rep183.cust-id NO-LOCK NO-ERROR.
      IF AVAIL cust-corp THEN
         ASSIGN
            mCustName = IF TRIM(cust-corp.name-short) EQ "" 
                        THEN TRIM(cust-corp.name-corp) 
                        ELSE TRIM(cust-corp.name-short)
            mCustID   = rep183.cust-id.
   END.
   WHEN "Ч" THEN
   DO:
      FIND FIRST person WHERE person.person-id = rep183.cust-id NO-LOCK NO-ERROR.
      IF AVAIL person THEN
         ASSIGN
            mCustName = TRIM(person.name-last) + " " + 
                        TRIM(person.first-names)
            mCustID   = rep183.cust-id.
   END.
   WHEN "Б" THEN
   DO:
      FIND banks WHERE banks.bank-id = rep183.cust-id NO-LOCK NO-ERROR.
      IF AVAIL banks THEN
         ASSIGN
            mCustName = IF TRIM(banks.short-name) EQ "" 
                        THEN TRIM(banks.name) 
                        ELSE TRIM(cust-corp.name-short)
            mCustID   = rep183.cust-id.
   END.
   END CASE.
   
   mCustName = TRIM(REPLACE(mCustName,"?","")).
   IF mCustName EQ "" THEN mCustName = "Не найдено название".

   /*остаток на БС*/
   RUN acct-pos IN h_base(rep183.acct,rep183.currency,mRepDate,mRepDate,CHR(251)).

/*   MESSAGE rep183.acct ";" sh-bal ";" sh-val*/
/*   VIEW-AS ALERT-BOX.                       */

/*   mOst = IF rep183.currency NE ""                         */
/*      THEN TRIM(STRING(ABS(sh-val),"->>>,>>>,>>>,>>9.99")) */
/*      ELSE TRIM(STRING(ABS(sh-bal),"->>>,>>>,>>>,>>9.99")).*/
      
   mOst = IF rep183.currency NE ""
      THEN TRIM(STRING(sh-val * - 1,"->>>,>>>,>>>,>>9.99"))
      ELSE TRIM(STRING(sh-bal * - 1,"->>>,>>>,>>>,>>9.99")).   
         
   ASSIGN
      rep183.ost       = mOst
      rep183.cust-name = mCustName
      mOpLst           = "".

   FOR EACH op-entry WHERE
          op-entry.op-date EQ mRepDate
      AND op-entry.acct-db EQ rep183.acct
      NO-LOCK,
      FIRST op OF op-entry WHERE
         NOT (op.doc-num   BEGINS("П"))
/*         AND  op.op-status GE CHR(251)*/
      NO-LOCK:

      IF op-entry.amt-rub GT 0 THEN
      ASSIGN
         mOpLst  = IF mOpLst EQ "" THEN TRIM(STRING(op.op)) ELSE mOpLst + "," + TRIM(STRING(op.op))
         mOrdPay = op.order-pay. 
   END.
   ASSIGN
      rep183.op-lst  = mOpLst
      rep183.ord-pay = mOrdPay.
END.

/*Вывод отчета*/
/*
1.	   Код клиента;
2.	   Наименование;
3.	   Номер р/счета: Номер р/счета клиента по которому произошло дебетование по счету в текущем опер. дне;
4.	   Остаток на р/счете: РЕАЛЬНЫЙ ОСТАТОК ДЕНЕЖНЫХ СРЕДСТВ НА МОМЕНТ ФОРМИРОВАНИЯ ОТЧЕТА.
5.	   Очередность платежа: поле очередность платежа в расчетном документе при дебетовании р/счета;
6.	   Дата документа: Дата первичного документа;
7.	   Блокировки: Наименование блокировки
8.	   Сумма блокировки: Сумма на которую установлена блокировка.
*/

{rep-183-h.i}

mCnt = 0.
FOR EACH rep183 NO-LOCK:
   IF {assigned rep183.op-lst} THEN
   DO:
      mCnt = mCnt + 1.
      
      PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s72"><Data ss:Type="String">' + STRING(rep183.cust-id) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s70"><Data ss:Type="String">' + rep183.cust-name + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s73"><Data ss:Type="String">' + DelFilFromAcct(rep183.acct) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="Number">' + rep183.ost       + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s72"><Data ss:Type="String">' + 
         (IF rep183.contract EQ ""  THEN "Нет" ELSE rep183.contract)  + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s72"><Data ss:Type="String">' + 
         (IF rep183.nam-block EQ "" THEN "Нет" ELSE rep183.nam-block) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s71"><Data ss:Type="Number">' + 
         TRIM(STRING(ABS(rep183.sum-block),"->>>,>>>,>>>,>>9.99")) + '</Data></Cell>' SKIP.
      PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
      
      DO mInt = 1 TO NUM-ENTRIES(rep183.op-lst):
         
         FIND FIRST op WHERE
            op.op EQ INT64(ENTRY(mInt,rep183.op-lst,","))
         NO-LOCK NO-ERROR.
         IF AVAIL(op) THEN
         DO:
            FIND FIRST op-entry OF op
            NO-LOCK NO-ERROR.
            IF AVAIL(op-entry) THEN
            DO:
               PUT STREAM out-stream UNFORMATTED '   <Row>' SKIP.
               PUT STREAM out-stream UNFORMATTED '    <Cell ss:StyleID="s72"><Data ss:Type="String">' + "Документ" + '</Data></Cell>' SKIP.
               PUT STREAM out-stream UNFORMATTED '    <Cell ss:MergeAcross="5" ss:StyleID="m44471920"><Data ss:Type="String">' + 
                  "Номер: "  + TRIM(op.doc-num) + 
                  " Сумма: " + TRIM(STRING(op-entry.amt-rub,"->>>,>>>,>>>,>>9.99")) + 
                  " Оч.плат: " + TRIM(op.order-pay) +
                  " Статус: " + TRIM(op.op-status) +
                  " Назн: " + REPLACE(REPLACE(TRIM(op.details),"<",""),">","") + '</Data></Cell>' SKIP.
               PUT STREAM out-stream UNFORMATTED '   </Row>' SKIP.
            END.
         END.
      END.
   END.
END.

{rep-183-f.i}

OUTPUT STREAM out-stream CLOSE.

/*IF mCnt NE 0                                            */
/*THEN MESSAGE "Отчет готов."           VIEW-AS ALERT-BOX.*/
/*ELSE MESSAGE "Нет данных для отчета." VIEW-AS ALERT-BOX.*/
  
IF mCnt NE 0 
THEN MESSAGE "Отчет готов.".
ELSE MESSAGE "Нет данных для отчета.".

RUN sndbispc ("file=" + mFileName + ";class=bq").

RETURN.
