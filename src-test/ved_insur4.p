/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: ved_insur.p
      Comment: Отчет по перечислениям в пользу страховых компаний
   Parameters:
         Uses:
      Used by:
      Created: 11/03/13
     Modified: 11/03/13 Serge
*/
{globals.i}
{intrface.get loan}
{intrface.get i254}
{intrface.get comm}
{intrface.get lngar}
{intrface.get chwch}
{intrface.get xobj}     /* Библиотека для работы с объектами. */
{intrface.get db2l}
{client.i}
{lshpr.pro}
/*{tmprecid.def}*/
{wordwrap.def}
{svarloan.def}
/*{navigate.def}*/
/*{loan_par.def &new = new}
{flt-file.i}*/
{sh-defs.i}
  
DEFINE INPUT PARAMETER iParStr AS CHARACTER NO-UNDO.

DEFINE NEW SHARED STREAM vvs.
DEFINE VARIABLE fname AS CHARACTER  INIT "./ved_insur.csv"  NO-UNDO.
DEFINE VARIABLE delim AS CHARACTER INIT ";" FORMAT "x(1)" NO-UNDO.
DEFINE VARIABLE eol AS CHARACTER FORMAT "x(2)" NO-UNDO.
eol = CHR(13) + CHR(10).

/*затычки для  вызова RE_PARAM*/
DEFINE VARIABLE a1 			   AS DECIMAL NO-UNDO.
DEFINE VARIABLE a2 			   AS DECIMAL NO-UNDO.
DEFINE VARIABLE par_0  		   AS DECIMAL NO-UNDO.
/**/
DEFINE BUFFER bloan     FOR loan.
DEFINE BUFFER bcust-corp     FOR cust-corp.
DEFINE BUFFER bterm-obl      FOR term-obl.
DEFINE BUFFER zterm-obl      FOR term-obl.
DEFINE VARIABLE mSumm AS DECIMAL NO-UNDO.
DEFINE VARIABLE mName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctSsud AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctCrR AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctCrRBal AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAcctCrRCur AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCID AS CHARACTER NO-UNDO.
DEFINE VARIABLE rSum AS DECIMAL NO-UNDO.
DEFINE VARIABLE rOst AS DECIMAL NO-UNDO.
DEFINE VARIABLE vStrh AS CHARACTER NO-UNDO.
DEFINE VARIABLE rDate AS DATE NO-UNDO.
DEFINE VARIABLE rDays AS INTEGER NO-UNDO.
DEFINE VARIABLE err-since AS INTEGER NO-UNDO.
DEFINE VARIABLE choice AS log NO-UNDO.
DEFINE VARIABLE months AS INTEGER.
DEFINE VARIABLE days AS INTEGER.
DEFINE VARIABLE poluch AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-end-date AS DATE NO-UNDO.

DEFINE TEMP-TABLE Loan-n
   FIELD rec_entry AS ROWID
   FIELD cust-cat AS CHARACTER
   FIELD cust-id AS INT64
   FIELD cli-name AS CHARACTER
   FIELD doc-ref AS CHARACTER
   FIELD open-date AS DATE
   FIELD end-date AS DATE
   FIELD docno AS CHARACTER
   FIELD op-date AS DATE
   FIELD acct-db AS CHARACTER
   /*    FIELD summ AS DECIMAL*/
   FIELD type AS CHARACTER
   FIELD buyer AS CHARACTER
   FIELD curr AS CHARACTER
   FIELD branch-name AS CHARACTER
   FIELD cont-type LIKE loan.cont-type
   FIELD insur-name AS CHARACTER /* название страховой компании */
   FIELD vidstr AS CHARACTER
   FIELD product AS CHARACTER
   FIELD cr-open-date AS DATE
   FIELD cr-end-date AS DATE
   FIELD cr-cont-code AS CHARACTER
   FIELD cr-cont-type AS CHARACTER
   FIELD cr-loan-status LIKE loan.loan-status
   FIELD cr-summ AS DECIMAL
   FIELD cr-stavka AS DECIMAL
   FIELD cr-date_vid AS DATE

   FIELD percent AS DECIMAL
   FIELD dat-sum AS DATE
   FIELD sum AS DECIMAL
   FIELD dat-fsum AS DATE
   FIELD fsum AS DECIMAL
   FIELD cli-birthday AS DATE
   FIELD cli-document AS CHARACTER
   FIELD cli-document-id AS CHARACTER
   FIELD cli-gender AS CHARACTER
   /*vvv*/
   FIELD cli-telphone AS CHARACTER
   FIELD cli-person-id AS INT64
   /* plus.vvv  19/06/2014 */
   FIELD sumKASKO AS DECIMAL
   /**/
   /**/
   FIELD months AS INTEGER
   FIELD insur-dpl AS DATE
   FIELD insur-spl AS DECIMAL
   FIELD name-ben AS CHARACTER
   FIELD vznos-avto AS DECIMAL
   FIELD marka-avto AS CHARACTER
   FIELD year-avto AS CHARACTER
   FIELD str-end-date AS DATE
   FIELD date_rast AS CHARACTER
   FIELD poluchstrah AS CHARACTER
   FIELD gorod00 AS CHARACTER
   .
/*DEF BUFFER bloan-n FOR loan-n.*/
/*DEFINE QUERY qLoan FOR bloan, loan.*/

{getdates.i}
{setdest.i &col=170 }

err-since = 0.


/*--------- собираем все страховые компании с реквизитами ------------*/
/*
DEFINE TEMP-TABLE insur_corp
    FIELD cust-id   LIKE cust-corp.cust-id
    FIELD benacct   AS CHARACTER
    FIELD bank-code AS CHARACTER
    .
    
FOR EACH loan
 WHERE loan.class-code EQ "insurance"
   AND loan.contract   EQ 'СТРАХ'
   AND loan.open-date <= end-date NO-LOCK ON ERROR UNDO, THROW:
    FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
    IF NOT AVAIL cust-corp THEN NEXT.
    /*UNDO, THROW NEW Progress.Lang.AppError( "не найдено юл " + string(loan.cust-id)).
    MESSAGE "не найдено юл " + string(loan.cust-id) VIEW-AS ALERT-BOX.*/
    FIND FIRST insur_corp WHERE insur_corp.cust-id EQ loan.cust-id NO-ERROR.
    IF (NOT AVAIL insur_corp) AND cust-corp.benacct NE ? AND 
    cust-corp.benacct NE ""
     THEN DO:
    CREATE insur_corp.
    ASSIGN
    insur_corp.cust-id = loan.cust-id
    insur_corp.benacct = cust-corp.benacct
    insur_corp.bank-code = cust-corp.bank-code
    .
    END.
    poluch = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"полстрахпрем","").
    IF poluch NE "" THEN DO:
    FIND FIRST code
     WHERE code.class EQ 'strahpol' AND code.code EQ poluch NO-LOCK NO-ERROR.
    IF AVAIL code THEN DO:
        FIND FIRST insur_corp
         WHERE insur_corp.bank-code EQ code.misc[3]
           AND insur_corp.benacct EQ code.description[2] NO-ERROR.
        IF (NOT AVAIL insur_corp) AND code.description[2] NE ? AND 
        code.description[2] NE ""
         THEN DO:
        CREATE insur_corp.
        ASSIGN
        insur_corp.cust-id = ?
        insur_corp.benacct = code.description[2]
        insur_corp.bank-code = code.misc[3]
        .
        /*if insur_corp.benacct eq "40702810355000001465" then
         message loan.parent-cont-code view-as alert-box.*/
        END.
    END.
    END.
END.
*/

DO ON ERROR UNDO,THROW:
   FOR EACH op-entry
      WHERE
      op-entry.filial-id EQ shFilial AND
      op-entry.op-date >= beg-date AND
      op-entry.op-date <= end-date AND /* надо разделять платежи за разные года */
      (NOT op-entry.acct-cr BEGINS "706") AND
      (NOT op-entry.acct-cr BEGINS "45") AND
      (NOT op-entry.acct-cr BEGINS "47") AND
      (op-entry.acct-db BEGINS "40817" OR op-entry.acct-db BEGINS "40820")
      AND CAN-FIND(FIRST loan-acct
      WHERE CAN-DO( 'кредит', loan-acct.contract)
      AND loan-acct.acct EQ op-entry.acct-db
      AND CAN-DO('КредРасч', loan-acct.acct-type)
      NO-LOCK)
      NO-LOCK,
      FIRST OP OF op-entry NO-LOCK ON ERROR UNDO, THROW:
   
      FIND FIRST loan-n
         WHERE loan-n.rec_entry EQ ROWID( op-entry)
         NO-LOCK NO-ERROR.
      /**/
      IF AVAILABLE loan-n THEN NEXT.

      /*
     FIND FIRST insur_corp
      WHERE op-entry.acct-cr BEGINS insur_corp.benacct
         OR op.ben-acct EQ insur_corp.benacct NO-LOCK NO-ERROR.
      IF NOT AVAIL insur_corp THEN DO:
      NEXT.
      END.*/

      FOR EACH loan-acct
         WHERE loan-acct.contract EQ 'Кредит'
         AND loan-acct.acct EQ op-entry.acct-db
         AND CAN-DO( 'КредРасч', loan-acct.acct-type)
         NO-LOCK,
         EACH loan
         WHERE loan.parent-contract  EQ loan-acct.contract
         AND loan.parent-cont-code EQ loan-acct.cont-code
         AND loan.class-code EQ "insurance"
         AND loan.contract   EQ 'СТРАХ'
         AND loan.open-date <= op.op-date
         NO-LOCK,
         EACH term-obl OF loan
         WHERE term-obl.idnt     EQ 1
         NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ) ON ERROR UNDO,THROW:
      /**/
      IF term-obl.amt-rub EQ op-entry.amt-rub
         /*AND term-obl.end-date - 3 <= op.op-date вдруг с датами накосячили
         AND op.op-date - term-obl.end-date < 365*/
         THEN 
      DO:
         DEFINE VARIABLE truRek AS LOG NO-UNDO.
         truRek = TRUE.
         /* надо проверить что реквизиты действительно совпадают с реквизитами договора *
         IF loan.cust-cat EQ "ю" THEN DO:
             FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
             IF AVAIL cust-corp THEN
             IF cust-corp.benacct NE ? AND cust-corp.benacct NE ""
                AND ((cust-corp.bank-code NE "044599129" AND cust-corp.benacct EQ op.ben-acct)
                   OR (cust-corp.bank-code EQ "044599129" AND cust-corp.benacct EQ delfilfromacct(op-entry.acct-cr)))
              THEN truRek = True.
         END. ELSE
         IF loan.cust-cat EQ "ч" THEN DO:
             FIND FIRST person WHERE person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
             IF AVAIL person THEN
             IF person.benacct NE ? AND person.benacct NE ""
              AND ((person.bank-code NE "044599129" AND person.benacct EQ op.ben-acct)
                   OR (person.bank-code EQ "044599129" AND person.benacct EQ delfilfromacct(op-entry.acct-cr)))
              THEN truRek = True.
         END. */
         poluch = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"полстрахпрем","").
         /*IF poluch NE "" THEN DO:
             FIND FIRST code
              WHERE code.class EQ 'strahpol'
                AND code.code EQ poluch
                 NO-LOCK NO-ERROR.
             IF AVAIL code
                /*AND insur_corp.bank-code EQ code.misc[3]*/
            AND op.ben-acct EQ code.description[2]
              THEN truRek = True.
         END.*/
         /*IF NOT truRek THEN NEXT.*/
	
         FIND FIRST bloan
            WHERE bloan.contract  EQ loan-acct.contract
            AND bloan.cont-code EQ loan-acct.cont-code
            NO-LOCK.
		
         DEFINE VARIABLE pr-c AS CHARACTER NO-UNDO.
         DEFINE VARIABLE pr-c-2 AS CHARACTER NO-UNDO.

         pr-c = GetXattrValueEx("loan",bloan.contract + "," + bloan.cont-code,"продавец","").
         pr-c-2 = GetXattrValueEx("loan",bloan.contract + "," + bloan.cont-code,"ПолучательРекв","").
         /**/
         IF pr-c-2 NE "" THEN 
         DO:
            DEFINE VARIABLE i1 AS INTEGER NO-UNDO.
            DEFINE VARIABLE i2 AS INTEGER NO-UNDO.
    
            i1 = INDEX(pr-c-2, "^").
            i2 = length(pr-c-2) - i1.
            i1 = i1 + 1.
            pr-c-2 = substring(pr-c-2, i1, i2).
  
            i1 = INDEX(pr-c-2, "^").
            i2 = length(pr-c-2) - i1.
            i1 = i1 + 1.
            pr-c-2 = substring(pr-c-2, i1, i2).

            i1 = INDEX(pr-c-2, "^").
            i2 = length(pr-c-2) - i1 - 2.
            i1 = i1 + 2.
            pr-c-2 = substring(pr-c-2, i1, i2).

            pr-c-2 = replace(pr-c-2, '?', '"').
         END.

         CREATE loan-n.
         ASSIGN
            /*loan-n.cli-name  = op-entry.acct-db*/
            loan-n.vidstr   = op-entry.acct-db
            loan-n.rec_entry = ROWID( op-entry)
            loan-n.insur-dpl = op.op-date
            /*
            loan-n.insur-spl = op-entry.amt-rub
            */
            .
         /*
         /* plus.vvv 19/06/2014 */
         /* найдем %% ставку при выдаче КД*/
         FOR EACH COMM-RATE 
             WHERE COMM-RATE.KAU = bloan.contract + "," + bloan.cont-code
             AND COMM-RATE.COMMISSION = "%Кред"
             BY COMM-RATE.SINCE
             :
                 /**/
                 loan-n.first_rate = COMM-RATE.RATE-COMM.
                 /**/
                 LEAVE.
                 /**/
         END.
         /* действующая %% ставка */
         FOR EACH COMM-RATE 
             WHERE COMM-RATE.KAU = bloan.contract + "," + bloan.cont-code
             AND COMM-RATE.COMMISSION = "%Кред"
             AND COMM-RATE.SINCE < end-date
             BY COMM-RATE.SINCE DESCENDING
             :
                 /**/
                 loan-n.last_rate = COMM-RATE.RATE-COMM.
                 /**/
                 LEAVE.
                 /**/
         END.
         /* первоначальная сумма КД */
         /*
         FIND FIRST zterm-obl OF LOAN
             WHERE zterm-obl.IDNT = 2
             AND zterm-obl.NN = 0
         NO-LOCK NO-ERROR.
             /**/
             IF AVAIL zterm-obl THEN
                 loan-n.sum_orig = zterm-obl.AMT-RUB.
         */
         /* найдем остаток по кредиту */
         RUN RE_PARAM IN h_Loan
                     (0, /*код параметра*/
                     end-date, /*дата*/
                     bloan.contract,  /* назначение договора */
                     bloan.cont-code, /* код договора */
                     OUTPUT par_0, 	/* значение параметра */
                     OUTPUT a1,
                     OUTPUT a2).
         /**/
         loan-n.sum_remn = par_0.
         /* plus.vvv 19/06/2014 */	
         */
         /*IF op.name-ben EQ ? OR op.name-ben EQ "" THEN DO:
             FIND FIRST acct WHERE acct.acct EQ op-entry.acct-cr NO-LOCK.
             IF acct.cust-cat EQ "ю" OR acct.cust-cat EQ "ч" THEN RUN RE_CLIENT (acct.cust-cat, acct.cust-id, INPUT-OUTPUT loan-n.name-ben).
         END. ELSE loan-n.name-ben  = op.name-ben.*/

         loan-n.name-ben = pr-c-2.

         IF pr-c-2 NE "" THEN 
         DO:
            loan-n.name-ben = pr-c-2.
         END.
         ELSE
         DO:
            FIND FIRST code
               WHERE code.class EQ 'strahpol'
               AND code.code EQ pr-c NO-LOCK NO-ERROR.
            IF AVAILABLE code THEN
               loan-n.name-ben = code.misc[6].
         END.
         FIND FIRST code
            WHERE code.class EQ 'ГородПоКредиту'
            AND code.code EQ SUBSTR(loan.cont-code,1,2) NO-LOCK NO-ERROR.
         IF AVAILABLE code THEN
            loan-n.gorod00 = 'Г ' + code.name.

         ASSIGN
            loan-n.doc-ref   = loan.doc-ref 
            WHEN truRek
            loan-n.open-date = loan.open-date 
            WHEN truRek
            loan-n.vidstr = GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code, "vidstr","") 
            WHEN truRek
            .

         loan-n.date_rast = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"DateRastor","").
	    
         /**/
         /* plus.vvv */
         /**/
         IF INDEX(loan-n.vidstr, "КАСКО") > 0 THEN
            loan-n.sumKASKO = op-entry.amt-rub.
         ELSE
            loan-n.insur-spl = op-entry.amt-rub.
         /* plus.vvv */		
         /**/
         FIND FIRST cust-role
            WHERE cust-role.file-name EQ "loan" AND
            cust-role.surrogate EQ loan.contract + ',' + loan.cont-code AND
            cust-role.class-code EQ "Застрахованный" AND
            cust-role.cust-cat = "Ч" NO-LOCK NO-ERROR.
         IF AVAILABLE cust-role THEN 
         DO:
            FIND FIRST person WHERE person.person-id = INT64(cust-role.cust-id) NO-LOCK NO-ERROR.
         END. 
         ELSE 
         DO:
            FIND FIRST person WHERE person.person-id = loan.cust-id NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE person THEN
            ASSIGN
               loan-n.cust-cat  = "ч"
               loan-n.cust-id   = person.person-id
               .
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK.
         ASSIGN
            loan-n.cli-person-id   = person.person-id WHEN AVAIL( person )
            loan-n.cli-name        = person.name-last + " " + person.first-names WHEN AVAIL( person )
            loan-n.cli-birthday    = person.birthday WHEN AVAIL( person )
            loan-n.cli-document-id = person.document-id WHEN AVAIL( person )
            loan-n.cli-document    = person.document /*+ 	" выдан " + GetXAttrValue("person",STRING(person.person-id),"Document4Date_vid") + " " + person.issue*/ WHEN AVAIL( person )
            loan-n.cli-gender   = (IF person.gender THEN "М" ELSE "Ж" ) WHEN AVAIL( person )
		
            /*vvv*/
            loan-n.cli-telphone = ("'" + person.phone[1] + "," + person.phone[2]) WHEN AVAIL( person )
            /*vvv*/
            loan-n.insur-name = STRING(cust-corp.cust-stat) + " " + STRING(cust-corp.name-corp) WHEN AVAIL( cust-corp )
            .
         ASSIGN
            loan-n.cr-end-date = bloan.end-date 
            WHEN truRek.
         FIND FIRST branch OF bloan NO-LOCK NO-ERROR.
         loan-n.branch-name = (IF AVAILABLE branch THEN string(bloan.branch-id) + " - " + branch.short-name ELSE "").
         /* сумма кредита */
         FIND FIRST bterm-obl 
            WHERE bterm-obl.contract  EQ bloan.contract
            AND bterm-obl.cont-code EQ bloan.cont-code
            AND bterm-obl.idnt      EQ 2
            /*AND bterm-obl.end-date  GE loan-cond.since */
            NO-LOCK NO-ERROR.
         ASSIGN
            loan-n.cr-summ = (IF AVAILABLE bterm-obl THEN bterm-obl.amt ELSE 0) 
            WHEN truRek.

         ASSIGN
            loan-n.vznos-avto = DEC( GetXattrValueEx("loan",bloan.contract + "," + bloan.cont-code,"rko11_price","0")) -
            DEC( GetXattrValueEx("loan",bloan.contract + "," + bloan.cont-code,"rko1_price","0")) NO-ERROR.
         /* данные авто */
         FIND FIRST bterm-obl 
            WHERE bterm-obl.contract  EQ bloan.contract
            AND bterm-obl.cont-code EQ bloan.cont-code
            AND bterm-obl.idnt      EQ 5
            NO-LOCK NO-ERROR.
         ASSIGN
            loan-n.marka-avto = IF AVAILABLE bterm-obl THEN GetXattrValueEx( "term-obl", GetSurrogate("term-obl",ROWID(bterm-obl)), "TCbrand", "") ELSE ""
            loan-n.year-avto  = IF AVAILABLE bterm-obl THEN GetXattrValueEx( "term-obl", GetSurrogate("term-obl",ROWID(bterm-obl)), "TCyear", "") ELSE ""
            .
         IF loan-n.marka-avto EQ "" THEN
            loan-n.marka-avto = IF AVAILABLE bterm-obl THEN GetXattrValueEx( "term-obl", GetSurrogate("term-obl",ROWID(bterm-obl)), "TCmodel", "") ELSE "".
         /* смотрим был ли выдан кредит */
         FOR EACH loan-int 
            WHERE loan-int.contract  EQ bloan.contract
            AND loan-int.cont-code EQ bloan.cont-code
            AND loan-int.id-d EQ 0
            NO-LOCK BY mdate:
            ASSIGN
               loan-n.cr-date_vid = loan-int.mdate  
               WHEN truRek.
            LEAVE.
         END.
         /* % вознаграждение банку */
         ASSIGN
            loan-n.percent = DEC(GetXAttrValueEx("cust-corp",
            STRING(loan.cust-id),
            "insurPercent",
            "")) WHEN truRek.
         loan-n.str-end-date = loan.end-date.

         loan-n.poluchstrah = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"ПолучательРекв","").

         IF NUM-ENTRIES(loan-n.poluchstrah,"^") GE 5 THEN
            loan-n.poluchstrah = ENTRY(5,loan-n.poluchstrah,"^").
         ELSE
            loan-n.poluchstrah = "".
      END.
   END.
END.

OUTPUT STREAM vvs TO VALUE (fname)
   UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
PUT STREAM vvs UNFORMATTED
   "страховая компания" delim
   "программа страхования" delim
   "продавец" delim
   "фио застрахованного лица" delim
   "дата рождения застрахованного" delim
   "адрес рег. застрахованного" delim
   "почтовый адрес застрахованного" delim
   "телефон застрахованного" delim
   "тип документа" delim
   "№ документа" delim
   "кем выдан документ" delim
   "дата выдачи документа" delim
   "№ страхового сертификата" delim
   "дата страхового сертификата" delim
   "Дата предоставления кредита" delim
   "компания" delim
   "срок предоставления кредита" delim
   "сумма кредита в рублях" delim
   "сумма первоначального взноса за авто" delim
   "марка авто" delim
   "год выпуска авто" delim
   "д.переч.страховой премии" delim
   "страховая премия по НС" delim
   "страховая премия по КАСКО" delim
   "регион" delim
   "комиссия банка с ндс в рублях" delim
   "дата окончания страхового договора" delim
   "дата расторжения страхового договора" DELIM
   "Наименование получателя денежных средств по договору страхования" delim
   "город привлечения"
   eol.

FOR EACH Loan-n
   WHERE loan-n.cust-id NE ? NO-LOCK
   BREAK BY loan-n.cr-cont-code BY loan-n.open-date ON ERROR UNDO,THROW:
   FIND FIRST person WHERE person.person-id EQ loan-n.cli-person-id NO-LOCK NO-ERROR.
    
   DEFINE VARIABLE oAdrFakt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE oAdrProp AS CHARACTER NO-UNDO.
   oAdrProp = "".
   oAdrFakt = "".
   IF AVAILABLE person THEN 
   DO:
      RUN RetAdr.p(person.person-id,  "Ч", "АдрФакт", end-date, OUTPUT oAdrFakt).
      RUN RetAdr.p(person.person-id,  "Ч", "АдрПроп", end-date, OUTPUT oAdrProp).
   END.
   DEFINE VARIABLE kem AS CHARACTER NO-UNDO.
   IF AVAILABLE person THEN
      kem = GetXAttrValueEx("person",string(person.person-id),"Document4Date_vid","").
   ELSE kem = "".
   kem = replace (kem, CHR(10), ' ').
   kem = replace (kem, CHR(13), ' ').
   PUT STREAM vvs UNFORMATTED
      loan-n.insur-name delim
      loan-n.vidstr delim
      loan-n.name-ben delim
      loan-n.cli-name delim
      loan-n.cli-birthday delim
      oAdrProp delim
      oAdrFakt delim
      loan-n.cli-telphone delim
      loan-n.cli-document-id delim
      loan-n.cli-document delim
      (IF AVAILABLE person THEN replace(person.issue,CHR(10),' ') ELSE "") delim
      (IF AVAILABLE person THEN kem ELSE "") delim
      loan-n.doc-ref delim
      loan-n.open-date delim
      loan-n.cr-date_vid delim
      "оао ""плюс банк""" delim
      loan-n.cr-end-date delim
      loan-n.cr-summ delim
      loan-n.vznos-avto delim
      loan-n.marka-avto delim
      loan-n.year-avto delim
      loan-n.insur-dpl delim
      loan-n.insur-spl delim
      loan-n.sumKASKO delim
      loan-n.branch-name delim
      ROUND( loan-n.insur-spl * loan-n.percent / 100,2 ) delim
      loan-n.str-end-date delim
      loan-n.date_rast DELIM
      loan-n.poluchstrah delim
      loan-n.gorod00
      eol.
END.
OUTPUT STREAM vvs CLOSE.

CATCH eAnyError AS Progress.Lang.Error:
   PUT UNFORMATTED "ошибка: " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
END CATCH.
END.

PUT UNFORMATTED "Нажмите ESC для выгрузки отчета в BisPC_" SKIP(1).
{preview.i &col=170}

RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}
