DEF STREAM vvs.

PROCEDURE XmlLoanInfo.
    DEF PARAM BUFFER bwapp FOR wapp.messages.
    DEF INPUT PARAMETER claim-id AS CHAR NO-UNDO.
    DEF INPUT PARAMETER task-id AS CHAR NO-UNDO.

    DEFINE VARIABLE hSAXWriter AS HANDLE.
    DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lmsg AS LONGCHAR NO-UNDO.
    DEF BUFFER bAnswer FOR wapp.messages.
    DEF VAR newID AS INT64 NO-UNDO.
    DEF BUFFER bloan FOR loan.
    /*DEFINe VAR loanSurr AS CHAR NO-UNDO.*/
    DEF BUFFER bloan-cond FOR loan-cond.
    DEF VAR vSurr AS CHAR NO-UNDO.
    DEF VAR vSurrLoanCond AS CHAR NO-UNDO.
    DEF VAR vSurrCommRate AS CHAR NO-UNDO.
    DEF VAR i AS INT NO-UNDO.

    FIND FIRST signs
     WHERE signs.file-name EQ 'loan'
       AND signs.code EQ 'PLDealID'
       AND signs.code-value EQ claim-id
       NO-LOCK NO-ERROR.
    IF NOT AVAIL signs THEN
        UNDO , THROW NEW Progress.Lang.AppError(
         "договор с номером заявки '" + string(claim-id) + 
         "' не найден.").
    DEF BUFFER bsigns FOR signs.
    FIND FIRST bsigns
     WHERE bsigns.file-name EQ 'loan'
       AND bsigns.code EQ 'PLDealID'
       AND bsigns.code-value EQ claim-id
       AND bsigns.surrogate NE signs.surrogate
       NO-LOCK NO-ERROR.
    IF AVAIL bsigns THEN
        UNDO , THROW NEW Progress.Lang.AppError(
         "несколько договоров с номером заявки '" + string(claim-id) + 
         "', непонятно какой выгружать.").

    FIND FIRST bloan
     WHERE bloan.contract EQ ENTRY( 1, signs.surrogate)
       AND bloan.cont-code EQ ENTRY( 2, signs.surrogate)
       NO-LOCK.

    CREATE SAX-WRITER hSAXWriter.
    hSAXWriter:FORMATTED = TRUE.
    hSAXWriter:ENCODING = "windows-1251".
    lOK = hSAXWriter:SET-OUTPUT-DESTINATION("longchar", lmsg).
    lOK = hSAXWriter:START-DOCUMENT( ).

    DO ON ERROR UNDO,THROW:
    vSurr = GetSurrogateBuffer( "loan", (BUFFER bloan:HANDLE)).

    IF bloan.cust-cat NE 'Ч' THEN
        UNDO , THROW NEW Progress.Lang.AppError(
         "категория клиента на договоре должна быть Ч").
    FIND FIRST person
     WHERE person.person-id EQ bloan.cust-id NO-LOCK.
    FIND FIRST bloan-cond
     WHERE bloan-cond.contract EQ bloan.contract
       AND bloan-cond.cont-code EQ bloan.cont-code NO-LOCK.
    vSurrLoanCond = GetSurrogate("loan-cond",ROWID(bloan-cond)).
    
    lOK = hSAXWriter:START-ELEMENT("Loan").
    lOK = hSAXWriter:DECLARE-NAMESPACE("http://plus-bank.ru/Pipeline2BIS/ContractDataResponseV8", "").
    lOK = hSAXWriter:DECLARE-NAMESPACE("http://www.w3.org/2001/XMLSchema-instance", "xsi").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT( "claim-id", GetXAttrValueEx( "loan", vSurr, "PLDealID", "") ).
    IF task-id NE ? THEN
      lOK = hSAXWriter:WRITE-DATA-ELEMENT( "task-id", task-id ).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("doc-ref", bloan.doc-ref).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("open-date", XmlDate(bloan.open-date)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("end-date", XmlDate(bloan.end-date)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("branch-id", bloan.branch-id).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("filial-id", bloan.filial-id).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("cred-term",
	GetXAttrValueEx( "loan-cond" , vSurrLoanCond, "NMonthes", "0")).
    DEF VAR fsum AS DEC NO-UNDO.
    fsum = 0.
    FOR EACH term-obl WHERE
             term-obl.contract  = bloan.contract
         AND term-obl.cont-code = bloan.cont-code
         AND term-obl.idnt      = 2
     NO-LOCK BY term-obl.end-date:
      fsum = term-obl.amt-rub.
      LEAVE.
    END.
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( fsum)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("price-avto", XmlDec( DEC( GetXAttrValueEx( "loan", vSurr, "rko11_price",?)))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-kred-avto", XmlDec( DEC( GetXAttrValueEx( "loan", vSurr, "rko1_price",?)))).

    /*DEF VAR frate AS DEC NO-UNDO.
    frate = 0.
    FIND FIRST comm-rate WHERE
             comm-rate.commission = '%Кред'
         AND comm-rate.kau  = bloan.contract + "," + bloan.cont-code
         AND comm-rate.currency = bloan.currency
         AND comm-rate.acct      = "0"
         AND comm-rate.since     = bloan.open-date
     NO-LOCK NO-ERROR.
    IF NOT AVAIL comm-rate THEN
	UNDO, THROW NEW Progress.Lang.AppError( "нет ставки кредита").
    IF comm-rate.rate-fixed NE NO THEN
      UNDO, THROW  NEW Progress.Lang.AppError( "ставка кредита указана не в процентах").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("rate-cred", XmlDec( comm-rate.rate-comm)).
*/

/*    FIND FIRST comm-rate WHERE
             comm-rate.commission = 'РКО'
         AND comm-rate.kau  = bloan.contract + "," + bloan.cont-code
         AND comm-rate.currency = bloan.currency
         AND comm-rate.acct      = "0"
         AND comm-rate.since     = bloan.open-date
     NO-LOCK NO-ERROR.
    IF NOT AVAIL comm-rate THEN
	UNDO, THROW NEW Progress.Lang.AppError( "нет ставки рко").
    IF AVAIL comm-rate THEN DO:
	IF comm-rate.rate-fixed EQ NO THEN
          UNDO, THROW  NEW Progress.Lang.AppError( "ставка кредита указана в процентах").
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("rate-rko", XmlDec( comm-rate.rate-comm)).
    END.*/
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("product-id", GetXAttrValueEx( "loan", vSurr, "продкод","")).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("psk",
     TRIM( STRING( DEC(GetXAttrValueEx( "loan", vSurr, "пск","0")) , "->>>>>>9.999"))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("eps",
     TRIM( STRING( DEC(GetXAttrValueEx( "loan", vSurr, "пскбезстрах","0")) , "->>>>>>9.999"))).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("cred-date", string(loan-cond.cred-date)).

    lOK = hSAXWriter:START-ELEMENT("Person").
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("name-last", person.name-last).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("first-names", person.first-names).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("gender", (IF person.gender THEN 'true' ELSE 'false')).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Birthday", XmlDate(person.birthday)).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BirthPlace",
	    GetXAttrValueEx("person", STRING(person.person-id), "BirthPlace", "")).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("country-id", country-id).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("inn", person.inn).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("tax-insp",
	    GetXAttrValueEx("person", STRING(person.person-id), "кодреггни", "")).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("country-id2",
	    GetXAttrValueEx("person", STRING(person.person-id), "country-id2", "")).

    DEF VAR ph AS CHAR NO-UNDO.
    ph = ENTRY(1,person.phone[1]).
    IF ph = "" AND NUM-ENTRIES( person.phone[1]) > 1 THEN ph = ENTRY(2,person.phone[1]).
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("phone-home",ph).

    IF NUM-ENTRIES( person.phone[2]) > 1 THEN ph = ENTRY(2,person.phone[2]). ELSE ph = "".
    lOK = hSAXWriter:WRITE-DATA-ELEMENT( "cell-phone", ph).

    /*lOK = hSAXWriter:WRITE-DATA-ELEMENT("send-bki", '').*/

    /*lOK = hSAXWriter:START-ELEMENT("Addresses").*/
    DEF VAR vAdrType AS INT NO-UNDO.
    DO vAdrType = 1 TO 2 ON ERROR UNDO, THROW:
	FIND FIRST cust-ident
		     WHERE cust-ident.cust-cat       EQ "Ч"
	               AND cust-ident.cust-id        EQ Person.person-id
	               AND cust-ident.class-code     EQ "p-cust-adr"
	               AND cust-ident.cust-code-type EQ ENTRY( vAdrType, "адрпроп,адрфакт")
		     NO-LOCK NO-ERROR.
	IF NOT AVAIL cust-ident THEN NEXT.
	lOK = hSAXWriter:START-ELEMENT("Address").
	{cust-adr.obj 
		     &addr-to-vars = YES
		     &tablefield   = "TRIM(cust-ident.issue)"
	}
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("addr-type", ENTRY( vAdrType, "АдрПроп,АдрФакт")).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "country-id", GetXattrValueEx("cust-ident",
                                                  cust-ident.cust-code-type + ',' + 
                                                  cust-ident.cust-code      + ',' + 
                                                  STRING(cust-ident.cust-type-num),
                                                  "country-id", ""
                                                 )).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "code-gni", GetXAttrValueEx( 'cust-ident',
		    cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),
		    'КодРегГНИ',"")).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "code-reg", "?").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-obl", vOblChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-city", vGorChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-npunkt", vPunktChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-street", vUlChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-house", vDomChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-str", vStrChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-corp", vKorpChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-kv", vKvChar).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT( "addr-idx", IF vAdrIndInt EQ 0 THEN "" ELSE string(vAdrIndInt)).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("since", XmlDate( cust-ident.open-date)).
	lOK = hSAXWriter:END-ELEMENT("Address").
    END. /* DO */
    /*lOK = hSAXWriter:END-ELEMENT("Addresses").*/

    /*-------------- документы ----------------*/
    FOR EACH cust-ident
	     WHERE cust-ident.cust-cat       EQ "Ч"
               AND cust-ident.cust-id        EQ Person.person-id
               AND cust-ident.class-code     EQ "p-cust-ident"
               AND (cust-ident.open-date     <= loan.open-date)
               AND (cust-ident.close-date EQ ? OR cust-ident.close-date > loan.open-date)
     NO-LOCK BREAK BY cust-ident.cust-id ON ERROR UNDO, THROW:
	/*IF FIRST(cust-ident.cust-id) THEN DO:
	    lOK = hSAXWriter:START-ELEMENT("Documents").
	END.*/
	lOK = hSAXWriter:START-ELEMENT("Document").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("docum-type", cust-ident.cust-code-type).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("docum-no", cust-ident.cust-code).
	/*DEF VAR doc-podr AS CHAR NO-UNDO.
	doc-podr = GetXattrValueEx( "cust-ident",
                                GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                                "Подразд", "").
        IF doc-podr <> "" THEN*/
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("docum-podr",
		GetXattrValueEx( "cust-ident",
                                GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                                "Подразд", "")
	    ).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("docum-date", XmlDate( cust-ident.open-date)).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("docum-issue", string(cust-ident.issue)).
	lOK = hSAXWriter:END-ELEMENT("Document").
	/*IF LAST(cust-ident.cust-id) THEN DO:
	    lOK = hSAXWriter:END-ELEMENT("Documents").
	END.*/
    END.
    lOK = hSAXWriter:END-ELEMENT("Person").

    /*------------------ дог.страхования --------------------*/
    DEF BUFFER bloans FOR loan.
    DEF VAR polRekv AS CHAR NO-UNDO.
    DEF VAR polRekv2 AS CHAR NO-UNDO.
    FOR EACH bloans
     WHERE bloans.parent-contract EQ bloan.contract
       AND bloans.parent-cont-code EQ bloan.cont-code
       AND bloans.class-code  = "insurance"
      NO-LOCK ON ERROR UNDO, THROW:
	lOK = hSAXWriter:START-ELEMENT("Contract").
	DEF VAR vstr AS CHAR NO-UNDO.
	DEF VAR vistr AS INT NO-UNDO.
        DEF VAR mStr AS CHARACTER NO-UNDO.
        mStr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "VidStr", "").
        IF mStr EQ "ЖизньВнеш" THEN mStr = "С_ВНЕШ".
 	vistr = LOOKUP(mStr,"ЖизньС1,ЖизньС2,ЖизньС1_КЛ,ЖизньС2_КЛ,КАСКО_К,КАСКО_Н,КАСКО_GAP,С_ВНЕШ").
	IF vistr < 1
	 THEN UNDO, THROW  NEW Progress.Lang.AppError( "не корректный тип договора страхования (" + 
		GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "VidStr", "") + ").").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contract-type", ENTRY( vistr, "С1,С2,С1_КЛ,С2_КЛ,КАСКО_К,КАСКО_Н,КАСКО_GAP,С_ВНЕШ")).
	DEF VAR vPLpolis AS CHAR NO-UNDO.
	vPLpolis = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "PLpolis", "").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("doc-ref", (IF vPLpolis NE "" THEN vPLpolis ELSE bloans.doc-ref)).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("open-date", XmlDate( bloans.open-date)).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("end-date", XmlDate( bloans.end-date)).
	DEF VAR vCsum AS DEC NO-UNDO.
	vCsum = 0.
	FOR EACH term-obl
         WHERE term-obl.contract EQ bloans.contract
           AND term-obl.cont-code EQ bloans.cont-code
           AND term-obl.idnt EQ 1
           NO-LOCK BY term-obl.end-date 
           ON ERROR UNDO, THROW:
            vCsum = term-obl.amt-rub.
	    LEAVE.
	END.
	lOK = hSAXWriter:START-ELEMENT("contract-summa").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Общая").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( vCsum)).
	lOK = hSAXWriter:END-ELEMENT("contract-summa").

	lOK = hSAXWriter:START-ELEMENT("contract-summa").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Общая_НДС").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "nds", "")))).
	lOK = hSAXWriter:END-ELEMENT("contract-summa").

	vstr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "СумПремия_ГАП", ?).
	if vstr NE ? THEN DO ON ERROR UNDO,THROW:
	  lOK = hSAXWriter:START-ELEMENT("contract-summa").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Премия_GAP").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(vstr))).
	  lOK = hSAXWriter:END-ELEMENT("contract-summa").
	END.
	vstr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "СумПремия_ДСАГО", ?).
	if vstr NE ? THEN DO ON ERROR UNDO,THROW:
	  lOK = hSAXWriter:START-ELEMENT("contract-summa").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Премия_ДСАГО").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(vstr))).
	  lOK = hSAXWriter:END-ELEMENT("contract-summa").
	END.
	vstr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "СумПремия_КАСКО", ?).
	if vstr NE ? THEN DO ON ERROR UNDO,THROW:
	  lOK = hSAXWriter:START-ELEMENT("contract-summa").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Премия_КАСКО").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(vstr))).
	  lOK = hSAXWriter:END-ELEMENT("contract-summa").
	END.
	vstr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "СумСтраховая_ГАП", ?).
	if vstr NE ? THEN DO ON ERROR UNDO,THROW:
	  lOK = hSAXWriter:START-ELEMENT("contract-summa").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Страховая_GAP").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(vstr))).
	  lOK = hSAXWriter:END-ELEMENT("contract-summa").
	END.
	vstr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "СумСтраховая_ДСАГО", ?).
	if vstr NE ? THEN DO ON ERROR UNDO,THROW:
	  lOK = hSAXWriter:START-ELEMENT("contract-summa").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Страховая_ДСАГО").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(vstr))).
	  lOK = hSAXWriter:END-ELEMENT("contract-summa").
	END.
	vstr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "СумСтраховая_КАСКО", ?).
	if vstr NE ? THEN DO ON ERROR UNDO,THROW:
	  lOK = hSAXWriter:START-ELEMENT("contract-summa").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Страховая_КАСКО").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(vstr))).
	  lOK = hSAXWriter:END-ELEMENT("contract-summa").
	END.
	vstr = GetXAttrValueEx( "loan", bloans.contract + "," + bloans.cont-code, "СумСтраховая_Общая", ?).
	if vstr NE ? THEN DO ON ERROR UNDO,THROW:
	  lOK = hSAXWriter:START-ELEMENT("contract-summa").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Страховая_общая").
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(vstr))).
	  lOK = hSAXWriter:END-ELEMENT("contract-summa").
	END.



	/*
	DEF VAR pol_ID AS CHAR NO-UNDO.
	pol_ID = GetXattrValueEx("loan",bloans.contract + "," + bloans.cont-code,"полстрахпрем","").
	IF pol_ID NE "" THEN DO:
	    FIND FIRST code
	     WHERE code.class EQ 'strahpol'
	       AND code.code EQ pol_ID
	        NO-LOCK NO-ERROR.
	    IF AVAIL code THEN DO:
		* это полное наименование *
	        lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-name", code.description[1]).
	        lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-inn", code.name).
	        lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-current-account", code.description[2]).
	        lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-corr-account", code.misc[2]).
	        lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-name", code.misc[1]).
	        lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-bik", code.misc[3]).
	        lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-phone", code.misc[4]).
	    END.
	END. ELSE DO:
	    IF bloans.cust-cat EQ "ю" THEN DO:
	        FIND FIRST cust-corp WHERE cust-corp.cust-id EQ bloans.cust-id NO-LOCK NO-ERROR.
	        IF AVAIL cust-corp THEN DO:
		lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-name", cust-corp.name-short).
		lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-inn", cust-corp.inn).
		lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-current-account", cust-corp.benacct).
		lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-corr-account", cust-corp.corr-acct).
		lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-name", "").
		lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-bik", cust-corp.bank-code).
		lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-phone", "").
		END.
	    END.
	END.*/
	
	def var curr-premium-account as char no-undo init ''.
	def var curr-payments-account as char no-undo init ''.
	def var insurance-premium-summa as decimal no-undo init 0.
	def var other-payments-summa as decimal no-undo init 0.
	
	polRekv = GetXattrValueEx("loan",bloans.contract + "," + bloans.cont-code,"получательрекв","").
	IF NUM-ENTRIES(polRekv, '^') < 5
	 THEN UNDO, THROW  NEW Progress.Lang.AppError( "нет реквизитов получателя на договоре страхования.").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-name", ENTRY( 5, polRekv, '^')).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-inn",  ENTRY( 3, polRekv, '^')).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-current-account", ENTRY( 2, polRekv, '^')).
	FIND FIRST banks-code WHERE banks-code.bank-code-type = 'мфо-9'
           AND banks-code.bank-code = ENTRY( 1, polRekv, '^') NO-LOCK NO-ERROR.
        IF AVAIL banks-code
         THEN FIND FIRST banks of banks-code. ELSE RELEASE banks.
        IF AVAIL banks
    	 THEN FIND FIRST banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id
             AND CAN-FIND (banks OF banks-corr WHERE banks.flag-rkc) NO-LOCK NO-ERROR.
         ELSE RELEASE banks.
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-corr-account", (IF AVAIL banks-corr THEN banks-corr.corr-acct ELSE "")).
	
	
	polRekv2 = GetXattrValueEx("loan",bloans.contract + "," + bloans.cont-code,"получательрекв2","").
	IF NUM-ENTRIES(polRekv2, '^') > 3 THEN DO:
         curr-premium-account  = ENTRY( 2, polRekv2, '^').
         curr-payments-account = ENTRY( 1, polRekv2, '^').
         insurance-premium-summa = DEC( ENTRY( 4, polRekv2, '^')).
         other-payments-summa = DEC( ENTRY( 3, polRekv2, '^')).
    end.
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("curr-premium-account", curr-premium-account).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("curr-payments-account", curr-payments-account).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("insurance-premium-summa", XmlDec( DEC(insurance-premium-summa))).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("other-payments-summa", XmlDec( DEC(other-payments-summa))).
	
	
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-name", (IF AVAIL banks THEN BankNameCity(BUFFER Banks) ELSE "")).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-bik",        ENTRY( 1, polRekv, '^')).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-phone", "").

	lOK = hSAXWriter:END-ELEMENT("Contract").
    END.
    /* договор купли продажи */
	lOK = hSAXWriter:START-ELEMENT("Contract").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contract-type", "КПР").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("doc-ref", GetXAttrValueEx( "loan", vSurr, "rko2_N_DKP", "")).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("open-date", XmlDate( Date(GetXAttrValueEx( "loan", vSurr, "rko3_DATE_DKP", "")))).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("end-date", XmlDate(  Date(GetXAttrValueEx( "loan", vSurr, "rko3_DATE_DKP", "")))).

	lOK = hSAXWriter:START-ELEMENT("contract-summa").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Общая").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(GetXAttrValueEx( "loan", vSurr, "rko11_PRICE", "")))).
	lOK = hSAXWriter:END-ELEMENT("contract-summa").
	lOK = hSAXWriter:START-ELEMENT("contract-summa").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Общая_НДС").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(GetXAttrValueEx( "loan", vSurr, "nds", "")))).
	lOK = hSAXWriter:END-ELEMENT("contract-summa").
	
/*	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contract-summa", XmlDec( DEC(GetXAttrValueEx( "loan", vSurr, "rko11_PRICE", "")))).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contract-nds", XmlDec( DEC(GetXAttrValueEx( "loan", vSurr, "nds", "")))).*/

	lOK = hSAXWriter:START-ELEMENT("contract-summa").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Общая").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(GetXAttrValueEx( "loan", vSurr, "rko11_PRICE", "")))).
	lOK = hSAXWriter:END-ELEMENT("contract-summa").
	lOK = hSAXWriter:START-ELEMENT("contract-summa").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("sum-type", "Общая_НДС").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("summa", XmlDec( DEC(GetXAttrValueEx( "loan", vSurr, "nds", "")))).
	lOK = hSAXWriter:END-ELEMENT("contract-summa").

	polRekv = GetXattrValueEx("loan", vSurr, "получательрекв","").
	IF NUM-ENTRIES(polRekv, '^') < 5
	 THEN UNDO, THROW  NEW Progress.Lang.AppError( "нет реквизитов получателя на кредитном договоре.").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-name", ENTRY( 5, polRekv, '^')).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-inn",  ENTRY( 3, polRekv, '^')).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-current-account", ENTRY( 2, polRekv, '^')).
	FIND FIRST banks-code WHERE banks-code.bank-code-type = 'мфо-9'
           AND banks-code.bank-code = ENTRY( 1, polRekv, '^') NO-LOCK NO-ERROR.
        IF AVAIL banks-code
         THEN FIND FIRST banks of banks-code. ELSE RELEASE banks.
        IF AVAIL banks
    	 THEN FIND FIRST banks-corr WHERE banks-corr.bank-corr EQ banks.bank-id
             AND CAN-FIND (banks OF banks-corr WHERE banks.flag-rkc) NO-LOCK NO-ERROR.
         ELSE RELEASE banks.
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-corr-account", (IF AVAIL banks-corr THEN banks-corr.corr-acct ELSE "")).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-name", (IF AVAIL banks THEN BankNameCity(BUFFER Banks) ELSE "")).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-bank-bik",        ENTRY( 1, polRekv, '^')).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("contractor-phone", "").
	lOK = hSAXWriter:END-ELEMENT("Contract").

       /*---------------график погашения---------------*/
       {empty ttGraph}
       FOR EACH term-obl
          WHERE term-obl.contract EQ bloan.contract
          AND term-obl.cont-code EQ bloan.cont-code
          AND ((term-obl.idnt EQ 1) OR (term-obl.idnt EQ 2) OR (term-obl.idnt EQ 3))
          AND term-obl.end-date GE bloan.open-date NO-LOCK ON ERROR UNDO, THROW:
          FIND FIRST ttGraph
             WHERE ttGraph.p-date EQ term-obl.end-date NO-ERROR.
          IF NOT AVAIL ttGraph THEN 
          DO:
             CREATE ttGraph.
             ASSIGN
                ttGraph.p-date = term-obl.end-date
                ttGraph.p-percent = 0
                 ttGraph.p-rest    = 0.
          END.
          CASE term-obl.idnt:
             WHEN 1 THEN ttGraph.p-percent = term-obl.amt-rub.
             WHEN 2 THEN ttGraph.p-rest    = term-obl.amt-rub.
             WHEN 3 THEN ttGraph.p-dolg    = term-obl.amt-rub.
          END CASE.
       END.
       lOK = hSAXWriter:START-ELEMENT("Terms").
       FOR EACH ttGraph NO-LOCK BY ttGraph.p-date ON ERROR UNDO, THROW:
          lOK = hSAXWriter:START-ELEMENT("Term").
          lOK = hSAXWriter:WRITE-DATA-ELEMENT("date", XmlDate(ttGraph.p-date)).
          lOK = hSAXWriter:WRITE-DATA-ELEMENT("percent", XmlDec(ttGraph.p-percent)).
          lOK = hSAXWriter:WRITE-DATA-ELEMENT("rest", XmlDec(ttGraph.p-rest)).
          IF ttGraph.p-dolg EQ ?
             THEN UNDO, THROW  NEW Progress.Lang.AppError( "ошибка в графике, отсутствует сумма осн.долга").
          lOK = hSAXWriter:WRITE-DATA-ELEMENT("dolg", XmlDec(ttGraph.p-dolg)).
          lOK = hSAXWriter:END-ELEMENT("Term").
       END.
       lOK = hSAXWriter:END-ELEMENT("Terms").

    /*------------------ счета --------------------*/
    DEF VAR vvCurAcct AS CHAR NO-UNDO.
    vvCurAcct = GetXAttrValueEx( "loan", bloan.contract + "," + bloan.cont-code, "PLDealCurAcct",?).
    lOK = hSAXWriter:START-ELEMENT("Accounts").
    FOR EACH loan-acct
     WHERE loan-acct.contract EQ bloan.contract
       AND loan-acct.cont-code EQ bloan.cont-code
       AND loan-acct.since EQ bloan.open-date
      NO-LOCK ON ERROR UNDO, THROW:
	lOK = hSAXWriter:START-ELEMENT("Account").
	DEF VAR vrole AS CHAR NO-UNDO.
	vrole = loan-acct.acct-type.
	 IF loan-acct.acct-type EQ "КредКОМВыд" THEN vrole = "КредКомВыд".
	find first acct where acct.acct EQ loan-acct.acct NO-LOCK.
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("role", vrole).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("acct", trim( delfilfromloan(loan-acct.acct))).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("dogno", GetXAttrValueEx( "acct", acct.acct + "," + acct.currency, "доготкрлс",acct.number)).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("open-date", XmlDate( acct.open-date)).
	lOK = hSAXWriter:END-ELEMENT("Account").
	IF loan-acct.acct-type EQ "КредРасч" THEN vvCurAcct = ?.
    END.
    IF vvCurAcct Ne ? THEN DO ON ERROR UNDO, THROW:
	lOK = hSAXWriter:START-ELEMENT("Account").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("role", "КредРасч").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("acct", trim( delfilfromloan(vvCurAcct))).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("dogno", trim( delfilfromloan(vvCurAcct))).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("open-date", XmlDate( bloan.open-date)).
	lOK = hSAXWriter:END-ELEMENT("Account").
    END.
    lOK = hSAXWriter:END-ELEMENT("Accounts").

    lOK = hSAXWriter:START-ELEMENT("Rates").
    FOR EACH comm-rate
     WHERE comm-rate.kau      EQ bloan.contract + "," + bloan.cont-code
       AND comm-rate.currency EQ bloan.currency
       AND comm-rate.acct     EQ "0"
       AND comm-rate.since    EQ bloan.open-date
       AND comm-rate.commission NE "%крпр"
       AND comm-rate.commission NE "пеняптс"
       AND comm-rate.commission NE "пенякаско"
      NO-LOCK ON ERROR UNDO, THROW:
	lOK = hSAXWriter:START-ELEMENT("Rate").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("comm-rate", comm-rate.commission).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("rate-fixed", IF comm-rate.rate-fixed THEN "true" ELSE "false").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("rate", XmlDec(comm-rate.rate-comm)).
	lOK = hSAXWriter:END-ELEMENT("Rate").
    END.
    lOK = hSAXWriter:END-ELEMENT("Rates").

    /*-------------- обеспечение ----------------*/
    FOR EACH term-obl
     WHERE term-obl.contract EQ bloan.contract 
       AND term-obl.cont-code EQ bloan.cont-code
       AND term-obl.idnt      EQ 5
      NO-LOCK ON ERROR UNDO, THROW:
        DEF VAR obl-type AS CHAR NO-UNDO.
	DEF VAR vSurrObesp AS CHAR NO-UNDO.
	vSurrObesp = GetSurrogate("term-obl",ROWID(term-obl)).
        obl-type = GetXAttrValueEx( "term-obl", vSurrObesp, "ВидОб", "").
        IF obl-type EQ "автомобиль" THEN DO:
	    lOK = hSAXWriter:START-ELEMENT("Car").
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("pledge-price-avto",	XmlDec( term-obl.amt-rub) ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("brand",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCbrand", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("model",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCmodel", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("type",		GetXAttrValueEx( "term-obl", vSurrObesp, "TCtype", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("category",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCclass", "") ).
	    /*lOK = hSAXWriter:WRITE-DATA-ELEMENT("create-year",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCyear", "") ).*/
	    lOK = XmlWrite( hSAXWriter, "create-year",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCyear", ?) ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("VIN",		GetXAttrValueEx( "term-obl", vSurrObesp, "TCVIN", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("engine-number",GetXAttrValueEx( "term-obl", vSurrObesp, "TCmotor", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("chassis-number",GetXAttrValueEx( "term-obl", vSurrObesp, "TCchassis-number", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("body-number",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCbody", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("pts-series",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCSER", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("pts-num",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCNUMB", "") ).
	    DEF VAR obl-dt AS DATE NO-UNDO.
	    obl-dt = DATE(GetXAttrValueEx( "term-obl", vSurrObesp, "TCDATE", "")) NO-ERROR.
	    /*IF obl-dt NE ? THEN*/
		lOK = XmlWrite( hSAXWriter, "pts-date",	XmlDate( obl-dt) ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("color",	GetXAttrValueEx( "term-obl", vSurrObesp, "TCVIN", "") ).
	    lOK = hSAXWriter:WRITE-DATA-ELEMENT("is-new", IF LOGICAL(GetXAttrValueEx( "term-obl", vSurrObesp, "TCis-new", ""),"Да/Нет") THEN "true" ELSE "false" ).
	    lOK = hSAXWriter:END-ELEMENT("Car").
        END. ELSE 
	    UNDO, THROW  NEW Progress.Lang.AppError( "не корректный тип обеспечения (" + obl-type + ").").
    END.


    lOK = hSAXWriter:END-ELEMENT("Loan").
    END.
    lOK = hSAXWriter:END-DOCUMENT( ).

    COPY-LOB FROM lmsg TO FILE "./arx/xml/" + TRIM(claim-id) + "-lmsg1.xml" NO-CONVERT.
    
    /* сверка формата XML */
    DEF VAR lc2 AS longchar NO-UNDO.
    output stream vvs to value ("ipl4.xsd_")
    UNBUFFERED /* CONVERT  TARGET "1251"  SOURCE "IBM866"*/.
    FIND FIRST wapp.message_types WHERE wapp.message_types.id EQ 2 NO-LOCK.
    lc2 = wapp.message_types.scheme.
    put stream vvs unformatted string(lc2) SKIP.
    output stream vvs close.
    DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
    CREATE X-DOCUMENT hDoc.

    DO ON ERROR UNDO, THROW:
	    /*hDoc:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/ContractDataResponseV4", "ipl2_V4.xsd").*/
/*       hDoc:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/ContractDataResponseV5", "ipl2_V5.xsd"). */
hDoc:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/ContractDataResponseV8", "ipl2_V8.xsd").
hDoc:STRICT-ENTITY-RESOLUTION = false.
       lReturn = hDoc:LOAD("longchar", lmsg, TRUE) NO-ERROR.
       COPY-LOB FROM lmsg TO FILE "./arx/xml/" + TRIM(claim-id) + "-lmsg2.xml" NO-CONVERT.
       IF NOT lReturn THEN
	       UNDO, THROW NEW Progress.Lang.AppError( "ошибка валидации сформированного ответа:" + RetValErr()).

	    FINALLY:
	       DELETE OBJECT hDoc.
	    END.
    END.
    
    newID = NEXT-VALUE( SEQ_MESSAGES, wapp).
    CREATE bAnswer.
    ASSIGN
      bAnswer.id = newID
      bAnswer.typeid = 2
      bAnswer.senderid = 2
      bAnswer.recipientid = 1
      bAnswer.createtime = DATETIME-TZ(NOW, 0)
      bAnswer.replyto = bwapp.id
      bAnswer.conversationid = bwapp.conversationid
      bAnswer.body = lmsg.
      
    VALIDATE bAnswer.
    
    CATCH eAnyError2 AS Progress.Lang.AppError:
   	 DEF VAR strr5 AS CHAR NO-UNDO.
   	 strr5 = "(" + eAnyError2:ReturnValue + ")".
   	 DO i = 1 TO eAnyError2:NumMessages:
   	    strr5 = strr5 + (IF i > 1 THEN ", " ELSE "") + eAnyError2:GetMessage(i).
   	 END.
   	 RUN XmlMsgAnswer(
   	   BUFFER wapp.messages,
   	   claim-id,
   	   task-id,
   	   "-2",
   	   "ошибка договора:" + strr5).
   	 PUT UNFORMATTED 
   	    "MsgID=" + string(wapp.messages.id) + "^ Error=" + strr5 SKIP.
    END CATCH.

    
    CATCH eAnyError AS Progress.Lang.Error:
   	 DEF VAR strr AS CHAR NO-UNDO.
   	 strr = RETURN-VALUE.
   	 DO i = 1 TO eAnyError:NumMessages:
       	    strr = strr + (IF i > 1 THEN ", " ELSE ": ") + eAnyError:GetMessage(i).
   	 END.
   	 PUT UNFORMATTED 
   	    "ID=" + string(wapp.messages.id) + " Error:" + strr SKIP.
   	 RUN XmlMsgAnswer( BUFFER wapp.messages,
   	   claim-id,
   	   task-id,
   	   "-2", "ошибка договора:" + strr).
    END CATCH.
END.
