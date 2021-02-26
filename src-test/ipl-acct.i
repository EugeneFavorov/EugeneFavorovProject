/* accttmpl.lib accttmpl.p */

DEF VAR vOpKindAcct AS CHAR NO-UNDO.

vOpKindAcct = GetXattrEx(loan.class-code,"Op-kind_Acct","Initial").
PUT UNFORMATTED "транзакция " + vOpKindAcct SKIP.

FIND op-kind WHERE op-kind.op-kind EQ vOpKindAcct
                          NO-LOCK NO-ERROR.
IF NOT AVAIL op-kind
 THEN UNDO, THROW NEW Progress.Lang.AppError( "не найдена транзакция по открытию счетов.").

DEF VAR lst-templ AS CHAR NO-UNDO.
DEF VAR mDateSogl AS CHAR NO-UNDO.
DEF VAR mOpenDate AS DATE NO-UNDO.
DEF VAR vReservAcct AS CHAR.

lst-templ = list-op-templ(op-kind.op-kind,"acct").
mDateSogl = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "ДатаСогл").

DEFINE VARIABLE DTCust AS CHARACTER  NO-UNDO.
/* определяем субъекта по резидентности клиента + типу клиента из заявки,
   а не по др 'субъект' */
DTCust = "ФЛ".
/*FOR EACH ttPERSON
 WHERE ttPERSON.request-id = ttREQUEST.request-id
   AND ttPERSON.role EQ "заемщик"  NO-LOCK:
 DTCust = CAPS( ttPERSON.person-status).
 LEAVE.
END.
*/

SHABL_CYCLE:
FOR EACH op-template OF op-kind
 WHERE CAN-DO(lst-templ,STRING(op-template.op-template))
 NO-LOCK ON ERROR UNDO, THROW:
    vReservAcct = ?.
    mOpenDate = IF    GetXattrValue("op-template",
                                      op-template.op-kind + "," + 
                                      STRING(op-template.op-template),
                                      "ДатаОткрСчет") EQ "Дата заключения"
                    AND mDateSogl NE "" 
                     THEN DATE( mDateSogl)
                     ELSE loan.open-date.

    DEF VAR mBlock AS CHAR NO-UNDO.
    CASE loan.cust-cat :
      WHEN 'Ч' THEN
         mBlock = GetXAttrValue ('person', string(loan.cust-id),'Блок').
      WHEN 'Ю' THEN
         mBlock = GetXAttrValue ('cust-corp', string(loan.cust-id),'Блок') .
      WHEN 'В' THEN
         mBlock = GetXAttrValue ('banks', string(loan.cust-id),'Блок') .
    END CASE.
    IF mBlock = "Да"
     THEN DO:
	UNDO, THROW NEW Progress.Lang.AppError( "клиент заблокирован. счет автоматически не создастся.").
    END.

    /*Возможные параметры для создания счета*/
    DEFINE VAR tv-op-kind          AS CHARACTER NO-UNDO.
    DEFINE VAR tv-op-template      AS INT64   NO-UNDO.

    DEFINE VAR tv-Bal-Acct         AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Acct-cat         AS CHARACTER
                                   INITIAL "b"  NO-UNDO.
    DEFINE VAR tv-Acct-Name        AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Acct-Mask        AS CHARACTER NO-UNDO.
    DEFINE VAR tv-NoCreateAcct     AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Contract         AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Acct             AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Currency         AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Details          AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Cust-cat         AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Cust-id-ch       AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Cust-id          AS INT64   NO-UNDO INITIAL ?.
    DEFINE VAR tv-Acct-type        AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Acct-code        AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Class-code       AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Create-Find      AS CHARACTER NO-UNDO.
    DEFINE VAR tv-Cr-Class-Code    AS CHARACTER NO-UNDO.
    DEFINE VAR tv-kau-id           AS CHARACTER NO-UNDO INITIAL "*".
    DEFINE VAR tv-otdel            AS CHARACTER NO-UNDO.

    ASSIGN
      tv-op-kind       = op-template.op-kind
      tv-op-template   = op-template.op-template
      tv-Bal-Acct      = Get_Param("bal-acct",RECID(op-template))
      tv-Acct-cat      = Get_Param("acct-cat",RECID(op-template))
      tv-Acct          = Get_Param("l-acct",RECID(op-template))
      tv-Acct-Mask     = Get_Param("open_mask",RECID(op-template))
      tv-NoCreateAcct  = Get_Param("NoCreateAcctDef",RECID(op-template))
      tv-Contract      = Get_Param("contract",RECID(op-template))
      tv-Currency      = Get_Param("l-currency",RECID(op-template))
      tv-Cust-cat      = Get_Param("l-cust-cat",RECID(op-template))
      tv-Cust-id-ch    = Get_Param("l-cust-id",RECID(op-template))
      tv-Acct-type     = Get_Param("acct-type",RECID(op-template))
      tv-Create-Find   = Get_Param("flag-create",RECID(op-template))
      tv-Details       = op-template.details
      tv-Cr-Class-Code = op-template.cr-class-code
      tv-kau-id        = Get_Param("kau-id",RECID(op-template))
      tv-otdel         = loan.branch-id /*GetXAttrValue("op-template", buf-optemplate.op-kind + ","
                                     + STRING(buf-optemplate.op-template), "branch-id" )*/
   .
   IF tv-Acct-type EQ "КредКОМВыд" THEN tv-Acct-type = "КредКомВыд".
   IF tv-kau-id = "" OR
      tv-kau-id = ?
   THEN
      tv-kau-id = "*".
    IF tv-Currency EQ ? OR tv-Currency EQ "" THEN
      tv-Currency = loan.currency.
    IF tv-Currency EQ "810" THEN tv-Currency = "".
    IF tv-Acct = "" OR tv-Acct = ? THEN tv-Acct = "*".

    ASSIGN
        tv-Cust-cat = loan.cust-cat
        tv-Cust-id  = loan.cust-id
        tv-Details  = ""
        .
    /*IF tv-Details NE "" AND tv-Details NE ? THEN
       RUN prsstr.p("nameaclb",INPUT-OUTPUT tv-Details).*/

    /* Search-Acct */
    IF tv-Create-Find = "Искать" THEN
    DO:
	DEFINE VARIABLE vFile  AS CHARACTER  NO-UNDO.
	DEFINE VARIABLE l-sk   AS INT NO-UNDO.
	DEFINE VARIABLE r-sk   AS INT NO-UNDO.
	DEFINE VARIABLE name-proc  AS CHARACTER  NO-UNDO.
	DEFINE VARIABLE param-proc AS CHARACTER  NO-UNDO.

	/*vFile = ENTRY(INDEX("ЧЮБ",tv-cust-cat),"person,cust-corp,banks").*/

	ASSIGN
	  l-sk = INDEX(tv-Acct,"(")
	  r-sk = R-INDEX(tv-Acct,")")
	.
	IF (l-sk NE 0 AND r-sk NE 0)
	 THEN DO:
	    ASSIGN
		name-proc  = TRIM(SUBSTRING(tv-Acct,1,l-sk - 1))
		param-proc = TRIM(SUBSTRING(tv-Acct,l-sk + 1,r-sk - l-sk - 1))
		.
	    IF name-proc EQ 'PLAcct' AND param-proc EQ 'PLAcct' THEN DO:
		DEF VAR plfind AS LOG NO-UNDO.
		plfind = False.
	        FOR EACH code
	         WHERE code.class EQ 'PLAcct446'
	           AND code.code BEGINS tv-Acct-type + CHR(1) + 'Кредит' NO-LOCK:
	            IF CAN-DO(ENTRY(3, code.code, CHR(1)), DTCust) AND
	              CAN-DO(ENTRY(4, code.code, CHR(1)), 'Обычный') AND
	              CAN-DO(ENTRY(5, code.code, CHR(1)), 'руб') THEN DO:
	    	    tv-Acct = code.val.
	    	    plfind = True.
    		    PUT UNFORMATTED 'маска счета ' code.val SKIP.
	        END.
	        IF NOT plfind
		 THEN UNDO, THROW NEW Progress.Lang.AppError( 'роль ' + tv-Acct-type + ' по шаблону транзакции ' + vOpKindAcct + ' счет не найден в классификаторе PLAcct').
	    END.
	    END.
	END.
	IF tv-Bal-Acct <> ?  THEN DO:
          FIND FIRST bal-acct WHERE
                   bal-acct.bal-acct = INT64(tv-Bal-acct)
                 NO-LOCK NO-ERROR.
    	    IF AVAIL bal-acct AND 
    	       bal-acct.cust-cat EQ "в" THEN
    	       ASSIGN tv-Cust-cat = bal-acct.cust-cat tv-Cust-id = ?.
        END.
/*           AND (tv-Contract    = ""    OR        ~
                acct.contract  = tv-Contract     ~
               )                                 ~
*/

	&SCOPED-DEFINE comm_acct_where           ~
           AND acct.currency   = tv-Currency     ~
           AND acct.close-date = ?               ~
           AND acct.open-date <= loan.open-date  ~
                 AND CAN-DO(tv-Acct, acct.number)

	&SCOPED-DEFINE kau-id_where              ~
           AND (   tv-kau-id = "*"               ~
                OR (AVAILABLE bal-acct AND       ~
                    bal-acct.kau-id = tv-kau-id  ~
                                              )  ~
                OR acct.kau-id = tv-kau-id       ~
               )

	IF tv-Bal-Acct <> ? THEN DO:
	    IF tv-Cust-cat = "в" THEN
    	    FIND FIRST acct WHERE
    	      acct.filial-id = shFilial AND
              acct.bal-acct = INT(tv-Bal-Acct) AND
    	      acct.cust-cat EQ tv-Cust-cat
    	      {&comm_acct_where}
    	      /*{&kau-id_where}*/
    	    NO-LOCK NO-ERROR.
    	    ELSE
    	    FIND FIRST acct WHERE
    	      acct.filial-id = shFilial AND
              acct.bal-acct = INT(tv-Bal-Acct) AND
    	      acct.cust-cat EQ tv-Cust-cat AND
    	      acct.cust-id EQ tv-Cust-id
    	      {&comm_acct_where}
    	      {&kau-id_where}
    	    NO-LOCK NO-ERROR.
    	END. ELSE DO:
    	    FIND FIRST acct WHERE YES
    	      AND acct.filial-id = shFilial
    	      {&comm_acct_where}
    	    NO-LOCK NO-ERROR.
    	END.
        PUT UNFORMATTED tv-Contract ";" tv-Currency ";" tv-Cust-cat ";" tv-Bal-Acct ";" tv-Cust-cat ";" tv-Acct ";" AVAIL(acct) ";" shFilial SKIP.
        IF NOT AVAIL acct THEN DO:
    	    IF tv-NoCreateAcct EQ 'да'
	     THEN UNDO, THROW NEW Progress.Lang.AppError( 'роль ' + tv-Acct-type + ' по шаблону транзакции ' + vOpKindAcct + ' счет не найден').
	    tv-Create-Find = "Создавать".
    	    PUT UNFORMATTED 'счет ' + tv-Acct-type + ' не найден, будем создавать' SKIP.
	END. ELSE
    	    PUT UNFORMATTED 'счет ' + tv-Acct-type + ' найден ' acct.number SKIP.
    END.



    IF tv-Create-Find = "Создавать" THEN DO:
	FIND FIRST bal-acct WHERE
              bal-acct.bal-acct = INT64(tv-Bal-Acct)
	NO-LOCK NO-ERROR.
	IF NOT AVAILABLE bal-acct
	 THEN UNDO, THROW NEW Progress.Lang.AppError(
		'не найден балансовый счет ' + STRING(tv-Bal-Acct) +
		' по шаблону транзакции ' + vOpKindAcct +
		' для роли ' + tv-Acct-type + '.').

	IF NOT CAN-DO(GetXclassAllparentsEx(tv-Cr-Class-Code),"acct" +
	    bal-acct.acct-cat)
	 THEN UNDO, THROW NEW Progress.Lang.AppError( 
	    'балансовый счет ' + STRING(tv-Bal-Acct) + 
	    ' по шаблону транзакции ' + vOpKindAcct + 
	    ' для роли ' + tv-Acct-type + 'Класс счета не соответствует категории').
	IF NOT CAN-DO( GetXAttrValueEx ("bal-acct",
                                     STRING( bal-acct.bal-acct),
                                     "субъект",
                                     "-"),DTCust)
	 THEN UNDO, THROW NEW Progress.Lang.AppError( 
	    'тип субъекта не соответствует балансовому счету ' + STRING(tv-Bal-Acct) + 
	    ' по шаблону транзакции ' + vOpKindAcct + 
	    ' для роли ' + tv-Acct-type).
	/* Если бал.счет не стоит на субаналитике,
	 ** но в шаблоне счета указан корректный КАУ,
	 ** то ставим на субаналитику лицевой счет.
	 */
	IF tv-kau-id <> "*" AND
	   tv-kau-id <> bal-acct.kau-id THEN DO:
	    IF GetCode("ШаблКау", tv-kau-id) = ? 
	     THEN UNDO, THROW NEW Progress.Lang.AppError( 
    	    	"Шаблон КАУ с кодом " + tv-kau-id +
                " не найден в классификаторе 'ШаблКАУ'!").
	END.

	/* Добавляем маски тэгов для генерации номеров счетов *
	IF INDEX(tv-Acct-Mask, "n") > 0 THEN DO:
	    * Номер договора берется из ДР "AgrCounter" на договоре *
	    vAgrCounter = GetXAttrValueEx ("loan",
                                     loan.contract + "," + loan.cont-code,
                                     "AgrCounter",
                                     ?).
	    * Если ДР "AgrCounter" не заполнен, и это транш,
	     ** то берем ДР с охватывающего договора *
	    IF     vAgrCounter EQ ?
	     AND NUM-ENTRIES(loan.cont-code, " ") EQ 2 THEN
		vAgrCounter = GetXAttrValueEx ("loan",
                                        loan.contract + "," + ENTRY(1, loan.cont-code, " "),
                                        "AgrCounter",
                                        ?).
	    IF vAgrCounter EQ ?
	     THEN UNDO, THROW NEW Progress.Lang.AppError( 
    		"Ошибка при открытии счета. " +
                "В шаблоне счета есть литеры <n>, " +
                "но допреквизит <AgrCounter> не заполнен").
	    * Подстановка номера в шаблон *
	    RUN PatternSubst ("n",
                        STRING(vAgrCounter),
                        INPUT-OUTPUT tv-Acct-Mask).
	END.*/

        /* подстановка даты  *
	IF INDEX(tv-Acct-Mask, "g") GT 0 THEN DO:
	    * если транш *
          IF     NUM-ENTRIES(loan.cont-code, " ")             EQ 2 
             AND GetXAttrInit(loan.class-code, "МаскСчетГод") EQ "Да" THEN
          DO: 
             FIND FIRST mbloan WHERE
                        mbloan.contract  EQ loan.contract
                    AND mbloan.cont-code EQ ENTRY(1, loan.cont-code, " ")
             NO-LOCK NO-ERROR.
             IF AVAILABLE mbloan THEN
             DO:
                RUN PatternSubst ("g",
                                  STRING(YEAR(DATE(GetXAttrValueEx ("loan",
                                                                    mbloan.contract + "," + mbloan.cont-code,
                                                                    "ДатаСогл",
                                                                    ?)))),
                                  INPUT-OUTPUT tv-Acct-Mask).
             END.
          END.
          ELSE RUN PatternSubst ("g",
                                 STRING(YEAR(loan.open-date)),
                                 INPUT-OUTPUT tv-Acct-Mask).
	END.*/
    
	RELEASE acct NO-ERROR.
	DEF VAR myAcct AS CHAR NO-UNDO.
	DEF VAR myErrMsg AS CHAR NO-UNDO.
	DEF VAR myOk AS LOG NO-UNDO.
	
	myAcct = "".
	
	IF CAN-DO( "40817,40820", tv-Bal-Acct) THEN DO ON ERROR UNDO, THROW:
	    IF (vOldReservedCurAcct NE ?)
	     AND (SUBSTR( vOldReservedCurAcct, 1, 5) EQ tv-Bal-Acct)
	     AND (SUBSTR( vOldReservedCurAcct, 10, 4) EQ loan.branch-id)
	      THEN DO ON ERROR UNDO, THROW:
		myAcct = vOldReservedCurAcct.
	    END.
	 IF (vOldReservedCurAcct2 NE ?)
	    /* AND (SUBSTR( vOldReservedCurAcct2, 1, 5) EQ tv-Bal-Acct)
	     AND (SUBSTR( vOldReservedCurAcct2, 10, 4) EQ loan.branch-id) */
	      THEN DO ON ERROR UNDO, THROW:
		myAcct = vOldReservedCurAcct2.
	    END.
	  IF myAcct EQ "" THEN DO ON ERROR UNDO, THROW:
	  PUT UNFORMATTED 'генерим номер счета класс- ' + STRING(tv-Cr-Class-Code) + ' счет 2 порядка- ' + STRING(tv-Bal-Acct) 
                 + ' маска= ' + STRING(tv-Acct-Mask) SKIP.
		RUN CreateAcctNumber IN h_acct (
		    tv-Cr-Class-Code,
		    INT64(tv-Bal-Acct),
		    tv-Currency,
		    GetXAttrInit(tv-Cr-Class-Code, "acct-cat"),
		    tv-Acct-Mask,
		    loan.branch-id,
		    tv-Cust-cat,
		    tv-Cust-id,
		    "",
		    ?,
		    OUTPUT myAcct,
		    OUTPUT myErrMsg) NO-ERROR.
	  PUT UNFORMATTED 'что то сгенерили: ' + STRING(myAcct)  + " ERROR-STATUS:ERROR = " + STRING(ERROR-STATUS:ERROR) SKIP.

		IF ERROR-STATUS:ERROR 
		 THEN UNDO, THROW NEW Progress.Lang.AppError( RETURN-VALUE).
		IF GetCodeEx("СчетаРезерва",myAcct,?) <> ?
		 THEN UNDO, THROW NEW Progress.Lang.AppError( "выбранный номер счета " + myAcct + " уже был зарезервирован ранее.").
		IF GetCodeEx("СчетаРезерва",AddFilToAcct(myAcct,shFilial),?) <> ?
		 THEN UNDO, THROW NEW Progress.Lang.AppError( "выбранный номер счета " + myAcct + " уже был зарезервирован ранее.").
	    END.
	  PUT UNFORMATTED 'сгенерили номер счета ' +  myAcct SKIP.
	    
	    RUN MyAcctKeep ( myAcct,STRING(ttREQUEST.claim-id), OUTPUT myOk).
	    IF ERROR-STATUS:ERROR 
	     THEN UNDO, THROW NEW Progress.Lang.AppError( RETURN-VALUE).
	    IF myOk NE YES
	     THEN UNDO, THROW NEW Progress.Lang.AppError( "ошибка резервирования счета.").
	    RUN MyAcctKeep ( AddFilToAcct( myAcct, shFilial),STRING(ttREQUEST.claim-id), OUTPUT myOk).
	    IF ERROR-STATUS:ERROR 
	     THEN UNDO, THROW NEW Progress.Lang.AppError( RETURN-VALUE).
	    IF myOk NE YES
	     THEN UNDO, THROW NEW Progress.Lang.AppError( "ошибка резервирования счета.").
	    vReservAcct = myAcct.
	END. ELSE DO ON ERROR UNDO, THROW:
        PUT UNFORMATTED 'создаем счет ' +  myAcct SKIP.
	RUN Cm_acct_cr IN h_acct (
          tv-Cr-Class-Code,      /* iClass                  */
          INT64(tv-Bal-Acct),    /* iBal                    */
          tv-Currency,           /* iCurr                   */
          tv-Cust-cat,           /* iCustCat                */
          tv-Cust-id,            /* iCustID                 */
          loan.open-date,        /* iOpenDate               */
          OUTPUT myAcct,         /* oAcct                   */
          BUFFER acct,           /* BUFFER iacct FOR acct . */
          tv-Acct-Mask,          /* iAcctMask               */
          tv-Details,            /* iDetails                */
          tv-kau-id,             /* iKauId                  */
          tv-Contract,           /* iContract               */
          loan.user-id,          /* iUserId                 */
          loan.branch-id,        /* iBranchId               */
          YES                    /* iCopyBalXattr           */
	 ) NO-ERROR.

	IF ERROR-STATUS:ERROR 
	 THEN UNDO, THROW NEW Progress.Lang.AppError( RETURN-VALUE).
	UpdateSigns( tv-Cr-Class-Code, GetSurrogate( "acct", ROWID(acct)), "PLDealID", ttREQUEST.claim-id, ?).
	UpdateSigns( tv-Cr-Class-Code, GetSurrogate( "acct", ROWID(acct)), "ДогОткрЛС", STRING(loan.open-date, "99/99/9999") + "," + loan.doc-ref, ?).

    	PUT UNFORMATTED 'роль ' + tv-Acct-type + ' счет '  + acct.number + ' создан.' SKIP.
	/* Commented by KSV: Ищем буфер созданного счета, т.к. вызывающая процедура
	 ** ожидает его */
	FIND FIRST acct WHERE
    	  acct.filial-id = shFilial AND
          acct.Acct      = myAcct AND
          acct.Currency  = tv-currency NO-LOCK NO-ERROR.

	/* Инициализация доп.реквизитов со счета 2-го порядка и из классификатора
	 ** МаскиНаслед */
	RUN BalToAcct_Xattr(RECID(acct),"*",YES,YES).
	IF RETURN-VALUE EQ "ERROR"
	 THEN UNDO, THROW NEW Progress.Lang.AppError(
		"Ошибка при инициализация доп.реквизитов со счета 2-го порядка " +
		"и из классификатора  [МаскиНаслед]  ").
	END.
    END.
    IF vReservAcct NE ? THEN DO ON ERROR UNDO, THROW:
	UpdateSigns( loan.class-code, GetSurrogate( "loan", ROWID(loan)), "PLDealCurAcct", myAcct, ?).
	PUT UNFORMATTED 'роль ' + tv-Acct-type + ' счет ' + myAcct + ' зарезервирован.' SKIP.
    END. ELSE DO ON ERROR UNDO, THROW:
	IF NOT AVAIL acct
	 THEN UNDO, THROW NEW Progress.Lang.AppError( 'роль ' + tv-Acct-type + ' по шаблону транзакции ' + vOpKindAcct + ' счет не найден и не создан').

	/* счет не ставим на аналитику, там всякие выводы на экран
	RUN SetKau IN h_loanx (tt-editacct.rid, 
                                RECID(loan),
                                tv-Acct-type).*/
	CREATE loan-acct.
         loan-acct.cont-code = loan.cont-code.
	ASSIGN
	    loan-acct.contract = loan.contract
	    loan-acct.acct = acct.acct
	    loan-acct.currency = acct.currency
	    loan-acct.acct-type = tv-Acct-type
	    loan-acct.since = loan.open-date
	    .
	/* если счет открывали по данной заявке, то дата открытия счета должна быть равна даты открытия договора */
	DEF BUFFER bl-acct FOR loan-acct.
	IF GetXattrValue( "acct", loan-acct.acct + "," + loan-acct.currency, "PLDealID")
	      EQ ttREQUEST.claim-id THEN DO ON ERROR UNDO, THROW:
	    IF acct.open-date < loan-acct.since AND
	      (NOT CAN-FIND(FIRST bl-acct OF acct WHERE bl-acct.since < loan-acct.since)) AND
	       (NOT CAN-FIND(FIRST op-entry WHERE op-entry.acct-db EQ acct.acct
	          AND op-entry.currency BEGINS acct.currency AND op-entry.op-date < loan-acct.since)) AND
	       (NOT CAN-FIND(FIRST op-entry WHERE op-entry.acct-cr EQ acct.acct
	          AND op-entry.currency BEGINS acct.currency AND op-entry.op-date < loan-acct.since))
	      THEN DO ON ERROR UNDO, THROW:
	        PUT UNFORMATTED 'роль ' + tv-Acct-type + ' счет ' + acct.number + ' меняем дату открытия.' SKIP.
	        FIND CURRENT acct EXCLUSIVE-LOCK.
	        ASSIGN
	    	    acct.open-date = loan-acct.since.
	        VALIDATE acct.
	    END.
	    UpdateSigns( acct.class-code, GetSurrogate( "acct", ROWID(acct)),
		 "ДогОткрЛС", 
		 STRING( acct.open-date, "99/99/9999") + "," + acct.number /* loan.doc-ref */, ?).
	    
	    UpdateSigns( acct.class-code, GetSurrogate( "acct", ROWID(acct)),
		 "СотрОткрСч", acct.user-id, ?).
	    RELEASE acct.
	END.
    END.
END.
