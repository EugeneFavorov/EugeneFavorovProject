{globals.i}
{svarloan.def new}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get db2l}
{intrface.get terr}
{intrface.get acct}
{intrface.get jloan}
{intrface.get count}
{intrface.get pqres}
{loan.pro}
{dpsproc.def}
/* {intrface.get cust} */
{pers.fun}
{client.i}

{ipl.def}

DEFINE VARIABLE hSAXnilAttr AS HANDLE.
CREATE SAX-ATTRIBUTES hSAXnilAttr.
hSAXnilAttr:INSERT-ATTRIBUTE("xsi:nil", "true").

FUNCTION XmlDate RETURN CHAR
    ( iDate AS DATE):
RETURN	    STRING( YEAR( iDate), "9999") + "-" +
	    STRING( MONTH( iDate), "99") + "-" + 
	    STRING( DAY( iDate), "99").
END FUNCTION.

FUNCTION XmlDec RETURN CHAR
    ( iD AS DEC):
    RETURN TRIM( STRING( iD, "->>>>>>>>>>>>>>9.99<")).
END FUNCTION.

FUNCTION RetValErr RETURN CHAR:
	DEF VAR strr2 AS CHAR NO-UNDO.
	DEF VAR i AS INT NO-UNDO.
	strr2 = "(" + RETURN-VALUE + ")".
	DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
	    strr2 = strr2 + (IF i > 1 THEN ", " ELSE "") + ERROR-STATUS:GET-MESSAGE(i).
	END.
    RETURN strr2.
END FUNCTION.


FUNCTION XmlWrite RETURNS LOG
    ( INPUT hSAXWriter AS HANDLE, INPUT  VarName AS CHAR, INPUT Val AS CHAR):
    def var lReturn as log no-undo.
    lReturn = hSAXWriter:WRITE-DATA-ELEMENT( VarName, IF Val EQ ? THEN "" ELSE Val, "", IF Val EQ ? THEN hSAXnilAttr ELSE ? ).
    IF NOT lReturn THEN
	    UNDO, THROW NEW Progress.Lang.AppError( "�訡�� ������樨 �����:" + RetValErr()).
    RETURN lReturn.
    CATCH eAnyError3 AS Progress.Lang.Error:
	    DEF VAR strr4 AS CHAR NO-UNDO.
	    DEF VAR ijj AS INT NO-UNDO.
	    strr4 = "(" /*+ eAnyError3:ReturnValue RETURN-VALUE*/ + ")".
	    DO ijj = 1 TO /*ERROR-STATUS:NUM-MESSAGES*/ eAnyError3:NumMessages:
		strr4 = strr4 + (IF ijj > 1 THEN ", " ELSE "") + eAnyError3:GetMessage(ijj).
	    END.
	    UNDO, THROW eAnyError3.
    END CATCH.
END.


{ipl-exp.i}

PROCEDURE XmlMsgAnswer.
	DEF PARAM BUFFER bwapp FOR wapp.messages.
	DEF INPUT PARAM req-id AS CHAR NO-UNDO.
	DEF INPUT PARAM task-id AS CHAR NO-UNDO.
	DEF INPUT PARAM status-no  AS CHAR NO-UNDO.
	DEF INPUT PARAM status-msg AS CHAR  NO-UNDO.   /* �� ����樨 */

	DEFINE VARIABLE hSAXWriter AS HANDLE.
	DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.
	DEFINE VARIABLE lmsg AS LONGCHAR NO-UNDO.
	DEF BUFFER bAnswer FOR wapp.messages.
	DEF VAR newID AS INT64 NO-UNDO.
	
	CREATE SAX-WRITER hSAXWriter.
	hSAXWriter:FORMATTED = TRUE.
	hSAXWriter:ENCODING = "windows-1251".
	lOK = hSAXWriter:SET-OUTPUT-DESTINATION("longchar", lmsg).
	lOK = hSAXWriter:START-DOCUMENT( ).
	/*lOK = hSAXWriter:DECLARE-NAMESPACE ("http://www.creditregistry.ru/schema/import").*/
	lOK = hSAXWriter:START-ELEMENT("Answer").
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("claim-id", req-id).
	IF task-id NE ? THEN
	  lOK = hSAXWriter:WRITE-DATA-ELEMENT("task-id", task-id).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("status", status-no).
	lOK = hSAXWriter:WRITE-DATA-ELEMENT("status-msg", status-msg).
	lOK = hSAXWriter:END-ELEMENT("Answer").
	lOK = hSAXWriter:END-DOCUMENT( ).
	newID = NEXT-VALUE(SEQ_MESSAGES, wapp).
	CREATE bAnswer.
	ASSIGN
	  bAnswer.id = newID
	  bAnswer.typeid = 4
	  bAnswer.senderid = 2
	  bAnswer.recipientid = 1
	  bAnswer.createtime = DATETIME-TZ( NOW, 0)
	  bAnswer.replyto = bwapp.id
	  bAnswer.conversationid = bwapp.conversationid
	  bAnswer.body =  lmsg
	  .
	VALIDATE bAnswer.
END.

/*------------------------------------------------------------------------------
  Purpose:     �믮���� १�ࢨ஢���� ����� ��� ��� ���쭥�襣�
               �ᯮ�짮�����. ��१�ࢨ஢���� ����� ��� �� ����� ����
               �ᯮ�짮��� ����୮.
  Parameters:  iAcct - ����� ���
               oOk   - 䫠� ������: YES - ��� ��१�ࢨ஢�� �ᯥ譮
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE MyAcctKeep:
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDeal AS CHARACTER  NO-UNDO. /* ����� ��� */
   DEFINE OUTPUT PARAMETER oOk   AS LOGICAL.

   DEFINE BUFFER CODE FOR CODE.

   IF GetCodeEx("��⠐���ࢠ",iAcct,?) <> ? THEN
   DO:
      oOk = YES.
      RETURN .
   END.


   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:
      CREATE CODE.
      ASSIGN
         CODE.class  = "��⠐���ࢠ"
         CODE.parent = "��⠐���ࢠ"
         CODE.code   = iAcct
         CODE.name   = iDeal
         code.val    = STRING(TODAY,'99.99.9999')
      oOk = YES.

      RELEASE code.
   END.  /* End of TR BLOCK */

END PROCEDURE.


PROCEDURE iplDeleteLoan.
	DEF INPUT PARAM vContract AS CHAR NO-UNDO.
	DEF INPUT PARAM vCont-code  AS CHAR NO-UNDO.
	DEF INPUT PARAM vDelAcct AS LOG NO-UNDO.
	DEF VAR vPLDealID AS CHAR NO-UNDO.
	DEF BUFFER loan FOR loan.
	put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " ��稭��� 㤠�����" skip.
	/* 㤠����� ������� */
	FIND FIRST loan
	  WHERE loan.contract EQ vContract
	    AND loan.cont-code EQ vCont-code
	    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
	IF NOT AVAIL loan THEN DO ON ERROR UNDO, THROW:
	    IF LOCKED loan THEN UNDO, THROW NEW Progress.Lang.AppError(
             "������� " + vCont-code +
             " �������஢�� � �� ����� ���� 㤠���.").
            ELSE UNDO, THROW NEW Progress.Lang.AppError(
             "������� " + vCont-code +
             " �� ������.").
	END.
	DO ON ERROR UNDO,THROW:
	IF loan.loan-status NE "����"
         THEN UNDO , THROW NEW Progress.Lang.AppError(
             "������� " + loan.doc-ref + 
             " ����� ����� '" + loan.loan-status + "' � �� ����� ���� 㤠���.").
	FIND FIRST loan-int
	 WHERE loan-int.contract = loan.contract
	   and loan-int.cont-code = loan.cont-code NO-LOCK NO-ERROR.
	IF AVAIL loan-int
         THEN UNDO , THROW NEW Progress.Lang.AppError(
             "�� �������� " + loan.doc-ref + 
             " �஢����� ����樨 �� �����⨪�, ������� �� ����� ���� 㤠���.").
	vPLDealID = GetXattrValue( "loan", loan.contract + "," + loan.cont-code, "PLDealID").
	IF vPLDealID EQ ?
         THEN UNDO , THROW NEW Progress.Lang.AppError(
             "������� " + loan.doc-ref + 
             " �� ����� ����� ���, � �� ����� ���� 㤠���.").
	
	DEF BUFFER insu FOR loan.
	DEF VAR vSurr2 AS CHAR NO-UNDO.
	/* ������� ���客���� */
	put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 ���.���客����" skip.
	for each insu
	 where insu.parent-contract = loan.contract
	   and insu.parent-cont-code = loan.cont-code
	   and insu.contract = "�����" ON ERROR UNDO,THROW:
	    for each signs
	     where signs.file-name = "loan"
	       and signs.surrogate = insu.contract + "," + insu.cont-code ON ERROR UNDO,THROW:
		DELETE signs.
	    END.
	    for each cust-role
	     where cust-role.file-name = "loan"
	       and cust-role.surrogate = insu.contract + "," + insu.cont-code ON ERROR UNDO,THROW:
		DELETE cust-role.
	    END.
	    for each term-obl
	     where term-obl.idnt EQ 1
	       and term-obl.contract EQ insu.contract
	       and term-obl.cont-code EQ insu.cont-code ON ERROR UNDO,THROW:
		DELETE term-obl.
	    END.
	    DELETE insu.
	END.
	/* ������� ���ᯥ祭�� */
	put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 ���.���ᯥ祭��" skip.
	for each term-obl
	     where term-obl.idnt EQ 5
	       and term-obl.contract EQ loan.contract
	       and term-obl.cont-code EQ loan.cont-code ON ERROR UNDO,THROW:
	    vSurr2 = GetSurrogate("term-obl",ROWID(term-obl)).
	    for each signs
	     where signs.file-name = "term-obl"
	       and signs.surrogate = vSurr2 ON ERROR UNDO,THROW:
		DELETE signs.
	    END.
	    DELETE term-obl.
	END.
	/* ��� */
	put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 �ਢ離� ��⮢ �� �������" skip.
	FOR EACH loan-acct
	 where loan-acct.contract EQ loan.contract
	   and loan-acct.cont-code EQ loan.cont-code ON ERROR UNDO,THROW:
	    FIND FIRST acct OF loan-acct.
	    DELETE loan-acct.
	    IF vDelAcct AND GetXattrValue( "acct", acct.acct + "," + acct.currency, "PLDealID") 
		  EQ vPLDealID THEN DO ON ERROR UNDO, THROW:

		IF (NOT CAN-FIND(FIRST loan-acct OF acct)) AND
		   (NOT CAN-FIND(FIRST op-entry WHERE op-entry.acct-db EQ acct.acct
		      AND op-entry.currency BEGINS acct.currency)) AND
		   (NOT CAN-FIND(FIRST op-entry WHERE op-entry.acct-cr EQ acct.acct
		      AND op-entry.currency BEGINS acct.currency))
		  THEN DO ON ERROR UNDO, THROW:
		    PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 ��� " acct.acct SKIP.
		    RUN DelSigns IN h_xclass ("acct", acct.acct + ',' + acct.currency).
		    /* !!!!!!!!!!?????????????????!!!!!!!!!!! */
		    /*
		    DEF VAR vResultC AS CHAR NO-UNDO.
		    RUN DelLinksCode IN h_xclass ("acct",
                                        "acct-reserve",
                                        acct.acct + "," + acct.currency,
                                        "",
                                            "",
                                        OUTPUT vResultC).
                    */
	    	    DELETE acct.
	    	END.
	    END.
	END.
	/* �᫮��� */
	for each loan-cond
	 where loan-cond.contract EQ loan.contract
	   and loan-cond.cont-code EQ loan.cont-code ON ERROR UNDO,THROW:
	    vSurr2 = GetSurrogate("loan-cond",ROWID(loan-cond)).
	    for each signs
	     where signs.file-name = "loan-cond"
	       and signs.surrogate = vSurr2 ON ERROR UNDO,THROW:
		DELETE signs.
	    END.
	    /* ��䨪� */
	    put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 ��䨪�" skip.
	    for each term-obl
	     where term-obl.idnt NE 5
	       and term-obl.contract EQ loan.contract
	       and term-obl.cont-code EQ loan.cont-code ON ERROR UNDO,THROW:
	        DELETE term-obl.
	    END.
	    /* �⠢�� */
	    put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 �⠢��" skip.
	    for each comm-rate
	     where comm-rate.kau EQ loan.contract + "," + loan.cont-code
	        ON ERROR UNDO,THROW:
	        DELETE comm-rate.
	    END.
	    put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 ᠬ� �᫮���" skip.
	    DELETE loan-cond.
	END.
	DEF VAR vvvCurAcct AS CHAR NO-UNDO.
	DEF VAR vvvOk AS LOG NO-UNDO.
	vvvCurAcct = GetXAttrValueEx( "loan", loan.contract + ',' + loan.cont-code, "PLDealCurAcct",?).
	IF vvvCurAcct NE ? THEN DO ON ERROR UNDO, THROW:
	 /*   RUN AcctFree IN h_acct( vvvCurAcct, OUTPUT vvvOk).
	    RUN AcctFree IN h_acct( AddFilToAcct( vvvCurAcct, shFilial), OUTPUT vvvOk).
	    08_09_2016 */
	END.
	put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + " 㤠�塞 ᠬ �������" skip.
	DELETE loan.
	END.
	    DEF VAR ijj AS INTEGER NO-UNDO.
	CATCH eAnyError2 AS Progress.Lang.AppError:
	    DEF VAR strr_3 AS CHAR NO-UNDO.
	    strr_3 = "(" + eAnyError2:ReturnValue + ")".
	    DO ijj = 1 TO eAnyError2:NumMessages:
		strr_3 = strr_3 + (IF ijj > 1 THEN ", " ELSE "") + eAnyError2:GetMessage(ijj).
	    END.
	    PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") + 
		" �訡�� 㤠����� ������� " + vCont-code + ":" + strr_3 SKIP.
	    UNDO, THROW eAnyError2.
	END CATCH.
	CATCH eAnyError3 AS Progress.Lang.Error:
	    DEF VAR strr4 AS CHAR NO-UNDO.
	    strr4 = "(" /*+ eAnyError3:ReturnValue RETURN-VALUE*/ + ")".
	    DO ijj = 1 TO /*ERROR-STATUS:NUM-MESSAGES*/ eAnyError3:NumMessages:
		strr4 = strr4 + (IF ijj > 1 THEN ", " ELSE "") + eAnyError3:GetMessage(ijj).
	    END.
	    PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") + 
		" �訡�� 㤠����� ������� " + vCont-code + ":" + strr4 SKIP.
	    UNDO, THROW eAnyError3.
	END CATCH.
END.

hPDS = DATASET dsRequests:HANDLE.

DEF VAR	cSchemaLocation  AS CHAR INIT ?  NO-UNDO. /* ?I ??u????? ??S? ??? */
DEF VAR	lOverrideDefaultMapping AS LOG INIT NO NO-UNDO.
DEF VAR i as int no-undo.
DEF VAR ccode as char no-undo.
def var acctopendate as date no-undo.


/*hPDS:WRITE-XMLSCHEMA("FILE", "ipl.xsd", YES, ?, NO).*/
DEF BUFFER bAnswer FOR wapp.messages.

RUN Init-SysMes IN h_tmess ("","","").
/*{setdest.i}*/

FOR EACH wapp.messages
  WHERE wapp.messages.deliverytime EQ ?
    AND wapp.messages.recipientid EQ 2
  EXCLUSIVE-LOCK BREAK BY CREATETIME ON ERROR UNDO,THROW:
    DEF VAR lc AS longchar NO-UNDO.
    DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
    DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
    DEFINE VARIABLE hField AS HANDLE NO-UNDO.

    DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
    DEFINE VARIABLE ix AS INTEGER NO-UNDO.
    DEFINE VARIABLE jx AS INTEGER NO-UNDO.
    DEF VAR vclaim-id AS INTEGER NO-UNDO.
    DEF VAR vtask-id AS INTEGER NO-UNDO.

  lc = wapp.messages.body. /*codepage-convert( wapp.messages.body, SESSION:CHARSET, "utf-8").*/

  /*put unformatted string(lc) skip.*/
  {empty ttRequest}
  {empty ttPERSON}
  {empty ttDOCUM}
  {empty ttADDR}
  DO ON ERROR UNDO, THROW:
    CASE wapp.messages.typeid:
    WHEN 1 THEN DO ON ERROR UNDO, THROW: /* ��� */


    /* ᢥઠ �ଠ� XML *
    DEF VAR lc2 AS longchar NO-UNDO.
    output stream vvs to value ("ipl3.xsd_")
    UNBUFFERED * CONVERT  TARGET "1251"  SOURCE "IBM866"*.
    FIND FIRST wapp.message_types WHERE wapp.message_types.id EQ 2 NO-LOCK.
    lc2 = wapp.message_types.scheme.
    put stream vvs unformatted string(lc2) SKIP.
    output stream vvs close.*/
    
    DEFINE VARIABLE hDoc3 AS HANDLE NO-UNDO.
    CREATE X-DOCUMENT hDoc3.

    DO ON ERROR UNDO, THROW:
   	 /* hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV2", "ipl1.xsd").
		 hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV3", "ipl1_V3.xsd").
		 hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV4", "ipl1_V4.xsd").
		 hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV5", "ipl1_V5.xsd").
		 hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV6", "ipl1_V6.xsd").
		 hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV7", "ipl1_V7.xsd").
		 hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV8", "ipl1_V8.xsd").
		 hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV9", "ipl1_V9.xsd").
	         hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV13", "ipl1_V13.xsd").        
       hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV14", "ipl1_V14.xsd"). */
	         hDoc3:ADD-SCHEMA-LOCATION("http://plus-bank.ru/Pipeline2BIS/BaseRequestV15", "ipl1_V15.xsd").        
                 hDoc3:STRICT-ENTITY-RESOLUTION = false.
       lReturn = hDoc3:LOAD("longchar", lc, TRUE) NO-ERROR.
       IF NOT lReturn THEN 
       DO:
    	    /*PUT UNFORMATTED string(lc) SKIP.*/
	       UNDO, THROW NEW Progress.Lang.AppError( "�訡�� ������樨 �����:" + RetValErr()).
	    END.

	    FINALLY:
	       DELETE OBJECT hDoc3.
	    END.
    END.

	lReturn = hPDS:READ-XML 
		( "longchar" , lc, /*ReadMode*/ "EMPTY",
			/*SchemaLocation*/ ?, /*OverrideDefaultMapping*/ NO, ?, "STRICT") NO-ERROR.
	IF NOT lReturn THEN
	    UNDO, THROW NEW Progress.Lang.AppError( "READ-XML:" + RetValErr()).
	{ipl-msg1.i}
    END.
    WHEN 3 THEN DO ON ERROR UNDO, THROW: /* ����� ������ ������� */
	lReturn = TEMP-TABLE ttREQUESTINFO:READ-XML
		( "longchar" , lc, /*ReadMode*/ "EMPTY",
			/*SchemaLocation*/ ?, /*OverrideDefaultMapping*/ NO, ?, "STRICT") NO-ERROR.
	IF NOT lReturn THEN
	    UNDO, THROW NEW Progress.Lang.AppError( "READ-XML:" + RetValErr()).
	FOR EACH ttREQUESTINFO ON ERROR UNDO, THROW:
	    RUN XmlLoanInfo(BUFFER wapp.messages, ttREQUESTINFO.claim-id).
	END.
    END.
    WHEN 6 THEN DO ON ERROR UNDO, THROW: /* ����� �� �뤠�� �।�� */
	CREATE X-DOCUMENT hDoc.
	CREATE X-NODEREF hRoot.
	CREATE X-NODEREF hTable.
	CREATE X-NODEREF hField.
	
	DO ON ERROR UNDO, THROW:
	    lReturn = hDoc:LOAD("longchar", lc, FALSE) NO-ERROR.
	    IF NOT lReturn THEN
		UNDO, THROW NEW Progress.Lang.AppError( "READ-XML:" + RetValErr()).
	    hDoc:GET-DOCUMENT-ELEMENT(hRoot).
	    /* Read each Customer from the root */
	    IF hRoot:NAME NE 'RequestConclusion'
		THEN UNDO, THROW NEW Progress.Lang.AppError( "⨯ ����� �� ᮮ⢥����� ⨯� ᮮ�饭��").
	    vclaim-id = ?.
	    vtask-id  = ?.
	    REPEAT ix = 1 TO hRoot:NUM-CHILDREN ON ERROR UNDO, THROW:
		hRoot:GET-CHILD(hTable, ix).
		IF hTable:NUM-CHILDREN < 1 THEN NEXT.
		hTable:GET-CHILD(hField, 1).
		CASE hTable:NAME:
		WHEN 'claim-id' THEN vclaim-id = INTEGER(hField:NODE-VALUE).
		WHEN 'task-id' THEN vtask-id = INTEGER(hField:NODE-VALUE).
		END. /* CASE */
	    END. /* REPEAT ix */
	    IF vclaim-id EQ ? OR vtask-id EQ ?
		THEN UNDO, THROW NEW Progress.Lang.AppError( "�� ��।����� �� ��ࠬ���� �����").
	    FINALLY:
		DELETE OBJECT hField.
		DELETE OBJECT hTable.
		DELETE OBJECT hDoc.
		DELETE OBJECT hRoot.
	    END.
	END.
        FIND FIRST signs
         WHERE signs.file-name EQ 'loan'
           AND signs.code EQ 'PLDealID'
           AND signs.code-value EQ STRING(vclaim-id)
           NO-LOCK NO-ERROR.
        IF NOT AVAIL signs THEN
            UNDO , THROW NEW Progress.Lang.AppError(
             "������� �� ��� � ����஬ '" + string(vclaim-id) + 
             "' �� ������.").
        PUT UNFORMATTED "** " + STRING(NOW,"99/99/9999 HH:MM:SS") +
    	  " �뤠� �।�� ��� CLAIM-ID=" + STRING(vclaim-id) SKIP.
	FIND FIRST loan
	  WHERE loan.contract EQ ENTRY( 1, signs.surrogate)
	    AND loan.cont-code EQ ENTRY( 2, signs.surrogate).
	
	IF loan.loan-status EQ "����"
         THEN DO ON ERROR UNDO, THROW:
            /* UNDO , THROW NEW Progress.Lang.AppError(
             "������� " + loan.doc-ref + " �� ��� � ����஬ '" + string(vclaim-id) + 
             "' 㦥 �� �뤠� ࠭��.").*/
	    RUN XmlMsgAnswer( BUFFER wapp.messages, vclaim-id, vtask-id,
	     "-2", "��� �������: ������� " + loan.doc-ref + " �� ��� � ����஬ '" + string(vclaim-id) + 
             "' 㦥 �� �뤠� ࠭��.").
	END. ELSE DO ON ERROR UNDO, THROW:
	IF loan.loan-status NE "����"
         THEN UNDO , THROW NEW Progress.Lang.AppError(
             "������� " + loan.doc-ref + " �� ��� � ����஬ '" + string(vclaim-id) + 
             "' ����� ����� '" + loan.loan-status + "' � �� ����� ���� �뤠�.").


        acctopendate = loan.open-date.

   	if fChkClsDate( loan.open-date, "*") then do:
   	      if fChkClsDate((loan.open-date + 1), "*") then
               UNDO, THROW NEW Progress.Lang.AppError( "����.���� " + string( loan.open-date, "99/99/9999")
           	+ ' 㦥 ������.').
	      acctopendate = loan.open-date + 1.
	end.
  
	
	/* ���뢠�� ��१�ࢨ஢���� ⥪�騩 ��� */
	DEF VAR vvvvCurAcct AS CHAR NO-UNDO.
	DEF VAR myCurAcct AS CHAR NO-UNDO.
	vvvvCurAcct = GetXAttrValueEx( "loan", loan.contract + ',' + loan.cont-code, "PLDealCurAcct",?).
        DEF VAR mCurFil AS CHAR NO-UNDO.
        mCurFil = shFilial.
	IF vvvvCurAcct NE ? THEN DO ON ERROR UNDO, THROW:
	    IF mCurFil NE loan.filial-id THEN DO:
	        PUT UNFORMATTED "ᬥ�� 䨫���� �� " loan.filial-id SKIP.
	        RUN SetEnvironment IN h_base (loan.filial-id).
	    END.
	    myCurAcct = AddFilToAcct(vvvvCurAcct, loan.filial-id).
	    FIND FIRST acct WHERE
	      acct.filial-id = loan.filial-id AND
	      acct.Acct      = myCurAcct AND
	      acct.Currency  = loan.currency NO-LOCK NO-ERROR.
	    IF AVAIL acct
	     THEN UNDO, THROW NEW Progress.Lang.AppError(
		"�訡�� �� ᮧ����� ��� " + myCurAcct + ", ��� 㦥 �������."
		).

	    RELEASE acct.
	    DEF VAR vca AS LOG NO-UNDO.
	    PUT UNFORMATTED "㤠�塞 �� १�ࢠ ��� " + vvvvCurAcct SKIP.
	    RUN AcctFree IN h_acct( vvvvCurAcct, OUTPUT vca).
	    PUT UNFORMATTED "㤠�塞 �� १�ࢠ ��� " + AddFilToAcct( vvvvCurAcct, shFilial) SKIP.
	    RUN AcctFree IN h_acct( AddFilToAcct( vvvvCurAcct, shFilial), OUTPUT vca).

	    PUT UNFORMATTED "ᮧ���� ��� " + DelFilFromAcct( vvvvCurAcct) SKIP.
	    
	    RUN Cm_acct_cr IN h_acct (
              "acctb",
              INT64(SUBSTR(vvvvCurAcct, 1, 5)),
              loan.currency,
              loan.cust-cat,
              loan.cust-id,
              acctopendate,
              OUTPUT myCurAcct,
              BUFFER acct,
              DelFilFromAcct( vvvvCurAcct),
              "",
              ?,
              "�����",
              loan.user-id,
              loan.branch-id,
              YES
	     ) NO-ERROR.

	    IF ERROR-STATUS:ERROR 
	     THEN UNDO, THROW NEW Progress.Lang.AppError( RETURN-VALUE).

	    PUT UNFORMATTED ' ��� ' acct.number + ' ᮧ���.' SKIP.

	    FIND FIRST acct WHERE
	      acct.filial-id = loan.filial-id AND
	      acct.Acct      = myCurAcct AND
	      acct.Currency  = loan.currency NO-LOCK.
	    UpdateSigns( acct.class-code, GetSurrogate( "acct", ROWID(acct)),
		 "��������",
		  STRING(loan.open-date, "99/99/9999") + "," + acct.number /* loan.doc-ref */ , ?).
	    UpdateSigns( acct.class-code, GetSurrogate( "acct", ROWID(acct)),
		 "���������", acct.user-id, ?).

	    RUN BalToAcct_Xattr(RECID(acct),"*",YES,YES).
	    IF RETURN-VALUE EQ "ERROR"
	     THEN UNDO, THROW NEW Progress.Lang.AppError(
		"�訡�� �� ���樠������ ���.४����⮢ � ��� 2-�� ���浪� " +
		"� �� �����䨪���  [��᪨��᫥�]  ").
	    CREATE loan-acct.
	     loan-acct.cont-code = loan.cont-code.
	    ASSIGN
	      loan-acct.contract = loan.contract
	      loan-acct.acct = acct.acct
	      loan-acct.currency = acct.currency
	      loan-acct.acct-type = "�।����"
	      loan-acct.since = loan.open-date
	    .
	    VALIDATE loan-acct.
	    FINALLY:
		IF mCurFil NE shFilial THEN DO:
		    PUT UNFORMATTED "ᬥ�� 䨫���� �� " mCurFil SKIP.
		    RUN SetEnvironment IN h_base (mCurFil).
		END.
	    END FINALLY.
	END.

        ASSIGN
    	    loan.loan-status = '����'.
    	VALIDATE loan.
        UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "�������",
                  "",
                  ?).
	RUN XmlMsgAnswer( BUFFER wapp.messages, vclaim-id, vtask-id,
	 "0", "������� " + loan.doc-ref + " ��ॢ���� � ����� �뤠�.").
	END.
        /*
        DEF VAR in-fil AS CHAR NO-UNDO.
        in-fil = ttREQUEST.branch-id.
        FIND FIRST branch WHERE branch-id = in-fil NO-LOCK NO-ERROR.
        IF NOT AVAIL branch THEN
                UNDO, THROW NEW Progress.Lang.AppError( "�� ������ ��� ���ࠧ������� " + in-fil
            	+ ' � ��������� ���.').
        DO WHILE NOT CAN-DO( "0,10,11", branch.branch-type) ON ERROR UNDO, THROW:
        	    in-fil = branch.parent-id.
    	    FIND FIRST branch WHERE branch-id = in-fil NO-LOCK.
        END.
        if shFilial NE in-fil THEN DO:
    	    RUN DelConnectLink.
	    RUN SetConnectLink IN h_base (in-fil).
	    RUN SetEnvironment IN h_base (in-fil). * ���⥪�� ��࠭���� 䨫���� *
	    gend-date = ttREQUEST.open-date.
	    PUT UNFORMATTED 'ᬥ�� ⥪ 䨫���� �� ' + shFilial SKIP.
        END.
        */


	/*
	<?xml version="1.0" encoding="windows-1251"?>
	<RequestConclution>
	  <claim-id>715</claim-id>
	  <task-id>254665</task-id>
	</RequestConclution>
	*/

    END.
    WHEN 7 THEN DO: /* ����� �� �⪠� ᤥ��� */
	CREATE X-DOCUMENT hDoc.
	CREATE X-NODEREF hRoot.
	CREATE X-NODEREF hTable.
	CREATE X-NODEREF hField.
	
	DO ON ERROR UNDO, THROW:
	    lReturn = hDoc:LOAD("longchar", lc, FALSE) NO-ERROR.
	    IF NOT lReturn THEN
		UNDO, THROW NEW Progress.Lang.AppError( "READ-XML:" + RetValErr()).
	    hDoc:GET-DOCUMENT-ELEMENT(hRoot).
	    /* Read each Customer from the root */
	    /* put unformatted "RootName=" + hRoot:NAME skip. */
	    IF hRoot:NAME NE 'RequestRollBack'
		THEN UNDO, THROW NEW Progress.Lang.AppError( "⨯ ����� �� ᮮ⢥����� ⨯� ᮮ�饭��").
	    vclaim-id = ?.
	    vtask-id  = ?.
	    REPEAT ix = 1 TO hRoot:NUM-CHILDREN ON ERROR UNDO, THROW:
		hRoot:GET-CHILD(hTable, ix).
		IF hTable:NUM-CHILDREN < 1 THEN NEXT.
		hTable:GET-CHILD(hField, 1).
		CASE hTable:NAME:
		WHEN 'claim-id' THEN vclaim-id = INTEGER(hField:NODE-VALUE).
		WHEN 'task-id' THEN vtask-id = INTEGER(hField:NODE-VALUE).
		END. /* CASE */
	    END. /* REPEAT ix */
	    IF vclaim-id EQ ? OR vtask-id EQ ?
		THEN UNDO, THROW NEW Progress.Lang.AppError( "�� ��।����� �� ��ࠬ���� �����").
	    put unformatted "�⪠� ᤥ��� " string(vclaim-id) " " string(vtask-id) skip.
	    FINALLY:
		DELETE OBJECT hDoc.
		DELETE OBJECT hRoot.
		DELETE OBJECT hTable.
		DELETE OBJECT hField.
	    END.
	END.
        FIND FIRST signs
         WHERE signs.file-name EQ 'loan'
           AND signs.code EQ 'PLDealID'
           AND signs.code-value EQ STRING(vclaim-id)
           NO-LOCK NO-ERROR.
        IF NOT AVAIL signs THEN
            UNDO , THROW NEW Progress.Lang.AppError(
             "������� �� ��� � ����஬ '" + string(vclaim-id) + 
             "' �� ������.").
        PUT UNFORMATTED "** " + STRING(NOW,"99/99/9999 HH:MM:SS") +
    	  " �⪠� �।��, ��� CLAIM-ID=" + STRING(vclaim-id) + " " + signs.surrogate SKIP.
    	vContCode = ENTRY( 2, signs.surrogate).
	/* 㤠����� ������� */
	FOR FIRST loan
	  WHERE loan.contract EQ ENTRY( 1, signs.surrogate)
	    AND loan.cont-code EQ ENTRY( 2, signs.surrogate) NO-LOCK ON ERROR UNDO,THROW:
	    put unformatted STRING(NOW,"99/99/9999 HH:MM:SS") + "㤠�塞 ������� " + loan.doc-ref skip.
    	    RUN iplDeleteLoan( loan.contract, loan.cont-code, True).
	END.
	
	RUN XmlMsgAnswer( BUFFER wapp.messages, vclaim-id, vtask-id,
	 "0", "������� " + vContCode + " 㤠���.").
	/*
	<?xml version="1.0" encoding="windows-1251"?>
	<RequestConclution>
	  <claim-id>715</claim-id>
	  <task-id>254665</task-id>
	</RequestConclution>
	    UNDO, THROW NEW Progress.Lang.AppError( "����� ���� �� ��ࠡ��뢠����").*/
    END.
    OTHERWISE DO:
	UNDO, THROW NEW Progress.Lang.AppError(
	 "ᮮ�饭�� � TYPEID=" + STRING( wapp.messages.typeid) +
	 " �� ��ࠡ��뢠����.").
    END.
    END. /* CASE */

    CATCH eAnyError2 AS Progress.Lang.AppError:
	DEF VAR strr5 AS CHAR NO-UNDO.
	strr5 = "(" + eAnyError2:ReturnValue + ")".
	DO i = 1 TO eAnyError2:NumMessages:
	    strr5 = strr5 + (IF i > 1 THEN ", " ELSE "") + eAnyError2:GetMessage(i).
	END.
	RUN XmlMsgAnswer( BUFFER wapp.messages,
	 IF AVAIL ttRequest THEN ttRequest.claim-id ELSE string(vclaim-id),
	 IF AVAIL ttRequest THEN ttRequest.task-id ELSE string(vtask-id),
	 "-1", "��� �������: �訡�� ��ࠡ�⪨ ᮮ�饭�� " + strr5).
	PUT UNFORMATTED 
	    "MsgID=" + string(wapp.messages.id) + "^ Error=" + strr5 SKIP.
    END CATCH.
    CATCH eAnyError AS Progress.Lang.Error:
	DEF VAR strr AS CHAR NO-UNDO.
	strr = RETURN-VALUE.
	DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    	    strr = strr + (IF i > 1 THEN ", " ELSE ": ") + eAnyError:GetMessage(i).
	END.
	PUT UNFORMATTED 
	    "ID=" + string(wapp.messages.id) + " Error:" + strr SKIP.
	RUN XmlMsgAnswer( BUFFER wapp.messages,
	 IF AVAIL ttRequest THEN ttRequest.claim-id ELSE string(vclaim-id),
	 IF AVAIL ttRequest THEN ttRequest.task-id ELSE string(vtask-id),
	 "-1", "��� �������: �訡�� ��ࠡ�⪨ ᮮ�饭��:" + strr).
    END CATCH.
  END. /* DO */
  ASSIGN
    wapp.messages.deliverytime = DATETIME-TZ( NOW, 0).
  VALIDATE wapp.messages.
END.

{intrface.del}
DELETE OBJECT hSAXnilAttr.

