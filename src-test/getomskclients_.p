/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: 
     Filename: clientsomsk.p
      Comment: 
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    

���
phone
homephone
workphone
cellphone
phonestr
smsphone

��� ��
����譨�
����譨� 䠪�
ࠡ�稩
�⮢�
⥫�䠪�
��:
cell-phone
����䮭3
tel
phone-home

��� ��
tel
Telex
����䮭
����䠪�(fax)

*/

/* &SCOPED-DEFINE DEBUG-LOG 1 */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

&SCOPED-DEFINE ACTION_NEW_FROM_OM 1
&SCOPED-DEFINE ACTION_UPDATE_FROM_OM 2

&SCOPED-DEFINE VOLUME_UPPER 1

/* ���-�� �����⮢, ����㦠���� �� ���� ࠧ */
&SCOPED-DEFINE COUNT_CLIENTS 1000

&SCOPED-DEFINE STATUS_NULL ?
&SCOPED-DEFINE STATUS_OK 1
&SCOPED-DEFINE STATUS_OK_WITH_ERROR 2
&SCOPED-DEFINE STATUS_UNKNOW_ERROR -1
&SCOPED-DEFINE STATUS_DOC_NOT_FOUND -2
&SCOPED-DEFINE STATUS_CLIENT_NAME -3
&SCOPED-DEFINE STATUS_CLIENT_TYPE -4
&SCOPED-DEFINE STATUS_DUPLICATE_CLIENT -5
&SCOPED-DEFINE STATUS_CLIENT_NOT_FOUND -6
&SCOPED-DEFINE STATUS_ADR_NOT_FOUND -7
&SCOPED-DEFINE STATUS_ADR_ERROR -8
&SCOPED-DEFINE STATUS_INN_ERROR -9
&SCOPED-DEFINE STATUS_TYPEFORM_ERROR -10
&SCOPED-DEFINE STATUS_BIRTHDAY_ERROR -11
&SCOPED-DEFINE STATUS_CLIENT_FROM_BIS -12
&SCOPED-DEFINE STATUS_OGRN_ERROR -13
&SCOPED-DEFINE STATUS_DOC_ERROR -14

&SCOPED-DEFINE ERROR_DOC_NOT_FOUND "�� ������ ���㬥��"
&SCOPED-DEFINE ERROR_DOC_TYPE "�����४�� ⨯ ���㬥��"
&SCOPED-DEFINE ERROR_DOC_NUMBER "�����४�� �����/��� ���㬥��"
&SCOPED-DEFINE ERROR_DATEDOC "�訡�� � ��� ���㬥��"
&SCOPED-DEFINE ERROR_DOC_UPDATE "�訡�� � ����ᥭ��/���������� ���㬥��"
&SCOPED-DEFINE ERROR_ADR_NOT_FOUND "�� ������ ����"
&SCOPED-DEFINE ERROR_ADR_UPDATE "�訡�� � ����ᥭ��/���������� ����"
&SCOPED-DEFINE ERROR_CLIENT_NAME "�訡�� � ������������ ������"
&SCOPED-DEFINE ERROR_CLIENT_TYPE "�訡�� � ⨯� ������"
&SCOPED-DEFINE ERROR_COUNTRY_NOT_FOUND "����।����� ��࠭� ������"
&SCOPED-DEFINE ERROR_ISSUE_EMPTY "����������� ���� �뤠� � ���㬥��"
&SCOPED-DEFINE ERROR_BIRTHDAY "�訡�� � ��� ஦�����/ॣ����樨 ������"
&SCOPED-DEFINE ERROR_DUPLICATE_CLIENT "������� ������ � ⠪�� ���㬥�⮬"
&SCOPED-DEFINE ERROR_CLIENT_NOT_FOUND "�� ������ ������ ��� ����������"
&SCOPED-DEFINE ERROR_PERSON_CREATE "�� 㤠���� ᮧ���� ������"
&SCOPED-DEFINE ERROR_INN "������ �ଠ� ���"
&SCOPED-DEFINE ERROR_OGRN "����������� ���� ����"
&SCOPED-DEFINE ERROR_TYPEFORM "�訡�� � TYPEFORM"
&SCOPED-DEFINE ERROR_CLIENT_FROM_BIS "������ ����砫쭮 �� ����"

&SCOPED-DEFINE DOLRUK_IF_EMPTY "��४��"

{globals.i}
/* {intrface.get osyst}
{filial.pro} */
{intrface.get xclass}
{intrface.get widg}
{intrface.get db2l}
{intrface.get cust}
{pers.fun}
{getomskclients.fun}


DEFINE INPUT-OUTPUT PARAMETER iCurrentCount AS INT64 NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iCountClients AS INT64 NO-UNDO. 
DEFINE OUTPUT PARAMETER bAllClients AS LOGICAL NO-UNDO.
DEFINE VAR barMessage AS CHAR NO-UNDO INIT "����㦠���� ������� ".
DEFINE VAR latStr AS CHAR INIT 'x,X,k,K,H,H' NO-UNDO CASE-SENSITIVE.
DEFINE VAR rusStr AS CHAR INIT '�,�,�,�,�,�' NO-UNDO CASE-SENSITIVE.
DEF VAR vStartTime AS DATETIME NO-UNDO.
DEF VAR bChangeCID AS LOG NO-UNDO.

FUNCTION TO_LAT RETURN CHAR(
    INPUT iStr AS CHAR):
    DEF VAR ii AS INT NO-UNDO.
    DEF VAR ij AS INT NO-UNDO.

    DO ii = 1 TO LENGTH( iStr):
	ij = LOOKUP(SUBSTR( iStr, ii, 1), rusStr).
        IF ij > 0 THEN
            SUBSTR( iStr, ii, 1) = ENTRY( ij, latStr).
    END.
    RETURN iStr.
END.

FUNCTION TO_RUS RETURN CHAR(
    INPUT iStr AS CHAR):
    DEF VAR ii AS INT NO-UNDO.
    DEF VAR ij AS INT NO-UNDO.

    DO ii = 1 TO LENGTH( iStr):
	ij = LOOKUP(SUBSTR( iStr, ii, 1), latStr).
	IF ij > 0 THEN
	    SUBSTR( iStr, ii, 1) = ENTRY( ij, rusStr).
    END.
    RETURN iStr.
END.

FUNCTION convertDoc2BIS RETURN CHAR(
    INPUT iDocType AS CHAR, 
    INPUT iDocSer AS CHAR, 
    INPUT iDocNum AS CHAR):

    iDocSer = TRIM( iDocSer).
    iDocNum = TRIM( iDocNum).
    IF iDocType EQ '��ᯮ��'
     AND LENGTH( iDocSer) EQ 4 AND INDEX( iDocSer, ' ') < 1 THEN
     iDocSer = SUBSTRING(iDocSer,1,2) + ' ' + SUBSTRING(iDocSer,3).
    IF iDocSer EQ ? THEN iDocSer = "".
    IF iDocSer EQ "-" THEN iDocSer = "".
    IF iDocNum EQ ? THEN iDocNum = "".

    IF iDocType EQ '���㬥��' THEN DO:
	IF NUM-ENTRIES( iDocSer, ' ') EQ 2 THEN iDocSer = REPLACE ( iDocSer, ' ', '-').
	IF NUM-ENTRIES( iDocSer, '-') EQ 2 THEN DO:
    	    ENTRY(1, iDocSer, '-') = TO_LAT(ENTRY( 1, iDocSer, '-')).
	    ENTRY(2, iDocSer, '-') = TO_RUS(ENTRY( 2, iDocSer, '-')).
	END.
    END.

    IF iDocType EQ '��ᯮ��' AND LENGTH( iDocSer) EQ 5 AND
      SUBSTR(iDocSer,1,1) EQ SUBSTR(iDocSer,2,1) AND
      SUBSTR(iDocSer,2,1) EQ SUBSTR(iDocSer,4,1) AND
      SUBSTR(iDocSer,4,1) EQ SUBSTR(iDocSer,5,1) THEN
	UNDO, THROW NEW Progress.Lang.AppError( "���� ��ᯮ�� �� ����� ������ �� ���������� ���").
	
    RETURN TRIM( iDocSer + ' ' + iDocNum).
END FUNCTION.

FUNCTION DeDublPhone RETURN CHAR (
    INPUT iNewPhone AS CHAR,
    INPUT iAllPhones AS CHAR):
    DEF VAR uk AS INT NO-UNDO.
    DEF VAR iStr AS CHAR NO-UNDO.
    iStr = ''.
    DO uk = 1 TO NUM-ENTRIES( iNewPhone):
	IF ENTRY( uk, iNewPhone) NE "" AND LOOKUP(ENTRY( uk, iNewPhone), iAllPhones) EQ 0 THEN DO:
	    iStr = (IF iStr EQ '' THEN '' ELSE iStr + ',') + ENTRY( uk, iNewPhone).
	END.
    END.
    RETURN iStr.
END FUNCTION.

IF iCurrentCount = 0 THEN DO ON ERROR UNDO, THROW:
    SELECT COUNT(*) INTO iCountClients FROM bank.clients-mfr WHERE 
        (bank.clients-mfr.action = {&ACTION_NEW_FROM_OM} OR bank.clients-mfr.action = {&ACTION_UPDATE_FROM_OM}) 
        AND bank.clients-mfr.status_ IS NULL.
    
END.

PUT UNFORMATTED "0 begin - " + STRING(iCurrentCount) SKIP.

/*
    {bar-beg2.i
     &BarTotal     = iCountClients
     &BarMessage   = barMessage
    }
*/

/* �᭮��� ४������ */
DEFINE VAR iCid AS INT64 NO-UNDO.
DEFINE VAR iBisId AS INT64 NO-UNDO.

DEFINE VAR iTypeCl AS INT64 NO-UNDO.
DEFINE VAR strAdressType AS CHAR NO-UNDO.
DEFINE VAR dateAdressOpen AS DATE NO-UNDO.
DEFINE VAR dateAdressClose AS DATE NO-UNDO.
DEFINE VAR strAdress AS CHAR NO-UNDO.
DEFINE VAR strKodReg AS CHAR NO-UNDO.
DEFINE VAR dateBirthDay AS DATE NO-UNDO.
DEFINE VAR strCountryId AS CHAR NO-UNDO.
DEFINE VAR strDocument AS CHAR NO-UNDO.
DEFINE VAR strDocumentId AS CHAR NO-UNDO.
DEFINE VAR strIssueCl AS CHAR NO-UNDO.
DEFINE VAR strIssueDoc AS CHAR NO-UNDO.
DEFINE VAR strKodPodr AS CHAR NO-UNDO.
DEFINE VAR dateOpenTime AS DATE NO-UNDO.
DEFINE VAR dateOpenDoc AS DATE NO-UNDO.
DEFINE VAR dateCloseDoc AS DATE NO-UNDO.
DEFINE VAR strFirstNames AS CHAR NO-UNDO. /* fullname  ��� ��.��� */
DEFINE VAR strNameLast AS CHAR NO-UNDO.   /* shortname ��� ��.��� */
DEFINE VAR bGender AS LOGICAL NO-UNDO.
DEFINE VAR strInn AS CHAR NO-UNDO.
DEFINE VAR strOgrn AS CHAR NO-UNDO.
DEFINE VAR strPhone AS CHAR NO-UNDO.
DEFINE VAR strTaxInsp AS CHAR NO-UNDO.
DEFINE VAR bCreated AS LOGICAL NO-UNDO.
DEFINE VAR strCustCat AS CHAR NO-UNDO.
DEFINE VAR strFormType AS CHAR NO-UNDO.
DEFINE VAR strDolRuk AS CHAR NO-UNDO.
DEFINE VAR strClientType AS CHAR NO-UNDO.
DEFINE VAR strBranchId AS CHAR NO-UNDO.
DEFINE VAR strSerNal AS CHAR NO-UNDO.
DEFINE VAR strNumNal AS CHAR NO-UNDO.

DEFINE VAR strStatusAdr AS CHAR NO-UNDO.
DEFINE VAR txtErr AS CHAR NO-UNDO.
DEFINE VAR j AS INT64 NO-UNDO. /* ��� cust-pri.cr */
DEFINE VAR tmpInt AS INT64 NO-UNDO.
DEFINE VAR tmpStr AS CHAR NO-UNDO.
DEFINE VAR clActionStatus AS INT64 NO-UNDO. /* 0 - ������ �� ���(��祣� �� ������), 1 - ���� �� ��᪀��, 2 - update �� ��᪀�� */ 
DEFINE VAR countClients AS INT64 NO-UNDO.
DEFINE VAR iTime AS INT64 NO-UNDO.
DEFINE VAR strPhone1 AS CHAR NO-UNDO.
DEFINE VAR strPhone2 AS CHAR NO-UNDO.
DEFINE VAR strPhoneOther AS CHAR NO-UNDO.
DEFINE TEMP-TABLE doc-mfr NO-UNDO
    LIKE bank.documents-mfr
    .
DEFINE TEMP-TABLE doc-mfr2 NO-UNDO
    LIKE bank.documents-mfr
    .
DEFINE TEMP-TABLE adr-mfr NO-UNDO
    LIKE bank.adress-mfr
    .


bChangeCID = FALSE.
countClients = 0.
vStartTime = NOW.
bAllClients = TRUE.
FOR EACH bank.clients-mfr
 WHERE 
        (bank.clients-mfr.action = {&ACTION_NEW_FROM_OM}
     OR bank.clients-mfr.action = {&ACTION_UPDATE_FROM_OM})
   AND bank.clients-mfr.status_ = {&STATUS_NULL}
/*   AND bank.clients-mfr.status_ = 0*/

/*      bank.clients-mfr.cid = 607375*/
/*   OR bank.clients-mfr.cid = 607815*/
/*   OR bank.clients-mfr.cid = 607732*/
/*   OR bank.clients-mfr.cid = 607667*/
/*   OR bank.clients-mfr.cid = 607690*/

    /* AND bank.clients-mfr.id = 21073 */
    /* and bank.clients-mfr.id = 130992 */
    /* and bank.clients-mfr.id = 138395 */
    /* and bank.clients-mfr.id = 281613 */
    EXCLUSIVE-LOCK /* BY bank.clients-mfr.subjectype */ BY bank.clients-mfr.id ON ERROR UNDO, THROW:

    PUT UNFORMATTED "1 begin - " + STRING(bank.clients-mfr.cid) SKIP.

    {empty doc-mfr}
    FOR EACH bank.documents-mfr
     WHERE bank.documents-mfr.clientid = bank.clients-mfr.id NO-LOCK:
        CREATE doc-mfr.
        BUFFER-COPY bank.documents-mfr TO doc-mfr.
    END.

    {empty doc-mfr2}
    
    FIND FIRST bank.clients-abs where clients-abs.cid EQ bank.clients-mfr.cid NO-LOCK NO-ERROR.
    IF NOT AVAIL clients-abs THEN NEXT.
/*    IF NOT AVAIL clients-abs THEN UNDO, THROW NEW Progress.Lang.AppError( "�� ������ ������ � banker.clients " + STRING(bank.clients-mfr.cid)).*/
    
    DEF VAR dv AS INT NO-UNDO.
    FOR EACH bank.expdoc
     WHERE expdoc.did = clients-abs.did
       AND (expdoc.field_ = 139 OR
            expdoc.field_ = 140 OR
            expdoc.field_ = 204 OR
            expdoc.field_ = 205 OR
            expdoc.field_ = 206 OR
            expdoc.field_ = 619) NO-LOCK:
        dv = (IF expdoc.volume EQ ? THEN 1 ELSE expdoc.volume).
        FIND FIRST doc-mfr2 WHERE doc-mfr2.volume = dv NO-ERROR.
        IF NOT AVAIL doc-mfr2 THEN DO:
          CREATE doc-mfr2.
          ASSIGN
             doc-mfr2.id       = dv
             doc-mfr2.volume   = dv
             doc-mfr2.clientid = bank.clients-mfr.id
             .
        END.
        CASE expdoc.field_:
        WHEN 139 THEN doc-mfr2.placedoc    = expdoc.contain.
        WHEN 140 THEN doc-mfr2.docopendate = DATE(expdoc.contain) NO-ERROR.
        WHEN 204 THEN DO:
    	    FIND FIRST bank.glossary
    	      WHERE glossary.TYPE = 42 AND glossary.gid = INTEGER(expdoc.contain) NO-LOCK NO-ERROR.
    	    doc-mfr2.doctype     = (IF AVAIL bank.glossary THEN bank.glossary.sname ELSE "").
    	    IF doc-mfr2.doctype EQ "��ᯮ�� ��" THEN doc-mfr2.doctype = "��ᯮ��".
    	    IF doc-mfr2.doctype EQ "��ᯮ�� ����" THEN doc-mfr2.doctype = "���㬥��".
    	    IF doc-mfr2.doctype EQ "�����࠭�� ��ᯮ��" THEN doc-mfr2.doctype = "�����१".
    	    IF doc-mfr2.doctype EQ "��.�.���.�ࠦ�.��" THEN doc-mfr2.doctype = "�६����".
    	    IF doc-mfr2.doctype EQ "C����⥫��⢮ � �।��⠢����� �६������ 㡥���" THEN doc-mfr2.doctype = "������".
    	    RELEASE bank.glossary.
    	END.
        WHEN 205 THEN doc-mfr2.docnumber   = expdoc.contain.
        WHEN 206 THEN doc-mfr2.docseria    = expdoc.contain.
        WHEN 619 THEN doc-mfr2.dockodpodr  = expdoc.contain.
        END.
    END.
    RELEASE bank.clients-abs.
    
    FOR EACH doc-mfr WHERE doc-mfr.doctype NE "����抠��":
	FIND FIRST doc-mfr2 WHERE doc-mfr2.volume = doc-mfr.volume NO-ERROR.
	IF NOT AVAIL doc-mfr2 THEN DO:
	    PUT UNFORMATTED doc-mfr.clientid ":�� ������ ���㬥�� volume=" doc-mfr.volume SKIP.
	    NEXT.
	END.
	IF doc-mfr2.placedoc NE doc-mfr.placedoc THEN
	    PUT UNFORMATTED doc-mfr.clientid ":" doc-mfr2.placedoc "<>" doc-mfr.placedoc SKIP.
	IF doc-mfr2.docopendate NE doc-mfr.docopendate THEN
	    PUT UNFORMATTED doc-mfr.clientid ":" doc-mfr2.docopendate "<>" doc-mfr.docopendate SKIP.
	IF doc-mfr2.doctype NE doc-mfr.doctype THEN
	    PUT UNFORMATTED doc-mfr.clientid ":" doc-mfr2.doctype "<>" doc-mfr.doctype SKIP.
	IF doc-mfr2.docnumber NE doc-mfr.docnumber THEN
	    PUT UNFORMATTED doc-mfr.clientid ":" doc-mfr2.docnumber "<>" doc-mfr.docnumber SKIP.
	IF doc-mfr2.docseria NE doc-mfr.docseria THEN
	    PUT UNFORMATTED doc-mfr.clientid ":" doc-mfr2.docseria "<>" doc-mfr.docseria SKIP.
	IF doc-mfr2.dockodpodr NE doc-mfr.dockodpodr THEN
	    PUT UNFORMATTED doc-mfr.clientid ":" doc-mfr2.dockodpodr "<>" doc-mfr.dockodpodr SKIP.
    END.
    
    FOR EACH doc-mfr2
     WHERE doc-mfr2.doctype NE "����抠��" /*EQ "��ᯮ��"*/ 
       AND doc-mfr2.docnumber NE ? AND doc-mfr2.docnumber NE ""
     :
	FIND FIRST doc-mfr WHERE doc-mfr.volume = doc-mfr2.volume AND doc-mfr.doctype = doc-mfr2.doctype NO-ERROR.
	IF NOT AVAIL doc-mfr THEN DO:
	    CREATE doc-mfr.
    	    BUFFER-COPY doc-mfr2 TO doc-mfr.
    	    PUT UNFORMATTED doc-mfr.clientid ": CID=" bank.clients-mfr.cid " �������� "
    		 doc-mfr2.doctype " " doc-mfr2.docseria " " doc-mfr2.docnumber " volume=" doc-mfr2.volume SKIP.
	END.
    END.
    {empty adr-mfr}
    FOR EACH bank.adress-mfr
     WHERE bank.adress-mfr.clientid = bank.clients-mfr.id NO-LOCK:
        CREATE adr-mfr.
        BUFFER-COPY bank.adress-mfr TO adr-mfr.
    END.
    
    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED STRING( NOW, "99/99/99 HH:MM:SS") + " (" + STRING( bank.clients-mfr.id) + ") ==== ��砫� ��ࠡ�⪨ ====" SKIP. &ENDIF
    iTime = MTIME.
    MYNEXT:
    DO ON ERROR UNDO, THROW:
        txtErr = "". 
        /* ��㥬 �����-��� */
        /* IF iCurrentCount MODULO 10 = 0 THEN DO: {bar2.i &BarPointer = iCurrentCount} END. */
        iCid = bank.clients-mfr.cid.
        iBisId = bank.clients-mfr.bisid.
        iTypeCl = bank.clients-mfr.subjectype.
        clActionStatus = 0.
        bCreated = FALSE.
        /*
                0 �ਤ��᪮� ���
                1 �����᪮� ���
                2 ����
                3 �������㠫�� �।�ਭ���⥫�
        */
        IF iTypeCl = 1 OR iTypeCl = 3 THEN
             strCustCat = '�'.
        ELSE IF iTypeCl = 0 THEN
             strCustCat = '�'.
             
        
        
    	ELSE NEXT.  
/*    	UNDO, THROW NEW Progress.Lang.AppError( "�� ��।����� ⨯ ������" ).*/
    	
    	PUT UNFORMATTED 
           iCid ";"
           iBisId ";"
           iTypeCl ";"
           clActionStatus
        SKIP.
        
    	/* 䨫��� ������ */
   
	strBranchId = IF bank.clients-mfr.branchid EQ '0100' THEN '0500' ELSE bank.clients-mfr.branchid.
    	IF strBranchId EQ ? OR TRIM(strBranchId) EQ "" THEN strBranchId = "0400".
        IF LENGTH(strBranchId) = 3 THEN strBranchId = '0' + strBranchId.
        DO WHILE TRUE ON ERROR UNDO, THROW:
            FIND FIRST branch
             WHERE branch.branch-id EQ strBranchId NO-LOCK NO-ERROR.
            IF NOT AVAIL branch OR CAN-DO( "10,11", branch.branch-type) THEN LEAVE.
            strBranchId = branch.parent-id.
        END.
        IF NOT AVAIL branch THEN strBranchId = '0400'.
        
	if shFilial NE strBranchId THEN DO ON ERROR UNDO, THROW:
	    shFilial = strBranchId.
	    /* RUN DelConnectLink.
	    RUN SetConnectLink IN h_base (strBranchId).
	    RUN SetEnvironment IN h_base (strBranchId). * ���⥪�� ��࠭���� 䨫���� *
	    gend-date = today. */
	    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED '    + ᬥ�� ⥪ 䨫���� �� ' + shFilial SKIP. &ENDIF
	END.

	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + �饬 ������" SKIP. &ENDIF
	/*===== �饬 ������ =====*/
        RELEASE person.
        RELEASE cust-corp.
    	IF (iTypeCl = 1 OR iTypeCl = 3) THEN DO: /* �� �� */
    	    /* �饬 �� �� CID */
    	    IF iTypeCl = 1 THEN
    	    FIND FIRST signs
    		 WHERE signs.file-name = 'person'
    		   AND signs.dec-value = iCid
    		   AND signs.code = 'CID' NO-LOCK NO-ERROR.
    	    ELSE
    	    FIND FIRST signs
    		 WHERE signs.file-name = 'person'
    		   AND signs.dec-value = iCid
    		   AND signs.code = 'CIDIP' NO-LOCK NO-ERROR.
    	    IF AVAIL signs THEN DO:
    		FIND FIRST person
    		     WHERE person.person-id = INTEGER( signs.surrogate) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    		IF NOT AVAIL person AND LOCKED person
    		  THEN UNDO, THROW NEW Progress.Lang.AppError( "������ person-id=" + signs.surrogate + ' �������஢�� ��㣨� ���짮��⥫��').
    		IF NOT AVAIL person
    		     THEN UNDO, THROW NEW Progress.Lang.AppError( "�� ������ ������ � person-id=" + STRING( iBisId)
    		         + " ��宦� ����ﭭ�� ������ signs � CID=" + STRING(signs.dec-value)).
    	    END.
    	    /* �饬 �� iBisId */
    	    IF NOT AVAIL person AND
    	      iBisId NE ? AND iBisId > 0 THEN DO:
    		FIND FIRST person
    		 WHERE person.person-id = iBisId EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    		IF NOT AVAIL person AND LOCKED person
    		  THEN UNDO, THROW NEW Progress.Lang.AppError( "������ person-id=" + STRING( iBisId) + ' �������஢�� ��㣨� ���짮��⥫��').
    		/*IF NOT AVAIL person
    		 THEN iBisId = ?.*/
    	    END.
    	    
    	    iBisId = (IF AVAIL person THEN person.person-id ELSE ?).
    	    clActionStatus = IF AVAIL person THEN 2 /* 2 - update �� ��᪀�� */ ELSE 1 /* 1 - ���� �� ��᪀�� */ .
        END. ELSE
        IF iTypeCl = 0 THEN DO: /* �� */
    	    /* �饬 �� iBisId */
    	    IF iBisId NE ?
    	      AND iBisId > 0 THEN DO ON ERROR UNDO, THROW:
    		FIND FIRST cust-corp
    		 WHERE cust-corp.cust-id = iBisId EXCLUSIVE-LOCK NO-ERROR.
    		IF NOT AVAIL cust-corp AND LOCKED cust-corp
    		  THEN UNDO, THROW NEW Progress.Lang.AppError( "������ cust-id=" + STRING( iBisId) + ' �������஢�� ��㣨� ���짮��⥫��').
    		IF NOT AVAIL cust-corp
    		 THEN UNDO, THROW NEW Progress.Lang.AppError( "�� ������ ������ � cust-id=" + STRING( iBisId) ).
    	    END.
    	    /* �饬 �� �� CID */
    	    FIND FIRST signs
    		 WHERE signs.file-name = 'cust-corp'
    		   AND signs.dec-value = iCid
    		   AND signs.code = 'CID' SHARE-LOCK NO-ERROR.
    	    IF AVAIL signs THEN DO ON ERROR UNDO, THROW:
    		IF AVAIL cust-corp AND signs.surrogate NE STRING(cust-corp.cust-id)
    		 THEN UNDO, THROW NEW Progress.Lang.AppError( "CID=" + STRING( iCid) + " ������ � ��㣮�� ������(cust-id=" + signs.surrogate +
    		    " ����� cust-id=" + STRING(cust-corp.cust-id)).
    		IF NOT AVAIL cust-corp THEN DO ON ERROR UNDO, THROW:
    		    FIND FIRST cust-corp
    		     WHERE cust-corp.cust-id = INTEGER( signs.surrogate) EXCLUSIVE-LOCK NO-ERROR.
    		    IF NOT AVAIL cust-corp AND LOCKED cust-corp
    		     THEN UNDO, THROW NEW Progress.Lang.AppError( "������ cust-id=" + signs.surrogate + ' �������஢�� ��㣨� ���짮��⥫��').
    		    IF NOT AVAIL cust-corp
    		     THEN UNDO, THROW NEW Progress.Lang.AppError( "�� ������ ������ � cust-id=" + STRING( iBisId)
    		         + " ��宦� ����ﭭ�� ������ signs � CID=" + STRING(signs.dec-value)).
    		    iBisId = cust-corp.cust-id.
    		END.
    	    END.
    	    clActionStatus = IF AVAIL cust-corp THEN 2 /* 2 - update �� ��᪀�� */ ELSE 1 /* 1 - ���� �� ��᪀�� */ .
        END. ELSE UNDO, THROW NEW Progress.Lang.AppError( "�� ������ ⨯ ������ iTypeCl=" + STRING(iTypeCl) ).

	/*===== �饬 �� ���㬥���( ��� ���/���) � 㤮�⮢��塞�� �� �� ������ �� ������ =====*/
	DEF VAR CheckedDoc AS LOG NO-UNDO.
	DEF VAR cntPriznak AS INT NO-UNDO.
	CheckedDoc = FALSE.
	cntPriznak = 0.
    	IF (iTypeCl = 1 OR iTypeCl = 3) THEN DO: /* �� �� */
	    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + �饬 �� ���㬥��� � 㤮�⮢��塞�� �� �� ������ �� ������" SKIP. &ENDIF
    	    FOR EACH doc-mfr
    	     WHERE doc-mfr.clientid = bank.clients-mfr.id NO-LOCK ON ERROR UNDO, THROW:
    	        tmpStr = convertDoc2BIS( doc-mfr.doctype, doc-mfr.docseria, doc-mfr.docnumber).
    	        FIND FIRST cust-ident
    	         WHERE cust-ident.class-code     EQ 'p-cust-ident'
    	           AND cust-ident.cust-cat       EQ '�'
    	           AND cust-ident.cust-code-type EQ doc-mfr.doctype
    	           AND cust-ident.cust-code      EQ tmpStr
    	         NO-LOCK NO-ERROR.
    	        IF AVAIL cust-ident THEN DO ON ERROR UNDO, THROW:
    	    	    IF AVAIL person AND cust-ident.cust-id NE person.person-id
    	    	     THEN UNDO, THROW NEW Progress.Lang.AppError( "���㬥�� " + doc-mfr.doctype +
    	    		  " " + tmpStr +
    	    		  ' ������ CID=' + STRING(iCid) + ' ������ � ��㣮�� ������ � person-id=' + STRING( iBisId) +
    			  ".").
    		    IF NOT AVAIL person THEN DO ON ERROR UNDO, THROW:
    			FIND FIRST person
    			  WHERE person.person-id = cust-ident.cust-id EXCLUSIVE-LOCK NO-ERROR.
    		        IF NOT AVAIL person AND LOCKED person
    		          THEN UNDO, THROW NEW Progress.Lang.AppError( "������ person-id=" + STRING( cust-ident.cust-id) + ' �������஢�� ��㣨� ���짮��⥫��').
    			IF NOT AVAIL person THEN UNDO, THROW NEW Progress.Lang.AppError(
    			     "����ﭭ�� ������ cust-ident � cust-id=" + STRING( cust-ident.cust-id) + " cust-cat=�").
			&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ��諨 ������ �� ���㬥���" +
			     doc-mfr.doctype + " " + tmpStr SKIP. &ENDIF
			     
			DEF VAR tmpStr2 AS CHAR NO-UNDO.
			tmpStr2 = GetXattrValueEx( "person", STRING( person.person-id), (IF iTypeCl = 1 THEN "CID" ELSE "CIDIP"), STRING(iCid)).
			IF tmpStr2 EQ "-1" THEN tmpStr2 = STRING(iCid).
    			IF AVAIL person
    			  AND tmpStr2 NE STRING(iCid)
    			 THEN UNDO, THROW NEW Progress.Lang.AppError( "���㬥�� " + doc-mfr.doctype +
    			    " " + tmpStr + " ������ � ������ person-id=" + STRING( person.person-id) +
    			    " � CID=" + tmpStr2 /* GetXattrValueEx( "person", STRING( person.person-id), "CID", STRING(iCid)) */ +
    			    " ����� " + STRING(iCid)).
    			iBisId = IF AVAIL person THEN person.person-id ELSE ?.
    		    END.
    		    CheckedDoc = TRUE.
    		END.
    	    END.
    	    IF AVAIL person THEN DO ON ERROR UNDO, THROW:
    		IF NOT CheckedDoc
    	         THEN UNDO, THROW NEW Progress.Lang.AppError( '������ �� ����� ���� ���㬥�⮢ � �����⮬ person-id=' + (IF iBisId <> ? THEN STRING( iBisId) ELSE "?")).
    	        
    	        DEF VAR uii AS INT NO-UNDO.
    	        DO uii = 1 TO NUM-ENTRIES( bank.clients-mfr.fname, ' '):
    	    	    IF NUM-ENTRIES( person.name-last + ' ' + person.first-names, ' ') >= uii AND
    	    		ENTRY( uii, bank.clients-mfr.fname, ' ') EQ ENTRY( uii, person.name-last + ' ' + person.first-names, ' ')
    	    		THEN cntPriznak = cntPriznak + 1.
    	        END.
    	        
    		/* ASSIGN
    		cntPriznak = cntPriznak + 1
    		 WHEN bank.clients-mfr.fname EQ person.name-last + ' ' + person.first-names
    		. */
    		ASSIGN
    		cntPriznak = cntPriznak + 1
    		 WHEN bank.clients-mfr.birthdate = person.birthday
    		.
    		ASSIGN
    		cntPriznak = cntPriznak + 1
    		 WHEN (IF person.gender THEN "�" ELSE "�") EQ Convert2Gender( bank.clients-mfr.fname, bank.clients-mfr.sex)
    		.
    		IF cntPriznak < 4
    		 THEN UNDO, THROW NEW Progress.Lang.AppError( '������ CID=' + STRING(iCid) +
    		  ' �� ����� �����筮 ���� �ਧ����� (���, ��� ஦�����, ���) � person-id=' + STRING( person.person-id)).
    	    END.
        END. ELSE
        IF /* AVAIL cust-corp AND*/ iTypeCl = 0 THEN DO ON ERROR UNDO, THROW: /* �� */
    	    cntPriznak = 0.
    	    IF AVAIL cust-corp THEN DO:
    		ASSIGN
    		cntPriznak = cntPriznak + 1
    		    WHEN bank.clients-mfr.inn EQ cust-corp.inn.
    		ASSIGN
    		cntPriznak = cntPriznak + 1
    		    WHEN bank.clients-mfr.kpp EQ GetXattrValueEx( "cust-corp", STRING( cust-corp.cust-id), "���", "").
    		ASSIGN
    		cntPriznak = cntPriznak + 1
    		    WHEN bank.clients-mfr.ogrn EQ GetXattrValueEx( "cust-corp", STRING( cust-corp.cust-id), "����", "").
    		ASSIGN
    		cntPriznak = cntPriznak + 1
    		    WHEN cust-corp.name-corp EQ bank.clients-mfr.fname.
    		ASSIGN
    		cntPriznak = cntPriznak + 1
        	    WHEN cust-corp.name-short EQ bank.clients-mfr.sname.
    		ASSIGN
    		cntPriznak = cntPriznak + 1
    		    WHEN cust-corp.tax-insp EQ STRING( bank.clients-mfr.taxid).
		/*
		put unformatted "'" bank.clients-mfr.inn "' '" cust-corp.inn "'" SKIP.
		put unformatted "'" bank.clients-mfr.kpp "' '" GetXattrValueEx( "cust-corp", STRING( cust-corp.cust-id), "���", "") "'" SKIP.
		put unformatted "'" bank.clients-mfr.ogrn "' '" GetXattrValueEx( "cust-corp", STRING( cust-corp.cust-id), "����", "") "'" SKIP.
		put unformatted "'" cust-corp.name-corp "' '" bank.clients-mfr.fname "'" SKIP.
		put unformatted "'" cust-corp.name-short "' '" bank.clients-mfr.sname "'" SKIP.
		put unformatted "'" cust-corp.tax-insp "' '" STRING( bank.clients-mfr.taxid) "'" SKIP.
		*/
		IF cntPriznak < 3
    		 THEN UNDO, THROW NEW Progress.Lang.AppError( '������ CID=' + STRING(iCid) +
    	         ' ���� ���� �ਧ����� (���, ���,����,�.����.,��) � cust-id=' + STRING( cust-corp.cust-id)).
    	    END.
    	    /* ������ �� �裡 �� ��諨, �饬 �� ��� � ��� */
    	    IF NOT AVAIL cust-corp THEN DO ON ERROR UNDO, THROW:
    		FIND FIRST cust-corp WHERE cust-corp.inn EQ bank.clients-mfr.inn NO-ERROR.
    		DO WHILE AVAIL cust-corp
    		   ON ERROR UNDO, THROW:
    		    IF bank.clients-mfr.kpp = "" OR bank.clients-mfr.kpp = "0"
    		     OR bank.clients-mfr.kpp EQ GetXattrValueEx( "cust-corp", STRING( cust-corp.cust-id), "���", "")
    		     THEN LEAVE.
    		    FIND NEXT cust-corp WHERE cust-corp.inn EQ bank.clients-mfr.inn NO-ERROR.
    		END.
    		IF AVAIL cust-corp THEN iBisId = cust-corp.cust-id.
    	    END.
    	    
        END.

        /*===== �஢��塞 ���� �� ᨭ�஭���஢��� =====*/
	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + �஢��塞 ���� �� ᨭ�஭���஢���" SKIP. &ENDIF
    	IF AVAIL person AND (iTypeCl = 1 OR iTypeCl = 3) THEN DO ON ERROR UNDO, THROW: /* �� �� */
    	    DEF VAR eText AS CHAR NO-UNDO.
    	    tmpStr = GetXAttrValueEx("person",STRING(iBisId),"syncOmskABS","���").
    	    IF tmpStr <> "��" THEN DO ON ERROR UNDO, THROW:
    		/* �᫨ �� ����� ���� ᮢ������, � �� ��ࢮ��砫쭠� ����㧪� */
    		tmpStr = '��'.
    		eText = "".
    		FIND FIRST doc-mfr
    		 WHERE doc-mfr.clientid = bank.clients-mfr.id
    		   AND TRIM(doc-mfr.doctype) EQ person.document-id
    	           AND doc-mfr.volume EQ 1
    		   NO-LOCK NO-ERROR.
    		IF (NOT AVAIL doc-mfr)
    		   OR person.document <> convertDoc2BIS( TRIM(doc-mfr.doctype), doc-mfr.docseria, doc-mfr.docnumber)
    		   OR person.document-id <> TRIM(doc-mfr.doctype)
    		   THEN DO:
    		    tmpStr = '���'.
    		    eText = (IF eText <> "" THEN eText + ";" ELSE "") +
    			"�� ᮢ��� ���㬥�� ���(" + person.document-id + " " + person.document
    			 + ") � ��� " + TRIM(doc-mfr.doctype) + " "
    			 + convertDoc2BIS( TRIM(doc-mfr.doctype), doc-mfr.docseria, doc-mfr.docnumber)
			.
    		END.
    		   
    		IF UPPER(bank.clients-mfr.fname) <> UPPER(person.name-last + ' ' + person.first-names)
    		 THEN DO:
    		    tmpStr = '���'.
    		    eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    			"�� ᮢ���� ���".
    		END.
    		   
    		IF bank.clients-mfr.birthdate <> person.birthday
    		 THEN DO: 
    		    tmpStr = '���'.
    		    eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    			"�� ᮢ���� ��� ஦�����".
    		END.
    		   
    		IF UPPER(person.country-id) <> UPPER(bank.clients-mfr.countryid)
    		 THEN DO: 
    		    tmpStr = '���'.
    		    eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    			"�� ᮢ���� ��࠭�".
    		END.
    		   
    		IF (bank.clients-mfr.inn NE ? AND bank.clients-mfr.inn NE "" AND person.inn NE ? AND person.inn NE "")
    		    AND bank.clients-mfr.inn <> person.inn
    		 THEN DO: 
    		    tmpStr = '���'. 
    		    eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    			"�� ᮢ��� ��� � ��� " + (IF person.inn EQ ? THEN "?" ELSE person.inn) + " � ��� " + (IF bank.clients-mfr.inn EQ ? THEN "?" ELSE bank.clients-mfr.inn).
    		END.

    		IF (IF person.gender THEN "�" ELSE "�") <> Convert2Gender( bank.clients-mfr.fname, bank.clients-mfr.sex)
    		 THEN DO: 
    		    tmpStr = '���'. 
    		    eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    			"�� ᮢ��� ���".
    		END.
		
    		FIND FIRST adr-mfr
    		 WHERE adr-mfr.clientid = bank.clients-mfr.id
    		   AND adr-mfr.adresstype = '����ய' NO-LOCK NO-ERROR.
    		IF NOT AVAIL adr-mfr THEN DO: 
    		    tmpStr = '���'.
    		    eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    			"��� ���� �ய�᪨".
    		END.
    		
    		ELSE DO ON ERROR UNDO, THROW:
    		    DEF VAR u1 AS CHAR NO-UNDO.
    		    DEF VAR u2 AS CHAR NO-UNDO.
		    strAdress = ConvertAdress2BIS(TRIM(adr-mfr.adress)).
		    u1 = ENTRY(3,strAdress,"|").
		    u2 = person.address[1].
		    /* IF NOT (ENTRY(3,strAdress,"|") BEGINS person.address[1]) */
		    IF ENTRY(5,u1) <> ENTRY(5,u2) OR ENTRY(6,u1) <> ENTRY(6,u2) OR ENTRY(8,u1) <> ENTRY(8,u2) OR ( ENTRY(3,u1) <> "" AND ENTRY(3,u2) <> "" AND ENTRY(3,u1) <> ENTRY(3,u2))
    		     THEN DO:
    		        tmpStr = '���'.
    			eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    		    	    "�� ᮢ��� ���� �ய�᪨ � ��� (" + 
    		    	    (IF person.address[1] EQ ? THEN "?" ELSE person.address[1]) + 
    		    	    ") � ��� (" + ENTRY(3,strAdress,"|") + ")".
    		    END.
    		    
    		    IF ENTRY(2,strAdress,"|") <> ''
    		     AND GetXattrValueEx("person",STRING(person.person-id),"���������","") <> ENTRY(2,strAdress,"|")
    		     THEN DO: 
    		        tmpStr = '���'.
    			eText = (IF eText <> "" THEN eText + ";" ELSE "") + 
    		    	    "�� ᮢ��� ��� ॣ���� " + ENTRY(2,strAdress,"|") + "<>" + GetXattrValueEx("person",STRING(person.person-id),"���������","").
    		    END.
    		END.
    		IF tmpStr <> "��" THEN
    		     UNDO, THROW NEW Progress.Lang.AppError( "�ॡ���� ��筠� ᨭ�஭����� �� (" + eText + ").").
        	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ������ ᮢ������ " + STRING( person.person-id) SKIP. &ENDIF
    	    END.
        END.
    	IF AVAIL cust-corp AND (iTypeCl = 0) THEN DO ON ERROR UNDO, THROW: /* �� */
    	    /* DEF VAR eText2 AS CHAR NO-UNDO. */
    	    tmpStr = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"syncOmskABS","���").
    	    IF tmpStr <> "��" THEN DO ON ERROR UNDO, THROW:
    		 UNDO, THROW NEW Progress.Lang.AppError( "�ॡ���� ��筠� ᨭ�஭����� ��.").
    	    END.
    	END.

	IF iTypeCl = 1 OR iTypeCl = 3 THEN DO ON ERROR UNDO, THROW:

	    /* �஢��塞 ᮢ������� CID */
    	    IF AVAIL person THEN DO:
    		IF iTypeCl = 1 THEN DO:
    		    tmpStr = GetXattrValueEx( "person", STRING( person.person-id),"CID", "-1").
    		    IF tmpStr NE "-1" AND STRING(bank.clients-mfr.cid) NE tmpStr
    		      THEN UNDO, THROW NEW Progress.Lang.AppError( "� ������ person-id=" + STRING( person.person-id) +
    		                " CID <> " + STRING(bank.clients-mfr.cid)).
    		END.
    		IF iTypeCl = 3 THEN DO:
    		    tmpStr = GetXattrValueEx( "person", STRING( person.person-id),"CID", "-1").
    		    IF tmpStr NE "-1" AND bank.clients-mfr.cidpers NE ? AND STRING(bank.clients-mfr.cidpers) NE tmpStr
    		      THEN UNDO, THROW NEW Progress.Lang.AppError( "� ������ person-id=" + STRING( person.person-id) +
    		                " CID <> " + STRING(bank.clients-mfr.cidpers)).
    		    tmpStr = GetXattrValueEx( "person", STRING( person.person-id),"CIDIP", "-1").
    		    IF tmpStr NE "-1" AND STRING(bank.clients-mfr.cid) NE tmpStr
    		      THEN UNDO, THROW NEW Progress.Lang.AppError( "� ������ person-id=" + STRING( person.person-id) +
    		                " CID <> " + STRING(bank.clients-mfr.cid)).
    		END.
    	    END.


	    IF AVAIL person THEN DO:
		/* �� ᮧ����� ������ */
		find first history
		    where history.modify eq 'C'
		      and history.file-name eq 'person'
		      and history.field-ref = string(person.person-id)
		      and history.user-id = 'SYNC'
		    no-lock no-error.
		if (not avail history OR can-do("04*", strBranchId)) AND person.person-id <> 53852 then
    		     UNDO, THROW NEW Progress.Lang.AppError( "���� ᨭ�஭����㥬 ⮫쪮 ��᪨� �����⮢.").
    	    END.
    	    
    	    
    	    
    	    
	END. ELSE
	IF iTypeCl = 0 THEN DO ON ERROR UNDO, THROW:
	    IF AVAIL cust-corp THEN DO:
		/* �� ᮧ����� ������ */
		find first history
		    where history.modify eq 'C'
		      and history.file-name eq 'cust-corp'
		      and history.field-ref = string(cust-corp.cust-id)
		      and history.user-id = 'SYNC'
		    no-lock no-error.
		if not avail history then
    		     UNDO, THROW NEW Progress.Lang.AppError( "���� ᨭ�஭����㥬 ⮫쪮 ��᪨� �����⮢.").
    	    END.
	END. ELSE
    		     UNDO, THROW NEW Progress.Lang.AppError( "���� ᨭ�஭����㥬 ⮫쪮 ��᪨� �����⮢.").

	/* �᫨ ������ ���� � ��� ��� � ���, � �஢��塞 ����稥 ��⮢ � ������ */
	IF bank.clients-mfr.opentime < DATE (10, 1, 2014)
	AND (NOT AVAIL person) AND (NOT AVAIL cust-corp)
	AND iCid <> 176065 AND iCid <> 407562
	 THEN DO:
	    FIND FIRST bank.accounts use-index in_accounts_cid
	     WHERE bank.accounts.cid EQ iCid AND (bank.accounts.clostime EQ ? OR bank.accounts.clostime >= DATE(1,1,2014) ) NO-LOCK NO-ERROR.
	    IF NOT AVAIL bank.accounts AND bank.clients-mfr.cidpers NE ? THEN
	    FIND FIRST bank.accounts use-index in_accounts_cid
	     WHERE bank.accounts.cid EQ bank.clients-mfr.cidpers AND (bank.accounts.clostime EQ ? OR bank.accounts.clostime >= DATE(1,1,2014) ) NO-LOCK NO-ERROR.
	    IF NOT AVAIL bank.accounts THEN DO:
		FIND FIRST guaranty
		 WHERE bank.guaranty.cid EQ iCid AND PRKEY <> 'DF0' NO-LOCK NO-ERROR.
		IF NOT AVAIL guaranty THEN
		    FIND FIRST usr_warranty
		     WHERE usr_warranty.cid = iCid OR CID_TARGET = iCid NO-LOCK NO-ERROR.
		ELSE RELEASE usr_warranty. 
		IF (NOT AVAIL guaranty) AND (NOT AVAIL usr_warranty) /* CAN-FIND( guaranty
		 WHERE bank.guaranty.cid EQ iCid AND PRKEY <> 'DF0' NO-LOCK )*/ THEN
		   UNDO, THROW NEW Progress.Lang.AppError( "������ _CID=" + STRING(iCid) + " �� ����� ������� ��⮢ � �����").
		RELEASE bank.guaranty.
		RELEASE usr_warranty.
	    END.
	    RELEASE bank.accounts.
	END.

	IF iTypeCl = 1 OR iTypeCl = 3
	 THEN clActionStatus = IF AVAIL person THEN 2 ELSE 1.
	IF iTypeCl = 0	
	 THEN clActionStatus = IF AVAIL cust-corp THEN 2 ELSE 1.

	&IF DEFINED( DEBUG-LOG) &THEN ELSE PUT UNFORMATTED "    + �஢��塞 ������" SKIP. &ENDIF
        /*===== �஢��塞 ������ =====*/
        
        FIND FIRST doc-mfr
         WHERE doc-mfr.clientid = bank.clients-mfr.id
           NO-LOCK NO-ERROR.
        IF NOT AVAIL doc-mfr AND (iTypeCl = 1 OR iTypeCl = 3) AND clActionStatus = 1 THEN DO ON ERROR UNDO, THROW:
    	    UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_NOT_FOUND}).
            /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_DOC_NOT_FOUND}, INPUT {&ERROR_DOC_NOT_FOUND}).
            UNDO, LEAVE MYNEXT. */
        END.

        /*
        FIND FIRST adr-mfr
         WHERE adr-mfr.clientid = bank.clients-mfr.id AND adr-mfr.adressvolume = {&VOLUME_UPPER} NO-LOCK NO-ERROR.
        IF NOT AVAIL adr-mfr THEN DO:
            IF clActionStatus = 1 THEN DO:
                IF iTypeCl = 1 OR iTypeCl = 3 THEN DO:  
                    txtErr = ADDERROR(txtErr,{&ERROR_ADR_NOT_FOUND}).
                END.
                ELSE IF iTypeCl = 0 THEN DO: /* �᫨ �ਪ, � ���� ��易⥫��! */
            	    UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_ADR_NOT_FOUND}).
                    /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_ADR_NOT_FOUND}, INPUT {&ERROR_ADR_NOT_FOUND}).
                    UNDO, LEAVE MYNEXT. */
                END.
            END.
            strAdress = ConvertAdress2BIS("").
        END.
        ELSE DO:
            strAdress = ConvertAdress2BIS(TRIM(adr-mfr.adress)).
        END.
        strStatusAdr = ENTRY(1,strAdress,"|").

        /* �� ��㧨� ���� ���������� ���� */
        IF strStatusAdr = "bad" THEN DO ON ERROR UNDO, THROW:
    	    UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_ADR_UPDATE}).
            * RUN SET_ERROR_CLIENT(INPUT {&STATUS_ADR_ERROR}, INPUT {&ERROR_ADR_UPDATE}).
            UNDO, LEAVE MYNEXT. *
        END.
        strKodReg = ENTRY(2,strAdress,"|").
        strAdress = ENTRY(3,strAdress,"|").
        */

	/* �롨ࠥ� �᭮���� ���� ��� ������ */
        strStatusAdr = "notFound".
        FOR EACH adr-mfr WHERE adr-mfr.clientid = bank.clients-mfr.id 
            /* AND (adr-mfr.adresstype = "����ய" OR adr-mfr.adresstype = "�������") */ 
            /* AND adr-mfr.adressvolume = {&VOLUME_UPPER} */ NO-LOCK ON ERROR UNDO, THROW:
            IF (iTypeCl = 0 AND adr-mfr.adresstype = "�����")
              OR ((iTypeCl = 1 OR iTypeCl = 3) AND adr-mfr.adresstype = "����ய") THEN DO:
        	strAdress = ConvertAdress2BIS(TRIM(adr-mfr.adress)).        
		strStatusAdr = ENTRY(1,strAdress,"|").
        	IF strStatusAdr = "bad" THEN NEXT.
        	strKodReg = ENTRY(2,strAdress,"|").
        	strAdress = ENTRY(3,strAdress,"|").
        	LEAVE.
    	    END.
        END.
        IF strStatusAdr = "notFound" THEN
            UNDO, THROW NEW Progress.Lang.AppError( "�� ������ �᭮���� ���� ������").

 /*       IF strStatusAdr = "bad" THEN DO:
            RUN SET_ERROR_CLIENT(INPUT {&STATUS_ADR_ERROR}, INPUT {&ERROR_ADR_UPDATE}).
            UNDO MYNEXT, LEAVE MYNEXT.
        END.
   */     

        IF bank.clients-mfr.birthdate = ?
          AND clActionStatus = 1
          AND (iTypeCl = 1 OR iTypeCl = 3) THEN DO ON ERROR UNDO, THROW:
            UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_BIRTHDAY}).
            /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_BIRTHDAY_ERROR}, INPUT {&ERROR_BIRTHDAY}).
            UNDO MYNEXT, LEAVE MYNEXT. */
        END. 
        ELSE DO ON ERROR UNDO, THROW:
            dateBirthDay = bank.clients-mfr.birthdate.
        END. 
        IF  iTypeCl = 0 THEN DO ON ERROR UNDO, THROW:
            IF bank.clients-mfr.regdate = ? AND clActionStatus = 1 THEN DO ON ERROR UNDO, THROW:
        	dateBirthDay = DATE (1, 1, 1900).
        	/* UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_BIRTHDAY}). */
                /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_BIRTHDAY_ERROR}, INPUT {&ERROR_BIRTHDAY}).
                UNDO MYNEXT, LEAVE MYNEXT. */
            END. 
            ELSE dateBirthDay = bank.clients-mfr.regdate.
        END.      

        IF bank.clients-mfr.opentime = ? AND clActionStatus = 1 THEN DO ON ERROR UNDO, THROW:
            dateOpenTime = TODAY.
        END. 
        ELSE dateOpenTime = bank.clients-mfr.opentime.

        IF bank.clients-mfr.countryid = ? THEN DO ON ERROR UNDO, THROW:
            IF clActionStatus = 1 THEN DO ON ERROR UNDO, THROW:
                txtErr = ADDERROR(txtErr,{&ERROR_COUNTRY_NOT_FOUND}). 
                strCountryId = "rus".
            END.
            ELSE strCountryId = ?.
        END.
        ELSE DO ON ERROR UNDO, THROW:
            strCountryId = TRIM(bank.clients-mfr.countryid).
            FIND FIRST country WHERE country.country-id = strCountryId NO-LOCK NO-ERROR.
            IF NOT AVAIL country THEN DO ON ERROR UNDO, THROW:
                txtErr = ADDERROR(txtErr,{&ERROR_COUNTRY_NOT_FOUND}).
                IF clActionStatus = 1 THEN strCountryId = "RUS".
                ELSE strCountryId = ?.
            END.   
        END.
        IF (bank.clients-mfr.fname = ?
            OR TRIM(bank.clients-mfr.fname) = "")
           AND clActionStatus = 1 THEN DO ON ERROR UNDO, THROW:
           UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_CLIENT_NAME}).
            /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_CLIENT_NAME}, INPUT {&ERROR_CLIENT_NAME}).
            UNDO, LEAVE MYNEXT. */
        END.
        IF bank.clients-mfr.fname <> ? THEN
            strFirstNames = TRIM(bank.clients-mfr.fname).
            ELSE strFirstNames = ?.
        /*
            0 �ਤ��᪮� ���
            1 �����᪮� ���
            2 ����
            3 �������㠫�� �।�ਭ���⥫�
        */
        IF bank.clients-mfr.subjectype = ? OR bank.clients-mfr.subjectype > 3 THEN DO ON ERROR UNDO, THROW:
    	    UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_CLIENT_TYPE}).
            /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_CLIENT_TYPE}, INPUT {&ERROR_CLIENT_TYPE}).
            UNDO, LEAVE MYNEXT. */
        END.   
        IF (iTypeCl = 1 OR iTypeCl = 3) THEN DO ON ERROR UNDO, THROW:
            IF strFirstNames <> ? THEN DO ON ERROR UNDO, THROW:
                tmpInt = INDEX(strFirstNames," "). /* ࠧ��ࠥ� ��ப� ���   - �� �१ �஡��� � ������ */
                IF tmpInt > 0 THEN DO ON ERROR UNDO, THROW:
                    strNameLast = ENTRY(1,strFirstNames," ").
                    IF LENGTH(strFirstNames) > tmpInt THEN
                        strFirstNames = SUBSTRING(strFirstNames,tmpInt + 1).
                    ELSE
                        strFirstNames = "".
                END.
                ELSE DO ON ERROR UNDO, THROW:
                    strNameLast = strFirstNames.
                    strFirstNames = "".
                END.
            END.
            ELSE strNameLast = ?.
    	    IF Convert2Gender( bank.clients-mfr.fname, bank.clients-mfr.sex) EQ ?
        	THEN UNDO, THROW NEW Progress.Lang.AppError( "�� 㪠��� ��� ������ CID=" + STRING( bank.clients-mfr.cid)).
        END. /* IF iTypeCl = 1 OR iTypeCl = 3 THEN DO: */
        ELSE IF iTypeCl = 0 THEN DO ON ERROR UNDO, THROW:
            IF bank.clients-mfr.sname = ? THEN DO ON ERROR UNDO, THROW:
                IF clActionStatus = 1 THEN 
                    strNameLast = "".
                ELSE 
                    strNameLast = ?.
            END.
            ELSE 
                strNameLast = TRIM(bank.clients-mfr.sname).          
        END.

    	bGender = (Convert2Gender( bank.clients-mfr.fname, bank.clients-mfr.sex) EQ "�").

        IF bank.clients-mfr.inn = ? OR TRIM(bank.clients-mfr.inn) = '' THEN DO ON ERROR UNDO, THROW:
            /* IF clActionStatus = 1 THEN DO ON ERROR UNDO, THROW: */
                IF iTypeCl = 0 THEN DO ON ERROR UNDO, THROW: /* �᫨ �ਪ - ��� ��易⥫�� */
            	    UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_INN} + " INN=? ��易⥫��").
                    /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_INN_ERROR}, INPUT {&ERROR_INN} + " INN=? ��易⥫��").
                    UNDO MYNEXT, LEAVE MYNEXT. */
                END.
                strInn = "".
            /* END.
            ELSE strInn = ?. */
        END.
        ELSE DO ON ERROR UNDO, THROW:
            strInn = TRIM(bank.clients-mfr.inn).
            IF IsMyNumber(strInn) <> FALSE OR (LENGTH(strInn) <> 10 AND LENGTH(strInn) <> 12) THEN DO ON ERROR UNDO, THROW: /* �ଠ� ��� */
                IF iTypeCl = 0 THEN DO ON ERROR UNDO, THROW:
            	    UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_INN} + "�� 10 � 12 ������ � ��易⥫��").
                    /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_INN_ERROR}, INPUT {&ERROR_INN} + "�� 10 � 12 ������ � ��易⥫��").
                    UNDO MYNEXT, LEAVE MYNEXT. */
                END.
                /* txtErr = ADDERROR(txtErr,{&ERROR_INN}). */
                strInn = "".
            END.
        END.
        IF bank.clients-mfr.ogrn = ? OR TRIM(bank.clients-mfr.ogrn) = "" THEN DO ON ERROR UNDO, THROW:
            IF iTypeCl = 0 THEN DO ON ERROR UNDO, THROW: /* �᫨ �ਪ - ���� ��易⥫�� */
                IF bank.clients-mfr.cid <> 763 AND bank.clients-mfr.cid <> 419 AND bank.clients-mfr.cid <> 2399 AND bank.clients-mfr.cid <> 9333
                AND bank.clients-mfr.countryid EQ 'RUS'
                 THEN
        	  UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_OGRN}).
                /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_OGRN_ERROR}, INPUT {&ERROR_OGRN}).
                UNDO, LEAVE MYNEXT. */
            END.
        END.
        strOgrn = (IF bank.clients-mfr.ogrn NE ? THEN TRIM(bank.clients-mfr.ogrn) ELSE ?).

        IF bank.clients-mfr.formtype <> ? THEN 
            strFormType = TRIM(bank.clients-mfr.formtype).
        ELSE strFormType = "".
        IF iTypeCl = 0 THEN DO ON ERROR UNDO, THROW: /* �᫨ �ਪ - ��� �।����� ��易⥫�� � �� �ࠢ�筨�� */ 
            FIND FIRST code WHERE code.class EQ '����।�' AND code.parent EQ '����।�' AND code.code = strFormType NO-LOCK NO-ERROR.
            IF NOT AVAIL code THEN DO ON ERROR UNDO, THROW:
        	/*UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_CLIENT_TYPE}).
                 RUN SET_ERROR_CLIENT(INPUT {&STATUS_CLIENT_TYPE}, INPUT {&ERROR_CLIENT_TYPE}).
                UNDO, LEAVE MYNEXT. */
                strFormType = "".
            END. ELSE
            strFormType = TRIM(code.val).
        END.
        
        IF bank.clients-mfr.client_type <> ? THEN 
            strClientType = TRIM(bank.clients-mfr.client_type).
        ELSE strClientType = "".
        IF bank.clients-mfr.cid EQ 88282 THEN strClientType = "�����".
        PUT UNFORMATTED iTypeCl ";" strClientType SKIP.
        IF iTypeCl = 0 THEN DO ON ERROR UNDO, THROW: /* �᫨ �ਪ - (��ꥪ�) ��易⥫�� � �� �ࠢ�筨�� */ 
            FIND FIRST code
             WHERE code.class EQ '��ꥪ�'
               AND code.parent EQ '��ꥪ�'
               AND code.code = strClientType NO-LOCK NO-ERROR.
            IF NOT AVAIL code THEN DO ON ERROR UNDO, THROW:
        	UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_TYPEFORM}).
                /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_TYPEFORM_ERROR}, INPUT {&ERROR_TYPEFORM}).
                UNDO, LEAVE MYNEXT. */
            END.
        END.
        IF bank.clients-mfr.dolruk <> ? AND TRIM(bank.clients-mfr.dolruk) <> "" THEN 
            strDolRuk = TRIM(bank.clients-mfr.dolruk).
        ELSE strDolRuk = {&DOLRUK_IF_EMPTY}. 
       
        DEF VAR iCellPhone AS CHAR NO-UNDO.
        DEF VAR iSmsPhone AS CHAR NO-UNDO.
        iCellPhone = IF bank.clients-mfr.cellphone NE ? THEN bank.clients-mfr.cellphone ELSE "".
        IF bank.clients-mfr.smsphone NE ? AND bank.clients-mfr.smsphone NE "" THEN DO:
    	    iSmsPhone  = bank.clients-mfr.smsphone.
    	END. ELSE DO:
    	    iSmsPhone  = iCellPhone.
    	    iCellPhone = "".
    	END.
        strPhone1 = TRIM( IF bank.clients-mfr.homephone NE ? THEN ENTRY( 1, bank.clients-mfr.homephone) ELSE "") + ','.
        strPhone2 = TRIM( IF bank.clients-mfr.workphone NE ? THEN ENTRY( 1, bank.clients-mfr.workphone) ELSE "") + ',' +
                    TRIM( ENTRY( 1, iSmsPhone)).
        strPhoneOther = "".
        DEF VAR ik AS INT NO-UNDO.
/* put unformatted iSmsPhone "#" strPhone2 SKIP. */
        strPhoneOther = DeDublPhone( DeDublPhone( DeDublPhone( bank.clients-mfr.phone, strPhoneOther),  strPhone1), strPhone2).

        ik = INDEX( bank.clients-mfr.homephone, ',').
        strPhoneOther = DeDublPhone( DeDublPhone( DeDublPhone( SUBSTR(bank.clients-mfr.homephone, IF ik > 0 THEN ik + 1 ELSE 1), strPhoneOther), strPhone1), strPhone2).

        ik = INDEX( bank.clients-mfr.workphone, ',').
        strPhoneOther = DeDublPhone( DeDublPhone( DeDublPhone( SUBSTR(bank.clients-mfr.workphone,IF ik > 0 THEN ik + 1 ELSE 1), strPhoneOther), strPhone1), strPhone2).

        strPhoneOther = DeDublPhone( DeDublPhone( DeDublPhone( iCellPhone, strPhoneOther),  strPhone1), strPhone2).
        ik = INDEX( iSmsPhone, ',').
        strPhoneOther = DeDublPhone( DeDublPhone( DeDublPhone( SUBSTR(iSmsPhone,IF ik > 0 THEN ik + 1 ELSE 1), strPhoneOther),  strPhone1), strPhone2).
        ik = INDEX( bank.clients-mfr.phonestr, ',').
        strPhoneOther = DeDublPhone( DeDublPhone( DeDublPhone( SUBSTR(bank.clients-mfr.phonestr,IF ik > 0 THEN ik + 1 ELSE 1), strPhoneOther),  strPhone1), strPhone2).
        
	/*    IF NUM-ENTRIES(person.phone[1]) < 2
	      THEN person.phone[1]    = ttPERSON.phone-home + ','.
	      ELSE ENTRY(1,person.phone[1]) = replace( ttPERSON.phone-home, ',', ' ').
	    IF NUM-ENTRIES(person.phone[2]) < 2
	      THEN person.phone[2] = ENTRY(1,person.phone[2]) + ',' + replace( ttPERSON.cell-phone, ',', ' ').
	      ELSE ENTRY(2,person.phone[2]) = replace( ttPERSON.cell-phone, ',', ' ').
	    UpdateSigns( "person", STRING(person.person-id), "Document4Date_vid", STRING(ttDOCUM.docum-date,"99/99/9999"), ?). */

        /* IF bank.clients-mfr.phone = ? THEN strPhone = "".
            ELSE DO ON ERROR UNDO, THROW:
                strPhone = TRIM(bank.clients-mfr.phone).
                IF LENGTH(strPhone) > 248 THEN strPhone = SUBSTRING(strPhone,1,248).
            END. */


        IF bank.clients-mfr.taxid = ? THEN DO ON ERROR UNDO, THROW:
            IF clActionStatus = 1 THEN
                strTaxInsp = "".
            ELSE strTaxInsp = ?.
        END.
            ELSE strTaxInsp = STRING(bank.clients-mfr.taxid).
        /* end ������ */
          
        /*===== �஢��塞 ���㬥��� �� �� ���४⭮��� =====*/
        IF iTypeCl = 1 OR iTypeCl = 3 THEN DO ON ERROR UNDO, THROW:
    	FOR EACH doc-mfr
    	 WHERE doc-mfr.clientid = bank.clients-mfr.id NO-LOCK ON ERROR UNDO, THROW:
    	    /* tmpStr = convertDoc2BIS( doc-mfr.doctype, doc-mfr.docseria, doc-mfr.docnumber). */
    	    IF (doc-mfr.doctype = ? OR TRIM(doc-mfr.doctype) = "") AND clActionStatus = 1 THEN DO:
    		IF doc-mfr.volume EQ 1
        	 THEN UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_TYPE}).
        	 ELSE NEXT.
        	/* RUN SET_ERROR_CLIENT(INPUT {&STATUS_DOC_NOT_FOUND}, INPUT {&ERROR_DOC_TYPE}).
        	UNDO, LEAVE MYNEXT. */
    	    END.
    	    strDocumentId = TRIM(doc-mfr.doctype).
    	    FIND FIRST code
    	     WHERE code.class EQ "�������"
    	       AND code.parent EQ "�������"
    	       AND code.code = strDocumentId
    	        NO-LOCK NO-ERROR.
    	    IF NOT AVAIL code THEN DO:
    		IF doc-mfr.volume EQ 1
    		 THEN UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_TYPE} + "(" + strDocumentId + ")").
    		 ELSE NEXT.
            	    /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_DOC_NOT_FOUND}, INPUT {&ERROR_DOC_TYPE}).
            	    UNDO, LEAVE MYNEXT. */
    	    END.
    	    strDocument = convertDoc2BIS( strDocumentId, doc-mfr.docseria, doc-mfr.docnumber).

    	    /* IF doc-mfr.doctype = ? AND bank.clients-mfr.action <> {&ACTION_NEW_FROM_OM} THEN DO:
        	strDocumentId = ?.
    	    END.
    	    ELSE DO:
        	strDocumentId = TRIM(doc-mfr.doctype).
        	FIND FIRST code WHERE  code.class EQ "�������" AND code.parent EQ "�������" AND code.code = strDocumentId NO-LOCK NO-ERROR.
        	IF NOT AVAIL code THEN DO:
        	    IF doc-mfr.volume EQ 1
        	    THEN UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_TYPE}).
        	    ELSE NEXT.
            	    /* RUN SET_ERROR_CLIENT(INPUT {&STATUS_DOC_NOT_FOUND}, INPUT {&ERROR_DOC_TYPE}).
            	    UNDO, LEAVE MYNEXT. */
        	END.
    	    END.
    	    IF doc-mfr.docseria = ?  THEN DO: 
        	IF clActionStatus = 1 THEN
            	    strDocument = "".
        	ELSE strDocument = ?.            
    	    END.
    	    ELSE 
        	strDocument = TRIM(doc-mfr.docseria).
            
    	    IF (doc-mfr.docnumber <> ? AND TRIM(doc-mfr.docnumber) <> "") THEN 
        	strDocument = strDocument + ' ' + TRIM(doc-mfr.docnumber). 
	    */

    	    IF strDocument = "" OR IsCorrectNumberDoc (INPUT strDocumentId, INPUT strDocument) THEN DO:
    		IF doc-mfr.volume EQ 1
    		 THEN UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_NUMBER} + " " + strDocumentId + " " + strDocument).
    		 ELSE NEXT.
        	/* RUN SET_ERROR_CLIENT(INPUT {&STATUS_DOC_ERROR}, INPUT {&ERROR_DOC_NUMBER}).
        	UNDO, LEAVE MYNEXT.           */ 
    	    END.

    	    IF doc-mfr.placedoc = ? OR TRIM(doc-mfr.placedoc) = "" THEN DO: 
        	strIssueCl = "".
        	strIssueDoc = "".
    	    END.
    	    ELSE DO:
        	strIssueCl = TRIM(doc-mfr.placedoc).
        	strIssueDoc = strIssueCl.
    	    END.
    	    IF doc-mfr.dockodpodr <> ? AND TRIM(doc-mfr.dockodpodr) <> "" THEN DO:
        	strKodPodr = TRIM(doc-mfr.dockodpodr).
        	strIssueCl = strIssueCl + ',' + strKodPodr.
    	    END.
    	    ELSE strKodPodr = "".

    	    /* IF strIssueCl = "" THEN txtErr = ADDERROR(txtErr,{&ERROR_ISSUE_EMPTY}). */
    	    
    	    IF doc-mfr.docopendate = ? THEN DO:
    		IF doc-mfr.volume EQ 1 AND doc-mfr.docnumber <> '���' AND doc-mfr.doctype <> "����抠��"
    		THEN UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DATEDOC}).
    		ELSE NEXT.
        	/* RUN SET_ERROR_CLIENT(INPUT {&STATUS_DOC_NOT_FOUND}, INPUT {&ERROR_DATEDOC}).
        	UNDO, LEAVE MYNEXT. */
    	    END.
    	    ELSE DO:
        	dateOpenDoc = doc-mfr.docopendate.
    	    END. 
    	END. /* FOR EACH */
        END. /* IF iTypeCl = 1 OR iTypeCl = 3 THEN DO: */
        /* end ���㬥�� */
           
                 
      IF clActionStatus = 1
         AND (iBisId <= 0 OR iBisId = ?) 
         AND (iTypeCl = 1 OR iTypeCl = 3) THEN DO ON ERROR UNDO, THROW:
         /* ᮧ���� PERSON */
         /* FIND FIRST cust-ident
          WHERE cust-ident.cust-code-type = strDocumentId 
            AND cust-ident.cust-cat = '�'
            AND cust-ident.class-code = 'p-cust-ident'
            AND cust-ident.cust-code = strDocument NO-LOCK NO-ERROR.

         IF NOT AVAIL cust-ident THEN */
         
	FIND FIRST doc-mfr
    	 WHERE doc-mfr.clientid = bank.clients-mfr.id
    	   AND TRIM(doc-mfr.doctype) EQ "��ᯮ��"
    	   AND doc-mfr.volume EQ 1
    	    NO-LOCK NO-ERROR.
	IF NOT AVAIL doc-mfr THEN DO:
	    FIND FIRST doc-mfr
    	     WHERE doc-mfr.clientid = bank.clients-mfr.id
    	       AND doc-mfr.volume EQ 1 AND doc-mfr.docnumber <> '���'
    	        NO-LOCK NO-ERROR.
    	END.
    	
    	IF AVAIL doc-mfr THEN DO ON ERROR UNDO, THROW:
	    RUN MakeIssue IN h_cust (doc-mfr.placedoc, doc-mfr.dockodpodr, OUTPUT strIssueCl).
    	    strDocumentId = TRIM(doc-mfr.doctype).
    	    strDocument = convertDoc2BIS( doc-mfr.doctype, doc-mfr.docseria, doc-mfr.docnumber).
    	    strKodPodr = doc-mfr.dockodpodr.
    	    dateOpenDoc = doc-mfr.docopendate.
	END. ELSE DO ON ERROR UNDO, THROW:
	    UNDO, THROW NEW Progress.Lang.AppError( "� ������ ��� ���㬥�� �� 㬮�砭��").
    	    /* strDocumentId = ?.
    	    strDocument = ?.
    	    strKodPodr = ?.
    	    dateOpenDoc = ?. */
	END.

        CREATE person.
        /* {cust-pri.cr &file=person} */
        ASSIGN
    	    person.person-id = Get-New-Person-Id()
    	    person.name-last = strNameLast
    	    person.first-names = strFirstNames
    	    /*person.address[1] = strAdress*/
    	    person.phone[1] = strPhone1
    	    person.phone[2] = strPhone2
    	    person.country-id = strCountryId
    	    person.document = strDocument
    	    person.document-id = strDocumentId
    	    person.tax-insp = strTaxInsp
    	    person.inn = strInn
    	    person.issue = strIssueCl
    	    person.birthday = dateBirthDay
    	    person.gender = bGender
    	    person.date-in = dateOpenTime
    	    .
        iBisId = person.person-id.
	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ᮧ���� �� person-id=" + STRING( person.person-id) SKIP. &ENDIF
        VALIDATE person.
        bCreated = TRUE.
        UpdateSigns("person",STRING(iBisId),"fromOmskABS","��",?).
        UpdateSigns("person",STRING(iBisId),"syncOmskABS","��",?).
        UpdateSigns("person",STRING(iBisId),"�������","������",?).
        UpdateSigns("person",STRING(iBisId),"Document4Date_vid",STRING(dateOpenDoc,"99.99.9999"),?).
            /* IF strKodReg <> ? AND strKodReg <> "" THEN
                UpdateSigns("person",STRING(iBisId),"������",strKodReg,?).*/
            /*
            0 �ਤ��᪮� ���
            1 �����᪮� ���
            2 ����
            3 �������㠫�� �।�ਭ���⥫�
            */
      END.  /* ᮧ���� PERSON */
      
      ELSE IF clActionStatus = 1 
         AND (iBisId <= 0 OR iBisId = ?) 
         AND iTypeCl = 0 THEN DO ON ERROR UNDO, THROW:
put unformatted icid 1111 skip.
         /* ᮧ���� CUST-CORP */
         IF strOgrn NE ? THEN DO:
put unformatted icid 2222 skip.

           FIND FIRST signs WHERE
            signs.file-name = "cust-corp"
            AND signs.code = "����"
            AND signs.xattr-value = strOgrn 
            NO-LOCK NO-ERROR.
           IF AVAIL signs THEN DO:
            iBisId = INT64(signs.surrogate).
            RUN SET_ERROR_CLIENT(INPUT {&STATUS_DUPLICATE_CLIENT}, INPUT {&ERROR_DUPLICATE_CLIENT}).
            UNDO, LEAVE MYNEXT. 
           END.
         END. ELSE DO:
         
         END.
         CREATE cust-corp.
         /*�᭮���*/
         ASSIGN
         cust-corp.cust-id         = Get-New-CustCorp-Id()
         cust-corp.cust-stat       = strFormType
         cust-corp.name-corp       = strFirstNames
         cust-corp.name-short      = strNameLast
         cust-corp.addr-of-low[1]  = strAdress
         cust-corp.inn             = strInn
         cust-corp.date-in         = dateOpenTime
         cust-corp.country-id      = strCountryId
         cust-corp.tax-insp        = strTaxInsp
         cust-corp.class-code      = "cust-corp" 
         .
         iBisId = cust-corp.cust-id.              
         VALIDATE cust-corp.
         bCreated = TRUE.
         /* RELEASE cust-corp.
         FIND FIRST cust-corp WHERE cust-corp.cust-id = iBisId NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN DO:
            RUN SET_ERROR_CLIENT(INPUT {&STATUS_UNKNOW_ERROR}, INPUT {&ERROR_PERSON_CREATE}).
            UNDO, LEAVE MYNEXT.   
         END. */
         /*�������⥫��*/
         /* ��易⥫�� */
         sUpdateSigns("cust-corp",STRING(iBisId),"fromOmskABS","��",?).
         sUpdateSigns("cust-corp",STRING(iBisId),"syncOmskABS","��",?).
         sUpdateSigns("cust-corp",STRING(iBisId),"RegDate",STRING(dateBirthDay,"99.99.9999"),?).
         sUpdateSigns("cust-corp",STRING(iBisId),"�����",strDolRuk,?).     
         sUpdateSigns("cust-corp",STRING(iBisId),"��ꥪ�",strClientType,?).
         sUpdateSigns("cust-corp",STRING(iBisId),"����",strOgrn,?).
         IF strKodReg <> ? AND strKodReg <> "" THEN DO:
            sUpdateSigns("cust-corp",STRING(iBisId),"���������",strKodReg,?). 
            sUpdateSigns("cust-corp",STRING(iBisId),"������",ConvertRegion ( strKodReg),?).
	 END.
      END. /* ᮧ���� CUST-CORP */
      

    /*===== update ������, ��� ���㬥�⮢, ���ᮢ =====*/
    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ���������� ������, ��� ���㬥�⮢, ���ᮢ" SKIP. &ENDIF
    UpdateClient:
    DO ON ERROR UNDO, THROW:
        IF iTypeCl = 1 OR iTypeCl = 3 THEN DO ON ERROR UNDO, THROW:  /* PERSON */
    	    IF NOT AVAIL person OR iBisId = ? OR iBisId <= 0 or person.person-id <> iBisId
    	     THEN UNDO, THROW NEW Progress.Lang.AppError( '������ �� ������ �� ����⪥ ����������').
    	     
            /* IF iBisId = ? OR iBisId <= 0 THEN DO:  * �஡㥬 ���᪠�� �� ��⨢���� ���㬥��� (volume = 0) *
                IF strDocumentId <> ? AND strDocumentId <> "" AND strDocument <> ? AND strDocument <> "" THEN DO:
                    FIND FIRST cust-ident WHERE cust-ident.cust-code-type = strDocumentId 
                        AND cust-ident.cust-cat = '�' AND cust-ident.class-code = 'p-cust-ident'
                        AND cust-ident.cust-code = strDocument NO-LOCK NO-ERROR.
                    IF AVAIL cust-ident THEN iBisId = cust-ident.cust-id.
                END.
                IF iBisId = ? OR iBisId <= 0 THEN DO:
                    RUN SET_ERROR_CLIENT(INPUT {&STATUS_CLIENT_NOT_FOUND}, INPUT {&ERROR_CLIENT_NOT_FOUND}).
                    UNDO, LEAVE MYNEXT. 
                END.
            END.
            FIND FIRST person WHERE person.person-id = iBisId SHARE-LOCK NO-ERROR.
                IF NOT AVAIL person THEN DO:
                    RUN SET_ERROR_CLIENT(INPUT {&STATUS_CLIENT_NOT_FOUND}, INPUT {&ERROR_CLIENT_NOT_FOUND}).
                    UNDO, LEAVE MYNEXT. 
                END.
            */

            IF bCreated <> TRUE THEN DO ON ERROR UNDO, THROW:
                IF strNameLast <> ? AND TRIM(strNameLast) <> "" AND person.name-last <> strNameLast THEN
                        person.name-last = strNameLast.
                IF strFirstNames <> ? AND TRIM(strFirstNames) <> "" AND person.first-names <> strFirstNames THEN
                        person.first-names = strFirstNames.
                /* IF strAdress <> ? AND TRIM(strAdress) <> "" THEN
                        person.address[1] = strAdress. */
                IF strPhone1 <> ? AND TRIM(strPhone1) <> "" AND person.phone[1] <> strPhone1 THEN
                        person.phone[1] = strPhone1.
                IF strPhone2 <> ? AND TRIM(strPhone2) <> "" AND person.phone[2] <> strPhone2 THEN
                        person.phone[2] = strPhone2.
/* put unformatted "##" + person.phone[2] SKIP. */
                IF strCountryId <> ? AND TRIM(strCountryId) <> "" AND person.country-id <> strCountryId THEN   
                        person.country-id = strCountryId.
                /* IF strDocument <> ? AND TRIM(strDocument) <> "" AND person.document <> strDocument THEN 
                        person.document = strDocument.
                IF strDocumentId <> ? AND TRIM(strDocumentId) <> "" AND person.document-id <> strDocumentId THEN
                        person.document-id = strDocumentId.
                */
                IF strTaxInsp <> ? AND TRIM(strTaxInsp) <> "" AND person.tax-insp <> TRIM(strTaxInsp)
                 THEN person.tax-insp = TRIM(strTaxInsp).

                IF strInn <> ? AND TRIM(strInn) <> "" AND person.inn <> strInn
                 THEN person.inn = strInn.

                IF strIssueCl <> ? AND TRIM(strIssueCl) <> "" AND person.issue <> strIssueCl
                 THEN person.issue = strIssueCl.

                IF dateBirthDay <> ? AND person.birthday <> dateBirthDay THEN
                        person.birthday = dateBirthDay.
                IF bGender <> ? AND person.gender <> bGender THEN
                        person.gender = bGender.
                IF dateOpenTime <> ? AND dateOpenTime <> TODAY THEN
                        person.date-in = dateOpenTime.                        
            END.
            IF iCid <> ? AND iCid <> 0 THEN DO ON ERROR UNDO, THROW:
                    IF iTypeCl = 1 THEN DO:
                	sUpdateSigns("person",STRING(iBisId),"CID",STRING(iCid),?).
            	    END.
                    IF iTypeCl = 3 THEN DO:
                	IF GetXattrValueEx( "person", STRING( iBisId),"CIDIP", "-") NE STRING(iCid) THEN DO:
                	    UpdateSigns("person",STRING(iBisId),"CIDIP",STRING(iCid),?).
                	    bChangeCID = TRUE.
                	END.
                	IF bank.clients-mfr.cidpers NE ? THEN
                	    sUpdateSigns("person",STRING(iBisId),"CID",STRING(bank.clients-mfr.cidpers),?).
            	    END.
            END.
            /* ���������� ����������� ���४����⮢ */
    	    IF iTypeCl = 1 THEN DO:
    		IF person.country-id EQ "RUS" THEN tmpStr = "��". ELSE tmpStr = "���".
    	    END. ELSE DO:
    		tmpStr = "���".
    	    END.
    	    sUpdateSigns("person",STRING(iBisId),"��ꥪ�",tmpStr,?).
    	    sUpdateSigns("person",STRING(iBisId),"�।��",IF iTypeCl = 3 THEN "�।��" ELSE ?,?).

            IF bank.clients-mfr.birthplace <> ?
               AND TRIM(bank.clients-mfr.birthplace) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"birthplace",TRIM(bank.clients-mfr.birthplace),?).
	    /*
            IF bank.clients-mfr.phonestr <> ? AND TRIM(bank.clients-mfr.phonestr) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"����䮭3",TRIM(bank.clients-mfr.phonestr),?).

            IF bank.clients-mfr.homephone <> ? AND TRIM(bank.clients-mfr.homephone) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"phone-home",TRIM(bank.clients-mfr.homephone),?).

            IF bank.clients-mfr.workphone <> ? AND TRIM(bank.clients-mfr.workphone) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"tel",TRIM(bank.clients-mfr.workphone),?).

            IF bank.clients-mfr.cellphone <> ? AND TRIM(bank.clients-mfr.cellphone) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"cell-phone",TRIM(bank.clients-mfr.cellphone),?).
	    */
            sUpdateSigns("person",STRING(iBisId),"����䮭3",TRIM(strPhoneOther),?).

            /* IF bank.clients-mfr.clostime <> ?
               AND person.date-out <> DATE(bank.clients-mfr.clostime)
             THEN person.date-out = DATE(bank.clients-mfr.clostime). */

            IF bank.clients-mfr.pfr <> ? AND TRIM(bank.clients-mfr.pfr) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"NumberPfr",TRIM(bank.clients-mfr.pfr),?).

            IF bank.clients-mfr.kpp <> ? AND TRIM(bank.clients-mfr.kpp) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"���",TRIM(bank.clients-mfr.kpp),?).

            IF bank.clients-mfr.ogrn <> ? AND TRIM(bank.clients-mfr.ogrn) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"����",TRIM(bank.clients-mfr.ogrn),?).   

            IF bank.clients-mfr.okato <> ? AND TRIM(bank.clients-mfr.okato) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"�����-�����",TRIM(bank.clients-mfr.okato),?). 

            IF bank.clients-mfr.okpo <> ? AND TRIM(bank.clients-mfr.okpo) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"okpo",TRIM(bank.clients-mfr.okpo),?).

            IF bank.clients-mfr.okved <> ? AND TRIM(bank.clients-mfr.okved) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"�����",TRIM(bank.clients-mfr.okved),?).

            IF bank.clients-mfr.email <> ? AND TRIM(bank.clients-mfr.email) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"e-mail",TRIM(bank.clients-mfr.email),?).

            IF bank.clients-mfr.grp <> ?
             THEN sUpdateSigns("person",STRING(iBisId),"grp",STRING(bank.clients-mfr.grp),?).

            IF bank.clients-mfr.officeno <> ?
             THEN sUpdateSigns("person",STRING(iBisId),"officeno",STRING(bank.clients-mfr.officeno),?).

            IF bank.clients-mfr.dolruk <> ? AND TRIM(bank.clients-mfr.dolruk) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"�����",TRIM(bank.clients-mfr.dolruk),?).

            IF bank.clients-mfr.director <> ? AND TRIM(bank.clients-mfr.director) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"�����",TRIM(bank.clients-mfr.director),?).

            IF bank.clients-mfr.channel <> ? AND TRIM(bank.clients-mfr.channel) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"������ਢ�",TRIM(bank.clients-mfr.channel),?).

	    /* ��� ॣ����樨 ( ��᢮���� ����, �뤠� ᢨ�. � ॣ����樨) vABSClientRow.REGDATE */
            IF bank.clients-mfr.regdate <> ?
             THEN DO ON ERROR UNDO, THROW:
                sUpdateSigns("person",STRING(iBisId),"��⠂�।",STRING( bank.clients-mfr.regdate, "99/99/9999"),?).
        	sUpdateSigns("person",STRING(iBisId),"��⠎���", STRING( bank.clients-mfr.regdate, "99/99/9999"),?).
	    END.

	    /* �࣠� �뤠�訩 ᢨ��⥫��⢮ � ॣ����樨 */
            IF bank.clients-mfr.orgreg <> ? AND TRIM(bank.clients-mfr.orgreg) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"�࣑����।",TRIM(bank.clients-mfr.orgreg),?).

	    /* ���� �뤠� ᢨ��⥫��⢠ � ॣ����樨 */
            IF bank.clients-mfr.placereg <> ? AND TRIM(bank.clients-mfr.placereg) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"���③���।",TRIM(bank.clients-mfr.placereg),?).

	    strSerNal = (IF bank.clients-mfr.sernal <> ? AND TRIM(bank.clients-mfr.sernal) <> ""
	     THEN TRIM(bank.clients-mfr.sernal) ELSE '').
            strNumNal = (IF bank.clients-mfr.numnal <> ? AND TRIM(bank.clients-mfr.numnal) <> "" 
             THEN TRIM(bank.clients-mfr.numnal) ELSE '').
            /* ����� ᢨ��⥫��⢠ � ॣ����樨 */
            IF bank.clients-mfr.sersvidreg + "," + bank.clients-mfr.numsvidreg <> ','
            AND bank.clients-mfr.sersvidreg + "," + bank.clients-mfr.numsvidreg <> ?
             THEN sUpdateSigns("person",STRING(iBisId),"�焮���", bank.clients-mfr.sersvidreg + "," + bank.clients-mfr.numsvidreg, ?).


                /* IF bank.clients-mfr.highorg <> ? AND TRIM(bank.clients-mfr.highorg) <> ""
                 THEN
                    UpdateSigns("cust-corp",STRING(iBisId),"�࣓�ࠢ",TRIM(bank.clients-mfr.highorg),?). */
                /*IF bank.clients-mfr.sumreg <> ? THEN
                    UpdateSigns("cust-corp",STRING(iBisId),"��⠢���",STRING(bank.clients-mfr.sumreg,,  "->,>>>,>>>,>>9.99"),?). */


	    /* ᢨ��⥫��⢮ � ���⠭���� �� ��� */
	    strSerNal = (IF bank.clients-mfr.sernal <> ? AND TRIM(bank.clients-mfr.sernal) <> ""
	     THEN TRIM(bank.clients-mfr.sernal) ELSE '').
            strNumNal = (IF bank.clients-mfr.numnal <> ? AND TRIM(bank.clients-mfr.numnal) <> "" 
             THEN TRIM(bank.clients-mfr.numnal) ELSE '').
            IF strSerNal <> '' AND strNumNal <> ''
             THEN sUpdateSigns("person",STRING(iBisId),"�焮�", strSerNal + ',' + strNumNal,?).
            IF bank.clients-mfr.datenal <> ?
             THEN sUpdateSigns( "person", STRING(iBisId), "�焮����", STRING(bank.clients-mfr.datenal, "99/99/9999"),?).
            /* ELSE sUpdateSigns("person",STRING(iBisId),"�焮����",?,?). */
            /* �஢��� �᪠ � �業�� �᪠ */
            IF bank.clients-mfr.risklevel <> ? THEN
                    sUpdateSigns("person",STRING(iBisId),"��᪎��",IF bank.clients-mfr.risklevel > 0 THEN "��᮪��" ELSE "", ?).
            IF bank.clients-mfr.riskosnov <> ? AND TRIM(bank.clients-mfr.riskosnov) <> "" THEN
                    sUpdateSigns("person",STRING(iBisId),"�業����᪠",TRIM(bank.clients-mfr.riskosnov),?).
            /* IF bank.clients-mfr.vidbsns <> ? AND TRIM(bank.clients-mfr.vidbsns) <> ""
             THEN sUpdateSigns("person",STRING(iBisId),"�������",TRIM(bank.clients-mfr.vidbsns),?). */
            IF bank.clients-mfr.vidbsns <> ? AND TRIM(bank.clients-mfr.vidbsns) <> "" THEN
                    sUpdateSigns("person",STRING(iBisId),"�ᭂ��넥��",TRIM(bank.clients-mfr.vidbsns),?).


	    /* �� ᬥ�� ��� ���塞 � ஫� ������ short-name � cust-name */
	    FOR EACH cust-role
             WHERE cust-role.cust-cat = "�"
               AND cust-role.cust-id  = STRING( person.person-id) ON ERROR UNDO, THROW:
                IF person.name-last + " " + person.first-names <> cust-role.short-name OR
                  person.name-last + " " + person.first-names <> cust-role.cust-name THEN DO ON ERROR UNDO, THROW:
            	    IF person.name-last + " " + person.first-names <> cust-role.short-name
            	     THEN ASSIGN cust-role.short-name = person.name-last + " " + person.first-names.
            	    IF person.name-last + " " + person.first-names <> cust-role.cust-name
            	     THEN ASSIGN cust-role.cust-name = person.name-last + " " + person.first-names.
        	    VALIDATE cust-role.
        	END.
	    END.
	    
            sUpdateSigns("person",STRING(iBisId),"syncOmskABS","��",?).
            
            
            sUpdateSigns("person",STRING(iBisId),"��⠏஢����", STRING(Today,"99/99/9999"),?). 
                               

        END. /*         IF iTypeCl = 1 OR iTypeCl = 3 THEN DO:   �饬 PERSON  */ 

        ELSE IF iTypeCl = 0 THEN DO:  /* CUST-CORP */
            IF iBisId = ? OR iBisId <= 0 THEN DO:  /* �஡㥬 ���᪠�� �� INN � ����� */
                IF strInn <> ? AND strFirstNames <> ? AND strInn <> "" AND strFirstNames <> "" THEN DO:
                    FIND FIRST cust-corp WHERE
                        cust-corp.name-corp = strFirstNames
                        AND cust-corp.inn = strInn 
                        NO-LOCK NO-ERROR.
                    IF AVAIL cust-corp THEN iBisId = cust-corp.cust-id.
                END.
                IF iBisId = ? OR iBisId <= 0 THEN DO:
                    RUN SET_ERROR_CLIENT(INPUT {&STATUS_CLIENT_NOT_FOUND}, INPUT {&ERROR_CLIENT_NOT_FOUND}).
                    UNDO, LEAVE MYNEXT. 
                END.
            END.
            FIND FIRST cust-corp WHERE cust-corp.cust-id = iBisId SHARE-LOCK NO-ERROR.
            IF NOT AVAIL cust-corp THEN DO:
                RUN SET_ERROR_CLIENT(INPUT {&STATUS_CLIENT_NOT_FOUND}, INPUT {&ERROR_CLIENT_NOT_FOUND}).
                UNDO, LEAVE MYNEXT. 
            END.
            IF bCreated <> TRUE THEN DO:
                IF strFirstNames <> ? AND TRIM(strFirstNames) <> ""
                 AND cust-corp.name-corp <> strFirstNames
                 THEN cust-corp.name-corp = strFirstNames.
                IF strClientType <> ? AND TRIM(strFormType) <> ""
                 AND cust-corp.cust-stat <> strFormType
                 THEN cust-corp.cust-stat = strFormType.
                IF strNameLast <> ? AND TRIM(strNameLast) <> ""
                 AND cust-corp.name-short <> strNameLast
                 THEN cust-corp.name-short = strNameLast.
                IF strInn <> ? AND TRIM(strInn) <> ""
                 AND cust-corp.inn <> strInn
                 THEN cust-corp.inn = strInn. 
                IF dateBirthDay <> ? THEN DO:
                    sUpdateSigns("cust-corp",STRING(iBisId),"RegDate",STRING(dateBirthDay,"99.99.9999"),?).
                    sUpdateSigns("cust-corp",STRING(iBisId),"��⠎���",STRING(dateBirthDay,"99.99.9999"),?).
                END.
                IF dateOpenTime <> ?
                 AND cust-corp.date-in <> dateOpenTime
                 THEN cust-corp.date-in = dateOpenTime.
                IF strCountryId <> ? AND TRIM(strCountryId) <> ""
                 AND cust-corp.country-id <> strCountryId
                 THEN cust-corp.country-id = strCountryId.
                IF strTaxInsp <> ? AND TRIM(strTaxInsp) <> ""
                 AND cust-corp.tax-insp <> strTaxInsp
                 THEN cust-corp.tax-insp = strTaxInsp.
                IF strDolRuk <> ? AND TRIM(strDolRuk) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"�����",strDolRuk,?).
                IF strClientType <> ? AND TRIM(strClientType) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"��ꥪ�",strClientType,?).
                IF strOgrn <> ? AND TRIM(strOgrn) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"����",TRIM(strOgrn),?).   
            END.
            IF iCid <> ? AND iCid <> 0
                 THEN sUpdateSigns("cust-corp",STRING(iBisId),"CID",STRING(iCid),?).

            /* ���������� ����������� ���४����⮢ */
    	    IF bCreated THEN
                IF strBranchId <> ? AND TRIM(strBranchId) <> "" THEN DO:
                    IF LENGTH(strBranchId) = 3 THEN strBranchId = '0' + strBranchId.
                    sUpdateSigns("cust-corp",STRING(iBisId),"branch-id",strBranchId,?).
                END.
            IF bank.clients-mfr.birthplace <> ? AND TRIM(bank.clients-mfr.birthplace) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"RegPlace",TRIM(bank.clients-mfr.birthplace),?).
            IF bank.clients-mfr.Phone <> ? AND TRIM(bank.clients-mfr.Phone) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"tel",TRIM(bank.clients-mfr.Phone),?).
            IF bank.clients-mfr.workphone <> ? AND TRIM(bank.clients-mfr.workphone) <> ""
                 AND cust-corp.fax <> TRIM(bank.clients-mfr.workphone)
                 THEN cust-corp.fax = TRIM(bank.clients-mfr.workphone).
            IF bank.clients-mfr.clostime <> ?
                 AND cust-corp.date-out <> DATE(bank.clients-mfr.clostime)
                 THEN cust-corp.date-out = DATE(bank.clients-mfr.clostime).  
            IF bank.clients-mfr.kpp <> ? AND TRIM(bank.clients-mfr.kpp) <> ""
                 THEN sUpdateSigns("cust-corp",STRING(iBisId),"���",TRIM(bank.clients-mfr.kpp),?).
            IF bank.clients-mfr.okato <> ? AND TRIM(bank.clients-mfr.okato) <> ""
                 THEN sUpdateSigns("cust-corp",STRING(iBisId),"�����-�����",TRIM(bank.clients-mfr.okato),?).
            IF bank.clients-mfr.okpo <> ? AND TRIM(bank.clients-mfr.okpo) <> ""
                 AND cust-corp.okpo <> TRIM(bank.clients-mfr.okpo)
                 THEN cust-corp.okpo = TRIM(bank.clients-mfr.okpo).  
            IF bank.clients-mfr.okved <> ? AND TRIM(bank.clients-mfr.okved) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"�����",TRIM(bank.clients-mfr.okved),?).
            IF bank.clients-mfr.email <> ? AND TRIM(bank.clients-mfr.email) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"e-mail",TRIM(bank.clients-mfr.email),?).                     
            IF bank.clients-mfr.grp <> ? THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"grp",STRING(bank.clients-mfr.grp),?).                    
            IF bank.clients-mfr.officeno <> ? THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"officeno",STRING(bank.clients-mfr.officeno),?).
            IF bank.clients-mfr.director <> ? AND TRIM(bank.clients-mfr.director) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"�����",TRIM(bank.clients-mfr.director),?).                    
            IF bank.clients-mfr.bkeeper <> ? AND TRIM(bank.clients-mfr.bkeeper) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"������",TRIM(bank.clients-mfr.bkeeper),?).   
            IF bank.clients-mfr.orgreg <> ? AND TRIM(bank.clients-mfr.orgreg) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"�࣑����।",TRIM(bank.clients-mfr.orgreg),?). 
            IF bank.clients-mfr.placereg <> ? AND TRIM(bank.clients-mfr.placereg) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"���③���।",TRIM(bank.clients-mfr.placereg),?). 
            IF bank.clients-mfr.highorg <> ? AND TRIM(bank.clients-mfr.highorg) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"�࣓�ࠢ",TRIM(bank.clients-mfr.highorg),?).
            IF bank.clients-mfr.sumreg <> ? THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"��⠢���",STRING(bank.clients-mfr.sumreg, "->,>>>,>>>,>>9.99"),?).                    
            IF bank.clients-mfr.risklevel <> ? THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"��᪎��",IF bank.clients-mfr.risklevel > 0 THEN "��᮪��" ELSE "", ?).
            IF bank.clients-mfr.riskosnov <> ? AND TRIM(bank.clients-mfr.riskosnov) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"�業����᪠",TRIM(bank.clients-mfr.riskosnov),?).
            /* IF bank.clients-mfr.vidbsns <> ? AND TRIM(bank.clients-mfr.vidbsns) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"��撨�",TRIM(bank.clients-mfr.vidbsns),?). */
            IF bank.clients-mfr.vidbsns <> ? AND TRIM(bank.clients-mfr.vidbsns) <> "" THEN
                    sUpdateSigns("cust-corp",STRING(iBisId),"�ᭂ��넥��",TRIM(bank.clients-mfr.vidbsns),?).
                    
                    
                    

                /* ᢨ��⥫��⢮ � ���⠭���� �� ��� */
                IF bank.clients-mfr.sernal <> ? AND TRIM(bank.clients-mfr.sernal) <> ""
                 THEN strSerNal = TRIM(bank.clients-mfr.sernal).
                 ELSE strSerNal = ''.
                IF bank.clients-mfr.numnal <> ? AND TRIM(bank.clients-mfr.numnal) <> "" THEN
                    strNumNal = TRIM(bank.clients-mfr.numnal).
                ELSE strNumNal = ''.                 
                IF strSerNal <> '' AND strNumNal <> '' THEN 
                    sUpdateSigns("cust-corp",STRING(iBisId), "�焮�", strSerNal + ',' + strNumNal,?).
                IF bank.clients-mfr.datenal <> ?
                 THEN sUpdateSigns("cust-corp",STRING(iBisId), "�焮����", STRING(bank.clients-mfr.datenal, "99/99/9999"),?).
                 /* ELSE sUpdateSigns("cust-corp",STRING(iBisId), "�焮����", ?, ?). */

                sUpdateSigns("cust-corp",STRING(iBisId),"syncOmskABS","��",?).       
                
                           
                sUpdateSigns("cust-corp",STRING(iBisId),"��⠏஢����", STRING(Today,"99/99/9999"),?).
    
                
                
                
        END. /* ELSE IF iTypeCl = 0 THEN DO:  CUST-CORP */
        


	/* ஫� ������ � 䨫���� */
        IF bCreated THEN DO ON ERROR UNDO, THROW:
                	FIND FIRST cust-role
                	 WHERE cust-role.class-code = "ImaginClient"
                	   AND cust-role.cust-cat = strCustCat
                	   AND cust-role.cust-id  = (IF strCustCat EQ "�" THEN STRING( person.person-id) ELSE STRING( cust-corp.cust-id))
                	   AND cust-role.file-name = "branch"
                	   NO-ERROR.
            		IF AVAIL cust-role AND cust-role.surrogate NE strBranchId THEN DO ON ERROR UNDO, THROW:
            		    ASSIGN
                		cust-role.surrogate  = strBranchId
                		cust-role.open-date  = (IF strCustCat EQ "�" THEN person.date-in ELSE cust-corp.date-in)
                		cust-role.short-name = (IF strCustCat EQ "�" THEN person.name-last + " " + person.first-names ELSE cust-corp.name-short)
                	    .
            		    VALIDATE cust-role.
            		    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ��ࠢ��� 䨫��� " + strBranchId SKIP. &ENDIF
            		END.
        END. ELSE DO ON ERROR UNDO, THROW:
                	FIND FIRST cust-role
                	 WHERE cust-role.class-code = "ImaginClient"
                	   AND cust-role.cust-cat = strCustCat
                	   AND cust-role.cust-id  = (IF strCustCat EQ "�" THEN STRING( person.person-id) ELSE STRING( cust-corp.cust-id))
                	   AND cust-role.file-name = "branch"
                	   AND cust-role.surrogate = strBranchId
                	   NO-ERROR.
        END.
        IF NOT AVAIL cust-role THEN DO ON ERROR UNDO, THROW:
            CREATE cust-role.
            ASSIGN
                	cust-role.class-code = "ImaginClient"
                	cust-role.cust-cat = strCustCat
                	cust-role.cust-id  = (IF strCustCat EQ "�" THEN STRING( person.person-id) ELSE STRING( cust-corp.cust-id))
                	cust-role.file-name = "branch"
                	cust-role.surrogate = strBranchId
                	cust-role.open-date = (IF strCustCat EQ "�" THEN person.date-in ELSE cust-corp.date-in)
                	cust-role.short-name = (IF strCustCat EQ "�" THEN person.name-last + " " + person.first-names ELSE cust-corp.name-short)
                	.
            VALIDATE cust-role.
            /* RUN SetClientRole IN h_cust (STRING(BUFFER person:HANDLE), "�", YES) NO-ERROR. */
            &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + �������� 䨫��� " + strBranchId SKIP. &ENDIF
        END.

        /*=========== ���������� ���㬥�⮢ ============ */
        DEF VAR mainDocRecId AS RECID NO-UNDO.
        mainDocRecId = ?.
        IF strCustCat EQ "�" THEN
        FOR EACH doc-mfr
         WHERE doc-mfr.clientid = bank.clients-mfr.id
           AND doc-mfr.volume = 1 AND doc-mfr.docnumber <> '���'
             NO-LOCK /* BY doc-mfr.docopendate
             BY doc-mfr.doctype BY doc-mfr.docseria by doc-mfr.docnumber
              DESCENDING */ ON ERROR UNDO, THROW:
            MYNEXTDOC:
                  DO ON ERROR UNDO, THROW:
                    IF doc-mfr.doctype = ?  THEN
                        UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_TYPE} + " IDDOC = " + STRING(doc-mfr.id) + " DOCTYPE=?"). 

                    strDocumentId = TRIM(doc-mfr.doctype).
                    FIND FIRST code
                         WHERE  code.class EQ "�������"
                           AND code.parent EQ "�������"
                           AND code.code = strDocumentId NO-LOCK NO-ERROR.
                    IF NOT AVAIL code THEN
                            UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_TYPE} + " IDDOC = " + STRING(doc-mfr.id) + " DOCTYPE �� ������ � �����䨪���" ). 

    	    	    strDocument = convertDoc2BIS( doc-mfr.doctype, doc-mfr.docseria, doc-mfr.docnumber).
                    
                    IF strDocument = "" THEN
                        UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DOC_NUMBER} + " IDDOC = " + STRING(doc-mfr.id) + "DOCUMENT=''" ). 
                    /* IF doc-mfr.placedoc = ? OR TRIM(doc-mfr.placedoc) = "" THEN
                        UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_ISSUE_EMPTY} + " IDDOC = " + STRING(doc-mfr.id) ). */
                    strIssueDoc = (IF doc-mfr.placedoc = ? THEN "" ELSE TRIM(doc-mfr.placedoc)).

                    /*
                    IF doc-mfr.dockodpodr <> ? AND TRIM(doc-mfr.dockodpodr) <> "" THEN DO:
                        strKodPodr = TRIM(doc-mfr.dockodpodr).
                        strIssueCl = strIssueCl + ',' + strKodPodr.
                    END.
                    ELSE strKodPodr = "".
                    IF strIssueCl = "" THEN txtErr = ADDERROR(txtErr,{&ERROR_ISSUE_EMPTY}). 
                    */

                    IF doc-mfr.docopendate = ? THEN
    		       /*IF doc-mfr.volume EQ 1 AND doc-mfr.docnumber <> '���' AND doc-mfr.doctype <> "����抠��"
                        THEN*/ UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_DATEDOC} + " IDDOC = " + STRING(doc-mfr.id) ).
                        /*ELSE NEXT.*/
                    dateOpenDoc = doc-mfr.docopendate.

                    FIND FIRST cust-ident WHERE cust-ident.cust-cat       = strCustCat
                        AND cust-ident.cust-id        = iBisId
                        AND cust-ident.class-code     = "p-cust-ident"
                        AND cust-ident.cust-code-type = strDocumentId
                        AND cust-ident.cust-code      = strDocument
                         SHARE-LOCK NO-ERROR.

                    IF AVAIL cust-ident /* AND cust-ident.cust-code = strDocument AND cust-ident.open-date = dateOpenDoc */
                     THEN DO ON ERROR UNDO, THROW:
                        /* update */
                        ASSIGN
                            cust-ident.open-date      = dateOpenDoc
                            cust-ident.issue          = strIssueDoc
                    	    /* �᫨ ���㬥�� �� ����, ��� �㦭� ᭮�� ᤥ���� ࠡ�稬 */
                            cust-ident.close-date = ? WHEN cust-ident.close-date NE ?
                            .
                        IF dateCloseDoc <> ? THEN cust-ident.close-date = dateCloseDoc.
                        VALIDATE cust-ident.
			sUpdateSigns( cust-ident.class-code,
                                GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                                "���ࠧ�", doc-mfr.dockodpodr, ?).
                    END.
                    ELSE DO ON ERROR UNDO, THROW:
                            CREATE cust-ident NO-ERROR.
                            ASSIGN
                                cust-ident.cust-cat       = strCustCat
                                cust-ident.cust-id        = iBisId
                                cust-ident.cust-code-type = strDocumentId
                                cust-ident.cust-type-num  = ?
                                cust-ident.cust-code      = strDocument
                                cust-ident.open-date      = dateOpenDoc
                                cust-ident.issue          = strIssueDoc
                                cust-ident.class-code     = "p-cust-ident"
                            .
                            IF dateCloseDoc <> ? THEN cust-ident.close-date = dateCloseDoc.
                            
			    sUpdateSigns( cust-ident.class-code,
                                GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                                "���ࠧ�", doc-mfr.dockodpodr, ?).
                            DO ON ERROR UNDO, THROW:
                        	VALIDATE cust-ident.
			    CATCH eAnyError AS Progress.Lang.Error:
				UNDO, THROW NEW Progress.Lang.AppError( "VALIDATE:" + RETURN-VALUE + " " + eAnyError:GetMessage(1)).
			        /* txtErr = (IF txtErr <> "" THEN txtErr + ';' ELSE '') + RETURN-VALUE + " " + eAnyError:GetMessage(1) */.
			    END CATCH.
			    END.
		    END.
                    /* ���� ������� �।��騥 ���㬥��� ⠪��� �� ⨯� */
                    DEF BUFFER bcust-ident FOR cust-ident.
                    FOR EACH bcust-ident 
                         WHERE bcust-ident.cust-cat = cust-ident.cust-cat
                           AND bcust-ident.cust-id  = cust-ident.cust-id
                           AND bcust-ident.cust-code-type = cust-ident.cust-code-type
                           AND bcust-ident.class-code     = "p-cust-ident"
                           AND ROWID(cust-ident) <> ROWID(bcust-ident) ON ERROR UNDO, THROW:
                            ASSIGN
                        	bcust-ident.close-date = cust-ident.open-date
                        	.
                    END.
                    IF mainDocRecId EQ ? THEN mainDocRecId = RECID( cust-ident).
                    IF cust-ident.cust-code-type EQ "��ᯮ��" THEN mainDocRecId = RECID( cust-ident).
                  END.  /* MYNEXTDOC: DO: */
       END. /* FOR EACH doc-mfr WHERE doc-mfr.clientid = bank.clients-mfr.id NO-LOCK: */

       /* ������塞 ������ ������ �� �᭮����� ���㬥��� */
       IF mainDocRecId NE ? AND AVAIL person THEN DO:
    	    FIND FIRST cust-ident WHERE RECID(cust-ident) EQ mainDocRecId NO-LOCK.
    	    ASSIGN
    		person.document-id = cust-ident.cust-code-type WHEN person.document-id NE cust-ident.cust-code-type
    		person.document    = cust-ident.cust-code      WHEN person.document    NE cust-ident.cust-code
    		.
    	    sUpdateSigns("person",STRING(iBisId),"Document4Date_vid",STRING(cust-ident.open-date,"99.99.9999"),?).
    	    VALIDATE person.
       END.
        
        /*=========== ���������� ���ᮢ ============ */
	DEFINE VAR vAddrMainType AS CHAR  NO-UNDO INIT ?.
	DEFINE VAR vAdrCntXattr  AS CHAR  NO-UNDO.
	RUN GetTypeMainAdr ( strCustCat, OUTPUT vAddrMainType, OUTPUT vAdrCntXattr).
    	IF iBisId <> ? AND iBisId > 0 THEN DO ON ERROR UNDO, THROW:
    	    FOR EACH adr-mfr
    	     WHERE adr-mfr.clientid = bank.clients-mfr.id
    	      /* AND adr-mfr.adressvolume = {&VOLUME_UPPER} */
    	      NO-LOCK ON ERROR UNDO, THROW:
    	        DEF VAR strKodRegOKATO AS CHAR NO-UNDO.
    	        DEF VAR ii AS INt NO-UNDO.
    	        DEF VAR vCountry-id AS CHAR NO-UNDO.

    	        strAdressType = adr-mfr.adresstype.
    	        dateAdressOpen = adr-mfr.adressopen.
    	        IF dateAdressOpen = ? THEN dateAdressOpen = TODAY.
    	        dateAdressClose = adr-mfr.adressclose. 
    	        strAdress = ConvertAdress2BIS(TRIM(adr-mfr.adress)).
    	        strStatusAdr = ENTRY(1,strAdress,"|").
    	        /* �� ��㧨� ���� ���������� ���� */
    	        IF strStatusAdr = "bad"
    	         THEN UNDO, THROW NEW Progress.Lang.AppError( {&ERROR_ADR_UPDATE} + " IDADR = " + STRING(adr-mfr.id) ).
        	strKodReg = ENTRY(2,strAdress,"|").
        	strAdress = ENTRY(3,strAdress,"|").
        	strKodRegOKATO = ConvertRegion ( strKodReg).
    	        vCountry-id = IF strKodRegOKATO NE ? AND LENGTH( strKodRegOKATO) > 1 THEN "RUS" ELSE "".
        	/* ��⮢�� ⠡��� *
        	ii = INTEGER( strKodReg) NO-ERROR.
        	DO ii = 1 TO 99:
        	    PUT UNFORMATTED
        		GetCodeName( "���������", (IF ii < 10 THEN "0" ELSE "" ) + STRING( ii)) " "
        		GetCodeName( "������", ENTRY( ii,GNI2OKATO))
        		SKIP.
        	END.
        	*/

		/* ��� ���� ����� ���� ���� � ���, � � ���� 㦥 ���� ����� ���� */
		IF NOT bCreated THEN DO:
            	    FIND FIRST cust-ident USE-INDEX cust
            	     WHERE cust-ident.class-code     = "p-cust-adr"
            	       AND cust-ident.cust-cat       = strCustCat
            	       AND cust-ident.cust-id        = iBisId
            	       AND cust-ident.cust-code-type = strAdressType
            	       AND cust-ident.issue          = strAdress
            	       AND cust-ident.close-date     NE ?
            	       NO-LOCK NO-ERROR.
            	    IF AVAIL cust-ident THEN DO:
                	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ���� " + strAdressType + " 㦥 �� � ������, ����୮ �� ���� � � ��� ����� �����." SKIP. &ENDIF
            		RELEASE cust-ident.
            		NEXT.
            	    END.
		END.
		
		/* �饬 ⥪�騩 ���� �⮣� ⨯� */
                FIND LAST cust-ident USE-INDEX cust
                 WHERE cust-ident.class-code     = "p-cust-adr"
                   AND cust-ident.cust-cat       = strCustCat
                   AND cust-ident.cust-id        = iBisId
                   AND cust-ident.cust-code-type = strAdressType
                   AND cust-ident.close-date     EQ ?
                   EXCLUSIVE-LOCK NO-ERROR.

		/* �� �� ��ଠ��� ��ਠ�� � ��⮩ ॣ����樨 ����
                FIND FIRST cust-ident USE-INDEX cust
                 WHERE cust-ident.class-code     = "p-cust-adr"
                   AND cust-ident.cust-cat       = strCustCat
                   AND cust-ident.cust-id        = iBisId
                   AND cust-ident.cust-code-type = strAdressType
                   AND cust-ident.cust-code      = STRING(YEAR (dateAdressOpen),"9999") +
                                                   STRING(MONTH(dateAdressOpen),"99")   +
                                                   STRING(DAY  (dateAdressOpen),"99")
                   AND cust-ident.open-date      = dateAdressOpen
                   EXCLUSIVE-LOCK NO-ERROR. *
                 * �᫨ ��ப� � ���ᮬ ᮢ������, �� ॣ��� �� ���⠢���, � ���� �� �ண��� �⮡� �� ������ ॣ��� � ��� *
		IF AVAIL cust-ident AND cust-ident.issue EQ strAdress
		 AND (strKodReg EQ "" OR strKodReg EQ ?) THEN DO ON ERROR UNDO, THROW:
		    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ��ப� ���� ᮢ������, �� � ��� ��� ॣ����, ��⠢�� ����� ���." SKIP. &ENDIF
		    NEXT.
		END. */

                IF NOT AVAIL cust-ident OR
                    cust-ident.issue NE strAdress OR
            	    GetXattrValueEx( "cust-ident", GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
            		"country-id", "") NE vCountry-id OR
            	    GetXattrValueEx( "cust-ident", GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
            		"���������", "") NE strKodReg OR
            	    GetXattrValueEx( "cust-ident", GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
            		"������", "") NE strKodRegOKATO
                 THEN DO:

                    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ��� ��ண� ���� " + strAdressType + " ��� �� �� ᮢ������." SKIP. &ENDIF
                    /* IF AVAIL cust-ident THEN DO ON ERROR UNDO, THROW:
                	PUT UNFORMATTED
                	"cust-ident.issue=" + cust-ident.issue + "#" SKIP
                	"strAdress=" + strAdress + "#" SKIP
            		"country-id=" + GetXattrValueEx( "cust-ident", GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
            		"country-id", "") + "#" SKIP
            		"vcountry-id=" + vCountry-id + "#" SKIP
            		GetXattrValueEx( "cust-ident", GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
            		"���������", "") SKIP
            		strKodReg SKIP
            		GetXattrValueEx( "cust-ident", GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
            		"������", "") SKIP
            		strKodRegOKATO SKIP.
            	    END. */

                    /* �᫨ ���� �� ���� ���� ������ ��᪮�쪮 ࠧ, � ���� ��ࠢ�塞 ⥪���� ������ */
                    IF AVAIL cust-ident AND cust-ident.open-date NE dateAdressOpen
                     THEN DO:
                	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ���� " + strAdressType + " ������� ���� ��㣮� ��⮩." SKIP. &ENDIF
                        RELEASE cust-ident.
		    END.
            	    IF NOT AVAIL cust-ident THEN DO:
                	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ᮧ���� ���� " + strAdressType SKIP. &ENDIF
                	CREATE cust-ident.
                	ASSIGN
                    	    cust-ident.class-code     = "p-cust-adr"
                    	    cust-ident.cust-cat       = strCustCat
                    	    cust-ident.cust-id        = iBisId
                    	    cust-ident.cust-code-type = strAdressType
                    	    cust-ident.cust-type-num  = ?
                    	    cust-ident.open-date      = dateAdressOpen
                    	    cust-ident.cust-code      =
                                STRING(YEAR (dateAdressOpen),"9999") +
                                STRING(MONTH(dateAdressOpen),"99")   +
                                STRING(DAY  (dateAdressOpen),"99")
                        .
            	    END.
            	    IF cust-ident.issue NE strAdress OR cust-ident.issue EQ ?
            	     THEN DO:
                	&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ��ᢠ����� ᠬ ���� " + strAdressType SKIP. &ENDIF
            		ASSIGN
                	    cust-ident.issue          = strAdress
                	.
            	    END.
            	    VALIDATE cust-ident.
            	    sUpdateSigns( "p-cust-adr",
                             GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                             "country-id", vCountry-id, ?).
            	    sUpdateSigns( "p-cust-adr",
                             GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                             "���������", strKodReg, ?).
            	    sUpdateSigns( "p-cust-adr",
                             GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                             "������", strKodRegOKATO, ?).

		    /* ᨭ�஭����㥬 � �᭮���� ����窮� �� */
            	    IF strAdressType = vAddrMainType THEN DO:
		    	sUpdateSigns( (IF strCustCat EQ "�" THEN "person" ELSE "cust-corp"),
		             STRING(iBisId) /*GetSurrogateBuffer("person",(BUFFER person:HANDLE))*/ ,
		             "�����������", STRING(YEAR (dateAdressOpen),"9999") +
		                              STRING(MONTH(dateAdressOpen),"99")   +
		                              STRING(DAY  (dateAdressOpen),"99"), ?).
			IF strCustCat EQ "�" THEN
            		    sUpdateSigns( (IF strCustCat EQ "�" THEN "person" ELSE "cust-corp"), STRING(iBisId), "country-id", vCountry-id, ?).
            		sUpdateSigns( (IF strCustCat EQ "�" THEN "person" ELSE "cust-corp"), STRING(iBisId), "���������", strKodReg, ?).
            		sUpdateSigns( (IF strCustCat EQ "�" THEN "person" ELSE "cust-corp"), STRING(iBisId), "������", strKodRegOKATO, ?).
            		IF strCustCat EQ "�" THEN DO:
                	    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ��ᢠ����� �᭮���� ���� " + strAdressType SKIP. &ENDIF
		    	    ASSIGN person.address[1] = strAdress.
			    VALIDATE person.
            		END.
            		IF strCustCat EQ "�" THEN DO:
                	    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ��ᢠ����� �᭮���� ���� " + strAdressType SKIP. &ENDIF
		    	    ASSIGN cust-corp.addr-of-low[1] = strAdress.
			    VALIDATE cust-corp.
            		END.
			/* &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED
			    "    + ����஭����㥬 �� ���� " + strAdressType + " ��� " + GetXattrValueEx( "person", string( iBisId), "�����������", "-") SKIP.
			    &ENDIF */
            		
			/* RUN SyncAdrSubjToCident IN h_cust (
			            STRING(BUFFER person:HANDLE), "�", dateAdressOpen, strAdressType). */
			&IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED
			    "    + ����஭���஢�� ���� " + strAdressType + " �� "
				 + string( dateAdressOpen) + " ��� " + GetXattrValueEx( (IF strCustCat EQ "�" THEN "person" ELSE "cust-corp"), string( iBisId), "�����������", "-") SKIP.
			    &ENDIF
			/* NEXT. */
		    END.
		END.
                ELSE &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + ���� ���� " + strAdressType + " ᮢ������." SKIP. &ENDIF

                /* txtErr = ADDERROR(txtErr,{&ERROR_ADR_UPDATE} + " IDADR = " + STRING(adr-mfr.id) ). */

                /* ����뢠�� ���� ���� ⠪��� ⨯� �᫨ �� */
                DEF BUFFER b2cust-ident FOR cust-ident.
                FOR EACH b2cust-ident
                 WHERE b2cust-ident.class-code     = "p-cust-adr"
                   AND b2cust-ident.cust-cat       = strCustCat
                   AND b2cust-ident.cust-id        = iBisId
                   AND b2cust-ident.cust-code-type = strAdressType
                   AND b2cust-ident.open-date      < cust-ident.open-date
                   AND b2cust-ident.close-date EQ ?
                   EXCLUSIVE-LOCK:
            	    ASSIGN
            		b2cust-ident.close-date = cust-ident.open-date.
            	    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "     + ���� ���� " +
            		 strAdressType + " �� " + STRING(b2cust-ident.open-date) + " ������ ������" SKIP. &ENDIF
		END.

            END.  /* FOR EACH adr-mfr */
        END. /* IF */
        
    END. /*   UpdateClient: */
	IF bCreated OR bChangeCID THEN DO:
	    FOR EACH bank.accounts_change
	     WHERE accounts_change.cid = bank.clients-mfr.cid AND accounts_change.status_ <> 0:
	        ASSIGN
	    	    accounts_change.status_ = 0.
	    END.
    	    IF bank.clients-mfr.cidpers NE ? THEN
	    FOR EACH bank.accounts_change
	     WHERE accounts_change.cid = bank.clients-mfr.cidpers AND accounts_change.status_ <> 0:
	        ASSIGN
	    	    accounts_change.status_ = 0.
	    END.
	END.
	IF txtErr <> '' THEN  UNDO, THROW NEW Progress.Lang.AppError( txtErr).
	PUT UNFORMATTED
	 ( IF bCreated THEN "    + ᮧ���" ELSE "    + ��������") + " ������ " + 
	 ( IF iTypeCl = 1 OR iTypeCl = 3 THEN "�� person" ELSE "�� cust") + "-id=" + STRING( iBisId) + " CID=" + STRING(iCid) SKIP.

	CATCH eAnyError AS Progress.Lang.Error:
	    txtErr = (IF txtErr <> "" THEN txtErr + ';' ELSE '') + RETURN-VALUE + " " + eAnyError:GetMessage(1).
	END CATCH.
    END.  /* MYNEXT */
    IF txtErr <> "" THEN DO ON ERROR UNDO, THROW:
	PUT UNFORMATTED STRING( NOW, "99/99/99 HH:MM:SS") + " (id=" + STRING( bank.clients-mfr.id) + ", CID=" + STRING( bank.clients-mfr.cid) + ") " + txtErr SKIP.
	IF INDEX( txtErr, "�������஢�� ��㣨� ���짮��⥫��") < 1 THEN DO:
    	    bank.clients-mfr.statu = {&STATUS_OK_WITH_ERROR}.
    	    bank.clients-mfr.errortext = txtErr.
    	END.
    END.
    ELSE DO ON ERROR UNDO, THROW:
        bank.clients-mfr.statu = {&STATUS_OK}.
        bank.clients-mfr.errortext = ''.
    END.
    IF iBisId <> ? AND iBisId > 0
     THEN bank.clients-mfr.bisid = iBisId.
    VALIDATE bank.clients-mfr.
    &IF DEFINED( DEBUG-LOG) &THEN PUT UNFORMATTED "    + �६� ��ࠡ�⪨ ������ " + STRING( MTIME - iTime) + " �.ᥪ." SKIP. &ENDIF
    
        iCurrentCount = iCurrentCount + 1.
        countClients = countClients + 1.
        IF countClients > {&COUNT_CLIENTS} THEN DO:
            bAllClients = FALSE.
            LEAVE.
        END.
/*        IF INTERVAL( NOW, vStartTime, 'seconds') > 300 THEN LEAVE.*/
END.

/* PUT UNFORMATTED STRING( NOW, "99/99/99 HH:MM:SS") + " ===" SKIP. */

{intrface.del}

/*
/* ������ �訡�� � ��ப� ������ */
PROCEDURE SET_ERROR_CLIENT.
    DEFINE INPUT PARAM iError AS INT64 NO-UNDO. /* ��� �訡�� */
    DEFINE INPUT PARAM strError AS CHAR NO-UNDO. /* ⥪�� �訡��  */ 
    IF AVAIL bank.clients-mfr THEN DO:
        IF bank.clients-mfr.id <> ? THEN
            strError = "ID = " + STRING(bank.clients-mfr.id) + ', '  + strError.
        bank.clients-mfr.stat = iError.
        bank.clients-mfr.errortext = TRIM(strError).
    END.
    ELSE DO:
        PUT UNFORMATTED "�� ������� ������ � �����⮬!!!" SKIP.
        /* MESSAGE "�� ������� ������ � �����⮬!!!" VIEW-AS ALERT-BOX. */
    END.    
END PROCEDURE.
*/

