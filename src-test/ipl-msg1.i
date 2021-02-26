/* i-avanta.pro */
DEF BUFFER bcust-ident FOR cust-ident.
DEF VAR vSendBki AS LOG NO-UNDO.

FUNCTION sGetEmty RETURN CHAR(
   INPUT sIn AS CHAR
   ):
   DEF VAR vCh AS CHAR NO-UNDO.

   IF sIn = ? THEN sIn = ''.
   vCh = sIn.
   RETURN vCh.
END FUNCTION.

  DEFINE VAR mFlagSet         AS LOG  INIT ?   NO-UNDO.
  /* ��ࠡ��뢠�� ����祭��� ᮮ�饭�� */
  FOR EACH ttREQUEST ON ERROR UNDO, THROW:

    FIND FIRST signs
     WHERE signs.file-name EQ 'loan'
       AND signs.code EQ 'PLDealID'
       AND signs.code-value EQ ttREQUEST.claim-id
       NO-LOCK NO-ERROR.
    PUT UNFORMATTED "** " + STRING(NOW,"99/99/9999 HH:MM:SS") +
	  " ��ࠡ�⪠ ��� CLAIM-ID=" + STRING(ttREQUEST.claim-id) SKIP.

    DEF BUFFER old_loan FOR loan.
    DEF VAR old_loan_acct AS CHAR NO-UNDO.
    DEF VAR r-old_person AS INT64 NO-UNDO.
    DEF VAR vOld_Num AS INT64 NO-UNDO.
    DEF VAR vOldReservedCurAcct AS CHAR NO-UNDO.
    DEF VAR vOldReservedCurAcct2 AS CHAR NO-UNDO.
    vOldReservedCurAcct = ?.
    vOldReservedCurAcct2 = ?.
    old_loan_acct = ''.
    r-old_person = ?.
    IF AVAIL signs THEN
	FIND FIRST old_loan
	 WHERE old_loan.contract EQ ENTRY( 1, signs.surrogate)
	   AND old_loan.cont-code EQ ENTRY( 2, signs.surrogate)
	   NO-LOCK.
    ELSE RELEASE old_loan.
    IF AVAIL old_loan THEN DO:
	vOld_Num = INTEGER( GetXattrValue( "loan", GetSurrogateBuffer( "loan", (BUFFER old_loan:HANDLE)), "AgrCounter")).
	ASSIGN
	 r-old_person = old_loan.cust-id WHEN old_loan.cust-cat EQ "�"
	     AND GetXattrValue( "person", STRING(old_loan.cust-id), "PLDealID") EQ ttREQUEST.claim-id.
	FOR EACH loan-acct OF old_loan NO-LOCK ON ERROR UNDO,THROW:
	    IF GetXattrValue( "acct", loan-acct.acct + "," + loan-acct.currency, "PLDealID") 
	      EQ ttREQUEST.claim-id THEN DO ON ERROR UNDO, THROW:
	        FIND FIRST acct OF loan-acct.
	        IF ttREQUEST.open-date < acct.open-date THEN DO ON ERROR UNDO, THROW:
	    	    ASSIGN
	    		acct.open-date = ttREQUEST.open-date.
	    	    VALIDATE acct.
		    /*UpdateSigns( acct.class-code, GetSurrogate( "acct", ROWID(acct)), "��������", STRING(loan.open-date, "99/99/9999") + "," + loan.doc-ref, ?).
	    	    */
	    	    RELEASE acct.
	    	END.
		old_loan_acct = old_loan_acct + (IF old_loan_acct NE "" THEN "," ELSE "") + loan-acct.acct.
	    END.
	END.
	IF old_loan.filial-id NE shFilial
	 THEN vOldReservedCurAcct = "".
	 ELSE vOldReservedCurAcct = GetXAttrValueEx( "loan", old_loan.contract + ',' + old_loan.cont-code, "PLDealCurAcct",?).
	PUT UNFORMATTED " 㤠�塞 ������� " + old_loan.doc-ref SKIP.
	RUN iplDeleteLoan( old_loan.contract, old_loan.cont-code, False ).
	RELEASE old_loan.
    END. ELSE DO:
	vOld_Num = ?.
    END.
    IF vOldReservedCurAcct = "" OR vOldReservedCurAcct = ? THEN DO:
       FIND FIRST code WHERE code.class EQ "��⠐���ࢠ"
           AND code.parent EQ "��⠐���ࢠ"
           AND code.name = ttREQUEST.claim-id NO-LOCK NO-ERROR.
       IF AVAIL code THEN vOldReservedCurAcct2 = TRIM(code.code).
    END.
    
    
    DEF VAR in-fil AS CHAR NO-UNDO.
    in-fil = ttREQUEST.branch-id.
    IF shFilial EQ "0000" THEN in-fil = "0000".
    FIND FIRST branch WHERE branch.branch-id = in-fil NO-LOCK NO-ERROR.
    IF NOT AVAIL branch THEN
            UNDO, THROW NEW Progress.Lang.AppError( "�� ������ ��� ���ࠧ������� " + in-fil
        	+ ' � ��������� ���.').
    DO WHILE NOT CAN-DO( "0,10,11", branch.branch-type) ON ERROR UNDO, THROW:
    	    in-fil = branch.parent-id.
	    FIND FIRST branch WHERE branch.branch-id = in-fil NO-LOCK.
    END.
    if shFilial NE in-fil THEN DO ON ERROR UNDO, THROW:
	    RUN DelConnectLink.
	    RUN SetConnectLink IN h_base (in-fil).
	    RUN SetEnvironment IN h_base (in-fil). /* ���⥪�� ��࠭���� 䨫���� */
	    gend-date = ttREQUEST.open-date.
	    PUT UNFORMATTED 'ᬥ�� ⥪ 䨫���� �� ' + shFilial SKIP.
    END.
    
    PUT UNFORMATTED ttREQUEST.claim-id ";" string( ttREQUEST.open-date, "99/99/9999") SKIP.
    
    /*IF ttREQUEST.claim-id NE "1163095" THEN*/
       if fChkClsDate( ttREQUEST.open-date, "*") then
               UNDO, THROW NEW Progress.Lang.AppError( "����.���� " + string( ttREQUEST.open-date, "99/99/9999")
           	+ ' 㦥 ������.').

    /* �饬 ������ ⮫쪮 �� 㤮�⮢�७�� ��筮�� */
    DEF VAR vUdLich  AS LOG NO-UNDO.
    FOR EACH ttPERSON WHERE ttPERSON.request-id = ttREQUEST.request-id,
          EACH ttDOCUM WHERE ttPERSON.person-id = ttDOCUM.person-id ON ERROR UNDO, THROW:
        /* �஢�ઠ ����� ���㬥�� �� ॣ.��ࠦ����, �������� � �ࠢ�筨�� ������� */
        DEF VAR vRegExpr AS CHAR NO-UNDO.
        DEF VAR vResult  AS CHAR NO-UNDO.
        DEF VAR vErrMes  AS CHAR NO-UNDO.
        vRegExpr = GetCodeMisc("�������", ttDOCUM.docum-type, 3).
        vUdLich  = GetCodeMisc("�������", ttDOCUM.docum-type, 2) EQ "��".
        IF NOT DYNAMIC-FUNCTION("ereg":U, vRegExpr, ttDOCUM.docum-no, OUTPUT vResult, INPUT-OUTPUT vErrMes)
	     THEN UNDO, THROW NEW Progress.Lang.AppError(
	        "����� ���㬥�� '" +
	        ttDOCUM.docum-no + "' �� ᮮ⢥����� �ࠢ��� ������樨, 㪠������ � �����䨪��� �������"). 
	
        /* �஢�ઠ �� ����稥 � ���� ������⢨⥫��� ��ᯮ�⮢ */
        FIND FIRST code WHERE
        	     code.class  EQ "black-list"
                 AND code.parent EQ "black-list"
                 AND code.code   EQ ttDOCUM.docum-no
                 AND code.name   EQ ttDOCUM.docum-type
                NO-LOCK NO-ERROR.
        IF AVAIL(code) THEN
	         UNDO , THROW NEW Progress.Lang.AppError( "���㬥�� '" + code.name + "' ������ � ���� ������⢨⥫��� ���㬥�⮢. ��稭� '" + GetCodeName("black-why",code.val) + "'").

        FIND FIRST cust-ident WHERE cust-ident.class-code EQ 'p-cust-ident' AND
	     cust-ident.cust-cat = '�' AND
	     cust-ident.cust-code EQ ttDOCUM.docum-no AND
	     cust-ident.cust-code-type EQ ttDOCUM.docum-type NO-LOCK NO-ERROR.
	ttDOCUM.docum-in-bis = AVAIL cust-ident.
        IF AVAIL cust-ident THEN DO ON ERROR UNDO, THROW:
    	    put unformatted "������ ������ �� ���㬥��� '" + ttDOCUM.docum-type + "', person-id=" + string( cust-ident.cust-id) skip.
            IF ttPERSON.real-person-id NE ? AND
                ttPERSON.real-person-id <> cust-ident.cust-id THEN
                UNDO, THROW NEW Progress.Lang.AppError( '� 䨧.��� ('
            	     + STRING(ttPERSON.real-person-id) + ') 㪠���� ���㬥��� ��㣮�� ������ (' + STRING(cust-ident.cust-id) + ') � ���').

            IF cust-ident.open-date NE ttDOCUM.docum-date THEN
                UNDO, THROW NEW Progress.Lang.AppError( '� 䨧.��� person-id=' + string( cust-ident.cust-id) +
            	    ' �� ᮢ������ ��� �뤠� ���㬥�� "' + ttDOCUM.docum-type + '" � ��� (' + STRING(cust-ident.open-date) + ')').

            IF cust-ident.close-date NE ? THEN
                UNDO, THROW NEW Progress.Lang.AppError( '� 䨧.��� person-id=' + string( cust-ident.cust-id) +
            	    ' 㪠���� � ��� ���㬥�� "' + ttDOCUM.docum-type + '" �� ����⢨⥫�� � ��� � ' + STRING(cust-ident.close-date)).

	    /* �᫨ ���㬥�� ���� �.��筮��, � ᮢ��頥� ������ � ��� */
            ASSIGN ttPERSON.real-person-id = cust-ident.cust-id WHEN vUdLich.
        END.
    END. /* FOR EACH */

    /* ��᫥ ⮣� ��� ��諨 ������, �஢�ઠ ���� � ������ ��� ���㬥�� 㦥 ������ � ���� ����� ���� */
    FOR EACH ttPERSON WHERE ttPERSON.request-id = ttREQUEST.request-id,
          EACH ttDOCUM WHERE ttPERSON.person-id = ttDOCUM.person-id ON ERROR UNDO, THROW:
	/* ���㬥�� ��諨 � ���, �� �� �� 㤮�⮢�७�� � �� ���� ������ �� ����� �ࠢ� ᢥ���� */
        IF ttPERSON.real-person-id EQ ? AND ttDOCUM.docum-in-bis THEN
                UNDO, THROW NEW Progress.Lang.AppError( '������� 䨧.��� � ���, �� ' +
            	    ' 㪠���� ���㬥�� �� ���� �.��筮�� � ���, ���� �� ���㬥��� ' + ttDOCUM.docum-type + ' �� �����⨬.').
        IF ttPERSON.real-person-id NE ? THEN DO ON ERROR UNDO, THROW:
    	    FIND FIRST bcust-ident WHERE bcust-ident.class-code EQ 'p-cust-ident' AND
	     bcust-ident.cust-cat = '�' AND
	     bcust-ident.cust-id EQ ttPERSON.real-person-id AND
	     bcust-ident.cust-code-type EQ ttDOCUM.docum-type AND
	     bcust-ident.open-date > ttDOCUM.docum-date NO-LOCK NO-ERROR.
            IF AVAIL bcust-ident THEN
                UNDO, THROW NEW Progress.Lang.AppError( '� 䨧.��� (' + STRING(ttPERSON.real-person-id) +
            	    ') 㪠���� ���㬥�� �� ����⢨⥫��, � ���� ���� ���㬥�� �������騩 � ' + STRING(bcust-ident.open-date) + ' � ���').
            
        END.
    END. /* FOR EACH */

    DEF VAR ic AS INT NO-UNDO.
    ic = 0.
    FOR EACH ttCAR
	 WHERE ttCAR.request-id = ttREQUEST.request-id
	  ON ERROR UNDO, THROW:
	    ic = ic + 1.
    END.
    IF ic < 1 THEN UNDO MAIN, THROW NEW Progress.Lang.AppError( "���������� ��易⥫�� ४������ ��⮬�����").
    IF ic > 1 THEN UNDO MAIN, THROW NEW Progress.Lang.AppError( " ��� ����� ���� 㪠���� ४������ ⮫쪮 ������ ��⮬�����").
    FIND FIRST ttCAR
	 WHERE ttCAR.request-id = ttREQUEST.request-id.
    /* ᢥ�塞 �᭮��� ४������ ��������� �����⮢ */
    /*DEF VAR erm AS CHAR INIT ? NO-UNDO.*/
    FOR EACH ttPERSON
	 WHERE ttPERSON.request-id = ttREQUEST.request-id
	   AND ttPERSON.real-person-id NE ? ON ERROR UNDO, THROW:
	    FIND FIRST person WHERE person.person-id EQ ttPERSON.real-person-id NO-LOCK.
	    IF CAPS(ttPERSON.name-last) NE CAPS(person.name-last) OR
	       CAPS(ttPERSON.first-names) NE CAPS(person.first-names) OR
	       ttPERSON.Birthday NE person.birthday  OR
	       ttPERSON.gender NE person.gender THEN DO ON ERROR UNDO, THROW:
	        /*erm = "� 䨧.��� �� ᮢ������ �᭮��� ४������ � �� (䨮,��,���)".
	        LEAVE.*/
	        UNDO MAIN, THROW NEW Progress.Lang.AppError( "� 䨧.��� �� ᮢ������ �᭮��� ४������ � �� (䨮,��,���)").
	    END.
    END.
    /*IF erm NE ? THEN UNDO MAIN, THROW NEW Progress.Lang.AppError( erm).*/

    /* ���� ��諨 ��ࠢ�塞, ���� ������塞 ����� */
    vSendBki = False.
    FOR EACH ttPERSON
	 WHERE ttPERSON.request-id = ttREQUEST.request-id ON ERROR UNDO, THROW:
	    DEF VAR mCliName AS CHAR NO-UNDO.
	    DEF VAR piis AS CHAR NO-UNDO.
	    /* �஢�ઠ �� �ਭ���������� � ��� �����⮢ */
	    mCliName = ttPERSON.name-last + " " + ttPERSON.first-names.
	    /*IF CompareName(mCliName, "plat") THEN DO:
    		UNDO, THROW NEW Progress.Lang.AppError( "������ '" + mCliName + "' ������ � �ࠢ�筨�� �����⮢.").
	    END.*/
	    vSendBki = ttPERSON.send-bki.
	    /*DEF VAR IsCrPers AS LOG NO-UNDO.
	    IsCrPers = False.*/
	    IF ttPERSON.real-person-id EQ ?
	     THEN DO ON ERROR UNDO, THROW:
	        /* ���� �� ��� � �� */
	        DEF VAR dlist AS CHAR NO-UNDO.
	        dlist = ''.
	        FOR EACH person
	         WHERE person.name-last   EQ ttPERSON.name-last
	           AND person.first-names EQ ttPERSON.first-names
	           AND person.birthday    EQ ttPERSON.Birthday
	           NO-LOCK:
	    	    dlist = dlist + (IF length(dlist) > 0 THEN ', ' ELSE '') + STRING( person.person-id).
		END.
	        IF dlist NE '' THEN
	         UNDO, THROW NEW Progress.Lang.AppError(
	             " person-id=(" + dlist +
	             ") ������� ������(�) � ⥬ �� ��� � ��⮩ ஦�����, �� �� ᮢ�����騬� ���㬥�⠬�").
	        
		/* ������塞 */
		CREATE person.
		ASSIGN
		    person.person-id   = Get-New-Person-Id()
		    person.name-last   = ttPERSON.name-last
		    person.first-names = ttPERSON.first-names
            	    person.birthday    = ttPERSON.Birthday
            	    person.date-in     = TODAY
		    .
		UpdateSigns( "person", STRING(person.person-id), "PLDealID", ttREQUEST.claim-id, ?).
		UpdateSigns( "person", STRING(person.person-id), "CID", "-1", ?).
			
		/**PEO 03.02.16**/
        UpdateSigns( "person", STRING(person.person-id), "��⠏஢����", STRING(Today,"99/99/9999"), ?).        
        /**PEO 03.02.16  end **/
		
		/*IsCrPers = True.*/
	    END. ELSE DO ON ERROR UNDO, THROW:
        	FIND FIRST person WHERE person.person-id EQ ttPERSON.real-person-id NO-ERROR NO-WAIT.
        	IF NOT AVAIL person THEN DO:
        	    IF LOCKED person
        	     THEN UNDO, THROW NEW Progress.Lang.AppError( " person-id=" + STRING(ttPERSON.real-person-id) + " ������ �������஢��").
        	    UNDO, THROW NEW Progress.Lang.AppError( " person-id=" + STRING(ttPERSON.real-person-id) + " �訡�� ���᪠ ������").
        	END.
	    END.

	    FIND FIRST ttDOCUM
		 WHERE ttPERSON.person-id = ttDOCUM.person-id AND ttDOCUM.docum-type = '��ᯮ��' NO-LOCK NO-ERROR.
	    IF NOT AVAIL ttDOCUM THEN
		 FIND FIRST ttDOCUM
		  WHERE ttPERSON.person-id = ttDOCUM.person-id NO-LOCK NO-ERROR.
	    RUN MakeIssue IN h_cust (ttDOCUM.docum-issue, ttDOCUM.docum-podr, OUTPUT piis).
	    ASSIGN
		person.country-id  = ttPERSON.country-id
		person.document-id = ttDOCUM.docum-type
		person.document    = ttDOCUM.docum-no
		person.issue       = piis /*REPLACE(ttDOCUM.docum-issue,',',' ') + ',' + ttDOCUM.docum-podr */
		person.gender      = ttPERSON.gender
		.
	    IF NUM-ENTRIES(person.phone[1]) < 2
	      THEN person.phone[1]    = ttPERSON.phone-home + ','.
	      ELSE ENTRY(1,person.phone[1]) = replace( ttPERSON.phone-home, ',', ' ').
	    IF NUM-ENTRIES(person.phone[2]) < 2
	      THEN person.phone[2] = ENTRY(1,person.phone[2]) + ',' + replace( ttPERSON.cell-phone, ',', ' ').
	      ELSE ENTRY(2,person.phone[2]) = replace( ttPERSON.cell-phone, ',', ' ').
	    UpdateSigns( "person", STRING(person.person-id), "Document4Date_vid", STRING(ttDOCUM.docum-date,"99/99/9999"), ?).
	    VALIDATE person.

	    PUT UNFORMATTED "�������� person ok" SKIP.
/*	    IF    ERROR-STATUS:ERROR OR RETURN-VALUE GT "" THEN MESSAGE (IF RETURN-VALUE GT "" THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1)) VIEW-AS ALERT-BOX.*/

	    UpdateSigns( "person", STRING(person.person-id), "BirthPlace", ttPERSON.BirthPlace, ?).
	    IF ttPERSON.real-person-id EQ ? THEN
            UpdateSigns( "person", STRING(person.person-id), "branch-id", shFilial, ?).
	    /*UpdateSigns( "person", STRING(person.person-id), "���ॣ���", ttPERSON.tax-insp, ?).*/
	    
	    IF GetXAttrValueEx ( "person", STRING(person.person-id), "�������", ?) EQ ?
	     THEN UpdateSigns( "person", STRING(person.person-id), "�������", "������", ?).
	    IF GetXAttrValueEx ( "person", STRING(person.person-id), "��ꥪ�", ?) EQ ?
	     THEN UpdateSigns( "person", STRING(person.person-id), "��ꥪ�", "��", ?).
	    /*
	    IF ttPERSON.EmployerName NE ? AND ttPERSON.EmployerName NE "" THEN DO:
		UpdateSigns( "person", STRING(person.person-id), "�����", ttPERSON.EmployerName, ?).
		UpdateSigns( "person", STRING(person.person-id), "�����_�������", ttPERSON.OccupationCode, ?).
		UpdateSigns( "person", STRING(person.person-id), "�����_�������", ttPERSON.OccupationStatus, ?).
		UpdateSigns( "person", STRING(person.person-id), "�����_��ࠄ���", ttPERSON.OccupationTrade, ?).
		UpdateSigns( "person", STRING(person.person-id), "����⠍���", ttPERSON.OccuDateHired, ?).
		UpdateSigns( "person", STRING(person.person-id), "����⠍������", ttPERSON.OccuDateTerm, ?).
		UpdateSigns( "person", STRING(person.person-id), "�����_�����", ttPERSON.OccuCurrent, ?).
		UpdateSigns( "person", STRING(person.person-id), "����⠏��������", ttPERSON.OccuTitle, ?).
	    END.
	    */

	    FIND FIRST ttEMPL
	     WHERE ttPERSON.person-id = ttEMPL.person-id NO-LOCK NO-ERROR.
	    IF AVAIL ttEMPL THEN DO:
		put unformatted " ����㦠�� ����� � ࠡ��" SKIP.
		UpdateSigns( "person", STRING(person.person-id), "�����", ttEMPL.empl-name, ?).
		UpdateSigns( "person", STRING(person.person-id), "�����_���", ttEMPL.empl-inn, ?).
		/* UpdateSigns( "person", STRING(person.person-id), "�����_�������", ttPERSON.OccupationCode, ?). */
		/* UpdateSigns( "person", STRING(person.person-id), "�����_�������", ttEMPL.empl-contract-type, ?). */
		/* UpdateSigns( "person", STRING(person.person-id), "�����_��ࠄ���", ttPERSON.OccupationTrade, ?). */
		/* UpdateSigns( "person", STRING(person.person-id), "����⠍���", ttPERSON.OccuDateHired, ?). */
		/* UpdateSigns( "person", STRING(person.person-id), "����⠍������", ttPERSON.OccuDateTerm, ?). */
		/* ����� ���� ࠡ��� 1- ⥪�騩 2-�०��� */
		/* UpdateSigns( "person", STRING(person.person-id), "�����_�����", ttEMPL.empl-status, ?). */
		UpdateSigns( "person", STRING(person.person-id), "����⠏��������", ttEMPL.empl-position, ?).   /* ��������� */
		UpdateSigns( "person", STRING(person.person-id), "����⠀��", ttEMPL.empl-legal-address, ?).    /* ��.���� */
		UpdateSigns( "person", STRING(person.person-id), "����⠀������", ttEMPL.empl-fact-address, ?). /* 䠪�.���� */
		UpdateSigns( "person", STRING(person.person-id), "����⠒��", ttEMPL.empl-fixed-phone, ?).      /* ��樮���� ⥫ */
	    END.

	    put unformatted "������塞 㤮�⮢�७�� ��筮��" skip.

	    /*------------------ 㤮�⮢�७�� ��筮�� --------------------------------------*/
	    FOR EACH ttDOCUM WHERE ttPERSON.person-id = ttDOCUM.person-id ON ERROR UNDO,THROW:
        	FIND FIRST cust-ident WHERE
		        cust-ident.cust-cat       EQ "�"
		        AND cust-ident.cust-id        EQ person.person-id
		        AND cust-ident.cust-code-type EQ ttDOCUM.docum-type
		        AND cust-ident.cust-code      EQ ttDOCUM.docum-no
		        AND cust-ident.class-code     EQ 'p-cust-ident'
                        AND cust-ident.open-date      EQ ttDOCUM.docum-date
		        NO-ERROR NO-WAIT.
        	IF NOT AVAIL cust-ident THEN DO:
        	    IF LOCKED cust-ident
        	     THEN UNDO, THROW NEW Progress.Lang.AppError( " person-id=" + STRING(person.person-id) + " ������ �������஢��").
        	END.
        	IF  NOT AVAIL cust-ident THEN DO ON ERROR UNDO,THROW:
            	    CREATE cust-ident.
            	    ASSIGN
                        cust-ident.class-code     = 'p-cust-ident'
                        cust-ident.cust-id        = person.person-id
                        cust-ident.cust-cat       = '�'
                        cust-ident.cust-code-type = ttDOCUM.docum-type
                        cust-ident.cust-code      = ttDOCUM.docum-no
                        cust-ident.open-date      = ttDOCUM.docum-date
                        cust-ident.cust-type-num  = ?
                    .
		END.
		RUN MakeIssue IN h_cust (ttDOCUM.docum-issue, ttDOCUM.docum-podr, OUTPUT piis).
		ASSIGN
                    cust-ident.issue          = piis.
                VALIDATE cust-ident.
                /* ����� � ����� �訡�� �ttHistoryFieldsCS
                IF ERROR-STATUS:ERROR OR RETURN-VALUE GT ""
                     THEN UNDO MAIN, THROW NEW Progress.Lang.AppError( "x:" +
                        (IF RETURN-VALUE GT ""
                            THEN RETURN-VALUE
                            ELSE ERROR-STATUS:GET-MESSAGE(1))).*/
                /* ᭠砫� ������� ��ᯮ�� �� ������ */
		UpdateSignsEx( cust-ident.class-code,
                                GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                                "���ࠧ�", ttDOCUM.docum-podr).
        	FIND FIRST cust-ident WHERE
		        cust-ident.cust-cat       EQ "�"
		        AND cust-ident.cust-id        EQ person.person-id
		        AND cust-ident.cust-code-type EQ ttDOCUM.docum-type
		        AND cust-ident.cust-code      EQ ttDOCUM.docum-no
		        AND cust-ident.class-code     EQ 'p-cust-ident'
		        NO-LOCK.
		IF ttDOCUM.docum-podr NE ? THEN
		    UpdateSigns ("p-cust-ident",cust-ident.cust-code-type + "," + 
                                                cust-ident.cust-code + "," + 
                                                STRING (cust-ident.cust-type-num),
                                 "���ࠧ�", ttDOCUM.docum-podr, ?).
        	/* ���� ������� ��᫥���� ���㬥�� � ������ � ⥬ �� ⨯�� */
    		FIND LAST bcust-ident WHERE bcust-ident.class-code EQ 'p-cust-ident' AND
	         bcust-ident.cust-cat = '�' AND
	         bcust-ident.cust-id EQ cust-ident.cust-id AND
	         bcust-ident.cust-code-type EQ cust-ident.cust-code-type AND
	         bcust-ident.open-date < cust-ident.open-date AND
	         bcust-ident.close-date EQ ? NO-ERROR.
	        IF AVAIL bcust-ident THEN DO:
	    	    ASSIGN
	    		bcust-ident.close-date = cust-ident.open-date
	    		.
	    	    put unformatted "���� ���㬥�� ������ ������." skip.
	    	    RELEASE bcust-ident.
		END.
		/* i-mg-cli.p */
	    END.
	    put unformatted "�ய��뢠�� ���㬥�� �� 㬮�砭��." skip.
	    FOR EACH ttDOCUM WHERE ttPERSON.person-id = ttDOCUM.person-id ON ERROR UNDO,THROW:
    		IF ttDOCUM.docum-type EQ "��ᯮ��" THEN DO ON ERROR UNDO,THROW:
		    RUN MakeIssue IN h_cust (ttDOCUM.docum-issue, ttDOCUM.docum-podr, OUTPUT piis).
        	    ASSIGN
			person.document-id = ttDOCUM.docum-type
			person.document    = ttDOCUM.docum-no
			/*person.issue       = REPLACE(ttDOCUM.docum-issue,',',' ') + 
			    (IF ttDOCUM.docum-podr NE ? THEN ',' + ttDOCUM.docum-podr ELSE '')*/
			person.issue = piis
			.
		    VALIDATE person.
		    UpdateSigns("person",STRING(person.person-id),"Document4Date_vid",STRING(ttDOCUM.docum-date,"99/99/9999"),YES).
		END.
	    END.
	    put unformatted "������塞 ����" skip.

	    DEF VAR vAddr AS CHAR NO-UNDO.
	    DEFINE VAR vDate         AS DATE         NO-UNDO.
	    DEFINE VAR vAddrMainType AS CHAR         NO-UNDO.
	    DEFINE VAR vAdrCntXattr  AS CHAR         NO-UNDO.

	    RUN GetTypeMainAdr ("�",OUTPUT vAddrMainType,OUTPUT vAdrCntXattr).
	    FOR EACH ttADDR
	     WHERE ttPERSON.person-id = ttADDR.person-id ON ERROR UNDO, THROW:
		vDate = (IF ttADDR.registration-date EQ ? THEN today ELSE ttADDR.registration-date).
		
		/* �஢��塞 ���� �� ����� ���� ����� �� ����� ���� */
		FIND FIRST cust-ident USE-INDEX cust WHERE
                	    cust-ident.cust-cat       EQ "�"
	                 AND cust-ident.cust-id        EQ person.person-id
	                 AND cust-ident.cust-code-type EQ ttADDR.addr-type
	                 AND cust-ident.open-date > vDate
	                     NO-LOCK NO-ERROR.
	        IF AVAIL cust-ident THEN
			UNDO, THROW NEW Progress.Lang.AppError( "� ������ '" + mCliName + "' ������ ���� ⮣� �� ⨯�, �� � ����� ����� ��⮩.").
		 IF ttADDR.code-gni EQ ? THEN
			UNDO, THROW NEW Progress.Lang.AppError( "� ������ '" + mCliName + "' ���� ������ ᮤ�ঠ�� ��� ॣ����.").
 
		IF ttADDR.addr-idx  EQ ? OR ttADDR.addr-idx EQ "" OR ttADDR.addr-idx  EQ "0" OR
		   ttADDR.addr-idx  EQ "000000" OR LENGTH( ttADDR.addr-idx) < 6 THEN 
			UNDO, THROW NEW Progress.Lang.AppError( "� ������ '" + mCliName + "' ���� ������ ᮤ�ঠ�� ���⮢� ������.").
 
		IF (LOOKUP( CAPS(ttADDR.addr-street), 
			"���,����,����,�����������,������,����������,�� ��������,��� ������,�/�,������ �����������,�����,NOT,NA,N/A,NONE,TEST,MISSING,ERROR,UNKNOWN,NO DATA,NOT SUPPLIED,NOT PROVIDED,-"
				) > 1) THEN
			UNDO, THROW NEW Progress.Lang.AppError( "� ������ '" + mCliName + "' ���� ᮤ�ন� �������⨬�� ���祭�� '"
			   + ttADDR.addr-street + "'").
 
/*
		DEF BUFFER DLbuf FOR DataLine.
		IF NOT AVAIL DLbuf THEN DO ON ERROR UNDO, THROW:
		    FIND FIRST Code WHERE
		       Code.class EQ ""
		       AND Code.code  EQ "GNIRayon"
		       NO-LOCK NO-ERROR.
		    FIND FIRST DLbuf WHERE
		         DLbuf.Data-ID EQ INT64(Code.misc[8]) AND
		         DLbuf.Sym1 BEGINS ttADDR.code-gni AND
		         DLbuf.Txt  BEGINS SUBSTRING(ttADDR.addr-idx, 1, 3)
			NO-LOCK NO-ERROR.
		END.
		IF NOT AVAIL DLbuf THEN DO ON ERROR UNDO, THROW:
		    FIND FIRST Code WHERE
		       Code.class EQ ""
		       AND Code.code  EQ "GNICity"
		       NO-LOCK NO-ERROR.
		    FIND FIRST DLbuf WHERE
		         DLbuf.Data-ID EQ INT64(Code.misc[8]) AND
		         DLbuf.Sym1 BEGINS ttADDR.code-gni AND
		         DLbuf.Txt  BEGINS SUBSTRING(ttADDR.addr-idx, 1, 3)
			NO-LOCK NO-ERROR.
		END.
		IF NOT AVAIL DLbuf THEN DO ON ERROR UNDO, THROW:
		    FIND FIRST Code WHERE
		       Code.class EQ ""
		       AND Code.code  EQ "GNITown"
		       NO-LOCK NO-ERROR.
		    FIND FIRST DLbuf WHERE
		         DLbuf.Data-ID EQ INT64(Code.misc[8]) AND
		         DLbuf.Sym1 BEGINS ttADDR.code-gni AND
		         DLbuf.Txt  BEGINS SUBSTRING(ttADDR.addr-idx, 1, 3)
			NO-LOCK NO-ERROR.
		END.
		IF NOT AVAIL DLbuf THEN DO ON ERROR UNDO, THROW:
		    FIND FIRST Code WHERE
		       Code.class EQ ""
		       AND Code.code  EQ "GNIStreet"
		       NO-LOCK NO-ERROR.
		    FIND FIRST DLbuf WHERE
		         DLbuf.Data-ID EQ INT64(Code.misc[8]) AND
		         DLbuf.Sym1 BEGINS ttADDR.code-gni AND
		         DLbuf.Txt  BEGINS SUBSTRING(ttADDR.addr-idx, 1, 3)
			NO-LOCK NO-ERROR.
		END.
		IF NOT AVAIL DLbuf THEN
		    UNDO, THROW NEW Progress.Lang.AppError(
		    /* put unformatted */
		    "�� ������ ������ " + ttADDR.addr-idx + " � ॣ���� " + ttADDR.code-gni 
		    /* SKIP. */
		       ).
*/
		
		/* �஢��塞 �� ������ gnitrig.i
		DEF BUFFER DLbuf FOR DataLine.
		DEF VAR vKladrCode AS CHAR NO-UNDO.
		RELEASE DLbuf.
		vKladrCode = ttADDR.code-gni.
		FIND FIRST Code WHERE
		   Code.class EQ ""
		   AND Code.code  EQ "GNIRayon"
		   NO-LOCK NO-ERROR.
		IF NOT AVAILABLE(Code) THEN
		    UNDO, THROW NEW Progress.Lang.AppError(
		       "��������� �ࠢ�筨� GNIRayon !"
		       ).
		DEF VAR mDataID AS INT64 NO-UNDO.
		ASSIGN mDataID = INT64(Code.misc[8]) NO-ERROR.
		FIND FIRST DataBlock WHERE
		   DataBlock.Data-ID EQ mDataID
		   NO-LOCK NO-ERROR.
		IF mDataID                EQ 0         OR
		   mDataID                EQ ?         OR
		   NOT AVAILABLE DataBlock             OR
		   DataBlock.DataClass-ID NE "GNIRayon" THEN
		    UNDO, THROW NEW Progress.Lang.AppError(
		       "��������� �ࠢ�筨� GNIRayon !"
		       ).
		FIND FIRST DataLine WHERE
		         DataLine.Data-ID EQ mDataID   AND
		         DataLine.Sym1 BEGINS ttADDR.code-gni AND
		         SUBSTR(DataLine.Sym1,6,6) = "000000" AND
		         ttADDR.addr-obl EQ DataLine.Sym2 + ' ' + DataLine.Sym3
		         NO-LOCK NO-ERROR.
		
		DEF VAR vCodeOblChar_ AS CHAR NO-UNDO.
		vCodeOblChar_ = (IF AVAIL DataLine THEN DataLine.Sym1 ELSE "").
		IF AVAIL DataLine THEN
		    vKladrCode = SUBSTRING (DataLine.Sym1, 1, 5).
		IF AVAIL DataLine THEN
		    put unformatted "GNIRegion=" + DataLine.Sym1 + ',' + DataLine.Sym2 SKIP.
		ELSE put unformatted "GNIRegion=? (" + ttADDR.addr-obl + ")" SKIP.
		
		FIND FIRST Code WHERE
		   Code.class EQ ""
		   AND Code.code  EQ "GNICity"
		   NO-LOCK NO-ERROR.
		IF NOT AVAILABLE(Code) THEN
		    UNDO, THROW NEW Progress.Lang.AppError(
		       "��������� �ࠢ�筨� GNICity"
		       ).
		ASSIGN mDataID = INT64(Code.misc[8]) NO-ERROR.
		FIND FIRST DataBlock WHERE
		   DataBlock.Data-ID EQ mDataID
		   NO-LOCK NO-ERROR.
		IF mDataID                EQ 0         OR
		   mDataID                EQ ?         OR
		   NOT AVAILABLE DataBlock             OR
		   DataBlock.DataClass-ID NE "GNICity" THEN
		    UNDO, THROW NEW Progress.Lang.AppError(
		       "��������� �ࠢ�筨� GNICity !"
		       ).
		FIND FIRST DataLine WHERE
		         DataLine.Data-ID EQ mDataID   AND
		         DataLine.Sym1 BEGINS (IF vCodeOblChar_ NE "" THEN SUBSTRING (vCodeOblChar_, 1, 5) ELSE ttADDR.code-gni) AND
		         ttADDR.addr-city EQ DataLine.Sym2 + ' ' + DataLine.Sym3
		         NO-LOCK NO-ERROR.
		DEF VAR vCodeGorChar_ AS CHAR NO-UNDO.
		vCodeGorChar_ = (IF AVAIL DataLine THEN DataLine.Sym1 ELSE "").
		IF AVAIL DataLine THEN
		    vKladrCode = SUBSTRING (DataLine.Sym1, 1, 8).
		IF AVAIL DataLine AND DataLine.Sym4 NE "" AND DataLine.Sym4 NE ? THEN
		    FIND FIRST DLbuf WHERE ROWID(DLbuf) EQ ROWID(DataLine) NO-LOCK.
		IF AVAIL DataLine THEN
		    put unformatted "GNICity=" + DataLine.Sym1 + ',' + DataLine.Sym2 SKIP.
		ELSE put unformatted "GNICity=? (" + ttADDR.code-gni + "," + vCodeOblChar_ + "," + ttADDR.addr-city + ")" SKIP.
		IF AVAIL DataLine AND DataLine.Sym4 NE "" AND DataLine.Sym4 NE ? THEN
		    put unformatted "������=" + DataLine.Sym4 SKIP.

		FIND FIRST Code WHERE
		   Code.class EQ ""
		   AND Code.code  EQ "GNITown"
		   NO-LOCK NO-ERROR.
		IF NOT AVAILABLE(Code) THEN
		    UNDO, THROW NEW Progress.Lang.AppError(
		       "��������� �ࠢ�筨� GNITown"
		       ).
		ASSIGN mDataID = INT64(Code.misc[8]) NO-ERROR.
		FIND FIRST DataBlock WHERE
		   DataBlock.Data-ID EQ mDataID
		   NO-LOCK NO-ERROR.
		IF mDataID                EQ 0         OR
		   mDataID                EQ ?         OR
		   NOT AVAILABLE DataBlock             OR
		   DataBlock.DataClass-ID NE "GNITown" THEN
		    UNDO, THROW NEW Progress.Lang.AppError(
		       "��������� �ࠢ�筨� GNITown !"
		       ).
		FIND FIRST DataLine WHERE
		         DataLine.Data-ID EQ mDataID   AND
		         DataLine.Sym1 BEGINS (IF vCodeGorChar_ NE "" THEN (IF vCodeOblChar_ NE "" THEN SUBSTRING (vCodeOblChar_, 1, 5) ELSE ttADDR.code-gni) ELSE SUBSTRING (vCodeOblChar_, 1, 8)) AND
		         ttADDR.addr-npunkt EQ DataLine.Sym2 + ' ' + DataLine.Sym3
		         NO-LOCK NO-ERROR.
		DEF VAR vCodeTownChar_ AS CHAR NO-UNDO.
		vCodeTownChar_ = (IF AVAIL DataLine THEN DataLine.Sym1 ELSE "").
		IF AVAIL DataLine THEN
		    vKladrCode = SUBSTRING (DataLine.Sym1, 1, 11).
		IF AVAIL DataLine AND DataLine.Sym4 NE "" AND DataLine.Sym4 NE ? THEN
		    FIND FIRST DLbuf WHERE ROWID(DLbuf) EQ ROWID(DataLine) NO-LOCK.
		IF AVAIL DataLine THEN
		put unformatted "GNITown=" + DataLine.Sym1 + ',' + DataLine.Sym2 SKIP.
		ELSE put unformatted "GNITown=? (" + ttADDR.code-gni + "," + vCodeOblChar_ + "," + vCodeGorChar_ + "," + ttADDR.addr-city + ")" SKIP.
		IF AVAIL DataLine AND DataLine.Sym4 NE "" AND DataLine.Sym4 NE ? THEN
		    put unformatted "������=" + DataLine.Sym4 SKIP.
		
		IF NOT AVAIL DLbuf THEN DO ON ERROR UNDO, THROW:
		    FIND FIRST Code WHERE
		       Code.class EQ ""
		       AND Code.code  EQ "GNICity"
		       NO-LOCK NO-ERROR.
		    FIND FIRST DLbuf WHERE DLbuf.Data-ID EQ INT64(Code.misc[8]) AND DLbuf.Sym4 EQ ttADDR.addr-idx AND DLbuf.Sym1 BEGINS vKladrCode NO-LOCK NO-ERROR.
		END.
		*/
		/*
		FOR EACH DataLine WHERE
		         DataLine.Data-ID EQ mDataID   AND
		         DataLine.Sym1 BEGINS (IF iParent = "GNIStreet"
                               THEN SUBSTR(iGniSym1,1,11)
                               ELSE IF iParent = "GNIDoma"
                                    THEN SUBSTR(iGniSym1,1,15)
                                    ELSE iGniSym1) AND
		         (IF iParent = "GNIRayon"  THEN SUBSTR(DataLine.Sym1,6,6) = "000000" ELSE YES)
		         NO-LOCK  BY Data-ID BY Sym2:
		END.
		*/
		IF ttADDR.addr-type EQ "���䠪�" THEN ASSIGN ttADDR.addr-type = "�������".
		IF ttADDR.addr-type EQ "���ய" THEN ASSIGN ttADDR.addr-type = "����ய".
        	vAddr = (IF ttADDR.addr-idx  NE ? THEN ttADDR.addr-idx  ELSE "") + "," +
		        (IF ttADDR.addr-obl  NE ? THEN ttADDR.addr-obl  ELSE "") + "," +
		        (IF ttADDR.addr-city NE ? THEN ttADDR.addr-city ELSE "") + "," +
		        (IF ttADDR.addr-npunkt NE ? THEN ttADDR.addr-npunkt ELSE "") + "," +
		        (IF ttADDR.addr-street NE ? THEN ttADDR.addr-street ELSE "") + "," +
		        (IF ttADDR.addr-house  NE ? THEN ttADDR.addr-house  ELSE "") + "," +
		        (IF ttADDR.addr-str    NE ? THEN ttADDR.addr-str    ELSE "") + "," +
		        (IF ttADDR.addr-kv     NE ? THEN ttADDR.addr-kv     ELSE "").
        	IF ttADDR.addr-type = vAddrMainType THEN DO ON ERROR UNDO,THROW:
            	    ASSIGN person.address[1] = vAddr.
                    PUT UNFORMATTED " country-id = " ttADDR.country-id.
                    PUT UNFORMATTED " ���� = "      vAddr.
                    PUT UNFORMATTED " ��������� = "  ttADDR.code-gni.
                    PUT UNFORMATTED " ������ = "     ttADDR.code-reg SKIP.
        	    UpdateSigns("person",
                         GetSurrogateBuffer("person",(BUFFER person:HANDLE)),
                         "���������", INPUT ttADDR.code-gni,?).
/*put unformatted ttADDR.code-gni "-###-" ttADDR.code-reg skip.*/
        	UpdateSigns("person",
                         GetSurrogateBuffer("person",(BUFFER person:HANDLE)),
                         "������", INPUT ttADDR.code-reg,?).
            	    RUN SyncAdrSubjToCident IN h_cust (
            		STRING(BUFFER person:HANDLE), "�",vDate, ttADDR.addr-type) .
            	    NEXT.
        	END.
    		FIND FIRST cust-ident USE-INDEX cust
    		 WHERE cust-ident.Class-Code     EQ "p-cust-adr"
            	   AND cust-ident.cust-cat       EQ "�"
	           AND cust-ident.cust-id        EQ person.person-id
	           AND cust-ident.cust-code      EQ STRING(YEAR (vDate),"9999") +
                                                    STRING(MONTH(vDate),"99")   +
                                                    STRING(DAY  (vDate),"99")
	           AND cust-ident.cust-code-type EQ ttADDR.addr-type
	           AND cust-ident.open-date EQ vDate
	           EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        	IF NOT AVAIL cust-ident THEN DO:
        	    IF LOCKED cust-ident
        	     THEN UNDO, THROW NEW Progress.Lang.AppError( " person-id=" + STRING(person.person-id) + " ������ �������஢��").
        	END.
        	IF NOT AVAIL cust-ident THEN DO ON ERROR UNDO, THROW:
	            CREATE cust-ident.
	    	    ASSIGN
	             cust-ident.cust-cat       = "�"
	             cust-ident.cust-id        = person.person-id
	             cust-ident.cust-type-num  = ?
	             cust-ident.cust-code-type = ttADDR.addr-type
	             cust-ident.open-date      = vDate
	             cust-ident.cust-code      = STRING(YEAR (vDate),"9999") +
		                                     STRING(MONTH(vDate),"99")   +
		                                     STRING(DAY  (vDate),"99")
		     cust-ident.Class-Code     = "p-cust-adr"
	             .
	        END.
	        ASSIGN
	             cust-ident.issue          = vAddr
	             
	          .
        	VALIDATE cust-ident.
    		UpdateSigns( "p-cust-adr",
                         GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                         "country-id",
                         ttADDR.country-id,
                         ?).

        	UpdateSigns("p-cust-adr",
                         GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                         "���������", INPUT ttADDR.code-gni,?).
/*put unformatted ttADDR.code-gni "###" ttADDR.code-reg skip.*/
        	UpdateSigns("p-cust-adr",
                         GetSurrogateBuffer("cust-ident",(BUFFER cust-ident:HANDLE)),
                         "������", INPUT ttADDR.code-reg,?).
                /* ����뢠�� ���� ���� �᫨ ���� */

    		FIND LAST cust-ident USE-INDEX cust
    		 WHERE cust-ident.Class-Code     EQ "p-cust-adr"
            	   AND cust-ident.cust-cat       EQ "�"
	           AND cust-ident.cust-id        EQ person.person-id
	           AND cust-ident.cust-code-type EQ ttADDR.addr-type
	           AND cust-ident.open-date < vDate
	           AND cust-ident.close-date EQ ?
	           EXCLUSIVE-LOCK NO-ERROR.
	        IF AVAIL cust-ident THEN DO:
	    	    ASSIGN
	    		cust-ident.close-date = vDate
	    		.
	    	    put unformatted "���� ���� ������ ������." skip.
		END.

	    END.
    END.
    /* {run-meth.i '"person"' "chkupd" '"pers-req"'} (recid(b-person)).^
    if return-value <> "" then undo,retry . */

    /* UNDO, THROW NEW Progress.Lang.AppError( "������ '" + mCliName + "' ������ � �ࠢ�筨�� �����⮢.").
*/

    put unformatted "���� ���������" skip.

    DEF VAR vContCode AS CHAR NO-UNDO.
    FIND LAST tmp-code WHERE
                 tmp-code.class    EQ "�த���"
         AND     tmp-code.code     EQ ttREQUEST.product-id
         AND     tmp-code.beg-date LE ttREQUEST.open-date
         AND (   tmp-code.end-date GE ttREQUEST.open-date
              OR tmp-code.end-date EQ ?)
	 NO-LOCK NO-ERROR.
    IF NOT AVAIL tmp-code THEN
        UNDO, THROW NEW Progress.Lang.AppError(
        	"�� ������ �த�� " +
        	(IF ttREQUEST.product-id EQ ? THEN "?" ELSE ttREQUEST.product-id) +
        	" ��� �� ���� " + (IF ttREQUEST.open-date NE ? THEN STRING(ttREQUEST.open-date, "99/99/9999") ELSE "?") +
        	" �த�� �� �������.").

    IF GetXAttrInit(tmp-code.misc[1],"contract") NE "�।��" THEN 
	UNDO, THROW NEW Progress.Lang.AppError(
            "����� �த�� " + tmp-code.misc[1] + " �� ᮮ⢥����� ⨯� �।��").

    DEF VAR mClassCode AS CHAR NO-UNDO.
    mClassCode = IF {assigned tmp-code.misc[1]} THEN tmp-code.misc[1] 
                   ELSE "loan_allocat".
    /*------------------ ��᢮���� ����� ������� ------------------*/
    /* ��. 蠡�����->蠡�����, ��. counters->�����稪
       [�����]-[�����]-[����]
     */
    DEF VAR cn AS CHAR NO-UNDO.
    DEF VAR vTemplate AS CHAR NO-UNDO.
    DEF VAR vCounter AS CHAR NO-UNDO.
    DEF VAR vNum AS INT64 NO-UNDO.
    DEF VAR numit AS INT64 NO-UNDO.
    numit = 0.
    DO WHILE True ON ERROR UNDO, THROW:
	/*IF vOld_Num EQ ? THEN DO:*/
        RUN GetClassTemplatePars in h_jloan (mClassCode,
                               OUTPUT vTemplate,
                               OUTPUT vCounter).
    	/*vContCode = ReplaceBasicTags(vTemplate,
                           vCounter,
                           ttREQUEST.open-date).
    	vNum = GetCounterCurrentValue (vCounter, ttREQUEST.open-date).
                           */
	vContCode = FGetSetting("����������", vTemplate, ?).
        IF vOld_Num EQ ? OR vOld_Num EQ 0 THEN DO ON ERROR UNDO, THROW:
	    vNum   = GetCounterNextValue(vCounter, ttREQUEST.open-date).
        END. ELSE DO ON ERROR UNDO, THROW:
    	    vNum = vOld_Num.
        END.
        IF vNum EQ 0 OR vNum EQ ? THEN
	    UNDO, THROW NEW Progress.Lang.AppError(
        	"�� ��᢮�� ����� �������").

        vContCode = ReplaceTag(vContCode, "�", STRING(vNum), NO).
/*put unformatted string(vNum) skip.*/
	/*vContCode = ReplaceTag(vContCode, "�", vCurrency, NO).
	vContCode = ReplaceTag(vContCode, "�", shFilial, YES).*/
	/*vContCode = ReplaceTag(vContCode, "v", IF loan.currency EQ "" THEN "�" ELSE "�", NO).*/
        FIND FIRST code
        WHERE code.class EQ '�த����'
         AND code.name EQ '�'
         AND CAN-DO( code.val, ttREQUEST.product-id) NO-LOCK NO-ERROR.
        IF NOT AVAIL code THEN DO:
          UNDO, THROW NEW Progress.Lang.AppError(
        	"�� ������ ⨯ �த�� � �����䨪��� �த���� " +
        	(IF ttREQUEST.product-id EQ ? THEN "?" ELSE ttREQUEST.product-id)
        	).
        END.
        vContCode = ReplaceTag(vContCode, "�", code.code, NO).
        FIND FIRST code
         WHERE code.class EQ '�த����'
        and code.name EQ '�'
        AND CAN-DO( code.val, ttREQUEST.branch-id) NO-LOCK NO-ERROR.
        IF NOT AVAIL code THEN DO:
            UNDO, THROW NEW Progress.Lang.AppError(
                "�� ������� ���ࠧ������� � �����䨪��� �த���� " +
                ttREQUEST.branch-id ).
        END.
        vContCode = addfiltoloan(ReplaceTag(vContCode, "�", code.code, NO),shFilial).
        FIND FIRST loan WHERE loan.cont-code EQ vContCode NO-LOCK NO-ERROR.
        IF NOT AVAIL loan THEN LEAVE.
        vOld_Num = ?.
        numit = numit + 1.
        IF numit > 9999 THEN
            UNDO, THROW NEW Progress.Lang.AppError(
                "���������� ��᢮��� ����� �������.").
    END.
    IF vNum EQ ? THEN
            UNDO, THROW NEW Progress.Lang.AppError(
                "���������� ��᢮��� ����� �������, ᪮॥ �ᥣ� �� ����饭 Iserv.").
put unformatted "����� ������� " string(vNum) " " vContCode skip.




    
    DEF VAR vend-date AS DATE NO-UNDO.
    DEF VAR vD AS INT NO-UNDO.
    DEF VAR vM AS INT NO-UNDO.
    DEF VAR vY AS INT NO-UNDO.
    DEF VAR vCred-Date AS INT NO-UNDO.

    vCred-Date = (IF DAY(ttREQUEST.open-date) > 20 THEN DAY(ttREQUEST.open-date) - 11 ELSE DAY(ttREQUEST.open-date)).
/*ttREQUEST.open-date.*/

/*vCred-Date = DAY(ttREQUEST.open-date).*/

    vD = vCred-Date /*DAY(   ttREQUEST.open-date)*/.
    vM = MONTH( ttREQUEST.open-date) + ttREQUEST.cred-term.
    vY = YEAR(  ttREQUEST.open-date).
    DO WHILE vM > 12:
	vY = vY + 1.
	vM = vM - 12.
    END.
    ttREQUEST.end-date = DATE( vM, vD, vY).
    DO WHILE {holiday.i ttREQUEST.end-date}:
	ASSIGN
         ttREQUEST.end-date = ttREQUEST.end-date + 1.
    END.

    /* vContCode = '45-00-99999-����'.*/
/*put unformatted "ᮧ���� " delfilfromloan(vContCode) skip.*/
    CREATE loan.
    /*ASSIGN
	    loan.filial-id = ShFilial.*/
    ASSIGN
	    loan.branch-id   = (IF shFilial EQ "0000" THEN "0000" ELSE ttREQUEST.branch-id) /*getThisUserXAttrValue("�⤥�����") v.mik */
	    /* loan.doc-num     = vContCode*/ 
	    loan.cont-code   = vContCode
	    loan.currency    = ""
	    loan.contract    = "�।��"
	    loan.class-code  = mClassCode
    	    loan.cust-cat    = '�'
    	    loan.cust-id     = person.person-id
	    loan.loan-status = '����'
	    loan.doc-ref     = delfilfromloan(vContCode)
	    loan.cont-type   = (IF {assigned tmp-code.misc[3]} THEN tmp-code.misc[3] ELSE "��⮏���")
	    loan.open-date   = ttREQUEST.open-date
	    loan.end-date    = ttREQUEST.end-date
	    loan.since       = loan.open-date
	    /* loan.gr_riska    = 1 */
	    loan.user-id     = 'SERV0400'
	    .
    VALIDATE loan.
/*put unformatted "ᮧ���� " delfilfromloan(vContCode) skip.*/
    DEF VAR vSurr AS CHAR NO-UNDO.
    DEF VAR vSurrLoanCond AS CHAR NO-UNDO.
    DEF VAR vSurrCommRate AS CHAR NO-UNDO.
    DEF VAR vErr AS CHAR NO-UNDO.
    vSurr = GetSurrogateBuffer("loan",(BUFFER loan:HANDLE)).
	/*------- �����㥬 ��易⥫�� ���.४������ �� ������� -------*
	FOR EACH xattr WHERE xattr.class-code EQ loan.class-code
                    AND xattr.mandatory  EQ YES
	       NO-LOCK:
		IF GetXAttrValue ("loan",vSurr,xattr.xattr-code) NE "" THEN
		    NEXT.
		UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  xattr.xattr-code,
                  GetXAttrInit(loan.Class-Code,xattr.xattr-code),
                  ?) NO-ERROR.
put unformatted "�ய�ᠫ� ��� " xattr.xattr-code "=" GetXAttrInit(loan.Class-Code,xattr.xattr-code) skip.
		IF ERROR-STATUS:ERROR THEN DO:
    		    vErr = "������� " + vContCode + ". " + ERROR-STATUS:GET-MESSAGE(1).
    		    UNDO, THROW NEW Progress.Lang.AppError( vErr).
    		    * RUN Fill-SysMes IN h_tmess("","",1,vErr).*
		    NEXT.
		END.
	END.*/

/*
	IF ERROR-STATUS:ERROR THEN DO:
		RUN Fill-SysMes IN h_tmess("","",1, ERROR-STATUS:GET-MESSAGE(1)).
		NEXT.
	END.
	UpdateSigns(loan.Class-Code,
               loan.contract + "," + loan.cont-code,
               "��卮���",
               STRING(ttLoan.ish-num),
               ?).
	UpdateSigns(xloan.class-code,
               xloan.contract + "," + xloan.cont-code,
               "��周�⥬�",
               STRING(ttloan.ish-sys),
               ?).
	UpdateSigns(xloan.class-code,
               xloan.contract + "," + xloan.cont-code,
               "CustImpID",
               ttLoan.CustCesID,
               ?).
*/
	UpdateSigns(loan.class-code,
               loan.contract + "," + loan.cont-code,
               "AgrCounter",
               STRING(vNum),
               ?).
	UpdateSigns(loan.class-code,
               loan.contract + "," + loan.cont-code,
               "PLDealID",
               STRING(ttREQUEST.claim-id),
               ?).
        IF ttREQUEST.claim-date NE ? THEN
	UpdateSigns(loan.class-code,
               loan.contract + "," + loan.cont-code,
               "PLDealDate",
               STRING(ttREQUEST.claim-date),
               ?).
	UpdateSigns(loan.class-code,
               loan.contract + "," + loan.cont-code,
               "PLDealSalePoint",
               ttREQUEST.branch-id,
               ?).
	UpdateSigns(loan.class-code,
               loan.contract + "," + loan.cont-code,
               "��⠑���",
               STRING(loan.open-date),
               ?).
	UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "�த���",
                  ttREQUEST.product-id,
                  ?).

    FIND FIRST ttCONTRACT
     WHERE ttREQUEST.request-id EQ ttCONTRACT.request-id
       AND ttCONTRACT.contract-type EQ "���" NO-LOCK NO-ERROR.
    IF NOT AVAIL ttCONTRACT THEN
        UNDO, THROW NEW Progress.Lang.AppError( "� ��� ���������� ४������ ������� �㯫�-�த���.").
    /* 業� ��⮬����� */
    UpdateSigns( loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "rko11_price",
                  STRING(ttREQUEST.price-avto),
                  ?).
    /* 業� ��⮬����� � ��� �।�� */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "rko1_price",
                  STRING(ttREQUEST.sum-kred-avto),
                  ?).
    FIND FIRST ttCONTRACTSUM
              WHERE ttCONTRACTSUM.request-id = ttCONTRACT.request-id
                AND ttCONTRACTSUM.contract-type = ttCONTRACT.contract-type
        	AND ttCONTRACTSUM.sum-type = '����_���' NO-LOCK NO-ERROR.
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "rko1_nds",
                  IF AVAIL ttCONTRACTSUM
                   THEN (IF ttCONTRACTSUM.summa NE ? THEN STRING(ttCONTRACTSUM.summa) ELSE "0")
                   ELSE "0",
                  ?).
    /* N ������� �㯫� �த���  */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "rko2_N_DKP",
                  ttCONTRACT.contract-num,
                  ?).
    /* ��� ������� �㯫� �த���  */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "rko3_DATE_DKP",
                  STRING( ttCONTRACT.contract-date, "99/99/9999"),
                  ?).
    IF ttCONTRACT.contract-invoice-num <> ? AND TRIM(ttCONTRACT.contract-invoice-num) <> '' THEN DO:
	    /* N ��� �� ������ ��⮬�����  */
	    UpdateSigns( loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "rko_invoice", ttCONTRACT.contract-invoice-num, ?).
    END.
    IF ttCONTRACT.contract-invoice-date <> ? THEN DO:
	    /* ��� ��� �� ������ ��⮬�����  */
	    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "rko_invoice_date",
                  STRING( ttCONTRACT.contract-invoice-date, "99/99/9999"),
                  ?).
    END.
    /* ᮣ��ᨥ �� ���㧪� � ��� */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "�������",
                  (IF vSendBki THEN "��" ELSE "���"),
                  ?).

    /*  */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "Credmen",
                  "-",
                  ?).
    /* ४������ �����⥫� �।�� �� ��� */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "�����⥫�४�",
                  REPLACE( ttCONTRACT.contractor-bank-bik, '^', '`') + "^" +
                  REPLACE( ttCONTRACT.contractor-current-account, '^', '`') + "^" +
                  REPLACE( ttCONTRACT.contractor-inn, '^', '`') + "^" +
                  "^" +
                  REPLACE( ttCONTRACT.contractor-name, '^', '`') + "^"
                  ,
                  ?).
    /* ��� ��� (�த��) ��� ���⭮�� */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "tariff-code",
                  ttREQUEST.tariff-code,
                  ?).

    /* ������塞 ������� ���客���� */
    FOR EACH ttCONTRACT
     WHERE ttREQUEST.request-id EQ ttCONTRACT.request-id
       AND ttCONTRACT.contract-type NE "���"
      /* AND CAN-DO("����,�����", ttCONTRACT.contract-type)*/
        NO-LOCK ON ERROR UNDO,THROW:
	IF CAN-DO( "�1,�2,�1_��,�2_��,�����_�,�����_�,�����_GAP,�_����", ttCONTRACT.contract-type) THEN DO ON ERROR UNDO,THROW:
	    put unformatted "������塞 ������� ���客���� � ⨯�� " + ttCONTRACT.contract-type skip.
    	    /* ���� ���客�� �������� */
    	    DEF VAR vCustName AS CHAR NO-UNDO.
    	    DEF VAR vCustNameFull AS CHAR NO-UNDO.
    	    DEF VAR vStrPol_ID AS CHAR NO-UNDO.
    	    DEF VAR ccid LIKE cust-corp.cust-id NO-UNDO.
    	    ccid = ?.
    	    vStrPol_ID = ?.
    	    FOR EACH cust-corp
    	     WHERE cust-corp.inn EQ ttCONTRACT.contractor-inn
/*	      and cust-corp.benacct = ttCONTRACT.contractor-current-account	 */
    	      NO-LOCK ON ERROR UNDO, THROW:
		vCustName = cust-corp.name-short.
		RUN RE_CLIENT_LARGE( "�",cust-corp.cust-id, INPUT-OUTPUT vCustNameFull).
	        IF CAPS(ttCONTRACT.contractor-name) EQ CAPS(vCustName) OR 
    	           CAPS(ttCONTRACT.contractor-name) EQ CAPS(vCustNameFull) THEN DO:
    	           ccid = cust-corp.cust-id.
    	           LEAVE.
    	        END.
    	    END.
    	    IF ccid NE ? THEN
    		FIND FIRST cust-corp WHERE cust-corp.cust-id EQ ccid NO-LOCK NO-ERROR.
    	    ELSE RELEASE cust-corp.
    	    IF NOT AVAIL cust-corp THEN DO ON ERROR UNDO, THROW:
    		FIND FIRST cust-corp
    	    	    WHERE cust-corp.inn EQ ttCONTRACT.contractor-inn NO-LOCK NO-ERROR.
    	        
	        IF AVAIL cust-corp THEN
    	            UNDO, THROW NEW Progress.Lang.AppError( "������� ���客���� '" + ttCONTRACT.contract-type +
    	                    "' �� ᮢ������ ������������ ��.��� (��� " + 
    			    STRING(ttCONTRACT.contractor-inn) + " " + ttCONTRACT.contractor-name + ") 㪠������ � ᮣ��襭�� � ����묨 ��� (" +
    			    vCustName + " ��� " + vCustNameFull + ")").
    	    END.
    	    IF NOT AVAIL cust-corp THEN
    		UNDO, THROW NEW Progress.Lang.AppError( "������� ���客���� '" + ttCONTRACT.contract-type +
    	                    "' �� ������� ��.��� (��� " + 
    		    STRING(ttCONTRACT.contractor-inn) + ") 㪠������ � ᮣ��襭��.").
    	    /*
	    IF ttCONTRACT.contractor-current-account NE cust-corp.benacct OR
	       ttCONTRACT.contractor-bank-bik NE cust-corp.bank-code THEN DO:
	        * �饬 ४������ � �����䨪��� *
	        FIND FIRST code
	         WHERE code.class EQ 'strahpol'
	           *AND ttCONTRACT.contractor-name            EQ code.description[1]*
	           AND ttCONTRACT.contractor-inn             EQ code.name
	           AND ttCONTRACT.contractor-current-account EQ code.description[2]
	           AND ttCONTRACT.contractor-bank-bik        EQ code.misc[3]
	            NO-LOCK NO-ERROR.
	        IF NOT AVAIL code THEN
    		    UNDO, THROW NEW Progress.Lang.AppError(
    		        "�� ᮢ������ ४������ ��.��� (��� " + 
    		        STRING(ttCONTRACT.contractor-inn) +
    		        "), 㪠����� � ᮣ��襭��, � ����묨 ���").
    		vStrPol_ID = code.code.
	    END.*/

    	    DEF BUFFER insur_loan FOR LOAN.
    	    DEF VAR insur-doc-ref AS CHAR NO-UNDO.
    	    DEF VAR insurtmp AS CHAR NO-UNDO.
    	    IF (ttCONTRACT.contract-type = "�����_GAP" OR ttCONTRACT.contract-type = "�����_�") THEN insur-doc-ref = loan.doc-ref + "-" + "��".
    	    ELSE
    	    insur-doc-ref = loan.doc-ref + "-" +
    		 (IF ttCONTRACT.contract-type EQ "�����" THEN "�" ELSE ttCONTRACT.contract-type).
    		 
    		CASE ttCONTRACT.contractor-inn: /* ��᮫�� */
    		    WHEN "7728178835" THEN DO:
    		        insurtmp = loan.doc-ref.
    		        IF LENGTH(insurtmp) > 12 THEN insurtmp = SUBSTRING(insurtmp,1,12).
    		        insur-doc-ref = "001-NSPI-" + insurtmp + "/" + SUBSTRING(STRING(YEAR(TODAY),"9999"),3).
    		    END.
    		    WHEN "1657023630" THEN DO:  /* ����� */
    		        insur-doc-ref = "�� " + loan.doc-ref + "-��". 
    		    END.     
    		    WHEN "1435159327" THEN DO:  /* ����� */
                    insur-doc-ref = "���� " + loan.doc-ref + "-��". 
                END. 
    		END CASE.


	    CREATE insur_loan.
    	    ASSIGN
            /*insur_loan.filial-id = loan.filial-id*/
            insur_loan.branch-id   = loan.branch-id
            insur_loan.class-code  = "insurance"
            insur_loan.cont-code   = addfiltoloan(insur-doc-ref,shFilial)
            insur_loan.contract    = "�����"
            insur_loan.parent-cont-code   = loan.cont-code
            insur_loan.parent-contract    = loan.contract
            insur_loan.currency    = ""
            insur_loan.cust-cat    = '�'
            insur_loan.cust-id     = cust-corp.cust-id
            insur_loan.loan-status = loan.loan-status
            insur_loan.doc-ref     = insur-doc-ref
            insur_loan.open-date   = ttCONTRACT.contract-date
            insur_loan.end-date    = ttCONTRACT.contract-end-date
	    insur_loan.user-id     = 'SERV0400'
            .
    	    VALIDATE insur_loan.
    	    DEF VAR vstr AS CHAR NO-UNDO.
    	    CASE LOOKUP( ttCONTRACT.contract-type, "�1,�2,�����_�,�����_�,�����_GAP,�_����,�1_��,�2_��"):
    	    WHEN 1 THEN vstr = "�����1".
    	    WHEN 2 THEN vstr = "�����2".
    	    WHEN 3 THEN vstr = "�����_�".
    	    WHEN 4 THEN vstr = "�����_�".
    	    WHEN 5 THEN vstr = "�����_GAP".
    	    WHEN 6 THEN vstr = "����삭��".
    	    WHEN 7 THEN vstr = "�����1_��".
    	    WHEN 8 THEN vstr = "�����2_��".
    	    OTHERWISE
    	    UNDO, THROW NEW Progress.Lang.AppError(
    		        "�� ������� ⨯ ����ࠪ� " + ttCONTRACT.contract-type).
    	    END.
    	    UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "VidStr", vstr,
                  ?).
    	    UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "PLpolis", ttCONTRACT.contract-num,
                  ?).
    	    IF vStrPol_ID NE ? THEN
    		UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "�������६", vStrPol_ID,
                  ?).
    	    UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "nds", string(ttCONTRACT.contract-nds),
                  ?).
	    /* ४������ �����⥫� ���客�� �६�� */
	    UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "�����⥫�४�",
                  REPLACE( ttCONTRACT.contract-bank-bik, '^', '`') + "^" +
                  REPLACE( ttCONTRACT.contract-current-account, '^', '`') + "^" +
                  REPLACE( ttCONTRACT.contract-inn, '^', '`') + "^" +
                  "^" +
                  REPLACE( ttCONTRACT.contract-name, '^', '`') + "^"
                  ,
                  ?).
                  
        /* ४������ �����⥫� ���客�� �६�� */
        UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "�����⥫�४�2",
                  REPLACE( ttCONTRACT.curr-payments-account, '^', '`') + "^" +
                  REPLACE( ttCONTRACT.curr-premium-account, '^', '`') + "^" +
                  STRING( ttCONTRACT.other-payments-summa) + "^" +
                  STRING( ttCONTRACT.insurance-premium-summa) + "^"
                  ,
                  ?).
                  
	    /* ����祭�� � ���� ��
	     1 - �룮���ਮ���⥫� ����
	     2 - �룮���ਮ���⥫� ����騪
	     */
	    DEF VAR bfic AS CHAR NO-UNDO.
	    bfic = ?.
	    if CAN-DO( "�1,�2,�1_��,�2_��", ttCONTRACT.contract-type) AND 
	       CAN-DO( "*-��*,*-��*", insur_loan.cont-code) THEN
	    bfic =  "���".
	    ELSE
	    if CAN-DO( "�����*", ttCONTRACT.contract-type) AND 
	       CAN-DO( "*-��*,*-��*", insur_loan.cont-code) THEN
	    bfic =  "��".
	    	    ELSE
	    if CAN-DO( "�_����*", ttCONTRACT.contract-type) AND 
	       CAN-DO( "*-��*,*-��*", insur_loan.cont-code) THEN
	    bfic =  "���".
	    	    ELSE
	    if CAN-DO( "�_����", ttCONTRACT.contract-type) AND 
	       CAN-DO( "*-���*", insur_loan.cont-code) THEN
	    bfic =  "���".
	    ELSE
	    if CAN-DO( "�1,�2,�1_��,�2_��", ttCONTRACT.contract-type) AND 
	       CAN-DO( "*-���*", insur_loan.cont-code) THEN
	    bfic =  "���".
	    ELSE
	    if CAN-DO( "�����*", ttCONTRACT.contract-type) AND 
	       CAN-DO( "*-���*", insur_loan.cont-code) THEN
	    bfic =  "��".

/*	    UpdateSigns( insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "��������",
                  (if
                    ttCONTRACT.contract-beneficiary EQ '2'
                    then '���' else '��'),
                  ?).
*/
/* kam */
	    UpdateSigns( insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "��������","���",
                  ?).
/* end of kam */

    	    UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  "DogDateColl", string(ttCONTRACT.CollAgrNum),
                  ?).


	    /* put unformatted insur_loan.cont-code " " GetXAttrValue("loan", GetSurrogate("loan",ROWID(insur_loan)), "��������") skip. */
/*
	    IF bfic NE ? AND
		bfic NE GetXAttrValue("loan", GetSurrogate("loan",ROWID(insur_loan)), "��������")
	    THEN
    		UNDO, THROW NEW Progress.Lang.AppError(
    		        "�� ���४⭮� ���祭�� �����樠� ������� ���客���� (" + ttCONTRACT.contract-beneficiary + ")").
*/
            FOR EACH ttCONTRACTSUM
              WHERE ttCONTRACTSUM.request-id = ttCONTRACT.request-id
                AND ttCONTRACTSUM.contract-type = ttCONTRACT.contract-type:
              IF ttCONTRACTSUM.sum-type = '����' THEN DO ON ERROR UNDO, THROW:
    		/* ������塞 � ��䨪 ���� ���⥦ �� ���客�� */
	        CREATE term-obl.
	        ASSIGN
		    term-obl.class-code = 'obl-insurance'
		    term-obl.contract   = insur_loan.contract
		    term-obl.cont-code  = insur_loan.cont-code
		    term-obl.idnt       = 1
		    /*term-obl.symbol     = loan.cust-cat*/
		    term-obl.fop        = 0
		    term-obl.amt-rub    = ttCONTRACTSUM.summa
		    term-obl.currency   = loan.currency
		    term-obl.acct       = ""
		    /*term-obl.fop-date   = loan.open-date*/
		    term-obl.end-date   = insur_loan.open-date
		    /*term-obl.sop-date   = ?*/
		    term-obl.sop-offbal = 0
		    term-obl.fop-offbal = 0
		    term-obl.nn         = 0
		    .
		VALIDATE term-obl.
              END.
               DEF VAR ttstr AS CHAR NO-UNDO.
              ttstr = "��" + ttCONTRACTSUM.sum-type.
              IF ttstr = '�㬑��客��_GAP' THEN ttstr = '�㬑��客��_���'.
              IF ttstr = '�㬏६��_GAP' THEN ttstr = '�㬏६��_���'.
	      IF ttstr = '�㬎���_���' THEN ttstr = 'nds'.
    	      UpdateSigns(insur_loan.class-code,
                  GetSurrogate("loan",ROWID(insur_loan)),
                  ttstr, STRING(ttCONTRACTSUM.summa),
                  ?).

            END.

	    /* ������塞 � ��� �����客����� ��� */
	    DEFINE VARIABLE vCust_Name AS CHARACTER  NO-UNDO.
	    DEFINE VARIABLE vINN AS CHARACTER  NO-UNDO.
	    DEFINE VARIABLE vKPP AS CHARACTER  NO-UNDO.
	    DEFINE VARIABLE vAddress AS CHARACTER  NO-UNDO.
	    DEFINE VARIABLE vType AS CHARACTER  NO-UNDO.
	    DEFINE VARIABLE vCode AS CHARACTER  NO-UNDO.
	    DEFINE VARIABLE vAcct AS CHARACTER  NO-UNDO.
	    vCust_Name  = GetCliName(loan.cust-cat,
                          STRING (loan.cust-id),
                          OUTPUT vAddress,
                          OUTPUT vINN,
                          OUTPUT vKPP,
                          INPUT-OUTPUT vType,
                          OUTPUT vCode,
                          OUTPUT vAcct).
	    CREATE cust-role.
	    ASSIGN
             cust-role.country-id       = GetValueAttr(ENTRY(LOOKUP(loan.cust-cat,"�,�,�"),"cust-corp,banks,person"),
                                                                 STRING(loan.cust-id),
                                                                 "country-id")
             cust-role.Class-Code       = "�����客����"
             cust-role.file-name        = "loan"
             cust-role.cust-cat         = loan.cust-cat
             cust-role.cust-id          = STRING (loan.cust-id)
             cust-role.address          = vAddress
             cust-role.cust-name        = vCust_Name
             cust-role.inn              = vINN
             cust-role.kpp              = vKPP
             cust-role.open-date        = insur_loan.open-date
             cust-role.cust-code-type   = vType
             cust-role.cust-code        = vCode
             cust-role.corr-acct        = vAcct WHEN {assigned vAcct}
             cust-role.surrogate        = insur_loan.contract + "," + insur_loan.cont-code
          .
        END. /* ELSE 
    	    UNDO, THROW NEW Progress.Lang.AppError(
    		        "�� ������� ⨯ ����ࠪ� " + ttCONTRACT.contract-type).
*/
	IF CAN-DO( "VIPAssistance*", ttCONTRACT.contract-type) THEN DO ON ERROR UNDO,THROW:
	    put unformatted "������塞 �����⥫� ��� VIP Assistance " + ttCONTRACT.contract-type skip.
	    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "VIP_Assis_cred",
                  (IF ttCONTRACT.contract-type EQ "VIPAssistance_�" THEN "��" ELSE "���"),
                  ?).
	    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "VIP_Assis_rekv",
                  sGetEmty(REPLACE( ttCONTRACT.contract-bank-bik, '^', '`')) + "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-current-account, '^', '`')) + "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-inn, '^', '`')) + "^" +
                  "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-name, '^', '`')) + "^",
                  ?).
	END.
	
	IF CAN-DO( "BlackEdition*", ttCONTRACT.contract-type) THEN DO ON ERROR UNDO,THROW:
        put unformatted "������塞 �����⥫� ��� BlackEdition " + ttCONTRACT.contract-type skip.
        UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "BlackEdit_cred",
                  (IF ttCONTRACT.contract-type EQ "BlackEdition_�" THEN "��" ELSE "���"),
                  ?).
        UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "BlackEdit_rekv",
                  sGetEmty(REPLACE( ttCONTRACT.contract-bank-bik, '^', '`')) + "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-current-account, '^', '`')) + "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-inn, '^', '`')) + "^" +
                  "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-name, '^', '`')) + "^",
                  ?).
       END.

	IF CAN-DO( "PPO*", ttCONTRACT.contract-type) THEN DO ON ERROR UNDO,THROW:
        put unformatted "������塞 �����⥫� ��� PPOCARD " + ttCONTRACT.contract-type skip.
        UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "PROCARD_cred",
                  (IF ttCONTRACT.contract-type EQ "PPO_K" THEN "��" ELSE "���"),
                  ?).
        UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "PROCARD_rekv",
                  sGetEmty(REPLACE( ttCONTRACT.contract-bank-bik, '^', '`')) + "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-current-account, '^', '`')) + "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-inn, '^', '`')) + "^" +
                  "^" +
                  sGetEmty(REPLACE( ttCONTRACT.contract-name, '^', '`')) + "^",
                  ?).
	UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "ProCard_dogno",
                  ttCONTRACT.contract-num,
                  ?).
       END.



    END.

    /* ������塞 ����� ����� �� �த��� VIP Assistance */
    FIND FIRST ttCARD
	 WHERE ttCARD.request-id = ttREQUEST.request-id NO-LOCK NO-ERROR.
    IF AVAIL ttCARD THEN DO:
	put unformatted " ������塞 ����� ����� �� �த��� VIP Assistance" SKIP.
	UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "VIP_Assis_cardno",
                  ttCARD.card-vin,
                  ?).
	UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "VIP_Assis_dogno",
                  ttCARD.card-number,
                  ?).
	UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "VIP_Assis_date",
                  STRING( ttCARD.card-date, "99/99/9999"),
                  ?).
	UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "VIP_Assis_sum",
                  STRING( ttCARD.card-price),
                  ?).
	UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "VIP_Assis_term",
                  STRING( ttCARD.card-term),
                  ?).
    END.
    
    /* ������塞 ����� ����� �� �த��� BlackEdit */
    FIND FIRST ttBLACKCARD
     WHERE ttBLACKCARD.request-id = ttREQUEST.request-id NO-LOCK NO-ERROR.
    IF AVAIL ttBLACKCARD THEN DO:
    put unformatted " ������塞 ����� ����� �� �த��� BlackEdit" SKIP.
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "BlackEdit_cardno",
                  ttBLACKCARD.card-vin,
                  ?).
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "BlackEdit_dogno",
                  ttBLACKCARD.card-number,
                  ?).
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "BlackEdit_date",
                  STRING( ttBLACKCARD.card-date, "99/99/9999"),
                  ?).
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "BlackEdit_sum",
                  STRING( ttBLACKCARD.card-price),
                  ?).
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "BlackEdit_term",
                  STRING( ttBLACKCARD.card-term),
                  ?).
    END.

    /* ������塞 ����� ����� �� �த��� PROCARD */
    FIND FIRST ttPROCARD
     WHERE ttPROCARD.request-id = ttREQUEST.request-id NO-LOCK NO-ERROR.
    IF AVAIL ttPROCARD THEN DO:
    put unformatted " ������塞 ����� ����� �� �த��� ProCard" SKIP.
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "ProCard_cardno",
                  ttPROCARD.card-vin,
                  ?).
/*
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "ProCard_dogno",
                  ttPROCARD.card-number,
                  ?).
*/
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "ProCard_date",
                  STRING( ttPROCARD.card-date, "99/99/9999"),
                  ?).
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "ProCard_sum",
                  STRING( ttPROCARD.card-price),
                  ?).
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "ProCard_term",
                  STRING( ttPROCARD.card-term),
                  ?).
    END.

    
    /*------- ᮧ���� �᫮��� �� ������� -------*/
    FIND FIRST loan-cond WHERE     loan-cond.contract  EQ loan.contract
                                 AND loan-cond.cont-code EQ loan.cont-code
                                 AND loan-cond.since     EQ loan.open-date
	  EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL loan-cond THEN DO:
        CREATE loan-cond NO-ERROR.
        ASSIGN
         loan-cond.contract    = loan.contract
         loan-cond.cont-code   = loan.cont-code
         loan-cond.since       = loan.open-date
         loan-cond.class-code  = "an-cond"
        .
    END.
    vSurrLoanCond = GetSurrogate("loan-cond",ROWID(loan-cond)).
    /*------- �����㥬 ��易⥫�� ���.४������ �� �᫮��� -------*/
    FOR EACH xattr WHERE xattr.class-code EQ loan-cond.class-code
                     AND xattr.mandatory  EQ YES 
                     AND xattr.system EQ NO
	       NO-LOCK:
		IF GetXAttrValue ("loan-cond",vSurrLoanCond,xattr.xattr-code) NE "" THEN
		    NEXT.
		UpdateSigns(loan-cond.class-code,
                  vSurrLoanCond,
                  xattr.xattr-code,
                  GetXAttrInit(loan-cond.Class-Code,xattr.xattr-code),
                  ?) NO-ERROR.
		IF ERROR-STATUS:ERROR THEN DO:
    		    vErr = "������� " + vContCode + ". �᫮���:" + ERROR-STATUS:GET-MESSAGE(1).
    		    UNDO, THROW NEW Progress.Lang.AppError( vErr).
    		    /* RUN Fill-SysMes IN h_tmess("","",1,vErr).*/
		    NEXT.
		END.
	END.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "PayType", "�뤠�", ?).

	DEF VAR vIntOffs AS CHAR INIT "->" NO-UNDO.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "int-offset", vIntOffs, ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-mode",  GetXAttrEx(loan-cond.class-code,"cred-mode","initial"), ?).

	DEF VAR vCredOffs AS CHAR INIT "->" NO-UNDO.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-offset",      vCredOffs, ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "delay-offset",     "->", ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "delay-offset-int", "->", ?).

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-work-calend", "������", ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "end-date",         STRING( loan.end-date, "99/99/9999"), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "CondEndDate",      STRING( loan.end-date, "99/99/9999"), ?).

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "NDays", "0", ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "NMonthes", string(ttREQUEST.cred-term), ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "NYears", "0", ?).

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "�᪫���", "no", ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "�奬�����", "�����⭠�", ?).

	/*UpdateSigns("loan-cond", vSurrLoanCond, "ProbegNextPlan", "���", ?) NO-ERROR.*/
	DEF VAR vKolLgtPer AS CHAR NO-UNDO.
	IF DAY(loan.open-date) > 20 
	THEN vKolLgtPer = "1". 
	ELSE vKolLgtPer = "0".

	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "�����⏥�",     vKolLgtPer, ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "�����⏥����",  "0", ?).
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "int-mode",       GetXAttrEx(loan-cond.class-code,"int-mode","initial"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "cred-curr-next", GetXAttrEx(loan-cond.class-code,"cred-curr-next","initial"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "int-curr-next",  GetXAttrEx(loan-cond.class-code,"int-curr-next","initial"),  ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "DateDelay",      GetXAttrEx(loan-cond.class-code,"DateDelay","initial"), ?) NO-ERROR.
	UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "DateDelayInt",   GetXAttrEx(loan-cond.class-code,"DateDelayInt","initial"), ?) NO-ERROR.

	/*UpdateSigns(loan-cond.Class-Code, vSurrLoanCond, "�����",        "��", ?) NO-ERROR.*/

	DEF VAR vDisch-type AS INT NO-UNDO.
	vDisch-type = INT64(GetXattrInit(loan-cond.class-code,"disch-type")) NO-ERROR.
	IF NOT ERROR-STATUS:ERROR THEN
         loan-cond.disch-type = vDisch-type.
	ELSE 
         loan-cond.disch-type = 1.      

	ASSIGN
               loan-cond.cred-period = '�'
               /* loan-cond.cred-month  = INT64(ttPayrule.month) */
               loan-cond.cred-date   = vCred-Date /*(IF DAY(loan.open-date) > 20 THEN DAY(loan.open-date) - 11 ELSE DAY(loan.open-date))*/
               loan-cond.int-period  = '�'
               loan-cond.int-date    = vCred-Date /*(IF DAY(loan.open-date) > 20 THEN DAY(loan.open-date) - 11 ELSE DAY(loan.open-date))*/
               .

	DEF BUFFER xcomm-rate FOR comm-rate.
	FOR EACH comm-rate
	  WHERE comm-rate.filial-id EQ shFilial
	    AND comm-rate.since <= loan.open-date
	    AND comm-rate.currency EQ loan.currency
	    AND comm-rate.kau EQ '�த���,' + tmp-code.misc[7]
	    /* USE INDEX kau*/ NO-LOCK BREAK BY comm-rate.commission BY comm-rate.since:
	    IF NOT LAST-OF(comm-rate.commission) THEN NEXT.
	    /* IF comm-rate.rate-comm EQ 0 AND comm-rate.commission NE '%���' THEN NEXT. */
	    CREATE xcomm-rate.
	    ASSIGN
             xcomm-rate.commission = comm-rate.commission
             xcomm-rate.kau        = loan.contract + "," + loan.cont-code
             xcomm-rate.currency   = loan.currency
             xcomm-rate.acct       = "0"
             xcomm-rate.since      = loan.open-date
             xcomm-rate.rate-comm  = comm-rate.rate-comm
             xcomm-rate.rate-fixed = comm-rate.rate-fixed
             xcomm-rate.filial-id  = shFilial
             .
            IF comm-rate.commission EQ '%���' THEN DO:
		/* ��⠥� ������ ����᫥��� �� ������ࠬ ���客���� */
		DEF VAR vSumK AS DEC NO-UNDO.
		DEF VAR vSumKPerech AS DEC NO-UNDO.
		/*
		vSumK = round( ttREQUEST.sum-kred-avto * 0.0075, 2).
            	IF vSumK < 500 THEN vSumK = 500.
            	IF vSumK > 1500 THEN vSumK = 1500.
            	vSumKPerech = vSumK.
            	*/

            	vSumKPerech = 0.
            	/*
		FOR EACH ttCONTRACT
		 WHERE ttREQUEST.request-id EQ ttCONTRACT.request-id
		    NO-LOCK ON ERROR UNDO,THROW:
			IF CAN-DO( "�1,�2,�1_��,�2_��,�����_�", ttCONTRACT.contract-type) THEN DO ON ERROR UNDO,THROW:
			    vSumK = round( ttCONTRACT.contract-summa * 0.0075, 2).
            		    IF vSumK < 500 THEN vSumK = 500.
            		    IF vSumK > 1500 THEN vSumK = 1500.
            		    vSumKPerech = vSumKPerech + vSumK.
			END.
			IF CAN-DO( "���", ttCONTRACT.contract-type) THEN DO ON ERROR UNDO,THROW:
			    IF CAN-DO( "40817*,40820*",ttCONTRACT.contractor-current-account) THEN DO ON ERROR UNDO,THROW:
				IF NOT CAN-DO( "045209783,044599129,047106641", ttCONTRACT.contractor-bank-bik) THEN DO ON ERROR UNDO,THROW:
				    vSumK = round( ttREQUEST.sum-kred-avto * 0.0075, 2).
            			    IF vSumK < 500 THEN vSumK = 500.
            		    	    IF vSumK > 1500 THEN vSumK = 1500.
            		    	    vSumKPerech = vSumKPerech + vSumK.
			        END.
			    END. ELSE DO ON ERROR UNDO,THROW:
				vSumK = round( ttREQUEST.sum-kred-avto * 0.0075, 2).
            			IF vSumK < 500 THEN vSumK = 500.
            		        IF vSumK > 1500 THEN vSumK = 1500.
            		        vSumKPerech = vSumKPerech + vSumK.
            		    END.
			END.
			
		END.
		*/
                xcomm-rate.rate-comm = vSumKPerech.
                xcomm-rate.rate-fixed = YES.
            END.
            
            /*��������� � 05.11.2015*/
            IF comm-rate.commission EQ '%��' THEN
            DO:
               IF CAN-DO("*-��*",loan.cont-code) THEN
               DO:
                  xcomm-rate.rate-comm  = 0.00.
                  xcomm-rate.rate-fixed = YES.   
               END.
               ELSE IF CAN-DO("*-��*",loan.cont-code) THEN
               DO:
      	         IF loan.open-date GE DATE("05/11/2015") THEN 
      	         DO:
                    xcomm-rate.rate-comm  = ROUND(ttREQUEST.summa * 0.03, 2).
                    IF xcomm-rate.rate-comm GT 11000 THEN xcomm-rate.rate-comm = 11000.
                    xcomm-rate.rate-fixed = YES.
                  END.
                  ELSE
                  DO:
                     xcomm-rate.rate-comm  = ROUND(ttREQUEST.summa * 0.025, 2).
                     IF xcomm-rate.rate-comm GT 9500 THEN xcomm-rate.rate-comm = 9500.
                     xcomm-rate.rate-fixed = YES.
                  END.
               END.
               ELSE
               DO:
                  xcomm-rate.rate-comm = ROUND(ttREQUEST.summa * 0.025, 2).
                  IF xcomm-rate.rate-comm GT 7000 AND loan.open-date LT DATE("01/07/2014") 
                     THEN xcomm-rate.rate-comm = 7000.
                  IF xcomm-rate.rate-comm GT 9500 AND loan.open-date GE DATE("01/07/2014")
                     THEN xcomm-rate.rate-comm = 9500.
                  xcomm-rate.rate-fixed = YES.
               END.
               PUT UNFORMATTED loan.cont-code " %�� = " xcomm-rate.rate-comm ";" "ttREQUEST.summa = " ttREQUEST.summa SKIP.
            END.
            
            IF comm-rate.commission EQ '����-�' AND loan.open-date >= DATE(7,1,2014) THEN DO:
               xcomm-rate.rate-comm  = 0.054.
               xcomm-rate.rate-fixed = NO.
            END.
            
	    VALIDATE xcomm-rate.
	END.
	/* �᭮��� ��業�� */
	FIND FIRST comm-rate WHERE
                 comm-rate.commission EQ '%�।'
             AND comm-rate.acct       EQ "0"
             AND comm-rate.currency   EQ loan.currency
             AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code
             AND comm-rate.min-value  EQ 0.00
             AND comm-rate.period     EQ 0
             AND comm-rate.since      EQ loan.open-date
          EXCLUSIVE-LOCK NO-ERROR.
	IF NOT AVAIL comm-rate THEN
         CREATE comm-rate.
	ASSIGN
         comm-rate.commission = '%�।'
         comm-rate.kau        = loan.contract + "," + loan.cont-code
         comm-rate.currency   = loan.currency
         comm-rate.acct       = "0"
         comm-rate.since      = loan.open-date
         comm-rate.rate-comm  = ttREQUEST.rate-cred
         comm-rate.rate-fixed = NO
         comm-rate.filial-id  = shFilial
	.
	VALIDATE comm-rate.
	/* ����祭�� ��業�� */
	FIND FIRST comm-rate WHERE
                 comm-rate.commission EQ '%����'
             AND comm-rate.acct       EQ "0"
             AND comm-rate.currency   EQ loan.currency
             AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code
             AND comm-rate.min-value  EQ 0.00
             AND comm-rate.period     EQ 0
             AND comm-rate.since      EQ loan.open-date
          EXCLUSIVE-LOCK NO-ERROR.
	IF NOT AVAIL comm-rate THEN
         CREATE comm-rate.
	ASSIGN
         comm-rate.commission = '%����'
         comm-rate.kau        = loan.contract + "," + loan.cont-code
         comm-rate.currency   = loan.currency
         comm-rate.acct       = "0"
         comm-rate.since      = loan.open-date
         comm-rate.rate-comm  = ttREQUEST.rate-cred
         comm-rate.rate-fixed = NO
         comm-rate.filial-id  = shFilial
	.
	VALIDATE comm-rate.

	/*IF ttREQUEST.rate-rko NE ? THEN DO:
        FIND FIRST comm-rate WHERE
                 comm-rate.commission EQ '%���'
             AND comm-rate.acct       EQ "0"
             AND comm-rate.currency   EQ loan.currency
             AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code
             AND comm-rate.min-value  EQ 0.00
             AND comm-rate.period     EQ 0
             AND comm-rate.since      EQ loan.open-date
          EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL comm-rate THEN
            CREATE comm-rate.
        ASSIGN
         comm-rate.commission = '%���'
         comm-rate.kau        = loan.contract + "," + loan.cont-code
         comm-rate.currency   = loan.currency
         comm-rate.acct       = "0"
         comm-rate.since      = loan.open-date
         comm-rate.rate-comm  = ttREQUEST.rate-rko
         comm-rate.rate-fixed = YES
        .
        VALIDATE comm-rate.
	END.*/

	/* ���� ��䨪� */

	DEF VAR vCredSumm AS DEC NO-UNDO.
	DEF VAR vAnnuitCorr AS INT NO-UNDO.
	DEF VAR vSummDepos AS DEC NO-UNDO.
	DEF VAR vFirstPeriod AS INT NO-UNDO.
	DEF VAR vPartAmount AS DEC NO-UNDO.
	DEF VAR vAnnSumm AS DEC NO-UNDO.
        vCredSumm = ttREQUEST.summa.
	vAnnuitCorr  = INT(GetXattrValueEx("loan-cond",vSurrLoanCond,"����⊮��",?)).
	vSummDepos   = DEC(GetXAttrValueEx("loan",vSurr,"Sum-depos","0")).
	vFirstPeriod = INT(GetXattrValueEx("loan-cond",vSurrLoanCond,"FirstPeriod","0")).
	vPartAmount  = DEC(GetXattrValueEx("loan-cond",vSurrLoanCond,"PartAmount","0")).
	RUN CalcAnnuitet (loan.contract,
                              loan.cont-code,
                              loan.open-date,
                              loan.end-date,
                              vCredSumm,
                              ttREQUEST.rate-cred,
                              loan-cond.cred-date, 
                              loan-cond.cred-period,  
                              loan-cond.cred-month,
                              INT(vKolLgtPer),
                              STRING(LOOKUP(vCredOffs,"--,->,<-")),
                              vAnnuitCorr, 
                              vSummDepos,  
                              vFirstPeriod,
                              vPartAmount, 
                              OUTPUT vAnnSumm).
	/* ������뢠�� ��䨪 ���⥦�� �� ��業⠬. */
    CALCTERM:
    DO ON ERROR UNDO, THROW:
	UpdateSigns("loan-cond", vSurrLoanCond, "����⏫��", STRING(vAnnSumm), ?).
	RUN SetSysConf IN h_base ("������������� �� �������� �����",STRING(LOOKUP(vCredOffs,"--,->,<-"))).
	RUN SetSysConf IN h_base ("������� �� ��������� �����",STRING(LOOKUP(vIntOffs,"--,->,<-"))).
	RUN SetSysConf IN h_base ("����. �� �������� ����� ����.�����", "1").
	RUN SetSysConf IN h_base ("����. �� ����. ����� ����.�����", "1").
	/* ����� �뢮� �� �࠭ ��䨪�� ��� ���४�஢��  */
	/*DEF VAR mRisk AS DEC NO-UNDO.
	mRisk = LnRsrvRate(loan.contract, loan.cont-code, loan.open-date).*/
	RUN SetSysConf IN h_base ("�� �������� ������� �� �����", "��").
	RUN mm-to.p(RECID(loan),
                  RECID(loan-cond),
                   vCredSumm,
                   1,
                   YES,
                   YES,
                   YES,
                   YES,
                   ? /*mRisk*/,
                   1) NO-ERROR.
	/* ���⨬ ��... */
	RUN DeleteOldDataProtocol IN h_base ("������� �� ��������� �����").
	RUN DeleteOldDataProtocol IN h_base ("������������� �� �������� �����").
	RUN DeleteOldDataProtocol IN h_base ("����. �� �������� ����� ����.�����").
	RUN DeleteOldDataProtocol IN h_base ("����. �� ����. ����� ����.�����").
	RUN DeleteOldDataProtocol IN h_base ("�� �������� ������� �� �����").
	IF ERROR-STATUS:ERROR THEN
    	    UNDO, THROW NEW Progress.Lang.AppError( RETURN-VALUE).
    /* �� ࠡ�⠥�, ���.४����� ����뢠���� �����.
	DEF VAR vflagRecalc AS LOG NO-UNDO.
	vflagRecalc = false.
	FOR EACH term-obl
	 WHERE term-obl.contract EQ loan.contract
	   AND term-obl.cont-code EQ loan.cont-code
	   AND term-obl.idnt EQ 1
	   AND term-obl.end-date GE loan.open-date NO-LOCK:
	   IF vAnnSumm < term-obl.amt-rub THEN DO:
	    put unformatted "�㬬� ������ 㢥��祭� � " vAnnSumm " �� " term-obl.amt-rub skip.
	    vAnnSumm = term-obl.amt-rub.
	    vflagRecalc = true.
	   END.
	END.
	IF vflagRecalc THEN UNDO, RETRY CALCTERM.
    */
    END.
    
    put unformatted "������塞 ������� ���ᯥ祭��" skip.
    IF ttCAR.is-new EQ ? THEN
    	    UNDO, THROW NEW Progress.Lang.AppError( "�� �������� �ਧ��� ��� ����/����").

    CREATE term-obl.
    /* sphere-l.p */
    ASSIGN
	term-obl.class-code = 'term-obl-gar'
	term-obl.contract = loan.contract
	term-obl.cont-code = loan.cont-code
	term-obl.idnt = 5
	term-obl.symbol     = loan.cust-cat
	term-obl.fop        = loan.cust-id
	term-obl.amt-rub    = ttREQUEST.pledge-price-avto
	term-obl.currency   = loan.currency
	term-obl.acct       = ""
	term-obl.fop-date   = loan.open-date
	term-obl.end-date   = loan.end-date
	term-obl.sop-date   = ?
	term-obl.sop-offbal = 1
	term-obl.fop-offbal = 0
	term-obl.nn         = 0
	.
    VALIDATE term-obl.
    DEF VAR vSurrObesp AS CHAR NO-UNDO.
    vSurrObesp = GetSurrogate("term-obl",ROWID(term-obl)).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCVIN", ttCAR.VIN, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCbrand", ttCAR.brand, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCmodel", ttCAR.model, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCtype", ttCAR.type, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCclass", ttCAR.category, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCyear", ttCAR.create-year, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCmotor", ttCAR.engine-number, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCchassis", ttCAR.chassis-number, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCbody", ttCAR.body-number, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCcolor", ttCAR.tc-color, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCSER", ttCAR.pts-series, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCNUMB", ttCAR.pts-num, ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCDATE", STRING(ttCAR.pts-date, "99/99/9999"), ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCis-new", IF ttCAR.is-new THEN "��" ELSE "���", ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "TCmaxWeight", ttCAR.permissible-weight, ?).

    UpdateSigns( term-obl.class-code, vSurrObesp, "��������",
                     "�।��",
                     ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "�����",
                     "��⮬�����",
                     ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "�������",
                     "0",
                     ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "��������",
                     loan.doc-ref + "-���",
                     ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "��⠏���",
                     string(loan.open-date),
                     ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "CustSurr", loan.cust-cat + "," + STRING(loan.cust-id), ?).
    UpdateSigns( term-obl.class-code, vSurrObesp, "������", "���", ?).

    /*Set_QualityGar ("comm-rate",
                         vContract + "," + vContcode + ",5," + STRING(ttCollateral.end-date) + "," + STRING(term-obl.nn),
                         vQuality).*/

    /* ���뢠�� ��� */
	/* vAcctTypeList = GetXattrEx(loan.class-code,"acct-type-list","Initial"). */
    {ipl-acct.i &opkind = vOpKindAcct}

    /* �� */
    DEF VAR vEps AS DECIMAL NO-UNDO.
    RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.open-date, OUTPUT vEps).

    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "��",
                  trim(string( vEps, "->>>>9.999")),
                  ?).
    /* �� */
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "��",
                  trim(string( vEps, "->>>>9.999")),
                  ?).
    DEF VAR vEpsB AS DECIMAL NO-UNDO.
    DO ON ERROR UNDO, THROW:
	RUN SetSysConf IN h_base ("�᪡������","��").
	RUN pCalcEps117 IN h_pqres (loan.contract, loan.cont-code, loan.class-code, loan.open-date, OUTPUT vEpsB).
	FINALLY:
	    RUN DeleteOldDataProtocol IN h_base ("�᪡������").
	END.
    END.
    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "�᪡������",
                  trim( string( vEpsB, "->>>>9.999")),
                  ?).
    
         /* �������� � �ਢ離� ��⮢ 
      {sgn-acct.i &BufferLoan = "LOAN"
                  &OpenDate   = loan.open-date
                  &OUT-Error  = fl-error
      }
      IF fl-error EQ -1 THEN
         RETURN ERROR "��ࠡ�⪠ ��ࢠ��".*/

	/* ��⠥� ��䨪 �� �।�� */
	
/*
      FOR EACH xattr WHERE xattr.class-code EQ loan-cond.class-code
                       AND xattr.mandatory  EQ YES
      NO-LOCK:
         IF GetXAttrValue ("loan-cond",vSurrLoanCond,xattr.xattr-code) NE "" THEN
            NEXT.
         UpdateSigns(xloan.class-code,
                     vSurr,
                     xattr.xattr-code,
                     GetXAttrInit(loan-cond.Class-Code,xattr.xattr-code),
                     ?) NO-ERROR.
         IF ERROR-STATUS:ERROR
         THEN DO:
            vErr = "������� " + vContCode + ". " + ERROR-STATUS:GET-MESSAGE(1).
            RUN CrTTLogElement("NO",
                               "loan",
                               ttLoan.id,
                               ttLoan.action,
                               vContract + "," + vContCode,
                               vErr).
            RUN Fill-SysMes IN h_tmess("","",1,vErr).
            vFlagWarn = YES.
            NEXT.
         END.
      END.
         
      FOR EACH ttPayrule WHERE ttPayrule.TermID = ttTerm.id NO-LOCK:

         
         IF ttPayrule.type = "��" THEN
         DO:
            ASSIGN
               loan-cond.cred-period = ttPayrule.period
               loan-cond.cred-month  = INT64(ttPayrule.month)
               loan-cond.cred-date   = INT64(ttPayrule.day)
            NO-ERROR.
            IF ttPayrule.period-type = "0" OR ttPayrule.period-type = "1" THEN
               loan-cond.delay1 = INT64(ttPayrule.period-day).
            ELSE IF ttPayrule.period-type EQ "2" 
                 OR ttPayrule.period-type EQ "" THEN
               loan-cond.delay1 = INT64(ttPayrule.period-duration).
            IF loan-cond.cred-period EQ "�" THEN
               IF loan-cond.cred-date LT 1 OR
                  loan-cond.cred-date GT 7 THEN
            DO:
               mMsg = "� �᫮��� ������� " + vContCode + " � ���������� ��䨪�� �஡�� ����襭�� �।�� " + STRING(loan-cond.delay1) + " .�訡��".
               UNDO MAIN, RETRY MAIN.
            END.
            UpdateSigns("loan-cond",
                        vSurrLoanCond,
                        "cred-mode",
                        IF ttPayrule.period-type = "2" THEN "����焭��" ELSE "��⠎����",
                        ?).
            UpdateSigns("loan-cond",
                        vSurrLoanCond,
                        "cred-offset",
                        IF ttPayrule.shift = "0" THEN "--" ELSE IF ttPayrule.shift = "2" THEN "<-" ELSE "->",
                        ?).
            UpdateSigns("loan-cond", vSurrLoanCond, "delay-offset",
                        IF ttPayrule.period-shift = "0" THEN "--" ELSE IF ttPayrule.period-shift = "2" THEN "<-" ELSE "->",
                        ?).
            IF ttPayrule.period-type = "2" THEN
               UpdateSigns("loan-cond",
                           vSurrLoanCond,
                           "cred-work-calend",
                           IF ttPayrule.period-long-type = "0" THEN "�����" ELSE "������",
                           ?).
         END.
         ELSE IF ttPayrule.type = "���" THEN
         DO:
            ASSIGN
               loan-cond.int-period = ttPayrule.period
               loan-cond.int-month  = INT64(ttPayrule.month)
               loan-cond.int-date   = INT64(ttPayrule.day)
            NO-ERROR.

            IF ttPayrule.period-type = "0" OR ttPayrule.period-type = "1" THEN
               loan-cond.delay = INT64(ttPayrule.period-day).
            ELSE IF ttPayrule.period-type EQ "2" 
                 OR ttPayrule.period-type EQ "" THEN
               loan-cond.delay = INT64(ttPayrule.period-duration).

            UpdateSigns("loan-cond",
                        vSurrLoanCond,
                        "int-mode",
                        IF ttPayrule.period-type = "2" THEN "����焭��" ELSE "��⠎����",
                        ?).
            UpdateSigns("loan-cond",
                        vSurrLoanCond,
                        "int-offset",
                        IF ttPayrule.shift = "0" THEN "--" ELSE IF ttPayrule.shift = "2" THEN "<-" ELSE "->",
                        ?).
            UpdateSigns("loan-cond",
                        vSurrLoanCond,
                        "delay-offset-int",
                        IF ttPayrule.period-shift = "0" THEN "--" ELSE IF ttPayrule.period-shift = "2" THEN "<-" ELSE "->",
                        ?).
            IF ttPayrule.period-type = "2" THEN
               UpdateSigns("loan-cond",
                           vSurrLoanCond,
                           "int-work-calend",
                           IF ttPayrule.period-long-type = "0" THEN "�����" ELSE "������",
                           ?).
         END.
      END.
   END.
   FOR EACH ttRate NO-LOCK:
      IF ttRate.type EQ "%reserve%" THEN
         ttRate.type = "%���".
      FIND FIRST commission WHERE commission.commission EQ ttRate.fee-type
                              AND commission.currency   EQ xloan.currency
                              AND commission.min-value  EQ 0.00
                              AND commission.period     EQ 0
      NO-LOCK NO-ERROR.
      IF NOT AVAIL commission THEN DO:
         vErr = "������� " + xloan.cont-code + ". " + "��� ⠪�� �����ᨨ � �ࠢ�筨�� � ����� " +
                 ttRate.fee-type + " � ����� '" + xloan.currency +
                 "'. ������� �㤥� �ய�饭� " + ttRate.id.
         RUN Fill-SysMes IN h_tmess("","",1,vErr).
         vFlagWarn = YES.
         NEXT.
      END.
      FIND FIRST comm-rate WHERE
                 comm-rate.commission EQ ttRate.fee-type
             AND comm-rate.acct       EQ "0"
             AND comm-rate.currency   EQ xloan.currency
             AND comm-rate.kau        EQ xloan.contract + "," + xloan.cont-code
             AND comm-rate.min-value  EQ 0.00
             AND comm-rate.period     EQ 0
             AND comm-rate.since      EQ ttRate.fee-date
      EXCLUSIVE-LOCK NO-ERROR.
      
      IF     AVAIL comm-rate
         AND comm-rate.rate-comm NE DEC(IF ttRate.fee-amount EQ ? THEN ttRate.fee-rate ELSE ttRate.fee-amount) THEN
      DO:
         vErr = "������� " + vContCode + "." +
                " �������� �⠢�� �� �����ᨨ " + ttRate.id + " � " +
                STRING(comm-rate.rate-comm) + " �� " + STRING(DEC(IF ttRate.fee-amount EQ ? THEN ttRate.fee-rate ELSE ttRate.fee-amount)) +
                " � " + STRING(ttRate.since).
         RUN Fill-SysMes IN h_tmess("","",0,vErr).
         vFlagWarn = YES.
      END.
      ELSE IF NOT AVAIL comm-rate THEN
         CREATE comm-rate.
      ASSIGN
         comm-rate.commission = ttRate.fee-type
         comm-rate.kau        = xloan.contract + "," + xloan.cont-code
         comm-rate.currency   = xloan.currency
         comm-rate.acct       = "0"
         comm-rate.since      = ttRate.fee-date
         comm-rate.rate-comm  = DEC(IF ttRate.fee-amount EQ ? THEN ttRate.fee-rate ELSE ttRate.fee-amount)
         comm-rate.rate-fixed = IF ttRate.fee-amount EQ ? THEN NO ELSE YES
      no-error.
      IF ERROR-STATUS:ERROR
      THEN DO:
         RUN Fill-SysMes IN h_tmess("","",1,ERROR-STATUS:GET-MESSAGE(1)).
         NEXT.
      END.
      vSurrCommRate = GetSurrogate("comm-rate",ROWID(comm-rate)).
      UpdateSigns("comm-rate",
                  vSurrCommRate,
                  "comm-rate-min",
                  ttRate.min-fee-amt,
                  ?).
      UpdateSigns("comm-rate",
                  vSurrCommRate,
                  "comm-rate-curr",
                  ttRate.fee-val,
                  ?).
   END.
*/
/*    UpdateSigns(loan.class-code,
                  loan.contract + "," + loan.cont-code,
                  "�������",
                  "��",
                  ?).*/
	PUT UNFORMATTED
		"MsgID=" + string(wapp.messages.id) + ": ᮧ��� ������� '" + vContCode + "'" SKIP.
	PUT UNFORMATTED "��� ��� 㤠����� '" string(old_loan_acct) "'" SKIP.
	/* ������� ᮧ���, ����� ��������� 㤠���� ���� ��� � ��ண� ������ */
	DO i = 1 TO NUM-ENTRIES( old_loan_acct) ON ERROR UNDO, THROW:
	    FIND FIRST acct WHERE acct.acct EQ ENTRY( i, old_loan_acct).
	    IF (NOT CAN-FIND(FIRST loan-acct OF acct)) AND
	       (NOT CAN-FIND(FIRST op-entry WHERE op-entry.acct-db EQ acct.acct
	          AND op-entry.currency BEGINS acct.currency)) AND
	       (NOT CAN-FIND(FIRST op-entry WHERE op-entry.acct-cr EQ acct.acct
	          AND op-entry.currency BEGINS acct.currency))
	      THEN DO ON ERROR UNDO, THROW:
	        RUN DelSigns IN h_xclass ("acct",acct.acct + ',' + acct.currency).
		/* !!!!!!!!!!?????????????????!!!!!!!!!!! */
		PUT UNFORMATTED "㤠�塞 ��� " acct.acct SKIP.
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
	/*
	IF 
	*/
	
	
	/* �⢥� �� �ᯥ譮� ����㧪� */
	RUN XmlMsgAnswer( BUFFER wapp.messages, ttRequest.claim-id, ttRequest.task-id,
	 "0", "������� " + vContCode + " �������.").
	RUN XmlLoanInfo(BUFFER wapp.messages, ttRequest.claim-id, ttRequest.task-id).
	CATCH eAnyError2 AS Progress.Lang.AppError:
	    DEF VAR strr3 AS CHAR NO-UNDO.
	    strr3 = "(" + eAnyError2:ReturnValue + ")".
	    DO i = 1 TO eAnyError2:NumMessages:
		strr3 = strr3 + (IF i > 1 THEN ", " ELSE "") + eAnyError2:GetMessage(i).
	    END.
	    PUT UNFORMATTED 
		"MsgID=" + string(wapp.messages.id) + "^ Error=" + strr3 SKIP.
	    RUN XmlMsgAnswer( BUFFER wapp.messages, ttRequest.claim-id, ttRequest.task-id,
	     "-1", "��� �������: �訡�� ��������� ������� " + strr3).
	END CATCH.
	CATCH eAnyError3 AS Progress.Lang.Error:
	    DEF VAR strr4 AS CHAR NO-UNDO.
	    strr4 = "(" /*+ eAnyError3:ReturnValue RETURN-VALUE*/ + ")".
	    DO i = 1 TO /*ERROR-STATUS:NUM-MESSAGES*/ eAnyError3:NumMessages:
		strr4 = strr4 + (IF i > 1 THEN ", " ELSE "") + eAnyError3:GetMessage(i).
	    END.
	    PUT UNFORMATTED 
		"MsgID=" + string(wapp.messages.id) + ". Error=" + strr4 SKIP.
	    RUN XmlMsgAnswer( BUFFER wapp.messages, ttRequest.claim-id, ttRequest.task-id,
	     "-1", "��� �������: �訡�� ��������� ������� " + strr4).
	END CATCH.
    /*END. DO TRANSACTION */
  END.

