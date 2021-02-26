{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{intrface.get xclass}

DEF INPUT PARAM vTmpName AS CHAR NO-UNDO.

DEF VAR vOpenDate	AS CHAR	NO-UNDO.
DEF VAR vDovNom		AS CHAR	NO-UNDO.
DEF VAR vBnkVkl		AS CHAR NO-UNDO.
DEF VAR vBnkVklTmp  AS CHAR NO-UNDO.
DEF VAR vClosDate	AS CHAR	NO-UNDO.
DEF VAR vDovName	AS CHAR	NO-UNDO.
DEF VAR vDovBirth	AS CHAR	NO-UNDO.
DEF VAR vDovBirthPl	AS CHAR	NO-UNDO.
DEF VAR vDovTDoc	AS CHAR	NO-UNDO.
DEF VAR vDovNDoc	AS CHAR	NO-UNDO.
DEF VAR vDovDDoc	AS CHAR	NO-UNDO.
DEF VAR vDovKDoc	AS CHAR	NO-UNDO.
DEF VAR vDovAdr		AS CHAR	NO-UNDO.

DEF VAR vPolName	AS CHAR	NO-UNDO.
DEF VAR vPolBirth	AS CHAR	NO-UNDO.
DEF VAR vPolBirthPl	AS CHAR	NO-UNDO.
DEF VAR vPolTDoc	AS CHAR	NO-UNDO.
DEF VAR vPolNDoc	AS CHAR	NO-UNDO.
DEF VAR vPolDDoc	AS CHAR	NO-UNDO.
DEF VAR vPolKDoc	AS CHAR	NO-UNDO.
DEF VAR vPolAdr		AS CHAR	NO-UNDO.

DEF VAR vTmpDat		AS DATE NO-UNDO.
DEF VAR vTmpStr		AS CHAR NO-UNDO.
DEF VAR vTmpDec		AS DEC  NO-UNDO.
DEF VAR vTmpInt		AS INT  NO-UNDO.

DEF BUFFER ploan 	FOR loan.

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"
&GLOB Days "��ࢮ�,��஥,����,�⢥�⮥,��⮥,��⮥,ᥤ쬮�,���쬮�,����⮥,����⮥,��������⮥,�������⮥,�ਭ���⮥,���ୠ��⮥,~
��⭠��⮥,��⭠��⮥,ᥬ����⮥,��ᥬ����⮥,����⭠��⮥,�����⮥,������� ��ࢮ�,������� ��஥,������� ����,~
������� �⢥�⮥,������� ��⮥,������� ��⮥,������� ᥤ쬮�,������� ���쬮�,������� ����⮥,�ਤ�⮥,�ਤ��� ��ࢮ�"
&GLOB Years "��ࢮ��,��ண�,���쥣�,�⢥�⮣�,��⮣�,��⮣�,ᥤ쬮��,���쬮��,����⮣�,����⮣�,�������⮣�,�������⮣�,�ਭ���⮣�,~
���ୠ��⮣�,��⭠��⮣�,��⭠��⮣�,ᥬ����⮣�,��ᥬ����⮣�,����⭠�⮣�,�����⮣�,������� ��ࢮ��,������� ��ࢮ��,~
������� ��ண�,������� ���쥣�,������� �⢥�⮣�,������� ��⮣�,��⮣�,������� ᥤ쬮��,������� ���쬮��,������� ����⮣�,�ਤ�⮣�,�ਤ��� ��ࢮ��,~
�ਤ��� ��ࢮ��,�ਤ��� ��ண�,�ਤ��� ���쥣�,�ਤ��� �⢥�⮣�,�ਤ��� ��⮣�,�ਤ��� ��⮣�,�ਤ��� ᥤ쬮��,�ਤ��� ���쬮��,�ਤ��� ����⮣�"

FOR EACH tmprecid,
FIRST loan WHERE recid(loan) EQ tmprecid.id 
		   AND   loan.contract EQ "proxy" NO-LOCK:

	{empty ttnames}

	IF loan.cust-cat = "�"
	THEN DO:
		FIND FIRST cust-corp WHERE cust-corp.cust-id EQ loan.cust-id NO-LOCK NO-ERROR.
		IF AVAIL cust-corp THEN vDovName = cust-corp.name-short.
	END.
	IF loan.cust-cat = "�"
	THEN DO:
		FIND FIRST person WHERE person.person-id EQ loan.cust-id NO-LOCK NO-ERROR.
		IF AVAIL person THEN vDovName = person.name-last + " " + person.first-names.
			RUN Insert_TTName ("dfio",vDovName).
		vDovBirth = STRING(DAY(person.birthday)) + " " + ENTRY(MONTH(person.birthday),{&Months}) + " " + STRING(YEAR(person.birthday)).
			RUN Insert_TTName ("dbir",vDovBirth).
		vDovBirthPl = getxattrvalue ("person",string(person.person-id), "BirthPlace").
			RUN Insert_TTName ("dbipl",vDovBirthPl).
		vDovTDoc = GetCodeNameEx("�������", person.document-id, person.document-id).
   			RUN Insert_TTName ("ddtype",vDovTDoc).
		vDovNDoc = person.document.
			RUN Insert_TTName ("ddnu",vDovNDoc).
		vTmpDat = DATE(getxattrvalue ("person",string(person.person-id), "Document4Date_vid")).
		vDovDDoc = STRING(DAY(vTmpDat)) + " " + ENTRY(MONTH(vTmpDat),{&Months}) + " " + STRING(YEAR(vTmpDat)).
			RUN Insert_TTName ("ddvy",vDovDDoc).
		vDovKDoc = person.issue.
			RUN Insert_TTName ("ddkem",vDovKDoc).
		RUN RetAdr.p(loan.cust-id,loan.cust-cat,"����ய",?,OUTPUT vDovAdr).
			RUN Insert_TTName ("dadr",vDovAdr).
	END.

	vOpenDate = STRING(DAY(loan.open-date)) + " " + ENTRY(MONTH(loan.open-date),{&Months}) + " " + STRING(YEAR(loan.open-date)).
		RUN Insert_TTName ("date",vOpenDate).
	vTmpStr = ENTRY(DAY(loan.open-date),{&Days}) + " " + ENTRY(MONTH(loan.open-date),{&Months}) + " ��� ����� " + ENTRY(YEAR(loan.open-date) - 2000,{&Years})+ " ����".
		RUN Insert_TTName ("datep",vTmpStr).
	vTmpStr = STRING(DAY(loan.end-date)) + " " + ENTRY(MONTH(loan.end-date),{&Months}) + " " + STRING(YEAR(loan.end-date)).
		RUN Insert_TTName ("dsrok",vTmpStr).		
	vDovNom = loan.doc-num.
		RUN Insert_TTName ("ndov",vDovNom).


	vPolName = getxattrvalue ("loan",string("proxy," + loan.cont-code), "agent-id").
	FIND FIRST person where person.person-id = dec(vPolName) NO-LOCK NO-ERROR.
	IF AVAIL person then DO:
		vPolName = person.name-last + " " + person.first-names.
			RUN Insert_TTName ("pfio",vPolName).
		vPolBirth = STRING(DAY(person.birthday)) + " " + ENTRY(MONTH(person.birthday),{&Months}) + " " + STRING(YEAR(person.birthday)).
			RUN Insert_TTName ("pbir",vPolBirth).		
		vPolBirthPl = getxattrvalue ("person",string(person.person-id), "BirthPlace").
			RUN Insert_TTName ("pbipl",vPolBirthPl).		
		vPolTDoc = GetCodeNameEx("�������", person.document-id, person.document-id).
   			RUN Insert_TTName ("pdtype",vPolTDoc).
		vPolNDoc = person.document.
			RUN Insert_TTName ("pdnu",vPolNDoc).		
		vTmpDat = DATE(getxattrvalue ("person",string(person.person-id), "Document4Date_vid")).
		vPolDDoc = STRING(DAY(vTmpDat)) + " " + ENTRY(MONTH(vTmpDat),{&Months}) + " " + STRING(YEAR(vTmpDat)).
			RUN Insert_TTName ("pdvy",vPolDDoc).		
		vPolKDoc = person.issue.
			RUN Insert_TTName ("pdkem",vPolKDoc).		
		RUN RetAdr.p(person.person-id,"�","����ய",?,OUTPUT vPolAdr).
			RUN Insert_TTName ("padr",vPolAdr).
	END.
	
	FIND FIRST branch WHERE branch.branch-id EQ loan.branch-id NO-LOCK NO-ERROR.
	IF AVAIL branch THEN 
		RUN Insert_TTName ("city",TRIM(ENTRY(2,ENTRY(2,branch.address),'.'))).

	vTmpStr = getxattrvalue ("loan",string("proxy," + loan.cont-code), "loan-allowed").
	IF vTmpStr EQ "*" THEN DO:
		RUN Insert_TTName ("all","1").
		RUN Insert_TTName ("mn","1").
	END.
	ELSE DO:
		RUN Insert_TTName ("all","0").
		DO vTmpInt = 1 TO NUM-ENTRIES (vTmpStr,","):
			FIND FIRST ploan WHERE 
				ploan.contract = "dps" AND
				ploan.cont-code = ENTRY(vTmpInt,vTmpStr,",")
			NO-LOCK NO-ERROR.
			IF AVAIL(ploan) THEN DO:
				vBnkVklTmp = " � " + ploan.doc-ref + " �� " + STRING(DAY(ploan.open-date)) + " " + ENTRY(MONTH(ploan.open-date),{&Months}) + " " + STRING(YEAR(ploan.open-date)) + " ����".
				{additem.i vBnkVkl vBnkVklTmp}
			END.
			/*vBnkVkl = vBnkVkl + " � " + ploan.doc-ref + " �� " + STRING(DAY(ploan.open-date)) + " " + ENTRY(MONTH(ploan.open-date),{&Months}) + " " + 
					  STRING(YEAR(ploan.open-date)) + " ����,".*/
		END.
		/*vBnkVkl = TRIM (vBnkVkl, ",").*/
		IF NUM-ENTRIES (vTmpStr,",") GE 2 THEN DO:
			vBnkVkl = "������ࠬ ������᪨� �������" + vBnkVkl.
			RUN Insert_TTName ("mn","1").
		END.
		ELSE DO:
			vBnkVkl = "�������� ������᪮�� ������" + vBnkVkl.
			RUN Insert_TTName ("mn","0").
		END.
		RUN Insert_TTName ("bvkl",vBnkVkl).
	END.
	
	FIND FIRST _User WHERE _User._Userid EQ loan.user-id NO-LOCK NO-ERROR.
	IF NUM-ENTRIES(STRING(_user._User-Name),".") > 1 THEN vTmpStr = STRING(_user._User-Name).
	ELSE vTmpStr = ENTRY(1,STRING(_user._User-Name)," ") + " " +
				   SUBSTRING(ENTRY(2,STRING(_user._User-Name)," "),1,1,"CHARACTER") + "." +
				   SUBSTRING(ENTRY(3,STRING(_user._User-Name)," "),1,1,"CHARACTER") + ". ".
		RUN Insert_TTName ("sotfi.o.",vTmpStr).
	vTmpStr = GetXAttrValueEx("_user", loan.user-id, "User-nameTP", "").
		RUN Insert_TTName ("sotfiotp",vTmpStr).
	vTmpStr = GetXAttrValueEx("_user", loan.user-id, "����᭍����", "").
		RUN Insert_TTName ("sotdn",vTmpStr).
	vTmpStr = GetXAttrValueEx("_user", loan.user-id, "����᭄��", "").
	IF NUM-ENTRIES(vTmpStr,"/") GE 3 THEN
		vTmpStr = ENTRY(1,vTmpStr,"/") + " " + ENTRY(INT64(ENTRY(2,vTmpStr,"/")),{&Months}) + " " + ENTRY(3,vTmpStr,"/").
		RUN Insert_TTName ("sotdd",vTmpStr).
	vTmpStr = GetXAttrValueEx("_user", loan.user-id, "���������", "").
	vTmpStr = CAPS(SUBSTRING(vTmpStr,1,1)) + SUBSTRING(vTmpStr,2).
		RUN Insert_TTName ("sotdol",vTmpStr).
	vTmpStr = GetXAttrValueEx("_user", loan.user-id, "��������쒏", "").
	vTmpStr = LC(SUBSTRING(vTmpStr,1,1)) + SUBSTRING(vTmpStr,2).
		RUN Insert_TTName ("sotdoltp",vTmpStr).
	vTmpStr = GetXAttrValueEx("_user", loan.user-id, "�⤥���", "").
		RUN Insert_TTName ("sototdelrp",vTmpStr).
	
	vTmpStr = GetXAttrValueEx("_User", loan.user-id, "filial-id", "").
	FIND FIRST branch WHERE branch.Branch-Id EQ vTmpStr NO-LOCK NO-ERROR.
	IF AVAILABLE branch THEN
	DO:
		vTmpStr = GetXattrValueEx("branch", STRING(branch.Branch-Id), "����������", "") + ' ��� "���� ����"'.
		RUN Insert_TTName ("filialname",vTmpStr).
	END.

	RUN printvd.p (vTmpName,INPUT TABLE ttnames).

END.