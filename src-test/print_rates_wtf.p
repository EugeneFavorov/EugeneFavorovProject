{globals.i}
{intrface.get tmess}

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: 
      Comment:��������� �� ������������ (�ਪ�,䨧)
   Parameters:
         Uses:
      Used by:
      Created: kam + vvv
*/

{globals.i}
{tmprecid.def}
{param-dog.p}

/*-----------------------------*/
FUNCTION GetUserName RETURNS CHAR
	(
	USR	AS CHAR
	):
	
DEF VAR	user_name AS CHAR NO-UNDO.

	FIND FIRST _USER
		WHERE _USER._USERID EQ USR
	NO-LOCK NO-ERROR.
	
	IF AVAILABLE _USER THEN
		user_name = STRING(_USER._USER-NAME).
	/**/
	RETURN user_name.
	/**/
END.
/*-----------------------------*/

/* �������� ��ࠬ���  = ��� ��楤��� + ����䨪� � �������� �࠭���樨 */
DEF INPUT PARAM iParam AS CHAR NO-UNDO. /* g-comp_v, spr */

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������"

def new shared stream puk.
def var fname as char  init "./current_rates.csv"  no-undo.

def var sell 	as char init "������" no-undo.
def var buy 	as char init "�������" no-undo.
def var spec	as char init "���樠���" no-undo.
DEF VAR mNumb 	AS CHARACTER NO-UNDO.  /* ����� �ᯮ�殮��� */
DEF VAR mDate 	AS DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.  /* ��� */
DEF VAR TMP_TIME	AS DATETIME NO-UNDO.
DEF VAR tmp_tt	AS DATETIME NO-UNDO.
DEF VAR TMP_BRANCH		AS CHAR NO-UNDO.
def var BranchAddress as char no-undo.
def var BranchName	 as char no-undo.
def var i as decimal init 1 no-undo.
def var UserName		as char	no-undo.
DEF VAR ii AS decimal init 0 NO-UNDO.
DEF VAR ss AS decimal init 0 NO-UNDO.
def var fl as decimal init 0 no-undo.

DEFINE BUFFER WW FOR HISTORY.
DEF VAR max_interval AS INTEGER INIT 60000 NO-UNDO. /* 60 ᥪ㭤 */

DEFINE TEMP-TABLE rates
    FIELD branch 	AS CHAR		/* ���ࠧ������� */
	FIELD brname	AS CHAR		/* ������������ ���ࠧ������� */
    FIELD address	AS CHAR		/* ���� ���ࠧ�������*/
	FIELD set_date	AS DATE		/* ��� */
	FIELD who		AS CHAR		/* ���짮��⥫� */
	/**/
	FIELD buy840	AS DECIMAL	/* ���㯪� 840 */
	FIELD sell840	AS DECIMAL	/* �த��� 840 */
	/**/
    FIELD buy978 	AS DECIMAL	/* ���㯪� 978 */
    FIELD sell978	AS DECIMAL	/* �த��� 978 */
	/**/
    FIELD buy398 	AS DECIMAL	/* ���㯪� 398 */
    FIELD sell398	AS DECIMAL	/* �த��� 398 */
	/**/
    FIELD buy156 	AS DECIMAL	/* ���㯪� 156 */
    FIELD sell156	AS DECIMAL	/* �த��� 156 */
	/**/
    .

{empty rates}

DEFINE TEMP-TABLE spec_rates
	FIELD rate_code	AS CHAR		/* ����� ᯥ���� */
	FIELD rate_val	AS CHAR		/* ���祭�� ᯥ���� */
	FIELD rate_set	AS DATETIME	/* ��� � �६� ��⠭���� ���� */	
	FIELD rate_who 	AS CHAR		/* �� ��⠭���� */	
    FIELD oper_type	AS CHAR		/* ⨯ ����樨 � ᯥ���ᮬ */
	FIELD oper_time	AS DATETIME	/* ��� � �६� ����樨 */
	FIELD clz		AS CHAR		/* ������������ ������ */
	FIELD oper_who  AS CHAR		/* �� �����⢨� ������ */
	FIELD sum_rub	AS DECIMAL	/* �㬬� �஢���� � �㡫�� */
	FIELD sum_val	AS DECIMAL	/* �㬬� �஢���� � ����� */
    .

{empty spec_rates}

/* �ଠ �⡮� ������ */

DEFINE FRAME fGet   
   mDate 			LABEL			"���       " 							SKIP
   WITH WIDTH 25 OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ �⡮� ������ ]" .

MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mDate
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	
	/* ������ �� �� ���짮��⥫� */
	FIND FIRST SIGNS
		WHERE SIGNS.FILE-NAME = "_user"
		AND SIGNS.SURROGATE = CAPS(userid("bisquit"))
		AND SIGNS.CODE = "�⤥�����"
	NO-LOCK NO-ERROR.
	
	/**/
	IF AVAIL SIGNS THEN
	
	DO:

		/* ��।���� ������������ � ���� ���ࠧ������� */
		RUN GetBranch(INPUT SIGNS.CODE-VALUE,INPUT-OUTPUT BranchName,INPUT-OUTPUT BranchAddress).
		
		/* ᮧ����� ������ ������*/
		CREATE rates.	
		
		/* �⡥६ ��⠭������� ����� �� 㪠����� ���� */
		FOR EACH instr-rate
			WHERE instr-rate.instr-cat = "currency"
			AND ( instr-rate.rate-type = sell OR instr-rate.rate-type = buy OR instr-rate.rate-type = spec )			
			AND instr-rate.since EQ mDate
			NO-LOCK:
			/**/
			ii = ii + 1.
			/* �᫨ �� ����� ��⠭������ � ���� �६� */
			/* � �� � ���� ������ */
			/**/
			ASSIGN
				rates.branch = SIGNS.CODE-VALUE
				rates.brname = BranchName
				rates.address = BranchAddress
				rates.set_date = mDate
				.	

				/* �த��� */
				IF instr-rate.rate-type = sell THEN
					DO:
						/* 840 */
						IF  instr-rate.instr-code = '840' THEN
								ASSIGN
								rates.sell840 = instr-rate.rate-instr.
						/* 978 */
						IF  instr-rate.instr-code = '978' THEN
								ASSIGN
								rates.sell978 = instr-rate.rate-instr.
						/* 398 */
						IF  instr-rate.instr-code = '398' THEN
								ASSIGN
								rates.sell398 = instr-rate.rate-instr.	
						/* 156 */
						IF  instr-rate.instr-code = '156' THEN
								ASSIGN
								rates.sell156 = instr-rate.rate-instr.
					END.
					
				/* ���㯪� */	
				IF instr-rate.rate-type = buy THEN
					DO:
						/* 840 */
						IF  instr-rate.instr-code = '840' THEN
								ASSIGN
								rates.buy840 = instr-rate.rate-instr.
						/* 978 */
						IF  instr-rate.instr-code = '978' THEN
								ASSIGN
								rates.buy978 = instr-rate.rate-instr.
						/* 398 */
						IF  instr-rate.instr-code = '398' THEN
								ASSIGN
								rates.buy398 = instr-rate.rate-instr.	
						/* 156 */
						IF  instr-rate.instr-code = '156' THEN
								ASSIGN
								rates.buy156 = instr-rate.rate-instr.	
					END.
				/*
				/* ���樠��� */	
				IF instr-rate.rate-type = spec THEN
					DO:
						/* 840 */
						IF  instr-rate.instr-code = '840' THEN
								ASSIGN
								rates.spec840 = instr-rate.rate-instr.
						/* 978 */
						IF  instr-rate.instr-code = '978' THEN
								ASSIGN
								rates.spec978 = instr-rate.rate-instr.
						/* 398 */
						IF  instr-rate.instr-code = '398' THEN
								ASSIGN
								rates.spec398 = instr-rate.rate-instr.	
						/* 156 */
						IF  instr-rate.instr-code = '156' THEN
								ASSIGN
								rates.spec156 = instr-rate.rate-instr.	
					END.
				*/	
				/* ����� CREATE */	
				FIND FIRST HISTORY 
					WHERE HISTORY.FILE-NAME = 'instr-rate'
					AND HISTORY.FIELD-REF = instr-rate.instr-cat + "," + instr-rate.rate-type + "," + instr-rate.instr-code + "," + STRING(instr-rate.since, "99/99/99")
					AND HISTORY.MODIFY = "C"
				NO-LOCK NO-ERROR.
				
				IF AVAIL HISTORY THEN
					DO:
						/* ������ ��� ���짮��⥫� */
						UserName = GetUserName( HISTORY.USER-ID ).
						/* ���짮��⥫� */
						IF rates.who <> "" AND INDEX(rates.who, UserName) = 0 THEN 
							DO:
								ASSIGN
								rates.who = rates.who + "," + UserName.
							END.
						ELSE
							DO:
								ASSIGN
								rates.who = UserName.
							END.
					END.
		END. /* FOR EACH instr-rate */		
	END. /* IF AVAIL branch */
	
END.

/* ���� ������ �� ᯥ���ᠬ �� 㪠����� ���� */
/* ���� �� �ᥬ �࠭����� */
FOR EACH op-kind 
	WHERE op-kind.op-kind MATCHES("*" + ENTRY(2, iParam))
	AND op-kind.PROC EQ ENTRY(1, iParam)
	NO-LOCK:
		/* �饬 ���㬥��� �� 㪠����� ���� � �������� �࠭����� */
		FOR EACH op
			WHERE op.op-kind = op-kind.op-kind
			AND op.op-date = mDate
			NO-LOCK:			
				/* 㪠���� �㬬� � ������� */
				FOR EACH op-entry
					WHERE op-entry.op = op.op
					AND op-entry.amt-cur <> 0
					NO-LOCK:
						/**/
						ss = ss + 1.
						/**/
						CREATE spec_rates.
						/**/
						ASSIGN
							spec_rates.rate_code = op-entry.currency	/* ��� ������ */
							spec_rates.rate_val = GetXAttrValueEx ("op", STRING(op.op), "sprate", "")	/* ���祭�� ᯥ���� */
							spec_rates.oper_type = ( IF SUBSTRING(op-entry.acct-db, 6, 3) = "810" THEN "���㯪�" ELSE "�த���" ) /* ⨯ ����樨 */
							spec_rates.oper_who = GetUserName( op-entry.user-id ) /* �� ᮢ��訫 ������ */
							spec_rates.sum_rub = op-entry.amt-rub	/* ���祭�� � �㡫�� */
							spec_rates.sum_val = op-entry.amt-cur	/* ���祭�� � ����� */
						.
						/* ������ ������ �� ���� �।�� �� �஢���� */
						FIND FIRST ACCT
						WHERE ACCT.ACCT = op-entry.acct-cr
						NO-LOCK NO-ERROR.
							/**/
							IF AVAIL ACCT THEN
								DO:
									/**/
									IF ACCT.CUST-CAT = "�" THEN
										DO:
											/**/
											FIND FIRST person
											WHERE person.person-id = ACCT.CUST-ID
											NO-LOCK NO-ERROR.
												/**/
												IF AVAIL person THEN
													spec_rates.clz = person.name-last + " " + person.first-names.
												/**/
											/**/
										END.
									IF ACCT.CUST-CAT = "�" THEN
										DO:
											/**/
											FIND FIRST cust-corp
											WHERE cust-corp.cust-id = ACCT.CUST-ID
											NO-LOCK NO-ERROR.
												/**/
												IF AVAIL cust-corp THEN
													spec_rates.clz = cust-corp.cust-stat + " " + cust-corp.name-corp.
												/**/											
											/**/
										END.
									/**/
								END.
							/**/
						/* ������ �६� ᮧ����� ᥣ� ���㬥��*/
						FIND FIRST HISTORY 
						WHERE HISTORY.FILE-NAME = "op"
						AND HISTORY.FIELD-REF = STRING(op.op)
						AND HISTORY.MODIF-DATE = mDate
						AND HISTORY.MODIFY = "C"
						NO-LOCK NO-ERROR.
							/**/
							IF AVAIL HISTORY THEN
								spec_rates.oper_time = DATETIME(HISTORY.MODIF-DATE, HISTORY.MODIF-TIME * 1000).
							/**/
						/* �� ���祭�� � ���� ������ ���஡㥬 �� ���ਨ ���᭨�� �� � ����� �� ����� */
						/* ���� �� 㤠��� */
						FOR EACH HISTORY
							WHERE HISTORY.FILE-NAME = "instr-rate"
							AND HISTORY.MODIF-DATE = mDate
							AND HISTORY.MODIFY = "D"
							NO-LOCK:
								/* � ⮬� �� ������ ᮢ������ ���祭�� � ��� ������ */
								IF ENTRY(2, HISTORY.FIELD-VALUE) = spec_rates.rate_code
								AND ENTRY(8, HISTORY.FIELD-VALUE) = spec_rates.rate_val 
								/* ���� 㤠�����  - ��⮬ ᮧ������ ���㬥�� */
								AND spec_rates.oper_time - DATETIME(HISTORY.MODIF-DATE, HISTORY.MODIF-TIME * 1000) <= max_interval
								THEN
									DO:
										/* ������ ����� � ��� �� ᮧ��� ᯥ樠��� ���� */
										FIND FIRST WW
										WHERE WW.FILE-NAME = "instr-rate"
										AND WW.FIELD-REF = HISTORY.FIELD-REF
										AND WW.MODIF-DATE = mDate
										AND WW.MODIFY = "C"
										NO-LOCK NO-ERROR.
											/**/
											IF AVAIL WW THEN
												DO:
													/**/
													rate_set = DATETIME(HISTORY.MODIF-DATE, HISTORY.MODIF-TIME * 1000). /* ����� ��⠭����� ᯥ���� */
													rate_who = GetUserName( op-entry.user-id ). /* �� ��⠭���� */
													/**/
												END.
											/**/
										/**/
									END.
								/**/
						END.
						/*  ᯠᨡ�! */
				END.
				/**/
		END.
		/**/
END.
/**/

/* ��室�� ������������ � ���� ���ࠧ������� */
PROCEDURE GetBranch:
	DEF INPUT 			PARAMETER 	br 		AS CHARACTER.
	DEF INPUT-OUTPUT 	PARAMETER 	sname 	AS CHARACTER.
	DEF INPUT-OUTPUT 	PARAMETER 	sadres 	AS CHARACTER.
	
	DEF VAR M_INDX1 AS INTEGER INIT 0 NO-UNDO.
	DEF VAR M_SUBSTR AS CHARACTER NO-UNDO.
	DEF VAR M_INDX2 AS INTEGER INIT 0 NO-UNDO.
	
	FIND FIRST branch
		WHERE branch.branch-id = br
	NO-LOCK NO-ERROR.
	
	IF AVAIL branch THEN
		DO:
			sname = branch.name.
			sadres = branch.address.
		END.
	/**/
	M_INDX1 = INDEX(sname, '"').
	M_SUBSTR = SUBSTRING(sname, M_INDX1 + 1).
	M_INDX2 = INDEX(M_SUBSTR, '"').
	
	IF M_INDX1 <> 0 AND M_INDX2 <> 0 THEN
		sname = SUBSTRING(sname, 1, M_INDX1 + M_INDX2).
	/* �� */
	sname = REPLACE(sname, "�������⥫�� ���", "��").
	/* �।�⭮-���ᮢ� ��� */
	sname = REPLACE(sname, "�।�⭮-���ᮢ� ���", "���").
	
END.

/*
/* �뢮� */	
run instview.p(TEMP-TABLE spec_rates:HANDLE). 
/**/
*/

IF LASTKEY EQ 27 THEN
	RETURN.
	
IF ii > 0 OR ss > 0 THEN DO:

fname = "./rates_" + replace(string(mDATE,"99.99.9999"),".","_") + "_" + userid('bisquit') + ".xml".	

output stream puk to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
	
put stream puk unformatted '<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>12585</WindowHeight>
  <WindowWidth>27795</WindowWidth>
  <WindowTopX>480</WindowTopX>
  <WindowTopY>120</WindowTopY>
  <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="m54376960">
   <Alignment ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="m54376980">
   <Alignment ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s19">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s24">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s29">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s30">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
   <NumberFormat ss:Format="0.0000"/>
  </Style>
  <Style ss:ID="s34">
   <Alignment ss:Vertical="Center"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s36">
   <Alignment ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s37">
   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s38">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s40">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s41">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  
  <Style ss:ID="s99">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s106">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s137">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s138">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s139">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
   <NumberFormat ss:Format="General Date"/>
  </Style>
 </Styles>'.
 /**/
put stream puk unformatted 
 '<Worksheet ss:Name="����� ������ ' + REPLACE(STRING( rates.set_date, "99/99/9999" ), "/", "_") + ' ">
  <Table  x:FullColumns="1"
   x:FullRows="1" ss:StyleID="s19">
   <Column ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="82.5"/>
   <Column ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="47.25"/>
   <Column ss:Index="9" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="54"/>
   <Column ss:Index="11" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="54"/>
   <Column ss:Index="13" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="54"/>
   <Row>
	<Cell ss:MergeAcross="13" ss:StyleID="s40"><Data ss:Type="String">����⮥ ��樮��୮� ����⢮ &quot;���� ����&quot; &#10;</Data></Cell>
   </Row>
   <Row>
	<Cell ss:MergeAcross="13" ss:StyleID="s40"><Data ss:Type="String">������������                                            &#10;</Data></Cell>
   </Row>
   <Row ss:Index="4">
	<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">� � � � � � � � � � � �</Data></Cell>
   </Row>
   <Row>
	<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">�� ��⠭������� ���������� ���ᮢ ���㯪� � ���ᮢ �த��� �������筮� �����࠭��� ������</Data></Cell>
   </Row>
   <Row ss:Index="7">
	<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">� ' + STRING(mNumb) + ' �� ' + STRING(Day(mDATE),'99') + " " + entry(MONTH(mDATE),{&Months}) + " " + STRING(YEAR(mDATE)) + " ����"'</Data></Cell>
   </Row>
   <Row ss:Index="9">
	<Cell ss:Index="2" ss:MergeAcross="12" ss:MergeDown="1" ss:StyleID="s37"><Data
	  ss:Type="String">��⠭����� � 09 �ᮢ 00 ����� ' + STRING(Day(mDATE),'99') + " " + entry(MONTH(mDATE),{&Months}) + " " + STRING(YEAR(mDATE)) + " ����"'                &#10;� ����� ����� ᫥���騥 ����� ���㯪� � �த��� �������筮� �����࠭��� ������ �� ���ᨩ᪨� �㡫�: &#10;</Data></Cell>
   </Row>
   <Row ss:Index="12">
	<Cell ss:MergeAcross="7" ss:MergeDown="1" ss:StyleID="s38"><Data
	  ss:Type="String">������������ ���ࠧ������� ��� &quot;���� ����&quot;</Data></Cell>
	<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">������ ���</Data></Cell>
	<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">����</Data></Cell>
	<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">������⠭᪨� ⥭��</Data></Cell>
	<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">��⠩᪨� �</Data></Cell>
   </Row>
   <Row>
	<Cell ss:Index="9" ss:StyleID="s24"><Data ss:Type="String">���㯪�</Data></Cell>
	<Cell ss:StyleID="s24"><Data ss:Type="String">�த���</Data></Cell>
	<Cell ss:StyleID="s24"><Data ss:Type="String">���㯪�</Data></Cell>
	<Cell ss:StyleID="s24"><Data ss:Type="String">�த���</Data></Cell>
	<Cell ss:StyleID="s24"><Data ss:Type="String">���㯪�</Data></Cell>
	<Cell ss:StyleID="s24"><Data ss:Type="String">�த���</Data></Cell>
	<Cell ss:StyleID="s24"><Data ss:Type="String">���㯪�</Data></Cell>
	<Cell ss:StyleID="s24"><Data ss:Type="String">�த���</Data></Cell>
   </Row>
   '.
 /**/
 FOR EACH rates
 NO-LOCK BY rates.set_date:
	
		ii = 1.
		
		PUT STREAM puk UNFORMATTED '<Row ss:Height="15">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="3" ss:StyleID="m54376960"><Data ss:Type="String">' + STRING(rates.brname) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:MergeAcross="3" ss:StyleID="m54376980"><Data ss:Type="String">' + STRING(rates.address) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.buy840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.sell840) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.buy978) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.sell978) + '</Data></Cell>\n'.  
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.buy398) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.sell398) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.buy156) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s30"><Data ss:Type="Number">' + STRING(rates.sell156) + '</Data></Cell>\n'.		
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
 END.
/**/	
put stream puk unformatted
'  <Row ss:Index="18" ss:Height="15">
    <Cell ss:MergeAcross="1" ss:StyleID="s34"><Data ss:Type="String">��砫쭨� �⤥�� �������</Data></Cell>
    <Cell ss:StyleID="s29"/>
    <Cell ss:StyleID="s29"/>
    <Cell ss:MergeAcross="1" ss:StyleID="s36"><Data ss:Type="String">��� �.�.</Data></Cell>
   </Row>
'.
/**/
put stream puk unformatted
'		</Table>
	</Worksheet>
'.
/* ��������, �� ���ॡ���� ᮧ���� ��ன ���� � ����묨 �� ᯥ���ᠬ */
IF ss > 0 THEN
	DO:
	/**/
	put stream puk unformatted '
		 <Worksheet ss:Name="�������">
		  <Table x:FullColumns="1"
		   x:FullRows="1" ss:DefaultRowHeight="15">
		   <Column ss:AutoFitWidth="0" ss:Width="64.5"/>
		   <Column ss:AutoFitWidth="0" ss:Width="61.5"/>
		   <Column ss:Width="120"/>
		   <Column ss:AutoFitWidth="0" ss:Width="85.5"/>
		   <Column ss:AutoFitWidth="0" ss:Width="75"/>
		   <Column ss:Width="180"/>
		   <Column ss:Width="120"/>
		   <Column ss:AutoFitWidth="0" ss:Width="135"/>
		   <Column ss:AutoFitWidth="0" ss:Width="82.5"/>
		   <Column ss:AutoFitWidth="0" ss:Width="96"/>
		   <Row ss:Height="30">
			<Cell ss:StyleID="s106"><Data ss:Type="String">��� ��������</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">���祭��</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">��� � �६� ��⠭����</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">��⠭����</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">��� ����樨</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">������������ ������</Data></Cell>			
			<Cell ss:StyleID="s106"><Data ss:Type="String">��� � �६� ����樨</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">�믮����</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">�㬬� � �㡫��</Data></Cell>
			<Cell ss:StyleID="s106"><Data ss:Type="String">�㬬� � �������</Data></Cell>
		   </Row>
		'.
		/**/
		FOR EACH spec_rates
		NO-LOCK BY spec_rates.rate_set:
				/**/
				ss = 1.
				/*
				FIELD rate_code	AS CHAR		/* ����� ᯥ���� */
				FIELD rate_val	AS CHAR		/* ���祭�� ᯥ���� */
				FIELD rate_set	AS DATETIME	/* ��� � �६� ��⠭���� ���� */	
				FIELD rate_who 	AS CHAR		/* �� ��⠭���� */	
				FIELD oper_type	AS CHAR		/* ⨯ ����樨 � ᯥ���ᮬ */
				FIELD oper_time	AS DATETIME	/* ��� � �६� ����樨 */
				FIELD oper_who  AS CHAR		/* �� �����⢨� ������ */
				FIELD sum_rub	AS DECIMAL	/* �㬬� �஢���� � �㡫�� */
				FIELD sum_val	AS DECIMAL	/* �㬬� �஢���� � ����� */				
				*/
				PUT STREAM puk UNFORMATTED '<Row>\n'.
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s137"><Data ss:Type="String">' + STRING(spec_rates.rate_code) + '</Data></Cell>\n'.
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s99"><Data ss:Type="Number">' + STRING(spec_rates.rate_val) + '</Data></Cell>\n'.
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s139"><Data ss:Type="String">' + STRING(spec_rates.rate_set) + '</Data></Cell>\n'.
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s138"><Data ss:Type="String">' + STRING(spec_rates.rate_who) + '</Data></Cell>\n'.
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s138"><Data ss:Type="String">' + STRING(spec_rates.oper_type) + '</Data></Cell>\n'. 
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s138"><Data ss:Type="String">' + STRING(spec_rates.clz) + '</Data></Cell>\n'. 
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s139"><Data ss:Type="String">' + STRING(spec_rates.oper_time) + '</Data></Cell>\n'.  
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s138"><Data ss:Type="String">' + STRING(spec_rates.oper_who) + '</Data></Cell>\n'. 
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s99"><Data ss:Type="Number">' + STRING(spec_rates.sum_rub) + '</Data></Cell>\n'. 
				PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s99"><Data ss:Type="Number">' + STRING(spec_rates.sum_val) + '</Data></Cell>\n'.	
				PUT STREAM puk UNFORMATTED '</Row>\n'.	
				/**/
		END.		
		/**/
		put stream puk unformatted
		'		</Table>
			</Worksheet>
		'.		
	/**/
	END.
/**/
put stream puk unformatted
' </Workbook>
'.	
/**/
output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").
/**/
END.
ELSE
	MESSAGE "������ �� 㪠����� ���� �� �������!" VIEW-AS ALERT-BOX.

/* ����� �뢮�� */		


