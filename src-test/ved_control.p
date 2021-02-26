{globals.i}
/*
��� �������� ����஫� ��� ���
*/
def new shared stream puk.
def var fname as char  init "./ved-control.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
def var ddd as char no-undo.

{getdates.i}

IF beg-date = end-date THEN
	ddd = STRING(beg-date, '99.99.9999').
ELSE
	ddd = "��ਮ� � " + STRING(beg-date, '99.99.9999') + " �� " + STRING(end-date, '99.99.9999').

eol = chr(13) + chr(10).

DEFINE BUFFER ss FOR SIGNS.
DEFINE BUFFER kk FOR LOAN-COND.

DEF VAR my_indx1	AS INTEGER NO-UNDO.
DEF VAR my_indx2	AS INTEGER NO-UNDO.
DEF VAR my_indx3	AS INTEGER NO-UNDO.
DEF VAR tmp_str		AS CHARACTER NO-UNDO.
DEF VAR min_date	AS DATE NO-UNDO.

/* ��� */
def temp-table pdg NO-UNDO
	FIELD LOAN_NUMB 	AS CHARACTER		/* � �� */
	FIELD CLIENT_NAME 	AS CHARACTER		/* ������������ ������ */
	FIELD LOAN_START	AS DATE				/* ��� �����祭�� �� */
	FIELD D_FULL_PAY	AS DATE				/* ��� ������� ����筮�� ��襭�� */
	FIELD S_FULL_PAY	AS DECIMAL			/* C㬬� ������� ����筮�� ��襭�� */
	FIELD LOAN_CLOSE	AS DATE				/* ��� ������� �� */
.
/* ��� */	
def temp-table hdg NO-UNDO
	FIELD LOAN_NUMB 	AS CHARACTER		/* � �� */
	FIELD CLIENT_NAME 	AS CHARACTER		/* ������������ ������ */
	FIELD LOAN_START	AS DATE				/* ��� �����祭�� �� */
	FIELD ANN_PAY1		AS DECIMAL			/* �㬬� �����⭮�� ���⥦� �� �뤠� �।�� */
	FIELD D_PART_PAY	AS DATE				/* ��� ���筮�� ����筮�� ��襭��  */
	FIELD S_PART_PAY	AS DECIMAL			/* C㬬� ���筮�� ����筮�� ��襭��  */
	FIELD ANN_PAY2		AS DECIMAL			/* C㬬� �����⭮�� ���⥦� ��᫥ ���筮�� ����筮�� ����襭�� �।�� */
	FIELD DATE_CLOSE1	AS DATE				/* ��� ������� �� �� ���� ���筮�� ����筮�� ����襭�� �।�� */
	FIELD DATE_CLOSE2	AS DATE				/* ��� ������� �� ��᫥ ���� ���筮�� ����筮�� ����襭�� �।�� */
.

{spinner.i "��������, �ନ����� ����..."}

/* ������ ��������� ��������� */
/*
FOR EACH OP-ENTRY 
WHERE OP-ENTRY.OP-DATE >= beg-date
AND   OP-ENTRY.OP-DATE <= end-date
AND   OP-ENTRY.ACCT-DB BEGINS "40817"
AND	  OP-ENTRY.ACCT-CR BEGINS "455"
AND   CAN-FIND (
				FIRST OP
				WHERE OP.OP = OP-ENTRY.OP 
				AND CAPS(OP.DETAILS) MATCHES("*���������*")
			   )
NO-LOCK BY OP-ENTRY.OP-DATE:
*/
FOR EACH LOAN-COND
	WHERE LOAN-COND.SINCE >= beg-date
	AND	  LOAN-COND.SINCE <= end-date NO-LOCK,
		FIRST SIGNS 
			WHERE SIGNS.FILE-NAME = "loan-cond"
			AND SIGNS.SURROGATE = LOAN-COND.CONTRACT + "," + LOAN-COND.CONT-CODE + "," + STRING(LOAN-COND.SINCE, '99/99/99')
			AND SIGNS.CODE = 'PayType'
			AND SIGNS.XATTR-VALUE = '����襭��'
			NO-LOCK:

	/* ᮧ����� ��ப� ��� */
	CREATE pdg.
	
	/* ���� ������� ����襭�� */
	pdg.D_FULL_PAY = LOAN-COND.SINCE.
	
	/* ��� �����祭�� �� */
	FIND FIRST LOAN
		WHERE LOAN.CONTRACT = LOAN-COND.CONTRACT
		AND   LOAN.CONT-CODE = LOAN-COND.CONT-CODE
	NO-LOCK NO-ERROR.
		
		IF AVAIL LOAN THEN
			DO:
				pdg.LOAN_START = LOAN.OPEN-DATE.
				/**/
				pdg.LOAN_CLOSE = LOAN.CLOSE-DATE.
				/* �������� ����� ������� */
				pdg.LOAN_NUMB = LOAN.DOC-REF.
	
					/* ��� 䨧� ��� */
					IF LOAN.CUST-CAT = "�" THEN DO:
						/* ��� ������ */
						FIND FIRST PERSON 
						WHERE PERSON.PERSON-ID = LOAN.CUST-ID
						NO-LOCK NO-ERROR.
							IF AVAIL PERSON THEN
							/* �������� ��� ������ */
							pdg.CLIENT_NAME = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
					END.
					/* ������������ �ਤ��᪮�� ��� */
					IF LOAN.CUST-CAT = "�" THEN DO:
						/* ��� ������ */
						FIND FIRST CUST-CORP 
						WHERE CUST-CORP.CUST-ID = LOAN.CUST-ID
						NO-LOCK NO-ERROR.
							IF AVAIL CUST-CORP THEN
							/* �������� ��� ������ */
							pdg.CLIENT_NAME = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
					END.
			END.
			
	/* ������ �㬬� ���⥦� */
	FIND FIRST ss 
		WHERE ss.FILE-NAME = "loan-cond"
		AND ss.SURROGATE = LOAN-COND.CONTRACT + "," + LOAN-COND.CONT-CODE + "," + STRING(LOAN-COND.SINCE, '99/99/99')
		AND ss.CODE = 'PaySum'
		NO-LOCK NO-ERROR.
			
			IF AVAIL ss THEN
				pdg.S_FULL_PAY = ss.DEC-VALUE.

	/* 
	MESSAGE pdg.LOAN_NUMB + "***" + STRING(pdg.S_FULL_PAY) VIEW-AS ALERT-BOX.
	RETURN.
	*/	
END.

/* ��������� ��������� ��������� */
/*
/* ���� �� �஢����� */
/* ����� 40817 �।�� 455 � �����祭��� ���⥦�, ᮤ�ঠ騬 ᫮�� <������> */
FOR EACH OP-ENTRY 
WHERE OP-ENTRY.OP-DATE >= beg-date
AND   OP-ENTRY.OP-DATE <= end-date
AND   OP-ENTRY.ACCT-DB BEGINS "40817"
AND	  OP-ENTRY.ACCT-CR BEGINS "455"
AND   CAN-FIND (
				FIRST OP
				WHERE OP.OP = OP-ENTRY.OP 
				AND CAPS(OP.DETAILS) MATCHES("*������*")
			   )
NO-LOCK BY OP-ENTRY.OP-DATE:
*/
FOR EACH LOAN-COND
	WHERE LOAN-COND.SINCE >= beg-date
	AND	  LOAN-COND.SINCE <= end-date NO-LOCK,
		FIRST SIGNS 
			WHERE SIGNS.FILE-NAME = "loan-cond"
			AND SIGNS.SURROGATE = LOAN-COND.CONTRACT + "," + LOAN-COND.CONT-CODE + "," + STRING(LOAN-COND.SINCE, '99/99/99')
			AND SIGNS.CODE = 'PayType'
			AND SIGNS.XATTR-VALUE = '���������'
			NO-LOCK:
			
/* */
	/* ᮧ����� ��ப� */
	CREATE hdg.
	
	/* ���� ���筮�� ����襭�� */
	hdg.D_PART_PAY = LOAN-COND.SINCE.
	
	/* ��� �����祭�� �� */
	FIND FIRST LOAN
		WHERE LOAN.CONTRACT = LOAN-COND.CONTRACT
		AND   LOAN.CONT-CODE = LOAN-COND.CONT-CODE
	NO-LOCK NO-ERROR.
	
		IF AVAIL LOAN THEN
			DO:
				hdg.LOAN_START = LOAN.OPEN-DATE.
				/* �������� ����� ������� */
				hdg.LOAN_NUMB = LOAN.DOC-REF.				
				/* */
				IF LOAN.CLOSE-DATE <> ? THEN
					hdg.DATE_CLOSE2 = LOAN.CLOSE-DATE.
				ELSE DO:
					/*hdg.DATE_CLOSE2 = LOAN.END-DATE.*/
					FIND LAST TERM-OBL 
					WHERE TERM-OBL.CONTRACT = LOAN.CONTRACT
					AND	  TERM-OBL.CONT-CODE = LOAN.CONT-CODE
					AND	  TERM-OBL.IDNT = 1
					AND	  TERM-OBL.AMT-RUB <> 0.
						
						IF AVAIL TERM-OBL THEN
							hdg.DATE_CLOSE2 = TERM-OBL.END-DATE.
				
				END.
					/* ��� 䨧� ��� */
					IF LOAN.CUST-CAT = "�" THEN DO:
						/* ��� ������ */
						FIND FIRST PERSON 
						WHERE PERSON.PERSON-ID = LOAN.CUST-ID
						NO-LOCK NO-ERROR.
							IF AVAIL PERSON THEN
							/* �������� ��� ������ */
							hdg.CLIENT_NAME = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
					END.
					/* ������������ �ਤ��᪮�� ��� */
					IF LOAN.CUST-CAT = "�" THEN DO:
						/* ��� ������ */
						FIND FIRST CUST-CORP 
						WHERE CUST-CORP.CUST-ID = LOAN.CUST-ID
						NO-LOCK NO-ERROR.
							IF AVAIL CUST-CORP THEN
							/* �������� ��� ������ */
							hdg.CLIENT_NAME = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
					END.
			END.
				
	/* ������ �㬬� ����筮�� ���⥦� */
	FIND FIRST ss 
		WHERE ss.FILE-NAME = "loan-cond"
		AND ss.SURROGATE = LOAN-COND.CONTRACT + "," + LOAN-COND.CONT-CODE + "," + STRING(LOAN-COND.SINCE, '99/99/99')
		AND ss.CODE = 'PaySum'
		NO-LOCK NO-ERROR.
			
			IF AVAIL ss THEN
				hdg.S_PART_PAY = ss.DEC-VALUE.
	
	/* ������ �㬬� �����⭮�� ���⥦� ��᫥ ���筮�� ����筮�� ����襭�� �।�� */
	FIND FIRST ss
		WHERE ss.FILE-NAME = "loan-cond"
		AND ss.SURROGATE = LOAN-COND.CONTRACT + "," + LOAN-COND.CONT-CODE + "," + STRING(LOAN-COND.SINCE, '99/99/99')
		AND ss.CODE = '����⯫��'
		NO-LOCK NO-ERROR.
			
			IF AVAIL ss THEN
				hdg.ANN_PAY2 = ss.DEC-VALUE.
	
	/* ������ �㬬� �����⭮�� ���⥦� �� ���筮�� ����筮�� ����襭�� �।�� */
	FIND LAST kk
		WHERE kk.CONTRACT = LOAN-COND.CONTRACT
		AND   kk.CONT-CODE = LOAN-COND.CONT-CODE
		AND   kk.SINCE < LOAN-COND.SINCE
	NO-LOCK NO-ERROR.
		
		IF AVAIL kk THEN DO:
			FIND FIRST ss 
				WHERE ss.FILE-NAME = "loan-cond"
				AND ss.SURROGATE = kk.CONTRACT + "," + kk.CONT-CODE + "," + STRING(kk.SINCE, '99/99/99')
				AND ss.CODE = '����⯫��'
			NO-LOCK NO-ERROR.
				
				IF AVAIL ss THEN
					hdg.ANN_PAY1 = ss.DEC-VALUE.		
		END.
		
	/* ���஡㥬 ���� ���� ����砭�� */	
	min_date = ?.
	
	
	/*
	FOR EACH HISTORY
	WHERE HISTORY.FILE-NAME EQ "TERM-OBL"
	AND	HISTORY.FIELD-REF MATCHES (LOAN.CONTRACT + "," + LOAN.CONT-CODE + ",2*")
	AND	HISTORY.MODIFY EQ "D"
	NO-LOCK:

		/* ���� ���-� �ᯠ���� */
		my_indx1 = INDEX(HISTORY.FIELD-VALUE, "fop-date," + STRING(LOAN-COND.SINCE, '99/99/99') + ",").
		my_indx2 = INDEX(HISTORY.FIELD-VALUE, "amt-rub,0,").
		my_indx3 = INDEX(HISTORY.FIELD-VALUE, "end-date,").
		tmp_str = entry(2,substring( HISTORY.FIELD-VALUE, my_indx3 )).
		IF my_indx1 > 0 AND my_indx2 > 0 AND my_indx3 > 0 THEN DO:
			
			/*
			MESSAGE HISTORY.FIELD-REF + " * " + tmp_str VIEW-AS ALERT-BOX.
			RETURN.
			*/
			
			IF DATE(tmp_str) < min_date THEN
				min_date = DATE(tmp_str).
			
			IF min_date = ? THEN
				min_date = DATE(tmp_str).
		END.
	END.
	/*
	MESSAGE min_date VIEW-AS ALERT-BOX.
	*/
	/* �᫨ ��諨 ���� */
	IF min_date <> ? THEN
		hdg.DATE_CLOSE1 = min_date.
	/* �᫨ �� ��諨 */	
	/* ���஡㥬 �ன��� �� �।��饩 ����� */
	ELSE 
		DO:
			/***************************************/
			FOR EACH HISTORY
			WHERE HISTORY.FILE-NAME EQ "TERM-OBL"
			AND	HISTORY.FIELD-REF MATCHES (kk.CONTRACT + "," + kk.CONT-CODE + ",2*")
			AND	HISTORY.MODIFY EQ "D"
			NO-LOCK:

				/* ���� ���-� �ᯠ���� */
				my_indx1 = INDEX(HISTORY.FIELD-VALUE, "fop-date," + STRING(kk.SINCE, '99/99/99') + ",").
				my_indx2 = INDEX(HISTORY.FIELD-VALUE, "amt-rub,0,").
				my_indx3 = INDEX(HISTORY.FIELD-VALUE, "end-date,").
				tmp_str = entry(2,substring( HISTORY.FIELD-VALUE, my_indx3 )).
				IF my_indx1 > 0 AND my_indx2 > 0 AND my_indx3 > 0 THEN DO:
					
					IF DATE(tmp_str) < min_date THEN
						min_date = DATE(tmp_str).
					
					IF min_date = ? THEN
						min_date = DATE(tmp_str).
				END.
			END.			
			/***************************************/
		END.
	/*
	MESSAGE min_date VIEW-AS ALERT-BOX.
	*/
	
	IF min_date = ? THEN
		hdg.DATE_CLOSE1 = DATE('01/01/1999').	
	ELSE
		hdg.DATE_CLOSE1 = min_date.
	/*
	MESSAGE  STRING(min_date, '99/99/9999') + " * " STRING(hdg.DATE_CLOSE1, '99/99/9999') + " * " + LOAN.CONT-CODE VIEW-AS ALERT-BOX.
	RETURN.
	*/
	*/
END.

fname = "./ved_control_" + replace(string(today,"99.99.9999"),".","_") + "_" +  "_" + userid('bisquit') + ".xml".	

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
  <ActiveSheet>1</ActiveSheet>
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
  <Style ss:ID="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s63">
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s65">
   <Alignment ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s66">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
  </Style>
  <Style ss:ID="s67">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s68">
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
  <Style ss:ID="s71">
   <Alignment ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s72">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s73">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s74">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s75">
   <Alignment ss:Horizontal="Right" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Color="#000000"/>
  </Style>
 </Styles>
 '.

	/* ��� ���� */
	put stream puk unformatted
	'<Worksheet ss:Name= "' + "���" + '">
	  <Table ss:ExpandedColumnCount="6" x:FullColumns="1"
	   x:FullRows="1" ss:DefaultRowHeight="15">
	   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="135.75"/>
	   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="171"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="87"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="104.25"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="111"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="102.75"/>
	   <Row ss:AutoFitHeight="0">
		<Cell ss:MergeAcross="5"><Data ss:Type="String">��������� � �஢������� ��� �� ' + ddd + ' </Data></Cell>
	   </Row>
	   <Row ss:AutoFitHeight="0">
		<Cell ss:Index="3" ss:StyleID="s65"/>
		<Cell ss:StyleID="s65"/>
		<Cell ss:StyleID="s65"/>
		<Cell ss:StyleID="s65"/>
	   </Row>
	   <Row ss:AutoFitHeight="0" ss:Height="25.5" ss:StyleID="s66">
		<Cell ss:StyleID="s67"><Data ss:Type="String">� ��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� ������</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� �����祭�� ��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� ������� ����筮�� ��襭�� </Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">�㬬� ������� ����筮�� ��襭��</Data></Cell>
		<Cell ss:StyleID="s68"><Data ss:Type="String">��� ������� ��</Data></Cell>
	   </Row>
	'. 
/* ���� �� ������ ����� */ 
FOR EACH pdg
	NO-LOCK BY pdg.D_FULL_PAY BY pdg.CLIENT_NAME:
		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s73"><Data ss:Type="String">' + pdg.LOAN_NUMB + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s73"><Data ss:Type="String">' + pdg.CLIENT_NAME + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + STRING( pdg.LOAN_START, '99.99.9999' ) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + STRING( pdg.D_FULL_PAY, '99.99.9999' ) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s75"><Data ss:Type="Number">' + TRIM(STRING( pdg.S_FULL_PAY, '>>>>>>>9.99')) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + STRING( pdg.LOAN_CLOSE, '99.99.9999' ) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.
END.
	/* ����뢠�� ���� */
	put stream puk unformatted
		'</Table> 
	</Worksheet>
	'.

	/* ��� ���� */
	put stream puk unformatted
	'<Worksheet ss:Name= "' + "���" + '">
	  <Table ss:ExpandedColumnCount="9" x:FullColumns="1"
	  x:FullRows="1" ss:DefaultRowHeight="15">
	   <Column ss:StyleID="s62" ss:Width="113.25"/>
	   <Column ss:StyleID="s62" ss:Width="201.75"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="109.5"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="94.5"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="79.5"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="97.5"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="130.5"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="131.25"/>
	   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="159.75"/>
	   <Row ss:AutoFitHeight="0">
		<Cell ss:MergeAcross="8"><Data ss:Type="String">��������� � �஢������� ��� �� ' + ddd + ' </Data></Cell>
	   </Row>
	   <Row ss:AutoFitHeight="0">
		<Cell ss:StyleID="s74"/>
		<Cell ss:StyleID="s74"/>
		<Cell ss:StyleID="s71"/>
		<Cell ss:StyleID="s71"/>
		<Cell ss:StyleID="s71"/>
		<Cell ss:StyleID="s71"/>
		<Cell ss:StyleID="s71"/>
		<Cell ss:StyleID="s71"/>
		<Cell ss:StyleID="s71"/>
	   </Row>
	   <Row ss:AutoFitHeight="0" ss:Height="63" ss:StyleID="s72">
		<Cell ss:StyleID="s67"><Data ss:Type="String">� ��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� ������</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� �����祭�� ��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">�㬬� �����⭮�� ���⥦� �� �뤠� �।��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� ���筮�� ����筮�� ��襭��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">�㬬� ���筮�� ����筮�� ��襭��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">�㬬� �����⭮�� ���⥦� ��᫥ ���筮�� ����筮�� ����襭�� �।��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� ������� �� �� ���� ���筮�� ����筮�� ����襭�� �।��</Data></Cell>
		<Cell ss:StyleID="s67"><Data ss:Type="String">��� ������� �� ��᫥ ���� ���筮�� ����筮�� ����襭�� �।��</Data></Cell>
	   </Row>
	'. 
	FOR EACH hdg
		NO-LOCK BY hdg.D_PART_PAY BY hdg.CLIENT_NAME:

		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0">\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s73"><Data ss:Type="String">' + hdg.LOAN_NUMB + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s73"><Data ss:Type="String">' + hdg.CLIENT_NAME + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + STRING( hdg.LOAN_START, '99.99.9999' ) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s75"><Data ss:Type="Number">' + TRIM(STRING( hdg.ANN_PAY1, "->>>>>>>>>9.99")) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + STRING( hdg.D_PART_PAY, '99.99.9999' ) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s75"><Data ss:Type="Number">' + TRIM(STRING( hdg.S_PART_PAY, "->>>>>>>>>9.99")) + '</Data></Cell>\n'. 
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s75"><Data ss:Type="Number">' + TRIM(STRING( hdg.ANN_PAY2, "->>>>>>>>>9.99")) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + '' + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s67"><Data ss:Type="String">' + STRING( hdg.DATE_CLOSE2, '99.99.9999' ) + '</Data></Cell>\n'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.

	END.

put stream puk unformatted
'
	  </Table>
	</Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").
/*---------------------------*/