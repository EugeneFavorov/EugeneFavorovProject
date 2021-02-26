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

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������"

def new shared stream puk.
def var fname as char  init "./current_rates.csv"  no-undo.

def var sell 	as char init "�த���" no-undo.
def var buy 	as char init "���㯪�" no-undo.
DEF VAR mNumb 	AS CHARACTER NO-UNDO.  /* ����� �ᯮ�殮��� */
DEF VAR mDate 	AS DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.  /* ��� */
DEF VAR mTimes 	AS CHARACTER NO-UNDO.  /* �६� */
DEF VAR QP 		AS DATETIME NO-UNDO.
DEF VAR SF		AS DATETIME NO-UNDO.
DEF VAR CM		AS DATETIME NO-UNDO.
DEF VAR TMP_TIME	AS DATETIME NO-UNDO.
DEF VAR tmp_tt	AS DATETIME NO-UNDO.
DEF VAR TMP_BRANCH		AS CHAR NO-UNDO.
def var BranchAddress as char no-undo.
def var BranchName	 as char no-undo.
def var i as decimal init 1 no-undo.
DEF VAR ii AS decimal init 0 NO-UNDO.
def var fl as decimal init 0 no-undo.
/*
tmp_tt = DATETIME("01-01-2030" + " " + "23:59:59").
*/
/*
QP = ADD-INTERVAL(NOW, 3, "hours").

mTimes = REPLACE(SUBSTRING(STRING(QP,"99/99/9999 HH:MM:SS.SSS"), 12, 9), ":","").
*/

DEFINE TEMP-TABLE rates
    FIELD branch 	AS CHAR		/* ���ࠧ������� */
	FIELD brname	AS CHAR		/* ������������ ���ࠧ������� */
    FIELD address	AS CHAR		/* ���� ���ࠧ�������*/
	FIELD datet		AS DATETIME /* �६� ��⠭���� */
	FIELD buy840	AS DECIMAL	/* ���㯪� 840 */
	FIELD sell840	AS DECIMAL	/* �த��� 840 */
    FIELD buy978 	AS DECIMAL	/* ���㯪� 978 */
    FIELD sell978	AS DECIMAL	/* �த��� 978*/
    FIELD buy398 	AS DECIMAL	/* ���㯪� 398 */
    FIELD sell398	AS DECIMAL	/* �த��� 398 */
    .

DEF BUFFER ttrates FOR rates.
DEF BUFFER ffrates FOR rates.

{empty rates}

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
	
	SF = DATETIME(STRING(mDATE, "99-99-9999") + " " + "23:59:59"). 
	CM = DATETIME(STRING(mDATE, "99-99-9999") + " " + "00:00:00"). 
	
	/* ᮧ����� ������ ������*/
	CREATE ttrates.
					
	/* �⡥६ ��⠭������� ����� �� ���� ���� */
	FOR EACH irate-time
		WHERE irate-time.iratedatetime <= SF
		AND	irate-time.iratedatetime >= CM
		AND (irate-time.rate-type = sell OR irate-time.rate-type = buy)
		NO-LOCK BY irate-time.iratedatetime /*DESCENDING*/ BY irate-time.branch-id :
			
			ii = ii + 1.
			/*---------------------------------------------------------------*/
			/* �᫨ �ந��諮 ��������� ���ࠧ������� ��� �६��� */
			IF TMP_BRANCH <> irate-time.branch-id OR TMP_TIME <> irate-time.iratedatetime THEN
				DO:
					/* � ���� �� ��࠭���, �.�. ���ࠧ������� �� ���⮥*/
					IF TMP_BRANCH <> "" THEN
						DO:
							/* �㦭� ᪮��஢��� ⥪���� ������ � �㦭�� ⠡���� */
							BUFFER-COPY ttrates TO rates.

							VALIDATE rates NO-ERROR.
							
							RELEASE rates.
						END.
						
					/* �᫨ �� �� ���� ����� ��� */
					IF (TMP_TIME <> irate-time.iratedatetime AND TMP_TIME <> ?) OR fl = 1 THEN 
						DO:
							fl = 1.
							
							/* �㦭� ���� ��᫥���� ���祭�� ���ᮢ ������ �� ����� ������������� � ᪮��஢��� �� �६����� ������*/
							/* ⥬ ᠬ� ��࠭�� ���� ���祭�� �� �⮬� ���ࠧ������� */
							FOR EACH ffrates
								WHERE ffrates.branch EQ irate-time.branch-id
							BY ffrates.datet DESCENDING:
								/**/
								BUFFER-COPY ffrates TO ttrates.
								/**/
								LEAVE.
							END.

						END.
				END.
			/*---------------------------------------------------------------*/
					/* ��।���� ������������ � ���� ���ࠧ������� */

					RUN GetBranch(INPUT irate-time.branch-id,INPUT-OUTPUT BranchName,INPUT-OUTPUT BranchAddress).
					/**/
					ASSIGN
					ttrates.branch = irate-time.branch-id
					ttrates.brname = BranchName
					ttrates.address = BranchAddress
					ttrates.datet = irate-time.iratedatetime
					.
	
						/* �த��� */
						IF irate-time.rate-type = sell THEN
							DO:
								/* 840*/
								IF  irate-time.instr-code = '840' THEN
										ASSIGN
										ttrates.sell840 = irate-time.rate-instr.
								/* 978 */
								IF  irate-time.instr-code = '978' THEN
										ASSIGN
										ttrates.sell978 = irate-time.rate-instr.
								/* 398 */
								IF  irate-time.instr-code = '398' THEN
										ASSIGN
										ttrates.sell398 = irate-time.rate-instr.				
							END.
						/* ���㯪� */	
						IF irate-time.rate-type = buy THEN
							DO:
								/* 840*/
								IF  irate-time.instr-code = '840' THEN
										ASSIGN
										ttrates.buy840 = irate-time.rate-instr.
								/* 978 */
								IF  irate-time.instr-code = '978' THEN
										ASSIGN
										ttrates.buy978 = irate-time.rate-instr.
								/* 398 */
								IF  irate-time.instr-code = '398' THEN
										ASSIGN
										ttrates.buy398 = irate-time.rate-instr.					
							END.
					/**/

					/*�������� �६� */
					TMP_TIME = irate-time.iratedatetime.
					/* �������� ���ࠧ������� */
					TMP_BRANCH = irate-time.branch-id.
	END.
END.


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



/* �뢮� */	
/*
run instview.p(TEMP-TABLE rates:HANDLE). 
*/

IF LASTKEY EQ 27 THEN
	RETURN.
	
IF ii > 0 THEN DO:

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
 </Styles>'.
 /**/
 /**/
 FOR EACH rates
 WHERE rates.branch <> ""
 NO-LOCK BY rates.datet BY rates.branch:
	
	 /* �᫨ ����� ��㯯� - � ᮧ����� ���� ���� */
		IF tmp_tt <> rates.datet THEN DO:
			/* �।���⥫쭮 ���஥� �।��騩 ����*/
			IF tmp_tt <> ? THEN
				DO:
				put stream puk unformatted
				'  <Row ss:Index="40" ss:Height="15">
					<Cell ss:MergeAcross="4" ss:StyleID="s34"><Data ss:Type="String">��砫쭨� �⤥�� ����権 �� ����⭮� � �������� �뭪��</Data></Cell>
					<Cell ss:StyleID="s29"/>
					<Cell ss:StyleID="s29"/>

				    <Cell ss:MergeAcross="1" ss:StyleID="s36"><Data ss:Type="String">��� �.�.</Data></Cell>
				   </Row>
				'.
				/**/
				put stream puk unformatted
				'</Table>
					</Worksheet>'.
					
				END.
			
				/* ���� ���� */
				mNumb = SUBSTRING(STRING( rates.datet, "99/99/9999 HH:MM:SS.SSS" ), 1, 10) + "/" + STRING(i).
				i = i + 1.
				
				put stream puk unformatted 
				 '<Worksheet ss:Name="����� ����� ' + REPLACE(SUBSTRING(STRING( rates.datet, "99/99/9999 HH:MM:SS.SSS" ), 12, 8),":", "-") + ' ">
				  <Table ss:ExpandedColumnCount="14" ss:ExpandedRowCount="40" x:FullColumns="1"
				   x:FullRows="1" ss:StyleID="s19">
				   <Column ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="82.5"/>
				   <Column ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="47.25"/>
				   <Column ss:Index="9" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="60"/>
				   <Column ss:Index="11" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="54.75"/>
				   <Column ss:Index="13" ss:StyleID="s19" ss:AutoFitWidth="0" ss:Width="55.5"/>
				   <Row>
					<Cell ss:MergeAcross="13" ss:StyleID="s40"><Data ss:Type="String">�㡫�筮� ��樮��୮� ����⢮ &quot;���� ����&quot; &#10;</Data></Cell>
				   </Row>
				   <Row>
					<Cell ss:MergeAcross="13" ss:StyleID="s40"><Data ss:Type="String">������������                                            &#10;</Data></Cell>
				   </Row>
				   <Row ss:Index="4">
					<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">� � � � � � � � � � � �</Data></Cell>
				   </Row>
				   <Row>
					<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">�� ��⠭������� ���ᮢ ���㯪� � ���ᮢ �த��� ����筮� �����࠭��� ������</Data></Cell>
				   </Row>
				   <Row ss:Index="7">
					<Cell ss:MergeAcross="13" ss:StyleID="s41"><Data ss:Type="String">� ' + STRING(mNumb) + ' �� ' + STRING(Day(mDATE),'99') + " " + entry(MONTH(mDATE),{&Months}) + " " + STRING(YEAR(mDATE)) + " ����"'</Data></Cell>
				   </Row>
				   <Row ss:Index="9">
					<Cell ss:Index="2" ss:MergeAcross="12" ss:MergeDown="1" ss:StyleID="s37"><Data
					  ss:Type="String">��⠭����� � ' + SUBSTRING(STRING(rates.datet, "99/99/9999 HH:MM:SS.SSS"), 12, 2) + ' �ᮢ ' + SUBSTRING(STRING(rates.datet, "99/99/9999 HH:MM:SS.SSS"), 15, 2)  + ' ����� ' + STRING(Day(mDATE),'99') + " " + entry(MONTH(mDATE),{&Months}) + " " + STRING(YEAR(mDATE)) + " ����"'                &#10;� ����� ����� ᫥���騥 ����� ���㯪� � �த��� ����筮� �����࠭��� ������ �� ���ᨩ᪨� �㡫�: &#10;</Data></Cell>
				   </Row>
				   <Row ss:Index="12">
					<Cell ss:MergeAcross="7" ss:MergeDown="1" ss:StyleID="s38"><Data
					  ss:Type="String">������������ ���ࠧ������� ��� &quot;���� ����&quot;</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">������ ���</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">����</Data></Cell>
					<Cell ss:MergeAcross="1" ss:StyleID="s38"><Data ss:Type="String">������⠭᪨� ⥭��</Data></Cell>
				   </Row>
				   <Row>
					<Cell ss:Index="9" ss:StyleID="s24"><Data ss:Type="String">���㯪�</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">�த���</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">���㯪�</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">�த���</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">���㯪�</Data></Cell>
					<Cell ss:StyleID="s24"><Data ss:Type="String">�த���</Data></Cell>
				   </Row>
				   '.
			/* �������� �६� */
			tmp_tt = rates.datet.
		END.
		
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
		PUT STREAM puk UNFORMATTED '</Row>\n'.	
 END.
	
put stream puk unformatted
'  <Row ss:Index="40" ss:Height="60">
    <Cell ss:MergeAcross="1" ss:StyleID="s34"><Data ss:Type="String">��砫쭨� �⤥�� ����権 �� ����⭮� � �������� �뭪��</Data></Cell>
    <Cell ss:StyleID="s29"/>
    <Cell ss:StyleID="s29"/>
    <Cell ss:MergeAcross="1" ss:StyleID="s36"><Data ss:Type="String">��� �.�.</Data></Cell>
   </Row>
'.


put stream puk unformatted
'		</Table>
	</Worksheet>
 </Workbook>
'.	

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").

END.
ELSE
	MESSAGE "������ �� 㪠����� ���� �� �������!" VIEW-AS ALERT-BOX.

/* ����� �뢮�� */		


