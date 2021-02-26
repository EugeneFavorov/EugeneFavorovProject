{globals.i}
{sh-defs.i}
{intrface.get tmess}

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� "���� ����"
     Filename: 
      Comment: ����� � �।���, � ������ ��������� �஢���� � 㪠����� ����
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/

/* ��訢��� ���� */

DEFINE VARIABLE cl_name		AS CHARACTER NO-UNDO.
DEFINE NEW SHARED STREAM puk.
DEFINE VARIABLE fname 		AS CHARACTER  INIT "./bad_credits.csv"  NO-UNDO.
DEFINE VARIABLE ssum		AS DECIMAL INIT 0 NO-UNDO.

DEFINE VARIABLE mDate 		AS DATE FORMAT "99.99.9999" NO-UNDO.  /* ��� */
DEFINE VARIABLE mOst0 		AS LOGICAL  NO-UNDO.  /* ��� */

DEFINE TEMP-TABLE bad_cred
   FIELD cr_numb 	AS CHARACTER		/* ����� �� */
   FIELD cr_name	AS CHARACTER		/* ������������ ������*/
   .

{empty bad_cred}

mDate = date( month( TODAY ), 1, year( TODAY )) - 1.
mOst0 = TRUE.


/* �ଠ �⡮� ������ */

DEFINE FRAME fGet   
   mDate 			LABEL			"���       " 			SKIP
   mOst0			LABEL			"���⮪ > 0"	VIEW-AS TOGGLE-BOX		SKIP	
   WITH WIDTH 25 OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ �⡮� ������ ]" .

MAIN_BLOCK:
DO:
   DO ON ENDKEY UNDO, LEAVE:
      pick-value = ?.
      PAUSE 0.
      UPDATE
         mDate
         mOst0
         WITH FRAME fGet.		
   END.
	
   HIDE FRAME fGet NO-PAUSE.
	
   IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
		{justasec}
		
   /* ���� �� �ᥬ �।�⠬*/
   /* ������� ࠭�� */
   /* � ��� ��� �஢���� �� 㪠����� ���� */
   FOR EACH loan
      WHERE 
      loan.filial-id EQ shFilial
      AND loan.contract = '�।��'
      AND loan.loan-status = '����'
      AND loan.close-date = ?
      AND loan.open-date < mDate
      AND NOT CAN-FIND(
      /* �஢���� �� �� */
      FIRST loan-int
      WHERE loan-int.contract = '�।��'
      AND loan-int.cont-code = loan.cont-code
      AND loan-int.mdate = mDate
      AND CAN-FIND(
      /* �஢���� �� ��।������ ��⠬ */
      FIRST op-entry
      WHERE op-entry.op = loan-int.op
      AND (
      ( SUBSTRING(op-entry.acct-db, 1, 5) = '91604' AND  SUBSTRING(op-entry.acct-cr, 1, 3) = '999')
      OR
      ( SUBSTRING(op-entry.acct-db, 1, 5) = '47427' AND  SUBSTRING(op-entry.acct-cr, 1, 5) = '70601')
      OR
      ((SUBSTRING(op-entry.acct-db, 1, 3) = '455' OR SUBSTRING(op-entry.acct-db, 1, 3) = '452') AND
      (SUBSTRING(op-entry.acct-cr, 1, 5) = '40817' OR SUBSTRING(op-entry.acct-cr, 1, 5) = '40702'))
      )
      )
      )
      /* ����⥪� ��⮢ */				
      AND CAN-FIND(
      FIRST loan-acct
      WHERE loan-acct.contract = '�।��'
      AND loan-acct.cont-code = loan.cont-code
      AND (SUBSTRING(loan-acct.acct, 1, 3) = '455' OR SUBSTRING(loan-acct.acct, 1, 3) = '452')
      ):
      /*---------------------*/
      FIND LAST comm-rate  WHERE comm-rate.kau EQ loan.cont-code 
                             AND comm-rate.commission = '%�।' 
                             AND comm-rate.since <= mDate
                             AND comm-rate.rate-comm = 0 
      NO-LOCK NO-ERROR.
      IF AVAILABLE comm-rate THEN NEXT.
      RELEASE comm-rate.

      IF mOst0 = TRUE THEN
      DO:
         ssum = 0.
         /* ����㬨�㥬 ���⪨ �� ��⠬ */
         FOR EACH loan-acct
            WHERE loan-acct.contract = '�।��'
            AND loan-acct.cont-code = loan.cont-code
            AND ( SUBSTRING(loan-acct.acct, 1, 3) = '455' OR SUBSTRING(loan-acct.acct, 1, 3) = '458' )
            NO-LOCK:
            /* ��室�� ���⪨ �� ��⠬ */
            RUN acct-pos IN h_base (loan-acct.acct,
               loan-acct.currency,
               mDate,
               mDate,
               ?).
            /**/
            IF loan-acct.currency = "" THEN 
               ssum = ssum + abs(sh-bal).
            ELSE
               ssum = ssum + abs(sh-val).
							
         END.
					
         /* �᫨ ���⪨ > 0 - ⮣�� */
         IF ssum > 0 THEN
         DO:
            /* ᮧ����� ������ */
            CREATE bad_cred.
            /**/
            ASSIGN
               bad_cred.cr_numb = loan.cont-code.
            /**/
            RUN GetName(INPUT loan.cust-cat,INPUT cust-id,INPUT-OUTPUT cl_name).
							
            ASSIGN
               bad_cred.cr_name = cl_name.
         END.
      END.
      ELSE
      DO:
         /* ᮧ����� ������ */
         CREATE bad_cred.
         /**/
         ASSIGN
            bad_cred.cr_numb = loan.cont-code.
         /**/
         RUN GetName(INPUT loan.cust-cat,INPUT cust-id,INPUT-OUTPUT cl_name).
					
         ASSIGN
            bad_cred.cr_name = cl_name.		
      END.
   END.
END.
/*
/* �뢮� */	
run instview.p(TEMP-TABLE bad_cred:HANDLE). 
*/
/* ��室�� ������������ ������ */
PROCEDURE GetName:
   DEFINE INPUT PARAMETER cat AS CHARACTER.
   DEFINE INPUT PARAMETER id AS INT64.
   DEFINE INPUT-OUTPUT PARAMETER sname AS CHARACTER.
	
   IF cat = "�" THEN
   DO:
      FIND FIRST PERSON 
         WHERE PERSON.PERSON-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE PERSON THEN
         /* ��� ������ */
         sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
   END.
   ELSE
   DO:
      FIND FIRST CUST-CORP 
         WHERE CUST-CORP.CUST-ID = id
         NO-LOCK NO-ERROR.
      IF AVAILABLE CUST-CORP THEN
         /* ������������ �࣠����樨 */
         sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
   END.
END.


fname = "./bad_credits_" + replace(string(mDate,"99.99.9999"),".","_") + "_" + USERID('bisquit') + ".xml".	

OUTPUT STREAM puk TO VALUE (fname)
   UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
	
PUT STREAM puk UNFORMATTED '<?xml version="1.0"?>
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
  <Style ss:ID="s68">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s73">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s82">
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="����1">
  <Table ss:ExpandedColumnCount="2" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:AutoFitWidth="0" ss:Width="164.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="204.75"/>
   <Row>
    <Cell ss:MergeAcross="1" ss:StyleID="s68"><Data ss:Type="String">���᮪ �।�⮢ �� ���� ' + STRING(mDate,"99.99.9999") + '</Data></Cell>
   </Row>
   <Row>
    <Cell ss:StyleID="s73"><Data ss:Type="String">� ��</Data></Cell>
    <Cell ss:StyleID="s73"><Data ss:Type="String">������������ ������</Data></Cell>
   </Row>
'.

FOR EACH bad_cred
   NO-LOCK BY bad_cred.cr_numb:
	
   PUT STREAM puk UNFORMATTED '<Row>'.
   PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s82"><Data ss:Type="String">' + STRING(bad_cred.cr_numb) + '</Data></Cell>'.
   PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s82"><Data ss:Type="String">' + STRING(bad_cred.cr_name) + '</Data></Cell>'.
   PUT STREAM puk UNFORMATTED '</Row>'.
END.	


PUT STREAM puk UNFORMATTED
   '		</Table>
	</Worksheet>
 </Workbook>
'.

OUTPUT STREAM puk CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").
