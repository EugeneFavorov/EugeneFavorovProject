{globals.i}
{intrface.get tmess}
{sh-defs.i}
{tmprecid.def}
{ttretval.def}

/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment: дни рождений VIP физиков
   Parameters:
         Uses:
      Used by:
      Created: vvv
*/

DEF VAR TMP_SUM 	AS DECIMAL INIT 0 	NO-UNDO.
DEF VAR TMP_PHONE 	AS CHAR 	NO-UNDO.
DEF VAR LIST_BR		AS CHAR	INIT ""		NO-UNDO.
DEF VAR client_name	AS CHAR 			NO-UNDO.
/**/
def new shared stream puk.
def var fname as char  init "./clients_birthday.csv"  no-undo.

/* временная таблица */
DEFINE TEMP-TABLE gg
    FIELD id	 	AS INT64	/* ид физ. лица */
	FIELD fname		AS CHAR		/* ФИО клиента */
	FIELD fphone	AS CHAR		/* Контактный телефон */
    FIELD fbirth	AS DATE		/* дата рождения*/
	FIELD fbirth2	AS DATE		/* ДР в указанном периоде*/
	FIELD dps_sum	AS DECIMAL	/* текущая сумма вкладов клиента */
    .

	
DEF VAR mDate1 AS DATE INIT TODAY NO-UNDO.  /* дата1 */
DEF VAR mDate2 AS DATE INIT TODAY NO-UNDO.  /* дата2 */
DEF VAR mFilial	AS CHAR INIT "Все" NO-UNDO.  /* валюта 1 */
DEF VAR mValue AS DECIMAL INIT 500000.00 NO-UNDO.  /* сумма вкладов */

/* форма отбора данных */

DEFINE FRAME fGet  
   mDate1 			LABEL 			"В период с     " FORMAT "99/99/9999"		SKIP
   mDate2			LABEL			"По             " FORMAT "99/99/9999"		SKIP 
   mFilial			LABEL			"Подразделение  " FORMAT "x(10)"			SKIP
   mValue 			LABEL			"Сумма вкладов >" FORMAT "9999999.99"/*FORMAT ">>>>>>9.99"*/		SKIP

   WITH WIDTH 30 OVERLAY SIDE-LABEL CENTERED ROW 9
        TITLE COLOR BRIGHT-WHITE "[ Дни рождения вкладчиков ]" .
/*----------------------------*/
ON F1 OF mFilial IN FRAME fGet  
DO:
   pick-value = "".
   DO WITH FRAME fGet :
		DO TRANSACTION:
			/**/
			RUN browseld.p ("branch",
						   "",
						   "",
						   "branch-type",
						   4).
			/**/
			IF LAST-EVENT:FUNCTION NE "END-ERROR" AND pick-value NE "" THEN 
				DO:			
					SELF:SCREEN-VALUE = pick-value.
				END.		 
        END.
   END.
   RETURN NO-APPLY.
END.
/*----------------------------*/
MAIN_BLOCK:
DO:
	DO ON ENDKEY UNDO, LEAVE:
	  pick-value = ?.
	  PAUSE 0.
		UPDATE
			mDate1
			mDate2
			mFilial
			mValue
			WITH FRAME fGet.		
	END.
	
	HIDE FRAME fGet NO-PAUSE.
	
	IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.
	
	/* проверка на длительность указанного периода */
	IF YEAR(mDate1) <> YEAR(mDate2) AND mDate1 > mDate2 AND mDate2 - mDate1 > 365 THEN
		DO:
			MESSAGE "Период не должен быть больше 365 дней!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			UNDO, RETRY.
		END.
	
	/* последовательность дат */
	IF mDate1 > mDate2 THEN
		DO:
			MESSAGE "Неверная последовательность дат!" VIEW-AS ALERT-BOX TITLE "Ошибка".
			UNDO, RETRY.	
		END.
	{spinner.i "Отбор данных..."}
	/* отберем физ.лица, у которых ДР в указанный период */
	FOR EACH PERSON
		WHERE
			( 
			STRING(DAY(person.birthday),'99') + "/" + STRING(MONTH(person.birthday), '99') <> '29/02'
			AND 
				(
					( DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate1)) >= mDate1 AND DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate1)) <= mDate2 )
					OR
					( DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate2)) >= mDate1 AND DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate2)) <= mDate2 )
				)
			)
			OR
			(
			STRING(DAY(person.birthday),'99') + "/" + STRING(MONTH(person.birthday),'99') = '29/02'
			AND mDate1 <= DATE(2,29,YEAR(mDate1))
			AND mDate2 >= DATE(2,29,YEAR(mDate2))
			):
			
			TMP_SUM = 0.
			
			/* идем по договорам клиента */
			FOR EACH LOAN 
				WHERE LOAN.CUST-CAT = 'Ч'
				AND LOAN.CUST-ID = PERSON.PERSON-ID
				AND LOAN.CONTRACT = 'dps'
				AND LOAN.CLOSE-DATE = ?
				AND (mFilial = 'Все' OR LOAN.BRANCH-ID = mFilial):
				
					/* отбираем счета договора */
					FOR EACH LOAN-ACCT
						WHERE LOAN-ACCT.CONTRACT = 'dps'
						AND LOAN-ACCT.CONT-CODE = LOAN.CONT-CODE
						AND (LOAN-ACCT.ACCT-TYPE = 'loan-dps-p' OR LOAN-ACCT.ACCT-TYPE = 'loan-dps-t')
						:
					
						/* смотрим состояние счета */
						RUN acct-pos IN h_base (loan-acct.acct,
												loan-acct.currency,
												mDate1,
												mDate1,
												?).
						/**/						
						TMP_SUM = TMP_SUM + abs(sh-bal).
					END.
			END.
			
			/* отбираем клиентов с суммой вкладов выше указанной */
			IF TMP_SUM > mValue THEN
				DO:
				/**/
				RUN GetName(INPUT 'Ч',INPUT person.person-id,INPUT-OUTPUT client_name).
				/**/
				RUN GetPhone(INPUT 'Ч',INPUT person.person-id,OUTPUT TMP_PHONE).
				/**/
				/*
				TMP_PHONE = (IF person.phone[1] = "," THEN "" ELSE person.phone[1]) + (IF person.phone[2] = "," THEN "" ELSE person.phone[2]).
				*/				/*
				TMP_PHONE = (IF person.phone[1] = "," THEN "" ELSE person.phone[1]) + (IF person.phone[2] = "," THEN "" ELSE person.phone[2]).
				*/
				/**/
				CREATE gg.
				/**/
				ASSIGN
					gg.id = person.person-id
					gg.fname = client_name
					gg.fphone = TMP_PHONE
					gg.fbirth = person.birthday
					gg.dps_sum = TMP_SUM.
				
				/* если года в указанном диапазоне разные */
				IF YEAR(mDate1) <> YEAR(mDate2) THEN
					DO:
						IF DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate2)) <= mDate2 THEN
								ASSIGN
								gg.fbirth2 = DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate2)).
						ELSE
								ASSIGN
								gg.fbirth2 = DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate1)).
					END.
				ELSE
					ASSIGN
					gg.fbirth2 = DATE(MONTH(person.birthday), DAY(person.birthday), YEAR(mDate1)).
				
				END.		
	END.
END.
					

/*-----------------------------------------*/
IF LASTKEY EQ 27 THEN
	RETURN.
/*-----------------------------------------*/
IF AVAIL gg THEN DO:
/*
/* вывод */	
run instview.p(TEMP-TABLE gg:HANDLE). 
*/

/* plus.vvv */
/* 29/05/2014 */
/* нужно исключить клиентов, у которых кредит, а в качестве залога депозит */

FOR EACH gg
	EXCLUSIVE-LOCK:
	/**/
	FOR EACH LOAN
		WHERE LOAN.CUST-CAT = 'Ч'
		AND LOAN.CUST-ID = gg.ID
		AND LOAN.CONTRACT = 'Кредит'
		AND LOAN.CLOSE-DATE = ?
		NO-LOCK,
			FIRST SIGNS
				WHERE SIGNS.FILE-NAME = 'loan'
				AND SIGNS.SURROGATE = LOAN.CONTRACT + "," + LOAN.CONT-CODE
				AND SIGNS.CODE = "sum-depos"
				AND SIGNS.CODE-VALUE <> "" :
				/**/
				DELETE gg.
				LEAVE.
				/**/
	END.
END.
/**/

fname = "./BIRTHDAY_" + replace(string(TODAY,"99.99.9999"),".","_") + "_" + userid('bisquit') + ".xml".	

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
  <Style ss:ID="s171">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s173">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s174">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s175">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s176">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s177">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s178">
   <Alignment ss:Horizontal="Right" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s70">
   <Alignment ss:Horizontal="Right" ss:Vertical="Center"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="12" ss:Color="#000000"/>
   <NumberFormat ss:Format="Standard"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Лист1">
  <Table x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
   <Column ss:AutoFitWidth="0" ss:Width="57"/>
   <Column ss:AutoFitWidth="0" ss:Width="70.5"/>
   <Column ss:AutoFitWidth="0" ss:Width="74.25"/>
   <Column ss:AutoFitWidth="0" ss:Width="201.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="101.75"/>
   <Column ss:AutoFitWidth="0" ss:Width="92.5"/>
   <Row ss:AutoFitHeight="0" ss:Height="15.75">
    <Cell ss:MergeAcross="5" ss:MergeDown="1" ss:StyleID="s171"><Data
      ss:Type="String">Дни рождения вкладчиков за период с ' + STRING(mDate1, '99.99.9999') + ' г. по ' + STRING(mDate2, '99.99.9999') + ' г., имеющих сумму вкладов не менее ' + STRING(mValue) + ' рублей</Data></Cell>
   </Row>
   <Row ss:AutoFitHeight="0" ss:Height="24"/>
   <Row ss:AutoFitHeight="0" ss:Height="33">
    <Cell ss:StyleID="s173"><Data ss:Type="String">ИД клиента</Data></Cell>
    <Cell ss:StyleID="s173"><Data ss:Type="String">Дата рождения</Data></Cell>
    <Cell ss:StyleID="s174"><Data ss:Type="String">Дата</Data></Cell>
    <Cell ss:StyleID="s174"><Data ss:Type="String">Ф.И.О клиента</Data></Cell>
	<Cell ss:StyleID="s174"><Data ss:Type="String">Телефон</Data></Cell>
    <Cell ss:StyleID="s175"><Data ss:Type="String">Текущая сумма вкладов</Data></Cell>
   </Row>'.
	
	FOR EACH gg
	NO-LOCK BY gg.fbirth2 BY gg.fname: 	

		PUT STREAM puk UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="30">'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s174"><Data ss:Type="Number">' + STRING(gg.id) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s176"><Data ss:Type="String">' + STRING(gg.fbirth, '99.99.9999') + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s176"><Data ss:Type="String">' + STRING(gg.fbirth2, '99.99.9999') + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s177"><Data ss:Type="String">' + STRING(gg.fname) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s177"><Data ss:Type="String">' + STRING(gg.fphone) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '<Cell ss:StyleID="s70"><Data ss:Type="Number">' + STRING(gg.dps_sum) + '</Data></Cell>'.
		PUT STREAM puk UNFORMATTED '</Row>\n'.   
	END.

	put stream puk unformatted
'		</Table>
	</Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").	

END.
ELSE
	MESSAGE "Не найдено клиентов, удовлетворяющих заданным условиям!" VIEW-AS ALERT-BOX.

/*-----------------------------------------*/
/* находим наименование клиента */
PROCEDURE GetName:
	DEF INPUT PARAMETER cat AS CHARACTER.
    DEF INPUT PARAMETER id AS INT64.
	DEF INPUT-OUTPUT PARAMETER sname AS CHARACTER.
	
	IF cat = "Ч" THEN
		DO:
			FIND FIRST PERSON 
			WHERE PERSON.PERSON-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL PERSON THEN
				/* ФИО клиента */
				sname = PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
		END.
	ELSE
		DO:
			FIND FIRST CUST-CORP 
			WHERE CUST-CORP.CUST-ID = id
			NO-LOCK NO-ERROR.
				IF AVAIL CUST-CORP THEN
				/* наименование организации */
				sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
		END.
END.
/*-----------------------------------------*/
/* находим телефон клиента */
PROCEDURE GetPhone:
	DEF INPUT PARAMETER cat AS CHARACTER.
    DEF INPUT PARAMETER id AS INT64.
	DEF OUTPUT PARAMETER sphone AS CHARACTER.
		
	IF cat = "Ч" THEN
		DO:
			FIND FIRST PERSON 
			WHERE PERSON.PERSON-ID = id
			NO-LOCK NO-ERROR.
				/**/
				IF AVAIL PERSON THEN DO:
					/* телефон клиента */
					sphone = person.phone[1] + person.phone[2].
					/* отредактируем номера телефонов */
					sphone = REPLACE(sphone, ",,", ",").
					/**/
					sphone = TRIM(sphone, ",").
					/* может есть еще Доп. рек. */
					FIND FIRST SIGNS
						WHERE SIGNS.FILE-NAME = 'person'
						AND SIGNS.SURROGATE = STRING(PERSON.PERSON-ID)
						AND SIGNS.CODE = 'Телефон3'
					NO-LOCK NO-ERROR.
					/**/
					IF AVAIL SIGNS THEN
						sphone = (IF sphone = "" THEN "" ELSE ",") + SIGNS.XATTR-VALUE.
					/**/
				END.
		END.

END.


