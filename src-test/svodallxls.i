/* ������� ��������� �� �ᥬ ���짮��⥫� ���ࠧ�������*/

DEF BUFFER b-tt-op-day FOR tt-op-day. /*��� ������ ���� �⮣��*/
DEF BUFFER c-tt-op-day FOR tt-op-day. /*��� ������ �⮣�� ���ࠧ�������*/

DEF VAR allKKO AS CHAR NO-UNDO. /*��� ��।������ ����室����� ���� �� ���ࠧ�������*/

/* ��� ᮧ��������� 䠩�� */
fname = "./SVODALL" + "_" + STRING(end-date,'99-99-9999') + "_" + USERID('bisquit') + ".xml".

/* �����뢠�� � 䠩� */   
OUTPUT STREAM vvs TO VALUE (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
/* �⨫� */  
PUT STREAM vvs UNFORMATTED 
'<?xml version="1.0"?>\n
<?mso-application progid="Excel.Sheet"?>\n
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">\n
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">\n
  <Version>14.00</Version>\n
 </DocumentProperties>\n
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">\n
  <AllowPNG/>\n
 </OfficeDocumentSettings>\n
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">\n
  <ProtectStructure>False</ProtectStructure>\n
  <ProtectWindows>False</ProtectWindows>\n
 </ExcelWorkbook>\n
 <Styles>\n
  <Style ss:ID="Default" ss:Name="Normal">\n
   <Alignment ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
   <Interior/>\n
   <NumberFormat/>\n
   <Protection/>\n
  </Style>\n
  <Style ss:ID="left">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="left_b">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="right">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="right_b">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
 </Styles>\n
 '.

/* ᮧ���� ��������� */
FOR EACH tt-op-day WHERE tt-op-day.hozdoc NE '-'
				   AND   CAN-DO(tUserID,tt-op-day.user-id)
				   AND   tt-op-day.razdel NE 'k'
	BREAK BY tt-op-day.hozdoc
		  BY tt-op-day.user-name
		  BY tt-op-day.branch-id:

	tNull = FALSE.

  	IF FIRST-OF(tt-op-day.hozdoc) THEN DO:

  		/* ������塞 ⨯ �訢� */
	    tStr = tt-op-day.hozdoc.
	    
	    CASE tStr:
	      WHEN '�����' THEN
	        tShiv = '���㬥��� ���'.
	      WHEN '�����' THEN
	        tShiv = '�����᪨� ������ࠦ�����'.
	      WHEN '������' THEN
	        tShiv = '�����⢥��� ����樨'.
	      WHEN '�������' THEN
	        tShiv = '�������� �㬬�'.
	    END CASE. 

		PUT STREAM vvs UNFORMATTED
 ' <Worksheet ss:Name="' + tShiv + '">\n
  <Table ss:ExpandedColumnCount="5" ss:ExpandedRowCount="1500000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="18"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="160.5"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="82.5" ss:Span="1"/>\n
   <Column ss:Index="5" ss:AutoFitWidth="0" ss:Width="48.75"/>\n
'.

		/* ��������� �������� */
		PUT STREAM vvs UNFORMATTED
'  <Row>\n
    <Cell ss:MergeAcross="4" ss:StyleID="left_b"><Data ss:Type="String">��騩 ॥��� ����権</Data></Cell>\n
   </Row>\n
   <Row>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">��</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">' + STRING(end-date,'99.99.9999') + ' �.' + '</Data></Cell>\n
   </Row>\n
   <Row>\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">������������ ���ࠧ�������</Data></Cell>\n
    <Cell ss:StyleID="left"><Data ss:Type="String">' + vUserOtd + '</Data></Cell>\n
   </Row>\n
   <Row>\n
   	<Cell ss:StyleID="left"><Data ss:Type="String">' + tShiv + '</Data></Cell>\n
   </Row>\n
   <Row>\n
   </Row>\n
'.

		ASSIGN
			vSum-b-p   = 0.00
			vSum-o-p   = 0.00
			vSum-b-p-v = 0.00
			vSum-o-p-v = 0.00
			vSum-b-e   = 0.00
			vSum-o-e   = 0.00
			vSum-b-e-v = 0.00
			vSum-o-e-v = 0.00.

		/* ������塞 �⮣� �� �ᥬ ���짮��⥫� ���ࠧ�������*/
		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '1'
							 AND   b-tt-op-day.acct-cat EQ 'b'
							 AND   b-tt-op-day.razdel EQ 'b':
			vSum-b-p = vSum-b-p + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '1'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b':
			vSum-o-p = vSum-o-p + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '1'
							 AND   b-tt-op-day.acct-cat EQ 'b'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.currency NE '':
			vSum-b-p-v = vSum-b-p-v + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '1'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.currency NE '':
			vSum-o-p-v = vSum-o-p-v + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc 
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'b'
							 AND   b-tt-op-day.razdel EQ 'b':
			vSum-b-e = vSum-b-e + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b':
			vSum-o-e = vSum-o-e + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'b'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.currency NE '':
			vSum-b-e-v = vSum-b-e-v + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   CAN-DO(tUserID,b-tt-op-day.user-id)
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.currency NE '':
			vSum-o-e-v = vSum-o-e-v + b-tt-op-day.amt-rub.
		END.

		PUT STREAM vvs UNFORMATTED
'	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"/>\n
      <Cell ss:StyleID="left_b"><Data ss:Type="String">�����ᮢ�</Data></Cell>\n
      <Cell ss:StyleID="left_b"><Data ss:Type="String">��������ᮢ�</Data></Cell>\n
	</Row>\n
	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�࠭���� � �㬠���� ����</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-p,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-p,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-p-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-p-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"/>\n
      <Cell ss:StyleID="left_b"><Data ss:Type="String">�����ᮢ�</Data></Cell>\n
      <Cell ss:StyleID="left_b"><Data ss:Type="String">��������ᮢ�</Data></Cell>\n
	</Row>\n
	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�࠭���� � ���஭��� ����</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-e,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-e,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-e-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right_b"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-e-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   <Row>\n
   </Row>\n
'.

	PUT STREAM vvs UNFORMATTED
'	<Row>\n
	</Row>\n
	<Row>\n
      <Cell ss:MergeAcross="3" ss:StyleID="left"><Data ss:Type="String">� ⮬ �᫥ �� ���짮��⥫�:</Data></Cell>\n
   </Row>\n
'.

	END.

	/* ������塞 �⮣� �� ������� ���짮��⥫� �� ��� */
	IF FIRST-OF(tt-op-day.user-name) THEN DO:

		/*����室��� �� ������ �� ���ࠧ�������*/
		allKKO = ''.
		FOR EACH b-tt-op-day 
			WHERE b-tt-op-day.user-name EQ tt-op-day.user-name 
			/*AND	b-tt-op-day.branch-id NE '0400'    */                  /*�����誠 �᪫�砥� �஢���� ᤥ����� � ��᪮�᪮� 䨫���� pda 19/11/2015*/
			BREAK BY b-tt-op-day.branch-id:
	/*
			/*28.01.16 pda ����� �� ���ࠧ������� �� �ॡ����*/
			IF FIRST-OF(b-tt-op-day.branch-id) 
			AND CAN-DO('��⮬���᪨� �࠭���樨',b-tt-op-day.user-name) THEN
				{additem.i allKKO b-tt-op-day.branch-id}
	*/	
		END.
			PUT STREAM vvs UNFORMATTED
'	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">��������� �஢���� ���짮��⥫�</Data></Cell>\n
      <Cell ss:MergeAcross="1" ss:StyleID="right_b"><Data ss:Type="String">' + tt-op-day.user-name + '</Data></Cell>\n
    </Row>\n
'.

		ASSIGN
			vSum-b-p   = 0.00
			vSum-o-p   = 0.00
			vSum-b-p-v = 0.00
			vSum-o-p-v = 0.00
			vSum-b-e   = 0.00
			vSum-o-e   = 0.00
			vSum-b-e-v = 0.00
			vSum-o-e-v = 0.00.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
						 	 AND   b-tt-op-day.save-type EQ '1'
						 	 AND   b-tt-op-day.acct-cat EQ 'b'
						 	 AND   b-tt-op-day.razdel EQ 'b'
						 	 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
		vSum-b-p = vSum-b-p + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   b-tt-op-day.save-type EQ '1'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
			vSum-o-p = vSum-o-p + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
						 	 AND   b-tt-op-day.save-type EQ '1'
						 	 AND   b-tt-op-day.acct-cat EQ 'b'
						 	 AND   b-tt-op-day.razdel EQ 'b'
						 	 AND   b-tt-op-day.currency NE ''
						 	 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
		vSum-b-p-v = vSum-b-p-v + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   b-tt-op-day.save-type EQ '1'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.currency NE ''
							 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
			vSum-o-p-v = vSum-o-p-v + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'b'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
			vSum-b-e = vSum-b-e + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
			vSum-o-e = vSum-o-e + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'b'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.currency NE ''
							 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
			vSum-b-e-v = vSum-b-e-v + b-tt-op-day.amt-rub.
		END.

		FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ tt-op-day.hozdoc
							 AND   b-tt-op-day.save-type EQ '2'
							 AND   b-tt-op-day.acct-cat EQ 'o'
							 AND   b-tt-op-day.razdel EQ 'b'
							 AND   b-tt-op-day.currency NE ''
							 AND   b-tt-op-day.user-name EQ tt-op-day.user-name:
			vSum-o-e-v = vSum-o-e-v + b-tt-op-day.amt-rub.
		END.

		PUT STREAM vvs UNFORMATTED
'	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"/>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">�����ᮢ�</Data></Cell>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">��������ᮢ�</Data></Cell>\n
	</Row>\n
	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">�࠭���� � �㬠���� ����</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-p,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-p,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-p-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-p-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"/>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">�����ᮢ�</Data></Cell>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">��������ᮢ�</Data></Cell>\n
	</Row>\n
	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">�࠭���� � ���஭��� ����</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-e,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-e,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-e-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-e-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
'.

		FOR EACH c-tt-op-day WHERE 
				 c-tt-op-day.user-name EQ tt-op-day.user-name
			BREAK BY c-tt-op-day.branch-id:

			IF FIRST-OF(c-tt-op-day.branch-id) AND NUM-ENTRIES(allKKO) > 1 THEN DO:
	
				FIND FIRST code WHERE code.class EQ '��⏮�ࠧ�'
        	     			    AND   code.code EQ c-tt-op-day.branch-id
       			 NO-LOCK NO-ERROR.	
	
				PUT STREAM vvs UNFORMATTED
'	<Row>\n
      <Cell ss:MergeAcross="3" ss:StyleID="right_b"><Data ss:Type="String">' + code.name + '</Data></Cell>\n
    </Row>\n
'.

				ASSIGN
					vSum-b-p   = 0.00
					vSum-o-p   = 0.00
					vSum-b-p-v = 0.00
					vSum-o-p-v = 0.00
					vSum-b-e   = 0.00
					vSum-o-e   = 0.00
					vSum-b-e-v = 0.00
					vSum-o-e-v = 0.00.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
								 	 AND   b-tt-op-day.save-type EQ '1'
								 	 AND   b-tt-op-day.acct-cat EQ 'b'
								 	 AND   b-tt-op-day.razdel EQ 'b'
								 	 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
								 	 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
				vSum-b-p = vSum-b-p + b-tt-op-day.amt-rub.
				END.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
									 AND   b-tt-op-day.save-type EQ '1'
									 AND   b-tt-op-day.acct-cat EQ 'o'
									 AND   b-tt-op-day.razdel EQ 'b'
									 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
									 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
					vSum-o-p = vSum-o-p + b-tt-op-day.amt-rub.
				END.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
								 	 AND   b-tt-op-day.save-type EQ '1'
								 	 AND   b-tt-op-day.acct-cat EQ 'b'
								 	 AND   b-tt-op-day.razdel EQ 'b'
								 	 AND   b-tt-op-day.currency NE ''
								 	 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
								 	 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
				vSum-b-p-v = vSum-b-p-v + b-tt-op-day.amt-rub.
				END.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
									 AND   b-tt-op-day.save-type EQ '1'
									 AND   b-tt-op-day.acct-cat EQ 'o'
									 AND   b-tt-op-day.razdel EQ 'b'
									 AND   b-tt-op-day.currency NE ''
									 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
									 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
					vSum-o-p-v = vSum-o-p-v + b-tt-op-day.amt-rub.
				END.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
									 AND   b-tt-op-day.save-type EQ '2'
									 AND   b-tt-op-day.acct-cat EQ 'b'
									 AND   b-tt-op-day.razdel EQ 'b'
									 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
									 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
					vSum-b-e = vSum-b-e + b-tt-op-day.amt-rub.
				END.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
									 AND   b-tt-op-day.save-type EQ '2'
									 AND   b-tt-op-day.acct-cat EQ 'o'
									 AND   b-tt-op-day.razdel EQ 'b'
									 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
									 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
					vSum-o-e = vSum-o-e + b-tt-op-day.amt-rub.
				END.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
									 AND   b-tt-op-day.save-type EQ '2'
									 AND   b-tt-op-day.acct-cat EQ 'b'
									 AND   b-tt-op-day.razdel EQ 'b'
									 AND   b-tt-op-day.currency NE ''
									 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
									 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
					vSum-b-e-v = vSum-b-e-v + b-tt-op-day.amt-rub.
				END.
		
				FOR EACH b-tt-op-day WHERE b-tt-op-day.hozdoc EQ c-tt-op-day.hozdoc
									 AND   b-tt-op-day.save-type EQ '2'
									 AND   b-tt-op-day.acct-cat EQ 'o'
									 AND   b-tt-op-day.razdel EQ 'b'
									 AND   b-tt-op-day.currency NE ''
									 AND   b-tt-op-day.user-name EQ c-tt-op-day.user-name
									 AND   b-tt-op-day.branch-id EQ c-tt-op-day.branch-id:
					vSum-o-e-v = vSum-o-e-v + b-tt-op-day.amt-rub.
				END.
	
				PUT STREAM vvs UNFORMATTED
'	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"/>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">�����ᮢ�</Data></Cell>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">��������ᮢ�</Data></Cell>\n
	</Row>\n
	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">�࠭���� � �㬠���� ����</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-p,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-p,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-p-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-p-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"/>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">�����ᮢ�</Data></Cell>\n
      <Cell ss:StyleID="left"><Data ss:Type="String">��������ᮢ�</Data></Cell>\n
	</Row>\n
	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">�࠭���� � ���஭��� ����</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-e,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-e,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
   	<Row>\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-b-e-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
      <Cell ss:StyleID="right"><Data ss:Type="String">' + (TRIM(STRING(vSum-o-e-v,'>>>,>>>,>>>,>>9.99'))) + '</Data></Cell>\n
   	</Row>\n
'.

			END.

		END.
/*===========================================================================================================*/
	PUT STREAM vvs UNFORMATTED
'	<Row>\n
    </Row>\n
'.

	END.

	IF LAST-OF(tt-op-day.hozdoc) THEN
		PUT STREAM vvs UNFORMATTED
'</Table>\n
  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">\n
   	<FitToPage/>\n
  	<Print>\n
   	 <FitHeight>0</FitHeight>\n
   	</Print>\n
  </WorksheetOptions>\n
 </Worksheet>\n
'.
	
END. /* FOR EACH tt-op-day */

/* �� ������⢨� �஢������� �஢���� � ���짮��⥫� */
IF tNull THEN
    PUT STREAM vvs UNFORMATTED
'   <Worksheet ss:Name="�஢���� ����������">\n
    <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="150000" x:FullColumns="1"
     x:FullRows="1" ss:DefaultRowHeight="15">\n
     <Column ss:AutoFitWidth="0" ss:Width="108.85" ss:Span="1"/>\n
     <Column ss:Index="3" ss:AutoFitWidth="0" ss:Width="83.25" ss:Span="1"/>\n
     <Column ss:Index="5" ss:AutoFitWidth="0" ss:Width="54"/>\n
     <Column ss:AutoFitWidth="0" ss:Width="55.5"/>\n
     <Column ss:AutoFitWidth="0" ss:Width="54.75"/>\n
     <Column ss:AutoFitWidth="0" ss:Width="59.25"/>\n
     <Column ss:AutoFitWidth="0" ss:Width="57.25"/>\n
     <Column ss:AutoFitWidth="0" ss:Width="63.75"/>\n
     <Row ss:Height="17.25">\n
      <Cell ss:MergeAcross="9" ss:StyleID="left_b"><Data ss:Type="String">�஢������� �஢���� �� �����㦥��</Data></Cell>\n
     </Row>\n
     </Table>\n
     </Worksheet>\n
'.

PUT STREAM vvs UNFORMATTED
'</Workbook>\n
'.

OUTPUT STREAM vvs CLOSE.

/*�뢮� �१ bispc*/
RUN sndbispc ("file=" + fname + ";class=bq").