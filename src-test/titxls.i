DEF VAR fname AS CHAR.
DEF VAR r     AS CHAR INIT ' ��.'.

DEF NEW SHARED STREAM vvs.

fname = "./Titul" + "_" + STRING(end-date,'99-99-9999') + "_" + iFilID + "_" + USERID('bisquit') + ".xml".

/*�⨫�, 蠯�� �� ������� �⤥����� ���쭨��*/   
OUTPUT STREAM vvs TO VALUE (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
  PUT STREAM vvs UNFORMATTED '
<?xml version="1.0"?>\n
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
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="def_c">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="def_l">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>\n
  </Style>\n
  <Style ss:ID="def_b">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="header">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="13"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="dat">\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="left">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Center"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="left_b">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Center"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
  </Style>\n
  <Style ss:ID="left_g">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Interior ss:Color="#C0C0C0" ss:Pattern="Solid"/>\n
  </Style>\n
  <Style ss:ID="left_gb">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>\n
   <Interior ss:Color="#C0C0C0" ss:Pattern="Solid"/>\n
  </Style>\n
  <Style ss:ID="summ">\n
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
  </Style>\n
  <Style ss:ID="right_i">\n
   <Alignment ss:Horizontal="Right" ss:Vertical="Bottom"/>\n
   <Borders>\n
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
   </Borders>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Color="#000000"
    ss:Italic="1"/>\n
   <Interior/>\n
  </Style>\n
  <Style ss:ID="left_gno">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Borders/>\n
   <Interior ss:Color="#C0C0C0" ss:Pattern="Solid"/>\n
  </Style>\n
    <Style ss:ID="left_ino">\n
   <Alignment ss:Horizontal="Left" ss:Vertical="Bottom"/>\n
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="9"
    ss:Color="#000000" ss:Italic="1"/>\n
  </Style>\n
 </Styles>\n
 <Worksheet ss:Name="������ ����">\n
  <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="1500000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">\n
   <Column ss:AutoFitWidth="0" ss:Width="79.5"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="141"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="23.25" ss:Span="1"/>\n
   <Column ss:Index="5" ss:AutoFitWidth="0" ss:Width="74.25"/>\n
   <Column ss:Width="37.5"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="53.25"/>\n
   <Column ss:AutoFitWidth="0" ss:Width="39"/>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="def_b"><Data ss:Type="String">�ப �࠭���� 5 ���</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="def_b"><Data ss:Type="String">��娢�� ������' + arhInd + '</Data></Cell>\n
   </Row>\n
'.

FIND FIRST code WHERE code.class EQ '��⏮�ࠧ�'
                AND   code.code EQ iFilID
    NO-LOCK NO-ERROR.

/*�㦭� �� ������ ᠬ� ���ࠧ������� � ��� ���쭮� ����*/
DEF VAR selfprint AS LOG INIT FALSE NO-UNDO.
IF code.misc[2] EQ '��' THEN  
  selfprint = TRUE. 

/*�᫨ � �� ������ ���ࠧ������� �� �뫮 ���㬥�⮢, ᮧ������*/
/*���㤭����� ��㣨� ���ࠧ�������, � �� �㦥� selfprint     */
DEF VAR allKKO AS CHAR NO-UNDO.
FOR EACH tt-kko 
    BREAK BY tt-kko.kko:

    IF FIRST-OF(tt-kko.kko) THEN
    	{additem.i allKKO tt-kko.kko} 

END.

/*�⤥�����, ��� ���ண� ��⠢����� ���쭨�*/
/*+ ��� ��⠢�����                            */
PUT STREAM vvs UNFORMATTED
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="7" ss:StyleID="header"><Data ss:Type="String">' + code.name + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:StyleID="def_l"><Data ss:Type="String">��</Data></Cell>\n
    <Cell ss:StyleID="dat"><Data ss:Type="String">' + STRING(end-date,'99.99.9999') + ' �.' + '</Data></Cell>\n
   </Row>\n
   <Row>\n
   </Row>\n
'.

/*�⮣� �� �⤥�����*/
PUT STREAM vvs UNFORMATTED
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"/>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">�� �����ᮢ� ��⠬</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">�� ��������ᮢ� ��⠬</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">��壠���᪨� ���㬥��� �� �㬬�</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">�࠭���� � �����஭��� ����</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:Index="2"/>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="7" ss:StyleID="left_g"><Data ss:Type="String">�࠭���� � �⤥���� ������:</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(code.val,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">���ᮢ� ���㬥���</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n   
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">���㬥��� ���</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(code.val,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����᪨� ������ࠦ�����</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(code.val,'�������') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�������� �㬬�</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(code.val,'������') THEN
	PUT STREAM vvs UNFORMATTED
'  <Row ss:AutoFitHeight="0">\n
	    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����⢥��� ����樨</Data></Cell>\n
	    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
	    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
	   </Row>\n
	   <Row ss:AutoFitHeight="0">\n
	    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
	    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
	    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
	   </Row>\n
'.

PUT STREAM vvs UNFORMATTED
'<Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="7" ss:StyleID="left_g"><Data ss:Type="String">�࠭���� � �����஭��� ����</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(code.val,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����᪨� ������ࠦ�����</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(code.val,'�������') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�������� �㬬�</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(code.val,'������') THEN
  PUT STREAM vvs UNFORMATTED
'  <Row ss:AutoFitHeight="0">\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����⢥��� ����樨</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
     </Row>\n
     <Row ss:AutoFitHeight="0">\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
     </Row>\n
'.

IF CAN-DO(code.val,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">��壠���᪨� ���㬥���</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.
/*
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="right_i"><Data ss:Type="String">�� ��� �� ��⮬���᪨� �࠭�����*</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-aut-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-aut-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="right_i"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-auv-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-auv-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.*/

FOR EACH tt-kko 
  WHERE (IF selfprint AND allKKO NE iFilID
  		 THEN TRUE
  		 ELSE tt-kko.kko NE iFilID)
    BREAK BY tt-kko.city-id BY tt-kko.kko
    :

    /*��⠥� �⮣� ��� ������� �⤥�����*/
        {tititog.i &kko=tt-kko.kko}

      IF FIRST(tt-kko.city-id) THEN 
      PUT STREAM vvs UNFORMATTED
      '<Row ss:AutoFitHeight="0">\n
        <Cell ss:Index="2"/>\n
       </Row>\n
       <Row ss:AutoFitHeight="0" ss:Height="15.75">\n
        <Cell ss:MergeAcross="7" ss:StyleID="left_ino"><Data ss:Type="String">�࠭���� ���㬥�⮢ �����⢫���� � ����� �����⢫���� ����権</Data></Cell>\n
       </Row>\n
      '.

      PUT STREAM vvs UNFORMATTED
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="7" ss:StyleID="header"><Data ss:Type="String">' + tt-kko.city + '</Data></Cell>\n
   </Row>\n
  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"/>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">�� �����ᮢ� ��⠬</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">�� ��������ᮢ� ��⠬</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">��壠���᪨� ���㬥��� �� �㬬�</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">�࠭���� � �����஭��� ����</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_g"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mSum-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:Index="2"/>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="7" ss:StyleID="left_g"><Data ss:Type="String">�࠭���� � �⤥���� ������:</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">���ᮢ� ���㬥���</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mKas-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">���㬥��� ���</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mDd-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����᪨� ������ࠦ�����</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-p-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'�������') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�������� �㬬�</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-p-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'������') THEN
  PUT STREAM vvs UNFORMATTED
'  <Row ss:AutoFitHeight="0">\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����⢥��� ����樨</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
     </Row>\n
     <Row ss:AutoFitHeight="0">\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-p-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
     </Row>\n
'.

PUT STREAM vvs UNFORMATTED
'<Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="7" ss:StyleID="left_g"><Data ss:Type="String">�࠭���� � �����஭��� ����</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����᪨� ������ࠦ�����</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mAg-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'�������') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�������� �㬬�</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mOt-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'������') THEN
  PUT STREAM vvs UNFORMATTED
'  <Row ss:AutoFitHeight="0">\n
      <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">�����⢥��� ����樨</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
     </Row>\n
     <Row ss:AutoFitHeight="0">\n
      <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
      <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mHoz-e-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
     </Row>\n
'.

IF CAN-DO(tt-kko.shiv,'�����') THEN
  PUT STREAM vvs UNFORMATTED 
'  <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left_b"><Data ss:Type="String">��壠���᪨� ���㬥���</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="left"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-val-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-val-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.
/*
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="right_i"><Data ss:Type="String">�� ��� �� ��⮬���᪨� �࠭�����*</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-aut-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-aut-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1" ss:StyleID="right_i"><Data ss:Type="String">� �.�. �� ������ � ��. ����⮩</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-auv-b-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
    <Cell ss:MergeAcross="2" ss:StyleID="summ"><Data ss:Type="String">' + TRIM(STRING(mBuh-auv-o-all,'>>>,>>>,>>>,>>9.99')) + r + '</Data></Cell>\n
   </Row>\n
'.*/

PUT STREAM vvs UNFORMATTED
       '<Row ss:AutoFitHeight="0">\n
    <Cell ss:Index="2"/>\n
   </Row>\n
    '.

END.

/*������*/
PUT STREAM vvs UNFORMATTED
'<Row ss:AutoFitHeight="0">\n
    <Cell ss:Index="2"/>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1"><Data ss:Type="String">���㬥��� ����஢��� � ������</Data></Cell>\n
    <Cell ss:Index="5" ss:MergeAcross="2" ss:StyleID="dat"/>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:Index="5" ss:MergeAcross="2" ss:StyleID="def_c"><Data ss:Type="String">( �������)</Data></Cell>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:MergeAcross="1"><Data ss:Type="String">� ����묨 ��壠���᪮�� ��� ᢥ७�</Data></Cell>\n
    <Cell ss:Index="5" ss:MergeAcross="2" ss:StyleID="dat"/>\n
   </Row>\n
   <Row ss:AutoFitHeight="0">\n
    <Cell ss:Index="5" ss:MergeAcross="2" ss:StyleID="def_c"><Data ss:Type="String">( �������)</Data></Cell>\n
   </Row>\n
  </Table>\n
  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">\n
   <FitToPage/>\n
   <Print>\n
    <FitHeight>0</FitHeight>\n
   </Print>\n
  </WorksheetOptions>\n
 </Worksheet>\n
</Workbook>\n
'.
OUTPUT STREAM vvs CLOSE.

/*�뢮� ���쭨�� �१ bispc*/
RUN sndbispc ("file=" + fname + ";class=bq").  