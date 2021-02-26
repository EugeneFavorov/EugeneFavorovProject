
DEF VAR fname as char.
   def new shared stream vvs.

    
    fname = "./Titul"  + "_" + userid('bisquit') + ".xml".
   
   output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
  put stream vvs unformatted '
 <?xml version="1.0"?>
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
   <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="6"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s62">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s64">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s65">
   <Font ss:Size="9" ss:Bold="1"/>
   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>
   <NumberFormat ss:Format="0"/>
  </Style>

  <Style ss:ID="s67">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000" ss:Bold="1"/>
  </Style>

  <Style ss:ID="s68">
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="8" 
    ss:Color="#000000" ss:Bold="1"/>
  </Style>





 </Styles>
 <Worksheet ss:Name="journal">
  <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="1500000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="7">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="30"/>'.

kRS = 9.


FOR EACH tt-kko NO-LOCK
    BREAK BY tt-kko.city-id BY tt-kko.kko
    :
    IF FIRST-OF(tt-kko.kko) THEN 
        ASSIGN
        mKas-b = 0.00
        mKas-o = 0.00
        mBuh-b = 0.00
        mBuh-o = 0.00
        mBuh-val-b = 0.00
        mBuh-val-o = 0.00
        mKas-val-b = 0.00
        mKas-val-o = 0.00
        mBuh-el-b = 0.00
        mBuh-el-o = 0.00
        mKas-el-b = 0.00
        mKas-el-o = 0.00
        .
    IF FIRST(tt-kko.city-id) THEN 
    DO:
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ss:MergeAcross="1" ><Data ss:Type="String">�ப �࠭����</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="2" ></Cell>
		</Row>
		'.
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ss:MergeAcross="1" ><Data ss:Type="String">��娢�� ������</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="2" ></Cell>
		</Row>
		'.
	kRow = kRow + kRS * 2.
    END.
    ELSE
    DO:
        IF FIRST-OF(tt-kko.kko) THEN 
        DO:
        FIND FIRST branch WHERE branch.branch-id EQ tt-kko.kko
             NO-LOCK NO-ERROR.
        IF AVAILABLE(branch) THEN DO:
            bank-name = branch.NAME + " �. " + tt-kko.city.
		END.
	ELSE DO:
	bank-name = tt-kko.city.
	END.
        END.
    END.
    IF FIRST-OF(tt-kko.kko) THEN 
    DO:
    IF kRow >= 500 THEN DO:
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(650 - kRow) + '">
		<Cell ss:StyleID="s63" ss:MergeAcross="4" ></Cell>
		</Row>
		'.
	kRow = 0. 

    end.
/*IF tt-kko.kko = '0400'
THEN DO:*/
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="24">
		<Cell ss:StyleID="s65" ><Data ss:Type="String">' + tt-kko.kko + '</Data></Cell>
		<Cell ss:StyleID="s65" ss:MergeAcross="3" ><Data ss:Type="String">' + Bank-name + '</Data></Cell>
		</Row>
		'.
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68"><Data ss:Type="String"></Data></Cell>
		<Cell ss:StyleID="s68" ss:MergeAcross="2" ><Data ss:Type="String">(������ ��� ᮪�饭��� �ଥ���� ������������ �।�⭮�</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String"></Data></Cell>
		<Cell ss:StyleID="s68" ss:MergeAcross="2" ><Data ss:Type="String">�࣠����樨 �(���) ������������ 䨫����)</Data></Cell>
		</Row>
		'.
	kRow = kRow + 24 + kRS * 2.
    END.


    IF FIRST(tt-kko.city-id) THEN 
    DO:
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">���㬥��� ��</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">' + vDateString + '</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s63" ss:MergeAcross="4" ></Cell>
		</Row>
		'.
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String"></Data></Cell>
		<Cell ss:StyleID="s68"  ss:MergeAcross="1" ><Data ss:Type="String">�� �����ᮢ� ��⠬</Data></Cell>
		<Cell ss:StyleID="s68"  ss:MergeAcross="1" ><Data ss:Type="String">�� ��������ᮢ� ��⠬</Data></Cell>
		</Row>
		'.

	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">�㬬�</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mBalans) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mVBalans) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		</Row>
		'.
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">�� ���:</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="3" ></Cell>
		</Row>
		'.
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68"  ss:MergeAcross="2" ><Data ss:Type="String">�࠭���� �� �㬠���� ���⥫� � ��室���� � �⤥���� ������:</Data></Cell>
		</Row>
		'.
	kRow = kRow + kRS * 6 .
    END.

/*kau buh b 810*/
/*if tt-kko.kko = "0000" then
DO:*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p":
        mBuh-b = mBuh-b + tt-day-itog.itog.
    END.
    /* buh o*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "o"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p":
        mBuh-o = mBuh-o + tt-day-itog.itog.
    END.



	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��壠���᪨� ���㬥���</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mBuh-b) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mBuh-o) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		</Row>
		'.
	kRow = kRow + kRS.
/*END.*/
/*end kau*/
    /* kas b*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k":
            mKas-b = mKas-b + tt-day-itog.itog.
    END.

    /* kas o*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "o"
        AND tt-day-itog.razdel EQ "k":
        mKas-o = mKas-o + tt-day-itog.itog.
    END.
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">���ᮢ� ���㬥���</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mKas-b) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mKas-o) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s63" ss:MergeAcross="4" ></Cell>
		</Row>
		'.
	kRow = kRow + kRS * 2.
    /* buh b*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p":
        mBuh-val-b = mBuh-val-b + tt-day-itog.itog.
    END.
    /* buh o*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "o"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p":
        mBuh-val-o = mBuh-val-o + tt-day-itog.itog.
    END.

	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68"  ss:MergeAcross="1" ><Data ss:Type="String">�� ������ � �����࠭��� ����⮩</Data></Cell>
		</Row>
		'.
	
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��壠���᪨� ���㬥���</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mBuh-val-b) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mBuh-val-o) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">�ப �࠭����</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="3" ></Cell>
		</Row>
		'.
	kRow = kRow + kRS * 3.

    /* kas val b*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k":
        mKas-val-b = mKas-val-b + tt-day-itog.itog.
    END.
    /* kas val o*/
    FOR EACH tt-day-itog NO-LOCK
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "o"
        AND tt-day-itog.razdel EQ "k":
        mKas-val-o = mKas-val-o + tt-day-itog.itog.
    END.
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">���ᮢ� ���㬥���</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mKas-val-b) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mKas-val-o) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">�ப �࠭����</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="3" ></Cell>
		</Row>
		'.
	kRow = kRow + kRS * 2.

    /* kas el b*/
    FOR EACH tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k"
        AND tt-day-itog.save-type EQ "e":
        mKas-el-b = mKas-el-b + tt-day-itog.itog.
    END.
    /* kas el o*/
    FOR EACH tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "o"
        AND tt-day-itog.razdel EQ "k"
        AND tt-day-itog.save-type EQ "e":
        mKas-el-o = mKas-el-o + tt-day-itog.itog.
    END.

	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68"  ss:MergeAcross="1" ><Data ss:Type="String">�࠭���� � ���஭��� ����:</Data></Cell>
		</Row>
		'.

	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">���ᮢ� ���㬥���</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mKas-el-b) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mKas-el-o) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">�ப �࠭����</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="3" ></Cell>
		</Row>
		'.
	kRow = kRow + kRS * 2.

    /* buh el b*/
    FOR EACH tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "e":
        mBuh-el-b = mBuh-el-b + tt-day-itog.itog.
    END.
    /* buh el o*/
    FOR EACH tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "o"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "e":
        mBuh-el-o = mBuh-el-o + tt-day-itog.itog.
    END.
    
	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��壠���᪨� ���㬥���</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mBuh-el-b) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mBuh-el-o) + '</Data></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">��.</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ><Data ss:Type="String">�ப �࠭����</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="3" ></Cell>
		</Row>
		'.
	kRow = kRow + kRS * 2.
    IF LAST(tt-kko.city-id) THEN
    DO:



	PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s63" ss:MergeAcross="4" ></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" > 
		<Cell ss:StyleID="s68"  ss:MergeAcross="1" ><Data ss:Type="String">���㬥��� ����஢��� � ������</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="2" ></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68"  ss:MergeAcross="4" ><Data ss:Type="String">(������� ��壠���᪮�� ࠡ�⭨��, �����⢨�襣� �訢 � �஢��� ������� ����஢����� ���㬥�⮢)</Data></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s63" ss:MergeAcross="4" ></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68"  ss:MergeAcross="1" ><Data ss:Type="String">� ����묨 ��壠���᪮�� ��� ᢥ७�</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="2" ></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s68" ss:MergeAcross="1" ></Cell>
		<Cell ss:StyleID="s68" ><Data ss:Type="String">(�������)</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="1" ></Cell>
		</Row>
		<Row ss:Height="' + string(kRS) + '" >
		<Cell ss:StyleID="s63" ss:MergeAcross="4" ></Cell>
		</Row>

		'.
	kRow = kRow + kRS * 7.
    END.


END.

/*������ PAGE*/
PUT STREAM VVS UNFORMATTED '<Row ss:Height="' + string(665 - kRow) + '">
		<Cell ss:StyleID="s63" ss:MergeAcross="4" ></Cell>
	</Row>
	'.
kRow = 0. 
/* put stream vvs unformatted
   '
  </Table>\n
  <Table ss:ExpandedColumnCount="16" ss:ExpandedRowCount="10000" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="7">
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s62" ss:AutoFitWidth="0" ss:Width="110"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="30"/>'.
*/
{tit-dtl-xls.i}






 put stream vvs unformatted
   '
  </Table>\n
 </Worksheet>\n
</Workbook>\n
'.
output stream vvs close.
RUN sndbispc ("file=" + fname + ";class=bq").  