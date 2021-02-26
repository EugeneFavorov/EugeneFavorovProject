/* ������� �� �������� ��/�� */
/* ����� peo � fev */

{globals.i}
{pp-corr.p}
{sh-defs.i}
{ksh-defs.i NEW}

DEF VAR nameCl  AS char  no-undo.
DEF VAR acct474 AS char  no-undo.
DEF VAR tmpdate AS date  no-undo.
DEF VAR opdate  AS date  no-undo.
DEF VAR time16  AS int64 no-undo.
DEF VAR sBlock  AS dec no-undo.
DEF VAR sSumm   AS dec no-undo.
DEF VAR sOst    AS dec no-undo.
DEF VAR sComis  AS dec no-undo.

DEF BUFFER b-acct FOR acct. 

DEF VAR fname AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER iForm  AS CHAR.

def new shared stream vvs.
  
DEFINE TEMP-TABLE otchet 
        FIELD acct_cl            AS CHAR                /* ���� ������ */
        FIELD acct_47423         AS CHAR                /* ��� �����ᨨ */
        FIELD sBlock             AS dec               /* �㬬� �����஢��*/ 
        FIELD ssumma              AS dec               /* ���⮪ ���*/
        FIELD sComis              AS dec               /* �㬬� �����ᨨ*/
        FIELD sOst                AS dec               /* �㬬� ���⪠*/
        FIELD sCart               AS dec               /* �㬬� ����⥪�*/
 
        INDEX acct_cl acct_cl       
    .

IF iForm NE 'NO' THEN DO:

  {empty otchet}
      
  fname = "./otchet_incass_ul"  + "_" + userid('bisquit') + ".xml".

  opdate = date(getsysconf("OpDate")).
  tmpdate  = date(month(opdate),1,year(opdate)).
  beg-date = tmpdate.
  end-date = date_correct(month(opdate),0,31,year(opdate)).
   
  {getdate.i
  &NoInit    = "YES"
  }
      
    FOR EACH signs WHERE signs.file-name  EQ "acct"
        AND signs.code       EQ "incas"
        AND signs.code-value EQ "��"
        AND SUBSTR(signs.surrogate, 27, 4) EQ shFilial
        NO-LOCK,
        EACH acct WHERE acct.filial-id  EQ shFilial
        AND acct.acct       EQ ENTRY(1,signs.surrogate)                            
        AND acct.close-date EQ ?                            
        /* AND acct.currency   EQ ''*/   
        NO-LOCK:                                             
                             
        FOR EACH b-acct WHERE b-acct.filial-id  EQ shFilial
            AND acct.cust-cat   EQ b-acct.cust-cat
            AND acct.cust-id    EQ b-acct.cust-id                          
            AND b-acct.close-date EQ ?  
            AND b-acct.acct begins "47423"                             
                         
            NO-LOCK:
                                                 
            IF GetXAttrValueEx("acct",b-acct.acct + "," + b-acct.currency, "groupOABS", "") NE "591"
                THEN NEXT.
                              
            acct474 = b-acct.acct. 
                                  
            RUN acct_Block(INPUT acct.acct,INPUT acct.currency ,OUTPUT sBlock).     
            RUN acct_bal(INPUT acct.acct,INPUT acct.currency ,OUTPUT sOst).
            RUN acct_bal(INPUT b-acct.acct,INPUT b-acct.currency ,OUTPUT sComis).                    
                                
             sComis = abs(sComis).                  
            /*MESSAGE string(sBlock) string(sSumm) VIEW-AS ALERT-BOX.             
              RUN acct-pos IN h_base (acct.acct,acct.currency, today,today,CHR(251)).
           
                          
            If sh-bal GE 0 THEN
                MESSAGE "��� �����" + string(- sh-bal)  VIEW-AS ALERT-BOX.*/ 
                
          /*  IF  (sBlock gt 0) and (sSumm GT sComis)  then*/
            do:             
            create otchet.            
            assign                              
                otchet.acct_cl    = acct.acct
                otchet.acct_47423 = b-acct.acct
                otchet.sOst       = sOst
                otchet.sComis     = sComis 
                otchet.sBlock     = sBlock 
                otchet.sSumma     = sSumm                                                 
            .
            end.
            
        END. 
    end.
    
  iF avail otchet then 
    
  do:
  output stream vvs to value (fname)
          UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
  put stream vvs unformatted 
 '<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <LastSaved>2014-04-14T03:05:08Z</LastSaved>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
 
 <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>12585</WindowHeight>
  <WindowWidth>27795</WindowWidth>
  <WindowTopX>675</WindowTopX>
  <WindowTopY>3300</WindowTopY>
  <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Swiss" ss:Size="16"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="res">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s61">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="500" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>   
  <Style ss:ID="s62">
   <Alignment ss:Horizontal="Left" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
  </Style>
  <Style ss:ID="s63">
   <Alignment ss:Horizontal="Center" ss:Vertical="Center" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="11" ss:Color="#000000"/>
    <NumberFormat ss:Format="0.00"/>    
  </Style>    
 </Styles>
 <Worksheet ss:Name="����1">
  <Table ss:ExpandedColumnCount="15" x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="15">
       
   <Column ss:AutoFitWidth="0" ss:Width="150"/>
   <Column ss:AutoFitWidth="0" ss:Width="150"/>   
   <Column ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:AutoFitWidth="0" ss:Width="100"/>
   <Column ss:AutoFitWidth="0" ss:Width="100"/> 
   <Column ss:AutoFitWidth="0" ss:Width="100"/> 
   <Column ss:AutoFitWidth="0" ss:Width="100"/>    
   <Row ss:AutoFitHeight="0" ss:Height="30">    
   <Cell ss:StyleID="Default"><Data ss:Type="String">��� ' + string(end-date) + ' </Data></Cell> 
   </Row>  
   
    <Row ss:AutoFitHeight="0" ss:Height="29.25">
   <Cell ss:StyleID="s61"><Data ss:Type="String">�/��</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">��� �����ᨨ</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">���⮪ �� �/��</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">����.�������</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">�㬬� �����஢��</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">�㬬� ����⥪�</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">�㬬� ᯨᠭ��</Data></Cell>  
   </Row>
  '.
 /*    
   <Cell ss:MergeDown="1"  ss:StyleID="m50945760"><Data ss:Type="String">�㬬� ᯨᠭ��</Data></Cell> 
    <Cell ss:MergeDown="1" ss:StyleID="s62"><Data ss:Type="String">�㬬� ����⥪�</Data></Cell>
      <Cell ss:MergeDown="1" ss:StyleID="m50945760"><Data ss:Type="String">�㬬� ᯨᠭ��</Data></Cell>    
   */

      FOR EACH otchet NO-LOCK:
          /*
          FIELD first_date   AS DATE                 /* ��� �஢���� */
          FIELD acct_cl      AS CHAR                 /* ��� ����騪� */
          FIELD name_cl      AS CHAR                 /* ������������ ����騪� */
          FIELD summ         AS DECIMAL              /* �㬬� �����樨 */
          FIELD summ_kom     AS DECIMAL              /* �㬬� �����ᨨ �� ������� */
          FIELD tarif        AS DECIMAL              /* ��� */
          */
          
         
          
        PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0" ss:Height="20.25">\n'.
        PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + TRIM(ENTRY(1,otchet.acct_cl, "@")) + '</Data></Cell>\n'.
        PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s61"><Data ss:Type="String">' + TRIM(ENTRY(1,otchet.acct_47423, "@")) + '</Data></Cell>\n'.
        PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + string(otchet.sOst) + '</Data></Cell>\n'.
        PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + string(otchet.sComis) + '</Data></Cell>\n'.
        PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + string(otchet.sBlock) + '</Data></Cell>\n'.
        PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + string(otchet.sCart) + '</Data></Cell>\n'.
        PUT STREAM vvs UNFORMATTED '<Cell ss:StyleID="s63"><Data ss:Type="Number">' + STRING(otchet.sSumma) + '</Data></Cell>\n'.     
        PUT STREAM vvs UNFORMATTED '</Row>\n'.         
          
          
          
          /*put stream vvs unformatted '<Cell ss:StyleID="s63"><Data ss:Type="String">'.
          put stream vvs unformatted otchet.acct_cl + '</Data></Cell>\n'.
          put stream vvs unformatted '<Cell><Data ss:Type="String">'.
          put stream vvs unformatted otchet.acct_47423 + '</Data></Cell>\n'.
          put stream vvs unformatted '<Cell ss:StyleID="s63"><Data ss:Type="String">'.
          put stream vvs unformatted string(otchet.summa) + '</Data></Cell>\n'.
          
          put stream vvs unformatted '<Cell ss:StyleID="s63"><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchet.count_klb_posle_16,"->>>>>>>>>9")) + '</Data></Cell>\n'.
          put stream vvs unformatted '<Cell ss:StyleID="s63"><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchet.count_bum_do_16,"->>>>>>>>>9")) + '</Data></Cell>\n'.
          put stream vvs unformatted '<Cell ss:StyleID="s63"><Data ss:Type="String">'.
          put stream vvs unformatted trim(string(otchet.count_bum_posle_16,"->>>>>>>>>9")) + '</Data></Cell>\n'.
          .
          put stream vvs unformatted '</Row>'.*/
      END.
  put stream vvs unformatted 
  '  </Table>
   </Worksheet>
  </Workbook>
  ' .

  output stream vvs close.
  RUN sndbispc ("file=" + fname + ";class=bq").

END.

end.

IF iForm = 'YES' THEN DO: 
  /*���塞 handle ��� ��।�� temp-table � ��*/
  DEF NEW SHARED VAR hO-tt  AS HANDLE NO-UNDO.
  DEF VAR            hO-b   AS HANDLE NO-UNDO.
  DEF VAR            hO-btt AS HANDLE NO-UNDO.

  /*������塞 ⠡���� ��� ��*/
  hO-b = BUFFER otchet:HANDLE.

  CREATE TEMP-TABLE hO-tt.
  hO-tt:CREATE-LIKE("otchet").
  hO-tt:TEMP-TABLE-PREPARE("xotchet").
  
  hO-btt = hO-tt:DEFAULT-BUFFER-HANDLE.

  FOR EACH otchet:
    hO-btt:BUFFER-CREATE.
    hO-btt:BUFFER-COPY(hO-b).
  END.
    RETURN STRING(hO-tt).
END.
/*㤠�塞 �।*/
IF iForm = 'NO' THEN DELETE OBJECT hO-tt.

/*

PROCEDURE GetName:
 DEF INPUT PARAMETER cat AS CHARACTER.
 DEF INPUT PARAMETER id AS INT64.
 DEF OUTPUT PARAMETER sname AS CHARACTER.
 
 IF cat = "�" THEN
  DO:
   FIND FIRST PERSON 
   WHERE PERSON.PERSON-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL PERSON THEN
    /* ��� ������ */
    sname = "�� " + PERSON.NAME-LAST + " " + PERSON.FIRST-NAMES.
  END.
 ELSE
  DO:
   FIND FIRST CUST-CORP 
   WHERE CUST-CORP.CUST-ID = id
   NO-LOCK NO-ERROR.
    IF AVAIL CUST-CORP THEN
    /* ������������ �࣠����樨 */
    sname = CUST-CORP.CUST-STAT + " " + CUST-CORP.NAME-CORP.
  END.
END. */

PROCEDURE acct_bal:
 DEF INPUT PARAMETER acct_  AS CHARACTER.
 DEF INPUT PARAMETER acct_c AS CHARACTER.
 DEF OUTPUT PARAMETER summ AS DECIMAL.
 
    RUN acct-pos IN h_base (acct_,acct_c, end-date,end-date,CHR(251)).
                          
    summ = - sh-bal.   

END. 

PROCEDURE acct_Block:
    DEF INPUT PARAMETER acct_  AS CHARACTER.
    DEF INPUT PARAMETER acct_c  AS CHARACTER.
    DEF OUTPUT PARAMETER sBlock  AS dec.
     /*�����஢��*/   
        
DEFINE VARIABLE mBlock       AS dec NO-UNDO.   
      /*  AND BlockObject.beg-datetime ge beg-date*/
     /*  AND BlockObject.block-type   EQ "�����㬬"  NO-ERROR*/
        
                 
    FOR EACH BlockObject WHERE BlockObject.file-name    EQ "acct"
        AND Blockobject.surrogate    EQ acct_ + "," + acct_c         
        AND BlockObject.end-datetime EQ ?
        AND blockobject.class-code EQ 'BlockAcct'        
        NO-LOCK .
        
          sBlock = sBlock +  abs(BlockObject.val[3]) .
          
     end.              
END. 

    
