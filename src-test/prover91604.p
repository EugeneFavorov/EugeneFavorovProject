/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО "ПЛЮС БАНК"
     Filename: 
      Comment:отчет-проверка 91604*
*/

{globals.i}
{tmprecid.def}
{param-dog.p}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get loan}


def var sumKredPr as decimal no-undo.
def var sumKredTv as decimal no-undo.
def var vklloan as logical no-undo.
def var pos as char no-undo.
def var koeff as decimal no-undo.
def var custN as char no-undo.
DEF VAR fName AS CHAR NO-UNDO.
def var termsince as date no-undo.
DEF NEW SHARED STREAM vvs.


DEFINE TEMP-TABLE fin
    FIELD filial                AS CHAR         /* филиал */
    FIELD contcode              AS CHAR         /* номер КД */
    FIELD name                  AS CHAR         /* наименование клиента */
    FIELD koeff                 AS DECIMAL         /* коэф-т резерва */
    FIELD pos                   AS CHAR         /* ПОС */
    FIELD sumKredPr     AS DECIMAL
    FIELD sumKredTv     AS DECIMAL
    .
 
{empty fin}
/**/

end-date = TODAY.
{getdate.i}
/**/

HIDE FRAME ftune NO-PAUSE.

IF LASTKEY EQ KEYCODE("ESC") THEN
        RETURN.

/* идем по всем КД, у которых последний платеж входит в указанный период*/
FOR EACH loan
        WHERE loan.contract = 'Кредит'
        AND loan.open-date <= end-date
        AND loan.close-date = ?
        AND loan.filial-id <> '0400'
        NO-LOCK:
        vklloan = false.
        pos = ''.
        koeff = 0.
        custN = ''.
        termsince = date("01/01/1950").
        for each comm-rate where 
            comm-rate.commission = "%Рез" AND
            comm-rate.kau = loan.Contract + ',' + loan.cont-code AND
            comm-rate.currency = loan.Currency AND
            comm-rate.acct     = "0" AND
            /* comm-rate.rate-comm < 50 AND */
            comm-rate.rate-comm > 0 AND
            comm-rate.since    <= end-date AND 
            comm-rate.period    = 0
            NO-LOCK by comm-rate.since desc:
                if comm-rate.rate-comm < 50 then do:
                    koeff = comm-rate.rate-comm.
                    vklloan = true.
                end.
                termsince = comm-rate.since.
            leave.
        end.
        /* if vklloan = false then */ do:
            for each term-obl where
                term-obl.cont-code = loan.cont-code AND
                term-obl.contract = loan.contract AND
                term-obl.idnt = 128 AND
                term-obl.end-date <= end-date AND
                term-obl.end-date > termsince 
                no-lock by term-obl.end-date desc:
                    pos = term-obl.lnk-cont-code.                      
                leave.
            end.   
            if pos <> '' then do:
                for each comm-rate where 
                    comm-rate.commission = "%Рез" AND
                    comm-rate.kau = 'ПОС,' + pos AND
                    comm-rate.currency = loan.Currency AND
                    comm-rate.acct     = "0" AND
                    /* comm-rate.rate-comm < 20 AND */
                    comm-rate.rate-comm > 0 AND
                    comm-rate.since    <= end-date AND 
                    comm-rate.period    = 0
                    NO-LOCK by comm-rate.since desc:
                    if comm-rate.rate-comm < 20 then do:
                        koeff = comm-rate.rate-comm.
                        pos = term-obl.lnk-cont-code.
                        vklloan = true.
                    end.
                    else do:
                        pos = ''.
                        koeff = 0.
                        vklloan = false.
                    end.
                    leave.
                end.
            end.     
        end.
        if vklloan then do:
            RUN GetName(INPUT loan.cust-cat,INPUT loan.cust-id,INPUT-OUTPUT custN).
            sumKredPr = 0.
            FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                AND loan-acct.cont-code = loan.cont-code
                AND loan-acct.acct-type = 'КредПр%В' NO-LOCK NO-ERROR.
                IF AVAIL loan-acct THEN DO:
                    find first acct where acct.acct = loan-acct.acct no-lock no-error.
                    if avail acct /* and acct.filial-id = shfilial */ then do:
                        RUN acct-pos IN h_base (loan-acct.acct, acct.currency, end-date, end-date, ?).
                        sumKredPr = abs(sh-bal).
                    end.
                END.
            sumKredTv = 0.
            FIND LAST loan-acct WHERE loan-acct.contract = 'Кредит'
                AND loan-acct.cont-code = loan.cont-code
                AND loan-acct.acct-type = 'КредТВ' NO-LOCK NO-ERROR.
                IF AVAIL loan-acct THEN DO:
                    find first acct where acct.acct = loan-acct.acct no-lock no-error.
                    if avail acct /* and acct.filial-id = shfilial */ then do:
                        RUN acct-pos IN h_base (loan-acct.acct, acct.currency, end-date, end-date, ?).
                        sumKredTv = abs(sh-bal).
                    end.
                END.
            if (sumKredPr + sumKredTv) > 0 THEN do:
                CREATE fin.
                   ASSIGN
                     fin.contcode = loan.cont-code
                     fin.filial = string(loan.filial-id)
                     fin.name = custN
                     fin.koeff = koeff
                     fin.pos = pos
                     fin.sumKredPr = sumKredPr
                     fin.sumKredTv = sumKredTv
                     .
            end.
        end.
END.

/* выводим в ексель */
    fname = "./prover91604"  + "_" + USERID('bisquit') + ".xml".
    OUTPUT STREAM vvs TO VALUE (fname)
        UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
    PUT STREAM vvs UNFORMATTED
'
<?xml version="1.0"?>
<?mso-application progid="Excel.Sheet"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <Created>2014-04-24T09:54:07Z</Created>
  <Version>14.00</Version>
 </DocumentProperties>
 <OfficeDocumentSettings xmlns="urn:schemas-microsoft-com:office:office">
  <AllowPNG/>
 </OfficeDocumentSettings>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <WindowHeight>12075</WindowHeight>
  <WindowWidth>15315</WindowWidth>
  <WindowTopX>360</WindowTopX>
  <WindowTopY>90</WindowTopY>
  <ProtectStructure>False</ProtectStructure>
  <ProtectWindows>False</ProtectWindows>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Arial Cyr" x:CharSet="204"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s62" ss:Name="qwe">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"
    ss:Color="#000000"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s63">
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s64">
   <NumberFormat ss:Format="Short Date"/>
  </Style>
  <Style ss:ID="s65">
    <NumberFormat ss:Format="Fixed"/>
  </Style>
  <Style ss:ID="s66" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="10" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="@"/>
  </Style>
  <Style ss:ID="s67" ss:Parent="s62">
   <Alignment ss:Horizontal="Center" ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders>
    <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>
    <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>
   </Borders>
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
    ss:Size="10" ss:Bold="1"/>
   <Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>
   <NumberFormat ss:Format="Short Date"/>
  </Style>
 </Styles>
 <Worksheet ss:Name="lll">
  <Table >
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="85.5"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s63" ss:AutoFitWidth="0" ss:Width="160.00"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>
   <Column ss:StyleID="s65" ss:AutoFitWidth="0" ss:Width="111"/>

   <Row ss:Index="2" ss:AutoFitHeight="0" ss:Height="56.25">
    <Cell ss:StyleID="s66"><Data ss:Type="String">Филиал</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Номер КД</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">Клиент</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">коэф-т резерва</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">ПОС</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">КредПр%В</Data></Cell>
    <Cell ss:StyleID="s66"><Data ss:Type="String">КредТВ</Data></Cell>
   </Row>
'.

for each fin no-lock:
PUT STREAM vvs UNFORMATTED '<Row ss:AutoFitHeight="0">'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED string(fin.filial) + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED fin.contcode + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED fin.name + '</Data></Cell>\n'. 
            PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED STRING(fin.koeff,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED fin.pos + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED STRING(fin.sumKredPr,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED  '<Cell><Data ss:Type="String">'.
            PUT STREAM vvs UNFORMATTED STRING(fin.sumKredTv,"->>>>>>>>>9.99") + '</Data></Cell>\n'.
            PUT STREAM vvs UNFORMATTED '</Row>'.
end.

PUT STREAM vvs UNFORMATTED 
'  </Table>
 </Worksheet>
</Workbook>
' .

OUTPUT STREAM vvs CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").


{intrface.del}

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
