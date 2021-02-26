/* Заявка на доступ Бисквит (Отчет по правам пользователей) XML */
/* pda */

Form "~n@(#) Us1-Inf.P 1.0 ??? ??/??/?? Dima 19/08/98"
     with frame sccs-id stream-io width 250.
&GLOB count1 20

DEF NEW SHARED STREAM vvs.
DEFINE VARIABLE in-userid like _user._userid no-undo.

{globals.i}
{wclass.i}
{wordwrap.def}
{intrface.get rights}
{intrface.get instrum}
/* {instrum.fun}*/ 

in-userid = userid("bisquit").
if IsUserAdm(userid("bisquit")) or (userid("bisquit") = "U0400MAY") or (userid("bisquit") = "0000GAI") then
   message "Введите код пользователя: " update in-userid.

{setdest.i &cols=100 &custom="printer.page-lines - "}

DEFINE VARIABLE fname      AS CHARACTER NO-UNDO.         /* имя создаваемого файла */
DEFINE VARIABLE count      AS INT no-undo.
DEFINE VARIABLE i          AS INT no-undo.
DEFINE VARIABLE attr-name  AS char extent {&count1} no-undo.
DEFINE VARIABLE attr-value AS char extent {&count1} no-undo.
DEFINE VARIABLE ibsousr    AS char no-undo.
DEFINE VARIABLE vFilialLst AS char no-undo. /*список филиалов к которым имеет доступ пользователь*/

DEFINE VARIABLE GlavBuh AS char no-undo.
GlavBuh = FGetSetting("ФИОБух",?,"").

DEFINE VARIABLE attr-list AS char init
"
dpr-watch,
ДокОснДата,
ОперДеньМакс,
ОперДеньМин,
multy-login,
ДокОснНомер,
ПросмотрОст,
Отделение,
ОтделенияДБ,
ОтделенияКР,
ИзмКодОтдел,
Дата,
ИзмЗакОД,
ИзмДокум,
ИзмДопРекв,
WorkInBlockedDay,
Л/С,
ОтделенияТр,
Отделения,
Подчиненные,
СтатусАннул,
СтатусИзм,
СтатусОткат,
СтатусПрисв,
СтатусПросм,
СтатусРед,
СтатусСозд,
ОперСчетДБ,
ОперСчетКР,
"
NO-UNDO.

DEF VAR attr-list-r as char init "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,3,3" NO-UNDO.
DEF VAR row as int no-undo.

FOR EACH _user 
   WHERE _userid MATCHES in-userid 
NO-LOCK:
   ibsousr = GetXattrValueEx("_user", STRING(_userid), "АльтернИмя","________________________").

   FOR EACH branchobj 
      WHERE branchobj.file-name EQ "_user"
        AND branchobj.surrogate EQ in-userid
        AND branchobj.role EQ "Filial"
   NO-LOCK:
      IF CAN-DO("*00",TRIM(branchobj.branch-id)) THEN
      DO:
         {additem.i vFilialLst branchobj.branch-id}
      END.
   END.


   fname = "./" + STRING(in-userid) + ".xml".
  
  /* записываем в файл */
  OUTPUT STREAM vvs TO VALUE (fname)
     UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
  
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
     <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>\n
     <Borders/>\n
     <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="11"
      ss:Color="#000000"/>\n
     <Interior/>\n
     <NumberFormat/>\n
     <Protection/>\n
    </Style>\n
    <Style ss:ID="head">\n
     <Alignment ss:Horizontal="Center" ss:Vertical="Center"/>\n
     <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="12"
      ss:Color="#000000" ss:Bold="1"/>\n
    </Style>\n
    <Style ss:ID="botbor">\n
     <Borders>\n
      <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
     </Borders>\n
    </Style>\n
    <Style ss:ID="center">\n
     <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
    </Style>\n
    <Style ss:ID="center_b"> 
     <Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>\n
     <Borders>\n
      <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
     </Borders>\n
    </Style>\n
    <Style ss:ID="tab">\n
     <Alignment ss:Horizontal="Center" ss:Vertical="Top" ss:WrapText="1"/>\n
     <Borders>\n
      <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
     </Borders>\n
     <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="11" ss:Bold="1"/>\n
    </Style>\n
    <Style ss:ID="tab_b">\n
     <Alignment ss:Horizontal="Left" ss:Vertical="Top" ss:WrapText="1"/>\n
     <Borders>\n
      <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
     </Borders>\n
     <Font ss:FontName="Courier New" x:CharSet="204" x:Family="Swiss" ss:Size="11"
      ss:Color="#000000" ss:Bold="1"/>\n
    </Style>\n
    <Style ss:ID="tab_c">\n
     <Alignment ss:Horizontal="Center" ss:Vertical="Top"/>\n
     <Borders>\n
      <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
     </Borders>\n
    </Style>\n
    <Style ss:ID="tab_r">\n
     <Alignment ss:Horizontal="Right" ss:Vertical="Top"/>\n
     <Borders>\n
      <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
     </Borders>\n
    </Style>\n
    <Style ss:ID="tab_l">\n
     <Alignment ss:Horizontal="Left" ss:Vertical="Top" ss:WrapText="1"/>\n
     <Borders>\n
      <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>\n
      <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>\n
     </Borders>\n
    </Style>\n
   </Styles>\n
  '.
  
   /* Заголовок таблицы */
   PUT STREAM vvs UNFORMATTED
      ' <Worksheet ss:Name="' + in-userid + '">\n
        <Table>\n
         <Column ss:AutoFitWidth="0" ss:Width="35"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="35"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="250"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="50"/>\n
         <Column ss:AutoFitWidth="0" ss:Width="280"/>\n
         <Row>\n
         </Row>\n
         <Row>\n
          <Cell ss:MergeAcross="5" ss:StyleID="head"><Data ss:Type="String">З А Я В К А   Н А   Д О С Т У П   К   С И С Т Е М Е   Б И С К В И Т</Data></Cell>\n
         </Row>\n
         <Row>\n
         </Row>\n
         <Row>\n
          <Cell ss:Index="2" ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'Доступ к Филиалам: ' + vFilialLst + '</Data></Cell>\n
         </Row>\n
         <Row>\n
          <Cell ss:Index="2" ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'Отдел: ' + GetXattrValueEx("_user", STRING(_userid), "ОтделОтч","") + '</Data></Cell>\n
          <Cell ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'Тел.: ' + GetXattrValueEx("_user", STRING(_userid), "Телефон","") + '</Data></Cell>\n
         </Row>\n
         <Row>\n
          <Cell ss:Index="2" ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'Должность: ' + GetXattrValueEx("_user", STRING(_userid), "Должность","") + '</Data></Cell>\n
          <Cell ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'Принтер: ' + GetXattrValueEx("_user", STRING(_userid), "Принтер","") + '</Data></Cell>\n
         </Row>\n
         <Row>\n
          <Cell ss:Index="2" ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'ФАМИЛИЯ И.О.: ' + _user._user-name + '</Data></Cell>\n
          <Cell ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'КОД: ' + _user._userid + '</Data></Cell>\n
         </Row>\n
         <Row>\n
          <Cell ss:Index="2" ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'Табельный номер: ' + GetXattrValueEx("_user", STRING(_userid), "tab-no","") + '</Data></Cell>\n
          <Cell ss:MergeAcross="2" ss:StyleID="Default"><Data ss:Type="String">' + 'Чьи права на транзакции скопированы: ' + _user._user-name + '</Data></Cell>\n
         </Row>\n
         <Row>\n
         </Row>\n
         <Row>\n
          <Cell ss:MergeAcross="5" ss:StyleID="head"><Data ss:Type="String">Д О С Т У П   К   М О Д У Л Я М</Data></Cell>\n
         </Row>\n
         <Row>\n
         </Row>\n
       '.

   /* права доступа к модулям */
   i = 1.
   
   FOR EACH user-proc WHERE
            user-proc.parent EQ 0
   NO-LOCK:
      IF TRIM(user-proc.name-proc) = "" OR CAN-DO( "Корневой*,Администр*,ДБО,КОРРЕСП*,АККРЕДИТ*,УПРАВЛЕНЧ*,ДОВЕРИТ*", TRIM(user-proc.name-proc)) THEN NEXT.
   
      IF i = 1 THEN 
         PUT STREAM vvs UNFORMATTED
            ' <Row>
               <Cell ss:Index="3" ss:StyleID="Default"><Data ss:Type="String">' + '[' + (IF GetSurrPermissionUser("user-proc", STRING(user-proc.public-number), "run", _user._userid) THEN CHR(251) ELSE ' ') + '] ' + user-proc.name-proc + '</Data></Cell>\n
            '.
      ELSE    
         PUT STREAM vvs UNFORMATTED
            '  <Cell ss:Index="5" ss:StyleID="Default"><Data ss:Type="String">' + '[' + (IF GetSurrPermissionUser("user-proc", STRING(user-proc.public-number), "run", _user._userid) THEN CHR(251) ELSE ' ') + '] ' + user-proc.name-proc + '</Data></Cell>\n
              </Row>\n
            '.
          
         i = (IF i = 1 THEN 2 ELSE 1). 
   END.

   /* закрываем тэг <Row>*/
   IF i = 2 THEN
      PUT STREAM vvs UNFORMATTED
         ' </Row>\n
         '.
   
   PUT STREAM vvs UNFORMATTED
      ' <Row>\n 
        </Row>\n 
      '.
   FOR EACH signs 
      WHERE signs.code EQ "ГрупТабл"
        AND signs.file-name EQ "_file"
        AND CAN-DO("person,cust-corp", signs.surrogate)
      NO-LOCK,
      FIRST _File WHERE _File._file-name EQ signs.surrogate 
   NO-LOCK: 
      PUT STREAM vvs UNFORMATTED
         ' <Row>\n 
            <Cell ss:Index="2" ss:MergeAcross="1" ss:StyleID="Default"><Data ss:Type="String">' + '[' + (IF CAN-DO(_file._can-read, _user._userid) THEN CHR(251) ELSE ' ') + '] ' + TRIM(_file._desc) + '</Data></Cell>\n
           </Row>\n 
         '.
   END.

   DO i = 1 TO NUM-ENTRIES(attr-list, ","):
      FIND FIRST xattr 
           WHERE xattr.Class-Code EQ "_user"
             AND xattr.Xattr-Code EQ ENTRY(i, attr-list, ",")
      NO-LOCK NO-ERROR.
      IF NOT AVAIL XATTR THEN NEXT.
      
      attr-name[1] = xattr.name.

      FIND FIRST signs 
           WHERE signs.file-name EQ "_user"
             AND signs.code      EQ ENTRY(i, attr-list, ",")
             AND signs.surrogate EQ _user._userid
      NO-LOCK NO-ERROR.

      IF AVAIL signs THEN      
         attr-value[1] = IF xattr.indexed THEN signs.code-value ELSE signs.xattr-value.
      ELSE
         attr-value[1] = "".
      
      DO count = 1 TO 1:
         PUT STREAM vvs UNFORMATTED
            ' <Row>\n 
               <Cell ss:MergeAcross="2" ss:StyleID="tab_l"><Data ss:Type="String">' + attr-name[count] + '</Data></Cell>\n
               <Cell ss:Index="4" ss:MergeAcross="2" ss:StyleID="tab_l"><Data ss:Type="String">' + attr-value[count] + '</Data></Cell>\n
              </Row>\n
            '.
      END.
   END.
      
   /* подпись */
   PUT STREAM vvs UNFORMATTED
      ' <Row>\n
        </Row>\n
        <Row>\n
         <Cell ss:Index="3" ss:StyleID="Default"><Data ss:Type="String">Руководитель подразделения</Data></Cell>\n
         <Cell ss:Index="4" ss:MergeAcross="1" ss:StyleID="center_b"><Data ss:Type="String"></Data></Cell>\n
        </Row>\n
        <Row>\n
         <Cell ss:Index="4" ss:MergeAcross="1" ss:StyleID="center"><Data ss:Type="String">Ф.И.О.</Data></Cell>\n
        </Row>\n
      '.
         
  PUT STREAM vvs UNFORMATTED
     ' </Table>\n
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

END. /*for each _user*/    

RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}