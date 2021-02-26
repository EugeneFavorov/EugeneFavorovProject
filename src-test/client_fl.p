{globals.i} 
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get date}  
{tmprecid.def}  
{getpath.fun }
{gt-utilities.def}
{sh-defs.i}
{sh-temp.i new}
{sh-defs.i}     /* для acct-pos */
        
DEFINE VARIABLE mBranch LIKE branch.branch-id INITIAL "" NO-UNDO.
DEFINE VARIABLE vOutPath       AS CHARACTER INITIAL "./" NO-UNDO.
DEFINE VARIABLE groupOABS      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE flag           AS CHARACTER NO-UNDO.
DEFINE VARIABLE flag2          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE inn            AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsBankClient  AS LOGICAL   NO-UNDO.   
DEFINE VARIABLE acct_          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE DateOpenAcct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE DateChangeAcct AS DATE NO-UNDO .
DEFINE VARIABLE fio            AS CHARACTER NO-UNDO.
DEFINE VARIABLE tel            AS CHARACTER NO-UNDO.
DEFINE VARIABLE adr            AS CHARACTER NO-UNDO.
DEFINE VARIABLE fax            AS CHARACTER NO-UNDO.
DEFINE VARIABLE Kod_branch     AS CHARACTER NO-UNDO.
DEFINE VARIABLE Date_o_ank     AS CHARACTER NO-UNDO.
DEFINE VARIABLE Date_buhrep    AS CHARACTER NO-UNDO.
DEFINE VARIABLE Cl_bank        AS CHARACTER NO-UNDO.
DEFINE VARIABLE id-branch      AS CHARACTER NO-UNDO.
DEFINE VARIABLE rent_date      AS CHARACTER NO-UNDO.
DEFINE VARIABLE rend_doc       AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE  tt-inf
   FIELD mID            AS INT64
   FIELD mBranch        AS CHARACTER 
   FIELD mName          AS CHARACTER
   FIELD mDateOpenAcct  AS CHARACTER
   FIELD mDateChangeA   AS CHARACTER
   FIELD mAcct          AS CHARACTER
   FIELD mDate_o_ank    AS CHARACTER
   FIELD mDate_buhrep   AS CHARACTER
   FIELD mCl_bank       AS CHARACTER
   FIELD mid-branch     AS CHARACTER
   FIELD mUser          AS CHARACTER
   FIELD mINN           AS CHARACTER
   FIELD mTel           AS CHARACTER 
   FIELD mAdrFakt       AS CHARACTER    
   FIELD mRent_date     AS CHARACTER
   FIELD mRend_doc      AS CHARACTER 
   FIELD groupOABS      AS CHARACTER 
   FIELD stop-list      AS CHARACTER    
  INDEX mID mID
   .
DEF BUFFER b-acct    FOR acct.
DEF BUFFER bacct     FOR acct.
DEF BUFFER b-person      FOR person.
DEF BUFFER b-cust-corp   FOR cust-corp.

def new shared stream puk.
DEFINE VARIABLE fname as char   no-undo.

PAUSE 0.
FORM
  
    /*mBranch VIEW-AS FILL-IN SIZE 10 BY 2 AT ROW 2 COL 2  LABEL  "Код подразделения  "  */
    
    mBranch VIEW-AS COMBO-BOX LIST-ITEMS "*","0000", "0300", "0500" INNER-LINES 4 DROP-DOWN-LIST SIZE 10 BY 4 AT ROW 2 COL 2 LABEL "Подразделение      " 
  /*  tclient  VIEW-AS COMBO-BOX LIST-ITEMS "Ю", "ИП" INNER-LINES 2 DROP-DOWN-LIST SIZE 10 BY 4 AT ROW 3 COL 2 LABEL "Тип клиента        "  */
    groupOABS  VIEW-AS FILL-IN SIZE 10 BY 4 AT ROW 4 COL 2  LABEL  "Группа в Омской АБС"  

    WITH FRAME fMain CENTERED ROW 10 OVERLAY SIDE-LABELS
    TITLE COLOR bright-red "[ ВЫБОР ПАРАМЕТРОВ ОТЧЕТА ]"  .   

ON LEAVE OF mBranch 
    DO:
  
        ASSIGN  mBranch.
        IF NOT CAN-FIND(FIRST branch WHERE CAN-DO(mBranch,branch.branch-id)) AND  (FRAME-VALUE <> 'СВОД') AND (FRAME-VALUE <> '') AND  (FRAME-VALUE <> '*')
            THEN 
        DO:
            RUN Fill-SysMes IN h_tmess ("","","-1","Не найден код подразделения " + mBranch).
            RETURN NO-APPLY "NO-APPLY".
        END.    
    END.

ON F1 OF mBranch 
    DO:
        DO TRANSACTION:
            FIND branch WHERE branch.isbank NO-LOCK NO-ERROR.
            IF AMBIGUOUS branch THEN 
            DO:
                RUN branch#x.p ("","СПРАВОЧНИК ОРГСТРУКТУРЫ",5).
                IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN
                    mBranch = pick-value.
            END.
            ELSE mBranch = dept.branch.
            DISPLAY mBranch WITH FRAME fMain.

        END.
        RETURN NO-APPLY.
    END.

/*ON RETURN OF tclient IN FRAME fMain 
    DO:
        tclient = tclient:SCREEN-VALUE.  
        APPLY "TAB" TO SELF.
    END.

ON "GO",CTRL-G OF FRAME fMain ANYWHERE 
    DO:
        APPLY "LEAVE" TO tclient IN FRAME fMain.
        IF RETURN-VALUE EQ "NO-APPLY" THEN
            RETURN NO-APPLY.   
    END.*/

MAIN:
DO ON ERROR  UNDO, LEAVE 
    ON ENDKEY UNDO, LEAVE WITH FRAME fMain: 
    UPDATE
        mBranch /*tclient*/ groupOABS
        .
END. 
HIDE FRAME fMain NO-PAUSE.
IF KEYFUNC(LASTKEY) EQ "END-ERROR" THEN
    RETURN.

ASSIGN
    mBranch /*tclient*/ groupOABS .

IF (groupOABS = '')  OR (groupOABS = '*') or groupOABS = ? THEN groupOABS = "*".
  
   FOR EACH b-person WHERE b-person.date-out = ? NO-LOCK,
        FIRST bacct WHERE  b-person.person-id EQ bacct.cust-id
        AND    bacct.cust-cat EQ 'Ч' 
        AND    can-do('Депоз,Текущ',bacct.contract) 
        AND    bacct.close-date = ?         
        AND   CAN-DO(mBranch,bacct.filial-id) 
     /*    NO-LOCK ,                   
        
       FIRST signs WHERE signs.file-name EQ "person"
        AND   signs.surrogate EQ STRING(b-person.person-id)
        AND   signs.code EQ "субъект"
        AND signs.code-value EQ "ФЛП"   */                 
        NO-LOCK :  
            
           
            
               if CAN-DO(groupOABS,GetXAttrValueEx("person",string(b-person.person-id),"groupOABS",""))  then
        do:
          tel = "".  
 
           tel = IF (b-person.phone[1] NE ",")  and  {assigned b-person.phone[1]} THEN 
                IF SUBSTRING(b-person.phone[1],1,1) EQ "," THEN
                SUBSTRING(b-person.phone[1],2) 
                ELSE IF SUBSTRING(b-person.phone[1],LENGTH(b-person.phone[1]),1) EQ "," THEN
                SUBSTRING(b-person.phone[1],1,LENGTH(b-person.phone[1]) - 1)
                ELSE b-person.phone[1] 
                ELSE "" +
                IF b-person.phone[2] NE ","  and  {assigned b-person.phone[2]} THEN 
                IF SUBSTRING(b-person.phone[2],1,1) EQ "," THEN 
                IF SUBSTRING(b-person.phone[2],LENGTH(b-person.phone[2]),1) EQ "," THEN
                SUBSTRING(b-person.phone[2],1,LENGTH(b-person.phone[2]) - 1) 
                ELSE b-person.phone[2] 
                ELSE ("," + b-person.phone[2]) ELSE "" +
                IF (b-person.phone[1] EQ "," AND b-person.phone[2] EQ "," AND tel EQ "") THEN "" ELSE "," +
                        IF tel NE "" THEN 
                        IF SUBSTRING(tel,LENGTH(tel),1) EQ "," THEN 
                            SUBSTRING(tel,1,LENGTH(tel) - 1) 
                        ELSE tel 
                ELSE "".    
                                
            CREATE tt-inf.
            ASSIGN               
                tt-inf.mINN = b-person.inn
                tt-inf.mID  = b-person.person-id.              
                     
           RUN ChangeSettings("person",STRING(b-person.person-id),b-person.name-last + ' ' + b-person.first-name,"Ч").        
        end.      
       FIND FIRST code WHERE 
             code.class          = 'StopList'
        and  code.parent         = 'StopList'
        and  code.name           = 'OSK'
        and  code.misc[2]       EQ string(b-person.person-id) 
        and  code.misc[1]       EQ 'Ч' 
        NO-LOCK NO-ERROR.
        
        IF AVAIL(CODE) THEN 
            tt-inf.stop-list ='Да'.
        else
            tt-inf.stop-list ='Нет'.   

    end.  

 
RUN PrintExcel.  

{intrface.del}

PROCEDURE ChangeSettings:
    DEFINE INPUT PARAMETER vTable      AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER vTable-id   AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER short_client AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER type_ AS CHARACTER NO-UNDO. 
    
    mIsBankClient = CAN-FIND(FIRST mail-user WHERE mail-user.cust-cat = type_
                AND mail-user.cust-id  = int64(vTable-id)
                AND CAN-DO(mail-user.acct,bacct.acct)).
                        
     if tel eq "" then   
     tel = if   GetXattrValue(vTable,vTable-id,"cell-phone") eq "" then      
          GetXAttrValueEx(vTable,vTable-id, "tel", "") +  ", " 
            + GetXAttrValueEx(vTable,vTable-id, "fax", "")
          else GetXattrValue(vTable,vTable-id,"cell-phone") .
              
     If trim(tel) eq "," then tel ="".
                  
    RUN acct-pos IN h_base (bacct.acct, bacct.currency,?, ?, ?).

    DateChangeAcct = lastmove.
   
    IF ((DateChangeAcct = ?) OR DateChangeAcct EQ date("30/04/2015")) and  (mBranch  EQ '0000')    THEN
        
        FOR EACH b-acct WHERE  b-acct.number  eq bacct.number                     
            AND    b-acct.filial-id  EQ '0400'   
            NO-LOCK :
            RUN acct-pos IN h_base (b-acct.acct, b-acct.currency,?, DATE("29/04/2015"), ?).
            DateChangeAcct = lastmove.
        END.
        
      RUN RetAdr.p(vTable-id, type_, "АдрФакт", ?, OUTPUT adr).  
     if trim(adr) eq "" then RUN RetAdr.p(vTable-id, type_, "АдрПроп", ?, OUTPUT adr).
      if trim(adr) eq "" then RUN RetAdr.p(vTable-id, type_, "АдрУвед", ?, OUTPUT adr).  
        
    ASSIGN    
    tt-inf.mAcct         = STRING(bacct.acct, "x(20)")    
    tt-inf.mDateOpenAcct = STRING(bacct.open-date)      
    tt-inf.mName         = short_client
    tt-inf.mTel          = tel
    tt-inf.mBranch       = bacct.filial-id
    tt-inf.mDate_o_ank   = GetTempXAttrValueEx(vTable,vTable-id, "ДатаОбнАнкеты",TODAY,"")
    tt-inf.mDate_buhrep  = GetXAttrValueEx(vTable,vTable-id,"date-buhrep","") 
    tt-inf.mRent_date    = GetXAttrValueEx(vTable,vTable-id, "arenda-date","")
    tt-inf.mRend_doc     = GetXAttrValueEx(vTable,vTable-id, "arenda","")
    tt-inf.groupOABS     = GetXAttrValueEx(vTable,vTable-id,"groupOABS","")
    tt-inf.mAdrFakt      = adr
    .        
        
    if mIsBankClient THEN tt-inf.mCl_bank  =  'X'.
    If DateChangeAcct NE ? THEN tt-inf.mDateChangeA = string(DateChangeAcct). 
    IF GetXAttrValueEx(vTable,vTable-id,"branch-id","") <> "" THEN tt-inf.mid-branch    = CHR(39) + GetXAttrValueEx(vTable,vTable-id,"branch-id","").
  
END PROCEDURE. 

PROCEDURE PrintExcel. 
    
    DEFINE VARIABLE DateChangeA      AS CHARACTER NO-UNDO.
    
    
    fname = "./Client2"  + ".xml". 

output stream puk to value (fname)
    UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".
    
  /*  UNBUFFERED CONVERT TARGET "1251".*/
put stream puk unformatted '<?xml version="1.0"?>
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
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Swiss" ss:Size="11"
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
   <Font ss:FontName="Times New Roman" x:CharSet="204" x:Family="Roman"
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
 <Worksheet ss:Name="Лист1">
  <Table  x:FullColumns="1"
   x:FullRows="1" ss:DefaultRowHeight="16">
       
   <Column ss:AutoFitWidth="0" ss:Width="70"/>
   <Column ss:AutoFitWidth="0" ss:Width="500"/>   
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/>  
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="500"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/>   
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/> 
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Column ss:AutoFitWidth="0" ss:Width="120"/>
   <Row ss:AutoFitHeight="0" ss:Height="20">    

   </Row>  
   
   <Row ss:AutoFitHeight="0" ss:Height="12">    
   <Cell ss:StyleID="Default"><Data ss:Type="String">Дата обновления анкет</Data></Cell> 
   </Row>    
      
   <Row ss:AutoFitHeight="0" ss:Height="45">
   <Cell ss:StyleID="s61"><Data ss:Type="String">Код клиента в Бисквит</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Наименование клиента</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата открытия счета</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата последней операции</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">№ расчетного счета</Data></Cell> 
  
   <Cell ss:StyleID="s61"><Data ss:Type="String">Дата обновления анкет</Data></Cell>     
   <Cell ss:StyleID="s61"><Data ss:Type="String">Бухгалтерская отчетность(дата)</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Наличие Банк-Клиент</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Отделение:Код подразделения</Data></Cell>      
   <Cell ss:StyleID="s61"><Data ss:Type="String">Контактный телефон</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Адрес фактический</Data></Cell> 
   <Cell ss:StyleID="s61"><Data ss:Type="String">ИНН</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Аренда: Дата договора</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">Аренда: Договор/Свидельство</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">Стоп-Лист OSK</Data></Cell>'.
     
   if mBranch begins "05" then  
    PUT STREAM puk  UNFORMATTED      
    '<Cell ss:StyleID="s61"><Data ss:Type="String">Группа в Омской АБС</Data></Cell>'.        
   
    
   PUT STREAM puk  UNFORMATTED           '</Row>'.
    
    
   PUT STREAM puk UNFORMATTED    

   '<Row ss:AutoFitHeight="0" ss:Height="12">
   <Cell ss:StyleID="s61"><Data ss:Type="String">1</Data></Cell>   
   <Cell ss:StyleID="s61"><Data ss:Type="String">2</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">3</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">4</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">5</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">6</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">7</Data></Cell>     
   <Cell ss:StyleID="s61"><Data ss:Type="String">8</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">9</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">10</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">11</Data></Cell>     
   <Cell ss:StyleID="s61"><Data ss:Type="String">12</Data></Cell>  
   <Cell ss:StyleID="s61"><Data ss:Type="String">13</Data></Cell> 
   <Cell ss:StyleID="s61"><Data ss:Type="String">14</Data></Cell>
   <Cell ss:StyleID="s61"><Data ss:Type="String">15</Data></Cell>  
    '.      
   
   if mBranch begins "05" then  
    PUT STREAM puk  UNFORMATTED      
    '<Cell ss:StyleID="s61"><Data ss:Type="String">16</Data></Cell>'.        
   
    PUT STREAM puk  UNFORMATTED           '</Row>'.
   
    DateChangeA = If DateChangeAcct NE ? THEN string(DateChangeAcct) ELSE "" .
    
    FOR EACH tt-inf
        NO-LOCK by tt-inf.mId :      
        PUT STREAM puk UNFORMATTED 
                                 '<Row ss:AutoFitHeight="0" ss:Height="35">\n'                                                   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  STRING(tt-inf.mId)   + '</Data></Cell>\n'   CHR(9)                              
                                 '<Cell ss:StyleID="s62"><Data ss:Type="String">' +  tt-inf.mName         + '</Data></Cell>\n'   CHR(9)                                 
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mDateOpenAcct + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mDateChangeA  + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mAcct         + '</Data></Cell>\n'   CHR(9)         
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mDate_o_ank   + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mDate_buhrep  + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mCl_bank      + '</Data></Cell>\n'   CHR(9)                                 
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mid-branch    + '</Data></Cell>\n'   CHR(9)                               
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mTel          + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mAdrFakt      + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mINN          + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mRent_date    + '</Data></Cell>\n'   CHR(9)
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.mRend_doc     + '</Data></Cell>\n'   CHR(9) 
                                 '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.stop-list     + '</Data></Cell>\n'   CHR(9).                              
    
  
   
    if mBranch begins "05" then  
    PUT STREAM puk  UNFORMATTED      
    '<Cell ss:StyleID="s61"><Data ss:Type="String">' +  tt-inf.groupOABS     + '</Data></Cell>\n'   CHR(9).     
   
    PUT STREAM puk  UNFORMATTED           '</Row>\n'.
   
   
     end.
     
     put stream puk unformatted
'
      </Table>
    </Worksheet>
 </Workbook>
'.

output stream puk close.
RUN sndbispc ("file=" + fname + ";class=bq").
     
END PROCEDURE.          