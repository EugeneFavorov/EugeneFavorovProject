/*
               Банковская интегрированная система БИСквитmSymbol
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: Пакет документов при открытие расч.счета
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 
      Modified: 
*/
{globals.i}                                 /* глобальные переменные         */
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{intrface.get xclass}
{intrface.get instrum}
{intrface.get tmess}
{bank-id.i}
{op-ident.i}

DEFINE VARIABLE mSignsV          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSignsV1          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSignsV2          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSignsV3          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSignsV4          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSignsV5          AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAdrReg          AS CHARACTER          NO-UNDO.  /*АдрЮр*/
DEFINE VARIABLE mAdrFact         AS CHARACTER          NO-UNDO. /*АдрФакт*/
DEFINE VARIABLE mDatTMP          AS DATE               NO-UNDO.      /*RegDate*/

DEFINE INPUT PARAMETER iParms AS CHARACTER NO-UNDO.

DEF BUFFER bperson FOR person.

FUNCTION Find_Bank CHAR (INPUT Acct_Bank    AS CHAR) FORWARD.

RUN Clear_TTName.

FOR EACH tmprecid,
   FIRST op WHERE recid(op) EQ tmprecid.id NO-LOCK:
    FIND FIRST _User WHERE _User._Userid EQ op.user-inspector NO-LOCK NO-ERROR.
        IF AVAIL(_User) /*AND (_User._User-Name NE "SYNC")*/ THEN    
           do:
            RUN Insert_TTName("user-name", _User._User-Name).
            RUN Insert_TTName("user-id", _User._Userid).  
            end.
   RUN Insert_TTName("user-data",  REPLACE(STRING(op.op-date , "99/99/9999"), "/", ".") ).
            
  /* for first op-bank of op no-lock,         
   first banks-code where banks-code.bank-code eq op-bank.bank-code and op-bank.bank-code-type eq banks-code.bank-code-type no-lock,
   first banks of banks-code no-lock:     
        RUN Insert_TTName("BankPl",banks.name + " " + banks.town-type + " " + banks.town).   
   end.
                */
   RUN Insert_TTName("DatePrint", REPLACE(STRING(TODAY , "99/99/9999"), "/", ".")).
   If getXAttrValue ("op",STRING (op.op),"ПодозДокумент") NE ""  THEN
   RUN Insert_TTName("VidOp", 'Операция подлежащая обязательному контролю ' +  getXAttrValue ("op",STRING (op.op),"ПодозДокумент")). 
   RUN Insert_TTName("VidOpеtext", 'необычная сделка'). 
   RUN Insert_TTName("OpDetails",op.details).  
   RUN Insert_TTName("OpDate",REPLACE(STRING(op.op-date, "99/99/9999"), "/", ".")). 
  

    For EACH op-entry OF op NO-LOCK:       
                          
        RUN Insert_TTName("SumOp",trim(string(op-entry.amt-rub,">>>>>>>99.99"))). 
        If  op-entry.amt-cur NE 0 Then    
            RUN Insert_TTName("SumInOp",trim(string(op-entry.amt-cur,">>>>>>>99.99"))).
        else
            RUN Insert_TTName("SumInOp",trim(string(op-entry.amt-rub,">>>>>>>99.99")) + ' RUR').                  
                    
        If GetXAttrValue("op",string(op.op),"acct-rec") ne "" THEN 
        Do:
            FIND FIRST op-bank WHERE op-bank.op = op.op AND op-bank.bank-code-type = "МФО-9" NO-LOCK NO-ERROR.
                  RUN Insert_TTName("BankPl",op-bank.bank-name ).     
            RUN AcctAndClient('P',op-entry.acct-cr).
            RUN Insert_TTName("AcctPl",STRING(op.ben-acct,"x(25)")).     
            
                  
          /*  RUN Insert_TTName("BankPl", Find_Bank(op.ben-acct) ).         */    
            RUN Insert_TTName("PBankPl",  Find_Bank(op-entry.acct-cr) ).   
            
        END. 
        If GetXAttrValue("op",string(op.op),"acct-send") ne "" THEN
        Do:             
            RUN AcctAndClient('',op-entry.acct-db). 
            RUN Insert_TTName("PAcctPl",STRING(op.ben-acct,"x(25)")). 
            RUN Insert_TTName("BankPl", Find_Bank(op-entry.acct-db)).
            
             FIND FIRST op-bank WHERE op-bank.op = op.op AND op-bank.bank-code-type = "МФО-9" NO-LOCK NO-ERROR.
                  RUN Insert_TTName("PBankPl",op-bank.bank-name ).     
             
           /* RUN Insert_TTName("PBankPl",Find_Bank(op.ben-acct)).  */ 
        END.               
      
   end.          
 
  /*FIND FIRST history WHERE history.file-name = 'op' 
                                    AND history.field-ref = string(op.op) 
                                    AND history.field-val MATCHES "*op-status*" 
                                    AND history.modify = '{&hi-w}' 
               NO-LOCK NO-ERROR.
MESSAGE  history.modify VIEW-AS ALERT-BOX .


 IF AVAIL history
                  THEN vStatus = ENTRY(LOOKUP("op-status",history.field-val) + 1,history.field-value).
                  ELSE vStatus = cop.op-status. 
                                    
   FIND FIRST history WHERE history.modify EQ "C" 
    AND NOT history.FIELD-ref BEGINS "_system_"
    AND history.field-ref EQ STRING(person.person-id)
    AND history.file-name EQ 'person' NO-LOCK NO-ERROR.
*//*
FIND FIRST _User WHERE _User._Userid EQ history.user-id  NO-LOCK NO-ERROR.
               
    RUN Insert_TTName("user-name", _User._User-Name) NO-ERROR.*/               
end.

    RUN Insert_TTName("user-inspector", ENTRY(2, iParms, "|")) NO-ERROR.
    RUN Insert_TTName("data-inspector", REPLACE(STRING(TODAY , "99/99/9999"), "/", ".")). 
                                                                                      
                      /* Вывод данных по шаблону iParms (до "|") в файл отчета */ 
          RUN printvd.p (ENTRY(1, iParms, "|"), INPUT TABLE ttnames).  
        {intrface.del comm}

PROCEDURE AcctAndClient:   
    DEF INPUT PARAMETER flag                          AS CHARACTER NO-UNDO. 
    DEF INPUT PARAMETER acct_pay                    AS CHARACTER NO-UNDO. 

DEFINE VARIABLE fl          AS CHARACTER          NO-UNDO.    
    
    FOR FIRST acct WHERE acct.acct EQ acct_pay  NO-LOCK.
         
        /*   IF AVAILABLE acct THEN
           do:*/
           fl = flag + "AcctPl".
        RUN Insert_TTName(fl,STRING(acct.acct,"x(25)")).        
                     
        IF  acct.cust-cat EQ 'Ч' THEN
            FOR FIRST person WHERE person.person-id EQ acct.cust-id NO-LOCK:
                mSignsV = STRING( person.name-last + " " + person.first-names ).
                fl = flag + "PersonName".
                RUN Insert_TTName(fl,mSignsV).
                RUN RetAdr.p(person.person-id, "Ч", "АдрПроп", ?, OUTPUT mAdrReg).
                fl = flag + "PersonAdrP".
                RUN Insert_TTName(fl,mAdrReg). 
                RUN RetAdr.p(person.person-id, "Ч", "АдрФакт", ?, OUTPUT mAdrFact).
                fl = flag + "PersonAdrF".
                RUN Insert_TTName(fl,mAdrFact).    
                    
                mSignsV  = GetCodeName("КодДокум", person.document-id) . 
                fl = flag + "PersonDoc".               
                RUN Insert_TTName(fl,mSignsV).
                    
                IF NUM-ENTRIES(person.document," ") EQ 2 THEN 
                DO:
                    fl = flag + "PersonDocS".
                    RUN Insert_TTName(fl,ENTRY(1,person.document," ")).
                    fl = flag + "PersonDocNo".
                    RUN Insert_TTName(fl,ENTRY(2,person.document," ")).                                           
                END.
                ELSE 
                    IF NUM-ENTRIES(person.document," ") EQ 1 THEN 
                    DO:
                        fl = flag + "PersonDocS".
                        RUN Insert_TTName(fl,""). 
                        fl = flag + "PersonDocNo".
                        RUN Insert_TTName(fl,person.document).           
                    END.
                    ELSE 
                    DO:
                        fl = flag + "PersonDocS".
                        RUN Insert_TTName(fl,ENTRY(1,person.document," ") + " " +   ENTRY(2,person.document," ") ).
                        fl = flag + "PersonDocNo".
                        RUN Insert_TTName(fl,ENTRY(3,person.document," ")).
                    END.             
                                          
                fl = flag + "PersonIssue".                                    
                IF NUM-ENTRIES(person.issue,",") LT 2 THEN               
                    RUN Insert_TTName(fl,person.issue).              
                ELSE 
                DO:
                    RUN Insert_TTName("PersonIssue",ENTRY(1,person.issue,",")).
                    fl = flag + "PersonDocKod".
                    RUN Insert_TTName(fl,ENTRY(2,person.issue,",")).
                END.          
                                 
               mSignsV = GetXAttrValueEx("person", STRING(person.person-id),"Document4Date_vid","").
               fl = flag + "PersonDocDate" . 
                RUN Insert_TTName(fl,STRING(mSignsV,"99.99.9999")).                    
                                
                fl = flag + "PersonDR".  
                RUN Insert_TTName(fl,STRING(person.birthday,"99.99.9999")).
                mSignsV = GetXAttrValueEx("person",STRING(person.person-id),"BirthPlace","").    
                fl = flag + "PersonMR".
                RUN Insert_TTName(fl,mSignsV).
            END.
        ELSE /*плательщик юр.лицо*/
            FOR FIRST cust-corp  WHERE cust-corp.cust-id EQ acct.cust-id NO-LOCK:
                fl = flag + "CustName".
                RUN Insert_TTName(fl, IF cust-corp.name-short NE '' THEN cust-corp.name-short ELSE cust-corp.name-corp).                   
                RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрЮр", ?,OUTPUT mAdrReg).
                fl = flag + "CustMestReg".
                RUN Insert_TTName(fl,mAdrReg).         
                RUN RetAdr.p(cust-corp.cust-id,  "Ю", "АдрФакт", ?,OUTPUT mAdrFact).
                fl = flag + "CustMest".
                RUN Insert_TTName(fl, mAdrFact). 
                fl = flag + "CustOKPO". 
                RUN Insert_TTName(fl,cust-corp.okpo).
                mSignsV = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ОГРН","").  
                fl = flag + "CustRegNo".                                       
                RUN Insert_TTName(fl, mSignsV). 
                mDatTMP = DATE(GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"RegDate","")) NO-ERROR.
                fl = flag + "CustDateReg".
                RUN Insert_TTName(fl, REPLACE(STRING(mDatTMP, "99/99/9999"), "/", ".")).
                fl = flag + "CustINN".
                RUN Insert_TTName(fl,cust-corp.inn).     
                mSignsV = ''. 
                /*поиск директора и его днных в связаных субъектах*/     
                FOR FIRST cust-role WHERE cust-role.file-name EQ "cust-corp"
                    AND   cust-role.surrogate EQ STRING(cust-corp.cust-id)
                    AND   cust-role.class-code MATCHES '*иректор*'  NO-LOCK :      

                    /* {additem.i mSignsV cust-role.cust-name} */             
                    FIND FIRST bperson WHERE cust-role.cust-id EQ string(bperson.person-id)
                        NO-LOCK NO-ERROR.
                
                    mSignsV = STRING( bperson.name-last + " " + bperson.first-names ).
                    fl = flag + "DirFio".
                    RUN Insert_TTName(fl,mSignsV).
                    
                    IF NUM-ENTRIES(bperson.document," ") EQ 2 THEN 
                    DO:
                        fl = flag + "DirDocS".
                        RUN Insert_TTName(fl,ENTRY(1,bperson.document," ")).
                        fl = flag + "DirDocNo".                       
                        RUN Insert_TTName(fl,ENTRY(2,bperson.document," ")).                                           
                    END.
                    ELSE 
                        IF NUM-ENTRIES(bperson.document," ") EQ 1 THEN 
                        DO:
                            fl = flag + "DirDocS".
                            RUN Insert_TTName(fl,"").
                            fl = flag + "DirDocNo".   
                            RUN Insert_TTName(fl,bperson.document).           
                        END.
                        ELSE 
                        DO:
                            fl = flag + "DirDocS".
                            RUN Insert_TTName(fl,ENTRY(1,bperson.document," ") + " " +   ENTRY(2,bperson.document," ") ).
                            fl = flag + "DirDocNo".
                            RUN Insert_TTName(fl,ENTRY(3,bperson.document," ")).
                        END.  
                    fl = flag + "DirIssue".    
                    IF NUM-ENTRIES(bperson.issue,",") LT 2 THEN                                
                        RUN Insert_TTName(fl,bperson.issue).              
                    ELSE 
                    DO:
                        RUN Insert_TTName(fl,ENTRY(1,bperson.issue,",")).
                        fl = flag + "DirDocKod".
                        RUN Insert_TTName(fl,ENTRY(2,bperson.issue,",")).
                    END.               
                           
                    mSignsV  = GetCodeName("КодДокум", bperson.document-id) .        
                    fl = flag + "DirDoc".         
                    RUN Insert_TTName(fl,mSignsV). 
                    
                    mSignsV = GetXAttrValueEx("person", STRING(bperson.person-id),"Document4Date_vid",""). 
                    fl = flag +  "DirDocDate".
                    RUN Insert_TTName(fl, REPLACE( mSignsV, "/", ".")).               
                                        
                    RUN RetAdr.p(bperson.person-id, "Ч", "АдрПроп", ?, OUTPUT mAdrReg). 
                    fl = flag + "DirMestReg". 
                    RUN Insert_TTName(fl,mAdrReg). 
                    RUN RetAdr.p(bperson.person-id, "Ч", "АдрФакт", ?, OUTPUT mAdrFact).
                    fl = flag + "DirMest".
                    RUN Insert_TTName(fl,mAdrFact). 
                    fl = flag + "DirDR".                 
                    RUN Insert_TTName(fl,STRING(bperson.birthday,"99.99.9999")).
                    mSignsV = GetXAttrValueEx("person",STRING(bperson.person-id),"BirthPlace",""). 
                    fl = flag + "DirMR".                    
                    RUN Insert_TTName(fl,mSignsV).
                END.               
                fl = flag + "DirFio".
                IF NOT AVAILABLE(cust-role) THEN                         
                    RUN Insert_TTName(fl, GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"ФИОРук",?)).                               
            END.       
    END. 
END PROCEDURE.

FUNCTION Find_Bank CHAR (INPUT Acct_Bank    AS CHAR):
    FOR FIRST acct  WHERE acct.acct EQ op-entry.acct-cr  NO-LOCK,
        FIRST branch  WHERE branch.branch-id EQ acct.filial-id NO-LOCK ,
        FIRST banks  WHERE banks.bank-id EQ branch.bank-id  NO-LOCK :
        RETURN  banks.name.
    end.                                      
END FUNCTION.
   