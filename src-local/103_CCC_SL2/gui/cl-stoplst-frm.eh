ON VALUE-CHANGED OF code.name IN  FRAME edit
DO:
   ASSIGN  
       code.NAME
       code.description[1] = "" . 
   code.description[1]:SCREEN-VALUE = STRING(CalcCodeNum(code.name:SCREEN-VALUE) ). 
END.
ON GO OF FRAME edit
DO: 
   ASSIGN 
      mDataAdd:SCREEN-VALUE     = IF iMode = "F9" AND in-rec-id <> 0 THEN mDataAdd:SCREEN-VALUE ELSE STRING(TODAY,"99/99/9999")
      code.misc[8]:SCREEN-VALUE = USERID("bisquit")
      code.NAME.      
   mOk = UpdateSignsEx(CODE.class, code.class + "," + code.code,"RezolutionData",mDataRez:SCREEN-VALUE).
   mOk = UpdateSignsEx(CODE.CLASS, code.class + "," + code.code,"AddData",mDataAdd:SCREEN-VALUE).
   mOk = UpdateSignsEx(CODE.CLASS, code.class + "," + code.code,"CloseDate",mDataCls:SCREEN-VALUE).
   mOk = UpdateSignsEx(code.class, code.class + "," + code.code,"birthday",mBirthDay:SCREEN-VALUE).
   mOk = UpdateSignsEx(code.class, code.class + "," + code.code,"birthplace",mBirthPlace:SCREEN-VALUE).
END.

ON LEAVE OF code.misc[2] IN FRAME edit
DO:
   IF code.misc[2]:SCREEN-VALUE  IN FRAME edit <> ""    AND 
      code.misc[1]:SCREEN-VALUE  IN FRAME edit <> ""    AND
      code.misc[2]:SCREEN-VALUE  IN FRAME edit <> "0"   
   THEN DO:
      CASE  CODE.misc[1]:SCREEN-VALUE:
         WHEN "û" THEN DO:
            FIND cust-corp WHERE cust-corp.cust-id = int64(CODE.misc[2]:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF AVAIL cust-corp THEN 
            DO:
               ASSIGN 
                  code.misc[3]:SCREEN-VALUE  = TRIM(cust-corp.name-corp) + IF TRIM(cust-corp.name-short)NE "" THEN "/" + TRIM(cust-corp.name-short) ELSE ""
                  code.misc[4]:SCREEN-VALUE  = cust-corp.country-id
                  code.misc[5]:SCREEN-VALUE  = IF cust-corp.inn <> ? THEN  cust-corp.inn ELSE ""
                  code.misc[6]:SCREEN-VALUE  = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"document","")
                  mBirthDay   :SCREEN-VALUE  = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"birthday","")
                  mBirthPlace :SCREEN-VALUE  = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"birthplace","").
            END.                             
         END.
      
         WHEN "ó" THEN DO:
            FIND FIRST person WHERE person.person-id = int64(CODE.misc[2]:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF AVAIL person THEN
            DO:   
               ASSIGN
                  code.misc[3]:SCREEN-VALUE = TRIM(person.name-last) + " " + TRIM(person.first-names)
                  code.misc[4]:SCREEN-VALUE = person.country-id
                  code.misc[5]:SCREEN-VALUE = IF person.inn <> ? THEN person.inn ELSE ""
                  code.misc[6]:SCREEN-VALUE = person.document
                  mBirthDay   :SCREEN-VALUE = STRING(person.birthday,"99/99/9999")
                  mBirthPlace :SCREEN-VALUE = GetXattrValueEx("person",STRING(person.person-id),"birthplace","").
            END.
         END.
      
         WHEN "Å" THEN DO:
            FIND banks WHERE banks.bank-id = int64(CODE.misc[2]:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF AVAIL banks THEN
            DO:
               ASSIGN
                  code.misc[3]:SCREEN-VALUE =   TRIM(banks.name) + IF TRIM(banks.short-name) <> "" THEN "/" +  TRIM(banks.short-name) ELSE ""
                  code.misc[4]:SCREEN-VALUE =   banks.country-id
                  code.misc[6]:SCREEN-VALUE =   "" .
                  
                  
                  FIND FIRST cust-ident   WHERE 
                             cust-ident.cust-cat       =  "Å"
                         AND cust-ident.cust-id        =  banks.bank-id        
                         AND cust-ident.cust-code-type =  "àçç"
                   NO-LOCK  NO-ERROR.
                   IF AVAIL cust-ident AND  {assigned cust-ident.cust-code} THEN
                      code.misc[5]:SCREEN-VALUE =   cust-ident.cust-code  .
            END.
         END.
      END CASE.
   END.
   RETURN.
END.
/* $LINTFILE='cl-stoplst-frm.eh' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.368+03:00' */
/*prosignthtR4F8cJVyWoPPF/4oeYw*/