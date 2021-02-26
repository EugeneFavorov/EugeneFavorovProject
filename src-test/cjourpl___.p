/*
               Банковская интегрированная система БИСквит 
    Copyright: (C) Плюс Банк
     Filename: cjourpl.p
      Comment: Кассовый журнал по приходу 2481-У.
   Parameters:
         Uses:
      Used by:
      Created: 29.10.2014   guva
     Modified: 13.11.2014   guva
*/
DEFINE INPUT  PARAMETER iParam AS CHARACTER NO-UNDO.

{globals.i}
{intrface.get acct}
{intrface.get instrum}
{intrface.get prnvd}
{wordwrap.def}
{cjourpl.def}
{intrface.get tmess}

DEFINE VARIABLE mTypeJour AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE vI AS INT64 NO-UNDO.
DEFINE VARIABLE mKassir AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE mOperYes AS LOGICAL INITIAL YES NO-UNDO.
{cjour.pro}

DEFINE VARIABLE vAmt         as DECIMAL                 NO-UNDO.
DEFINE VARIABLE vAltAmt      LIKE tt-totals-rec.alt-amt NO-UNDO.
DEFINE VARIABLE mAmtAll      LIKE tt-totals-rec.alt-amt NO-UNDO.
DEFINE VARIABLE mAmtAllSecond LIKE tt-totals-rec.alt-amt NO-UNDO.
DEFINE VARIABLE vValidCSList AS CHARACTER INITIAL ""    NO-UNDO.
DEFINE VARIABLE vAuthor      AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vTmp6        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vCurrency    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vAcct        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE mSignatures  AS CHARACTER               NO-UNDO.
DEFINE VARIABLE mNumProc     AS INT64                   NO-UNDO.
DEFINE VARIABLE kass         AS CHARACTER               NO-UNDO.
DEFINE VARIABLE kassFIO      AS CHARACTER               NO-UNDO.
DEFINE VARIABLE mAcct        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vbranch      AS CHARACTER               NO-UNDO.
DEFINE VARIABLE formatAmt    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE user_id      AS CHARACTER               NO-UNDO.
DEFINE VARIABLE user_inspector AS CHARACTER               NO-UNDO.



{get-fmt.i 
   &obj=B-Acct-Fmt
}

DEFINE BUFFER b-totals-rec  FOR tt-totals-rec.
DEFINE BUFFER bop-entry     FOR op-entry.
DEFINE BUFFER bop           FOR op.
DEFINE BUFFER xuser         FOR _user.
DEFINE BUFFER bsessions     FOR sessions.
DEFINE BUFFER bsigns        FOR signs.
DEFINE BUFFER bbsigns       FOR signs.
DEFINE BUFFER bacct         FOR acct.

FUNCTION GetUserIDHis RETURNS CHARACTER PRIVATE
    (INPUT op_user AS char,
    INPUT op_op AS char 
    ): 
   
    FIND FIRST   history WHERE history.field-ref EQ op_op                      
        and history.file-name eq 'op'             
        and  lookup("op-status,КЗ",history.field-value) > 0
        NO-LOCK  NO-ERROR.     
  
    IF NOT AVAIL history THEN 
        RETURN op_user.  
    else 
        RETURN history.user-id.  
END FUNCTION.

FUNCTION GetUserName RETURNS CHARACTER PRIVATE
    (INPUT iUser AS CHARACTER  ) FORWARD.

{tmprecid.def}

do on error undo, return on endkey undo, return:
    run bran#ot.p ("*", 3).
    if return-value = "error" or keyfunc(lastkey) = "END-ERROR" then
       undo, return.
    if not can-find(first tmprecid) then do:
       message "Не выбраны отделения!" view-as alert-box.
       undo, retry.
    end.
end.

IF LOWER(iParam) NE "расход" THEN
    mTypeJour = "приход".
ELSE
    mTypeJour = "расход".

{getdate.i &DispAfterDate = "SKIP~
                                ""Отчет по""
                                AT ROW 2 COL 6 
                                mKassir~
                                LABEL """"~
                                HELP  ""Отбор по контролерам""~
                                 VIEW-AS RADIO-SET RADIO-BUTTONS~
                                 "Кассирам", "yes",
                                 "Операционистам", "no"
                                AT ROW 2 COL 17"                                                                
              &UpdAfterDate  = "mKassir"}              
              
IF mTypeJour EQ "приход" THEN
Do:
FOR EACH tmprecid, 
FIRST branch WHERE RECID(branch) = tmprecid.id NO-LOCK,
 EACH bop-entry
	WHERE bop-entry.op-date EQ end-date
	AND bop-entry.acct-db BEGINS "20202"
	AND  Not (bop-entry.acct-cr BEGINS "70603")
	AND  Not (bop-entry.acct-cr BEGINS "70608")
	AND  Not (bop-entry.acct-cr Matches "70601*6101*")
	AND  Not (bop-entry.acct-cr Matches "70601*6102*")
	AND  Not (bop-entry.acct-cr Matches "70606*6101*")
    AND  Not (bop-entry.acct-cr Matches "70606*6102*")
	NO-LOCK,
	 EACH acct
        where acct.acct eq bop-entry.acct-db
        AND acct.branch-id eq  branch.branch-id      
        NO-LOCK,  
	FIRST bop
	   WHERE bop.op EQ bop-entry.op	   	 
	  	   AND CAN-DO("!А*,*", bop.op-status)
	NO-LOCK 
	BREAK BY bop-entry.acct-db :	    
	  IF FIRST-OF(bop-entry.acct-db) THEN
	   DO: 
    	   FIND FIRST tt-header
    	       WHERE tt-header.acct     = bop-entry.acct-db 
    	       AND tt-header.currency = IF SUBSTRING(bop-entry.acct-db, 6, 3) EQ "810" THEN "" ELSE SUBSTRING(bop-entry.acct-db, 6, 3)
    	   NO-ERROR.
           IF NOT AVAILABLE(tt-header) THEN 
           DO:
               CREATE tt-header.
               ASSIGN
                   tt-header.date      = end-date
                   tt-header.acct      = bop-entry.acct-db
                   tt-header.currency  = IF SUBSTRING(bop-entry.acct-db, 6, 3) EQ "810" THEN "" ELSE SUBSTRING(bop-entry.acct-db, 6, 3)
                   .
           END.        
       END.
       FIND FIRST doc-type
       	WHERE doc-type.doc-type EQ bop.doc-type
       	NO-LOCK NO-ERROR.
       IF AVAILABLE(doc-type) THEN
          mDocCode = doc-type.digital.
       ELSE
          mDocCode = "03".
          
          if (user-inspector eq ?) or (user-inspector eq "") THEN user_inspector = user_id.
          
                    /* CurToCurWork("Учетный", op-entry.currency, "", op.op-date, op-entry.amt-cur)*/
          If bop.doc-type eq '037п' THEN
             do:
                 user_id = bop.user-inspector.
                 
                 user_inspector = GetUserIDHis(bop.user-id,STRING(bop.op)).    
                  if (user-inspector eq ?) or (user-inspector eq "") THEN user_inspector = user_id.
             end.
           else  
            do:
                 user_id = GetUserIDHis(bop.user-id,STRING(bop.op)).
                 user_inspector = bop.user-inspector.
                  if (user-inspector eq ?) or (user-inspector eq "") THEN user_inspector = user_id.
             end.           
          
         CREATE tt-journal-rec.
            ASSIGN
               tt-journal-rec.c-acct   = bop-entry.acct-db
               tt-journal-rec.doc-num  = bop.doc-num
               tt-journal-rec.acct     = IF  (bop-entry.acct-cr BEGINS "303")  THEN  bop.ben-acct else  bop-entry.acct-cr
               tt-journal-rec.doc-code = mDocCode
               tt-journal-rec.amt      = IF tt-header.currency eq "" THEN  bop-entry.amt-rub else bop-entry.amt-cur   
               tt-journal-rec.symbol   = bop-entry.symbol
               tt-journal-rec.op       = bop.op
               tt-journal-rec.buh      = user_id
               tt-journal-rec.kassir   = user_inspector
               tt-journal-rec.op-status = bop.op-status
               .     
END. 
 
end.
ELSE
    FOR EACH tmprecid, 
        FIRST branch WHERE RECID(branch) = tmprecid.id NO-LOCK ,
        EACH bop-entry
        WHERE bop-entry.op-date EQ end-date
        AND bop-entry.acct-cr BEGINS "20202"
        AND  Not (bop-entry.acct-db BEGINS "70603")
        AND  Not (bop-entry.acct-db BEGINS "70608")
        AND  Not (bop-entry.acct-db Matches "70601*6101*")
        AND  Not (bop-entry.acct-db Matches "70601*6102*")
        AND  Not (bop-entry.acct-db Matches "70606*6101*")
        AND  Not (bop-entry.acct-db Matches "70606*6102*")
        NO-LOCK,
        EACH acct
        where acct.acct eq bop-entry.acct-cr
        AND acct.branch-id eq  branch.branch-id      
        NO-LOCK,  
        FIRST bop
        WHERE bop.op EQ bop-entry.op       
        AND CAN-DO("!А*,*", bop.op-status)
        NO-LOCK
        BREAK BY bop-entry.acct-cr:
       IF FIRST-OF(bop-entry.acct-cr) THEN
       DO: 
           FIND FIRST tt-header
              WHERE tt-header.acct     = bop-entry.acct-cr 
              AND tt-header.currency = IF SUBSTRING(bop-entry.acct-cr, 6, 3) EQ "810" THEN "" ELSE SUBSTRING(bop-entry.acct-cr, 6, 3) 
           NO-ERROR.
           IF NOT AVAILABLE(tt-header) THEN 
           DO:
               CREATE tt-header.
               ASSIGN
                   tt-header.date      = end-date
                   tt-header.acct      = bop-entry.acct-cr
                   tt-header.currency  = IF SUBSTRING(bop-entry.acct-cr, 6, 3) EQ "810" THEN "" ELSE SUBSTRING(bop-entry.acct-cr, 6, 3)
                   .
           END.      
                   
       END.
       FIND FIRST doc-type
        WHERE doc-type.doc-type EQ bop.doc-type
        NO-LOCK NO-ERROR.
       IF AVAILABLE(doc-type) THEN
          mDocCode = doc-type.digital.
       ELSE
          mDocCode = "04".
          
      IF tt-header.currency NE "" THEN vAmt = bop-entry.amt-cur. ELSE vAmt = bop-entry.amt-rub.
      
      If bop.doc-type eq '037п' THEN
             do:
                 user_id = bop.user-inspector.
                 user_inspector = GetUserIDHis(bop.user-id,STRING(bop.op)).
                  if (user-inspector eq ?) or (user-inspector eq "") THEN user_inspector = user_id.
             end.
           else  
            do:
                 user_id = GetUserIDHis(bop.user-id,STRING(bop.op)).
                 user_inspector = bop.user-inspector.
                  if (user-inspector eq ?) or (user-inspector eq "") THEN user_inspector = user_id.
             end.        
          
         CREATE tt-journal-rec.
            ASSIGN
               tt-journal-rec.c-acct   = bop-entry.acct-cr
               tt-journal-rec.doc-num  = bop.doc-num
               tt-journal-rec.acct     = IF  bop-entry.acct-db BEGINS "303" THEN  bop.ben-acct else  bop-entry.acct-db 
               tt-journal-rec.doc-code = mDocCode
               tt-journal-rec.amt      = IF tt-header.currency eq "" THEN  bop-entry.amt-rub else bop-entry.amt-cur   
               tt-journal-rec.symbol   = bop-entry.symbol
               tt-journal-rec.op       = bop.op
               tt-journal-rec.buh      = user_id
               tt-journal-rec.kassir   = user_inspector
               tt-journal-rec.op-status = bop.op-status
               .
END.
vbranch = branch.name.

/*поиск кассира нужен код ВЫБРАННОГО пордразделения!!!  */
kass = GetXAttrValue("_user", tt-journal-rec.kassir, "Отделение").            
FOR EACH bsessions WHERE 
    bsessions.op-date    EQ end-date
    AND bsessions.Branch-Id  EQ  /*kass */ branch.branch-id         
    NO-LOCK,      
    EACH bsigns WHERE 
        bsigns.file-name  EQ "sessions"
        AND bsigns.code       EQ "ЗавКассой"
        AND bsigns.surrogate  EQ STRING(bsessions.dpr-id)
        AND bsigns.code-value EQ "Да"
        NO-LOCK,
        FIRST bbsigns where bbsigns.file-name eq "_user" and bbsigns.surrogate eq caps(bsessions.USER-ID) and 
            bbsigns.code eq "Отделение" and bbsigns.code-value EQ branch.branch-id
            NO-LOCK,
            FIRST xuser
                WHERE xuser._userid EQ bbsigns.surrogate          
                NO-LOCK: 
        
      kassFIO =  xuser._User-name.    
END.
               
IF NOT CAN-FIND(FIRST tt-journal-rec) THEN 
DO:
    RUN Fill-SysMes IN h_tmess ('','','',"На " + STRING(end-date) + " нет данных для отчета").
    RETURN.
END.

RUN BeginCircle_TTName IN h_prnvd ("doc").
FOR EACH tt-header 
    BY SUBSTRING(STRING(tt-header.acct),1,8)
    BY SUBSTRING(STRING(tt-header.acct),14):  
    tt-header.type = mTypeJour.
    ASSIGN
        tt-header.form-code = "0401704" WHEN mTypeJour = "приход"
        tt-header.form-code = "0401705" WHEN mTypeJour = "расход"
    .

   FIND FIRST tt-journal-rec 
        WHERE tt-journal-rec.c-acct = tt-header.acct
   NO-ERROR.
/*   vCSType = getCSType(tt-header.type). */
   RUN CalcOverall(tt-header.acct, OUTPUT vAmt, OUTPUT vAltAmt).
   mOperYes = YES.
  
  IF NOT mKassir THEN
   DO:
      FOR EACH tt-journal-rec
        WHERE  
        tt-journal-rec.c-acct = tt-header.acct 
      /*  AND CAN-DO("!20202*,*", tt-journal-rec.acct)*/:
        LEAVE.
      END.  
       IF NOT AVAILABLE(tt-journal-rec) THEN
          mOperYes = NO.
   END.
   
   IF mOperYes THEN
   DO:
        vAuthor = tt-header.author.
/*        IF mVarVal[6] NE "?" AND mVarVal[6] NE "" THEN
           vTmp6 = mVarVal[6].
        ELSE
            IF tt-header.currency NE "" THEN */
               vTmp6 = " Код валюты: " + GetISOCode(tt-header.currency).

           /* шапка отчета */
           
           RUN Insert_TTName IN h_prnvd ("branch[doc]", vbranch).
           RUN Insert_TTName IN h_prnvd ("lq[doc]", fill('-',length(vbranch))).
           RUN Insert_TTName IN h_prnvd ("code[doc]",  tt-header.form-code).
           RUN Insert_TTName IN h_prnvd ("author[doc]",vAuthor).
           RUN Insert_TTName IN h_prnvd ("jtype[doc]",getJournalTypeStr(tt-header.type)).
           RUN Insert_TTName IN h_prnvd ("ttype[doc]",STRING(tt-header.type + "а","x(9)")).
           RUN Insert_TTName IN h_prnvd ("date[doc]",(IF mDatePar EQ "НЕТ" THEN
                                                            {term2str tt-header.date tt-header.date yes}  
                                                         ELSE
        
                                                            "   " + STRING(tt-header.date, "99.99.9999"))).
           RUN Insert_TTName IN h_prnvd ("det[doc]"," Счет по учету кассы № " + STRING(tt-journal-rec.c-acct, GetAcctFmt("b"))).
           RUN Insert_TTName IN h_prnvd ("pos6[doc]",vTmp6).
    
/*           vSortOrder   = FGetSetting("СортКасСумм", "", "Да") EQ "Да". */
           /* строки отчета */
           RUN BeginCircle_TTName IN h_prnvd ("row").  
           IF mKassir THEN DO:
          /*    FOR EACH tt-journal-rec 
                WHERE (tt-journal-rec.c-acct = tt-header.acct) /*and (tt-journal-rec.kassir NE "")*/
                BREAK BY tt-journal-rec.kassir 
                BY tt-journal-rec.acct 
                BY tt-journal-rec.doc-num */
                
                 FOR EACH tt-journal-rec 
                WHERE (tt-journal-rec.c-acct = tt-header.acct) /*and (tt-journal-rec.kassir NE "")*/
                BREAK BY tt-journal-rec.kassir 
                BY tt-journal-rec.buh : 
                 IF FIRST-OF(tt-journal-rec.kassir) THEN
                    mAmtAll = 0.0.
                 mAmtAll = mAmtAll + tt-journal-rec.amt.
                 
                  IF FIRST-OF(tt-journal-rec.buh) THEN
                    mAmtAllSecond = 0.0.
                 mAmtAllSecond = mAmtAllSecond + tt-journal-rec.amt.

                 vCurrency = SUBSTRING(tt-journal-rec.acct, 6, 3).
                 IF tt-header.currency EQ FGetSetting("КодНацВал", ?, "810") THEN
                    vCurrency = "". 
                
                           vAcct =  string({out-fmt.i tt-journal-rec.acct fmt}).            
                /*    {find-act.i &acct = vAcct
                                 &curr = vCurrency}*/
                     IF AVAILABLE acct THEN
                        vAcct = STRING(tt-journal-rec.acct , GetAcctFmt(acct.acct-cat)).
                        
                     if (tt-journal-rec.amt ne 0) then 
                        formatAmt =  REPLACE(STRING(tt-journal-rec.amt,">>,>>>,>>>,>>>.99"), ".", "-").
                      else formatAmt =  '0-00'.   
                                                                   
                      RUN Insert_TTName IN h_prnvd ("num[row]",STRING(tt-journal-rec.doc-num , "x(11)")).
                      RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(vAcct, "x(24)")).
                      RUN Insert_TTName IN h_prnvd ("code[row]",STRING(tt-journal-rec.doc-code, "xx")).
                      RUN Insert_TTName IN h_prnvd ("amt[row]",formatAmt).
                      
/*                        RUN Insert_TTName IN h_prnvd ("note[row]",STRING(IF CAN-DO(vValidCSList, tt-journal-rec.symbol) THEN tt-journal-rec.symbol ELSE "", "xx")). */
                      RUN Insert_TTName IN h_prnvd ("note[row]",STRING(tt-journal-rec.symbol, "xx")).
                      RUN Insert_TTName IN h_prnvd ("altamt[row]",(IF tt-journal-rec.op-status GE CHR(251) THEN "подтверждено" ELSE "не подтверждено")).
/*                        RUN Insert_TTName IN h_prnvd ("buh[row]",GetUserName(tt-journal-rec.buh)).
                      RUN Insert_TTName IN h_prnvd ("kas[row]",GetUserName(tt-journal-rec.kassir)).
*/                        
                      RUN Insert_TTName IN h_prnvd ("buh[row]",GetUserName(tt-journal-rec.buh)).
                      RUN Insert_TTName IN h_prnvd ("kas[row]",GetUserName(tt-journal-rec.kassir)).
                      RUN NextCircle_TTName IN h_prnvd ("row").
                      
                      IF LAST-OF(tt-journal-rec.buh) THEN
                      DO:
                        if (mAmtAllSecond ne 0) then 
                        formatAmt =  REPLACE(STRING(mAmtAllSecond,">>,>>>,>>>,>>>.99"), ".", "-").
                        else formatAmt =  '0-00'.   
                        RUN Insert_TTName IN h_prnvd ("num[row]"," Итого по  ").
                        RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(GetUserName(tt-journal-rec.buh), "x(24)")).
                        RUN Insert_TTName IN h_prnvd ("code[row]","  ").
                        RUN Insert_TTName IN h_prnvd ("amt[row]",formatAmt /*REPLACE(STRING(mAmtAll,">>,>>>,>>>,>>>.99"), ".", "-")*/ ).
                        RUN Insert_TTName IN h_prnvd ("note[row]","  "). 
                        RUN Insert_TTName IN h_prnvd ("altamt[row]","            ").
                        RUN Insert_TTName IN h_prnvd ("buh[row]"," ").
                        RUN Insert_TTName IN h_prnvd ("kas[row]"," ").
                        RUN NextCircle_TTName IN h_prnvd ("row").
                      END.   
                      IF LAST-OF(tt-journal-rec.kassir) THEN
                      DO:
                        if (mAmtAll ne 0) then 
                        formatAmt =  REPLACE(STRING(mAmtAll,">>,>>>,>>>,>>>.99"), ".", "-").
                        else formatAmt =  '0-00'.   
                        RUN Insert_TTName IN h_prnvd ("num[row]"," Итого по  ").
                        RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(GetUserName(tt-journal-rec.kassir), "x(24)")).
                        RUN Insert_TTName IN h_prnvd ("code[row]","  ").
                    /*    RUN Insert_TTName IN h_prnvd ("amt[row]",formatAmt /*REPLACE(STRING(mAmtAll,">>,>>>,>>>,>>>.99"), ".", "-")*/ ).*/
                        RUN Insert_TTName IN h_prnvd ("note[row]","  "). 
                        RUN Insert_TTName IN h_prnvd ("altamt[row]","            ").
                        RUN Insert_TTName IN h_prnvd ("buh[row]"," ").
                        RUN Insert_TTName IN h_prnvd ("kas[row]"," ").
                        RUN NextCircle_TTName IN h_prnvd ("row").
                      END.
              END.
           END.
           ELSE DO:
              FOR EACH tt-journal-rec 
              WHERE tt-journal-rec.c-acct = tt-header.acct         
              BREAK BY tt-journal-rec.buh  BY  tt-journal-rec.kassir:

               IF FIRST-OF(tt-journal-rec.buh) THEN
                  mAmtAll = 0.0.
               mAmtAll = mAmtAll + tt-journal-rec.amt.
               
               IF FIRST-OF(tt-journal-rec.kassir) THEN               
                    mAmtAllSecond = 0.0.
                 mAmtAllSecond = mAmtAllSecond + tt-journal-rec.amt.
               
                 vCurrency = SUBSTRING(tt-journal-rec.acct, 6, 3).
                 IF tt-header.currency EQ FGetSetting("КодНацВал", ?, "810") THEN
                    vCurrency = "".
                     
                       vAcct =  string({out-fmt.i tt-journal-rec.acct fmt}).            
                /*    {find-act.i &acct = vAcct
                                 &curr = vCurrency}*/
                     IF AVAILABLE acct THEN
                        vAcct = STRING(tt-journal-rec.acct , GetAcctFmt(acct.acct-cat)).
                        
                       if (tt-journal-rec.amt ne 0) then 
                        formatAmt =  REPLACE(STRING(tt-journal-rec.amt,">>,>>>,>>>,>>>.99"), ".", "-").
                      else formatAmt =  '0-00'.      
                    
                    RUN Insert_TTName IN h_prnvd ("num[row]",STRING(tt-journal-rec.doc-num , "x(11)")).
                    RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(vAcct, "x(24)")).
                    RUN Insert_TTName IN h_prnvd ("code[row]",STRING(tt-journal-rec.doc-code, "xx")).
                    RUN Insert_TTName IN h_prnvd ("amt[row]",formatAmt).
                    RUN Insert_TTName IN h_prnvd ("note[row]",STRING(tt-journal-rec.symbol, "xx")).
                    RUN Insert_TTName IN h_prnvd ("altamt[row]",(IF tt-journal-rec.op-status GE CHR(251) THEN "подтверждено" ELSE "не подтверждено")).
                    RUN Insert_TTName IN h_prnvd ("buh[row]",GetUserName(tt-journal-rec.buh)).
                    RUN Insert_TTName IN h_prnvd ("kas[row]",GetUserName(tt-journal-rec.kassir)).
                    RUN NextCircle_TTName IN h_prnvd ("row").
                     IF LAST-OF(tt-journal-rec.kassir) THEN
                    DO:
                      if (mAmtAll ne 0) then 
                        formatAmt = REPLACE(STRING(mAmtAllSecond,">>,>>>,>>>,>>>.99"), ".", "-"). 
                      else formatAmt = '0-00'.  
                      RUN Insert_TTName IN h_prnvd ("num[row]"," Итого по  ").
                      RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(GetUserName(tt-journal-rec.kassir), "x(24)")).
                      RUN Insert_TTName IN h_prnvd ("code[row]","  ").
                      RUN Insert_TTName IN h_prnvd ("amt[row]",formatAmt).
                      RUN Insert_TTName IN h_prnvd ("note[row]","  ").
                      RUN Insert_TTName IN h_prnvd ("altamt[row]","            ").
                      RUN Insert_TTName IN h_prnvd ("buh[row]"," ").
                      RUN Insert_TTName IN h_prnvd ("kas[row]"," ").
                      RUN NextCircle_TTName IN h_prnvd ("row").
                    END.
                    
                    IF LAST-OF(tt-journal-rec.buh) THEN
                    DO:
                      if (mAmtAll ne 0) then 
                        formatAmt = REPLACE(STRING(mAmtAll,">>,>>>,>>>,>>>.99"), ".", "-"). 
                      else formatAmt = '0-00'.  
                      RUN Insert_TTName IN h_prnvd ("num[row]"," Итого по  ").
                      RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(GetUserName(tt-journal-rec.buh), "x(24)")).
                      RUN Insert_TTName IN h_prnvd ("code[row]","  ").
                  /*    RUN Insert_TTName IN h_prnvd ("amt[row]",/*formatAmt */ "     ").*/
                      RUN Insert_TTName IN h_prnvd ("note[row]","  ").
                      RUN Insert_TTName IN h_prnvd ("altamt[row]","            ").
                      RUN Insert_TTName IN h_prnvd ("buh[row]"," ").
                      RUN Insert_TTName IN h_prnvd ("kas[row]"," ").
                      RUN NextCircle_TTName IN h_prnvd ("row").
                    END.
              END.
           END.
           RUN EndCircle_TTName IN h_prnvd ("row").

   /* Окончательные итоги */
   
    if (vAmt ne 0) then 
      formatAmt = REPLACE(STRING(vAmt,">>,>>>,>>>,>>>.99"), ".", "-"). 
    else formatAmt = '0-00'.
    
   RUN Insert_TTName IN h_prnvd ("tamt[doc]",formatAmt).
   RUN Insert_TTName IN h_prnvd ("taltamt[doc]", "").
/*    IF tt-header.currency = "" THEN
                                                     ""
                                                 ELSE
                                                     formatAmt(vAltAmt)).
*/                                                     

   /* Подписи */

  /* mNumProc = INT64(GetSysConf("user-proc-id")) NO-ERROR.
   FIND FIRST user-proc WHERE
      RECID(user-proc) = mNumProc
   NO-LOCK NO-ERROR.

   IF (AVAIL(user-proc) AND user-proc.procedure <> ENTRY(1, PROGRAM-NAME(1), ".")) OR
      NOT AVAIL(user-proc) THEN DO:
      FIND FIRST user-proc WHERE
           user-proc.PROCEDURE = ENTRY(1, PROGRAM-NAME(1), ".")
      NO-LOCK NO-ERROR.
      IF AVAIL(user-proc) THEN
         RUN SetSysConf IN h_base ("user-proc-id",
                                    RECID(user-proc)). /* для signatur.i */
   END.

   IF AVAIL(user-proc) THEN
      mSignatures = GetXAttrValue("user-proc",
                                   STRING(user-proc.public-number),
                                  "Подписи").

   RUN PrintSignatures.  */
   
     FIND FIRST xuser
        WHERE xuser._userid EQ USERID("bisquit")
           NO-LOCK NO-ERROR.
    RUN Insert_TTName IN h_prnvd ("buh[doc]",   xuser._User-name).
  
    RUN Insert_TTName IN h_prnvd ("kas[doc]",kassFIO).
   
   RUN NextCircle_TTName IN h_prnvd ("doc").
END.
END.

RUN EndCircle_TTName IN h_prnvd ("doc").


RUN prnvd IN h_prnvd ("cjourpl").


RUN Clear_TTName IN h_prnvd.

{intrface.del}

PROCEDURE PrintSignatures.
   DEFINE VARIABLE mIn-proc         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vNSym        AS INT64 NO-UNDO.
   DEFINE VARIABLE vNumStr      AS INT64 NO-UNDO.
   DEFINE VARIABLE vSepNum      AS INT64 NO-UNDO.
   DEFINE VARIABLE vSepChar     AS CHARACTER NO-UNDO.

   &GLOBAL-DEFINE DEF-SIGN "Дир,ГлБух,МестоПечати,Исполн,Дата"

   &SCOPED-DEFINE SignaturBranchId dept.branch

   /*** Печать подписей по законодательству **************************************/
   IF AVAIL(user-proc) THEN
      mSignatures = GetXAttrValue("user-proc",
                                   STRING(user-proc.public-number),
                                   "Подписи").


   /* Если списка подписей нет, то устанавливаем стандартный список */
   IF mSignatures EQ "" THEN
      mSignatures = {&DEF-SIGN}.

   {signatur.i &in-proc = mIn-proc}
   IF {assigned mIn-proc} THEN DO:
      IF NUM-ENTRIES(mIn-proc,"│") > NUM-ENTRIES(mIn-proc,"|") THEN DO:
         vSepChar = "│".
         vSepNum = NUM-ENTRIES(mIn-proc,"│"). 
      END.
      ELSE DO:
         vSepNum = NUM-ENTRIES(mIn-proc,"|"). 
         vSepChar = "|".
      END.
      RUN BeginCircle_TTName IN h_prnvd ("sig").

      DO vI = 1 TO vSepNum:
         IF TRIM( ENTRY(vI,mIn-proc,vSepChar)) NE "" THEN DO:
            IF INDEX(mIn-proc,"~n") NE 0 THEN DO:
               vNumStr = NUM-ENTRIES(ENTRY(vI,mIn-proc,vSepChar),"~n").
               DO vNSym = 1 TO vNumStr:
                  RUN Insert_TTName IN h_prnvd ("signtxt[sig]", ENTRY(vNSym,ENTRY(vI,mIn-proc,vSepChar),"~n")).
                  RUN NextCircle_TTName IN h_prnvd ("sig").            
               END.
            END.
            ELSE DO:
               RUN Insert_TTName IN h_prnvd ("signtxt[sig]", ENTRY(vI,mIn-proc,vSepChar)).
               RUN NextCircle_TTName IN h_prnvd ("sig").            
            END.
         END.
      END.  
      RUN EndCircle_TTName IN h_prnvd ("sig").   
   END.        
END PROCEDURE. 


/* ************************  Function Implementations ***************** */


FUNCTION GetUserName RETURNS CHARACTER PRIVATE
	    (INPUT iUser AS CHARACTER  
	    ):

/*------------------------------------------------------------------------------
		Purpose: Возвращает Фамилию и инициалы пользователя в БИСКВИТЕ
        Parameters: user-id в БИСКВИТЕ
        Notes:                                                                        
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE result AS CHARACTER INIT " " NO-UNDO.
        DEFINE VARIABLE vTmpStr AS CHARACTER INIT " " NO-UNDO.
		
		DEFINE BUFFER xuser FOR _user.
		FIND FIRST xuser
		   WHERE xuser._userid EQ iUser
		   NO-LOCK NO-ERROR.
        IF AVAILABLE(xuser) THEN
        DO:
           vTmpStr = xuser._User-name.
           IF NUM-ENTRIES(vTmpStr, ".") GT 1 THEN
              vTmpStr = REPLACE(vTmpStr, ".", " ").
           DO WHILE INDEX(vTmpStr, "  ") GT 0:
              vTmpStr = REPLACE(vTmpStr, "  ", " ").              
           END.        
           result = ENTRY(1, vTmpStr, " ") + 
                    " " + (IF NUM-ENTRIES(vTmpStr, " ") GE 2 
                    THEN SUBSTRING(ENTRY(2, vTmpStr, " "), 1, 1) + "."
                    ELSE " ") +            
                    (IF NUM-ENTRIES(vTmpStr, " ") GE 3 
                    THEN SUBSTRING(ENTRY(3, vTmpStr, " "), 1, 1) + "."
                    ELSE " ")
                    .
        END.
        RETURN result.
		
END FUNCTION.
