/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: cjour.i
      Comment: Кассовые журналы 2481-У.
   Parameters:
         Uses:
      Used by:
      Created: 09.10.2010   krok
     Modified: 20.01.2013 08:30 STSS    
*/

{globals.i}
{intrface.get acct}
{intrface.get instrum}
{intrface.get prnvd}
{wordwrap.def}
{cjourpl.def}
DEFINE VARIABLE mTotalsByCS   AS LOGICAL NO-UNDO.
DEFINE VARIABLE mSetInspector AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE mKassir AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE mOperYes AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE mfistdoc-num  AS CHARACTER  NO-UNDO.
{cjour.pro}

DEFINE VARIABLE vSortOrder   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE vAmt         LIKE tt-totals-rec.amt     NO-UNDO.
DEFINE VARIABLE vAltAmt      LIKE tt-totals-rec.alt-amt NO-UNDO.
DEFINE VARIABLE vTotalsStr   AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vTotalCsStr  AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vCSType      AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vValidCSList AS CHARACTER INITIAL ""    NO-UNDO.
DEFINE VARIABLE vNumPage     AS INT64                   NO-UNDO.
DEFINE VARIABLE vAuthor      AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vTmp6        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vCurrency    AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vAcct        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE vTmpStr      AS CHARACTER               NO-UNDO.
DEFINE VARIABLE mSignatures  AS CHARACTER               NO-UNDO.
DEFINE VARIABLE mNumProc     AS INT64                   NO-UNDO.

DEFINE BUFFER b-totals-rec FOR tt-totals-rec.

IF FGetSetting("КасЖур", "ПарИтКС", "ДА") EQ "ДА" THEN
   mTotalsByCS = YES.
ELSE
   mTotalsByCS = NO.

/*                                ""Итоги по КС:""
                                AT ROW 2 COL 6
                                mTotalsByCS~
                                    LABEL """"~
                                    HELP  ""Подведение итогов в разрезе кассовых символов""~
                                VIEW-AS TOGGLE-BOX~
                                AT ROW 2 COL 21
                                SKIP~ */
                                
/* &IF DEFINED(user-inspector) > 0 &THEN */
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
/*&ELSE
   {getdate.i &DispAfterDate = "SKIP~
                                ""Итоги по КС:""
                                AT ROW 2 COL 6
                                mTotalsByCS~
                                    LABEL """"~
                                    HELP  ""Подведение итогов в разрезе кассовых символов""~
                                VIEW-AS TOGGLE-BOX~
                                AT ROW 2 COL 19"
              &UpdAfterDate  = "mTotalsByCS"}
&ENDIF
*/
{deskjourpl.i &proc_def = YES &user-id = YES}
OUTPUT {&stream} CLOSE.
mSetInspector = NO.
IF mSetInspector EQ YES THEN DO:
   RUN yesInspectorDesk.
END.
ELSE DO: 
   RUN noInspectorDesk.
END.

PROCEDURE yesInspectorDesk:
   {deskjourpl.i &user-id   = YES
               &all-currs = YES
               &i2481-tt  = YES
               &DESK_ACCT = {&desk-acct}
               &CLNT_ACCT = {&clnt-acct}
               &main_work = YES
   }
   RETURN.           
END PROCEDURE.

PROCEDURE noInspectorDesk:
   &IF DEFINED(user-inspector) > 0 &THEN
      &UNDEFINE user-inspector
   &ENDIF
   {deskjourpl.i &user-id   = YES
               &all-currs = YES
               &i2481-tt  = YES
               &DESK_ACCT = {&desk-acct}
               &CLNT_ACCT = {&clnt-acct}
               &main_work = YES
   }
   RETURN.                  
END PROCEDURE.
  

&IF DEFINED(sv-ko) &THEN DO:
   FOR EACH tt-journal-rec2 NO-LOCK 
      BREAK BY tt-journal-rec2.c-acct + substr(tt-journal-rec2.acct,1,5) + tt-journal-rec2.sprate:
      IF tt-journal-rec2.c-acct BEGINS "20202" AND tt-journal-rec2.acct BEGINS "20202" AND tt-journal-rec2.sprate NE "" THEN DO:
         ACCUMULATE tt-journal-rec2.amt (TOTAL BY (tt-journal-rec2.c-acct + substr(tt-journal-rec2.acct,1,5) + tt-journal-rec2.sprate)).
         ACCUMULATE tt-journal-rec2.alt-amt (TOTAL BY (tt-journal-rec2.c-acct + substr(tt-journal-rec2.acct,1,5) + tt-journal-rec2.sprate)).
            IF FIRST-OF(tt-journal-rec2.c-acct + substr(tt-journal-rec2.acct,1,5) + tt-journal-rec2.sprate) THEN
               mfistdoc-num = tt-journal-rec2.doc-num.

               IF LAST-OF(tt-journal-rec2.c-acct + substr(tt-journal-rec2.acct,1,5) + tt-journal-rec2.sprate) THEN DO:
                  CREATE tt-journal-rec.
                  ASSIGN
                     tt-journal-rec.c-acct   = tt-journal-rec2.c-acct
                     tt-journal-rec.doc-num  = mfistdoc-num
                     tt-journal-rec.acct     = tt-journal-rec2.acct
                     tt-journal-rec.doc-code = tt-journal-rec2.doc-code
                     tt-journal-rec.amt      = ACCUM TOTAL BY (tt-journal-rec2.c-acct + substr(tt-journal-rec2.acct,1,5) + tt-journal-rec2.sprate) tt-journal-rec2.amt 
                     tt-journal-rec.symbol   = tt-journal-rec2.symbol
                     tt-journal-rec.op       = tt-journal-rec2.op
                     tt-journal-rec.note     = tt-journal-rec2.note
                     tt-journal-rec.alt-amt  = ACCUM TOTAL BY (tt-journal-rec2.c-acct + substr(tt-journal-rec2.acct,1,5) + tt-journal-rec2.sprate) tt-journal-rec2.alt-amt
                     .
                     
               
               END.
      END.
      ELSE DO:
         CREATE tt-journal-rec.
            ASSIGN
               tt-journal-rec.c-acct   = tt-journal-rec2.c-acct
               tt-journal-rec.doc-num  = tt-journal-rec2.doc-num
               tt-journal-rec.acct     = tt-journal-rec2.acct
               tt-journal-rec.doc-code = tt-journal-rec2.doc-code
               tt-journal-rec.amt      = tt-journal-rec2.amt
               tt-journal-rec.symbol   = tt-journal-rec2.symbol
               tt-journal-rec.op       = tt-journal-rec2.op
               tt-journal-rec.note     = tt-journal-rec2.note
               tt-journal-rec.alt-amt  = tt-journal-rec2.alt-amt
               .
      END.
   END.
END.
&ENDIF

IF NOT CAN-FIND(FIRST tt-journal-rec) THEN DO:
   &IF DEFINED(by-acct) &THEN
      MESSAGE "На " + STRING(end-date) + " по выбраным Вами счетам нет данных для отчета" VIEW-AS ALERT-BOX INFO BUTTONS OK.
   &ELSE
      IF entries > 1 THEN
         MESSAGE "На " + STRING(end-date) + " по выбраным Вами пользователям нет данных для отчета" VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ELSE
         MESSAGE "На " + STRING(end-date) + " по " + list-id + " нет данных для отчета" VIEW-AS ALERT-BOX INFO BUTTONS OK.
   &ENDIF
   RETURN.
END.

RUN BeginCircle_TTName IN h_prnvd ("doc").
FOR EACH tt-header 
BY SUBSTRING(STRING(tt-header.acct),1,8)
BY SUBSTRING(STRING(tt-header.acct),14):  
    tt-header.type = {&journal-type}.
    IF {assigned mOKUD} THEN
        tt-header.form-code = mOKUD.
    ELSE
        ASSIGN
            tt-header.form-code = "0401704" WHEN {&journal-type} = "приход"
            tt-header.form-code = "0401705" WHEN {&journal-type} = "расход"
        .

   FIND FIRST tt-journal-rec WHERE tt-journal-rec.c-acct = tt-header.acct.
   RELEASE op-entry.

   RUN ParsSet(BUFFER op-entry,tt-journal-rec.op,tt-journal-rec.c-acct).
   vCSType = getCSType(tt-header.type).
   FOR EACH tmp-code
   WHERE
       tmp-code.class    =  "КасСимволы" AND
       tmp-code.beg-date <= tt-header.date    AND
       (tmp-code.end-date = ?       OR
        tmp-code.end-date > tt-header.date)   AND
       (NOT {assigned tmp-code.val} OR
        tmp-code.val = vCSType)
   NO-LOCK:
       vValidCSList = concat(vValidCSList, tmp-code.code).
   END.
   
   IF mTotalsByCS THEN DO:
       RUN CalcTotals(tt-header.acct).
       RUN CalcOverall(tt-header.acct, OUTPUT vAmt, OUTPUT vAltAmt).        
   END.
   ELSE
       RUN CalcOverall(tt-header.acct, OUTPUT vAmt, OUTPUT vAltAmt).

       mOperYes = YES.
   {find-act.i &acct = tt-header.acct
               &curr = tt-header.currency}
   IF NOT AVAIL acct THEN
       RETURN.
   ELSE
   DO:
       IF NOT mKassir THEN
          FOR EACH tt-journal-rec
            WHERE  
            tt-journal-rec.c-acct = tt-header.acct 
            AND CAN-DO("!20202*,*", tt-journal-rec.acct):
                LEAVE.
          END. 
      IF NOT AVAILABLE(tt-journal-rec) THEN
        mOperYes = NO.
   END.
   IF mOperYes THEN
   DO:
   vAuthor = tt-header.author.

   IF mVarVal[6] NE "?" AND mVarVal[6] NE "" THEN
      vTmp6 = mVarVal[6].
   ELSE 
      IF tt-header.currency <> "" THEN
         vTmp6 = " Код валюты: " + GetISOCode(tt-header.currency).

   /* шапка отчета */
   RUN Insert_TTName IN h_prnvd ("code[doc]",tt-header.form-code).
   RUN Insert_TTName IN h_prnvd ("author[doc]",vAuthor).
   RUN Insert_TTName IN h_prnvd ("jtype[doc]",getJournalTypeStr(tt-header.type)).
   RUN Insert_TTName IN h_prnvd ("ttype[doc]",STRING(tt-header.type + "а","x(9)")).
   RUN Insert_TTName IN h_prnvd ("date[doc]",(IF mDatePar EQ "НЕТ" THEN
                                                    {term2str tt-header.date tt-header.date yes}  
                                                 ELSE

                                                    "   " + STRING(tt-header.date, "99.99.9999"))).
   RUN Insert_TTName IN h_prnvd ("det[doc]"," Счет по учету кассы № " + STRING(acct.acct, GetAcctFmt(acct.acct-cat))).
   RUN Insert_TTName IN h_prnvd ("pos6[doc]",vTmp6).
    
   vSortOrder   = FGetSetting("СортКасСумм", "", "Да") EQ "Да".
   /* строки отчета */
   RUN BeginCircle_TTName IN h_prnvd ("row").  
   IF mKassir THEN DO:
      FOR EACH tt-journal-rec 
        WHERE tt-journal-rec.c-acct = tt-header.acct
        BY tt-journal-rec.acct BY tt-journal-rec.kassir BY tt-journal-rec.doc-num:
         vCurrency = SUBSTRING(tt-journal-rec.acct, 6, 3).
         IF tt-header.currency EQ FGetSetting("КодНацВал", ?, "810") THEN
            vCurrency = "".
             {find-act.i &acct = tt-journal-rec.acct
                         &curr = vCurrency}
             IF AVAILABLE acct THEN
                vAcct = STRING(tt-journal-rec.acct, GetAcctFmt(acct.acct-cat)).
                RUN Insert_TTName IN h_prnvd ("num[row]",STRING(tt-journal-rec.doc-num , "x(11)")).
                RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(vAcct, "x(24)")).
                RUN Insert_TTName IN h_prnvd ("code[row]",STRING(tt-journal-rec.doc-code, "xx")).
                RUN Insert_TTName IN h_prnvd ("amt[row]",formatAmt(tt-journal-rec.amt)).
                RUN Insert_TTName IN h_prnvd ("note[row]",STRING(IF CAN-DO(vValidCSList, tt-journal-rec.symbol) THEN tt-journal-rec.symbol ELSE "", "xx")).
                RUN Insert_TTName IN h_prnvd ("altamt[row]",(IF tt-journal-rec.op-status GE CHR(251) THEN "подтверждено" ELSE "не подтверждено")).
                RUN Insert_TTName IN h_prnvd ("buh[row]",tt-journal-rec.buh).
                RUN Insert_TTName IN h_prnvd ("kas[row]",tt-journal-rec.kassir).
                RUN NextCircle_TTName IN h_prnvd ("row").
                 
      END.
   END.
   ELSE DO:
      FOR EACH tt-journal-rec 
      WHERE tt-journal-rec.c-acct = tt-header.acct 
      AND CAN-DO("!20202*,*", tt-journal-rec.acct)
      BY tt-journal-rec.acct BY tt-journal-rec.buh BY tt-journal-rec.doc-num:
         vCurrency = SUBSTRING(tt-journal-rec.acct, 6, 3).
         IF tt-header.currency EQ FGetSetting("КодНацВал", ?, "810") THEN
            vCurrency = "".
             {find-act.i &acct = tt-journal-rec.acct
                         &curr = vCurrency}
             IF AVAILABLE acct THEN
                vAcct = STRING(tt-journal-rec.acct, GetAcctFmt(acct.acct-cat)).
            RUN Insert_TTName IN h_prnvd ("num[row]",STRING(tt-journal-rec.doc-num , "x(11)")).
            RUN Insert_TTName IN h_prnvd ("acct[row]",STRING(vAcct, "x(24)")).
            RUN Insert_TTName IN h_prnvd ("code[row]",STRING(tt-journal-rec.doc-code, "xx")).
            RUN Insert_TTName IN h_prnvd ("amt[row]",formatAmt(tt-journal-rec.amt)).
            RUN Insert_TTName IN h_prnvd ("note[row]",STRING(IF CAN-DO(vValidCSList, tt-journal-rec.symbol) THEN tt-journal-rec.symbol ELSE "", "xx")).
            RUN Insert_TTName IN h_prnvd ("altamt[row]",(IF tt-journal-rec.op-status GE CHR(251) THEN "подтверждено" ELSE "не подтверждено")).
            RUN Insert_TTName IN h_prnvd ("buh[row]",tt-journal-rec.buh).
            RUN Insert_TTName IN h_prnvd ("kas[row]",tt-journal-rec.kassir).
            RUN NextCircle_TTName IN h_prnvd ("row").
            
            
      END.
   END.
   RUN EndCircle_TTName IN h_prnvd ("row").

   /* Итоги по КС */
   IF mTotalsByCS THEN DO:
      vTmpStr = "Итого по КС :".
      RUN BeginCircle_TTName IN h_prnvd ("cs").  
      FOR EACH tt-totals-rec 
      WHERE
         tt-totals-rec.c-acct = tt-header.acct
      NO-LOCK:
         FIND LAST b-totals-rec WHERE b-totals-rec.c-acct = tt-header.acct
         NO-LOCK NO-ERROR.
         RUN Insert_TTName IN h_prnvd ("str[cs]",vTmpStr).
         vTmpStr = "".
         RUN Insert_TTName IN h_prnvd ("amtcs[cs]",formatAmt(tt-totals-rec.amt)).
         RUN Insert_TTName IN h_prnvd ("sym[cs]",STRING(IF CAN-DO(vValidCSList,tt-totals-rec.symbol) THEN STRING(tt-totals-rec.symbol,"xx") ELSE "")).
         RUN Insert_TTName IN h_prnvd ("altamt[cs]",(IF tt-header.currency EQ "" THEN
                                                        ""
                                                     ELSE
                                                        formatAmt(tt-totals-rec.alt-amt))).

         IF RECID(tt-totals-rec) EQ RECID(b-totals-rec) THEN 
         DO:
            RUN Insert_TTName IN h_prnvd ("endLine[cs]", "1").
            RUN Insert_TTName IN h_prnvd ("rowLine[cs]", "").
         END.
         ELSE
         DO:
            RUN Insert_TTName IN h_prnvd ("endLine[cs]", "").
            RUN Insert_TTName IN h_prnvd ("rowLine[cs]", "1").
         END.
         RUN NextCircle_TTName IN h_prnvd ("cs").
      END.
      RUN EndCircle_TTName IN h_prnvd ("cs").                      
   END.
   /* Окончательные итоги */
   RUN Insert_TTName IN h_prnvd ("tamt[doc]",formatAmt(vAmt)).
   RUN Insert_TTName IN h_prnvd ("taltamt[doc]", IF tt-header.currency = "" THEN
                                                     ""
                                                 ELSE
                                                     formatAmt(vAltAmt)).

   /* Подписи */

   mNumProc = INT64(GetSysConf("user-proc-id")) NO-ERROR.
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

   RUN PrintSignatures.  
   END.
   RUN NextCircle_TTName IN h_prnvd ("doc").
END.
RUN EndCircle_TTName IN h_prnvd ("doc").

/* IF NOT mTotalsByCS THEN
DO:
  IF FGetSettingEx("КасЖур","КасЖурНумСт","",NO) = "ДА"
     THEN RUN prnvd IN h_prnvd ("cjour").
     ELSE RUN prnvd IN h_prnvd ("cjour2").
END.
ELSE
DO:
  IF FGetSettingEx("КасЖур","КасЖурНумСт","",NO) = "ДА" 
     THEN RUN prnvd IN h_prnvd ("cjourCS").
     ELSE RUN prnvd IN h_prnvd ("cjour2CS").
END.
*/
RUN prnvd IN h_prnvd ("cjourpl").

RUN Clear_TTName IN h_prnvd.


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

{intrface.del}
