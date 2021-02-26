{getdate.i}

FUNCTION GetDateString RETURN CHARACTER PRIVATE(INPUT iDate AS DATE):
    IF iDate = ? THEN
        RETURN ?.
    RETURN ("``" + STRING(DAY(iDate)) + "\'\' " + ENTRY(MONTH(iDate), "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря") + " " + STRING(YEAR(iDate)) + " г.").
END FUNCTION.

DEFINE VARIABLE bDocRub   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bDocDrg   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bDocKas   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE nDigit    AS INT64   NO-UNDO.
DEFINE VARIABLE mReRate   AS CHARACTER NO-UNDO.
DEFINE VARIABLE adb       AS CHARACTER NO-UNDO.
DEFINE VARIABLE acr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE isCash    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mIsDetail AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mItem     AS INT64   NO-UNDO.
DEFINE VARIABLE vTitleSet AS CHARACTER NO-UNDO.
DEFINE VARIABLE vI        AS INT64   NO-UNDO.
DEFINE VARIABLE vBal   AS DECIMAL NO-UNDO.
DEFINE VARIABLE vNBal  AS DECIMAL NO-UNDO.
DEFINE VARIABLE mYes   AS LOGICAL   NO-UNDO.


DEFINE VARIABLE nDocAmt   LIKE op-entry.amt-rub NO-UNDO.

DEFINE BUFFER   xop-entry FOR op-entry.
DEFINE BUFFER   TytleCode FOR code.

DEFINE TEMP-TABLE ttDetail
    FIELD fItem    AS INT64
    FIELD fDocNum  AS CHARACTER
    FIELD fAcctDb  AS CHARACTER
    FIELD fAcctCr  AS CHARACTER
    FIELD fAmt     AS DECIMAL
    FIELD fAcctCat AS CHARACTER 
    FIELD fDocKas  AS LOGICAL
    FIELD fDocRub  AS LOGICAL
    FIELD fDocDrg  AS LOGICAL
    INDEX fItem fItem
.

{op-cash.def}
mReRate    = FGetSetting("БухЖур(b)", "ПерСчет",   "*empty*").

mIsDetail = NO.
MESSAGE "Выводить расшифровку к титульному листу?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE mIsDetail. 
mItem = 0.
vBal  = 0.0.
vNBal = 0.0.
vTitleSet = FGetSetting("Отчеты","ТитулДокДня",?).
FOR EACH op WHERE op.op-date    = end-date
              AND op.op-status >= gop-status
              AND (IF shMode THEN op.filial-id = shFilial ELSE YES) NO-LOCK
    BREAK BY op.branch-id:

    /*for each op-entry of op no-lock:
        IF op-entry.currency NE "" THEN bDocRub =NO.
    end.*/
    
    for each op-entry of op:
       bDocRub = YES.

       IF op-entry.currency NE "" THEN bDocRub =NO.

       ASSIGN
          adb = ?
          acr = ?
       .

       IF op-entry.acct-cr EQ ? THEN 
       DO:
          FIND FIRST xop-entry WHERE xop-entry.op = op.op 
                                 AND xop-entry.acct-db EQ ? 
             USE-INDEX op-entry NO-LOCK NO-ERROR.
          ASSIGN
            adb = op-entry.acct-db
            acr = IF AVAILABLE xop-entry THEN xop-entry.acct-cr ELSE "**empty**"
          .
       END.
       ELSE IF op-entry.acct-db EQ ? THEN 
       DO:
          FIND FIRST xop-entry WHERE xop-entry.op = op.op 
                                 AND xop-entry.acct-cr EQ ? 
             USE-INDEX op-entry NO-LOCK  NO-ERROR.
          ASSIGN       
             adb = IF AVAILABLE xop-entry THEN xop-entry.acct-db ELSE "**empty**"
             acr = op-entry.acct-cr
          .
       END.
       ELSE 
       DO:
          IF vTitleSet EQ "ТитулДокДня" AND 
             CAN-FIND(FIRST TytleCode WHERE Tytlecode.class EQ "ТитулДокДня")
          THEN DO:
    	  mYes = TRUE.	
             FOR EACH TytleCode WHERE TytleCode.class EQ "ТитулДокДня" NO-LOCK:
                IF CAN-DO(TytleCode.code,op-entry.acct-db) 
THEN DO:
		if CAN-DO(TytleCode.val,op-entry.acct-cr) and mYes then
		DO:
                         IF op-entry.acct-db begins "9" OR op-entry.acct-cr begins "9" THEN
                            vNBal = vNBal + op-entry.amt-rub.
                         ELSE
                            vBal = vBal + op-entry.amt-rub.                     
    	  	mYes = FALSE.	
                END.
                END.
             END.                                                                  
          END.    
          ASSIGN
             adb = op-entry.acct-db
             acr = op-entry.acct-cr
          .
       END.
       {op-cash.i}

        assign
  
            bDocKas     = isCash.
            bDocDrg     = false

        .

        IF bDocRub THEN
           bDocRub = NOT (CAN-FIND( FIRST acct WHERE acct.acct = op-entry.acct-cr
                                                 AND CAN-DO(mReRate, acct.acct))
                          OR 
                          CAN-FIND( FIRST acct WHERE acct.acct = op-entry.acct-db
                                                 AND CAN-DO(mReRate, acct.acct))
           ).


        IF bDocRub AND op-entry.acct-db = ? THEN
            bDocRub = NOT CAN-FIND(FIRST op-entry OF op WHERE op-entry.currency <> "" 
                                                          AND op-entry.acct-cr = ?
                                  ).
        ELSE IF bDocRub AND op-entry.acct-cr = ? THEN
            bDocRub = NOT CAN-FIND(FIRST op-entry OF op WHERE op-entry.currency <> "" 
                                                          AND op-entry.acct-db = ?
                                  ).

        if( (not bDocRub) and (not bDocDrg) ) then do:

            nDigit = -1.

            assign

                nDigit = INT64( substr( op-entry.currency, 1, 1 ) )

            no-error.

            bDocDrg = (nDigit < 0).

        end.

        assign

            nDocAmt = 0
            nDocAmt = op-entry.amt-rub

            when op-entry.acct-db <> ?

        .
        mItem = mItem + 1.

        IF mIsDetail AND op-entry.acct-db NE ? THEN
        DO:
           CREATE ttDetail.
           ASSIGN
              ttDetail.fItem    = mItem
              ttDetail.fDocNum  = op.doc-num
              ttDetail.fAcctDb  = adb
              ttDetail.fAcctCr  = acr
              ttDetail.fAmt     = nDocAmt
              ttDetail.fAcctCat = op.acct-cat
              ttDetail.fDocKas  = bDocKas
              ttDetail.fDocRub  = bDocRub
              ttDetail.fDocDrg  = bDocDrg
           .
        END.

        assign

            tot-b  = tot-b  + nDocAmt       /* Баланс все документы */

                when (op.acct-cat = "b")

            k-b = k-b + nDocAmt             /* Баланс все кассовые */

                when (op.acct-cat = "b")

                 /*and (bDocRub)*/ and (bDocKas)

            val-b = val-b + nDocAmt         /* Баланс валюта мемориальные */
    
                when (op.acct-cat = "b")
    
                 and (not bDocRub) and (not bDocDrg) and (not bDocKas)
    
            kinv-b = kinv-b + nDocAmt       /* Баланс валюта кассовые */
    
                when (op.acct-cat = "b")

             and (not bDocRub) and (not bDocDrg) and (bDocKas)

            drag-b = drag-b + nDocAmt       /* Баланс драг.мет. мемориальные */
    
                when (op.acct-cat = "b")
    
                 and (not bDocRub) and (bDocDrg) and (not bDocKas)
    
            k-drag-b = k-drag-b + nDocAmt   /* Баланс драг.мет. кассовые */
    
                when (op.acct-cat = "b")

             and (not bDocRub) and (bDocDrg) and (bDocKas)

            tot-o  = tot-o  + nDocAmt       /* Внебаланс все документы */
    
                when (op.acct-cat = "o")
    
            k-o = k-o + nDocAmt             /* Внебаланс все кассовые */
    
                when (op.acct-cat = "o")
    
                 /*and (bDocRub)*/ and (bDocKas)

            val-o = val-o + nDocAmt         /* Внебаланс валюта мемориальные */
    
                when (op.acct-cat = "o")
    
                 and (not bDocRub) and (not bDocDrg) and (not bDocKas)
    
            kinv-o = kinv-o + nDocAmt       /* Внебаланс валюта кассовые */
    
                when (op.acct-cat = "o")

                 and (not bDocRub) and (not bDocDrg) and (bDocKas)
    
            drag-o = drag-o + nDocAmt       /* Внебаланс драг.мет. мемориальные */
    
                when (op.acct-cat = "o")
    
                 and (not bDocRub) and (bDocDrg) and (not bDocKas)
    
            k-drag-o = k-drag-o + nDocAmt   /* Внебаланс драг.мет. кассовые */

                when (op.acct-cat = "o")
    
                 and (not bDocRub) and (bDocDrg) and (bDocKas)
    
        .
    
    end.
    
end.
