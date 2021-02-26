/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: U:\GUVA\TITUL\TITUL_OMS.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 21.08.2012 14:17 gva     
     Modified: 21.08.2012 16:55 gva      
     Modified: 21.08.2012 18:15 gva      
     Modified: 22.08.2012 10:41 gva      
     Modified: 22.08.2012 10:09 gva      
     Modified: 22.08.2012 10:10 gva      
     Modified: 22.08.2012 10:12 gva      
     Modified: 22.08.2012 10:23 gva      
     Modified: 22.08.2012 10:40 gva      
     Modified: 22.08.2012 18:26 gva      
     Modified: 23.08.2012 12:43 gva      
     Modified: 24.08.2012 14:18 gva      
     Modified: 24.08.2012 14:23 gva      
     Modified: 24.08.2012 14:23 gva      
     Modified: 24.08.2012 14:24 gva      
     Modified: 24.08.2012 14:25 gva      
     Modified: 24.08.2012 15:36 gva      
     Modified: 24.08.2012 14:57 gva      
     Modified: 24.08.2012 15:00 gva      
     Modified: 24.08.2012 15:02 gva      
     Modified: 24.08.2012 15:07 gva      
     Modified: 24.08.2012 15:34 gva      
     Modified: 27.08.2012 10:21 gva      
     Modified: 27.08.2012 10:32 gva      
     Modified: 27.08.2012 10:37 gva      
     Modified: 27.08.2012 10:38 gva      
     Modified: 27.08.2012 10:42 gva      
     Modified: 27.08.2012 10:46 gva      
     Modified: 27.08.2012 10:49 gva      
     Modified: 27.08.2012 14:31 gva      
     Modified: 27.08.2012 16:52 gva      
     Modified: 27.08.2012 14:34 gva      
     Modified: 27.08.2012 14:35 gva      
     Modified: 27.08.2012 17:01 gva      
     Modified: 27.08.2012 14:38 gva      
     Modified: 27.08.2012 14:39 gva      
     Modified: 27.08.2012 14:42 gva      
     Modified: 27.08.2012 14:43 gva      
     Modified: 27.08.2012 14:44 gva      
     Modified: 27.08.2012 15:40 gva      
     Modified: 27.08.2012 15:53 gva      
     Modified: 27.08.2012 17:06 gva      
     Modified: 27.08.2012 15:58 gva      
     Modified: 27.08.2012 17:27 gva      
     Modified: 27.08.2012 16:20 gva      
     Modified: 27.08.2012 16:49 gva      
     Modified: 27.08.2012 17:09 gva      
     Modified: 27.08.2012 17:25 gva      
     Modified: 28.08.2012 11:09 gva      
     Modified: 28.08.2012 12:20 gva      
     Modified: 28.08.2012 12:21 gva      
     Modified: 28.08.2012 12:22 gva      
     Modified: 28.08.2012 12:23 gva      
     Modified: 28.08.2012 16:19 gva      
     Modified: 28.08.2012 17:26 gva      
     Modified: 28.08.2012 15:19 gva      
     Modified: 28.08.2012 15:29 gva      
     Modified: 28.08.2012 15:30 gva      
     Modified: 28.08.2012 15:31 gva      
     Modified: 28.08.2012 15:31 gva      
     Modified: 28.08.2012 15:37 gva      
     Modified: 28.08.2012 16:18 gva      
     Modified: 28.08.2012 17:32 gva      
     Modified: 28.08.2012 17:34 gva      
     Modified: 28.08.2012 17:40 gva      
     Modified: 28.08.2012 17:50 gva      
     Modified: 30.08.2012 14:35 gva      
     Modified: 30.08.2012 15:06 gva      
     Modified: 30.08.2012 15:09 gva      
     Modified: 30.08.2012 15:16 gva      
     Modified: 06.09.2012 10:23 gva      
     Modified: 06.09.2012 11:43 gva      
     Modified: 07.09.2012 14:11 gva      
     Modified: 07.09.2012 17:28 gva      
     Modified: 07.09.2012 19:12 gva      
     Modified: 07.09.2012 19:12 gva      
     Modified: 07.09.2012 17:43 gva      
     Modified: 07.09.2012 19:16 gva      
     Modified: 10.09.2012 14:19 gva      
     Modified: 10.09.2012 17:13 gva      
     Modified: 10.09.2012 13:25 gva      
     Modified: 10.09.2012 14:05 gva      
     Modified: 10.09.2012 14:20 gva      
     Modified: 10.09.2012 14:20 gva      
     Modified: 10.09.2012 14:22 gva      
     Modified: 10.09.2012 14:23 gva      
     Modified: 10.09.2012 17:15 gva      
     Modified: 10.09.2012 17:17 gva      
     Modified: 10.09.2012 17:21 gva      
     Modified: 10.09.2012 17:21 gva      
     Modified: 11.09.2012 09:49 gva      
     Modified: 11.09.2012 09:50 gva      
     Modified: 11.09.2012 09:53 gva      
     Modified: 24.01.2014 kau	Разделение документов проводимых сотрудниками головного офиса.
     Modified: 20.03.2014 kau	Переделал для EXCEL
     Modified: 29.04.2014 kau	По служебке ОБУ.335 сделал исключение мемориальных документов на сумму курсовой разницы
     Modified: 07.05.2014 kau	По служебке ОБУ.335 убрал исключение и сделал относ таких документов в электронные
     Modified: 10.03.2015 ayv   По СЗ УУАиПККО.080 документы транзакций ВыдСК2 и ВыдОпл2 вынесены в электронные
	comment: Транзакция использует класификаторый PaperAcct, TitulBr, TitTranz
		Настроечные параметры БухЖур(b), БухЖур(o), elec-type, user-gb

*/

Define Input Param ipCity as CHARACTER no-undo.
{globals.i}
DEFINE VARIABLE mTitleSet AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBranch AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vBranch AS CHARACTER  NO-UNDO.
DEFINE VARIABLE isCash  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE adb       AS CHARACTER NO-UNDO.
DEFINE VARIABLE acr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mYes AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mSumm LIKE op-entry.amt-rub  NO-UNDO.
DEFINE VARIABLE mElecSave AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUser AS CHARACTER  NO-UNDO.


DEFINE VARIABLE mBalans LIKE op-entry.amt-rub INIT 0.00 NO-UNDO.
DEFINE VARIABLE mVBalans LIKE op-entry.amt-rub INIT 0.00 NO-UNDO.
Define Variable Bank-name        As Character            No-Undo.
Define Variable vDateString      As Character            No-Undo.
DEF VAR kRow	AS DEC	NO-UNDO.
DEF VAR kRS	AS DEC	NO-UNDO.


DEFINE BUFFER xop-entry FOR op-entry.
DEFINE BUFFER xop FOR op.
DEFINE BUFFER   TytleCode FOR code.


{titul.def}

 {getdate.i}

FIND FIRST CODE 
    WHERE 
    CODE.class EQ "TitulBr"
    AND code.PARENT EQ "TitulBr"
    AND CODE.CODE EQ ipCity
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE(CODE) THEN
    MESSAGE "No param otc"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE
    mBranch = CODE.val.
FIND FIRST branch WHERE branch.branch-id EQ mBranch
     NO-LOCK NO-ERROR.
IF AVAILABLE(branch) THEN
DO:
    vBranch = mBranch.
    FOR EACH branch NO-LOCK
        WHERE branch.parent-id EQ vBranch
        BY Branch.branch-id:
        mBranch = mBranch + "," + Branch.branch-id.
    END.
END.
        mBranch = mBranch + ",ЦОУ".
    

{op-cash.def}

FOR EACH op NO-LOCK
    WHERE
    op.op-date EQ end-date
    AND op.op-status BEGINS "√"
    AND CAN-DO(mBranch, op.branch-id)
    AND op.filial-id EQ '0400':
    FOR EACH op-entry NO-LOCK
        WHERE op-entry.op EQ op.op
        AND op-entry.op-date EQ end-date
        :
/*
/*kau 29.04.2014*/
	IF op-entry.op-date >= DATE( 4, 1, 2014) AND op-entry.currency NE '' AND op-entry.amt-cur = 0
	THEN NEXT.
*/

        ASSIGN
        adb = op-entry.acct-db
        acr = op-entry.acct-cr.
        IF op.branch-id EQ "ЦОУ" THEN 
            mBr = SUBSTRING(adb, 10, 4) .
        ELSE 
            mBr = op.branch-id.
        
        FIND FIRST CODE 
            WHERE 
            CODE.class EQ "TitulBr"
            AND code.PARENT EQ "TitulBr"
            AND CAN-DO(CODE.val, mBr)
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(CODE) THEN 
            MESSAGE "no"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        {op-cash.i}
/*kau*/	
	cUser = FGetSetting("user-gb","","").
	IF CAN-DO (cUser,op.user-id) OR op.user-id BEGINS 'SERV' THEN 
	DO:
        CREATE tt-op-day.
        ASSIGN
            tt-op-day.op-cid = RECID(op)
            tt-op-day.ope-cid = RECID(op-entry)
            tt-op-day.city-id = 99
            tt-op-day.city = "Документы проведенные сотрудниками головного офиса на балансе филиала"
            tt-op-day.kko = "0000"
            tt-op-day.currency = IF op-entry.currency NE "" AND op-entry.currency NE ? THEN "VAL" ELSE "810"
            tt-op-day.acct-cat = op.acct-cat
            tt-op-day.razdel = "b"
            tt-op-day.acct-db = op-entry.acct-db
            tt-op-day.acct-cr = op-entry.acct-cr
            tt-op-day.doc-num = op.doc-num
            tt-op-day.amt-rub = op-entry.amt-rub
            tt-op-day.save-type = "p".
            .
	END.
	ELSE
	DO:
/*end kau*/
	        CREATE tt-op-day.
        	ASSIGN
	            tt-op-day.op-cid = RECID(op)
        	    tt-op-day.ope-cid = RECID(op-entry)
	            tt-op-day.city-id = INTEGER(TRIM(CODE.CODE))
        	    tt-op-day.city = CODE.NAME
	            tt-op-day.kko = mBr
        	    tt-op-day.currency = IF op-entry.currency NE "" AND op-entry.currency NE ? THEN "VAL" ELSE "810"
	            tt-op-day.acct-cat = op.acct-cat
        	    tt-op-day.razdel = "b"
	            tt-op-day.acct-db = op-entry.acct-db
        	    tt-op-day.acct-cr = op-entry.acct-cr
	            tt-op-day.doc-num = op.doc-num
        	    tt-op-day.amt-rub = op-entry.amt-rub
	            tt-op-day.save-type = "p".
        	    .
	END.
        IF isCash THEN
            tt-op-day.razdel = "k".
        mElecSave = FGetSetting("elec-type", "",   "01КЛ,01МБ,017,09*,06*,016,ВБО").
       IF CAN-DO(mElecSave, op.doc-type) THEN
           ASSIGN
           tt-op-day.save-type = "e".

        mYes = TRUE.	

/*kau 07.05.2014*/
	IF op-entry.op-date >= DATE( 4, 1, 2014) AND op-entry.currency NE '' AND op-entry.amt-cur = 0
	THEN DO:
		tt-op-day.save-type = "e".
		mYes = FALSE.
	END.

/*ayv 10.03.2015*/
   IF CAN-DO('ВыдСК2,ВыдОпл2',op.op-kind) AND op.doc-date GE 01/01/15 THEN
       ASSIGN
          tt-op-day.save-type = 'e'
          mYes = FALSE. 
	
/*ayv 11.08.2015*/
   IF op-entry.acct-db BEGINS '47422' AND op-entry.acct-cr BEGINS '40702810404000001704' AND op.doc-date GE 01/01/15 THEN
       ASSIGN
          tt-op-day.save-type = 'e'
          mYes = FALSE. 

   IF op.user-id EQ 'K0400STA' AND op.doc-date GE 07/01/14 AND op.doc-date LE 01/01/15 THEN
       ASSIGN
          tt-op-day.save-type = 'e'
          mYes = FALSE.

   IF CAN-DO("Курс,routMF_all",op.op-kind) AND op.op-date GE 01/01/2015 THEN
       ASSIGN
          tt-op-day.save-type = 'e'
          mYes = FALSE.

/*ayv 17.08.2015*/
   IF op-entry.acct-db BEGINS '30302' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF op-entry.acct-db BEGINS '30302' AND op-entry.acct-cr BEGINS '20202' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'p'
           mYes = FALSE.

   IF op-entry.acct-db BEGINS '47422' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF (op-entry.acct-db BEGINS '40820' OR op-entry.acct-db BEGINS '40817') AND GetXAttrValueEx('op',STRING(op.op),'способполуч','') EQ 'Электронный' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF (op-entry.acct-db BEGINS '40820' OR op-entry.acct-db BEGINS '40817') AND op-entry.acct-db BEGINS '60308' AND op.op-kind EQ '050302+' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'p'
           mYes = FALSE.

   IF (op-entry.acct-db BEGINS '30221' 
   OR op-entry.acct-db BEGINS '30223' 
   OR op-entry.acct-db BEGINS '30102') AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF (op-entry.acct-db BEGINS '47416' 
   OR op-entry.acct-cr BEGINS '47416') AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF op-entry.acct-db BEGINS '47427' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF op.op-kind EQ 'i-ed114' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF op-entry.acct-db BEGINS '40817' AND op-entry.acct-cr BEGINS '47422' AND op.doc-type EQ '01' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF op.user-id EQ 'BUH_STA' AND op.op-kind EQ '04013' AND op.doc-num EQ '741295' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF op-entry.acct-db BEGINS '40702' AND op.op-kind BEGINS 'klb' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF op-entry.acct-db BEGINS '42301' AND CAN-DO('014403,012sz*,028cl*',op.op-kind) AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
           tt-op-day.save-type = 'e'
           mYes = FALSE.

   IF (op.user-id EQ 'BIS' OR op.user-id EQ 'IRBIS') AND op.doc-date EQ 04/30/2015 THEN
        ASSIGN
            tt-op-day.save-type = 'e'
            mYes = FALSE.

   IF op-entry.acct-db BEGINS '61301' AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
            tt-op-day.save-type = 'e'
            mYes = FALSE.

   IF (op.op-kind EQ '0150004_2' OR op.op-kind EQ '0150004_21' OR op.op-kind EQ '014401') AND op.doc-date GE 01/01/2015 THEN
        ASSIGN
            tt-op-day.save-type = 'e'
            mYes = FALSE.

	FIND FIRST TytleCode WHERE TytleCode.class EQ "TitTranz" 
			and TytleCode.code EQ op.op-kind NO-LOCK NO-ERROR.
	IF AVAIL tytlecode THEN DO:
	tt-op-day.save-type = TytleCode.val.
	END.
	
           FOR EACH TytleCode WHERE TytleCode.class EQ "PaperAcct" NO-LOCK:
              IF CAN-DO(TytleCode.code,op-entry.acct-db) AND mYes THEN 
              DO:
                  IF CAN-DO(TytleCode.val,op-entry.acct-cr) AND mYes THEN
                  DO:
                      ASSIGN
                      tt-op-day.save-type = "p".
                      mYes = FALSE.   
                  END.
              END.
           END.                                                                  

    END.
END.



FOR EACH tt-op-day NO-LOCK
    BREAK BY tt-op-day.city-id BY tt-op-day.city
    BY tt-op-day.kko
    BY tt-op-day.acct-cat
    BY tt-op-day.save-type 
    BY tt-op-day.currency
    BY tt-op-day.razdel
    BY tt-op-day.acct-db
    BY tt-op-day.acct-cr
    :
    IF FIRST-OF(tt-op-day.kko) THEN
    DO:
    CREATE tt-kko.
    ASSIGN 
        tt-kko.city-id =  tt-op-day.city-id
        tt-kko.city =  tt-op-day.city
        tt-kko.kko =  tt-op-day.kko.
    END.



END.



mSumm = 0.
FOR EACH tt-op-day NO-LOCK
    BREAK BY TRIM(STRING(tt-op-day.city-id)) + tt-op-day.city + tt-op-day.kko + tt-op-day.acct-cat + tt-op-day.save-type + tt-op-day.currency + tt-op-day.razdel
    :
IF FIRST-OF(TRIM(STRING(tt-op-day.city-id)) + tt-op-day.city + tt-op-day.kko + tt-op-day.acct-cat + tt-op-day.save-type + tt-op-day.currency + tt-op-day.razdel) THEN
    mSumm = 0.

    mSumm = mSumm + tt-op-day.amt-rub.

    IF LAST-OF(TRIM(STRING(tt-op-day.city-id)) + tt-op-day.city + tt-op-day.kko + tt-op-day.acct-cat + tt-op-day.save-type + tt-op-day.currency + tt-op-day.razdel) THEN
    DO:
        {day_grp.i}
    END.

END.



{strtout3.i &cols=83 &option=Paged}


FOR EACH tt-day-itog NO-LOCK
    WHERE tt-day-itog.acct-cat EQ "b"
    :
    mBalans = mBalans + tt-day-itog.itog.
END.

FOR EACH tt-day-itog NO-LOCK
    WHERE tt-day-itog.acct-cat EQ "o"
    :
    mVBalans = mVBalans + tt-day-itog.itog.
END.

/* Вычисление значения специального поля Bank-name */
{get_set.i "Банк"}
    assign
       bank-name = setting.val
    .
vDateString = GetDateString(end-date).










/*Вот тут сделать печать в ексель.*/
{tit-xls.i}

/*
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
    put unformatted "Срок хранения   __________________________________________________" skip.
    put unformatted " " skip.
    put unformatted "Архивный индекс __________________________________________________" skip.
    put unformatted " " skip.
    put skip(1).
    END.
    ELSE
    DO:
        IF FIRST-OF(tt-kko.kko) THEN 
        DO:
        FIND FIRST branch WHERE branch.branch-id EQ tt-kko.kko
             NO-LOCK NO-ERROR.
        IF AVAILABLE(branch) THEN
            bank-name = branch.NAME + " г. " + tt-kko.city.
	ELSE bank-name = tt-kko.city.
        END.
    END.
    IF FIRST-OF(tt-kko.kko) THEN 
    DO:
    put unformatted "  " tt-kko.kko "  " Bank-name Format "x(82)"
                    "" skip.
    put unformatted "   (полное или сокращенное фирменное наименование кредитной организации и(или)" skip.
    put unformatted "                              наименование филиала)" skip.
    put skip(3).
    END.
    IF FIRST(tt-kko.city-id) THEN 
    DO:
    put unformatted "Документы за " vDateString Format "x(27)"
                    "" skip.
    put skip(3).
    put unformatted "                                по балансовым счетам       по внебалансовым счетам" skip.
    put skip(1).
    put unformatted " " skip.
    put unformatted "Сумма                       " mBalans Format "->>>,>>>,>>>,>>9.99"
                    "  руб.     " mVBalans Format "->>>,>>>,>>>,>>9.99"
                    "  руб." skip.
    put skip(2).
    put unformatted "Из них:" skip.
    put unformatted "хранятся на бумажном носителе и находятся в отдельных папках:" skip.
    put skip(1).
    END.
/*kau buh b 810*/
if tt-kko.kko = "0000" then
DO:
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

    put unformatted "бухгалтерские документы     " mBuh-b Format "->>>,>>>,>>>,>>9.99"
                    "  руб.     " mBuh-o Format "->>>,>>>,>>>,>>9.99"
                    "  руб." skip(1).

END.
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
    put unformatted "кассовые документы          " mKas-b Format "->>>,>>>,>>>,>>9.99"
                    "  руб.     " mKas-o Format "->>>,>>>,>>>,>>9.99"
                    "  руб." skip.
    put skip(2).

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
    put unformatted "    по операциям с иностранной валютой:" skip.
    put skip(1).
    put unformatted "бухгалтерские документы     " mBuh-val-b Format "->>>,>>>,>>>,>>9.99"
                    "  руб.     " mBuh-val-o Format "->>>,>>>,>>>,>>9.99"
                    "  руб." skip.
    put skip(1).
    put unformatted "     срок хранения  ____________" skip.
    put unformatted " " skip.

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

    put unformatted "кассовые документы          " mKas-val-b Format "->>>,>>>,>>>,>>9.99"
                    "  руб.     " mKas-val-o Format "->>>,>>>,>>>,>>9.99"
                    "  руб." skip.
    put skip(1).
    put unformatted "     срок хранения  ____________" skip.
    put unformatted " " skip.
    put skip(1).


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
    put unformatted "хранятся в электронном виде:" skip.
    put unformatted " " skip.
    put unformatted "кассовые документы          " mKas-el-b Format "->>>,>>>,>>>,>>9.99"
                    "  руб.     " mKas-el-o Format "->>>,>>>,>>>,>>9.99"
                    "  руб." skip.
    put skip(1).
    
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
    
    put unformatted "бухгалтерские документы     " mBuh-el-b Format "->>>,>>>,>>>,>>9.99"
                    "  руб.     " mBuh-el-o Format "->>>,>>>,>>>,>>9.99"
                    "  руб." skip.
    put skip(1).

    IF LAST(tt-kko.city-id) THEN
    DO:
    put unformatted " " skip.
    put skip(1).
    put unformatted "Документы сброшюрованы и подшиты __________________________________________________" skip.
    put skip(1).
    put unformatted "___________________________________________________________________________________" skip.
    put unformatted "     (подпись бухгалтерского работника, осуществившего сшив и проверку полноты" skip.
    put unformatted "                            сброшюрованных документов)" skip.
    put skip(1).
    put unformatted "С данными бухгалтерского учета сверено ___________________" skip.
    put unformatted "                                            (подпись)" skip.
    END.

END.

{t-dtl.i}
*/
{endout3.i &nofooter=yes}

