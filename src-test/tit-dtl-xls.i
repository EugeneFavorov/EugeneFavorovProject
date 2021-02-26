DEFINE VARIABLE mIsNotEmpty AS LOGICAL NO-UNDO.
DEFINE VARIABLE mItog       AS DECIMAL NO-UNDO.
mIsNotEmpty = NO.

DO: /*1*/


	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Расшифровка бухгалтерских документов за </Data></Cell>
		<Cell><Data ss:Type="String">' + string(end-date) + '</Data></Cell>
		</Row>
		'.

	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String">Номер документа</Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Номер Счета</Data></Cell>
		<Cell><Data ss:Type="String">Сумма(руб.)</Data></Cell>
		</Row>
		<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell><Data ss:Type="String">По дебету</Data></Cell>
		<Cell><Data ss:Type="String">По кредету</Data></Cell>
		</Row>
		<Row> 
		<Cell><Data ss:Type="String">1</Data></Cell>
		<Cell><Data ss:Type="String">2</Data></Cell>
		<Cell><Data ss:Type="String">3</Data></Cell>
		<Cell><Data ss:Type="String">4</Data></Cell>
		</Row>
		'.


FOR EACH tt-kko NO-LOCK
    BREAK BY tt-kko.city-id BY tt-kko.kko
    :
    IF FIRST(tt-kko.city-id) THEN 
    DO: /*2*/
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">По балансовым счетам</Data></Cell>
		</Row>
		'.
        mItog = 0.

        FOR EACH tt-op-day NO-LOCK
            WHERE tt-op-day.acct-cat EQ "b"
            AND tt-op-day.currency EQ "810"
            AND tt-op-day.acct-cat EQ "b"
            AND tt-op-day.razdel EQ "b"
            AND tt-op-day.save-type EQ "p"
            AND tt-op-day.kko NE '0000' /*and tt-op-day.kko NE tt-kko.kko*/
            BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
            :
		FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
			NO-LOCK NO-ERROR.
		FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
			NO-LOCK NO-ERROR.
			       		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>
			'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
        END.

	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="1" ></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>
		'.
        mItog = 0.
    END. /*2*/

    IF FIRST-OF(tt-kko.kko) AND NOT FIRST(tt-kko.city-id) THEN 
    DO: /*3*/
        FIND FIRST branch WHERE branch.branch-id EQ tt-kko.kko
             NO-LOCK NO-ERROR.
        IF AVAILABLE(branch) THEN
            bank-name = branch.NAME + " г. " + tt-kko.city.
	ELSE bank-name = tt-kko.city.

	PUT STREAM VVS UNFORMATTED '<Row ss:Height="18"> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">'  + string(tt-kko.kko) + ' ' + Bank-name + '</Data></Cell>
       		</Row>
		<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">По балансовым счетам</Data></Cell>
       		</Row>
		'.
        mItog = 0.
    END. /*3*/

/*kau 2801*/
/*buh b*/
    if tt-kko.kko = "0000" then
    DO:
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p" 
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* buh b*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Бухгалтерские документы</Data></Cell>
       		</Row>
		'.
    mItog = 0.
    FOR EACH tt-op-day NO-LOCK
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "810"
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "b"
        AND tt-op-day.save-type EQ "p" 
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :

/*dtl*/
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>
			'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
/*end-dtl*/


    END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="1" ></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>
		'.
        mItog = 0.
    END.
    END.
/*end kau*/

    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "810"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* kas b*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Кассовые документы</Data></Cell>
       		</Row>
		'.
    mItog = 0.
    FOR EACH tt-op-day NO-LOCK
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "810"
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "k"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>
			'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
    END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s63" ss:MergeAcross="1" ></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>
		'.
        mItog = 0.
    END.
/*----*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.save-type EQ "p"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:         
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">По операциям с инвалютой</Data></Cell>
       		</Row>
		'.
    END.
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "p"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* buh b*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Бухгалтерские</Data></Cell>
       		</Row>
		'.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.currency EQ "VAL"
           AND tt-op-day.acct-cat EQ "b"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "p"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
/*dtl*/
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>
			'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
/*end-dtl*/
       END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>
		'.
	mItog = 0.
    END.
    /* kas val b*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.currency EQ "VAL"
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k"
        AND tt-day-itog.save-type EQ "p"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* kas val b*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Кассовые</Data></Cell>
       		</Row>
		'.
    mItog = 0.

    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "VAL"
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "p"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
/*dtl*/
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>
			'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
/*end-dtl*/
    END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>
		'.
    mItog = 0.
    END.

    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.save-type EQ "e"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">хранятся в электронном виде</Data></Cell>
       		</Row>
		'.
    END.
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "k"
        AND tt-day-itog.save-type EQ "e"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* kas el b*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Кассовые</Data></Cell>
       		</Row>
		'.
    mItog = 0.

    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.acct-cat EQ "b"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "e"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
/*dtl*/
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>
			'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
/*end-dtl*/
    END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>
		'.
    mItog = 0.
    END.

    
    /* buh el b*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "b"
        AND tt-day-itog.razdel EQ "b"
        AND tt-day-itog.save-type EQ "e"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
    /* buh el b*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Бухгалтерские</Data></Cell>
       		</Row>
		'.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.acct-cat EQ "b"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "e"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
/*dtl*/
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>
			'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
/*end-dtl*/
       END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>
		'.
       mItog = 0.
    END.
/*------------------------*/
    FIND FIRST tt-day-itog 
        WHERE tt-day-itog.kko EQ tt-kko.kko
        AND tt-day-itog.acct-cat EQ "o"
         NO-LOCK NO-ERROR.
    IF AVAILABLE(tt-day-itog) THEN
    DO:
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">По внебалансовым счетам</Data></Cell>
       		</Row>'.
        mItog = 0.
        FOR EACH tt-op-day NO-LOCK
            WHERE tt-op-day.acct-cat EQ "o"
            AND tt-op-day.currency EQ "810"
            AND tt-op-day.acct-cat EQ "b"
            AND tt-op-day.razdel EQ "b"
            AND tt-op-day.save-type EQ "p"
            BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
            :
/*dtl*/
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
/*end-dtl*/
        END.

	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>'.
        mItog = 0.
    END.
        mItog = 0.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "810"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "k"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* kas o*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">По внебалансовым счетам</Data></Cell>
       		</Row>'.
    mItog = 0.
    FOR EACH tt-op-day NO-LOCK
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "810"
        AND tt-op-day.acct-cat EQ "o"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "p"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
    END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>'.
        mItog = 0.
        END.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "VAL"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "b"
            NO-LOCK NO-ERROR.
       IF AVAILABLE(tt-day-itog) THEN
       DO:
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">По операциям с инвалютой</Data></Cell>
       		</Row>'.
       END.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "VAL"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "b"
            AND tt-day-itog.save-type EQ "p"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* buh o*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Бухгалтерские</Data></Cell>
       		</Row>'.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.currency EQ "VAL"
           AND tt-op-day.acct-cat EQ "o"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "p"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
       END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>'.
       mItog = 0.
        END.
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.currency EQ "VAL"
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "k"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* kas val o*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Кассовые</Data></Cell>
       		</Row>'.
    mItog = 0.
    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.currency EQ "VAL"
        AND tt-op-day.acct-cat EQ "o"
        AND tt-op-day.razdel EQ "k"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
/*{dtl-each.i}*/
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
    END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>'.
    mItog = 0.
        END.

        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.save-type EQ "e"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell ss:MergeAcross="3" ><Data ss:Type="String">хранятся в электронном виде</Data></Cell>
       		</Row>'.
        END.
        FIND FIRST tt-day-itog 
           WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "k"
            AND tt-day-itog.save-type EQ "e"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* kas el o*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Кассовые</Data></Cell>
       		</Row>'.
    mItog = 0.

    FOR EACH tt-op-day 
        WHERE tt-op-day.kko EQ tt-kko.kko
        AND tt-op-day.acct-cat EQ "o"
        AND tt-op-day.razdel EQ "k"
        AND tt-op-day.save-type EQ "e"
        BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
        :
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
    END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>'.
    mItog = 0.
        END.

    
        FIND FIRST tt-day-itog 
            WHERE tt-day-itog.kko EQ tt-kko.kko
            AND tt-day-itog.acct-cat EQ "o"
            AND tt-day-itog.razdel EQ "b"
            AND tt-day-itog.save-type EQ "e"
             NO-LOCK NO-ERROR.
        IF AVAILABLE(tt-day-itog) THEN
        DO:
    /* buh el o*/
  	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell><Data ss:Type="String"></Data></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Бухгалтерские</Data></Cell>
       		</Row>'.
       mItog = 0.

       FOR EACH tt-op-day 
           WHERE tt-op-day.kko EQ tt-kko.kko
           AND tt-op-day.acct-cat EQ "o"
           AND tt-op-day.razdel EQ "b"
           AND tt-op-day.save-type EQ "e"
           BY tt-op-day.acct-db BY tt-op-day.acct-cr BY tt-op-day.acct-cr
           :
	    FIND FIRST op WHERE RECID(op) EQ tt-op-day.op-cid
	        NO-LOCK NO-ERROR.
	    FIND FIRST op-entry WHERE RECID(op-entry) EQ tt-op-day.ope-cid
	        NO-LOCK NO-ERROR.
		PUT STREAM VVS UNFORMATTED '<Row> 
			<Cell><Data ss:Type="String">' + STRING(op.doc-num) + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-db, "x(20)") + '</Data></Cell>
			<Cell><Data ss:Type="String">' + STRING(op-entry.acct-cr, "x(20)") + '</Data></Cell>
			<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(op-entry.amt-rub) + '</Data></Cell>
        		</Row>'.
		mIsNotEmpty = YES.
		mItog = mItog + op-entry.amt-rub.
       END.
	PUT STREAM VVS UNFORMATTED '<Row> 
		<Cell></Cell>
		<Cell ss:MergeAcross="1" ><Data ss:Type="String">Итого</Data></Cell>
		<Cell ss:StyleID="s64" ><Data ss:Type="Number">' + String(mItog) + '</Data></Cell>
       		</Row>'.
       mItog = 0.
        END.

END.



END. /*1*/