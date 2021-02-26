      IF AVAIL sf THEN
      DO:
         IF {assigned sf.currency} THEN
         DO:
            FIND FIRST currency WHERE currency.currency EQ sf.currency
            NO-LOCK NO-ERROR.
            IF AVAILABLE currency THEN
               mCurName = currency.name-currenc + ", " + sf.currency.
            ELSE
               mCurName = sf.currency.
         END.
         ELSE
            mCurName = "          ----          ".
         mComment = sf.comment.
      END.
      ELSE
         ASSIGN
            mComment = ""
            mCurName = ""
         .
      ASSIGN
         mPosrName   = IF AVAILABLE sf THEN 
                          GetXAttrValueEx("loan",sf.contract + "," + sf.cont-code,
                                       "ИнфПосредник","")
                       ELSE ""
         mPosrINN    = IF NUM-ENTRIES(mPosrName,";") GE 3 THEN
                          ENTRY(NUM-ENTRIES(mPosrName,";") - 1 ,mPosrName,";")
                       ELSE (IF NUM-ENTRIES(mPosrName,";") EQ 2 THEN
                          ENTRY(2 ,mPosrName,";")
                       ELSE "")
      .
      IF mPosrINN GT "" THEN
         mPosrName   = SplitStr(SUBSTRING(mPosrName,1,
                                R-INDEX(mPosrName,mPosrINN) - 2),20,"~n").

      IF AVAILABLE sf THEN DO:
      mPayDocNum = "".
      RUN SFAmtServs(sf.contract,sf.cont-code,
             OUTPUT mAmtC ,
             OUTPUT mIntAmt,
             OUTPUT mAmtNoNds).
      /* Определяем все связанные с ней проводки */
      mSurrOp = GetLinks(sf.class-code,                   
                         sf.contract + "," + sf.cont-code,
                         ?,                               
                         "sf-op-pay",
                         ";",                             
                         ?).
      IF mSurrOp EQ "" THEN
         mSurrOp = GetXAttrValueEx("loan",sf.contract + "," + sf.cont-code,
                                   "OpPay","").
      IF mSurrOp EQ "" THEN
         mSurrOp = GetLinks(sf.class-code,                   
                            sf.contract + "," + sf.cont-code,
                            ?,                               
                            "sf-op-dr,sf-op-nds",
                            ";",                             
                            ?).
      END.
      IF mSurrOp GT "" THEN
      DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
         FIND FIRST op WHERE 
                    op.op EQ INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
         NO-LOCK NO-ERROR.
         IF AVAIL op
              AND op.op-date GE bDb.beg-date 
              AND op.op-date LE bDb.end-date 
         THEN 
            IF LOOKUP(op.doc-num + " " + STRING(op.op-date,"99/99/9999"),
                      mPayDocNum, ";") EQ 0 THEN
            DO:
               {additem2.i "mPayDocNum"
                           "op.doc-num + ' ' + STRING(op.op-date,'99/99/9999')"
                           ";"
               }
            END.
      END.
      IF NOT {assigned mPayDocNum} THEN
         mPayDocNum = "---".
      ELSE
         mPayDocNum = SplitStr(REPLACE(mPayDocNum, ";", "; "), 15, "~n").
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='21/08/2015 11:33:01.560+04:00' */
/* $LINTUSER='boam' */
/* $LINTMODE='1' */
/* $LINTFILE='selprn_newf.i' */
/*prosignyVPX1+hLA49OrQIpRkxYtQ*/