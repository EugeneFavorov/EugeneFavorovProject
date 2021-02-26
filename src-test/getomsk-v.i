/*валюта*/
IF ((SUBSTR(post-mfr.debacc,6,3) NE '810'  AND CAN-DO("!706*,!70701*,!70706*,!90901*,!90902*,!99998*,!99999*,*", post-mfr.debacc) ) OR
   (SUBSTR(post-mfr.credacc,6,3) NE '810' AND CAN-DO("!706*,!70701*,!70706*,!90901*,!90902*,!99998*,!99999*,*", post-mfr.credacc)))
   AND (post-mfr.postype NE 11 AND
       (NOT post-mfr.debacc BEGINS '70608')) /* переоценка */
   AND (NOT (post-mfr.debacc begins '30306' AND post-mfr.credacc begins '70701'))
   AND post-mfr.docno NE 122568216 THEN
DO:
   IF post-mfr.inpostno > 1 THEN
   DO:
      DEF BUFFER bpost FOR bank.post-banker.
      /* поиск валютной суммы по новому алгоритму */
      DEF VAR mdcn LIKE post-banker.docno NO-UNDO.
      DEF VAR i3 AS INT NO-UNDO.
      DEF VAR i3t AS DATETIME NO-UNDO.
      DEF VAR vPereots AS LOG NO-UNDO.
      RELEASE bpost-mfr.
      RELEASE bpost-mfr-cred.
      mdcn = ?.
      i3 = 0. i3t = NOW.
     
      {empty tt-post}
      
      FOR EACH bpost WHERE
         bpost.inpostno EQ post-mfr.inpostno
         NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
         CREATE tt-post.
         BUFFER-COPY bpost TO tt-post.
      END.
     
      DEF VAR encn AS INT NO-UNDO.
      RELEASE bpost-mfr.
      RELEASE bpost-mfr-cred.
      vPereots = False.
      encn = 0.
      /* поиск валютной суммы точно по ДЕБЕТ/КРЕДИТ и только по валютному счету */
      DEF VAR v1cnt AS INT NO-UNDO.
      DEF VAR v1docno LIKE tt-post.docno NO-UNDO.
      DEF VAR v2cnt AS INT NO-UNDO.
      DEF VAR v2docno LIKE tt-post.docno NO-UNDO.
      v1cnt = 0. v2cnt = 0.
      v1docno = ?. v2docno = ?.
     
      IF  SUBSTR(post-mfr.credacc,6,3) NE '810' 
      AND SUBSTR(post-mfr.debacc,6,3)  NE '810' THEN 
      DO:
         /* валюта с обоих сторон */
         IF SUBSTR(post-mfr.credacc,6,3) NE SUBSTR(post-mfr.debacc,6,3) THEN
         DO:
            /* валюты не совпадают */
            FIND FIRST tt-post WHERE
                   tt-post.inpostno EQ post-mfr.inpostno
               AND tt-post.docno NE post-mfr.docno AND tt-post.curtype NE 'RUR'
               AND SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) BEGINS '00000'
               AND SUBSTR( tt-post.debacc,  LENGTH(tt-post.debacc) - 19, 20) EQ post-mfr.debacc
            NO-LOCK NO-ERROR.
            IF AVAIL tt-post THEN
            DO:
               v1docno = tt-post.docno.
               v1cnt = 1.
            END.
            FIND FIRST tt-post WHERE
                   tt-post.inpostno EQ post-mfr.inpostno
               AND tt-post.docno NE post-mfr.docno AND tt-post.curtype NE 'RUR'
               AND SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) EQ post-mfr.credacc
               AND SUBSTR( tt-post.debacc,  LENGTH(tt-post.debacc) - 19, 20) BEGINS '00000'
            NO-LOCK NO-ERROR.
            IF AVAIL tt-post THEN
            DO:
               FOR EACH bpost-mfr-cred WHERE bpost-mfr-cred.docno EQ tt-post.docno
                  NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
                  LEAVE.
               END.
            END.
            ELSE
            DO:
               v1cnt = 0.
            END.
         END.

         IF SUBSTR(post-mfr.credacc,6,3) EQ SUBSTR(post-mfr.debacc,6,3) THEN
         FOR EACH tt-post WHERE
                tt-post.inpostno EQ post-mfr.inpostno
            AND tt-post.docno NE post-mfr.docno
            AND SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) EQ post-mfr.credacc
            AND SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) EQ post-mfr.debacc:
            
            v1docno = tt-post.docno.
            v1cnt = v1cnt + 1.
         END. /* FOR EACH tt-post */
         
         PUT UNFORMATTED "1. v1cnt = " v1cnt SKIP.                
	      
	      IF v1cnt > 1 THEN
         DO:
		      UNDO, THROW NEW Progress.Lang.AppError( 
			   STRING( post-mfr.docno) + ": >1 валютных сумм в пакете").
	      END.
	      ELSE IF v1cnt = 1 THEN
         DO:
            FOR EACH bpost-mfr WHERE bpost-mfr.docno EQ v1docno
               NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
               LEAVE.
            END.
         END.
         ELSE
		      UNDO, THROW NEW Progress.Lang.AppError( 
		      STRING( post-mfr.docno) + ": не нашел валютную сумму в пакете").
      END. 
      ELSE
      DO: /* IF */
         /* валюта только с одной стороны, м.б. переоценка */
         /* сначала ищем с той же стороны что и рублевая сумма */
         /* 1.вариант валютная сумма docno - 1 */
         FOR EACH tt-post WHERE
                tt-post.inpostno EQ post-mfr.inpostno
            AND tt-post.docno EQ post-mfr.docno - 1
            AND (
          	   (SUBSTR(post-mfr.credacc,6,3) NE '810' AND (
          	    (SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) BEGINS '0000' AND
          	     SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) EQ post-mfr.credacc)
          	    )
          	   ) OR 
          	   (SUBSTR(post-mfr.debacc,6,3) NE '810' AND (
          	    (SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) BEGINS '0000' AND
          	     SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) EQ post-mfr.debacc) 
          	    )
          	   ) ):
            v1docno = tt-post.docno.
            v1cnt = v1cnt + 1.
	      END. /* FOR EACH tt-post */
         /* 2. ищем по всему пакету */
         IF v1cnt EQ 0 THEN
         FOR EACH tt-post WHERE 
                tt-post.inpostno EQ post-mfr.inpostno
            AND tt-post.docno NE post-mfr.docno
            AND (
       		    (SUBSTR(post-mfr.credacc,6,3) NE '810' AND (
       		     /*(SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) BEGINS '00000' AND
     		      SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) EQ post-mfr.credacc) OR*/
       		     (SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) BEGINS '0000' AND
       		      SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) EQ post-mfr.credacc)
       		     )
       		    ) OR 
       		    (SUBSTR(post-mfr.debacc,6,3) NE '810' AND (
       		     (SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) BEGINS '0000' AND
       		      SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) EQ post-mfr.debacc) /*OR
     		     (SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) BEGINS '00000' AND
     		      SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) EQ post-mfr.debacc)*/
       		     )
       		    ) ):
            v1docno = tt-post.docno.
            v1cnt = v1cnt + 1.
            PUT UNFORMATTED "21. v1cnt = " v1cnt 
               "; tt-post.inpostno = " tt-post.inpostno  
               "; v1docno = " v1docno 
               "; post-mfr.docno = " post-mfr.docno
            SKIP.                     
            IF (post-mfr.docno = 118444614 
            AND v1docno = 118444612) THEN
            DO:
               v1cnt = 1.
               leave.
            END.
         END. /* FOR */
         
         PUT UNFORMATTED "2. v1cnt = " v1cnt SKIP.
         
         /* если нет с той же стороны что и рублевая сумма, ищем с другой стороны (переоценка) */
         IF v1cnt < 1 THEN
         FOR EACH tt-post WHERE
                tt-post.inpostno EQ post-mfr.inpostno
            AND tt-post.docno NE post-mfr.docno
            AND
       		   (SUBSTR(post-mfr.credacc,6,3) NE '810' AND 
       		   ((SUBSTR(tt-post.credacc,LENGTH(tt-post.credacc) - 19, 20) BEGINS '0000' 
       		     AND
       		     SUBSTR(tt-post.debacc,LENGTH(tt-post.debacc) - 19, 20) EQ post-mfr.credacc)
       		    )) 
       		    OR 
          		 (SUBSTR(post-mfr.debacc,6,3) NE '810' AND (
          		 (SUBSTR( tt-post.debacc, LENGTH(tt-post.debacc) - 19, 20) BEGINS '0000' AND
          		  SUBSTR( tt-post.credacc, LENGTH(tt-post.credacc) - 19, 20) EQ post-mfr.debacc))
          	):
            v1docno = tt-post.docno.
            v1cnt = v1cnt + 1.
  	      END. /* FOR */
         
         PUT UNFORMATTED "3. v1cnt = " v1cnt SKIP.
	      
	      IF v1cnt > 1 THEN
         DO:
		      UNDO, THROW NEW Progress.Lang.AppError( 
			   STRING( post-mfr.docno) + ": >1 валютных сумм в пакете").
	      END. 
	      ELSE IF v1cnt = 1 THEN
         DO:
            FOR EACH bpost-mfr WHERE
               bpost-mfr.docno EQ v1docno
               NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
               LEAVE.
            END.
            IF NOT AVAIL bpost-mfr 
               THEN UNDO, THROW NEW Progress.Lang.AppError( "потеряли post пока грузили").
            END. 
            ELSE
         		UNDO, THROW NEW Progress.Lang.AppError( 
         		STRING( post-mfr.docno) + ": не нашел валютную сумму в пакете").
	         IF AVAIL bpost-mfr
	            AND post-mfr.credacc NE '70606810503531601401'  THEN  /* расходы по % вкладов */
	         DO:
      	      /* теперь эта валюта наша или к ней надо привязать нашу переоценку
     	         ищем документ которому отдаем валюту */
      	      DEF VAR vPer AS LOG NO-UNDO.
               FOR EACH tt-post WHERE
                      tt-post.inpostno EQ post-mfr.inpostno
                  AND (tt-post.curtype EQ 'RUR' OR tt-post.curtype EQ 'RUS' OR tt-post.curtype EQ 'RURS')
                  AND (
                      SUBSTR( bpost-mfr.credacc, LENGTH(bpost-mfr.credacc) - 19, 20) EQ tt-post.credacc OR
                      SUBSTR( bpost-mfr.credacc, LENGTH(bpost-mfr.credacc) - 19, 20) BEGINS '00000'
                       )
                  AND (
                      SUBSTR( bpost-mfr.debacc, LENGTH(bpost-mfr.debacc) - 19, 20) EQ tt-post.debacc OR
                      SUBSTR( bpost-mfr.debacc, LENGTH(bpost-mfr.debacc) - 19, 20) BEGINS '00000'
                      )
                  :
                  
                  IF v2docno = ? THEN
                  DO:
                	   v2docno = tt-post.docno.
                	   vPer = tt-post.debacc BEGINS '706' OR tt-post.credacc BEGINS '706'.
                	   NEXT. /*-------------------------------------------------------------------------*/
                  END.
                  /* у нас не переоценка, а у найденного переоценка */
                  IF (tt-post.debacc BEGINS '706' OR tt-post.credacc BEGINS '706') = false
                     AND vPer THEN
                  DO:
                	   v2docno = tt-post.docno.
                	   vPer = tt-post.debacc BEGINS '706' OR tt-post.credacc BEGINS '706'.
                	   NEXT. /*-------------------------------------------------------------------------*/
                  END.
                  /* у нас переоценка, а у найденного не переоценка */
                  IF (tt-post.debacc BEGINS '706' OR tt-post.credacc BEGINS '706') 
                  AND vPer = false 
                  THEN NEXT.  /*-------------------------------------------------------------------------*/
          		   UNDO, THROW NEW Progress.Lang.AppError(
                	STRING(post-mfr.docno) + ": у валютной суммы несколько рублевых в пакете").
          		   v2docno = ?.
          		   LEAVE.
               END. /* FOR */
      	      IF v2docno <> ? THEN
               DO:
      	         IF v2docno <> post-mfr.docno THEN
                  DO:
         	    	   IF post-mfr.debacc BEGINS '706' OR post-mfr.credacc BEGINS '706' THEN
                     DO:
          	    		   /* мы переоценка, привязываемся к документу */
                        FIND FIRST signs WHERE 
                               signs.dec-value EQ bpost-mfr.docno
                           AND signs.FILE-NAME EQ 'op-entry'
                           AND signs.CODE EQ 'link-vdocno'
                        NO-LOCK NO-ERROR.
                        IF AVAIL(signs) THEN
                        DO:
                           curOp = INTEGER(ENTRY(1,signs.surrogate)).
                        END.
       	    	         docDID = bpost-mfr.did.
     	                  IF docDID EQ ?
                          THEN UNDO, THROW NEW Progress.Lang.AppError( "docDID не может быть равен NULL").
                     END. /* IF */
      	    	      /* валюта не наша */
      	    	      RELEASE bpost-mfr.
      	         END. /*ELSE DO:*/ /* IF v2docno <> post-mfr.docno */
      	    	   /* валюта наша, но вдруг переоценка уже загружена, надо к ней привязаться ??? */
      	         /*END.*/
      	      END. /* IF  v2docno <> ? */
	         END. /* IF AVAIL bpost-mfr */
         END. /* IF */
      END. /* IF post-mfr.inpostno > 1 */

      IF post-mfr.inpostno EQ 1 /*OR post-mfr.docno = 118399289*/ THEN
      DO:
         /* документ мог быть распакечен, а линки остались */
         FOR EACH blinks WHERE
            STRING(blinks.linktype) = '17'
            AND blinks.id2 = post-mfr.did 
            NO-LOCK,
            EACH blinks2 WHERE 
            STRING(blinks2.linktype) = '4' AND blinks2.id1 EQ blinks.id1 
            NO-LOCK,
            FIRST bpost WHERE
                bpost.docno = blinks2.id2
            AND bpost.docno NE post-mfr.docno
            AND (NOT bpost.curtype BEGINS 'RU')
            /* проверяем подходит ли дебет и кредит*/
            AND (
      		    (SUBSTR(post-mfr.credacc,6,3) NE '810' AND 
      		     SUBSTR( bpost.credacc, LENGTH(bpost.credacc) - 19, 20) EQ post-mfr.credacc) OR
      		    (SUBSTR(post-mfr.credacc,6,3) EQ '810' AND 
      		     SUBSTR( bpost.credacc, LENGTH(bpost.credacc) - 19, 20) BEGINS '00000')
      		     )
	         AND (
      		    (SUBSTR(post-mfr.debacc,6,3) NE '810' AND
      		     SUBSTR( bpost.debacc, LENGTH(bpost.debacc) - 19, 20) EQ post-mfr.debacc) OR
      		    (SUBSTR(post-mfr.debacc,6,3) EQ '810' AND
      		     SUBSTR( bpost.debacc, LENGTH(bpost.debacc) - 19, 20) BEGINS '0000')
      		     )
            NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
               
            IF AVAIL bpost-mfr
               THEN UNDO, THROW NEW Progress.Lang.AppError( "в опер.дне несколько сумм в валюте для данной проводки").
            FOR EACH bpost-mfr WHERE
               bpost-mfr.docno EQ bpost.docno
               NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
               LEAVE.
            END.
            IF NOT AVAIL bpost-mfr 
               THEN PUT UNFORMATTED "FFFFFFFFFFFFFF" SKIP.
	      END.

         IF NOT AVAIL bpost-mfr THEN
         DO:
            IF SUBSTR(post-mfr.debacc,6,3) NE '810' AND
               SUBSTR(post-mfr.credacc,6,3) NE '810' AND
               NOT AVAIL bpost-mfr THEN
            DO:
               FOR EACH bpost WHERE
                      bpost.postime >= DATE( MONTH( post-mfr.postime), DAY( post-mfr.postime), YEAR( post-mfr.postime))
                  AND bpost.postime < DATE( post-mfr.postime) + 1
                  AND SUBSTR(bpost.debacc, LENGTH(bpost.debacc) - 19) EQ post-mfr.debacc
                  AND SUBSTR(bpost.credacc, LENGTH(bpost.credacc) - 19) EQ post-mfr.credacc
                  AND (NOT bpost.curtype BEGINS "RU")
                  AND bpost.docno <> post-mfr.docno
                  NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
                  
                  IF AVAIL bpost-mfr
                     THEN UNDO, THROW NEW Progress.Lang.AppError( "в опер.дне несколько сумм в валюте для данной проводки").
                  FOR EACH bpost-mfr WHERE 
                     bpost-mfr.docno EQ bpost.docno
                     NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
                     LEAVE.
                  END.
                  IF NOT AVAIL bpost-mfr 
                     THEN PUT UNFORMATTED "FFFFFFFFFFFFFF" SKIP.
               END.
            END.
            ELSE
            DO:
               IF post-mfr.inpostno EQ 1 THEN
               DO:
                  v1cnt = 0.
                  v1docno = ?.
                  FOR EACH bpost WHERE
                         bpost.postime >= DATE( MONTH( post-mfr.postime), DAY( post-mfr.postime), YEAR( post-mfr.postime))
                     AND bpost.postime < DATE( post-mfr.postime) + 1
                     AND bpost.docno NE post-mfr.docno
                     AND STRING(bpost.inpostno) EQ '1'
                     AND (
                         (SUBSTR(post-mfr.credacc,6,3) NE '810' AND 
                          SUBSTR( bpost.credacc, LENGTH(bpost.credacc) - 19, 20) EQ post-mfr.credacc) OR
                         (SUBSTR(post-mfr.credacc,6,3) EQ '810' AND 
                          SUBSTR( bpost.credacc, LENGTH(bpost.credacc) - 19, 20) BEGINS '00000')
                          )
                     AND (
                         (SUBSTR(post-mfr.debacc,6,3) NE '810' AND
                          SUBSTR( bpost.debacc, LENGTH(bpost.debacc) - 19, 20) EQ post-mfr.debacc) OR
                         (SUBSTR(post-mfr.debacc,6,3) EQ '810' AND
                          SUBSTR( bpost.debacc, LENGTH(bpost.debacc) - 19, 20) BEGINS '0000')
                          )
                     NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
                     
                     IF AVAIL bpost-mfr
                        THEN UNDO, THROW NEW Progress.Lang.AppError( "в опер.дне несколько сумм в валюте для данной проводки").
                     FOR EACH bpost-mfr WHERE
                        bpost-mfr.docno EQ bpost.docno
                        NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
                        LEAVE.
                     END.
                  END.
               END.
            END.
         END. /* IF NOT AVAIL bpost-mfr */
      END.

      IF NOT AVAIL bpost-mfr 
      AND
      (
         (CAN-DO( "!706*,!99996*,!99997*,!90901*,!90902*,*", post-mfr.debacc)
      AND CAN-DO( "!706*,!99996*,!99997*,!90901*,!90902*,*", post-mfr.credacc)))
      AND ( NOT ( CAN-DO( "90*,91*", post-mfr.debacc) AND post-mfr.credacc MATCHES "99999*" + SUBSTR( post-mfr.debacc, 1, 5)))
      AND ( NOT ( CAN-DO( "90*,91*", post-mfr.credacc) AND post-mfr.debacc MATCHES "99998*" + SUBSTR( post-mfr.credacc, 1, 5)))
      AND post-mfr.docno <> 112508030
      AND post-mfr.docno <> 154759757
      AND post-mfr.docno <> 154760773
      AND post-mfr.docno <> 154760545
         THEN UNDO, THROW NEW Progress.Lang.AppError( "у валютной проводки невозможно найти сумму валюты, ili nekuda privyazat kurs raznitsu").
      END.

      /* opredelyaem nomer transakcii */
      RELEASE tpost-mfr.

      IF post-mfr.inpostno > 1 THEN
      FOR EACH tpost-mfr WHERE
             tpost-mfr.docno NE post-mfr.docno
         AND tpost-mfr.inpostno EQ post-mfr.inpostno
         NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
         
         /* ищем к какому документу привязать проводку */
      FIND FIRST signs WHERE
             signs.dec-value EQ tpost-mfr.docno
         AND signs.FILE-NAME EQ 'op-entry'
         AND signs.CODE EQ 'link-docno'
      NO-LOCK NO-ERROR.
      IF AVAIL(signs) THEN LEAVE.
   END.

     
   IF AVAIL tpost-mfr THEN
   DO:
      FIND FIRST bbop WHERE
         bbop.op EQ INTEGER(ENTRY(1,signs.surrogate))
      NO-LOCK NO-ERROR.
      vOpTransId = (IF AVAIL bbop THEN bbop.op-transaction ELSE ?).
      RELEASE bbop.
   END.
   ELSE vOpTransId = ?.

	/* поиск транзакции по порожденному документу в абс из БИС */
   IF vOpTransId EQ ? AND post-mfr.inpostno > 1 THEN
   DO:
      FIND FIRST bis_op-mfr WHERE
         bis_op-mfr.docno EQ post-mfr.inpostno
      NO-LOCK NO-ERROR.
      IF AVAIL bis_op-mfr THEN
      DO:
         FIND FIRST bbop WHERE
            bbop.op EQ bis_op-mfr.op
         NO-LOCK NO-ERROR.
         vOpTransId = (IF AVAIL bbop THEN bbop.op-transaction ELSE ?).
         RELEASE bbop.
      END.
      RELEASE bis_op-mfr.
   END.

   IF AVAIL(bpost-mfr) THEN
      docDID = bpost-mfr.did.

   {empty tt-expdoc}
  
   FOR EACH expdoc WHERE
           expdoc.did    EQ docDID /* post-mfr.did */
      AND (expdoc.volume EQ ? OR expdoc.volume EQ 1) 
      NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
      
      CREATE tt-expdoc.
      BUFFER-COPY expdoc TO tt-expdoc.
      
      IF ((tt-expdoc.field_ EQ 4) OR (tt-expdoc.field_ EQ 8))
      AND LENGTH(tt-expdoc.contain) > 20 THEN
      ASSIGN
         tt-expdoc.contain = SUBSTR(tt-expdoc.contain,LENGTH(tt-expdoc.contain) - 19,20).
   END.

   IF ((post-mfr.debacc begins '40912' or post-mfr.debacc begins '40913') 
   AND post-mfr.credacc begins '3030') THEN
   DO:
      /* 10-бик банка получателя */
   IF GetExpDocValue( 838,?) NE ? THEN
   DO:
      FIND FIRST tt-expdoc WHERE
              tt-expdoc.field_ EQ 10 
         AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1)
      NO-ERROR.
      ASSIGN
         tt-expdoc.contain = GetExpDocValue( 838,?).
   END.
   ELSE
   DO:
      FOR EACH expdoc WHERE
              expdoc.did    EQ post-mfr.did
         AND (expdoc.volume EQ ? OR expdoc.volume EQ 1)
         AND expdoc.field_ EQ 838
         NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
     
         FIND FIRST tt-expdoc WHERE
                 tt-expdoc.field_ EQ 10 
            AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1)
         NO-ERROR.
         ASSIGN
            tt-expdoc.contain = expdoc.contain.
      END.
   END.
   IF GetExpDocValue( 249,?) NE ? THEN
   DO:
      FIND FIRST tt-expdoc WHERE
              tt-expdoc.field_ EQ 8 
         AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1)
      NO-ERROR.
      ASSIGN
         tt-expdoc.contain = GetExpDocValue(249,?).
   END.
   ELSE
   DO:
      FOR EACH expdoc WHERE
              expdoc.did EQ post-mfr.did
         AND (expdoc.volume EQ ? OR expdoc.volume EQ 1)
         AND expdoc.field_ EQ 249
         NO-LOCK QUERY-TUNING ( NO-INDEX-HINT ):
         
         FIND FIRST tt-expdoc WHERE
                 tt-expdoc.field_ EQ 8 
            AND (tt-expdoc.volume EQ ? OR tt-expdoc.volume EQ 1)
         NO-ERROR.
         ASSIGN
            tt-expdoc.contain = expdoc.contain.
      END.
   END.
END. 
/*валюта*/
