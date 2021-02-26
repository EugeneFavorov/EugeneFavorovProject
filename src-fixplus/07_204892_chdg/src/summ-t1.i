{pick-var.i}

&IF DEFINED(SUMM_PROC_HIST) EQ 0 &THEN
&IF DEFINED (SUMM_PROC) EQ 0 &THEN
IF term-obl.sop-date NE ? THEN
   summ-t = 0.
ELSE
&ENDIF
&ENDIF
DO:
   DEFINE VAR xxx   AS DEC NO-UNDO.
   DEFINE VAR xtmp  AS DEC NO-UNDO.
   DEFINE VAR xxx1  AS DEC NO-UNDO.
   DEFINE VAR xxxB  AS DEC NO-UNDO.
   DEFINE VAR xxPr  AS DEC NO-UNDO.
   DEFINE VAR xxBPr AS DEC NO-UNDO.
   DEFINE VAR yyy   AS DEC NO-UNDO.
   DEFINE VAR yyyB  AS DEC NO-UNDO.
   DEFINE VAR yyPr  AS DEC NO-UNDO.
   DEFINE VAR yyBPr AS DEC NO-UNDO.
   DEFINE VAR OpSum46  AS DEC NO-UNDO.
   DEFINE VAR OpSum362 AS DEC NO-UNDO.
   DEFINE VAR OpSum100 AS DEC NO-UNDO.
   DEFINE VAR OpSum309 AS DEC NO-UNDO.

   DEFINE BUFFER bloan-int FOR loan-int.

   ASSIGN
      e1     = 0
      e2     = 0
      e3     = 0
&IF DEFINED (SUMM_PROC) EQ 0 &THEN
      summ-t = 0
&ENDIF
   .
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-k  EQ 6
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      IF CAN-DO({&iskldbparam}, STRING(loan-int.id-d)) THEN
         NEXT.
      e1 = e1 + loan-int.amt.
   END.
      /* ���뢠�� 46-� ������ - ����� �ॡ������ */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ 5
      AND   loan-int.id-k  EQ 35
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 + loan-int.amt.
   END.
      /* ���뢠�� 37-� ������ - ��饭�� %% */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ 5
      AND   loan-int.id-k  EQ 4
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 + loan-int.amt.
   END.
      /* ���뢠�� 362-� ������ - ����� ����祭�� ��業⮢ 302-� */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ 5
      AND   loan-int.id-k  EQ 10
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 + loan-int.amt.
   END.
      /* ���뢠�� 475-� ������ - ����. �����. %% �� ��� १�ࢠ */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ 351
      AND   loan-int.id-k  EQ 10
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 + loan-int.amt.
   END.
      /* ���뢠�� 4007-� ������ - ����襭�� ��㯫����� ��業⮢
      ** �� ������ ���㠫쭠 ⮫쪮 ��� ᤥ��� ���㯪� � ���㫥 �䨭���஢���� */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ 5
      AND   loan-int.id-k  EQ 5402
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 + loan-int.amt.
   END.
      /* ���뢠�� 481-� ������ - ���᫥��� %% ����� ��ਮ��� */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ ?
      AND   loan-int.id-k  EQ 352
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 + loan-int.amt.
   END.
      /* ���⠥� �㬬� ��ࠬ��஢, ����� �� ������ ������ �� �㬬�
      ** ����襭�� ��業⮢ ( ���� %% �� ���� �.�. � �.�. ) */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND  ((     loan-int.id-d  EQ 6
            AND   loan-int.mdate LE mPayDate
            )
         OR (     loan-int.id-d  EQ 32
            AND   loan-int.mdate LE mPayDate
            )
         OR (     loan-int.id-d  EQ 24
            AND   loan-int.mdate LE mPayDate
            ))
   NO-LOCK:
      FIND FIRST chowhe WHERE
                 chowhe.id-k EQ loan-int.id-k
         AND     chowhe.id-d EQ loan-int.id-d
      NO-LOCK NO-ERROR.
      IF AVAIL chowhe
         AND GetXAttrValueEx("chowhe", STRING(chowhe.id-op), "�环����", "��") EQ "���" THEN
         e1 = e1 - loan-int.amt.
      ELSE
         NEXT.
   END.
   /*187945 ��� ��� ����ᮢ ��ࠡ��� �� ����砥�             �᫨ <������� =���>*/
   IF FGetSetting("�������",?,"���") = '���' AND
      NOT CAN-DO(FGetSetting("���������࠭�","�᪫����ኮ���",""),loan.class-code) THEN
   DO:
      /*����� �� �ᥬ �㦭� ������ �� �������� �� ���� ������ �������.*/
      /*��ॡ�ࠥ� �� 67,385,46,362,98,99*/
      /*��ॡ�ࠥ� �� 104,323,304,86,309,100*/
      FOR EACH loan-int WHERE                        /*67*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code 
         AND   loan-int.id-d  EQ 32
         AND   loan-int.id-k  EQ  8
         AND   loan-int.mdate LE mPayDate)
          OR                                         /*385*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code 
         AND   loan-int.id-d  EQ 10
         AND   loan-int.id-k  EQ  8
         AND   loan-int.mdate LE mPayDate)
          OR                                        /*46*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ  5
         AND   loan-int.id-k  EQ 35
         AND   loan-int.mdate LE mPayDate)
          OR                                        /*99*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 10
         AND   loan-int.id-k  EQ 34
         AND   loan-int.mdate LE mPayDate)
          OR                                        /*362*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ  5
         AND   loan-int.id-k  EQ 10
         AND   loan-int.mdate LE mPayDate)
          OR                                        /*98*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 10
         AND   loan-int.id-k  EQ 33
         AND   loan-int.mdate LE mPayDate)
          OR                                    /*104*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 24
         AND   loan-int.id-k  EQ  8
         AND   loan-int.mdate LE mPayDate)
          OR                                    /*323*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 48
         AND   loan-int.id-k  EQ  8
         AND   loan-int.mdate LE mPayDate)
          OR                                    /*304*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 48
         AND   loan-int.id-k  EQ 29
         AND   loan-int.mdate LE mPayDate)

          OR                                   /*86*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 31
         AND   loan-int.id-k  EQ  8
         AND   loan-int.mdate LE mPayDate)
          OR                                   /*309*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 30
         AND   loan-int.id-k  EQ 48
         AND   loan-int.mdate LE mPayDate)
          OR                                   /*100*/
              (loan-int.contract  EQ loan.contract 
         AND   loan-int.cont-code EQ loan.cont-code
         AND   loan-int.id-d  EQ 30
         AND   loan-int.id-k  EQ 29
         AND   loan-int.mdate LE mPayDate)
      NO-LOCK BY loan-int.mdate BY loan-int.avt DESC:
         /*�᫨ ����砥��� ������ 67 (32- 8)
           㢥��稢��� ��६����� ���.*/
         IF     loan-int.id-d  EQ 32
            AND loan-int.id-k  EQ  8
         THEN 
            ASSIGN
               xxx = xxx + loan-int.amt
               OpSum46 = 0
               OpSum362 = 0
            .
         /*46*/
         IF     loan-int.id-d  EQ  5
            AND loan-int.id-k  EQ 35
         THEN  
           ASSIGN
              OpSum46  = loan-int.amt-rub
           .
         /*�᫨ ��� ����� ���, � �����뢠�� �㬬�
           ��  ����襭�� = yyy =���(46,���)*/
         IF xxx GT 0 THEN
            ASSIGN
               yyy = yyy + MIN(OpSum46,xxx)
               /*�����31/08/14: ��᫥ ����樨 46  ��� 㬥��蠥� 
               �� �㬬� ����襭��: ��� = ����। - YYY* */
               xxx = xxx - MIN(OpSum46,xxx)
               OpSum46 = 0
            . 

         /*
         �᫨ ����砥��� ������ 385 (10 8)
         㢥��稢��� �� �� �㬬� ��६����� ��Pr.
         ??? ��� ���� ���㫨�� xxx
         */
         IF     loan-int.id-d  EQ 10
            AND loan-int.id-k  EQ  8
         THEN 
            xxPr = xxPr + loan-int.amt.
         /*
         �᫨ ����砥��� ������ 98(10-33) ��� 99(10-34),�
         ��Pr 㢥��稢��� �� �㬬� ���-yyy, � 
         ���=0 - �ந������ ��७�� 8 ��ࠬ��� �� ������
         */
         IF    (loan-int.id-d  EQ 10
            AND loan-int.id-k  EQ 33)
             OR
               (loan-int.id-d  EQ 10
            AND loan-int.id-k  EQ 34)
         THEN 
            ASSIGN
               xxPr = xxPr + xxx
               xxx  = 0
            .
         /*
         �᫨ ��Pr ����� ���, 
         � �����뢠�� �㬬�  ��  ����襭��
          = yyPr =���(362,�� ��Pr)
         */            
         IF     loan-int.id-d  EQ  5
            AND loan-int.id-k  EQ 10
         THEN  
           ASSIGN
              OpSum362  = loan-int.amt-rub
           .
         IF xxPr GT 0 AND OpSum362 GT 0 THEN
            ASSIGN
               yyPr = yyPr + MIN(OpSum362,xxPr)
               /*�����31/08/14: ��᫥ ����樨 362  ��Pr 㬥��蠥� 
               �� �㬬� ����襭��: ��Pr = ��Pr �। -YYPr */
               xxPr = xxPr - MIN(OpSum362,xxPr)
               OpSum362  =  0 
            . 

         /*
         �᫨ ����砥��� ������ 104 (24-8) ��� 86 (31 8),
         㢥��稢��� ��६����� ���B
         */
         IF    (loan-int.id-d  EQ 24
            AND loan-int.id-k  EQ  8)
             OR
               (loan-int.id-d  EQ 31
            AND loan-int.id-k  EQ  8)
         THEN 
            /* �  � �⮬ ��� ��� ����樨 308(�����)*/
            IF NOT CAN-FIND (FIRST bloan-int WHERE bloan-int.id-d  EQ 48
                                               AND bloan-int.id-k  EQ 30 
                                               AND bloan-int.mdate EQ loan-int.mdate)
            THEN
               ASSIGN
               /*�᫨ �᫨  � ����� ��� 
                ��������� ����樨 104 � 308, �
                㢥��稢��� ��६����� ��BPr �� �㬬� =104, �  X��B=0 -
                �ந������ ��७�� 8 ��ࠬ��� �� ������ �� ���������
                */ 
                  xxxB = xxxB + loan-int.amt
               .
            ELSE
               ASSIGN
                  /*�᫨  � ����� ��� ��������� ����樨 104 � 308, �
                  㢥��稢��� ��६����� ��BPr �� �㬬� =104, �  X��B=0 
                  �ந������ ��७�� 8 ��ࠬ��� �� ������ �� ���������
                  */
                  xxBPr = xxBPr + loan-int.amt
                  xxxB  = 0
               .

         /*
         �᫨ ���B ����� ���,
         � ����塞 �㬬� �� ��襭�� 
         yyyB =  ���(100, ���B). 
         */
       
         IF     loan-int.id-d  EQ 30
            AND loan-int.id-k  EQ 29
         THEN  
           ASSIGN
              OpSum100  = loan-int.amt-rub
           .
          IF xxxB GT 0 AND OpSum100 GT 0 THEN
            ASSIGN
               yyyB = yyyB + MIN(OpSum100,xxxB)
               /*�����31/08/14: ��᫥ ����樨 100  ���B 㬥��蠥� 
                 �� �㬬� ����襭��: ���B = ���B �। - YYY�**
               */
               xxxB = xxxB - MIN(OpSum100,xxxB)
               OpSum100 = 0
            .
         /*
         ����砥��� ������ 323 (48 8),
         㢥��稢��� ��६����� ��BPr
         */
         IF     loan-int.id-d  EQ 48
            AND loan-int.id-k  EQ  8
         THEN 
            xxBPr = xxBPr + loan-int.amt.
         /*
         �᫨ ����砥��� ������ 304 (48 29), �
         㢥��稢��� ��६����� ��BPr �� �㬬� = X��B-yyyB,
         �  X��B=0 - �ந������ ��७�� 8 ��ࠬ���
         �� ������ �� ���������.
         */
         IF     loan-int.id-d  EQ 48
            AND loan-int.id-k  EQ 29
         THEN 
            ASSIGN
               xxBPr = xxBPr + xxxB
               xxxB  = 0
            .
         /*
         �᫨ X�BPr ����� ���,
         � ����塞 �㬬� �� ��襭��
          yyBPr =  ���(309, ��BPr). 
         */          
         IF     loan-int.id-d  EQ 30
            AND loan-int.id-k  EQ 48
         THEN  
           ASSIGN
              OpSum309 = loan-int.amt-rub
           .

         /*��᫥ ����樨 309  ��BPr 㬥��蠥�
           �� �㬬� ����襭��: ��BPr = ��BPr �। - YY�Pr
         */

         IF xxBPr GT 0 THEN
            ASSIGN 
               yyBPr = yyBPr + MIN(OpSum309,xxBPr)
               xxBPr = xxBPr - MIN(OpSum309,xxBPr)
               OpSum309 = 0
            . 
      END.
      /*
      ����� ��諨 �� ���� ������ �������.
      yyy+yyyB+yyPr+yyBPr ��  ��� �㬬� � ��� ᥩ�� ���� ��९���.
      ���� 㢥����� ������襭�� ���⮪ �� ��� �㬬�.      
      */
      e1 = e1 - (yyy + yyyB + yyPr + yyBPr).
      /*�.�. ��⮬ e1 = 0 - e1 � ⠪ ������� ��� �����*/
   END. /*IF FGetSetting("�������" NOT CAN-DO(FGetSetting("���������࠭�"*/

   /* ���⠥� �㬬� ��ࠬ��஢ ����� ���� �८�ࠧ����� � ����樨 �� ᫥���騩 ���� */
DEF VAR mC          AS INT64  NO-UNDO.
DEF VAR mSumma32    AS DEC  NO-UNDO.
DEF VAR mSumma24    AS DEC  NO-UNDO.
DEF VAR mSummaParam AS DEC  NO-UNDO.
DEF VAR mSumma      AS DEC  NO-UNDO.
DEF VAR mParamExt   AS CHAR NO-UNDO.
DEF VAR mAmtDiff    AS DEC  NO-UNDO.
DEF VAR mAmtDiff24  AS DEC  NO-UNDO.

   RUN STNDRT_PARAM_EX (loan.contract,
                        loan.cont-code,
                        32,
                        mPayDate,
                        loan.since,
                 OUTPUT mSumma32,
                 OUTPUT vDbSumDec,
                 OUTPUT vCrSumDec).
   RUN STNDRT_PARAM_EX (loan.contract,
                        loan.cont-code,
                        24,
                        mPayDate,
                        loan.since,
                 OUTPUT mSumma24,
                 OUTPUT vDbSumDec,
                 OUTPUT vCrSumDec).

   RUN inter_current  (BUFFER loan, 32, OUTPUT mAmtDiff) .
   RUN inter_current  (BUFFER loan, 24, OUTPUT mAmtDiff24) .

   mSumma32 = 0 - (mSumma32 + mSumma24 + mAmtDiff + mAmtDiff24).
   IF mSumma32 GT 0 THEN
   DO:
      mSumma = 0.
      /* ���� ��ࠬ��஢ �㦭� �뭥�� � ����ன�� */
      mParamExt = FGetSetting("�����",?,"") + ",4,81,82,96,704".
      DO mC = 1 TO EXTENT(pick-var) - 1:
         IF LOOKUP(STRING(pick-var[mC]),mParamExt) <> 0 THEN NEXT.
         FIND FIRST chowhe WHERE
                    LOOKUP(STRING(chowhe.id-d),"32,24") GT 0
            AND     chowhe.id-k EQ pick-var[mC]
         NO-LOCK NO-ERROR.
         IF NOT AVAIL chowhe OR GetXAttrValueEx("chowhe", STRING(chowhe.id-op), "�环����", "��") NE "���" THEN NEXT.

         RUN STNDRT_PARAM_EX (loan.contract,
                              loan.cont-code,
                              pick-var[mC],
                              mPayDate,
                              loan.since,
                       OUTPUT mSummaParam,
                       OUTPUT vDbSumDec,
                       OUTPUT vCrSumDec).
         RUN inter_current  (BUFFER loan, pick-var[mC], OUTPUT mAmtDiff) .
         mSumma = mSumma + mSummaParam + mAmtDiff.
      END.
      e1 = e1 - MIN(mSumma32, mSumma).
   END.
      /* ���⠥� 480-� ������ - ����祭� %% � ��� ����� ��ਮ��� */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ 352
      AND   loan-int.id-k  EQ 6
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 - loan-int.amt.
   END.
      /* ���⠥� 482-� ������ - �⬥�� ������ %% � ��� ����� ��ਮ��� */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  EQ 352
      AND   loan-int.id-k  EQ ?
      AND   loan-int.mdate LE mPayDate
   NO-LOCK:
      e1 = e1 - loan-int.amt.
   END.
      /* ����室��� ��������  ���᫥��� ��業�� �� ����
      ** ��砫쭮�� �襭��, ����祭�� ��業�� */
    DO i = 1 TO NUM-ENTRIES(vPar):
       RUN STNDRT_PARAM_EX (loan.contract,
                            loan.cont-code,
                            ENTRY(i, vPar),
                            vDateN,
                            loan.since,
                            OUTPUT vSumm,
                            OUTPUT vDbSumDec,
                            OUTPUT vCrSumDec).
      e1 = e1 - vSumm.
   END.
      /* ��᫥ �⮣� �� ����⠫� �㬬� ����襭�� % �� ���⮬ �����-� ᯥ���᪨� ����権 */
   e1 = 0 - e1.
&IF DEFINED (SUMM_PROC_HIST) EQ 0 &THEN

   FOR EACH xerm-obl WHERE 
            xerm-obl.contract   EQ loan.contract
        AND xerm-obl.cont-code  EQ loan.cont-code
&IF DEFINED (SUMM_PROC) EQ 0 &THEN
        AND xerm-obl.idnt       EQ term-obl.idnt 
&ELSE 
        AND xerm-obl.idnt       EQ 1
&ENDIF
&IF DEFINED (SUMM_PROC) EQ 0 &THEN
        AND xerm-obl.end-date   LE term-obl.end-date 
&ELSE 
        AND xerm-obl.dsc-beg-date   LE iEndDate 
&ENDIF
   NO-LOCK
&IF DEFINED (SUMM_PROC) NE 0 &THEN
   BY xerm-obl.end-date
&ENDIF
   :
      ASSIGN
         e1 = e1 + xerm-obl.amt
&IF DEFINED (SUMM_PROC) NE 0 &THEN
         e3 = e3 + xerm-obl.amt WHEN xerm-obl.dsc-beg-date GE iBegDate
&ENDIF
      .
&IF DEFINED (SUMM_PROC) EQ 0 &THEN
         /* �᫨ �㬬� ��易⥫��� ����� ��।������ ��易⥫��⢠, � ����� ����� �� �㦭� - ��易⥫��⢮ ��������� �� ����襭� */
      IF term-obl.amt LE e1 THEN
      DO:
         LEAVE.
      END.
&ENDIF
   END.
&IF DEFINED (SUMM_PROC) EQ 0 &THEN
   IF e1 LE 0 THEN
      summ-t = 0.
   ELSE
      IF term-obl.amt GT e1 THEN
         summ-t = e1.
      ELSE
         summ-t = term-obl.amt.
&ELSE
   oSumm = MIN (e1, e3).
&ENDIF
   RELEASE xerm-obl.

&ELSE

   FIND LAST term-obl-hist WHERE
             term-obl-hist.contract  EQ loan.contract
         AND term-obl-hist.cont-code EQ loan.cont-code
&IF DEFINED (SUMM_PROC) EQ 0 &THEN
         AND term-obl-hist.idnt      EQ term-obl.idnt 
&ELSE 
         AND term-obl-hist.idnt       EQ 1
&ENDIF
         AND term-obl-hist.since     LE iDatePlat
   NO-LOCK NO-ERROR.

   IF AVAIL term-obl-hist THEN
   DO:
      FOR EACH tobl-hist-amt WHERE 
               tobl-hist-amt.tobl-id      EQ term-obl-hist.tobl-id
           AND tobl-hist-amt.dsc-beg-date LE iEndDate 
      NO-LOCK
      BY tobl-hist-amt.end-date:
         ASSIGN
            e1 = e1 + tobl-hist-amt.amt
            e3 = e3 + tobl-hist-amt.amt WHEN tobl-hist-amt.dsc-beg-date GE iBegDate
         .
      END.
 
      oSumm = MIN (e1, e3).

      RELEASE tobl-hist-amt.
   END.

&ENDIF

END.
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:53:42.613+04:00' */
/* $LINTFILE='summ-t1.i' */
/*prosignkOG/vWzzztdEHeqktMzOSQ*/