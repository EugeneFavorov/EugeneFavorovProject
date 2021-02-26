{pick-var.i}
&SCOPED-DEFINE xFmt ">>>>>>>>>>>9.99"

/* ���᮪ ��ࠬ��஢ ��� ��� ��業⮢ �� ���� ��砫쭮�� �襭�� */
&SCOPED-DEFINE ParNr '33,29,10,48'

DEFINE VAR isLogSumm-t1   AS LOG INIT NO NO-UNDO.
   /* �⫠��筠� ���ଠ�� */
IF SESSION:DEBUG-ALERT AND SEARCH("summ-t1-log.p") <> ? THEN DO:
    isLogSumm-t1 = YES.
END.
IF isLogSumm-t1 AND AVAIL term-obl THEN DO:
   RUN summ-t1-log.p( STRING(TODAY) + " " +
                                  STRING(TIME,"HH:MM:SS") +
                                  " ������� "  + ENTRY(1,loan.cont-code ,"@")).
   RUN summ-t1-log.p( "~n~n~n" + "�������� ���� " + STRING(term-obl.end-date)).
END.

&IF DEFINED(SUMM_PROC_HIST) =  0 &THEN
&IF DEFINED (SUMM_PROC) =  0 &THEN
IF term-obl.sop-date <> ? THEN
   summ-t = 0.
ELSE
&ENDIF
&ENDIF

&IF DEFINED(no-date) = 0 &THEN
{intrface.get date}
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
   DEFINE VAR OpSum96  AS DEC NO-UNDO.
   DEFINE VAR OpSum97  AS DEC NO-UNDO.
   DEFINE VAR OpSum98  AS DEC NO-UNDO.
   DEFINE VAR OpSum100 AS DEC NO-UNDO.
   DEFINE VAR OpSum309 AS DEC NO-UNDO.
   DEFINE VAR mDateHolly AS DATE NO-UNDO.
      
   DEF BUFFER bxloan-int FOR loan-int.
   
   &IF DEFINED(PERC) =  0 &THEN
   DEFINE VARIABLE mNPRazdYchet      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mNPOverClassTranz AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mNPNachStraph     AS CHARACTER NO-UNDO.
   &ENDIF

   DEFINE BUFFER bloan-int FOR loan-int.

   ASSIGN
   &IF DEFINED(PERC) =  0 &THEN 
      mNPRazdYchet = FGetSetting("�������",?,"���")
      mNPOverClassTranz = FGetSetting("���������࠭�","�᪫����ኮ���","")
      mNPNachStraph = FGetSetting("�����",?,"")       
   &ENDIF
      e1     = 0
      e2     = 0
      e3     = 0
      xxx    = 0
      xtmp   = 0
      xxx1    = 0
      xxxB    = 0
      xxPr    = 0
      xxBPr   = 0
      yyy     = 0
      yyyB      = 0
      yyPr      = 0
      yyBPr     = 0
      OpSum46   = 0
      OpSum362  = 0
      OpSum96   = 0
      OpSum97   = 0
      OpSum100  = 0
      OpSum309  = 0

&IF DEFINED (SUMM_PROC) =  0 &THEN
      summ-t = 0
&ENDIF
   .


/*
&IF DEFINED(oracle) &THEN            
   DEFINE VARIABLE vSqlCmd AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vHdl    AS INT64       NO-UNDO.
   vSqlCmd = 
      "SELECT sum(l.amt_rub) FROM loan_int l " +
      " WHERE UPPER(l.contract) = '" + CAPS(loan.contract) + "' " +
      "   AND UPPER(l.cont_code) = '" + CAPS(loan.cont-code) + "' " +
      "   AND l.mdate <= to_date('" + STRING(mPayDate,"99/99/9999") + "','dd/mm/yyyy') " +
      "   AND ((l.id_k = 6 AND NOT l.id_d IN (" + {&iskldbparam} + ")) OR " +
      "        (l.id_d = 5 AND l.id_k IN (35, 4, 10, 5402)) OR " +
      "        (l.id_d IS null AND l.id_k IN (352, 16)) OR " + 
      "        (l.id_d = 23 AND l.id_k = 48) OR (l.id_d = 351 and l.id_k = 10) OR " +
      "        (l.id_d = 22 AND l.id_k = 10) OR (l.id_d IS null and l.id_k = 48) OR " +
      "        (l.id_d IS null and l.id_k = 29)) ".
   .
   RUN STORED-PROC send-sql-statement vHdl = PROC-HANDLE (vSqlCmd). 
   FOR EACH proc-text-buffer WHERE PROC-HANDLE = vHdl:
      e1 = e1 + DEC(proc-text).
   END.
   CLOSE STORED-PROC send-sql-statement WHERE PROC-HANDLE = vHdl.
&ELSE
*/
   FOR EACH loan-int WHERE 
           (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-k  =  6
        AND loan-int.mdate <= mPayDate
        AND loan-int.id-d  <> 95)
        /*AND NOT CAN-DO({&iskldbparam}, STRING(loan-int.id-d)))*/
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  5       /* ���뢠�� 46-� ������ - ����� �ॡ������ */
        AND loan-int.id-k  =  35
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  5       /* ���뢠�� 37-� ������ - ��饭�� %% */
        AND loan-int.id-k  =  4
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  5       /* ���뢠�� 362-� ������ - ����� ����祭�� ��業⮢ */
        AND loan-int.id-k  =  10      /* 302-� */
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  5     /* ���뢠�� 4007-� ������ - ����襭�� ��㯫����� ��業⮢*/
        AND loan-int.id-k  =  5402  /* ** �� ������ ���㠫쭠 ⮫쪮 ��� ᤥ��� ���㯪�  */
        AND loan-int.mdate <= mPayDate)  /* � ���㫥 �䨭���஢���� */
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  ?      /* ���뢠�� 481-� ������ - ���᫥��� %% ����� ��ਮ��� */
        AND loan-int.id-k  =  352
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  ?      /* ??? */
        AND loan-int.id-k  =  16
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  23      /* ���뢠�� 47-� ������ - ��७�� ���.�/� % �� %%, */
        AND loan-int.id-k  =  48      /* �����.� �� */
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  23      /* ���뢠�� 43-� ������ - ��७�� ���.�/� % �� %%,  */
        AND loan-int.id-k  =  ?       /* �����. � ����*/
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  351     /* ���뢠�� 475-� ������ - ����. �����. %%  */
        AND loan-int.id-k  =  10      /* �� ��� १�ࢠ */
        AND loan-int.mdate <= mPayDate )    
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  22     /* ���뢠�� 466-� ������ - �����.%% (���.), */
        AND loan-int.id-k  =  10     /* ᯨᠭ�� � ��⪨ */
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  ?      /* ���뢠�� 226-� ������ - ���.����.% �� �� �� ����. */
        AND loan-int.id-k  =  48     /* (48��) */
        AND loan-int.mdate <= mPayDate)
      OR   (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.id-d  =  ?      /* ���뢠�� 959-� ������ - ��饭�� ���.% �� ��  */
        AND loan-int.id-k  =  29     /* �� ����.(�� 29) */
        AND loan-int.mdate <= mPayDate)
   NO-LOCK:
      e1 = e1 + loan-int.amt.
   END.
/*   
&ENDIF
*/
      /* ���⠥� �㬬� ��ࠬ��஢, ����� �� ������ ������ �� �㬬�
      ** ����襭�� ��業⮢ ( ���� %% �� ���� �.�. � �.�. ) */
   FOR EACH loan-int WHERE
           (loan-int.contract  =  loan.contract
        AND loan-int.cont-code =  loan.cont-code
        AND loan-int.mdate <= mPayDate)
      NO-LOCK,
      FIRST chowhe WHERE 
            chowhe.id-k =  loan-int.id-k
        AND chowhe.id-d =  loan-int.id-d
         NO-LOCK:
         IF GetXAttrValueEx("chowhe", STRING(chowhe.id-op), "�环����", "��") =  "���" THEN
            e1 = e1 - loan-int.amt.
   END.

   /*187945 ��� ��� ����ᮢ ��ࠡ��� �� ����砥�             �᫨ <������� =���>*/
   {summ-t1-nrazd.i}

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
   IF mSumma32 >  0 THEN
   DO:
      mSumma = 0.
      /* ���� ��ࠬ��஢ �㦭� �뭥�� � ����ன�� */
      mParamExt = mNPNachStraph + ",4,81,82,96,704".
      DO mC = 1 TO EXTENT(pick-var) - 1:
         IF LOOKUP(STRING(pick-var[mC]),mParamExt) <> 0 THEN NEXT.
         FIND FIRST chowhe WHERE
                    LOOKUP(STRING(chowhe.id-d),"32,24") >  0
            AND     chowhe.id-k =  pick-var[mC]
         NO-LOCK NO-ERROR.
         IF NOT AVAIL chowhe OR GetXAttrValueEx("chowhe", STRING(chowhe.id-op), "�环����", "��") <> "���" THEN NEXT.

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
      AND   loan-int.id-d  =  352
      AND   loan-int.id-k  =  6
      AND   loan-int.mdate <= mPayDate
   NO-LOCK:
      e1 = e1 - loan-int.amt.
   END.
      /* ���⠥� 482-� ������ - �⬥�� ������ %% � ��� ����� ��ਮ��� */
   FOR EACH loan-int {wh-t &f=loan-int &c="/*"}
      AND   loan-int.id-d  =  352
      AND   loan-int.id-k  =  ?
      AND   loan-int.mdate <= mPayDate
   NO-LOCK:
      e1 = e1 - loan-int.amt.
   END.

      /* ����室��� ��������  ���᫥��� ��業�� �� ����
      ** ��砫쭮�� �襭��, ����祭�� ��業�� */
&IF DEFINED (SUMM_PROC) =  0 &THEN
    IF term-obl.end-date >= vDateN THEN
&ENDIF
       /*
       �� ����� ������襭���� ���⪠ �� %%
       �� 㢥��稢����� �� �㬬� ��ࠬ��஢ 33,29,10
       ����� �뫨 ᮧ���� �� ���� ��砫쭮�� �襭��.
       ����� ��ࠧ��, ��� ����ᥩ ��䨪� ����襭�� %% 
       ��� ������ ��室���� �� ���� �� ����砥� �����४��� �㬬�.
       */
    DO i = 1 TO NUM-ENTRIES({&ParNr}):
       RUN STNDRT_PARAM_EX (loan.contract,
                            loan.cont-code,
                            ENTRY(i, {&ParNr}),
                            vDateN,
                            loan.since,
                            OUTPUT vSumm,
                            OUTPUT vDbSumDec,
                            OUTPUT vCrSumDec).
      e1 = e1 - vSumm.
   END.
   
   &IF DEFINED (SUMM_PROC) <> 0 &THEN
      RUN STNDRT_PARAM_EX (loan.contract,
                            loan.cont-code,
                            "633",
                            mPayDate,
                            loan.since,
                            OUTPUT vSumm,
                            OUTPUT vDbSumDec,
                            OUTPUT vCrSumDec).
      e1 = e1 - vSumm.
   &ENDIF   
      /* ��᫥ �⮣� �� ����⠫� �㬬� ����襭�� % �� ���⮬ �����-� ᯥ���᪨� ����権 */
   e1 = 0 - e1.
&IF DEFINED (SUMM_PROC_HIST) =  0 &THEN

   FOR EACH xerm-obl WHERE 
            xerm-obl.contract   =  loan.contract
        AND xerm-obl.cont-code  =  loan.cont-code
&IF DEFINED (SUMM_PROC) =  0 &THEN
        AND xerm-obl.idnt       =  term-obl.idnt 
&ELSE 
        AND xerm-obl.idnt       =  1
&ENDIF
&IF DEFINED (SUMM_PROC) =  0 &THEN
        AND xerm-obl.end-date   <= term-obl.end-date 
&ELSE 
        AND xerm-obl.dsc-beg-date   <= iEndDate 
&ENDIF
   NO-LOCK
&IF DEFINED (SUMM_PROC) <> 0 &THEN
   BY xerm-obl.end-date
&ENDIF
   :
      ASSIGN
         e1 = e1 + xerm-obl.amt
&IF DEFINED (SUMM_PROC) <> 0 &THEN
         e3 = e3 + xerm-obl.amt WHEN xerm-obl.dsc-beg-date >= iBegDate
&ENDIF
      .
&IF DEFINED (SUMM_PROC) =  0 &THEN
         /* �᫨ �㬬� ��易⥫��� ����� ��।������ ��易⥫��⢠, � ����� ����� �� �㦭� - ��易⥫��⢮ ��������� �� ����襭� */
      IF term-obl.amt <= e1 THEN
      DO:
         LEAVE.
      END.
&ENDIF
   END.
&IF DEFINED (SUMM_PROC) =  0 &THEN
   IF e1 <= 0 THEN
      summ-t = 0.
   ELSE
      IF term-obl.amt >  e1 THEN
         summ-t = e1.
      ELSE
         summ-t = term-obl.amt.
&ELSE
   oSumm = MIN (e1, e3).
&ENDIF
   RELEASE xerm-obl.

&ELSE

   FOR EACH term-obl-hist WHERE
             term-obl-hist.contract  =  loan.contract
         AND term-obl-hist.cont-code =  loan.cont-code
&IF DEFINED (SUMM_PROC) =  0 &THEN
         AND term-obl-hist.idnt      =  term-obl.idnt 
&ELSE 
         AND term-obl-hist.idnt       =  1
&ENDIF
         AND term-obl-hist.since     >= iDatePlat
   NO-LOCK BY term-obl-hist.since:

      FOR EACH tobl-hist-amt WHERE 
               tobl-hist-amt.tobl-id      =  term-obl-hist.tobl-id
           AND tobl-hist-amt.dsc-beg-date <= iEndDate 
      NO-LOCK
      BY tobl-hist-amt.end-date:
         ASSIGN
            e1 = e1 + tobl-hist-amt.amt
            e3 = e3 + tobl-hist-amt.amt WHEN tobl-hist-amt.dsc-beg-date >= iBegDate
         .
      END.
 
      oSumm = MIN (e1, e3).

      RELEASE tobl-hist-amt.
      LEAVE.
   END.

&ENDIF

END.
/* $LINTFILE='summ-t1.i' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='05/06/2017 11:50:21.397+03:00' */
/*prosign06hDhERMlu4ZUFAzRsbp9A*/