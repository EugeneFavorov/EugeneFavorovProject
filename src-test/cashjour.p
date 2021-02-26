/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: cashjour.p
      Comment: ���ᮢ� ��ୠ�
   Parameters: in/out - �� ��室�/��室�
               RUB/RUR - ��� ���. ������
         Uses:
      Used by:
      Created: 29.09.2010 15:58 elus    
     Modified: 29.09.2010 15:58 elus    
*/
DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.

{globals.i}
{wordwrap.def}
{svodord.def}

{intrface.get vok} /* �����㬥�� ࠡ��� � ��쥪⠬� ��� */
{intrface.get sessions}
{intrface.get tparam}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get rights}
{intrface.get acct}
{intrface.get db2l}
{intrface.get instrum}
{intrface.get op}

{agr-beg.def 
   &NameTitle = "�������� ������ � ������"
   &TypeDoc   = "'*'"} 
&GLOBAL-DEFINE end-date mCuDate
{korder.i}

FUNCTION FormatSumm  CHAR (DEC) FORWARD.

DEFINE VARIABLE mRepDebit  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mColTit    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mSortOrder AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mLines     AS CHARACTER   NO-UNDO EXTENT 15. /* �����窠 */
DEFINE VARIABLE mCols      AS INT64       NO-UNDO. /* ��ਭ� ���� */
DEFINE VARIABLE mRKRPol    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mRKROtr    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mOpTime    AS INT64       NO-UNDO.
DEFINE VARIABLE mOpDate    AS DATE        NO-UNDO.

DEFINE TEMP-TABLE ttCasJour NO-UNDO
   FIELD acct-kas   AS CHARACTER
   FIELD doc-num    AS CHARACTER
   FIELD acct-cor   AS CHARACTER
   FIELD digital    AS CHARACTER
   FIELD amt-cur    AS DECIMAL
   FIELD symbol     AS CHARACTER
   FIELD amt-rub    AS DECIMAL
   FIELD branch     AS CHARACTER
   FIELD RecidOpEn  AS RECID
   FIELD op         AS INTEGER
   FIELD user-id    AS CHARACTER
   FIELD i-currency AS CHARACTER
   FIELD DocDate    AS CHARACTER
   FIELD OpDate     AS DATE
   FIELD dpr-id     AS INT64
   FIELD acct-cr    AS CHARACTER
   FIELD acct-db    AS CHARACTER
   FIELD acct-cat   LIKE acct.acct-cat
   INDEX idxdate DocDate
   .

DEFINE TEMP-TABLE ttCasJourItog NO-UNDO
   FIELD acct-kas AS CHARACTER
   FIELD symbol   AS CHARACTER
   FIELD amt-cur  AS DECIMAL
   FIELD amt-rub  AS DECIMAL
   FIELD user-id  AS CHARACTER
   INDEX idx IS UNIQUE acct-kas symbol user-id
   .

DEFINE VARIABLE mOtrVal    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mDate      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mEkvKursOp AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mSpacePrim AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mCodNacVal AS CHARACTER   NO-UNDO.

ASSIGN
   mRKRPol    = fGetSetting("���������","","")
   mRKROtr    = fGetSetting("���������","","")
   mOtrVal    = fGetSetting("��������","������","name-currenc")
   mDate      = fGetSetting("��������","���","��") EQ "��"
   mEkvKursOp = fGetSetting("��������","������Ꭿ","��") EQ "��"
   mSpacePrim = fGetSetting("��������","���ਬ","��") EQ "���"
   mRepDebit  = ENTRY(1,iParam) EQ "in"
   mColTit    = IF mRepDebit THEN "��室" ELSE "��室"
   mSortOrder = FGetSetting("���⊠�㬬", "", "��") EQ "��"
.

IF NUM-ENTRIES(iParam) > 1 THEN
   mCodNacVal = ENTRY(2,iParam).
ELSE
   mCodNacVal = "RUB".

ASSIGN
   mLines[ 1] = "������������������������������������������������������������������������������������������������Ŀ"        
   mLines[ 2] = "�     �      �        NN ��⮢         �  ����  �      �㬬�        ������� �    �ਬ�砭��     �"        
   mLines[ 3] = "�  ���ᮢ��� �                          ����㬥�-�                   �  ��   �                   �"        
   mLines[ 4] = "�  ���㬥�� �                          �   �   �                   ����� �                   �"        
   mLines[ 5] = "�            �                          �        �                   �" + mColTit
                                                                                          + "��                   �"
   mLines[ 6] = "������������������������������������������������������������������������������������������������Ĵ"
   mLines[ 7] = "�     1      �             2            �   3    �         4         �   5   �         6         �"
   mLines[10] = "������������������������������������������������������������������������������������������������Ĵ"
   mLines[12] = "� �⮣� �� "
                     + mColTit + "�                               � "
   mLines[13] = "������������������������������������������������������������������������������������������������Ĵ"
   mLines[14] = "��������������������������������������������������������������������������������������������������"
   mLines[15] = "�                                                � "

   mCols            = LENGTH(mLines[1]).
   .
&GLOBAL-DEFINE cols mCols

{agr-beg.i}

RUN CreateTemp-Table IN THIS-PROCEDURE.
{agr-end.i 
   &OnePageRep  = "YES"
   &OnePageName = "PrintCashJour"}

RUN End-SysMes IN h_tmess.

{intrface.del}

PROCEDURE PrintCashJour.
   DEFINE VARIABLE vOKUD         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRepName      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPrintTitItog AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vUser         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDate         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMonth        AS CHARACTER   NO-UNDO 
      INIT "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������".
   DEFINE VARIABLE vBranchName   AS CHARACTER   NO-UNDO.

   DEFINE BUFFER bttCasJour FOR ttCasJour.
   DEFINE BUFFER xttCasJour FOR ttCasJour.
   DEFINE BUFFER buser      FOR _user.

   IF mRepDebit THEN
      ASSIGN
         vOKUD    = "0401704" 
         vRepName = "��������"
         .
   ELSE 
      ASSIGN 
         vOKUD    = "0401705"
         vRepName = "���������"
         .

   FOR EACH bttCasJour
   BREAK BY bttCasJour.OpDate:

   IF mDate THEN
      vDate = IF {assigned mDateOtc} THEN STRING(DAY(DATE(bttCasJour.DocDate)),"99") + "." + STRING(MONTH(DATE(bttCasJour.DocDate)),"99") + "." + STRING(YEAR(DATE(bttCasJour.DocDate)))
                                     ELSE STRING(DAY(mCuDate),"99") + "." + STRING(MONTH(mCuDate),"99") + "." + STRING(YEAR(mCuDate)).

   ELSE
      vDate = IF {assigned mDateOtc} THEN STRING(DAY(DATE(bttCasJour.DocDate)),"99") + " " + ENTRY(INT(MONTH(DATE(bttCasJour.DocDate))),vMonth) + " " + STRING(YEAR(DATE(bttCasJour.DocDate)))
                                     ELSE STRING(DAY(mCuDate),"99") + " " + ENTRY(INT(MONTH(mCuDate)),vMonth) + " " + STRING(YEAR(mCuDate)).

      IF FIRST-OF(bttCasJour.OpDate) THEN
      DO:
         IF mCashOrd THEN
         DO:
            {empty RecIdOp}
            FOR EACH RecIdOpDate WHERE 
                     RecIdOpDate.DocDate EQ bttCasJour.DocDate
               NO-LOCK:
               CREATE
                  RecIdOp
                  .
               ASSIGN
                  RecIdOp.op = RecIdOpDate.op 
                  .
            END.
            RUN getcashtt.p(TABLE RecIdOp,OUTPUT TABLE TTSvodOrd,mUsDprIDLst).
            FOR EACH TTSvodOrd WHERE 
                     TTSvodOrd.OrdType BEGINS mColTit
               NO-LOCK:
               DO i = 1 TO NUM-ENTRIES(TTSvodOrd.OpLst):
                  IF i = 1 THEN
                  DO:
                     FIND FIRST ttCasJour WHERE
                                ttCasJour.op      EQ INT64(ENTRY(i,TTSvodOrd.OpLst))
                            AND ttCasJour.acct-cr EQ TTSvodOrd.AcctCr
                            AND ttCasJour.acct-db EQ TTSvodOrd.AcctDb
                        NO-ERROR.
                  END.
                  ELSE
                  DO:
                     FIND FIRST xttCasJour WHERE
                                xttCasJour.op      EQ INT64(ENTRY(i,TTSvodOrd.OpLst))
                            AND xttCasJour.acct-cr EQ TTSvodOrd.AcctCr
                            AND xttCasJour.acct-db EQ TTSvodOrd.AcctDb
                        NO-ERROR.
                     ASSIGN
                        ttCasJour.amt-cur = ttCasJour.amt-cur + xttCasJour.amt-cur
                        ttCasJour.amt-rub = ttCasJour.amt-rub + xttCasJour.amt-rub
                        ttCasJour.symbol  = ttCasJour.symbol + "," + xttCasJour.symbol WHEN NOT CAN-DO(ttCasJour.symbol,xttCasJour.symbol)
                     .                        
                     DELETE xttCasJour.
                  END.
               END.
            END.
         END.
         FOR EACH ttCasJour WHERE ttCasJour.DocDate EQ bttCasJour.DocDate
            NO-LOCK
            BREAK BY ttCasJour.branch
                  BY ttCasJour.user-id
                  BY ttCasJour.acct-kas
                  BY (IF mSortOrder
                         THEN STRING(ttCasJour.amt-cur, "99999999999999999999.99")
                         ELSE (SUBSTRING(ttCasJour.acct-cor,1,8) + SUBSTRING(ttCasJour.acct-cor,10)))
                  BY ttCasJour.doc-num
                  BY (IF mSortOrder
                      THEN (SUBSTRING(ttCasJour.acct-cor,1,8) + SUBSTRING(ttCasJour.acct-cor,10))
                      ELSE STRING(ttCasJour.amt-cur, "99999999999999999999.99")):

            IF FIRST-OF(ttCasJour.acct-kas) THEN
            DO:
               PAGE.
               IF     vUser NE ttCasJour.user-id 
                  AND mRazbIsp THEN
               DO:
                  vUser = ttCasJour.user-id.
                  FIND FIRST buser WHERE 
                             buser._Userid EQ ttCasJour.user-id
                     NO-LOCK NO-ERROR.
                  IF AVAIL buser THEN
                  PUT UNFORMATTED 
                     buser._User-Name  SKIP
                     FILL("�",{&Cols}) SKIP.
               END.
               /* ���客 5.10.16 ��� Ticket#2016093010000352
               vBranchName = (IF fGetSetting("������","","") NE ttCasJour.branch THEN (mBank + ", ") ELSE "") + mBranchName.              
               */
               vBranchName = mBranchName.              
               PUT UNFORMATTED 
                  SPACE(mCols - 10) "���������Ŀ"             SKIP
                  SPACE(mCols - 10) "� " + vOKUD + " �"       SKIP
                  SPACE(mCols - 10) "�����������"             SKIP(1)
                  vBranchName                                 SKIP
                  FILL('�',LENGTH(vBranchName))               SKIP
                  PADC("���⠢�⥫�",LENGTH(vBranchName))     SKIP(1)
                  PADC("���ᮢ� ��ୠ�",{&Cols})             SKIP
                  PADC("�� " + mColTit + "�",{&Cols})         SKIP(1)
                  PADC(vDate,{&Cols})                         SKIP
                  PADC("�����������",{&Cols})                 SKIP
                  PADC("���",{&Cols})                        SKIP(1)
                  "��� �� ���� ����� � " ttCasJour.acct-kas SKIP
                  SPACE(22) FILL('�',24)                      SKIP
                  ttCasJour.i-currency " "                    SKIP
                  . 
               DO i = 1 TO 10:
                  IF {assigned mLines[i]} THEN
                     PUT UNFORMATTED mLines[i] SKIP.
               END.
               vPrintTitItog = NO.
            END.
            PUT UNFORMATTED "� " STRING(ttCasJour.doc-num,"x(10)") " � "
                                 STRING(STRING(ttCasJour.acct-cor,GetAcctFmt(ttCasJour.acct-cat)),"x(24)") " � "
                                 STRING(ttCasJour.digital,"x(6)") " � "
                                 FormatSumm(ttCasJour.amt-cur)    " � "
                                 STRING(ttCasJour.symbol,"x(5)")  " � "
                                 (IF (ttCasJour.amt-rub = 0 OR mSpacePrim) THEN "                 " ELSE FormatSumm(ttCasJour.amt-rub)) " �" SKIP.
            /* �⮣� */
            IF LAST-OF(ttCasJour.acct-kas) THEN
            DO:
               PUT UNFORMATTED mLines[13] SKIP.
               FOR EACH ttCasJourItog WHERE
                        ttCasJourItog.acct-kas EQ ttCasJour.acct-kas
                    AND ttCasJourItog.user-id  EQ ttCasJour.user-id
                  NO-LOCK:
                  PUT UNFORMATTED (IF vPrintTitItog THEN mLines[15] ELSE mLines[12]) FormatSumm(ttCasJourItog.amt-cur) " � " STRING(ttCasJourItog.symbol,"x(5)") " � " (IF (ttCasJourItog.amt-rub EQ 0 OR mSpacePrim) THEN "                 " ELSE FormatSumm(ttCasJourItog.amt-rub)) " �" SKIP.
                  vPrintTitItog = YES.
               END.
               PUT UNFORMATTED mLines[14] SKIP.
               RUN GetRepFioByRef IN THIS-PROCEDURE (vRepName,
                                                     ttCasJour.branch,
                                                     ttCasJour.op,
                                                     ttCasJour.Dpr-id).
               DO i = 1 TO mTotalSign:
                  RUN PrintFioAndPost IN THIS-PROCEDURE (mFioInRep[i],
                                                         mPostInRep[i],
                                                         0).
               END.
               /* ������ */
            END.
         END.
      END.
   END.
END PROCEDURE. /* PrintCashJour */

PROCEDURE CreateTemp-Table.

   DEFINE VARIABLE vAcctKas   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcctCor   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vChDpr-id  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcctJourn AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSymbol    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vUser      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vICurrency AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDpr-id    AS INT64       NO-UNDO.
   DEFINE VARIABLE vDate      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRate      AS DECIMAL     NO-UNDO.

   DEFINE BUFFER xop-entry FOR op-entry.
   DEFINE BUFFER acct-kas  FOR acct.
   DEFINE BUFFER acct-cor  FOR acct.
   DEFINE BUFFER kau-entry FOR kau-entry.
   DEFINE BUFFER op        FOR op.
   DEFINE BUFFER op-entry  FOR op-entry.
   DEFINE BUFFER sessions  FOR sessions.
   DEFINE BUFFER currency  FOR currency.

   vAcctJourn = FGetSetting("��⊠���","","").

   DO i = 1 TO NUM-ENTRIES(mUsDprIDLst):
      vChDpr-id = ENTRY(i,mUsDprIDLst).
      LOOP:
      FOR EACH kau-entry WHERE 
               kau-entry.kau       EQ vChDpr-id
           AND kau-entry.kau-id    BEGINS "��������" 
           AND kau-entry.op-status GE CHR(251)
           AND kau-entry.op-date   EQ mCuDate
         NO-LOCK,
         FIRST op OF kau-entry
         NO-LOCK,
         EACH op-entry OF op
         NO-LOCK:

         IF Pereschet(op.op,op-entry.op-entry,kau-entry.debit) THEN
            NEXT LOOP.
         
         RUN GetDateTimeOpTr IN THIS-PROCEDURE (op.op-transaction, 
                                                op.op, 
                                                OUTPUT mOpTime,
                                                OUTPUT mOpDate).

         CASE mDateOtc:
            WHEN "" THEN
            DO:
            END.
            WHEN "*" THEN
            DO:
               IF NOT CAN-DO(mDateLst, STRING(mOpDate, "99/99/9999")) THEN NEXT LOOP.
            END.
            OTHERWISE
            DO:
               IF mDateOtc NE STRING(mOpDate, "99/99/9999") THEN NEXT LOOP.
            END.
         END CASE.

         vDate = IF {assigned mDateOtc} THEN STRING(mOpDate, "99/99/9999")
                                        ELSE "*".

         FIND FIRST RecIdOpDate WHERE RecIdOpDate.op EQ op.op
            NO-LOCK NO-ERROR.
         IF NOT AVAIL RecIdOpDate THEN
         DO:
            CREATE
               RecIdOpDate.
            ASSIGN
               RecIdOpDate.op      = op.op
               RecIdOpDate.DocDate = vDate
               .
         END.

         IF mRepDebit THEN
         DO:
            vDpr-id = INT(GetXattrValue("op",STRING(op.op),"dpr-id")).
            FIND FIRST sessions WHERE 
                       sessions.dpr-id EQ vDpr-id
               NO-LOCK NO-ERROR.
            IF NOT AVAIL sessions THEN
               NEXT.
            ASSIGN
               vAcctKas = op-entry.acct-db
               vAcctCor = op-entry.acct-cr
               vUser    = op.user-id WHEN mRazbIsp
               .
            IF vAcctCor EQ ? THEN
            DO:
               FIND FIRST xop-entry OF op WHERE 
                          xop-entry.acct-db EQ ? 
                      AND NOT CAN-DO(mRKRPol,xop-entry.acct-cr)
                  NO-LOCK NO-ERROR.
               IF AVAIL xop-entry THEN
                  vAcctCor = xop-entry.acct-cr.
            END.
         END.
         ELSE
         DO:
            ASSIGN
               vAcctKas = op-entry.acct-cr
               vAcctCor = op-entry.acct-db
               vUser    = op.user-inspector WHEN mRazbIsp
               .
            IF vAcctCor EQ ? THEN
            DO:
               FIND FIRST xop-entry OF op WHERE 
                          xop-entry.acct-cr EQ ? 
                      AND NOT CAN-DO(mRKROtr,xop-entry.acct-db)
                  NO-LOCK NO-ERROR.
               IF AVAIL xop-entry THEN
                  vAcctCor = xop-entry.acct-db.
            END.
         END.

         FIND FIRST acct-cor WHERE
                    acct-cor.acct       EQ vAcctCor
            NO-LOCK NO-ERROR.

         FIND FIRST acct-kas WHERE
                    acct-kas.acct       EQ vAcctKas
                AND acct-kas.contract   BEGINS "����"
                AND acct-kas.acct-cat   EQ "b"
            NO-LOCK NO-ERROR.
         IF     AVAIL acct-kas 
            AND NOT CAN-FIND(FIRST ttCasJour WHERE 
                                   ttCasJour.RecidOpEn EQ RECID(op-entry)) THEN
         DO:
            {acctread.i
                &bufacct=acct-kas
                &class-code= acct-kas.class-code
            }
            IF    NOT ({&user-rights})
               OR (IF mRepDebit THEN CAN-DO(mRKRPol,op-entry.acct-cr) 
                                ELSE CAN-DO(mRKROtr,op-entry.acct-db))
               OR CAN-DO(vAcctJourn,acct-kas.acct) THEN
               NEXT.
            FIND FIRST currency WHERE 
                       currency.currency EQ acct-kas.currency
               NO-LOCK NO-ERROR.

            IF acct-kas.currency EQ "" THEN
               IF mOtrVal        EQ "currency" THEN
                  vICurrency = "810".
               ELSE
                  vICurrency = mCodNacVal.
            ELSE
               vICurrency = GetBufferValue("currency","WHERE currency.currency EQ " + QUOTER(currency.currency),mOtrVal).

            vSymbol    = FRealSymbol (ROWID (op-entry), YES).
            vAcctKas   = STRING(acct-kas.acct,GetAcctFmt(acct-kas.acct-cat)).

            IF mRepDebit THEN
            DO:
               IF getTCodeFld("Val","��ᑨ�����",vSymbol,mCuDate) EQ "����" THEN
                  vSymbol = fGetSetting("��ᑨ�����","���-" + vSymbol,"").
            END.
            ELSE
            DO:
               IF getTCodeFld("Val","��ᑨ�����",vSymbol,mCuDate) EQ "���" THEN
                  vSymbol = fGetSetting("��ᑨ�����","���-" + vSymbol,"").
            END.    

            CREATE ttCasJour.
            ASSIGN
               mAgreeYes            = YES
               ttCasJour.acct-kas   = vAcctKas
               ttCasJour.doc-num    = op.doc-num
               ttCasJour.acct-cr    = IF mRepDebit THEN acct-cor.acct ELSE acct-kas.acct 
               ttCasJour.acct-db    = IF mRepDebit THEN acct-kas.acct ELSE acct-cor.acct 
               ttCasJour.acct-cor   = acct-cor.acct WHEN AVAIL acct-cor
               ttCasJour.amt-cur    = IF acct-kas.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur
               ttCasJour.symbol     = vSymbol
               ttCasJour.branch     = acct-kas.branch-id
               ttCasJour.RecidOpEn  = RECID(op-entry)
               ttCasJour.op         = op.op
               ttCasJour.user-id    = vUser
               vSymbol              = "" WHEN NOT mTotalCS
               ttCasJour.i-currency = vICurrency
               ttCasJour.DocDate    = vDate
               ttCasJour.Dpr-id     = INT64(vChDpr-id)
               ttCasJour.OpDate     = DATE(vDate)
               ttCasJour.acct-cat   = IF AVAIL acct-cor THEN acct-cor.acct-cat ELSE ""
               NO-ERROR.
            RUN GetCashDocTypeDigital IN h_op (BUFFER op-entry,
                                               acct-kas.acct,
                                               OUTPUT ttCasJour.digital).
            IF acct-kas.currency NE "" THEN
            DO:
               vRate = FindRateSimple("����",acct-kas.currency,mOpDate).
               ASSIGN
                  ttCasJour.amt-rub = IF mEkvKursOp THEN op-entry.amt-rub ELSE (vRate * op-entry.amt-cur).
            END.

            FIND FIRST ttCasJourItog WHERE 
                       ttCasJourItog.acct-kas EQ vAcctKas
                   AND ttCasJourItog.symbol   EQ vSymbol
                   AND ttCasJourItog.user-id  EQ vUser
               NO-LOCK NO-ERROR.
            IF NOT AVAIL ttCasJourItog THEN
            DO:
               CREATE ttCasJourItog.
               ASSIGN
                  ttCasJourItog.acct-kas = vAcctKas
                  ttCasJourItog.amt-cur  = ttCasJour.amt-cur
                  ttCasJourItog.amt-rub  = ttCasJour.amt-rub
                  ttCasJourItog.symbol   = vSymbol
                  ttCasJourItog.user-id  = vUser
                  .
            END.
            ELSE
               ASSIGN
                  ttCasJourItog.amt-cur  = ttCasJourItog.amt-cur + ttCasJour.amt-cur
                  ttCasJourItog.amt-rub  = ttCasJourItog.amt-rub + ttCasJour.amt-rub
                  .
         END.
      END. /* FOR EACH kau-entry */
   END. /* DO i = 1 */

END PROCEDURE. /* CreateTemp-Table */

FUNCTION FormatSumm CHAR (iSumm AS DEC):
   RETURN REPLACE(STRING(iSumm,">>,>>>,>>>,>>9.99"),".","-").
END FUNCTION.
/* $LINTUSER='BIS' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='26/11/2014 17:43:08.034+04:00' */
/* $LINTFILE='cashjour.p' */
/*prosignx2dDzBdmW3vBbLHXSSut2Q*/