/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: spr-cash318p.p
      Comment: ���⭠� �ࠢ�� 318-�
   Parameters: ���� ���㬥�⮢
         Uses:
      Used by:
      Created: 04.08.2008 20:28 elus
     Modified: 04.08.2008 20:28 elus
*/

{globals.i}
{wordwrap.def}
{intrface.get tparam}
{intrface.get sessions}
{intrface.get db2l}
{intrface.get vok}     /* �����㬥��� ��� ࠡ��� � ��쥪⠬� ��� pp-vok.p */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */

                        /* ���樠������ ����� ��⮪���஢����. */
RUN Init-SysMes IN h_tmess ("", "", "").

DEFINE INPUT PARAMETER iDocType AS CHARACTER NO-UNDO.

{svodord.def}

DEFINE VARIABLE mHeadDocType     AS CHARACTER NO-UNDO. /* ⨯� ���㬥�⮢ ����砥�� � 蠯�� */
DEFINE VARIABLE mBodyDocType     AS CHARACTER NO-UNDO. /* ⨯� ���㬥�⮢ ����砥�� � ⥫� */
DEFINE VARIABLE mChSetCodNacVal  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mChCodNacVal     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLines           AS CHARACTER NO-UNDO EXTENT 14. /* �����窠 */
DEFINE VARIABLE mCols            AS INT64     NO-UNDO. /* ��ਭ� ���� */
DEFINE VARIABLE mHeadAndFootCols AS INT64     NO-UNDO.
DEFINE VARIABLE mDate            AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOtstup          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSprCash0        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mPodschDocOtch   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mRecvType        AS CHARACTER NO-UNDO EXTENT 2.

IF NUM-ENTRIES(iDocType,";") >= 2 AND NUM-ENTRIES(iDocType,";") <= 4 THEN
   ASSIGN
      mHeadDocType = ENTRY(1,iDocType,";")
      mBodyDocType = ENTRY(2,iDocType,";")
      .
ELSE
DO:
   RUN Fill-SysMes("", "", 0, "�� ��୮� ������⢮ ��ࠬ��஢ ��楤��� ����.").
   RETURN.
END.

{ksh-defs.i}

DEFINE TEMP-TABLE ttSprCash NO-UNDO /* ����� ��� �ࠢ�� � ����筮�� */
   FIELD Acct     LIKE Acct.acct              /* ��楢�� ��� */
   FIELD Bal-acct LIKE Acct.bal-acct          /* C�� 2-�� ���浪� */
   FIELD Currency LIKE Currency.currency      /* ��� ������ */
   FIELD CurrName LIKE Currency.name-currenc  /* ������������ ������ */
   FIELD ICurrency LIKE Currency.i-currency   /* ISO-��� ������ */
   FIELD SummDb   AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* ��室 */
   FIELD QtyDb    AS INT64                  /* ���-�� ���㬥�⮢ � ��室� */
   FIELD SummCr   AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* ���室 */
   FIELD QtyCr    AS INT64                  /* ���-�� ���㬥�⮢ � ��室� */
   FIELD Balance  AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* ���⮪ �� ������ ��� */
   FIELD In-Bal   AS DECIMAL FORMAT "->>>,>>>,>>9.99" /* ���⮪ �� ������ �� */
   FIELD Dpr-Id   AS INTEGER                  /* ��� ᬥ�� */
   FIELD User-Id  AS CHARACTER                /* ���  User-� */
   FIELD UserName AS CHARACTER                /* ���  User-� */
   FIELD vChBlank AS CHARACTER
   FIELD RecvOnP  AS INT64

   FIELD DocDate  AS CHARACTER
   FIELD OpDate   AS DATE

   INDEX idxCurrency Currency
   INDEX idxAcct Acct
   INDEX idxU IS UNIQUE RecvOnP Acct Bal-acct Currency Dpr-Id User-Id vChBlank
                        DocDate
   .

&GLOBAL-DEFINE FLD1 ttSprCash.Dpr-Id
&GLOBAL-DEFINE BY-FLD1 BY {&FLD1}

&GLOBAL-DEFINE FLD2 ttSprCash.Currency
&GLOBAL-DEFINE BY-FLD2 BY {&FLD2}

&GLOBAL-DEFINE FLD5 ttSprCash.RecvOnP
&GLOBAL-DEFINE BY-FLD5 BY {&FLD5}

&GLOBAL-DEFINE FLD3 ttSprCash.User-Id
&GLOBAL-DEFINE BY-FLD3 BY {&FLD3}

&GLOBAL-DEFINE FLD4 ttSprCash.vChBlank
&GLOBAL-DEFINE BY-FLD4 BY {&FLD4}

{agr-beg.def 
   &NameTitle = "�������� �������"
   &TypeDoc   = "iDocType"
   &NameRep   = "'��瑯ࠢ'"}

ASSIGN
   mChSetCodNacVal = IF NUM-ENTRIES(iDocType,";") > 2 THEN ENTRY(3,iDocType,";") ELSE ?
   mChCodNacVal    = fGetSetting(mChSetCodNacVal,"","810") 
   mCodOurCur      = mChCodNacVal
   vOtstup         = IF NUM-ENTRIES(iDocType,";") > 3 THEN FILL(" ",INT(ENTRY(4,iDocType,";"))) ELSE ""   
   mSprCash0       = fGetSetting("spr-cash0","","��") EQ "��"
   mPodschDocOtch  = fGetSetting("����焮����","","���") EQ "��"
   NO-ERROR.

{agr-beg.i}

&SCOPED-DEFINE format-cur-name 16

ASSIGN
   mLines[ 1] = "    ��" + FILL("�",{&format-cur-name})                   + 
   "��������������������������������������������������������������������������"
    + "������������������������������Ŀ"
   mLines[ 2] = "    � " + PADC("������������",{&format-cur-name}) + 
   " �              ��室              �               ���室             �  "
    + "      �������, ���樠��        �"
   mLines[ 3] = "    � " + PADC("������",{&format-cur-name})                   + 
   " ���������������������������������������������������������������������Ĵ  "
    + "          � ������            �"
   mLines[ 4] = "    � " + FILL(" ",{&format-cur-name})                   + 
   " �������⢮�     �㬬� ��ࠬ�     �������⢮�     �㬬� ��ࠬ�     �  "
    + "        ��壠���᪨�          �"
   mLines[ 5] = "    � " + FILL(" ",{&format-cur-name})                   + 
   " � ���ᮢ�� �      � 㪠������      � ���ᮢ�� �      � 㪠������      �  "
    + "          ࠡ�⭨���           �"
   mLines[ 6] = "    � " + FILL(" ",{&format-cur-name})                   + 
   " ����㬥�⮢�  ������������ ������  ����㬥�⮢�  ������������ ������  �  "
    + "                               �"
   mLines[ 7] = "    ��" + FILL("�",{&format-cur-name})                   + 
   "��������������������������������������������������������������������������"
    + "������������������������������Ĵ"
   mLines[10] = "    � " + PADC("1",{&format-cur-name})                   + 
   " �     2    �           3           �     4    �           5           �  "
    + "               6               �"
   mLines[11] = "    ��" + FILL("�",{&format-cur-name})                   + 
   "��������������������������������������������������������������������������"
    + "������������������������������Ĵ"
   mLines[12] = "    ��" + FILL("�",{&format-cur-name})                   + 
   "��������������������������������������������������������������������������"
    + "��������������������������������"
   mLines[13] = "    ��" + FILL("�",{&format-cur-name})                   + 
   "��������������������������������������������������������������������������"
    + "������������������������������Ĵ"
   mLines[14] = "    ��" + FILL("�",{&format-cur-name})                   + 
   "��������������������������������������������������������������������������"
    + "������������������������������Ĵ"

   mCols            = LENGTH(vOtstup + mLines[1])
   mHeadAndFootCols = mCols - 6
   mRecvType[1] = "�� ���㬥�⠬, ��⠢����� �� �㬠���� ���⥫�:"
   mRecvType[2] = "�� ���㬥�⠬ � ���஭��� ����:"
   .
&GLOBAL-DEFINE cols mCols

RUN CreateTTRecIdOp.
RUN CreateTemp-Table.

/* ���⮪ �� ���� */

FOR EACH ttSprCash
NO-LOCK:
   {find-act.i &acct=ttSprCash.acct &curr=ttSprCash.currency}
   IF AVAILABLE acct THEN
      ttSprCash.Balance = IF acct.side EQ "�" THEN
                             (ttSprCash.In-Bal + ttSprCash.SummDb - ttSprCash.SummCr)
                          ELSE
                             (ttSprCash.In-Bal - ttSprCash.SummDb + ttSprCash.SummCr).
END.

{agr-end.i 
   &OnePageRep  = "YES"
   &OnePageName = "PrintSprCash"}

RUN End-SysMes IN h_tmess.

{intrface.del}

/* ========================== Procedure block ============================ */

PROCEDURE PrintSprCash.

   DEFINE VARIABLE vCodOurCur AS CHARACTER INITIAL "810" NO-UNDO. /* ��� ��樮���쭮� ������ */

   DEFINE BUFFER bttSprCash FOR ttSprCash.
   DEFINE BUFFER bTTSvodOrd FOR TTSvodOrd.

/* ��ࠬ���� �뢮�� �㬬� �ய���� */
   &GLOB   STR_SUM_NUM 12  /* ���-�� ��ப */
   DEFINE VARIABLE vIntStr AS CHARACTER /* 楫�� ���� �㬬� �ய���� */
      EXTENT {&STR_SUM_NUM} NO-UNDO.
   DEFINE VARIABLE vDecStr AS CHARACTER FORMAT "x(2)" NO-UNDO. /* �஡��� ���� �㬬� �ய���� */
   DEFINE VARIABLE vQtyDb  AS INT64 NO-UNDO.
   DEFINE VARIABLE vQtyCr  AS INT64 NO-UNDO.
   DEFINE VARIABLE vRepDate AS CHARACTER NO-UNDO.

   FOR EACH ttSprCash NO-LOCK:
      IF NOT CAN-DO(mUsDprIdLst,STRING(ttSprCash.Dpr-Id)) THEN
         DELETE ttSprCash.
   END.

   FOR EACH bttSprCash
      NO-LOCK
      BREAK BY bttSprCash.OpDate:
      IF FIRST-OF(bttSprCash.OpDate) THEN
      DO:
         vRepDate = IF {assigned mDateOtc} THEN bttSprCash.DocDate
                                           ELSE mChrDate. 
      /* ========================== ��������� ����� =========================== */
         {head-318p.i
            &CodForm = "'0402112'"
         }
         {orgname318p.i
            &CurBranchName = mBranchName
         }
      
         PUT UNFORMATTED
            PADC("�������� �������",{&Cols}) SKIP
            PADC(vRepDate          ,{&Cols}) SKIP(1).
      /* ������塞 ����祭� ��� ᮢ��襭�� ����権 */
         PUT UNFORMATTED 
            vOtstup SPACE(3) "����祭� ��� ᮢ��襭�� ����権 � �㬬� "  SKIP
            vOtstup SPACE(3) "(�㬬��) � 㪠������ ������������ ������:" SKIP
            SKIP(1). 
         
         FOR EACH ttSprCash WHERE ttSprCash.DocDate EQ bttSprCash.DocDate 
            NO-LOCK
            BREAK {&BY-FLD4} {&BY-FLD2}:
      
            ACCUMULATE 
               ttSprCash.In-Bal (TOTAL {&BY-FLD2})
               .
            IF LAST-OF({&FLD2}) THEN
            DO:
               /* �������� ��ப �㬬� �ய���� */
               RUN x-amtstr.p (ABSOLUTE(ACCUM TOTAL  {&BY-FLD2} ttSprCash.In-Bal),
                               ttSprCash.Currency,
                               YES,
                               YES,
                               OUTPUT vIntStr[1],
                               OUTPUT vDecStr).
              
               /* ����� '' ���⠥� ��� ���. ������ */
               IF ttSprCash.Currency EQ "" THEN
                  vCodOurCur = mCodOurCur.
               ELSE
                  vCodOurCur = ttSprCash.Currency.
              
               /* ����ࠥ� � ���� ��ப� ��, �� �⨬ �뢥�� �� ����� */
               vIntStr[1] = STRING(ttSprCash.CurrName,"x({&format-cur-name})") + " " + 
                            STRING(ACCUM TOTAL {&BY-FLD2} 
                            ttSprCash.In-Bal,"->,>>>,>>>,>>9.99") +
                            " " + ttSprCash.icurrency + " / " + 
                            vIntStr[1] + " " + vDecStr.
              
               {wordwrap.i
                 &s = vIntStr
                 &n = {&STR_SUM_NUM}
                 &l = mHeadAndFootCols
               }
               DO i = 1 TO {&STR_SUM_NUM}:
                  IF vIntStr [i] NE "" THEN
                     PUT UNFORMATTED vOtstup SPACE(3) vIntStr [i] SKIP.
               END.
               PUT UNFORMATTED vOtstup SPACE(3) FILL("�", mHeadAndFootCols) SKIP.
            END.
         END. /* FOR EACH ttSprCash */
      
         PUT UNFORMATTED PADC("(��ࠬ� � �ய����)",{&Cols}) SKIP(3).
      
      
      /* ========================== ����� ⠡��窨 ============================= */
         DO i = 1 TO 10:
            IF mLines[i] NE "" THEN
            PUT UNFORMATTED vOtstup mLines[i] SKIP.
         END.

         /* ������塞 �ࠢ�� */
         FOR EACH ttSprCash WHERE 
                  ttSprCash.DocDate   EQ bttSprCash.DocDate
                  AND (   ttSprCash.QtyDb NE 0
                   OR ttSprCash.QtyCr NE 0
                   OR mSprCash0 
				   OR NOT mPodschDocOtch )
            NO-LOCK
            BREAK {&BY-FLD5} {&BY-FLD4} {&BY-FLD1} {&BY-FLD2} {&BY-FLD3}:
            IF FIRST-OF( {&FLD5} ) THEN
               PUT UNFORMATTED
                  vOtstup mLines[13] SKIP 
                  vOtstup "    � " PADR(mRecvType[{&FLD5}],LENGTH(mLines[1]) - 7) 
                  "�" SKIP
                  vOtstup mLines[14] SKIP.
            ACCUMULATE
               ttSprCash.QtyDb  (TOTAL {&BY-FLD3})
               ttSprCash.SummDb (TOTAL {&BY-FLD3})
               ttSprCash.QtyCr  (TOTAL {&BY-FLD3})
               ttSprCash.SummCr (TOTAL {&BY-FLD3})
               .
            IF LAST-OF({&FLD3}) THEN
            DO:
               /* ����� '' ���⠥� ��� ���. ������ */
               IF ttSprCash.Currency EQ "" THEN
                  vCodOurCur = mCodOurCur.
               ELSE
                  vCodOurCur = ttSprCash.Currency.

               vQtyDb = (ACCUM TOTAL {&BY-FLD3} ttSprCash.QtyDb).
               vQtyCr = (ACCUM TOTAL {&BY-FLD3} ttSprCash.QtyCr).

               IF mCashOrd THEN
               DO:
                  {empty RecIdOp}
                  FOR EACH RecIdOpDate WHERE 
                           RecIdOpDate.DocDate EQ bttSprCash.DocDate
                     NO-LOCK:
                     CREATE
                        RecIdOp
                        .
                     ASSIGN
                        RecIdOp.op = RecIdOpDate.op 
                        .
                  END.
                  RUN getcashtt.p(TABLE RecIdOp,OUTPUT TABLE TTSvodOrd,mUsDprIDLst).
				  TTSvod:
                  FOR EACH TTSvodOrd WHERE 
                           TTSvodOrd.DocCur EQ ttSprCash.currency
                     NO-LOCK:
                     IF TTSvodOrd.OrdType EQ "��室��" THEN
                        vQtyDb = vQtyDb - TTSvodOrd.qty.
                     ELSE
                     DO:
                        IF     mPodschDocOtch 
                           AND CAN-FIND(FIRST bTTSvodOrd WHERE 
                                              bTTSvodOrd.OrdType EQ "��室��"
                                          AND bTTSvodOrd.OpLst   EQ TTSvodOrd.OpLst) THEN
                           NEXT TTSvod.
                        vQtyCr = vQtyCr - TTSvodOrd.qty.
                     END.
                  END.
               END.
      
               IF NOT FIRST-OF( {&FLD5} ) THEN 
                  PUT UNFORMATTED vOtstup mLines[11] SKIP.              
               PUT UNFORMATTED vOtstup "    � " ttSprCash.CurrName FORMAT "x({&format-cur-name})"  " � "
                  vQtyDb                                      FORMAT ">>>>>>>9" " � "
                  (ACCUM TOTAL {&BY-FLD3} ttSprCash.SummDb)   
                  FORMAT "->>>>>,>>>,>>9.99" " " ttSprCash.icurrency " � " 
                  vQtyCr                                      FORMAT ">>>>>>>9" " � "
                  (ACCUM TOTAL {&BY-FLD3} ttSprCash.SummCr)   
                  FORMAT "->>>>>,>>>,>>9.99" " " ttSprCash.icurrency " � "
                  STRING(IF ttSprCash.user-id NE "" THEN GetUserName(ttSprCash.User-Id) ELSE "","x(31)")  " � " SKIP.
            END.
         END. /* FOR EACH ttSprCash */
      
         PUT UNFORMATTED 
            vOtstup mLines[12] SKIP(2)
            vOtstup SPACE(3) "���⮪ �� ����� ��� � �㬬� " SKIP
            vOtstup SPACE(3) "(�㬬��) � 㪠������ ������������ ������: " 
            SKIP(1).
      /* ������塞 ���⮪ �� ����� ��� */
         FOR EACH ttSprCash WHERE ttSprCash.DocDate EQ bttSprCash.DocDate
            NO-LOCK
            BREAK {&BY-FLD4} {&BY-FLD2}:
            ACCUMULATE 
               ttSprCash.Balance (TOTAL {&BY-FLD2})
               .
            IF LAST-OF({&FLD2}) THEN
            DO:
               /* �������� ��ப �㬬� �ய���� */ 
               RUN x-amtstr.p (ABSOLUTE(ACCUM TOTAL {&BY-FLD2} ttSprCash.Balance), 
                               ttSprCash.Currency, 
                               YES, 
                               YES,
                               OUTPUT vIntStr[1],
                               OUTPUT vDecStr).
              
               /* ����� '' ���⠥� ��� ���. ������ */
               IF ttSprCash.Currency EQ "" THEN
                  vCodOurCur = mCodOurCur.
               ELSE
                  vCodOurCur = ttSprCash.Currency.
      
               /* ����ࠥ� � ���� ��ப� ��, �� �⨬ �뢥�� �� ����� */
               vIntStr[1] = STRING(ttSprCash.CurrName,"x({&format-cur-name})") + " " + 
                            STRING(ACCUM TOTAL {&BY-FLD2} 
                            ttSprCash.Balance,"->,>>>,>>>,>>9.99") +
                            " " + ttSprCash.icurrency + " / " + 
                            vIntStr[1] + " " + vDecStr.
      
               {wordwrap.i
                 &s = vIntStr
                 &n = {&STR_SUM_NUM}
                 &l = mHeadAndFootCols
               }
               DO i = 1 TO {&STR_SUM_NUM}:
                  IF vIntStr [i] NE "" THEN
                     PUT UNFORMATTED vOtstup SPACE(3) vIntStr [i] SKIP.
               END.
               PUT UNFORMATTED vOtstup SPACE(3) FILL("�", mHeadAndFootCols) SKIP.
            END.
         END. /* FOR EACH ttSprCash */
         
         PUT UNFORMATTED PADC("(��ࠬ� � �ய����)",{&Cols}) SKIP(3).
         RUN GetRepFioByRef(ENTRY(1,PROGRAM-NAME(2), "."),mCuBrchID,?,mCuDprID).
      
         RUN PrintFioAndPost(mFioInRep[1],mPostInRep[1],4).
         RUN PrintFioAndPost(mFioInRep[2],mPostInRep[2],4).
      END.      
   END.
END PROCEDURE. /* PrintSprCash */

PROCEDURE CreateTTRecIdOp:

   DEFINE VARIABLE i         AS INT64     NO-UNDO.
   DEFINE VARIABLE vChDpr-Id AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpTime   AS INT64     NO-UNDO.
   DEFINE VARIABLE vOpDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE vDate     AS CHARACTER NO-UNDO.

   DO i = 1 TO NUM-ENTRIES(mUsDprIDLst):
      vChDpr-Id = ENTRY(i,mUsDprIDLst).
      FOR EACH kau-entry WHERE kau-entry.kau       EQ vChDpr-Id
                           AND kau-entry.kau-id    BEGINS "��������" 
                           AND kau-entry.op-status GE CHR(251)
                           AND kau-entry.op-date   EQ mCuDate
      NO-LOCK,
      FIRST op OF kau-entry
      NO-LOCK
      BREAK BY kau-entry.op:
         IF FIRST-OF(kau-entry.op) THEN
         DO:
            RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
            
            CASE mDateOtc:
               WHEN "" THEN vDate = "*".
               WHEN "*" THEN
               DO:
                  vDate = STRING(vOpDate, "99/99/9999").
                  IF NOT CAN-DO(mDateLst, vDate) THEN NEXT.            
               END.
               OTHERWISE
               DO:
                  vDate = STRING(vOpDate, "99/99/9999").
                  IF mDateOtc NE vDate THEN NEXT.
               END.
            END CASE.            
            
            FIND FIRST RecIdOpDate WHERE RecIdOpDate.op EQ op.op 
               NO-LOCK NO-ERROR.
            IF NOT AVAILABLE RecIdOpDate THEN
            DO:
               CREATE RecIdOpDate.
               ASSIGN
                  RecIdOpDate.op      = op.op
                  RecIdOpDate.DocDate = vDate
                  .
            END. /* IF NOT AVAILABLE RecIdOp */
         END.   
      END. /* FOR EACH kau-entry */
   END. /* DO i = 1 */
END PROCEDURE. /* CreateTTRecIdOp */

FUNCTION CharFTC RETURNS CHARACTER (iAcct     AS CHARACTER,
                                    iCurrency AS CHARACTER):

   DEFINE VARIABLE vChFTC     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChAcctFTC AS CHARACTER NO-UNDO.
   DEFINE BUFFER currency FOR currency.

   vChAcctFTC = GetXAttrValue("acct", iAcct + "," + iCurrency,"form-type-code").
   IF vChAcctFTC NE "" THEN
   DO:
   FIND FIRST currency WHERE currency.currency EQ iCurrency
      NO-LOCK NO-ERROR.
   vChFTC     = GetCodeName("��������������",vChAcctFTC).

   /* �᫨ �� ��諨, � �饬 � sec-code (��஦�� 祪�) */
   IF NOT {assigned vChFTC} THEN
      vChFTC = GetSecCodeName(vChAcctFTC).
   IF (NOT {assigned vChFTC} OR vChFTC = "?") THEN
      vChFTC = "".
   ELSE
      vChFTC = (IF AVAIL currency THEN currency.name-currenc + "/�� " ELSE "") + vChFTC.
   END.      
   RETURN vChFTC.

END FUNCTION. /* CharFTC */

PROCEDURE CreateTemp-Table:

   DEFINE VARIABLE vFlagOpDb     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vFlagOpCr     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vFlagOp       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vIntDpr-Id    AS INT64   NO-UNDO.
   DEFINE VARIABLE vChFTC        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChAcctFTC    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vUsDprID      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE i             AS INT64   NO-UNDO.
   DEFINE VARIABLE vLogPereschet AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vByUserId     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSesUserId    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDate         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vRecvOnPaper  AS INT64     NO-UNDO.
   DEFINE BUFFER bsigns FOR signs.

   FOR EACH RecIdOpDate:
      /* ����� */
      CASE mDateOtc:
         WHEN "" THEN vDate = "*".
         WHEN "*" THEN
         DO:
            vDate = RecIdOpDate.DocDate.
            IF NOT CAN-DO(mDateLst, vDate) THEN NEXT.
         END.
         OTHERWISE
         DO:
            vDate = RecIdOpDate.DocDate.
            IF mDateOtc NE vDate THEN NEXT.
         END.
      END CASE.
      
      FOR FIRST op WHERE op.op EQ RecIdOpDate.op
                     AND CAN-DO(mHeadDocType, op.doc-type)
      NO-LOCK,
      FIRST signs WHERE signs.file-name EQ "op" 
                    AND signs.surrogate EQ STRING(op.op)     
                    AND signs.code      EQ "Dpr-Id"
                    AND signs.code-val  NE ""
      NO-LOCK,
      EACH kau-entry OF op WHERE /*kau-entry.debit EQ YES     sku �㦭� ��室�� ����樨
                             AND*/ kau-entry.kau EQ (IF NOT CAN-DO(mUsDprIDLst,TRIM(signs.code-val)) THEN 
                                                     mUsDprIDLst 
                                                  ELSE 
                                                     TRIM(signs.code-val)
                                                  )
                             AND kau-entry.op-status GE CHR(251)
      NO-LOCK BY kau-entry.op:
		FIND FIRST bsigns WHERE bsigns.file-name EQ 'sessions' AND
					bsigns.surrogate EQ kau-entry.kau AND 
					bsigns.code eq '������ᮩ' AND
					bsigns.code-val EQ '��' 
			NO-LOCK NO-ERROR.
		/* sku �᫨ ᬥ�� ���.�����, � �� ���뢠�� �ਥ� �� ���稭. ����� ����� ��� */	
		IF AVAIL bsigns AND kau-entry.op-date = op.contract-date AND kau-entry.debit EQ YES THEN NEXT.
		/* �᫨ ᬥ�� ���.����� � ��室�� ����樨 ���뢠�� ⮫쪮 ����� ��� ��㣮�� ������ */
		IF kau-entry.debit EQ NO AND 
		(AVAIL bsigns AND kau-entry.debit EQ NO AND kau-entry.op-date EQ op.contract-date) EQ NO
		THEN NEXT.
		
         ASSIGN
            mAgreeYes = TRUE
            vChFTC    = ?
            .
         
         {find-act.i &acct=kau-entry.acct &curr=kau-entry.currency}
         IF AVAILABLE acct THEN
         DO:
            vRecvOnPaper = INT64(GetXAttrValue("op",STRING(op.op),
                                 "���ᮡ�����") NE "") + 1.
            vChFTC = CharFTC(acct.acct,acct.currency).
            IF NOT (vChFTC NE "" AND NOT mChBlank) THEN
            DO:
               FIND FIRST currency WHERE 
                          currency.currency EQ kau-entry.currency
               NO-LOCK NO-ERROR.
               IF AVAIL currency THEN
               DO:
                  vIntDpr-Id = INT64(kau-entry.kau).
                  IF mByInsp THEN
                  DO:
                     vByUserId  = "".
                     vSesUserId = GetBufferValue("sessions","WHERE sessions.dpr-id EQ " + GetXattrValue("op",STRING(op.op),"dpr-id"),"user-id").
					 /*ayv ��������, �⮡� ��������� �����*/
					 vByUserId  = op.user-id.
                     /*IF vSesUserId NE op.user-id THEN
                        vByUserId  = op.user-id.
                     IF vSesUserId NE op.user-inspector THEN
                        vByUserId  = op.user-inspector.*/
					 IF vSesUserId NE op.user-inspector and op.user-inspector NE "" THEN
			            vByUserId  = op.user-inspector.
						else vByUserId  = op.user-id .
					/**/
                  END.
                  ELSE
                  DO:
                     vByUserId  = /*IF mChBlank THEN*/ op.user-id
                                              /*ELSE ""*/.
                  END.
                  FIND FIRST ttSprCash WHERE ttSprCash.Acct     EQ acct.acct  
                                         AND ttSprCash.Currency EQ acct.currency
                                         AND ttSprCash.Dpr-Id   EQ vIntDpr-Id
                                         AND ttSprCash.User-Id  EQ vByUserId 
                                         AND ttSprCash.vChBlank EQ vChFTC
                                         AND ttSprCash.DocDate  EQ vDate
                                         AND ttSprCash.RecvOnP  EQ vRecvOnPaper
                     NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE ttSprCash THEN
                  DO:
                     CREATE ttSprCash.
                     ASSIGN
                        ttSprCash.Acct     = acct.acct 
                        ttSprCash.Bal-acct = acct.bal-acct 
                        ttSprCash.Currency = currency.currency 
                        ttSprCash.RecvOnP  = vRecvOnPaper  
                        ttSprCash.CurrName = IF {assigned vChFTC} AND vChFTC NE "?" THEN 
                                                vChFTC
                                             ELSE 
                                                currency.name-currenc
                        ttSprCash.iCurrency = currency.i-currency
                     /* ttSprCash.In-Bal   = IF currency.currency EQ "" THEN 
                                                kau-entry.amt-rub
                                             ELSE
                                                kau-entry.amt-cur */
					 /* ayv ��ࠢ���� � ᮮ⢥��⢨� � ��ன ���ᨥ�, ��� ��୮�� ������ 蠯�� � ᮮ⢥��⢨� � ��।�祩 ������� ����� ����ࠬ� */
						ttSprCash.In-Bal   = (IF currency.currency EQ "" THEN 
                                                kau-entry.amt-rub
                                             ELSE
                                                kau-entry.amt-cur) * (IF kau-entry.debit EQ YES THEN 1 ELSE -1)
					 /**/
                        ttSprCash.Dpr-Id   = INT64(kau-entry.kau) 
                        ttSprCash.User-Id  = vByUserId 
                        ttSprCash.vChBlank = vChFTC
                        ttSprCash.DocDate  = vDate
                        ttSprCash.OpDate   = DATE(vDate)
                        NO-ERROR.
                  END.
                  ELSE
                  DO:
                     ASSIGN
                      /*ttSprCash.In-Bal   = ttSprCash.In-Bal + (IF currency.currency EQ "" THEN 
                                                                    kau-entry.amt-rub
                                                                 ELSE
                                                                    kau-entry.amt-cur) */
					  /* ayv ��ࠢ���� � ᮮ⢥��⢨� � ��ன ���ᨥ�, ��� ��୮�� ������ 蠯�� � ᮮ⢥��⢨� � ��।�祩 ������� ����� ����ࠬ� */
						ttSprCash.In-Bal   = ttSprCash.In-Bal + (IF currency.currency EQ "" THEN 
                                                                    kau-entry.amt-rub
                                                                 ELSE
                                                                    kau-entry.amt-cur) * (IF kau-entry.debit EQ YES THEN 1 ELSE -1)
					  /* ayv */
                        .
                  END. /* IF NOT AVAILABLE ttSprCash */
               END. /* IF AVAIL curreny */
            END. /* IF NOT (vChFTC NE "" AND NOT mChBlank) */
         END. /* IF AVAIL acct */
      END. /* FOR EACH op */

      /* ⥫� */
      FOR FIRST op WHERE 
                op.op EQ RecIdOpDate.op
            AND CAN-DO(mBodyDocType, op.doc-type)
         NO-LOCK,
      FIRST signs WHERE signs.file-name EQ "op" 
                    AND signs.surrogate EQ STRING(op.op)     
                    AND signs.code      EQ "Dpr-Id"
                    AND signs.code-val  NE ""
         NO-LOCK,
      EACH kau-entry OF op WHERE kau-entry.debit EQ (IF CAN-DO(mHeadDocType, op.doc-type) THEN
                                                        NO
                                                     ELSE
                                                        kau-entry.debit
                                                    )
                             AND kau-entry.kau EQ (IF NOT CAN-DO(mUsDprIDLst,TRIM(signs.code-val)) THEN 
                                                      kau-entry.kau
                                                   ELSE 
                                                      TRIM(signs.code-val)
                                                  )
                             AND kau-entry.op-status GE CHR(251)
         NO-LOCK 
      BREAK BY kau-entry.op:
      
         IF NOT Pereschet(op.op,kau-entry.op-entry,kau-entry.debit) THEN
         DO:
            /* ��� ������ ������⢠ ���㬥�⮢ */
            IF FIRST-OF(kau-entry.op) OR NOT mPodschKolDoc THEN 
               ASSIGN
                  vFlagOpDb = YES
                  vFlagOpCr = YES
				  vFlagOp   = YES
               .
            ASSIGN
               mAgreeYes = TRUE /* ����� ��� ���� ���� */
               vChFTC    = ?
               .

            {find-act.i &acct=kau-entry.acct &curr=kau-entry.currency}
            
            IF AVAILABLE acct THEN
            DO:
               vRecvOnPaper = INT64(GetXAttrValue("op",STRING(op.op),
                                    "���ᮡ�����") NE "") + 1.
               vChFTC = CharFTC(acct.acct,acct.currency).
               IF NOT (vChFTC NE "" AND NOT mChBlank) THEN /* �᫨ �� ��, � �ࠢ�� ��� ���, � �ய�᪠�� */
               DO:
                  FIND FIRST currency WHERE 
                             currency.currency EQ kau-entry.currency
                     NO-LOCK NO-ERROR.
                  IF AVAIL currency THEN
                  DO:
                     vIntDpr-Id = INT64(kau-entry.kau).
                     IF mByInsp THEN
                     DO:
                        vByUserId  = "".
                        vSesUserId = GetBufferValue("sessions","WHERE sessions.dpr-id EQ " + GetXattrValue("op",STRING(op.op),"dpr-id"),"user-id").
						/*ayv ��������, �⮡� ��������� �����*/
						vByUserId  = op.user-id.
						/*IF vSesUserId NE op.user-id THEN
							vByUserId  = op.user-id.
						IF vSesUserId NE op.user-inspector THEN
							vByUserId  = op.user-inspector.*/
						IF vSesUserId NE op.user-inspector and op.user-inspector NE "" THEN
							vByUserId  = op.user-inspector.
							else vByUserId  = op.user-id .
						/**/
					 END.
                     ELSE
                     DO:
                        vByUserId  = /*IF mChBlank THEN*/ op.user-id
                                                 /*ELSE ""*/.
                     END.
                     FIND FIRST ttSprCash WHERE ttSprCash.Acct     EQ acct.acct
                                            AND ttSprCash.Currency EQ acct.currency
                                            AND ttSprCash.Dpr-Id   EQ vIntDpr-Id
                                            AND ttSprCash.User-Id  EQ vByUserId
                                            AND ttSprCash.vChBlank EQ vChFTC
                                            AND ttSprCash.DocDate  EQ vDate
                                            AND ttSprCash.RecvOnP  EQ 
                                                vRecvOnPaper
                        NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE ttSprCash THEN
                     DO:
                        CREATE ttSprCash.
                        ASSIGN
                           ttSprCash.Acct     = acct.acct
                           ttSprCash.Bal-acct = acct.bal-acct
                           ttSprCash.Currency = currency.currency
                           ttSprCash.iCurrency = currency.i-currency
                           ttSprCash.RecvOnP   = vRecvOnPaper  
                           ttSprCash.CurrName = IF {assigned vChFTC} AND vChFTC NE "?" THEN
                                                   vChFTC
                                                ELSE
                                                   currency.name-currenc
                           ttSprCash.SummDb   = IF kau-entry.debit EQ YES THEN
                                                   (IF currency.currency EQ "" THEN
                                                       kau-entry.amt-rub
                                                    ELSE
                                                       kau-entry.amt-cur)
                                                ELSE
                                                   ttSprCash.SummDb
                           ttSprCash.SummCr   = IF kau-entry.debit EQ NO THEN
                                                   (IF currency.currency EQ "" THEN
                                                       kau-entry.amt-rub
                                                    ELSE
                                                       kau-entry.amt-cur)
                                                 ELSE
                                                    ttSprCash.SummCr
                           ttSprCash.QtyDb    = IF kau-entry.debit EQ YES AND vFlagOpDb THEN 1
                                                                                        ELSE 0
                           ttSprCash.QtyCr    = IF kau-entry.debit EQ NO AND vFlagOpCr  THEN 1
                                                                                        ELSE 0
                           ttSprCash.Dpr-Id   = INT64(kau-entry.kau) 
                           ttSprCash.User-Id  = vByUserId
                           ttSprCash.vChBlank = vChFTC
                           ttSprCash.DocDate  = vDate
                           ttSprCash.opDate   = DATE(vDate)
                           NO-ERROR.
                     END.
                     ELSE
                     DO:
                        ASSIGN
                           ttSprCash.SummDb   = IF kau-entry.debit THEN
                                                   (IF currency.currency EQ "" THEN
                                                       (ttSprCash.SummDb + kau-entry.amt-rub)
                                                    ELSE
                                                       (ttSprCash.SummDb + kau-entry.amt-cur))
                                                ELSE
                                                   ttSprCash.SummDb
                           ttSprCash.SummCr   = IF kau-entry.debit EQ NO THEN
                                                   (IF currency.currency EQ "" THEN
                                                       ttSprCash.SummCr + kau-entry.amt-rub
                                                    ELSE
                                                       ttSprCash.SummCr + kau-entry.amt-cur)
                                                ELSE
                                                   ttSprCash.SummCr
                           ttSprCash.QtyDb    = IF kau-entry.debit AND vFlagOpDb THEN ttSprCash.QtyDb + 1
                                                                                 ELSE ttSprCash.QtyDb
                           ttSprCash.QtyCr    = IF kau-entry.debit EQ NO AND vFlagOpCr THEN ttSprCash.QtyCr + 1
                                                                                       ELSE ttSprCash.QtyCr
                           .
                     END. /* IF NOT AVAILABLE ttSprCash */
                     IF kau-entry.debit     AND vFlagOpDb THEN vFlagOpDb = NO.
                     IF NOT kau-entry.debit AND vFlagOpCr THEN vFlagOpCr = NO.
					 IF     NOT vFlagOpCr 
                        AND NOT vFlagOpDb 
                        AND mPodschDocOtch
                        AND vFlagOp THEN
                        ASSIGN
                           ttSprCash.QtyCr = ttSprCash.QtyCr - 1
                           vFlagOp         = NO
                        .
                  END. /* IF AVAIL currency */
               END. /* IF NOT (vChFTC NE "" AND NOT mChBlank) */
            END. /* IF AVAIL acct */
         END. /* IF NOT vLogPereschet */
      END. /* FOR EACH op */
   END. /* FOR EACH RecIdOpDate */
END PROCEDURE. /* CreateTemp-Table */
/* $LINTUSER='STRE' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='22/10/2014 12:08:37.000+04:00' */
/* $LINTFILE='spr-cash318p.p' */
/*prosignONK4up3X61N1YPsLyeuVcQ*/