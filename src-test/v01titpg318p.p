/*          
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: V-TIT-PG318P.P
      Comment: ������ ���� ���ᮢ�� ���㬥�⮢ ���.
   Parameters: ���� ���㬥�⮢, �����
         Uses:
      Created: 04.10.2004 13:00 fedm     35751
     Modified:
*/

DEFINE INPUT PARAMETER iParams AS CHARACTER NO-UNDO.

{globals.i}
{intrface.get tparam}
{intrface.get sessions}
{intrface.get vok}
{intrface.get strng}
{intrface.get instrum}  /* ������⥪� ��� ࠡ��� � 䨭. �����㬥�⠬�. */
{svodord.def}

DEFINE VARIABLE mCols       AS INT64   NO-UNDO.
DEFINE VARIABLE mIntSh      AS INT64   NO-UNDO.
DEFINE VARIABLE mShLines    AS CHARACTER NO-UNDO  EXTENT 17.
DEFINE VARIABLE mLShLines   AS CHARACTER NO-UNDO  EXTENT 17.
DEFINE VARIABLE mFShLines   AS CHARACTER NO-UNDO  EXTENT 17.
DEFINE VARIABLE mLastSH     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mIntLastSH  AS INT64   NO-UNDO.
DEFINE VARIABLE mNumSh      AS CHARACTER NO-UNDO  EXTENT 21.
DEFINE VARIABLE mMaxSh      AS INT64   NO-UNDO.
DEFINE VARIABLE mTitDocPer  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocType1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocType2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTitVneb    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mByOperRate AS LOGICAL   NO-UNDO INIT YES.
DEFINE VARIABLE mOtstup     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNacCur     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIndexCase  AS CHARACTER NO-UNDO.
/* ������������ ࠧ����� ���� */
DEFINE VARIABLE mNames     AS CHARACTER NO-UNDO EXTENT 6 INITIAL 
   [ " ��室��, ",
     " ���室��, ",
     " �� ��室�, ",
     " �� ��室�, ",
     " �� ������,  ",
     " �� �।���, "
   ].

DEFINE VARIABLE mHNames    AS CHARACTER NO-UNDO EXTENT 3 INITIAL 
   [ " ���ᮢ� ���㬥��� ",
     " ���ᮢ� �थ� 0401106 ",
     " �थ� �� ��।�� 業���⥩ 0402102 "
   ].

FUNCTION GetRepSection RETURN INT64 (iAcct-cat AS CHARACTER,
                                     iDebit    AS LOGICAL,
                                     iDoc-type AS CHARACTER) FORWARD.

IF NUM-ENTRIES(iParams,";") > 2 THEN
   mOtstup = FILL(" ",INT64(ENTRY(3,iParams,";"))) NO-ERROR.

IF NUM-ENTRIES(iParams,";") > 3 THEN
   mNacCur = ENTRY(4,iParams,";").
ELSE
   mNacCur = "RUB".

IF NUM-ENTRIES(iParams,";") > 4 THEN
   mByOperRate = ENTRY(5,iParams,";") EQ "��������".

IF NUM-ENTRIES(iParams,";") > 1 THEN
   ASSIGN
      mDocType1 = ENTRY(1,iParams,";")
      mDocType2 = ENTRY(2,iParams,";")
   .
ELSE
   ASSIGN
      mDocType1 = "!03*,*"
      mDocType2 = "03*"
   .

ASSIGN
   mTitDocPer = FGetSetting("��℮����","","")
   mTitVneb   = FGetSetting("��₭��","","") EQ "��"
   .

IF NOT mTitVneb THEN
   ASSIGN
      mNames[5] = ""
      mNames[6] = ""
   .

{wordwrap.def}
{agr-beg.def
   &TypeDoc   = '"book"'
   &NameTitle = "��������� ���� �������� ���������� ���(318-�)"
   &NameRep   = '"��⋨��"'
}
{agr-beg.i}

mPodschKolDoc = mPodschKolDoc AND NOT mMoreTit.

/* ��ਭ� ���� */
IF mNameCurr THEN
   mCols = IF mMoreTit THEN 158 ELSE 72.
ELSE
   mCols = IF mMoreTit THEN 193 ELSE 79.
mCols = mCols + LENGTH(mOtstup).
&GLOBAL-DEFINE cols mCols

{agr-end.i
   &OnePageName = PrintRep
}

{intrface.del}

/* �६����� ⠡��� */
DEFINE TEMP-TABLE ttRep NO-UNDO
   FIELD type     AS INT64       /* ������ */
   FIELD currency AS CHAR      /* ����� */
   FIELD qty      AS INT64     EXTENT 21 /* ���-�� */
   FIELD amt      AS DECIMAL EXTENT 21 /* �㬬� � �㡫�� */
   FIELD OpDate   AS DATE
INDEX type-curr type currency.

DEFINE TEMP-TABLE ttRepOp NO-UNDO
   FIELD type     AS INT64       /* ������ */
   FIELD currency AS CHAR      /* ����� */
   FIELD qty      AS INT64
   FIELD amt      AS DECIMAL
   FIELD op       AS INT64
   FIELD ext      AS INT64
   FIELD OpDate   AS DATE
   FIELD Acct     AS Char
   FIELD doc-num  AS Char
   FIELD user-id  AS Char
   FIELD doc-type AS Char
INDEX type-curr type currency op
.

DEFINE TEMP-TABLE ttRecIdOp1 NO-UNDO LIKE RecIdOp
   FIELD type AS INT64

   INDEX idx-op op
.

DEFINE TEMP-TABLE TTSvodOrd1 NO-UNDO LIKE ttSvodOrd
   FIELD RepPart AS INT64

   INDEX idxType RepPart
.

DEFINE BUFFER bttRepOp  FOR ttRepOp.


/* ��楤�� �ନ஢���� ����� */
PROCEDURE PrintRep:
   
   DEFINE BUFFER bttRep FOR ttRep.
   
   /* ���������� �६����� ⠡���� */
   RUN CreateTT.
   RUN CreateTT_SH.   
   
   FOR EACH ttRep WHERE 
            ttRep.qty[1] = 0
        AND ttRep.amt[1] = 0
      EXCLUSIVE-LOCK:
      DELETE ttRep.
   END.
/*
output to "ttrep.txt".
   FOR EACH ttRep.
export ttrep.
   END.
output close.
*/   
   FOR EACH bttRep NO-LOCK
      BREAK BY bttRep.OpDate:
      IF FIRST-OF(bttRep.OpDate) THEN
      DO:
         /* ��������� ����� */
         {head-318p.i
            &Otstup  = 60
            &CodForm = "'0402433'"
         }
         mIndexCase = GetXattrValueEX("branch", STRING(mCuBrchID), "�����ᄥ��", "").
         PUT UNFORMATTED
            "������ ���� " + mIndexCase  SKIP(1).
         PUT UNFORMATTED
            PADC("�����",{&cols}) SKIP
            PADC("��� ���� (�����) � ��������� �����������",{&cols}) SKIP(1).
      
         {orgname318p.i
            &xxOtstup        = 20
            &CurBranchName = mBranchName
         }

         IF mCashOrd THEN
         DO:
            {empty ttSvodOrd1}
            {empty ttRecIdOp1}
            FOR EACH RecIdOpDate WHERE 
                     RecIdOpDate.OpDate EQ bttRep.OpDate
               NO-LOCK,
            FIRST ttRepOp    WHERE 
                  ttRepOp.op EQ RecIdOpDate.Op
               NO-LOCK:
               CREATE ttRecIdOp1.
               ASSIGN 
                  ttRecIdOp1.op   = RecIdOpDate.op
                  ttRecIdOp1.type = ttRepOp.type
               .
            END.
            RUN getcashttv.p(TABLE ttRecIdOp1, OUTPUT TABLE ttSvodOrd1, mUsDprIDLst, mByOperRate).
         END.
         mIntSh = IF mIntSh > EXTENT(ttRep.qty) THEN EXTENT(ttRep.qty) ELSE mIntSh.
      
         PUT UNFORMATTED
            PADC(term2str(bttRep.OpDate, bttRep.OpDate),{&cols}) SKIP
            PADC("�������������������������������",{&cols}) SKIP
            PADC("(���)",{&cols}) SKIP
            PADC("������⢮ ��� (�訢��) 1" + IF mMoreTit THEN STRING(mIntSh) ELSE "" ,  {&cols}) SKIP
            PADC("���� (�訢) � 1" + FILL(' ',{&cols} - 36) + "�ப �࠭���� - 5 ���",    {&cols}) SKIP(1).
      
         /* ��ନ஢���� ⥫� ����� */
         IF mMoreTit THEN
            RUN NewPrintBody(bttRep.OpDate).
         ELSE
            RUN PrintBody(bttRep.OpDate).

         /* ������  */
         RUN GetRepFioByRef(ENTRY(1,PROGRAM-NAME(2), "."),mCuBrchID,?,mCuDprID).
         PUT UNFORMATTED SKIP(1).
         RUN PrintFioAndPost(mFioInRep[1],mPostInRep[1],LENGTH(mOtstup)).
         PUT UNFORMATTED SKIP(1)
         mOtstup "� ����묨 ��壠���᪮�� ��� ᢥ७�:" SKIP(1).
         RUN PrintFioAndPost(mFioInRep[2],mPostInRep[2],LENGTH(mOtstup)).
      END.   
   END.
END PROCEDURE.

/* ���������� �६����� ⠡���� */
PROCEDURE CreateTT:
   /* ���������� ���஢ */
   DEF BUFFER code      FOR code.
   DEF BUFFER sessions  FOR sessions.
   DEF BUFFER kau-entry FOR kau-entry.
   DEF BUFFER acct      FOR acct.
   DEF BUFFER op        FOR op.
   DEF BUFFER op-entry  FOR op-entry.

   DEFINE VARIABLE vKau            AS CHARACTER   NO-UNDO. /* ������ (ID ᬥ��) */
   DEFINE VARIABLE vType           AS INT64       NO-UNDO. /* ��� �㡯஢���� (ࠧ��� �����) */
   DEFINE VARIABLE vFirstEntryInOp AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vIntCuDpr-Id    AS INT64       NO-UNDO.
   DEFINE VARIABLE vOpDate         AS DATE        NO-UNDO.
   DEFINE VARIABLE vOpTime         AS INT64       NO-UNDO.
   DEFINE VARIABLE vDate           AS DATE        NO-UNDO.

   DO i = 1 TO NUM-ENTRIES(mUsDprIDLst):

      vIntCuDpr-Id = INT64(ENTRY(i,mUsDprIDLst)) NO-ERROR.

      FIND FIRST sessions WHERE 
                 sessions.dpr-id EQ vIntCuDpr-Id 
         NO-LOCK NO-ERROR.
      IF AVAIL sessions THEN
      DO:
         vKau = STRING(sessions.dpr-id).
         kau-entry:
         FOR EACH kau-entry WHERE /* USE-INDEX kau */
                  kau-entry.kau-id    BEGINS "��������"
              AND kau-entry.kau       = vKau
              AND kau-entry.op-status BEGINS gop-status
               NO-LOCK,

            FIRST acct      WHERE
                  acct.acct           = kau-entry.acct
              AND acct.currency       = kau-entry.currency
              AND acct.branch-id      = sessions.branch-id
               NO-LOCK,

            FIRST op        WHERE
                  op.op               = kau-entry.op
              NO-LOCK:

            find FIRST op-entry WHERE op-entry.op       = kau-entry.op
                                  and op-entry.op-entry = kau-entry.op-entry
                                  NO-LOCK no-error.

            /* �஢�ઠ ������ �஢���� � ���㬥�� */
            IF (    NOT CAN-DO(mTitDocPer,op.doc-type) 
                AND NOT CAN-FIND(FIRST op-entry OF op))
               OR Pereschet(op.op,kau-entry.op-entry,kau-entry.debit) 
               OR (    NOT CAN-DO(mDocType1,op.doc-type)
                   AND NOT CAN-DO(mDocType2,op.doc-type)) THEN
               NEXT kau-entry.

            /* ��।������ ����� ࠧ���� */
            vType = GetRepSection(op.acct-cat,kau-entry.debit,op.doc-type).
            IF vType EQ -1 THEN
               NEXT kau-entry.
               
            RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).

/* ivv ��᪮��� ��� ����祭�� ���� ᮧ����� ���㬥�� GetDateTimeOpTr �� ����⥭, � ����������� ���ᮢ�� ���, 
ᮧ������ ��� ࠭��, � ����� ���� �祢����, � ���塞 ���� �� ���� ���㬥�� */

            vOpDate = op.op-date.
            
            CASE mDateOtc:
               WHEN "" THEN vDate = mCuDate.
               WHEN "*" THEN
               DO:
                  vDate = vOpDate.
                  IF NOT CAN-DO(mDateLst, STRING(vOpDate, "99/99/9999")) THEN NEXT kau-entry.
               END.
               OTHERWISE
               DO:
                  vDate = vOpDate.
                  IF mDateOtc NE STRING(vOpDate, "99/99/9999") THEN NEXT kau-entry.
               END.
            END CASE.            

            FIND FIRST ttRepOp WHERE
                       ttRepOp.type     = vType
                   AND ttRepOp.currency = kau-entry.currency
                   AND ttRepOp.op       = kau-entry.op
                   AND ttRepOp.OpDate   = vDate
               EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF NOT AVAIL ttRepOp THEN
            DO:
               CREATE ttRepOp.
               ASSIGN
                  ttRepOp.type     = vType
                  ttRepOp.currency = kau-entry.currency
                  ttRepOp.op       = kau-entry.op
                  ttRepOp.OpDate   = vDate
                  vFirstEntryInOp  = YES
                  ttRepOp.Acct     = op-entry.acct-db
                  ttRepOp.doc-num  = op.doc-num
                  ttRepOp.user-id  = op.user-id
                  ttRepOp.doc-type = op.doc-type
                  .
            END.
            ASSIGN
               ttRepOp.qty     = ttRepOp.qty + 1 WHEN vFirstEntryInOp OR NOT mPodschKolDoc
               ttRepOp.amt     = IF ttRepOp.currency NE "" THEN ttRepOp.amt + kau-entry.amt-cur ELSE ttRepOp.amt + kau-entry.amt-rub
               vFirstEntryInOp = NO
               .

            IF mCashOrd THEN
            DO:
               FIND FIRST RecIdOpDate WHERE
                          RecIdOpDate.op EQ op.op
                  NO-LOCK NO-ERROR.
               IF NOT AVAIL RecIdOpDate THEN
               DO:
                  CREATE
                     RecIdOpDate.
                  ASSIGN
                     RecIdOpDate.op     = op.op
                     RecIdOpDate.OpDate = vDate
                     .
               END.
            END.
         END. /* FOR EACH kau-entry */
      END.
   END.
/* ivv */
/* ��ꥤ��塞 ���㬥��� ��襤訥 � ���� ���ᮢ�� ��� �� ������-����� ࠧ�묨 ���㬥�⠬�,
�� �� �⮬ �ਭ�ᥭ� �����⮬ � ����� 祪�.*/
   FOR EACH bttRepOp.
       find first ttRepOp where  ttRepOp.Acct     eq bttRepOp.Acct
                            and  ttRepOp.doc-num  eq bttRepOp.doc-num
                            and  ttRepOp.user-id  eq bttRepOp.user-id
                            and  ttRepOp.doc-type eq bttRepOp.doc-type
                            and  ttRepOp.currency eq bttRepOp.currency
                            and  ttRepOp.op       ne bttRepOp.op
                            and  ttRepOp.user-id begins "serv"
                            no-error.
       if avail ttRepOp then do:
          bttRepOp.amt  = bttRepOp.amt + ttRepOp.amt.
          delete ttRepOp.
       end.
   end.

/* */
   FOR EACH ttRepOp WHERE
      NO-LOCK:
         /* ����/ᮧ����� ����� �� �६����� ⠡��� */
      FIND FIRST ttRep WHERE
                 ttRep.type     = ttRepOp.type
             AND ttRep.currency = ttRepOp.currency
             AND ttRep.OpDate   = ttRepOp.OpDate
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      IF NOT AVAIL ttRep THEN 
      DO:
         CREATE ttRep.
         ASSIGN
            ttRep.type     = ttRepOp.type
            ttRep.currency = ttRepOp.currency
            ttRep.OpDate   = ttRepOp.OpDate
            .
      END.
      /* ����������� ����� */
      ASSIGN
         ttRep.qty[1] = ttRep.qty[1] + ttRepOp.qty
         ttRep.amt[1] = ttRep.amt[1] + ttRepOp.amt
         .
   END.   
  /* {empty ttRepOp}*/
END PROCEDURE.

/* ���������� �६����� ⠡���� ����� */
PROCEDURE CreateTT_SH:
   /* ���������� ���஢ */
   DEF BUFFER code      FOR code.
   DEF BUFFER sessions  FOR sessions.
   DEF BUFFER kau-entry FOR kau-entry.
   DEF BUFFER acct      FOR acct.
   DEF BUFFER acct-db   FOR acct.
   DEF BUFFER acct-cr   FOR acct.
   DEF BUFFER op        FOR op.
   DEF BUFFER op-entry  FOR op-entry.

   DEF VAR vKau      AS CHAR  NO-UNDO. /* ������ (ID ᬥ��) */
   DEF VAR vType     AS INT64   NO-UNDO. /* ��� �㡯஢���� (ࠧ��� �����) */
   DEFINE VARIABLE vIntCuDpr-Id AS INT64     NO-UNDO.
   DEFINE VARIABLE mIntExt      AS INT64     NO-UNDO.
   DEFINE VARIABLE ii           AS INT64     NO-UNDO.
   DEFINE VARIABLE vOpTime      AS INT64     NO-UNDO.
   DEFINE VARIABLE vOpDate      AS DATE      NO-UNDO.
   DEFINE VARIABLE vDate        AS DATE      NO-UNDO.  
      DO i = 1 TO NUM-ENTRIES(mUsDprIDLst):

         vIntCuDpr-Id = INT64(ENTRY(i,mUsDprIDLst)) NO-ERROR.

         FIND FIRST sessions WHERE sessions.dpr-id EQ vIntCuDpr-Id NO-LOCK NO-ERROR.
         IF AVAIL sessions THEN
         DO:
            vKau = STRING(sessions.dpr-id).
            
            ii = 1.
            /* SORT-ACCESS code */
            FOR EACH code WHERE
                     code.class EQ "�訢���"
                  AND code.code BEGINS "�訢"
                  NO-LOCK
                  BY INT64(ENTRY(2,code.code,"_")):

               mNumSh[ii] = ENTRY(2,code.code,"_").
               ii = ii + 1.
               ACCUMULATE ? (COUNT).
               mIntSh = ACCUM COUNT ?.
   
               xkau-entry:
               FOR EACH kau-entry WHERE /* USE-INDEX kau */
                        kau-entry.kau-id    BEGINS "��������"  
                    AND kau-entry.kau       = vKau       
                    AND kau-entry.op-date   = mCuDate  
                    AND kau-entry.op-status BEGINS gop-status
                     NO-LOCK,
                  FIRST op        WHERE
                        op.op               = kau-entry.op
                    NO-LOCK:

                  /* �஢�ઠ ������ �஢���� � ���㬥�� */
                  IF (    NOT CAN-DO(mTitDocPer,op.doc-type) 
                      AND NOT CAN-FIND(FIRST op-entry OF op)) 
                     OR Pereschet(op.op,kau-entry.op-entry,kau-entry.debit) THEN
                     NEXT xkau-entry.
                     
                     mIntExt = INT64(ENTRY(2,code.code,"_")) + 1.
                     
                     CASE TRIM(code.description[2]):
                        WHEN "�����" THEN
                        DO:
                           IF kau-entry.currency EQ "" THEN
                              NEXT xkau-entry.
                        END.
                        WHEN "���. �����" THEN
                        DO:
                           IF kau-entry.currency NE "" THEN
                              NEXT xkau-entry.
                        END.
                        OTHERWISE
                        DO:
                           /* NEXT xkau-entry. */
                        END.
                     END CASE.
   
                     IF op.acct-cat = "o" THEN
                        IF mTitVneb THEN
                           vType = (IF kau-entry.debit THEN 5 ELSE 6).
                        ELSE
                           NEXT xkau-entry.
                     ELSE
                        IF CAN-DO(mDocType1, op.doc-type) THEN
                           vType = (IF kau-entry.debit THEN 1 ELSE 2).
                        ELSE
                           IF CAN-DO(mDocType2, op.doc-type) THEN
                              vType = (IF kau-entry.debit THEN 3 ELSE 4).
 
/*                     RUN GetDateTimeOpTr(op.op-transaction, op.op, OUTPUT vOpTime,OUTPUT vOpDate).
*/
                     vOpDate = op.op-date.

                     CASE mDateOtc:
                        WHEN "" THEN vDate = mCuDate.
                        WHEN "*" THEN
                        DO:
                           vDate = vOpDate.
                           IF NOT CAN-DO(mDateLst,STRING(vOpDate, "99/99/9999")) THEN NEXT xkau-entry.
                        END.
                        OTHERWISE
                        DO:
                           vDate = vOpDate.
                           IF mDateOtc NE STRING(vOpDate, "99/99/9999") THEN NEXT xkau-entry.
                        END.
                     END CASE.

                     /* ����/ᮧ����� ����� �� �६����� ⠡��� */
                     FIND FIRST ttRep WHERE
                                ttRep.type     EQ vType
                            AND ttRep.currency EQ kau-entry.currency
                            AND ttRep.OpDate   EQ vDate
                        NO-ERROR.
                     IF NOT AVAIL(ttRep) THEN 
                     DO:
                        CREATE ttRep.
                        ASSIGN
                           ttRep.type     = vType
                           ttRep.currency = kau-entry.currency
                           ttRep.OpDate   = vDate
                           .
                     END.
   
                     /* ���. ��� ��� �।�� */
                     IF NOT kau-entry.debit THEN
                     DO: /* 1 */
                        FIND FIRST op-entry OF op WHERE 
                            op-entry.acct-db NE ?                           
                            NO-LOCK NO-ERROR.
   
                        IF AVAILABLE op-entry THEN
                        DO: /* 2 */
                           IF CAN-DO(code.val, op-entry.acct-db) THEN
                           DO: /* 3 */
                              /* ����������� ����� */
                              ASSIGN
                                 ttRep.type = vType
                                 ttRep.qty[mIntExt]  = ttRep.qty[mIntExt]     + 1
                                 ttRep.amt[mIntExt]  = IF ttRep.currency NE "" THEN ttRep.amt[mIntExt] + kau-entry.amt-cur ELSE ttRep.amt[mIntExt] + kau-entry.amt-rub
                                 .
                           END. /* 3 */
                        END. /* 2 */
                     END. /* 1 */
   
                     /* ���. ��� ��� ����� */
                     IF kau-entry.debit THEN
                     DO: /* 1 */
                        FIND FIRST op-entry OF op WHERE 
                            op-entry.acct-cr NE ?                           
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE op-entry THEN
                        DO: /* 2 */
                           IF CAN-DO(code.val, op-entry.acct-cr) THEN
                           DO: /* 3 */
                               /* ����������� ����� */
                               ASSIGN
                                  ttRep.type = vType
                                  ttRep.qty[mIntExt]  = ttRep.qty[mIntExt]     + 1
                                  ttRep.amt[mIntExt]  = IF ttRep.currency NE "" THEN ttRep.amt[mIntExt] + kau-entry.amt-cur ELSE ttRep.amt[mIntExt] + kau-entry.amt-rub
                                  .
                           END. /* 3 */
                        END. /* 2 */
                     END. /* 1 */
                  /* END. /* code */ */
               END. /* FOR EACH kau-entry */
            END. /* code */
            mIntLastSH =  ii - 1.
         END.
      END.
END PROCEDURE.

PROCEDURE InitForm:
    IF mNameCurr THEN
    DO:
       mMaxSh = 3.
       ASSIGN
          mFShLines [ 1] = "��������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 2] = "� ���ᮢ� ���㬥��� �               �ᥣ�              " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 3] = "�                    �                                  " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 4] = "�                    �                                  " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 5] = "�                    �                                  " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 6] = "�                    �����������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 7] = "�                    � ������⢮ �    �㬬� ��ࠬ�    " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 8] = "�                    � ���㬥�⮢ �     � 㪠������     " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 9] = "�                    �   (��.)    � ������������ ������ " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [10] = "��������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [11] = "�                    �            �                     " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [12] = "�          1         �     2      �           3         " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [13] = "��������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [14] = "�                    �            �                     " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [15] = "��������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [16] = "��������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [17] = "��������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          .

       ASSIGN
          mShLines [ 1] = "�����������������������������������"
          mShLines [ 2] = "                                   "
          mShLines [ 3] = "                                   "
          mShLines [ 4] = "�����������������������������������"
          mShLines [ 5] = "          ���� (�訢) � *         �"
          mShLines [ 6] = "�����������������������������������"
          mShLines [ 7] = " ������⢮ �    �㬬� ��ࠬ�    �"
          mShLines [ 8] = " ���㬥�⮢ �     � 㪠������     �"
          mShLines [ 9] = "   (��.)    � ������������ ������ �"
          mShLines [10] = "�����������������������������������"
          mShLines [11] = "            �                     �"
          mShLines [12] = "     *      �          *          �"
          mShLines [13] = "�����������������������������������"
          mShLines [14] = "            �                     �"
          mShLines [15] = "�����������������������������������"
          mShLines [16] = "�����������������������������������"
          mShLines [17] = "�����������������������������������"

          .
       ASSIGN
          mLShLines[ 1] = "���������������������������������Ŀ"
          mLShLines[ 2] = "                                  �"
          mLShLines[ 3] = "                                  �"
          mLShLines[ 4] = "���������������������������������Ĵ"
          mLShLines[ 5] = "          ���� (�訢) � *         �"
          mLShLines[ 6] = "���������������������������������Ĵ"
          mLShLines[ 7] = " ������⢮ �    �㬬� ��ࠬ�    �"
          mLShLines[ 8] = " ���㬥�⮢ �     � 㪠������     �"
          mLShLines[ 9] = "   (��.)    � ������������ ������ �"
          mLShLines[10] = "���������������������������������Ĵ"
          mLShLines[11] = "            �                     �"
          mLShLines[12] = "     *      �          *          �"
          mLShLines[13] = "���������������������������������Ĵ"
          mLShLines[14] = "            �                     �"
          mLShLines[15] = "�����������������������������������"
          mLShLines[16] = "���������������������������������Ĵ"
          mLShLines[17] = "���������������������������������Ĵ"

          .

    END.
    ELSE
    DO:
       mMaxSh = 2.
       ASSIGN
          mFShLines [ 1] = "������������������������������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 2] = "� ���ᮢ� ���㬥��� �                          �ᥣ�                         " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 3] = "�                    �                                                        " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 4] = "�                    �                                                        " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 5] = "�                    �                                                        " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 6] = "�                    ���������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 7] = "�                    � ������⢮ �               �㬬� ��ࠬ�               " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 8] = "�                    � ���㬥�⮢ �      � 㪠������ ������������ ������      " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [ 9] = "�                    �   (��.)    �                                           " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [10] = "������������������������������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [11] = "�                    �            �                                           " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [12] = "�          1         �     2      �                     3                     " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [13] = "������������������������������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [14] = "�                    �            �                                           " + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [15] = "������������������������������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [16] = "������������������������������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          mFShLines [17] = "������������������������������������������������������������������������������" + (IF mMoreTit THEN "�" ELSE "�")
          .

       ASSIGN
          mShLines [ 1] = "���������������������������������������������������������" 
          mShLines [ 2] = "                                                         " 
          mShLines [ 3] = "                                                         " 
          mShLines [ 4] = "���������������������������������������������������������" 
          mShLines [ 5] = "                    ���� (�訢) � *                     �" 
          mShLines [ 6] = "���������������������������������������������������������" 
          mShLines [ 7] = " ������⢮ �               �㬬� ��ࠬ�               �" 
          mShLines [ 8] = " ���㬥�⮢ �      � 㪠������ ������������ ������      �" 
          mShLines [ 9] = "   (��.)    �                                           �" 
          mShLines [10] = "���������������������������������������������������������" 
          mShLines [11] = "            �                                           �" 
          mShLines [12] = "     *      �                     *                     �" 
          mShLines [13] = "���������������������������������������������������������" 
          mShLines [14] = "            �                                           �" 
          mShLines [15] = "���������������������������������������������������������" 
          mShLines [16] = "���������������������������������������������������������" 
          mShLines [17] = "���������������������������������������������������������" 
          .

       ASSIGN
          mLShLines[ 1] = "�������������������������������������������������������Ŀ"
          mLShLines[ 2] = "                                                        �"
          mLShLines[ 3] = "                                                        �"
          mLShLines[ 4] = "�������������������������������������������������������Ĵ"
          mLShLines[ 5] = "                    ���� (�訢) � *                     �"
          mLShLines[ 6] = "�������������������������������������������������������Ĵ"
          mLShLines[ 7] = " ������⢮ �               �㬬� ��ࠬ�               �"
          mLShLines[ 8] = " ���㬥�⮢ �      � 㪠������ ������������ ������      �"
          mLShLines[ 9] = "   (��.)    �                                           �"
          mLShLines[10] = "�������������������������������������������������������Ĵ"
          mLShLines[11] = "            �                                           �"
          mLShLines[12] = "     *      �                     *                     �"
          mLShLines[13] = "�������������������������������������������������������Ĵ"
          mLShLines[14] = "            �                                           �"
          mLShLines[15] = "���������������������������������������������������������"
          mLShLines[16] = "�������������������������������������������������������Ĵ"
          mLShLines[17] = "�������������������������������������������������������Ĵ"
          .
    END.
END PROCEDURE.

PROCEDURE NewPrintBody:
   
   DEFINE INPUT  PARAMETER iOpDate AS DATE NO-UNDO. 
   
   DEFINE VARIABLE vCurType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vQty     AS INT64       NO-UNDO.
   DEFINE VARIABLE vCurName AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vLogCurr AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE ii       AS INT64       NO-UNDO.
   /* ����稪 */
   DEF VAR vCnt    AS INT64   NO-UNDO.
   /* ������ */
   DEF VAR vType   AS INT64   NO-UNDO.

   RUN InitForm.

   DEFINE VARIABLE iSH          AS INT64                      NO-UNDO.
   DEFINE VARIABLE vINf         AS INT64              INIT 4  NO-UNDO.
   DEFINE VARIABLE mL           AS CHARACTER                    NO-UNDO.
   DEFINE VARIABLE mL16         AS CHARACTER                    NO-UNDO.
   DEFINE VARIABLE mL17         AS CHARACTER                    NO-UNDO.
   DEFINE VARIABLE mLT          AS CHARACTER                    NO-UNDO.
   DEFINE VARIABLE mLW          AS CHARACTER EXTENT 5           NO-UNDO.
   DEFINE VARIABLE mLength      AS INT64                      NO-UNDO.
   DEFINE VARIABLE iii          AS INT64                      NO-UNDO.

   DEFINE VARIABLE mBeg AS INT64     NO-UNDO.
   DEFINE VARIABLE mEnd AS INT64     NO-UNDO.
   DEFINE VARIABLE mMax AS INT64     NO-UNDO.
   DEFINE VARIABLE mAll AS INT64     NO-UNDO.
   DEFINE VARIABLE mSkip AS INT64     NO-UNDO.

   mMax = mMaxSh.
   mAll = mIntSh.
   mSkip = INT64(mAll / mMax).    

   mBeg = 1.
   mEnd =  IF (mBeg + (mMax - 1)) < mAll THEN (mBeg + (mMax - 1)) ELSE mAll.
xxx:
DO iii = 1 TO mSkip:
   /* ����� ��ࢮ� ��࠭��� */
   DO vCnt = 1 TO 12:
      IF vCnt <> 11 AND mFShLines[vCnt] <> "" THEN
      DO:
         mL = mFShLines[vCnt].
         IF mMoreTit THEN
         DO:
            IF vCnt EQ 3 THEN
            DO:
               mLT     = mL.
               mLW[1]  = "� ⮬ �᫥ � �⤥���� ����� (�訢��): " + STRING("").
               mLength = (LENGTH(mShLines[vCnt]) * ((mEnd - mBeg) + 1)).
               {wordwrap.i  
                   &s        = mLW
                   &n        = EXTENT(mLW)
                   &l        = mLength
                   }
               ii = 1.
               DO  WHILE ii < 5 AND mLW[ii] NE "":
                  mL  = mLT + PADC(mLW[ii],mLength - 1) + "�".
                  PUT UNFORMATTED mOtstup mL SKIP.
                  ii = ii + 1.
               END.
            END.
            DO ii = mBeg TO mEnd - 1:
              IF vCnt EQ 5  THEN
                 mLT =  (PADC("���� (�訢) � " + mNumSh[ii] ,LENGTH(mShLines[vCnt]) - 1) + "�"). 
              ELSE 
              IF vCnt EQ 12 THEN
              DO:
                 mLT = PADC(STRING(vINf),(INDEX(mShLines[vCnt],"*") * 2)) +
                       "�".
                 vINf = vINf + 1.
                 mLT = mLT + PADC(STRING(vINf),((R-INDEX(mShLines[vCnt],"*")
                       - LENGTH(mLT)) * 2) - 1) + "�".
                 vINf = vINf + 1.
              END.
              ELSE 
                 mLT = mShLines[vCnt].
              mL = mL + mLT.
            END.
            IF vCnt EQ 5  THEN
                 mLT =  (PADC("���� (�訢) � " + mNumSh[mEnd],LENGTH(mLShLines[vCnt]) - 1) + "�"). 
            ELSE
            IF vCnt EQ 12 THEN
            DO:
               mLT = PADC(STRING(vINf),(INDEX(mLShLines[vCnt],"*") * 2)) + "�".
               vINf = vINf + 1.
               mLT = mLT + PADC(STRING(vINf),((R-INDEX(mLShLines[vCnt],"*")
                     - LENGTH(mLT)) * 2) - 1) + "�".
               vINf = vINf + 1.
            END.
            ELSE 
               mLT = mLShLines[vCnt].

            mL = mL + mLT.
            
         END.
         IF vCnt NE 3 THEN
         PUT UNFORMATTED mOtstup mL SKIP.
      END.
   END.

   /* ��ॡ�� ࠧ����� ����� */
   DO vType = 1 TO EXTENT(mNames):
      IF mNames[vType] EQ "" THEN
         NEXT.
      /* �������⥫쭠� ����� ����� ࠧ������ */
      ASSIGN
         mL   = mFShLines[13]
         mL16 = mFShLines[16]
         mL17 = mFShLines[17]
      .
      IF mMoreTit THEN
      DO:
         DO ii = mBeg TO mEnd - 1:
         ASSIGN
            mL   = mL   + mShLines[13]
            mL16 = mL16 + mShLines[16]
            mL17 = mL17 + mShLines[17]
         .
         END.
         ASSIGN
            mL   = mL   + mlShLines[13]
            mL16 = mL16 + mlShLines[16]
            mL17 = mL17 + mlShLines[17]
         .
      END.


      IF vType MOD 2 EQ 1 THEN
         PUT UNFORMATTED 
         mOtstup mL16 SKIP 
         mOtstup "�" mHNames[INT64(vType / 2)] 
         FILL(" ",LENGTH(mL) - LENGTH(mHNames[INT64(vType / 2)]) - 2) "�" SKIP
         mOtstup mL17 SKIP. 
      ELSE
         PUT UNFORMATTED mOtstup mL SKIP.

      /* �뢮� ������ �� ࠧ���� */
      FOR EACH ttRep WHERE
               ttRep.type   = vType
           AND ttRep.OpDate = iOpDate     
         NO-LOCK:
         vQty = ttRep.qty[1].
         IF mCashOrd THEN
            vCurType = IF CAN-DO("1,3,5",STRING(vType)) THEN "��室��" ELSE "��室��".
            FOR EACH ttSvodOrd1 WHERE
                     ttSvodOrd1.DocCur  EQ ttRep.currency
                 AND ttSvodOrd1.OrdType EQ vCurType
                 AND ttSvodOrd1.RepPart EQ vType
               NO-LOCK:
               vQty = vQty - ttSvodOrd1.qty.
            END.   
         /* ������⢮ ��ப � ࠧ���� */
         ACCUMULATE ? (COUNT).
         vLogCurr = GetRepCurrName(ttRep.currency,OUTPUT vCurName).
         IF mNameCurr AND ttRep.currency = "" THEN 
            vCurName = TRIM(mNacCur).
         mL = "�" + STRING(ENTRY(MIN(ACCUM COUNT ?, NUM-ENTRIES(mNames[vType])), mNames[vType]),"x(20)") + "�" 
                  + (IF vType EQ 4 THEN "     X     " ELSE STRING(vQty,"zzzzzzzzzz9")) + " �" 
                  + (IF vLogCurr THEN STRING(ttRep.amt[1],
                                             "-zzz,zzz,zz9.99 ") 
                                 ELSE STRING(ttRep.amt[1],
                                             "-zzz,zzz,zz9.99")) + " "
                  + (IF vLogCurr THEN STRING(vCurName,"x(4)") 
                                 ELSE STRING(vCurName,
                                             "x({&format-cur-name})")) + "�"
                  .

         IF mMoreTit THEN
         DO:
            DO ii = (mBeg + 1) TO mEnd + 1:
               mLT = (IF vType EQ 4 THEN "     X     " ELSE PADC(STRING(ttRep.qty[ii],"zzzzzzzzzz9"),LENGTH(STRING(ttRep.qty[ii],"zzzzzzzzzz9")) - 1)) + " �" 
                     + (IF vLogCurr THEN STRING(ttRep.amt[ii],
                                                "-zzz,zzz,zz9.99 ") 
                                    ELSE STRING(ttRep.amt[ii],
                                                "-zzz,zzz,zz9.99")) + " "
                     + (IF vLogCurr THEN STRING(vCurName,"x(4)") ELSE STRING(vCurName,"x({&format-cur-name})")) + "�"
                     .
               mL  = mL + IF ttRep.amt[ii] EQ 0 THEN ((IF vLogCurr THEN ("            �                 ") ELSE ("            �                "))  + (IF vLogCurr THEN STRING(" ","x(4)") ELSE STRING(" ","x({&format-cur-name})")) + "�") ELSE mLT. 
            END.
         END.
         PUT UNFORMATTED mOtstup mL SKIP.

      END.

      /* �������� ����묨 ��ப��� �� ������� ������������ ࠧ���� */
      DO vCnt = (ACCUM COUNT ?) + 1 TO NUM-ENTRIES(mNames[vType]) - 1:
         mLT = "�" + STRING(ENTRY(vCnt, mNames[vType]),"x(20)") + "�".
         mL = mLT  + mShLines[14].
         IF mMoreTit THEN
         DO:
            DO ii = mBeg TO mEnd - 1:
               mL = mL + mShLines[14].
            END.
            mL = mL + mlShLines[14].
         END.
         PUT UNFORMATTED mOtstup mL SKIP.
      END.
   END.

   /* ������ */
   mL = mFShLines[15].
   IF mMoreTit THEN
   DO:
      DO ii = mBeg TO mEnd - 1:
         mL = mL + mShLines[15].
      END.
      mL = mL + mlShLines[15].
   END.
   PUT UNFORMATTED mOtstup mL SKIP.

   IF NOT mMoreTit THEN 
     LEAVE xxx. 
   mBeg = mEnd + 1.
   mEnd = IF (mBeg + (mMax - 1)) < mAll THEN (mBeg + (mMax - 1)) ELSE mAll.
END.

END PROCEDURE.

/* ��楤�� �ନ஢���� ⥫� ����� */
PROCEDURE PrintBody: 
   
   DEFINE INPUT  PARAMETER iOpDate AS DATE NO-UNDO.
     
   DEFINE VARIABLE vCurType AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vQty     AS INT64     NO-UNDO.
   /* ���������� ���஢ */
   DEF BUFFER currency FOR currency.
   /* ������ ����� */
   DEF VAR mLines    AS CHAR  NO-UNDO  EXTENT 16.
   DEFINE VARIABLE vCurName AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vLogCurr AS LOGICAL     NO-UNDO.


IF mNameCurr THEN
   IF mMoreTit THEN
      ASSIGN
         mLines[ 1] = "������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ"
         mLines[ 2] = "� ���ᮢ� ���㬥��� �               �ᥣ�             �                               � ⮬ �᫥ � �⤥���� �����(�訢��):                                �"
         mLines[ 3] = "�                    �                                 �����������������������������������������������������������������������������������������������������Ĵ"
         mLines[ 4] = "�                    �                                 �          ���� (�訢) �          �          ���� (�訢) �          �          ���� (�訢) �          �"
         mLines[ 5] = "�                    ���������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[ 6] = "�                    � ������⢮ �   �㬬� ��ࠬ�    � ������⢮ �   �㬬� ��ࠬ�    � ������⢮ �   �㬬� ��ࠬ�    � ������⢮ �   �㬬� ��ࠬ�    �"
         mLines[ 7] = "�                    � ���㬥�⮢ �    � 㪠������     � ���㬥�⮢ �    � 㪠������     � ���㬥�⮢ �    � 㪠������     � ���㬥�⮢ �    � 㪠������     �"
         mLines[ 8] = "�                    �   (��.)    � ������������ ������   (��.)    � ������������ ������   (��.)    � ������������ ������   (��.)    � ������������ ������"
         mLines[ 9] = "������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[10] = "�                    �            �                   |�            �                    �            �                    �            �                    �"
         mLines[11] = "�          1         �     2      �          3         �     4      �          5         �     6      �          7         �     8      �          9         �"
         mLines[12] = "������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[13] = "�                    |            �                    �            �                    �            �                    �            �                    �"
         mLines[14] = "��������������������������������������������������������������������������������������������������������������������������������������������������������������"
         mLines[15] = "������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[16] = "������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"

         .
   ELSE
      ASSIGN
         mLines[ 1] = "������������������������������������������������������Ŀ"
         mLines[ 2] = "� ���ᮢ� ���㬥��� �               �ᥣ�             �"
         mLines[ 3] = "�                    ���������������������������������Ĵ"
         mLines[ 4] = "�                    � ������⢮ �   �㬬� ��ࠬ�    �"
         mLines[ 5] = "�                    � ���㬥�⮢ �    � 㪠������     �"
         mLines[ 6] = "�                    �   (��.)    � ������������ ������"
         mLines[ 7] = "������������������������������������������������������Ĵ"
         mLines[10] = "�                    �            �                   |�"
         mLines[11] = "�          1         �     2      �          3         �"
         mLines[12] = "������������������������������������������������������Ĵ"
         mLines[13] = "�                    |            �                    �"
         mLines[14] = "��������������������������������������������������������"
         mLines[15] = "������������������������������������������������������Ĵ"
         mLines[16] = "������������������������������������������������������Ĵ"
         .
ELSE
   IF mMoreTit THEN
      ASSIGN
         mLines[ 1] = "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ"
         mLines[ 2] = "� ���ᮢ� ���㬥��� �                          �ᥣ�                         �                                       � ⮬ �᫥ � �⤥���� �����(�訢��):                                    �"
         mLines[ 3] = "�                    �                                                        �����������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[ 4] = "�                    �                                                        �                    ���� (�訢) �                       �                    ���� (�訢) �                       �"
         mLines[ 5] = "�                    ��������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[ 6] = "�                    � ������⢮ �               �㬬� ��ࠬ�               � ������⢮ �               �㬬� ��ࠬ�               � ������⢮ �               �㬬� ��ࠬ�               �"
         mLines[ 7] = "�                    � ���㬥�⮢ �      � 㪠������ ������������ ������      � ���㬥�⮢ �      � 㪠������ ������������ ������      � ���㬥�⮢ �      � 㪠������ ������������ ������      �"
         mLines[ 8] = "�                    �   (��.)    �                                           �   (��.)    �                                           �   (��.)    �                                           �"
         mLines[ 9] = "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[10] = "�                    �            �                                          |�            �                                           �            �                                           �"
         mLines[11] = "�          1         �     2      �                     3                     �     4      �                     5                     �     6      �                     7                     �"
         mLines[12] = "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[13] = "�                    |            �                                           �            �                                           �            �                                           �"
         mLines[14] = "�������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������"
         mLines[15] = "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         mLines[16] = "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
         .
   ELSE
      ASSIGN
         mLines[ 1] = "�����������������������������������������������������������������������������Ŀ"
         mLines[ 2] = "� ���ᮢ� ���㬥��� �                          �ᥣ�                         �"
         mLines[ 3] = "�                    ��������������������������������������������������������Ĵ"
         mLines[ 4] = "�                    � ������⢮ �               �㬬� ��ࠬ�               �"
         mLines[ 5] = "�                    � ���㬥�⮢ �      � 㪠������ ������������ ������      �"
         mLines[ 6] = "�                    �   (��.)    �                                           �"
         mLines[ 7] = "�����������������������������������������������������������������������������Ĵ"
         mLines[10] = "�                    �            �                                          |�"
         mLines[11] = "�          1         �     2      �                     3                     �"
         mLines[12] = "�����������������������������������������������������������������������������Ĵ"
         mLines[13] = "�                    |            �                                           �"
         mLines[14] = "�������������������������������������������������������������������������������"
         mLines[15] = "�����������������������������������������������������������������������������Ĵ"
         mLines[16] = "�����������������������������������������������������������������������������Ĵ"
         .
   /* ����稪 */
   DEF VAR vCnt    AS INT64   NO-UNDO.
   /* ������ */
   DEF VAR vType   AS INT64   NO-UNDO.

   /* ����� ��ࢮ� ��࠭��� */
   DO vCnt = 1 TO 11:
      IF vCnt <> 10 AND mLines[vCnt] <> "" THEN
         PUT UNFORMATTED mOtstup mLines[vCnt] SKIP.
   END.

   /* ��ॡ�� ࠧ����� ����� */
   DO vType = 1 TO EXTENT(mNames):
      IF mNames[vType] EQ "" THEN
         NEXT.
      /* �������⥫쭠� ����� ����� ࠧ������ */
      IF vType MOD 2 EQ 1 THEN
         PUT UNFORMATTED 
         mOtstup mLines[15] SKIP 
         mOtstup "�" mHNames[INT64(vType / 2)] 
         FILL(" ",LENGTH(mLines[1]) - LENGTH(mHNames[INT64(vType / 2)]) - 2) "�" SKIP
         mOtstup mLines[16] SKIP. 
      ELSE
         PUT UNFORMATTED mOtstup mLines[12] SKIP.

      /* �뢮� ������ �� ࠧ���� */
      FOR EACH ttRep WHERE
               ttRep.type   = vType
           AND ttRep.OpDate = iOpDate    
            NO-LOCK:
         vQty = ttRep.qty[1].
         IF mCashOrd THEN
            vCurType = IF CAN-DO("1,3,5",STRING(vType)) THEN "��室��" ELSE "��室��".
            FOR EACH ttSvodOrd1 WHERE
                     ttSvodOrd1.DocCur  EQ ttRep.currency
                 AND ttSvodOrd1.OrdType EQ vCurType
                 AND ttSvodOrd1.RepPart EQ vType
               NO-LOCK:
               vQty = vQty - ttSvodOrd1.qty.
            END.   
            
         /* ������⢮ ��ப � ࠧ���� */
         ACCUMULATE ? (COUNT).
         PUT UNFORMATTED mOtstup. 
         PUT
            ENTRY(MIN(ACCUM COUNT ?, NUM-ENTRIES(mNames[vType])),
                  mNames[vType]
                 )              FORMAT "�x(20)".
         IF vType EQ 4 THEN 
            PUT "�     X      �". 
         ELSE
            PUT "�" + padc(trim(string(vQty)),12) + "�" FORMAT "x(14)". 
         PUT ttRep.amt[1]           FORMAT "-zzz,zzz,zz9.99 ".
         vLogCurr = GetRepCurrName(ttRep.currency,OUTPUT vCurName).
         IF     mNameCurr 
            AND ttRep.currency = "" THEN 
            vCurName = TRIM(mNacCur).

         IF vLogCurr THEN 
            PUT
               vCurName FORMAT "x(3) ".
         ELSE
            PUT
               vCurName FORMAT "x({&format-cur-name})".
         PUT UNFORMATTED SUBSTR(mLines[10], INDEX(mLines[10], "|") + 1) SKIP.
      END.

      /* �������� ����묨 ��ப��� �� ������� ������������ ࠧ���� */
      DO vCnt = (ACCUM COUNT ?) + 1 TO NUM-ENTRIES(mNames[vType]) - 1:
         if vType EQ 4 THEN do:
            PUT UNFORMATTED
               mOtstup ENTRY(vCnt, mNames[vType]) FORMAT "�x(20)�"
               SUBSTR(mLines[13], INDEX(mLines[13], "|") + 1,5) "X" SUBSTR(mLines[13], INDEX(mLines[13], "|") + 7)
            SKIP.
         end.
         else do:
            PUT UNFORMATTED
               mOtstup ENTRY(vCnt, mNames[vType]) FORMAT "�x(20)�"
               SUBSTR(mLines[13], INDEX(mLines[13], "|") + 1)
            SKIP.
         end.
      END.
   END.
   /* ������ */
   PUT UNFORMATTED mOtstup mLines[14] SKIP.

END PROCEDURE.

FUNCTION GetRepSection RETURN INT64 (iAcct-cat AS CHARACTER,
                                     iDebit    AS LOGICAL,
                                     iDoc-type AS CHARACTER).

   DEFINE VARIABLE vType AS INT64 NO-UNDO INIT -1.

   IF     iAcct-cat = "o"
      AND mTitVneb THEN
      vType = (IF iDebit THEN 5 ELSE 6).
   ELSE
      IF CAN-DO(mDocType1,iDoc-type) THEN
         vType = (IF iDebit THEN 1 ELSE 2).
      ELSE
         IF CAN-DO(mDocType2,iDoc-type) THEN
            vType = (IF iDebit THEN 3 ELSE 4).

   RETURN vType.
END FUNCTION.
/* $LINTFILE='v-tit-pg318p.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='stre' */
/* $LINTDATE='09/03/2017 10:37:05.668+03:00' */
/*prosign4cwhxJIt+urRuLdUlv/O/g*/