/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2005 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: SF-CALC.P
      Comment: ����� ����� ���㯮� � �த��
   Parameters:
         Uses:
      Used by:
      Created: 23.01.2005 15:55 gorm     (39642)
     Modified: 22.02.2005 17:52 gorm     (39643)
     Modified: 22.03.2006 17:52 gorm     (51989) ������� ������ ��� ��⮢-䠪���.
                                         ������ ���-䠪���� �⡨����� � ����� 
                                         ���㯮�/�த�� � ��⮬ ⨯�� ����� � ����ᮢ
     Modified: 15.12.2006 18:01 laav     (62990) � �᫮��� �⡮� ��⮢-䠪��� ��������� �᫮���
                                         �� �ਭ���������� ���-䠪���� � �⤥����� DataBlock ���
                                         � ������ �� ��⮬��� �⮣� �⤥�����
*/
{globals.i}
{sv-calc.i}
{intrface.get xclass}
{intrface.get axd}
{intrface.get asset}

DEF VAR mAmount       AS DEC NO-UNDO.  /* �㬬� ���-䠪���� (��� ���筮� ������) */
DEF VAR mNDSAmt       AS DEC NO-UNDO.  /* ��� ���-䠪���� (��� ���筮� ������) */
DEF VAR mAmtNoNdsSF   AS DEC NO-UNDO.  /* �㬬� ���-䠪���� ��� ��� */
DEF VAR mAmountSF     AS DEC NO-UNDO.  /* �㬬� ���-䠪���� ������ ��� */
DEF VAR mNDSAmtSF     AS DEC NO-UNDO.  /* �㬬� ��� ���-䠪���� */

DEF VAR txt           AS CHAR NO-UNDO EXTENT 20.
DEF VAR mNDS          AS CHAR NO-UNDO. /* �⠢�� ��� �� ��㣥 */
DEF VAR mNum          AS INT64  NO-UNDO. /* ����� ��� ��।������, 
                                       ** � ������ ���� ��� �⭮��� �㬬� ��㣨 */
DEF VAR mCntryName    AS CHAR NO-UNDO. /* ������������ ��࠭� */
DEF VAR mNumGTD       AS CHAR NO-UNDO. /* ����� ��� */
DEF VAR mContract     AS CHAR NO-UNDO. /* �����祭�� ���-䠪���� */
DEF VAR mSalerName    AS CHAR NO-UNDO. /* ������������ ����ࠣ��� */
DEF VAR mSalerAddres  AS CHAR NO-UNDO. /* ���� ����ࠣ��� */
DEF VAR mSalerInn     AS CHAR NO-UNDO. /* ��� ����ࠣ��� */
DEF VAR mSalerKPP     AS CHAR NO-UNDO. /* ��� ����ࠣ��� */

DEF VAR mSgUch        AS CHAR NO-UNDO. /*���. ४. ������� */
DEF VAR mSgPart       AS CHAR NO-UNDO. /*���. ४. �������� */
DEF VAR mAntiContract AS CHAR NO-UNDO. /*��� ����ᮢ�� ��⮢-䠪���*/

DEF VAR mListUch      AS CHAR NO-UNDO. /*���᮪ ��������� ���祭�� �������*/
DEF VAR mSurrOp       AS CHAR NO-UNDO. /* ���᮪ ���ண�⮢ ���⥦��, �易���� � 
                                       ** ��⮬-䠪��ன */
DEF VAR mSF-OP-Links  AS CHAR NO-UNDO. /* ���᮪ ����� �裡 */

DEF VAR mI            AS INT64  NO-UNDO. /*���稪*/
DEF VAR mSimI         AS INT64  NO-UNDO. /*���稪*/
DEF VAR mBrList       AS CHAR NO-UNDO. /*���᮪ �⤥�����*/
DEF VAR mLstDate      AS CHAR NO-UNDO. /* ���᮪ ��� */
DEF VAR mOnBook       AS CHAR NO-UNDO. /* �� ������� */
DEF VAR mKnigaOtr     AS CHAR NO-UNDO. /* �� �������� */
DEF VAR mDopList      AS CHAR NO-UNDO. /* �� ������� */
DEF VAR mDopListDate  AS DATE NO-UNDO. /* �� ������� */
DEF VAR mSummPlat     AS CHAR NO-UNDO. /* �� �㬬돫�� */
DEF VAR mDate         AS DATE NO-UNDO.
DEF VAR mSumm         AS DEC  NO-UNDO.

DEF BUFFER bloan FOR loan.

/* ������ �������� �� ��� �� ᯨ᪠ � ��ਮ� */
FUNCTION DatePeriod RETURNS LOGICAL (INPUT iDtBeg  AS DATE,
                                     INPUT iDtEnd  AS DATE,
                                     INPUT iList   AS CHARACTER).
      DEFINE VARIABLE i       AS INT64  NO-UNDO.
      DEFINE VARIABLE oResult AS LOGICAL  NO-UNDO.
      oResult = NO.
      
      IF iList NE "" THEN
      DO i = 1 TO NUM-ENTRIES(iList) - 1:
         IF ENTRY(i, iList) > "" THEN
         IF       DATE(ENTRY(i, iList)) >= iDtBeg
              AND DATE(ENTRY(i, iList)) <= iDtEnd THEN
         oResult = YES.
      END.
      RETURN oResult.

END FUNCTION.

FUNCTION IsEmpty RETURN LOG (iValue AS CHAR):
   iValue = TRIM(iValue).
   RETURN (iValue = "" OR iValue = ? OR iValue = "?").
END FUNCTION. /* IsEmpty */

/* �����頥� ᯨ᮪ ���ண�⮢ ��ꥪ⮢,
** �ਢ易���� � 㪠������� ��ꥪ��
** �� 㪠����� ����� �裡 � ��⮬ ���ࠢ�����. */
FUNCTION VerifyLinks RETURNS CHAR
   (iClass-Code      AS CHAR, /* ID �����                            */
    iSurrogate       AS CHAR, /* ID(c��ண��) ��ꥪ�                 */
    iLink-Direction  AS CHAR, /* ���ࠢ����� �裡: s | t | ?         */
    iLink-Codes      AS CHAR, /* ���᮪ ����� ������ � CAN-DO �ଠ� */
    iDlm             AS CHAR /* �������⥫� १������饣� ᯨ᪠   */
   ):

   DEF VAR vList  AS CHAR  NO-UNDO.

   DEF BUFFER xlink  FOR xlink.
   DEF BUFFER links  FOR links.

   IF IsEmpty(iLink-Direction) THEN
      iLink-Direction = ?.

   FOR EACH xlink WHERE
            xlink.class-code = iClass-Code
        AND CAN-DO(iLink-Codes, xlink.link-code)
      NO-LOCK:
      
      IF  iLink-Direction = ? OR
         (iLink-Direction = "s" AND xlink.link-direction BEGINS "s")
      THEN
         FOR EACH links WHERE
                  links.link-id   = xlink.link-id
              AND links.source-id = iSurrogate
              AND LOOKUP(links.target-id, vList, iDlm) = 0
            NO-LOCK:
            vList = vList + (IF vList = "" THEN "" ELSE iDlm) + links.target-id.
         END.

      IF  iLink-Direction = ? OR
         (iLink-Direction = "t" AND xlink.link-direction BEGINS "t")
      THEN
         FOR EACH links WHERE
                  links.link-id   = xlink.link-id
              AND links.target-id = iSurrogate
              AND LOOKUP(links.source-id, vList, iDlm) = 0
            NO-LOCK:
            vList = vList + (IF vList = "" THEN "" ELSE iDlm) + links.source-id.
         END.
   END.

   RETURN vList.
END.


DEF VAR aaa AS CHARACTER.

Main_Block:
DO ON ERROR  UNDO Main_Block, LEAVE Main_Block
   ON ENDKEY UNDO Main_Block, LEAVE Main_Block:

   ASSIGN
      mListUch  = GetXAttrEx("axd-sf","�������","data-format")    /* "�����/�� �⮡ࠦ���" */
      mOnBook   = FGetSetting("�������","","")                    /* ����஥�� ��ࠬ��� "�������" */
   .
   
   /* ��।��塞 - ����� �த�� ��� ���㯮� */
   FIND FIRST Formula WHERE
              Formula.Var-ID       EQ "sf-kind"
          AND Formula.DataClass-ID EQ DataClass.DataClass-id 
      NO-LOCK NO-ERROR.
   
   IF AVAIL formula 
      THEN mContract = ENTRY(1,formula.formula,'~~').
      ELSE mContract = "sf-in". /* �� 㬮�砭�� - ����� ���㯮� */
   
   mAntiContract = IF mContract = "sf-in" 
                   THEN "sf-out" 
                   ELSE "sf-in".
   
   /*����ࠥ� � mBrList �� ���୨� �⤥����� ��� DataBlock (������ ⥪�騩)*/
   {getbrlst.i DataBlock.Branch-Id mBrList}

   /* �⡨ࠥ� �� ���-䠪���� (���⠢����� ��� �ਭ���),
      ��ॣ����஢���� �� ��ਮ� ���� */
   lnfr:
   FOR EACH loan WHERE 
            loan.filial-id  = shFilial
/*        AND CAN-DO(mBrList, loan.branch-id)*/
        AND loan.contract   = mContract
/*        AND loan.open-date <= DataBlock.end-date
        AND loan.doc-ref     = loan.doc-ref*/
   NO-LOCK USE-INDEX op-l-fil:

      /* ���-䠪���� ��� ⨯� ��� � ��砫�� ����ᮬ, 
      ** � ⠪ �� ⥪�騥 - �� �⮡ࠦ��� */
      IF loan.cont-type   = ""         OR
         loan.loan-status = "������"   OR
         loan.loan-status = "����騩"
      THEN NEXT lnfr.

      mLstDate = "".
      mSgUch = GetXattrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "�������",
                                "").

      /* �᫨ 㪠����, �� �� ���� �⮡ࠦ��� - �� �⮡ࠦ��� */
      IF mSgUch = ENTRY(2,mListUch,"/") THEN NEXT lnfr.
      mDopList = GetxAttrValueEx("loan",
                                       loan.contract + "," + loan.cont-code,
                                       "�������",
                                       "").
      IF mDopList NE "" THEN
      DO:
         mDopListDate = DATE(mDopList) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            mDopList = "".
      END.

      IF (mOnBook EQ "��") THEN
      DO:
         /* �஫���塞 �஢���� �� �裡 sf-op-dr, �᫨ �� ����� ���, � �஢��塞 �� ���� ���-䠪����,
            ���� ������塞 ����� �� �஢����� �᫨ ��� �஢���� �室�� � �᪮�� ��ਮ�. */
         ASSIGN
            mLstDate = ""
            mSF-OP-Links = "sf-op-dr"
            mSurrOp = VerifyLinks(loan.class-code,                      /* ID �����     */
                                  loan.contract + "," + loan.cont-code, /* ID(c��ண��) ��ꥪ�   */
                                  ?,                                    /* ���ࠢ����� �裡: s | t | ? */
                                  "sf-op-dr",                           /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                                  ";").                                 /* �������⥫� १������饣� ᯨ᪠   */
         
         IF TRIM(mSurrOp) NE ""  THEN
         DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
            FIND FIRST op-entry WHERE 
                       op-entry.op       = INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
                   AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI,mSurrOp,";")))
            NO-LOCK NO-ERROR.

            IF AVAIL op-entry THEN
            DO:
               IF    op-entry.op-date >= DataBlock.beg-date
               AND  op-entry.op-date <= DataBlock.end-date THEN
               /* � ������ ��砥 � mLstDate ����娢��� ���室�騩 op.op */
               mLstDate = IF mLstDate EQ "" THEN
                                             ENTRY(1,ENTRY(mI,mSurrOp,';')) + "," + ENTRY(2,ENTRY(mI,mSurrOp,';')) 
                       ELSE "," + ENTRY(1,ENTRY(mI,mSurrOp,';')) + "," +  ENTRY(2,ENTRY(mI,mSurrOp,';')).
            END.

         END.

         IF mLstDate > "" THEN
         DO:
            mKnigaOtr = GetXattrValueEx ("loan",
                                         loan.contract + "," + loan.cont-code,
                                         "��������",
                                         "������").

            IF mKnigaOtr EQ "������" THEN RUN VerifyStat.
            ELSE
            DO:
               mSurrOp = mLstDate.
               {buybook1.i &RETURN1 = " return"}
            END.
         END.
         IF TRIM(mSurrOp) EQ "" THEN
         DO:
            IF       loan.open-date >= DataBlock.beg-date 
                 AND loan.open-date <= DataBlock.end-date THEN
            DO:
               IF mDopList NE "" THEN
                  IF       mDopListDate GE DataBlock.beg-date 
                       AND mDopListDate LE DataBlock.end-date THEN
                     mDopList = "".
                  ELSE
                     mDopList = "���".
               RUN VerifyStat.
            END.
            ELSE
            DO:
               IF     mDopList     NE ""
                  AND mDopListDate GE DataBlock.beg-date 
                  AND mDopListDate LE DataBlock.end-date THEN
               DO:
                  mDopList = "��".
                  RUN VerifyStat.
               END.
            END.
         END.
      END.
      ELSE
      DO:
         IF  loan.loan-status EQ "�����"
         AND mOnBook          EQ "���" THEN
         DO:
            ASSIGN
               mLstDate = ""
               mSF-OP-Links = "sf-op-nds"
            .
            /* ��।��塞 �� �易��� � ��� �஢���� ��� ��� */
            mSurrOp = GetLinks(loan.class-code,                      /* ID ����� */
                               loan.contract + "," + loan.cont-code, /* ID(c��ண��) ��ꥪ� */
                               ?,                                    /* ���ࠢ����� �裡: s | t | ? */
                               "sf-op-nds",                          /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                               ";",                                  /* �������⥫� १������饣� ᯨ᪠ */
                               ?).

            IF mSurrOp > ""  THEN
            DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
               FIND FIRST op-entry WHERE 
                          op-entry.op       = INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
                      AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI,mSurrOp,";")))
               NO-LOCK NO-ERROR.
               IF AVAIL op-entry THEN
                  mLstDate = mLstDate + STRING(op-entry.op-date) + ",".
            END.

            /* ��������� ���� ॣ����樨 � ��ਮ� ���� ������ */
            IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate)) THEN
            RUN VerifyStat.
            ELSE
            DO:
               mLstDate = "".
               mSF-OP-Links = "sf-op-pay".
               /* ��।��塞 �� �易��� � ��� �஢���� sf-op-pay */
               mSurrOp = GetLinks(loan.class-code,                      /* ID ����� */
                                  loan.contract + "," + loan.cont-code, /* ID(c��ண��) ��ꥪ� */
                                  ?,                                    /* ���ࠢ����� �裡: s | t | ? */
                                  "sf-op-pay",                          /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                                  ";",                                  /* �������⥫� १������饣� ᯨ᪠ */
                                  ?).
               IF mSurrOp > ""  THEN
               DO mI = 1 TO NUM-ENTRIES(mSurrOp,";"):
                  FIND FIRST op-entry WHERE 
                             op-entry.op       = INT64(ENTRY(1,ENTRY(mI,mSurrOp,";")))
                         AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI,mSurrOp,";")))
                  NO-LOCK NO-ERROR.
                  IF AVAIL op-entry THEN
                     mLstDate = mLstDate + STRING(op-entry.op-date) + ",".
               END.
               /* ��������� ���� ॣ����樨 � ��ਮ� ���� ������ */
               IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate)) THEN
               RUN VerifyStat.
            END.
         END.
         ELSE 
         RUN VerifyStat.
      END.
   END.
   mDopList = "". /* ��� ⮣� �⮡� ����� �뫠 ��ୠ� ��ࠡ�⪠,
                     �������� ����室��� ������ ��६����� ������� ��� ������� �������. */
   teot:
   FOR EACH loan WHERE 
            loan.filial-id   EQ shFilial  
        AND loan.contract    EQ mContract
        AND loan.open-date   <= DataBlock.end-date
        AND (loan.loan-status EQ "����騩"                /* ����� ����� �������� ⮫쪮 ��� ����ᮢ��� ⨯� ������ */
        OR   loan.loan-status EQ "�����")
/*        AND CAN-DO(mBrList, loan.branch-id)*/
   NO-LOCK:
      
      mSgUch = GetXattrValueEx("loan",
                          loan.contract + "," + loan.cont-code,
                          "�������",
                          "").
      
      IF mSgUch = ENTRY(2,mListUch,"/") THEN NEXT teot.

      mLstDate = "".

      IF loan.loan-status EQ "����騩" THEN
      DO:
         mSF-OP-Links = "sf-op-nds".
         /*��।��塞 �� �易��� � ��� �஢���� ��� ��� */
         mSurrOp = GetLinks(loan.class-code,                      /* ID ����� */
                            loan.contract + "," + loan.cont-code, /* ID(c��ண��) ��ꥪ� */
                            ?,                                    /* ���ࠢ����� �裡: s | t | ? */
                            "sf-op-nds",                          /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                            ";",                                  /* �������⥫� १������饣� ᯨ᪠   */
                            ?).
      
         IF mSurrOp > "" THEN
         DO mI = 1 TO NUM-ENTRIES(mSurrOp, ";"):
            FIND FIRST op-entry WHERE 
                       op-entry.op       = INT64(ENTRY(1,ENTRY(mI, mSurrOp, ";")))
                   AND op-entry.op-entry = INT64(ENTRY(2,ENTRY(mI, mSurrOp, ";")))
            NO-LOCK NO-ERROR.
            IF AVAIL op-entry THEN
            mLstDate = mLstDate + STRING(op-entry.op-date) + ",".
         END.
      
         /* ��������� ���� ॣ����樨 � ��ਮ� ���� ������ */
         IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate) EQ NO) THEN
         DO:
            /* �饬 �易��� ������� ��� */
            FIND FIRST bloan WHERE
                       bloan.filial-id EQ loan.filial-id
                   AND bloan.contract  EQ loan.parent-contract
                   AND bloan.cont-code EQ loan.parent-cont-code
            NO-LOCK NO-ERROR.
      
            IF AVAIL bloan THEN
            DO:
               /* ��।��塞, ���� �� � ��䨪� �� ���� ��� ������, � ��⮩, �������饩 � ��ਮ� ����� ������ */
               FOR EACH term-obl WHERE 
                        term-obl.contract  EQ bloan.contract
                    AND term-obl.cont-code EQ bloan.cont-code 
                    AND term-obl.idnt      EQ 4
                    AND term-obl.end-date  >= DataBlock.beg-date
                    AND term-obl.end-date  <= DataBlock.end-date
                    AND term-obl.nn        EQ term-obl.nn
               NO-LOCK:
                  mLstDate = mLstDate + STRING(term-obl.end-date) + ",".
               END.
      
               IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, mLstDate)) THEN
               DO:
                  {buybook#.i &RETURN1 = " return"}
               END.
            END.
         END.
         ELSE
         DO:
            {buybook1.i &RETURN1 = " return"}
         END.
      END.

      IF loan.loan-status EQ "�����" THEN
      IF mSgUch = ENTRY(1,mListUch,"/") THEN RUN VerifyStat.

   END.
END. /*Main_Block*/
{intrface.del}
   
STATUS DEFAULT "".
RETURN "".

/*��ࠡ�⪠ ���-䠪���� (���������� �᭮���� ��ਡ�⮢ ���-䠪����)*/
PROCEDURE CalcSF:
   DEF PARAM BUFFER loan FOR loan.

   IF NOT AVAIL loan THEN RETURN.

   /* �᫨ ���. ४�. ������� �� ࠢ�� "�� �⮡ࠦ���", 
   ** � ��ࠦ��� � ���-䠪��� */
   IF mSgUch <> ENTRY(2,mListUch,"/") THEN
   DO:
      /* �஢��塞 �� �������� */
      mSgPart = GetXattrVAlueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "��������",
                                "������"). /* �� 㬮�砭�� - "������" */

      /* �᫨ ���筮�, � �㦥� 横� */
      IF    mSgPart EQ "����筮�" 
         OR mSgPart EQ "�����������"
      THEN DO:
         /* �������� ���᫥��⢨� ����� �᫨ �� ��� �᫨ ��� � �� �/� */
         IF mOnBook EQ "��"
         OR (loan.cont-type NE "�/�"
             AND mOnBook EQ "���")
         THEN
         DO:

            mSF-OP-Links = "sf-op-pay".
            mSurrOp = GetLinks(loan.class-code,                      /* ID �����     */
                               loan.contract + "," + loan.cont-code, /* ID(c��ண��) ��ꥪ�   */
                               ?,                                    /* ���ࠢ����� �裡: s | t | ? */
                               "sf-op-pay",                          /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                               ";",                                  /* �������⥫� १������饣� ᯨ᪠   */
                               ?).
            
            IF    TRIM(mSurrOp) NE ""
               OR GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "�㬬돫��" ) NE ""  THEN
            DO:
                {buybook1.i &RETURN1 = " return"}.
            END.
         END.
         ELSE
         DO:
            mSF-OP-Links = "sf-op-nds".
            mSurrOp = GetLinks(loan.class-code,                      /* ID �����     */
                               loan.contract + "," + loan.cont-code, /* ID(c��ண��) ��ꥪ�   */
                               ?,                                    /* ���ࠢ����� �裡: s | t | ? */
                               "sf-op-nds",                          /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                               ";",                                 /* �������⥫� १������饣� ᯨ᪠   */
                               ?).
            
            IF    mSurrOp > ""
               OR GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "�㬬돫��" ) NE ""  THEN
            DO:
               {buybook1.i &RETURN1 = " return"}.
            END.
         END.
      END.
      ELSE
      DO:
         IF mOnBook EQ "��" THEN
         DO:
            {buybook#.i &RETURN1 = " return"}
         END.
         ELSE
         IF (DatePeriod(DataBlock.beg-date, DataBlock.end-date, STRING(loan.end-date) + ",")) THEN
         DO:
            {buybook#.i &RETURN1 = " return"}
         END.
      END.
   END. 
END PROCEDURE.


PROCEDURE VerifyStat.

   /* ��ࠡ�⪠ ����� ����� */
   IF loan.loan-status EQ "�����" THEN
   IF (mOnBook EQ "��") THEN
   RUN CalcSf(BUFFER loan).
   ELSE
   DO:
      /* �஢��塞 �� �������� */
      mSgPart = GetXattrVAlueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "��������",
                                "������"). /* �� 㬮�砭�� - "������" */

      /* �᫨ ���筮�, � �㦥� 横� */
      IF mSgPart     EQ "����筮�"
      OR mSgPart     EQ "�����������"
      OR mContract   EQ "sf-in" THEN
      DO:
         mSF-OP-Links = "sf-op-nds".
         /* ��।��塞 �� �易��� � ��� �஢���� ��� ��� */
         mSurrOp = GetLinks(loan.class-code,                      /* ID ����� */
                            loan.contract + "," + loan.cont-code, /* ID(c��ண��) ��ꥪ� */
                            ?,                                    /* ���ࠢ����� �裡: s | t | ? */
                            "sf-op-nds",                          /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                            ";",                                  /* �������⥫� १������饣� ᯨ᪠ */
                            ?).
         IF    mSurrOp > ""
            OR GetXattrVAlue("loan",
                            loan.contract + "," + loan.cont-code,
                            "�㬬돫��" ) NE ""  THEN
         DO:
            {buybook1.i &RETURN1 = " RETURN"}
         END.
      END.
      ELSE
      DO:
         {buybook#.i &RETURN1 = " return"}
      END.
   END.
   
   /* �⡨ࠥ� �� ���-䠪����, ��騥�� ��ࠢ������ ������ ���-䠪���� */
   IF loan.loan-status EQ "���㫨஢��" THEN
   FOR EACH signs WHERE
            signs.CODE       EQ "��ࠢ"
        AND signs.FILE-NAME  EQ "loan" 
        AND signs.code-val   EQ STRING(loan.contract) + "," + 
                                STRING(loan.cont-code)
   NO-LOCK,
   FIRST bloan WHERE 
         bloan.contract   EQ ENTRY(1,signs.surrogate)
     AND bloan.cont-code  EQ ENTRY(2,signs.surrogate)
   NO-LOCK:
      /*��ࠡ�⪠ ���-䠪����*/
      RUN CalcSf(BUFFER bloan).
   END.
   
   IF     loan.loan-status NE "�����" 
      AND loan.loan-status NE "���㫨஢��"
      AND loan.loan-status NE "����騩" THEN
      RUN CalcSF(BUFFER loan).


END PROCEDURE.

/*prosignshkemL4Ql9AYnIoBV+P6OQ*/