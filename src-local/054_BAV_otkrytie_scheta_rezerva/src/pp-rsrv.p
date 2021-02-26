/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-RSRV.P
      Comment: (0006452) ������⥪� ��⮤�� ��� ࠡ��� � ��ࠬ��ࠬ�
               १�ࢨ஢����
   Parameters: ���
         Uses:
      Used by:
      Created: 26.04.2002 17:19 KSV
     Modified: 03.05.2002 13:50 KSV      (0006452) ������⥪� ��⮤�� ��� ࠡ��� � ��ࠬ��ࠬ�
                                         १�ࢨ஢����
     Modified: 13.05.2002 15:30 KSV      (0006452) ��ࠡ�⠭� ��楤��� GetGrRisk � GetPersRsrv,
                                         �������騥 �� 0-�� ���祭�� ��㯯� �᪠ � �����樥��
                                         १�ࢨ஢���� 0-� ���祭�� �����樥�� १�ࢨ஢���� �
                                         ��㯯� �᪠ ᮮ⢥��⢥��� (��� 㤠����� ᮮ⢥������
                                         ��ࠬ��஢).
                                         ��ࠢ���� ��楤�� SaveRsrvParameters, ��� १�ࢠ �ᥣ��
                                         �ਢ�뢠���� ��� ����. �᫨ �����樥�� १�ࢨ஢����
                                         ����� ���祭�� 0, � �� 㤠�����.
                                         ��ࠡ�⠭� ��楤�� Report, ���� �������� �� ����
                                         �᪠.
     Modified: 29.10.2002 15:52 KSV      (0010097) ��ࠡ�⠭� �롮�� ��⮢ ��� �ॣ㫨஢����
                                         १�ࢠ � ��楤�� FillTProv, ��������� ��ࠡ�⪠ ��᮪.
                                         ��ࠡ�⠭ ���� ���⭮� ���� १�ࢠ �१ ������
                                         ���⮪ ��� �᪠ ��� ������� ��⮢.
                                         ��ࠡ�⠭� ��楤�� Report.
     Modified: 13.01.2003 15:12 KSV      (0012706) � ��楤��� FillTProv �������� ��ࠬ��� pOtv,
                                         ������騩 �஢��� �⢥�ᢥ����� �� ��⠬ �᪠,
                                         �������騬 � �롮��. �஢�ઠ ��᪨ ��� ��७�ᥭ� ��
                                         �஢��� ��楢�� ��⮢, �.�. ��᪠ �஢������ ��
                                         ���.����, � �� �� �����ᮢ��.
     Modified: 06.05.2003 12:48 KSV      (0012706) ��ࠢ���� ��।�����
                                         �⢥�ᢥ����� �� ���� � ��楤��
                                         FillTProv. ��������� �㭪�� IsOtv,
                                         ��������� ���� �� ⥪�騩
                                         ���짮��⥫� �⢥�ᢥ��� �� ����.
     Modified: 27.05.2003 12:48 KSV      (0012941) ��ࠡ�⠭� ��楤��
                                         CrAcctRsrv. �ᯮ�짮��� �����㬥��
                                         ᮧ����� ���, ���७ ���祭�
                                         ��ࠬ��஢ ����ᮬ ���뢠����� ���.
                                         ��� ���ଠ�஢��.
     Modified: 20.04.2004     Gunk       ������� ������ ���� ����⨧�樮���� �㬬�
     Modified: 07.07.2004     Gunk       ��ਠ��� ���� ���� - �� ��
     Modified: 25.10.2004 15:28 MDY      (0036909) ������ �맮�� MakeAcct
                                          �� cm_acct_cr
     Modified: 06.09.2005 (0050303) १�ࢨ஢���� �� ����� �������
     Modified: 21.01.2008     jadv       (0085692) ��ࠡ�⠭� FillTProv()
     Modified: 21.01.2008 muta 0085780 283-� ���� १�ࢠ � �� �� ������⢨� ������஢ �� ��㤠� � ���ᥫ�
*/


{globals.i}
{rsrv.def}
{sh-defs.i}
{intrface.get xclass}
{intrface.get instrum}
{intrface.get acct}
{intrface.get strng}
{intrface.get tmess}
{intrface.get comm}
{intrface.get date}
{intrface.get loanx}
{intrface.get i254}
{intrface.get cdrep}
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */
{intrface.get seccd}
{ch_cart.i
   &ChkDate  = yes }
{gr-rsrv.pro}
{xlink.def}
{xlink.pro}

{pfuncdef 
   &NAME="rsrv"
   &LIBDEF=TRUE
   &LibName="�����⥪� �-権 ��� १�ࢨ஢���� ��業⮢"
}


DEFINE STREAM sOut.

&GLOBAL-DEFINE bAcctID  bAcct.acct + "," + bAcct.currency

DEFINE VARIABLE vHSuper  AS HANDLE     NO-UNDO.
DEFINE VARIABLE vAllRec  AS INT64    NO-UNDO.
DEFINE VARIABLE vOpDate  AS DATE       NO-UNDO.
DEFINE VARIABLE vAftProc AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mFillTTOpDate AS LOGICAL NO-UNDO. /* ���� ���஢���� op-date */
DEFINE VARIABLE mChkKartKom   AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE dProv NO-UNDO LIKE tProv.
DEFINE TEMP-TABLE ttOpDate NO-UNDO LIKE op-date.

DEF VAR acCalcBegMonth AS LOG NO-UNDO.
DEF VAR acDayPeriod    AS INT64 NO-UNDO.
DEF VAR acBegLoan      AS LOG NO-UNDO.
DEF VAR asOffCalend    AS LOG NO-UNDO.
DEF VAR mPutProt       AS LOG NO-UNDO. /* �뢮���� �� ��⮪�� ���� � 䠩� calcrsrv.log */
DEF VAR mChkKartB2     AS CHAR NO-UNDO. /* ���᮪ ��2, ��� ������ �஢����� ����稥 �2 �� ������� */
DEF VAR mCopyFrLnOff   AS LOG NO-UNDO.  /* �⪫���� ����஢���� ��ࠬ��஢ १�ࢨ஢���� � �������? */
DEF VAR mFirst         AS LOG NO-UNDO.
/* ���樠������ ����஥� ��� AcTurnover */
ASSIGN
   acCalcBegMonth = FGetSetting("�������", "����⍠猥�","���") =  "��"
   acDayPeriod    = INT64(FGetSetting("�������", "�������ਮ��","30"))
   acBegLoan      = FGetSetting("�������", "��瑤����","��") =  "��"
   asOffCalend    = FGetSetting("�������", "���������","��") =  "��"
   mPutProt       = FGetSetting("rsvrStream","", "") =  "��"
   mChkKartB2     = FGetSetting("����஢�2��2","", "-")
   mCopyFrLnOff   = FGetSetting("����⪫�����","", "-") =  "��"
   mChkKartKom    = FGetSetting("���⊮��ப","", "-") =  "��".
.
   /* � �㭪樨 GetPersReserv �ᯮ������ mPutProt �  STREAM sOut */
{rsrv.pro}

RUN RetStart IN THIS-PROCEDURE.

PROCEDURE SetSuperProc:
  DEFINE INPUT  PARAMETER pH AS HANDLE     NO-UNDO.
  vHSuper = pH.
END.

PROCEDURE SetAfterProcedure:
   DEF INPUT  PARAM iProcName AS CHAR   NO-UNDO.
   vAftProc = iProcName.
END PROCEDURE.

/*��������� �� ��� 0281775
�㭪�� ������뢠�� �㬬� १�ࢨ஢����,
�᫨ �� ��� ��⠭����� �� �283���*/
/*---------------------------------------------------------*/
FUNCTION ReCalcAmtRsrv RETURNS DECIMAL PRIVATE (iAcct     AS CHARACTER,
                                        iCurr     AS CHARACTER,
                                        iAmtRsrv  AS DECIMAL):
   DEFINE BUFFER acct FOR acct.
   DEFINE VARIABLE vDolyaKO283P AS CHARACTER NO-UNDO.
   
   FIND FIRST acct WHERE acct.acct     =  iAcct 
                     AND acct.currency =  iCurr
   NO-LOCK NO-ERROR.
         
   vDolyaKO283P = GetXAttrValueEx ("acct",
                                   acct.acct + "," + acct.currency,
                                   "�283���",
                                   "").
                                                                        
   IF {assigned vDolyaKO283P} THEN
   DO:
      RETURN (iAmtRsrv * DEC(vDolyaKO283P)) / 100.  
   END.
   ELSE
      RETURN iAmtRsrv.                                                                                                                                                 
END FUNCTION. 
/*---------------------------------------------------------*/                                            

/* ����祭�� ����稭� ���⭮� ����  */

FUNCTION GetCalcBase RETURNS DECIMAL (pAcct AS CHAR,
                                      pCurr AS CHAR,
                                      pBeg  AS DATE,
                                      pDate AS DATE):
   DEFINE VARIABLE vSumm     AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vAmor     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDiv      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vNum      AS INT64    NO-UNDO.
   DEFINE VARIABLE vSummAmor AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vf155Dec  AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vTmpDec   AS DECIMAL    EXTENT 6 NO-UNDO.
   DEFINE VARIABLE vBuySell  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vf155Char AS CHARACTER  EXTENT 2 NO-UNDO.
   DEFINE VARIABLE vAlgo     AS LOGICAL    NO-UNDO.

   DEFINE VARIABLE vIInt       AS INT64    NO-UNDO.
   DEFINE VARIABLE vPrefDopRecChar AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bAcct FOR acct.
   DEFINE BUFFER dAcct FOR acct.

   {find-act.i
      &bact = bAcct
      &acct = pAcct
      &curr = pCurr
   }

   /* ���� ᤥ��� - �. 0040791 */
   IF bacct.acct-cat =  "f" THEN DO:
      RUN f155#f.p (bacct.acct,
                    bacct.curr,
                    pBeg,
                    pdate,
                    OUTPUT vSumm,
                    OUTPUT vTmpDec[1],
                    OUTPUT vTmpDec[2],
                    OUTPUT vTmpDec[3],
                    OUTPUT vTmpDec[4],
                    OUTPUT vTmpDec[5],
                    OUTPUT vTmpDec[6],
                    OUTPUT vBuySell
                   ).
      RETURN vSumm.
   END.

   /* 0028069: ���ࠢ��쭮 ��।����� ���⮪ ��� �������� ��⮢ � ���. 280 */
   vSumm = 0.
   IF bAcct.open-date <= pDate AND
      (bAcct.close-date =  ? OR bAcct.close-date >  pDate) THEN DO:

      RUN acct-pos IN h_base (bAcct.acct,
                              bAcct.currency,
                              pDate,
                              pDate,
                              CHR(251)
      ).
      vSumm = IF bAcct.currency  >  "" AND
                 bAcct.rate-type <> "�����८�" 
              THEN 
                 CurToBaseWork ("�������", bAcct.currency, pDate, ABS(sh-val))
              ELSE
                 ABS(sh-bal).
   END.
   /* ��������  �㬬� � ��砥 ������ �� f115_dec, f155_dec ��।������� �������� */

   DO vIInt = 1 TO 2 :
      vPrefDopRecChar = (IF vIInt =  1 THEN "f115" ELSE "f155").
      vf155Char[1] = GetXAttrValueEx ("acct",bAcct.acct + "," + bAcct.curr,vPrefDopRecChar + "_dec","").

      IF vf155Char[1] =  "" THEN
         NEXT.

      ASSIGN
         vf155Char[2] = GetEntries(2,vf155Char[1],"|","")
         vf155Char[2] = REPLACE(vf155Char[2],"AMT","")
         vf155Char[1] = GetEntries(1,vf155Char[1],"|","")
         vf155Dec     = 0
      .
      IF     vf155Char[1] = vPrefDopRecChar + "_acct"
         AND vf155Char[2] <> "" THEN
      DO:
         vf155Dec = DECIMAL (vf155Char[2]) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            vf155Dec = 0.
      END.
      IF vf155Dec <> 0 THEN
         vSumm = vf155Dec.

      LEAVE.

   END.

   ASSIGN
      vAmor = GetXAttrValueEx("acct",{&bAcctID},"acct-amor",?)
      /* �����⨬ ���� १�ࢭ�� ���� */
      vAlgo = fGetSetting ("RsrvType",?,"�᭮����") =  "�᭮����"
   .

   DO vNum = 1 TO (IF vAlgo THEN NUM-ENTRIES (vAmor,"|") ELSE 1):
      ASSIGN
         vAcct = GetEntries (1, ENTRY (vNum, vAmor, "|"),";","")
         vDiv  = GetEntries (2, ENTRY (vNum, vAmor, "|"),";","100")
      .
      IF vAcct >  "" THEN
      DO:
         {find-act.i
            &bact = dAcct
            &acct = vAcct
         }
         IF AVAIL dAcct THEN
         DO:
            IF vDiv BEGINS "AMT:" THEN
            DO:
               /* ��� 60601 - �ᥣ�� ��. */
               ASSIGN
                  sh-bal = DEC (ENTRY (2, vDiv, ":"))
                  vDiv   = "100"
               .
            END.
            ELSE
            DO:
               sh-bal = 0.
               IF dAcct.open-date <= pDate AND
                  (dAcct.close-date =  ? OR dAcct.close-date >  pDate ) THEN DO:
                  RUN acct-pos IN h_base (dAcct.acct,
                                          dAcct.currency,
                                          pDate,
                                          pDate,
                                          CHR(251)
                  ).
               END.
               IF vAlgo THEN
                  sh-bal = sh-bal * DEC (vDiv) / 100.
            END.
            /* ���⠥� ����⠭��� �㬬� ����⨧�樨 */
            IF vAlgo
            THEN vSumm = MAX(0,(vSumm - ABS(sh-bal))).
            ELSE vSumm = MAX(0,(vSumm - ABS (sh-bal)) * DEC (vDiv) / 100).
         END.
      END.
   END.
   RUN RecalcRsrvBase_Oblig(pAcct,
                            pCurr,
                            pDate,
                            INPUT-OUTPUT vSumm).
   /*20209*/
   IF bacct.bal-acct =  20209 THEN
   DO:
      /*��室�騩 �� ᥣ����*/
      ASSIGN
         sh-bal = 0.
      RUN acct-pos IN h_base (bAcct.acct,
                              bAcct.currency,
                              pDate,
                              pDate,
                              CHR(251)
      ).
      vSumm = sh-bal.
      /*ᥣ����*/
      ASSIGN
         sh-db  = 0.
      RUN acct-pos IN h_base (bAcct.acct,
                              bAcct.currency,
                              pDate,
                              pDate,
                              CHR(251)
      ).
      vSumm = vSumm - sh-db.
      /*���*/
      ASSIGN
         sh-db  = 0.
      RUN acct-pos IN h_base (bAcct.acct,
                              bAcct.currency,
                              pDate - 1,
                              pDate - 1,
                              CHR(251)
      ).
      vSumm = vSumm - sh-db.
      /*�������*/
      ASSIGN
         sh-db  = 0.
      RUN acct-pos IN h_base (bAcct.acct,
                              bAcct.currency,
                              pDate - 2,
                              pDate - 2,
                              CHR(251)
      ).
      vSumm = vSumm - sh-db.
      vSumm = MAX(0.0, vSumm).
   END.

   /*
      ��� ��⮢ �� �� ���琥���.�����८� ���뢠��
      �㡫�� �������� ���⪠ �� ���� ��८業��
   */
   IF CAN-DO(FGetSetting("���琥���", "�����८�", ""), bAcct.acct) THEN
      vSumm = vSumm + getPereoc(bAcct.acct, pDate, YES).

   RETURN MAX (0.0, vSumm).
END.

/* ����� १�ࢠ ��⥭��� ���ᥫ�� �� ���᫥��� ��業⠬/��᪮�⠬ */
FUNCTION GetCalcBaseVeks RETURNS DECIMAL (pAcct AS CHAR,
                                          pCurr AS CHAR,
                                          pBeg  AS DATE,
                                          pDate AS DATE,
                                          pRsrv AS DECIMAL):  /*�����樥�� १�ࢨ஢����*/
   DEFINE VARIABLE vSumm     AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vAmor     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcct     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDiv      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vNum      AS INT64    NO-UNDO.
   DEFINE VARIABLE vSummAmor AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vf155Dec  AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vTmpDec   AS DECIMAL    EXTENT 6 NO-UNDO.
   DEFINE VARIABLE vBuySell  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vf155Char AS CHARACTER  EXTENT 2 NO-UNDO.
   DEFINE VARIABLE vAlgo     AS LOGICAL    NO-UNDO.

   DEFINE VARIABLE vGrPrD    AS INT64       NO-UNDO.  /* ��㯯� �᪠ �� "���ਧ���" */
   DEFINE VARIABLE vGRsrv    AS DECIMAL   NO-UNDO.  /* ��⥣��� ����⢠          */
   DEFINE VARIABLE vAcct-lbt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vAmnt     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vGr-Risk  AS INT64  NO-UNDO.   /* ��㯯� �᪠ */
   DEFINE VARIABLE vRate     AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vDate     AS DATE NO-UNDO.   /* ��� ᬥ�� ��㯯� �᪠ */

   DEFINE VARIABLE vIInt       AS INT64    NO-UNDO.
   DEFINE VARIABLE vPrefDopRecChar AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bAcct FOR acct.
   DEFINE BUFFER dAcct FOR acct.
   DEFINE BUFFER comm-rate FOR comm-rate.
   DEFINE BUFFER op-entry  FOR op-entry.

   {find-act.i
      &bact = bAcct
      &acct = pAcct
      &curr = pCurr
   }

   /* ���� ᤥ��� - �. 0040791 */
   IF bacct.acct-cat =  "f" THEN DO:
      RUN f155#f.p (bacct.acct,
                    bacct.curr,
                    pBeg,
                    pdate,
                    OUTPUT vSumm,
                    OUTPUT vTmpDec[1],
                    OUTPUT vTmpDec[2],
                    OUTPUT vTmpDec[3],
                    OUTPUT vTmpDec[4],
                    OUTPUT vTmpDec[5],
                    OUTPUT vTmpDec[6],
                    OUTPUT vBuySell
                   ).
      RETURN vSumm.
   END.

   /* 0028069: ���ࠢ��쭮 ��।����� ���⮪ ��� �������� ��⮢ � ���. 280 */
   vSumm = 0.
   IF bAcct.open-date <= pDate AND
      (bAcct.close-date =  ? OR bAcct.close-date >  pDate) THEN DO:

      RUN acct-pos IN h_base (bAcct.acct,
                              bAcct.currency,
                              pDate,
                              pDate,
                              CHR(251)
      ).
      vSumm = IF bAcct.currency = "" THEN
                 ABS(sh-bal)
              ELSE
                 CurToBaseWork ("�������", bAcct.currency, pDate, ABS(sh-val)).

      /* �㦭� ᪮�४�஢��� ���⮪ � ��⮬ ��⥣�ਨ ����⢠ */
      vGrPrD = INT64(FGetSetting("�302�", "���ਧ���", "2")).

      RUN GetGrRiskOnDate(pRsrv,
                          pDate,
                          OUTPUT vGRsrv). /* ��⥣��� ����⢠ */

      IF vGRsrv > vGrPrD  THEN DO:

         vDate = ?.

         cycle:
         FOR EACH comm-rate WHERE
                  comm-rate.acct       =  bAcct.acct
              AND comm-rate.currency   =  bAcct.currency
              AND comm-rate.commission =  "%१"
         NO-LOCK BY comm-rate.since DESCENDING:
            RUN GetAcctRisk  (bAcct.acct,
                              bAcct.currency,
                              comm-rate.since,
                              OUTPUT vRate,
                              OUTPUT vGr-Risk).

               /* �஢��塞 ��㯯� �᪠ �� ��।��������� ��室� */
            IF vGr-Risk <= vGrPrD THEN DO:
               vDate = comm-rate.since.
               LEAVE cycle.
            END.
         END.
         IF vDate =  ? THEN  vSumm = 0.0.
         ELSE DO:
            vAmnt = 0.0.
            FOR EACH op-entry WHERE
                     op-entry.acct-db   =  bAcct.acct      AND
                     op-entry.op-date   >  vDate           AND
                     op-entry.op-date   <= pDate           AND
                     op-entry.op-status >= "�"             AND
                 NOT op-entry.acct-cr   BEGINS "706"
            NO-LOCK:

               vAmnt = vAmnt + op-entry.amt-rub.
            END.
            vSumm = vSumm - (IF bAcct.currency = "" THEN ABS(vAmnt)
                             ELSE CurToBaseWork ("�������", bAcct.currency, pDate, ABS(vAmnt))).

         END.
      END.
   END.
   /* ��������  �㬬� � ��砥 ������ �� f115_dec, f155_dec ��।������� �������� */

   DO vIInt = 1 TO 2:
      vPrefDopRecChar = (IF vIInt =  1 THEN "f115" ELSE "f155").
      vf155Char[1] = GetXAttrValueEx ("acct",bAcct.acct + "," + bAcct.curr,vPrefDopRecChar + "_dec","").

      IF vf155Char[1] =  "" THEN
         NEXT.
      
      ASSIGN
         vf155Char[2] = GetEntries(2,vf155Char[1],"|","")
         vf155Char[2] = REPLACE(vf155Char[2],"AMT","")
         vf155Char[1] = GetEntries(1,vf155Char[1],"|","")
         vf155Dec     = 0
      .
      IF     vf155Char[1] = "f155_acct"
         AND vf155Char[2] <> "" THEN
      DO:
         vf155Dec = DECIMAL (vf155Char[2]) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            vf155Dec = 0.
      END.
      IF vf155Dec <> 0 THEN
         vSumm = vf155Dec.

      LEAVE.

   END.

   ASSIGN
      vAmor = GetXAttrValueEx("acct",{&bAcctID},"acct-amor",?)
      /* �����⨬ ���� १�ࢭ�� ���� */
      vAlgo = fGetSetting ("RsrvType",?,"�᭮����") =  "�᭮����"
   .

   DO vNum = 1 TO (IF vAlgo THEN NUM-ENTRIES (vAmor,"|") ELSE 1):
      ASSIGN
         vAcct = GetEntries (1, ENTRY (vNum, vAmor, "|"),";","")
         vDiv  = GetEntries (2, ENTRY (vNum, vAmor, "|"),";","100")
      .
      IF vAcct >  "" THEN
      DO:
         {find-act.i
            &bact = dAcct
            &acct = vAcct
         }
         IF AVAIL dAcct THEN
         DO:
            IF vDiv BEGINS "AMT:" THEN
            DO:
               ASSIGN
                  sh-bal = DEC (ENTRY (2, vDiv, ":"))
                  vDiv   = "100"
               .
               IF dAcct.Curr >  "" THEN
                  sh-bal = CurToBaseWork ("�������",dAcct.Curr, pDate, sh-bal).
            END.
            ELSE
            DO:
               sh-bal = 0.
               IF dAcct.open-date <= pDate AND
                  (dAcct.close-date =  ? OR dAcct.close-date >  pDate ) THEN DO:
                  RUN acct-pos IN h_base (dAcct.acct,
                                          dAcct.currency,
                                          pDate,
                                          pDate,
                                          CHR(251)
                  ).

               END.

               IF vAlgo THEN
                  sh-bal = sh-bal * DEC (vDiv) / 100.
            END.

            /* ���⠥� ����⠭��� �㬬� ����⨧�樨 */
            IF vAlgo
            THEN vSumm = MAX(0,(vSumm - ABS(sh-bal))).
            ELSE vSumm = MAX(0,(vSumm - ABS (sh-bal)) * DEC (vDiv) / 100).
         END.
      END.
   END.
   RUN RecalcRsrvBase_Oblig(pAcct,
                            pCurr,
                            pDate,
                            INPUT-OUTPUT vSumm).
   RETURN MAX (0.0, vSumm).

END.

/*
 �㬬� ���ᯥ祭�� �� ��
*/
FUNCTION getAcctSupply RETURNS DECIMAL
   (INPUT iAcct AS CHARACTER,
    INPUT iCurr AS CHARACTER,
    INPUT iDate AS DATE):

   DEFINE VARIABLE vRetVal AS DECIMAL    NO-UNDO.
   /* �뭥ᥭ� �� ���譨� ��������� ����� ᫮����� ���⥪�� */
   RUN getacctsupp.p (       iAcct,
                             iCurr,
                             iDate,
                      OUTPUT vRetVal).

   RETURN vRetVal.
END FUNCTION.

/* ����祭�� ����稭� �������⮣� �᪠ */
FUNCTION UncoverRiskAmount RETURNS DECIMAL (pAcct AS CHAR,
                                            pCurr AS CHAR,
                                            pDate AS DATE):

   RETURN MAX(GetCalcBase(pAcct,pCurr,pDate,pDate) - getAcctSupply(pAcct,pCurr,pDate),0.00).
END.

/* ����祭�� ����稭� ���⭮�� १�ࢠ */
FUNCTION CalcReserveAmount RETURNS DECIMAL (pAcct AS CHAR,
                                            pCurr AS CHAR,
                                            pDate AS DATE):

   RETURN ROUND(GetCalcBase(pAcct,pCurr,pDate,pDate) * (GetPersReserv(pAcct,pCurr,pDate) / 100),2).
END.

/* ����祭�� ����稭� �ନ�㥬��� १�ࢠ */
FUNCTION DesReserveAmount RETURNS DECIMAL (pAcct AS CHAR,
                                           pCurr AS CHAR,
                                           pDate AS DATE):
   DEFINE VARIABLE vRetVal AS DECIMAL NO-UNDO.
   vRetVal = IF GetPersReserv(pAcct,pCurr,pDate) =  ? THEN 0 ELSE GetPersReserv(pAcct,pCurr,pDate).
   RETURN ROUND(UncoverRiskAmount(pAcct,pCurr,pDate) * (vRetVal / 100),2).
END.



FUNCTION GetEnoughRsrv RETURNS DECIMAL (pDate AS DATE):
  DEFINE VARIABLE vERsrv AS DECIMAL    NO-UNDO.

  DEFINE BUFFER dCode FOR CODE.
  FOR EACH dCode WHERE
     dCode.class        = "���137�%" AND
     dCode.parent       = "���137�%" AND
     DATE(dCode.code)  <= pDate      NO-LOCK
     BY DATE(dCode.code) DESC:
     LEAVE.
  END.
  IF NOT AVAIL dCode THEN RETURN 0.0.
  vERsrv = DEC(dCode.val) NO-ERROR.
  RETURN IF ERROR-STATUS:ERROR THEN 0.0 ELSE vERsrv.
END.
/*----------------------------------------------------------------------------*/
/* ����祭�� ����稭� १�ࢠ �� �᭮����� ��।���� ���⭮� ����           */
/* � ��⮬. ���� �� �஢����� ���㣫����                                     */
/*----------------------------------------------------------------------------*/
FUNCTION getRsrvAmountBaseR RETURNS DECIMAL
   (INPUT pAcct    AS CHAR,
    INPUT pCurr    AS CHAR,
    INPUT pDate    AS DATE,
    INPUT pPRsrv   AS DEC,
    INPUT pClcBase AS DEC,
    INPUT pRound   AS LOG):

   DEFINE VARIABLE vRet       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vORsrvRate AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vPP        AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vBal       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vMaxBal    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vOpCode    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpName    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEvalMode  AS CHARACTER NO-UNDO.

   IF pRound THEN
      vRet = ROUND(pClcBase * (pPRsrv / 100) * (GetEnoughRsrv(pDate) / 100),2).
   ELSE
      vRet = pClcBase * (pPRsrv / 100) * (GetEnoughRsrv(pDate) / 100).

   vORsrvRate = AcORsrvRate(pAcct, pCurr, pDate).

   RUN AcORsrvCalcBase(pAcct, pCurr, pDate,
                       OUTPUT vPP, OUTPUT vBal, OUTPUT vMaxBal,
                       OUTPUT vOpCode, OUTPUT vOpName, OUTPUT vEvalMode).

   IF vEvalMode =  "��" THEN DO:
      IF pRound THEN
         vRet = MAXIMUM(vRet, ROUND(vMaxBal * vORsrvRate / 100, 2)).
      ELSE
         vRet = MAXIMUM(vRet, (vMaxBal * vORsrvRate) / 100).
   END.
   ELSE DO:
       IF vEvalMode =  "�" THEN DO:
           IF pRound THEN
               vRet = MAXIMUM(vRet, ROUND(vBal * vORsrvRate / 100, 2)).
           ELSE
               vRet = MAXIMUM(vRet, (vBal * vORsrvRate) / 100).
       END.
       ELSE DO:
           IF pRound THEN
               vRet = MAXIMUM(vRet, ROUND(vPP * vORsrvRate / 100, 2)).
           ELSE
               vRet = MAXIMUM(vRet, (vPP * vORsrvRate) / 100).
       END.
   END.

   RETURN vRet.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* ����祭�� ����稭� १�ࢠ �� �᭮����� ��।���� ���⭮� ����           */
/*----------------------------------------------------------------------------*/
FUNCTION getRsrvAmountBase RETURNS DECIMAL
   (INPUT pAcct    AS CHAR,
    INPUT pCurr    AS CHAR,
    INPUT pDate    AS DATE,
    INPUT pPRsrv   AS DEC,
    INPUT pClcBase AS DEC):

   DEFINE VARIABLE vRet       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vORsrvRate AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vPP        AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vBal       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vMaxBal    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vOpCode    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpName    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEvalMode  AS CHARACTER NO-UNDO.

   vRet = getRsrvAmountBaseR(pAcct, pCurr, pDate, pPRsrv, pClcBase, YES).

   RETURN vRet.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* ����祭�� ����稭� १�ࢠ                                                 */
/*----------------------------------------------------------------------------*/
FUNCTION GetRsrvAmount RETURNS DECIMAL (pAcct   AS CHAR,
                                        pCurr   AS CHAR,
                                        pDate   AS DATE,
                                        pPRsrv  AS DEC):
   RETURN getRsrvAmountBase (pAcct,
                             pCurr,
                             pDate,
                             pPRsrv,
                             GetCalcBase(pAcct,pCurr,pDate,pDate)).
END.
/*------------------------------------------------------------------------------
  Purpose:     ��।���� ���� �� �⢥��⢥��� �� ��� ���稭���� ⥪�饬�
               ���짮��⥫� ��⥬�
  Parameters:  iAcctUser - �⢥��⢥��� �� ���
  Notes:
------------------------------------------------------------------------------*/
FUNCTION IsOtv RETURNS LOGICAL (iAcctUser AS CHAR):
   DEFINE VARIABLE vUsers  AS CHARACTER  NO-UNDO.

   vUsers = GetSlaves().
   IF NOT CAN-DO(vUsers,USERID("bisquit")) THEN
      vUsers = USERID("bisquit") + (IF vUsers = "" THEN "" ELSE ",") + vUsers.
   RETURN CAN-DO(vUsers,iAcctUser).

END FUNCTION.

PROCEDURE FillTProv:
   DEFINE INPUT  PARAMETER pCClass  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER pDate    AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER pBefDate AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER pMask    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER pOtv     AS LOGICAL   NO-UNDO.
   DEFINE INPUT  PARAMETER pMaskDls AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER pShowAll AS LOGICAL   NO-UNDO. /* �⡨��� ��� � �㫥�� ���⪮� */
   DEFINE OUTPUT PARAMETER TABLE   FOR tProv.

   DEFINE  VARIABLE vAcct     AS CHARACTER NO-UNDO.
   DEFINE  VARIABLE vSumm     AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vRSum     AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vI        AS INT64   NO-UNDO.
   DEFINE  VARIABLE vList     AS CHARACTER NO-UNDO.
   DEFINE  VARIABLE vL        AS LOGICAL   .
   DEFINE  VARIABLE vPRsrv    AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vPRsrv1   AS DECIMAL   NO-UNDO. /* �����.१�ࢨ�.��� ��⥣.���.*/
   DEFINE  VARIABLE vRsrvRate AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vGRsrv    AS INT64   NO-UNDO.
   DEFINE  VARIABLE vMax      AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vMin      AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vPos      AS INT64   NO-UNDO.
   DEFINE  VARIABLE vNom      AS INT64   NO-UNDO.
   DEFINE  VARIABLE vTotal    AS INT64   NO-UNDO.
   DEFINE  VARIABLE vCustName     AS CHARACTER INIT   ? NO-UNDO.
   DEFINE  VARIABLE vAcctNameChar AS CHARACTER EXTENT 2 NO-UNDO. /* ������������ ����� ��� */
   DEFINE  VARIABLE vError    AS LOGICAL   NO-UNDO. /* ���� �訡��. */
   DEFINE  VARIABLE vResult   AS CHARACTER NO-UNDO. /* ������� ᮧ����� �裡. */
   DEFINE  VARIABLE vRiskType AS CHARACTER NO-UNDO. /* ��� �᪠ */
   DEFINE  VARIABLE vAcct-lbt AS CHARACTER NO-UNDO.
   DEFINE  VARIABLE vOsnSumm  AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vResSumm  AS DECIMAL   NO-UNDO.
   DEFINE  VARIABLE vRoleLst  AS CHARACTER NO-UNDO.
   DEFINE  VARIABLE vNpOkr    AS CHARACTER NO-UNDO.
   DEFINE  VARIABLE vVeksFlg  AS LOGICAL   NO-UNDO. /* �ਧ��� �ਭ��������� � ���ᥫ� */
   DEFINE  VARIABLE vUniBag   AS CHAR      NO-UNDO. /* ���祭�� �� ��� "UniformBag" */
   DEFINE  VARIABLE vRlRsrv   AS DECIMAL   NO-UNDO. /* ॠ��� ��業� १�ࢨ஢���� (�⭮襭�� ��ନ஢������ �� ��㤥 १�ࢠ � �㬬� �᭮����� ����� * 100) */
   DEFINE  VARIABLE vCustCat  AS CHARACTER NO-UNDO.
   DEFINE  VARIABLE vNotFrK2  AS LOGICAL   NO-UNDO. /* �������஢��� ����稥 �2 �� ������� */
   DEFINE VARIABLE mNeNach283  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mP283Nach   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mP283NeNach AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vP283Iskl   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vP283Date5  AS DATE      NO-UNDO.
   DEFINE VARIABLE vP283Alg5   AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE vIsOffshore AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vPP         AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vBal        AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vMaxBal     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vOpCode     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOpName     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEvalMode   AS CHARACTER NO-UNDO.

   DEFINE BUFFER bCode FOR CODE.
   DEFINE BUFFER bBA FOR bal-acct.
   DEFINE BUFFER bAcct FOR acct.
   DEFINE BUFFER dAcct FOR acct.
   DEFINE BUFFER zAcct FOR acct.
   DEFINE BUFFER bLA FOR loan-acct.
   DEFINE BUFFER dLA FOR loan-acct.
   DEFINE BUFFER cust-role FOR cust-role.

   EMPTY  TEMP-TABLE tProv.

   vRoleLst = FGetSetting("���犐",?,"").

   IF pMask = "" OR pMask = ? THEN pMask = "*".
   IF pMaskDls = "" OR pMaskDls = ? THEN pMaskDls = "*".

   SELECT COUNT(*) INTO vTotal FROM CODE WHERE
      CODE.class   = pCClass AND
      CODE.parent  = pCClass .

   RUN SetStatus(0,vTotal).
   vAllRec = 0.
   vOpDate = pDate.

   ACCT:
   FOR EACH bCode WHERE
        bCode.class  = pCClass AND
        bCode.parent = pCClass NO-LOCK,
       FIRST bBA WHERE
          bBA.bal-acct = INT64(ENTRY(1,bCode.code,"_")) NO-LOCK,
       EACH bAcct OF bBA WHERE
          bAcct.open-date <= pDate AND
          (bAcct.close-date > pDate OR
           bAcct.close-date =  ?) AND
          CAN-DO(pMask,bAcct.acct) AND
          CAN-DO(pMaskDls,bAcct.contract) AND
          bAcct.filial-id = shFilial
   NO-LOCK
   BREAK BY bCode.code:

      IF FIRST-OF(bCode.code) THEN
      DO:
         vPos = vPos + 1.
         RUN SetStatus(vPos,vTotal).
         IF RETURN-VALUE = "STOP" THEN LEAVE ACCT.
      END.

      IF CAN-FIND(FIRST tProv WHERE tProv.acct-risk = bAcct.acct NO-LOCK)
      THEN NEXT.

      IF pOtv AND NOT IsOtv(bAcct.user-id) THEN NEXT.

      IF NUM-ENTRIES(bCode.code,"_") > 1 THEN  vRiskType = ENTRY(2,bCode.code,"_").

      vNom = vNom + 1.

      RELEASE bLA.
      RELEASE dLA.
      RELEASE dAcct.

      ASSIGN
         vPRsrv = ?
         vGRsrv = ?.

      vSumm = GetCalcBase(bAcct.acct,bAcct.currency,pDate,pDate).

      /* ��।����� ��� १�ࢠ, �����-� १�ࢨ஢���� � ��㯯� �᪠ */
      vAcct = GetLinks ("acct", {&bAcctID}, ?, "acct-reserve", "", pDate).
      IF {assigned vAcct} THEN
      DO:
         {find-act.i
            &bact = dAcct
            &acct = ENTRY(1,vAcct)
            &curr = ENTRY(2,vAcct)
         }
                        /* �������� �裡, �᫨ �� ������ ��� १�ࢠ. */
         IF NOT AVAIL dAcct
         THEN DO:
            vAcct = ?.
            RUN DelLinksCode IN h_xclass (       "acct",
                                                 "acct-reserve",
                                                 {&bAcctID},
                                                 "",
                                                 "S",
                                          OUTPUT vError).
         END.
      END.
      /*�᫨ ��⠭����� �� "�᪫283" == ��, � ��� �ய�᪠����*/
      ASSIGN
         vP283Iskl = GetXAttrValueEx("acct",bAcct.acct + "," + bAcct.currency,"�᪫283","").
      IF vP283Iskl =  "��" THEN NEXT ACCT.
      /* ��।����� �易����� � ��⮬ �᪠ ������஬
      ** �᫨ ������� ������, � ��।��塞 ��㯯� �᪠ � ��業�
      ** � bCode.code ����� ������ ���� <���.����᪠>_<஫� ��� �᪠> */
      IF NUM-ENTRIES(bCode.code,"_") = 2 THEN
      DO:
         vList = ENTRY(2,bCode.code,"_").
         LOOP:
         DO vI = 1 TO NUM-ENTRIES(vList):
            FOR EACH bLA OF bAcct WHERE
                       bLA.acct-type  = ENTRY(vI,vList) AND
                       bLA.since     <= pDate           NO-LOCK BY bLA.since DESC:
               LEAVE LOOP.
            END.
         END.

         /* �᫨ � �����䨪��� ��⠭����� �ਧ��� �᪫���� � ��� �ਢ易� � ��������,
             � ⠪�� ��� �� ��ࠡ��뢠����*/
         IF AVAIL(bLa) AND bCode.misc[1] =  "��" THEN NEXT ACCT.

         
         /* �᫨ � �� �����283� ��������� ��� ��ண� ���浪�, �� १�ࢨ஢���� �ய�᪠���� ���, � ������ �� ����� �� 283��� == ��.
            �᫨ ��� ��������� � �� - �� १�ࢨ஢���� �ய�᪠���� ���, � ������ ����� �� 283����� == ��. (0216120) */
         ASSIGN
            mNeNach283  = FGetSetting("�����283","","")
            mP283Nach   = GetXAttrValueEx("acct", bAcct.acct + "," + bAcct.currency, "�283���", "")
            mP283NeNach = GetXAttrValueEx("acct", bAcct.acct + "," + bAcct.currency, "�283�����", "").
         IF     CAN-DO(mNeNach283, TRIM(STRING(bAcct.bal-acct))) THEN IF mP283Nach   <> "��" THEN NEXT ACCT.
         IF NOT CAN-DO(mNeNach283, TRIM(STRING(bAcct.bal-acct))) THEN IF mP283NeNach =  "��" THEN NEXT ACCT.
         
         /* ��� bLA not avail, ����� ��� �� �ਢ易� � ��������
         ** � bCode.val ����� ������ ���� <���.���१�ࢠ>_<஫� ��� १> */
         IF AVAIL bLA AND NUM-ENTRIES(bCode.val,"_") = 2 THEN
         DO:
            FIND LAST dLA WHERE
               dLA.contract    = bLA.contract             AND
               dLA.cont-code   = bLA.cont-code            AND
               dLA.acct-type   = ENTRY(2,bCode.val,"_")   AND
               dLA.since      <= pDate NO-LOCK NO-ERROR.

            IF AVAIL dLA AND ENTRY (1, vAcct) <> dLA.acct THEN
            DO:
               RUN LinkAcctReserve({&bAcctID},
                                   dLA.acct + "," + dLA.currency,
                                   pDate,
                                   OUTPUT vResult).
               IF LENGTH (vResult) >  0 THEN
                  vAcct = dLA.acct + "," + dLA.currency.
            END.
         END.
      END.

      ASSIGN
         vNotFrK2    = yes
         vIsOffshore = no
      .
      FIND FIRST cust-role WHERE cust-role.file-name  =  "acct"
                                    AND cust-role.surrogate  =  bAcct.acct + "," + bAcct.currency
                                    AND cust-role.class-code =  "acct-cust-off-shore"
      NO-LOCK NO-ERROR.
      IF AVAIL cust-role AND LOOKUP(cust-role.cust-cat,"�,�,�") >  0
      THEN DO:
         ASSIGN
            vOpName = ENTRY(LOOKUP(cust-role.cust-cat,"�,�,�"),"cust-corp,person,banks")
            vIsOffshore = GetXAttrValueEx(vOpName,STRING(cust-role.cust-id),"����","") <> ""
         .
         IF vIsOffshore THEN DO:
            vPRsrv = AcORsrvRate(bAcct.acct, bAcct.currency, pDate).
            RUN GetGrRiskOnDate IN THIS-PROCEDURE(vPRsrv,
                                                  pDate,
                                                  OUTPUT vGRsrv). /* ��⥣��� ����⢠ */
            RUN AcORsrvCalcBase IN THIS-PROCEDURE(bAcct.acct, bAcct.currency, pDate,
                                                  OUTPUT vPP, OUTPUT vBal, OUTPUT vMaxBal,
                                                  OUTPUT vOpCode, OUTPUT vOpName, OUTPUT vEvalMode).
         
            IF vEvalMode =  "��" THEN 
               vRsrvRate = vMaxBal.
            ELSE 
               IF vEvalMode =  "�" THEN
                  vRsrvRate = vBal.
               ELSE 
                  vRsrvRate = vPP.
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '���������� : ����� १�ࢠ ��� ��� ' (IF AVAIL bAcct THEN bAcct.acct ELSE '') 
                  ' �� ����� ��� ��� १����⮢ ������ ���.' SKIP
                  '����⭠� ����: ' TRIM(STRING(vRsrvRate, "->>>>>>>>>>>>>>9.99")) SKIP
                  '��� १�ࢠ: ' (IF AVAIL dAcct THEN dAcct.acct ELSE '')
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
            vRsrvRate = ROUND(vRsrvRate * vPRsrv / 100, 2).
         END.
      END.

      IF NOT vIsOffshore THEN DO:
      IF CAN-DO(mChkKartB2,STRING(bAcct.bal-acct)) AND (NOT AVAIL bLA OR mCopyFrLnOff) THEN
      DO:
         RUN CliChkCart2(INPUT bAcct.cust-cat,INPUT bAcct.cust-id,INPUT pDate,OUTPUT vL).
         If vL THEN DO: /* ���� ����⥪�, ��⠭�������� �-� १ 100% � 5-� ��⥣��� ����⢠ */
            ASSIGN
               vPRsrv = 100
               vRsrvRate = UncoverRiskAmount(bAcct.acct, bAcct.currency,pDate)
            .
            RUN GetGrRiskOnDate(vPRsrv,
                                pDate,
                                OUTPUT vGRsrv). /* ��⥣��� ����⢠ */
            vNotFrK2 = no.
            IF AVAIL bLA THEN
               RELEASE bLa.
            /* ��⮪���஢����, �᫨ �� "rsrvStream" = �� */
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '���������� : ����� १�ࢠ ��� ��� ' (IF AVAIL bAcct THEN bAcct.acct ELSE '') ' �� ������ �2.' SKIP
                  '����⭠� ����: ' TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
                  '��� १�ࢠ: ' (IF AVAIL dAcct THEN dAcct.acct ELSE '')
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.
      END.

      IF vNotFrK2 THEN     /* ����� ������, ��� ��� ����⥪� */
      /* �᫨ ��諨 �易��� � ��⮬ �������, � ��㯯� �᪠ � ��業� ��६ � ����,
      ** ( ⮫쪮 � ��砥, �᫨ ஫� ��� 㪠���� � �� ���犐 ) ���� � ��� */
      IF    AVAIL bla
        AND CAN-DO(vRoleLst,bla.acct-type) THEN
      DO:
            /* ��⠥� �⠭����� ��ࠧ��, ����. १�ࢨ஢���� + ��.�᪠ ������ � ������� (vPRsrv - ����. १) */
         vPRsrv = LnRsrvRate (bla.contract,
                              bla.cont-code,
                              pDate).
         vRsrvRate = UncoverRiskAmount(bla.acct,bla.currency,pDate) * vPRsrv / 100.

         RUN LnGetGrRiskUchBag (bla.contract,
                                bla.cont-code,
                                vPRsrv,
                                pDate,
                                OUTPUT vGRsrv).
            /* ��⮪���஢����, �᫨ �� "rsrvStream" = �� */
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               '���������� : ����� १�ࢠ ��� ��� ' bacct.acct ' �� ����� �������.' SKIP
               '��� १�ࢠ: '   (IF AVAIL dAcct THEN dAcct.acct ELSE ' �� ������') SKIP
               '����⭠� ����: ' TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
               '����: '           (IF AVAIL bla THEN bla.acct-type ELSE ' �� 㪠����') SKIP
               '�������: '        (IF AVAIL bla THEN ENTRY(1, bla.cont-code, "@") ELSE ' �� ��।����') SKIP
               '%��� ���.: '      TRIM(STRING(vPRsrv, "->>>>>>>>>>>>>>9.99999"))
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.
      END.

      ELSE
      DO:
            /* ��⮪���஢����, �᫨ �� "rsrvStream" = �� */
         IF mPutProt THEN
         DO:
            OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
            PUT STREAM sOut UNFORMATTED
               '���������� : ����� १�ࢠ ��� ��� ' (IF AVAIL bAcct THEN bAcct.acct ELSE '') ' �� ����� ���.' SKIP
               '����⭠� ����: ' TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
               '��� १�ࢠ: ' (IF AVAIL dAcct THEN dAcct.acct ELSE '')
            SKIP.
            OUTPUT STREAM sOut CLOSE.
         END.

         vAcct-lbt = GetXattrValue("acct", bAcct.acct + "," + bAcct.currency, "acct-lbt").
         IF {assigned vAcct-lbt} THEN
         DO:
            {find-act.i
               &bact = zAcct
               &acct = vAcct-lbt
            }
            /* ��⮪���஢����, �᫨ �� "rsrvStream" = �� */
            IF NOT AVAIL zAcct AND mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '������ : ����� १�ࢠ ��� ��� ' bAcct.acct SKIP
                  '�������� � �� Acct-lbt ���: ' vAcct-lbt SKIP ' �� ������!'
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.
         END.

            /* ��।���� ���� �� ��� ���ᥫ�� */
         vVeksFlg = FALSE. /* ����뢠�� 䫠� �ਭ��������� � ���ᥫ� */
         
         IF ENTRY(1, bCode.code,"_") BEGINS "51" THEN
         DO:
            IF AVAIL zAcct THEN
            DO:
               BLK:
               DO vI = 1 TO NUM-ENTRIES(vRiskType):
                  IF CAN-DO("�।�,�।��%,�।��᪄�,�।��᪏�", ENTRY(vI, vRiskType)) THEN
                  DO:
                     vVeksFlg = TRUE.
                     LEAVE BLK.
                  END.
               END.
            END.
         END.
         
         
         IF vVeksFlg THEN
         DO:                                   
            ASSIGN
                vPRsrv    = GetPersReserv(zacct.acct,zacct.curr,pDate)                
                vSumm     = GetCalcBase(zAcct.acct, zAcct.currency, pDate, pDate)
                vResSumm  = GetAcctReserve(zAcct.acct, zAcct.currency, pDate)              
                .
                               
            /* ��⮪���஢����, �᫨ �� "rsrvStream" = �� */
            IF mPutProt THEN
            DO:
               OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
               PUT STREAM sOut UNFORMATTED
                  '��� �᭮����� �����: ' zAcct.acct SKIP
                  '���⮪ �� ��: '        TRIM(STRING(vSumm, "->>>>>>>>>>>>>>9.99")) SKIP
                  '�㬬� १�ࢠ �� ��: '  TRIM(STRING(vResSumm, "->>>>>>>>>>>>>>9.99")) SKIP
                  '%��� ��� �� ��: '     TRIM(STRING(vPRsrv, "->>>>>>>>>>>>>>9.99"))
               SKIP.
               OUTPUT STREAM sOut CLOSE.
            END.

         END. /* vVeksFlg = YES */
         ELSE  /*  �� ��⠫�� ��⠬ */
            ASSIGN
               vUniBag   = GetXAttrValue("acct",bacct.acct + "," + bacct.curr,"UniformBag")
               /* ��諨 ��業� ��� ���� १�ࢠ */
               vPRsrv    = IF AVAIL zAcct THEN GetPersReserv(zacct.acct,zacct.curr,pDate)
                                          ELSE GetPersReserv(bacct.acct,bacct.curr,pDate).
         IF vPRsrv <> ? THEN
         DO:
            IF vUniBag =  "" THEN
               IF AVAIL zAcct THEN
                  RUN LnGetRiskGrOnDate (vPRsrv, pDate, OUTPUT vGRsrv).
               ELSE
                  vGRsrv = LnGetGrRiska(vPRsrv,pDate).
            ELSE
               IF AVAIL zAcct THEN
                  RUN LnGetRiskGrOnDate (vPRsrv, pDate, OUTPUT vGRsrv).
               ELSE
               DO:
                  IF bacct.cust-cat <> "�" THEN
                     vCustCat = bacct.cust-cat.
                  ELSE
                     vCustCat = GetXAttrValue("acct",bacct.acct + "," + bacct.currency,"�����").

                  vGRsrv = PsGetGrRiska (vPRsrv,vCustCat,pDate).
               END.
         END.
         ELSE
         DO: 
            vPRsrv = 0.
            TR:
            DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
            ON STOP UNDO TR,LEAVE TR:
               IF NOT AVAIL zAcct THEN
               DO:
                  IF SetPersReserv(bacct.acct,
                                   bacct.curr,
                                   pDate,
                                   0.00) <> YES THEN UNDO TR,LEAVE TR.
               END.
               ELSE
               DO:
                  IF SetPersReserv(zacct.acct,
                                   zacct.curr,
                                   pDate,
                                   0.00) <> YES THEN UNDO TR,LEAVE TR.
               END.
            END.  /* End of TR BLOCK */
         END.
         vRsrvRate = IF AVAIL zAcct THEN GetCalcBase(bAcct.acct, bAcct.currency, pDate, pDate) * vPRsrv / 100
                                    ELSE DesReserveAmount(bAcct.acct,bAcct.currency, pDate).
         IF vVeksFlg THEN
         DO:
             vP283Date5 = DATE(FGetSetting("���琥���","�283���5","")).            
             IF     vP283Date5 <> ?
                AND pDate      >= vP283Date5 THEN
             DO:
                 vP283Alg5  = FGetSetting("���琥���","�283���5","").
                 IF vP283Alg5 =  "��������琥�" THEN
                 DO:
                     vRsrvRate = getAcctReserve2(zAcct.acct, zAcct.currency, pDate, vPRsrv).                        
                 END.    
             END.
         END.                                
      END. /* ELSE �� ���� */
      END. /* IF NOT vIsOffshore */

      /* ���᫥��� ���⪠ �� ��� १�ࢠ */
      IF     {assigned vAcct} 
         AND AVAIL dAcct THEN
      DO:
         IF dAcct.open-date > pDate THEN
            vRSum = 0.
         ELSE
         DO:
            RUN acct-pos IN h_base (dAcct.acct,
                                    dAcct.currency,
                                    dAcct.open-date,
                                    pDate,
                                    CHR(251)).
            vRSum = ABS(sh-bal).
         END.
      END.

      IF (NOT {assigned vAcct} OR ({assigned vAcct} AND vRSum = 0 AND (AVAIL dAcct) AND NOT (dacct.open-date <= pDate AND pShowAll))) AND vSumm = 0 THEN NEXT ACCT.

      {getcust.i &name=vAcctNameChar &Offinn=YES &pref=b}
      vCustName = vAcctNameChar [1] + " " + vAcctNameChar [2].

      vAllRec = vAllRec + 1.

      CREATE  tProv.
      ASSIGN
         tProv.fNom           = vNom
         tProv.acct-cat       = bAcct.acct-cat
         tProv.risk-bal-acct  = bAcct.bal-acct
         tProv.acct-risk      = bAcct.acct
         tProv.currency       = bAcct.currency
         tProv.cust-name      = vCustName
         tProv.contract       = IF AVAIL bLA THEN bLA.contract ELSE ""
         tProv.cont-code      = IF AVAIL bLA THEN bLA.cont-code ELSE ""
         tProv.acct-risk-type = IF AVAIL bLA THEN bLA.acct-type ELSE ""
         tProv.acct-reserve   = IF vAcct <> ? THEN ENTRY (1, vAcct) ELSE ""
         tProv.rsrv-bal-acct  = INT64(ENTRY(1,bCode.val,"_"))
         tProv.acct-rsrv-type = IF NUM-ENTRIES(bCode.val,"_") = 2
                                THEN ENTRY(2,bCode.val,"_") ELSE ""
         tProv.since          = (IF AVAIL dLA THEN dLA.since ELSE ?)
         tProv.gr-rsrv        = IF vGRsrv <> ? THEN vGRsrv ELSE 0
         tProv.pers-rsrv      = vPRsrv
         tProv.acct-risk-bal  = vSumm
         tProv.acct-rsrv-bal  = IF vAcct <> ? THEN vRSum ELSE 0.
         tProv.rsrv-amt       = vRsrvRate
      .
      RUN RecalcUTReserve(tProv.acct-risk,
                          tProv.currency,
                          pDate,
                          INPUT-OUTPUT tProv.rsrv-amt).
            
      tProv.rsrv-amt = ReCalcAmtRsrv(bAcct.acct,
                                     bAcct.currency,
                                     tProv.rsrv-amt). 
                                                                                    
            /* ��⮪���஢����, �᫨ �� "rsrvStream" = �� */
      IF mPutProt THEN
      DO:         
         OUTPUT STREAM sOut TO "calcrsrv.log" APPEND.
         IF {assigned RETURN-VALUE} THEN DO:
            tProv.acct-rsrv-bal = 0.
            PUT STREAM sOut UNFORMATTED RETURN-VALUE SKIP.
         END.
         ELSE
         DO:               
            PUT STREAM sOut UNFORMATTED
               '%���: '           TRIM(STRING(tProv.pers-rsrv, "->>>>>>>>>>>>>>9.99999"))
               ' ��: '            tProv.gr-rsrv
               ' �㬬� १�ࢠ: ' TRIM(STRING(tProv.rsrv-amt,  "->>>>>>>>>>>>>>9.99"))
               ' %%: '            TRIM(STRING(vRlRsrv, "->>>>>9.99"))
            SKIP.
         END.   
         OUTPUT STREAM sOut CLOSE.
      END.
   END.
   RUN SetStatus(vTotal,vTotal).
   IF     vAftProc >  ""
      AND SearchPFile(vAftProc) THEN
      RUN VALUE(vAftProc + ".p") (pDate,
                                  pBefDate,
                                  INPUT-OUTPUT TABLE tProv).
   RETURN.
END.

PROCEDURE SetStatus:
  DEFINE INPUT  PARAMETER pPos AS INT64    NO-UNDO.
  DEFINE INPUT  PARAMETER pTot AS INT64    NO-UNDO.

  IF VALID-HANDLE(vHSuper) THEN
  DO:
    RUN SetStatus IN vHSuper (pPos,pTot) NO-ERROR.
    IF RETURN-VALUE = "STOP" THEN RETURN "STOP".
  END.
  IF ERROR-STATUS:ERROR THEN vHSuper = ?.
END.

PROCEDURE GetGrRisk:
  DEFINE INPUT  PARAMETER pPRsrv AS DECIMAL    NO-UNDO.
  DEFINE OUTPUT PARAMETER opGRsrv AS INT64 INIT ?   NO-UNDO.

  DEFINE VARIABLE vMax AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE vMin AS DECIMAL    NO-UNDO.

  DEFINE BUFFER dCode FOR CODE.

  IF pPRsrv = 0 THEN
  DO:
    opGRsrv = 0.
    RETURN .
  END.

  FOR EACH dCode WHERE
     dCode.class  = "GR-RESERVE" AND
     dCode.parent = "GR-RESERVE" NO-LOCK:
    ASSIGN
      vMin = DEC(ENTRY(1,dCode.val))
      vMax = DEC(ENTRY(2,dCode.val)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.

    IF pPRsrv >= vMin AND pPRsrv < vMax THEN
    DO:
      opGRsrv = INT64(dCode.code) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
        opGRsrv = ?.
        NEXT.
      END.
      RETURN .
    END.
  END.
END.

PROCEDURE GetPersRsrv:
  DEFINE INPUT  PARAMETER pBal AS INT64    NO-UNDO.
  DEFINE INPUT  PARAMETER pGRsrv AS INT64    NO-UNDO.
  DEFINE OUTPUT PARAMETER opRes AS DECIMAL INIT ?   NO-UNDO.

  DEFINE VARIABLE vMax AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE vMin AS DECIMAL    NO-UNDO.

  DEFINE BUFFER bCode FOR CODE.

  IF pGRsrv = 0 THEN
  DO:
    opRes = 0.
    RETURN .
  END.

  FIND FIRST bCode WHERE
     bCode.class  = "GR-RESERVE" AND
     bCode.parent = "GR-RESERVE" AND
     bCode.code = STRING(pGRsrv) NO-LOCK NO-ERROR.

  IF NOT AVAIL bCode THEN RETURN.

  ASSIGN
    vMin = DEC(ENTRY(1,bCode.val))
    vMax = DEC(ENTRY(2,bCode.val)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN GetBalPersSrv(pBal,pGRsrv,OUTPUT opRes).
  IF opRes <> ? AND opRes >= vMin AND opRes < vMax THEN RETURN.
  opRes = vMin.
END.

PROCEDURE GetBalPersSrv:
  DEFINE INPUT  PARAMETER pBal AS INT64    NO-UNDO.
  DEFINE INPUT  PARAMETER pGrRisk AS INT64    NO-UNDO.
  DEFINE OUTPUT PARAMETER opRsrv AS DECIMAL    NO-UNDO.

  DEFINE BUFFER bCode FOR CODE.

  FOR EACH bCode WHERE
     bCode.class  = "GR-RESERVE"    AND
     bCode.parent = STRING(pGrRisk) AND
     bCode.code   = STRING(pGrRisk) + "_" + STRING(pBal,"99999") NO-LOCK:

     opRsrv = DEC(bCode.val) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN NEXT.
     RETURN .
  END.
  opRsrv = ?.
END.

PROCEDURE SaveRsrvParameters:
  DEFINE INPUT PARAMETER pNom AS INT64 NO-UNDO.
  DEFINE INPUT PARAMETER pDate AS DATE       NO-UNDO.
  DEFINE INPUT PARAMETER TABLE FOR tProv.

  DEFINE VARIABLE vOk      AS LOGICAL.
  DEFINE VARIABLE vResult  AS CHARACTER NO-UNDO.

  DEFINE BUFFER bAcct FOR acct.
  DEFINE BUFFER bloan FOR loan.
  DEFINE BUFFER acct  FOR acct. /* ���������� ����. */
  DEFINE BUFFER loan-acct FOR loan-acct.
  FIND FIRST tProv WHERE tProv.fNom = pNom NO-ERROR.

  TR:
  DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
    ON STOP UNDO TR,LEAVE TR:

    {find-act.i
        &bact = bAcct
        &acct = tProv.acct-risk
        &curr = tProv.currency
    }
    IF NOT AVAIL bAcct THEN UNDO TR,LEAVE TR.

    IF {assigned tProv.acct-reserv}
    THEN DO:
       RUN LinkAcctReserve({&bAcctID},
                           tProv.acct-reserv + ",",
                           pDate,
                           OUTPUT vResult).
       IF vResult =  ""
          THEN UNDO TR,LEAVE TR.
    END.

    IF SetPersReserv(bacct.acct,
                     bacct.curr,
                     pDate,
                     IF tProv.gr-rsrv = 0 AND tProv.pers-rsrv = 0 
                        THEN ? 
                        ELSE tProv.pers-rsrv) <> YES 
    THEN UNDO TR,LEAVE TR.

    IF tProv.cont-code <> "" AND tProv.cont-code <> "0" AND  tProv.acct-rsrv-type <> "" THEN
    /* ���� �ନ����� � g-rsrv  */
    DO:
      FOR EACH loan-acct WHERE loan-acct.contract = tProv.contract AND loan-acct.cont-code = tProv.cont-code AND
        loan-acct.acct-type = tProv.acct-rsrv-type 
        AND loan-acct.since <= pDate NO-LOCK  BY loan-acct.since DESC:
        LEAVE.
      END.

      IF AVAIL loan-acct AND tProv.acct-reserve = ? THEN
      DO:
                  /* �������� �裡 acct-reserve */
         RUN DelLinksCode IN h_xclass ("acct",
                                       "acct-reserve",
                                       bacct.acct + "," + bacct.curr,
                                       loan-acct.acct + "," + loan-acct.currency,
                                       "",
                                       OUTPUT vOk).
          IF NOT vOk
             THEN UNDO TR,LEAVE TR.
                  /* �������� �ਢ離� � �������� */
        DELETE loan-acct.
      END.

      IF tProv.acct-reserve <> ? THEN
      DO:
        IF AVAIL loan-acct THEN
          FIND CURRENT loan-acct EXCLUSIVE NO-WAIT NO-ERROR.
        ELSE
        DO:
          CREATE loan-acct.
          ASSIGN
            loan-acct.contract = tProv.contract
            loan-acct.cont-code = tProv.cont-code
            loan-acct.acct-type = tProv.acct-rsrv-type
            loan-acct.currency = ""
            loan-acct.since = pDate.
        END.
        IF AVAIL loan-acct THEN loan-acct.acct = tProv.acct-reserve.
      END.
    END.

    /* ���⠭���� �� �����⨪� */
    FIND FIRST bLoan WHERE bLoan.contract  = loan-acct.contract
                       AND bLoan.cont-code = loan-acct.cont-code
       NO-LOCK NO-ERROR.
    IF AVAIL(bLoan) AND AVAIL(loan-acct)
    THEN DO:
       {find-act.i
          &acct = loan-acct.acct
          &curr = loan-acct.currency
       }
       RUN SetKau IN h_loanx (RECID(acct),
                              RECID(bLoan),
                              loan-acct.acct-type).
    END.

    vOk = YES.
  END.  /* End of TR BLOCK */
  IF vOk <> YES THEN RETURN ERROR.
END.

PROCEDURE SetCurrentRec:
  DEFINE INPUT  PARAMETER pNom AS INT64    NO-UNDO.
  DEFINE INPUT  PARAMETER TABLE FOR tProv.
  FOR EACH dProv:
    DELETE dProv.
  END.
  FIND FIRST tProv WHERE tProv.fNom = pNom NO-ERROR.
  CREATE dProv.
  BUFFER-COPY tProv TO dProv.
END.

PROCEDURE GetCurrentRec:
  DEFINE OUTPUT PARAMETER TABLE FOR tProv.
  FOR EACH tProv:
    DELETE tProv.
  END.
  FIND FIRST dProv WHERE NO-ERROR.
  IF AVAIL dProv THEN
  DO:
     CREATE tProv.
     BUFFER-COPY dProv TO tProv.
  END.
END.

FUNCTION GetTotal RETURNS INT64 ():
  RETURN vAllRec.
END.

FUNCTION GetOpDate RETURNS DATE ():
  RETURN vOpDate.
END.

PROCEDURE CrAcctRsrv:
  DEF INPUT  PARAM pNom            AS INT64  NO-UNDO.
  DEF INPUT  PARAM iClass          AS CHAR NO-UNDO.
  DEF INPUT  PARAM iDetails        AS CHAR NO-UNDO.
  DEF INPUT  PARAM iTakeBranchRisk AS LOG  NO-UNDO.
  DEF INPUT  PARAM iUserID         AS CHAR NO-UNDO.
  DEF INPUT  PARAM TABLE FOR tProv.
  DEF OUTPUT PARAM opAcct          AS CHAR INIT ?.

  DEF VAR vDetails AS CHAR  NO-UNDO INIT ?.
  DEF VAR vCustCat AS CHAR  NO-UNDO.
  DEF VAR vCustId  AS INT64   NO-UNDO.
  DEF VAR vKdDxRs  AS CHAR  NO-UNDO.         /* ��� ��室��-��室�� */
  DEF VAR vAccMask AS CHAR  NO-UNDO INIT ?.  /* ��᪠ ��� */
  DEF VAR vUserID  AS CHAR  NO-UNDO. 

  DEF BUFFER bAcct FOR acct.

  FIND FIRST tProv WHERE tProv.fNom = pNom NO-LOCK NO-ERROR.
  IF NOT AVAIL tProv THEN RETURN.

  {find-act.i
      &bact = bAcct
      &acct = tProv.acct-risk
      &curr = tProv.currency
  }
  IF NOT AVAIL bAcct THEN RETURN.

  /*
  IF NOT {assigned iClass} THEN /* ��� "acctbr1" - ᯥ�䨪� ���� */
     iClass = (IF SUBSTR(tProv.acct-risk,10,4) = "0000" THEN "acctbr" ELSE "acctbr1").
  */

  TR:
  DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
    ON STOP UNDO TR,LEAVE TR:

    /* ����砥� ������������ ����ਡ�����᪮�� ��� */
    ASSIGN
       vCustCat = bAcct.cust-cat
       vCustId  = bAcct.cust-id
       vDetails = iDetails.
    {acctcust.i}

    RUN SetSysConf IN h_base ("AcctRisk",tProv.acct-risk).

    RELEASE acct NO-ERROR.

      /* ����⠢�塞 � ���� ����� 䨫���� */
    IF iTakeBranchRisk THEN
    DO:
         /* ����稬 ���� ��� "��-㬮�砭��" ("��������������������") */
       RUN FindAcctMask IN h_acct (iClass,
                                   tProv.rsrv-bal-acct,
                                   INPUT-OUTPUT vAccMask,
                                   INPUT-OUTPUT vKdDxRs).
         /* ����⠢�塞 ����� "����" - ��� 䨫���� */
       IF INDEX(vAccMask, "�") > 0 THEN
       DO:
         RUN PatternSubst ("�",
                           bAcct.branch-id,
                           INPUT-OUTPUT vAccMask).
       END.
    END.
    ELSE
       vAccMask = ?.

    IF {assigned iUserID} THEN DO:
       IF iUserID =  "��␨᪠" THEN vUserID = bacct.user-id.
       ELSE vUserID = iUserID.
    END.
    ELSE DO:
       IF iTakeBranchRisk THEN vUserID = bAcct.user-id.
       ELSE vUserID = USERID('bisquit').
    END.
    RUN Cm_acct_cr IN h_acct (
          iClass,                /* iClass                */
          tProv.rsrv-bal-acct,   /* iBal                  */
          "",                    /* iCurr                 */
          vCustCat,              /* iCustCat              */
          vCustId,               /* iCustID               */
          vOpDate,               /* iOpenDate             */
          OUTPUT opAcct,         /* oAcct                 */
          BUFFER acct,           /* BUFFER iacct FOR acct */
          vAccMask,              /* iAcctMask             */
          vDetails,              /* iDetails              */
          ?,                     /* iKauId                */
          ?,                     /* iContract             */
          vUserID, /* iUserId */
          (IF iTakeBranchRisk THEN bAcct.branch-id ELSE ?), /* iBranchId */
          TRUE                   /* iCopyBalXattr         */
    ) NO-ERROR.
    {was-err.i &LBL=TR &TODO="LEAVE TR"}
    IF opAcct = ? THEN UNDO TR,LEAVE TR.
/* ��⠢�� ���� ���� */
    /* �ਢ離� � ��㯯� � �� */
    DEFINE VARIABLE cOFgroup  AS CHARACTER    NO-UNDO.
    cOFgroup = GetXattrValue("acct", bAcct.acct + "," + bAcct.currency, "groupOABS").
    IF {assigned cOFgroup}
    THEN DO:
        UpdateSigns ("acct", opAcct + ",", "groupOABS", cOFgroup, YES).
        RUN CreateLinks("acct", "acct-group", opAcct + ",", cOFgroup, vOpDate, ?, "").
    END.
/* ����� ��⠢�� ���� ���� */

    RUN DeleteOldDataProtocol IN h_base ("AcctRisk").
  END.  /* End of TR BLOCK */
END.

PROCEDURE Report:

  &SCOP SORT-BY1 tProv.rsrv-bal-acct
  &SCOP SORT-BY2 (IF LENGTH(tProv.acct-reserve) >= 20    ~
                  THEN                                   ~
                  (SUBSTRING(tProv.acct-reserve,1,8) +   ~
                   SUBSTRING(tProv.acct-reserve,10,11))  ~
                  ELSE "")


  &SCOP SORT-BY3 tProv.risk-bal-acct
  &SCOP SORT-BY4 (IF LENGTH(tProv.acct-risk) >= 20    ~
                  THEN                                   ~
                  (SUBSTRING(tProv.acct-risk,1,8) +   ~
                   SUBSTRING(tProv.acct-risk,10,11))  ~
                  ELSE "")


  DEFINE INPUT  PARAMETER pDate AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pFName AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER TABLE FOR tProv.

  DEFINE VARIABLE vUser AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vL AS LOGICAL NO-UNDO.

  DEFINE VARIABLE CreateAmt        AS DECIMAL NO-UNDO.
  DEFINE VARIABLE RestoreAmt       AS DECIMAL NO-UNDO.

  DEFINE VARIABLE AccRsrv-Amt AS DECIMAL NO-UNDO.
  DEFINE BUFFER buser FOR _user. 

  MESSAGE "�뢮���� ����� ����?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE vL.

  OS-DELETE VALUE(pFName).

  CASE vL:
    WHEN YES OR WHEN NO THEN.
    OTHERWISE RETURN.
  END CASE.


  OUTPUT STREAM sOut TO VALUE(pFName).
  PUT STREAM sOut UNFORMATTED "������ �� ��������� ������. ��������� �� " DAY(pDate) " "
    CAPS(ENTRY(MONTH(pDate),"ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������"))
    " " YEAR(pDate) SKIP(1).
  PUT STREAM sOut UNFORMATTED "����������� ������������� ������� - " GetEnoughRsrv(pDate) "%" SKIP.
  PUT STREAM sOut UNFORMATTED "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
  PUT STREAM sOut UNFORMATTED "�    ���� �����      �          ������         �       �������      � ��������� ����  �   ���� �������     �������� ��.����������  %%  ���������� ������ �     �������     �   ������������  �" SKIP.
  PUT STREAM sOut UNFORMATTED "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.

  FOR EACH tProv
     WHERE (tProv.rsrv-amt > (IF vL = YES THEN -999999999 ELSE 0))
        OR (tProv.acct-rsrv-bal > (IF vL = YES THEN -999999999 ELSE 0))
  BREAK BY {&SORT-BY1}
        BY {&SORT-BY2}
        BY {&SORT-BY4}
  :

     IF tProv.acct-reserve =  ""
     THEN AccRsrv-Amt = tProv.rsrv-amt.
     ELSE RUN Calc_Rep(tProv.rsrv-bal-acct,
                       tProv.acct-reserve,
                       OUTPUT AccRsrv-Amt
                       ).
     CreateAmt  = IF tProv.acct-rsrv-bal <  AccRsrv-Amt
                  THEN (AccRsrv-Amt - tProv.acct-rsrv-bal)
                  ELSE 0.
     RestoreAmt = IF tProv.acct-rsrv-bal >  AccRsrv-Amt
                     AND {assigned tProv.acct-reserve}
                  THEN (tProv.acct-rsrv-bal - AccRsrv-Amt)
                  ELSE 0.

      IF FIRST-OF({&SORT-BY2})
      OR tProv.acct-reserve =  "" THEN
      DO:
         ACCUMULATE AccRsrv-Amt         (TOTAL BY {&SORT-BY1}).
         ACCUMULATE AccRsrv-Amt         (TOTAL).
      END.

      IF  FIRST-OF({&SORT-BY2})
      AND tProv.acct-reserve <> "" THEN
      DO:
         ACCUMULATE tProv.acct-rsrv-bal (TOTAL BY {&SORT-BY1}).
         ACCUMULATE tProv.acct-rsrv-bal (TOTAL).
      END.

      IF   CreateAmt <> 0
      AND (FIRST-OF({&SORT-BY2})
       OR  tProv.acct-reserve =  "") THEN
      DO:
         ACCUMULATE CreateAmt           (TOTAL BY {&SORT-BY1}).
         ACCUMULATE CreateAmt           (TOTAL).
      END.

      IF RestoreAmt <> 0 AND FIRST-OF({&SORT-BY2}) THEN
      DO:
         ACCUMULATE RestoreAmt          (TOTAL BY {&SORT-BY1}).
         ACCUMULATE RestoreAmt          (TOTAL).
      END.

     PUT STREAM sOut UNFORMATTED
        "�" STRING(tProv.acct-risk,"x(20)")
        "�" STRING(tProv.cust-name,"x(25)")
        "�" STRING(tProv.cont-code,"x(20)")
        "�" STRING(tProv.acct-risk-bal,">>,>>>,>>>,>>9.99")
        "�" IF FIRST-OF({&SORT-BY2}) OR tProv.acct-reserve =  ""
            THEN STRING(tProv.acct-reserve,"x(20)")
            ELSE "        - / -       "
        "�" IF     FIRST-OF({&SORT-BY2})
               AND tProv.acct-reserve <> ""
            THEN STRING(tProv.acct-rsrv-bal,">>,>>>,>>>,>>9.99")
            ELSE STRING("","xxxxxxxxxxxxxxxxx")
        "�" STRING(tProv.gr-rsrv,">9")
        "�" STRING(tProv.pers-rsrv,">>9.99")
        "�" IF    FIRST-OF({&SORT-BY2})
               OR tProv.acct-reserve =  ""
            THEN STRING(AccRsrv-Amt,">>,>>>,>>>,>>9.99")
            ELSE STRING("","xxxxxxxxxxxxxxxxx")
        "�" IF     CreateAmt <> 0
               AND (    FIRST-OF({&SORT-BY2})
                    OR  tProv.acct-reserve =  "")
            THEN STRING(CreateAmt,">>,>>>,>>>,>>9.99")
            ELSE STRING("","xxxxxxxxxxxxxxxxx")
        "�" IF RestoreAmt <> 0 AND FIRST-OF({&SORT-BY2}) THEN
            STRING(RestoreAmt,">>,>>>,>>>,>>9.99")
            ELSE
            STRING("","xxxxxxxxxxxxxxxxx")
        "�"
     SKIP.
     IF LAST-OF({&SORT-BY1}) THEN DO:
        PUT STREAM sOut UNFORMATTED

       "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������͵"
        SKIP
        "�" "����� �� ����� ������� "  STRING(tProv.rsrv-bal-acct,"99999")
        " " "                 "
        " " STRING("","x(13)")
        " " STRING("","xxxxxxxxxxxxxxxxx")
        " " STRING("","x(20)")
        " " STRING(ACCUM TOTAL BY {&SORT-BY1} tProv.acct-rsrv-bal,">>,>>>,>>>,>>9.99")
        " " "  "
        " " STRING("","xxxxxx")
        " " STRING(ACCUM TOTAL BY {&SORT-BY1} AccRsrv-Amt,">>,>>>,>>>,>>9.99")
        " " STRING(ACCUM TOTAL BY {&SORT-BY1} CreateAmt,">>,>>>,>>>,>>9.99")
        " " STRING(ACCUM TOTAL BY {&SORT-BY1} RestoreAmt,">>,>>>,>>>,>>9.99")
        "�"
        SKIP.

        IF NOT LAST({&SORT-BY4}) THEN
        PUT STREAM sOut UNFORMATTED
        "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
        SKIP.
     END.
     IF LAST({&SORT-BY4}) THEN DO:
        PUT STREAM sOut UNFORMATTED
        "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������͵"
        SKIP
        "�" "�����                     "
        " " "                   "
        " " STRING("","x(13)")
        " " STRING("","xxxxxxxxxxxxxxxxx")
        " " STRING("","x(20)")
        " " STRING(ACCUM TOTAL tProv.acct-rsrv-bal,">>,>>>,>>>,>>9.99")
        " " "  "
        " " STRING("","xxxxxx")
        " " STRING(ACCUM TOTAL AccRsrv-Amt,">>,>>>,>>>,>>9.99")
        " " STRING(ACCUM TOTAL CreateAmt,">>,>>>,>>>,>>9.99")
        " " STRING(ACCUM TOTAL RestoreAmt,">>,>>>,>>>,>>9.99")
        "�"
        SKIP
        "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������;"
        SKIP.
     END.
  END.
  FIND FIRST buser WHERE buser._userid = USERID("bisquit") NO-LOCK NO-ERROR.
  vUser = (IF buser._user-name = ? OR buser._user-name = "" THEN 
  buser._userid ELSE buser._user-name).
  PUT STREAM sOut UNFORMATTED "�ᯮ����: " vUser SPACE(8) STRING(TODAY,"99/99/9999") SPACE STRING(TIME,"HH:MM:SS") SKIP.
  OUTPUT STREAM sOut CLOSE.
END.

PROCEDURE Calc_Rep:
   DEFINE INPUT  PARAMETER iBalAcct AS INT64   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oSumm    AS DECIMAL   NO-UNDO.

   DEFINE BUFFER bf_tProv FOR tProv.

   FOR EACH bf_tProv WHERE bf_tProv.acct-reserve  =  iAcct
                       AND bf_tProv.rsrv-bal-acct =  iBalAcct:
      oSumm = oSumm + bf_tProv.rsrv-amt.
   END.
END PROCEDURE.

/*
AcTurnover - ����� �㬬�୮�� ����� � ��砫� �����
*/
PROCEDURE AcTurnover:

   DEFINE INPUT  PARAMETER iAcct     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iDbCr     AS CHARACTER NO-UNDO.

   DEFINE OUTPUT PARAMETER oTurnover AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oReval    AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oCounter  AS INT64   NO-UNDO.
   DEFINE OUTPUT PARAMETER oBal      AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oMaxBal   AS DECIMAL   NO-UNDO.

   DEFINE VARIABLE vDateBeg AS DATE NO-UNDO.
   DEFINE VARIABLE vCDate   AS DATE NO-UNDO.
   DEFINE VARIABLE xDate    AS DATE NO-UNDO.

   DEFINE VARIABLE vPrevRate    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCurrRate    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vPrevDateBal AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vReval       AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vRevalDb     AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vRevalCr     AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vSum         AS DECIMAL NO-UNDO.

   DEFINE BUFFER acct    FOR acct.
   DEFINE BUFFER op-date FOR op-date.
   DEFINE BUFFER predate FOR ttOpDate.


   {find-act.i
       &acct = iAcct
       &curr = iCurr
   }

   IF NOT AVAILABLE acct THEN
      RETURN.

   IF NOT mFillTTOpDate THEN DO:
      /* WHOLE-INDEX op-date */
      FOR EACH op-date NO-LOCK:
         CREATE ttOpDate.
         BUFFER-COPY op-date TO ttOpDate.
      END.
      mFillTTOpDate = YES.
   END.


   IF acCalcBegMonth THEN
      vDateBeg = FirstMonDate(iDate).
   ELSE
      vDateBeg = 1 + iDate - acDayPeriod NO-ERROR.

   IF acBegLoan THEN
      vDateBeg = MAXIMUM(vDateBeg, acct.open-date).

   ASSIGN
      oCounter  = 0
      oMaxBal   = 0
      oTurnover = 0
      vRevalDb  = 0
      vRevalCr  = 0
   .

   DO vCDate = iDate TO vDateBeg BY -1:
      FIND FIRST ttOpDate WHERE ttOpDate.op-date =  vCDate NO-LOCK NO-ERROR.

      IF AVAILABLE ttOpDate THEN
      DO:

         IF iCurr =  "" THEN
            vCurrRate = 1.
         ELSE
            vCurrRate = FindRate("����", iCurr, vCDate).
         oCounter = oCounter + 1.
         RUN acct-pos-by-cat IN h_base (acct.acct-cat, iAcct, iCurr, vCDate, vCDate, CHR(251)).

         /* �� �ᥬ ���, � ������ ���⮪ �� ��� �� ������ */
         DO xDate = vCDate TO MAX (LastMove,vDateBeg) BY -1:
            FIND LAST predate WHERE predate.op-date <  vCDate NO-LOCK NO-ERROR.

            IF AVAILABLE predate THEN
            DO:

               IF iCurr =  "" THEN
                  vPrevRate = 1.
               ELSE
                  vPrevRate = FindRate("����", iCurr, predate.op-date).

               IF    (acct.side =  "�" AND sh-in-bal <  0)
                  OR (acct.side =  "�" AND sh-in-bal >  0)
               THEN
                  ASSIGN
                     sh-in-bal = 0
                     sh-in-val = 0
                  .
               ASSIGN
                  sh-in-bal = ABSOLUTE(sh-in-bal)
                  sh-in-val = ABSOLUTE(sh-in-val)
               .

               IF iCurr =  "" THEN
                  vPrevDateBal = sh-in-bal.
               ELSE
                  vPrevDateBal = sh-in-val.
               vReval = ROUND(vPrevDateBal * vCurrRate, 2) - ROUND(vPrevDateBal * vPrevRate, 2).

               IF acct.side =  "�" THEN
               DO:

                  IF vReval >  0 THEN
                     vRevalDb = vRevalDb + vReval.
                  ELSE
                     vRevalCr = vRevalCr + vReval.
               END.
               ELSE
               DO:

                  IF vReval <  0 THEN
                     vRevalDb = vRevalDb + vReval.
                  ELSE
                     vRevalCr = vRevalCr + vReval.
               END.
            END.

            IF    (acct.side =  "�" AND sh-bal <  0)
               OR (acct.side =  "�" AND sh-bal >  0) THEN
               ASSIGN
                  sh-bal = 0
                  sh-val = 0
               .
            ASSIGN
               sh-bal = ABSOLUTE(sh-bal)
               sh-val = ABSOLUTE(sh-val)
            .

            IF iCurr =  "" THEN
               vSum = sh-bal.
            ELSE
               vSum = ROUND(sh-val * vCurrRate, 2).

            IF vCDate =  iDate THEN
               oBal = vSum.

            oMaxBal = MAXIMUM(oMaxBal, vSum).

            IF xDate = vCDate  THEN DO:
               IF iDbCr =  "db" THEN
                  oTurnover = oTurnover + (IF iCurr =  ""
                                           THEN sh-db
                                           ELSE sh-vdb).
               ELSE
                  oTurnover = oTurnover + (IF iCurr =  ""
                                           THEN sh-cr
                                           ELSE sh-vcr).
            END.
         END.
         /* �ய�᪠�� �� ��⥭�� ��� */
         vCDate = MAX (LastMove,vDateBeg).
      END. /* AVAIL op-date */

   END.

   oReval = IF iDbCr =  "db" THEN vRevalDb ELSE vRevalCr.

   IF asOffCalend THEN
      oCounter = iDate - vDateBeg + 1.

END PROCEDURE.

/*
AcAvgTurnover - ����� �।���� ����� � ��砫� �����
*/
PROCEDURE AcAvgTurnover:

   DEFINE INPUT  PARAMETER iAcct     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iDbCr     AS CHARACTER NO-UNDO.

   DEFINE OUTPUT PARAMETER oAvgTurnover AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oBal         AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oMaxBal      AS DECIMAL   NO-UNDO.

   DEFINE VARIABLE vTurnover AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vReval    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCounter  AS INT64 NO-UNDO.

   RUN AcTurnover(iAcct, iCurr, iDate, iDbCr, OUTPUT vTurnover,
                  OUTPUT vReval, OUTPUT vCounter, OUTPUT oBal, OUTPUT oMaxBal).

   IF vCounter =  0 THEN
      oAvgTurnover = 0.
   ELSE
      oAvgTurnover = ROUND((vTurnover + vReval) / vCounter, 2).
END PROCEDURE.

/*
AcORsrvCalcBase - ����� ���� ��� ��।������ �������쭮� ����稭� �ନ�㥬��� १�ࢠ
*/

PROCEDURE AcORsrvCalcBase:

   DEFINE INPUT  PARAMETER iAcct     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS DATE      NO-UNDO.

   DEFINE OUTPUT PARAMETER oPP       AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oBal      AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oMaxBal   AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oOpCode   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oOpName   AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oEvalMode AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vDbCr        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vArgTurnover AS DECIMAL   NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   {find-act.i
      &acct = iAcct
      &curr = iCurr
   }

   IF NOT AVAILABLE acct THEN
      RETURN.


   IF acct.side =  "�" THEN
      vDbCr = "db".
   ELSE
      vDbCr = "cr".

   RUN AcAvgTurnover(iAcct, iCurr, iDate, vDbCr, OUTPUT vArgTurnover, OUTPUT oBal, OUTPUT oMaxBal).
   FOR EACH  code WHERE
             code.class  = "������"
         AND code.parent = "������"
         AND CAN-DO(code.val, iAcct) NO-LOCK BY code.code:
      LEAVE.
   END.

   IF AVAILABLE code THEN
      ASSIGN
         oOpCode   = TRIM(code.code)
         oOpName   = TRIM(code.name)
         oEvalMode = TRIM(code.description[1])
      .
   ELSE
      ASSIGN
         oOpCode   = "2.2"
         oOpName   = "����������������� ����� ������"
         oEvalMode = "��"
      .

   oPP = MAXIMUM(oBal, vArgTurnover).

END PROCEDURE.


/*---------------------------------------------------------------------------
  Function   : AcctRegNeed
  Name       : �㭪�� 䨪������ ��������� 䠪�஢, ������� �� ࠧ���
               १�ࢠ.

  Parameters : iAcct       - ����� ���
               iCurr       - ��� ������
               iPrevDate   - ��� �।��饣� �ॣ㫨஢���� १�ࢠ
               iCurrDate   - ��� ⥪�饣� �ॣ㫨஢���� १�ࢠ
  Notes:
  ---------------------------------------------------------------------------*/
FUNCTION AcctRegNeed RETURNS LOGICAL (INPUT iAcct     AS CHAR,
                                      INPUT iCurr     AS CHAR,
                                      INPUT iPrevDate AS DATE,
                                      INPUT iCurrDate AS DATE).

   DEFINE VARIABLE vPrevBal  AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCurrBal  AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vPrevRate AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCurrRate AS DECIMAL NO-UNDO.

   RUN acct-pos IN h_base (iAcct,
                           iCurr,
                           iPrevDate,
                           iPrevDate,
                           CHR(251)).

   vPrevBal = IF iCurr <> ""
              THEN ABS(sh-val)
              ELSE ABS(sh-bal).

   RUN acct-pos IN h_base (iAcct,
                           iCurr,
                           iCurrDate,
                           iCurrDate,
                           CHR(251)).

   vCurrBal = IF iCurr <> ""
              THEN ABS(sh-val)
              ELSE ABS(sh-bal).

   vPrevRate = GetPersReserv(iAcct,
                             iCurr,
                             iPrevDate).

   vCurrRate = GetPersReserv(iAcct,
                             iCurr,
                             iCurrDate).

   RETURN (   (vPrevBal <> vCurrBal)
           OR (vPrevRate <> vCurrRate)
          ).
END FUNCTION.

/*---------------------------------------------------------------------------
  Function   : IsAcctInBag
  Name       : �㭪�� ��।���� ����䥫�, � ����� ���. ���.

  Parameters : iAcct       - ����� ���
               iCurr       - ��� ������
  Notes:
  ---------------------------------------------------------------------------*/
FUNCTION IsAcctInBag RETURNS LOGICAL (INPUT iAcct    AS CHAR,
                                      INPUT iCurr    AS CHAR,
                                      INPUT iDate    AS DATE).

   RETURN (GetXattrValueEx("acct",iAcct + "," + iCurr,"UniformBag","") <> "").

END FUNCTION.


   /* ������ � ��ப� 蠡���� vMask 蠡������ ᨬ����� vChar �� ����� vNumb.
   ** ������ �ந�������� �ࠢ�-������. ������ ����� �ᥪ����� �� �ࠢ�� ���,
   ** ���⪨� - ���������� ��ﬨ ᫥��. ������� ����� ᫥������ �ࠧ���. */
PROCEDURE PatternSubst.
   DEF INPUT        PARAM vChar AS CHAR NO-UNDO. /* ������ 蠡���� ��� ������ */
   DEF INPUT        PARAM vNumb AS CHAR NO-UNDO. /* ����� ��� ����⠭���� � 蠡��� */
   DEF INPUT-OUTPUT PARAM vMask AS CHAR NO-UNDO. /* ��ப� 蠡���� */

   DEF VAR vPos0 AS INT64 NO-UNDO. /* ������ � 蠡���� */
   DEF VAR vPos1 AS INT64 NO-UNDO. /* ������ � �����  */
      /* ��।������ ���⮢�� ����権 � ���������� �������騬� ��ﬨ ����� */
   ASSIGN
      vPos0 = LENGTH(vMask)
      vNumb = FILL('0', vPos0) + vNumb
      vPos1 = LENGTH(vNumb)
   .
      /* �����筠� ������ (��室 � ���� 蠡����) � 蠡���� ᨬ�����  */
   DO WHILE TRUE:
      vPos0 = R-INDEX(vMask, vChar , vPos0).
      IF vPos0 <= 0 THEN LEAVE.
      ASSIGN
         SUBSTR(vMask, vPos0) = SUBSTR(vNumb, vPos1 , 1)
         vPos1 = vPos1 - 1
         vPos0 = vPos0 - 1
      .
   END.
END PROCEDURE.

/*
   ������ �㬬� १�ࢠ �� �������� � ��� �ய��樮���쭮 
   ���� � �ࠢ� ��饩 ������� ᮡ�⢥����� �� �����⢮.
   ���� �㬬� 䮭�� ��।������ ���祭��� ⥬���஢������ �� �㬬����
   �� ��� �᪠, �᫨ �� ���������, १���� ������ ᮢ������ � 
   ��室��� �㬬�� १�ࢠ.
*/
PROCEDURE RecalcUTReserve.
    DEFINE INPUT        PARAMETER iAcct     LIKE acct.acct     NO-UNDO.
    DEFINE INPUT        PARAMETER iCurrency LIKE acct.currency NO-UNDO.
    DEFINE INPUT        PARAMETER iDate     AS   DATE          NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioRsrvAmt AS   DECIMAL       NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vTmpStr  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vAcctPos AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vUTAmt   AS DECIMAL   NO-UNDO.

    {find-act.i &acct = iAcct
                &curr = iCurrency}
    IF AVAILABLE acct THEN DO:
        vTmpStr = GetTempXAttrValueEx("acct",
                                      Surrogate(BUFFER acct:HANDLE),
                                      "�㬬����",
                                      iDate,
                                      "").
        IF vTmpStr <> "" THEN DO:
            vUTAmt = DECIMAL(vTmpStr) NO-ERROR.
            IF vUTAmt > 0 AND NOT ERROR-STATUS:ERROR THEN DO:
                RUN acct-pos IN h_base (acct.acct,
                                        acct.currency,
                                        iDate,
                                        iDate,
                                        gop-status).
                vAcctPos = ABSOLUTE(IF {assigned acct.currency} THEN sh-val
                                                                ELSE sh-bal).
                IF vAcctPos > vUTAmt THEN DO:
                    ioRsrvAmt = 0.
                    RETURN "�訡��! ���⮪ �� ��� �ॢ�蠥� ����� " +
                           "�㬬� 䮭��, 㪠������ � ���. ४����� "  +
                           QUOTER("�㬬����") + " (" + STRING(vUTAmt) + ")".
                END.
                ioRsrvAmt = ioRsrvAmt * vAcctPos / vUTAmt.
            END.
        END.
    END.
END PROCEDURE.

/*
   ��������� ���� ���� १�ࢨ஢���� ��� ��⮢ �᪠ 
   � 㪠����� ��⮬ ��易⥫��� �� ������� �������� �।��.
*/
PROCEDURE RecalcRsrvBase_Oblig.
    DEFINE INPUT        PARAMETER iAcct      LIKE acct.acct     NO-UNDO.
    DEFINE INPUT        PARAMETER iCurrency  LIKE acct.currency NO-UNDO.
    DEFINE INPUT        PARAMETER iDate      AS   DATE          NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioRsrvBase AS   DECIMAL       NO-UNDO.

    DEFINE BUFFER acct FOR acct.

    DEFINE VARIABLE vAcctOblig AS CHARACTER NO-UNDO.

    {find-act.i &acct = iAcct
                &curr = iCurrency}
    IF AVAILABLE acct THEN DO:
        vAcctOblig = GetXAttrValue("acct",
                                   Surrogate(BUFFER acct:HANDLE),
                                   "acct-oblig").
        IF {assigned vAcctOblig} THEN DO:
            RELEASE acct.
            {find-act.i &acct = vAcctOblig}
            IF AVAILABLE acct          AND
               acct.open-date <= iDate AND
               (acct.close-date = ? OR acct.close-date > iDate)
            THEN DO:
                RUN acct-pos IN h_base (acct.acct,
                                        acct.currency,
                                        iDate,
                                        iDate,
                                        gop-status).
                ioRsrvBase = MAXIMUM(0, ioRsrvBase - ABSOLUTE(sh-bal)).
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE LinkAcctReserve.
    DEFINE INPUT  PARAMETER iAcctRiskSurr AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iAcctRsrvSurr AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iDate         AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER oResult       AS CHARACTER NO-UNDO.

    DEFINE BUFFER acct  FOR acct.
    DEFINE BUFFER links FOR links.
    DEFINE BUFFER xlink FOR xlink.

    DEFINE VARIABLE vAcct     LIKE acct.acct     NO-UNDO.
    DEFINE VARIABLE vCurrency LIKE acct.currency NO-UNDO.

    ASSIGN
        vAcct     = ENTRY(1, iAcctRiskSurr)
        vCurrency = ENTRY(2, iAcctRiskSurr)
    .
    {find-act.i &acct = vAcct
                &curr = vCurrency}
    IF AVAILABLE acct THEN
        FIND FIRST xlink WHERE
            xlink.class-code     = acct.class-code AND
            xlink.link-code      = "acct-reserve"  AND
            xlink.link-direction = "s"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE xlink THEN
        RETURN.
    /* SHARE-LOCK links  */    
    FOR EACH links WHERE
        links.link-id   = xlink.link-id AND
        links.source-id = iAcctRiskSurr AND
        links.beg-date  < iDate         AND
        links.end-date  = ?
    SHARE-LOCK:
        IF links.target-id = iAcctRsrvSurr THEN
            oResult = Surrogate(BUFFER links:HANDLE).
        ELSE
            links.end-date = iDate - 1.
    END.
    IF NOT {assigned oResult} THEN
        RUN CreateLinksRetSurr IN h_xclass ("acct",
                                            "acct-reserve",
                                            iAcctRiskSurr,
                                            iAcctRsrvSurr,
                                            iDate,
                                            ?,
                                            "",
                                            OUTPUT oResult).
END PROCEDURE.
/* $LINTFILE='pp-rsrv.p' */
/* $LINTMODE='1,-1,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='pase' */
/* $LINTDATE='07/07/2017 16:29:05.448+03:00' */
/*prosignDtNkmeA5jph1mVblxA/OQA*/