/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: SF-PRINT_2N.P
      Comment: ����� ��⮢-䠪���
   Parameters: ��ப�
         Uses:
      Used by:
      Created: 27.01.2005 Dasu
     Modified: 19/06/2006 ZIAL (0060144) ���. ��ࠡ�⪨ �� ��⠬-䠪���. ���� d15.
     Modified: 22/06/2006 ZIAL (0060144) ���. ��ࠡ�⪨ �� ��⠬-䠪���. ���� d15.
     Modified: 03/07/2006 ZIAL (0060144) ���. ��ࠡ�⪨ �� ��⠬-䠪���. ���� d15.
     Modified: 06/07/2006 ZIAL (0060144) ���. ��ࠡ�⪨ �� ��⠬-䠪���. ���� d15.
     Modified: 13/09/2013 kraw (0170344) ����� ��� ���
     Modified: 23/08/2017 kraw (0315149) ⮫쪮 ��ࢮ� ���祭�� ���
*/

/* � ����⢥ ��ࠬ��� 㪠�뢠���� ᯨ᮪ ����� �� �����䨪��� ����甠��.
** ��楤�� ࠡ�⠥� ⮫쪮 �� ⥬ �/�, � ������ ���祭�� �� ����甠��
** �室�� � ��� ᯨ᮪. �᫨ � ᯨ᪥ ���� "?", � ��ࠡ��뢠���� �/� �
** ������������ �� */
DEFINE INPUT PARAMETER iSostLst AS CHARACTER NO-UNDO.

{tmprecid.def}

{globals.i}
{intrface.get date}
{intrface.get axd}
{intrface.get asset}
{intrface.get xclass}
{intrface.get strng }
{intrface.get cdrep}
{intrface.get db2l}
{intrface.get cust}
{intrface.get tmess}
&IF DEFINED(OPS) <> 0 &THEN
{intrface.get prnvd}
&ENDIF
{getcli.pro}
{branch.pro}    /* �����㬥��� ��� ࠡ��� � ���ࠧ������ﬨ */
{wordwrap.def}

/* �� */
DEFINE VAR mSFNum         AS CHAR NO-UNDO. /* ����� ����-䠪���� */
DEFINE VAR mSFDate        AS DATE NO-UNDO. /* ��� �� */
DEFINE VAR mSFFixInfo     AS CHAR NO-UNDO. /* ���ଠ�� �� ��ࠢ����� */
DEFINE VAR mSFSost        AS CHAR NO-UNDO. /* ���ﭨ� �/� (����甠��) */
DEFINE VAR mSFFixDate     AS DATE NO-UNDO. /* ��� ��ࠢ����� */
DEFINE VAR mSFFixNum      AS CHAR NO-UNDO. /* ����� ��ࠢ����� */
DEFINE VAR mSFTovar       AS LOG  NO-UNDO. /* �� ����� */
DEFINE VAR mSFSeller      AS CHAR NO-UNDO. /* �த���� */
DEFINE VAR mSFSellerAddr  AS CHAR NO-UNDO. /* �த���� ���� �� */
DEFINE VAR mSFSellerINN   AS CHAR NO-UNDO. /* �த���� INN �� */
DEFINE VAR mSFSellerKPP   AS CHAR NO-UNDO. /* �த���� KPP �� */
DEFINE VAR mSFCurrInfo    AS CHAR NO-UNDO. /* ��� � ������������ ������ �� */
DEFINE VAR mSFCurrLine    AS CHAR NO-UNDO. /* ���� ��� ४����⠬� �� */
DEFINE VAR mSFCurrName    AS CHAR NO-UNDO. /* ������������ �� */
DEFINE VAR mIgk           AS CHAR NO-UNDO. /* �� ��� */

DEFINE VAR mSFBuyer       AS CHAR NO-UNDO. /* ���㯠⥫� */
DEFINE VAR mSFBuyerAddr   AS CHAR NO-UNDO. /* ���㯠⥫� ���� �� */
DEFINE VAR mSFBuyerINN    AS CHAR NO-UNDO. /* ���㯠⥫� INN �� */
DEFINE VAR mSFBuyerKPP    AS CHAR NO-UNDO. /* ���㯠⥫� KPP �� */
                         
DEFINE VAR mSFOtprav      AS CHAR NO-UNDO. /* ��㧮��ࠢ �� */
DEFINE VAR mSFOtpravAddr  AS CHAR NO-UNDO. /* ���� ��㧮��ࠢ �� */
DEFINE VAR mSFPoluch      AS CHAR NO-UNDO. /* ��㧮�����⥫� �� */
DEFINE VAR mSFPoluchAddr  AS CHAR NO-UNDO. /* ���� ��㧮�����⥫� �� */
                         
DEFINE VAR mOpNum         AS CHAR NO-UNDO. /* ����� ����񦭮�� ���㬥�� */
DEFINE VAR mOpDate        AS DATE NO-UNDO. /* ��� ����񦭮�� ���㬥�� */
DEFINE VAR mDocNumDate    AS CHAR NO-UNDO. /* ᯨ᮪ ����஢ ������� ���㬥�⮢ � ��� �뤠� ��� �஢���� ⨯� "sf-of-pay" */
DEFINE VAR mDocNumLine    AS CHAR NO-UNDO. /* ���� ��� ᯨ᪮� ����஢ ������� ���㬥�⮢ */
DEFINE VAR mSurrOp        AS CHAR NO-UNDO. /* ���᮪ ���ண�⮢ ���⥦��, �易���� � ���⮬-䠪��ன */
DEFINE VAR mTotalSumm     AS DEC  NO-UNDO.
DEFINE VAR mNalogSumm     AS DEC  NO-UNDO.
DEFINE VAR mPriceSumm     AS DEC  NO-UNDO.
DEFINE VAR mNameSrv       AS CHAR NO-UNDO. /* ������������ ��㣨 */
DEFINE VAR mBranchId      AS CHAR NO-UNDO.
DEFINE VAR mStrSeller     AS CHAR NO-UNDO EXTENT 10. /* �த���� */
DEFINE VAR mStrSellerAddr AS CHAR NO-UNDO EXTENT 10. /* �த���� ���� �� */
DEFINE VAR mStrBuyer      AS CHAR NO-UNDO EXTENT 10. /* ���㯠⥫� */
DEFINE VAR mStrBuyerAddr  AS CHAR NO-UNDO EXTENT 10. /* ���㯠⥫� ���� �� */
DEFINE VAR mStrOtprav     AS CHAR NO-UNDO EXTENT 10. /* ��㧮��ࠢ �� + ���� */
DEFINE VAR mStrPoluch     AS CHAR NO-UNDO EXTENT 10. /* ��㧮�����⥫� �� + ���� */
DEFINE VAR mWide          AS INT64  NO-UNDO.
DEFINE VAR mNameGO        AS CHARACTER   NO-UNDO.
DEFINE VAR mAdresGO       AS CHARACTER   NO-UNDO.
DEFINE VAR mMonthes       AS CHARACTER   NO-UNDO INIT "ﭢ���,䥢ࠫ�,����,~
��५�,���,���,���,������,ᥭ����,������,�����,�������".

DEFINE VAR mDispReq AS CHAR NO-UNDO.

DEFINE STREAM sfact.

DEF VAR mI AS INT64 NO-UNDO. /* ����稪 */
DEF VAR mPage AS INT64 NO-UNDO.
DEF VAR mLeng AS INT64 NO-UNDO INIT 187.
DEF VAR mLengBody AS INT64 NO-UNDO INIT 3.
DEF VAR mLs AS INT64 NO-UNDO INIT 0.

DEF VAR mMaxRow AS INT64  NO-UNDO.

DEFINE VARIABLE mLoanSurr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTermOblSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVATformat   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSchFPolnNam AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mStrNlog     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFractional  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mX           AS DECIMAL   NO-UNDO.
/* �ਧ��� �⪫�祭�� ������� �� �� �⪫��␠�� */
DEFINE VARIABLE mNoAutoCalc  AS LOGICAL   NO-UNDO.

&GLOB  STR1 '� ����񦭮-����⭮�� ���㬥���'
&IF DEFINED(OPS) =  0 &THEN
{setdest.i &stream="stream sfact" &filename="'_spool_sf.tmp'"}
&ENDIF

/* ⠡��� ��� ��� */
DEFINE TEMP-TABLE ttServ NO-UNDO
   FIELD NameServ    AS CHAR     /* ������������ ��㣨 */
   FIELD KodTov      AS CHAR     /* ��� ⮢�� ��� */
   FIELD Edin        AS CHAR     /* ������ ����७�� */
   FIELD EdinName    AS CHAR     /* �᫮���� ������祭�� ������� ����७�� */
   FIELD Quant       AS DECIMAL  /* ������⢮ */
   FIELD Price       AS DECIMAL  /* 業� �� ������� */
   FIELD SummOut     AS DECIMAL  /* �㬬� ��� ������ */
   FIELD Akciz       AS DECIMAL  /* �㬬� ��樧� */
   FIELD Rate        AS DECIMAL  /* �⠢�� ������ �� term-obl */
   FIELD Nlog        AS DECIMAL  /* �⠢�� ������ */
   FIELD NalogSumm   AS DECIMAL  /* �㬬� ������ */
   FIELD TotalSumm   AS DECIMAL  /* �㬬� � ������� */
   FIELD Contry      AS CHAR     /* ��࠭� �ந�宦����� */
   FIELD ContryName  AS CHAR     /* ������������ ��࠭� �ந�宦����� */
   FIELD GTDNum      AS CHAR     /* ����� ��� */
   FIELD Curr        AS CHAR     /* ����� */
   FIELD Rate-fixed  AS CHAR     /* ����஢���� ��� */
.

DEFINE BUFFER b-loan FOR loan. /* ���������� ����� */

mPage = 0.

ASSIGN
   mVATformat = (IF FGetSetting("�甎�����", "", "���") =  "��"
                 THEN ">>>>>>>9%"
                 ELSE ">>>>>9.99")   
   mSchFPolnNam = FGetSetting("�甏�������", "", "���") =  "��"
   mDispReq     = FGetSetting("�甎����������", "", "---")
.
IF mDispReq =  ? THEN mDispReq = '---'.

&IF DEFINED(OPS) <> 0 &THEN
RUN BeginCircle_TTName IN h_prnvd ("sf").
&ENDIF

FOR EACH TMPRECID NO-LOCK:
   FIND FIRST loan WHERE 
         RECID(loan) =  tmprecid.id /* ��諨 �� */
      NO-LOCK NO-ERROR.

   ASSIGN
      mStrSeller = ""
      mLoanSurr  = GetSurrogateBuffer("loan", (BUFFER loan:HANDLE))
      mSFSost    = GetXAttrValueEx("loan", mLoanSurr, "����甠��", "?")
      mBranchId  = TRIM(GetXAttrValueEx("_user",loan.user-id,"�⤥�����", ""))
   .
   IF iSostLst <> "" AND
      NOT CAN-DO(iSostLst, mSFSost) THEN
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "0",
         SUBSTITUTE("C/� &1 �ய�饭.~n��� ⠪�� ��⮢-䠪��� ������� ~
��㣠� ��楤�� ����.",
                    loan.doc-num)).
      NEXT.
   END.

   /* ��।��塞 ��६���� ��� �� */
   RUN SFAttribsNames IN h_axd (INPUT loan.contract,
                      INPUT loan.cont-code,

                      OUTPUT mSFSeller,
                      OUTPUT mSFSellerAddr,
                      OUTPUT mSFSellerINN,
                      OUTPUT mSFSellerKPP,

                      OUTPUT mSFBuyer,    
                      OUTPUT mSFBuyerAddr,
                      OUTPUT mSFBuyerINN,
                      OUTPUT mSFBuyerKPP,
                      
                      OUTPUT mSFOtprav,    
                      OUTPUT mSFOtpravAddr,
                      OUTPUT mSFPoluch,
                      OUTPUT mSFPoluchAddr).

   RUN SFAttribs_Nums IN h_axd (INPUT loan.contract,
                       INPUT loan.cont-code,

                       OUTPUT mSFNum,  
                       OUTPUT mSFDate,
                       OUTPUT mOpNum, 
                       OUTPUT mOpDate
                      ).
   
                         
                      
   ASSIGN
      mDocNumDate = ""
      mSFNum = "" WHEN mSFNum =  ?
      mOpNum = "" WHEN mOpNum =  ?
      mSFSellerKPP = ENTRY(1, mSFSellerKPP)
      mSFBuyerKPP  = ENTRY(1, mSFBuyerKPP)
   .

   IF mSFSost =  "?" THEN
      mSFFixInfo = "".
   ELSE
      mSFFixInfo = GetXattrValue("loan", mLoanSurr, "�����⊮��").

   mSFTovar = GetXattrValue("loan", mLoanSurr, "�����") =  "��".

   IF {assigned loan.currency} THEN
   DO:
      mSFCurrName = GetBufferValue("currency",
                                   "WHERE currency.currency EQ " + 
                                   QUOTER(loan.currency),
                                   "name-currenc").
      ASSIGN
         mSFCurrInfo = " ������������, ��� " + mSFCurrName + ", " + loan.currency
         mSFCurrLine = FILL(" ", 26) + FILL("�",LENGTH(mSFCurrName)) + "  ���"
         .
   END.
   ELSE
      ASSIGN    
         mSFCurrInfo = " ������������, ��� ���ᨩ᪨� �㡫�, 643"
         mSFCurrLine = FILL(" ", 26) + "����������������  ���"
         .
   mIgk = GetXAttrValueEx("loan",
                          loan.contract + "," + loan.cont-code,
                          "���",
                          "-").
   IF loan.cont-type =  "�/�" THEN   
   DO:
      mSurrOp = GetLinks(loan.class-code, /* ID �����     */
                mLoanSurr,                /* ID(c��ண��) ��ꥪ�   */
                ?,                        /* ���ࠢ����� �裡: s | t | ?         */
                "sf-op-pay",              /* ���᮪ ����� ������ � CAN-DO �ଠ� */
                ";",                      /* �������⥫� १������饣� ᯨ᪠   */
                ?).
      DO mI=1 TO NUM-ENTRIES(mSurrOp,";"):
         FOR EACH op WHERE op.op = INT64(ENTRY(1, ENTRY(mI, mSurrOp, ";"), ","))
         NO-LOCK:
            mDocNumDate = mDocNumDate + (IF mI = 1
                                         THEN ''
                                         ELSE FILL(' ',mLengBody + mLs + 
LENGTH({&STR1}))) + ' ' + "�" + STRING(OP.doc-num,"x(10)") + "��" + " " + 
STRING(OP.op-date,"99/99/9999").  
               
               END.
         mDocNumLine = "                               ������������    ���������������������������������".       
      END.
   END.
   IF loan.loan-work AND {assigned mOpNum} THEN
      ASSIGN
         mDocNumDate = SUBSTITUTE("�&1 �� &2",
                                  STRING(mOpNum,"x(10)"),
                                  STRING(mOpDate,"99/99/9999"))
         mDocNumLine = 
                 "                               ������������    ���������������������������������"
      .
   IF mDocNumDate =  "" THEN
   DO:
      ASSIGN
         mDocNumDate = "-"
         mDocNumLine = "                               �������������������������������������������������"
         .
   END.   

   
   mNoAutoCalc = CAN-DO(FGetSetting("�⪫��␠��", ?, ""), loan.contract).
   /* ᬮ�ਬ � �����뢠�� ��㣨 */
   FOR EACH term-obl WHERE 
            term-obl.contract  = loan.contract
        AND term-obl.cont-code = loan.cont-code
   NO-LOCK:
      mTermOblSurr = GetSurrogateBuffer("term-obl", (BUFFER term-obl:HANDLE)).
      mFractional  = (IF GetXattrValueEx(term-obl.class-code, mTermOblSurr, "�����", "") =  "��"
                      THEN YES
                      ELSE NO).
      
      CREATE ttServ.

      FIND FIRST currency WHERE currency.currency =  loan.currency NO-LOCK NO-ERROR.
      
      ASSIGN 
         mNameSrv        = SFAssetName(mTermOblSurr, term-obl.symbol)
&IF DEFINED(OPS) =  0 &THEN
         ttServ.NameServ = SplitStr(mNameSrv, 20, '~n')
&ELSE
         ttServ.NameServ = mNameSrv
&ENDIF
         ttServ.EdinName = SFAssetUnit(mTermOblSurr, term-obl.symbol)
         ttServ.Edin     = GetCodeEx("Unit", ttServ.EdinName, "")
&IF DEFINED(OPS) =  0 &THEN
         ttServ.EdinName = SplitStr(ttServ.EdinName, 14, '~n')
&ENDIF
         .

      ASSIGN
         ttServ.KodTov = GetXattrValueEx("term-obl",
                                         mTermOblSurr,
                                         "�������ࠅ��",
                                         "")
         ttServ.rate-fixed = GetXattrValueEx("term-obl", mTermOblSurr, "rate-fixed", "")
         ttServ.Curr      = IF loan.currency =  "" THEN "���ᨩ᪨� �㡫�"
                                                   ELSE (IF AVAIL currency
                                                         THEN currency.name-currenc
                                                         ELSE "")
         ttServ.Quant      = term-obl.ratio       /* ���-�� */
         ttServ.Price      = IF ttServ.rate-fixed =  '%'
                             THEN DEC(GetXattrValueEx("term-obl", 
                                                 mTermOblSurr,
                                                 "Price-rate",
                                                 STRING(term-obl.price)))
                             ELSE term-obl.price  /* 業� */
         ttServ.Akciz      = term-obl.dsc-int-amt /* ��樧 */
         ttServ.Rate       = term-obl.rate        /* �⠢�� ��� �� term-obl */
         ttServ.Nlog       = (IF mFractional
                              THEN (term-obl.rate / (term-obl.rate + 100))
                              ELSE  term-obl.rate)
                                                  /* �⠢�� ��� */
         ttServ.NalogSumm  = term-obl.int-amt     /* �㬬� ��� */
         ttServ.TotalSumm  = term-obl.amt-rub     /* ���� �㬬� */
         ttServ.SummOut    = (IF mNoAutoCalc       /* �㬬� ��� ������ */
                                THEN term-obl.amount-of-payment
                                ELSE (term-obl.amt-rub - term-obl.int-amt))
         ttServ.GTDNum     = GetXattrValueEx("term-obl", mTermOblSurr, "declare", "")            /* ����� ��� */
         ttServ.contry     = SFAssetCountryAttr(mTermOblSurr, term-obl.symbol, "country-alt-id") /* ��࠭� */
         ttServ.ContryName = SFAssetCountryAttr(mTermOblSurr, term-obl.symbol, "country-name")   /* ��࠭� */  
&IF DEFINED(OPS) =  0 &THEN
         ttServ.ContryName = SplitStr(ttServ.ContryName, 12, '~n')
&ENDIF
         .
      RELEASE ttServ.
   END.

&IF DEFINED(OPS) =  0 &THEN
   /* ������ ��ॢ�� ��࠭��� ��। ������ ����-䠪����, �஬� ��ࢮ� */
   
   IF mPage = 0 THEN mPage = 1.
   ELSE PAGE STREAM sfact.
&ENDIF
    
   ASSIGN 
      mStrSeller     = mSFSeller
      mStrSellerAddr = mSFSellerAddr
      mStrBuyer      = mSFBuyer
      mStrBuyerAddr  = mSFBuyerAddr 
   .

   IF loan.cust-cat =  "�" THEN
   DO:
      FIND FIRST cust-corp WHERE cust-corp.cust-id =  loan.cust-id NO-LOCK NO-ERROR.
      IF AVAIL cust-corp THEN
           ASSIGN
/* ��⠢�� ���� ���� */
            mStrBuyer = GetTempXattrValueEx("cust-corp",
                                                STRING(cust-corp.cust-id),
                                                "name-short",
                                                loan.open-date,"")
/* ����� ��⠢�� ���� ���� */
            mNameGO  = GetXAttrValueEx("cust-corp",
                                       STRING(cust-corp.cust-id),
                                       "NameGO",
                                       "")
            mAdresGO = GetXAttrValueEx("cust-corp",
                                       STRING(cust-corp.cust-id),
                                       "AdresGO",
                                       "")
         .
   END.

   IF     {assigned mNameGO}
      AND {assigned mAdresGO} THEN
   DO:
      IF loan.contract =  "sf-in" THEN
         ASSIGN
            mStrSeller     = mNameGO
            mStrSellerAddr = mAdresGO
         .
      ELSE
         ASSIGN
            mStrBuyer     = mNameGO
            mStrBuyerAddr = mAdresGO
         .
   END.
   
   IF    loan.cont-type =  "�/�"
      OR loan.cont-type =  "�/�"
      OR NOT mSFTovar THEN
      ASSIGN
         mStrOtprav     = "-"
         mStrPoluch     = "-"
      .
   ELSE
   DO:
      IF INDEX(mStrOtprav[1],mSFOtprav) =  0 THEN
         mStrOtprav = mSFOtprav + " " + mSFOtpravAddr.
      IF INDEX(mStrPoluch[1],mSFPoluch) =  0 THEN
         mStrPoluch = mSFPoluch + " " + mSFPoluchAddr.     
   END.

&IF DEFINED(OPS) =  0 &THEN
   {sf-print_2n.i}
&ELSE
   {sf-print_2n_ops.i}
&ENDIF

   /* ���⠥� */
   {empty ttServ}

   &IF DEFINED(OPS) <> 0 &THEN
   RUN NextCircle_TTName IN h_prnvd ("sf").
   &ENDIF

END.

&IF DEFINED(OPS) <> 0 &THEN
RUN EndCircle_TTName IN h_prnvd ("sf").
&ENDIF

&IF DEFINED(OPS) =  0 &THEN
{preview.i &STREAM="STREAM sfact" &FILENAME = "'_spool_sf.tmp'"}
&ELSE
RUN prnvd IN h_prnvd ("sfprint2n").
&ENDIF

{intrface.del}
/* $LINTFILE='sf-print_2n.p' */
/* $LINTMODE='1,3,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq/' */
/* $LINTUSER='ozmi' */
/* $LINTDATE='19/09/2017 11:37:37.641+03:00' */
/*prosign7uS0o0ERIg6EQWjRK3hnfA*/