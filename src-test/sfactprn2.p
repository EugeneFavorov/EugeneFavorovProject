/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: SFACTPRN2.P
      Comment: ��� �� �믮������ ࠡ��/��� �� ����-䠪���
   Parameters: none
         Uses:
      Used by:
      Created: 06.05.2006 ZIAL
     Modified: 18/05/2006 ZIAL (0059334) ���. �������� ��� �� �믮������ ࠡ��/��� 
               �� ����-䠪���
     Modified: 24/07/2006 ZIAL (0059334) ���. �������� ��� �� �믮������ ࠡ��/��� 
               �� ����-䠪���
     Modified: 20.03.2007 17:38 OZMI     (0070614)
     Modified: 04/02/2015 kraw (0247185) ���ꥬ � ᯥ梥���
*/

{tmprecid.def}

{globals.i}
{intrface.get date}
{intrface.get axd}
{intrface.get asset}
{intrface.get xclass}
{intrface.get strng }
{intrface.get tmess}
{getcli.pro}
{wordwrap.def}
/*��*/
DEFINE VAR mSFNum        AS CHAR NO-UNDO. /*����� ����-䠪����*/
DEFINE VAR mSFDate       AS DATE NO-UNDO. /*��� ��*/
DEFINE VAR mSFSeller     AS CHAR NO-UNDO. /*�த����*/
DEFINE VAR mSFSellerFIO  AS CHAR NO-UNDO. /*�।�⠢�⥫� �த���*/
DEFINE VAR mSFSellerAddr AS CHAR NO-UNDO. /*�த���� ���� ��*/
DEFINE VAR mSFSellerINN  AS CHAR NO-UNDO. /*�த���� INN ��*/
DEFINE VAR mSFSellerKPP  AS CHAR NO-UNDO. /*�த���� KPP ��*/


DEFINE VAR mSFBuyer      AS CHAR NO-UNDO. /*���㯠⥫�*/
DEFINE VAR mSFBuyerFIO   AS CHAR NO-UNDO. /*�।�⠢�⥫� ���㯠⥫�*/
DEFINE VAR mSFBuyerAddr  AS CHAR NO-UNDO. /*���㯠⥫� ���� ��*/
DEFINE VAR mSFBuyerINN   AS CHAR NO-UNDO. /*���㯠⥫� INN ��*/
DEFINE VAR mSFBuyerKPP   AS CHAR NO-UNDO. /*���㯠⥫� KPP ��*/


DEFINE VAR mSFOtprav     AS CHAR NO-UNDO. /*��㧮��ࠢ ��*/
DEFINE VAR mSFOtpravAddr AS CHAR NO-UNDO. /*���� ��㧮��ࠢ ��*/
DEFINE VAR mSFPoluch     AS CHAR NO-UNDO. /*��㧮�����⥫� ��*/
DEFINE VAR mSFPoluchAddr AS CHAR NO-UNDO. /*���� ��㧮�����⥫� ��*/

DEFINE VAR mOpNum        AS CHAR NO-UNDO. /*����� ����񦭮�� ���㬥��*/
DEFINE VAR mOpDate       AS DATE NO-UNDO. /*��� ����񦭮�� ���㬥��*/





DEFINE VAR mAxdNum       AS CHAR NO-UNDO.
DEFINE VAR mAxdDate      AS DATE NO-UNDO.



DEFINE VAR mTotalSumm    AS DECIMAL NO-UNDO.
DEFINE VAR mNalogSumm    AS DECIMAL NO-UNDO.
/*DEFINE VAR mIsOut        AS LOG     NO-UNDO. /*���⠢������ ��� ��� ��*/*/
DEFINE VAR mNameSrv      AS CHAR NO-UNDO.    /* ������������ ��㣨 */


DEFINE VAR mSummOut      AS DECIMAL NO-UNDO.
DEFINE VAR mSummOutRub   AS CHAR    NO-UNDO.
DEFINE VAR mSummOutCop   AS CHAR    NO-UNDO.
DEFINE VAR mSummNalogSumm   AS DECIMAL    NO-UNDO.


DEFINE VAR mSFSellerTel  AS CHAR    NO-UNDO.




DEFINE VAR mBranchId     AS CHAR NO-UNDO.

DEFINE VAR mN            AS INT64 FORMAT ">>9" NO-UNDO.




DEF VAR mMonthes AS CHAR NO-UNDO 
        INIT "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������".

DEFINE STREAM sfact.

DEF VAR mI AS INT64 NO-UNDO. /* ���稪 */

DEF VAR mUserCode        AS CHAR NO-UNDO.    /*�ᯮ���⥫�*/
DEF VAR mShFBankRek      AS CHAR NO-UNDO.

DEF VAR mClFileName      AS CHAR NO-UNDO.
DEF VAR mContrAgent      AS CHAR NO-UNDO.
DEF VAR mIspolnit        AS CHAR NO-UNDO.
DEF VAR mNameStr         AS CHAR NO-UNDO EXTENT 10.


{setdest.i &cols=120 &STREAM="stream sfact" }

/*⠡��� ��� ���*/
DEFINE TEMP-TABLE ttServ NO-UNDO
   FIELD NameServ    AS CHAR     /*������������ ��㣨*/
   FIELD Edin        AS CHAR     /*������ ����७��*/
   FIELD Quant       AS DECIMAL  /*������⢮*/
   FIELD Price       AS DECIMAL  /*業� �� �������*/
   FIELD SummOut     AS DECIMAL  /*�㬬� ��� ������*/
/*   FIELD Akciz       AS DECIMAL  /*�㬬� ��樧�*/*/
   FIELD Nlog        AS DECIMAL  /*�⠢�� ������*/
   FIELD NalogSumm   AS DECIMAL  /*�㬬� ������*/
   FIELD TotalSumm   AS DECIMAL  /*�㬬� � �������*/
/*   FIELD Contry      AS CHAR     /*��࠭� �ந�宦�����*/*/
/*   FIELD GTDNum      AS CHAR     /*����� ���*/*/
.


FOR EACH TMPRECID NO-LOCK:

   FIND FIRST loan WHERE 
         RECID(loan) EQ tmprecid.id /*��諨 ��*/
      NO-LOCK NO-ERROR.



   /*��।��塞 ��६���� ��� ��*/
   RUN SFAttribsNames IN THIS-PROCEDURE(INPUT loan.contract,
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

   RUN SFAttribs_Nums IN THIS-PROCEDURE(INPUT loan.contract,
                                        INPUT loan.cont-code,
                                       
                                        OUTPUT mSFNum,  
                                        OUTPUT mSFDate,
                                        OUTPUT mOpNum, 
                                        OUTPUT mOpDate
                                       ).

   RUN AxdAttribs_Nums IN THIS-PROCEDURE(
                                         INPUT loan.parent-contract,
                                         INPUT loan.parent-cont-code,
                                       
                                         OUTPUT mAxdNum,  
                                         OUTPUT mAxdDate
                                        ).
                                       
   IF mSFNum EQ ? THEN mSFNum = "".
   IF mOpNum EQ ? THEN mOpNum = "".
   IF mAxdNum EQ ? THEN mAxdNum = "".


   /*ᬮ�ਬ � �����뢠�� ��㣨*/
   FOR EACH term-obl WHERE 
            term-obl.contract  = loan.contract
        AND term-obl.cont-code = loan.cont-code
   NO-LOCK:
      
      CREATE ttServ.

      ASSIGN 
         mNameSrv = SFAssetName(term-obl.contract + "," +
                                term-obl.cont-code + "," + 
                                STRING(term-obl.idnt) + "," + 
                                STRING(term-obl.end-date) + "," +
                                STRING(term-obl.nn),
                                term-obl.symbol)
         ttServ.NameServ = SplitStr(mNameSrv,30,'~n')
         ttServ.Edin = SFAssetUnit(term-obl.contract         + "," 
                                 + term-obl.cont-code        + ","
                                 + STRING(term-obl.idnt)     + ","
                                 + STRING(term-obl.end-date) + ","
                                 + STRING(term-obl.nn), 
                                   term-obl.symbol)
         .
                          
      ASSIGN         
         ttServ.Quant  = term-obl.ratio      /* ���-�� */
         ttServ.Price = term-obl.price       /* 業� */
/*         ttServ.Akciz = term-obl.dsc-int-amt /* ��樧 */*/
         ttServ.Nlog  = term-obl.rate        /* �⠢�� ��� */
         ttServ.NalogSumm = term-obl.int-amt /* �㬬� ��� */
         ttServ.TotalSumm = term-obl.amt-rub /* ���� �㬬� */
         ttServ.SummOut = term-obl.amt-rub - term-obl.int-amt /*�㬬� ��� ������*/
/*         ttServ.GTDNum = GetXattrValueEx("term-obl",
                          term-obl.contract + "," + term-obl.cont-code
                          + "," + STRING(term-obl.idnt) + "," 
                          + STRING(term-obl.end-date) + "," 
                          + STRING(term-obl.nn),
                          "declare",
                          "")                             /* ����� ��� */*/
/*        ttServ.contry = SFAssetCountry(term-obl.contract         + "," 
                                       + term-obl.cont-code        + ","
                                       + STRING(term-obl.idnt)     + ","
                                       + STRING(term-obl.end-date) + ","
                                       + STRING(term-obl.nn), 
                                         term-obl.symbol) /* ��࠭� */*/
      . 
      RELEASE ttServ.
   END.

   IF loan.cust-cat EQ "�" THEN
      mClFileName = "cust-corp".
   ELSE IF loan.cust-cat EQ "�" THEN
      mClFileName = "banks".
   ELSE IF loan.cust-cat EQ "�" THEN
      mClFileName = "person".

   IF loan.contract EQ "sf-out" THEN
   DO:

      mSFSellerFIO = "".
      mSFBuyerFIO = "".

      mUserCode = FGetSetting("�।�‪�",?,"").
      IF mUserCode NE "" THEN 
      DO:
         FIND FIRST _User WHERE _User._Userid EQ mUserCode NO-LOCK NO-ERROR.
         IF AVAILABLE _User THEN ASSIGN mSFSellerFIO = _User._User-Name.
      END.

      /*ASSIGN mSFSeller = FGetSetting("����",?,"").*/

      IF loan.cust-cat EQ "�"  OR loan.cust-cat EQ "�" OR loan.cust-cat EQ "�" THEN
      DO:
         FIND FIRST cust-role WHERE 
            cust-role.file-name EQ mClFileName AND
            cust-role.class-code EQ "�।�⠢�⥫��⢮" AND
            cust-role.surrogate EQ STRING(loan.cust-id) NO-LOCK NO-ERROR.
         IF AVAILABLE cust-role THEN 
            ASSIGN mSFBuyerFIO = cust-role.cust-name.

         IF loan.cust-cat EQ "�" AND GetXattrValue("person", STRING(loan.cust-id),"�।��") NE "" 
         THEN
         DO:
            mSFBuyer =   mSFBuyer. 
/*            mSFBuyer =  FGetSetting("�甑⊫",?,"") + " " + mSFBuyer. */

         END.
      END.

      ASSIGN mSFBuyer = mSFBuyer.
      mShFBankRek = FGetSetting("�甁������",?,"").

      IF mShFBankRek EQ "��" THEN
      DO:

         ASSIGN
            mSFSellerAddr = FGetSetting("����_��",?,"")
            mSFSellerINN = FGetSetting("���",?,"")
            mSFSellerKPP  = FGetSetting("�������",?,"")
         .
      END.
      ELSE /*IF mShFBankRek EQ "���" THEN*/
      DO:

         /*mBranchId = getUserBranchId(USERID('bisquit')).*/
         FIND FIRST branch WHERE 
                    branch.Branch-Type EQ "10" NO-LOCK NO-ERROR.
         IF AVAIL branch THEN 
         DO:
            mBranchId = branch.branch-id.
            ASSIGN
               mSFSeller     = branch.name
               mSFSellerAddr = branch.address
               mSFSellerTel = GetXAttrValue("branch",mBranchId,"����䮭")
               mSFSellerINN = GetXAttrValue("branch",mBranchId,"���")
               mSFSellerKPP = GetXAttrValue("branch",mBranchId,"���")
            .
         END.
      END.

   END.
   ELSE IF loan.contract EQ "sf-in" THEN
   DO:

      mSFSellerFIO = "".
      mSFBuyerFIO = "".

      mUserCode = FGetSetting("�।�‪�",?,"").
      IF mUserCode NE "" THEN 
      DO:
         FIND FIRST _User WHERE _User._Userid EQ mUserCode NO-LOCK NO-ERROR.
         IF AVAILABLE _User THEN 
            ASSIGN mSFBuyerFIO = _User._User-Name.
      END.
      ASSIGN mSFBuyer = FGetSetting("����",?,"").

      IF loan.cust-cat EQ "�"  OR loan.cust-cat EQ "�" OR loan.cust-cat EQ "�" THEN
      DO:
         FIND FIRST cust-role WHERE 
            cust-role.file-name EQ mClFileName AND
            cust-role.class-code EQ "�।�⠢�⥫��⢮" AND
            cust-role.surrogate EQ STRING(loan.cust-id) NO-LOCK NO-ERROR.
         IF AVAILABLE cust-role THEN 
            ASSIGN mSFSellerFIO = cust-role.cust-name.
      END.
      ASSIGN mSFSeller = mSFSeller.

      mShFBankRek = FGetSetting("�甁������",?,"").
      IF mShFBankRek EQ "��" THEN
      DO:
         ASSIGN
            mSFBuyerAddr = FGetSetting("����_��",?,"")
            mSFBuyerINN = FGetSetting("���",?,"")
            mSFBuyerKPP  = FGetSetting("�������",?,"")
         .
      END.
      ELSE IF mShFBankRek EQ "���" THEN
      DO:
         mBranchId = getUserBranchId(USERID('bisquit')).
         FIND FIRST branch WHERE 
                    branch.Branch-Id EQ mBranchId NO-LOCK NO-ERROR.
         IF AVAIL branch THEN 
         DO:
         ASSIGN
            mSFBuyerAddr = branch.address
            mSFBuyerINN = GetXAttrValue("branch",mBranchId,"���")
            mSFBuyerKPP = GetXAttrValue("branch",mBranchId,"���")
         .
         END.
      END.

   END.

   {sfactprn2.i}     /*1 �����*/

   PUT STREAM sfact UNFORMATTED SKIP(4).

   IF mN GT 2 THEN 
      PAGE STREAM sfact.

   {sfactprn2.i}    /*2 �����*/

   PAGE STREAM sfact.

   /*���⠥�*/
   {empty ttServ}

END.

{preview.i &STREAM="STREAM sfact"}

{intrface.del}
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='31/08/2015 17:24:07.825+04:00' */
/* $LINTUSER='kuds' */
/* $LINTMODE='1' */
/* $LINTFILE='sfactprn2.p' */
/*prosignRYYpZhXPz9oN5NibH7Qt5w*/