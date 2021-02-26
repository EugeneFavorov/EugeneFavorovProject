{globals.i}
{intrface.get pbase}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{intrface.get instrum}
{intrface.get tmess}
{intrface.get pqres}
{dpsproc.def}
{parsin.def}
{sh-defs.i}
{tmprecid.def}

DEFINE VARIABLE mInt         AS INT64     NO-UNDO.
DEFINE VARIABLE mBegDate     AS DATE      NO-UNDO.
DEFINE VARIABLE mEndDate     AS DATE      NO-UNDO.
DEFINE VARIABLE mFilial      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskDb      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMaskCr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVal         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClassCode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBalAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrency    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctMask    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBranchID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustCat     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustID      AS INT64     NO-UNDO.
DEFINE VARIABLE mSubject     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mResAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNumber      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mErrMsg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUpd         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOk          AS LOGICAL   NO-UNDO.

/*{setdest.i &filename = "'111.log'"}*/

ASSIGN
   mInt     = 0.
   
FOR EACH tmprecid NO-LOCK,
   FIRST cust-corp WHERE
   RECID(cust-corp) EQ tmprecid.id
   NO-LOCK:
   ASSIGN
      mInt     = mInt + 1
      mCustID  = cust-corp.cust-id
      mSubject = GetXattrValueEx("cust-corp",STRING(mCustID),"��ꥪ�","")
      .
END.

IF    mInt GT 1
   OR mInt EQ 0 THEN
DO:   
   MESSAGE "������ ���� ��࠭ ���� ������!" 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND op-kind WHERE 
   op-kind.op-kind EQ "resacct"
NO-LOCK NO-ERROR.
FIND FIRST op-template OF op-kind
NO-LOCK NO-ERROR.
IF    NOT AVAIL(op-kind)
   OR NOT AVAIL(op-template) THEN
DO:
   MESSAGE "�� ������� �࠭����� ������ ���." 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

CASE mSubject:
   WHEN "���"  THEN mBalAcct = "40701".
   WHEN "����" THEN mBalAcct = "40702".
   WHEN "���"  THEN mBalAcct = "40703".
   OTHERWISE mBalAcct = ?.
END CASE.

IF mBalAcct EQ ? THEN
DO:
   MESSAGE "��� ��ꥪ� �� ᮮ⢥���� �᫮���." 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

ASSIGN
   mClassCode = op-template.cr-class-code
/*   mBalAcct   = Get_Param("bal-acct",  RECID(op-template))*/
   mCurrency  = Get_Param("l-currency",RECID(op-template))
   mAcctMask  = Get_Param("open_mask", RECID(op-template))
   mBranchID  = shFilial
   mCustCat   = "�"
   mResAcct   = mBalAcct + "8100" + mBranchID + STRING(mCustID,"9999999").

RUN RecalcKey IN h_acct (
   mClassCode,
   mBalAcct,
   mResAcct,
   OUTPUT mNumber
) NO-ERROR.

IF ERROR-STATUS:ERROR THEN
DO:
   MESSAGE "�� 㤠���� ������ ���� ���." 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

mResAcct = mNumber.

FORM
   mNumber  LABEL  "������ ���" 
            FORMAT "x(20)"
            HELP   "������ ���"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE " ��� ������ ".

PAUSE 0.

ON LEAVE OF mNumber
DO:
   IF LASTKEY NE 27 THEN
   DO:   
      ASSIGN
         mNumber.
   	/* �஢�ઠ ������ ��� � ��. */
      IF GetValueByQuery ("acct","acct","acct.acct EQ '" + AddFilToAcct(mNumber,shFilial) + "' NO-LOCK") NE ? THEN
      DO:
         MESSAGE "����� ��� 㦥 ����." 
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   	/* �஢��塞 ����稥 ������ ��� � �����䨪���.
		** �᫨ ⠪�� ����, � �⬥�塞 ����⢨�. */
      IF GetCode("��⠐���ࢠ", AddFilToAcct(mNumber,shFilial)) NE ?
      THEN
      DO:
         MESSAGE "��� 㦥 ��१�ࢨ஢��." 
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
END.

ON VALUE-CHANGED OF mNumber
DO:
   RUN RecalcKey IN h_acct (
      mClassCode,
      mBalAcct,
      mNumber:SCREEN-VALUE,
      OUTPUT mNumber
   ) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
   DO:
      MESSAGE "�� 㤠���� ������ ���� ���." 
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN mNumber:SCREEN-VALUE = mNumber.
   RETURN NO-APPLY.
END.

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frAcct.
      RETURN ERROR.
   END.
   UPDATE
      mNumber
   WITH FRAME frAcct.
END.
HIDE FRAME frAcct.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN AcctKeep IN h_acct (AddFilToAcct(mNumber, shFilial),OUTPUT mOk).
   IF ERROR-STATUS:ERROR THEN
   DO:
      MESSAGE "�� 㤠���� ��࠭��� ��� � �����䨪���." 
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   RUN AcctKeep IN h_acct (mNumber,OUTPUT mOk).
   IF ERROR-STATUS:ERROR THEN
   DO:
      MESSAGE "�� 㤠���� ��࠭��� ��� � �����䨪���." 
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   
   mUpd = UpdateSigns("cust-corp",STRING(mCustID),"ResDate",STRING(TODAY,"99/99/9999")    ,?).
   mUpd = UpdateSigns("cust-corp",STRING(mCustID),"ResAcct",AddFilToAcct(mNumber,shFilial),?).
   
   MESSAGE "��� " + mNumber + " ��१�ࢨ஢��."
   VIEW-AS ALERT-BOX TITLE " ����饭�� ".
   
/* RUN AcctFree IN h_acct (AddFilToAcct(vOldNumber, shFilial), OUTPUT vOk).*/   
      
END.

/*{preview.i &filename = "'111.log'"}*/

{intrface.del}

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.

