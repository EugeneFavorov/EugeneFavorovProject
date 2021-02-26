/*
	  ������᪠� ��⥣�஢����� ��⥬� �������
		Filename: handdoc.p
		Comment: ���� "����஫� "�����" �஢���� �� ��⠬ ��室��/���室��"
		Parameters:
		Uses:handdocxls.i
		Used by:
		Created: pda
		����஥�� ��ࠬ����:
		�����䨪����:
*/

{globals.i}
{intrface.get xclass}
{sh-defs.i}

DEF VAR fname  AS CHAR NO-UNDO.
DEF NEW SHARED STREAM vvs.

DEFINE INPUT PARAMETER tranzlst AS CHARACTER NO-UNDO.

DEF VAR vUserName AS CHARACTER NO-UNDO.        /*��� ���짮��⥫�*/
DEF VAR strdate   AS CHARACTER NO-UNDO.
DEF VAR userlst   AS CHARACTER NO-UNDO.        /*ᯨ᮪ ���짮��⥫�� ��� ����*/
/*DEF VAR tranzlst  AS CHARACTER NO-UNDO.*/    /*ᯨ᮪ �࠭���権*/
 
DEF TEMP-TABLE tt-op NO-UNDO 
	FIELD date-op    LIKE op-entry.op-date /*��� ����樨*/
	FIELD doc-num    LIKE op.doc-num       /*����� ���㬥��*/
	FIELD acct-db    LIKE op-entry.acct-db /*��� �����*/
	FIELD acct-cr    LIKE op-entry.acct-cr /*��� �।��*/
	FIELD amt        LIKE op-entry.amt-rub /*�㬬�*/
	FIELD details    LIKE op.details       /*�����祭�� ���⥦�*/
	FIELD user-name  AS   CHARACTER        /*��� ���짮��⥫�*/
	.

{getdates.i}

IF beg-date EQ end-date THEN
strdate = "���� �� " + STRING(beg-date,"99.99.9999").
  ELSE 
  strdate = "���� �� ��ਮ� � " + STRING(beg-date,"99.99.9999") + " �� " + STRING(end-date,"99.99.9999").

ASSIGN 
  userlst = "K0400IEV,AKO_VNV,U0400LEP,AKO_SEL,AKO_BNV,AKO_CKS,OIK_KEA,0000KDN,KFO_PAA,0000KIA,U0400LUV,K0400VEI,0000GTS,K0400MVS,0000KSA,OKO_REV"
.

FOR EACH op-entry
	WHERE op-entry.op-date GE beg-date
	  AND op-entry.op-date LE end-date
	  AND (op-entry.acct-db BEGINS "706"
	  OR op-entry.acct-cr BEGINS "706")
	  AND CAN-DO (userlst, op-entry.user-id)
	NO-LOCK,
	FIRST op OF op-entry
		WHERE CAN-DO (tranzlst, op.op-kind)
	NO-LOCK QUERY-TUNING(NO-INDEX-HINT):
      FIND FIRST _user 
           WHERE _user._userid EQ op-entry.user-id 
      NO-LOCK NO-ERROR.
         IF AVAIL(_user) THEN DO:
            ASSIGN
              vUserName = _user._user-name
            .
         END.
	      CREATE tt-op.
   	   	ASSIGN
   	   	   tt-op.date-op   = op-entry.op-date
   	   	   tt-op.doc-num   = op.doc-num
   	   	   tt-op.acct-db   = delFilFromAcct(op-entry.acct-db)
   	   	   tt-op.acct-cr   = delFilFromAcct(op-entry.acct-cr)
   	   	   tt-op.amt       = op-entry.amt-rub
   	   	   tt-op.details   = op.details
   	   	   tt-op.user-name = vUserName
   	   	   .
END. /*FOR EACH op-entry*/

{handdocxls.i}
/*
{setdest.i &file-name = "111.log"}
FOR EACH tt-op NO-LOCK:
  PUT UNFORMATTED
	 tt-op.date-op   "; "
	 tt-op.doc-num   "; "
	 tt-op.acct-db   "; "
	 tt-op.acct-cr   "; "
	 tt-op.amt       "; "
	 tt-op.details   "; "
	 tt-op.user-name "; "
  SKIP.   
END.   
/*
PUT UNFORMATTED ETIME " msec" SKIP.
*/
{preview.i &file-name = "111.log"}
*/