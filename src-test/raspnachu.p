{globals.i}

DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE vKindDate   AS DATE   NO-UNDO.
DEFINE VARIABLE vKindChar   AS CHAR   NO-UNDO.
DEFINE VARIABLE vTemplInt   AS INT64    NO-UNDO.
DEFINE VARIABLE vLongInt    AS INT64    NO-UNDO.
DEFINE VARIABLE vCommHandle AS HANDLE NO-UNDO. /*㪠��⥫� ��  ������⥪� �����ᨩ*/
DEF VAR vResult   AS DEC  NO-UNDO. /*��६���� */
DEF VAR vResult1  AS DEC  NO-UNDO. /*���        */
DEF VAR vBegDate  AS DATE NO-UNDO. /*�맮��     */
DEF VAR vComm     AS CHAR NO-UNDO. /*��� ����ᨨ*/
DEF VAR vInter    AS CHAR NO-UNDO. /*�奬� ���᫥���*/
DEF VAR vFlag     AS LOG  NO-UNDO. /*���᫥��� */
DEF VAR fNum      AS INT64    NO-UNDO. /*����� ��ப� ��� �뢮��*/
DEF VAR fLoan     AS CHAR   NO-UNDO. /*����� � ��� �������*/
DEF VAR vSubCod   AS CHAR NO-UNDO. /*⨯ ���⪠*/
DEF VAR vRateComm AS DEC  NO-UNDO. /*�⠢�� �� ������ ������*/
DEF VAR vAcctCust AS CHAR NO-UNDO. /*������������ �������� ���*/
DEF VAR vAmount   AS DEC  NO-UNDO. /*�⠢�� �� ������ ������*/

DEFINE BUFFER op-entry FOR op-entry.
DEFINE BUFFER op FOR op.
DEFINE BUFFER kau FOR kau.
DEFINE BUFFER kau-entry FOR kau-entry.
DEFINE BUFFER comm-rate FOR comm-rate.
DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER acct FOR acct.
DEFINE BUFFER bal-acct FOR bal-acct.

DEF TEMP-TABLE Rep-tt NO-UNDO
  FIELD ContCode AS CHAR        /*����� ������*/
  FIELD TypeOp   AS CHAR        /*⨯ - ���᫥���/㯫��*/
  FIELD OpenDate AS DATE        /*��� ������*/
  FIELD StDate   AS DATE        /*��� �ਢ��祭�� �।��*/
  FIELD Acct     AS CHAR        /*��� �� ����*/
  FIELD AcctCust AS CHAR        /*�������� ���*/
  FIELD Rate     AS DEC         /*% �⠢��*/
  FIELD Amount   AS DEC         /*% �㬬� �� ����*/
  FIELD EndDate  AS DATE INIT ? /*��� ������ �।��*/
INDEX main-ind ContCode OpenDate.
/*
{tmprecid.def}
*/
FORM
  fNum
     COLUMN-LABEL "N!�/�"
     FORMAT       ">>>>9"
  fLoan
     COLUMN-LABEL "����� � ��� �������"
     FORMAT       "x(31)"
  Rep-tt.AcctCust
     COLUMN-LABEL "������������!��������!���"
     FORMAT       "x(30)"
  Rep-tt.Acct
     COLUMN-LABEL "N!C��"
     FORMAT       "x(20)"
  Rep-tt.TypeOp
     COLUMN-LABEL "����ঠ���!����樨"
     FORMAT       "x(12)"
  Rep-tt.StDate
     COLUMN-LABEL "���!�ਢ��祭��"
     FORMAT       "99/99/9999"
  Rep-tt.EndDate
     COLUMN-LABEL "���!������"
     FORMAT       "99/99/9999"
  Rep-tt.Rate
     COLUMN-LABEL "�⠢�� %"
     FORMAT       ">>>>>>>9.99"
  Rep-tt.Amount
     COLUMN-LABEL "�㬬�"
     FORMAT       ">>>>>>>9.99"
HEADER "�ਫ������ 1 � ������樨"                                AT 90 SKIP
       "'� ���浪� ��壠���᪮�� ��� ����権 ���᫥��� �"    AT 62 SKIP
       "㯫��� / ����祭�� ��業⮢ �� �ਢ��祭�� �"           AT 69 SKIP
       "ࠧ��饭�� �।�⢠�'"                                   AT 93 SKIP
       "�㪮����⥫� ___________________________________________" AT 59 SKIP
       "(������������ ���ࠧ�������)"                             AT 75 SKIP
       "������������"                                             AT 50 SKIP
       "�� ��壠���᪮� ��ࠦ���� ����権 ���᫥��� � 㯫��� ��業⮢ ��" AT 20 SKIP
       "᫥���騬 ������ࠬ �� �ਢ��祭�� �।��"                          AT 30 SKIP
       "�� " + STRING(end-date, "99.99.9999") + " �."   FORMAT "X(16)"  AT 50       SKIP(4)

WITH FRAME RepFrame DOWN WIDTH 150 TITLE "" .

{def_work.i new}
{dpsproc.def}

{f_for_t.i}

{intrface.get loan}
{intrface.get tmess}

RUN LOAD_NACHTOOL (YES, OUTPUT vCommHandle).

/* ���᮪ 䨫�����, �� ����� ��⮢�� �믨᪨ */
ASSIGN
mFilialList = "0000,0300"
mNmFil = "gb,tf"
.
{getdate.i}
{setdirel.i}

/* ���� �ନ஢���� �믨᮪ */
DO mI = 1 TO NUM-ENTRIES(mFilialList):
{empty Rep-tt}               /*��⨬ ࠡ���� ⠡����*/
    FOR EACH op-entry
	    WHERE op-entry.op-date EQ end-date
	    AND op-entry.filial-id EQ ENTRY(mI, mFilialList)
	    AND op-entry.acct-db BEGINS "70606"
	    AND op-entry.acct-cr BEGINS "47426"
		NO-LOCK, 
		FIRST op
	    WHERE op.op EQ op-entry.op
	    AND op.op-date EQ end-date
	    AND op.op-status BEGINS CHR(251)
	    NO-LOCK:
	    IF NUM-ENTRIES(op-entry.kau-cr) EQ 3 THEN
       DO:
		    FIND FIRST loan
		    	WHERE loan.contract EQ ENTRY(1, op-entry.kau-cr)
		    	AND loan.cont-code  EQ ENTRY(2, op-entry.kau-cr)
		    NO-LOCK NO-ERROR.
		    FOR EACH loan-acct
		    	WHERE loan-acct.contract EQ loan.contract
		    	AND loan-acct.cont-code  EQ loan.cont-code
		    	AND loan-acct.acct-type EQ "�����"
		    	AND loan-acct.since LE end-date
		    NO-LOCK 
		    BY loan-acct.since DESC:
		    	LEAVE.
		    END.
		    FIND FIRST acct
		    	WHERE acct.acct EQ loan-acct.acct
		    NO-LOCK NO-ERROR.

	        FIND LAST cust-corp WHERE
	                  cust-corp.cust-id = loan.cust-id
	        NO-LOCK NO-ERROR.

	        vAcctCust  = IF AVAIL cust-corp
	                     THEN cust-corp.name-short
	                     ELSE "?".
          FOR EACH comm-rate
            WHERE comm-rate.filial-id EQ ENTRY(mI, mFilialList) AND 
                  comm-rate.since LE gend-date AND
                  comm-rate.kau EQ ENTRY(1, op-entry.kau-cr) + "," + ENTRY(2, op-entry.kau-cr)
            NO-LOCK
            BY comm-rate.since DESC:
            LEAVE.
          END.

    	    vAmount = (IF op-entry.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
/*	        END. */

	      /*ᮧ���� ࠡ���� ⠡����*/
	      CREATE Rep-tt.
	      ASSIGN
	        Rep-tt.ContCode = ENTRY(1, loan.cont-code, "@")
	        Rep-tt.TypeOp   = "���᫥���%"
	        Rep-tt.OpenDate = loan.open-date
	        Rep-tt.Acct     = TRIM(ENTRY(1, loan-acct.acct, "@"))
	        Rep-tt.Rate     = comm-rate.rate-comm
	        Rep-tt.EndDate  = loan.end-date WHEN loan.end-date <> ?
	        Rep-tt.AcctCust = vAcctCust
	        Rep-tt.StDate   = loan.open-date
	        Rep-tt.Amount   = vAmount
	        .
	    END.
	END.
/*�뢮� ������*/
{setdest.i}

fNum = 1.
FOR EACH Rep-tt USE-INDEX main-ind WITH  FRAME RepFrame:
   DISPLAY
     fNum
     string(Rep-tt.ContCode,"x(20)") + " " + STRING(Rep-tt.OpenDate,"99/99/9999") @ fLoan
     Rep-tt.AcctCust
     Rep-tt.Acct
     Rep-tt.TypeOp
     Rep-tt.StDate
     Rep-tt.EndDate WHEN Rep-tt.EndDate <> ? @ Rep-tt.EndDate
     Rep-tt.Rate
     Rep-tt.Amount
     .
   DOWN.
   fNum = fNum + 1.
END.

DO WITH FRAME RepFrame :
  UNDERLINE
    fNum
    fLoan
    Rep-tt.AcctCust
    Rep-tt.Acct
    Rep-tt.TypeOp
    Rep-tt.StDate
    Rep-tt.EndDate
    Rep-tt.Rate
    Rep-tt.Amount
    .
END.

PUT UNFORMATTED
  SKIP(1)
  '������� 㯮�����祭���� ��� __________________________(���������, �.�.�.) "__"_____________ 201 �'
  SKIP(1).

{preview.i}
 OS-COPY VALUE("./_spool.tmp") VALUE( mDir + mDirElec + "//raspnach-ul-" + ENTRY(mI, mNmFil) + "-" + STRING(end-date, "99.99.9999") + ".txt"). 

END.
{intrface.del}
