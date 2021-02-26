/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2002 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: rsrv-chk.p
      Comment: �᪫�祭�� �� �६����� ⠡���� �� ������஢, �� �����
               ��� ������ � ⥪�騩 ����.

   Parameters:
      Used by:
      Created: 
     Modified: xaro 01.12.2005 ��� 0054846
*/

{globals.i}
{flt-file.i}        /* ������� 䨫��� �� ������ࠬ */
{all_note.def}      /* ������ � recid, ��࠭��� �� 䨫���� ����ᥩ Shared
                       � ��⮩ ⥪�饣� ����.��� - in_op_date */
{sh-defs.i}
{flt_var.def}
{intrface.get olap}
{intrface.get loan}
{intrface.get pint}
{checkov.i}


FUNCTION loan-ost returns decimal (
    INPUT vContract AS CHAR,
    INPUT vContCode AS CHAR,
    INPUT vSince AS DATE,
    INPUT vAcctType AS CHAR):
    
    DEF VAR vRes AS DEC NO-UNDO.
    FIND LAST loan-acct
        WHERE loan-acct.contract EQ vContract
          AND loan-acct.cont-code EQ vContCode
          AND loan-acct.acct-type EQ vAcctType
          AND loan-acct.since <= vSince
        NO-LOCK NO-ERROR.
    IF AVAIL loan-acct THEN DO:
      RUN acct-pos IN h_base (loan-acct.acct,
                           loan-acct.currency,
                           vSince,
                           vSince,
                           "�"            /* = ����� "���" */
                           ).
      find first acct of loan-acct no-lock.
      vRes = (IF acct.side EQ "�" THEN sh-bal ELSE (- sh-bal)).
    END. ELSE vRes = 0.
    RETURN vRes.
end function.

FUNCTION fu-par returns decimal (
    BUFFER iloan FOR loan,
    INPUT vSince AS DATE,
    INPUT vParm AS INT):

    DEF VAR vAmt AS DEC NO-UNDO.
    DEF VAR vAmt2 AS DEC NO-UNDO.
    DEF VAR vAmt3 AS DEC NO-UNDO.
    DEF VAR vDbSumDec AS DEC NO-UNDO.
    DEF VAR vCrSumDec AS DEC NO-UNDO.
    DEF VAR vTmpCurr        AS CHAR   NO-UNDO.

    run STNDRT_PARAM in h_loan (
	iloan.contract, iloan.cont-code, vParm,
	IF iloan.since >= vSince THEN vSince ELSE iloan.since,
                              output vAmt, output vDbSumDec, output vCrSumDec).
    RUN inter_current(BUFFER iloan, vParm, OUTPUT vAmt2).
    vAmt = vAmt + vAmt2.

    IF iloan.since < vSince THEN DO:
	/* ���� �������� ����樨 �� ���騥 ��� */
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-d = vParm NO-LOCK:
	      vAmt = vAmt + loan-int.amt-rub.
	  END.
	  FOR EACH loan-int OF iloan
	   WHERE loan-int.mdate <= vSince
	     AND loan-int.mdate > iloan.since
	     AND loan-int.id-k = vParm NO-LOCK:
	      vAmt = vAmt - loan-int.amt-rub.
	  END.
    END.
    RETURN vAmt.
END.

DEFINE INPUT PARAMETER iParam  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER iOpRid  AS RECID  NO-UNDO.

DEF VAR mFlDel AS LOG NO-UNDO.
def var vkol-bal-tot as int no-undo.
def var vkol-bal as int no-undo.
DEF VAR vR AS LOG NO-UNDO.

DEF BUFFER bloan FOR loan.
DEF BUFFER xterm-obl FOR term-obl.


/* �஢�ઠ ������⢠ �⮡࠭��� ����ᥩ */
FIND LAST all_recids NO-ERROR.
IF NOT AVAIL all_recids THEN
DO:
   {intrface.del }
   RETURN.
END.


ASSIGN
   gend-date  = svPlanDate
   end-date   = svPlanDate
   in_op_date = svPlanDate.

IF in_op_date EQ ? THEN
   in_op_date = gend-date.


vkol-bal-tot = 0.
FOR EACH all_recids NO-LOCK:
vkol-bal-tot = vkol-bal-tot + 1.
END.
   {bar-beg2.i
        &BarTotal     = vkol-bal-tot
        &BarMessage   = """�⡮� ������஢ ��� ���᫥��� � ����襭��"""
   }

vkol-bal = 0.

FOR EACH all_recids, loan WHERE RECID(loan) = all_recids.rid NO-LOCK:
    mFlDel = FALSE.

/* kam */
   IF checkov(BUFFER loan) THEN mFlDel = TRUE.

if loan.user-id = 'SERVSOUZ' OR loan.loan-status EQ "���" then mFlDel = TRUE.

   IF mFlDel THEN
   DO:
      DELETE all_recids.
      NEXT.
   END.

END. /*FOR EACH*/


vkol-bal-tot = 0.
FOR EACH all_recids NO-LOCK:
vkol-bal-tot = vkol-bal-tot + 1.
END.
/*message "�⮡࠭� " + string(vkol-bal-tot) + " ������஢"  .*/

   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES     FILL (" ",79).
   PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES + 1 FILL (" ",79).
   PAUSE 0.  

{intrface.del}
