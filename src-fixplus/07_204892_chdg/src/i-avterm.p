/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: i-avterm.p
      Comment: ����� ��堭���� ࠧ���⪨ ��䨪�� ���⥦�� ��᫥
               ������ ����筮�� �।�� ���
   Parameters:
         Uses:
      Used by:
      Created: ����
     Modified:
*/
{globals.i}
{svarloan.def new}
{form.def}
{intrface.get xclass} /* ����㧪� �����㬥���� ����奬�  */

{intrface.get loan}

DEF INPUT PARAM iRecLoan    AS RECID NO-UNDO. /*recid �������*/
DEF INPUT PARAM iRecCond    AS RECID NO-UNDO. /*recid �᫮���*/
DEF INPUT PARAM iSumm       AS DEC   NO-UNDO. /* �㬬� (term-obl.idnt = 2)*/


DEF VAR mCredOffset     AS CHAR NO-UNDO.
DEF VAR mSurrLoanCond   AS CHAR NO-UNDO.
DEF VAR mOffset         AS CHAR NO-UNDO INIT ",->,<-".
DEF VAR mDelayOffset    AS CHAR NO-UNDO.  /* ᤢ�� ���� ����砭�� ����.��ਮ�� (��.����) */
DEF VAR mDelayOffsetInt AS CHAR NO-UNDO.  /* ᤢ�� ���� ����砭�� ����.��ਮ�� (��業��) */
DEF VAR mCondCount      AS INT64  NO-UNDO.

RUN RE_B_LOAN_BY_RID (iRecLoan,BUFFER loan).

IF NOT AVAIL loan THEN RETURN.

{svarloan.ini
  &incontr = loan.contract}

FIND FIRST loan-cond WHERE RECID(loan-cond) = iRecCond NO-WAIT NO-ERROR.

IF LOCKED loan-cond THEN RETURN {&RET-ERROR}.

ASSIGN
   mSurrLoanCond        = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
   mCredOffset          = GetXAttrValueEx("loan-cond",mSurrLoanCond,"cred-offset","")
   mDelayOffset         = GetXAttrValueEx("loan-cond",mSurrLoanCond,"delay-offset","")
   mDelayOffsetInt      = GetXAttrValueEx("loan-cond",mSurrLoanCond,"delay-offset-int","")
   .


IF GetXAttrInit(loan-cond.class-code, "�奬�����") NE "����७�஢�����" THEN 
DO:
   loan-cond.int-date   =  loan-cond.cred-date.
   loan-cond.int-period =  loan-cond.cred-period.

   

END.

RELEASE loan-cond.

RUN SetSysConf IN h_base(
   "������������� �� �������� �����",
   STRING(LOOKUP(mCredOffset,mOffset))).

RUN SetSysConf IN h_base(
   "������� �� ��������� �����",
   STRING(LOOKUP(mCredOffset,mOffset))).

RUN SetSysConf IN h_base(
   "����. �� �������� ����� ����.�����",
   STRING(LOOKUP(mDelayOffset,mOffset))).

RUN SetSysConf IN h_base(
   "����. �� ����. ����� ����.�����",
   STRING(LOOKUP(mDelayOffsetInt,mOffset))).

FOR EACH loan-cond WHERE loan-cond.contract  EQ loan.contract
                     AND loan-cond.cont-code EQ loan.cont-code
NO-LOCK:
   mCondCount = mCondCount + 1.
END.

RUN mm-to.p (iRecLoan,
             iRecCond,
             iSumm,
             {&MOD_ADD},
             YES,
             YES,
             YES,
             YES,
             loan.risk,
             mCondCount).

RUN DeleteOldDataProtocol IN h_base("������� �� ��������� �����").
RUN DeleteOldDataProtocol IN h_base("������������� �� �������� �����").

{intrface.del}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:47:42.104+04:00' */
/* $LINTFILE='i-avterm.p' */
/*prosignHKk82gqATEfsJ/oddXh3kg*/