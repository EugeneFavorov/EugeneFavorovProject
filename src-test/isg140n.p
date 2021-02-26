/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1997 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ISG140n.P
      Comment: �奬� ������ �� ॠ��� ����������� ���⮪
               �� ��� �� ��ਮ�, ����� �ॢ��室�� ������. 
               ���᫥��� �ந�室�� ⮫쪮 �� �� ��ਮ�, 
               ����� ���⮪ �� ��� �ॢ�蠫 ������. 
   Parameters:
      Created: shib 30/06/2006 
     Modified:
     
*/

DEFINE INPUT PARAM rid1         AS RECID                   NO-UNDO. /* �� ��� %% */
DEFINE INPUT PARAM in-commi     LIKE commission.commission NO-UNDO. /* �������  */
DEFINE INPUT PARAM rid          AS RECID                   NO-UNDO. /* ��� when acct */
DEFINE INPUT PARAM in_kau       LIKE kau.kau               NO-UNDO. /* ��� when avail kau */
DEFINE INPUT PARAM ipcurr_beg     AS DATE                  NO-UNDO. /* ��� ��砫� ��ਮ�� */
DEFINE INPUT PARAM ipcurr_end     AS DATE                  NO-UNDO. /* ��� ����砭��  ��ਮ�� */

{globals.i}
{def_work.i}            /* ��।������ ⠡���� fost */
{intrface.get date}     /* �����㬥��� ��� ࠡ��� � ��⠬�. */
/*{intrface.get xclass} */

DEFINE VARIABLE nach_h          AS HANDLE      NO-UNDO. /* �����⥫� �� �����㬥��਩ */
DEFINE VARIABLE vMinOst         AS DECIMAL     NO-UNDO. /* ������ */
DEFINE VARIABLE vMinRealOst     AS DECIMAL     NO-UNDO. /* ������ ��������� ���⮪ �� ���� �� ��ਮ� */
DEFINE VARIABLE mDays           AS INT64     NO-UNDO. /* ������⢮ ���� � ��ਮ��. */
DEFINE VARIABLE curr_beg AS DATE       NO-UNDO.
DEFINE VARIABLE curr_end AS DATE       NO-UNDO.
DEFINE VARIABLE dt_dop AS DATE       NO-UNDO.
DEFINE VARIABLE dd AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mAcctOpen    AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstAct    AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstActDB  AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstActCR  AS DATE    NO-UNDO.
DEFINE VARIABLE mFirstPeriod AS LOGICAL NO-UNDO.
DEFINE VARIABLE mNumDays     AS INT64   NO-UNDO.
curr_beg = ipcurr_beg.
curr_end = ipcurr_end.

FIND FIRST acct WHERE recid(acct) EQ rid
     NO-LOCK NO-ERROR.

/* ����㧪� �����㬥���� */
RUN load_nachtool (NO, OUTPUT nach_h).

/* ���� �����ᨨ */
RUN get_sch_line_by_rid IN nach_h (rid1, BUFFER interest-sch-line).

/* ���� ��� */
RUN GET_ACCT_BY_RID in nach_h (rid, BUFFER acct).
IF NOT AVAIL acct THEN 
DO:
    /* ���㧪� �����㬥���� */
    RUN remove_nachtool (NO, nach_h).
    RETURN "��� �� ������".
END.

ASSIGN
   mAcctOpen = acct.open-date
   mFirstAct = ?.
   
FOR EACH op-entry WHERE
       op-entry.acct-db   EQ acct.acct
   AND op-entry.filial-id EQ acct.filial-id
   AND op-entry.op-status GE CHR(251)  
   AND op-entry.op-date   GE DATE("01/01/2015")
   NO-LOCK:
      mFirstActDB = op-entry.op-date.
   LEAVE.     
END.
FOR EACH op-entry WHERE
       op-entry.acct-cr   EQ acct.acct
   AND op-entry.filial-id EQ acct.filial-id
   AND op-entry.op-status GE CHR(251)    
   AND op-entry.op-date   GE DATE("01/01/2015")
   NO-LOCK:
      mFirstActCR = op-entry.op-date.
   LEAVE.     
END.
IF      mFirstActDB NE ? 
   AND  mFirstActCR NE ? THEN mFirstAct = MIN(mFirstActDB,mFirstActCR).
ELSE IF mFirstActDB EQ ? 
   AND  mFirstActCR NE ? THEN mFirstAct = mFirstActCR.
ELSE IF mFirstActDB NE ? 
   AND  mFirstActCR EQ ? THEN mFirstAct = mFirstActDB.
ELSE IF mFirstActDB EQ ? 
   AND  mFirstActCR EQ ? THEN mFirstAct = ?.

IF     YEAR (mFirstAct) EQ YEAR (curr_end)  
   AND MONTH(mFirstAct) EQ MONTH(curr_end) 
THEN
ASSIGN 
   mFirstPeriod = YES
   curr_beg     = MAX(mFirstAct + 1,curr_beg)
   mNumDays     = curr_end - mFirstAct.
ELSE 
ASSIGN
   mFirstPeriod = NO.
   
/* ���� �������쭮�� ���⪠ ��� ��� vMinOst */
RUN GetMinRemainder IN nach_h (
    acct.acct,
    acct.currency,
    OUTPUT vMinOst).
IF RETURN-VALUE NE "" THEN 
DO:
    /* ���㧪� �����㬥���� */
    RUN remove_nachtool (NO, nach_h).
    RETURN RETURN-VALUE.
END.

/* �������� �������᪨� ���⪮� �� ���� */
RUN CREATE_REAL_FOST IN nach_h (rid, curr_beg, curr_end).
    
/* ����祭�� �������쭮�� ���⪠ �� ��ନ஢����� ��������. */
RUN GetMinAmt        IN nach_h (OUTPUT vMinRealOst).

IF vMinRealOst EQ 0 THEN 
DO:
    /* ���㧪� �����㬥���� */
    RUN remove_nachtool (NO, nach_h).
    RETURN "��������� ���⮪ �� ���� ࠢ�� ���, ���᫥��� �� �ந��������.".
END.

RUN CorrectFostBalance IN nach_h (vMinRealOst,vMinOst,"���⮪ �� �ॢ�蠫 ������, ���᫥��� �� �ந��������.").
IF RETURN-VALUE NE "" THEN 
DO:
    /* ���㧪� �����㬥���� */
    RUN remove_nachtool (NO, nach_h).
    RETURN RETURN-VALUE.
END. 

/* ���᫥��� ��ਮ��.
** ��।������ ������⢠ ���� � ���ࢠ��. */
mDays = cDay(interest-sch-line.interest-month, curr_beg, curr_end + 1).

/* ��ନ஢���� �����ᨨ. */
IF mFirstPeriod EQ YES 
   AND mNumDays LT 15
THEN
   RUN CREATE_RATE_CR IN nach_h (in-commi, rid, ?, ?, 0, mDays, curr_end).
ELSE   
   RUN CREATE_RATE_CR IN nach_h (in-commi, rid, ?, ?, ?, mDays, curr_end).
IF RETURN-VALUE NE ""
THEN DO:
    /* ���㧪� �����㬥���� */
   RUN remove_nachtool (NO, nach_h).
   RETURN RETURN-VALUE.
END.

/* ����� ���᫥��� � �ନ஢���� ���� */
RUN NACH_AND_REPORT IN nach_h (interest-sch-line.interest-sch, 
                               acct.acct,
                               acct.currency, 
                               in_kau, 
                               curr_beg, 
                               curr_end,
                               interest-sch-line.interest-month, 
                               interest-sch-line.basis-time).



/* ���㧪� �����㬥���� */
RUN remove_nachtool (NO, nach_h).

RETURN "".
