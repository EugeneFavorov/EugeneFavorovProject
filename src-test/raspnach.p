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
     FORMAT       ">>9.99"
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
       "᫥���騬 ������ࠬ �� �ਢ��祭�� �।���"                          AT 30 SKIP(4)

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
	    AND op-entry.acct-cr BEGINS "47411"
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
		    	AND CAN-DO("loan-dps-t*,loan-dps-p", loan-acct.acct-type)
		    	AND loan-acct.since LE end-date
		    NO-LOCK 
		    BY loan-acct.since DESC:
		    	LEAVE.
		    END.
		    FIND FIRST acct
		    	WHERE acct.acct EQ loan-acct.acct
		    NO-LOCK NO-ERROR.

	        RUN Get_Last_Commi in h_dpspc (RECID(loan), end-date, End-Date, OUTPUT vComm).
	        RUN Get_Last_Inter in h_dpspc (RECID(loan), end-date, End-Date, OUTPUT vInter).
	        FIND LAST person WHERE
	                  person.person-id = loan.cust-id
	        NO-LOCK NO-ERROR.

	        vAcctCust  = IF AVAIL person
	                     THEN name-last + " " + first-names
	                     ELSE "?".

/*	        { findsch.i
	             &dir    = last
	             &sch    = acct.acct
	             &since1 = " le End-Date"
	        }
	        IF AVAIL interest-sch-line
	        THEN DO:
	          vBegDate = End-Date - 1.
	          RUN nachkin.p(RECID(interest-sch-line),
	                        vComm,
	                        RECID(acct),
	                        End-Date,
	                        loan.contract + "," + loan.cont-code + "," + vSubCod ,
	                        yes,
	                        OUTPUT vResult,
	                        OUTPUT vResult1,
	                        INPUT-OUTPUT vBegDate,
	                        OUTPUT vFlag).
	           vRateComm = DEC(ENTRY(4,RETURN-VALUE)) NO-ERROR.
	        END. 
	        ELSE DO: */
	        RUN GetCommRate(BUFFER acct, vInter, vComm, "����", OUTPUT vRateComm ).
    	    vAmount = (IF op-entry.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
/*	        END. */

	      /*ᮧ���� ࠡ���� ⠡����*/
	      CREATE Rep-tt.
	      ASSIGN
	        Rep-tt.ContCode = ENTRY(1, loan.cont-code, "@")
	        Rep-tt.TypeOp   = "���᫥���%"
	        Rep-tt.OpenDate = loan.open-date
	        Rep-tt.Acct     = TRIM(ENTRY(1, loan-acct.acct, "@"))
	        Rep-tt.Rate     = vRateComm
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
 OS-COPY VALUE("./_spool.tmp") VALUE( mDir + mDirElec + "//raspnach-fl-" + ENTRY(mI, mNmFil) + "-" + STRING(end-date, "99.99.9999") + ".txt"). 

END.
/*���� �⠢�� �� ������*/
PROCEDURE GetCommRate.

   DEF PARAM  BUFFER acct  FOR acct.        /*����� ���*/
   DEF INPUT  PARAM iInter AS CHAR NO-UNDO. /*��� �奬�*/
   DEF INPUT  PARAM iComm  AS CHAR NO-UNDO. /*��� �⠢��*/
   DEF INPUT  PARAM iKau   AS CHAR NO-UNDO. /*��� ���⪠*/
   DEF OUTPUT PARAM oRate  AS DEC  NO-UNDO. /*�⠢��*/

   DEF VAR vDat-commi      AS DATE  NO-UNDO.
   DEF VAR vEnd-commi      AS DATE  NO-UNDO.
   DEF VAR vDat-t          AS DATE  NO-UNDO.
   DEF VAR vDep_per        AS CHAR  NO-UNDO.
   DEF VAR vFloat_interest AS LOG   NO-UNDO.
   DEF VAR vCommRecid      AS RECID NO-UNDO.

   DEF BUFFER ykau-entry FOR kau-entry.

   FIND LAST loan-cond WHERE
             loan-cond.contract  = loan.contract
         AND loan-cond.cont-code = loan.cont-code
         AND loan-cond.since    <= end-date NO-LOCK NO-ERROR.

   FIND LAST interest-sch-line WHERE
             interest-sch-line.interest-sch EQ iInter
         AND interest-sch-line.currency     EQ ""
         AND interest-sch-line.since        LE gend-date
         AND interest-sch-line.acct         = '0'
   NO-LOCK NO-ERROR .

   &glob float-interest nach_pl,n_kau_pl

   vFloat_interest = AVAIL interest-sch-line AND
         CAN-DO("{&float-interest}",interest-sch-line.proc-name).
   /* ���� ���⪠ �� ������ �� ���� gend-date */
   IF vFloat_interest
   THEN ASSIGN
         vEnd-commi = gend-date
         vDat-commi = gend-date
         .
   ELSE
      RUN get-beg-date-prol-vtb in h_dpspc (
                    RECID(loan),
                    loan-cond.since + 1,
                    OUTPUT vDat-commi,
                    OUTPUT vEnd-commi) .

   IF vFloat_interest
   THEN
      vDat-t = gend-date.
   ELSE DO:
      IF vEnd-commi <> ?
      THEN vDat-t = loan-cond.since.
      ELSE vDat-t = gend-date.
      /*  �饬 ��ࢮ� ��������, �᫨ ��� ����� ���� ������ � ���⮪ ������ ������� �� ���� ��ࢮ�� �������� */
      IF vDat-t = loan.open-date
      THEN DO:
         FIND FIRST ykau-entry WHERE
                    ykau-entry.acct     = loan-acct.acct
                AND ykau-entry.currency = loan-acct.currency
                AND ykau-entry.kau      BEGINS  loan.contract + "," + loan.cont-code + ","
                AND ykau-entry.op-date >= vDat-t
                AND NOT ykau-entry.debit NO-LOCK NO-ERROR.
         IF AVAIL ykau-entry
         THEN vDat-t = ykau-entry.op-date.
      END.
   END.

   /* ���� ���⪠ �� ������ �� ���� gend-date */
   RUN kau-pos.p (acct.acct,
                  acct.currency,
                  vDat-commi,
                  vDat-commi,
                  "�",
                  iKau).

   /* ��।������ ���� �஫����樨 ������ */
   RUN RE_CURRENT_KIND in h_dpspc(
            RECID(loan),        /* �����䨪��� ������� */
            gend-date,          /* ���, �� ������ "��� �� �����" */
            OUTPUT vKindDate,   /* ��� �஫����樨/������ */
            OUTPUT vKindChar,   /* ��� �࠭���樨 �஫����樨/������ */
            OUTPUT vTemplInt).  /* ����� 蠡���� ����窨 */

   /* ��।������ �த����⥫쭮�� ������ �� ��� �᫮���  */
   IF vEnd-commi = ?
   THEN vLongInt = 0 .
   ELSE RUN depos-dep-period in h_dpspc
            (RECID(loan),
             loan-cond.since,
             OUTPUT vLongInt) .

   /* ���� ��業⭮� �⠢�� */
   RUN GET_ACTUAL_COMM IN vCommHandle
          (iComm,                     /* ��� ����ᨨ/��� */
           RECID(acct),               /* �����䨪��� ��� */
           ?,                         /* ��� ����������� ������,
                                       ** � ��襬 ��砥 ? */
           ABSOLUTE(IF acct.currency EQ ""
                    THEN ksh-bal
                    ELSE ksh-val),    /* ���⮪ �� ������ �� ���� gend-date */
           vLongInt,                  /* �த����⥫쭮��� ������ */
           vDat-t,                     /* ��� �⮡ࠦ���� �����ᨨ */
           BUFFER comm-rate).

        /* �⮡ࠦ��� �������/���, �᫨ ����筮 �� ��諨 */
   IF AVAIL comm-rate
   THEN DO:
      vCommRecid = RECID(comm-rate).
      /* ���� ��������� ��業⭮� �⠢�� */
      FIND first commission WHERE
          RECID (commission) EQ vCommRecid
      NO-LOCK NO-ERROR.

      /* ���� ����筮�� ��� */
      FIND FIRST comm-rate WHERE
          RECID (comm-rate) EQ vCommRecid
      NO-LOCK NO-ERROR.

      /* �᫨ �� ������ commission,
      ** � �饬 ��� �⭮�⥫쭮 comm-rate */
      IF NOT AVAIL commission
      THEN FIND FIRST commission OF comm-rate
           NO-LOCK NO-ERROR.

      /* �᫨ �� ������ comm-rate
      ** � �饬 ��� �⭮�⥫쭮 commission */
      IF NOT AVAIL comm-rate
      THEN FIND LAST comm-rate OF commission WHERE
                     comm-rate.filial-id = shfilial AND 
                     comm-rate.branch-id = "" AND 
                     comm-rate.since LE gend-date
                 AND comm-rate.acct  EQ "0"
      NO-LOCK NO-ERROR.
      IF AVAIL comm-rate
      THEN oRate = comm-rate.rate-comm.
      RETURN.
       
   END.
   ELSE DO:
      MESSAGE "��� ������" loan.cont-code "�⠢�� �� ��।�����".
      PAUSE.
   END.

END PROCEDURE.


/* ��⨬ ������ */
{intrface.del}
