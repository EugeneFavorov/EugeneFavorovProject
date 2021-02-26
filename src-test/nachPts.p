/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2007 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: nachpts.p
      Comment: ��楤�� ���᫥��� �����ᨩ ���.
   Parameters:
         Uses:
      Used by:
      Created: kam
     Modified: 
*/

&SCOPED-DEFINE NUM_INTEREST 51

&SCOPED-DEFINE PAR_PTS 519
&SCOPED-DEFINE PAR_NACH 5
&SCOPED-DEFINE PAR_OPLAT 526
&SCOPED-DEFINE DOC_TYPE "���"
&SCOPED-DEFINE SROK_SDACHI "�ப����"
&SCOPED-DEFINE POST_BANK "���⁠��"

{intrface.get lv}
{intrface.get loan}

{globals.i}            /* �������� ��६���� ��ᨨ. */
{t-otch.i}             /* ������� ⠡��窨 otch1 */

DEF INPUT  PARAM iContract   AS CHAR NO-UNDO. 
DEF INPUT  PARAM iContcode   AS CHAR NO-UNDO. 
DEF INPUT  PARAM iBegDate1    AS DATE NO-UNDO. /* = iEndDate - ��� ������ */
DEF INPUT  PARAM iEndDate    AS DATE NO-UNDO. /* = iEndDate - ��� ������ */
DEF INPUT  PARAM iParam      AS CHAR NO-UNDO. /* ��ப� �� �奬��窮�: ����=���.............. */
DEF INPUT  PARAM iCommission AS CHAR NO-UNDO. /* ������������ �����ᨨ */
DEF INPUT  PARAM iLoanPar    AS INT64  NO-UNDO. /* ��ࠬ��� */

DEF BUFFER bufloan      FOR loan.
DEF VAR penyDateSrok AS DATE NO-UNDO.
DEF VAR penyDateBank AS DATE NO-UNDO.
DEF VAR penyRate LIKE comm-rate.rate-comm NO-UNDO.
DEF VAR penyFix AS CHAR NO-UNDO.
DEF VAR penyOstatok AS DECIMAL NO-UNDO.
DEF VAR loanOpenDate AS DATE NO-UNDO. 
DEF VAR summOpl519 AS DECIMAL NO-UNDO.
DEF VAR sumPredInterest AS DECIMAL NO-UNDO.
DEF VAR summPeny AS DECIMAL NO-UNDO.
DEF VAR summAllPeny AS DECIMAL NO-UNDO INIT 0.
DEF VAR tRecid AS INT64 NO-UNDO INIT -1000.

DEF TEMP-TABLE peny NO-UNDO
   FIELD dateSrok AS DATE         /* ��� (�ப ���) */
   FIELD dateBank AS DATE         /* ��� ����祭�� ��� */
   FIELD rate  LIKE comm-rate.rate-comm     /* �⠢�� (�㬬�) */
   FIELD fix   AS CHAR   /* fix (= rate ��� % �� ���⪠ ������������) %,=,1  (1-������६����)   */
   FIELD ostatok AS DECIMAL                 /* ���⮪ ������������ �� ���� since (��� ���� �� % �⠢�� �� �����������) */
   FIELD nachPeny AS DECIMAL                /* ���᫥�� ���� */
   FIELD recidId AS INT64                   /* �᫨ �� 0, � ���, ���� ��� ����(��� �⠢��) */
   .

{empty otch1} /* ��頥� ⠡���� � ������ﬨ */
/*
 message string(iEndDate) view-as alert-box.
 */
FIND FIRST bufloan WHERE
           bufloan.contract  EQ iContract
       AND bufloan.cont-code EQ iContcode
       AND bufloan.close-date EQ ?
NO-LOCK NO-ERROR.

IF NOT AVAIL bufloan THEN DO:
   RETURN.
END.

sumPredInterest = 0.
IF bufloan.since >= bufloan.l-int-date THEN DO:
    sumPredInterest = LoadPar({&NUM_INTEREST}, iContract + "," + iContcode).
END.
  /* message "sumPredInterest " + string(sumPredInterest) view-as alert-box. */ 
loanOpenDate = bufloan.open-date.

FOR EACH term-obl WHERE term-obl.contract = iContract 
    AND term-obl.cont-code = iContcode
    AND term-obl.idnt = 5 NO-LOCK:
    {empty  peny}
    /* �����ᨨ */
    FOR EACH comm-rate WHERE comm-rate.kau = iContract + ',' + iContcode 
        AND (comm-rate.commission = iCommission) /* iCommission = ���� */
        AND comm-rate.since <= iEndDate NO-LOCK:
        CREATE peny.
        ASSIGN  
            peny.dateSrok = comm-rate.since
            peny.dateBank = ?
            peny.rate = comm-rate.rate-comm
            peny.fix = '='
            peny.ostatok = 0
            nachPeny = 0
            peny.recidId = tRecid
        .
        tRecid = tRecid + 1.
    END.
    FIND FIRST peny NO-LOCK NO-ERROR.
    IF NOT AVAIL peny THEN RETURN.

    FIND FIRST peny WHERE peny.dateSrok = loanOpenDate NO-LOCK NO-ERROR.
    IF NOT AVAIL peny THEN DO:
        CREATE peny.
        ASSIGN  
            peny.dateSrok = loanOpenDate
            peny.dateBank = ?
            peny.rate = 0
            peny.fix = '='
            peny.ostatok = 0
            nachPeny = 0
            peny.recidId = -1001
        .
    END.

    FOR EACH pl_indocsreg WHERE pl_indocsreg.file_name = 'term-obl'
      AND pl_indocsreg.doc_type = {&DOC_TYPE}
      AND pl_indocsreg.surrogate = term-obl.cont-code + ' ' + string(term-obl.nn) 
      AND pl_indocsreg.date_value <= iEndDate
      NO-LOCK
      BY RECID(pl_indocsreg):
         RUN CreatePeny(INPUT pl_indocsreg.date_value, INPUT pl_indocsreg.event, INPUT RECID(pl_indocsreg)). 
    END.
    RUN CalcPeny(INPUT iEndDate + 1,OUTPUT summPeny).
    summAllPeny = summAllPeny + summPeny.
END.

/* message "summAllPeny " + string(summAllPeny) view-as alert-box. */ 

/* ⥯��� � ⠡��窥 peny - ࠧ���� ��ਮ�� ������ ��᪮ */
 /* message 'summAllPeny ' + string(summAllPeny) view-as alert-box.  
    run instview.p(TEMP-TABLE peny:HANDLE).
     */
summOpl519 = 0.

/* �饬 ����樨 ������ ����, �������騥 � ��ਮ� */
FOR EACH loan-int WHERE
            loan-int.contract  EQ iContract
      AND   loan-int.cont-code EQ iContCode
      AND   (loan-int.id-d      EQ {&PAR_OPLAT}  /* 526 */  OR loan-int.id-d      EQ {&PAR_NACH} /* 5 */ )
      AND   loan-int.id-k      EQ {&PAR_PTS}  /* 519 */
      AND   loan-int.mdate     >= loanOpenDate
      AND   loan-int.mdate     <= iEndDate
   NO-LOCK:
      summOpl519 = summOpl519 + loan-int.amt-rub.
END.
 /* message 'iEndDate ' + string(iEndDate) view-as alert-box.
    message 'summOpl519 ' + string(summOpl519) view-as alert-box. */        
  
/* IF sumPredInterest <> 0 AND summOpl519 = 0 THEN summAllPeny = summAllPeny - sumPredInterest. */
 /* sumPredInterest = sumPredInterest - summOpl519. */
 summAllPeny = summAllPeny - sumPredInterest. /*  ��� ������ ���। � nach-ind.i �� �㬬� �ਡ������� */
 
summAllPeny = summAllPeny - summOpl519. 
 /* message 'summAllPeny ' + string(summAllPeny) view-as alert-box. */
    CREATE otch1.
    ASSIGN
        otch1.bal-summ = ?
        otch1.beg-date = iEndDate
        otch1.end-date = iEndDate
        otch1.ndays    = ?
        otch1.rat1     = ?
        otch1.summ_pr = summAllPeny
        otch1.rate-fixed = TRUE
        .


{intrface.del}

/*
run instview.p(TEMP-TABLE otch1:HANDLE).
*/
/* ����� �᭮���� ���  */


/*------------------------------------------------------*/
/*------------------------------------------------------*/
/*------------------------------------------------------*/
/*------------------------------------------------------*/
/*------------------------------------------------------*/
/* ᮧ���� ������ peny */
PROCEDURE CreatePeny:
    DEFINE INPUT PARAMETER iDatePeny AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER iEvent AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER iRecid AS INT64 NO-UNDO.
    DEF VAR bFindPeny AS LOGICAL NO-UNDO.
    
    IF iEvent = {&SROK_SDACHI} THEN DO:
        FIND FIRST peny WHERE peny.dateSrok = iDatePeny AND peny.recidId < 0 NO-ERROR.
        IF AVAIL peny THEN peny.recidId = iRecid.
        ELSE DO:
            bFindPeny = FALSE.
            FOR EACH peny WHERE peny.dateSrok < iDatePeny 
              AND peny.recidId < iRecid NO-LOCK BY peny.recidId DESCENDING:
                bFindPeny = TRUE.
                penyRate = peny.rate.
                penyFix = peny.fix.
                penyOstatok = peny.ostatok.
                LEAVE.
            END.
            IF bFindPeny THEN DO:  /* �᫨ ������� ������ � �⠢��� ���� */
                CREATE peny.
                ASSIGN
                    peny.dateSrok = iDatePeny
                    peny.dateBank = ?
                    peny.rate = penyRate
                    peny.fix = penyFix
                    peny.ostatok = penyOstatok
                    peny.nachPeny = 0
                    peny.recidId = iRecid
                .
            END.
        END.
    END.   
    
    IF iEvent = {&POST_BANK} THEN DO:
        FOR EACH peny WHERE /* peny.dateSrok <= iDatePeny /*+ 1 */
          AND */ 
          peny.dateBank = ?
          AND peny.recidId < iRecid NO-LOCK BY peny.recidId DESCENDING:
            peny.dateBank = iDatePeny + 1.
            LEAVE.
        END.
    END.
END PROCEDURE.
  
    
/* ��⠥� ���� */
PROCEDURE CalcPeny:
    DEFINE INPUT PARAMETER dEndDate AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER iSummPeny AS DECIMAL NO-UNDO.
    DEFINE VAR tmpSumm AS DECIMAL NO-UNDO.
    DEFINE VAR prevDate AS DATE NO-UNDO.
    iSummPeny = 0.
    FOR EACH peny WHERE peny.recidId > 0 NO-LOCK:
        IF peny.dateBank = ? OR peny.dateBank > dEndDate THEN prevDate = dEndDate.  
            ELSE prevDate = peny.dateBank.
        IF prevDate > (peny.dateSrok + 1) THEN DO:  /* 30.06.14   +1 */
            tmpSumm = peny.rate * (prevDate - (peny.dateSrok + 1)). /* 30.06.14   +1 */  
            peny.nachPeny = tmpSumm.
        END. 
    END.
   
   /* run instview.p(TEMP-TABLE peny:HANDLE). */
    
    FOR EACH peny NO-LOCK:
        iSummPeny = iSummPeny + peny.nachPeny.
    END.
    
END PROCEDURE.   















