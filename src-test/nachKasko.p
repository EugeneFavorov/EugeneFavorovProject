/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2007 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: nachkasko.p
      Comment: ��楤�� ���᫥��� �����ᨩ ��᪮.
   Parameters:
         Uses:
      Used by:
      Created: kam
     Modified: 
*/

&SCOPED-DEFINE NUM_INTEREST 50
&SCOPED-DEFINE NUM_DAYS_SHTRAF 10

&SCOPED-DEFINE PAR_KASK 509
&SCOPED-DEFINE PAR_NACH 5
&SCOPED-DEFINE PAR_OPLAT 516

{intrface.get lv}
{intrface.get loan}

{globals.i}            /* �������� ��६���� ��ᨨ. */
{t-otch.i}             /* ������� ⠡��窨 otch1 */

/* ��ਠ�� ���᫥��� ���� %(��������� % �� ���⪠ ��),=(��������� �����⭠� �㬬�), 1- (������६����) */ 
FUNCTION GetFix RETURNS CHAR(INPUT bFix AS LOGICAL, INPUT iDateOpen AS DATE):  
    DEFINE VAR cRet AS CHAR NO-UNDO.
    IF iDateOpen>= DATE("08.07.2013") THEN DO:
        cRet = "1".
    END.
    ELSE DO:
        IF bFix THEN cRet = "=".
            ELSE cRet = "%".
    END.
    RETURN cRet.
END FUNCTION.

/*�㭪��, ��������� ������⢮ ࠡ��� ���� ����� ���� 㪠����묨 ��⠬�.*/
FUNCTION NumWorkday RETURN INT64 (INPUT IN-DATE-from AS DATE,INPUT IN-DATE-TO AS DATE).
  DEF VAR datatt AS DATE NO-UNDO.
  DEF VAR i AS INT64 NO-UNDO.
  DO datatt = in-date-from TO in-date-to:
    IF NOT holiday(datatt) THEN i = i + 1.
  END.
  RETURN i.
END FUNCTION.

DEF INPUT  PARAM iContract   AS CHAR NO-UNDO. 
DEF INPUT  PARAM iContcode   AS CHAR NO-UNDO. 
DEF INPUT  PARAM iBegDate1    AS DATE NO-UNDO. /* = iEndDate - ��� ������ */
DEF INPUT  PARAM iEndDate    AS DATE NO-UNDO. /* = iEndDate - ��� ������ */
DEF INPUT  PARAM iParam      AS CHAR NO-UNDO. /* ��ப� �� �奬��窮�: ����=���.............. */
DEF INPUT  PARAM iCommission AS CHAR NO-UNDO. /* ������������ �����ᨨ */
DEF INPUT  PARAM iLoanPar    AS INT64  NO-UNDO. /* ��ࠬ��� */


DEF BUFFER bufloan      FOR loan.
DEF BUFFER bufsigns     FOR signs.
DEF VAR penySince LIKE comm-rate.since NO-UNDO.
DEF VAR penyRate LIKE comm-rate.rate-comm NO-UNDO.
DEF VAR penyFix AS CHAR NO-UNDO.
DEF VAR penyOstatok AS DECIMAL NO-UNDO.
DEF VAR penyDateBeg AS DATE NO-UNDO.
DEF VAR penyDateRast AS DATE NO-UNDO.
DEF VAR penyCommis AS LOGICAL NO-UNDO.
DEF VAR cFix AS CHAR NO-UNDO.
DEF VAR loanOpenDate AS DATE NO-UNDO. 
DEF VAR summOpl AS DECIMAL NO-UNDO.
DEF VAR summOpl509 AS DECIMAL NO-UNDO.
DEF VAR summPeny AS DECIMAL NO-UNDO.
DEF VAR sumPredInterest AS DECIMAL NO-UNDO.
DEF VAR sumPredNach AS DECIMAL NO-UNDO.
DEF VAR sumTmp AS DECIMAL NO-UNDO.
DEF VAR bCalcVpered AS LOGICAL NO-UNDO.

DEF TEMP-TABLE peny NO-UNDO
   FIELD since LIKE comm-rate.since         /* ��� */
   FIELD rate  LIKE comm-rate.rate-comm     /* �⠢�� (�㬬�) */
   FIELD fix   AS CHAR   /* fix (= rate ��� % �� ���⪠ ������������) %,=,1  (1-������६����)   */
   FIELD ostatok AS DECIMAL                 /* ���⮪ ������������ �� ���� since */
   FIELD nachPeny AS DECIMAL                /* ���᫥�� ���� */
   FIELD commis AS LOGICAL                  /* ������� (��� �஬������ �����) */
   FIELD kasko AS LOGICAL                   /* ����稥 ��᪮ */
   INDEX ss IS UNIQUE since.

{empty otch1}
{empty  peny}

FIND FIRST bufloan WHERE
           bufloan.contract  EQ iContract
       AND bufloan.cont-code EQ iContcode
       AND bufloan.close-date EQ ?
NO-LOCK NO-ERROR.

IF NOT AVAIL bufloan THEN DO:
   RETURN.
END.

bCalcVpered = FALSE.
sumPredInterest = 0.
IF bufloan.since >= bufloan.l-int-date THEN DO:
    bCalcVpered = TRUE.
    sumPredInterest = LoadPar({&NUM_INTEREST}, iContract + "," + iContcode).
END.
 
loanOpenDate = bufloan.open-date.
                     
/* �����ᨨ */
FOR EACH comm-rate WHERE comm-rate.kau = iContract + ',' + iContcode 
    AND (comm-rate.commission = iCommission) /* iCommission = ���᪮ */
    AND comm-rate.since <= iEndDate NO-LOCK:
    CREATE peny.
    ASSIGN  peny.since = comm-rate.since
            peny.rate = comm-rate.rate-comm
            peny.fix = GetFix(INPUT comm-rate.rate-fixed,INPUT bufloan.open-date)
            peny.ostatok = 0
            nachPeny = 0
            peny.commis = TRUE
            peny.kasko = FALSE
    .
END.
FIND FIRST peny NO-LOCK NO-ERROR.
IF NOT AVAIL peny THEN RETURN.


FOR EACH bufloan WHERE bufloan.class-code = 'insurance' AND bufloan.contract EQ '�����' AND bufloan.parent-cont-code EQ
    iContcode AND bufloan.parent-contract EQ '�।��' AND bufloan.open-date <= iEndDate 
    ,FIRST signs WHERE signs.file-name = 'loan' 
    AND signs.code = 'VidStr' AND signs.surrogate = '�����,' + bufloan.cont-code 
    AND (signs.xattr-value = '�����_�' OR signs.xattr-value = '�����_�') NO-LOCK BY bufloan.open-date:
    
    FIND FIRST bufsigns WHERE bufsigns.file-name = 'loan' 
        AND bufsigns.code = 'DateRastor' AND bufsigns.surrogate = '�����,' + bufloan.cont-code NO-LOCK NO-ERROR.
    IF AVAIL bufsigns AND DATE(bufsigns.code-value) < bufloan.end-date THEN penyDateRast = DATE(bufsigns.code-value).
    ELSE penyDateRast = bufloan.end-date. /* ��� ���থ��� ������� */
    penyDateBeg = bufloan.open-date.
    /*
message 'iEndDate ' + string(iEndDate) + ' penyDateBeg ' + string(penyDateBeg) + ' penyDateRast ' + string(penyDateRast)
    view-as alert-box.
      */  
    RUN CreatePeny(penyDateBeg,TRUE).
    RUN CreatePeny(penyDateRast,FALSE).
    FOR EACH peny WHERE peny.since >= penyDateBeg AND peny.since < penyDateRast:
        peny.kasko = TRUE.
    END. 
    FOR EACH peny WHERE peny.since >= penyDateRast:
        peny.kasko = FALSE.
    END. 
    

    
END.
RUN CreatePeny(iEndDate + 1,TRUE). /* ᮧ���� ������ � ��⮩ ���� */
FOR EACH peny WHERE peny.since > (iEndDate + 1):
    DELETE peny.
END.
/* ⥯��� � ⠡��窥 peny - ࠧ���� ��ਮ�� ������ ��᪮ */

 /*   run instview.p(TEMP-TABLE peny:HANDLE). */

summOpl = 0.
summOpl509 = 0.

/* �饬 ����樨 ������ ����, �������騥 � ��ਮ� */
FOR EACH loan-int WHERE
            loan-int.contract  EQ iContract
      AND   loan-int.cont-code EQ iContCode
      AND   loan-int.id-d      EQ {&PAR_OPLAT}  /* 516 */
      AND   loan-int.id-k      EQ {&PAR_KASK}  /* 509 */
      AND   loan-int.mdate     >= loanOpenDate
      AND   loan-int.mdate     <= iEndDate 
   NO-LOCK:
      summOpl509 = summOpl509 + loan-int.amt-rub.
      /* ������� ���.������ �� ���᫥��� ���� */
/*      RUN CreateAutoOpers (iContract,
                           iContCode,
                           loan-int.mdate,
                           loan-int.amt-rub,
                           {&PAR_KASK},   /* 509 */
                           {&PAR_NACH}). /* 5 */
                           */
END.
/*
FOR EACH loan-int WHERE
            loan-int.contract  EQ iContract
      AND   loan-int.cont-code EQ iContCode
      AND   loan-int.id-d      EQ {&PAR_NACH}  /* 5 */
      AND   loan-int.id-k      EQ {&PAR_OPLAT}  /* 516 */
      AND   loan-int.mdate     >= loanOpenDate
      AND   loan-int.mdate     <= iEndDate 
   NO-LOCK:
      summOpl = summOpl + loan-int.amt-rub.
END.
*/

RUN CalcPeny(INPUT summOpl509 + summOpl,INPUT iEndDate, OUTPUT summPeny).
/*
message "summPeny " + string(summPeny) + " sumPredInterest " + string(sumPredInterest) + " summOpl509 " + string(summOpl509)
    view-as alert-box.
    */
summPeny = summPeny - sumPredInterest. /*  ��� ������ ���। � nach-ind.i �� �㬬� �ਡ������� */
summPeny = summPeny - summOpl509.
DO: 

    CREATE otch1.
    ASSIGN
        otch1.bal-summ = ?
        otch1.beg-date = iEndDate
        otch1.end-date = iEndDate
        otch1.ndays    = ?
        otch1.rat1     = ?
        otch1.summ_pr = summPeny
        otch1.rate-fixed = TRUE
        .
END.

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
    DEFINE INPUT PARAMETER iBegDatePeny AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER iKasko AS LOGICAL NO-UNDO.
    DEF VAR bFindPeny AS LOGICAL NO-UNDO.
    
    FIND FIRST peny WHERE peny.since = iBegDatePeny NO-ERROR.
    IF AVAIL peny THEN peny.kasko = iKasko. 
    ELSE DO:
        bFindPeny = FALSE.
        FOR EACH peny WHERE peny.since < iBegDatePeny NO-LOCK BY peny.since DESCENDING:
            bFindPeny = TRUE.
            penySince = peny.since.
            penyRate = peny.rate.
            penyFix = peny.fix.
            penyOstatok = peny.ostatok.
            LEAVE.
        END.
   
        IF bFindPeny THEN DO:  /* �᫨ ������� ���� ������� ���客����, �⫨��騩�� �� ���� �᫮��� */
            CREATE peny.
            ASSIGN
                peny.since = iBegDatePeny
                peny.rate = penyRate
                peny.fix = penyFix
                peny.ostatok = penyOstatok
                peny.commis = FALSE
                peny.kasko = iKasko
                nachPeny = 0
            .
        END.
    END.
END PROCEDURE.

/*
/* ᮧ���� ����� peny �� ����稨 �������� �� ��ࠬ��ࠬ */
PROCEDURE CreatePenyLoanInt:
    DEFINE INPUT PARAMETER iBegDatePeny AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER iEndDatePeny AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER in_contract AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER in_cont_code AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER in_param LIKE loan-int.id-k NO-UNDO.

    DEF VAR bFindPeny AS LOGICAL NO-UNDO.

    DEFINE VAR cr_oper AS DECIMAL NO-UNDO INIT 0.
    DEFINE VAR db_oper AS DECIMAL NO-UNDO INIT 0.    
    
    FOR EACH loan-int WHERE
           loan-int.contract  EQ in_contract
       AND loan-int.cont-code EQ in_cont_code
       AND ( loan-int.id-k      EQ in_param
            OR loan-int.id-d      EQ in_param)
       AND loan-int.mdate     >= iBegDatePeny
       AND loan-int.mdate     <= iEndDatePeny
       NO-LOCK BY loan-int.mdate:
           FIND FIRST peny WHERE peny.since = loan-int.mdate NO-ERROR.
           IF NOT AVAIL peny THEN DO:
                bFindPeny = FALSE.
                FOR EACH peny WHERE peny.since < iBegDatePeny NO-LOCK BY peny.since DESCENDING:
                    bFindPeny = TRUE.
                    penySince = peny.since.
                    penyRate = peny.rate.
                    penyFix = peny.fix.
                    penyOstatok = peny.ostatok.
                    LEAVE.
                END.
               
                Create Peny.
                ASSIGN
                    peny.since = iBegDatePeny
                    peny.rate = penyRate
                    peny.fix = penyFix
                    peny.ostatok = penyOstatok
                    peny.commis = FALSE
                    peny.kasko = iKasko
                    nachPeny = 0
                .
           END.
                  
    END.

END PROCEDURE.
*/

/* 㤠�塞 ����� � ��������� ���ﭨ�� � ��⪮� */
PROCEDURE RemoveDupl:
    DEFINE BUFFER bufDelPeny FOR peny.
    FOR EACH bufDelPeny NO-LOCK BY bufDelPeny.since:
        FOR EACH peny WHERE peny.since > bufDelPeny.since EXCLUSIVE-LOCK BY peny.since:
            IF peny.ostatok = bufDelPeny.ostatok AND peny.kasko = bufDelPeny.kasko AND peny.fix = bufDelPeny.fix THEN DO:
                DELETE peny.
            END.
            LEAVE.
        END.
    END.    
END PROCEDURE.    
    
/* ��⠥� ���� */
PROCEDURE CalcPeny:
    DEFINE INPUT PARAMETER iSummOpl AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER dEndDate AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER iSummPeny AS DECIMAL NO-UNDO.
    DEFINE VAR prevDate AS DATE NO-UNDO.
    DEFINE VAR tmpSumm AS DECIMAL NO-UNDO.
    DEFINE VAR tmpSummDb AS DECIMAL NO-UNDO.
    DEFINE VAR tmpSummCr AS DECIMAL NO-UNDO.    
    DEFINE VAR kolDays AS INT64 NO-UNDO.
    DEFINE VAR bAvailProc AS LOGICAL NO-UNDO.
    iSummPeny = 0.
    
    FIND FIRST peny WHERE peny.kasko = FALSE NO-LOCK NO-ERROR.
    IF NOT AVAIL peny THEN RETURN. /* �᫨ ��� ��� ��᪮ */

    bAvailProc = FALSE.
    /*
    FOR EACH peny WHERE peny.fix = '%' AND peny.kasko = FALSE SHARE-LOCK BY peny.since:
        /* ���� ������� ���⪨ � ࠧ���� �� ��� */
        prevDate = peny.since.
        RUN STNDRT_PARAM IN h_loan (
            iContract,       /* �����祭�� ������� */
            iContCode,      /* ����� ������� */
            {&PAR_KASK},        /* ��� ��ࠬ��� */
            prevDate,           /* ��� ���ﭨ� ������� */
            OUTPUT tmpSumm,     /* �㬬� ��ࠬ��� */
            OUTPUT tmpSummDb,   /* �㬬� ����⮢�� ����権 */
            OUTPUT tmpSummCr    /* �㬬� �।�⮢�� ����権 */
            ).
        peny.Ostatok = tmpSumm.
        bAvailProc = TRUE.
        LEAVE.
    END.
    IF bAvailProc THEN DO:
     /*   RUN CreatePenyLoanInt(INPUT prevDate, INPUT dEndDate, INPUT tmpSumm). */
        FOR EACH peny WHERE peny.ostatok = 0 AND peny.since > prevDate SHARE-LOCK BY peny.since:
            RUN STNDRT_PARAM IN h_loan (
                iContract,       /* �����祭�� ������� */
                iContCode,      /* ����� ������� */
                {&PAR_KASK},        /* ��� ��ࠬ��� */
                peny.since,         /* ��� ���ﭨ� ������� */
                OUTPUT tmpSumm,     /* �㬬� ��ࠬ��� */
                OUTPUT tmpSummDb,   /* �㬬� ����⮢�� ����権 */
                OUTPUT tmpSummCr    /* �㬬� �।�⮢�� ����権 */
                ).
            peny.Ostatok = tmpSumm.
        END.
    END.
    */
    RUN RemoveDupl.
    
   /* run instview.p(TEMP-TABLE peny:HANDLE). */
    
    DEFINE BUFFER bufPeny FOR peny.
    /*
    IF bAvailProc THEN DO: 
        FOR EACH peny WHERE peny.fix = '%' AND peny.kasko = FALSE AND peny.ostatok > 0 SHARE-LOCK BY peny.since:
            prevDate = peny.since.
            FOR EACH bufPeny WHERE bufpeny.since > prevDate NO-LOCK BY bufpeny.since DESCENDING:     
                prevDate = bufpeny.since.
                LEAVE.
            END.
            IF prevDate > peny.since THEN DO:
                tmpSumm = peny.rate * (prevDate - peny.since) / 100.
                peny.nachPeny = tmpSumm.
            END.
        END.
    END.
    */
    FOR EACH peny WHERE peny.fix = '=' AND peny.kasko = FALSE SHARE-LOCK BY peny.since: /* ��������� 200� */
        prevDate = peny.since.
        FOR EACH bufPeny WHERE bufpeny.since > prevDate /* AND bufpeny.since <= (dEndDate + 1) */ NO-LOCK BY bufpeny.since /* DESCENDING */:      
            prevDate = bufpeny.since.
            LEAVE.
        END.
        /* message "prevDate " + string(prevDate) + "peny.since " + string(peny.since) view-as alert-box. */
        IF prevDate > peny.since THEN DO:
            tmpSumm = peny.rate * (prevDate - peny.since).
            peny.nachPeny = tmpSumm.
        END.
    END.
    
    FOR EACH peny WHERE peny.fix = '1' AND peny.kasko = FALSE SHARE-LOCK BY peny.since:  /* ������६���� 3 ��� */
        /* IF iSummOpl >= peny.rate THEN LEAVE. */ /* �.�. ���� �����筮�, � � ⥮ਨ ���� 㦥 ����祭� */
        /* ��⠥� 10 ࠡ��� ���� �� �।����� ����� ����� */
        prevDate = iEndDate.
        FOR EACH bufPeny WHERE bufPeny.since > peny.since AND bufPeny.kasko = TRUE NO-LOCK BY bufPeny.since:
            prevDate = bufPeny.since.
            LEAVE.
        END.
        kolDays = NumWorkday(INPUT peny.since, INPUT prevDate).
        IF kolDays > {&NUM_DAYS_SHTRAF} THEN DO:
            peny.nachPeny = peny.rate.
            LEAVE.
        END.
    END. 
    
    FOR EACH peny NO-LOCK:
        iSummPeny = iSummPeny + peny.nachPeny.
    END.
/*    SELECT SUM(peny.nachPeny) INTO iSummPeny FROM peny. */
    
END PROCEDURE.   

/*
/* ��楤�� 㤠����� ��⮬���᪨� ����樨 */
PROCEDURE DelAutoOpers:
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.  /* �����祭�� */
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.  /* �������   */
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.  /* ��� */
   DEF INPUT PARAM iId-d      AS INT64  NO-UNDO.  /* ��ࠬ��� �� */
   DEF INPUT PARAM iId-k      AS INT64  NO-UNDO.  /* ��ࠬ��� �� */

   DEF BUFFER loan-int FOR loan-int.

   FOR EACH loan-int WHERE
            loan-int.contract  EQ iContract
      AND   loan-int.cont-code EQ iContCode
      AND   loan-int.id-d      EQ iId-d
      AND   loan-int.id-k      EQ iId-k
      AND   loan-int.mdate     GE iDate
      AND   loan-int.avt       EQ TRUE          /* ���쪮 ��⮬���᪨� */
   EXCLUSIVE-LOCK:
      DELETE loan-int.
   END.
END PROCEDURE.    /* DelAutoOpers */

/* ��楤�� ᮧ����� ��⮬���᪨� ����樨 */
PROCEDURE CreateAutoOpers:
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.  /* �����祭�� */
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.  /* �������   */
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.  /* ��� */
   DEF INPUT PARAM iAmount    AS DEC  NO-UNDO.  /* �㬬� */
   DEF INPUT PARAM iId-d      AS INT64  NO-UNDO.  /* ��ࠬ��� �� */
   DEF INPUT PARAM iId-k      AS INT64  NO-UNDO.  /* ��ࠬ��� �� */

   DEF BUFFER b-loan-int FOR loan-int.

   FIND FIRST b-loan-int WHERE
              b-loan-int.contract  EQ iContract
      AND     b-loan-int.cont-code EQ iContCode
      AND     b-loan-int.id-d      EQ iId-d
      AND     b-loan-int.id-k      EQ iId-k
      AND     b-loan-int.mdate     EQ iDate
   NO-LOCK NO-ERROR.
   IF NOT AVAIL b-loan-int THEN
   DO:
      RUN Cr_LoanInt IN h_lv (iContract,    /* �����䨪���   */
                              iContCode,    /* �������        */
                              iDate,        /* ��� ����樨   */
                              iAmount,      /* �㬬� ����樨  */
                              iId-d,        /* �. ���᫥��    */
                              iId-k,        /* �. ���ᠭ�      */
                              YES,          /* ���. ������   */
                              ?).           /* handle �஢���� */
   END.
END PROCEDURE.  /* CreateAutoOpers */
*/













