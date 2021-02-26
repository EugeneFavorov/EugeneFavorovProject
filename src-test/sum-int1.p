/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: sum-int1.p
      Comment: ������ �������� ���⥦�� %%
   Parameters: loan.contract,loan.cont-code
 ------------------------------------------------------------------------------
         Uses:
      Used by:
 ------------------------------------------------------------------------------
      Created: 
     Modified: nata 26/06/01 ��� ���������� ����� ��業�� �� ����� ��
                             ��� ������祭 ���� �१  ��⮤
     Modified: ���� 08/10/2003 ������� ��।���� �� �맮� �����㬥�⮢
*/

/*&GLOB PRINT_LOG YES*/
{globals.i}
{svarloan.def}
{intrface.get loan}
{intrface.get db2l} 
{intrface.get rights}  /* ����㧪� �����㬥���� �� �ࠢ�� */
{intrface.get xclass}
{intrface.get date}
{intrface.get pogcr}

DEF INPUT PARAM incontr LIKE loan.contract.
DEF INPUT PARAM intcode LIKE loan.cont-code.

DEF VAR vCommList      AS CHAR NO-UNDO.
DEF VAR vMainComm      AS CHAR NO-UNDO.
DEF VAR mDateM         AS DATE NO-UNDO.
DEF VAR mFlRecalc      AS CHAR NO-UNDO.
DEF VAR mSince         AS DATE NO-UNDO.
DEF VAR ml-int-date    AS DATE NO-UNDO.
DEF VAR mErrSaveHist   AS LOGICAL NO-UNDO. /* �ਧ��� �訡�� �� ��࠭���� �᫮���*/

/* =============================================================
   �᫨ �� �������� �� �।�ᬮ�७� ���᫥��� �᭮���� ��業⮢,
   � ���㠫����� ��䨪� ⠪ �� �� �ॡ����. */

FIND FIRST loan WHERE
           loan.contract  EQ inContr
       AND loan.cont-code EQ intCode
NO-LOCK NO-ERROR.

IF NOT AVAIL loan THEN DO:
   {intrface.del}
   RETURN.
END.

vCommList = GetXattrInit(loan.Class-code, "���������").

vMainComm = IF inContr = "�।��" THEN "%�।" ELSE "%���".

IF vCommList <> ? AND NOT CAN-DO(vCommList, vMainComm) THEN DO:
   {intrface.del}
   RETURN.
END.

/* ============================================================= */



&GLOB TermOblCheck term-obl.dsc-beg-date >= loan.since

&GLOB TermOblCheck1 (term-obl.dsc-beg-date - scr) <= loan.since

DEF VAR mTypeCond      AS LOG  NO-UNDO.    /* ��� �奬� ���⥦� */
DEF VAR mSurr          AS CHAR NO-UNDO.    /* ���ண�� loan-cond*/
DEF VAR mTimeVariable  AS INT64  NO-UNDO.
DEF VAR mTimeVariable1 AS INT64  NO-UNDO.

DEF VAR i      AS INT64  NO-UNDO.
DEF VAR dat-c  AS DATE NO-UNDO.
DEF VAR xd     AS DATE NO-UNDO.
DEF VAR dat-tt AS DATE NO-UNDO.

DEF VAR proc-name  AS CHAR NO-UNDO.

DEF VAR mRecalcLoan  AS LOG NO-UNDO.
DEF VAR mRecalcPP    AS LOG NO-UNDO.
DEF VAR vDateP       AS DATE NO-UNDO .
DEF VAR vDateDif     AS DATE NO-UNDO .
DEF new global shared TEMP-TABLE filost NO-UNDO
      FIELD type    AS INT64
      FIELD balance LIKE loan-var.balance
      FIELD since   LIKE term-obl.end-date
INDEX since IS UNIQUE type since ASCENDING
INDEX s since .


DEF VAR mLoanWork AS LOG NO-UNDO.

DEF BUFFER yloan      FOR loan .
DEF BUFFER xsetting   FOR setting.
DEF BUFFER xterm-obl  FOR term-obl.
DEF BUFFER xcomm-rate FOR comm-rate.
DEF BUFFER yloan-cond FOR loan-cond.


def VAR  t AS INT64 NO-UNDO.
def VAR  t1 AS INT64 NO-UNDO.
def VAR  t2 AS INT64 NO-UNDO.
def VAR  t3 AS INT64 NO-UNDO.
def VAR  t4 AS INT64 NO-UNDO.
def VAR  t5 AS INT64 NO-UNDO.
def VAR  t6 AS INT64 NO-UNDO.

t = ETIME.

{w-ost.i}

MAIN:
DO TRANSACTION
   ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

    FIND CURRENT loan
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR .

    IF NOT AVAIL loan THEN
    do:
       FIND FIRST loan WHERE
                  loan.contract  EQ inContr
              AND loan.cont-code EQ intCode
       NO-LOCK NO-ERROR.
       IF AVAIL loan THEN 
           RUN wholocks2.p ( RECID(loan),"loan" ,program-name(1) + '~n �訡�� �� �������쭮� ����㯥 � �������� ' + intcode).
       ELSE 
           message program-name(1) skip '�訡�� �� �������쭮� ����㯥 � �������� ' intcode view-as alert-box.
    
       LEAVE MAIN.
    END.

    /* ��࠭�� ���� �।��饣� ���ﭨ� ��� ���쭥�襣� ����⠭�������  */
    ASSIGN
       mSince      = loan.since
       ml-int-date = loan.l-int-date.

   /*mitr:spped 11/8/2005 --------------------------------------------------
   �ࠢ� ����㯠 �� �஢�����.
   ����� : �஢�ઠ �ࠢ �� ������ 
              �� ���� �������� ��᫠ � �窨 �७�� ������
   IF NOT CAN-DO (GetRightClasses (loan.class-code, "W"), loan.class-code)
   THEN RETURN.
   ------------------------------------------------------------------------*/
    
    
    RUN RE_L_COND IN h_Loan (loan.contract,
                             loan.cont-code,
                             loan.since,
                             BUFFER loan-cond).
    
    IF NOT AVAIL loan-cond THEN 
    DO:
       LEAVE MAIN.
    END.

   /* ������ ���� �������� ���⥦��  �᫨ ������ ���⥦, � ������� ࠡ�稩*/
    mLoanWork = NO.
    
    FIND LAST term-obl {wh-t &f=term-obl &c="/*"}
         AND  term-obl.end-date < loan.since
         AND  term-obl.sop-date = ?
         AND (term-obl.idnt     = 1
           OR term-obl.idnt     = 3)
       NO-LOCK NO-ERROR.
    
    IF AVAILABLE term-obl AND
       {&TermOblCheck}    THEN
    DO:
       mLoanWork = YES.
       /*loan.loan-work = YES.*/
    
       IF INDEX(loan.cont-code," ") NE 0 THEN
       DO:
          FIND FIRST yloan WHERE
                     yloan.contract  = loan.contract
                 AND yloan.cont-code = TRIM(SUBSTRING(loan.cont-code,
                                                      1,
                                                      INDEX(loan.cont-code," ") - 1))
          EXCLUSIVE-LOCK NO-WAIT NO-ERROR .
    
          IF NOT AVAIL yloan THEN
          DO:
             FIND FIRST yloan WHERE
                        yloan.contract  = loan.contract
                    AND yloan.cont-code = TRIM(SUBSTRING(loan.cont-code,
                                               1,
                                               INDEX(loan.cont-code," ") - 1))
             NO-LOCK NO-ERROR.
             IF AVAIL yloan THEN
                RUN wholocks2.p ( RECID(loan),"loan" ,
                                  program-name(1) + 
                                  '~n �訡�� �� �������쭮� ����㯥 � �������� ' + 
                                  ENTRY(1,loan.cont-code," ")).
             ELSE
                message program-name(1) skip 
                        '�訡�� �� �������쭮� ����㯥 � �������� ' 
                        ENTRY(1,loan.cont-code," ") VIEW-AS ALERT-BOX.
             UNDO MAIN, LEAVE MAIN.
          END.
          ELSE
             yloan.loan-work = mLoanWork /*loan.loan-work*/ .
       END.
    END.
    ELSE
    DO:
       FIND FIRST term-obl {wh-t &f=term-obl &c="/*"}
             AND  term-obl.end-date >= loan.since
             AND  term-obl.sop-date  = ?
             AND (term-obl.idnt      = 1
               OR term-obl.idnt      = 3)
       NO-LOCK NO-ERROR.    
       IF AVAILABLE term-obl  AND
          {&TermOblCheck1} THEN
       DO:
          mLoanWork = YES.
          /*
         loan.loan-work = YES.
         */
          IF INDEX(loan.cont-code," ") NE 0 THEN
          DO:
             FIND FIRST yloan WHERE
                        yloan.contract  = loan.contract
                    AND yloan.cont-code EQ TRIM(SUBSTRING(loan.cont-code,
                                                          1,
                                                          INDEX(loan.cont-code," ") - 1))
             EXCLUSIVE-LOCK NO-WAIT NO-ERROR .
             IF NOT AVAIL yloan THEN
             DO:
                FIND FIRST yloan WHERE
                           yloan.contract  = loan.contract
                       AND yloan.cont-code = TRIM(SUBSTRING(loan.cont-code,
                                                  1,
                                                  INDEX(loan.cont-code," ") - 1))
                NO-LOCK NO-ERROR.
                IF AVAIL yloan THEN
                   RUN wholocks2.p ( RECID(loan),"loan" ,
                                     program-name(1) + 
                                     '~n �訡�� �� �������쭮� ����㯥 � �������� ' + 
                                     ENTRY(1,loan.cont-code," ")).
                ELSE
                   message program-name(1) skip 
                           '�訡�� �� �������쭮� ����㯥 � �������� ' 
                           ENTRY(1,loan.cont-code," ") VIEW-AS ALERT-BOX.
                UNDO MAIN, LEAVE MAIN.
             END.
             ELSE
                yloan.loan-work = mLoanWork /*loan.loan-work*/ .
          END.    
       END.
    END.

    loan.loan-work = mLoanWork.
    
   /* ������ �������� ���⥦�� � ��⮩ > ���� ������ ���ﭨ� ������� */
    IF FGetSetting("������",?,?) = "���" THEN 
    DO:
       LEAVE MAIN.
    END.
    
    ASSIGN
       mSurr = loan-cond.contract + "," + 
               loan-cond.cont-code + "," + 
               string(loan-cond.since)
       
   /* �饬 ��ࢮ� �᫮��� � �����⭮� �奬�� */
       mTypeCond = GetXAttrValueEx("loan-cond",mSurr,"�奬�����","?") EQ "�����⭠�"
       .
    IF NOT  mTypeCond THEN DO :
       FOR EACH signs WHERE signs.file-name EQ 'loan-cond' 
            AND signs.code                  EQ "�奬�����" 
            AND signs.code-val              EQ "�����⭠�" 
            AND signs.surrogate BEGINS loan-cond.contract + "," +
                                       loan-cond.cont-code  + "," NO-LOCK: 
           vDateP = DATE(ENTRY(3,signs.surrogate)).
           LEAVE.
       END.
    END.
   
    /*
    � ⥯��� ���� ���� ���� ��  ����. �᫮��� ��᫥ ���
    */
    IF mTypeCond OR vDateP NE ? THEN
       FOR EACH yloan-cond WHERE yloan-cond.contract EQ loan-cond.contract
                             AND yloan-cond.cont-code EQ loan-cond.cont-code
                             AND yloan-cond.since GT (IF vDateP NE ? THEN vDateP ELSE loan-cond.since)
       NO-LOCK :
          mSurr = yloan-cond.contract + "," + 
                  yloan-cond.cont-code + "," + 
                  string(yloan-cond.since).
          IF GetXAttrValueEx("loan-cond",mSurr,"�奬�����","?") NE "�����⭠�" THEN
          DO:
             vDateDif = yloan-cond.since.
             LEAVE.
          END.
       END.
       
    EMPTY TEMP-TABLE filost.
    EMPTY TEMP-TABLE commrate.
    
    {get_meth.i  'NachProc' 'nach-pp'}

    IF  proc-name = "nach-pp" THEN
    DO:
       IF NOT mTypeCond THEN
       DO:
          i = 1.
          RUN PrepareFilOstSimple(loan.contract,
                                  loan.cont-code,
                                  loan.open-date,
                                  loan.end-date,
                                  loan.since,
                                  1).
    
          RUN SetSysConf in h_base ("�����멃�䨪���⪮�","�����멃�䨪���⪮�").
       END.
       ELSE
          RUN SetSysConf in h_base ("�����⏏������","�����⏏������").
    
       RUN SetSysConf IN h_base ("�����멃�䨪�����ᨩ","�����멃�䨪�����ᨩ").
    
       &IF DEFINED(PRINT_LOG) &THEN
         OUTPUT TO "mycount.log" APPEND.
            PUT UNFORMATTED " --- " STRING(TIME - mTimeVariable1,"hh:mm:ss") SKIP.
         OUTPUT CLOSE.
       &ENDIF
    
       lr-st = IF loan.contract = '�����'
               THEN ({&lrate-dim} / 2) + 1
               ELSE  1.
    
       { commrat1.i
          &dat-t = loan.end-date
          &cd-p  = pick-rate[1]
          &lr    = lr-st
          &date1 = loan.open-date
      }
       RELEASE filost.
       RELEASE commrate.
    
    END.
    ELSE IF proc-name BEGINS "lnsch" AND (NOT mTypeCond OR vDateDif NE ?)
    THEN
    DO:
    
       RUN PrepareFilOstSimple(loan.contract,
                               loan.cont-code,
                               loan.open-date,
                               loan.end-date,
                               loan.since,
                               1).
    
       RUN SetSysConf IN h_base ("�����⏏","��").
    END.

IF vDateP = ? THEN vDateP = loan.end-date.

    IF NOT mTypeCond THEN 
    DO:
       FIND LAST comm-rate WHERE
                 comm-rate.commission EQ "%�����"
             AND comm-rate.kau        EQ ENTRY(1, loan-cond.contract + "," + 
                                                  loan-cond.cont-code, " ")
             AND comm-rate.since      LE loan-cond.since
       NO-LOCK NO-ERROR.   
      
       IF AVAIL comm-rate THEN
       DO:
          FIND FIRST term-obl WHERE
                     term-obl.contract  EQ loan.contract
                 AND term-obl.cont-code EQ loan.cont-code
                 AND term-obl.idnt      EQ 2
          NO-LOCK NO-ERROR.
          
          IF AVAIL term-obl THEN
          DO:
             RUN CALC_PLAN_POG IN h_pogcr (loan.open-date,
                                           loan.end-date,
                                           term-obl.amt,
                                           ?,
                                           BUFFER loan,
                                           BUFFER loan-cond).
          END.
       END.   
      
       FOR EACH term-obl OF loan  WHERE
                term-obl.end-date GE loan.since
            AND term-obl.end-date LE vDateP 
            AND term-obl.idnt     EQ 1
       NO-LOCK :
          RUN proc16.p(loan.contract,
                       loan.cont-code,
                       term-obl.end-date,
                       RECID(term-obl),
                       1).
       END.
    END.
    IF vDateDif NE ? THEN 
    DO : /*�뫠 ᬥ�� ���. -> ����.*/
       FOR EACH term-obl OF loan  WHERE
                   term-obl.end-date GE loan.since
               AND term-obl.end-date GE vDateDif
               AND term-obl.idnt     EQ 1
          NO-LOCK :
          RUN proc16.p(loan.contract,
                       loan.cont-code,
                       term-obl.end-date,
                       RECID(term-obl),
                       1).
       END.
    END.
    IF   (mTypeCond 
      OR  vDateP   LT loan.end-date) 
      AND vDateDif EQ ? THEN
    DO:
       IF    loan.since GT vDateP 
          OR mTypeCond THEN     
          vDateP = loan.since .
       FIND  LAST loan-cond WHERE loan-cond.contract   EQ loan.contract
                              AND loan-cond.cont-code  EQ loan.cont-code
                              AND loan-cond.since      LE vDateP
       NO-LOCK NO-ERROR.
       IF AVAIL loan-cond THEN 
       DO:
          /* ��� ������ ��������㥬, �㦭� �� ������뢠�� ������� ��䨪� */
         /* ����砥� ���� ����樨 */
         mDateM = DATE(FGetSettingMF("����⠑���",?,?,loan.filial-id, NO)).
         /* �᫨ ������� �����஢����, �஢��塞 �� */
         IF mDateM GE loan.open-date THEN
         DO:
            mFlRecalc = FGetSettingMF("����猨����",?,?,loan.filial-id, no).
            /* �᫨ �� ������뢠��, � �஢��塞, ���� �� �᫮��� ��᫥ ���� ����� */
            IF mFlRecalc EQ "���" THEN
            DO:
               FIND FIRST yloan-cond WHERE yloan-cond.contract  EQ loan.contract
                                       AND yloan-cond.cont-code EQ loan.cont-code
                                       AND yloan-cond.since     GT mDateM
               NO-LOCK NO-ERROR.               
               /* �᫨ �᫮��� ���, �� ������뢠�� */
               IF NOT AVAIL yloan-cond THEN
               DO:
                  LEAVE MAIN.
               END.
               /* �᫨ �᫮��� ����, ���������㥬, � ���� ���� ������뢠��
               �����塞 ���� ��� ������ ��䨪�� */
               ELSE IF NOT (yloan-cond.since GT loan.since
                        AND yloan-cond.since GT loan.l-int-date) THEN
               DO:
                  IF     loan.l-int-date LT yloan-cond.since 
                     AND loan.l-int-date LT loan.since THEN
                     loan.l-int-date = yloan-cond.since.
                  ELSE                  
                     loan.since = yloan-cond.since.
               END.
            END.
         END.
         RUN SetSysConf IN h_base ("�����⏏","��").
         RUN anps.p(vDateP,1, BUFFER loan, BUFFER loan-cond).
          
         RUN SaveGraphToHistory IN h_loan (loan.Contract,
                                           loan.Cont-Code,
                                           loan-cond.since,
                                           "������",
                                           "������� ������� " + loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since),
                                           NO,
                                           ?,
                                           NO,
                                           OUTPUT mErrSaveHist).
         IF mErrSaveHist THEN
             MESSAGE program-name(1) SKIP '�訡�� ��࠭���� ���ਨ' intcode VIEW-AS ALERT-BOX.
       END.
    END.

FINALLY:
   IF AVAIL loan THEN
      ASSIGN
         loan.since       = mSince
         loan.l-int-date  = ml-int-date.

   EMPTY TEMP-TABLE filost.
   EMPTY TEMP-TABLE commrate.
   
   RUN DeleteOldDataProtocol IN h_base("�����멃�䨪���⪮�").
   RUN DeleteOldDataProtocol IN h_base("�����멃�䨪�����ᨩ").
   RUN DeleteOldDataProtocol IN h_base("��⠎��⪠").
   RUN DeleteOldDataProtocol IN h_base("�㬬����⪠").
   RUN DeleteOldDataProtocol IN h_base("�����⏏������").
   RUN DeleteOldDataProtocol IN h_base("��ࢠ��������").
   RUN DeleteOldDataProtocol IN h_base("�����⏏").

   {intrface.del}
END FINALLY.
END. /* do transaction */
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='11/12/2014 17:18:11.753+04:00' */
/* $LINTFILE='sum-int1.p' */
/*prosigngWl0wYTl0/J2bLOxwfVhfw*/