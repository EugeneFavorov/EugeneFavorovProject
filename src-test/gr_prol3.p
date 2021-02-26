/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: GR_PROL3.P
      Comment: ������ gr_prol ᮧ���騩 �����஢����� ���㬥���.
               � �⫨稥 �� gr_prol1 ᮧ���� ���㬥��� � ������� �㬬�
               �� ���� �ࠧ� �� �믮������, � �� ��᫥ ��ࠡ�⪨ 
               (��� � gr_prol).
   Parameters: in-op-date, oprid
         Uses:
      Used by:
      Created: 09.03.2010 15:18 feak    
     Modified: 09.03.2010 15:18 feak    
*/

DEF INPUT PARAM in-op-date LIKE op.op-date NO-UNDO.
DEF INPUT PARAM oprid      AS   RECID      NO-UNDO.
&IF DEFINED(auto) &THEN
DEF INPUT PARAM in-contract-date LIKE op.op-date NO-UNDO.
&ENDIF           

&SCOPED-DEFINE Nachkin-tt Nchk-tt
&SCOPED-DEFINE DoContract YES
&SCOPED-DEFINE BYrole     YES
&SCOPED-DEFINE Ofsrch     YES
           
{globals.i}
{crdps.def}
{g-defs.i}
{def-wf.i new}

DEF VAR templ-acct         AS CHAR                    NO-UNDO.
DEF VAR lst-tmpl-op        AS CHARACTER INITIAL ""    NO-UNDO.
DEF VAR templ-trans        AS CHAR                    NO-UNDO.
DEF VAR c-type             AS CHARACTER INITIAL "*"   NO-UNDO.
DEF VAR t-type             AS CHARACTER               NO-UNDO.
DEF VAR mFl-w              AS LOGICAL                 NO-UNDO. /* ���� ࠡ��� � ��㧥஬ ������஢, ������� ��� �������� �।�� */
DEF VAR cred               AS DATE                    NO-UNDO.
DEF VAR vTmpOutput         AS LOGICAL                 NO-UNDO.
DEF VAR mIi                AS INTEGER                 NO-UNDO.
DEF VAR mJ                 AS INTEGER                 NO-UNDO.
DEF VAR mFirstOpTempl      AS LOGICAL   INITIAL YES   NO-UNDO.
DEF VAR curr1              LIKE currency.currency     NO-UNDO. /* ����� ����樨, �� 1 蠡���� ���㬥�⮢ */
DEF VAR op-tmpl            AS CHARACTER               NO-UNDO.                        
DEF VAR tt-num             AS INTEGER                        .
DEF VAR str-num            AS CHAR                    NO-UNDO.
DEF VAR sts                AS CHARACTER               NO-UNDO. /* C���� ��� ������� */
DEF VAR sts_close          AS CHARACTER               NO-UNDO. /* C����, �� ���ண� ࠧ�襭� �஫������ */
DEF VAR cred-t             AS DATE                    NO-UNDO.
DEF VAR in-date            AS DATE                    NO-UNDO.
DEF VAR hnd-proc           AS HANDLE                  NO-UNDO.
DEF VAR loan_h             AS HANDLE                  NO-UNDO.
DEF VAR nopars             AS LOGICAL                 NO-UNDO.
DEF VAR nameproc           AS CHARACTER               NO-UNDO.
DEF VAR params             AS CHARACTER               NO-UNDO.
DEF VAR fl-o               AS INTEGER                 NO-UNDO.
DEF VAR fler               AS LOGICAL                 NO-UNDO.
DEF VAR t-details          AS CHAR                    NO-UNDO.
DEF VAR cod-ost            AS CHAR                    NO-UNDO.
DEF VAR mAcct              AS CHAR                    NO-UNDO.
DEF VAR old-op-kind        AS CHARACTER               NO-UNDO.
DEF VAR in-op-kind         LIKE op-kind.op-kind       NO-UNDO.
DEF VAR change-loan-cond   AS LOGICAL INITIAL YES     NO-UNDO.
DEF VAR flag-transfer      AS LOGICAL                 NO-UNDO.
DEF VAR rid1               AS RECID   EXTENT 30       NO-UNDO.
DEF VAR kau-rid            AS RECID   EXTENT 2        NO-UNDO.
DEF VAR ii                 AS INTEGER                 NO-UNDO.
DEF VAR result             AS INTEGER                 NO-UNDO.
DEF VAR in-acct-type       LIKE loan-acct.acct-type   NO-UNDO.
DEF VAR ret-str            AS CHARACTER               NO-UNDO.
DEF VAR ridd               AS RECID                   NO-UNDO.
DEF VAR tmp-recid          AS CHAR                    NO-UNDO.
DEF VAR mMultiRun          AS CHARACTER               NO-UNDO. /* ������� �஫������ ���⨢������ ������� */
DEF VAR k                  AS INTEGER                 NO-UNDO.
DEF VAR main-first         AS LOGICAL                 NO-UNDO.
DEF VAR dval               AS DATE                    NO-UNDO.
DEF VAR vSave%             AS LOGICAL                 NO-UNDO. /*���࠭��� �� ����� � ���᫥���� %%*/
DEF VAR vSilentMode        AS CHARACTER               NO-UNDO. /* �ਧ��� ����᪠ �࠭���樨 � ����⭮� ०���
                                                               ** �᫨ �� ����, � �࠭�� � ᥡ� ���-�� ����, 
                                                               ** ����� ����室��� �ਡ����� � �������� ��� */
DEF VAR mHPQuery           AS HANDLE                  NO-UNDO.
DEF VAR mRowId             AS ROWID                   NO-UNDO.
DEF VAR mLoanList          AS CHARACTER               NO-UNDO. /* ���᮪ ����஢ �஫����樨 � TRANSACTION */
DEF VAR mCii               AS INT64                   NO-UNDO.
DEF VAR mErr               AS INT64                   NO-UNDO.
DEF NEW GLOBAL SHARED STREAM err.
DEF STREAM _prolong_.
DEF STREAM err-ved.
DEF STREAM err-mess.

DEF BUFFER prl-op-kind     FOR op-kind.
DEF BUFFER xxop-entry      FOR op-entry.   /* �।���� �஢���� �� ���㬥��� */
DEF BUFFER xwop            FOR wop.        /* �ᯮ������ � ����� ����� */
DEF BUFFER xkau-entry      FOR kau-entry.
DEF BUFFER cr-acct         FOR acct.       /* ��� ���뢠��� �� �஫����樨 */
DEF BUFFER sroch-loan-acct FOR loan-acct.
DEF BUFFER  m-loan         FOR loan.
DEF BUFFER  m2-loan        FOR loan.

/* MESSAGE auto 
 VIEW-AS ALERT-BOX.
*/
{dps-a-cl.tmp "NEW SHARED "}
{g-currv1.i &OFbase="/*"}
{details.def}
{dpsproc.def}
{prn-ved.def
    &new-nach  = new-nach
    &tt        = {&Nachkin-tt}
}

{intrface.get tmess}
{intrface.get dps}
{intrface.get debug}
{intrface.get xclass}
{f_for_t.i}

{prn_ved_nach.i
   &Nachkin-tt    = {&Nachkin-tt}
   &no-ved_op     = YES   
   &ved_prolong   = YES
}
{chk_use.i} /* ���� �ࠢ ��� ���짮��⥫�. */
{dps-logs.i
   &DpsLogsQuoter = "'"}
{invest.num}
{ksh-defs.i new}
{prol.i}
{logg.i """��ୠ� �訡�� �� �஫����樨"""}
{cr-nach.i 
   no-svget="/*"}
{currency.def}
/*��� �����, �⮡� ����᪠� "������� �奬� ���᫥���" */
RUN SetSysConf IN h_base ("TypeNach", "����ࢨ஢����").
{prn-ved.i
   &EmptyTempTable     = "��頥� ⠡����"
   &DefPrintProcedures = "���塞 ��楤��� ����"
   &Stream             = " STREAM err"
   &FileName           = _spool4.tmp
   &PutPlanDate        = cred
}

FUNCTION Set_type   RETURNS LOGICAL (INPUT l-type  AS CHARACTER) IN loan_h.
FUNCTION Set_ost    RETURNS LOGICAL (INPUT cod-ost AS CHARACTER) IN loan_h.
FUNCTION Set_period RETURNS LOGICAL (INPUT dat1    AS DATE,
                                     INPUT dat2    AS DATE)      IN loan_h.
RUN "l-type.p" PERSISTENT SET loan_h.

FORM
   op-entry.op-date
   loan.cont-code
   loan.currency
   WITH FRAME info-fr.

/* �뢮� ��⮪��� �஫����஢����� ������஢ */
&IF DEFINED(auto) &THEN
   {setdest2.i
      &stream   = "stream _prolong_"
      &filename = _spool1.tmp
      &cols     = 120
      &OPTION   = APPEND
    }
&ELSE
   {setdest2.i
      &stream   = "stream _prolong_"
      &filename = _spool1.tmp
      &cols     = 120
   }
&ENDIF

RUN "l-trans.p" PERSISTENT SET hnd-proc (?,
                                         ?,
                                         ?,
                                         "�����").

/* �஢�ઠ ������ �࠭���樨 � 蠡����� � ��� */
{chktempl.i}

/* ����� ��⥬��� ᮮ�饭�� ��� �࠭���樨 */
RUN Init-SysMes IN h_tmess(op-kind.op-kind, "", "").

/* ���樠������ ᯨ᪠ 蠡����� */
ASSIGN
   templ-acct  = list-op-templ(op-kind.op-kind,"acct")
   lst-tmpl-op = list-op-templ(op-kind.op-kind,"op")
   templ-trans = list-op-templ(op-kind.op-kind,"loan-transaction")
   mFl-w       = GetSysConf("PlacementCLSW_RSHB") EQ "��". /* ࠡ�⠥� � ��㧥஬ ? */
   .

RUN GarbageCollect   IN h_base.
RUN NoGarbageCollect IN h_base.

/* ��ଠ ����� ������ � ��� � ���஡������ ���㬥�⮢ */
{g-frame3.i
   &op      = t-op
   &DoTable = yes
}
{g-frame3.i
   &DoFrame = Yes
   &row     = 10
   &op      = t-op
}
IF mFl-w THEN
   cred = in-op-date.

/* ����㧪� ����� �� */
{plibinit.i}

ASSIGN
   DebugParser = INTEGER(GetXattrValue("op-kind",
                                       op-kind.op-kind,
                                       "DebugParser"))
   vSilentMode = GetXattrValueEx("op-kind",
                                 op-kind.op-kind,
                                 "silent-mode",
                                 "")   
   .
mMultiRun = fGetSetting ("�஫���₪�", ?, ?).


MAIN:
DO ON ERROR    UNDO MAIN, LEAVE MAIN
   ON ENDKEY   UNDO MAIN, LEAVE MAIN:

   /* �����⮢�� 蠡����� ���㬥�⮢ */
   DO mIi = 1 TO NUM-ENTRIES(lst-tmpl-op):
      FIND FIRST op-template OF op-kind 
                 WHERE op-template.op-template EQ INT(ENTRY(mIi, lst-tmpl-op))
         NO-LOCK NO-ERROR.
      IF mFirstOpTempl THEN
         curr1 = op-templ.currency.   

      /* �஢�ઠ ���������� ���᫥��� �� ������ ��ࠧ�� 
      ** �஢������ ⮫쪮 �᫨ ࠭�� �� �� ������ ⠪�� 蠡���, �.�. �� ��
      ** ����祭 YES */
      IF NOT vTmpOutput THEN      
         RUN Get-Old-Ref IN h_dpspc (RECID(op-template),
                                     OUTPUT vTmpOutput).

      CREATE t-op.
      ASSIGN
         t-op.op             = op-template.op-templ
         t-op.doc-type       = op-template.doc-type
         t-op.op-date        = in-op-date
         &IF DEFINED(auto) = 0 &THEN
         t-op.contract-date  = in-op-date
         &ELSE
         t-op.contract-date  = in-contract-date
         &ENDIF
         t-op.doc-date       = in-op-date
         t-op.details        = op-template.details
         .

      IF NOT mFirstOpTempl THEN
         t-op.contract-date = cred.

      {g-frame3.i
         &op       = t-op
         &DoBefore = Yes
      }   

/*MESSAGE AUTO ","
VIEW-AS ALERT-BOX.*/
      &IF DEFINED(auto) = 0 &THEN
      IF vSilentMode EQ "" THEN
      DO:
         {g-frame3.i
            &op       = t-op
            &DoDisp   = YES      
         }
         {g-frame3.i
            &op    = t-op
            &DoSet = Yes
         }
      END.
      ELSE
         t-op.contract-date = in-op-date + INT(vSilentMode).
        /* MESSAGE t-op.contract-date ","
         VIEW-AS ALERT-BOX.*/   
      &ENDIF

/*MESSAGE t-op.contract-date ","
VIEW-AS ALERT-BOX.*/
      IF mFirstOpTempl THEN
         cred = t-op.contract-date.

      IF     mFirstOpTempl  
         AND {assigned t-op.doc-num} THEN
      DO:
         tt-num  = INTEGER(t-op.doc-num) NO-ERROR.
         str-num = IF ERROR-STATUS:ERROR THEN t-op.doc-num
                                         ELSE "".
      END.

      mFirstOpTempl = NO.
   END.

   /* ��室 �� "Esc" */
   IF  KEYFUNCTION(LASTKEY) EQ "end-error" THEN   
      HIDE FRAME opreq NO-PAUSE.   
   ELSE
   DO:
      &IF DEFINED(auto) &THEN
         {for_ved.i
            &filename = _spool4.tmp
            &OPTION   = APPEND}
      &ELSE
         {for_ved.i
            &filename = _spool4.tmp}
      &ENDIF      
      
      /* ��।������ ������ ����権 */
      FIND FIRST currency WHERE currency.currency EQ curr1 
         NO-LOCK NO-ERROR.         
      curr1 = IF AVAILABLE currency THEN currency.currency
                                    ELSE IF op-templ.currency NE "" THEN ?
                                                                    ELSE curr1.

      /* ��।������ ����ᮢ ��� ������� */
      op-tmpl  = STRING(GET_OP-TEMPL(op-kind.op-kind,
                                     "loan",
                                     "")).
      FIND FIRST op-template OF op-kind 
                             WHERE op-template.op-template EQ INTEGER(op-tmpl) 
         NO-LOCK NO-ERROR.
      sts_close = Get_Param('loan-status-cl', RECID(op-template)).   
      sts       = IF sts_close EQ ? THEN CHR(251)
                                    ELSE sts_close.

   
      RUN SetSysConf IN h_base("op-contract-date", STRING(cred)).

      /* ��।������ ⨯�� ������� ��� ��ࠡ�⪨ */
      c-type = GetXAttrValueEx("op-kind",
                               op-kind.op-kind,
                               "cont-type",
                               "*").      
      FOR EACH code WHERE code.class   EQ "cont-type"
                      AND code.parent  EQ "cont-type" 
         NO-LOCK:

         IF CAN-DO(c-type, code.code) THEN 
         DO:
            {additem.i t-type code.code}
         END.
      END.

      /* �롮� ������஢, �� ⨯� + �, �� ����� �� ����� �ࠢ� */
      IF mFl-w THEN /* �롮� �������, �᫨ ࠡ�⠥� � ��㧥஬ */
         ASSIGN
            in-cont-code   = GetSysConf("PlacementCLSW_RSHB_cont-code")
            c-type         = "*"
            .

      /* �����⮢�� ����� ������ ��� ��࠭���� %% */
      vSave% = (FGetSetting('�������',?,?) = "��").
      IF vSave%  THEN DO:
         RUN prep_cr_data (in-op-date,
                           GetThisUserOtdel(),
                           mDataClass-ID). 
      END.

      RUN qrybrwld.p ("dep_person",THIS-PROCEDURE:HANDLE,OUTPUT mHPQuery) NO-ERROR.
      /* � ��६����� mHBrwQuery (bstty.def) �������� ��� ����㦥���� DS-���������� */
      IF NOT VALID-HANDLE (mHPQuery) THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "�訡�� ����㧪� ��楤��� �ନ஢���� ����� ��� ����� ~"dep_person~"").
         UNDO MAIN, LEAVE MAIN.
      END.

      DO mIi = 1 TO NUM-ENTRIES(t-type):

         RUN SetDSContext IN mHPQuery ("icontr~001close-date1~001close-date2~001cont-type" + (IF curr1 NE ? THEN "~001currency"
                                                                                                            ELSE ""),
                                       "dps~001?~001?~001" + ENTRY(mIi, t-type) + (IF curr1 NE ? THEN ("~001" + curr1)
                                                                                                 ELSE ""),
                                       "loan.Add2Where",
                                       "    loan.loan-status GE '�' 
                                        AND loan.loan-status NE '���'") NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            UNDO MAIN, LEAVE MAIN.

         /* ����⨥ ����� */
         RUN Open-Query IN mHPQuery.
         /* ��ࠡ�⪠ �訡�� */
         IF ERROR-STATUS:ERROR THEN 
            UNDO MAIN, LEAVE MAIN.

         LOAN_CODE:
         DO WHILE TRUE
          TRANSACTION 
          ON ERROR  UNDO loan_code, NEXT  loan_code
          ON ENDKEY UNDO loan_code, LEAVE loan_code:
          IF RETRY THEN .

          mLoanList = "".
          mCii = 0.  
          RUN Get_Query_Record IN mHPQuery ("NEXT",
                                            "loan",
                                            OUTPUT mRowId).      
          IF mRowId EQ ? THEN
            LEAVE loan_code.                
          FIND FIRST m-loan WHERE ROWID(m-loan) EQ mRowId
             NO-LOCK NO-ERROR.
          IF mMultiRun EQ "��" THEN
          DO:
             /* ���稭���� ������ ���� �஫����஢����� ⮫쪮 ⮣��,
             ����� � �롮�� ������� �墠�뢠�騩 ������� */
             IF {assigned m-loan.parent-cont-code}  THEN
                NEXT loan_code.
             /* �᫨ m-loan �墠�뢠�騩 �������, � ᮡ�ࠥ� ��� ��⮬��� */
             FOR EACH m2-loan WHERE m2-loan.contract         EQ "dps"
                                AND m2-loan.parent-cont-code EQ m-loan.cont-code 
                NO-LOCK:
                {additem.i mLoanList m2-loan.cont-code}
             END.
          END.
          IF NOT {assigned mLoanList} THEN
             mLoanList = m-loan.cont-code.

          DO mCii = 1 TO NUM-ENTRIES (mLoanList):

             FIND FIRST loan WHERE loan.contract  EQ "dps"
                               AND loan.cont-code EQ ENTRY (mCii, mLoanList)
                NO-LOCK NO-ERROR.
             IF AVAIL loan THEN
             DO:                                                                
               /* �஢�ઠ �������� ���� �� ����������� �஫����樨 */
               RUN Get_Contract_Date IN h_dpspc(RECID(loan),
                                                cred,
                                                OUTPUT cred-t).
               IF cred NE cred-t THEN      
               DO:
                  NEXT loan_code.
               END.

               RUN CreateFrmFields (?,"LoanRecid","",STRING(RECID(loan))).

               ASSIGN
                  in-cont-code = loan.cont-code
                  in-contract  = loan.contract
                  in-date      = loan.end-date
                  .

               FIND FIRST prl-op-kind WHERE prl-op-kind.op-kind EQ loan.op-kind 
                  NO-LOCK NO-ERROR.
               IF NOT AVAILABLE prl-op-kind THEN
               DO:
                  /* �訡��, ��� �࠭���樨 ������ ������, �⪠� */
                  CREATE err. 
                  ASSIGN err.err = {&NotExistOpenOpKind}.
                  NEXT loan_code.
               END.
               /* �஢�ઠ ������ �᫮��� �� ������ */
               IF NOT CAN-FIND(FIRST loan-cond WHERE loan-cond.contract  EQ loan.contract
                                                 AND loan-cond.cont-code EQ loan.cont-code) THEN
               DO:
                  CREATE err. 
                  ASSIGN err.err = {&NotExistLoanCond}.
                  NEXT loan_code.
               END.      

               /* �������� ����� ��⮢ �� ����室����� */
               set_type(loan.cont-code).
               {cr_acc_p.i}

               /* ����祭�� ���ଠ樨 � ����뢠���� �� �஫����樨 ���� */
               {empty tmp-acct-cl}            
               RUN pFillAcctCl (ROWID(loan),
                                cr_acc_p-vsincedate).

               /* ᮧ���� ���������� */
               DO mJ = 1 TO NUM-ENTRIES(templ-trans):         
                  FIND FIRST op-template OF op-kind 
                                         WHERE op-template.op-template EQ INTEGER(ENTRY(mJ, templ-trans))
                     NO-LOCK NO-ERROR.

                  IF Cr_loan_trans(BUFFER loan-transaction,
                                   BUFFER loan,
                                   RECID(op-template),
                                   in-op-date,
                                   mess) EQ '-1' THEN
                  DO:
                     RUN Fill-SysMes IN h_tmess(op-kind.op-kind,"","-1","~n" + mess).
                     UNDO Loan_code,LEAVE Loan_code.
                  END.
                  IF AVAILABLE loan-transaction THEN
                  DO:
                     ASSIGN
                        /*llt = YES*/
                        loan-transaction.trans-code = TRANS_NUMBER(loan-transaction.contract,
                                                                   loan-transaction.cont-code)
                        .
                     RUN put-loan-trans IN hnd-proc (RECID(loan-transaction)).
                  END.
               END.

               /* ��ࠡ�⪠ 蠡����� ���㬥�⮢ */
               jj:
               FOR EACH op-template OF op-kind
                  NO-LOCK 
                  BREAK BY op-template.op-template
                     ON ERROR  UNDO loan_code, LEAVE loan_code
                     ON ENDKEY UNDO loan_code, LEAVE loan_code:
                 
                  nopars = NO.
                  RELEASE op.
                  RELEASE op-entry.
                  RELEASE kau-entry.

                  IF CAN-DO(lst-tmpl-op,STRING(op-template.op-template)) THEN
                  DO:
                     {bef-tran.i 
                        &bef-undo      = "NEXT jj."
                        &undo-no-proc  = "UNDO loan_code, LEAVE loan_code."
                     }

                     FIND FIRST t-op WHERE t-op.op EQ op-template.op-template 
                        NO-LOCK NO-ERROR.

                     IF NOT AVAILABLE t-op THEN 
                        NEXT jj.

                     IF GetXAttrValueEx("op-template",
                                        op-template.op-kind + "," + STRING(op-template.op-template),
                                        "PrsnTmpl",
                                        "���") EQ "���" THEN
                     DO:

                        /* �������� wop */
                        FOR EACH wop WHERE
                           wop.op-templ GE op-templ.op-templ:
                           DELETE wop.
                        END.

                        CREATE wop.
                        ASSIGN
                           wop.con-date =  cr_acc_p-vsincedate
                           wop.op-templ = op-templ.op-templ
                           wop.op-kind  = op-kind.op-kind  /* ��⠢���� Sema 23/05/99 */
                           in-status    = op-template.op-status
                           cur-op-date  = in-op-date
                        .

                        /* �������� ���㬥�� */
                        FIND FIRST person WHERE person.person-id EQ loan.cust-id 
                           NO-LOCK NO-ERROR.
                        CREATE op.
                        {op(sess).cr}
                        {g-op.ass}
                        ASSIGN
                           op.doc-type         = t-op.doc-type
                           op.details          = t-op.details
                                                 + IF fGetSetting("����঎������",?,'��') EQ '��'  THEN ' ����� N ' + loan.cont-code  
                                                                                                         + ' ������ ' + (IF AVAILABLE person THEN person.name-last + ' ' + person.first-name
                                                                                                                      ELSE "")
                                                   ELSE ""
                           op.contract-date    = t-op.contract-date
                        .                              
                        VALIDATE op NO-ERROR.

                        /* ���� ��᫥���� �஢���� �� ���㬥��� � ��।������ �� ����� */
                        FIND LAST xxop-entry WHERE xxop-entry.op EQ op.op 
                           NO-LOCK NO-ERROR.            
                        k = IF AVAILABLE xxop-entry THEN (xxop-entry.op-entry + 1)
                                                    ELSE (1).

                        /* ���� ��⮢ ��� �஢���� */
                        {g-acctv1.i
                           &OFbase  = YES
                           &BYrole  = YES
                           &vacct   = tacct
                        }
                        wop.con-date =  cred.            
                        IF    tacct-db EQ ? 
                           OR tacct-cr EQ ? THEN
                        DO:
                           CREATE err.
                           err.err = IF tacct-cr EQ ? THEN {&NotFindCrAcctLogg}
                                                      ELSE {&NotFindDbAcctLogg}.
                           UNDO loan_code, NEXT loan_code.
                        END.

                        IF tacct-cr NE tacct-db THEN
                        DO:
                           /* �᫨ ���筠� �஢���� - ᮧ����� �஢���� */
                           CREATE op-entry.
                        
                           {g-en.ass &ind=k}
   
                           /*kau-rid = 0.*/
                        END.
                        ELSE
                        DO:
                           /* �᫨ ��८業�� - ? */
                           IF op.class-code NE "opkau" THEN
                              UNDO jj, NEXT jj.
              
                           nopars = YES.
            
                           RUN crkau (tacct-db,
                                     (in-contract + "," + in-cont-code + "," 
                                          + IF NOT AVAIL loan-transaction THEN "��₪��"
                                                                          ELSE (loan-transaction.trans-code + ",��₪���")),
                                      RECID(op)).         
                        END.

                        /* �᫨ ᮧ�������� �஢���� */
                        IF AVAILABLE op-entry THEN
                        DO:
                           ASSIGN
                              op-entry.value-date = in-op-date
                              op-entry.op-status  = op.op-status
                              op-entry.acct-cat   = op.acct-cat
                              op-entry.acct-cr    = tacct-cr
                              op-entry.acct-db    = tacct-db
                           .         
                           op-entry.currency = IF op-template.currency NE ? THEN GetCurr(op-template.currency)
                                                                            ELSE loan.currency.
                        END.

                        /* ����� ������祭 ��⮢ ���४�஢�� �������� ���� ���㬥�⮢,
                        ** ᮧ�������� �� �஫����樨 , ��� ᤢ������� �� 1 ����, �᫨ ���
                        ** ��८�ଫ����, ���� ��⮩ � ���ன ������ ���������
                        ** ��業�� �� ��८�ଫ���� -�஫����樨 ������ */
                        RUN GetClassMethod IN h_xclass (loan.class-code,
                                                        "ch_cont_dat",
                                                        "","",
                                                        OUTPUT nameproc,
                                                        OUTPUT params).
                        IF {assigned nameproc} THEN
                        DO:
                           RUN VALUE(nameproc + ".p")(RECID(loan),
                                                      RECID(op),
                                                      cred,
                                                      OUTPUT fl-o).
                           IF fl-o LT 0 THEN
                           DO:
                              RUN CreateErr("�� ������ ��⮤ <ch_cont_dat> �� ����� " + loan.class-code + "." ).                        
                              UNDO loan_code, LEAVE loan_code.
                           END.
                        END.

                        /* ��᢮���� ����� wop */
                        {asswop.i}
   
                        /* ��ࠡ�⪠ ���� ���஡���� */
                        RUN ProcessDetails (RECID(wop), INPUT-OUTPUT op.details).

                        ASSIGN
                           wop.acct-db  = tacct-db
                           wop.acct-cr  = tacct-cr
                           wop.currency = IF op-template.currency NE ? THEN GetCurr(op-templ.currency)
                                                                       ELSE loan.currency
                           wop.con-date = op.contract-date
                           dval         = IF AVAILABLE op-entry THEN op-entry.value-date
                                                                ELSE op.op-date
                           wop.op-recid = IF AVAILABLE op-entry THEN RECID(op-entry)
                                                                ELSE ?
                        .

                        /* ��⠭���� ���� ��. ���⪠ */
                        cod-ost = get-ost-templ(op-kind.op-kind,
                                                op-template.op-template).
                        set_type(in-cont-code).
                        IF  cod-ost NE ? THEN
                           set_ost(cod-ost).

                        /* ��� ���⪠ �᫨ ��७�� �� �� */
                        IF     op-templ.acct-cr MATCHES '*loan-dps-p*' 
                           AND op-templ.acct-db MATCHES '*loan-dps-t*' THEN
                        DO:
                           set_type(in-cont-code).
                           set_ost('��₪��').
                        END.

                        IF NOT nopars THEN
                        DO:
                           RUN parssen.p (RECID(wop), 
                                          in-op-date, 
                                          OUTPUT fler).
                           IF fler THEN
                           DO:
                              CREATE err. 
                              ASSIGN err.err = {&ErrorParsenLogg}.
                              UNDO loan_code, NEXT loan_code.
                           END.

                           t-details = op-template.details.
                           RUN ProcessDetails (RECID(wop), INPUT-OUTPUT t-details).
                           op.details = t-details.         
                        END.

                        /* �᫨ �㬬� �� ���㬥��� �㫥�� - 㤠�塞 �஢���� � ��.�஢���� */
                        IF     wop.amt-rub EQ 0 
                           AND wop.amt-cur EQ 0 THEN
                        DO:
                           IF AVAILABLE op-entry THEN
                              DELETE op-entry.
                        
                           FIND FIRST kau-entry WHERE RECID(kau-entry) EQ kau-rid[1] 
                              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                           IF AVAILABLE kau-entry THEN
                              DELETE kau-entry.
                        
                           FIND FIRST kau-entry WHERE RECID(kau-entry) EQ kau-rid[2] 
                              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                           IF AVAILABLE kau-entry THEN
                              DELETE kau-entry.
                        
                           ASSIGN
                              kau-rid[1] = 0
                              kau-rid[2] = 0
                              .
                        
                           IF AVAILABLE op THEN
                              DELETE op.
                        
                           NEXT jj.
                        END.

                        IF AVAILABLE op-entry THEN
                           ASSIGN
                              op-entry.amt-rub = wop.amt-rub
                              op-entry.amt-cur = IF op-entry.currency NE "" THEN wop.amt-cur
                                                                            ELSE op-entry.amt-cur
                              op-entry.user-id    = USERID('bisquit')
                              .
                        
                        IF AVAILABLE op-entry THEN
                        DO:
                           fler = YES.
                           tr0:
                           DO:
                              {aft-temp.i
                                 &aft-undo = " UNDO loan_code, LEAVE loan_code."
                              }
                              {op-entry.upd
                                 &871=YES
                                 &open-undo = " leave tr0 "
                                 &kau-undo  = " leave tr0 "
                              }
                              /* �������� �� ⨯� PARSEN_<�����������> */
                              {g-psigns.i}
                              fler = NO.
                           END.
                           IF fler THEN
                           DO:
                              CREATE err. 
                              ASSIGN err.err = {&ErrorAcctOperLogg}.
                              UNDO loan_code, NEXT loan_code.
                           END.
                        END.
                     END.
                     ELSE
                     DO:
                        Set_type(loan.cont-code).
                        {wop-cr.i
                           &Err-ParsAcct   = "IF tacct-db EQ ? OR tacct-cr EQ ? THEN     ~
                                              DO:                                        ~
                                                 CREATE err.                             ~
                                                 IF tacct-cr = ? THEN ASSIGN err.err = {&NotFindCrAcctLogg}. ~
                                                                 ELSE ASSIGN err.err = {&NotFindDbAcctLogg}. ~
                                                 UNDO loan_code, NEXT loan_code .        ~
                                              END. "
                           &Err-ParsSumm   = "IF fler THEN                               ~
                                              DO:                                        ~
                                                 CREATE err.                             ~
                                                 ASSIGN err.err = {&ErrorParsenLogg}.    ~
                                                 UNDO loan_code,NEXT loan_code.          ~
                                              END. "                        
                           &NoParsDetails  = YES
                        }
                     END.
                  END.
               END.

               mAcct = GetSysConf("�������").
               RUN DeleteOldDataProtocol IN h_base ("�������").
               IF {assigned mAcct} THEN 
               DO:
                  RUN acctclopr.p (loan.cont-code, 
                                   in-op-date, 
                                   OUTPUT fl-o).
                  RUN modify-sts(RECID(loan)).
               END.
               ELSE
               DO:
                  /* ��।������ �஫����樨 � ���ன ���쬥� �᫮��� ��� �஫����樨,
                  ** � ⠪�� ����室������ ᮧ����� ������ �᫮��� */
                  ASSIGN
                     in-op-kind  = loan.op-kind
                     old-op-kind = in-op-kind
                     .
                  RUN put_kd.p (op-kind.op-kind,
                                RECID(loan),
                                "loan-op-kind",
                                OUTPUT fl-o,
                                OUTPUT in-op-kind).
                  IF fl-o EQ -1 THEN
                  DO:
                     RUN Fill-SysMes IN h_tmess(op-kind.op-kind,"","-1","� �⨬ ������஬ ��-� ࠡ�⠥�!").
                     UNDO loan_code, LEAVE loan_code.
                  END.

                  IF fl-o NE 0 THEN
                     RUN put_kd.p (loan.op-kind,
                                   RECID(loan),
                                   "prol-kind",
                                   OUTPUT fl-o,
                                   OUTPUT in-op-kind).

                  IF fl-o EQ -1 THEN
                  DO:
                     RUN Fill-SysMes IN h_tmess(op-kind.op-kind,"","-1","� �⨬ ������஬ ��-� ࠡ�⠥�!").

                     UNDO loan_code, LEAVE loan_code.
                  END.
                  IF NOT {assigned in-op-kind} THEN
                     ASSIGN
                        change-loan-cond = NO
                        in-op-kind       = loan.op-kind
                        .
                  ELSE
                     IF in-op-kind EQ old-op-kind THEN
                        ASSIGN
                           change-loan-cond = NO
                           in-op-kind       = loan.op-kind
                           .
                     ELSE
                        change-loan-cond = YES.

                  /* ���� ��⮤� �஫����樨 */
                  RUN GetClassMethod IN h_xclass (loan.class-code,
                                                  "ch_loan",
                                                  "","",
                                                  OUTPUT nameproc,
                                                  OUTPUT params).

                  IF {assigned nameproc} THEN
                  DO:

                     RUN Modify-Loan(in-cont-code,
                                     YES).

                     IF RETURN-VALUE NE "" THEN
                     DO:

                        IF     RETURN-VALUE  EQ "UNDO_STS" 
                           AND sts_close     NE ? THEN
                        DO:
                           /* �⪠�, �� ����室��� �������� ����� � ������*/

                           RUN acctclopr.p (loan.cont-code, 
                                            in-op-date, 
                                            OUTPUT fl-o).

                        END.
                        RUN modify-sts(RECID(loan)).
                        NEXT loan_code.
                     END.
                     RUN SetSysConf IN h_base("op-kind-prol", op-kind.op-kind).
                     RUN VALUE(nameproc + ".p")(RECID(loan),
                                                in-op-date,
                                                cred, 
                                                OUTPUT fl-o).
                     RUN DeleteOldDataProtocol IN h_base ("op-kind-prol").
                     IF fl-o LT 0 THEN
                     DO:
                        RUN CreateErr("����� " + loan.cont-code + ": �訡�� ��⮤� ��������� ��ࠬ��஢ ������ �� �஫����樨 <" + nameproc + ">.").
                        UNDO loan_code, NEXT loan_code.
                     END. 
                  END.
                  ELSE
                  DO:

                     RUN Modify-Loan(in-cont-code, NO).

                     CASE RETURN-VALUE:
                        WHEN "UNDO"          THEN
                           UNDO loan_code, NEXT loan_code.
                        WHEN "TRANSFER CASH" THEN
                           flag-transfer = YES.
                        WHEN "UNDO_STS" THEN
                        DO:

                           IF sts_close NE ? THEN
                           DO:
                              /*�⪠�, �� ����室��� �������� ����� � ������*/

                              RUN acctclopr.p (loan.cont-code, 
                                               in-op-date, 
                                               OUTPUT fl-o).

                           END.

                           RUN modify-sts(RECID(loan)).

                           NEXT loan_code.
                        END.
                     END CASE.

                     IF change-loan-cond EQ YES THEN
                     DO:

                        RUN Modify-Loan-Cond(In-Cont-Code,
                                             cred).
                        IF RETURN-VALUE EQ "UNDO" THEN
                        DO:
                           RUN CreateErr("����� " + loan.cont-code + ": �訡�� ��⮤� ��������� ��ࠬ��஢ ������ �� �஫����樨.").
                           UNDO loan_code, NEXT loan_code.
                        END.                       
                     END.
                  END.
               END.

               /* �⮡ࠦ���� ���ଠ樨 �� ��ࠡ�⠭��� ������ */
               RUN CreatettProlong (in-op-date,
                                    ENTRY (1, loan.cont-code, "@"),
                                    loan.currency).
   
               {cdealend.i 
                   &mt = "UNDO loan_code, NEXT loan_code"
                   &p = "o"
               }
            
               {cdealend.i 
                   &mt =  "UNDO loan_code, NEXT loan_code"
                   &p = "n"
               }
   
               IF     fGetSetting ("�������஫", "", "���") EQ "��"
                  AND {assigned templ-acct} THEN
               DO:
                  RUN acctclopr.p (loan.cont-code, 
                                   in-op-date, 
                                   OUTPUT fl-o).      
               END.
             END. /* IF AVAIL LOAN */

             IF mCii EQ NUM-ENTRIES (mLoanList)
                AND mMultiRun EQ "��" 
                AND {assigned loan.parent-cont-code} THEN
             DO: /* �ᯥ譮 �஫����஢���� ��᫥���� ���稭���� �����. �஫�����㥬 �墠�뢠�騩 */
                RUN ProlParentLoan IN THIS-PROCEDURE (RECID (loan),
                                                      OUTPUT mErr).
                IF mErr = -1 THEN
                   UNDO loan_code, NEXT loan_code.

             END.
          END. /* DO mCii = 1 TO...*/
         END. /* DO WHILE TRUE */
         RUN Close-Query IN mHPQuery (NO).
      END.
      RUN Close-Query IN mHPQuery (YES).

      /* ����� ���⮢ � ����� */
      &IF DEFINED(auto) = 0 &THEN
   
      IF vSilentMode EQ "" THEN
      DO:
         RUN DisplayttProlong.
         {preview2.i
            &stream   = "stream _prolong_"
            &filename = _spool1.tmp
         }
      
         /*����� �������⥩ ����.���*/
         {prn-ved.i
             &RunReportPrint  = "���⠥� ���������"}
   
         IF NOT {assigned mReportProcName}
            AND CAN-FIND(FIRST {&Nachkin-tt} NO-LOCK) THEN 
         DO:   
            {setdest2.i
               &stream   = "stream err "
               &filename = "_spool4.tmp"
               &cols     = 200
            }
            /* ��������� ���᫥���� ��業⮢ */
            RUN ved_nach (in-op-date,
                          cred).
         END.
         {preview2.i
            &stream   = "stream err"
            &filename = _spool4.tmp
            &nodef    = "/*"
         }
      END.
      ELSE 
      DO:      
         OUTPUT STREAM _prolong_ CLOSE.
         OUTPUT STREAM err CLOSE.
      END.
      
      /*�訡�� �� �஫����樨*/
      IF CAN-FIND (FIRST err) THEN 
      DO:   
        {setdest2.i
           &stream   = "stream err-ved"
           &filename = _log.tmp
           &cols     = 120
        }
        PUT STREAM err-ved  UNFORMATTED "������ ��� �����������: ~n ~n". 
        FOR EACH err:
            PUT STREAM err-ved  UNFORMATTED
               err.err + "~n".
        END.
      
        IF vSilentMode EQ "" THEN
        DO:
           {preview2.i
              &stream   = "stream err-ved"
              &filename = _log.tmp
              &nodef    = "/*"
           }
        END.
        ELSE
           OUTPUT STREAM err-ved CLOSE.
      END.
      
      /*����饭�� �� �஫����樨*/
      IF CAN-FIND (FIRST err) THEN 
      DO:
         {setdest2.i
            &stream   = "stream err-mess"
            &filename = _log1.tmp
            &cols     = 120
         }
         RUN ved_err ("").      
         IF vSilentMode EQ "" THEN
         DO:
            {preview2.i
               &stream   = "stream err-mess"
               &filename = _log1.tmp
               &nodef    = "/*"
            }
         END.
         ELSE
            OUTPUT STREAM err-mess CLOSE.
      END.
      
      &ENDIF
      
      IF vSave%  THEN 
      DO:
         RUN setlevel IN h_debug (mTmpLevel).
      END.
   END.
END.

HIDE FRAME opreq NO-PAUSE.
OUTPUT STREAM err-ved CLOSE.
OUTPUT STREAM err-mess CLOSE.
OUTPUT STREAM err CLOSE.
OUTPUT STREAM _prolong_ CLOSE.

RUN DeleteOldDataProtocol IN h_base ("TypeNach").
RUN DeleteOldDataProtocol IN h_base ("mData-ID").
RUN DeleteOldDataProtocol IN h_base ("OldRef").

/* ����� ��⪨ ����� ���⭮ */
RUN GarbageCollect IN h_base.
RUN End-SysMes IN h_tmess.

{plibdel.i}
DELETE PROCEDURE loan_h .
{intrface.del}

/* �������� �� ��᫥ �믮������ �࠭���樨 */
{cmd-exe1.i
   &cmd = "'Postcmd'"
}
{savecond.i &log=YES}


PROCEDURE ProlParentLoan:
   DEFINE INPUT   PARAMETER iRec  AS RECID          NO-UNDO.
   DEFINE OUTPUT  PARAMETER oErr  AS INT64 INIT -1  NO-UNDO.

   DEFINE BUFFER b-loan  FOR loan.
   DEFINE BUFFER bl-loan FOR loan.

   MAIN:
   DO ON ERROR    UNDO MAIN, LEAVE MAIN
      ON ENDKEY   UNDO MAIN, LEAVE MAIN:

      FIND FIRST b-loan WHERE RECID (b-loan) EQ iRec
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO MAIN, LEAVE MAIN.
      FIND FIRST bl-loan WHERE bl-loan.contract  EQ "dps"
                           AND bl-loan.cont-code EQ b-loan.parent-cont-code 
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL bl-loan
         OR ERROR-STATUS:ERROR  THEN
         UNDO MAIN, LEAVE MAIN.
      ASSIGN
         bl-loan.prolong  = b-loan.prolong
         bl-loan.end-date = b-loan.end-date
         bl-loan.loan-status = b-loan.loan-status
      .

      /* �⮡ࠦ���� ���ଠ樨 �� ��ࠡ�⠭��� ������ */
      RUN CreatettProlong (in-op-date,
                           ENTRY (1, bl-loan.cont-code, "@"),
                           bl-loan.currency).
      oErr = 0.

   END.
END PROCEDURE.
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='11/08/2015 12:05:14.494+04:00' */
/* $LINTUSER='osov' */
/* $LINTMODE='1' */
/* $LINTFILE='gr_prol3.p' */
/*prosign4EdnP8/nAicUL5WKIhJlKw*/